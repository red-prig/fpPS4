unit kern_descrip;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 kern_id,
 vfile,
 vstat,
 vuio,
 vcapability,
 vfcntl,
 vfilio,
 vmount,
 vfs_vnode,
 vsocketvar;

const
 FGET_GETCAP=$00000001;

function sys_getdtablesize():Integer;
function sys_dup2(from,_to:Integer):Integer;
function sys_dup(u_fd:Integer):Integer;
function sys_fcntl(fd,cmd:Integer;arg:QWORD):Integer;
function sys_close(fd:Integer):Integer;
function sys_fstat(fd:Integer;sb:p_stat):Integer;
function sys_fpathconf(fd,name:Integer):Integer;
function sys_flock(fd,how:Integer):Integer;

function  fget_unlocked(fd:Integer):p_file;
function  falloc_noinstall(resultfp:pp_file):Integer;
function  finstall(fp:p_file;fd:PInteger;flags:Integer):Integer;
function  falloc(resultfp:pp_file;resultfd:PInteger;flags:Integer):Integer;

procedure fhold(fp:p_file);
function  fdrop(fp:p_file):Integer;

function  fget(fd:Integer;
               rights:cap_rights_t;
               fpp:pp_file):Integer;

function  fget_mmap(fd:Integer;
                    rights:cap_rights_t;
                    maxprotp:PByte;
                    fpp:pp_file):Integer;

function  fget_read(fd:Integer;
                    rights:cap_rights_t;
                    fpp:pp_file):Integer;

function  fget_write(fd:Integer;
                     rights:cap_rights_t;
                     fpp:pp_file):Integer;

function  fgetcap(fd:Integer;
                  fpp:pp_file):Integer;

function  fgetvp(fd:Integer;
                 rights:cap_rights_t;
                 vpp:pp_vnode):Integer;

function  fgetvp_rights(fd:Integer;
                        need:cap_rights_t;
                        have:p_cap_rights_t;
                        vpp:pp_vnode):Integer;

function  fgetvp_read(fd:Integer;
                      rights:cap_rights_t;
                      vpp:pp_vnode):Integer;

function  fgetvp_exec(fd:Integer;
                      rights:cap_rights_t;
                      vpp:pp_vnode):Integer;

function  fgetvp_write(fd:Integer;
                       rights:cap_rights_t;
                       vpp:pp_vnode):Integer;

function  fgetsock(fd:Integer;
                   rights:cap_rights_t;
                   spp:pp_socket;
                   fflagp:PDWORD):Integer;

procedure fdclose(fp:p_file;idx:Integer);
function  dupfdopen(indx,dfd,mode,error:Integer):Integer;
procedure finit(fp:p_file;flag:DWORD;_type:Word;data:Pointer;ops:p_fileops);

function  badfo_readwrite(fp:p_file;uio:p_uio;flags:Integer):Integer;
function  badfo_truncate(fp:p_file;length:Int64):Integer;
function  badfo_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
function  badfo_poll(fp:p_file;events:Integer):Integer;
function  badfo_kqfilter(fp:p_file;kn:Pointer):Integer;
function  badfo_stat(fp:p_file;sb:p_stat):Integer;
function  badfo_close(fp:p_file):Integer;
function  badfo_chmod(fp:p_file;mode:mode_t):Integer;
function  badfo_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;

const
 badfileops:fileops=(
  fo_read    :@badfo_readwrite;
  fo_write   :@badfo_readwrite;
  fo_truncate:@badfo_truncate;
  fo_ioctl   :@badfo_ioctl;
  fo_poll    :@badfo_poll;
  fo_kqfilter:@badfo_kqfilter;
  fo_stat    :@badfo_stat;
  fo_close   :@badfo_close;
  fo_chmod   :@badfo_chmod;
  fo_chown   :@badfo_chown;
  fo_flags   :0
 );

var
 p_leader:packed record
  p_flag:Integer;
 end;

implementation

uses
 atomic,
 errno,
 systm,
 vfiledesc,
 sys_capability,
 vfs_subr,
 vfs_vnops,
 vnode_if,
 kern_resource,
 kern_mtx;

function badfo_readwrite(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Exit(EBADF);
end;

function badfo_truncate(fp:p_file;length:Int64):Integer;
begin
 Exit(EINVAL);
end;

function badfo_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin
 Exit(EBADF);
end;

function badfo_poll(fp:p_file;events:Integer):Integer;
begin
 Exit(0);
end;

function badfo_kqfilter(fp:p_file;kn:Pointer):Integer;
begin
 Exit(EBADF);
end;

function badfo_stat(fp:p_file;sb:p_stat):Integer;
begin
 Exit(EBADF);
end;

function badfo_close(fp:p_file):Integer;
begin
 Exit(EBADF);
end;

function badfo_chmod(fp:p_file;mode:mode_t):Integer;
begin
 Exit(EBADF);
end;

function badfo_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;
begin
 Exit(EBADF);
end;

{
 * System calls on descriptors.
 }
function sys_getdtablesize():Integer;
begin
 curkthread^.td_retval[0]:=lim_cur(RLIMIT_NOFILE);
 Exit(0);
end;

// Flags for do_dup()
const
 DUP_FIXED  =$1; // Force fixed allocation
 DUP_FCNTL  =$2; // fcntl()-style errors
 DUP_CLOEXEC=$4; // Atomically set FD_CLOEXEC

function do_dup(flags,old,new:Integer;retval:PQWORD):Integer; forward;

{
 * Duplicate a file descriptor to a particular value.
 *
 * Note: keep in mind that a potential race condition exists when closing
 * descriptors from a shared descriptor table (via rfork).
 }
function sys_dup2(from,_to:Integer):Integer;
begin
 Exit(do_dup(DUP_FIXED, from, _to,  @curkthread^.td_retval));
end;

{
 * Duplicate a file descriptor.
 }
function sys_dup(u_fd:Integer):Integer;
begin
 Exit(do_dup(0, u_fd, 0, @curkthread^.td_retval));
end;

function kern_fcntl(fd,cmd:Integer;arg:QWORD):Integer; forward;

{
 * The file control system call.
 }
function sys_fcntl(fd,cmd:Integer;arg:QWORD):Integer;
var
 fl:t_flock;
 ofl:__oflock;
 error:Integer;
begin
 error:=0;
 case cmd of
  F_OGETLK,
  F_OSETLK,
  F_OSETLKW:
   begin
    {
     * Convert old flock structure to new.
     }
    error:=copyin(Pointer(arg), @ofl, sizeof(ofl));
    fl.l_start :=ofl.l_start;
    fl.l_len   :=ofl.l_len;
    fl.l_pid   :=ofl.l_pid;
    fl.l_type  :=ofl.l_type;
    fl.l_whence:=ofl.l_whence;
    fl.l_sysid :=0;

    case cmd of
     F_OGETLK :cmd:=F_GETLK;
     F_OSETLK :cmd:=F_SETLK;
     F_OSETLKW:cmd:=F_SETLKW;
    end;
    arg:=QWORD(@fl);
   end;

  F_GETLK,
  F_SETLK,
  F_SETLKW,
  F_SETLK_REMOTE:
   begin
    error:=copyin(Pointer(arg), @fl, sizeof(fl));
    arg:=QWORD(@fl);
   end;
  else;
 end;
 if (error<>0) then
  Exit(error);
 error:=kern_fcntl(fd, cmd, arg);
 if (error<>0) then
  Exit(error);
 if (cmd=F_OGETLK) then
 begin
  ofl.l_start :=fl.l_start;
  ofl.l_len   :=fl.l_len;
  ofl.l_pid   :=fl.l_pid;
  ofl.l_type  :=fl.l_type;
  ofl.l_whence:=fl.l_whence;
  error:=copyout(@ofl, Pointer(arg), sizeof(ofl));
 end else
 if (cmd=F_GETLK) then
 begin
  error:=copyout(@fl, Pointer(arg), sizeof(fl));
 end;
 Exit(error);
end;

function fdunwrap(fd:Integer;rights:cap_rights_t;fpp:pp_file):Integer;
var
 err:Integer;
begin

 fpp^:=fget_unlocked(fd);
 if (fpp^=nil) then
  Exit(EBADF);

 if (fpp^^.f_type=DTYPE_CAPABILITY) then
 begin
  err:=cap_funwrap(fpp^, rights, fpp);
  if (err<>0) then
  begin
   fdrop(fpp^);
   fpp^:=nil;
   Exit(err);
  end;
 end;

 Exit(0);
end;

function kern_fcntl(fd,cmd:Integer;arg:QWORD):Integer;
label
 _break,
 do_setlk,
 _break2,
 do_readahead,
 readahead_vnlock_fail;
var
 td:p_kthread;
 flp:p_flock;
 fp:p_file;
 vp:p_vnode;
 error,flg,tmp:Integer;
 vfslocked:Integer;
 old,new:DWORD;
 bsize:QWORD;
 foffset:Int64;
begin
 td:=curkthread;
 vfslocked:=0;
 error:=0;
 flg:=F_POSIX;
 fp:=nil;

 case cmd of
  F_DUPFD:
   begin
    tmp:=arg;
    error:=do_dup(DUP_FCNTL, fd, tmp,@td^.td_retval);
   end;

  F_DUPFD_CLOEXEC:
   begin
    tmp:=arg;
    error:=do_dup(DUP_FCNTL or DUP_CLOEXEC, fd, tmp,@td^.td_retval);
   end;

  F_DUP2FD:
   begin
    tmp:=arg;
    error:=do_dup(DUP_FIXED, fd, tmp,@td^.td_retval);
   end;

  F_DUP2FD_CLOEXEC:
   begin
    tmp:=arg;
    error:=do_dup(DUP_FIXED or DUP_CLOEXEC, fd, tmp,@td^.td_retval);
   end;

  F_GETFD:
   begin
    fp:=fget_unlocked(fd);
    if (fp=nil) then
    begin
     error:=EBADF;
     goto _break;
    end;
    if ((fp^.f_exclose and UF_EXCLOSE)<>0) then
    begin
     td^.td_retval[0]:=FD_CLOEXEC;
    end else
    begin
     td^.td_retval[0]:=0;
    end;
   end;

  F_SETFD:
   begin
    fp:=fget_unlocked(fd);
    if (fp=nil) then
    begin
     error:=EBADF;
     goto _break;
    end;
    if ((arg and FD_CLOEXEC)<>0) then
    begin
     atomic_set_int(@fp^.f_exclose,UF_EXCLOSE);
    end else
    begin
     atomic_clear_int(@fp^.f_exclose,UF_EXCLOSE);
    end;
   end;

  F_GETFL:
   begin
    error:=fdunwrap(fd, CAP_FCNTL, @fp);
    if (error<>0) then
    begin
     goto _break;
    end;
    td^.td_retval[0]:=OFLAGS(fp^.f_flag);
   end;

  F_SETFL:
   begin
    error:=fdunwrap(fd, CAP_FCNTL, @fp);
    if (error<>0) then
    begin
     goto _break;
    end;
    repeat
     flg:=fp^.f_flag;
     tmp:=flg;
     tmp:=tmp and (not FCNTLFLAGS);
     tmp:=tmp or FFLAGS(arg and (not O_ACCMODE)) and FCNTLFLAGS;
    until (System.InterlockedCompareExchange(fp^.f_flag,tmp,flg)=flg);
    tmp:=fp^.f_flag and FNONBLOCK;
    error:=fo_ioctl(fp, FIONBIO, @tmp);
    if (error<>0) then
    begin
     goto _break;
    end;
    tmp:=fp^.f_flag and FASYNC;
    error:=fo_ioctl(fp, FIOASYNC, @tmp);
    if (error=0) then
    begin
     goto _break;
    end;
    atomic_clear_int(@fp^.f_flag, FNONBLOCK);
    tmp:=0;
    fo_ioctl(fp, FIONBIO, @tmp);
   end;

  F_GETOWN:
   begin
    error:=fdunwrap(fd, CAP_FCNTL, @fp);
    if (error<>0) then
    begin
     goto _break;
    end;
    error:=fo_ioctl(fp, FIOGETOWN, @tmp);
    if (error=0) then
    begin
     td^.td_retval[0]:=tmp;
    end;
   end;

  F_SETOWN:
   begin
    error:=fdunwrap(fd, CAP_FCNTL, @fp);
    if (error<>0) then
    begin
     goto _break;
    end;
    tmp:=arg;
    error:=fo_ioctl(fp, FIOSETOWN, @tmp);
   end;

  F_SETLK_REMOTE:
   begin
    error:=EPERM;
    //error:=priv_check(td, PRIV_NFS_LOCKD);
    if (error<>0) then
     Exit(error);
    flg:=F_REMOTE;
    goto do_setlk;
   end;

  F_SETLKW:
  begin
   flg:=flg or F_WAIT;
   goto do_setlk;
  end;

  F_SETLK:
   begin
    do_setlk:
    error:=fdunwrap(fd, CAP_FLOCK, @fp);
    if (error<>0) then
    begin
     goto _break;
    end;
    if (fp^.f_type<>DTYPE_VNODE) then
    begin
     error:=EBADF;
     goto _break;
    end;
    flp:=p_flock(arg);
    if (flp^.l_whence=SEEK_CUR) then
    begin
     FILEDESC_SLOCK(@fd_table);
     foffset:=foffset_get(fp);
     if (foffset < 0) or
        ((flp^.l_start > 0) and
         (foffset > High(Int64) - flp^.l_start)) then
     begin
      FILEDESC_SUNLOCK(@fd_table);
      error:=EOVERFLOW;
      goto _break;
     end;
     Inc(flp^.l_start,foffset);
     FILEDESC_SUNLOCK(@fd_table);
    end;

    {
     * VOP_ADVLOCK() may block.
     }
    vp:=fp^.f_vnode;
    vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
    case flp^.l_type of
     F_RDLCK:
      begin
       if ((fp^.f_flag and FREAD)=0) then
       begin
        error:=EBADF;
        goto _break2;
       end;
       PROC_LOCK();
       p_leader.p_flag:=p_leader.p_flag or P_ADVLOCK;
       PROC_UNLOCK();
       error:=VOP_ADVLOCK(vp, @p_leader, F_SETLK, flp, flg);
      end;
     F_WRLCK:
      begin
       if ((fp^.f_flag and FWRITE)=0) then
       begin
        error:=EBADF;
        goto _break2;
       end;
       PROC_LOCK();
       p_leader.p_flag:=p_leader.p_flag or P_ADVLOCK;
       PROC_UNLOCK();
       error:=VOP_ADVLOCK(vp, @p_leader, F_SETLK, flp, flg);
      end;
     F_UNLCK:
      begin
       error:=VOP_ADVLOCK(vp, @p_leader, F_UNLCK, flp, flg);
      end;
     F_UNLCKSYS:
      begin
       {
        * Temporary api for testing remote lock
        * infrastructure.
        }
       if (flg<>F_REMOTE) then
       begin
        error:=EINVAL;
         goto _break2;
       end;
       error:=VOP_ADVLOCK(vp, @p_leader, F_UNLCKSYS, flp, flg);
      end;
     else
      error:=EINVAL;
    end;
    _break2:
    VFS_UNLOCK_GIANT(vfslocked);
    vfslocked:=0;
   end;

  F_GETLK:
   begin
    error:=fdunwrap(fd, CAP_FLOCK, @fp);
    if (error<>0) then
    begin
     goto _break;
    end;
    if (fp^.f_type<>DTYPE_VNODE) then
    begin
     error:=EBADF;
     goto _break;
    end;
    flp:=p_flock(arg);
    if (flp^.l_type<>F_RDLCK) and
       (flp^.l_type<>F_WRLCK) and
       (flp^.l_type<>F_UNLCK) then
    begin
     error:=EINVAL;
     goto _break;
    end;
    if (flp^.l_whence=SEEK_CUR) then
    begin
     FILEDESC_SLOCK(@fd_table);
     foffset:=foffset_get(fp);
     if ((flp^.l_start > 0) and
         (foffset > High(Int64) - flp^.l_start)) or
        ((flp^.l_start < 0) and
         (foffset < Low(Int64) - flp^.l_start)) then
     begin
      FILEDESC_SUNLOCK(@fd_table);
      error:=EOVERFLOW;
      goto _break;
     end;
     Inc(flp^.l_start,foffset);
     FILEDESC_SUNLOCK(@fd_table);
    end;
    {
     * VOP_ADVLOCK() may block.
     }
    vp:=fp^.f_vnode;
    vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
    error:=VOP_ADVLOCK(vp, @p_leader, F_GETLK, flp, F_POSIX);
    VFS_UNLOCK_GIANT(vfslocked);
    vfslocked:=0;
   end;

  F_RDAHEAD:
   begin
    if (arg<>0) then
     arg:=128 * 1024
    else
     arg:=0;
    goto do_readahead;
   end;
  F_READAHEAD:
   begin
    do_readahead:
    fp:=fget_unlocked(fd);
    if (fp=nil) then
    begin
     error:=EBADF;
     goto _break;
    end;
    if (fp^.f_type<>DTYPE_VNODE) then
    begin
     error:=EBADF;
     goto _break;
    end;
    if (arg<>0) then
    begin
     vp:=fp^.f_vnode;
     vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
     error:=vn_lock(vp, LK_SHARED);
     if (error<>0) then
      goto readahead_vnlock_fail;
     bsize:=p_mount(fp^.f_vnode^.v_mount)^.mnt_stat.f_iosize;
     VOP_UNLOCK(vp, 0);
     fp^.f_seqcount:=(arg + bsize - 1) div bsize;
     repeat
      old:=fp^.f_flag;
      new:=old;
      new:=new or FRDAHEAD;
     until (System.InterlockedCompareExchange(fp^.f_flag,new,old)=old);
     readahead_vnlock_fail:
     VFS_UNLOCK_GIANT(vfslocked);
     vfslocked:=0;
    end else
    begin
     repeat
      old:=fp^.f_flag;
      new:=old;
      new:=new and (not FRDAHEAD);
     until (System.InterlockedCompareExchange(fp^.f_flag,new,old)=old);
    end;
   end;

  else
   error:=EINVAL;
 end;
 _break:

 if (fp<>nil) then
 begin
  fdrop(fp);
 end;

 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

function getmaxfd():Integer; inline;
begin
 Result:=lim_cur(RLIMIT_NOFILE);
end;

function closef(fp:p_file):Integer; forward;

{
 * Common code for dup, dup2, fcntl(F_DUPFD) and fcntl(F_DUP2FD).
 }
function do_dup(flags,old,new:Integer;retval:PQWORD):Integer;
var
 fp,delfp:p_file;
 maxfd:Integer;
begin
 {
  * Verify we have a valid descriptor to dup from and possibly to
  * dup to. Unlike dup() and dup2(), fcntl()'s F_DUPFD should
  * ExitEINVAL when the new descriptor is out of bounds.
  }
 if (old < 0) then
  Exit(EBADF);

 if (new < 0) then
 begin
  if (flags and DUP_FCNTL)<>0 then
   Exit(EINVAL)
  else
   Exit(EBADF);
 end;

 maxfd:=getmaxfd();
 if (new >= maxfd) then
 begin
  if (flags and DUP_FCNTL)<>0 then
   Exit(EINVAL)
  else
   Exit(EBADF);
 end;

 if ((flags and DUP_FIXED)<>0) and (old=new) then
 begin
  retval^:=new;
  if ((flags and DUP_CLOEXEC)<>0) then
  begin
   fp:=fget_unlocked(new);
   if (fp=nil) then
   begin
    Exit(EBADF);
   end;
   atomic_set_int(@fp^.f_exclose,UF_EXCLOSE);
   fdrop(fp);
  end;
  Exit(0);
 end;

 fp:=fget_unlocked(old);
 if (fp=nil) then
 begin
  Exit(EBADF);
 end;

 Assert(old<>new,'new fd is same as old');

 {
  * If the caller specified a file descriptor, make sure the file
  * table is large enough to hold it, and grab it.  Otherwise, just
  * allocate a new descriptor the usual way.  Since the filedesc
  * lock may be temporarily dropped in the process, we have to look
  * out for a race.
  }
 delfp:=nil;
 if ((flags and DUP_FIXED)<>0) then
 begin
  if not id_set(@fd_table.fd_ofiles,@fp^.desc,new,@delfp) then
  begin
   fdrop(fp);
   Exit(EBADF);
  end;
 end else
 begin
  if not id_new(@fd_table.fd_ofiles,@fp^.desc,@new) then
  begin
   fdrop(fp);
   Exit(ENFILE);
  end;
  fdrop(fp);
 end;
 fdrop(fp);


 retval^:=new;

 {
  * If we dup'd over a valid file, we now own the reference to it
  * and must dispose of it using closef() semantics (as if a
  * close() were performed on it).
  *
  * XXX this duplicates parts of close().
  }
 if (delfp<>nil) then
 begin
  //knote_fdclose(td, new);
  if (delfp^.f_type=DTYPE_MQUEUE) then
  begin
   //mq_fdclose(td, new, delfp);
  end;
  closef(delfp);
 end;
 Exit(0);
end;

function kern_close(fd:Integer):Integer;
var
 fp,fp_object:p_file;
begin
 fp:=nil;
 if not id_del(@fd_table.fd_ofiles,fd,@fp) then
 begin
  Exit(EBADF);
 end;

 {
  * We now hold the fp reference that used to be owned by the
  * descriptor array.  We have to unlock the FILEDESC *AFTER*
  * knote_fdclose to prevent a race of the fd getting opened, a knote
  * added, and deleteing a knote for the new fd.
  }
 //knote_fdclose(fd);

 {
  * When we're closing an fd with a capability, we need to notify
  * mqueue if the underlying object is of type mqueue.
  }
 if (cap_funwrap(fp, 0, @fp_object)=0) then
 begin
  if (fp_object^.f_type=DTYPE_MQUEUE) then
  begin
   //mq_fdclose(td, fd, fp_object);
  end;
  fdrop(fp_object);
 end;

 Exit(closef(fp));
end;

{
 * Close a file descriptor.
 }
function sys_close(fd:Integer):Integer;
begin
 Exit(kern_close(fd));
end;

{
 * Internal form of close.  Decrement reference count on file structure.
 * Note: td may be nil when closing a file that was being passed in a
 * message.
 *
 * XXXRW: Giant is not required for the caller, but often will be held; this
 * makes it moderately likely the Giant will be recursed in the VFS case.
 }
function closef(fp:p_file):Integer;
var
 vp:p_vnode;
 lf:t_flock;
 fp_object:p_file;
 vfslocked:Integer;
begin
 {
  * POSIX record locking dictates that any close releases ALL
  * locks owned by this process.  This is handled by setting
  * a flag in the unlock to free ONLY locks obeying POSIX
  * semantics, and not to free BSD-style file locks.
  * If the descriptor was in a message, POSIX-style locks
  * aren't passed with the descriptor, and the thread pointer
  * will be nil.  Callers should be careful only to pass a
  * nil thread pointer when there really is no owning
  * context that might have locks, or the locks will be
  * leaked.
  *
  * If this is a capability, we do lock processing under the underlying
  * node, not the capability itself.
  }
 if (cap_funwrap(fp, 0, @fp_object)=0) then
 begin
  if (fp_object^.f_type=DTYPE_VNODE) then
  begin
   vp:=fp_object^.f_vnode;
   vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
   if ((p_leader.p_flag and P_ADVLOCK)<>0) then
   begin
    lf.l_whence:=SEEK_SET;
    lf.l_start :=0;
    lf.l_len   :=0;
    lf.l_type  :=F_UNLCK;
    VOP_ADVLOCK(vp, @p_leader, F_UNLCK, @lf, F_POSIX);
   end;

   VFS_UNLOCK_GIANT(vfslocked);
  end;
  fdrop(fp_object);
 end;
 Exit(fdrop(fp));
end;

{
 * If a specific file object occupies a specific file descriptor, close the
 * file descriptor entry and drop a reference on the file object.  This is a
 * convenience function to handle a subsequent error in a function that calls
 * falloc() that handles the race that another thread might have closed the
 * file descriptor out from under the thread creating the file object.
}
procedure fdclose(fp:p_file;idx:Integer);
var
 tmp:p_file;
begin
 tmp:=p_file(id_get(@fd_table.fd_ofiles,idx));
 if (tmp=fp) then
 begin
  fdrop(tmp);
  fdrop(tmp);
 end else
 if (tmp<>nil) then
 begin
  fdrop(tmp);
 end;
end;

function kern_fstat(fd:Integer;sbp:p_stat):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget(fd, CAP_FSTAT, @fp);
 if (error<>0) then
  Exit(error);

 error:=fo_stat(fp, sbp);
 fdrop(fp);

 Exit(error);
end;

{
 * Exitstatus information about a file descriptor.
 }
function sys_fstat(fd:Integer;sb:p_stat):Integer;
var
 ub:t_stat;
 error:Integer;
begin
 error:=kern_fstat(fd, @ub);
 if (error=0) then
 begin
  error:=copyout(@ub, sb, sizeof(ub));
 end;
 Exit(error);
end;

{
 * Exitpathconf information about a file descriptor.
 }
function sys_fpathconf(fd,name:Integer):Integer;
label
 _out;
var
 td:p_kthread;
 fp:p_file;
 vp:p_vnode;
 error:Integer;
 vfslocked:Integer;
begin
 td:=curkthread;

 error:=fget(fd, CAP_FPATHCONF, @fp);
 if (error<>0) then
  Exit(error);

 { If asynchronous I/O is available, it works for all descriptors. }
 if (name=_PC_ASYNC_IO) then
 begin
  td^.td_retval[0]:=async_io_version;
  goto _out;
 end;
 vp:=fp^.f_vnode;
 if (vp<>nil) then
 begin
  vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
  vn_lock(vp, LK_SHARED or LK_RETRY);
  error:=VOP_PATHCONF(vp, name, td^.td_retval);
  VOP_UNLOCK(vp, 0);
  VFS_UNLOCK_GIANT(vfslocked);
 end else
 if (fp^.f_type=DTYPE_PIPE) or (fp^.f_type=DTYPE_SOCKET) then
 begin
  if (name<>_PC_PIPE_BUF) then
  begin
   error:=EINVAL;
  end else
  begin
   td^.td_retval[0]:=PIPE_BUF;
   error:=0;
  end;
 end else
 begin
  error:=EOPNOTSUPP;
 end;
_out:
 fdrop(fp);
 Exit(error);
end;

{
 * Apply an advisory lock on a file descriptor.
 *
 * Just attempt to get a record lock of the requested type on the entire file
 * (l_whence:=SEEK_SET, l_start:=0, l_len:=0).
 }
function sys_flock(fd,how:Integer):Integer;
label
 done2;
var
 fp:p_file;
 vp:p_vnode;
 lf:t_flock;
 vfslocked:Integer;
 error:Integer;
begin

 error:=fget(fd, CAP_FLOCK, @fp);
 if (error<>0) then
  Exit(error);
 if (fp^.f_type<>DTYPE_VNODE) then
 begin
  fdrop(fp);
  Exit(EOPNOTSUPP);
 end;

 vp:=fp^.f_vnode;
 vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
 lf.l_whence:=SEEK_SET;
 lf.l_start:=0;
 lf.l_len:=0;
 if ((how and LOCK_UN)<>0) then
 begin
  lf.l_type:=F_UNLCK;
  atomic_clear_int(@fp^.f_flag, FHASLOCK);
  error:=VOP_ADVLOCK(vp, fp, F_UNLCK, @lf, F_FLOCK);
  goto done2;
 end;
 if ((how and LOCK_EX)<>0) then
  lf.l_type:=F_WRLCK
 else
 if ((how and LOCK_SH)<>0) then
  lf.l_type:=F_RDLCK
 else
 begin
  error:=EBADF;
  goto done2;
 end;
 atomic_set_int(@fp^.f_flag, FHASLOCK);

 if ((how and LOCK_NB)<>0) then
 begin
  error:=VOP_ADVLOCK(vp, fp, F_SETLK, @lf, F_FLOCK);
 end else
 begin
  error:=VOP_ADVLOCK(vp, fp, F_SETLK, @lf, F_FLOCK or F_WAIT);
 end;

done2:
 fdrop(fp);
 VFS_UNLOCK_GIANT(vfslocked);
 Exit(error);
end;

{
 * Duplicate the specified descriptor to a free descriptor.
 }
function dupfdopen(indx,dfd,mode,error:Integer):Integer;
var
 wfp,fp:p_file;
begin
 {
  * If the to-be-dup'd fd number is greater than the allowed number
  * of file descriptors, or the fd to be dup'd has already been
  * closed, then reject.
  }

 wfp:=fget_unlocked(dfd);
 if (wfp=nil) then
 begin
  Exit(EBADF);
 end;

 {
  * There are two cases of interest here.
  *
  * For ENODEV simply dup (dfd) to file descriptor (indx) and return.
  *
  * For ENXIO steal away the file structure from (dfd) and store it in
  * (indx).  (dfd) is effectively closed by this operation.
  *
  * Any other error code is just returned.
  }
 case error of
  ENODEV:
   begin
    {
     * Check that the mode the file is being opened for is a
     * subset of the mode of the existing descriptor.
     }
    if (((mode and (FREAD or FWRITE)) or wfp^.f_flag)<>wfp^.f_flag) then
    begin
     fdrop(wfp);
     Exit(EACCES);
    end;

    fp:=nil;
    if not id_set(@fd_table.fd_ofiles,@wfp^.desc,indx,@fp) then
    begin
     fdrop(wfp);
     Exit(EBADF);
    end;

    if (fp<>nil) then
    begin
     fdrop(fp);
    end;

    fdrop(wfp);
    Exit(0);
   end;

  ENXIO:
   begin
    {
     * Steal away the file pointer from dfd and stuff it into indx.
     }
    fp:=nil;
    if not id_set(@fd_table.fd_ofiles,@wfp^.desc,indx,@fp) then
    begin
     fdrop(wfp);
     Exit(EBADF);
    end;

    if not id_del(@fd_table.fd_ofiles,dfd,nil) then
    begin
     fdrop(wfp);
     Exit(EBADF);
    end;

    if (fp<>nil) then
    begin
     fdrop(fp);
    end;

    fdrop(wfp);
    Exit(0);
   end;

  else
   begin
    fdrop(wfp);
    Exit(error);
   end;
 end;
 { NOTREACHED }
end;

{
 * Initialize the file pointer with the specified properties.
 *
 * The ops are set with release semantics to be certain that the flags, type,
 * and data are visible when ops is.  This is to prevent ops methods from being
 * called with bad data.
}
procedure finit(fp:p_file;flag:DWORD;_type:Word;data:Pointer;ops:p_fileops);
begin
 fp^.f_data:=data;
 fp^.f_flag:=flag;
 fp^.f_type:=_type;
 System.InterlockedExchange(fp^.f_ops,ops);
end;

procedure _fdrop(data:pointer); forward;

function falloc_noinstall(resultfp:pp_file):Integer;
var
 fp:p_file;
begin
 Assert(resultfp<>nil,'resultfp=nil');

 System.InterlockedIncrement(openfiles);

 fp:=AllocMem(SizeOf(t_file));
 if (fp=nil) then Exit(ENOMEM);

 fp^.desc.free:=@_fdrop;
 fp^.f_ops    :=@badfileops;
 fp^.f_data   :=nil;
 fp^.f_vnode  :=nil;

 resultfp^:=fp;
 Exit(0);
end;

function finstall(fp:p_file;fd:PInteger;flags:Integer):Integer;
begin
 Assert(fd<>nil,'fd=nil');
 Assert(fp<>nil,'fp=nil');

 if not id_new(@fd_table.fd_ofiles,@fp^.desc,fd) then
 begin
  Exit(ENFILE);
 end;

 if ((flags and O_CLOEXEC)<>0) then
 begin
  atomic_set_int(@fp^.f_exclose,UF_EXCLOSE);
 end;

 Exit(0);
end;

function falloc(resultfp:pp_file;resultfd:PInteger;flags:Integer):Integer;
var
 fp:p_file;
 error,fd:Integer;
begin
 error:=falloc_noinstall(@fp);
 if (error<>0) then
  Exit(error);  { no reference held on error }

 error:=finstall(fp,@fd,flags);
 if (error<>0) then
 begin
  _fdrop(fp);  { one reference (fp only) }
  Exit(error);
 end;

 if (resultfp<>nil) then
  resultfp^:=fp  { copy out result }
 else
  fdrop(fp);  { release local reference }

 if (resultfd<>nil) then
  resultfd^:=fd;

 Exit(0);
end;

procedure _fdrop(data:pointer);
var
 fp:p_file;
begin
 fp:=data;

 if (fp^.f_ops<>@badfileops) then
 begin
  fo_close(fp);
 end;

 System.InterlockedDecrement(openfiles);

 //FreeMem(fp^.f_advice);
 FreeMem(fp);
end;

procedure fhold(fp:p_file);
begin
 id_acqure(@fp^.desc);
end;

function fdrop(fp:p_file):Integer;
begin
 id_release(@fp^.desc);
 Result:=0;
end;

function fget_unlocked(fd:Integer):p_file;
begin
 if (fd<0) then Exit(nil);
 Result:=p_file(id_get(@fd_table.fd_ofiles,fd));
end;

function _fget(fd:Integer;
               fpp:pp_file;
               flags:Integer;
               needrights:cap_rights_t;
               haverightsp:p_cap_rights_t;
               maxprotp:PByte;
               fget_flags:Integer):Integer;
var
 fp:p_file;
 fp_fromcap:p_file;
 error:Integer;
begin
 fpp^:=nil;

 fp:=fget_unlocked(fd);
 if (fp=nil) then
  Exit(EBADF);

 if (fp^.f_ops=@badfileops) then
 begin
  fdrop(fp);
  Exit(EBADF);
 end;

 {
  * If this is a capability, what rights does it have?
  }
 if (haverightsp<>nil) then
 begin
  if (fp^.f_type=DTYPE_CAPABILITY) then
   haverightsp^:=cap_rights(fp)
  else
   haverightsp^:=CAP_MASK_VALID;
 end;

 {
  * If a capability has been requested, Exitthe capability directly.
  * Otherwise, check capability rights, extract the underlying object,
  * and check its access flags.
  }
 if ((fget_flags and FGET_GETCAP)<>0) then
 begin
  if (fp^.f_type<>DTYPE_CAPABILITY) then
  begin
   fdrop(fp);
   Exit(EINVAL);
  end;
 end else
 begin
  if (maxprotp=nil) then
   error:=cap_funwrap(fp, needrights,@fp_fromcap)
  else
   error:=cap_funwrap_mmap(fp, needrights, maxprotp,@fp_fromcap);

  if (error<>0) then
  begin
   fdrop(fp);
   Exit(error);
  end;

  {
   * If we've unwrapped a file, drop the original capability
   * and hold the new descriptor.  fp after this point refers to
   * the actual (unwrapped) object, not the capability.
   }
  if (fp<>fp_fromcap) then
  begin
   fhold(fp_fromcap);
   fdrop(fp);
   fp:=fp_fromcap;
  end;
 end;

 {
  * FREAD and FWRITE failure return EBADF as per POSIX.
  }
 error:=0;

 case (flags) of
  FREAD,
  FWRITE:
   begin
    if ((fp^.f_flag and flags)=0) then
     error:=EBADF;
   end;
  FEXEC:
   begin
    if ((fp^.f_flag and (FREAD or FEXEC))=0) or
       ((fp^.f_flag and FWRITE)<>0) then
    error:=EBADF;
   end;
  0:;
 else
  Assert(false,'wrong flags');
 end;

 if (error<>0) then
 begin
  fdrop(fp);
  Exit(error);
 end;

 fpp^:=fp;
 Exit(0);
end;

function fget(fd:Integer;
              rights:cap_rights_t;
              fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, 0, rights, nil, nil, 0));
end;

function fget_mmap(fd:Integer;
                   rights:cap_rights_t;
                   maxprotp:PByte;
                   fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, 0, rights, nil, maxprotp, 0));
end;

function fget_read(fd:Integer;
                   rights:cap_rights_t;
                   fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, FREAD, rights, nil, nil, 0));
end;

function fget_write(fd:Integer;
                    rights:cap_rights_t;
                    fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, FWRITE, rights, nil, nil, 0));
end;

{
 * Unlike the other fget() calls, which accept and check capability rights
 * but never Exitcapabilities, fgetcap() Exits the capability but doesn't
 * check capability rights.
}
function fgetcap(fd:Integer;
                 fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, 0, 0, nil, nil, FGET_GETCAP));
end;


{
 * Like fget() but loads the underlying vnode, or Exits an error if the
 * descriptor does not represent a vnode.  Note that pipes use vnodes but
 * never have VM objects.  The Exited vnode will be vref()'d.
 *
 * XXX: what about the unused flags ?
}
function _fgetvp(fd:Integer;
                 flags:Integer;
                 needrights:cap_rights_t;
                 haverightsp:p_cap_rights_t;
                 vpp:pp_vnode):Integer;
var
 fp:p_file;
 error:Integer;
begin
 vpp^:=nil;
 error:=_fget(fd, @fp, flags, needrights, haverightsp,nil, 0);
 if (error<>0) then
  Exit(error);
 if (fp^.f_vnode=nil) then
 begin
  error:=EINVAL;
 end else
 begin
  vpp^:=fp^.f_vnode;
  vref(vpp^);
 end;
 fdrop(fp);

 Exit(error);
end;

function fgetvp(fd:Integer;
                rights:cap_rights_t;
                vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, 0, rights, nil, vpp));
end;

function fgetvp_rights(fd:Integer;
                       need:cap_rights_t;
                       have:p_cap_rights_t;
                       vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, 0, need, have, vpp));
end;

function fgetvp_read(fd:Integer;
                     rights:cap_rights_t;
                     vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, FREAD, rights, nil, vpp));
end;

function fgetvp_exec(fd:Integer;
                     rights:cap_rights_t;
                     vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, FEXEC, rights, nil, vpp));
end;

function fgetvp_write(fd:Integer;
                      rights:cap_rights_t;
                      vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, FWRITE, rights, nil, vpp));
end;

{
 * Like fget() but loads the underlying socket, or Exits an error if the
 * descriptor does not represent a socket.
 *
 * We bump the ref count on the Exited socket.  XXX Also obtain the SX lock
 * in the future.
 *
 * Note: fgetsock() and fputsock() are deprecated, as consumers should rely
 * on their file descriptor reference to prevent the socket from being free'd
 * during use.
}
function fgetsock(fd:Integer;
                  rights:cap_rights_t;
                  spp:pp_socket;
                  fflagp:PDWORD):Integer;
var
 fp:p_file;
 error:Integer;
begin
 spp^:=nil;
 if (fflagp<>nil) then
  fflagp^:=0;
 error:=_fget(fd, @fp, 0, rights, nil, nil, 0);
 if (error<>0) then
  Exit(error);
 if (fp^.f_type<>DTYPE_SOCKET) then
 begin
  error:=ENOTSOCK;
 end else
 begin
  spp^:=fp^.f_data;
  if (fflagp<>nil) then
   fflagp^:=fp^.f_flag;
  SOCK_LOCK(spp^);
  soref(spp^);
  SOCK_UNLOCK(spp^);
 end;
 fdrop(fp);

 Exit(error);
end;

end.

