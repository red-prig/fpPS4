unit vsys_generic;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_mtx,
 kern_mtxpool,
 kern_condvar,
 kern_thr,
 vselinfo,
 vioccom,
 vpoll,
 vselect,
 vuio,
 vfile,
 vcapability,
 vfs_vnode,
 vfcntl,
 vfilio,
 vfiledesc,
 time,
 signal,
 vm,
 vmparam;

const
 SYS_IOCTL_SMALL_SIZE =128; { bytes }
 SYS_IOCTL_SMALL_ALIGN=8; { bytes }

 SELTD_PENDING=$0001;   { We have pending events. }
 SELTD_RESCAN =$0002;   { Doing a rescan. }

type
{
 * One seltd per-thread allocated on demand as needed.
 *
 * t - protected by st_mtx
 *  k - Only accessed by curthread or read-only
 }
 p_selfd=^t_selfd;

 p_seltd=^t_seltd;
 t_seltd=packed record
  st_selq :STAILQ_HEAD; { (k) List of selfds. }
  st_free1:p_selfd;     { (k) free fd for read set. }
  st_free2:p_selfd;     { (k) free fd for write set. }
  st_mtx  :mtx;         { Protects struct seltd }
  st_wait :t_cv;        { (t) Wait channel. }
  st_flags:Integer;     { (t) SELTD_ flags. }
 end;

 {
  * One selfd allocated per-thread per-file-descriptor.
  * f - protected by sf_mtx
  }
 t_selfd=packed record
  sf_link   :STAILQ_ENTRY; { (k) fds owned by this td. }
  sf_threads:TAILQ_ENTRY;  { (f) fds on this selinfo. }
  sf_si     :p_selinfo;    { (f) selinfo when linked. }
  sf_mtx    :p_mtx;        { Pointer to selinfo mtx. }
  sf_td     :p_seltd;      { (k) owning seltd. }
  sf_cookie :Pointer;      { (k) fd or pollfd. }
 end;

function  _pollout(td:p_kthread;fds,ufds:p_pollfd;nfd:DWORD):Integer;
function  pollscan(td:p_kthread;fds:p_pollfd;nfd:DWORD):Integer;
function  pollrescan(td:p_kthread):Integer;
function  selscan(td:p_kthread;ibits,obits:pp_fd_mask;nfd:Integer):Integer;
function  selrescan(td:p_kthread;ibits,obits:pp_fd_mask):Integer;
procedure selfdalloc(td:p_kthread;cookie:Pointer);
procedure selfdfree(stp:p_seltd;sfp:p_selfd);
procedure seldrain(sip:p_selinfo);
procedure selrecord(selector:p_kthread;sip:p_selinfo);
function  dofileread(fd:Integer;fp:p_file;auio:p_uio;offset:Int64;flags:Integer):Integer;
function  dofilewrite(fd:Integer;fp:p_file;auio:p_uio;offset:Int64;flags:Integer):Integer;
procedure doselwakeup(sip:p_selinfo;pri:Integer);
procedure seltdinit(td:p_kthread);
function  seltdwait(td:p_kthread;timo:Int64):Integer;
procedure seltdclear(td:p_kthread);
function  poll_no_poll(events:Integer):Integer;

//

function sys_read(fd:Integer;buf:Pointer;nbyte:QWORD):Integer;
function sys_pread(fd:Integer;buf:Pointer;nbyte:QWORD;offset:Int64):Integer;
function sys_readv(fd:Integer;iovp:p_iovec;iovcnt:DWORD):Integer;
function sys_preadv(fd:Integer;iovp:p_iovec;iovcnt:DWORD;offset:Int64):Integer;
function sys_write(fd:Integer;buf:Pointer;nbyte:QWORD):Integer;
function sys_pwrite(fd:Integer;buf:Pointer;nbyte:QWORD;offset:Int64):Integer;
function sys_writev(fd:Integer;iovp:p_iovec;iovcnt:DWORD):Integer;
function sys_pwritev(fd:Integer;iovp:p_iovec;iovcnt:DWORD;offset:Int64):Integer;
function sys_ftruncate(fd:Integer;length:Int64):Integer;
function sys_ioctl(fd:Integer;com:QWORD;data:Pointer):Integer;
function sys_pselect(nd:Integer;
                     uin,uou,uex:p_fd_set;
                     uts:ptimespec;
                     sm:p_sigset_t):Integer;
function sys_select(nd:Integer;
                    uin,uou,uex:p_fd_set;
                    utv:ptimeval):Integer;
function sys_poll(fds:p_pollfd;nfds:DWORD;timeout:Integer):Integer;

implementation

uses
 atomic,
 systm,
 errno,
 kern_descrip,
 subr_uio,
 kern_sig,
 kern_thread,
 kern_time,
 sys_capability;

var
 mtxpool_select:p_mtx_pool;

function kern_readv(fd:Integer;auio:p_uio):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget_read(fd, CAP_READ or CAP_SEEK, @fp);
 if (error<>0) then
  Exit(error);
 error:=dofileread(fd, fp, auio, -1, 0);
 fdrop(fp);
 Exit(error);
end;

function kern_preadv(fd:Integer;auio:p_uio;offset:Int64):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget_read(fd, CAP_READ, @fp);
 if (error<>0) then
  Exit(error);
 if ((fp^.f_ops^.fo_flags and DFLAG_SEEKABLE)=0) then
  error:=ESPIPE
 else
 if (offset < 0) and (fp^.f_vnode^.v_type<>VCHR) then
  error:=EINVAL
 else
  error:=dofileread(fd, fp, auio, offset, FOF_OFFSET);
 fdrop(fp);
 Exit(error);
end;

function kern_writev(fd:Integer;auio:p_uio):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget_write(fd, CAP_WRITE or CAP_SEEK, @fp);
 if (error<>0) then
  Exit(error);
 error:=dofilewrite(fd, fp, auio, -1, 0);
 fdrop(fp);
 Exit(error);
end;

function kern_pwritev(fd:Integer;auio:p_uio;offset:Int64):Integer;
var
 fp:p_file;
 error:Integer;
begin
 error:=fget_write(fd, CAP_WRITE, @fp);
 if (error<>0) then
  Exit(error);
 if ((fp^.f_ops^.fo_flags and DFLAG_SEEKABLE)=0) then
  error:=ESPIPE
 else
 if (offset < 0) and (fp^.f_vnode^.v_type<>VCHR) then
  error:=EINVAL
 else
  error:=dofilewrite(fd, fp, auio, offset, FOF_OFFSET);
 fdrop(fp);
 Exit(error);
end;

//

function sys_read(fd:Integer;buf:Pointer;nbyte:QWORD):Integer;
var
 auio:t_uio;
 aiov:iovec;
 error:Integer;
begin
 if (nbyte > IOSIZE_MAX) then
  Exit(EINVAL);
 aiov.iov_base  :=buf;
 aiov.iov_len   :=nbyte;
 auio.uio_iov   :=@aiov;
 auio.uio_iovcnt:=1;
 auio.uio_resid :=nbyte;
 auio.uio_segflg:=UIO_USERSPACE;
 error:=kern_readv(fd, @auio);
 Exit(error);
end;

{
 * Positioned read system call
 }
function sys_pread(fd:Integer;buf:Pointer;nbyte:QWORD;offset:Int64):Integer;
var
 auio:t_uio;
 aiov:iovec;
 error:Integer;
begin
 if (nbyte > IOSIZE_MAX) then
  Exit(EINVAL);
 aiov.iov_base  :=buf;
 aiov.iov_len   :=nbyte;
 auio.uio_iov   :=@aiov;
 auio.uio_iovcnt:=1;
 auio.uio_resid :=nbyte;
 auio.uio_segflg:=UIO_USERSPACE;
 error:=kern_preadv(fd, @auio, offset);
 Exit(error);
end;

{
 * Scatter read system call.
 }
function sys_readv(fd:Integer;iovp:p_iovec;iovcnt:DWORD):Integer;
var
 auio:p_uio;
 error:Integer;
begin
 error:=copyinuio(iovp, iovcnt, @auio);
 if (error<>0) then
  Exit(error);
 error:=kern_readv(fd, auio);
 FreeMem(auio);
 Exit(error);
end;

{
 * Scatter positioned read system call.
 }
function sys_preadv(fd:Integer;iovp:p_iovec;iovcnt:DWORD;offset:Int64):Integer;
var
 auio:p_uio;
 error:Integer;
begin
 error:=copyinuio(iovp, iovcnt, @auio);
 if (error<>0) then
  Exit(error);
 error:=kern_preadv(fd, auio, offset);
 FreeMem(auio);
 Exit(error);
end;

{
 * Common code for readv and preadv that reads data in
 * from a file using the passed in uio, offset, and flags.
 }
function dofileread(fd:Integer;fp:p_file;auio:p_uio;offset:Int64;flags:Integer):Integer;
var
 td:p_kthread;
 cnt:Int64;
 error:Integer;
begin
 td:=curkthread;
 { Finish zero length reads right here }
 if (auio^.uio_resid=0) then
 begin
  td^.td_retval[0]:=0;
  Exit(0);
 end;
 auio^.uio_rw:=UIO_READ;
 auio^.uio_offset:=offset;
 auio^.uio_td:=td;

 cnt:=auio^.uio_resid;
 error:=fo_read(fp, auio, flags);
 if (error<>0) then
 begin
  if (auio^.uio_resid<>cnt) and ((error=ERESTART) or (error=EINTR) or (error=EWOULDBLOCK)) then
   error:=0;
 end;
 Dec(cnt,auio^.uio_resid);

 td^.td_retval[0]:=cnt;
 Exit(error);
end;

function sys_write(fd:Integer;buf:Pointer;nbyte:QWORD):Integer;
var
 auio:t_uio;
 aiov:iovec;
 error:Integer;
begin
 if (nbyte > IOSIZE_MAX) then
  Exit(EINVAL);
 aiov.iov_base  :=buf;
 aiov.iov_len   :=nbyte;
 auio.uio_iov   :=@aiov;
 auio.uio_iovcnt:=1;
 auio.uio_resid :=nbyte;
 auio.uio_segflg:=UIO_USERSPACE;
 error:=kern_writev(fd, @auio);
 Exit(error);
end;

{
 * Positioned write system call.
 }
function sys_pwrite(fd:Integer;buf:Pointer;nbyte:QWORD;offset:Int64):Integer;
var
 auio:t_uio;
 aiov:iovec;
 error:Integer;
begin
 if (nbyte > IOSIZE_MAX) then
  Exit(EINVAL);
 aiov.iov_base  :=buf;
 aiov.iov_len   :=nbyte;
 auio.uio_iov   :=@aiov;
 auio.uio_iovcnt:=1;
 auio.uio_resid :=nbyte;
 auio.uio_segflg:=UIO_USERSPACE;
 error:=kern_pwritev(fd, @auio, offset);
 Exit(error);
end;

{
 * Gather write system call.
 }
function sys_writev(fd:Integer;iovp:p_iovec;iovcnt:DWORD):Integer;
var
 auio:p_uio;
 error:Integer;
begin
 error:=copyinuio(iovp, iovcnt, @auio);
 if (error<>0) then
  Exit(error);
 error:=kern_writev(fd, auio);
 FreeMem(auio);
 Exit(error);
end;

{
 * Gather positioned write system call.
 }
function sys_pwritev(fd:Integer;iovp:p_iovec;iovcnt:DWORD;offset:Int64):Integer;
var
 auio:p_uio;
 error:Integer;
begin
 error:=copyinuio(iovp, iovcnt, @auio);
 if (error<>0) then
  Exit(error);
 error:=kern_pwritev(fd, auio, offset);
 FreeMem(auio);
 Exit(error);
end;

{
 * Common code for writev and pwritev that writes data to
 * a file using the passed in uio, offset, and flags.
 }
function dofilewrite(fd:Integer;fp:p_file;auio:p_uio;offset:Int64;flags:Integer):Integer;
var
 td:p_kthread;
 cnt:Int64;
 error:Integer;
begin
 td:=curkthread;

 auio^.uio_rw    :=UIO_WRITE;
 auio^.uio_td    :=td;
 auio^.uio_offset:=offset;

 cnt:=auio^.uio_resid;
 //if (fp^.f_type=DTYPE_VNODE) and
 //   ((fp^.f_vnread_flags and FDEVFS_VNODE)=0) then
 // bwillwrite();
 error:=fo_write(fp, auio, flags);
 if (error<>0) then
 begin
  if (auio^.uio_resid<>cnt) and ((error=ERESTART) or (error=EINTR) or (error=EWOULDBLOCK)) then
   error:=0;
  { Socket layer is responsible for issuing SIGPIPE. }
  if (fp^.f_type<>DTYPE_SOCKET) and (error=EPIPE) then
  begin
   PROC_LOCK();
   tdsignal(td, SIGPIPE);
   PROC_UNLOCK();
  end;
 end;
 Dec(cnt,auio^.uio_resid);

 td^.td_retval[0]:=cnt;
 Exit(error);
end;

{
 * Truncate a file given a file descriptor.
 *
 * Can't use fget_write() here, since must ExitEINVAL and not EBADF if the
 * descriptor isn't writable.
 }
function kern_ftruncate(fd:Integer;length:Int64):Integer;
var
 fp:p_file;
 error:Integer;
begin
 if (length < 0) then
  Exit(EINVAL);
 error:=fget(fd, CAP_FTRUNCATE, @fp);
 if (error<>0) then
  Exit(error);
 if ((fp^.f_flag and FWRITE)=0) then
 begin
  fdrop(fp);
  Exit(EINVAL);
 end;
 error:=fo_truncate(fp, length);
 fdrop(fp);
 Exit(error);
end;

function sys_ftruncate(fd:Integer;length:Int64):Integer;
begin
 Exit(kern_ftruncate(fd, length));
end;

function kern_ioctl(fd:Integer;com:QWORD;data:Pointer):Integer;
label
 _out;
var
 fp:p_file;
 error:Integer;
 tmp:Integer;
begin
 error:=fget(fd, CAP_IOCTL, @fp);
 if (error<>0) then
  Exit(error);
 if ((fp^.f_flag and (FREAD or FWRITE))=0) then
 begin
  fdrop(fp);
  Exit(EBADF);
 end;
 case com of
  FIONCLEX:
   begin
    atomic_clear_int(@fp^.f_exclose,UF_EXCLOSE);
    goto _out;
   end;
  FIOCLEX:
   begin
    atomic_set_int(@fp^.f_exclose,UF_EXCLOSE);
    goto _out;
   end;
  FIONBIO:
   begin
    tmp:=PInteger(data)^;
    if (tmp<>0) then
     atomic_set_int(@fp^.f_flag, FNONBLOCK)
    else
     atomic_clear_int(@fp^.f_flag, FNONBLOCK);
    data:=@tmp;
   end;
  FIOASYNC:
   begin
    tmp:=PInteger(data)^;
    if (tmp<>0) then
     atomic_set_int(@fp^.f_flag, FASYNC)
    else
     atomic_clear_int(@fp^.f_flag, FASYNC);
    data:=@tmp;
   end;
 end;
 error:=fo_ioctl(fp, com, data);
_out:
 fdrop(fp);
 Exit(error);
end;

function sys_ioctl(fd:Integer;com:QWORD;data:Pointer):Integer;
label
 _out;
var
 smalldata:array[0..SYS_IOCTL_SMALL_SIZE-1] of Byte; //__aligned(SYS_IOCTL_SMALL_ALIGN)
 arg,error:Integer;
 size:DWORD;
begin
 if (com > $ffffffff) then
 begin
  Writeln('WARNING pid %d (%s): ioctl sign-extension ioctl ',com);
  com:=com and $ffffffff;
 end;

 {
  * Interpret high order word to find amount of data to be
  * copied to/from the user's address space.
  }
 size:=IOCPARM_LEN(com);
 if (size > IOCPARM_MAX) or
    ((com and (IOC_VOID or IOC_IN or IOC_OUT))=0) or
    (((com and (IOC_IN or IOC_OUT))<>0) and (size=0)) or
    (((com and IOC_VOID)<>0) and (size > 0) and (size<>sizeof(Integer))) then
  Exit(ENOTTY);

 if (size > 0) then
 begin
  if ((com and IOC_VOID)<>0) then
  begin
   { Integer argument. }
   arg:=ptrint(data);
   data:=@arg;
   size:=0;
  end else
  begin
   if (size > SYS_IOCTL_SMALL_SIZE) then
    data:=AllocMem(size)
   else
    data:=@smalldata;
  end;
 end else
  data:=@data;
 if ((com and IOC_IN)<>0) then
 begin
  error:=copyin(data, data, size);
  if (error<>0) then
   goto _out;
 end else
 if ((com and IOC_OUT)<>0) then
 begin
  {
   * Zero the buffer so the user always
   * gets back something deterministic.
   }
  FillChar(data,size,0);
 end;

 error:=kern_ioctl(fd, com, data);

 if (error=0) and ((com and IOC_OUT)<>0) then
  error:=copyout(data, data, size);

_out:
 if (size > SYS_IOCTL_SMALL_SIZE) then
  FreeMem(data);
 Exit(error);
end;

function poll_no_poll(events:Integer):Integer;
begin
 {
  * return true for read/write.  If the user asked for something
  * special, return POLLNVAL, so that clients have a way of
  * determining reliably whether or not the extended
  * functionality is present without hard-coding knowledge
  * of specific filesystem implementations.
  }
 if (events and (not POLLSTANDARD))<>0 then
  Exit(POLLNVAL);

 Exit(events and (POLLIN or POLLOUT or POLLRDNORM or POLLWRNORM));
end;

function kern_select(nd:Integer;
                     fd_in,fd_ou,fd_ex:p_fd_set;
                     tvp:ptimeval;
                     abi_nfdbits:Integer):Integer; forward;

function kern_pselect(nd:Integer;
                      uin,uou,uex:p_fd_set;
                      tvp:ptimeval;
                      uset:p_sigset_t;
                      abi_nfdbits:Integer):Integer;
var
 td:p_kthread;
 error:Integer;
begin
 td:=curkthread;
 if (uset<>nil) then
 begin
  error:=kern_sigprocmask(td, SIG_SETMASK, uset, @td^.td_oldsigmask, 0);
  if (error<>0) then
   Exit(error);
  td^.td_pflags:=td^.td_pflags or TDP_OLDMASK;
  {
   * Make sure that ast() is called on Exitto
   * usermode and TDP_OLDMASK is cleared, restoring old
   * sigmask.
   }
  thread_lock(td);
  td^.td_flags:=td^.td_flags or TDF_ASTPENDING;
  thread_unlock(td);
 end;
 error:=kern_select(nd, uin, uou, uex, tvp, abi_nfdbits);
 Exit(error);
end;

function sys_pselect(nd:Integer;
                     uin,uou,uex:p_fd_set;
                     uts:ptimespec;
                     sm:p_sigset_t):Integer;
var
 ts:timespec;
 tv:timeval;
 tvp:ptimeval;
 _set:sigset_t;
 uset:p_sigset_t;
 error:Integer;
begin
 if (uts<>nil) then
 begin
  error:=copyin(uts, @ts, sizeof(ts));
  if (error<>0) then
   Exit(error);
  TIMESPEC_TO_TIMEVAL(@tv, @ts);
  tvp:=@tv;
 end else
  tvp:=nil;
 if (sm<>nil) then
 begin
  error:=copyin(sm, @_set, sizeof(_set));
  if (error<>0) then
   Exit(error);
  uset:=@_set;
 end else
  uset:=nil;
 Exit(kern_pselect(nd, uin, uou, uex, tvp, uset, NFDBITS));
end;

function sys_select(nd:Integer;
                    uin,uou,uex:p_fd_set;
                    utv:ptimeval):Integer;
var
 tv:timeval;
 tvp:ptimeval;
 error:Integer;
begin
 if (utv<>nil) then
 begin
  error:=copyin(utv, @tv, sizeof(tv));
  if (error<>0) then
   Exit(error);
  tvp:=@tv;
 end else
  tvp:=nil;

 Exit(kern_select(nd, uin, uou, uex, tvp, NFDBITS));
end;

{
 * In the unlikely case when user specified n greater then the last
 * open file descriptor, check that no bits are set after the last
 * valid fd.  We must ExitEBADF if any is set.
 *
 * There are applications that rely on the behaviour.
 *
 * nd is fd_lastfile + 1.
 }
function select_check_badfd(fd_in:p_fd_set;nd,ndu,abi_nfdbits:Integer):Integer;
var
 addr,oaddr:PByte;
 b,i,res:Integer;
 bits:Byte;
begin
 if (nd >= ndu) or (fd_in=nil) then
  Exit(0);

 oaddr:=nil;
 bits:=0; { silence gcc }
 For i:=nd to ndu-1 do
 begin
  b:=i div NBBY;
  addr:=PByte(fd_in) + b;
  if (addr<>oaddr) then
  begin
   res:=fubyte(addr^);
   if (res=-1) then
    Exit(EFAULT);
   oaddr:=addr;
   bits:=res;
  end;
  if ((bits and (1 shl (i mod NBBY)))<>0) then
   Exit(EBADF);
 end;
 Exit(0);
end;

function kern_select(nd:Integer;
                     fd_in,fd_ou,fd_ex:p_fd_set;
                     tvp:ptimeval;
                     abi_nfdbits:Integer):Integer;
label
 done;
var
 td:p_kthread;
 {
  * The magic 2048 here is chosen to be just enough for FD_SETSIZE
  * infds with the new FD_SETSIZE of 1024, and more than enough for
  * FD_SETSIZE infds, outfds and exceptfds with the old FD_SETSIZE
  * of 256.
  }
 s_selbits:array[0..((2048 + (NFDBITS - 1)) div NFDBITS)-1] of fd_mask;
 ibits,obits,selbits:p_fd_mask;
 _atv:timeval;
 atv,rtv,ttv,timo:Int64;
 error,lf,ndu:Integer;
 nbufbytes,ncpbytes,ncpubytes,_nfdbits:DWORD;

 procedure putbits(name:p_fd_set;x:Integer);
 var
  error2:Integer;
 begin
  if (name<>nil) then
  begin
   error2:=copyout(@obits[x], name, ncpubytes);
   if (error2<>0) then
    error:=error2;
  end;
 end;

begin
 if (nd < 0) then
  Exit(EINVAL);

 td:=curkthread;
 ndu:=nd;

 //lf:=fd_table.fd_lastfile;
 //if (nd > lf + 1)
 // nd:=lf + 1;

 error:=select_check_badfd(fd_in, nd, ndu, abi_nfdbits);
 if (error<>0) then
  Exit(error);
 error:=select_check_badfd(fd_ou, nd, ndu, abi_nfdbits);
 if (error<>0) then
  Exit(error);
 error:=select_check_badfd(fd_ex, nd, ndu, abi_nfdbits);
 if (error<>0) then
  Exit(error);

 {
  * Allocate just enough bits for the non-nil fd_sets.  Use the
  * preallocated auto buffer if possible.
  }
 _nfdbits:=roundup(nd, NFDBITS);
 ncpbytes:=_nfdbits div NBBY;
 ncpubytes:=roundup(nd, abi_nfdbits) div NBBY;
 nbufbytes:=0;

 if (fd_in<>nil) then
  Inc(nbufbytes,2 * ncpbytes);
 if (fd_ou<>nil) then
  Inc(nbufbytes,2 * ncpbytes);
 if (fd_ex<>nil) then
  Inc(nbufbytes,2 * ncpbytes);

 if (nbufbytes <= sizeof(s_selbits)) then
  selbits:=@s_selbits[0]
 else
  selbits:=AllocMem(nbufbytes);

 {
  * Assign pointers into the bit buffers and fetch the input bits.
  * Put the output buffers together so that they can be bzeroed
  * together.
  }
 if (nbufbytes<>0) then
  FillChar(selbits, nbufbytes div 2,0);

 if (tvp<>nil) then
 begin
  _atv:=tvp^;
  if (itimerfix(@_atv)<>0) then
  begin
   error:=EINVAL;
   goto done;
  end;
  atv:=TIMEVAL_TO_UNIT(@_atv);
  rtv:=get_unit_uptime;
  atv:=atv+rtv;
 end else
 begin
  atv:=0;
 end;
 timo:=0;
 seltdinit(td);
 { Iterate until the timeout expires or descriptors become ready. }
 repeat
  error:=selscan(td, @ibits, @obits, nd);
  if (error<>0) or (td^.td_retval[0]<>0) then
   break;
  if (atv<>0) then
  begin
   rtv:=get_unit_uptime;
   if (rtv>=atv) then
    break;
   ttv:=atv-rtv;

   if (ttv>24*60*60*hz) then
    timo:=24*60*60*hz
   else
    timo:=tvtohz(ttv);
  end;
  error:=seltdwait(td, timo);
  if (error<>0) then
   break;
  error:=selrescan(td, @ibits, @obits);
  if (error<>0) or (td^.td_retval[0]<>0) then
   break;
 until false;
 seltdclear(td);

done:
 { select is not restarted after signals... }
 if (error=ERESTART) then
  error:=EINTR;
 if (error=EWOULDBLOCK) then
  error:=0;

 if (error=0) then
 begin
  putbits(fd_in, 0);
  putbits(fd_ou, 1);
  putbits(fd_ex, 2);
 end;

 if (selbits<>@s_selbits[0]) then
  FreeMem(selbits);

 Exit(error);
end;
{
 * Convert a select bit set to poll flags.
 *
 * The backend always Exits POLLHUP/POLLERR if appropriate and we
 * Exitthis as a set bit in any set.
 }
const
 select_flags:array[0..2] of Integer=(
  POLLRDNORM or POLLHUP or POLLERR,
  POLLWRNORM or POLLHUP or POLLERR,
  POLLRDBAND or POLLERR);

{
 * Compute the fo_poll flags required for a fd given by the index and
 * bit position in the fd_mask array.
 }
function selflags(ibits:pp_fd_mask;idx:Integer;bit:fd_mask):Integer;
var
 flags,msk:Integer;
begin
 flags:=0;
 For msk:=0 to 2 do
 begin
  if (ibits[msk]=nil) then
   continue;
  if ((ibits[msk][idx] and bit)=0) then
   continue;
  flags:=flags or select_flags[msk];
 end;
 Exit(flags);
end;

{
 * Set the appropriate output bits given a mask of fired events and the
 * input bits originally requested.
 }
function selsetbits(ibits,obits:pp_fd_mask;idx:Integer;bit:fd_mask;events:Integer):Integer;
var
 msk,n:Integer;
begin
 n:=0;
 For msk:=0 to 2 do
 begin
  if ((events and select_flags[msk])=0) then
   continue;
  if (ibits[msk]=nil) then
   continue;
  if ((ibits[msk][idx] and bit)=0) then
   continue;
  {
   * XXX Check for a duplicate set.  This can occur because a
   * socket calls selrecord() twice for each poll() call
   * resulting in two selfds per real fd.  selrescan() will
   * call selsetbits twice as a result.
   }
  if ((obits[msk][idx] and bit)<>0) then
   continue;
  obits[msk][idx]:=obits[msk][idx] or bit;
  Inc(n);
 end;

 Exit(n);
end;

function getselfd_cap(fd:Integer;fpp:pp_file):Integer;
var
 fp:p_file;
 fp_fromcap:p_file;
 error:Integer;
begin
 fp:=fget_unlocked(fd);
 if (fp=nil) then
  Exit(EBADF);
 {
  * If the file descriptor is for a capability, test rights and use
  * the file descriptor references by the capability.
  }
 error:=cap_funwrap(fp, CAP_POLL_EVENT, @fp_fromcap);
 if (error<>0) then
 begin
  fdrop(fp);
  Exit(error);
 end;
 if (fp<>fp_fromcap) then
 begin
  fhold(fp_fromcap);
  fdrop(fp);
  fp:=fp_fromcap;
 end;
 fpp^:=fp;
 Exit(0);
end;

{
 * Traverse the list of fds attached to this thread's seltd and check for
 * completion.
 }
function selrescan(td:p_kthread;ibits,obits:pp_fd_mask):Integer;
var
 si:p_selinfo;
 stp:p_seltd;
 sfp:p_selfd;
 sfn:p_selfd;
 fp:p_file;
 bit:fd_mask;
 fd,ev,n,idx:Integer;
 error:Integer;
begin
 stp:=td^.td_sel;
 n:=0;

 sfp:=STAILQ_FIRST(@stp^.st_selq);
 while (sfp<>nil) do
 begin
  sfn:=STAILQ_NEXT(sfp,@sfp^.sf_link);
  //
  fd:=ptrint(sfp^.sf_cookie);
  si:=sfp^.sf_si;
  selfdfree(stp, sfp);
  { If the selinfo wasn't cleared the event didn't fire. }
  if (si<>nil) then
   continue;
  error:=getselfd_cap(fd, @fp);
  if (error<>0) then
   Exit(error);
  idx:=fd div NFDBITS;
  bit:=fd_mask(1) shl (fd mod NFDBITS);
  ev:=fo_poll(fp, selflags(ibits, idx, bit));
  fdrop(fp);
  if (ev<>0) then
   Inc(n,selsetbits(ibits, obits, idx, bit, ev));
  //
  sfp:=sfn;
 end;
 stp^.st_flags:=0;
 td^.td_retval[0]:=n;
 Exit(0);
end;

{
 * Perform the initial filedescriptor scan and register ourselves with
 * each selinfo.
 }
function selscan(td:p_kthread;ibits,obits:pp_fd_mask;nfd:Integer):Integer;
var
 fp:p_file;
 bit:fd_mask;
 ev,flags,_end,fd:Integer;
 n,idx:Integer;
 error:Integer;
begin
 n:=0;
 idx:=0;
 fd:=0;
 while (fd < nfd) do
 begin
  _end:=fd+NFDBITS;
  if (_end>nfd) then _end:=nfd;
  bit:=1;
  while (fd < _end) do
  begin
   { Compute the list of events we're interested in. }
   flags:=selflags(ibits, idx, bit);
   if (flags=0) then
    continue;
   error:=getselfd_cap(fd, @fp);
   if (error<>0) then
    Exit(error);
   selfdalloc(td, Pointer(ptrint(fd)));
   ev:=fo_poll(fp, flags);
   fdrop(fp);
   if (ev<>0) then
    Inc(n,selsetbits(ibits, obits, idx, bit, ev));
   //
   bit:=bit shl 1;
   Inc(fd);
  end;
  //
  Inc(idx);
 end;

 td^.td_retval[0]:=n;
 Exit(0);
end;

function sys_poll(fds:p_pollfd;nfds:DWORD;timeout:Integer):Integer;
label
 done,
 _out;
var
 td:p_kthread;
 bits:p_pollfd;
 smallbits:array[0..31] of t_pollfd;
 atv,rtv,ttv,timo:Int64;
 error:Integer;
 ni:QWORD;
begin
 if (nfds > maxfilesperproc) and (nfds > FD_SETSIZE) then
  Exit(EINVAL);
 td:=curkthread;
 ni:=nfds * sizeof(t_pollfd);
 if (ni > sizeof(smallbits)) then
  bits:=AllocMem(ni)
 else
  bits:=@smallbits;
 error:=copyin(fds, bits, ni);
 if (error<>0) then
  goto done;
 if (timeout<>INFTIM) then
 begin
  atv:=USEC_TO_UNIT(_msec2usec(timeout));
  rtv:=get_unit_uptime;
  atv:=atv+rtv;
 end else
 begin
  atv:=0;
 end;
 timo:=0;
 seltdinit(td);
 { Iterate until the timeout expires or descriptors become ready. }
 repeat
  error:=pollscan(td, bits, nfds);
  if (error<>0) or (td^.td_retval[0]<>0) then
   break;
  if (atv<>0) then
  begin
   rtv:=get_unit_uptime;
   if (rtv>=atv) then
    break;
   ttv:=atv-rtv;

   if (ttv>24*60*60*hz) then
    timo:=24*60*60*hz
   else
    timo:=tvtohz(ttv);
  end;
  error:=seltdwait(td, timo);
  if (error<>0) then
   break;
  error:=pollrescan(td);
  if (error<>0) or (td^.td_retval[0]<>0) then
   break;
 until false;
 seltdclear(td);

done:
 { poll is not restarted after signals... }
 if (error=ERESTART) then
  error:=EINTR;
 if (error=EWOULDBLOCK) then
  error:=0;
 if (error=0) then
 begin
  error:=_pollout(td, bits, fds, nfds);
  if (error<>0) then
   goto _out;
 end;
_out:
 if (ni > sizeof(smallbits)) then
  FreeMem(bits);
 Exit(error);
end;

function pollrescan(td:p_kthread):Integer;
var
 stp:p_seltd;
 sfp:p_selfd;
 sfn:p_selfd;
 si:p_selinfo;
 fp:p_file;
 fd:p_pollfd;
 n:Integer;
begin
 n:=0;
 stp:=td^.td_sel;
 FILEDESC_SLOCK(@fd_table);
 sfp:=STAILQ_FIRST(@stp^.st_selq);
 while (sfp<>nil) do
 begin
  sfn:=STAILQ_NEXT(sfp,@sfp^.sf_link);
  //
  fd:=p_pollfd(sfp^.sf_cookie);
  si:=sfp^.sf_si;
  selfdfree(stp, sfp);
  { If the selinfo wasn't cleared the event didn't fire. }
  if (si<>nil) then
   continue;
  fp:=fget_unlocked(fd^.fd);
  if (fp=nil) or (cap_funwrap(fp, CAP_POLL_EVENT, @fp)<>0) then
  begin
   if (fp<>nil) then
    fdrop(fp);
   //
   fd^.revents:=POLLNVAL;
   Inc(n);
   continue;
  end;
  {
   * Note: backend also Exits POLLHUP and
   * POLLERR if appropriate.
   }
  fd^.revents:=fo_poll(fp, fd^.events);
  if (fd^.revents<>0) then
   Inc(n);
  //
  fdrop(fp);
  //
  sfp:=sfn;
 end;
 FILEDESC_SUNLOCK(@fd_table);
 stp^.st_flags:=0;
 td^.td_retval[0]:=n;
 Exit(0);
end;

function _pollout(td:p_kthread;fds,ufds:p_pollfd;nfd:DWORD):Integer;
var
 error:Integer;
 i,n:DWORD;
begin
 error:=0;
 i:=0;
 n:=0;
 For i:=0 to nfd-1 do
 begin
  error:=copyout(@fds^.revents, @ufds^.revents, sizeof(ufds^.revents));
  if (error<>0) then
   Exit(error);
  if (fds^.revents<>0) then
   Inc(n);
  Inc(fds);
  Inc(ufds);
 end;
 td^.td_retval[0]:=n;
 Exit(0);
end;

function pollscan(td:p_kthread;fds:p_pollfd;nfd:DWORD):Integer;
var
 i:Integer;
 fp:p_file;
 n:Integer;
begin
 n:=0;
 i:=0;
 FILEDESC_SLOCK(@fd_table);
 While (i < nfd) do
 begin
  //if (fds^.fd >= fd_table.fd_nfiles) then
  //begin
  // fds^.revents:=POLLNVAL;
  // Inc(n);
  //end else
  if (fds^.fd < 0) then
  begin
   fds^.revents:=0;
  end else
  begin
   fp:=fget_unlocked(fds^.fd);
   if ((fp=nil) or (cap_funwrap(fp, CAP_POLL_EVENT, @fp)<>0)) then
   begin
    fds^.revents:=POLLNVAL;
    Inc(n);
   end else
   begin
    {
     * Note: backend also Exits POLLHUP and
     * POLLERR if appropriate.
     }
    selfdalloc(td, fds);
    fds^.revents:=fo_poll(fp, fds^.events);
    {
     * POSIX requires POLLOUT to be never
     * set simultaneously with POLLHUP.
     }
    if ((fds^.revents and POLLHUP)<>0) then
     fds^.revents:=fds^.revents and (not POLLOUT);

    if (fds^.revents<>0) then
     Inc(n);
   end;
   //
   if (fp<>nil) then
    fdrop(fp);
  end;
  //
  Inc(i);
  Inc(fds);
 end;
 FILEDESC_SUNLOCK(@fd_table);
 td^.td_retval[0]:=n;
 Exit(0);
end;

{
 * Preallocate two selfds associated with 'cookie'.  Some fo_poll routines
 * have two select sets, one for read and another for write.
 }
procedure selfdalloc(td:p_kthread;cookie:Pointer);
var
 stp:p_seltd;
begin
 stp:=td^.td_sel;
 if (stp^.st_free1=nil) then
  stp^.st_free1:=AllocMem(SizeOf(t_selfd));
 stp^.st_free1^.sf_td:=stp;
 stp^.st_free1^.sf_cookie:=cookie;
 if (stp^.st_free2=nil) then
  stp^.st_free2:=AllocMem(SizeOf(t_selfd));
 stp^.st_free2^.sf_td:=stp;
 stp^.st_free2^.sf_cookie:=cookie;
end;

procedure selfdfree(stp:p_seltd;sfp:p_selfd);
begin
 STAILQ_REMOVE(@stp^.st_selq,sfp,@sfp^.sf_link);
 mtx_lock(sfp^.sf_mtx^);
 if (sfp^.sf_si<>nil) then
  TAILQ_REMOVE(@sfp^.sf_si^.si_tdlist,sfp,@sfp^.sf_threads);
 mtx_unlock(sfp^.sf_mtx^);
 FreeMem(sfp);
end;

{ Drain the waiters tied to all the selfd belonging the specified selinfo. }
procedure seldrain(sip:p_selinfo);
begin
 {
  * This feature is already provided by doselwakeup(), thus it is
  * enough to go for it.
  * Eventually, the context, should take care to avoid races
  * between thread calling select()/poll() and file descriptor
  * detaching, but, again, the races are just the same as
  * selwakeup().
  }
 doselwakeup(sip, -1);
end;

{
 * Record a select request.
 }
procedure selrecord(selector:p_kthread;sip:p_selinfo);
var
 sfp:p_selfd;
 stp:p_seltd;
 mtxp:p_mtx;
begin
 stp:=selector^.td_sel;
 {
  * Don't record when doing a rescan.
  }
 if ((stp^.st_flags and SELTD_RESCAN)<>0) then
  Exit;
 {
  * Grab one of the preallocated descriptors.
  }
 sfp:=stp^.st_free1;
 if (sfp<>nil) then
  stp^.st_free1:=nil
 else
 begin
  sfp:=stp^.st_free2;
  if (sfp<>nil) then
   stp^.st_free2:=nil
  else
  begin
   Assert(false,'selrecord: No free selfd on selq');
   Exit;
  end;
 end;
 mtxp:=sip^.si_mtx;
 if (mtxp=nil) then
  mtxp:=mtx_pool_find(mtxpool_select, sip);
 {
  * Initialize the sfp and queue it in the thread.
  }
 sfp^.sf_si:=sip;
 sfp^.sf_mtx:=mtxp;
 STAILQ_INSERT_TAIL(@stp^.st_selq,sfp,@sfp^.sf_link);
 {
  * Now that we've locked the sip, check for initialization.
  }
 mtx_lock(mtxp^);
 if (sip^.si_mtx=nil) then
 begin
  sip^.si_mtx:=mtxp;
  TAILQ_INIT(@sip^.si_tdlist);
 end;
 {
  * Add this thread to the list of selfds listening on this selinfo.
  }
 TAILQ_INSERT_TAIL(@sip^.si_tdlist,sfp,@sfp^.sf_threads);
 mtx_unlock(sip^.si_mtx^);
end;

{ Wake up a selecting thread. }

procedure selwakeup(sip:p_selinfo);
begin
 doselwakeup(sip, -1);
end;

{ Wake up a selecting thread, and set its priority. }
procedure selwakeuppri(sip:p_selinfo;pri:Integer);
begin
 doselwakeup(sip, pri);
end;

{
 * Do a wakeup when a selectable event occurs.
 }
procedure doselwakeup(sip:p_selinfo;pri:Integer);
var
 sfp:p_selfd;
 sfn:p_selfd;
 stp:p_seltd;
begin
 { If it's not initialized there can't be any waiters. }
 if (sip^.si_mtx=nil) then
  Exit;
 {
  * Locking the selinfo locks all selfds associated with it.
  }
 mtx_lock(sip^.si_mtx^);
 sfp:=TAILQ_FIRST(@sip^.si_tdlist);
 while (sfp<>nil) do
 begin
  sfn:=TAILQ_NEXT(sfp,@sfp^.sf_threads);
  {
   * Once we remove this sfp from the list and clear the
   * sf_si seltdclear will know to ignore this si.
   }
  TAILQ_REMOVE(@sip^.si_tdlist,sfp,@sfp^.sf_threads);
  sfp^.sf_si:=nil;
  stp:=sfp^.sf_td;
  mtx_lock(stp^.st_mtx);
  stp^.st_flags:=stp^.st_flags or SELTD_PENDING;
  cv_broadcastpri(@stp^.st_wait, pri);
  mtx_unlock(stp^.st_mtx);
  //
  sfp:=sfn;
 end;
 mtx_unlock(sip^.si_mtx^);
end;

procedure seltdinit(td:p_kthread);
label
 _out;
var
 stp:p_seltd;
begin
 stp:=td^.td_sel;
 if (stp<>nil) then
  goto _out;
 stp:=AllocMem(sizeof(t_seltd));
 td^.td_sel:=stp;
 mtx_init(stp^.st_mtx, 'sellck');
 cv_init(@stp^.st_wait,'select');
_out:
 stp^.st_flags:=0;
 STAILQ_INIT(@stp^.st_selq);
end;

function seltdwait(td:p_kthread;timo:Int64):Integer;
var
 stp:p_seltd;
 error:Integer;
begin
 stp:=td^.td_sel;
 {
  * An event of interest may occur while we do not hold the seltd
  * locked so check the pending flag before we sleep.
  }
 mtx_lock(stp^.st_mtx);
 {
  * Any further calls to selrecord will be a rescan.
  }
 stp^.st_flags:=stp^.st_flags or SELTD_RESCAN;
 if ((stp^.st_flags and SELTD_PENDING)<>0) then
 begin
  mtx_unlock(stp^.st_mtx);
  Exit(0);
 end;
 if (timo > 0) then
  error:=_cv_timedwait_sig(@stp^.st_wait, @stp^.st_mtx, timo)
 else
  error:=_cv_wait_sig(@stp^.st_wait, @stp^.st_mtx);
 mtx_unlock(stp^.st_mtx);

 Exit(error);
end;

procedure seltdfini(td:p_kthread);
var
 stp:p_seltd;
begin
 stp:=td^.td_sel;
 if (stp=nil) then
  Exit;
 if (stp^.st_free1<>nil) then
  FreeMem(stp^.st_free1);
 if (stp^.st_free2<>nil) then
  FreeMem(stp^.st_free2);
 td^.td_sel:=nil;
 FreeMem(stp);
end;

{
 * Remove the references to the thread from all of the objects we were
 * polling.
 }
procedure seltdclear(td:p_kthread);
var
 stp:p_seltd;
 sfp:p_selfd;
 sfn:p_selfd;
begin
 stp:=td^.td_sel;
 sfp:=STAILQ_FIRST(@stp^.st_selq);
 while (sfp<>nil) do
 begin
  sfn:=STAILQ_NEXT(sfp,@sfp^.sf_link);
  //
  selfdfree(stp, sfp);
  //
  sfp:=sfn;
 end;
 stp^.st_flags:=0;
end;

procedure selectinit();
begin
 mtxpool_select:=mtx_pool_create('select mtxpool', 128);
end;

initialization
 selectinit();

end.

