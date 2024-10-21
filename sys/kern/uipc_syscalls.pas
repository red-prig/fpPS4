unit uipc_syscalls;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_socket(domain,stype,protocol:Integer):Integer;
function sys_socketex(name:pchar;domain,stype,protocol:Integer):Integer;
function sys_socketclose(fd:Integer):Integer;
function sys_bind(s:Integer;name:Pointer;namelen:Integer):Integer;
function sys_listen(s,backlog:Integer):Integer;
function sys_accept(s:Integer;aname,anamelen:Pointer):Integer;
function sys_connect(fd:Integer;name:Pointer;namelen:Integer):Integer;
function sys_setsockopt(s,level,name:Integer;val:Pointer;valsize:Integer):Integer;

implementation

uses
 errno,
 mqueue,
 systm,
 md_sleep,
 kern_thr,
 kern_descrip,
 sys_conf,
 vsocket,
 sockopt,
 vsocketvar,
 vuio,
 vfile,
 vfcntl,
 vcapability,
 subr_backtrace;

{
 Convert a user file descriptor to a kernel file entry and check that, if
 it is a capability, the right rights are present. A reference on the file
 entry is held upon returning.
}
function getsock_cap(fd    :Integer;
                     rights:cap_rights_t;
                     fpp   :pp_file;
                     fflagp:PDWORD):Integer;
var
 fp:p_file;
begin
 fp:=fget_unlocked(fd);

 if (fp=nil) then
 begin
  Exit(EBADF);
 end;

 if (fp^.f_type<>DTYPE_SOCKET) and
    (fp^.f_type<>DTYPE_IPCSOCKET) then
 begin
  fdrop(fp);
  Exit(ENOTSOCK);
 end;

 if (fflagp<>nil) then
 begin
  fflagp^:=fp^.f_flag;
 end;

 fpp^:=fp;
 Exit(0);
end;

function soo_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin
 Result:=0;

 Writeln('soo_ioctl(0x',HexStr(com,8),')');

 case com of
  $802450C9: //-bnet_is_system_process()
            begin
             PInteger(data)^:=0; //-is_system
            end;
  else
   begin
    print_error_td('soo_ioctl(0x'+HexStr(com,8)+')');
    Assert(False);
   end;
 end;

end;

const
 socketops:fileops=(
  fo_read    :fo_rdwr_t    (@_nullop  );
  fo_write   :fo_rdwr_t    (@_nullop  );
  fo_truncate:fo_truncate_t(@_nullop  );
  fo_ioctl   :fo_ioctl_t   (@soo_ioctl);
  fo_poll    :fo_poll_t    (@_nullop  );
  fo_kqfilter:fo_kqfilter_t(@_nullop  );
  fo_stat    :fo_stat_t    (@_nullop  );
  fo_close   :fo_close_t   (@_nullop  );
  fo_chmod   :fo_chmod_t   (@_einval  );
  fo_chown   :fo_chown_t   (@_einval  );
  fo_flags   :0;
 );

function sys_socket(domain,stype,protocol:Integer):Integer;
var
 td:p_kthread;
 so:Pointer;
 fp:p_file;
 fd:Integer;
 dtype:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 fd:=0;
 Result:=falloc(@fp,@fd,0);
 if (Result<>0) then Exit();

 so:=nil; /////socreate

 dtype:=DTYPE_SOCKET;
 if (domain=PF_UNIX) then
 begin
  dtype:=DTYPE_IPCSOCKET;
 end;

 finit(fp,FREAD or FWRITE, dtype, so, @socketops);

 fdrop(fp);

 td^.td_retval[0]:=fd;
end;

function sys_socketex(name:pchar;domain,stype,protocol:Integer):Integer;
var
 td:p_kthread;
 so:Pointer;
 fp:p_file;
 fd:Integer;
 dtype:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 fd:=0;
 Result:=falloc(@fp,@fd,0);
 if (Result<>0) then Exit();

 so:=nil; /////socreate

 dtype:=DTYPE_SOCKET;
 if (domain=PF_UNIX) then
 begin
  dtype:=DTYPE_IPCSOCKET;
 end;

 finit(fp,FREAD or FWRITE, dtype, so, @socketops);

 fdrop(fp);

 td^.td_retval[0]:=fd;
end;

function sys_socketclose(fd:Integer):Integer;
begin
 Result:=kern_close(fd);
end;

function getsockaddr(namp:pp_sockaddr;uaddr:Pointer;len:size_t):Integer;
var
 sa:p_sockaddr;
begin
 sa:=nil;

 if (len>SOCK_MAXADDRLEN) then
 begin
  Exit(ENAMETOOLONG);
 end;

 if (len<size_t(@p_sockaddr(nil)^.sa_data[0])) then
 begin
  Exit(EINVAL);
 end;

 sa:=AllocMem(len);

 Result:=copyin(uaddr, sa, len);

 if (Result<>0) then
 begin
  FreeMem(sa);
 end else
 begin
  sa^.sa_len:=len;
  namp^:=sa;
 end;
end;

function kern_bind(fd:Integer;sa:p_sockaddr):Integer;
var
 //so:p_socket;
 fp:p_file;
 error:Integer;
begin
 error:=getsock_cap(fd, CAP_BIND, @fp, nil);
 if (error<>0) then
 begin
  Exit(error);
 end;
 //so:=fp^.f_data;
 //error:=sobind(so, sa, td);
 fdrop(fp);
 Exit(error);
end;

function sys_bind(s:Integer;name:Pointer;namelen:Integer):Integer;
var
 sa:p_sockaddr;
 error:Integer;
begin
 error:=getsockaddr(@sa, name, namelen);

 if (error<>0) then
 begin
  Exit(error);
 end;

 error:=kern_bind(s, sa);

 FreeMem(sa);

 Exit(error);
end;

function kern_connect(fd:Integer;sa:p_sockaddr):Integer;
var
 fp:p_file;
begin
 fp:=nil;
 Result:=getsock_cap(fd, CAP_CONNECT, @fp, nil);
 if (Result<>0) then Exit;

 Result:=ECONNREFUSED;

 fdrop(fp);
end;

function sys_listen(s,backlog:Integer):Integer;
var
 //so:p_socket;
 fp:p_file;
 error:Integer;
begin
 error:=getsock_cap(s, CAP_LISTEN, @fp, nil);
 if (error=0) then
 begin
  //so:=fp^.f_data;
  //error:=solisten(so, uap^.backlog, td);
  fdrop(fp);
 end;
 Exit(error);
end;

function kern_accept(s:Integer;name:pp_sockaddr;
                     namelen:p_socklen_t;fp:pp_file):Integer;
label
 done,
 noconnection;
var
 td:p_kthread;
 headfp,nfp:p_file;
 sa:p_sockaddr;
 error:Integer;
 //so,head:p_socket;
 fd:Integer;
 fflag:DWORD;
 pgid:Integer; //pid_t
 tmp:Integer;
begin
 td:=curkthread;
 headfp:=nil;
 nfp   :=nil;
 sa    :=nil;

 if (name<>nil) then
 begin
  name^:=nil;
  if (namelen^ < 0) then
  begin
   Exit(EINVAL);
  end;
 end;

 error:=getsock_cap(s, CAP_ACCEPT, @headfp, @fflag);

 if (error<>0) then
 begin
  Exit(error);
 end;

 {
 head:=headfp^.f_data;
 if ((head^.so_options and SO_ACCEPTCONN)=0) then
 begin
  error:=EINVAL;
  goto done;
 end;
 }

 error:=falloc(@nfp, @fd, 0);

 if (error<>0) then
 begin
  goto done;
 end;

 //////////////////
 msleep_td(0);
 error:=EWOULDBLOCK;
 goto noconnection;
  //////////////////

 ACCEPT_LOCK();

 {
 if (((head^.so_state and SS_NBIO)<>0) and TAILQ_EMPTY(@head^.so_comp)) then
 begin
  ACCEPT_UNLOCK();
  error:=EWOULDBLOCK;
  goto noconnection;
 end;
 }

 {
 while ((TAILQ_EMPTY(@head^.so_comp)) and (head^.so_error=0)) do
 begin
  if (head^.so_rcv.sb_state & SBS_CANTRCVMORE) then
  begin
   head^.so_error:=ECONNABORTED;
   break;
  end;
  error:=msleep(@head^.so_timeo, @accept_mtx, PSOCK or PCATCH, 'accept', 0);
  if (error) then
  begin
   ACCEPT_UNLOCK();
   goto noconnection;
  end;
 end;
 }

 {
 if (head^.so_error<>0) then
 begin
  error:=head^.so_error;
  head^.so_error:=0;
  ACCEPT_UNLOCK();
  goto noconnection;
 end;
 }

 {
 so:=TAILQ_FIRST(@head^.so_comp);
 Assert(not (so^.so_qstate and SQ_INCOMP), 'accept1: so SQ_INCOMP');
 Assert(so^.so_qstate and SQ_COMP, 'accept1: so not SQ_COMP');
 }

 {
  * Before changing the flags on the socket, we have to bump the
  * reference count.  Otherwise, if the protocol calls sofree(),
  * the socket will be released due to a zero refcount.
  }

 {
 SOCK_LOCK(so);   { soref() and so_state update }
 soref(so);   { file descriptor reference }

 TAILQ_REMOVE(@head^.so_comp, so, so_list);
 Dec(head^.so_qlen);
 so^.so_state :=so^.so_state or (head^.so_state and SS_NBIO);
 so^.so_qstate:=so^.so_qstate and (not SQ_COMP);
 so^.so_head  :=nil;

 SOCK_UNLOCK(so);
 }

 ACCEPT_UNLOCK();


 { An extra reference on `nfp' has been held for us by falloc(). }
 td^.td_retval[0]:=fd;

 {
 { connection has been removed from the listen queue }
 KNOTE_UNLOCKED(@head^.so_rcv.sb_sel.si_note, 0);

 pgid:=fgetown(@head^.so_sigio);
 if (pgid<>0) then
 begin
  fsetown(pgid, @so^.so_sigio);
 end;

 finit(nfp, fflag, DTYPE_SOCKET, so, @socketops);

 { Sync socket nonblocking/async state with file flags }
 tmp:=fflag and FNONBLOCK;
 fo_ioctl(nfp, FIONBIO, @tmp, td^.td_ucred, td);

 tmp:=fflag and FASYNC;
 fo_ioctl(nfp, FIOASYNC, @tmp, td^.td_ucred, td);

 sa:=0;
 error:=soaccept(so, @sa);
 }

 if (error<>0) then
 begin
  {
   * Exit a namelen of zero for older code which might
   * ignore the Exit value from accept.
   }
  if (name<>nil) then
  begin
   namelen^:=0;
  end;
  goto noconnection;
 end;

 if (sa=nil) then
 begin
  if (name<>nil) then
  begin
   namelen^:=0;
  end;
  goto done;
 end;

 if (name<>nil) then
 begin
  { check sa_len before it is destroyed }
  if (namelen^ > sa^.sa_len) then
  begin
   namelen^:=sa^.sa_len;
  end;

  name^:=sa;
  sa:=nil;
 end;

noconnection:
 if (sa<>nil) then
 begin
  FreeMem(sa);
 end;

 {
  * close the new descriptor, assuming someone hasn't ripped it
  * out from under us.
  }
 if (error<>0) then
 begin
  fdclose(nfp, fd);
 end;

 {
  * Release explicitly held references before Exiting.  We Exit
  * a reference on nfp to the caller on success if they request it.
  }
done:
 if (fp<>nil) then
 begin
  if (error=0) then
  begin
   fp^:=nfp;
   nfp:=nil;
  end else
  begin
   fp^:=nil;
  end;
 end;
 //
 if (nfp<>nil) then
 begin
  fdrop(nfp);
 end;
 fdrop(headfp);
 //
 Exit(error);
end;

function accept1(s:Integer;aname:p_sockaddr;anamelen:p_socklen_t):Integer;
var
 td:p_kthread;
 name:p_sockaddr;
 namelen:socklen_t;
 fp:p_file;
 error:Integer;
begin
 td:=curkthread;

 if (aname=nil) then
 begin
  Exit(kern_accept(s, nil, nil, nil));
 end;

 error:=copyin(anamelen, @namelen, sizeof(namelen));

 if (error<>0) then
 begin
  Exit(error);
 end;

 error:=kern_accept(s, @name, @namelen, @fp);

 {
  * Exit a namelen of zero for older code which might
  * ignore the Exit value from accept.
  }
 if (error<>0) then
 begin
  copyout(@namelen, anamelen, sizeof(anamelen^));
  Exit(error);
 end;

 if (error=0) and (name<>nil) then
 begin
  error:=copyout(name, name, namelen);
 end;

 if (error=0) then
 begin
  error:=copyout(@namelen,anamelen,sizeof(namelen));
 end;

 if (error<>0) then
 begin
  fdclose(fp, td^.td_retval[0]);
 end;

 fdrop(fp);
 FreeMem(name);

 Exit(error);
end;

function sys_accept(s:Integer;aname,anamelen:Pointer):Integer;
begin
 Exit(accept1(s,aname,anamelen));
end;

function sys_connect(fd:Integer;name:Pointer;namelen:Integer):Integer;
var
 sa:p_sockaddr;
begin
 sa:=nil;
 Result:=getsockaddr(@sa, name, namelen);
 if (Result<>0) then Exit;

 Result:=kern_connect(fd, sa);

 FreeMem(sa);
end;

function kern_setsockopt(s,level,name:Integer;val:Pointer;valseg:uio_seg;valsize:Integer):Integer;
var
 error:Integer;
 //so:p_socket;
 fp:p_file;
 sopt:t_sockopt;
begin
 if (val=nil) and (valsize<>0) then
 begin
  Exit(EFAULT);
 end;

 if (valsize < 0) then
 begin
  Exit(EINVAL);
 end;

 sopt.sopt_dir    :=SOPT_SET;
 sopt.sopt_level  :=level;
 sopt.sopt_name   :=name;
 sopt.sopt_val    :=val;
 sopt.sopt_valsize:=valsize;

 case (valseg) of
  UIO_USERSPACE:sopt.sopt_td:=curkthread;
  UIO_SYSSPACE :sopt.sopt_td:=nil;
  else
   Assert(false,'kern_setsockopt called with bad valseg');
 end;

 error:=getsock_cap(s, CAP_SETSOCKOPT, @fp, nil);
 if (error=0) then
 begin
  //so:=fp->f_data;
  //error:=sosetopt(so, @sopt);
  fdrop(fp);
 end;

 Exit(error);
end;

function sys_setsockopt(s,level,name:Integer;val:Pointer;valsize:Integer):Integer;
begin
 Result:=kern_setsockopt(s,level,name,val,UIO_USERSPACE,valsize);
end;


end.

