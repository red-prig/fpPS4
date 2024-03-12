unit uipc_syscalls;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_socket(domain,stype,protocol:Integer):Integer;
function sys_socketex(name:pchar;domain,stype,protocol:Integer):Integer;
function sys_socketclose(fd:Integer):Integer;
function sys_connect(fd:Integer;name:Pointer;namelen:Integer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_descrip,
 sys_conf,
 vsocket,
 vfile,
 vfcntl,
 vcapability,
 subr_backtrace;

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

end.

