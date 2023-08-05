unit uipc_syscalls;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_socket(domain,stype,protocol:Integer):Integer;
function sys_socketex(name:pchar;domain,stype,protocol:Integer):Integer;
function sys_socketclose(fd:Integer):Integer;

implementation

uses
 errno,
 kern_thr,
 kern_descrip,
 kern_conf,
 vsocket,
 vfile,
 vfcntl,
 trap;

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
    print_backtrace(stderr,Pointer(curkthread^.td_frame.tf_rip),Pointer(curkthread^.td_frame.tf_rbp),0);
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



end.

