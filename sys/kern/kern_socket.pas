unit kern_socket;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_event,
 kern_descrip,
 vstat,
 vuio,
 vfile;

function soo_read    (fp:p_file;uio:p_uio;flags:Integer):Integer;
function soo_write   (fp:p_file;uio:p_uio;flags:Integer):Integer;
function soo_truncate(fp:p_file;length:Int64):Integer;
function soo_ioctl   (fp:p_file;com:QWORD;data:Pointer):Integer;
function soo_poll    (fp:p_file;events:Integer):Integer;
function soo_kqfilter(fp:p_file;kn:p_knote):Integer;
function soo_stat    (fp:p_file;sb:p_stat):Integer;
function soo_close   (fp:p_file):Integer;

const
 socketops:fileops=(
  fo_read    :fo_rdwr_t    (@soo_read);
  fo_write   :fo_rdwr_t    (@soo_write);
  fo_truncate:fo_truncate_t(@soo_truncate);
  fo_ioctl   :fo_ioctl_t   (@soo_ioctl);
  fo_poll    :fo_poll_t    (@soo_poll);
  fo_kqfilter:fo_kqfilter_t(@soo_kqfilter);
  fo_stat    :fo_stat_t    (@soo_stat);
  fo_close   :fo_close_t   (@soo_close);
  fo_chmod   :fo_chmod_t   (@invfo_chmod);
  fo_chown   :fo_chown_t   (@invfo_chown);
  fo_flags   :DFLAG_PASSABLE;
 );

implementation

uses
 errno,
 md_sleep,
 subr_backtrace;

function soo_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Writeln('TODO:soo_read');
 //msleep_td(0);
 Result:=EWOULDBLOCK;
end;

function soo_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Writeln('TODO:soo_write');
 //msleep_td(0);
 Result:=EWOULDBLOCK;
end;

function soo_truncate(fp:p_file;length:Int64):Integer;
begin
 Result:=EINVAL;
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

function soo_poll(fp:p_file;events:Integer):Integer;
begin
 Writeln('TODO:soo_poll');
 Assert(false);
 Result:=0;
end;

function soo_kqfilter(fp:p_file;kn:p_knote):Integer;
begin
 Writeln('TODO:soo_kqfilter');
 Assert(false);
 Result:=0;
end;

function soo_stat(fp:p_file;sb:p_stat):Integer;
begin
 Writeln('TODO:soo_stat');
 Assert(false);
 Result:=0;
end;

function soo_close(fp:p_file):Integer;
begin
 Result:=0;
 //so = fp->f_data;
 fp^.f_ops :=@badfileops;
 fp^.f_data:=nil;
 //if (so<>nil) then Result = soclose(so);
end;


end.

