unit kern_blockpool;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_blockpool_open(flags:Integer):Integer;

implementation

uses
 errno,
 kern_thr,
 kern_descrip,
 vfile,
 vfcntl,
 kern_conf,
 vstat;

function blockpool_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin
 Assert(False);
 Result:=0;
end;

function blockpool_stat(fp:p_file;sb:p_stat):Integer;
begin
 Assert(False);
 Result:=0;
end;

function blockpool_close(fp:p_file):Integer;
begin
 Assert(False);
 Result:=0;
end;

const
 blockpool_ops:fileops=(
  fo_read    :fo_rdwr_t    (@_enxio);
  fo_write   :fo_rdwr_t    (@_enxio);
  fo_truncate:fo_truncate_t(@_enxio);
  fo_ioctl   :@blockpool_ioctl;
  fo_poll    :fo_poll_t    (@_eopnotsupp);
  fo_kqfilter:fo_kqfilter_t(@_eopnotsupp);
  fo_stat    :@blockpool_stat;
  fo_close   :@blockpool_close;
  fo_chmod   :fo_chmod_t   (@_einval);
  fo_chown   :fo_chown_t   (@_einval);
  fo_flags   :0;
 );

function sys_blockpool_open(flags:Integer):Integer;
var
 td:p_kthread;
 bp:Pointer;
 fp:p_file;
 fd:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);
 //0x100000(O_CLOEXEC) | 0x400000(ASLR_FD????)
 if ((flags and $ffafffff)<>0) then Exit(EINVAL);

 flags:=flags or FWRITE;

 fd:=0;
 Result:=falloc(@fp,@fd,flags);
 if (Result<>0) then Exit();

 bp:=nil; /////

 finit(fp, flags, DTYPE_BLOCKPOOL, bp, @blockpool_ops);

 fdrop(fp);

 td^.td_retval[0]:=fd;
end;

end.

