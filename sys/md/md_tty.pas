unit md_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 ntapi,
 vuio,
 subr_uio,
 sys_tty;

function  ttydisc_read_poll (tp:p_tty):QWORD;
function  ttydisc_write_poll(tp:p_tty):QWORD;

function  ttydisc_read (tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
function  ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;

function  ttycrt_write(tp:p_tty;iov_base:Pointer;iov_len:qword):Integer;

procedure md_init_tty;

implementation

function ttydisc_read_poll(tp:p_tty):QWORD;
var
 N:DWORD;
begin
 N:=0;

 case GetFileType(tp^.t_rd_handle) of
  FILE_TYPE_DISK:
   begin
    N:=1;
   end;
  FILE_TYPE_CHAR:
   begin
    GetNumberOfConsoleInputEvents(tp^.t_rd_handle,@N);
   end;
  FILE_TYPE_PIPE:
   begin
    PeekNamedPipe(tp^.t_rd_handle,
                  nil,
                  0,
                  nil,
                  @N,
                  nil);
   end;
  else;
 end;

 Result:=N;
end;

function ttydisc_write_poll(tp:p_tty):QWORD;
begin
 Result:=1;
end;

function ttydisc_read(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
var
 LEN:QWORD;
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
 BUF:array[0..1023] of AnsiChar;
begin
 //init
 BLK:=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_USE_FILE_POINTER_POSITION_L);
 //
 LEN:=uio^.uio_resid;
 if (len>Length(BUF)) then len:=Length(BUF);
 //
 NtReadFile(tp^.t_rd_handle,0,nil,nil,@BLK,@BUF,LEN,@OFFSET,nil);
 //
 Result:=uiomove(@BUF, BLK.Information, uio);
end;

function ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
var
 MAX,LEN,OFS:QWORD;
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
 PTR:Pointer;
 BUF:array[0..1023] of AnsiChar;
begin
 //init
 BLK:=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
 //tty name
 if ((tp^.t_flags and TF_NOWRITEPREFIX)=0) then
 begin
  OFS:=tp^.t_nlen;
  Move(tp^.t_name^,BUF,OFS);
  PTR:=@BUF[OFS];
  MAX:=Length(BUF)-OFS;
  LEN:=uio^.uio_resid+OFS;
 end else
 begin
  PTR:=@BUF[0];
  MAX:=Length(BUF);
  LEN:=uio^.uio_resid;
  OFS:=0;
 end;
 //text
 while (LEN<>0) do
 begin
  if (LEN>MAX) then LEN:=MAX;
  //
  Result:=uiomove(PTR, LEN-OFS, uio);
  if (Result<>0) then Break;
  //
  NtWriteFile(tp^.t_wr_handle,0,nil,nil,@BLK,@BUF,LEN,@OFFSET,nil);
  //
  PTR:=@BUF[0];
  MAX:=Length(BUF);
  LEN:=uio^.uio_resid;
  OFS:=0;
 end;
end;

function ttycrt_write(tp:p_tty;iov_base:Pointer;iov_len:qword):Integer;
var
 MAX,LEN,OFS:QWORD;
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
 PTR:Pointer;
 BUF:array[0..1023] of AnsiChar;
begin
 Result:=0;
 //init
 BLK:=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
 //tty name
 if ((tp^.t_flags and TF_NOWRITEPREFIX)=0) then
 begin
  OFS:=tp^.t_nlen;
  Move(tp^.t_name^,BUF,OFS);
  PTR:=@BUF[OFS];
  MAX:=Length(BUF)-OFS;
  LEN:=iov_len+OFS;
 end else
 begin
  PTR:=@BUF[0];
  MAX:=Length(BUF);
  LEN:=iov_len;
  OFS:=0;
 end;
 //text
 while (LEN<>0) do
 begin
  if (LEN>MAX) then LEN:=MAX;
  //
  MAX:=LEN-OFS;
  Move(iov_base^,PTR^,MAX);
  Inc(iov_base,MAX);
  Dec(iov_len ,MAX);
  //
  NtWriteFile(tp^.t_wr_handle,0,nil,nil,@BLK,@BUF,LEN,@OFFSET,nil);
  //
  PTR:=@BUF[0];
  MAX:=Length(BUF);
  LEN:=iov_len;
  OFS:=0;
 end;
end;

procedure md_init_tty;
var
 i:Integer;
begin
 For i:=0 to High(std_tty) do
 begin
  std_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  std_tty[i].t_wr_handle:=GetStdHandle(STD_OUTPUT_HANDLE);
 end;

 For i:=0 to High(deci_tty) do
 begin
  deci_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  deci_tty[i].t_wr_handle:=GetStdHandle(STD_OUTPUT_HANDLE);
 end;
end;

end.



