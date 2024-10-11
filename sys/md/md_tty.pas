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

implementation

uses
 kern_thr;

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

type
 TWRITE_BUF=object
  MAX,LEN:QWORD;
  PTR:Pointer;
  BUF:array[0..1023] of AnsiChar;
  Procedure INIT ();
  function  WRITE(N:Pointer;L:QWORD):QWORD;
  function  UP   (L:QWORD):QWORD;
 end;

Procedure TWRITE_BUF.INIT();
begin
 PTR:=@BUF[0];
 MAX:=Length(BUF);
 LEN:=0;
end;

function TWRITE_BUF.WRITE(N:Pointer;L:QWORD):QWORD;
begin
 if (L>MAX) then L:=MAX;
 Move(N^,PTR^,L);
 Inc(PTR,L);
 Dec(MAX,L);
 Inc(LEN,L);
 Result:=L;
end;

function TWRITE_BUF.UP(L:QWORD):QWORD;
begin
 if (L>MAX) then L:=MAX;
 Inc(PTR,L);
 Dec(MAX,L);
 Inc(LEN,L);
 Result:=L;
end;

function ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
var
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
 BUF:TWRITE_BUF;
 i:QWORD;
 td:p_kthread;
begin
 Result:=0;
 //init
 BLK:=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
 //
 BUF.INIT();
 //tty name
 if ((tp^.t_flags and TF_NOWRITEPREFIX)=0) then
 begin
  BUF.WRITE(tp^.t_name,tp^.t_nlen);
 end;
 //thread
 td:=curkthread;
 if (td<>nil) then
 begin
  BUF.WRITE(pchar('('),1);
  BUF.WRITE(@td^.td_name,strlen(@td^.td_name));
  BUF.WRITE(pchar('):'),2);
 end;
 //text
 while (uio^.uio_resid<>0) do
 begin
  i:=uio^.uio_resid;
  Result:=uiomove(BUF.PTR, BUF.MAX, uio);
  if (Result<>0) then Break;
  i:=i-uio^.uio_resid;
  BUF.UP(i);
  //
  NtWriteFile(tp^.t_wr_handle,0,nil,nil,@BLK,@BUF.BUF,BUF.LEN,@OFFSET,nil);
  //
  BUF.INIT();
 end;
 //
 if (tp^.t_update<>nil) then
 begin
  tp^.t_update();
 end;
end;

function ttycrt_write(tp:p_tty;iov_base:Pointer;iov_len:qword):Integer;
var
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
 BUF:TWRITE_BUF;
 i:QWORD;
 td:p_kthread;
 R:DWORD;
begin
 Result:=0;
 //init
 BLK:=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
 //
 BUF.INIT();
 //tty name
 if ((tp^.t_flags and TF_NOWRITEPREFIX)=0) then
 begin
  BUF.WRITE(tp^.t_name,tp^.t_nlen);
 end;
 //thread
 td:=curkthread;
 if (td<>nil) then
 begin
  BUF.WRITE(pchar('('),1);
  BUF.WRITE(@td^.td_name,strlen(@td^.td_name));
  BUF.WRITE(pchar('):'),2);
 end;
 //text
 while (iov_len<>0) do
 begin
  i:=BUF.WRITE(iov_base,iov_len);
  Inc(iov_base,i);
  Dec(iov_len ,i);
  //
  R:=NtWriteFile(tp^.t_wr_handle,0,nil,nil,@BLK,@BUF.BUF,BUF.LEN,@OFFSET,nil);
  //
  if (R=STATUS_PENDING) then
  begin
   R:=NtWaitForSingleObject(tp^.t_wr_handle,False,nil);
   if (R=0) then
   begin
    R:=BLK.Status;
   end;
  end;
  //
  BUF.INIT();
 end;
 //
 if (tp^.t_update<>nil) then
 begin
  tp^.t_update();
 end;
end;

procedure md_init_tty; register;
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

 std_tty [2].t_wr_handle:=GetStdHandle(STD_ERROR_HANDLE);
 deci_tty[2].t_wr_handle:=GetStdHandle(STD_ERROR_HANDLE);

 debug_tty.t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
 debug_tty.t_wr_handle:=GetStdHandle(STD_OUTPUT_HANDLE);
end;

initialization
 init_tty:=@md_init_tty;

end.



