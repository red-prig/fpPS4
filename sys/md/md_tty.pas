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

function ttydisc_read_poll (tp:p_tty):QWORD;
function ttydisc_write_poll(tp:p_tty):QWORD;

function ttydisc_read (tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
function ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;

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
 len:QWORD;
 S:RawByteString;
begin
 Readln(S);

 len:=Length(S);
 if (len > uio^.uio_resid) then
 begin
  len:=uio^.uio_resid;
 end;

 Result:=uiomove(PChar(S), len, uio);
end;

function ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
var
 LEN:QWORD;
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
 PTR:Pointer;
 BUF:array[0..1023] of AnsiChar;
begin
 //init
 BLK:=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
 //tty name
 LEN:=tp^.t_nlen;
 Move(tp^.t_name^,BUF,LEN);
 PTR:=@BUF[LEN];
 LEN:=uio^.uio_resid+LEN;
 //text
 while (LEN<>0) do
 begin
  if (len>Length(BUF)) then len:=Length(BUF);
  //
  Result:=uiomove(PTR, len, uio);
  if (Result<>0) then Break;
  //
  NtWriteFile(tp^.t_wr_handle,0,nil,nil,@BLK,@BUF,LEN,@OFFSET,nil);
  //
  PTR:=@BUF[0];
  LEN:=uio^.uio_resid;
 end;
end;



end.



