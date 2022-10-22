unit sys_crt;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 sys_pthread,
 spinlock;

Procedure sys_crt_init;

implementation

uses
 sys_kernel,
 sys_signal;

var
 StdOutLock:Pointer=nil;
 StdOutColor:Word;

const
 StdErrColor=FOREGROUND_RED;

function GetConsoleTextAttribute(hConsoleOutput:HANDLE;var wAttributes:WORD):WINBOOL;
var
 info:CONSOLE_SCREEN_BUFFER_INFO;
begin
 Result:=GetConsoleScreenBufferInfo(hConsoleOutput,@info);
 if Result then
 begin
  wAttributes:=info.wAttributes
 end;
end;

function GetConsoleCursorPosition(hConsoleOutput:HANDLE;var dwCursorPosition:COORD):WINBOOL;
var
 info:CONSOLE_SCREEN_BUFFER_INFO;
begin
 Result:=GetConsoleScreenBufferInfo(hConsoleOutput,@info);
 if Result then
 begin
  dwCursorPosition:=info.dwCursorPosition;
 end;
end;

Procedure CrtOutWrite(var t:TextRec);
var
 n:DWORD;
Begin
 if (t.BufPos=0) then exit;
 n:=0;

 _sig_lock(SL_NOINTRRUP);
 spin_lock(StdOutLock);

 WriteConsole(t.Handle,
              t.Bufptr,
              t.BufPos,
              @n,
              nil);

 spin_unlock(StdOutLock);
 _sig_unlock(SL_NOINTRRUP);

 if (n<>t.BufPos) then InOutRes:=101;
 t.BufPos:=0;
end;

Procedure CrtErrWrite(var t:TextRec);
var
 n:DWORD;
 dwCursorPosition:COORD;
Begin
 if (t.BufPos=0) then exit;
 n:=0;

 _sig_lock(SL_NOINTRRUP);
 spin_lock(StdOutLock);

 dwCursorPosition:=Default(COORD);

 GetConsoleCursorPosition(t.Handle,
                          dwCursorPosition);

 WriteConsole(t.Handle,
              t.Bufptr,
              t.BufPos,
              @n,
              nil);

 FillConsoleOutputAttribute(t.Handle,
                            StdErrColor,
                            t.BufPos,
                            dwCursorPosition,
                            n);

 spin_unlock(StdOutLock);
 _sig_unlock(SL_NOINTRRUP);

 if (n<>t.BufPos) then InOutRes:=101;
 t.BufPos:=0;
end;

Procedure CrtFileWrite(var t:TextRec);
var
 n:DWORD;
Begin
 if (t.BufPos=0) then exit;
 n:=0;

 _sig_lock(SL_NOINTRRUP);
 spin_lock(StdOutLock);

 WriteFile(t.Handle,
           t.Bufptr^,
           t.BufPos,
           n,
           nil);

 spin_unlock(StdOutLock);
 _sig_unlock(SL_NOINTRRUP);

 if (n<>t.BufPos) then InOutRes:=101;
 t.BufPos:=0;
end;

Procedure CrtClose(Var F:TextRec);
Begin
 F.Mode:=fmClosed;
end;

Procedure CrtOpenOut(Var F:TextRec);
Begin
 TextRec(F).Handle:=GetStdHandle(STD_OUTPUT_HANDLE);

 if (SwGetFileType(TextRec(F).Handle)=FILE_TYPE_CHAR) then
 begin
  TextRec(F).InOutFunc:=@CrtOutWrite;
  TextRec(F).FlushFunc:=@CrtOutWrite;
  TextRec(F).CloseFunc:=@CrtClose;
 end else
 begin
  TextRec(F).InOutFunc:=@CrtFileWrite;
  TextRec(F).FlushFunc:=@CrtFileWrite;
  TextRec(F).CloseFunc:=@CrtClose;
 end;
end;

Procedure CrtOpenErr(Var F:TextRec);
Begin
 TextRec(F).Handle:=GetStdHandle(STD_ERROR_HANDLE);

 if (SwGetFileType(TextRec(F).Handle)=FILE_TYPE_CHAR) then
 begin
  TextRec(F).InOutFunc:=@CrtErrWrite;
  TextRec(F).FlushFunc:=@CrtErrWrite;
  TextRec(F).CloseFunc:=@CrtClose;
 end else
 begin
  TextRec(F).InOutFunc:=@CrtFileWrite;
  TextRec(F).FlushFunc:=@CrtFileWrite;
  TextRec(F).CloseFunc:=@CrtClose;
 end;
end;

procedure AssignCrt(var F:Text;cb:codepointer);
begin
 Assign(F,'');
 TextRec(F).OpenFunc:=cb;
end;

Procedure sys_crt_init;
begin
 tcb_thread:=nil; //need zero tcb

 AssignCrt(Output,@CrtOpenOut);
 Rewrite(Output);

 AssignCrt(StdOut,@CrtOpenOut);
 Rewrite(StdOut);

 AssignCrt(ErrOutput,@CrtOpenErr);
 Rewrite(ErrOutput);

 AssignCrt(StdErr,@CrtOpenErr);
 Rewrite(StdErr);
end;

initialization
 StdOutColor:=7;
 GetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),StdOutColor);

end.

