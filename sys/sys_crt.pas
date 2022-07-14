unit sys_crt;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 spinlock;

Procedure sys_crt_init;

implementation

uses
 sys_signal;

var
 StdOutLock:Pointer=nil;

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

Procedure CrtOutWrite(var t:TextRec);
var
 n:DWORD;
Begin
 if (t.BufPos=0) then exit;
 n:=0;

 _sig_lock(SL_NOINTRRUP);
 spin_lock(StdOutLock);

 WriteConsole(t.Handle,t.Bufptr,t.BufPos,@n,nil);

 if (n<>t.BufPos) then InOutRes:=101;
 t.BufPos:=0;

 spin_unlock(StdOutLock);
 _sig_unlock(SL_NOINTRRUP);
end;

Procedure CrtErrWrite(var t:TextRec);
const
 new=FOREGROUND_RED;
var
 n:DWORD;
 old:WORD;
Begin
 if (t.BufPos=0) then exit;
 n:=0;

 _sig_lock(SL_NOINTRRUP);
 spin_lock(StdOutLock);

 old:=7;
 GetConsoleTextAttribute(t.Handle,old);
 SetConsoleTextAttribute(t.Handle,new);

 WriteConsole(t.Handle,t.Bufptr,t.BufPos,@n,nil);

 SetConsoleTextAttribute(t.Handle,old);

 if (n<>t.BufPos) then InOutRes:=101;
 t.BufPos:=0;

 spin_unlock(StdOutLock);
 _sig_unlock(SL_NOINTRRUP);
end;

Procedure CrtClose(Var F:TextRec);
Begin
 F.Mode:=fmClosed;
end;

Procedure CrtOpenOut(Var F:TextRec);
Begin
 TextRec(F).Handle:=GetStdHandle(STD_OUTPUT_HANDLE);
 TextRec(F).InOutFunc:=@CrtOutWrite;
 TextRec(F).FlushFunc:=@CrtOutWrite;
 TextRec(F).CloseFunc:=@CrtClose;
end;

Procedure CrtOpenErr(Var F:TextRec);
Begin
 TextRec(F).Handle:=GetStdHandle(STD_ERROR_HANDLE);
 TextRec(F).InOutFunc:=@CrtErrWrite;
 TextRec(F).FlushFunc:=@CrtErrWrite;
 TextRec(F).CloseFunc:=@CrtClose;
end;

procedure AssignCrt(var F:Text;cb:codepointer);
begin
 Assign(F,'');
 TextRec(F).OpenFunc:=cb;
end;

Procedure sys_crt_init;
begin
 AssignCrt(Output,@CrtOpenOut);
 Rewrite(Output);

 AssignCrt(StdOut,@CrtOpenOut);
 Rewrite(StdOut);

 AssignCrt(ErrOutput,@CrtOpenErr);
 Rewrite(ErrOutput);

 AssignCrt(StdErr,@CrtOpenErr);
 Rewrite(StdErr);
end;

end.

