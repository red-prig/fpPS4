unit sys_crt;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 sys_pthread;

var
 StdWrn:Text;

Procedure sys_crt_init;
Procedure CrtOutWriteDirect(T:PText;data:Pointer;len:SizeInt);

implementation

uses
 sys_kernel,
 sys_signal;

var
 StdOutLock:TRTLCriticalSection;
 StdOutColor:Word;

const
 StdErrColor=FOREGROUND_RED;
 StdWrnColor=14;

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

Procedure _CrtOutWrite(var t:TextRec;data:Pointer;len:SizeInt);
var
 n:DWORD;
Begin
 if (data=nil) or (len=0) then exit;
 n:=0;

 _sig_lock(SL_NOINTRRUP);
 EnterCriticalSection(StdOutLock);

 if Boolean(t.UserData[2]) then //IsChar
 begin
  SetConsoleTextAttribute(t.Handle,t.UserData[1]);
  WriteConsole(t.Handle,
               data,
               len,
               @n,
               nil);
 end else
 begin
  WriteFile(t.Handle,
            data^,
            len,
            n,
            nil);
 end;

 LeaveCriticalSection(StdOutLock);
 _sig_unlock(SL_NOINTRRUP);
end;

Procedure CrtOutWrite(var t:TextRec);
Begin
 _CrtOutWrite(t,t.Bufptr,t.BufPos);
 t.BufPos:=0;
end;

Procedure CrtOutWriteDirect(T:PText;data:Pointer;len:SizeInt);
begin
 if (T=nil) then Exit;
 _CrtOutWrite(TextRec(T^),data,len);
end;

Procedure CrtClose(Var F:TextRec);
Begin
 F.Mode:=fmClosed;
end;

Procedure CrtOpenOut(Var F:TextRec);
var
 _type:Shortint;
 IsChar:Boolean;
Begin
 _type:=Shortint(TextRec(F).UserData[2]);

 TextRec(F).Handle:=GetStdHandle(_type);

 IsChar:=SwGetFileType(TextRec(F).Handle)=FILE_TYPE_CHAR;
 TextRec(F).UserData[2]:=ord(IsChar);

 TextRec(F).InOutFunc:=@CrtOutWrite;
 TextRec(F).FlushFunc:=@CrtOutWrite;
 TextRec(F).CloseFunc:=@CrtClose;
end;

procedure AssignCrt(var F:Text;_type:DWORD;clr:Byte);
begin
 Assign(F,'');
 TextRec(F).OpenFunc:=@CrtOpenOut;
 TextRec(F).UserData[1]:=clr;
 TextRec(F).UserData[2]:=Shortint(Integer(_type));
end;

Procedure sys_crt_init;
begin
 tcb_thread:=nil; //need zero tcb

 AssignCrt(Output   ,STD_OUTPUT_HANDLE,StdOutColor);
 AssignCrt(StdOut   ,STD_OUTPUT_HANDLE,StdOutColor);
 AssignCrt(ErrOutput,STD_ERROR_HANDLE ,StdErrColor);
 AssignCrt(StdErr   ,STD_ERROR_HANDLE ,StdErrColor);

 Rewrite(Output);
 Rewrite(StdOut);
 Rewrite(ErrOutput);
 Rewrite(StdErr);
end;

Procedure _sys_crt_init;
var
 F:Thandle;
begin
 StdOutColor:=7;
 F:=GetStdHandle(STD_OUTPUT_HANDLE);
 if (SwGetFileType(F)=FILE_TYPE_CHAR) then
 begin
  GetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),StdOutColor);
 end;
 //
 AssignCrt(StdWrn,STD_OUTPUT_HANDLE,StdWrnColor);
 Rewrite(StdWrn);
end;

procedure _sys_crt_fini;
var
 F:Thandle;
begin
 F:=GetStdHandle(STD_OUTPUT_HANDLE);
 if (SwGetFileType(F)=FILE_TYPE_CHAR) then
 begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),StdOutColor);
 end;
end;

initialization
 AddExitProc(@_sys_crt_fini);
 InitCriticalSection(StdOutLock);
 _sys_crt_init;

end.

