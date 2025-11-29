unit sys_crt_gui;

{$mode ObjFPC}{$H+}

interface

Procedure sys_crt_init(Attach:Boolean=False);

implementation

uses
 sysutils,
 windows;

Procedure CrtOutWrite(var t:TextRec);
var
 i:Integer;
 h:THandle;
Begin
 if (t.BufPos=0) then Exit;

 i:=PDWORD(@t.UserData)^;
 h:=GetStdHandle(i);

 FileWrite(h,t.Bufptr^,t.BufPos);

 t.BufPos:=0;
end;

Procedure CrtClose(Var F:TextRec);
Begin
 F.Mode:=fmClosed;
end;

Procedure CrtOpenOut(Var F:TextRec);
Begin
 F.InOutFunc:=@CrtOutWrite;
 F.FlushFunc:=@CrtOutWrite;
 F.CloseFunc:=@CrtClose;
end;

procedure AssignTTY(var F:Text;i:DWORD);
begin
 Assign(F,'');
 //
 TextRec(F).OpenFunc :=@CrtOpenOut;
 //
 PDWORD(@TextRec(F).UserData)^:=i;
end;

Procedure sys_crt_init(Attach:Boolean=False);
begin
 if Attach then
 begin
  AttachConsole(ATTACH_PARENT_PROCESS);
 end;
 //
 AssignTTY(Output   ,STD_OUTPUT_HANDLE);
 AssignTTY(StdOut   ,STD_OUTPUT_HANDLE);
 AssignTTY(ErrOutput,STD_ERROR_HANDLE);
 AssignTTY(StdErr   ,STD_ERROR_HANDLE);
 //
 Rewrite(Output);
 Rewrite(StdOut);
 Rewrite(ErrOutput);
 Rewrite(StdErr);
end;

initialization
 sys_crt_init(True);

end.

