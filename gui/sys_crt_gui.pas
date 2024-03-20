unit sys_crt_gui;

{$mode ObjFPC}{$H+}

interface

Procedure sys_crt_init;

implementation

uses
 windows,
 ntapi;

Procedure CrtOutWrite(var t:TextRec);
var
 i:DWORD;
 h:THandle;
 BLK:IO_STATUS_BLOCK;
 OFFSET:Int64;
Begin
 if (t.BufPos=0) then Exit;

 i:=PDWORD(@t.UserData)^;
 h:=GetStdHandle(i);

 OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
 NtWriteFile(h,0,nil,nil,@BLK,t.Bufptr,t.BufPos,@OFFSET,nil);

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

Procedure sys_crt_init;
begin
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
 sys_crt_init;

end.

