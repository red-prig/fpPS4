unit sys_crt;

{$mode ObjFPC}{$H+}

interface

uses
 vuio,
 sys_tty;

Procedure sys_tty_init;

implementation

uses
 md_tty,
 kern_thread;

Procedure CrtOutWrite(var t:TextRec);
var
 tp:p_tty;
Begin
 if (t.BufPos=0) then Exit;

 tp:=PPointer(@t.UserData)^;
 if (tp=nil) then Exit;

 ttycrt_write(tp,t.Bufptr,t.BufPos);

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

procedure AssignTTY(var F:Text;tp:p_tty);
begin
 Assign(F,'');
 //
 TextRec(F).OpenFunc :=@CrtOpenOut;
 //
 PPointer(@TextRec(F).UserData)^:=tp;
end;

Procedure sys_tty_init;
begin
 AssignTTY(Output   ,@std_tty[ 1]);
 AssignTTY(StdOut   ,@std_tty[ 1]);
 AssignTTY(ErrOutput,@std_tty[ 2]);
 AssignTTY(StdErr   ,@std_tty[ 2]);
 //
 Rewrite(Output);
 Rewrite(StdOut);
 Rewrite(ErrOutput);
 Rewrite(StdErr);
end;

initialization
 init_tty_cb:=@sys_tty_init;

end.

