unit sys_crt;

{$mode ObjFPC}{$H+}

interface

uses
 sys_tty;

Procedure sys_tty_init;

implementation

uses
 vuio,
 md_tty,
 kern_thread;

Procedure CrtOutWrite(var t:TextRec);
var
 tp:p_tty;
 aiov:iovec;
 auio:t_uio;
Begin
 if (t.BufPos=0) then Exit;

 tp:=PPointer(@t.UserData)^;
 if (tp=nil) then Exit;

 aiov.iov_base  :=t.Bufptr;
 aiov.iov_len   :=t.BufPos;
 auio.uio_iov   :=@aiov;
 auio.uio_iovcnt:=1;
 auio.uio_offset:=0;
 auio.uio_resid :=t.BufPos;
 auio.uio_segflg:=UIO_SYSSPACE;
 auio.uio_rw    :=UIO_WRITE;

 md_tty_write(tp,tp^.priv,@auio,0);

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

