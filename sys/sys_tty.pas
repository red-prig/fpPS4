unit sys_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface


uses
 sysutils,
 vselinfo,
 kern_mtx;

type
 p_tty=^t_tty;
 t_tty=record
  t_name   :PChar;
  t_nlen   :QWORD;

  t_mtx    :p_mtx;      // TTY lock.
  t_mtxobj :mtx;        // Per-TTY lock (when not borrowing).

  // Polling mechanisms.
  t_inpoll :t_selinfo;  // (t) Input  poll queue.
  t_outpoll:t_selinfo;  // (t) Output poll queue.

  t_rd_handle :THandle;
  t_wr_handle :THandle;
 end;

procedure tty_lock  (tp:p_tty);
procedure tty_unlock(tp:p_tty);

procedure tty_init(tp:p_tty;name:PChar;mutex:p_mtx);
procedure tty_fini(tp:p_tty);

implementation

uses
 vsys_generic,
 sys_event;

procedure tty_lock(tp:p_tty);
begin
 mtx_lock(tp^.t_mtx^)
end;

procedure tty_unlock(tp:p_tty);
begin
 mtx_unlock(tp^.t_mtx^)
end;

procedure tty_init(tp:p_tty;name:PChar;mutex:p_mtx);
begin
 if (tp=nil) then Exit;

 tp^.t_name:=name;

 if (name<>nil) then
 begin
  tp^.t_nlen:=strlen(name);
 end;

 if (mutex<>nil) then
 begin
  tp^.t_mtx:=mutex;
 end else
 begin
  tp^.t_mtx:=@tp^.t_mtxobj;
  mtx_init(tp^.t_mtxobj, 'ttymtx');
 end;

 knlist_init_mtx(@tp^.t_inpoll .si_note, tp^.t_mtx);
 knlist_init_mtx(@tp^.t_outpoll.si_note, tp^.t_mtx);

end;

procedure tty_fini(tp:p_tty);
begin

 seldrain(@tp^.t_inpoll);
 seldrain(@tp^.t_outpoll);

 knlist_destroy(@tp^.t_inpoll .si_note);
 knlist_destroy(@tp^.t_outpoll.si_note);

 if (tp^.t_mtx=@tp^.t_mtxobj) then
 begin
  mtx_destroy(tp^.t_mtxobj);
 end;

end;


end.



