unit sys_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface


uses
 sysutils,
 vselinfo,
 kern_mtx,
 subr_msgbuf;

type
 p_tty=^t_tty;
 t_tty=record
  t_name   :PChar;
  t_nlen   :DWORD;

  //t_flags  :WORD;

  t_mtx    :p_mtx;      // TTY lock.
  t_mtxobj :mtx;        // Per-TTY lock (when not borrowing).

  // Polling mechanisms.
  t_inpoll :t_selinfo;  // (t) Input  poll queue.
  t_outpoll:t_selinfo;  // (t) Output poll queue.

  priv:Pointer;

  t_update    :TProcedure;
 end;

procedure tty_lock  (tp:p_tty);
procedure tty_unlock(tp:p_tty);

procedure tty_init(tp:p_tty;name:PChar;mutex:p_mtx);
procedure tty_fini(tp:p_tty);

var
 std_tty  :array[0..2 ] of t_tty;
 deci_tty :array[0..11] of t_tty;
 debug_tty:t_tty;

procedure sys_tty_init;

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

 //tp^.t_flags:=flags;
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

procedure sys_tty_init;
begin
 tty_init( @std_tty[ 0],'Input' ,nil);
 tty_init( @std_tty[ 1],'Output',nil);
 tty_init( @std_tty[ 2],'Error' ,nil);
 //
 tty_init(@deci_tty[ 0],'stdin' ,nil);
 tty_init(@deci_tty[ 1],'stdout',nil);
 tty_init(@deci_tty[ 2],'stderr',nil);
 tty_init(@deci_tty[ 3],'tty2'  ,nil);
 tty_init(@deci_tty[ 4],'tty3'  ,nil);
 tty_init(@deci_tty[ 5],'tty4'  ,nil);
 tty_init(@deci_tty[ 6],'tty5'  ,nil);
 tty_init(@deci_tty[ 7],'tty6'  ,nil);
 tty_init(@deci_tty[ 8],'tty7'  ,nil);
 tty_init(@deci_tty[ 9],'ttya0' ,nil);
 tty_init(@deci_tty[10],'ttyb0' ,nil);
 tty_init(@deci_tty[11],'ttyc0' ,nil);
 //
 tty_init(@debug_tty   ,'Debug' ,nil);
end;


end.



