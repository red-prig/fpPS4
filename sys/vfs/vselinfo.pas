unit vselinfo;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 sys_event,
 kern_mtx;

type
 {
  * Used to maintain information about processes that wish to be
  * notified when I/O becomes possible.
  }
 p_selinfo=^t_selinfo;
 t_selinfo=packed record
  si_tdlist:TAILQ_HEAD; { List of sleeping threads. }
  si_note  :t_knlist;   { kernel note list }
  si_mtx   :p_mtx;      { Lock for tdlist. }
 end;

function SEL_WAITING(si:p_selinfo):Boolean; inline;

implementation

function SEL_WAITING(si:p_selinfo):Boolean; inline;
begin
 Result:=(not TAILQ_EMPTY(@si^.si_tdlist));
end;

end.

