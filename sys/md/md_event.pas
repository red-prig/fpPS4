unit md_event;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ntapi,
 windows;

type
 t_fast_event=packed record
  signals:DWORD;
  waiters:DWORD;
 end;

 p_event=^t_event;
 t_event=packed record
  ev_description:Pchar;
  ev_state      :t_fast_event;
 end;

procedure ev_init   (var ev:t_event;desc:Pchar); inline;
procedure ev_destroy(var ev:t_event); inline;
procedure ev_wait   (var ev:t_event);
procedure ev_signal (var ev:t_event);

implementation

uses
 kern_mtx;

function GetKey(var ev:t_event):Pointer; inline;
begin
 Result:=Pointer(PTRUINT(@ev) and (not PTRUINT(1)));
end;

procedure ev_init(var ev:t_event;desc:Pchar); inline;
begin
 ev.ev_description :=desc;
 QWORD(ev.ev_state):=0;
end;

procedure ev_destroy(var ev:t_event); inline;
begin
 //
end;

procedure ev_wait(var ev:t_event);
var
 old:t_fast_event;
 new:t_fast_event;
begin

 repeat
  QWORD(old):=System.InterlockedExchangeAdd64(QWORD(ev.ev_state),0);

  if (old.signals>0) then
  begin
   new:=old;
   new.signals:=new.signals-1;

   if System.InterlockedCompareExchange64(QWORD(ev.ev_state),QWORD(new),QWORD(old)) = QWORD(old) then
   begin
    Break;
   end;

  end else
  begin
   new:=old;
   new.waiters:=new.waiters+1;

   if System.InterlockedCompareExchange64(QWORD(ev.ev_state),QWORD(new),QWORD(old)) = QWORD(old) then
   begin
    NtWaitForKeyedEvent(KeyedEventHandle, GetKey(ev), False, nil);
   end;
  end;

 until false;

end;

procedure ev_signal(var ev:t_event);
var
 old:t_fast_event;
 new:t_fast_event;
begin

 repeat
  QWORD(old):=System.InterlockedExchangeAdd64(QWORD(ev.ev_state),0);

  new:=old;

  if (new.signals<=new.waiters) then
  begin
   new.signals:=new.signals+1;
  end;

  if (new.waiters<>0) then
  begin
   new.waiters:=new.waiters-1;
  end;

  if System.InterlockedCompareExchange64(QWORD(ev.ev_state),QWORD(new),QWORD(old)) = QWORD(old) then
  begin

   if (old.waiters<>0) then
   begin
    NtReleaseKeyedEvent(KeyedEventHandle, GetKey(ev), False, nil);
   end;

   Break;
  end;

 until false;
end;

end.

