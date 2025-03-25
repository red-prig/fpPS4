unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 ntapi;

type
 t_fast_mutex=bitpacked record
  recursion:WORD;
  waiters  :WORD;
  owned    :DWORD;
 end;

 p_mtx=^mtx;
 mtx=packed record
  n:PChar;
  fast_mutex:t_fast_mutex;
  {$IFDEF DEBUG_MTX}
  debug_own:array[0..2] of Pointer;
  {$ENDIF}
 end;

const
 //Flags for lockinit().
 LK_INIT_MASK =$0000FF;
 LK_CANRECURSE=$000001;
 LK_NODUP     =$000002;
 LK_NOPROFILE =$000004;
 LK_NOSHARE   =$000008;
 LK_NOWITNESS =$000010;
 LK_QUIET     =$000020;
 LK_ADAPTIVE  =$000040;

 //Additional attributes to be used in lockmgr().
 LK_EATTR_MASK=$00FF00;
 LK_INTERLOCK =$000100;
 LK_NOWAIT    =$000200;
 LK_RETRY     =$000400;
 LK_SLEEPFAIL =$000800;
 LK_TIMELOCK  =$001000;

 //Operations for lockmgr().
 LK_TYPE_MASK =$FF0000;
 LK_DOWNGRADE =$010000;
 LK_DRAIN     =$020000;
 LK_EXCLOTHER =$040000;
 LK_EXCLUSIVE =$080000;
 LK_RELEASE   =$100000;
 LK_SHARED    =$200000;
 LK_UPGRADE   =$400000;
 LK_TRYUPGRADE=$800000;

 LK_TOTAL_MASK=(LK_INIT_MASK or LK_EATTR_MASK or LK_TYPE_MASK);

procedure mtx_init   (var m:mtx;name:PChar);
procedure mtx_destroy(var m:mtx);
procedure mtx_lock   (var m:mtx);
function  mtx_trylock(var m:mtx):Boolean;
procedure mtx_unlock (var m:mtx);
function  mtx_owned  (var m:mtx):Boolean;
procedure mtx_assert (var m:mtx);

implementation

{$IFDEF DEBUG_MTX}
uses
 md_systm,
 kern_thr;
{$ENDIF}

var
 qhandle:THandle=0;

procedure mtx_init(var m:mtx;name:PChar); inline;
begin
 m.n:=name;
 QWORD(m.fast_mutex):=0;
end;

procedure mtx_destroy(var m:mtx); inline;
begin
 //
end;

function GetKey(var m:mtx):Pointer; inline;
begin
 Result:=Pointer(PTRUINT(@m) and (not PTRUINT(1)));
end;

procedure mtx_lock(var m:mtx);
var
 i:Integer;
 old:t_fast_mutex;
 new:t_fast_mutex;
{$IFDEF DEBUG_MTX}
 rbp:Pointer;
{$ENDIF}
begin
 //Writeln('lock:',m.n,':',HexStr(@m));
 {$IFDEF DEBUG_MTX}
 if curkthread<>nil then
  curkthread^.td_debug_mtx:=@m;
 {$ENDIF}

 i:=0;

 repeat
  QWORD(old):=System.InterlockedExchangeAdd64(QWORD(m.fast_mutex),0);

  if (old.owned=0) or (old.owned=ThreadID) then
  begin
   new.recursion:=old.recursion+1;
   new.waiters  :=old.waiters;
   new.owned    :=ThreadID;

   if System.InterlockedCompareExchange64(QWORD(m.fast_mutex),QWORD(new),QWORD(old)) = QWORD(old) then
   begin
    Break;
   end;

  end else
  begin
   Inc(i);

   if (i>=100) then
   begin

    new:=old;
    new.waiters:=new.waiters+1;

    if System.InterlockedCompareExchange64(QWORD(m.fast_mutex),QWORD(new),QWORD(old)) = QWORD(old) then
    begin
     NtWaitForKeyedEvent(qhandle, GetKey(m), False, nil);
    end;

    i:=0;
   end;
  end;

 until false;

 {$IFDEF DEBUG_MTX}
 if curkthread<>nil then
  curkthread^.td_debug_mtx:=nil;
 rbp:=nil;
 asm
  movq %rbp,rbp
 end;
 m.debug_own[0]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[1]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[2]:=md_fuword(PPointer(rbp)[1]);
 {$ENDIF}
end;

function mtx_trylock(var m:mtx):Boolean;
var
 old:t_fast_mutex;
 new:t_fast_mutex;
{$IFDEF DEBUG_MTX}
 rbp:Pointer;
{$ENDIF}
begin

 repeat
  QWORD(old):=System.InterlockedExchangeAdd64(QWORD(m.fast_mutex),0);

  if (old.owned=0) or (old.owned=ThreadID) then
  begin
   new.recursion:=old.recursion+1;
   new.waiters  :=old.waiters;
   new.owned    :=ThreadID;

   if System.InterlockedCompareExchange64(QWORD(m.fast_mutex),QWORD(new),QWORD(old)) = QWORD(old) then
   begin
    Result:=True;
    Break;
   end;

  end else
  begin
   Result:=False;
   Break;
  end;

 until false;

 {$IFDEF DEBUG_MTX}
 if Result then
 begin
  rbp:=nil;
  asm
   movq %rbp,rbp
  end;
 m.debug_own[0]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[1]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[2]:=md_fuword(PPointer(rbp)[1]);
 end;
 {$ENDIF}
end;

procedure mtx_unlock(var m:mtx);
var
 old:t_fast_mutex;
 new:t_fast_mutex;
begin
 mtx_assert(m);

 repeat

  QWORD(old):=System.InterlockedExchangeAdd64(QWORD(m.fast_mutex),0);

  if (old.owned=0) or (old.owned<>ThreadID) or (old.recursion=0) then
  begin
   //current thread not owned
   Exit;
  end;

  new:=old;

  if (old.recursion=1) then
  begin
   new.recursion:=0;
   new.owned    :=0;

   if (new.waiters<>0) then
   begin
    new.waiters:=new.waiters-1;
   end;

  end else
  begin
   new.recursion:=old.recursion-1;
  end;

  {$IFDEF DEBUG_MTX}
  if (new.owned=0) then
  begin
   m.debug_own[0]:=nil;
   m.debug_own[1]:=nil;
   m.debug_own[2]:=nil;
  end;
  {$ENDIF}

  if System.InterlockedCompareExchange64(QWORD(m.fast_mutex),QWORD(new),QWORD(old)) = QWORD(old) then
  begin

   if (new.recursion=0) and (old.waiters<>0) then
   begin
    NtReleaseKeyedEvent(qhandle, GetKey(m), False, nil);
   end;

   Break;
  end;

 until false;
end;

function mtx_owned(var m:mtx):Boolean; inline;
begin
 Result:=m.fast_mutex.owned=ThreadID;
end;

procedure mtx_assert(var m:mtx); inline;
begin
 if not mtx_owned(m) then
 begin
  Assert(false,'mtx_assert:'+IntToStr(m.fast_mutex.owned)+'<>'+IntToStr(ThreadID));
 end;
end;

initialization
 NtCreateKeyedEvent(@qhandle, DWORD(-1), nil, 0);

end.

