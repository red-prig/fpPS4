unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils;

type
 p_mtx=^mtx;
 mtx=packed record
  n:PChar;
  c:TRTLCriticalSection;
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
 kern_thr;
{$ENDIF}

procedure mtx_init(var m:mtx;name:PChar); inline;
begin
 m.n:=name;
 InitCriticalSection(m.c);
end;

procedure mtx_destroy(var m:mtx); inline;
begin
 DoneCriticalSection(m.c);
end;

procedure mtx_lock(var m:mtx); {$IFNDEF DEBUG_MTX} inline; {$ENDIF}
{$IFDEF DEBUG_MTX}
var
 rbp:Pointer;
{$ENDIF}
begin
 {$IFDEF DEBUG_MTX}
 curkthread^.td_debug_mtx:=@m;
 {$ENDIF}
 EnterCriticalSection(m.c);
 {$IFDEF DEBUG_MTX}
 curkthread^.td_debug_mtx:=nil;
 rbp:=nil;
 asm
  movq %rbp,rbp
 end;
 m.debug_own[0]:=PPointer(rbp)[1]; rbp:=PPointer(rbp)[0];
 m.debug_own[1]:=PPointer(rbp)[1]; rbp:=PPointer(rbp)[0];
 m.debug_own[2]:=PPointer(rbp)[1];
 {$ENDIF}
end;

function mtx_trylock(var m:mtx):Boolean; {$IFNDEF DEBUG_MTX} inline; {$ENDIF}
{$IFDEF DEBUG_MTX}
var
 rbp:Pointer;
{$ENDIF}
begin
 Result:=TryEnterCriticalSection(m.c)<>0;
 {$IFDEF DEBUG_MTX}
 if Result then
 begin
  rbp:=nil;
  asm
   movq %rbp,rbp
  end;
 m.debug_own[0]:=PPointer(rbp)[1]; rbp:=PPointer(rbp)[0];
 m.debug_own[1]:=PPointer(rbp)[1]; rbp:=PPointer(rbp)[0];
 m.debug_own[2]:=PPointer(rbp)[1];
 end;
 {$ENDIF}
end;

procedure mtx_unlock(var m:mtx); {$IFNDEF DEBUG_MTX} inline; {$ENDIF}
begin
 {$IFDEF DEBUG_MTX}
 m.debug_own[0]:=nil;
 m.debug_own[1]:=nil;
 m.debug_own[2]:=nil;
 {$ENDIF}
 LeaveCriticalSection(m.c);
end;

function mtx_owned(var m:mtx):Boolean; inline;
begin
 Result:=m.c.OwningThread=GetCurrentThreadId;
end;

procedure mtx_assert(var m:mtx); inline;
begin
 Assert(mtx_owned(m),IntToStr(m.c.OwningThread)+'<>'+IntToStr(GetCurrentThreadId));
end;

end.

