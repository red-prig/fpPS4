unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_mtx=^mtx;
 mtx=TRTLCriticalSection;

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

procedure mtx_init   (var m:mtx);
procedure mtx_destroy(var m:mtx);
procedure mtx_lock   (var m:mtx);
function  mtx_trylock(var m:mtx):Boolean;
procedure mtx_unlock (var m:mtx);
function  mtx_owned  (var m:mtx):Boolean;
procedure mtx_assert (var m:mtx);

implementation

procedure mtx_init(var m:mtx); inline;
begin
 InitCriticalSection(m);
end;

procedure mtx_destroy(var m:mtx); inline;
begin
 DoneCriticalSection(m);
end;

procedure mtx_lock(var m:mtx); inline;
begin
 EnterCriticalSection(m);
end;

function mtx_trylock(var m:mtx):Boolean; inline;
begin
 Result:=TryEnterCriticalSection(m)<>0;
end;

procedure mtx_unlock(var m:mtx); inline;
begin
 LeaveCriticalSection(m);
end;

function mtx_owned(var m:mtx):Boolean; inline;
begin
 Result:=m.OwningThread=GetCurrentThreadId;
end;

procedure mtx_assert(var m:mtx); inline;
begin
 Assert(mtx_owned(m));
end;

end.

