unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_mtx=^mtx;
 mtx=TRTLCriticalSection;

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

