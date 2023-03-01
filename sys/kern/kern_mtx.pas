unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 mtx=TRTLCriticalSection;

procedure mtx_init(var m:mtx);
procedure mtx_destroy(var m:mtx);
procedure mtx_lock(var m:mtx);
procedure mtx_unlock(var m:mtx);

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

procedure mtx_unlock(var m:mtx); inline;
begin
 LeaveCriticalSection(m);
end;


end.

