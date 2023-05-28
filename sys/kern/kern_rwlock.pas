unit kern_rwlock;

{$mode ObjFPC}{$H+}

interface

procedure rw_rlock    (Var SRWLock:Pointer);         stdcall; external 'kernel32' name 'AcquireSRWLockShared'      ;
procedure rw_runlock  (Var SRWLock:Pointer);         stdcall; external 'kernel32' name 'ReleaseSRWLockShared'      ;
procedure rw_wlock    (Var SRWLock:Pointer);         stdcall; external 'kernel32' name 'AcquireSRWLockExclusive'   ;
procedure rw_wunlock  (Var SRWLock:Pointer);         stdcall; external 'kernel32' name 'ReleaseSRWLockExclusive'   ;
Function  rw_try_rlock(Var SRWLock:Pointer):Boolean; stdcall; external 'kernel32' name 'TryAcquireSRWLockShared'   ;
Function  rw_try_wlock(Var SRWLock:Pointer):Boolean; stdcall; external 'kernel32' name 'TryAcquireSRWLockExclusive';

implementation

end.

