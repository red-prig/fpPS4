unit ps4_guard_internal;

{$mode ObjFPC}{$H+}

interface

type
 p_guard_t=^guard_t;
 guard_t=packed record
  init_half:DWORD;
  lock_half:DWORD;
 end;
 guard_lock_t=DWORD;

function  ps4___cxa_guard_acquire(guard_object:p_guard_t):Integer; SysV_ABI_CDecl;
procedure ps4___cxa_guard_abort(guard_object:p_guard_t); SysV_ABI_CDecl;
procedure ps4___cxa_guard_release(guard_object:p_guard_t); SysV_ABI_CDecl;

implementation

const
 LOCKED     =1;
 INITIALISED=1;
 INITIAL    =0;

function INIT_PART(guard:p_guard_t):PDWORD; inline;
begin
 Result:=@guard^.init_half;
end;

function LOCK_PART(guard:p_guard_t):PDWORD; inline;
begin
 Result:=@guard^.lock_half;
end;

{
 * Acquires a lock on a guard, returning 0 if the object has already been
 * initialised, and 1 if it has not.  If the object is already constructed then
 * this function just needs to read a byte from memory and return.
}
function ps4___cxa_guard_acquire(guard_object:p_guard_t):Integer; SysV_ABI_CDecl;
var
 old:guard_lock_t;
begin
 // Not an atomic read, doesn't establish a happens-before relationship, but
 // if one is already established and we end up seeing an initialised state
 // then it's a fast path, otherwise we'll do something more expensive than
 // this test anyway...
 if (INITIALISED = INIT_PART(guard_object)^) then
  Exit(0);
 // Spin trying to do the initialisation
 repeat
  // Loop trying to move the value of the guard from 0 (not
  // locked, not initialised) to the locked-uninitialised
  // position.

  old:=InterlockedCompareExchange(LOCK_PART(guard_object)^,INITIAL,LOCKED);
  if (old = INITIAL) then
  begin
   if (INITIALISED <> INIT_PART(guard_object)^) then
    Exit(1);

   // No need for a memory barrier here,
   // see first comment.
   LOCK_PART(guard_object)^ := INITIAL;
   Exit(0);
  end;

  assert(old = LOCKED);
  // Another thread holds the lock.
  ThreadSwitch;
 until false;
end;

{
 * Releases the lock without marking the object as initialised.  This function
 * is called if initialising a static causes an exception to be thrown.
}
procedure ps4___cxa_guard_abort(guard_object:p_guard_t); SysV_ABI_CDecl;
var
 reset:Boolean;
begin
 reset:=InterlockedCompareExchange(LOCK_PART(guard_object)^,LOCKED,INITIAL)=INITIAL;
 assert(reset);
end;

{
 * Releases the guard and marks the object as initialised.  This function is
 * called after successful initialisation of a static.
}
procedure ps4___cxa_guard_release(guard_object:p_guard_t); SysV_ABI_CDecl;
var
 reset:Boolean;
begin
 reset:=InterlockedCompareExchange(INIT_PART(guard_object)^,INITIAL,INITIALISED)=INITIALISED;
 assert(reset);
 LOCK_PART(guard_object)^ := INITIAL;
end;

end.

