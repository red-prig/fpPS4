unit spinlock;

{$mode objfpc}{$H+}

interface

type
 backoff_exp=object
  private
   Const
    lower_bound = 16;        ///< Minimum spinning limit
    upper_bound = 16*1024;   ///< Maximum spinning limit
   Var
    m_nExpCur:SizeUInt;      //=lower_bound
  public
   Procedure Wait;
   Procedure Reset;
 end;

 r_spin_lock=record
  _lock:DWORD;
  count:DWORD;
  owner:DWORD;
 end;

function  spin_trylock(Var P:Pointer):Boolean;
function  spin_trylock(Var P:SizeUint):Boolean;
{$IF defined(CPUX86_64)}
function  spin_trylock(Var P:DWORD):Boolean;
{$ENDIF}
function  spin_tryunlock(Var P:Pointer):Boolean;
function  spin_tryunlock(Var P:SizeUint):Boolean;
{$IF defined(CPUX86_64)}
function  spin_tryunlock(Var P:DWORD):Boolean;
{$ENDIF}
procedure spin_lock(Var P:Pointer);
procedure spin_lock(Var P:SizeUint);
{$IF defined(CPUX86_64)}
procedure spin_lock(Var P:DWORD);
{$ENDIF}
procedure spin_unlock(Var P:Pointer);
procedure spin_unlock(Var P:SizeUint);
{$IF defined(CPUX86_64)}
procedure spin_unlock(Var P:DWORD);
{$ENDIF}

function  spin_trylock(var t:r_spin_lock):Boolean;
procedure spin_lock(var t:r_spin_lock);
procedure spin_unlock(var t:r_spin_lock);

Procedure wait_until_equal(Var P:Pointer;V:Pointer);
Procedure wait_until_equal(Var P:SizeUint;V:SizeUint);
{$IF defined(CPUX86_64)}
Procedure wait_until_equal(Var P:DWORD;V:DWORD);
Procedure wait_until_equal(Var P:Integer;V:Integer);
{$ENDIF}

const
 EVL_DIS=0; //disable
 EVL_NEW=1; //new
 EVL_ENB=2; //enable

function  event_try_enable(Var P:Pointer):Boolean;
function  event_try_enable(Var P:DWORD):Boolean;
function  event_try_disable(Var P:Pointer):Boolean;
function  event_try_disable(Var P:DWORD):Boolean;
procedure event_disable(Var P:Pointer);
procedure event_disable(Var P:DWORD);

implementation

Uses
 atomic,
 sys_kernel,
 sys_signal;

Procedure backoff_exp.Wait;
Var
 n:Int64;
begin
 if (m_nExpCur<=upper_bound) then
 begin
  For n:=0 to m_nExpCur-1 do
  begin
   SwYieldExecution;
  end;
  m_nExpCur:=m_nExpCur*2;
 end else
 begin
  n:=-1000;
  SwDelayExecution(False,@n);
 end;
end;

Procedure backoff_exp.Reset;
begin
 m_nExpCur:=lower_bound;
end;

function spin_trylock(Var P:Pointer):Boolean;
begin
 Result:=XCHG(P,Pointer(1))=nil;
end;

function spin_trylock(Var P:SizeUint):Boolean;
begin
 Result:=XCHG(P,1)=0;
end;

{$IF defined(CPUX86_64)}
function spin_trylock(Var P:DWORD):Boolean;
begin
 Result:=XCHG(P,1)=0;
end;
{$ENDIF}

function spin_tryunlock(Var P:Pointer):Boolean;
begin
 Result:=XCHG(P,nil)=Pointer(1);
end;

function spin_tryunlock(Var P:SizeUint):Boolean;
begin
 Result:=XCHG(P,0)=1;
end;

{$IF defined(CPUX86_64)}
function spin_tryunlock(Var P:DWORD):Boolean;
begin
 Result:=XCHG(P,0)=1;
end;
{$ENDIF}

procedure spin_lock(Var P:Pointer);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(P,Pointer(1))<>nil) do bkoff.Wait;
end;

procedure spin_lock(Var P:SizeUint);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(P,1)<>0) do bkoff.Wait;
end;

{$IF defined(CPUX86_64)}
procedure spin_lock(Var P:DWORD);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(P,1)<>0) do bkoff.Wait;
end;
{$ENDIF}

procedure spin_unlock(Var P:Pointer);
begin
 store_release(P,nil);
end;

procedure spin_unlock(Var P:SizeUint);
begin
 store_release(P,0);
end;

{$IF defined(CPUX86_64)}
procedure spin_unlock(Var P:DWORD);
begin
 store_release(P,0);
end;
{$ENDIF}

//recrusive spin lock

function spin_trylock(var t:r_spin_lock):Boolean;
begin
 Result:=True;
 _sig_lock;
  if spin_trylock(t._lock) then
  begin
   t.count:=0;
   t.owner:=GetCurrentThreadId;
  end else
  if (t.owner=GetCurrentThreadId) then
  begin
   Inc(t.count);
  end else
  begin
   Result:=False;
  end;
 _sig_unlock;
end;

procedure spin_lock(var t:r_spin_lock);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (not spin_trylock(t)) do bkoff.Wait;
end;

procedure spin_unlock(var t:r_spin_lock);
begin
 _sig_lock;
  if (t.count=0) then
  begin
   t.owner:=DWORD(-1);
   spin_unlock(t._lock);
  end else
  begin
   Dec(t.count);
  end;
 _sig_unlock;
end;

//

Procedure wait_until_equal(Var P:Pointer;V:Pointer);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (load_acq_rel(P)=V) do bkoff.Wait;
end;

Procedure wait_until_equal(Var P:SizeUint;V:SizeUint);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (load_acq_rel(P)=V) do bkoff.Wait;
end;

{$IF defined(CPUX86_64)}
Procedure wait_until_equal(Var P:DWORD;V:DWORD);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (load_acq_rel(P)=V) do bkoff.Wait;
end;

Procedure wait_until_equal(Var P:Integer;V:Integer);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (load_acq_rel(P)=V) do bkoff.Wait;
end;
{$ENDIF}

function event_try_enable(Var P:Pointer):Boolean;
begin
 Result:=(XCHG(P,Pointer(EVL_NEW))=Pointer(EVL_DIS));
 if Result then
 begin
  store_release(P,Pointer(EVL_ENB));
 end;
end;

function event_try_enable(Var P:DWORD):Boolean;
begin
 Result:=(XCHG(P,EVL_NEW)=EVL_DIS);
 if Result then
 begin
  store_release(P,EVL_ENB);
 end;
end;

function event_try_disable(Var P:Pointer):Boolean;
begin
 Result:=False;
 Case SizeUint(_CAS(P,Pointer(EVL_ENB),Pointer(EVL_DIS))) of
  EVL_DIS,
  EVL_ENB:Result:=True;
  EVL_NEW:store_release(P,Pointer(EVL_ENB));
  else;
 end;
end;

function event_try_disable(Var P:DWORD):Boolean;
begin
 Result:=False;
 Case _CAS(P,EVL_ENB,EVL_DIS) of
  EVL_DIS,
  EVL_ENB:Result:=True;
  EVL_NEW:store_release(P,EVL_ENB);
  else;
 end;
end;

procedure event_disable(Var P:Pointer);
begin
 store_release(P,Pointer(EVL_DIS));
end;

procedure event_disable(Var P:DWORD);
begin
 store_release(P,EVL_DIS);
end;

end.

