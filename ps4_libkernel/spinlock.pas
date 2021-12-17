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

function  spin_trylock(Var P:Pointer):Boolean; inline;
function  spin_trylock(Var P:DWORD):Boolean; inline;
function  spin_tryunlock(Var P:Pointer):Boolean; inline;
function  spin_tryunlock(Var P:DWORD):Boolean; inline;
procedure spin_lock(Var P:Pointer);
procedure spin_lock(Var P:DWORD);
procedure spin_unlock(Var P:Pointer);
procedure spin_unlock(Var P:DWORD);

function  event_try_enable(Var P:Pointer):Boolean;
function  event_try_enable(Var P:DWORD):Boolean;
function  event_try_disable(Var P:Pointer):Boolean;
function  event_try_disable(Var P:DWORD):Boolean;
procedure event_disable(Var P:Pointer);
procedure event_disable(Var P:DWORD);

implementation

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

function XCHG(Var addr:DWORD;New:DWORD):DWORD; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

function CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function CAS(Var addr:DWORD;Comp,New:DWORD):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

Procedure store_release(Var addr:Pointer;v:Pointer); inline;
begin
 WriteBarrier;
 addr:=v;
end;

Procedure store_release(Var addr:DWORD;v:DWORD); inline;
begin
 WriteBarrier;
 addr:=v;
end;

procedure spin_pause; assembler; nostackframe;
asm
 pause
end;

Procedure backoff_exp.Wait;
Var
 n:SizeUInt;
begin
 if (m_nExpCur<=upper_bound) then
 begin
  For n:=0 to m_nExpCur-1 do
  begin
   spin_pause;
  end;
  m_nExpCur:=m_nExpCur*2;
 end else
 begin
  System.ThreadSwitch;
 end;
end;

Procedure backoff_exp.Reset;
begin
 m_nExpCur:=lower_bound;
end;

function spin_trylock(Var P:Pointer):Boolean; inline;
begin
 Result:=XCHG(P,Pointer(1))=nil;
end;

function spin_trylock(Var P:DWORD):Boolean; inline;
begin
 Result:=XCHG(P,1)=0;
end;

function spin_tryunlock(Var P:Pointer):Boolean; inline;
begin
 Result:=XCHG(P,nil)=Pointer(1);
end;

function spin_tryunlock(Var P:DWORD):Boolean; inline;
begin
 Result:=XCHG(P,0)=1;
end;

procedure spin_lock(Var P:Pointer);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(P,Pointer(1))<>nil) do bkoff.Wait;
end;

procedure spin_lock(Var P:DWORD);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(P,1)<>0) do bkoff.Wait;
end;

procedure spin_unlock(Var P:Pointer);
begin
 store_release(P,nil);
end;

procedure spin_unlock(Var P:DWORD);
begin
 store_release(P,0);
end;

const
 EVL_DIS=0; //disable
 EVL_NEW=1; //new
 EVL_ENB=2; //enable

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
 Result:=CAS(P,Pointer(EVL_ENB),Pointer(EVL_DIS));
end;

function event_try_disable(Var P:DWORD):Boolean;
begin
 Result:=CAS(P,EVL_ENB,EVL_DIS);
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

