unit kern_lock;

{$mode ObjFPC}{$H+}

interface

uses
 atomic,
 ntapi;

procedure klock(Var P:Integer);
procedure kunlock(Var P:Integer);

implementation

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

Procedure backoff_exp.Wait;
Var
 n:SizeInt;
begin
 if (m_nExpCur<=upper_bound) then
 begin
  For n:=0 to m_nExpCur-1 do
  begin
   NtYieldExecution;
  end;
  m_nExpCur:=m_nExpCur*2;
 end else
 begin
  n:=-1000;
  NtDelayExecution(False,@n);
 end;
end;

Procedure backoff_exp.Reset;
begin
 m_nExpCur:=lower_bound;
end;

procedure klock(Var P:Integer);
Var
 bkoff:backoff_exp;
begin
 bkoff.Reset;
 While (XCHG(P,1)<>0) do bkoff.Wait;
end;

procedure kunlock(Var P:Integer);
begin
 store_release(P,0);
end;

end.

