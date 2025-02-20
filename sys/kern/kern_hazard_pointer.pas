unit kern_hazard_pointer;

{$mode ObjFPC}{$H+}

interface

type
 TGuard=object
  private
   type
    PGuardHandle=^TGuardHandle;
    TGuardHandle=packed record
     Item:Pointer;
    end;
   Var
    Handle:PGuardHandle;
  public
   type
    TFuncFree=Function(P:Pointer):SizeUInt;
    TFuncGet =function(P:Pointer):Pointer;
   function  New:TGuard; static;
   procedure Free;
   procedure Clear;
   function  Assign(P:Pointer):Pointer;
   function  Get:Pointer;
   function  Protect(Var P:Pointer;Func:TFuncGet=nil):Pointer;
   Procedure Retire (P:Pointer;FuncFree:TFuncFree); static;
   Procedure Flush; static;
 end;

Procedure tlHpInit;
Procedure tlHpFree;

implementation

uses
 atomic,
 mqueue,
 g_node_splay,
 kern_thr;

function AllocGuard:Pointer;
var
 td:p_kthread;
 i:Byte;
begin
 Result:=nil;
 td:=curkthread;
 Assert(td<>nil,'AllocGuard#1');

 For i:=0 to High(kthread.td_guards) do
 if (td^.td_guards[i]=nil) then
 begin
  td^.td_guards[i]:=Pointer(1);
  Exit(@td^.td_guards[i]);
 end;

 Assert(false,'AllocGuard#2');
end;

Procedure FreeGuard(P:Pointer); inline;
begin
 PPointer(P)^:=nil;
end;

////////

type
 p_r_node=^t_r_node;
 t_r_node=record
  entry:LIST_ENTRY;
  //
  P:Pointer;
  F:TGuard.TFuncFree;
 end;

 p_pointer_node=^t_pointer_node;
 t_pointer_node=object
  //key should be first
  P:Pointer;
  //
  pLeft :p_pointer_node;
  pRight:p_pointer_node;
  //
  function c(n1,n2:p_pointer_node):Integer; static;
 end;
 TPointerSet=specialize TNodeSplay<t_pointer_node>;

function t_pointer_node.c(n1,n2:p_pointer_node):Integer;
begin
 Result:=Integer(n1^.P>n2^.P)-Integer(n1^.P<n2^.P);
end;

threadvar
 rlist :LIST_HEAD;
 rcount:Integer;

type
 t_scan_mode=(smLazy,smLazyOne,smForce);

function Scan(mode:t_scan_mode):Pointer;
label
 _again;
var
 p_set :TPointerSet;
 p_node:p_pointer_node;
 r_node:p_r_node;
 r_next:p_r_node;
 ttd   :p_kthread;
 i     :Byte;
begin
 Result:=nil;

 _again:

 r_node:=LIST_FIRST(@rlist);
 if (r_node=nil) then Exit;

 p_set:=Default(TPointerSet);

 if (mode=smForce) then
 begin
  threads_lock;
 end else
 begin
  if not threads_trylock then Exit;
 end;

 ttd:=TAILQ_FIRST(get_p_threads);
 while (ttd<>nil) do
 begin

  For i:=0 to High(kthread.td_guards) do
  if (ttd^.td_guards[i]<>nil) and
     (ttd^.td_guards[i]<>Pointer(1)) then
  begin
   p_node:=AllocMem(SizeOf(t_pointer_node));
   p_node^.P:=ttd^.td_guards[i];
   p_set.Insert(p_node);
  end;

  ttd:=TAILQ_NEXT(ttd,@ttd^.td_plist)
 end;

 threads_unlock;

 while (r_node<>nil) do
 begin
  r_next:=LIST_NEXT(r_node,@r_node^.entry);
  //
  p_node:=p_set.Find(@r_node^.P);
  //
  if (p_node=nil) then
  begin
   //delete node
   LIST_REMOVE(r_node,@r_node^.entry);
   //free element
   if (r_node^.F<>nil) then
   begin
    r_node^.F(r_node^.P);
   end;
   //
   if (mode=smLazyOne) then
   begin
    //set result and exit
    Dec(rcount);
    Result:=r_node;
    Break;
   end else
   begin
    //free node
    Dec(rcount);
    FreeMem(r_node);
   end;
  end;
  //
  r_node:=r_next;
 end;

 //free set
 p_node:=p_set.Min;
 while (p_node<>nil) do
 begin
  p_set.Delete(p_node);
  //
  FreeMem(p_node);
  //
  p_node:=p_set.Min;
 end;

 if (mode=smForce) and
    (LIST_FIRST(@rlist)<>nil) then
 begin
  goto _again;
 end;
end;

Procedure Retire(P:Pointer;FuncFree:TGuard.TFuncFree);
var
 node:p_r_node;
begin
 node:=Scan(smLazyOne);
 //
 if (node<>nil) then
 begin
  node:=AllocMem(SizeOf(t_r_node));
 end;
 node^.P:=P;
 node^.F:=FuncFree;
 //
 LIST_INSERT_HEAD(@rlist,node,@node^.entry);
 //
 Inc(rcount);
 //
 if rcount>(4*256) then
 begin
  Scan(smLazy);
 end;
end;

Procedure tlHpInit; public;
begin
 rlist :=Default(LIST_HEAD);
 rcount:=0;
end;

Procedure tlHpFree; public;
begin
 Scan(smForce);
end;

////////

function TGuard.New:TGuard;
begin
 Result.Handle:=AllocGuard;
 Result.Clear;
end;

procedure TGuard.Free;
begin
 if Assigned(Handle) then
 begin
  Clear;
  FreeGuard(Handle);
  Handle:=nil;
 end;
end;

procedure TGuard.Clear;
begin
 if Assigned(Handle) then
 begin
  PGuardHandle(Handle)^.Item:=Pointer(1);
 end;
end;

function TGuard.Assign(P:Pointer):Pointer;
begin
 Result:=nil;
 if Assigned(Handle) then
 begin
  store_seq_cst(PGuardHandle(Handle)^.Item,P);
  Result:=P;
 end;
end;

function TGuard.Get:Pointer;
begin
 Result:=nil;
 if Assigned(Handle) then
 begin
  Result:=PGuardHandle(Handle)^.Item;
 end;
end;

function TGuard.Protect(Var P:Pointer;Func:TFuncGet=nil):Pointer;
Var
 pCur,pRet:Pointer;
begin
 Assert(Handle<>nil);
 Result:=nil;
 if Assigned(Handle) then
 begin
  pCur:=load_acq_rel(P);
  repeat
   if (Func=nil) then
   begin
    store_seq_cst(PGuardHandle(Handle)^.Item,pCur);
   end else
   begin
    store_seq_cst(PGuardHandle(Handle)^.Item,Func(pCur));
   end;
   pRet:=load_acquire(pCur);
   pCur:=load_acq_rel(P);
  until (pRet=pCur);
  Result:=pCur;
 end;
end;

Procedure TGuard.Retire(P:Pointer;FuncFree:TFuncFree);
begin
 if Assigned(P) and Assigned(FuncFree) then
 begin
  Retire(P,FuncFree);
 end;
end;

Procedure TGuard.Flush;
begin
 Scan(smForce);
end;

/////////

end.

