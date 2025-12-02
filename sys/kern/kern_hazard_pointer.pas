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
   Procedure WaitFor(P:Pointer); static;
   Procedure Flush; static;
   Procedure FLazy; static;
 end;

procedure hazard_init;

implementation

uses
 atomic,
 mqueue,
 LFQueue,
 g_node_splay,
 kern_thr,
 time,
 md_sleep,
 kern_daemon;

var
 rlist_bs:LIST_HEAD=(lh_first:nil);
 rlist_lf:TIntrusiveMPSCQueue=(tail_:@rlist_lf.stub_;stub_:(next_:nil);head_:@rlist_lf.stub_);
 rcount  :Integer=0;

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

procedure WaitForRetire(P:Pointer);
label
 _again;
var
 p_data:Pointer;
 ttd   :p_kthread;
 i     :Byte;
begin
 if (P=nil) or (P=Pointer(1)) then Exit;

 _again:

 threads_lock;

 ttd:=TAILQ_FIRST(get_p_threads);
 while (ttd<>nil) do
 begin

  if (ttd<>curkthread) then
  begin
   For i:=0 to High(kthread.td_guards) do
   begin
    p_data:=load_acq_rel(ttd^.td_guards[i]);

    if (p_data=P) then
    begin
     threads_unlock;
     msleep_td(hz div 10000);
     goto _again;
    end;

   end;
  end;

  ttd:=TAILQ_NEXT(ttd,@ttd^.td_plist)
 end;

 threads_unlock;
end;

type
 t_scan_mode=(smLazy,smForce);

Procedure Scan(mode:t_scan_mode);
label
 _again;
var
 p_set :TPointerSet;
 p_data:Pointer;
 p_node:p_pointer_node;
 r_node:p_r_node;
 r_next:p_r_node;
 ttd   :p_kthread;
 f_list:LIST_HEAD;
 i     :Byte;
begin

 _again:

 p_set :=Default(TPointerSet);
 r_node:=nil;
 f_list:=Default(LIST_HEAD);

 if (mode=smForce) then
 begin
  threads_lock;
 end else
 begin
  if not threads_trylock then Exit;
 end;

 //flush to base list
 while rlist_lf.Pop(r_node) do
 begin
  LIST_INSERT_HEAD(@rlist_bs,r_node,@r_node^.entry);
 end;

 r_node:=LIST_FIRST(@rlist_bs);
 if (r_node=nil) then
 begin
  //zero list
  threads_unlock;
  Exit;
 end;

 ttd:=TAILQ_FIRST(get_p_threads);
 while (ttd<>nil) do
 begin

  For i:=0 to High(kthread.td_guards) do
  begin
   p_data:=load_acq_rel(ttd^.td_guards[i]);
   if (p_data<>nil) and
      (p_data<>Pointer(1)) then
   begin
    p_node:=AllocMem(SizeOf(t_pointer_node));
    p_node^.P:=p_data;
    p_set.Insert(p_node);
   end;
  end;

  ttd:=TAILQ_NEXT(ttd,@ttd^.td_plist)
 end;

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
   //add to free list
   LIST_INSERT_HEAD(@f_list,r_node,@r_node^.entry);
  end;
  //
  r_node:=r_next;
 end;

 threads_unlock;

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

 //free elements
 r_node:=LIST_FIRST(@f_list);
 while (r_node<>nil) do
 begin
  LIST_REMOVE(r_node,@r_node^.entry);
  //free element
  if (r_node^.F<>nil) then
  begin
   r_node^.F(r_node^.P);
  end;
  //free node
  System.InterlockedDecrement(rcount);
  FreeMem(r_node);
  //
  r_node:=LIST_FIRST(@f_list);
 end;

 if (mode=smForce) and
    (LIST_FIRST(@rlist_bs)<>nil) then
 begin
  msleep_td(hz div 10000);
  goto _again;
 end;

end;

Procedure Retire(P:Pointer;FuncFree:TGuard.TFuncFree);
var
 node:p_r_node;
begin
 node:=AllocMem(SizeOf(t_r_node));
 node^.P:=P;
 node^.F:=FuncFree;
 //
 rlist_lf.Push(node);
 System.InterlockedIncrement(rcount);
 //
 if rcount>(4*256) then
 begin
  Scan(smLazy);
 end;
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

Procedure TGuard.WaitFor(P:Pointer);
begin
 WaitForRetire(P);
end;

Procedure TGuard.Flush;
begin
 Scan(smForce);
end;

Procedure TGuard.FLazy;
begin
 Scan(smLazy);
end;

Procedure Guard_Lazy; SysV_ABI_CDecl;
begin
 Scan(smLazy);
end;

/////////

var
 stub:t_daemon_node;

procedure hazard_init;
begin
 sys_daemon_add_cbs(@stub,@Guard_Lazy);
end;

end.

