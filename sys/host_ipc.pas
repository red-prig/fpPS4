unit host_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 SysUtils,
 time,
 mqueue,
 LFQueue,
 host_ipc_interface,
 kern_thr,
 sys_event,
 md_event,
 kern_mtx;

const
 iRESULT=host_ipc_interface.iRESULT;

type
 TIpcValue      =host_ipc_interface.TIpcValue;
 TOnMessage     =host_ipc_interface.TOnMessage;
 THostIpcHandler=host_ipc_interface.THostIpcHandler;
 THostIpcResult =host_ipc_interface.THostIpcResult;
 THostIpc       =host_ipc_interface.THostIpc;

 THostIpcConnect=class;

 THostIpcDispatcher=class
  protected
   var
    FRefs:Integer;
    FTerm:Boolean;
  public
   FHandler:THostIpcHandler;
   Constructor Create(_Handler:THostIpcHandler);
   Destructor  Destroy; override;
   procedure   Acquire;
   procedure   Release;
   procedure   DoMethod  (Client:THostIpcConnect;mtype,mtid:DWORD;input:TIpcValue);
   procedure   DoDispatch(Client:THostIpcConnect;mtype,mtid:DWORD;input:TIpcValue); virtual;
   procedure   thread_new;  virtual;
   procedure   thread_free; virtual;
   Function    GetCallback(mtype:DWORD):TOnMessage; virtual;
   procedure   Update(); virtual;
 end;

 THostIpcDispatchQueue=class(THostIpcDispatcher)
  public
   type
    PNodeHeader=^TNodeHeader;
    TNodeHeader=packed record
     mtype:DWORD;
     mlen :DWORD;
     mtid :DWORD;
     buf  :record end;
    end;

    PQNode=^TQNode;
    TQNode=packed record
     next_ :PQNode;
     Client:THostIpcConnect;
     header:TNodeHeader;
     value :TIpcValue;
     buf   :record end;
    end;
  protected
   var
    FQueue:TIntrusiveMPSCQueue;
  public
   Constructor Create(_Handler:THostIpcHandler);
   Destructor  Destroy; override;
   function    QueueRecv:PQNode;
   procedure   QueueSend(Client:THostIpcConnect;mtype,mtid:DWORD;value:TIpcValue); virtual;
   procedure   QueuePush(node:PQNode); virtual;
   procedure   QueueFlush;
   procedure   DoDispatch(Client:THostIpcConnect;mtype,mtid:DWORD;input:TIpcValue); override;
   procedure   Update(); override;
 end;

 THostIpcClientResult=class(THostIpcResult)
  Client:THostIpcConnect;
  rid   :DWORD;
  //
  procedure  InvokeResult(value:TIpcValue); override;
  Destructor Destroy; override;
 end;

 THostIpcConnect=class(THostIpc)
  public
   type
    PNodeIpcSync=^TNodeIpcSync;
    TNodeIpcSync=packed record
     entry:LIST_ENTRY;
     event:t_event;
     value:TIpcValue;
     tid  :DWORD;
    end;
  protected
   var
    FDispatcher:THostIpcDispatcher;
    FWaits     :LIST_HEAD;
    FWLock     :mtx;
    FRefs      :Integer;
    FRTid      :Integer;
    FTerm      :Boolean;
    FBroke     :Boolean;
   procedure   SetDispatcher(_Dispatcher:THostIpcDispatcher);
   function    NewNodeSync:PNodeIpcSync;
   procedure   FreeNodeSync(node:PNodeIpcSync);
  public
   var
    FKevObj:TObject;
   //
   property    Dispatcher:THostIpcDispatcher read FDispatcher write SetDispatcher;
   //
   function    Handler:THostIpcHandler; override;
   //
   procedure   TriggerNodeSync(tid:DWORD;value:TIpcValue);
   //
   function    NewSyncKey:Pointer;       override;
   procedure   FreeSyncKey(key:Pointer); override;
   procedure   WaitSyncKey(key:Pointer); override;
   function    GetSyncValue(key:Pointer):TIpcValue; override;
   //
   procedure   Send    (mtype:DWORD;key:Pointer;value:TIpcValue); override;
   procedure   SendImpl(mtype,mtid:DWORD;value:TIpcValue); virtual;
   procedure   Disconnect();                               override;
   //
   function    HoldResult:THostIpcResult; override;
   procedure   InvokeResult(tid:DWORD;value:TIpcValue);
   //
   Constructor Create(_Dispatcher:THostIpcDispatcher);
   Destructor  Destroy; override;
   procedure   Acquire;
   procedure   Release;
 end;

 THostIpcDispatchGui=class(THostIpcDispatchQueue)
  procedure QueueSend(Client:THostIpcConnect;mtype,mtid:DWORD;value:TIpcValue); override;
  procedure QueuePush(node:PQNode); override;
 end;

 THostIpcDispatchKern=class(THostIpcDispatchQueue)
  FEvent:t_event;
  Ftd   :p_kthread;
  procedure QueueSend(Client:THostIpcConnect;mtype,mtid:DWORD;value:TIpcValue); override;
  procedure QueuePush(node:PQNode); override;
  procedure thread_new;  override;
  procedure thread_free; override;
 end;

 THostIpcSimple=class(THostIpcConnect)
  FDest:THostIpcSimple;
  procedure SendImpl(mtype,mtid:DWORD;value:TIpcValue); override;
 end;

operator := (A:RawByteString):TMsgHash;
operator := (A:DWORD):TMsgHash;

operator := (A:Integer):TIpcValue;
operator := (A:QWORD):TIpcValue;
operator := (A:RawByteString):TIpcValue;

implementation


operator := (A:RawByteString):TMsgHash;
begin
 Result:=Default(TMsgHash);
 Result.msg:=A;
end;

operator := (A:DWORD):TMsgHash;
begin
 Result:=Default(TMsgHash);
 Result.f_mtype:=A;
end;

//

operator := (A:Integer):TIpcValue;
begin
 Result:=TIpcValue.AsQWORD(A);
end;

operator := (A:QWORD):TIpcValue;
begin
 Result:=TIpcValue.AsQWORD(A);
end;

operator := (A:RawByteString):TIpcValue;
begin
 Result:=TIpcValue.New(@A[1],Length(A));
end;

//

Constructor THostIpcDispatcher.Create(_Handler:THostIpcHandler);
begin
 inherited Create;
 FHandler:=_Handler;
end;

Destructor THostIpcDispatcher.Destroy;
begin
 FTerm:=True;
 thread_free;
 inherited;
end;

procedure THostIpcDispatcher.Acquire;
begin
 System.InterlockedIncrement(FRefs);
end;

procedure THostIpcDispatcher.Release;
begin
 if System.InterlockedDecrement(FRefs)=0 then
 begin
  Free;
 end;
end;

procedure THostIpcDispatcher.DoMethod(Client:THostIpcConnect;mtype,mtid:DWORD;input:TIpcValue);
var
 output:TIpcValue;
 OnMsg :TOnMessage;
begin
 output:=Default(TIpcValue);

 if (Client<>nil) then
 if (not Client.FTerm) then
 begin
  Client.FRTid:=mtid;

  if (mtype=iRESULT) then
  begin
   Client.TriggerNodeSync(Client.FRTid,input);
   input:=Default(TIpcValue); //transfer owned
  end else
  begin
   OnMsg:=GetCallback(mtype);
   if (OnMsg<>nil) then
   begin
    output:=OnMsg(Client,input);
   end else
   begin
    //nop?
    output:=-1;
   end;
   //is sync
   if (Client.FRTid<>0) then
   begin
    Client.InvokeResult(Client.FRTid,output);
    output:=Default(TIpcValue); //transfer owned
   end;
  end;
 end;

 //
 input.Free;
 output.Free;
end;

procedure THostIpcDispatcher.DoDispatch(Client:THostIpcConnect;mtype,mtid:DWORD;input:TIpcValue);
begin
 DoMethod(Client,mtype,mtid,input);
end;

procedure THostIpcDispatcher.thread_new;
begin
 //
end;

procedure THostIpcDispatcher.thread_free;
begin
 //
end;

Function THostIpcDispatcher.GetCallback(mtype:DWORD):TOnMessage;
begin
 Result:=nil;
 if (FHandler<>nil) then
 begin
  Result:=FHandler.GetCallback(mtype);
 end;
end;

procedure THostIpcDispatcher.Update();
begin
 //
end;

//

Constructor THostIpcDispatchQueue.Create(_Handler:THostIpcHandler);
begin
 inherited;
 FQueue.Create;
end;

Destructor THostIpcDispatchQueue.Destroy;
begin
 FTerm:=True;
 QueueFlush;
 inherited;
end;

function THostIpcDispatchQueue.QueueRecv:PQNode;
begin
 Result:=nil;
 FQueue.Pop(Result);
end;

procedure THostIpcDispatchQueue.QueueSend(Client:THostIpcConnect;mtype,mtid:DWORD;value:TIpcValue);
var
 node:PQNode;
begin
 if (mtype=iRESULT) then
 begin
  //Trigger Direct!
  Client.TriggerNodeSync(mtid,value);
 end else
 begin
  Client.Acquire;
  //
  node:=AllocMem(SizeOf(TQNode));
  node^.Client:=Client;
  node^.header.mtype:=mtype;
  node^.header.mlen :=value.GetLen;
  node^.header.mtid :=mtid;
  //
  node^.value:=value.Copy;
  //
  FQueue.Push(node);
 end;
end;

procedure THostIpcDispatchQueue.QueuePush(node:PQNode);
begin
 if (node^.header.mtype=iRESULT) then
 begin
  //Trigger Direct!
  node^.Client.TriggerNodeSync(node^.header.mtid,node^.value);
  FreeMem(Node);
 end else
 begin
  node^.Client.Acquire;
  //
  FQueue.Push(node);
 end;
end;

procedure THostIpcDispatchQueue.QueueFlush;
var
 node:PQNode;
begin
 node:=nil;
 while FQueue.Pop(node) do
 begin
  node^.value.Free;
  FreeMem(node);
 end;
end;

procedure THostIpcDispatchQueue.DoDispatch(Client:THostIpcConnect;mtype,mtid:DWORD;input:TIpcValue);
begin
 QueueSend(Client,mtype,mtid,input);
end;

procedure THostIpcDispatchQueue.Update();
var
 node  :PQNode;
 Client:THostIpcConnect;
begin
 if FTerm then Exit;

 node:=QueueRecv;

 while (node<>nil) do
 begin
  //
  Client:=node^.Client;
  Assert(Client<>nil);

  DoMethod(Client,node^.header.mtype,node^.header.mtid,node^.value);

  //
  FreeMem(node);
  //
  Client.Release;
  //
  if FTerm then Exit;
  //
  node:=QueueRecv;
 end;
end;

//

Constructor THostIpcConnect.Create(_Dispatcher:THostIpcDispatcher);
begin
 inherited Create;
 Dispatcher:=_Dispatcher;
 FRefs:=1;
 LIST_INIT(@FWaits);
 mtx_init(FWLock,'ipc');
end;

Destructor THostIpcConnect.Destroy;
begin
 FreeAndNil(FKevObj);
 //
 Dispatcher:=nil;
 //
 mtx_destroy(FWLock);
 //
 inherited;
end;

procedure THostIpcConnect.Acquire;
begin
 System.InterlockedIncrement(FRefs);
end;

procedure THostIpcConnect.Release;
begin
 if System.InterlockedDecrement(FRefs)=0 then
 begin
  Free;
 end;
end;

procedure THostIpcConnect.Disconnect();
begin
 FTerm:=True;
end;

//

procedure THostIpcConnect.InvokeResult(tid:DWORD;value:TIpcValue);
begin
 Assert(tid<>iBROKEN);
 SendImpl(iRESULT,tid,value);
end;

//

procedure THostIpcClientResult.InvokeResult(value:TIpcValue);
begin
 if (Client<>nil) then
 begin
  Client.InvokeResult(rid,value);
 end;
end;

Destructor THostIpcClientResult.Destroy;
begin
 if (Client<>nil) then
 begin
  Client.Release;
 end;
end;

//

function THostIpcConnect.HoldResult:THostIpcResult;
var
 r:THostIpcClientResult;
begin
 if (FRTid=0) then Exit(nil);
 //
 Acquire;
 r:=THostIpcClientResult.Create;
 r.Client:=Self;
 r.rid   :=FRTid;
 //
 Result:=r;
 FRTid:=0;
end;

//

procedure THostIpcConnect.SetDispatcher(_Dispatcher:THostIpcDispatcher);
begin
 if (FDispatcher=_Dispatcher) then Exit;
 //
 if (FDispatcher<>nil) then
 begin
  FDispatcher.Release;
 end;
 //
 FDispatcher:=_Dispatcher;
 //
 if (FDispatcher<>nil) then
 begin
  FDispatcher.Acquire;
 end;
end;

function THostIpcConnect.Handler:THostIpcHandler;
begin
 Result:=nil;
 if (FDispatcher<>nil) then
 begin
  Result:=FDispatcher.FHandler;
 end;
end;

function THostIpcConnect.NewNodeSync:PNodeIpcSync;
var
 node:PNodeIpcSync;
begin
 node:=AllocMem(SizeOf(TNodeIpcSync));
 node^.tid:=ThreadID;

 ev_init(node^.event,'TNodeIpcSync');

 mtx_lock(FWLock);
  LIST_INSERT_HEAD(@FWaits,node,@node^.entry);
 mtx_unlock(FWLock);

 Result:=node;
end;

procedure THostIpcConnect.FreeNodeSync(node:PNodeIpcSync);
begin
 mtx_lock(FWLock);
  LIST_REMOVE(node,@node^.entry);
 mtx_unlock(FWLock);

 ev_destroy(node^.event);

 FreeMem(node);
end;

procedure THostIpcConnect.TriggerNodeSync(tid:DWORD;value:TIpcValue);
var
 node:PNodeIpcSync;
begin
 mtx_lock(FWLock);

  if (tid=iBROKEN) then
  begin
   FBroke:=True;
  end;

  node:=LIST_FIRST(@FWaits);

  while (node<>nil) do
  begin
   if (node^.tid=tid) or (tid=iBROKEN) then
   begin
    node^.value:=value.Copy;

    ev_signal(node^.event);

    Break;
   end;

   node:=LIST_NEXT(node,@node^.entry);
  end;

 mtx_unlock(FWLock);
end;

procedure THostIpcConnect.Send(mtype:DWORD;key:Pointer;value:TIpcValue);
var
 node:PNodeIpcSync absolute key;
begin
 if (key=nil) then
 begin
  SendImpl(mtype,0,value);
 end else
 begin
  SendImpl(mtype,node^.tid,value);
 end;
end;

//

function THostIpcConnect.NewSyncKey:Pointer;
begin
 Result:=NewNodeSync;
end;

procedure THostIpcConnect.FreeSyncKey(key:Pointer);
var
 node:PNodeIpcSync absolute key;
begin
 if (node<>nil) then
 begin
  FreeNodeSync(node);
 end;
end;

procedure THostIpcConnect.WaitSyncKey(key:Pointer);
var
 node:PNodeIpcSync absolute key;
begin
 if FTerm or FBroke then Exit;

 if (node<>nil) then
 begin
  ev_wait(node^.event);
 end;
end;

function THostIpcConnect.GetSyncValue(key:Pointer):TIpcValue;
var
 node:PNodeIpcSync absolute key;
begin
 if FBroke then
 begin
  Result:=-1;
 end else
 if (node<>nil) then
 begin
  Result:=node^.value;
 end else
 begin
  Result:=Default(TIpcValue);
 end;
end;


//

procedure THostIpcConnect.SendImpl(mtype,mtid:DWORD;value:TIpcValue);
begin
 value.Free;
end;

//

procedure THostIpcDispatchGui.QueueSend(Client:THostIpcConnect;mtype,mtid:DWORD;value:TIpcValue);
begin
 //queued, but executed in the main thread along with the GUI
 inherited;
 //
 if Assigned(Classes.WakeMainThread) then
 begin
  Classes.WakeMainThread(nil);
 end;
end;

procedure THostIpcDispatchGui.QueuePush(node:PQNode);
begin
 //queued, but executed in the main thread along with the GUI
 inherited;
 //
 if Assigned(Classes.WakeMainThread) then
 begin
  Classes.WakeMainThread(nil);
 end;
end;

//

procedure THostIpcDispatchKern.QueueSend(Client:THostIpcConnect;mtype,mtid:DWORD;value:TIpcValue);
begin
 //queued, but executed in the kern thread
 inherited;
 //
 ev_signal(FEvent);
end;

procedure THostIpcDispatchKern.QueuePush(node:PQNode);
begin
 //queued, but executed in the kern thread
 inherited;
 //
 ev_signal(FEvent);
end;

procedure simple_kern_thread(parameter:pointer); SysV_ABI_CDecl;
var
 ipc:THostIpcDispatchKern;
begin
 Writeln('[simple_kern_thread]');

 ipc:=THostIpcDispatchKern(parameter);

 repeat
  if ipc.FQueue.IsEmpty then
  begin
   ev_wait(ipc.FEvent);
  end;
  ipc.Update();
 until ipc.FTerm;

end;

procedure THostIpcDispatchKern.thread_new;
begin
 if (Ftd=nil) then
 begin
  ev_init(FEvent,'THostIpcServerSimpleKern');
  kthread_add(@simple_kern_thread,Self,@Ftd,0,'[ipc_pipe]',TDP_KIGNSUSP);
 end;
end;

procedure THostIpcDispatchKern.thread_free;
begin
 if (Ftd<>nil) then
 begin
  FTerm:=True;
  ev_signal(FEvent);
  WaitForThreadTerminate(p_kthread(Ftd)^.td_handle,0);
  thread_dec_ref(Ftd);
  Ftd:=nil;
  ev_destroy(FEvent);
 end;
end;

//

procedure THostIpcSimple.SendImpl(mtype,mtid:DWORD;value:TIpcValue);
begin
 if (FDest<>nil) then
 if (FDest.Dispatcher<>nil) then
 begin
  FDest.Dispatcher.DoDispatch(FDest,mtype,mtid,value);
 end;
end;

//


end.

