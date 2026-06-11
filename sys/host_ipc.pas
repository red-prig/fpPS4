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
  header:TNodeHeader;
  value :TIpcValue;
  buf   :record end;
 end;

 PNodeIpcSync=^TNodeIpcSync;
 TNodeIpcSync=packed record
  entry:LIST_ENTRY;
  event:t_event;
  value:TIpcValue;
  tid  :DWORD;
 end;

 TIpcValue        =host_ipc_interface.TIpcValue;
 TOnMessage       =host_ipc_interface.TOnMessage;
 THostIpcHandler  =host_ipc_interface.THostIpcHandler;
 THostIpcInterface=host_ipc_interface.THostIpcInterface;

 THostIpcConnect=class(THostIpcInterface)
  protected
   FQueue:TIntrusiveMPSCQueue;
   FWaits:LIST_HEAD;
   FWLock:mtx;
   Ftd   :Pointer; //p_kthread
   Fkq   :Pointer;
   FTerm :Boolean;
   procedure   SyncResult(tid:DWORD;value:TIpcValue);
   function    NewNodeSync:PNodeIpcSync;
   procedure   FreeNodeSync(node:PNodeIpcSync);
   procedure   TriggerNodeSync(tid:DWORD;value:TIpcValue);
   procedure   QueueSend(mtype,mtid:DWORD;value:TIpcValue);
   function    QueueRecv:PQNode;
   procedure   QueueFlush;
   function    RecvKevent  (Value:TIpcValue):TIpcValue;
   procedure   UpdateKevent();
   procedure   WakeupKevent(); virtual;
  public
   //
   function    NewSyncKey:Pointer;       override;
   procedure   FreeSyncKey(key:Pointer); override;
   procedure   WaitSyncKey(key:Pointer); override;
   function    GetSyncValue(key:Pointer):TIpcValue; override;
   //
   procedure   Send    (mtype:DWORD;key:Pointer;value:TIpcValue); override;
   procedure   SendImpl(mtype,mtid:DWORD;value:TIpcValue); virtual;
   procedure   Update();                                    override;
   procedure   Disconnect();                                override;
   //
   Constructor Create;
   Destructor  Destroy;     override;
   procedure   thread_new;  virtual;
   procedure   thread_free; virtual;
 end;

 THostIpcSimpleKERN=class;

 THostIpcSimpleMGUI=class(THostIpcConnect)
  FDest:THostIpcSimpleKERN;
  procedure SendImpl(mtype,mtid:DWORD;value:TIpcValue); override;
 end;

 THostIpcSimpleKERN=class(THostIpcConnect)
  FDest :THostIpcSimpleMGUI;
  FEvent:PRTLEvent;
  Constructor Create;
  Destructor  Destroy;     override;
  procedure   thread_new;  override;
  procedure   thread_free; override;
  procedure   SendImpl(mtype,mtid:DWORD;value:TIpcValue); override;
  Function    GetCallback(mtype:DWORD):TOnMessage;     override;
  procedure   WakeupKevent(); override;
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

Constructor THostIpcConnect.Create;
begin
 inherited;
 FQueue.Create;
 LIST_INIT(@FWaits);
 mtx_init(FWLock,'ipc');
end;

Destructor THostIpcConnect.Destroy;
begin
 QueueFlush;
 mtx_destroy(FWLock);
 if (Fkq<>nil) then
 begin
  kqueue_close2(Fkq);
 end;
 inherited;
end;

procedure THostIpcConnect.thread_new;
begin
 //
end;

procedure THostIpcConnect.thread_free;
begin
 //
end;

procedure THostIpcConnect.QueueSend(mtype,mtid:DWORD;value:TIpcValue);
var
 node:PQNode;
begin
 if (mtype=iRESULT) then
 begin
  //Trigger Direct!
  TriggerNodeSync(mtid,value);
 end else
 begin
  node:=AllocMem(SizeOf(TQNode));
  node^.header.mtype:=mtype;
  node^.header.mlen :=value.GetLen;
  node^.header.mtid :=mtid;
  //
  node^.value:=value.Copy;
  //
  FQueue.Push(node);
 end;
end;

function THostIpcConnect.QueueRecv:PQNode;
begin
 Result:=nil;
 FQueue.Pop(Result);
end;

procedure THostIpcConnect.QueueFlush;
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

procedure kq_wakeup(data:Pointer); SysV_ABI_CDecl;
begin
 THostIpcConnect(data).WakeupKevent();
end;

function THostIpcConnect.RecvKevent(Value:TIpcValue):TIpcValue;
var
 kev:p_kevent;
 count:Integer;
begin
 kev  :=Value.GetBuf;
 count:=Value.GetLen div SizeOf(t_kevent);

 if (Fkq=nil) then
 begin
  Fkq:=kern_kqueue2('[ipc]',@kq_wakeup,Pointer(Self));
 end;

 //changelist
 Result:=kern_kevent2(Fkq,kev,count,nil,0,nil,@count);
end;

procedure THostIpcConnect.UpdateKevent();
var
 kev:array[0..7] of t_kevent;
 t:timespec;
 r:Integer;
begin
 if (Fkq=nil) then Exit;
 t:=Default(timespec);

 repeat

  r:=0;
  kern_kevent2(Fkq,nil,0,@kev,8,@t,@r);

  if (r>0) then
  begin
   InvokeAsyn(iKEV_EVENT.mtype,@kev,r*SizeOf(t_kevent));
  end;

 until (r<>8);
end;

procedure THostIpcConnect.WakeupKevent();
begin
 //
end;

procedure THostIpcConnect.Update();
var
 node  :PQNode;
 input :TIpcValue;
 output:TIpcValue;
 OnMsg :TOnMessage;
begin
 if FTerm then Exit;

 node:=QueueRecv;

 while (node<>nil) do
 begin
  //

  input:=node^.value;

  if (node^.header.mtype=iRESULT) then
  begin
   TriggerNodeSync(node^.header.mtid,input);
   input:=Default(TIpcValue); //transfer owned
  end else
  begin
   OnMsg:=GetCallback(node^.header.mtype);
   if (OnMsg<>nil) then
   begin
    output:=OnMsg(input);
   end else
   begin
    //nop?
    output:=-1;
   end;
   //is sync
   if (node^.header.mtid<>0) then
   begin
    SyncResult(node^.header.mtid,output);
    output:=Default(TIpcValue); //transfer owned
   end;
  end;

  //
  FreeMem(node);
  input.Free;
  output.Free;
  //
  if FTerm then Exit;
  //
  node:=QueueRecv;
 end;
end;

procedure THostIpcConnect.Disconnect();
begin
 FTerm:=True;
end;

//

procedure THostIpcConnect.SyncResult(tid:DWORD;value:TIpcValue);
begin
 SendImpl(iRESULT,tid,value);
end;

//

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
  node:=LIST_FIRST(@FWaits);

  while (node<>nil) do
  begin
   if (node^.tid=tid) then
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
 if (node<>nil) then
 begin
  ev_wait(node^.event);
 end;
end;

function THostIpcConnect.GetSyncValue(key:Pointer):TIpcValue;
var
 node:PNodeIpcSync absolute key;
begin
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
 //
end;

procedure THostIpcSimpleMGUI.SendImpl(mtype,mtid:DWORD;value:TIpcValue);
begin
 if (FDest<>nil) then
 begin
  FDest.QueueSend(mtype,mtid,value);
  //
  RTLEventSetEvent(FDest.FEvent);
  //
 end;
end;

procedure simple_kern_thread(parameter:pointer); SysV_ABI_CDecl;
var
 ipc:THostIpcSimpleKERN;
begin
 Writeln('[simple_kern_thread]');

 ipc:=THostIpcSimpleKERN(parameter);

 repeat
  if ipc.FQueue.IsEmpty then
  begin
   RTLEventWaitFor(ipc.FEvent);
  end;
  ipc.Update();
 until ipc.FTerm;

end;

Constructor THostIpcSimpleKERN.Create;
begin
 inherited;
 FEvent:=RTLEventCreate;
end;

Destructor THostIpcSimpleKERN.Destroy;
begin
 thread_free;
 RTLEventDestroy(FEvent);
 inherited;
end;

procedure THostIpcSimpleKERN.thread_new;
begin
 if (Ftd=nil) then
 begin
  kthread_add(@simple_kern_thread,Self,@Ftd,0,'[ipc_pipe]',TDP_KIGNSUSP);
 end;
end;

procedure THostIpcSimpleKERN.thread_free;
begin
 if (Ftd<>nil) then
 begin
  FTerm:=True;
  RTLEventSetEvent(FEvent);
  WaitForThreadTerminate(p_kthread(Ftd)^.td_handle,0);
  thread_dec_ref(Ftd);
  Ftd:=nil;
 end;
end;

Function THostIpcSimpleKERN.GetCallback(mtype:DWORD):TOnMessage;
begin
 if (mtype=iKEV_CHANGE.mtype) then
 begin
  Result:=@RecvKevent;
 end else
 begin
  Result:=inherited;
 end;
end;

procedure THostIpcSimpleKERN.SendImpl(mtype,mtid:DWORD;value:TIpcValue);
begin
 if (FDest<>nil) then
 begin
  FDest.QueueSend(mtype,mtid,value);
  //
  if Assigned(Classes.WakeMainThread) then
  begin
   Classes.WakeMainThread(nil);
  end;
  //
 end;
end;

procedure THostIpcSimpleKERN.WakeupKevent();
begin
 UpdateKevent();
end;

//


end.

