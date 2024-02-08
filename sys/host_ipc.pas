unit host_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 mqueue,
 LFQueue,
 kern_thr,
 kern_thread,
 kern_mtx;

type
 t_mtype=(
  iRESULT,
  iMOUNT,
  iKNOTE
 );

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
  buf   :record end;
 end;

 PNodeIpcSync=^TNodeIpcSync;
 TNodeIpcSync=packed record
  entry :LIST_ENTRY;
  event :PRTLEvent;
  value :Ptruint;
  tid   :DWORD;
 end;

 PHostIpcResult=^THostIpcResult;
 THostIpcResult=packed record
  value:Ptruint;
  tid  :DWORD;
 end;

 PHostIpcKnote=^THostIpcKnote;
 THostIpcKnote=packed record
  pid   :Integer;
  filter:Integer;
  hint  :QWORD
 end;

 THostIpcHandler=class
  function OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint; virtual;
 end;

 THostIpcConnect=class
  protected
   FQueue:TIntrusiveMPSCQueue;
   FWaits:LIST_HEAD;
   FWLock:mtx;
   procedure   SyncResult(tid:DWORD;value:Ptruint);
   function    NewNodeSync:PNodeIpcSync;
   procedure   FreeNodeSync(node:PNodeIpcSync);
   procedure   TriggerNodeSync(tid:DWORD;value:Ptruint);
   procedure   Pack(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer);
   function    Recv:PQNode;
   procedure   RecvSync(node:PQNode);
  public
   //
   procedure   knote(pid,filter:Integer;hint:QWORD);
   //
   function    SendSync(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
   procedure   SendAsyn(mtype:t_mtype;mlen:DWORD;buf:Pointer);
   procedure   Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer); virtual;
   procedure   Update(Handler:THostIpcHandler); virtual;
   //
   Constructor Create;
   Destructor  Destroy; override;
 end;

 THostIpcSimpleKERN=class;

 THostIpcSimpleMGUI=class(THostIpcConnect)
  FDest:THostIpcSimpleKERN;
  procedure Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer); override;
 end;

 THostIpcSimpleKERN=class(THostIpcConnect)
  FDest:THostIpcSimpleMGUI;
  FEvent:PRTLEvent;
  Ftd:p_kthread;
  FHandler:THostIpcHandler;
  Constructor Create;
  Destructor  Destroy; override;
  procedure   Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer); override;
  procedure   Update(Handler:THostIpcHandler); override;
 end;

implementation

function THostIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
begin
 //
end;

Constructor THostIpcConnect.Create;
begin
 FQueue.Create;
 LIST_INIT(@FWaits);
 mtx_init(FWLock,'ipc');
end;

Destructor THostIpcConnect.Destroy;
begin
 mtx_destroy(FWLock);
 inherited;
end;

procedure THostIpcConnect.Pack(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer);
var
 node:PQNode;
begin
 node:=AllocMem(SizeOf(TQNode)+mlen);
 node^.header.mtype:=DWORD(mtype);
 node^.header.mlen :=mlen;
 node^.header.mtid :=mtid;
 Move(buf^,node^.buf,mlen);
 //
 FQueue.Push(node);
end;

function THostIpcConnect.Recv:PQNode;
begin
 Result:=nil;
 FQueue.Pop(Result);
end;

procedure THostIpcConnect.RecvSync(node:PQNode);
var
 res:THostIpcResult;
 mlen:DWORD;
begin
 res:=Default(THostIpcResult);

 mlen:=node^.header.mlen;
 if (mlen>SizeOf(THostIpcResult)) then
 begin
  mlen:=SizeOf(THostIpcResult);
 end;

 Move(node^.buf,res,mlen);

 TriggerNodeSync(res.tid,res.value);
end;

procedure THostIpcConnect.Update(Handler:THostIpcHandler);
var
 node:PQNode;
 value:Ptruint;
begin
 node:=Recv;
 while (node<>nil) do
 begin
  //
  if (node^.header.mtype=DWORD(iRESULT)) then
  begin
   RecvSync(node);
  end else
  if (Handler<>nil) then
  begin
   value:=Handler.OnMessage(t_mtype(node^.header.mtype),node^.header.mlen,@node^.buf);

   //is sync
   if (node^.header.mtid<>0) then
   begin
    SyncResult(node^.header.mtid,value);
   end;

  end;
  //
  FreeMem(node);
  //
  node:=Recv;
 end;
end;

//

procedure THostIpcConnect.SyncResult(tid:DWORD;value:Ptruint);
var
 note:THostIpcResult;
begin
 note.value:=value;
 note.tid  :=tid;
 //
 Send(iRESULT,SizeOf(note),0,@note);
end;

procedure THostIpcConnect.knote(pid,filter:Integer;hint:QWORD);
var
 note:THostIpcKnote;
begin
 note.pid   :=pid;
 note.filter:=filter;
 note.hint  :=hint;
 //
 SendAsyn(iKNOTE,SizeOf(note),@note);
end;

//

function THostIpcConnect.NewNodeSync:PNodeIpcSync;
var
 node:PNodeIpcSync;
begin
 node:=AllocMem(SizeOf(TNodeIpcSync));
 node^.event:=RTLEventCreate;
 node^.tid  :=GetThreadID;

 RTLEventResetEvent(node^.event);

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

 RTLEventDestroy(node^.event);

 FreeMem(node);
end;

procedure THostIpcConnect.TriggerNodeSync(tid:DWORD;value:Ptruint);
var
 node:PNodeIpcSync;
begin
 mtx_lock(FWLock);
  node:=LIST_FIRST(@FWaits);

  while (node<>nil) do
  begin
   if (node^.tid=tid) then
   begin
    node^.value:=value;

    RTLEventSetEvent(node^.event);

    Break;
   end;

   node:=LIST_NEXT(node,@node^.entry);
  end;

 mtx_unlock(FWLock);
end;

function THostIpcConnect.SendSync(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
var
 node:PNodeIpcSync;
begin
 node:=NewNodeSync;

 Send(mtype,mlen,node^.tid,buf);

 RTLEventWaitFor(node^.event);

 Result:=node^.value;

 FreeNodeSync(node);
end;

procedure THostIpcConnect.SendAsyn(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 Send(mtype,mlen,0,buf);
end;

procedure THostIpcConnect.Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer);
begin
 //
end;

procedure THostIpcSimpleMGUI.Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer);
begin
 if (FDest<>nil) then
 begin
  FDest.Pack(mtype,mlen,mtid,buf);
  //
  RTLEventSetEvent(FDest.FEvent);
  //
 end;
end;

procedure simple_kern_thread(parameter:pointer); SysV_ABI_CDecl;
var
 ipc:THostIpcSimpleKERN;
begin
 ipc:=THostIpcSimpleKERN(parameter);

 repeat
  ipc.Update(ipc.FHandler);
 until false;

end;

Constructor THostIpcSimpleKERN.Create;
begin
 inherited;
 FEvent:=RTLEventCreate;
 Ftd:=nil;
 kthread_add(@simple_kern_thread,Self,@Ftd,'[ipc_pipe]');
end;

Destructor THostIpcSimpleKERN.Destroy;
begin
 thread_dec_ref(Ftd);
 RTLEventDestroy(FEvent);
 inherited;
end;

procedure THostIpcSimpleKERN.Update(Handler:THostIpcHandler);
begin
 if FQueue.IsEmpty then
 begin
  RTLEventWaitFor(FEvent);
 end;
 inherited Update(Handler);
end;

procedure THostIpcSimpleKERN.Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer);
begin
 if (FDest<>nil) then
 begin
  FDest.Pack(mtype,mlen,mtid,buf);
  //
  if Assigned(Classes.WakeMainThread) then
  begin
   Classes.WakeMainThread(nil);
  end;
  //
 end;
end;

end.

