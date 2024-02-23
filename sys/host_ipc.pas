unit host_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 time,
 mqueue,
 LFQueue,
 kern_thr,
 kern_thread,
 sys_event,
 kern_mtx;

type
 t_mtype=(
  iRESULT,
  iKEV_CHANGE,
  iKEV_EVENT,
  iMOUNT,
  iMAIN_WINDOWS,
  iCAPTION_FPS
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

 THostIpcHandler=class
  function OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint; virtual;
 end;

 THostIpcConnect=class
  protected
   FQueue:TIntrusiveMPSCQueue;
   FWaits:LIST_HEAD;
   FWLock:mtx;
   Fkq:Pointer;
   procedure   SyncResult(tid:DWORD;value:Ptruint);
   function    NewNodeSync:PNodeIpcSync;
   procedure   FreeNodeSync(node:PNodeIpcSync);
   procedure   TriggerNodeSync(tid:DWORD;value:Ptruint);
   procedure   Pack(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer);
   function    Recv:PQNode;
   procedure   Flush;
   procedure   RecvSync(node:PQNode);
   procedure   RecvKevent(node:PQNode);
   procedure   UpdateKevent();
   procedure   WakeupKevent(); virtual;
  public
   //
   procedure   kevent(kev:p_kevent;count:Integer);
   function    OpenMainWindows():THandle;
   procedure   SetCaptionFps(Ffps:QWORD);
   //
   function    SendSync(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
   procedure   SendAsyn(mtype:t_mtype;mlen:DWORD;buf:Pointer);
   procedure   Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer); virtual;
   procedure   Update(Handler:THostIpcHandler); virtual;
   //
   Constructor Create;
   Destructor  Destroy; override;
   procedure   thread_new;  virtual;
   procedure   thread_free; virtual;
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
  procedure   thread_new; override;
  procedure   thread_free; override;
  procedure   Send(mtype:t_mtype;mlen,mtid:DWORD;buf:Pointer); override;
  procedure   WakeupKevent(); override;
 end;

implementation

function THostIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
begin
 Result:=0;
end;

Constructor THostIpcConnect.Create;
begin
 FQueue.Create;
 LIST_INIT(@FWaits);
 mtx_init(FWLock,'ipc');
end;

Destructor THostIpcConnect.Destroy;
begin
 Flush;
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

procedure THostIpcConnect.Flush;
var
 node:PQNode;
begin
 node:=nil;
 while FQueue.Pop(node) do
 begin
  FreeMem(node);
 end;
end;

procedure THostIpcConnect.RecvSync(node:PQNode);
var
 value:Ptruint;
 mlen:DWORD;
begin
 value:=0;

 mlen:=node^.header.mlen;
 if (mlen>SizeOf(Ptruint)) then
 begin
  mlen:=SizeOf(Ptruint);
 end;

 Move(node^.buf,value,mlen);

 TriggerNodeSync(node^.header.mtid,value);
end;

procedure kq_wakeup(data:Pointer); SysV_ABI_CDecl;
begin
 THostIpcConnect(data).WakeupKevent();
end;

procedure THostIpcConnect.RecvKevent(node:PQNode);
var
 kev:p_kevent;
 value:Ptruint;
 count:Integer;
begin
 kev  :=@node^.buf;
 count:=node^.header.mlen div SizeOf(t_kevent);

 if (Fkq=nil) then
 begin
  Fkq:=kern_kqueue2('[ipc]',@kq_wakeup,Pointer(Self));
 end;

 //changelist
 value:=kern_kevent2(Fkq,kev,count,nil,0,nil,@count);

 //is sync
 if (node^.header.mtid<>0) then
 begin
  SyncResult(node^.header.mtid,value);
 end;

 UpdateKevent();
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
   SendAsyn(iKEV_EVENT,r*SizeOf(t_kevent),@kev);
  end;

 until (r<>8);
end;

procedure THostIpcConnect.WakeupKevent();
begin
 //
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

  case t_mtype(node^.header.mtype) of
   iRESULT    :RecvSync(node);
   iKEV_CHANGE:RecvKevent(node);

   else
     if (Handler<>nil) then
     begin
      value:=Handler.OnMessage(t_mtype(node^.header.mtype),node^.header.mlen,@node^.buf);

      //is sync
      if (node^.header.mtid<>0) then
      begin
       SyncResult(node^.header.mtid,value);
      end;

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
begin
 Send(iRESULT,SizeOf(Ptruint),tid,@value);
end;

procedure THostIpcConnect.kevent(kev:p_kevent;count:Integer);
begin
 SendAsyn(iKEV_CHANGE,count*SizeOf(t_kevent),kev);
end;

function THostIpcConnect.OpenMainWindows():THandle;
begin
 Result:=THandle(SendSync(iMAIN_WINDOWS,0,nil));
end;

procedure THostIpcConnect.SetCaptionFps(Ffps:QWORD);
begin
 SendAsyn(iCAPTION_FPS,SizeOf(Ffps),@Ffps);
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
  if ipc.FQueue.IsEmpty then
  begin
   RTLEventWaitFor(ipc.FEvent);
  end;
  ipc.Update(ipc.FHandler);
 until false;

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
  kthread_add(@simple_kern_thread,Self,@Ftd,'[ipc_pipe]');
 end;
end;

procedure THostIpcSimpleKERN.thread_free;
begin
 if (Ftd<>nil) then
 begin
  thread_dec_ref(Ftd);
  Ftd:=nil;
 end;
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

procedure THostIpcSimpleKERN.WakeupKevent();
begin
 UpdateKevent();
end;

end.

