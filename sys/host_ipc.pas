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
 kern_mtx;

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
  buf   :record end;
 end;

 PNodeIpcSync=^TNodeIpcSync;
 TNodeIpcSync=packed record
  entry:LIST_ENTRY;
  event:PRTLEvent;
  value:Ptruint;
  tid  :DWORD;
 end;

 THostIpcConnect=class(THostIpcInterface)
  protected
   FQueue:TIntrusiveMPSCQueue;
   FWaits:LIST_HEAD;
   FWLock:mtx;
   Fkq   :Pointer;
   procedure   SyncResult(tid:DWORD;value:Ptruint);
   function    NewNodeSync:PNodeIpcSync;
   procedure   FreeNodeSync(node:PNodeIpcSync);
   procedure   TriggerNodeSync(tid:DWORD;value:Ptruint);
   procedure   Pack(mtype,mlen,mtid:DWORD;buf:Pointer);
   function    Recv:PQNode;
   procedure   Flush;
   procedure   RecvSync(node:PQNode);
   function    RecvKevent(mlen:DWORD;buf:Pointer):Ptruint;
   procedure   UpdateKevent();
   procedure   WakeupKevent(); virtual;
  public
   //
   function    SendSync(mtype,mlen:DWORD;buf:Pointer):Ptruint; override;
   procedure   SendAsyn(mtype,mlen:DWORD;buf:Pointer);         override;
   procedure   Send    (mtype,mlen,mtid:DWORD;buf:Pointer);    virtual;
   procedure   Update  ();                                     override;
   //
   Constructor Create;
   Destructor  Destroy;     override;
   procedure   thread_new;  virtual;
   procedure   thread_free; virtual;
 end;

 THostIpcSimpleKERN=class;

 THostIpcSimpleMGUI=class(THostIpcConnect)
  FDest:THostIpcSimpleKERN;
  procedure Send(mtype,mlen,mtid:DWORD;buf:Pointer); override;
 end;

 THostIpcSimpleKERN=class(THostIpcConnect)
  FDest     :THostIpcSimpleMGUI;
  FEvent    :PRTLEvent;
  FTerminate:Boolean;
  Constructor Create;
  Destructor  Destroy;     override;
  procedure   thread_new;  override;
  procedure   thread_free; override;
  procedure   Send(mtype,mlen,mtid:DWORD;buf:Pointer); override;
  Function    GetCallback(mtype:DWORD):TOnMessage;     override;
  procedure   WakeupKevent(); override;
 end;

//

 TGameProcess=class
  g_ipc  :THostIpcConnect;
  g_proc :THandle;
  g_p_pid:Integer;
  g_fork :Boolean;
  function   is_terminated:Boolean; virtual;
  function   exit_code:DWORD;       virtual;
  procedure  suspend; virtual;
  procedure  resume;  virtual;
  procedure  stop;    virtual;
  Destructor Destroy; override;
 end;

implementation

Constructor THostIpcConnect.Create;
begin
 inherited;
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

procedure THostIpcConnect.Pack(mtype,mlen,mtid:DWORD;buf:Pointer);
var
 node:PQNode;
begin
 node:=AllocMem(SizeOf(TQNode)+mlen);
 node^.header.mtype:=mtype;
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

function THostIpcConnect.RecvKevent(mlen:DWORD;buf:Pointer):Ptruint;
var
 kev:p_kevent;
 count:Integer;
begin
 kev  :=buf;
 count:=mlen div SizeOf(t_kevent);

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
   if (iKEV_EVENT=0) then iKEV_EVENT:=HashIpcStr('KEV_EVENT');
   SendAsyn(iKEV_EVENT,r*SizeOf(t_kevent),@kev);
  end;

 until (r<>8);
end;

procedure THostIpcConnect.WakeupKevent();
begin
 //
end;

procedure THostIpcConnect.Update();
var
 node :PQNode;
 value:Ptruint;
 OnMsg:TOnMessage;
begin
 if FStop then Exit;

 node:=Recv;

 while (node<>nil) do
 begin
  //

  if (node^.header.mtype=iRESULT) then
  begin
   RecvSync(node);
  end else
  begin
   OnMsg:=GetCallback(node^.header.mtype);
   if (OnMsg<>nil) then
   begin
    value:=OnMsg(node^.header.mlen,@node^.buf);
   end else
   begin
    //nop?
    value:=Ptruint(-1);
   end;
   //is sync
   if (node^.header.mtid<>0) then
   begin
    SyncResult(node^.header.mtid,value);
   end;
  end;

  //
  FreeMem(node); //RenderDoc -> ExceptionCode:0xC0000005
  //
  if FStop then Exit;
  //
  node:=Recv;
 end;
end;

//

procedure THostIpcConnect.SyncResult(tid:DWORD;value:Ptruint);
begin
 Send(iRESULT,SizeOf(Ptruint),tid,@value);
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

function THostIpcConnect.SendSync(mtype,mlen:DWORD;buf:Pointer):Ptruint;
var
 node:PNodeIpcSync;
begin
 node:=NewNodeSync;

 Send(mtype,mlen,node^.tid,buf);

 RTLEventWaitFor(node^.event);

 Result:=node^.value;

 FreeNodeSync(node);
end;

procedure THostIpcConnect.SendAsyn(mtype,mlen:DWORD;buf:Pointer);
begin
 Send(mtype,mlen,0,buf);
end;

procedure THostIpcConnect.Send(mtype,mlen,mtid:DWORD;buf:Pointer);
begin
 //
end;

procedure THostIpcSimpleMGUI.Send(mtype,mlen,mtid:DWORD;buf:Pointer);
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
 Writeln('[simple_kern_thread]');

 ipc:=THostIpcSimpleKERN(parameter);

 repeat
  if ipc.FQueue.IsEmpty then
  begin
   RTLEventWaitFor(ipc.FEvent);
  end;
  ipc.Update();
 until ipc.FTerminate;

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
  kthread_add(@simple_kern_thread,Self,@Ftd,0,'[ipc_pipe]');
 end;
end;

procedure THostIpcSimpleKERN.thread_free;
begin
 if (Ftd<>nil) then
 begin
  FTerminate:=True;
  RTLEventSetEvent(FEvent);
  WaitForThreadTerminate(p_kthread(Ftd)^.td_handle,0);
  thread_dec_ref(Ftd);
  Ftd:=nil;
 end;
end;

Function THostIpcSimpleKERN.GetCallback(mtype:DWORD):TOnMessage;
begin
 if (iKEV_CHANGE=0) then iKEV_CHANGE:=HashIpcStr('KEV_CHANGE');
 if (mtype=iKEV_CHANGE) then
 begin
  Result:=@RecvKevent;
 end else
 begin
  Result:=inherited;
 end;
end;

procedure THostIpcSimpleKERN.Send(mtype,mlen,mtid:DWORD;buf:Pointer);
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

//

function TGameProcess.is_terminated:Boolean;
begin
 Result:=False;
end;

function TGameProcess.exit_code:DWORD;
begin
 Result:=0;
end;

procedure TGameProcess.suspend;
begin
 //
end;

procedure TGameProcess.resume;
begin
 //
end;

procedure TGameProcess.stop;
begin
 //
end;

Destructor TGameProcess.Destroy;
begin
 FreeAndNil(g_ipc);
 inherited;
end;


end.

