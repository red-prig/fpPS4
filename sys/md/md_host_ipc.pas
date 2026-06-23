unit md_host_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 Classes,
 kern_mtx,
 evbuffer,
 evpoll,
 host_ipc_interface,
 host_ipc;

type
 t_push_cb=Function(Node:Pointer):Boolean of object;

 t_ipc_proto=object
  Fbev   :Pbufferevent;
  Finput :Pevbuffer;
  Foutput:Pevbuffer;

  FHeader:TNodeHeader;
  FState :Integer;

  procedure Send(mtype,mlen,mtid:DWORD;buf:Pointer);
  procedure Recv(FPush:t_push_cb);
 end;

 TGlobalEvpoll=class
  evpoll   :Tevpoll;
  td_handle:TThreadID;
  refs     :Integer;
  Constructor Create;
  Destructor  Destroy; override;
 end;

 THostIpcPipe=class(THostIpcConnect)
  proto:t_ipc_proto;
  attach_evpoll:Boolean;
  procedure   set_pipe(fd:THandle);
  procedure   Recv_pipe; virtual;
  Function    Push(Node:Pointer):Boolean;
  procedure   SendImpl(mtype,mtid:DWORD;value:TIpcValue); override;
  procedure   WakeupKevent(); override;
  Destructor  Destroy; override;
  procedure   thread_new;  override;
  procedure   thread_free; override;
 end;

 THostIpcPipeMGUI=class(THostIpcPipe)
  procedure   Recv_pipe;   override;
 end;

 THostIpcPipeKERN=class(THostIpcPipe)
  Function    GetCallback(mtype:DWORD):TOnMessage; override;
  procedure   Recv_pipe;   override;
 end;

implementation

var
 global_evpoll_mtx:mtx;
 global_evpoll    :TGlobalEvpoll;

Constructor TGlobalEvpoll.Create;
begin
 inherited;
 evpoll_init(@evpoll,nil);
end;

Destructor TGlobalEvpoll.Destroy;
begin
 evpoll_free(@evpoll);
 inherited;
end;

function pipe_thread(parameter:pointer):ptrint;
begin
 Result:=0;
 evpoll_loop(parameter);
end;

procedure THostIpcPipe.thread_new;
begin
 if attach_evpoll then Exit;

 mtx_lock(global_evpoll_mtx);

  if (global_evpoll=nil) then
  begin
   global_evpoll:=TGlobalEvpoll.Create;
  end;

  if (global_evpoll.refs=0) then
  begin
   global_evpoll.td_handle:=BeginThread(@pipe_thread,@global_evpoll.evpoll);
  end;

  Inc(global_evpoll.refs);

 mtx_unlock(global_evpoll_mtx);

 attach_evpoll:=True;
end;

procedure THostIpcPipe.thread_free;
begin
 if not attach_evpoll then Exit;

 mtx_lock(global_evpoll_mtx);

  Dec(global_evpoll.refs);

  if (global_evpoll.refs=0) then
  begin
   evpoll_break(@global_evpoll.evpoll);
   //
   WaitForThreadTerminate(global_evpoll.td_handle,0);
   CloseThread(global_evpoll.td_handle);
   //
   FreeAndNil(global_evpoll);
  end;

 mtx_unlock(global_evpoll_mtx);

 attach_evpoll:=False;
end;

procedure t_ipc_proto.Send(mtype,mlen,mtid:DWORD;buf:Pointer);
var
 node:PNodeHeader;
begin
 node:=AllocMem(SizeOf(TNodeHeader)+mlen);
 node^.mtype:=DWORD(mtype);
 node^.mlen :=mlen;
 node^.mtid :=mtid;
 Move(buf^,node^.buf,mlen);

 evbuffer_add_ref(Foutput,node,0,SizeOf(TNodeHeader)+mlen,Freemem_ptr);

 bufferevent_write(Fbev);
end;

procedure t_ipc_proto.Recv(FPush:t_push_cb);
label
 _next;
var
 node:PQNode;
begin
 repeat

  case FState of
   0:
     begin
      if (evbuffer_get_length(Finput)<SizeOf(TNodeHeader)) then Exit;

      evbuffer_remove(Finput,@FHeader,SizeOf(TNodeHeader));

      FState:=1;

      if (FHeader.mlen=0) then goto _next;
     end;
   1:
     begin
      if (evbuffer_get_length(Finput)<FHeader.mlen) then Exit;

      _next:

      node:=AllocMem(SizeOf(TQNode)+FHeader.mlen);
      node^.header:=FHeader;
      node^.value :=TIpcValue.Static(@node^.buf,FHeader.mlen);

      evbuffer_remove(Finput,node^.value.GetBuf,FHeader.mlen);

      FPush(node);

      FState:=0;
     end;

   else;
  end;

 until (evbuffer_get_length(Finput)=0);

end;

procedure pipe_kern_thread(parameter:pointer); SysV_ABI_CDecl;
begin
 evpoll_loop(parameter);
end;

Procedure eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer);
begin

 if ((events and (BEV_EVENT_ERROR or BEV_EVENT_EOF))<>0) then
 begin
  Exit;
 end;

 if (events and BEV_EVENT_READING)<>0 then
 begin
  THostIpcPipe(ctx).Recv_pipe;
  bufferevent_read(bev);
 end;

 if (events and BEV_EVENT_WRITING)<>0 then
 begin
  bufferevent_write(bev);
 end;

end;

procedure THostIpcPipe.set_pipe(fd:THandle);
begin
 thread_new;

 proto.Fbev   :=bufferevent_pipe_new  (@global_evpoll.evpoll,fd);
 proto.Finput :=bufferevent_get_input (proto.Fbev);
 proto.Foutput:=bufferevent_get_output(proto.Fbev);

 bufferevent_setcb(proto.Fbev,@eventcb,Pointer(Self));
 bufferevent_enable(proto.Fbev);
end;

procedure THostIpcPipe.Recv_pipe;
begin
 proto.Recv(@Self.Push);
end;

Function THostIpcPipe.Push(Node:Pointer):Boolean;
begin
 if (PQNode(Node)^.header.mtype=iRESULT) then
 begin
  //Trigger Direct
  TriggerNodeSync(PQNode(Node)^.header.mtid,PQNode(Node)^.value);
  FreeMem(Node);
 end else
 begin
  Result:=FQueue.Push(node);
 end;
end;

procedure THostIpcPipe.SendImpl(mtype,mtid:DWORD;value:TIpcValue);
begin
 proto.Send(mtype,value.GetLen,mtid,value.GetBuf);
 value.Free;
end;

Procedure ev_wakeup(param1:SizeUInt;param2:Pointer); register;
begin
 THostIpcPipe(param2).UpdateKevent();
end;

procedure THostIpcPipe.WakeupKevent();
begin
 evpoll_post(@global_evpoll.evpoll,@ev_wakeup,0,Pointer(Self));
end;

Destructor THostIpcPipe.Destroy;
begin
 bufferevent_free(proto.Fbev);
 thread_free;
 inherited;
end;

//

procedure THostIpcPipeMGUI.Recv_pipe;
begin
 inherited;
 //
 if Assigned(Classes.WakeMainThread) then
 begin
  Classes.WakeMainThread(nil);
 end;
end;

//

Function THostIpcPipeKERN.GetCallback(mtype:DWORD):TOnMessage;
begin
 if (mtype=iKEV_CHANGE.mtype) then
 begin
  Result:=@RecvKevent;
 end else
 begin
  Result:=inherited;
 end;
end;

procedure THostIpcPipeKERN.Recv_pipe;
begin
 inherited;
 Update();
end;

initialization
 mtx_init(global_evpoll_mtx,'global_evpool_mtx');

end.



