unit md_host_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 kern_thr,
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

 THostIpcPipe=class(THostIpcConnect)
  evpoll:Tevpoll;
  proto :t_ipc_proto;
  procedure   set_pipe(fd:THandle);
  procedure   Recv_pipe; virtual;
  Function    Push(Node:Pointer):Boolean;
  procedure   SendImpl(mtype,mtid:DWORD;value:TIpcValue); override;
  procedure   WakeupKevent(); override;
  Constructor Create;
  Destructor  Destroy; override;
 end;

 THostIpcPipeMGUI=class(THostIpcPipe)
  Ftd_handle:TThreadID;
  procedure   Recv_pipe;   override;
  procedure   thread_new;  override;
  procedure   thread_free; override;
 end;

 THostIpcPipeKERN=class(THostIpcPipe)
  Function    GetCallback(mtype:DWORD):TOnMessage; override;
  procedure   Recv_pipe;   override;
  procedure   thread_new;  override;
  procedure   thread_free; override;
 end;

implementation

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

function pipe_gui_thread(parameter:pointer):ptrint;
begin
 Result:=0;
 evpoll_loop(parameter);
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
 proto.Fbev   :=bufferevent_pipe_new  (@evpoll,fd);
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
 evpoll_post(@evpoll,@ev_wakeup,0,Pointer(Self));
end;

Constructor THostIpcPipe.Create;
begin
 inherited;
 evpoll_init(@evpoll,nil);
 //thread_new;
end;

Destructor THostIpcPipe.Destroy;
begin
 evpoll_break(@evpoll);
 thread_free;
 bufferevent_free(proto.Fbev);
 evpoll_free(@evpoll);
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

procedure THostIpcPipeMGUI.thread_new;
begin
 if (Ftd_handle=0) then
 begin
  Ftd_handle:=BeginThread(@pipe_gui_thread,@evpoll);
 end;
end;

procedure THostIpcPipeMGUI.thread_free;
begin
 if (Ftd_handle<>0) then
 begin
  WaitForThreadTerminate(Ftd_handle,0);
  CloseThread(Ftd_handle);
  Ftd_handle:=0;
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

procedure THostIpcPipeKERN.thread_new;
begin
 if (Ftd=nil) then
 begin
  kthread_add(@pipe_kern_thread,@evpoll,@Ftd,0,'[ipc_pipe]',TDP_KIGNSUSP);
 end;
end;

procedure THostIpcPipeKERN.thread_free;
begin
 if (Ftd<>nil) then
 begin
  WaitForThreadTerminate(p_kthread(Ftd)^.td_handle,0);
  thread_dec_ref(Ftd);
  Ftd:=nil;
 end;
end;


end.



