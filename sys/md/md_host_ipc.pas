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
 t_ipc_proto=object
  type
   PQNode     =THostIpcDispatchQueue.PQNode;
   TQNode     =THostIpcDispatchQueue.TQNode;
   PNodeHeader=THostIpcDispatchQueue.PNodeHeader;
   TNodeHeader=THostIpcDispatchQueue.TNodeHeader;
   t_push_cb  =procedure(Node:PQNode) of object;
  var
   Fbev   :Pbufferevent;
   Finput :Pevbuffer;
   Foutput:Pevbuffer;

   FHeader:TNodeHeader;
   FState :Integer;

  procedure Send(mtype,mlen,mtid:DWORD;buf:Pointer);
  procedure Recv(Client:THostIpcConnect;Dispatch:THostIpcDispatchQueue);
 end;

 TGlobalEvpoll=class
  evpoll   :Tevpoll;
  td_handle:TThreadID;
  Constructor Create;
  Destructor  Destroy; override;
 end;

 THostIpcPipe=class(THostIpcConnect)
  evpoll:TGlobalEvpoll;
  proto :t_ipc_proto;
  procedure   set_pipe(fd:THandle);
  procedure   Recv_pipe;
  procedure   SendImpl(mtype,mtid:DWORD;value:TIpcValue); override;
  Destructor  Destroy; override;
 end;

implementation

var
 global_evpoll_mtx:mtx;
 global_evpoll    :TGlobalEvpoll;

function fetch_global_server:TGlobalEvpoll;
begin
 mtx_lock(global_evpoll_mtx);

  if (global_evpoll=nil) then
  begin
   global_evpoll:=TGlobalEvpoll.Create;
  end;

  Result:=global_evpoll;

 mtx_unlock(global_evpoll_mtx);
end;

function pipe_thread(parameter:pointer):ptrint;
begin
 Result:=0;
 evpoll_loop(parameter);
end;

Constructor TGlobalEvpoll.Create;
begin
 inherited;
 if (td_handle=0) then
 begin
  evpoll_init(@evpoll,nil);
  //
  td_handle:=BeginThread(@pipe_thread,@evpoll);
 end;
end;

Destructor TGlobalEvpoll.Destroy;
begin
 if (td_handle<>0) then
 begin
  evpoll_break(@evpoll);
  //
  WaitForThreadTerminate(td_handle,0);
  CloseThread(td_handle);
  //
  evpoll_free(@evpoll);
 end;
 inherited;
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

procedure t_ipc_proto.Recv(Client:THostIpcConnect;Dispatch:THostIpcDispatchQueue);
label
 _next;
var
 node:PQNode;
 Fimm:Ptruint;
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

      //alloc optimization
      if (FHeader.mlen<=SizeOf(Fimm)) then
      begin
       Fimm:=0;
       evbuffer_remove(Finput,@Fimm,FHeader.mlen);

       if (FHeader.mtype=iRESULT) then
       begin
        //Trigger Direct!
        Client.TriggerNodeSync(FHeader.mtid,TIpcValue.Static(@Fimm,FHeader.mlen));
       end else
       begin
        node:=AllocMem(SizeOf(TQNode));
        node^.Client:=Client;
        node^.header:=FHeader;
        node^.value :=TIpcValue.Static(@Fimm,FHeader.mlen);

        Dispatch.QueuePush(node);
       end;

      end else
      begin
       node:=AllocMem(SizeOf(TQNode)+FHeader.mlen);
       node^.Client:=Client;
       node^.header:=FHeader;
       node^.value :=TIpcValue.Static(@node^.buf,FHeader.mlen);

       evbuffer_remove(Finput,node^.value.GetBuf,FHeader.mlen);

       Dispatch.QueuePush(node);
      end;

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
 if (evpoll=nil) then
 begin
  evpoll:=fetch_global_server;
 end;

 proto.Fbev   :=bufferevent_pipe_new  (@evpoll.evpoll,fd);
 proto.Finput :=bufferevent_get_input (proto.Fbev);
 proto.Foutput:=bufferevent_get_output(proto.Fbev);

 bufferevent_setcb (proto.Fbev,@eventcb,Pointer(Self));
 bufferevent_enable(proto.Fbev);
end;

procedure THostIpcPipe.Recv_pipe;
begin
 proto.Recv(Self,THostIpcDispatchQueue(Dispatcher));
end;

procedure THostIpcPipe.SendImpl(mtype,mtid:DWORD;value:TIpcValue);
begin
 proto.Send(mtype,value.GetLen,mtid,value.GetBuf);
 value.Free;
end;

Destructor THostIpcPipe.Destroy;
begin
 bufferevent_free(proto.Fbev);
 inherited;
end;
//

initialization
 mtx_init(global_evpoll_mtx,'global_evpool_mtx');

end.



