unit host_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 LFQueue;

type
 t_mtype=(
  iMOUNT,
  iMOUNT_RET,
  iKNOTE
 );

 PNodeHeader=^TNodeHeader;
 TNodeHeader=packed record
  mtype:DWORD;
  mlen :DWORD;
  buf  :record end;
 end;

 PQNode=^TQNode;
 TQNode=packed record
  next_ :PQNode;
  header:TNodeHeader;
  buf   :record end;
 end;

 PHostIpcKnote=^THostIpcKnote;
 THostIpcKnote=packed record
  pid   :Integer;
  filter:Integer;
  hint  :QWORD
 end;

 THostIpcHandler=class
  Procedure OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer); virtual;
 end;

 THostIpcConnect=class
  FQueue:TIntrusiveMPSCQueue;
  //
  procedure   knote(pid,filter:Integer;hint:QWORD);
  //
  procedure   Send(mtype:t_mtype;mlen:DWORD;buf:Pointer); virtual;
  procedure   Pack(mtype:t_mtype;mlen:DWORD;buf:Pointer);
  function    Recv:PQNode;
  procedure   Update(Handler:THostIpcHandler); virtual;
  //
  Constructor Create;
 end;

 THostIpcSimpleKERN=class;

 THostIpcSimpleMGUI=class(THostIpcConnect)
  FDest:THostIpcSimpleKERN;
  procedure Send(mtype:t_mtype;mlen:DWORD;buf:Pointer); override;
 end;

 THostIpcSimpleKERN=class(THostIpcConnect)
  FDest:THostIpcSimpleMGUI;
  FEvent:PRTLEvent;
  Constructor Create;
  Destructor  Destroy; override;
  procedure   Send(mtype:t_mtype;mlen:DWORD;buf:Pointer); override;
  procedure   Update(Handler:THostIpcHandler); override;
 end;

implementation

Procedure THostIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 //
end;

Constructor THostIpcConnect.Create;
begin
 FQueue.Create;
end;

procedure THostIpcConnect.Pack(mtype:t_mtype;mlen:DWORD;buf:Pointer);
var
 node:PQNode;
begin
 node:=AllocMem(SizeOf(TQNode)+mlen);
 node^.header.mtype:=DWORD(mtype);
 node^.header.mlen :=mlen;
 Move(buf^,node^.buf,mlen);
 //
 FQueue.Push(node);
end;

function THostIpcConnect.Recv:PQNode;
begin
 Result:=nil;
 FQueue.Pop(Result);
end;

procedure THostIpcConnect.Update(Handler:THostIpcHandler);
var
 node:PQNode;
begin
 node:=Recv;
 while (node<>nil) do
 begin
  //
  if (Handler<>nil) then
  begin
   Handler.OnMessage(t_mtype(node^.header.mtype),node^.header.mlen,@node^.buf);
  end;
  //
  FreeMem(node);
  //
  node:=Recv;
 end;
end;

//

procedure THostIpcConnect.knote(pid,filter:Integer;hint:QWORD);
var
 note:THostIpcKnote;
begin
 note.pid   :=pid;
 note.filter:=filter;
 note.hint  :=hint;
 //
 Send(iKNOTE,SizeOf(note),@note);
end;

//

procedure THostIpcConnect.Send(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 //
end;

procedure THostIpcSimpleMGUI.Send(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 if (FDest<>nil) then
 begin
  FDest.Pack(mtype,mlen,buf);
  //
  RTLEventSetEvent(FDest.FEvent);
  //
 end;
end;

Constructor THostIpcSimpleKERN.Create;
begin
 inherited;
 FEvent:=RTLEventCreate;
end;

Destructor THostIpcSimpleKERN.Destroy;
begin
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

procedure THostIpcSimpleKERN.Send(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 if (FDest<>nil) then
 begin
  FDest.Pack(mtype,mlen,buf);
  //
  if Assigned(Classes.WakeMainThread) then
  begin
   Classes.WakeMainThread(nil);
  end;
  //
 end;
end;

end.

