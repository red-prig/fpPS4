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

 PHostIpcKnote=^THostIpcKnote;
 THostIpcKnote=packed record
  pid   :Integer;
  filter:Integer;
  hint  :QWORD
 end;

 PQNode=^TQNode;
 TQNode=packed record
  next_:PQNode;
  mtype:DWORD;
  mlen :DWORD;
  buf  :record end;
 end;

 THostIpcCb=procedure(node,data:Pointer); register;

 THostIpcCbRec=record
  cbs :THostIpcCb;
  data:Pointer;
 end;

 THostIpcConnect=class
  FQueue:TIntrusiveMPSCQueue;
  FCbReg:array[t_mtype] of THostIpcCbRec;
  //
  procedure   knote(pid,filter:Integer;hint:QWORD);
  //
  procedure   Send(mtype:t_mtype;mlen:DWORD;buf:Pointer); virtual;
  procedure   Pack(mtype:t_mtype;mlen:DWORD;buf:Pointer);
  function    Recv:PQNode;
  procedure   Update; virtual;
  //
  Constructor Create;
 end;

 THostIpcSimpleKERN=class;

 THostIpcSimpleGUI=class(THostIpcConnect)
  FDest:THostIpcSimpleKERN;
  procedure Send(mtype:t_mtype;mlen:DWORD;buf:Pointer); override;
 end;

 THostIpcSimpleKERN=class(THostIpcConnect)
  FDest:THostIpcSimpleGUI;
  FEvent:PRTLEvent;
  Constructor Create;
  Destructor  Destroy; override;
  procedure   Send(mtype:t_mtype;mlen:DWORD;buf:Pointer); override;
  procedure   Update; override;
 end;

implementation

Constructor THostIpcConnect.Create;
begin
 FQueue.Create;
end;

procedure THostIpcConnect.Pack(mtype:t_mtype;mlen:DWORD;buf:Pointer);
var
 node:PQNode;
begin
 node:=AllocMem(SizeOf(TQNode)+mlen);
 node^.mtype:=DWORD(mtype);
 node^.mlen :=mlen;
 Move(buf^,node^.buf,mlen);
end;

function THostIpcConnect.Recv:PQNode;
begin
 Result:=nil;
 FQueue.Pop(Result);
end;

procedure THostIpcConnect.Update;
var
 node:PQNode;
 rec:THostIpcCbRec;
begin
 node:=Recv;
 while (node<>nil) do
 begin
  rec:=FCbReg[t_mtype(node^.mtype)];
  if (rec.cbs<>nil) then
  begin
   rec.cbs(@node^.buf,rec.data);
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

procedure THostIpcSimpleGUI.Send(mtype:t_mtype;mlen:DWORD;buf:Pointer);
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

procedure THostIpcSimpleKERN.Update;
begin
 RTLEventWaitFor(FEvent);
 inherited;
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

