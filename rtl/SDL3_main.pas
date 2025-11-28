unit SDL3_main;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 sys_crt_gui,
 LFQueue,
 SDL3;

type
 TFunc=Function(Data:Pointer):Pointer; register;

Function SDL3_SendSync(Func:TFunc;Data:Pointer):Pointer;

implementation

type
 PQNode=^TQNode;
 TQNode=record
  next_:PQNode;
  //
  Event:PRTLEvent;
  Func :TFunc;
  Data :Pointer;
 end;

var
 _SDL3_Init:Integer=0;
 SDL3_Event:PRTLEvent;
 SDL3_Queue:TIntrusiveMPSCQueue=(tail_:@SDL3_Queue.stub_;stub_:(next_:nil);head_:@SDL3_Queue.stub_);

function SDL3_thread(parameter:pointer):ptrint; register; forward;

procedure SDL3_Init_thread;
begin
 if (System.InterlockedExchange(_SDL3_Init,1)=0) then
 begin
  SDL3_Event:=RTLEventCreate;
  BeginThread(@SDL3_thread);
 end;
end;

Function SDL3_SendSync(Func:TFunc;Data:Pointer):Pointer;
var
 node:PQNode;
begin
 SDL3_Init_thread;

 node:=AllocMem(SizeOf(TQNode));
 if (node=nil) then Exit(nil);

 node^.Event:=RTLEventCreate;
 node^.Func :=Func;
 node^.Data :=Data;

 RTLEventResetEvent(node^.event);

 SDL3_Queue.Push(node);
 RTLEventSetEvent(SDL3_Event);

 RTLEventWaitFor(node^.event);

 Result:=node^.Data;

 RTLEventDestroy(node^.event);
 FreeMem(node);
end;

function SDL3_thread(parameter:pointer):ptrint; register;
var
 node :PQNode;
 event:TSDL_Event;
begin
 Result:=0;
 sys_crt_gui.sys_crt_init;

 repeat

  RTLEventWaitFor(SDL3_Event,200);

  while SDL3_Queue.Pop(node) do
  begin
   node^.Data:=node^.Func(node^.Data);
   RTLEventSetEvent(node^.Event);
  end;

  if SDL_PollEvent(@event) then
  begin
   //
  end;

 until false;

end;


end.

