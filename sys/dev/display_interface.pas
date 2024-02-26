unit display_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sys_event,
 md_time;

const
 EVENTID_FLIP     =$0006;
 EVENTID_VBLANK   =$0007;
 EVENTID_SETMODE  =$0051;
 EVENTID_POSITION =$0058;
 EVENTID_PREVBLANK=$0059;

type
 p_flip_status=^t_flip_status;
 t_flip_status=packed record
  flipArg        :QWORD;
  flipArg2       :QWORD;
  count          :QWORD;
  processTime    :QWORD;
  tsc            :QWORD;
  currentBuffer  :DWORD;
  flipPendingNum0:DWORD;
  gcQueueNum     :DWORD;
  flipPendingNum1:DWORD;
  submitTsc      :QWORD;
 end;

 p_resolution_status=^t_resolution_status;
 t_resolution_status=packed record
  width           :DWORD;
  heigth          :DWORD;
  paneWidth       :DWORD;
  paneHeight      :DWORD;
  refreshHz       :DWORD; //Single
  screenSizeInInch:DWORD; //Single
 end;

 p_register_buffer_attr=^t_register_buffer_attr;
 t_register_buffer_attr=packed record
  pixelFormat:DWORD;
  tilingMode :DWORD;
  pitchPixel :DWORD;
  width      :DWORD;
  height     :DWORD;
  f_0x20     :Byte;
  f_0x21     :Byte;
  options    :WORD;
 end;

 p_register_buffer=^t_register_buffer;
 t_register_buffer=packed record
  index :DWORD;   //buffer index [0..15]
  attrid:DWORD;   //attribute id [0..3]
  left  :Pointer; //buffer ptr
  right :Pointer; //Stereo ptr
 end;

 p_submit_flip=^t_submit_flip;
 t_submit_flip=packed record
  bufferIndex:DWORD;
  flipMode   :DWORD;
  flipArg    :QWORD;
  flipArg2   :QWORD;
 end;

 TDisplayHandle=class
  event_flip:p_knlist;
  last_status:t_flip_status;
  procedure knote_eventid            (event_id:WORD;flipArg:QWORD);
  function  Open                     ():Integer; virtual;
  function  GetFlipStatus            (status:p_flip_status):Integer; virtual;
  function  GetResolutionStatus      (status:p_resolution_status):Integer; virtual;
  function  SetFlipRate              (rate:Integer):Integer; virtual;
  function  RegisterBufferAttribute  (attrid:Byte;attr:p_register_buffer_attr):Integer; virtual;
  function  SubmitBufferAttribute    (attrid:Byte;attr:p_register_buffer_attr):Integer; virtual;
  function  UnregisterBufferAttribute(attrid:Byte):Integer; virtual;
  function  RegisterBuffer           (buf:p_register_buffer):Integer; virtual;
  function  UnregisterBuffer         (index:Integer):Integer; virtual;
  function  SubmitFlip               (submit:p_submit_flip):Integer; virtual;
  function  SubmitFlipEop            (submit:p_submit_flip;submit_id:QWORD):Integer; virtual;
 end;

 TAbstractDisplay=class of TDisplayHandle;

implementation

//

procedure TDisplayHandle.knote_eventid(event_id:WORD;flipArg:QWORD);
begin
 knote(event_flip, event_id or (flipArg shl 16), 0);
end;

function TDisplayHandle.Open():Integer;
begin
 last_status.currentBuffer:=DWORD(-1);
 last_status.flipArg      :=QWORD(-1);

 Result:=0;
end;

function TDisplayHandle.GetFlipStatus(status:p_flip_status):Integer;
begin
 status^:=last_status;

 Result:=0;
end;

function TDisplayHandle.GetResolutionStatus(status:p_resolution_status):Integer;
begin
 status^.width           :=1920;
 status^.heigth          :=1080;
 status^.paneWidth       :=1920;
 status^.paneHeight      :=1080;
 status^.refreshHz       :=$426fc28f; //( 59.94)
 status^.screenSizeInInch:=$42500000; //( 52.00)
 //
 Result:=0;
end;

function TDisplayHandle.SetFlipRate(rate:Integer):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.RegisterBufferAttribute(attrid:Byte;attr:p_register_buffer_attr):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.SubmitBufferAttribute(attrid:Byte;attr:p_register_buffer_attr):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.UnregisterBufferAttribute(attrid:Byte):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.RegisterBuffer(buf:p_register_buffer):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.UnregisterBuffer(index:Integer):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.SubmitFlip(submit:p_submit_flip):Integer;
begin
 last_status.flipArg      :=submit^.flipArg;
 last_status.flipArg2     :=submit^.flipArg2;
 last_status.count        :=last_status.count+1;
 last_status.submitTsc    :=rdtsc;
 last_status.currentBuffer:=submit^.bufferIndex;

 knote_eventid(EVENTID_FLIP, submit^.flipArg);

 last_status.tsc        :=rdtsc;
 last_status.processTime:=last_status.tsc;

 Result:=0;
end;


function TDisplayHandle.SubmitFlipEop(submit:p_submit_flip;submit_id:QWORD):Integer;
begin
 last_status.flipArg      :=submit^.flipArg;
 last_status.flipArg2     :=submit^.flipArg2;
 last_status.count        :=last_status.count+1;
 last_status.submitTsc    :=rdtsc;
 last_status.currentBuffer:=submit^.bufferIndex;

 knote_eventid(EVENTID_FLIP, submit^.flipArg);

 last_status.tsc        :=rdtsc;
 last_status.processTime:=last_status.tsc;

 Result:=0;
end;

end.

