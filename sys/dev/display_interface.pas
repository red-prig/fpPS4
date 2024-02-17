unit display_interface;

{$mode ObjFPC}{$H+}

interface

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
  screenSizeInInch:DWORD;
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
  function GetFlipStatus          (status:p_flip_status):Integer; virtual;
  function GetResolutionStatus    (status:p_resolution_status):Integer; virtual;
  function SetFlipRate            (rate:Integer):Integer; virtual;
  function RegisterBufferAttribute(attrid:Byte;attr:p_register_buffer_attr):Integer; virtual;
  function SubmitBufferAttribute  (attrid:Byte;attr:p_register_buffer_attr):Integer; virtual;
  function RegisterBuffer         (buf:p_register_buffer):Integer; virtual;
  function UnregisterBuffer       (index:Integer):Integer; virtual;
  function SubmitFlip             (submit:p_submit_flip):Integer; virtual;
  function SubmitFlipEop          (submit:p_submit_flip;submit_id:QWORD):Integer; virtual;
 end;

 TDisplayInterface=class
  class Function Open:TDisplayHandle; virtual;
 end;

 TAbstractDisplay=class of TDisplayInterface;

implementation

class Function TDisplayInterface.Open:TDisplayHandle;
begin
 Result:=TDisplayHandle.Create;
end;

//

function TDisplayHandle.GetFlipStatus(status:p_flip_status):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.GetResolutionStatus(status:p_resolution_status):Integer;
begin
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
 Result:=0;
end;

function TDisplayHandle.SubmitFlipEop(submit:p_submit_flip;submit_id:QWORD):Integer;
begin
 Result:=0;
end;

end.

