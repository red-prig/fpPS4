unit display_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sys_event,
 kern_mtx,
 time;

const
 //SceVideoOutPixelFormat
 SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB = $80000000; // MSB first. Blue is at LSB
 SCE_VIDEO_OUT_PIXEL_FORMAT_B8_G8_R8_A8_SRGB = SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB;  // alias name in a gnm-friendly order (LSB first)

 SCE_VIDEO_OUT_PIXEL_FORMAT_A16R16G16B16_FLOAT = $C1060000; // MSB first. Blue is at LSB
 SCE_VIDEO_OUT_PIXEL_FORMAT_B16_G16_R16_A16_FLOAT = SCE_VIDEO_OUT_PIXEL_FORMAT_A16R16G16B16_FLOAT;  // alias name in a gnm-friendly order (LSB first)

 SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB = $80002200; // MSB first. Red is at LSB
 SCE_VIDEO_OUT_PIXEL_FORMAT_R8_G8_B8_A8_SRGB = SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB;  // alias name in a gnm-friendly order (LSB first)

 SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10 = $88060000; // MSB first. Blue is at LSB
 SCE_VIDEO_OUT_PIXEL_FORMAT_B10_G10_R10_A2 = SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10;  // alias name in a gnm-friendly order (LSB first)

 SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_SRGB = $88000000; // MSB first. Blue is at LSB
 SCE_VIDEO_OUT_PIXEL_FORMAT_B10_G10_R10_A2_SRGB = SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_SRGB;  // alias name in a gnm-friendly order (LSB first)

 SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_BT2020_PQ = $88740000; // MSB first. Blue is at LSB
 SCE_VIDEO_OUT_PIXEL_FORMAT_B10_G10_R10_A2_BT2020_PQ = SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_BT2020_PQ;  // alias name in a gnm-friendly order (LSB first)

 //SceVideoOutFlipMode
 SCE_VIDEO_OUT_FLIP_MODE_VSYNC         = 1; // on real video out vsync
 SCE_VIDEO_OUT_FLIP_MODE_HSYNC         = 2; // ASAP (but not immediate)
 SCE_VIDEO_OUT_FLIP_MODE_WINDOW        = 3; // similar to vsync but may flip on some windows at the top and the bottom of the display. N/A on Neo mode
 SCE_VIDEO_OUT_FLIP_MODE_VSYNC_MULTI   = 4; // vsync mode but allows multiple flips per vsync. flipRate is not valid. N/A on Neo mode
 SCE_VIDEO_OUT_FLIP_MODE_VSYNC_MULTI_2 = 5; // vsync mode but allows multiple flips per vsync. flipRate is valid
 SCE_VIDEO_OUT_FLIP_MODE_WINDOW_2      = 6; // Window mode but the top margin is less accurate than _MODE_WINDOW. The bottom margin must be 0.

 //SceVideoOutTilingMode
 SCE_VIDEO_OUT_TILING_MODE_TILE   = 0; // 32bpp pixel format only if on Neo mode
 SCE_VIDEO_OUT_TILING_MODE_LINEAR = 1; // 32bpp pixel format only

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
  bufferIndex:Integer;
  flipMode   :DWORD;
  flipArg    :QWORD;
  flipArg2   :QWORD;
 end;

 TDisplayHandle=class
  event_flip:p_knlist;
  mtx:p_mtx;
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
  function  TriggerFlipEop           (submit_id:QWORD):Integer; virtual;
  function  Vblank                   ():Integer; virtual;
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
 last_status.submitTsc    :=rdtsc();
 last_status.currentBuffer:=submit^.bufferIndex;

 knote_eventid(EVENTID_FLIP, submit^.flipArg);

 last_status.tsc        :=rdtsc();
 last_status.processTime:=last_status.tsc;

 Result:=0;
end;

function TDisplayHandle.SubmitFlipEop(submit:p_submit_flip;submit_id:QWORD):Integer;
begin
 last_status.flipArg      :=submit^.flipArg;
 last_status.flipArg2     :=submit^.flipArg2;
 last_status.count        :=last_status.count+1;
 last_status.submitTsc    :=rdtsc();
 last_status.currentBuffer:=submit^.bufferIndex;

 knote_eventid(EVENTID_FLIP, submit^.flipArg);

 last_status.tsc        :=rdtsc();
 last_status.processTime:=last_status.tsc;

 Result:=0;
end;

function TDisplayHandle.TriggerFlipEop(submit_id:QWORD):Integer;
begin
 Result:=0;
end;

function TDisplayHandle.Vblank():Integer;
begin
 Result:=0;
end;

end.

