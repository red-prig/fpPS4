unit ps4_libSceVideoOut;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  ExtCtrls,
  Interfaces,
  Forms,
  LMessages,
  LCLType,
  LCLIntf,

  LFQueue,
  ps4_program,
  sys_types,

  ps4_queue,
  ps4_handles,
  ps4_libkernel,

  vulkan,
  vDevice,
  vMemory;

type
 pSceVideoOutStereoBuffers=^SceVideoOutStereoBuffers;
 SceVideoOutStereoBuffers=packed record
  left :Pointer;
  right:Pointer;
 end;

 pSceVideoOutBufferAttribute=^sceVideoOutBufferAttribute;
 SceVideoOutBufferAttribute=packed record
  format     ,
  tmode      ,
  aspect     ,
  width      ,
  height     ,
  pixelPitch ,
  option:DWORD;
  reserved0:DWORD;
  reserved1:QWORD;
 end;

 PSceVideoOutResolutionStatus=^SceVideoOutResolutionStatus ;
 SceVideoOutResolutionStatus=packed record
  width       :DWORD;
  height      :DWORD;
  paneWidth   :DWORD;
  paneHeight  :DWORD;
  refreshRate :QWORD;
  screenSize  :Single;
  flags       :Word;
  reserved0   :Word;
  reserved1:array[0..2] of DWORD;
 end;

 //Color adjustment
 PSceVideoOutColorSettings=^SceVideoOutColorSettings;
 SceVideoOutColorSettings=packed record
  gamma:array[0..2] of Single;
  option:DWORD;
 end;

 PSceVideoOutFlipStatus=^SceVideoOutFlipStatus;
 SceVideoOutFlipStatus=packed record
  count           :QWORD;
  processTime     :QWORD;
  tsc             :QWORD;
  flipArg         :Int64;
  submitTsc       :QWORD;
  _reserved0      :QWORD;
  gcQueueNum      :Longint;
  flipPendingNum  :Longint;
  currentBuffer   :Longint;
  _reserved1      :DWORD;
 end;

 PSceVideoOutVblankStatus=^SceVideoOutVblankStatus;
 SceVideoOutVblankStatus=packed record
  count:QWORD;            // count of vblanks after sceVideoOutOpen()
  processTime:QWORD;      // processTime of the time of the latest vblank event
  tsc:QWORD;		  // Timestamp counter value when the latest vblank executed
  _reserved:QWORD;
  flags:Byte;             // SceVideoOutVblankStatusFlags
  pad1:array[0..6] of Byte;
 end;

 pSceVideoOutDeviceCapabilityInfo=^SceVideoOutDeviceCapabilityInfo;
 SceVideoOutDeviceCapabilityInfo=packed record
  capability:QWORD; //SceVideoOutDeviceCapability
 end;

const
 //SceVideoOutBufferAttributeOption
 SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_NONE = 0;
 SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_VR = 7;
 SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_STRICT_COLORIMETRY = (1 shl 3);

 SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_MASK = (1 shl 4) - 1;

 SCE_VIDEO_OUT_BUFFER_NUM_MAX = 16;
 SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_NUM_MAX = 4;
 SCE_VIDEO_OUT_BUFFER_FLIP_RATE_MAX = 2;

 SCE_VIDEO_OUT_BUFFER_INDEX_BLANK = -1;  // special buffer index to blank screen
 SCE_VIDEO_OUT_BUFFER_INITIAL_FLIP_ARG = -1;  // initial flipArg valu at sceVideoOutOpen()


 //SceVideoOutResolutionStatusFlags
 SCE_VIDEO_OUT_RESOLUTION_STATUS_FLAGS_OUTPUT_MASK = (1 shl 0);
 SCE_VIDEO_OUT_RESOLUTION_STATUS_FLAGS_OUTPUT_IN_USE = (0 shl 0);
 SCE_VIDEO_OUT_RESOLUTION_STATUS_FLAGS_OUTPUT_NOT_IN_USE = (1 shl 0);


 //SceVideoOutRefreshRate
 SCE_VIDEO_OUT_REFRESH_RATE_UNKNOWN = 0;
 SCE_VIDEO_OUT_REFRESH_RATE_23_98HZ = 1;
 SCE_VIDEO_OUT_REFRESH_RATE_50HZ    = 2;
 SCE_VIDEO_OUT_REFRESH_RATE_59_94HZ = 3;
 SCE_VIDEO_OUT_REFRESH_RATE_29_97HZ = 6;

 // for VR
 SCE_VIDEO_OUT_REFRESH_RATE_89_91HZ  = 35;
 SCE_VIDEO_OUT_REFRESH_RATE_119_88HZ = 13;

 SCE_VIDEO_OUT_REFRESH_RATE_ANY = $FFFFFFFFFFFFFFFF;

 //SceVideoOutBusType
 SCE_VIDEO_OUT_BUS_TYPE_MAIN = 0;


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

 //SceVideoOutAspectRatio
 SCE_VIDEO_OUT_ASPECT_RATIO_16_9 = 0;

 //SceVideoOutEventId
 SCE_VIDEO_OUT_EVENT_FLIP             = 0;
 SCE_VIDEO_OUT_EVENT_VBLANK           = 1;
 SCE_VIDEO_OUT_EVENT_PRE_VBLANK_START = 2;


 //#define SCE_KERNEL_EVFILT_TIMER    EVFILT_TIMER
 //#define SCE_KERNEL_EVFILT_READ     EVFILT_READ
 //#define SCE_KERNEL_EVFILT_WRITE    EVFILT_WRITE
 //#define SCE_KERNEL_EVFILT_USER     EVFILT_USER
 //#define SCE_KERNEL_EVFILT_FILE     EVFILT_VNODE
 //#define SCE_KERNEL_EVFILT_GNM      EVFILT_GRAPHICS_CORE
 //#define SCE_KERNEL_EVFILT_VIDEO_OUT      EVFILT_DISPLAY
 //#define SCE_KERNEL_EVFILT_HRTIMER  EVFILT_HRTIMER

 SCE_VIDEO_OUT_CURSOR_IMAGE_ADDRESS_PENDING=0; //Output status for cursor image buffer
 SCE_VIDEO_OUT_CURSOR_DISABLE_PENDING      =1; //Output reflection status for cursor disable processing

 SCE_VIDEO_OUT_CURSOR_NUM_MAX=2;

const
 SCE_ERROR_ERROR_FLAG=$80000000;
 //SCE_ERROR_MAKE_ERROR(fac, sts) (SCE_ERROR_ERROR_FLAG | ((fac) << 16) | (sts))
 SCE_GNM_ERROR_UNKNOWN=(SCE_ERROR_ERROR_FLAG or ($FF shl 16) or $EE);

const
 SCE_VIDEO_OUT_ERROR_INVALID_VALUE                    =-2144796671; // 0x80290001 */
 SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS                  =-2144796670; // 0x80290002 */
 SCE_VIDEO_OUT_ERROR_INVALID_PIXEL_FORMAT             =-2144796669; // 0x80290003 */
 SCE_VIDEO_OUT_ERROR_INVALID_PITCH                    =-2144796668; // 0x80290004 */
 SCE_VIDEO_OUT_ERROR_INVALID_RESOLUTION               =-2144796667; // 0x80290005 */
 SCE_VIDEO_OUT_ERROR_INVALID_FLIP_MODE                =-2144796666; // 0x80290006 */
 SCE_VIDEO_OUT_ERROR_INVALID_TILING_MODE              =-2144796665; // 0x80290007 */
 SCE_VIDEO_OUT_ERROR_INVALID_ASPECT_RATIO             =-2144796664; // 0x80290008 */
 SCE_VIDEO_OUT_ERROR_RESOURCE_BUSY                    =-2144796663; // 0x80290009 */
 SCE_VIDEO_OUT_ERROR_INVALID_INDEX                    =-2144796662; // 0x8029000A */
 SCE_VIDEO_OUT_ERROR_INVALID_HANDLE                   =-2144796661; // 0x8029000B */
 SCE_VIDEO_OUT_ERROR_INVALID_EVENT_QUEUE              =-2144796660; // 0x8029000C */
 SCE_VIDEO_OUT_ERROR_INVALID_EVENT                    =-2144796659; // 0x8029000D */
 SCE_VIDEO_OUT_ERROR_NO_EMPTY_SLOT                    =-2144796657; // 0x8029000F */
 SCE_VIDEO_OUT_ERROR_SLOT_OCCUPIED                    =-2144796656; // 0x80290010 */
 SCE_VIDEO_OUT_ERROR_FLIP_QUEUE_FULL                  =-2144796654; // 0x80290012 */
 SCE_VIDEO_OUT_ERROR_INVALID_MEMORY                   =-2144796653; // 0x80290013 */
 SCE_VIDEO_OUT_ERROR_MEMORY_NOT_PHYSICALLY_CONTIGUOUS =-2144796652; // 0x80290014 */
 SCE_VIDEO_OUT_ERROR_MEMORY_INVALID_ALIGNMENT         =-2144796651; // 0x80290015 */
 SCE_VIDEO_OUT_ERROR_UNSUPPORTED_OUTPUT_MODE          =-2144796650; // 0x80290016 */
 SCE_VIDEO_OUT_ERROR_OVERFLOW                         =-2144796649; // 0x80290017 */
 SCE_VIDEO_OUT_ERROR_NO_DEVICE                        =-2144796648; // 0x80290018 */
 SCE_VIDEO_OUT_ERROR_UNAVAILABLE_OUTPUT_MODE          =-2144796647; // 0x80290019 */
 SCE_VIDEO_OUT_ERROR_INVALID_OPTION                   =-2144796646; // 0x8029001A */
 SCE_VIDEO_OUT_ERROR_PORT_UNSUPPORTED_FUNCTION        =-2144796645; // 0x8029001B */
 SCE_VIDEO_OUT_ERROR_UNSUPPORTED_OPERATION            =-2144796644; // 0x8029001C */
 SCE_VIDEO_OUT_ERROR_FATAL                            =-2144796417; // 0x802900FF */
 SCE_VIDEO_OUT_ERROR_UNKNOWN                          =-2144796418; // 0x802900FE */
 SCE_VIDEO_OUT_ERROR_ENOMEM                           =-2144792564; // 0x8029100C */

type
 PPInt64=^PInt64;

function ps4_sceVideoOutGetBufferLabelAddress(hVideo:Integer;ptr:PPInt64):Integer; SysV_ABI_CDecl;

Procedure App_Run;

type
 PqcFlipInfo=^TqcFlipInfo;
 TqcFlipInfo=record
  hVideo:Integer;
  bufferIndex:Integer;
  flipMode:Integer;
  flipArg:Int64;
 end;

function _qc_sceVideoOutSubmitFlip(Flip:PqcFlipInfo):Integer;

var
 MainWindows:THandle;

implementation

uses
 vFlip,
 sys_signal,
 sys_kernel,
 sys_time,
 ps4_time,
 spinlock,
 hamt,
 param_sfo;

type
 PQNode=^TQNode;
 TOnParent=Procedure(node:PQNode) of object;
 TQNode=object
  private
   next_:PQNode;
  public
   lock:Pointer;
   Parent:TOnParent;
   wait:SizeUint;
   u:record
    Case Byte of
     0:(userID,
        busType,
        index:Integer);
     1:(bufferIndex,
        flipMode:Integer;
        flipArg:Int64;
        _type:Integer);
   end;
 end;

var
 FQueueVideoOutCount:PtrUint;
 FQueueVideoOut:TIntrusiveMPSCQueue;

Procedure Push2VideoOut(P:Pointer);
begin
 if FQueueVideoOut.Push(P) then
 begin
  System.InterLockedIncrement(Pointer(FQueueVideoOutCount));
  if Assigned(WakeMainThread) then
   WakeMainThread(nil);
 end;
end;

type
 TUserApp=class(TApplication)
  procedure OnIdleUpdate(Sender:TObject;var Done:Boolean);
  procedure OnTimer(Sender:TObject);
 end;

procedure TUserApp.OnIdleUpdate(Sender:TObject;var Done:Boolean);
var
 rc:PtrUInt;
 node:PQNode;
begin
 Done:=True;
 Node:=nil;
 rc:=FQueueVideoOutCount;
 While FQueueVideoOut.Pop(Node) do
 begin
  System.InterLockedDecrement(Pointer(FQueueVideoOutCount));
  if Assigned(Node^.Parent) then
  begin
   Node^.Parent(Node);
  end;
  if rc=0 then Break;
  Dec(rc);
  if rc=0 then Break;
 end;

end;

procedure TUserApp.OnTimer(Sender:TObject);
var
 Done:Boolean;
begin
 OnIdleUpdate(Self,Done);
end;

var
 Timer:TTimer;

Procedure App_Run;
begin
 Timer:=TTimer.Create(Application);
 Timer.Interval:=10;
 Timer.OnTimer:=@TUserApp(Application).OnTimer;
 Timer.Enabled:=true;

 Application.OnException:=nil;
 Application.CaptureExceptions:=False;
 Application.AddOnIdleHandler(@TUserApp(Application).OnIdleUpdate,False);
 Application.Scaled:=True;
 Application.Initialize;
 Application.Run;
end;

var
 FVideoOutMap:TIntegerHandles;

type
 TMyForm=class(TForm)
  public
   caption_format:RawByteString;
   procedure SetCaptionFPS(Ffps:QWORD);
   procedure CloseEvent(Sender:TObject;var CloseAction:TCloseAction);
   procedure WMEraseBkgnd(var Message:TLMEraseBkgnd); message LM_ERASEBKGND;
   procedure KeyEvent(Sender:TObject;var Key:Word;Shift:TShiftState);
 end;

 TVPos=packed record
  X,Y:DWORD;
 end;
 TVCursor=packed record
  lock:DWORD;
  enable:DWORD;
  Pos:TVPos;
  adr:Pointer;
  pending:DWORD;
 end;
 TVCursors=array[0..SCE_VIDEO_OUT_CURSOR_NUM_MAX-1] of TVCursor;

 TVideoOut=class(TClassHandle)

  FForm:TMyForm;
  TVBlank:TTimer;

  FGpuFlip:TvFlip;

  FNodePos,FNodesUses:PtrUInt;
  FNodes:array[0..15] of TQNode;

  FlipEvents:Thamt64locked;
  VblankEvents:Thamt64locked;

  Fgamma:TGamma;

  Ffps        :QWORD;
  Ftsc_prev   :QWORD;

  VblankStatus:record
   FprocessTime:QWORD;
   FTsc        :QWORD;
  end;

  FlipStatus:record
   Fcount_flips   :QWORD;   //Number of flips completed after opening the port
   FprocessTime   :QWORD;   //Process time upon completion of the last flip
   Ftsc_flips     :QWORD;   //System timestamp counter value when the last flip completed
   FsubmitTsc     :QWORD;   //Timestamp counter value when the last completed flip is requested
   FflipArg       :Int64;
   FgcQueueNum    :Longint; //Number of flips where execution is not yet complete for GPU commands issued when the flips were submitted using Gnm::submitAndFlipCommandBuffers() or Gnmx::GfxContext::submitAndFlip()
   FflipPendingNum:Longint; //Total number of submitted flips that are not completed yet (including gcQueueNum)
   FcurrentBuffer:Longint;
  end;

  FLabels:array[0..15] of Int64;

  //(MAIN port: 1 to 16, AUX port: 1 to 8)
  FBuffers:record
   lock:Pointer;
   addr:array[0..15] of SceVideoOutStereoBuffers;
   attr:array[0..15] of SceVideoOutBufferAttribute;
  end;

  FCursors:array[0..SCE_VIDEO_OUT_CURSOR_NUM_MAX-1] of TVCursor;

  FlipRate:Byte;

  function  alloc_node:PQNode;
  procedure free_node(n:PQNode);

  procedure OnVblank(Sender:TObject);
  procedure sceVideoOutOpen(node:PQNode);

  procedure post_event_flip(flipArg:Int64);
  procedure post_event_vblank;

  procedure sceVideoOutSubmitFlip(node:PQNode);

  Constructor Create;
  Destructor  Destroy; override;
 end;

procedure _on_free_kevent(data,userdata:Pointer);
begin
 if (data=nil) then Exit;
 _free_kevent_node(PKEventNode(data));
end;

Destructor TVideoOut.Destroy;
begin
 FreeAndNil(FGpuFlip);
 FreeAndNil(TVBlank);
 FreeAndNil(FForm);
 FlipEvents.LockWr;
 HAMT_clear64(@FlipEvents.hamt,@_on_free_kevent,nil);
 VblankEvents.LockWr;
 HAMT_clear64(@VblankEvents.hamt,@_on_free_kevent,nil);
 inherited;
end;

const
 pd_Width=1280;
 pd_Height=720;

 //pd_Width=1312;
 //pd_Height=738;

 //pd_Width=1440;
 //pd_Height=810;

 //pd_Width=1920;
 //pd_Height=1080;

function TVideoOut.alloc_node:PQNode;
var
 pos:PtrUInt;
 n:PQNode;
begin
 Result:=nil;
 repeat
  if (FNodesUses=Length(FNodes)) then Exit;
  pos:=FNodePos;
  n:=@FNodes[pos mod Length(FNodes)];
  if spin_trylock(n^.lock) then
  begin
   System.InterlockedIncrement(Pointer(FNodesUses));
   FNodePos:=pos+1;
   Acqure;
   Exit(n);
  end;
  SwYieldExecution;
 until false;
end;

procedure TVideoOut.free_node(n:PQNode);
begin
 if (self=nil) or (n=nil) then Exit;
 System.InterlockedDecrement(Pointer(FNodesUses));
 spin_unlock(n^.lock);
 Release;
end;

//

procedure TMyForm.SetCaptionFPS(Ffps:QWORD);
begin
 Caption:=Format(caption_format,[Ffps]);
end;

procedure TMyForm.CloseEvent(Sender:TObject;var CloseAction:TCloseAction);
begin
 //Halt;
 Application.Terminate;
end;

procedure TMyForm.WMEraseBkgnd(var Message:TLMEraseBkgnd);
begin
 Message.Result:=1;
end;

procedure TMyForm.KeyEvent(Sender:TObject;var Key:Word;Shift:TShiftState);
var
 style:ptrint;
begin
 if (Shift=[ssAlt]) and (Key=13) then //Alt+Enter
 begin
  LockRealizeBounds;

  //BorderStyle:=bsNone; is buggy
  style:=GetWindowLong(Handle,GWL_STYLE);

  if ((style and WS_POPUP)=0) then //Windows->Fullscreen
  begin
   style:=style and (not WS_BORDER);
   style:=style or WS_POPUP;
   SetWindowLong(Handle,GWL_STYLE,style);

   WindowState:=wsFullScreen;
  end else
  begin //Fullscreen->Windows
   style:=style and (not WS_POPUP);
   style:=style or WS_BORDER;
   SetWindowLong(Handle,GWL_STYLE,style);

   WindowState:=wsNormal;
  end;

  UnlockRealizeBounds;
 end;
end;

procedure TVideoOut.OnVblank(Sender:TObject);
begin
 post_event_vblank;
end;

function get_caption_format:RawByteString;
var
 TITLE,TITLE_ID,APP_VER:RawByteString;
begin
 TITLE   :=param_sfo.ParamSfoGetString('TITLE');
 TITLE_ID:=param_sfo.ParamSfoGetString('TITLE_ID');
 APP_VER :=param_sfo.ParamSfoGetString('APP_VER');

 Result:=Format('fpPS4 (%s) [%s-%s:%s]',[{$I tag.inc},TITLE,TITLE_ID,APP_VER])+' FPS:%d';
end;

procedure TVideoOut.sceVideoOutOpen(node:PQNode);
begin

 VblankStatus.FprocessTime:=ps4_sceKernelReadTsc;
 VblankStatus.FTsc        :=ps4_sceKernelGetProcessTime;

 Writeln('sceVideoOutOpen:',HexStr(Pointer(Self)));
 FForm:=TMyForm.CreateNew(nil);
 FForm.ShowInTaskBar:=stAlways;
 FForm.DoubleBuffered:=False;
 FForm.ParentDoubleBuffered:=False;
 FForm.FormStyle:=fsNormal;
 FForm.SetBounds(100, 100, pd_Width, pd_Height);
 FForm.caption_format:=get_caption_format;
 FForm.SetCaptionFPS(0);
 FForm.OnClose:=@FForm.CloseEvent;
 FForm.OnKeyDown:=@FForm.KeyEvent;

 Application.UpdateMainForm(FForm);
 FForm.Show;

 FGpuFlip:=TvFlip.Create(FForm.Handle);
 FGpuFlip.FNeoMode:=ps4_sceKernelIsNeoMode<>0;

 TVBlank:=TTimer.Create(FForm);
 TVBlank.Interval:=(1000 div 60); //59.94
 TVBlank.OnTimer:=@OnVblank;
 TVBlank.Enabled:=true;

 Case node^.u.busType of
  SCE_VIDEO_OUT_BUS_TYPE_MAIN:
   begin
    MainWindows:=FForm.Handle;
   end;
  else
 end;

 //data? nop
 free_node(node);
end;

Constructor TVideoOut.Create;
const
 Single1:Single=1;
begin
 FlipEvents.Init;
 VblankEvents.Init;

 FlipStatus.FflipArg:=SCE_VIDEO_OUT_BUFFER_INITIAL_FLIP_ARG;
 FlipStatus.FcurrentBuffer:=SCE_VIDEO_OUT_BUFFER_INDEX_BLANK;

 FillDWord(Fgamma,3,PDWORD(@Single1)^);
end;

function ps4_sceVideoOutOpen(userID,busType,index:Integer;param:Pointer):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
 node:PQNode;
begin
 _sig_lock;

 H:=TVideoOut.Create;
 node:=H.alloc_node;

 Result:=-1;
 if not FVideoOutMap.New(H,Result) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);

 //Writeln('>sceVideoOutOpen:',HexStr(Pointer(H)));

 node^.Parent:=@H.sceVideoOutOpen;

 node^.u.userID :=userID;
 node^.u.busType:=busType;
 node^.u.index  :=index;

 Push2VideoOut(node);

 H.Release;

 //Writeln('<sceVideoOutOpen:',userID,' ',busType);

 _sig_unlock;
end;

function ps4_sceVideoOutClose(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 _sig_lock;
 if not FVideoOutMap.Delete(handle) then Result:=SCE_VIDEO_OUT_ERROR_INVALID_HANDLE;
 _sig_unlock;
end;

procedure ps4_sceVideoOutSetBufferAttribute; assembler; nostackframe;
label
 _end;
asm
 test %rdi,%rdi
 je   _end
 mov %esi,    (%rdi) //format
 mov %edx, 0x4(%rdi) //tmode
 mov %ecx, 0x8(%rdi) //aspect
 mov %r8d, 0xC(%rdi) //width
 mov %r9d,0x10(%rdi) //height
 mov 0x08(%rsp),%eax //<pixelPitch
 mov %eax,0x14(%rdi) //>pixelPitch
 _end:
end;

//SceKernelEvent=packed record
// ident:PtrUint;   //SCE_VIDEO_OUT_EVENT_FLIP
// filter:SmallInt; //SCE_KERNEL_EVFILT_VIDEO_OUT
// flags:Word;      //0
// fflags:DWORD;    //0
// data:Ptrint;     //flipArg 48 bits
// udata:Pointer;   //udata
//end;

function ps4_sceVideoOutGetEventCount(ev:PSceKernelEvent):Integer; SysV_ABI_CDecl;
begin
 if (ev=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);
 if (ev^.filter<>SCE_KERNEL_EVFILT_VIDEO_OUT) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT);
 Result:=(ev^.data shr 12) and $F;
end;

function ps4_sceVideoOutGetEventData(ev:PSceKernelEvent;data:Pint64):Integer; SysV_ABI_CDecl;
var
 ret:int64;
begin
 if (ev=nil) or (data=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);
 if (ev^.filter<>SCE_KERNEL_EVFILT_VIDEO_OUT) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT);
 ret:=ev^.data shr 16;
 if (ev^.data>=0) or ((ev^.ident shr 48)<>$0006) then
 begin
  data^:=ret;
 end else
 begin
  data^:=ret-$1000000000000;
 end;
 Result:=0;
end;

function ps4_sceVideoOutGetEventId(ev:PSceKernelEvent):Integer; SysV_ABI_CDecl;
begin
 if (ev=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);
 if (ev^.filter<>SCE_KERNEL_EVFILT_VIDEO_OUT) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT);

 Case (ev^.ident shr 48) of
  $0006:Result:=0; //SCE_VIDEO_OUT_EVENT_FLIP
  $0007:Result:=1; //SCE_VIDEO_OUT_EVENT_VBLANK
  $0008:Result:=SCE_VIDEO_OUT_ERROR_INVALID_EVENT;
  $0051:Result:=8;
  $0058:Result:=12;
  else
      Result:=SCE_VIDEO_OUT_ERROR_INVALID_EVENT;
 end;

end;

function ps4_sceVideoOutAddFlipEvent(eq:SceKernelEqueue;hVideo:Integer;udata:Pointer):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
 P:PPointer;
 node:PKEventNode;
begin
 if (eq=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT_QUEUE);

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;

 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 _sig_lock;
 H.FlipEvents.LockWr;
 P:=HAMT_search64(@H.FlipEvents.hamt,QWORD(eq));
 if (P<>nil) then
 begin
  node:=P^;
  node^.ev.udata:=udata;
 end else
 begin
  node:=_alloc_kevent_node(eq,SizeOf(TKEventNode));
  if (node=nil) or (node=Pointer(1)) then
  begin
   H.FlipEvents.Unlock;
   H.Release;
   _sig_unlock;
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT_QUEUE);
  end;
  node^.ev.ident :=$0006 shl 48; //SCE_VIDEO_OUT_EVENT_FLIP;
  node^.ev.flags :=EV_CLEAR;
  node^.ev.filter:=SCE_KERNEL_EVFILT_VIDEO_OUT;
  node^.ev.udata :=udata;
  HAMT_insert64(@H.FlipEvents.hamt,QWORD(eq),node);
 end;
 H.FlipEvents.Unlock;

 Writeln('sceVideoOutAddFlipEvent:',HexStr(udata));
 Result:=0;

 H.Release;
 _sig_unlock;
end;

function ps4_sceVideoOutAddVblankEvent(eq:SceKernelEqueue;hVideo:Integer;udata:Pointer):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
 P:PPointer;
 node:PKEventNode;
begin
 if (eq=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT_QUEUE);

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;

 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 _sig_lock;
 H.VblankEvents.LockWr;
 P:=HAMT_search64(@H.VblankEvents.hamt,QWORD(eq));
 if (P<>nil) then
 begin
  node:=P^;
  node^.ev.udata:=udata;
 end else
 begin
  node:=_alloc_kevent_node(eq,SizeOf(TKEventNode));
  if (node=nil) or (node=Pointer(1)) then
  begin
   H.VblankEvents.Unlock;
   H.Release;
   _sig_unlock;
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_EVENT_QUEUE);
  end;
  node^.ev.ident :=$0007 shl 48; //SCE_VIDEO_OUT_EVENT_VBLANK;
  node^.ev.flags :=EV_CLEAR;
  node^.ev.filter:=SCE_KERNEL_EVFILT_VIDEO_OUT;
  node^.ev.udata :=udata;
  HAMT_insert64(@H.VblankEvents.hamt,QWORD(eq),node);
 end;
 H.VblankEvents.Unlock;

 Writeln('sceVideoOutAddVblankEvent:',HexStr(udata));
 Result:=0;

 H.Release;
 _sig_unlock;
end;

//

function __sceVideoOutRegisterBuffers(hVideo:Integer;
                                      index:Integer;
                                      addr:PPointer;
                                      num:Integer;
                                      attr:PSceVideoOutBufferAttribute
                                     ):Integer;
var
 H:TVideoOut;
 buf:TvPointer;
 i,s:Integer;
begin
 For i:=0 to num-1 do
  begin
   if not TryGetHostPointerByAddr(addr[i],buf) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_MEMORY);
  end;

 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 spin_lock(H.FBuffers.lock);

 s:=index+num-1;
 For i:=index to s do
  begin
   if (H.FBuffers.addr[i].left<>nil) then
   begin
    spin_unlock(H.FBuffers.lock);
    H.Release;
    Exit(SCE_VIDEO_OUT_ERROR_SLOT_OCCUPIED);
   end;
  end;

 For i:=index to s do
  begin
   H.FBuffers.addr[i].left :=addr[i-index];
   H.FBuffers.addr[i].right:=nil;
   H.FBuffers.attr[i]:=attr^;
  end;

 spin_unlock(H.FBuffers.lock);

 H.Release;

 Result:=0;
end;


function __sceVideoOutRegisterStereoBuffers(hVideo:Integer;
                                            index:Integer;
                                            buffers:pSceVideoOutStereoBuffers;
                                            bufferNum:Integer;
                                            attr:PSceVideoOutBufferAttribute
                                           ):Integer;
var
 H:TVideoOut;
 buf:TvPointer;
 i,s:Integer;
begin
 For i:=0 to bufferNum-1 do
  begin
   if not TryGetHostPointerByAddr(buffers[i].left ,buf) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_MEMORY);
   if not TryGetHostPointerByAddr(buffers[i].right,buf) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_MEMORY);
  end;

 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 spin_lock(H.FBuffers.lock);

 s:=index+bufferNum-1;
 For i:=index to s do
  begin
   if (H.FBuffers.addr[i].left<>nil) then
   begin
    spin_unlock(H.FBuffers.lock);
    H.Release;
    Exit(SCE_VIDEO_OUT_ERROR_SLOT_OCCUPIED);
   end;
  end;

 For i:=index to s do
  begin
   H.FBuffers.addr[i]:=buffers[i-index];
   H.FBuffers.attr[i]:=attr^;
  end;

 spin_unlock(H.FBuffers.lock);

 H.Release;

 Result:=0;
end;

function _sceVideoOutUnregisterBuffers(hVideo:Integer;index:Integer):Integer;
var
 H:TVideoOut;
begin
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 spin_lock(H.FBuffers.lock);

 H.FBuffers.addr[index]:=Default(SceVideoOutStereoBuffers);
 H.FBuffers.attr[index]:=Default(SceVideoOutBufferAttribute);

 spin_unlock(H.FBuffers.lock);

 H.Release;

 Result:=0;
end;

//

function ps4_sceVideoOutRegisterBuffers(hVideo:Integer;
                                        index:Integer;
                                        addr:PPointer;
                                        num:Integer;
                                        attr:PSceVideoOutBufferAttribute
                                       ):Integer; SysV_ABI_CDecl;
begin

 if (addr=nil) or (attr=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);

 Case index of
  0..15:;
  else
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 end;

 if (num<0) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);

 Case (index+num) of
  1..16:;
  else
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 end;

 case attr^.format of
  SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB        :;
  SCE_VIDEO_OUT_PIXEL_FORMAT_A16R16G16B16_FLOAT   :;
  SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB        :;
  SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10          :;
  SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_SRGB     :;
  SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_BT2020_PQ:;
  else
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_PIXEL_FORMAT);
 end;

 Case attr^.tmode of
  SCE_VIDEO_OUT_TILING_MODE_LINEAR:;
  SCE_VIDEO_OUT_TILING_MODE_TILE:;
  else
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_TILING_MODE);
 end;

 if (attr^.aspect<>SCE_VIDEO_OUT_ASPECT_RATIO_16_9) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ASPECT_RATIO);
 if (attr^.width=0) or (attr^.height=0)             then Exit(SCE_VIDEO_OUT_ERROR_INVALID_RESOLUTION);
 if (attr^.pixelPitch=0)                            then Exit(SCE_VIDEO_OUT_ERROR_INVALID_PITCH);

 {Case attr^.option of
  SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_NONE              :;
  SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_VR                :;
  SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_STRICT_COLORIMETRY:;
  else
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_OPTION);
 end;}

 _sig_lock;
 Result:=__sceVideoOutRegisterBuffers(hVideo,
                                      index,
                                      addr,
                                      num,
                                      attr);
 _sig_unlock;
end;

function ps4_sceVideoOutRegisterStereoBuffers(hVideo:Integer;
                                              index:Integer;
                                              buffers:pSceVideoOutStereoBuffers;
                                              bufferNum:Integer;
                                              attr:PSceVideoOutBufferAttribute
                                             ):Integer; SysV_ABI_CDecl;
begin

  if (buffers=nil) or (attr=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);

  Case index of
   0..15:;
   else
    Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
  end;

  if (bufferNum<0) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);

  Case (index+bufferNum) of
   1..16:;
   else
    Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
  end;

  case attr^.format of
   SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB        :;
   SCE_VIDEO_OUT_PIXEL_FORMAT_A16R16G16B16_FLOAT   :;
   SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB        :;
   SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10          :;
   SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_SRGB     :;
   SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_BT2020_PQ:;
   else
    Exit(SCE_VIDEO_OUT_ERROR_INVALID_PIXEL_FORMAT);
  end;

  Case attr^.tmode of
   SCE_VIDEO_OUT_TILING_MODE_LINEAR:;
   SCE_VIDEO_OUT_TILING_MODE_TILE:;
   else
    Exit(SCE_VIDEO_OUT_ERROR_INVALID_TILING_MODE);
  end;

  if (attr^.aspect<>SCE_VIDEO_OUT_ASPECT_RATIO_16_9) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ASPECT_RATIO);
  if (attr^.width=0) or (attr^.height=0)             then Exit(SCE_VIDEO_OUT_ERROR_INVALID_RESOLUTION);
  if (attr^.pixelPitch=0)                            then Exit(SCE_VIDEO_OUT_ERROR_INVALID_PITCH);

  {Case attr^.option of
   SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_NONE              :;
   SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_VR                :;
   SCE_VIDEO_OUT_BUFFER_ATTRIBUTE_OPTION_STRICT_COLORIMETRY:;
   else
    Exit(SCE_VIDEO_OUT_ERROR_INVALID_OPTION);
  end;}

  _sig_lock;
  Result:= __sceVideoOutRegisterStereoBuffers(hVideo,
                                              index,
                                              buffers,
                                              bufferNum,
                                              attr);
  _sig_unlock;
end;

function ps4_sceVideoOutUnregisterBuffers(hVideo:Integer;index:Integer):Integer; SysV_ABI_CDecl;
begin

 Case index of
  0..15:;
  else
   Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 end;

 _sig_lock;
 Result:=_sceVideoOutUnregisterBuffers(hVideo,index);
 _sig_unlock;
end;

function ps4_sceVideoOutColorSettingsSetGamma_(P:PSceVideoOutColorSettings;
                                               gamma:Single;
                                               sizeOfSettings:DWORD):Integer; SysV_ABI_CDecl;
const
 Single1:Single=1;
begin
 if (P=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);
 sizeOfSettings:=sizeOfSettings div SizeOf(Single);
 if (sizeOfSettings=0) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);
 if (gamma<0.1) then gamma:=0.1;
 if (gamma>2.0) then gamma:=2.0;
 FillDWord(P^,sizeOfSettings,PDWORD(@Single1)^);
 if (sizeOfSettings>3) then sizeOfSettings:=3;
 FillDWord(P^,sizeOfSettings,PDWORD(@gamma)^);
 //Writeln('SetGamma:',HexStr(P),' ',gamma);
 Result:=0;
end;

function ps4_sceVideoOutAdjustColor_(handle:Integer;
                                     pSettings:PSceVideoOutColorSettings;
                                     sizeOfSettings:DWORD):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 if (pSettings=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);
 sizeOfSettings:=sizeOfSettings div SizeOf(Single);
 if (sizeOfSettings=0) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);

 if (sizeOfSettings>3) then sizeOfSettings:=3;

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(handle));
 _sig_unlock;

 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 Move(pSettings^.gamma,H.Fgamma,sizeOfSettings*SizeOf(Single));
 if Assigned(H.FGpuFlip) then
 begin
  H.FGpuFlip.SetGamma(H.Fgamma);
 end;

 _sig_lock;
 H.Release;
 _sig_unlock;

 //Writeln('AdjustColor:',handle,' ',HexStr(pSettings),' ',sizeOfSettings);
 Result:=0;
end;

function ps4_sceVideoOutGetResolutionStatus(hVideo:Integer;status:PSceVideoOutResolutionStatus):Integer; SysV_ABI_CDecl;
const
 //pd_Width=1312;
 //pd_Height=738;

//pd_Width=1440;
//pd_Height=810;

pd_Width=1920;
pd_Height=1080;
begin
 if (status=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);

 Writeln('sceVideoOutGetResolutionStatus:',hVideo);
 status^:=Default(SceVideoOutResolutionStatus);
 status^.width       :=pd_Width;
 status^.height      :=pd_Height;
 status^.paneWidth   :=pd_Width;
 status^.paneHeight  :=pd_Height;
 status^.refreshRate :=SCE_VIDEO_OUT_REFRESH_RATE_59_94HZ;
 status^.screenSize  :=32; //screenSizeInInch
 status^.flags       :=SCE_VIDEO_OUT_RESOLUTION_STATUS_FLAGS_OUTPUT_IN_USE;
 Result:=0;
end;

function ps4_sceVideoOutSetFlipRate(hVideo:Integer;rate:Integer):Integer; SysV_ABI_CDecl;
Const
 rateTable:array[0..2] of Byte=(60, 30, 20);
var
 H:TVideoOut;
begin
 case rate of
  0..2:
    begin
     _sig_lock;
     H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
     _sig_unlock;
     if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

     if (H.FlipRate<>rateTable[rate]) then
     begin
      Writeln('sceVideoOutSetFlipRate:',rateTable[rate]);
     end;

     H.FlipRate:=rateTable[rate];
     H.Release;
     Result:=0;
    end;
  else
   Result:=SCE_VIDEO_OUT_ERROR_INVALID_VALUE;
 end;
end;

function ps4_sceVideoOutGetFlipStatus(hVideo:Integer;status:PSceVideoOutFlipStatus):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 status^:=Default(SceVideoOutFlipStatus);
 status^.count         :=H.FlipStatus.Fcount_flips   ;
 status^.processTime   :=H.FlipStatus.FprocessTime   ;
 status^.tsc           :=H.FlipStatus.Ftsc_flips     ;
 status^.submitTsc     :=H.FlipStatus.FsubmitTsc     ;
 status^.flipArg       :=H.FlipStatus.FflipArg       ;
 status^.gcQueueNum    :=H.FlipStatus.FgcQueueNum    ;
 status^.flipPendingNum:=H.FlipStatus.FflipPendingNum;
 status^.currentBuffer :=H.FlipStatus.FcurrentBuffer ;
 Result:=0;

 H.Release;
end;

function ps4_sceVideoOutIsFlipPending(hVideo:Integer):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 Result:=H.FlipStatus.FflipPendingNum;

 H.Release;
end;

function ps4_sceVideoOutGetBufferLabelAddress(hVideo:Integer;ptr:PPInt64):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 if (ptr=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;

 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);
 ptr^:=@H.FLabels;

 _sig_lock;
 H.Release;
 _sig_unlock;

 Result:=0;
end;

function _on_after(node:PKEventNode;data:Pointer):Boolean;
begin
 //Writeln('+on_trigger_flip');
 node^.ev.data:=((node^.ev.data shr 16) shl 16) or (1 shl 12);
end;

procedure _on_trigger_flip(data,userdata:Pointer);
var
 node:PKEventNode;
 count:Byte;
begin
 node:=data;
 if (node=nil) then Exit;
 count:=(node^.ev.data shr 12) and $F;
 if (count<15) then Inc(count);
 node^.ev.data:=(ptruint(userdata) shl 16) or (count shl 12);
 _trigger_kevent_node(node,@_on_after,nil);
end;

procedure _on_trigger_blank(data,userdata:Pointer);
var
 node:PKEventNode;
 count:Byte;
begin
 node:=data;
 if (node=nil) then Exit;
 node^.ev.data:=(ptruint(userdata) and $FFFFFFFFFFFF);
 _trigger_kevent_node(node,@_on_after,nil);
end;

procedure TVideoOut.post_event_flip(flipArg:Int64);
begin
 //Writeln('post_event_flip');
 FlipEvents.LockRd;
 HAMT_traverse64(@FlipEvents.hamt,@_on_trigger_flip,Pointer(flipArg));
 FlipEvents.Unlock;
end;

procedure TVideoOut.post_event_vblank;
var
 elap:QWORD;
 count:QWORD;
 time:DWORD;
 hz:Byte;
begin
 hz:=60; //59.94

 time:=(1000000 div hz);
 elap:=SwTimePassedUnits(VblankStatus.FTsc);
 elap:=(elap+9) div 10;

 count:=elap div time;

 //Writeln('post_event_vblank');
 VblankEvents.LockRd;
 HAMT_traverse64(@VblankEvents.hamt,@_on_trigger_blank,Pointer(count));
 VblankEvents.Unlock;
end;

procedure TVideoOut.sceVideoOutSubmitFlip(node:PQNode);
var
 bufferIndex:Integer;
 flipMode:Integer;
 flipArg:Int64;
 _type:Integer;

 pos:array[0..1] of TVCursorPos;

 addr:Pointer;
 attr:SceVideoOutBufferAttribute;

 //buf:TvPointer;

 elap:QWORD;
 time:DWORD;

 //t1,t2:QWORD;
begin
 bufferIndex:=node^.u.bufferIndex;
 flipMode   :=node^.u.flipMode   ;
 flipArg    :=node^.u.flipArg    ;
 _type      :=node^.u._type      ;

 //node^.wait:=1;
 //free_node(node);

 //Writeln('sceVideoOutSubmitFlip:',bufferIndex);

 //First Set flip data, second post event !!!!!

 FlipStatus.FcurrentBuffer:=bufferIndex;

 FlipStatus.FflipArg    :=flipArg;

 //ps4_usleep(150);

 if (bufferIndex=SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
 begin
  //ps4_usleep(150);
  //post_event_vblank(flipArg);
 end else
 begin
  if (FGpuFlip=nil) then
  begin
   //ps4_usleep(150);
  end else
  begin

   spin_lock(FBuffers.lock);
   addr:=FBuffers.addr[bufferIndex].left;
   attr:=FBuffers.attr[bufferIndex];
   spin_unlock(FBuffers.lock);

   FGpuFlip.SetCurrentBuffer(FlipStatus.FcurrentBuffer);
   FGpuFlip.SetImageFormat(attr.format,attr.tmode);
   FGpuFlip.SetImageSize(attr.width,attr.height);
   FGpuFlip.SetHostBuffer(addr);
   spin_lock(FCursors[0].lock);
   spin_lock(FCursors[1].lock);

   QWORD(Pos[0]):=System.InterlockedExchangeAdd64(QWORD(FCursors[0].Pos),0);
   QWORD(Pos[1]):=System.InterlockedExchangeAdd64(QWORD(FCursors[1].Pos),0);

   FGpuFlip.SetCursor(0,FCursors[0].enable=1,FCursors[0].adr,@FCursors[0].pending,Pos[0]);
   FGpuFlip.SetCursor(1,FCursors[1].enable=1,FCursors[1].adr,@FCursors[1].pending,Pos[1]);

   FGpuFlip.Flip;

   if (FCursors[0].enable=0) then
   begin
    FCursors[0].adr:=nil;
   end;
   if (FCursors[1].enable=0) then
   begin
    FCursors[1].adr:=nil;
   end;

   spin_unlock(FCursors[0].lock);
   spin_unlock(FCursors[1].lock);


  end;
  //post_event_vblank(flipArg);
  //post_event_flip(flipArg);
 end;

 //node^.wait:=1;
 //free_node(node);

 //FlipRate:=20;
 if (flipMode=SCE_VIDEO_OUT_FLIP_MODE_VSYNC) then
 if (FlipRate<>0) then
 begin
  time:=(1000000 div (FlipRate{+1}));

  elap:=SwTimePassedUnits(FlipStatus.Ftsc_flips);
  elap:=((elap+9) div 10)+120;

  if (elap<time) then
  begin
   time:=time-elap;
  end else
  begin
   time:=0;
  end;

  //SwSaveTime(t1);

  if (time>100) then
  begin
   ps4_usleep(time);
  end;
  //Sleep(_usec2msec(time));

  //t2:=SwTimePassedUnits(t1);
  //t2:=(t2+9) div 10;
  //Writeln('elap=',elap,' time=',time,' usleep=',t2);

  if (FGpuFlip<>nil) and (bufferIndex<>SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
  begin
   //FGpuFlip.IsComplite(bufferIndex);
   While (not FGpuFlip.IsComplite(bufferIndex)) do
   begin
    ps4_usleep(150);
   end;
  end;

 end;

 if (bufferIndex=SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
 begin
  post_event_flip(flipArg);
  post_event_vblank;
 end else
 begin
  System.InterlockedDecrement64(FLabels[bufferIndex]);

  post_event_flip(flipArg);
  post_event_vblank;
 end;

 Case _type of
  0:begin
     System.InterlockedDecrement(FlipStatus.FflipPendingNum);
    end;
  1:begin
     System.InterlockedDecrement(FlipStatus.FgcQueueNum);
     System.InterlockedDecrement(FlipStatus.FflipPendingNum);
    end;
 end;

 FlipStatus.Fcount_flips:=FlipStatus.Fcount_flips+1;   //Number of flips completed after opening the port self
 FlipStatus.FprocessTime:=ps4_sceKernelGetProcessTime; //Process time upon completion of the last flip
 FlipStatus.Ftsc_flips  :=ps4_sceKernelReadTsc;        //System timestamp counter value when the last flip completed

 if (Ftsc_prev=0) then
 begin
  Ftsc_prev:=FlipStatus.Ftsc_flips;
  Ffps:=1;
 end else
 begin
  Inc(Ffps);
  if ((FlipStatus.Ftsc_flips-Ftsc_prev) div ps4_sceKernelGetTscFrequency)>=1 then
  begin
   FForm.SetCaptionFPS(Ffps);
   Ffps:=0;
   Ftsc_prev:=FlipStatus.Ftsc_flips;
  end;
 end;

 node^.wait:=1;
 free_node(node);

end;

function ps4_sceVideoOutSubmitFlip(hVideo:Integer;bufferIndex,flipMode:Integer;flipArg:Int64):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
 node:PQNode;

begin
 Result:=0;

 if ((bufferIndex<0) or (bufferIndex>=SCE_VIDEO_OUT_BUFFER_NUM_MAX)) and
    (bufferIndex<>SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
 begin
  Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 end;

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 node:=H.alloc_node;

 if (node=nil) then
 begin
  H.Release;
  Exit(SCE_VIDEO_OUT_ERROR_FLIP_QUEUE_FULL);
 end;

 if (bufferIndex<>SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
 begin
  System.InterlockedIncrement64(H.FLabels[bufferIndex]);
 end;

 node^.Parent:=@H.sceVideoOutSubmitFlip;
 node^.u.bufferIndex:=bufferIndex;
 node^.u.flipMode   :=flipMode;
 node^.u.flipArg    :=flipArg;
 node^.u._type      :=0;
 node^.wait         :=0;

 System.InterlockedIncrement(H.FlipStatus.FflipPendingNum);

 H.FlipStatus.FsubmitTsc:=ps4_sceKernelReadTsc; //Timestamp counter value when the last completed flip is requested

 //Writeln('submit_event_flip');
 _sig_lock;

 //Writeln('>sceVideoOutSubmitFlip:',bufferIndex);

 Push2VideoOut(node);

 wait_until_equal(node^.wait,0);

 //Writeln('<sceVideoOutSubmitFlip:',bufferIndex);

 //H.FlipStatus.FsubmitTsc:=ps4_sceKernelReadTsc; //Timestamp counter value when the last completed flip is requested

 H.Release;
 _sig_unlock;
end;

function _qc_sceVideoOutSubmitFlip(Flip:PqcFlipInfo):Integer;
var
 H:TVideoOut;
 node:PQNode;

begin
 Result:=0;
 if (Flip=nil) then Exit;

 if ((Flip^.bufferIndex<0) or (Flip^.bufferIndex>=SCE_VIDEO_OUT_BUFFER_NUM_MAX)) and
    (Flip^.bufferIndex<>SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
 begin
  Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 end;

 H:=TVideoOut(FVideoOutMap.Acqure(Flip^.hVideo));
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 node:=H.alloc_node;
 if (node=nil) then
 begin
  H.Release;
  Exit(SCE_VIDEO_OUT_ERROR_FLIP_QUEUE_FULL);
 end;

 if (Flip^.bufferIndex<>SCE_VIDEO_OUT_BUFFER_INDEX_BLANK) then
 begin
  System.InterlockedIncrement64(H.FLabels[Flip^.bufferIndex]);
 end;

 node^.Parent:=@H.sceVideoOutSubmitFlip;
 node^.u.bufferIndex:=Flip^.bufferIndex;
 node^.u.flipMode   :=Flip^.flipMode;
 node^.u.flipArg    :=Flip^.flipArg;
 node^.u._type      :=1;
 node^.wait         :=0;

 System.InterlockedIncrement(H.FlipStatus.FgcQueueNum);
 System.InterlockedIncrement(H.FlipStatus.FflipPendingNum);

 H.FlipStatus.FsubmitTsc:=ps4_sceKernelReadTsc; //Timestamp counter value when the last completed flip is requested

 //Writeln('submit_event_flip');
 Push2VideoOut(node);

 wait_until_equal(node^.wait,0);

 //H.FlipStatus.FsubmitTsc:=ps4_sceKernelReadTsc; //Timestamp counter value when the last completed flip is requested

 H.Release;
end;

function ps4_sceVideoOutCursorSetPosition(hVideo:Integer;index:Integer;posX,posY:DWORD):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
 P:TVPos;
begin
 Result:=0;
 if (index<0) or (index>=SCE_VIDEO_OUT_CURSOR_NUM_MAX) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);
 P.X:=posX; P.Y:=posY;
 System.InterlockedExchange64(QWORD(H.FCursors[index].Pos),QWORD(P));
 _sig_lock;
 H.Release;
 _sig_unlock;
end;

function __sceVideoOutCursorEnable(hVideo:Integer;index:Integer;address:Pointer):Integer;
var
 H:TVideoOut;
 buf:TvPointer;
begin
 Result:=0;
 if (index<0) or (index>=SCE_VIDEO_OUT_CURSOR_NUM_MAX) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);

 if not IsAlign(address,4*1024) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);
 if not TryGetHostPointerByAddr(address,buf) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_MEMORY);

 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 spin_trylock(H.FCursors[index].lock);
 H.FCursors[index].enable:=1;
 H.FCursors[index].adr:=address;
 spin_unlock(H.FCursors[index].lock);

 H.Release;
end;

function ps4_sceVideoOutCursorEnable(hVideo:Integer;index:Integer;address:Pointer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=__sceVideoOutCursorEnable(hVideo,index,address);
 _sig_unlock;
end;

function __sceVideoOutCursorSetImageAddress(hVideo:Integer;index:Integer;address:Pointer):Integer;
var
 H:TVideoOut;
 buf:TvPointer;
begin
 Result:=0;
 if (index<0) or (index>=SCE_VIDEO_OUT_CURSOR_NUM_MAX) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 if not IsAlign(address,4*1024) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_ADDRESS);
 if not TryGetHostPointerByAddr(address,buf) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_MEMORY);

 spin_trylock(H.FCursors[index].lock);
 H.FCursors[index].adr:=address;
 spin_unlock(H.FCursors[index].lock);

 H.Release;
end;

function ps4_sceVideoOutCursorSetImageAddress(hVideo:Integer;index:Integer;address:Pointer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=__sceVideoOutCursorSetImageAddress(hVideo,index,address);
 _sig_unlock;
end;

function ps4_sceVideoOutCursorIsUpdatePending(hVideo:Integer;index:Integer;_type:DWORD):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 Result:=0;
 if (index<0) or (index>=SCE_VIDEO_OUT_CURSOR_NUM_MAX) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_INDEX);
 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 Case _type of
  SCE_VIDEO_OUT_CURSOR_IMAGE_ADDRESS_PENDING:if (H.FCursors[index].pending<>0) then Result:=1;
  SCE_VIDEO_OUT_CURSOR_DISABLE_PENDING      :if (H.FCursors[index].adr<>nil) then Result:=1;
  else
   Result:=SCE_VIDEO_OUT_ERROR_INVALID_VALUE;
 end;

 _sig_lock;
 H.Release;
 _sig_unlock;
end;

function ps4_sceVideoOutGetVblankStatus(hVideo:Integer;status:PSceVideoOutVblankStatus):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;

 elap:QWORD;
 pc,pf:QWORD;
 proc:QWORD;
 count:QWORD;
 time:DWORD;
 hz:Byte;
begin
 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 hz:=60; //59.94

 time:=(1000000 div hz);
 elap:=SwTimePassedUnits(H.VblankStatus.FTsc);
 elap:=(elap+9) div 10;

 count:=elap div time;

 proc:=ps4_sceKernelGetProcessTime;
 proc:=proc-H.VblankStatus.FprocessTime;
 proc:=AlignDw(proc,time);
 proc:=proc+H.VblankStatus.FprocessTime;

 SwQueryPerformanceCounter(pc,pf);
 pc:=pc-H.VblankStatus.FTsc;
 time:=(pf div hz);
 pc:=AlignDw(pc,time);
 pc:=pc+H.VblankStatus.FTsc;

 status^:=Default(SceVideoOutVblankStatus);
 status^.count         :=count;
 status^.processTime   :=proc;
 status^.tsc           :=pc;
 status^.flags         :=0;
 Result:=0;

 _sig_lock;
 H.Release;
 _sig_unlock;
end;

function ps4_sceVideoOutSetWindowModeMargins(hVideo:Integer;top,bottom:Integer):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 _sig_lock;
 H.Release;
 _sig_unlock;

 Result:=0;
end;

const
//SceVideoOutDeviceCapability
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_YUV                        = 1;
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_XVYCC                      = 1 shl 1;
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_S3D_FRAME_PACKING_59_94HZ  = 1 shl 2;
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_VR_VIEW_59_94HZ            = 1 shl 4;
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_VR_VIEW_119_88HZ           = 1 shl 5;
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_VR_VIEW_89_91HZ            = 1 shl 6;
 SCE_VIDEO_OUT_DEVICE_CAPABILITY_BT2020_PQ                  = 1 shl 7;

//SceVideoOutSignalEncoding
 SCE_VIDEO_OUT_SIGNAL_ENCODING_UNKNOWN = 0;
 SCE_VIDEO_OUT_SIGNAL_ENCODING_RGB444  = 1;
 SCE_VIDEO_OUT_SIGNAL_ENCODING_YUV444  = 2;
 SCE_VIDEO_OUT_SIGNAL_ENCODING_YUV422  = 3;

 SCE_VIDEO_OUT_SIGNAL_ENCODING_ANY     = $FF;


//SceVideoOutSignalRange
 SCE_VIDEO_OUT_SIGNAL_RANGE_UNKNOWN = 0;
 SCE_VIDEO_OUT_SIGNAL_RANGE_LIMITED = 1;
 SCE_VIDEO_OUT_SIGNAL_RANGE_FULL    = 2;

 SCE_VIDEO_OUT_SIGNAL_RANGE_ANY     = $FF;

//SceVideoOutColorimetry
 SCE_VIDEO_OUT_COLORIMETRY_UNKNOWN   = 0;
 SCE_VIDEO_OUT_COLORIMETRY_SRGB      = 1;
 SCE_VIDEO_OUT_COLORIMETRY_CERGB     = 2;
 SCE_VIDEO_OUT_COLORIMETRY_YCBCR     = 3;
 SCE_VIDEO_OUT_COLORIMETRY_YCBCR601  = 4;
 SCE_VIDEO_OUT_COLORIMETRY_YCBCR709  = 5;
 SCE_VIDEO_OUT_COLORIMETRY_XVYCC     = 6;
 SCE_VIDEO_OUT_COLORIMETRY_XVYCC601  = 7;
 SCE_VIDEO_OUT_COLORIMETRY_XVYCC709  = 8;

 SCE_VIDEO_OUT_COLORIMETRY_BT2020       = 9 ; // RGB or YCbCr
 SCE_VIDEO_OUT_COLORIMETRY_RGB2020      = 10;
 SCE_VIDEO_OUT_COLORIMETRY_YCBCR2020    = 11;

 SCE_VIDEO_OUT_COLORIMETRY_BT2020_PQ    = 12; // RGB or YCbCr
 SCE_VIDEO_OUT_COLORIMETRY_RGB2020_PQ   = 13;
 SCE_VIDEO_OUT_COLORIMETRY_YCBCR2020_PQ = 14;

 SCE_VIDEO_OUT_COLORIMETRY_ANY          = $FF;

//SceVideoOutColorDepth
 SCE_VIDEO_OUT_COLOR_DEPTH_UNKNOWN = 0 ;
 SCE_VIDEO_OUT_COLOR_DEPTH_24BPP   = 24;
 SCE_VIDEO_OUT_COLOR_DEPTH_30BPP   = 30;
 SCE_VIDEO_OUT_COLOR_DEPTH_36BPP   = 36;

 SCE_VIDEO_OUT_COLOR_DEPTH_ANY     = $FF;


//SceVideoOutResolution
 SCE_VIDEO_OUT_RESOLUTION_UNKNOWN         =  0;

 SCE_VIDEO_OUT_RESOLUTION_480I            =  1;
 SCE_VIDEO_OUT_RESOLUTION_480I_WIDESCREEN =  2;
 SCE_VIDEO_OUT_RESOLUTION_576I            =  3;
 SCE_VIDEO_OUT_RESOLUTION_576I_WIDESCREEN =  4;

 SCE_VIDEO_OUT_RESOLUTION_480P            =  5;
 SCE_VIDEO_OUT_RESOLUTION_480P_WIDESCREEN =  6;
 SCE_VIDEO_OUT_RESOLUTION_576P            =  7;
 SCE_VIDEO_OUT_RESOLUTION_576P_WIDESCREEN =  8;

 SCE_VIDEO_OUT_RESOLUTION_720P            =  9;
 SCE_VIDEO_OUT_RESOLUTION_1080I           = 10;
 SCE_VIDEO_OUT_RESOLUTION_1080P           = 11;

 SCE_VIDEO_OUT_RESOLUTION_720P_S3D_FRAME_PACKING = $A0;

 SCE_VIDEO_OUT_RESOLUTION_1080P_VR_VIEW  = $E1;

 SCE_VIDEO_OUT_RESOLUTION_ANY_S3D       = $FFFFFFFF81FFFFFF;
 SCE_VIDEO_OUT_RESOLUTION_ANY_VR_VIEW   = $FFFFFFFFC1FFFFFF;
 SCE_VIDEO_OUT_RESOLUTION_ANY           = $FFFFFFFFFFFFFFFF;

//SceVideoOutContentType
 SCE_VIDEO_OUT_CONTENT_TYPE_UNKNOWN  = 0;
 SCE_VIDEO_OUT_CONTENT_TYPE_GRAPHICS = 1;
 SCE_VIDEO_OUT_CONTENT_TYPE_PHOTO    = 2;
 SCE_VIDEO_OUT_CONTENT_TYPE_CINEMA   = 3;
 SCE_VIDEO_OUT_CONTENT_TYPE_GAME     = 4;
 SCE_VIDEO_OUT_CONTENT_TYPE_ANY      = $FF;


type
 pSceVideoOutMode=^SceVideoOutMode;
 SceVideoOutMode=packed record
  size:DWORD;          // sizeof(SceVideoOutMode)
  signalEncoding:Byte; // SceVideoOutSignalEncoding
  signalRange:Byte;    // SceVideoOutSignalRange
  colorimetry:Byte;    // SceVideoOutColorimetry
  depth:Byte;          // SceVideoOutColorDepth
  refreshRate:QWORD;   // SceVideoOutRefreshRate
  resolution:QWORD;    // SceVideoOutResolution
  contentType:Byte;    // SceVideoOutContentType
  _reserved0:array[0..2] of Byte;
  _reserved:DWORD;
 end;

function ps4_sceVideoOutModeSetAny_(pMode:pSceVideoOutMode;sizeOfMode:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

type
 SceVideoOutVrViewCropAdjustment=packed record
  verticalOffset:Word;
  reserved0:Word;
  reserved1:array[0..2] of DWORD;
 end;

 SceVideoOutConfigureOptions=packed record
  vrViewCropAdjustment:SceVideoOutVrViewCropAdjustment;
 end;
 pSceVideoOutConfigureOptions=^SceVideoOutConfigureOptions;


function ps4_sceVideoOutConfigureOutputMode_(hVideo:Integer;
                                             reserved:DWORD;
                                             pMode:pSceVideoOutMode;
                                             pOptions:pSceVideoOutConfigureOptions;
                                             sizeOfMode:DWORD;
                                             sizeOfOptions:DWORD):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 if (pMode=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);
 if (sizeOfMode<SizeOf(SceVideoOutMode)) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);

 Case pMode^.resolution of
  SCE_VIDEO_OUT_RESOLUTION_720P_S3D_FRAME_PACKING,
  SCE_VIDEO_OUT_RESOLUTION_1080P_VR_VIEW         ,
  SCE_VIDEO_OUT_RESOLUTION_ANY_S3D               ,
  SCE_VIDEO_OUT_RESOLUTION_ANY_VR_VIEW           :Exit(SCE_VIDEO_OUT_ERROR_UNSUPPORTED_OUTPUT_MODE);
 end;

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 _sig_lock;
 H.Release;
 _sig_unlock;

 Result:=0;
end;

function ps4_sceVideoOutGetDeviceCapabilityInfo_(hVideo:Integer;
                                                 pInfo:pSceVideoOutDeviceCapabilityInfo;
                                                 sizeOfInfo:QWORD):Integer; SysV_ABI_CDecl;
var
 H:TVideoOut;
begin
 if (pInfo=nil)then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);
 if (sizeOfInfo<SizeOf(SceVideoOutDeviceCapabilityInfo)) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_VALUE);

 _sig_lock;
 H:=TVideoOut(FVideoOutMap.Acqure(hVideo));
 _sig_unlock;
 if (H=nil) then Exit(SCE_VIDEO_OUT_ERROR_INVALID_HANDLE);

 _sig_lock;
 H.Release;
 _sig_unlock;

 pInfo^.capability:=0;

 Result:=0;
end;

//

function Load_libSceVideoOut(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceVideoOut');

 lib^.set_proc($529DFA3D393AF3B1,@ps4_sceVideoOutOpen);
 lib^.set_proc($BAAB951F8FC3BBBF,@ps4_sceVideoOutClose);
 lib^.set_proc($32DE101C793190E7,@ps4_sceVideoOutGetEventCount);
 lib^.set_proc($AD651370A7645334,@ps4_sceVideoOutGetEventData);
 lib^.set_proc($536249B52A8D2992,@ps4_sceVideoOutGetEventId);
 lib^.set_proc($1D7CE32BDC88DF49,@ps4_sceVideoOutAddFlipEvent);
 lib^.set_proc($5EBBBDDB01C94668,@ps4_sceVideoOutAddVblankEvent);
 lib^.set_proc($8BAFEC47DD56B7FE,@ps4_sceVideoOutSetBufferAttribute);
 lib^.set_proc($C37058FAD0048906,@ps4_sceVideoOutRegisterBuffers);
 lib^.set_proc($9424C23A88116E4D,@ps4_sceVideoOutRegisterStereoBuffers);
 lib^.set_proc($379283B642238C9E,@ps4_sceVideoOutUnregisterBuffers);
 lib^.set_proc($0D886159B2527918,@ps4_sceVideoOutColorSettingsSetGamma_);
 lib^.set_proc($A6FF42239542F91D,@ps4_sceVideoOutAdjustColor_);
 lib^.set_proc($EA43E78F9D53EB66,@ps4_sceVideoOutGetResolutionStatus);
 lib^.set_proc($0818AEE26084D430,@ps4_sceVideoOutSetFlipRate);
 lib^.set_proc($49B537770A7CD254,@ps4_sceVideoOutGetFlipStatus);
 lib^.set_proc($CE05E27C74FD12B6,@ps4_sceVideoOutIsFlipPending);
 lib^.set_proc($538E8DC0E889A72B,@ps4_sceVideoOutSubmitFlip);
 lib^.set_proc($375EC02BCF0D743D,@ps4_sceVideoOutCursorSetPosition);
 lib^.set_proc($50F656087F2A4CCE,@ps4_sceVideoOutCursorEnable);
 lib^.set_proc($BBFF5B856400A6AF,@ps4_sceVideoOutCursorSetImageAddress);
 lib^.set_proc($1E26CEB5ECF34FA3,@ps4_sceVideoOutCursorIsUpdatePending);
 lib^.set_proc($D456412B2F0778D5,@ps4_sceVideoOutGetVblankStatus);
 lib^.set_proc($313C71ACE09E4A28,@ps4_sceVideoOutSetWindowModeMargins);
 lib^.set_proc($A63903B20C658BA7,@ps4_sceVideoOutModeSetAny_);
 lib^.set_proc($3756C4A09E12470E,@ps4_sceVideoOutConfigureOutputMode_);
 lib^.set_proc($90654B73786D404F,@ps4_sceVideoOutGetDeviceCapabilityInfo_);
end;

initialization
 FQueueVideoOut.Create;
 FVideoOutMap:=TIntegerHandles.Create(1);
 ps4_app.RegistredPreLoad('libSceVideoOut.prx',@Load_libSceVideoOut);

end.

