unit renderdoc;

{$mode ObjFPC}{$H+}

interface

uses
  Windows,
  Classes,
  SysUtils;

type
 RENDERDOC_Version=(
  eRENDERDOC_API_Version_1_0_0 = 10000,    // RENDERDOC_API_1_0_0 = 1 00 00
  eRENDERDOC_API_Version_1_0_1 = 10001,    // RENDERDOC_API_1_0_1 = 1 00 01
  eRENDERDOC_API_Version_1_0_2 = 10002,    // RENDERDOC_API_1_0_2 = 1 00 02
  eRENDERDOC_API_Version_1_1_0 = 10100,    // RENDERDOC_API_1_1_0 = 1 01 00
  eRENDERDOC_API_Version_1_1_1 = 10101,    // RENDERDOC_API_1_1_1 = 1 01 01
  eRENDERDOC_API_Version_1_1_2 = 10102,    // RENDERDOC_API_1_1_2 = 1 01 02
  eRENDERDOC_API_Version_1_2_0 = 10200,    // RENDERDOC_API_1_2_0 = 1 02 00
  eRENDERDOC_API_Version_1_3_0 = 10300,    // RENDERDOC_API_1_3_0 = 1 03 00
  eRENDERDOC_API_Version_1_4_0 = 10400,    // RENDERDOC_API_1_4_0 = 1 04 00
  eRENDERDOC_API_Version_1_4_1 = 10401,    // RENDERDOC_API_1_4_1 = 1 04 01
  eRENDERDOC_API_Version_1_4_2 = 10402,    // RENDERDOC_API_1_4_2 = 1 04 02
  eRENDERDOC_API_Version_1_5_0 = 10500,    // RENDERDOC_API_1_5_0 = 1 05 00
  eRENDERDOC_API_Version_1_6_0 = 10600     // RENDERDOC_API_1_6_0 = 1 06 00
 );

 RENDERDOC_CaptureOption=(
  // Allow the application to enable vsync
  //
  // Default - enabled
  //
  // 1 - The application can enable or disable vsync at will
  // 0 - vsync is force disabled
  eRENDERDOC_Option_AllowVSync = 0,

  // Allow the application to enable fullscreen
  //
  // Default - enabled
  //
  // 1 - The application can enable or disable fullscreen at will
  // 0 - fullscreen is force disabled
  eRENDERDOC_Option_AllowFullscreen = 1,

  // Record API debugging events and messages
  //
  // Default - disabled
  //
  // 1 - Enable built-in API debugging features and records the results into
  //     the capture, which is matched up with events on replay
  // 0 - no API debugging is forcibly enabled
  eRENDERDOC_Option_APIValidation = 2,
  eRENDERDOC_Option_DebugDeviceMode = 2,    // deprecated name of this enum

  // Capture CPU callstacks for API events
  //
  // Default - disabled
  //
  // 1 - Enables capturing of callstacks
  // 0 - no callstacks are captured
  eRENDERDOC_Option_CaptureCallstacks = 3,

  // When capturing CPU callstacks, only capture them from actions.
  // This option does nothing without the above option being enabled
  //
  // Default - disabled
  //
  // 1 - Only captures callstacks for actions.
  //     Ignored if CaptureCallstacks is disabled
  // 0 - Callstacks, if enabled, are captured for every event.
  eRENDERDOC_Option_CaptureCallstacksOnlyDraws = 4,
  eRENDERDOC_Option_CaptureCallstacksOnlyActions = 4,

  // Specify a delay in seconds to wait for a debugger to attach, after
  // creating or injecting into a process, before continuing to allow it to run.
  //
  // 0 indicates no delay, and the process will run immediately after injection
  //
  // Default - 0 seconds
  //
  eRENDERDOC_Option_DelayForDebugger = 5,

  // Verify buffer access. This includes checking the memory returned by a Map() call to
  // detect any out-of-bounds modification, as well as initialising buffers with undefined contents
  // to a marker value to catch use of uninitialised memory.
  //
  // NOTE: This option is only valid for OpenGL and D3D11. Explicit APIs such as D3D12 and Vulkan do
  // not do the same kind of interception & checking and undefined contents are really undefined.
  //
  // Default - disabled
  //
  // 1 - Verify buffer access
  // 0 - No verification is performed, and overwriting bounds may cause crashes or corruption in
  //     RenderDoc.
  eRENDERDOC_Option_VerifyBufferAccess = 6,

  // The old name for eRENDERDOC_Option_VerifyBufferAccess was eRENDERDOC_Option_VerifyMapWrites.
  // This option now controls the filling of uninitialised buffers with 0xdddddddd which was
  // previously always enabled
  eRENDERDOC_Option_VerifyMapWrites = eRENDERDOC_Option_VerifyBufferAccess,

  // Hooks any system API calls that create child processes, and injects
  // RenderDoc into them recursively with the same options.
  //
  // Default - disabled
  //
  // 1 - Hooks into spawned child processes
  // 0 - Child processes are not hooked by RenderDoc
  eRENDERDOC_Option_HookIntoChildren = 7,

  // By default RenderDoc only includes resources in the final capture necessary
  // for that frame, this allows you to override that behaviour.
  //
  // Default - disabled
  //
  // 1 - all live resources at the time of capture are included in the capture
  //     and available for inspection
  // 0 - only the resources referenced by the captured frame are included
  eRENDERDOC_Option_RefAllResources = 8,

  // **NOTE**: As of RenderDoc v1.1 this option has been deprecated. Setting or
  // getting it will be ignored, to allow compatibility with older versions.
  // In v1.1 the option acts as if it's always enabled.
  //
  // By default RenderDoc skips saving initial states for resources where the
  // previous contents don't appear to be used, assuming that writes before
  // reads indicate previous contents aren't used.
  //
  // Default - disabled
  //
  // 1 - initial contents at the start of each captured frame are saved, even if
  //     they are later overwritten or cleared before being used.
  // 0 - unless a read is detected, initial contents will not be saved and will
  //     appear as black or empty data.
  eRENDERDOC_Option_SaveAllInitials = 9,

  // In APIs that allow for the recording of command lists to be replayed later,
  // RenderDoc may choose to not capture command lists before a frame capture is
  // triggered, to reduce overheads. This means any command lists recorded once
  // and replayed many times will not be available and may cause a failure to
  // capture.
  //
  // NOTE: This is only true for APIs where multithreading is difficult or
  // discouraged. Newer APIs like Vulkan and D3D12 will ignore this option
  // and always capture all command lists since the API is heavily oriented
  // around it and the overheads have been reduced by API design.
  //
  // 1 - All command lists are captured from the start of the application
  // 0 - Command lists are only captured if their recording begins during
  //     the period when a frame capture is in progress.
  eRENDERDOC_Option_CaptureAllCmdLists = 10,

  // Mute API debugging output when the API validation mode option is enabled
  //
  // Default - enabled
  //
  // 1 - Mute any API debug messages from being displayed or passed through
  // 0 - API debugging is displayed as normal
  eRENDERDOC_Option_DebugOutputMute = 11,

  // Option to allow vendor extensions to be used even when they may be
  // incompatible with RenderDoc and cause corrupted replays or crashes.
  //
  // Default - inactive
  //
  // No values are documented, this option should only be used when absolutely
  // necessary as directed by a RenderDoc developer.
  eRENDERDOC_Option_AllowUnsupportedVendorExtensions = 12,

  // Define a soft memory limit which some APIs may aim to keep overhead under where
  // possible. Anything above this limit will where possible be saved directly to disk during
  // capture.
  // This will cause increased disk space use (which may cause a capture to fail if disk space is
  // exhausted) as well as slower capture times.
  //
  // Not all memory allocations may be deferred like this so it is not a guarantee of a memory
  // limit.
  //
  // Units are in MBs, suggested values would range from 200MB to 1000MB.
  //
  // Default - 0 Megabytes
  eRENDERDOC_Option_SoftMemoryLimit = 13
 );

 RENDERDOC_DevicePointer=THandle;
 RENDERDOC_WindowHandle =THandle;

function  IsRenderDocPreLoaded:Boolean;
procedure LoadRenderDoc;

procedure GetAPIVersion(major,minor,patch:PInteger);

function  SetCaptureOptionU32(opt:RENDERDOC_CaptureOption;val:DWORD ):Integer;
function  SetCaptureOptionF32(opt:RENDERDOC_CaptureOption;val:Single):Integer;

function  GetCaptureOptionU32(opt:RENDERDOC_CaptureOption):DWORD;
function  GetCaptureOptionF32(opt:RENDERDOC_CaptureOption):Single;

/////

Procedure RemoveHooks;
Procedure UnloadCrashHandler;

/////

function  IsTargetControlConnected:DWORD;

/////

procedure StartFrameCapture  (device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle);
function  IsFrameCapturing   ():DWORD;
function  EndFrameCapture    (device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle):DWORD;
function  DiscardFrameCapture(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle):DWORD;

implementation

var
 LibHandle:TLibHandle=NilHandle;

type
 pRENDERDOC_GetAPI=function(version:RENDERDOC_Version;outAPIPointers:PPointer):Integer; cdecl;

 //

 pRENDERDOC_GetAPIVersion           =procedure(major,minor,patch:PInteger); cdecl;

 pRENDERDOC_SetCaptureOptionU32     =function(opt:RENDERDOC_CaptureOption;val:DWORD ):Integer; cdecl;
 pRENDERDOC_SetCaptureOptionF32     =function(opt:RENDERDOC_CaptureOption;val:Single):Integer; cdecl;

 pRENDERDOC_GetCaptureOptionU32     =function(opt:RENDERDOC_CaptureOption):DWORD; cdecl;
 pRENDERDOC_GetCaptureOptionF32     =function(opt:RENDERDOC_CaptureOption):Single; cdecl;

 /////

 pRENDERDOC_RemoveHooks             =procedure(); cdecl;
 pRENDERDOC_UnloadCrashHandler      =procedure(); cdecl;

 /////

 pRENDERDOC_IsTargetControlConnected=function():DWORD; cdecl;

 /////

 pRENDERDOC_StartFrameCapture       =procedure(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle); cdecl;
 pRENDERDOC_IsFrameCapturing        =function():DWORD; cdecl;
 pRENDERDOC_EndFrameCapture         =function(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle):DWORD; cdecl;
 pRENDERDOC_DiscardFrameCapture     =function(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle):DWORD; cdecl;



 PRENDERDOC_API_1_6_0=^TRENDERDOC_API_1_6_0;
 TRENDERDOC_API_1_6_0=record
  GetAPIVersion             :pRENDERDOC_GetAPIVersion;

  SetCaptureOptionU32       :pRENDERDOC_SetCaptureOptionU32;
  SetCaptureOptionF32       :pRENDERDOC_SetCaptureOptionF32;

  GetCaptureOptionU32       :pRENDERDOC_GetCaptureOptionU32;
  GetCaptureOptionF32       :pRENDERDOC_GetCaptureOptionF32;

  SetFocusToggleKeys        :Pointer; //pRENDERDOC_SetFocusToggleKeys
  SetCaptureKeys            :Pointer; //pRENDERDOC_SetCaptureKeys

  GetOverlayBits            :Pointer; //pRENDERDOC_GetOverlayBits
  MaskOverlayBits           :Pointer; //pRENDERDOC_MaskOverlayBits

  RemoveHooks               :pRENDERDOC_RemoveHooks;
  UnloadCrashHandler        :pRENDERDOC_UnloadCrashHandler;

  SetCaptureFilePathTemplate:Pointer; //pRENDERDOC_SetCaptureFilePathTemplate
  GetCaptureFilePathTemplate:Pointer; //pRENDERDOC_GetCaptureFilePathTemplate

  GetNumCaptures            :Pointer; //pRENDERDOC_GetNumCaptures
  GetCapture                :Pointer; //pRENDERDOC_GetCapture

  TriggerCapture            :Pointer; //pRENDERDOC_TriggerCapture
  IsTargetControlConnected  :pRENDERDOC_IsTargetControlConnected;

  LaunchReplayUI            :Pointer; //pRENDERDOC_LaunchReplayUI

  SetActiveWindow           :Pointer; //pRENDERDOC_SetActiveWindow

  StartFrameCapture         :pRENDERDOC_StartFrameCapture;
  IsFrameCapturing          :pRENDERDOC_IsFrameCapturing;
  EndFrameCapture           :pRENDERDOC_EndFrameCapture;

  TriggerMultiFrameCapture  :Pointer; //pRENDERDOC_TriggerMultiFrameCapture

  SetCaptureFileComments    :Pointer; //pRENDERDOC_SetCaptureFileComments
  DiscardFrameCapture       :pRENDERDOC_DiscardFrameCapture;
  ShowReplayUI              :Pointer; //pRENDERDOC_ShowReplayUI

  SetCaptureTitle           :Pointer; //pRENDERDOC_SetCaptureTitle
 end;

var
 RENDERDOC_GetAPI:pRENDERDOC_GetAPI=nil;
 RENDERDOC_API   :PRENDERDOC_API_1_6_0=nil;

function IsRenderDocPreLoaded:Boolean;
begin
 Result:=Windows.GetModuleHandle('renderdoc.dll')<>0;
end;

procedure LoadRenderDoc;
var
 fname:RawByteString;
 r:Integer;
begin
 if (LibHandle<>0) then Exit;

 fname:=Sysutils.GetEnvironmentVariable('ProgramFiles')+DirectorySeparator+'RenderDoc'+DirectorySeparator+'renderdoc.dll';

 LibHandle:=System.LoadLibrary(fname);
 if (LibHandle=NilHandle) then Exit;

 Pointer(RENDERDOC_GetAPI):=System.GetProcedureAddress(LibHandle,'RENDERDOC_GetAPI');
 if (RENDERDOC_GetAPI=nil) then Exit;

 r:=RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_6_0,@RENDERDOC_API);
 if (r=0) then Exit;
end;

procedure GetAPIVersion(major,minor,patch:PInteger);
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.GetAPIVersion<>nil) then
 begin
  RENDERDOC_API^.GetAPIVersion(major,minor,patch);
 end;
end;

function SetCaptureOptionU32(opt:RENDERDOC_CaptureOption;val:DWORD):Integer;
begin
 Result:=0;
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.SetCaptureOptionU32<>nil) then
 begin
  Result:=RENDERDOC_API^.SetCaptureOptionU32(opt,val);
 end;
end;

function SetCaptureOptionF32(opt:RENDERDOC_CaptureOption;val:Single):Integer;
begin
 Result:=0;
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.SetCaptureOptionF32<>nil) then
 begin
  Result:=RENDERDOC_API^.SetCaptureOptionF32(opt,val);
 end;
end;

function GetCaptureOptionU32(opt:RENDERDOC_CaptureOption):DWORD;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.GetCaptureOptionU32<>nil) then
 begin
  Result:=RENDERDOC_API^.GetCaptureOptionU32(opt);
 end;
end;

function GetCaptureOptionF32(opt:RENDERDOC_CaptureOption):Single;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.GetCaptureOptionF32<>nil) then
 begin
  Result:=RENDERDOC_API^.GetCaptureOptionF32(opt);
 end;
end;

Procedure RemoveHooks;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.RemoveHooks<>nil) then
 begin
  RENDERDOC_API^.RemoveHooks();
 end;
end;

Procedure UnloadCrashHandler;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.UnloadCrashHandler<>nil) then
 begin
  RENDERDOC_API^.UnloadCrashHandler();
 end;
end;

function IsTargetControlConnected:DWORD;
begin
 Result:=0;
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.IsTargetControlConnected<>nil) then
 begin
  Result:=RENDERDOC_API^.IsTargetControlConnected();
 end;
end;

procedure StartFrameCapture(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle);
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.StartFrameCapture<>nil) then
 begin
  RENDERDOC_API^.StartFrameCapture(device,wndHandle);
 end;
end;

function IsFrameCapturing():DWORD;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.IsFrameCapturing<>nil) then
 begin
  Result:=RENDERDOC_API^.IsFrameCapturing();
 end;
end;

function EndFrameCapture(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle):DWORD;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.EndFrameCapture<>nil) then
 begin
  Result:=RENDERDOC_API^.EndFrameCapture(device,wndHandle);
 end;
end;

function DiscardFrameCapture(device:RENDERDOC_DevicePointer;wndHandle:RENDERDOC_WindowHandle):DWORD;
begin
 if (RENDERDOC_API<>nil) then
 if (RENDERDOC_API^.DiscardFrameCapture<>nil) then
 begin
  Result:=RENDERDOC_API^.DiscardFrameCapture(device,wndHandle);
 end;
end;


end.

