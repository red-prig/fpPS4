unit kern_authinfo;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_proc_type_info=^t_proc_type_info;
 t_proc_type_info=packed record
  size  :QWORD;
  bptype:DWORD;
  pflags:DWORD;
 end;
 {$IF sizeof(t_proc_type_info)<>16}{$STOP sizeof(t_proc_type_info)<>16}{$ENDIF}

 p_authinfo=^t_authinfo;
 t_authinfo=packed record
  app_type :QWORD;
  app_caps :array[0..3] of QWORD; //62 bit IsSystem;61 bit IsGame;60 bit IsNongame;
  app_attrs:array[0..3] of QWORD;
  unknow   :array[0..7] of QWORD;
 end;
 {$IF sizeof(t_authinfo)<>136}{$STOP sizeof(t_authinfo)<>136}{$ENDIF}

const
 //eLoadOptions
 LOAD_OPTIONS_DEFAULT                         =$0000;
 LOAD_OPTIONS_LOAD_SUSPENDED                  =$0001;
 LOAD_OPTIONS_USE_SYSTEM_LIBRARY_VERIFICATION =$0002;
 LOAD_OPTIONS_SLV_MODE_WARN                   =$0004;
 LOAD_OPTIONS_ARG_STACK_SIZE                  =$0008;
 LOAD_OPTIONS_FULL_DEBUG_REQUIRED             =$0010;

 //mmap_flags
 //bit 1 -> is_big_app
 //bit 2 -> first find addr is (1 shl 33) ->
 //          _sceKernelMapFlexibleMemory
 //          _sceKernelMapDirectMemory
 //          sceKernelMapDirectMemory2

 //attributeExe
 //bit 1 -> use in [libkernel_exception] ->
 //      -> sceKernelInstallExceptionHandler
 //      -> sceKernelRemoveExceptionHandler
 //      -> sceKernelAddGpuExceptionEvent
 //      -> sceKernelDeleteGpuExceptionEvent
 //      -> sceKernelBacktraceSelf
 //bit 2 -> sys_mdbg_service

type
 TCUSANAME=array[0..9] of AnsiChar;

const
 //SceLncAppType
 SCE_LNC_APP_TYPE_INVALID   =-1;
 SCE_LNC_APP_TYPE_NULL      = 0;
 SCE_LNC_APP_TYPE_SHELL_UI  = 1; //isSystemApp
 SCE_LNC_APP_TYPE_DAEMON    = 2; //isSystemApp
 SCE_LNC_APP_TYPE_CDLG      = 3; //isSystemApp
 SCE_LNC_APP_TYPE_MINI_APP  = 4; //isSystemApp
 SCE_LNC_APP_TYPE_BIG_APP   = 5;
 SCE_LNC_APP_TYPE_SHELL_CORE= 6; //isSystemApp
 SCE_LNC_APP_TYPE_SHELL_APP = 7; //isSystemApp

 //[preloadPrxFlags] -> sceSysmodulePreloadModuleForLibkernel
 //0x0000000004 libSceNet
 //0x0000000008 libSceIpmi
 //0x0000000010 libSceMbus
 //0x0000000020 libSceRegMgr
 //0x0000000040 libSceRtc
 //0x0000000080 libSceAvSetting
 //0x0000000100 libSceVideoOut
 //0x0000000200 libSceGnmDriver
 //0x0000000400 libSceAudioOut
 //0x0000000800 libSceAudioIn
 //0x0000001000 libSceAjm
 //0x0000002000 libScePad
 //0x0000004000 libSceCamera
 //0x0000008000 libSceDbg
 //0x0000010000 libSceNetCtl
 //0x0000020000 libScettp
 //0x0000040000 libSceSsl
 //0x0000080000 libSceNpCommon
 //0x0000100000 libSceNpManager
 //0x0000200000 libSceNpWebApi
 //0x0000400000 libSceSaveData
 //0x0000800000 libSceSystemService
 //0x0001000000 libSceUserService
 //0x0002000000 libSceCommonDialog
 //0x0004000000 libSceSysUtil
 //0x0008000000 libScePerf
 //0x0010000000 libSceWebKit2ForVideoService
 //0x0020000000 libSceOrbisCompatForVideoService
 //0x0040000000 libSceFios2,libc
 //0x0080000000 libSceRazorCpu
 //0x0100000000 libSceRazorCpu_debug
 //0x1000000000 libSceHttp2
 //0x2000000000 libSceNpGameIntent
 //0x4000000000 libSceNpWebApi2

type
 t_title_workaround=packed record
  version:Integer;
  align  :Integer;
  ids    :array[0..1] of QWORD;
 end;

//workaround ids bits number
const
 BUG107292_EXTRA_USB_AUDIO_DEVICE                         =$00;
 BUG119504_SUSPEND_BLACK_LIST                             =$01;
 BUG113237_LIVE_DETAIL_BLACK_LIST                         =$02;
 BUG117780_GPU_DOUBLE_PRECISION                           =$03;
 BUG134640_FFXIV_MOVIE_CRASH                              =$04;
 BUG140207_FAKE_BG_EXECUTION                              =$05;
 BUG140207_DELAY_SUSPEND                                  =$06;
 BUG141751_FORCE_VIDEO_RECORDING_COMPATIBLE               =$07;
 BUG141953_FORCE_CONTENT_SEARCH_COMPATIBLE                =$08;
 BUG135666_FORCED_BASE_MODE                               =$09;
 BUG142996_NEW_QUICK_MENU_BLACK_LIST                      =$0A;
 BUG146562_PRODUCT_DETAIL_BLACK_LIST                      =$0B;
 BUG141677_DISABLE_SERVICE_ENTITLEMENT_UPDATE_EVENT       =$0C;
 BUG158272_EXTERNAL_HDD_BLACK_LIST                        =$0D;
 BUG159526_INVALIDATE_ENTITLEMENT_IN_APPLICATION_DB       =$0E;
 BUG163566_BOOST_MODE_BLACK_LIST                          =$0F;
 BUG171584_EXTERNAL_HDD_ACCESS_LATENCY                    =$10;
 BUG183465_SPECIAL_ISSUE                                  =$11;
 BUG183542_NEO_VDDNB_VID_3STEP                            =$12;
 BUG183542_NEO_VDDNB_VID_4STEP                            =$13;
 BUG183542_NEO_VDDNB_VID_5STEP                            =$14;
 BUG184831_NEO_VDDNB_VID_STEP_UP_ALL_TITLE                =$15;
 BUG180029_SAVE_DATA_MEMORY_TIMEOUT_10SEC                 =$16;
 BUG180341_WEBAPI_NOT_COPY_ERROR_JSON                     =$17;
 BUG180847_USE_RECRYPT_BLOCKS                             =$18;
 BUG182301_NP_MANAGER_KEEP_COMPATIBLE                     =$19;
 BUG182170_OSK                                            =$1A;
 BUG188290_NEO_SCLK_DOWN_LEVEL1                           =$1B;
 BUG188290_NEO_SCLK_DOWN_LEVEL2                           =$1C;
 BUG188290_NEO_SCLK_DOWN_LEVEL3                           =$1D;
 BUG187987_NTS_CONNECTHASHTABLE                           =$1E;
 BUG190872_HIDE_4K                                        =$1F;
 BUG191849_HDCP_CHECK_APP_ONLY                            =$20;
 BUG193000_USE_OLD_WEB_BROWSER_ENGINE                     =$21;
 BUG186690_IME_DISABLE_REMOTE_PLAY                        =$22;
 BUG196278_IME_DISABLE_REMOTE_PLAY_WITH_DISABLE_CONTROLLER=$23;
 BUG196285_IME_PACKED_UPDATE_TEXT                         =$24;
 BUG186690_IME_REMOTE_PLAY_FINISHED_BY_PRESS_ENTER        =$25;
 BUG196699_SYSMODULE_SWITCH_LIBSSL                        =$26;
 BUG192912_PLAYGO_FULL_MULTISTREAM                        =$27;
 BUG201910_DINO_FRONTIER_DLSYM                            =$28;
 BUG202240_HIKARU_UTADA_SCHED                             =$29;
 BUG198989_ANTHEM_KERNEL_PANIC                            =$2A;
 BUG202952_SSL_CHECK_RECV_PENDING_ALWAYS_TRUE             =$2B;
 BUG203700_PARTY_ROLLBACK                                 =$2C;
 CAMELOT3106_USE_OLD_STYLE_USER_AGENT                     =$2D;
 BUG209289_SESSION_SIGNALING_TERMINATE_ON_LEFT            =$2E;
 BUG198642_LB_SYNC_RESET_TO_FIX_CURSOR_8000               =$2F;
 BUG198642_LB_SYNC_RESET_NOT_TO_FIX_CURSOR                =$30;
 BUG210925_ENABLE_TLS_BUG_FIX                             =$31;
 NUM_WORKAROUND_ID                                        =$32;

type
 p_appinfo=^t_appinfo;
 t_appinfo=packed record
  AppId           :Integer;       //4
  mmap_flags      :Integer;       //4
  attributeExe    :Integer;       //4
  attribute2      :Integer;       //4
  CUSANAME        :TCUSANAME;     //10 titleId
  debug_level     :Byte;          //1
  slv_flags       :Byte;          //1  eLoadOptions
  budget_flags    :Byte;
  debug_out       :Byte;
  f_1e            :Byte;
  requiredHdcpType:Byte;
  preloadPrxFlags :QWORD;
  attribute       :Integer;
  hasParamSfo     :Integer;
  titleWorkaround :t_title_workaround;
 end;
 {$IF sizeof(t_appinfo)<>72}{$STOP sizeof(t_appinfo)<>72}{$ENDIF}

var
 g_authinfo:t_authinfo;
 g_appinfo :t_appinfo;

function sceSblACMgrHasMmapSelfCapability(info:p_authinfo):Boolean;
function sceSblACMgrHasUseHp3dPipeCapability(info:p_authinfo):Boolean;
function sceSblACMgrIsVideoplayerProcess(info:p_authinfo):Boolean;
function sceSblACMgrHasUseVideoServiceCapability(info:p_authinfo):Boolean;
function sceSblACMgrIsNongameUcred(info:p_authinfo):Boolean;
function sceSblACMgrIsSystemUcred(info:p_authinfo):Boolean;
function sceSblACMgrHasSceProgramAttribute(info:p_authinfo):Boolean;
function sceSblACMgrIsDebuggableProcess(info:p_authinfo):Boolean;
function sceSblACMgrIsAllowedToMmapSelf(icurr,ifile:p_authinfo):Boolean;
function sceSblACMgrIsDiagProcess(info:p_authinfo):Boolean;
function is_sce_prog_attr_20_800000(info:p_authinfo):Boolean;
function is_sce_prog_attr_20_400000(info:p_authinfo):Boolean;
function is_sce_prog_attr_40_800000(info:p_authinfo):Boolean;
function is_sce_prog_attr_40_400000(info:p_authinfo):Boolean;

function sys_get_proc_type_info(dst:Pointer):Integer;
function sys_get_authinfo(pid:Integer;info:Pointer):Integer;

implementation

uses
 errno,
 systm,
 kern_proc;

function sceSblACMgrHasMmapSelfCapability(info:p_authinfo):Boolean;
begin
 Result:=(info^.app_caps[1] and QWORD($400000000000000))<>0;
end;

function sceSblACMgrHasUseHp3dPipeCapability(info:p_authinfo):Boolean;
var
 app_type:QWORD;
begin
 app_type:=info^.app_type;
 if ((app_type<>QWORD($3800000000000009)) and (app_type<>QWORD($380100000000002c))) then
 begin
  Exit(false);
 end;
 Exit(true);
end;

function sceSblACMgrIsVideoplayerProcess(info:p_authinfo):Boolean;
begin
 Result:=(info^.app_type + QWORD($c7ffffffefffffff)) < 2;
end;

function sceSblACMgrHasUseVideoServiceCapability(info:p_authinfo):Boolean;
begin
 Result:=(info^.app_caps[1] and QWORD($200000000000000))<>0;
end;

function sceSblACMgrIsNongameUcred(info:p_authinfo):Boolean;
begin
 Result:=(info^.app_caps[0] and QWORD($1000000000000000))<>0;
end;

function sceSblACMgrIsSystemUcred(info:p_authinfo):Boolean;
begin
 Result:=(info^.app_caps[0] and QWORD($4000000000000000))<>0;
end;

function sceSblACMgrHasSceProgramAttribute(info:p_authinfo):Boolean;
begin
 Result:=(info^.app_attrs[0] and QWORD($80000000))<>0;
end;

function sceSblACMgrIsDebuggableProcess(info:p_authinfo):Boolean;
var
 attr:QWORD;
begin
 attr:=info^.app_attrs[0];
 if ((attr and QWORD($1000000))=0) then
 begin
  if ((attr and QWORD($2000000))<>0) then
  begin
   Exit(true);
  end;
  //sceSblRcMgrIsAllowULDebugger
 end;
 //sceSblRcMgrIsSoftwagnerQafForAcmgr
 Exit(false);
end;

function sceSblACMgrIsAllowedToMmapSelf(icurr,ifile:p_authinfo):Boolean;
begin
 Result:=True;
 if ((icurr^.app_caps[1] and QWORD($400000000000000))=0) or
    ((ifile^.app_attrs[0] and QWORD($8000000))=0) then
 begin
  Result:=False;
 end;
end;

function sceSblACMgrIsDiagProcess(info:p_authinfo):Boolean;
var
 attr:QWORD;
begin
 Result:=True;
 attr:=info^.app_type and QWORD($ff0f000000000000);
 if (attr<>QWORD($3801000000000000)) and (attr<>QWORD($3802000000000000)) then
 begin
  Result:=False;
 end;
end;

function is_sce_prog_attr_20_800000(info:p_authinfo):Boolean;
begin
 Result:=True;
 if ((info^.app_caps[1] and QWORD($2000000000000000))=0) or
    ((info^.app_attrs[0] and QWORD($800000))=0) then
 begin
  Result:=False;
 end;
end;

function is_sce_prog_attr_20_400000(info:p_authinfo):Boolean;
begin
 Result:=True;
 if ((info^.app_caps[1] and QWORD($2000000000000000))=0) or
    ((info^.app_attrs[0] and QWORD($400000))=0) then
 begin
  Result:=False;
 end;
end;

function is_sce_prog_attr_40_800000(info:p_authinfo):Boolean;
begin
 Result:=True;
 if ((info^.app_caps[1] and QWORD($4000000000000000))=0) or
    ((info^.app_attrs[0] and QWORD($800000))=0) then
 begin
  Result:=False;
 end;
end;

function is_sce_prog_attr_40_400000(info:p_authinfo):Boolean;
begin
 Result:=True;
 if ((info^.app_caps[1] and QWORD($4000000000000000))=0) or
    ((info^.app_attrs[0] and QWORD($400000))=0) then
 begin
  Result:=False;
 end;
end;

function sys_get_proc_type_info(dst:Pointer):Integer;
var
 info:t_proc_type_info;
begin
 info:=Default(t_proc_type_info);

 Result:=copyin(dst,@info.size,SizeOf(QWORD));
 if (Result<>0) then Exit;

 if (info.size<>SizeOf(t_proc_type_info)) then Exit(EINVAL);

 info.bptype:=p_proc.p_budget_ptype;

 if sceSblACMgrIsVideoplayerProcess(@g_authinfo) then
 begin
  info.pflags:=info.pflags or $04;
 end;

 if sceSblACMgrHasUseVideoServiceCapability(@g_authinfo) then
 begin
  info.pflags:=info.pflags or $10;
 end;

 if sceSblACMgrHasSceProgramAttribute(@g_authinfo) then
 begin
  info.pflags:=info.pflags or $40;
 end;

 if sceSblACMgrIsDebuggableProcess(@g_authinfo) then
 begin
  info.pflags:=info.pflags or $80;
 end;

 //sceSblACMgrIsJitCompilerProcess()         -> | 0x01
 //sceSblACMgrIsJitApplicationProcess()      -> | 0x02
 //sceSblACMgrIsVideoplayerProcess()         -> | 0x04
 //sceSblACMgrIsDiskplayeruiProcess()        -> | 0x08
 //sceSblACMgrHasUseVideoServiceCapability() -> | 0x10
 //sceSblACMgrIsWebcoreProcess()             -> | 0x20
 //sceSblACMgrHasSceProgramAttribute()       -> | 0x40
 //sceSblACMgrIsDebuggableProcess()          -> | 0x80

 Result:=copyout(@info,dst,SizeOf(t_proc_type_info));
end;

function sys_get_authinfo(pid:Integer;info:Pointer):Integer;
var
 data:t_authinfo;
 x,y:QWORD;
begin
 Result:=0;

 if (pid<>0) and
    (pid<>p_proc.p_pid) then
 begin
  Exit(ESRCH);
 end;

 data:=Default(t_authinfo);

 //if (priv_check(td,$2ae)=0) then
 //begin
 // data:=g_authinfo;
 //end else
 begin
  x:=g_authinfo.app_type;
  y:=x+QWORD($c7ffffffeffffffc);

  if (y < $f) and (((QWORD($6001) shr (y and $3f)) and 1)<>0) then
  begin
   data.app_type:=x;
  end;

  data.app_caps[0]:=g_authinfo.app_caps[0] and QWORD($7000000000000000);
 end;

 if (info<>nil) then
 begin
  Result:=copyout(@data,info,SizeOf(t_authinfo));
 end;
end;



end.

