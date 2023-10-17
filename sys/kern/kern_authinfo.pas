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
  app_type   :QWORD;
  app_flags  :QWORD; //62 bit IsSystemProcess;61 bit IsGameProcess1;60 bit IsGameProcess2;
  app_cap    :QWORD;
  unknow1    :array[0..1] of QWORD;
  sce_prg_atr:QWORD;
  unknow2    :array[0..10] of QWORD;
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

 //excp_flags
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
 SCE_LNC_APP_TYPE_INVALID =-1;
 SCE_LNC_APP_TYPE_SHELL_UI= 1;
 SCE_LNC_APP_TYPE_DAEMON  = 2;
 SCE_LNC_APP_TYPE_CDLG    = 3;
 SCE_LNC_APP_TYPE_MINI_APP= 4;
 SCE_LNC_APP_TYPE_BIG_APP = 5;

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

 p_appinfo=^t_appinfo;
 t_appinfo=packed record
  AppId           :Integer;       //4
  mmap_flags      :Integer;       //4
  attributeExe    :Integer;       //4
  AppType         :Integer;       //4  SceLncAppType
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

function sceSblACMgrHasUseHp3dPipeCapability(info:p_authinfo):Boolean;
function sceSblACMgrIsVideoplayerProcess(info:p_authinfo):Boolean;
function sceSblACMgrHasUseVideoServiceCapability(info:p_authinfo):Boolean;
function sceSblACMgrHasSceProgramAttribute(info:p_authinfo):Boolean;

function sys_get_proc_type_info(dst:Pointer):Integer;
function sys_get_authinfo(pid:Integer;info:Pointer):Integer;

implementation

uses
 errno,
 systm,
 kern_proc,
 md_proc;

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
 Result:=((info^.app_cap shr $39) and 1)<>0;
end;

function sceSblACMgrHasSceProgramAttribute(info:p_authinfo):Boolean;
var
 sce_prog_attr:QWORD;
begin
 sce_prog_attr:=info^.sce_prg_atr;
 if ((sce_prog_attr and $1000000)=0) then
 begin
  if ((sce_prog_attr and $2000000)<>0) then
  begin
   Exit(true);
  end;
  //sceSblRcMgrIsAllowULDebugger
 end;
 //sceSblRcMgrIsSoftwagnerQafForAcmgr
 Exit(false);
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
  info.pflags:=info.pflags or $80;
 end;

 //sceSblACMgrIsJitCompilerProcess()         -> | 0x01
 //sceSblACMgrIsJitApplicationProcess()      -> | 0x02
 //sceSblACMgrIsVideoplayerProcess()         -> | 0x04
 //sceSblACMgrIsDiskplayeruiProcess()        -> | 0x08
 //sceSblACMgrHasUseVideoServiceCapability() -> | 0x10
 //sceSblACMgrIsWebcoreProcess()             -> | 0x20
 //is_libkernel_sys()                        -> | 0x40
 //sceSblACMgrHasSceProgramAttribute()       -> | 0x80

 Result:=copyout(@info,dst,SizeOf(t_proc_type_info));
end;

function sys_get_authinfo(pid:Integer;info:Pointer):Integer;
var
 data:t_authinfo;
 x,y:QWORD;
begin
 Result:=0;

 if (pid<>0) and
    (pid<>g_pid) then
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

  data.app_flags:=g_authinfo.app_flags and QWORD($7000000000000000);
 end;

 if (info<>nil) then
 begin
  Result:=copyout(@data,info,SizeOf(t_authinfo));
 end;
end;



end.

