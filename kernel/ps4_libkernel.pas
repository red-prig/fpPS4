unit ps4_libkernel;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  RWLock,
  sys_types,
  ps4_map_mm,
  ps4_mspace,
  ps4_pthread,
  ps4_signal,
  ps4_mutex,
  ps4_cond,
  ps4_sema,
  ps4_rwlock,
  ps4_barrier,
  ps4_time,
  ps4_kernel_file,
  ps4_queue,
  ps4_event_flag,
  ps4_elf,
  ps4_program,

  //trace_manager,

  Classes,
  SysUtils;

function  ps4___error:Pointer; SysV_ABI_CDecl;
function  ps4_sceKernelError(i:Integer):Integer; SysV_ABI_CDecl;

function  ps4_sceKernelIsNeoMode:Integer; SysV_ABI_CDecl;
function  ps4_sceKernelGetCompiledSdkVersion(sdkVersion:PDWORD):Integer; SysV_ABI_CDecl;

implementation

uses
 sys_path,
 sys_kernel,
 sys_pthread,
 sys_signal,
 sys_dev;

type
 pSceKernelUuid=^SceKernelUuid;
 SceKernelUuid=TGuid;

function ps4_sceKernelUuidCreate(outUuid:pSceKernelUuid):Integer; SysV_ABI_CDecl;
begin
 if (outUuid=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 CreateGUID(outUuid^);
 _set_errno(0);
 Result:=0;
end;

function ps4___error:Pointer; SysV_ABI_CDecl;
begin
 Result:=_error;
end;

function ps4_sceKernelError(i:Integer):Integer; SysV_ABI_CDecl;
begin
 if (i=0) then
  Result:=0
 else
  Result:=i-$7ffe0000;
end;

Const
 _stack_chk_guard:QWORD=$deadbeefa55a857;
 ps4_stack_chk_guard:Pointer=@_stack_chk_guard;

procedure ps4_stack_chk_fail; SysV_ABI_CDecl;
begin
 Writeln(StdErr,GetCurrentThreadId,':Stack overflow detected! Aborting program.');
 DebugBreak;
end;

{$I StopNotificationReason.inc}

// eStopNotificationReason
procedure ps4_sceKernelDebugRaiseException(dwStopReason,dwStopId:DWORD); SysV_ABI_CDecl;
begin
 Writeln(StdErr,'RaiseException:',HexStr(dwStopReason,8),':',HexStr(dwStopId,8),':',GetStopReasonInfo(dwStopReason));
 DebugBreak;
end;

procedure ps4_sceKernelDebugRaiseExceptionOnReleaseMode(dwStopReason,dwStopId:DWORD); SysV_ABI_CDecl;
begin
 //skip
end;

procedure ps4_sceKernelDebugOutText(dbg_id:Integer;text:Pchar); SysV_ABI_CDecl;
begin
 Writeln(text);
end;

//ps4 neo mode is support? (Ps4 Pro)
function ps4_sceKernelIsNeoMode:Integer; SysV_ABI_CDecl;
begin
 Result:=0; //no
 //Result:=1; //yes
end;

//ps5? no
function ps4_sceKernelIsProspero:Integer; SysV_ABI_CDecl;
begin
 Result:=0; //no
end;

function ps4_sceKernelGetModuleInfoFromAddr(Addr:Pointer;flags:DWORD;info:pSceKernelModuleInfoEx):Integer; SysV_ABI_CDecl;
var
 node:TElf_node;
begin
 if (info=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);

 if (flags - 1 < 2) then
 begin
  if (info=nil) then
  begin
   Result:=SCE_KERNEL_ERROR_EFAULT;
  end else
  begin

   _sig_lock;

   Writeln('GetModuleInfoFromAddr:',HexStr(Addr),':',flags,':',HexStr(info));
   node:=ps4_app.AcqureFileByCodeAdr(Addr);

   if (node=nil) then
   begin
    _sig_unlock;

    info^:=Default(SceKernelModuleInfoEx);
    Exit(SCE_KERNEL_ERROR_ESRCH);
   end;

   info^:=node.GetModuleInfoEx;

   node.Release;

   _sig_unlock;
   Result:=0;

  end;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_EINVAL;
  info^:=Default(SceKernelModuleInfoEx);
 end;
end;

function ps4_sceKernelGetModuleInfo(handle:Integer;info:pSceKernelModuleInfo):Integer; SysV_ABI_CDecl;
var
 node:TElf_node;
begin
 if (info=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);
 _sig_lock;
 Writeln('sceKernelGetModuleInfo:',handle,':',HexStr(info));
 node:=ps4_app.AcqureFileByHandle(handle);
 if (node=nil) then
 begin
  _sig_unlock;
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;
 info^:=node.GetModuleInfo;
 node.Release;
 _sig_unlock;
 Result:=0;
end;

function ps4_sceKernelGetModuleInfoForUnwind(addr:Pointer;flags:DWORD;info:pSceModuleInfoForUnwind):Integer; SysV_ABI_CDecl;
var
 node:TElf_node;
 info_ex:SceKernelModuleInfoEx;
begin
 if (info=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);
 if (flags - 1 < 2) then
 begin
  if (info=nil) then
  begin
   Result:=SCE_KERNEL_ERROR_EFAULT;
  end else
  begin
   Result:=SCE_KERNEL_ERROR_EINVAL;
   if (info^.st_size > 303) then
   begin

    _sig_lock;

    Writeln('sceKernelGetModuleInfoForUnwind:',HexStr(Addr),':',flags,':',HexStr(info));
    node:=ps4_app.AcqureFileByCodeAdr(Addr);

    if (node=nil) then
    begin
     _sig_unlock;

     info^:=Default(SceModuleInfoForUnwind);
     Exit(SCE_KERNEL_ERROR_ESRCH);
    end;

    info_ex:=node.GetModuleInfoEx;

    node.Release;

    _sig_unlock;

    info^.name             :=info_ex.name;
    info^.eh_frame_hdr_addr:=info_ex.eh_frame_hdr_addr;
    info^.eh_frame_addr    :=info_ex.eh_frame_addr;
    info^.eh_frame_size    :=info_ex.eh_frame_size;
    info^.seg0_addr        :=info_ex.segments[0].address;
    info^.seg0_size        :=info_ex.segments[0].size;

    Result:=0;
   end;
  end;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_EINVAL;
  info^:=Default(SceModuleInfoForUnwind);
 end;
end;

function ps4_sceSysmoduleGetModuleInfoForUnwind(addr:Pointer;flags:DWORD;info:pSceModuleInfoForUnwind):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceKernelGetModuleInfoForUnwind(addr,flags,info);
end;

function ps4_sceKernelGetLibkernelTextLocation(address:PPointer;size:PQWORD):Integer; SysV_ABI_CDecl;
var
 elf:Telf_file;
begin
 Result:=0;
 elf:=Telf_file(ps4_program.ps4_app.prog);
 if (elf=nil) then Exit(-1);

 address^:=elf.ModuleInfo.segmentInfo[0].address;
 size   ^:=elf.ModuleInfo.segmentInfo[0].size;
end;

type
 PInternalSegmentInfo=^TInternalSegmentInfo;
 TInternalSegmentInfo=packed record
  address:Pointer;
  size:QWORD;
 end;

function ps4_sceKernelInternalMemoryGetModuleSegmentInfo(pOut:PInternalSegmentInfo):Integer; SysV_ABI_CDecl;
begin
 //pOut^.address:=nil;
 //pOut^.size:=0;
 //Result:=0;
 Result:=ps4_sceKernelGetLibkernelTextLocation(@pOut^.address,@pOut^.size)
end;

function ps4_sceKernelGetProcParam:PSceProcParam; SysV_ABI_CDecl;
begin
 Writeln('KernelGetProcParam');
 Result:=GetSceProcParam;
end;

type
 PAppHeapAPI=^TAppHeapAPI;
 TAppHeapAPI=packed record
  malloc:Pointer;
  free  :Pointer;
  unknow:array[0..3] of Pointer;
  posix_memalign:Pointer;
 end;

procedure ps4__sceKernelRtldSetApplicationHeapAPI(heap_api:PAppHeapAPI); SysV_ABI_CDecl;
begin
 Writeln('SetApplicationHeapAPI:',HexStr(heap_api));
 Writeln('               malloc:',HexStr(heap_api^.malloc));
 Writeln('                 free:',HexStr(heap_api^.free));
 Writeln('       posix_memalign:',HexStr(heap_api^.posix_memalign));
end;

//cb used in pthread_exit
function ps4_sceKernelSetThreadDtors(Proc:TProcedure):Integer; SysV_ABI_CDecl;
begin
 sceKernelThreadDtors:=Proc;
 Writeln('sceKernelSetThreadDtors:',HexStr(proc));
 Result:=0;
end;

//cb used in sceKernelStopUnloadModule
function ps4_sceKernelSetThreadAtexitCount(proc:TKernelAtexitFuncCount):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceKernelSetThreadAtexitCount:',HexStr(proc));
 Result:=0;
end;

//cb used in sceKernelStopUnloadModule
function ps4_sceKernelSetThreadAtexitReport(proc:TKernelAtexitReportFunc):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceKernelSetThreadAtexitReport:',HexStr(proc));
 Result:=0;
end;

//extern "C" {
//int user_malloc_init(void);
//int user_malloc_finalize(void);
//void *user_malloc(size_t size);
//void user_free(void *ptr);
//void *user_calloc(size_t nelem, size_t size);
//void *user_realloc(void *ptr, size_t size);
//void *user_memalign(size_t boundary, size_t size);
//int user_posix_memalign(void **ptr, size_t boundary, size_t size);
//void *user_reallocalign(void *ptr, size_t size, size_t boundary);
//int user_malloc_stats(SceLibcMallocManagedSize *mmsize);
//int user_malloc_stats_fast(SceLibcMallocManagedSize *mmsize);
//size_t user_malloc_usable_size(void *ptr);
//}

function ps4_sceKernelGetSanitizerMallocReplaceExternal:Pointer; SysV_ABI_CDecl;
begin
 //sceKernelDlsym: libSceDbgAddressSanitizer.prx->__asan_malloc_replace_external
 Result:=nil;
end;

function ps4_sceKernelGetSanitizerNewReplaceExternal:Pointer; SysV_ABI_CDecl;
begin
 //sceKernelDlsym: libSceDbgAddressSanitizer.prx->__asan_new_replace_external
 Result:=nil;
end;

function ps4_sceKernelGetSanitizerMallocReplace:Pointer; SysV_ABI_CDecl;
begin
 //sceKernelDlsym: libSceDbgAddressSanitizer.prx->__asan_malloc_replace
 Result:=nil;
end;

function ps4_sceKernelGetSanitizerNewReplace:Pointer; SysV_ABI_CDecl;
begin
 //sceKernelDlsym: libSceDbgAddressSanitizer.prx->__asan_new_replace
 Result:=nil;
end;

function ps4_sceKernelIsAddressSanitizerEnabled:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceKernelMapSanitizerShadowMemory(addr:Pointer;len:qword;flags:DWORD;name:Pchar):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
end;

function ps4_sceKernelGetCompiledSdkVersion(sdkVersion:PDWORD):Integer; SysV_ABI_CDecl;
var
 P:PSceProcParam;
begin
 Result:=SCE_KERNEL_ERROR_EINVAL;
 if (sdkVersion=nil) then Exit;
 P:=GetSceProcParam;

 if (P<>nil) then
 if (P^.Header.Size>$13) then
 if (P^.Header.Magic=$4942524f) then
 if (P^.Header.Entry_count<>0) then
 begin
  sdkVersion^:=P^.Header.SDK_version;
  Result:=0;
 end;
end;

const
 //eLoadOptions
 LOAD_OPTIONS_DEFAULT                         =$0000;
 LOAD_OPTIONS_LOAD_SUSPENDED                  =$0001;
 LOAD_OPTIONS_USE_SYSTEM_LIBRARY_VERIFICATION =$0002;
 LOAD_OPTIONS_SLV_MODE_WARN                   =$0004;
 LOAD_OPTIONS_ARG_STACK_SIZE                  =$0008;
 LOAD_OPTIONS_FULL_DEBUG_REQUIRED             =$0010;

type
 PSCE_APP_ENV=^TSCE_APP_ENV;
 TSCE_APP_ENV=packed record
  unk1:array[0..15] of Byte; //16
  ustr:array[0.. 9] of char; //10
  unk2:array[0..37] of Byte; //38
  flags:Byte;                //1  eLoadOptions
  unk3:array[0.. 6] of Byte; //7
 end;

//sysctl to KERN_PROC_ENV
function ps4_sceKernelGetAppInfo(pid:Integer;env:PSCE_APP_ENV):Integer; SysV_ABI_CDecl;
begin
 //ignore pid
 Result:=0;
 if (env=nil) then
 begin
  _set_errno(EINVAL);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 env^:=Default(TSCE_APP_ENV);
end;

//dynlib_get_obj_member(handle,8,&ptr); module param
//dynlib_get_obj_member(handle,1,&ptr); init
//dynlib_get_obj_member(handle,2,&ptr); fini

function _sceKernelLoadStartModule(moduleFileName:Pchar;
                                   argc:size_t;
                                   argp:PPointer;
                                   flags:DWORD;
                                   pOpt:PSceKernelLoadModuleOpt;
                                   pRes:PInteger):SceKernelModule;
var
 node:TElf_node;
 fn:RawByteString;
 i:Integer;
begin
 Result:=0;
 if (pOpt<>nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 Writeln('Load Lib:',moduleFileName);

 Result:=parse_filename(moduleFileName,fn);

 Case Result of
  PT_FILE:;
  else
          Exit(_set_sce_errno(SCE_KERNEL_ERROR_EACCES));
 end;

 node:=ps4_app.AcqureFileByName(ExtractFileName(fn));
 if (node<>nil) then
 begin
  Writeln('File Loaded:',ExtractFileName(fn));

  Result:=node.Handle;
  node.Release;

  if (pRes<>nil) then pRes^:=0;

  Exit;
 end;

 Writeln('Load File:',fn);

 node:=LoadPs4ElfFromFile(fn);
 if (node<>nil) then
 begin
  node.IsStatic:=False;
  node.Acqure;

  node.Prepare;
  if not ps4_app.RegistredElf(node) then Assert(false,'RegistredElf');
  ps4_app.ResolveDepended(node);
  ps4_app.LoadSymbolImport(nil);
  ps4_app.ReLoadSymbolImport(Pointer(node));
  ps4_app.InitProt;
  ps4_app.InitThread(0);

  node.IsInit:=True; //mark exclude

  ps4_app.InitCode;

  i:=node.module_start(argc,argp,nil);

  Result:=node.Handle;
  node.Release;

  if (pRes<>nil) then pRes^:=i;

  if (i<0) then Result:=SCE_KERNEL_ERROR_EINVAL;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_ENOENT;
 end;

 _set_sce_errno(Result);
end;

function ps4_sceKernelLoadStartModule(moduleFileName:Pchar;
                                      argc:size_t;
                                      argp:PPointer;
                                      flags:DWORD;
                                      pOpt:PSceKernelLoadModuleOpt;
                                      pRes:PInteger):SceKernelModule; SysV_ABI_CDecl;
begin
 if (flags<>0) then Exit(SCE_KERNEL_ERROR_EINVAL);

 _sig_lock;

 Result:=_sceKernelLoadStartModule(moduleFileName,
                                   argc,
                                   argp,
                                   0,
                                   pOpt,
                                   pRes);

 _sig_unlock;
end;

Function ps4_sceKernelDlsym(handle:Integer;symbol:PChar;addrp:PPointer):Integer; SysV_ABI_CDecl;
var
 node:TElf_node;
 p:Pointer;
begin
 Result:=0;
 if (addrp=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);

 _sig_lock;

 Writeln('sceKernelDlsym:',symbol);

 node:=ps4_app.AcqureFileByHandle(handle);
 if (node=nil) then
 begin
  _sig_unlock;
  Exit(SCE_KERNEL_ERROR_ESRCH);
 end;

 p:=node.get_proc_by_name(symbol);

 if (p<>nil) then
 begin
  addrp^:=p;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_EFAULT;
 end;

 node.Release;
 _sig_unlock;
end;

Function ps4_sceKernelGetModuleList(list:PInteger;numArray:QWORD;actualNum:PQWORD):Integer; SysV_ABI_CDecl;
var
 i:QWORD;
 node:TElf_node;
begin
 Result:=0;
 if (list=nil) or (actualNum=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);

 _sig_lock;
 ps4_app.LockRd;

 i:=0;
 node:=ps4_app.FirstFile;
 While (node<>nil) do
 begin
  if (i<numArray) then
  begin
   list[i]:=node.Handle;
  end;

  Inc(i);
  node:=node.Next;
 end;

 ps4_app.Unlock;
 _sig_unlock;

 actualNum^:=i;
 if (i>numArray) then Result:=SCE_KERNEL_ERROR_ENOMEM;

 Writeln('sceKernelGetModuleList:',HexStr(list),' ',numArray,' ',i);
end;

const
//
//For CPU mode
//  bit 0: 7/6 CPU mode flag 1: 7 CPU mode 0: 6 CPU mode
//  bit 1: Reserved
//  bit 2: CPU #6 flag 1: normal 0: low
//
 SCE_KERNEL_CPUMODE_6CPU       =0;
 SCE_KERNEL_CPUMODE_7CPU_LOW   =1;
 SCE_KERNEL_CPUMODE_7CPU_NORMAL=5;

function ps4_sceKernelGetCpumode():Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_KERNEL_CPUMODE_7CPU_NORMAL;
end;

//wtf it mean?
function ps4_sceKernelSetFsstParam(prio:Integer;
                                   mask:QWORD //SceKernelCpumask
                                   ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

{$I libsysmodule.inc}

function ps4_sceSysmoduleLoadModule(id:Word):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleLoadModule:',GetSysmoduleName(id));
 Result:=0;
end;

function ps4_sceSysmoduleUnloadModule(id:Word):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleUnloadModule:',GetSysmoduleName(id));
 Result:=0;
end;

function ps4_sceSysmoduleIsLoaded(id:Word):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleIsLoaded:',GetSysmoduleName(id));
 Result:=0;
end;

//

function ps4_sceSysmoduleLoadModuleInternalWithArg(id:Word;
                                                   argc:size_t;
                                                   argp:PPointer;
                                                   flags:DWORD;
                                                   pRes:PInteger):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleLoadModuleInternalWithArg:',GetSysmoduleName(id));
 if (pRes<>nil) then pRes^:=0;
 Result:=0;
end;

const
 __progname:PChar='eboot.bin'; //argv[0]

function Load_libSceSysmodule(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSysmodule');

 lib^.set_proc($83C70CDFD11467AA,@ps4_sceSysmoduleLoadModule);
 lib^.set_proc($791D9B6450005344,@ps4_sceSysmoduleUnloadModule);
 lib^.set_proc($7CC3F934750E68C9,@ps4_sceSysmoduleIsLoaded);

 lib^.set_proc($847AC6A06A0D7FEB,@ps4_sceSysmoduleLoadModuleInternalWithArg);

 lib^.set_proc($E1F539CAF3A4546E,@ps4_sceSysmoduleGetModuleInfoForUnwind);
end;

procedure _kernel_init;
begin
 ps4_sceKernelGetCompiledSdkVersion(@SDK_VERSION);
 _mem_init;
 _sys_dev_init;
 ps4_malloc_init;
end;

function Load_libkernel(Const name:RawByteString):TElf_node;
var
 lib,px:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libkernel');

 lib^.set_proc($BCD7B5C387622C2B,@_dynamic_tls_get_addr);

 lib^.set_proc($763C713A65BAFDAC,@__progname);
 lib^.set_proc($F41703CA43E6A352,@ps4___error);
 lib^.set_proc($0F8CA56B7BF1E2D6,@ps4_sceKernelError);
 lib^.set_proc($7FBB8EC58F663355,@ps4_stack_chk_guard);
 lib^.set_proc($3AEDE22F569BBE78,@ps4_stack_chk_fail);
 lib^.set_proc($91BC385071D2632D,@ps4_pthread_cxa_finalize);

 lib^.set_proc($5E3A28B22C3E5CF2,@ps4_sceKernelUuidCreate);

 //signal

 lib^.set_proc($5644C0B2B643709D,@ps4_sigfillset);
 lib^.set_proc($2548A616D29ED0A7,@ps4_sigaddset);
 lib^.set_proc($68F732A6D6CE899B,@ps4_sigprocmask); //sigprocmask
 lib^.set_proc($EB1569CB415DABE2,@ps4_sigprocmask); //_sigprocmask
 lib^.set_proc($2592B0E7E5AB9DAC,@ps4_pthread_sigmask);
 lib^.set_proc($72B6F98FB9A49357,@ps4_is_signal_return);
 lib^.set_proc($2A22443C4591C946,@ps4_sigaction);
 lib^.set_proc($5400DCDCC350DDC3,@ps4_signal_);

 lib^.set_proc($38C0D128A019F08E,@ps4_sceKernelDebugRaiseException);
 lib^.set_proc($CC4FF05C86632E83,@ps4_sceKernelDebugRaiseExceptionOnReleaseMode);
 lib^.set_proc($F4960DA8DEA300A2,@ps4_sceKernelDebugOutText);

 //signal
 lib^.set_proc($0262749A7DA5E253,@ps4_sceKernelGetLibkernelTextLocation);
 lib^.set_proc($FD84D6FAA5DCDC24,@ps4_sceKernelInternalMemoryGetModuleSegmentInfo);
 lib^.set_proc($7FB28139A7F2B17A,@ps4_sceKernelGetModuleInfoFromAddr);
 lib^.set_proc($914A60AD722BCFB4,@ps4_sceKernelGetModuleInfo);
 lib^.set_proc($4694092552938853,@ps4_sceKernelGetModuleInfoForUnwind);
 lib^.set_proc($F79F6AADACCF22B8,@ps4_sceKernelGetProcParam);
 lib^.set_proc($A7911C41E11E2401,@ps4__sceKernelRtldSetApplicationHeapAPI);
 lib^.set_proc($ACD856CFE96F38C5,@ps4_sceKernelSetThreadDtors);
 lib^.set_proc($A41FF2199DA743DA,@ps4_sceKernelSetThreadAtexitCount);
 lib^.set_proc($5A109CD70DC48522,@ps4_sceKernelSetThreadAtexitReport);

 lib^.set_proc($A72E8BF2389500DF,@ps4_sceKernelGetSanitizerMallocReplaceExternal);
 lib^.set_proc($6E7671620005780D,@ps4_sceKernelGetSanitizerNewReplaceExternal);
 lib^.set_proc($6EDD0F38451975D1,@ps4_sceKernelGetSanitizerMallocReplace);
 lib^.set_proc($1782A26F731BD302,@ps4_sceKernelGetSanitizerNewReplace);
 lib^.set_proc($8E1FBC5E22B82DE1,@ps4_sceKernelIsAddressSanitizerEnabled);
 lib^.set_proc($F1C0250B3A0E8A27,@ps4_sceKernelMapSanitizerShadowMemory);

 lib^.set_proc($581EBA7AFBBC6EC5,@ps4_sceKernelGetCompiledSdkVersion);
 lib^.set_proc($1BF318BF97AB5DA5,@ps4_sceKernelGetAppInfo);

 lib^.set_proc($C33BEA4F852A297F,@ps4_sceKernelLoadStartModule);
 lib^.set_proc($22EC6752E5E4E818,@ps4_sceKernelGetModuleList);
 lib^.set_proc($2F01BC8379E2AB00,@ps4_sceKernelDlsym);

 lib^.set_proc($54EC7C3469875D3B,@ps4_sceKernelGetCpumode);
 lib^.set_proc($56306D83906D97DE,@ps4_sceKernelSetFsstParam);

 //mutex

 lib^.set_proc($7501D612C26DA04E,@ps4_pthread_mutexattr_init);
 lib^.set_proc($1C5EE52B8EB1CE36,@ps4_pthread_mutexattr_destroy);
 lib^.set_proc($19916523B461B90A,@ps4_pthread_mutexattr_gettype);
 lib^.set_proc($9839A030E19552A8,@ps4_pthread_mutexattr_settype);

 lib^.set_proc($3E62FF4F0294CD72,@ps4_pthread_mutexattr_getpshared);
 lib^.set_proc($117BF7CED1AAB433,@ps4_pthread_mutexattr_setpshared);
 lib^.set_proc($C83696C54139D2CD,@ps4_pthread_mutexattr_getprotocol);
 lib^.set_proc($E6DC4A7DC3140289,@ps4_pthread_mutexattr_setprotocol);
 lib^.set_proc($FA6F3EAAEA8EC213,@ps4_pthread_mutexattr_getprioceiling);
 lib^.set_proc($64BBDFEA55407383,@ps4_pthread_mutexattr_setprioceiling);

 lib^.set_proc($B6D1CD7D4FAA0C15,@ps4_pthread_mutex_init);
 lib^.set_proc($96D09F686AF62461,@ps4_pthread_mutex_destroy);
 lib^.set_proc($EC7D224CE7224CBA,@ps4_pthread_mutex_lock);
 lib^.set_proc($D99F8FA58E826898,@ps4_pthread_mutex_unlock);
 lib^.set_proc($228F7E9D329766D0,@ps4_pthread_mutex_timedlock);
 lib^.set_proc($2BF8D785BB76827E,@ps4_pthread_mutex_trylock);

 lib^.set_proc($17C6D41F0006DBCE,@ps4_scePthreadMutexattrInit);
 lib^.set_proc($B2658492D8B2C86D,@ps4_scePthreadMutexattrDestroy);
 lib^.set_proc($82AB84841AD2DA2C,@ps4_scePthreadMutexattrGettype);
 lib^.set_proc($88CA7C42913E5CEE,@ps4_scePthreadMutexattrSettype);
 lib^.set_proc($1A84E615EBA2FA14,@ps4_scePthreadMutexattrGetprotocol);
 lib^.set_proc($D451AF5348BDB1A4,@ps4_scePthreadMutexattrSetprotocol);
 lib^.set_proc($4A08CCA721FD67D2,@ps4_scePthreadMutexattrGetprioceiling);
 lib^.set_proc($E77D8869082EC0C8,@ps4_scePthreadMutexattrSetprioceiling);

 lib^.set_proc($726A3544862F6BDA,@ps4_scePthreadMutexInit);
 lib^.set_proc($D8E7F47FEDE68611,@ps4_scePthreadMutexDestroy);
 lib^.set_proc($F542B5BCB6507EDE,@ps4_scePthreadMutexLock);
 lib^.set_proc($21A7C8D8FC5C3E74,@ps4_scePthreadMutexTimedlock);
 lib^.set_proc($B67DD5943D211BAD,@ps4_scePthreadMutexUnlock);
 lib^.set_proc($BA9A15AF330715E1,@ps4_scePthreadMutexTrylock);

 //mutex

 //rwlock

 lib^.set_proc($C4579BB00E18B052,@ps4_pthread_rwlockattr_init);
 lib^.set_proc($AAC7668178EA4A09,@ps4_pthread_rwlockattr_destroy);
 lib^.set_proc($97E6C6E5FB189218,@ps4_pthread_rwlockattr_gettype_np);
 lib^.set_proc($F0DB8E1E24EBD55C,@ps4_pthread_rwlockattr_settype_np);
 lib^.set_proc($56A10CB82BFFA876,@ps4_pthread_rwlockattr_getpshared);
 lib^.set_proc($3AE2A0FA44430FB5,@ps4_pthread_rwlockattr_setpshared);

 lib^.set_proc($CAD4142CDFE784BE,@ps4_pthread_rwlock_init);
 lib^.set_proc($D78EF56A33F3C61D,@ps4_pthread_rwlock_destroy);
 lib^.set_proc($8868ECAF5580B48D,@ps4_pthread_rwlock_rdlock);
 lib^.set_proc($B08951BD0AAC3766,@ps4_pthread_rwlock_wrlock);
 lib^.set_proc($485C5330E7EE0A41,@ps4_pthread_rwlock_tryrdlock);
 lib^.set_proc($5E15879FA3F947B5,@ps4_pthread_rwlock_trywrlock);
 lib^.set_proc($12098BA3A11682CA,@ps4_pthread_rwlock_unlock);

 lib^.set_proc($C8E7C683F2356482,@ps4_scePthreadRwlockattrInit);
 lib^.set_proc($8B689F6777D2D9FA,@ps4_scePthreadRwlockattrDestroy);
 lib^.set_proc($2B296CD42845CAB7,@ps4_scePthreadRwlockattrGettype);
 lib^.set_proc($87F3A27E2A2E05DF,@ps4_scePthreadRwlockattrSettype);

 lib^.set_proc($E942C06B47EAE230,@ps4_scePthreadRwlockInit);
 lib^.set_proc($041FA46F4F1397D0,@ps4_scePthreadRwlockDestroy);
 lib^.set_proc($3B1F62D1CECBE70D,@ps4_scePthreadRwlockRdlock);
 lib^.set_proc($9AA74DA2BAC1FA02,@ps4_scePthreadRwlockWrlock);
 lib^.set_proc($5C3DE60DEC9B0A79,@ps4_scePthreadRwlockTryrdlock);
 lib^.set_proc($6C81E86424E89AC2,@ps4_scePthreadRwlockTrywrlock);
 lib^.set_proc($F8BF7C3C86C6B6D9,@ps4_scePthreadRwlockUnlock);

 //rwlock

 //Sema

 lib^.set_proc($A43B8F11FDE6E1F2,@ps4_sem_init);
 lib^.set_proc($7035B6DF7440C16A,@ps4_sem_destroy);
 lib^.set_proc($06AF8B455FCDE879,@ps4_sem_getvalue);
 lib^.set_proc($20A3FCB72A744149,@ps4_sem_post);
 lib^.set_proc($C39207CAF6A183FA,@ps4_sem_timedwait);
 lib^.set_proc($5815B3B1189F0840,@ps4_sem_trywait);
 lib^.set_proc($602579746181702A,@ps4_sem_wait);

 lib^.set_proc($D7CF31E7B258A748,@ps4_sceKernelCreateSema);
 lib^.set_proc($47526F9FC6D2096F,@ps4_sceKernelDeleteSema);
 lib^.set_proc($6716B45614154EC9,@ps4_sceKernelWaitSema);
 lib^.set_proc($E1CCE9A47062AE2C,@ps4_sceKernelSignalSema);
 lib^.set_proc($D76C0E1E4F32C1BD,@ps4_sceKernelPollSema);
 lib^.set_proc($E03334E94D813446,@ps4_sceKernelCancelSema);

 //Sema

 //cond

 lib^.set_proc($98AA13C74DC74560,@ps4_pthread_condattr_init);
 lib^.set_proc($74972E4159FAFC8C,@ps4_pthread_condattr_destroy);

 lib^.set_proc($7130D8C5350D3E13,@ps4_pthread_condattr_getclock);
 lib^.set_proc($123965680A803D9A,@ps4_pthread_condattr_setclock);
 lib^.set_proc($874A94A92B8E982F,@ps4_pthread_condattr_getpshared);
 lib^.set_proc($DC1A4FF39D21053E,@ps4_pthread_condattr_setpshared);

 lib^.set_proc($D13C959383122EDD,@ps4_pthread_cond_init);
 lib^.set_proc($4575EA8B80AD17CC,@ps4_pthread_cond_destroy);

 lib^.set_proc($D8C3B2FAB51FBA14,@ps4_pthread_cond_signal);
 lib^.set_proc($9A4C767D584D32C8,@ps4_pthread_cond_broadcast);

 lib^.set_proc($3A9F130466392878,@ps4_pthread_cond_wait);
 lib^.set_proc($DBB6C08222663A1D,@ps4_pthread_cond_timedwait);

 lib^.set_proc($9B9FF66EC35FBFBB,@ps4_scePthreadCondattrInit);
 lib^.set_proc($C1A3DCC58891DD60,@ps4_scePthreadCondattrDestroy);

 lib^.set_proc($D936FDDAABA9AE5D,@ps4_scePthreadCondInit);
 lib^.set_proc($83E3D977686269C8,@ps4_scePthreadCondDestroy);

 lib^.set_proc($90387F35FC6032D1,@ps4_scePthreadCondSignal);
 lib^.set_proc($58A0172785C13D0E,@ps4_scePthreadCondWait);
 lib^.set_proc($06632363199EC35C,@ps4_scePthreadCondTimedwait);
 lib^.set_proc($246823ED4BEB97E0,@ps4_scePthreadCondBroadcast);

 //cond

 //barrier

 lib^.set_proc($66C5CB16D7768EA4,@ps4_pthread_barrier_init);
 lib^.set_proc($F8FAAE6FD1D908FA,@ps4_pthread_barrier_destroy);
 lib^.set_proc($09AC1980262A5D69,@ps4_pthread_barrier_wait);

 //barrier

 //thread

 lib^.set_proc($9EC628351CB0C0D8,@ps4_scePthreadAttrInit);
 lib^.set_proc($EB6282C04326CDC3,@ps4_scePthreadAttrDestroy);
 lib^.set_proc($C2D92DFED791D6CA,@ps4_pthread_attr_init);
 lib^.set_proc($CC772163C7EDE699,@ps4_pthread_attr_destroy);
 lib^.set_proc($5135F325B5A18531,@ps4_scePthreadAttrSetstacksize);
 lib^.set_proc($D90D33EAB9C1AD31,@ps4_pthread_attr_setstacksize);
 lib^.set_proc($FD6ADEA6BB6ED10B,@ps4_scePthreadAttrSetdetachstate);
 lib^.set_proc($13EB72A37969E4BC,@ps4_pthread_attr_setdetachstate);
 lib^.set_proc($E3E87D133C0A1782,@ps4_scePthreadAttrSetschedpolicy);
 lib^.set_proc($0F3112F61405E1FE,@ps4_scePthreadAttrSetschedparam);
 lib^.set_proc($1573D61CD93C39FD,@ps4_scePthreadAttrGetschedparam);
 lib^.set_proc($7AE291826D159F63,@ps4_pthread_attr_setschedparam);
 lib^.set_proc($AA593DA522EC5263,@ps4_pthread_attr_getschedparam);
 lib^.set_proc($DEAC603387B31130,@ps4_scePthreadAttrSetaffinity);
 lib^.set_proc($F3EB39073663C528,@ps4_scePthreadAttrGetaffinity);
 lib^.set_proc($7976D44A911A4EC0,@ps4_scePthreadAttrSetinheritsched);
 lib^.set_proc($B711ED9E027E7B27,@ps4_scePthreadAttrGetguardsize);
 lib^.set_proc($46EDFA7E24ED2730,@ps4_scePthreadAttrGetstackaddr);
 lib^.set_proc($FDF03EED99460D0B,@ps4_scePthreadAttrGetstacksize);
 lib^.set_proc($FEAB8F6B8484254C,@ps4_scePthreadAttrGetstack);
 lib^.set_proc($25A44CCBE41CA5E5,@ps4_scePthreadAttrGetdetachstate);
 lib^.set_proc($5544F5652AC74F42,@ps4_pthread_attr_getdetachstate);

 lib^.set_proc($C755FBE9AAD83315,@ps4_scePthreadAttrGet);

 lib^.set_proc($2668BEF70F6ED04E,@ps4_pthread_create_name_np);
 lib^.set_proc($3B184807C2C1FCF4,@ps4_pthread_create);
 lib^.set_proc($E9482DC15FB4CDBE,@ps4_scePthreadCreate);

 lib^.set_proc($E2A1AB47A7A83FD6,@ps4_scePthreadDetach);
 lib^.set_proc($F94D51E16B57BE87,@ps4_pthread_detach);

 lib^.set_proc($A27358F41CA7FD6F,@ps4_scePthreadJoin);
 lib^.set_proc($87D09C3F7274A153,@ps4_pthread_join);

 lib^.set_proc($678428B15B80B00D,@ps4_pthread_once);
 lib^.set_proc($D786CE00200D4C1A,@ps4_scePthreadOnce);

 lib^.set_proc($DCFB55EA9DD0357E,@ps4_scePthreadEqual);
 lib^.set_proc($ED7976E7B33854D2,@ps4_pthread_equal);

 lib^.set_proc($DE483BAD3D0D408B,@ps4_scePthreadExit);
 lib^.set_proc($149AD3E4BB940405,@ps4_pthread_exit);

 lib^.set_proc($959CC5792C4F974F,@ps4_pthread_setcancelstate);

 lib^.set_proc($128B51F1ADC049FE,@ps4_pthread_self);
 lib^.set_proc($688F8E782CFCC6B4,@ps4_scePthreadSelf);

 lib^.set_proc($1E82D558D6A70417,@ps4_getpid);
 lib^.set_proc($108FF9FE396AD9D1,@ps4_scePthreadGetthreadid);

 lib^.set_proc($1E8C3B07C39EB7A9,@ps4_scePthreadGetname);
 lib^.set_proc($181518EF2C1D50B1,@ps4_scePthreadRename);

 lib^.set_proc($6EDDC24C12A61B22,@ps4_scePthreadSetaffinity);
 lib^.set_proc($ADCAD5149B105916,@ps4_scePthreadGetaffinity);

 lib^.set_proc($8345530717C9CAED,@ps4_sceKernelGetCurrentCpu);

 lib^.set_proc($D6D2B21BB465309A,@ps4_scePthreadGetprio);
 lib^.set_proc($5B41E99B65F4B8F1,@ps4_scePthreadSetprio);

 lib^.set_proc($3F8D644D6512DC42,@ps4_scePthreadGetschedparam);
 lib^.set_proc($A084454E3A082DB8,@ps4_scePthreadSetschedparam);

 lib^.set_proc($08136D5CEA1E7FF1,@ps4_sched_get_priority_max);
 lib^.set_proc($9B4892EA336C5DDB,@ps4_sched_get_priority_min);

 lib^.set_proc($4FBDA1CFA7DFAB4F,@ps4_scePthreadYield);
 lib^.set_proc($0791A65432B0A67D,@ps4_pthread_yield);
 lib^.set_proc($E971B8077DCDD3D8,@ps4_sched_yield);

 lib^.set_proc($E1979959C32C015D,@ps4_pthread_cleanup_push);
 lib^.set_proc($455C5BD12B1AE6DD,@ps4_pthread_cleanup_pop);
 lib^.set_proc($D71BED515C75FD28,@ps4___pthread_cleanup_push_imp);
 lib^.set_proc($896B0595831FDCAC,@ps4___pthread_cleanup_pop_imp);

 lib^.set_proc($9AA50B35D8A64E7D,@ps4_pthread_key_create);
 lib^.set_proc($E81A4466E0D3ED82,@ps4_pthread_key_delete);
 lib^.set_proc($D3F297692EF4C72E,@ps4_pthread_getspecific);
 lib^.set_proc($5AB38BBC7534C903,@ps4_pthread_setspecific);

 //thread

 lib^.set_proc($5AC95C2B51507062,@ps4_sceKernelIsNeoMode);
 lib^.set_proc($9A9C4076A5BB74A6,@ps4_sceKernelIsProspero);

 //mmap

 lib^.set_proc($93E017AAEDBF7817,@ps4_getpagesize);
 lib^.set_proc($A4EF7A4F0CCE9B91,@ps4_sceKernelGetDirectMemorySize);
 lib^.set_proc($68DCF5D5F9E7CE2E,@ps4_sceKernelAvailableFlexibleMemorySize);

 lib^.set_proc($AD35F0EB9C662C80,@ps4_sceKernelAllocateDirectMemory);
 lib^.set_proc($07EBDCD803B666B7,@ps4_sceKernelAllocateMainDirectMemory);
 lib^.set_proc($0B47FB4C971B7DA7,@ps4_sceKernelAvailableDirectMemorySize);
 lib^.set_proc($047A2E2D0CE1D17D,@ps4_sceKernelDirectMemoryQuery);
 lib^.set_proc($042F8E1B99BDF9BC,@ps4_sceKernelGetDirectMemoryType);
 lib^.set_proc($8705523C29A9E6D3,@ps4_sceKernelCheckedReleaseDirectMemory);
 lib^.set_proc($301B88B6F6DAEB3F,@ps4_sceKernelReleaseDirectMemory);

 lib^.set_proc($04F13DB3DBD0417A,@ps4_mmap);
 lib^.set_proc($3C68501DDFDDCEFF,@ps4_sceKernelMmap);
 lib^.set_proc($52A0C68D7039C943,@ps4_munmap);
 lib^.set_proc($71091EF54B8140E9,@ps4_sceKernelMunmap);
 lib^.set_proc($B5E888B4BD9BA05C,@ps4_sceKernelReleaseFlexibleMemory);
 lib^.set_proc($61039FC4BE107DE5,@ps4_mprotect);
 lib^.set_proc($BD23009B77316136,@ps4_sceKernelMprotect);
 lib^.set_proc($F5B7DD2C8CAEC026,@ps4_sceKernelMtypeprotect);
 lib^.set_proc($58571F2F697389DA,@ps4_sceKernelQueryMemoryProtection);
 lib^.set_proc($AD58D1BC72745FA7,@ps4_sceKernelVirtualQuery);
 lib^.set_proc($0C6306DC9B21AD95,@ps4_sceKernelSetVirtualRangeName);
 lib^.set_proc($21620105D4C78ADE,@ps4_sceKernelMapFlexibleMemory);
 lib^.set_proc($98BF0D0C7F3A8902,@ps4_sceKernelMapNamedFlexibleMemory);
 lib^.set_proc($91CF8B1042186A47,@ps4_sceKernelMapNamedSystemFlexibleMemory);
 lib^.set_proc($EE8C6FDCF3C2BA6A,@ps4_sceKernelReserveVirtualRange);
 lib^.set_proc($0504278A8963F6D4,@ps4_sceKernelMapDirectMemory2);
 lib^.set_proc($2FF4372C48C86E00,@ps4_sceKernelMapDirectMemory);
 lib^.set_proc($35C6965317CC3484,@ps4_sceKernelMapNamedDirectMemory);

 lib^.set_proc($B59638F9264D1610,@ps4_msync);
 lib^.set_proc($0E435E6F1989C952,@ps4_sceKernelMsync);

 //mmap

 //queue

 lib^.set_proc($0F439D14C8E9E3A2,@ps4_sceKernelCreateEqueue);
 lib^.set_proc($7F3C8C2ACF648A6D,@ps4_sceKernelWaitEqueue);
 lib^.set_proc($BF3FA9836CDDA292,@ps4_sceKernelGetEventUserData);

 lib^.set_proc($E11EBF3AF2367040,@ps4_sceKernelAddUserEvent);
 lib^.set_proc($583B339926D6B839,@ps4_sceKernelAddUserEventEdge);
 lib^.set_proc($17A7B4930A387279,@ps4_sceKernelTriggerUserEvent);

 //queue

 //event_flag

 lib^.set_proc($0691686E8509A195,@ps4_sceKernelCreateEventFlag);
 lib^.set_proc($253BC17E58586B34,@ps4_sceKernelWaitEventFlag);
 lib^.set_proc($20E9D2BC7CEABBA0,@ps4_sceKernelSetEventFlag);
 lib^.set_proc($EEE8411564404BAD,@ps4_sceKernelClearEventFlag);
 lib^.set_proc($F26AA5F4E7109DDE,@ps4_sceKernelDeleteEventFlag);
 lib^.set_proc($3D992EE19AD726A8,@ps4_sceKernelCancelEventFlag);

 //event_flag

 //time

 lib^.set_proc($9FCF2FC770B99D6F,@ps4_gettimeofday);
 lib^.set_proc($B26223EDEAB3644F,@ps4_clock_getres);
 lib^.set_proc($94B313F6F240724D,@ps4_clock_gettime);
 lib^.set_proc($7A37A471A35036AD,@ps4_sceKernelGettimeofday);
 lib^.set_proc($90E7277ABCA99D00,@ps4_sceKernelGettimezone);

 lib^.set_proc($77B9D48F52CE7435,@ps4_clock_settime);
 lib^.set_proc($55D5C80C06C9DED4,@ps4_settimeofday);
 lib^.set_proc($0A108E0A13D4FD83,@ps4_sceKernelSettimeofday);

 lib^.set_proc($D63DD2DE7FED4D6E,@ps4_sceKernelGetTscFrequency);
 lib^.set_proc($FF62115023BFFCF3,@ps4_sceKernelReadTsc);
 lib^.set_proc($4018BB1C22B4DE1C,@ps4_sceKernelClockGettime);
 lib^.set_proc($E09DAC5099AE1D94,@ps4_sceKernelGetProcessTime);
 lib^.set_proc($04DA30C76979F3C1,@ps4_sceKernelGetProcessTimeCounterFrequency);
 lib^.set_proc($7E0C6731E4CD52D6,@ps4_sceKernelGetProcessTimeCounter);

 lib^.set_proc($C92F14D931827B50,@ps4_nanosleep);
 lib^.set_proc($42FB19C689AF507B,@ps4_sceKernelNanosleep);

 lib^.set_proc($41CB5E4706EC9D5D,@ps4_usleep);
 lib^.set_proc($D637D72D15738AC7,@ps4_sceKernelUsleep);

 lib^.set_proc($D30BB7DE1BA735D1,@ps4_sleep);
 lib^.set_proc($FD947E846EDA0C7C,@ps4_sceKernelSleep);

 lib^.set_proc($FE8E6E103A4DFA86,@ps4_sceKernelConvertUtcToLocaltime);
 lib^.set_proc($D0D4C737534A38D2,@ps4_sceKernelConvertLocaltimeToUtc);

 //time

 //file

 lib^.set_proc($D46DE51751A0D64F,@ps4_sceKernelOpen);
 lib^.set_proc($50AD939760D6527B,@ps4_sceKernelClose);
 lib^.set_proc($A226FBE85FF5D9F9,@ps4_sceKernelLseek);
 lib^.set_proc($0A0E2CAD9E9329B5,@ps4_sceKernelRead);
 lib^.set_proc($FABDEB305C08B55E,@ps4_sceKernelPread);
 lib^.set_proc($E304B37BDD8184B2,@ps4_sceKernelWrite);
 lib^.set_proc($9CA5A2FCDD87055E,@ps4_sceKernelPwrite);
 lib^.set_proc($556DD355988CE3F1,@ps4_sceKernelFtruncate);
 lib^.set_proc($901C023EC617FE6E,@ps4_sceKernelFstat);

 lib^.set_proc($B5A4568532454E01,@ps4_sceKernelGetdirentries);
 lib^.set_proc($8F6008A92A893F4C,@ps4_sceKernelGetdents);

 lib^.set_proc($C2E0ABA081A3B768,@ps4_open);  //open
 lib^.set_proc($E9CDEB09513F7D35,@ps4_open);  //_open

 lib^.set_proc($3B2E88A7082D60E9,@ps4_lseek); //lseek

 lib^.set_proc($6D8FCF3BA261CE14,@ps4_close); //close
 lib^.set_proc($34DB4568A25B3EDD,@ps4_close); //_close

 lib^.set_proc($0D1B81B76A6F2029,@ps4_read);  //_read
 lib^.set_proc($02A062A02DAF1772,@ps4_read);  //read

 lib^.set_proc($7B3BFF45204D2AA2,@ps4_pread); //pread

 lib^.set_proc($F9646590A8D9BDA8,@ps4_readv); //_readv
 lib^.set_proc($23B22670B76CFEE5,@ps4_readv); //readv

 lib^.set_proc($171559A81000EE4B,@ps4_write); //_write
 lib^.set_proc($14DE2068F9AE155F,@ps4_write); //write

 lib^.set_proc($0B6909FDBC92E6B3,@ps4_pwrite); //pwrite

 lib^.set_proc($8A1E020FDFE08213,@ps4_ftruncate);
 lib^.set_proc($9AA40C875CCF3D3F,@ps4_fstat);

 lib^.set_proc($7F4F4ABC83F2FD06,@ps4_getdirentries);
 lib^.set_proc($D86EA2EA13085146,@ps4_getdents);

 lib^.set_proc($13A6A8DF8C0FC3E5,@ps4_stat);
 lib^.set_proc($795F70003DAB8880,@ps4_sceKernelStat);

 lib^.set_proc($246322A3EDB52F87,@ps4_mkdir);
 lib^.set_proc($D7F2C52E6445C713,@ps4_sceKernelMkdir);

 lib^.set_proc($540CECC2F4CE0B32,@ps4_unlink);
 lib^.set_proc($0145D5C5678953F0,@ps4_sceKernelUnlink);

 lib^.set_proc($B96C96DEFF7CB14E,@ps4_sceKernelCheckReachability);

 //file

 px:=Result._add_lib('libScePosix');
 px^.MapSymbol:=lib^.MapSymbol;

 px:=Result._add_lib('libkernel_cpumode_platform');
 px^.MapSymbol:=lib^.MapSymbol;

 lib:=Result._add_lib('libkernel_unity');

 lib^.set_proc($5A4C0477737BC346,@ps4_sceKernelInstallExceptionHandler);
 lib^.set_proc($421BF90110283847,@ps4_sceKernelRemoveExceptionHandler);
 lib^.set_proc($8A5D379E5B8A7CC9,@ps4_sceKernelRaiseException);

 //
 _kernel_init;
end;

initialization
 ps4_app.RegistredPreLoad('libSceSysmodule.prx',@Load_libSceSysmodule);
 ps4_app.RegistredPreLoad('libkernel.prx',@Load_libkernel);

end.

