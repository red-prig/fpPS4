unit ps4_libkernel;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  RWLock,
  sys_types,
  ps4_map_mm,
  ps4_pthread,
  ps4_signal,
  ps4_mutex,
  ps4_cond,
  ps4_sema,
  ps4_rwlock,
  ps4_time,
  ps4_kernel_file,
  ps4_queue,
  ps4_event_flag,
  ps4_elf,
  ps4_program,

  //trace_manager,

  Classes,
  SysUtils;

function  ps4_sceKernelIsNeoMode:Integer; SysV_ABI_CDecl;

implementation

uses
 sys_kernel,
 sys_pthread,
 sys_signal;

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
 Writeln('Stack overflow detected! Aborting program.');
 DebugBreak;
end;

{$I StopNotificationReason.inc}

// eStopNotificationReason
procedure ps4_sceKernelDebugRaiseException(dwStopReason,dwStopId:DWORD); SysV_ABI_CDecl;
var
 t:pthread;
begin
 t:=_get_curthread;
 if (t<>nil) then
  Writeln('RaiseThread=',t^.name);
 Writeln(StdErr,'RaiseException:',HexStr(dwStopReason,8),':',HexStr(dwStopId,8),':',GetStopReasonInfo(dwStopReason));
 DebugBreak;
end;

procedure ps4_sceKernelDebugRaiseExceptionOnReleaseMode(dwStopReason,dwStopId:DWORD); SysV_ABI_CDecl;
var
 t:pthread;
begin
 t:=_get_curthread;
 if (t<>nil) then
  Writeln('RaiseThread=',t^.name);
 Writeln(StdErr,'RaiseException:',HexStr(dwStopReason,8),':',HexStr(dwStopId,8),':',GetStopReasonInfo(dwStopReason));
 DebugBreak;
end;

//ps4 neo mode is support? (Ps4 Pro)
function ps4_sceKernelIsNeoMode:Integer; SysV_ABI_CDecl;
begin
 Result:=0; //no
 //Result:=1; //yes
end;

//void * _aligned_malloc(
//    size_t size,
//    size_t alignment
//);

//void _aligned_free (
//   void *memblock
//);

function ps4_sceKernelGetModuleInfoFromAddr(Addr:Pointer;P2:Integer;info:PKernelModuleInfo):Integer; SysV_ABI_CDecl;
var
 node:TElf_node;
begin
 if (info=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);
 _sig_lock;
 Writeln('GetModuleInfoFromAddr:',HexStr(Addr),':',P2,':',HexStr(info));
 node:=ps4_app.AcqureFileByCodeAdr(Addr);
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

function ps4_sceKernelGetModuleInfo(handle:Integer;info:PKernelModuleInfo):Integer; SysV_ABI_CDecl;
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

type
 PInternalSegmentInfo=^TInternalSegmentInfo;
 TInternalSegmentInfo=packed record
  address:Pointer;
  size:DWORD;
 end;

function ps4_sceKernelInternalMemoryGetModuleSegmentInfo(pOut:PInternalSegmentInfo):Integer; SysV_ABI_CDecl;
begin
 pOut^.address:=nil;
 pOut^.size:=0;
 Result:=0;
 //sceKernelGetLibkernelTextLocation(pOut^.address,pOut^.size)
end;

function ps4_sceKernelGetProcParam:Pointer; SysV_ABI_CDecl;
begin
 Writeln('KernelGetProcParam');
 Result:=GetProcParam;
end;

type
 PAppHeapAPI=^TAppHeapAPI;
 TAppHeapAPI=packed record
  _malloc,_free:Pointer;
 end;

procedure ps4_sceKernelRtldSetApplicationHeapAPI(heap_api:PAppHeapAPI); SysV_ABI_CDecl;
begin
 Writeln('SetApplicationHeapAPI:',HexStr(heap_api));
 Writeln(' __malloc:',HexStr(heap_api^._malloc)); //__malloc
 Writeln('   __free:',HexStr(heap_api^._free));   //__free
end;

//registred destroy proc?
function ps4_sceKernelSetThreadDtors(Proc:TProcedure):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceKernelSetThreadDtors:',HexStr(proc));
 Result:=0;
end;

function ps4___cxa_atexit(func:atexit_func;arg:Pointer;dso_handle:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln('__cxa_atexit:',HexStr(func));
 Result:=0;
end;

//registred thread atexit proc?
function ps4_sceKernelSetThreadAtexitCount(proc:TKernelAtexitFunc):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceKernelSetThreadAtexitCount:',HexStr(proc));
 Result:=0;
end;

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

//PlayStation®4 Clang: UndefinedBehaviorSanitizer (UBSan)
function ps4_sceKernelGetSanitizerNewReplaceExternal():Pointer; SysV_ABI_CDecl;
begin
 //list mem of proc????
 Result:=nil;
end;

function ps4_sceKernelIsAddressSanitizerEnabled():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceKernelIsAddressSanitizerEnabled');
 Result:=0;
end;

function ps4_sceKernelGetCompiledSdkVersion(sdkVersion:PDWORD):Integer; SysV_ABI_CDecl;
begin
 if (sdkVersion=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 sdkVersion^:=$FFFFFFFF;
 Result:=0;
end;

//dynamic load????
function ps4_sceKernelLoadStartModule(moduleFileName:Pchar;
                                      argc:size_t;
                                      argp:PPointer;
                                      flags:DWORD;
                                      pOpt:PSceKernelLoadModuleOpt;
                                      pRes:PInteger):SceKernelModule; SysV_ABI_CDecl;
var
 node:TElf_node;
 fn:RawByteString;
 i:Integer;
begin
 Result:=0;
 _sig_lock;
 Writeln('Load Lib:',moduleFileName);

 fn:=_parse_filename(moduleFileName);

 Writeln('Load File:',fn);

 node:=LoadPs4ElfFromFile(fn);
 if (node<>nil) then
 begin
  node.IsStatic:=False;
  node.Acqure;

  node.Prepare;
  ps4_app.RegistredElf(node);
  ps4_app.ResolveDepended(node);
  ps4_app.LoadSymbolImport(nil);
  ps4_app.ReLoadSymbolImport(Pointer(node));
  ps4_app.InitProt;
  ps4_app.InitThread(0);

  node.IsInit:=True; //mark exclude

  ps4_app.InitCode;

  i:=node.module_start(argc,argp);

  Result:=node.Handle;
  node.Release;

  if (pRes<>nil) then pRes^:=i;

  if (i<0) then Result:=SCE_KERNEL_ERROR_EINVAL;
 end else
 begin
  Result:=SCE_KERNEL_ERROR_ENOENT;
 end;

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

function ps4_memset(ptr:Pointer;value:Integer;num:size_t):Pointer; SysV_ABI_CDecl;
begin
 FillByte(ptr^,num,Byte(value));
 Result:=ptr;
end;

function ps4_memcmp(buf1,buf2:Pointer;count:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=CompareByte(buf1^,buf2^,count);
end;

function ps4_memcpy_s(dst:Pointer;dstSize:size_t;src:Pointer;count:size_t):Integer; SysV_ABI_CDecl;
begin
 if (count=0) then Exit(0);

 if (dst=nil) or (src=nil) then
 begin
  if (dst<>nil) then FillChar(dst^,dstSize,0);
  _set_errno(EINVAL);
  Exit(EINVAL);
 end;

 if (dstSize<count) then
 begin
  FillChar(dst^,dstSize,0);
  _set_errno(ERANGE);
  Exit(ERANGE);
 end;

 Move(src^,dst^,count);
 Result:=0;
end;

function ps4_strcpy_s(dst:PChar;destSize:size_t;src:PChar):Integer; SysV_ABI_CDecl;
var
 count:size_t;
begin
 if (dst=nil) or (src=nil) then
 begin
  if (dst<>nil) then dst[0]:=#0;
  _set_errno(EINVAL);
  Exit(EINVAL);
 end;

 count:=System.strlen(src)+1;
 if (count>destSize) then
 begin
  dst[0]:=#0;
  _set_errno(ERANGE);
  Exit(ERANGE);
 end;

 Move(src^,dst^,count);
 Result:=0;
end;

function ps4_memcpy(dst,src:Pointer;len:size_t):Pointer; SysV_ABI_CDecl;
begin
 Move(src^,dst^,len);
 Result:=dst;
end;

const
 SCE_LIBC_MSPACE_THREAD_UNSAFE  =($00000001);
 SCE_LIBC_MSPACE_DEBUG_SHORTAGE =($00000004);

type
 SceLibcMspace=^TSceLibcMspace;
 TSceLibcMspace=packed record
  base:Pointer;
  capacity:size_t;
  count:size_t;
  lock:TRWLock;
 end;

function ps4_sceLibcMspaceCreate(name:PChar;base:Pointer;capacity:size_t;flag:Integer):SceLibcMspace; SysV_ABI_CDecl;
begin
 Result:=nil;

 if ((QWORD(base) and $F)<>0) or ((capacity and $F)<>0) then Exit;
 if (capacity<=1440) then Exit;

 Writeln('sceLibcMspaceCreate:',name,':',HexStr(base),'..',HexStr(base+capacity));
 Result:=SwAllocMem(SizeOf(TSceLibcMspace));
 Result^.base:=base;
 Result^.capacity:=capacity;
 Result^.count:=1440;

 _sig_lock;
 rwlock_init(Result^.lock);
 _sig_unlock;
end;

function ps4_sceLibcMspaceMalloc(msp:SceLibcMspace;size:size_t):Pointer; SysV_ABI_CDecl;
begin
 Result:=nil;
 if (msp=nil) then Exit;
 _sig_lock;
 rwlock_wrlock(msp^.lock);
 if ((msp^.count+size)<=msp^.capacity) then
 begin
  Result:=msp^.base+msp^.count;
  msp^.count:=msp^.count+size;
 end;
 rwlock_unlock(msp^.lock);
 _sig_unlock;
end;

function ps4_expf(x:Single):Single; SysV_ABI_CDecl;
begin
 Result:=System.Exp(x);
end;

type
 PGetTraceInfo=^TGetTraceInfo;
 TGetTraceInfo=packed record
  Size:QWORD;                    //32
  flag:DWORD;                    //1
  get_segment_info:DWORD;        //0
  mspace_atomic_id_mask:PQWORD;
  mstate_table:PQWORD;
 end;

var
 g_mspace_atomic_id_mask:QWORD=0;
 g_mstate_table:array[0..63] of QWORD;

//mysterious function
procedure ps4_sceLibcHeapGetTraceInfo(P:PGetTraceInfo); SysV_ABI_CDecl;
begin
 P^.get_segment_info     :=0;
 P^.mspace_atomic_id_mask:=@g_mspace_atomic_id_mask;
 P^.mstate_table         :=@g_mstate_table;
end;

function ps4_sceSysmoduleLoadModule(id:Word):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleLoadModule:',HexStr(id,4)); //libSceNgs2.sprx	SCE_SYSMODULE_NGS2	0x000B
 Result:=0;
end;

function ps4_sceSysmoduleUnloadModule(id:Word):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleUnloadModule:',HexStr(id,4));
 Result:=0;
end;

function ps4_sceSysmoduleIsLoaded(id:Word):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSysmoduleIsLoaded:',HexStr(id,4));
 Result:=0;
end;

const
 __progname:PChar='eboot.bin'; //argv[0]

Const
 Need_sceLibcInternal:QWORD=1;

 _Stdin :QWORD=0;
 _Stdout:QWORD=1;
 _Stderr:QWORD=2;

function _get_proc_libSceLibcInternal(src:PLIBRARY;nid:QWORD):Pointer;
var
 lib:PLIBRARY;
begin
 Result:=nil;
 lib:=ps4_app.GetLib('libc');
 if (lib<>nil) then
 begin
  Result:=lib^.get_proc(Nid);
 end;
 if (Result=nil) then
 begin
  Result:=src^._get_proc(nid);
 end;
end;

{
function _get_proc_libSceLibcInternal(src:PLIBRARY;nid:QWORD):Pointer;
var
 lib:PLIBRARY;
begin
 Result:=src^._get_proc(nid);
 if (Result=nil) then
 begin
  Case nid of
   $78B743C3A974FDB5: //snprintf
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;
   $F33B2ED385CDB19E: //expf
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
    if (Result=nil) then
    begin
     Result:=@ps4_expf;
    end;
   end;
  $DC63E98D0740313C: //__cxa_guard_acquire
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;
  $F6B01E00D4F6B721: //__cxa_guard_release
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;
  end;
  if (Result<>nil) then
  begin
   src^.set_proc(nid,Result);
  end;
 end;
end;
}

function Load_libSceLibcInternal(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceLibcInternal');

 lib^.Fget_proc_cb:=@_get_proc_libSceLibcInternal;

 lib^.set_proc($653E0E0C3D93B3DA,@Need_sceLibcInternal);

 lib^.set_proc($D530E8FC89AA9097,@_Stdin );
 lib^.set_proc($DAC5B3858A851F81,@_Stdout);
 lib^.set_proc($1FC029ACA799B4D8,@_Stderr);

 lib^.set_proc($F334C5BC120020DF,@ps4_memset);
 lib^.set_proc($0DF8AF3C0AE1B9C8,@ps4_memcmp);
 lib^.set_proc($3452ECF9D44918D8,@ps4_memcpy_s);
 lib^.set_proc($E576B600234409DA,@ps4_strcpy_s);
 lib^.set_proc($437541C425E1507B,@ps4_memcpy);
 lib^.set_proc($FE19F5B5C547AB94,@ps4_sceLibcMspaceCreate);
 lib^.set_proc($3898E6FD03881E52,@ps4_sceLibcMspaceMalloc);

 lib^.set_proc($B6CBC49A77A7CF8F,@ps4___cxa_atexit);

 lib:=Result._add_lib('libSceLibcInternalExt');

 lib^.set_proc($356B53375D1C2731,@ps4_sceLibcHeapGetTraceInfo);
end;

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

 //signal

 lib^.set_proc($38C0D128A019F08E,@ps4_sceKernelDebugRaiseException);
 lib^.set_proc($CC4FF05C86632E83,@ps4_sceKernelDebugRaiseExceptionOnReleaseMode);

 lib^.set_proc($5644C0B2B643709D,@ps4_sigfillset);
 lib^.set_proc($68F732A6D6CE899B,@ps4_sigprocmask);
 lib^.set_proc($EB1569CB415DABE2,@ps4_sigprocmask);
 lib^.set_proc($72B6F98FB9A49357,@ps4_is_signal_return);

 //signal

 lib^.set_proc($93E017AAEDBF7817,@ps4_getpagesize);
 lib^.set_proc($04F13DB3DBD0417A,@ps4_mmap);
 lib^.set_proc($52A0C68D7039C943,@ps4_munmap);
 lib^.set_proc($B59638F9264D1610,@ps4_msync);
 lib^.set_proc($61039FC4BE107DE5,@ps4_mprotect);


 lib^.set_proc($FD84D6FAA5DCDC24,@ps4_sceKernelInternalMemoryGetModuleSegmentInfo);
 lib^.set_proc($7FB28139A7F2B17A,@ps4_sceKernelGetModuleInfoFromAddr);
 lib^.set_proc($914A60AD722BCFB4,@ps4_sceKernelGetModuleInfo);
 lib^.set_proc($F79F6AADACCF22B8,@ps4_sceKernelGetProcParam);
 lib^.set_proc($A7911C41E11E2401,@ps4_sceKernelRtldSetApplicationHeapAPI);
 lib^.set_proc($ACD856CFE96F38C5,@ps4_sceKernelSetThreadDtors);
 lib^.set_proc($A41FF2199DA743DA,@ps4_sceKernelSetThreadAtexitCount);
 lib^.set_proc($5A109CD70DC48522,@ps4_sceKernelSetThreadAtexitReport);
 lib^.set_proc($6E7671620005780D,@ps4_sceKernelGetSanitizerNewReplaceExternal);
 lib^.set_proc($8E1FBC5E22B82DE1,@ps4_sceKernelIsAddressSanitizerEnabled);
 lib^.set_proc($581EBA7AFBBC6EC5,@ps4_sceKernelGetCompiledSdkVersion);

 lib^.set_proc($C33BEA4F852A297F,@ps4_sceKernelLoadStartModule);
 lib^.set_proc($22EC6752E5E4E818,@ps4_sceKernelGetModuleList);
 lib^.set_proc($2F01BC8379E2AB00,@ps4_sceKernelDlsym);

 lib^.set_proc($54EC7C3469875D3B,@ps4_sceKernelGetCpumode);

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
 lib^.set_proc($9A4C767D584D32C8,@ps4_pthread_cond_broadcast);

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

 //thread

 lib^.set_proc($9EC628351CB0C0D8,@ps4_scePthreadAttrInit);
 lib^.set_proc($EB6282C04326CDC3,@ps4_scePthreadAttrDestroy);
 lib^.set_proc($C2D92DFED791D6CA,@ps4_pthread_attr_init);
 lib^.set_proc($CC772163C7EDE699,@ps4_pthread_attr_destroy);
 lib^.set_proc($5135F325B5A18531,@ps4_scePthreadAttrSetstacksize);
 lib^.set_proc($D90D33EAB9C1AD31,@ps4_pthread_attr_setstacksize);
 lib^.set_proc($FD6ADEA6BB6ED10B,@ps4_scePthreadAttrSetdetachstate);
 lib^.set_proc($E3E87D133C0A1782,@ps4_scePthreadAttrSetschedpolicy);
 lib^.set_proc($0F3112F61405E1FE,@ps4_scePthreadAttrSetschedparam);
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
 lib^.set_proc($E9482DC15FB4CDBE,@ps4_scePthreadCreate);
 lib^.set_proc($3B184807C2C1FCF4,@ps4_pthread_create);
 lib^.set_proc($E2A1AB47A7A83FD6,@ps4_scePthreadDetach);
 lib^.set_proc($F94D51E16B57BE87,@ps4_pthread_detach);
 lib^.set_proc($A27358F41CA7FD6F,@ps4_scePthreadJoin);

 lib^.set_proc($678428B15B80B00D,@ps4_pthread_once);
 lib^.set_proc($D786CE00200D4C1A,@ps4_scePthreadOnce);

 lib^.set_proc($DCFB55EA9DD0357E,@ps4_scePthreadEqual);
 lib^.set_proc($ED7976E7B33854D2,@ps4_pthread_equal);

 lib^.set_proc($DE483BAD3D0D408B,@ps4_scePthreadExit);
 lib^.set_proc($149AD3E4BB940405,@ps4_pthread_exit);

 lib^.set_proc($128B51F1ADC049FE,@ps4_pthread_self);
 lib^.set_proc($688F8E782CFCC6B4,@ps4_scePthreadSelf);

 lib^.set_proc($1E82D558D6A70417,@ps4_getpid);

 lib^.set_proc($1E8C3B07C39EB7A9,@ps4_scePthreadGetname);
 lib^.set_proc($181518EF2C1D50B1,@ps4_scePthreadRename);

 lib^.set_proc($6EDDC24C12A61B22,@ps4_scePthreadSetaffinity);
 lib^.set_proc($ADCAD5149B105916,@ps4_scePthreadGetaffinity);


 lib^.set_proc($D6D2B21BB465309A,@ps4_scePthreadGetprio);
 lib^.set_proc($5B41E99B65F4B8F1,@ps4_scePthreadSetprio);

 lib^.set_proc($3F8D644D6512DC42,@ps4_scePthreadGetschedparam);
 lib^.set_proc($A084454E3A082DB8,@ps4_scePthreadSetschedparam);

 lib^.set_proc($08136D5CEA1E7FF1,@ps4_sched_get_priority_max);
 lib^.set_proc($9B4892EA336C5DDB,@ps4_sched_get_priority_min);

 lib^.set_proc($4FBDA1CFA7DFAB4F,@ps4_scePthreadYield);
 lib^.set_proc($0791A65432B0A67D,@ps4_pthread_yield);

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

 //mmap

 lib^.set_proc($A4EF7A4F0CCE9B91,@ps4_sceKernelGetDirectMemorySize);
 lib^.set_proc($0B47FB4C971B7DA7,@ps4_sceKernelAvailableDirectMemorySize);
 lib^.set_proc($047A2E2D0CE1D17D,@ps4_sceKernelDirectMemoryQuery);
 lib^.set_proc($AD35F0EB9C662C80,@ps4_sceKernelAllocateDirectMemory);
 lib^.set_proc($2FF4372C48C86E00,@ps4_sceKernelMapDirectMemory);
 lib^.set_proc($98BF0D0C7F3A8902,@ps4_sceKernelMapNamedFlexibleMemory);
 lib^.set_proc($21620105D4C78ADE,@ps4_sceKernelMapFlexibleMemory);
 lib^.set_proc($71091EF54B8140E9,@ps4_sceKernelMunmap);
 lib^.set_proc($58571F2F697389DA,@ps4_sceKernelQueryMemoryProtection);

 //mmap

 //queue

 lib^.set_proc($0F439D14C8E9E3A2,@ps4_sceKernelCreateEqueue);
 lib^.set_proc($7F3C8C2ACF648A6D,@ps4_sceKernelWaitEqueue);
 lib^.set_proc($BF3FA9836CDDA292,@ps4_sceKernelGetEventUserData);

 //queue

 //event_flag

 lib^.set_proc($0691686E8509A195,@ps4_sceKernelCreateEventFlag);
 lib^.set_proc($253BC17E58586B34,@ps4_sceKernelWaitEventFlag);
 lib^.set_proc($20E9D2BC7CEABBA0,@ps4_sceKernelSetEventFlag);
 lib^.set_proc($EEE8411564404BAD,@ps4_sceKernelClearEventFlag);
 lib^.set_proc($F26AA5F4E7109DDE,@ps4_sceKernelDeleteEventFlag);

 //event_flag

 //time

 lib^.set_proc($9FCF2FC770B99D6F,@ps4_gettimeofday);
 lib^.set_proc($B26223EDEAB3644F,@ps4_clock_getres);
 lib^.set_proc($94B313F6F240724D,@ps4_clock_gettime);
 lib^.set_proc($7A37A471A35036AD,@ps4_sceKernelGettimeofday);
 lib^.set_proc($D63DD2DE7FED4D6E,@ps4_sceKernelGetTscFrequency);
 lib^.set_proc($FF62115023BFFCF3,@ps4_sceKernelReadTsc);
 lib^.set_proc($4018BB1C22B4DE1C,@ps4_sceKernelClockGettime);
 lib^.set_proc($E09DAC5099AE1D94,@ps4_sceKernelGetProcessTime);
 lib^.set_proc($C92F14D931827B50,@ps4_nanosleep);
 lib^.set_proc($41CB5E4706EC9D5D,@ps4_usleep);
 lib^.set_proc($D637D72D15738AC7,@ps4_sceKernelUsleep);

 //time

 //file

 lib^.set_proc($D46DE51751A0D64F,@ps4_sceKernelOpen);
 lib^.set_proc($A226FBE85FF5D9F9,@ps4_sceKernelLseek);
 lib^.set_proc($E304B37BDD8184B2,@ps4_sceKernelWrite);
 lib^.set_proc($0A0E2CAD9E9329B5,@ps4_sceKernelRead);
 lib^.set_proc($FABDEB305C08B55E,@ps4_sceKernelPread);
 lib^.set_proc($50AD939760D6527B,@ps4_sceKernelClose);

 lib^.set_proc($C2E0ABA081A3B768,@ps4_open);
 lib^.set_proc($6D8FCF3BA261CE14,@ps4_close);
 lib^.set_proc($171559A81000EE4B,@ps4_write);
 lib^.set_proc($0D1B81B76A6F2029,@ps4_read);

 lib^.set_proc($795F70003DAB8880,@ps4_sceKernelStat);
 lib^.set_proc($13A6A8DF8C0FC3E5,@ps4_stat);

 lib^.set_proc($901C023EC617FE6E,@ps4_sceKernelFstat);
 lib^.set_proc($9AA40C875CCF3D3F,@ps4_fstat);

 lib^.set_proc($D7F2C52E6445C713,@ps4_sceKernelMkdir);
 lib^.set_proc($246322A3EDB52F87,@ps4_mkdir);

 //file

 px:=Result._add_lib('libScePosix');
 px^.MapSymbol:=lib^.MapSymbol;

 lib:=Result._add_lib('libkernel_unity');

 lib^.set_proc($5A4C0477737BC346,@ps4_sceKernelInstallExceptionHandler);
 lib^.set_proc($8A5D379E5B8A7CC9,@ps4_sceKernelRaiseException);

end;

initialization
 ps4_app.RegistredPreLoad('libSceLibcInternal.prx',@Load_libSceLibcInternal);
 ps4_app.RegistredPreLoad('libSceSysmodule.prx',@Load_libSceSysmodule);
 ps4_app.RegistredPreLoad('libkernel.prx',@Load_libkernel);

end.

