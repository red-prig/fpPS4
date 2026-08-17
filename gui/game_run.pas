unit game_run;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 SysUtils,
 CharStream,
 Dialogs,
 kern_thr,
 md_sleep,
 md_pipe,
 host_ipc_interface,
 host_ipc,
 md_host_ipc,
 param_sfo_gui,
 core_shell,
 game_info,
 game_run_context,
 game_mount;

type
 TGameRunConfig=record
  hOutput:THandle;
  hError :THandle;

  FConfInfo:TConfigInfo;
  FGameItem:TGameItem;
  FParamSfo:TParamSfoFile;

  FLoadExec:Boolean;
 end;

 TGameProcessSimple=class(TGameProcess)
  Ftd:p_kthread;
  procedure  suspend; override;
  procedure  resume;  override;
  Destructor Destroy; override;
 end;

function run_item(const cfg:TGameRunConfig;var Context:TGameRunContext):Integer;

implementation

uses
 errno,
 signal,
 sys_sysinit,
 kern_exec,
 kern_exit,
 sys_crt, //<- init writeln redirect
 sys_tty,
 md_exception, //<- install custom

 sys_event,

 kern_proc,
 md_systm,
 md_systm_fork,

 md_game_process,

 kern_jit,
 kern_jit_ctx,
 kern_lazy_jit,

 dev_dce,
 display_soft,

 time,
 pm4_me,

 vDevice,

 //internal libs
 ps4_libSceDiscMap,
 ps4_libSceSystemService,
 ps4_libSceUserService,
 ps4_libSceAppContent,
 ps4_libSceIpmi,
 ps4_libSceMbus,
 ps4_libSceDialogs,
 ps4_libSceAvSetting,
 ps4_libSceNpCommon,
 ps4_libSceNpManager,
 ps4_libSceNpTrophy,
 ps4_libSceNpScoreRanking,
 ps4_libSceNpUtility,
 ps4_libSceNpTus,
 ps4_libSceNpGameIntent,
 ps4_libSceNpWebApi,
 ps4_libSceNpWebApi2,
 ps4_libSceNpSns,
 ps4_libSceNpMatching2,
 ps4_libSceNpSignaling,
 ps4_libSceNpSessionSignaling,
 ps4_libSceNpParty,
 ps4_libSceNpEntitlementAccess,
 ps4_libSceRemoteplay,
 ps4_libSceScreenShot,
 ps4_libSceSaveData,
 ps4_libSceAudioOut,
 ps4_libSceAudioIn,
 ps4_libSceNetCtl,
 ps4_libSceGameLiveStreaming,
 ps4_libSceVideoRecording,
 ps4_libSceIme,
 ps4_libSceMove,
 ps4_libSceSharePlay,
 ps4_libSceShareUtility,
 ps4_libScePlayGo,
 ps4_libSceAjm,
 ps4_libSceCompanionUtil,
 ps4_libSceAutoMounterClient,
 ps4_libSceHmd,
 ps4_libSceVrTracker,
 ps4_libSceAudio3d,
 //internal libs

 kern_rtld,
 kern_budget,
 kern_authinfo,
 sys_bootparam,
 subr_backtrace;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

//

procedure TGameProcessSimple.suspend;
begin
 thread_suspend_all(nil);
end;

procedure TGameProcessSimple.resume;
begin
 thread_resume_all(nil);
end;

Destructor TGameProcessSimple.Destroy;
begin
 if (Ftd<>nil) then
 begin
  thread_dec_ref(Ftd);
  Ftd:=nil;
 end;
 inherited;
end;

procedure re_init_tty; register;
var
 i:Integer;
begin
 For i:=0 to High(std_tty) do
 begin
  //std_tty[i].t_rd_handle:=StdInputHandle;
  //std_tty[i].t_wr_handle:=t_wr_handle;
  //std_tty[i].t_update   :=@WakeMainThread;
 end;

 For i:=0 to High(deci_tty) do
 begin
  //deci_tty[i].t_rd_handle:=StdInputHandle;
  //deci_tty[i].t_wr_handle:=t_wr_handle;
  //deci_tty[i].t_update   :=@WakeMainThread;
 end;

 //debug_tty.t_wr_handle:=t_wr_handle;
 //debug_tty.t_update   :=@WakeMainThread;
end;

procedure load_config(ConfInfo:TConfigInfo);
begin
 sys_bootparam.set_neo_mode(ConfInfo.BootParamInfo.Neo);

 sys_bootparam.p_halt_on_exit       :=ConfInfo.BootParamInfo.halt_on_exit;
 sys_bootparam.p_print_guest_syscall:=ConfInfo.BootParamInfo.print_guest_syscall;
 sys_bootparam.p_print_pmap         :=ConfInfo.BootParamInfo.print_pmap;
 sys_bootparam.p_print_jit_preload  :=ConfInfo.BootParamInfo.print_jit_preload;
 sys_bootparam.p_print_gpu_ops      :=ConfInfo.BootParamInfo.print_gpu_ops;
 sys_bootparam.p_print_gpu_hint     :=ConfInfo.BootParamInfo.print_gpu_hint;

 //

 kern_jit.print_asm :=ConfInfo.JITInfo.print_asm;
 kern_jit.debug_info:=ConfInfo.JITInfo.debug_info;

 kern_jit_ctx.jit_relative_analize:=ConfInfo.JITInfo.relative_analize;
 kern_jit_ctx.jit_scan_switchtable:=ConfInfo.JITInfo.scan_switchtable;
 kern_jit_ctx.jit_scan_nopsequence:=ConfInfo.JITInfo.scan_nopsequence;
 kern_jit_ctx.jit_memory_guard    :=ConfInfo.JITInfo.memory_guard;
 kern_lazy_jit.use_lazy_jit       :=ConfInfo.JITInfo.lazy_jit;

 //

 time.strict_ps4_freq        :=ConfInfo.MiscInfo.strict_ps4_freq;
 pm4_me.use_renderdoc_capture:=ConfInfo.MiscInfo.renderdoc_capture;

 //

 vDevice.VulkanDeviceGuid:=Default(TGUID);
 TryStringToGUID(ConfInfo.VulkanInfo.device,vDevice.VulkanDeviceGuid);

 vDevice.VulkanAppFlags:=t_vulkan_app_flags(ConfInfo.VulkanInfo.app_flags);
 //

 ps4_libSceSystemService.FSystemName  :=ConfInfo.PS4SystemService.SystemName;
 ps4_libSceSystemService.FLanguage    :=ConfInfo.PS4SystemService.Language;
 ps4_libSceSystemService.FDateFormat  :=ConfInfo.PS4SystemService.DateFormat;
 ps4_libSceSystemService.FTimeFormat  :=ConfInfo.PS4SystemService.TimeFormat;
 ps4_libSceSystemService.FButtonAssign:=ConfInfo.PS4SystemService.ButtonAssign;

 ps4_libSceAudioOut.FMainDevice      :=ConfInfo.PS4Audio.MainDevice;
 ps4_libSceAudioOut.FHeadphoneDevice :=ConfInfo.PS4Audio.HeadphoneDevice;
 ps4_libSceAudioOut.FControllerDevice:=ConfInfo.PS4Audio.ControllerDevice;
 ps4_libSceAudioOut.FSpecialDevice   :=ConfInfo.PS4Audio.SpecialDevice;
end;

function get_errno_str(err:Integer):RawByteString;
begin
 case err of
  EPERM  :Result:='Operation not permitted';
  ENOENT :Result:='No such file or directory';
  EACCES :Result:='Permission denied';
  EEXIST :Result:='Directory exists';
  ENOTDIR:Result:='Not a directory';
  else
          Result:=IntToStr(err);
 end;
end;

function GetEnlargeFmem256mb(hasParamSfo:Integer;attribute2:DWORD):Boolean; inline;
begin
 Result:=(hasParamSfo<>0) and ((attribute2 and $4000)<>0);
end;

function Get2mbPageMode(attribute2:DWORD):DWORD; inline;
begin
 Result:=(attribute2 shr 15) and 3;
end;

procedure prepare(GameStartupInfo:TGameStartupInfo); SysV_ABI_CDecl;
var
 err:Integer;
 mode:DWORD;
 argv:PPChar;
 i,argc:Integer;
 Item:TGameItem;
 LoadExec:Boolean;
begin
 //re_init_tty;
 //init_tty:=@re_init_tty;

 load_config(GameStartupInfo.FConfInfo);

 //init all
 sys_init;

 if (p_host_ipc<>nil) then
 begin
  THostIpcConnect(p_host_ipc).Dispatcher.thread_new;
 end;

 //p_cpuid        :=CPUID_NEO_MODE;
 //p_base_ps4_mode:=0;
 //p_neomode      :=1;

 dev_dce.dce_interface:=display_soft.TDisplayHandleSoft;

 Item:=GameStartupInfo.FGameItem;

 g_appinfo.mmap_flags      :=1; //is_big_app ???
 g_appinfo.attributeExe    :=GameStartupInfo.ATTRIBUTE_EXE;
 g_appinfo.attribute2      :=GameStartupInfo.ATTRIBUTE2;
 g_appinfo.CUSANAME        :=GameStartupInfo.TITLE_ID;
 g_appinfo.requiredHdcpType:=GameStartupInfo.RequiredHdcpType;
 g_appinfo.attribute       :=GameStartupInfo.ATTRIBUTE;
 g_appinfo.hasParamSfo     :=GameStartupInfo.hasParamSfo;

 //g_appinfo.debug_level:=1;

 g_appinfo.titleWorkaround.version:=69;

 if PDWORD(@g_appinfo.CUSANAME)^=DWORD($41535543) then //'CUSA'
 begin
  g_appinfo.titleWorkaround.ids[0]:=$200000; //BUG184831_NEO_VDDNB_VID_STEP_UP_ALL_TITLE=$15;
 end;

 //budget init
 p_proc.p_budget_ptype:=PTYPE_BIG_APP;
 p_proc.p_vm_container:=1;

 kern_app_state_change(as_start);
 kern_app_state_change(as_begin_game_app_mount);

 if GetEnlargeFmem256mb(g_appinfo.hasParamSfo,g_appinfo.attribute2) then
 begin
  kern_app_state_change(as__enable_ext_game_fmem);
 end;

 kern_reserve_2mb_page(0,M2MB_DEFAULT);

 mode:=Get2mbPageMode(g_appinfo.attribute2);
 if ((GameStartupInfo.SELF_2MIB_PAGE_AMOUNT=0) or (mode>M2MB_DISABLE)) then
 begin
  kern_reserve_2mb_page(GameStartupInfo.SELF_2MIB_PAGE_AMOUNT,mode);
 end;

 LoadExec:=GameStartupInfo.LoadExec;

 LOG_INFO('Name    :',Item.FGameInfo.Name      );
 LOG_INFO('TitleId :',Item.FGameInfo.TitleId   );
 LOG_INFO('Version :',Item.FGameInfo.Version   );
 LOG_INFO('AppVer  :',Item.FGameInfo.AppVer    );
 LOG_INFO('Exec    :',Item.FGameInfo.Exec      );

 LOG_INFO('game    :',Item.FMountList.game     );
 LOG_INFO('firmware:',Item.FMountList.firmware );

 LOG_INFO('LocalDir:',GameStartupInfo.LocalDir );

 InitMount(GameStartupInfo);

 ///argv

 argv:=nil;
 argc:=parse_params(Item.FGameInfo.Exec,argv);

 LOG_TRACE('main_thread:',HexStr(curkthread));

 //
 FreeAndNil(GameStartupInfo);
 //

 LOG_INFO('main_execve->');
 For i:=0 to argc-1 do
 begin
  LOG_INFO(' argv[',i,']:',argv[i]);
 end;

 Flush(stdout);

 if (argv[0]=nil) then
 begin
  err:=ENOENT;
 end else
 begin
  err:=main_execve(argv[0],argv,nil);
 end;

 if (err=0) then
 begin
  //free data
  free_params(argv);

  //jump to code
  main_switch_context;
 end else
 if (err<>0) then
 begin
  if not LoadExec then
  begin
   print_error_td('[execve error]'+#13#10+
                  ' cmd:"'+argv[0]+'"'#13#10+
                  ' err:'+get_errno_str(err)
                 ,False);

   exit1(W_EXITCODE(err, SIGABRT));
  end else
  begin
   exit1(0);
  end;
  //

 end;
 //

end;

{
function NtTerminateProcessTrap(ProcessHandle:THANDLE;ExitStatus:DWORD):DWORD; MS_ABI_Default;
begin
 Result:=0;
 LOG_ERROR(stderr,'NtTerminateProcess:0x',HexStr(ExitStatus,8));
 print_backtrace(StdErr,Get_pc_addr,get_frame,0);
 print_backtrace_td(StdErr);
 asm
  mov ProcessHandle,%R10
  mov ExitStatus   ,%EDX
  mov $0x2c        ,%EAX
  syscall
 end;
end;

type
 t_jmp_rop=packed record
  cmd:WORD;  //FF 25
  ofs:DWORD; //00 00 00 00
  adr:QWORD;
 end;

Procedure CreateNtTerminateTrap;
var
 rop:t_jmp_rop;
 adr:Pointer;
 num:PTRUINT;
 R:Boolean;
begin
 rop.cmd:=$25FF;
 rop.ofs:=0;
 rop.adr:=QWORD(@NtTerminateProcessTrap);

 adr:=GetProcAddress(GetModuleHandle('ntdll.dll'),'NtTerminateProcess');

 num:=0;
 R:=WriteProcessMemory(GetCurrentProcess,adr,@rop,SizeOf(rop),num);
 LOG_TRACE('CreateNtTerminateTrap:0x',HexStr(adr),' ',R,' ',num);
end;
}

type
 TKevKqueue=class
  FClient:THostIpc;
  Fkq    :Pointer;
  Constructor Create(Client:THostIpc);
  Destructor  Destroy; override;
  procedure   UpdateKevent;
  procedure   WakeupKevent;
  function    OnKevChange(Client:THostIpc;Value:TIpcValue):TIpcValue;
 end;

procedure kq_wakeup(data:Pointer); SysV_ABI_CDecl;
begin
 TKevKqueue(data).WakeupKevent();
end;

Constructor TKevKqueue.Create(Client:THostIpc);
begin
 FClient:=Client;
 Fkq:=kern_kqueue2('[ipc]',@kq_wakeup,Pointer(Self));
end;

Destructor TKevKqueue.Destroy;
begin
 if (Fkq<>nil) then
 begin
  kqueue_close2(Fkq);
 end;
end;

procedure TKevKqueue.UpdateKevent;
var
 kev:array[0..7] of t_kevent;
 t:timespec;
 r:Integer;
begin
 if (Fkq=nil) then Exit;
 t:=Default(timespec);

 repeat

  r:=0;
  kern_kevent2(Fkq,nil,0,@kev,8,@t,@r);

  if (r>0) then
  begin
   FClient.InvokeAsyn(iKEV_EVENT.mtype,@kev,r*SizeOf(t_kevent));
  end;

 until (r<>8);
end;

procedure TKevKqueue.WakeupKevent;
begin
 UpdateKevent;
end;

function TKevKqueue.OnKevChange(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 kev:p_kevent;
 count:Integer;
 KevObj:TKevKqueue;
begin
 kev  :=Value.GetBuf;
 count:=Value.GetLen div SizeOf(t_kevent);

 with THostIpcConnect(Client) do
 begin
  if (FKevObj=nil) then
  begin
   FKevObj:=TKevKqueue.Create(Client);
  end;
  KevObj:=TKevKqueue(FKevObj);
 end;

 //changelist
 Result:=kern_kevent2(KevObj.Fkq,kev,count,nil,0,nil,@count);
end;

type
 TMountConfigInvoke=object
  function OnGetMountConfig(Client:THostIpc;Value:TIpcValue):TIpcValue;
 end;

function TMountConfigInvoke.OnGetMountConfig(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TGameMountConfigExport;
begin
 data:=GameMountConfigExport;

 Result:=TIpcValue.&Object(data);

 FreeAndNil(data);
end;

procedure game_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 td:p_kthread;
 r:Integer;
 ppid:Integer;

 pipefd:THandle;
 parent:THandle;

 IpcHandler:THostIpcHandler;
 kipc:THostIpcPipe;

 mem:TPCharStream;
 GameStartupInfo:TGameStartupInfo;
begin
 //while not IsDebuggerPresent do sleep(100);

 mem:=TPCharStream.Create(data,size);

 GameStartupInfo:=TGameStartupInfo.Create(True);
 GameStartupInfo.Deserialize(mem);

 mem.Free;

 //free shared
 FreeMem(data);

 ppid:=md_getppid;

 LOG_INFO('game_process started pid:',GetProcessID,' parent_pid:',ppid);

 parent:=md_pidfd_open(ppid);

 pipefd:=GameStartupInfo.Pipe;
 pipefd:=md_pidfd_getfd(parent,pipefd);

 md_pidfd_close(parent);

 IpcHandler:=THostIpcHandler.Create;
 IpcHandler.AddCallback(iKEV_CHANGE.msg ,@TKevKqueue(nil).OnKevChange);
 IpcHandler.AddCallback('GetMountConfig',@TMountConfigInvoke(nil^).OnGetMountConfig);

 kipc:=THostIpcPipe.Create(THostIpcDispatchKern.Create(IpcHandler));
 kipc.set_pipe(pipefd);

 p_host_ipc:=kipc;

 //CreateNtTerminateTrap;

 p_is_fork:=True;

 td:=nil;
 r:=kthread_add(@prepare,GameStartupInfo,@td,0,'[main]');
 Assert(r=0);

 msleep_td(0);
end;

function GetRequiredHdcpType(attribute,disp_location_1,disp_location_2:DWORD):Byte; inline;
begin
 Result:=0;
 if (((disp_location_1+1) and 2)=0) and (((disp_location_2+1) and 2)=0) then
 begin
  if ((attribute and $200)=0) then Exit;
 end else
 begin
  if ((attribute and $400)<>0) then Exit;
 end;
 Result:=1;
end;

{
 isInitUserAlwaysLogin
   (m_attribute & 1) == 0

 isBgSuspend
   (m_attribute & 0x10) != 0

 isBgSuspendIfSpecial
   (m_attribute & 0x100) != 0

 //Best effort threads use 2 CPU cores. [0xC0] [m_type=MINI_APP,BIG_APP] [SdkVersion < 0x3000000]
 IsNotSetBestEffortOnNewProcess
   ((&param_1->m_attribute + 2) & 0x20) >> 5) != 0

 IsNotCheckWorkaroundExtraUsbAudioDevice
   (((&param_1->m_attribute + 2) & 4) >> 2) != 0

 ///////////////

 isEnlargeFmem256mb
   (&m_attribute2 + 1) & 0x40) >> 6) != 0

 savedata:[ServerCmdBackup]
  if ((compiledSdkVer < 0x4500000) && ( (((&ATTRIBUTE2 + 2) & 2) >> 1) == false )) {
    slot = 1;
  }
  else {
    slot = 2;
  }
  sceFsISSchedConfigCurrentThread(slot,4);

}

{

 int GetFormatTypeFromATTRIBUTE2(uint ATTRIBUTE2,int *p_FormatType)
 {
   *p_FormatTyp = 2 - (uint)((ATTRIBUTE2 & 0x400) == 0);
   return 0;
 }

 if (FormatType == 1) {
   "FormatUfs";   sceFsUfsMkfs
 }
 else {
   "FormatUfsFC"; sceFsUfsMkfsWithFixedCylinderGroupSize
 }

}


function run_item(const cfg:TGameRunConfig;var Context:TGameRunContext):Integer;
label
 _error;
var
 r:Integer;

 kern2mgui:t_pipe_pair;

 fork_info:t_fork_proc;

 kev:t_kevent;

 IpcHandler:THostIpcHandler;

 p_mgui_ipc:THostIpcPipe;

 s_kern_ipc:THostIpcSimple;
 s_mgui_ipc:THostIpcSimple;

 GameStartupInfo:TGameStartupInfo;
 mem:TMemoryStream;
begin
 Result:=0;
 r:=0;

 GameStartupInfo:=TGameStartupInfo.Create(False);
 GameStartupInfo.FConfInfo:=cfg.FConfInfo;
 GameStartupInfo.FGameItem:=cfg.FGameItem;
 GameStartupInfo.LoadExec :=cfg.FLoadExec;

 GameStartupInfo.LocalDir   :=GetAppConfigDir(False);
 GameStartupInfo.Category   :='gd'; //m_type = SCE_LNC_APP_TYPE_BIG_APP;
 GameStartupInfo.APP_VER    :='01.00';
 GameStartupInfo.hasParamSfo:=ord(cfg.FParamSfo<>nil);

 if (cfg.FParamSfo<>nil) then
 begin

  GameStartupInfo.CATEGORY                        :=cfg.FParamSfo.GetString('CATEGORY');
  GameStartupInfo.TITLE                           :=cfg.FParamSfo.GetString('TITLE');
  GameStartupInfo.TITLE_ID                        :=cfg.FParamSfo.GetString('TITLE_ID');
  GameStartupInfo.CONTENT_ID                      :=cfg.FParamSfo.GetString('CONTENT_ID');
  GameStartupInfo.INSTALL_DIR_SAVEDATA            :=cfg.FParamSfo.GetString('INSTALL_DIR_SAVEDATA');
  GameStartupInfo.SAVE_DATA_TRANSFER_TITLE_ID_LIST:=cfg.FParamSfo.GetString('SAVE_DATA_TRANSFER_TITLE_ID_LIST');
  GameStartupInfo.APP_VER                         :=cfg.FParamSfo.GetString('APP_VER');

  GameStartupInfo.SYSTEM_VER           :=cfg.FParamSfo.GetUInt('SYSTEM_VER');
  GameStartupInfo.ATTRIBUTE            :=cfg.FParamSfo.GetUInt('ATTRIBUTE');
  GameStartupInfo.ATTRIBUTE2           :=cfg.FParamSfo.GetUInt('ATTRIBUTE2');
  GameStartupInfo.ATTRIBUTE_EXE        :=cfg.FParamSfo.GetUInt('ATTRIBUTE_EXE');
  GameStartupInfo.SELF_2MIB_PAGE_AMOUNT:=cfg.FParamSfo.GetUInt('SELF_2MIB_PAGE_AMOUNT');

  GameStartupInfo.RequiredHdcpType     :=GetRequiredHdcpType(
                                          GameStartupInfo.ATTRIBUTE,
                                          cfg.FParamSfo.GetUInt('DISP_LOCATION_1'),
                                          cfg.FParamSfo.GetUInt('DISP_LOCATION_2')
                                         );

  if ((GameStartupInfo.ATTRIBUTE2 and $40)=0) then
  begin
   GameStartupInfo.DownloadMb_0:=cfg.FParamSfo.GetUInt('DOWNLOAD_DATA_SIZE');
  end;


  if ((GameStartupInfo.ATTRIBUTE2 and $2000)=0) then
  begin
   GameStartupInfo.DownloadMb_1:=cfg.FParamSfo.GetUInt('DOWNLOAD_DATA_SIZE_1');
  end;

 end;

 ////
 StdOutputHandle:=cfg.hOutput;
 StdErrorHandle :=cfg.hError ;

 //reinit std I/O
 SysInitStdIO;
  ////

 fork_info:=Default(t_fork_proc);

 if cfg.FConfInfo.MiscInfo.fork_proc then
 begin
  Context.FGameProcess:=TGameProcessPipe.Create;
  Context.FGameProcess.g_fork:=True;

  with TGameProcessPipe(Context.FGameProcess) do
  begin
   r:=md_pipe2(kern2mgui,MD_PIPE_ASYNC0 or MD_PIPE_ASYNC1);
   if (r<>0) then goto _error;

   p_mgui_ipc:=THostIpcPipe.Create(Context.FIpcDispatch);
   p_mgui_ipc.set_pipe(kern2mgui[0]);

   g_ipc:=p_mgui_ipc;
   FChildpip:=kern2mgui[1];
  end;

  //

  mem:=TMemoryStream.Create;

  GameStartupInfo.FPipe:=kern2mgui[1];
  GameStartupInfo.Serialize(mem);
  FreeAndNil(GameStartupInfo);

  fork_info.hInput :=StdInputHandle;
  fork_info.hOutput:=cfg.hOutput;
  fork_info.hError :=cfg.hError;

  fork_info.proc:=@game_process;
  fork_info.data:=mem.Memory;
  fork_info.size:=mem.Size;

  r:=md_fork_process(fork_info,MD_FORK_PDEATHSIG or MD_FORK_PGAMEVMA);

  mem.Free;
 end else
 begin
  Context.FGameProcess:=TGameProcessSimple.Create;
  Context.FGameProcess.g_fork:=False;

  with TGameProcessSimple(Context.FGameProcess) do
  begin

   IpcHandler:=THostIpcHandler.Create;
   IpcHandler.AddCallback(iKEV_CHANGE.msg,@TKevKqueue(nil).OnKevChange);
   IpcHandler.AddCallback('GetMountConfig',@TMountConfigInvoke(nil^).OnGetMountConfig);

   s_kern_ipc:=THostIpcSimple.Create(THostIpcDispatchKern.Create(IpcHandler));
   s_mgui_ipc:=THostIpcSimple.Create(Context.FIpcDispatch);

   s_kern_ipc.FDest:=s_mgui_ipc;
   s_mgui_ipc.FDest:=s_kern_ipc;

   g_ipc:=s_mgui_ipc;

   p_host_ipc:=s_kern_ipc;

   Ftd:=nil;
   r:=kthread_add(@prepare,GameStartupInfo,@Ftd,0,'[main]');

   fork_info.fork_pid:=GetProcessID;
  end;

 end;

 if (r<>0) then
 begin
  _error:
  FreeAndNil(Context.FGameProcess);
  Exit(r);
 end;

 Context.FGameProcess.g_proc :=fork_info.hProcess;
 Context.FGameProcess.g_p_pid:=fork_info.fork_pid;

 Context.FIpcDispatch.thread_new;

 kev.ident :=fork_info.fork_pid;
 kev.filter:=EVFILT_PROC;
 kev.flags :=EV_ADD;
 kev.fflags:=NOTE_EXIT or NOTE_EXEC;
 kev.data  :=0;
 kev.udata :=nil;

 Context.FGameProcess.g_ipc.kevent(@kev,1);

 if (not cfg.FLoadExec) then
 begin
  Context.FParamSfo:=cfg.FParamSfo;
  Context.FGameItem:=cfg.FGameItem;
  Context.FGameItem.FLock:=True;
 end;

end;


end.



