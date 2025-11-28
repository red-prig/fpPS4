unit game_run;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 Classes,
 SysUtils,
 CharStream,
 Dialogs,
 kern_thr,
 md_sleep,
 md_pipe,
 host_ipc,
 host_ipc_interface,
 md_host_ipc,
 param_sfo_gui,
 game_info,
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

function run_item(const cfg:TGameRunConfig):TGameProcess;

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
 ps4_libSceNpParty,
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
  //std_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  //std_tty[i].t_wr_handle:=t_wr_handle;
  //std_tty[i].t_update   :=@WakeMainThread;
 end;

 For i:=0 to High(deci_tty) do
 begin
  //deci_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
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
 kern_jit_ctx.jit_memory_guard    :=ConfInfo.JITInfo.memory_guard;

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

procedure free_params(argv:PPChar);
var
 curr:PPChar;
begin
 if (argv=nil) then Exit;
 curr:=argv;
 while (curr^<>nil) do
 begin
  FreeMem(curr^);
  Inc(curr);
 end;
 FreeMem(argv);
end;

function parse_params(const params:RawByteString;var argv:PPChar):Integer;
var
 curr:PChar;
 last:PChar;

 barg:PChar;
 blen:Integer;

 argc:Integer;

 state:char;

 procedure concat_arg(delta:Integer);
 var
  i:Integer;
 begin
  if (curr<>last) then
  begin
   i:=(curr-last);
   ReAllocMem(barg,blen+i+1); //zero truncate
   Move(last^,barg[blen],i);
   blen:=blen+i;
   barg[blen]:=#0;
  end;
  last:=curr+delta;
 end;

 procedure next_arg;
 begin
  if (barg<>nil) then
  begin
   ReAllocMem(argv,SizeOf(Pointer)*(argc+1+1)); //zero truncate
   argv[argc]:=barg;
   Inc(argc);
   argv[argc]:=nil; //truncate
   barg:=nil;       //reset
   blen:=0;         //reset
  end;
 end;

begin
 Result:=1;

 //init
 argc:=0;
 argv:=AllocMem(SizeOf(Pointer)*2);
 argv[0]:=nil; //truncate

 curr:=@params[1];
 last:=curr;

 barg:=nil;
 blen:=0;

 state:=#0;

 if (curr<>nil) then
 while (curr^<>#0) do
 begin

  case state of
   ' ':
     if (curr^<>' ') then
     begin
      last:=curr; //update pos
      state:=#0;
     end;
   '\':
     begin
      last:=curr; //update pos
      state:=#0;
      //skip
      Inc(curr);
      Continue;
     end;
   else;
  end;

  case curr^ of

   ' ':
     begin
      if (state=#0) then
      begin
       concat_arg(1);
       next_arg;
       state:=' ';
      end;
     end;

   '''',
    '"':
     begin
      if (state=#0) then
      begin
       concat_arg(1);
       state:=curr^;
      end else
      if (state=curr^) then
      begin
       concat_arg(1);
       state:=#0;
      end;
     end;

    '\':
     begin
      concat_arg(1);
      state:='\';
     end;

   else;
  end;

  Inc(curr);
 end;

 if (state<>' ') then
 begin
  concat_arg(0);
  next_arg;
 end;

 Result:=argc;
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
  THostIpcConnect(p_host_ipc).thread_new;
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

 Writeln('Name    :',Item.FGameInfo.Name      );
 Writeln('TitleId :',Item.FGameInfo.TitleId   );
 Writeln('Version :',Item.FGameInfo.Version   );
 Writeln('AppVer  :',Item.FGameInfo.AppVer    );
 Writeln('Exec    :',Item.FGameInfo.Exec      );

 Writeln('game    :',Item.FMountList.game     );
 Writeln('firmware:',Item.FMountList.firmware );

 Writeln('LocalDir:',GameStartupInfo.LocalDir );

 InitMount(GameStartupInfo);

 ///argv

 argv:=nil;
 argc:=parse_params(Item.FGameInfo.Exec,argv);

 Writeln('main_thread:',HexStr(curkthread));

 //
 FreeAndNil(GameStartupInfo);
 //

 Writeln('main_execve->');
 For i:=0 to argc-1 do
 begin
  Writeln(' argv[',i,']:',argv[i]);
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
 Writeln(stderr,'NtTerminateProcess:0x',HexStr(ExitStatus,8));
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
 Writeln('CreateNtTerminateTrap:0x',HexStr(adr),' ',R,' ',num);
end;
}

procedure fork_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 td:p_kthread;
 r:Integer;

 pipefd:THandle;
 parent:THandle;

 kipc:THostIpcPipeKERN;

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

 parent:=md_pidfd_open(md_getppid);

 pipefd:=GameStartupInfo.Pipe;
 pipefd:=md_pidfd_getfd(parent,pipefd);

 kipc:=THostIpcPipeKERN.Create;
 kipc.set_pipe(pipefd);

 p_host_ipc    :=kipc;
 p_host_handler:=THostIpcHandler.Create;
 p_host_ipc    .FHandler:=p_host_handler;

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
 (m_attribute & 1) == 0     || param_sfo_not_found

 isBgSuspend
 (m_attribute & 0x10) != 0  || param_sfo_not_found

 isBgSuspendIfSpecial
 (m_attribute & 0x100) != 0 || param_sfo_not_found
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

{

//Best effort threads use 2 CPU cores. [0xC0] [m_type=MINI_APP]
byte GetBesteffort(t_app_m_info *param_1)
{
  if (param_1->NewProcess != 0) {
    return 1;
  }
  if (param_1->m_is_param_sfo_not_found != 0) {
    return 0;
  }
  return (*(byte *)((long)&param_1->m_attribute + 2) & 0x20) >> 5;
}

}

function run_item(const cfg:TGameRunConfig):TGameProcess;
label
 _error;
var
 r:Integer;

 kern2mgui:array[0..1] of THandle;

 fork_info:t_fork_proc;

 kev:t_kevent;

 p_mgui_ipc:THostIpcPipeMGUI;

 s_kern_ipc:THostIpcSimpleKERN;
 s_mgui_ipc:THostIpcSimpleMGUI;

 GameStartupInfo:TGameStartupInfo;
 mem:TMemoryStream;
begin
 Result:=nil;
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

  GameStartupInfo.CATEGORY             :=cfg.FParamSfo.GetString('CATEGORY');
  GameStartupInfo.TITLE                :=cfg.FParamSfo.GetString('TITLE');
  GameStartupInfo.TITLE_ID             :=cfg.FParamSfo.GetString('TITLE_ID');
  GameStartupInfo.CONTENT_ID           :=cfg.FParamSfo.GetString('CONTENT_ID');
  GameStartupInfo.INSTALL_DIR_SAVEDATA :=cfg.FParamSfo.GetString('INSTALL_DIR_SAVEDATA');
  GameStartupInfo.APP_VER              :=cfg.FParamSfo.GetString('APP_VER');

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

 SetStdHandle(STD_OUTPUT_HANDLE,cfg.hOutput);
 SetStdHandle(STD_ERROR_HANDLE ,cfg.hError );

 fork_info:=Default(t_fork_proc);

 if cfg.FConfInfo.MiscInfo.fork_proc then
 begin
  Result:=TGameProcessPipe.Create;
  Result.g_fork:=True;

  with TGameProcessPipe(Result) do
  begin
   r:=md_pipe2(@kern2mgui,MD_PIPE_ASYNC0 or MD_PIPE_ASYNC1);
   if (r<>0) then goto _error;

   p_mgui_ipc:=THostIpcPipeMGUI.Create;
   p_mgui_ipc.set_pipe(kern2mgui[0]);

   g_ipc:=p_mgui_ipc;
   FChildpip:=kern2mgui[1];
  end;

  //

  mem:=TMemoryStream.Create;

  GameStartupInfo.FPipe:=kern2mgui[1];
  GameStartupInfo.Serialize(mem);
  FreeAndNil(GameStartupInfo);

  fork_info.hInput :=GetStdHandle(STD_INPUT_HANDLE);
  fork_info.hOutput:=cfg.hOutput;
  fork_info.hError :=cfg.hError;

  fork_info.proc:=@fork_process;
  fork_info.data:=mem.Memory;
  fork_info.size:=mem.Size;

  r:=md_fork_process(fork_info);

  mem.Free;
 end else
 begin
  Result:=TGameProcessSimple.Create;
  Result.g_fork:=False;

  with TGameProcessSimple(Result) do
  begin

   s_kern_ipc:=THostIpcSimpleKERN.Create;
   s_mgui_ipc:=THostIpcSimpleMGUI.Create;

   s_kern_ipc.FDest:=s_mgui_ipc;
   s_mgui_ipc.FDest:=s_kern_ipc;

   g_ipc:=s_mgui_ipc;

   p_host_ipc    :=s_kern_ipc;
   p_host_handler:=THostIpcHandler.Create;
   p_host_ipc    .FHandler:=p_host_handler;

   Ftd:=nil;
   r:=kthread_add(@prepare,GameStartupInfo,@Ftd,0,'[main]');

   fork_info.fork_pid:=GetProcessID;
  end;

 end;

 if (r<>0) then
 begin
  _error:
  ShowMessage('error run process code=0x'+HexStr(r,8));
  FreeAndNil(Result);
  Exit;
 end;

 Result.g_proc :=fork_info.hProcess;
 Result.g_p_pid:=fork_info.fork_pid;

 Result.g_ipc.thread_new;

 kev.ident :=fork_info.fork_pid;
 kev.filter:=EVFILT_PROC;
 kev.flags :=EV_ADD;
 kev.fflags:=NOTE_EXIT or NOTE_EXEC;
 kev.data  :=0;
 kev.udata :=nil;

 Result.g_ipc.kevent(@kev,1);

end;



end.



