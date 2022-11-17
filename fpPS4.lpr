
uses
 {$IFDEF Linux}
 cmem,
 cthreads,
 {$ENDIF}
 windows,
 seh64,
 Classes,
 sysutils,
 stub_manager,
 sys_crt,
 sys_types,
 sys_pthread,
 sys_path,
 ps4libdoc,
 ps4_libkernel,
 ps4_libSceLibcInternal,
 ps4_libSceScreenShot,
 ps4_libSceRtc,
 ps4_libSceNpSignaling,
 ps4_libSceNpMatching2,
 ps4_libSceRemoteplay,
 ps4_libSceAjm,
 ps4_libSceMouse,
 ps4_libSceIme,
 ps4_libSceMove,
 ps4_libScePlayGo,
 ps4_libSceDiscMap,
 ps4_libSceAppContent,
 ps4_libSceNet,
 ps4_libSceHttp,
 ps4_libSceGnmDriver,
 ps4_libSceNpScore,
 ps4_libSceNpTrophy,
 ps4_libSceSystemService,
 ps4_libSceNpManager,
 ps4_libSceNpGameIntent,
 ps4_libSceSaveData,
 ps4_libSceDialogs,
 ps4_libSceUserService,
 ps4_libSceAudioOut,
 ps4_libSceVideoOut,
 ps4_libScePad,
 ps4_libSceNpWebApi,
 ps4_elf,
 ps4_pthread,
 ps4_program,
 ps4_elf_tls,

 ps4_videodrv,
 vMemory,
 vImageManager,

 trace_manager;

function ParseCmd:Boolean;
var
 i,n:Integer;
label
 promo;
begin
 if (ParamCount=0) then
 begin
  promo:
  Writeln('fpPS4 '+{$I tag.inc});
  Writeln('Copyright (c) 2021-2022 by Red_prig');
  Writeln('PS4 compatibility layer (emulator) on Free Pascal '+{$I %FPCVERSION%});
  Writeln(' Parameters:');
  Writeln('  -e <name>  //decrypted elf or self file name');
  Writeln('  -f <name>  //folder of app   (/app0)');
  Writeln('  -p <name>  //folder of patch (/app1)');
  Writeln('  -s <name>  //savedata path');

  Writeln('  -h <name>  //enable hack');
  Writeln('     DEPTH_DISABLE_HACK   //disable depth buffer');
  Writeln('     COMPUTE_DISABLE_HACK //disable compute shaders');
  Writeln('     MEMORY_BOUND_HACK    //limit the amount of GPU allocated memory (iGPU)');
  Writeln('     IMAGE_TEST_HACK      //always mark that the texture has changed');
  Writeln('     IMAGE_LOAD_HACK      //never reload texture');

  Exit(False);
 end;

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
    '-e':n:=0;
    '-f':n:=1;
    '-p':n:=2;
    '-s':n:=3;
    '-h':n:=4;
   else
     if (n<>-1) then
     begin
      Case n of
       0:begin
          if (ps4_app.app0_file<>'') then Goto promo;
          ps4_app.app0_file:=Trim(ParamStr(i));
          if (ps4_app.app0_path='') then
          begin
           ps4_app.app0_path:=ExtractFileDir(ps4_app.app0_file);
           if (ExcludeLeadingPathDelimiter(ps4_app.app0_path)='') then ps4_app.app0_path:=GetCurrentDir;
          end;
         end;
       1:begin
          ps4_app.app0_path:=Trim(ParamStr(i));
          if (ExcludeLeadingPathDelimiter(ps4_app.app0_path)='') then ps4_app.app0_path:=GetCurrentDir;
         end;
       2:begin
          ps4_app.app1_path:=Trim(ParamStr(i));
          if (ExcludeLeadingPathDelimiter(ps4_app.app1_path)='') then ps4_app.app1_path:=GetCurrentDir;
         end;
       3:begin
          ps4_app.save_path:=Trim(ParamStr(i));
         end;
       4:begin
          case UpperCase(ParamStr(i)) of
           'DEPTH_DISABLE_HACK'  :ps4_videodrv.DEPTH_DISABLE_HACK:=True;
           'COMPUTE_DISABLE_HACK':ps4_videodrv.COMPUTE_DISABLE_HACK:=True;
           'MEMORY_BOUND_HACK'   :vMemory.MEMORY_BOUND_HACK:=True;
           'IMAGE_TEST_HACK'     :vImageManager.IMAGE_TEST_HACK:=True;
           'IMAGE_LOAD_HACK'     :vImageManager.IMAGE_LOAD_HACK:=True;
           else;
          end;
         end;
      end;
      n:=-1;
     end;
  end;
 end;

 if (ps4_app.app0_file='') or (ps4_app.app0_path='') or (ps4_app.save_path='') then Goto promo;

 if (ps4_app.app1_path=ps4_app.app0_path) then
 begin
  ps4_app.app1_path:='';
 end;

 if not FileExists(ps4_app.app0_file) then
 begin
  Writeln(StdErr,'File not found:',ps4_app.app0_file);
  Writeln;
  Goto promo;
 end;

 if not DirectoryExists(ps4_app.app0_path) then
 begin
  Writeln(StdErr,'Path not found:',ps4_app.app0_path);
  Writeln;
  Goto promo;
 end;

 if (ps4_app.app1_path<>'') then
 if not DirectoryExists(ps4_app.app1_path) then
 begin
  Writeln(StdErr,'Path not found:',ps4_app.app1_path);
  Writeln;
  Goto promo;
 end;

 Result:=True;
end;

{
type
 _TElf_node=class(TElf_node)
 end;

procedure Print_libs(node:TElf_node);
var
 i,l:SizeInt;
 lib:PLIBRARY;
begin
 l:=Length(_TElf_node(node).aLibs);
 if (l<>0) then
 begin
  For i:=0 to l-1 do
  begin
   lib:=_TElf_node(node).aLibs[i];
   Writeln(hexStr(lib));
   if lib<>nil then
    Writeln(lib^.Import,' ',lib^.strName);
  end;
 end;
end;
}

var
 Stub:TStubMemoryProc;

procedure _nop_stub; assembler; nostackframe;
asm
 xor %rax,%rax
end;

procedure print_stub(nid:QWORD;lib:PLIBRARY); MS_ABI_Default;
begin
 Writeln(StdErr,'nop nid:',lib^.strName,':',HexStr(nid,16),':',ps4libdoc.GetFunctName(nid));
 //DebugBreak;
 Sleep(INFINITE);
 //readln;
 //Print_libs(ps4_app.GetFile('libc.prx'));
end;

function ps4_sceSslInit(poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslInit:',poolSize);
 Result:=3;
end;

function ps4_sceHttp2Init(libnetMemId,libsslCtxId:Integer;
                          poolSize:size_t;
                          maxConcurrentRequest:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceHttp2Init:',poolSize);
 Result:=3;
end;

function ps4_sceVoiceQoSInit(
          pMemBlock:Pointer;
          memSize:DWORD;
          appType:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceUltInitialize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceGameLiveStreamingInitialize(heapSize:qword):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGameLiveStreamingInitialize:',heapSize);
 Result:=0;
end;

function ps4_sceSharePlayInitialize(pHeap:Pointer;heapSize:qword):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSharePlayInitialize:',HexStr(pHeap),':',heapSize);
 Result:=0;
end;

function ResolveImport(elf:Telf_file;Info:PResolveImportInfo;data:Pointer):Pointer;
var
 lib:PLIBRARY;
begin
 Result:=nil;

 //cache
 Result:=Info^.lib^.get_proc(Info^.nid);
 if (Result<>nil) then
 begin
  //Writeln('Cache^:',Info^.lib^.strName,':',ps4libdoc.GetFunctName(Info^.Nid));
  Exit;
 end;

 lib:=ps4_app.GetLib(Info^.lib^.strName);
 if (lib<>nil) then
 begin
  Result:=lib^.get_proc(Info^.Nid);
 end;

 //Writeln('Resolve:',Info^.lib^.strName,':',ps4libdoc.GetFunctName(Info^.Nid));

 if (Result=nil) then
 begin
  Case Info^.lib^.strName of

   'libSceSsl':
    Case Info^.nid of
     QWORD($85DA551140C55B7B):Result:=@ps4_sceSslInit;
    end;

   'libSceHttp2':
    Case Info^.nid of
     QWORD($DC909EDE509B43C0):Result:=@ps4_sceHttp2Init;
    end;

    'libSceVoiceQoS':
    Case Info^.nid of
     QWORD($53C21F365EBF0ACB):Result:=@ps4_sceVoiceQoSInit;
    end;

    'libSceUlt':
    Case Info^.nid of
     QWORD($859220D44586B073):Result:=@ps4_sceUltInitialize;
    end;

    'libSceGameLiveStreaming':
    Case Info^.nid of
     QWORD($92F604C369419DD9):Result:=@ps4_sceGameLiveStreamingInitialize;
    end;

    'libSceSharePlay':
    Case Info^.nid of
     QWORD($8ACAEEAAD86961CC):Result:=@ps4_sceSharePlayInitialize;
    end;

  end;
 end;

 {
 if (Result<>nil) and ((Info^.sType=STT_FUN) or (Info^.sType=STT_SCE)) then //trace
 begin

 Case Info^.lib^.strName of
  'libSceNpToolkit':;
  'libSceSaveDataDialog':;
  'libSceVideoOut':;
  'libSceSystemService':;
  'libSceUserService':;
  'libSceNetCtl':;
  'libSceNpManager':;
  'libSceNpManagerForToolkit':;
  'libScePad':;
  'libSceFios2':;
  'libSceGnmDriver':;
  'libSceAjm':;
  'libSceAudioOut':;
  'libc':;
  'libSceLibcInternal':;
  'libScePosix':;
  'libSceDiscMap':;
  else
   Case RawByteString(ps4libdoc.GetFunctName(Info^.Nid)) of

    'scePadGetControllerInformation':;
    'scePadRead':;
    'scePadSetVibration':;

    'sceUserServiceGetLoginUserIdList':;
    'sceSystemServiceGetStatus':;

    'sceKernelDebugRaiseException':;
    'sceKernelDebugRaiseExceptionOnReleaseMode':;
    '_sigprocmask':;
    '__error':;
    'sceKernelClearEventFlag':;
    'sceKernelWaitEventFlag':;
    'sceKernelSetEventFlag':;
    'sceNetCtlCheckCallbackForNpToolkit':;
    'sceKernelReadTsc':;
    'scePthreadCondSignal':;
    'scePthreadCondTimedwait':;
    'scePthreadYield':;
    'nanosleep':;
    'sceKernelGetProcessTime':;
    'sceKernelGetProcessTimeCounter':;
    'clock_gettime':;
    'pthread_mutex_init':;
    'sceKernelLseek':;
    'scePthreadMutexDestroy':;
    'sceKernelRead':;
    'sceKernelSignalSema':;
    'sceKernelWaitSema':;
    'scePthreadMutexInit':;
    'scePthreadMutexattrInit':;
    'scePthreadMutexattrDestroy':;
    'scePthreadMutexattrSettype':;
    'scePthreadMutexTrylock':;
    'scePthreadMutexLock':;
    'scePthreadMutexUnlock':;
    'pthread_self':;
    'scePthreadSelf':;
    'scePthreadEqual':;
    'sceKernelGettimeofday':;
    'sceKernelClockGettime':;
    'pthread_mutex_lock':;
    'pthread_mutex_unlock':;
    'sceKernelPread':;
    'sceKernelClose':;
    'sceDiscMapIsRequestOnHDD':;
    //'Unknow':;
    'sceFiosIOFilterPsarcDearchiver':;
    'sceFiosFHReadSync':;
    'sceFiosFHTell':;
    'sceNgs2VoiceGetState':;
    'sceNgs2SystemRender':;
    'sceAudioOutOutputs':;
    '__tls_get_addr':;
    'scePthreadRwlockRdlock':;
    'scePthreadRwlockUnlock':;
    'scePthreadCondBroadcast':;
    'sceFiosFHCloseSync':;
    'sceKernelStat':;
    'sceKernelOpen':;
    'sceKernelUsleep':;
    '_write':;
    else
     begin
      Result:=TStubMemoryTrace(Stub).NewTraceStub(Info^.Nid,Info^.lib,Result,@_trace_enter,@_trace_exit);
     end;
   end;
 end;

 end;
 }

 if (Result=nil) then
 begin
  if (Info^.sType=STT_FUN) or (Info^.sType=STT_SCE) then
  begin
   Result:=Stub.NewNopStub(Info^.Nid,Info^.lib,@print_stub);
  end else
  if (Info^.sBind<>STB_WEAK) then
  begin
   Writeln(StdErr,'Warn^:',Info^.lib^.strName,':',ps4libdoc.GetFunctName(Info^.Nid),':',HexStr(Info^.Nid,16));
  end;

 end;

 if (Result<>nil) then //cache
 begin
  Info^.lib^.set_proc(Info^.nid,Result);
 end;
end;

function ReloadImport(elf:Telf_file;Info:PResolveImportInfo;data:Pointer):Pointer;
var
 node:Telf_file;
 lib:PLIBRARY;
begin
 //prev
 Result:=Info^.lib^.get_proc(Info^.nid);

 node:=Telf_file(data);

 lib:=ps4_app.GetLib(Info^.lib^.strName);
 if (lib=nil) then Exit;

 if (lib^.parent<>node) then Exit;

 Result:=lib^.get_proc(Info^.Nid);

 //cache
 Info^.lib^.set_proc(Info^.nid,Result);
end;

var
 main:pthread;

{$R *.res}

procedure LoadProgram;
var
 elf:Telf_file;
 f:RawByteString;
begin
 elf:=nil;

 if (ps4_app.app1_path<>'') then
 begin
  //first try patch
  f:='';
  if (parse_filename('/app1/eboot.bin',f)=PT_FILE) then
  begin
   elf:=Telf_file(LoadPs4ElfFromFile(f));
  end;
 end;

 if (elf=nil) then
 begin
  //second try app0_file
  elf:=Telf_file(LoadPs4ElfFromFile(ps4_app.app0_file));
 end;

 Assert(elf<>nil,'program not loaded!');

 ps4_app.prog:=elf;
end;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;
 sys_crt_init;

 ps4_app.save_path:=IncludeTrailingPathDelimiter(GetCurrentDir)+'savedata';
 if not ParseCmd then Exit;

 //ps4_app.app_path:='..\samples\api_gnm\simplet-single-triangle';
 //ps4_app.app_file:='..\samples\api_gnm\simplet-single-triangle\simplet-single-triangle_debug.elf';

 //ps4_app.app_file:='..\samples\api_gnm\simplet-cmask\simplet-cmask_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm\simplet-simple-fs\';
 //ps4_app.app_file:='..\samples\api_gnm\simplet-simple-fs\simplet-simple-fs_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm\';
 //ps4_app.app_file:='..\samples\api_gnm\basic-sample\basic-sample_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm\';
 //ps4_app.app_file:='..\samples\api_gnm\anisotropy-sample\anisotropy-sample_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm\';
 //ps4_app.app_file:='..\samples\api_gnm\depth-mode-sample\depth-mode-sample_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm\';
 //ps4_app.app_file:='..\samples\api_gnm\eqaa-sample\eqaa-sample_debug.elf';

 //ps4_app.app_path:='..\samples\tutorial_graphics_programming\basic_quad\';
 //ps4_app.app_file:='..\samples\tutorial_graphics_programming\basic_quad\basic_quad_debug.elf';

 //ps4_app.app_path:='..\samples\tutorial_anti-aliasing\';
 //ps4_app.app_file:='..\samples\tutorial_anti-aliasing\tutorial_anti-aliasing_debug.elf';

 //ps4_app.app_path:='..\samples\tutorial_graphics_programming\basic-compute\';
 //ps4_app.app_file:='..\samples\tutorial_graphics_programming\basic-compute\basic-compute_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm';
 //ps4_app.app_file:='..\samples\api_gnm\drawindirect-sample\drawindirect-sample_debug.elf';

 //ps4_app.app_file:='..\samples\api_video_out\videoout_cursor.elf';
 //ps4_app.app_file:='..\samples\api_video_out\videoout_flip.elf';

 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic2.elf';

 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic3.elf';
 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic5.elf';

 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic_1d.elf';

 //ps4_app.app_path:='..\samples\http_get\';
 //ps4_app.app_file:='..\samples\http_get\simple5.elf';

 //ps4_app.app_path:='G:\Games\MOMODORA\CUSA05694\';
 //ps4_app.app_file:='G:\Games\MOMODORA\CUSA05694\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\MOMODORA\CUSA05694\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\MOMODORA\CUSA05694\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\eboot.bin';

 //ps4_app.app_path:='G:\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\';
 //ps4_app.app_file:='G:\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\eboot.elf';
 //ps4_app.app_file:='G:\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Organ.Trail.Complete.Edition\CUSA02791\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Organ.Trail.Complete.Edition\CUSA02791\eboot.bin';

 //ps4_app.app_path:='G:\Games\Organ.Trail.Complete.Edition\CUSA02791\';
 //ps4_app.app_file:='G:\Games\Organ.Trail.Complete.Edition\CUSA02791\eboot.bin';

 //ps4_app.app_path:='G:\Games\Bloodborne Game of the Year Edition v1.09 [RUS]\';
 //ps4_app.app_file:='G:\Games\Bloodborne Game of the Year Edition v1.09 [RUS]\eboot.bin';

 //ps4_app.app_path:='G:\Games\BLAZING_CHROME\CUSA14656\';
 //ps4_app.app_file:='G:\Games\BLAZING_CHROME\CUSA14656\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\BLAZING_CHROME\CUSA14656';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\BLAZING_CHROME\CUSA14656\eboot.bin';

 //ps4_app.app_path:='G:\Games\Sonic Mania\CUSA07023\';
 //ps4_app.app_file:='G:\Games\Sonic Mania\CUSA07023\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Sonic Mania\CUSA07023\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Sonic Mania\CUSA07023\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Worms\CUSA04047\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Worms\CUSA04047\eboot.bin';

 //ps4_app.app_path:='G:\Games\Worms\CUSA04047\';
 //ps4_app.app_file:='G:\Games\Worms\CUSA04047\eboot.bin';

 //ps4_app.app_path:='G:\Games\Super Meat Boy\';
 //ps4_app.app_file:='G:\Games\Super Meat Boy\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Super Meat Boy\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Super Meat Boy\eboot.bin';

 //ps4_app.app_path:='G:\Games\Limbo\CUSA01369\';
 //ps4_app.app_file:='G:\Games\Limbo\CUSA01369\eboot.bin';

 //ps4_app.app_path:='G:\Games\Hue\CUSA05065\';
 //ps4_app.app_file:='G:\Games\Hue\CUSA05065\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Hue\CUSA05065\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Hue\CUSA05065\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\creeperdiver-ps4\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\creeperdiver-ps4\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\SPELUNKYCLASSIC\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\SPELUNKYCLASSIC\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Cursed Castilla v1.00 (ASIA)\CUSA07773\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Cursed Castilla v1.00 (ASIA)\CUSA07773\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Undertale\CUSA09415\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Undertale\CUSA09415\eboot.bin';

 //ps4_app.app_path:='G:\Games\Undertale\CUSA09415\';
 //ps4_app.app_file:='G:\Games\Undertale\CUSA09415\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Cladun Returns This Is Sengoku v1.00\CUSA06770\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Cladun Returns This Is Sengoku v1.00\CUSA06770\eboot.bin';

 //ps4_app.app_path:='G:\Games\Cladun Returns This Is Sengoku v1.00\CUSA06770\';
 //ps4_app.app_file:='G:\Games\Cladun Returns This Is Sengoku v1.00\CUSA06770\eboot.bin';

 //ps4_app.app_path:='G:\Games\ps4-homebrew\Blue Boi  Quickz\';
 //ps4_app.app_file:='G:\Games\ps4-homebrew\Blue Boi  Quickz\eboot.bin';

 //ps4_app.app_path:='G:\Games\Shovel Knight\CUSA01867\';
 //ps4_app.app_file:='G:\Games\Shovel Knight\CUSA01867\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Stardew_Valley\CUSA06829\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Stardew_Valley\CUSA06829\eboot.bin';

 //ps4_app.app_path:='G:\Games\Stardew_Valley\CUSA06829\';
 //ps4_app.app_file:='G:\Games\Stardew_Valley\CUSA06829\eboot.bin';

 //ps4_app.app_path:='G:\Games\Super Exploding Zoo\CUSA00446\';
 //ps4_app.app_file:='G:\Games\Super Exploding Zoo\CUSA00446\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Super.Exploding.Zoo\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Super.Exploding.Zoo\eboot.bin';

 //ps4_app.app_path:='G:\Games\Untitled Goose Game\CUSA23079\';
 //ps4_app.app_file:='G:\Games\Untitled Goose Game\CUSA23079\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Untitled Goose Game\CUSA23079\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Untitled Goose Game\CUSA23079\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\JETPACKJOYRIDE\CUSA03633\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\JETPACKJOYRIDE\CUSA03633\eboot.bin';

 //ps4_app.app_path:='G:\Games\JETPACKJOYRIDE\CUSA03633\';
 //ps4_app.app_file:='G:\Games\JETPACKJOYRIDE\CUSA03633\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Dont.Die.Mr.Robot\CUSA02782\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Dont.Die.Mr.Robot\CUSA02782\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Patapon\CUSA07184\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Patapon\CUSA07184\eboot.bin';

 //ps4_app.app_path:='G:\Games\Patapon\CUSA07184\';
 //ps4_app.app_file:='G:\Games\Patapon\CUSA07184\eboot.bin';

 //ps4_app.app_path:='G:\Games\Bards.Gold\CUSA05012\';
 //ps4_app.app_file:='G:\Games\Bards.Gold\CUSA05012\eboot.bin';

 //ps4_app.app_path:='G:\Games\Record of Lodoss War Deedlit in Wonder Labyrinth\CUSA29366\';
 //ps4_app.app_file:='G:\Games\Record of Lodoss War Deedlit in Wonder Labyrinth\CUSA29366\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\LODOSSWARDEEDLIT\CUSA29366\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\LODOSSWARDEEDLIT\CUSA29366\eboot.bin';

 //ps4_app.app_path:='G:\Games\Spelunky 2\CUSA20601\';
 //ps4_app.app_file:='G:\Games\Spelunky 2\CUSA20601\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\SPELUNKY2\CUSA20601\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\SPELUNKY2\CUSA20601\eboot.bin';

 //ps4_app.app_path:='G:\Games\Desert.Child\CUSA12744\';
 //ps4_app.app_file:='G:\Games\Desert.Child\CUSA12744\eboot.bin';

 //ps4_app.app_path:='G:\Games\Blackhole\CUSA06921\';
 //ps4_app.app_file:='G:\Games\Blackhole\CUSA06921\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Blackhole\CUSA06921\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Blackhole\CUSA06921\eboot.bin';

 //ps4_app.app_path:='G:\Games\Spelunky\CUSA00491\';
 //ps4_app.app_file:='G:\Games\Spelunky\CUSA00491\eboot.bin';

 //ps4_app.app_path:='G:\Games\Mega Man Legacy Collection v1.00\CUSA02516\';
 //ps4_app.app_file:='G:\Games\Mega Man Legacy Collection v1.00\CUSA02516\eboot.bin';

 //ps4_app.app_path:='G:\Games\ps4-homebrew\PS4_Player\';
 //ps4_app.app_file:='G:\Games\ps4-homebrew\PS4_Player\eboot.bin';

 //ps4_app.app_path:='G:\Games\ps4-homebrew\TEST_PAD\';
 //ps4_app.app_file:='G:\Games\ps4-homebrew\TEST_PAD\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\TEST_PAD\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\TEST_PAD\eboot.bin';

 //ps4_app.app_path:='G:\Games\Castlevania\SLUS00067\';
 //ps4_app.app_file:='G:\Games\Castlevania\SLUS00067\eboot.bin';

 //ps4_app.app_path:='G:\Games\Roombo First Blood\CUSA19205\';
 //ps4_app.app_file:='G:\Games\Roombo First Blood\CUSA19205\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\PS4_Player_9.00\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\PS4_Player_9.00\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\Eboot_Give\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\Eboot_Give\RayTracing3_eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Metal_Max_Xeno\CUSA12350\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Metal_Max_Xeno\CUSA12350\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Roombo First Blood\CUSA19205\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Roombo First Blood\CUSA19205\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\pad\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\pad\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Gem.Smashers\CUSA07572\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Gem.Smashers\CUSA07572\eboot.bin';

 //ps4_app.app_path:='G:\Games\Gem.Smashers\CUSA07572\';
 //ps4_app.app_file:='G:\Games\Gem.Smashers\CUSA07572\eboot.bin';

 //ps4_app.app_path:='G:\Games\Taiko No Tatsujin\CUSA07515\';
 //ps4_app.app_file:='G:\Games\Taiko No Tatsujin\CUSA07515\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Taiko_No_Tatsujin\CUSA07515\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Taiko_No_Tatsujin\CUSA07515\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\project_PM4_KernelEqueue\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\project_PM4_KernelEqueue\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Shantae Riskys Revenge\CUSA01587\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Shantae Riskys Revenge\CUSA01587\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\OpenOrbis\IPNGDRAW\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\OpenOrbis\IPNGDRAW\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\LAPY10018\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\LAPY10018\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Super Star Wars\CUSA03292\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Super Star Wars\CUSA03292\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\VA-11.Hall-A\CUSA15402\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\VA-11.Hall-A\CUSA15402\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\Quiz\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\Quiz\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\namco\uroot\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\namco\uroot\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\ps4-homebrew\PS4-Xplorer\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\ps4-homebrew\PS4-Xplorer\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Chronicles_of_Teddy_Harmony_of_Exidus\CUSA03328\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Chronicles_of_Teddy_Harmony_of_Exidus\CUSA03328\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Another_World\CUSA00602\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Another_World\CUSA00602\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Dino.Dinis.Kick.Off.Revival\CUSA03453\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Dino.Dinis.Kick.Off.Revival\CUSA03453\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Cat.Quest\CUSA09499\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Cat.Quest\CUSA09499\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Kitten.Squad\CUSA04801\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Kitten.Squad\CUSA04801\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Mitsurugi.Kamui.Hikae\CUSA02166\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Mitsurugi.Kamui.Hikae\CUSA02166\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Prison_Architect\CUSA03487\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Prison_Architect\CUSA03487\eboot.bin';

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Pumped.BMX.Plus.PS4-PRELUDE\CUSA02589\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Pumped.BMX.Plus.PS4-PRELUDE\CUSA02589\eboot.bin';

 ps4_app.resolve_cb:=@ResolveImport;
 ps4_app.reload_cb :=@ReloadImport;

 LoadProgram;
 ps4_app.prog.Prepare;

 ps4_app.RegistredElf    (ps4_app.prog);
 ps4_app.ResolveDepended (ps4_app.prog);
 ps4_app.LoadSymbolImport(nil);


 Stub.FinStub;
 ps4_app.InitProt;

 _pthread_run_entry(@main,GetSceUserMainThreadName,GetSceUserMainThreadStackSize);

 ps4_libSceVideoOut.App_Run;

 //KillALLThreads TODO
 //readln;
end.



