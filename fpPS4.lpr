
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
 sys_types,
 sys_pthread,
 ps4libdoc,
 ps4_libSceRemoteplay,
 ps4_libSceAjm,
 ps4_libSceMouse,
 ps4_libSceIme,
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
 ps4_libSceSaveData,
 ps4_libSceDialogs,
 ps4_libSceUserService,
 ps4_libsceaudioout,
 ps4_libSceVideoOut,
 ps4_libScePad,
 ps4_libkernel,
 ps4_elf,
 ps4_pthread,
 ps4_program,
 ps4_elf_tls,

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
  Writeln('  -f <name>  //folder of app');
  Writeln('  -s <name>  //savedata path');
  Exit(False);
 end;

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
    '-e':n:=0;
    '-f':n:=1;
    '-s':n:=2;
   else
     if (n<>-1) then
     begin
      Case n of
       0:begin
          if (ps4_app.app_file<>'') then Goto promo;
          ps4_app.app_file:=Trim(ParamStr(i));
          if (ps4_app.app_path='') then
          begin
           ps4_app.app_path:=ExtractFileDir(ps4_app.app_file);
           if (ExcludeLeadingPathDelimiter(ps4_app.app_path)='') then ps4_app.app_path:=GetCurrentDir;
          end;
         end;
       1:begin
          ps4_app.app_path:=Trim(ParamStr(i));
          if (ExcludeLeadingPathDelimiter(ps4_app.app_path)='') then ps4_app.app_path:=GetCurrentDir;
         end;
       2:begin
          ps4_app.save_path:=Trim(ParamStr(i));
         end;
      end;
      n:=-1;
     end;
  end;
 end;

 if (ps4_app.app_file='') or (ps4_app.app_path='') or (ps4_app.save_path='') then Goto promo;

 if not FileExists(ps4_app.app_file) then
 begin
  Writeln('File not found:',ps4_app.app_file);
  Writeln;
  Goto promo;
 end;

 if not DirectoryExists(ps4_app.app_path) then
 begin
  Writeln('Path not found:',ps4_app.app_path);
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
 Writeln('nop nid:',lib^.strName,':',HexStr(nid,16),':',ps4libdoc.GetFunctName(nid));
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

function ps4_sceNpWebApiInitialize(libHttpCtxId:Integer;poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpWebApiInitialize:',libHttpCtxId,':',poolSize);
 Result:=4;
end;

function ps4_sceMoveInit:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMoveInit');
 Result:=0;
end;

function ps4_sceScreenShotSetOverlayImageWithOrigin(
          filePath:PChar;
          marginX:Integer;
          marginY:Integer;
          origin:Integer //SceScreenShotOrigin
         ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceScreenShotSetOverlayImageWithOrigin:',filePath);
 Result:=0;
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

 //if (lib<>nil) then
 //if (lib^.strName='mono-ps4') then
 //begin
 // Writeln(Info^.pName);
 //
 // if Info^.nid=$4FCEF2B219D790C5 then
 // begin
 //  writeln;
 // end;
 //
 //end;

 if (Result=nil) then
 begin
  Case Info^.lib^.strName of

   'libSceSsl':
    Case Info^.nid of
     QWORD($85DA551140C55B7B):Result:=@ps4_sceSslInit;
    end;

   'libSceNpWebApi':
    Case Info^.nid of
     QWORD($1B70272CD7510631):Result:=@ps4_sceNpWebApiInitialize;
    end;

   'libSceMove':
    Case Info^.nid of
     QWORD($8F521313F1282661):Result:=@ps4_sceMoveInit;
    end;

    'libSceScreenShot':
    Case Info^.nid of
     QWORD($EF7590E098F49C92):Result:=@ps4_sceScreenShotSetOverlayImageWithOrigin;
    end;

    'libSceVoiceQoS':
    Case Info^.nid of
     QWORD($53C21F365EBF0ACB):Result:=@ps4_sceVoiceQoSInit;
    end;

    'libSceUlt':
    Case Info^.nid of
     QWORD($859220D44586B073):Result:=@ps4_sceUltInitialize;
    end;

  end;
 end;

 if (Result<>nil) and (Info^.sType=STT_FUN) then //trace
 begin

 //Case Info^.lib^.strName of
 // 'libc':;
 // 'libSceLibcInternal':;
 // else
 //  Case RawByteString(ps4libdoc.GetFunctName(Info^.Nid)) of
 //   'scePthreadMutexInit':;
 //   'scePthreadMutexattrInit':;
 //   'scePthreadMutexattrDestroy':;
 //   'scePthreadMutexattrSettype':;
 //   'scePthreadMutexTrylock':;
 //   'scePthreadMutexLock':;
 //   'scePthreadMutexUnlock':;
 //   'pthread_self':;
 //   'scePthreadSelf':;
 //   'scePthreadEqual':;
 //   'sceKernelGettimeofday':;
 //   'sceKernelClockGettime':;
 //   'pthread_mutex_lock':;
 //   'pthread_mutex_unlock':;
 //   'sceKernelPread':;
 //   'sceKernelClose':;
 //   'sceDiscMapIsRequestOnHDD':;
 //   'Unknow':;
 //   'sceFiosIOFilterPsarcDearchiver':;
 //   'sceFiosFHReadSync':;
 //   'sceFiosFHTell':;
 //   'sceNgs2VoiceGetState':;
 //   'sceNgs2SystemRender':;
 //   'sceAudioOutOutputs':;
 //   '__tls_get_addr':;
 //   'scePthreadRwlockRdlock':;
 //   'scePthreadRwlockUnlock':;
 //   'scePthreadCondBroadcast':;
 //   'sceFiosFHCloseSync':;
 //   'sceKernelStat':;
 //   'sceFiosFHOpenSync':;
 //   'sceFiosFHGetSize':;
 //   'sceKernelOpen':;
 //   else
 //    begin
 //     Result:=TStubMemoryTrace(Stub).NewTraceStub(Info^.Nid,Info^.lib,Result,@_trace_enter,@_trace_exit);
 //    end;
 //  end;
 //end;

 end;


 if (Result=nil) then
 begin
  if (Info^.sType=STT_FUN) then
  begin
   Result:=Stub.NewNopStub(Info^.Nid,Info^.lib,@print_stub);
   //Writeln('Warn^:',Info^.lib^.strName,':',ps4libdoc.GetFunctName(Info^.Nid),':',HexStr(Info^.Nid,16));
  end else
  begin
   //PNAME = 'module_stop',
   //_MD = $11af200,
   //LIB = $114e110,
   //NID = 0,
   //OFFSET = 311584,
   //RTYPE = 7, //R_X86_64_JUMP_SLOT
   //SBIND = 2, //STB_WEAK
   //STYPE = 0} //STT_NOTYPE
   Writeln('Warn^:',Info^.lib^.strName,':',ps4libdoc.GetFunctName(Info^.Nid),':',HexStr(Info^.Nid,16));
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

 //if (lib^.strName='mono-ps4') then
 //begin
 // Writeln(Info^.pName);
 //
 // if Info^.nid=$4FCEF2B219D790C5 then
 // begin
 //  writeln;
 // end;
 //
 //end;

 if (lib^.parent<>node) then Exit;

 Result:=lib^.get_proc(Info^.Nid);

 //cache
 Info^.lib^.set_proc(Info^.nid,Result);
end;

var
 elf:Telf_file;
 //i:Integer;
 //F:THandle;

 main:pthread;

//label
// _lab;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;

 ps4_app.save_path:=IncludeTrailingPathDelimiter(GetCurrentDir)+'savedata';
 if not ParseCmd then Exit;

 //ps4_app.app_path:='..\samples\api_gnm\simplet-single-triangle';
 //ps4_app.app_file:='..\samples\api_gnm\simplet-single-triangle\simplet-single-triangle_debug.elf';

 //ps4_app.app_file:='..\samples\api_gnm\simplet-cmask\simplet-cmask_debug.elf';

 //ps4_app.app_path:='..\samples\api_gnm\simplet-simple-fs\';
 //ps4_app.app_file:='..\samples\api_gnm\simplet-simple-fs\simplet-simple-fs_debug.elf';

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
 //ps4_app.app_file:='..\samples\http_get\simple4.elf';

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

 //ps4_app.app_path:='C:\Users\User\Desktop\Games\Cladun Returns This Is Sengoku v1.00\CUSA06770\';
 //ps4_app.app_file:='C:\Users\User\Desktop\Games\Cladun Returns This Is Sengoku v1.00\CUSA06770\eboot.bin';

 //elf:=Telf_file(LoadPs4ElfFromFile('libSceLibcInternal.sprx'));
 //elf.Prepare;
 //elf.SavePs4ElfToFile('libSceLibcInternal.prx');
 //F:=FileCreate('libSceLibcInternal.txt');
 //elf.DympSymbol(F);
 //FileClose(F);
 //FreeAndNil(elf);

 ps4_app.resolve_cb:=@ResolveImport;
 ps4_app.reload_cb :=@ReloadImport;

 elf:=Telf_file(LoadPs4ElfFromFile(ps4_app.app_file));
 elf.Prepare;
 ps4_app.prog:=elf;
 ps4_app.RegistredElf(elf);
 ps4_app.ResolveDepended(elf);
 ps4_app.LoadSymbolImport(nil);


 Stub.FinStub;
 ps4_app.InitProt;

 _pthread_run_entry(@main);

 //ps4_app.InitCode;
 //elf.mapCodeEntry;

 ps4_libSceVideoOut.App_Run;

 //KillALLThreads TODO
 //readln;
end.



