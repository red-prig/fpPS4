
uses
 {$IFDEF Linux}
 cmem,
 cthreads,
 {$ENDIF}
 seh64,
 Classes,sysutils,
 stub_manager,
 ps4libdoc,
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
 ps4_types,
 ps4_elf,
 ps4_pthread,
 ps4_program,
 ps4_elf_tls;

function ParseCmd:Boolean;
var
 i,n:Integer;
label
 promo;
begin
 if (ParamCount=0) then
 begin
  promo:
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

 if not DirectoryExists(ps4_app.save_path) then
 begin
  Writeln('Path not found:',ps4_app.save_path);
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

procedure print_stub(nid:QWORD;lib:PLIBRARY);
begin
 Writeln('nop nid:',lib^.strName,':',HexStr(nid,16),':',ps4libdoc.GetFunctName(nid));
 writeln;
 //readln;
 //Print_libs(ps4_app.GetFile('libc.prx'));
end;

function ps4_sceSslInit(poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslInit:',poolSize);
 Result:=3;
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

 if (Result=nil) then
 begin
  Case Info^.lib^.strName of

   'libSceSsl':
    Case Info^.nid of
     $85DA551140C55B7B:Result:=@ps4_sceSslInit;
    end;

  end;
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

var
 elf:Telf_file;
 //i:Integer;
 //F:THandle;

 main:pthread;


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

 //ps4_app.app_file:='..\samples\api_video_out\videoout_cursor.elf';
 //ps4_app.app_file:='..\samples\api_video_out\videoout_flip.elf';

 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic2.elf';

 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic3.elf';
 //ps4_app.app_file:='..\samples\api_video_out\videoout_basic5.elf';

 //ps4_app.app_path:='G:\Games\MOMODORA\CUSA05694\';
 //ps4_app.app_file:='G:\Games\MOMODORA\CUSA05694\eboot.bin';

 //ps4_app.app_path:='G:\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\';
 //ps4_app.app_file:='G:\Games\We.Are.Doomed.PS4-PRELUDE\CUSA02394\eboot.elf';

 //Writeln(_parse_filename('/app0/data/system_ps4/flatShader_vv.sb'));

 //Writeln(_parse_filename('savedata0/11/../app.prf'));

 //elf:=Telf_file(LoadPs4ElfFromFile('libSceLibcInternal.sprx'));
 //elf.Prepare;
 //elf.SavePs4ElfToFile('libSceLibcInternal.prx');
 //F:=FileCreate('libSceLibcInternal.txt');
 //elf.DympSymbol(F);
 //FileClose(F);
 //FreeAndNil(elf);

 elf:=Telf_file(LoadPs4ElfFromFile(ps4_app.app_file));
 elf.Prepare;
 ps4_app.prog:=elf;
 ps4_app.RegistredElf(elf);
 ps4_app.ResolveDepended(elf);
 ps4_app.LoadSymbolImport(@ResolveImport,nil);


 Stub.FinStub;
 ps4_app.InitProt;
 ps4_app.InitThread;

 _pthread_run_entry(@main);

 //ps4_app.InitCode;
 //elf.mapCodeEntry;

 ps4_libSceVideoOut.App_Run;

 //KillALLThreads TODO
 //readln;
end.



