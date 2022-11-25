unit ps4_elf;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  sha1,
  sys_types,
  sys_kernel,
  ps4libdoc,
  ps4_program,
  ps4_elf_tls,
  Classes,
  SysUtils;

type
 t_user_malloc_init       =function():Integer;                              SysV_ABI_CDecl;
 t_user_malloc_finalize   =function():Integer;                              SysV_ABI_CDecl;
 t_user_malloc            =function(size:qword):Pointer;                    SysV_ABI_CDecl;
 t_user_free              =procedure(ptr:Pointer);                          SysV_ABI_CDecl;
 t_user_calloc            =function(nelem,count:qword):Pointer;             SysV_ABI_CDecl;
 t_user_realloc           =function(ptr:Pointer;size:qword):Pointer;        SysV_ABI_CDecl;
 t_user_memalign          =function(align,size:qword):Pointer;              SysV_ABI_CDecl;
 t_user_reallocalign      =function(ptr:Pointer;size,align:qword):Pointer;  SysV_ABI_CDecl;
 t_user_posix_memalign    =function(ptr:PPointer;align,size:qword):Integer; SysV_ABI_CDecl;
 t_user_malloc_stats      =function(mmsize:Pointer):Integer;                SysV_ABI_CDecl;
 t_user_malloc_stats_fast =function(mmsize:Pointer):Integer;                SysV_ABI_CDecl;
 t_user_malloc_usable_size=function(ptr:Pointer):qword;                     SysV_ABI_CDecl;

 PsceLibcMallocReplace=^TsceLibcMallocReplace;
 TsceLibcMallocReplace=packed record
  Size:QWORD;
  Unknown1:QWORD; //00000001
  user_malloc_init       :t_user_malloc_init;
  user_malloc_finalize   :t_user_malloc_finalize;
  user_malloc            :t_user_malloc;
  user_free              :t_user_free;
  user_calloc            :t_user_calloc;
  user_realloc           :t_user_realloc;
  user_memalign          :t_user_memalign;
  user_reallocalign      :t_user_reallocalign;
  user_posix_memalign    :t_user_posix_memalign;
  user_malloc_stats      :t_user_malloc_stats;
  user_malloc_stats_fast :t_user_malloc_stats_fast;
  user_malloc_usable_size:t_user_malloc_usable_size;
 end;

 t_user_new_1         =function(size:qword):Pointer;                    SysV_ABI_CDecl;
 t_user_new_2         =function(size:qword;throw:Pointer):Pointer;      SysV_ABI_CDecl;
 t_user_new_array_1   =function(size:qword):Pointer;                    SysV_ABI_CDecl;
 t_user_new_array_2   =function(size:qword;throw:Pointer):Pointer;      SysV_ABI_CDecl;
 t_user_delete_1      =procedure(ptr:Pointer);                          SysV_ABI_CDecl;
 t_user_delete_2      =procedure(ptr:Pointer;throw:Pointer);            SysV_ABI_CDecl;
 t_user_delete_array_1=procedure(ptr:Pointer);                          SysV_ABI_CDecl;
 t_user_delete_array_2=procedure(ptr:Pointer;throw:Pointer);            SysV_ABI_CDecl;
 t_user_delete_3      =procedure(ptr:Pointer;size:qword);               SysV_ABI_CDecl;
 t_user_delete_4      =procedure(ptr:Pointer;size:qword;throw:Pointer); SysV_ABI_CDecl;
 t_user_delete_array_3=procedure(ptr:Pointer;size:qword);               SysV_ABI_CDecl;
 t_user_delete_array_4=procedure(ptr:Pointer;size:qword;throw:Pointer); SysV_ABI_CDecl;

 PsceLibcNewReplace=^TsceLibcNewReplace;
 TsceLibcNewReplace=packed record
  Size:QWORD;
  Unknown1:QWORD; //00000002
  user_new_1         :t_user_new_1;
  user_new_2         :t_user_new_2;
  user_new_array_1   :t_user_new_array_1;
  user_new_array_2   :t_user_new_array_2;
  user_delete_1      :t_user_delete_1;
  user_delete_2      :t_user_delete_2;
  user_delete_array_1:t_user_delete_array_1;
  user_delete_array_2:t_user_delete_array_2;
  user_delete_3      :t_user_delete_3;
  user_delete_4      :t_user_delete_4;
  user_delete_array_3:t_user_delete_array_3;
  user_delete_array_4:t_user_delete_array_4;
 end;

 t_user_malloc_init_for_tls   =function():Integer;                              SysV_ABI_CDecl;
 t_user_malloc_fini_for_tls   =function():Integer;                              SysV_ABI_CDecl;
 t_user_malloc_for_tls        =function(size:qword):Pointer;                    SysV_ABI_CDecl;
 t_user_posix_memalign_for_tls=function(ptr:PPointer;align,size:qword):Integer; SysV_ABI_CDecl;
 t_user_free_for_tls          =procedure(ptr:Pointer);                          SysV_ABI_CDecl;

 PsceLibcMallocReplaceForTls=^TsceLibcMallocReplaceForTls;
 TsceLibcMallocReplaceForTls=packed record
  Size:QWORD;
  Unknown1:QWORD; //00000001
  user_malloc_init_for_tls   :t_user_malloc_init_for_tls;
  user_malloc_fini_for_tls   :t_user_malloc_fini_for_tls;
  user_malloc_for_tls        :t_user_malloc_for_tls;
  user_posix_memalign_for_tls:t_user_posix_memalign_for_tls;
  user_free_for_tls          :t_user_free_for_tls;
 end;

 PSceLibcParam=^TSceLibcParam;
 TSceLibcParam=packed record
  Size:QWORD;
  entry_count:DWORD;
  SceLibcInternalHeap              :DWORD; //1 //(entry_count > 1)
  sceLibcHeapSize                  :PDWORD;
  sceLibcHeapDelayedAlloc          :PDWORD;
  sceLibcHeapExtendedAlloc         :PDWORD;
  sceLibcHeapInitialSize           :PDWORD;
  _sceLibcMallocReplace            :PsceLibcMallocReplace;
  _sceLibcNewReplace               :PsceLibcNewReplace;
  sceLibcHeapHighAddressAlloc      :PQWORD;    //(entry_count > 2)
  Need_sceLibc                     :PDWORD;
  sceLibcHeapMemoryLock            :PQWORD;    //(entry_count > 4)
  sceKernelInternalMemorySize      :PQWORD;
  _sceLibcMallocReplaceForTls      :PsceLibcMallocReplaceForTls;
  //The maximum amount of the memory areas for the mspace. min:0x1000000
  sceLibcMaxSystemSize             :PQWORD;    //(entry_count > 7)
  sceLibcHeapDebugFlags            :PQWORD;    //(entry_count > 8)
  sceLibcStdThreadStackSize        :PDWORD;
  Unknown3:QWORD;
  sceKernelInternalMemoryDebugFlags:PDWORD;
  sceLibcWorkerThreadNum           :PDWORD;
  sceLibcWorkerThreadPriority      :PDWORD;
  sceLibcThreadUnnamedObjects      :PDWORD;
 end;

 PSceKernelMemParam=^TSceKernelMemParam;
 TSceKernelMemParam=packed record
  Size:QWORD;
  sceKernelExtendedPageTable   :PQWORD;
  sceKernelFlexibleMemorySize  :PQWORD;
  sceKernelExtendedMemory1     :PQWORD;
  sceKernelExtendedGpuPageTable:PQWORD;
  sceKernelExtendedMemory2     :PQWORD;
  sceKernelExtendedCpuPageTable:PQWORD;
 end;

 PSceKernelFsParam=^TSceKernelFsParam;
 TSceKernelFsParam=packed record
  Size:QWORD;
  sceKernelFsDupDent:PQWORD;
 end;

 PSceProcParam=^TSceProcParam;
 TSceProcParam=packed record
  Header:packed record
   Size:QWORD;        //0x50
   Magic:DWORD;       //"ORBI" //0x4942524F
   Entry_count:DWORD; //>=1
   SDK_version:QWORD; //0x4508101
  end;
  sceProcessName            :PChar;
  sceUserMainThreadName     :PChar;
  sceUserMainThreadPriority :PDWORD;
  sceUserMainThreadStackSize:PDWORD;
  _sceLibcParam             :PSceLibcParam;
  _sceKernelMemParam        :PSceKernelMemParam;
  _sceKernelFsParam         :PSceKernelFsParam;
  sceProcessPreloadEnabled  :PDWORD;
 end;

 Telf_file=class;

 PRelaInfo=^TRelaInfo;
 TRelaInfo=packed record
  pName:PChar;
  value:QWORD;
  Offset:QWORD;
  Addend:QWORD;
  rType:DWORD;
  sBind:Byte;
  sType:Byte;
  shndx:Word;
 end;
 TOnRelaInfoCb=Procedure(elf:Telf_file;Info:PRelaInfo;data:Pointer);

 PResolveImportInfo=^TResolveImportInfo;
 TResolveImportInfo=packed record
  pName:PChar;
  _md:PMODULE;
  lib:PLIBRARY;
  nid:QWORD;
  Offset:QWORD;
  rType:DWORD;
  sBind:Byte;
  sType:Byte;
 end;
 TResolveImportCb=function(elf:Telf_file;Info:PResolveImportInfo;data:Pointer):Pointer;

 Telf_file=class(TElf_node)

  pSceFileName:RawByteString;

  mElf:TMemChunk;
  mMap:TMemChunk;

  ModuleInfo:SceKernelModuleInfo;

  ro_segments:array[0..3] of TMemChunk;

  pEntry:QWORD;
  dtInit:QWORD;
  dtFini:QWORD;

  pProcParam     :QWORD;
  pModuleParam   :QWORD;

  dtflags:QWORD;

  pTls:packed record
   tmpl_start:QWORD;
   tmpl_size :QWORD;
   full_size :QWORD;
   align     :QWORD;
   index     :QWORD;
   offset    :Int64;

   stub:TMemChunk;
  end;

  pInit:packed record
   dt_preinit_array,
   dt_preinit_array_count:QWORD;
   dt_init_array,
   dt_init_array_count:QWORD;
  end;

  eh_frame_hdr:packed record
   addr:QWORD;
   size:QWORD;
  end;

  eh_frame:packed record
   addr:QWORD;
   size:QWORD;
  end;

  pSceDynLib:TMemChunk;       //mElf

  pDynEnt    :PElf64_Dyn;     //mElf
  nDynEntCount:DWord;         //mElf

  pSymTab:PElf64_Sym;       //pSceDynLib
  nSymTabCount:Int64;       //pSceDynLib

  pStrTable:PAnsiChar;      //pSceDynLib
  nStrTabSize:Int64;        //pSceDynLib

  pRelaEntries:Pelf64_rela; //pSceDynLib
  nRelaCount:Int64;         //pSceDynLib

  pPltRela:PElf64_Rela;     //pSceDynLib
  nPltRelaCount:Int64;      //pSceDynLib

  nPltRelType:Int64;

  protected
   function  PreparePhdr:Boolean;
   procedure PrepareTables(var entry:Elf64_Dyn);
   procedure ParseSingleDynEntry(var entry:Elf64_Dyn);
   function  PrepareDynTables:Boolean;
   procedure _calculateTotalLoadableSize(elf_phdr:Pelf64_phdr;s:SizeInt);
   procedure _mapSegment(phdr:PElf64_Phdr;const name:PChar);
   procedure _addSegment(phdr:PElf64_Phdr;const name:PChar);
   function  _ro_seg_len:Integer;
   function  _ro_seg_find_lo(adr:Pointer):Integer;
   function  _ro_seg_find_hi(adr:Pointer):Integer;
   procedure _add_ro_seg(phdr:PElf64_Phdr);
   procedure _print_rdol;
   function  _ro_seg_adr_in(adr:Pointer;size:QWORD):Boolean;
   procedure _find_tls_stub(elf_phdr:Pelf64_phdr;s:SizeInt);
   function  MapImageIntoMemory:Boolean;
   function  RelocateRelaEnum(cbs:TOnRelaInfoCb;data:Pointer):Boolean;
   function  RelocatePltRelaEnum(cbs:TOnRelaInfoCb;data:Pointer):Boolean;
   function  ParseSymbolsEnum(cbs:TOnRelaInfoCb;data:Pointer):Boolean;
   function  LoadSymbolExport:Boolean;
   procedure _PatchTls(Proc:Pointer;Addr:PByte;Size:QWORD);
   procedure PatchTls(Proc:Pointer);
   procedure mapProt;
   procedure mapCodeInit;
   Procedure ClearElfFile;
   function  _init_tls(is_static:QWORD):Ptls_tcb;
   function  _get_tls:Ptls_tcb;
  public
   Procedure Clean; override;
   function  SavePs4ElfToFile(Const name:RawByteString):Boolean;
   function  Prepare:Boolean; override;
   Procedure LoadSymbolImport(cbs,data:Pointer); override;
   Procedure ReLoadSymbolImport(cbs,data:Pointer); override;
   function  DympSymbol(F:THandle):Boolean;
   procedure InitThread(is_static:QWORD); override;
   Procedure InitProt;   override;
   Procedure InitCode;   override;
   function  module_start(argc:size_t;argp,param:PPointer):Integer; override;
   function  GetCodeFrame:TMemChunk;  override;
   function  GetEntry:Pointer;  override;
   function  GetdInit:Pointer;  override;
   function  GetdFini:Pointer;  override;
   Function  GetModuleInfo:SceKernelModuleInfo; override;
   Function  GetModuleInfoEx:SceKernelModuleInfoEx; override;
   procedure mapCodeEntry;
 end;

type
 TinitProc   =function(argc:Integer;argv,environ:PPchar):Integer; SysV_ABI_CDecl; //preinit_array/init_array
 TEntryPoint =procedure(pEnv:Pointer;pfnExitHandler:Pointer);     SysV_ABI_CDecl; //EntryPoint
 TmoduleStart=function(argc:size_t;argp,proc:Pointer):Integer;    SysV_ABI_CDecl; //module_start/module_stop

function  LoadPs4ElfFromFile(Const name:RawByteString):TElf_node;

function  ps4_nid_hash(const name:RawByteString):QWORD;

function  EncodeValue64(nVal:QWORD):RawByteString;
function  DecodeValue64(strEnc:PAnsiChar;len:SizeUint;var nVal:QWORD):Boolean;

function  DecodeEncName(strEncName:PAnsiChar;var nModuleId,nLibraryId:WORD;var nNid:QWORD):Boolean;

function  _dynamic_tls_get_addr(ti:PTLS_index):Pointer; SysV_ABI_CDecl;

implementation

type
 Ppatch_ld=^Tpatch_ld;
 Tpatch_ld=packed record
  //_movabs_rax:array[0..1] of Byte; // $48 $B8   //2
  _addr:Pointer;                                //8
  //_jmp_rax:array[0..1] of Byte;    // $FF $E0   //2  = 14
 end;

 Ppatch_fs=^Tpatch_fs;
 Tpatch_fs=packed record
  _call_rip:array[0..1] of Byte; //$ff $15
  //_push_rdx:Byte; // $52  //1
  //_push_rcx:Byte; // $51  //1
  //_call_32:Byte;  // $E8  //1
  _ofs:Integer;           //4
  //_pop_rcx:Byte;  // $59  //1
  //_pop_rdx:Byte;  // $5a  //1 = 9
  _nop:array[0..2] of Byte; //$90 $90 $90
 end;

//ff 15 [d3 ff ff ff]

Const
 _patch_ld:Tpatch_ld=(
  //_movabs_rax:($48,$B8);
  _addr:nil;
  //_jmp_rax:($FF,$E0);
 );

 _patch_fs:Tpatch_fs=(
  _call_rip:($ff,$15);
  //_push_rcx:$51;
  //_push_rdx:$52;
  //_call_32:$E8;
  _ofs:0;
  //_pop_rdx:$5a;
  //_pop_rcx:$59;
  _nop:($90,$90,$90);
 );

Procedure Telf_file.ClearElfFile;
begin
 if (Self=nil) then Exit;
 FreeMem(mElf.pAddr);
 mElf:=Default(TMemChunk);
 pSceDynLib:=Default(TMemChunk);
 pDynEnt      :=nil;
 nDynEntCount :=0;
 pSymTab      :=nil;
 nSymTabCount :=0;
 pStrTable    :=nil;
 nStrTabSize  :=0;
 pRelaEntries :=nil;
 nRelaCount   :=0;
 pPltRela     :=nil;
 nPltRelaCount:=0;
end;

Procedure Telf_file.Clean;
begin
 if (Self=nil) then Exit;
 ClearElfFile;
 FreeMem(mElf.pAddr);
 if (mMap.pAddr<>nil) then
 begin
  VirtualFree(mMap.pAddr,0,MEM_RELEASE);
 end;
 pSceFileName:='';
 inherited;
end;

function test_SF_flags(flags:QWORD):RawByteString;
begin
 Result:='';
 if (flags and SF_ORDR)<>0 then Result:=Result+' SF_ORDR';
 if (flags and SF_ENCR)<>0 then Result:=Result+' SF_ENCR';
 if (flags and SF_SIGN)<>0 then Result:=Result+' SF_SIGN';
 if (flags and SF_DFLG)<>0 then Result:=Result+' SF_DFLG';
 if (flags and SF_BFLG)<>0 then Result:=Result+' SF_BFLG';
end;

function LoadPs4ElfFromFile(Const name:RawByteString):TElf_node;
Var
 elf:Telf_file;

 F:THandle;
 i:SizeInt;
 self_header:Tself_header;
 Segments:Pself_segment;

 SELF_MEM:Pointer;
 SELF_SIZE:Int64;

 elf_hdr:Pelf64_hdr;
 elf_phdr:Pelf64_phdr;

 LowSeg,s:Int64;

 Num_Segments:Word;

 function _test_self(self_header:Pself_header):Boolean;
 begin
  Result:=False;
  Writeln('SELF.Content_Type=',HexStr(self_header^.Content_Type,2));
  Writeln('SELF.Program_Type=',HexStr(self_header^.Program_Type,2));
  Writeln('SELF.Header_Size =',HexStr(self_header^.Header_Size,4));
  Writeln('SELF.Sign_Size   =',HexStr(self_header^.Sign_Size,4));
  Writeln('SELF.Size_of     =',HexStr(self_header^.Size_of,8));
  Writeln('SELF.Num_Segments=',self_header^.Num_Segments);
  if (self_header^.Num_Segments=0) then Exit;
  Result:=True;
 end;

 function _test_elf(elf_hdr:Pelf64_hdr):Boolean;
 begin
  Result:=False;

  if PDWORD(@elf_hdr^.e_ident)^<>ELFMAG then
  begin
   Writeln(StdErr,name,' ELF identifier mismatch:',HexStr(PDWORD(@elf_hdr^.e_ident)^,8));
   Exit;
  end;

  Case elf_hdr^.e_type of
   ET_SCE_EXEC       :;
   ET_SCE_REPLAY_EXEC:;
   ET_SCE_RELEXEC    :;
   ET_SCE_DYNEXEC    :;
   ET_SCE_DYNAMIC    :;
   else
    begin
     Writeln(StdErr,name,' unspported TYPE:',HexStr(elf_hdr^.e_type,4));
     Exit;
    end;
  end;

  if (elf_hdr^.e_machine<>EM_X86_64) then
  begin
   Writeln(StdErr,name,' unspported ARCH:',elf_hdr^.e_machine);
   Exit;
  end;

  Writeln('hdr.[EI_CLASS]  :',elf_hdr^.e_ident[EI_CLASS]);
  Writeln('hdr.[EI_DATA]   :',elf_hdr^.e_ident[EI_DATA]);
  Writeln('hdr.[EI_VERSION]:',elf_hdr^.e_ident[EI_VERSION]);
  Writeln('hdr.[EI_OSABI]  :',elf_hdr^.e_ident[EI_OSABI]);
  Writeln('hdr.e_type      :',HexStr(elf_hdr^.e_type,4));
  Writeln('hdr.e_machine   :',HexStr(elf_hdr^.e_machine,4));
  Writeln('hdr.e_version   :',HexStr(elf_hdr^.e_version,4));
  Writeln('hdr.e_entry     :',HexStr(elf_hdr^.e_entry,16));
  Writeln('hdr.e_phoff     :',HexStr(elf_hdr^.e_phoff,16));
  Writeln('hdr.e_shoff     :',HexStr(elf_hdr^.e_shoff,16));
  Writeln('hdr.e_flags     :',HexStr(elf_hdr^.e_flags,8));
  Writeln('hdr.e_ehsize    :',HexStr(elf_hdr^.e_ehsize,4));
  Writeln('hdr.e_phentsize :',HexStr(elf_hdr^.e_phentsize,4));
  Writeln('hdr.e_phnum     :',elf_hdr^.e_phnum);
  Writeln('hdr.e_shentsize :',HexStr(elf_hdr^.e_shentsize,4));
  Writeln('hdr.e_shnum     :',elf_hdr^.e_shnum    );
  Writeln('hdr.e_shstrndx  :',HexStr(elf_hdr^.e_shstrndx,4));
  if (elf_hdr^.e_phnum=0) then Exit;
  Result:=True;
 end;

begin
 Result:=nil;
 if (name='') then Exit;
 F:=FileOpen(name,fmOpenRead);
 if (F=feInvalidHandle) then Exit;
 FileRead(F,self_header.Magic,SizeOf(DWORD));
 case self_header.Magic of
  ELFMAG:
   begin
    SELF_SIZE:=FileSeek(F,0,fsFromEnd);
    if (SELF_SIZE<=0) then
    begin
     Writeln(StdErr,'Error read file:',name);
     FileClose(F);
     Exit;
    end;
    SELF_MEM:=GetMem(SELF_SIZE);
    FileSeek(F,0,fsFromBeginning);
    s:=FileRead(F,SELF_MEM^,SELF_SIZE);
    FileClose(F);

    if (s<>SELF_SIZE) then
    begin
     Writeln(StdErr,'Error read file:',name);
     FreeMem(SELF_MEM);
     Exit;
    end;

    if not _test_elf(SELF_MEM) then
    begin
     Writeln(StdErr,'Error test file:',name);
     FreeMem(SELF_MEM);
     Exit;
    end;

    elf:=Telf_file.Create;
    elf.mElf.nSize:=SELF_SIZE;
    elf.mElf.pAddr:=SELF_MEM;
    elf._set_filename(name);
    Result:=elf;
   end;
  self_magic:
   begin
    SELF_SIZE:=FileSeek(F,0,fsFromEnd);
    if (SELF_SIZE<=0) then
    begin
     Writeln(StdErr,'Error read file:',name);
     FileClose(F);
     Exit;
    end;
    SELF_MEM:=GetMem(SELF_SIZE);
    FileSeek(F,0,fsFromBeginning);
    s:=FileRead(F,SELF_MEM^,SELF_SIZE);
    FileClose(F);

    if (s<>SELF_SIZE) then
    begin
     Writeln(StdErr,'Error read file:',name);
     FreeMem(SELF_MEM);
     Exit;
    end;

    if not _test_self(SELF_MEM) then
    begin
     FreeMem(SELF_MEM);
     Exit;
    end;

    Num_Segments:=Pself_header(SELF_MEM)^.Num_Segments;
    Segments:=SELF_MEM+SizeOf(Tself_header);
    For i:=0 to Num_Segments-1 do
    begin
     if (Segments[i].flags and (SF_ENCR or SF_DFLG))<>0 then
     begin
      Writeln(StdErr,'[',i,']');
      Writeln(StdErr,' Seg.flags =',test_SF_flags(Segments[i].flags));
      Writeln(StdErr,' Seg.id    =',Segments[i].flags shr 20);
      Writeln(StdErr,' Seg.offset=',HexStr(Segments[i].offset,16));
      Writeln(StdErr,' Seg.c_size=',HexStr(Segments[i].encrypted_compressed_size,16));
      Writeln(StdErr,' Seg.d_size=',HexStr(Segments[i].decrypted_decompressed_size,16));
      Writeln(StdErr,'encrypted or deflated SELF not support!');
      FreeMem(SELF_MEM);
      Exit;
     end;
    end;
    elf_hdr:=Pointer(Segments)+(Num_Segments*SizeOf(Tself_segment));

    if not _test_elf(elf_hdr) then
    begin
     Writeln(StdErr,'Error test file:',name);
     FreeMem(SELF_MEM);
     Exit;
    end;

    elf_phdr:=Pointer(elf_hdr)+SizeOf(elf64_hdr);
    LowSeg:=High(Int64);
    elf:=Telf_file.Create;
    elf.mElf.nSize:=0;
    For i:=0 to elf_hdr^.e_phnum-1 do
    begin
     s:=elf_phdr[i].p_offset;
     if (s<>0) then
      if (s<LowSeg) then LowSeg:=s;

     s:=s+elf_phdr[i].p_filesz;
     if s>elf.mElf.nSize then elf.mElf.nSize:=s;
    end;
    if (LowSeg=High(Int64)) then
    begin
     Writeln(StdErr,'Error LowSeg');
     FreeMem(SELF_MEM);
     elf.Free;
     Exit;
    end;
    elf.mElf.pAddr:=AllocMem(elf.mElf.nSize);
    Writeln('Elf with LowSeg:',LowSeg,' Size:',elf.mElf.nSize);
    Move(elf_hdr^,elf.mElf.pAddr^,LowSeg);
    For i:=0 to Num_Segments-1 do
     if ((Segments[i].flags and SF_BFLG)<>0) then
     begin
      s:=(Segments[i].flags shr 20) and $FFF;
      Move(Pointer(SELF_MEM+Segments[i].offset)^,
           Pointer(elf.mElf.pAddr+elf_phdr[s].p_offset)^,
           Segments[i].encrypted_compressed_size);
     end;
    FreeMem(SELF_MEM);
    elf._set_filename(name);
    Result:=elf;
   end;
  else
   begin
    FileClose(F);
    Writeln(StdErr,name,' is unknow file type:',HexStr(self_header.Magic,8));
   end;
 end;
end;

function Telf_file.SavePs4ElfToFile(Const name:RawByteString):Boolean;
Var
 F:THandle;
begin
 Result:=False;
 if (Self=nil) then Exit;
 if (name='') then Exit;
 if (mElf.pAddr=nil) or (mElf.nSize=0) then Exit;
 F:=FileCreate(name);
 if (F=feInvalidHandle) then Exit;
 FileWrite(F,mElf.pAddr^,mElf.nSize);
 FileClose(F);
end;

function GetStr(p:Pointer;L:SizeUint):RawByteString;
begin
 SetString(Result,P,L);
end;

function test_PF_flags(p_flags:Elf64_Word):RawByteString;
begin
 Result:='';
 if (p_flags and PF_X)<>0 then Result:=Result+' PF_X';
 if (p_flags and PF_W)<>0 then Result:=Result+' PF_W';
 if (p_flags and PF_R)<>0 then Result:=Result+' PF_R';
end;

function read_encoded_value(hdr:p_eh_frame_hdr;hdr_size,hdr_vaddr:Int64;enc:Byte;var P:Pointer;var res:Int64):Boolean;
var
 value:Int64;
 endp:Pointer;
begin
 Result:=False;

 res:=0;

 Case (enc and $70) of
  DW_EH_PE_absptr:;
  DW_EH_PE_pcrel:
   begin
    res:=(hdr_vaddr + (P - Pointer(hdr)));
   end;
  DW_EH_PE_datarel:
   begin
    res:=hdr_vaddr;
   end;
  else
   Exit(True);
 end;

 value:=0;
 endp:=Pointer(hdr) + hdr_size;

 Case (enc and $0f) of
  DW_EH_PE_udata2:
   begin
    if (p + 2 > endp) then Exit(True);
    value:=PWORD(p)^;
    p:=p + 2;
   end;
  DW_EH_PE_sdata2:
   begin
    if (p + 2 > endp) then Exit(True);
    value:=PSmallint(p)^;
    p:=p + 2;
   end;
  DW_EH_PE_udata4:
   begin
    if (p + 4 > endp) then Exit(True);
    value:=PDWORD(p)^;
    p:=p + 4;
   end;
  DW_EH_PE_sdata4:
   begin
    if (p + 4 > endp) then Exit(True);
    value:=PInteger(p)^;
    p:=p + 4;
   end;
  DW_EH_PE_udata8,
  DW_EH_PE_sdata8:
   begin
    if (p + 8 > endp) then Exit(True);
    value:=PInteger(p)^;
    p:=p + 8;
   end;
  DW_EH_PE_absptr:
   begin
    value:=0;
   end;
  else
   Exit(True);
 end;

 res:=res+value;

 Result:=False;
end;

function parse_eh_frame_hdr(hdr:p_eh_frame_hdr;hdr_size,hdr_vaddr:Int64;var eh_frame_ptr:Int64):Boolean;
var
 p:Pointer;
begin
 if (hdr^.eh_frame_ptr_enc=DW_EH_PE_omit) then Exit(False);

 p:=@hdr^.encoded;

 if read_encoded_value(hdr,hdr_size,
                   hdr_vaddr,
                   hdr^.eh_frame_ptr_enc,
                   p,
                   eh_frame_ptr) then Exit(False);

 Result:=True;
end;

function Telf_file.PreparePhdr:Boolean;
Var
 i:SizeInt;
 elf_hdr:Pelf64_hdr;
 elf_phdr:Pelf64_phdr;
 P:Pointer;
begin
 Result:=False;

 ModuleInfo:=Default(SceKernelModuleInfo);
 ModuleInfo.size:=SizeOf(SceKernelModuleInfo);

 elf_hdr:=mElf.pAddr;
 if (elf_hdr=nil) then Exit;
 elf_phdr:=Pointer(elf_hdr)+SizeOf(elf64_hdr);

 if (elf_hdr^.e_phnum=0) then Exit;

 For i:=0 to elf_hdr^.e_phnum-1 do
 begin

  {
  Writeln('[',i,']');
  case elf_phdr[i].p_type of
   PT_NULL   :Writeln(' phdr.PT_NULL');
   PT_LOAD   :Writeln(' phdr.PT_LOAD ');
   PT_DYNAMIC:Writeln(' phdr.PT_DYNAMIC');
   PT_INTERP :Writeln(' phdr.PT_INTERP');
   PT_NOTE   :Writeln(' phdr.PT_NOTE');
   PT_SHLIB  :Writeln(' phdr.PT_SHLIB');
   PT_PHDR   :Writeln(' phdr.PT_PHDR');
   PT_TLS    :Writeln(' phdr.PT_TLS');

   PT_GNU_EH_FRAME :Writeln(' phdr.PT_GNU_EH_FRAME');
   PT_GNU_STACK    :Writeln(' phdr.PT_GNU_STACK');

   PT_SCE_RELA        :Writeln(' phdr.PT_SCE_RELA');
   PT_SCE_DYNLIBDATA  :Writeln(' phdr.PT_SCE_DYNLIBDATA');

   PT_SCE_RELRO       :Writeln(' phdr.PT_SCE_RELRO');
   PT_SCE_COMMENT     :Writeln(' phdr.PT_SCE_COMMENT');
   PT_SCE_VERSION     :Writeln(' phdr.PT_SCE_VERSION');

   PT_SCE_PROCPARAM   :Writeln(' phdr.PT_SCE_PROCPARAM');
   PT_SCE_MODULE_PARAM:Writeln(' phdr.PT_SCE_MODULE_PARAM');

   else
    Writeln(' phdr.',HexStr(elf_phdr[i].p_type,16));
  end;

  Writeln(' phdr.p_flags =',test_PF_flags(elf_phdr[i].p_flags));
  Writeln(' phdr.p_offset=',HexStr(elf_phdr[i].p_offset,16));
  Writeln(' phdr.p_memsz =',HexStr(elf_phdr[i].p_memsz,16));
  Writeln(' phdr.p_filesz=',HexStr(elf_phdr[i].p_filesz,16));
  Writeln(' phdr.p_paddr =',HexStr(elf_phdr[i].p_paddr,16));
  Writeln(' phdr.p_vaddr =',HexStr(elf_phdr[i].p_vaddr,16));
  Writeln(' phdr.p_align =',HexStr(elf_phdr[i].p_align,16));
  }

  case elf_phdr[i].p_type of

   PT_NOTE:; //skip

   PT_LOAD:;//load
   PT_SCE_RELRO:;//load

   PT_SCE_PROCPARAM:
   begin
    pProcParam:=elf_phdr[i].p_vaddr;
   end;

   PT_SCE_MODULE_PARAM:
    begin
     pModuleParam:=elf_phdr[i].p_vaddr;
    end;

   PT_INTERP:
    begin
     P:=mElf.pAddr+elf_phdr[i].p_offset;
     Writeln(GetStr(P,elf_phdr[i].p_filesz));
    end;

   PT_DYNAMIC:
     begin
      pDynEnt:=mElf.pAddr+elf_phdr[i].p_offset;
      nDynEntCount:=elf_phdr[i].p_filesz div sizeof(Elf64_Dyn);
     end;
    PT_SCE_DYNLIBDATA:
     begin
      pSceDynLib.pAddr:=mElf.pAddr+elf_phdr[i].p_offset;
      pSceDynLib.nSize:=elf_phdr[i].p_filesz;
     end;
    PT_TLS:
     begin
      pTls.tmpl_start:=elf_phdr[i].p_vaddr;
      pTls.tmpl_size :=elf_phdr[i].p_filesz;
      pTls.full_size :=elf_phdr[i].p_memsz;
      pTls.align     :=elf_phdr[i].p_align;
      pTls.index     :=PtrUint(Self);// module id
      pTls.offset    :=-pTls.full_size;  //offset in static

      //Assert(IsAlign(elf.pTls.tmpl_start,elf.pTls.align));
      Writeln('tmpl_size:',pTls.tmpl_size,' full_size:',pTls.full_size);

     end;
    PT_SCE_COMMENT:
     begin

     end;
    PT_SCE_VERSION:
     begin

     end;
    PT_GNU_EH_FRAME:
     begin
      eh_frame_hdr.addr:=elf_phdr[i].p_vaddr;
      eh_frame_hdr.size:=elf_phdr[i].p_filesz;
     end;

    else
     Writeln('PHDR:',HexStr(elf_phdr[i].p_type,16));

  end;

 end;

 Result:=True;
end;

function _lib_attr_text(id:DWord):RawByteString;
begin
 Case id of
  $01:Result:='AUTO_EXPORT';
  $02:Result:='WEAK_EXPORT';
  $08:Result:='LOOSE_IMPORT';
  $09:Result:='AUTO_EXPORT|LOOSE_IMPORT';
  $10:Result:='WEAK_EXPORT|LOOSE_IMPORT';
  else
   Result:=HexStr(id,8);
 end;
end;

function _mod_attr_text(id:DWord):RawByteString;
begin
 Case id of
  $00:Result:='NONE';
  $01:Result:='SCE_CANT_STOP';
  $02:Result:='SCE_EXCLUSIVE_LOAD';
  $04:Result:='SCE_EXCLUSIVE_START';
  $08:Result:='SCE_CAN_RESTART';
  $10:Result:='SCE_CAN_RELOCATE';
  $20:Result:='SCE_CANT_SHARE';
  else
   Result:=HexStr(id,8);
 end;
end;

procedure Telf_file.PrepareTables(var entry:Elf64_Dyn);
var
 i:Byte;
begin
 case entry.d_tag of

  DT_PREINIT_ARRAY:
   begin
    pInit.dt_preinit_array:=entry.d_un.d_ptr;
    Writeln('DT_PREINIT_ARRAY:',HexStr(entry.d_un.d_ptr,16));
   end;
  DT_PREINIT_ARRAYSZ:
   begin
    pInit.dt_preinit_array_count:=entry.d_un.d_ptr div SizeOf(Pointer);
    Writeln('DT_PREINIT_ARRAY_count:',pInit.dt_preinit_array_count);
   end;

  DT_INIT:
   begin
    dtInit:=entry.d_un.d_ptr;
    Writeln('INIT addr:',entry.d_un.d_ptr);
   end;
  DT_INIT_ARRAY:
   begin
    pInit.dt_init_array:=entry.d_un.d_ptr;
    Writeln('DT_INIT_ARRAY:',HexStr(entry.d_un.d_ptr,16));
   end;
  DT_INIT_ARRAYSZ:
   begin
    pInit.dt_init_array_count:=entry.d_un.d_ptr div SizeOf(Pointer);
    Writeln('DT_INIT_ARRAY_count:',pInit.dt_init_array_count);
   end;

  DT_FINI:
   begin
    dtFini:=entry.d_un.d_ptr;
    Writeln('FINI addr:',HexStr(entry.d_un.d_ptr,16));
   end;
  DT_SCE_SYMTAB:
   begin
    pSymTab:=pSceDynLib.pAddr+entry.d_un.d_ptr;
    Writeln('DT_SCE_SYMTAB:',entry.d_un.d_ptr);
   end;
  DT_SCE_SYMTABSZ:  //symbol table
   begin
    nSymTabCount:=entry.d_un.d_val div SizeOf(Elf64_Sym);
    Writeln('DT_SCE_SYMTABSZ:',entry.d_un.d_val);
   end;
  DT_SCE_STRTAB: //string table
   begin
    pStrTable:=pSceDynLib.pAddr+entry.d_un.d_ptr;
    Writeln('DT_SCE_STRTAB:',entry.d_un.d_ptr);
   end;
  DT_SCE_STRSZ:
   begin
    nStrTabSize:=entry.d_un.d_ptr;
    Writeln('DT_SCE_STRSZ:',entry.d_un.d_val);
   end;
  DT_SCE_RELA: //Relocation table
   begin
    pRelaEntries:=pSceDynLib.pAddr+entry.d_un.d_ptr;
    Writeln('DT_SCE_RELA:',entry.d_un.d_ptr);
   end;
  DT_SCE_RELASZ:
   begin
    nRelaCount:=entry.d_un.d_val div sizeof(Elf64_Rela);
    Writeln('DT_SCE_STRSZ:',nRelaCount);
   end;
  DT_SCE_JMPREL: //PLT relocation table
   begin
    pPltRela:=pSceDynLib.pAddr+entry.d_un.d_ptr;
    Writeln('DT_SCE_JMPREL:',entry.d_un.d_ptr);
   end;
  DT_SCE_PLTREL:
   begin
    nPltRelType:=entry.d_un.d_val;
    Writeln('DT_SCE_PLTREL:',entry.d_un.d_val);
   end;
  DT_SCE_PLTRELSZ:
   begin
    nPltRelaCount:=entry.d_un.d_val div sizeof(Elf64_Rela);
    Writeln('DT_SCE_PLTRELSZ:',nPltRelaCount);
   end;

  DT_NEEDED:;
  DT_SCE_MODULE_INFO:;
  DT_SCE_NEEDED_MODULE:;
  DT_SCE_EXPORT_LIB:;
  DT_SCE_IMPORT_LIB:;

  //Offset of the hash table. Similar to ELF hash tables, just has a custom tag.
  DT_SCE_HASH  :Writeln('HASH:',entry.d_un.d_val);
  DT_SCE_HASHSZ:Writeln('HASH_SIZE:',entry.d_un.d_val);

  //Offset of the global offset table.
  DT_SCE_PLTGOT           :Writeln('DT_SCE_PLTGOT           :',HexStr(entry.d_un.d_val,16));
  DT_NULL                 :Writeln('DT_NULL');
  DT_DEBUG                :Writeln('DT_DEBUG                :',entry.d_un.d_val);
  DT_TEXTREL              :Writeln('DT_TEXTREL              :',entry.d_un.d_val);

  // Pointer to array of termination functions
  DT_FINI_ARRAY           :Writeln('DT_FINI_ARRAY           :',entry.d_un.d_val);
  DT_FINI_ARRAYSZ         :Writeln('DT_FINI_ARRAYSZ         :',entry.d_un.d_val);

  //Should be set to `DF_TEXTREL`.
  DT_FLAGS:
  begin
   dtflags:=entry.d_un.d_val;
   Writeln('DT_FLAGS:',entry.d_un.d_val);
   if (dtflags and DF_STATIC_TLS)<>0 then
   begin
    Writeln('DF_STATIC_TLS');
   end;
  end;

  DT_SCE_FINGERPRINT:
   begin
    Move((pSceDynLib.pAddr+entry.d_un.d_val)^,ModuleInfo.fingerprint,SCE_DBG_NUM_FINGERPRINT);
    Write('DT_SCE_FINGERPRINT:');
     For i:=0 to SCE_DBG_NUM_FINGERPRINT-1 do
      Write(HexStr(ModuleInfo.fingerprint[i],2));
    Writeln;
   end;

  //Offset of the filename in the string table.
  DT_SCE_FILENAME:;
  DT_SCE_MODULE_ATTR:;

  DT_SCE_EXPORT_LIB_ATTR:;
  DT_SCE_IMPORT_LIB_ATTR:;

  //This element holds the size, in bytes, of the DT_RELA relocation entry.
  DT_SCE_RELAENT:
  begin
   Writeln('DT_SCE_RELAENT:',HexStr(entry.d_un.d_val,16));
   Assert(SizeOf(elf64_rela)=entry.d_un.d_val);
  end;

  //This element holds the size, in bytes, of a symbol table entry.
  DT_SCE_SYMENT:
  begin
   Writeln('DT_SCE_SYMENT:',HexStr(entry.d_un.d_val,16));
   Assert(SizeOf(elf64_sym)=entry.d_un.d_val);
  end;

  DT_SONAME:Writeln('DT_SONAME:',PChar(pSceDynLib.pAddr+entry.d_un.d_val));

  else
   Writeln('TAG:',HexStr(entry.d_tag,16));
 end;
end;

procedure Telf_file.ParseSingleDynEntry(var entry:Elf64_Dyn);
var
 mu:TModuleValue;
 lu:TLibraryValue;
 _md:TMODULE;
 lib:TLIBRARY;
begin
 case entry.d_tag of
  //Offset of the filename in the string table.
  DT_SCE_FILENAME:
   begin
    mu.value:=entry.d_un.d_val;
    pSceFileName:=PChar(@pStrTable[mu.name_offset]);
    Writeln('DT_SCE_FILENAME:',pSceFileName,':',HexStr(mu.id,4)); //build file name
   end;
  DT_NEEDED:
   begin
    mu.value:=entry.d_un.d_val;
    _md.strName:=PChar(@pStrTable[mu.name_offset]);
    _add_need(_md.strName);
    Writeln('DT_NEEDED:',_md.strName); //import filename
   end;
  DT_SCE_MODULE_INFO:
   begin
    mu.value:=entry.d_un.d_val;
    _md.strName:=PChar(@pStrTable[mu.name_offset]);
    _md.Import:=False;
    Writeln('DT_SCE_MODULE_INFO:',_md.strName,':',HexStr(mu.id,4)); //current module name
    _set_mod(mu.id,_md);
    StrPLCopy(@ModuleInfo.name,_md.strName,SCE_DBG_MAX_NAME_LENGTH);
   end;
  DT_SCE_NEEDED_MODULE:
   begin
    mu.value:=entry.d_un.d_val;
    _md.strName:=PChar(@pStrTable[mu.name_offset]);
    _md.Import:=True;
    Writeln('DT_SCE_NEEDED_MODULE:',_md.strName,':',HexStr(mu.id,4)); //import module name
    _set_mod(mu.id,_md);
   end;
  DT_SCE_IMPORT_LIB:
   begin
    lu.value:=entry.d_un.d_val;
    lib.strName:=PChar(@pStrTable[lu.name_offset]);
    lib.Import:=True;
    Writeln('DT_SCE_IMPORT_LIB:',lib.strName,':',HexStr(lu.id,4)); //import lib name
    _set_lib(lu.id,lib);
   end;
  DT_SCE_EXPORT_LIB:
   begin
    lu.value:=entry.d_un.d_val;
    lib.strName:=PChar(@pStrTable[lu.name_offset]);
    lib.Import:=False;
    Writeln('DT_SCE_EXPORT_LIB:',lib.strName,':',HexStr(lu.id,4)); //export libname
    _set_lib(lu.id,lib);
   end;

  DT_SCE_MODULE_ATTR:
  begin
   mu.value:=entry.d_un.d_val;
   Writeln('DT_SCE_MODULE_ATTR      :',HexStr(mu.id,4),
                                   ':',mu.version_major,
                                   ':',mu.version_minor,
                                   ':',_mod_attr_text(mu.name_offset));
   _set_mod_attr(mu);
  end;

  DT_SCE_EXPORT_LIB_ATTR:
  begin
   lu.value:=entry.d_un.d_val;
   Writeln('DT_SCE_EXPORT_LIB_ATTR  :',HexStr(lu.id,4),
                                   ':',lu.version_major,
                                   ':',lu.version_minor,
                                   ':',_lib_attr_text(lu.name_offset));
   _set_lib_attr(lu);
  end;

  DT_SCE_IMPORT_LIB_ATTR:
  begin
   lu.value:=entry.d_un.d_val;
   Writeln('DT_SCE_IMPORT_LIB_ATTR  :',HexStr(lu.id,4),
                                   ':',lu.version_major,
                                   ':',lu.version_minor,
                                   ':',_lib_attr_text(lu.name_offset));
   _set_lib_attr(lu);
  end;

 end;
end;

function Telf_file.PrepareDynTables:Boolean;
var
 i:SizeInt;
begin
 Result:=False;
 if (pDynEnt=nil) then Exit;
 if (nDynEntCount=0) then Exit;
 For i:=0 to nDynEntCount-1 do
 begin
  PrepareTables(pDynEnt[i]);
 end;
 For i:=0 to nDynEntCount-1 do
 begin
  ParseSingleDynEntry(pDynEnt[i]);
 end;
 Result:=True;
end;

function __static_get_tls_adr:Pointer; assembler; nostackframe; forward;

function Telf_file.Prepare:Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 if FPrepared then Exit(True);
 Result:=PreparePhdr;
 if not Result then raise Exception.Create('Error PreparePhdr');
 Result:=PrepareDynTables;
 if not Result then raise Exception.Create('Error PrepareDynTables');
 Result:=MapImageIntoMemory;
 if not Result then raise Exception.Create('Error MapImageIntoMemory');
 Result:=LoadSymbolExport;
 PatchTls(@__static_get_tls_adr);
 FPrepared:=True;
end;

function _isSegmentLoadable(phdr:PElf64_Phdr):Boolean; inline;
begin
 Result:=False;
 Case phdr^.p_type of
  PT_SCE_RELRO,
  PT_LOAD:Result:=True;
 end;
end;

procedure _add_seg(MI:pSceKernelModuleInfo;adr:Pointer;size:DWORD;prot:Integer); inline;
var
 i:DWORD;
begin
 if (MI^.segmentCount>=SCE_DBG_MAX_SEGMENTS) then Exit;
 i:=MI^.segmentCount;
 MI^.segmentInfo[i].address:=adr;
 MI^.segmentInfo[i].size:=size;
 MI^.segmentInfo[i].prot:=prot;
 Inc(MI^.segmentCount);
end;

procedure Telf_file._calculateTotalLoadableSize(elf_phdr:Pelf64_phdr;s:SizeInt);
var
 i:SizeInt;
 loadAddrEnd  :Int64;
 alignedAddr  :Int64;
begin
 loadAddrEnd  :=0;
 if (s<>0) then
  For i:=0 to s-1 do
   if _isSegmentLoadable(@elf_phdr[i]) then
   begin
    alignedAddr:=AlignUp(elf_phdr[i].p_vaddr+elf_phdr[i].p_memsz,elf_phdr[i].p_align);
    if (alignedAddr>loadAddrEnd) then
    begin
     loadAddrEnd:=alignedAddr;
    end;
   end;
 mMap.nSize:=AlignUp(loadAddrEnd,PHYSICAL_PAGE_SIZE);
end;

function get_end_pad(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 Result:=0;
 if (alignment=0) then Exit;
 tmp:=addr mod alignment;
 if (tmp=0) then Exit;
 Result:=(alignment-tmp);
end;

procedure Telf_file._mapSegment(phdr:PElf64_Phdr;const name:PChar);
var
 Src:Pointer;
 Dst:Pointer;
 Size:QWORD;
begin
 Assert(IsAlign(phdr^.p_vaddr ,phdr^.p_align));
 Assert(IsAlign(phdr^.p_offset,phdr^.p_align));

 Size:=AlignUp(phdr^.p_memsz,PHYSICAL_PAGE_SIZE);
 Dst:=mMap.pAddr+phdr^.p_vaddr;
 Src:=mElf.pAddr+phdr^.p_offset;

 FillChar(Dst^,Size,0);

 Move(Src^,Dst^,phdr^.p_filesz);

 _add_seg(@ModuleInfo,Dst,Size,phdr^.p_flags);

 //Writeln('ENDPAD:',get_end_pad(phdr^.p_memsz,phdr^.p_align));
 Writeln(name,':',HexStr(Dst),'..',HexStr(Pointer(Dst+Size)),'  ',HexStr(Pointer(phdr^.p_offset)),'..',HexStr(Pointer(phdr^.p_offset+Size)));
end;

procedure Telf_file._addSegment(phdr:PElf64_Phdr;const name:PChar);// inline;
var
 Dst:Pointer;
 Size:QWORD;
begin
 if (phdr^.p_vaddr=0) or (phdr^.p_memsz=0) then Exit;
 Size:=phdr^.p_memsz;
 Dst:=mMap.pAddr+phdr^.p_vaddr;
 _add_seg(@ModuleInfo,Dst,Size,phdr^.p_flags);
 Writeln(name,':',HexStr(Dst),'..',HexStr(Pointer(Dst+Size)));
end;

function Telf_file._ro_seg_len:Integer;
var
 i:Integer;
begin
 Result:=Length(ro_segments);
 For i:=Low(ro_segments) to High(ro_segments) do
 if (ro_segments[i].pAddr=nil) then
 begin
  Exit(i);
 end;
end;

function Telf_file._ro_seg_find_lo(adr:Pointer):Integer;
var
 i:Integer;
begin
 Result:=-1;
 For i:=Low(ro_segments) to High(ro_segments) do
 begin
  if (ro_segments[i].pAddr=nil) then Exit;
  if (ro_segments[i].pAddr=adr) then Exit(i);
 end;
end;

function Telf_file._ro_seg_find_hi(adr:Pointer):Integer;
var
 i:Integer;
begin
 Result:=-1;
 For i:=Low(ro_segments) to High(ro_segments) do
 begin
  if (ro_segments[i].pAddr=nil) then Exit;
  if (ro_segments[i].pAddr+ro_segments[i].nSize=adr) then Exit(i);
 end;
end;

procedure Telf_file._add_ro_seg(phdr:PElf64_Phdr);
var
 Dst:Pointer;
 Size:QWORD;
 i,len:Integer;
begin
 //Writeln('_RDOL:',HexStr(phdr^.p_vaddr,16),'..',HexStr(phdr^.p_vaddr+phdr^.p_memsz,16));
 if (phdr^.p_vaddr=0) or (phdr^.p_memsz=0) then Exit;
 len:=_ro_seg_len;
 Size:=phdr^.p_memsz;
 Dst:=mMap.pAddr+phdr^.p_vaddr;
 if (len=0) then
 begin
  ro_segments[len].pAddr:=Dst;
  ro_segments[len].nSize:=Size;
 end else
 begin
  i:=_ro_seg_find_lo(Dst+Size);
  if (i<>-1) then
  begin
   ro_segments[i].pAddr:=Dst;
  end else
  begin
   i:=_ro_seg_find_hi(Dst);
   if (i<>-1) then
   begin
    ro_segments[i].nSize:=ro_segments[i].nSize+Size;
   end else
   begin
    if len>=Length(ro_segments) then Exit;
    ro_segments[len].pAddr:=Dst;
    ro_segments[len].nSize:=Size;
   end;
  end;
 end;
end;

procedure Telf_file._print_rdol;
var
 i:Integer;
 _lo,_hi:Pointer;
begin
 For i:=Low(ro_segments) to High(ro_segments) do
 begin
  if (ro_segments[i].pAddr=nil) then Exit;
  _lo:=ro_segments[i].pAddr;
  _hi:=_lo+ro_segments[i].nSize;
  if (_lo<>_hi) then
   Writeln('.RDOL:',HexStr(_lo),'..',HexStr(_hi));
 end;
end;

function Telf_file._ro_seg_adr_in(adr:Pointer;size:QWORD):Boolean;
var
 i:Integer;
 _lo,_hi,tmp:Pointer;
begin
 Result:=False;
 tmp:=adr+size;
 For i:=Low(ro_segments) to High(ro_segments) do
 begin
  if (ro_segments[i].pAddr=nil) then Exit;
  _lo:=ro_segments[i].pAddr;
  _hi:=_lo+ro_segments[i].nSize;
  If (adr>_lo) then _lo:=adr;
  If (tmp<_hi) then _hi:=tmp;
  if (_lo<_hi) then Exit(True);
 end;
end;

procedure Telf_file._find_tls_stub(elf_phdr:Pelf64_phdr;s:SizeInt);
Const
 patch_ld_size=SizeOf(Tpatch_ld);
var
 i:SizeInt;
 p:QWORD;
begin
  //find stub for static tls
 if (s<>0) then
  For i:=0 to s-1 do
  begin
   if _isSegmentLoadable(@elf_phdr[i]) then
    if ((elf_phdr[i].p_flags and PF_X)<>0) then
    begin
     p:=get_end_pad(elf_phdr[i].p_memsz,PHYSICAL_PAGE_SIZE);
     if (p>=patch_ld_size) then
     begin
      pTls.stub.pAddr:=Pointer(elf_phdr[i].p_vaddr+elf_phdr[i].p_memsz);
      pTls.stub.nSize:=p;
      Exit;
     end;
    end;
  end;
end;

function Telf_file.MapImageIntoMemory:Boolean;
var
 i:SizeInt;
 elf_hdr:Pelf64_hdr;
 elf_phdr:Pelf64_phdr;

 hdr:p_eh_frame_hdr;
 eh_frame_ptr:Int64;
begin
 Result:=False;
 if (Self=nil) then Exit;
 elf_hdr:=mElf.pAddr;
 if (elf_hdr=nil) then Exit;
 elf_phdr:=Pointer(elf_hdr)+SizeOf(elf64_hdr);
 if (elf_hdr^.e_phnum=0) then Exit;
 _calculateTotalLoadableSize(elf_phdr,elf_hdr^.e_phnum);
 if (mMap.nSize=0) then Exit;

 if (pTls.full_size<>0) then
 begin
  _find_tls_stub(elf_phdr,elf_hdr^.e_phnum);
  if (pTls.stub.nSize=0) then //extra stub
  begin
   pTls.stub.pAddr:=Pointer(mMap.nSize);
   pTls.stub.nSize:=PHYSICAL_PAGE_SIZE;
   mMap.nSize:=mMap.nSize+PHYSICAL_PAGE_SIZE;
  end;
 end;

 mMap.pAddr:=nil;

 if (ps4_app.prog=self) then
 begin
  mMap.pAddr:=Pointer($800000000);
 end;

 mMap.pAddr:=VirtualAlloc(mMap.pAddr,mMap.nSize,MEM_COMMIT or MEM_RESERVE,PAGE_READWRITE);

 Assert(mMap.pAddr<>nil);
 Assert(IsAlign(mMap.pAddr,16*1024));

 if (pTls.full_size<>0) then
 begin
  pTls.stub.pAddr:=mMap.pAddr+QWORD(pTls.stub.pAddr);
 end;

 For i:=0 to elf_hdr^.e_phnum-1 do
 begin
  Case elf_phdr[i].p_type of
   PT_LOAD:
    begin
     if ((elf_phdr[i].p_flags and PF_X)<>0) then
     begin
      _mapSegment(@elf_phdr[i],'.CODE');
     end else
     if ((elf_phdr[i].p_flags and PF_W)<>0) then
     begin
      _mapSegment(@elf_phdr[i],'.DATA');
     end;
    end;
   PT_SCE_RELRO:
    begin
     _mapSegment(@elf_phdr[i],'.RELR');
    end;
   PT_DYNAMIC:
    begin
     _addSegment(@elf_phdr[i],'.DYNM');
    end;

   PT_SCE_PROCPARAM,
   PT_TLS:
    begin
     if (elf_phdr[i].p_flags=PF_R) then
     begin
      _add_ro_seg(@elf_phdr[i]);
     end;
    end;

   PT_GNU_EH_FRAME:
    begin
     if (elf_phdr[i].p_flags=PF_R) then
     begin
      _add_ro_seg(@elf_phdr[i]);
     end;

     eh_frame_hdr.addr:=elf_phdr[i].p_vaddr;
     eh_frame_hdr.size:=elf_phdr[i].p_filesz;

     hdr:=mMap.pAddr+eh_frame_hdr.addr;

     eh_frame_ptr:=0;
     if parse_eh_frame_hdr(hdr,
                           eh_frame_hdr.size,
                           Int64(hdr),
                           eh_frame_ptr) then
     begin
      eh_frame.addr:=eh_frame_ptr-QWORD(mMap.pAddr);

      if (eh_frame_hdr.addr>eh_frame.addr) then
      begin
       eh_frame.size:=(eh_frame_hdr.addr-eh_frame.addr);
      end else
      begin
       eh_frame.size:=(mMap.nSize-eh_frame_hdr.addr)
      end;

     end;

    end;

   else;
  end;
 end;

 _print_rdol;



 pEntry:=Pelf64_hdr(mElf.pAddr)^.e_entry;

 Result:=True;
end;

function __rtype(nType:DWORD):RawByteString;
begin
 case nType of
  R_X86_64_NONE     :Result:='R_X86_64_NONE     ';
  R_X86_64_PC32     :Result:='R_X86_64_PC32     ';
  R_X86_64_COPY     :Result:='R_X86_64_COPY     ';
  R_X86_64_TPOFF32  :Result:='R_X86_64_TPOFF32  ';
  R_X86_64_DTPOFF32 :Result:='R_X86_64_DTPOFF32 ';
  R_X86_64_RELATIVE :Result:='R_X86_64_RELATIVE ';
  R_X86_64_TPOFF64  :Result:='R_X86_64_TPOFF64  ';
  R_X86_64_DTPMOD64 :Result:='R_X86_64_DTPMOD64 ';
  R_X86_64_DTPOFF64 :Result:='R_X86_64_DTPOFF64 ';
  R_X86_64_JUMP_SLOT:Result:='R_X86_64_JUMP_SLOT';
  R_X86_64_GLOB_DAT :Result:='R_X86_64_GLOB_DAT ';
  R_X86_64_64       :Result:='R_X86_64_64       ';
  else               Result:='R_'+HexStr(nType,16);
 end;
end;

function __nBind(nBind:Byte):RawByteString;
begin
 case nBind of
  STB_LOCAL :Result:='STB_LOCAL ';
  STB_GLOBAL:Result:='STB_GLOBAL';
  STB_WEAK  :Result:='STB_WEAK  ';
  else       Result:='STB_'+HexStr(nBind,2)+'    ';
 end;
end;

function __sType(sType:Byte):RawByteString;
begin
 case sType of
  STT_NOTYPE :Result:='STT_NOTYPE ';
  STT_OBJECT :Result:='STT_OBJECT ';
  STT_FUN    :Result:='STT_FUN    ';
  STT_SECTION:Result:='STT_SECTION';
  STT_FILE   :Result:='STT_FILE   ';
  STT_COMMON :Result:='STT_COMMON ';
  STT_TLS    :Result:='STT_TLS    ';
  STT_SCE    :Result:='STT_SCE    ';
  else        Result:='STT_'+HexStr(sType,2)+'     ';
 end;
end;

function __other(st_other:Byte):RawByteString;
begin
 case st_other of
  STV_DEFAULT  :Result:='STV_DEFAULT  ';
  STV_INTERNAL :Result:='STV_INTERNAL ';
  STV_HIDDEN   :Result:='STV_HIDDEN   ';
  STV_PROTECTED:Result:='STV_PROTECTED';
  else          Result:='STV_'+HexStr(st_other,2)+'       ';
 end;
end;

function __shndx(st_shndx:Word):RawByteString;
begin
 case st_shndx of
  SHN_UNDEF  :Result:='IMPORT';
  else        Result:='EXPORT';
 end;
end;

procedure _rela_info_convert(Info:PRelaInfo;pRela:Pelf64_rela;symbol:Pelf64_sym); inline;
begin
 Info^.value:=symbol^.st_value;
 Info^.Offset:=pRela^.r_offset;
 Info^.Addend:=pRela^.r_addend;
 Info^.rType:=ELF64_R_TYPE(pRela^.r_info);
 Info^.sBind:=ELF64_ST_BIND(symbol^.st_info);
 Info^.sType:=ELF64_ST_TYPE(symbol^.st_info);
 Info^.shndx:=symbol^.st_shndx;
end;

function Telf_file.RelocateRelaEnum(cbs:TOnRelaInfoCb;data:Pointer):Boolean;
var
 i:SizeInt;
 pRela:Pelf64_rela;
 Info:TRelaInfo;
 symbol:Pelf64_sym;
 nSymIdx:DWORD;
begin
 Result:=False;
 if (cbs=nil) then Exit;
 if (nRelaCount=0) then Exit(True);
 For i:=0 to nRelaCount-1 do
 begin
  pRela:=@pRelaEntries[i];
  nSymIdx:=ELF64_R_SYM(pRela^.r_info);
  symbol:=@pSymTab[nSymIdx];
  Info.pName:=@pStrTable[symbol^.st_name];
  _rela_info_convert(@Info,pRela,symbol);

  case Info.sType of
   STT_NOTYPE :;
   STT_OBJECT :;
   STT_FUN    :;
   STT_SECTION:;
   STT_FILE   :;
   STT_COMMON :;
   STT_SCE    :;
   STT_TLS    :Writeln(__sType(Info.sType));
   else
    Writeln(__sType(Info.sType));
  end;

  cbs(Self,@Info,data);

 end;
 Result:=True;
end;

function Telf_file.RelocatePltRelaEnum(cbs:TOnRelaInfoCb;data:Pointer):Boolean;
var
 i:SizeInt;
 pRela:Pelf64_rela;
 Info:TRelaInfo;
 symbol:Pelf64_sym;
 nSymIdx:DWORD;
begin
 Result:=False;
 if (cbs=nil) then Exit;
 if (nPltRelaCount=0) then Exit(True);
 For i:=0 to nPltRelaCount-1 do
 begin
  pRela:=@pPltRela[i];
  nSymIdx:=ELF64_R_SYM(pRela^.r_info);
  symbol:=@pSymTab[nSymIdx];
  Info.pName:=@pStrTable[symbol^.st_name];
  _rela_info_convert(@Info,pRela,symbol);

  case Info.sType of
   STT_NOTYPE :;
   STT_OBJECT :;
   STT_FUN    :;
   STT_SECTION:;
   STT_FILE   :;
   STT_COMMON :;
   STT_SCE    :;
   STT_TLS    :Writeln(__sType(Info.sType));
   else
    Writeln(__sType(Info.sType));
  end;

  cbs(Self,@Info,data);
 end;
 Result:=True;
end;

function Telf_file.ParseSymbolsEnum(cbs:TOnRelaInfoCb;data:Pointer):Boolean;
var
 i:SizeInt;
 Info:TRelaInfo;
 symbol:Pelf64_sym;
begin
 Result:=False;
 if (cbs=nil) then Exit;

 if (nSymTabCount=0) then Exit(True);
 For i:=0 to nSymTabCount-1 do
 begin
  symbol:=@pSymTab[i];
  Info.pName:=@pStrTable[symbol^.st_name];

  Info.value:=symbol^.st_value;
  Info.Offset:=0;
  Info.Addend:=0;
  Info.rType:=R_X86_64_64;
  Info.sBind:=ELF64_ST_BIND(symbol^.st_info);
  Info.sType:=ELF64_ST_TYPE(symbol^.st_info);
  Info.shndx:=symbol^.st_shndx;

  if (Info.shndx<>SHN_UNDEF) then
  begin
   cbs(Self,@Info,data);
  end;

  if (Info.shndx<>SHN_UNDEF) then
  case Info.sType of
   STT_NOTYPE :;
   STT_OBJECT :;
   STT_FUN    :;
   //STT_SECTION:;
   //STT_FILE   :;
   STT_COMMON :;
   STT_TLS    :;
   STT_SCE    :;
   else
    Writeln(__sType(Info.sType));
  end;

 end;
 Result:=True;
end;

var
 mod_space:TMODULE=(
  attr:0;
  Import:True;
  strName:'';
 );

var
 lib_space:TLIBRARY=(
  parent:nil;
  MapSymbol:nil;
  attr:0;
  Import:True;
  strName:'';
  Fset_proc_cb:nil;
  Fget_proc_cb:nil;
 );

function _convert_info_name(elf:Telf_file;
                            Info:PRelaInfo;
                            IInfo:PResolveImportInfo
                            ):Boolean;
var
 nModId,nLibId:WORD;
begin
 Result:=True;

 nModId:=$FFFF; //no mod
 nLibId:=$FFFF; //no lib

 case Info^.sType of
  STT_NOTYPE: //original string
   begin
    IInfo^.nid:=ps4_nid_hash(Info^.pName);

    if (Info^.shndx=SHN_UNDEF) then //import
    begin
     //Export link without explicit name
    end else
    begin
     nModId:=elf._find_mod_export;
     nLibId:=elf._find_lib_export;
    end;

   end;
  STT_SCE: //base64 string without specifying the module and library
   begin
    if not DecodeValue64(Info^.pName,StrLen(Info^.pName),IInfo^.nid) then
    begin
     Exit(False);
    end;

    if (Info^.shndx=SHN_UNDEF) then //import
    begin
     //Export link without explicit name
    end else
    begin
     nModId:=elf._find_mod_export;
     nLibId:=elf._find_lib_export;
    end;

   end;
  else //base64 string specifying the module and library
   begin
    if not DecodeEncName(Info^.pName,nModId,nLibId,IInfo^.nid) then
    begin
     Exit(False);
    end;
   end;
 end;

 if (nModId=$FFFF) then
 begin
  IInfo^._md:=@mod_space;
 end else
 begin
  IInfo^._md:=elf._get_mod(nModId);
 end;

 if (nLibId=$FFFF) then
 begin
  IInfo^.lib:=@lib_space;
 end else
 begin
  IInfo^.lib:=elf._get_lib(nLibId);
 end;

end;

Procedure OnLoadRelaExport(elf:Telf_file;Info:PRelaInfo;data:Pointer);

 procedure _do_set(nSymVal:Pointer); inline;
 begin
  if (Info^.Offset<>0) then
  begin
   PPointer(elf.mMap.pAddr+Info^.Offset)^:=nSymVal;
  end;
 end;

 procedure _do_load(nSymVal:Pointer);
 var
  IInfo:TResolveImportInfo;

  val:Pointer;

  Import:Boolean;

 begin
  Import:=(Info^.shndx=SHN_UNDEF);

  if Import then Exit;

  IInfo:=Default(TResolveImportInfo);

  if not _convert_info_name(elf,Info,@IInfo) then
  begin
   Writeln(StdErr,'Error decode:',Info^.pName);
   Exit;
  end;

  if (IInfo._md=nil)then
  begin
   Writeln(StdErr,'Unknow module from ',Info^.pName);
  end else
  if (IInfo._md^.Import<>Import) then
  begin
   Writeln(StdErr,'Wrong module ref:',IInfo._md^.strName,':',IInfo._md^.Import,'<>',Import);
  end;

  if (IInfo.lib=nil) then
  begin
   Writeln(StdErr,'Unknow library from ',Info^.pName);
   Exit;
  end else
  if (IInfo.lib^.Import<>Import) then
  begin
   Writeln(StdErr,'Wrong library ref:',IInfo.lib^.strName,':',IInfo.lib^.Import,'<>',Import);
   Exit;
  end;

  _do_set(nSymVal);

  val:=elf.mMap.pAddr+Info^.value;

  IInfo.lib^.set_proc(IInfo.nid,val);

 end;

begin

 case Info^.rType of
  R_X86_64_NONE    :Writeln('R_X86_64_NONE    ');
  R_X86_64_PC32    :Writeln('R_X86_64_PC32    ');
  R_X86_64_COPY    :Writeln('R_X86_64_COPY    ');

  R_X86_64_TPOFF32 :Writeln('R_X86_64_TPOFF32 ');
  R_X86_64_DTPOFF32:Writeln('R_X86_64_DTPOFF32');

  R_X86_64_RELATIVE:
   begin
    _do_set(elf.mMap.pAddr+Info^.Addend);
   end;

  R_X86_64_TPOFF64:
    begin            //tls_offset????
     _do_load(Pointer(elf.pTls.offset+Info^.value+Info^.Addend));
    end;

  R_X86_64_DTPMOD64:
    begin            //tls_index????
     _do_set(Pointer(elf.pTls.index));
    end;

  R_X86_64_DTPOFF64:  //tls
    begin
     _do_load(Pointer(Info^.value+Info^.Addend));
    end;

  R_X86_64_JUMP_SLOT,
  R_X86_64_GLOB_DAT,
  R_X86_64_64:
   begin
    Case Info^.sBind of
     STB_LOCAL:
     begin
      _do_set(elf.mMap.pAddr+Info^.value+Info^.Addend);
     end;
     STB_GLOBAL,
     STB_WEAK:
      begin
       _do_load(elf.mMap.pAddr+Info^.value+Info^.Addend);
      end;
     else
      Writeln(StdErr,'invalid sym bingding ',Info^.sBind);
    end;
   end;

  else
   Writeln(StdErr,'rela type not handled ', HexStr(Info^.rType,8));

 end;

end;

//

Procedure OnLoadRelaImport(elf:Telf_file;Info:PRelaInfo;data:Pointer);

 procedure _do_set(nSymVal:Pointer); inline;
 begin
  if (Info^.Offset<>0) then
  begin
   PPointer(elf.mMap.pAddr+Info^.Offset)^:=nSymVal;
  end;
 end;

 procedure _do_load(nSymVal:Pointer);
 var
  IInfo:TResolveImportInfo;

  Import:Boolean;

 begin
  Import:=(Info^.shndx=SHN_UNDEF);

  if not Import then Exit;

  IInfo:=Default(TResolveImportInfo);

  if not _convert_info_name(elf,Info,@IInfo) then
  begin
   Writeln(StdErr,'Error decode:',Info^.pName);
   Exit;
  end;

  if (IInfo._md=nil) then
  begin
   Writeln(StdErr,'Unknow module from ',Info^.pName);
  end else
  if (IInfo._md^.Import<>Import) then
  begin
   Writeln(StdErr,'Wrong module ref:',IInfo._md^.strName,':',IInfo._md^.Import,'<>',Import);
  end;

  if (IInfo.lib=nil) then
  begin
   Writeln(StdErr,'Unknow library from ',Info^.pName);
   Exit;
  end else
  if (IInfo.lib^.Import<>Import) then
  begin
   Writeln(StdErr,'Wrong library ref:',IInfo.lib^.strName,':',IInfo.lib^.Import,'<>',Import);
   Exit;
  end;

  nSymVal:=nil;
  if (data<>nil) and (PPointer(data)[0]<>nil) and (Info^.Offset<>0) then
  begin
   IInfo.pName :=Info^.pName;
   IInfo.Offset:=Info^.Offset;
   IInfo.rType :=Info^.rType;
   IInfo.sBind :=Info^.sBind;
   IInfo.sType :=Info^.sType;
   nSymVal:=TResolveImportCb(PPointer(data)[0])(elf,@IInfo,PPointer(data)[1]);
   if nSymVal<>nil then nSymVal:=nSymVal+Info^.Addend;
   _do_set(nSymVal);
  end;

 end;

begin
 case Info^.rType of
  R_X86_64_TPOFF64:
    begin            //tls_offset????
     _do_load(Pointer(elf.pTls.offset+Info^.value+Info^.Addend));
    end;

  R_X86_64_DTPMOD64:;

  R_X86_64_DTPOFF64:  //tls
    begin
     _do_load(Pointer(Info^.value+Info^.Addend));
    end;

  R_X86_64_JUMP_SLOT,
  R_X86_64_GLOB_DAT,
  R_X86_64_64:
   begin
    Case Info^.sBind of
     STB_GLOBAL,
     STB_WEAK:
      begin
       _do_load(elf.mMap.pAddr+Info^.value+Info^.Addend);
      end;
    end;
   end;
 end;
end;


Procedure OnDumpRela(elf:Telf_file;Info:PRelaInfo;data:Pointer);
const
 NL=#13#10;

 procedure FWrite(Const str:RawByteString); inline;
 begin
  FileWrite(THandle(Data),PChar(str)^,Length(str))
 end;

 procedure FWriteln(Const str:RawByteString); inline;
 begin
  FWrite(str+NL);
 end;

 procedure _do_set(nSymVal:Pointer);
 begin
  if (Info^.shndx=SHN_UNDEF) then
  begin
   FWriteln(__nBind(Info^.sBind)+':IMPORT:O:'+HexStr(Info^.Offset,16)+':V:'+HexStr(nSymVal));
  end else
  begin
   FWriteln(__nBind(Info^.sBind)+':EXPORT:O:'+HexStr(Info^.Offset,16)+':V:'+HexStr(nSymVal));
  end;
 end;

 procedure _do_load(nSymVal:Pointer);
 var
  functName:PChar;

  IInfo:TResolveImportInfo;

  Import:Boolean;

 begin
  Import:=(Info^.shndx=SHN_UNDEF);

  IInfo:=Default(TResolveImportInfo);

  if not _convert_info_name(elf,Info,@IInfo) then
  begin
   FWriteln('Error decode:'+Info^.pName);
  end;

  if (IInfo._md=nil) then
  begin
   FWriteln('Unknow module from '+Info^.pName);
  end else
  if (IInfo._md^.Import<>Import) then
  begin
   FWriteln('Wrong module ref:'+IInfo._md^.strName+':'+BoolToStr(IInfo._md^.Import)+'<>'+BoolToStr(Import));
  end;

  if (IInfo.lib=nil) then
  begin
   FWriteln('Unknow library from '+Info^.pName);
  end else
  if (IInfo.lib^.Import<>Import) then
  begin
   FWriteln('Wrong library ref:'+IInfo.lib^.strName+':'+BoolToStr(IInfo.lib^.Import)+'<>'+BoolToStr(Import));
  end;

  functName:=ps4libdoc.GetFunctName(IInfo.nid);

  FWrite(__nBind(Info^.sBind)+':'+__sType(Info^.sType)+':');

  if (IInfo._md<>nil) then
  begin
   FWrite(IInfo._md^.strName);
  end;

  FWriteln(':');

  if (IInfo.lib<>nil) then
  begin
   FWrite(IInfo.lib^.strName);
  end;

  FWriteln(':'+functName);

  if Import then
  begin
   FWriteln(' IMPORT:N:'+Info^.pName+':O:'+HexStr(Info^.Offset,16)+':V:'+HexStr(nSymVal)+':NID:'+HexStr(IInfo.nid,16));
  end else
  begin
   FWriteln(' EXPORT:N:'+Info^.pName+':O:'+HexStr(Info^.Offset,16)+':V:'+HexStr(nSymVal)+':NID:'+HexStr(IInfo.nid,16));
  end;

 end;

begin

 case Info^.rType of
  R_X86_64_NONE    :FWriteln('R_X86_64_NONE');
  R_X86_64_PC32    :FWriteln('R_X86_64_PC32');
  R_X86_64_COPY    :FWriteln('R_X86_64_COPY');

  R_X86_64_TPOFF32 :FWriteln('R_X86_64_TPOFF32');
  R_X86_64_DTPOFF32:FWriteln('R_X86_64_DTPOFF32');

  R_X86_64_RELATIVE:
   begin
    _do_set(Pointer(Info^.Addend));
   end;

  R_X86_64_TPOFF64:
    begin            //tls_offset????
     _do_load(Pointer(elf.pTls.offset+Info^.value+Info^.Addend));
    end;

  R_X86_64_DTPMOD64:
    begin            //tls_index????
     _do_set(Pointer(0));
    end;

  R_X86_64_DTPOFF64:  //tls
    begin
     _do_load(Pointer(Info^.value+Info^.Addend));
    end;

  R_X86_64_JUMP_SLOT,
  R_X86_64_GLOB_DAT,
  R_X86_64_64:
   begin
    Case Info^.sBind of
     STB_LOCAL:
     begin
      _do_set(Pointer(Info^.value+Info^.Addend));
     end;
     STB_GLOBAL,
     STB_WEAK:
      begin
        _do_load(Pointer(Info^.value)+Info^.Addend);
      end;
     else
        FWriteln('invalid sym bingding '+__nBind(Info^.sBind));
    end;
   end;

  else
   FWriteln('rela type not handled '+HexStr(Info^.rType,8));

 end;

end;

function Telf_file.LoadSymbolExport:Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 if (mMap.pAddr=nil) or (mMap.nSize=0) then Exit;
 Result:=RelocateRelaEnum(@OnLoadRelaExport,nil);
 if not Result then raise Exception.Create('Error RelocateRelaEnum');
 Result:=RelocatePltRelaEnum(@OnLoadRelaExport,nil);
 if not Result then raise Exception.Create('Error RelocatePltRelaEnum');
 Result:=ParseSymbolsEnum(@OnLoadRelaExport,nil);
 if not Result then raise Exception.Create('Error ParseSymbolsEnum');
end;

Procedure Telf_file.LoadSymbolImport(cbs,data:Pointer);
var
 _data:array[0..1] of Pointer;
begin
 if (Self=nil) then Exit;
 if FLoadImport then Exit;
 if (mMap.pAddr=nil) or (mMap.nSize=0) then Exit;
 _data[0]:=cbs;
 _data[1]:=data;
 RelocateRelaEnum(@OnLoadRelaImport,@_data);
 RelocatePltRelaEnum(@OnLoadRelaImport,@_data);
 ParseSymbolsEnum(@OnLoadRelaImport,@_data);
 FLoadImport:=True;
end;

Procedure Telf_file.ReLoadSymbolImport(cbs,data:Pointer);
var
 _data:array[0..1] of Pointer;
begin
 if (Self=nil) then Exit;
 if not FLoadImport then Exit;
 if (mMap.pAddr=nil) or (mMap.nSize=0) then Exit;
 _data[0]:=cbs;
 _data[1]:=data;
 RelocateRelaEnum(@OnLoadRelaImport,@_data);
 RelocatePltRelaEnum(@OnLoadRelaImport,@_data);
 ParseSymbolsEnum(@OnLoadRelaImport,@_data);
end;

Procedure OnDumpInitProc(elf:Telf_file;F:THandle);
const
 NL=#13#10;

 procedure FWrite(Const str:RawByteString); inline;
 begin
  FileWrite(F,PChar(str)^,Length(str))
 end;

 procedure FWriteln(Const str:RawByteString); inline;
 begin
  FWrite(str+NL);
 end;

var
 i,c,o:SizeInt;
 base:Pointer;
 P:PPointer;

begin
 FWriteln('e_entry:' +HexStr(elf.pEntry,16));

 base:=elf.mMap.pAddr;

 FWriteln('dtInit:'+HexStr(elf.dtInit,16));

 c:=elf.pInit.dt_preinit_array_count;
 if (c<>0) then
  Case Int64(elf.pInit.dt_preinit_array) of
   -1,0,1:;//skip
   else
    begin
     P:=base+elf.pInit.dt_preinit_array;
     dec(c);
     For i:=0 to c do
     begin
      o:=SizeInt(P[i])-SizeInt(base);
      FWriteln('dt_preinit['+IntToStr(i)+']:' +HexStr(o,16));
     end;
    end;
  end;

 c:=elf.pInit.dt_init_array_count;
 if (c<>0) then
  Case Int64(elf.pInit.dt_init_array) of
   -1,0,1:;//skip
   else
    begin
     P:=base+elf.pInit.dt_init_array;
     dec(c);
     For i:=0 to c do
     begin
      o:=SizeInt(P[i])-SizeInt(base);
      FWriteln('dt_init['+IntToStr(i)+']:' +HexStr(o,16));
     end;
    end;
  end;

 FWriteln('dtFini:'+HexStr(elf.dtFini,16));
end;

function Telf_file.DympSymbol(F:THandle):Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 OnDumpInitProc(Self,F);
 Result:=RelocateRelaEnum(@OnDumpRela,Pointer(F));
 Result:=RelocatePltRelaEnum(@OnDumpRela,Pointer(F));
 Result:=ParseSymbolsEnum(@OnDumpRela,Pointer(F));
end;

function ps4_nid_hash(const name:RawByteString):QWORD;
const
 salt:array[0..15] of Byte=($51,$8D,$64,$A6,$35,$DE,$D8,$C1,$E6,$B0,$39,$B1,$C3,$E5,$52,$30);
var
 Context:TSHA1Context;
 Digest:TSHA1Digest;
begin
 SHA1Init(Context);
 SHA1Update(Context,PChar(name)^,Length(name));
 SHA1Update(Context,salt,Length(salt));
 SHA1Final(Context,Digest);
 Result:=PQWORD(@Digest)^;
end;

function EncodeValue64(nVal:QWORD):RawByteString;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 SetLength(Result,nEncLenMax);
 For i:=nEncLenMax downto 1 do
 begin
  if (i<>nEncLenMax) then
  begin
   nIndex:=nVal and 63;
   nVal:=nVal shr 6;
  end else
  begin
   nIndex:=(nVal and 15) shl 2;
   nVal:=nVal shr 4;
  end;
  case nIndex of
    0..25:Result[i]:=Char(nIndex+Byte('A')-0);
   26..51:Result[i]:=Char(nIndex+Byte('a')-26);
   52..61:Result[i]:=Char(nIndex+Byte('0')-52);
       62:Result[i]:='+';
       63:Result[i]:='-';
  end;
 end;
end;

function DecodeValue64(strEnc:PAnsiChar;len:SizeUint;var nVal:QWORD):Boolean;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 Result:=False;
 nVal:=0;
 if (len>nEncLenMax) or (len=0) then Exit;
 For i:=0 to len-1 do
 begin
  case strEnc[i] of
   'A'..'Z':nIndex:=Byte(strEnc[i])-Byte('A')+0;
   'a'..'z':nIndex:=Byte(strEnc[i])-Byte('a')+26;
   '0'..'9':nIndex:=Byte(strEnc[i])-Byte('0')+52;
   '+':nIndex:=62;
   '-':nIndex:=63;
   else Exit;
  end;
  if (i<(nEncLenMax-1)) then
  begin
   nVal:=nVal shl 6;
   nVal:=nVal or nIndex;
  end else
  begin
   nVal:=nVal shl 4;
   nVal:=nVal or (nIndex shr 2);
  end;
 end;
 Result:=True;
end;

function DecodeValue16(strEnc:PAnsiChar;len:SizeUint;var nVal:WORD):Boolean;
const
 nEncLenMax=3;
var
 i,nIndex:Integer;
begin
 Result:=False;
 nVal:=0;
 if (len>nEncLenMax) or (len=0) then Exit;
 For i:=0 to len-1 do
 begin
  case strEnc[i] of
   'A'..'Z':nIndex:=Byte(strEnc[i])-Byte('A')+0;
   'a'..'z':nIndex:=Byte(strEnc[i])-Byte('a')+26;
   '0'..'9':nIndex:=Byte(strEnc[i])-Byte('0')+52;
   '+':nIndex:=62;
   '-':nIndex:=63;
   else Exit;
  end;
  if (i<(nEncLenMax-1)) then
  begin
   nVal:=nVal shl 6;
   nVal:=nVal or nIndex;
  end else
  begin
   nVal:=nVal shl 4;
   nVal:=nVal or (nIndex shr 2);
  end;
 end;
 Result:=True;
end;

function DecodeEncName(strEncName:PAnsiChar;var nModuleId,nLibraryId:WORD;var nNid:QWORD):Boolean;
var
 i,len:Integer;
begin
 Result:=False;
 len:=StrLen(strEncName);
 i:=IndexByte(strEncName^,len,Byte('#'));
 if (i=-1) then Exit;
 if not DecodeValue64(strEncName,i,nNid) then Exit;
 Inc(i);
 Inc(strEncName,i);
 Dec(len,i);
 i:=IndexByte(strEncName^,len,Byte('#'));
 if (i=-1) then Exit;
 if not DecodeValue16(strEncName,i,nLibraryId) then Exit;
 Inc(i);
 Inc(strEncName,i);
 Dec(len,i);
 if not DecodeValue16(strEncName,len,nModuleId) then Exit;
 Result:=True;
end;

//64488b042500000000       mov    %fs:0x0,%rax
//                     0   1  2  3  4
//  0    1    2    3   4   5  6  7  8
//[64] [48] [8b] [04] 25 [00 00 00 00] :0x0


//MOV RAX,qword ptr FS:[0x0]

//  0    1    2
//[66] [66] [66]

//  3    4    5    6   7   8  9 10 11
//[64] [48] [8b] [04] 25 [00 00 00 00]

//[66] [66] [66]
//64488b042500000000 //data16 data16 data16 mov %fs:0x0,%rax


//                  v this adr - base adr
//[e8] [9e be b3 00] relative
//^ call
//  0    1  2  3  4
//90 = nop

//48030504853603           add    0x3368504(%rip),%rax        # 0x81d44a8
//      v-add
//[48] [03] [05] [04 85 36 03] - adr
//^64 bits

//000000010000366B 51                       push   %rcx
//000000010000366C 48b9d5728a0e00000000     movabs $0xe8a72d5,%rcx
//0000000100003676 ff2500000000             jmpq   *0x0(%rip)        # 0x10000367c <main+76>
//ff 25 eb ff ff ff
// 0  1  2  3  4  5

// $51 push   %rcx
// $59 pop    %rcx


//const
 //DWNOP=$90909090;

function IndexUnalignDWord(Const buf;len:SizeInt;b:DWord):SizeInt;
var
 psrc:PDWORD;
 pend:PBYTE;
begin
 psrc:=@buf;
 pend:=PBYTE(psrc)+len;
 while (PBYTE(psrc)<pend) do
 begin
  if (psrc^=b) then
  begin
   Result:=PBYTE(psrc)-PBYTE(@buf);
   exit;
  end;
  inc(PBYTE(psrc));
 end;
 Result:=-1;
end;

procedure Telf_file._PatchTls(Proc:Pointer;Addr:PByte;Size:QWORD);
Const
 prefix1:DWORD=$048b4864;

 prefix2:Byte =$25;
 prefix3:DWORD=$00000000;

 //prefix3:DWORD=$05034800;

 //prefix4:QWORD=$0503480000000025;

 prefix5:DWORD=$666666;

 prefixm:DWORD=$FFFFFF;

var
 Stub:Pointer;
 c:SizeInt;
 _call:Tpatch_fs;

 procedure do_patch_ld(p:PByte); inline;
 var
  _jmp:Tpatch_ld;
 begin
  _jmp:=_patch_ld;
  _jmp._addr:=proc;
  Ppatch_ld(p)^:=_jmp;
 end;

 procedure do_patch(p:PByte); inline;
 begin
  _call._ofs:=Integer(PtrInt(Stub)-PtrInt(P)-PtrInt(@Tpatch_fs(nil^).{_pop_rcx}_nop));
  Ppatch_fs(p)^:=_call;

  p:=p-3;
  if ((PDWORD(p)^ and prefixm)=prefix5) then
  begin
   p[0]:=$90; //nop
   p[1]:=$90; //nop
   p[2]:=$90; //nop
  end;

 end;

 procedure do_find(p:PByte;s:SizeInt);
 var
  i:SizeInt;
  A:PByte;
 begin
  repeat
   i:=IndexUnalignDWord(P^,s,prefix1);
   if (i=-1) then Break;
   A:=@P[i];

   if (A[4]=prefix2) and (PDWORD(@A[5])^=prefix3) then
   //if (PQWORD(@A[4])^=prefix4) then
   if not _ro_seg_adr_in(A,12) then
   begin
    Inc(c);
    do_patch(A);
   end;

   Inc(i,4);
   Inc(P,i);
   Dec(s,i);
  until (s<=0);
 end;

begin
 if (Size=0) then Exit;
 if (Size>=12) then Size:=Size-12;
 c:=0;
 _call:=_patch_fs;

 Stub:=pTls.stub.pAddr;

 do_find(@Addr[0],Size-0);

 //Writeln('patch_tls_count=',c);
 //do_find(@Addr[1],Size-1);
 //Writeln('patch_tls_count=',c);
 //do_find(@Addr[2],Size-2);
 //Writeln('patch_tls_count=',c);
 //do_find(@Addr[3],Size-3);

 Writeln('patch_tls_count=',c);
 if (c<>0) then
 begin
  do_patch_ld(Stub);
 end;
end;

procedure Telf_file.PatchTls(Proc:Pointer);
var
 i,c:DWORD;
begin
 if (Self=nil) or (Proc=nil) then Exit;
 if (pTls.full_size=0) then Exit; //no tls?
 c:=ModuleInfo.segmentCount;
 if (c<>0) then
 For i:=0 to c-1 do
  if (ModuleInfo.segmentInfo[i].prot and PF_X<>0) then
  begin
   _PatchTls(Proc,
             ModuleInfo.segmentInfo[i].address,
             ModuleInfo.segmentInfo[i].Size);
  end;
end;

function _static_get_tls_adr:Pointer; MS_ABI_Default;
var
 elf:Telf_file;
begin
 Result:=nil;
 elf:=Telf_file(ps4_app.prog);
 if (elf=nil) then Exit;
 Result:=elf._get_tls;
end;

function __static_get_tls_adr:Pointer; assembler; nostackframe;
asm
 push %rcx
 push %rdx
 push %r8
 push %r9
 push %R10
 push %R11

 call _static_get_tls_adr

 pop %R11
 pop %R10
 pop %r9
 pop %r8
 pop %rdx
 pop %rcx
end;

//-not need:
//-RAX:result
////-RCX,RDX:Tpatch_fs
//-other:win call save

//The registers RAX, RCX, RDX,  R8,  R9, R10, R11               are considered volatile    (вызывающий сохраняет)
//The registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, and R15 are considered nonvolatile (вызываемый сохраняет)

function _dynamic_tls_get_addr(ti:PTLS_index):Pointer; SysV_ABI_CDecl;
var
 elf:Telf_file;
 tcb:Ptls_tcb;
begin
 Result:=nil;
 if (ti=nil) then Exit;
 elf:=Telf_file(ti^.ti_moduleid);
 if (elf=nil) then Exit;
 tcb:=elf._get_tls;
 if (tcb=nil) then Exit;
 Result:=tcb^._dtv.value+ti^.ti_tlsoffset;
end;

function Telf_file._init_tls(is_static:QWORD):Ptls_tcb;
Var
 tcb_size:QWORD;
 tcb:Ptls_tcb;
 base:Pointer;
 adr:Pointer;
begin
 tcb_size:=pTls.full_size;
 tcb:=_init_tls_tcb(tcb_size,is_static,Handle);
 base:=tcb^._dtv.value;
 Assert(IsAlign(base,pTls.align));
 if (pTls.tmpl_size<>0) then
 begin
  adr:=mMap.pAddr+pTls.tmpl_start;
  Move(adr^,base^,pTls.tmpl_size);
 end;
 Result:=tcb;
end;

function Telf_file._get_tls:Ptls_tcb;
begin
 Result:=_get_tls_tcb(Handle);
 if (Result=nil) then
 begin
  Result:=_init_tls(0);
 end;
end;

procedure Telf_file.InitThread(is_static:QWORD);
begin
 if (Self=nil) then Exit;
 if FInitThread then Exit;
 if (pTls.full_size=0) then Exit;
 if (_get_tls_tcb(Handle)<>nil) then Exit;
 _init_tls(is_static);
 FInitThread:=True;
end;

function __map_segment_prot(prot:Integer):DWORD;
begin
 Result:=0;
 if (prot=0) then Exit(PAGE_NOACCESS);

 if (prot and PF_X)<>0 then
 begin
  if (prot and PF_W)<>0 then
  begin
   Result:=PAGE_EXECUTE_READWRITE;
  end else
  if (prot and PF_R)<>0 then
  begin
   Result:=PAGE_EXECUTE_READ;
  end else
  begin
   Result:=PAGE_EXECUTE;
  end;
 end else
 if (prot and PF_W)<>0 then
 begin
  Result:=PAGE_READWRITE;
 end else
 begin
  Result:=PAGE_READONLY;
 end;
end;

procedure Telf_file.mapProt;
var
 i,c:DWORD;
 dummy:DWORD;

 R:Boolean;

begin
 if (Self=nil) then Exit;
 c:=ModuleInfo.segmentCount;
 if c<>0 then

 begin
  Writeln('MAPF:',HexStr(mMap.pAddr),'..',HexStr(mMap.pAddr+mMap.nSize));
  For i:=0 to c-1 do
   if (ModuleInfo.segmentInfo[i].prot and PF_X<>0) then
   begin
    R:=VirtualProtect(ModuleInfo.segmentInfo[i].address,
                      ModuleInfo.segmentInfo[i].Size,
                      {__map_segment_prot(ModuleInfo.segmentInfo[i].prot)} PAGE_EXECUTE_READWRITE,
                      @dummy);

    FlushInstructionCache(GetCurrentProcess,
                          ModuleInfo.segmentInfo[i].address,
                          ModuleInfo.segmentInfo[i].Size);

    Writeln('PF_X:',HexStr(ModuleInfo.segmentInfo[i].address),'..',
      HexStr(ModuleInfo.segmentInfo[i].address+ModuleInfo.segmentInfo[i].Size),':',
      R);
   end;
 end;

 if (pTls.stub.pAddr<>nil) then
 begin
  Assert(pTls.stub.nSize>=SizeOf(Tpatch_ld));
  //extra tls stub
  if (pTls.stub.nSize=PHYSICAL_PAGE_SIZE) then
  begin
   R:=VirtualProtect(pTls.stub.pAddr,
                     pTls.stub.nSize,
                     {PAGE_EXECUTE_READ}PAGE_EXECUTE_READWRITE,
                     @dummy);

   FlushInstructionCache(GetCurrentProcess,
                         pTls.stub.pAddr,
                         pTls.stub.nSize);

   Writeln('STUB:',HexStr(pTls.stub.pAddr),'..',HexStr(pTls.stub.pAddr+pTls.stub.nSize),':',R);
  end else
  //inline stub
  begin
   Writeln('STUB:',HexStr(pTls.stub.pAddr),'..',HexStr(pTls.stub.pAddr+pTls.stub.nSize));
  end;
 end;
end;

function call_dt_preinit_array(Params:PPS4StartupParams;Proc:Pointer):Integer;
begin
 Result:=0;
 if (Proc<>nil) then
 begin
  Result:=TinitProc(Proc)(Params^.argc,@Params^.argv,nil);
  Writeln('call_dt_preinit_array:',Result);
 end;
end;

function call_dt_init_array(Params:PPS4StartupParams;Proc:Pointer):Integer;
begin
 Result:=0;
 if (Proc<>nil) then
 begin
  Result:=TinitProc(Proc)(Params^.argc,@Params^.argv,nil);
  Writeln('call_dt_init_array:',Result);
 end;
end;

procedure Telf_file.mapCodeInit;
var
 Prog:Telf_file;
 //i,c:SizeInt;
 //StartupParams:TPS4StartupParams;
 //base:Pointer;
 //P:PPointer;
begin
 if (Self=nil) then Exit;
 Prog:=Telf_file(ps4_app.prog);
 if (Prog=nil) then Exit;

 Writeln('mapCodeInit:',pFileName);

 //StartupParams:=Default(TPS4StartupParams);
 //StartupParams.argc:=1;
 //StartupParams.argv[0]:=PChar(Prog.pFileName);

 //base:=mMap.pAddr;

 if (Prog<>Self) then
 begin
  module_start(0,nil,nil);
 end;

 //if (Prog<>Self) then
 //begin
 // //dt_Init
 // TinitProc(base+dtInit)(StartupParams.argc,@StartupParams.argv,nil);
 //end;

 {
 c:=pInit.dt_preinit_array_count;
 if (c<>0) then
  Case Int64(pInit.dt_preinit_array) of
   -1,0,1:;//skip
   else
    begin
     P:=base+pInit.dt_preinit_array;
     dec(c);
     For i:=0 to c do
     begin
      call_dt_preinit_array(@StartupParams,P[i]);
     end;
    end;
  end;

 c:=pInit.dt_init_array_count;
 if (c<>0) then
  Case Int64(pInit.dt_init_array) of
   -1,0,1:;//skip
   else
    begin
     P:=base+pInit.dt_init_array;
     dec(c);
     For i:=0 to c do
     begin
      call_dt_init_array(@StartupParams,P[i]);
     end;
    end;
  end;
 }
end;

Procedure Telf_file.InitProt;
begin
 if FInitProt then Exit;
 //ClearElfFile;
 mapProt;
 FInitProt:=True;
end;

Procedure Telf_file.InitCode;
begin
 if FInitCode then Exit;
 mapCodeInit;
 FInitCode:=True;
end;

function Telf_file.module_start(argc:size_t;argp,param:PPointer):Integer;
var
 //mp:PsceModuleParam;
 //M:Pointer;
 P:TmoduleStart;
begin
 Result:=0;

 //Pointer(mp):=Pointer(mMap.pAddr+pModuleParam);
 //M:=get_proc_by_name('module_start');

 Pointer(P):=Pointer(mMap.pAddr+dtInit);

 Writeln('>module_start:',pFileName);

 Result:=P(argc,argp,param);

 Writeln('<module_start:',pFileName);
end;

function Telf_file.GetCodeFrame:TMemChunk;
begin
 Result:=Default(TMemChunk);
 safe_move(mMap,Result,SizeOf(TMemChunk));
end;

function Telf_file.GetEntry:Pointer;
begin
 Case Int64(pEntry) of
  -1:Result:=nil;//skip
  else
   begin
    Result:=Pointer(mMap.pAddr+QWORD(pEntry));
   end;
 end;
end;

function Telf_file.GetdInit:Pointer;
begin
 Case Int64(dtInit) of
  -1:Result:=nil;//skip
  else
   begin
    Result:=Pointer(mMap.pAddr+QWORD(dtInit));
   end;
 end;
end;

function Telf_file.GetdFini:Pointer;
begin
 Case Int64(dtFini) of
  -1:Result:=nil;//skip
  else
   begin
    Result:=Pointer(mMap.pAddr+QWORD(dtFini));
   end;
 end;
end;

Function Telf_file.GetModuleInfo:SceKernelModuleInfo;
begin
 if (ModuleInfo.name[0]=#0) then
 begin
  MoveChar0(PChar(pFileName)^,ModuleInfo.name,SCE_DBG_MAX_NAME_LENGTH);
 end;
 Result:=ModuleInfo;
end;

Function Telf_file.GetModuleInfoEx:SceKernelModuleInfoEx;
begin
 Result:=inherited;

 if (ModuleInfo.name[0]=#0) then
 begin
  MoveChar0(PChar(pFileName)^,ModuleInfo.name,SCE_DBG_MAX_NAME_LENGTH);
 end;

 Result.name:=ModuleInfo.name;

 Result.tls_index        :=pTls.index;
 Result.tls_init_addr    :=mMap.pAddr+pTls.tmpl_start;
 Result.tls_init_size    :=pTls.tmpl_size;
 Result.tls_size         :=pTls.full_size;
 Result.tls_offset       :=pTls.offset;
 Result.tls_align        :=pTls.align;

 Result.init_proc_addr   :=mMap.pAddr+dtInit;
 Result.fini_proc_addr   :=mMap.pAddr+dtFini;

 Result.eh_frame_hdr_addr:=mMap.pAddr+eh_frame_hdr.addr;
 Result.eh_frame_addr    :=mMap.pAddr+eh_frame.addr;
 Result.eh_frame_hdr_size:=eh_frame_hdr.size;
 Result.eh_frame_size    :=eh_frame.size;

 Result.segments     :=ModuleInfo.segmentInfo;
 Result.segment_count:=ModuleInfo.segmentCount;
end;

procedure _Entry(P:TEntryPoint;pFileName:Pchar);
var
 StartupParams:TPS4StartupParams;
begin
 StartupParams:=Default(TPS4StartupParams);
 StartupParams.argc:=1;
 StartupParams.argv[0]:=pFileName;

 //OpenOrbis relies on the fact that besides %rdi and %rsp also link to StartupParams, a very strange thing
 asm
  xor %rsi,%rsi
  lea StartupParams,%rdi
  mov %rdi,%rsp
  jmp P
 end;

 //P(@StartupParams,nil);
end;

procedure Telf_file.mapCodeEntry;
var
 P:TEntryPoint;
begin
 Pointer(P):=Pointer(pEntry);
 Writeln('Entry:',HexStr(P));
 Case Int64(P) of
  -1,0,1:;//skip
  else
   begin
    Pointer(P):=Pointer(mMap.pAddr+QWORD(P));

    _Entry(P,PChar(pFileName));
   end;
 end;

end;

end.

