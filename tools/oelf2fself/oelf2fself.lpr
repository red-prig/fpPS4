program oelf2fself;

uses
 sysutils,
 DCPsha256,
 elf64 in '..\..\sys\elf64.pas',
 ps4libdoc;

const
 PAGE_SHIFT=14;
 PAGE_SIZE =1 shl PAGE_SHIFT; //16384
 PAGE_MASK =PAGE_SIZE-1;

 BLOCK_SIZE=$4000;

 SELF_MODE_SPECIFICUSER = $1;
 SELF_DATA_LSB          = $1;

 SELF_HEADER_SIZE          =$20;
 SELF_ENTRY_SIZE           =$50;
 SELF_ELF_HEADER_SIZE      =$40;
 SELF_ELF_PROGHEADER_SIZE  =$38;
 SELF_EXTENDED_HEADER_SIZE =$40;
 SELF_META_FOOTER_SIZE     =$50;
 SELF_NPDRM_BLOCK_SIZE     =$30;
 SELF_META_BLOCK_SIZE      =$50;
 SELF_META_DATA_BLOCK_SIZE =$20;
 SELF_SIGNATURE_SIZE       =$100;

type
 Tsha256Digest=array[0..31] of Byte;

 Tsignature=array[0..SELF_SIGNATURE_SIZE-1] of Byte;

 AByte=array of byte;

 SelfEntryInfo=record
  data:AByte;
  info:t_self_segment;
 end;

 a_self_segment=array of SelfEntryInfo;

 p_image_params=^t_image_params;
 t_image_params=packed record
  image_header:p_elf64_hdr;
  entry_addr  :Pointer;
  elf_size    :Integer;

  dyn_vaddr:p_elf64_dyn;

  tls_size     :QWORD;
  tls_align    :QWORD;
  tls_init_size:QWORD;
  tls_init_addr:Pointer;

  eh_frame_hdr_addr:Pointer;
  eh_frame_hdr_size:QWORD;

  proc_param_addr:pSceProcParam;
  proc_param_size:QWORD;

  module_param_addr:psceModuleParam;
  module_param_size:QWORD;

  dyn_id            :Integer;
  sce_dynlib_data_id:Integer;
  sce_comment_id    :Integer;
  dyn_exist         :Integer;

  dyn_offset          :QWORD;
  dyn_filesz          :QWORD;

  sce_dynlib_data_addr:QWORD;
  sce_dynlib_data_size:QWORD;

  sce_comment_offset  :QWORD;
  sce_comment_filesz  :QWORD;

  min_addr:QWORD;
  max_addr:QWORD;

  _selfEntries:a_self_segment;

  outputFself:THandle;
 end;

function maxInt64(a,b:Int64):Int64; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function minInt64(a,b:Int64):Int64; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function get_elf_phdr(elf_hdr:p_elf64_hdr):p_elf64_phdr;
begin
 if (elf_hdr=nil) then Exit(nil);
 if (elf_hdr^.e_phoff=0) then
 begin
  Result:=Pointer(elf_hdr+1);
 end else
 begin
  Result:=Pointer(elf_hdr)+elf_hdr^.e_phoff;
 end;
end;

function get_elf_phdr_offset(elf_hdr:p_elf64_hdr):Int64;
begin
 if (elf_hdr=nil) then Exit(0);
 if (elf_hdr^.e_phoff=0) then
 begin
  Result:=SizeOf(elf64_hdr);
 end else
 begin
  Result:=elf_hdr^.e_phoff;
 end;
end;

procedure fixup_offset_size(var offset,size:Int64;max:Int64);
var
 s,e:Int64;
begin
 s:=offset;
 e:=s+size;

 s:=MinInt64(s,max);
 e:=MinInt64(e,max);

 offset:=s;
 size  :=(e-s);
end;

procedure free_imgp(imgp:p_image_params);
begin
 if (imgp=nil) then Exit;
 SetLength(imgp^._selfEntries,0);
 FreeMem(imgp^.image_header);
 imgp^:=Default(t_image_params);
end;

function load_oelf(Const name:RawByteString;imgp:p_image_params):Integer;
Var
 F:THandle;
 obj_size:Int64;
 n:Int64;
 Magic:DWORD;
 elf_hdr:p_elf64_hdr;
begin
 Result:=0;

 if (name='') or (imgp=nil) then Exit(-1);

 F:=FileOpen(name,fmOpenRead);
 if (F=feInvalidHandle) then Exit(-2);

 n:=FileRead(F,Magic,SizeOf(DWORD));
 if (n<>SizeOf(DWORD)) then
 begin
  FileClose(F);
  Exit(-3);
 end;

 obj_size:=FileSeek(F,0,fsFromEnd);

 if (obj_size<=0) then
 begin
  FileClose(F);
  Exit(-4);
 end;

 case Magic of
  ELFMAG: //elf64
    begin
      elf_hdr:=AllocMem(obj_size);

      FileSeek(F,0,fsFromBeginning);
      n:=FileRead(F,elf_hdr^,obj_size);
      FileClose(F);

      if (n<>obj_size) then
      begin
       FreeMem(elf_hdr);
       elf_hdr:=nil;
       Exit(-5);
      end;

      imgp^.image_header:=elf_hdr;
      imgp^.elf_size    :=obj_size;
    end;
  else
    begin
     Exit(-5);
    end;
 end;

end;

function scan_phdr(imgp:p_image_params;phdr:p_elf64_phdr;count:Integer):Integer;
var
 i:Integer;
 text_id     :Integer;
 data_id     :Integer;
 sce_relro_id:Integer;
 vaddr:QWORD;
 memsz:QWORD;
begin
 if (imgp=nil) then Exit(-1);
 if (phdr=nil) then Exit(-1);
 if (count=0)  then Exit(-1);

 imgp^.min_addr:=High(Int64);
 imgp^.max_addr:=0;

 text_id     :=-1;
 data_id     :=-1;
 sce_relro_id:=-1;
 imgp^.dyn_id:=-1;

 if (count<>0) then
 For i:=0 to count-1 do
 begin

  case phdr[i].p_type of
   PT_LOAD,
   PT_SCE_RELRO:
     begin
      vaddr:=phdr[i].p_vaddr;

      if ((phdr[i].p_align and PAGE_MASK)<>0) or
         ((vaddr and PAGE_MASK)<>0) or
         ((phdr[i].p_offset and PAGE_MASK)<>0) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' is not page aligned');
       Exit(-2);
      end;

      memsz:=phdr[i].p_memsz;

      if (memsz<=phdr[i].p_filesz) and (phdr[i].p_filesz<>memsz) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if (memsz > $7fffffff) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_offset');
       Exit(-2);
      end;

      imgp^.min_addr:=MinInt64(imgp^.min_addr,vaddr);

      vaddr:=(vaddr+memsz+$3fff) and QWORD($ffffffffffffc000);

      imgp^.max_addr:=MaxInt64(imgp^.max_addr,vaddr);

      if (phdr[i].p_type=PT_SCE_RELRO) then
      begin
       sce_relro_id:=i;
      end else
      if ((phdr[i].p_flags and PF_X)=0) then
      begin
       if (data_id=-1) then data_id:=i;
      end else
      begin
       text_id:=i;
      end;
     end;

   PT_DYNAMIC:
     begin
      imgp^.dyn_exist :=1;
      imgp^.dyn_id    :=i;
      imgp^.dyn_vaddr :=Pointer(phdr[i].p_vaddr);
      imgp^.dyn_offset:=phdr[i].p_offset;
      imgp^.dyn_filesz:=phdr[i].p_filesz;

      memsz:=phdr[i].p_memsz;

      if (memsz<=phdr[i].p_filesz) and (phdr[i].p_filesz<>memsz) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if (memsz > $7fffffff) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_offset');
       Exit(-2);
      end;
     end;

   PT_TLS:
     begin
      imgp^.tls_size     :=phdr[i].p_memsz;
      imgp^.tls_align    :=phdr[i].p_align;
      imgp^.tls_init_size:=phdr[i].p_filesz;
      imgp^.tls_init_addr:=Pointer(phdr[i].p_vaddr);

      memsz:=phdr[i].p_memsz;

      if (memsz<=phdr[i].p_filesz) and (phdr[i].p_filesz<>memsz) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if (memsz > $7fffffff) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_offset');
       Exit(-2);
      end;

      if (phdr[i].p_align > 32) then
      begin
       Writeln(StdErr,'scan_phdr:',' alignment of segment #',i,' it must be less than 32.');
       Exit(-2);
      end;
     end;

   PT_SCE_DYNLIBDATA:
     begin
      imgp^.sce_dynlib_data_id  :=i;
      imgp^.sce_dynlib_data_addr:=phdr[i].p_offset;
      imgp^.sce_dynlib_data_size:=phdr[i].p_filesz;

      if (phdr[i].p_memsz<>0) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
       Exit(-2);
      end;

      if (phdr[i].p_filesz > $7fffffff) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_filesz');
       Exit(-2);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_offset');
       Exit(-2);
      end;
     end;

   PT_SCE_PROCPARAM:
     begin
      imgp^.proc_param_addr:=Pointer(phdr[i].p_vaddr);
      imgp^.proc_param_size:=phdr[i].p_filesz;
     end;

   PT_SCE_MODULE_PARAM:
     begin
      imgp^.module_param_addr:=Pointer(phdr[i].p_vaddr);
      imgp^.module_param_size:=phdr[i].p_filesz;
     end;

   PT_GNU_EH_FRAME:
    begin
     imgp^.eh_frame_hdr_addr:=Pointer(phdr[i].p_vaddr);
     imgp^.eh_frame_hdr_size:=phdr[i].p_memsz;

     memsz:=phdr[i].p_memsz;

     if (memsz<=phdr[i].p_filesz) and (phdr[i].p_filesz<>memsz) then
     begin
      Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
      Exit(-2);
     end;

     if (memsz > $7fffffff) then
     begin
      Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
      Exit(-2);
     end;

     if ((phdr[i].p_offset shr $20)<>0) then
     begin
      Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_offset');
      Exit(-2);
     end;
    end;

   PT_SCE_COMMENT:
    begin
     imgp^.sce_comment_id    :=i;
     imgp^.sce_comment_offset:=phdr[i].p_offset;
     imgp^.sce_comment_filesz:=phdr[i].p_filesz;

     if (phdr[i].p_memsz<>0) then
     begin
      Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_memsz');
      Exit(-2);
     end;

     if (phdr[i].p_filesz > $7fffffff) then
     begin
      Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_filesz');
      Exit(-2);
     end;

     if ((phdr[i].p_offset shr $20)<>0) then
     begin
      Writeln(StdErr,'scan_phdr:',' segment #',i,' wrong p_offset');
      Exit(-2);
     end;
    end;

  end;

 end;

 if (imgp^.min_addr=High(Int64)) then
 begin
  Exit(-2);
 end;

 if (imgp^.max_addr=0) then
 begin
  Exit(-2);
 end;

 if (imgp^.dyn_exist<>0) then
 begin
  if (imgp^.sce_dynlib_data_size=0) then
  begin
   Writeln(StdErr,'scan_phdr:',' sce_dynlib_data_size is zero');
   Exit(-2);
  end;

  if (imgp^.dyn_filesz=0) then
  begin
   Writeln(StdErr,'scan_phdr:',' dyn_filesz is zero');
   Exit(-2);
  end;
 end;

 if (sce_relro_id<>-1) then
 begin
  vaddr:=phdr[sce_relro_id].p_vaddr;

  if (vaddr=0) then
  begin
   Exit(-2);
  end;

  memsz:=phdr[sce_relro_id].p_memsz;

  if (memsz=0) then
  begin
   Exit(-2);
  end;

  if (((phdr[text_id].p_vaddr+phdr[text_id].p_memsz+$1fffff) and QWORD($ffffffffffe00000))<>vaddr) and
     (((phdr[text_id].p_vaddr+phdr[text_id].p_memsz+$003fff) and QWORD($ffffffffffffc000))<>vaddr) then
  begin
   Exit(-2);
  end;

  if (((vaddr+memsz+$1fffff) and QWORD($ffffffffffe00000))<>phdr[data_id].p_vaddr) and
     (((vaddr+memsz+$003fff) and QWORD($ffffffffffffc000))<>phdr[data_id].p_vaddr) then
  begin
   Exit(-2);
  end;
 end;

 Result:=0;
end;

function exec_oelf_imgact(imgp:p_image_params):Integer;
var
 hdr :p_elf64_hdr;
 phdr:p_elf64_phdr;
begin
 Result:=0;

 if (imgp=nil) then Exit(-1);

 hdr:=imgp^.image_header;

 //hdr^.e_shoff    :=0;
 //hdr^.e_shentsize:=0;
 //hdr^.e_shnum    :=0;
 //hdr^.e_shstrndx :=0;

 if (hdr=nil) then Exit(-1);

 Case hdr^.e_type of
  ET_SCE_EXEC       ,
  ET_SCE_REPLAY_EXEC,
  ET_SCE_DYNEXEC    :
  else
   begin
    Writeln(StdErr,'exec_oelf_imgact:',' unspported e_type:0x',HexStr(hdr^.e_type,4));
    Exit(-2);
   end;
 end;

 if (hdr^.e_machine<>EM_X86_64) then
 begin
  Writeln(StdErr,'exec_oelf_imgact:',' unspported e_machine:',hdr^.e_machine);
  Exit(-2);
 end;

 phdr:=get_elf_phdr(hdr);

 Result:=scan_phdr(imgp,phdr,hdr^.e_phnum);
 if (Result<>0) then
 begin
  Writeln(StdErr,'exec_oelf_imgact:','found illegal segment header');
  Exit;
 end;

 if (imgp^.dyn_exist=0) and (hdr^.e_type=ET_SCE_DYNEXEC) then
 begin
  Writeln(StdErr,'exec_oelf_imgact:','illegal ELF file image');
  Exit(-2);
 end;

end;

procedure sha256(P:Pointer;S:longword;F:PByte);
Var
 D:TDCP_sha256;
begin
 D.Init;
 D.Update(P^,S);
 D.Final(F^);
end;

function setProperty(prop,bit,mask,val:QWORD):QWORD; inline;
begin
 Result:=prop or ((val and mask) shl bit);
end;

procedure append(var A:a_self_segment;S:SelfEntryInfo);
var
 i:Integer;
begin
 i:=Length(A);
 SetLength(A,i+1);
 A[i]:=S;
end;

function ilog2(i:QWORD):QWORD; inline;
begin
 Result:=BsrQWORD(i or 1);
end;

function make(size:sizeint):AByte; inline;
begin
 Result:=nil;
 SetLength(Result,size);
 FillChar(Result[0],size,0);
end;

function createSelfEntries(imgp:p_image_params;
                           prog:p_elf64_phdr;
                           prog_size:Integer):Integer;
var
 i,entryIndex:Integer;
 metaEntryProperties:QWORD;
 dataEntryProperties:QWORD;
 seg:SelfEntryInfo;
begin
 entryIndex:=0;

 for i:=0 to prog_size-1 do
 begin

  case prog^.p_type of
   PT_LOAD:;
   PT_SCE_RELRO:;
   PT_SCE_DYNLIBDATA:;
   PT_SCE_COMMENT:;
   else
    begin
     Inc(prog);
     continue;
    end;
  end;

  metaEntryProperties:=0;

  metaEntryProperties:=metaEntryProperties or SELF_PROPS_SIGNED;
  metaEntryProperties:=metaEntryProperties or SELF_PROPS_HAS_DIGESTS;
  metaEntryProperties:=setProperty(metaEntryProperties, SELF_PROPS_SEGMENT_INDEX_OFFSET, SELF_PROPS_SEGMENT_INDEX_MASK, entryIndex+1);

  seg:=Default(SelfEntryInfo);
  seg.info.flags:=metaEntryProperties;

  append(imgp^._selfEntries,seg);

  dataEntryProperties:=0;

  dataEntryProperties:=dataEntryProperties or SELF_PROPS_SIGNED;
  dataEntryProperties:=dataEntryProperties or SELF_PROPS_BLOCKED;
  dataEntryProperties:=setProperty(dataEntryProperties, SELF_PROPS_BLOCK_SIZE_OFFSET   , SELF_PROPS_BLOCK_SIZE_MASK   , ilog2(BLOCK_SIZE)-12);
  dataEntryProperties:=setProperty(dataEntryProperties, SELF_PROPS_SEGMENT_INDEX_OFFSET, SELF_PROPS_SEGMENT_INDEX_MASK, i);

  seg:=Default(SelfEntryInfo);
  seg.info.flags:=dataEntryProperties;

  append(imgp^._selfEntries,seg);

  entryIndex += 2;
  Inc(prog);
 end;

 Result:=length(imgp^._selfEntries) * SELF_META_DATA_BLOCK_SIZE;
end;

function writeNullBytes(imgp:p_image_params;
                        size:Integer):Integer;
var
 buf:Pointer;
 w:sizeint;
begin
 if (size=0) then Exit(0);

 buf:=AllocMem(size);

 w:=FileWrite(imgp^.outputFself,buf^,size);
 if (w<>size) then
 begin
  Writeln(StdErr,'writeNullBytes:',' FileWrite:',w);
 end;

 FreeMem(buf);
 Result:=size;
end;

function writeNullPadding(imgp:p_image_params;
                          size,_align:Integer):Integer;
var
 buf:Pointer;
 w:sizeint;
begin
 size:=(-size) and (_align-1);
 if (size=0) then Exit(0);

 buf:=AllocMem(size);

 w:=FileWrite(imgp^.outputFself,buf^,size);
 if (w<>size) then
 begin
  Writeln(StdErr,'writeNullBytes:',' FileWrite:',w);
 end;

 FreeMem(buf);
 Result:=size;
end;

function writeSelfHeader(imgp:p_image_params;
                         version:byte;
                         mode:byte;
                         endian:byte;
                         attr:byte;
                         headerSize:word;
                         fileSize:QWORD;
                         flags:word):Integer;
var
 selfHeader:t_self_header;
 w:sizeint;
begin
 selfHeader.Magic       :=SELF_MAGIC;
 selfHeader.Version     :=version;
 selfHeader.Mode        :=mode;
 selfHeader.Endian      :=endian;
 selfHeader.Attr        :=attr;
 selfHeader.Content_Type:=1;
 selfHeader.Program_Type:=1;
 selfHeader.Padding     :=0;
 selfHeader.Header_Size :=headerSize;
 selfHeader.Meta_size   :=(length(imgp^._selfEntries) * SELF_ENTRY_SIZE) + SELF_META_FOOTER_SIZE + SELF_SIGNATURE_SIZE;
 selfHeader.File_size   :=fileSize;
 selfHeader.Num_Segments:=length(imgp^._selfEntries);
 selfHeader.Flags       :=flags;
 selfHeader.reserved    :=0;

 w:=FileWrite(imgp^.outputFself,selfHeader,SizeOf(selfHeader));
 if (w<>SizeOf(selfHeader)) then
 begin
  Writeln(StdErr,'writeSelfHeader:',' FileWrite:',w);
 end;

 Result:=SizeOf(selfHeader);
end;

function writeSelfEntries(imgp:p_image_params):Integer;
var
 i:Integer;
 w:sizeint;
begin
 Result:=0;

 for i:=0 to high(imgp^._selfEntries) do
 begin

  w:=FileWrite(imgp^.outputFself,imgp^._selfEntries[i].info,SizeOf(t_self_segment));
  if (w<>SizeOf(t_self_segment)) then
  begin
   Writeln(StdErr,'writeSelfEntries:',' FileWrite:',w);
  end;

  Result:=Result+SizeOf(t_self_segment);
 end;

end;

function writeELFHeaders(imgp:p_image_params):Integer;
var
 hdr:elf64_hdr;
 prog:p_elf64_phdr;
 prog_size:Integer;
 w,s:sizeint;
begin
 Result:=0;

 prog:=get_elf_phdr(imgp^.image_header);
 prog_size:=imgp^.image_header^.e_phnum;

 hdr:=Default(elf64_hdr);
 hdr:=imgp^.image_header^;

 w:=FileWrite(imgp^.outputFself,hdr,SizeOf(elf64_hdr));
 if (w<>SizeOf(elf64_hdr)) then
 begin
  Writeln(StdErr,'writeELFHeaders:',' FileWrite:',w);
 end;

 Result:=SizeOf(elf64_hdr);

 s:=Sizeof(elf64_phdr)*prog_size;

 w:=FileWrite(imgp^.outputFself,prog^,s);
 if (w<>s) then
 begin
  Writeln(StdErr,'writeELFHeaders:',' FileWrite:',w);
 end;

 Result:=Result+s;
end;

function writeExtendedInfo(imgp:p_image_params;
                           pType:RawByteString;
                           paid:uint64;
                           appVersion:uint64;
                           fwVersion:uint64;
                           var digest:Tsha256Digest):Integer;
var
 extendedHeader:t_self_authinfo;
 programType:Integer;
 w:Integer;
begin
 programType := SELF_PT_FAKE;

 case lowercase(Trim(pType)) of
  'npdrm_exec'   :programType:=SELF_PT_NPDRM_EXEC;
  'npdrm_dynlib' :programType:=SELF_PT_NPDRM_DYNLIB;
  'system_exec'  :programType:=SELF_PT_SYSTEM_EXEC;
  'system_dynlib':programType:=SELF_PT_SYSTEM_DYNLIB;
  'host_kernel'  :programType:=SELF_PT_HOST_KERNEL;
  'secure_module':programType:=SELF_PT_SECURE_MODULE;
  'secure_kernel':programType:=SELF_PT_SECURE_KERNEL;
  else;
 end;

 extendedHeader.AuthorityID    :=paid;
 extendedHeader.Program_Type   :=programType;
 extendedHeader.Program_Version:=appVersion;
 extendedHeader.System_Version :=fwVersion;
 extendedHeader.Digest_SHA_256 :=digest;

 w:=FileWrite(imgp^.outputFself,extendedHeader,sizeof(t_self_authinfo));
 if (w<>sizeof(t_self_authinfo)) then
 begin
  Writeln(StdErr,'writeExtendedInfo:',' FileWrite:',w);
 end;

 Result:=sizeof(t_self_authinfo);
end;

function writeNpdrmControlBlock(imgp:p_image_params):Integer;
var
 controlBlock:t_self_npdrm_control_block;
 w:Integer;
begin
 controlBlock:=Default(t_self_npdrm_control_block);
 controlBlock.pType:=SELF_CONTROL_BLOCK_TYPE_NPDRM;

 w:=FileWrite(imgp^.outputFself,controlBlock,sizeof(t_self_npdrm_control_block));
 if (w<>sizeof(t_self_npdrm_control_block)) then
 begin
  Writeln(StdErr,'writeNpdrmControlBlock:',' FileWrite:',w);
 end;

 Result:=sizeof(t_self_npdrm_control_block);
end;

function writeMetaBlocks(imgp:p_image_params):Integer;
var
 buf:Pointer;
 w,size:sizeint;
begin
 size:=SELF_META_BLOCK_SIZE*length(imgp^._selfEntries);
 buf:=AllocMem(size);

 w:=FileWrite(imgp^.outputFself,buf^,size);
 if (w<>size) then
 begin
  Writeln(StdErr,'writeMetaBlocks:',' FileWrite:',w);
 end;

 FreeMem(buf);
 Result:=size;
end;

function writeMetaFooter(imgp:p_image_params;val:DWORD):Integer;
var
 buf:Pointer;
 w,size:sizeint;
begin
 size:=SELF_META_FOOTER_SIZE;
 buf:=AllocMem(size);

 PDWORD(buf+$30)^:=val;

 w:=FileWrite(imgp^.outputFself,buf^,size);
 if (w<>size) then
 begin
  Writeln(StdErr,'writeMetaFooter:',' FileWrite:',w);
 end;

 FreeMem(buf);
 Result:=size;
end;

function writeSignature(imgp:p_image_params;var signature:Tsignature):Integer;
var
 w:sizeint;
begin

 w:=FileWrite(imgp^.outputFself,signature,sizeof(Tsignature));
 if (w<>sizeof(Tsignature)) then
 begin
  Writeln(StdErr,'writeSignature:',' FileWrite:',w);
 end;

 Result:=sizeof(Tsignature);
end;

function writeSegments(imgp:p_image_params):Integer;
var
 i:Integer;
 w,s:sizeint;
begin
 Result:=0;

 for i:=0 to high(imgp^._selfEntries) do
 begin

  //Writeln('offset:',imgp^._selfEntries[i].info.offset:6,
  //        ' len:',Length(imgp^._selfEntries[i].data):6,
  //        ' end:',imgp^._selfEntries[i].info.offset+Length(imgp^._selfEntries[i].data):6);

  s:=imgp^._selfEntries[i].info.offset;
  w:=FileSeek(imgp^.outputFself,s,fsFromBeginning);
  if (w<>s) then
  begin
   Writeln(StdErr,'writeSegments:',' FileSeek:',w);
  end;

  s:=Length(imgp^._selfEntries[i].data);

  w:=FileWrite(imgp^.outputFself,imgp^._selfEntries[i].data[0],s);
  if (w<>s) then
  begin
   Writeln(StdErr,'writeSegments:',' FileWrite:',w);
  end;

  Result:=Result+s;
 end;

end;

function CreateFSELF(imgp:p_image_params;
                     outputPath:RawByteString;
                     paid:int64;
                     pType:RawByteString;
                     appVersion:int64;
                     fwVersion:int64;
                     authInfo:RawByteString):Integer;

var
 sha256Digest:Tsha256Digest;
 signature:Tsignature;
 headerSize:QWORD;
 offset:QWORD;
 fileSize:QWORD;
 finalFileSize:QWORD;
 prog:p_elf64_phdr;
 prog_size:Integer;
 i,entryIndex:Integer;
 numBlocks:Integer;
 signedBlockCount:Integer;
 flags:Integer;
 metaData:AByte;
 segmentData:AByte;
 base:Pointer;
begin

 imgp^.outputFself:=FileCreate(outputPath);

 if (imgp^.outputFself=THandle(-1)) then
 begin
  Writeln(StdErr,'CreateFSELF:',' FileCreate error');
  Exit;
 end;

 sha256Digest:=Default(Tsha256Digest);
 sha256(imgp^.image_header,imgp^.elf_size,@sha256Digest);

 signature:=Default(Tsignature);

 {
 if authInfo != "" {
  signature = createSignature(authInfo, paid)
 }
 }

 prog:=get_elf_phdr(imgp^.image_header);
 prog_size:=imgp^.image_header^.e_phnum;

 headerSize := SELF_HEADER_SIZE;
 headerSize += createSelfEntries(imgp,prog,prog_size);
 headerSize += SELF_ELF_HEADER_SIZE;
 headerSize += prog_size * SELF_ELF_PROGHEADER_SIZE;

 headerSize := Align(headerSize, $10);

 headerSize += SELF_EXTENDED_HEADER_SIZE;
 headerSize += SELF_NPDRM_BLOCK_SIZE;

 //Writeln('headerSize=',headerSize);

 entryIndex := 0;
 offset := headerSize + (length(imgp^._selfEntries)*SELF_ENTRY_SIZE)+SELF_META_FOOTER_SIZE+SELF_SIGNATURE_SIZE;

 //Writeln('MetaSize=',offset);

 for i:=0 to prog_size-1 do
 begin

  case prog^.p_type of
   PT_LOAD:;
   PT_SCE_RELRO:;
   PT_SCE_DYNLIBDATA:;
   PT_SCE_COMMENT:;
   else
    begin
     Inc(prog);
     continue;
    end;
  end;

  numBlocks := align(prog^.p_filesz, BLOCK_SIZE) div BLOCK_SIZE;

  metaData:=make(SELF_META_DATA_BLOCK_SIZE*numBlocks);

  imgp^._selfEntries[entryIndex].Data        := metaData;
  imgp^._selfEntries[entryIndex].info.offset := offset;
  imgp^._selfEntries[entryIndex].info.filesz := length(metaData);
  imgp^._selfEntries[entryIndex].info.memsz  := length(metaData);

  offset += imgp^._selfEntries[entryIndex].info.filesz;
  offset := align(offset, $10);

  segmentData := make(prog^.p_filesz);

  base:=Pointer(imgp^.image_header)+prog^.p_offset;
  Move(base^,segmentData[0],length(segmentData));

  imgp^._selfEntries[entryIndex+1].Data        := segmentData;
  imgp^._selfEntries[entryIndex+1].info.offset := offset;
  imgp^._selfEntries[entryIndex+1].info.filesz := prog^.p_filesz;
  imgp^._selfEntries[entryIndex+1].info.memsz  := prog^.p_filesz;

  offset += imgp^._selfEntries[entryIndex+1].info.filesz;
  offset := align(offset, $10);

  entryIndex += 2;
  Inc(prog);
 end;

 fileSize := offset;

 //Writeln('fileSize=',fileSize);

 signedBlockCount := $2;
 flags := $2 or ((signedBlockCount and $7) shl 4);

 finalFileSize := 0;

 finalFileSize += writeSelfHeader(imgp,
  0,
  SELF_MODE_SPECIFICUSER,
  SELF_DATA_LSB,
  $12,
  headerSize,
  fileSize,
  flags
 );

 finalFileSize += writeNullPadding(imgp, finalFileSize, $10);
 finalFileSize += writeSelfEntries(imgp);
 finalFileSize += writeELFHeaders(imgp);
 finalFileSize += writeNullPadding(imgp, finalFileSize, $10);
 finalFileSize += writeExtendedInfo(imgp, pType, paid, appVersion, fwVersion, sha256Digest);
 finalFileSize += writeNpdrmControlBlock(imgp);

 //Writeln('headerSize2=',finalFileSize);

 finalFileSize += writeMetaBlocks(imgp);
 finalFileSize += writeMetaFooter(imgp, $10000);
 finalFileSize += writeSignature(imgp, signature);

 //Writeln('MetaSize2=',finalFileSize);

 finalFileSize += writeSegments(imgp);

 finalFileSize:=FileSeek(imgp^.outputFself,0,fsFromEnd);

 if (finalFileSize<fileSize) then
 begin
  writeNullBytes(imgp,fileSize-finalFileSize);
 end;

 //Writeln('finalFileSize=',finalFileSize);

 FileClose(imgp^.outputFself);

 Result:=0;
end;

function get_sdk_version_str(version:QWORD):RawByteString;
begin
 Result:=HexStr((version shr 24),2)+'.'+HexStr(((version shr 12) and $fff),3)+'.'+HexStr((version and $fff),3);
end;

function HexToDword(S:RawByteString;_def:DWORD):DWORD;
begin
 Result:=_def;

 if (copy(S,1,2)='0x') then
 begin
  Delete(S,1,2);
  S:='$'+S;
 end;

 TryStrToUInt(S,Result);
end;

function HexToQword(S:RawByteString;_def:QWORD):QWORD;
begin
 Result:=_def;

 if (copy(S,1,2)='0x') then
 begin
  Delete(S,1,2);
  S:='$'+S;
 end;

 TryStrToUInt64(S,Result);
end;

var
 InputFileName :RawByteString='';                //input Orbis OELF filepath
 OutputFileName:RawByteString='';                //output Orbis FSELF filepath
 ptype         :RawByteString='';                //program type
 authInfo      :RawByteString='';                //authentication info
 paid          :QWORD        =$3800000000000011; //program authentication ID
 appVer        :QWORD        =$10000510000;      //application version
 fwVer         :QWORD        =$10000510000;      //firmware version

procedure parse_param;
var
 i:Integer;
 S:RawByteString;
 n:Integer;
begin
 n:=-1;
 For i:=1 to ParamCount do
 begin
  S:=Trim(ParamStr(i));

  case LowerCase(S) of
          '-i':n:=0;
          '-o':n:=1;
      '-ptype':n:=2;
   '-authinfo':n:=3;
       '-paid':n:=4;
     '-appver':n:=5;
      '-fwver':n:=6;
   else
     if (n<>-1) then
     begin
      Case n of
       0:InputFileName :=S;
       1:OutputFileName:=S;
       2:ptype         :=LowerCase(S);
       3:authInfo      :=S;
       4:paid          :=HexToQword(S,paid);
       5:appVer        :=HexToQword(S,appVer);
       6:fwVer         :=HexToQword(S,fwVer);
      end;
      n:=-1;
     end else
     begin
      InputFileName:=S;
     end;
  end;

 end;
end;

var
 r:Integer;
 img:t_image_params;

begin
 parse_param;

 if (ParamCount<=1) or (InputFileName='') then
 begin

  Writeln('Usage: oelf2fself <option(s)>');
  Writeln(' Convert Orbis ELF file to Fake SELF file');
  Writeln(' Options are:');
  Writeln('  -i        input  Orbis OELF filepath');
  Writeln('  -o        output Orbis FSELF filepath');
  Writeln('  -sdkver   SDK version integer');
  Writeln('  -ptype    program type');
  Writeln('  -authinfo authentication info');
  Writeln('  -paid     program authentication ID');
  Writeln('  -appver   application version');
  Writeln('  -fwver    firmware version');

  Exit;
 end;

 if (OutputFileName='') then
 begin
  OutputFileName:=ChangeFileExt(InputFileName,'.bin');
 end;

 img:=Default(t_image_params);
 r:=load_oelf(InputFileName,@img);

 if (r<>0) then
 begin
  Writeln('Error(',r,') load file:',InputFileName);
 end else
 begin
  r:=exec_oelf_imgact(@img);

  if (r<>0) then
  begin
   Writeln('Error(',r,') parse file:',InputFileName);
  end else
  begin

   CreateFSELF(@img,
               OutputFileName,
               paid,
               ptype,
               appVer,
               fwVer,
               authInfo);

  end;




 end;

 free_imgp(@img);

 //readln;
end.

