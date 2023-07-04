program readself;

uses
 sysutils,
 elf64,
 elf_nid_utils,
 ps4libdoc;

type
 p_elf_obj=^elf_obj;
 elf_obj=packed record
  is_encrypted:Integer;
  self:record
   hdr     :p_self_header;
   segs    :p_self_segment;
   elf_hdr :p_elf64_hdr;
   MinSeg  :Int64;
   MaxSeg  :Int64;
  end;
  elf:record
   hdr :p_elf64_hdr;
   size:Int64;
  end;
  size:Int64;
  //
  min_offset:Int64;
  max_offset:Int64;
  //
  dyn_addr:p_elf64_dyn;
  dyn_size:Int64;
  //
  tls_addr:Pointer;
  tls_size:Int64;
  //
  sce_dynlib_addr:Pointer;
  sce_dynlib_size:Int64;
  //
  proc_param_addr:PSceProcParam;
  proc_param_size:Int64;
  //
  module_param_addr:PsceModuleParam;
  module_param_size:Int64;
  //
  eh_frame_hdr_addr:Pointer;
  eh_frame_hdr_size:Int64;
  //
  dt_symtab_addr:p_elf64_sym;
  dt_symtab_size:Int64;
  //
  dt_strtab_addr:PAnsiChar;
  dt_strtab_size:Int64;
  //
  dt_rela_addr:p_elf64_rela;
  dt_rela_size:Int64;
  //
  dt_rela_plt_addr:p_elf64_rela;
  dt_rela_plt_size:Int64;
 end;

function maxInt64(a,b:Int64):Int64; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function minInt64(a,b:Int64):Int64; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

procedure free_elf_obj(obj:p_elf_obj);
begin
 if (obj=nil) then Exit;
 FreeMem(obj^.self.hdr);
 FreeMem(obj^.elf.hdr);
 obj^:=Default(elf_obj);
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

function load_self(Const name:RawByteString;obj:p_elf_obj):Integer;
Var
 F:THandle;
 n,s:Int64;
 Magic:DWORD;
 i,count:Integer;
 is_encrypted:Integer;
 self_hdr :p_self_header;
 self_segs:p_self_segment;
 elf_hdr  :p_elf64_hdr;
 elf_phdr :p_elf64_phdr;
 MinSeg   :Int64;
 MaxSeg   :Int64;
 src_ofs  :Int64;
 dst_ofs  :Int64;
 mem_size :Int64;
begin
 Result:=0;

 if (name='') or (obj=nil) then Exit(-1);

 F:=FileOpen(name,fmOpenRead);
 if (F=feInvalidHandle) then Exit(-2);

 n:=FileRead(F,Magic,SizeOf(DWORD));
 if (n<>SizeOf(DWORD)) then
 begin
  FileClose(F);
  Exit(-3);
 end;

 case Magic of
  ELFMAG: //elf64
    begin
      obj^.size:=FileSeek(F,0,fsFromEnd);

      if (obj^.size<=0) then
      begin
       FileClose(F);
       Exit(-4);
      end;

      obj^.elf.hdr :=GetMem(obj^.size);
      obj^.elf.size:=obj^.size;

      FileSeek(F,0,fsFromBeginning);
      n:=FileRead(F,obj^.elf.hdr^,obj^.size);
      FileClose(F);

      if (n<>obj^.size) then
      begin
       FreeMem(obj^.elf.hdr);
       obj^.elf.hdr:=nil;
       Exit(-5);
      end;
    end;
  SELF_MAGIC:
    begin
      obj^.size:=FileSeek(F,0,fsFromEnd);

      if (obj^.size<=0) then
      begin
       FileClose(F);
       Exit(-4);
      end;

      self_hdr:=GetMem(obj^.size);
      obj^.self.hdr:=self_hdr;

      FileSeek(F,0,fsFromBeginning);
      n:=FileRead(F,self_hdr^,obj^.size);
      FileClose(F);

      if (n<>obj^.size) then
      begin
       FreeMem(obj^.self.hdr);
       obj^.self.hdr:=nil;
       Exit(-5);
      end;

      count:=self_hdr^.Num_Segments;

      self_segs:=Pointer(self_hdr+1);
      obj^.self.segs:=self_segs;

      is_encrypted:=0;
      if (count<>0) then
      For i:=0 to count-1 do
       if ((self_segs[i].flags and (SELF_PROPS_ENCRYPTED or SELF_PROPS_COMPRESSED))<>0) then
       begin
        is_encrypted:=1;
        Break;
       end;

      obj^.is_encrypted:=is_encrypted;

      elf_hdr:=Pointer(self_segs)+(count*SizeOf(t_self_segment));
      obj^.self.elf_hdr:=elf_hdr;

      elf_phdr:=get_elf_phdr(elf_hdr);

      MinSeg:=High(Int64);
      MaxSeg:=0;

      count:=self_hdr^.Num_Segments;

      if (count<>0) then
      For i:=0 to count-1 do
       if ((self_segs[i].flags and SELF_PROPS_BLOCKED)<>0) then
       begin
        s:=SELF_SEGMENT_INDEX(self_segs[i].flags);
        s:=elf_phdr[s].p_offset;
        MinSeg:=MinInt64(s,MinSeg);
        s:=s+minInt64(self_segs[i].filesz,self_segs[i].filesz);
        MaxSeg:=MaxInt64(s,MaxSeg);
       end;

      obj^.self.MinSeg:=MinSeg;
      obj^.self.MaxSeg:=MaxSeg;

      if (is_encrypted=0) then //load elf
      begin
       obj^.elf.hdr :=AllocMem(MaxSeg);
       obj^.elf.size:=MaxSeg;

       //elf_hdr part
       n:=ptruint(elf_hdr)-ptruint(self_hdr);        //offset to hdr
       s:=self_hdr^.Header_Size+self_hdr^.Meta_size; //offset to end
       s:=MinInt64(obj^.size,s);                     //min size
       s:=MaxInt64(s,n)-n;                           //get size

       //first page
       Move(elf_hdr^,obj^.elf.hdr^,s);

       count:=self_hdr^.Num_Segments;

       if (count<>0) then
       For i:=0 to count-1 do
        if ((self_segs[i].flags and SELF_PROPS_BLOCKED)<>0) then
        begin
         s:=SELF_SEGMENT_INDEX(self_segs[i].flags);

         mem_size:=minInt64(self_segs[i].filesz,self_segs[i].memsz);

         src_ofs:=self_segs[i].offset;  //start offset
         dst_ofs:=elf_phdr[s].p_offset; //start offset

         fixup_offset_size(src_ofs,mem_size,obj^.size);
         fixup_offset_size(dst_ofs,mem_size,MaxSeg);

         Move( (Pointer(self_hdr)    +src_ofs)^, //src
               (Pointer(obj^.elf.hdr)+dst_ofs)^, //dst
               mem_size);                        //size
        end;
      end;

    end;
  else
    begin
     FileClose(F);
     Exit(-1);
    end;
 end;

end;

procedure print_file_size(obj:p_elf_obj);
begin
 Writeln('FILE Size:',obj^.size);
 Writeln();
end;

procedure print_self_header(obj:p_elf_obj);
var
 s,e:ptruint;
begin
 if (obj^.self.hdr<>nil) then
 begin
  s:=0;
  e:=obj^.self.hdr^.Header_Size+obj^.self.hdr^.Meta_size;

  Writeln('SELF Header:0x',HexStr(s,8),'..0x',HexStr(e,8),':',e);

  Writeln(' Magic       :0x',HexStr(obj^.self.hdr^.Magic,8));
  Writeln(' Version     :',obj^.self.hdr^.Version     );
  Writeln(' Mode        :',obj^.self.hdr^.Mode        );
  Writeln(' Endian      :',obj^.self.hdr^.Endian      );
  Writeln(' Attr        :',obj^.self.hdr^.Attr        );
  Writeln(' Content_Type:',obj^.self.hdr^.Content_Type);
  Writeln(' Program_Type:0x',HexStr(obj^.self.hdr^.Program_Type,2));
  Writeln(' Header_Size :',obj^.self.hdr^.Header_Size );
  Writeln(' Meta_size   :',obj^.self.hdr^.Meta_size   );
  Writeln(' File_size   :',obj^.self.hdr^.File_size   );
  Writeln(' Num_Segments:',obj^.self.hdr^.Num_Segments);
  Writeln(' Flags       :0x',HexStr(obj^.self.hdr^.Flags,4));
  Writeln(' MinSeg      :0x',HexStr(obj^.self.MinSeg,8));
  Writeln(' MaxSeg      :0x',HexStr(obj^.self.MaxSeg,8));
 end else
 begin
  Writeln('SELF Header:','not exist');
 end;
 Writeln();
end;

procedure print_bytes(p:PByte;size:Integer);
begin
 while (size<>0) do
 begin
  Write(HexStr(P^,2));
  Inc(P);
  Dec(size);
 end;
end;

function get_program_type_str(pt:Byte):RawByteString;
begin
 Result:='';
 Case pt  of
  SELF_PT_FAKE         :Result:='(FAKE)';
  SELF_PT_NPDRM_EXEC   :Result:='(NPDRM_EXEC)';
  SELF_PT_NPDRM_DYNLIB :Result:='(NPDRM_DYNLIB)';
  SELF_PT_SYSTEM_EXEC  :Result:='(SYSTEM_EXEC)';
  SELF_PT_SYSTEM_DYNLIB:Result:='(SYSTEM_DYNLIB)';
  SELF_PT_HOST_KERNEL  :Result:='(HOST_KERNEL)';
  SELF_PT_SECURE_MODULE:Result:='(SECURE_MODULE)';
  SELF_PT_SECURE_KERNEL:Result:='(SECURE_KERNEL)';
  else;
 end;
end;

function get_sdk_version_str(version:QWORD):RawByteString;
begin
 Result:=Format('%.*x.%.*x.%.*x',[2,(version shr 24),
                                  3,((version shr 12) and $fff),
                                  3,(version and $fff)]);
end;

procedure print_self_authinfo(obj:p_elf_obj);
label
 _not_exist;
var
 s,e,len:ptruint;
 elf_hdr:p_elf64_hdr;
 authinfo:p_self_authinfo;
begin
 if (obj^.self.hdr<>nil) then
 begin
  elf_hdr:=obj^.self.elf_hdr;
  s:=SizeOf(t_self_header);
  s:=s+(obj^.self.hdr^.Num_Segments*SizeOf(t_self_segment));
  s:=s+get_elf_phdr_offset(elf_hdr);
  s:=s+(elf_hdr^.e_phnum*SizeOf(elf64_phdr));
  s:=AlignUp(s,SELF_SEGMENT_BLOCK_ALIGNMENT);

  e:=obj^.self.hdr^.Header_Size;

  if (s>=e) then goto _not_exist;

  len:=(e-s);

  if (len<SizeOf(t_self_authinfo)) then goto _not_exist;

  Writeln('SELF authinfo:0x',HexStr(s,8),'..0x',HexStr(e,8),':',len);

  authinfo:=Pointer(Pointer(obj^.self.hdr)+s);

  Writeln(' AuthorityID    :0x',HexStr(authinfo^.AuthorityID,16));
  Writeln(' Program_Type   :0x',HexStr(authinfo^.Program_Type,2),' ',get_program_type_str(authinfo^.Program_Type));
  Writeln(' Program_Version:',get_sdk_version_str(authinfo^.Program_Version));
  Writeln(' System_Version :',get_sdk_version_str(authinfo^.System_Version ));

  Write  (' Digest_SHA_256 :');
  print_bytes(@authinfo^.Digest_SHA_256,32);
  Writeln;

 end else
 begin
  _not_exist:
  Writeln('SELF authinfo:','not exist');
 end;
 Writeln();
end;

procedure print_hex(m:PBYTE;s,e:ptruint);
begin
 Writeln(' Offset(h)|00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F');
 Write  (' ---------+-----------------------------------------------');

 for s:=s to e-1 do
 begin
   if ((s mod 16)=0) then
   begin
    Writeln();
    Write(' 0x',HexStr(s-(s mod 16),7),'|');
   end;
  Write(HexStr(m^,2),' ');
  Inc(m);
 end;

 Writeln;
end;

procedure print_self_metadata(obj:p_elf_obj);
label
 _not_exist;
var
 s,e:ptruint;
 m:PBYTE;
begin
 if (obj^.self.hdr<>nil) then
 begin
  s:=obj^.self.hdr^.Header_Size;
  e:=s+obj^.self.hdr^.Meta_size;

  if (s>=e) then goto _not_exist;

  Writeln('SELF Metadata:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  m:=Pointer(Pointer(obj^.self.hdr)+s);

  print_hex(m,s,e);
 end else
 begin
  _not_exist:
  Writeln('SELF Metadata:','not exist');
 end;
 Writeln;
end;

function get_seg_flags_str(flags:QWORD):RawByteString;
begin
 Result:='_______';

 if ((flags and SELF_PROPS_ORDERED    )<>0) then Result[1]:='O';
 if ((flags and SELF_PROPS_ENCRYPTED  )<>0) then Result[2]:='E';
 if ((flags and SELF_PROPS_SIGNED     )<>0) then Result[3]:='S';
 if ((flags and SELF_PROPS_COMPRESSED )<>0) then Result[4]:='C';
 if ((flags and SELF_PROPS_BLOCKED    )<>0) then Result[5]:='B';
 if ((flags and SELF_PROPS_HAS_DIGESTS)<>0) then Result[6]:='D';
 if ((flags and SELF_PROPS_HAS_EXTENTS)<>0) then Result[7]:='E';
end;

procedure print_self_segs(obj:p_elf_obj);
label
 _not_exist;
var
 i,count:Integer;
 seg:p_self_segment;
 s,e:ptruint;
begin
 if (obj^.self.segs<>nil) then
 begin
  count:=obj^.self.hdr^.Num_Segments;

  if (count=0) then goto _not_exist;

  s:=ptruint(obj^.self.segs)-ptruint(obj^.self.hdr);
  e:=s+(count*SizeOf(t_self_segment));

  Writeln('SELF Segments Headers:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  seg:=obj^.self.segs;

  Writeln(' Num Flags   W BlockSize  SEGID Offset     Filesz     Memsz');

  For i:=0 to count-1 do
  begin
   Write(' ',i:3,' ');
   Write(get_seg_flags_str(seg^.flags)   ,' ');
   Write(SELF_WINDOW(seg^.flags)         ,' ');
   Write('0x',HexStr(SELF_BLOCK_SIZE(seg^.flags),8),' ');
   Write(SELF_SEGMENT_INDEX(seg^.flags):5,' ');

   Write('0x',HexStr(seg^.offset,8),' ');
   Write('0x',HexStr(seg^.filesz,8),' ');
   Write('0x',HexStr(seg^.memsz ,8),' ');

   Writeln();
   Inc(seg);
  end;
 end else
 begin
  _not_exist:
  Writeln('SELF Segments Headers:','not exist');
 end;
 Writeln();
end;

function get_elf_hdr_offset(obj:p_elf_obj):Int64;
begin
 if (obj^.self.hdr<>nil) then
 begin
  Result:=obj^.self.hdr^.Num_Segments*SizeOf(t_self_segment);
 end else
 begin
  Result:=0;
 end;
end;

function get_e_class_str(e_class:Byte):RawByteString;
begin
 Result:='';
 Case e_class  of
  0:Result:='(SNONE)';
  1:Result:='(S32)';
  2:Result:='(S64)';
  else;
 end;
end;

function get_e_data_str(e_data:Byte):RawByteString;
begin
 Result:='';
 Case e_data  of
  0:Result:='(NONE)';
  1:Result:='(2LSB)';
  2:Result:='(2MSB)';
  else;
 end;
end;

function get_e_type_str(e_type:Word):RawByteString;
begin
 Result:='';
 Case e_type  of
  ET_NONE           :Result:='(NONE)';
  ET_REL            :Result:='(REL)';
  ET_EXEC           :Result:='(EXEC)';
  ET_DYN            :Result:='(DYN)';
  ET_CORE           :Result:='(CORE)';
  ET_SCE_EXEC       :Result:='(SCE_EXEC)';
  ET_SCE_REPLAY_EXEC:Result:='(SCE_REPLAY_EXEC)';
  ET_SCE_RELEXEC    :Result:='(SCE_RELEXEC)';
  ET_SCE_STUBLIB    :Result:='(SCE_STUBLIB)';
  ET_SCE_DYNEXEC    :Result:='(SCE_DYNEXEC)';
  ET_SCE_DYNAMIC    :Result:='(SCE_DYNAMIC)';
  else;
 end;
end;

function get_e_machine_str(e_machine:Word):RawByteString;
begin
 Result:='';
 Case e_machine  of
  EM_NONE  :Result:='(NONE)';
  EM_X86_64:Result:='(X86_64)';
  else;
 end;
end;

function get_osabi_name(e_machine:Word):RawByteString;
begin
 Result:='';
 Case e_machine  of
  0:Result:='(NONE)';
  3:Result:='(LINUX)';
  9:Result:='(FREEBSD)';
  else;
 end;
end;

procedure print_elf_header(obj:p_elf_obj);
var
 s,e:ptruint;
 elf_hdr:p_elf64_hdr;
begin
 if (obj^.elf.hdr<>nil) then
 begin
  elf_hdr:=obj^.elf.hdr;
  s:=get_elf_hdr_offset(obj);
  e:=s+SizeOf(elf64_hdr);

  Writeln('ELF Header:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln(' Magic      :0x',HexStr(PDWORD(@elf_hdr^.e_ident)^,8));
  Writeln(' Class:     :',elf_hdr^.e_ident[EI_CLASS],' ',get_e_class_str(elf_hdr^.e_ident[EI_CLASS]));
  Writeln(' Data:      :',elf_hdr^.e_ident[EI_DATA],' ',get_e_data_str(elf_hdr^.e_ident[EI_DATA]));
  Writeln(' Version:   :',elf_hdr^.e_ident[EI_VERSION]);
  Writeln(' OS/ABI:    :',elf_hdr^.e_ident[EI_OSABI],' ',get_osabi_name(elf_hdr^.e_ident[EI_OSABI]));
  Writeln(' ABI Version:',elf_hdr^.e_ident[EI_ABIVERSION]);

  Writeln(' e_type     :0x',HexStr(elf_hdr^.e_type,4),' ',get_e_type_str(elf_hdr^.e_type));
  Writeln(' e_machine  :',elf_hdr^.e_machine,' ',get_e_machine_str(elf_hdr^.e_machine));
  Writeln(' e_version  :',elf_hdr^.e_version  );
  Writeln(' e_entry    :0x',HexStr(elf_hdr^.e_entry,8));
  Writeln(' e_phoff    :0x',HexStr(elf_hdr^.e_phoff,8));
  Writeln(' e_shoff    :0x',HexStr(elf_hdr^.e_shoff,8));
  Writeln(' e_flags    :0x',HexStr(elf_hdr^.e_flags,8));
  Writeln(' e_ehsize   :',elf_hdr^.e_ehsize   );
  Writeln(' e_phentsize:',elf_hdr^.e_phentsize);
  Writeln(' e_phnum    :',elf_hdr^.e_phnum    );
  Writeln(' e_shentsize:',elf_hdr^.e_shentsize);
  Writeln(' e_shnum    :',elf_hdr^.e_shnum    );
  Writeln(' e_shstrndx :',elf_hdr^.e_shstrndx );

 end else
 begin
  Writeln('ELF Header:','not exist');
 end;
 Writeln();
end;

function get_pt_name(p_type:DWORD):RawByteString;
begin
 Result:='';
 Case p_type  of
  PT_NULL            :Result:='NULL            ';
  PT_LOAD            :Result:='LOAD            ';
  PT_DYNAMIC         :Result:='DYNAMIC         ';
  PT_INTERP          :Result:='INTERP          ';
  PT_NOTE            :Result:='NOTE            ';
  PT_SHLIB           :Result:='SHLIB           ';
  PT_PHDR            :Result:='PHDR            ';
  PT_TLS             :Result:='TLS             ';
  PT_SCE_RELA        :Result:='SCE_RELA        ';
  PT_SCE_DYNLIBDATA  :Result:='SCE_DYNLIBDATA  ';
  PT_SCE_PROCPARAM   :Result:='SCE_PROCPARAM   ';
  PT_SCE_MODULE_PARAM:Result:='SCE_MODULE_PARAM';
  PT_SCE_RELRO       :Result:='SCE_RELRO       ';
  PT_GNU_EH_FRAME    :Result:='GNU_EH_FRAME    ';
  PT_GNU_STACK       :Result:='GNU_STACK       ';
  PT_SCE_COMMENT     :Result:='SCE_COMMENT     ';
  PT_SCE_VERSION     :Result:='SCE_VERSION     ';
  PT_SCE_SEGSYM      :Result:='SCE_SEGSYM      ';
  else               Result:='0x'+HexStr(p_type,8)+'      ';
 end;
end;

function get_p_flags_str(flags:DWORD):RawByteString;
begin
 Result:='___';

 if ((flags and PF_X)<>0) then Result[1]:='X';
 if ((flags and PF_W)<>0) then Result[2]:='W';
 if ((flags and PF_R)<>0) then Result[3]:='R';
end;

procedure print_elf_phdr(obj:p_elf_obj);
var
 s,e:ptruint;
 elf_hdr :p_elf64_hdr;
 elf_phdr:p_elf64_phdr;
 i,count:Integer;
begin
 if (obj^.elf.hdr<>nil) then
 begin
  elf_hdr :=obj^.elf.hdr;
  elf_phdr:=get_elf_phdr(elf_hdr);
  count:=elf_hdr^.e_phnum;

  s:=get_elf_hdr_offset(obj);
  s:=s+get_elf_phdr_offset(elf_hdr);
  e:=s+count*SizeOf(elf64_phdr);

  Writeln('Program Headers:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln(' Type             Flags Offset     VirtAddr   PhysAddr   FileSize   MemSize    Align');
  if (count<>0) then
  For i:=0 to count-1 do
  begin
   Write(' ',get_pt_name(elf_phdr^.p_type),' ');
   Write(get_p_flags_str(elf_phdr^.p_flags),'   ');

   Write('0x'+HexStr(elf_phdr^.p_offset,8),' ');
   Write('0x'+HexStr(elf_phdr^.p_vaddr ,8),' ');
   Write('0x'+HexStr(elf_phdr^.p_paddr ,8),' ');
   Write('0x'+HexStr(elf_phdr^.p_filesz,8),' ');
   Write('0x'+HexStr(elf_phdr^.p_memsz ,8),' ');
   Write('0x'+HexStr(elf_phdr^.p_align ,8),' ');

   Writeln;

   Inc(elf_phdr);
  end;

 end else
 begin
  Writeln('Program Headers:','not exist');
 end;
 Writeln();
end;

procedure scan_phdr(obj:p_elf_obj);
var
 elf_hdr:p_elf64_hdr;
 entry:p_elf64_phdr;
 i,count:Integer;
begin
 if (obj^.elf.hdr=nil) then Exit;

 elf_hdr :=obj^.elf.hdr;
 entry:=get_elf_phdr(elf_hdr);
 count:=elf_hdr^.e_phnum;

 obj^.min_offset:=High(Int64);
 obj^.max_offset:=0;

 if (count<>0) then
 For i:=0 to count-1 do
 begin

  case entry^.p_type of
   PT_LOAD,
   PT_SCE_RELA:
     begin
      obj^.min_offset:=MinInt64(obj^.min_offset,entry^.p_offset);
      obj^.max_offset:=MaxInt64(obj^.max_offset,entry^.p_offset+entry^.p_filesz);
     end;

   PT_DYNAMIC:
     begin
      obj^.dyn_addr:=Pointer(elf_hdr)+entry^.p_offset;
      obj^.dyn_size:=entry^.p_filesz;
     end;

   PT_TLS:
     begin
      obj^.tls_addr:=Pointer(elf_hdr)+entry^.p_offset;
      obj^.tls_size:=entry^.p_filesz;
     end;

   PT_SCE_DYNLIBDATA:
     begin
      obj^.sce_dynlib_addr:=Pointer(elf_hdr)+entry^.p_offset;
      obj^.sce_dynlib_size:=entry^.p_filesz;
     end;

   PT_SCE_PROCPARAM:
     begin
      obj^.proc_param_addr:=Pointer(elf_hdr)+entry^.p_offset;
      obj^.proc_param_size:=entry^.p_filesz;
     end;

   PT_SCE_MODULE_PARAM:
     begin
      obj^.module_param_addr:=Pointer(elf_hdr)+entry^.p_offset;
      obj^.module_param_size:=entry^.p_filesz;
     end;

   PT_GNU_EH_FRAME:
    begin
     obj^.eh_frame_hdr_addr:=Pointer(elf_hdr)+entry^.p_offset;
     obj^.eh_frame_hdr_size:=entry^.p_filesz;
    end;

   PT_SCE_COMMENT:
    begin
     //
    end;

  end;

  Inc(entry);
 end;

end;

procedure scan_dynamic(obj:p_elf_obj);
var
 entry:p_elf64_dyn;
 i,count:Integer;
begin
 if (obj^.dyn_addr=nil) then Exit;

 entry:=obj^.dyn_addr;
 count:=obj^.dyn_size div sizeof(elf64_dyn);

 if (count<>0) then
 For i:=0 to count-1 do
 begin

  case entry^.d_tag of

   DT_SCE_SYMTAB:
    begin
     obj^.dt_symtab_addr:=obj^.sce_dynlib_addr+entry^.d_un.d_ptr;
    end;
   DT_SCE_SYMTABSZ:
    begin
     obj^.dt_symtab_size:=entry^.d_un.d_val;
    end;

   DT_SCE_STRTAB:
    begin
     obj^.dt_strtab_addr:=obj^.sce_dynlib_addr+entry^.d_un.d_ptr;
    end;

   DT_STRSZ,
   DT_SCE_STRSZ:
    begin
     obj^.dt_strtab_size:=entry^.d_un.d_ptr;
    end;

   DT_SCE_RELA:
    begin
     obj^.dt_rela_addr:=obj^.sce_dynlib_addr+entry^.d_un.d_ptr;
    end;

   DT_RELASZ,
   DT_SCE_RELASZ:
    begin
     obj^.dt_rela_size:=entry^.d_un.d_val;
    end;

   DT_SCE_JMPREL:
    begin
     obj^.dt_rela_plt_addr:=obj^.sce_dynlib_addr+entry^.d_un.d_ptr;
    end;

   DT_PLTRELSZ,
   DT_SCE_PLTRELSZ:
    begin
     obj^.dt_rela_plt_size:=entry^.d_un.d_val;
    end;

   else;
  end;

  Inc(entry);
 end;

end;

function get_dt_name(d_tag:DWORD):RawByteString;
begin
 Result:='';
 Case d_tag  of
  DT_NULL                     :Result:='NULL                    ';
  DT_NEEDED                   :Result:='NEEDED                  ';
  DT_PLTRELSZ                 :Result:='PLTRELSZ                ';
  DT_PLTGOT                   :Result:='PLTGOT                  ';
  DT_HASH                     :Result:='HASH                    ';
  DT_STRTAB                   :Result:='STRTAB                  ';
  DT_SYMTAB                   :Result:='SYMTAB                  ';
  DT_RELA                     :Result:='RELA                    ';
  DT_RELASZ                   :Result:='RELASZ                  ';
  DT_RELAENT                  :Result:='RELAENT                 ';
  DT_STRSZ                    :Result:='STRSZ                   ';
  DT_SYMENT                   :Result:='SYMENT                  ';
  DT_INIT                     :Result:='INIT                    ';
  DT_FINI                     :Result:='FINI                    ';
  DT_SONAME                   :Result:='SONAME                  ';
  DT_RPATH                    :Result:='RPATH                   ';
  DT_SYMBOLIC                 :Result:='SYMBOLIC                ';
  DT_REL                      :Result:='REL                     ';
  DT_RELSZ                    :Result:='RELSZ                   ';
  DT_RELENT                   :Result:='RELENT                  ';
  DT_PLTREL                   :Result:='PLTREL                  ';
  DT_DEBUG                    :Result:='DEBUG                   ';
  DT_TEXTREL                  :Result:='TEXTREL                 ';
  DT_JMPREL                   :Result:='JMPREL                  ';
  DT_BIND_NOW                 :Result:='BIND_NOW                ';
  DT_INIT_ARRAY               :Result:='INIT_ARRAY              ';
  DT_FINI_ARRAY               :Result:='FINI_ARRAY              ';
  DT_INIT_ARRAYSZ             :Result:='INIT_ARRAYSZ            ';
  DT_FINI_ARRAYSZ             :Result:='FINI_ARRAYSZ            ';
  DT_RUNPATH                  :Result:='RUNPATH                 ';
  DT_FLAGS                    :Result:='FLAGS                   ';
  DT_ENCODING                 :Result:='ENCODING                ';
  DT_PREINIT_ARRAY            :Result:='PREINIT_ARRAY           ';
  DT_PREINIT_ARRAYSZ          :Result:='PREINIT_ARRAYSZ         ';
  DT_SCE_IDTABENTSZ           :Result:='SCE_IDTABENTSZ          ';
  DT_SCE_FINGERPRINT          :Result:='SCE_FINGERPRINT         ';
  DT_SCE_ORIGINAL_FILENAME    :Result:='SCE_ORIGINAL_FILENAME   ';
  DT_SCE_MODULE_INFO          :Result:='SCE_MODULE_INFO         ';
  DT_SCE_NEEDED_MODULE        :Result:='SCE_NEEDED_MODULE       ';
  DT_SCE_MODULE_ATTR          :Result:='SCE_MODULE_ATTR         ';
  DT_SCE_EXPORT_LIB           :Result:='SCE_EXPORT_LIB          ';
  DT_SCE_IMPORT_LIB           :Result:='SCE_IMPORT_LIB          ';
  DT_SCE_EXPORT_LIB_ATTR      :Result:='SCE_EXPORT_LIB_ATTR     ';
  DT_SCE_IMPORT_LIB_ATTR      :Result:='SCE_IMPORT_LIB_ATTR     ';
  DT_SCE_STUB_MODULE_NAME     :Result:='SCE_STUB_MODULE_NAME    ';
  DT_SCE_STUB_MODULE_VERSION  :Result:='SCE_STUB_MODULE_VERSION ';
  DT_SCE_STUB_LIBRARY_NAME    :Result:='SCE_STUB_LIBRARY_NAME   ';
  DT_SCE_STUB_LIBRARY_VERSION :Result:='SCE_STUB_LIBRARY_VERSION';
  DT_SCE_HASH                 :Result:='SCE_HASH                ';
  DT_SCE_PLTGOT               :Result:='SCE_PLTGOT              ';
  DT_SCE_JMPREL               :Result:='SCE_JMPREL              ';
  DT_SCE_PLTREL               :Result:='SCE_PLTREL              ';
  DT_SCE_PLTRELSZ             :Result:='SCE_PLTRELSZ            ';
  DT_SCE_RELA                 :Result:='SCE_RELA                ';
  DT_SCE_RELASZ               :Result:='SCE_RELASZ              ';
  DT_SCE_RELAENT              :Result:='SCE_RELAENT             ';
  DT_SCE_STRTAB               :Result:='SCE_STRTAB              ';
  DT_SCE_STRSZ                :Result:='SCE_STRSZ               ';
  DT_SCE_SYMTAB               :Result:='SCE_SYMTAB              ';
  DT_SCE_SYMENT               :Result:='SCE_SYMENT              ';
  DT_SCE_HASHSZ               :Result:='SCE_HASHSZ              ';
  DT_SCE_SYMTABSZ             :Result:='SCE_SYMTABSZ            ';
  DT_SCE_HIOS                 :Result:='SCE_HIOS                ';
  DT_GNU_HASH                 :Result:='GNU_HASH                ';
  DT_VERSYM                   :Result:='VERSYM                  ';
  DT_RELACOUNT                :Result:='RELACOUNT               ';
  DT_RELCOUNT                 :Result:='RELCOUNT                ';
  DT_FLAGS_1                  :Result:='FLAGS_1                 ';
  DT_VERDEF                   :Result:='VERDEF                  ';
  DT_VERDEFNUM                :Result:='VERDEFNUM               ';
  else                         Result:='0x'+HexStr(d_tag,8)+'              ';
 end;
end;

function get_lib_attr_str(id:DWord):RawByteString;
begin
 Case id of
  $01:Result:='AUTO_EXPORT';
  $02:Result:='WEAK_EXPORT';
  $08:Result:='LOOSE_IMPORT';
  $09:Result:='AUTO_EXPORT|LOOSE_IMPORT';
  $10:Result:='WEAK_EXPORT|LOOSE_IMPORT';
  else
      Result:='0x'+HexStr(id,8);
 end;
end;

function get_mod_attr_str(id:DWord):RawByteString;
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
      Result:='0x'+HexStr(id,8);
 end;
end;

procedure print_elf_dynamic(obj:p_elf_obj);
var
 s,e:ptruint;
 entry:p_elf64_dyn;
 i,count:Integer;
 mu:TModuleValue;
 la:TLibraryAttr;
 str:PAnsiChar;
begin
 if (obj^.dyn_addr<>nil) then
 begin
  entry:=obj^.dyn_addr;
  count:=obj^.dyn_size div sizeof(elf64_dyn);

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(entry)-Pointer(obj^.elf.hdr));
  e:=s+obj^.dyn_size;

  Writeln('Dynamic section:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln(' Tag                      Name/Value');
  if (count<>0) then
  For i:=0 to count-1 do
  begin

   Write(' ',get_dt_name(entry^.d_tag),' ');

   case entry^.d_tag of
    DT_SONAME,
    DT_SCE_IMPORT_LIB,
    DT_SCE_EXPORT_LIB,
    DT_SCE_NEEDED_MODULE,
    DT_SCE_MODULE_INFO,
    DT_SCE_ORIGINAL_FILENAME,
    DT_NEEDED:
      begin
       mu.value:=entry^.d_un.d_val;
       str:=@obj^.dt_strtab_addr[mu.name_offset];
       Write(mu.id:2,':',str);
      end;

    DT_SCE_MODULE_ATTR:
      begin
       mu.value:=entry^.d_un.d_val;
       Write(mu.id:2,':',get_mod_attr_str(mu.name_offset));
      end;

    DT_SCE_EXPORT_LIB_ATTR,
    DT_SCE_IMPORT_LIB_ATTR:
      begin
       la.value:=entry^.d_un.d_val;
       Write(la.id:2,':',get_lib_attr_str(la.attr));
      end;

    DT_SCE_FINGERPRINT:
     begin
      print_bytes(obj^.sce_dynlib_addr+entry^.d_un.d_val,20);
     end;

    DT_INIT_ARRAYSZ,
    DT_FINI_ARRAYSZ,
    DT_SCE_HASHSZ,
    DT_SCE_SYMTABSZ,
    DT_STRSZ,
    DT_SCE_STRSZ,
    DT_RELASZ,
    DT_SCE_RELASZ,
    DT_PLTRELSZ,
    DT_SCE_PLTRELSZ,
    DT_SYMENT,
    DT_SCE_SYMENT:
     begin
      Write(entry^.d_un.d_val);
     end;

    else
      Write('0x'+HexStr(entry^.d_un.d_val,18));
   end;

   Writeln;

   Inc(entry);
  end;

 end else
 begin
  Writeln('Dynamic section:','not exist');
 end;
 Writeln();
end;

function get_r_info_str(r_type:DWORD):RawByteString;
begin
 Case r_type of
  R_X86_64_NONE               :Result:='R_X86_64_NONE               ';
  R_X86_64_64                 :Result:='R_X86_64_64                 ';
  R_X86_64_PC32               :Result:='R_X86_64_PC32               ';
  R_X86_64_GOT32              :Result:='R_X86_64_GOT32              ';
  R_X86_64_PLT32              :Result:='R_X86_64_PLT32              ';
  R_X86_64_COPY               :Result:='R_X86_64_COPY               ';
  R_X86_64_GLOB_DAT           :Result:='R_X86_64_GLOB_DAT           ';
  R_X86_64_JUMP_SLOT          :Result:='R_X86_64_JUMP_SLOT          ';
  R_X86_64_RELATIVE           :Result:='R_X86_64_RELATIVE           ';
  R_X86_64_GOTPCREL           :Result:='R_X86_64_GOTPCREL           ';
  R_X86_64_32                 :Result:='R_X86_64_32                 ';
  R_X86_64_32S                :Result:='R_X86_64_32S                ';
  R_X86_64_16                 :Result:='R_X86_64_16                 ';
  R_X86_64_PC16               :Result:='R_X86_64_PC16               ';
  R_X86_64_8                  :Result:='R_X86_64_8                  ';
  R_X86_64_PC8                :Result:='R_X86_64_PC8                ';
  R_X86_64_DTPMOD64           :Result:='R_X86_64_DTPMOD64           ';
  R_X86_64_DTPOFF64           :Result:='R_X86_64_DTPOFF64           ';
  R_X86_64_TPOFF64            :Result:='R_X86_64_TPOFF64            ';
  R_X86_64_TLSGD              :Result:='R_X86_64_TLSGD              ';
  R_X86_64_TLSLD              :Result:='R_X86_64_TLSLD              ';
  R_X86_64_DTPOFF32           :Result:='R_X86_64_DTPOFF32           ';
  R_X86_64_GOTTPOFF           :Result:='R_X86_64_GOTTPOFF           ';
  R_X86_64_TPOFF32            :Result:='R_X86_64_TPOFF32            ';
  R_X86_64_PC64               :Result:='R_X86_64_PC64               ';
  R_X86_64_GOTOFF64           :Result:='R_X86_64_GOTOFF64           ';
  R_X86_64_GOTPC32            :Result:='R_X86_64_GOTPC32            ';
  R_X86_64_GOT64              :Result:='R_X86_64_GOT64              ';
  R_X86_64_GOTPCREL64         :Result:='R_X86_64_GOTPCREL64         ';
  R_X86_64_GOTPC64            :Result:='R_X86_64_GOTPC64            ';
  R_X86_64_GOTPLT64           :Result:='R_X86_64_GOTPLT64           ';
  R_X86_64_PLTOFF64           :Result:='R_X86_64_PLTOFF64           ';
  R_X86_64_SIZE32             :Result:='R_X86_64_SIZE32             ';
  R_X86_64_SIZE64             :Result:='R_X86_64_SIZE64             ';
  R_X86_64_GOTPC32_TLSDESC    :Result:='R_X86_64_GOTPC32_TLSDESC    ';
  R_X86_64_TLSDESC_CALL       :Result:='R_X86_64_TLSDESC_CALL       ';
  R_X86_64_TLSDESC            :Result:='R_X86_64_TLSDESC            ';
  R_X86_64_IRELATIVE          :Result:='R_X86_64_IRELATIVE          ';
  R_X86_64_RELATIVE64         :Result:='R_X86_64_RELATIVE64         ';
  R_X86_64_ORBIS_GOTPCREL_LOAD:Result:='R_X86_64_ORBIS_GOTPCREL_LOAD';

  else
                               Result:='0x'+HexStr(r_type,8)+'                  ';
 end;
end;

procedure print_elf_rela(obj:p_elf_obj);
var
 s,e:ptruint;
 entry:p_elf64_rela;
 i,count:Integer;
 idx:DWORD;
 sym:p_elf64_sym;
 str:PAnsiChar;
begin
 if (obj^.dt_rela_addr<>nil) then
 begin
  entry:=obj^.dt_rela_addr;
  count:=obj^.dt_rela_size div sizeof(elf64_rela);

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(entry)-Pointer(obj^.elf.hdr));
  e:=s+obj^.dt_rela_size;

  Writeln('Relocation section ''.rela.dyn'':0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln(' Offset     Addend     Type                         Sym. Name');
  if (count<>0) then
  For i:=0 to count-1 do
  begin

   Write(' 0x',HexStr(entry^.r_offset,8),' ');

   Write('0x',HexStr(entry^.r_addend,8),' ');

   Write(get_r_info_str(ELF64_R_TYPE(entry^.r_info)),' ');

   idx:=ELF64_R_SYM(entry^.r_info);
   sym:=@obj^.dt_symtab_addr[idx];
   str:=@obj^.dt_strtab_addr[sym^.st_name];

   Write(str);

   writeln;

   Inc(entry);
  end;

 end else
 begin
  Writeln('Relocation section ''.rela.dyn''','not exist');
 end;
 Writeln();

 if (obj^.dt_rela_plt_addr<>nil) then
 begin
  entry:=obj^.dt_rela_plt_addr;
  count:=obj^.dt_rela_plt_size div sizeof(elf64_rela);

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(entry)-Pointer(obj^.elf.hdr));
  e:=s+obj^.dt_rela_size;

  Writeln('Relocation section ''.rela.plt'':0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln(' Offset     Addend     Type                         Sym. Name');
  if (count<>0) then
  For i:=0 to count-1 do
  begin

   Write(' 0x',HexStr(entry^.r_offset,8),' ');

   Write('0x',HexStr(entry^.r_addend,8),' ');

   Write(get_r_info_str(ELF64_R_TYPE(entry^.r_info)),' ');

   idx:=ELF64_R_SYM(entry^.r_info);
   sym:=@obj^.dt_symtab_addr[idx];
   str:=@obj^.dt_strtab_addr[sym^.st_name];

   Write(str);

   writeln;

   Inc(entry);
  end;

 end else
 begin
  Writeln('Relocation section ''.rela.plt''','not exist')
 end;
 Writeln;
end;

function get_st_type_str(st_type:Byte):RawByteString;
begin
 Case st_type of
  STT_NOTYPE :Result:='NOTYPE ';
  STT_OBJECT :Result:='OBJECT ';
  STT_FUN    :Result:='FUN    ';
  STT_SECTION:Result:='SECTION';
  STT_FILE   :Result:='FILE   ';
  STT_COMMON :Result:='COMMON ';
  STT_TLS    :Result:='TLS    ';
  STT_LOOS   :Result:='LOOS   ';
  STT_SCE    :Result:='SCE    ';
  else
   Result:=IntToStr(st_type)+'      ';
 end;
end;

function get_st_bind_str(st_bind:Byte):RawByteString;
begin
 Case st_bind of
  STB_LOCAL :Result:='LOCAL ';
  STB_GLOBAL:Result:='GLOBAL';
  STB_WEAK  :Result:='WEAK  ';
  else
   Result:=IntToStr(st_bind)+'     ';
 end;
end;

function get_st_vis_str(st_vis:Byte):RawByteString;
begin
 case st_vis of
  STV_DEFAULT  :Result:='DEFAULT  ';
  STV_INTERNAL :Result:='INTERNAL ';
  STV_HIDDEN   :Result:='HIDDEN   ';
  STV_PROTECTED:Result:='PROTECTED';
  else          Result:=IntToStr(st_vis)+'        ';
 end;
end;

function get_st_shndx_str(st_shndx:Word):RawByteString;
begin
 case st_shndx of
  SHN_UNDEF  :Result:='UNDEF';
  else        Result:='0x'+HexStr(st_shndx,3);
 end;
end;

procedure print_elf_symtab(obj:p_elf_obj);
var
 s,e:ptruint;
 entry:p_elf64_sym;
 i,count:Integer;
 str:PAnsiChar;
begin
 if (obj^.dt_symtab_addr<>nil) then
 begin
  entry:=obj^.dt_symtab_addr;
  count:=obj^.dt_symtab_size div sizeof(elf64_sym);

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(entry)-Pointer(obj^.elf.hdr));
  e:=s+obj^.dt_symtab_size;

  Writeln('Symbol table ''.symtab'':0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln('  Num Value      Size Type    Bind   Vis       Ndx   Name');
  if (count<>0) then
  For i:=0 to count-1 do
  begin
   Write(' ',i:4,' ');

   Write('0x',HexStr(entry^.st_value,8),' ');

   Write(entry^.st_size:4,' ');

   Write(get_st_type_str(ELF64_ST_TYPE(entry^.st_info)),' ');

   Write(get_st_bind_str(ELF64_ST_BIND(entry^.st_info)),' ');

   Write(get_st_vis_str(ELF64_ST_VISIBILITY(entry^.st_other)),' ');

   Write(get_st_shndx_str(entry^.st_shndx),' ');

   str:=@obj^.dt_strtab_addr[entry^.st_name];

   Write(str);

   writeln;

   Inc(entry);
  end;

 end else
 begin
  Writeln('Symbol table ''.symtab''','not exist');
 end;
 Writeln();
end;

function get_vaddr_by_offset(obj:p_elf_obj;offset:Int64):Int64;
var
 elf_hdr:p_elf64_hdr;
 entry:p_elf64_phdr;
 i,count:Integer;
begin
 Result:=-1;
 if (obj^.elf.hdr=nil) then Exit;

 elf_hdr :=obj^.elf.hdr;
 entry:=get_elf_phdr(elf_hdr);
 count:=elf_hdr^.e_phnum;

 if (count<>0) then
 For i:=0 to count-1 do
 begin
  case entry^.p_type of
   PT_LOAD,
   PT_SCE_RELA:
     begin
      if (offset>=entry^.p_offset) and (offset<(entry^.p_offset+entry^.p_filesz)) then
      begin
       offset:=offset-entry^.p_offset; //delta
       Result:=entry^.p_vaddr+offset;
       Break;
      end;
     end;
  end;
  Inc(entry);
 end;
end;

function get_rela_by_offset(obj:p_elf_obj;offset:Int64):Int64;
var
 pvaddr:Int64;
 entry:p_elf64_rela;
 i,count:Integer;
begin
 Result:=-1;
 if (obj^.dt_rela_addr=nil) then Exit;

 pvaddr:=get_vaddr_by_offset(obj,offset);
 if (pvaddr=-1) then Exit;

 entry:=obj^.dt_rela_addr;
 count:=obj^.dt_rela_size div SizeOf(elf64_rela);

 if (count<>0) then
 For i:=0 to count-1 do
 begin

  if (entry^.r_offset=pvaddr) then
  begin

   case ELF64_R_TYPE(entry^.r_info) of
    R_X86_64_RELATIVE:
      begin
       Result:=entry^.r_addend;
       Break;
      end;
    else;
   end;

  end;

  Inc(entry);
 end;

end;

function upgrade_ptr(pp:Pointer;obj:p_elf_obj):Pointer;
var
 offset:Int64;
begin
 Result:=nil;
 offset:=Int64(pp)-Int64(obj^.elf.hdr);
 offset:=get_rela_by_offset(obj,offset);
 if (offset<>-1) then
 begin
  Result:=Pointer(obj^.elf.hdr)+offset+obj^.min_offset;
 end else
 if (PInt64(pp)^<>0) then
 begin
  Result:=Pointer(obj^.elf.hdr)+PInt64(pp)^+obj^.min_offset;
 end;
 if (Result<>nil) then
 begin
  if (Result>=(Pointer(obj^.elf.hdr)+obj^.elf.size)) then Result:=nil;
 end;
end;

procedure WOfsstr(const prefix:RawByteString;pp:PPointer;obj:p_elf_obj);
var
 p:PChar;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  p:='null';
 end;

 Writeln(prefix,p);
end;

procedure WOfsDwr(const prefix:RawByteString;pp:PPointer;obj:p_elf_obj);
var
 p:PDWORD;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln(prefix,'null');
 end else
 begin
  Writeln(prefix,'0x',HexStr(p^,8));
 end;
end;

procedure WOfsQwr(const prefix:RawByteString;pp:PPointer;obj:p_elf_obj);
var
 p:PQWORD;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln(prefix,'null');
 end else
 begin
  Writeln(prefix,'0x',HexStr(p^,16));
 end;
end;

procedure W_sceLibcMallocReplace(pp:PPointer;obj:p_elf_obj);
var
 p:PsceLibcMallocReplace;
 e:Pointer;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln('   _sceLibcMallocReplace            :','null');
  Exit;
 end;

 e:=Pointer(p)+p^.Size;

 Writeln('   _sceLibcMallocReplace            :');

 if (Pointer(@p^.Size                   )+SizeOf(p^.Size                   )<=e) then Writeln('     Size                           :0x',HexStr(p^.Size    ,8));
 if (Pointer(@p^.Unknown1               )+SizeOf(p^.Unknown1               )<=e) then Writeln('     Unknown1                       :0x',HexStr(p^.Unknown1,8));

 if (Pointer(@p^.user_malloc_init       )+SizeOf(p^.user_malloc_init       )<=e) then WOfsQwr('     user_malloc_init               :',@p^.user_malloc_init       ,obj);
 if (Pointer(@p^.user_malloc_finalize   )+SizeOf(p^.user_malloc_finalize   )<=e) then WOfsQwr('     user_malloc_finalize           :',@p^.user_malloc_finalize   ,obj);
 if (Pointer(@p^.user_malloc            )+SizeOf(p^.user_malloc            )<=e) then WOfsQwr('     user_malloc                    :',@p^.user_malloc            ,obj);
 if (Pointer(@p^.user_free              )+SizeOf(p^.user_free              )<=e) then WOfsQwr('     user_free                      :',@p^.user_free              ,obj);
 if (Pointer(@p^.user_calloc            )+SizeOf(p^.user_calloc            )<=e) then WOfsQwr('     user_calloc                    :',@p^.user_calloc            ,obj);
 if (Pointer(@p^.user_realloc           )+SizeOf(p^.user_realloc           )<=e) then WOfsQwr('     user_realloc                   :',@p^.user_realloc           ,obj);
 if (Pointer(@p^.user_memalign          )+SizeOf(p^.user_memalign          )<=e) then WOfsQwr('     user_memalign                  :',@p^.user_memalign          ,obj);
 if (Pointer(@p^.user_reallocalign      )+SizeOf(p^.user_reallocalign      )<=e) then WOfsQwr('     user_reallocalign              :',@p^.user_reallocalign      ,obj);
 if (Pointer(@p^.user_posix_memalign    )+SizeOf(p^.user_posix_memalign    )<=e) then WOfsQwr('     user_posix_memalign            :',@p^.user_posix_memalign    ,obj);
 if (Pointer(@p^.user_malloc_stats      )+SizeOf(p^.user_malloc_stats      )<=e) then WOfsQwr('     user_malloc_stats              :',@p^.user_malloc_stats      ,obj);
 if (Pointer(@p^.user_malloc_stats_fast )+SizeOf(p^.user_malloc_stats_fast )<=e) then WOfsQwr('     user_malloc_stats_fast         :',@p^.user_malloc_stats_fast ,obj);
 if (Pointer(@p^.user_malloc_usable_size)+SizeOf(p^.user_malloc_usable_size)<=e) then WOfsQwr('     user_malloc_usable_size        :',@p^.user_malloc_usable_size,obj);
end;

procedure W_sceLibcNewReplace(pp:PPointer;obj:p_elf_obj);
var
 p:PsceLibcNewReplace;
 e:Pointer;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln('   _sceLibcNewReplace               :','null');
  Exit;
 end;

 e:=Pointer(p)+p^.Size;

 Writeln('   _sceLibcNewReplace               :');

 if (Pointer(@p^.Size               )+SizeOf(p^.Size               )<=e) then Writeln('     Size                           :0x',HexStr(p^.Size    ,8));
 if (Pointer(@p^.Unknown1           )+SizeOf(p^.Unknown1           )<=e) then Writeln('     Unknown1                       :0x',HexStr(p^.Unknown1,8));

 if (Pointer(@p^.user_new_1         )+SizeOf(p^.user_new_1         )<=e) then WOfsQwr('     user_new_1                     :',@p^.user_new_1         ,obj);
 if (Pointer(@p^.user_new_2         )+SizeOf(p^.user_new_2         )<=e) then WOfsQwr('     user_new_2                     :',@p^.user_new_2         ,obj);
 if (Pointer(@p^.user_new_array_1   )+SizeOf(p^.user_new_array_1   )<=e) then WOfsQwr('     user_new_array_1               :',@p^.user_new_array_1   ,obj);
 if (Pointer(@p^.user_new_array_2   )+SizeOf(p^.user_new_array_2   )<=e) then WOfsQwr('     user_new_array_2               :',@p^.user_new_array_2   ,obj);
 if (Pointer(@p^.user_delete_1      )+SizeOf(p^.user_delete_1      )<=e) then WOfsQwr('     user_delete_1                  :',@p^.user_delete_1      ,obj);
 if (Pointer(@p^.user_delete_2      )+SizeOf(p^.user_delete_2      )<=e) then WOfsQwr('     user_delete_2                  :',@p^.user_delete_2      ,obj);
 if (Pointer(@p^.user_delete_array_1)+SizeOf(p^.user_delete_array_1)<=e) then WOfsQwr('     user_delete_array_1            :',@p^.user_delete_array_1,obj);
 if (Pointer(@p^.user_delete_array_2)+SizeOf(p^.user_delete_array_2)<=e) then WOfsQwr('     user_delete_array_2            :',@p^.user_delete_array_2,obj);
 if (Pointer(@p^.user_delete_3      )+SizeOf(p^.user_delete_3      )<=e) then WOfsQwr('     user_delete_3                  :',@p^.user_delete_3      ,obj);
 if (Pointer(@p^.user_delete_4      )+SizeOf(p^.user_delete_4      )<=e) then WOfsQwr('     user_delete_4                  :',@p^.user_delete_4      ,obj);
 if (Pointer(@p^.user_delete_array_3)+SizeOf(p^.user_delete_array_3)<=e) then WOfsQwr('     user_delete_array_3            :',@p^.user_delete_array_3,obj);
 if (Pointer(@p^.user_delete_array_4)+SizeOf(p^.user_delete_array_4)<=e) then WOfsQwr('     user_delete_array_4            :',@p^.user_delete_array_4,obj);
end;

procedure W_sceLibcMallocReplaceForTls(pp:PPointer;obj:p_elf_obj);
var
 p:PsceLibcMallocReplaceForTls;
 e:Pointer;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln('   _sceLibcMallocReplaceForTls      :','null');
  Exit;
 end;

 e:=Pointer(p)+p^.Size;

 Writeln('   _sceLibcMallocReplaceForTls      :');

 if (Pointer(@p^.Size                       )+SizeOf(p^.Size                       )<=e) then Writeln('     Size                           :0x',HexStr(p^.Size    ,8));
 if (Pointer(@p^.Unknown1                   )+SizeOf(p^.Unknown1                   )<=e) then Writeln('     Unknown1                       :0x',HexStr(p^.Unknown1,8));

 if (Pointer(@p^.user_malloc_init_for_tls   )+SizeOf(p^.user_malloc_init_for_tls   )<=e) then WOfsQwr('     user_malloc_init_for_tls       :',@p^.user_malloc_init_for_tls   ,obj);
 if (Pointer(@p^.user_malloc_fini_for_tls   )+SizeOf(p^.user_malloc_fini_for_tls   )<=e) then WOfsQwr('     user_malloc_fini_for_tls       :',@p^.user_malloc_fini_for_tls   ,obj);
 if (Pointer(@p^.user_malloc_for_tls        )+SizeOf(p^.user_malloc_for_tls        )<=e) then WOfsQwr('     user_malloc_for_tls            :',@p^.user_malloc_for_tls        ,obj);
 if (Pointer(@p^.user_posix_memalign_for_tls)+SizeOf(p^.user_posix_memalign_for_tls)<=e) then WOfsQwr('     user_posix_memalign_for_tls    :',@p^.user_posix_memalign_for_tls,obj);
 if (Pointer(@p^.user_free_for_tls          )+SizeOf(p^.user_free_for_tls          )<=e) then WOfsQwr('     user_free_for_tls              :',@p^.user_free_for_tls          ,obj);
end;

procedure W_sceLibcParam(pp:PPointer;obj:p_elf_obj);
var
 p:PSceLibcParam;
 e:Pointer;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln('   _sceLibcParam                    :','null');
  Exit;
 end;

 e:=Pointer(p)+p^.Size;

 Writeln('   _sceLibcParam                    :');

 if (Pointer(@p^.Size                             )+SizeOf(p^.Size                             )<=e) then Writeln('   Size                             :0x',HexStr(p^.Size,8)   );
 if (Pointer(@p^.entry_count                      )+SizeOf(p^.entry_count                      )<=e) then Writeln('   entry_count                      :',p^.entry_count        );
 if (Pointer(@p^.SceLibcInternalHeap              )+SizeOf(p^.SceLibcInternalHeap              )<=e) then Writeln('   SceLibcInternalHeap              :',p^.SceLibcInternalHeap);

 if (Pointer(@p^.sceLibcHeapSize                  )+SizeOf(p^.sceLibcHeapSize                  )<=e) then WOfsDwr('   sceLibcHeapSize                  :',@p^.sceLibcHeapSize                  ,obj);
 if (Pointer(@p^.sceLibcHeapDelayedAlloc          )+SizeOf(p^.sceLibcHeapDelayedAlloc          )<=e) then WOfsDwr('   sceLibcHeapDelayedAlloc          :',@p^.sceLibcHeapDelayedAlloc          ,obj);
 if (Pointer(@p^.sceLibcHeapExtendedAlloc         )+SizeOf(p^.sceLibcHeapExtendedAlloc         )<=e) then WOfsDwr('   sceLibcHeapExtendedAlloc         :',@p^.sceLibcHeapExtendedAlloc         ,obj);
 if (Pointer(@p^.sceLibcHeapInitialSize           )+SizeOf(p^.sceLibcHeapInitialSize           )<=e) then WOfsDwr('   sceLibcHeapInitialSize           :',@p^.sceLibcHeapInitialSize           ,obj);

 if (Pointer(@p^._sceLibcMallocReplace            )+SizeOf(p^._sceLibcMallocReplace            )<=e) then W_sceLibcMallocReplace(@p^._sceLibcMallocReplace,obj);
 if (Pointer(@p^._sceLibcNewReplace               )+SizeOf(p^._sceLibcNewReplace               )<=e) then W_sceLibcNewReplace   (@p^._sceLibcNewReplace   ,obj);

 if (Pointer(@p^.sceLibcHeapHighAddressAlloc      )+SizeOf(p^.sceLibcHeapHighAddressAlloc      )<=e) then WOfsQwr('   sceLibcHeapHighAddressAlloc      :',@p^.sceLibcHeapHighAddressAlloc      ,obj);
 if (Pointer(@p^.Need_sceLibc                     )+SizeOf(p^.Need_sceLibc                     )<=e) then WOfsDwr('   Need_sceLibc                     :',@p^.Need_sceLibc                     ,obj);
 if (Pointer(@p^.sceLibcHeapMemoryLock            )+SizeOf(p^.sceLibcHeapMemoryLock            )<=e) then WOfsQwr('   sceLibcHeapMemoryLock            :',@p^.sceLibcHeapMemoryLock            ,obj);
 if (Pointer(@p^.sceKernelInternalMemorySize      )+SizeOf(p^.sceKernelInternalMemorySize      )<=e) then WOfsDwr('   sceKernelInternalMemorySize      :',@p^.sceKernelInternalMemorySize      ,obj);

 if (Pointer(@p^._sceLibcMallocReplaceForTls      )+SizeOf(p^._sceLibcMallocReplaceForTls      )<=e) then W_sceLibcMallocReplaceForTls(@p^._sceLibcMallocReplaceForTls,obj);

 if (Pointer(@p^.sceLibcMaxSystemSize             )+SizeOf(p^.sceLibcMaxSystemSize             )<=e) then WOfsQwr('   sceLibcMaxSystemSize             :',@p^.sceLibcMaxSystemSize             ,obj);
 if (Pointer(@p^.sceLibcHeapDebugFlags            )+SizeOf(p^.sceLibcHeapDebugFlags            )<=e) then WOfsQwr('   sceLibcHeapDebugFlags            :',@p^.sceLibcHeapDebugFlags            ,obj);
 if (Pointer(@p^.sceLibcStdThreadStackSize        )+SizeOf(p^.sceLibcStdThreadStackSize        )<=e) then WOfsDwr('   sceLibcStdThreadStackSize        :',@p^.sceLibcStdThreadStackSize        ,obj);


 if (Pointer(@p^.sceKernelInternalMemoryDebugFlags)+SizeOf(p^.sceKernelInternalMemoryDebugFlags)<=e) then WOfsDwr('   sceKernelInternalMemoryDebugFlags:',@p^.sceKernelInternalMemoryDebugFlags,obj);
 if (Pointer(@p^.sceLibcWorkerThreadNum           )+SizeOf(p^.sceLibcWorkerThreadNum           )<=e) then WOfsDwr('   sceLibcWorkerThreadNum           :',@p^.sceLibcWorkerThreadNum           ,obj);
 if (Pointer(@p^.sceLibcWorkerThreadPriority      )+SizeOf(p^.sceLibcWorkerThreadPriority      )<=e) then WOfsDwr('   sceLibcWorkerThreadPriority      :',@p^.sceLibcWorkerThreadPriority      ,obj);
 if (Pointer(@p^.sceLibcThreadUnnamedObjects      )+SizeOf(p^.sceLibcThreadUnnamedObjects      )<=e) then WOfsDwr('   sceLibcThreadUnnamedObjects      :',@p^.sceLibcThreadUnnamedObjects      ,obj);
end;

procedure W_sceKernelMemParam(pp:PPointer;obj:p_elf_obj);
var
 p:PSceKernelMemParam;
 e:Pointer;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln('   _sceKernelMemParam               :','null');
  Exit;
 end;

 e:=Pointer(p)+p^.Size;

 Writeln('   _sceKernelMemParam               :');

 if (Pointer(@p^.Size                         )+SizeOf(p^.Size                         )<=e) then Writeln('     Size                           :0x',HexStr(p^.Size,8));

 if (Pointer(@p^.sceKernelExtendedPageTable   )+SizeOf(p^.sceKernelExtendedPageTable   )<=e) then WOfsQwr('     sceKernelExtendedPageTable     :',@p^.sceKernelExtendedPageTable   ,obj);
 if (Pointer(@p^.sceKernelFlexibleMemorySize  )+SizeOf(p^.sceKernelFlexibleMemorySize  )<=e) then WOfsQwr('     sceKernelFlexibleMemorySize    :',@p^.sceKernelFlexibleMemorySize  ,obj);
 if (Pointer(@p^.sceKernelExtendedMemory1     )+SizeOf(p^.sceKernelExtendedMemory1     )<=e) then WOfsQwr('     sceKernelExtendedMemory1       :',@p^.sceKernelExtendedMemory1     ,obj);
 if (Pointer(@p^.sceKernelExtendedGpuPageTable)+SizeOf(p^.sceKernelExtendedGpuPageTable)<=e) then WOfsQwr('     sceKernelExtendedGpuPageTable  :',@p^.sceKernelExtendedGpuPageTable,obj);
 if (Pointer(@p^.sceKernelExtendedMemory2     )+SizeOf(p^.sceKernelExtendedMemory2     )<=e) then WOfsQwr('     sceKernelExtendedMemory2       :',@p^.sceKernelExtendedMemory2     ,obj);
 if (Pointer(@p^.sceKernelExtendedCpuPageTable)+SizeOf(p^.sceKernelExtendedCpuPageTable)<=e) then WOfsQwr('     sceKernelExtendedCpuPageTable  :',@p^.sceKernelExtendedCpuPageTable,obj);
end;

procedure W_sceKernelFsParam(pp:PPointer;obj:p_elf_obj);
var
 p:PSceKernelFsParam;
 e:Pointer;
begin
 p:=upgrade_ptr(pp,obj);

 if (p=nil) then
 begin
  Writeln('   _sceKernelFsParam                :','null');
  Exit;
 end;

 e:=Pointer(p)+p^.Size;

 Writeln('   _sceKernelFsParam                :');

 if (Pointer(@p^.Size               )+SizeOf(p^.Size             )<=e) then Writeln('     Size                           :0x',HexStr(p^.Size,8));

 if (Pointer(@p^.sceKernelFsDupDent)+SizeOf(p^.sceKernelFsDupDent)<=e) then WOfsQwr('     sceKernelFsDupDent             :',@p^.sceKernelFsDupDent,obj);
end;

procedure print_elf_sce_procparam(obj:p_elf_obj);
var
 s,e:ptruint;

 pa:PSceProcParam;
 pe:Pointer;
begin
 if (obj^.proc_param_addr<>nil) then
 begin
  pa:=obj^.proc_param_addr;
  pe:=Pointer(pa)+obj^.proc_param_size;

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(pa)-Pointer(obj^.elf.hdr));
  e:=s+obj^.proc_param_size;

  Writeln('SCE proc param:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  if (Pointer(@pa^.Size                      )+SizeOf(pa^.Size                      )<=pe) then Writeln(' Size                               :0x',HexStr(pa^.Size              ,8));
  if (Pointer(@pa^.Magic                     )+SizeOf(pa^.Magic                     )<=pe) then Writeln(' Magic                              :0x',HexStr(pa^.Magic             ,8));
  if (Pointer(@pa^.Entry_count               )+SizeOf(pa^.Entry_count               )<=pe) then Writeln(' Entry_count                        :0x',HexStr(pa^.Entry_count       ,8));
  if (Pointer(@pa^.SDK_version               )+SizeOf(pa^.SDK_version               )<=pe) then Writeln(' SDK_version                        :',get_sdk_version_str(pa^.SDK_version));
  if (Pointer(@pa^.sceProcessName            )+SizeOf(pa^.sceProcessName            )<=pe) then WOfsstr(' sceProcessName                     :',@pa^.sceProcessName            ,obj);
  if (Pointer(@pa^.sceUserMainThreadName     )+SizeOf(pa^.sceUserMainThreadName     )<=pe) then WOfsstr(' sceUserMainThreadName              :',@pa^.sceUserMainThreadName     ,obj);
  if (Pointer(@pa^.sceUserMainThreadPriority )+SizeOf(pa^.sceUserMainThreadPriority )<=pe) then WOfsDwr(' sceUserMainThreadPriority          :',@pa^.sceUserMainThreadPriority ,obj);
  if (Pointer(@pa^.sceUserMainThreadStackSize)+SizeOf(pa^.sceUserMainThreadStackSize)<=pe) then WOfsDwr(' sceUserMainThreadStackSize         :',@pa^.sceUserMainThreadStackSize,obj);

  if (Pointer(@pa^._sceLibcParam             )+SizeOf(pa^._sceLibcParam             )<=pe) then W_sceLibcParam     (@pa^._sceLibcParam     ,obj);
  if (Pointer(@pa^._sceKernelMemParam        )+SizeOf(pa^._sceKernelMemParam        )<=pe) then W_sceKernelMemParam(@pa^._sceKernelMemParam,obj);
  if (Pointer(@pa^._sceKernelFsParam         )+SizeOf(pa^._sceKernelFsParam         )<=pe) then W_sceKernelFsParam (@pa^._sceKernelFsParam ,obj);

  if (Pointer(@pa^.sceProcessPreloadEnabled  )+SizeOf(pa^.sceProcessPreloadEnabled  )<=pe) then WOfsDwr(' sceProcessPreloadEnabled           :',@pa^.sceProcessPreloadEnabled  ,obj);
 end else
 begin
  Writeln('SCE proc param:','not exist');
 end;
 Writeln;
end;

procedure print_elf_sce_moduleparam(obj:p_elf_obj);
var
 s,e:ptruint;

 pa:PsceModuleParam;
 pe:Pointer;
begin
 if (obj^.module_param_addr<>nil) then
 begin
  pa:=obj^.module_param_addr;
  pe:=Pointer(pa)+obj^.module_param_size;

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(pa)-Pointer(obj^.elf.hdr));
  e:=s+obj^.module_param_size;

  Writeln('SCE module param:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  if (Pointer(@pa^.Size       )+SizeOf(pa^.Size       )<=pe) then Writeln(' Size       :0x',HexStr(pa^.Size       ,8));
  if (Pointer(@pa^.Magic      )+SizeOf(pa^.Magic      )<=pe) then Writeln(' Magic      :0x',HexStr(pa^.Magic      ,8));
  if (Pointer(@pa^.SDK_version)+SizeOf(pa^.SDK_version)<=pe) then Writeln(' SDK_version:',get_sdk_version_str(pa^.SDK_version));
 end else
 begin
  Writeln('SCE module param:','not exist');
 end;
 Writeln;
end;

procedure print_elf_tls(obj:p_elf_obj);
label
 _not_exist;
var
 s,e:ptruint;
 m:PBYTE;
begin
 if (obj^.tls_addr<>nil) then
 begin
  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(obj^.tls_addr)-Pointer(obj^.elf.hdr));
  e:=s+obj^.tls_size;

  if (s>=e) then goto _not_exist;

  Writeln('TLS data:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  m:=obj^.tls_addr;

  print_hex(m,s,e);
 end else
 begin
  _not_exist:
  Writeln('TLS data:','not exist');
 end;
 Writeln;
end;

type
 p_nid_info=^t_nid_info;
 t_nid_info=record
  sym:elf64_sym;
  enc:RawByteString;
  dst:RawByteString;
  nid:QWORD;
  lib:WORD;
 end;

procedure convert_info_name(info:p_nid_info);
var
 nModId,nLibId:WORD;
begin
 nLibId:=0;

 case ELF64_ST_TYPE(info^.sym.st_info) of
  STT_NOTYPE: //original string
   begin
    info^.nid:=ps4_nid_hash(info^.enc);
    info^.dst:=info^.enc;
    info^.enc:=EncodeValue64(info^.nid);
   end;
  STT_SCE:    //base64 string without specifying the module and library
   begin
    DecodeValue64(PChar(info^.enc),Length(info^.enc),info^.nid);
    info^.dst:=GetFunctName(info^.nid);
   end;
  else        //base64 string specifying the module and library
   begin
    DecodeEncName(PChar(info^.enc),nModId,nLibId,info^.nid);
    info^.dst:=GetFunctName(info^.nid);
   end;
 end;

 info^.lib:=nLibId;
end;

function get_st_type_short(st_type:Byte):RawByteString;
begin
 Case st_type of
  STT_NOTYPE :Result:='NTP';
  STT_OBJECT :Result:='OBJ';
  STT_FUN    :Result:='FUN';
  STT_SECTION:Result:='SEC';
  STT_FILE   :Result:='FIL';
  STT_COMMON :Result:='CMN';
  STT_TLS    :Result:='TLS';
  STT_SCE    :Result:='SCE';
  else
              Result:=HexStr(st_type,3);
 end;
end;

function get_st_bind_short(st_bind:Byte):RawByteString;
begin
 Case st_bind of
  STB_LOCAL :Result:='L';
  STB_GLOBAL:Result:='G';
  STB_WEAK  :Result:='W';
  else
             Result:=HexStr(st_bind,1);
 end;
end;

type
 TLIBRARY=record
  Name  :RawByteString;
  Import:Boolean
 end;

procedure print_string_dump(obj:p_elf_obj);
var
 d_entry:p_elf64_dyn;
 s_entry:p_elf64_sym;
 i,count:Integer;
 lu:TLibraryValue;
 str:PAnsiChar;

 lib:TLIBRARY;

 lib_array:array of TLIBRARY;

 nid_info:t_nid_info;

 procedure set_lib(id:Word;lib:TLIBRARY); inline;
 var
  i:Integer;
 begin
  i:=Length(lib_array);
  if (i<=id) then
  begin
   i:=id+1;
   SetLength(lib_array,i);
  end;
  lib_array[id]:=lib;
 end;

begin
 SetLength(lib_array,0);

 if (obj^.dyn_addr<>nil) then
 begin
  d_entry:=obj^.dyn_addr;
  count:=obj^.dyn_size div sizeof(elf64_dyn);

  if (count<>0) then
  For i:=0 to count-1 do
  begin

   case d_entry^.d_tag of

    DT_SCE_IMPORT_LIB,
    DT_SCE_EXPORT_LIB:
      begin
       lu.value:=d_entry^.d_un.d_val;
       str:=@obj^.dt_strtab_addr[lu.name_offset];

       lib.Name  :=str;
       lib.Import:=(d_entry^.d_tag=DT_SCE_IMPORT_LIB);

       set_lib(lu.id,lib);
      end;

    else;
   end;

   Inc(d_entry);
  end;

 end;

 if (obj^.dt_symtab_addr<>nil) then
 begin
  s_entry:=obj^.dt_symtab_addr;
  count:=obj^.dt_symtab_size div sizeof(elf64_sym);

  if (count<>0) then
  For i:=0 to count-1 do
  begin
   str:=@obj^.dt_strtab_addr[s_entry^.st_name];

   if (Trim(str)<>'') then
   begin
    nid_info:=Default(t_nid_info);
    nid_info.sym:=s_entry^;
    nid_info.enc:=str;

    convert_info_name(@nid_info);

    Case lib_array[nid_info.lib].Import of
     True :Write('I|');
     False:Write('E|');
    end;

    Write(get_st_type_short(ELF64_ST_TYPE(s_entry^.st_info)),'|');
    Write(get_st_bind_short(ELF64_ST_BIND(s_entry^.st_info)),'|');

    Write('0x',HexStr(nid_info.nid,16),'|');

    Write(BaseEncName(PChar(nid_info.enc)),'|');

    Write(lib_array[nid_info.lib].Name,'|');

    Write(nid_info.dst);

    writeln;
   end;

   Inc(s_entry);
  end;

 end;

 Writeln();
end;

type
 t_print_param=(
  pp_file_size,
  pp_self_header,
  pp_self_segs,
  pp_self_authinfo,
  pp_self_metadata,
  pp_file_header,
  pp_program_headers,
  pp_dynamic,
  pp_relocs,
  pp_symbols,
  pp_sce_procparam,
  pp_sce_moduleparam,
  pp_tls,
  pp_string_dump
 );
 t_print_param_set=Set of t_print_param;

var
 print_param:t_print_param_set=[];
 FileName:RawByteString;

procedure parse_param;
var
 i:Integer;
 S:RawByteString;
begin
 print_param:=[];
 For i:=1 to ParamCount do
 begin
  S:=Trim(ParamStr(i));
  case S of
   '-a','--all'            :print_param:=[pp_file_size,
                                          pp_self_header,
                                          pp_self_segs,
                                          pp_self_authinfo,
                                          pp_file_header,
                                          pp_program_headers,
                                          pp_dynamic,
                                          pp_relocs,
                                          pp_symbols,
                                          pp_sce_procparam,
                                          pp_sce_moduleparam
                                         ];

   '-F','--file_size'      :print_param:=print_param+[pp_file_size      ];
   '-H','--self_header'    :print_param:=print_param+[pp_self_header    ];
   '-S','--self_segs'      :print_param:=print_param+[pp_self_segs      ];
   '-A','--self_authinfo'  :print_param:=print_param+[pp_self_authinfo  ];
   '-M','--self_metadata'  :print_param:=print_param+[pp_self_metadata  ];
   '-h','--file-header'    :print_param:=print_param+[pp_file_header    ];
   '-l','--program-headers':print_param:=print_param+[pp_program_headers];
   '-d','--dynamic'        :print_param:=print_param+[pp_dynamic        ];
   '-r','--relocs'         :print_param:=print_param+[pp_relocs         ];
   '-s','--symbols'        :print_param:=print_param+[pp_symbols        ];
   '-P','--sce_procparam'  :print_param:=print_param+[pp_sce_procparam  ];
   '-m','--sce_moduleparam':print_param:=print_param+[pp_sce_moduleparam];
   '-t','--tls'            :print_param:=print_param+[pp_tls            ];
   '-p','--string-dump'    :print_param:=print_param+[pp_string_dump    ];
   else
    FileName:=S;
  end;
 end;
end;

var
 r:Integer;
 obj:elf_obj;

begin
 parse_param;

 if (ParamCount<=1) or (FileName='') then
 begin
  Writeln('Usage: readself <option(s)> elf/self-file');
  Writeln(' Display information about the contents of ELF/SELF format files');
  Writeln(' Options are:');
  Writeln('  -a --all             Equivalent to: -F -H -S -A -h -l -d -r -s -P -m');
  Writeln('  -F --file_size       Display the file size');
  Writeln('  -H --self_header     Display the SELF file header');
  Writeln('  -S --self_segs       Display the segments header');
  Writeln('  -A --self_authinfo   Display the SELF authinfo');
  Writeln('  -M --self_metadata   Display the SELF metadata');
  Writeln('  -h --file-header     Display the ELF file header');
  Writeln('  -l --program-headers Display the program headers');
  Writeln('  -d --dynamic         Display the dynamic section');
  Writeln('  -r --relocs          Display the relocations');
  Writeln('  -s --symbols         Display the symbol table');
  Writeln('  -P --sce_procparam   Display the sce proc param');
  Writeln('  -m --sce_moduleparam Display the sce module param');
  Writeln('  -t --tls             Display the tls data');
  Writeln('  -p --string-dump     Dump the contents of symbols');
  Exit;
 end;

 r:=load_self(FileName,@obj);

 if (r=0) then
 begin
  if (pp_file_size       in print_param) then print_file_size    (@obj);
  if (pp_self_header     in print_param) then print_self_header  (@obj);
  if (pp_self_segs       in print_param) then print_self_segs    (@obj);
  if (pp_self_authinfo   in print_param) then print_self_authinfo(@obj);
  if (pp_self_metadata   in print_param) then print_self_metadata(@obj);

  if (obj.is_encrypted<>0) then
  begin
   Writeln('Elf is_encrypted');
   Writeln;
  end else
  begin
   if (pp_file_header     in print_param) then print_elf_header   (@obj);
   if (pp_program_headers in print_param) then print_elf_phdr     (@obj);

   scan_phdr(@obj);
   scan_dynamic(@obj);

   if (pp_dynamic         in print_param) then print_elf_dynamic        (@obj);
   if (pp_relocs          in print_param) then print_elf_rela           (@obj);
   if (pp_symbols         in print_param) then print_elf_symtab         (@obj);
   if (pp_sce_procparam   in print_param) then print_elf_sce_procparam  (@obj);
   if (pp_sce_moduleparam in print_param) then print_elf_sce_moduleparam(@obj);
   if (pp_tls             in print_param) then print_elf_tls            (@obj);


   if (pp_string_dump     in print_param) then print_string_dump        (@obj);

  end;

 end else
 begin
  Writeln('Error(',r,') load file:',FileName);
 end;

 free_elf_obj(@obj);

 //readln;
end.

