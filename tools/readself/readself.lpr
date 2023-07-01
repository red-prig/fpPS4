program readself;

uses
 sysutils,
 elf64;

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
  dyn_adr:p_elf64_dyn;
  dyn_cnt:Int64;
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
        s:=self_segs[i].offset;
        MinSeg:=MinInt64(s,MinSeg);
        s:=s+minInt64(self_segs[i].filesz,self_segs[i].filesz);
        MaxSeg:=MaxInt64(s,MaxSeg);
       end;

      MinSeg:=MinInt64(MinSeg,s);
      MaxSeg:=MinInt64(MaxSeg,s);

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
  Writeln(' Program_Version:0x',HexStr(authinfo^.Program_Version,16));
  Writeln(' System_Version :0x',HexStr(authinfo^.System_Version,16));

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
 if (obj^.is_encrypted<>0) then
 begin
  Writeln('Elf is_encrypted');
  Writeln;
  Exit;
 end;

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
 if (obj^.is_encrypted<>0) then
 begin
  Writeln('Elf is_encrypted');
  Writeln;
  Exit;
 end;

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
 elf_hdr :p_elf64_hdr;
 elf_phdr:p_elf64_phdr;
 i,count:Integer;
begin
 if (obj^.is_encrypted<>0) then Exit;
 if (obj^.elf.hdr=nil) then Exit;

 elf_hdr :=obj^.elf.hdr;
 elf_phdr:=get_elf_phdr(elf_hdr);
 count:=elf_hdr^.e_phnum;

 if (count<>0) then
 For i:=0 to count-1 do
 begin

  case elf_phdr^.p_type of
   PT_DYNAMIC:
     begin
      obj^.dyn_adr:=Pointer(elf_hdr)+elf_phdr^.p_offset;
      obj^.dyn_cnt:=elf_phdr^.p_filesz div sizeof(Elf64_Dyn);
     end;

   PT_TLS:
     begin
      obj^.tls_addr:=Pointer(elf_hdr)+elf_phdr^.p_offset;
      obj^.tls_size:=elf_phdr^.p_filesz;
     end;

   PT_SCE_DYNLIBDATA:
     begin
      obj^.sce_dynlib_addr:=Pointer(elf_hdr)+elf_phdr^.p_offset;
      obj^.sce_dynlib_size:=elf_phdr^.p_filesz;
     end;

   PT_SCE_PROCPARAM:
     begin
      obj^.proc_param_addr:=Pointer(elf_hdr)+elf_phdr^.p_offset;
      obj^.proc_param_size:=elf_phdr^.p_filesz;
     end;

   PT_SCE_MODULE_PARAM:
     begin
      obj^.module_param_addr:=Pointer(elf_hdr)+elf_phdr^.p_offset;
      obj^.module_param_size:=elf_phdr^.p_filesz;
     end;

   PT_GNU_EH_FRAME:
    begin
     obj^.eh_frame_hdr_addr:=Pointer(elf_hdr)+elf_phdr^.p_offset;
     obj^.eh_frame_hdr_size:=elf_phdr^.p_filesz;
    end;

   PT_LOAD,
   PT_SCE_RELRO:
    begin

    end;


  end;

  Inc(elf_phdr);
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

procedure print_elf_dynamic(obj:p_elf_obj);
var
 s,e:ptruint;
 dyn_adr:p_elf64_dyn;
 i,count:Integer;
begin
 if (obj^.is_encrypted<>0) then
 begin
  Writeln('Elf is_encrypted');
  Writeln;
  Exit;
 end;

 if (obj^.dyn_adr<>nil) then
 begin
  dyn_adr:=obj^.dyn_adr;
  count  :=obj^.dyn_cnt;

  s:=get_elf_hdr_offset(obj);
  s:=s+(Pointer(dyn_adr)-Pointer(obj^.elf.hdr));
  e:=s+(count*sizeof(Elf64_Dyn));

  Writeln('Dynamic section:0x',HexStr(s,8),'..0x',HexStr(e,8),':',(e-s));

  Writeln(' Tag                      Name/Value');
  if (count<>0) then
  For i:=0 to count-1 do
  begin

   Write(' ',get_dt_name(dyn_adr^.d_tag),' ');

   Write('0x'+HexStr(dyn_adr^.d_un.d_val,18),' ');

   Writeln;

   Inc(dyn_adr);
  end;

 end else
 begin
  Writeln('Dynamic section:','not exist');
 end;
 Writeln();
end;

procedure print_elf_tls(obj:p_elf_obj);
label
 _not_exist;
var
 s,e:ptruint;
 m:PBYTE;
begin
 if (obj^.is_encrypted<>0) then
 begin
  Writeln('Elf is_encrypted');
  Writeln;
  Exit;
 end;

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
 t_print_param=(
  pp_file_size,
  pp_self_header,
  pp_self_segs,
  pp_self_authinfo,
  pp_self_metadata,
  pp_file_header,
  pp_program_headers,
  pp_dynamic,
  pp_tls
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
   '-a','-all'        :print_param:=[pp_file_size,
                                     pp_self_header,
                                     pp_self_segs,
                                     pp_self_authinfo,
                                     pp_self_metadata,
                                     pp_file_header,
                                     pp_program_headers,
                                     pp_dynamic,
                                     pp_tls];

   '-F','--file_size'      :print_param:=print_param+[pp_file_size      ];
   '-H','--self_header'    :print_param:=print_param+[pp_self_header    ];
   '-S','--self_segs'      :print_param:=print_param+[pp_self_segs      ];
   '-A','--self_authinfo'  :print_param:=print_param+[pp_self_authinfo  ];
   '-M','--self_metadata'  :print_param:=print_param+[pp_self_metadata  ];
   '-h','--file-header'    :print_param:=print_param+[pp_file_header    ];
   '-l','--program-headers':print_param:=print_param+[pp_program_headers];
   '-d','--dynamic'        :print_param:=print_param+[pp_dynamic];
   '-t','--tls'            :print_param:=print_param+[pp_tls];
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

 r:=load_self(FileName,@obj);

 if (r=0) then
 begin
  if (pp_file_size       in print_param) then print_file_size    (@obj);
  if (pp_self_header     in print_param) then print_self_header  (@obj);
  if (pp_self_segs       in print_param) then print_self_segs    (@obj);
  if (pp_self_authinfo   in print_param) then print_self_authinfo(@obj);
  if (pp_self_metadata   in print_param) then print_self_metadata(@obj);
  if (pp_file_header     in print_param) then print_elf_header   (@obj);
  if (pp_program_headers in print_param) then print_elf_phdr     (@obj);

  scan_phdr(@obj);

  if (pp_dynamic         in print_param) then print_elf_dynamic  (@obj);
  if (pp_tls             in print_param) then print_elf_tls      (@obj);
 end else
 begin
  Writeln('Error(',r,') load file:',FileName);
 end;

 free_elf_obj(@obj);

 readln;
end.

