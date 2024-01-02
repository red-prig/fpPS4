unit kern_rtld;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_param,
 kern_thr,
 vnode,
 sys_vm_object,
 vuio,
 elf64,
 kern_authinfo;

const
 AT_NULL        = 0; // Terminates the vector.
 AT_IGNORE      = 1; // Ignored entry.
 AT_EXECFD      = 2; // File descriptor of program to load.
 AT_PHDR        = 3; // Program header of program already loaded.
 AT_PHENT       = 4; // Size of each program header entry.
 AT_PHNUM       = 5; // Number of program header entries.
 AT_PAGESZ      = 6; // Page size in bytes.
 AT_BASE        = 7; // Interpreter's base address.
 AT_FLAGS       = 8; // Flags (unused for i386).
 AT_ENTRY       = 9; // Where interpreter should transfer control.
 AT_NOTELF      =10; // Program is not ELF ??
 AT_UID         =11; // Real uid.
 AT_EUID        =12; // Effective uid.
 AT_GID         =13; // Real gid.
 AT_EGID        =14; // Effective gid.
 AT_EXECPATH    =15; // Path to the executable.
 AT_CANARY      =16; // Canary for SSP
 AT_CANARYLEN   =17; // Length of the canary.
 AT_OSRELDATE   =18; // OSRELDATE.
 AT_NCPUS       =19; // Number of CPUs.
 AT_PAGESIZES   =20; // Pagesizes.
 AT_PAGESIZESLEN=21; // Number of pagesizes.
 AT_TIMEKEEP    =22; // Pointer to timehands.
 AT_STACKPROT   =23; // Initial stack protection.

 AT_COUNT       =24; // Count of defined aux entry types.

 ARG_MAX=262144; // max bytes for an exec function

 ps_arg_cache_limit=$400;

type
 p_ps_strings=^t_ps_strings;
 t_ps_strings=packed record
  ps_argvstr :ppchar;  //first of 0 or more argument string
  ps_nargvstr:Integer; //the number of argument strings
  _align1    :Integer; //
  ps_envstr  :ppchar;  //first of 0 or more environment strings
  ps_nenvstr :Integer; //the number of environment strings
  _align2    :Integer; //
 end;
 {$IF sizeof(t_ps_strings)<>32}{$STOP sizeof(t_ps_strings)<>32}{$ENDIF}

 p_image_args=^t_image_args;
 t_image_args=packed record
  buf        :PChar;   //pointer to string buffer
  begin_argv :PChar;   //beginning of argv in buf
  begin_envv :PChar;   //beginning of envv in buf
  endp       :PChar;   //current `end' pointer of arg & env strings
  fname      :PChar;   //pointer to filename of executable (system space)
  fname_buf  :PChar;   //pointer to optional malloc(M_TEMP) buffer
  stringspace:Integer; //space left in arg & env buffer
  argc       :Integer; //count of argument strings
  envc       :Integer; //count of environment strings
  fd         :Integer; //file descriptor of the executable
 end;

 p_elf64_auxargs=^t_elf64_auxargs;
 t_elf64_auxargs=packed record
  execfd:Int64;
  phdr  :QWORD;
  phent :QWORD;
  phnum :QWORD;
  pagesz:QWORD;
  base  :QWORD;
  flags :QWORD;
  entry :QWORD;
 end;

 p_image_params=^t_image_params;
 t_image_params=packed record
  vp          :p_vnode;
  obj         :vm_object_t;
  attr        :p_vattr;
  image_self  :p_self_header;
  image_header:p_elf64_hdr;
  entry_addr  :Pointer;
  reloc_base  :Pointer;
  opened      :Integer;
  elf_size    :Integer;
  auxargs     :p_elf64_auxargs;
  auxarg_size :QWORD;
  args        :p_image_args;
  execpath    :PChar;
  execpathp   :Pointer;
  freepath    :PChar;
  canary      :Pointer;
  pagesizes   :Pointer;
  canarylen   :Integer;
  pagesizeslen:Integer;

  dyn_vaddr:p_elf64_dyn;

  tls_size     :QWORD;
  tls_align    :QWORD;
  tls_init_size:QWORD;
  tls_init_addr:Pointer;

  eh_frame_hdr_addr:Pointer;
  eh_frame_hdr_size:QWORD;

  authinfo:t_authinfo;

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

  relro_addr:Pointer;
  relro_size:QWORD;

  hdr_e_type:Integer;
 end;

const
 M2MB_NOTDYN_FIXED=0; //Default    =0     (ATTRIBUTE2:0x00000)
 M2MB_DISABLE     =1; //NotUsed    =32768 (ATTRIBUTE2:0x08000)
 M2MB_READONLY    =2; //Text_rodata=65536 (ATTRIBUTE2:0x10000)
 M2MB_ENABLE      =3; //All_section=98304 (ATTRIBUTE2:0x18000)

function  maxInt64(a,b:Int64):Int64; inline;
function  minInt64(a,b:Int64):Int64; inline;

function  get_elf_phdr(elf_hdr:p_elf64_hdr):p_elf64_phdr; inline;

procedure rtld_free_self(imgp:p_image_params);
function  rtld_load_self(imgp:p_image_params):Integer;
procedure rtld_load_auth(imgp:p_image_params);

function  is_used_mode_2mb(phdr:p_elf64_phdr;is_dynlib,budget_ptype:Integer):Boolean;

function  rtld_dirname(path,bname:pchar):Integer;
function  rtld_file_exists(path:pchar):Boolean;

function  convert_prot(flags:Elf64_Word):Byte;

function  rtld_mmap  (addr:PQWORD;size:QWORD):Integer;
procedure rtld_munmap(base:Pointer;size:QWORD);

function  scan_phdr(imgp:p_image_params;phdr:p_elf64_phdr;count:Integer):Integer;

function  elf64_get_eh_frame_info(hdr:p_eh_frame_hdr;
                                  hdr_size :QWORD;
                                  hdr_vaddr:QWORD;
                                  data_size:QWORD;
                                  eh_frame_addr:PPointer;
                                  eh_frame_size:PQWORD):Integer;


function  scan_dyn_offset(imgp:p_image_params;phdr:p_elf64_phdr;count:Integer):Integer;

function  self_load_section(imgp:p_image_params;
                            id,vaddr,offset,memsz,filesz:QWORD;
                            prot:Byte;
                            use_mode_2mb:Boolean;
                            name:pchar;
                            var cache:Pointer):Integer;

function  is_system_path(path:pchar):Boolean;
function  is_libc_or_fios(path:pchar):Boolean;
function  dynlib_basename(path:pchar):pchar;

implementation

uses
 errno,
 systm,
 vnamei,
 vfs_lookup,
 vfs_subr,
 vnode_if,
 vm,
 vmparam,
 vm_map,
 vm_mmap,
 md_map,
 kern_proc,
 kern_budget,
 kern_patcher;

function maxInt64(a,b:Int64):Int64; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function minInt64(a,b:Int64):Int64; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function get_elf_phdr(elf_hdr:p_elf64_hdr):p_elf64_phdr; inline;
begin
 Result:=Pointer(elf_hdr+1);
end;

function get_elf_phdr_offset(elf_hdr:p_elf64_hdr):Int64; inline;
begin
 Result:=SizeOf(elf64_hdr);
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

function kread(vp:p_vnode;buf:Pointer;nbyte,offset:Integer):Integer;
var
 uio:t_uio;
 aio:iovec;
begin
 uio:=Default(t_uio);
 aio:=Default(iovec);
 //
 aio.iov_base  :=buf;
 aio.iov_len   :=nbyte;
 //
 uio.uio_iov   :=@aio;
 uio.uio_iovcnt:=1;
 uio.uio_offset:=offset;
 uio.uio_segflg:=UIO_SYSSPACE;
 uio.uio_rw    :=UIO_READ;
 uio.uio_resid :=nbyte;
 uio.uio_td    :=curkthread;
 //
 Result:=VOP_READ(vp,@uio,0);

 if (Result=0) then
 begin
  if (uio.uio_resid<>0) then
  begin
   Result:=ENOEXEC;
  end;
 end;
end;

procedure rtld_free_self(imgp:p_image_params);
begin
 FreeMem(imgp^.image_header);
 FreeMem(imgp^.image_self);
 imgp^.image_header:=nil;
 imgp^.image_self:=nil;
 imgp^.elf_size:=0;
end;

function rtld_load_self(imgp:p_image_params):Integer;
Var
 vp:p_vnode;
 obj_size:Int64;
 n,s:Int64;
 Magic:DWORD;
 i,count:Integer;
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

 if (imgp=nil) then Exit(EINVAL);

 vp:=imgp^.vp;
 obj_size:=imgp^.attr^.va_size;

 if (obj_size=0) then Exit(ENOEXEC);

 Result:=kread(vp,@Magic,SizeOf(DWORD),0);
 if (Result<>0) then Exit;

 case Magic of
  ELFMAG: //elf64
    begin
      elf_hdr:=AllocMem(obj_size);

      Result:=kread(vp,elf_hdr,obj_size,0);
      if (Result<>0) then
      begin
       FreeMem(elf_hdr);
       Exit;
      end;

      imgp^.image_header:=elf_hdr;
      imgp^.image_self  :=nil;
      imgp^.elf_size    :=obj_size;
    end;
  SELF_MAGIC: //self
    begin
      self_hdr:=AllocMem(obj_size);

      Result:=kread(vp,self_hdr,obj_size,0);
      if (Result<>0) then
      begin
       FreeMem(self_hdr);
       Exit;
      end;

      if (self_hdr^.File_size>obj_size) then
      begin
       FreeMem(self_hdr);
       Exit(EFAULT);
      end;

      count:=self_hdr^.Num_Segments;

      if (count=0) then
      begin
       FreeMem(self_hdr);
       Exit(ENOEXEC);
      end;

      self_segs:=Pointer(self_hdr+1);

      For i:=0 to count-1 do
       if ((self_segs[i].flags and (SELF_PROPS_ENCRYPTED or SELF_PROPS_COMPRESSED))<>0) then
       begin
        Writeln(StdErr,'exec_load_self:',imgp^.execpath,'is encrypted!');
        FreeMem(self_hdr);
        Exit(ENOEXEC);
       end;

      elf_hdr:=Pointer(self_segs)+(count*SizeOf(t_self_segment));

      elf_phdr:=get_elf_phdr(elf_hdr);

      MinSeg:=High(Int64);
      MaxSeg:=0;

      count:=self_hdr^.Num_Segments;

      For i:=0 to count-1 do
       if ((self_segs[i].flags and SELF_PROPS_BLOCKED)<>0) then
       begin
        s:=SELF_SEGMENT_INDEX(self_segs[i].flags);
        s:=elf_phdr[s].p_offset;
        MinSeg:=MinInt64(s,MinSeg);
        s:=s+minInt64(self_segs[i].filesz,self_segs[i].memsz);
        MaxSeg:=MaxInt64(s,MaxSeg);
       end;

      if (MinSeg>MaxSeg) then
      begin
       FreeMem(self_hdr);
       Exit(EFAULT);
      end;

      imgp^.image_header:=AllocMem(MaxSeg);
      imgp^.elf_size    :=MaxSeg;

      //elf_hdr part
      n:=ptruint(elf_hdr)-ptruint(self_hdr);        //offset to hdr
      s:=self_hdr^.Header_Size+self_hdr^.Meta_size; //offset to end
      s:=MinInt64(obj_size,s);                      //min size
      s:=MaxInt64(s,n)-n;                           //get size

      //first page
      Move(elf_hdr^,imgp^.image_header^,s);

      count:=self_hdr^.Num_Segments;

      For i:=0 to count-1 do
       if ((self_segs[i].flags and SELF_PROPS_BLOCKED)<>0) then
       begin
        s:=SELF_SEGMENT_INDEX(self_segs[i].flags);

        mem_size:=minInt64(self_segs[i].filesz,self_segs[i].memsz);

        src_ofs:=self_segs[i].offset;  //start offset
        dst_ofs:=elf_phdr[s].p_offset; //start offset

        fixup_offset_size(src_ofs,mem_size,obj_size);
        fixup_offset_size(dst_ofs,mem_size,MaxSeg);

        if (src_ofs>=obj_size) then
        begin
         Assert(false,'src_ofs>=obj_size');
        end;

        if ((src_ofs+mem_size)>obj_size) then
        begin
         Assert(false,'(src_ofs+mem_size)>obj_size');
        end;

        if (dst_ofs>=MaxSeg) then
        begin
         Assert(false,'dst_ofs>=MaxSeg');
        end;

        if ((dst_ofs+mem_size)>MaxSeg) then
        begin
         Assert(false,'(dst_ofs+mem_size)>MaxSeg');
        end;

        Move( (Pointer(self_hdr)          +src_ofs)^, //src
              (Pointer(imgp^.image_header)+dst_ofs)^, //dst
              mem_size);                              //size
       end;

      imgp^.image_self:=self_hdr;
    end;
  else
    begin
     Exit(ENOEXEC);
    end;
 end;

end;

procedure rtld_load_auth(imgp:p_image_params);
var
 hdr:p_elf64_hdr;
 authinfo:p_self_authinfo;
 s:ptruint;
begin
 if (imgp=nil) then Exit;

 imgp^.authinfo:=Default(t_authinfo);
 imgp^.authinfo.app_caps [0]:=QWORD($2000000000000000); //IsGame
 imgp^.authinfo.app_attrs[0]:=$400000 or $800000; //allow dmem map

 if (imgp^.image_header=nil) or
    (imgp^.image_self  =nil) then
 begin
  case ExtractFileExt(imgp^.execpath) of
   '.sprx','.prx' :imgp^.authinfo.app_type:=QWORD($3900000000000002);
   '.sdll','.sexe':imgp^.authinfo.app_type:=QWORD($3901000000000001);
   else
                   imgp^.authinfo.app_type:=QWORD($3100000000000001);
  end;
  Exit;
 end;

 hdr:=imgp^.image_header;
 s:=SizeOf(t_self_header);
 s:=s+(imgp^.image_self^.Num_Segments*SizeOf(t_self_segment));
 s:=s+get_elf_phdr_offset(hdr);
 s:=s+(hdr^.e_phnum*SizeOf(elf64_phdr));
 s:=AlignUp(s,SELF_SEGMENT_BLOCK_ALIGNMENT);

 authinfo:=Pointer(Pointer(imgp^.image_self)+s);

 imgp^.authinfo.app_type:=authinfo^.AuthorityID;
end;

function is_used_mode_2mb(phdr:p_elf64_phdr;is_dynlib,budget_ptype:Integer):Boolean;
var
 flag_write:Integer;
begin
 Result:=False;

 if (budget_ptype=PTYPE_BIG_APP) then
 begin
  flag_write:=2;
  if (phdr^.p_type<>PT_SCE_RELRO) then
  begin
   flag_write:=phdr^.p_flags and 2;
  end;

  case p_proc.p_mode_2mb of
   M2MB_NOTDYN_FIXED:Result:=(is_dynlib=0) and (p_proc.p_self_fixed<>0);
   M2MB_READONLY    :Result:=(flag_write=0);
   M2MB_ENABLE      :Result:=True;
   else;
  end;

 end;
end;

function get_char_sep(path:pchar):char; inline;
const
 c_host='/host/';
begin
 if (StrLComp(path,c_host,Length(c_host))=0) then
 begin
  Result:='\';
 end else
 begin
  Result:='/';
 end;
end;

function rtld_dirname(path,bname:pchar):Integer;
var
 endp:pchar;
 chr:char;
begin
 Result:=0;

 { Empty or NULL string gets treated as "." }
 if (path=nil) or (path^=#0) then
 begin
  bname[0]:='.';
  bname[1]:=#0;
  Exit(0);
 end;

 chr:=get_char_sep(path);

 { Strip trailing slashes }
 endp:=path + strlen(path) - 1;
 while (endp > path) and (endp^=chr) do Dec(endp);

 { Find the start of the dir }
 while (endp > path) and (endp^<>chr) do Dec(endp);

 { Either the dir is "/" or there are no slashes }
 if (endp=path) then
 begin
  if (endp^=chr) then
  begin
   bname[0]:='/';
  end else
  begin
   bname[0]:='.';
  end;
  bname[1]:=#0;
  Exit(0);
 end else
 begin
  repeat
   Dec(endp);
  until not ((endp > path) and (endp^=chr));
 end;

 if ((endp - path + 2) > PATH_MAX) then
 begin
  Writeln(StdErr,'Filename is too long:',path);
  Exit(-1);
 end;

 Move(path^, bname^, endp - path + 1);
 bname[endp - path + 1]:=#0;

 Result:=0;
end;

function rtld_file_exists(path:pchar):Boolean;
var
 nd:t_nameidata;
 error:Integer;
begin
 Result:=False;
 if (path=nil) then Exit;

 NDINIT(@nd,LOOKUP,LOCKLEAF or FOLLOW or SAVENAME or MPSAFE, UIO_SYSSPACE, path, curkthread);

 error:=nd_namei(@nd);

 if (error=0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  vput(nd.ni_vp);
  Exit(True);
 end;

 NDFREE(@nd, NDF_ONLY_PNBUF);
end;

function convert_prot(flags:Elf64_Word):Byte;
begin
 Result:=0;
 if ((flags and PF_X)<>0) then Result:=Result or VM_PROT_EXECUTE;
 if ((flags and PF_W)<>0) then Result:=Result or VM_PROT_WRITE;
 if ((flags and PF_R)<>0) then Result:=Result or VM_PROT_READ;
end;

function rtld_mmap(addr:PQWORD;size:QWORD):Integer;
var
 map:vm_map_t;
begin
 map:=p_proc.p_vmspace;

 if (p_proc.p_sce_replay_exec<>0) then
 begin
  addr^:=SCE_REPLAY_EXEC_START;
 end;

 Result:=vm_mmap2(map,addr,size,0,0,MAP_ANON or MAP_PRIVATE or (21 shl MAP_ALIGNMENT_BIT),OBJT_DEFAULT,nil,0);
end;

procedure rtld_munmap(base:Pointer;size:QWORD);
var
 map:vm_map_t;
begin
 if (base<>nil) and (size<>0) then
 begin
  map:=p_proc.p_vmspace;
  //
  vm_map_remove(map,QWORD(base),QWORD(base) + size);
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
 if (imgp=nil) then Exit(EINVAL);
 if (phdr=nil) then Exit(EINVAL);
 if (count=0)  then Exit(EINVAL);

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
       Writeln(StdErr,'scan_phdr:',imgp^.execpath,'segment #',i,' is not page aligned');
       Exit(ENOEXEC);
      end;

      memsz:=phdr[i].p_memsz;

      if (memsz<=phdr[i].p_filesz) and (phdr[i].p_filesz<>memsz) then
      begin
       Exit(ENOEXEC);
      end;

      if (memsz > $7fffffff) then
      begin
       Exit(ENOEXEC);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Exit(ENOEXEC);
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
       Exit(ENOEXEC);
      end;

      if (memsz > $7fffffff) then
      begin
       Exit(ENOEXEC);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Exit(ENOEXEC);
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
       Exit(ENOEXEC);
      end;

      if (memsz > $7fffffff) then
      begin
       Exit(ENOEXEC);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Exit(ENOEXEC);
      end;

      if (phdr[i].p_align > 32) then
      begin
       Writeln(StdErr,'scan_phdr:',imgp^.execpath,'alignment of segment #',i,' it must be less than 32.');
       Exit(ENOEXEC);
      end;
     end;

   PT_SCE_DYNLIBDATA:
     begin
      imgp^.sce_dynlib_data_id  :=i;
      imgp^.sce_dynlib_data_addr:=phdr[i].p_offset;
      imgp^.sce_dynlib_data_size:=phdr[i].p_filesz;

      if (phdr[i].p_memsz<>0) then
      begin
       Exit(ENOEXEC);
      end;

      if (phdr[i].p_filesz > $7fffffff) then
      begin
       Exit(ENOEXEC);
      end;

      if ((phdr[i].p_offset shr $20)<>0) then
      begin
       Exit(ENOEXEC);
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
      Exit(ENOEXEC);
     end;

     if (memsz > $7fffffff) then
     begin
      Exit(ENOEXEC);
     end;

     if ((phdr[i].p_offset shr $20)<>0) then
     begin
      Exit(ENOEXEC);
     end;
    end;

   PT_SCE_COMMENT:
    begin
     imgp^.sce_comment_id    :=i;
     imgp^.sce_comment_offset:=phdr[i].p_offset;
     imgp^.sce_comment_filesz:=phdr[i].p_filesz;

     if (phdr[i].p_memsz<>0) then
     begin
      Exit(ENOEXEC);
     end;

     if (phdr[i].p_filesz > $7fffffff) then
     begin
      Exit(ENOEXEC);
     end;

     if ((phdr[i].p_offset shr $20)<>0) then
     begin
      Exit(ENOEXEC);
     end;
    end;

  end;

 end;

 if (imgp^.min_addr=High(Int64)) then
 begin
  Exit(EINVAL);
 end;

 if (imgp^.max_addr=0) then
 begin
  Exit(EINVAL);
 end;

 if (imgp^.dyn_exist<>0) then
 begin
  if (imgp^.sce_dynlib_data_size=0) then
  begin
   Exit(EINVAL);
  end;

  if (imgp^.dyn_filesz=0) then
  begin
   Exit(EINVAL);
  end;
 end;

 if (sce_relro_id<>-1) then
 begin
  vaddr:=phdr[sce_relro_id].p_vaddr;

  if (vaddr=0) then
  begin
   Exit(EINVAL);
  end;

  memsz:=phdr[sce_relro_id].p_memsz;

  if (memsz=0) then
  begin
   Exit(EINVAL);
  end;

  if (((phdr[text_id].p_vaddr+phdr[text_id].p_memsz+$1fffff) and QWORD($ffffffffffe00000))<>vaddr) and
     (((phdr[text_id].p_vaddr+phdr[text_id].p_memsz+$003fff) and QWORD($ffffffffffffc000))<>vaddr) then
  begin
   Exit(EINVAL);
  end;

  if (((vaddr+memsz+$1fffff) and QWORD($ffffffffffe00000))<>phdr[data_id].p_vaddr) and
     (((vaddr+memsz+$003fff) and QWORD($ffffffffffffc000))<>phdr[data_id].p_vaddr) then
  begin
   Exit(EINVAL);
  end;
 end;

 Result:=0;
end;

function elf64_get_eh_frame_info(hdr:p_eh_frame_hdr;
                                 hdr_size :QWORD;
                                 hdr_vaddr:QWORD;
                                 data_size:QWORD;
                                 eh_frame_addr:PPointer;
                                 eh_frame_size:PQWORD):Integer;
label
 __result;
var
 ret1:Integer;
 h,res,pos:PByte;
 enc:Byte;
 offset:QWORD;
 size:QWORD;
 fde_count:DWORD;
 _end:DWORD;
begin
 enc:=0;
 ret1:=copyin(@hdr^.eh_frame_ptr_enc,@enc,1);
 if (ret1<>0) then Exit(-1);

 h:=Pointer(hdr + 1);

 offset:=0;
 res:=nil;
 case enc of
  DW_EH_PE_udata4:
    begin
     ret1:=copyin(h,@offset,4);
     if (ret1<>0) then Exit(-1);

     res:=Pointer(Integer(offset) + hdr_vaddr);
    end;
  DW_EH_PE_pcrel or DW_EH_PE_sdata4:
    begin
     ret1:=copyin(h,@offset,4);
     if (ret1<>0) then Exit(-1);

     res:=h + Integer(offset);
    end;
  else
    Exit(-1)
 end;

 size:=0;
 if (res=nil) then
 begin
   __result:
   eh_frame_addr^:=res;
   eh_frame_size^:=size;
  Exit(0);
 end;

 fde_count:=0;
 ret1:=copyin(res,@fde_count,4);
 if (ret1<>0) then Exit(-1);

 pos:=res;
 size:=0;

 repeat

  offset:=fde_count;
  if (offset=$ffffffff) then
  begin
   ret1:=copyin(pos + 4,@offset,8);
   if (ret1<>0) then break;
   offset:=offset + 12;
  end else
  begin
   if (fde_count=0) then
   begin
    size:=size + 4;
    goto __result;
   end;
   offset:=offset + 4;
  end;

  _end:=offset + size;

  if (data_size <= (QWORD(res) + _end)) then goto __result;
  pos:=pos + offset;

  ret1:=copyin(pos,@fde_count,4);
  size:=_end;

 until (ret1<>0);

 Result:=-1;
end;

function scan_dyn_offset(imgp:p_image_params;phdr:p_elf64_phdr;count:Integer):Integer;
var
 p_offset:QWORD;
 p_filesz:QWORD;
 i:Integer;
begin
 Result:=0;

 if (imgp^.dyn_id=-1) then Exit;

 p_offset:=phdr[imgp^.dyn_id].p_offset;
 p_filesz:=phdr[imgp^.dyn_id].p_filesz;

 if (count<>0) then
 For i:=0 to count-1 do
 begin

  if (phdr[i].p_offset <= p_offset) and
     (imgp^.dyn_id<>i) and
     ( (p_filesz + p_offset) <= (phdr[i].p_offset + phdr[i].p_filesz)) then
  begin
   if (i<>-1) then
   begin
    imgp^.dyn_id    :=i;
    imgp^.dyn_offset:=imgp^.dyn_offset - phdr[i].p_offset;
    Exit(0);
   end;
   break;
  end;

 end;

 Result:=EINVAL;
end;

function self_load_section(imgp:p_image_params;
                           id,vaddr,offset,memsz,filesz:QWORD;
                           prot:Byte;
                           use_mode_2mb:Boolean;
                           name:pchar;
                           var cache:Pointer):Integer;
var
 map:vm_map_t;
 vaddr_lo:QWORD;
 vaddr_hi:QWORD;
 base    :Pointer;
begin
 Result:=0;

 if (memsz<filesz) then
 begin
  Writeln(StdErr,'[KERNEL] self_load_section: memsz',HexStr(memsz,8),') < filesz(',HexStr(filesz,8),') at segment ',id);
  Exit(ENOEXEC);
 end;

 if ((prot and 6)=6) then
 begin
  Writeln(StdErr,'[KERNEL] self_load_section: writeable text segment ',id,', ',HexStr(vaddr,8));
  Exit(ENOEXEC);
 end;

 if ((vaddr and $3fff)<>0) then
 begin
  Writeln(StdErr,'[KERNEL] self_load_section: non-aligned segment ',id,', ',HexStr(vaddr,8));
  Exit(ENOEXEC);
 end;

 vaddr_lo:=vaddr and $ffffffffffffc000;
 vaddr_hi:=(memsz + vaddr + $3fff) and $ffffffffffffc000;

 if (use_mode_2mb) then
 begin
  vaddr_lo:=(vaddr + $1fffff) and $ffffffffffe00000;
  vaddr_hi:=(vaddr + memsz + $3fff) and $ffffffffffe00000;
 end;

 base:=Pointer(imgp^.image_header)+offset;

 map:=p_proc.p_vmspace;

 vm_map_lock(map);

 //remove prev if exist
 vm_map_delete(map,vaddr_lo,vaddr_hi,True);

 Result:=vm_map_insert(map,imgp^.obj,offset,vaddr_lo,vaddr_hi,VM_PROT_RW,prot or VM_PROT_RW,0,false);
 if (Result<>0) then
 begin
  vm_map_unlock(map);
  //
  Writeln(StdErr,'[KERNEL] self_load_section: vm_map_insert failed ',id,', ',HexStr(vaddr,8));
  Exit(vm_mmap_to_errno(Result));
 end;

 vm_map_set_name_locked(map,vaddr_lo,vaddr_hi,name);

 memsz:=vaddr_hi-vaddr_lo;
 cache:=ReAllocMem(cache,memsz);

 if ((prot and VM_PROT_EXECUTE)<>0) then
 begin
  FillChar(cache^,memsz,$90);
 end else
 begin
  FillChar(cache^,memsz,0);
 end;

 Move(base^,cache^,filesz);

 if ((prot and VM_PROT_EXECUTE)<>0) then
 begin
  Writeln('P_X:vaddr=0x',HexStr(vaddr,12),' offset=0x',HexStr(offset,6),' memsz=0x',HexStr(memsz,6),' filesz=0x',HexStr(filesz,6));
  patcher_process_section(imgp^.obj,cache,Pointer(vaddr_lo),filesz);
 end;

 Result:=copyout(cache,Pointer(vaddr_lo),memsz);
 if (Result<>0) then
 begin
  vm_map_unlock(map);
  //
  Writeln(StdErr,'[KERNEL] self_load_section: copyout failed ',
    id,', ',HexStr(base),'->',HexStr(vaddr_lo,8),':',HexStr(memsz,8));
  readln;
  Exit;
 end;

 Result:=vm_map_protect(map,vaddr_lo,vaddr_hi,prot,False);
 if (Result<>0) then
 begin
  vm_map_unlock(map);
  //
  Writeln(StdErr,'[KERNEL] self_load_section: vm_map_protect failed ',id,', ',HexStr(vaddr,8));
  Exit(vm_mmap_to_errno(Result));
 end;

 if ((prot and VM_PROT_EXECUTE)<>0) then
 begin
  md_cacheflush(Pointer(vaddr_lo),memsz,ICACHE);
 end;

 vm_map_unlock(map);
end;

function is_system_path(path:pchar):Boolean;
begin
 if (path=nil)     then Exit(False);
 if (path[0]<>'/') then Exit(False);
 Result:=StrLComp(p_proc.p_randomized_path,@path[1],Length(p_proc.p_randomized_path))=0;
end;

function dynlib_basename(path:pchar):pchar;
var
 idx:pchar;
 chr:char;
begin
 if (path=nil) then Exit(nil);

 chr:=get_char_sep(path);

 idx:=strrscan(path,chr);
 if (idx=nil) then
 begin
  Result:=path;
 end else
 begin
  Result:=idx+1;
 end;
end;

function is_libc_or_fios(path:pchar):Boolean;
const
 c_libc='libc.';
 c_libSceFios2='libSceFios2.';
var
 f:pchar;
begin
 f:=dynlib_basename(path);

 if (StrLComp(f,c_libc,Length(c_libc))=0) or
    (StrLComp(f,c_libSceFios2,Length(c_libSceFios2))=0) then
 begin
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;


end.

