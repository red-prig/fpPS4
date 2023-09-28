unit kern_exec;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 kern_thr,
 vnode,
 vuio,
 vcapability,
 elf64,
 kern_rtld,
 subr_dynlib;

function  exec_alloc_args(args:p_image_args):Integer;
procedure exec_free_args(args:p_image_args);

function  exec_copyin_args(args:p_image_args;
                           fname:pchar;
                           segflg:uio_seg;
                           argv:ppchar;
                           envv:ppchar):Integer;

function  kern_execve(td:p_kthread;args:p_image_args):Integer;
function  sys_execve(fname:pchar;argv,envv:ppchar):Integer;

implementation

uses
 systm,
 errno,
 kern_proc,
 kern_mtx,
 vm,
 vmparam,
 vm_map,
 vm_mmap,
 vm_object,
 vm_pager,
 vnamei,
 vfs_lookup,
 vmount,
 vfile,
 vstat,
 vfcntl,
 vfs_vnops,
 vfs_subr,
 kern_descrip,
 vfs_cache,
 vnode_if,
 _resource,
 kern_resource,
 sys_event,
 kern_event,
 machdep,
 kern_dlsym,
 kern_authinfo,
 vfs_syscalls,
 kern_jit_dynamic;

function exec_alloc_args(args:p_image_args):Integer;
begin
 args^.buf:=AllocMem(PATH_MAX + ARG_MAX);
 if (args^.buf=nil) then Exit(ENOMEM);
 Result:=0;
end;

procedure exec_free_args(args:p_image_args);
begin
 if (args^.buf<>nil) then
 begin
  FreeMem(args^.buf);
  args^.buf:=nil;
 end;
 if (args^.fname_buf<>nil) then
 begin
  FreeMem(args^.fname_buf);
  args^.fname_buf:=nil;
 end;
end;

{
 * Copy out argument and environment strings from the old process address
 * space into the temporary string buffer.
 }
function exec_copyin_args(args:p_image_args;
                          fname:pchar;
                          segflg:uio_seg;
                          argv:ppchar;
                          envv:ppchar):Integer;
label
 err_exit;
var
 argp,envp:pchar;
 error:Integer;
 length:int64;

 function f_fuword_argv:Boolean; inline;
 begin
  argp:=fuword(argv^);
  Inc(argv);
  Result:=(argp<>nil);
 end;

 function f_fuword_envv:Boolean; inline;
 begin
  envp:=fuword(envv^);
  Inc(envv);
  Result:=(envp<>nil);
 end;

begin
 args^:=Default(t_image_args);

 if (argv=nil) then
 begin
  Exit(EFAULT);
 end;

 {
  * Allocate demand-paged memory for the file name, argument, and
  * environment strings.
  }
 error:=exec_alloc_args(args);
 if (error<>0) then
 begin
  Exit(error);
 end;

 {
  * Copy the file name.
  }
 if (fname<>nil) then
 begin
  args^.fname:=args^.buf;

  if (segflg=UIO_SYSSPACE) then
  begin
   error:=copystr  (fname, args^.fname, PATH_MAX, @length);
  end else
  begin
   error:=copyinstr(fname, args^.fname, PATH_MAX, @length);
  end;

  if (error<>0) then goto err_exit;
 end else
 begin
  length:=0;
 end;

 args^.begin_argv:=args^.buf + length;
 args^.endp:=args^.begin_argv;
 args^.stringspace:=ARG_MAX;

 {
  * extract arguments first
  }
 while (f_fuword_argv) do
 begin
  if (argp=Pointer(QWORD(-1))) then
  begin
   error:=EFAULT;
   goto err_exit;
  end;
  error:=copyinstr(argp, args^.endp, args^.stringspace, @length);
  if (error<>0) then
  begin
   if (error=ENAMETOOLONG) then
   begin
    error:=E2BIG;
   end;
   goto err_exit;
  end;
  Dec(args^.stringspace,length);
  Inc(args^.endp       ,length);
  Inc(args^.argc);
 end;

 args^.begin_envv:=args^.endp;

 {
  * extract environment strings
  }
 if (envv<>nil) then
 begin
  while (f_fuword_envv) do
  begin
   if (envp=Pointer(QWORD(-1))) then
   begin
    error:=EFAULT;
    goto err_exit;
   end;
   error:=copyinstr(envp, args^.endp, args^.stringspace, @length);
   if (error<>0) then
   begin
    if (error=ENAMETOOLONG) then
    begin
     error:=E2BIG;
    end;
    goto err_exit;
   end;
   Dec(args^.stringspace,length);
   Inc(args^.endp       ,length);
   Inc(args^.envc);
  end;
 end;

 Exit(0);

err_exit:
 exec_free_args(args);
 Exit(error);
end;


{
 * Destroy old address space, and allocate a new stack
 * The new stack is only SGROWSIZ large because it is grown
 * automatically in trap.c.
 }
function exec_new_vmspace(imgp:p_image_params):Integer;
var
 error:Integer;
 vmspace:p_vmspace;
 map:vm_map_t;
 //obj:vm_object_t;
 sv_minuser:QWORD;
 sv_maxuser:QWORD;
 stack_addr:QWORD;
 ssiz:QWORD;
begin
 vmspace:=@g_vmspace;

 { May be called with Giant held }
 //EVENTHANDLER_INVOKE(process_exec, p, imgp);

 {
  * Blow away entire process VM, if address space not shared,
  * otherwise, create a new VM space so that other threads are
  * not disrupted
  }
 map:=@vmspace^.vm_map;

 sv_minuser:=pmap_mem[0].start;
 sv_maxuser:=VM_MAXUSER_ADDRESS;

 if (vm_map_min(map)=sv_minuser) and
    (vm_map_max(map)=sv_maxuser) then
 begin
  //shmexit(vmspace);
  //pmap_remove_pages(vmspace_pmap(vmspace));
  vm_map_remove(map, vm_map_min(map), vm_map_max(map));
 end else
 begin
  error:=vmspace_exec(sv_minuser, sv_maxuser);
  if (error<>0) then Exit(error);
 end;

 //calc shared page addres
 vmspace^.sv_usrstack:=Pointer(USRSTACK {- (aslr_offset and $ffc000)});

 { Map a shared page }

 //mapping shared page (sv_usrstack_len=0x4000)
 error:=vm_map_fixed(map,nil,0,
         QWORD(vmspace^.sv_usrstack), PAGE_SIZE,
         VM_PROT_RW,
         VM_PROT_ALL,
         MAP_INHERIT_SHARE or MAP_ACC_NO_CHARGE,
         0);

 if (error<>0) then
 begin
  Exit(error);
 end;

 {
 obj:=sv^.sv_shared_page_obj;
 if (obj<>nil) then
 begin
  vm_object_reference(obj);
  error:=vm_map_fixed(map, obj, 0,
      sv^.sv_shared_page_base, sv^.sv_shared_page_len,
      VM_PROT_READ or VM_PROT_EXECUTE,
      VM_PROT_READ or VM_PROT_EXECUTE,
      MAP_INHERIT_SHARE or MAP_ACC_NO_CHARGE);

  if (error<>0) then
  begin
   vm_object_deallocate(obj);
   Exit(error);
  end;
 end;
 }

 ssiz:=MAXSSIZ;

 stack_addr:=QWORD(vmspace^.sv_usrstack) - ssiz;

 error:=vm_map_stack(map,stack_addr,ssiz,VM_PROT_RW,VM_PROT_ALL,MAP_STACK_GROWS_DOWN);

 if (error<>0) then
 begin
  Exit(error);
 end;

 vm_map_set_name(map,stack_addr,QWORD(vmspace^.sv_usrstack),'main stack');

 { vm_ssize and vm_maxsaddr are somewhat antiquated concepts in the
  * VM_STACK case, but they are still used to monitor the size of the
  * process stack so we can check the stack rlimit.
  }
 vmspace^.vm_ssize   :=sgrowsiz shr PAGE_SHIFT;
 vmspace^.vm_maxsaddr:=vmspace^.sv_usrstack - ssiz;

 Exit(0);
end;


function exec_copyout_strings(imgp:p_image_params):PQWORD;
var
 vms:p_vmspace;
 argc,envc:Integer;
 vectp:ppchar;
 stringp:pchar;
 destp:Pointer;
 arginfo:p_ps_strings;
 stack_base:PQWORD;
 execpath_len:QWORD;
 canary:array[0..7] of QWORD;
begin
 {
  * Calculate string base and vector table pointers.
  * Also deal with signal trampoline code for this exec type.
  }
 if (imgp^.execpath<>nil) and (imgp^.auxargs<>nil) then
  execpath_len:=strlen(imgp^.execpath) + 1
 else
  execpath_len:=0;

 vms:=@g_vmspace;

 vms^.ps_strings:=(vms^.sv_usrstack-SizeOf(t_ps_strings));
 arginfo:=vms^.ps_strings;
 destp:=Pointer(arginfo);

 {
  * Copy the image path for the rtld.
  }
 if (execpath_len<>0) then
 begin
  Dec(destp,execpath_len);
  imgp^.execpathp:=destp;
  copyout(imgp^.execpath,destp,execpath_len);
 end;

 {
  * Prepare the canary for SSP.
  }
 //arc4rand(canary, sizeof(canary), 0);

 canary[0]:=QWORD($FEEDBABEFEEDBABE);
 canary[1]:=QWORD($FEEDBABEFEEDBABE);
 canary[2]:=QWORD($FEEDBABEFEEDBABE);
 canary[3]:=QWORD($FEEDBABEFEEDBABE);
 canary[4]:=QWORD($FEEDBABEFEEDBABE);
 canary[5]:=QWORD($FEEDBABEFEEDBABE);
 canary[6]:=QWORD($FEEDBABEFEEDBABE);
 canary[7]:=QWORD($FEEDBABEFEEDBABE);

 Dec(destp,sizeof(canary));
 imgp^.canary:=destp;
 copyout(@canary, destp, sizeof(canary));
 imgp^.canarylen:=sizeof(canary);

 {
  * Prepare the pagesizes array.
  }
 Dec(destp,sizeof(pagesizes));
 destp:=AlignUp(destp,8);

 imgp^.pagesizes:=destp;
 copyout(@pagesizes, destp, sizeof(pagesizes));
 imgp^.pagesizeslen:=Length(pagesizes);

 Dec(destp,ARG_MAX-imgp^.args^.stringspace);
 destp:=AlignUp(destp,8);

 {
  * If we have a valid auxargs ptr, prepare some room
  * on the stack.
  }
 if (imgp^.auxargs<>nil) then
 begin
  {
   * 'AT_COUNT*2' is size for the ELF Auxargs data. This is for
   * lower compatibility.
   }
  if (imgp^.auxarg_size<>0) then
  begin
   imgp^.auxarg_size:=imgp^.auxarg_size;
  end else
  begin
   imgp^.auxarg_size:=(AT_COUNT * 2);
  end;
  {
   * The '+ 2' is for the nil pointers at the end of each of
   * the arg and env vector sets,and imgp^.auxarg_size is room
   * for argument of Runtime loader.
   }
  vectp:=(destp - (imgp^.args^.argc + imgp^.args^.envc + 2 + imgp^.auxarg_size) * sizeof(Pointer));
 end else
 begin
  {
   * The '+ 2' is for the nil pointers at the end of each of
   * the arg and env vector sets
   }
  vectp:=(destp - (imgp^.args^.argc + imgp^.args^.envc + 2) * sizeof(Pointer));
 end;

 {
  * vectp also becomes our initial stack base
  }
 stack_base:=Pointer(vectp);

 stringp:=imgp^.args^.begin_argv;
    argc:=imgp^.args^.argc;
    envc:=imgp^.args^.envc;

 {
  * Copy out strings - arguments and environment.
  }
 copyout(stringp, destp, ARG_MAX - imgp^.args^.stringspace);

 {
  * Fill in "ps_strings" struct for ps, w, etc.
  }
 suword(arginfo^.ps_argvstr^, vectp);
 suword32(PDWORD(@arginfo^.ps_nargvstr)^, argc);

 {
  * Fill in argument portion of vector table.
  }
 while (argc>0) do
 begin
  suword(vectp^, destp);
  Inc(vectp);

  while (stringp^<>#0) do
  begin
   Inc(stringp);
   Inc(destp);
  end;
  Inc(stringp);

  Inc(destp);
  Dec(argc);
 end;

 { a nil vector table pointer separates the argp's from the envp's }
 suword(vectp^, nil);
 Inc(vectp);

 suword(arginfo^.ps_envstr^, vectp);
 suword32(PDWORD(@arginfo^.ps_nenvstr)^, envc);

 {
  * Fill in environment portion of vector table.
  }
 while (envc>0) do
 begin
  suword(vectp^, destp);
  Inc(vectp);

  while (stringp^<>#0) do
  begin
   Inc(stringp);
   Inc(destp);
  end;
  Inc(stringp);

  Inc(destp);
  Dec(envc);
 end;

 { end of vector table is a nil pointer }
 suword(vectp^, nil);

 Exit(stack_base);
end;

{
 * Check permissions of file to execute.
 * Called with imgp^.vp locked.
 * Return 0 for success or error code on failure.
 }
function exec_check_permissions(imgp:p_image_params):Integer;
var
 vp:p_vnode;
 attr:p_vattr;
 error:Integer;
begin
 vp:=imgp^.vp;
 attr:=imgp^.attr;

 { Get file attributes }
 error:=VOP_GETATTR(vp, attr);
 if (error<>0) then Exit(error);

 {
  * 1) Check if file execution is disabled for the filesystem that
  *    this file resides on.
  * 2) Ensure that at least one execute bit is on. Otherwise, a
  *    privileged user will always succeed, and we don't want this
  *    to happen unless the file really is executable.
  * 3) Ensure that the file is a regular file.
  }
 if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_NOEXEC)<>0) or
    ((attr^.va_mode and (S_IXUSR or S_IXGRP or S_IXOTH))=0) or
    (attr^.va_type<>VREG) then
 begin
  Exit(EACCES);
 end;

 {
  * Zero length files can't be exec'd
  }
 if (attr^.va_size=0) then Exit(ENOEXEC);

 {
  *  Check for execute permission to file based on current credentials.
  }
 error:=VOP_ACCESS(vp, VEXEC);
 if (error<>0) then Exit(error);

 {
  * Check number of open-for-writes on the file and deny execution
  * if there are any.
  }
 if (vp^.v_writecount<>0) then Exit(ETXTBSY);

 {
  * Call filesystem specific open routine (which does nothing in the
  * general case).
  }
 error:=VOP_OPEN(vp, FREAD, nil);
 if (error=0) then imgp^.opened:=1;

 Exit(error);
end;

function scan_load_sections(imgp:p_image_params;phdr:p_elf64_phdr;count:Integer):Integer;
var
 i:Integer;

 hdr:p_elf64_hdr;

 total_size:QWORD;
 data_size :QWORD;
 data_addr :QWORD;
 text_addr :QWORD;
 text_size :QWORD;

 p_memsz   :QWORD;
 p_vaddr   :QWORD;
 p_filesz  :QWORD;
 p_offset  :QWORD;

 addr:QWORD;
 size:QWORD;

 p_type   :Elf64_Word;
 p_flags  :Byte;
 _2mb_mode:Boolean;
 used_mode_2m:Boolean;

 auxargs:p_elf64_auxargs;

 cache:Pointer;
begin
 Result:=0;

 total_size:=0;
 data_size :=0;
 data_addr :=0;
 text_addr :=0;
 text_size :=0;

 hdr:=imgp^.image_header;

 if (budget_ptype_caller=0) then
 begin
  _2mb_mode:=((g_mode_2mb or 1)=3) or ((g_self_fixed<>0) and (g_mode_2mb=M2MB_NOTDYN_FIXED));
 end else
 begin
  _2mb_mode:=False;
 end;

 cache:=nil;

 if (count<>0) then
 For i:=0 to count-1 do
 begin
  p_type :=phdr^.p_type;
  p_memsz:=phdr^.p_memsz;

  if ((p_type=PT_SCE_RELRO) or (p_type=PT_LOAD)) and (p_memsz<>0) then
  begin

   p_flags:=VM_PROT_READ or VM_PROT_WRITE;
   if (p_type<>PT_SCE_RELRO) then
   begin
    p_flags:=convert_prot(phdr^.p_flags);
   end;

   p_vaddr:=phdr^.p_vaddr;

   if (hdr^.e_type=ET_SCE_DYNEXEC) then
   begin
    p_vaddr:=p_vaddr + QWORD(imgp^.reloc_base);
   end;

   p_filesz:=phdr^.p_filesz;
   p_offset:=phdr^.p_offset;

   if (p_type=PT_SCE_RELRO) and (budget_ptype_caller=0) then
   begin

    if (_2mb_mode=false) then
    begin
     used_mode_2m:=false;
    end else
    begin
     used_mode_2m:=is_used_mode_2mb(phdr,0,0);
    end;

    Result:=self_load_section(imgp,
                              i,
                              p_vaddr,
                              p_offset,
                              p_memsz,
                              p_filesz,
                              p_flags,
                              used_mode_2m,
                              'executable',
                              cache);

   end else
   begin

    if (_2mb_mode=false) then
    begin
     used_mode_2m:=false;
    end else
    begin
     used_mode_2m:=is_used_mode_2mb(phdr,0,budget_ptype_caller);
    end;

    Result:=self_load_section(imgp,
                              i,
                              p_vaddr,
                              p_offset,
                              p_memsz,
                              p_filesz,
                              p_flags,
                              used_mode_2m,
                              'executable',
                              cache);
   end;
   if (Result<>0) then
   begin
    FreeMem(cache);
    Exit;
   end;

   addr:=(p_vaddr and QWORD($ffffffffffffc000));
   size:=((p_vaddr and $3fff) + $3fff + phdr^.p_memsz) and QWORD($ffffffffffffc000);

   if (p_type=PT_SCE_RELRO) then
   begin
    imgp^.relro_addr:=Pointer(addr);
    imgp^.relro_size:=size;
   end else
   if ((phdr^.p_flags and PF_X)<>0) and (text_size < size) then
   begin
    text_size:=size;
    text_addr:=addr;
   end else
   begin
    data_size:=size;
    data_addr:=addr;
   end;

   total_size:=total_size+size;
  end;

  Inc(phdr);
 end;

 FreeMem(cache);

 if (data_addr=0) and (data_size=0) then
 begin
  data_addr:=text_addr;
  data_size:=text_size;
 end;

 if  (data_size > lim_cur(RLIMIT_DATA)) or
     (text_size > maxtsiz) or
     (total_size > lim_cur(RLIMIT_VMEM)) then
 begin
  Exit(ENOMEM);
 end;

 g_vmspace.vm_tsize:=text_size shr PAGE_SHIFT;
 g_vmspace.vm_taddr:=Pointer(text_addr);
 g_vmspace.vm_dsize:=data_size shr PAGE_SHIFT;
 g_vmspace.vm_daddr:=Pointer(data_addr);

 addr:=0;
 if (hdr^.e_type=ET_SCE_DYNEXEC) then
 begin
  addr:=text_addr;
 end;

 imgp^.entry_addr:=Pointer(addr + hdr^.e_entry);
 imgp^.reloc_base:=Pointer(addr);

 auxargs:=AllocMem(SizeOf(t_elf64_auxargs));

 auxargs^.execfd:=-1;
 auxargs^.phdr  :=0;
 auxargs^.phent :=hdr^.e_phentsize;
 auxargs^.phnum :=hdr^.e_phnum;
 auxargs^.pagesz:=PAGE_SIZE;
 auxargs^.base  :=(QWORD(g_vmspace.vm_daddr) + $3fff + lim_max(RLIMIT_DATA)) and QWORD($ffffffffffffc000);
 auxargs^.flags :=0;
 auxargs^.entry :=QWORD(imgp^.entry_addr);

 p_proc.p_osrel:=$dbbcc;

 imgp^.auxargs:=auxargs;

 MoveChar0(imgp^.execpath^,p_proc.prog_name,1024);

 if (imgp^.relro_addr<>nil) and (imgp^.relro_size<>0) then
 begin
  Result:=vm_map_protect(@g_vmspace.vm_map,QWORD(imgp^.relro_addr),QWORD(imgp^.relro_addr)+imgp^.relro_size,VM_PROT_READ,False);
  Result:=vm_mmap_to_errno(Result);
 end;

end;

function dynlib_proc_initialize_step1(imgp:p_image_params):Integer;
var
 obj:p_lib_info;
 text_addr:Pointer;
 eh_frame_addr:Pointer;
 eh_frame_size:QWORD;
begin
 Result:=0;

 TAILQ_INIT(@dynlibs_info.list_global);
 TAILQ_INIT(@dynlibs_info.list_main);
 TAILQ_INIT(@dynlibs_info.init_list);
 TAILQ_INIT(@dynlibs_info.fini_list);
 TAILQ_INIT(@dynlibs_info.obj_list);

 dynlibs_info.obj_count       :=0;
 dynlibs_info.tls_last_offset :=0;
 dynlibs_info.tls_last_size   :=0;
 dynlibs_info.tls_static_space:=0;
 dynlibs_info.tls_count       :=1;
 dynlibs_info.tls_max         :=1;

 obj:=obj_new();
 obj^.rtld_flags.mainprog:=1;
 obj^.relocbase:=imgp^.reloc_base;

 text_addr:=g_vmspace.vm_taddr;

 obj^.map_base:=text_addr;

 obj^.text_size:=g_vmspace.vm_tsize * PAGE_SIZE;
 obj^.data_addr:=g_vmspace.vm_daddr;
 obj^.data_size:=g_vmspace.vm_dsize * PAGE_SIZE;

 obj^.map_size :=((QWORD(obj^.data_addr) + obj^.data_size + $3fff) and QWORD($ffffffffffffc000)) - QWORD(text_addr);

 obj^.relro_addr:=imgp^.relro_addr;
 obj^.relro_size:=imgp^.relro_size;

 obj^.tls_index    :=1;
 obj^.tls_size     :=imgp^.tls_size;
 obj^.tls_align    :=imgp^.tls_align;
 obj^.tls_init_size:=imgp^.tls_init_size;
 obj^.tls_init_addr:=imgp^.tls_init_addr;

 eh_frame_addr:=nil;
 eh_frame_size:=0;

 if (elf64_get_eh_frame_info(imgp^.eh_frame_hdr_addr,
                             imgp^.eh_frame_hdr_size,
                             0,
                             obj^.text_size + QWORD(text_addr),
                             @eh_frame_addr,
                             @eh_frame_size)<>0) then
 begin
  eh_frame_addr:=nil;
  eh_frame_size:=0;
 end;

 obj^.eh_frame_hdr_addr:=imgp^.eh_frame_hdr_addr;
 obj^.eh_frame_hdr_size:=imgp^.eh_frame_hdr_size;
 obj^.eh_frame_addr    :=eh_frame_addr;
 obj^.eh_frame_size    :=eh_frame_size;

 obj^.entry_addr       :=imgp^.entry_addr;

 obj_set_lib_path(obj,imgp^.execpath);

 //*(byte *)&obj^.rtld_flags:=*(byte *)&obj^.rtld_flags | 1;

 obj^.loaded:=4;

 dynlibs_info.libprogram:=obj;

 if (imgp^.dyn_exist=0) then
 begin
  dynlibs_info.dyn_non_exist:=1;
  obj^.rel_data:=nil;
  //
 end else
 begin
  dynlibs_info.dyn_non_exist:=0;

  Result:=acquire_per_file_info_obj(imgp,obj);

  if (Result<>0) then
  begin
   obj_free(dynlibs_info.libprogram);
   dynlibs_info.libprogram:=nil;
  end;
 end;
end;

procedure null_init; assembler; nostackframe;
asm
 //
end;

function dynlib_proc_initialize_step2(imgp:p_image_params):Integer;
var
 obj,tail:p_lib_info;

 init_proc_addr:Pointer;
 fini_proc_addr:Pointer;
begin
 Result:=0;

 dynlibs_info.proc_param_addr:=imgp^.proc_param_addr;
 dynlibs_info.proc_param_size:=imgp^.proc_param_size;

 obj:=dynlibs_info.libprogram;

 if (imgp^.dyn_exist=0) then
 begin
  dynlibs_add_obj(obj);
  Exit;
 end;

 Result:=digest_dynamic(obj);
 if (Result<>0) then
 begin
  Writeln(StdErr,'dynlib_proc_initialize_step2:','digest_dynamic()=',Result);
  Exit;
 end;

 init_relo_bits(obj);
 dynlibs_add_obj(obj);

 dynlibs_info.sym_zero.st_info :=(STB_GLOBAL shl 4) or STT_NOTYPE;
 dynlibs_info.sym_zero.st_shndx:=SHN_UNDEF;
 dynlibs_info.sym_zero.st_value:=-Int64(obj^.relocbase);

 init_proc_addr:=obj^.init_proc_addr;
 fini_proc_addr:=obj^.fini_proc_addr;

 obj^.fini_proc_addr:=nil;
 obj^.init_proc_addr:=nil;

 tail:=TAILQ_LAST(@dynlibs_info.obj_list);
 if (tail=nil) then
 begin
  tail:=dynlibs_info.obj_list.tqh_first;
 end;

 initlist_add_objects(dynlibs_info.fini_proc_list,
                      dynlibs_info.obj_list.tqh_first,
                      tail,
                      dynlibs_info.init_proc_list);

 obj^.init_proc_addr:=@null_init;//init_proc_addr;
 obj^.fini_proc_addr:=@null_init;//fini_proc_addr;

 ///
end;

function dynlib_copy_executable_sdk_version():Integer;
var
 proc_param:pSceProcParam;
begin
 proc_param:=dynlibs_info.proc_param_addr;
 //
 if (proc_param=nil) then
 begin
  p_proc.p_sdk_version:=0;
  Result:=0;
 end else
 begin
  Result:=copyin(@proc_param^.SDK_version,@p_proc.p_sdk_version,SizeOf(Integer));
 end;
end;

procedure dynlib_proc_initialize_step3(imgp:p_image_params);
label
 _dyn_not_exist;
var
 obj:p_lib_info;
 str:RawByteString;
 err:Integer;
 flags:DWORD;
begin
 err:=0;

 obj:=nil;

 //if (td_proc->sdk_version < 0x5000000) {
 //  *(byte *)&dynlibs_info->bits = *(byte *)&dynlibs_info->bits | 0x20;
 //}

 if (imgp^.dyn_exist=0) then goto _dyn_not_exist;

 flags:=$40; //priv libs?
 if (budget_ptype_caller=0) then flags:=flags or $20; //vm_map_wire

 pick_obj(dynlibs_info.libprogram);

 str:='libkernel.sprx';
 obj:=preload_prx_modules(pchar(str),flags,err);
 dynlibs_info.libkernel:=obj;

 if (obj=nil) then
 begin
  Writeln(StdErr,'preload_prx_modules:',str,' not loaded');
 end;

 str:='libSceLibcInternal.sprx';
 obj:=preload_prx_modules(pchar(str),flags,err);

 if (obj=nil) then
 begin
  Writeln(StdErr,'preload_prx_modules:',str,' not loaded');
 end;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  objlist_push_tail(dynlibs_info.list_main,obj);

  Inc(obj^.ref_count);

  objlist_push_tail(obj^.dagmembers,obj);
  objlist_push_tail(obj^.dldags    ,obj);

  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 dynlibs_info.rep_unpf:=do_dlsym(dynlibs_info.libkernel,'sceKernelReportUnpatchedFunctionCall',nil,0);
 dynlibs_info.__freeze:=do_dlsym(dynlibs_info.libkernel,'__freeze','libkernel_sysc_se', 0);
 dynlibs_info.sysc_s00:=do_dlsym(dynlibs_info.libkernel,'sysc_s00','libkernel_sysc_se', 0);
 dynlibs_info.sysc_e00:=do_dlsym(dynlibs_info.libkernel,'sysc_e00','libkernel_sysc_se', 0);

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  dynlib_initialize_pltgot_each(obj);
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 if (dynlibs_info.libkernel<>nil) then
 begin
  imgp^.entry_addr:=dynlibs_info.libkernel^.entry_addr;

  p_proc.libkernel_start_addr:=dynlibs_info.libkernel^.map_base;
  p_proc.libkernel___end_addr:=dynlibs_info.libkernel^.map_base + dynlibs_info.libkernel^.text_size;
 end;

 _dyn_not_exist:

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if not alloc_obj_id(obj) then
  begin
   Assert(False,'ID for PRX cannot be assigned.');
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

end;

function exec_self_imgact(imgp:p_image_params):Integer;
var
 hdr :p_elf64_hdr;
 phdr:p_elf64_phdr;
 reloc_base:Pointer;
begin
 Result:=0;

 if (imgp=nil) then Exit(EINVAL);

 hdr:=imgp^.image_header;

 if (hdr=nil) then Exit(EINVAL);

 Case hdr^.e_type of
  ET_SCE_EXEC       ,
  ET_SCE_REPLAY_EXEC,
  ET_SCE_DYNEXEC    :
  else
   begin
    Writeln(StdErr,'exec_self_imgact:',imgp^.execpath,' unspported e_type:0x',HexStr(hdr^.e_type,4));
    Exit(ENOEXEC);
   end;
 end;

 if (hdr^.e_machine<>EM_X86_64) then
 begin
  Writeln(StdErr,'exec_self_imgact:',imgp^.execpath,' unspported e_machine:',hdr^.e_machine);
  Exit(ENOEXEC);
 end;

 imgp^.hdr_e_type:=hdr^.e_type;

 phdr:=get_elf_phdr(hdr);

 Result:=scan_phdr(imgp,phdr,hdr^.e_phnum);
 if (Result<>0) then
 begin
  Writeln(StdErr,'exec_self_imgact:','found illegal segment header in ',imgp^.execpath);
  Exit;
 end;

 if (imgp^.dyn_exist=0) and (hdr^.e_type=ET_SCE_DYNEXEC) then
 begin
  Writeln(StdErr,'exec_self_imgact:','illegal ELF file image',imgp^.execpath);
  Exit(ENOEXEC);
 end;

 rtld_load_auth(imgp);

 if (hdr^.e_type=ET_SCE_REPLAY_EXEC) then
 begin
  p_proc.p_sce_replay_exec:=1;
 end;

 if (hdr^.e_type=ET_SCE_DYNEXEC) then
 begin
  reloc_base:=Pointer(PROC_IMAGE_AREA_START);
  imgp^.reloc_base       :=reloc_base;
  imgp^.dyn_vaddr        :=reloc_base+QWORD(imgp^.dyn_vaddr        );
  imgp^.tls_init_addr    :=reloc_base+QWORD(imgp^.tls_init_addr    );
  imgp^.eh_frame_hdr_addr:=reloc_base+QWORD(imgp^.eh_frame_hdr_addr);
  imgp^.proc_param_addr  :=reloc_base+QWORD(imgp^.proc_param_addr  );
 end;

 if (hdr^.e_type=ET_EXEC) then
 begin
  Exit(ENOEXEC);
 end;

 if (imgp^.dyn_exist=0) then
 begin
  //
 end else
 begin
  Result:=scan_dyn_offset(imgp,phdr,hdr^.e_phnum);
  if (Result<>0) then Exit;
 end;

 VOP_UNLOCK(imgp^.vp, 0);

 Result:=exec_new_vmspace(imgp);

 vn_lock(imgp^.vp, LK_EXCLUSIVE or LK_RETRY);

 if (Result<>0) then Exit;

 imgp^.obj:=vm_pager_allocate(OBJT_SELF,
                              imgp^.vp,
                              imgp^.max_addr-imgp^.min_addr,
                              1,
                              0);

 Result:=scan_load_sections(imgp,phdr,hdr^.e_phnum);
 if (Result<>0) then
 begin
  Exit;
 end;

 Result:=dynlib_proc_initialize_step1(imgp);
 if (Result<>0) then
 begin
  Writeln(StdErr,'exec_self_imgact:',imgp^.execpath,' dynlib_proc_initialize_step1:',Result);
  Exit;
 end;

 Result:=dynlib_proc_initialize_step2(imgp);
 if (Result<>0) then
 begin
  Writeln(StdErr,'exec_self_imgact:',imgp^.execpath,' dynlib_proc_initialize_step1:',Result);
  Exit;
 end;

 Result:=dynlib_copy_executable_sdk_version();
 if (Result<>0) then
 begin
  Writeln(StdErr,'exec_self_imgact:','sdk version is not found in ',imgp^.execpath);
  Exit;
 end;
end;

procedure AUXARGS_ENTRY(var pos:PPointer;id,val:QWORD); inline;
begin
 suword(pos^, Pointer(id )); Inc(pos);
 suword(pos^, Pointer(val)); Inc(pos);
end;

procedure AUXARGS_ENTRY(var pos:PPointer;id:QWORD;val:Pointer); inline;
begin
 suword(pos^, Pointer(id )); Inc(pos);
 suword(pos^, Pointer(val)); Inc(pos);
end;

function self_orbis_fixup(stack_base:PPointer;imgp:p_image_params):Integer;
const
 osreldate=$000DBBA0;
 mp_ncpus=8;
var
 args:p_elf64_auxargs;
 base:PPointer;
 pos :PPointer;
begin
 args:=imgp^.auxargs;

 base:=stack_base^;
 pos:=base + (imgp^.args^.argc + imgp^.args^.envc + 2);

 if (args^.execfd<>-1) then
 begin
  AUXARGS_ENTRY(pos, AT_EXECFD, args^.execfd);
 end;

 AUXARGS_ENTRY(pos, AT_PHDR  , args^.phdr  );
 AUXARGS_ENTRY(pos, AT_PHENT , args^.phent );
 AUXARGS_ENTRY(pos, AT_PHNUM , args^.phnum );
 AUXARGS_ENTRY(pos, AT_PAGESZ, args^.pagesz);
 AUXARGS_ENTRY(pos, AT_FLAGS , args^.flags );
 AUXARGS_ENTRY(pos, AT_ENTRY , args^.entry );
 AUXARGS_ENTRY(pos, AT_BASE  , args^.base  );

 if (imgp^.execpathp<>nil) then
 begin
  AUXARGS_ENTRY(pos, AT_EXECPATH, imgp^.execpathp);
 end;

 AUXARGS_ENTRY(pos, AT_OSRELDATE, osreldate);

 if (imgp^.canary<>nil) then
 begin
  AUXARGS_ENTRY(pos, AT_CANARY   , imgp^.canary   );
  AUXARGS_ENTRY(pos, AT_CANARYLEN, imgp^.canarylen);
 end;

 AUXARGS_ENTRY(pos, AT_NCPUS, mp_ncpus);

 if (imgp^.pagesizes<>nil) then
 begin
  AUXARGS_ENTRY(pos, AT_PAGESIZES   , imgp^.pagesizes   );
  AUXARGS_ENTRY(pos, AT_PAGESIZESLEN, imgp^.pagesizeslen);
 end;

 AUXARGS_ENTRY(pos, AT_STACKPROT, VM_PROT_RW);

 AUXARGS_ENTRY(pos, AT_NULL, 0);

 FreeMem(imgp^.auxargs);
 imgp^.auxargs:=nil;

 Dec(base);
 suword(base^, Pointer(QWORD(imgp^.args^.argc)));

 stack_base^:=base;

 Exit(0);
end;

procedure init_tty;
begin
 kern_openat(STDIN_FILENO ,'/dev/deci_stdin' ,UIO_SYSSPACE,O_RDWR,0);
 kern_openat(STDOUT_FILENO,'/dev/deci_stdout',UIO_SYSSPACE,O_RDWR,0);
 kern_openat(STDERR_FILENO,'/dev/deci_stderr',UIO_SYSSPACE,O_RDWR,0);
end;

const
 fexecv_proc_title='(fexecv)';

function do_execve(td:p_kthread;args:p_image_args):Integer;
label
 _fullpath,
 exec_fail,
 exec_fail_dealloc,
 done2;
var
 nd:t_nameidata;
 stack_base:Pointer;
 i,error:Integer;
 image_params:t_image_params;
 imgp:p_image_params;
 attr:t_vattr;

 textvp,binvp:p_vnode;

 vfslocked:Integer;
 tvfslocked:Integer;

 newargs,oldargs:p_pargs;
begin

 textvp:=nil;
 binvp :=nil;

 vfslocked:=0;
 imgp:=@image_params;
 image_params:=Default(t_image_params);

 p_proc.p_flag:=p_proc.p_flag or P_INEXEC;

 {
  * Initialize part of the common data
  }
 attr:=Default(t_vattr);
 imgp^.attr:=@attr;
 imgp^.args:=args;

 {
  * Translate the file name. namei() returns a vnode pointer
  * in ni_vp amoung other things.
  *
  * XXXAUDIT: It would be desirable to also audit the name of the
  * interpreter if this is an interpreted binary.
  }
 if (args^.fname<>nil) then
 begin
  NDINIT(@nd, LOOKUP, ISOPEN or LOCKLEAF or FOLLOW or SAVENAME or MPSAFE or AUDITVNODE1, UIO_SYSSPACE, args^.fname, td);
 end;

 if (args^.fname<>nil) then
 begin

  error:=nd_namei(@nd);
  if (error<>0) then goto exec_fail;

  vfslocked:=NDHASGIANT(@nd);
  binvp:=nd.ni_vp;
  imgp^.vp:=binvp;
 end else
 begin
  {
   * Some might argue that CAP_READ and/or CAP_MMAP should also
   * be required here; such arguments will be entertained.
   *
   * Descriptors opened only with O_EXEC or O_RDONLY are allowed.
   }
  error:=fgetvp_exec(args^.fd, CAP_FEXECVE, @binvp);
  if (error<>0) then goto exec_fail;

  vfslocked:=VFS_LOCK_GIANT(binvp^.v_mount);
  vn_lock(binvp, LK_EXCLUSIVE or LK_RETRY);

  imgp^.vp:=binvp;
 end;

 {
  * Check file permissions (also 'opens' file)
  }
 error:=exec_check_permissions(imgp);
 if (error<>0) then goto exec_fail_dealloc;

 //imgp^.obj:=imgp^.vp^.v_object;
 //if (imgp^.object<>nil) then
 //begin
 // vm_object_reference(imgp^.object);
 //end;

 error:=rtld_load_self(imgp);
 if (error<>0) then goto exec_fail_dealloc;

 p_proc.p_osrel:=0;

 if (args^.fname=nil) then
 begin
  _fullpath:

  VOP_UNLOCK(imgp^.vp, 0);

  i:=vn_fullpath(imgp^.vp,@imgp^.execpath,@imgp^.freepath);
  if (i<>0) then
  begin
   imgp^.execpath:=args^.fname;
  end;

  vn_lock(imgp^.vp, LK_EXCLUSIVE or LK_RETRY);
 end else
 if (args^.fname^<>'/') then
 begin
  goto _fullpath;
 end else
 begin
  imgp^.execpath:=args^.fname;
 end;

 error:=exec_self_imgact(imgp);
 if (error<>0) then goto exec_fail_dealloc;

 {
  * NB: We unlock the vnode here because it is believed that none
  * of the sv_copyout_strings/sv_fixup operations require the vnode.
  }
 VOP_UNLOCK(imgp^.vp, 0);

 {
  * Do the best to calculate the full path to the image file.
  }
 if (imgp^.auxargs<>nil) and
    (
     ((args^.fname<>nil) and (args^.fname[0]='/')) or
     (vn_fullpath(imgp^.vp, @imgp^.execpath, @imgp^.freepath)<>0)
    ) then
 begin
  imgp^.execpath:=args^.fname;
 end;

 {
  * Copy out strings (args and env) and initialize stack base
  }
 stack_base:=exec_copyout_strings(imgp);

 {
  * If custom stack fixup routine present for this process
  * let it do the stack setup.
  * Else stuff argument count as first item on stack
  }

 self_orbis_fixup(@stack_base,imgp);

 {
  * Malloc things before we need locks.
  }
 i:=imgp^.args^.begin_envv - imgp^.args^.begin_argv;
 { Cache arguments if they fit inside our allowance }
 if (ps_arg_cache_limit >= (i + sizeof(t_pargs))) then
 begin
  newargs:=pargs_alloc(i);
  Move(imgp^.args^.begin_argv^,newargs^.ar_args,i);
 end;

 { close files on exec }
 fdcloseexec();

 vn_lock(imgp^.vp, LK_SHARED or LK_RETRY);

 { Get a reference to the vnode prior to locking the proc }
 VREF(binvp);

 {
  * For security and other reasons, signal handlers cannot
  * be shared after an exec. The new process gets a copy of the old
  * handlers. In execsigs(), the new process will have its signals
  * reset.
  }
 PROC_LOCK();

 if (args^.fname<>nil) then
 begin
  Move(nd.ni_cnd.cn_nameptr^, p_proc.p_comm, maxInt64(nd.ni_cnd.cn_namelen, MAXCOMLEN));
 end else
 begin
  Move(fexecv_proc_title, p_proc.p_comm, sizeof(fexecv_proc_title));
 end;

 Move(p_proc.p_comm, td^.td_name, sizeof(td^.td_name));

 {
  * mark as execed, wakeup the process that vforked (if any) and tell
  * it that it now has its own resources back
  }
 p_proc.p_flag:=p_proc.p_flag or P_EXEC;

 {
  * Notify others that we exec'd, and clear the P_INEXEC flag
  * as we're now a bona fide freshly-execed process.
  }
 KNOTE_LOCKED(@p_proc.p_klist, NOTE_EXEC);
 p_proc.p_flag:=p_proc.p_flag and (not P_INEXEC);

 { clear 'fork but no exec' flag, as we _are_ execing }
 //p^.p_acflag:= and ~AFORK;

 {
  * Free any previous argument cache and replace it with
  * the new argument cache, if any.
  }
 oldargs:=p_proc.p_args;
 p_proc.p_args:=newargs;
 newargs:=nil;

 PROC_UNLOCK();

 dynlib_proc_initialize_step3(imgp);

 { Set values passed into the program in registers. }
 exec_setregs(td, QWORD(imgp^.entry_addr), QWORD(stack_base), QWORD(g_vmspace.sv_usrstack));

 vfs_mark_atime(imgp^.vp);

 //copy authinfo
 g_authinfo:=imgp^.authinfo;

 //copy appinfo
 g_appinfo.mmap_flags:=g_appinfo.mmap_flags or 1; //is_big_app ???
 if (p_proc.p_sce_replay_exec<>0) then
 begin
  g_appinfo.mmap_flags:=g_appinfo.mmap_flags or 2; //is_system ???
 end;

 //TODO load CUSANAME

 //init std tty
 init_tty;

 {
  * Free any resources malloc'd earlier that we didn't use.
  }

 VOP_UNLOCK(imgp^.vp, 0);

 {
  * Handle deferred decrement of ref counts.
  }
 if (textvp<>nil) then
 begin
  tvfslocked:=VFS_LOCK_GIANT(textvp^.v_mount);
  vrele(textvp);
  VFS_UNLOCK_GIANT(tvfslocked);
 end;
 if (binvp<>nil) and (error<>0) then
 begin
  vrele(binvp);
 end;

 vn_lock(imgp^.vp, LK_SHARED or LK_RETRY);

 pargs_drop(oldargs);
 pargs_drop(newargs);

exec_fail_dealloc:

 {
  * free various allocated resources
  }
 rtld_free_self(imgp);

 if (imgp^.vp<>nil) then
 begin
  if (args^.fname<>nil) then
  begin
   NDFREE(@nd, NDF_ONLY_PNBUF);
  end;

  if (imgp^.opened<>0) then
  begin
   VOP_CLOSE(imgp^.vp, FREAD);
  end;

  vput(imgp^.vp);
 end;

 vm_object_deallocate(imgp^.obj);

 FreeMem(imgp^.freepath);

 if (error=0) then
 begin
  //PROC_LOCK();
  //td^.td_dbgflags:= or TDB_EXEC;
  //PROC_UNLOCK();

  {
   * Stop the process here if its stop event mask has
   * the S_EXEC bit set.
   }
  //STOPEVENT(p, S_EXEC, 0);
  goto done2;
 end;

exec_fail:
 { we're done here, clear P_INEXEC }
 PROC_LOCK();
 p_proc.p_flag:=p_proc.p_flag and (not P_INEXEC);
 PROC_UNLOCK();

done2:

 VFS_UNLOCK_GIANT(vfslocked);
 exec_free_args(args);

 if (error=0) then
 begin
  kern_jit_dynamic.switch_to_jit();
 end;

 Exit(error);
end;

function kern_execve(td:p_kthread;args:p_image_args):Integer;
var
 error:Integer;
begin
 if (td=nil) then Exit(-1);

 Assert((td^.td_pflags and TDP_EXECVMSPC)=0,'nested execve');

 error:=do_execve(td, args);

 if ((td^.td_pflags and TDP_EXECVMSPC)<>0) then
 begin
  td^.td_pflags:=td^.td_pflags and (not TDP_EXECVMSPC);
 end;

 Exit(error);
end;

function sys_execve(fname:pchar;argv,envv:ppchar):Integer;
var
 error:Integer;
 args:t_image_args;
begin
 error:=exec_copyin_args(@args, fname, UIO_USERSPACE, argv, envv);

 if (error=0) then
 begin
  error:=kern_execve(curkthread, @args);
 end;

 Result:=(error);
end;

end.

