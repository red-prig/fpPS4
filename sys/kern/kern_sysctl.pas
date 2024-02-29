unit kern_sysctl;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface


const
 CTL_MAXNAME=24; // largest number of components supported

 CTLTYPE       =$f; // Mask for the type
 CTLTYPE_NODE  =1;  // name is a node
 CTLTYPE_INT   =2;  // name describes an integer
 CTLTYPE_STRING=3;  // name describes a string
 CTLTYPE_S64   =4;  // name describes a signed 64-bit number
 CTLTYPE_OPAQUE=5;  // name describes a structure
 CTLTYPE_STRUCT=CTLTYPE_OPAQUE; // name describes a structure
 CTLTYPE_UINT  =6;  // name describes an unsigned integer
 CTLTYPE_LONG  =7;  // name describes a long
 CTLTYPE_ULONG =8;  // name describes an unsigned long
 CTLTYPE_U64   =9;  // name describes an unsigned 64-bit number

 CTLFLAG_RD     =$80000000; // Allow reads of variable
 CTLFLAG_WR     =$40000000; // Allow writes to the variable
 CTLFLAG_RW     =(CTLFLAG_RD or CTLFLAG_WR);
 CTLFLAG_ANYBODY=$10000000; // All users can set this var
 CTLFLAG_SECURE =$08000000; // Permit set only if securelevel<=0
 CTLFLAG_PRISON =$04000000; // Prisoned roots can fiddle
 CTLFLAG_DYN    =$02000000; // Dynamic oid - can be freed
 CTLFLAG_SKIP   =$01000000; // Skip this sysctl when listing
 CTLMASK_SECURE =$00F00000; // Secure level
 CTLFLAG_TUN    =$00080000; // Tunable variable
 CTLFLAG_RDTUN  =(CTLFLAG_RD or CTLFLAG_TUN);
 CTLFLAG_RWTUN  =(CTLFLAG_RW or CTLFLAG_TUN);
 CTLFLAG_MPSAFE =$00040000; // Handler is MP safe
 CTLFLAG_VNET   =$00020000; // Prisons with vnet can fiddle
 CTLFLAG_DYING  =$00010000; // oid is being removed
 CTLFLAG_CAPRD  =$00008000; // Can be read in capability mode
 CTLFLAG_CAPWR  =$00004000; // Can be written in capability mode
 CTLFLAG_CAPRW  =(CTLFLAG_CAPRD or CTLFLAG_CAPWR);

 OID_AUTO=(-1);

 CTL_AUTO_START=$100;

//Top-level identifiers
 CTL_UNSPEC  = 0; // unused
 CTL_KERN    = 1; // "high kernel": proc, limits
 CTL_VM      = 2; // virtual memory
 CTL_VFS     = 3; // filesystem, mount type is next
 CTL_NET     = 4; // network, see socket.h
 CTL_DEBUG   = 5; // debugging parameters
 CTL_HW      = 6; // generic cpu/io
 CTL_MACHDEP = 7; // machine dependent
 CTL_USER    = 8; // user-level
 CTL_P1003_1B= 9; // POSIX 1003.1B
 CTL_MAXID   =10; // number of valid top-level ids


//CTL_KERN identifiers
 KERN_PROC_     =14;
 KERN_USRSTACK  =33;
 KERN_ARND      =37;
 KERN_SDKVERSION=38; //SDK version

 KERN_SMP     =$485; //(OID_AUTO) Kernel SMP
 KERN_SCHED   =$2A0; //(OID_AUTO) Scheduler
 KERN_NEOMODE =$50F; //(OID_AUTO) Neo mode

//CTL_VM subtypes
 KERN_VM_PS4DEV=1;     //vm parameters for PS4 (DevKit only)

 KERN_VM_BUDGETS=$139; //(OID_AUTO) VM budgets

//KERN_PROC subtypes
 KERN_PROC_APPINFO     =35; //Application information
 KERN_PROC_SDK_VERSION =36; //SDK version of the executable file
 KERN_PROC_IDTABLE     =37; //ID table information

 KERN_PROC_SANITIZER   =41; //kern_sanitizer (Sanitizing mode)
 KERN_PROC_PTC         =43; //Process time counter (value at program start)
 KERN_PROC_TEXT_SEGMENT=44; //kern_dynlib_get_libkernel_text_segment

//KERN_SMP subtypes
 KERN_CPUS=$48A; //(OID_AUTO) Number of CPUs online

//KERN_SCHED subtypes
 KERN_SCHED_CPUSETSIZE=$4E4; //(OID_AUTO) sizeof(cpuset_t)

//CTL_HW identifiers
 HW_MACHINE     = 1; // string: machine class
 HW_MODEL       = 2; // string: specific machine model
 HW_NCPU        = 3; // int: number of cpus
 HW_BYTEORDER   = 4; // int: machine byte order
 HW_PHYSMEM     = 5; // int: total memory
 HW_USERMEM     = 6; // int: non-kernel memory
 HW_PAGESIZE    = 7; // int: software page size
 HW_DISKNAMES   = 8; // strings: disk drive names
 HW_DISKSTATS   = 9; // struct: diskstats[]
 HW_FLOATINGPT  =10; // int: has HW floating point?
 HW_MACHINE_ARCH=11; // string: machine architecture
 HW_REALMEM     =12; // int: 'real' memory
 HW_MAXID       =13; // number of valid hw ids

//MACHDEP subtypes
 MACHDEP_TSC_FREQ  =$1EC; //(OID_AUTO) Time Stamp Counter frequency
 MACHDEP_BOOTPARAMS=$14D; //(OID_AUTO) orbis bootparams

//BOOTPARAMS subtypes
 BOOTPARAMS_IS_MAIN_ON_STANDBY=$100; //(OID_AUTO) Is main on standby mode
 BOOTPARAMS_BASE_PS4_MODE     =$151; //(OID_AUTO) base ps4 mode

//KERN_VM_PS4DEV subtypes
 KERN_VM_PS4DEV_TRCMEM_TOTAL=$23B; //(OID_AUTO) trace memory total
 KERN_VM_PS4DEV_TRCMEM_AVAIL=$23C; //(OID_AUTO) trace memory available

//KERN_VM_BUDGETS
 KERN_VM_BUDGETS_MLOCK_AVAIL=$13A; //(OID_AUTO) Available MLOCK budget
 KERN_VM_BUDGETS_MLOCK_TOTAL=$13B; //(OID_AUTO) Total MLOCK budget

//SYSCTL_HANDLER_ARGS oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req

type
 p_sysctl_req=^t_sysctl_req;

 t_sysctl_func=function(req:p_sysctl_req;p:Pointer;s:QWORD):Integer;

 t_sysctl_req=record
  td      :Pointer; //p_kthread
  lock    :Integer;
  oldptr  :Pointer;
  oldlen  :QWORD;
  oldidx  :QWORD;
  oldfunc :t_sysctl_func;
  newptr  :Pointer;
  newlen  :QWORD;
  newidx  :QWORD;
  newfunc :t_sysctl_func;
  validlen:QWORD;
  flags   :Integer;
 end;

 p_sysctl_oid=^t_sysctl_oid;

 t_oid_handler=function(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;

 t_sysctl_oid=record
  oid_arg1   :Pointer;
  oid_arg2   :Integer;
  oid_kind   :DWORD;
  oid_name   :PInteger;
  oid_handler:t_oid_handler;
 end;

function sys___sysctl(name   :PInteger;
                      namelen:DWORD;
                      old    :Pointer;
                      oldlenp:PQWORD;
                      new    :Pointer;
                      newlen :QWORD):Integer;

procedure sysctl_register_all(); //SYSINIT(sysctl, SI_SUB_KMEM, SI_ORDER_ANY, sysctl_register_all, 0);

implementation

uses
 sysutils,
 errno,
 systm,
 vmparam,
 vm_map,
 kern_thr,
 kern_sx,
 time,
 elf64,
 subr_dynlib,
 kern_authinfo,
 md_arc4random,
 sys_bootparam,
 kern_proc,
 md_proc,
 kern_budget,
 subr_backtrace;

var
 sysctllock   :t_sx;
 sysctlmemlock:t_sx;

procedure sysctl_register_all();
begin
 sx_init(@sysctlmemlock, 'sysctl mem');
 sx_init(@sysctllock   , 'sysctl lock');
end;

procedure SYSCTL_XLOCK(); inline;
begin
 sx_xlock(@sysctllock)
end;

procedure SYSCTL_XUNLOCK(); inline;
begin
 sx_xunlock(@sysctllock);
end;

procedure SYSCTL_ASSERT_XLOCKED(); inline;
begin
 sx_assert(@sysctllock)
end;

function SYSCTL_IN(req:p_sysctl_req;p:Pointer;s:QWORD):Integer; inline;
begin
 Result:=req^.newfunc(req,p,s);
end;

function SYSCTL_OUT(req:p_sysctl_req;p:Pointer;s:QWORD):Integer; inline;
begin
 Result:=req^.oldfunc(req,p,s);
end;

function SYSCTL_HANDLE(noid:p_sysctl_oid;
                       name:PInteger;
                       kind:DWORD;
                       func:Pointer):Integer; inline;
begin
 noid^.oid_name   :=name+1;
 noid^.oid_kind   :=kind;
 noid^.oid_arg1   :=nil;
 noid^.oid_arg2   :=0;
 noid^.oid_handler:=t_oid_handler(func);
 Result:=0
end;

function SYSCTL_HANDLE(noid:p_sysctl_oid;
                       name:PInteger;
                       kind:DWORD;
                       arg1:Pointer;
                       func:Pointer):Integer; inline;
begin
 noid^.oid_name   :=name+1;
 noid^.oid_kind   :=kind;
 noid^.oid_arg1   :=arg1;
 noid^.oid_arg2   :=0;
 noid^.oid_handler:=t_oid_handler(func);
 Result:=0
end;

function SYSCTL_HANDLE(noid:p_sysctl_oid;
                       name:PInteger;
                       kind:DWORD;
                       arg2:Integer;
                       func:Pointer):Integer; inline;
begin
 noid^.oid_name   :=name+1;
 noid^.oid_kind   :=kind;
 noid^.oid_arg1   :=nil;
 noid^.oid_arg2   :=arg2;
 noid^.oid_handler:=t_oid_handler(func);
 Result:=0
end;

function SYSCTL_HANDLE(noid:p_sysctl_oid;
                       name:PInteger;
                       kind:DWORD;
                       arg1:Pointer;
                       arg2:Integer;
                       func:Pointer):Integer; inline;
begin
 noid^.oid_name   :=name+1;
 noid^.oid_kind   :=kind;
 noid^.oid_arg1   :=arg1;
 noid^.oid_arg2   :=arg2;
 noid^.oid_handler:=t_oid_handler(func);
 Result:=0
end;

//Transfer function to/from user space.
function sysctl_old_user(req:p_sysctl_req;p:Pointer;l:QWORD):Integer;
var
 i,len,origidx:QWORD;
 error:Integer;
begin
 origidx:=req^.oldidx;
 Inc(req^.oldidx,l);

 if (req^.oldptr=nil) then
 begin
  Exit(0);
 end;

 i:=l;
 len:=req^.validlen;
 if (len <= origidx) then
 begin
  i:=0;
 end else
 begin
  if (i > len - origidx) then
  begin
   i:=len - origidx;
  end;
  //if (req^.lock=REQ_WIRED) then
  //begin
  // error:=copyout_nofault(p, req^.oldptr + origidx, i);
  //end else
  begin
   error:=copyout(p, req^.oldptr + origidx, i);
  end;
  if (error<>0) then
  begin
   Exit(error);
  end;
 end;
 if (i < l) then
 begin
  Exit(ENOMEM);
 end;
 Exit(0);
end;

function sysctl_new_user(req:p_sysctl_req;p:Pointer;l:QWORD):Integer;
var
 error:Integer;
begin
 if (req^.newptr=nil) then
 begin
  Exit(0);
 end;

 if ((req^.newlen - req^.newidx) < l) then
 begin
  Exit(EINVAL);
 end;

 error:=copyin(req^.newptr + req^.newidx, p, l);

 Inc(req^.newidx,l);
 Exit(error);
end;

//Exit(ENOTDIR);
//Exit(ENOENT);

function sysctl_kern_proc_idtable(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
begin
 //get idtable key count
 Exit(ENOENT); //sceSblACMgrIsSystemUcred
end;

function sysctl_kern_usrstack(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 vms:p_vmspace;
begin
 vms:=p_proc.p_vmspace;
 Result:=SYSCTL_OUT(req,@vms^.sv_usrstack,SizeOf(Pointer));
end;

function sysctl_kern_arandom(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
type
 t_data_256=array[0..255] of Byte;
var
 len:Integer;
 data:t_data_256;
begin
 len:=256;
 if (req^.oldlen < 256)  then
 begin
  len:=req^.oldlen;
 end;

 data:=Default(t_data_256);
 arc4rand(@data,len,0); //ASLR?

 Result:=SYSCTL_OUT(req,@data,len);
end;

function sysctl_kern_proc_appinfo(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 pid:Integer;
 flags:Integer;
 appinfo:t_appinfo;
begin
 if (req^.oldlen > 72) then Exit(EINVAL);

 pid:=PInteger(arg1)^;

 if (pid<>p_proc.p_pid) then Exit(EINVAL);

 //sceSblACMgrIsSystemUcred()!=0 -> any proc
 //sceSblACMgrIsSystemUcred()==0 -> cur proc

 Result:=SYSCTL_OUT(req,@g_appinfo,SizeOf(t_appinfo));

 if (Result=0) and (req^.newlen=SizeOf(t_appinfo)) then
 begin
  Result:=SYSCTL_IN(req,@appinfo,SizeOf(t_appinfo));
  if (Result=0) then
  begin
   flags:=g_appinfo.mmap_flags;
   g_appinfo:=appinfo;
   g_appinfo.mmap_flags:=g_appinfo.mmap_flags or (flags and 2)
  end;
 end;

end;

function sysctl_kern_proc_sanitizer(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 Sanitizer:Integer;
begin
 Sanitizer:=0;
 Result:=SYSCTL_OUT(req,@Sanitizer,SizeOf(Integer));
end;

function sysctl_kern_proc_ptc(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
begin
 Result:=SYSCTL_OUT(req,@p_proc.p_ptc,SizeOf(Int64));
end;

function _copy_libkernel_addr(req:p_sysctl_req):Integer;
var
 addr_out:array[0..1] of Pointer;
begin
 addr_out[0]:=p_proc.p_libkernel_start_addr;
 addr_out[1]:=p_proc.p_libkernel___end_addr;

 Result:=SYSCTL_OUT(req,@addr_out,SizeOf(addr_out));
end;

function sysctl_kern_text_segment(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 addr_out:array[0..1] of Pointer;
 libc_param:TSceLibcParam;
 sceLibcHeapDebugFlags:Integer;
begin
 Result:=0;

 addr_out[0]:=nil;
 addr_out[1]:=nil;

 if false then //sceSblACMgrIsShellcoreProces
               //sceSblACMgrIsSystemUcred     && sceRegMgrSrvGetQAFforReg   && sceRegMgrGetInt(sys_heap_trace)
               //sceSblACMgrIsWebcoreProcess  && sceKernelIsDevelopmentMode
               //sceRegMgrSrvGetQAFforReg     && sceRegMgrGetInt(game_heap_trace)
               //sceKernelIsDevelopmentMode   && sceKernelIsAssistMode      && sceRegMgrGetInt(game_intmem_dbg)
 begin
  Exit(_copy_libkernel_addr(req));
 end;

 Result:=copy_libc_param(@libc_param);

 if (Result=0) then
 if (libc_param.entry_count>8) then
 begin

  if (libc_param.SceLibcInternalHeap=1) then
  begin
   Result:=copyin(libc_param.sceLibcHeapDebugFlags,@sceLibcHeapDebugFlags,4);
   if (Result=0) and ((sceLibcHeapDebugFlags and 8)<>0) then
   begin
    Exit(_copy_libkernel_addr(req));
   end else
   begin
    Result:=0;
   end;
  end;

  if (libc_param.entry_count > 11) and (libc_param.SceLibcInternalHeap=1) then
  begin
   Result:=copyin(libc_param.sceKernelInternalMemoryDebugFlags,@sceLibcHeapDebugFlags,4);
   if (Result=0) and ((sceLibcHeapDebugFlags and 8)<>0) then
   begin
    Exit(_copy_libkernel_addr(req));
   end else
   begin
    Result:=0;
   end;
  end;

 end;

 SYSCTL_OUT(req,@addr_out,SizeOf(addr_out));
end;

function sysctl_handle_int(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 tmpout:Integer;
begin
 if (arg1<>nil) then
 begin
  tmpout:=PInteger(arg1)^;
 end else
 begin
  tmpout:=arg2;
 end;

 Result:=SYSCTL_OUT(req,@tmpout,SizeOf(Integer));

 if (Result<>0) or (req^.newptr=nil) then Exit;

 if (arg1=nil) then Exit(EPERM);

 Result:=SYSCTL_IN(req, arg1, sizeof(Integer));
end;

function sysctl_handle_64(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 tmpout:QWORD;
begin
 if (arg1=nil) then
 begin
  Exit(EINVAL);
 end;

 tmpout:=PInteger(arg1)^;

 Result:=SYSCTL_OUT(req,@tmpout,SizeOf(QWORD));

 if (Result<>0) or (req^.newptr=nil) then Exit;

 if (arg1=nil) then Exit(EPERM);

 Result:=SYSCTL_IN(req, arg1, sizeof(QWORD));
end;

function name2oid(name:pchar;oid,len:PInteger):Integer;
begin
 Result:=0;

 case RawByteString(name) of
  'kern.sdk_version':
    begin
     oid[0]:=CTL_KERN;
     oid[1]:=KERN_SDKVERSION;
     len^  :=2;
    end;
  'kern.smp.cpus':
    begin
     oid[0]:=CTL_KERN;
     oid[1]:=KERN_SMP;
     oid[2]:=KERN_CPUS;
     len^  :=3;
    end;
  'kern.proc.ptc':
    begin
     oid[0]:=CTL_KERN;
     oid[1]:=KERN_PROC_;
     oid[2]:=KERN_PROC_PTC;
     len^  :=3;
    end;
  'kern.sched.cpusetsize':
    begin
     oid[0]:=CTL_KERN;
     oid[1]:=KERN_SCHED;
     oid[2]:=KERN_SCHED_CPUSETSIZE;
     len^  :=3;
    end;
  'kern.neomode':
    begin
     oid[0]:=CTL_KERN;
     oid[1]:=KERN_NEOMODE;
     len^  :=2;
    end;
  'machdep.tsc_freq':
    begin
     oid[0]:=CTL_MACHDEP;
     oid[1]:=MACHDEP_TSC_FREQ;
     len^  :=2;
    end;
  'machdep.bootparams.base_ps4_mode':
    begin
     oid[0]:=CTL_MACHDEP;
     oid[1]:=MACHDEP_BOOTPARAMS;
     oid[2]:=BOOTPARAMS_BASE_PS4_MODE;
     len^  :=3;
    end;
  'vm.ps4dev.trcmem_total':
    begin
     oid[0]:=CTL_VM;
     oid[1]:=KERN_VM_PS4DEV;
     oid[2]:=KERN_VM_PS4DEV_TRCMEM_TOTAL;
     len^  :=3;
    end;
  'vm.ps4dev.trcmem_avail':
    begin
     oid[0]:=CTL_VM;
     oid[1]:=KERN_VM_PS4DEV;
     oid[2]:=KERN_VM_PS4DEV_TRCMEM_AVAIL;
     len^  :=3;
    end;
  'vm.budgets.mlock_avail':
    begin
     oid[0]:=CTL_VM;
     oid[1]:=KERN_VM_BUDGETS;
     oid[2]:=KERN_VM_BUDGETS_MLOCK_AVAIL;
     len^  :=3;
    end;
  'vm.budgets.mlock_total':
    begin
     oid[0]:=CTL_VM;
     oid[1]:=KERN_VM_BUDGETS;
     oid[2]:=KERN_VM_BUDGETS_MLOCK_TOTAL;
     len^  :=3;
    end;


  else
   print_error_td('Unhandled name2oid:'+name);
   Assert(False);
   Result:=ENOENT;
 end;
end;

function sysctl_sysctl_name2oid(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
type
 t_path=array[0..MAXPATHLEN] of AnsiChar;
var
 new:t_path;
 oid:array[0..CTL_MAXNAME-1] of Integer;
 len:Integer;
begin
 if (req^.newlen=0) then Exit(ENOENT);

 if (req^.newlen >= MAXPATHLEN) then Exit(ENAMETOOLONG);

 new:=Default(t_path);

 Result:=SYSCTL_IN(req, @new, req^.newlen);
 if (Result<>0) then Exit;

 Result:=name2oid(@new, @oid, @len);
 if (Result<>0) then Exit;

 Result:=SYSCTL_OUT(req, @oid, len*sizeof(Integer));
end;

function sysctl_kern_proc(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  KERN_PROC_APPINFO     :Result:=SYSCTL_HANDLE(noid,name,$C0040001,@sysctl_kern_proc_appinfo);
  KERN_PROC_SANITIZER   :Result:=SYSCTL_HANDLE(noid,name,$80040001,@sysctl_kern_proc_sanitizer);
  KERN_PROC_PTC         :Result:=SYSCTL_HANDLE(noid,name,$90040009,@sysctl_kern_proc_ptc);
  KERN_PROC_TEXT_SEGMENT:Result:=SYSCTL_HANDLE(noid,name,$80040001,@sysctl_kern_text_segment);
  else
   begin
    print_error_td('Unhandled sysctl_kern_proc:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_kern_smp(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
const
 smp_cpus=8;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  KERN_CPUS:Result:=SYSCTL_HANDLE(noid,name,$80048002,smp_cpus,@sysctl_handle_int);

  else
   begin
    print_error_td('Unhandled sysctl_kern_smp:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_kern_sched(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  KERN_SCHED_CPUSETSIZE:Result:=SYSCTL_HANDLE(noid,name,$80040002,8,@sysctl_handle_int);

  else
   begin
    print_error_td('Unhandled sysctl_kern_sched:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_kern(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  KERN_PROC_     :Result:=sysctl_kern_proc(name+1,namelen-1,noid,req);

  KERN_USRSTACK  :Result:=SYSCTL_HANDLE(noid,name,$80008008,@sysctl_kern_usrstack);
  KERN_ARND      :Result:=SYSCTL_HANDLE(noid,name,$80048005,@sysctl_kern_arandom);
  KERN_SDKVERSION:Result:=SYSCTL_HANDLE(noid,name,$80048006,p_system_sdk_version,@sysctl_handle_int);

  KERN_SMP       :Result:=sysctl_kern_smp  (name+1,namelen-1,noid,req);
  KERN_SCHED     :Result:=sysctl_kern_sched(name+1,namelen-1,noid,req);
  KERN_NEOMODE   :Result:=SYSCTL_HANDLE(noid,name,$80040002,p_neomode,@sysctl_handle_int);
  else
   begin
    print_error_td('Unhandled sysctl_kern:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_mlock_avail(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 val:QWORD;
begin
 val:=get_mlock_avail;
 Result:=SYSCTL_OUT(req,@val,SizeOf(QWORD));
end;

function sysctl_mlock_total(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 val:QWORD;
begin
 val:=get_mlock_total;
 Result:=SYSCTL_OUT(req,@val,SizeOf(QWORD));
end;

function sysctl_vm(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of

  KERN_VM_BUDGETS_MLOCK_AVAIL:Result:=SYSCTL_HANDLE(noid,name,$80000008,@sysctl_mlock_avail); //sceKernelAvailableFlexibleMemorySize
  KERN_VM_BUDGETS_MLOCK_TOTAL:Result:=SYSCTL_HANDLE(noid,name,$80000008,@sysctl_mlock_total); //sceKernelConfiguredFlexibleMemorySize

  else
   begin
    print_error_td('Unhandled sysctl_vm:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_sysctl(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  3:Result:=SYSCTL_HANDLE(noid,name,$D004C002,@sysctl_sysctl_name2oid);

  else
   begin
    print_error_td('Unhandled sysctl_sysctl:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_hw(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  HW_PAGESIZE:Result:=SYSCTL_HANDLE(noid,name,$80048002,PAGE_SIZE,@sysctl_handle_int);

  else
   begin
    print_error_td('Unhandled sysctl_hw:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_machdep_tsc_freq(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 freq:QWORD;
begin
 freq:=System.InterlockedExchangeAdd64(tsc_freq,0);
 if (freq=0) then Exit(EOPNOTSUPP);

 Result:=sysctl_handle_64(oidp, @freq, 0, req);
 if (Result=0) and (req^.newptr<>nil) then
 begin
  System.InterlockedExchange64(tsc_freq,freq);
 end;
end;

function sysctl_bootparams(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  BOOTPARAMS_BASE_PS4_MODE:Result:=SYSCTL_HANDLE(noid,name,$80040002,p_base_ps4_mode,@sysctl_handle_int);
  else
   begin
    print_error_td('Unhandled sysctl_bootparams:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_machdep(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  MACHDEP_TSC_FREQ  :Result:=SYSCTL_HANDLE(noid,name,$C0000009,@sysctl_machdep_tsc_freq);
  MACHDEP_BOOTPARAMS:Result:=sysctl_bootparams(name+1,namelen-1,noid,req);

  else
   begin
    print_error_td('Unhandled sysctl_machdep:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_find_oid(name   :PInteger;
                         namelen:DWORD;
                         noid   :p_sysctl_oid;
                         req    :p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOENT);
 Result:=ENOENT;

 case name[0] of
  CTL_UNSPEC :Result:=sysctl_sysctl (name+1,namelen-1,noid,req);
  CTL_KERN   :Result:=sysctl_kern   (name+1,namelen-1,noid,req);
  CTL_VM     :Result:=sysctl_vm     (name+1,namelen-1,noid,req);
  CTL_HW     :Result:=sysctl_hw     (name+1,namelen-1,noid,req);
  CTL_MACHDEP:Result:=sysctl_machdep(name+1,namelen-1,noid,req);
  else
   begin
    print_error_td('Unhandled sysctl_root:'+IntToStr(name[0]));
    Assert(False);
   end;
 end;
end;

function sysctl_root(oidp:p_sysctl_oid;
                     arg1:PInteger;
                     arg2:DWORD;
                     req :p_sysctl_req):Integer;
var
 oid:t_sysctl_oid;
 indx:Integer;
begin
 oid:=Default(t_sysctl_oid);

 Result:=sysctl_find_oid(arg1, arg2, @oid, req);
 if (Result<>0) then Exit;

 if (oid.oid_handler=nil) then Exit(EINVAL);
 if (oid.oid_name   =nil) then Exit(EINVAL);

 // Is this sysctl writable?
 if (req^.newptr<>nil) and ((oid.oid_kind and CTLFLAG_WR)=0) then
 begin
  Exit(EPERM);
 end;

 // Is this sysctl writable by only privileged users?
 if (req^.newptr<>nil) and ((oid.oid_kind and CTLFLAG_ANYBODY)=0) then
 begin
  //if (oid^.oid_kind and CTLFLAG_PRISON)
  // priv:=PRIV_SYSCTL_WRITEJAIL;
  //else
  // priv:=PRIV_SYSCTL_WRITE;
  //
  //error:=priv_check(req^.td, priv);
  //
  //if (error<>0) then Ext(error);

  Exit(EPERM);
 end;

 indx:=oid.oid_name-arg1;

 if ((oid.oid_kind and CTLTYPE)=CTLTYPE_NODE) then
 begin
  arg1:=arg1 + indx;
  arg2:=arg2 - indx;
 end else
 begin
  arg1:=oid.oid_arg1;
  arg2:=oid.oid_arg2;
 end;

 Result:=oid.oid_handler(@oid, arg1, arg2, req);

end;

function userland_sysctl(name    :PInteger;
                         namelen :DWORD;
                         old     :Pointer;
                         oldlenp :PQWORD;
                         inkernel:Integer;
                         new     :Pointer;
                         newlen  :QWORD;
                         retval  :PQWORD;
                         flags   :Integer):Integer;
var
 error,memlocked:Integer;
 req:t_sysctl_req;
begin
 error:=0;

 req:=Default(t_sysctl_req);

 req.td   :=curkthread;
 req.flags:=flags;

 if (oldlenp<>nil) then
 begin
  if (inkernel<>0) then
  begin
   req.oldlen:=oldlenp^;
  end else
  begin
   error:=copyin(oldlenp, @req.oldlen, sizeof(Pointer));
   if (error<>0) then Exit(error);
  end;
 end;
 req.validlen:=req.oldlen;

 if (old<>nil) then
 begin
  //if (!useracc(old, req.oldlen, VM_PROT_WRITE))
  // Exit(EFAULT);
  req.oldptr:=old;
 end;

 if (new<>nil) then
 begin
  //if (!useracc(new, newlen, VM_PROT_READ))
  // Exit(EFAULT);
  req.newlen:=newlen;
  req.newptr:=new;
 end;

 req.oldfunc:=@sysctl_old_user;
 req.newfunc:=@sysctl_new_user;
 //req.lock:=REQ_UNWIRED;

 if (req.oldlen > PAGE_SIZE) then
 begin
  memlocked:=1;
  sx_xlock(@sysctlmemlock);
 end else
 begin
  memlocked:=0;
 end;

 repeat
  req.oldidx:=0;
  req.newidx:=0;
  SYSCTL_XLOCK();
  error:=sysctl_root(nil, name, namelen, @req);
  SYSCTL_XUNLOCK();
  if (error<>EAGAIN) then
  begin
   break;
  end;
  //kern_yield(PRI_USER);
 until false;

 //if (req.lock=REQ_WIRED) and (req.validlen > 0) then
 //begin
 // vsunlock(req.oldptr, req.validlen);
 //end;

 if (memlocked<>0) then
 begin
  sx_xunlock(@sysctlmemlock);
 end;

 if (error<>0) and (error<>ENOMEM) then
 begin
  Exit(error);
 end;

 if (retval<>nil) then
 begin
  if (req.oldptr<>nil) and (req.oldidx > req.validlen) then
   retval^:=req.validlen
  else
   retval^:=req.oldidx;
 end;
 Exit(error);
end;

function sys___sysctl(name   :PInteger;
                      namelen:DWORD;
                      old    :Pointer;
                      oldlenp:PQWORD;
                      new    :Pointer;
                      newlen :QWORD):Integer;
var
 error,i:Integer;
 _name:array[0..CTL_MAXNAME-1] of Integer;
 j:QWORD;
begin
 if (namelen > CTL_MAXNAME) or (namelen < 2) then
 begin
  Exit(EINVAL);
 end;

 error:=copyin(name, @_name, namelen * sizeof(Integer));
 if (error<>0) then Exit(error);

 error:=userland_sysctl(@_name,
                        namelen,
                        old,
                        oldlenp,
                        0,
                        new,
                        newlen,
                        @j,
                        0);

 if (error<>0) and (error<>ENOMEM) then
 begin
  Exit(error);
 end;

 if (oldlenp<>nil) then
 begin
  i:=copyout(@j, oldlenp, sizeof(j));
  if (i<>0) then
  begin
   Exit(i);
  end;
 end;

 Exit(error);
end;






end.

