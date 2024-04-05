unit kern_budget;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 sys_vm_object;

type
 p_budget_resource=^t_budget_resource;
 t_budget_resource=packed record
  resid:Integer;
  attr :Integer;
  limit:QWORD;   //cpuset
  used :QWORD;
 end;

//budget resid
const
 SCE_KERNEL_BUDGET_INVALID     =0;
 SCE_KERNEL_BUDGET_MEMORY_DMEM =1;
 SCE_KERNEL_BUDGET_MEMORY_VMEM =2;
 SCE_KERNEL_BUDGET_MEMORY_MLOCK=3;
 SCE_KERNEL_BUDGET_CPU_SET     =4;
 SCE_KERNEL_BUDGET_FD_FILE     =5;
 SCE_KERNEL_BUDGET_FD_SOCKET   =6;
 SCE_KERNEL_BUDGET_FD_EQUEUE   =7;
 SCE_KERNEL_BUDGET_FD_PIPE     =8;
 SCE_KERNEL_BUDGET_FD_DEVICE   =9;
 SCE_KERNEL_BUDGET_THREADS     =10;
 SCE_KERNEL_BUDGET_FD_IPCSOCKET=11;

//budget proc_type
 PTYPE_BIG_APP          = 0;
 PTYPE_MINI_APP         = 1;
 PTYPE_SYSTEM           = 2;
 PTYPE_NONGAME_MINI_APP = 3;

function  sys_budget_create(name:pchar;ptype:DWORD;new:Pointer;count:DWORD;prev:Pointer):Integer;
function  sys_budget_delete(key:Integer):Integer;
function  sys_budget_get(key:Integer;ptr:Pointer;psize:PInteger):Integer;
function  sys_budget_set(key:Integer):Integer;
function  sys_budget_get_ptype(pid:Integer):Integer;
function  sys_budget_get_ptype_of_budget(key:Integer):Integer;
function  sys_budget_getid():Integer;

//internals
type
 t_budget_info=packed record
  zero      :QWORD;
  dmem_alloc:QWORD;
  malloc    :QWORD;
  mlock     :QWORD;
 end;

const
 field_dmem_alloc=1;
 field_mlock     =2;
 field_malloc    =3;

const
 FMEM_BASE          =$4000000;
 bigapp_size        =$4000000;
 bigapp_max_exe_size=$20000000;

var
 FMEM_LIMIT   :QWORD=0;
 DMEM_LIMIT   :QWORD=$180000000;
 EXE_FILE_SIZE:QWORD=FMEM_BASE+0;
 ExtendedSize :QWORD=0;

 ext_game_fmem_addr:Integer=0;
 IGNORE_EXTENDED_DMEM_BASE:Integer=0;

procedure set_budget_limit (ptype,field:Integer;value:QWORD);
function  vm_budget_limit  (ptype,field:Integer):QWORD;
function  vm_budget_used   (ptype,field:Integer):QWORD;
function  vm_budget_reserve(ptype,field:Integer;len:QWORD):Integer;
procedure vm_budget_release(ptype,field:Integer;len:QWORD);

procedure set_bigapp_cred_limits;
procedure set_bigapp_limits(size,unknow:QWORD);

function  dmem_process_relocated():Integer;

function  get_mlock_avail():QWORD;
function  get_mlock_total():QWORD;

implementation

uses
 errno,
 sys_bootparam,
 kern_thr,
 kern_proc,
 kern_rwlock,
 systm,
 elf64,
 subr_dynlib,
 kern_authinfo;

var
 budget_limit  :t_budget_info;
 budget_reserve:t_budget_info;

 budget_lock   :Pointer;

procedure set_budget_limit(ptype,field:Integer;value:QWORD);
begin
 rw_wlock(budget_lock);

  case field of
   field_dmem_alloc:Writeln('set_budget_limit(field_dmem_alloc,0x',HexStr(value,16),')');
   field_mlock     :Writeln('set_budget_limit(field_mlock     ,0x',HexStr(value,16),')');
   field_malloc    :Writeln('set_budget_limit(field_malloc    ,0x',HexStr(value,16),')');
   else;
  end;

  //ptype/budget_id ignored TODO
  PQWORD(@budget_limit)[field]:=value;

 rw_wunlock(budget_lock);
end;

function vm_budget_limit(ptype,field:Integer):QWORD;
begin
 rw_wlock(budget_lock);

  //ptype/budget_id ignored TODO
  Result:=PQWORD(@budget_limit)[field];

 rw_wunlock(budget_lock);
end;

function vm_budget_used(ptype,field:Integer):QWORD;
begin
 rw_wlock(budget_lock);

  //ptype/budget_id ignored TODO
  Result:=PQWORD(@budget_reserve)[field];

 rw_wunlock(budget_lock);
end;

function vm_budget_reserve(ptype,field:Integer;len:QWORD):Integer;
var
 rsv,limit:QWORD;
begin
 if (ptype<PTYPE_BIG_APP) then
 begin
  Result:=0;
 end else
 begin
  rw_wlock(budget_lock);

   //ptype/budget_id ignored TODO

   rsv  :=PQWORD(@budget_reserve)[field];
   limit:=PQWORD(@budget_limit  )[field];

   if (rsv <= limit) and
      (len <= (limit - rsv)) then
   begin
    PQWORD(@budget_reserve)[field]:=rsv + len;
    Result:=0;
   end else
   begin
    Result:=ENOMEM;
   end;

  rw_wunlock(budget_lock);
 end;
end;

procedure vm_budget_release(ptype,field:Integer;len:QWORD);
var
 rsv,size:QWORD;
begin
 if (ptype > -1) then
 begin
  rw_wlock(budget_lock);

   //ptype/budget_id ignored TODO

   rsv:=PQWORD(@budget_reserve)[field];

   size:=0;
   if (len <= rsv) then
   begin
    size:=rsv - len;
   end;

   PQWORD(@budget_reserve)[field]:=size;

  rw_wunlock(budget_lock);
 end;
end;

procedure set_bigapp_cred_limits;
var
 size :QWORD;
 value:QWORD;
 m_256:QWORD;
 ret  :Boolean;
begin
 ret:=is_sce_prog_attr_20_800000(@g_appinfo);

 size:=$a0000000; //2GB

 if (ret=False) then
 begin
  ret:=is_sce_prog_attr_20_400000(@g_appinfo);

  if ret then
  begin
   size:=$30000000; //768MB
  end else
  begin
   size:=QWORD(-1);
  end;

 end;

 m_256:=QWORD(ext_game_fmem_addr<>0) * $10000000;

 value:=size;
 if (DMEM_LIMIT < size) then
 begin
  value:=DMEM_LIMIT;
 end;

 FMEM_LIMIT:=size;

 set_budget_limit(PTYPE_BIG_APP,field_dmem_alloc,value);
 set_budget_limit(PTYPE_BIG_APP,field_mlock     ,(EXE_FILE_SIZE + m_256) - FMEM_BASE);
 set_budget_limit(PTYPE_BIG_APP,field_malloc    ,m_256 + EXE_FILE_SIZE);
end;

procedure set_bigapp_limits(size,unknow:QWORD);
var
 m_256:QWORD;
 value:QWORD;
begin
 if (EXE_FILE_SIZE<>size) then
 begin
  //reserve dmem
  EXE_FILE_SIZE:=size;

  m_256:=QWORD(ext_game_fmem_addr<>0) * $10000000;

  value:=FMEM_LIMIT;
  if (DMEM_LIMIT < FMEM_LIMIT) then
  begin
   value:=DMEM_LIMIT;
  end;

  set_budget_limit(PTYPE_BIG_APP,field_dmem_alloc,value);
  set_budget_limit(PTYPE_BIG_APP,field_mlock     ,(EXE_FILE_SIZE + m_256) - FMEM_BASE);
  set_budget_limit(PTYPE_BIG_APP,field_malloc    ,m_256 + EXE_FILE_SIZE);
 end;
end;

function expand_and_reserve_game_fmem(size:QWORD):QWORD;
begin
 if (size <= QWORD(bigapp_max_exe_size - EXE_FILE_SIZE)) then
 begin
  EXE_FILE_SIZE:=EXE_FILE_SIZE + size;
  DMEM_LIMIT   :=DMEM_LIMIT    - size;
  Exit(DMEM_LIMIT + size);
 end;

 Writeln(stderr,'expand_and_reserve_game_fmem=',size);
 Assert(false,'expand_and_reserve_game_fmem');
end;

function expand_budget_limit(size:QWORD):QWORD;
var
 m_256:QWORD;
 value:QWORD;
begin
 Result:=expand_and_reserve_game_fmem(size);

 m_256:=QWORD(ext_game_fmem_addr<>0) * $10000000;

 value:=FMEM_LIMIT;
 if (DMEM_LIMIT < FMEM_LIMIT) then
 begin
  value:=DMEM_LIMIT;
 end;

 set_budget_limit(PTYPE_BIG_APP,field_dmem_alloc,value);
 set_budget_limit(PTYPE_BIG_APP,field_mlock     ,(EXE_FILE_SIZE + m_256) - FMEM_BASE);
 set_budget_limit(PTYPE_BIG_APP,field_malloc    ,m_256 + EXE_FILE_SIZE);

 if (vm_budget_reserve(PTYPE_BIG_APP,field_mlock,size)<>0) then
 begin
  Writeln(stderr,'wrong MLOCK budget accounting');
  Assert(false,'wrong MLOCK budget accounting');
 end;
end;

function allocate_extended_page_table_pool(ExtendedCpuPageTable:QWORD;
                                           ExtendedGpuPageTable:QWORD;
                                           param_3:QWORD):Integer;
var
 GpuPages:QWORD;
 CpuPages:QWORD;
 GpuPagesSize:QWORD;
 size:QWORD;
begin
 Result:=0;

 GpuPages:=(ExtendedGpuPageTable + $7fffff) shr 23;
 CpuPages:=(ExtendedCpuPageTable + $1fffff) shr 21;
 GpuPagesSize:=GpuPages * $4000;

 if ((GpuPagesSize or CpuPages)<> 0) then
 begin
  if (param_3=0) then
  begin
   size:=(QWORD(ord(p_neomode=0)) * $800000) + GpuPagesSize + (CpuPages * $1000);

   if (QWORD(bigapp_max_exe_size - EXE_FILE_SIZE) < size) then
   begin
    Writeln('[KERNEL] WARNING: Failed to allocate extended page table pool.  shortage = '
           ,(EXE_FILE_SIZE - bigapp_max_exe_size) + $fffff + (size shr 20),
           'MiB');
    Exit(ENOMEM);
   end;

   param_3:=expand_budget_limit(size);
  end;

  //reserved physical pages
 end;

end;

function dmem_process_relocated():Integer;
label
 _no_mem_param,
 _next;
var
 proc_param:TSceProcParam;

 mem_param:TSceKernelMemParam;
 mem_param_size:QWORD;

 size :QWORD;
 m_256:QWORD;
 FMEM_SIZE:QWORD;

 mmap_flags:Integer;

 ExtendedCpuPageTable:QWORD;
 ExtendedPageTable   :QWORD;
 ExtendedGpuPageTable:QWORD;
 FlexibleMemorySize  :QWORD;

 ExtendedMemory1:Boolean;
 ExtendedMemory2:Boolean;
begin
 Result:=0;

 if (p_proc.p_budget_ptype=PTYPE_BIG_APP) then
 begin
  mmap_flags:=g_appinfo.mmap_flags and 1;
 end else
 begin
  mmap_flags:=0;
 end;

 proc_param:=Default(TSceProcParam);
 Result:=copy_proc_param(@proc_param);

 if (Result=ENOENT) then
 begin
  _no_mem_param:
  mem_param:=Default(TSceKernelMemParam);
  Result:=0;
 end else
 begin
  if (Result=0) then
  begin
   if (proc_param._sceKernelMemParam=nil) then goto _no_mem_param;

   mem_param_size:=fuword64(proc_param._sceKernelMemParam^.Size);

   if (mem_param_size=QWORD($ffffffffffffffff)) then
   begin
    Result:=EACCES;
   end else
   begin
    if (mem_param_size>sizeof(TSceKernelMemParam)) then
    begin
     mem_param_size:=sizeof(TSceKernelMemParam);
    end;

    mem_param:=Default(TSceKernelMemParam);
    Result:=copyin(proc_param._sceKernelMemParam,@mem_param,mem_param_size);

    if (Result=0) then goto _next;
   end;

  end;
  //
  Writeln('[KERNEL] ERROR: failed to load memory parameter: ',Result);
 end;

 _next:

 if (Byte((mmap_flags xor 1) or ord(p_proc.p_self_fixed=0))=0) then
 begin
  p_proc.p_self_fixed:=0;

  ExtendedMemory1:=true;
  if (p_proc.p_sdk_version < $5000000) then
  begin
   ExtendedMemory1:=fubyte(mem_param.sceKernelExtendedMemory1^)=1;
  end;

  m_256:=QWORD(ext_game_fmem_addr<>0) * $10000000;

  if (p_neomode<>0) and
     ((not ExtendedMemory1) or (IGNORE_EXTENDED_DMEM_BASE<>0)) then
  begin
   FMEM_SIZE:=$10000000 - EXE_FILE_SIZE;

   size:=FMEM_SIZE;
   if (FMEM_LIMIT <= FMEM_SIZE) then
   begin
    size:=FMEM_LIMIT;
   end;

   DMEM_LIMIT:=FMEM_SIZE;

   set_budget_limit(PTYPE_BIG_APP,field_dmem_alloc,size);
   set_budget_limit(PTYPE_BIG_APP,field_mlock     ,(EXE_FILE_SIZE + m_256) - FMEM_BASE);
   set_budget_limit(PTYPE_BIG_APP,field_malloc    ,m_256 + EXE_FILE_SIZE);
  end;

  ExtendedMemory2:=true;
  if (p_proc.p_sdk_version < $5000000) then
  begin
   ExtendedMemory2:=fubyte(mem_param.sceKernelExtendedMemory2^)=1;
  end;

  if (IGNORE_EXTENDED_DMEM_BASE=0) and
     (p_neomode=0) then
  begin
   if (ExtendedMemory2) then
   begin
    Writeln('[System] : SCE_KERNEL_EXTENDED_DMEM_BASE_128 was ignored');
   end else
   begin
    FMEM_SIZE:=$8000000 - EXE_FILE_SIZE;

    size:=FMEM_SIZE;

    if (FMEM_LIMIT <= FMEM_SIZE) then
    begin
     size:=FMEM_LIMIT;
    end;

    DMEM_LIMIT:=FMEM_SIZE;

    set_budget_limit(PTYPE_BIG_APP,field_dmem_alloc,size);
    set_budget_limit(PTYPE_BIG_APP,field_mlock     ,(EXE_FILE_SIZE + m_256) - FMEM_BASE);
    set_budget_limit(PTYPE_BIG_APP,field_malloc    ,m_256 + EXE_FILE_SIZE);
   end;
  end;

  ExtendedCpuPageTable:=fuword64(mem_param.sceKernelExtendedCpuPageTable^);
  ExtendedPageTable   :=fuword64(mem_param.sceKernelExtendedPageTable^);


  if (int64(ExtendedCpuPageTable) < 0) and
     (int64(ExtendedPageTable) < 1) then
  begin
   ExtendedCpuPageTable:=0;
  end else
  if (ExtendedCpuPageTable > $1000000000) then
  begin
   ExtendedCpuPageTable:=0;
   Writeln(stderr,'[KERNEL] ERROR: The extended CPU page table pool must be smaller than 64GiB');
   Result:=ENOMEM;
  end;

  ExtendedGpuPageTable:=fuword64(mem_param.sceKernelExtendedGpuPageTable^);

  ExtendedGpuPageTable:=(not SarInt64(ExtendedGpuPageTable,63)) and ExtendedGpuPageTable;

  // MAX 64GB
  if (ExtendedGpuPageTable < $1000000001) then
  begin
   ExtendedSize:=0;
   FMEM_SIZE:=bigapp_max_exe_size;

   if (Result=0) then
   begin
    Result:=allocate_extended_page_table_pool(ExtendedCpuPageTable,ExtendedGpuPageTable,0);

    FMEM_SIZE:=bigapp_max_exe_size;

    if (Result=0) then
    begin
     if (mem_param.sceKernelFlexibleMemorySize=nil) then
     begin
      Result:=0;
     end else
     begin
      FlexibleMemorySize:=fuword64(mem_param.sceKernelFlexibleMemorySize^);

      Result:=0;
      FMEM_SIZE:=bigapp_max_exe_size;

      if (FlexibleMemorySize<>QWORD($ffffffffffffffff)) then
      begin
       FMEM_SIZE:=FMEM_BASE + FlexibleMemorySize;

       if (bigapp_max_exe_size < FMEM_SIZE) or
          (FMEM_SIZE < bigapp_size) or
          ((FMEM_SIZE and QWORD($ffffffffffffc000))<>FMEM_SIZE) then
       begin
        Writeln(stderr,'[KERNEL] ERROR: invalid FMEM size (0x',HexStr(FlexibleMemorySize,16),') is specified.');
        Result:=EINVAL;
        FMEM_SIZE:=bigapp_max_exe_size;
       end;
      end;
     end;
    end;
   end;

  end else
  begin
   Writeln('[KERNEL] ERROR: The extended GPU page table pool must be smaller than 64GiB');
   Result:=ENOMEM;
   ExtendedSize:=0;
   FMEM_SIZE:=bigapp_max_exe_size;
  end;

  if (FMEM_SIZE < EXE_FILE_SIZE) then
  begin
   Writeln(stderr,'[KERNEL] ERROR: The executable file size (= 0x',HexStr(EXE_FILE_SIZE,16),
                  ') < specified FMEM size (=0x',HexStr(FMEM_SIZE,16),')');
   if (Result=0) then
   begin
    Result:=Integer($a0020326);
   end;
  end else
  if (Result=0) then
  begin
   Result:=0;
   set_bigapp_limits(FMEM_SIZE,0);
  end;

 end else
 if (ExtendedSize=0) and
    (mmap_flags<>0) then
 begin
  ExtendedPageTable:=fuword64(mem_param.sceKernelExtendedPageTable^);

  if (Int64(ExtendedPageTable) < 1) or
     (ExtendedPageTable=$100000000) then
  begin
   if (Int64(ExtendedPageTable) > 0) then
   begin
    allocate_extended_page_table_pool(ExtendedPageTable,0,0);
   end;
  end else
  begin
   Writeln(stderr,'[KERNEL] ERROR: failed to extend page table pool: ',Result);
  end;

 end;

end;

function get_mlock_avail():QWORD;
var
 m:QWORD;
 limit:QWORD;
begin
 Result:=0;

 //HACK: Features vm_map_wire need to be added
 m    :=vm_budget_used (p_proc.p_budget_ptype,field_mlock);
 limit:=vm_budget_limit(p_proc.p_budget_ptype,field_mlock);

 if (limit>m) then
 begin
  Result:=limit-m;
 end;
end;

function get_mlock_total():QWORD;
begin
 Result:=vm_budget_limit(p_proc.p_budget_ptype,field_mlock)
end;

function sys_budget_create(name:pchar;ptype:DWORD;new:Pointer;count:DWORD;prev:Pointer):Integer;
begin
 //name  != null
 //ptype -> [0..3]  (proc_type)
 //new   -> p_budget_resource
 //count -> [0..11] (new/prev)
 //prev  -> p_budget_resource
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_delete(key:Integer):Integer;
begin
 //key -> id_table
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_get(key:Integer;ptr:Pointer;psize:PInteger):Integer;
begin
 //key   -> [-2..-5] (budget limits [0..3]) else -> id_table
 //ptr   -> p_budget_resource
 //psize -> in/out size
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_set(key:Integer):Integer;
begin
 //key -> id_table
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_get_ptype(pid:Integer):Integer;
var
 td:p_kthread;
begin
 //sceKernelGetProcessType

 td:=curkthread;
 if (td=nil) then Exit(-1);

 if (pid<>-1) and
    (pid<>p_proc.p_pid) then
 begin
  Exit(ENOSYS);
 end;

 td^.td_retval[0]:=p_proc.p_budget_ptype;

 Result:=0;
end;

function sys_budget_get_ptype_of_budget(key:Integer):Integer;
begin
 //key -> id_table
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;

function sys_budget_getid():Integer;
begin
 Exit(ENOSYS); //sceSblACMgrIsSystemUcred
end;


end.



