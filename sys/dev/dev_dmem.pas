unit dev_dmem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf;

procedure dmemdev_init();

implementation

uses
 errno,
 vuio,
 subr_uio,
 vm,
 vmparam,
 dmem_map,
 kern_dmem,
 sys_vm_object,
 vm_pager,
 kern_authinfo,
 subr_backtrace;

type
 PAvailableDirectMemorySize=^TAvailableDirectMemorySize;
 TAvailableDirectMemorySize=packed record
  start:QWORD; //in,out
  __end:QWORD; //in
  align:QWORD; //in
  osize:QWORD; //out
 end;

 PAllocateDirectMemory=^TAllocateDirectMemory;
 TAllocateDirectMemory=packed record
  start:QWORD; //in,out
  __end:QWORD; //in
  len  :QWORD; //in
  align:QWORD; //in
  mtype:DWORD;
 end;

 PReleaseDirectMemory=^TReleaseDirectMemory;
 TReleaseDirectMemory=packed record
  start:QWORD; //in
  len  :QWORD; //in
 end;

Function dmem_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 dmap:p_dmem_obj;
begin
 Result:=0;

 dmap:=dev^.si_drv1;

 Writeln('dmem_ioctl("',dev^.si_name,'",0x',HexStr(cmd,8),',0x',HexStr(fflag,8),')');

 case cmd of
  $4008800A: //sceKernelGetDirectMemorySize
            begin
             PQWORD(data)^:=SCE_KERNEL_MAIN_DMEM_SIZE;
            end;

  $C0208016: //sceKernelAvailableDirectMemorySize
            begin
             with PAvailableDirectMemorySize(data)^ do
             begin
              Result:=dmem_map_query_available(dmap^.dmem,start,__end,align,start,osize);
             end;
            end;

  $C0288001: //sceKernelAllocateDirectMemory
            begin
             with PAllocateDirectMemory(data)^ do
             begin
              Result:=dmem_map_alloc(dmap^.dmem,start,__end,len,align,mtype,start);
             end;
            end;

  $80108002: //sceKernelReleaseDirectMemory
            begin
             with PReleaseDirectMemory(data)^ do
             begin
              Result:=dmem_map_release(dmap^.dmem,start,len,False);
             end;
            end;

  else
   begin
    print_backtrace_td(stderr);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

Function dmem_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
var
 dmap:p_dmem_obj;
 entry:p_dmem_map_entry;
 r:Boolean;
begin
 Result:=0;

 if is_sce_prog_attr_40_800000(@g_authinfo) then
 begin
  Exit(EPERM);
 end;

 if is_sce_prog_attr_40_400000(@g_authinfo) then
 begin
  Exit(EPERM);
 end;

 Writeln('dmem_mmap("',dev^.si_name,'",0x',HexStr(offset,8),',0x',HexStr(paddr),',',nprot,')');

 dmap:=dev^.si_drv1;

 entry:=nil;

 dmem_map_lock(dmap^.dmem);

  r:=dmem_map_lookup_entry(dmap^.dmem,
                           OFF_TO_IDX(offset),
                           @entry);

 dmem_map_unlock(dmap^.dmem);

 if not r then
 begin
  Exit(-1);
 end;

 paddr^:=offset {+ };
 memattr^:=0;
end;

Function dmem_mmap_single2(dev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer;maxprotp:p_vm_prot_t;flagsp:PInteger):Integer;
var
 dmap:p_dmem_obj;
 ofs:vm_ooffset_t;
 flags:Integer;
begin
 Result:=0;

 if is_sce_prog_attr_40_800000(@g_authinfo) then
 begin
  Exit(EPERM);
 end;

 if is_sce_prog_attr_40_400000(@g_authinfo) then
 begin
  Exit(EPERM);
 end;

 Writeln('dmem_mmap_single2("',dev^.si_name,'",0x',HexStr(offset^,8),',0x',HexStr(size,8),',',nprot,')');

 ofs:=offset^;

 if (ofs > -1) and (size <= $5000000000 - ofs) then
 begin
  //
 end else
 begin
  Exit(EACCES);
 end;

 dmap:=dev^.si_drv1;

 flags:=flagsp^;

 Result:=dmem_map_set_mtype(dmap^.dmem,
                            OFF_TO_IDX(ofs),
                            OFF_TO_IDX(ofs+size),
                            -1,
                            nprot,
                            flags);

 if (Result<>0) then Exit;

 if ((maxprotp^ and nprot)=nprot) then
 begin
  Exit(EACCES);
 end;

 obj^:=dmap^.vobj;
end;

Function dmem_open(dev:p_cdev;oflags,devtype:Integer):Integer;
begin
 Result:=0;

 if is_sce_prog_attr_40_800000(@g_authinfo) then
 begin
  Exit(EPERM);
 end;

 if is_sce_prog_attr_40_400000(@g_authinfo) then
 begin
  Exit(EPERM);
 end;
end;

const
 dmem_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'dmem';
  d_open        :@dmem_open;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :nil;
  d_write       :nil;
  d_ioctl       :@dmem_ioctl;
  d_poll        :nil;
  d_mmap        :@dmem_mmap;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :nil;
  d_purge       :nil;
  d_mmap_single :nil;
  d_mmap_single2:@dmem_mmap_single2;
 );

procedure dmemdev_init();
var
 dev:p_cdev;
 obj:vm_object_t;
 i:Integer;
begin
 For i:=0 to 2 do
 begin
  dev:=make_dev(@dmem_cdevsw,i,0,0,&777,'dmem%d',[i]);
  dev^.si_drv1:=@dmem_maps[i];
  //
  obj:=vm_pager_allocate(OBJT_DEVICE,dev,0,0,0);
  obj^.size:=$1400000;
  obj^.flags:=obj^.flags or OBJ_DMEM_EXT;
  obj^.un_pager.map_base:=Pointer(VM_MIN_GPU_ADDRESS);
  vm_object_reference(obj);
  //
  dmem_maps[i].dmem:=@kern_dmem.dmem;
  dmem_maps[i].vobj:=obj;
 end;
end;


end.

