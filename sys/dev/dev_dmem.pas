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
 dmem_map,
 kern_dmem,
 sys_vm_object,
 trap;

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

Function dmem_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

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
              Result:=dmem_map_query_available(@dmem,start,__end,align,start,osize);
             end;
            end;

  $C0288001: //sceKernelAllocateDirectMemory
            begin
             with PAllocateDirectMemory(data)^ do
             begin
              Result:=dmem_map_alloc(@dmem,start,__end,len,align,mtype,start);
             end;
            end

  else
   begin
    print_backtrace_td(stderr);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

Function dmem_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
begin
 Result:=0;

 Writeln('dmem_mmap("',dev^.si_name,'",0x',HexStr(offset,8),',0x',HexStr(paddr),',',nprot,')');

 print_backtrace_td(stderr);
 Assert(False);
end;

Function dmem_mmap_single2(dev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer):Integer;
//byte *maxprotp,uint *flagsp
begin

 Result:=0;

 Writeln('dmem_mmap_single2("',dev^.si_name,'",0x',HexStr(offset^,8),',0x',HexStr(size,8),',',nprot,')');

 print_backtrace_td(stderr);
 Assert(False);
end;

const
 dmem_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'dmem';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :nil;
  d_write      :nil;
  d_ioctl      :@dmem_ioctl;
  d_poll       :nil;
  d_mmap       :@dmem_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :nil;
  d_purge      :nil;
  d_mmap_single:nil;
 );

procedure dmemdev_init();
var
 i:Integer;
begin
 For i:=0 to 2 do
 begin
  make_dev(@dmem_cdevsw,i,0,0,&777,'dmem%d',[i]);
 end;
end;


end.

