unit dev_gc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf;

procedure gc_initialize();

implementation

uses
 errno,
 kern_authinfo,
 vm,
 vmparam,
 kern_thr,
 trap;

var
 gc_mmap_ptr:Pointer=nil;

Function gc_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('gc_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  0:;
  else
   begin
    print_backtrace(stderr,Pointer(curkthread^.td_frame.tf_rip),Pointer(curkthread^.td_frame.tf_rbp),0);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

Function gc_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
begin
 if sceSblACMgrHasUseHp3dPipeCapability(@g_authinfo) then
 begin
  Exit(EINVAL);
 end;

 if (offset>=PAGE_SIZE) then
 begin
  Exit(EPERM);
 end;

 paddr^:=offset + QWORD(gc_mmap_ptr);
 memattr^:=0;

 Result:=0;
end;

const
 gc_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'gc';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :nil;
  d_write      :nil;
  d_ioctl      :@gc_ioctl;
  d_poll       :nil;
  d_mmap       :@gc_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :nil;
  d_purge      :nil;
  d_mmap_single:nil;
 );

procedure gc_initialize();
begin
 gc_mmap_ptr:=Pointer($fe0200000);

 make_dev(@gc_cdevsw,0,0,0,&666,'gc',[]);
end;


end.

