unit dev_dce;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf;

procedure dce_initialize();

implementation

uses
 errno,
 kern_thr,
 trap,
 vm,
 vm_object,
 sys_event;

Function dce_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('dce_ioctl(0x',HexStr(cmd,8),')');

 Assert(false);
end;

Function dce_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
begin
 Result:=0;

 Assert(false);
end;

Function dce_mmap_single(cdev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;obj:p_vm_object_t;nprot:Integer):Integer;
begin
 Result:=0;

 Assert(false);
end;

Function dce_kqfilter(dev:p_cdev;kn:p_knote):Integer;
begin
 Result:=0;

 Assert(false);
end;

const
 dce_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'dce';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :nil;
  d_write      :nil;
  d_ioctl      :@dce_ioctl;
  d_poll       :nil;
  d_mmap       :@dce_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :@dce_kqfilter;
  d_purge      :nil;
  d_mmap_single:@dce_mmap_single;
 );

procedure dce_initialize();
begin
 make_dev(@dce_cdevsw,0,0,0,0666,'dce',[]);
end;


end.

