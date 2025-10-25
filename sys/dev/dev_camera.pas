unit dev_camera;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure camera_init();

implementation

uses
 errno,
 vm;

Function camera_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('camera_ioctl(0x',HexStr(cmd,8),')');

 case cmd of

  $40408E22: //mmap config?
    begin
     PDWORD(data)[0]:=0;
     PDWORD(data)[1]:=0;

     //if sceSblACMgrIsShelluiProcess(*) {
     //  *(byte *)data = *(byte *)data | 1;
     //}

    end;

   //$c0288e14:; //sceCameraDevOpen

  else
   Assert(false);
 end;

end;

Function camera_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
begin
 Writeln('TODO:camera_mmap');
 Exit(EINVAL);
end;

const
 camera_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'camera_dev';
  d_open        :nil;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :nil;
  d_write       :nil;
  d_ioctl       :@camera_ioctl;
  d_poll        :nil;
  d_mmap        :@camera_mmap;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :nil;
  d_purge       :nil;
  d_mmap_single :nil;
  d_mmap_single2:nil;
 );

procedure camera_init();
begin
 make_dev(@camera_cdevsw,0,0,0,&644,'camera',[]);
end;


end.

