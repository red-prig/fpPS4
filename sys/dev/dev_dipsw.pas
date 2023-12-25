unit dev_dipsw;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf;

procedure dipsw_init();

implementation

uses
 errno;

Function dipsw_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('dipsw_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $20008800:;
  $40048806:;
  $40048807:;
  $40088808:;
  $40088809:;
  $80028801:;
  $80028802:;
  $c0088803:;
  $80108804:;
  $80108805:;
  $8010880a:;
  else
   Result:=EINVAL;
 end;

end;

const
 dipsw_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'dipsw_dev';
  d_open        :nil;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :nil;
  d_write       :nil;
  d_ioctl       :@dipsw_ioctl;
  d_poll        :nil;
  d_mmap        :nil;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :nil;
  d_purge       :nil;
  d_mmap_single :nil;
  d_mmap_single2:nil;
 );

procedure dipsw_init();
begin
 make_dev(@dipsw_cdevsw,0,0,0,&644,'dipsw',[]);
end;


end.

