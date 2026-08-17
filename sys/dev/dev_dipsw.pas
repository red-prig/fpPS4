unit dev_dipsw;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure dipsw_init();

implementation

uses
 errno,
 sys_bootparam,
 kern_authinfo;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

Function dipsw_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 case cmd of
  $20008800:LOG_INFO('dipsw_ioctl("InitializeDipsw")');
  $40048806:LOG_INFO('dipsw_ioctl("isDevelopmentMode")');
  $40048807:LOG_INFO('dipsw_ioctl("isTestKit")');
  $40088808:LOG_INFO('dipsw_ioctl("IsDisableRazor")');
  $40088809:LOG_INFO('dipsw_ioctl("IsDisableBinaryVersionCheck")');
  $80028801:LOG_INFO('dipsw_ioctl("SetDipsw")');
  $80028802:LOG_INFO('dipsw_ioctl("UnsetDipsw")');
  $c0088803:LOG_INFO('dipsw_ioctl("CheckDipsw")');
  $80108804:LOG_INFO('dipsw_ioctl("ReadDipswData")');
  $80108805:LOG_INFO('dipsw_ioctl("WriteDipswData")');
  $8010880a:LOG_INFO('dipsw_ioctl("GetAllDipswData")');
  else
   begin
    LOG_ERROR('dipsw_ioctl(0x',HexStr(cmd,8),')');
    Exit(EINVAL);
   end;
 end;

 if not sceSblACMgrIsSystemUcred(@g_authinfo) then
 begin
  //allow in sandbox:
  case cmd of
   $40048806:; //isDevelopmentMode
   $40048807:; //isTestKit
   $40088808:; //IsDisableRazor
   $40088809:; //IsDisableBinaryVersionCheck
   else
    begin
     LOG_ERROR('dipsw_ioctl(0x',HexStr(cmd,8),')');
     Exit(EINVAL);
    end;
  end;
 end;

 case cmd of
  $40048806:PInteger(data)[0]:=p_isDevelopmentMode;
  $40048807:PInteger(data)[0]:=p_isTestKit;
  $40088808:PInteger(data)[1]:=p_IsDisableRazor;
  $40088809:PInteger(data)[1]:=p_IsDisableBinaryVersionCheck;
  else;
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

