unit dev_random;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure random_init();

implementation

uses
 errno,
 vuio,
 vfilio,
 vpoll,
 md_arc4random;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

const
 RANDOM_MINOR=0;

Function random_close(dev:p_cdev;fflag,devtype:Integer):Integer;
begin
 Result:=0;
end;

Function random_read(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 iov  :p_iovec;
 cnt  :QWORD;
begin

 while (uio^.uio_resid >0) and
       (uio^.uio_iovcnt>0) do
 begin
  iov:=uio^.uio_iov;
  cnt:=iov^.iov_len;

  if (cnt > uio^.uio_resid) then
  begin
   cnt:=uio^.uio_resid;
  end;

  arc4rand(iov^.iov_base,cnt,0);

  Inc(iov^.iov_base  ,cnt);
  Dec(iov^.iov_len   ,cnt);
  Dec(uio^.uio_resid ,cnt);
  Inc(uio^.uio_offset,cnt);
  Inc(uio^.uio_iov);
  Dec(uio^.uio_iovcnt);
 end;

 Exit(0);
end;

Function random_write(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 iov  :p_iovec;
 cnt  :QWORD;
begin

 while (uio^.uio_resid >0) and
       (uio^.uio_iovcnt>0) do
 begin
  iov:=uio^.uio_iov;
  cnt:=iov^.iov_len;

  if (cnt > uio^.uio_resid) then
  begin
   cnt:=uio^.uio_resid;
  end;

  //

  Inc(iov^.iov_base  ,cnt);
  Dec(iov^.iov_len   ,cnt);
  Dec(uio^.uio_resid ,cnt);
  Inc(uio^.uio_offset,cnt);
  Inc(uio^.uio_iov);
  Dec(uio^.uio_iovcnt);
 end;

 Exit(0);
end;

Function random_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 LOG_TRACE('random_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  //Really handled in upper layer
  FIOASYNC:;
  FIONBIO :;
  else
   Result:=ENOTTY;
 end;

end;

Function random_poll(dev:p_cdev;events:Integer):Integer;
var
 revents:Integer;
begin
 revents:=0;

 if (events and (POLLIN or POLLRDNORM))<>0 then
 begin
  revents:=events and (POLLIN or POLLRDNORM);
 end;

 Result:=(revents);
end;

const
 random_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'random';
  d_close       :@random_close;
  d_read        :@random_read;
  d_write       :@random_write;
  d_ioctl       :@random_ioctl;
  d_poll        :@random_poll;
 );

procedure random_init();
var
 random_dev:p_cdev;
begin
 random_dev:=make_dev_credf(MAKEDEV_ETERNAL_KLD,
                            @random_cdevsw,
                            RANDOM_MINOR,
                            UID_ROOT,
                            GID_WHEEL,
                            &666,
                            'random',
                            []);

 make_dev_alias(random_dev, 'urandom', []);
end;

end.

