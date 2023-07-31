unit dev_null;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf;

const
 ZERO_REGION_SIZE=(2 * 1024 * 1024); // 2MB

var
 zero_region:Pointer;
 _zero_region:array[0..ZERO_REGION_SIZE-1] of Byte;

procedure init_zero_region;

Function null_modevent(_mod,_type:Integer):Integer; //DEV_MODULE(nil, null_modevent, nil);

implementation

uses
 errno,
 vuio,
 vdisk,
 vfilio,
 subr_uio;

var
{ For use with destroy_dev(9). }
 null_dev:p_cdev;
 zero_dev:p_cdev;

Function null_write(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;             forward;
Function null_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer; forward;
Function zero_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer; forward;
Function zero_read(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;              forward;

const
 null_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'null';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :d_read_t(@_nullop);
  d_write      :@null_write;
  d_ioctl      :@null_ioctl;
  d_poll       :nil;
  d_mmap       :nil;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :nil;
  d_purge      :nil;
  d_mmap_single:nil;
 );

 zero_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :D_MMAP_ANON;
  d_name       :'zero';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :@zero_read;
  d_write      :@null_write;
  d_ioctl      :@zero_ioctl;
  d_poll       :nil;
  d_mmap       :nil;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :nil;
  d_purge      :nil;
  d_mmap_single:nil;
 );

{ ARGSUSED }
Function null_write(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
begin
 uio^.uio_resid:=0;

 Exit(0);
end;

{ ARGSUSED }
Function null_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 error:Integer;
begin
 error:=0;

 case (cmd) of
  DIOCSKERNELDUMP:
   begin
    error:=EPERM;
    //error:=priv_check(td, PRIV_SETDUMPER);
    //if (error=0)
    // error:=set_dumper(nil);
   end;
  FIONBIO:;
  FIOASYNC:
   begin
    if (PInteger(data)^<>0) then
    begin
     error:=EINVAL;
    end;
   end;
  else
   error:=ENOIOCTL;
 end;

 Exit(error);
end;

{ ARGSUSED }
Function zero_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 error:Integer;
begin
 error:=0;

 case (cmd) of
  FIONBIO:;
  FIOASYNC:
   begin
    if (PInteger(data)^<>0) then
    begin
     error:=EINVAL;
    end;
   end;
  else
   error:=ENOIOCTL;
 end;

 Exit(error);
end;

procedure init_zero_region;
begin
 if (zero_region=nil) then
 begin
  FillChar(_zero_region,SizeOf(_zero_region),0);
  zero_region:=@_zero_region;
 end;
end;

{ ARGSUSED }
Function zero_read(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 zbuf:Pointer;
 len:QWORD;
 error:Integer;
begin
 error:=0;

 Assert(uio^.uio_rw=UIO_READ,'Cant be in %s for write');

 init_zero_region;

 zbuf:=zero_region;
 while (uio^.uio_resid > 0) and (error=0) do
 begin
  len:=uio^.uio_resid;
  if (len > ZERO_REGION_SIZE) then
  begin
   len:=ZERO_REGION_SIZE;
  end;
  error:=uiomove(zbuf, len, uio);
 end;

 Exit(error);
end;

{ ARGSUSED }
Function null_modevent(_mod,_type:Integer):Integer;
begin
 case (_type) of
  MOD_LOAD:
   begin
    null_dev:=make_dev_credf(0, @null_cdevsw, 0, UID_ROOT, GID_WHEEL, &666, 'null',[]);
    zero_dev:=make_dev_credf(0, @zero_cdevsw, 0, UID_ROOT, GID_WHEEL, &666, 'zero',[]);
   end;

  MOD_UNLOAD:
   begin
    destroy_dev(null_dev);
    destroy_dev(zero_dev);
   end;

  MOD_SHUTDOWN:;

  else
   Exit(EOPNOTSUPP);
 end;

 Exit(0);
end;

end.

