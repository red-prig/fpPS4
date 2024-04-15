unit dev_rng;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure rng_init();

implementation

uses
 errno,
 md_arc4random;

Function rng_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('rng_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $40445301: //_get_genuine_random
            begin
             arc4rand(data+4,64,0);
             PInteger(data)^:=0;
            end;
  $40445302: //Fips186Prng
            begin
             arc4rand(data+4,64,0);
             PInteger(data)^:=0;
            end;
  else
   Result:=ENOIOCTL;
 end;

end;

const
 devw_rng:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'rng';
  d_open        :nil;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :nil;
  d_write       :nil;
  d_ioctl       :@rng_ioctl;
  d_poll        :nil;
  d_mmap        :nil;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :nil;
  d_purge       :nil;
  d_mmap_single :nil;
  d_mmap_single2:nil;
 );

procedure rng_init();
begin
 make_dev(@devw_rng,0,0,0,&444,'rng',[]);
end;

end.

