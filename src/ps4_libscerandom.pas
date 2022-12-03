unit ps4_libSceRandom;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  sys_signal,
  sys_kernel;

const
 SCE_RANDOM_MAX_SIZE=64;

implementation

function ps4_sceRandomGetRandomNumber(buf:Pointer;size:qword):Integer; SysV_ABI_CDecl;
begin
 if (buf=nil) or (size>SCE_RANDOM_MAX_SIZE) then Exit(EINVAL);

 _sig_lock;
 Result:=BCryptGenRandom(nil,buf,size,BCRYPT_USE_SYSTEM_PREFERRED_RNG);
 _sig_unlock;

 if (Result=0) then
 begin
  _set_errno(0);
 end else
 begin
  Result:=ENODEV;
  _set_errno(ENODEV);
 end;
end;

function Load_libSceRandom(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceRandom');
 lib^.set_proc($3C8EE32198F8A5C1,@ps4_sceRandomGetRandomNumber);
end;

initialization
 ps4_app.RegistredPreLoad('libSceRandom.prx',@Load_libSceRandom);

end.

