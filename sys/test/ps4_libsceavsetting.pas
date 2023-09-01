unit ps4_libSceAvSetting;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}
{$WARN 4110 off}

interface

uses
  subr_dynlib;

implementation

function ps4_sceAvSettingInit:Integer;
begin
 Result:=0;
end;

//

function Load_libSceAvSetting(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAvSetting');

 lib:=Result^.add_lib('libSceAvSetting');
 lib.set_proc($10865D5934FB65DC,@ps4_sceAvSettingInit);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceAvSetting.prx',@Load_libSceAvSetting);

end.

