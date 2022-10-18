unit ps4_libSceMouse;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

type
 pSceMouseOpenParam=^SceMouseOpenParam;
 SceMouseOpenParam=packed record
  behaviorFlag:Byte;
  reserved:array[0..6] of Byte;
 end;

 pSceMouseData=^SceMouseData;
 SceMouseData=packed record
  timestamp:QWORD;
  connected:Integer;
  buttons:DWORD;
  xAxis:Integer;
  yAxis:Integer;
  wheel:Integer;
  tilt :Integer;
  reserve:qword;
 end;

const
 SCE_MOUSE_ERROR_INVALID_ARG    =-2132869119; //0x80DF0001
 SCE_MOUSE_ERROR_INVALID_HANDLE =-2132869117; //0x80DF0003
 SCE_MOUSE_ERROR_ALREADY_OPENED =-2132869116; //0x80DF0004
 SCE_MOUSE_ERROR_NOT_INITIALIZED=-2132869115; //0x80DF0005
 SCE_MOUSE_ERROR_FATAL          =-2132868865; //0x80DF00FF

 SCE_MOUSE_MAX_DATA_NUM=64;

function ps4_sceMouseInit:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMouseInit');
 Result:=0;
end;

function ps4_sceMouseOpen(userId,_type,index:Integer;
                          pParam:pSceMouseOpenParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMouseOpen');
 Result:=0;
end;

function ps4_sceMouseRead(handle:Integer;
                          pData:pSceMouseData;
                          num:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pData=nil) then Exit(SCE_MOUSE_ERROR_INVALID_HANDLE);
 if (num<=0) or (num>SCE_MOUSE_MAX_DATA_NUM) then Exit(SCE_MOUSE_ERROR_INVALID_HANDLE);

 pData[0]:=Default(SceMouseData);
 pData[0].connected:=Integer(False);
 Result:=1;
end;

function Load_libSceMouse(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceMouse');
 lib^.set_proc($42CD305AE96097B5,@ps4_sceMouseInit);
 lib^.set_proc($45AAB16487FA0EF1,@ps4_sceMouseOpen);
 lib^.set_proc($C7CAA75EA87FB623,@ps4_sceMouseRead);
end;

initialization
 ps4_app.RegistredPreLoad('libSceMouse.prx',@Load_libSceMouse);

end.

