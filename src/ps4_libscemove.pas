unit ps4_libSceMove;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

const
 SCE_MOVE_ERROR_INVALID_ARG   =-2131886077; //0x80EE0003
 SCE_MOVE_ERROR_INVALID_HANDLE=-2131886076; //0x80EE0004
 SCE_MOVE_ERROR_NOT_INIT      =-2131886079; //0x80EE0001

 SCE_MOVE_RETURN_CODE_NO_CONTROLLER_CONNECTED=1;

 SCE_MOVE_MAX_STATE_NUM=32;

type
 pSceMoveDeviceInfo=^SceMoveDeviceInfo;
 SceMoveDeviceInfo=packed record
  sphereRadius:Single;
  accelToSphereOffset:array[0..2] of Single;
 end;

 SceMoveButtonData=packed record
  digitalButtons:Word;
  analogT:Word;
 end;

 SceMoveExtensionPortData=packed record
  status      :Word;
  digital1    :Word;
  digital2    :Word;
  analogRightX:Word;
  analogRightY:Word;
  analogLeftX :Word;
  analogLeftY :Word;
  custom:array[0..4] of AnsiChar;
 end;

 pSceMoveData=^SceMoveData;
 SceMoveData=packed record
  accelerometer:array[0..2] of Single;
  gyro:array[0..2] of Single;
  pad:SceMoveButtonData;
  ext:SceMoveExtensionPortData;
  timestamp:QWORD;
  counter:Integer;
  temperature:Single;
 end;

function ps4_sceMoveInit:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMoveInit');
 Result:=0;
end;

function ps4_sceMoveOpen(userId,_type,index:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMoveOpen:',userId,' ',_type,' ',index);
 Result:=0;
end;

function ps4_sceMoveClose(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceMoveClose:',handle);
 Result:=0;
end;

function ps4_sceMoveGetDeviceInfo(handle:Integer;pInfo:pSceMoveDeviceInfo):Integer; SysV_ABI_CDecl;
begin
 if (pInfo=nil) then Exit(SCE_MOVE_ERROR_INVALID_ARG);
 pInfo^:=Default(SceMoveDeviceInfo);
 Result:=SCE_MOVE_RETURN_CODE_NO_CONTROLLER_CONNECTED;
end;

function ps4_sceMoveReadStateRecent(handle:Integer;
                                    timestamp:QWORD;
                                    pData:pSceMoveData;
                                    num:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (pData=nil) or (num=nil) then Exit(SCE_MOVE_ERROR_INVALID_ARG);
 pData^:=Default(SceMoveData);
 num^:=0;
 Result:=SCE_MOVE_RETURN_CODE_NO_CONTROLLER_CONNECTED;
end;

function Load_libSceMove(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceMove');
 lib^.set_proc($8F521313F1282661,@ps4_sceMoveInit);
 lib^.set_proc($1F30BAD0C7E32715,@ps4_sceMoveOpen);
 lib^.set_proc($5D7EB0971A47C9EA,@ps4_sceMoveClose);
 lib^.set_proc($1965D3CB1B3841B1,@ps4_sceMoveGetDeviceInfo);
 lib^.set_proc($7F66DCA4AEA425F8,@ps4_sceMoveReadStateRecent);
end;

initialization
 ps4_app.RegistredPreLoad('libSceMove.prx',@Load_libSceMove);

end.

