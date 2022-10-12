unit ps4_libSceScreenShot;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceScreenShotSetOverlayImage(
          filePath:PChar;
          offsetx:Integer;
          offsety:Integer
         ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceScreenShotSetOverlayImage:',filePath);
 Result:=0;
end;

function ps4_sceScreenShotSetOverlayImageWithOrigin(
          filePath:PChar;
          marginX:Integer;
          marginY:Integer;
          origin:Integer //SceScreenShotOrigin
         ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceScreenShotSetOverlayImageWithOrigin:',filePath);
 Result:=0;
end;

function ps4_sceScreenShotEnable():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceScreenShotEnable');
 Result:=0;
end;

function ps4_sceScreenShotDisable():Integer; SysV_ABI_CDecl;
begin
 Writeln('sceScreenShotDisable');
 Result:=0;
end;

function Load_libSceScreenShot(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceScreenShot');

 lib^.set_proc($6A11E139FF903644,@ps4_sceScreenShotSetOverlayImage);
 lib^.set_proc($EF7590E098F49C92,@ps4_sceScreenShotSetOverlayImageWithOrigin);
 lib^.set_proc($DB1C54B6E0BF4731,@ps4_sceScreenShotEnable);
 lib^.set_proc($B4861FD16E554E2F,@ps4_sceScreenShotDisable);
end;

initialization
 ps4_app.RegistredPreLoad('libSceScreenShot.prx',@Load_libSceScreenShot);

end.


