unit ps4_libSceHmd;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

implementation


function ps4_sceHmdInitialize315():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionQueryOnionBuffSize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionQueryOnionBuffAlign():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionQueryGarlicBuffSize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionQueryGarlicBuffAlign():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionSetUserEventStart():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionSetUserEventEnd():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdGetDeviceInformation():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionStop():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionUnsetDisplayBuffers():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionFinalize():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdReprojectionClearUserEventStart():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHmdTerminate():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceHmd(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceHmd');
 lib^.set_proc($B3F27AE9AAFD839D,@ps4_sceHmdInitialize315);
 lib^.set_proc($90B50090DE9AD5EF,@ps4_sceHmdReprojectionQueryOnionBuffSize);
 lib^.set_proc($216C9B59B47FC6F0,@ps4_sceHmdReprojectionQueryOnionBuffAlign);
 lib^.set_proc($CF42AD375BEA1761,@ps4_sceHmdReprojectionQueryGarlicBuffSize);
 lib^.set_proc($4E470035C18CD2CF,@ps4_sceHmdReprojectionQueryGarlicBuffAlign);
 lib^.set_proc($EDAB340A35D6D41F,@ps4_sceHmdReprojectionSetUserEventStart);
 lib^.set_proc($927C888659292E01,@ps4_sceHmdReprojectionSetUserEventEnd);
 lib^.set_proc($B610EDF6EA59969F,@ps4_sceHmdGetDeviceInformation);
 lib^.set_proc($BF33049300507223,@ps4_sceHmdReprojectionStop);
 lib^.set_proc($88634DA430E3730A,@ps4_sceHmdReprojectionUnsetDisplayBuffers);
 lib^.set_proc($66B579608A83D3D2,@ps4_sceHmdReprojectionFinalize);
 lib^.set_proc($99DC856DA263EBA3,@ps4_sceHmdReprojectionClearUserEventStart);
 lib^.set_proc($CFF44C20BA8FEAD1,@ps4_sceHmdTerminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceHmd.prx',@Load_libSceHmd);

end.

