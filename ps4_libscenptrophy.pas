unit ps4_libSceNpTrophy;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes, SysUtils;

implementation

function ps4_sceNpTrophyCreateContext(context:PInteger;
                                  userId:Integer;
                                  serviceLabel:DWORD;
                                  options:QWORD):Integer; SysV_ABI_CDecl;
begin
 context^:=543;
 Result:=0;
end;

function ps4_sceNpTrophyCreateHandle(handle:PInteger):Integer; SysV_ABI_CDecl;
begin
 handle^:=3333;
 Result:=0;
end;

function ps4_sceNpTrophyRegisterContext(context:Integer;
                                    handle:Integer;
                                    options:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpTrophyDestroyHandle(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpTrophy(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpTrophy');
 lib^.set_proc($5DB9236E86D99426,@ps4_sceNpTrophyCreateContext);
 lib^.set_proc($ABB53AB440107FB7,@ps4_sceNpTrophyCreateHandle);
 lib^.set_proc($4C9080C6DA3D4845,@ps4_sceNpTrophyRegisterContext);
 lib^.set_proc($18D705E2889D6346,@ps4_sceNpTrophyDestroyHandle);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpTrophy.prx',@Load_libSceNpTrophy);

end.

