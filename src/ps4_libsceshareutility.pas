unit ps4_libSceShareUtility;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

const
  SCE_SHARE_UTILITY_HEAP_SIZE=128*1024;

implementation

function ps4_sceShareUtilityInitializeEx2(functionFlag:QWORD;
                                          heapSize:QWORD;
                                          threadPriority:Integer;
                                          affinityMask:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceShareUtilityAdvanceInitialize(functionFlag:QWORD;
                                              heapSize:QWORD;
                                              threadPriority:Integer;
                                              affinityMask:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceShareUtility(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceShareUtility');
 lib^.set_proc($8FB0E56A50731E1F,@ps4_sceShareUtilityInitializeEx2);
 lib^.set_proc($BB86C21A4AA1381E,@ps4_sceShareUtilityAdvanceInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceShareUtility.prx',@Load_libSceShareUtility);

end.

