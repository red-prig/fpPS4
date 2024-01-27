unit ps4_libSceJson2;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4__ZN3sce4Json12MemAllocatorC2Ev():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json14InitParameter2C1Ev():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5ValueC1Ev():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5Value3setEb():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5Value3setEl():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5Value3setEm():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5Value3setEd():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json6StringC1EPKc():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5Value3setERKNS0_6StringE():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json6StringD1Ev():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json5Value3setENS0_9ValueTypeE():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json14InitParameter217setFileBufferSizeEm():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json14InitParameter212setAllocatorEPNS0_12MemAllocatorEPv():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json11InitializerC1Ev():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json11Initializer10initializeEPKNS0_14InitParameter2E():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json11Initializer27setGlobalNullAccessCallbackEPFRKNS0_5ValueENS0_9ValueTypeEPS3_PvES7_():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4__ZN3sce4Json11Initializer10initializeEPKNS0_13InitParameterE():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceJson2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceJson2');
 lib^.set_proc($FE125171EF309F55,@ps4__ZN3sce4Json12MemAllocatorC2Ev);
 lib^.set_proc($5923AE81EE48B028,@ps4__ZN3sce4Json14InitParameter2C1Ev);
 lib^.set_proc($A81323AB2067DCE3,@ps4__ZN3sce4Json5ValueC1Ev);
 lib^.set_proc($E721EE8965E8DA08,@ps4__ZN3sce4Json5Value3setEb);
 lib^.set_proc($4315556213FF9AF8,@ps4__ZN3sce4Json5Value3setEl);
 lib^.set_proc($4887B56665BB7BBB,@ps4__ZN3sce4Json5Value3setEm);
 lib^.set_proc($0529960C8915E30E,@ps4__ZN3sce4Json5Value3setEd);
 lib^.set_proc($F4A5191632352310,@ps4__ZN3sce4Json6StringC1EPKc);
 lib^.set_proc($EA5DC1BF6832B0D7,@ps4__ZN3sce4Json5Value3setERKNS0_6StringE);
 lib^.set_proc($706D551361CC97A7,@ps4__ZN3sce4Json6StringD1Ev);
 lib^.set_proc($20A4229AF1BD5AAB,@ps4__ZN3sce4Json5Value3setENS0_9ValueTypeE);
 lib^.set_proc($12EF798E6AA7E51C,@ps4__ZN3sce4Json14InitParameter217setFileBufferSizeEm);
 lib^.set_proc($236402F0F6212566,@ps4__ZN3sce4Json14InitParameter212setAllocatorEPNS0_12MemAllocatorEPv);
 lib^.set_proc($70AE9B6077FF4391,@ps4__ZN3sce4Json11InitializerC1Ev);
 lib^.set_proc($2175BFCFCA6081F8,@ps4__ZN3sce4Json11Initializer10initializeEPKNS0_14InitParameter2E);
 lib^.set_proc($F9DAC3172012EAEE,@ps4__ZN3sce4Json11Initializer27setGlobalNullAccessCallbackEPFRKNS0_5ValueENS0_9ValueTypeEPS3_PvES7_);
 lib^.set_proc($0B1C32EF01EAE09D,@ps4__ZN3sce4Json11Initializer10initializeEPKNS0_13InitParameterE);
end;

initialization
 ps4_app.RegistredPreLoad('libSceJson2.prx',@Load_libSceJson2);

end.

