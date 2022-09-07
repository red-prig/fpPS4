unit ps4_libSceNpMatching2;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

type
 pSceNpMatching2InitializeParameter=^SceNpMatching2InitializeParameter;
 SceNpMatching2InitializeParameter=packed record
  poolSize:QWORD;         // 0 = default
  cpuAffinityMask:QWORD;  // 0 = default SceKernelCpumask
  threadPriority:Integer; // 0 = default
  padding:Integer;
  threadStackSize:QWORD;  // 0 = default
  size:QWORD;             // size of this structure
  sslPoolSize:QWORD;      // 0 = default
 end;

 SceNpMatching2ContextCallback=procedure(
                                ctxId,event:Word;
                                eventCause:Byte;
                                errorCode:Integer;
                                arg:Pointer); SysV_ABI_CDecl;


function ps4_sceNpMatching2Initialize(param:pSceNpMatching2InitializeParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpMatching2RegisterContextCallback(cbFunc:SceNpMatching2ContextCallback;cbFuncArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpMatching2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpMatching2');
 lib^.set_proc($D74B777B9F893E75,@ps4_sceNpMatching2Initialize);
 lib^.set_proc($7D041F3FCEC8EE1B,@ps4_sceNpMatching2RegisterContextCallback);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpMatching2.prx',@Load_libSceNpMatching2);

end.

