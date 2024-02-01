unit ps4_libSceNpSignaling;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program;

const
 SCE_NP_SIGNALING_CONTEXT_MAX=8;

type
 pSceNpSignalingHandler=^SceNpSignalingHandler;
 SceNpSignalingHandler=packed record
  ctxId    :DWORD;
  subjectId:DWORD;
  event    :Integer;
  errorCode:Integer;
  arg      :Pointer;
 end;

implementation

function ps4_sceNpSignalingInitialize(poolSize:QWORD;
                                      threadPriority:Integer;
                                      cpuAffinityMask:Integer;
                                      threadStackSize:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpSignalingCreateContext(userId,handler:SceNpSignalingHandler;
                                         arg:Pointer;
                                         ctxId:PQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpSignaling(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpSignaling');
 lib^.set_proc($DCA3AE0B84666595,@ps4_sceNpSignalingInitialize);
 lib^.set_proc($E7262311D778B7C6,@ps4_sceNpSignalingCreateContext)
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpSignaling.prx',@Load_libSceNpSignaling);

end.

