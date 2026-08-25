unit ps4_libSceNpSignaling;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

uses
 ps4_libnet,
 ps4_libSceNpCommon;

const
 SCE_NP_SIGNALING_CONTEXT_MAX=8;

type
 SceNpSignalingHandler=procedure(
  ctxId    :DWORD;
  subjectId:DWORD;
  event    :Integer;
  errorCode:Integer;
  arg      :Pointer
 );

 pSceNpSignalingNetInfo=^SceNpSignalingNetInfo;
 SceNpSignalingNetInfo=packed record
  size       :QWORD;
  local_addr :SceNetInAddr;
  mapped_addr:SceNetInAddr;
  nat_status :Integer;
 end;

function ps4_sceNpSignalingInitialize(poolSize       :QWORD;
                                      threadPriority :Integer;
                                      cpuAffinityMask:Integer;
                                      threadStackSize:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceNpSignalingCreateContext(npId   :pSceNpId;
                                         handler:SceNpSignalingHandler;
                                         arg    :Pointer;
                                         ctxId  :PDWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceNpSignalingCreateContextA(npId   :pSceNpId;
                                          handler:SceNpSignalingHandler;
                                          arg    :Pointer;
                                          ctxId  :PDWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceNpSignalingGetLocalNetInfo(ctxId:DWORD;
                                           info :pSceNpSignalingNetInfo):Integer;
begin
 Result:=0;
end; 

function ps4_sceNpSignalingSetContextOption(ctxId  :DWORD;
                                            optname:Integer;
                                            optval :Integer):Integer;
begin
 Result:=0;
end; 

{$WARN 4110 off}
function Load_libSceNpSignaling(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpSignaling');

 lib:=Result^.add_lib('libSceNpSignaling');
 lib.set_proc($DCA3AE0B84666595,@ps4_sceNpSignalingInitialize);
 lib.set_proc($E7262311D778B7C6,@ps4_sceNpSignalingCreateContext);
 lib.set_proc($7432CD15D63C770B,@ps4_sceNpSignalingCreateContextA);
 lib.set_proc($53C01032538505CF,@ps4_sceNpSignalingGetLocalNetInfo);
 lib.set_proc($207443BD9A1D3D86,@ps4_sceNpSignalingSetContextOption);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpSignaling.prx',@Load_libSceNpSignaling);

end.

