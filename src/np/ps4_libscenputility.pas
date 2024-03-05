unit ps4_libSceNpUtility;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libSceNpCommon;

const
 SCE_NP_LOOKUP_MAX_CTX_NUM=32;

type
 pSceNpBandwidthTestInitParam=^SceNpBandwidthTestInitParam;
 SceNpBandwidthTestInitParam=packed record
  size           :QWORD;
  threadPriority :Integer;
  padding        :array[0..3] of char;
  cpuAffinityMask:QWORD;
  reserved       :array[0..7] of DWORD;
 end;

implementation

function ps4_sceNpLookupCreateTitleCtx(selfNpId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 Result:=1;
end;

function ps4_sceNpLookupCreateTitleCtxA(selfNpId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=1;
end;

function ps4_sceNpBandwidthTestInitStart(const param:pSceNpBandwidthTestInitParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpUtility(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpUtility');
 lib^.set_proc($F39DF743E2D4EC44,@ps4_sceNpLookupCreateTitleCtx);
 lib^.set_proc($BD3F7186A3CEEBED,@ps4_sceNpLookupCreateTitleCtxA);
 lib^.set_proc($8E4B70C37C895E77,@ps4_sceNpBandwidthTestInitStart);
 ///lib^.set_proc($E7262311D778B7C6,@ps4_sceNpSignalingCreateContext);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpUtility.prx',@Load_libSceNpUtility);

end.


