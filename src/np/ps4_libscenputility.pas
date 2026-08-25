unit ps4_libSceNpUtility;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 ps4_libSceNpCommon;

const
 SCE_NP_LOOKUP_MAX_CTX_NUM=32;

type
 pSceNpBandwidthTestInitParam=^SceNpBandwidthTestInitParam;
 SceNpBandwidthTestInitParam=packed record
  size           :QWORD;
  threadPriority :Integer;
  padding        :Integer;
  cpuAffinityMask:QWORD;
  reserved       :array[0..7] of DWORD;
 end;

implementation

function ps4_sceNpLookupCreateTitleCtx(selfNpId:PSceNpId):Integer;
begin
 Result:=1;
end;

function ps4_sceNpLookupCreateTitleCtxA(selfNpId:Integer):Integer;
begin
 Result:=1;
end;

function ps4_sceNpBandwidthTestInitStart(const param:pSceNpBandwidthTestInitParam):Integer;
begin
 Result:=0;
end;

function ps4_sceNpBandwidthTestGetStatus(contextId:Integer;status:PInteger):Integer;
begin
 Result:=0;
end;

function ps4_sceNpWordFilterCreateTitleCtx(selfNpId:PSceNpId):Integer;
begin
 Result:=2;
end;

{$WARN 4110 off}
function Load_libSceNpUtility(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpUtility');

 lib:=Result^.add_lib('libSceNpUtility');
 lib.set_proc($F39DF743E2D4EC44,@ps4_sceNpLookupCreateTitleCtx);
 lib.set_proc($BD3F7186A3CEEBED,@ps4_sceNpLookupCreateTitleCtxA);
 lib.set_proc($8E4B70C37C895E77,@ps4_sceNpBandwidthTestInitStart);
 lib.set_proc($05821918A9BA6CEE,@ps4_sceNpBandwidthTestGetStatus);
 ///lib.set_proc($E7262311D778B7C6,@ps4_sceNpSignalingCreateContext);
 lib.set_proc($AFD0602343DF2598,@ps4_sceNpWordFilterCreateTitleCtx);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpUtility.prx',@Load_libSceNpUtility);

end.


