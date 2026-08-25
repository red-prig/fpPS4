unit ps4_libSceNpTus;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 np_error,
 ps4_libscenpcommon;

type
 pSceNpTssDataStatus=^SceNpTssDataStatus;
 SceNpTssDataStatus=packed record
  lastModified  :QWORD; //SceRtcTick
  statusCodeType:Integer;
  _align        :Integer;
  contentLength :QWORD;
 end;

///

const
 SCE_NP_TUS_DATA_INFO_MAX_SIZE=384;

type
 pSceNpTusDataInfo=^SceNpTusDataInfo;
 SceNpTusDataInfo=packed record
  infoSize:QWORD;
  data    :array[0..SCE_NP_TUS_DATA_INFO_MAX_SIZE-1] of Byte;
 end;

 pSceNpTusDataStatus=^SceNpTusDataStatus;
 SceNpTusDataStatus=packed record
  ownerId            :SceNpId;
  hasData            :Integer;
  lastChangedDate    :QWORD; //SceRtcTick
  lastChangedAuthorId:SceNpId;
  pad                :array[0..3] of Byte;
  data               :Pointer;
  dataSize           :QWORD;
  info               :SceNpTusDataInfo;
 end;

 pSceNpTusDataStatusA=^SceNpTusDataStatusA;
 SceNpTusDataStatusA=packed record
  ownerId                   :SceNpOnlineId;
  reserved1                 :array[0..15] of Byte;
  hasData                   :Integer;
  lastChangedDate           :QWORD; //SceRtcTick
  lastChangedAuthorId       :SceNpOnlineId;
  reserved2                 :array[0..15] of Byte;
  pad                       :array[0..3] of Byte;
  data                      :Pointer;
  dataSize                  :QWORD;
  info                      :SceNpTusDataInfo;
  ownerAccountId            :SceNpAccountId;
  lastChangedAuthorAccountId:SceNpAccountId;
  reserved                  :array[0..15] of Byte;
 end;  

implementation

function ps4_sceNpTssCreateNpTitleCtx(serviceLabel:DWord;npId:PSceNpId):Integer;
begin
 Result:=120;
end;

function ps4_sceNpTssCreateNpTitleCtxA(serviceLabel:DWord;selfId:Integer):Integer;
begin
 Result:=121;
end;

function ps4_sceNpTssGetData(reqId:Integer;
                             slotId:DWORD;
                             dataStatus:pSceNpTssDataStatus;
                             dataStatusSize:QWORD;
                             data:Pointer;
                             recvSize:QWORD;
                             option:Pointer):Integer;
begin
 if (dataStatus<>nil) then
 begin
  dataStatus^:=Default(SceNpTssDataStatus);
 end;
 Result:=0;
end;

function ps4_sceNpTssGetDataAsync(reqId:Integer;
                                  slotId:DWORD;
                                  dataStatus:pSceNpTssDataStatus;
                                  dataStatusSize:QWORD;
                                  data:Pointer;
                                  recvSize:QWORD;
                                  option:Pointer):Integer;
begin
 if (dataStatus<>nil) then
 begin
  dataStatus^:=Default(SceNpTssDataStatus);
 end;
 Result:=0;
end;

//

function ps4_sceNpTusCreateRequest(titleCtxId:Integer):Integer;
begin
 Result:=122;
end;

function ps4_sceNpTusDeleteRequest(reqId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpTusCreateNpTitleCtx(serviceLabel:DWord;npId:PSceNpId):Integer;
begin
 Result:=123;
end;

function ps4_sceNpTusCreateNpTitleCtxA(serviceLabel:DWord;selfId:Integer):Integer;
begin
 Result:=124;
end;

function ps4_sceNpTusGetData(reqId:Integer;
                             targetNpId:pSceNpId;
                             slotId:DWORD;
                             dataStatus:pSceNpTusDataStatus;
                             dataStatusSize:QWORD;
                             data:Pointer;
                             recvSize:QWORD;
                             option:Pointer):Integer;
begin
 Result:=SCE_NP_COMMUNITY_SERVER_ERROR_FORBIDDEN;
end;

function ps4_sceNpTusGetDataA(reqId:Integer;
                              targetAccountId:SceNpAccountId;
                              slotId:DWORD;
                              dataStatus:pSceNpTusDataStatusA;
                              dataStatusSize:QWORD;
                              data:Pointer;
                              recvSize:QWORD;
                              option:Pointer):Integer;
begin
 Result:=SCE_NP_COMMUNITY_SERVER_ERROR_FORBIDDEN;
end;

function ps4_sceNpTusSetDataA(reqId:Integer;
                              targetAccountId:SceNpAccountId;
                              slotId:DWORD;
                              totalSize:QWORD;
                              sendSize:QWORD;
                              const data:Pointer;
                              const info:pSceNpTusDataInfo;
                              infoStructSize:QWORD;
                              const isLastChangedAuthor:PQWORD;
                              const isLastChangedDate:PQWORD; //SceRtcTick
                              option:Pointer):Integer;
begin
 Result:=SCE_NP_COMMUNITY_SERVER_ERROR_FORBIDDEN;
end;

function ps4_sceNpTusSetThreadParam(threadPriority:Integer;
                                    cpuAffinityMask:QWORD //SceKernelCpumask
                                   ):Integer;
begin
 Result:=0;
end;

function ps4_sceNpTusWaitAsync(reqId:Integer;pResult:PInteger):Integer;
begin
 if (pResult<>nil) then
 begin
  pResult^:=SCE_NP_COMMUNITY_SERVER_ERROR_FORBIDDEN;
 end;
 Result:=0;
end;

function ps4_sceNpTusPollAsync(reqId:Integer;pResult:PInteger):Integer;
begin
 if (pResult<>nil) then
 begin
  pResult^:=SCE_NP_COMMUNITY_SERVER_ERROR_FORBIDDEN;
 end;
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceNpTus(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpTus');

 lib:=Result^.add_lib('libSceNpTus');
 //
 lib.set_proc($B1155BD827F41878,@ps4_sceNpTssCreateNpTitleCtx);
 lib.set_proc($941B6B93EEE5935E,@ps4_sceNpTssCreateNpTitleCtxA);
 lib.set_proc($FD2511F94A0B4BA7,@ps4_sceNpTssGetData);
 lib.set_proc($0D2DB2BB74A38F5A,@ps4_sceNpTssGetDataAsync);
 //
 lib.set_proc($DDB876681BEF9AF3,@ps4_sceNpTusCreateRequest);
 lib.set_proc($09C207E347584BCF,@ps4_sceNpTusDeleteRequest);
 lib.set_proc($04890C9947CD2963,@ps4_sceNpTusCreateNpTitleCtx);
 lib.set_proc($D67FDD1AE9018276,@ps4_sceNpTusCreateNpTitleCtxA);
 lib.set_proc($5CECECCCEE0E3565,@ps4_sceNpTusGetData);
 lib.set_proc($C96107505918D6A2,@ps4_sceNpTusGetDataA);
 lib.set_proc($573C4DDED3A8BA3F,@ps4_sceNpTusSetDataA);
 lib.set_proc($E86283751085C7C7,@ps4_sceNpTusSetThreadParam);
 lib.set_proc($8583C9156CC53E30,@ps4_sceNpTusWaitAsync);
 lib.set_proc($B7B6FA766A503622,@ps4_sceNpTusPollAsync);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpTus.prx',@Load_libSceNpTus);

end.

