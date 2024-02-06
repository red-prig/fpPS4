unit ps4_libSceNpTus;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libscenpcommon;

const
 SCE_NP_TUS_DATA_INFO_MAX_SIZE=384;

type
 pSceNpTusDataInfo=^SceNpTusDataInfo;
 SceNpTusDataInfo=packed record
  infoSize:QWORD;
  data    :array[0..SCE_NP_TUS_DATA_INFO_MAX_SIZE-1] of Byte;
 end;

 pSceNpTusDataStatusA=^SceNpTusDataStatusA;
 SceNpTusDataStatusA=packed record
  ownerId                   :SceNpOnlineId;
  reserved1                 :array[0..15] of Byte;
  hasData                   :Integer;
  lastChangedDate           :PQWORD; //SceRtcTick
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

function ps4_sceNpTusCreateRequest(titleCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpTusDeleteRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpTssCreateNpTitleCtx(serviceLabel:DWord;npId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
end;

function ps4_sceNpTssCreateNpTitleCtxA(serviceLabel:DWord;selfId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
end;

function ps4_sceNpTusCreateNpTitleCtx(serviceLabel:DWord;npId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
end;

function ps4_sceNpTusCreateNpTitleCtxA(serviceLabel:DWord;selfId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
end;

function ps4_sceNpTusGetDataA(reqId:Integer;
                              targetAccountId:SceNpAccountId;
                              slotId:DWORD;
                              dataStatus:SceNpTusDataStatusA;
                              dataStatusSize:QWORD;
                              data:Pointer;
                              recvSize:QWORD;
                              option:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
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
                              option:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpTus(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpTus');
 lib^.set_proc($DDB876681BEF9AF3,@ps4_sceNpTusCreateRequest);
 lib^.set_proc($09C207E347584BCF,@ps4_sceNpTusDeleteRequest);
 lib^.set_proc($B1155BD827F41878,@ps4_sceNpTssCreateNpTitleCtx);
 lib^.set_proc($941B6B93EEE5935E,@ps4_sceNpTssCreateNpTitleCtxA);
 lib^.set_proc($04890C9947CD2963,@ps4_sceNpTusCreateNpTitleCtx);
 lib^.set_proc($D67FDD1AE9018276,@ps4_sceNpTusCreateNpTitleCtxA);
 lib^.set_proc($C96107505918D6A2,@ps4_sceNpTusGetDataA);
 lib^.set_proc($573C4DDED3A8BA3F,@ps4_sceNpTusSetDataA);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpTus.prx',@Load_libSceNpTus);

end.

