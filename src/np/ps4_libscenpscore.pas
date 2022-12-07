unit ps4_libSceNpScore;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils,
  ps4_libSceNpManager;

implementation

function ps4_sceNpScoreCreateNpTitleCtx(npServiceLabel:Integer;selfNpId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 Result:=1;
end;

function ps4_sceNpScoreCreateNpTitleCtxA(npServiceLabel:Integer;selfId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=2;
end;

function ps4_sceNpScoreCreateRequest(titleCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 writeln('ScoreCreateRequest:',titleCtxId);
 Result:=894;
end;

function ps4_sceNpScoreDeleteRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
begin
 writeln('sceNpScoreDeleteRequest:',reqId);
 Result:=0;
end;

type
 pSceNpScoreRankData=^SceNpScoreRankData;
 SceNpScoreRankData=packed record
  npId:SceNpId;
  reserved:array[0..48] of Byte;
  pad0:array[0..2] of Byte;
  pcId:Integer;
  serialRank:DWORD;
  rank:DWORD;
  highestRank:DWORD;
  scoreValue:Int64;
  hasGameData:Integer;
  pad1:array[0..3] of Byte;
  recordDate:QWORD;
 end;

 PSceNpScoreRankDataA=^SceNpScoreRankDataA;
 SceNpScoreRankDataA=packed record
  onlineId:SceNpOnlineId;
  reserved0:array[0..15] of Byte;
  reserved:array[0..48] of Byte;
  pad0:array[0..2] of Byte;
  pcId:Integer;
  serialRank:DWORD;
  rank:DWORD;
  highestRank:DWORD;
  hasGameData:Integer;
  pad1:array[0..3] of Byte;
  scoreValue:Int64;
  recordDate:QWORD;
  accountId:QWORD;
  pad2:array[0..7] of Byte;
 end;

const
 SCE_NP_SCORE_COMMENT_MAXLEN=63;

type
 PSceNpScoreComment=^SceNpScoreComment;
 SceNpScoreComment=packed record
  utf8Comment:array[0..SCE_NP_SCORE_COMMENT_MAXLEN] of Char;
 end;

const
 SCE_NP_SCORE_GAMEINFO_MAXSIZE=189;

type
 PSceNpScoreGameInfo=^SceNpScoreGameInfo;
 SceNpScoreGameInfo=packed record
  infoSize:size_t;
  data:array[0..SCE_NP_SCORE_GAMEINFO_MAXSIZE-1] of Byte;
  pad2:array[0..2] of Byte;
 end;

 PSceNpScoreGetFriendRankingOptParam=^SceNpScoreGetFriendRankingOptParam;
 SceNpScoreGetFriendRankingOptParam=packed record
  size:size_t;
  startSerialRank:PInteger;
  hits:PInteger;
 end;

function ps4_sceNpScoreGetFriendsRanking(
             reqId:Integer;
             boardId:DWORD;
             includeSelf:Integer;
             rankArray:PSceNpScoreRankData;
             rankArraySize:size_t;
             commentArray:PSceNpScoreComment;
             commentArraySize:size_t;
             infoArray:PSceNpScoreGameInfo;
             infoArraySize:size_t;
             arrayNum:size_t;
             lastSortDate:PQWORD;
             totalRecord:PDWORD;
             option:PSceNpScoreGetFriendRankingOptParam):Integer; SysV_ABI_CDecl;
begin
 if (lastSortDate<>nil) then
 begin
  lastSortDate^:=0;
 end;
 if (totalRecord<>nil) then
 begin
  totalRecord^:=0;
 end;
 Result:=0;
end;

function ps4_sceNpScoreGetFriendsRankingA(
             reqId:Integer;
             boardId:DWORD;
             includeSelf:Integer;
             rankArray:PSceNpScoreRankDataA;
             rankArraySize:size_t;
             commentArray:PSceNpScoreComment;
             commentArraySize:size_t;
             infoArray:PSceNpScoreGameInfo;
             infoArraySize:size_t;
             arrayNum:size_t;
             lastSortDate:PQWORD;
             totalRecord:PDWORD;
             option:PSceNpScoreGetFriendRankingOptParam):Integer; SysV_ABI_CDecl;
begin
 if (lastSortDate<>nil) then
 begin
  lastSortDate^:=0;
 end;
 if (totalRecord<>nil) then
 begin
  totalRecord^:=0;
 end;
 Result:=0;
end;

type
 PSceNpScoreAccountIdPcId=^SceNpScoreAccountIdPcId;
 SceNpScoreAccountIdPcId=packed record
  accountId:QWORD;
  pcId:Integer;
  pad:array[0..3] of Byte;
 end;

function ps4_sceNpScoreGetRankingByAccountIdPcId(
             reqId:Integer;
             boardId:DWORD;
             idArray:PSceNpScoreAccountIdPcId;
             idArraySize:size_t;
             rankArray:PSceNpScoreRankDataA;
             rankArraySize:size_t;
             commentArray:PSceNpScoreComment;
             commentArraySize:size_t;
             infoArray:PSceNpScoreGameInfo;
             infoArraySize:size_t;
             arrayNum:size_t;
             lastSortDate:PQWORD;
             totalRecord:PDWORD;
             option:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (lastSortDate<>nil) then
 begin
  lastSortDate^:=0;
 end;
 if (totalRecord<>nil) then
 begin
  totalRecord^:=0;
 end;
 Result:=0;
end;

function ps4_sceNpScoreGetRankingByRange(
             reqId:Integer;
             boardId:DWORD;
             startSerialRank:DWORD;
             rankArray:pSceNpScoreRankData;
             rankArraySize:QWORD;
             commentArray:pSceNpScoreComment;
             commentArraySize:QWORD;
             infoArray:pSceNpScoreGameInfo;
             infoArraySize:QWORD;
             arrayNum:QWORD;
             lastSortDate:PQWORD;
             totalRecord:PDWORD;
             option:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (lastSortDate<>nil) then
 begin
  lastSortDate^:=0;
 end;
 if (totalRecord<>nil) then
 begin
  totalRecord^:=0;
 end;
 Result:=0;
end;

function ps4_sceNpScoreRecordScore(
             reqId:Integer;
             boardId:DWORD;      //SceNpScoreBoardId
             score:Int64;        //SceNpScoreValue
             scoreComment:pSceNpScoreComment;
             gameInfo:pSceNpScoreGameInfo;
             tmpRank:PDWORD;     //SceNpScoreRankNumber
             compareDate:PQWORD; //SceRtcTick
             option:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (tmpRank<>nil) then
 begin
  tmpRank^:=0;
 end;
 Result:=0;
end;

function ps4_sceNpScoreRecordScoreAsync(
             reqId:Integer;
             boardId:DWORD;      //SceNpScoreBoardId
             score:Int64;        //SceNpScoreValue
             scoreComment:pSceNpScoreComment;
             gameInfo:pSceNpScoreGameInfo;
             tmpRank:PDWORD;     //SceNpScoreRankNumber
             compareDate:PQWORD; //SceRtcTick
             option:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (tmpRank<>nil) then
 begin
  tmpRank^:=0;
 end;
 Result:=0;
end;

function ps4_sceNpScorePollAsync(
             reqId:Integer;
             r_out:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (r_out<>nil) then
 begin
  r_out^:=0;
 end;
 Result:=0;
end;

function Load_libSceNpScoreRanking(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpScore');
 lib^.set_proc($2A7340D53120B412,@ps4_sceNpScoreCreateNpTitleCtx);
 lib^.set_proc($1969D640D5D91F93,@ps4_sceNpScoreCreateNpTitleCtxA);
 lib^.set_proc($816F2ACA362B51B9,@ps4_sceNpScoreCreateRequest);
 lib^.set_proc($74AF3F4A061FEABE,@ps4_sceNpScoreDeleteRequest);
 lib^.set_proc($F24B88CD4C3ABAD4,@ps4_sceNpScoreGetFriendsRanking);
 lib^.set_proc($80C6CE9FEFFA7970,@ps4_sceNpScoreGetFriendsRankingA);
 lib^.set_proc($F66644828884ABA6,@ps4_sceNpScoreGetRankingByAccountIdPcId);
 lib^.set_proc($2811F10E3CA4FE30,@ps4_sceNpScoreGetRankingByRange);
 lib^.set_proc($CD3D1706D82D3922,@ps4_sceNpScoreRecordScore);
 lib^.set_proc($00D26CB0FCF7998D,@ps4_sceNpScoreRecordScoreAsync);
 lib^.set_proc($9B50DF351B2D9124,@ps4_sceNpScorePollAsync);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpScoreRanking.prx',@Load_libSceNpScoreRanking);

end.

