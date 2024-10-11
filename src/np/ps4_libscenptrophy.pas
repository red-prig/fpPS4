unit ps4_libSceNpTrophy;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  subr_dynlib,
  ps4_libSceNpCommon,
  np_error;

const
 SCE_NP_TROPHY_NUM_MAX                  =(128);
 SCE_NP_TROPHY_SCREENSHOT_TARGET_NUM_MAX=(4);

 SCE_NP_TROPHY_GAME_TITLE_MAX_SIZE =(128);
 SCE_NP_TROPHY_GAME_DESCR_MAX_SIZE =(1024);
 SCE_NP_TROPHY_GROUP_TITLE_MAX_SIZE=(128);
 SCE_NP_TROPHY_GROUP_DESCR_MAX_SIZE=(1024);
 SCE_NP_TROPHY_NAME_MAX_SIZE       =(128);
 SCE_NP_TROPHY_DESCR_MAX_SIZE      =(1024);

// grade
 SCE_NP_TROPHY_GRADE_UNKNOWN =(0);
 SCE_NP_TROPHY_GRADE_PLATINUM=(1);
 SCE_NP_TROPHY_GRADE_GOLD    =(2);
 SCE_NP_TROPHY_GRADE_SILVER  =(3);
 SCE_NP_TROPHY_GRADE_BRONZE  =(4);


type
 SceNpTrophyHandle  =Integer;
 SceNpTrophyContext =Integer;
 SceNpTrophyId      =Integer;
 SceNpTrophyGroupId =Integer;
 SceNpTrophyGrade   =Integer;
 SceNpTrophyFlagMask=DWORD;

const
 SCE_NP_TROPHY_INVALID_HANDLE    =(-1);
 SCE_NP_TROPHY_INVALID_CONTEXT   =(-1);
 SCE_NP_TROPHY_INVALID_TROPHY_ID =(-1);
 SCE_NP_TROPHY_INVALID_GROUP_ID  =(-2);
 SCE_NP_TROPHY_BASE_GAME_GROUP_ID=(-1);

// trophy flag array
 SCE_NP_TROPHY_FLAG_SETSIZE   =(128);
 SCE_NP_TROPHY_FLAG_BITS      =(sizeof(SceNpTrophyFlagMask) * 8);
 SCE_NP_TROPHY_FLAG_BITS_ALL  =(High(SceNpTrophyFlagMask));
 SCE_NP_TROPHY_FLAG_BITS_SHIFT=(5);
 SCE_NP_TROPHY_FLAG_BITS_MASK =(SCE_NP_TROPHY_FLAG_BITS - 1);
 SCE_NP_TROPHY_FLAG_BITS_MAX  =(SCE_NP_TROPHY_FLAG_SETSIZE - 1);

type
 pSceNpTrophyFlagArray=^SceNpTrophyFlagArray;
 SceNpTrophyFlagArray=packed record
  flagBits:array[0..(SCE_NP_TROPHY_FLAG_SETSIZE shr SCE_NP_TROPHY_FLAG_BITS_SHIFT)-1] of SceNpTrophyFlagMask;
 end;

 pSceNpTrophyGameDetails=^SceNpTrophyGameDetails;
 SceNpTrophyGameDetails=packed record
  size:QWORD;
  numGroups  :DWORD;
  numTrophies:DWORD;
  numPlatinum:DWORD;
  numGold    :DWORD;
  numSilver  :DWORD;
  numBronze  :DWORD;
  title:array[0..SCE_NP_TROPHY_GAME_TITLE_MAX_SIZE-1] of AnsiChar;
  description:array[0..SCE_NP_TROPHY_GAME_DESCR_MAX_SIZE-1] of AnsiChar;
 end;

 pSceNpTrophyGameData=^SceNpTrophyGameData;
 SceNpTrophyGameData=packed record
  size:QWORD;
  unlockedTrophies  :DWORD;
  unlockedPlatinum  :DWORD;
  unlockedGold      :DWORD;
  unlockedSilver    :DWORD;
  unlockedBronze    :DWORD;
  progressPercentage:DWORD;
 end;

 pSceNpTrophyDetails=^SceNpTrophyDetails;
 SceNpTrophyDetails=packed record
  size:qword;
  trophyId:SceNpTrophyId;
  trophyGrade:SceNpTrophyGrade;
  groupId:SceNpTrophyGroupId;
  hidden:boolean;
  reserved:array[0..2] of Byte;
  name:array[0..SCE_NP_TROPHY_NAME_MAX_SIZE-1] of AnsiChar;
  description:array[0..SCE_NP_TROPHY_DESCR_MAX_SIZE-1] of AnsiChar;
 end;

 pSceNpTrophyData=^SceNpTrophyData;
 SceNpTrophyData=packed record
  size:qword;
  trophyId:SceNpTrophyId;
  unlocked:boolean;
  reserved:array[0..2] of Byte;
  timestamp:QWORD;
 end;

 pSceNpTrophyGroupDetails=^SceNpTrophyGroupDetails;
 SceNpTrophyGroupDetails=packed record
  size:qword;
  groupId:SceNpTrophyGroupId;
  numTrophies:DWORD;
  numPlatinum:DWORD;
  numGold    :DWORD;
  numSilver  :DWORD;
  numBronze  :DWORD;
  title:array[0..SCE_NP_TROPHY_GROUP_TITLE_MAX_SIZE-1] of AnsiChar;
  description:array[0..SCE_NP_TROPHY_GROUP_DESCR_MAX_SIZE-1] of AnsiChar;
 end;

 pSceNpTrophyGroupData=^SceNpTrophyGroupData;
 SceNpTrophyGroupData=packed record
  size:qword;
  groupId:SceNpTrophyGroupId;
  unlockedTrophies  :DWORD;
  unlockedPlatinum  :DWORD;
  unlockedGold      :DWORD;
  unlockedSilver    :DWORD;
  unlockedBronze    :DWORD;
  progressPercentage:DWORD;
  reserved:DWORD;
 end;

implementation

function ps4_sceNpTrophyCreateContext(context     :PInteger;
                                      userId      :Integer;
                                      serviceLabel:SceNpServiceLabel;
                                      options     :QWORD):Integer;
begin
 Writeln('sceNpTrophyCreateContext');
 if (context=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);
 context^:=543;
 Result:=0;
end;

function ps4_sceNpTrophyCreateHandle(handle:PInteger):Integer;
begin
 Writeln('sceNpTrophyCreateHandle');
 if (handle=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);
 handle^:=3333;
 Result:=0;
end;

function ps4_sceNpTrophyDestroyContext(context:Integer):Integer;
begin
 Writeln('sceNpTrophyDestroyContext');
 Result:=0;
end;

function ps4_sceNpTrophyDestroyHandle(handle:Integer):Integer;
begin
 Writeln('sceNpTrophyDestroyHandle:',handle);
 Result:=0;
end;

function ps4_sceNpTrophyAbortHandle(handle:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpTrophyRegisterContext(context:Integer;
                                        handle :Integer;
                                        options:QWORD):Integer;
begin
 Writeln('sceNpTrophyRegisterContext:',handle);
 Result:=0;
end;

function ps4_sceNpTrophyGetTrophyUnlockState(context:Integer;
                                             handle :Integer;
                                             flags  :pSceNpTrophyFlagArray;
                                             count  :PDWORD):Integer;
begin
 Writeln('sceNpTrophyGetTrophyUnlockState:',handle);
 if (flags=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);
 if (count=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);
 Result:=0;
 flags^:=Default(SceNpTrophyFlagArray);
 count^:=2;  //must answer correctly for each game
end;

function ps4_sceNpTrophyUnlockTrophy(context   :Integer;
                                     handle    :Integer;
                                     trophyId  :Integer;
                                     platinumId:PInteger):Integer;
begin
 Writeln('sceNpTrophyUnlockTrophy:',trophyId);
 if (platinumId=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);
 platinumId^:=SCE_NP_TROPHY_INVALID_TROPHY_ID;
 Result:=0;
end;

function ps4_sceNpTrophyGetGameInfo(context:Integer;
                                    handle :Integer;
                                    details:pSceNpTrophyGameDetails;
                                    data   :pSceNpTrophyGameData):Integer;
begin
 Writeln('sceNpTrophyGetGameInfo:',handle);

 if (details<>nil) then
 begin
  details^.numGroups  :=0;
  details^.numTrophies:=1;
  details^.numPlatinum:=0;
  details^.numGold    :=0;
  details^.numSilver  :=0;
  details^.numBronze  :=1;
  details^.title      :='tname';
  details^.description:='tdesc';
 end;

 if (data<>nil) then
 begin
  data^.unlockedTrophies  :=0;
  data^.unlockedPlatinum  :=0;
  data^.unlockedGold      :=0;
  data^.unlockedSilver    :=0;
  data^.unlockedBronze    :=0;
  data^.progressPercentage:=0;
 end;

 Result:=0;
end;

function ps4_sceNpTrophyGetTrophyInfo(context :Integer;
                                      handle  :Integer;
                                      trophyId:Integer;
                                      details :pSceNpTrophyDetails;
                                      data    :pSceNpTrophyData):Integer;
begin
 Result:=0;
 if (details<>nil) then
 begin
  details^.trophyId   :=trophyId;
  details^.trophyGrade:=SCE_NP_TROPHY_GRADE_BRONZE;
  details^.groupId    :=-1;
  details^.hidden     :=false;
  details^.name       :='tname';
  details^.description:='tdesc';
 end;
 if (data<>nil) then
 begin
  data^.trophyId :=trophyId;
  data^.unlocked :=false;
  data^.timestamp:=0;
 end;
end;

function ps4_sceNpTrophyGetGroupInfo(context:Integer;
                                     handle :Integer;
                                     groupId:Integer;
                                     details:pSceNpTrophyGroupDetails;
                                     data   :pSceNpTrophyGroupData):Integer;
begin
 Result:=0;
 if (details<>nil) then
 begin
  details^.groupId    :=groupId;
  details^.numTrophies:=0;
  details^.numPlatinum:=0;
  details^.numGold    :=0;
  details^.numSilver  :=0;
  details^.numBronze  :=0;
  details^.title      :='gname';
  details^.description:='gdesc';
 end;
 if (data<>nil) then
 begin
  data^.groupId           :=groupId;
  data^.unlockedTrophies  :=0;
  data^.unlockedPlatinum  :=0;
  data^.unlockedGold      :=0;
  data^.unlockedSilver    :=0;
  data^.unlockedBronze    :=0;
  data^.progressPercentage:=0;
 end;
end;

//result is png image
function ps4_sceNpTrophyGetGameIcon(context:Integer;
                                    handle :Integer;
                                    buffer :Pointer;
                                    size   :PQWORD):Integer;
begin
 Writeln('sceNpTrophyGetGameIcon:',context,' ',handle);

 if (size=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);

 size^:=8;

 if (buffer<>nil) then
 begin
  pqword(buffer)^:=0;
 end;

 Result:=0;
end;

//result is png image
function ps4_sceNpTrophyGetTrophyIcon(context :Integer;
                                      handle  :Integer;
                                      trophyId:Integer;
                                      buffer  :Pointer;
                                      size    :PQWORD):Integer;
begin
 Writeln('sceNpTrophyGetTrophyIcon:',context,' ',handle,' ',trophyId);

 if (size=nil) then Exit(SCE_NP_TROPHY_ERROR_INVALID_ARGUMENT);

 size^:=8;

 if (buffer<>nil) then
 begin
  pqword(buffer)^:=0;
 end;

 Result:=0;
end;

function Load_libSceNpTrophy(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpTrophy');

 lib:=Result^.add_lib('libSceNpTrophy');
 lib.set_proc($5DB9236E86D99426,@ps4_sceNpTrophyCreateContext);
 lib.set_proc($ABB53AB440107FB7,@ps4_sceNpTrophyCreateHandle);
 lib.set_proc($1355ABC1DD3B2EBF,@ps4_sceNpTrophyDestroyContext);
 lib.set_proc($18D705E2889D6346,@ps4_sceNpTrophyDestroyHandle);
 lib.set_proc($6939C7B3B5BFF549,@ps4_sceNpTrophyAbortHandle);
 lib.set_proc($4C9080C6DA3D4845,@ps4_sceNpTrophyRegisterContext);
 lib.set_proc($2C7B9298EDD22DDF,@ps4_sceNpTrophyGetTrophyUnlockState);
 lib.set_proc($DBCC6645415AA3AF,@ps4_sceNpTrophyUnlockTrophy);
 lib.set_proc($6183F77F65B4F688,@ps4_sceNpTrophyGetGameInfo);
 lib.set_proc($AAA515183810066D,@ps4_sceNpTrophyGetTrophyInfo);
 lib.set_proc($C1353019FB292A27,@ps4_sceNpTrophyGetGroupInfo);
 lib.set_proc($1CBC33D5F448C9C0,@ps4_sceNpTrophyGetGameIcon);
 lib.set_proc($7812FE97A1C6F719,@ps4_sceNpTrophyGetTrophyIcon);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceNpTrophy.prx',@Load_libSceNpTrophy);

end.

