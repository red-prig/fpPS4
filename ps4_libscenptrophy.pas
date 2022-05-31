unit ps4_libSceNpTrophy;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

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

implementation

function ps4_sceNpTrophyCreateContext(context:PInteger;
                                      userId:Integer;
                                      serviceLabel:DWORD;
                                      options:QWORD):Integer; SysV_ABI_CDecl;
begin
 context^:=543;
 Result:=0;
end;

function ps4_sceNpTrophyCreateHandle(handle:PInteger):Integer; SysV_ABI_CDecl;
begin
 handle^:=3333;
 Result:=0;
end;

function ps4_sceNpTrophyDestroyHandle(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpTrophyRegisterContext(context:Integer;
                                        handle:Integer;
                                        options:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpTrophyGetTrophyUnlockState(context:Integer;
                                             handle:Integer;
                                             flags:pSceNpTrophyFlagArray;
                                             count:PDWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 flags^:=Default(SceNpTrophyFlagArray);
 count^:=0;
end;

function ps4_sceNpTrophyUnlockTrophy(context:Integer;
                                     handle:Integer;
                                     trophyId:Integer;
                                     platinumId:PInteger):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpTrophyUnlockTrophy:',trophyId);
 if (platinumId<>nil) then
 begin
  platinumId^:=SCE_NP_TROPHY_INVALID_TROPHY_ID;
 end;
 Result:=0;
end;

function Load_libSceNpTrophy(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpTrophy');
 lib^.set_proc($5DB9236E86D99426,@ps4_sceNpTrophyCreateContext);
 lib^.set_proc($ABB53AB440107FB7,@ps4_sceNpTrophyCreateHandle);
 lib^.set_proc($18D705E2889D6346,@ps4_sceNpTrophyDestroyHandle);
 lib^.set_proc($4C9080C6DA3D4845,@ps4_sceNpTrophyRegisterContext);
 lib^.set_proc($2C7B9298EDD22DDF,@ps4_sceNpTrophyGetTrophyUnlockState);
 lib^.set_proc($DBCC6645415AA3AF,@ps4_sceNpTrophyUnlockTrophy);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpTrophy.prx',@Load_libSceNpTrophy);

end.

