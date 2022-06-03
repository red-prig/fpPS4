unit ps4_libSceNpManager;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

Const
 SCE_NP_COUNTRY_CODE_LENGTH=2;

type
 // Np country code (ISO 3166-1 two-letter system)
 SceNpCountryCode=packed record
  data:array[0..SCE_NP_COUNTRY_CODE_LENGTH-1] of AnsiChar;
  term:AnsiChar;
  padding:array[0..1] of AnsiChar;
 end;

 SceNpAgeRestriction=packed record
  countryCode:SceNpCountryCode;
  age:Shortint;
  padding:array[0..2] of Byte;
 end;

 PSceNpContentRestriction=^SceNpContentRestriction;
 SceNpContentRestriction=packed record
  size:QWORD;
  defaultAgeRestriction:Byte;
  padding:array[0..2] of Byte;
  ageRestrictionCount:Integer;
  ageRestriction:SceNpAgeRestriction;
 end;

const
 SCE_NP_ONLINEID_MIN_LENGTH=3;
 SCE_NP_ONLINEID_MAX_LENGTH=16;

type
 pSceNpOnlineId=^SceNpOnlineId;
 SceNpOnlineId=packed record
  data:array[0..SCE_NP_ONLINEID_MAX_LENGTH-1] of AnsiChar;
  term:AnsiChar;
  dummy:array[0..2] of AnsiChar;
 end;

 PSceNpId=^SceNpId;
 SceNpId=packed record
  handle:SceNpOnlineId;
  opt:array[0..7] of Byte;
  reserved:array[0..7] of Byte;
 end;

const
 SCE_NP_TITLE_ID_LEN=12;

type
 PSceNpTitleId=^SceNpTitleId;
 SceNpTitleId=packed record
  id:array[0..SCE_NP_TITLE_ID_LEN] of Char;
  padding:array[0..2] of Byte;
 end;

const
 SCE_NP_TITLE_SECRET_SIZE=128;

type
 PSceNpTitleSecret=^SceNpTitleSecret;
 SceNpTitleSecret=array[0..SCE_NP_TITLE_SECRET_SIZE-1] of Byte;

const
 SCE_NP_STATE_UNKNOWN    =0;
 SCE_NP_STATE_SIGNED_OUT =1;
 SCE_NP_STATE_SIGNED_IN  =2;

type
 SceUserServiceUserId=Integer;

 SceNpStateCallback=procedure(userId:SceUserServiceUserId;
                              state:Integer;
                              npId:pSceNpId;
                              userdata:Pointer); SysV_ABI_CDecl;

 SceNpStateCallbackA=procedure(userId:SceUserServiceUserId;
                               state:Integer;
                               userdata:Pointer); SysV_ABI_CDecl;

 SceNpGamePresenceCallback=procedure(pOnlineId:pSceNpOnlineId;
                                     status:Integer;
                                     userdata:Pointer); SysV_ABI_CDecl;

 SceNpPlusEventCallback=procedure(userId:SceUserServiceUserId;
                                  event:Integer;
                                  userdata:Pointer); SysV_ABI_CDecl;

const
 SCE_NP_ERROR_INVALID_ARGUMENT=$80550003;
 SCE_NP_ERROR_CALLBACK_ALREADY_REGISTERED=$80550008;

implementation

function ps4_sceNpSetContentRestriction(pRestriction:PSceNpContentRestriction):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpSetContentRestriction:',HexStr(pRestriction));
 Result:=0;
end;

function ps4_sceNpGetNpId(userId:Integer;npId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 npId^:=Default(SceNpId);
 npId^.handle.data:='user';
 Result:=0;
end;

function ps4_sceNpGetOnlineId(userId:Integer;onlineId:pSceNpOnlineId):Integer; SysV_ABI_CDecl;
begin
 onlineId^:=Default(SceNpOnlineId);
 onlineId^.data:='user';
 Result:=0;
end;

function GetStr(p:Pointer;L:SizeUint):RawByteString; inline;
begin
 SetString(Result,P,L);
end;

function ps4_sceNpSetNpTitleId(titleId:PSceNpTitleId;titleSecret:PSceNpTitleSecret):Integer; SysV_ABI_CDecl;
begin
 Writeln(GetStr(@titleId^.id,StrLen(@titleId^.id)));
 Result:=0;
end;

function ps4_sceNpCheckCallback():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

var
 Cb4Toolkit:packed record
  callback:SceNpStateCallback;
  userdata:Pointer;
 end;

function ps4_sceNpRegisterStateCallbackForToolkit(callback:SceNpStateCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Cb4Toolkit.callback:=callback;
 Cb4Toolkit.userdata:=userdata;
 Result:=0;
end;

function ps4_sceNpRegisterStateCallback(callback:SceNpStateCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Cb4Toolkit.callback:=callback;
 Cb4Toolkit.userdata:=userdata;
 Result:=0;
end;

function ps4_sceNpRegisterGamePresenceCallback(callback:SceNpGamePresenceCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpRegisterPlusEventCallback(callback:SceNpPlusEventCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpCheckCallbackForLib():Integer; SysV_ABI_CDecl;
begin
 if (Cb4Toolkit.callback<>nil) then
 begin
  //Cb4Toolkit.callback(0,SCE_NP_STATE_SIGNED_OUT,Cb4Toolkit.userdata);
 end;
 Result:=0;
end;

Const
 SCE_NP_UTIL_ERROR_NOT_MATCH=$80550609;

function ps4_sceNpCmpNpId(npid1,npid2:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 if (npid1=nil) or (npid2=nil) then Exit(Integer(SCE_NP_ERROR_INVALID_ARGUMENT));

 if (CompareChar0(npid1^.handle,npid2^.handle,SCE_NP_ONLINEID_MAX_LENGTH)=0) and
    (QWORD(npid1^.opt)=QWORD(npid2^.opt)) then
 begin
  Result:=0;
 end else
 begin
  Result:=Integer(SCE_NP_UTIL_ERROR_NOT_MATCH);
 end;

end;

function Load_libSceNpManager(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpManager');
 lib^.set_proc($036090DE4812A294,@ps4_sceNpSetContentRestriction);
 lib^.set_proc($A7FA3BE029E83736,@ps4_sceNpGetNpId);
 lib^.set_proc($5C39DC5D02095129,@ps4_sceNpGetOnlineId);
 lib^.set_proc($11CEB7CB9F65F6DC,@ps4_sceNpSetNpTitleId);
 lib^.set_proc($DD997C05E3D387D6,@ps4_sceNpCheckCallback);
 lib^.set_proc($55F45298F9A3F10F,@ps4_sceNpRegisterStateCallback);
 lib^.set_proc($B8526968A341023E,@ps4_sceNpRegisterGamePresenceCallback);
 lib^.set_proc($1889880A787E6E80,@ps4_sceNpRegisterPlusEventCallback);

 lib:=Result._add_lib('libSceNpManagerForToolkit');
 lib^.set_proc($D1CEC76D744A52DE,@ps4_sceNpRegisterStateCallbackForToolkit);
 lib^.set_proc($2442C77F8C4FB9FA,@ps4_sceNpCheckCallbackForLib);
end;

function Load_libSceNpCommon(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpCommon');
 lib^.set_proc($8BC5265D34AAECDE,@ps4_sceNpCmpNpId);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpManager.prx',@Load_libSceNpManager);
 ps4_app.RegistredPreLoad('libSceNpCommon.prx',@Load_libSceNpCommon);

end.

