unit ps4_libSceNpManager;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes, SysUtils;

implementation

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

function ps4_sceNpSetContentRestriction(pRestriction:PSceNpContentRestriction):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpSetContentRestriction:',HexStr(pRestriction));
 Result:=0;
end;

const
 SCE_NP_ONLINEID_MIN_LENGTH=3;
 SCE_NP_ONLINEID_MAX_LENGTH=16;

type
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


function ps4_sceNpGetNpId(userId:Integer;npId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 npId^:=Default(SceNpId);
 npId^.handle.data:='user';
 Result:=0;
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

const
 SCE_NP_STATE_UNKNOWN    =0;
 SCE_NP_STATE_SIGNED_OUT =1;
 SCE_NP_STATE_SIGNED_IN  =2;

type
 SceUserServiceUserId=Integer;

 SceNpStateCallbackA=procedure(userId:SceUserServiceUserId;state:Integer;userdata:Pointer); SysV_ABI_CDecl;

//int sceNpRegisterStateCallbackA(
//		SceNpStateCallbackA callback,
//		void *userdata);

const
 SCE_NP_ERROR_INVALID_ARGUMENT=$80550003;
 SCE_NP_ERROR_CALLBACK_ALREADY_REGISTERED=$80550008;

var
 Cb4Toolkit:packed record
  callback:SceNpStateCallbackA;
  userdata:Pointer;
 end;

function ps4_sceNpRegisterStateCallbackForToolkit(callback:SceNpStateCallbackA;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Cb4Toolkit.callback:=callback;
 Cb4Toolkit.userdata:=userdata;
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

function Load_libSceNpManager(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNpManager');
 lib^.set_proc($036090DE4812A294,@ps4_sceNpSetContentRestriction);
 lib^.set_proc($A7FA3BE029E83736,@ps4_sceNpGetNpId);
 lib^.set_proc($11CEB7CB9F65F6DC,@ps4_sceNpSetNpTitleId);
 lib^.set_proc($DD997C05E3D387D6,@ps4_sceNpCheckCallback);

 lib:=Result._add_lib('libSceNpManagerForToolkit');
 lib^.set_proc($D1CEC76D744A52DE,@ps4_sceNpRegisterStateCallbackForToolkit);
 lib^.set_proc($2442C77F8C4FB9FA,@ps4_sceNpCheckCallbackForLib);

end;

initialization
 ps4_app.RegistredPreLoad('libSceNpManager.prx',@Load_libSceNpManager);

end.

