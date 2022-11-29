unit ps4_libSceNpManager;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_program,
  Classes,
  SysUtils;

Const
 SCE_NP_COUNTRY_CODE_LENGTH=2;

type
 // Np country code (ISO 3166-1 two-letter system)
 pSceNpCountryCode=^SceNpCountryCode;
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
 //SceNpState
 SCE_NP_STATE_UNKNOWN    =0;
 SCE_NP_STATE_SIGNED_OUT =1;
 SCE_NP_STATE_SIGNED_IN  =2;

type
 pSceNpCreateAsyncRequestParameter=^SceNpCreateAsyncRequestParameter;
 SceNpCreateAsyncRequestParameter=packed record
  size:qword;
  cpuAffinityMask:qword; //SceKernelCpumask
  threadPriority:Integer;
  padding:Integer;
 end;

type
 SceUserServiceUserId=Integer;

 SceNpStateCallback=procedure(userId:SceUserServiceUserId;
                              state:Integer; //SceNpState
                              npId:pSceNpId;
                              userdata:Pointer); SysV_ABI_CDecl;

 SceNpStateCallbackA=procedure(userId:SceUserServiceUserId;
                               state:Integer; //SceNpState
                               userdata:Pointer); SysV_ABI_CDecl;

 SceNpGamePresenceCallback=procedure(pOnlineId:pSceNpOnlineId;
                                     status:Integer; //SceNpGamePresenceStatus
                                     userdata:Pointer); SysV_ABI_CDecl;

 SceNpPlusEventCallback=procedure(userId:SceUserServiceUserId;
                                  event:Integer; //SceNpPlusEventType
                                  userdata:Pointer); SysV_ABI_CDecl;

 SceNpReachabilityStateCallback=procedure(userId:SceUserServiceUserId;
                                          state:Integer; //SceNpReachabilityState
                                          userdata:Pointer); SysV_ABI_CDecl;

const
 SCE_NP_ERROR_INVALID_ARGUMENT=Integer($80550003);
 SCE_NP_ERROR_CALLBACK_ALREADY_REGISTERED=Integer($80550008);

 SCE_NP_ERROR_SIGNED_OUT=Integer($80550006);

implementation

uses
 ps4_map_mm;

function ps4_sceNpSetContentRestriction(pRestriction:PSceNpContentRestriction):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNpSetContentRestriction:',HexStr(pRestriction));
 Result:=0;
end;

function ps4_sceNpGetAccountIdA(userId:Integer;pAccountId:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (pAccountId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pAccountId^:=1111;
 Result:=0;
end;

const
 GEOCLASS_NATION = 16;

 GEO_ISO2 = $0004;

type
 GEOID = LONG;
 GEOTYPE = DWORD;
 GEOCLASS = DWORD;

function GetUserGeoID(GeoClass: GEOCLASS):GEOID; stdcall external kernel32;

function GetGeoInfoA(Location: GEOID; GeoType: GEOTYPE; lpGeoData: LPSTR;
  cchData: Integer; LangId: LANGID): Integer; stdcall external kernel32;

function ps4_sceNpGetAccountCountry(onlineId:pSceNpOnlineId;pCountryCode:pSceNpCountryCode):Integer; SysV_ABI_CDecl;
var
 g:GEOID;
 s:integer;
 b:RawByteString;
begin
 if (onlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pCountryCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 g:=GetUserGeoID(GEOCLASS_NATION);
 s:=GetGeoInfoA(g,GEO_ISO2,nil,0,0);
 SetLength(b,s);
 GetGeoInfoA(g,GEO_ISO2,PChar(b),s,0);

 if (s>=2) then
 begin
  pCountryCode^:=Default(SceNpCountryCode);
  pCountryCode^.data[0]:=LowerCase(b[1]);
  pCountryCode^.data[1]:=LowerCase(b[2]);
 end else
 begin
  pCountryCode^:=Default(SceNpCountryCode);
  pCountryCode^.data[0]:='u';
  pCountryCode^.data[1]:='s';
 end;

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

function ps4_sceNpGetState(userId:Integer;state:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (state<>nil) then state^:=SCE_NP_STATE_SIGNED_OUT;
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

function ps4_sceNpRegisterStateCallbackA(callback:SceNpStateCallbackA;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
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

function ps4_sceNpRegisterNpReachabilityStateCallback(callback:SceNpReachabilityStateCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpCreateRequest():Integer; SysV_ABI_CDecl;
begin
 Result:=11;
end;

function ps4_sceNpCreateAsyncRequest(pParam:pSceNpCreateAsyncRequestParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=22;
end;

function ps4_sceNpDeleteRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpCheckNpAvailability(reqId:Integer;onlineId:pSceNpOnlineId;pReserved:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 //Result:=SCE_NP_ERROR_SIGNED_OUT;
end;

function ps4_sceNpCheckNpAvailabilityA(reqId,userId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 //Result:=SCE_NP_ERROR_SIGNED_OUT;
end;

function ps4_sceNpCheckNpReachability(reqId,userId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 //Result:=SCE_NP_ERROR_SIGNED_OUT;
end;

type
 pSceNpCheckPlusParameter=^SceNpCheckPlusParameter;
 SceNpCheckPlusParameter=packed record
  size:QWORD;
  userId:Integer;
  padding:array[0..3] of Byte;
  features:QWORD;
  reserved:array[0..31] of Byte;
 end;

 pSceNpCheckPlusResult=^SceNpCheckPlusResult;
 SceNpCheckPlusResult=packed record
  authorized:Boolean;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceNpCheckPlus(reqId:Integer;
                            pParam:pSceNpCheckPlusParameter;
                            pResult:pSceNpCheckPlusResult
                           ):Integer; SysV_ABI_CDecl;
begin
 if (pResult<>nil) then
 begin
  pResult^.authorized:=False;
 end;
 Result:=0;
end;

function ps4_sceNpPollAsync(reqId:Integer;
                            pResult:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pResult<>nil) then
 begin
  pResult^:=0;
 end;
 Result:=0; //SCE_NP_POLL_ASYNC_RET_FINISHED
end;

//

function ps4_sceNpCheckCallbackForLib():Integer; SysV_ABI_CDecl;
begin
 if (Cb4Toolkit.callback<>nil) then
 begin
  //Cb4Toolkit.callback(0,SCE_NP_STATE_SIGNED_OUT,nil,Cb4Toolkit.userdata);
 end;
 Result:=0;
end;

Const
 SCE_NP_UTIL_ERROR_NOT_MATCH=Integer($80550609);

function ps4_sceNpCmpNpId(npid1,npid2:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 if (npid1=nil) or (npid2=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 if (CompareChar0(npid1^.handle,npid2^.handle,SCE_NP_ONLINEID_MAX_LENGTH)=0) and
    (QWORD(npid1^.opt)=QWORD(npid2^.opt)) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_NP_UTIL_ERROR_NOT_MATCH;
 end;

end;

type
 pnp_mem=^np_mem;
 np_mem=packed record
  len:qword;
  unknow:qword;
  ptr:Pointer;
 end;

function ps4_sceNpAllocateKernelMemoryWithAlignment(
          len:qword;
          name:Pchar;
          ptr_out:PPointer;
          mem_out:pnp_mem):Integer; SysV_ABI_CDecl;
var
 pad_len:qword;
begin
 if (mem_out=nil) then
 begin
  Exit(-$7faa7ffb); //NP-32268-1
 end;

 mem_out^.unknow:=0;
 pad_len:=0;
 if (len and $3fff)<>0 then
 begin
  pad_len:=$4000-(len and $3fff);
 end;
 mem_out^.len:=pad_len+len;

 Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result >-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

function ps4_sceNpAllocateKernelMemoryNoAlignment(
          len:qword;
          name:Pchar;
          ptr_out:PPointer;
          mem_out:pnp_mem):Integer; SysV_ABI_CDecl;
begin
 if (mem_out=nil) then
 begin
  Exit(-$7faa7ffb); //NP-32268-1
 end;

 mem_out^.unknow:=0;
 mem_out^.len:=len;

 Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result >-1) then
 begin
  ptr_out^:=mem_out^.ptr;
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
 lib^.set_proc($ADB9276948E9A96A,@ps4_sceNpGetAccountIdA);
 lib^.set_proc($1A1CFD8960D4B42E,@ps4_sceNpGetAccountCountry);
 lib^.set_proc($A7FA3BE029E83736,@ps4_sceNpGetNpId);
 lib^.set_proc($5C39DC5D02095129,@ps4_sceNpGetOnlineId);
 lib^.set_proc($7901FB9D63DC0207,@ps4_sceNpGetState);
 lib^.set_proc($11CEB7CB9F65F6DC,@ps4_sceNpSetNpTitleId);
 lib^.set_proc($DD997C05E3D387D6,@ps4_sceNpCheckCallback);
 lib^.set_proc($55F45298F9A3F10F,@ps4_sceNpRegisterStateCallback);
 lib^.set_proc($A9025F3BC1C089A6,@ps4_sceNpRegisterStateCallbackA);
 lib^.set_proc($B8526968A341023E,@ps4_sceNpRegisterGamePresenceCallback);
 lib^.set_proc($1889880A787E6E80,@ps4_sceNpRegisterPlusEventCallback);
 lib^.set_proc($870E4A36A0007A5B,@ps4_sceNpRegisterNpReachabilityStateCallback);
 lib^.set_proc($1A92D00CD28809A7,@ps4_sceNpCreateRequest);
 lib^.set_proc($7A2A8C0ADF54B212,@ps4_sceNpCreateAsyncRequest);
 lib^.set_proc($4BB4139FBD8FAC3C,@ps4_sceNpDeleteRequest);
 lib^.set_proc($DABB059A519695E4,@ps4_sceNpCheckNpAvailability);
 lib^.set_proc($F19D897391AF1832,@ps4_sceNpCheckNpAvailabilityA);
 lib^.set_proc($29F199836CBBDE83,@ps4_sceNpCheckNpReachability);
 lib^.set_proc($AFA33260992BCB3F,@ps4_sceNpCheckPlus);
 lib^.set_proc($BAA70F24B58BD3C3,@ps4_sceNpPollAsync);

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
 lib^.set_proc($80C958E9E7B0AFF7,@ps4_sceNpAllocateKernelMemoryWithAlignment);
 lib^.set_proc($3163CE92ACD8B2CD,@ps4_sceNpAllocateKernelMemoryNoAlignment);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpManager.prx',@Load_libSceNpManager);
 ps4_app.RegistredPreLoad('libSceNpCommon.prx' ,@Load_libSceNpCommon);

end.

