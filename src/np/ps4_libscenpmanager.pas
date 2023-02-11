unit ps4_libSceNpManager;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_program,
  np_error,
  ps4_libSceNpCommon;

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

 //SceNpGamePresenceStatus
 SCE_NP_GAME_PRESENCE_STATUS_OFFLINE=0;
 SCE_NP_GAME_PRESENCE_STATUS_ONLINE =1;

//SceNpReachabilityState
 SCE_NP_REACHABILITY_STATE_UNAVAILABLE=0;
 SCE_NP_REACHABILITY_STATE_AVAILABLE  =1;
 SCE_NP_REACHABILITY_STATE_REACHABLE  =2;

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

 SceNpGamePresenceCallbackA=procedure(userId:Integer;
                                      status:Integer; //SceNpGamePresenceStatus
                                      userdata:Pointer); SysV_ABI_CDecl;

 SceNpPlusEventCallback=procedure(userId:SceUserServiceUserId;
                                  event:Integer; //SceNpPlusEventType
                                  userdata:Pointer); SysV_ABI_CDecl;

 SceNpReachabilityStateCallback=procedure(userId:SceUserServiceUserId;
                                          state:Integer; //SceNpReachabilityState
                                          userdata:Pointer); SysV_ABI_CDecl;

implementation

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

function sys_get_country_code(p:pSceNpCountryCode):Integer;
var
 g:GEOID;
 s:integer;
 b:RawByteString;
begin
 Result:=0;
 g:=GetUserGeoID(GEOCLASS_NATION);
 s:=GetGeoInfoA(g,GEO_ISO2,nil,0,0);

 b:='';
 SetLength(b,s);
 GetGeoInfoA(g,GEO_ISO2,PChar(b),s,0);

 if (s>=2) then
 begin
  p^.data[0]:=LowerCase(b[1]);
  p^.data[1]:=LowerCase(b[2]);
 end else
 begin
  p^.data[0]:='u';
  p^.data[1]:='s';
 end;
end;

function ps4_sceNpGetAccountCountry(onlineId:pSceNpOnlineId;pCountryCode:pSceNpCountryCode):Integer; SysV_ABI_CDecl;
begin
 if (onlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pCountryCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pCountryCode^:=Default(SceNpCountryCode);

 Result:=sys_get_country_code(pCountryCode);
end;

function ps4_sceNpGetAccountCountryA(userId:Integer;pCountryCode:pSceNpCountryCode):Integer; SysV_ABI_CDecl;
begin
 if (pCountryCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pCountryCode^:=Default(SceNpCountryCode);

 Result:=sys_get_country_code(pCountryCode);
end;

function ps4_sceNpGetNpId(userId:Integer;npId:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 if (npId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 npId^:=Default(SceNpId);
 npId^.handle.data:='user';
 Result:=0;
end;

function ps4_sceNpGetOnlineId(userId:Integer;onlineId:pSceNpOnlineId):Integer; SysV_ABI_CDecl;
begin
 if (onlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 onlineId^:=Default(SceNpOnlineId);
 onlineId^.data:='user';
 Result:=0;
end;

function ps4_sceNpGetState(userId:Integer;state:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (state=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 state^:=SCE_NP_STATE_SIGNED_OUT;
 Result:=0;
end;

function ps4_sceNpGetGamePresenceStatus(pOnlineId:pSceNpOnlineId;pStatus:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pStatus=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pStatus^:=SCE_NP_GAME_PRESENCE_STATUS_OFFLINE;
 Result:=0;
end;

function ps4_sceNpGetGamePresenceStatusA(userId:Integer;pStatus:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pStatus=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pStatus^:=SCE_NP_GAME_PRESENCE_STATUS_OFFLINE;
 Result:=0;
end;

function ps4_sceNpGetNpReachabilityState(userId:Integer;state:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (state=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 state^:=SCE_NP_REACHABILITY_STATE_UNAVAILABLE;
 Result:=0;
end;

function ps4_sceNpHasSignedUp(userId:Integer;hasSignedUp:PBoolean):Integer; SysV_ABI_CDecl;
begin
 if (hasSignedUp=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 hasSignedUp^:=True;
 Result:=0;
end;

function GetStr(p:Pointer;L:SizeUint):RawByteString; inline;
begin
 SetString(Result,P,L);
end;

function ps4_sceNpSetNpTitleId(titleId:PSceNpTitleId;titleSecret:PSceNpTitleSecret):Integer; SysV_ABI_CDecl;
begin
 if (titleId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (titleSecret=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 Writeln('sceNpSetNpTitleId:',GetStr(@titleId^.id,StrLen(@titleId^.id)));
 Result:=0;
end;

function ps4_sceNpCheckCallback():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function ps4_sceNpRegisterStateCallbackForToolkit(callback:SceNpStateCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterStateCallbackForToolkit:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//

function ps4_sceNpRegisterStateCallback(callback:SceNpStateCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpRegisterStateCallbackA(callback:SceNpStateCallbackA;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterStateCallback:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpRegisterGamePresenceCallback(callback:SceNpGamePresenceCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpRegisterGamePresenceCallbackA(callback:SceNpGamePresenceCallbackA;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpRegisterPlusEventCallback(callback:SceNpPlusEventCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterPlusEventCallback():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpRegisterNpReachabilityStateCallback(callback:SceNpReachabilityStateCallback;userdata:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterNpReachabilityStateCallback():Integer; SysV_ABI_CDecl;
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

function ps4_sceNpAbortRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
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
 pSceNpParentalControlInfo=^SceNpParentalControlInfo;
 SceNpParentalControlInfo=packed record
  contentRestriction:Boolean;
  chatRestriction   :Boolean;
  ugcRestriction    :Boolean;
 end;

function ps4_sceNpGetParentalControlInfo(reqId:Integer;
                                         pOnlineId:pSceNpOnlineId;
                                         pAge:PByte;
                                         pInfo:pSceNpParentalControlInfo
                                        ):Integer; SysV_ABI_CDecl;
begin
 if (pOnlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pAge=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pInfo=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pAge^:=18;

 pInfo^.contentRestriction:=False;
 pInfo^.chatRestriction   :=False;
 pInfo^.ugcRestriction    :=False;

 Result:=0;
end;

function ps4_sceNpGetParentalControlInfoA(reqId:Integer;
                                          userId:Integer;
                                          pAge:PByte;
                                          pInfo:pSceNpParentalControlInfo
                                         ):Integer; SysV_ABI_CDecl;
begin
 if (pAge=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pInfo=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pAge^:=18;

 pInfo^.contentRestriction:=False;
 pInfo^.chatRestriction   :=False;
 pInfo^.ugcRestriction    :=False;

 Result:=0;
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
 if (pParam=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pResult=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pResult^.authorized:=False;
 Result:=0;
end;

type
 pSceNpNotifyPlusFeatureParameter=^SceNpNotifyPlusFeatureParameter;
 SceNpNotifyPlusFeatureParameter=packed record
  size:QWORD;
  userId:Integer;
  padding:Integer;
  features:QWORD;
  reserved:array[0..31] of Byte;
 end;

function ps4_sceNpNotifyPlusFeature(pParam:pSceNpNotifyPlusFeatureParameter):Integer; SysV_ABI_CDecl;
begin
 if (pParam=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 Result:=0;
end;

//

function ps4_sceNpPollAsync(reqId:Integer;
                            pResult:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pResult=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pResult^:=0;
 Result:=0; //SCE_NP_POLL_ASYNC_RET_FINISHED
end;

//

function ps4_sceNpCheckCallbackForLib():Integer; SysV_ABI_CDecl;
begin
 //if (Cb4Toolkit.callback<>nil) then
 //begin
 // Cb4Toolkit.callback(0,SCE_NP_STATE_SIGNED_OUT,nil,Cb4Toolkit.userdata);
 //end;
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
 lib^.set_proc($ADB9276948E9A96A,@ps4_sceNpGetAccountIdA);
 lib^.set_proc($1A1CFD8960D4B42E,@ps4_sceNpGetAccountCountry);
 lib^.set_proc($253FADD346B74F10,@ps4_sceNpGetAccountCountryA);
 lib^.set_proc($A7FA3BE029E83736,@ps4_sceNpGetNpId);
 lib^.set_proc($5C39DC5D02095129,@ps4_sceNpGetOnlineId);
 lib^.set_proc($7901FB9D63DC0207,@ps4_sceNpGetState);
 lib^.set_proc($20F6F585DD700067,@ps4_sceNpGetGamePresenceStatus);
 lib^.set_proc($A0F3BD538D98A602,@ps4_sceNpGetGamePresenceStatusA);
 lib^.set_proc($7BF66E846128782E,@ps4_sceNpGetNpReachabilityState);
 lib^.set_proc($39A777AEF63F3494,@ps4_sceNpHasSignedUp);
 lib^.set_proc($11CEB7CB9F65F6DC,@ps4_sceNpSetNpTitleId);
 lib^.set_proc($DD997C05E3D387D6,@ps4_sceNpCheckCallback);
 lib^.set_proc($55F45298F9A3F10F,@ps4_sceNpRegisterStateCallback);
 lib^.set_proc($A9025F3BC1C089A6,@ps4_sceNpRegisterStateCallbackA);
 lib^.set_proc($9A38D35E1F8D1D66,@ps4_sceNpUnregisterStateCallback);
 lib^.set_proc($B8526968A341023E,@ps4_sceNpRegisterGamePresenceCallback);
 lib^.set_proc($2ACC312F19387356,@ps4_sceNpRegisterGamePresenceCallbackA);
 lib^.set_proc($1889880A787E6E80,@ps4_sceNpRegisterPlusEventCallback);
 lib^.set_proc($C558AA25D0E02A5D,@ps4_sceNpUnregisterPlusEventCallback);
 lib^.set_proc($870E4A36A0007A5B,@ps4_sceNpRegisterNpReachabilityStateCallback);
 lib^.set_proc($71120B004BE7FBD3,@ps4_sceNpUnregisterNpReachabilityStateCallback);
 lib^.set_proc($1A92D00CD28809A7,@ps4_sceNpCreateRequest);
 lib^.set_proc($7A2A8C0ADF54B212,@ps4_sceNpCreateAsyncRequest);
 lib^.set_proc($4BB4139FBD8FAC3C,@ps4_sceNpDeleteRequest);
 lib^.set_proc($3B32AF4EF8376585,@ps4_sceNpAbortRequest);
 lib^.set_proc($DABB059A519695E4,@ps4_sceNpCheckNpAvailability);
 lib^.set_proc($F19D897391AF1832,@ps4_sceNpCheckNpAvailabilityA);
 lib^.set_proc($29F199836CBBDE83,@ps4_sceNpCheckNpReachability);
 lib^.set_proc($8A5C0B338CCE9AEE,@ps4_sceNpGetParentalControlInfo);
 lib^.set_proc($9BD2F73BACACB7F5,@ps4_sceNpGetParentalControlInfoA);
 lib^.set_proc($AFA33260992BCB3F,@ps4_sceNpCheckPlus);
 lib^.set_proc($19AC6BA7711663F3,@ps4_sceNpNotifyPlusFeature);
 lib^.set_proc($BAA70F24B58BD3C3,@ps4_sceNpPollAsync);

 lib:=Result._add_lib('libSceNpManagerForToolkit');
 lib^.set_proc($D1CEC76D744A52DE,@ps4_sceNpRegisterStateCallbackForToolkit);
 lib^.set_proc($608BEAAAF2728C47,@ps4_sceNpUnregisterStateCallbackForToolkit);
 lib^.set_proc($2442C77F8C4FB9FA,@ps4_sceNpCheckCallbackForLib);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpManager.prx',@Load_libSceNpManager);

end.

