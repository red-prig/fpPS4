unit ps4_libSceNpManager;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  windows,
  subr_dynlib,
  np_error,
  ps4_libSceUserService,
  ps4_libSceNpCommon;

type
 SceNpStateCallback=procedure(userId:SceUserServiceUserId;
                              state:SceNpState;
                              npId:pSceNpId;
                              userdata:Pointer);

 SceNpStateCallbackA=procedure(userId:SceUserServiceUserId;
                               state:SceNpState;
                               userdata:Pointer);

 SceNpGamePresenceCallback=procedure(pOnlineId:pSceNpOnlineId;
                                     status:SceNpGamePresenceStatus;
                                     userdata:Pointer);

 SceNpGamePresenceCallbackA=procedure(userId:SceUserServiceUserId;
                                      status:SceNpGamePresenceStatus;
                                      userdata:Pointer);

 SceNpPlusEventCallback=procedure(userId:SceUserServiceUserId;
                                  event:SceNpPlusEventType;
                                  userdata:Pointer);

 SceNpReachabilityStateCallback=procedure(userId:SceUserServiceUserId;
                                          state:SceNpReachabilityState;
                                          userdata:Pointer);

//SceNpInGameMessage

const
 NP_IN_GAME_MESSAGE_POOL_SIZE=(16*1024);
 SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX=512;

type
 pSceNpInGameMessageData=^SceNpInGameMessageData;
 SceNpInGameMessageData=packed record
  data    :array[0..SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX-1] of AnsiChar;
  dataSize:QWORD;
 end;

 SceNpInGameMessageEventCallbackA=procedure(libCtxId,pTo:Integer;pToOnlineId:pSceNpOnlineId;pFrom:Integer;pMessage:pSceNpInGameMessageData;pUserArg:Pointer);

 //SceNpInGameMessage

implementation

function ps4_sceNpSetContentRestriction(pRestriction:PSceNpContentRestriction):Integer;
begin
 Writeln('sceNpSetContentRestriction:',HexStr(pRestriction));
 Result:=0;
end;

function ps4_sceNpGetAccountId(onlineId:pSceNpOnlineId;pAccountId:PQWORD):Integer;
begin
 if (onlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pAccountId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pAccountId^:=1111;
 Result:=0;
end;

function ps4_sceNpGetAccountIdA(userId:SceUserServiceUserId;pAccountId:PQWORD):Integer;
begin
 if (pAccountId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pAccountId^:=1111;
 Result:=0;
end;

function ps4_sceNpGetUserIdByAccountId(AccountId:QWORD;userId:pSceUserServiceUserId):Integer;
begin
 if (userId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 if (AccountId=1111) then
 begin
  userId^:=base_user_id;
  Result:=0;
 end else
 begin
  Result:=SCE_NP_ERROR_USER_NOT_FOUND;
 end;
end;

{
ja Japanese
en English
fr French
es Spanish
de German
it Italian
nl Dutch
pt Portuguese
ru Russian
ko Korean
zh-TW Chinese (Traditional)
zh-CN Chinese (Simplified)
fi Finnish
sv Swedish
da Danish
no Norwegian
pl Polish
tr Turkish
ar Arabic
}

function ps4_sceNpGetAccountLanguage(reqId:Integer;pOnlineId:pSceNpOnlineId;pLangCode:pSceNpLanguageCode):Integer;
begin
 if (pLangCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pLangCode^.code:='en';
 Result:=0;
end;

function ps4_sceNpGetAccountLanguageA(reqId:Integer;userId:SceUserServiceUserId;pLangCode:pSceNpLanguageCode):Integer;
begin
 if (pLangCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pLangCode^.code:='en';
 Result:=0;
end;

function ps4_sceNpInGameMessageInitialize(poolSize:size_t;pOption:Pointer):Integer;
begin
 Result:=6;
end;

function ps4_SceNpInGameMessageTerminate(libCtxId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpInGameMessageCreateHandle(libCtxId:Integer):Integer;
begin
 Result:=3;
 //SCE_NP_IN_GAME_MESSAGE_ERROR_NOT_SIGNED_IN 0x80552B04
end;

function ps4_sceNpInGameMessageDeleteHandle(libCtxId,handleId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpInGameMessagePrepareA(libCtxId,handleId:Integer;pReserved:Pointer;cbFunc:SceNpInGameMessageEventCallbackA;pUserArg:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpInGameMessageSendDataA(libCtxId:Integer;pTo:pSceNpPeerAddressA;pFrom:pSceNpPeerAddressA;pMessage:pSceNpInGameMessageData):Integer;
begin
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

{
ae UAE
ar Argentina
at Austria
au Australia
be Belgium
bg Bulgaria
bh Bahrain
bo Bolivia
br Brazil
ca Canada
ch Switzerland
cl Chile
cn China
co Colombia
cr Costa Rica
cy Cyprus
cz Czech Republic
de Germany
dk Denmark
ec Ecuador
es Spain
fi Finland
fr France
gb UK
gr Greece
gt Guatemala
hk Hong Kong
hn Honduras
hr Croatia
hu Hungary
id Indonesia
ie Ireland
il Israel
in India
is Iceland
it Italy
jp Japan
kr Korea
kw Kuwait
lb Lebanon
lu Luxembourg
mt Malta
mx Mexico
my Malaysia
ni Nicaragua
nl Netherlands
no Norway
nz New Zealand
om Oman
pa Panama
pe Peru
pl Poland
pt Portugal
py Paraguay
qa Qatar
ro Romania
ru Russia
sa Saudi Arabia
se Sweden
sg Singapore
si Slovenia
sk Slovakia
sv El Salvador
th Thailand
tr Turkey
tw Taiwan
ua Ukraine
us United States
uy Uruguay
za South Africa
}

function ps4_sceNpGetAccountCountry(onlineId:pSceNpOnlineId;pCountryCode:pSceNpCountryCode):Integer;
begin
 if (onlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pCountryCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pCountryCode^:=Default(SceNpCountryCode);

 Result:=sys_get_country_code(pCountryCode);
end;

function ps4_sceNpGetAccountCountryA(userId:SceUserServiceUserId;pCountryCode:pSceNpCountryCode):Integer;
begin
 if (pCountryCode=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pCountryCode^:=Default(SceNpCountryCode);

 Result:=sys_get_country_code(pCountryCode);
end;

function ps4_sceNpGetNpId(userId:SceUserServiceUserId;npId:PSceNpId):Integer;
begin
 if (npId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 npId^:=Default(SceNpId);
 npId^.handle.data:='user';
 Result:=0;
end;

function ps4_sceNpGetOnlineId(userId:SceUserServiceUserId;onlineId:pSceNpOnlineId):Integer;
begin
 if (onlineId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 onlineId^:=Default(SceNpOnlineId);
 onlineId^.data:='user';
 Result:=0;
end;

function ps4_sceNpGetState(userId:SceUserServiceUserId;state:PInteger):Integer;
begin
 if (state=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 state^:=SCE_NP_STATE_SIGNED_IN;
 Result:=0;
end;

function ps4_sceNpGetGamePresenceStatus(pOnlineId:pSceNpOnlineId;pStatus:PInteger):Integer;
begin
 if (pStatus=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pStatus^:=SCE_NP_GAME_PRESENCE_STATUS_OFFLINE;
 Result:=0;
end;

function ps4_sceNpGetGamePresenceStatusA(userId:SceUserServiceUserId;pStatus:PInteger):Integer;
begin
 if (pStatus=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pStatus^:=SCE_NP_GAME_PRESENCE_STATUS_OFFLINE;
 Result:=0;
end;

function ps4_sceNpGetNpReachabilityState(userId:SceUserServiceUserId;state:PInteger):Integer;
begin
 if (state=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 state^:=SCE_NP_REACHABILITY_STATE_UNAVAILABLE;
 Result:=0;
end;

function ps4_sceNpHasSignedUp(userId:SceUserServiceUserId;hasSignedUp:PBoolean):Integer;
begin
 if (hasSignedUp=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 hasSignedUp^:=True;
 Result:=0;
end;

function GetStr(p:Pointer;L:SizeUint):RawByteString; inline;
begin
 SetString(Result,P,L);
end;

function ps4_sceNpSetNpTitleId(titleId:PSceNpTitleId;titleSecret:PSceNpTitleSecret):Integer;
begin
 if (titleId=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (titleSecret=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 Writeln('sceNpSetNpTitleId:',GetStr(@titleId^.id,StrLen(@titleId^.id)));
 Result:=0;
end;

function ps4_sceNpCheckCallback():Integer;
begin
 Result:=0;
end;

//

function ps4_sceNpRegisterStateCallbackForToolkit(callback:SceNpStateCallback;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterStateCallbackForToolkit:Integer;
begin
 Result:=0;
end;

//

function ps4_sceNpRegisterStateCallback(callback:SceNpStateCallback;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpRegisterStateCallbackA(callback:SceNpStateCallbackA;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterStateCallback:Integer;
begin
 Result:=0;
end;

function ps4_sceNpRegisterGamePresenceCallback(callback:SceNpGamePresenceCallback;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpRegisterGamePresenceCallbackA(callback:SceNpGamePresenceCallbackA;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpRegisterPlusEventCallback(callback:SceNpPlusEventCallback;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterPlusEventCallback():Integer;
begin
 Result:=0;
end;

function ps4_sceNpRegisterNpReachabilityStateCallback(callback:SceNpReachabilityStateCallback;userdata:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpUnregisterNpReachabilityStateCallback():Integer;
begin
 Result:=0;
end;

function ps4_sceNpCreateRequest():Integer;
begin
 Result:=11;
end;

function ps4_sceNpCreateAsyncRequest(pParam:pSceNpCreateAsyncRequestParameter):Integer;
begin
 Result:=22;
end;

function ps4_sceNpDeleteRequest(reqId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpAbortRequest(reqId:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpCheckNpAvailability(reqId:Integer;onlineId:pSceNpOnlineId;pReserved:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpCheckNpAvailabilityA(reqId:Integer;userId:SceUserServiceUserId):Integer;
begin
 Result:=0;
end;

function ps4_sceNpCheckNpReachability(reqId:Integer;userId:SceUserServiceUserId):Integer;
begin
 Result:=0;
end;

function ps4_sceNpGetParentalControlInfo(reqId:Integer;
                                         pOnlineId:pSceNpOnlineId;
                                         pAge:PByte;
                                         pInfo:pSceNpParentalControlInfo
                                        ):Integer;
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
                                          userId:SceUserServiceUserId;
                                          pAge:PByte;
                                          pInfo:pSceNpParentalControlInfo
                                         ):Integer;
begin
 if (pAge=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pInfo=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pAge^:=18;

 pInfo^.contentRestriction:=False;
 pInfo^.chatRestriction   :=False;
 pInfo^.ugcRestriction    :=False;

 Result:=0;
end;

function ps4_sceNpCheckPlus(reqId:Integer;
                            pParam:pSceNpCheckPlusParameter;
                            pResult:pSceNpCheckPlusResult
                           ):Integer;
begin
 if (pParam=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if (pResult=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pResult^.authorized:=False;
 Result:=0;
end;

function ps4_sceNpNotifyPlusFeature(pParam:pSceNpNotifyPlusFeatureParameter):Integer;
begin
 if (pParam=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 Result:=0;
end;

function ps4_sceNpUnregisterStateCallbackA(callbackId:Integer):Integer;
begin
 Result:=0;
end;

//

function ps4_sceNpPollAsync(reqId:Integer;
                            pResult:PInteger):Integer;
begin
 if (pResult=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 pResult^:=0;
 Result:=0; //SCE_NP_POLL_ASYNC_RET_FINISHED
end;

//

function ps4_sceNpCheckCallbackForLib():Integer;
begin
 //if (Cb4Toolkit.callback<>nil) then
 //begin
 // Cb4Toolkit.callback(0,SCE_NP_STATE_SIGNED_OUT,nil,Cb4Toolkit.userdata);
 //end;
 Result:=0;
end;

function ps4_sceNpGetAccountDateOfBirth(pOnlineId:pSceNpOnlineId;
                                        pDateOfBirth:PSceNpDate):Integer;
begin
 if pDateOfBirth=nil then
  Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pDateOfBirth^.year :=1990;
 pDateOfBirth^.month:=1;
 pDateOfBirth^.day  :=1;
 Result:=0;
end;

function ps4_sceNpGetAccountDateOfBirthA(userId:SceUserServiceUserId;
                                         pDateOfBirth:PSceNpDate):Integer;
begin
 if pDateOfBirth=nil then
  Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 pDateOfBirth^.year :=1990;
 pDateOfBirth^.month:=1;
 pDateOfBirth^.day  :=1;
 Result:=0;
end;

//

function ps4_sceNpManagerIntCreateRequest():Integer;
begin
 Result:=33;
end;

function ps4_sceNpManagerIntDeleteRequest(req:Integer):Integer;
begin
 Result:=0;
end;

function ps4_sceNpManagerIntSetTimeout(req           :Integer;
                                       resolveRetry  :DWORD;
                                       resolveTimeout:DWORD;
                                       connTimeout   :DWORD;
                                       sendTimeout   :DWORD;
                                       recvTimeout   :DWORD
                                      ):Integer;
begin
 Result:=0;
end;

function ps4_sceNpManagerIntCheckNpAvailability(req    :Integer;
                                                userId :Integer;
                                                is_sync:Boolean):Integer;
begin
 //PS: sceNpCheckNpReachability
 Result:=SCE_NP_ERROR_SIGNED_OUT;
end;

//+JmXFo3Jh6g
function ps4_JmXFo3Jh6g(SdkVersion,error:DWORD):DWORD;
const
 error_table:array[0..20] of DWORD=(
  $80552A05,
  $80552A08,
  $80552A0A,
  $80552A10,
  $80552A11,
  $80552A12,
  $80552A13,
  $80552A14,
  $80552A15,
  $80552A16,
  $80552A17,
  $80552A18,
  $80552A19,
  $80552A1A,
  $80552A1B,
  $80552A1C,
  $80552A1D,
  $80552A80,
  $80552A81,
  $80552A82,
  $80552A83
);
var
 id:Integer;
begin
  if (SdkVersion < $1500000) then
  begin
    if (Integer(error) < -$7d1fefff) then
    begin
      case (error) of
       $82e00001:
        id := 10;
       $82e00002,
       $82e00003,
       $82e00004,
       $82e00005,
       $82e00006,
       $82e00007,
       $82e00008,
       $82e00009,
       $82e0000a,
       $82e0000b,
       $82e0000c,
       $82e0000d,
       $82e0000e,
       $82e0000f,
       $82e00010,
       $82e00011,
       $82e00013,
       $82e00015,
       $82e00016,
       $82e00017,
       $82e00018,
       $82e00019,
       $82e0001a:
        Exit(error);
       $82e00012:
        id := 16;
       $82e00014:
        id := 3;
       $82e0001b:
        id := 11;
       $82e0001c:
        id := 4;
       $82e0001d:
        id := 5;
      else
        if (error = $82e00064) then
        begin
          id := 12;
        end else
        begin
          if (error <> $82e00067) then
          begin
            Exit(error);
          end;
          id := 13;
        end;
      end;
    end else
    if (Integer(error) < -$7d1efe6f) then
    begin
      case (error) of
       $82e01001:
        id := 6;
       $82e01002:
        id := 7;
       $82e01003:
        id := 8;
       $82e01004:
        id := 9;
       $82e01038:
        id := 2;
       $82e01039:
        id := 0;
       $82e01042:
        id := 14;
       $82e01045:
        id := 15;
       $82e0104d:
        id := 1;
       else
        Exit(error);
      end;
    end else
    if (Integer(error) < -$7d1efe0c) then
    begin
      if (error = $82e10191) then
      begin
        id := 19;
      end else
      begin
        if (error <> $82e1019a) then
        begin
          Exit(error);
        end;
        id := 17;
      end;
    end else
    if (error = $82e101f4) then
    begin
      id := 20;
    end else
    begin
      if (error <> $82e101f7) then
      begin
        Exit(error);
      end;
      id := 18;
    end;
    error:=error_table[id];
  end;

  Exit(error);
end;

//

function Load_libSceNpManager(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpManager');

 lib:=Result^.add_lib('libSceNpManager');
 lib.set_proc($036090DE4812A294,@ps4_sceNpSetContentRestriction);
 lib.set_proc($6BC47DFFBE6EE223,@ps4_sceNpGetAccountId);
 lib.set_proc($ADB9276948E9A96A,@ps4_sceNpGetAccountIdA);
 lib.set_proc($56061CCCF181E6CB,@ps4_sceNpGetUserIdByAccountId);
 lib.set_proc($1A1CFD8960D4B42E,@ps4_sceNpGetAccountCountry);
 lib.set_proc($253FADD346B74F10,@ps4_sceNpGetAccountCountryA);
 lib.set_proc($A7FA3BE029E83736,@ps4_sceNpGetNpId);
 lib.set_proc($5C39DC5D02095129,@ps4_sceNpGetOnlineId);
 lib.set_proc($7901FB9D63DC0207,@ps4_sceNpGetState);
 lib.set_proc($20F6F585DD700067,@ps4_sceNpGetGamePresenceStatus);
 lib.set_proc($A0F3BD538D98A602,@ps4_sceNpGetGamePresenceStatusA);
 lib.set_proc($7BF66E846128782E,@ps4_sceNpGetNpReachabilityState);
 lib.set_proc($39A777AEF63F3494,@ps4_sceNpHasSignedUp);
 lib.set_proc($11CEB7CB9F65F6DC,@ps4_sceNpSetNpTitleId);
 lib.set_proc($DD997C05E3D387D6,@ps4_sceNpCheckCallback);
 lib.set_proc($55F45298F9A3F10F,@ps4_sceNpRegisterStateCallback);
 lib.set_proc($A9025F3BC1C089A6,@ps4_sceNpRegisterStateCallbackA);
 lib.set_proc($9A38D35E1F8D1D66,@ps4_sceNpUnregisterStateCallback);
 lib.set_proc($B8526968A341023E,@ps4_sceNpRegisterGamePresenceCallback);
 lib.set_proc($2ACC312F19387356,@ps4_sceNpRegisterGamePresenceCallbackA);
 lib.set_proc($1889880A787E6E80,@ps4_sceNpRegisterPlusEventCallback);
 lib.set_proc($C558AA25D0E02A5D,@ps4_sceNpUnregisterPlusEventCallback);
 lib.set_proc($870E4A36A0007A5B,@ps4_sceNpRegisterNpReachabilityStateCallback);
 lib.set_proc($71120B004BE7FBD3,@ps4_sceNpUnregisterNpReachabilityStateCallback);
 lib.set_proc($1A92D00CD28809A7,@ps4_sceNpCreateRequest);
 lib.set_proc($7A2A8C0ADF54B212,@ps4_sceNpCreateAsyncRequest);
 lib.set_proc($4BB4139FBD8FAC3C,@ps4_sceNpDeleteRequest);
 lib.set_proc($3B32AF4EF8376585,@ps4_sceNpAbortRequest);
 lib.set_proc($DABB059A519695E4,@ps4_sceNpCheckNpAvailability);
 lib.set_proc($F19D897391AF1832,@ps4_sceNpCheckNpAvailabilityA);
 lib.set_proc($29F199836CBBDE83,@ps4_sceNpCheckNpReachability);
 lib.set_proc($8A5C0B338CCE9AEE,@ps4_sceNpGetParentalControlInfo);
 lib.set_proc($9BD2F73BACACB7F5,@ps4_sceNpGetParentalControlInfoA);
 lib.set_proc($AFA33260992BCB3F,@ps4_sceNpCheckPlus);
 lib.set_proc($19AC6BA7711663F3,@ps4_sceNpNotifyPlusFeature);
 lib.set_proc($BAA70F24B58BD3C3,@ps4_sceNpPollAsync);
 lib.set_proc($337C055DB610B400,@ps4_sceNpUnregisterStateCallbackA);
 lib.set_proc($F150537917F56702,@ps4_sceNpGetAccountDateOfBirth);
 lib.set_proc($AB733B5F304A0B7B,@ps4_sceNpGetAccountDateOfBirthA);
 lib.set_proc($299D4C8FDC841987,@ps4_sceNpGetAccountLanguage);
 lib.set_proc($4CF31B808C6FA20D,@ps4_sceNpGetAccountLanguageA);

 lib.set_proc($1858555294666C71,@ps4_sceNpInGameMessageInitialize);
 lib.set_proc($6CC1B77159949AE9,@ps4_SceNpInGameMessageTerminate);
 lib.set_proc($B385046B988125D7,@ps4_sceNpInGameMessageCreateHandle);
 lib.set_proc($F9A9EE4B1D9ABC74,@ps4_sceNpInGameMessageDeleteHandle);
 lib.set_proc($2242FAD85329229A,@ps4_sceNpInGameMessagePrepareA);
 lib.set_proc($3D00C5C5C9EAC6DC,@ps4_sceNpInGameMessageSendDataA);

 lib.set_proc($C5993E41C8AFAC51,@ps4_sceNpManagerIntCreateRequest);
 lib.set_proc($1E7782F92A5E2F07,@ps4_sceNpManagerIntDeleteRequest);
 lib.set_proc($3D9873FAF8E9D823,@ps4_sceNpManagerIntSetTimeout);
 lib.set_proc($9B826253C9363F22,@ps4_sceNpManagerIntCheckNpAvailability);
 lib.set_proc($F89997168DC987A8,@ps4_JmXFo3Jh6g);

 lib:=Result^.add_lib('libSceNpManagerForToolkit');
 lib.set_proc($D1CEC76D744A52DE,@ps4_sceNpRegisterStateCallbackForToolkit);
 lib.set_proc($608BEAAAF2728C47,@ps4_sceNpUnregisterStateCallbackForToolkit);
 lib.set_proc($2442C77F8C4FB9FA,@ps4_sceNpCheckCallbackForLib);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpManager.prx',@Load_libSceNpManager);

end.

