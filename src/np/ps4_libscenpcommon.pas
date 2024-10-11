unit ps4_libSceNpCommon;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib,
 np_error;

type
 SceUserServiceUserId=Integer;

Const
 SCE_NP_TITLE_ID_LEN=12;

type
 pSceNpTitleId=^SceNpTitleId;
 SceNpTitleId=packed record
  id:array[0..SCE_NP_TITLE_ID_LEN] of Char;
  padding:array[0..2] of Byte;
 end;

const
 SCE_NP_TITLE_SECRET_SIZE=128;

type
 pSceNpTitleSecret=^SceNpTitleSecret;
 SceNpTitleSecret=array[0..SCE_NP_TITLE_SECRET_SIZE-1] of Byte;

const
 SCE_NP_CLIENT_ID_MAX_LEN=128;

type
 pSceNpClientId=^SceNpClientId;
 SceNpClientId=packed record
  id:array[0..SCE_NP_CLIENT_ID_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

const
 SCE_NP_CLIENT_SECRET_MAX_LEN=256;

type
 pSceNpClientSecret=^SceNpClientSecret;
 SceNpClientSecret=packed record
  secret:array[0..SCE_NP_CLIENT_SECRET_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

const
 SCE_NP_DEFAULT_SERVICE_LABEL=$00000000;
 SCE_NP_INVALID_SERVICE_LABEL=$FFFFFFFF;

 SCE_NP_AUTHORIZATION_CODE_MAX_LEN=128;

type
 pSceNpAuthorizationCode=^SceNpAuthorizationCode;
 SceNpAuthorizationCode=packed record
  code:array[0..SCE_NP_AUTHORIZATION_CODE_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

const
 SCE_NP_ID_TOKEN_MAX_LEN=4096;

type
 pSceNpIdToken=^SceNpIdToken;
 SceNpIdToken=packed record
  token:array[0..SCE_NP_ID_TOKEN_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

 pSceNpCommunicationId=^SceNpCommunicationId;
 SceNpCommunicationId=packed record
  data :array[0..8] of Char;
  term :Char;
  num  :Byte;
  dummy:Char;
 end;

const
 SCE_NP_COMMUNICATION_PASSPHRASE_SIZE=128;

type
 pSceNpCommunicationPassphrase=^SceNpCommunicationPassphrase;
 SceNpCommunicationPassphrase=packed record
  data:array[0..SCE_NP_COMMUNICATION_PASSPHRASE_SIZE-1] of Byte;
 end;

const
 SCE_NP_PORT=3658;

 //SceNpState
 SCE_NP_STATE_UNKNOWN    =0;
 SCE_NP_STATE_SIGNED_OUT =1;
 SCE_NP_STATE_SIGNED_IN  =2;

type
 SceNpState=Integer;

const
 //SceNpPlatformType
 SCE_NP_PLATFORM_TYPE_NONE =0;
 SCE_NP_PLATFORM_TYPE_PS3  =1;
 SCE_NP_PLATFORM_TYPE_VITA =2;
 SCE_NP_PLATFORM_TYPE_PS4  =3;
 SCE_NP_PLATFORM_TYPE_ORBIS=3;

type
 SceNpPlatformType=Integer;

const
 //SceNpGamePresenceStatus
 SCE_NP_GAME_PRESENCE_STATUS_OFFLINE=0;
 SCE_NP_GAME_PRESENCE_STATUS_ONLINE =1;

 SCE_NP_UNIFIED_ENTITLEMENT_LABEL_SIZE=17;

type
 SceNpGamePresenceStatus=Integer;

 pSceNpUnifiedEntitlementLabel=^SceNpUnifiedEntitlementLabel;
 SceNpUnifiedEntitlementLabel=packed record
  data:array[0..SCE_NP_UNIFIED_ENTITLEMENT_LABEL_SIZE] of Char;
  padding:array[0..2] of Byte;
 end;

 pSceNpAccountId=^SceNpAccountId;
 SceNpAccountId=QWORD;

const
 SCE_NP_INVALID_ACCOUNT_ID=0;

type
 pSceNpPeerAddressA=^SceNpPeerAddressA;
 SceNpPeerAddressA=packed record
  accountId:SceNpAccountId;
  platform:SceNpPlatformType;
  padding:array[0..3] of Byte;
 end;

const
 SCE_NP_LANGUAGE_CODE_MAX_LEN=5;

type
 pSceNpLanguageCode=^SceNpLanguageCode;
 SceNpLanguageCode=packed record
  code:array[0..SCE_NP_LANGUAGE_CODE_MAX_LEN] of AnsiChar;
  padding:array[0..9] of Byte;
 end;

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

 pSceNpDate=^SceNpDate;
 SceNpDate=packed record
  year :Word;
  month:Byte;
  day  :Byte;
  _pad :DWord;
 end;

 SceNpAgeRestriction=packed record
  countryCode:SceNpCountryCode;
  age:Shortint;
  padding:array[0..2] of Byte;
 end;

const
 SCE_NP_NO_AGE_RESTRICTION=0;

type
 pSceNpContentRestriction=^SceNpContentRestriction;
 SceNpContentRestriction=packed record
  size:QWORD;
  defaultAgeRestriction:Byte;
  padding:array[0..2] of Byte;
  ageRestrictionCount:Integer;
  ageRestriction:SceNpAgeRestriction;
 end;

 pSceNpParentalControlInfo=^SceNpParentalControlInfo;
 SceNpParentalControlInfo=packed record
  contentRestriction:Boolean;
  chatRestriction   :Boolean;
  ugcRestriction    :Boolean;
 end;

const
//SceNpReachabilityState
 SCE_NP_REACHABILITY_STATE_UNAVAILABLE=0;
 SCE_NP_REACHABILITY_STATE_AVAILABLE  =1;
 SCE_NP_REACHABILITY_STATE_REACHABLE  =2;

type
 SceNpReachabilityState=Integer;

const
 SCE_NP_PLUS_FEATURE_REALTIME_MULTIPLAY=1;

type
 pSceNpCheckPlusParameter=^SceNpCheckPlusParameter;
 SceNpCheckPlusParameter=packed record
  size:QWORD;
  userId:SceUserServiceUserId;
  padding:array[0..3] of Byte;
  features:QWORD;
  reserved:array[0..31] of Byte;
 end;

 pSceNpCheckPlusResult=^SceNpCheckPlusResult;
 SceNpCheckPlusResult=packed record
  authorized:Boolean;
  reserved:array[0..31] of Byte;
 end;

 SceNpPlusEventType=Integer;

const
 SCE_NP_PLUS_EVENT_RECHECK_NEEDED=1;

type
 pSceNpNotifyPlusFeatureParameter=^SceNpNotifyPlusFeatureParameter;
 SceNpNotifyPlusFeatureParameter=packed record
  size:QWORD;
  userId:SceUserServiceUserId;
  padding:Integer;
  features:QWORD;
  reserved:array[0..31] of Byte;
 end;

const
 SCE_NP_MAX_REQUEST_NUM  =32;
 SCE_NP_TIMEOUT_NO_EFFECT=0;

 SCE_NP_POLL_ASYNC_RET_FINISHED=0;
 SCE_NP_POLL_ASYNC_RET_RUNNING =1;

type
 pSceNpCreateAsyncRequestParameter=^SceNpCreateAsyncRequestParameter;
 SceNpCreateAsyncRequestParameter=packed record
  size:qword;
  cpuAffinityMask:qword; //SceKernelCpumask
  threadPriority:Integer;
  padding:Integer;
 end;

const
 SCE_NP_SESSION_ID_MAX_SIZE   =45;
 SCE_NP_INVITATION_ID_MAX_SIZE=60;

type
 pSceNpSessionId=^SceNpSessionId;
 SceNpSessionId=packed record
  data:array[0..SCE_NP_SESSION_ID_MAX_SIZE-1] of Char;
  term:Char;
  padding:array[0..1] of Char;
 end;

 pSceNpInvitationId=^SceNpInvitationId;
 SceNpInvitationId=packed record
  data:array[0..SCE_NP_INVITATION_ID_MAX_SIZE-1] of Char;
  term:Char;
  padding:array[0..2] of Char;
 end;

const
 SCE_NP_SESSION_INVITATION_EVENT_FLAG_INVITATION=1;

 SCE_NP_ONLINEID_MIN_LENGTH=3;
 SCE_NP_ONLINEID_MAX_LENGTH=16;

type
 SceNpServiceLabel=DWORD;

 pSceNpOnlineId=^SceNpOnlineId;
 SceNpOnlineId=packed record
  data :array[0..SCE_NP_ONLINEID_MAX_LENGTH-1] of AnsiChar;
  term :AnsiChar;
  dummy:array[0..2] of AnsiChar;
 end;

 SceNpSessionInvitationEventFlag=Integer;

 pSceNpSessionInvitationEventParam=^SceNpSessionInvitationEventParam;
 SceNpSessionInvitationEventParam=packed record
  sessionId        :SceNpSessionId;
  invitationId     :SceNpInvitationId;
  flag             :SceNpSessionInvitationEventFlag;
  padding          :array[0..3] of Char;
  onlineId         :SceNpOnlineId;
  userId           :SceUserServiceUserId;
  referralOnlineId :SceNpOnlineId;
  referralAccountId:SceNpAccountId;
 end;

 SceNpGameCustomDataId=QWORD;

 pSceNpGameCustomDataEventParam=^SceNpGameCustomDataEventParam;
 SceNpGameCustomDataEventParam=packed record
  itemId  :SceNpGameCustomDataId;
  onlineId:SceNpOnlineId;
  userId  :SceUserServiceUserId;
 end;

const
 SCE_NP_ARCH_ERROR_UNKNOWN=-2141880310;

type
 PSceNpId=^SceNpId;
 SceNpId=packed record
  handle  :SceNpOnlineId;
  opt     :array[0..7] of Byte;
  reserved:array[0..7] of Byte;
 end;

 pSceNpHeap=^SceNpHeap;
 SceNpHeap=packed record
  mspace:Pointer;
 end;

type
 SceNpMallocFunc =function(size:size_t;userdata:Pointer):Pointer;
 SceNpReallocFunc=function(ptr:Pointer;size:size_t;userdata:Pointer):Pointer;
 SceNpFreeFunc   =procedure(ptr,userdata:Pointer);

 pSceNpAllocator=^SceNpAllocator;
 SceNpAllocator=packed record
  mallocFunc :SceNpMallocFunc;
  reallocFunc:SceNpReallocFunc;
  freeFunc   :SceNpFreeFunc;
  userdata   :Pointer;
 end;

 PSceNpObject=^SceNpObject;
 SceNpObject=packed record
  mem  :pSceNpAllocator; // 8
  _unk1:QWord;   // 16
  entry:Pointer; // 24
 end;

 pSceNpHeapStat=^SceNpHeapStat;
 SceNpHeapStat=packed record
  maxSystemSize   :QWORD;
  maxInuseSize    :QWORD;
  currentInuseSize:QWORD;
 end;

implementation

{
uses
 ps4_event_flag,
 ps4_mspace_internal,
 ps4_mutex,
 ps4_map_mm;
}

function ps4_sceNpCmpNpId(npid1,npid2:PSceNpId):Integer;
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

function ps4_sceNpCmpOnlineId(str1,str2:PChar):Integer;
begin
 if (str1=nil) or (str2=nil) then
  Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if CompareChar0(str1,str2,SCE_NP_ONLINEID_MAX_LENGTH)=0 then
  Result:=0
 else
  Result:=SCE_NP_UTIL_ERROR_NOT_MATCH;
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
          mem_out:pnp_mem):Integer;
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

 Assert(False);
 //Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result>-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

function ps4_sceNpAllocateKernelMemoryNoAlignment(
          len:qword;
          name:Pchar;
          ptr_out:PPointer;
          mem_out:pnp_mem):Integer;
begin
 if (mem_out=nil) then
 begin
  Exit(-$7faa7ffb); //NP-32268-1
 end;

 mem_out^.unknow:=0;
 mem_out^.len:=len;

 Assert(False);
 //Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result>-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

{
function ps4_sceNpMutexInit(mutex:PScePthreadMutex;name:PChar;isRecursive:Boolean):Integer;
var
 attr:pthread_mutex_attr;
begin
 Result:=ps4_scePthreadMutexattrInit(@attr);
 if Result=0 then
 begin
  if isRecursive then
   Result:=ps4_scePthreadMutexattrSettype(@attr,SCE_PTHREAD_MUTEX_RECURSIVE);
  if Result=0 then
   Result:=ps4_scePthreadMutexInit(mutex,@attr,name);
  ps4_scePthreadMutexattrDestroy(@attr);
 end;
end;

function ps4_sceNpMutexLock(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexLock(mutex);
end;

function ps4_sceNpMutexUnlock(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexUnlock(mutex);
end;

function ps4_sceNpMutexTryLock(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexTryLock(mutex);
end;

function ps4_sceNpMutexDestroy(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexDestroy(mutex);
end;

function ps4_sceNpHeapInit(heap:pSceNpHeap;base:Pointer;capacity:size_t;name:PChar):Integer;
var
 m:Pointer;
begin
 Result:=SCE_NP_ARCH_ERROR_UNKNOWN;
 if heap<>nil then
 begin
  m:=ps4_sceLibcMspaceCreate(name,base,capacity,0);
  if (m<>nil) then
  begin
   heap^.mspace:=m;
   Result:=0;
  end;
 end;
end;

function ps4__sceNpHeapMalloc(heap:pSceNpHeap;size:size_t):Pointer;
begin
 Result:=nil;
 if (heap^.mspace<>nil) then
 begin
  Result:=ps4_sceLibcMspaceMalloc(heap^.mspace,size);
 end;
end;

function ps4_sceNpHeapGetStat(heap:pSceNpHeap;stat:pSceNpHeapStat):Integer;
var
 data:SceLibcMallocManagedSize;
begin
 data:=Default(SceLibcMallocManagedSize);
 data.size   :=40;
 data.version:=1;

 Result:=ps4_sceLibcMspaceMallocStats(heap^.mspace,@data);

 if (Result=0) then
 begin
  stat^.maxSystemSize   :=data.maxSystemSize;
  stat^.maxInuseSize    :=data.maxInuseSize;
  stat^.currentInuseSize:=data.currentInuseSize;
 end;
end;

procedure ps4_sceNpHeapDestroy(heap:pSceNpHeap);
begin
 if (heap^.mspace<>nil) then
 begin
  ps4_sceLibcMspaceDestroy(heap^.mspace);
  heap^.mspace:=nil;
 end;
end;

function ps4_sceNpCreateEventFlag(ef:pSceKernelEventFlag;
                                  pName:PChar;
                                  attr:DWORD;
                                  initPattern:QWORD
                                  ):Integer;
begin
 Result:=ps4_sceKernelCreateEventFlag(ef,pName,attr,initPattern,nil);
 Result:=(Result shr $1F) and Result; // Looks like bool, but True when Result<0
end;
}

//void * sce::np::Object::operator_new(size_t size,SceNpAllocator *mem)
function ps4__ZN3sce2np6ObjectnwEmR14SceNpAllocator(size:size_t;mem:pSceNpAllocator):Pointer;
var
 npObj:PSceNpObject;
begin
 npObj:=mem^.mallocFunc(size+$10,mem^.userdata);
 if npObj<>nil then
 begin
  npObj^.mem:=mem;
  Result:=@npObj^.entry;
 end else
  Result:=nil;
end;

function Load_libSceNpCommon(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpCommon');

 lib:=Result^.add_lib('libSceNpCommon');
 lib.set_proc($8BC5265D34AAECDE,@ps4_sceNpCmpNpId);
 lib.set_proc($763F8EE5A0F66B44,@ps4_sceNpCmpOnlineId);
 lib.set_proc($80C958E9E7B0AFF7,@ps4_sceNpAllocateKernelMemoryWithAlignment);
 lib.set_proc($3163CE92ACD8B2CD,@ps4_sceNpAllocateKernelMemoryNoAlignment);
 //lib.set_proc($B84C1A83FD1864F7,@ps4_sceNpMutexInit);
 //lib.set_proc($AFD05EB7EB3A7CA7,@ps4_sceNpMutexLock);
 //lib.set_proc($A19C9BF64B6E0A90,@ps4_sceNpMutexUnlock);
 //lib.set_proc($0EEB259A8A90FA79,@ps4_sceNpMutexTryLock);
 //lib.set_proc($950D7506930CE0B5,@ps4_sceNpMutexDestroy);
 // These sceNpLwMutexXxx have the same interface & functionally as sceNpMutexXxx
 //lib.set_proc($D4289723F33210AB,@ps4_sceNpMutexInit);    // sceNpLwMutexInit
 //lib.set_proc($D7C8FEAA4E9D4709,@ps4_sceNpMutexLock);    // sceNpLwMutexLock
 //lib.set_proc($0901B6A32C75FE73,@ps4_sceNpMutexUnlock);  // sceNpLwMutexUnlock
 //lib.set_proc($869D24560BB9171C,@ps4_sceNpMutexTryLock); // sceNpLwMutexTryLock
 //lib.set_proc($E33C5EBE082D62B4,@ps4_sceNpMutexDestroy); // sceNpLwMutexDestroy
 //
 //lib.set_proc($07EC86217D7E0532,@ps4_sceNpHeapInit);
 //lib.set_proc($9305B9A9D75FF8BA,@ps4__sceNpHeapMalloc);
 //lib.set_proc($DA3747A0FA52F96D,@ps4_sceNpHeapGetStat);
 //lib.set_proc($C15767EFC1CA737D,@ps4_sceNpHeapDestroy);
 //lib.set_proc($EA3156A407EA01C7,@ps4_sceNpCreateEventFlag);
 lib.set_proc($D2CC8D921240355C,@ps4__ZN3sce2np6ObjectnwEmR14SceNpAllocator);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceNpCommon.prx',@Load_libSceNpCommon);

end.

