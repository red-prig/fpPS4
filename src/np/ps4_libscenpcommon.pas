unit ps4_libSceNpCommon;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam,
 subr_dynlib,
 np_error,
 ps4_libSceUserService;

Const
 SCE_NP_TITLE_ID_LEN=12;

type
 pSceNpTitleId=^SceNpTitleId;
 SceNpTitleId=packed record
  id     :array[0..SCE_NP_TITLE_ID_LEN] of Char;
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
  id     :array[0..SCE_NP_CLIENT_ID_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

const
 SCE_NP_CLIENT_SECRET_MAX_LEN=256;

type
 pSceNpClientSecret=^SceNpClientSecret;
 SceNpClientSecret=packed record
  secret :array[0..SCE_NP_CLIENT_SECRET_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

const
 SCE_NP_DEFAULT_SERVICE_LABEL=$00000000;
 SCE_NP_INVALID_SERVICE_LABEL=$FFFFFFFF;

 SCE_NP_AUTHORIZATION_CODE_MAX_LEN=128;

type
 pSceNpAuthorizationCode=^SceNpAuthorizationCode;
 SceNpAuthorizationCode=packed record
  code   :array[0..SCE_NP_AUTHORIZATION_CODE_MAX_LEN] of Char;
  padding:array[0..6] of Byte;
 end;

const
 SCE_NP_ID_TOKEN_MAX_LEN=4096;

type
 pSceNpIdToken=^SceNpIdToken;
 SceNpIdToken=packed record
  token  :array[0..SCE_NP_ID_TOKEN_MAX_LEN] of Char;
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
  data   :array[0..SCE_NP_UNIFIED_ENTITLEMENT_LABEL_SIZE] of Char;
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
  platform :SceNpPlatformType;
  padding  :array[0..3] of Byte;
 end;

const
 SCE_NP_LANGUAGE_CODE_MAX_LEN=5;

type
 pSceNpLanguageCode=^SceNpLanguageCode;
 SceNpLanguageCode=packed record
  code   :array[0..SCE_NP_LANGUAGE_CODE_MAX_LEN] of AnsiChar;
  padding:array[0..9] of Byte;
 end;

Const
 SCE_NP_COUNTRY_CODE_LENGTH=2;

type
 // Np country code (ISO 3166-1 two-letter system)
 pSceNpCountryCode=^SceNpCountryCode;
 SceNpCountryCode=packed record
  data   :array[0..SCE_NP_COUNTRY_CODE_LENGTH-1] of AnsiChar;
  term   :AnsiChar;
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
  age        :Shortint;
  padding    :array[0..2] of Byte;
 end;

const
 SCE_NP_NO_AGE_RESTRICTION=0;

type
 pSceNpContentRestriction=^SceNpContentRestriction;
 SceNpContentRestriction=packed record
  size                 :QWORD;
  defaultAgeRestriction:Byte;
  padding              :array[0..2] of Byte;
  ageRestrictionCount  :Integer;
  ageRestriction       :SceNpAgeRestriction;
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
  size    :QWORD;
  userId  :SceUserServiceUserId;
  padding :array[0..3] of Byte;
  features:QWORD;
  reserved:array[0..31] of Byte;
 end;

 pSceNpCheckPlusResult=^SceNpCheckPlusResult;
 SceNpCheckPlusResult=packed record
  authorized:Boolean;
  reserved  :array[0..31] of Byte;
 end;

 SceNpPlusEventType=Integer;

const
 SCE_NP_PLUS_EVENT_RECHECK_NEEDED=1;

type
 pSceNpNotifyPlusFeatureParameter=^SceNpNotifyPlusFeatureParameter;
 SceNpNotifyPlusFeatureParameter=packed record
  size    :QWORD;
  userId  :SceUserServiceUserId;
  padding :Integer;
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
  size           :qword;
  cpuAffinityMask:qword; //SceKernelCpumask
  threadPriority :Integer;
  padding        :Integer;
 end;

const
 SCE_NP_SESSION_ID_MAX_SIZE   =45;
 SCE_NP_INVITATION_ID_MAX_SIZE=60;

type
 pSceNpSessionId=^SceNpSessionId;
 SceNpSessionId=packed record
  data   :array[0..SCE_NP_SESSION_ID_MAX_SIZE-1] of Char;
  term   :Char;
  padding:array[0..1] of Char;
 end;

 pSceNpInvitationId=^SceNpInvitationId;
 SceNpInvitationId=packed record
  data   :array[0..SCE_NP_INVITATION_ID_MAX_SIZE-1] of Char;
  term   :Char;
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
  len   :qword;
  unknow:qword;
  ptr   :Pointer;
 end;

var
 ps4_sceKernelMapNamedFlexibleMemory:function(
  virtualAddrDest:PPointer;
  length:QWORD;
  prots,flags:Integer;
  name:PChar):Integer;

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
 if (len and PAGE_MASK)<>0 then
 begin
  pad_len:=PAGE_SIZE-(len and PAGE_MASK);
 end;
 mem_out^.len:=pad_len+len;

 Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

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

 Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result>-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

const
 SCE_PTHREAD_MUTEX_RECURSIVE = 2; // Recursive mutex

type
 PScePthreadMutex=Pointer;
 p_pthread_mutex_attr=^pthread_mutex_attr;
 pthread_mutex_attr=Pointer;

var
 ps4_scePthreadMutexattrInit   :function(pAttr:p_pthread_mutex_attr):Integer;
 ps4_scePthreadMutexattrDestroy:function(pAttr:p_pthread_mutex_attr):Integer;
 ps4_scePthreadMutexattrSettype:function(pAttr:p_pthread_mutex_attr;t:Integer):Integer;

 ps4_scePthreadMutexInit       :function(pMutex:PScePthreadMutex;pAttr:p_pthread_mutex_attr;str:PChar):Integer;
 ps4_scePthreadMutexLock       :function(pMutex:PScePthreadMutex):Integer;
 ps4_scePthreadMutexTrylock    :function(pMutex:PScePthreadMutex):Integer;
 ps4_scePthreadMutexUnlock     :function(pMutex:PScePthreadMutex):Integer;
 ps4_scePthreadMutexDestroy    :function(pMutex:PScePthreadMutex):Integer;

function ps4_sceNpMutexInit(mutex:PScePthreadMutex;name:PChar;isRecursive:Boolean):Integer;
var
 ga:TGUEST_STACK;
 attr:p_pthread_mutex_attr;
begin
 ga:=prolog;

 attr:=ga.alloca(SizeOf(Pointer));
 attr^:=nil;

 Result:=ps4_scePthreadMutexattrInit(attr);

 if (Result=0) then
 begin
  if isRecursive then
  begin
   Result:=ps4_scePthreadMutexattrSettype(attr,SCE_PTHREAD_MUTEX_RECURSIVE);
  end;
  if (Result=0) then
  begin
   Result:=ps4_scePthreadMutexInit(mutex,attr,name);
  end;
  ps4_scePthreadMutexattrDestroy(attr);
 end;

 ga.epilog;
end;

function ps4_sceNpMutexLock(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexLock(mutex);
 Result:=(Result shr $1F) and Result;
end;

function ps4_sceNpMutexUnlock(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexUnlock(mutex);
 Result:=(Result shr $1F) and Result;
end;

function ps4_sceNpMutexTryLock(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexTryLock(mutex);
 if (Result<>Integer($80020010)) then
 begin
  Result:=(Result shr $1F) and Result;
 end else
 begin
  Result:=Integer($8055800f);
 end;
 Result:=(Result shr $1F) and Result;
end;

function ps4_sceNpMutexDestroy(mutex:PScePthreadMutex):Integer;
begin
 Result:=ps4_scePthreadMutexDestroy(mutex);
end;

type
 pSceLibcMspace=Pointer;

 pSceLibcMallocManagedSize=^SceLibcMallocManagedSize;
 SceLibcMallocManagedSize=packed record
  size             :word;  //1
  version          :word;  //40
  reserved1        :dword;
  maxSystemSize    :QWORD;
  currentSystemSize:QWORD;
  maxInuseSize     :QWORD;
  currentInuseSize :QWORD;
 end;

var
 ps4_sceLibcMspaceCreate     :function(name:PChar;base:Pointer;capacity:size_t;flag:Integer):pSceLibcMspace;
 ps4_sceLibcMspaceDestroy    :function(msp:pSceLibcMspace):Integer;
 ps4_sceLibcMspaceMalloc     :function(msp:pSceLibcMspace;size:size_t):Pointer;
 ps4_sceLibcMspaceMallocStats:function(msp:pSceLibcMspace;mmsize:pSceLibcMallocManagedSize):Integer;

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

//

type
 SceKernelEventFlag =Pointer;
 pSceKernelEventFlag=^SceKernelEventFlag;
 pSceKernelEventFlagOptParam=Pointer;

var
 ps4_sceKernelCreateEventFlag:function(ef:pSceKernelEventFlag;
                                       pName:PChar;
                                       attr:DWORD;
                                       initPattern:QWORD;
                                       pOptParam:pSceKernelEventFlagOptParam
                                      ):Integer;

 ps4_sceKernelCloseEventFlag :function(ef:SceKernelEventFlag):Integer;
 ps4_sceKernelDeleteEventFlag:function(ef:SceKernelEventFlag):Integer;

function ps4_sceNpCreateEventFlag(ef:pSceKernelEventFlag;
                                  pName:PChar;
                                  attr:DWORD;
                                  initPattern:QWORD
                                 ):Integer;
begin
 Result:=ps4_sceKernelCreateEventFlag(ef,pName,attr,initPattern,nil);
 Result:=(Result shr $1F) and Result; // Looks like bool, but True when Result<0
end;

function ps4_sceNpCloseEventFlag(ef:SceKernelEventFlag):Integer;
begin
 Result:=ps4_sceKernelCloseEventFlag(ef);
 Result:=(Result shr $1F) and Result;
end;

function ps4_sceNpDeleteEventFlag(ef:SceKernelEventFlag):Integer;
begin
 Result:=ps4_sceKernelDeleteEventFlag(ef);
 Result:=(Result shr $1F) and Result;
end;

//

function ExecuteGuest_mallocFunc(addr:Pointer;size:size_t;userdata:Pointer):Pointer; external name 'ExecuteGuest';

//void * sce::np::Object::operator_new(size_t size,SceNpAllocator *mem)
function ps4__ZN3sce2np6ObjectnwEmR14SceNpAllocator(size:size_t;mem:pSceNpAllocator):Pointer;
var
 npObj:PSceNpObject;
begin
 npObj:=ExecuteGuest_mallocFunc(mem^.mallocFunc,size+$10,mem^.userdata);
 if (npObj<>nil) then
 begin
  npObj^.mem:=mem;
  Result:=@npObj^.entry;
 end else
 begin
  Result:=nil;
 end;
end;

//

type
 p_obj_cbs=^t_obj_cbs;
 t_obj_cbs=packed record
  clear:Pointer;
  free :Pointer;
 end;

 p_EventFlag=^t_EventFlag;
 t_EventFlag=packed record
  cbs   :p_obj_cbs;
  evf   :Pointer;
  evtype:Integer;
 end;

 p_Mutex=^t_Mutex;
 t_Mutex=packed record
  cbs   :p_obj_cbs;
  mutex :Pointer;
  init  :Byte;
 end;

var
 global_evf_cbs  :p_obj_cbs; init_evf_cbs  :t_obj_cbs; //ps4__ZN3sce2np9EventFlagD2Ev/ps4__ZN3sce2np9EventFlagD0Ev
 global_mutex_cbs:p_obj_cbs; init_mutex_cbs:t_obj_cbs;

//sce::np::EventFlag::~EventFlag(EventFlag *this)
procedure ps4__ZN3sce2np9EventFlagD2Ev(this:p_EventFlag);
begin
 global_evf_cbs^:=init_evf_cbs;
 //
 this^.cbs:=global_evf_cbs;
 if (this^.evtype=2) then
 begin
  ps4_sceNpCloseEventFlag(this^.evf);
 end else
 if (this^.evtype=1) then
 begin
  ps4_sceNpDeleteEventFlag(this^.evf);
 end;
end;

//sce::np::EventFlag::~EventFlag(EventFlag *this,SceNpAllocator *allocator)
procedure ps4__ZN3sce2np9EventFlagD0Ev(this:p_EventFlag;allocator:Pointer);
begin
 global_evf_cbs^:=init_evf_cbs;
 //
 this^.cbs   :=global_evf_cbs;
 if (this^.evtype=2) then
 begin
  ps4_sceNpCloseEventFlag(this^.evf);
 end else
 if (this^.evtype=1) then
 begin
  ps4_sceNpDeleteEventFlag(this^.evf);
 end;
 Assert(false,' Object::operator.delete');
 //Object::operator.delete((Object *)this,allocator);
end;

//sce::np::EventFlag::EventFlag(EventFlag *this)
procedure ps4__ZN3sce2np9EventFlagC1Ev(this:p_EventFlag);
begin
 global_evf_cbs^:=init_evf_cbs;
 //
 this^.cbs   :=global_evf_cbs;
 this^.evf   :=nil;
 this^.evtype:=0;
end;

//sce::np::EventFlag::Create(EventFlag *this,char *name,uint attr)
function ps4__ZN3sce2np9EventFlag6CreateEPKcj(this:p_EventFlag;name:PChar;attr:DWORD):Integer;
begin
 Result:=Integer($80559e03);
 if (this^.evtype=0) then
 begin
  Result:=ps4_sceNpCreateEventFlag(@this^.evf,name,attr,0);
  if (Result > -1) then
  begin
   this^.evtype:=1;
  end;
 end;
end;

//

//sce::np::Mutex::Mutex(Mutex *this)
procedure ps4__ZN3sce2np5MutexC1Ev(this:p_Mutex);
begin
 global_mutex_cbs^:=init_mutex_cbs;
 //
 this^.cbs  :=global_mutex_cbs;
 this^.mutex:=nil;
 this^.init :=0;
end;

//sce::np::Mutex::Init(Mutex *this,char *name,uint flags)
function ps4__ZN3sce2np5Mutex4InitEPKcj(this:p_Mutex;name:PChar;flags:DWORD):Integer;
begin
 Result:=Integer($80559e03);
 if (this^.init=0) then
 begin
  Result:=ps4_sceNpMutexInit(@this^.mutex,name,(flags and 1)<>0);
  if (Result > -1) then
  begin
   Result:=0;
   this^.init:=1;
  end;
 end;
end;

function Load_libSceNpCommon(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
 module:TMODULE;
begin
 Result:=obj_new_int('libSceNpCommon');

 lib:=Result^.add_lib('libSceNpCommon');
 lib.set_proc($8BC5265D34AAECDE,@ps4_sceNpCmpNpId);
 lib.set_proc($763F8EE5A0F66B44,@ps4_sceNpCmpOnlineId);
 lib.set_proc($80C958E9E7B0AFF7,@ps4_sceNpAllocateKernelMemoryWithAlignment);
 lib.set_proc($3163CE92ACD8B2CD,@ps4_sceNpAllocateKernelMemoryNoAlignment);
 lib.set_proc($B84C1A83FD1864F7,@ps4_sceNpMutexInit);
 lib.set_proc($AFD05EB7EB3A7CA7,@ps4_sceNpMutexLock);
 lib.set_proc($A19C9BF64B6E0A90,@ps4_sceNpMutexUnlock);
 lib.set_proc($0EEB259A8A90FA79,@ps4_sceNpMutexTryLock);
 lib.set_proc($950D7506930CE0B5,@ps4_sceNpMutexDestroy);
 // These sceNpLwMutexXxx have the same interface & functionally as sceNpMutexXxx
 lib.set_proc($D4289723F33210AB,@ps4_sceNpMutexInit);    // sceNpLwMutexInit
 lib.set_proc($D7C8FEAA4E9D4709,@ps4_sceNpMutexLock);    // sceNpLwMutexLock
 lib.set_proc($0901B6A32C75FE73,@ps4_sceNpMutexUnlock);  // sceNpLwMutexUnlock
 lib.set_proc($869D24560BB9171C,@ps4_sceNpMutexTryLock); // sceNpLwMutexTryLock
 lib.set_proc($E33C5EBE082D62B4,@ps4_sceNpMutexDestroy); // sceNpLwMutexDestroy
 //
 lib.set_proc($07EC86217D7E0532,@ps4_sceNpHeapInit);
 lib.set_proc($9305B9A9D75FF8BA,@ps4__sceNpHeapMalloc);
 lib.set_proc($DA3747A0FA52F96D,@ps4_sceNpHeapGetStat);
 lib.set_proc($C15767EFC1CA737D,@ps4_sceNpHeapDestroy);
 //
 lib.set_proc($EA3156A407EA01C7,@ps4_sceNpCreateEventFlag);
 lib.set_proc($FA79A7F99D27583A,@ps4_sceNpCloseEventFlag);
 lib.set_proc($B239C87850AE4C3D,@ps4_sceNpDeleteEventFlag);
 //
 lib.set_proc($D2CC8D921240355C,@ps4__ZN3sce2np6ObjectnwEmR14SceNpAllocator);
 //
 lib.set_proc($70C3A0904D8CD9EF,@ps4__ZN3sce2np9EventFlagC1Ev);
 lib.set_proc($6A6162FC0BF5F615,@ps4__ZN3sce2np9EventFlag6CreateEPKcj);
 //
 lib.set_proc($3B502F950537DE92,@ps4__ZN3sce2np5MutexC1Ev);
 lib.set_proc($69334E97D101E15E,@ps4__ZN3sce2np5Mutex4InitEPKcj);
 //
 lib.add_data(@global_evf_cbs    ,SizeOf(t_obj_cbs));
 lib.add_func(@init_evf_cbs.clear,@ps4__ZN3sce2np9EventFlagD2Ev);
 lib.add_func(@init_evf_cbs.free ,@ps4__ZN3sce2np9EventFlagD0Ev);
 //
 lib.add_data(@global_mutex_cbs  ,SizeOf(t_obj_cbs));
 init_mutex_cbs.clear:=Pointer(1);
 init_mutex_cbs.free :=Pointer(2);
 //

 //import

 module:=Result^.add_mod('libkernel',1);
 lib:=module.add_lib('libkernel');

 lib.set_proc($0691686E8509A195,@ps4_sceKernelCreateEventFlag);
 lib.set_proc($B3DFD16B1BA4BB34,@ps4_sceKernelCloseEventFlag);
 lib.set_proc($F26AA5F4E7109DDE,@ps4_sceKernelDeleteEventFlag);

 lib.set_proc($98BF0D0C7F3A8902,@ps4_sceKernelMapNamedFlexibleMemory);

 lib.set_proc($17C6D41F0006DBCE,@ps4_scePthreadMutexattrInit);
 lib.set_proc($B2658492D8B2C86D,@ps4_scePthreadMutexattrDestroy);
 lib.set_proc($88CA7C42913E5CEE,@ps4_scePthreadMutexattrSettype);

 lib.set_proc($726A3544862F6BDA,@ps4_scePthreadMutexInit);
 lib.set_proc($D8E7F47FEDE68611,@ps4_scePthreadMutexDestroy);
 lib.set_proc($F542B5BCB6507EDE,@ps4_scePthreadMutexLock);
 lib.set_proc($B67DD5943D211BAD,@ps4_scePthreadMutexUnlock);
 lib.set_proc($BA9A15AF330715E1,@ps4_scePthreadMutexTrylock);

 module:=Result^.add_mod('libSceLibcInternal',1);
 lib:=module.add_lib('libSceLibcInternal');

 lib.set_proc($FE19F5B5C547AB94,@ps4_sceLibcMspaceCreate);
 lib.set_proc($5BA4A25528820ED2,@ps4_sceLibcMspaceDestroy);
 lib.set_proc($3898E6FD03881E52,@ps4_sceLibcMspaceMalloc);
 lib.set_proc($99F1DD25322F86EA,@ps4_sceLibcMspaceMallocStats);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpCommon.prx',@Load_libSceNpCommon);

end.

