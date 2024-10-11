unit ps4_libSceNpCommon;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 np_error;

const
 SCE_NP_DEFAULT_SERVICE_LABEL=$00000000;
 SCE_NP_INVALID_SERVICE_LABEL=$FFFFFFFF;

 SCE_NP_ONLINEID_MIN_LENGTH=3;
 SCE_NP_ONLINEID_MAX_LENGTH=16;

 SCE_NP_COMMUNICATION_PASSPHRASE_SIZE=128;

 SCE_NP_ARCH_ERROR_UNKNOWN=-2141880310;

type
 SceNpServiceLabel=DWORD;
 
 pSceNpAccountId=^SceNpAccountId;
 SceNpAccountId=QWORD;

 SceNpPlatformType=Integer;

 pSceNpOnlineId=^SceNpOnlineId;
 SceNpOnlineId=packed record
  data :array[0..SCE_NP_ONLINEID_MAX_LENGTH-1] of AnsiChar;
  term :AnsiChar;
  dummy:array[0..2] of AnsiChar;
 end;

 PSceNpId=^SceNpId;
 SceNpId=packed record
  handle  :SceNpOnlineId;
  opt     :array[0..7] of Byte;
  reserved:array[0..7] of Byte;
 end;

 pSceNpCommunicationId=^SceNpCommunicationId;
 SceNpCommunicationId=packed record
  data :array[0..8] of Char;
  term :Char;
  num  :Byte;
  dummy:Char;
 end;

 pSceNpCommunicationPassphrase=^SceNpCommunicationPassphrase;
 SceNpCommunicationPassphrase=packed record
  data:array[0..SCE_NP_COMMUNICATION_PASSPHRASE_SIZE-1] of Byte;
 end;

 pSceNpHeap=^SceNpHeap;
 SceNpHeap=packed record
  mspace:Pointer;
 end;

type
 SceNpMallocFunc =function(size:size_t;userdata:Pointer):Pointer; SysV_ABI_CDecl;
 SceNpReallocFunc=function(ptr:Pointer;size:size_t;userdata:Pointer):Pointer; SysV_ABI_CDecl;
 SceNpFreeFunc   =procedure(ptr,userdata:Pointer); SysV_ABI_CDecl;

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

uses
 ps4_event_flag,
 ps4_mspace_internal,
 ps4_mutex,
 ps4_map_mm;

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

function ps4_sceNpCmpOnlineId(str1,str2:PChar):Integer; SysV_ABI_CDecl;
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

 if (ptr_out<>nil) and (Result>-1) then
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

 if (ptr_out<>nil) and (Result>-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

function ps4_sceNpMutexInit(mutex:PScePthreadMutex;name:PChar;isRecursive:Boolean):Integer; SysV_ABI_CDecl;
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

function ps4_sceNpMutexLock(mutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexLock(mutex);
end;

function ps4_sceNpMutexUnlock(mutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexUnlock(mutex);
end;

function ps4_sceNpMutexTryLock(mutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexTryLock(mutex);
end;

function ps4_sceNpMutexDestroy(mutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexDestroy(mutex);
end;

function ps4_sceNpHeapInit(heap:pSceNpHeap;base:Pointer;capacity:size_t;name:PChar):Integer; SysV_ABI_CDecl;
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

function ps4__sceNpHeapMalloc(heap:pSceNpHeap;size:size_t):Pointer; SysV_ABI_CDecl;
begin
 Result:=nil;
 if (heap^.mspace<>nil) then
 begin
  Result:=ps4_sceLibcMspaceMalloc(heap^.mspace,size);
 end;
end;

function ps4_sceNpHeapGetStat(heap:pSceNpHeap;stat:pSceNpHeapStat):Integer; SysV_ABI_CDecl;
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

procedure ps4_sceNpHeapDestroy(heap:pSceNpHeap); SysV_ABI_CDecl;
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
                                  ):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceKernelCreateEventFlag(ef,pName,attr,initPattern,nil);
 Result:=(Result shr $1F) and Result; // Looks like bool, but True when Result<0
end;

//void * sce::np::Object::operator_new(size_t size,SceNpAllocator *mem)
function ps4__ZN3sce2np6ObjectnwEmR14SceNpAllocator(size:size_t;mem:pSceNpAllocator):Pointer; SysV_ABI_CDecl;
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

function Load_libSceNpCommon(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpCommon');
 lib^.set_proc($8BC5265D34AAECDE,@ps4_sceNpCmpNpId);
 lib^.set_proc($763F8EE5A0F66B44,@ps4_sceNpCmpOnlineId);
 lib^.set_proc($80C958E9E7B0AFF7,@ps4_sceNpAllocateKernelMemoryWithAlignment);
 lib^.set_proc($3163CE92ACD8B2CD,@ps4_sceNpAllocateKernelMemoryNoAlignment);
 lib^.set_proc($B84C1A83FD1864F7,@ps4_sceNpMutexInit);
 lib^.set_proc($AFD05EB7EB3A7CA7,@ps4_sceNpMutexLock);
 lib^.set_proc($A19C9BF64B6E0A90,@ps4_sceNpMutexUnlock);
 lib^.set_proc($0EEB259A8A90FA79,@ps4_sceNpMutexTryLock);
 lib^.set_proc($950D7506930CE0B5,@ps4_sceNpMutexDestroy);
 // These sceNpLwMutexXxx have the same interface & functionally as sceNpMutexXxx
 lib^.set_proc($D4289723F33210AB,@ps4_sceNpMutexInit);    // sceNpLwMutexInit
 lib^.set_proc($D7C8FEAA4E9D4709,@ps4_sceNpMutexLock);    // sceNpLwMutexLock
 lib^.set_proc($0901B6A32C75FE73,@ps4_sceNpMutexUnlock);  // sceNpLwMutexUnlock
 lib^.set_proc($869D24560BB9171C,@ps4_sceNpMutexTryLock); // sceNpLwMutexTryLock
 lib^.set_proc($E33C5EBE082D62B4,@ps4_sceNpMutexDestroy); // sceNpLwMutexDestroy
 //
 lib^.set_proc($07EC86217D7E0532,@ps4_sceNpHeapInit);
 lib^.set_proc($9305B9A9D75FF8BA,@ps4__sceNpHeapMalloc);
 lib^.set_proc($DA3747A0FA52F96D,@ps4_sceNpHeapGetStat);
 lib^.set_proc($C15767EFC1CA737D,@ps4_sceNpHeapDestroy);
 lib^.set_proc($EA3156A407EA01C7,@ps4_sceNpCreateEventFlag);
 lib^.set_proc($D2CC8D921240355C,@ps4__ZN3sce2np6ObjectnwEmR14SceNpAllocator);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpCommon.prx' ,@Load_libSceNpCommon);

end.

