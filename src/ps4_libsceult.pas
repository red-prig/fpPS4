unit ps4_libSceUlt;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
 SysUtils,
 ps4_program,
 sys_pthread,
 sys_kernel,
 ps4_pthread,
 ps4_mutex,
 Generics.Collections;

implementation

uses
 ps4_libSceFiber;

{$I sce_errno.inc}

const
 SCE_ULT_ERROR_NULL          =$80810001;
 SCE_ULT_ERROR_ALIGNMENT     =$80810002;
 SCE_ULT_ERROR_RANGE         =$80810003;
 SCE_ULT_ERROR_INVALID       =$80810004;
 SCE_ULT_ERROR_PERMISSION    =$80810005;
 SCE_ULT_ERROR_STATE         =$80810006;
 SCE_ULT_ERROR_BUSY          =$80810007;
 SCE_ULT_ERROR_AGAIN         =$80810008;
 SCE_ULT_ERROR_NOT_INITIALIZE=$8081000A;
 SCE_ULT_MAX_NAME_LENGTH     =31;

 ULT_STATE_INIT        =0;
 ULT_STATE_RUN         =1;
 ULT_STATE_SUSPEND     =2;
 ULT_STATE_PREPARE_JOIN=3;
 ULT_STATE_DESTROYED   =4;
 ULT_STATE_WAIT        =5;

type
 SceUltUlthreadEntry=function(arg:QWord):Integer; SysV_ABI_CDecl;

 PWorkerThread                          =^TWorkerThread;
 PSceUltUlthreadRuntime                 =^SceUltUlthreadRuntime;
 PSceUltUlthreadRuntimeOptParam         =^SceUltUlthreadRuntimeOptParam;
 PSceUltWaitingQueueResourcePoolOptParam=^SceUltWaitingQueueResourcePoolOptParam;
 PSceUltUlthreadOptParam                =^SceUltUlthreadOptParam;
 PSceUltWaitingQueueResourcePool        =^SceUltWaitingQueueResourcePool;
 PSceUltUlthread                        =^SceUltUlthread;
 PSceUltQueueDataResourcePoolOptParam   =^SceUltQueueDataResourcePoolOptParam;
 PSceUltQueueDataResourcePool           =^SceUltQueueDataResourcePool;
 PSceUltQueueOptParam                   =^SceUltQueueOptParam;
 PSceUltQueue                           =^SceUltQueue;
 PSceUltMutexOptParam                   =^SceUltMutexOptParam;
 PSceUltMutex                           =^SceUltMutex;

 SceUltUlthreadRuntimeOptParam=packed record
  oneShotThreadStackSize     :QWord;    // 8
  workerThreadCpuAffinityMask:QWord;    // 16
  workerThreadPriority       :Integer;  // 20
  inheritSched               :Integer;  // 24
  _unknown                   :array[0..127-24] of Byte; // 128
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltUlthread=packed record
  fiber       :PSceFiber;           // 8
  entry       :SceUltUlthreadEntry; // 16
  arg         :QWord;               // 24
  state       :QWord;               // 32
  runtime     :PSceUltUlthreadRuntime; // 40
  returnStatus:QWord;                  // 48
  _unknown:array[0..255-48] of Byte; // 256
  procedure init(const aRuntime:PSceUltUlthreadRuntime;
                 const aName:PChar;
                 const aEntry:SceUltUlthreadEntry;
                 const aArg:QWord;
                 const aContext:Pointer;
                 const aSizeContext:QWord);
  function  getState:Integer;
  procedure destroy;
 end;

 TUlThreadList=specialize TList<PSceUltUlthread>;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltUlthreadOptParam=packed record
  attr    :DWord;                 // 4
  _unknown:array[0..127-4] of Byte; // 128
 end;

 TWorkerThread=record
  thread      :pthread;
  runtime     :PSceUltUlthreadRuntime;
  ulThreadList:TUlThreadList;
  current     :QWord;         // Current ulThread index
  indx        :QWord;         // This worker thread index in runtime
  wakeUpEvent :PRTLEvent;
 end;

 TWorkerThreadList=specialize TList<PWorkerThread>;

 // Our own invention of SceUltUlthreadRuntime. Not compatible with original one
 // Original size is 4096 bytes, but we ignore this info
 SceUltUlthreadRuntime=packed record
  param           :SceUltUlthreadRuntimeOptParam;
  name            :array[0..SCE_ULT_MAX_NAME_LENGTH] of Char;
  maxUlThread     :Integer;
  maxWorkerThread :Integer;
  workerThreadList:TWorkerThreadList;
  ulThreadCount   :QWord;
  balancer        :QWord;
  cs              :TRTLCriticalSection;
  procedure enter;
  procedure leave;
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltWaitingQueueResourcePoolOptParam=packed record
  _unknown:array[0..127] of Byte; // 128
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltWaitingQueueResourcePool=packed record
  param         :SceUltWaitingQueueResourcePoolOptParam; // 128
  name          :array[0..SCE_ULT_MAX_NAME_LENGTH] of Char; // 160
  numThreads    :DWord;                // 168
  numSyncObjects:DWord;                // 176
  workArea      :Pointer;              // 184
  _unknown:array[0..255-184] of Byte;  // 256
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltQueueDataResourcePoolOptParam=packed record
  _unknown:array[0..127] of Byte; // 128
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltQueueDataResourcePool=packed record
  param          :SceUltQueueDataResourcePoolOptParam; // 128
  numData        :DWord;                               // 132
  numQueueObjects:DWord;                               // 136
  dataSize       :QWord;                               // 144
  waitingQueue   :PSceUltWaitingQueueResourcePool;     // 152
  workArea       :Pointer;                             // 160
  name           :array[0..SCE_ULT_MAX_NAME_LENGTH] of Char; // 192
  cs             :TRTLCriticalSection;                 // 200
  queuePtr       :Pointer;                             // 208
  pushEvent      :PRTLEvent;                           // 216
  popEvent       :PRTLEvent;                           // 224
  _unknown:array[0..511-224] of Byte; // 512
  procedure enter;
  procedure leave;
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltQueueOptParam=packed record
  _unknown:array[0..127] of Byte; // 128
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltQueue=packed record
  param       :SceUltQueueOptParam; // 128
  name        :array[0..SCE_ULT_MAX_NAME_LENGTH] of Char; // 160
  queueData   :PSceUltQueueDataResourcePool;    // 168
  waitingQueue:PSceUltWaitingQueueResourcePool; // 176
  _unknown:array[0..511-152] of Byte; // 512
  function push(const aData:Pointer):Integer;
  function pop(const aData:Pointer):Integer;
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltMutexOptParam=packed record
  _unknown:array[0..127] of Byte; // 128
 end;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltMutex=packed record
  param       :SceUltMutexOptParam;  // 128
  name        :array[0..SCE_ULT_MAX_NAME_LENGTH] of Char; // 160
  waitingQueue:PSceUltWaitingQueueResourcePool; // 168
  handle      :p_pthread_mutex;      // 176
  _unknown:array[0..255-176] of Byte; // 256
 end;

threadvar
 _currentUlThread:PSceUltUlthread;

// Wrappers

procedure _workerThreadEntry(const workerThread:PWorkerThread); SysV_ABI_CDecl;
var
 ulThread:PSceUltUlthread;
 runtime     :PSceUltUlthreadRuntime;
begin
 assert(workerThread<>nil,'workerThread cannot be null');
 Writeln(SysLogPrefix,'_workerThreadEntry Start');
 runtime:=workerThread^.runtime;
 while True do
 begin
  while workerThread^.ulThreadList.Count=0 do
  begin
   RTLeventWaitFor(workerThread^.wakeUpEvent);
  end;
  if workerThread^.ulThreadList.Count=0 then
   continue;
  // Select next ulThread to be executed
  runtime^.enter;
   Inc(workerThread^.current);
   if workerThread^.current>=workerThread^.ulThreadList.Count then
    workerThread^.current:=0;
   ulThread:=workerThread^.ulThreadList[workerThread^.current];
   _currentUlThread:=ulThread;
  runtime^.leave;

  // Execute ulThread
  if (ulThread^.state<>ULT_STATE_PREPARE_JOIN) and (ulThread^.state<>ULT_STATE_WAIT) then
  begin
   if ulThread^.fiber^.state=FIBER_STATE_INIT then
   begin
    ulThread^.state:=ULT_STATE_RUN;
    ps4_sceFiberRun(ulThread^.fiber,0,@ulThread^.returnStatus);
   end else
   if ulThread^.fiber^.state=FIBER_STATE_SUSPEND then
    ps4_sceFiberSwitch(ulThread^.fiber,0,nil);
  end;
 end;
 Writeln(SysLogPrefix,'_workerThreadEntry End');
 ps4_scePthreadExit(nil);
end;

procedure _ulThreadEntry(argInit,argRun:QWord); SysV_ABI_CDecl;
var
 ulThread:PSceUltUlthread;
begin
 ulThread:=PSceUltUlthread(argInit);
 assert(ulThread<>nil,'ulThread cannot be null');

 Writeln(SysLogPrefix,'_ulThreadEntry Start');
 ulThread^.entry(ulThread^.arg);
 Writeln(SysLogPrefix,'_ulThreadEntry End');
 ulThread^.state:=ULT_STATE_PREPARE_JOIN;
end;

function _workerThreadCreate(const runtime:PSceUltUlthreadRuntime):PWorkerThread;
begin
 New(Result);
 Result^.current     :=0;
 Result^.ulThreadList:=TUlThreadList.Create;
 Result^.ulThreadList.Capacity:=runtime^.maxUlThread;
 Result^.runtime     :=runtime;
 Result^.indx        :=runtime^.workerThreadList.Count;
 Result^.wakeUpEvent :=RTLEventCreate;
 runtime^.workerThreadList.Add(Result);
 ps4_scePthreadCreate(@Result^.thread,nil,@_workerThreadEntry,Result,PChar(runtime^.name+'_ultWorker_'+IntToStr(Result^.indx)));
end;

procedure _currentUlThreadSetState(const state:QWord); inline;
begin
 if _currentUlThread<>nil then
  _currentUlThread^.state:=state;
end;

procedure SceUltUlthreadRuntime.enter;
begin
 EnterCriticalSection(cs);
end;

procedure SceUltUlthreadRuntime.leave;
begin
 LeaveCriticalSection(cs);
end;

procedure SceUltUlthread.init(const aRuntime:PSceUltUlthreadRuntime;
                              const aName:PChar;
                              const aEntry:SceUltUlthreadEntry;
                              const aArg:QWord;
                              const aContext:Pointer;
                              const aSizeContext:QWord);
var
 workerThread:PWorkerThread;
begin
 aRuntime^.enter;
  fiber  :=AllocMem(SizeOf(SceFiber));
  runtime:=aRuntime;
  entry  :=aEntry;
  arg    :=aArg;
  ps4_sceFiberInitialize(fiber,aName,@_ulThreadEntry,QWord(@Self),aContext,aSizeContext,nil);

  workerThread:=runtime^.workerThreadList[runtime^.balancer mod runtime^.maxWorkerThread];
  workerThread^.ulThreadList.Add(@Self);
  Inc(runtime^.balancer);
  RTLEventSetEvent(workerThread^.wakeUpEvent);
 aRuntime^.leave;
end;

function SceUltUlthread.getState:Integer;
var
 i,j         :Integer;
 workerThread:PWorkerThread;
begin
 runtime^.enter;
  for j:=0 to runtime^.workerThreadList.Count-1 do
  begin
   workerThread:=runtime^.workerThreadList[j];
   for i:=0 to workerThread^.ulThreadList.Count-1 do
    if workerThread^.ulThreadList[i]=@Self then
     Exit(workerThread^.ulThreadList[i]^.state);
  end;
  Result:=ULT_STATE_DESTROYED;
 runtime^.leave;
end;

procedure SceUltUlthread.destroy;
var
 workerThread:PWorkerThread;
 ulThread    :PSceUltUlthread;
 i,j         :Integer;
begin
 ulThread:=@Self;
 runtime^.enter;
  for j:=0 to runtime^.workerThreadList.Count-1 do
  begin
   workerThread:=runtime^.workerThreadList[j];
   for i:=0 to workerThread^.ulThreadList.Count-1 do
    if workerThread^.ulThreadList[i]=ulThread then
    begin
     ps4_sceFiberFinalize(ulThread^.fiber);
     workerThread^.ulThreadList.Delete(i);
     Exit;
    end;
  end;
 runtime^.leave;
end;

procedure SceUltQueueDataResourcePool.enter;
begin
 EnterCriticalSection(cs);
end;

procedure SceUltQueueDataResourcePool.leave;
begin
 LeaveCriticalSection(cs);
end;

function SceUltQueue.push(const aData:Pointer):Integer;
begin
 queueData^.enter;
  while ((QWord(queueData^.queuePtr) - QWord(queueData^.workArea)) div queueData^.dataSize) >= queueData^.numData do
  begin
   _currentUlThreadSetState(ULT_STATE_WAIT);
   queueData^.leave;
   RTLeventWaitFor(queueData^.popEvent);
   queueData^.enter;
  end;
  _currentUlThreadSetState(ULT_STATE_RUN);
  Move(aData^,queueData^.queuePtr^,queueData^.dataSize);
  Inc(queueData^.queuePtr,queueData^.dataSize);
  Result:=0;
  RTLEventSetEvent(queueData^.pushEvent);
 queueData^.leave;
end;

function SceUltQueue.pop(const aData:Pointer):Integer;
begin
 queueData^.enter;
  while QWord(queueData^.queuePtr) <= QWord(queueData^.workArea) do
  begin
   _currentUlThreadSetState(ULT_STATE_WAIT);
   queueData^.leave;
   RTLeventWaitFor(queueData^.pushEvent);
   queueData^.enter;
  end;
  _currentUlThreadSetState(ULT_STATE_RUN);
  Move(queueData^.workArea^,aData^,queueData^.dataSize);
  Move((queueData^.workArea+queueData^.dataSize)^,queueData^.workArea^,QWord(queueData^.queuePtr)-QWord(queueData^.workArea)-queueData^.dataSize);
  Dec(queueData^.queuePtr,queueData^.dataSize);
  Result:=0;
  RTLEventSetEvent(queueData^.popEvent);
 queueData^.leave;
end;

// APIs

function ps4_sceUltInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceUltInitialize');
 Result:=0;
end;

function ps4_sceUltUlthreadRuntimeGetWorkAreaSize(maxUlThread,maxWorkerThread:DWord):QWord; SysV_ABI_CDecl;
begin
 Result:=8; // TODO: Fake size. Not important for current implementation of this lib.
end;

function ps4_sceUltUlthreadRuntimeCreate(runtime        :PSceUltUlthreadRuntime;
                                         name           :PChar;
                                         maxUlThread    :Integer;
                                         maxWorkerThread:Integer;
                                         workArea       :Pointer;
                                         param          :PSceUltUlthreadRuntimeOptParam):Integer; SysV_ABI_CDecl;
var
 i:Integer;
begin
 if (runtime=nil) or (name=nil) or (workArea=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 if (maxUlThread=0) or (maxWorkerThread=0) then
  Exit(SCE_ULT_ERROR_INVALID);
 Writeln(SysLogPrefix,'sceUltUlthreadRuntimeCreate,name=',name,',maxUltThread=',maxUlThread,',maxWorkerThread=',maxWorkerThread);
 StrLCopy(@runtime^.name[0],name,SCE_ULT_MAX_NAME_LENGTH);
 runtime^.maxUlThread    :=maxUlThread;
 runtime^.maxWorkerThread:=maxWorkerThread;
 runtime^.balancer       :=0;
 InitCriticalSection(runtime^.cs);
 if param<>nil then
  runtime^.param:=param^
 else
 begin
  runtime^.param.oneShotThreadStackSize     :=16*1024;
  runtime^.param.workerThreadCpuAffinityMask:=SCE_KERNEL_CPUMASK_6CPU_ALL;
  runtime^.param.workerThreadPriority       :=SCE_KERNEL_PRIO_FIFO_DEFAULT;
  runtime^.param.inheritSched               :=SCE_PTHREAD_INHERIT_SCHED;
 end;
 runtime^.workerThreadList:=TWorkerThreadList.Create;
 runtime^.workerThreadList.Capacity:=maxWorkerThread;
 for i:=0 to maxWorkerThread-1 do
  _workerThreadCreate(runtime);
end;

function ps4_sceUltUlthreadRuntimeOptParamInitialize(optParam:PSceUltUlthreadOptParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceUltUlthreadCreate(ulThread   :PSceUltUlthread;
                                  name       :PChar;
                                  entry      :SceUltUlthreadEntry;
                                  arg        :QWord;
                                  context    :Pointer;
                                  sizeContext:QWord;
                                  runtime    :PSceUltUlthreadRuntime;
                                  optParam   :PSceUltUlthreadOptParam):Integer; SysV_ABI_CDecl;
begin
 if (ulThread=nil) or (name=nil) or (entry=nil) or (runtime=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 if (sizeContext>0) and (sizeContext<512) then
  Exit(SCE_ULT_ERROR_RANGE);
 if (context<>nil) and ((sizeContext=0) or (sizeContext mod 16<>0)) then
  Exit(SCE_ULT_ERROR_INVALID);
 if runtime^.workerThreadList=nil then
  Exit(SCE_ULT_ERROR_STATE);
 if runtime^.ulThreadCount>=runtime^.maxUlThread then
  Exit(SCE_ULT_ERROR_AGAIN);
 Writeln(SysLogPrefix,'sceUltUlthreadCreate,name=',name,'runtime=',runtime^.name);
 ulThread^.init(runtime,name,entry,arg,context,sizeContext);
end;

function ps4_sceUltUlthreadJoin(ulThread:PSceUltUlthread;status:PInteger):Integer; SysV_ABI_CDecl;
begin
 if ulThread=nil then
  Exit(SCE_ULT_ERROR_NULL);
 if ulThread=_currentUlThread then
  Exit(SCE_ULT_ERROR_PERMISSION);
 if ulThread^.getState=ULT_STATE_DESTROYED then
  Exit(SCE_ULT_ERROR_STATE);
 Writeln(SysLogPrefix,'sceUltUlthreadJoin,fiber=',GetFiberStringParam(ulThread^.fiber));
 _currentUlThreadSetState(ULT_STATE_WAIT);
 repeat
  SwYieldExecution;
 until ulThread^.getState=ULT_STATE_PREPARE_JOIN;
 _currentUlThreadSetState(ULT_STATE_RUN);
 //
 if (status<>nil) and (ulThread^.fiber^.pArgReturn<>nil) then
 begin
  status^:=ulThread^.fiber^.pArgReturn^;
  Writeln(SysLogPrefix,'Return code:',status^);
 end;
 ulThread^.destroy;
end;

function ps4_sceUltUlthreadExit(status:Integer):Integer; SysV_ABI_CDecl;
var
 ulThread:PSceUltUlthread;
begin
 ulThread:=_currentUlThread;
 if ulThread=nil then
  Exit(SCE_ULT_ERROR_PERMISSION);
 Writeln(SysLogPrefix,'sceUltUlthreadExit,fiber=',GetFiberStringParam(ulThread^.fiber));
 ulThread^.state:=ULT_STATE_PREPARE_JOIN;
 ps4_sceFiberReturnToThread(status,nil);
end;

//

function ps4_sceUltWaitingQueueResourcePoolGetWorkAreaSize(numThreads,numSyncObjects:DWord):QWord; SysV_ABI_CDecl;
begin
 Result:=8; // TODO: Fake size. Not important for current implementation of this lib.
end;

function ps4_sceUltWaitingQueueResourcePoolCreate(pool          :PSceUltWaitingQueueResourcePool;
                                                  name          :PChar;
                                                  numThreads    :DWord;
                                                  numSyncObjects:DWord;
                                                  workArea      :Pointer;
                                                  param         :PSceUltWaitingQueueResourcePoolOptParam):Integer; SysV_ABI_CDecl;
begin
 if (pool=nil) or (name=nil) or (workArea=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 if (numThreads=0) or (numSyncObjects=0) then
  Exit(SCE_ULT_ERROR_INVALID);
 Writeln(SysLogPrefix,'sceUltWaitingQueueResourcePoolCreate,name=',name,',numThreads=',numThreads,',numSyncObjects=',numSyncObjects);
 StrLCopy(@pool^.name[0],name,SCE_ULT_MAX_NAME_LENGTH);
 pool^.numThreads    :=numThreads;
 pool^.numSyncObjects:=numSyncObjects;
 pool^.workArea      :=workArea;
 pool^.workArea      :=workArea;
 if param<>nil then
  pool^.param:=param^;
 Result:=0; // TODO: Not used by current implementation of this lib.
end;

//

function ps4_sceUltQueueDataResourcePoolGetWorkAreaSize(numData:DWord;dataSize:QWord;numQueueObjects:DWord):Integer; SysV_ABI_CDecl;
begin
 Result:=numData*dataSize*numQueueObjects;
end;

function ps4_sceUltQueueDataResourcePoolCreate(pool           :PSceUltQueueDataResourcePool;
                                               name           :PChar;
                                               numData        :DWord;
                                               dataSize       :QWord;
                                               numQueueObjects:DWord;
                                               waitingQueue   :PSceUltWaitingQueueResourcePool;
                                               workArea       :Pointer;
                                               param          :PSceUltQueueDataResourcePoolOptParam):Integer; SysV_ABI_CDecl;
begin
 if (pool=nil) or (name=nil) or (workArea=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 if (numData=0) or (dataSize=0) or (numQueueObjects=0) then
  Exit(SCE_ULT_ERROR_INVALID);
 assert(numQueueObjects=1,'TODO: numQueueObjects higher than 1');
 Writeln(SysLogPrefix,'sceUltQueueDataResourcePoolCreate,name=',name,',numData=',numData,',dataSize=',dataSize,',numQueueObjects=',numQueueObjects);
 StrLCopy(@pool^.name[0],name,SCE_ULT_MAX_NAME_LENGTH);
 pool^.numData        :=numData;
 pool^.dataSize       :=dataSize;
 pool^.numQueueObjects:=numQueueObjects;
 pool^.waitingQueue   :=waitingQueue;
 pool^.workArea       :=workArea;
 pool^.queuePtr       :=workArea;
 InitCriticalSection(pool^.cs);
 pool^.pushEvent      :=RTLEventCreate;
 pool^.popEvent       :=RTLEventCreate;
 if param<>nil then
  pool^.param:=param^;
 Result:=0;
end;

//

function ps4_sceUltQueueCreate(queue       :PSceUltQueue;
                               name        :PChar;
                               dataSize    :QWord;
                               waitingQueue:PSceUltWaitingQueueResourcePool;
                               queueData   :PSceUltQueueDataResourcePool;
                               param       :PSceUltQueueOptParam):Integer; SysV_ABI_CDecl;
begin
 if (queue=nil) or (name=nil) or (queueData=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 if (dataSize=0) or (dataSize>queueData^.dataSize) then
  Exit(SCE_ULT_ERROR_INVALID);
 Writeln(SysLogPrefix,'sceUltQueueCreate,name=',name,',dataSize=',dataSize);
 StrLCopy(@queue^.name[0],name,SCE_ULT_MAX_NAME_LENGTH);
 queue^.queueData   :=queueData;
 queue^.waitingQueue:=waitingQueue;
 if param<>nil then
  queue^.param:=param^;
 Result:=0;
end;

function ps4_sceUltQueuePush(queue:PSceUltQueue;data:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (queue=nil) or (data=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 Writeln(SysLogPrefix,'sceUltQueuePush,queue=',queue^.name);
 Result:=queue^.push(data);
end;

function ps4_sceUltQueuePop(queue:PSceUltQueue;data:Pointer):Integer; SysV_ABI_CDecl;
begin
 if (queue=nil) or (data=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 Writeln(SysLogPrefix,'sceUltQueuePop,queue=',queue^.name);
 Result:=queue^.pop(data);
end;

//

function ps4_sceUltMutexCreate(mutex       :PSceUltMutex;
                               name        :PChar;
                               waitingQueue:PSceUltWaitingQueueResourcePool;
                               param       :PSceUltMutexOptParam):Integer; SysV_ABI_CDecl;
begin
 if (mutex=nil) or (name=nil) or (waitingQueue=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 Writeln(SysLogPrefix,'sceUltMutexCreate,name=',name);
 mutex^.handle      :=AllocMem(SizeOf(pthread_mutex));
 ps4_pthread_mutex_init(mutex^.handle,nil);
 StrLCopy(@mutex^.name[0],name,SCE_ULT_MAX_NAME_LENGTH);
 mutex^.waitingQueue:=waitingQueue;
 if param<>nil then
  mutex^.param:=param^;
 Result:=0;
end;

function ps4_sceUltMutexLock(mutex:PSceUltMutex):Integer; SysV_ABI_CDecl;
begin
 if (mutex=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 //Writeln(SysLogPrefix,'sceUltMutexLock,mutex=',mutex^.name);
 ps4_pthread_mutex_lock(mutex^.handle);
end;

function ps4_sceUltMutexUnlock(mutex:PSceUltMutex):Integer; SysV_ABI_CDecl;
begin
 if (mutex=nil) then
  Exit(SCE_ULT_ERROR_NULL);
 //Writeln(SysLogPrefix,'sceUltMutexUnlock,mutex=',mutex^.name);
 ps4_pthread_mutex_unlock(mutex^.handle);
end;

function ps4_sceUltMutexOptParamInitialize(param:PSceUltMutexOptParam):Integer; SysV_ABI_CDecl;
begin
 if param=nil then
  Exit(SCE_ULT_ERROR_NULL);
 //Writeln(SysLogPrefix,'sceUltMutexOptParamInitialize');
 Result:=0;
end;

//

function Load_libSceUlt(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUlt');

 lib^.set_proc($859220D44586B073,@ps4_sceUltInitialize);
 lib^.set_proc($82BB36A5B7366B03,@ps4_sceUltUlthreadRuntimeGetWorkAreaSize);
 lib^.set_proc($576BB758BAF087AE,@ps4_sceUltUlthreadRuntimeOptParamInitialize);
 lib^.set_proc($8F0F45919057A3F8,@ps4_sceUltUlthreadRuntimeCreate);
 lib^.set_proc($CE7237ABC4BB290E,@ps4_sceUltUlthreadCreate);
 lib^.set_proc($802780239ECB1A02,@ps4_sceUltUlthreadJoin);
 lib^.set_proc($905FFFB37C598DCA,@ps4_sceUltUlthreadExit);

 lib^.set_proc($588595D5077B3C55,@ps4_sceUltWaitingQueueResourcePoolGetWorkAreaSize);
 lib^.set_proc($6221EE8CE1BDBD76,@ps4_sceUltWaitingQueueResourcePoolCreate);

 lib^.set_proc($7AF8FD60F912F2CE,@ps4_sceUltQueueDataResourcePoolGetWorkAreaSize);
 lib^.set_proc($4C51E6EBF37ABE4B,@ps4_sceUltQueueDataResourcePoolCreate);

 lib^.set_proc($F58E6478EBDBEA89,@ps4_sceUltQueueCreate);
 lib^.set_proc($754C295F77B93431,@ps4_sceUltQueuePush);
 lib^.set_proc($4554AADADB26DB2C,@ps4_sceUltQueuePop);

 lib^.set_proc($9A6B7C49AEAD2FA7,@ps4_sceUltMutexCreate);
 lib^.set_proc($F21106911D697EBF,@ps4_sceUltMutexLock);
 lib^.set_proc($8745DE6CA88C06D9,@ps4_sceUltMutexUnlock);
 lib^.set_proc($D7EF2DF5A1CB8B3F,@ps4_sceUltMutexOptParamInitialize);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUlt.prx',@Load_libSceUlt);

end.

