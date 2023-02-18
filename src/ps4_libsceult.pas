unit ps4_libSceUlt;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 ps4_program,
 sys_pthread,
 sys_kernel,
 ps4_pthread,
 Generics.Collections;

implementation

uses
 ps4_libSceFiber;

const
 SCE_ULT_ERROR_NULL          =$80810001;
 SCE_ULT_ERROR_ALIGNMENT     =$80810002;
 SCE_ULT_ERROR_RANGE         =$80810003;
 SCE_ULT_ERROR_INVALID       =$80810004;
 SCE_ULT_ERROR_PERMISSION    =$80810005;
 SCE_ULT_ERROR_STATE         =$80810006;
 SCE_ULT_ERROR_AGAIN         =$80810008;
 SCE_ULT_ERROR_NOT_INITIALIZE=$8081000A;

 ULT_STATE_INIT        =0;
 ULT_STATE_RUN         =1;
 ULT_STATE_SUSPEND     =2;
 ULT_STATE_PREPARE_JOIN=3;
 ULT_STATE_DESTROYED   =4;

type
 SceUltUlthreadEntry=function(arg:QWord):Integer; SysV_ABI_CDecl;

 SceUltUlthreadRuntimeOptParam=packed record
  oneShotThreadStackSize     :QWord;    // 8
  workerThreadCpuAffinityMask:QWord;    // 16
  workerThreadPriority       :Integer;  // 20
  inheritSched               :Integer;  // 24
  _unknown                   :array[0..127-24] of Byte; // 128
 end;
 PSceUltUlthreadRuntimeOptParam=^SceUltUlthreadRuntimeOptParam;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltUlthread=packed record
  fiber   :PSceFiber;           // 8
  entry   :SceUltUlthreadEntry; // 16
  arg     :QWord;               // 24
  state   :QWord;               // 32
  runtime :Pointer;             // 40
  _unknown:array[0..255-40] of Byte; // 256
 end;
 PSceUltUlthread=^SceUltUlthread;

 TUlThreadList=specialize TList<PSceUltUlthread>;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceUltUlthreadOptParam=packed record
  attr    :DWord;                 // 4
  _unknown:array[0..127-4] of Byte; // 256
 end;
 PSceUltUlthreadOptParam=^SceUltUlthreadOptParam;

 TWorkerThread=record
  thread      :pthread;
  runtime     :Pointer;
  ulThreadList:TUlThreadList;
  current     :QWord;
 end;
 PWorkerThread=^TWorkerThread;

 TWorkerThreadList=specialize TList<PWorkerThread>;

 // Our own invention of SceUltUlthreadRuntime. Not compatible with original one
 // Original size is 4096 bytes, but we ignore this info
 SceUltUlthreadRuntime=packed record
  name            :RawByteString;
  maxUlThread     :Integer;
  maxWorkerThread :Integer;
  param           :SceUltUlthreadRuntimeOptParam;
  workerThreadList:TWorkerThreadList;
  ulThreadCount   :QWord;
  cs              :TRTLCriticalSection;
  balancer        :QWord;
 end;
 PSceUltUlthreadRuntime=^SceUltUlthreadRuntime;

threadvar
 _currentUlThread:PSceUltUlthread;

// Wrappers

procedure _ulThreadDestroy(const ulThread:PSceUltUlthread);
var
 workerThread:PWorkerThread;
 runtime     :PSceUltUlthreadRuntime;
 i,j         :Integer;
begin
 runtime:=ulThread^.runtime;
 EnterCriticalSection(runtime^.cs);
  for j:=0 to runtime^.workerThreadList.Count-1 do
  begin
   workerThread:=runtime^.workerThreadList[j];
   for i:=0 to workerThread^.ulThreadList.Count-1 do
    if workerThread^.ulThreadList[i]=ulThread then
    begin
     ps4_sceFiberFinalize(ulThread^.fiber);
     Dispose(ulThread);
     workerThread^.ulThreadList.Delete(i);
     Exit;
    end;
  end;
 LeaveCriticalSection(runtime^.cs);
end;

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
  if workerThread^.ulThreadList.Count=0 then
  begin
   SwYieldExecution;
  end else
  begin
   if workerThread^.ulThreadList.Count=0 then
    continue;
   // Select next ulThread to be executed
   EnterCriticalSection(runtime^.cs);
    Inc(workerThread^.current);
    if workerThread^.current>=workerThread^.ulThreadList.Count then
     workerThread^.current:=0;
    ulThread:=workerThread^.ulThreadList[workerThread^.current];
    _currentUlThread:=ulThread;
   LeaveCriticalSection(runtime^.cs);

   // Execute ulThread
   if ulThread^.state<>ULT_STATE_PREPARE_JOIN then
   begin
    if ulThread^.fiber^.state=FIBER_STATE_INIT then
    begin
     ulThread^.state:=ULT_STATE_RUN;
     ps4_sceFiberRun(ulThread^.fiber,0,nil);
    end else
    if ulThread^.fiber^.state=FIBER_STATE_SUSPEND then
     ps4_sceFiberSwitch(ulThread^.fiber,0,nil);
   end;
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
 runtime^.workerThreadList.Add(Result);
 ps4_scePthreadCreate(@Result^.thread,nil,@_workerThreadEntry,Result,PChar(runtime^.name+'_ultWorker_'+IntToStr(runtime^.workerThreadList.Count)));
end;

procedure _ulThreadCreate(const ulThread:PSceUltUlthread;
                          const runtime:PSceUltUlthreadRuntime;
                          const name:PChar;
                          const entry:SceUltUlthreadEntry;
                          const arg:QWord;
                          const context:Pointer;
                          const sizeContext:QWord);
var
 workerThread:PWorkerThread;
begin
 EnterCriticalSection(runtime^.cs);
  New(ulThread^.fiber);
  ulThread^.runtime:=runtime;
  ulThread^.entry  :=entry;
  ulThread^.arg    :=arg;
  ps4_sceFiberInitialize(ulThread^.fiber,name,@_ulThreadEntry,QWord(ulThread),context,sizeContext,nil);

  workerThread:=runtime^.workerThreadList[runtime^.balancer mod runtime^.maxWorkerThread];
  workerThread^.ulThreadList.Add(ulThread);
  Inc(runtime^.balancer);
 LeaveCriticalSection(runtime^.cs);
end;

function _ulThreadGetState(const ulThread:PSceUltUlthread):Integer;
var
 i,j         :Integer;
 runtime     :PSceUltUlthreadRuntime;
 workerThread:PWorkerThread;
begin
 runtime:=ulThread^.runtime;
 EnterCriticalSection(runtime^.cs);
  for j:=0 to runtime^.workerThreadList.Count-1 do
  begin
   workerThread:=runtime^.workerThreadList[j];
   for i:=0 to workerThread^.ulThreadList.Count-1 do
    if workerThread^.ulThreadList[i]=ulThread then
     Exit(workerThread^.ulThreadList[i]^.state);
  end;
  Result:=ULT_STATE_DESTROYED;
 LeaveCriticalSection(runtime^.cs);
end;

// APIs

function ps4_sceUltInitialize():Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceUltInitialize');
 Result:=0;
end;

function ps4_sceUltUlthreadRuntimeGetWorkAreaSize(maxUlThread,maxWorkerThread:Integer):QWord; SysV_ABI_CDecl;
begin
 Result:=8; // TODO: Fake size. Not important for current implementation of this lib.
end;

function ps4_sceUltWaitingQueueResourcePoolGetWorkAreaSize(numThreads,numSyncObjects:Integer):QWord; SysV_ABI_CDecl;
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
 runtime^.name           :=name;
 runtime^.maxUlThread    :=maxUlThread;
 runtime^.maxWorkerThread:=maxWorkerThread;
 runtime^.balancer       :=0;
 if param<>nil then
  runtime^.param:=param^
 else
 begin
  runtime^.param.oneShotThreadStackSize     :=16*1024;
  runtime^.param.workerThreadCpuAffinityMask:=SCE_KERNEL_CPUMASK_6CPU_ALL;
  runtime^.param.workerThreadPriority       :=SCE_KERNEL_PRIO_FIFO_DEFAULT;
  runtime^.param.inheritSched               :=SCE_PTHREAD_INHERIT_SCHED;
  InitCriticalSection(runtime^.cs);
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
 _ulThreadCreate(ulThread,runtime,name,entry,arg,context,sizeContext);
end;

function ps4_sceUltUlthreadJoin(ulThread:PSceUltUlthread;status:PInteger):Integer; SysV_ABI_CDecl;
begin
 if ulThread=nil then
  Exit(SCE_ULT_ERROR_NULL);
 if ulThread=_currentUlThread then
  Exit(SCE_ULT_ERROR_PERMISSION);
 if _ulThreadGetState(ulThread)=ULT_STATE_DESTROYED then
  Exit(SCE_ULT_ERROR_STATE);
 Writeln(SysLogPrefix,'sceUltUlthreadJoin,fiber=',GetFiberStringParam(ulThread^.fiber));
 repeat
  SwYieldExecution;
 until _ulThreadGetState(ulThread)=ULT_STATE_PREPARE_JOIN;
 //
 if (status<>nil) and (ulThread^.fiber^.pArgReturn<>nil) then
  status^:=ulThread^.fiber^.pArgReturn^;
 _ulThreadDestroy(ulThread);
end;

function Load_libSceUlt(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceUlt');

 lib^.set_proc($859220D44586B073,@ps4_sceUltInitialize);
 lib^.set_proc($82BB36A5B7366B03,@ps4_sceUltUlthreadRuntimeGetWorkAreaSize);
 lib^.set_proc($588595D5077B3C55,@ps4_sceUltWaitingQueueResourcePoolGetWorkAreaSize);
 lib^.set_proc($576BB758BAF087AE,@ps4_sceUltUlthreadRuntimeOptParamInitialize);
 lib^.set_proc($8F0F45919057A3F8,@ps4_sceUltUlthreadRuntimeCreate);
 lib^.set_proc($CE7237ABC4BB290E,@ps4_sceUltUlthreadCreate);
 lib^.set_proc($802780239ECB1A02,@ps4_sceUltUlthreadJoin);
end;

initialization
 ps4_app.RegistredPreLoad('libSceUlt.prx',@Load_libSceUlt);

end.

