unit ps4_libSceSaveData;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 LFQueue,
 mqueue,
 errno,
 SceSaveData,
 SaveDataBackend,
 kern_thr,
 kern_proc,
 kern_ksched,
 kern_authinfo,
 md_event,
 kern_mtx,
 mpmc_queue,
 subr_dynlib,
 game_mount,
 vfs_mountroot,
 ps4_libSceUserService;

implementation

///

type
 t_init_version=(VERSION_INIT_0,VERSION_INIT_2,VERSION_INIT_3,VERSION_INIT_CDLG);

 TCustomCommand=class
  type
   PQNode=^TQNode;
   TQNode=object
    entry:TAILQ_ENTRY;
    self_:TCustomCommand;
   end;
  var
   node  :TQNode;
   finish:Boolean;
   refs  :Integer;
  Constructor Create;
  procedure   inc_ref;
  procedure   dec_ref;
  procedure   Run; virtual;
 end;

 TProgressJob=class(TCustomCommand)
  p_progress:PInteger;
  procedure   Run; override;
 end;

 TEventJob=class(TCustomCommand)
  repeat_count:Integer;
  procedure   Run; override;
 end;

 TJobList=object
  signal:t_event;
  queue :TIntrusiveMPSCQueue;
  tqlist:TAILQ_HEAD;
  count :Integer;
  procedure Init;
  procedure Fini;
  procedure SendCmd(cmd:TCustomCommand);
  procedure SendEventJob();
  procedure Action;
 end;

 TSaveDataInstance=class
  version             :t_init_version;
  memory_timeout_10sec:Boolean;
  force_default_prio  :Boolean;
  not_prio_by_cusaname:Boolean;
  thread_stop         :Boolean;
  priority            :Integer;
  threadStackSize     :DWORD;
  cpuAffinityMask     :QWORD;
  job_thread          :Pointer;
  mtx                 :mtx;
  //
  job_list            :TJobList;
  //
  Backend:TSaveDataBackendConnect;
  //
  cb_event   :SceSaveDataEventCallbackFunc;
  cb_userdata:Pointer;
  //
  procedure Terminate;
 end;

var
 g_instance:TSaveDataInstance;

function CheckDataInitParams0(params:pSceSaveDataInitParams):Integer; inline;
begin
 if (params=nil) then Exit(0);
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (DWORD(params^.priority-256)<512) then
 if (Byte(params^.reserved[0])<2) then
 if CheckReserved(params^.reserved[1],sizeof(params^.reserved)-1) then
 begin
  Result:=0;
 end;
end;

function CheckDataInitParams1(params:pSceSaveDataInitParams):Integer; inline;
begin
 if (params=nil) then Exit(0);
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (DWORD(params^.priority-256)<512) then
 if CheckReserved(params^.reserved,sizeof(params^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckDataInitParams2(params:pSceSaveDataInitParams2):Integer; inline;
begin
 if (params=nil) then Exit(0);
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (DWORD(params^.priority-256)<512) then
 if (DWORD(params^.threadStackSize-1)>$3ffe) then
 if (QWORD(params^.cpuAffinityMask)<64) then
 if CheckReserved(params^.reserved,sizeof(params^.reserved)) then
 begin
  Result:=0;
 end;
end;

function CheckDataInitParams3(params:Pointer):Integer; inline;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (params=nil) then
 begin
  Result:=0;
 end;
end;

function InitInstance(instance:TSaveDataInstance;params:Pointer;version:t_init_version):Integer;
begin
 mtx_init(instance.mtx,'SaveDataInstance');
 //

 instance.version        :=version;
 instance.priority       :=700;
 instance.threadStackSize:=$4000;
 instance.cpuAffinityMask:=0;

 case version of
  VERSION_INIT_0:
   begin
    if (params=nil) then Exit(0);

    if (p_proc.p_sdk_version >= $2000000) then
    begin
     Result:=CheckDataInitParams1(params);
     if (Result=0) then
     begin
      instance.priority            :=pSceSaveDataInitParams(params)^.priority;
      instance.not_prio_by_cusaname:=true;
     end;
    end else
    begin
     Result:=CheckDataInitParams0(params);
     if (Result=0) then
     begin
      instance.priority            :=pSceSaveDataInitParams(params)^.priority;
      instance.force_default_prio  :=(pSceSaveDataInitParams(params)^.reserved[0]<>0);
      instance.not_prio_by_cusaname:=true;
     end;
    end;

   end;
  VERSION_INIT_2:
   begin
    Result:=CheckDataInitParams2(params);
    if (Result=0) then
    begin
     instance.priority            :=pSceSaveDataInitParams2(params)^.priority;
     instance.not_prio_by_cusaname:=true;
     instance.threadStackSize     :=pSceSaveDataInitParams2(params)^.threadStackSize;
     instance.cpuAffinityMask     :=pSceSaveDataInitParams2(params)^.cpuAffinityMask;
    end;
   end;
  VERSION_INIT_3:
   begin
    Result:=CheckDataInitParams3(params);
   end;
  VERSION_INIT_CDLG:
   Assert(False,'VERSION_INIT_CDLG');
 end;

end;

procedure Getprio_by_cusaname(instance:TSaveDataInstance);
var
 sched_param:t_sched_param;
begin
 if (p_proc.p_sdk_version < $2000000) and
    (instance.force_default_prio=false) then
 begin
  instance.priority:=700;
 end;

 if (instance.not_prio_by_cusaname=false) then
 begin

  case String(g_appinfo.CUSANAME) of
   'CUSA00503',
   'CUSA01425',
   'CUSA00220':
     begin
      //scePthreadGetprio(scePthreadSelf(),&instance->prio)

      sched_param:=Default(t_sched_param);

      PROC_LOCK();
      ksched_getparam(@ksched, curkthread, @sched_param);
      PROC_UNLOCK();

      if (sched_param.sched_priority<>0) then
      begin
       instance.priority:=sched_param.sched_priority;
      end;

     end;
   else;
  end;

 end;
end;

///

Constructor TCustomCommand.Create;
begin
 node.self_:=self;
 finish:=False;
end;

procedure TCustomCommand.inc_ref;
begin
 System.InterlockedIncrement(refs);
end;

procedure TCustomCommand.dec_ref;
begin
 if (System.InterlockedDecrement(refs)=0) then
 begin
  Free;
 end;
end;

procedure TCustomCommand.Run;
begin
 //
end;

///

procedure TProgressJob.Run;
var
 progres:Single;
begin
 if (g_instance=nil) then
 begin
  finish:=True;
  Exit;
 end;

 //TODO: Invoke GetProgress
 progres:=1;

 if (p_progress <> nil) then
 begin
  p_progress^:=Trunc(progres*100);
 end;

 if (p_progress <> nil) and
    (p_progress^ = 100) then
 begin
  finish:=True;
 end;
end;

///

procedure ExecuteGuest_cb_event(addr,event,userdata:Pointer); external name 'ExecuteGuest';

procedure TEventJob.Run;
var
 err:Integer;
 event:SceSaveDataEvent;
 ga:TGUEST_STACK;
 p_event:pSceSaveDataEvent;
begin
 if (g_instance=nil) then
 begin
  finish:=True;
  Exit;
 end;

 if (repeat_count < 60) then
 begin
  Inc(repeat_count);
  Exit;
 end;

 mtx_lock(g_instance.mtx);

  err:=g_instance.Backend.GetEventResult(@event);

 mtx_unlock(g_instance.mtx);

 if (err=SCE_SAVE_DATA_ERROR_NOT_FOUND) then
 begin
  repeat_count:=0;
  Exit;
 end;

 //CallEventCallback
 mtx_lock(g_instance.mtx);

  if (g_instance.cb_event<>nil) then
  begin
   ga:=prolog;

   p_event:=ga.alloca(SizeOf(SceSaveDataEvent));
   p_event^:=event;

   ExecuteGuest_cb_event(g_instance.cb_event,p_event,g_instance.cb_userdata);

   ga.epilog;
  end;

 mtx_unlock(g_instance.mtx);
 //CallEventCallback

 finish:=True;
end;

///

procedure TJobList.Init;
begin
 ev_init(signal,'signal');
 queue.Create;
 TAILQ_INIT(@tqlist);
 count:=0;
end;

procedure TJobList.Fini;
var
 node:TCustomCommand.PQNode;
 cmd:TCustomCommand;
begin
 node:=nil;
 while (queue.Pop(node)) do
 begin
  cmd:=node^.self_;
  cmd.Free;
 end;

 node:=TAILQ_FIRST(@tqlist);
 while (node<>nil) do
 begin
  TAILQ_REMOVE(@tqlist,node,@node^.entry);
  //
  cmd:=node^.self_;
  cmd.Free;
  ///
  node:=TAILQ_FIRST(@tqlist);
 end;

 count:=0;
end;

procedure TJobList.SendCmd(cmd:TCustomCommand);
begin
 if (cmd=nil) then Exit;
 cmd.inc_ref;
 queue.Push(@cmd.node);
 ev_signal(signal);
end;

procedure TJobList.SendEventJob();
var
 cmd:TEventJob;
begin
 cmd:=TEventJob.Create;
 SendCmd(cmd);
end;

procedure TJobList.Action;
var
 node,next:TCustomCommand.PQNode;
 cmd:TCustomCommand;
begin
 node:=nil;
 while (queue.Pop(node)) do
 begin
  TAILQ_INSERT_TAIL(@tqlist,node,@node^.entry);
  Inc(count);
 end;

 if (count=0) then
 begin
  ev_wait(signal);
 end;

 node:=TAILQ_FIRST(@tqlist);
 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.entry);
  //
  cmd:=node^.self_;

  if cmd.finish then
  begin
   TAILQ_REMOVE(@tqlist,node,@node^.entry);
   Dec(count);
   cmd.dec_ref;
  end else
  begin
   cmd.Run;
  end;

  ///
  node:=next;
 end;
end;

function job_thread(data:Pointer):Pointer; SysV_ABI_CDecl;
var
 instance:TSaveDataInstance;
begin
 Result:=nil;
 instance:=g_instance;

 writeln('job_thread');

 repeat
  instance.job_list.Action;

  sleep(16);
 until instance.thread_stop;

end;

type
 p_pthread_attr_t=^pthread_attr_t;
 pthread_attr_t  =Pointer;

 p_pthread_t=^pthread_t;
 pthread_t  =Pointer;

var
 ps4_job_thread                   :Pointer=nil;
 ps4_scePthreadAttrInit           :function(pAttr:p_pthread_attr_t):Integer;
 ps4_scePthreadAttrDestroy        :function(pAttr:p_pthread_attr_t):Integer;
 ps4_scePthreadAttrSetstacksize   :function(pAttr:p_pthread_attr_t;size:QWORD):Integer;
 ps4_scePthreadAttrSetaffinity    :function(pAttr:p_pthread_attr_t;mask:QWORD):Integer;
 ps4_scePthreadAttrSetinheritsched:function(pAttr:p_pthread_attr_t;sched_inherit:Integer):Integer;
 ps4_scePthreadAttrSetschedpolicy :function(pAttr:p_pthread_attr_t;policy:Integer):Integer;
 ps4_scePthreadAttrSetschedparam  :function(pAttr:p_pthread_attr_t;param:PInteger):Integer;
 ps4_scePthreadCreate             :function(pthread:p_pthread_t;
                                            pAttr  :p_pthread_attr_t;
                                            entry  :Pointer;
                                            arg    :Pointer;
                                            name   :Pchar):Integer;
 ps4_scePthreadJoin               :function(pthread:pthread_t;value_ptr:PPointer):Integer;

procedure InitJobThread(instance:TSaveDataInstance);
const
 ThreadName='SceSaveData'#0;
var
 ga:TGUEST_STACK;
 p_attr       :p_pthread_attr_t;
 p_pthread    :p_pthread_t;
 p_policy     :PInteger;
 p_thread_name:PChar;
begin
 instance.job_list.Init;
 instance.thread_stop:=False;
 instance.job_thread :=nil;

 ga:=prolog;

 p_attr       :=ga.alloca(SizeOf(Pointer));
 p_pthread    :=ga.alloca(SizeOf(pthread_t));
 p_policy     :=ga.alloca(SizeOf(Integer));
 p_thread_name:=ga.alloca(Length(ThreadName));

 p_attr^:=nil;
 StrPCopy(p_thread_name,ThreadName);

 ps4_scePthreadAttrInit(p_attr);
 ps4_scePthreadAttrSetstacksize(p_attr,instance.threadStackSize);
 ps4_scePthreadAttrSetschedpolicy(p_attr,2);

 if (instance.priority <> 0) then
 begin
  p_policy^:=instance.priority;
  ps4_scePthreadAttrSetschedparam(p_attr,p_policy);
 end;

 if (instance.cpuAffinityMask <> 0) then
 begin
  ps4_scePthreadAttrSetaffinity(p_attr,instance.cpuAffinityMask);
 end;

 ps4_scePthreadCreate(p_pthread,p_attr,ps4_job_thread,nil,p_thread_name);

 instance.job_thread:=p_pthread^;

 ps4_scePthreadAttrDestroy(p_attr);

 ga.epilog;

 Assert(instance.job_thread<>nil);
end;

procedure JoinThread(instance:TSaveDataInstance);
begin
 if (instance.job_thread<>nil) then
 begin
  instance.thread_stop:=True;
  ev_signal(instance.job_list.signal);
  //
  ps4_scePthreadJoin(instance.job_thread,nil);
  instance.job_thread:=nil;
  //
  instance.job_list.Fini;
 end;
end;

function ConnectInstance(instance:TSaveDataInstance):Integer;
begin
 Result:=0;

 if (instance.version=VERSION_INIT_3) then
 begin

  if (p_proc.p_sdk_version < $6500000) then
  begin

   if (
       g_appinfo.titleWorkaround.ids[0] and
       (QWORD(1) shl BUG180029_SAVE_DATA_MEMORY_TIMEOUT_10SEC)
      )<>0 then
   begin
    instance.memory_timeout_10sec:=True;
   end;

  end else
  begin
   instance.memory_timeout_10sec:=True;
  end;

 end else
 begin
  Getprio_by_cusaname(instance);

  InitJobThread(instance);
 end;

 instance.Backend:=TSaveDataBackendConnect.Create;
end;

function CreateSaveDataInstance(params:Pointer;version:t_init_version):Integer;
var
 instance:TSaveDataInstance;
begin
 if (g_instance<>nil) then Exit(0);

 instance:=TSaveDataInstance.Create;
 Result:=InitInstance(instance,params,version);
 g_instance:=instance;

 if (Result<0) then
 begin
  g_instance.Free;
  g_instance:=nil;
  Exit;
 end;

 Result:=ConnectInstance(g_instance);
end;

function ps4_sceSaveDataInitialize(params:pSceSaveDataInitParams):Integer;
begin
 Result:=CreateSaveDataInstance(params,VERSION_INIT_0);
end;

function ps4_sceSaveDataInitialize2(params:pSceSaveDataInitParams2):Integer;
begin
 Result:=CreateSaveDataInstance(params,VERSION_INIT_2);
end;

function ps4_sceSaveDataInitialize3(params:pSceSaveDataInitParams3):Integer;
begin
 Result:=CreateSaveDataInstance(params,VERSION_INIT_3);
end;

procedure TSaveDataInstance.Terminate;
begin
 JoinThread(self);
 if (Backend<>nil) then
 begin
  Backend.UmountAllForce;
  Backend.Free;
 end;
 Free;
end;

function ps4_sceSaveDataTerminate:Integer;
begin
 if (g_instance<>nil) then
 begin
  g_instance.Terminate;
  g_instance:=nil;
  Exit(0);
 end;
 Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
end;

function ps4_sceSaveDataSetupSaveDataMemory(
           userId    :SceUserServiceUserId;
           memorySize:QWORD;
           param     :pSceSaveDataParam):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;
end;

function ps4_sceSaveDataSetupSaveDataMemory2(
           setupParam:pSceSaveDataMemorySetup2;
           pResult   :pSceSaveDataMemorySetupResult):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;
end;

function ps4_sceSaveDataGetSaveDataMemory(
           userId :SceUserServiceUserId;
           buf    :Pointer;
           bufSize:QWORD;
           offset :QWORD):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (buf<>nil) then
 begin
  FillChar(buf^,bufSize,0);
 end;
 Result:=0;
end;

function ps4_sceSaveDataGetSaveDataMemory2(
           getParam:pSceSaveDataMemoryGet2):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (getParam<>nil) then
 begin
  if (getParam^.data<>nil) then
  begin
   if (getParam^.data^.buf<>nil) then
   begin
    FillChar(getParam^.data^.buf^,getParam^.data^.bufSize,0);
   end;
  end;
 end;
 Result:=0;
end;

function ps4_sceSaveDataSetSaveDataMemory(
           userId :SceUserServiceUserId;
           buf    :Pointer;
           bufSize:QWORD;
           offset :QWORD):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;
end;

function ps4_sceSaveDataSetSaveDataMemory2(
           setParam:pSceSaveDataMemorySet2):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;
end;

function ps4_sceSaveDataSyncSaveDataMemory(
           syncParam:pSceSaveDataMemorySync):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;
end;

function SaveDataDelete(del:pSceSaveDataDelete):Integer;
begin
 Result:=CheckSaveDataDelete(del);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.DoDelete(del);

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 begin
  if (p_proc.p_sdk_version < $3500000) then
  begin
   del^.progress:=100;
  end else
  begin
   //
  end;
 end;

end;

function ps4_sceSaveDataDelete(del:pSceSaveDataDelete):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (del=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=SaveDataDelete(del);
end;

function SaveDataMount(mount      :pSceSaveDataMount;
                       pResult    :pSceSaveDataMountResult;
                       Transfering:Boolean):Integer;
var
 mountMode:DWORD;
 output   :TMountResult;
begin
 Result:=CheckSaveDataMount(mount,pResult,Transfering);
 if (Result<>0) then Exit;

 mountMode:=mount^.mountMode;
 if (p_proc.p_sdk_version < $4500000) then
 begin
  mountMode:=mountMode and (not SDM_CREATE2);
 end;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.DoMount(mount,output,Transfering);

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 begin
  //out
  pResult^.mountPoint    :=mount_savedata_slot_name[output.slot_id];
  pResult^.requiredBlocks:=output.requiredBlocks;

  if (p_proc.p_sdk_version < $3500000) then
  begin
   pResult^.progress:=100;
  end else
  begin
   pResult^.mountStatus:=output.mountStatus;
  end;
 end;

end;

function ps4_sceSaveDataMount(mount      :pSceSaveDataMount;
                              mountResult:pSceSaveDataMountResult):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (mount=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=SaveDataMount(mount,mountResult,False);
end;

function ps4_sceSaveDataMount2(mount      :pSceSaveDataMount2;
                               mountResult:pSceSaveDataMountResult):Integer;
var
 tmp:SceSaveDataMount;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (mount=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if not CheckReserved(mount^.reserved,sizeof(mount^.reserved)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 tmp:=Default(SceSaveDataMount);
 tmp.userId   :=mount^.userId   ;
 tmp.dirName  :=mount^.dirName  ;
 tmp.blocks   :=mount^.blocks   ;
 tmp.mountMode:=mount^.mountMode;

 Result:=SaveDataMount(@tmp,mountResult,False);
end;

function ps4_sceSaveDataTransferringMount(mount      :pSceSaveDataTransferringMount;
                                          mountResult:pSceSaveDataMountResult):Integer;
var
 tmp:SceSaveDataMount;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (mount=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if not CheckReserved(mount^.reserved,sizeof(mount^.reserved)) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 tmp:=Default(SceSaveDataMount);
 tmp.userId     :=mount^.userId     ;
 tmp.titleId    :=mount^.titleId    ;
 tmp.dirName    :=mount^.dirName    ;
 tmp.fingerprint:=mount^.fingerprint;
 tmp.mountMode  :=SDM_RDONLY        ;

 Result:=SaveDataMount(@tmp,mountResult,True);
end;

function SaveDataUmount(mountPoint:pSceSaveDataMountPoint;backup:boolean):Integer;
var
 slot_id:Integer;
begin
 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.DoUmount(slot_id,backup);

 mtx_unlock(g_instance.mtx);

 if (Result=0) and backup then
 if (g_instance.job_thread<>nil) then
 begin
  g_instance.job_list.SendEventJob();
 end;
end;

function ps4_sceSaveDataUmount(mountPoint:pSceSaveDataMountPoint):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=SaveDataUmount(mountPoint,False);
end;

function ps4_sceSaveDataUmountWithBackup(mountPoint:pSceSaveDataMountPoint):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=SaveDataUmount(mountPoint,True);
end;

function ps4_sceSaveDataGetMountInfo(mountPoint:pSceSaveDataMountPoint;
                                     info:pSceSaveDataMountInfo):Integer;
var
 slot_id:Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 Result:=CheckMountInfo(info);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.GetMountInfo(slot_id,info);

 mtx_unlock(g_instance.mtx);
end;

function _convert_dir_name_search(P:PChar):RawByteString;
var
 i:Integer;
begin
 Result:=RawByteString(P); //copy
 if (Length(Result)=0) then
 begin
  Result:='*';
 end else
 For i:=1 to Length(Result) do
 begin
  Case Result[i] of
   '%':Result[i]:='*';
   '_':Result[i]:='#';
   else;
  end;
 end;
end;

{
function StringListAscCompare(List:TStringList;Index1,Index2:Integer):Integer;
begin
 Result:=CompareStr(List[Index1],List[Index2]);
end;

function StringListDscCompare(List:TStringList;Index1,Index2:Integer):Integer;
begin
 Result:=CompareStr(List[Index2],List[Index1]);
end;
}

function ps4_sceSaveDataDirNameSearch(cond:pSceSaveDataDirNameSearchCond;
                                      sres:pSceSaveDataDirNameSearchResult):Integer;
{
var
 ROut:TRawByteSearchRec;
 S,F:RawByteString;
 List:TStringList;
 i,n:Integer;
 }
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;

 if (cond=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 if (sres=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 Case cond^.order of
  SDSO_ASCENT :;
  SDSO_DESCENT:;
  else
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 sres^.setNum:=0;

 //Assert(cond^.key  =SCE_SAVE_DATA_SORT_KEY_DIRNAME);

 {
 s:=IncludeTrailingPathDelimiter(ps4_app.save_path)+_convert_dir_name_search(Pchar(cond^.dirName));

 _sig_lock;

 ROut:=Default(TRawByteSearchRec);
 if (FindFirst(s,faDirectory,ROut)=0) then
 begin
  List:=TStringList.Create;
  repeat
   if (ROut.FindData.dwFileAttributes and faDirectory)=faDirectory then
   begin
    F:=UTF8Encode(WideString(ROut.FindData.cFileName));
    Case F of
     '.','..':;
     else
      List.Add(F);
    end;
   end;
  until (FindNext(ROut)<>0);
  FindClose(ROut);

  sres^.hitNum:=List.Count;
  if (List.Count<>0) and (sres^.dirNamesNum<>0) then
  begin
   Case cond^.order of
    SCE_SAVE_DATA_SORT_ORDER_ASCENT :List.CustomSort(@StringListAscCompare);
    SCE_SAVE_DATA_SORT_ORDER_DESCENT:List.CustomSort(@StringListDscCompare);
    else;
   end;

   n:=List.Count;
   if (n>sres^.dirNamesNum) then n:=sres^.dirNamesNum;

   sres^.setNum:=n;

   For i:=0 to n-1 do
   begin
    s:=List[i];

    if (sres^.dirNames<>nil) then
    begin
     sres^.dirNames[i]:=Default(SceSaveDataDirName);
     MoveChar0(PChar(s)^,sres^.dirNames[i],SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
    end;

    if (sres^.params<>nil) then
    begin
     sres^.params[i]:=Default(SceSaveDataParam);
    end;

    if (sres^.infos<>nil) then
    begin
     sres^.infos[i]:=Default(SceSaveDataSearchInfo);
     sres^.infos[i].blocks    :=100000;
     sres^.infos[i].freeBlocks:=100000;
    end;

   end;

  end else
  begin
   sres^.setNum:=0;
  end;

  FreeAndNil(List);

 end;

 _sig_unlock;
 }

end;

function ps4_sceSaveDataGetParam(mountPoint  :pSceSaveDataMountPoint;
                                 paramType   :SceSaveDataParamType;
                                 paramBuf    :Pointer;
                                 paramBufSize:QWORD;
                                 gotSize     :PQWORD
                                ):Integer;
var
 slot_id:Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 Result:=CheckGetParamData(paramType,paramBuf,paramBufSize);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.GetParam(slot_id,paramType,paramBuf,paramBufSize,gotSize);

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataSetParam(mountPoint  :pSceSaveDataMountPoint;
                                 paramType   :SceSaveDataParamType;
                                 paramBuf    :Pointer;
                                 paramBufSize:QWORD):Integer;
var
 slot_id:Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 Result:=CheckSetDataParam(paramType,paramBuf,paramBufSize);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.SetParam(slot_id,paramType,paramBuf,paramBufSize);

 mtx_unlock(g_instance.mtx);
end;

//Save icon
function ps4_sceSaveDataSaveIcon(mountPoint:pSceSaveDataMountPoint;
                                 param:pSceSaveDataIcon):Integer;
var
 slot_id:Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 Result:=CheckSaveSaveDataIcon(param);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.SaveIcon(slot_id,param);

 mtx_unlock(g_instance.mtx);
end;

//Load icon
function ps4_sceSaveDataLoadIcon(mountPoint:pSceSaveDataMountPoint;
                                 param:pSceSaveDataIcon):Integer;
var
 slot_id:Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(mountPoint),slot_id);
 if (Result<>0) then Exit;

 Result:=CheckLoadSaveDataIcon(param);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.LoadIcon(slot_id,param);

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataRegisterEventCallback(cb:SceSaveDataEventCallbackFunc;userdata:Pointer):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (cb=nil) or (g_instance.version=VERSION_INIT_3) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 mtx_lock(g_instance.mtx);

  g_instance.cb_event   :=cb;
  g_instance.cb_userdata:=userdata;

 mtx_unlock(g_instance.mtx);

 Result:=0;
end;

function ps4_sceSaveDataUnregisterEventCallback(cb:SceSaveDataEventCallbackFunc):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (cb=nil) or (g_instance.version=VERSION_INIT_3) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 Result:=SCE_SAVE_DATA_ERROR_NOT_REGIST_CALLBACK;

 mtx_lock(g_instance.mtx);

   if (g_instance.cb_event=cb) then
   begin
    g_instance.cb_event   :=nil;
    g_instance.cb_userdata:=nil;
    Result:=0;
   end;

 mtx_unlock(g_instance.mtx);
end;

//sceSaveDataBackup()
//sceSaveDataUmountWithBackup()
//sceSaveDataSyncSaveDataMemory()
function ps4_sceSaveDataGetEventResult(param:pSceSaveDataEventParam;
                                       event:pSceSaveDataEvent):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (g_instance.version<>VERSION_INIT_3) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if (event=nil) or (param<>nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.GetEventResult(event);

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataClearProgress():Integer;
begin
 //Сlearing the progress value for:
 //sceSaveDataMount2()
 //sceSaveDataDelete()
 //sceSaveDataRestoreBackupData()
 //sceSaveDataGetProgress()

 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=0;
end;

function ps4_sceSaveDataGetProgress(progress:PSingle):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (progress=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 progress^:=0;

 Result:=0;
end;

function ps4_sceSaveDataBackup(backup:pSceSaveDataBackup):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckSaveDataBackup(backup);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.DoBackup(backup);

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 if (g_instance.job_thread<>nil) then
 begin
  g_instance.job_list.SendEventJob();
 end;
end;

function ps4_sceSaveDataCheckBackupData(check:pSceSaveDataCheckBackupData):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckCheckBackupData(check,False);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.CheckBackup(check);

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataRestoreBackupData(restore:pSceSaveDataRestoreBackupData):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckRestoreBackupData(restore);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.RestoreBackup(restore);

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 begin
  if (p_proc.p_sdk_version < $3500000) then
  begin
   restore^.progress:=100;
  end else
  begin
   //
  end;
 end;

end;

procedure init_save;
begin
 //backup.queue.Create(32);
end;

function Load_libSceSaveData(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
 module:TMODULE;
begin
 Result:=obj_new_int('libSceSaveData');

 lib:=Result^.add_lib('libSceSaveData');
 lib.set_proc($664661B2408F5C5C,@ps4_sceSaveDataInitialize);
 lib.set_proc($9753660DE0E93465,@ps4_sceSaveDataInitialize2);
 lib.set_proc($4F2C2B14A0A82C66,@ps4_sceSaveDataInitialize3);
 lib.set_proc($C8A0F2F12E722C0D,@ps4_sceSaveDataTerminate);
 lib.set_proc($BFB00000CA342F3E,@ps4_sceSaveDataSetupSaveDataMemory);
 lib.set_proc($A10C921147E05D10,@ps4_sceSaveDataSetupSaveDataMemory2);
 lib.set_proc($EC1B79A410BF01CA,@ps4_sceSaveDataGetSaveDataMemory);
 lib.set_proc($43038EEEF7A09D5F,@ps4_sceSaveDataGetSaveDataMemory2);
 lib.set_proc($8776144735C64954,@ps4_sceSaveDataSetSaveDataMemory);
 lib.set_proc($71DBB2F6FE18993E,@ps4_sceSaveDataSetSaveDataMemory2);
 lib.set_proc($C224FD8DE0BBC4FC,@ps4_sceSaveDataSyncSaveDataMemory);
 lib.set_proc($4B51A478F235EF34,@ps4_sceSaveDataDelete);
 lib.set_proc($DF61D0010770336A,@ps4_sceSaveDataMount);
 lib.set_proc($D33E393C81FE48D2,@ps4_sceSaveDataMount2);
 lib.set_proc($580CD64D99B51FE2,@ps4_sceSaveDataTransferringMount);
 lib.set_proc($04C47817F51E9371,@ps4_sceSaveDataUmount);
 lib.set_proc($57069DC0104127CD,@ps4_sceSaveDataUmountWithBackup);
 lib.set_proc($EB9547D1069ACFAB,@ps4_sceSaveDataGetMountInfo);
 lib.set_proc($7722219D7ABFD123,@ps4_sceSaveDataDirNameSearch);
 lib.set_proc($5E0BD2B88767325C,@ps4_sceSaveDataGetParam);
 lib.set_proc($F39CEE97FFDE197B,@ps4_sceSaveDataSetParam);
 lib.set_proc($73CF18CB9E0CC74C,@ps4_sceSaveDataSaveIcon);
 lib.set_proc($7068CEDF0337576F,@ps4_sceSaveDataLoadIcon);
 lib.set_proc($86C29DE5CDB5B107,@ps4_sceSaveDataRegisterEventCallback);
 lib.set_proc($BFF00AD40C50852D,@ps4_sceSaveDataUnregisterEventCallback);
 lib.set_proc($8FCC4AB62163D126,@ps4_sceSaveDataGetEventResult);
 lib.set_proc($5B3FF82597DE3BD8,@ps4_sceSaveDataClearProgress);
 lib.set_proc($00D9925948B2C864,@ps4_sceSaveDataGetProgress);
 lib.set_proc($CF5240F3F889B779,@ps4_sceSaveDataBackup);
 lib.set_proc($4503AA0DB9376D25,@ps4_sceSaveDataCheckBackupData);
 lib.set_proc($954F58445B20C125,@ps4_sceSaveDataRestoreBackupData);

 //
 lib.add_func(@ps4_job_thread,@job_thread).Argc(1);
 //

 module:=Result^.add_mod('libkernel',1);
 lib:=module.add_lib('libkernel');

 lib.set_proc($9EC628351CB0C0D8,@ps4_scePthreadAttrInit           );
 lib.set_proc($EB6282C04326CDC3,@ps4_scePthreadAttrDestroy        );
 lib.set_proc($5135F325B5A18531,@ps4_scePthreadAttrSetstacksize   );
 lib.set_proc($DEAC603387B31130,@ps4_scePthreadAttrSetaffinity    );
 lib.set_proc($7976D44A911A4EC0,@ps4_scePthreadAttrSetinheritsched);
 lib.set_proc($E3E87D133C0A1782,@ps4_scePthreadAttrSetschedpolicy );
 lib.set_proc($0F3112F61405E1FE,@ps4_scePthreadAttrSetschedparam  );
 lib.set_proc($E9482DC15FB4CDBE,@ps4_scePthreadCreate             );
 lib.set_proc($A27358F41CA7FD6F,@ps4_scePthreadJoin               );

 //init_save;
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceSaveData.prx',@Load_libSceSaveData);

end.

