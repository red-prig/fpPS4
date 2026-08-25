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
 vm,
 vmparam,
 vm_map,
 vm_mmap,
 vm_object,
 game_mount,
 vfs_mountroot,
 ps4_libSceUserService,
 md_systm,
 sys_bootparam,
 host_ipc;

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
  procedure   Stop;
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
  function  SendProgressJob(p_progress:PInteger):TProgressJob;
  procedure Action;
 end;

 PIconBufSize=^TIconBufSize;
 TIconBufSize=packed record //64
  max:QWORD;
  cur:QWORD;
  reserved:array[0..5] of QWORD;
 end;

 PSdMemoryBuffer=^TSdMemoryBuffer;
 TSdMemoryBuffer=object
  //shm
  Paddr:Pointer;
  Fsize:QWORD;
  //areas
  PmemoryData    :Pointer;
  FmemorySize    :QWORD;
  //
  PiconMemorySize:PIconBufSize;
  PiconData      :Pointer;
  //
  PParamData     :pSceSaveDataParam;
  //
  function  mmap_shm(mmapAddr:Pointer;MemoryBudget:Integer;size:QWORD):Integer;
  Procedure Free;
  function  CreateShm(mmapAddr      :Pointer;
                      MemoryBudget  :Integer;
                      memorySize    :DWORD;
                      iconMemorySize:DWORD;
                      paramSize     :DWORD):Integer;
 end;

 PPerSdSlot=^TPerSdSlot;
 TPerSdSlot=object
  is_setup     :Boolean;
  FslotId      :Byte;
  FbufferNum   :Byte;
  FbufferId    :Byte;
  FMemoryBudget:Byte;
  sd_buffers   :array[0..1] of TSdMemoryBuffer;
  //
  procedure Free;
  function  CreateBuffers(bufferNum     :Integer;
                          mmapAddr      :Pointer;
                          MemoryBudget  :Integer;
                          memorySize    :DWORD;
                          iconMemorySize:DWORD;
                          paramSize     :DWORD):Integer;
  function  ReadMemoryData(user_id:DWORD;p_existedMemorySize:PQWORD):Integer;
 end;

 PPerUserInfo=^TPerUserInfo;
 TPerUserInfo=object
  userId           :DWORD;
  shm_size_game    :DWORD;
  shm_size_shell   :DWORD;
  sd_slot          :array[0..3] of TPerSdSlot;
  is_setup         :Boolean;
  UNLOCK_LIMITATION:Boolean;
  //
  procedure Free;
  function  get_slot_node(slotId:DWORD):PPerSdSlot;
  procedure apply_memory_setup(slot_node:PPerSdSlot;option:DWORD);
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
  users:array[0..3] of TPerUserInfo;
  //
  function  get_sum_shm_size(userId:DWORD):DWORD;
  function  get_user_node(userId:DWORD):PPerUserInfo;
  procedure free_user(userId:Integer);
  function  InitInstance(params:Pointer;_version:t_init_version):Integer;
  procedure select_prio_by_cusaname;
  procedure InitJobThread;
  procedure JoinThread;
  function  ConnectInstance:Integer;
  procedure Terminate;
 end;

function TSaveDataInstance.get_sum_shm_size(userId:DWORD):DWORD;
var
 i:Integer;
begin
 Result:=0;
 For i:=0 to High(users) do
  if users[i].is_setup and (users[i].userId=userId) then
  begin
   Exit(users[i].shm_size_game + users[i].shm_size_shell);
  end;
end;

function TSaveDataInstance.get_user_node(userId:DWORD):PPerUserInfo;
var
 i:Integer;
begin
 Result:=nil;
 For i:=0 to High(users) do
  if users[i].is_setup and (users[i].userId=userId) then
  begin
   Exit(@users[i]);
  end;
 For i:=0 to High(users) do
  if not users[i].is_setup then
  begin
   users[i].userId:=userId;
   Exit(@users[i]);
  end;
end;

procedure TSaveDataInstance.free_user(userId:Integer);
var
 i:Integer;
begin
 For i:=0 to High(users) do
  if (userId=-1) or (users[i].is_setup and (users[i].userId=userId)) then
  begin
   users[i].Free;
  end;
end;

procedure TPerUserInfo.Free;
var
 i:Integer;
begin
 userId           :=0;
 shm_size_game    :=0;
 shm_size_shell   :=0;
 is_setup         :=False;
 UNLOCK_LIMITATION:=False;
 //destroy
 for i:=0 to High(sd_slot) do
 begin
  sd_slot[i].Free;
 end;
end;

function TPerUserInfo.get_slot_node(slotId:DWORD):PPerSdSlot;
begin
 Result:=@sd_slot[slotId];
 Result^.FslotId:=slotId;
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

function TSaveDataInstance.InitInstance(params:Pointer;_version:t_init_version):Integer;
begin
 mtx_init(mtx,'SaveDataInstance');
 //

 version        :=_version;
 priority       :=700;
 threadStackSize:=$4000;
 cpuAffinityMask:=0;

 case version of
  VERSION_INIT_0:
   begin
    if (params=nil) then Exit(0);

    if (p_proc.p_sdk_version >= $2000000) then
    begin
     Result:=CheckDataInitParams1(params);
     if (Result=0) then
     begin
      priority            :=pSceSaveDataInitParams(params)^.priority;
      not_prio_by_cusaname:=true;
     end;
    end else
    begin
     Result:=CheckDataInitParams0(params);
     if (Result=0) then
     begin
      priority            :=pSceSaveDataInitParams(params)^.priority;
      force_default_prio  :=(pSceSaveDataInitParams(params)^.reserved[0]<>0);
      not_prio_by_cusaname:=true;
     end;
    end;

   end;
  VERSION_INIT_2:
   begin
    Result:=CheckDataInitParams2(params);
    if (Result=0) then
    begin
     priority            :=pSceSaveDataInitParams2(params)^.priority;
     not_prio_by_cusaname:=true;
     threadStackSize     :=pSceSaveDataInitParams2(params)^.threadStackSize;
     cpuAffinityMask     :=pSceSaveDataInitParams2(params)^.cpuAffinityMask;
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

procedure TSaveDataInstance.select_prio_by_cusaname;
var
 sched_param:t_sched_param;
begin
 if (p_proc.p_sdk_version < $2000000) and
    (force_default_prio=false) then
 begin
  priority:=700;
 end;

 if (not_prio_by_cusaname=false) then
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
       priority:=sched_param.sched_priority;
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

procedure TCustomCommand.Stop;
begin
 finish:=True;
end;

procedure TCustomCommand.Run;
begin
 //
end;

///

procedure TProgressJob.Run;
var
 err:Integer;
 progres:Single;
begin
 if (g_instance=nil) then
 begin
  Stop;
  Exit;
 end;

 err:=g_instance.Backend.GetProgress(@progres);

 if (err=0) then
 begin

  if (p_progress <> nil) then
  begin
   p_progress^:=Trunc(progres*100);
  end;

  if (p_progress <> nil) and
     (p_progress^ = 100) then
  begin
   Stop;
  end;

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
  Stop;
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

 Stop;
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

function TJobList.SendProgressJob(p_progress:PInteger):TProgressJob;
var
 cmd:TProgressJob;
begin
 cmd:=TProgressJob.Create;
 cmd.p_progress:=p_progress;
 cmd.inc_ref;
 SendCmd(cmd);
 Result:=cmd;
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

procedure TSaveDataInstance.InitJobThread;
const
 ThreadName='SceSaveData'#0;
var
 ga:TGUEST_STACK;
 p_attr       :p_pthread_attr_t;
 p_pthread    :p_pthread_t;
 p_policy     :PInteger;
 p_thread_name:PChar;
begin
 job_list.Init;
 thread_stop:=False;
 job_thread :=nil;

 ga:=prolog;

 p_attr       :=ga.alloca(SizeOf(Pointer));
 p_pthread    :=ga.alloca(SizeOf(pthread_t));
 p_policy     :=ga.alloca(SizeOf(Integer));
 p_thread_name:=ga.alloca(Length(ThreadName));

 p_attr^:=nil;
 StrPCopy(p_thread_name,ThreadName);

 ps4_scePthreadAttrInit(p_attr);
 ps4_scePthreadAttrSetstacksize(p_attr,threadStackSize);
 ps4_scePthreadAttrSetschedpolicy(p_attr,2);

 if (priority <> 0) then
 begin
  p_policy^:=priority;
  ps4_scePthreadAttrSetschedparam(p_attr,p_policy);
 end;

 if (cpuAffinityMask <> 0) then
 begin
  ps4_scePthreadAttrSetaffinity(p_attr,cpuAffinityMask);
 end;

 ps4_scePthreadCreate(p_pthread,p_attr,ps4_job_thread,nil,p_thread_name);

 job_thread:=p_pthread^;

 ps4_scePthreadAttrDestroy(p_attr);

 ga.epilog;

 Assert(job_thread<>nil);
end;

procedure TSaveDataInstance.JoinThread;
begin
 if (job_thread<>nil) then
 begin
  thread_stop:=True;
  ev_signal(job_list.signal);
  //
  ps4_scePthreadJoin(job_thread,nil);
  job_thread:=nil;
  //
  job_list.Fini;
 end;
end;

function OpenSaveDataBackend():THandle;
var
 Value :TIpcValue;
 data  :TPipeSend;
 parent:THandle;
begin
 data:=Default(TPipeSend);

 Value:=p_host_ipc.InvokeSync('OpenSaveDataBackend');
 Value.MoveTo(@data,sizeof(data));
 Value.Free;

 if (data.parent_pid=-1) then Exit(data.parent_pid);

 parent:=md_pidfd_open(data.parent_pid);

 Result:=md_pidfd_getfd(parent,data.pipe_fd);

 md_pidfd_close(parent);
end;

function TSaveDataInstance.ConnectInstance:Integer;
var
 pipefd:THandle;
begin
 Result:=0;

 if (version=VERSION_INIT_3) then
 begin

  if (p_proc.p_sdk_version < $6500000) then
  begin

   if (
       g_appinfo.titleWorkaround.ids[0] and
       (QWORD(1) shl BUG180029_SAVE_DATA_MEMORY_TIMEOUT_10SEC)
      )<>0 then
   begin
    memory_timeout_10sec:=True;
   end;

  end else
  begin
   memory_timeout_10sec:=True;
  end;

 end else
 begin
  select_prio_by_cusaname;

  InitJobThread;
 end;

 pipefd:=OpenSaveDataBackend();
 if (Int64(pipefd)=-1) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Backend:=TSaveDataBackendConnect.CreateClient(THostIpcConnect(p_host_ipc).Dispatcher,pipefd);
 Backend.SendMountConfig(GameMountConfigExport);
end;

function CreateSaveDataInstance(params:Pointer;version:t_init_version):Integer;
var
 instance:TSaveDataInstance;
begin
 if (g_instance<>nil) then Exit(0);

 instance:=TSaveDataInstance.Create;
 Result:=instance.InitInstance(params,version);
 g_instance:=instance;

 if (Result<0) then
 begin
  g_instance.Free;
  g_instance:=nil;
  Exit;
 end;

 Result:=g_instance.ConnectInstance;

 if (Result<0) then
 begin
  g_instance.Free;
  g_instance:=nil;
  Exit;
 end;
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
 JoinThread;
 free_user(-1);
 if (Backend<>nil) then
 begin
  Backend.ExitClient;
  Backend.UmountAllForce;
  Backend.Free;
 end;
 Free;
end;

function ps4_sceSaveDataTerminate:Integer;
begin
 if (g_instance<>nil) then
 begin

  if (p_proc.p_sdk_version > $3ffffff) then
  if (g_instance.Backend<>nil) then
  if (g_instance.Backend.MountSlots<>0) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_BUSY);
  end;

  g_instance.Terminate;
  g_instance:=nil;
  Exit(0);
 end;
 Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
end;

const
 SHM_SHELL=0;
 SHM_GAME =1;

function GetMemoryBudget(user_node:PPerUserInfo;memorySize:QWORD;option:DWORD):Integer;
var
 sum:QWORD;
begin
 if ((option and SDMO_UNLOCK_LIMITATION)=0) then
 begin
  Result:=ord(($80000 + (ord((option and SDMO_DOUBLE_BUFFER)=0) * $80000)) < memorySize);
 end else
 begin
  sum:=user_node^.shm_size_shell;

  if ((option and SDMO_DOUBLE_BUFFER)=0) then
  begin
   sum:=sum+memorySize;
  end else
  begin
   sum:=sum+memorySize*2;
  end;

  Result:=ord($400000 < sum);
 end;
end;

function TSdMemoryBuffer.mmap_shm(mmapAddr:Pointer;MemoryBudget:Integer;size:QWORD):Integer;
var
 map:vm_map_t;
begin
 map:=p_proc.p_vmspace;

 //create psevdo shm

 if (MemoryBudget=SHM_SHELL) then
 begin
  Result:=vm_mmap2(map,@mmapAddr,size,3,3,MAP_ANON or MAP_PRIVATE or MAP_SYSTEM,OBJT_DEFAULT,nil,0,nil);
 end else
 begin
  Result:=vm_mmap2(map,@mmapAddr,size,3,3,MAP_ANON or MAP_PRIVATE,OBJT_DEFAULT,nil,0,nil);
 end;

 if (Result=0) then
 begin
  Paddr:=mmapAddr;
  Fsize:=size;
 end;

end;

Procedure TSdMemoryBuffer.Free;
begin
 if (Paddr<>nil) then
 begin
  sys_munmap(Paddr,Fsize);
  self:=Default(TSdMemoryBuffer);
 end;
end;

function TSdMemoryBuffer.CreateShm(mmapAddr      :Pointer;
                                   MemoryBudget  :Integer;
                                   memorySize    :DWORD;
                                   iconMemorySize:DWORD;
                                   paramSize     :DWORD):Integer;
var
 size:QWORD;
 err:Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;

 if (memorySize < $2000001) and
    (iconMemorySize < $1c801) and
    (paramSize < $531) then
 begin

  if (iconMemorySize=0) then
  begin
   size:=0;
  end else
  begin
   size:=iconMemorySize + 64;
  end;
  size:=size + paramSize + memorySize;

  err:=mmap_shm(mmapAddr,MemoryBudget,size);
  if (err<>0) then Exit(SCE_SAVE_DATA_ERROR_OUT_OF_MEMORY);

  mmapAddr:=Paddr;

  PmemoryData:=mmapAddr;
  FmemorySize:=memorySize;

  if (iconMemorySize<>0) then
  begin
   PiconMemorySize:=(mmapAddr + memorySize);
   PiconData      :=(PiconMemorySize + 1);
   //
   PiconMemorySize^:=Default(TIconBufSize);
   PiconMemorySize^.max:=iconMemorySize;
  end;

  if (paramSize<>0) then
  begin
   mmapAddr:=PmemoryData;
   size    :=FmemorySize;
   if (iconMemorySize<>0) then
   begin
    mmapAddr:=PiconData;
    size    :=iconMemorySize;
   end;
   PParamData:=(mmapAddr + size);
  end;

  Result:=0;
 end;

end;

procedure TPerSdSlot.Free;
var
 i:Integer;
begin
 is_setup     :=False;
 FbufferNum   :=0;
 FbufferId    :=0;
 FMemoryBudget:=0;
 //destroy
 for i:=0 to High(sd_buffers) do
 begin
  sd_buffers[i].Free;
 end;
end;

function TPerSdSlot.CreateBuffers(bufferNum     :Integer;
                                  mmapAddr      :Pointer;
                                  MemoryBudget  :Integer;
                                  memorySize    :DWORD;
                                  iconMemorySize:DWORD;
                                  paramSize     :DWORD):Integer;
var
 i:Integer;
begin
 if (bufferNum<>1) and (bufferNum<>2) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 for i:=0 to bufferNum-1 do
 begin
  Result:=sd_buffers[i].CreateShm(mmapAddr,MemoryBudget,memorySize,iconMemorySize,paramSize);
  if (Result<>0) then Exit;
 end;

 FbufferNum   :=bufferNum;
 FbufferId    :=0;
 FMemoryBudget:=MemoryBudget;
end;

procedure TPerUserInfo.apply_memory_setup(slot_node:PPerSdSlot;option:DWORD);
var
 i:Integer;
begin
 if ((option and SDMO_UNLOCK_LIMITATION)<>0) then
 begin
  UNLOCK_LIMITATION:=true;
 end;

 for i:=0 to slot_node^.FbufferNum-1 do
 begin

  if (slot_node^.FMemoryBudget=SHM_SHELL) then
  begin
   shm_size_shell:=shm_size_shell + slot_node^.sd_buffers[i].FmemorySize;
  end else
  begin
   shm_size_game :=shm_size_game  + slot_node^.sd_buffers[i].FmemorySize;
  end;

 end;

 slot_node^.is_setup:=True;
 is_setup:=True;
end;

function TPerSdSlot.ReadMemoryData(user_id:DWORD;p_existedMemorySize:PQWORD):Integer;
label
 __end;
var
 data:record
  case Byte of
   0:(minfo:SceSaveDataMount);
   1:(icon :SceSaveDataIcon);
 end;
 mresult:SceSaveDataMountResult;
 i      :Integer;
 slot_id:Integer;
begin
 FillChar(data,SizeOf(data),0);
 FillChar(mresult,SizeOf(mresult),0);

 data.minfo.userId   :=user_id;
 data.minfo.dirName  :=pSceSaveDataDirName(sdmemory_slot_name[FslotId]);
 data.minfo.blocks   :=96;
 data.minfo.mountMode:=SDMM_RDONLY;

 Result:=g_instance.Backend.DoMount(@data.minfo,@mresult,False,True);
 if (Result<>0) then Exit;

 slot_id:=0;
 Result:=GetMountSlotIdByMountPoint(pchar(@mresult.mountPoint),slot_id);
 if (Result<>0) then Exit;

 if (FbufferNum>0) then
 For i:=0 to FbufferNum-1 do
 begin

  //   /memory.dat
  if (sd_buffers[i].PmemoryData<>nil) then
  begin
   Result:=g_instance.Backend.ReadMemory(slot_id,sd_buffers[i].PmemoryData,sd_buffers[i].FmemorySize,p_existedMemorySize);

   if (Result<>0) then goto __end;
  end;

  if (sd_buffers[i].PParamData<>nil) then
  begin
   Result:=g_instance.Backend.GetParam(slot_id,
                                       SCE_SAVE_DATA_PARAM_TYPE_ALL,
                                       sd_buffers[i].PParamData,
                                       $530,
                                       nil);

   if (Result<>0) then goto __end;
  end;

  if (sd_buffers[i].PiconData<>nil) then
  begin

   FillChar(data,SizeOf(data),0);

   data.icon.buf     :=sd_buffers[i].PiconData;
   data.icon.bufSize :=sd_buffers[i].PiconMemorySize^.max;
   data.icon.dataSize:=data.icon.bufSize;

   Result:=g_instance.Backend.LoadIcon(slot_id,@data.icon,True);

   if (Result<>0) then goto __end;

   sd_buffers[i].PiconMemorySize^.cur:=data.icon.dataSize;
  end;

 end; //for

 __end:
  Result:=g_instance.Backend.DoUmount(slot_id,False);
end;

function SetupSaveDataMemory2Lt65(setupParam:pSceSaveDataMemorySetup2;
                                  InitParam :pSceSaveDataParam;
                                  p_existedMemorySize:PQWORD):Integer;
var
 userId        :DWORD;
 memorySize    :QWORD;
 iconMemorySize:QWORD;
 paramSize     :QWORD;
 MemoryBudget  :Integer;
 mmapAddr      :Pointer;
 bufferNum     :Integer;

 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 userId     :=setupParam^.userId;
 memorySize :=setupParam^.memorySize;
 iconMemorySize:=setupParam^.iconMemorySize;
 paramSize  :=ord((setupParam^.option and SDMO_SET_PARAM)<>0) * $530;
 bufferNum  :=2 - ord((setupParam^.option and SDMO_DOUBLE_BUFFER)=0);

 user_node:=g_instance.get_user_node(userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if user_node^.UNLOCK_LIMITATION and ((setupParam^.option and SDMO_UNLOCK_LIMITATION)=0) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 slot_node:=user_node^.get_slot_node(0);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 if (p_proc.p_sdk_version < $2500000) then
 begin
  mmapAddr:=nil;
 end else
 begin
  mmapAddr:=Pointer($880000000);
 end;

 //SetupSaveDataMemory_0x22

 MemoryBudget:=GetMemoryBudget(user_node,memorySize,setupParam^.option);

 if (MemoryBudget=SHM_SHELL) then
 begin

  //if (params != NULL) {
  //  memcpy(&input.params,params,0x524);
  //}

  //CreateSharedMemory_0x21

  //SetupSaveDataMemory_0x22

  Result:=slot_node^.
          CreateBuffers(bufferNum     ,
                        mmapAddr      ,
                        MemoryBudget  ,
                        memorySize    ,
                        iconMemorySize,
                        paramSize     );
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  Result:=g_instance.Backend.SetupMemory(userId,0,bufferNum,memorySize,iconMemorySize,paramSize,InitParam);
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  //CreateSharedSemaphore_0x2f

  //CreateShmInternal

  user_node^.apply_memory_setup(slot_node,setupParam^.option);

 end else
 begin //SHM_GAME

  //if (params != NULL) {
  //  memcpy(&input.params,params,0x524);
  //}

  Result:=slot_node^.
          CreateBuffers(bufferNum     ,
                        mmapAddr      ,
                        MemoryBudget  ,
                        memorySize    ,
                        iconMemorySize,
                        paramSize     );
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  //CreateSharedMemory_0x21

  //CreateShmInternal

  Result:=g_instance.Backend.SetupMemory(userId,0,bufferNum,memorySize,iconMemorySize,paramSize,InitParam);
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  user_node^.apply_memory_setup(slot_node,setupParam^.option);

  //SetupSaveDataMemory_0x22

 end;

 slot_node^.ReadMemoryData(userId,p_existedMemorySize);

 Result:=0;
end;

function SetupSaveDataMemory2Be65(setupParam:pSceSaveDataMemorySetup2;
                                  p_existedMemorySize:PQWORD):Integer;
var
 userId        :DWORD;
 memorySize    :QWORD;
 iconMemorySize:QWORD;
 paramSize     :QWORD;
 MemoryBudget  :Integer;
 bufferNum     :Integer;

 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 userId     :=setupParam^.userId;
 memorySize :=setupParam^.memorySize;
 iconMemorySize:=setupParam^.iconMemorySize;
 paramSize  :=ord((setupParam^.option and SDMO_SET_PARAM)<>0) * $530;
 bufferNum  :=2 - ord((setupParam^.option and SDMO_DOUBLE_BUFFER)=0);

 Result:=CheckSdSlotId(setupParam^.slotId);
 if (Result<>0) then Exit;

 user_node:=g_instance.get_user_node(userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if user_node^.UNLOCK_LIMITATION and ((setupParam^.option and SDMO_UNLOCK_LIMITATION)=0) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 slot_node:=user_node^.get_slot_node(setupParam^.slotId);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_BUSY);
 end;

 //SetupSaveDataMemory_0x51

 MemoryBudget:=GetMemoryBudget(user_node,memorySize,setupParam^.option);

 if (MemoryBudget=SHM_SHELL) then
 begin

  //CreateSharedMemory_0x4e

  //SetupSaveDataMemory_0x51

  Result:=slot_node^.
          CreateBuffers(bufferNum     ,
                        Pointer($880000000),
                        MemoryBudget  ,
                        memorySize    ,
                        iconMemorySize,
                        paramSize     );
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  Result:=g_instance.Backend.SetupMemory(userId,setupParam^.slotId,bufferNum,memorySize,iconMemorySize,paramSize,nil);
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  //CreateSharedSemaphore_0x50

  //OpenSema

  //CreateShmInternal

  user_node^.apply_memory_setup(slot_node,setupParam^.option);

 end else
 begin //SHM_GAME

  Result:=slot_node^.
          CreateBuffers(bufferNum     ,
                        Pointer($880000000),
                        MemoryBudget  ,
                        memorySize    ,
                        iconMemorySize,
                        paramSize     );
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  Result:=g_instance.Backend.SetupMemory(userId,setupParam^.slotId,bufferNum,memorySize,iconMemorySize,paramSize,nil);
  if (Result<>0) then
  begin
   slot_node^.Free;
   Exit;
  end;

  //CreateSharedMemory_0x4e

  //CreateSharedSemaphore_0x50

  //SetupSaveDataMemory_0x51

  //OpenSema

  //CreateShmInternal

  user_node^.apply_memory_setup(slot_node,setupParam^.option);

 end;

 slot_node^.ReadMemoryData(userId,p_existedMemorySize);

 Result:=0;
end;

function ps4_sceSaveDataSetupSaveDataMemory(
           userId    :SceUserServiceUserId;
           memorySize:QWORD;
           param     :pSceSaveDataParam):Integer;
var
 info:SceSaveDataMemorySetup2;
 existedMemorySize:QWORD;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckSetupSaveDataParam(userId,memorySize,param);
 if (Result<>0) then Exit;

 info:=Default(SceSaveDataMemorySetup2);
 info.userId    :=userId;
 info.memorySize:=memorySize;

 existedMemorySize:=0;

 mtx_lock(g_instance.mtx);

  Result:=SetupSaveDataMemory2Lt65(@info,param,@existedMemorySize);

 mtx_unlock(g_instance.mtx)
end;

function SetSaveDataMemory2Lt65(setParam:pSceSaveDataMemorySet2):Integer; forward;
function SetSaveDataMemory2Be65(setParam:pSceSaveDataMemorySet2):Integer; forward;

function SetupSaveDataMemory2(
           setupParam:pSceSaveDataMemorySetup2;
           pResult   :pSceSaveDataMemorySetupResult):Integer;
var
 __setupParam:SceSaveDataMemorySetup2;
 existedMemorySize:QWORD;
 sum_sd_size:DWORD;
 sdm:SceSaveDataMemorySet2;
begin
 Result:=0;

 if (p_proc.p_sdk_version < $4500000) then
 begin
  sum_sd_size:=g_instance.get_sum_shm_size(setupParam^.userId);

  Result:=CheckSetupParam(setupParam,sum_sd_size);
  if (Result<>0) then Exit;

  if (pResult<>nil) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
  end;

  existedMemorySize:=0;
  Result:=SetupSaveDataMemory2Lt65(setupParam,nil,@existedMemorySize);
 end else
 begin
  //>=$4500000

  if (p_proc.p_sdk_version > $54fffff) then
  begin
   __setupParam:=setupParam^;
   __setupParam.option:=__setupParam.option or SDMO_UNLOCK_LIMITATION;
   setupParam:=@__setupParam;
  end;

  sum_sd_size:=g_instance.get_sum_shm_size(setupParam^.userId);

  Result:=CheckSetupParam(setupParam,sum_sd_size);
  if (Result<>0) then Exit;

  if (setupParam^.initParam<>nil) then
  begin
   if ((setupParam^.option and SDMO_SET_PARAM)=0) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;

   Result:=CheckSetDataParam(SCE_SAVE_DATA_PARAM_TYPE_ALL,setupParam^.initParam,$530);
   if (Result<>0) then Exit;
  end;

  if (setupParam^.initIcon<>nil) then
  begin
   if (setupParam^.iconMemorySize=0) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;

   Result:=CheckSaveSaveDataIcon(setupParam^.initIcon);
   if (Result<>0) then Exit;

   if (setupParam^.initIcon^.dataSize > setupParam^.iconMemorySize) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;
  end;

  if (pResult<>nil) then
  begin
   if not CheckReserved(pResult^.reserved,sizeof(pResult^.reserved)) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;
  end;

  if (p_proc.p_sdk_version < $6500000) then
  begin
   Result:=SetupSaveDataMemory2Lt65(setupParam,nil,@existedMemorySize);
  end else
  begin
   Result:=SetupSaveDataMemory2Be65(setupParam,@existedMemorySize);
  end;
  if (Result<>0) then Exit;

  if (pResult<>nil) then
  begin
   pResult^.existedMemorySize:=existedMemorySize;
  end;

  if ((setupParam^.initParam<>nil) or (setupParam^.initIcon<>nil)) and (existedMemorySize=0) then
  begin
   sdm:=Default(SceSaveDataMemorySet2);
   sdm.userId:=setupParam^.userId;
   sdm.slotId:=setupParam^.slotId;
   sdm.param :=setupParam^.initParam;
   sdm.icon  :=setupParam^.initIcon;

   if (p_proc.p_sdk_version < $6500000) then
   begin
    Result:=SetSaveDataMemory2Lt65(@sdm);
   end else
   begin
    Result:=SetSaveDataMemory2Be65(@sdm);
   end;

  end;

  //>=$4500000
 end;

end;

function ps4_sceSaveDataSetupSaveDataMemory2(
           setupParam:pSceSaveDataMemorySetup2;
           pResult   :pSceSaveDataMemorySetupResult):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (p_proc.p_sdk_version > $34fffff) and (setupParam=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 mtx_lock(g_instance.mtx);

  Result:=SetupSaveDataMemory2(setupParam,pResult);

 mtx_unlock(g_instance.mtx)
end;

///

function ReadFromBuf(src:PSdMemoryBuffer;dst:pSceSaveDataMemoryGet2):Integer;
var
 iconBufSize:QWORD;
 data:pSceSaveDataMemoryData;
 icon:pSceSaveDataIcon;
begin

 if (src^.PiconMemorySize=nil) then
 begin
  iconBufSize:=0;
 end else
 begin
  iconBufSize:=src^.PiconMemorySize^.max;
 end;

 Result:=CheckSaveDataMemoryRead(dst,src^.FmemorySize,src^.PParamData,src^.PiconData,iconBufSize);
 if (Result<>0) then Exit;

 data:=dst^.data;
 if (data<>nil) then
 begin
  Move((src^.PmemoryData+data^.offset)^,data^.buf^,data^.bufSize);
 end;

 icon:=dst^.icon;
 if (icon<>nil) then
 begin
  if (src^.PiconMemorySize=nil) then
  begin
   iconBufSize:=0;
  end else
  begin
   iconBufSize:=src^.PiconMemorySize^.cur;
   Move(src^.PiconData^,icon^.buf^,iconBufSize);
  end;
  icon^.dataSize:=iconBufSize;
 end;

 if (dst^.param<>nil) then
 begin
  Move(src^.PParamData^,dst^.param^,$530);
 end;

 Result:=0;
end;

///

function GetParams(slot_node:PPerSdSlot;dst:pSceSaveDataMemoryGet2):Integer;
begin
 if not slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
 end;

 Result:=ReadFromBuf(@slot_node^.sd_buffers[slot_node^.FbufferId],dst);
end;

///

function GetSaveDataMemory2Lt65(getParam:pSceSaveDataMemoryGet2):Integer;
var
 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 Result:=CheckSaveDataMemoryGet2Lt65(getParam);
 if (Result<>0) then Exit;

 user_node:=g_instance.get_user_node(getParam^.userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 slot_node:=user_node^.get_slot_node(0);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
 end;

 Result:=GetParams(slot_node,getParam);
end;

function GetSaveDataMemory2Be65(getParam:pSceSaveDataMemoryGet2):Integer;
var
 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 Result:=CheckSaveDataMemoryGet2Be65(getParam);
 if (Result<>0) then Exit;

 user_node:=g_instance.get_user_node(getParam^.userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 slot_node:=user_node^.get_slot_node(getParam^.slotId);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
 end;

 Result:=GetParams(slot_node,getParam);
end;

//

function ps4_sceSaveDataGetSaveDataMemory(
           userId :SceUserServiceUserId;
           buf    :Pointer;
           bufSize:QWORD;
           offset :QWORD):Integer;
var
 data:SceSaveDataMemoryData;
 info:SceSaveDataMemoryGet2;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (buf=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if (p_proc.p_sdk_version < $1750000) then
 begin
  offset:=0;
 end;

 data:=Default(SceSaveDataMemoryData);
 data.buf    :=buf;
 data.bufSize:=bufSize;
 data.offset :=offset;

 info:=Default(SceSaveDataMemoryGet2);
 info.userId:=userId;
 info.data  :=@data;

 mtx_lock(g_instance.mtx);

  Result:=GetSaveDataMemory2Lt65(@info);

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataGetSaveDataMemory2(getParam:pSceSaveDataMemoryGet2):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 mtx_lock(g_instance.mtx);

  if (p_proc.p_sdk_version < $6500000) then
  begin
   Result:=GetSaveDataMemory2Lt65(getParam);
  end else
  begin
   Result:=GetSaveDataMemory2Be65(getParam);
  end;

 mtx_unlock(g_instance.mtx);
end;

///

function WriteToBuf(dst:PSdMemoryBuffer;src:pSceSaveDataMemorySet2):Integer;
var
 iconBufSize:QWORD;
 data:pSceSaveDataMemoryData;
 icon:pSceSaveDataIcon;
begin

 if (dst^.PiconMemorySize=nil) then
 begin
  iconBufSize:=0;
 end else
 begin
  iconBufSize:=dst^.PiconMemorySize^.max;
 end;

 Result:=CheckSaveDataMemoryWrite(src,dst^.FmemorySize,dst^.PParamData,dst^.PiconData,iconBufSize);
 if (Result<>0) then Exit;

 data:=src^.data;
 if (data<>nil) then
 begin
  Move(data^.buf^,(dst^.PmemoryData+data^.offset)^,data^.bufSize);
 end;

 icon:=src^.icon;
 if (icon<>nil) and (dst^.PiconMemorySize<>nil) then
 begin
  Move(icon^.buf^,dst^.PiconData^,icon^.dataSize);
  dst^.PiconMemorySize^.cur:=src^.icon^.dataSize;
 end;

 if (src^.param<>nil) then
 begin
  Move(src^.param^,dst^.PParamData^,$530);
 end;

 Result:=0;
end;

function WriteToBufv(dst:PSdMemoryBuffer;src:pSceSaveDataMemorySet2):Integer;
var
 iconBufSize:QWORD;
 data:pSceSaveDataMemoryData;
 icon:pSceSaveDataIcon;
 md:SceSaveDataMemoryData;
 i:Integer;
begin

 if (dst^.PiconMemorySize=nil) then
 begin
  iconBufSize:=0;
 end else
 begin
  iconBufSize:=dst^.PiconMemorySize^.max;
 end;

 Result:=CheckSaveDataMemoryWritev(src,dst^.PParamData,dst^.PiconData,iconBufSize);
 if (Result<>0) then Exit;

 data:=src^.data;
 if (data<>nil) and (src^.dataNum<>0) then
 begin

  For i:=0 to src^.dataNum-1 do
  begin
   md:=data[i];
   Result:=CheckSaveDataMemoryDatav(@md,dst^.FmemorySize);
   if (Result<>0) then Exit;

   Move(md.buf^,(dst^.PmemoryData+md.offset)^,md.bufSize);
  end;

 end;

 icon:=src^.icon;
 if (icon<>nil) and (dst^.PiconMemorySize<>nil) then
 begin
  Move(icon^.buf^,dst^.PiconData^,icon^.dataSize);
  dst^.PiconMemorySize^.cur:=src^.icon^.dataSize;
 end;

 if (src^.param<>nil) then
 begin
  Move(src^.param^,dst^.PParamData^,$530);
 end;

 Result:=0;
end;

procedure CopyBuf(dst,src:PSdMemoryBuffer);
var
 max:QWORD;
begin
 if (src^.PmemoryData=nil) then
 begin
  dst^.PmemoryData:=nil;
 end else
 begin
  Move(src^.PmemoryData^,dst^.PmemoryData^,src^.FmemorySize);
 end;

 dst^.FmemorySize:=src^.FmemorySize;

 if (src^.PiconMemorySize=nil) then
 begin
  dst^.PiconMemorySize:=nil;
 end else
 begin
  dst^.PiconMemorySize^:=src^.PiconMemorySize^;
 end;

 if (src^.PiconData=nil) then
 begin
  dst^.PiconData:=nil;
 end else
 begin

  if (src^.PiconMemorySize=nil) then
  begin
   max:=0;
  end else
  begin
   max:=src^.PiconMemorySize^.max;
  end;

  Move(src^.PiconMemorySize^,dst^.PiconMemorySize^,max);
 end;

 if (src^.PParamData=nil) then
 begin
  dst^.PParamData:=nil;
 end else
 begin
  Move(src^.PParamData^,dst^.PParamData^,$530);
 end;

end;

///

function SetParamsLt40(user_node:PPerUserInfo;slot_node:PPerSdSlot;src:pSceSaveDataMemorySet2):Integer;
var
 data:pSceSaveDataMemoryData;
 buffer_id:Integer;
begin
 if not slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
 end;

 if (slot_node^.FbufferNum < 2) then
 begin
  buffer_id:=0;
 end else
 begin
  buffer_id:=slot_node^.FbufferId xor 1;
  slot_node^.FbufferId:=buffer_id;

  data:=src^.data;

  if (data<>nil) then
  begin
   if (data^.offset<>0) or
      (data^.bufSize<>slot_node^.sd_buffers[buffer_id].FmemorySize) then
   begin
    Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
   end;
  end;

 end;

 Result:=WriteToBuf(@slot_node^.sd_buffers[buffer_id],src);

 g_instance.Backend.WriteMemory(user_node^.userId,
                                slot_node^.FslotId,
                                buffer_id,
                                slot_node^.sd_buffers[buffer_id].Paddr,
                                slot_node^.sd_buffers[buffer_id].Fsize);
end;

function SetParamsBe40(user_node:PPerUserInfo;slot_node:PPerSdSlot;src:pSceSaveDataMemorySet2):Integer;
var
 buffer_id:Integer;
begin
 if not slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
 end;

 if (slot_node^.FbufferNum < 2) then
 begin
  if (src^.dataNum = 0) then
  begin
   Result:=WriteToBuf(@slot_node^.sd_buffers[0],src);
  end else
  begin
   Result:=WriteToBufv(@slot_node^.sd_buffers[0],src);
  end;

  buffer_id:=0;
 end else
 begin
  buffer_id:=slot_node^.FbufferId;

  if (src^.data<>nil) and
     (src^.data^.bufSize <> slot_node^.sd_buffers[buffer_id].FmemorySize) then
  begin
   CopyBuf(@slot_node^.sd_buffers[buffer_id xor 1],@slot_node^.sd_buffers[buffer_id]);
  end;

  buffer_id:=buffer_id xor 1;

  if (src^.dataNum = 0) then
  begin
   Result:=WriteToBuf(@slot_node^.sd_buffers[buffer_id],src);
  end else
  begin
   Result:=WriteToBufv(@slot_node^.sd_buffers[buffer_id],src);
  end;

  if (Result=0) then
  begin
   slot_node^.FbufferId:=buffer_id;
  end;
 end;

 g_instance.Backend.WriteMemory(user_node^.userId,
                                slot_node^.FslotId,
                                buffer_id,
                                slot_node^.sd_buffers[buffer_id].Paddr,
                                slot_node^.sd_buffers[buffer_id].Fsize);
end;

function SetSaveDataMemory2Lt65(setParam:pSceSaveDataMemorySet2):Integer;
var
 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 Result:=CheckSaveDataMemorySet2Lt65(setParam);
 if (Result<>0) then Exit;

 user_node:=g_instance.get_user_node(setParam^.userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 slot_node:=user_node^.get_slot_node(0);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if not slot_node^.is_setup then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
 end;

 if (p_proc.p_sdk_version < $4000000) then
 begin
  Result:=SetParamsLt40(user_node,slot_node,setParam);
 end else
 begin
  Result:=SetParamsBe40(user_node,slot_node,setParam);
 end;

end;

function SetSaveDataMemory2Be65(setParam:pSceSaveDataMemorySet2):Integer;
var
 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 Result:=CheckSaveDataMemorySet2Be65(setParam);
 if (Result<>0) then Exit;

 user_node:=g_instance.get_user_node(setParam^.userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 slot_node:=user_node^.get_slot_node(setParam^.slotId);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 Result:=SetParamsBe40(user_node,slot_node,setParam);
end;

///

function ps4_sceSaveDataSetSaveDataMemory(
           userId :SceUserServiceUserId;
           buf    :Pointer;
           bufSize:QWORD;
           offset :QWORD):Integer;
var
 data:SceSaveDataMemoryData;
 info:SceSaveDataMemorySet2;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 if (buf=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 if (p_proc.p_sdk_version < $1750000) then
 begin
  offset:=0;
 end;

 data:=Default(SceSaveDataMemoryData);
 data.buf    :=buf;
 data.bufSize:=bufSize;
 data.offset :=offset;

 info:=Default(SceSaveDataMemorySet2);
 info.userId:=userId;
 info.data  :=@data;

 mtx_lock(g_instance.mtx);

  Result:=SetSaveDataMemory2Lt65(@info);

 mtx_unlock(g_instance.mtx);
end;

function ps4_sceSaveDataSetSaveDataMemory2(setParam:pSceSaveDataMemorySet2):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 mtx_lock(g_instance.mtx);

  if (p_proc.p_sdk_version < $6500000) then
  begin
   Result:=SetSaveDataMemory2Lt65(setParam);
  end else
  begin
   Result:=SetSaveDataMemory2Be65(setParam);
  end;

 mtx_unlock(g_instance.mtx);
end;

function SaveDataSyncSaveDataMemory(syncParam:pSceSaveDataMemorySync):Integer;
var
 user_node:PPerUserInfo;
 slot_node:PPerSdSlot;
begin
 user_node:=g_instance.get_user_node(syncParam^.userId);
 if (user_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 slot_node:=user_node^.get_slot_node(syncParam^.slotId);
 if (slot_node=nil) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (p_proc.p_sdk_version < $6500000) then
 begin
  if not slot_node^.is_setup then
  begin
   Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  end;
 end else
 begin
  if not slot_node^.is_setup then
  begin
   Exit(SCE_SAVE_DATA_ERROR_MEMORY_NOT_READY);
  end;
 end;

 Result:=g_instance.Backend.SyncMemory(syncParam);
end;

function ps4_sceSaveDataSyncSaveDataMemory(syncParam:pSceSaveDataMemorySync):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckSceSaveDataMemorySync(syncParam);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  Result:=SaveDataSyncSaveDataMemory(syncParam);

 mtx_unlock(g_instance.mtx);

 if (Result=0) and ((syncParam^.option and 1)=0) then
 if (g_instance.job_thread<>nil) then
 begin
  g_instance.job_list.SendEventJob();
 end;
end;

function SaveDataDelete(del:pSceSaveDataDelete):Integer;
var
 cmd:TProgressJob;
begin
 Result:=CheckSaveDataDelete(del);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  cmd:=nil;
  if (p_proc.p_sdk_version < $3500000) and (g_instance.job_thread<>nil) then
  begin
   cmd:=g_instance.job_list.SendProgressJob(@del^.progress);
  end;

  Result:=g_instance.Backend.DoDelete(del);

  if (cmd<>nil) then
  begin
   cmd.Stop;
   cmd.dec_ref;
  end;

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 begin
  if (p_proc.p_sdk_version < $3500000) then
  begin
   del^.progress:=100;
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
 cmd:TProgressJob;
begin
 Result:=CheckSaveDataMount(mount,pResult,Transfering);
 if (Result<>0) then Exit;

 cmd:=nil;

 mtx_lock(g_instance.mtx);

  cmd:=nil;
  if (p_proc.p_sdk_version < $3500000) and (g_instance.job_thread<>nil) then
  begin
   cmd:=g_instance.job_list.SendProgressJob(@pResult^.progress);
  end;

  Result:=g_instance.Backend.DoMount(mount,pResult,Transfering,False);

  Writeln('SaveDataMount("',mount^.dirName^.data,'"):0x',HexStr(Result,8));

  if (cmd<>nil) then
  begin
   cmd.Stop;
   cmd.dec_ref;
  end;

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 begin
  if (p_proc.p_sdk_version < $3500000) then
  begin
   pResult^.progress:=100;
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
 tmp.mountMode  :=SDMM_RDONLY       ;

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

function ps4_sceSaveDataDirNameSearch(cond   :pSceSaveDataDirNameSearchCond;
                                      pResult:pSceSaveDataDirNameSearchResult):Integer;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckDirNameSearchCond(cond,False);
 if (Result<>0) then Exit;

 Result:=CheckDirNameSearchResult(pResult);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  if (p_proc.p_sdk_version > $16fffff) then
  begin
   if (pResult^.dirNames<>nil) then
   begin
    FillChar(pResult^.dirNames^,pResult^.dirNamesNum shl 5,0);
   end;
   if (pResult^.params<>nil) then
   begin
    FillChar(pResult^.params^,pResult^.dirNamesNum * $530,0);
   end;
  end;

  Result:=g_instance.Backend.DirNameSearch(cond,pResult,False);

 mtx_unlock(g_instance.mtx);
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

  Result:=g_instance.Backend.LoadIcon(slot_id,param,False);

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

 if not (g_instance.version in [VERSION_INIT_2,VERSION_INIT_3]) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.ClearProgress();

 mtx_unlock(g_instance.mtx);
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

 if not (g_instance.version in [VERSION_INIT_2,VERSION_INIT_3]) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_PARAMETER);
 end;

 mtx_lock(g_instance.mtx);

  Result:=g_instance.Backend.GetProgress(progress);

 mtx_unlock(g_instance.mtx);
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
var
 cmd:TProgressJob;
begin
 if (g_instance=nil) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_NOT_INITIALIZED);
 end;

 Result:=CheckRestoreBackupData(restore);
 if (Result<>0) then Exit;

 mtx_lock(g_instance.mtx);

  cmd:=nil;
  if (p_proc.p_sdk_version < $3500000) and (g_instance.job_thread<>nil) then
  begin
   cmd:=g_instance.job_list.SendProgressJob(@restore^.progress);
  end;

  Result:=g_instance.Backend.RestoreBackup(restore);

  if (cmd<>nil) then
  begin
   cmd.Stop;
   cmd.dec_ref;
  end;

 mtx_unlock(g_instance.mtx);

 if (Result=0) then
 begin
  if (p_proc.p_sdk_version < $3500000) then
  begin
   restore^.progress:=100;
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

