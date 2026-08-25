unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 TypInfo,
 mqueue,
 LFQueue,

 sys_eventvar,

 si_ci_vi_merged_enum,
 si_ci_vi_merged_groups,

 sys_bootparam,
 host_ipc,
 md_sleep,

 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vHostBufferManager,
 vImage,
 vImageManager,
 vRender,
 vRenderPassManager,
 vPipelineManager,
 vFramebufferManager,
 vShader,
 vShaderExt,
 vShaderManager,
 vRegs2Vulkan,
 vCmdBuffer,
 vDescriptorSet,
 vSampler,
 vSamplerManager,
 vMetaManager,

 vImageTiling,
 vDependence,

 renderdoc,

 sys_event,
 time,
 md_time,
 kern_thr,
 sched_ule,
 pm4defs,
 pm4_stream;

function  gc_add_internal_ptr   (kq,ptr,udata:Pointer):Integer; register; external;
function  gc_del_internal_ptr   (kq,ptr:Pointer):Integer;       register; external;
procedure gc_wakeup_internal_ptr(ptr:Pointer);                  register; external;

Const
 CONST_RAM_SIZE=48*1024;

type
 t_on_submit_flip_eop=function(submit_id:QWORD):Integer;

 p_pm4_stall=^t_pm4_stall;
 t_pm4_stall=record
  next:p_pm4_stall;
  //
  list:TAILQ_HEAD; //p_pm4_stream
  //
  count:Ptruint;
  flow :Ptruint;
 end;

 p_me_wait_addr=^t_me_wait_addr;
 t_me_wait_addr=object
  Fcode_addr:Pointer;
  Fdmem_addr:Pointer;
  Fregs_addr:Pointer;
  //
  procedure add_reg(kq:Pointer);
  procedure del_reg(kq:Pointer);
  procedure set_adr(kq,addr:Pointer);
 end;

 p_pm4_me=^t_pm4_me;
 t_pm4_me=object
  //
  queue:TIntrusiveMPSCQueue; //p_pm4_stream
  //
  stall:array[t_pm4_stream_type] of t_pm4_stall;
  //
  sheduler:record
   start :p_pm4_stall;
   switch:Boolean;
   count :Byte;
  end;
  //
  //event:PRTLEvent;
  on_idle:TProcedure;
  on_submit_flip_eop:t_on_submit_flip_eop;
  //
  started:Pointer;
  td:p_kthread;
  //
  gc_knlist:p_knlist;
  gc_kqueue:p_kqueue;
  //
  wait_ptr:array[t_pm4_stream_type] of t_me_wait_addr;
  //
  imdone_count:QWORD;
  //
  CONST_RAM:array[0..CONST_RAM_SIZE-1] of Byte; //48KB
  //
  CE_COUNT:DWORD;
  DE_COUNT:DWORD;
  //
  procedure Init(knlist:p_knlist);
  procedure start;
  procedure trigger;
  procedure wait;
  procedure imdone;
  procedure knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
  procedure Push(var stream:t_pm4_stream);
  procedure reset_sheduler;
  procedure set_step(s:t_pm4_stream_type);
  procedure next_step;
  function  next_task:Boolean;
  procedure switch_task;
  procedure add_stream   (stream:p_pm4_stream);
  function  get_next     :p_pm4_stream;
  procedure remove_stream(stream:p_pm4_stream);
 end;

 PvCmdFreeNode=^TvCmdFreeNode;
 TvCmdFreeNode=record
  entry:STAILQ_ENTRY;
  FCmd :TVkCommandBuffer;
 end;

 TvCmdCachedPool=class(TvCmdPool)
  FMemCache:STAILQ_HEAD; //PvCmdFreeNode
  FDeffered:STAILQ_HEAD; //PvCmdFreeNode
  FTrimCount:Integer;
  Constructor Create(FFamily:TVkUInt32);
  procedure   Free(cmd:TVkCommandBuffer); register; override;
  procedure   Trim;                       register; override;
 end;

 t_pool_line=array[0..3] of TvCustomCmdPool;
 t_pool_cache=object
  queue:TvQueue;
  line :t_pool_line;
  last :TvCustomCmdPool;
  Procedure Init(Q:TvQueue);
  function  fetch(i:QWORD):TvCustomCmdPool;
  procedure trim;
  procedure trim_all;
 end;

 TvStreamCmdBuffer=class(TvCmdBuffer)
  entry :TAILQ_ENTRY;  //stall
  stream:p_pm4_stream;
  //
  function  OnAlloc(size:Ptruint):Pointer; register; override;
  Procedure OnFree (P:Pointer   );         register; override;
  function  IsLinearAlloc:Boolean;         register; override;
 end;

 t_me_render_context=object
  me      :p_pm4_me;
  stream  :p_pm4_stream;
  node    :p_pm4_node;
  //
  rel_time:QWORD;
  //
  rt_info :p_pm4_rt_info;
  Render  :TvRenderPassBeginInfo;
  //
  gfx_pool:t_pool_cache;
  //
  Cmd     :TvStreamCmdBuffer;
  stall   :array[t_pm4_stream_type] of TAILQ_HEAD; //TvStreamCmdBuffer
  //
  dep:TvDependenciesObject;
  images_size:QWORD;
  buffer_size:QWORD;
  //
  procedure Init;
  procedure BeginCmdBuffer;
  procedure FinishCmdBuffer;
  function  CmdStatus(i:t_pm4_stream_type):TVkResult;
  function  PingCmd:Boolean;
  function  WaitConfirm:Boolean;
  function  WaitConfirmOrSwitch:Boolean;
  Procedure InsertLabel(pLabelName:PVkChar);
  Procedure BeginLabel(pLabelName:PVkChar);
  Procedure EndLabel();
  //
  procedure switch_task;
  procedure complete_and_next_task;
  procedure on_idle;

  Procedure RefToParent(obj:TvRefsObject); register;
  procedure FlushParent; register;
 end;

var
 use_renderdoc_capture:Boolean=False;
 act_renderdoc_capture:Boolean=False;
 wait_loop_detect     :Boolean=True;
 wait_loop_autoskip   :Boolean=False;

implementation

uses
 windows,
 kern_dmem,
 kern_proc,
 vm_map,
 vm_tracking_map,
 dev_dce;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

function GetAsyncKeyState(vKey:longint):Boolean; inline;
begin
 Result:=(Windows.GetKeyState(vKey) and $8000)<>0;
end;

procedure StartFrameCapture;
begin
 if use_renderdoc_capture then
 begin

  if GetAsyncKeyState(VK_F1) then
  begin
   act_renderdoc_capture:=True;
  end;

  if GetAsyncKeyState(VK_F2) then
  begin
   act_renderdoc_capture:=False;
  end;

  if act_renderdoc_capture then
  begin
   if (renderdoc.IsFrameCapturing()=0) then
   begin
    SetCaptureOptionU32(eRENDERDOC_Option_RefAllResources,1);
    renderdoc.StartFrameCapture(0,0);
   end;
  end else
  begin
   if (renderdoc.IsFrameCapturing()<>0) then
   begin
    renderdoc.EndFrameCapture(0,0);
   end;
  end;

 end;
end;

procedure EndFrameCapture;
begin
 if use_renderdoc_capture then
 begin
  if (renderdoc.IsFrameCapturing()<>0) then
  begin
   renderdoc.EndFrameCapture(0,0);
  end;
 end;
end;

procedure t_pm4_me.Init(knlist:p_knlist);
var
 i:t_pm4_stream_type;
begin
 queue.Create;

 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  if (i=High(t_pm4_stream_type)) then
  begin
   stall[i].next:=@stall[Low(t_pm4_stream_type)];
  end else
  begin
   stall[i].next:=@stall[Succ(i)];
  end;
  //
  TAILQ_INIT(@stall[i].list);
 end;

 gc_knlist:=knlist;

 gc_kqueue:=kern_kqueue2('[gc_kqueue]',nil,nil);
 gc_add_internal_ptr(gc_kqueue,@queue,@queue);
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl; forward;

procedure t_pm4_me.start;
begin
 if (XCHG(started,Pointer(1))=nil) then
 begin
  //event:=RTLEventCreate;
  //
  kthread_add(@pm4_me_thread,@self,@td,(8*1024*1024) div (16*1024),'[GFX_ME]');
 end;
end;

procedure t_pm4_me.trigger;
begin
 if (gc_kqueue<>nil) then
 begin
  gc_wakeup_internal_ptr(@queue);
 end;

 {
 if (event<>nil) then
 begin
  RTLEventSetEvent(event);
 end;
 }
end;

procedure t_pm4_me.wait;
var
 kev:array[0..15] of t_kevent;
 t:timespec;
 i,r:Integer;
 wait_addr:p_me_wait_addr;
 wmin_addr:p_me_wait_addr;
begin
 t:=Default(timespec);

 t.tv_sec :=0;
 t.tv_nsec:=1000000000 div 1000;

 r:=0;

 if (gc_kqueue<>nil) then
 begin
  kern_kevent2(gc_kqueue,nil,0,@kev,Length(kev),@t,@r);
 end;

 wmin_addr:=nil;

 if (r<>0) then
 For i:=0 to r-1 do
 begin
  if (kev[i].udata=@queue) then
  begin
   //
  end else
  begin
   wait_addr:=kev[i].udata;
   if (wmin_addr=nil) or
      (wmin_addr>wait_addr) then
   begin
    wmin_addr:=wait_addr
   end;
  end;
 end;

 if (wmin_addr<>nil) then
 begin
  i:=(PtrUint(wmin_addr)-PtrUint(@wait_ptr)) div SizeOf(t_me_wait_addr);
  set_step(t_pm4_stream_type(i));
 end;
end;

procedure t_pm4_me.imdone;
begin
 System.InterlockedIncrement64(imdone_count);
 trigger;
end;

procedure t_pm4_me.knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
begin
 knote(gc_knlist, event_id or (me_id shl 8) or (timestamp shl 16), lockflags);
end;

procedure t_pm4_me.Push(var stream:t_pm4_stream);
var
 node:p_pm4_stream;
 buft:t_pm4_stream_type;
begin
 if (stream.First=nil) then Exit;
 //self alloc
 node:=stream.allocator.Alloc(SizeOf(t_pm4_stream));
 //
 node^:=stream;
 //
 buft:=stream.buft;
 stream:=Default(t_pm4_stream);
 stream.buft:=buft;
 //
 queue.Push(node);
 //
 start;
 //
 trigger;
end;

procedure t_pm4_me.reset_sheduler;
begin
 //reset stall iterator
 sheduler.start :=@stall[Low(t_pm4_stream_type)];
 sheduler.switch:=False;
 sheduler.count :=0;
end;

procedure t_pm4_me.set_step(s:t_pm4_stream_type);
begin
 sheduler.start :=@stall[s];
 sheduler.switch:=False;
 sheduler.count :=0;
end;

procedure t_pm4_me.next_step;
begin
 //next
 sheduler.start:=sheduler.start^.next;
 //
 if (sheduler.start^.flow=0) then
 begin
  sheduler.start^.flow:=sheduler.start^.count;
 end;
end;

function t_pm4_me.next_task:Boolean;
begin
 if TAILQ_EMPTY(@sheduler.start^.list) or
    (sheduler.start^.flow=0) then
 begin
  //next
  next_step;
  //
  Result:=True;
 end else
 begin
  Dec(sheduler.start^.flow);
  //
  Result:=False;
 end;
end;

procedure t_pm4_me.switch_task;
begin
 sheduler.switch:=True;
 //
 Inc(sheduler.count);
 //
 if (sheduler.count=Length(stall)) then
 begin
  //next
  next_step;
  //wait
  wait;
  //msleep_td(hz div 10000);
  //
  sheduler.count:=0;
 end else
 begin
  //next
  next_step;
 end;
end;

procedure t_pm4_me.add_stream(stream:p_pm4_stream);
var
 i:t_pm4_stream_type;
begin
 i:=stream^.buft;
 TAILQ_INSERT_TAIL(@stall[i].list,stream,@stream^.next_);
 //
 Inc(stall[i].count);
 //
 stream^.Acquire; //stall
end;

function t_pm4_me.get_next:p_pm4_stream;
var
 i:t_pm4_stream_type;
begin
 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  Result:=TAILQ_FIRST(@sheduler.start^.list);
  if (Result<>nil) then Break;
  //next
  next_step;
 end;
end;

procedure free_stream(stream:p_pm4_stream);
var
 tmp:t_pm4_stream;
begin
 tmp:=stream^;
 tmp.Free;
end;

procedure t_pm4_me.remove_stream(stream:p_pm4_stream);
var
 i:t_pm4_stream_type;
begin
 //pop
 i:=stream^.buft;
 TAILQ_REMOVE(@stall[i].list,stream,@stream^.next_);
 //
 Dec(stall[i].count);
 //
 if stream^.Release then //stall
 begin
  //
  free_stream(stream);
 end;
end;

//

Constructor TvCmdCachedPool.Create(FFamily:TVkUInt32);
begin
 inherited;

 STAILQ_INIT(@FMemCache);
 STAILQ_INIT(@FDeffered);
end;

procedure TvCmdCachedPool.Free(cmd:TVkCommandBuffer); register;
var
 node:PvCmdFreeNode;
begin
 if STAILQ_EMPTY(@FMemCache) then
 begin
  node:=AllocMem(SizeOf(TvCmdFreeNode));
 end else
 begin
  node:=STAILQ_FIRST(@FMemCache);
  STAILQ_REMOVE(@FMemCache,node,@node^.entry);
 end;

 node^.FCmd:=cmd;

 STAILQ_INSERT_TAIL(@FDeffered,node,@node^.entry);
end;

procedure TvCmdCachedPool.Trim; register;
var
 node:PvCmdFreeNode;
begin
 node:=STAILQ_FIRST(@FDeffered);

 while (node<>nil) do
 begin
  STAILQ_REMOVE(@FDeffered,node,@node^.entry);

  inherited Free(node^.FCmd);

  STAILQ_INSERT_TAIL(@FMemCache,node,@node^.entry);

  //
  node:=STAILQ_FIRST(@FDeffered);
 end;

 Inc(FTrimCount);

 if (FTrimCount>=5000) then
 begin
  FTrimCount:=0;
  inherited Trim;
 end;
end;

//

Procedure t_pool_cache.Init(Q:TvQueue);
begin
 queue:=Q;
end;

function t_pool_cache.fetch(i:QWORD):TvCustomCmdPool;
var
 p:Byte;
begin
 p:=i mod Length(t_pool_line);

 if (line[p]=nil) then
 begin
  line[p]:=TvCmdCachedPool.Create(queue.FFamily);
 end;

 if (last<>line[p]) then
 begin
  last:=line[p];
  last.Trim;
 end;

 Result:=last;
end;

procedure t_pool_cache.trim;
begin
 if (last<>nil) then
 begin
  last.Trim;
 end;
end;

procedure t_pool_cache.trim_all;
var
 i:Byte;
begin
 For i:=0 to High(t_pool_line) do
 begin
  line[i].Trim;
 end;
end;

//

function TvStreamCmdBuffer.OnAlloc(size:Ptruint):Pointer; register;
begin
 Result:=stream^.allocator.Alloc(size);
 FillChar(Result^,size,0);
end;

Procedure TvStreamCmdBuffer.OnFree(P:Pointer); register;
begin
 //
end;

function TvStreamCmdBuffer.IsLinearAlloc:Boolean; register;
begin
 Result:=True;
end;

procedure t_me_render_context.RefToParent(obj:TvRefsObject); register;
begin
 if (dep=nil) then
 begin
  dep:=TvDependenciesObject.Create;
 end;
 dep.RefTo(obj);
 //
 if obj.InheritsFrom(TvCustomImage) then
 begin
  images_size:=images_size+TvCustomImage(obj).FSize;
 end;
 if obj.InheritsFrom(TvBuffer) then
 begin
  buffer_size:=buffer_size+TvBuffer(obj).FSize;
 end;
end;

procedure t_me_render_context.FlushParent; register;
begin
 if (dep<>nil) then
 begin
  dep.ReleaseAllDependencies(dep);
 end;
 images_size:=0;
 buffer_size:=0;
end;

//

procedure t_me_render_context.Init;
var
 i:t_pm4_stream_type;
begin
 gfx_pool.Init(RenderQueue);

 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  TAILQ_INIT(@stall[i]);
 end;

end;

procedure t_me_render_context.BeginCmdBuffer;
var
 buft:t_pm4_stream_type;
 imdone_count:QWORD;
 Pool:TvCustomCmdPool;
begin
 if (Cmd<>nil) then Exit; //Already allocated

 buft:=stream^.buft;
 //Select Vulkan compute only queue?

 imdone_count:=me^.imdone_count;

 Pool:=gfx_pool.fetch(imdone_count);

 Cmd:=TvStreamCmdBuffer.Create(Pool,gfx_pool.queue);
 Cmd.stream:=stream;

 stream^.Acquire; //TvStreamCmdBuffer
end;

procedure free_cmd_buffer(cmd:TvStreamCmdBuffer);
var
 stream:p_pm4_stream;
begin
 stream:=cmd.stream;

 //
 cmd.ReleaseResource;
 cmd.Free;
 //

 if stream^.Release then //TvStreamCmdBuffer
 begin
  free_stream(stream);
 end;
end;

procedure pm4_Writeback_Finish(var ctx:t_me_render_context); forward;

//
procedure t_me_render_context.FinishCmdBuffer;
var
 buft:t_pm4_stream_type;

 r:TVkResult;
begin
 if (Cmd=nil) then Exit;

 pm4_Writeback_Finish(Self);

 r:=Cmd.QueueSubmit;

 if p_print_gpu_ops then
 begin
  LOG_TRACE('QueueSubmit:',r);
 end;

 if (r<>VK_SUCCESS) then
 begin
  EndFrameCapture;
  PrintMemoryBudget;
  Assert(false,'QueueSubmit');
 end;

 r:=Cmd.Status;

 case r of
  VK_SUCCESS  :;
  VK_NOT_READY:
   begin
    //insert

    buft:=Cmd.stream^.buft;

    TAILQ_INSERT_TAIL(@stall[buft],Cmd,@Cmd.entry);

    Cmd:=nil;

    Exit;
   end;
  else
   LOG_ERROR(stderr,'last.Status=',r); //error
 end;

 free_cmd_buffer(Cmd);

 Cmd:=nil;
end;

function t_me_render_context.CmdStatus(i:t_pm4_stream_type):TVkResult;
var
 last:TvStreamCmdBuffer;
begin
 last:=TvStreamCmdBuffer(TAILQ_FIRST(@stall[i]));

 while (last<>nil) do
 begin

  Result:=last.Status;

  case Result of
   VK_SUCCESS  :;
   VK_NOT_READY:Exit;
   else
    LOG_ERROR(stderr,'last.Status=',Result); //error
  end;

  TAILQ_REMOVE(@stall[i],last,@last.entry);

  free_cmd_buffer(last);

  last:=TvStreamCmdBuffer(TAILQ_FIRST(@stall[i]));
 end;

 Result:=VK_SUCCESS;
end;

function t_me_render_context.PingCmd:Boolean;
var
 i:t_pm4_stream_type;
begin
 Result:=False;
 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  Result:=Result or (CmdStatus(i)=VK_NOT_READY);
 end;
end;

function t_me_render_context.WaitConfirm:Boolean;
begin
 gfx_pool.trim;

 FinishCmdBuffer;

 Result:=(CmdStatus(stream^.buft)<>VK_NOT_READY);
end;

function t_me_render_context.WaitConfirmOrSwitch:Boolean;
begin
 gfx_pool.trim;

 FinishCmdBuffer;

 if (stream=nil) then Exit(True);

 Result:=(CmdStatus(stream^.buft)<>VK_NOT_READY);

 if not Result then
 begin
  switch_task;
 end;
end;

Procedure t_me_render_context.InsertLabel(pLabelName:PVkChar);
begin
 if (DebugReport.FCmdInsertDebugUtilsLabel=nil) then Exit;

 if (Cmd=nil) then Exit;

 BeginCmdBuffer;

 Cmd.InsertLabel(pLabelName);
end;

Procedure t_me_render_context.BeginLabel(pLabelName:PVkChar);
begin
 if (DebugReport.FCmdBeginDebugUtilsLabel=nil) then Exit;

 if (Cmd=nil) then Exit;

 BeginCmdBuffer;

 Cmd.BeginLabel(pLabelName);
end;

Procedure t_me_render_context.EndLabel();
begin
 if (Cmd=nil) then Exit;

 Cmd.EndLabel();
end;

procedure t_me_render_context.switch_task;
begin
 FinishCmdBuffer;
 //
 me^.switch_task;
end;

procedure t_me_render_context.complete_and_next_task;
begin
 FinishCmdBuffer;
 //
 me^.next_task;
end;

procedure t_me_render_context.on_idle;
begin
 if (me^.on_idle<>nil) then
 begin
  me^.on_idle();
 end;
end;

//

function GetMixedFlag(const curr:t_pm4_usage):Byte;
begin
 if (PopCnt(DWORD(curr.img_usage))>1) then
 begin
  Result:=TM_MIXED;
 end else
 begin
  Result:=0;
 end;
end;

function GetImageLayout(const curr:t_pm4_usage):TVkImageLayout;
begin
 if (PopCnt(DWORD(curr.img_usage))>1) then
 begin
  Result:=VK_IMAGE_LAYOUT_GENERAL;
 end else
 case t_image_usage(BsfDWord(DWORD(curr.img_usage))) of
  iu_attachment:
   begin
    Result:=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
   end;
  iu_depthstenc:
   begin
    if ((curr.shd_usage and (TM_WRITE or TM_CLEAR))<>0) then
    begin
     Result:=VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
    end else
    begin
     Result:=VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL;
    end;
   end;
  iu_sampled,
  iu_storage:
   begin
    if ((curr.shd_usage and (TM_WRITE or TM_CLEAR))<>0) then
    begin
     Result:=VK_IMAGE_LAYOUT_GENERAL;
    end else
    begin
     Result:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    end;
   end;
  iu_transfer:
   begin
    //mem_usage ???
    if ((curr.mem_usage and (TM_WRITE or TM_READ))=(TM_WRITE or TM_READ)) then
    begin
     Result:=VK_IMAGE_LAYOUT_GENERAL;
    end else
    if ((curr.mem_usage and TM_WRITE)<>0) then
    begin
     Result:=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    end else
    begin
     Result:=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
    end;
   end;
  else
   Result:=VK_IMAGE_LAYOUT_UNDEFINED;
 end;
end;

function ConvertRW(IMAGE_USAGE:Byte;R,W:TVkAccessFlagBits):TVkAccessFlags; inline;
begin
 Result:=(ord(R)*ord((IMAGE_USAGE and TM_READ               )<>0) ) or
         (ord(W)*ord((IMAGE_USAGE and (TM_WRITE or TM_CLEAR))<>0) );
end;

function GetAccessMaskImg(const curr:t_pm4_usage):TVkAccessFlags;
begin
 Result:=
  ConvertRW(curr.shd_usage,VK_ACCESS_SHADER_READ_BIT                  ,VK_ACCESS_SHADER_WRITE_BIT                  ) or
  ConvertRW(curr.clr_usage,VK_ACCESS_COLOR_ATTACHMENT_READ_BIT        ,VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT        ) or
  ConvertRW(curr.dsa_usage,VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT,VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
end;

function GetAccessMaskBuf(const curr:t_pm4_usage):TVkAccessFlags;
begin
 Result:=ConvertRW(curr.mem_usage,VK_ACCESS_SHADER_READ_BIT,VK_ACCESS_SHADER_WRITE_BIT);
end;

function GetStageMask(BindPoint:TVkPipelineBindPoint):TVkPipelineStageFlags;
begin
 case BindPoint of
  BP_GRAPHICS:Result:=ord(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT);
  BP_COMPUTE :Result:=ord(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
  else        Result:=ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT);
 end;
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

const
 VK_ACCESS_BUF_ANY=ord(VK_ACCESS_MEMORY_READ_BIT) or ord(VK_ACCESS_MEMORY_WRITE_BIT);
 VK_STAGE_BUF_ANY =ord(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT);

function _FetchImageForce(var ctx:t_me_render_context;const F:TvImageKey;usage:s_image_usage):TvImage2;
begin
 repeat

  Result:=FetchImage(ctx.Cmd,F,usage);

  if (Result=nil) then
  begin
   repeat
    msleep_td(hz div 10000);
   until ctx.WaitConfirm;
   ctx.BeginCmdBuffer;
  end;

 until (Result<>nil);
end;

function ConvertImage(var ctx:t_me_render_context;usage:s_image_usage;src:TvImage2;FromFormat,ToFormat:TVkFormat):TvImage2;
var
 F:TvImageKey;
 dst:TvImage2;
 range:TVkImageCopy;
 range_all:array[0..15] of TVkImageCopy;
 i,m:Integer;
begin
 Assert(src<>nil);

 F:=src.key;
 F.cformat:=ToFormat;

 if not ExtractImage(src) then
 begin
  Assert(false,'ExtractImage');
 end;

 dst:=_FetchImageForce(ctx,F,usage);

 src.PushBarrier(ctx.cmd,
                 ord(VK_ACCESS_TRANSFER_READ_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 dst.PushBarrier(ctx.cmd,
                 ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                 VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                 ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 //
 range.srcSubresource.aspectMask    :=GetAspectMaskByFormat(FromFormat);
 range.srcSubresource.mipLevel      :=0;
 range.srcSubresource.baseArrayLayer:=0;
 range.srcSubresource.layerCount    :=src.key.params.layerCount;

 range.srcOffset.x:=0;
 range.srcOffset.y:=0;
 range.srcOffset.z:=0;

 range.dstSubresource.aspectMask    :=GetAspectMaskByFormat(ToFormat);
 range.dstSubresource.mipLevel      :=0;
 range.dstSubresource.baseArrayLayer:=0;
 range.dstSubresource.layerCount    :=dst.key.params.layerCount;

 range.dstOffset.x:=0;
 range.dstOffset.y:=0;
 range.dstOffset.z:=0;

 range.extent.width :=src.key.params.width;
 range.extent.height:=src.key.params.height;
 range.extent.depth :=src.key.params.depth;
 //

 m:=src.key.params.mipLevels;
 For i:=0 to m-1 do
 begin
  range_all[i]:=range;
  range_all[i].srcSubresource.mipLevel:=i;
  range_all[i].dstSubresource.mipLevel:=i;
 end;

 if (FromFormat<>VK_FORMAT_S8_UINT) then //TODO: do this using a special shader
 ctx.Cmd.CopyImage(
  src.FHandle,
  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
  dst.FHandle,
  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
  m,
  @range_all[0]
 );

 dst.change_rate:=src.change_rate;

 Result:=dst;
end;

function FetchImageForce(var ctx:t_me_render_context;const F:TvImageKey;usage:s_image_usage):TvImage2;
begin

 Result:=_FetchImageForce(ctx,F,usage);

 //function ExtractImage(img:TvImage2):Boolean;

 Assert(Result<>nil);

 //TODO: more general type compatibility checking
 case Result.FFormat of
  //
  VK_FORMAT_R32_UINT,
  VK_FORMAT_R32_SINT,
  VK_FORMAT_R32_SFLOAT:
    if (iu_depthstenc in usage) then
    begin
     //R32 -> D32
     Result:=ConvertImage(ctx,usage,Result,Result.FFormat,VK_FORMAT_D32_SFLOAT);
    end;
  //
  VK_FORMAT_R16_UNORM,
  VK_FORMAT_R16_SNORM,
  VK_FORMAT_R16_UINT,
  VK_FORMAT_R16_SINT,
  VK_FORMAT_R16_SFLOAT:
    if (iu_depthstenc in usage) then
    begin
     //R16 -> D16
     Result:=ConvertImage(ctx,usage,Result,Result.FFormat,VK_FORMAT_D16_UNORM);
    end;
  //
  VK_FORMAT_D32_SFLOAT:
   if ([iu_attachment,iu_storage]*usage<>[]) then
   begin
    //D32 -> R32
    Result:=ConvertImage(ctx,usage,Result,VK_FORMAT_D32_SFLOAT,VK_FORMAT_R32_SFLOAT);
   end;
  VK_FORMAT_D32_SFLOAT_S8_UINT:

    case F.cformat of
     VK_FORMAT_R32_UINT,
     VK_FORMAT_R32_SINT,
     VK_FORMAT_R32_SFLOAT:
      if ([iu_attachment,iu_storage]*usage<>[]) then
      begin
       //D32 -> R32
       Result:=ConvertImage(ctx,usage,Result,VK_FORMAT_D32_SFLOAT,F.cformat);
      end;
     VK_FORMAT_R8_UNORM,
     VK_FORMAT_R8_SNORM,
     VK_FORMAT_R8_UINT,
     VK_FORMAT_R8_SINT,
     VK_FORMAT_R8_SRGB:
      if ([iu_attachment,iu_storage,iu_sampled]*usage<>[]) then
      begin
       //S8 -> R8
       Result:=ConvertImage(ctx,usage,Result,VK_FORMAT_S8_UINT,F.cformat);
      end;
    end;
  //
  VK_FORMAT_D16_UNORM:
   if ([iu_attachment,iu_storage]*usage<>[]) then
   begin
    //D16 -> R16
    Result:=ConvertImage(ctx,usage,Result,VK_FORMAT_D16_UNORM,VK_FORMAT_R16_UNORM);
   end
  //
  else;
 end;

 ctx.RefToParent(Result);

end;

procedure Prepare_Uniforms(var ctx:t_me_render_context;
                           BindPoint:TVkPipelineBindPoint;
                           var UniformBuilder:TvUniformBuilder);
var
 i:Integer;

 ri:TvImage2;

 buf:TvHostBuffer;
 diff_u:TVkDeviceSize;
 diff_a:TVkDeviceSize;

 resource_instance:p_pm4_resource_instance;
 b:Boolean;
begin
 LOG_TRACE('[Prepare_Uniforms]->');

 if (Length(UniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FImages) do
  With UniformBuilder.FImages[i] do
  begin

   if (FImage.params.invalid<>0) then
   begin
    //skip
    Continue;
   end;

   resource_instance:=ctx.node^.scope.find_image_resource_instance(FImage);

   if (resource_instance=nil) then
   begin
    case btype of
     vbSampled:
      begin
       resource_instance:=ctx.stream^.insert_image_resource(
        @ctx.node^.scope,
        FImage,
        memuse,
        [iu_sampled],
        'Prepare_Uniforms');
      end;
     vbStorage,
     vbMipStorage:
      begin
       resource_instance:=ctx.stream^.insert_image_resource(
        @ctx.node^.scope,
        FImage,
        memuse,
        [iu_storage],
        'Prepare_Uniforms');
      end;
     else
      Assert(false);
    end;
   end;

   Assert(resource_instance<>nil);

   if not resource_instance^.prepared then
   begin
    resource_instance^.prepared:=true;

    //ri:=TvImage2(resource_instance^.resource^.rimage);
    ri:=nil;

    if (ri<>nil) then
    begin
     ctx.Cmd.RefTo(ri);
    end;

    if (ri<>nil) then
    if (ri.is_invalid) then
    begin
     resource_instance^.resource^.rimage:=nil;
     ri:=nil;
    end;

    if (ri=nil) then
    begin
     ri:=FetchImageForce(ctx,
                         FImage,
                         resource_instance^.curr.img_usage);

     resource_instance^.resource^.rimage:=ri;
    end;

    LOG_TRACE(GetVkFormatStr(ri.key.cformat));

    repeat

     b:=pm4_load_from(ctx.Cmd,ri,resource_instance^.curr.mem_usage);

     if (not b) then
     begin
      repeat until ctx.WaitConfirm;
      ctx.BeginCmdBuffer;
     end;
    until (b);

    ri.PushBarrier(ctx.Cmd,
                   GetAccessMaskImg(resource_instance^.curr),
                   GetImageLayout(resource_instance^.curr),
                   GetStageMask(BindPoint));

   end;

  end;
 end;

 //Buffers

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  if (memuse and TM_INVAL)=0 then
  begin

   resource_instance:=ctx.node^.scope.find_buffer_resource_instance(R_BUF,addr,size);

   if (resource_instance=nil) then
   begin
    resource_instance:=ctx.stream^.insert_buffer_resource(
     @ctx.node^.scope,
     R_BUF,
     addr,
     size,
     memuse,
     [iu_buffer],
     'Prepare_Uniforms');
   end;

   Assert(resource_instance<>nil);

   if not resource_instance^.prepared then
   begin
    resource_instance^.prepared:=true;

    //buf:=TvHostBuffer(resource_instance^.resource^.rimage);
    buf:=nil;

    if (buf<>nil) then
    begin
     ctx.Cmd.RefTo(buf);
    end;

    if (buf<>nil) then
    if (buf.is_invalid) then
    begin
     resource_instance^.resource^.rimage:=nil;
     buf:=nil;
    end;

    if (buf=nil) then
    begin

     repeat

      buf:=FetchHostBuffer(ctx.Cmd,QWORD(addr),size);

      if (buf=nil) then
      begin
       repeat until ctx.WaitConfirm;
       ctx.BeginCmdBuffer;
      end;
     until (buf<>nil);

     Assert(buf<>nil);

     ctx.RefToParent(buf);

     resource_instance^.resource^.rimage:=buf;

     diff_u:=QWORD(addr)-buf.FAddr;
     diff_a:=AlignDw(diff_u,limits.minStorageBufferOffsetAlignment);

     //TODO: Barrier state cache
     ctx.Cmd.BufferMemoryBarrier(buf.FHandle,
                                 VK_ACCESS_BUF_ANY,
                                 GetAccessMaskBuf(resource_instance^.curr),
                                 diff_a,size,
                                 VK_STAGE_BUF_ANY,
                                 GetStageMask(BindPoint)
                                );


    end;

   end;

  end;
 end;
 //buffers

 LOG_TRACE('<-[Prepare_Uniforms]');
end;

procedure BindMipStorage(var ctx:t_me_render_context;
                         fset,bind:TVkUInt32;
                         DescriptorGroup:TvDescriptorInterface;
                         ri:TvImage2;
                         const FView:TvImageViewKey;
                         Layout:TVkImageLayout);
var
 i,p:Integer;
 iv:TvImageView2;
 aiv:array[0..15] of TVkImageView;
 MView:TvImageViewKey;
begin
 if (ri=nil) then
 begin

  For i:=0 to 15 do
  begin
   aiv[i]:=VK_NULL_HANDLE;
  end;

 end else
 begin

  p:=0;
  For i:=FView.base_level to FView.last_level do
  begin
   MView:=FView;
   MView.base_level:=i;
   MView.last_level:=i;
   //
   iv:=ri.FetchView(ctx.Cmd,MView,iu_storage);
   aiv[p]:=iv.FHandle;
   //
   Inc(p);
  end;

  //fill by 16?

  while (p<16) do
  begin
   aiv[p]:=iv.FHandle;
   //
   Inc(p);
  end;

 end;

 DescriptorGroup.BindStorages(fset,bind,
                              0,p,
                              @aiv[0],
                              Layout);

end;

Function get_bind_str(FBind:TvPointer):RawByteString;
begin
 if (FBind.FMemory=nil) then
 begin
  Result:='(nil)';
 end else
 begin
  Result:='0x'+HexStr(FBind.FMemory.FHandle,16);
 end;
end;

procedure Bind_Uniforms(var ctx:t_me_render_context;
                        BindPoint:TVkPipelineBindPoint;
                        var UniformBuilder:TvUniformBuilder);

var
 i:Integer;

 DescriptorGroup:TvDescriptorInterface;

 ri:TvImage2;
 iv:TvImageView2;
 sm:TvSampler;

 buf:TvHostBuffer;

 diff_u:TVkDeviceSize;
 diff_a:TVkDeviceSize;
 align :TVkDeviceSize;
 range :TVkDeviceSize;

 resource_instance:p_pm4_resource_instance;

 Layout:TVkImageLayout;
begin
 DescriptorGroup:=ctx.Cmd.FetchDescriptorInterface(BindPoint);

 //images
 if (Length(UniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FImages) do
  With UniformBuilder.FImages[i] do
  begin

   if (FImage.params.invalid<>0) then
   begin

    if (limits.nullDescriptor<>VK_TRUE) then
    begin
     Assert(false,'unsupported nullDescriptor');
    end;

    case btype of
     vbSampled:
      begin
       DescriptorGroup.BindImage(fset,bind,
                                 VK_NULL_HANDLE,
                                 VK_IMAGE_LAYOUT_GENERAL);
      end;
     vbStorage:
      begin
       DescriptorGroup.BindStorage(fset,bind,
                                   VK_NULL_HANDLE,
                                   VK_IMAGE_LAYOUT_GENERAL);
      end;
     vbMipStorage:
      begin
       BindMipStorage(ctx,
                      fset,bind,
                      DescriptorGroup,
                      nil,
                      FView,
                      VK_IMAGE_LAYOUT_GENERAL);

      end;
     else
      Assert(false);
    end;

   end else
   begin
    resource_instance:=ctx.node^.scope.find_image_resource_instance(FImage);

    Assert(resource_instance<>nil);

    //ri:=TvImage2(resource_instance^.resource^.rimage);

    ri:=FetchImageForce(ctx,
                        FImage,
                        resource_instance^.curr.img_usage
                       );

    Assert(ri<>nil);

    Layout:=GetImageLayout(resource_instance^.curr);

    if (ri.Barrier.ImgLayout=VK_IMAGE_LAYOUT_GENERAL) then
    begin
     Layout:=VK_IMAGE_LAYOUT_GENERAL;
    end;

    case btype of
     vbSampled:
      begin
       iv:=ri.FetchView(ctx.Cmd,FView,iu_sampled);
       Assert(iv<>nil);

       LOG_TRACE('BindImage:->[',i,']'#13#10,
               ' 0x',HexStr(ri.FHandle,16),':',GetVkFormatStr(ri.key.cformat),':',ri.FName,'->'#13#10,
               ' 0x',HexStr(iv.FHandle,16),':',GetVkFormatStr(iv.key.cformat),':',iv.FName);

       DescriptorGroup.BindImage(fset,bind,
                                 iv.FHandle,
                                 Layout);
      end;
     vbStorage:
      begin
       //reset dst_sel
       FView.dstSel:=Default(TvDstSel);
       //

       iv:=ri.FetchView(ctx.Cmd,FView,iu_storage);
       Assert(iv<>nil);

       LOG_TRACE('BindStorage:->[',i,']'#13#10,
               ' 0x',HexStr(ri.FHandle,16),':',ri.key.cformat,':',ri.FName,'->'#13#10,
               ' 0x',HexStr(iv.FHandle,16),':',iv.key.cformat,':',iv.FName);

       DescriptorGroup.BindStorage(fset,bind,
                                   iv.FHandle,
                                   Layout);
      end;
     vbMipStorage:
      begin
       //reset dst_sel
       FView.dstSel:=Default(TvDstSel);
       //

       BindMipStorage(ctx,
                      fset,bind,
                      DescriptorGroup,
                      ri,
                      FView,
                      Layout);

      end;
     else
      Assert(false);
    end;

   end;

  end;
 end;
 //images

 //samplers
 if (Length(UniformBuilder.FSamplers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FSamplers) do
  With UniformBuilder.FSamplers[i] do
  begin
   sm:=FetchSampler(ctx.Cmd,PS);

   DescriptorGroup.BindSampler(fset,bind,sm.FHandle);

  end;
 end;
 //samplers

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  if (memuse and TM_INVAL)=0 then
  begin

   resource_instance:=ctx.node^.scope.find_buffer_resource_instance(R_BUF,addr,size);

   {
   if (resource_instance<>nil) then
   begin

    LOG_TRACE('rb:curr:',HexStr(resource_instance^.curr.mem_usage,1),
              ' prev:',HexStr(resource_instance^.prev.mem_usage,1),
              ' next:',HexStr(resource_instance^.next.mem_usage,1)
           );

   end;
   }

   buf:=FetchHostBuffer(ctx.Cmd,QWORD(addr),size);

   Assert(buf<>nil);

   diff_u:=QWORD(addr)-buf.FAddr;
   diff_a:=AlignDw(diff_u,limits.minStorageBufferOffsetAlignment);

   align:=diff_u-diff_a;

   if (align<>offset) then
   begin
    Assert(false,'wrong buffer align '+IntToStr(align)+'<>'+IntToStr(offset));
   end;

   range:=size;

   LOG_TRACE('BindBuffer:->[',i,':',bind,']:',GetVkFormatStr(cformat),' 0x',HexStr(QWORD(addr),10),' ',get_bind_str(buf.FBind),#13#10,
           ' 0x',HexStr(buf.FHandle,16),':',buf.FName,'->[',diff_a,'..',diff_a+range,']');

   DescriptorGroup.BindBuffer(fset,bind,
                              buf.FHandle,
                              diff_a,
                              range {VK_WHOLE_SIZE});

   if ((memuse and TM_WRITE)<>0) then
   begin
    ctx.Cmd.AddPlannedTrigger(QWORD(addr),QWORD(addr)+size,nil);
   end;

  end;
 end;
 //buffers

end;

procedure Bind_Pushs(var ctx:t_me_render_context;
                     ShaderGroup:TvShaderGroup;
                     dst:PGPU_USERDATA);
const
 bind_points:array[Boolean] of TVkPipelineBindPoint=(VK_PIPELINE_BIND_POINT_GRAPHICS,VK_PIPELINE_BIND_POINT_COMPUTE);
var
 Shader:TvShaderExt;
 i:TvShaderStage;
 FData:PDWORD;
 addr:Pointer;
begin
 For i:=Low(TvShaderStage) to High(TvShaderStage) do
 begin
  Shader:=ShaderGroup.FKey.FShaders[i];
  if (Shader<>nil) then
  if (Shader.FPushConst.size<>0) then
  begin
   FData:=dst^.get_user_data(i);
   addr :=Shader.GetPushConstData(FData);

   Assert(addr<>nil,'push const is NULL');

   ctx.Cmd.PushConstant(bind_points[Shader.FStage=VK_SHADER_STAGE_COMPUTE_BIT],
                        ord(Shader.FStage),
                        Shader.FPushConst.offset,
                        Shader.FPushConst.size,
                        addr);

  end;
 end;
end;

procedure pm4_InitStream(var ctx:t_me_render_context);
var
 i:p_pm4_resource_instance;
 resource:p_pm4_resource;

 ri:TvImage2;
 ht:TvMetaHtile;
 hc:TvMetaCmask;
begin
 if ctx.stream^.init then Exit;

 i:=ctx.stream^.init_scope.first;

 if (i=nil) then Exit;

 while (i<>nil) do
 begin

  resource:=i^.resource;

  if (resource^.rtype=R_IMG) and
     (not resource^.rcombined) then
  begin

   //start on demaind

   StartFrameCapture;

   ctx.BeginCmdBuffer;

   //

   LOG_TRACE('init_img:',HexStr(resource^.rkey.Addr),' ',(resource^.rkey.params.width),'x',(resource^.rkey.params.height));

   //now preload only sampled image
   if (resource^.uall.img_usage=[iu_sampled]) then
   begin
    ri:=FetchImage(ctx.Cmd,
                   resource^.rkey,
                   i^.curr.img_usage + i^.next.img_usage
                  );

    if (ri=nil) then
    begin
     //NO MEM
     Break;
    end;

    resource^.rimage:=ri;

    //pm4_load_from(ctx.Cmd,ri,i^.curr.mem_usage);
   end;
  end else
  if (resource^.rtype=R_HTILE) then
  begin

   //start on demaind

   ctx.BeginCmdBuffer;

   ht:=FetchHtile(ctx.Cmd,resource^.rkey,resource^.rsize);

   resource^.rclear:=ht.rclear;
  end else
  if (resource^.rtype=R_CMASK) then
  begin

   //start on demaind

   ctx.BeginCmdBuffer;

   hc:=FetchCmask(ctx.Cmd,resource^.rkey,resource^.rsize);

   resource^.rclear:=hc.rclear;
  end;

  i:=TAILQ_NEXT(i,@i^.init_entry);
 end;

 ctx.stream^.init:=True;
end;


procedure pm4_ClearDepth(var rt_info:t_pm4_rt_info;
                         var ctx:t_me_render_context);
var
 ri:TvImage2;
 iv:TvImageView2;
 cclear:array[0..1] of Boolean;
 range :TVkImageSubresourceRange;
begin
 //ClearDepthTarget

 ctx.Cmd.EndRenderPass;

 ctx.Cmd.BeginLabel('ClearDepth');

 ri:=FetchImageForce(ctx,
                     rt_info.DB_INFO.FImageInfo,
                     [iu_depthstenc]);

 Assert(ri<>nil);

 iv:=ri.FetchView(ctx.Cmd,rt_info.DB_INFO.FImageView,iu_depthstenc);

 ctx.RefToParent(ri);

 ri.PushBarrier(ctx.Cmd,
                ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 cclear[0]:=((rt_info.DB_INFO.DEPTH_USAGE   and TM_CLEAR)<>0) and
            (GetDepthOnlyFormat  (ri.key.cformat)<>VK_FORMAT_UNDEFINED);

 cclear[1]:=((rt_info.DB_INFO.STENCIL_USAGE and TM_CLEAR)<>0) and
            (GetStencilOnlyFormat(ri.key.cformat)<>VK_FORMAT_UNDEFINED);

 range:=iv.GetSubresRange;

 range.aspectMask:=(ord(VK_IMAGE_ASPECT_DEPTH_BIT  )*ord(cclear[0])) or
                   (ord(VK_IMAGE_ASPECT_STENCIL_BIT)*ord(cclear[1]));

 ctx.Cmd.ClearDepthStencilImage(ri.FHandle,
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                @rt_info.DB_INFO.CLEAR_VALUE.depthStencil,
                                range);

 ctx.Cmd.EndLabel();

 ctx.FlushParent;
end;

procedure DumpShaderGroup(ShaderGroup:TvShaderGroup);
var
 i:TvShaderStage;
 str:RawByteString;
begin
 str:='[DumpShaderGroup]'#13#10;
 For i:=Low(TvShaderStage) to High(TvShaderStage) do
 if (ShaderGroup.FKey.FShaders[i]<>nil) then
 begin
  str:=str+' ('+HexStr(ShaderGroup.FKey.FShaders[i].FHash_gcn,16)+') '+GetDumpSpvName(i,ShaderGroup.FKey.FShaders[i].FHash_spv)+#13#10;
 end;

 LOG_TRACE(stderr,str);
end;

procedure pm4_DrawPrepare(var ctx:t_me_render_context);
var
 i:Integer;

 FAttrBuilder:TvAttrBuilder;

 FUniformBuilder:TvUniformBuilder;

 RP_KEY:TvRenderPassKey;
 RP:TvRenderPass2;

 GP_KEY:TvGraphicsPipelineKey;
 GP:TvGraphicsPipeline2;

 FB_KEY:TvFramebufferImagelessKey;
 FB_KEY2:TvFramebufferBindedKey;
 FB:TvFramebuffer;

 ri:TvImage2;
 rd:TvCustomImage2;
 rs:TvCustomImage2;
 iv:TvImageView2;

 color_instance:array[0..7] of p_pm4_resource_instance;

 flag:Integer;
 img_usage:s_image_usage;

 meta_instance:p_pm4_resource_instance;
 d_instance:p_pm4_resource_instance;
 s_instance:p_pm4_resource_instance;

 //
 GPU_REGS:TGPU_REGS;
 CX_REG  :TCONTEXT_REG_GROUP;    // 0xA000

 pa:TPushConstAllocator;
 pp:PPushConstAllocator;
begin
 //recheck shaders
 GPU_REGS.SG_REG:=@ctx.rt_info^.SHADERDATA.SG_REG;
 GPU_REGS.CX_REG:=@CX_REG;
 GPU_REGS.UC_REG:=@ctx.rt_info^.SHADERDATA.UC_REG;
 GPU_REGS.SDP   :=@ctx.rt_info^.ShaderDrawParams;

 CX_REG:=Default(TCONTEXT_REG_GROUP);

 //copy
 CX_REG.SPI_PS_INPUT_ENA        :=ctx.rt_info^.SHADERDATA.SPI_PS_INPUT_ENA        ;
 CX_REG.SPI_PS_INPUT_ADDR       :=ctx.rt_info^.SHADERDATA.SPI_PS_INPUT_ADDR       ;
 CX_REG.SPI_INTERP_CONTROL_0    :=ctx.rt_info^.SHADERDATA.SPI_INTERP_CONTROL_0    ;
 CX_REG.SPI_PS_IN_CONTROL       :=ctx.rt_info^.SHADERDATA.SPI_PS_IN_CONTROL       ;
 CX_REG.SPI_SHADER_Z_FORMAT     :=ctx.rt_info^.SHADERDATA.SPI_SHADER_Z_FORMAT     ;
 CX_REG.SPI_SHADER_COL_FORMAT   :=ctx.rt_info^.SHADERDATA.SPI_SHADER_COL_FORMAT   ;
 CX_REG.SPI_PS_INPUT_CNTL       :=ctx.rt_info^.SHADERDATA.SPI_PS_INPUT_CNTL       ;
 CX_REG.DB_SHADER_CONTROL       :=ctx.rt_info^.SHADERDATA.DB_SHADER_CONTROL       ;
 CX_REG.PA_CL_VS_OUT_CNTL       :=ctx.rt_info^.SHADERDATA.PA_CL_VS_OUT_CNTL       ;
 CX_REG.VGT_INSTANCE_STEP_RATE_0:=ctx.rt_info^.SHADERDATA.VGT_INSTANCE_STEP_RATE_0;
 CX_REG.VGT_INSTANCE_STEP_RATE_1:=ctx.rt_info^.SHADERDATA.VGT_INSTANCE_STEP_RATE_1;
 CX_REG.RENDER_TARGET           :=ctx.rt_info^.SHADERDATA.RENDER_TARGET           ;

 pa.Init;
 pp:=@pa;

 ctx.rt_info^.ShaderGroup:=FetchShaderGroupRT(GPU_REGS,pp);
 Assert(ctx.rt_info^.ShaderGroup<>nil);
 //recheck shaders

 RP_KEY.Clear;

 if (ctx.rt_info^.RT_COUNT<>0) then
 For i:=0 to ctx.rt_info^.RT_COUNT-1 do
  begin

   if (ctx.rt_info^.RT_INFO[i].CMASK_INFO.KEY.Addr<>nil) then
   begin

    meta_instance:=ctx.node^.scope.find_buffer_resource_instance(R_CMASK,
                                                                 ctx.rt_info^.RT_INFO[i].CMASK_INFO.KEY.Addr,
                                                                 ctx.rt_info^.RT_INFO[i].CMASK_INFO.SIZE);
    Assert(meta_instance<>nil);

    if meta_instance^.resource^.rclear then
    begin
     //-TM_READ +TM_CLEAR
     ctx.rt_info^.RT_INFO[i].IMAGE_USAGE:=ctx.rt_info^.RT_INFO[i].IMAGE_USAGE and (not TM_READ) or TM_CLEAR;

     meta_instance^.resource^.rclear:=False;
    end;

   end;

   if (ctx.rt_info^.RT_INFO[i].FImageInfo.params.invalid<>0) then
   begin
    //skip
    color_instance[i]:=nil;
   end else
   begin
    color_instance[i]:=ctx.node^.scope.find_image_resource_instance(ctx.rt_info^.RT_INFO[i].FImageInfo);

    Assert(color_instance[i]<>nil);

    if (color_instance[i]^.curr.clr_usage<>ctx.rt_info^.RT_INFO[i].IMAGE_USAGE) then
    begin
     color_instance[i]^.curr.clr_usage:=ctx.rt_info^.RT_INFO[i].IMAGE_USAGE;
     color_instance[i]^.curr.mem_usage:=ctx.rt_info^.RT_INFO[i].IMAGE_USAGE;
     //TODO: next usage!
    end;

   end;

   //TODO: fixup cformat

   flag:=0;
   if (color_instance[i]<>nil) then
   begin
    flag:=GetMixedFlag(color_instance[i]^.curr);
   end;

   //TODO: fixup cformat

   RP_KEY.AddColorAt(ctx.rt_info^.RT_INFO[i].attachment,
                     ctx.rt_info^.RT_INFO[i].FImageInfo.cformat,
                     ctx.rt_info^.RT_INFO[i].IMAGE_USAGE or
                     flag,
                     ctx.rt_info^.RT_INFO[i].FImageInfo.params.samples);

  end;

 if ctx.rt_info^.DB_ENABLE then
 begin

  //set clear flag on cleared htile
  if (ctx.rt_info^.DB_INFO.HTILE_INFO.TILE_SURFACE_ENABLE<>0) then
  begin
   meta_instance:=ctx.node^.scope.find_buffer_resource_instance(R_HTILE,
                                                                 ctx.rt_info^.DB_INFO.HTILE_INFO.KEY.Addr,
                                                                 ctx.rt_info^.DB_INFO.HTILE_INFO.SIZE);
   Assert(meta_instance<>nil);

   if meta_instance^.resource^.rclear then
   begin
    //-TM_READ +TM_CLEAR
    ctx.rt_info^.DB_INFO.DEPTH_USAGE:=ctx.rt_info^.DB_INFO.DEPTH_USAGE and (not TM_READ) or TM_CLEAR;

    meta_instance^.resource^.rclear:=False;
   end;

  end;

  d_instance:=ctx.node^.scope.find_image_resource_instance(GetDepthOnly  (ctx.rt_info^.DB_INFO.FImageInfo));

  if (d_instance<>nil) then
  if (d_instance^.curr.dsa_usage<>ctx.rt_info^.DB_INFO.DEPTH_USAGE) then
  begin
   d_instance^.curr.dsa_usage:=ctx.rt_info^.DB_INFO.DEPTH_USAGE;
   d_instance^.curr.mem_usage:=ctx.rt_info^.DB_INFO.DEPTH_USAGE;
   //TODO: next usage!
  end;

  //TODO: fixup cformat

  RP_KEY.AddDepthAt(ctx.rt_info^.RT_COUNT, //add to last attachment id
                    ctx.rt_info^.DB_INFO.FImageInfo.cformat,
                    ctx.rt_info^.DB_INFO.DEPTH_USAGE,
                    ctx.rt_info^.DB_INFO.STENCIL_USAGE,
                    ctx.rt_info^.DB_INFO.FImageInfo.params.samples);

  RP_KEY.SetZorderStage(ctx.rt_info^.DB_INFO.zorder_stage);

 end;

 //DumpShaderGroup(ctx.rt_info^.ShaderGroup);

 RP:=FetchRenderPass(ctx.Cmd,@RP_KEY);

 if (RP=nil) then
 begin
  DumpShaderGroup(ctx.rt_info^.ShaderGroup);

  Assert(false,'FetchRenderPass');
 end;

 GP_KEY.Clear;

 GP_KEY.FRenderPass :=RP;
 GP_KEY.FShaderGroup:=ctx.rt_info^.ShaderGroup;

 GP_KEY.SetBlendInfo(ctx.rt_info^.BLEND_INFO.logicOp,@ctx.rt_info^.BLEND_INFO.blendConstants);

 GP_KEY.SetPrimType (ctx.rt_info^.PRIM_TYPE,GP_KEY.FShaderGroup.FKey.FPrimtype);
 GP_KEY.SetPrimReset(ctx.rt_info^.PRIM_RESET);

 if (ctx.rt_info^.VP_COUNT<>0) then
 For i:=0 to ctx.rt_info^.VP_COUNT-1 do
  begin
   GP_KEY.AddVPort(ctx.rt_info^.VPORT[i],ctx.rt_info^.SCISSOR[i]);
  end;

 if (ctx.rt_info^.RT_COUNT<>0) then
 For i:=0 to ctx.rt_info^.RT_COUNT-1 do
  begin
   GP_KEY.AddBlend(ctx.rt_info^.RT_INFO[i].blend);
  end;

 FAttrBuilder:=Default(TvAttrBuilder);
 ctx.rt_info^.ShaderGroup.ExportAttrBuilder(FAttrBuilder,@ctx.rt_info^.USERDATA);

 if not limits.VK_EXT_vertex_input_dynamic_state then
 begin
  GP_KEY.SetVertexInput(FAttrBuilder);
 end;

 GP_KEY.rasterizer   :=ctx.rt_info^.RASTERIZATION.State;
 GP_KEY.ClipSpace    :=ctx.rt_info^.RASTERIZATION.ClipSpace;
 GP_KEY.DepthClip    :=ctx.rt_info^.RASTERIZATION.DepthClip;
 GP_KEY.multisampling:=ctx.rt_info^.MULTISAMPLE;

 GP_KEY.SetProvoking(TVkProvokingVertexModeEXT(ctx.rt_info^.PROVOKING));

 if ctx.rt_info^.DB_ENABLE then
 begin
  GP_KEY.DepthStencil:=ctx.rt_info^.DB_INFO.ds_state;
 end;

 GP:=FetchGraphicsPipeline(ctx.Cmd,@GP_KEY);

 if (GP=nil) then
 begin
  LOG_TRACE(stderr,'FetchGraphicsPipeline=nil');

  DumpShaderGroup(ctx.rt_info^.ShaderGroup);

  Assert (false ,'FetchGraphicsPipeline=nil');
 end;

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FB_KEY:=Default(TvFramebufferImagelessKey);

  FB_KEY.SetRenderPass(RP);
  FB_KEY.SetSize(ctx.rt_info^.SCREEN_SIZE);

  if (ctx.rt_info^.RT_COUNT<>0) then
  For i:=0 to ctx.rt_info^.RT_COUNT-1 do
   begin
    //TODO: fixup cformat

    FB_KEY.AddImageAt(ctx.rt_info^.RT_INFO[i].FImageInfo,ctx.rt_info^.RT_INFO[i].FImageView);
   end;

  if ctx.rt_info^.DB_ENABLE then
  begin
   //TODO: fixup cformat

   FB_KEY.AddImageAt(ctx.rt_info^.DB_INFO.FImageInfo,ctx.rt_info^.DB_INFO.FImageView);
  end;
 end else
 begin
  FB_KEY2:=Default(TvFramebufferBindedKey);

  FB_KEY2.SetRenderPass(RP);
  FB_KEY2.SetSize(ctx.rt_info^.SCREEN_SIZE);
 end;

 ctx.Render:=Default(TvRenderPassBeginInfo);

 ctx.Render.SetRenderPass(RP);
 ctx.Render.SetRenderArea(ctx.rt_info^.SCREEN_RECT);

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FB:=FetchFramebufferImageless(ctx.Cmd,@FB_KEY);
  ctx.Render.SetFramebuffer(FB);
 end;

 if (ctx.rt_info^.RT_COUNT<>0) then
 For i:=0 to ctx.rt_info^.RT_COUNT-1 do
  begin

   //ri:=TvImage2(color_instance[i]^.resource^.rimage);
   ri:=nil;

   if (ri<>nil) then
   begin
    ctx.Cmd.RefTo(ri);
   end;

   if (ri<>nil) then
   if (ri.is_invalid) then
   begin
    color_instance[i]^.resource^.rimage:=nil;
    ri:=nil;
   end;

   if (ri=nil) then
   begin

    img_usage:=[];
    if (color_instance[i]<>nil) then
    begin
     {[iu_attachment]}
     img_usage:=color_instance[i]^.curr.img_usage;
    end;

    ri:=FetchImageForce(ctx,
                        ctx.rt_info^.RT_INFO[i].FImageInfo,
                        img_usage);

    if (color_instance[i]<>nil) then
    begin
     color_instance[i]^.resource^.rimage:=ri;
    end;
   end;

   pm4_load_from(ctx.Cmd,ri,ctx.rt_info^.RT_INFO[i].IMAGE_USAGE);

   iv:=ri.FetchView(ctx.Cmd,ctx.rt_info^.RT_INFO[i].FImageView,iu_attachment);

   if (color_instance[i]<>nil) then
   begin
    ri.PushBarrier(ctx.Cmd,
                   GetAccessMaskImg(color_instance[i]^.curr),
                   GetImageLayout(color_instance[i]^.curr),
                   ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                   ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );
   end;

   //

   ctx.Render.AddClearColor(ctx.rt_info^.RT_INFO[i].CLEAR_COLOR);

   LOG_TRACE('BindFrame:->[',i,']'#13#10,
           ' 0x',HexStr(ri.FHandle,16),':',GetVkFormatStr(ri.key.cformat),':',ri.FName,'->'#13#10,
           ' 0x',HexStr(iv.FHandle,16),':',GetVkFormatStr(iv.key.cformat),':',iv.FName);

   //
   if limits.VK_KHR_imageless_framebuffer then
   begin
    ctx.Render.AddImageView(iv);
   end else
   begin
    FB_KEY2.AddImageView(iv);
   end;
   //

  end;

 if ctx.rt_info^.DB_ENABLE then
 begin

  d_instance:=ctx.node^.scope.find_image_resource_instance(GetDepthOnly  (ctx.rt_info^.DB_INFO.FImageInfo));
  s_instance:=ctx.node^.scope.find_image_resource_instance(GetStencilOnly(ctx.rt_info^.DB_INFO.FImageInfo));

  ri:=nil;
  rd:=nil;
  rs:=nil;

  {
  if (d_instance<>nil) then
  begin
   rd:=TvCustomImage2(d_instance^.resource^.rimage);
  end;
  }

  if (rd<>nil) then
  begin
   ctx.Cmd.RefTo(rd);
  end;

  if (rd<>nil) then
  if (rd.is_invalid) then
  begin
   d_instance^.resource^.rimage:=nil;
   rd:=nil;
  end;

  {
  if (s_instance<>nil) then
  begin
   rs:=TvCustomImage2(s_instance^.resource^.rimage);
  end;
  }

  if (rs<>nil) then
  begin
   ctx.Cmd.RefTo(rs);
  end;

  if (rs<>nil) then
  if (rs.is_invalid) then
  begin
   s_instance^.resource^.rimage:=nil;
   rs:=nil;
  end;

  if (rd<>nil) then
  begin
   ri:=TvImage2(rd.Parent);
  end else
  if (rs<>nil) then
  begin
   ri:=TvImage2(rs.Parent);
  end;

  if (ri<>nil) then
  if (ri.DepthOnly  <>rd) or
     (ri.StencilOnly<>rs) then
  begin
   ri:=nil;
   rd:=nil;
   rs:=nil;
  end;

  //

  if (ri=nil) then
  begin
   ri:=FetchImageForce(ctx,
                       ctx.rt_info^.DB_INFO.FImageInfo,
                       [iu_depthstenc]);

   Assert(ri<>nil);

   rd:=ri.DepthOnly;
   rs:=ri.StencilOnly;

   if (d_instance<>nil) then
   begin
    d_instance^.resource^.rimage:=rd;

    ctx.RefToParent(ri);
   end;

   if (s_instance<>nil) then
   begin
    s_instance^.resource^.rimage:=rs;

    ctx.RefToParent(ri);
   end;
  end;

  //

  pm4_load_from(ctx.Cmd,rd,ctx.rt_info^.DB_INFO.DEPTH_USAGE);
  pm4_load_from(ctx.Cmd,rs,ctx.rt_info^.DB_INFO.STENCIL_USAGE);

  iv:=ri.FetchView(ctx.Cmd,ctx.rt_info^.DB_INFO.FImageView,iu_depthstenc);

  ri.PushBarrier(ctx.Cmd,
                 GetDepthStencilAccessAttachMask(ctx.rt_info^.DB_INFO.DEPTH_USAGE,ctx.rt_info^.DB_INFO.STENCIL_USAGE),
                 GetDepthStencilSendLayout(ctx.rt_info^.DB_INFO.DEPTH_USAGE,ctx.rt_info^.DB_INFO.STENCIL_USAGE),
                 ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                 ctx.rt_info^.DB_INFO.zorder_stage
                 );

  //

  ctx.Render.AddClearColor(ctx.rt_info^.DB_INFO.CLEAR_VALUE);

  LOG_TRACE('BindDepth:->'#13#10,
          ' 0x',HexStr(ri.FHandle,16),':',GetVkFormatStr(ri.key.cformat),':',ri.FName,'->'#13#10,
          ' 0x',HexStr(iv.FHandle,16),':',GetVkFormatStr(iv.key.cformat),':',iv.FName);

  //
  if limits.VK_KHR_imageless_framebuffer then
  begin
   ctx.Render.AddImageView(iv);
  end else
  begin
   FB_KEY2.AddImageView(iv);
  end;
  //

 end;

 if not limits.VK_KHR_imageless_framebuffer then
 begin
  FB:=FetchFramebufferBinded(ctx.Cmd,@FB_KEY2);
  ctx.Render.SetFramebuffer(FB);
 end;

 ////////
 FUniformBuilder:=Default(TvUniformBuilder);
 ctx.rt_info^.ShaderGroup.ExportUnifBuilder(FUniformBuilder,@ctx.rt_info^.USERDATA);

 Prepare_Uniforms(ctx,BP_GRAPHICS,FUniformBuilder);
 ////////

 DumpShaderGroup(ctx.rt_info^.ShaderGroup);

 if not ctx.Cmd.BeginRenderPass(@ctx.Render,GP) then
 begin
  LOG_TRACE(stderr,'BeginRenderPass(ctx.Render)');

  DumpShaderGroup(ctx.rt_info^.ShaderGroup);

  Assert (false ,'BeginRenderPass(ctx.Render)');
 end;

 ctx.Cmd.SetVertexInput   (FAttrBuilder);
 ctx.Cmd.BindVertexBuffers(FAttrBuilder);

 Bind_Uniforms(ctx,
               BP_GRAPHICS,
               FUniformBuilder);

 Bind_Pushs(ctx,ctx.rt_info^.ShaderGroup,@ctx.rt_info^.USERDATA);

end;

procedure pm4_Writeback_After(var ctx:t_me_render_context);
var
 //i:Integer;

 ri:TvImage2;
 //rd:TvCustomImage2;
 //rs:TvCustomImage2;

 resource_instance:p_pm4_resource_instance;

 //d_instance:p_pm4_resource_instance;
 //s_instance:p_pm4_resource_instance;
begin
 //write back

 resource_instance:=ctx.node^.scope.Min;

 while (resource_instance<>nil) do
 begin

  if (resource_instance^.resource^.rtype=R_IMG) then
  begin
   ri:=TvImage2(resource_instance^.resource^.rimage);

   if (ri<>nil) then
   if not ri.IsDepthAndStencil then
   begin

    //is write on current stage
    if ((resource_instance^.curr.mem_usage and (TM_WRITE or TM_CLEAR))<>0) then
    begin

     ri.mark_init;

     //is used in fuzzy match resources
     if (resource_instance^.next_overlap.mem_usage<>0) then
     begin
      pm4_write_back(ctx.Cmd,ri);
      //
      resource_instance^.resource^.rwriteback:=False;
     end else
     begin
      //
      resource_instance^.resource^.rwriteback:=True;
     end;

    end;
   end;

  end;

  resource_instance:=ctx.node^.scope.Next(resource_instance);
 end;

 {
 if ctx.rt_info^.DB_ENABLE then
 begin

  d_instance:=ctx.node^.scope.find_image_resource_instance(GetDepthOnly  (ctx.rt_info^.DB_INFO.FImageInfo));
  s_instance:=ctx.node^.scope.find_image_resource_instance(GetStencilOnly(ctx.rt_info^.DB_INFO.FImageInfo));

  ri:=nil;
  rd:=nil;
  rs:=nil;

  if (d_instance<>nil) then
  begin
   rd:=TvCustomImage2(d_instance^.resource^.rimage);
  end;

  if (s_instance<>nil) then
  begin
   rs:=TvCustomImage2(s_instance^.resource^.rimage);
  end;

  if (rd<>nil) then
  begin
   rd.mark_init;

   Assert(d_instance<>nil);

   if (d_instance^.next_overlap.mem_usage<>0) then
   begin
    pm4_write_back(ctx.Cmd,rd);
    //
    d_instance^.resource^.rwriteback:=False;
   end else
   begin
    //
    d_instance^.resource^.rwriteback:=True;
   end;

  end;

  //

  if (rs<>nil) then
  begin
   rs.mark_init;

   Assert(s_instance<>nil);

   if (s_instance^.next_overlap.mem_usage<>0) then
   begin
    pm4_write_back(ctx.Cmd,rs);
    //
    s_instance^.resource^.rwriteback:=False;
   end else
   begin
    //
    s_instance^.resource^.rwriteback:=True;
   end;

  end;

  //
 end;
 }

 //write back
end;

procedure pm4_Writeback_Finish(var ctx:t_me_render_context);
var
 ri:TvImage2;
 ht:TvMetaHtile;
 hc:TvMetaCmask;

 resource:p_pm4_resource;
begin
 if (ctx.stream=nil) then Exit;

 //write back

 resource:=ctx.stream^.resource_set.Min;

 while (resource<>nil) do
 begin

  if resource^.rwriteback then
  begin

   if (resource^.rtype=R_IMG) then
   begin

    ri:=TvImage2(resource^.rimage);

    Assert(ri<>nil);

    //
    pm4_write_back(ctx.Cmd,ri);
    //
    resource^.rwriteback:=False;
   end;

  end;

  if (resource^.rtype=R_HTILE) then
  begin
   ht:=FetchHtile(ctx.Cmd,resource^.rkey,resource^.rsize);

   ht.rclear:=resource^.rclear;
  end else
  if (resource^.rtype=R_CMASK) then
  begin
   hc:=FetchCmask(ctx.Cmd,resource^.rkey,resource^.rsize);

   hc.rclear:=resource^.rclear;
  end;

  resource:=ctx.stream^.resource_set.Next(resource);
 end;

 //write back
end;

procedure pm4_Hint(var ctx:t_me_render_context;node:p_pm4_node_hint);
begin
 ctx.InsertLabel(PChar(@node^.data));
end;

procedure pm4_Draw(var ctx:t_me_render_context;node:p_pm4_node_draw);
begin
 ctx.rt_info:=@node^.rt_info;

 if (ctx.rt_info^.RT_COUNT=0) and (not ctx.rt_info^.DB_ENABLE) then
 begin
  ctx.InsertLabel('decompress Dcc/Depth/Fmask');
  //zero attachment (decompress Dcc/Depth/Fmask) skip
  Exit;
 end;

 if (ctx.rt_info^.VP_COUNT=0) then
 begin
  //no viewports?
  Exit;
 end;

 //
 pm4_InitStream(ctx);
 //

 //if not ctx.WaitConfirmOrSwitch then Exit;

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 //

 if (node^.ntype<>ntClearDepth) then
 begin
  pm4_DrawPrepare(ctx);
 end;

 ctx.Cmd.FinstanceCount:=node^.numInstances;
 ctx.Cmd.FINDEX_TYPE   :=TVkIndexType(node^.INDEX_TYPE);

 case node^.ntype of
  ntDrawIndex2:
   begin
    LOG_TRACE(HexStr(node^.id,16),':DrawIndexOffset2(',HexStr(Pointer(node^.indexBase)),',',
                                                     node^.indexOffset ,',',
                                                     node^.vertexOffset,',',
                                                     node^.indexCount  ,')');

    ctx.Cmd.DrawIndexOffset2(Pointer(node^.indexBase),
                             node^.indexOffset,
                             node^.vertexOffset,
                             node^.indexCount);
   end;
  ntDrawIndexOffset2:
   begin
    LOG_TRACE(HexStr(node^.id,16),':DrawIndexOffset2(',HexStr(Pointer(node^.indexBase)),',',
                                                     node^.indexOffset ,',',
                                                     node^.vertexOffset,',',
                                                     node^.indexCount  ,')');

    ctx.Cmd.DrawIndexOffset2(Pointer(node^.indexBase),
                             node^.indexOffset,
                             node^.vertexOffset,
                             node^.indexCount);
   end;
  ntDrawIndexAuto:
   begin
    LOG_TRACE(HexStr(node^.id,16),':DrawIndexAuto(',node^.vertexOffset,',',
                                                  node^.indexCount  ,')');

    ctx.Cmd.DrawIndexAuto(node^.vertexOffset,
                          node^.indexCount);
   end;
  ntClearDepth:
   begin
    LOG_TRACE(HexStr(node^.id,16),':ClearDepth');

    pm4_ClearDepth(node^.rt_info,ctx);
   end;

  ntDrawIndirect:
   begin
    LOG_TRACE(HexStr(node^.id,16),':DrawIndirect(',HexStr(Pointer(node^.indirectBase)),',',
                                                   node^.dataOffset,')');

    ctx.Cmd.DrawIndirect(
     node^.vertexOffset,
     Pointer(node^.indirectBase),
     node^.dataOffset,
     sizeof(TVkDrawIndexedIndirectCommand),
     node^.count);

   end;

  ntDrawIndexIndirect:
   begin
    LOG_TRACE(HexStr(node^.id,16),':DrawIndexIndirect(',HexStr(Pointer(node^.indirectBase)),',',
                                                      node^.dataOffset,')');

    ctx.Cmd.DrawIndexIndirect(
     Pointer(node^.indexBase),
     node^.indexOffset,
     node^.vertexOffset,
     node^.indexCount,
     //
     Pointer(node^.indirectBase),
     nil,
     node^.dataOffset,
     sizeof(TVkDrawIndexedIndirectCommand),
     1
    );
   end;

  ntDrawIndexIndirectCountMulti:
   begin
    LOG_TRACE(HexStr(node^.id,16),':DrawIndexIndirectCountMulti(',HexStr(Pointer(node^.indirectBase)),',',
                                                                HexStr(Pointer(node^.countAddr   )),',',
                                                                node^.dataOffset,',',
                                                                node^.stride    ,',',
                                                                node^.count     ,')');

    ctx.Cmd.DrawIndexIndirect(
     Pointer(node^.indexBase),
     node^.indexOffset,
     node^.vertexOffset,
     node^.indexCount,
     //
     Pointer(node^.indirectBase),
     Pointer(node^.countAddr),
     node^.dataOffset,
     node^.stride,
     node^.count
    );
   end;
  else;
   Assert(false,'pm4_Draw');
 end;

 /////////

 pm4_Writeback_After(ctx);

 ctx.FlushParent;
end;

procedure pm4_Resolve(var ctx:t_me_render_context;node:p_pm4_node_Resolve);
var
 ri_src,ri_dst:TvImage2;
 range:TVkImageResolve;

begin
 //
 pm4_InitStream(ctx);
 //

 //if not ctx.WaitConfirmOrSwitch then Exit;

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 ctx.Cmd.EndRenderPass;

 ri_src:=FetchImageForce(ctx,
                         node^.RT[0].FImageInfo,
                         [iu_transfer]
                        );

 Assert(ri_src<>nil);

 ri_dst:=FetchImageForce(ctx,
                         node^.RT[1].FImageInfo,
                         [iu_transfer]
                        );

 Assert(ri_dst<>nil);

 ri_src.PushBarrier(ctx.Cmd,
                    ord(VK_ACCESS_TRANSFER_READ_BIT),
                    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 ri_dst.PushBarrier(ctx.Cmd,
                    ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                    ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 range:=Default(TVkImageResolve);

 range.srcSubresource:=ri_src.GetSubresLayer;
 range.dstSubresource:=ri_dst.GetSubresLayer;

 range.srcOffset.Create(node^.SCREEN.offset.x,node^.SCREEN.offset.y,0);
 range.dstOffset:=range.srcOffset;

 range.extent.Create(node^.SCREEN.extent.width,node^.SCREEN.extent.height,1);

 ctx.Cmd.ResolveImage(ri_src.FHandle,
                      VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                      ri_dst.FHandle,
                      VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                      1,@range);

 ctx.FlushParent;
end;

procedure pm4_FastClear(var ctx:t_me_render_context;node:p_pm4_node_FastClear);
var
 ri:TvImage2;
 range:TVkImageSubresourceRange;

 resource_instance:p_pm4_resource_instance;
begin
 {
 //
 pm4_InitStream(ctx);
 //

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 ctx.Cmd.EndRenderPass;

 resource_instance:=ctx.node^.scope.find_image_resource_instance(node^.RT.FImageInfo);
 Assert(resource_instance<>nil);

 ri:=FetchImage(ctx.Cmd,
                node^.RT.FImageInfo,
                resource_instance^.curr.img_usage
                );

 ri.PushBarrier(ctx.Cmd,
                ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 range:=ri.GetSubresRange;

 ctx.Cmd.ClearColorImage(ri.FHandle,
                         VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                         @node^.RT.CLEAR_COLOR,
                         1,@range);

 //writeback
 ri.mark_init;

 if (resource_instance^.next_overlap.mem_usage<>0) then
 begin
  pm4_write_back(ctx.Cmd,ri);
  //
  resource_instance^.resource^.rwriteback:=False;
 end else
 begin
  //
  resource_instance^.resource^.rwriteback:=True;
 end;
 //writeback
 }

end;

function Detect_buf_meta(var ctx:t_me_render_context;
                         var UniformBuilder:TvUniformBuilder):Boolean;
var
 i:Integer;

 buffer:p_pm4_resource;

 ht:TvMetaHtile;
 hc:TvMetaCmask;
begin
 Result:=False;

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  begin

   //get buffer with write usege
   if ((memuse and TM_WRITE)<>0) then
   begin

    buffer:=ctx.stream^.find_buffer_resource(R_BUF,addr,size);

    if (buffer<>nil) then
    begin

     ht:=FetchHtile(ctx.Cmd,buffer^.rkey,size,False);
     if (ht<>nil) then
     begin
      Exit(True);
     end;

     hc:=FetchCmask(ctx.Cmd,buffer^.rkey,size,False);
     if (hc<>nil) then
     begin
      Exit(True);
     end;

    end;

   end;

  end;
 end;
 //buffers

end;

procedure Prepare_buf_clear(var ctx:t_me_render_context;
                            var UniformBuilder:TvUniformBuilder);
var
 i:Integer;

 resource_instance:p_pm4_resource_instance;
 buffer,meta:p_pm4_resource;

 hb:TvMetaBuffer;
begin
 buffer:=nil;

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  begin

   //get buffer with write usege
   if ((memuse and TM_WRITE)<>0) then
   begin
    resource_instance:=ctx.node^.scope.find_buffer_resource_instance(R_BUF,addr,size);
    if (resource_instance<>nil) then
    begin
     buffer:=resource_instance^.resource;
     Break;
    end;
   end;

  end;
 end;
 //buffers

 //TODO: get clear value!

 Assert(buffer<>nil);

 //set flag by buffer in current stream
 buffer^.rclear:=True;

 //set flag by buffer to next stream
 hb:=FetchBuffer(ctx.Cmd,buffer^.rkey.Addr,buffer^.rsize);

 Assert(hb<>nil);

 hb.rclear:=True;

 //set flag by htile in current stream
 meta:=ctx.stream^.find_buffer_resource(R_HTILE,buffer^.rkey.Addr,buffer^.rsize);
 //
 if (meta<>nil) then
 begin
  meta^.rclear:=True;
 end;

 //set flag by cmask in current stream
 meta:=ctx.stream^.find_buffer_resource(R_CMASK,buffer^.rkey.Addr,buffer^.rsize);
 //
 if (meta<>nil) then
 begin
  meta^.rclear:=True;
 end;

end;

function pm4_DispatchPrepare(var ctx:t_me_render_context;node:p_pm4_node_Dispatch):Boolean;
var
 dst:PGPU_USERDATA;

 CP_KEY:TvComputePipelineKey;
 CP:TvComputePipeline2;

 FUniformBuilder:TvUniformBuilder;

 //
 GPU_REGS:TGPU_REGS;
 pa:TPushConstAllocator;
 pp:PPushConstAllocator;
begin
 Result:=False;
 ////////

 //hack
 dst:=Pointer(@node^.COMPUTE_GROUP.COMPUTE_USER_DATA)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 //recheck shaders
 GPU_REGS.SC_REG:=@node^.COMPUTE_GROUP;

 pa.Init;
 pp:=@pa;

 node^.ShaderGroup:=FetchShaderGroupCS(GPU_REGS,pp);
 Assert(node^.ShaderGroup<>nil);
 //recheck shaders

 CP_KEY.FShaderGroup:=node^.ShaderGroup;
 CP:=FetchComputePipeline(ctx.Cmd,@CP_KEY);

 FUniformBuilder:=Default(TvUniformBuilder);
 CP_KEY.FShaderGroup.ExportUnifBuilder(FUniformBuilder,dst);

 //htile/cmask/rt heuristic
 if (CP_KEY.FShaderGroup.FKey.FShaders[vShaderStageCs].IsCSClearShader) then
 begin
  Prepare_buf_clear(ctx,FUniformBuilder);
  //
  ctx.InsertLabel('clear htile/cmask/rt');
 end else
 if Detect_buf_meta(ctx,FUniformBuilder) then
 begin
  LOG_TRACE('Detect_buf_meta:0x',HexStr(CP_KEY.FShaderGroup.FKey.FShaders[vShaderStageCs].FHash_gcn,16));
  //
  Prepare_buf_clear(ctx,FUniformBuilder);
  //
  ctx.InsertLabel('clear htile/cmask/rt');
 end;

 Prepare_Uniforms(ctx,BP_COMPUTE,FUniformBuilder);
 ////////

 DumpShaderGroup(CP_KEY.FShaderGroup);

 if not ctx.Cmd.BindCompute(CP) then
 begin
  LOG_TRACE(stderr,'BindCompute(CP)');

  DumpShaderGroup(CP_KEY.FShaderGroup);

  Assert(false  ,'BindCompute(CP)');
 end;

 Bind_Uniforms(ctx,
               BP_COMPUTE,
               FUniformBuilder);

 Bind_Pushs(ctx,CP_KEY.FShaderGroup,dst);

 Result:=True;
end;

procedure pm4_DispatchDirect(var ctx:t_me_render_context;node:p_pm4_node_DispatchDirect);
begin
 //
 pm4_InitStream(ctx);
 //

 //if not ctx.WaitConfirmOrSwitch then Exit;

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 //
 ctx.Cmd.EndRenderPass;

 if not pm4_DispatchPrepare(ctx,node) then Exit;

 LOG_TRACE('DispatchDirect(',node^.DIM_X,',',node^.DIM_Y,',',node^.DIM_Z,')');

 ctx.Cmd.DispatchDirect(node^.DIM_X,node^.DIM_Y,node^.DIM_Z);

 /////////

 pm4_Writeback_After(ctx);

 ctx.FlushParent;
end;

procedure pm4_DispatchIndirect(var ctx:t_me_render_context;node:p_pm4_node_DispatchIndirect);
begin
 //
 pm4_InitStream(ctx);
 //

 //if not ctx.WaitConfirmOrSwitch then Exit;

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 //
 ctx.Cmd.EndRenderPass;

 if not pm4_DispatchPrepare(ctx,node) then Exit;

 LOG_TRACE(HexStr(node^.id,16),':DispatchIndirect(0x',HexStr(node^.BASE,11),',0x',HexStr(node^.Offset,8),')');

 ctx.Cmd.DispatchIndirect(Pointer(node^.BASE),node^.Offset);

 /////////

 pm4_Writeback_After(ctx);

 ctx.FlushParent;
end;

function mul_div_u64(m,d,v:QWORD):QWORD; sysv_abi_default; assembler; nostackframe;
asm
 movq v,%rax
 mulq m
 divq d
end;

const
 GLOBAL_CLOCK_FREQUENCY  =100*1000*1000; //100MHz
 GPU_CORE_CLOCK_FREQUENCY=800*1000*1000; //800MHz

 //neo mode & ext_gpu_timer -> 911*000*000


procedure pm4_EventWriteEop(var ctx:t_me_render_context;node:p_pm4_node_EventWriteEop);
var
 curr,diff:QWORD;
 addr_dmem:Pointer;
 data_size:Byte;
begin
 if not ctx.stream^.hint_repeat then
 begin
  ctx.InsertLabel(PChar('WriteEop:0x'+HexStr(QWORD(node^.addr),10)));
  if p_print_gpu_ops then
  begin
   LOG_TRACE('WriteEop:0x'+HexStr(QWORD(node^.addr),10));
  end;
  ctx.stream^.hint_repeat:=True;
 end;

 if not ctx.WaitConfirmOrSwitch then Exit;

 ctx.stream^.hint_repeat:=False;

 curr:=md_rdtsc_unit;
 diff:=curr-ctx.rel_time;

 if (node^.addr<>nil) then
 begin

  addr_dmem:=nil;
  if (node^.intSel <>EVENTWRITEEOP_INT_SEL_SEND_INT) and
     (node^.dataSel<>EVENTWRITEEOP_DATA_SEL_DISCARD) then
  begin
   addr_dmem:=get_dmem_ptr(node^.addr);
  end;

  if (addr_dmem<>nil) then
  Case node^.dataSel of
   //
   EVENTWRITEEOP_DATA_SEL_DISCARD:
    data_size:=0;

    //32bit data
   EVENTWRITEEOP_DATA_SEL_SEND_DATA32:
    begin
     PDWORD(addr_dmem)^:=node^.data;

     data_size:=4;
    end;

    //64bit data
   EVENTWRITEEOP_DATA_SEL_SEND_DATA64:
    begin
     PQWORD(addr_dmem)^:=node^.data;

     data_size:=8;
    end;

     //system 100Mhz global clock. (relative time)
   EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK:
    begin
     PQWORD(addr_dmem)^:=mul_div_u64(GLOBAL_CLOCK_FREQUENCY,UNIT_PER_SEC,diff);

     data_size:=8;
    end;

     //GPU 800Mhz clock.           (relative time)
   EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:
    begin
     PQWORD(addr_dmem)^:=mul_div_u64(GPU_CORE_CLOCK_FREQUENCY,UNIT_PER_SEC,diff);

     data_size:=8;
    end;

   else
    Assert(false,'pm4_EventWriteEop');
  end;

  if (addr_dmem<>nil) then
  vm_map_track_trigger(p_proc.p_vmspace,QWORD(node^.addr),QWORD(node^.addr)+data_size,nil,M_DMEM_WRITE);
 end;

 if (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT) or
    (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM) then
 begin
  ctx.me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;

end;

procedure pm4_SubmitFlipEop(var ctx:t_me_render_context;node:p_pm4_node_SubmitFlipEop);
begin
 if not ctx.stream^.hint_repeat then
 begin
  ctx.InsertLabel(PChar('SubmitFlipEop:0x'+HexStr(node^.eop_value,16)));
  ctx.stream^.hint_repeat:=True;
 end;

 if not ctx.WaitConfirmOrSwitch then Exit;

 ctx.stream^.hint_repeat:=False;

 if (ctx.me^.on_submit_flip_eop<>nil) then
 begin
  ctx.me^.on_submit_flip_eop(node^.eop_value);
 end;

 //do not trigger eop!
end;

function get_compute_pipe_id(buft:t_pm4_stream_type):Byte; inline;
begin
 Result:=ord(buft) - ord(stCompute0);
end;

procedure pm4_ReleaseMem(var ctx:t_me_render_context;node:p_pm4_node_ReleaseMem);
var
 curr,diff:QWORD;
 addr_dmem:Pointer;
 data_size:Byte;
begin
 if not ctx.stream^.hint_repeat then
 begin
  ctx.InsertLabel(PChar('ReleaseMem:0x'+HexStr(QWORD(node^.addr),10)));
  ctx.stream^.hint_repeat:=True;
 end;

 if not ctx.WaitConfirmOrSwitch then Exit;

 ctx.stream^.hint_repeat:=False;

 curr:=md_rdtsc_unit;
 diff:=curr-ctx.rel_time;

 if (node^.addr<>nil) then
 begin

  addr_dmem:=nil;
  if (node^.srcSel<>RELEASEMEM_DATA_SEL_DISCARD) then
  begin
   addr_dmem:=get_dmem_ptr(node^.addr);
  end;

  Case node^.dstSel of
   RELEASEMEM_DST_SEL_MEMORY:;
   RELEASEMEM_DST_SEL_L2    :Assert(false,'RELEASEMEM_DST_SEL_L2');
   else
    Assert(false,'pm4_ReleaseMem:dstSel');
  end;

  if (addr_dmem<>nil) then
  Case node^.srcSel of
   //
   RELEASEMEM_DATA_SEL_DISCARD:
    data_size:=0;

    //32bit data
   RELEASEMEM_DATA_SEL_SEND_DATA32:
    begin
     PDWORD(addr_dmem)^:=node^.data;

     data_size:=4;
    end;

    //64bit data
   RELEASEMEM_DATA_SEL_SEND_DATA64:
    begin
     PQWORD(addr_dmem)^:=node^.data;

     data_size:=8;
    end;

     //system 100Mhz global clock. (relative time)
   RELEASEMEM_DATA_SEL_SEND_GPU_CLOCK:
    begin
     PQWORD(addr_dmem)^:=mul_div_u64(GLOBAL_CLOCK_FREQUENCY,UNIT_PER_SEC,diff);

     data_size:=8;
    end;

     //GPU 800Mhz clock.           (relative time)
   RELEASEMEM_DATA_SEL_SEND_CP_PERFCOUNTER:
    begin
     PQWORD(addr_dmem)^:=mul_div_u64(GPU_CORE_CLOCK_FREQUENCY,UNIT_PER_SEC,diff);

     data_size:=8;
    end;

   else
    Assert(false,'pm4_ReleaseMem:srcSel');
  end;

  if (addr_dmem<>nil) then
  vm_map_track_trigger(p_proc.p_vmspace,QWORD(node^.addr),QWORD(node^.addr)+data_size,nil,M_DMEM_WRITE);
 end;

 if (node^.intSel=RELEASEMEM_INT_SEL_SEND_INT) or
    (node^.intSel=RELEASEMEM_INT_SEL_SEND_INT_ON_CONFIRM) then
 begin
  ctx.me^.knote_eventid(get_compute_pipe_id(ctx.stream^.buft),0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;

end;

procedure pm4_EventWrite(var ctx:t_me_render_context;node:p_pm4_node_EventWrite);
begin
 Case node^.eventType of
  CS_PARTIAL_FLUSH,            //CS
  CACHE_FLUSH_AND_INV_EVENT,   //CB,DB
  DB_CACHE_FLUSH_AND_INV,      //DB
  FLUSH_AND_INV_DB_META,       //HTILE
  FLUSH_AND_INV_CB_META,       //CMASK
  FLUSH_AND_INV_CB_PIXEL_DATA: //CB
   begin
    if (ctx.Cmd<>nil) and ctx.Cmd.IsAllocated then
    begin
     //GPU
     ctx.Cmd.WriteEvent(node^.eventType);
    end;
   end;
  //FLUSH_AND_INV_CB_DATA_TS   :LOG_TRACE(' eventType=FLUSH_AND_INV_CB_DATA_TS');
  THREAD_TRACE_MARKER:
   begin
    ctx.InsertLabel('THREAD_TRACE_MARKER');
   end;
  PIPELINESTAT_STOP:
   begin
    ctx.InsertLabel('PIPELINESTAT_STOP');
   end;
  PERFCOUNTER_START:
   begin
    ctx.InsertLabel('PERFCOUNTER_START');
   end;
  PERFCOUNTER_STOP:
   begin
    ctx.InsertLabel('PERFCOUNTER_STOP');
   end;
  PERFCOUNTER_SAMPLE:
   begin
    ctx.InsertLabel('PERFCOUNTER_SAMPLE');
   end;
  PIXEL_PIPE_STAT_RESET: //[OcclusionQuery] Reset this query
   begin
    LOG_WARNING(stderr,'TODO:PIXEL_PIPE_STAT_RESET');
   end;
  else
   begin
    LOG_CRITICAL(StdErr,'EventWrite eventType=0x',HexStr(node^.eventType,2));
    Assert (false ,'EventWrite eventType=0x'+HexStr(node^.eventType,2));
   end;

 end;

end;

var
 fake_zpass_counter:QWORD=0;

procedure pm4_PipeStatDump(var ctx:t_me_render_context;node:p_pm4_node_PipeStatDump);
const
 c_db_counts:array[0..1] of Byte=(8,16);
 c_db_stride:array[0..3] of Byte=(4,8,16,32);
 c_ready_mask_64=QWORD(1) shl 63;
 c_ready_mask_32=QWORD(1) shl 31;
var
 i,count,stride:Byte;
 instance_mask :Word;
 addr_dmem:Pointer;
begin
 if not ctx.WaitConfirmOrSwitch then Exit;

 count :=c_db_counts[p_neomode and 1];
 stride:=c_db_stride[node^.Control.stride];
 instance_mask:=node^.Control.instance_enable;

 addr_dmem:=get_dmem_ptr(Pointer(node^.address));

 fake_zpass_counter:=fake_zpass_counter+1;

 if (stride=4) then
 begin

  For i:=0 to count-1 do
   if (instance_mask and (1 shl i))<>0 then
   begin
    PDWORD(addr_dmem)[i]:=c_ready_mask_32 or fake_zpass_counter;
   end;

 end else
 begin

  For i:=0 to count-1 do
  begin
   if (instance_mask and (1 shl i))<>0 then
   begin
    PQWORD(addr_dmem)^:=c_ready_mask_64 or fake_zpass_counter;
   end;
   addr_dmem:=addr_dmem+stride;
  end;

 end;

end;

procedure pm4_EventWriteEos(var ctx:t_me_render_context;node:p_pm4_node_EventWriteEos);
var
 addr_dmem:Pointer;
begin

 if (node^.addr<>nil) then
 Case node^.command of

   //32bit data
  EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY:
   begin

    if (ctx.Cmd<>nil) and ctx.Cmd.IsAllocated then
    begin
     //GPU
     ctx.Cmd.WriteEos(node^.eventType,node^.addr,node^.data,false);
    end else
    begin
     //soft

     addr_dmem:=get_dmem_ptr(node^.addr);

     PDWORD(addr_dmem)^:=node^.data;

     vm_map_track_trigger(p_proc.p_vmspace,QWORD(node^.addr),QWORD(node^.addr)+4,nil,M_DMEM_WRITE);
    end;

   end;

  else
   Assert(false,'pm4_EventWriteEos');
 end;

end;

procedure pm4_WriteData(var ctx:t_me_render_context;node:p_pm4_node_WriteData);
var
 src_dmem:PDWORD;
 dst_dmem:PDWORD;
 byteSize:QWORD;
begin
 StartFrameCapture;

 case node^.dstSel of
  WRITE_DATA_DST_SEL_MEMORY_SYNC,  //writeDataInline
  WRITE_DATA_DST_SEL_TCL2,         //writeDataInlineThroughL2
  WRITE_DATA_DST_SEL_MEMORY_ASYNC:
    if (node^.dst<>nil) then
    begin

     if (ctx.Cmd<>nil) and ctx.Cmd.IsAllocated then
     begin
      //GPU
      byteSize:=node^.num_dw*SizeOf(DWORD);

      if p_print_gpu_ops then
      begin
       LOG_TRACE('[1]WriteData:0x',HexStr(QWORD(node^.src),10),'->',HexStr(QWORD(node^.dst),10),':size=0x',HexStr(byteSize,5));
      end;

      ctx.Cmd.dmaData1(node^.src,node^.dst,byteSize,node^.wrConfirm);
     end else
     begin
      //soft

      if p_print_gpu_ops then
      begin
       LOG_TRACE('[2]WriteData:0x',HexStr(QWORD(node^.src),10),'->',HexStr(QWORD(node^.dst),10),':size=0x',HexStr(byteSize,5));
      end;

      src_dmem:=get_dmem_ptr(node^.src);

      dst_dmem:=get_dmem_ptr(node^.dst);

      byteSize:=node^.num_dw*SizeOf(DWORD);

      Move(src_dmem^,dst_dmem^,byteSize);

      vm_map_track_trigger(p_proc.p_vmspace,QWORD(node^.dst),QWORD(node^.dst)+byteSize,nil,M_DMEM_WRITE);
     end;
    end;
  else
    Assert(false,'WriteData: dstSel=0x'+HexStr(node^.dstSel,1));
 end;

end;

const
 DmaDataStr:array[0..15] of Pchar=(
  {0} 'Memory',
  {1} 'Gds',
  {2} 'Data',
  {3} 'MemoryUsingL2',
  {4} 'Register',
  {5} '0x5',
  {6} '0x6',
  {7} '0x7',
  {8} '0x8',
  {9} '0x9',
  {A} '0xA',
  {B} '0xB',
  {C} 'RegisterNoIncrement',
  {D} '0xD',
  {E} '0xE',
  {F} '0xF'
 );

procedure pm4_DmaData(var ctx:t_me_render_context;node:p_pm4_node_DmaData);
var
 adrSrc:QWORD;
 adrDst:QWORD;
 adrSrc_dmem:Pointer;
 adrDst_dmem:Pointer;
 byteCount:DWORD;
 srcSel,dstSel:Byte;
begin

 StartFrameCapture;

 adrDst   :=node^.dst;
 adrSrc   :=node^.src;
 byteCount:=node^.numBytes;
 srcSel   :=node^.srcSel;
 dstSel   :=node^.dstSel;

 case (srcSel or (dstSel shl 4)) of
  (kDmaDataSrcMemory        or (kDmaDataDstMemory        shl 4)),
  (kDmaDataSrcMemoryUsingL2 or (kDmaDataDstMemory        shl 4)),
  (kDmaDataSrcMemory        or (kDmaDataDstMemoryUsingL2 shl 4)),
  (kDmaDataSrcMemoryUsingL2 or (kDmaDataDstMemoryUsingL2 shl 4)):
    begin

     if (ctx.Cmd<>nil) and ctx.Cmd.IsAllocated then
     begin
      //GPU

      ctx.Cmd.dmaData1(Pointer(adrSrc),Pointer(adrDst),byteCount,node^.cpSync<>0);

      //GPU
     end else
     begin
      //soft

      adrDst_dmem:=get_dmem_ptr(Pointer(adrDst));

      adrSrc_dmem:=get_dmem_ptr(Pointer(adrSrc));

      Move(adrSrc_dmem^,adrDst_dmem^,byteCount);

      vm_map_track_trigger(p_proc.p_vmspace,QWORD(adrDst),QWORD(adrDst)+byteCount,nil,M_DMEM_WRITE);

      //soft
     end;

    end;
  (kDmaDataSrcData          or (kDmaDataDstMemory        shl 4)),
  (kDmaDataSrcData          or (kDmaDataDstMemoryUsingL2 shl 4)):
    begin

     if (ctx.Cmd<>nil) and ctx.Cmd.IsAllocated then
     begin
      //GPU

      ctx.Cmd.dmaData2(DWORD(adrSrc),Pointer(adrDst),byteCount,node^.cpSync<>0);

      //GPU
     end else
     begin
      //soft

      adrDst_dmem:=get_dmem_ptr(Pointer(adrDst));

      FillDWORD(adrDst_dmem^,(byteCount div 4),DWORD(adrSrc));

      vm_map_track_trigger(p_proc.p_vmspace,QWORD(adrDst),QWORD(adrDst)+byteCount,nil,M_DMEM_WRITE);

      //soft
     end;

    end;
 else
    LOG_CRITICAL(StdErr,'DmaData: srcSel='+DmaDataStr[srcSel and 15]+' dstSel='+DmaDataStr[dstSel and 15]);
    Assert      (false,'DmaData: srcSel='+DmaDataStr[srcSel and 15]+' dstSel='+DmaDataStr[dstSel and 15]);
 end;

end;

function get_dce_label_id(addr_dmem:Pointer):Integer;
begin
 Result:=-1;

 if (QWORD(addr_dmem)>=QWORD(@dev_dce.dce_page^.labels)  ) and
    (QWORD(addr_dmem)< QWORD(@dev_dce.dce_page^.label_)+8) then
 begin
  Result:=(QWORD(addr_dmem)-QWORD(@dev_dce.dce_page^.labels)) div 8;
 end;
end;

Function me_test_mem(node:p_pm4_node_WaitRegMem;{var }dmem:PDWORD):Boolean;
var
 val,ref:DWORD;
begin
 {
 dmem:=nil;
 if not get_dmem_ptr(node^.pollAddr,@dmem,nil) then
 begin
  Assert(false,'addr:0x'+HexStr(node^.pollAddr)+' not in dmem!');
 end;
 }

 LOG_TRACE('me_test_mem:labels[',get_dce_label_id(dmem),']=',dmem^,' refValue=',node^.refValue,' compareFunc=',node^.compareFunc);

 val:=dmem^ and node^.mask;
 ref:=node^.refValue;
 Case node^.compareFunc of
  WAIT_REG_MEM_FUNC_ALWAYS       :Result:=True;
  WAIT_REG_MEM_FUNC_LESS         :Result:=(val<ref);
  WAIT_REG_MEM_FUNC_LESS_EQUAL   :Result:=(val<=ref);
  WAIT_REG_MEM_FUNC_EQUAL        :Result:=(val=ref);
  WAIT_REG_MEM_FUNC_NOT_EQUAL    :Result:=(val<>ref);
  WAIT_REG_MEM_FUNC_GREATER_EQUAL:Result:=(val>=ref);
  WAIT_REG_MEM_FUNC_GREATER      :Result:=(val>ref);
  else
   Assert(false,'me_test_mem');
 end;
end;

procedure t_me_wait_addr.add_reg(kq:Pointer);
begin
 if (Fdmem_addr<>nil) then
 begin
  Fregs_addr:=Fdmem_addr;
  gc_add_internal_ptr(kq,Fregs_addr,@Self);
 end;
end;

procedure t_me_wait_addr.del_reg(kq:Pointer);
begin
 if (Fregs_addr<>nil) then
 begin
  gc_del_internal_ptr(kq,Fregs_addr);
  Fregs_addr:=nil;
 end;
end;

procedure t_me_wait_addr.set_adr(kq,addr:Pointer);
begin
 if (Fcode_addr=addr) then Exit;

 del_reg(kq);

 Fcode_addr:=addr;
 Fdmem_addr:=get_dmem_ptr(addr);
end;

function SendWarnMsg(const s:RawByteString):Integer;
begin
 Result:=p_host_ipc.warning(s);
end;

procedure pm4_WaitRegMem(var ctx:t_me_render_context;node:p_pm4_node_WaitRegMem);
label
 _repeat,
 _reset;
var
 wait_addr:p_me_wait_addr;
begin
 if not ctx.stream^.hint_repeat then
 begin
  ctx.InsertLabel(PChar('WaitRegMem:0x'+HexStr(QWORD(node^.pollAddr),10)));
  ctx.stream^.hint_repeat:=True;
 end;

 if not ctx.WaitConfirmOrSwitch then Exit;

 ctx.stream^.hint_repeat:=False;

 wait_addr:=@ctx.me^.wait_ptr[ctx.stream^.buft];

 wait_addr^.set_adr(ctx.me^.gc_kqueue,node^.pollAddr);

 _repeat:

 if me_test_mem(node,wait_addr^.Fdmem_addr) then
 begin
  ctx.stream^.hint_loop:=0;
 end else
 begin
  wait_addr^.add_reg(ctx.me^.gc_kqueue);
  //
  Inc(ctx.stream^.hint_loop);
  //
  if wait_loop_detect then
  if (ctx.stream^.hint_loop>10000) then
  begin
   //loop detection
   if wait_loop_autoskip then
   begin
    LOG_TRACE(stderr,'WaitRegMem hang detected 0x',HexStr(QWORD(node^.pollAddr),10),' -> skip');
    goto _reset;
   end else
   begin
    LOG_TRACE(stderr,'WaitRegMem hang detected 0x',HexStr(QWORD(node^.pollAddr),10));
    //
    if SendWarnMsg('Hang in WaitRegMem instruction detected, skip instruction?')=0 then
    begin
     LOG_TRACE(stderr,' -> skip');
     goto _reset;
    end else
    begin
     LOG_TRACE(stderr,' -> repeat');
     ctx.stream^.hint_loop:=0;
    end;
    //
   end;
  end; //hint_loop
  //
  ctx.switch_task;
  //early check
  if (ctx.me^.sheduler.start=@ctx.me^.stall[ctx.stream^.buft]) then
  begin
   goto _repeat;
  end;
  //
  Exit; //dont reset wait addr
 end;

 _reset:
  ctx.stream^.hint_loop:=0;
  wait_addr^.set_adr(ctx.me^.gc_kqueue,nil);
end;

//

procedure pm4_LoadConstRam(var ctx:t_me_render_context;node:p_pm4_node_LoadConstRam);
var
 addr_dmem:Pointer;

 start:DWORD;
 __end:DWORD;
 size :DWORD;
begin
 //if not ctx.WaitConfirmOrSwitch then Exit;

 addr_dmem:=get_dmem_ptr(node^.addr);

 start:=node^.offset;
 __end:=start+(node^.num_dw*SizeOf(DWORD));

 if (start>CONST_RAM_SIZE) then
 begin
  start:=CONST_RAM_SIZE;
 end;

 if (__end>CONST_RAM_SIZE) then
 begin
  __end:=CONST_RAM_SIZE;
 end;

 size:=(__end-start);

 if p_print_gpu_ops then
 begin
  LOG_TRACE('LoadConstRam:0x',HexStr(QWORD(addr_dmem),10),'->[0x',HexStr(start,4),']:size=0x',HexStr(size,6));
 end;

 Move(addr_dmem^,ctx.me^.CONST_RAM[start],size);
end;

procedure pm4_DumpConstRam(var ctx:t_me_render_context;node:p_pm4_node_LoadConstRam);
var
 addr_dmem:Pointer;

 start:DWORD;
 __end:DWORD;
 size :DWORD;
begin
 //if not ctx.WaitConfirmOrSwitch then Exit;

 addr_dmem:=get_dmem_ptr(node^.addr);

 start:=node^.offset;
 __end:=start+(node^.num_dw*SizeOf(DWORD));

 if (start>CONST_RAM_SIZE) then
 begin
  start:=CONST_RAM_SIZE;
 end;

 if (__end>CONST_RAM_SIZE) then
 begin
  __end:=CONST_RAM_SIZE;
 end;

 size:=(__end-start);

 if p_print_gpu_ops then
 begin
  LOG_TRACE('DumpConstRam:[0x',HexStr(start,4),']->0x',HexStr(QWORD(addr_dmem),10),':size=0x',HexStr(size,6));
 end;

 Move(ctx.me^.CONST_RAM[start],addr_dmem^,size);

 ctx.BeginCmdBuffer;
 ctx.Cmd.AddPlannedTrigger(QWORD(node^.addr),QWORD(node^.addr)+size,nil);
end;

//

procedure pm4_IncrementCE(var ctx:t_me_render_context;node:p_pm4_node);
begin
 Inc(ctx.me^.CE_COUNT);
end;

procedure pm4_IncrementDE(var ctx:t_me_render_context;node:p_pm4_node);
begin
 Inc(ctx.me^.DE_COUNT);
end;

procedure pm4_WaitOnCECounter(var ctx:t_me_render_context;node:p_pm4_node);
begin
 if (ctx.me^.CE_COUNT <= ctx.me^.DE_COUNT) then
 begin
  if p_print_gpu_ops then
  begin
   LOG_TRACE('WaitOnCECounter:(',ctx.me^.CE_COUNT,' <= ',ctx.me^.DE_COUNT,')');
  end;
  ctx.switch_task;
 end else
 begin
  if p_print_gpu_ops then
  begin
   LOG_TRACE('WaitOnCECounter:(',ctx.me^.CE_COUNT,' > ',ctx.me^.DE_COUNT,')');
  end;
 end;
end;

procedure pm4_WaitOnDECounterDiff(var ctx:t_me_render_context;node:p_pm4_node_WaitOnDECounterDiff);
var
 diff:DWORD;
begin
 diff:=node^.diff;
 //force unsigned compare
 if (DWORD(ctx.me^.DE_COUNT - ctx.me^.CE_COUNT) >= diff) then
 begin
  if p_print_gpu_ops then
  begin
   LOG_TRACE('WaitOnDECounterDiff:(',ctx.me^.DE_COUNT,' - ',ctx.me^.CE_COUNT,') >= ',diff);
  end;
  ctx.switch_task;
 end else
 begin
  if p_print_gpu_ops then
  begin
   LOG_TRACE('WaitOnDECounterDiff:(',ctx.me^.DE_COUNT,' - ',ctx.me^.CE_COUNT,') < ',diff);
  end;
 end;
end;

procedure pm4_PfpSyncMe(var ctx:t_me_render_context;node:p_pm4_node_PfpSyncMe);
begin
 if not ctx.WaitConfirmOrSwitch then Exit;

 RTLEventSetEvent(node^.event);
end;

procedure pm4_WaitSemaphore(var ctx:t_me_render_context;node:p_pm4_node_Semaphore);
var
 addr:PQWORD;
begin
 addr:=node^.addr;

 if (addr^>0) then
 begin
  //is signaled

  if (node^.behavior=0) then
  begin
   //decrement
   addr^:=addr^-1;
  end;

 end else
 begin
  ctx.switch_task;
 end;

end;

procedure pm4_SignalSemaphore(var ctx:t_me_render_context;node:p_pm4_node_Semaphore);
var
 addr:PQWORD;
begin
 if not ctx.WaitConfirmOrSwitch then Exit;

 addr:=node^.addr;

 if (node^.behavior=0) then
 begin
  //Increment
  addr^:=addr^+1;
 end else
 begin
  //Set 1
  addr^:=1;
 end;

end;

//

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 ctx:t_me_render_context;
 imdone_count:QWORD;
begin
 sched_prio(curkthread,64);

 ctx:=Default(t_me_render_context);
 ctx.Init;
 ctx.me:=me;

 imdone_count:=0;

 if use_renderdoc_capture then
 begin
  if not IsRenderDocPreLoaded then
  begin
   //disable capture if we are not working with Renderdoc GUI
   use_renderdoc_capture:=False;
  end else
  begin
   renderdoc.LoadRenderDoc;
   renderdoc.UnloadCrashHandler;
  end;
 end;

 me^.reset_sheduler;

 repeat

  //test submit done
  if (me^.imdone_count<>imdone_count) then
  begin
   imdone_count:=me^.imdone_count;
   EndFrameCapture;
  end;

  //read from queue
  ctx.stream:=nil;
  if me^.queue.Pop(ctx.stream) then
  begin
   me^.add_stream(ctx.stream);
   //
   ctx.stream:=nil;
  end;

  //get next task
  ctx.stream:=me^.get_next;

  if (ctx.stream<>nil) then
  begin

   //start relative timer
   if (ctx.rel_time=0) then
   begin
    ctx.rel_time:=md_rdtsc_unit;
   end;
   //

   //restore cursor
   ctx.node:=ctx.stream^.curr;
   if (ctx.node=nil) then
   begin
    ctx.node:=ctx.stream^.First;
    ctx.stream^.curr:=ctx.node;
   end;

   while (ctx.node<>nil) do
   begin

    if not ctx.stream^.hint_cmds then
    begin
     if p_print_gpu_ops then
     begin
      LOG_TRACE('+',HexStr(ctx.node^.id,16),':',ctx.node^.ntype);
     end;
     ctx.stream^.hint_cmds:=True;
    end;

    //wait last stall cmd ???
    //if ctx.WaitConfirm then
    begin
     case ctx.node^.ntype of
      ntHint               :pm4_Hint               (ctx,Pointer(ctx.node));
      ntDrawIndex2         :pm4_Draw               (ctx,Pointer(ctx.node));
      ntDrawIndexOffset2   :pm4_Draw               (ctx,Pointer(ctx.node));
      ntDrawIndirect       :pm4_Draw               (ctx,Pointer(ctx.node));
      ntDrawIndexIndirect  :pm4_Draw               (ctx,Pointer(ctx.node));
      ntDrawIndexIndirectCountMulti:pm4_Draw       (ctx,Pointer(ctx.node));
      ntDrawIndexAuto      :pm4_Draw               (ctx,Pointer(ctx.node));
      ntClearDepth         :pm4_Draw               (ctx,Pointer(ctx.node));
      ntResolve            :pm4_Resolve            (ctx,Pointer(ctx.node));
      ntFastClear          :pm4_FastClear          (ctx,Pointer(ctx.node));
      ntDispatchDirect     :pm4_DispatchDirect     (ctx,Pointer(ctx.node));
      ntDispatchIndirect   :pm4_DispatchIndirect   (ctx,Pointer(ctx.node));
      ntEventWrite         :pm4_EventWrite         (ctx,Pointer(ctx.node));
      ntPipeStatDump       :pm4_PipeStatDump       (ctx,Pointer(ctx.node));
      ntEventWriteEop      :pm4_EventWriteEop      (ctx,Pointer(ctx.node));
      ntSubmitFlipEop      :pm4_SubmitFlipEop      (ctx,Pointer(ctx.node));
      ntReleaseMem         :pm4_ReleaseMem         (ctx,Pointer(ctx.node));
      ntEventWriteEos      :pm4_EventWriteEos      (ctx,Pointer(ctx.node));
      ntWriteData          :pm4_WriteData          (ctx,Pointer(ctx.node));
      ntDmaData            :pm4_DmaData            (ctx,Pointer(ctx.node));
      ntWaitRegMem         :pm4_WaitRegMem         (ctx,Pointer(ctx.node));

      ntLoadConstRam       :pm4_LoadConstRam       (ctx,Pointer(ctx.node));
      ntDumpConstRam       :pm4_DumpConstRam       (ctx,Pointer(ctx.node));

      ntIncrementCE        :pm4_IncrementCE        (ctx,Pointer(ctx.node));
      ntIncrementDE        :pm4_IncrementDE        (ctx,Pointer(ctx.node));
      ntWaitOnCECounter    :pm4_WaitOnCECounter    (ctx,Pointer(ctx.node));
      ntWaitOnDECounterDiff:pm4_WaitOnDECounterDiff(ctx,Pointer(ctx.node));

      ntPfpSyncMe          :pm4_PfpSyncMe          (ctx,Pointer(ctx.node));

      ntWaitSemaphore      :pm4_WaitSemaphore      (ctx,Pointer(ctx.node));
      ntSignalSemaphore    :pm4_SignalSemaphore    (ctx,Pointer(ctx.node));

      else
       begin
        LOG_CRITICAL(StdErr,'me:+',ctx.node^.ntype);
        Assert(false,'me:+'+GetEnumName(TypeInfo(t_pm4_node_type),ord(ctx.node^.ntype)));
       end;
     end;
    end;

    if me^.sheduler.switch then
    begin
     //save position
     ctx.stream^.curr:=ctx.node;
     //Switching to another task
     Break;
    end;

    //reset hint
    ctx.stream^.hint_cmds:=False;

    //next command
    ctx.node:=ctx.stream^.Next(ctx.node);
   end;

   if me^.sheduler.switch then
   begin
    //
    me^.sheduler.switch:=False;
    //Switching to another task
    Continue;
   end else
   begin
    //Complete the task and switch to the next one
    ctx.complete_and_next_task;
   end;

   //
   me^.remove_stream(ctx.stream);
   ctx.stream:=nil;

   //
   Continue;
  end;

  //stall is empty!

  me^.reset_sheduler;

  ctx.rel_time:=0; //reset time

  //TODO: Timeline semaphore
  if not ctx.PingCmd then
  begin
   ctx.on_idle;
  end;

  //RTLEventWaitFor(me^.event,100);
  me^.wait;

 until false;

end;

end.

