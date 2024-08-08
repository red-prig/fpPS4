unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 LFQueue,

 si_ci_vi_merged_enum,

 md_sleep,

 Vulkan,
 vDevice,
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

 renderdoc,

 sys_event,
 time,
 md_time,
 kern_thr,
 pm4defs,
 pm4_stream;

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
  event:PRTLEvent;
  on_idle:TProcedure;
  on_submit_flip_eop:t_on_submit_flip_eop;
  //
  started:Pointer;
  td:p_kthread;
  //
  gc_knlist:p_knlist;
  //
  imdone_count:QWORD;
  //
  CONST_RAM:array[0..CONST_RAM_SIZE-1] of Byte; //48KB
  //
  procedure Init(knlist:p_knlist);
  procedure start;
  procedure trigger;
  procedure imdone;
  procedure knote_eventid(event_id,me_id:Byte;timestamp:QWORD;lockflags:Integer);
  procedure Push(var stream:t_pm4_stream);
  procedure reset_sheduler;
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
  procedure Init;
  procedure BeginCmdBuffer;
  procedure FinishCmdBuffer;
  function  CmdStatus(i:t_pm4_stream_type):TVkResult;
  procedure PingCmd;
  function  WaitConfirmOrSwitch:Boolean;
  Procedure InsertLabel(pLabelName:PVkChar);
  Procedure BeginLabel(pLabelName:PVkChar);
  Procedure EndLabel();
  //
  procedure switch_task;
  procedure next_task;
  procedure on_idle;
 end;

var
 use_renderdoc_capture:Boolean=False;

implementation

uses
 kern_dmem,
 kern_proc,
 vm_map,
 vm_tracking_map,
 dev_dce;

procedure StartFrameCapture;
begin
 if use_renderdoc_capture then
 begin
  if (renderdoc.IsFrameCapturing()=0) then
  begin
   renderdoc.StartFrameCapture(0,0);
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
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl; forward;

procedure t_pm4_me.start;
begin
 if (XCHG(started,Pointer(1))=nil) then
 begin
  event:=RTLEventCreate;
  //
  kthread_add(@pm4_me_thread,@self,@td,(8*1024*1024) div (16*1024),'[GFX_ME]');
 end;
end;

procedure t_pm4_me.trigger;
begin
 if (event<>nil) then
 begin
  RTLEventSetEvent(event);
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
  //wait
  msleep_td(hz div 1000);
  //
  sheduler.count:=0;
 end;
 //next
 next_step;
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

 Writeln('QueueSubmit:',r);

 if (r<>VK_SUCCESS) then
 begin
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
   Writeln(stderr,'last.Status=',r); //error
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
    Writeln(stderr,'last.Status=',Result); //error
  end;

  TAILQ_REMOVE(@stall[i],last,@last.entry);

  free_cmd_buffer(last);

  last:=TvStreamCmdBuffer(TAILQ_FIRST(@stall[i]));
 end;

 Result:=VK_SUCCESS;
end;

procedure t_me_render_context.PingCmd;
var
 i:t_pm4_stream_type;
begin
 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  CmdStatus(i);
 end;
end;

function t_me_render_context.WaitConfirmOrSwitch:Boolean;
begin
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

procedure t_me_render_context.next_task;
begin
 if me^.next_task then
 begin
  FinishCmdBuffer;
 end;
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
  else
   Result:=VK_IMAGE_LAYOUT_UNDEFINED;
 end;
end;

function ConvertRW(IMAGE_USAGE:Byte;R,W:TVkAccessFlagBits):TVkAccessFlags; inline;
begin
 Result:=(ord(R)*ord((IMAGE_USAGE and TM_READ               )<>0) ) or
         (ord(W)*ord((IMAGE_USAGE and (TM_WRITE or TM_CLEAR))<>0) );
end;

function GetAccessMask(const curr:t_pm4_usage):TVkAccessFlags;
begin
 Result:=
  ConvertRW(curr.shd_usage,VK_ACCESS_SHADER_READ_BIT                  ,VK_ACCESS_SHADER_WRITE_BIT                  ) or
  ConvertRW(curr.clr_usage,VK_ACCESS_COLOR_ATTACHMENT_READ_BIT        ,VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT        ) or
  ConvertRW(curr.dsa_usage,VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT,VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
end;

procedure Prepare_Uniforms(var ctx:t_me_render_context;
                           var UniformBuilder:TvUniformBuilder);
var
 i:Integer;

 ri:TvImage2;

 resource_instance:p_pm4_resource_instance;
begin
 //Writeln('[Prepare_Uniforms]->');

 if (Length(UniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FImages) do
  With UniformBuilder.FImages[i] do
  begin

   resource_instance:=ctx.node^.scope.find_image_resource_instance(FImage);

   Assert(resource_instance<>nil);

   ri:=TvImage2(resource_instance^.resource^.rimage);

   if (ri=nil) then
   begin
    ri:=FetchImage(ctx.Cmd,
                   FImage,
                   {[iu_sampled]} resource_instance^.curr.img_usage
                  );

    resource_instance^.resource^.rimage:=ri;
   end;

   //Writeln(ri.key.cformat);

   pm4_load_from(ctx.Cmd,ri,TM_READ);

   ri.PushBarrier(ctx.Cmd,
                  GetAccessMask(resource_instance^.curr),
                  GetImageLayout(resource_instance^.curr),
                  ord(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT));

  end;
 end;

 //Writeln('<-[Prepare_Uniforms]');
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
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

 diff :TVkDeviceSize;
 align:TVkDeviceSize;
 range:TVkDeviceSize;

 resource_instance:p_pm4_resource_instance;
begin
 DescriptorGroup:=ctx.Cmd.FetchDescriptorInterface(BindPoint);

 //images
 if (Length(UniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FImages) do
  With UniformBuilder.FImages[i] do
  begin

   resource_instance:=ctx.node^.scope.find_image_resource_instance(FImage);

   Assert(resource_instance<>nil);

   ri:=TvImage2(resource_instance^.resource^.rimage);

   Assert(ri<>nil);

   {
   ri:=FetchImage(ctx.Cmd,
                  FImage,
                  [iu_sampled]
                 );
   }

   iv:=ri.FetchView(ctx.Cmd,FView,iu_sampled);

   DescriptorGroup.BindImage(fset,bind,
                             iv.FHandle,
                             GetImageLayout(resource_instance^.curr));


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
  begin

   resource_instance:=ctx.node^.scope.find_buffer_resource_instance(addr,size);

   {
   if (resource_instance<>nil) then
   begin

    Writeln('rb:curr:',HexStr(resource_instance^.curr.mem_usage,1),
              ' prev:',HexStr(resource_instance^.prev.mem_usage,1),
              ' next:',HexStr(resource_instance^.next.mem_usage,1)
           );

   end;
   }

   buf:=FetchHostBuffer(ctx.Cmd,QWORD(addr),size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   diff:=QWORD(addr)-buf.FAddr;

   align:=diff-AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   if (align<>offset) then
   begin
    Assert(false,'wrong buffer align '+IntToStr(align)+'<>'+IntToStr(offset));
   end;

   diff:=AlignDw(diff,limits.minStorageBufferOffsetAlignment);

   range:=size;

   DescriptorGroup.BindBuffer(fset,bind,
                              buf.FHandle,
                              diff,
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

   Assert(addr<>nil,'push const');

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

   //Writeln('init_img:',HexStr(resource^.rkey.Addr),' ',(resource^.rkey.params.width),'x',(resource^.rkey.params.height));

   ri:=FetchImage(ctx.Cmd,
                  resource^.rkey,
                  i^.curr.img_usage + i^.next.img_usage
                 );

   resource^.rimage:=ri;

   pm4_load_from(ctx.Cmd,ri,i^.curr.mem_usage);
  end else
  if (resource^.rtype=R_HTILE) then
  begin
   ht:=FetchHtile(ctx.Cmd,resource^.rkey,resource^.rsize);

   resource^.rclear:=ht.rclear;
  end;

  i:=TAILQ_NEXT(i,@i^.init_entry);
 end;

 ctx.stream^.init:=True;
end;


procedure pm4_ClearDepth(var rt_info:t_pm4_rt_info;
                         CmdBuffer:TvCmdBuffer);
var
 ri:TvImage2;
 cclear:array[0..1] of Boolean;
 range :TVkImageSubresourceRange;
begin
 //ClearDepthTarget

 CmdBuffer.EndRenderPass;

 CmdBuffer.BeginLabel('ClearDepth');

 ri:=FetchImage(CmdBuffer,
                rt_info.DB_INFO.FImageInfo,
                [iu_depthstenc]
                );

 ri.PushBarrier(CmdBuffer,
                ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

 cclear[0]:=((rt_info.DB_INFO.DEPTH_USAGE   and TM_CLEAR)<>0) and
            (GetDepthOnlyFormat  (ri.key.cformat)<>VK_FORMAT_UNDEFINED);

 cclear[1]:=((rt_info.DB_INFO.STENCIL_USAGE and TM_CLEAR)<>0) and
            (GetStencilOnlyFormat(ri.key.cformat)<>VK_FORMAT_UNDEFINED);

 range:=ri.GetSubresRange;

 range.aspectMask:=(ord(VK_IMAGE_ASPECT_DEPTH_BIT  )*ord(cclear[0])) or
                   (ord(VK_IMAGE_ASPECT_STENCIL_BIT)*ord(cclear[1]));

 CmdBuffer.ClearDepthStencilImage(ri.FHandle,
                                  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                  @rt_info.DB_INFO.CLEAR_VALUE.depthStencil,
                                  range);

 CmdBuffer.EndLabel();
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

 Writeln(stderr,str);
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

 htile_instance:p_pm4_resource_instance;
 d_instance:p_pm4_resource_instance;
 s_instance:p_pm4_resource_instance;
begin
 RP_KEY.Clear;

 if (ctx.rt_info^.RT_COUNT<>0) then
 For i:=0 to ctx.rt_info^.RT_COUNT-1 do
  begin

   color_instance[i]:=ctx.node^.scope.find_image_resource_instance(ctx.rt_info^.RT_INFO[i].FImageInfo);

   Assert(color_instance[i]<>nil);

   //TODO: fixup cformat

   RP_KEY.AddColorAt(ctx.rt_info^.RT_INFO[i].attachment,
                     ctx.rt_info^.RT_INFO[i].FImageInfo.cformat,
                     ctx.rt_info^.RT_INFO[i].IMAGE_USAGE or
                     GetMixedFlag(color_instance[i]^.curr),
                     ctx.rt_info^.RT_INFO[i].FImageInfo.params.samples);

  end;

 if ctx.rt_info^.DB_ENABLE then
 begin

  //set clear flag on cleared htile
  if (ctx.rt_info^.DB_INFO.HTILE_INFO.TILE_SURFACE_ENABLE<>0) then
  begin
   htile_instance:=ctx.node^.scope.find_htile_resource_instance(ctx.rt_info^.DB_INFO.HTILE_INFO.KEY.Addr,
                                                                ctx.rt_info^.DB_INFO.HTILE_INFO.SIZE);
   Assert(htile_instance<>nil);

   if htile_instance^.resource^.rclear then
   begin
    //clear TM_READ
    ctx.rt_info^.DB_INFO.DEPTH_USAGE:=ctx.rt_info^.DB_INFO.DEPTH_USAGE and (not TM_READ);
    //set   TM_CLEAR
    ctx.rt_info^.DB_INFO.DEPTH_USAGE:=ctx.rt_info^.DB_INFO.DEPTH_USAGE or TM_CLEAR;

    htile_instance^.resource^.rclear:=False;
   end;

  end;

  //TODO: fixup cformat

  RP_KEY.AddDepthAt(ctx.rt_info^.RT_COUNT, //add to last attachment id
                    ctx.rt_info^.DB_INFO.FImageInfo.cformat,
                    ctx.rt_info^.DB_INFO.DEPTH_USAGE,
                    ctx.rt_info^.DB_INFO.STENCIL_USAGE,
                    ctx.rt_info^.DB_INFO.FImageInfo.params.samples);

  RP_KEY.SetZorderStage(ctx.rt_info^.DB_INFO.zorder_stage);

 end;

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

 GP_KEY.rasterizer   :=ctx.rt_info^.RASTERIZATION;
 GP_KEY.multisampling:=ctx.rt_info^.MULTISAMPLE;

 GP_KEY.SetProvoking(TVkProvokingVertexModeEXT(ctx.rt_info^.PROVOKING));

 if ctx.rt_info^.DB_ENABLE then
 begin
  GP_KEY.DepthStencil:=ctx.rt_info^.DB_INFO.ds_state;
 end;

 GP:=FetchGraphicsPipeline(ctx.Cmd,@GP_KEY);

 if limits.VK_KHR_imageless_framebuffer then
 begin
  FB_KEY:=Default(TvFramebufferImagelessKey);

  FB_KEY.SetRenderPass(RP);
  FB_KEY.SetSize(ctx.rt_info^.SCREEN_SIZE);

  if (ctx.rt_info^.RT_COUNT<>0) then
  For i:=0 to ctx.rt_info^.RT_COUNT-1 do
   begin
    //TODO: fixup cformat

    FB_KEY.AddImageAt(ctx.rt_info^.RT_INFO[i].FImageInfo);
   end;

  if ctx.rt_info^.DB_ENABLE then
  begin
   //TODO: fixup cformat

   FB_KEY.AddImageAt(ctx.rt_info^.DB_INFO.FImageInfo);
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

   ri:=TvImage2(color_instance[i]^.resource^.rimage);

   if (ri=nil) then
   begin
    ri:=FetchImage(ctx.Cmd,
                   ctx.rt_info^.RT_INFO[i].FImageInfo,
                   {[iu_attachment]}  color_instance[i]^.curr.img_usage
                   );

    color_instance[i]^.resource^.rimage:=ri;
   end;

   pm4_load_from(ctx.Cmd,ri,ctx.rt_info^.RT_INFO[i].IMAGE_USAGE);

   iv:=ri.FetchView(ctx.Cmd,ctx.rt_info^.RT_INFO[i].FImageView,iu_attachment);

   ri.PushBarrier(ctx.Cmd,
                  GetAccessMask(color_instance[i]^.curr),
                  GetImageLayout(color_instance[i]^.curr),
                  ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                  ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

   //

   ctx.Render.AddClearColor(ctx.rt_info^.RT_INFO[i].CLEAR_COLOR);

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
   ri:=FetchImage(ctx.Cmd,
                  ctx.rt_info^.DB_INFO.FImageInfo,
                  [iu_depthstenc]
                  );

   rd:=ri.DepthOnly;
   rs:=ri.StencilOnly;

   if (d_instance<>nil) then
   begin
    d_instance^.resource^.rimage:=rd;
   end;

   if (s_instance<>nil) then
   begin
    s_instance^.resource^.rimage:=rs;
   end;
  end;

  //

  pm4_load_from(ctx.Cmd,rd,ctx.rt_info^.DB_INFO.DEPTH_USAGE);
  pm4_load_from(ctx.Cmd,rs,ctx.rt_info^.DB_INFO.STENCIL_USAGE);

  iv:=ri.FetchView(ctx.Cmd,iu_depthstenc);

  ri.PushBarrier(ctx.Cmd,
                 GetDepthStencilAccessAttachMask(ctx.rt_info^.DB_INFO.DEPTH_USAGE,ctx.rt_info^.DB_INFO.STENCIL_USAGE),
                 GetDepthStencilSendLayout(ctx.rt_info^.DB_INFO.DEPTH_USAGE,ctx.rt_info^.DB_INFO.STENCIL_USAGE),
                 ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                 ctx.rt_info^.DB_INFO.zorder_stage
                 );

  //

  ctx.Render.AddClearColor(ctx.rt_info^.DB_INFO.CLEAR_VALUE);

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

 Prepare_Uniforms(ctx,FUniformBuilder);
 ////////

 if not ctx.Cmd.BeginRenderPass(@ctx.Render,GP) then
 begin
  Writeln(stderr,'BeginRenderPass(ctx.Render)');

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
 i:Integer;

 ri:TvImage2;
 rd:TvCustomImage2;
 rs:TvCustomImage2;

 resource_instance:p_pm4_resource_instance;

 d_instance:p_pm4_resource_instance;
 s_instance:p_pm4_resource_instance;
begin
 //write back

 if (ctx.rt_info^.RT_COUNT<>0) then
 For i:=0 to ctx.rt_info^.RT_COUNT-1 do
  if (ctx.rt_info^.RT_INFO[i].attachment<>VK_ATTACHMENT_UNUSED) then
  begin

   resource_instance:=ctx.node^.scope.find_image_resource_instance(ctx.rt_info^.RT_INFO[i].FImageInfo);

   Assert(resource_instance<>nil);

   ri:=TvImage2(resource_instance^.resource^.rimage);

   Assert(ri<>nil);

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

  end;

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

 //write back
end;

procedure pm4_Writeback_Finish(var ctx:t_me_render_context);
var
 ri:TvImage2;
 ht:TvMetaHtile;

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

 //
 pm4_InitStream(ctx);
 //

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
    ctx.Cmd.DrawIndex2(Pointer(node^.indexBase),node^.indexCount);
   end;
  ntDrawIndexAuto:
   begin
    ctx.Cmd.DrawIndexAuto(node^.indexCount);
   end;
  ntClearDepth:
   begin
    pm4_ClearDepth(node^.rt_info,ctx.Cmd);
   end;
  else;
   Assert(false,'pm4_Draw');
 end;

 /////////

 pm4_Writeback_After(ctx);

end;

procedure pm4_FastClear(var ctx:t_me_render_context;node:p_pm4_node_FastClear);
{
var
 ri:TvImage2;
 range:TVkImageSubresourceRange;

 resource_instance:p_pm4_resource_instance;
}
begin
{
 //
 pm4_InitStream(ctx);
 //

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 ctx.Cmd.EndRenderPass;

 ri:=FetchImage(ctx.Cmd,
                node^.RT.FImageInfo,
                [iu_attachment]
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

 resource_instance:=ctx.node^.scope.find_image_resource_instance(node^.RT.FImageInfo);
 Assert(resource_instance<>nil);

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

procedure Prepare_htile(var ctx:t_me_render_context;
                        var UniformBuilder:TvUniformBuilder);
var
 i:Integer;

 resource_instance:p_pm4_resource_instance;
 resource:p_pm4_resource;

 //ht:TvMetaHtile;
 hb:TvMetaBuffer;
begin
 resource:=nil;

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  begin

   if ((memuse and TM_WRITE)<>0) then
   begin
    resource_instance:=ctx.node^.scope.find_buffer_resource_instance(addr,size);
    if (resource_instance<>nil) then
    begin
     resource:=resource_instance^.resource;
     Break;
    end;
   end;

  end;
 end;
 //buffers

 Assert(resource<>nil);

 //set flag by buffer in current stream
 resource^.rclear:=True;

 //set flag by buffer to next stream
 hb:=FetchBuffer(ctx.Cmd,resource^.rkey.Addr,resource^.rsize);
 hb.rclear:=True;

 //set flag by htile in current stream
 resource:=ctx.stream^.find_htile_resource(resource^.rkey.Addr,resource^.rsize);
 //
 if (resource<>nil) then
 begin
  resource^.rclear:=True;
 end;

end;

procedure pm4_DispatchPrepare(var ctx:t_me_render_context;node:p_pm4_node_DispatchDirect);
var
 dst:PGPU_USERDATA;

 CP_KEY:TvComputePipelineKey;
 CP:TvComputePipeline2;

 FUniformBuilder:TvUniformBuilder;

begin
 CP_KEY.FShaderGroup:=node^.ShaderGroup;
 CP:=FetchComputePipeline(ctx.Cmd,@CP_KEY);

 ////////

 //hack
 dst:=Pointer(@node^.USER_DATA_CS)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 FUniformBuilder:=Default(TvUniformBuilder);
 CP_KEY.FShaderGroup.ExportUnifBuilder(FUniformBuilder,dst);

 //htile heuristic
 if (CP_KEY.FShaderGroup.FKey.FShaders[vShaderStageCs].IsCSClearShader) then
 begin
  Prepare_htile(ctx,FUniformBuilder);
  //
  ctx.InsertLabel('clear htile/rendertarget');
 end;

 Prepare_Uniforms(ctx,FUniformBuilder);
 ////////

 if not ctx.Cmd.BindCompute(CP) then
 begin
  Writeln(stderr,'BindCompute(CP)');

  DumpShaderGroup(ctx.rt_info^.ShaderGroup);

  Assert(false  ,'BindCompute(CP)');
 end;

 Bind_Uniforms(ctx,
               BP_COMPUTE,
               FUniformBuilder);

 Bind_Pushs(ctx,CP_KEY.FShaderGroup,dst);

end;

procedure pm4_DispatchDirect(var ctx:t_me_render_context;node:p_pm4_node_DispatchDirect);
begin
 //
 pm4_InitStream(ctx);
 //

 StartFrameCapture;

 ctx.BeginCmdBuffer;

 //
 ctx.Cmd.EndRenderPass;

 pm4_DispatchPrepare(ctx,node);

 ctx.Cmd.DispatchDirect(node^.DIM_X,node^.DIM_Y,node^.DIM_Z);

 /////////
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
  ctx.stream^.hint_repeat:=True;
 end;

 if not ctx.WaitConfirmOrSwitch then Exit;

 ctx.stream^.hint_repeat:=False;

 curr:=md_rdtsc_unit;
 diff:=curr-ctx.rel_time;

 if (node^.addr<>nil) then
 begin
  if (node^.dataSel<>EVENTWRITEEOP_DATA_SEL_DISCARD) then
  begin
   if not get_dmem_ptr(node^.addr,@addr_dmem,nil) then
   begin
    Assert(false,'addr:0x'+HexStr(node^.addr)+' not in dmem!');
   end;
  end;

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

  vm_map_track_trigger(p_proc.p_vmspace,QWORD(node^.addr),QWORD(node^.addr)+data_size,nil,M_DMEM_WRITE);
 end;

 if (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT) or
    (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM) then
 begin
  ctx.me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;

end;

procedure pm4_SubmitFlipEop(var ctx:t_me_render_context;node:p_pm4_node_SubmitFlipEop);
var
 curr:QWORD;
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

 curr:=md_rdtsc_unit;

 if (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT) or
    (node^.intSel=EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM) then
 begin
  ctx.me^.knote_eventid($40,0,curr*NSEC_PER_UNIT,0); //(absolute time) (freq???)
 end;

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
  if (node^.srcSel<>RELEASEMEM_DATA_SEL_DISCARD) then
  begin
   if not get_dmem_ptr(node^.addr,@addr_dmem,nil) then
   begin
    Assert(false,'addr:0x'+HexStr(node^.addr)+' not in dmem!');
   end;
  end;

  Case node^.dstSel of
   RELEASEMEM_DST_SEL_MEMORY:;
   RELEASEMEM_DST_SEL_L2    :Assert(false,'RELEASEMEM_DST_SEL_L2');
   else
    Assert(false,'pm4_ReleaseMem:dstSel');
  end;

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
  //CACHE_FLUSH_AND_INV_EVENT  :Writeln(' eventType=FLUSH_AND_INV_EVENT');
  FLUSH_AND_INV_CB_PIXEL_DATA,
  //FLUSH_AND_INV_DB_DATA_TS   :Writeln(' eventType=FLUSH_AND_INV_DB_DATA_TS');
  FLUSH_AND_INV_DB_META, //HTILE
  FLUSH_AND_INV_CB_META: //CMASK
   begin
    if (ctx.Cmd<>nil) and ctx.Cmd.IsAllocated then
    begin
     //GPU
     ctx.Cmd.WriteEvent(node^.eventType);
    end;
   end;
  //FLUSH_AND_INV_CB_DATA_TS   :Writeln(' eventType=FLUSH_AND_INV_CB_DATA_TS');
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

  else
   begin
    Writeln(stderr,'EventWrite eventType=0x',HexStr(node^.eventType,2));
    Assert (false ,'EventWrite eventType=0x'+HexStr(node^.eventType,2));
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

     addr_dmem:=nil;
     if not get_dmem_ptr(Pointer(node^.addr),@addr_dmem,nil) then
     begin
      Assert(false,'addr:0x'+HexStr(Pointer(node^.addr))+' not in dmem!');
     end;

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

      ctx.Cmd.dmaData1(node^.src,node^.dst,byteSize,node^.wrConfirm);
     end else
     begin
      //soft

      if not get_dmem_ptr(node^.src,@src_dmem,nil) then
      begin
       Assert(false,'addr:0x'+HexStr(node^.src)+' not in dmem!');
      end;

      if not get_dmem_ptr(node^.dst,@dst_dmem,nil) then
      begin
       Assert(false,'addr:0x'+HexStr(node^.dst)+' not in dmem!');
      end;

      byteSize:=node^.num_dw*SizeOf(DWORD);

      Move(src_dmem^,dst_dmem^,byteSize);

      vm_map_track_trigger(p_proc.p_vmspace,QWORD(node^.dst),QWORD(node^.dst)+byteSize,nil,M_DMEM_WRITE);
     end;
    end;
  else
    Assert(false,'WriteData: dstSel=0x'+HexStr(node^.dstSel,1));
 end;

end;

procedure pm4_DmaData(var ctx:t_me_render_context;node:p_pm4_node_DmaData);
var
 adrSrc:QWORD;
 adrDst:QWORD;
 adrSrc_dmem:QWORD;
 adrDst_dmem:QWORD;
 byteCount:DWORD;
 srcSel,dstSel:Byte;
begin

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

      if not get_dmem_ptr(Pointer(adrDst),@adrDst_dmem,nil) then
      begin
       Assert(false,'addr:0x'+HexStr(Pointer(adrDst))+' not in dmem!');
      end;

      if not get_dmem_ptr(Pointer(adrSrc),@adrSrc_dmem,nil) then
      begin
       Assert(false,'addr:0x'+HexStr(Pointer(adrSrc))+' not in dmem!');
      end;

      Move(Pointer(adrSrc_dmem)^,Pointer(adrDst_dmem)^,byteCount);

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

      if not get_dmem_ptr(Pointer(adrDst),@adrDst_dmem,nil) then
      begin
       Assert(false,'addr:0x'+HexStr(Pointer(adrDst))+' not in dmem!');
      end;

      FillDWORD(Pointer(adrDst_dmem)^,(byteCount div 4),DWORD(adrSrc));

      vm_map_track_trigger(p_proc.p_vmspace,QWORD(adrDst),QWORD(adrDst)+byteCount,nil,M_DMEM_WRITE);

      //soft
     end;

    end;
 else
    Assert(false,'DmaData: srcSel=0x'+HexStr(srcSel,1)+' dstSel=0x'+HexStr(dstSel,1));
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

Function me_test_mem(node:p_pm4_node_WaitRegMem):Boolean;
var
 addr_dmem:Pointer;
 val,ref:DWORD;
begin
 if not get_dmem_ptr(node^.pollAddr,@addr_dmem,nil) then
 begin
  Assert(false,'addr:0x'+HexStr(node^.pollAddr)+' not in dmem!');
 end;

 //Writeln('me_test_mem:',get_dce_label_id(addr_dmem),' ',node^.refValue);

 val:=PDWORD(addr_dmem)^ and node^.mask;
 ref:=node^.refValue;
 Case node^.compareFunc of
  WAIT_REG_MEM_FUNC_ALWAYS       :Result:=True;
  WAIT_REG_MEM_FUNC_LESS         :Result:=(val<ref);
  WAIT_REG_MEM_FUNC_LESS_EQUAL   :Result:=(val<=ref);
  WAIT_REG_MEM_FUNC_EQUAL        :Result:=(val=ref);
  WAIT_REG_MEM_FUNC_NOT_EQUAL    :Result:=(val<>ref);
  WAIT_REG_MEM_FUNC_GREATER_EQUAL:Result:=(val>ref);
  WAIT_REG_MEM_FUNC_GREATER      :Result:=(val>=ref);
  else
   Assert(false,'me_test_mem');
 end;
end;

procedure pm4_WaitRegMem(var ctx:t_me_render_context;node:p_pm4_node_WaitRegMem);
begin
 if not ctx.stream^.hint_repeat then
 begin
  ctx.InsertLabel(PChar('WaitRegMem:0x'+HexStr(QWORD(node^.pollAddr),10)));
  ctx.stream^.hint_repeat:=True;
 end;

 if not ctx.WaitConfirmOrSwitch then Exit;

 ctx.stream^.hint_repeat:=False;

 if me_test_mem(node) then
 begin
  ctx.stream^.hint_loop:=0;
 end else
 begin
  Inc(ctx.stream^.hint_loop);
  //
  if (ctx.stream^.hint_loop>10000) then
  begin
   //loop detection
   Writeln(stderr,'WaitReg loop detected -> skip');
   Exit;
  end;
  //
  ctx.switch_task;
 end;

end;

//

procedure pm4_LoadConstRam(var ctx:t_me_render_context;node:p_pm4_node_LoadConstRam);
var
 addr_dmem:Pointer;

 start:DWORD;
 __end:DWORD;
 size :DWORD;
begin
 if not get_dmem_ptr(node^.addr,@addr_dmem,nil) then
 begin
  Assert(false,'addr:0x'+HexStr(node^.addr)+' not in dmem!');
 end;

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

 Move(addr_dmem^,ctx.me^.CONST_RAM[start],size);
end;

//

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 ctx:t_me_render_context;
 imdone_count:QWORD;
begin
 ctx:=Default(t_me_render_context);
 ctx.Init;
 ctx.me:=me;

 imdone_count:=QWORD(-1);

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

  if (me^.imdone_count<>imdone_count) then
  begin
   imdone_count:=me^.imdone_count;
   EndFrameCapture;
  end;

  ctx.stream:=nil;
  if me^.queue.Pop(ctx.stream) then
  begin
   me^.add_stream(ctx.stream);
   //
   ctx.stream:=nil;
  end;

  ctx.stream:=me^.get_next;

  if (ctx.stream<>nil) then
  begin

   //start relative timer
   if (ctx.rel_time=0) then
   begin
    ctx.rel_time:=md_rdtsc_unit;
   end;
   //

   ctx.node:=ctx.stream^.curr;
   if (ctx.node=nil) then
   begin
    ctx.node:=ctx.stream^.First;
    ctx.stream^.curr:=ctx.node;
   end;

   while (ctx.node<>nil) do
   begin
    //Writeln('+',ctx.node^.ntype);

    case ctx.node^.ntype of
     ntHint          :pm4_Hint          (ctx,Pointer(ctx.node));
     ntDrawIndex2    :pm4_Draw          (ctx,Pointer(ctx.node));
     ntDrawIndexAuto :pm4_Draw          (ctx,Pointer(ctx.node));
     ntClearDepth    :pm4_Draw          (ctx,Pointer(ctx.node));
     ntFastClear     :pm4_FastClear     (ctx,Pointer(ctx.node));
     ntDispatchDirect:pm4_DispatchDirect(ctx,Pointer(ctx.node));
     ntEventWrite    :pm4_EventWrite    (ctx,Pointer(ctx.node));
     ntEventWriteEop :pm4_EventWriteEop (ctx,Pointer(ctx.node));
     ntSubmitFlipEop :pm4_SubmitFlipEop (ctx,Pointer(ctx.node));
     ntReleaseMem    :pm4_ReleaseMem    (ctx,Pointer(ctx.node));
     ntEventWriteEos :pm4_EventWriteEos (ctx,Pointer(ctx.node));
     ntWriteData     :pm4_WriteData     (ctx,Pointer(ctx.node));
     ntDmaData       :pm4_DmaData       (ctx,Pointer(ctx.node));
     ntWaitRegMem    :pm4_WaitRegMem    (ctx,Pointer(ctx.node));

     ntLoadConstRam  :pm4_LoadConstRam  (ctx,Pointer(ctx.node));

     else
      begin
       Writeln(stderr,'me:+',ctx.node^.ntype);
       Assert(false,'me:+');
      end;
    end;

    if me^.sheduler.switch then
    begin
     //save position
     ctx.stream^.curr:=ctx.node;
     //
     Break;
    end;

    //
    ctx.node:=ctx.stream^.Next(ctx.node);
   end;

   if me^.sheduler.switch then
   begin
    me^.sheduler.switch:=False;
    //
    Continue;
   end else
   begin
    ctx.next_task;
   end;

   me^.remove_stream(ctx.stream);
   ctx.stream:=nil;

   //
   Continue;
  end;

  ctx.PingCmd;

  //stall is empty!

  me^.reset_sheduler;

  ctx.rel_time:=0; //reset time

  //
  ctx.on_idle;
  //

  RTLEventWaitFor(me^.event);
 until false;

end;

end.

