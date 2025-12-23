unit pm4_stream;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 LFQueue,
 md_map,
 bittype,
 pm4defs,
 pm4_context,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups,

 Vulkan,

 vImage,

 vShader,
 vShaderExt,
 vShaderManager,

 vRegs2Vulkan,

 vImageTiling,
 g_node_splay

 ;

type
 t_cache_block_allocator=object
  const
   mem_size =64*1024;
   max_count=256;
  //
  var
   queue:TIntrusiveMPSCQueue;
   xlock:Pointer;
   count:QWORD;
  //
  procedure Init;
  Function  Alloc:Pointer;
  Procedure Free(node:Pointer);
 end;

 t_pm4_allocator=object
  type
   PAllocNode=^TAllocNode;
   TAllocNode=packed record
    link:PAllocNode;
    size:QWORD;
    data:record end;
   end;
  var
   pHead:SLIST_HEAD;
   curr_apos:ptruint; //alloc pos in current node
   curr_size:ptruint; //useable size of current node
   used_size:ptruint; //full usable size
   full_size:ptruint; //full alloc size
  Function  Alloc(Size:ptruint):Pointer;
  Procedure Free;
 end;

 p_pm4_rt_info=^t_pm4_rt_info;
 t_pm4_rt_info=packed object
  USERDATA  :TGPU_USERDATA;
  SHADERDATA:TGPU_SHADERDATA_RT;

  ShaderGroup:TvShaderGroup;

  RT_INFO:array[0..7] of TRT_INFO;
  DB_INFO:TDB_INFO;

  BLEND_INFO:TBLEND_INFO;

  VPORT  :array[0..15] of TVkViewport;
  SCISSOR:array[0..15] of TVkRect2D;

  RASTERIZATION:TRASTERIZATION_INFO;
  MULTISAMPLE  :TVkPipelineMultisampleStateCreateInfo;

  SCREEN_RECT:TVkRect2D;
  SCREEN_SIZE:TVkExtent2D;

  RT_COUNT  :Byte;
  DB_ENABLE :Boolean;
  PRIM_TYPE :Byte;
  PRIM_RESET:Byte;
  VP_COUNT  :Byte;
  PROVOKING :Byte;

  ShaderDrawParams:t_shader_draw_params;
 end;

 t_pm4_stream_type=(
  stGfxRing,
  stGfxDcb,
  stGfxCcb,
  stCompute0,
  stCompute1,
  stCompute2,
  stCompute3,
  stCompute4,
  stCompute5,
  stCompute6
 );

 t_pm4_node_type=(
  ntHint,
  ntLoadConstRam,
  ntDumpConstRam,
  ntIncrementCE,
  ntIncrementDE,
  ntWaitOnCECounter,
  ntWaitOnDECounterDiff,
  ntEventWrite,
  ntPipeStatDump,
  ntEventWriteEop,
  ntEventWriteEos,
  ntSubmitFlipEop,
  ntReleaseMem,
  ntDmaData,
  ntWriteData,
  ntWaitRegMem,
  ntFastClear,
  ntResolve,
  ntClearDepth,
  ntDrawIndex2,
  ntDrawIndexOffset2,
  ntDrawIndexIndirect,
  ntDrawIndexIndirectCountMulti,
  ntDrawIndexAuto,
  ntDispatchDirect,
  ntDispatchIndirect,
  ntPfpSyncMe
 );

const
 R_IMG  =0;
 R_BUF  =1;
 R_HTILE=2;
 R_CMASK=3;

type
 t_pm4_usage=packed record
  case Byte of
   0:(DATA:QWORD);
   1:(mem_usage:Byte;
      shd_usage:Byte;
      clr_usage:Byte;
      dsa_usage:Byte;
      img_usage:s_image_usage
     );
 end;
 {$IF sizeof(s_image_usage)<>4}{$STOP sizeof(s_image_usage)<>4}{$ENDIF}

operator + (a,b:t_pm4_usage):t_pm4_usage;

type
 p_pm4_resource_instance    =^t_pm4_resource_instance;
 p_pm4_resource_curr_scope  =^t_pm4_resource_curr_scope;
 p_pm4_resource_stream_scope=^t_pm4_resource_stream_scope;

 p_pm4_resource_init_scope=^t_pm4_resource_init_scope;
 t_pm4_resource_init_scope=object
  list:TAILQ_HEAD; //p_pm4_resource_instance
  function  first:p_pm4_resource_instance;
  procedure insert(i:p_pm4_resource_instance);
 end;

 p_pm4_resource=^t_pm4_resource;
 t_pm4_resource=object
  pLeft :p_pm4_resource; //t_pm4_resource_set
  pRight:p_pm4_resource; //t_pm4_resource_set
  //
  rwrite:p_pm4_resource_instance;
  //
  rtype :Integer;
  rsize :DWORD;
  rkey  :TvImageKey;
  //
  uall:t_pm4_usage;
  //
  rimage:TObject;
  //
  rcombined :Boolean;
  rclear    :Boolean;
  rcmask    :Boolean;
  rwriteback:Boolean;
  //
  function c(n1,n2:p_pm4_resource):Integer; static;
 end;

 t_pm4_resource_set=specialize TNodeSplay<t_pm4_resource>;

 t_pm4_resource_instance=object
  init_entry:TAILQ_ENTRY; //p_pm4_resource_init_scope
  //
  pLeft :p_pm4_resource_instance; //t_pm4_resource_instance_set
  pRight:p_pm4_resource_instance; //t_pm4_resource_instance_set
  //
  init_scope:p_pm4_resource_init_scope;
  curr_scope:p_pm4_resource_curr_scope;
  //
  resource:p_pm4_resource;
  //
  prepared:Boolean;
  //
  curr:t_pm4_usage;
  prev:t_pm4_usage;
  next:t_pm4_usage;
  //
  prev_overlap:t_pm4_usage;
  next_overlap:t_pm4_usage;
  //
  function c(n1,n2:p_pm4_resource_instance):Integer; static;
 end;

 t_pm4_resource_instance_set=specialize TNodeSplay<t_pm4_resource_instance>;

 t_pm4_resource_curr_scope=object
  resource_instance_set:t_pm4_resource_instance_set;
  function  Min:p_pm4_resource_instance;
  function  Next(node:p_pm4_resource_instance):p_pm4_resource_instance;
  procedure insert(i:p_pm4_resource_instance);
  function  find_resource_instance(r:p_pm4_resource):p_pm4_resource_instance;
  function  find_image_resource_instance (const rkey:TvImageKey):p_pm4_resource_instance;
  function  find_buffer_resource_instance(rtype:Integer;addr:Pointer;size:DWORD):p_pm4_resource_instance;
 end;

 t_pm4_resource_stream_scope=object
  next_:TAILQ_HEAD; //Must be the first element in memory
  //
  allocator:t_pm4_allocator;
  //
  resource_set:t_pm4_resource_set;
  init_scope:t_pm4_resource_init_scope;
  //
  function  find_image_resource          (const rkey:TvImageKey):p_pm4_resource;
  function  fetch_image_resource         (const rkey:TvImageKey;hint:PChar):p_pm4_resource;
  function  find_buffer_resource         (rtype:Integer;addr:Pointer;size:DWORD):p_pm4_resource;
  function  fetch_buffer_resource        (rtype:Integer;addr:Pointer;size:DWORD;hint:PChar):p_pm4_resource;
  function  fetch_resource_instance      (scope:p_pm4_resource_curr_scope;r:p_pm4_resource;mem_usage:Integer;img_usage:s_image_usage):p_pm4_resource_instance;
  function  insert_image_resource        (scope:p_pm4_resource_curr_scope;const rkey:TvImageKey;mem_usage:Integer;img_usage:s_image_usage;hint:PChar):p_pm4_resource_instance;
  function  insert_buffer_resource       (scope:p_pm4_resource_curr_scope;rtype:Integer;addr:Pointer;size:DWORD;mem_usage:Integer;hint:PChar):p_pm4_resource_instance;
  procedure connect_resource_instance    (i:p_pm4_resource_instance);
  procedure connect_resource_scope       (scope:p_pm4_resource_curr_scope);
 end;

 p_pm4_node=^t_pm4_node;
 t_pm4_node=object
  entry:TAILQ_ENTRY;
  //
  scope:t_pm4_resource_curr_scope;
  //
  ntype:t_pm4_node_type;
  //
  id:QWORD;
 end;

 p_pm4_node_Hint=^t_pm4_node_Hint;
 t_pm4_node_Hint=packed object(t_pm4_node)
  data:record end; //@pchar
 end;

 p_pm4_node_LoadConstRam=^t_pm4_node_LoadConstRam;
 t_pm4_node_LoadConstRam=packed object(t_pm4_node)
  addr  :Pointer;
  num_dw:Word;
  offset:Word;
 end;

 p_pm4_node_WaitOnDECounterDiff=^t_pm4_node_WaitOnDECounterDiff;
 t_pm4_node_WaitOnDECounterDiff=packed object(t_pm4_node)
  diff:DWORD;
 end;

 p_pm4_node_EventWrite=^t_pm4_node_EventWrite;
 t_pm4_node_EventWrite=packed object(t_pm4_node)
  eventType:Byte;
 end;

 p_pm4_node_PipeStatDump=^t_pm4_node_PipeStatDump;
 t_pm4_node_PipeStatDump=packed object(t_pm4_node)
  Address:QWORD;
  Control:TPixelPipeStatControl;
 end;

 p_pm4_node_EventWriteEop=^t_pm4_node_EventWriteEop;
 t_pm4_node_EventWriteEop=packed object(t_pm4_node)
  addr     :Pointer;
  data     :QWORD;
  eventType:Byte;
  dataSel  :Byte;
  intSel   :Byte;
 end;

 p_pm4_node_EventWriteEos=^t_pm4_node_EventWriteEos;
 t_pm4_node_EventWriteEos=packed object(t_pm4_node)
  addr     :Pointer;
  data     :DWORD;
  eventType:Byte;
  command  :Byte;
 end;

 p_pm4_node_SubmitFlipEop=^t_pm4_node_SubmitFlipEop;
 t_pm4_node_SubmitFlipEop=packed object(t_pm4_node)
  eop_value:QWORD;
  intSel   :Byte
 end;

 p_pm4_node_ReleaseMem=^t_pm4_node_ReleaseMem;
 t_pm4_node_ReleaseMem=packed object(t_pm4_node)
  addr     :Pointer;
  data     :QWORD;
  eventType:Byte;
  srcSel   :Byte;
  dstSel   :Byte;
  intSel   :Byte;
 end;

 p_pm4_node_DmaData=^t_pm4_node_DmaData;
 t_pm4_node_DmaData=packed object(t_pm4_node)
  dst     :QWORD;
  src     :QWORD;
  numBytes:DWORD;
  srcSel  :Byte;
  dstSel  :Byte;
  cpSync  :Byte;
 end;

 p_pm4_node_WriteData=^t_pm4_node_WriteData;
 t_pm4_node_WriteData=packed object(t_pm4_node)
  dst      :Pointer;
  src      :Pointer;
  num_dw   :Word;
  dstSel   :Byte;
  wrConfirm:Boolean;
 end;

 p_pm4_node_WaitRegMem=^t_pm4_node_WaitRegMem;
 t_pm4_node_WaitRegMem=packed object(t_pm4_node)
  pollAddr    :Pointer;
  refValue    :DWORD;
  mask        :DWORD;
  compareFunc :Byte;
 end;

 p_pm4_node_FastClear=^t_pm4_node_FastClear;
 t_pm4_node_FastClear=object(t_pm4_node)
  RT:TRT_INFO;
 end;

 p_pm4_node_Resolve=^t_pm4_node_Resolve;
 t_pm4_node_Resolve=object(t_pm4_node)
  RT:array[0..1] of TRT_INFO;
  SCREEN:TVkRect2D;
 end;

 p_pm4_node_draw=^t_pm4_node_draw;
 t_pm4_node_draw=object(t_pm4_node)
  rt_info:t_pm4_rt_info;

  u:record
   case Byte of
    0:(indexBase   :QWORD;
       indexOffset :DWORD;
       vertexOffset:DWORD;
       indexCount  :DWORD;
       numInstances:DWORD;
       INDEX_TYPE:Byte;
       SWAP_MODE :Byte);
    1:(
       indirectBase:QWORD;
       countAddr   :QWORD;
       dataOffset  :DWORD;
       stride      :DWORD;
       count       :DWORD;
      );
  end;

 end;

 p_pm4_node_Dispatch=^t_pm4_node_Dispatch;
 t_pm4_node_Dispatch=object(t_pm4_node)

  COMPUTE_GROUP:TSH_REG_COMPUTE_GROUP;

  ShaderGroup:TvShaderGroup;

 end;

 p_pm4_node_DispatchDirect=^t_pm4_node_DispatchDirect;
 t_pm4_node_DispatchDirect=object(t_pm4_node_Dispatch)

  DIM_X:DWORD;
  DIM_Y:DWORD;
  DIM_Z:DWORD;

 end;

 p_pm4_node_DispatchIndirect=^t_pm4_node_DispatchIndirect;
 t_pm4_node_DispatchIndirect=object(t_pm4_node_Dispatch)

  BASE  :QWORD;
  Offset:DWORD;

 end;

 p_pm4_node_PfpSyncMe=^t_pm4_node_PfpSyncMe;
 t_pm4_node_PfpSyncMe=object(t_pm4_node)
  event:PRTLEvent;
 end;

 p_pm4_stream=^t_pm4_stream;
 t_pm4_stream=object(t_pm4_resource_stream_scope)
  //
  list:TAILQ_HEAD; //t_pm4_node
  //
  buft:t_pm4_stream_type;
  //
  init:Boolean;
  hint_repeat:Boolean;
  hint_loop:Ptruint;
  hint_cmds:Boolean;
  //
  curr:p_pm4_node;
  //
  refs:Ptruint;
  //
  procedure Free;
  Procedure add_node(node:p_pm4_node);
  function  First:p_pm4_node;
  function  Next(node:p_pm4_node):p_pm4_node; static;
  //
  procedure Acquire;
  function  Release:Boolean;
  //
  procedure Hint         (P1,P2:PChar;maxsize:Integer);
  procedure LoadConstRam (addr:Pointer;num_dw,offset:Word);
  procedure DumpConstRam (addr:Pointer;num_dw,offset:Word);
  procedure IncrementCE  ();
  procedure IncrementDE  ();
  procedure WaitOnCECounter();
  procedure WaitOnDECounterDiff(diff:DWORD);
  procedure EventWrite   (eventType:Byte);
  procedure PipeStatDump (Address:QWORD;Control:TPixelPipeStatControl);
  procedure EventWriteEop(addr:Pointer;data:QWORD;eventType,dataSel,intSel:Byte);
  procedure EventWriteEos(addr:Pointer;data:DWORD;eventType,command:Byte);
  procedure SubmitFlipEop(eop_value:QWORD;intSel:Byte);
  procedure ReleaseMem   (addr:Pointer;data:QWORD;eventType,srcSel,dstSel,intSel:Byte);
  procedure DmaData      (dstSel:Byte;dst:QWORD;srcSel:Byte;srcOrData:QWORD;numBytes:DWORD;isBlocking:Byte);
  procedure WriteData    (dstSel:Byte;dst,src:Pointer;num_dw:Word;wrConfirm:Byte);
  procedure WaitRegMem   (pollAddr:Pointer;refValue,mask:DWORD;compareFunc:Byte);
  procedure FastClear    (var CX_REG:TCONTEXT_REG_GROUP);
  procedure Resolve      (var CX_REG:TCONTEXT_REG_GROUP);
  function  ColorControl (var CX_REG:TCONTEXT_REG_GROUP):Boolean;
  procedure Init_Uniforms(node:p_pm4_node;var UniformBuilder:TvUniformBuilder);
  procedure Init_Pushs   (node:p_pm4_node;
                          ShaderGroup:TvShaderGroup;
                          var GPU_REGS:TGPU_REGS);
  procedure Build_rt_info(node:p_pm4_node;
                          var rt_info:t_pm4_rt_info;
                          var GPU_REGS:TGPU_REGS;
                          ShaderDrawParams:t_shader_draw_params);
  function  BuildDraw        (ntype:t_pm4_node_type;context:p_amd_context):p_pm4_node_draw;
  procedure DrawIndex2       (context:p_amd_context);
  procedure DrawIndexOffset2 (context:p_amd_context;
                              indexOffset:DWORD);
  procedure DrawIndexIndirect(context   :p_amd_context;
                              dataOffset:DWORD);
  procedure DrawIndexIndirectCountMulti(context   :p_amd_context;
                                        dataOffset:DWORD;
                                        stride    :DWORD;
                                        count     :DWORD;
                                        countAddr :QWORD);
  procedure DrawIndexAuto   (context:p_amd_context);
  procedure Build_cs_info   (node:p_pm4_node_Dispatch;var GPU_REGS:TGPU_REGS);
  procedure DispatchDirect  (var SC_REG:TSH_REG_COMPUTE_GROUP);
  procedure DispatchIndirect(var SC_REG:TSH_REG_COMPUTE_GROUP;
                             BASE  :QWORD;
                             Offset:DWORD);
  procedure PfpSyncMe(event:PRTLEvent);
 end;

implementation

uses
 sys_bootparam;

var
 cache_block_allocator:t_cache_block_allocator;

//

operator + (a,b:t_pm4_usage):t_pm4_usage; inline;
begin
 //hack
 Result.DATA:=a.DATA or b.DATA;
end;

//

function t_pm4_resource.c(n1,n2:p_pm4_resource):Integer;
begin
 //0 Addr
 Result:=Integer(n1^.rkey.Addr>n2^.rkey.Addr)-Integer(n1^.rkey.Addr<n2^.rkey.Addr);
 if (Result<>0) then Exit;

 //1 rtype
 Result:=Integer(n1^.rtype>n2^.rtype)-Integer(n1^.rtype<n2^.rtype);
 if (Result<>0) then Exit;

 case n1^.rtype of
  R_IMG:
   begin
    Result:=CompareNormalized(n1^.rkey,n2^.rkey);
   end;
  R_BUF,
  R_HTILE,
  R_CMASK:
   begin
    //2 rsize
    Result:=Integer(n1^.rsize>n2^.rsize)-Integer(n1^.rsize<n2^.rsize);
   end;
  else;
 end;

end;

function t_pm4_resource_instance.c(n1,n2:p_pm4_resource_instance):Integer;
begin
 Result:=t_pm4_resource.c(n1^.resource,n2^.resource);
end;

//


function  t_pm4_resource_init_scope.first:p_pm4_resource_instance;
begin
 Result:=TAILQ_FIRST(@list);
end;


procedure t_pm4_resource_init_scope.insert(i:p_pm4_resource_instance);
begin
 if (i^.init_entry.tqe_next<>nil) or
    (i^.init_entry.tqe_prev<>nil) then Exit;

 if (list.tqh_last=nil) then
 begin
  TAILQ_INIT(@list);
 end;

 TAILQ_INSERT_TAIL(@list,i,@i^.init_entry);

 i^.init_scope:=@self;
end;

//

function t_pm4_resource_curr_scope.Min:p_pm4_resource_instance;
begin
 Result:=resource_instance_set.Min;
end;

function t_pm4_resource_curr_scope.Next(node:p_pm4_resource_instance):p_pm4_resource_instance;
begin
 Result:=resource_instance_set.Next(node);
end;

procedure t_pm4_resource_curr_scope.insert(i:p_pm4_resource_instance);
var
 f:p_pm4_resource_instance;
begin
 f:=resource_instance_set.Find(i);

 if (f<>nil) then
 begin
  //union

  f^.curr:=f^.curr + i^.curr;
  f^.prev:=f^.prev + i^.prev;
  f^.next:=f^.next + i^.next;

 end else
 begin
  resource_instance_set.Insert(i);
 end;

 //
end;

function t_pm4_resource_curr_scope.find_resource_instance(r:p_pm4_resource):p_pm4_resource_instance;
var
 tmp:t_pm4_resource_instance;
begin
 if (r=nil) then Exit(nil);

 tmp:=Default(t_pm4_resource_instance);
 tmp.resource:=r;

 Result:=resource_instance_set.Find(@tmp);
end;

function t_pm4_resource_curr_scope.find_image_resource_instance(const rkey:TvImageKey):p_pm4_resource_instance;
var
 tmp:t_pm4_resource;
begin
 if (rkey.cformat=VK_FORMAT_UNDEFINED) then Exit(nil);
 if (rkey.params.invalid<>0) then Exit(nil);

 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_IMG;
 tmp.rkey :=rkey;

 Result:=find_resource_instance(@tmp);
end;

function t_pm4_resource_curr_scope.find_buffer_resource_instance(rtype:Integer;addr:Pointer;size:DWORD):p_pm4_resource_instance;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=rtype;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=find_resource_instance(@tmp);
end;

function t_pm4_resource_stream_scope.find_image_resource(const rkey:TvImageKey):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_IMG;
 tmp.rkey :=rkey;

 Result:=resource_set.Find(@tmp);
end;

function t_pm4_resource_stream_scope.fetch_image_resource(const rkey:TvImageKey;hint:PChar):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_IMG;
 tmp.rkey :=rkey;

 Result:=resource_set.Find(@tmp);

 if (Result=nil) then
 begin
  tmp.rsize:=get_image_size(rkey);

  if p_print_gpu_ops then
  begin
   Writeln('fetch_image_resource:0x',HexStr(rkey.Addr),' 0x',HexStr(tmp.rsize,4));
  end;

  Result:=allocator.Alloc(SizeOf(t_pm4_resource));
  Result^:=tmp;

  resource_set.Insert(Result);
 end;
end;

function t_pm4_resource_stream_scope.find_buffer_resource(rtype:Integer;addr:Pointer;size:DWORD):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=rtype;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=resource_set.Find(@tmp);
end;

function t_pm4_resource_stream_scope.fetch_buffer_resource(rtype:Integer;addr:Pointer;size:DWORD;hint:PChar):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=rtype;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=resource_set.Find(@tmp);

 if (Result=nil) then
 begin
  Result:=allocator.Alloc(SizeOf(t_pm4_resource));
  Result^:=tmp;

  if p_print_gpu_ops then
  begin
   Writeln('fetch_buffer_resource(',hint,'):0x',HexStr(addr),' 0x',HexStr(size,4));
  end;

  resource_set.Insert(Result);
 end;
end;

function t_pm4_resource_stream_scope.fetch_resource_instance(scope:p_pm4_resource_curr_scope;r:p_pm4_resource;mem_usage:Integer;img_usage:s_image_usage):p_pm4_resource_instance;
var
 curr:t_pm4_usage;
begin
 Result:=scope^.find_resource_instance(r);

 if (Result=nil) then
 begin
  Result:=allocator.Alloc(SizeOf(t_pm4_resource_instance));
  Result^:=Default(t_pm4_resource_instance);
  //
  Result^.resource:=r;
 end;

 {
 TODO: This is a hack for loading storage textures that
 may not be fully filled, or may only attached only one texture layer.
 }
 if (iu_storage in img_usage) then
 begin
  mem_usage:=mem_usage or TM_READ;
 end;

 curr.mem_usage:=mem_usage;
 curr.shd_usage:=0;
 curr.clr_usage:=0;
 curr.dsa_usage:=0;
 curr.img_usage:=img_usage;

 if ([iu_sampled,iu_storage]*img_usage<>[]) then
 begin
  curr.shd_usage:=mem_usage;
 end;

 if (iu_attachment in img_usage) then
 begin
  curr.clr_usage:=mem_usage;
 end;

 if (iu_depthstenc in img_usage) then
 begin
  curr.dsa_usage:=mem_usage;
 end;

 Result^.curr:=Result^.curr + curr;

 r^.uall:=r^.uall + curr;
end;

function t_pm4_resource_stream_scope.insert_image_resource(scope:p_pm4_resource_curr_scope;const rkey:TvImageKey;mem_usage:Integer;img_usage:s_image_usage;hint:PChar):p_pm4_resource_instance;
var
 r:p_pm4_resource;
 i:p_pm4_resource_instance;
begin
 if (rkey.cformat=VK_FORMAT_UNDEFINED) then Exit(nil);
 if (rkey.params.invalid<>0) then Exit(nil);

 r:=fetch_image_resource   (rkey,hint);
 i:=fetch_resource_instance(scope,r,mem_usage,img_usage);

 if ((mem_usage and TM_READ)<>0) then
 if (i^.prev.mem_usage=0) then //no prev usage
 begin
  //init
  init_scope.insert(i);
 end;

 scope^.insert(i);

 Result:=i;
end;

function t_pm4_resource_stream_scope.insert_buffer_resource(scope:p_pm4_resource_curr_scope;rtype:Integer;addr:Pointer;size:DWORD;mem_usage:Integer;hint:PChar):p_pm4_resource_instance;
var
 r:p_pm4_resource;
 i:p_pm4_resource_instance;
begin
 r:=fetch_buffer_resource  (rtype,addr,size,hint);
 i:=fetch_resource_instance(scope,r,mem_usage,[iu_buffer]);

 if ((mem_usage and TM_READ)<>0) then
 if (i^.prev.mem_usage=0) then //no prev usage
 begin
  //init
  init_scope.insert(i);
 end;

 scope^.insert(i);

 Result:=i;
end;

//

procedure t_pm4_resource_stream_scope.connect_resource_instance(i:p_pm4_resource_instance);
var
 start:Pointer;
 __end:Pointer;
 node:p_pm4_resource;
 prev:p_pm4_resource_instance;
 tmp:t_pm4_resource;
begin
 //find cross

 tmp:=Default(t_pm4_resource);
 tmp:=i^.resource^;

 start:=tmp.rkey.Addr;
 __end:=start+tmp.rsize;

 tmp.rtype:=High(Integer);
 tmp.rkey.Addr:=start;

 //[s|new|e] ->
 //      [s|old|e]

 node:=resource_set.Find_ls(@tmp);

 while (node<>nil) do
 begin
  //

  if (__end>(node^.rkey.Addr)) and (start<(node^.rkey.Addr+node^.rsize)) then
  begin
   prev:=node^.rwrite;
   if (prev<>nil) and (prev<>i) then
   begin
    //sum prev of curr
       i^.prev:=   i^.prev + prev^.curr;
    //sum next of prev
    prev^.next:=prev^.next + i^.curr;

    if (prev^.resource<>i^.resource) then
    begin
     //sum prev of curr
        i^.prev_overlap:=   i^.prev_overlap + prev^.curr;
     //sum next of prev
     prev^.next_overlap:=prev^.next_overlap + i^.curr;
    end;

   end;
   //
   if ((i^.curr.mem_usage and (TM_WRITE or TM_CLEAR))<>0) then
   begin
    node^.rwrite:=i;
   end;
  end;

  node:=resource_set.Prev(node);
 end;
end;

procedure t_pm4_resource_stream_scope.connect_resource_scope(scope:p_pm4_resource_curr_scope);
var
 node:p_pm4_resource_instance;
begin
 node:=scope^.resource_instance_set.Min;

 while (node<>nil) do
 begin

  connect_resource_instance(node);

  node:=scope^.resource_instance_set.Next(node);
 end;

end;

//

procedure t_pm4_stream.Free;
begin
 list:=Default(TAILQ_HEAD);
 allocator.Free;
end;

var
 global_id:QWORD=0;

Procedure t_pm4_stream.add_node(node:p_pm4_node);
begin
 if (list.tqh_last=nil) then
 begin
  TAILQ_INIT(@list);
 end;

 node^.id:=System.InterlockedIncrement64(global_id);
 //Writeln('add_node:',node^.id);

 TAILQ_INSERT_TAIL(@list,node,@node^.entry);

 connect_resource_scope(@node^.scope);
end;

function t_pm4_stream.First:p_pm4_node;
begin
 Result:=TAILQ_FIRST(@list);
end;

function t_pm4_stream.Next(node:p_pm4_node):p_pm4_node;
begin
 Result:=TAILQ_NEXT(node,@node^.entry);
end;

//

procedure t_pm4_stream.Acquire;
begin
 System.InterlockedIncrement(Pointer(refs));
end;

function t_pm4_stream.Release:Boolean;
begin
 Result:=System.InterlockedDecrement(Pointer(refs))=nil;
end;

//

procedure t_pm4_stream.Hint(P1,P2:PChar;maxsize:Integer);
var
 len1,len2:Integer;
 node:p_pm4_node_Hint;
begin
 len1:=StrLen(P1);

 len2:=0;
 while (maxsize<>0) do
 begin
  Inc(len2);
  if (P2[len2]=#0) then
  begin
   Break;
  end;
  Dec(maxsize);
 end;

 node:=allocator.Alloc(SizeOf(t_pm4_node_Hint)+len1+len2+1);

 node^.ntype :=ntHint;
 node^.scope :=Default(t_pm4_resource_curr_scope);

 Move(P1^,PChar(@node^.data)[0]   ,len1);
 Move(P2^,PChar(@node^.data)[len1],len2);
 PChar(@node^.data)[len1+len2]:=#0;

 add_node(node);
end;

procedure t_pm4_stream.LoadConstRam(addr:Pointer;num_dw,offset:Word);
var
 node:p_pm4_node_LoadConstRam;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_LoadConstRam));

 node^.ntype :=ntLoadConstRam;
 node^.scope :=Default(t_pm4_resource_curr_scope);
 node^.addr  :=addr;
 node^.num_dw:=num_dw;
 node^.offset:=offset;

 insert_buffer_resource(@node^.scope,
                        R_BUF,
                        addr,
                        num_dw*SizeOf(DWORD),
                        TM_READ,
                        'LoadConstRam');

 add_node(node);
end;

procedure t_pm4_stream.DumpConstRam(addr:Pointer;num_dw,offset:Word);
var
 node:p_pm4_node_LoadConstRam;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_LoadConstRam));

 node^.ntype :=ntDumpConstRam;
 node^.scope :=Default(t_pm4_resource_curr_scope);
 node^.addr  :=addr;
 node^.num_dw:=num_dw;
 node^.offset:=offset;

 insert_buffer_resource(@node^.scope,
                        R_BUF,
                        addr,
                        num_dw*SizeOf(DWORD),
                        TM_WRITE,
                        'DumpConstRam');

 add_node(node);
end;

procedure t_pm4_stream.IncrementCE();
var
 node:p_pm4_node;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node));

 node^.ntype:=ntIncrementCE;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 add_node(node);
end;

procedure t_pm4_stream.IncrementDE();
var
 node:p_pm4_node;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node));

 node^.ntype:=ntIncrementDE;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 add_node(node);
end;

procedure t_pm4_stream.WaitOnCECounter();
var
 node:p_pm4_node;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node));

 node^.ntype:=ntWaitOnCECounter;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 add_node(node);
end;

procedure t_pm4_stream.WaitOnDECounterDiff(diff:DWORD);
var
 node:p_pm4_node_WaitOnDECounterDiff;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_WaitOnDECounterDiff));

 node^.ntype:=ntWaitOnDECounterDiff;
 node^.scope:=Default(t_pm4_resource_curr_scope);
 node^.diff :=diff;

 add_node(node);
end;

procedure t_pm4_stream.EventWrite(eventType:Byte);
var
 node:p_pm4_node_EventWrite;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWrite));

 node^.ntype    :=ntEventWrite;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.eventType:=eventType;

 add_node(node);
end;

procedure t_pm4_stream.PipeStatDump(Address:QWORD;Control:TPixelPipeStatControl);
var
 node:p_pm4_node_PipeStatDump;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_PipeStatDump));

 node^.ntype  :=ntPipeStatDump;
 node^.scope  :=Default(t_pm4_resource_curr_scope);
 node^.Address:=Address;
 node^.Control:=Control;

 add_node(node);
end;

procedure t_pm4_stream.EventWriteEop(addr:Pointer;data:QWORD;eventType,dataSel,intSel:Byte);
var
 node:p_pm4_node_EventWriteEop;

 function get_data_size:DWORD; inline;
 begin
  Result:=0;
  //
  Case dataSel of
   EVENTWRITEEOP_DATA_SEL_SEND_DATA32        :Result:=4;
   EVENTWRITEEOP_DATA_SEL_SEND_DATA64        :Result:=8;
   EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK     :Result:=8;
   EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:Result:=8;
   else;
  end;
 end;

begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWriteEop));

 node^.ntype    :=ntEventWriteEop;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.addr     :=addr;
 node^.data     :=data;
 node^.eventType:=eventType;
 node^.dataSel  :=dataSel;
 node^.intSel   :=intSel;

 if (addr<>nil) then
 begin
  insert_buffer_resource(@node^.scope,
                         R_BUF,
                         addr,
                         get_data_size,
                         TM_WRITE,
                         'EventWriteEop');
 end;

 add_node(node);
end;

procedure t_pm4_stream.EventWriteEos(addr:Pointer;data:DWORD;eventType,command:Byte);
var
 node:p_pm4_node_EventWriteEos;

 function get_data_size:DWORD; inline;
 begin
  Result:=0;
  //
  Case command of
   EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY:Result:=4;
   else;
  end;
 end;

begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWriteEos));

 node^.ntype    :=ntEventWriteEos;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.addr     :=addr;
 node^.data     :=data;
 node^.eventType:=eventType;
 node^.command  :=command;

 if (addr<>nil) then
 begin
  insert_buffer_resource(@node^.scope,
                         R_BUF,
                         addr,
                         get_data_size,
                         TM_WRITE,
                         'EventWriteEos');
 end;

 add_node(node);
end;

procedure t_pm4_stream.SubmitFlipEop(eop_value:QWORD;intSel:Byte);
var
 node:p_pm4_node_SubmitFlipEop;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_SubmitFlipEop));

 node^.ntype    :=ntSubmitFlipEop;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.eop_value:=eop_value;
 node^.intSel   :=intSel;

 add_node(node);
end;

procedure t_pm4_stream.ReleaseMem(addr:Pointer;data:QWORD;eventType,srcSel,dstSel,intSel:Byte);
var
 node:p_pm4_node_ReleaseMem;

 function get_data_size:DWORD; inline;
 begin
  Result:=0;
  //
  Case srcSel of
   RELEASEMEM_DATA_SEL_SEND_DATA32        :Result:=4;
   RELEASEMEM_DATA_SEL_SEND_DATA64        :Result:=8;
   RELEASEMEM_DATA_SEL_SEND_GPU_CLOCK     :Result:=8;
   RELEASEMEM_DATA_SEL_SEND_CP_PERFCOUNTER:Result:=8;
   else;
  end;
 end;

begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_ReleaseMem));

 node^.ntype    :=ntReleaseMem;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.addr     :=addr;
 node^.data     :=data;
 node^.eventType:=eventType;
 node^.srcSel   :=srcSel;
 node^.dstSel   :=dstSel;
 node^.intSel   :=intSel;

 if (addr<>nil) then
 begin
  insert_buffer_resource(@node^.scope,
                         R_BUF,
                         addr,
                         get_data_size,
                         TM_WRITE,
                         'ReleaseMem');
 end;

 add_node(node);
end;

procedure t_pm4_stream.DmaData(dstSel:Byte;dst:QWORD;srcSel:Byte;srcOrData:QWORD;numBytes:DWORD;isBlocking:Byte);
var
 node:p_pm4_node_DmaData;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_DmaData));

 node^.ntype   :=ntDmaData;
 node^.scope   :=Default(t_pm4_resource_curr_scope);
 node^.dst     :=dst;
 node^.src     :=srcOrData;
 node^.numBytes:=numBytes;
 node^.srcSel  :=srcSel;
 node^.dstSel  :=dstSel;
 node^.cpSync  :=isBlocking;

 case srcSel of
  kDmaDataSrcMemory,
  kDmaDataSrcMemoryUsingL2:
   if (srcOrData<>0) then
   begin
    insert_buffer_resource(@node^.scope,
                           R_BUF,
                           Pointer(srcOrData),
                           numBytes,
                           TM_READ,
                           'DmaData');
   end;
  else;
 end;

 case dstSel of
  kDmaDataDstMemory,
  kDmaDataDstMemoryUsingL2:
  if (dst<>0) then
  begin
   insert_buffer_resource(@node^.scope,
                          R_BUF,
                          Pointer(dst),
                          numBytes,
                          TM_WRITE,
                          'DmaData');
  end;
 end;

 add_node(node);
end;

procedure t_pm4_stream.WriteData(dstSel:Byte;dst,src:Pointer;num_dw:Word;wrConfirm:Byte);
var
 node:p_pm4_node_WriteData;
begin
 //Can I copy the link?
 //Or do I have to copy the data?

 node:=allocator.Alloc(SizeOf(t_pm4_node_WriteData){+num_dw*SizeOf(DWORD)});

 node^.ntype    :=ntWriteData;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.dst      :=dst;
 //node^.src      :=Pointer(node+1);
 node^.src      :=src;
 node^.num_dw   :=num_dw;
 node^.dstSel   :=dstSel;
 node^.wrConfirm:=(wrConfirm<>0);

 //Move(src^,node^.src^,num_dw*SizeOf(DWORD));

 if (src<>nil) then
 begin
  insert_buffer_resource(@node^.scope,
                         R_BUF,
                         src,
                         num_dw*SizeOf(DWORD),
                         TM_READ,
                         'WriteData');
 end;

 case dstSel of
  WRITE_DATA_DST_SEL_MEMORY_SYNC,
  WRITE_DATA_DST_SEL_TCL2,
  WRITE_DATA_DST_SEL_MEMORY_ASYNC:
   if (dst<>nil) then
   begin
    insert_buffer_resource(@node^.scope,
                           R_BUF,
                           Pointer(dst),
                           num_dw*SizeOf(DWORD),
                           TM_WRITE,
                           'WriteData');
   end;
  else;
 end;

 add_node(node);
end;

procedure t_pm4_stream.WaitRegMem(pollAddr:Pointer;refValue,mask:DWORD;compareFunc:Byte);
var
 node:p_pm4_node_WaitRegMem;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_WaitRegMem));

 node^.ntype      :=ntWaitRegMem;
 node^.scope      :=Default(t_pm4_resource_curr_scope);
 node^.pollAddr   :=pollAddr;
 node^.refValue   :=refValue;
 node^.mask       :=mask;
 node^.compareFunc:=compareFunc;

 add_node(node);
end;

procedure t_pm4_stream.FastClear(var CX_REG:TCONTEXT_REG_GROUP);
var
 GPU_REGS:TGPU_REGS;
 RT:TRT_INFO;

 node:p_pm4_node_FastClear;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.CX_REG:=@CX_REG;

 node:=allocator.Alloc(SizeOf(t_pm4_node_FastClear));

 node^.ntype :=ntFastClear;
 node^.scope :=Default(t_pm4_resource_curr_scope);

 //
 RT:=GPU_REGS.GET_RT_INFO(0,True);

 //-TM_READ +TM_CLEAR
 RT.IMAGE_USAGE:=RT.IMAGE_USAGE and (not TM_READ) or TM_CLEAR;

 Assert(RT.CMASK_INFO.KEY.Addr<>nil);

 //

 insert_image_resource(@node^.scope,
                       RT.FImageInfo,
                       RT.IMAGE_USAGE,
                       [iu_transfer],
                       'FastClear');

 insert_buffer_resource(@node^.scope,
                        R_CMASK,
                        RT.CMASK_INFO.KEY.Addr,
                        RT.CMASK_INFO.SIZE,
                        RT.IMAGE_USAGE,
                        'FastClear'
                       );

 //
 node^.RT:=RT;
 //

 add_node(node);
end;

procedure t_pm4_stream.Resolve(var CX_REG:TCONTEXT_REG_GROUP);
var
 GPU_REGS:TGPU_REGS;
 RT:array[0..1] of TRT_INFO;
 SCREEN:TVkRect2D;

 node:p_pm4_node_Resolve;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.CX_REG:=@CX_REG;

 Assert(DWORD(CX_REG.CB_TARGET_MASK)=$F);

 node:=allocator.Alloc(SizeOf(t_pm4_node_Resolve));

 node^.ntype :=ntResolve;
 node^.scope :=Default(t_pm4_resource_curr_scope);

 //
 RT[0]:=GPU_REGS.GET_RT_INFO(0,True);
 RT[1]:=GPU_REGS.GET_RT_INFO(1,True);

 RT[0].IMAGE_USAGE:=TM_READ;
 RT[1].IMAGE_USAGE:=TM_WRITE;

 insert_image_resource(@node^.scope,
                       RT[0].FImageInfo,
                       RT[0].IMAGE_USAGE,
                       [iu_transfer],
                       'Resolve');

 insert_image_resource(@node^.scope,
                       RT[1].FImageInfo,
                       RT[1].IMAGE_USAGE,
                       [iu_transfer],
                       'Resolve');

 SCREEN:=GPU_REGS.GET_SCREEN;

 node^.RT:=RT;
 node^.SCREEN:=SCREEN;

 add_node(node);
end;

function t_pm4_stream.ColorControl(var CX_REG:TCONTEXT_REG_GROUP):Boolean;
begin
 Result:=False;

 case CX_REG.CB_COLOR_CONTROL.MODE of
  CB_DISABLE:
   if p_print_gpu_ops then
   begin
    Writeln('DISABLE');
   end;
  CB_NORMAL:; //next
  CB_ELIMINATE_FAST_CLEAR:
   // Expand latest specified clear color into pixel data for the fast cleared color/depth resource.
   begin
    FastClear(CX_REG);
    Exit(True);
   end;
  CB_RESOLVE:
   // Fixed function resolve. (MSAA)
   begin
    Resolve(CX_REG);
    Exit(True);
   end;
  CB_DECOMPRESS:
   if p_print_gpu_ops then
   begin
    Writeln('DECOMPRESS');
   end;
  CB_FMASK_DECOMPRESS: // Fmask decompression for shader readability.
   if p_print_gpu_ops then
   begin
    Writeln('FMASK_DECOMPRESS');
   end;
  CB_DCC_DECOMPRESS: // Indicates this color target view is for a DCC decompress
   if p_print_gpu_ops then
   begin
    Writeln('DCC_DECOMPRESS');
   end;
  else
   Assert(False,'unknow color control:0x'+HexStr(CX_REG.CB_COLOR_CONTROL.MODE,1));
 end;

end;

procedure t_pm4_stream.Init_Uniforms(node:p_pm4_node;var UniformBuilder:TvUniformBuilder);
var
 i:Integer;
begin

 //images
 if (Length(UniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FImages) do
  With UniformBuilder.FImages[i] do
  begin

   case btype of
    vbSampled:
     begin
      insert_image_resource(@node^.scope,
                            FImage,
                            memuse,
                            [iu_sampled],
                            'Init_Uniforms');
     end;
    vbStorage,
    vbMipStorage:
     begin
      insert_image_resource(@node^.scope,
                            FImage,
                            memuse,
                            [iu_storage],
                            'Init_Uniforms');
     end;
    else
     Assert(false);
   end;

  end;
 end;
 //images

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  if (memuse and TM_INVAL)=0 then
  begin

   insert_buffer_resource(@node^.scope,
                          R_BUF,
                          addr,
                          size,
                          memuse,
                          'Init_Uniforms');

  end;
 end;
 //buffers

end;

procedure t_pm4_stream.Init_Pushs(node:p_pm4_node;
                                  ShaderGroup:TvShaderGroup;
                                  var GPU_REGS:TGPU_REGS);
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
   FData:=GPU_REGS.get_user_data(i);
   addr :=Shader.GetPushConstData(FData);

   insert_buffer_resource(@node^.scope,
                          R_BUF,
                          addr,
                          Shader.FPushConst.size,
                          TM_READ,
                          'Init_Pushs');

  end;
 end;
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

procedure t_pm4_stream.Build_rt_info(node:p_pm4_node;
                                     var rt_info:t_pm4_rt_info;
                                     var GPU_REGS:TGPU_REGS;
                                     ShaderDrawParams:t_shader_draw_params);
var
 i:Integer;
 RT:TRT_INFO;
 FUniformBuilder:TvUniformBuilder;

 pa:TPushConstAllocator;
 pp:PPushConstAllocator;

 r:p_pm4_resource;
 resource_instance:p_pm4_resource_instance;
begin

 if (p_neomode<>0) then
 begin
  //FP16_INTERP_MODE support only Neo mode
  for i:=0 to 31 do
  begin
   Assert(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FP16_INTERP_MODE=0,'SPI_PS_INPUT_CNTL['+IntToStr(i)+'].FP16_INTERP_MODE='+IntToStr(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FP16_INTERP_MODE));
  end;
 end;

 GPU_REGS.export_user_data_rt(@rt_info.USERDATA);

 //copy
 rt_info.SHADERDATA.SG_REG                  :=GPU_REGS.SG_REG^                         ;
 rt_info.SHADERDATA.SPI_PS_INPUT_ENA        :=GPU_REGS.CX_REG^.SPI_PS_INPUT_ENA        ;
 rt_info.SHADERDATA.SPI_PS_INPUT_ADDR       :=GPU_REGS.CX_REG^.SPI_PS_INPUT_ADDR       ;
 rt_info.SHADERDATA.SPI_INTERP_CONTROL_0    :=GPU_REGS.CX_REG^.SPI_INTERP_CONTROL_0    ;
 rt_info.SHADERDATA.SPI_PS_IN_CONTROL       :=GPU_REGS.CX_REG^.SPI_PS_IN_CONTROL       ;
 rt_info.SHADERDATA.SPI_PS_INPUT_CNTL       :=GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL       ;
 rt_info.SHADERDATA.DB_SHADER_CONTROL       :=GPU_REGS.CX_REG^.DB_SHADER_CONTROL       ;
 rt_info.SHADERDATA.VGT_INSTANCE_STEP_RATE_0:=GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_0;
 rt_info.SHADERDATA.VGT_INSTANCE_STEP_RATE_1:=GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_1;
 rt_info.SHADERDATA.RENDER_TARGET           :=GPU_REGS.CX_REG^.RENDER_TARGET           ;
 rt_info.SHADERDATA.UC_REG                  :=GPU_REGS.UC_REG^;

 rt_info.RT_COUNT:=0;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to GPU_REGS.GET_HI_RT do
  begin
   RT:=GPU_REGS.GET_RT_INFO(i);

   //

   if (RT.CMASK_INFO.KEY.Addr<>nil) then
   begin
    //perfetch check
    r:=find_buffer_resource(R_CMASK,RT.CMASK_INFO.KEY.Addr,RT.CMASK_INFO.SIZE);

    if (r<>nil) then
    if (r^.rcmask) then
    begin
     //-TM_READ +TM_CLEAR
     RT.IMAGE_USAGE:=RT.IMAGE_USAGE and (not TM_READ) or TM_CLEAR;
     //
     r^.rcmask:=False;
    end;

    insert_buffer_resource(@node^.scope,
                           R_CMASK,
                           RT.CMASK_INFO.KEY.Addr,
                           RT.CMASK_INFO.SIZE,
                           RT.IMAGE_USAGE,
                           'Build_rt_info');
   end;

   insert_image_resource(@node^.scope,
                         RT.FImageInfo,
                         RT.IMAGE_USAGE,
                         [iu_attachment],
                         'Build_rt_info');

   //

   rt_info.RT_INFO[rt_info.RT_COUNT]:=RT;

   Inc(rt_info.RT_COUNT);
  end;

 rt_info.DB_ENABLE:=GPU_REGS.DB_ENABLE;

 if rt_info.DB_ENABLE then
 begin
  rt_info.DB_INFO:=GPU_REGS.GET_DB_INFO;

  //

  resource_instance:=insert_image_resource(@node^.scope,
                                           GetDepthOnly(rt_info.DB_INFO.FImageInfo),
                                           rt_info.DB_INFO.DEPTH_USAGE,
                                           [iu_depthstenc],
                                           'Build_rt_info');

  if (resource_instance<>nil) then
  with resource_instance^.resource^ do
  begin
   rcombined:=rcombined or IsDepthAndStencil(rt_info.DB_INFO.FImageInfo.cformat);
  end;

  resource_instance:=insert_image_resource(@node^.scope,
                                           GetStencilOnly(rt_info.DB_INFO.FImageInfo),
                                           rt_info.DB_INFO.STENCIL_USAGE,
                                           [iu_depthstenc],
                                           'Build_rt_info');

  if (resource_instance<>nil) then
  with resource_instance^.resource^ do
  begin
   rcombined:=rcombined or IsDepthAndStencil(rt_info.DB_INFO.FImageInfo.cformat);
  end;

  if (rt_info.DB_INFO.HTILE_INFO.TILE_SURFACE_ENABLE<>0) then
  begin
   resource_instance:=insert_buffer_resource(@node^.scope,
                                             R_HTILE,
                                             rt_info.DB_INFO.HTILE_INFO.KEY.Addr,
                                             rt_info.DB_INFO.HTILE_INFO.SIZE,
                                             rt_info.DB_INFO.DEPTH_USAGE,
                                             'Build_rt_info');
  end;

 end;

 if (rt_info.RT_COUNT=0) and (not rt_info.DB_ENABLE) then
 begin
  Writeln('zero attachment???');
 end;

 rt_info.BLEND_INFO:=GPU_REGS.GET_BLEND_INFO;

 rt_info.PRIM_TYPE :=ord(GPU_REGS.GET_PRIM_TYPE);
 rt_info.PRIM_RESET:=GPU_REGS.GET_PRIM_RESET;

 rt_info.VP_COUNT:=0;

 For i:=0 to 15 do
  if GPU_REGS.VP_ENABLE(i) then
  begin
   rt_info.VPORT  [rt_info.VP_COUNT]:=GPU_REGS.GET_VPORT  (i);
   rt_info.SCISSOR[rt_info.VP_COUNT]:=GPU_REGS.GET_SCISSOR(i);

   Inc(rt_info.VP_COUNT);
  end;

 rt_info.RASTERIZATION:=GPU_REGS.GET_RASTERIZATION;
 rt_info.MULTISAMPLE  :=GPU_REGS.GET_MULTISAMPLE;

 rt_info.PROVOKING:=ord(GPU_REGS.GET_PROVOKING);

 rt_info.SCREEN_RECT:=GPU_REGS.GET_SCREEN;
 rt_info.SCREEN_SIZE:=GPU_REGS.GET_SCREEN_SIZE;

 rt_info.ShaderDrawParams:=ShaderDrawParams;

 //

 pa.Init;
 pp:=@pa;

 rt_info.ShaderGroup:=FetchShaderGroupRT(GPU_REGS,pp);
 Assert(rt_info.ShaderGroup<>nil);

 //DumpShaderGroup(rt_info.ShaderGroup);

 //

 FUniformBuilder:=Default(TvUniformBuilder);
 rt_info.ShaderGroup.ExportUnifBuilder(FUniformBuilder,@rt_info.USERDATA);

 Init_Uniforms(node,FUniformBuilder);
end;

function t_pm4_stream.BuildDraw(ntype:t_pm4_node_type;context:p_amd_context):p_pm4_node_draw;
var
 GPU_REGS:TGPU_REGS;

 node:p_pm4_node_draw;

begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SG_REG:=@context^.SG_REG;
 GPU_REGS.CX_REG:=@context^.CX_REG;
 GPU_REGS.UC_REG:=@context^.UC_REG;
 GPU_REGS.SDP   :=@context^.ShaderDrawParams;

 if DWORD(context^.CX_REG.VGT_SHADER_STAGES_EN)<>0 then
 begin
  Writeln('Skip tessellation:0x',HexStr(DWORD(context^.CX_REG.VGT_SHADER_STAGES_EN),8));
  Exit(nil);
 end;

 node:=allocator.Alloc(SizeOf(t_pm4_node_draw));

 node^:=Default(t_pm4_node_draw); //init all to zero

 node^.ntype :=ntype;
 //node^.scope :=Default(t_pm4_resource_curr_scope);

 Build_rt_info(node,node^.rt_info,GPU_REGS,context^.ShaderDrawParams);

 case ntype of
  ntDrawIndex2,
  ntDrawIndexOffset2,
  ntDrawIndexAuto:
   begin
    node^.u.indexBase   :=context^.CX_REG.VGT_DMA_BASE or (QWORD(context^.CX_REG.VGT_DMA_BASE_HI.BASE_ADDR) shl 32);
    node^.u.vertexOffset:=context^.CX_REG.VGT_INDX_OFFSET;
    node^.u.indexCount  :=context^.UC_REG.VGT_NUM_INDICES;
    node^.u.numInstances:=context^.UC_REG.VGT_NUM_INSTANCES;
    node^.u.INDEX_TYPE  :=ord(GPU_REGS.GET_INDEX_TYPE);
    node^.u.SWAP_MODE   :=context^.CX_REG.VGT_DMA_INDEX_TYPE.SWAP_MODE;
   end;
  else;
 end;

 //heuristic
 if (ntype=ntDrawIndexAuto) and
    (node^.u.numInstances<=1) and
    (node^.rt_info.RT_COUNT=0) and
    (node^.rt_info.DB_ENABLE) and
    (
     ((node^.rt_info.DB_INFO.DEPTH_USAGE   and TM_CLEAR)<>0) or
     ((node^.rt_info.DB_INFO.STENCIL_USAGE and TM_CLEAR)<>0)
    ) then

 if IsClearDepthShaders(node^.rt_info.ShaderGroup.FKey.FShaders) then
 begin
  //ClearDepthTarget

  node^.ntype:=ntClearDepth;
 end;

 //clearRenderTarget
 //VS 0xFE54CC4687E2FF59
 //PS 0x91E6C1F562F6F2DE

 add_node(node);
 Result:=node;
end;

procedure t_pm4_stream.DrawIndex2(context:p_amd_context);
begin
 if ColorControl(context^.CX_REG) then Exit;

 BuildDraw(ntDrawIndex2,context);
end;

procedure t_pm4_stream.DrawIndexAuto(context:p_amd_context);
begin
 if ColorControl(context^.CX_REG) then Exit;

 BuildDraw(ntDrawIndexAuto,context);
end;

procedure t_pm4_stream.DrawIndexOffset2(context:p_amd_context;
                                        indexOffset:DWORD);
var
 node:p_pm4_node_draw;
begin
 if ColorControl(context^.CX_REG) then Exit;

 node:=BuildDraw(ntDrawIndexOffset2,context);

 if (node<>nil) then
 begin
  node^.u.indexOffset:=indexOffset;
 end;

end;

procedure t_pm4_stream.DrawIndexIndirect(context   :p_amd_context;
                                         dataOffset:DWORD);
var
 node:p_pm4_node_draw;
begin
 if ColorControl(context^.CX_REG) then Exit;

 node:=BuildDraw(ntDrawIndexIndirect,context);

 if (node<>nil) then
 begin
  node^.u.indirectBase:=context^.BASE_ADDR_DRAW_INDIRECT;
  node^.u.dataOffset  :=dataOffset;
  node^.u.stride      :=sizeof(TDrawIndexedIndirectArgs);
  node^.u.count       :=1;
  node^.u.countAddr   :=0;
 end;

end;

procedure t_pm4_stream.DrawIndexIndirectCountMulti(context   :p_amd_context;
                                                   dataOffset:DWORD;
                                                   stride    :DWORD;
                                                   count     :DWORD;
                                                   countAddr :QWORD);
var
 node:p_pm4_node_draw;
begin
 if ColorControl(context^.CX_REG) then Exit;

 node:=BuildDraw(ntDrawIndexIndirectCountMulti,context);

 if (node<>nil) then
 begin
  node^.u.indirectBase:=context^.BASE_ADDR_DRAW_INDIRECT;
  node^.u.dataOffset  :=dataOffset;
  node^.u.stride      :=stride;
  node^.u.count       :=count;
  node^.u.countAddr   :=countAddr;
 end;

end;

procedure t_pm4_stream.Build_cs_info(node:p_pm4_node_Dispatch;var GPU_REGS:TGPU_REGS);
var
 dst:PGPU_USERDATA;
 FUniformBuilder:TvUniformBuilder;

 pa:TPushConstAllocator;
 pp:PPushConstAllocator;
begin

 //copy
 node^.COMPUTE_GROUP:=GPU_REGS.SC_REG^;

 //hack
 dst:=Pointer(@node^.COMPUTE_GROUP.COMPUTE_USER_DATA)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 pa.Init;
 pp:=@pa;

 node^.ShaderGroup:=FetchShaderGroupCS(GPU_REGS,pp);
 Assert(node^.ShaderGroup<>nil);

 //

 FUniformBuilder:=Default(TvUniformBuilder);
 node^.ShaderGroup.ExportUnifBuilder(FUniformBuilder,dst);

 Init_Uniforms(node,FUniformBuilder);
end;

procedure t_pm4_stream.DispatchDirect(var SC_REG:TSH_REG_COMPUTE_GROUP);
var
 GPU_REGS:TGPU_REGS;

 node:p_pm4_node_DispatchDirect;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SC_REG:=@SC_REG;

 node:=allocator.Alloc(SizeOf(t_pm4_node_DispatchDirect));

 node^.ntype:=ntDispatchDirect;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 Build_cs_info(node,GPU_REGS);

 node^.DIM_X:=GPU_REGS.SC_REG^.COMPUTE_DIM_X;
 node^.DIM_Y:=GPU_REGS.SC_REG^.COMPUTE_DIM_Y;
 node^.DIM_Z:=GPU_REGS.SC_REG^.COMPUTE_DIM_Z;

 add_node(node);
end;

procedure t_pm4_stream.DispatchIndirect(var SC_REG:TSH_REG_COMPUTE_GROUP;
                                        BASE  :QWORD;
                                        Offset:DWORD);
var
 GPU_REGS:TGPU_REGS;

 node:p_pm4_node_DispatchIndirect;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SC_REG:=@SC_REG;

 node:=allocator.Alloc(SizeOf(t_pm4_node_DispatchIndirect));

 node^.ntype:=ntDispatchIndirect;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 Build_cs_info(node,GPU_REGS);

 node^.BASE  :=BASE;
 node^.Offset:=Offset;

 if (BASE<>0) then
 begin
  insert_buffer_resource(@node^.scope,
                         R_BUF,
                         Pointer(BASE+Offset),
                         3*4,
                         TM_READ,
                         'DispatchIndirect');
 end;

 add_node(node);
end;

procedure t_pm4_stream.PfpSyncMe(event:PRTLEvent);
var
 node:p_pm4_node_PfpSyncMe;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_PfpSyncMe));

 node^.ntype:=ntPfpSyncMe;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 node^.event:=event;

 add_node(node);
end;

//

procedure t_cache_block_allocator.init;
begin
 queue.Create;
 xlock:=nil;
 count:=0;
end;

Function t_cache_block_allocator.Alloc:Pointer;
begin
 Result:=nil;
 if (XCHG(xlock,Pointer(1))=nil) then
 begin
  if queue.Pop(Result) then
  begin
   XCHG(xlock,nil);
   //
   System.InterlockedDecrement64(count);
   Exit;
  end;
  XCHG(xlock,nil);
 end;
 //
 Result:=kmem_alloc(mem_size,VM_RW);
 Assert(Result<>nil);
end;

Procedure t_cache_block_allocator.Free(node:Pointer);
begin
 if (node=nil) then Exit;
 //
 if (count+1<=max_count) then
 begin
  if (System.InterlockedIncrement64(count)<=max_count) then
  begin
   queue.Push(node);
   Exit;
  end else
  begin
   System.InterlockedDecrement64(count);
  end;
 end;
 //
 kmem_free(node,mem_size);
end;

//

Function t_pm4_allocator.Alloc(Size:ptruint):Pointer;
var
 mem_size:ptruint;
 node:PAllocNode;

begin
 if (pHead.slh_first=nil) or (Size>curr_size) then
 begin

  if (Size>(cache_block_allocator.mem_size-SizeOf(TAllocNode))) then
  begin
   mem_size:=Align(Size+SizeOf(TAllocNode),64*1024);
   //
   node:=kmem_alloc(mem_size,VM_RW);
   Assert(node<>nil);
  end else
  begin
   mem_size:=cache_block_allocator.mem_size;
   //
   node:=cache_block_allocator.Alloc;
  end;

  node^.size:=mem_size;

  SLIST_INSERT_HEAD(@pHead,node,@node^.link);

  curr_apos:=0;
  curr_size:=mem_size-SizeOf(TAllocNode);
  Inc(full_size,mem_size);
 end;

 node:=SLIST_FIRST(@pHead);

 Result:=@PByte(@node^.data)[curr_apos];

 Inc(used_size,Size);
 Size:=Align(Size,SizeOf(ptruint));
 Inc(curr_apos,Size);
 Dec(curr_size,Size);
end;

Procedure t_pm4_allocator.Free;
var
 node:PAllocNode;
begin
 node:=SLIST_FIRST(@pHead);

 While (node<>nil) do
 begin
  SLIST_REMOVE(@pHead,node,@node^.link);

  if (node^.size=cache_block_allocator.mem_size) then
  begin
   cache_block_allocator.Free(node);
  end else
  begin
   kmem_free(node,node^.size);
  end;

  node:=SLIST_FIRST(@pHead);
 end;
 Self:=Default(t_pm4_allocator);
end;


initialization
 cache_block_allocator.Init;

end.

