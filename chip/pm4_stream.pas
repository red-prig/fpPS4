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
 t_pm4_rt_info=object
  USERDATA:TGPU_USERDATA;

  ShaderGroup:TvShaderGroup;

  RT_INFO:array[0..7] of TRT_INFO;
  DB_INFO:TDB_INFO;

  BLEND_INFO:TBLEND_INFO;

  VPORT  :array[0..15] of TVkViewport;
  SCISSOR:array[0..15] of TVkRect2D;

  RASTERIZATION:TVkPipelineRasterizationStateCreateInfo;
  MULTISAMPLE  :TVkPipelineMultisampleStateCreateInfo;

  SCREEN_RECT:TVkRect2D;
  SCREEN_SIZE:TVkExtent2D;

  RT_COUNT  :Byte;
  DB_ENABLE :Boolean;
  PRIM_TYPE :Byte;
  PRIM_RESET:Byte;
  VP_COUNT  :Byte;
  PROVOKING :Byte;
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
  ntEventWrite,
  ntEventWriteEop,
  ntEventWriteEos,
  ntSubmitFlipEop,
  ntDmaData,
  ntWriteData,
  ntWaitRegMem,
  ntFastClear,
  ntResolve,
  ntClearDepth,
  ntDrawIndex2,
  ntDrawIndexAuto,
  ntDispatchDirect
 );

const
 R_IMG  =0;
 R_BUF  =1;
 R_HTILE=2;

type
 t_pm4_usage=record
  mem_usage:Integer;
  img_usage:s_image_usage;
 end;

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
  rclear    :Boolean;
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
  procedure insert(i:p_pm4_resource_instance);
  function  find_resource_instance(r:p_pm4_resource):p_pm4_resource_instance;
  function  find_image_resource_instance (const rkey:TvImageKey):p_pm4_resource_instance;
  function  find_buffer_resource_instance(addr:Pointer;size:DWORD):p_pm4_resource_instance;
  function  find_htile_resource_instance (addr:Pointer;size:DWORD):p_pm4_resource_instance;
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
  function  fetch_image_resource         (const rkey:TvImageKey):p_pm4_resource;
  function  find_buffer_resource         (addr:Pointer;size:DWORD):p_pm4_resource;
  function  fetch_buffer_resource        (addr:Pointer;size:DWORD):p_pm4_resource;
  function  find_htile_resource          (addr:Pointer;size:DWORD):p_pm4_resource;
  function  fetch_htile_resource         (const rkey:TvImageKey;size:DWORD):p_pm4_resource;
  function  fetch_resource_instance      (scope:p_pm4_resource_curr_scope;r:p_pm4_resource;mem_usage:Integer;img_usage:s_image_usage):p_pm4_resource_instance;
  function  insert_image_resource        (scope:p_pm4_resource_curr_scope;const rkey:TvImageKey;mem_usage:Integer;img_usage:s_image_usage):p_pm4_resource_instance;
  function  insert_buffer_resource       (scope:p_pm4_resource_curr_scope;addr:Pointer;size:DWORD;mem_usage:Integer):p_pm4_resource_instance;
  function  insert_htile_resource        (scope:p_pm4_resource_curr_scope;const rkey:TvImageKey;size:DWORD;mem_usage:Integer):p_pm4_resource_instance;
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

 p_pm4_node_EventWrite=^t_pm4_node_EventWrite;
 t_pm4_node_EventWrite=packed object(t_pm4_node)
  eventType:Byte;
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
  CX_REG:TCONTEXT_REG_GROUP; // 0xA000
 end;

 p_pm4_node_draw=^t_pm4_node_draw;
 t_pm4_node_draw=object(t_pm4_node)
  rt_info:t_pm4_rt_info;

  indexBase   :QWORD;
  indexOffset :DWORD;
  indexCount  :DWORD;
  numInstances:DWORD;

  INDEX_TYPE:Byte;
  SWAP_MODE :Byte;
 end;

 p_pm4_node_DispatchDirect=^t_pm4_node_DispatchDirect;
 t_pm4_node_DispatchDirect=object(t_pm4_node)

  USER_DATA_CS:TSPI_USER_DATA;

  ShaderGroup:TvShaderGroup;

  DIM_X:DWORD;
  DIM_Y:DWORD;
  DIM_Z:DWORD;

  //SH_REG:TSH_REG_GROUP;         // 0x2C00
 end;

 p_pm4_stream=^t_pm4_stream;
 t_pm4_stream=object(t_pm4_resource_stream_scope)
  //
  list:TAILQ_HEAD; //t_pm4_node
  //
  buft:t_pm4_stream_type;
  //
  init:Boolean;
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
  procedure Hint         (P1,P2:PChar);
  procedure LoadConstRam (addr:Pointer;num_dw,offset:Word);
  procedure EventWrite   (eventType:Byte);
  procedure EventWriteEop(addr:Pointer;data:QWORD;eventType,dataSel,intSel:Byte);
  procedure EventWriteEos(addr:Pointer;data:DWORD;eventType,command:Byte);
  procedure SubmitFlipEop(eop_value:QWORD;intSel:Byte);
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
                          var GPU_REGS:TGPU_REGS);
  procedure BuildDraw    (ntype:t_pm4_node_type;
                          var SH_REG:TSH_REG_GROUP;
                          var CX_REG:TCONTEXT_REG_GROUP;
                          var UC_REG:TUSERCONFIG_REG_SHORT);
  procedure DrawIndex2   (var SH_REG:TSH_REG_GROUP;
                          var CX_REG:TCONTEXT_REG_GROUP;
                          var UC_REG:TUSERCONFIG_REG_SHORT);
  procedure DrawIndexAuto(var SH_REG:TSH_REG_GROUP;
                          var CX_REG:TCONTEXT_REG_GROUP;
                          var UC_REG:TUSERCONFIG_REG_SHORT);
  procedure Build_cs_info (node:p_pm4_node_DispatchDirect;var GPU_REGS:TGPU_REGS);
  procedure DispatchDirect(var SH_REG:TSH_REG_GROUP);
 end;

implementation

uses
 sys_bootparam;

var
 cache_block_allocator:t_cache_block_allocator;

//

operator + (a,b:t_pm4_usage):t_pm4_usage;
begin
 Result.mem_usage:=a.mem_usage or b.mem_usage;
 Result.img_usage:=b.img_usage +  b.img_usage;
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
  R_HTILE:
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

 if (list.tqh_first=nil) and
    (list.tqh_last =nil) then
 begin
  TAILQ_INIT(@list);
 end;

 TAILQ_INSERT_TAIL(@list,i,@i^.init_entry);

 i^.init_scope:=@self;
end;

//

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

 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_IMG;
 tmp.rkey :=rkey;

 Result:=find_resource_instance(@tmp);
end;

function t_pm4_resource_curr_scope.find_buffer_resource_instance(addr:Pointer;size:DWORD):p_pm4_resource_instance;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_BUF;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=find_resource_instance(@tmp);
end;

function t_pm4_resource_curr_scope.find_htile_resource_instance(addr:Pointer;size:DWORD):p_pm4_resource_instance;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_HTILE;
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

function t_pm4_resource_stream_scope.fetch_image_resource(const rkey:TvImageKey):p_pm4_resource;
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

  Result:=allocator.Alloc(SizeOf(t_pm4_resource));
  Result^:=tmp;

  resource_set.Insert(Result);
 end;
end;

function t_pm4_resource_stream_scope.find_buffer_resource(addr:Pointer;size:DWORD):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_BUF;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=resource_set.Find(@tmp);
end;

function t_pm4_resource_stream_scope.fetch_buffer_resource(addr:Pointer;size:DWORD):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_BUF;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=resource_set.Find(@tmp);

 if (Result=nil) then
 begin
  Result:=allocator.Alloc(SizeOf(t_pm4_resource));
  Result^:=tmp;

  resource_set.Insert(Result);
 end;
end;

function t_pm4_resource_stream_scope.find_htile_resource(addr:Pointer;size:DWORD):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_HTILE;
 tmp.rkey.Addr:=addr;
 tmp.rsize:=size;

 Result:=resource_set.Find(@tmp);
end;

function t_pm4_resource_stream_scope.fetch_htile_resource(const rkey:TvImageKey;size:DWORD):p_pm4_resource;
var
 tmp:t_pm4_resource;
begin
 tmp:=Default(t_pm4_resource);
 tmp.rtype:=R_HTILE;
 tmp.rkey :=rkey;
 tmp.rsize:=size;

 Result:=resource_set.Find(@tmp);

 if (Result=nil) then
 begin
  Result:=allocator.Alloc(SizeOf(t_pm4_resource));
  Result^:=tmp;

  resource_set.Insert(Result);
 end;
end;

function t_pm4_resource_stream_scope.fetch_resource_instance(scope:p_pm4_resource_curr_scope;r:p_pm4_resource;mem_usage:Integer;img_usage:s_image_usage):p_pm4_resource_instance;
begin
 Result:=scope^.find_resource_instance(r);

 if (Result=nil) then
 begin
  Result:=allocator.Alloc(SizeOf(t_pm4_resource_instance));
  Result^:=Default(t_pm4_resource_instance);
  //
  Result^.resource:=r;
  Result^.curr.mem_usage:=mem_usage;
  Result^.curr.img_usage:=img_usage;
 end else
 begin
  Result^.curr.mem_usage:=Result^.curr.mem_usage or mem_usage;
  Result^.curr.img_usage:=Result^.curr.img_usage +  img_usage;
 end;

end;

function t_pm4_resource_stream_scope.insert_image_resource(scope:p_pm4_resource_curr_scope;const rkey:TvImageKey;mem_usage:Integer;img_usage:s_image_usage):p_pm4_resource_instance;
var
 r:p_pm4_resource;
 i:p_pm4_resource_instance;
begin
 if (rkey.cformat=VK_FORMAT_UNDEFINED) then Exit;

 r:=fetch_image_resource(rkey);
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

function t_pm4_resource_stream_scope.insert_buffer_resource(scope:p_pm4_resource_curr_scope;addr:Pointer;size:DWORD;mem_usage:Integer):p_pm4_resource_instance;
var
 r:p_pm4_resource;
 i:p_pm4_resource_instance;
begin
 r:=fetch_buffer_resource(addr,size);
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

function t_pm4_resource_stream_scope.insert_htile_resource(scope:p_pm4_resource_curr_scope;const rkey:TvImageKey;size:DWORD;mem_usage:Integer):p_pm4_resource_instance;
var
 r:p_pm4_resource;
 i:p_pm4_resource_instance;
begin
 r:=fetch_htile_resource(rkey,size);
 i:=fetch_resource_instance(scope,r,mem_usage,[iu_htile]); //iu_htile

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

Procedure t_pm4_stream.add_node(node:p_pm4_node);
begin
 if (list.tqh_first=nil) and
    (list.tqh_last =nil) then
 begin
  TAILQ_INIT(@list);
 end;

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

procedure t_pm4_stream.Hint(P1,P2:PChar);
var
 len1,len2:ptruint;
 node:p_pm4_node_Hint;
begin
 len1:=StrLen(P1);
 len2:=StrLen(P2);
 node:=allocator.Alloc(SizeOf(t_pm4_node_Hint)+len1+len2+1);

 node^.ntype :=ntHint;
 node^.scope :=Default(t_pm4_resource_curr_scope);

 Move(P1^,node^.data,len1);
 Move(P2^,PChar(@node^.data)[len1],len2+1);

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

procedure t_pm4_stream.EventWriteEop(addr:Pointer;data:QWORD;eventType,dataSel,intSel:Byte);
var
 node:p_pm4_node_EventWriteEop;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWriteEop));

 node^.ntype    :=ntEventWriteEop;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.addr     :=addr;
 node^.data     :=data;
 node^.eventType:=eventType;
 node^.dataSel  :=dataSel;
 node^.intSel   :=intSel;

 add_node(node);
end;

procedure t_pm4_stream.EventWriteEos(addr:Pointer;data:DWORD;eventType,command:Byte);
var
 node:p_pm4_node_EventWriteEos;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWriteEos));

 node^.ntype    :=ntEventWriteEos;
 node^.scope    :=Default(t_pm4_resource_curr_scope);
 node^.addr     :=addr;
 node^.data     :=data;
 node^.eventType:=eventType;
 node^.command  :=command;

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
 RT:=GPU_REGS.GET_RT_INFO(0);

 {
 //clear TM_READ
 RT.IMAGE_USAGE:=RT.IMAGE_USAGE and (not TM_READ);
 //set   TM_CLEAR
 RT.IMAGE_USAGE:=RT.IMAGE_USAGE or TM_CLEAR;

 //

 insert_image_resource(@node^.scope,
                       RT.FImageInfo,
                       RT.IMAGE_USAGE,
                       [iu_attachment]);

 }

 //
 node^.RT:=RT;
 //

 add_node(node);
end;

procedure t_pm4_stream.Resolve(var CX_REG:TCONTEXT_REG_GROUP);
var
 node:p_pm4_node_Resolve;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_Resolve));

 node^.ntype :=ntResolve;
 node^.scope :=Default(t_pm4_resource_curr_scope);
 node^.CX_REG:=CX_REG;

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

   insert_image_resource(@node^.scope,
                         FImage,
                         TM_READ,
                         [iu_sampled]);

  end;
 end;
 //images

 //buffers
 if (Length(UniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(UniformBuilder.FBuffers) do
  With UniformBuilder.FBuffers[i] do
  begin

   insert_buffer_resource(@node^.scope,
                          addr,
                          size,
                          memuse);

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
                          addr,
                          Shader.FPushConst.size,
                          TM_READ);

  end;
 end;
end;

procedure t_pm4_stream.Build_rt_info(node:p_pm4_node;
                                     var rt_info:t_pm4_rt_info;
                                     var GPU_REGS:TGPU_REGS);
var
 i:Integer;
 RT:TRT_INFO;
 FUniformBuilder:TvUniformBuilder;

 pa:TPushConstAllocator;
 pp:PPushConstAllocator;

 resource_instance:p_pm4_resource_instance;
begin
 for i:=0 to 31 do
 begin
  Assert(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FP16_INTERP_MODE=0,'SPI_PS_INPUT_CNTL['+IntToStr(i)+'].FP16_INTERP_MODE='+IntToStr(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FP16_INTERP_MODE));
 end;

 GPU_REGS.export_user_data_rt(@rt_info.USERDATA);

 rt_info.RT_COUNT:=0;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to GPU_REGS.GET_HI_RT do
  begin
   RT:=GPU_REGS.GET_RT_INFO(i);

   //

   insert_image_resource(@node^.scope,
                         RT.FImageInfo,
                         RT.IMAGE_USAGE,
                         [iu_attachment]);

   //

   rt_info.RT_INFO[rt_info.RT_COUNT]:=RT;

   Inc(rt_info.RT_COUNT);
  end;

 rt_info.DB_ENABLE:=GPU_REGS.DB_ENABLE;

 if rt_info.DB_ENABLE then
 begin
  rt_info.DB_INFO:=GPU_REGS.GET_DB_INFO;

  //

  insert_image_resource(@node^.scope,
                        GetDepthOnly(rt_info.DB_INFO.FImageInfo),
                        rt_info.DB_INFO.DEPTH_USAGE,
                        [iu_depthstenc]);

  insert_image_resource(@node^.scope,
                        GetStencilOnly(rt_info.DB_INFO.FImageInfo),
                        rt_info.DB_INFO.STENCIL_USAGE,
                        [iu_depthstenc]);

  if (rt_info.DB_INFO.HTILE_INFO.TILE_SURFACE_ENABLE<>0) then
  begin
   resource_instance:=insert_htile_resource(@node^.scope,
                                            rt_info.DB_INFO.HTILE_INFO.KEY,
                                            rt_info.DB_INFO.HTILE_INFO.SIZE,
                                            rt_info.DB_INFO.DEPTH_USAGE);
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

 //

 pa.Init;
 pp:=@pa;

 rt_info.ShaderGroup:=FetchShaderGroupRT(GPU_REGS,pp);
 Assert(rt_info.ShaderGroup<>nil);

 //

 FUniformBuilder:=Default(TvUniformBuilder);
 rt_info.ShaderGroup.ExportUnifBuilder(FUniformBuilder,@rt_info.USERDATA);

 Init_Uniforms(node,FUniformBuilder);
end;

function IsClearDepthShaders(const FShaders:AvShaderStage):Boolean; inline;
begin
 Result:=False;

 if (FShaders[vShaderStageLs]=nil) and
    (FShaders[vShaderStageHs]=nil) and
    (FShaders[vShaderStageEs]=nil) and
    (FShaders[vShaderStageGs]=nil) and
    (FShaders[vShaderStageVs]<>nil) and
    (FShaders[vShaderStagePs]<>nil) and
    (FShaders[vShaderStageCs]=nil) then

 if (FShaders[vShaderStageVs].FHash_gcn=QWORD($00DF6E6331449451)) and
    (FShaders[vShaderStagePs].FHash_gcn=QWORD($E9FF5D4699E5B9AD)) then
 begin
  Result:=True;
 end;
end;

procedure t_pm4_stream.BuildDraw(ntype:t_pm4_node_type;
                                 var SH_REG:TSH_REG_GROUP;
                                 var CX_REG:TCONTEXT_REG_GROUP;
                                 var UC_REG:TUSERCONFIG_REG_SHORT);
var
 GPU_REGS:TGPU_REGS;

 node:p_pm4_node_draw;

begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SH_REG:=@SH_REG;
 GPU_REGS.CX_REG:=@CX_REG;
 GPU_REGS.UC_REG:=@UC_REG;

 node:=allocator.Alloc(SizeOf(t_pm4_node_draw));

 node^.ntype :=ntype;
 node^.scope :=Default(t_pm4_resource_curr_scope);

 Build_rt_info(node,node^.rt_info,GPU_REGS);

 node^.indexBase   :=CX_REG.VGT_DMA_BASE or (QWORD(CX_REG.VGT_DMA_BASE_HI.BASE_ADDR) shl 32);
 node^.indexOffset :=CX_REG.VGT_INDX_OFFSET;
 node^.indexCount  :=UC_REG.VGT_NUM_INDICES;
 node^.numInstances:=UC_REG.VGT_NUM_INSTANCES;

 node^.INDEX_TYPE:=ord(GPU_REGS.GET_INDEX_TYPE);
 node^.SWAP_MODE :=CX_REG.VGT_DMA_INDEX_TYPE.SWAP_MODE;

 //heuristic
 if (ntype=ntDrawIndexAuto) and
    (node^.numInstances<=1) and
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

 add_node(node);
end;

procedure t_pm4_stream.DrawIndex2(var SH_REG:TSH_REG_GROUP;
                                  var CX_REG:TCONTEXT_REG_GROUP;
                                  var UC_REG:TUSERCONFIG_REG_SHORT);
begin
 if ColorControl(CX_REG) then Exit;

 BuildDraw(ntDrawIndex2,SH_REG,CX_REG,UC_REG);
end;

procedure t_pm4_stream.DrawIndexAuto(var SH_REG:TSH_REG_GROUP;
                                     var CX_REG:TCONTEXT_REG_GROUP;
                                     var UC_REG:TUSERCONFIG_REG_SHORT);
begin
 if ColorControl(CX_REG) then Exit;

 BuildDraw(ntDrawIndexAuto,SH_REG,CX_REG,UC_REG);
end;

procedure t_pm4_stream.Build_cs_info(node:p_pm4_node_DispatchDirect;var GPU_REGS:TGPU_REGS);
var
 dst:PGPU_USERDATA;
 FUniformBuilder:TvUniformBuilder;

 pa:TPushConstAllocator;
 pp:PPushConstAllocator;
begin
 //hack
 dst:=Pointer(@node^.USER_DATA_CS)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 GPU_REGS.export_user_data_cs(dst);

 pa.Init;
 pp:=@pa;

 node^.ShaderGroup:=FetchShaderGroupCS(GPU_REGS,pp);
 Assert(node^.ShaderGroup<>nil);

 node^.DIM_X:=GPU_REGS.SH_REG^.COMPUTE_DIM_X;
 node^.DIM_Y:=GPU_REGS.SH_REG^.COMPUTE_DIM_Y;
 node^.DIM_Z:=GPU_REGS.SH_REG^.COMPUTE_DIM_Z;

 //

 FUniformBuilder:=Default(TvUniformBuilder);
 node^.ShaderGroup.ExportUnifBuilder(FUniformBuilder,dst);

 Init_Uniforms(node,FUniformBuilder);
end;

procedure t_pm4_stream.DispatchDirect(var SH_REG:TSH_REG_GROUP);
var
 GPU_REGS:TGPU_REGS;

 node:p_pm4_node_DispatchDirect;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SH_REG:=@SH_REG;

 node:=allocator.Alloc(SizeOf(t_pm4_node_DispatchDirect));

 node^.ntype:=ntDispatchDirect;
 node^.scope:=Default(t_pm4_resource_curr_scope);

 Build_cs_info(node,GPU_REGS);

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
 md_mmap(Result,mem_size,VM_RW);
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
 md_unmap(node,mem_size);
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
   node:=nil;
   md_mmap(node,mem_size,VM_RW);
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
   md_unmap(node,node^.size);
  end;

  node:=SLIST_FIRST(@pHead);
 end;
 Self:=Default(t_pm4_allocator);
end;


initialization
 cache_block_allocator.Init;

end.

