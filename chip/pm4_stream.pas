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
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups,

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
 vPipeline,
 vSetsPoolManager,
 vSampler,
 vSamplerManager,

 shader_dump
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
  ntDrawIndex2,
  ntDrawIndexAuto,
  ntDispatchDirect
 );

 p_pm4_node=^t_pm4_node;
 t_pm4_node=object
  entry:TAILQ_ENTRY;
  ntype:t_pm4_node_type;
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
  dst   :QWORD;
  src   :Pointer;
  num_dw:Word;
  dstSel:Byte;
 end;

 p_pm4_node_WaitRegMem=^t_pm4_node_WaitRegMem;
 t_pm4_node_WaitRegMem=packed object(t_pm4_node)
  pollAddr    :QWORD;
  refValue    :DWORD;
  mask        :DWORD;
  compareFunc :Byte;
 end;

 p_pm4_node_FastClear=^t_pm4_node_FastClear;
 t_pm4_node_FastClear=object(t_pm4_node)
  CX_REG:TCONTEXT_REG_GROUP; // 0xA000
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
 t_pm4_stream=object
  next_:Pointer;
  //
  allocator:t_pm4_allocator;
  //
  list:TAILQ_HEAD; //t_pm4_node
  //
  buft:t_pm4_stream_type;
  //
  procedure Free;
  Procedure add_node(node:p_pm4_node);
  function  First:p_pm4_node;
  function  Next(node:p_pm4_node):p_pm4_node; static;
  //
  procedure LoadConstRam (addr:Pointer;num_dw,offset:Word);
  procedure EventWrite   (eventType:Byte);
  procedure EventWriteEop(addr:Pointer;data:QWORD;eventType,dataSel,intSel:Byte);
  procedure EventWriteEos(addr:Pointer;data:DWORD;eventType,command:Byte);
  procedure SubmitFlipEop(eop_value:QWORD;intSel:Byte);
  procedure DmaData      (dstSel:Byte;dst:QWORD;srcSel:Byte;srcOrData:QWORD;numBytes:DWORD;isBlocking:Byte);
  procedure WriteData    (dstSel:Byte;dst:QWORD;src:Pointer;num_dw:Word);
  procedure WaitRegMem   (pollAddr:QWORD;refValue,mask:DWORD;compareFunc:Byte);
  procedure FastClear    (var CX_REG:TCONTEXT_REG_GROUP);
  procedure Resolve      (var CX_REG:TCONTEXT_REG_GROUP);
  function  ColorControl (var CX_REG:TCONTEXT_REG_GROUP):Boolean;
  procedure Build_rt_info(var rt_info:t_pm4_rt_info;var GPU_REGS:TGPU_REGS);
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

var
 cache_block_allocator:t_cache_block_allocator;

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
end;

function t_pm4_stream.First:p_pm4_node;
begin
 Result:=TAILQ_FIRST(@list);
end;

function t_pm4_stream.Next(node:p_pm4_node):p_pm4_node;
begin
 Result:=TAILQ_NEXT(node,@node^.entry);
end;

procedure t_pm4_stream.LoadConstRam(addr:Pointer;num_dw,offset:Word);
var
 node:p_pm4_node_LoadConstRam;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_LoadConstRam));

 node^.ntype :=ntLoadConstRam;
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
 node^.eventType:=eventType;

 add_node(node);
end;

procedure t_pm4_stream.EventWriteEop(addr:Pointer;data:QWORD;eventType,dataSel,intSel:Byte);
var
 node:p_pm4_node_EventWriteEop;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_EventWriteEop));

 node^.ntype    :=ntEventWriteEop;
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
 node^.dst     :=dst;
 node^.src     :=srcOrData;
 node^.numBytes:=numBytes;
 node^.srcSel  :=srcSel;
 node^.dstSel  :=dstSel;
 node^.cpSync  :=isBlocking;

 add_node(node);
end;

procedure t_pm4_stream.WriteData(dstSel:Byte;dst:QWORD;src:Pointer;num_dw:Word);
var
 node:p_pm4_node_WriteData;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_WriteData)+num_dw*SizeOf(DWORD));

 node^.ntype :=ntWriteData;
 node^.dst   :=dst;
 node^.src   :=Pointer(node+1);
 node^.num_dw:=num_dw;
 node^.dstSel:=dstSel;

 Move(src^,node^.src^,num_dw*SizeOf(DWORD));

 add_node(node);
end;

procedure t_pm4_stream.WaitRegMem(pollAddr:QWORD;refValue,mask:DWORD;compareFunc:Byte);
var
 node:p_pm4_node_WaitRegMem;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_WaitRegMem));

 node^.ntype      :=ntWaitRegMem;
 node^.pollAddr   :=pollAddr;
 node^.refValue   :=refValue;
 node^.mask       :=mask;
 node^.compareFunc:=compareFunc;

 add_node(node);
end;

procedure t_pm4_stream.FastClear(var CX_REG:TCONTEXT_REG_GROUP);
var
 node:p_pm4_node_FastClear;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_FastClear));

 node^.ntype :=ntFastClear;
 node^.CX_REG:=CX_REG;

 add_node(node);
end;

procedure t_pm4_stream.Resolve(var CX_REG:TCONTEXT_REG_GROUP);
var
 node:p_pm4_node_Resolve;
begin
 node:=allocator.Alloc(SizeOf(t_pm4_node_Resolve));

 node^.ntype :=ntResolve;
 node^.CX_REG:=CX_REG;

 add_node(node);
end;

function t_pm4_stream.ColorControl(var CX_REG:TCONTEXT_REG_GROUP):Boolean;
begin
 Result:=False;

 case CX_REG.CB_COLOR_CONTROL.MODE of
  CB_DISABLE             :Writeln('DISABLE');
  CB_NORMAL              :;
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
  CB_DECOMPRESS          :Writeln('DECOMPRESS');
  CB_FMASK_DECOMPRESS    :Writeln('FMASK_DECOMPRESS'); // Fmask decompression for shader readability.
  CB_DCC_DECOMPRESS      :Writeln('DCC_DECOMPRESS');   // Indicates this color target view is for a DCC decompress
  else
   Assert(False,'unknow color control:0x'+HexStr(CX_REG.CB_COLOR_CONTROL.MODE,1));
 end;

end;

procedure t_pm4_stream.Build_rt_info(var rt_info:t_pm4_rt_info;var GPU_REGS:TGPU_REGS);
var
 i:Integer;
begin
 for i:=0 to 31 do
 begin
  if (GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].OFFSET<>0) and (GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].OFFSET<>i) then
  begin
   Assert(false,                                         'SPI_PS_INPUT_CNTL['+IntToStr(i)+'].OFFSET='          +IntToStr(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].OFFSET          ));
  end;
  Assert(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].DEFAULT_VAL     =0,'SPI_PS_INPUT_CNTL['+IntToStr(i)+'].DEFAULT_VAL='     +IntToStr(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].DEFAULT_VAL     ));
  Assert(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FLAT_SHADE      =0,'SPI_PS_INPUT_CNTL['+IntToStr(i)+'].FLAT_SHADE='      +IntToStr(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FLAT_SHADE      ));
  Assert(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FP16_INTERP_MODE=0,'SPI_PS_INPUT_CNTL['+IntToStr(i)+'].FP16_INTERP_MODE='+IntToStr(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FP16_INTERP_MODE));
 end;

 GPU_REGS.export_user_data_rt(@rt_info.USERDATA);

 rt_info.ShaderGroup:=FetchShaderGroupRT(GPU_REGS,nil{@pa});
 Assert(rt_info.ShaderGroup<>nil);

 rt_info.RT_COUNT:=0;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to GPU_REGS.GET_HI_RT do
  begin
   rt_info.RT_INFO[rt_info.RT_COUNT]:=GPU_REGS.GET_RT_INFO(i);

   Inc(rt_info.RT_COUNT);
  end;

 rt_info.DB_ENABLE:=GPU_REGS.DB_ENABLE;

 if rt_info.DB_ENABLE then
 begin
  rt_info.DB_INFO:=GPU_REGS.GET_DB_INFO;
 end;

 rt_info.BLEND_INFO:=GPU_REGS.GET_BLEND_INFO;

 rt_info.PRIM_TYPE :=ord(GPU_REGS.GET_PRIM_TYPE);
 rt_info.PRIM_RESET:=GPU_REGS.GET_PRIM_RESET;

 rt_info.VP_COUNT:=0;

 For i:=0 to 15 do
  if GPU_REGS.VP_ENABLE(i) then
  begin
   rt_info.VPORT  [rt_info.VP_COUNT]:=GPU_REGS.GET_VPORT(i);
   rt_info.SCISSOR[rt_info.VP_COUNT]:=GPU_REGS.GET_SCISSOR(i) ;

   Inc(rt_info.VP_COUNT);
  end;

 rt_info.RASTERIZATION:=GPU_REGS.GET_RASTERIZATION;
 rt_info.MULTISAMPLE  :=GPU_REGS.GET_MULTISAMPLE;

 rt_info.PROVOKING:=ord(GPU_REGS.GET_PROVOKING);

 rt_info.SCREEN_RECT:=GPU_REGS.GET_SCREEN;
 rt_info.SCREEN_SIZE:=GPU_REGS.GET_SCREEN_SIZE;

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

 Build_rt_info(node^.rt_info,GPU_REGS);

 node^.indexBase   :=CX_REG.VGT_DMA_BASE or (QWORD(CX_REG.VGT_DMA_BASE_HI.BASE_ADDR) shl 32);
 node^.indexOffset :=CX_REG.VGT_INDX_OFFSET;
 node^.indexCount  :=UC_REG.VGT_NUM_INDICES;
 node^.numInstances:=UC_REG.VGT_NUM_INSTANCES;

 node^.INDEX_TYPE:=ord(GPU_REGS.GET_INDEX_TYPE);
 node^.SWAP_MODE :=CX_REG.VGT_DMA_INDEX_TYPE.SWAP_MODE;

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
begin
 //hack
 dst:=Pointer(@node^.USER_DATA_CS)-Ptruint(@TGPU_USERDATA(nil^).A[vShaderStageCs]);

 GPU_REGS.export_user_data_cs(dst);

 node^.ShaderGroup:=FetchShaderGroupCS(GPU_REGS,nil{@pa});
 Assert(node^.ShaderGroup<>nil);

 node^.DIM_X:=GPU_REGS.SH_REG^.COMPUTE_DIM_X;
 node^.DIM_Y:=GPU_REGS.SH_REG^.COMPUTE_DIM_Y;
 node^.DIM_Z:=GPU_REGS.SH_REG^.COMPUTE_DIM_Z;
end;

procedure t_pm4_stream.DispatchDirect(var SH_REG:TSH_REG_GROUP);
var
 GPU_REGS:TGPU_REGS;

 node:p_pm4_node_DispatchDirect;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SH_REG:=@SH_REG;

 node:=allocator.Alloc(SizeOf(t_pm4_node_DispatchDirect));

 Build_cs_info(node,GPU_REGS);

 node^.ntype :=ntDispatchDirect;

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
 node:=pHead.slh_first;
 if (node<>nil) then
 begin
  pHead.slh_first:=node^.link;
 end;
 While (node<>nil) do
 begin

  if (node^.size=cache_block_allocator.mem_size) then
  begin
   cache_block_allocator.Free(node);
  end else
  begin
   md_unmap(node,node^.size);
  end;

  node:=pHead.slh_first;
  if (node<>nil) then
  begin
   pHead.slh_first:=node^.link;
  end;
 end;
 Self:=Default(t_pm4_allocator);
end;


initialization
 cache_block_allocator.Init;

end.

