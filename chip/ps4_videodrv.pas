unit ps4_videodrv;

{$mode objfpc}{$H+}

{/$define ww}
{/$define null_rt}

interface

uses
  Classes,
  SysUtils,
  LFQueue,
  bittype,

  sys_types,
  sys_kernel,
  ps4_libSceVideoOut,
  ps4_pssl,
  ps4_shader,
  pm4defs,

  //ps4_Tiling,

  vulkan,
  vDevice,
  vMemory,
  vShader,
  vPipeline,
  vImage,
  vRender,
  vCmdBuffer,

  vShaderExt,
  vShaderManager,
  vPipelineLayoutManager,
  vSetsPoolManager,
  vHostBufferManager,
  vImageManager,
  vSampler,
  vSamplerManager,

  vRenderPassManager,

  si_ci_vi_merged_offset,
  si_ci_vi_merged_enum,
  si_ci_vi_merged_registers

  ;

type
 PvSubmitInfo=^TvSubmitInfo;
 TvSubmitInfo=packed record
  count:DWORD;
  dcbGpuAddrs:PPointer;
  dcbSizesInBytes:PDWORD;
  ccbGpuAddrs:PPointer;
  ccbSizesInBytes:PDWORD;
 end;

const
 kIndirectBufferMaximumSizeInBytes=$3FFFFC;

 function vSubmitCommandBuffers(
            Submit:PvSubmitInfo;
            Flip:PqcFlipInfo):Integer;

procedure vSubmitDone;

implementation

Uses
 ntapi,
 atomic,
 spinlock,
 ps4_libSceGnmDriver,
 ps4_gpu_regs,
 shader_dump;

type
 PvSubmitNode=^TvSubmitNode;
 TvSubmitNode=record
  next_:PvSubmitNode;
  //
  Submit:TvSubmitInfo;
  Flip:TqcFlipInfo;
 end;

 TvCmdRing=object
  Queue:TIntrusiveMPSCQueue;
  Current:PvSubmitNode;
  CmdBuffer:TvCmdBuffer;
  SetShCount:ptruint;
  SetCxCount:ptruint;
  LastSetReg:WORD;
  Procedure Init;
  Function  Next:Boolean;
  Procedure AllocCmdBuffer;
 end;

 pvMeFlipInfo=^TvMeFlipInfo;
 TvMeFlipInfo=record
  qcInfo:TqcFlipInfo;
  FlipLData:DWORD;
  FlipLabel:PDWORD;
  Interrupt:Boolean;
 end;

 pvMeEopInfo=^TvMeEopInfo;
 TvMeEopInfo=record
  adr:Pointer;
  data:QWORD;
  dataSel:Byte;
  Interrupt:Boolean;
 end;

 PvMeWaitMemInfo=^TvMeWaitMemInfo;
 TvMeWaitMemInfo=record
  adr:Pointer;
  ref:DWORD;
  mask:DWORD;
  cFunc:Byte;
 end;

 TvMicroEngineType=(metCmdBuffer,metFlip,metEop,metWaitMem);

 PvMicroEngineNode=^TvMicroEngineNode;
 TvMicroEngineNode=record
  next_:PvMicroEngineNode;
  //
  mode:TvMicroEngineType;

  Case Byte of
   0:(CmdBuffer:TvCmdBuffer);
   1:(FlipInfo:TvMeFlipInfo);
   2:(EopInfo:TvMeEopInfo);
   3:(WaitMem:TvMeWaitMemInfo);
 end;

 TvMicroEngine=object
  Queue:TIntrusiveMPSCQueue;
  Current:PvMicroEngineNode;
  Procedure Init;
  Function  Next:Boolean;
  Procedure PushCmd(var Cmd:TvCmdBuffer);
  Procedure PushFlip(var qcInfo:TqcFlipInfo;FlipLData:DWORD;FlipLabel:PDWORD;Interrupt:Boolean);
  Procedure PushEop(adr:Pointer;data:QWORD;dataSel:Byte;Interrupt:Boolean);
  Procedure PushWaitMem(adr:Pointer;ref,mask:DWORD;cFunc:Byte);
 end;

var
 _gfx_lazy_init:Integer=0;
 _gfx_handle:Thandle=0;

 GFXRing:TvCmdRing;
 GFXMicroEngine:TvMicroEngine;

 FIdleEvent:PRTLEvent=nil;

 GPU_REGS:TGPU_REGS;

 FCmdPool:TvCmdPool;

Procedure TvCmdRing.Init;
begin
 Queue.Create
end;

Function TvCmdRing.Next:Boolean;
begin
 Result:=False;

 if (Current<>nil) then
 begin
  FreeMem(Current);
  Current:=nil;
 end;
 LastSetReg:=0;

 Result:=Queue.Pop(Current);
end;

Procedure TvCmdRing.AllocCmdBuffer;
begin
 InitVulkan;
 if (FCmdPool=nil) then
 begin
  FCmdPool:=TvCmdPool.Create(VulkanApp.FGFamily);
 end;
 if (CmdBuffer=nil) then
 begin
  CmdBuffer:=TvCmdBuffer.Create(FCmdPool,RenderQueue);
 end;
end;

Procedure TvMicroEngine.Init;
begin
 Queue.Create;
end;

Function TvMicroEngine.Next:Boolean;
begin
 Result:=False;

 if (Current<>nil) then
 begin
  FreeMem(Current);
  Current:=nil;
 end;

 Result:=Queue.Pop(Current);
end;

Procedure TvMicroEngine.PushCmd(var Cmd:TvCmdBuffer);
var
 node:PvMicroEngineNode;
begin
 if (Cmd=nil) then Exit;

 node:=AllocMem(SizeOf(TvMicroEngineNode));
 if (node=nil) then Exit;

 node^.mode:=metCmdBuffer;
 node^.CmdBuffer:=Cmd;

 Queue.Push(node);

 Cmd:=nil;
end;

Procedure TvMicroEngine.PushFlip(var qcInfo:TqcFlipInfo;FlipLData:DWORD;FlipLabel:PDWORD;Interrupt:Boolean);
var
 node:PvMicroEngineNode;
begin
 node:=AllocMem(SizeOf(TvMicroEngineNode));
 if (node=nil) then Exit;

 node^.mode              :=metFlip;
 node^.FlipInfo.qcInfo   :=qcInfo;
 node^.FlipInfo.FlipLData:=FlipLData;
 node^.FlipInfo.FlipLabel:=FlipLabel;
 node^.FlipInfo.Interrupt:=Interrupt;

 Queue.Push(node);

 qcInfo:=Default(TqcFlipInfo);
end;

Procedure TvMicroEngine.PushEop(adr:Pointer;data:QWORD;dataSel:Byte;Interrupt:Boolean);
var
 node:PvMicroEngineNode;
begin
 node:=AllocMem(SizeOf(TvMicroEngineNode));
 if (node=nil) then Exit;

 node^.mode             :=metEop;
 node^.EopInfo.adr      :=adr;
 node^.EopInfo.data     :=data;
 node^.EopInfo.dataSel  :=dataSel;
 node^.EopInfo.Interrupt:=Interrupt;

 Queue.Push(node);
end;

Procedure TvMicroEngine.PushWaitMem(adr:Pointer;ref,mask:DWORD;cFunc:Byte);
var
 node:PvMicroEngineNode;
begin
 node:=AllocMem(SizeOf(TvMicroEngineNode));
 if (node=nil) then Exit;

 node^.mode         :=metWaitMem;
 node^.WaitMem.adr  :=adr;
 node^.WaitMem.ref  :=ref;
 node^.WaitMem.mask :=mask;
 node^.WaitMem.cFunc:=cFunc;

 Queue.Push(node);
end;

procedure gfx_cp_parser(node:PvSubmitInfo);          forward;
function  gfx_submit(CmdBuffer:TvCmdBuffer):Boolean; forward;
function  gfx_test(CmdBuffer:TvCmdBuffer):Boolean;   forward;

Function me_flip(node:pvMeFlipInfo):Boolean;
begin
 Result:=True;
 if (node=nil) then Exit;

 if (node^.FlipLabel<>nil) then
 begin
  node^.FlipLabel^:=node^.FlipLData;
 end;

 if (node^.qcInfo.hVideo<>0) then
 begin
  _qc_sceVideoOutSubmitFlip(@node^.qcInfo);
 end;

 if node^.Interrupt then
 begin
  post_event_eop;
 end;
end;

Function me_eop(node:pvMeEopInfo):Boolean;
begin
 Result:=True;
 if (node=nil) then Exit;

 Case node^.dataSel of
  EVENTWRITEEOP_DATA_SEL_DISCARD      :;//nop
  kEventWriteSource32BitsImmediate    :PDWORD(node^.adr)^:=PDWORD(@node^.data)^;
  kEventWriteSource64BitsImmediate    :PQWORD(node^.adr)^:=PQWORD(@node^.data)^;
  kEventWriteSourceGlobalClockCounter ,
  kEventWriteSourceGpuCoreClockCounter:PQWORD(node^.adr)^:=GetTickCount64*1000;
  else
   Assert(False);
 end;

 if (node^.Interrupt) then
 begin
  post_event_eop;
 end;
end;

Function me_test_mem(node:PvMeWaitMemInfo):Boolean;
var
 val,ref:DWORD;
begin
 val:=PDWORD(node^.adr)^ and node^.mask;
 ref:=node^.ref;
 Case node^.cFunc of
  WAIT_REG_MEM_FUNC_ALWAYS       :Result:=True;
  WAIT_REG_MEM_FUNC_LESS         :Result:=(val<ref);
  WAIT_REG_MEM_FUNC_LESS_EQUAL   :Result:=(val<=ref);
  WAIT_REG_MEM_FUNC_EQUAL        :Result:=(val=ref);
  WAIT_REG_MEM_FUNC_NOT_EQUAL    :Result:=(val<>ref);
  WAIT_REG_MEM_FUNC_GREATER_EQUAL:Result:=(val>ref);
  WAIT_REG_MEM_FUNC_GREATER      :Result:=(val>=ref);
  else
   Assert(false);
 end;
end;

Function me_node_test(node:PvMicroEngineNode):Boolean;
begin
 Result:=True;
 if (node=nil) then Exit;

 Case node^.mode of
  metCmdBuffer:
   begin
    Result:=gfx_test(node^.CmdBuffer);
   end;
  metFlip:Result:=True;
  metEop :Result:=True;
  metWaitMem:
   begin
    Result:=me_test_mem(@node^.WaitMem);
   end;
  else
   Assert(false);
 end;

end;

Function me_node_submit(node:PvMicroEngineNode):Boolean;
begin
 Result:=True;
 if (node=nil) then Exit;

 Case node^.mode of
  metCmdBuffer:
   begin
    Result:=gfx_submit(node^.CmdBuffer);
   end;
  metFlip:
   begin
    Result:=me_flip(@node^.FlipInfo);
   end;
  metEop:
   begin
    Result:=me_eop(@node^.EopInfo);
   end;
  metWaitMem:;
  else
   Assert(false);
 end;
end;

function GFX_thread(p:pointer):ptrint;
var
 time:Int64;
 work_do:Boolean;
begin
 Result:=0;
 repeat
  work_do:=False;

  if GFXRing.Next then
  begin
   gfx_cp_parser(@GFXRing.Current^.Submit);
   work_do:=True;
  end;

  if (GFXMicroEngine.Current<>nil) then
  begin
   if not me_node_test(GFXMicroEngine.Current) then
   begin
    time:=-100000;
    NtDelayExecution(True,@time);
    Continue;
   end;
   work_do:=True;
  end;

  if GFXMicroEngine.Next then
  begin
   me_node_submit(GFXMicroEngine.Current);
   work_do:=True;
  end;

  if not work_do then
  begin
   RTLEventSetEvent(FIdleEvent);
   time:=Int64(NT_INFINITE);
   NtDelayExecution(True,@time);
  end;

 until false;
end;

procedure Init_gfx;
var
 t:Thandle;
begin
 if XCHG(_gfx_lazy_init,1)=0 then
 begin

  GFXRing.Init;
  GFXMicroEngine.Init;

  FIdleEvent:=RTLEventCreate;
  RTLEventSetEvent(FIdleEvent);

  t:=BeginThread(@GFX_thread);

  _gfx_handle:=t;
 end else
 begin
  wait_until_equal(_gfx_handle,0);
 end;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function calc_submit_size(node:PvSubmitInfo;var dcbSize,ccbSize:DWORD):Integer;
var
 n:DWORD;
begin
 Result:=0;

 dcbSize:=0;
 ccbSize:=0;

 if (node=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 n:=0;
 While (n<node^.count) do
 begin

  if (node^.dcbGpuAddrs<>nil) and (node^.dcbSizesInBytes<>nil) then
  begin
   if (node^.dcbGpuAddrs[n]<>nil) and (node^.dcbSizesInBytes[n]<>0) then
   if (node^.dcbSizesInBytes[n]<>0) then
   begin
    if (node^.dcbSizesInBytes[n]>kIndirectBufferMaximumSizeInBytes) then
    begin
     Exit(SCE_KERNEL_ERROR_EINVAL);
    end;
    dcbSize:=dcbSize+node^.dcbSizesInBytes[n];
   end;
  end;

  if (node^.ccbGpuAddrs<>nil) and (node^.ccbSizesInBytes<>nil) then
  begin
   if (node^.ccbGpuAddrs[n]<>nil) and (node^.ccbSizesInBytes[n]<>0) then
   if (node^.ccbSizesInBytes[n]<>0) then
   begin
    if (node^.ccbSizesInBytes[n]>kIndirectBufferMaximumSizeInBytes) then
    begin
     Exit(SCE_KERNEL_ERROR_EINVAL);
    end;
    ccbSize:=ccbSize+node^.ccbSizesInBytes[n];
   end;
  end;

  Inc(n);
 end;
end;

function vSubmitCommandBuffers(
           Submit:PvSubmitInfo;
           Flip:PqcFlipInfo):Integer;
var
 node:PvSubmitNode;
 dcbSize:DWORD;
 ccbSize:DWORD;
 addr:Pointer;
 size:DWORD;
begin
 Result:=0;
 if (Submit=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 dcbSize:=0;
 ccbSize:=0;
 Result:=calc_submit_size(Submit,dcbSize,ccbSize);
 if (Result<>0) then Exit;

 if (dcbSize=0) and (ccbSize=0) then
 begin
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 //calc size

 size:=SizeOf(TvSubmitNode);

 if (Submit^.dcbGpuAddrs<>nil) and (Submit^.dcbSizesInBytes<>nil) then
 begin
  size:=size+Submit^.count*(SizeOf(Pointer)+SizeOf(DWORD));
 end;

 if (Submit^.ccbGpuAddrs<>nil) and (Submit^.ccbSizesInBytes<>nil) then
 begin
  size:=size+Submit^.count*(SizeOf(Pointer)+SizeOf(DWORD));
 end;

 //alloc

 node:=AllocMem(size);
 if (node=nil) then
 begin
  Exit(SCE_KERNEL_ERROR_ENOMEM);
 end;

 //distrib

 addr:=Pointer(node)+SizeOf(TvSubmitNode);

 if (Submit^.dcbGpuAddrs<>nil) and (Submit^.dcbSizesInBytes<>nil) then
 begin
  node^.Submit.dcbGpuAddrs:=addr;
  addr:=addr+Submit^.count*SizeOf(Pointer);
  node^.Submit.dcbSizesInBytes:=addr;
  addr:=addr+Submit^.count*SizeOf(DWORD);
  //copy
  Move(Submit^.dcbGpuAddrs^    ,node^.Submit.dcbGpuAddrs^    ,Submit^.count*SizeOf(Pointer));
  Move(Submit^.dcbSizesInBytes^,node^.Submit.dcbSizesInBytes^,Submit^.count*SizeOf(DWORD));
 end;

 if (Submit^.ccbGpuAddrs<>nil) and (Submit^.ccbSizesInBytes<>nil) then
 begin
  node^.Submit.ccbGpuAddrs:=addr;
  addr:=addr+Submit^.count*SizeOf(Pointer);
  node^.Submit.ccbSizesInBytes:=addr;
  addr:=addr+Submit^.count*SizeOf(DWORD);
  //copy
  Move(Submit^.ccbGpuAddrs^    ,node^.Submit.ccbGpuAddrs^    ,Submit^.count*SizeOf(Pointer));
  Move(Submit^.ccbSizesInBytes^,node^.Submit.ccbSizesInBytes^,Submit^.count*SizeOf(DWORD));
 end;

 node^.Submit.count:=Submit^.count;

 Init_gfx;

 if (Flip<>nil) then
 begin
  node^.Flip:=Flip^;
 end;

 RTLEventResetEvent(FIdleEvent);
 GFXRing.Queue.Push(node);
 NtQueueApcThread(_gfx_handle,@_apc_null,0,nil,0);
end;

procedure vSubmitDone;
begin
 if (FIdleEvent<>nil) then
 begin
  RTLEventWaitFor(FIdleEvent);
 end;
 //Sleep(100);
 //Device.WaitIdle;
end;


procedure onPrepareFlip();
begin
 GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
 GFXMicroEngine.PushFlip(GFXRing.Current^.Flip,0,nil,False);
end;

procedure onPrepareFlipLabel(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlip);
var
 adr:PDWORD;
begin
 QWORD(adr):=QWORD(Body^.ADDRES_LO) or (QWORD(Body^.ADDRES_HI) shl $20);
 {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA);{$endif}

 GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
 GFXMicroEngine.PushFlip(GFXRing.Current^.Flip,Body^.DATA,adr,False);
end;

procedure onPrepareFlipWithEopInterrupt(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlipWithEopInterrupt);
begin
 {$ifdef ww}writeln;{$endif}

 GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
 GFXMicroEngine.PushFlip(GFXRing.Current^.Flip,0,nil,True);
end;

procedure onPrepareFlipWithEopInterruptLabel(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4PrepareFlipWithEopInterrupt);
var
 adr:PDWORD;
begin
 QWORD(adr):=QWORD(Body^.ADDRES_LO) or (QWORD(Body^.ADDRES_HI) shl $20);
 {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA);{$endif}

 GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
 GFXMicroEngine.PushFlip(GFXRing.Current^.Flip,Body^.DATA,adr,True);
end;

procedure onEventWriteEop(pm4Hdr:PM4_TYPE_3_HEADER;Body:PEVENTWRITEEOP);
var
 adr:Pointer;
begin
 Assert(Body^.EVENT_CNTL.EVENT_INDEX=EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP);

 {$ifdef ww}
 Case Body^.EVENT_CNTL.EVENT_TYPE of
  kEopFlushCbDbCaches             :Writeln('kEopFlushCbDbCaches');
  kEopFlushAndInvalidateCbDbCaches:Writeln('kEopFlushAndInvalidateCbDbCaches');
  kEopCbDbReadsDone               :Writeln('kEopCbDbReadsDone');
  else
   Assert(false);
 end;
 {$endif}

 if ((Body^.DATA_CNTL.reserved6 and 1)<>0) then
 begin
  {$ifdef ww}Writeln('kEventWriteDestTcL2');{$endif}
 end else
 begin

  QWORD(adr):=QWORD(Body^.ADDRESS_LO) or (QWORD(Body^.DATA_CNTL.addressHi) shl $20);

  {$ifdef ww}
  Case Body^.DATA_CNTL.dataSel of
   kEventWriteSource32BitsImmediate    :Writeln('kEventWriteSource32BitsImmediate     adr:',HexStr(adr),' data:',Body^.DATA_LO);
   kEventWriteSource64BitsImmediate    :Writeln('kEventWriteSource64BitsImmediate     adr:',HexStr(adr),' data:',PQWORD(@Body^.DATA_LO)^);
   kEventWriteSourceGlobalClockCounter :Writeln('kEventWriteSourceGlobalClockCounter  adr:',HexStr(adr),' data:',GetTickCount64*1000);
   kEventWriteSourceGpuCoreClockCounter:Writeln('kEventWriteSourceGpuCoreClockCounter adr:',HexStr(adr),' data:',GetTickCount64*1000);
   else;
  end;
  {$endif}

  GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
  GFXMicroEngine.PushEop(adr,PQWORD(@Body^.DATA_LO)^,Body^.DATA_CNTL.dataSel,(Body^.DATA_CNTL.intSel<>0));

  {
  Case Body^.DATA_CNTL.dataSel of
   EVENTWRITEEOP_DATA_SEL_DISCARD:;//nop
   kEventWriteSource32BitsImmediate    :PDWORD(adr)^:=Body^.DATA_LO;
   kEventWriteSource64BitsImmediate    :PQWORD(adr)^:=PQWORD(@Body^.DATA_LO)^;
   kEventWriteSourceGlobalClockCounter ,
   kEventWriteSourceGpuCoreClockCounter:PQWORD(adr)^:=GetTickCount64*1000;
   else
    Assert(False);
  end;

  if (Body^.DATA_CNTL.intSel<>0) then
  begin
   {$ifdef ww}Writeln('Interrupt');{$endif}

   vSubmitDone;

   post_event_eop;
  end;
  }

 end;

end;

procedure onEventWriteEos(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDEVENTWRITEEOS);
var
 adr:PDWORD;
begin
 Assert(Body^.eventType=47);

 Case Body^.eventIndex of
  EVENT_WRITE_EOS_INDEX_CSDONE_PSDONE:
  begin
   Case Body^.command of
    //EVENT_WRITE_EOS_CMD_STORE_APPEND_COUNT_TO_MEMORY:;
    //EVENT_WRITE_EOS_CMD_STORE_GDS_DATA_TO_MEMORY    :;
    EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY  :
    begin
     QWORD(adr):=QWORD(Body^.addressLo) or (QWORD(Body^.addressHi) shl $20);
     {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',Body^.DATA){$endif};
     GFXRing.AllocCmdBuffer;
     GFXRing.CmdBuffer.writeAtEndOfShader(Body^.eventType,adr,Body^.DATA);
    end;
    else
     Assert(False);
   end;
  end;
  else
   Assert(False);
 end;

end;

procedure onEventWrite(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDEVENTWRITE);
begin
 {$ifdef ww}
 Case Body^.eventType of
  THREAD_TRACE_MARKER        :Writeln(' THREAD_TRACE_MARKER');
  FLUSH_AND_INV_CB_PIXEL_DATA:Writeln(' FLUSH_AND_INV_CB_PIXEL_DATA');
  FLUSH_AND_INV_CB_META      :Writeln(' FLUSH_AND_INV_CB_META');
  else
   Assert(False,IntToStr(Body^.eventType));
 end;

 Case Body^.EVENTINDEX of
  EVENT_WRITE_INDEX_ANY_NON_TIMESTAMP    :Writeln(' ANY_NON_TIMESTAMP');
  EVENT_WRITE_INDEX_ZPASS_DONE           :Writeln(' ZPASS_DONE');
  EVENT_WRITE_INDEX_SAMPLE_PIPELINESTAT  :Writeln(' SAMPLE_PIPELINESTATS');
  EVENT_WRITE_INDEX_SAMPLE_STREAMOUTSTATS:Writeln(' SAMPLE_STREAMOUTSTAT[S|S1|S2|S3]');
  EVENT_WRITE_INDEX_VS_PS_PARTIAL_FLUSH  :Writeln(' [CS|VS|PS]_PARTIAL_FLUSH');
  EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP    :Writeln(' ANY_EOP_TIMESTAMP');
  EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP    :Writeln(' ANY_EOS_TIMESTAMP');
  EVENT_WRITE_INDEX_CACHE_FLUSH_EVENT    :Writeln(' CACHE_FLUSH, CACHE_FLUSH_AND_INV_EVENT');
  else
   Assert(False);
 end;
 {$endif}
end;

procedure onDMAData(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4DMADATA);
var
 adrSrc,adrDst:PDWORD;
 srcSel,dstSel:DWORD;
begin
 srcSel:=((PDWORD(Body)[0] shr $1d) and 3) or ((PDWORD(Body)[5] shr $19) and 8) or ((PDWORD(Body)[5] shr $18) and 4);
 dstSel:=((PDWORD(Body)[0] shr $14) and 1) or ((PDWORD(Body)[5] shr $1a) and 8) or ((PDWORD(Body)[5] shr $19) and 4);

 QWORD(adrSrc):=QWORD(Body^.srcAddrLo) or (QWORD(Body^.srcAddrHi) shl $20);
 QWORD(adrDst):=QWORD(Body^.dstAddrLo) or (QWORD(Body^.dstAddrHi) shl $20);

 //Flags1.cpSync  isBlocking

 case srcSel of
  kDmaDataSrcMemory,
  kDmaDataSrcMemoryUsingL2:
   begin
    case dstSel of
     kDmaDataDstMemory:
      begin

       Case Body^.Flags1.engine of
        CP_DMA_ENGINE_ME:
         begin
          GFXRing.AllocCmdBuffer;
          GFXRing.CmdBuffer.dmaData(adrSrc,adrDst,Body^.Flags2.byteCount,Boolean(Body^.Flags1.cpSync));
          //Move(adrSrc^,adrDst^,Body^.Flags2.byteCount);
         end;
        CP_DMA_ENGINE_PFP:
         begin
          Move(adrSrc^,adrDst^,Body^.Flags2.byteCount);
         end;
       end;

      end;
     kDmaDataDstRegister,
     kDmaDataDstRegisterNoIncrement:
     begin
      if (Body^.dstAddrLo=$3022C) then
      begin
       {$ifdef ww}Writeln('prefetchIntoL2:',HexStr(adrSrc),' count(DW):',Body^.Flags2.byteCount div 4){$endif};
      end else
      begin
       {$ifdef ww}Writeln('SetRegister:',HexStr(Body^.dstAddrLo shr 2,4),' count(DW):',Body^.Flags2.byteCount div 4){$endif};
       Assert(false,'TODO');
      end;
     end;
     else
      Assert(False);
    end;
   end;
  kDmaDataSrcData:
   begin
    case dstSel of
     kDmaDataDstMemory:
      begin

       Case Body^.Flags1.engine of
        CP_DMA_ENGINE_ME:
         begin
          GFXRing.AllocCmdBuffer;
          GFXRing.CmdBuffer.dmaData(Body^.srcAddrLo,adrDst,Body^.Flags2.byteCount,Boolean(Body^.Flags1.cpSync));
          //FillDWORD(adrDst^,Body^.Flags2.byteCount div 4,Body^.srcAddrLo);
         end;
        CP_DMA_ENGINE_PFP:
         begin
          FillDWORD(adrDst^,Body^.Flags2.byteCount div 4,Body^.srcAddrLo);
         end;
       end;

      end;
     kDmaDataDstRegister,
     kDmaDataDstRegisterNoIncrement:
      {$ifdef ww}Writeln('SetRegister:',HexStr(Body^.dstAddrLo shr 2,4),' count(DW):1'){$endif};
     kDmaDataDstGds:
      {$ifdef ww}Writeln('SetGds'){$endif};
     else
      Assert(False);
    end;
   end;
  else
   Assert(False);
 end;
end;

procedure onWriteData(pm4Hdr:PM4_TYPE_3_HEADER;Body:PTPM4CMDWRITEDATA);
var
 adr:PDWORD;
 count:Word;
begin

 Assert(Body^.CONTROL.wrOneAddr=0);

 Case Body^.CONTROL.dstSel of
  WRITE_DATA_DST_SEL_MEMORY_SYNC,
  WRITE_DATA_DST_SEL_MEMORY_ASYNC:
    begin
     count:=pm4Hdr.count;
     if (count>=3) then
     begin
      count:=count-2;
      QWORD(adr):=QWORD(Body^.dstAddrLo) or (QWORD(Body^.dstAddrHi) shl $20);
      {$ifdef ww}Writeln('adr:',HexStr(adr),' data:',PDWORD(@Body^.DATA)^){$endif};

      Case Body^.CONTROL.engineSel of
       WRITE_DATA_ENGINE_ME:
        begin
         GFXRing.AllocCmdBuffer;
         GFXRing.CmdBuffer.dmaData(@Body^.DATA,adr,count*SizeOf(DWORD),Boolean(Body^.CONTROL.wrConfirm));
         //Move(Body^.DATA,adr^,count*SizeOf(DWORD));
        end;
       WRITE_DATA_ENGINE_PFP:
        begin
         Move(Body^.DATA,adr^,count*SizeOf(DWORD));
        end;
       else
        Assert(False);
      end;

     end;
    end;
  else
   Assert(False);
 end;

end;

//vkFlushMappedMemoryRanges analog
procedure onAcquireMem(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4ACQUIREMEM);
begin
 {$ifdef ww}writeln;{$endif}
end;

procedure onWaitRegMem(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDWAITREGMEM);
var
 adr:Pointer;
begin
  {$ifdef ww}
 Case Body^.engine of
  WAIT_REG_MEM_ENGINE_ME:
    Case Body^.memSpace of
     WAIT_REG_MEM_SPACE_REGISTER:Writeln(' waitOnRegister');
     WAIT_REG_MEM_SPACE_MEMORY  :Writeln(' waitOnAddress');
     else
      Assert(false);
    end;
  WAIT_REG_MEM_ENGINE_PFP:
    Case Body^.memSpace of
     WAIT_REG_MEM_SPACE_REGISTER:Writeln(' waitOnRegisterAndStall');
     WAIT_REG_MEM_SPACE_MEMORY  :Writeln(' waitOnAddressAndStall');
     else
      Assert(false);
    end;
  else
   Assert(false);
 end;
 {$endif}

 Case Body^.engine of
  WAIT_REG_MEM_ENGINE_ME:
    Case Body^.memSpace of
     WAIT_REG_MEM_SPACE_MEMORY:
       begin

        QWORD(adr):=QWORD(Body^.pollAddressLo) or (QWORD(Body^.pollAddressHi) shl $20);

        GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
        GFXMicroEngine.PushWaitMem(adr,Body^.reference,Body^.mask,Body^.compareFunc);

       end;
     else
      Assert(false);
    end;
  else
   Assert(false);
 end;

end;

procedure onPm40(pm4Hdr:PM4_TYPE_0_HEADER;Body:PDWORD);
begin
 {$ifdef ww}Writeln('PM4_TYPE_0:Reg:',HexStr(pm4Hdr.baseIndex,4),' count(DW):',pm4Hdr.count+1);{$endif}
end;

procedure onPushMarker(Body:PChar);
begin
 {$ifdef ww}Writeln('\HINT_PUSH_MARKER:',Body);{$endif}
end;

procedure onWidthHeight(Body:PWORD);
begin
 {$ifdef ww}Writeln('\HINT_',Body[0],'_',Body[1]);{$endif}
end;

procedure onNop(pm4Hdr:PM4_TYPE_3_HEADER;Body:PDWORD);
begin

 Case GFXRing.LastSetReg of
  mmCB_COLOR0_FMASK_SLICE,
  mmCB_COLOR1_FMASK_SLICE,
  mmCB_COLOR2_FMASK_SLICE,
  mmCB_COLOR3_FMASK_SLICE,
  mmCB_COLOR4_FMASK_SLICE,
  mmCB_COLOR5_FMASK_SLICE,
  mmCB_COLOR6_FMASK_SLICE,
  mmCB_COLOR7_FMASK_SLICE,

  mmCB_COLOR0_DCC_BASE,
  mmCB_COLOR1_DCC_BASE,
  mmCB_COLOR2_DCC_BASE,
  mmCB_COLOR3_DCC_BASE,
  mmCB_COLOR4_DCC_BASE,
  mmCB_COLOR5_DCC_BASE,
  mmCB_COLOR6_DCC_BASE,
  mmCB_COLOR7_DCC_BASE,

  mmDB_HTILE_SURFACE:
   begin
    onWidthHeight(PWORD(Body));
    Exit;
   end;
 end;

 case Body^ of

  {$ifdef ww}

  OP_HINT_UPDATE_PS_DB_CONTROL                     :Writeln('\HINT_UPDATE_PS_DB_CONTROL');
  OP_HINT_UPDATE_VS_OUT_CNTL                       :Writeln('\HINT_UPDATE_VS_OUT_CNTL');
  OP_HINT_UPDATE_PS_FORMAT                         :Writeln('\HINT_UPDATE_PS_FORMAT');
  OP_HINT_UPDATE_PS_INPUT                          :Writeln('\HINT_UPDATE_PS_INPUT');
  OP_HINT_UPDATE_PS_IN_CONTROL                     :Writeln('\HINT_UPDATE_PS_IN_CONTROL');
  OP_HINT_UPDATE_VS_OUT_CONFIG                     :Writeln('\HINT_UPDATE_VS_OUT_CONFIG');
  OP_HINT_UPDATE_PS_RSRC                           :Writeln('\HINT_UPDATE_PS_RSRC');
  OP_HINT_UPDATE_PS_BARY_CNTL                      :Writeln('\HINT_UPDATE_PS_BARY_CNTL');
  OP_HINT_UPDATE_VS_RSRC                           :Writeln('\HINT_UPDATE_VS_RSRC');
  OP_HINT_UPDATE_VS_POS_FORMAT                     :Writeln('\HINT_UPDATE_VS_POS_FORMAT');

  OP_HINT_WRITE_GPU_PREFETCH_INTO_L2               :Writeln('\HINT_WRITE_GPU_PREFETCH_INTO_L2');
  OP_HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER        :Writeln('\HINT_BASE_ALLOCATE_FROM_COMMAND_BUFFER');
  OP_HINT_PUSH_MARKER                              ://Writeln('\HINT_PUSH_MARKER');
   onPushMarker(@Body[1]);

  OP_HINT_POP_MARKER                               :Writeln('\HINT_POP_MARKER');
  OP_HINT_SET_VSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_VSHARP_IN_USER_DATA');
  OP_HINT_SET_TSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_TSHARP_IN_USER_DATA');
  OP_HINT_SET_SSHARP_IN_USER_DATA                  :Writeln('\HINT_SET_SSHARP_IN_USER_DATA');
  OP_HINT_SET_USER_DATA_REGION                     :Writeln('\HINT_SET_USER_DATA_REGION');
  OP_HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS      :Writeln('\HINT_BASE_MARK_DISPATCH_DRAW_ACB_ADDRESS');
  OP_HINT_INLINE_DATA1                             :Writeln('\HINT_INLINE_DATA1');
  OP_HINT_INLINE_DATA2                             :Writeln('\HINT_INLINE_DATA2');

  OP_HINT_SET_DB_RENDER_CONTROL                    :Writeln('\HINT_SET_DB_RENDER_CONTROL');
  OP_HINT_SET_DB_COUNT_CONTROL                     :Writeln('\HINT_SET_DB_COUNT_CONTROL');
  OP_HINT_SET_RENDER_OVERRIDE_CONTROL              :Writeln('\HINT_SET_RENDER_OVERRIDE_CONTROL');
  OP_HINT_SET_RENDER_OVERRIDE2CONTROL              :Writeln('\HINT_SET_RENDER_OVERRIDE2CONTROL');
  OP_HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK      :Writeln('\HINT_SET_PS_SHADER_SAMPLE_EXCLUSION_MASK');
  OP_HINT_SET_DEPTH_BOUNDS_RANGE                   :Writeln('\HINT_SET_DEPTH_BOUNDS_RANGE');
  OP_HINT_SET_STENCIL_CLEAR_VALUE                  :Writeln('\HINT_SET_STENCIL_CLEAR_VALUE');
  OP_HINT_SET_DEPTH_CLEAR_VALUE                    :Writeln('\HINT_SET_DEPTH_CLEAR_VALUE');
  OP_HINT_SET_SCREEN_SCISSOR                       :Writeln('\HINT_SET_SCREEN_SCISSOR');
  OP_HINT_SET_DEPTH_RENDER_TARGET                  :Writeln('\HINT_SET_DEPTH_RENDER_TARGET');
  OP_HINT_SET_BORDER_COLOR_TABLE_ADDR              :Writeln('\HINT_SET_BORDER_COLOR_TABLE_ADDR');
  OP_HINT_SET_WINDOW_OFFSET                        :Writeln('\HINT_SET_WINDOW_OFFSET');
  OP_HINT_SET_WINDOW_SCISSOR                       :Writeln('\HINT_SET_WINDOW_SCISSOR');
  OP_HINT_SET_CLIP_RECTANGLE_RULE                  :Writeln('\HINT_SET_CLIP_RECTANGLE_RULE');
  OP_HINT_SET_HARDWARE_SCREEN_OFFSET               :Writeln('\HINT_SET_HARDWARE_SCREEN_OFFSET');
  OP_HINT_SET_RENDER_TARGET_MASK                   :Writeln('\HINT_SET_RENDER_TARGET_MASK');
  OP_HINT_SET_GENERIC_SCISSOR                      :Writeln('\HINT_SET_GENERIC_SCISSOR');
  OP_HINT_SET_PERFMON_ENABLE                       :Writeln('\HINT_SET_PERFMON_ENABLE');
  OP_HINT_SET_SCALED_RESOLUTION_GRID               :Writeln('\HINT_SET_SCALED_RESOLUTION_GRID');
  OP_HINT_SET_FOVEATED_WINDOW                      :Writeln('\HINT_SET_FOVEATED_WINDOW');
  OP_HINT_SET_INDEX_OFFSET                         :Writeln('\HINT_SET_INDEX_OFFSET');
  OP_HINT_SET_PRIMITIVE_RESET_INDEX                :Writeln('\HINT_SET_PRIMITIVE_RESET_INDEX');
  OP_HINT_SET_STENCIL_OP_CONTROL                   :Writeln('\HINT_SET_STENCIL_OP_CONTROL');
  OP_HINT_SET_STENCIL                              :Writeln('\HINT_SET_STENCIL');
  OP_HINT_SET_PS_SHADER_USAGE                      :Writeln('\HINT_SET_PS_SHADER_USAGE');
  OP_HINT_SET_GRAPHICS_SCRATCH_SIZE                :Writeln('\HINT_SET_GRAPHICS_SCRATCH_SIZE');
  OP_HINT_SET_DEPTH_STENCIL_CONTROL                :Writeln('\HINT_SET_DEPTH_STENCIL_CONTROL');
  OP_HINT_SET_DEPTH_EQAA_CONTROL                   :Writeln('\HINT_SET_DEPTH_EQAA_CONTROL');
  OP_HINT_SET_CB_CONTROL                           :Writeln('\HINT_SET_CB_CONTROL');
  OP_HINT_SET_CLIP_CONTROL                         :Writeln('\HINT_SET_CLIP_CONTROL');
  OP_HINT_SET_PRIMITIVE_SETUP                      :Writeln('\HINT_SET_PRIMITIVE_SETUP');
  OP_HINT_SET_VIEWPORT_TRANSFORM_CONTROL           :Writeln('\HINT_SET_VIEWPORT_TRANSFORM_CONTROL');
  OP_HINT_SET_OBJECT_ID_MODE                       :Writeln('\HINT_SET_OBJECT_ID_MODE');
  OP_HINT_SET_COMPUTE_SHADER_CONTROL               :Writeln('\HINT_SET_COMPUTE_SHADER_CONTROL');
  OP_HINT_SET_COMPUTE_SCRATCH_SIZE                 :Writeln('\HINT_SET_COMPUTE_SCRATCH_SIZE');
  OP_HINT_SET_PRIMITIVE_TYPE_BASE                  :Writeln('\HINT_SET_PRIMITIVE_TYPE_BASE');
  OP_HINT_SET_POINT_SIZE                           :Writeln('\HINT_SET_POINT_SIZE');
  OP_HINT_SET_POINT_MIN_MAX                        :Writeln('\HINT_SET_POINT_MIN_MAX');
  OP_HINT_SET_LINE_WIDTH                           :Writeln('\HINT_SET_LINE_WIDTH');
  OP_HINT_SET_GS_MODE                              :Writeln('\HINT_SET_GS_MODE');
  OP_HINT_SET_GS_ON_CHIP_CONTROL                   :Writeln('\HINT_SET_GS_ON_CHIP_CONTROL');
  OP_HINT_SET_SCAN_MODE_CONTROL                    :Writeln('\HINT_SET_SCAN_MODE_CONTROL');
  OP_HINT_SET_PS_SHADER_RATE                       :Writeln('\HINT_SET_PS_SHADER_RATE');
  OP_HINT_SET_PRIMITIVE_ID_ENABLE                  :Writeln('\HINT_SET_PRIMITIVE_ID_ENABLE');
  OP_HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE         :Writeln('\HINT_SET_PRIMITIVE_RESET_INDEX_ENABLE');
  OP_HINT_SET_DRAW_PAYLOAD_CONTROL                 :Writeln('\HINT_SET_DRAW_PAYLOAD_CONTROL');
  OP_HINT_SET_INSTANCE_STEP_RATE                   :Writeln('\HINT_SET_INSTANCE_STEP_RATE');
  OP_HINT_SETUP_ES_GS_RING_REGISTERS               :Writeln('\HINT_SETUP_ES_GS_RING_REGISTERS');
  OP_HINT_SET_VERTEX_REUSE_ENABLE                  :Writeln('\HINT_SET_VERTEX_REUSE_ENABLE');
  OP_HINT_SET_HTILE_STENCIL0                       :Writeln('\HINT_SET_HTILE_STENCIL0');
  OP_HINT_SET_HTILE_STENCIL1                       :Writeln('\HINT_SET_HTILE_STENCIL1');
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1           :Writeln('\HINT_SETUP_DRAW_OPAQUE_PARAMETERS_1');
  OP_HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0           :Writeln('\HINT_SETUP_DRAW_OPAQUE_PARAMETERS_0');
  OP_HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS :Writeln('\HINT_SET_TESSELLATION_DISTRIBUTION_THRESHOLDS');
  OP_HINT_SET_ACTIVE_SHADER_STAGES                 :Writeln('\HINT_SET_ACTIVE_SHADER_STAGES');
  OP_HINT_SETUP_GS_VS_RING_REGISTERS               :Writeln('\HINT_SETUP_GS_VS_RING_REGISTERS');
  OP_HINT_SET_ALPHA_TO_MASK_CONTROL                :Writeln('\HINT_SET_ALPHA_TO_MASK_CONTROL');
  OP_HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK:Writeln('\HINT_SET_DISPATCH_DRAW_INDEX_DEALLOCATION_MASK');
  OP_HINT_SET_POLYGON_OFFSET_Z_FORMAT              :Writeln('\HINT_SET_POLYGON_OFFSET_Z_FORMAT');
  OP_HINT_SET_POLYGON_OFFSET_CLAMP                 :Writeln('\HINT_SET_POLYGON_OFFSET_CLAMP');
  OP_HINT_SET_POLYGON_OFFSET_FRONT                 :Writeln('\HINT_SET_POLYGON_OFFSET_FRONT');
  OP_HINT_SET_POLYGON_OFFSET_BACK                  :Writeln('\HINT_SET_POLYGON_OFFSET_BACK');
  OP_HINT_SET_GS_MODE_DISABLE                      :Writeln('\HINT_SET_GS_MODE_DISABLE');
  OP_HINT_SET_STREAMOUT_MAPPING                    :Writeln('\HINT_SET_STREAMOUT_MAPPING');
  OP_HINT_SET_AA_SAMPLE_COUNT                      :Writeln('\HINT_SET_AA_SAMPLE_COUNT');
  OP_HINT_SET_VERTEX_QUANTIZATION                  :Writeln('\HINT_SET_VERTEX_QUANTIZATION');
  OP_HINT_SET_GUARD_BANDS                          :Writeln('\HINT_SET_GUARD_BANDS');
  OP_HINT_SET_AA_SAMPLE_MASK1                      :Writeln('\HINT_SET_AA_SAMPLE_MASK1');
  OP_HINT_SET_AA_SAMPLE_MASK2                      :Writeln('\HINT_SET_AA_SAMPLE_MASK2');
  OP_HINT_SET_TEXTURE_GRADIENT_FACTORS             :Writeln('\HINT_SET_TEXTURE_GRADIENT_FACTORS');
  OP_HINT_SET_PERF_COUNTER_CONTROL_PA              :Writeln('\HINT_SET_PERF_COUNTER_CONTROL_PA');
  OP_HINT_SET_PRIMITIVE_TYPE_NEO                   :Writeln('\HINT_SET_PRIMITIVE_TYPE_NEO');
  {$endif}

  OP_HINT_PREPARE_FLIP_VOID:
  begin
   onPrepareFlip();
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_VOID');{$endif}
  end;
  OP_HINT_PREPARE_FLIP_LABEL:
  begin
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_LABEL');{$endif}
   onPrepareFlipLabel(pm4Hdr,@Body[1]);
  end;
  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID:
  begin
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID');{$endif}
   onPrepareFlipWithEopInterrupt(pm4Hdr,@Body[1]);
  end;
  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL:
  begin
   {$ifdef ww}Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL');{$endif}
   onPrepareFlipWithEopInterruptLabel(pm4Hdr,@Body[1]);
  end;
  {$ifdef ww}else
    Writeln('\Hint:',HexStr(Body^,8));{$endif}
 end;
end;

procedure onContextControl(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDCONTEXTCONTROL);
begin
 {$ifdef ww}writeln;{$endif}
end;

//The purpose of the Clear_State packet is to reduce command buffer preamble setup time for all driver versions of
//both DX and OpenGL and to specifically support DX11’s Display Lists requirements. The definition of Clear State
//is essentially everything off, resources all NULL, other values set to a defined default state.

procedure onClearState(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDCLEARSTATE);
begin
 GPU_REGS.Clear;
end;

procedure SetContextReg(reg:WORD;value:DWORD);
begin
 GFXRing.LastSetReg:=reg;

 Case reg of

  mmCB_COLOR0_BASE..mmCB_COLOR7_DCC_BASE:
  begin
   PDWORD(@GPU_REGS.RENDER_TARGET)[reg-mmCB_COLOR0_BASE]:=value;
  end;

  mmCB_TARGET_MASK  :DWORD(GPU_REGS.TARGET_MASK)     :=value;
  mmCB_COLOR_CONTROL:DWORD(GPU_REGS.CB_COLOR_CONTROL):=value;


  mmCB_BLEND_RED..mmCB_BLEND_ALPHA:
  begin
   PDWORD(@GPU_REGS.CB_BLEND_RGBA)[reg-mmCB_BLEND_RED]:=value;
  end;

  mmCB_BLEND0_CONTROL..mmCB_BLEND7_CONTROL:
  begin
   PDWORD(@GPU_REGS.CB_BLEND_CONTROL)[reg-mmCB_BLEND0_CONTROL]:=value;
  end;

  mmCB_SHADER_MASK  :DWORD(GPU_REGS.SPI.PS.SHADER_MASK):=value;

  mmPA_SC_MODE_CNTL_0:DWORD(GPU_REGS.SC_MODE_CNTL_0) :=value;
  mmPA_SC_MODE_CNTL_1:DWORD(GPU_REGS.SC_MODE_CNTL_1) :=value;

  mmPA_SC_GENERIC_SCISSOR_TL:DWORD(GPU_REGS.GENERIC_SCISSOR.TL) :=value;
  mmPA_SC_GENERIC_SCISSOR_BR:DWORD(GPU_REGS.GENERIC_SCISSOR.BR) :=value;

  mmPA_SC_VPORT_SCISSOR_0_TL..mmPA_SC_VPORT_SCISSOR_15_BR:
  begin
   PDWORD(@GPU_REGS.VPORT_SCISSOR)[reg-mmPA_SC_VPORT_SCISSOR_0_TL]:=value;
  end;

  mmPA_SC_VPORT_ZMIN_0..mmPA_SC_VPORT_ZMAX_15:
  begin
   PDWORD(@GPU_REGS.VPORT_ZMIN_MAX)[reg-mmPA_SC_VPORT_ZMIN_0]:=value;
  end;

  mmPA_CL_VPORT_XSCALE..mmPA_CL_VPORT_ZOFFSET_15:
  begin
   PDWORD(@GPU_REGS.VPORT_SCALE_OFFSET)[reg-mmPA_CL_VPORT_XSCALE]:=value;
  end;

  mmPA_CL_VTE_CNTL:DWORD(GPU_REGS.VTE_CNTL):=value;

  mmPA_SC_SCREEN_SCISSOR_TL:DWORD(GPU_REGS.SCREEN_SCISSOR_TL):=value;
  mmPA_SC_SCREEN_SCISSOR_BR:DWORD(GPU_REGS.SCREEN_SCISSOR_BR):=value;

  mmPA_SC_AA_MASK_X0Y0_X1Y0:DWORD(GPU_REGS.SC_AA_MASK_X0Y0_X1Y0):=value;
  mmPA_SC_AA_MASK_X0Y1_X1Y1:DWORD(GPU_REGS.SC_AA_MASK_X0Y1_X1Y1):=value;
  mmPA_SC_AA_CONFIG        :DWORD(GPU_REGS.SC_AA_CONFIG):=value;

  mmPA_SU_HARDWARE_SCREEN_OFFSET:DWORD(GPU_REGS.HARDWARE_SCREEN_OFFSET):=value;

  mmPA_SU_VTX_CNTL:DWORD(GPU_REGS.VTX_CNTL):=value;

  mmPA_SU_LINE_CNTL:DWORD(GPU_REGS.SU_LINE_CNTL)      :=value;
  mmPA_SU_POINT_SIZE:DWORD(GPU_REGS.SU_POINT_SIZE)    :=value;
  mmPA_SU_POINT_MINMAX:DWORD(GPU_REGS.SU_POINT_MINMAX):=value;

  mmPA_CL_CLIP_CNTL:DWORD(GPU_REGS.CL_CLIP_CNTL)        :=value;
  mmPA_SC_CLIPRECT_RULE:DWORD(GPU_REGS.SC_CLIPRECT_RULE):=value;

  mmPA_CL_GB_VERT_CLIP_ADJ:PDWORD(@GPU_REGS.GB_CLIP.VERT_CLIP_ADJ)^:=value;
  mmPA_CL_GB_VERT_DISC_ADJ:PDWORD(@GPU_REGS.GB_CLIP.VERT_DISC_ADJ)^:=value;
  mmPA_CL_GB_HORZ_CLIP_ADJ:PDWORD(@GPU_REGS.GB_CLIP.HORZ_CLIP_ADJ)^:=value;
  mmPA_CL_GB_HORZ_DISC_ADJ:PDWORD(@GPU_REGS.GB_CLIP.HORZ_DISC_ADJ)^:=value;

  mmSPI_VS_OUT_CONFIG    :DWORD(GPU_REGS.SPI.VS.OUT_CONFIG):=value;
  mmPA_CL_VS_OUT_CNTL    :DWORD(GPU_REGS.SPI.VS.OUT_CNTL):=value;

  mmSPI_SHADER_POS_FORMAT:DWORD(GPU_REGS.SPI.VS.POS_FORMAT):=value;
  mmSPI_SHADER_Z_FORMAT  :DWORD(GPU_REGS.SPI.PS.Z_FORMAT)  :=value;
  mmSPI_SHADER_COL_FORMAT:DWORD(GPU_REGS.SPI.PS.COL_FORMAT):=value;
  mmSPI_BARYC_CNTL       :DWORD(GPU_REGS.SPI.PS.BARYC_CNTL):=value;

  mmSPI_PS_INPUT_ENA     :DWORD(GPU_REGS.SPI.PS.INPUT_ENA) :=value;
  mmSPI_PS_INPUT_ADDR    :DWORD(GPU_REGS.SPI.PS.INPUT_ADDR):=value;
  mmSPI_PS_IN_CONTROL    :DWORD(GPU_REGS.SPI.PS.IN_CONTROL):=value;

  mmSPI_PS_INPUT_CNTL_0..mmSPI_PS_INPUT_CNTL_31:
  begin
   PDWORD(@GPU_REGS.SPI.PS.INPUT_CNTL)[reg-mmSPI_PS_INPUT_CNTL_0]:=value;
  end;

  mmDB_SHADER_CONTROL    :DWORD(GPU_REGS.SPI.PS.SHADER_CONTROL):=value;

  mmDB_RENDER_CONTROL    :DWORD(GPU_REGS.DEPTH.RENDER_CONTROL):=value;
  mmDB_DEPTH_CONTROL     :DWORD(GPU_REGS.DEPTH.DEPTH_CONTROL):=value;

  mmDB_DEPTH_VIEW        :DWORD(GPU_REGS.DEPTH.DEPTH_VIEW        ):=value;
  mmDB_HTILE_DATA_BASE   :DWORD(GPU_REGS.DEPTH.HTILE_DATA_BASE   ):=value;
  mmDB_DEPTH_BOUNDS_MIN  :DWORD(GPU_REGS.DEPTH.DEPTH_BOUNDS_MIN  ):=value;
  mmDB_DEPTH_BOUNDS_MAX  :DWORD(GPU_REGS.DEPTH.DEPTH_BOUNDS_MAX  ):=value;
  mmDB_STENCIL_CLEAR     :DWORD(GPU_REGS.DEPTH.STENCIL_CLEAR     ):=value;
  mmDB_DEPTH_CLEAR       :DWORD(GPU_REGS.DEPTH.DEPTH_CLEAR       ):=value;

  mmDB_DEPTH_INFO        :DWORD(GPU_REGS.DEPTH.DEPTH_INFO        ):=value;
  mmDB_Z_INFO            :DWORD(GPU_REGS.DEPTH.Z_INFO            ):=value;
  mmDB_STENCIL_INFO      :DWORD(GPU_REGS.DEPTH.STENCIL_INFO      ):=value;
  mmDB_Z_READ_BASE       :DWORD(GPU_REGS.DEPTH.Z_READ_BASE       ):=value;
  mmDB_STENCIL_READ_BASE :DWORD(GPU_REGS.DEPTH.STENCIL_READ_BASE ):=value;
  mmDB_Z_WRITE_BASE      :DWORD(GPU_REGS.DEPTH.Z_WRITE_BASE      ):=value;
  mmDB_STENCIL_WRITE_BASE:DWORD(GPU_REGS.DEPTH.STENCIL_WRITE_BASE):=value;
  mmDB_DEPTH_SIZE        :DWORD(GPU_REGS.DEPTH.DEPTH_SIZE        ):=value;
  mmDB_DEPTH_SLICE       :DWORD(GPU_REGS.DEPTH.DEPTH_SLICE       ):=value;

  mmDB_HTILE_SURFACE     :DWORD(GPU_REGS.DEPTH.HTILE_SURFACE     ):=value;

  mmVGT_SHADER_STAGES_EN :DWORD(GPU_REGS.VGT_SHADER_STAGES_EN) :=value;
  mmVGT_OUT_DEALLOC_CNTL :DWORD(GPU_REGS.VGT_OUT_DEALLOC_CNTL) :=value;

  mmVGT_VTX_CNT_EN       :DWORD(GPU_REGS.VGT_VTX_INDX.CNT_EN):=value;

  mmVGT_MIN_VTX_INDX     :DWORD(GPU_REGS.VGT_VTX_INDX.MIN_INDX):=value;
  mmVGT_MAX_VTX_INDX     :DWORD(GPU_REGS.VGT_VTX_INDX.MAX_INDX):=value;

  mmVGT_INDX_OFFSET      :DWORD(GPU_REGS.VGT_VTX_INDX.INDX_OFFSET):=value;

  mmVGT_MULTI_PRIM_IB_RESET_INDX:DWORD(GPU_REGS.VGT_MULTI_PRIM_IB_RESET_INDX):=value;

  mmVGT_OUTPUT_PATH_CNTL:DWORD(GPU_REGS.VGT_OUTPUT_PATH_CNTL):=value;

  //mmVGT_GS_MODE:value:=value;

  mmPA_SU_POLY_OFFSET_DB_FMT_CNTL:DWORD(GPU_REGS.PA_SU_POLY_OFFSET_DB_FMT_CNTL):=value;

  {$ifdef ww}else
   Writeln('SetContextReg:',getRegName(reg),'=',HexStr(value,8));{$endif}
 end;

end;

procedure SetShReg(reg:WORD;value:DWORD);
begin
 GFXRing.LastSetReg:=reg;

 Case reg of

  mmSPI_SHADER_PGM_LO_PS   :GPU_REGS.SPI.PS.LO:=value;
  mmSPI_SHADER_PGM_HI_PS   :GPU_REGS.SPI.PS.HI:=value;
  mmSPI_SHADER_PGM_RSRC1_PS:DWORD(GPU_REGS.SPI.PS.RSRC1):=value;
  mmSPI_SHADER_PGM_RSRC2_PS:DWORD(GPU_REGS.SPI.PS.RSRC2):=value;
  mmSPI_SHADER_PGM_RSRC3_PS:DWORD(GPU_REGS.SPI.PS.RSRC3):=value;

  mmSPI_SHADER_USER_DATA_PS_0..mmSPI_SHADER_USER_DATA_PS_15:
   PDWORD(@GPU_REGS.SPI.PS.USER_DATA)[reg-mmSPI_SHADER_USER_DATA_PS_0]:=value;

  mmSPI_SHADER_PGM_LO_VS   :GPU_REGS.SPI.VS.LO:=value;
  mmSPI_SHADER_PGM_HI_VS   :GPU_REGS.SPI.VS.HI:=value;
  mmSPI_SHADER_PGM_RSRC1_VS:DWORD(GPU_REGS.SPI.VS.RSRC1):=value;
  mmSPI_SHADER_PGM_RSRC2_VS:DWORD(GPU_REGS.SPI.VS.RSRC2):=value;
  mmSPI_SHADER_PGM_RSRC3_VS:DWORD(GPU_REGS.SPI.VS.RSRC3):=value;

  mmSPI_SHADER_USER_DATA_VS_0..mmSPI_SHADER_USER_DATA_VS_15:
   PDWORD(@GPU_REGS.SPI.VS.USER_DATA)[reg-mmSPI_SHADER_USER_DATA_VS_0]:=value;

  mmSPI_SHADER_LATE_ALLOC_VS:DWORD(GPU_REGS.SPI.VS.LATE_ALLOC):=value;

  //mmSPI_SHADER_PGM_RSRC3_GS:value:=value;
  //mmSPI_SHADER_PGM_RSRC3_ES:value:=value;
  //mmSPI_SHADER_PGM_RSRC3_HS:value:=value;
  //mmSPI_SHADER_PGM_RSRC3_LS:value:=value;

  mmCOMPUTE_PGM_LO         :GPU_REGS.SPI.CS.LO:=value;
  mmCOMPUTE_PGM_HI         :GPU_REGS.SPI.CS.HI:=value;
  mmCOMPUTE_PGM_RSRC1      :DWORD(GPU_REGS.SPI.CS.RSRC1):=value;
  mmCOMPUTE_PGM_RSRC2      :DWORD(GPU_REGS.SPI.CS.RSRC2):=value;

  mmCOMPUTE_NUM_THREAD_X   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_X):=value;
  mmCOMPUTE_NUM_THREAD_Y   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_Y):=value;
  mmCOMPUTE_NUM_THREAD_Z   :DWORD(GPU_REGS.SPI.CS.NUM_THREAD_Z):=value;

  mmCOMPUTE_USER_DATA_0..mmCOMPUTE_USER_DATA_15:
   PDWORD(@GPU_REGS.SPI.CS.USER_DATA)[reg-mmCOMPUTE_USER_DATA_0]:=value;

  mmCOMPUTE_STATIC_THREAD_MGMT_SE0:DWORD(GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE0):=value;
  mmCOMPUTE_STATIC_THREAD_MGMT_SE1:DWORD(GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE1):=value;
  mmCOMPUTE_RESOURCE_LIMITS       :DWORD(GPU_REGS.SPI.CS.RESOURCE_LIMITS):=value;

  {$ifdef ww}else
   Writeln('onSetShReg:',getRegName(reg),'=',HexStr(value,8));{$endif}
 end;
end;

procedure SetUContextReg(reg:WORD;value:DWORD);
begin
 GFXRing.LastSetReg:=reg;

 Case reg of

  mmVGT_PRIMITIVE_TYPE:DWORD(GPU_REGS.VGT_PRIMITIVE_TYPE):=value;
  mmVGT_INDEX_TYPE    :DWORD(GPU_REGS.VGT_INDEX_TYPE    ):=value;
  mmVGT_NUM_INSTANCES :DWORD(GPU_REGS.VGT_NUM_INSTANCES ):=value;
  mmGRBM_GFX_INDEX    :DWORD(GPU_REGS.GRBM_GFX_INDEX    ):=value;

  {$ifdef ww}else
   Writeln('SetUContextReg:',getRegName(reg),'=',HexStr(value,8));{$endif}
 end;

end;

const
 CONTEXT_REG_BASE = $A000;
 CONTEXT_SPACE_START=$0000a000;

procedure onSetContextReg(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=pm4Hdr.count;
 if c<>0 then
 For i:=0 to c-1 do
 begin
  r:=CONTEXT_REG_BASE+Body^.REG_OFFSET+i;
  v:=PDWORD(@Body^.REG_DATA)[i];

  //{$ifdef ww}Writeln('SetContextReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  //Continue;

  Inc(GFXRing.SetCxCount);

  SetContextReg(r,v);

 end;
end;

const
 PERSISTENT_SPACE_START=$00002c00;

procedure onSetShReg(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=pm4Hdr.count;
 if c<>0 then
 For i:=0 to c-1 do
 begin
  r:=PERSISTENT_SPACE_START+Body^.REG_OFFSET+i;
  v:=PDWORD(@Body^.REG_DATA)[i];

  //{$ifdef ww}Writeln('SetShReg:',getRegName(r),'=',HexStr(v,8));{$endif}
  //Continue;

  Inc(GFXRing.SetShCount);

  SetShReg(r,v);

 end;
end;

type
 PVGT_PRIMITIVE_TYPE=^TVGT_PRIMITIVE_TYPE;
 PGRBM_GFX_INDEX=^TGRBM_GFX_INDEX;

Const
 UCONFIG_SPACE_START=$0000c000;

procedure onSetUConfigReg(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;

begin
 //r:=Body^.REG_OFFSET+$C000;

 //mmVGT_PRIMITIVE_TYPE__CI__VI                     = 0xC242;
 //mmVGT_INDEX_TYPE__CI__VI                         = 0xC243;
 //mmVGT_NUM_INSTANCES__CI__VI                      = 0xC24D;

 c:=pm4Hdr.count;
 if c<>0 then
 For i:=0 to c-1 do
 begin
  r:=UCONFIG_SPACE_START{ $C000}+Body^.REG_OFFSET+i;
  v:=PDWORD(@Body^.REG_DATA)[i];

  //{$ifdef ww}Writeln('SetUConfigReg:',getRegName(r),'=',HexStr(v,8));{$endif}

  SetUContextReg(r,v);

 end;


end;

type
 PVGT_DMA_INDEX_TYPE=^TVGT_DMA_INDEX_TYPE;

procedure onIndexType(pm4Hdr:PM4_TYPE_3_HEADER;Body:PVGT_DMA_INDEX_TYPE);
begin
 GPU_REGS.VGT_DMA.INDEX_TYPE:=Body^;
 {$ifdef ww}
 Case Body^.INDEX_TYPE of
  VGT_INDEX_16:Write('VGT_INDEX_16');
  VGT_INDEX_32:Write('VGT_INDEX_32');
  VGT_INDEX_8 :Write('VGT_INDEX_8');
  else         Write('VGT_INDEX_UNKNOW');
 end;
 Writeln;
 {$endif}
end;

//SLICE.TILE_MAX number of tiles in a slice (equal to Pitch * Height / 64),

//PITCH.TILE_MAX = 159,   //(PITCH.TILE_MAX+1)*8=1280
//SLICE.TILE_MAX = 15359, //(SLICE.TILE_MAX+1)/(PITCH.TILE_MAX+1)*8=768

type
 TvEvent2=class(TvEvent)
  Procedure Release(Sender:TObject);
 end;

Procedure TvEvent2.Release(Sender:TObject);
begin
 Free;
end;

var
 LastSetShCount:ptruint;
 LastSetCxCount:ptruint;

 LastRenderCmd:TvRenderTargets;

procedure UpdateGpuRegsInfo;
var
 FAttrBuilder:TvAttrBuilder;
 FUniformBuilder:TvUniformBuilder;

 i,o,a:DWORD;
 pData:Pointer;

 FRenderCmd:TvRenderTargets;

 RT_INFO:TRT_INFO;
 DB_INFO:TDB_INFO;

 ri:TvImage2;
 iv:TvImageView2;

 sm:TvSampler;

 range:TVkImageSubresourceRange;
 clr:TVkClearColorValue;

 clr2:TVkClearDepthStencilValue;

 BufferImageCopy:TVkBufferImageCopy;

 fdump_ps,fdump_vs:RawByteString;

 buf:TvHostBuffer;

 FDescriptorGroup:TvDescriptorGroup;

 FVSShader:TvShaderExt;
 FPSShader:TvShaderExt;

 FShadersKey:TvShadersKey;
 FShaderGroup:TvShaderGroup;

 ctx_change:Boolean;

begin

 {$ifdef null_rt}Exit;{$endif}

 if (LastSetShCount=GFXRing.SetShCount) and
    (LastSetCxCount=GFXRing.SetCxCount) then
 begin
  Exit;
 end;

 ctx_change:=(LastSetCxCount<>GFXRing.SetCxCount);
 ctx_change:=True;

 LastSetShCount:=GFXRing.SetShCount;
 LastSetCxCount:=GFXRing.SetCxCount;

 fdump_ps:=DumpPS(GPU_REGS);
 fdump_vs:=DumpVS(GPU_REGS);

 {$ifdef ww}Writeln(fdump_vs);{$endif}
 {$ifdef ww}Writeln(fdump_ps);{$endif}

 //if not GPU_REGS.COMP_ENABLE then Exit;

 if not (GPU_REGS.COMP_ENABLE or GPU_REGS.DB_ENABLE) then Exit;

 GFXRing.AllocCmdBuffer;

 ///////////////////

 if ctx_change then
 begin

  FRenderCmd:=TvRenderTargets.Create;
  LastRenderCmd:=FRenderCmd;

  FRenderCmd.FRenderPass:=TvRenderPass.Create;
  FRenderCmd.FPipeline  :=TvGraphicsPipeline.Create;
  FRenderCmd.FPipeline.FRenderPass:=FRenderCmd.FRenderPass;

  FRenderCmd.FFramebuffer:=TvFramebuffer.Create;
  FRenderCmd.FFramebuffer.SetRenderPass(FRenderCmd.FRenderPass);

  //////////////////


  FRenderCmd.FFramebuffer.FreeImageViews;
  FRenderCmd.FRenderPass.Clear;
  FRenderCmd.FPipeline.Clear;

  FRenderCmd.FFramebuffer.SetSize(GPU_REGS.GET_SCREEN_SIZE);

  FRenderCmd.FPipeline.SetPrimType(GPU_REGS.GET_PRIM_TYPE);
  FRenderCmd.FPipeline.SetBlendColors(@GPU_REGS.CB_BLEND_RGBA);

  FRenderCmd.FRenderArea:=GPU_REGS.GET_SCREEN;


  For i:=0 to 15 do
   if GPU_REGS.VP_ENABLE(i) then
   begin
    FRenderCmd.FPipeline.AddVPort(GPU_REGS.GET_VPORT(i),GPU_REGS.GET_SCISSOR(i));
   end;

  GFXRing.CmdBuffer.EndRenderPass;

  if GPU_REGS.COMP_ENABLE then
  For i:=0 to 7 do
  if GPU_REGS.RT_ENABLE(i) then
   begin
    RT_INFO:=GPU_REGS.GET_RT_INFO(i);

    {$ifdef ww}Writeln('RT:',i,' ',HexStr(RT_INFO.FImageInfo.Addr));{$endif}

    //RT_INFO.IMAGE_USAGE:=RT_INFO.IMAGE_USAGE or TM_CLEAR;
    //RT_INFO.IMAGE_USAGE:=RT_INFO.IMAGE_USAGE and (not TM_READ);

    ri:=FetchImage(GFXRing.CmdBuffer,
                   RT_INFO.FImageInfo,
                   ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                   ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or
                   ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                   ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                   RT_INFO.IMAGE_USAGE
                   );

    //ri.data_usage:=ri.data_usage and (not TM_READ); //reset read

    iv:=ri.FetchView(GFXRing.CmdBuffer,RT_INFO.FImageView);

    //

    {$ifdef ww}
    Writeln('TM_READ :',RT_INFO.IMAGE_USAGE and TM_READ <>0);
    Writeln('TM_WRITE:',RT_INFO.IMAGE_USAGE and TM_WRITE<>0);
    Writeln('TM_CLEAR:',RT_INFO.IMAGE_USAGE and TM_CLEAR<>0);
    {$endif}

    //Writeln(hexstr(PDWORD(RT_INFO.FImageInfo.Addr)[0],8));
    //writeln;

    //RT_INFO.IMAGE_USAGE:={TM_CLEAR or }TM_READ{ or TM_WRITE};

    //RT_INFO.IMAGE_USAGE:=RT_INFO.IMAGE_USAGE and (not TM_CLEAR);

    ri.PushBarrier(GFXRing.CmdBuffer,
                   GetColorAccessMask(RT_INFO.IMAGE_USAGE),
                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                   ord(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) );

    FRenderCmd.FFramebuffer.AddImageView(iv);

    //Writeln('colorAttachmentCount:',FRenderCmd.FRenderPass.subpass.colorAttachmentCount);
    //Writeln('AtCount:',FRenderCmd.FRenderPass.AtCount);

    FRenderCmd.FRenderPass.AddColorRef(FRenderCmd.FRenderPass.subpass.colorAttachmentCount,RT_INFO.IMAGE_USAGE);

    FRenderCmd.FRenderPass.AddColorAt(RT_INFO.FImageInfo.cformat,
                                      RT_INFO.IMAGE_USAGE,
                                      TVkSampleCountFlagBits(RT_INFO.FImageInfo.params.samples));


    //RT_INFO.blend.blendEnable:=0;
    FRenderCmd.FPipeline.AddBlend(RT_INFO.blend);

    FRenderCmd.AddClearColor(TVkClearValue(RT_INFO.CLEAR_COLOR));

   end;

  if GPU_REGS.DB_ENABLE then
  begin
   DB_INFO:=GPU_REGS.GET_DB_INFO;

   {$ifdef ww}
   Writeln('DB');
   Writeln('TM_READ :',DB_INFO.DEPTH_USAGE and TM_READ <>0);
   Writeln('TM_WRITE:',DB_INFO.DEPTH_USAGE and TM_WRITE<>0);
   Writeln('TM_CLEAR:',DB_INFO.DEPTH_USAGE and TM_CLEAR<>0);
   {$endif}

   //DB_INFO.DEPTH_USAGE:={TM_CLEAR or} TM_READ or TM_WRITE;


   ri:=FetchImage(GFXRing.CmdBuffer,
                  DB_INFO.FImageInfo,
                  ord(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  {DB_INFO.DEPTH_USAGE}0
                  );

   //ri.data_usage:=ri.data_usage and (not TM_READ); //reset read

   iv:=ri.FetchView(GFXRing.CmdBuffer);


   if not GPU_REGS.COMP_ENABLE then
   begin
    ri.PushBarrier(GFXRing.CmdBuffer,
                   ord(VK_ACCESS_TRANSFER_WRITE_BIT),
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                   ord(VK_PIPELINE_STAGE_TRANSFER_BIT));

    range:=iv.GetSubresRange;
    clr2:=DB_INFO.CLEAR_VALUE.depthStencil;

    GFXRing.CmdBuffer.ClearDepthStencilImage(ri.FHandle,
                                             VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                             @clr2,
                                             1,@range);

    Exit;
   end;

   ri.PushBarrier(GFXRing.CmdBuffer,
                  GetDepthStencilAccessMask(DB_INFO.DEPTH_USAGE,DB_INFO.STENCIL_USAGE),
                  GetDepthStencilLayout    (DB_INFO.DEPTH_USAGE,DB_INFO.STENCIL_USAGE),
                  DB_INFO.zorder_stage );


   FRenderCmd.FFramebuffer.AddImageView(iv);

   //Writeln('colorAttachmentCount:',FRenderCmd.FRenderPass.subpass.colorAttachmentCount);
   //Writeln('AtCount:',FRenderCmd.FRenderPass.AtCount);

   FRenderCmd.FRenderPass.SetDepthStencilRef(FRenderCmd.FRenderPass.subpass.colorAttachmentCount,DB_INFO.DEPTH_USAGE,DB_INFO.STENCIL_USAGE);

   //if not GPU_REGS.COMP_ENABLE then
   //begin
   // DB_INFO.DEPTH_CLEAR:=True;
   //end;

   FRenderCmd.FRenderPass.AddDepthAt(
    DB_INFO.FImageInfo.cformat,
    DB_INFO.DEPTH_USAGE,
    DB_INFO.STENCIL_USAGE);

   FRenderCmd.FRenderPass.SetZorderStage(DB_INFO.zorder_stage);

   FRenderCmd.AddClearColor(DB_INFO.CLEAR_VALUE);

   FRenderCmd.FPipeline.DepthStencil.depthTestEnable      :=DB_INFO.depthTestEnable      ;
   FRenderCmd.FPipeline.DepthStencil.depthWriteEnable     :=DB_INFO.depthWriteEnable     ;
   FRenderCmd.FPipeline.DepthStencil.depthCompareOp       :=DB_INFO.depthCompareOp       ;
   FRenderCmd.FPipeline.DepthStencil.depthBoundsTestEnable:=DB_INFO.depthBoundsTestEnable;
   FRenderCmd.FPipeline.DepthStencil.stencilTestEnable    :=DB_INFO.stencilTestEnable    ;
   FRenderCmd.FPipeline.DepthStencil.front                :=DB_INFO.front                ;
   FRenderCmd.FPipeline.DepthStencil.back                 :=DB_INFO.back                 ;
   FRenderCmd.FPipeline.DepthStencil.minDepthBounds       :=DB_INFO.minDepthBounds       ;
   FRenderCmd.FPipeline.DepthStencil.maxDepthBounds       :=DB_INFO.maxDepthBounds       ;

  end;

 end else //ctx_change
 begin
  FRenderCmd:=LastRenderCmd;
 end;

 {$ifdef ww}Writeln('[FVSShader]');{$endif}
 FVSShader:=FetchShader(vShaderStageVs,0,GPU_REGS);
 if (FVSShader=nil) then Exit;

 FAttrBuilder:=Default(TvAttrBuilder);
 FVSShader.EnumVertLayout(@FAttrBuilder.AddAttr,FVSShader.FDescSetId,@GPU_REGS.SPI.VS.USER_DATA);

 if (Length(FAttrBuilder.FBindDescs)<>0) then
 begin
  With FRenderCmd.FPipeline.vertexInputInfo do
  begin
   vertexBindingDescriptionCount  :=Length(FAttrBuilder.FBindDescs);
   pVertexBindingDescriptions     :=@FAttrBuilder.FBindDescs[0];
   vertexAttributeDescriptionCount:=Length(FAttrBuilder.FAttrDescs);
   pVertexAttributeDescriptions   :=@FAttrBuilder.FAttrDescs[0];
  end;
 end;

 {$ifdef ww}Writeln('[FPSShader]');{$endif}
 FPSShader:=FetchShader(vShaderStagePs,1,GPU_REGS);
 if (FPSShader=nil) then Exit;

 FShadersKey:=Default(TvShadersKey);
 FShadersKey.SetVSShader(FVSShader);
 FShadersKey.SetPSShader(FPSShader);

 FShaderGroup:=FetchShaderGroup(@FShadersKey);
 Assert(FShaderGroup<>nil);

 FRenderCmd.FPipeline.FShaderGroup:=FShaderGroup;

  FDescriptorGroup:=FetchDescriptorGroup(GFXRing.CmdBuffer,FShaderGroup.FLayout);


  FUniformBuilder:=Default(TvUniformBuilder);

  FVSShader.EnumUnifLayout(@FUniformBuilder.AddAttr,FVSShader.FDescSetId,@GPU_REGS.SPI.VS.USER_DATA);

  if (FPSShader<>nil) then
   FPSShader.EnumUnifLayout(@FUniformBuilder.AddAttr,FPSShader.FDescSetId,@GPU_REGS.SPI.PS.USER_DATA);

 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(GFXRing.CmdBuffer,
                  FImage,
                  ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  TM_READ
                  );

   iv:=ri.FetchView(GFXRing.CmdBuffer,FView);

   //if not GFXRing.CmdBuffer.IsRenderPass then
   begin
    ri.PushBarrier(GFXRing.CmdBuffer,
                   ord(VK_ACCESS_SHADER_READ_BIT),
                   VK_IMAGE_LAYOUT_GENERAL,
                   ord(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                   ord(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) );
   end;

  end;
 end;

 if ctx_change then
 begin
  if not GFXRing.CmdBuffer.BeginRenderPass(FRenderCmd) then
  begin
   Writeln('!BeginRenderPass');
   Assert(false);
  end;
 end;

 //

 if (FVSShader.FPushConst.size<>0) then
 begin
  pData:=FVSShader.GetPushConstData(@GPU_REGS.SPI.VS.USER_DATA);

  if (pData<>nil) then
  GFXRing.CmdBuffer.PushConstant(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                 ord(VK_SHADER_STAGE_VERTEX_BIT),
                                 0,FVSShader.FPushConst.size,
                                 pData);

 end;

 if (FPSShader<>nil) then
 if (FPSShader.FPushConst.size<>0) then
 begin
  pData:=FPSShader.GetPushConstData(@GPU_REGS.SPI.PS.USER_DATA);

  if (pData<>nil) then
  GFXRing.CmdBuffer.PushConstant(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                 ord(VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,FPSShader.FPushConst.size,
                                 pData);
 end;

 if (Length(FAttrBuilder.FBindExt)<>0) then
 begin
  For i:=0 to High(FAttrBuilder.FBindExt) do
  With FAttrBuilder.FBindExt[i] do
  begin
   buf:=FetchHostBuffer(GFXRing.CmdBuffer,min_addr,stride*count,ord(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT));

   GFXRing.CmdBuffer.BindVertexBuffer(
                         binding,
                         buf.FHandle,
                         buf.Foffset);

  end;
 end;

 //

 if (Length(FUniformBuilder.FImages)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FImages) do
  With FUniformBuilder.FImages[i] do
  begin

   ri:=FetchImage(GFXRing.CmdBuffer,
                  FImage,
                  ord(VK_IMAGE_USAGE_SAMPLED_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or
                  ord(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  TM_READ
                  );

   iv:=ri.FetchView(GFXRing.CmdBuffer,FView);

   ri.data_usage:=ri.data_usage and (not TM_READ); ////////

   FDescriptorGroup.FSets[fset].BindImg(bind,0,
                                        iv.FHandle,
                                        VK_IMAGE_LAYOUT_GENERAL);


  end;
 end;
 //

 if (Length(FUniformBuilder.FSamplers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FSamplers) do
  With FUniformBuilder.FSamplers[i] do
  begin
   sm:=FetchSampler(GFXRing.CmdBuffer,PS);

   FDescriptorGroup.FSets[fset].BindSmp(bind,0,sm.FHandle);

  end;
 end;

 //
 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   buf:=FetchHostBuffer(GFXRing.CmdBuffer,addr,size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   o:=buf.Foffset;

   a:=o-AlignDw(o,limits.minStorageBufferOffsetAlignment);
   //Writeln('align_offset=',a);
   if (a<>offset) then Assert(false);

   o:=AlignDw(o,limits.minStorageBufferOffsetAlignment{ $10});    //minStorageBufferOffsetAlignment

   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        o,
                                        VK_WHOLE_SIZE);

  end;
 end;

 GFXRing.CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_GRAPHICS,FDescriptorGroup);

 //writeln;
end;

procedure UpdateGpuRegsInfoCompute;
var
 FUniformBuilder:TvUniformBuilder;

 fdump_cs:RawByteString;

 i,o,a:Integer;

 pData:Pointer;

 buf:TvHostBuffer;

 FDescriptorGroup:TvDescriptorGroup;

 FCSShader:TvShaderExt;

 FShadersKey:TvShadersKey;
 FShaderGroup:TvShaderGroup;

 FComputePipeline:TvComputePipeline2;
begin

 {$ifdef null_rt}Exit;{$endif}

 if (LastSetShCount=GFXRing.SetShCount) and
    (LastSetCxCount=GFXRing.SetCxCount) then
 begin
  Exit;
 end;

 LastSetShCount:=GFXRing.SetShCount;
 LastSetCxCount:=GFXRing.SetCxCount;

 fdump_cs:=DumpCS(GPU_REGS);

 GFXRing.AllocCmdBuffer;

 FCSShader:=FetchShader(vShaderStageCs,0,GPU_REGS);
 if (FCSShader=nil) then Exit;


 FShadersKey:=Default(TvShadersKey);
 FShadersKey.SetCSShader(FCSShader);

 FShaderGroup:=FetchShaderGroup(@FShadersKey);
 Assert(FShaderGroup<>nil);

 FComputePipeline:=TvComputePipeline2.Create;
 FComputePipeline.SetLayout(FShaderGroup.FLayout);
 FComputePipeline.SetShader(FCSShader);
 FComputePipeline.Compile;

 GFXRing.CmdBuffer.BindCompute(FComputePipeline);

 if (FCSShader.FPushConst.size<>0) then
 begin
  pData:=FCSShader.GetPushConstData(@GPU_REGS.SPI.CS.USER_DATA);

  if (pData<>nil) then
  GFXRing.CmdBuffer.PushConstant(VK_PIPELINE_BIND_POINT_COMPUTE,
                          ord(VK_SHADER_STAGE_COMPUTE_BIT),
                          0,FCSShader.FPushConst.size,
                          pData);
 end;

 FUniformBuilder:=Default(TvUniformBuilder);
 FCSShader.EnumUnifLayout(@FUniformBuilder.AddAttr,FCSShader.FDescSetId,@GPU_REGS.SPI.CS.USER_DATA);

 FDescriptorGroup:=FetchDescriptorGroup(GFXRing.CmdBuffer,FShaderGroup.FLayout);

 if (Length(FUniformBuilder.FBuffers)<>0) then
 begin
  For i:=0 to High(FUniformBuilder.FBuffers) do
  With FUniformBuilder.FBuffers[i] do
  begin

   buf:=FetchHostBuffer(GFXRing.CmdBuffer,addr,size,ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT));

   o:=buf.Foffset;

   a:=o-AlignDw(o,limits.minStorageBufferOffsetAlignment);
   //Writeln('align_offset=',a);
   if (a<>offset) then Assert(false);

   o:=AlignDw(o,limits.minStorageBufferOffsetAlignment { $10});    //minStorageBufferOffsetAlignment


   FDescriptorGroup.FSets[fset].BindBuf(bind,0,
                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                        buf.FHandle,
                                        o,
                                        VK_WHOLE_SIZE);

  end;
 end;

 GFXRing.CmdBuffer.BindSets(VK_PIPELINE_BIND_POINT_COMPUTE,FDescriptorGroup);

 //
end;

procedure onDrawIndex2(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDRAWINDEX2);
var
 Addr:Pointer;

begin
 GPU_REGS.VGT_DMA.MAX_SIZE:=Body^.maxSize;
 GPU_REGS.VGT_DMA.BASE_LO :=Body^.indexBaseLo;
 GPU_REGS.VGT_DMA.BASE_HI :=Body^.indexBaseHi;
 GPU_REGS.VGT_DMA.SIZE    :=Body^.indexCount;
 GPU_REGS.VGT_DMA.INDICES :=Body^.indexCount;

  //drawInitiator:TVGT_DRAW_INITIATOR;

 UpdateGpuRegsInfo;

 Addr:=getIndexAddress(GPU_REGS.VGT_DMA.BASE_LO,GPU_REGS.VGT_DMA.BASE_HI);

 GFXRing.CmdBuffer.DrawIndex2(Addr,GPU_REGS.VGT_DMA.INDICES,GPU_REGS.GET_INDEX_TYPE);

 {$ifdef ww}Writeln('DrawIndex:',Body^.indexCount);{$endif}

 //GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
end;

procedure onDrawIndexAuto(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDRAWINDEXAUTO);
begin
 GPU_REGS.VGT_DMA.INDICES:=Body^.indexCount;

 UpdateGpuRegsInfo;

 GFXRing.CmdBuffer.DrawIndexAuto(GPU_REGS.VGT_DMA.INDICES);

 {$ifdef ww}Writeln('onDrawIndexAuto:',Body^.indexCount);{$endif}

 //GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
end;

procedure onDispatchDirect(pm4Hdr:PM4_TYPE_3_HEADER;Body:PPM4CMDDISPATCHDIRECT);
begin

 UpdateGpuRegsInfoCompute;

 GFXRing.CmdBuffer.DispatchDirect(Body^.dimX,Body^.dimY,Body^.dimZ);

 {$ifdef ww}Writeln('onDispatchDirect:',Body^.dimX,':',Body^.dimY,':',Body^.dimZ);{$endif}

 //GFXMicroEngine.PushCmd(GFXRing.CmdBuffer);
end;

type
 PVGT_DMA_NUM_INSTANCES=^TVGT_DMA_NUM_INSTANCES;

procedure onNumInstances(pm4Hdr:PM4_TYPE_3_HEADER;Body:PVGT_DMA_NUM_INSTANCES);
begin
 GPU_REGS.VGT_DMA.NUM_INSTANCES:=Body^;
 {$ifdef ww}Writeln('onNumInstances:',Body^);{$endif}
end;

procedure gfx_cp_parser(node:PvSubmitInfo);
var
 n,i,s,t:DWORD;
 token:DWORD;
 P:PByte;

begin
 if (node=nil) then Exit;

 n:=0;
 While (n<node^.count) do
 begin

  if (node^.ccbGpuAddrs<>nil) and (node^.ccbSizesInBytes<>nil) then
  begin
   Assert(node^.ccbSizesInBytes[n]=0,'TODO CCB');
  end;

  i:=0;
  s:=node^.dcbSizesInBytes[n];
  P:=PByte(node^.dcbGpuAddrs[n]);
  While (i<s) do
  begin
   token:=PDWORD(P)^;

   case PM4_TYPE(token) of
    0:begin
       onPm40(PM4_TYPE_0_HEADER(token),@PDWORD(P)[1]);
      end;
    3:begin
       case PM4_TYPE_3_HEADER(token).opcode of
        IT_NOP:
        begin
         onNop(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_EVENT_WRITE_EOP:
        begin
         {$ifdef ww}Writeln('IT_EVENT_WRITE_EOP');{$endif}
         onEventWriteEop(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_EVENT_WRITE_EOS:
        begin
         {$ifdef ww}Writeln('IT_EVENT_WRITE_EOS');{$endif}
         onEventWriteEos(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_DMA_DATA:
        begin
         {$ifdef ww}Writeln('IT_DMA_DATA');{$endif}
         onDMAData(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_ACQUIRE_MEM:
        begin
         {$ifdef ww}Writeln('IT_ACQUIRE_MEM');{$endif}
         onAcquireMem(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_CONTEXT_CONTROL:
        begin
         {$ifdef ww}Writeln('IT_CONTEXT_CONTROL');{$endif}
         onContextControl(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_CLEAR_STATE:
        begin
         {$ifdef ww}Writeln('IT_CLEAR_STATE');{$endif}
         onClearState(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_SET_CONTEXT_REG:
        begin
         {$ifdef ww}Writeln('IT_SET_CONTEXT_REG');{$endif}
         onSetContextReg(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_SET_SH_REG:
        begin
         {$ifdef ww}Writeln('IT_SET_SH_REG');{$endif}
         onSetShReg(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_SET_UCONFIG_REG:
        begin
         {$ifdef ww}Writeln('IT_SET_UCONFIG_REG');{$endif}
         onSetUConfigReg(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_INDEX_TYPE:
        begin
         {$ifdef ww}Writeln('IT_INDEX_TYPE');{$endif}
         onIndexType(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_DRAW_INDEX_2:
        begin
         {$ifdef ww}Writeln('IT_DRAW_INDEX_2');{$endif}
         onDrawIndex2(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_DRAW_INDEX_AUTO:
        begin
         {$ifdef ww}Writeln('IT_DRAW_INDEX_AUTO');{$endif}
         onDrawIndexAuto(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;
        IT_DISPATCH_DIRECT:
        begin
         {$ifdef ww}Writeln('IT_DISPATCH_DIRECT');{$endif}
         onDispatchDirect(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;

        IT_NUM_INSTANCES:
        begin
         {$ifdef ww}Writeln('IT_NUM_INSTANCES');{$endif}
         onNumInstances(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;

        IT_WAIT_REG_MEM:
        begin
         {$ifdef ww}Writeln('IT_WAIT_REG_MEM');{$endif}
         onWaitRegMem(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;

        IT_WRITE_DATA:
        begin
         {$ifdef ww}Writeln('IT_WRITE_DATA');{$endif}
         onWriteData(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;

        IT_EVENT_WRITE:
        begin
         {$ifdef ww}Writeln('IT_EVENT_WRITE'){$endif};
         onEventWrite(PM4_TYPE_3_HEADER(token),@PDWORD(P)[1]);
        end;

        else
         begin
          Writeln('PM4_TYPE_3.opcode:',HexStr(PM4_TYPE_3_HEADER(token).opcode,2));
          Assert(False);
         end;
       end;

       case PM4_TYPE_3_HEADER(token).opcode of
        IT_SET_CONTEXT_REG:;
        IT_SET_SH_REG     :;
        IT_SET_UCONFIG_REG:;
        else
         GFXRing.LastSetReg:=0;
       end;

      end;

    else
     begin
      Writeln('PM4_TYPE_',PM4_TYPE(token));
      Assert(False);
     end;
   end;

   t:=PM4_LENGTH_DW(token)*sizeof(DWORD);
   P:=P+t;
   i:=i+t;
  end;
  Inc(n);
 end;

end;

function gfx_submit(CmdBuffer:TvCmdBuffer):Boolean;
begin
 Result:=False;
 if (CmdBuffer=nil) then Exit;

 CmdBuffer.EndRenderPass;
 CmdBuffer.Fence.Reset;

 if (CmdBuffer.cmd_count<>0) then
 begin
  Result:=CmdBuffer.QueueSubmit;
 end;
end;

function gfx_test(CmdBuffer:TvCmdBuffer):Boolean;
begin
 Result:=True;
 if (CmdBuffer=nil) then Exit;
 Result:=(CmdBuffer.ret<>0) or (CmdBuffer.Fence.Status=VK_SUCCESS);
 if Result then
 begin
  CmdBuffer.ReleaseResource;
  FreeAndNil(CmdBuffer);
  //GPU_REGS.ClearDMA;
 end;
end;

initialization
 GPU_REGS.Clear;

end.

