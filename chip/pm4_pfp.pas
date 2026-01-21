unit pm4_pfp;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 pm4defs,
 pm4_context,
 pm4_stream,
 time,
 md_sleep,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

type
 p_pfp_ctx=^t_pfp_ctx;

 t_pm4_parse_cb=function(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;

 p_pm4_ibuffer=^t_pm4_ibuffer;
 t_pm4_ibuffer=record
  next:TAILQ_ENTRY;
  base:Pointer;
  buff:Pointer;
  size:Ptruint;
  bpos:Ptruint;
  picb:t_pm4_parse_cb;
  buft:t_pm4_stream_type;
  c_id:Byte;
 end;

 t_flush_stream=procedure(var stream:t_pm4_stream) of object;

 t_pfp_ctx=object
  freen:TAILQ_HEAD;
  stall:array[t_pm4_stream_type] of TAILQ_HEAD;
  //
  stream:array[t_pm4_stream_type] of t_pm4_stream;
  //
  on_flush_stream:t_flush_stream;
  //
  context:t_amd_context;
  //
  curr_ibuf :p_pm4_ibuffer;
  //
  LastSetReg:Word;
  event:PRTLEvent;
  //
  function  stream_type:t_pm4_stream_type;
  procedure init;
  procedure add_stall(ibuf:p_pm4_ibuffer);
  procedure free;
  //
  Procedure Flush_stream(t:t_pm4_stream_type);
  //
  procedure set_esgs_gsvs_ring_size(esgsRingSize,gsvsRingSize:DWORD);
  //
  procedure set_reg(i:word;data:DWORD);
  procedure set_sh_reg(i:word;data:DWORD);
  procedure set_asc_reg(i:word;data:DWORD);
  procedure set_ctx_reg(i:word;data:DWORD);
 end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;
                       buff:Pointer;
                       size:Ptruint;
                        icb:t_pm4_parse_cb;
                       buft:t_pm4_stream_type;
                       c_id:Byte=0):Boolean;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;
                        buf:PPM4CMDINDIRECTBUFFER;
                        icb:t_pm4_parse_cb;
                       buft:t_pm4_stream_type):Boolean;

function pm4_ibuf_parse(pctx:p_pfp_ctx;ibuf:p_pm4_ibuffer):Integer;

function pm4_parse_ccb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
function pm4_parse_dcb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
function pm4_parse_compute_ring(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;

implementation

uses
 sys_bootparam,
 kern_dmem,
 kern_proc,
 vm_map,
 vm_tracking_map;

function PM4_TYPE(token:DWORD):Byte; inline;
begin
 Result:=(token shr 30) and 3;
end;

function PM4_LENGTH(token:DWORD):DWORD; inline;
begin
 //body size must overflow to zero
 Result:=(((token shr 14) + 4) and $FFFC) + 4;
end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;
                       buff:Pointer;
                       size:Ptruint;
                        icb:t_pm4_parse_cb;
                       buft:t_pm4_stream_type;
                       c_id:Byte=0):Boolean;
begin
 Result:=True;
 ibuf^.next:=Default(TAILQ_ENTRY);
 ibuf^.base:=nil;
 ibuf^.buff:=buff;
 ibuf^.size:=size;
 ibuf^.bpos:=0;
 ibuf^.picb:=icb;
 ibuf^.buft:=buft;
 ibuf^.c_id:=c_id;
end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;
                        buf:PPM4CMDINDIRECTBUFFER;
                        icb:t_pm4_parse_cb;
                       buft:t_pm4_stream_type):Boolean;
var
 ib_base:QWORD;
 ib_size:QWORD;
 addr:Pointer;
begin
 Result:=False;

 case buf^.header.opcode of
  IT_INDIRECT_BUFFER_CNST:;
  IT_INDIRECT_BUFFER     :;
  else
   begin
    Writeln('init not indirect buffer:0x',HexStr(DWORD(buf^.header),8));
    Assert(false,'init not indirect buffer');
   end;
 end;

 ib_base:=QWORD(buf^.ibBase);
 ib_size:=QWORD(buf^.ibSize)*sizeof(DWORD);

 addr:=get_dmem_ptr(Pointer(ib_base));

 //Writeln(' addr:0x'+HexStr(ib_base,16)+' '+HexStr(ib_size,16));

 ibuf^.next:=Default(TAILQ_ENTRY);
 ibuf^.base:=Pointer(ib_base); //adjust guest addr
 ibuf^.buff:=addr;
 ibuf^.size:=ib_size;
 ibuf^.bpos:=0;
 ibuf^.picb:=icb;
 ibuf^.buft:=buft;
 ibuf^.c_id:=0;

 Result:=True;
end;

function pm4_ibuf_parse(pctx:p_pfp_ctx;ibuf:p_pm4_ibuffer):Integer;
var
 buff:Pointer;
 i,token,len:DWORD;
begin
 Result:=0;

 pctx^.curr_ibuf :=ibuf;

 case pctx^.stream_type of
  stGfxDcb,
  stGfxCcb:pctx^.LastSetReg:=0;
  else;
 end;

 i:=ibuf^.bpos;
 buff:=ibuf^.buff+i;
 i:=ibuf^.size-i;

 while (i<>0) do
 begin
  token:=PDWORD(buff)^;

  if (PM4_TYPE(token)=2) then
  begin
   len:=sizeof(DWORD);
  end else
  begin
   len:=PM4_LENGTH(token);
  end;

  if (len>i) then
  begin
   i:=0;
   Break;
  end;

  Result:=ibuf^.picb(pctx,token,buff);
  if (Result<>0) then
  begin
   Break;
  end;

  Inc(buff,len);
  Dec(i,len);
 end;

 ibuf^.bpos:=ibuf^.size-i;

 pctx^.curr_ibuf:=nil;
end;

function t_pfp_ctx.stream_type:t_pm4_stream_type;
begin
 Result:=curr_ibuf^.buft;
end;

procedure t_pfp_ctx.init;
var
 i:t_pm4_stream_type;
begin
 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  stream[i]:=Default(t_pm4_stream);
  stream[i].buft:=i;
 end;
end;

procedure t_pfp_ctx.add_stall(ibuf:p_pm4_ibuffer);
var
 node:p_pm4_ibuffer;
 buft:t_pm4_stream_type;
begin
 node:=TAILQ_FIRST(@freen);
 if (node<>nil) then
 begin
  TAILQ_REMOVE(@freen,node,@node^.next);
 end else
 begin
  node:=AllocMem(SizeOf(t_pm4_ibuffer));
 end;

 node^:=ibuf^;

 buft:=node^.buft;

 if (stall[buft].tqh_last=nil) then
 begin
  TAILQ_INIT(@stall[buft]);
 end;

 TAILQ_INSERT_TAIL(@stall[buft],node,@node^.next);
end;

procedure free_nodes(head:P_TAILQ_HEAD);
var
 node:p_pm4_ibuffer;
begin
 node:=TAILQ_FIRST(head);
 while (node<>nil) do
 begin
  TAILQ_REMOVE(head,node,@node^.next);
  FreeMem(node);
  node:=TAILQ_FIRST(head);
 end;
end;

procedure t_pfp_ctx.free;
var
 i:t_pm4_stream_type;
begin
 free_nodes(@freen);

 for i:=Low(t_pm4_stream_type) to High(t_pm4_stream_type) do
 begin
  free_nodes(@stall[i]);
 end;
end;

Procedure t_pfp_ctx.Flush_stream(t:t_pm4_stream_type);
begin
 Assert(on_flush_stream<>nil,'on_flush_stream');

 on_flush_stream(stream[t]);
end;

procedure t_pfp_ctx.set_esgs_gsvs_ring_size(esgsRingSize,gsvsRingSize:DWORD);
begin
 context.UC_REG.VGT_ESGS_RING_SIZE:=esgsRingSize;
 context.UC_REG.VGT_GSVS_RING_SIZE:=gsvsRingSize;
end;

procedure t_pfp_ctx.set_reg(i:word;data:DWORD);
begin
 if not context.set_reg(i,data) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,'Unknow:',getRegName(i),':=0x',HexStr(data,8));
 end;
end;

procedure t_pfp_ctx.set_sh_reg(i:word;data:DWORD);
begin
 if not context.set_sh_reg(i,data) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,'Unknow:',getRegName(i+$2C00),':=0x',HexStr(data,8));
 end;
end;

procedure t_pfp_ctx.set_asc_reg(i:word;data:DWORD);
var
 c_id:Byte;
begin
 c_id:=curr_ibuf^.c_id;

 if not context.set_asc_reg(c_id,i,data) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,'Unknow:',getRegName(i+$2C00),':=0x',HexStr(data,8));
 end;
end;

procedure t_pfp_ctx.set_ctx_reg(i:word;data:DWORD);
begin
 if not context.set_ctx_reg(i,data) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,'Unknow:',getRegName(i+$A000),':=0x',HexStr(data,8));
 end;
end;

///

procedure onLoadConstRam(pctx:p_pfp_ctx;Body:PPM4CMDCONSTRAMLOAD);
begin
 Assert(pctx^.stream_type=stGfxCcb);

 {
 Writeln(' adr=0x',HexStr(Body^.addr,16));
 Writeln(' len=0x',HexStr(Body^.numDwords*4,4));
 Writeln(' ofs=0x',HexStr(Body^.offset,4));
 }

 pctx^.stream[stGfxCcb].LoadConstRam(Pointer(Body^.addr and (not QWORD(31))),
                                     Body^.numDwords and (not 7),
                                     Body^.offset and (not 31));
end;

function PM4_BODY_LENGTH_DW(header:PM4_TYPE_3_HEADER;sizeof:WORD):WORD; inline;
begin
 Result:=header.count + 2;
 sizeof:=sizeof div 4;
 if (Result>sizeof) then
 begin
  Result:=Result-sizeof;
 end else
 begin
  Result:=0;
 end;
end;

procedure onWriteConstRam(pctx:p_pfp_ctx;Body:PPM4CMDCONSTRAMWRITE);
var
 count:Word;

 src:PDWORD;
 src_dmem:PDWORD;
begin
 Assert(pctx^.stream_type=stGfxCcb);

 count:=PM4_BODY_LENGTH_DW(Body^.header,SizeOf(PM4CMDCONSTRAMWRITE));
 if (count=0) then Exit;

 src_dmem:=@Body^.data;

 //convert src_dmem -> src

 with pctx^.curr_ibuf^ do
 begin
  src:=base+(Int64(src_dmem)-Int64(buff));
 end;

 pctx^.stream[stGfxCcb].LoadConstRam(src,count,Body^.offset and (not 3));
end;

procedure onDumpConstRam(pctx:p_pfp_ctx;Body:PPM4CMDCONSTRAMDUMP);
begin
 Assert(pctx^.stream_type=stGfxCcb);

 Assert(Body^.incrementCs=0);
 Assert(Body^.incrementCe=0);

 pctx^.stream[stGfxCcb].DumpConstRam(Pointer(Body^.addr and (not QWORD(3))),
                                     Body^.numDwords,
                                     Body^.offset and (not 3));
end;

procedure onIncrementCECounter(pctx:p_pfp_ctx;Body:Pointer);
begin
 Assert(pctx^.stream_type=stGfxCcb);

 pctx^.stream[stGfxCcb].IncrementCE();
end;

procedure onIncrementDECounter(pctx:p_pfp_ctx;Body:Pointer);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 pctx^.stream[stGfxDcb].IncrementDE();
end;

procedure onWaitOnCECounter(pctx:p_pfp_ctx;Body:PPM4CMDWAITONCECOUNTER);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 pctx^.stream[stGfxDcb].WaitOnCECounter();
end;

procedure onWaitOnDECounterDiff(pctx:p_pfp_ctx;Body:PPM4CMDWAITONDECOUNTERDIFF);
begin
 Assert(pctx^.stream_type=stGfxCcb);

 //(DE_COUNT - CE_COMPARE_COUNT) < DIFF

 pctx^.stream[stGfxCcb].WaitOnDECounterDiff(Body^.counterDiff);
end;

const
 ShdrType:array[0..1] of Pchar=('(GX)','(CS)');

function pm4_parse_ccb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     if p_print_gpu_ops then Writeln('PM4_TYPE_0');
    end;
  2:begin //PM4_TYPE_2
     if p_print_gpu_ops then Writeln('PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     if p_print_gpu_ops then
     if (PM4_TYPE_3_HEADER(token).opcode<>IT_NOP) or
        (not p_print_gpu_hint) then
     begin
      Writeln('IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),
                ' ',ShdrType[PM4_TYPE_3_HEADER(token).shaderType],
              ' len:',PM4_LENGTH(token));
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_NOP:;

      IT_LOAD_CONST_RAM         :onLoadConstRam       (pctx,buff);
      IT_WRITE_CONST_RAM        :onWriteConstRam      (pctx,buff);
      IT_DUMP_CONST_RAM         :onDumpConstRam       (pctx,buff);

      IT_INCREMENT_CE_COUNTER   :onIncrementCECounter (pctx,buff);
      IT_WAIT_ON_DE_COUNTER_DIFF:onWaitOnDECounterDiff(pctx,buff);

      else
       begin
        Writeln(stderr,'[CCB]PM4_TYPE_3.opcode:',get_op_name(PM4_TYPE_3_HEADER(token).opcode));
        Assert (False ,'[CCB]PM4_TYPE_3.opcode:'+get_op_name(PM4_TYPE_3_HEADER(token).opcode));
       end;
     end;

    end;
  else
   begin
    Writeln(stderr,'[CCB]PM4_TYPE_',PM4_TYPE(token));
    Assert (False ,'[CCB]PM4_TYPE_'+IntToStr(PM4_TYPE(token)));
   end;
 end;

end;

procedure FlushAndWaitMe(pctx:p_pfp_ctx);
var
 event:PRTLEvent;
begin
 if (pctx^.stream_type=stGfxDcb) then
 begin

  if (pctx^.event=nil) then
  begin
   pctx^.event:=RTLEventCreate;
  end;

  event:=pctx^.event;

  pctx^.stream[stGfxDcb].PfpSyncMe(event);

  pctx^.Flush_stream(stGfxDcb);

  RTLEventWaitFor(event);
 end;
end;

procedure onEventWrite(pctx:p_pfp_ctx;Body:PTPM4CMDEVENTWRITE);
const
 c_p_stride:array[0..3] of PChar=('32_BITS','64_BITS','128_BITS','256_BITS');
begin
 Assert(pctx^.stream_type=stGfxDcb);

 DWORD(pctx^.context.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 if p_print_gpu_ops then
 Case Body^.eventType of
  CS_PARTIAL_FLUSH           :Writeln(' eventType=CS_PARTIAL_FLUSH');
  CACHE_FLUSH_AND_INV_EVENT  :Writeln(' eventType=FLUSH_AND_INV_EVENT');
  DB_CACHE_FLUSH_AND_INV     :Writeln(' eventType=DB_CACHE_FLUSH_AND_INV');
  FLUSH_AND_INV_DB_DATA_TS   :Writeln(' eventType=FLUSH_AND_INV_DB_DATA_TS');
  FLUSH_AND_INV_DB_META      :Writeln(' eventType=FLUSH_AND_INV_DB_META');
  FLUSH_AND_INV_CB_DATA_TS   :Writeln(' eventType=FLUSH_AND_INV_CB_DATA_TS');
  FLUSH_AND_INV_CB_META      :Writeln(' eventType=FLUSH_AND_INV_CB_META');
  FLUSH_AND_INV_CB_PIXEL_DATA:Writeln(' eventType=FLUSH_AND_INV_CB_PIXEL_DATA');
  THREAD_TRACE_MARKER        :Writeln(' eventType=THREAD_TRACE_MARKER');
  PIXEL_PIPE_STAT_CONTROL    :Writeln(' eventType=PIXEL_PIPE_STAT_CONTROL');
  PIXEL_PIPE_STAT_DUMP       :Writeln(' eventType=PIXEL_PIPE_STAT_DUMP');
  PIXEL_PIPE_STAT_RESET      :Writeln(' eventType=PIXEL_PIPE_STAT_RESET');
  PIPELINESTAT_STOP          :Writeln(' eventType=PIPELINESTAT_STOP');
  PERFCOUNTER_START          :Writeln(' eventType=PERFCOUNTER_START');
  PERFCOUNTER_STOP           :Writeln(' eventType=PERFCOUNTER_STOP');
  PERFCOUNTER_SAMPLE         :Writeln(' eventType=PERFCOUNTER_SAMPLE');
  else
                              Writeln(' eventType=0x',HexStr(Body^.eventType,2));
 end;

 if p_print_gpu_ops then
 Case Body^.eventType of
  PIXEL_PIPE_STAT_CONTROL:
   begin
    Writeln('  counter_id=',Body^.u.Control.counter_id);
    Writeln('  stride    =',c_p_stride[Body^.u.Control.stride]);
   end;
  PIXEL_PIPE_STAT_DUMP:
   begin
    Writeln('  address=0x',HexStr(Body^.u.address and QWORD($FFFFFFFFF8),10));
   end;
 end;

 Case Body^.eventType of
  PIXEL_PIPE_STAT_CONTROL:
   begin
    pctx^.context.PixelPipeStatControl:=Body^.u.Control;
   end;
  PIXEL_PIPE_STAT_DUMP:
   begin
    pctx^.stream[stGfxDcb].PipeStatDump(Body^.u.address and QWORD($FFFFFFFFF8),pctx^.context.PixelPipeStatControl);
   end;
  else
   begin
    pctx^.stream[stGfxDcb].EventWrite(Body^.eventType);
   end;
 end;

end;

procedure onEventWriteEop(pctx:p_pfp_ctx;Body:PPM4CMDEVENTWRITEEOP);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 //FlushAndWaitMe(pctx);

 Case Body^.eventType of
  CACHE_FLUSH_TS,               //FlushCbDbCaches
  CACHE_FLUSH_AND_INV_TS_EVENT, //FlushAndInvalidateCbDbCaches
  BOTTOM_OF_PIPE_TS:;           //CbDbReadsDone
  else
   Assert(False,'EventWriteEop: eventType=0x'+HexStr(Body^.eventType,1));
 end;

 if (Body^.eventIndex<>EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP) then
 begin
  Assert(False,'EventWriteEop: eventIndex=0x'+HexStr(Body^.eventIndex,1));
 end;

 DWORD(pctx^.context.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 if p_print_gpu_ops then
 begin
  Case Body^.eventType of
   CACHE_FLUSH_TS              :Writeln(' eventType  =','FlushCbDbCaches');
   CACHE_FLUSH_AND_INV_TS_EVENT:Writeln(' eventType  =','FlushAndInvalidateCbDbCaches');
   BOTTOM_OF_PIPE_TS           :Writeln(' eventType  =','CbDbReadsDone');
   else;
  end;

  Writeln(' interrupt  =0x',HexStr(Body^.intSel,2));
  Writeln(' srcSelector=0x',HexStr(Body^.dataSel,2));
  Writeln(' dstGpuAddr =0x',HexStr(Body^.address,10));
  Writeln(' immValue   =0x',HexStr(Body^.DATA,16));
 end;

 //if (Body^.destTcL2<>0) then Exit; //write to L2

 pctx^.stream[stGfxDcb].EventWriteEop(Pointer(Body^.address),Body^.DATA,Body^.eventType,Body^.dataSel,Body^.intSel);

 pctx^.Flush_stream(stGfxDcb);
end;

procedure onEventWriteEos(pctx:p_pfp_ctx;Body:PPM4CMDEVENTWRITEEOS);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 //shaderType is any?
 //Assert(Body^.header.shaderType=1,'shaderType<>CS');

 Case Body^.eventType of
  CS_DONE:;
  PS_DONE:;
  else
   Assert(False,'EventWriteEos: eventType=0x'+HexStr(Body^.eventType,1));
   //Writeln(stderr,'EventWriteEos: eventType=0x'+HexStr(Body^.eventType,1));
 end;

 if p_print_gpu_ops then
 Case Body^.eventType of
  CS_DONE:Writeln(' CS_DONE');
  PS_DONE:Writeln(' PS_DONE');
  else;
 end;

 if (Body^.eventIndex<>EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP) then
 begin
  Assert(False,'EventWriteEos: eventIndex=0x'+HexStr(Body^.eventIndex,1));
  //Writeln(stderr,'EventWriteEos: eventIndex=0x'+HexStr(Body^.eventIndex,1));
 end;

 DWORD(pctx^.context.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 pctx^.stream[stGfxDcb].EventWriteEos(Pointer(Body^.address),Body^.data,Body^.eventType,Body^.command);

 //FlushAndWaitMe(pctx);
end;

const
 engine_str:array[0..3] of RawByteString=('ME','PFP','CE','3');

procedure onDmaData(pctx:p_pfp_ctx;Body:PPM4DMADATA);
var
 adrSrc:QWORD;
 adrDst:QWORD;
 adrSrc_dmem:Pointer;
 adrDst_dmem:Pointer;
 byteCount:DWORD;
 srcSel,dstSel:Byte;
begin
 //Assert(pctx^.stream_type=stGfxDcb);

 if (Body^.Flags2.saic=CPDMA_ADDR_SPACE_REG) then
 begin
  Assert(Body^.Flags2.saic<>0,'DmaData: read from fifo reg');
 end;

 if (Body^.Flags2.das=CPDMA_ADDR_SPACE_REG) then
 begin
  Assert(Body^.Flags2.daic<>0,'DmaData: send to fifo reg');
 end;

 srcSel:=((PDWORD(Body)[1] shr $1d) and 3) or ((PDWORD(Body)[6] shr $19) and 8) or ((PDWORD(Body)[6] shr $18) and 4);
 dstSel:=((PDWORD(Body)[1] shr $14) and 1) or ((PDWORD(Body)[6] shr $1a) and 8) or ((PDWORD(Body)[6] shr $19) and 4);

 adrSrc:=Body^.srcAddr;
 adrDst:=Body^.dstAddr;
 byteCount:=Body^.Flags2.byteCount;

 case dstSel of
  kDmaDataDstRegister,
  kDmaDataDstRegisterNoIncrement:
    if (DWORD(adrDst)=$3022C) then
    begin
     //prefetchIntoL2
     Exit;
    end;
  else;
 end;

 Case Body^.Flags1.engine of
  CP_DMA_ENGINE_ME:
   begin

    if p_print_gpu_ops then
    begin
     Writeln('[1]DmaData:0x',HexStr(adrSrc,10),'->',HexStr(adrDst,10),':size=0x',HexStr(byteCount,6));
    end;

    pctx^.stream[stGfxDcb].DmaData(dstSel,adrDst,srcSel,adrSrc,byteCount,Body^.Flags1.cpSync);
   end;
  CP_DMA_ENGINE_PFP:
   begin
    //Execute on the parser side

    //FlushAndWaitMe(pctx);

    if p_print_gpu_ops then
    begin
     Writeln('[2]DmaData:0x',HexStr(adrSrc,10),'->',HexStr(adrDst,10),':size=0x',HexStr(byteCount,6));
    end;

    adrDst_dmem:=get_dmem_ptr(Pointer(adrDst));

    case (srcSel or (dstSel shl 4)) of
     (kDmaDataSrcMemory        or (kDmaDataDstMemory        shl 4)),
     (kDmaDataSrcMemoryUsingL2 or (kDmaDataDstMemory        shl 4)),
     (kDmaDataSrcMemory        or (kDmaDataDstMemoryUsingL2 shl 4)),
     (kDmaDataSrcMemoryUsingL2 or (kDmaDataDstMemoryUsingL2 shl 4)):
       begin
        adrSrc_dmem:=get_dmem_ptr(Pointer(adrSrc));

        Move(adrSrc_dmem^,adrDst_dmem^,byteCount);

        vm_map_track_trigger(p_proc.p_vmspace,QWORD(adrDst),QWORD(adrDst)+byteCount,nil,M_DMEM_WRITE);
       end;
     (kDmaDataSrcData          or (kDmaDataDstMemory        shl 4)),
     (kDmaDataSrcData          or (kDmaDataDstMemoryUsingL2 shl 4)):
       begin
        FillDWORD(adrDst_dmem^,(byteCount div 4),DWORD(adrSrc));

        vm_map_track_trigger(p_proc.p_vmspace,QWORD(adrDst),QWORD(adrDst)+byteCount,nil,M_DMEM_WRITE);
       end;
    else
       Assert(false,'DmaData: srcSel=0x'+HexStr(srcSel,1)+' dstSel=0x'+HexStr(dstSel,1));
    end;

   end;
  else
   Assert(false,'DmaData: engine='+engine_str[Body^.Flags1.engine]);
 end;

end;

procedure onWriteData(pctx:p_pfp_ctx;Body:PPM4CMDWRITEDATA);
var
 src:PDWORD;
 dst:PDWORD;
 src_dmem:PDWORD;
 dst_dmem:PDWORD;
 count:Word;
 engineSel:Byte;
 dstSel:Byte;
begin
 if (Body^.CONTROL.dstSel=WRITE_DATA_DST_SEL_REGISTER) then
 begin
  Assert(Body^.CONTROL.wrOneAddr=0,'WriteData: send to fifo reg');
 end;

 count:=PM4_BODY_LENGTH_DW(Body^.header,SizeOf(PM4CMDWRITEDATA));

 if p_print_gpu_ops then
 begin
  Writeln(' engine     =',engine_str[Body^.CONTROL.engineSel]);
  Writeln(' dstSel     =',Body^.CONTROL.dstSel,' ',Body^.CONTROL.wrConfirm);
  Writeln(' dstAddr    =0x',HexStr(Body^.dstAddr,10));
  Writeln(' length     =',count*4);

  case count of
   1:Writeln(' data       =0x',HexStr(PDWORD(@Body^.DATA)^,8 ));
   2:Writeln(' data       =0x',HexStr(PQWORD(@Body^.DATA)^,16));
   else;
  end;
 end;

 if (count=0) then Exit;

 dst:=Pointer(Body^.dstAddr);
 src_dmem:=@Body^.DATA;

 engineSel:=Body^.CONTROL.engineSel;
 dstSel   :=Body^.CONTROL.dstSel;

 Case engineSel of
  WRITE_DATA_ENGINE_ME:
    begin
     //convert src_dmem -> src

     with pctx^.curr_ibuf^ do
     begin
      src:=base+(Int64(src_dmem)-Int64(buff));
     end;

     pctx^.stream[pctx^.stream_type].WriteData(dstSel,dst,src,count,Body^.CONTROL.wrConfirm);
    end;
  WRITE_DATA_ENGINE_PFP:
    begin

     //FlushAndWaitMe(pctx);

     case dstSel of
      WRITE_DATA_DST_SEL_MEMORY_SYNC,  //writeDataInline
      WRITE_DATA_DST_SEL_TCL2,         //writeDataInlineThroughL2
      WRITE_DATA_DST_SEL_MEMORY_ASYNC:
        begin
         dst_dmem:=get_dmem_ptr(dst);

         Move(src_dmem^,dst_dmem^,count*SizeOf(DWORD));

         vm_map_track_trigger(p_proc.p_vmspace,QWORD(dst),QWORD(dst)+count*SizeOf(DWORD),nil,M_DMEM_WRITE);
        end;
      else
        Assert(false,'WriteData: dstSel=0x'+HexStr(dstSel,1));
     end;

    end;
  else
    Assert(false,'WriteData: engineSel='+engine_str[engineSel]);
 end;

end;

Function me_test_mem(pollAddr:Pointer;ref,mask:DWORD;compareFunc:Byte):Boolean;
var
 val:DWORD;
begin
 val:=PQWORD(pollAddr)^ and mask;
 Case compareFunc of
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

procedure onWaitRegMem(pctx:p_pfp_ctx;Body:PPM4CMDWAITREGMEM);
begin

 if p_print_gpu_ops then
 begin
  Writeln(' engine     =',engine_str[Body^.engine]);
  Writeln(' memSpace   =',Body^.memSpace);
  Writeln(' operation  =',Body^.operation);
  Writeln(' pollAddress=0x',HexStr(Body^.pollAddress,10));
  Writeln(' reference  =0x',HexStr(Body^.reference,8));
  Writeln(' mask       =0x',HexStr(Body^.mask,8));
  Writeln(' compareFunc=0x',HexStr(Body^.compareFunc,1));
 end;

 Assert(Body^.operation=0,'WaitRegMem: operation=0x'+HexStr(Body^.operation,1));

 Case Body^.memSpace of
  WAIT_REG_MEM_SPACE_MEMORY:;
  else
   Assert(False,'WaitRegMem: memSpace=0x'+HexStr(Body^.memSpace,1));
 end;

 Case Body^.engine of
  WAIT_REG_MEM_ENGINE_ME:
    begin
     pctx^.stream[pctx^.stream_type].WaitRegMem(Pointer(Body^.pollAddress),Body^.reference,Body^.mask,Body^.compareFunc);
    end;
  WAIT_REG_MEM_ENGINE_PFP:
    begin

     pctx^.Flush_stream(pctx^.stream_type);

     while not me_test_mem(Pointer(Body^.pollAddress),Body^.reference,Body^.mask,Body^.compareFunc) do
     begin
      msleep_td(hz div 10000);
     end;

    end;
  else
    Assert(false,'WaitRegMem: engine='+engine_str[Body^.engine]);
 end;

end;

function revbinstr(val:int64;cnt:byte):shortstring;
var
 i:Integer;
begin
 Result[0]:=AnsiChar(cnt);
 for i:=1 to cnt do
 begin
  Result[i]:=AnsiChar(48+val and 1);
  val:=val shr 1;
 end;
end;

function coherCntl_str(CNTL:TCP_COHER_CNTL):shortstring;
begin
 Result:='';
 if (CNTL.DEST_BASE_0_ENA         <>0) then Result:=Result+'DS_B0 ';
 if (CNTL.DEST_BASE_1_ENA         <>0) then Result:=Result+'DS_B1 ';
 if (CNTL.CB0_DEST_BASE_ENA       <>0) then Result:=Result+'CB0_D ';
 if (CNTL.CB1_DEST_BASE_ENA       <>0) then Result:=Result+'CB1_D ';
 if (CNTL.CB2_DEST_BASE_ENA       <>0) then Result:=Result+'CB2_D ';
 if (CNTL.CB3_DEST_BASE_ENA       <>0) then Result:=Result+'CB3_D ';
 if (CNTL.CB4_DEST_BASE_ENA       <>0) then Result:=Result+'CB4_D ';
 if (CNTL.CB5_DEST_BASE_ENA       <>0) then Result:=Result+'CB5_D ';
 if (CNTL.CB6_DEST_BASE_ENA       <>0) then Result:=Result+'CB6_D ';
 if (CNTL.CB7_DEST_BASE_ENA       <>0) then Result:=Result+'CB7_D ';
 if (CNTL.DB_DEST_BASE_ENA        <>0) then Result:=Result+'DB_DS ';
 if (CNTL.TCL1_VOL_ACTION_ENA     <>0) then Result:=Result+'TCL1V ';
 if (CNTL.TC_VOL_ACTION_ENA       <>0) then Result:=Result+'TC_VA ';
 if (CNTL.TC_WB_ACTION_ENA        <>0) then Result:=Result+'TC_WB ';
 if (CNTL.DEST_BASE_2_ENA         <>0) then Result:=Result+'DS_B2 ';
 if (CNTL.DEST_BASE_3_ENA         <>0) then Result:=Result+'DS_B3 ';
 if (CNTL.TCL1_ACTION_ENA         <>0) then Result:=Result+'TCL1A ';
 if (CNTL.TC_ACTION_ENA           <>0) then Result:=Result+'TC_AC ';
 if (CNTL.CB_ACTION_ENA           <>0) then Result:=Result+'CB_AC ';
 if (CNTL.DB_ACTION_ENA           <>0) then Result:=Result+'DB_AC ';
 if (CNTL.SH_KCACHE_ACTION_ENA    <>0) then Result:=Result+'SH_KA ';
 if (CNTL.SH_KCACHE_VOL_ACTION_ENA<>0) then Result:=Result+'SH_KV ';
 if (CNTL.SH_ICACHE_ACTION_ENA    <>0) then Result:=Result+'SH_IA ';
end;

procedure onAcquireMem(pctx:p_pfp_ctx;Body:PPM4ACQUIREMEM);
var
 addr,size:QWORD;
begin
 //Assert(pctx^.stream_type=stGfxDcb);

 pctx^.context.UC_REG.CP_COHER_BASE_HI.COHER_BASE_HI_256B:=Body^.coherBaseHi;
 DWORD(pctx^.context.UC_REG.CP_COHER_CNTL)               :=Body^.coherCntl;
 pctx^.context.UC_REG.CP_COHER_SIZE                      :=Body^.coherSizeLo;
 pctx^.context.UC_REG.CP_COHER_BASE                      :=Body^.coherBaseLo;
 pctx^.context.UC_REG.CP_COHER_SIZE_HI.COHER_SIZE_HI_256B:=Body^.coherSizeHi;

 if p_print_gpu_ops then
 begin
  addr:=(QWORD(Body^.coherBaseLo) shl 8) or (QWORD(Body^.coherBaseHi) shl 40);
  size:=(QWORD(Body^.coherSizeLo) shl 8) or (QWORD(Body^.coherSizeHi) shl 40);

  Writeln('onAcquireMem:');
  Writeln(' Cntl=',coherCntl_str(pctx^.context.UC_REG.CP_COHER_CNTL));
  Writeln(' addr=0x',HexStr(addr,10));
  Writeln(' size=0x',HexStr(size,10));
 end;

 //FlushAndWaitMe(pctx);
end;

procedure onContextControl(pctx:p_pfp_ctx;Body:PPM4CMDCONTEXTCONTROL);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if (DWORD(Body^.loadControl )<>$80000000) or
    (DWORD(Body^.shadowEnable)<>$80000000) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' loadControl =b',revbinstr(DWORD(Body^.loadControl ),32));
  Writeln(stderr,' shadowEnable=b',revbinstr(DWORD(Body^.shadowEnable),32));
 end;
end;

function GetBaseIndexStr(i:Byte):RawByteString;
begin
 case i of
  BASE_INDEX_DISPLAY_LIST     :Result:='DISPLAY_LIST';
  BASE_INDEX_DRAW_INDIRECT    :Result:='DRAW/DISPATCH_INDIRECT';
  BASE_INDEX_LOAD_REG         :Result:='LOAD_REG';
  BASE_INDEX_INDIRECT_DATA    :Result:='INDIRECT_DATA';
  else
                               Result:='0x'+HexStr(i,1);
 end;
end;

procedure onSetBase(pctx:p_pfp_ctx;Body:PPM4CMDDRAWSETBASE);
var
 addr:QWORD;
begin
 //Assert(pctx^.stream_type=stGfxDcb);

 addr:=QWORD(Body^.address);
 if (addr<>0) then
 if p_print_gpu_ops then
 begin
  Writeln(' baseIndex=0x',GetBaseIndexStr(Body^.baseIndex));
  Writeln(' address  =0x',HexStr(addr,11));
 end;

 case Body^.baseIndex of
  BASE_INDEX_DISPLAY_LIST     :pctx^.context.BASE_ADDR_DISPLAY_LIST :=addr;
  BASE_INDEX_DRAW_INDIRECT    :pctx^.context.BASE_ADDR_DRAW_INDIRECT:=addr;
  BASE_INDEX_LOAD_REG         :pctx^.context.BASE_ADDR_LOAD_REG     :=addr;
  BASE_INDEX_INDIRECT_DATA    :pctx^.context.BASE_ADDR_INDIRECT_DATA:=addr;
  else;
 end;

end;

procedure onSetPredication(pctx:p_pfp_ctx;Body:PPM4CMDSETPREDICATION);
const
 c_pred_b:array[0..1] of PChar=('DrawIfNotVisible','DrawIfVisible');
 c_hint_v:array[0..1] of PChar=('Wait','Draw');
 c_pred_o:array[0..3] of PChar=('CLEAR','ZPASS','PRIMCOUNT','MEM');
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if p_print_gpu_ops then
 if (Body^.predOp<>SET_PRED_CLEAR) or (Body^.startAddress<>0) then
 begin
  Writeln(' startAddress=0x',HexStr(Body^.startAddress,10));
  Writeln(' pred        =',c_pred_b[Body^.predicationBoolean]);
  Writeln(' hint        =',c_hint_v[Body^.hint]);
  Writeln(' predOp      =',c_pred_o[Body^.predOp and 3]);
  Writeln(' continueBit =',Body^.continueBit);
 end;
end;

procedure onDrawPreamble(pctx:p_pfp_ctx;Body:PPM4CMDDRAWPREAMBLE);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 pctx^.context.UC_REG.VGT_PRIMITIVE_TYPE:=Body^.control1;
 pctx^.context.CX_REG.IA_MULTI_VGT_PARAM:=Body^.control2;
 pctx^.context.CX_REG.VGT_LS_HS_CONFIG  :=Body^.control3;
end;

procedure onClearState(pctx:p_pfp_ctx;Body:Pointer);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 pctx^.context.clear_state;
end;

const
 CONFIG_SPACE_START=$2000;

procedure onSetConfigReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 Assert(pctx^.stream_type=stGfxDcb);

 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=CONFIG_SPACE_START+Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   //
   if p_print_gpu_ops then
   begin
    Writeln(' SET:',getRegName(r),':=0x',HexStr(v,8));
   end;
   //
   pctx^.set_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=CONFIG_SPACE_START+Body^.REG_OFFSET+c-1;
 end;
end;

const
 CONTEXT_REG_BASE=$A000;

procedure onSetContextReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 Assert(pctx^.stream_type=stGfxDcb);

 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   //
   if p_print_gpu_ops then
   begin
    Writeln(' SET:',getRegName(r+CONTEXT_REG_BASE),':=0x',HexStr(v,8));
   end;
   //
   pctx^.set_ctx_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=CONTEXT_REG_BASE+Body^.REG_OFFSET+c-1;
 end;
end;

const
 SH_REG_BASE=$2C00;

procedure onSetShReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 Assert(pctx^.stream_type=stGfxDcb);

 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   //
   if p_print_gpu_ops then
   begin
    Writeln(' SET:',getRegName(r+SH_REG_BASE),':=0x',HexStr(v,8));
   end;
   //
   pctx^.set_sh_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=SH_REG_BASE+Body^.REG_OFFSET+c-1;
 end;
end;

Const
 USERCONFIG_REG_BASE=$C000;

procedure onSetUConfigReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 Assert(pctx^.stream_type=stGfxDcb);

 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=USERCONFIG_REG_BASE+Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   //
   if p_print_gpu_ops then
   begin
    Writeln(' SET:',getRegName(r),':=0x',HexStr(v,8));
   end;
   //
   pctx^.set_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=USERCONFIG_REG_BASE+Body^.REG_OFFSET+c-1;
 end;
end;

procedure onPm40(pctx:p_pfp_ctx;Body:PPM4_TYPE_0_HEADER);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.count;
 if (c<>0) then
 For i:=0 to c-1 do
 begin
  r:=Body^.baseIndex+i;
  v:=PDWORD(@Body[1])[i];
  pctx^.set_reg(r,v);
 end;
end;

procedure onIndexBufferSize(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXBUFFERSIZE);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 pctx^.context.UC_REG.VGT_NUM_INDICES:=Body^.numIndices;
end;

procedure onIndexType(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXTYPE);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 pctx^.context.CX_REG.VGT_DMA_INDEX_TYPE:=Body^.data;
 pctx^.context.UC_REG.VGT_INDEX_TYPE.INDEX_TYPE:=pctx^.context.CX_REG.VGT_DMA_INDEX_TYPE.INDEX_TYPE;
end;

procedure onIndexBase(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXBASE);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 Assert(Body^.baseSelect=0);

 if p_print_gpu_ops then
 begin
  Writeln(' indexBase=',HexStr(PQWORD(@Body^.indexBaseLo)^,10));
 end;

 pctx^.context.CX_REG.VGT_DMA_BASE             :=Body^.indexBaseLo and (not 1);
 pctx^.context.CX_REG.VGT_DMA_BASE_HI.BASE_ADDR:=Body^.indexBaseHi;
end;

procedure onNumInstances(pctx:p_pfp_ctx;Body:PPM4CMDDRAWNUMINSTANCES);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if p_print_gpu_ops then
 begin
  Writeln(' numInstances=',Body^.numInstances);
 end;

 pctx^.context.CX_REG.VGT_DMA_NUM_INSTANCES:=Body^.numInstances;
 pctx^.context.UC_REG.VGT_NUM_INSTANCES    :=Body^.numInstances;
end;

procedure onDrawIndex2(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEX2);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if (DWORD(Body^.drawInitiator)<>0) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 if p_print_gpu_ops then
 begin
  Writeln(' indexBase =',HexStr(PQWORD(@Body^.indexBaseLo)^,10));
  Writeln(' indexCount=',Body^.indexCount);
 end;

 pctx^.context.CX_REG.VGT_DMA_MAX_SIZE         :=Body^.maxSize;
 pctx^.context.CX_REG.VGT_DMA_BASE             :=Body^.indexBaseLo and (not 1);
 pctx^.context.CX_REG.VGT_DMA_BASE_HI.BASE_ADDR:=Body^.indexBaseHi;
 pctx^.context.CX_REG.VGT_DMA_SIZE             :=Body^.indexCount;
 pctx^.context.UC_REG.VGT_NUM_INDICES          :=Body^.indexCount;
 pctx^.context.CX_REG.VGT_DRAW_INITIATOR       :=Body^.drawInitiator;

 pctx^.context.ShaderDrawParams:=Default(t_shader_draw_params);

 pctx^.stream[stGfxDcb].DrawIndex2(@pctx^.context);

 //FlushAndWaitMe(pctx);
end;

procedure onDrawIndexOffset2(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXOFFSET2);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if (DWORD(Body^.drawInitiator)<>0) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 pctx^.context.CX_REG.VGT_DMA_MAX_SIZE         :=Body^.maxSize;
 pctx^.context.CX_REG.VGT_DMA_SIZE             :=Body^.indexCount;
 pctx^.context.UC_REG.VGT_NUM_INDICES          :=Body^.indexCount;
 pctx^.context.CX_REG.VGT_DRAW_INITIATOR       :=Body^.drawInitiator;

 pctx^.context.ShaderDrawParams:=Default(t_shader_draw_params);

 pctx^.stream[stGfxDcb].DrawIndexOffset2(@pctx^.context,
                                         Body^.indexOffset);

 //FlushAndWaitMe(pctx);
end;

procedure onDrawIndexAuto(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXAUTO);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if (DWORD(Body^.drawInitiator)<>2) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 if p_print_gpu_ops then
 begin
  Writeln(' indexCount=',Body^.indexCount);
 end;

 pctx^.context.CX_REG.VGT_DMA_SIZE      :=Body^.indexCount;
 pctx^.context.UC_REG.VGT_NUM_INDICES   :=Body^.indexCount;
 pctx^.context.CX_REG.VGT_DRAW_INITIATOR:=Body^.drawInitiator;

 pctx^.context.ShaderDrawParams:=Default(t_shader_draw_params);

 pctx^.stream[stGfxDcb].DrawIndexAuto(@pctx^.context);

 //FlushAndWaitMe(pctx);
end;

procedure onDrawIndexIndirect(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXINDIRECT);
var
 dataOffset:DWORD;
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if (DWORD(Body^.drawInitiator)<>0) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 if p_print_gpu_ops then
 begin
  Writeln(' BASE_ADDR_DRAW_INDIRECT=0x',HexStr(pctx^.context.BASE_ADDR_DRAW_INDIRECT,16));
  Writeln(' dataOffset             =',Body^.dataOffset);
  Writeln(' baseVtxLoc             =',getRegName(SH_REG_BASE+Body^.baseVtxLoc  ));
  Writeln(' startInstLoc           =',getRegName(SH_REG_BASE+Body^.startInstLoc));
 end;

 pctx^.context.CX_REG.VGT_DRAW_INITIATOR:=Body^.drawInitiator;

 dataOffset:=Body^.dataOffset and (not 3);

 pctx^.context.DrawIndirect(IT_DRAW_INDEX_INDIRECT,
                            Body^.baseVtxLoc,
                            Body^.startInstLoc,
                            0);

 pctx^.stream[stGfxDcb].DrawIndexIndirect(
  @pctx^.context,
  dataOffset);

end;

procedure onDrawIndexIndirectCountMulti(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXINDIRECTCOUNTMULTI);
var
 dataOffset:DWORD;
 countAddr :QWORD;
 drawIndexLoc:WORD;
begin
 Assert(pctx^.stream_type=stGfxDcb);

 if (DWORD(Body^.drawInitiator)<>0) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 if p_print_gpu_ops then
 begin
  Writeln(' BASE_ADDR_DRAW_INDIRECT=0x',HexStr(pctx^.context.BASE_ADDR_DRAW_INDIRECT,16));
  Writeln(' dataOffset             =',Body^.dataOffset            );
  Writeln(' baseVtxLoc             =',getRegName(SH_REG_BASE+Body^.baseVtxLoc  ));
  Writeln(' startInstLoc           =',getRegName(SH_REG_BASE+Body^.startInstLoc));
  Writeln(' drawIndexLoc           =',getRegName(SH_REG_BASE+Body^.drawIndexLoc));
  Writeln(' countIndirectEnable    =',Body^.countIndirectEnable   );
  Writeln(' drawIndexEnable        =',Body^.drawIndexEnable       );
  Writeln(' count                  =',Body^.count                 );
  Writeln(' countAddr              =0x',HexStr(Body^.countAddr,16));
  Writeln(' stride                 =',Body^.stride                );
 end;

 pctx^.context.CX_REG.VGT_DRAW_INITIATOR:=Body^.drawInitiator;

 dataOffset:=Body^.dataOffset and (not 3);

 countAddr:=0;
 if (Body^.countIndirectEnable<>0) then
 begin
  countAddr:=Body^.countAddr and (not QWORD(3));
 end;

 drawIndexLoc:=0;
 if (Body^.drawIndexEnable<>0) then
 begin
  drawIndexLoc:=Body^.drawIndexLoc;
 end;

 pctx^.context.DrawIndirect(IT_DRAW_INDEX_INDIRECT,
                            Body^.baseVtxLoc,
                            Body^.startInstLoc,
                            drawIndexLoc);

 pctx^.stream[stGfxDcb].DrawIndexIndirectCountMulti(
  @pctx^.context,
  dataOffset,
  Body^.stride,
  Body^.count,
  countAddr);

end;

procedure onDispatchDirect(pctx:p_pfp_ctx;Body:PPM4CMDDISPATCHDIRECT);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 Assert(Body^.header.shaderType=1,'shaderType<>CS');

 if (DWORD(Body^.dispatchInitiator)<>1) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' dispatchInitiator=b',revbinstr(DWORD(Body^.dispatchInitiator),32));
 end;

 if p_print_gpu_ops then
 begin
  Writeln(' dim=',Body^.dimX,' ',Body^.dimY,' ',Body^.dimZ);
 end;

 pctx^.context.SC_REG.COMPUTE_DIM_X:=Body^.dimX;
 pctx^.context.SC_REG.COMPUTE_DIM_Y:=Body^.dimY;
 pctx^.context.SC_REG.COMPUTE_DIM_Z:=Body^.dimZ;
 pctx^.context.SC_REG.COMPUTE_DISPATCH_INITIATOR:=Body^.dispatchInitiator;

 pctx^.stream[stGfxDcb].DispatchDirect(pctx^.context.SC_REG);
end;

procedure onDispatchIndirect(pctx:p_pfp_ctx;Body:PPM4CMDDISPATCHINDIRECT);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 Assert(Body^.header.shaderType=1,'shaderType<>CS');

 if (DWORD(Body^.dispatchInitiator)<>1) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' dispatchInitiator=b',revbinstr(DWORD(Body^.dispatchInitiator),32));
 end;

 if p_print_gpu_ops then
 begin
  Writeln(' dataOffset=',Body^.dataOffset);
 end;

 pctx^.context.SC_REG.COMPUTE_DISPATCH_INITIATOR:=Body^.dispatchInitiator;

 pctx^.stream[stGfxDcb].DispatchIndirect(pctx^.context.SC_REG,
                                         pctx^.context.BASE_ADDR_DISPATCH_INDIRECT,
                                         Body^.dataOffset and (not 3));
end;

procedure onPfpSyncMe(pctx:p_pfp_ctx;Body:Pointer);
begin
 Assert(pctx^.stream_type=stGfxDcb);

 //stallCommandBufferParser
 //PFP waits until the ME completes all preceding commands before allowing the next batch to proceed.

 FlushAndWaitMe(pctx);
end;

procedure onMemSemaphore(pctx:p_pfp_ctx;Body:PPM4CMDMEMSEMAPHORE);
begin
 case Body^.semSel of
  MEM_SEMA_SIGNAL:
    pctx^.stream[pctx^.stream_type].SignalSemaphore(
     Pointer(Body^.addr and (not QWORD(7))),
     Body^.signalType,
     Body^.useMailbox);
  MEM_SEMA_WAIT  :
    pctx^.stream[pctx^.stream_type].WaitSemaphore(
     Pointer(Body^.addr and (not QWORD(7))),
     Body^.signalType);
  else
   Assert(false,'onMemSemaphore:'+IntToStr(Body^.semSel));
 end;
end;

procedure onPushMarker(pctx:p_pfp_ctx;Body:PChar;size:Integer);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_PUSH_MARKER:',Body);
 end;
 pctx^.stream[pctx^.stream_type].Hint('\HINT_PUSH_MARKER:',Body,size);
end;

procedure onPopMarker(pctx:p_pfp_ctx);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_POP_MARKER');
 end;
 pctx^.stream[pctx^.stream_type].Hint('\HINT_POP_MARKER','',0);
end;

procedure onSetMarker(pctx:p_pfp_ctx;Body:PChar;size:Integer);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_SET_MARKER:',Body);
 end;
 pctx^.stream[pctx^.stream_type].Hint('\HINT_SET_MARKER:',Body,size);
end;

procedure onMarker(pctx:p_pfp_ctx;Body:PChar;size:Integer);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_MARKER');
 end;
 pctx^.stream[pctx^.stream_type].Hint('\HINT_MARKER','',0);
end;

procedure onWidthHeight(Body:PWORD);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_',Body[0],'_',Body[1]);
 end;
end;

procedure onPrepareFlipLabel(Body:PPM4PrepareFlip);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_PREPARE_FLIP_LABEL:0x',HexStr(Body^.ADDRES,16),':',HexStr(Body^.DATA,8));
 end;
end;

procedure onPrepareFlipWithEopInterrupt(Body:PPM4PrepareFlipWithEopInterrupt);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT:0x',HexStr(Body^.ADDRES,16),':',HexStr(Body^.DATA,8));
 end;
end;

procedure onPrepareFlipWithEopInterruptLabel(Body:PPM4PrepareFlipWithEopInterrupt);
begin
 if p_print_gpu_hint then
 begin
  Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL:0x',HexStr(Body^.ADDRES,16),':',HexStr(Body^.DATA,8));
 end;
end;

procedure onNop(pctx:p_pfp_ctx;Body:PDWORD);
begin

 case pctx^.stream_type of
  stGfxDcb,
  stGfxCcb:
    begin

     Case pctx^.LastSetReg of
      mmPA_SC_SCREEN_SCISSOR_BR,

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

      mmDB_STENCIL_CLEAR,
      //mmDB_RENDER_CONTROL,

      mmDB_HTILE_SURFACE:
       begin
        onWidthHeight(@Body[1]);
        Exit;
       end;
      else;
     end;

    end;
  else;
 end;

 case Body[1] of

  OP_HINT_PUSH_MARKER:
   begin
    onPushMarker(pctx,@Body[2],PM4_LENGTH(Body[0]) - 8);
   end;

  OP_HINT_POP_MARKER:
   begin
    onPopMarker(pctx);
   end;

  OP_HINT_SET_MARKER:
   begin
    onSetMarker(pctx,@Body[2],PM4_LENGTH(Body[0]) - 8);
   end;

  OP_HINT_MARKER:
   begin
    onMarker(pctx,@Body[2],PM4_LENGTH(Body[0]) - 8);
   end;

  OP_HINT_PREPARE_FLIP_LABEL:
   begin
    onPrepareFlipLabel(@Body[2]);
   end;

  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID:
   begin
    onPrepareFlipWithEopInterrupt(@Body[2]);
   end;

  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL:
   begin
    onPrepareFlipWithEopInterruptLabel(@Body[2]);
   end;

  else
   if p_print_gpu_hint then
   begin
    Writeln('\HINT_',get_hint_name(Body[1]));
   end;
 end;
end;

procedure onIndirectBufferDcb(pctx:p_pfp_ctx;Body:PPM4CMDINDIRECTBUFFER);
var
 curr_ibuf:p_pm4_ibuffer;
 ibuf:t_pm4_ibuffer;
 i:Integer;
begin
 if p_print_gpu_ops then
 begin
  Writeln('[DCB]INDIRECT_BUFFER 0x',HexStr(Body^.ibBase,10));
 end;

 if pm4_ibuf_init(@ibuf,Body,@pm4_parse_dcb,pctx^.stream_type) then
 begin
  curr_ibuf:=pctx^.curr_ibuf;

  i:=pm4_ibuf_parse(pctx,@ibuf);

  if (i<>0) then
  begin
   pctx^.add_stall(@ibuf);
  end;

  pctx^.curr_ibuf:=curr_ibuf;
 end;
end;

function pm4_parse_dcb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     if p_print_gpu_ops then Writeln('PM4_TYPE_0 len:',PM4_LENGTH(token));
     onPm40(pctx,buff);
    end;
  2:begin //PM4_TYPE_2
     if p_print_gpu_ops then Writeln('PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     if p_print_gpu_ops then
     if (PM4_TYPE_3_HEADER(token).opcode<>IT_NOP) or
        (not p_print_gpu_hint) then
     begin
      Writeln('IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),
                ' ',ShdrType[PM4_TYPE_3_HEADER(token).shaderType],
              ' len:',PM4_LENGTH(token));
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_NOP                            :onNop                        (pctx,buff);
      IT_WRITE_DATA                     :onWriteData                  (pctx,buff);
      IT_EVENT_WRITE                    :onEventWrite                 (pctx,buff);
      IT_EVENT_WRITE_EOP                :onEventWriteEop              (pctx,buff);
      IT_EVENT_WRITE_EOS                :onEventWriteEos              (pctx,buff);
      IT_DMA_DATA                       :onDmaData                    (pctx,buff);
      IT_WAIT_REG_MEM                   :onWaitRegMem                 (pctx,buff);
      IT_ACQUIRE_MEM                    :onAcquireMem                 (pctx,buff);
      IT_CONTEXT_CONTROL                :onContextControl             (pctx,buff);
      IT_DRAW_PREAMBLE                  :onDrawPreamble               (pctx,buff);
      IT_CLEAR_STATE                    :onClearState                 (pctx,buff);
      IT_SET_CONFIG_REG                 :onSetConfigReg               (pctx,buff);
      IT_SET_CONTEXT_REG                :onSetContextReg              (pctx,buff);
      IT_SET_SH_REG                     :onSetShReg                   (pctx,buff);
      IT_SET_UCONFIG_REG                :onSetUConfigReg              (pctx,buff);
      IT_INDEX_BUFFER_SIZE              :onIndexBufferSize            (pctx,buff);
      IT_INDEX_TYPE                     :onIndexType                  (pctx,buff);
      IT_INDEX_BASE                     :onIndexBase                  (pctx,buff);
      IT_NUM_INSTANCES                  :onNumInstances               (pctx,buff);
      IT_DRAW_INDEX_2                   :onDrawIndex2                 (pctx,buff);
      IT_DRAW_INDEX_OFFSET_2            :onDrawIndexOffset2           (pctx,buff);
      IT_DRAW_INDEX_AUTO                :onDrawIndexAuto              (pctx,buff);
      IT_DRAW_INDEX_INDIRECT            :onDrawIndexIndirect          (pctx,buff);
      IT_DRAW_INDEX_INDIRECT_COUNT_MULTI:onDrawIndexIndirectCountMulti(pctx,buff);
      IT_DISPATCH_DIRECT                :onDispatchDirect             (pctx,buff);
      IT_DISPATCH_INDIRECT              :onDispatchIndirect           (pctx,buff);
      IT_PFP_SYNC_ME                    :onPfpSyncMe                  (pctx,buff);
      IT_MEM_SEMAPHORE                  :onMemSemaphore               (pctx,buff);

      IT_SET_BASE                       :onSetBase                    (pctx,buff);
      IT_SET_PREDICATION                :onSetPredication             (pctx,buff);

      IT_INCREMENT_DE_COUNTER           :onIncrementDECounter         (pctx,buff);
      IT_WAIT_ON_CE_COUNTER             :onWaitOnCECounter            (pctx,buff);

      IT_INDIRECT_BUFFER                :onIndirectBufferDcb          (pctx,buff);

      else
       begin
        Writeln(stderr,'[DCB]PM4_TYPE_3.opcode:',get_op_name(PM4_TYPE_3_HEADER(token).opcode));
        Assert (False ,'[DCB]PM4_TYPE_3.opcode:'+get_op_name(PM4_TYPE_3_HEADER(token).opcode));
       end;
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_SET_CONFIG_REG :;
      IT_SET_CONTEXT_REG:;
      IT_SET_SH_REG     :;
      IT_SET_UCONFIG_REG:;
      else
       pctx^.LastSetReg:=0;
     end;


    end;
  else
   begin
    Writeln(stderr,'[DCB]PM4_TYPE_',PM4_TYPE(token));
    Assert (False ,'[DCB]PM4_TYPE_'+IntToStr(PM4_TYPE(token)));
   end;
 end;

end;

procedure onSetShRegCompute(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   //
   if p_print_gpu_ops then
   begin
    Writeln(' [ASC]SET:',getRegName(r+$2C00),':=0x',HexStr(v,8));
   end;
   //
   pctx^.set_asc_reg(r,v);
  end;
  //
 end;
end;

procedure onDispatchDirectCompute(pctx:p_pfp_ctx;Body:PPM4CMDDISPATCHDIRECT);
var
 c_id:Byte;
begin
 Assert(Body^.header.shaderType=1,'shaderType<>CS');

 if (DWORD(Body^.dispatchInitiator)<>1) then
 if p_print_gpu_ops then
 begin
  Writeln(stderr,' dispatchInitiator=b',revbinstr(DWORD(Body^.dispatchInitiator),32));
 end;

 c_id:=pctx^.curr_ibuf^.c_id;

 pctx^.context.ASC_COMPUTE[c_id].COMPUTE_DIM_X:=Body^.dimX;
 pctx^.context.ASC_COMPUTE[c_id].COMPUTE_DIM_Y:=Body^.dimY;
 pctx^.context.ASC_COMPUTE[c_id].COMPUTE_DIM_Z:=Body^.dimZ;
 pctx^.context.ASC_COMPUTE[c_id].COMPUTE_DISPATCH_INITIATOR:=Body^.dispatchInitiator;

 pctx^.stream[pctx^.stream_type].DispatchDirect(pctx^.context.ASC_COMPUTE[c_id]);
end;

procedure onReleaseMemCompute(pctx:p_pfp_ctx;Body:PPM4CMDRELEASEMEM);
begin
 Case Body^.eventType of
  CS_DONE,
  CACHE_FLUSH_TS,               //FlushCbDbCache
  CACHE_FLUSH_AND_INV_TS_EVENT, //FlushAndInvalidateCbDbCaches
  BOTTOM_OF_PIPE_TS,            //CbDbReadsDone
  FLUSH_AND_INV_DB_DATA_TS,     //FlushAndInvalidateDbCache
  FLUSH_AND_INV_CB_DATA_TS:;    //FlushAndInvalidateCbCache
  else
   Assert(False,'ReleaseMem: eventType=0x'+HexStr(Body^.eventType,1));
 end;

 case Body^.eventIndex of
  EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP:;
  EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP:;
  else
   Assert(False,'ReleaseMem: eventIndex=0x'+HexStr(Body^.eventIndex,1));
 end;

 DWORD(pctx^.context.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 if p_print_gpu_ops then
 begin
  Case Body^.eventType of
   CS_DONE,
   CACHE_FLUSH_TS              :Writeln(' eventType  =','FlushCbDbCache');
   CACHE_FLUSH_AND_INV_TS_EVENT:Writeln(' eventType  =','FlushAndInvalidateCbDbCaches');
   BOTTOM_OF_PIPE_TS           :Writeln(' eventType  =','CbDbReadsDone');
   FLUSH_AND_INV_DB_DATA_TS    :Writeln(' eventType  =','FlushAndInvalidateDbCache');
   FLUSH_AND_INV_CB_DATA_TS    :Writeln(' eventType  =','FlushAndInvalidateCbCache');
   else;
  end;

  Writeln(' interrupt  =0x',HexStr(Body^.intSel,2));
  Writeln(' srcSelector=0x',HexStr(Body^.dataSel,2));
  Writeln(' dstSelector=0x',HexStr(Body^.dstSel,2));
  Writeln(' dstGpuAddr =0x',HexStr(Body^.address,10));
  Writeln(' immValue   =0x',HexStr(Body^.data,16));
 end;

 pctx^.stream[pctx^.stream_type].ReleaseMem(Pointer(Body^.address),Body^.data,Body^.eventType,Body^.dataSel,Body^.dstSel,Body^.intSel);

 pctx^.Flush_stream(pctx^.stream_type);
end;

procedure onIndirectBufferCompute(pctx:p_pfp_ctx;Body:PPM4CMDINDIRECTBUFFER);
var
 curr_ibuf:p_pm4_ibuffer;
 ibuf:t_pm4_ibuffer;
 i:Integer;
begin
 if p_print_gpu_ops then
 begin
  Writeln('[ASC]INDIRECT_BUFFER (CS) 0x',HexStr(Body^.ibBase,10));
 end;

 if pm4_ibuf_init(@ibuf,Body,@pm4_parse_compute_ring,pctx^.stream_type) then
 begin
  curr_ibuf:=pctx^.curr_ibuf;

  i:=pm4_ibuf_parse(pctx,@ibuf);

  if (i<>0) then
  begin
   pctx^.add_stall(@ibuf);
  end;

  pctx^.curr_ibuf:=curr_ibuf;
 end;
end;

function pm4_parse_compute_ring(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
var
 ibuf:t_pm4_ibuffer;
 i:Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     if p_print_gpu_ops then Writeln('[ASC]PM4_TYPE_0 len:',PM4_LENGTH(token));
     onPm40(pctx,buff);
    end;
  2:begin //PM4_TYPE_2
     if p_print_gpu_ops then Writeln('[ASC]PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     if p_print_gpu_ops then
     if (PM4_TYPE_3_HEADER(token).opcode<>IT_NOP) or
        (not p_print_gpu_hint) then
     begin
      Writeln('[ASC]IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),
                ' ',ShdrType[PM4_TYPE_3_HEADER(token).shaderType],
              ' len:',PM4_LENGTH(token));
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_NOP                            :onNop                  (pctx,buff);
      IT_WRITE_DATA                     :onWriteData            (pctx,buff);
      IT_DMA_DATA                       :onDmaData              (pctx,buff);
      IT_SET_SH_REG                     :onSetShRegCompute      (pctx,buff);
      IT_DISPATCH_DIRECT                :onDispatchDirectCompute(pctx,buff);
      IT_RELEASE_MEM                    :onReleaseMemCompute    (pctx,buff);
      IT_WAIT_REG_MEM                   :onWaitRegMem           (pctx,buff);
      IT_ACQUIRE_MEM                    :onAcquireMem           (pctx,buff);
      IT_INDIRECT_BUFFER                :onIndirectBufferCompute(pctx,buff);

      IT_MEM_SEMAPHORE                  :onMemSemaphore         (pctx,buff);

      IT_SET_QUEUE_REG                  :Writeln('SET_QUEUE_REG:Skip');

      else
       begin
        Writeln(stderr,'[ASC]PM4_TYPE_3.opcode:',get_op_name(PM4_TYPE_3_HEADER(token).opcode));
        Assert (False ,'[ASC]PM4_TYPE_3.opcode:'+get_op_name(PM4_TYPE_3_HEADER(token).opcode));
       end;
     end;

    end;
  else
   begin
    Writeln(stderr,'[ASC]PM4_TYPE_',PM4_TYPE(token));
    Assert (False ,'[ASC]PM4_TYPE_'+IntToStr(PM4_TYPE(token)));
   end;
 end;

end;




end.

