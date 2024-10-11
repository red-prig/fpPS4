unit dev_gc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure gc_initialize();

function  gc_add_internal_ptr   (kq,ptr,udata:Pointer):Integer; register;
function  gc_del_internal_ptr   (kq,ptr:Pointer):Integer;       register;
procedure gc_wakeup_internal_ptr(ptr:Pointer);                  register;

var
 sync_me_submit:Boolean=False; //forced wait for all tasks to complete on the GPU side after submit

implementation

uses
 errno,
 kern_mtx,
 sys_event,
 kern_event,
 sched_ule,
 kern_authinfo,
 vm,
 vmparam,
 vm_pmap,
 sys_vm_object,
 vm_pager,
 vm_map,
 vm_mmap,
 kern_rwlock,
 kern_proc,
 kern_thr,
 time,
 md_sleep,
 pm4defs,
 pm4_ring,
 pm4_stream,
 pm4_pfp,
 pm4_me,

 dev_dce,

 vDevice,
 vMemory,

 sys_bootparam,

 subr_backtrace;

var
 gc_page:PDWORD; //SceGnmDingDongArea

 gc_submits_allowed_vaddr:PInteger=nil; //0=true,1=false (0xfe0100000)
 gc_submits_allowed_vmirr:PInteger=nil;

 gc_knlock:mtx;
 gc_knlist:t_knlist;

procedure unmap_dmem_gc(start,__end:DWORD); public;
begin
 if (MemManager<>nil) then
 begin
  MemManager.unmap_host(start,__end);
 end;
end;

function mmap_addr(paddr,psize:QWORD;
                   prot:Integer;
                   pout_addr:PQWORD):Integer;
var
 map:vm_map_t;
begin
 if ((psize and $3fff)=0) and ((prot and $33)=prot) then
 begin
  map:=@p_vmspace(p_proc.p_vmspace)^.vm_map;

  if (paddr=0) and ((g_appinfo.mmap_flags and 2)<>0) then
  begin
   paddr:=QWORD($fc0000000);
  end;

  Result:=vm_mmap2(map,
                   @paddr,psize,
                   prot,prot,
                   MAP_ANON or MAP_SYSTEM or MAP_SHARED,OBJT_DEFAULT,
                   nil,0);

  if (Result=0) then
  begin
   vm_map_set_name(map,paddr,paddr+psize,'SceAppCommArea');

   pout_addr^:=paddr;
  end;

 end else
 begin
  Result:=EINVAL;
 end;
end;

type
 p_SetGsRingSizes=^t_SetGsRingSizes;
 t_SetGsRingSizes=packed record
  esgsRingSize:DWORD;
  gsvsRingSize:DWORD;
  zero        :DWORD;
 end;

 p_SetMipStatsReport=^t_SetMipStatsReport;
 t_SetMipStatsReport=packed record
  p_type:DWORD;
  param1:DWORD;
  param2:DWORD;
  param3:DWORD;
 end;

 p_submit_args=^t_submit_args;
 t_submit_args=packed record
  pid  :DWORD;
  count:DWORD;
  cmds :PQWORD;
  eop_v:QWORD;
  wait :Integer;
 end;

 p_map_compute_queue_args=^t_map_compute_queue_args;
 t_map_compute_queue_args=packed record
  pipeHi         :DWORD;
  pipeLo         :DWORD;
  queueId        :DWORD;
  g_queueId      :DWORD;
  ringBaseAddress:Pointer;
  readPtrAddress :Pointer;
  dingDongPtr    :Pointer;
  lenLog2        :DWORD;
  pipePriority   :DWORD;
 end;

 p_unmap_compute_queue_args=^t_unmap_compute_queue_args;
 t_unmap_compute_queue_args=packed record
  pipeHi :DWORD;
  pipeLo :DWORD;
  queueId:DWORD;
 end;

 p_ding_dong_args=^t_ding_dong_args;
 t_ding_dong_args=packed record
  pipeHi             :DWORD;
  pipeLo             :DWORD;
  queueId            :DWORD;
  nextStartOffsetInDw:DWORD;
 end;

var
 ring_gfx      :t_pm4_ring;
 ring_gfx_lock :Pointer=nil;

 ring_watchdog :PRTLEvent=nil;
 watchdog_label:QWORD=0;

 GC_SRI_event:PRTLEvent=nil;
 GC_SRI_label:QWORD=0;

 me_idle_event:PRTLEvent=nil;
 me_idle_label:QWORD=0;

 parse_gfx_started:Pointer=nil;
 parse_gfx_td:p_kthread;

 pfp_ctx:t_pfp_ctx;

 pm4_me_gfx:t_pm4_me;

var
 map_queue_valid:QWORD=0;
 map_queue_hqd  :array[0..63] of t_gc_hqd;

 //asc_queues

//gfx ring only
procedure onEventWriteEop(pctx:p_pfp_ctx;Body:PPM4CMDEVENTWRITEEOP);
var
 submit_id:QWORD;
begin
 submit_id:=Body^.DATA;

 if p_print_gpu_ops then
 begin
  Writeln('[R]IT_EVENT_WRITE_EOP=0x',HexStr(submit_id,16),' ',Body^.intSel);
 end;

 pctx^.stream[stGfxDcb].SubmitFlipEop(Body^.DATA,Body^.intSel);
end;

function pm4_parse_gfx_ring(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
var
 ibuf:t_pm4_ibuffer;
 i:Integer;
begin
 Result:=0;

 case token of
  $c0023300:
   begin
    if p_print_gpu_ops then
    begin
     Writeln('[R]INDIRECT_BUFFER (ccb) 0x',HexStr(PPM4CMDINDIRECTBUFFER(buff)^.ibBase,10));
    end;
    if pm4_ibuf_init(@ibuf,buff,@pm4_parse_ccb,stGfxCcb) then
    begin
     i:=pm4_ibuf_parse(pctx,@ibuf);
     if (i<>0) then
     begin
      pctx^.add_stall(@ibuf);
     end;

     //pm4_me_gfx.Push(pfp_ctx.stream[stGfxCcb]);
    end;
   end;
  $c0023f00:
   begin
    if p_print_gpu_ops then
    begin
     Writeln('[R]INDIRECT_BUFFER (dcb) 0x',HexStr(PPM4CMDINDIRECTBUFFER(buff)^.ibBase,10));
    end;
    if pm4_ibuf_init(@ibuf,buff,@pm4_parse_dcb,stGfxDcb) then
    begin
     i:=pm4_ibuf_parse(pctx,@ibuf);
     if (i<>0) then
     begin
      pctx^.add_stall(@ibuf);
     end;

     //pm4_me_gfx.Push(pfp_ctx.stream[stGfxDcb]);
    end;
   end;
  $c0008b00:
   begin
    if p_print_gpu_ops then
    begin
     Writeln('[R]SWITCH_BUFFER');
    end;
   end;
  $C0044700: //IT_EVENT_WRITE_EOP
   begin
    onEventWriteEop(pctx,buff);
   end;
  else
   begin
    Assert(False);
   end;
 end;

end;

function get_compute_stream_type(c_id:DWORD):t_pm4_stream_type; inline;
begin
 Result:=t_pm4_stream_type(ord(stCompute0) + (c_id div 8)); //pipe id
end;

procedure gc_idle; forward;

procedure parse_gfx_ring(parameter:pointer); SysV_ABI_CDecl;
var
 buff:Pointer;
 i,size:DWORD;

 ibuf:t_pm4_ibuffer;
 buft:t_pm4_stream_type;

 base_guest_addr:Pointer;
 bits:QWORD;
 c_id:DWORD;
 p_id:DWORD;
 send:DWORD;
begin
 sched_prio(curkthread,64);

 if LoadVulkan then
 begin
  InitVulkan;
 end;

 repeat

  if gc_ring_pm4_peek(@ring_gfx,@size,@buff) then
  begin
   //Writeln('packet:0x',HexStr(buff),':',size);

   if pm4_ibuf_init(@ibuf,buff,size,@pm4_parse_gfx_ring,stGfxRing) then
   begin
    i:=pm4_ibuf_parse(@pfp_ctx,@ibuf);

    if (i<>0) then
    begin
     //pm4_me_gfx.Push(pfp_ctx.stream_dcb);
     //pm4_me_gfx.Push(pfp_ctx.stream_ccb);

     pfp_ctx.add_stall(@ibuf);
    end;

   end;

   gc_ring_pm4_drain(@ring_gfx,size);

   //
   for buft:=stGfxDcb to stGfxCcb do
   begin
    pfp_ctx.Flush_stream(buft);
   end;
   //

   Continue;
  end;

  bits:=map_queue_valid;

  if (bits<>0) then
  begin
   //init sended bits
   send:=0;

   while (bits<>0) do
   begin
    c_id:=BsfQWord(bits);

    //start
    rw_wlock(ring_gfx_lock);

    p_id:=2; //double check

    while (p_id<>0) and gc_map_hdq_peek(@map_queue_hqd[c_id],@size,@buff) do
    begin

     //adjust guest addr
     base_guest_addr:=map_queue_hqd[c_id].base_guest_addr + (buff - map_queue_hqd[c_id].base_dmem_addr);

     //
     rw_wunlock(ring_gfx_lock);
     //

     if pm4_ibuf_init(@ibuf,buff,size,@pm4_parse_compute_ring,get_compute_stream_type(c_id),c_id) then
     begin
      //adjust guest addr
      ibuf.base:=base_guest_addr;

      i:=pm4_ibuf_parse(@pfp_ctx,@ibuf);

      if (i<>0) then
      begin
       pfp_ctx.add_stall(@ibuf);
      end;
     end;

     //
     rw_wlock(ring_gfx_lock);
     //

     gc_map_hdq_drain(@map_queue_hqd[c_id],size);

     //set sended bits
     send:=send or (1 shl (c_id div 8)); //by pipe id

     Dec(p_id);
    end; //while

    rw_wunlock(ring_gfx_lock);
    //end

    //clear
    bits:=bits and (not (1 shl c_id));
   end; //while

   if (send<>0) then
   begin

    //
    while (send<>0) do
    begin
     c_id:=BsfDWord(send);

     pfp_ctx.Flush_stream( t_pm4_stream_type(ord(stCompute0) + c_id) );

     //clear
     send:=send and (not (1 shl c_id));
    end;
    //

    Continue;
   end; //(send<>0)

  end; //(bits<>0)

  gc_idle;

  if (watchdog_label=0) then
  begin
   RTLEventWaitFor(ring_watchdog);
  end else
  begin
   msleep_td(hz div 10000);
  end;
 until false;


end;

procedure start_gfx_ring;
begin
 if (System.InterlockedExchange(parse_gfx_started,Pointer(1))=nil) then
 begin
  pfp_ctx.init;
  pfp_ctx.on_flush_stream:=@pm4_me_gfx.Push;

  ring_watchdog:=RTLEventCreate;
  GC_SRI_event :=RTLEventCreate;

  kthread_add(@parse_gfx_ring,nil,@parse_gfx_td,(8*1024*1024) div (16*1024),'[GFX_PFP]');
 end;
end;

procedure retrigger_watchdog;
begin
 if (ring_watchdog<>nil) then
 begin
  watchdog_label:=1; //thread can`t wait
  RTLEventSetEvent(ring_watchdog);
 end;
end;

procedure gc_wait_GC_SRI;
begin
 if (GC_SRI_label=0) then Exit;

 if (GC_SRI_event<>nil) then
 begin
  RTLEventWaitFor(GC_SRI_event);
 end;
end;

procedure gc_idle;
begin
 if (GC_SRI_event<>nil) then
 begin
  RTLEventSetEvent(GC_SRI_event);
 end;
 GC_SRI_label:=0;

 if (gc_submits_allowed_vmirr<>nil) then
 begin
  gc_submits_allowed_vmirr^:=0; //true
 end;
end;

procedure me_idle; register;
begin
 if (me_idle_label=0) then Exit;
 //
 if (me_idle_event<>nil) then
 begin
  RTLEventSetEvent(me_idle_event);
 end;
 //
 me_idle_label:=0;
end;

procedure wait_me_idle;
begin
 //first wait PFP
 if (GC_SRI_event<>nil) then
 begin
  RTLEventWaitFor(GC_SRI_event);
 end;
 //
 me_idle_label:=0; //dont SetEvent
 //
 if (me_idle_event=nil) then
 begin
  me_idle_event:=RTLEventCreate;
 end else
 begin
  RTLEventResetEvent(me_idle_event);
 end;
 //
 me_idle_label:=1; //can SetEvent
 //
 pm4_me_gfx.trigger; //update if wait
 RTLEventWaitFor(me_idle_event);
end;

procedure gc_imdone;
var
 prev:QWORD;
begin
 if (GC_SRI_event=nil)       then Exit;
 if (pm4_me_gfx.started=nil) then Exit;

 gc_wait_GC_SRI;

 prev:=System.InterlockedExchange64(GC_SRI_label,1);

 if (prev=0) then
 begin

  if (gc_submits_allowed_vmirr<>nil) then
  begin
   gc_submits_allowed_vmirr^:=1; //false
  end;

  retrigger_watchdog;
  pm4_me_gfx.imdone;

  watchdog_label:=0; //thread can wait
 end;

end;

Function gc_map_compute_queue(data:p_map_compute_queue_args):Integer;
var
 g_queueId   :DWORD;
 pipeHi      :DWORD;
 pipeLo      :DWORD;
 queueId     :DWORD;
 pipePriority:DWORD;
 id          :DWORD;
begin
 Result:=0;

 g_queueId   :=data^.g_queueId;
 pipeHi      :=data^.pipeHi;
 pipeLo      :=data^.pipeLo;
 queueId     :=data^.queueId;
 pipePriority:=data^.pipePriority;

 //if (not IsDevKit) or

 if (pipeHi  <> $0769c766) or (pipeLo    <> $72e8e3c1) or
    (queueId <> $db72af28) or (g_queueId <> $d245ed58) then
 begin
  //if (not IsDevKit) or (not IsDiag)

  if (pipeHi  <> $e13ec1f1) or (pipeLo    <> $76c0801c) or
     (queueId <> $75c36152) or (g_queueId <> $4be587dd) then
  begin

   if (pipeHi = 2) and
      (pipeLo = 3) then
   begin
    Exit(Integer($804c000a));
   end;

   if (1 < (pipeHi - 1)) then
   begin
    Exit(Integer($804c000b));
   end;

   if (3 < pipeLo) then
   begin
    Exit(Integer($804c000b));
   end;

   if (7 < queueId) then
   begin
    Exit(Integer($804c000b));
   end;

  end else
  begin
   pipeHi   :=2;
   g_queueId:=19;
   pipeLo   :=3;
   queueId  :=3;
  end;
 end else
 begin
  pipeHi   :=2;
  g_queueId:=20;
  queueId  :=4;
  pipeLo   :=3;
 end;

 if (pipePriority >= 2) then
 begin
  Exit(EINVAL);
 end;

 id:=(pipeHi - 1) * 32 + pipeLo * 8 + queueId;

 if ((map_queue_valid and (1 shl id))<>0) then
 begin
  Exit(Integer($804c0012));
 end;

 Result:=gc_map_hqd(data^.ringBaseAddress,
                    data^.readPtrAddress,
                    @gc_page[id],
                    data^.lenLog2,
                    g_queueId,
                    pipePriority,
                    @map_queue_hqd[id]);

 if (Result=0) then
 begin
  map_queue_valid:=map_queue_valid or (1 shl id);

  //what is it?
  id:=(pipeHi - 1) * 4 + (pipeLo - 8);

  map_queue_valid:=map_queue_valid or (1 shl id);
 end;

end;

Function gc_unmap_compute_queue(data:p_unmap_compute_queue_args):Integer;
var
 pipeHi      :DWORD;
 pipeLo      :DWORD;
 queueId     :DWORD;
 id          :DWORD;
begin
 Result:=0;

 pipeHi :=data^.pipeHi;
 pipeLo :=data^.pipeLo;
 queueId:=data^.queueId;

 //if (not IsDevKit) or (not IsDiag)

 if (pipeHi  <> $0769c766) or
    (pipeLo  <> $72e8e3c1) or
    (queueId <> $db72af28) then
 begin

  //if (not IsDevKit) or (not IsDiag)

  if (pipeHi  <> $e13ec1f1) or
     (pipeLo  <> $76c0801c) or
     (queueId <> $75c36152) then
  begin

   if (pipeHi = 2) and
      (pipeLo = 3) then
   begin
    Exit(Integer($804c000a));
   end;

   if (1 < (pipeHi - 1)) then
   begin
    Exit(Integer($804c000b));
   end;

   if (3 < pipeLo) then
   begin
    Exit(Integer($804c000b));
   end;

   if (7 < queueId) then
   begin
    Exit(Integer($804c000b));
   end;

  end else
  begin
   pipeLo :=3;
   pipeHi :=2;
   queueId:=3;
  end;
 end else
 begin
  queueId:=4;
  pipeLo :=3;
  pipeHi :=2;
 end;

 id:=(pipeHi - 1) * 32 + pipeLo * 8 + queueId;

 if ((map_queue_valid and (1 shl id))<>0) then
 begin

  gc_unmap_hqd(@map_queue_hqd[id]);

  map_queue_valid:=map_queue_valid and (not (1 shl id));

 end;
end;

Function gc_ding_dong(data:p_ding_dong_args):Integer;
var
 pipeHi :DWORD;
 pipeLo :DWORD;
 queueId:DWORD;
 id     :DWORD;
begin
 Result:=0;

 pipeHi :=data^.pipeHi;
 pipeLo :=data^.pipeLo;
 queueId:=data^.queueId;

 //if (not IsDevKit) or (not IsDiag)

 if (pipeHi  <> $0769c766) or
    (pipeLo  <> $72e8e3c1) or
    (queueId <> $db72af28) then
 begin

  if (pipeHi = 2) and
     (pipeLo = 3) then
  begin
   Exit(Integer($804c000a));
  end;

  if (1 < (pipeHi - 1)) then
  begin
   Exit(Integer($804c000b));
  end;

  if (3 < pipeLo) then
  begin
   Exit(Integer($804c000b));
  end;

  if (7 < queueId) then
  begin
   Exit(Integer($804c000b));
  end;

 end else
 begin
  queueId:=4;
  pipeLo :=3;
  pipeHi :=2;
 end;

 id:=(pipeHi - 1) * 32 + pipeLo * 8 + queueId;

 if ((map_queue_valid and (1 shl id))=0) then
 begin
  Exit(Integer($804c0001));
 end;

 Result:=gc_map_hdq_ding_dong(@map_queue_hqd[id],
                              data^.nextStartOffsetInDw);

end;

Function gc_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 vaddr:QWORD;
begin
 Result:=0;

 Writeln('gc_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $C0108120: //call in neo mode (Tca)
            begin
             Exit(19);
            end;

  $C004811F: //sceGnmGetNumTcaUnits -> 0x01
             //ext_gpu_timer        -> 0x08
            begin
             PInteger(data)^:=0;
            end;

  $C00C8110: //sceGnmSetGsRingSizes
            begin
             Writeln('SetGsRingSizes(0x',HexStr(p_SetGsRingSizes(data)^.esgsRingSize,8),',0x'
                                        ,HexStr(p_SetGsRingSizes(data)^.gsvsRingSize,8),')');
             pfp_ctx.set_esgs_gsvs_ring_size(p_SetGsRingSizes(data)^.esgsRingSize,
                                             p_SetGsRingSizes(data)^.gsvsRingSize);
            end;

  $C0848119: //*MipStatsReport
            begin
             case PInteger(data)^ of
              $10001:
                     begin
                      Writeln('MipStatsReport(0x',HexStr(p_SetMipStatsReport(data)^.param1,8),',0x'
                                                 ,HexStr(p_SetMipStatsReport(data)^.param2,8),',0x'
                                                 ,HexStr(p_SetMipStatsReport(data)^.param3,8),')');
                     end;

              $18001:; //diag?

              else
               Exit(EINVAL);
             end;
            end;
  $C008811B: //sceGnmAreSubmitsAllowed
            begin
             if (gc_submits_allowed_vaddr=nil) then
             begin
              vaddr:=0;
              Result:=mmap_addr($fe0100000,$4000,1,@vaddr);

              if (Result<>0) then
              begin
               Result:=ENOMEM;
               Exit;
              end;

              gc_submits_allowed_vaddr:=Pointer(vaddr);
              gc_submits_allowed_vmirr:=mirror_map(vaddr,$4000);
             end;

             PPointer(data)^:=gc_submits_allowed_vaddr;

             gc_submits_allowed_vmirr^:=0; //init true
            end;

  $C010810B: //get cu mask
            begin
             PInteger(data)[0]:=$10; //& 0x3ff   GC SE0 Redundant CU: 0x10
             PInteger(data)[1]:=$10; //& 0x3ff   GC SE1 Redundant CU: 0x10
             PInteger(data)[2]:=$00; //& 0x3ff   GC SE2 Redundant CU: 0x00
             PInteger(data)[3]:=$00; //& 0x3ff   GC SE3 Redundant CU: 0x00
            end;

  $C0048116: //sceGnmSubmitDone
            begin
             Writeln('sceGnmSubmitDone');

             gc_imdone;
            end;

  $C0048117: //wait idle
            begin
             Writeln('gc_wait_idle');

             gc_wait_GC_SRI;
            end;

  $C0048114: //sceGnmFlushGarlic
            begin
             Writeln('sceGnmFlushGarlic');

             MemManager.Flush;
            end;

  $C0108102: //submit
            begin
             start_gfx_ring;

             rw_wlock(ring_gfx_lock);

              Result:=gc_submit_internal(@ring_gfx,
                                         p_submit_args(data)^.count,
                                         p_submit_args(data)^.cmds);

             rw_wunlock(ring_gfx_lock);

             if (Result=0) then
             begin
              retrigger_watchdog;

              if sync_me_submit then
              begin
               //force wait GPU idle
               wait_me_idle;
              end;
              //msleep_td(hz);
             end;

            end;

  $C020810C: //submit eop
            begin
             start_gfx_ring;

             rw_wlock(ring_gfx_lock);

              Result:=gc_submit_internal(@ring_gfx,
                                         p_submit_args(data)^.count,
                                         p_submit_args(data)^.cmds);

              if (Result=0) then
              begin
               {
                The original data is an incremental "submit_id | (vmid << 32)",
                now this is directly sended "eop_v"
               }

               Writeln('submit_eop=0x',HexStr(p_submit_args(data)^.eop_v,16),' ',p_submit_args(data)^.wait);

               Result:=gc_pm4_event_write_eop(@ring_gfx,
                                              nil,
                                              p_submit_args(data)^.eop_v,
                                              1,
                                              p_submit_args(data)^.wait
                                              );
              end;

             rw_wunlock(ring_gfx_lock);

             if (Result=0) then
             begin
              retrigger_watchdog;
             end;
            end;

  $C0088101: //switch_buffer
            begin
             start_gfx_ring;

             rw_wlock(ring_gfx_lock);

              Result:=gc_switch_buffer_internal(@ring_gfx);

             rw_wunlock(ring_gfx_lock);

             if (Result=0) then
             begin
              retrigger_watchdog;
             end;
            end;

  $C030810D: //sceGnmMapComputeQueue
            begin
             rw_wlock(ring_gfx_lock);

              //reset prio
              p_map_compute_queue_args(data)^.pipePriority:=0;

              Result:=gc_map_compute_queue(data);

             rw_wunlock(ring_gfx_lock);

             if (Result=0) then
             begin
              retrigger_watchdog;
             end;
            end;

  $C030811A: //sceGnmMapComputeQueueWithPriority
            begin
             rw_wlock(ring_gfx_lock);

              Result:=gc_map_compute_queue(data);

             rw_wunlock(ring_gfx_lock);

             if (Result=0) then
             begin
              retrigger_watchdog;
             end;
            end;

  $C00C810E: //sceGnmUnmapComputeQueue
            begin
             rw_wlock(ring_gfx_lock);

              Result:=gc_unmap_compute_queue(data);

             rw_wunlock(ring_gfx_lock);
            end;

  $C010811C: //sceGnmDingDong
            begin
             start_gfx_ring;

             rw_wlock(ring_gfx_lock);

              Result:=gc_ding_dong(data);

             rw_wunlock(ring_gfx_lock);

             if (Result=0) then
             begin
              retrigger_watchdog;
             end;
            end;

  else
   begin
    print_error_td('gc_ioctl(0x'+HexStr(cmd,8)+')');
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

Function gc_mmap_single(cdev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;objp:p_vm_object_t;nprot:Integer):Integer;
var
 obj:vm_object_t;
begin
 if sceSblACMgrHasUseHp3dPipeCapability(@g_authinfo) then
 begin
  Exit(EINVAL);
 end;

 if (offset^>=PAGE_SIZE) then
 begin
  Exit(EPERM);
 end;

 if (size<>PAGE_SIZE) then
 begin
  Exit(EINVAL);
 end;

 obj:=vm_pager_allocate(OBJT_DEVICE,cdev,PAGE_SIZE,nprot,offset^);
 obj^.flags:=obj^.flags or OBJ_DMEM_EXT;
 obj^.un_pager.map_base:=gc_page;

 if (obj=nil) then
 begin
  Exit(EINVAL);
 end;

 objp^:=obj;

 Result:=0;
end;

Function gc_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
begin
 if sceSblACMgrHasUseHp3dPipeCapability(@g_authinfo) then
 begin
  Exit(EINVAL);
 end;

 if (offset>=PAGE_SIZE) then
 begin
  Exit(EPERM);
 end;

 paddr^:=offset + (QWORD(gc_page)-VM_MIN_GPU_ADDRESS);
 memattr^:=0;

 Result:=0;
end;

const
 gc_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'gc';
  d_open        :nil;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :nil;
  d_write       :nil;
  d_ioctl       :@gc_ioctl;
  d_poll        :nil;
  d_mmap        :@gc_mmap;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :nil;
  d_purge       :nil;
  d_mmap_single :@gc_mmap_single;
  d_mmap_single2:nil;
 );

{
 event_id
  (SourceID == 0xb5)
   0x00 -> Compute0
   0x01 -> Compute1
   0x02 -> Compute2
   0x03 -> Compute3
   0x04 -> Compute4
   0x05 -> Compute5
   0x06 -> Compute6
   0x40 -> GfxEop
   0x41 -> GfxEop (meid ????)

  0x84 -> set timer hz

  (SourceID == 0xef)
   0x83,
   0x85,
   0x86 -> (dbggc)
}

function filterops_graphics_core_attach(kn:p_knote):Integer;
var
 kn_sdata:QWORD;
 event_id:Byte;
 cap:Boolean;
 me_id:Byte;
begin
 Result:=0;

 event_id:=Byte(kn^.kn_sdata); //kev.ident = kev.data

 cap:=sceSblACMgrHasUseHp3dPipeCapability(@g_authinfo);

 me_id:=0;

 case event_id of
  $00..$41,
  $80..$86:
   begin

    if (not cap) then
    begin
     if (event_id=$84) or (event_id=$41) then Exit(EPERM);
    end else
    begin
     if (event_id=$40) then Exit(EPERM);
    end;

    kn_sdata:=kn^.kn_sdata;

    case event_id of
     $82..$86:kn_sdata:=(kn_sdata and QWORD($ffffffffffff0000)) or
                        QWORD(event_id);
      else
              kn_sdata:=(kn_sdata and QWORD($ffffffffffff0000)) or
                        QWORD(event_id) or
                        QWORD((me_id and $ff) shl 8);
    end;

    kn^.kn_sdata:=kn_sdata;

    kn^.kn_flags:=kn^.kn_flags or EV_CLEAR; { automatically set }

    knlist_add(@gc_knlist,kn,0);
   end;
  else
   Result:=EINVAL;
 end;

end;

procedure filterops_graphics_core_detach(kn:p_knote);
begin
 knlist_remove(@gc_knlist,kn,0);
end;

function filterops_graphics_core_event(kn:p_knote;hint:QWORD):Integer;
var
 me_id:Byte;
begin
 //hint:[event_id:8|meid:8|timestamp:48]
 if (hint=0) then
 begin
  Result:=ord(kn^.kn_kevent.data<>0);
 end else
 begin
  Result:=0;
  if (Byte(kn^.kn_sdata)=Byte(hint)) then //kn^.kn_sdata.event_id = hint.event_id
  begin
   me_id:=(hint shr 8);

   if (me_id=$80) or
      (me_id=Byte(kn^.kn_sdata shr 8)) then
   begin
    Result:=1;

    me_id:=Byte(kn^.kn_kevent.data shr 8);

    kn^.kn_kevent.data:=(hint and QWORD($ffffffffffff00ff)) or
                        (QWORD(me_id) shl 8);

   end;
  end;
 end;
end;

procedure filterops_graphics_core_touch(kn:p_knote;kev:p_kevent;_type:QWORD);
begin
 if (_type=EVENT_PROCESS) then
 begin
  kev^:=kn^.kn_kevent;
 end;
end;

const
 filterops_graphics_core:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filterops_graphics_core_attach;
  f_detach:@filterops_graphics_core_detach;
  f_event :@filterops_graphics_core_event;
  f_touch :@filterops_graphics_core_touch
 );

////

var
 gc_internal_knlock:mtx;
 gc_internal_knlist:t_knlist;

function filterops_internal_attach(kn:p_knote):Integer;
begin
 Result:=0;

 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR; { automatically set }

 knlist_add(@gc_internal_knlist,kn,0);
end;

procedure filterops_internal_detach(kn:p_knote);
begin
 knlist_remove(@gc_internal_knlist,kn,0);
end;

function filterops_internal_event(kn:p_knote;hint:QWORD):Integer;
begin
 if (hint=0) then
 begin
  Result:=ord(kn^.kn_kevent.data<>0);
 end else
 begin
  Result:=0;
  if (kn^.kn_kevent.ident=hint) then
  begin
   Result:=1;
   kn^.kn_kevent.data:=1;
  end;
 end;
end;

const
 filterops_internal:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filterops_internal_attach;
  f_detach:@filterops_internal_detach;
  f_event :@filterops_internal_event;
  f_touch :nil
 );

procedure gc_init_internal();
begin
 mtx_init(gc_internal_knlock,'gc internal kn lock');
 knlist_init_mtx(@gc_internal_knlist,@gc_internal_knlock);
end;

function gc_add_internal_ptr(kq,ptr,udata:Pointer):Integer; register; [public, alias:'gc_add_internal_ptr'];
var
 kev:t_kevent;
 fops:p_filterops;
begin
 kev:=Default(t_kevent);
 kev.ident:=PtrUint(ptr);
 kev.flags:=EV_ADD;
 kev.udata:=udata;
 //
 fops:=@filterops_internal;
 Result:=kqueue_register2(kq,@kev,fops);
end;

function gc_del_internal_ptr(kq,ptr:Pointer):Integer; register; [public, alias:'gc_del_internal_ptr'];
var
 kev:t_kevent;
 fops:p_filterops;
begin
 kev:=Default(t_kevent);
 kev.ident:=PtrUint(ptr);
 kev.flags:=EV_DELETE;
 //
 fops:=@filterops_internal;
 Result:=kqueue_register2(kq,@kev,fops);
end;

procedure gc_wakeup_internal_ptr(ptr:Pointer); register; [public, alias:'gc_wakeup_internal_ptr'];
begin
 knote(@gc_internal_knlist, PtrUint(ptr), 0);
end;

////

procedure gc_initialize();
begin
 gc_page:=dev_mem_alloc(1);

 make_dev(@gc_cdevsw,0,0,0,&666,'gc',[]);

 mtx_init(gc_knlock,'gc kn lock');
 knlist_init_mtx(@gc_knlist,@gc_knlock);

 gc_init_internal();

 kqueue_add_filteropts(EVFILT_GRAPHICS_CORE,@filterops_graphics_core);

 gc_ring_create(@ring_gfx,GC_RING_SIZE);

 pm4_me_gfx.Init(@gc_knlist);
 pm4_me_gfx.on_idle:=@me_idle;
 pm4_me_gfx.on_submit_flip_eop:=@dev_dce.TriggerFlipEop;
end;


end.

