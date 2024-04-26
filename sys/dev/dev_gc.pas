unit dev_gc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure gc_initialize();

implementation

uses
 errno,
 kern_mtx,
 sys_event,
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
 md_sleep,
 pm4_ring,
 pm4_pfp,
 pm4_me,

 vDevice,
 vMemory,

 subr_backtrace;

var
 gc_page:Pointer;

 gc_submits_allowed_vaddr:PInteger=nil; //0=true,1=false (0xfe0100000)
 gc_submits_allowed_vmirr:PInteger=nil;

 gc_knl_lock:mtx;
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

var
 ring_gfx:t_pm4_ring;
 ring_gfx_lock:Pointer=nil;

 parse_gfx_started:Pointer=nil;
 parse_gfx_td:p_kthread;

 pfp_ctx:t_pfp_ctx;

 pm4_me_gfx:t_pm4_me;

function pm4_parse_ring(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
var
 ibuf:t_pm4_ibuffer;
 i:Integer;
begin
 Result:=0;

 case token of
  $c0023300:
   begin
    if pctx^.print_ops then
    begin
     Writeln('INDIRECT_BUFFER (ccb) 0x',HexStr(PPM4CMDINDIRECTBUFFER(buff)^.ibBase,10));
    end;
    if pm4_ibuf_init(@ibuf,buff,@pm4_parse_ccb) then
    begin
     i:=pm4_ibuf_parse(pctx,@ibuf);
     if (i<>0) then
     begin
      pctx^.add_stall(@ibuf);
     end;
    end;
   end;
  $c0023f00:
   begin
    if pctx^.print_ops then
    begin
     Writeln('INDIRECT_BUFFER (dcb) 0x',HexStr(PPM4CMDINDIRECTBUFFER(buff)^.ibBase,10));
    end;
    if pm4_ibuf_init(@ibuf,buff,@pm4_parse_dcb) then
    begin
     i:=pm4_ibuf_parse(pctx,@ibuf);
     if (i<>0) then
     begin
      pctx^.add_stall(@ibuf);
     end;
    end;
   end;
  $c0008b00:
   begin
    if pctx^.print_ops then
    begin
     Writeln('SWITCH_BUFFER');
    end;
   end;
  else;
 end;

end;

procedure parse_gfx_ring(parameter:pointer); SysV_ABI_CDecl;
var
 buff:Pointer;
 i,size:DWORD;

 ibuf:t_pm4_ibuffer;
begin

 if LoadVulkan then
 begin
  InitVulkan;
 end;

 repeat

  if gc_ring_pm4_peek(@ring_gfx,@size,@buff) then
  begin
   //Writeln('packet:0x',HexStr(buff),':',size);

   if pm4_ibuf_init(@ibuf,buff,size,@pm4_parse_ring) then
   begin
    i:=pm4_ibuf_parse(@pfp_ctx,@ibuf);

    if (i<>0) then
    begin
     pm4_me_gfx.Push(pfp_ctx.stream_dcb);
     pm4_me_gfx.Push(pfp_ctx.stream_ccb);

     pfp_ctx.add_stall(@ibuf);
    end;

   end;

   pm4_me_gfx.Push(pfp_ctx.stream_ccb);
   pm4_me_gfx.Push(pfp_ctx.stream_dcb);

   gc_ring_pm4_drain(@ring_gfx,size);
  end;

  msleep_td(100);
 until false;


end;

procedure start_gfx_ring;
begin
 if (System.InterlockedExchange(parse_gfx_started,Pointer(1))=nil) then
 begin
  pfp_ctx.print_hint:=true;
  pfp_ctx.print_ops :=true;

  kthread_add(@parse_gfx_ring,nil,@parse_gfx_td,(8*1024*1024) div (16*1024),'[GFX_PFP]');
 end;
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

  $C004811F: //sceGnmGetNumTcaUnits
            begin
             Exit(19);
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

             gc_submits_allowed_vmirr^:=0; //init
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
            end;

  $C0048114: //sceGnmFlushGarlic
            begin
             Writeln('sceGnmFlushGarlic');
            end;

  $C0108102: //submit
            begin
             rw_wlock(ring_gfx_lock);

              start_gfx_ring;

              Result:=gc_submit_internal(@ring_gfx,
                                         p_submit_args(data)^.count,
                                         p_submit_args(data)^.cmds);

             rw_wunlock(ring_gfx_lock);
            end;

  $C0088101: //switch_buffer
            begin
             rw_wlock(ring_gfx_lock);

              start_gfx_ring;

              Result:=gc_switch_buffer_internal(@ring_gfx);

             rw_wunlock(ring_gfx_lock);
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

 paddr^:=offset {+ };
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

function filterops_graphics_core_attach(kn:p_knote):Integer;
var
 kn_sdata:QWORD;
 event_id:Byte;
 cap:Boolean;
 unk:Integer;
begin
 Result:=0;

 event_id:=Byte(kn^.kn_data);

 cap:=sceSblACMgrHasUseHp3dPipeCapability(@g_authinfo);

 unk:=0;

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

    kn^.kn_sdata:=(kn_sdata and QWORD($ffffffffffffff00)) or QWORD(event_id);

    case event_id of
     $82..$86:kn_sdata:=(kn_sdata and QWORD($ffffffffffff0000)) or
                        QWORD(event_id);
      else
              kn_sdata:=(kn_sdata and QWORD($ffffffffffff0000)) or
                        QWORD(event_id) or
                        QWORD((unk and $ff) shl 8);
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
 event_id:Byte;
begin
 if (hint=0) then
 begin
  Result:=ord(kn^.kn_kevent.data<>0);
 end else
 begin
  Result:=0;
  if (Byte(kn^.kn_sdata)=Byte(hint)) then
  begin
   event_id:=(hint shr 8);

   if (event_id=$80) or
      (Byte(kn^.kn_sdata shr 8)=event_id) then
   begin
    Result:=1;

    event_id:=Byte(kn^.kn_kevent.data shr 8);

    kn^.kn_kevent.data:=(hint and QWORD($ffffffffffff00ff)) or
                        (QWORD(event_id) shl 8);

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

procedure gc_initialize();
begin
 gc_page:=dev_mem_alloc(1);

 make_dev(@gc_cdevsw,0,0,0,&666,'gc',[]);

 mtx_init(gc_knl_lock,'gc knl lock');
 knlist_init_mtx(@gc_knlist,@gc_knl_lock);

 kqueue_add_filteropts(EVFILT_GRAPHICS_CORE,@filterops_graphics_core);

 gc_ring_create(@ring_gfx,GC_RING_SIZE);

 pm4_me_gfx.Init;
end;


end.

