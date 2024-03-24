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
 kern_thread,
 md_sleep,
 pm4_ring,
 pm4defs,
 subr_backtrace;

var
 gc_page:Pointer;

 gc_submits_allowed_vaddr:PInteger=nil; //0=true,1=false (0xfe0100000)
 gc_submits_allowed_vmirr:PInteger=nil;

 gc_knl_lock:mtx;
 gc_knlist:t_knlist;

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
   paddr:=$fc0000000;
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

 parse_gfx_td:p_kthread;

function PM4_LENGTH(token:DWORD):DWORD; inline;
begin
 Result:=((token shr 14) and $FFFC) + 8;
end;

procedure parse_gfx_buffer(buf:PPM4CMDINDIRECTBUFFER);
var
 addr:Pointer;
 size:QWORD;

 i,token,len:DWORD;
begin
 case buf^.header of
  $c0023300:Writeln('INDIRECT_BUFFER_CNST');
  $c0023f00:Writeln('COND_INDIRECT_BUFFER');
  else;
 end;

 i:=buf^.ibSize*sizeof(DWORD);

 addr:=nil;
 size:=0;
 if get_dmem_ptr(Pointer(buf^.ibBase),@addr,@size) then
 begin
  if (i>size) then
  begin
   Assert(false,'addr:0x'+HexStr(buf^.ibBase+size,16)+' not in dmem!');
  end;

  Writeln(' addr:0x'+HexStr(buf^.ibBase,16)+' '+HexStr(i,16));
 end else
 begin
  Assert(false,'addr:0x'+HexStr(buf^.ibBase,16)+' not in dmem!');
 end;

 while (i<>0) do
 begin
  token:=PDWORD(addr)^;
  len:=PM4_LENGTH(token);
  if (len>i) then Exit;

  case PM4_TYPE(token) of
   0:begin //PM4_TYPE_0
      Writeln('PM4_TYPE_0');
      //onPm40(PM4_TYPE_0_HEADER(token),@PDWORD(P)[1]);
     end;
   2:begin //PM4_TYPE_2
      Writeln('PM4_TYPE_2');
      //onPm42(PM4_TYPE_2_HEADER(token));

      //no body
      len:=sizeof(DWORD);
     end;
   3:begin //PM4_TYPE_3
      case PM4_TYPE_3_HEADER(token).opcode of
       IT_NOP                :Writeln('IT_NOP                ');
       IT_EVENT_WRITE_EOP    :Writeln('IT_EVENT_WRITE_EOP    ');
       IT_EVENT_WRITE_EOS    :Writeln('IT_EVENT_WRITE_EOS    ');
       IT_DMA_DATA           :Writeln('IT_DMA_DATA           ');
       IT_ACQUIRE_MEM        :Writeln('IT_ACQUIRE_MEM        ');
       IT_CONTEXT_CONTROL    :Writeln('IT_CONTEXT_CONTROL    ');
       IT_CLEAR_STATE        :Writeln('IT_CLEAR_STATE        ');
       IT_SET_CONTEXT_REG    :Writeln('IT_SET_CONTEXT_REG    ');
       IT_SET_SH_REG         :Writeln('IT_SET_SH_REG         ');
       IT_SET_UCONFIG_REG    :Writeln('IT_SET_UCONFIG_REG    ');
       IT_SET_CONFIG_REG     :Writeln('IT_SET_CONFIG_REG     ');
       IT_INDEX_BUFFER_SIZE  :Writeln('IT_INDEX_BUFFER_SIZE  ');
       IT_INDEX_TYPE         :Writeln('IT_INDEX_TYPE         ');
       IT_DRAW_INDEX_2       :Writeln('IT_DRAW_INDEX_2       ');
       IT_DRAW_INDEX_AUTO    :Writeln('IT_DRAW_INDEX_AUTO    ');
       IT_INDEX_BASE         :Writeln('IT_INDEX_BASE         ');
       IT_DRAW_INDEX_OFFSET_2:Writeln('IT_DRAW_INDEX_OFFSET_2');
       IT_DISPATCH_DIRECT    :Writeln('IT_DISPATCH_DIRECT    ');
       IT_NUM_INSTANCES      :Writeln('IT_NUM_INSTANCES      ');
       IT_WAIT_REG_MEM       :Writeln('IT_WAIT_REG_MEM       ');
       IT_WRITE_DATA         :Writeln('IT_WRITE_DATA         ');
       IT_EVENT_WRITE        :Writeln('IT_EVENT_WRITE        ');
       IT_PFP_SYNC_ME        :Writeln('IT_PFP_SYNC_ME        ');

       IT_SET_BASE           :Writeln('IT_SET_BASE           ');
       IT_DRAW_PREAMBLE      :Writeln('IT_DRAW_PREAMBLE      ');
       IT_SET_PREDICATION    :Writeln('IT_SET_PREDICATION    ');

       IT_LOAD_CONST_RAM     :Writeln('IT_LOAD_CONST_RAM     ');

       else
                              Writeln('PM4_TYPE_3.opcode:0x',HexStr(PM4_TYPE_3_HEADER(token).opcode,2));
      end;
     end;
   else
    begin
     Writeln('PM4_TYPE_',PM4_TYPE(token));
     Assert(False);
    end;
  end;

  Inc(addr,len);
  Dec(i,len);
 end;

end;

procedure parse_gfx_ring(parameter:pointer); SysV_ABI_CDecl;
var
 buff:Pointer;
 i,size,op,len:DWORD;
begin

 repeat

  if gc_ring_pm4_peek(@ring_gfx,@size,@buff) then
  begin
   Writeln('packet:0x',HexStr(buff),':',size);

   i:=size;
   while (i<>0) do
   begin
    op:=PDWORD(buff)^;
    len:=PM4_LENGTH(op);
    if (len>i) then Exit;

    case op of
     $c0023300:parse_gfx_buffer(buff);
     $c0023f00:parse_gfx_buffer(buff);
     $c0008b00:Writeln('SWITCH_BUFFER');
     else;
    end;

    Inc(buff,len);
    Dec(i,len);
   end;

   //buf:PPM4CMDINDIRECTBUFFER;
   //Writeln('opcode =0x',HexStr(buf[i].header,8));
   //Writeln('ib_base=0x',HexStr(buf[i].ibBase,16));
   //Writeln('ib_size=0x',HexStr(buf[i].ibSize,3));

   gc_ring_pm4_drain(@ring_gfx,size-i);
  end;

  msleep_td(100);
 until false;


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

             Result:=gc_submit_internal(@ring_gfx,
                                        p_submit_args(data)^.count,
                                        p_submit_args(data)^.cmds);

             rw_wunlock(ring_gfx_lock);
            end;

  $C0088101: //switch_buffer
            begin
             rw_wlock(ring_gfx_lock);

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
 kthread_add(@parse_gfx_ring,nil,@parse_gfx_td,0,'[GFX]');
end;


end.

