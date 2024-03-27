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
 kern_dmem,
 kern_rwlock,
 kern_proc,
 kern_thr,
 kern_thread,
 md_sleep,
 pm4_ring,
 pm4_pfp,
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

procedure onLoadConstRam(Body:PPM4CMDCONSTRAMLOAD);
var
 buf:PDWORD;
 i:integer;

 addr:Pointer;
 size:QWORD;
begin
 Writeln(' adr=0x',HexStr(Body^.addr,16));
 Writeln(' len=0x',HexStr(Body^.numDwords*4,4));
 Writeln(' ofs=0x',HexStr(Body^.offset,4));

 {
 addr:=nil;
 size:=0;
 get_dmem_ptr(Pointer(Body^.addr),@addr,@size);

 buf:=addr;
 for i:=0 to Body^.numDwords-1 do
 begin
  Writeln('  0x',HexStr(buf[i],8));
 end;
 }
end;

function pm4_parse_ccb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     Writeln('PM4_TYPE_0');
    end;
  2:begin //PM4_TYPE_2
     Writeln('PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     Writeln('IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),' len:',PM4_LENGTH(token));

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_LOAD_CONST_RAM:onLoadConstRam(buff);

      else;
     end;

    end;
  else
   begin
    Writeln('PM4_TYPE_',PM4_TYPE(token));
    Assert(False);
   end;
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

procedure onContextControl(Body:PPM4CMDCONTEXTCONTROL);
begin
 Writeln(' loadControl =b',revbinstr(DWORD(Body^.loadControl ),32));
 Writeln(' shadowEnable=b',revbinstr(DWORD(Body^.shadowEnable),32));
end;

procedure onSetBase(Body:PPM4CMDDRAWSETBASE);
var
 addr:QWORD;
begin
 addr:=Body^.addressLo or (Body^.addressHi shl 32);

 Writeln(' baseIndex=0x',HexStr(Body^.baseIndex,4));
 Writeln(' address  =0x',HexStr(addr,16));
end;

procedure onSetPredication(Body:PPM4CMDSETPREDICATION);
var
 addr:QWORD;
begin
 addr:=Body^.startAddressLo or (Body^.startAddrHi shl 32);

 Writeln(' startAddress=0x',HexStr(addr,16));
 Writeln(' pred        =',Body^.predicationBoolean);
 Writeln(' hint        =',Body^.hint);
 Writeln(' predOp      =',Body^.predOp);
 Writeln(' continueBit =',Body^.continueBit);
end;

procedure onDrawPreamble(Body:PPM4CMDDRAWPREAMBLE);
begin
 //Writeln;
end;

procedure onSetShReg(Body:PPM4CMDSETDATA);
begin
 Writeln(' 0x',HexStr(SH_REG_BASE+Body^.REG_OFFSET,4),'..',
          '0x',HexStr(SH_REG_BASE+Body^.REG_OFFSET+Body^.header.count-1,4));
end;

procedure onSetUConfigReg(Body:PPM4CMDSETDATA);
begin
 Writeln(' 0x',HexStr(USERCONFIG_REG_BASE+Body^.REG_OFFSET,4),'..',
          '0x',HexStr(USERCONFIG_REG_BASE+Body^.REG_OFFSET+Body^.header.count-1,4));
end;

procedure onSetContextReg(Body:PPM4CMDSETDATA);
begin
 Writeln(' 0x',HexStr(CONTEXT_REG_BASE+Body^.REG_OFFSET,4),'..',
          '0x',HexStr(CONTEXT_REG_BASE+Body^.REG_OFFSET+Body^.header.count-1,4));
end;

function pm4_parse_dcb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     Writeln('PM4_TYPE_0');
    end;
  2:begin //PM4_TYPE_2
     Writeln('PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     Writeln('IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),' len:',PM4_LENGTH(token));

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_NOP                :;
      IT_EVENT_WRITE_EOP    :;
      IT_EVENT_WRITE_EOS    :;
      IT_DMA_DATA           :;
      IT_ACQUIRE_MEM        :;
      IT_CONTEXT_CONTROL    :onContextControl(buff);
      IT_CLEAR_STATE        :;
      IT_SET_CONTEXT_REG    :onSetContextReg(buff);
      IT_SET_SH_REG         :onSetShReg(buff);
      IT_SET_UCONFIG_REG    :onSetUConfigReg(buff);
      IT_SET_CONFIG_REG     :;
      IT_INDEX_BUFFER_SIZE  :;
      IT_INDEX_TYPE         :;
      IT_DRAW_INDEX_2       :;
      IT_DRAW_INDEX_AUTO    :;
      IT_INDEX_BASE         :;
      IT_DRAW_INDEX_OFFSET_2:;
      IT_DISPATCH_DIRECT    :;
      IT_NUM_INSTANCES      :;
      IT_WAIT_REG_MEM       :;
      IT_WRITE_DATA         :;
      IT_EVENT_WRITE        :;
      IT_PFP_SYNC_ME        :;

      IT_SET_BASE           :onSetBase(buff);
      IT_DRAW_PREAMBLE      :onDrawPreamble(buff);
      IT_SET_PREDICATION    :onSetPredication(buff);

      else;
     end;


    end;
  else
   begin
    Writeln('PM4_TYPE_',PM4_TYPE(token));
    Assert(False);
   end;
 end;

end;

function pm4_parse_ring(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
var
 ibuf:t_pm4_ibuffer;
 i:Integer;
begin
 Result:=0;

 case token of
  $c0023300:
   begin
    Writeln('INDIRECT_BUFFER_CNST (ccb)');
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
    Writeln('INDIRECT_BUFFER (dcb)');
    if pm4_ibuf_init(@ibuf,buff,@pm4_parse_dcb) then
    begin
     i:=pm4_ibuf_parse(pctx,@ibuf);
     if (i<>0) then
     begin
      pctx^.add_stall(@ibuf);
     end;
    end;
   end;
  $c0008b00:Writeln('SWITCH_BUFFER');
  else;
 end;

end;

procedure parse_gfx_ring(parameter:pointer); SysV_ABI_CDecl;
var
 buff:Pointer;
 i,size:DWORD;

 pfp_ctx:t_pfp_ctx;
 ibuf:t_pm4_ibuffer;
begin
 pfp_ctx:=Default(t_pfp_ctx);

 repeat

  if gc_ring_pm4_peek(@ring_gfx,@size,@buff) then
  begin
   Writeln('packet:0x',HexStr(buff),':',size);

   if pm4_ibuf_init(@ibuf,buff,size,@pm4_parse_ring) then
   begin
    i:=pm4_ibuf_parse(@pfp_ctx,@ibuf);

    if (i<>0) then
    begin
     pfp_ctx.add_stall(@ibuf);
    end;

   end;

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

