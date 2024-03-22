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
 md_map,
 systm,
 kern_rwlock,
 kern_proc,
 kern_thr,
 kern_thread,
 md_sleep,
 subr_backtrace,
 bittype;

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

 //IT_INDIRECT_BUFFER_CNST = $00000033;  ccb  0xc0023300
 //IT_COND_INDIRECT_BUFFER = $0000003f;  dcb  0xc0023f00

 PPM4CMDINDIRECTBUFFER=^PM4CMDINDIRECTBUFFER;
 PM4CMDINDIRECTBUFFER=bitpacked record
  header   :DWORD; // PM4_TYPE_3_HEADER
  ibBase   :QWORD; // Indirect buffer base address, must be 4 byte aligned
  //
  ibSize   :bit20; // Indirect buffer size
  reserved0:bit4;
  vmid     :bit4;  // Virtual memory domain ID for command buffer
  reserved1:bit4;
 end;

 PPM4CMDSWITCHBUFFER=^PM4CMDSWITCHBUFFER;
 PM4CMDSWITCHBUFFER=bitpacked record
  header:DWORD;
  data  :DWORD;
 end;

const
 GC_RING_SIZE =$80000;

type
 p_pm4_ring=^t_pm4_ring;
 t_pm4_ring=packed record
  buff:Pointer;
  size:DWORD;
  rptr:DWORD;
  wptr:DWORD;
  aptr:DWORD;
 end;

function gc_ring_create(ring:p_pm4_ring;size:ptruint):Integer;
var
 hMem:THandle;
begin
 Result:=0;
 if (ring=nil) then Exit(-1);

 size:=1 shl BsfQWORD(size);
 if (size<64*1024) then size:=64*1024;

 ring^.buff:=nil;
 ring^.size:=size;
 ring^.rptr:=0;
 ring^.wptr:=0;
 ring^.aptr:=0;

 Result:=md_reserve_ex(ring^.buff,size+64*1024);
 if (Result<>0) then Exit;

 Result:=md_split(ring^.buff,size);
 if (Result<>0) then Exit;

 hMem:=0;
 Result:=md_memfd_create(hMem,size);
 if (Result<>0) then Exit;

 Result:=md_file_mmap_ex(hMem,ring^.buff,0,size,MD_PROT_RW);
 if (Result<>0) then
 begin
  md_memfd_close(hMem);
  Exit;
 end;

 Result:=md_file_mmap_ex(hMem,ring^.buff+size,size,64*1024,MD_PROT_RW);

 md_memfd_close(hMem);
end;

function gc_ring_free(ring:p_pm4_ring):Integer;
begin
 Result:=0;
 if (ring=nil) then Exit;
 if (ring^.buff=nil) then Exit;
 if (ring^.size=0) then Exit;

 Result:=md_unmap_ex(ring^.buff,ring^.size+64*1024);
end;

//need lock
function gc_ring_pm4_alloc(ring:p_pm4_ring;size:DWORD;buff:PPointer):Boolean;
var
 next:DWORD;
begin
 Result:=False;
 if (size>ring^.size) then Exit;

 next:=ring^.aptr+size;

 if (next>=ring^.size) then
 begin
  next:=next and (ring^.size-1);
  if (next>ring^.rptr) then Exit;
 end;

 buff^:=ring^.buff+ring^.aptr;
 ring^.aptr:=next;
 Result:=True;
end;

procedure gc_ring_pm4_submit(ring:p_pm4_ring);
begin
 System.InterlockedExchange(ring^.wptr,ring^.aptr);
end;

procedure gc_ring_pm4_release(ring:p_pm4_ring);
begin
 ring^.aptr:=ring^.wptr;
end;

//single consumer
function gc_ring_pm4_peek(ring:p_pm4_ring;size:PDWORD;buff:PPointer):Boolean;
var
 rptr:DWORD;
 wptr:DWORD;
 s   :DWORD;
begin
 Result:=False;

 rptr:=ring^.rptr;
 wptr:=ring^.wptr;

 if (rptr>wptr) then
 begin
  s:=(ring^.size-rptr)+wptr;
 end else
 begin
  s:=wptr-rptr;
 end;

 if (s<>0) then
 begin
  size^:=s;
  buff^:=ring^.buff+rptr;
  Result:=True;
 end;
end;

//single consumer
function gc_ring_pm4_drain(ring:p_pm4_ring;size:DWORD):Boolean;
var
 rptr:DWORD;
 wptr:DWORD;
 s   :DWORD;
begin
 Result:=False;

 rptr:=ring^.rptr;
 wptr:=ring^.wptr;

 if (rptr>wptr) then
 begin
  s:=(ring^.size-rptr)+wptr;
 end else
 begin
  s:=wptr-rptr;
 end;

 if (size>s) then Exit;

 rptr:=rptr+size;
 rptr:=rptr and (ring^.size-1);

 System.InterlockedExchange(ring^.rptr,rptr);

 Result:=True;
end;

function gc_submit_internal(ring:p_pm4_ring;count:DWORD;cmds:Pointer):Integer;
var
 size:QWORD;
 buf:PPM4CMDINDIRECTBUFFER;
 op:DWORD;
begin
 Result:=0;
 if (count=0) then Exit;

 if (count>=$1000) then Exit(-2142502897);

 size:=(count*16);

 buf:=nil;
 if not gc_ring_pm4_alloc(ring,size,@buf) then
 begin
  Writeln(stderr,'### gc_submit_common : Cannot allocate a space in ring buffer.');
  Exit(16);
 end;

 Result:=copyin(cmds,buf,size);

 if (Result<>0) then
 begin
  gc_ring_pm4_release(ring);
  Exit(-2142502898);
 end;

 while (count<>0) do
 begin
  op:=buf^.header;

  if ((op<>$c0023300) and (op<>$c0023f00)) then
  begin
   Writeln(stderr,'## gc_insert_indirect_buffer: invalid opcode = 0x',HexStr(op,8));
   gc_ring_pm4_release(ring);
   Exit(-2142502896);
  end;

  if (buf^.ibSize=0) then
  begin
   Writeln(stderr,'## gc_insert_indirect_buffer: invalid ib_size = 0x',HexStr(buf^.ibSize,5));
   gc_ring_pm4_release(ring);
   Exit(-2142502895);
  end;

  Inc(buf);
  Dec(count);
 end;

 gc_ring_pm4_submit(ring);
end;

function gc_switch_buffer_internal(ring:p_pm4_ring):Integer;
var
 buf:PPM4CMDSWITCHBUFFER;
begin
 Result:=0;

 buf:=nil;
 if not gc_ring_pm4_alloc(ring,sizeof(PM4CMDSWITCHBUFFER),@buf) then
 begin
  Writeln(stderr,'### gc_switch_buffer_internal : Cannot allocate a space in ring buffer.');
  Exit(16);
 end;

 //IT_SWITCH_BUFFER = $0000008b;

 buf^.header:=$c0008b00;
 buf^.data  :=0;

 gc_ring_pm4_submit(ring);
end;

var
 ring_gfx:t_pm4_ring;
 ring_gfx_lock:Pointer=nil;

 parse_gfx_td:p_kthread;

procedure parse_gfx_ring(parameter:pointer); SysV_ABI_CDecl;
var
 buff:Pointer;
 size:DWORD;
begin

 repeat

  if gc_ring_pm4_peek(@ring_gfx,@size,@buff) then
  begin
   Writeln('packet:0x',HexStr(buff),':',size);

   gc_ring_pm4_drain(@ring_gfx,size);
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

