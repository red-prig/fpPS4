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
 kern_proc,
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

const
 GC_RING_SIZE =$80000;
 GC_RING_COUNT=GC_RING_SIZE div 16;

function gc_submit_internal(ring:Pointer;count:DWORD;cmds:Pointer):Integer;
var
 size_dw:QWORD;
begin
 if (count=0) then Exit(0);

 if (count >= $1000) then Exit(-2142502897);

 size_dw:=(count*4) and $3ffffffc;


end;

Function gc_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 vaddr:QWORD;
 buf:PPM4CMDINDIRECTBUFFER;
 i:Integer;
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
             Writeln(p_submit_args(data)^.count);
             Writeln(HexStr(p_submit_args(data)^.cmds));

             buf:=Pointer(p_submit_args(data)^.cmds);

             for i:=0 to p_submit_args(data)^.count-1 do
             begin
              Writeln('opcode =0x',HexStr(buf[i].header,8));
              Writeln('ib_base=0x',HexStr(buf[i].ibBase,16));
              Writeln('ib_size=0x',HexStr(buf[i].ibSize,3));
             end;


             //IT_INDIRECT_BUFFER_CNST = $00000033;  ccb
             //IT_COND_INDIRECT_BUFFER = $0000003f;  dcb

             //opcode           0x33                     0x3F
             //if ((op != L'\xc0023300') && (op != L'\xc0023f00')) {
             //  printf("## %s: invalid opcode = 0x%08x\n","gc_insert_indirect_buffer");
             //  ret1 = 0x804c0010;
             //  goto LAB_ffffffff826bd78b;
             //}
             //if ((_buffer[1] & 0xfffff00000000) == 0) {
             //  printf("## %s: invalid ib_size = 0x%05x\n","gc_insert_indirect_buffer",0);
             //  ret1 = 0x804c0011;
             //  goto LAB_ffffffff826bd78b;
             //}


             Assert(False);
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

end;


end.

