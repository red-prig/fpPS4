unit kern_dmem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 sys_conf,
 vm_object,
 vm,
 vm_blockpool,
 dmem_map,
 rmem_map;

type
 p_query_memory_prot=^t_query_memory_prot;
 t_query_memory_prot=packed record
  start:Pointer;
  __end:Pointer;
  prot  :Integer;
  eflags:Integer;
 end;
 {$IF sizeof(t_query_memory_prot)<>24}{$STOP sizeof(t_query_memory_prot)<>24}{$ENDIF}

type
 p_dmem_obj=^t_dmem_obj;
 t_dmem_obj=record
  dmem:p_dmem_map;
  vobj:vm_object_t;
 end;

var
 dmem:t_dmem_map;
 rmap:t_rmem_map;

 dmem_maps:array[0..2] of t_dmem_obj;

procedure init_dmem_map;

function  sys_dmem_container(d_pool_id:Integer):Integer;
function  sys_set_chicken_switches(flags:Integer):Integer;

function  sys_mmap_dmem(vaddr :Pointer;
                        length:QWORD;
                        mtype :DWORD;
                        prot  :DWORD;
                        flags :DWORD;
                        phaddr:QWORD):Pointer;

function  sys_virtual_query(addr:Pointer;
                            flags:DWORD;
                            info:Pointer;
                            infoSize:QWORD):Integer;

function  sys_query_memory_protection(addr:Pointer;info:Pointer):Integer;

function  rmem_map_test_lock(start,__end:QWORD;mode:Integer):Boolean;

function  obj2dmem(obj:vm_object_t):p_dmem_map;

function  get_dmem_ptr(addr:Pointer):Pointer;

implementation

uses
 errno,
 md_systm,
 systm,
 vmparam,
 vm_map,
 kern_authinfo,
 kern_thr,
 kern_proc;

//////////

function IDX_TO_OFF(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function AlignUp(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
var
 tmp:PtrUInt;
begin
 if (alignment=0) then Exit(addr);
 tmp:=addr+PtrUInt(alignment-1);
 Result:=tmp-(tmp mod alignment)
end;

function AlignDw(addr:PtrUInt;alignment:PtrUInt):PtrUInt; inline;
begin
 Result:=addr-(addr mod alignment);
end;

procedure init_dmem_map;
var
 vmap:vm_map_t;
begin
 dmem_map_init(@dmem,0,SCE_KERNEL_MAIN_DMEM_SIZE);
 rmem_map_init(@rmap,0,SCE_KERNEL_MAIN_DMEM_SIZE);

 vmap:=p_proc.p_vmspace;

 vmap^.rmap:=@rmap;

 dmem.vmap:=vmap;
 dmem.rmap:=@rmap;

 rmap.tmap:=@vmap^.pmap^.tr_map;
end;

function sys_dmem_container(d_pool_id:Integer):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 td^.td_retval[0]:=p_proc.p_dmem_pool_id;
 Result:=0;

 if (d_pool_id<>-1) then
 begin
  //Result:=priv_check(td,685);
  //(param < 3)
  Exit(EPERM);
 end;
end;

function sys_set_chicken_switches(flags:Integer):Integer;
begin
 Writeln('[KERNEL] set_chicken_switches(',flags,')');
 p_proc.p_dmem_aliasing:=p_proc.p_dmem_aliasing or flags;
 //0x1 - kern_mmap_dmem -> any alias
 //0x2 - kern_mmap_dmem -> GPU -> CPU only
 Result:=0;
end;

function sdk_version_big_20():Boolean; inline;
begin
 Result:=p_proc.p_sdk_version > $2ffffff;
end;

function vm_mmap_to_errno(rv:Integer):Integer; inline;
begin
 Case rv of
  KERN_SUCCESS           :Result:=0;
  KERN_INVALID_ADDRESS,
  KERN_NO_SPACE          :Result:=ENOMEM;
  KERN_PROTECTION_FAILURE:Result:=EACCES;
  else
   Result:=EINVAL;
 end;
end;

function rmem_map_test_lock(start,__end:QWORD;mode:Integer):Boolean;
begin
 rmem_map_lock(@rmap);
  Result:=rmem_map_test(@rmap,start,__end,mode);
 rmem_map_unlock(@rmap);
end;

function kern_mmap_dmem(map   :vm_map_t;
                        addr  :p_vm_offset_t;
                        phaddr:QWORD;
                        vaddr :QWORD;
                        length:QWORD;
                        mtype :DWORD;
                        prot  :DWORD;
                        align :QWORD;
                        flags :DWORD;
                        anon  :Pointer):Integer;
label
 _fixed,
 _rmap_insert;
var
 dmap:t_dmem_obj;

 v_end:QWORD;
 faddr:QWORD;

 entry,next:vm_map_entry_t;

 cow:DWORD;
 err:Integer;

 found:Boolean;
begin
 Result:=0;
 addr^:=0;

 if (((phaddr shr 36) > 4) or ((max_valid_dmem - phaddr) < length)) then
 begin
  Exit(EACCES);
 end;

 dmap:=dmem_maps[p_proc.p_dmem_pool_id];

 cow:=(flags and MAP_NO_COALESCE) or MAP_COW_MMAP_DMEM;

 vm_map_lock(map);

 if (align=0) then
 begin
  //MAP_FIXED
  _fixed:

  v_end:=vaddr+length;

  if (v_end <=map^.max_offset) and
     ( ((flags and MAP_SANITIZER)<>0) or
       ((vaddr shr 47) <> 0) or
       (v_end <= MAP_AREA_END) or
       (sdk_version_big_20()=false) ) then
  begin
   found:=rmem_map_test_lock(phaddr,phaddr+length,0);

   //
   if (not found) or //not found
      (is_sce_prog_attr_20_800000(@g_authinfo)) or
      ((p_proc.p_dmem_aliasing and 1)<>0) then //aliasing
   begin
    _rmap_insert:

    err:=dmem_map_set_mtype(dmap.dmem,
                            OFF_TO_IDX(phaddr),
                            OFF_TO_IDX(phaddr+length),
                            mtype,
                            prot,
                            flags);

    if (err=0) then
    begin

     //try to expand addres space
     vm_map_expand(map, vaddr, v_end);

     if (align=0) and ((flags and MAP_NO_OVERWRITE)=0) then
     begin
      vm_map_delete(map, vaddr, v_end);
     end;

     vm_object_reference(dmap.vobj);

     err:=vm_map_insert(map,
                        dmap.vobj,
                        phaddr,
                        vaddr, v_end,
                        prot, VM_PROT_ALL,
                        cow,
                        anon
                       );

     if (err=0) then
     begin
      addr^:=vaddr;
     end else
     begin
      vm_object_deallocate(dmap.vobj);
      //
      Result:=vm_mmap_to_errno(err);
     end;
    end;

   end else
   if ((p_proc.p_dmem_aliasing and 2)<>0) then //aliasing gpu???
   begin
    if ((prot and VM_PROT_GPU_ALL)<>0) then
    begin
     Writeln('TODO check aliasing prot');
    end;
    goto _rmap_insert;
   end else
   begin
    Writeln('[KERNEL] multiple VA mappings are detected. va:[0x',HexStr(vaddr,16),',0x',HexStr(v_end,16),')');
    Result:=EINVAL;
   end;

  end else
  begin
   Result:=ENOMEM;
   if (align=0) then
   begin
    Result:=EINVAL;
   end;
  end;

 end else
 begin
  //(align<>0)
  //find free space

  err:=vm_map_findspace(map,vaddr,length,@faddr);

  if (err=0) then
  begin
   repeat
    faddr:=AlignUp(faddr,align);
    vaddr:=faddr;

    err:=ord(vm_map_lookup_entry(map,faddr,@entry));

    next:=entry;
    if (err=0) then
    begin
     next:=entry^.next;
     if (next=@map^.header) then
     begin
      if (length <= (map^.header.__end - vaddr)) then goto _fixed;
      Break;
     end;
     if (length <= (next^.start - vaddr)) then goto _fixed;
    end;

    err:=vm_map_findspace(map,next^.__end,length,@faddr);
   until (err<>0);
  end;

  Result:=ENOMEM;
 end;

 vm_map_unlock(map);
end;

function sys_mmap_dmem(vaddr :Pointer;
                       length:QWORD;
                       mtype :DWORD;
                       prot  :DWORD;
                       flags :DWORD;
                       phaddr:QWORD):Pointer;
var
 td:p_kthread;
 map:vm_map_t;
 addr:vm_offset_t;
 align:QWORD;

 rbp:PPointer;
 rip:Pointer;
 stack_addr:Pointer;
begin
 td:=curkthread;
 if (td=nil) then Exit(Pointer(-1));

 if is_sce_prog_attr_40_800000(@g_authinfo) then
 begin
  Exit(Pointer(EPERM));
 end;

 if is_sce_prog_attr_40_400000(@g_authinfo) then
 begin
  Exit(Pointer(EPERM));
 end;

 if (p_proc.p_dmem_pool_id<>1) then
 begin
  Exit(Pointer(EOPNOTSUPP));
 end;

 addr:=vm_offset_t(vaddr);

 if ((addr and PAGE_MASK)<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((phaddr and PAGE_MASK)<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if (((flags and $e09fff6f) or (prot and $ffffffcc))<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if (DWORD(mtype + 1) > 11) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if (length <= PAGE_MASK) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((length and PAGE_MASK)<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((flags and MAP_SANITIZER)<>0) then
 begin
  Exit(Pointer(EINVAL)); //is_sanitizer()=0
  //Sanitizer
 end;

 //backtrace
 rbp:=Pointer(td^.td_frame.tf_rbp);
 stack_addr:=nil;

 while (QWORD(rbp) < QWORD($800000000000)) do //sv_maxuser
 begin
  rip:=md_fuword(rbp[1]);
  rbp:=md_fuword(rbp[0]);

  if (QWORD(rip)=QWORD(-1)) or
     (QWORD(rbp)=QWORD(-1)) then
  begin
   Break;
  end;

  if (p_proc.p_libkernel_start_addr >  rip) or
     (p_proc.p_libkernel___end_addr <= rip) then
  begin
   stack_addr:=rip;
   Break;
  end;

 end;
 //backtrace

 map:=p_proc.p_vmspace;

 if ((flags and MAP_FIXED)=0) then
 begin
  if (addr=0) then
  begin
   if ( (QWORD(stack_addr) - QWORD($7f0000000)) < QWORD($800000000)) then //ET_DYN_LOAD_ADDR_SYS
   begin
    addr:=SCE_SYS_HEAP_START;
   end else
   begin
    addr:=SCE_USR_HEAP_START;
   end;
  end else
  if ( (QWORD(stack_addr) - QWORD($7f0000000)) > QWORD($7ffffffff)) and
     (addr <= QWORD($ff0000000)) and
     ( (length + addr) > QWORD($7efffffff)) then
  begin
   addr:=$ff0000000; //SCE_KERNEl_GNM_TESS_AREA
  end;

  align:=(flags shr MAP_ALIGNMENT_SHIFT) and $1f;
  if (align<PAGE_SHIFT) then align:=1;
  align:=QWORD(1) shl align;
 end else
 begin
  //Address range must be all in user VM space.
  if (addr < vm_map_min(map)) or
     (addr + length > vm_map_max(map)) then
  begin
   Exit(Pointer(EINVAL));
  end;

  if (addr+length<addr) then
  begin
   Exit(Pointer(EINVAL));
  end;

  align:=0;
 end;

 Result:=Pointer(kern_mmap_dmem(map,
                                @addr,
                                phaddr,
                                addr,
                                length,
                                mtype,
                                prot or ((prot shr 1) and 1),
                                align,
                                flags,
                                stack_addr));

 td^.td_retval[0]:=addr;

 Writeln('0x',HexStr(QWORD(stack_addr),11),'->',
         'sys_mmap_dmem(','0x',HexStr(QWORD(vaddr),11),
                         ',0x',HexStr(length,11),
                         ',0x',HexStr(mtype,1),
                         ',0x',HexStr(prot,1),
                         ',0x',HexStr(flags,8),
                         ',0x',HexStr(phaddr,10),
                          '):',Integer(Result),
                         ':0x',HexStr(addr,11),'..0x',HexStr(addr+length,11));
end;

function IN_CUSALIST_1:Boolean;
begin
 case String(g_appinfo.CUSANAME) of
  'CUSA00663',
  'CUSA01270',
  'CUSA00966',
  'CUSA01199',
  'CUSA00606',
  'CUSA00605',
  'CUSA00476':
    Result:=True;
  else;
    Result:=False;
 end;
end;

function IN_CUSALIST_2:Boolean;
begin
 case String(g_appinfo.CUSANAME) of
  'CUSA00345',
  'CUSA00380',
  'CUSA00432',
  'CUSA01086',
  'CUSA01101',
  'CUSA01276',
  'CUSA01374',
  'CUSA01382',
  'CUSA01405':
    Result:=True;
  else;
    Result:=False;
 end;
end;

function get_obj_mtype(obj:vm_map_object):Byte;
begin
 Result:=obj^.un_pager.physhm.mtype;
end;

function is_valid_entry(entry:vm_map_entry_t):Boolean; inline;
begin
 Result:=not (entry^.inheritance in [VM_INHERIT_PATCH,VM_INHERIT_HOLE]);
end;

function next_valid_entry(map:vm_map_t;entry:vm_map_entry_t):vm_map_entry_t;
begin
 while (entry<>@map^.header) and (not is_valid_entry(entry)) do
 begin
  entry:=entry^.next;
 end;
 Result:=entry;
end;

procedure dmem_vmo_get_info(map  :vm_map_t;
                            entry:vm_map_entry_t;
                            addr :QWORD;
                            qinfo:pSceKernelVirtualQueryInfo;
                            has_sdk_version_5:Boolean);
var
 obj:vm_map_object;
 start:QWORD;
 relofs:Int64;

 offset:QWORD;

 d_start,d_start2:QWORD;
 d_end,d_end2:QWORD;
 d_mtype:DWORD;

 ret:Integer;
 otype:objtype_t;
begin
 qinfo^:=Default(SceKernelVirtualQueryInfo);

 if is_valid_entry(entry) then
 begin
  if (entry^.name[0]<>#0) then
  begin
   qinfo^.name:=entry^.name;
  end else
  begin
   qinfo^.name:='anon:'+LowerCase(HexStr(QWORD(entry^.anon_addr),12));
  end;
 end;

 obj:=entry^.vm_obj;

 if (obj<>nil) then
 if (obj^.otype=OBJT_BLOCKPOOL) then
 begin
  qinfo^.bits.isPooledMemory:=1;

  qinfo^.pstart    :=Pointer(entry^.start);
  qinfo^.p__end    :=Pointer(entry^.__end);

  ret:=blockpool_obj_get_info(map,obj,addr,qinfo,has_sdk_version_5);

  qinfo^.bits.isCommitted:=ret;
  qinfo^.offset:=QWORD(qinfo^.pstart) - entry^.start;
  Exit;
 end;

 qinfo^.pstart    :=Pointer(entry^.start);
 qinfo^.p__end    :=Pointer(entry^.__end);
 qinfo^.protection:=entry^.max_protection and entry^.protection;
 qinfo^.memoryType:=0;

 if (obj<>nil) then
 begin

  if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
  begin
   offset:=entry^.offset;

   //exclude VM_PROT_EXECUTE
   qinfo^.protection:=qinfo^.protection and (VM_PROT_GPU_ALL or VM_PROT_RW);

   start :=entry^.start;
   relofs:=offset - start;

   if (addr < start) then
   begin
    addr:=start;
   end;

   ret:=dmem_map_get_mtype(dmem_maps[p_proc.p_dmem_pool_id].dmem,
                           obj,
                           addr + (entry^.offset - start), //send not transformed offset
                           @d_start2,@d_end2,
                           @d_mtype);
   if (ret<>0) then
   begin
    Assert(false,'dmem_vmo_get_type error %d');
   end;

   qinfo^.bits.isDirectMemory:=1;
   qinfo^.bits.isCommitted   :=1;

   d_start:=(d_start2 - relofs);
   if ((d_start2 - relofs) <= entry^.start) then
   begin
    d_start:=entry^.start;
   end;

   qinfo^.pstart:=Pointer(d_start);
   d_end:=(d_end2 - relofs);
   if (entry^.__end <= (d_end2 - relofs)) then
   begin
    d_end:=entry^.__end;
   end;

   qinfo^.p__end    :=Pointer(d_end);
   qinfo^.memoryType:=d_mtype;
   qinfo^.offset    :=d_start + relofs;

   Exit;
  end;

  otype:=obj^.otype;
  if (OBJT_PHYSHM < otype) then Exit;

  case otype of
   OBJT_DEFAULT:
     begin
      //fake shared
      Exit;
     end;
   OBJT_SWAP   ,
   OBJT_VNODE  ,
   OBJT_JITSHM ,
   OBJT_SELF   :; //skip
   OBJT_PHYSHM :
     begin
      qinfo^.memoryType:=get_obj_mtype(obj);
      qinfo^.offset    :=entry^.offset;
      qinfo^.bits.isCommitted:=1;
     end;
   else
     begin
      Exit;
     end;
  end;

 end;

 if (entry^.max_protection<>0) then
 begin
  qinfo^.bits.isFlexibleMemory:=1;

  if (entry^.wired_count>0) then
  begin
   qinfo^.bits.isCommitted:=1;
  end;

  if ((entry^.eflags and (MAP_ENTRY_GROWS_DOWN or MAP_ENTRY_GROWS_UP))<>0) then
  begin
   qinfo^.bits.isStack:=1;
  end;
 end;

end;

function _get_bits_str(var qinfo:SceKernelVirtualQueryInfo):RawByteString; inline;
var
 _F:array[0..1] of Char='_F';
 _D:array[0..1] of Char='_D';
 _S:array[0..1] of Char='_S';
 _P:array[0..1] of Char='_P';
 _C:array[0..1] of Char='_C';
begin
 Result:=_F[qinfo.bits.isFlexibleMemory]+
         _D[qinfo.bits.isDirectMemory  ]+
         _S[qinfo.bits.isStack         ]+
         _P[qinfo.bits.isPooledMemory  ]+
         _C[qinfo.bits.isCommitted     ];
end;

function sys_virtual_query(addr:Pointer;
                           flags:DWORD;  //SCE_KERNEL_VQ_FIND_NEXT
                           info:Pointer; //pSceKernelVirtualQueryInfo
                           infoSize:QWORD):Integer;
label
 _next,
 _dmem_vmo_get_type;
var
 td:p_kthread;
 map:vm_map_t;
 entry,next:vm_map_entry_t;
 rbp:PPointer;
 rip:Pointer;
 has_sdk_version_5:Boolean;
 is_libsys_call:Boolean;
 is_found:Boolean;
 qinfo:SceKernelVirtualQueryInfo;
 size:QWORD;
 start:QWORD;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 //Writeln('sys_virtual_query:',HexStr(addr),' ',flags);

 QWORD(addr):=QWORD(addr) and QWORD(not PAGE_MASK);

 map:=p_proc.p_vmspace;

 if (vm_map_max(map) < QWORD(addr)) then
 begin
  Exit(EINVAL);
 end;

 //backtrace
 rbp:=Pointer(td^.td_frame.tf_rbp);

 repeat
  if ((QWORD(rbp) shr 47)<>0) then
  begin
   has_sdk_version_5:=(p_proc.p_sdk_version > $4ffffff);
   is_libsys_call   :=false;
   Break;
  end;

  rip:=md_fuword(rbp[1]);
  rbp:=md_fuword(rbp[0]);

  if (QWORD(rip)=QWORD(-1)) or
     (QWORD(rbp)=QWORD(-1)) then
  begin
   has_sdk_version_5:=(p_proc.p_sdk_version > $4ffffff);
   is_libsys_call   :=false;
   Break;
  end;

  if (p_proc.p_libkernel_start_addr >  rip) or
     (p_proc.p_libkernel___end_addr <= rip) then
  begin
   if ((QWORD(rip) - QWORD($7f0000000)) < QWORD($800000000)) then //ET_DYN_LOAD_ADDR_SYS
   begin
    has_sdk_version_5:=true;
    is_libsys_call   :=true;
   end else
   begin
    has_sdk_version_5:=(p_proc.p_sdk_version > $4ffffff);
    is_libsys_call   :=false;
   end;
   Break;
  end;

 until false;
 //backtrace

 vm_map_lock(map);

 vm_map_lookup_entry(map,QWORD(addr),@entry);

 entry:=next_valid_entry(map,entry);

 is_found:=(QWORD(addr)>=entry^.start) and (QWORD(addr)<entry^.__end);

 if not is_found then
 begin

  if ((flags and SCE_KERNEL_VQ_FIND_NEXT)<>0) then
  begin
   next:=next_valid_entry(map,entry^.next);
   if (next<>@map^.header) then
   begin
    addr :=Pointer(next^.start);
    entry:=next;
    goto _next;
   end;
  end;

  //not found
   vm_map_unlock(map);
   Exit(EACCES);
 end;

 _next:

 qinfo:=Default(SceKernelVirtualQueryInfo);

 if is_libsys_call or
    ((entry^.start shr 28) < 127) or
    (entry^.__end > QWORD($ff0000000)) then  //SCE_KERNEl_GNM_TESS_AREA
 begin
  _dmem_vmo_get_type:

  dmem_vmo_get_info(map,entry,QWORD(addr),@qinfo,has_sdk_version_5);

  vm_map_unlock(map);

  size:=$48;
  if (infoSize < $48) then
  begin
   size:=DWORD(infoSize);
  end;
 end else
 begin

  if IN_CUSALIST_1 then
  begin
   vm_map_unlock(map);

   if ((flags and SCE_KERNEL_VQ_FIND_NEXT)=0) then
   begin
    if (QWORD(addr) < QWORD($fecc6c000)) then Exit(EACCES);
   end;

   qinfo.pstart    :=Pointer($fecc6c000);
   qinfo.p__end    :=Pointer($ff0000000);
   qinfo.protection:=3;
   qinfo.memoryType:=0;
   qinfo.bits.isFlexibleMemory:=1;

   size:=DWORD(infoSize);
   if (infoSize > $47) then
   begin
    size:=$48;
   end;
  end else
  if IN_CUSALIST_2 then
  begin
   vm_map_unlock(map);

   if ((flags and SCE_KERNEL_VQ_FIND_NEXT)=0) then
   begin
    if (QWORD(addr) < QWORD($fee6bc000)) then
    begin
     Exit(EACCES);
    end;
   end;

   qinfo.pstart    :=Pointer($fee6bc000);
   qinfo.p__end    :=Pointer($ff0000000);
   qinfo.protection:=3;
   qinfo.memoryType:=0;
   qinfo.bits.isFlexibleMemory:=1;

   size:=DWORD(infoSize);
   if (infoSize > $47) then
   begin
    size:=$48;
   end;
  end else
  begin

   if ((flags and SCE_KERNEL_VQ_FIND_NEXT)=0) then
   begin
    vm_map_unlock(map);
    Exit(EACCES);
   end;

   start:=entry^.start;
   while (start > QWORD($7efffffff)) and (entry^.__end <= QWORD($ff0000000)) do
   begin
    next:=next_valid_entry(map,entry^.next);
    if (next<>@map^.header) then
    begin
     vm_map_unlock(map);
     Exit(EACCES);
    end;
    addr :=Pointer(next^.start);
    entry:=next;
    start:=QWORD(addr);
   end;

   goto _dmem_vmo_get_type;
  end;

 end;

 {
 Writeln('[qinfo]:',#13#10' pstart:',HexStr(qinfo.pstart)
                   ,#13#10' p__end:',HexStr(qinfo.p__end)
                   ,#13#10' offset:',HexStr(qinfo.offset,16)
                   ,#13#10' protec:',HexStr(qinfo.protection,2)
                   ,#13#10' flags :',_get_bits_str(qinfo)
                   ,#13#10' mtypes:',qinfo.memoryType
                   ,#13#10' name  :',qinfo.name
                   );
 }


 Result:=copyout(@qinfo,info,size);
end;

function sys_query_memory_protection(addr:Pointer;info:Pointer):Integer;
label
 _simple;
var
 map:vm_map_t;
 _addr:vm_offset_t;
 __end:vm_offset_t;
 entry:vm_map_entry_t;
 data:t_query_memory_prot;
 qinfo:SceKernelVirtualQueryInfo;
 is_found:Boolean;
begin
 Result:=EINVAL;
 _addr:=trunc_page(vm_offset_t(addr));
 map:=p_proc.p_vmspace;
 __end:=vm_map_max(map);

 if (_addr<__end) or (_addr=__end) then
 begin
  vm_map_lock(map);

  vm_map_lookup_entry(map,QWORD(addr),@entry);

  entry:=next_valid_entry(map,entry);

  is_found:=(QWORD(addr)>=entry^.start) and (QWORD(addr)<entry^.__end);

  if not is_found then
  begin
   vm_map_unlock(map);
   Result:=EACCES;
  end else
  begin

   if (entry^.vm_obj=nil) or
      (p_proc.p_sdk_version < $10000000) then
   begin
    //private
    _simple:

    data.start:=Pointer(entry^.start);
    data.__end:=Pointer(entry^.__end);
    data.prot:=(entry^.max_protection and entry^.protection);
    data.eflags:=entry^.eflags;

    if (entry^.vm_obj<>nil) then
    if ((entry^.vm_obj^.flags and OBJ_DMEM_EXT)<>0) then
    begin
     //exclude VM_PROT_EXECUTE
     data.prot:=data.prot and (VM_PROT_GPU_ALL or VM_PROT_RW);
    end;

   end else
   if (entry^.vm_obj^.otype<>OBJT_BLOCKPOOL) then
   begin
    //object
    goto _simple;
   end else
   begin
    dmem_vmo_get_info(map,entry,QWORD(addr),@qinfo,True);
    data.start:=qinfo.pstart;
    data.__end:=qinfo.p__end;
    data.prot :=qinfo.protection;
    data.eflags:=entry^.eflags;
   end;

   vm_map_unlock(map);
   Result:=copyout(@data,info,SizeOf(t_query_memory_prot));
  end;
 end;
end;

function obj2dmem(obj:vm_object_t):p_dmem_map; public;
var
 dev:p_cdev;
 dmem_obj:p_dmem_obj;
begin
 Result:=nil;

 if (obj=nil) then Exit;

 dev:=obj^.handle;

 if (dev=nil) then Exit;

 dmem_obj:=dev^.si_drv1;

 if (dmem_obj=nil) then Exit;

 Result:=dmem_obj^.dmem;
end;

function get_dmem_ptr(addr:Pointer):Pointer;
begin
 Result:=addr+VM_MIN_GPU_ADDRESS;
end;



end.

