unit kern_dmem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_vm_object,
 dmem_map,
 rmem_map;

type
 pSceKernelDirectMemoryQueryInfo=^SceKernelDirectMemoryQueryInfo;
 SceKernelDirectMemoryQueryInfo=packed record
  start:QWORD;
  __end:QWORD;
  mType:Integer;
  align:Integer;
 end;

const
 SCE_KERNEL_VIRTUAL_RANGE_NAME_SIZE=32;
 SCE_KERNEL_DMQ_FIND_NEXT=1;
 SCE_KERNEL_VQ_FIND_NEXT =1;

type
 pSceKernelVirtualQueryInfo=^SceKernelVirtualQueryInfo;
 SceKernelVirtualQueryInfo=packed record
  pstart:Pointer;
  p__end:Pointer;
  offset:QWORD;
  protection:Integer;
  memoryType:Integer;
  bits:bitpacked record
   isFlexibleMemory:0..1; //1
   isDirectMemory  :0..1; //2
   isStack         :0..1; //4
   isPooledMemory  :0..1; //8
   isCommitted     :0..1; //16
  end;
  name:array[0..SCE_KERNEL_VIRTUAL_RANGE_NAME_SIZE-1] of AnsiChar;
  align:array[0..6] of Byte;
 end;

const
 //SceKernelMapEntryOperation
 SCE_KERNEL_MAP_OP_MAP_DIRECT  =0;
 SCE_KERNEL_MAP_OP_UNMAP       =1;
 SCE_KERNEL_MAP_OP_PROTECT     =2;
 SCE_KERNEL_MAP_OP_MAP_FLEXIBLE=3;
 SCE_KERNEL_MAP_OP_TYPE_PROTECT=4;

type
 pSceKernelBatchMapEntry=^SceKernelBatchMapEntry;
 SceKernelBatchMapEntry=packed record
  start:Pointer;
  offset:QWORD;
  length:QWORD;
  protection:Byte;
  mtype:Byte;
  pad1:Word;
  operation:Integer;
 end;

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

function  rmem_map_test_lock(start,__end:QWORD):Boolean;

function  get_dmem_offset(addr:QWORD;p_offset,p_size:PQWORD):Boolean;
function  get_dmem_ptr(addr:Pointer;p_ptr:PPointer;p_size:PQWORD):Boolean;

implementation

uses
 errno,
 systm,
 vm,
 vmparam,
 vm_map,
 kern_authinfo,
 kern_thr,
 kern_proc;

//////////

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
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
 dmem_map_init(@dmem,0,{SCE_KERNEL_MAIN_DMEM_SIZE} (VM_MAX_GPU_ADDRESS-VM_MIN_GPU_ADDRESS) );
 rmem_map_init(@rmap,0,{SCE_KERNEL_MAIN_DMEM_SIZE} (VM_MAX_GPU_ADDRESS-VM_MIN_GPU_ADDRESS) );

 vmap:=p_proc.p_vmspace;

 vmap^.rmap:=@rmap;

 dmem.vmap:=vmap;
 dmem.rmap:=@rmap;

 rmap.tmap:=@vmap^.pmap^.tr_map;
end;

const
 default_pool_id=1;

function sys_dmem_container(d_pool_id:Integer):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 td^.td_retval[0]:=default_pool_id;
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

function rmem_map_test_lock(start,__end:QWORD):Boolean;
begin
 rmem_map_lock(@rmap);
  Result:=rmem_map_test(@rmap,OFF_TO_IDX(start),OFF_TO_IDX(__end));
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
                        flags :DWORD):Integer;
label
 _fixed,
 _rmap_insert;
var
 dmap:t_dmem_obj;

 v_end:QWORD;
 faddr:QWORD;

 entry,next:vm_map_entry_t;

 cow:Integer;
 err:Integer;

 found:Boolean;
begin
 Result:=0;
 addr^:=0;

 if (((phaddr shr 36) > 4) or ((max_valid - phaddr) < length)) then
 begin
  Exit(EACCES);
 end;

 dmap:=dmem_maps[default_pool_id];

 //entry->eflags = flags & 0x400000 | 0x20000 | 0x80000
 //0x400000 -> MAP_ENTRY_NO_COALESCE -> MAP_NO_COALESCE
 //0x20000  -> not simplify ???
 //0x80000  -> ???

 cow:=(flags and MAP_NO_COALESCE);

 vm_map_lock(map);

 if (align=0) then
 begin
  //MAP_FIXED
  _fixed:

  v_end:=vaddr+length;

  if (v_end <= map^.max_offset) and
     ( ((flags and MAP_SANITIZER)<>0) or
       ((vaddr shr 47) <> 0) or
       (v_end < QWORD($fc00000001)) or
       (sdk_version_big_20()=false) ) then
  begin
   found:=rmem_map_test_lock(phaddr,phaddr+length);

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
     vm_object_reference(dmap.vobj);

     err:=vm_map_insert(map, dmap.vobj, phaddr, vaddr, v_end, prot, VM_PROT_ALL, cow, ((p_proc.p_dmem_aliasing and 3)<>0));

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

 if (default_pool_id<>1) then
 begin
  Exit(Pointer(EOPNOTSUPP));
 end;

 addr:=vm_offset_t(vaddr);

 if ((addr and $3fff)<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((phaddr and $3fff)<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if (((flags and $e09fff6f) or (prot and $ffffffcc))<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if (length <= $3fff) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((length and $3fff)<>0) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((flags and MAP_SANITIZER)<>0) then
 begin
  Exit(Pointer(EINVAL)); //is_sanitizer()=0
  //Sanitizer
 end;

 map:=p_proc.p_vmspace;

 if ((flags and MAP_FIXED)=0) then
 begin
  if (addr=0) then
  begin
   if (p_proc.p_sce_replay_exec=0) then
   begin
    addr:=SCE_USR_HEAP_START;
   end else
   begin
    addr:=SCE_REPLAY_EXEC_START;
   end;
  end else
  if (p_proc.p_sce_replay_exec<>0) and
     (addr<QWORD($ff0000001)) and
     ((length+addr)>QWORD($7efffffff)) then
  begin
   addr:=$ff0000000;
  end;

  align:=(flags shr MAP_ALIGNMENT_SHIFT) and $1f;
  if (align<PAGE_SHIFT) then align:=1;
  align:=1 shl align;
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
                                flags));

 td^.td_retval[0]:=addr;
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
    Result:=False;
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
    Result:=False;
  else;
    Result:=False;
 end;
end;

function get_obj_mtype(obj:vm_map_object):Byte;
begin
 Result:=obj^.un_pager.physhm.mtype;
end;

procedure dmem_vmo_get_type(map:vm_map_t;
                            entry:vm_map_entry_t;
                            addr:QWORD;
                            qinfo:pSceKernelVirtualQueryInfo;
                            sdk_version_big_4ffffff:Boolean);
var
 obj:vm_map_object;
 start:QWORD;
 relofs:Int64;

 d_start,d_start2:QWORD;
 d_end,d_end2:QWORD;
 d_mtype:DWORD;

 ret:Integer;
 otype:objtype_t;
begin
 qinfo^:=Default(SceKernelVirtualQueryInfo);

 case entry^.inheritance of
  VM_INHERIT_PATCH:;
  VM_INHERIT_HOLE :;
  else
    begin
     qinfo^.name:=entry^.name;
    end;
 end;

 obj:=entry^.vm_obj;

 if (obj<>nil) and (obj^.otype=OBJT_BLOCKPOOL) then
 begin
  qinfo^.bits.isPooledMemory:=1;

  Assert(false,'dmem_vmo_get_type:OBJT_BLOCKPOOL');

  //qinfo^.bits:=qinfo->bits and $ef or ((ret1 and 1) shl 4);
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
   qinfo^.protection:=qinfo^.protection and (VM_PROT_GPU_ALL or VM_PROT_RW);

   start :=entry^.start;
   relofs:=entry^.offset - start;

   if (addr < start) then
   begin
    addr:=start;
   end;

   ret:=dmem_map_get_mtype(dmem_maps[default_pool_id].dmem,obj,addr + relofs,@d_start2,@d_end2,@d_mtype);
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
   OBJT_DEFAULT,
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

  //if wired_count>0 then
  begin
   qinfo^.bits.isCommitted:=1;
  end;

  if ((entry^.eflags and (MAP_ENTRY_GROWS_DOWN or MAP_ENTRY_GROWS_UP))<>0) then
  begin
   qinfo^.bits.isStack:=1;
  end;
 end;

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
 sdk_version_big_4ffffff:Boolean;
 is_libsys_call:Boolean;
 qinfo:SceKernelVirtualQueryInfo;
 size:QWORD;
 start:QWORD;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 Writeln('sys_virtual_query:',HexStr(addr),' ',flags);

 QWORD(addr):=QWORD(addr) and $ffffffffffffc000;

 map:=p_proc.p_vmspace;

 if (vm_map_max(map) < QWORD(addr)) then
 begin
  Exit(EINVAL);
 end;

 rbp:=Pointer(td^.td_frame.tf_rbp);

 repeat
  if ((QWORD(rbp) shr 47)<>0) then
  begin
   sdk_version_big_4ffffff:=(p_proc.p_sdk_version > $4ffffff);
   is_libsys_call        :=false;
   Break;
  end;

  rip:=fuword(rbp[1]);
  rbp:=fuword(rbp[0]);

  if (QWORD(rip)=QWORD(-1)) or
     (QWORD(rbp)=QWORD(-1)) then
  begin
   sdk_version_big_4ffffff:=(p_proc.p_sdk_version > $4ffffff);
   is_libsys_call         :=false;
   Break;
  end;

  if (p_proc.p_libkernel_start_addr >  rip) or
     (p_proc.p_libkernel___end_addr <= rip) then
  begin
   if ((Int64(rip) - Int64($7f0000000)) < Int64($800000000)) then //ET_DYN_LOAD_ADDR_SYS
   begin
     sdk_version_big_4ffffff:=true;
     is_libsys_call         :=true;
   end else
   begin
    sdk_version_big_4ffffff:=(p_proc.p_sdk_version > $4ffffff);
    is_libsys_call         :=false;
   end;
   Break;
  end;

 until false;

 vm_map_lock(map);

 if not vm_map_lookup_entry(map,QWORD(addr),@entry) then
 begin
  if ((flags and SCE_KERNEL_VQ_FIND_NEXT)<>0) then
  begin
   next:=entry^.next;
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
    (entry^.__end > QWORD($ff0000000)) then
 begin
  _dmem_vmo_get_type:

  dmem_vmo_get_type(map,entry,QWORD(addr),@qinfo,sdk_version_big_4ffffff);

  vm_map_unlock(map);

  size:=$48;
  if (infoSize < $48) then
  begin
   size:=infoSize and $ffffffff;
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

   size:=infoSize and $ffffffff;
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
    if (QWORD(addr) < QWORD($fee6bc000)) then Exit(EACCES);
   end;

   qinfo.pstart    :=Pointer($fee6bc000);
   qinfo.p__end    :=Pointer($ff0000000);
   qinfo.protection:=3;
   qinfo.memoryType:=0;
   qinfo.bits.isFlexibleMemory:=1;

   size:=infoSize and $ffffffff;
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
   while (start > QWORD($7efffffff)) and (entry^.__end < QWORD($ff0000001)) do
   begin
    next:=entry^.next;
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

 Result:=copyout(@qinfo,info,size);
end;

function get_dmem_offset(addr:QWORD;p_offset,p_size:PQWORD):Boolean;
var
 map:vm_map_t;
 entry:vm_map_entry_t;
 obj:vm_object_t;
 offset,size:QWORD;

 dmem:p_dmem_map;
 curr,next:p_dmem_map_entry;
begin
 Result:=False;
 map:=p_proc.p_vmspace;

 vm_map_lock(map);

 if vm_map_lookup_entry(map,addr,@entry) then
 begin
  obj:=entry^.vm_obj;

  if (obj<>nil) then
  begin
   Result:=(obj^.flags and OBJ_DMEM_EXT)<>0;
  end;

  if Result then
  begin
   offset:=entry^.offset;

   //transform by base addr
   offset:=offset + (QWORD(obj^.un_pager.map_base) - VM_MIN_GPU_ADDRESS);

   offset:=offset+(addr-entry^.start);

   if (p_offset<>nil) then
   begin
    p_offset^:=offset;
   end;

   if (p_size<>nil) then
   begin
    size:=entry^.__end-addr;

    dmem:=dmem_maps[default_pool_id].dmem;
    dmem_map_lock(dmem);

     if dmem_map_lookup_entry(dmem,OFF_TO_IDX(offset),@curr) then
     begin
      while True do
      begin
       next:=curr^.next;

       if (next=@dmem^.header) then
       begin
        Break;
       end;

       if (curr^.__end<>next^.start) then
       begin
        Break;
       end;

       curr:=next;
      end;

      size:=IDX_TO_OFF(curr^.__end)-offset;
     end;
    dmem_map_unlock(dmem);

    p_size^:=size;
   end;
  end;
 end;

 vm_map_unlock(map);
end;

function get_dmem_ptr(addr:Pointer;p_ptr:PPointer;p_size:PQWORD):Boolean;
var
 offset:QWORD;
begin
 offset:=0;

 Result:=get_dmem_offset(QWORD(addr),@offset,p_size);

 if Result then
 if (p_ptr<>nil) then
 begin
  p_ptr^:=Pointer(VM_MIN_GPU_ADDRESS+offset);
 end;
end;



end.

