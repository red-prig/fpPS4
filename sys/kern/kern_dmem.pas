unit kern_dmem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm_object,
 dmem_map,
 rmem_map;

var
 dobj:vm_object_t;
 dmem:t_dmem_map;
 rmap:t_rmem_map;

procedure init_dmem_map;

function  sys_dmem_container(d_pool_id:Integer):Integer;
function  sys_set_chicken_switches(flags:Integer):Integer;

function  sys_mmap_dmem(vaddr :Pointer;
                        length:QWORD;
                        mtype :DWORD;
                        prot  :DWORD;
                        flags :DWORD;
                        phaddr:QWORD):Pointer;

implementation

uses
 errno,
 vm,
 vmparam,
 vm_map,
 kern_thr;

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
begin
 dobj:=vm_object_allocate(OBJT_PHYS,OFF_TO_IDX(SCE_KERNEL_MAIN_DMEM_SIZE));
 dmem_map_init(@dmem,0,SCE_KERNEL_MAIN_DMEM_SIZE);
 rmem_map_init(@rmap,0,SCE_KERNEL_MAIN_DMEM_SIZE);
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
  //Result:=priv_check(td,0x2ad);
  //(param < 3)
  Exit(EPERM);
 end;
end;

function sys_set_chicken_switches(flags:Integer):Integer;
begin
 Writeln('[KERNEL] set_chicken_switches(',flags,')');
 p_proc.p_dmem_aliasing:=p_proc.p_dmem_aliasing or flags;
 //0x1 - kern_mmap_dmem ->
 //0x2 - kern_mmap_dmem ->
 Result:=0;
end;

function sdk_version_big_20():Boolean; inline;
begin
 Result:=p_proc.p_sdk_version > $2ffffff;
end;

function kern_mmap_dmem(map   :vm_map_t;
                        addr  :p_vm_offset_t;
                        phaddr:QWORD;
                        vaddr :QWORD;
                        length:QWORD;
                        mtype :DWORD;
                        prot  :DWORD;
                        align :DWORD;
                        flags :DWORD):Integer;
label
 _fixed;
var
 v_end:QWORD;
 faddr:QWORD;
 entry,next:vm_map_entry_t;

 rentry:p_rmem_map_entry;

 err:Integer;
begin
 Result:=0;

 if (((phaddr shr 36) > 4) or ((max_valid - phaddr) < length)) then
 begin
  Exit(EACCES);
 end;

 vm_map_lock(map);

 if (align=0) then
 begin
  //MAP_FIXED
  _fixed:

  v_end:=vaddr+length;

  if (v_end <= map^.max_offset) and
     ( ((flags and $200000)<>0) or
       ((vaddr shr 47) <> 0) or
       (v_end < $fc00000001) or
       (sdk_version_big_20()=false) ) then
  begin
   rmem_map_lock(@rmap);

   //
   if (not rmem_map_lookup_entry(@rmap,OFF_TO_IDX(phaddr),@rentry)) or //not found
      ((p_proc.p_dmem_aliasing and 1)<>0) then //aliasing
   begin
    err:=dmem_map_mtype(@dmem,OFF_TO_IDX(phaddr),OFF_TO_IDX(phaddr+length),mtype);

    if (err=0) then
    begin
     err:=vm_map_insert(map, dobj, phaddr, vaddr, v_end, prot, prot, 0);

     if (err=0) then
     begin
      err:=rmem_map_insert(@rmap, OFF_TO_IDX(vaddr), OFF_TO_IDX(phaddr), OFF_TO_IDX(phaddr+length));

      if (err=0) then
      begin
       //
      end else
      begin
       vm_map_delete(map,vaddr,v_end);
      end;

     end;
    end;

   end;

   rmem_map_unlock(@rmap);
  end else
  if ((p_proc.p_dmem_aliasing and 2)<>0) and //aliasing gpu???
     ((prot and VM_PROT_GPU_ALL)<>0) then
  begin
   Assert(False,'TODO');
  end else
  begin
   Writeln('[KERNEL] multiple VA mappings are detected. va:[0x',HexStr(vaddr,16),',0x',HexStr(v_end,16),')');
   Result:=EINVAL;
  end;

 end else
 begin
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
 addr:vm_offset_t;
 align:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(Pointer(-1));

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

 if ((flags and $200000)<>0) then
 begin
  //Sanitizer
 end;

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

  align:=flags and MAP_ALIGNMENT_SHIFT;
  if (align<PAGE_SHIFT) then align:=1;
 end else
 begin
  //Address range must be all in user VM space.
  if (addr < vm_map_min(@g_vmspace.vm_map)) or
     (addr + length > vm_map_max(@g_vmspace.vm_map)) then
  begin
   Exit(Pointer(EINVAL));
  end;

  if (addr+length<addr) then
  begin
   Exit(Pointer(EINVAL));
  end;

  align:=0;
 end;

 Result:=Pointer(kern_mmap_dmem(@g_vmspace.vm_map,
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




end.

