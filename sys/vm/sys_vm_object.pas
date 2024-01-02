unit sys_vm_object;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vm,
 kern_mtx;

type
 obj_type=(
   OBJT_DEFAULT  ,//0
   OBJT_SWAP     ,//1
   OBJT_VNODE    ,//2
   OBJT_DEVICE   ,//3
   OBJT_PHYS     ,//4
   OBJT_DEAD     ,//5
   OBJT_SG       ,//6
   OBJT_JITSHM   ,//7
   OBJT_SELF     ,//8
   OBJT_TRCMEM   ,//9
   OBJT_PHYSHM   ,//10
   OBJT_BLOCKPOOL //11
  );

 objtype_t=obj_type;

 p_vm_object_t=^vm_object_t;
 vm_object_t=^t_vm_object;
 t_vm_object=packed record
  mtx               :mtx;
  patchq            :TAILQ_HEAD;              // list of patches
  size              :vm_pindex_t;             // Object size
  generation        :Integer;                 // generation ID
  ref_count         :Integer;                 // How many refs??
  otype             :objtype_t;               // type of pager
  pg_color          :Word;
  flags             :Word;                    // see below
  handle            :Pointer;
  paging_in_progress:Integer;
  map_base          :Pointer;
  un_pager:packed record
   vnp:packed record
    vnp_size:QWORD;
    writemappings:vm_ooffset_t;
   end;
   physhm:packed record
    mtype:Byte;
   end;
  end;
 end;

const
 //Flags
 OBJ_ACTIVE       =$0004; // active objects
 OBJ_DEAD         =$0008; // dead objects (during rundown)
 OBJ_NOSPLIT      =$0010; // dont split this object
 OBJ_PIPWNT       =$0040; // paging in progress wanted
 OBJ_MIGHTBEDIRTY =$0100; // object might be dirty, only for vnode
 OBJ_COLORED      =$1000; // pg_color is defined
 OBJ_ONEMAPPING   =$2000; // One USE (a single, non-forked) mapping flag
 OBJ_DISCONNECTWNT=$4000; // disconnect from vnode wanted
 OBJ_DMEM_EXT     =$8000;
 OBJ_DMEM_EXT2    =$0020;

 OBJPC_SYNC  =$1;  // sync I/O
 OBJPC_INVAL =$2;  // invalidate
 OBJPC_NOSYNC=$4;  // skip if PG_NOSYNC

 //The following options are supported by vm_object_page_remove().
 OBJPR_CLEANONLY=$1; // Don't remove dirty pages.
 OBJPR_NOTMAPPED=$2; // Don't unmap pages.
 OBJPR_NOTWIRED =$4; // Don't remove wired pages.

function  VM_OBJECT_MTX(obj:vm_object_t):p_mtx;
procedure VM_OBJECT_LOCK(obj:vm_object_t);
function  VM_OBJECT_TRYLOCK(obj:vm_object_t):Boolean;
procedure VM_OBJECT_UNLOCK(obj:vm_object_t);
function  VM_OBJECT_LOCKED(obj:vm_object_t):Boolean;
procedure VM_OBJECT_LOCK_ASSERT(obj:vm_object_t);

procedure vm_object_set_flag(obj:vm_object_t;bits:Word);
procedure vm_object_clear_flag(obj:vm_object_t;bits:Word);

function  vm_object_allocate(t:objtype_t;size:vm_pindex_t):vm_object_t;
procedure vm_object_destroy(obj:vm_object_t);
procedure vm_object_reference(obj:vm_object_t);

procedure vm_object_pip_add(obj:vm_object_t;i:word);
procedure vm_object_pip_subtract(obj:vm_object_t;i:word);
function  vm_object_type(obj:vm_object_t):obj_type;

procedure vm_object_patch_remove(_obj:Pointer;start,__end:DWORD); external;

procedure vm_object_deallocate(obj:vm_object_t); external;

procedure vm_object_collapse(obj:vm_object_t); external;

function  vm_object_coalesce(prev_object:vm_object_t;
                             prev_offset:vm_ooffset_t;
                             prev_size  :vm_ooffset_t;
                             next_size  :vm_ooffset_t;
                             reserved   :Boolean):Boolean; external;

procedure vm_object_page_remove(obj:vm_object_t;
                                start:vm_pindex_t;
                                __end:vm_pindex_t;
                                options:Integer); external;

function  vm_object_page_clean(obj:vm_object_t;
                               start,__end:vm_ooffset_t;
                               flags:Integer):Boolean; external;

implementation

uses
 vnode;

//

procedure vref(vp:p_vnode); external;

//

function VM_OBJECT_MTX(obj:vm_object_t):p_mtx;
begin
 Result:=@obj^.mtx;
end;

procedure VM_OBJECT_LOCK(obj:vm_object_t);
begin
 mtx_lock(obj^.mtx);
end;

function VM_OBJECT_TRYLOCK(obj:vm_object_t):Boolean;
begin
 Result:=mtx_trylock(obj^.mtx);
end;

procedure VM_OBJECT_UNLOCK(obj:vm_object_t);
begin
 mtx_unlock(obj^.mtx);
end;

function VM_OBJECT_LOCKED(obj:vm_object_t):Boolean;
begin
 Result:=(mtx_owned(obj^.mtx));
end;

procedure VM_OBJECT_LOCK_ASSERT(obj:vm_object_t);
begin
 Assert(mtx_owned(obj^.mtx));
end;

procedure vm_object_set_flag(obj:vm_object_t;bits:Word);
begin
 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.flags:=obj^.flags or bits;
end;

procedure vm_object_clear_flag(obj:vm_object_t;bits:Word);
begin
 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.flags:=obj^.flags and (not bits);
end;

function vm_object_allocate(t:objtype_t;size:vm_pindex_t):vm_object_t;
begin
 Result:=AllocMem(SizeOf(t_vm_object));

 mtx_init(Result^.mtx,'vm_object');

 TAILQ_INIT(@Result^.patchq);

 Result^.otype     :=t;
 Result^.size      :=size;
 Result^.generation:=1;
 Result^.ref_count :=1;

 if (t=OBJT_DEFAULT) then
 begin
  Result^.flags:=OBJ_ONEMAPPING;
 end;
end;

procedure vm_object_destroy(obj:vm_object_t);
begin
 mtx_destroy(obj^.mtx);
 FreeMem(obj);
end;

procedure vm_object_reference(obj:vm_object_t);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK(obj);

  Inc(obj^.ref_count);

  if (obj^.otype=OBJT_VNODE) then
  begin
   if (obj^.otype=OBJT_VNODE) then
   begin
    vref(obj^.handle);
   end;
  end;

 VM_OBJECT_UNLOCK(obj);
end;

procedure vm_object_pip_add(obj:vm_object_t;i:word);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.paging_in_progress:=obj^.paging_in_progress+i;
end;

procedure vm_object_pip_subtract(obj:vm_object_t;i:word);
begin
 if (obj=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(obj);
 obj^.paging_in_progress:=obj^.paging_in_progress-i;
end;

function vm_object_type(obj:vm_object_t):obj_type;
begin
 if (obj=nil) then Exit(OBJT_DEFAULT);

 Result:=obj^.otype;
end;


end.




