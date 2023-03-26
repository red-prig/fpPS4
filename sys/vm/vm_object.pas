unit vm_object;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vm,
 kern_mtx;

type
 obj_type=(
  OBJT_DEFAULT,
  OBJT_VNODE,
  OBJT_DMEM,
  OBJT_DEAD);

 objtype_t=obj_type;

 p_vm_object_t=^vm_object_t;
 vm_object_t=^_vm_object;
 _vm_object=packed record
  mtx:mtx;
  memq:TAILQ_HEAD;                    // list of resident pages
  root:Pointer;                       // root of the resident page splay tree
  size:vm_pindex_t;                   // Object size
  generation:Integer;                 // generation ID
  ref_count:Integer;                  // How many refs??
  otype:objtype_t;                    // type of pager
  flags:Word;                         // see below
  handle:Pointer;
  paging_in_progress:Integer;
  un_pager:packed record
   vnp:packed record
    vnp_size:QWORD;
    writemappings:vm_ooffset_t;
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

 OBJPC_SYNC  =$1;  // sync I/O
 OBJPC_INVAL =$2;  // invalidate
 OBJPC_NOSYNC=$4;  // skip if PG_NOSYNC

 //The following options are supported by vm_object_page_remove().
 OBJPR_CLEANONLY=$1; // Don't remove dirty pages.
 OBJPR_NOTMAPPED=$2; // Don't unmap pages.
 OBJPR_NOTWIRED =$4; // Don't remove wired pages.

procedure vm_object_reference (_object:vm_object_t);
function  vm_object_allocate  (t:objtype_t;size:vm_pindex_t):vm_object_t;
procedure vm_object_deallocate(_object:vm_object_t);
procedure vm_object_clear_flag(_object:vm_object_t;bits:Word);

procedure vm_object_pip_add(_object:vm_object_t;i:word);
procedure vm_object_pip_subtract(_object:vm_object_t;i:word);
procedure vm_object_pip_wakeup(_object:vm_object_t);

procedure VM_OBJECT_LOCK   (_object:vm_object_t);
function  VM_OBJECT_TRYLOCK(_object:vm_object_t):Boolean;
procedure VM_OBJECT_UNLOCK (_object:vm_object_t);
function  VM_OBJECT_LOCKED (_object:vm_object_t):Boolean;
procedure VM_OBJECT_LOCK_ASSERT(_object:vm_object_t);

function IDX_TO_OFF(x:DWORD):QWORD; inline;
function OFF_TO_IDX(x:QWORD):DWORD; inline;

function vm_object_coalesce(prev_object:vm_object_t;
                            prev_offset:vm_ooffset_t;
                            prev_size  :vm_ooffset_t;
                            next_size  :vm_ooffset_t;
                            reserved   :Boolean):Boolean;

implementation

uses
 vmparam;

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

procedure VM_OBJECT_LOCK(_object:vm_object_t);
begin
 mtx_lock(_object^.mtx);
end;

function VM_OBJECT_TRYLOCK(_object:vm_object_t):Boolean;
begin
 Result:=mtx_trylock(_object^.mtx);
end;

procedure VM_OBJECT_UNLOCK(_object:vm_object_t);
begin
 mtx_unlock(_object^.mtx);
end;

function VM_OBJECT_LOCKED(_object:vm_object_t):Boolean;
begin
 Result:=(mtx_owned(_object^.mtx));
end;

procedure VM_OBJECT_LOCK_ASSERT(_object:vm_object_t);
begin
 Assert(mtx_owned(_object^.mtx));
end;

procedure vm_object_clear_flag(_object:vm_object_t;bits:Word);
begin
 VM_OBJECT_LOCK_ASSERT(_object);
 _object^.flags:=_object^.flags and (not bits);
end;

function vm_object_allocate(t:objtype_t;size:vm_pindex_t):vm_object_t;
begin
 Result:=AllocMem(SizeOf(_vm_object));

 mtx_init(Result^.mtx);

 TAILQ_INIT(@Result^.memq);

 Result^.otype     :=t;
 Result^.size      :=size;
 Result^.generation:=1;
 Result^.ref_count :=1;
end;

procedure _vm_object_deallocate(_object:vm_object_t);
begin
 Case _object^.otype of
  OBJT_DEFAULT:;
  OBJT_VNODE  :;
  OBJT_DMEM   :;
  OBJT_DEAD   :;
 end;
 mtx_destroy(_object^.mtx);
 FreeMem(_object);
end;

procedure vm_object_reference(_object:vm_object_t);
begin
 if (_object=nil) then Exit;

 System.InterlockedIncrement(_object^.ref_count);
end;

procedure vm_object_deallocate(_object:vm_object_t);
begin
 if (_object=nil) then Exit;

 if (System.InterlockedDecrement(_object^.ref_count)=0) then
 begin
  _vm_object_deallocate(_object);
 end;
end;

procedure vm_object_destroy(_object:vm_object_t);
begin
 vm_object_deallocate(_object);
end;

procedure vm_object_pip_add(_object:vm_object_t;i:word);
begin
 if (_object=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(_object);
 _object^.paging_in_progress:=_object^.paging_in_progress+i;
end;

procedure vm_object_pip_subtract(_object:vm_object_t;i:word);
begin
 if (_object=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(_object);
 _object^.paging_in_progress:=_object^.paging_in_progress-i;
end;

procedure vm_object_pip_wakeup(_object:vm_object_t);
begin
 if (_object=nil) then Exit;

 VM_OBJECT_LOCK_ASSERT(_object);
 Dec(_object^.paging_in_progress);
 if ((_object^.flags and OBJ_PIPWNT)<>0) and (_object^.paging_in_progress=0) then
 begin
  vm_object_clear_flag(_object, OBJ_PIPWNT);
  //wakeup(_object);
 end;
end;


procedure vm_object_page_remove(_object:vm_object_t;
                                start:vm_pindex_t;
                                __end:vm_pindex_t;
                                options:Integer);
begin

end;

procedure vm_object_collapse(_object:vm_object_t);
begin
 //
end;

function vm_object_coalesce(prev_object:vm_object_t;
                            prev_offset:vm_ooffset_t;
                            prev_size  :vm_ooffset_t;
                            next_size  :vm_ooffset_t;
                            reserved   :Boolean):Boolean;
var
 next_pindex:vm_pindex_t;
begin
 if (prev_object=nil) then Exit(TRUE);

 VM_OBJECT_LOCK(prev_object);
 if (prev_object^.otype<>OBJT_DEFAULT) then
 begin
  VM_OBJECT_UNLOCK(prev_object);
  Exit(FALSE);
 end;

 {
  * Try to collapse the object first
  }
 vm_object_collapse(prev_object);

 prev_size:=prev_size shr PAGE_SHIFT;
 next_size:=next_size shr PAGE_SHIFT;
 next_pindex:=OFF_TO_IDX(prev_offset) + prev_size;

 if (prev_object^.ref_count > 1) and
    (prev_object^.size<>next_pindex) then
 begin
  VM_OBJECT_UNLOCK(prev_object);
  Exit(FALSE);
 end;

 {
  * Remove any pages that may still be in the object from a previous
  * deallocation.
  }
 if (next_pindex<prev_object^.size) then
 begin
  vm_object_page_remove(prev_object,next_pindex,next_pindex+next_size,0);
 end;

 {
  * Extend the object if necessary.
  }
 if (next_pindex + next_size > prev_object^.size) then
  prev_object^.size:=next_pindex + next_size;

 VM_OBJECT_UNLOCK(prev_object);
 Result:=(TRUE);
end;


end.

