unit vm_patch_link;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_stub;

type
 t_patch_type=(pt_fsbase,pt_gsbase,pt_syscall,pt_unresolve,pt_jit);

 p_patch_node=^t_patch_node;
 t_patch_node=record
  link :TAILQ_ENTRY;
  page :TAILQ_ENTRY;
  vaddr:Pointer;
  vsize:Integer;
  ptype:t_patch_type;
  stub :p_stub_chunk;
 end;

function  vm_get_patch_link(vaddr:Pointer;vsize:Integer):p_stub_chunk;
function  vm_patch_exist(vaddr:Pointer;vsize:Integer):Boolean;

procedure vm_add_patch_link     (_obj,vaddr:Pointer;vsize:Integer;ptype:t_patch_type;stub:p_stub_chunk);
procedure vm_free_patch_link    (_obj:Pointer;node:p_patch_node);
procedure vm_object_patch_remove(_obj:Pointer;start,__end:DWORD);
function  vm_get_patch_link     (_obj,vaddr:Pointer):p_stub_chunk;
procedure vm_rem_patch_link     (_obj,vaddr:Pointer);

implementation

uses
 hamt,
 kern_rwlock,
 vmparam,
 vm_object;

type
 p_patch_page=^t_patch_page;
 t_patch_page=TAILQ_HEAD;

var
 hamt_lock:Pointer=nil;
 hamt_page:TSTUB_HAMT32;

function OFF_TO_IDX(x:Pointer):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

procedure hamt_insert_link(node:p_patch_node);
var
 data:PPointer;
 page:p_patch_page;
begin
 rw_wlock(hamt_lock);

 data:=HAMT_search32(@hamt_page,OFF_TO_IDX(node^.vaddr));

 if (data=nil) then
 begin
  page:=AllocMem(SizeOf(t_patch_page));
  TAILQ_INIT(page);
  data:=HAMT_insert32(@hamt_page,OFF_TO_IDX(node^.vaddr),page);
 end;

 page:=data^;

 TAILQ_INSERT_TAIL(page,node,@node^.page);

 rw_wunlock(hamt_lock);
end;

procedure hamt_remove_link(node:p_patch_node);
var
 data:PPointer;
 page:p_patch_page;
begin
 rw_wlock(hamt_lock);

 data:=HAMT_search32(@hamt_page,OFF_TO_IDX(node^.vaddr));

 if (data=nil) then
 begin
  rw_wunlock(hamt_lock);
  Exit;
 end;

 page:=data^;

 TAILQ_REMOVE(page,node,@node^.page);

 if TAILQ_EMPTY(page) then
 begin
  HAMT_delete32(@hamt_page,OFF_TO_IDX(node^.vaddr),nil);
  FreeMem(page);
 end;

 rw_wunlock(hamt_lock);
end;

function vm_get_patch_link(vaddr:Pointer;vsize:Integer):p_stub_chunk;
var
 data:PPointer;
 page:p_patch_page;
 entry,next:p_patch_node;
begin
 Result:=nil;

 rw_wlock(hamt_lock);

 data:=HAMT_search32(@hamt_page,OFF_TO_IDX(vaddr));

 if (data=nil) then
 begin
  rw_wunlock(hamt_lock);
  Exit;
 end;

 page:=data^;

 entry:=TAILQ_FIRST(page);
 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if ((vaddr+vsize)>entry^.vaddr) and (vaddr<(entry^.vaddr+entry^.vsize)) then
  begin
   Result:=entry^.stub;
   p_inc_ref(Result);
   //
   rw_wunlock(hamt_lock);
   Exit;
  end;
  //
  entry:=next;
 end;

 rw_wunlock(hamt_lock);
end;

function vm_patch_exist(vaddr:Pointer;vsize:Integer):Boolean;
var
 stub:p_stub_chunk;
begin
 stub:=vm_get_patch_link(vaddr,vsize);

 if (stub<>nil) then
 begin
  p_dec_ref(stub);
  Result:=True;
 end else
 begin
  Result:=False;
 end;
end;

procedure vm_add_patch_link(_obj,vaddr:Pointer;vsize:Integer;ptype:t_patch_type;stub:p_stub_chunk);
var
 obj:vm_object_t;
 node:p_patch_node;
begin
 //Writeln('patch:vaddr=0x',HexStr(vaddr),' type:',ptype);

 obj:=_obj;
 node:=AllocMem(SizeOf(t_patch_node));
 node^.vaddr:=vaddr;
 node^.vsize:=vsize;
 node^.ptype:=ptype;
 node^.stub :=stub;

 p_inc_ref(stub);

 VM_OBJECT_LOCK(obj);
 TAILQ_INSERT_TAIL(@obj^.patchq,node,@node^.link);
 VM_OBJECT_UNLOCK(obj);

 hamt_insert_link(node);
end;

procedure vm_free_patch_link(_obj:Pointer;node:p_patch_node);
var
 obj:vm_object_t;
begin
 obj:=_obj;
 TAILQ_REMOVE(@obj^.patchq,node,@node^.link);

 hamt_remove_link(node);

 p_dec_ref(node^.stub);
 FreeMem(node);
end;

procedure vm_object_patch_remove(_obj:Pointer;start,__end:DWORD);
var
 obj:vm_object_t;
 entry,next:p_patch_node;
begin
 obj:=_obj;

 VM_OBJECT_LOCK(obj);

 entry:=TAILQ_FIRST(@obj^.patchq);
 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if ((start=0) or (OFF_TO_IDX(entry^.vaddr)>=start)) and
     ((__end=0) or (OFF_TO_IDX(entry^.vaddr)<=__end)) then
  begin
   vm_free_patch_link(_obj,entry);
  end;
  //
  entry:=next;
 end;

 VM_OBJECT_UNLOCK(obj);
end;

function vm_get_patch_link(_obj,vaddr:Pointer):p_stub_chunk;
var
 obj:vm_object_t;
 entry,next:p_patch_node;
begin
 Result:=nil;
 obj:=_obj;

 VM_OBJECT_LOCK(obj);

 entry:=TAILQ_FIRST(@obj^.patchq);
 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if (entry^.vaddr=vaddr) then
  begin
   Result:=entry^.stub;
   p_inc_ref(Result);
   //
   VM_OBJECT_UNLOCK(obj);
   Exit;
  end;
  //
  entry:=next;
 end;

 VM_OBJECT_UNLOCK(obj);
end;

procedure vm_rem_patch_link(_obj,vaddr:Pointer);
var
 obj:vm_object_t;
 entry,next:p_patch_node;
begin
 obj:=_obj;

 VM_OBJECT_LOCK(obj);

 entry:=TAILQ_FIRST(@obj^.patchq);
 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if (entry^.vaddr=vaddr) then
  begin
   vm_free_patch_link(_obj,entry);
  end;
  //
  entry:=next;
 end;

 VM_OBJECT_UNLOCK(obj);
end;

end.

