unit vm_patch_link;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_stub;

type
 t_patch_type=(pt_fsbase,pt_gsbase,pt_syscall,pt_unresolve);

 p_patch_node=^t_patch_node;
 t_patch_node=record
  link :TAILQ_ENTRY;
  vaddr:Pointer;
  ptype:t_patch_type;
  stub :p_stub_chunk;
 end;

procedure vm_add_patch_link     (_obj,vaddr:Pointer;ptype:t_patch_type;stub:p_stub_chunk);
procedure vm_free_patch_link    (_obj:Pointer;node:p_patch_node);
procedure vm_object_patch_remove(_obj:Pointer;start,__end:DWORD);
function  vm_get_patch_link     (_obj,vaddr:Pointer):p_stub_chunk;
procedure vm_rem_patch_link     (_obj,vaddr:Pointer);

implementation

uses
 vmparam,
 vm_object;

procedure vm_add_patch_link(_obj,vaddr:Pointer;ptype:t_patch_type;stub:p_stub_chunk);
var
 obj:vm_object_t;
 node:p_patch_node;
begin
 //Writeln('patch:vaddr=0x',HexStr(vaddr),' type:',ptype);

 obj:=_obj;
 node:=AllocMem(SizeOf(t_patch_node));
 node^.vaddr:=vaddr;
 node^.ptype:=ptype;
 node^.stub :=stub;

 p_inc_ref(stub);

 VM_OBJECT_LOCK(obj);
 TAILQ_INSERT_TAIL(@obj^.patchq,node,@node^.link);
 VM_OBJECT_UNLOCK(obj);
end;

procedure vm_free_patch_link(_obj:Pointer;node:p_patch_node);
var
 obj:vm_object_t;
begin
 obj:=_obj;
 TAILQ_REMOVE(@obj^.patchq,node,@node^.link);

 p_dec_ref(node^.stub);
 FreeMem(node);
end;

function OFF_TO_IDX(x:Pointer):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
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

