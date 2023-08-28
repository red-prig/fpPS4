unit vm_patch_link;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_stub;

type
 t_patch_type=(pt_fsbase,pt_gsbase,pt_syscall,pt_unresolve,pt_jit);

 p_patch_info=^t_patch_info;
 t_patch_info=record
  vaddr:Pointer;
  vsize:Integer;
  ptype:t_patch_type;
  stub :p_stub_chunk;
 end;

 p_patch_node=^t_patch_node;
 t_patch_node=record
  link:TAILQ_ENTRY;
  page:TAILQ_ENTRY;
  info:t_patch_info;
 end;

function  vm_get_patch_link(vaddr:Pointer;vsize:Integer):t_patch_info;
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

 data:=HAMT_search32(@hamt_page,OFF_TO_IDX(node^.info.vaddr));

 if (data=nil) then
 begin
  page:=AllocMem(SizeOf(t_patch_page));
  TAILQ_INIT(page);
  data:=HAMT_insert32(@hamt_page,OFF_TO_IDX(node^.info.vaddr),page);
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

 data:=HAMT_search32(@hamt_page,OFF_TO_IDX(node^.info.vaddr));

 if (data=nil) then
 begin
  rw_wunlock(hamt_lock);
  Exit;
 end;

 page:=data^;

 TAILQ_REMOVE(page,node,@node^.page);

 if TAILQ_EMPTY(page) then
 begin
  HAMT_delete32(@hamt_page,OFF_TO_IDX(node^.info.vaddr),nil);
  FreeMem(page);
 end;

 rw_wunlock(hamt_lock);
end;

function info_cross(info:p_patch_info;vaddr:Pointer;vsize:Integer):Boolean; inline;
begin
 Result:=((vaddr+vsize)>info^.vaddr) and (vaddr<(info^.vaddr+info^.vsize));
end;

function _vm_get_patch_link(page:p_patch_page;vaddr:Pointer):t_patch_info;
var
 entry,next:p_patch_node;
begin
 Result:=Default(t_patch_info);
 if (page=nil) then Exit;

 entry:=TAILQ_FIRST(page);
 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if (entry^.info.vaddr=vaddr) then
  begin
   Result:=entry^.info;
   p_inc_ref(Result.stub);
  end;
  //
  entry:=next;
 end;
end;

function _vm_get_patch_link(page:p_patch_page;vaddr:Pointer;vsize:Integer):t_patch_info;
var
 entry,next:p_patch_node;
begin
 Result:=Default(t_patch_info);
 if (page=nil) then Exit;

 entry:=TAILQ_FIRST(page);
 while (entry<>nil) do
 begin
  next:=TAILQ_NEXT(entry,@entry^.link);
  //
  if info_cross(@entry^.info,vaddr,vsize) then
  begin
   Result:=entry^.info;
   p_inc_ref(Result.stub);
  end;
  //
  entry:=next;
 end;
end;

function vm_get_patch_link(vaddr:Pointer;vsize:Integer):t_patch_info;
var
 off1,off2:DWORD;
 data:PPointer;
 page:p_patch_page;
 entry,next:p_patch_node;
begin
 Result:=Default(t_patch_info);

 off1:=OFF_TO_IDX(vaddr);

 rw_wlock(hamt_lock);

 data:=HAMT_search32(@hamt_page,off1);

 if (data=nil) then
 begin
  page:=nil;
 end else
 begin
  page:=data^;
 end;

 if (vsize=0) then
 begin
  Result:=_vm_get_patch_link(page,vaddr);
 end else
 begin
  Result:=_vm_get_patch_link(page,vaddr,vsize);

  off2:=OFF_TO_IDX(vaddr+vsize-1);

  if (Result.stub=nil) and (off1<>off2) then
  begin
   data:=HAMT_search32(@hamt_page,off2);

   if (data=nil) then
   begin
    page:=nil;
   end else
   begin
    page:=data^;
   end;

   Result:=_vm_get_patch_link(page,vaddr,vsize);
  end;
 end;

 rw_wunlock(hamt_lock);
end;

function vm_patch_exist(vaddr:Pointer;vsize:Integer):Boolean;
var
 info:t_patch_info;
begin
 info:=vm_get_patch_link(vaddr,vsize);

 if (info.stub<>nil) then
 begin
  p_dec_ref(info.stub);
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
 node^.info.vaddr:=vaddr;
 node^.info.vsize:=vsize;
 node^.info.ptype:=ptype;
 node^.info.stub :=stub;

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

 p_dec_ref(node^.info.stub);
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
  if ((start=0) or (OFF_TO_IDX(entry^.info.vaddr)>=start)) and
     ((__end=0) or (OFF_TO_IDX(entry^.info.vaddr)< __end)) then
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
  if (entry^.info.vaddr=vaddr) then
  begin
   Result:=entry^.info.stub;
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
  if (entry^.info.vaddr=vaddr) then
  begin
   vm_free_patch_link(_obj,entry);
  end;
  //
  entry:=next;
 end;

 VM_OBJECT_UNLOCK(obj);
end;

end.

