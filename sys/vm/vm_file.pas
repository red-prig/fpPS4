unit vm_file;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam;

type
 t_uncb=function(base:Pointer;size:QWORD):Integer;

 p_vm_file_obj=^vm_file_obj;
 vm_file_obj=packed record
  base:Pointer;
  size:QWORD;
  refs:QWORD;
  uncb:t_uncb;
 end;

 pp_vm_file_entry=^p_vm_file_entry;
 p_vm_file_entry=^vm_file_entry;
 vm_file_entry=packed record
  prev  :p_vm_file_entry;
  next  :p_vm_file_entry;
  left  :p_vm_file_entry;
  right :p_vm_file_entry;
  start :QWORD;
  __end :QWORD;
  offset:QWORD;
  obj   :p_vm_file_obj;
 end;

 p_vm_file_map=^vm_file_map;
 vm_file_map=packed object
  header  :vm_file_entry;
  root    :p_vm_file_entry;
  nentries:DWORD;
  pages   :DWORD;
  property  min_offset:QWORD read header.start write header.start;
  property  max_offset:QWORD read header.__end write header.__end;
 end;

function  vm_file_obj_allocate  (base:Pointer;size:QWORD;uncb:t_uncb):p_vm_file_obj;
procedure vm_file_obj_destroy   (obj:p_vm_file_obj);
procedure vm_file_obj_reference (obj:p_vm_file_obj);
procedure vm_file_obj_deallocate(obj:p_vm_file_obj);

procedure vm_file_map_init(map:p_vm_file_map;min,max:QWORD);
procedure vm_file_map_free(map:p_vm_file_map);

function  vm_file_map_lookup_entry(
            map    :p_vm_file_map;
            address:QWORD;
            entry  :pp_vm_file_entry):Boolean;

function  vm_file_map_insert(
            map   :p_vm_file_map;
            obj   :p_vm_file_obj;
            offset:QWORD;
            start :QWORD;
            __end :QWORD):Integer;

function  vm_file_map_delete(map:p_vm_file_map;
                             start:QWORD;
                             __end:QWORD):Integer;

implementation

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function vm_file_obj_allocate(base:Pointer;size:QWORD;uncb:t_uncb):p_vm_file_obj;
begin
 Result:=AllocMem(SizeOf(vm_file_obj));

 Result^.base:=base;
 Result^.size:=size;
 Result^.refs:=1;
 Result^.uncb:=uncb;
end;

procedure vm_file_obj_destroy(obj:p_vm_file_obj);
begin
 if (obj^.uncb<>nil) then
 begin
  obj^.uncb(obj^.base,obj^.size);
 end;

 FreeMem(obj);
end;

procedure vm_file_obj_reference(obj:p_vm_file_obj);
begin
 if (obj=nil) then Exit;

 System.InterlockedIncrement64(obj^.refs);
end;

procedure vm_file_obj_deallocate(obj:p_vm_file_obj);
begin
 if (obj=nil) then Exit;

 if (System.InterlockedDecrement64(obj^.refs)=0) then
 begin
  vm_file_obj_destroy(obj);
 end;
end;

procedure vm_file_map_init(map:p_vm_file_map;min,max:QWORD);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset :=min;
 map^.max_offset :=max;
 map^.root       :=nil;
 map^.nentries   :=0;
 map^.pages      :=0;
end;

procedure vm_file_map_free(map:p_vm_file_map);
begin
 vm_file_map_delete(map,map^.min_offset,map^.max_offset);
end;

procedure vm_file_entry_dispose(map:p_vm_file_map;entry:p_vm_file_entry); inline;
begin
 vm_file_obj_deallocate(entry^.obj);
 FreeMem(entry);
end;

function vm_file_entry_create(map:p_vm_file_map):p_vm_file_entry;
var
 new_entry:p_vm_file_entry;
begin
 new_entry:=AllocMem(SizeOf(vm_file_entry));
 Assert((new_entry<>nil),'vm_file_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

function vm_file_entry_splay(addr:QWORD;root:p_vm_file_entry):p_vm_file_entry;
var
 llist,rlist:p_vm_file_entry;
 ltree,rtree:p_vm_file_entry;
 y          :p_vm_file_entry;
begin
 if (root=nil) then Exit(root);

 llist:=nil;
 rlist:=nil;
 repeat
  if (addr<root^.start) then
  begin
   y:=root^.left;
   if (y=nil) then break;
   if (addr<y^.start) and (y^.left<>nil) then
   begin
    root^.left:=y^.right;
    y^.right:=root;
    root:=y^.left;
    y^.left:=rlist;
    rlist:=y;
   end else
   begin
    root^.left:=rlist;
    rlist:=root;
    root:=y;
   end;
  end else
  if (addr>=root^.__end) then
  begin
   y:=root^.right;
   if (y=nil) then break;
   if (addr>=y^.__end) and (y^.right<>nil) then
   begin
    root^.right:=y^.left;
    y^.left:=root;
    root:=y^.right;
    y^.right:=llist;
    llist:=y;
   end else
   begin
    root^.right:=llist;
    llist:=root;
    root:=y;
   end;
  end else
  begin
   break;
  end;
 until false;

 ltree:=root^.left;
 while (llist<>nil) do
 begin
  y:=llist^.right;
  llist^.right:=ltree;
  ltree:=llist;
  llist:=y;
 end;

 rtree:=root^.right;
 while (rlist<>nil) do
 begin
  y:=rlist^.left;
  rlist^.left:=rtree;
  rtree:=rlist;
  rlist:=y;
 end;

 root^.left:=ltree;
 root^.right:=rtree;

 Result:=(root);
end;

procedure vm_file_map_entry_link(
           map        :p_vm_file_map;
           after_where:p_vm_file_entry;
           entry      :p_vm_file_entry);
begin
 Inc(map^.nentries);

 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   vm_file_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left:=after_where;
  after_where^.right:=nil;
 end else
 begin
  entry^.right:=map^.root;
  entry^.left:=nil;
 end;
 map^.root:=entry;
end;

procedure vm_file_map_entry_unlink(
           map  :p_vm_file_map;
           entry:p_vm_file_entry);
var
 next,prev,root:p_vm_file_entry;
begin
 if (entry<>map^.root) then
 begin
  vm_file_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_file_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

function vm_file_map_lookup_entry(
           map    :p_vm_file_map;
           address:QWORD;
           entry  :pp_vm_file_entry):Boolean;
var
 cur:p_vm_file_entry;
begin
 cur:=map^.root;
 if (cur=nil) then
 begin
  entry^:=@map^.header;
 end else
 if (address>=cur^.start) and (cur^.__end>address) then
 begin
  entry^:=cur;
  Exit(TRUE);
 end else
 begin
  cur:=vm_file_entry_splay(address,cur);
  map^.root:=cur;

  if (address>=cur^.start) then
  begin
   entry^:=cur;
   if (cur^.__end>address) then
   begin
    Exit(TRUE);
   end;
  end else
  begin
   entry^:=cur^.prev;
  end;
 end;
 Result:=(FALSE);
end;

function vm_file_map_insert(
           map   :p_vm_file_map;
           obj   :p_vm_file_obj;
           offset:QWORD;
           start :QWORD;
           __end :QWORD):Integer;
var
 new_entry :p_vm_file_entry;
 prev_entry:p_vm_file_entry;
 temp_entry:p_vm_file_entry;
begin
 if (start>=__end) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 if vm_file_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 new_entry:=vm_file_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 new_entry^.offset:=offset;
 new_entry^.obj   :=obj;

 vm_file_map_entry_link(map, prev_entry, new_entry);
 map^.pages:=map^.pages+OFF_TO_IDX(new_entry^.__end - new_entry^.start);
end;

procedure vm_file_map_entry_delete(map:p_vm_file_map;entry:p_vm_file_entry);
begin
 vm_file_map_entry_unlink(map, entry);

 map^.pages:=map^.pages-OFF_TO_IDX(entry^.__end - entry^.start);

 vm_file_entry_dispose(map,entry);
end;

procedure _vm_map_clip_start(map:p_vm_file_map;entry:p_vm_file_entry;start:QWORD);
var
 new_entry:p_vm_file_entry;
begin
 new_entry:=vm_file_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.offset:=entry^.offset + (start - entry^.start);
 entry^.start:=start;

 vm_file_map_entry_link(map, entry^.prev, new_entry);

 vm_file_obj_reference(new_entry^.obj);
end;

procedure vm_map_clip_start(map:p_vm_file_map;entry:p_vm_file_entry;start:QWORD);
begin
 if (start>entry^.start) then
 begin
  _vm_map_clip_start(map,entry,start);
 end;
end;

procedure _vm_map_clip_end(map:p_vm_file_map;entry:p_vm_file_entry;__end:QWORD);
var
 new_entry:p_vm_file_entry;
begin
 new_entry:=vm_file_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end:=__end;
 new_entry^.offset:=new_entry^.offset + (__end - entry^.start);

 vm_file_map_entry_link(map, entry, new_entry);

 vm_file_obj_reference(new_entry^.obj);
end;

procedure vm_map_clip_end(map:p_vm_file_map;entry:p_vm_file_entry;__end:QWORD);
begin
 if (__end<entry^.__end) then
 begin
  _vm_map_clip_end(map,entry,__end);
 end;
end;

function vm_file_map_delete(map:p_vm_file_map;
                            start:QWORD;
                            __end:QWORD):Integer;
var
 entry      :p_vm_file_entry;
 first_entry:p_vm_file_entry;
 next       :p_vm_file_entry;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (not vm_file_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  vm_map_clip_start(map, entry, start);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  vm_map_clip_end(map, entry, __end);
  next:=entry^.next;
  vm_file_map_entry_delete(map, entry);
  entry:=next;
 end;
 Result:=(KERN_SUCCESS);
end;

end.

