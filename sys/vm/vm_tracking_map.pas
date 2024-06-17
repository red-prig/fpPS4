unit vm_tracking_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 mqueue,
 kern_mtx;

type
 p_vm_track_interval=^t_vm_track_interval;
 t_vm_track_interval=record
  start:vm_offset_t;
  __end:vm_offset_t;
 end;

 t_on_destroy=function (handle:Pointer):Boolean;
 t_on_trigger=procedure(handle:Pointer;start,__end:vm_offset_t);

 p_vm_track_object=^t_vm_track_object;
 t_vm_track_object=record
  handle    :Pointer;
  ref_count :DWORD;
  timestamp :DWORD;
  main      :t_vm_track_interval;
  instances :TAILQ_HEAD; //p_vm_track_object_instance
  //
  on_destroy:t_on_destroy;
  on_trigger:t_on_trigger;
 end;

 pp_vm_track_map_entry=^p_vm_track_map_entry;
 p_vm_track_map_entry=^t_vm_track_map_entry;
 t_vm_track_map_entry=packed record
  prev       :p_vm_track_map_entry; // previous entry
  next       :p_vm_track_map_entry; // next entry
  left       :p_vm_track_map_entry; // left child in binary search tree
  right      :p_vm_track_map_entry; // right child in binary search tree
  start      :vm_offset_t;          // start address
  __end      :vm_offset_t;          // end address
  adj_free   :vm_offset_t;          // amount of adjacent free space
  max_free   :vm_offset_t;          // max free space in subtree
  instances  :TAILQ_HEAD;           // p_vm_track_object_instance
 end;

 p_vm_track_object_instance=^t_vm_track_object_instance;
 t_vm_track_object_instance=record
  entry_link:TAILQ_ENTRY; //p_vm_track_map_entry->instances
  obj_link  :TAILQ_ENTRY; //p_vm_track_object   ->instances
  entry     :p_vm_track_map_entry;
  obj       :p_vm_track_object;
 end;

 p_vm_track_map=^t_vm_track_map;
 t_vm_track_map=object
  header   :t_vm_track_map_entry; // List of entries
  lock     :mtx;                  // Lock for map data
  size     :vm_size_t;            // virtual size
  root     :p_vm_track_map_entry; // Root of a binary search tree
  nentries :Integer;              // Number of entries
  timestamp:DWORD;
  property  min_offset:vm_offset_t read header.start write header.start;
  property  max_offset:vm_offset_t read header.__end write header.__end;
 end;


//

function  vm_track_object_allocate  (handle:TObject;start,__end:vm_offset_t):p_vm_track_object;
procedure vm_track_object_deallocate(obj:p_vm_track_object);
procedure vm_track_object_reference (obj:p_vm_track_object);

//

function  vm_track_map_insert       (map:p_vm_track_map;start,__end:vm_offset_t;obj:p_vm_track_object):Integer;
function  vm_track_map_remove_object(map:p_vm_track_map;obj:p_vm_track_object):Integer;
function  vm_track_map_remove_memory(map:p_vm_track_map;start,__end:vm_offset_t):Integer;
function  vm_track_map_trigger      (map:p_vm_track_map;start,__end:vm_offset_t):Integer;

implementation

function vm_track_object_allocate(handle:TObject;start,__end:vm_offset_t):p_vm_track_object;
begin
 Result:=AllocMem(SizeOf(t_vm_track_object));

 Result^.handle   :=handle;
 Result^.ref_count:=1;

 TAILQ_INIT(@Result^.instances);

 Result^.main.start:=start;
 Result^.main.__end:=__end;
end;

procedure vm_track_object_destroy(obj:p_vm_track_object);
begin
 FreeMem(obj);
end;

procedure vm_track_object_deallocate(obj:p_vm_track_object);
begin
 if (obj=nil) then Exit;

 if (System.InterlockedDecrement(obj^.ref_count)=0) then
 begin

  if (obj^.on_destroy<>nil) then
  begin
   if not obj^.on_destroy(obj^.handle) then
   begin
    Exit;
   end;
  end;

  vm_track_object_destroy(obj);
 end;
end;

procedure vm_track_object_reference(obj:p_vm_track_object);
begin
 if (obj=nil) then Exit;

 System.InterlockedIncrement(obj^.ref_count);
end;

procedure vm_track_object_trigger(map:p_vm_track_map;obj:p_vm_track_object;start,__end:vm_offset_t);
begin
 if (obj=nil) then Exit;

 if (obj^.timestamp<>map^.timestamp) then
 begin
  obj^.timestamp:=map^.timestamp;

  if (obj^.on_trigger<>nil) then
  begin
   obj^.on_trigger(obj^.handle,start,__end);
  end;

 end;
end;

//

//

procedure _vm_track_entry_add_obj(entry:p_vm_track_map_entry;obj:p_vm_track_object);
var
 node:p_vm_track_object_instance;
begin
 node:=AllocMem(SizeOf(t_vm_track_object_instance));

 node^.entry:=entry;
 node^.obj  :=obj;

 TAILQ_INSERT_TAIL(@entry^.instances,node,@node^.entry_link);

 TAILQ_INSERT_TAIL(@obj^.instances,node,@node^.obj_link);

 vm_track_object_reference(obj);
end;

procedure vm_track_entry_add_obj(entry:p_vm_track_map_entry;obj:p_vm_track_object);
var
 node:p_vm_track_object_instance;
begin
 node:=TAILQ_FIRST(@entry^.instances);

 while (node<>nil) do
 begin

  if (node^.obj=obj) then
  begin
   Exit;
  end;

  node:=TAILQ_NEXT(node,@node^.entry_link);
 end;

 _vm_track_entry_add_obj(entry,obj);
end;

function _vm_track_entry_del_node(entry:p_vm_track_map_entry;node:p_vm_track_object_instance):Boolean;
begin
 TAILQ_REMOVE(@entry^.instances,node,@node^.entry_link);

 TAILQ_REMOVE(@node^.obj^.instances,node,@node^.obj_link);

 vm_track_object_deallocate(node^.obj);

 FreeMem(node);

 Result:=(TAILQ_FIRST(@entry^.instances)=nil);
end;

function vm_track_entry_del_obj(entry:p_vm_track_map_entry;obj:p_vm_track_object):Boolean;
var
 node:p_vm_track_object_instance;
begin
 node:=TAILQ_FIRST(@entry^.instances);

 while (node<>nil) do
 begin

  if (node^.obj=obj) then
  begin
   Result:=_vm_track_entry_del_node(entry,node);

   Exit;
  end;

  node:=TAILQ_NEXT(node,@node^.entry_link);
 end;

 Result:=False;
end;

procedure vm_track_entry_del_obj_all(entry:p_vm_track_map_entry);
var
 node,next:p_vm_track_object_instance;
begin
 node:=TAILQ_FIRST(@entry^.instances);

 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.entry_link);

  _vm_track_entry_del_node(entry,node);

  node:=next;
 end;
end;

procedure vm_track_entry_trigger(map:p_vm_track_map;entry:p_vm_track_map_entry;start,__end:vm_offset_t);
var
 node:p_vm_track_object_instance;
begin
 node:=TAILQ_FIRST(@entry^.instances);

 while (node<>nil) do
 begin

  vm_track_object_trigger(map,node^.obj,start,__end);

  node:=TAILQ_NEXT(node,@node^.entry_link);
 end;
end;

//

function in_obj_list(const b:TAILQ_HEAD;obj:p_vm_track_object):Boolean;
var
 node:p_vm_track_object_instance;
begin
 Result:=False;

 node:=TAILQ_FIRST(@b);

 while (node<>nil) do
 begin

  if (node^.obj=obj) then
  begin
   Exit(True);
  end;

  node:=TAILQ_NEXT(node,@node^.entry_link);
 end;
end;

function compare_obj_list(const a,b:TAILQ_HEAD):Boolean;
var
 node:p_vm_track_object_instance;
begin
 node:=TAILQ_FIRST(@a);

 while (node<>nil) do
 begin

  if not in_obj_list(b,node^.obj) then
  begin
   Exit(False);
  end;

  node:=TAILQ_NEXT(node,@node^.entry_link);
 end;

 Result:=True;
end;

procedure copy_obj_list(const src:TAILQ_HEAD;entry:p_vm_track_map_entry);
var
 node:p_vm_track_object_instance;
begin

 TAILQ_INIT(@entry^.instances);

 node:=TAILQ_FIRST(@src);

 while (node<>nil) do
 begin

  _vm_track_entry_add_obj(entry,node^.obj);

  node:=TAILQ_NEXT(node,@node^.entry_link);
 end;
end;

//

procedure vm_track_map_RANGE_CHECK(map:p_vm_track_map;var start,__end:vm_offset_t); inline;
begin
 if (start<map^.min_offset) then
 begin
  start:=map^.min_offset;
 end;
 if (__end>map^.max_offset) then
 begin
  __end:=map^.max_offset;
 end;
 if (start>__end) then
 begin
  start:=__end;
 end;
end;

procedure vm_track_map_lock(map:p_vm_track_map); inline;
begin
 mtx_lock(map^.lock);
 Inc(map^.timestamp);
end;

procedure vm_track_map_unlock(map:p_vm_track_map); inline;
begin
 mtx_unlock(map^.lock);
end;

function vm_map_locked(map:p_vm_track_map):Boolean; inline;
begin
 Result:=mtx_owned(map^.lock);
end;

procedure VM_MAP_ASSERT_LOCKED(map:p_vm_track_map); inline;
begin
 Assert(vm_map_locked(map));
end;

procedure vm_track_map_init(map:p_vm_track_map;min,max:vm_offset_t);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.header.adj_free:=(max-min);
 map^.header.max_free:=(max-min);
 map^.root:=nil;
 //
 mtx_init(map^.lock,'vm_track_map');
end;

procedure vm_track_entry_dispose(map:p_vm_track_map;entry:p_vm_track_map_entry); inline;
begin
 vm_track_entry_del_obj_all(entry);
 //
 FreeMem(entry);
end;

function vm_track_entry_create(map:p_vm_track_map):p_vm_track_map_entry;
var
 new_entry:p_vm_track_map_entry;
begin
 new_entry:=AllocMem(SizeOf(t_vm_track_map_entry));
 Assert((new_entry<>nil),'vm_track_map_entry_create: kernel resources exhausted');

 TAILQ_INIT(@new_entry^.instances);

 Result:=new_entry;
end;

procedure vm_track_entry_set_max_free(entry:p_vm_track_map_entry);
begin
 entry^.max_free:=entry^.adj_free;
 if (entry^.left<>nil) then
 if (entry^.left^.max_free>entry^.max_free) then
 begin
  entry^.max_free:=entry^.left^.max_free;
 end;
 if (entry^.right<>nil) then
 if (entry^.right^.max_free>entry^.max_free) then
 begin
  entry^.max_free:=entry^.right^.max_free;
 end;
end;

function vm_track_entry_splay(addr:vm_offset_t;root:p_vm_track_map_entry):p_vm_track_map_entry;
var
 llist,rlist:p_vm_track_map_entry;
 ltree,rtree:p_vm_track_map_entry;
 y          :p_vm_track_map_entry;
begin
 { Special case of empty tree. }
 if (root=nil) then Exit(root);

 llist:=nil;
 rlist:=nil;
 repeat
  { root is never nil in here. }
  if (addr<root^.start) then
  begin
   y:=root^.left;
   if (y=nil) then break;
   if (addr<y^.start) and (y^.left<>nil) then
   begin
    { Rotate right and put y on rlist. }
    root^.left:=y^.right;
    y^.right:=root;
    vm_track_entry_set_max_free(root);
    root:=y^.left;
    y^.left:=rlist;
    rlist:=y;
   end else
   begin
    { Put root on rlist. }
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
    { Rotate left and put y on llist. }
    root^.right:=y^.left;
    y^.left:=root;
    vm_track_entry_set_max_free(root);
    root:=y^.right;
    y^.right:=llist;
    llist:=y;
   end else
   begin
    { Put root on llist. }
    root^.right:=llist;
    llist:=root;
    root:=y;
   end;
  end else
  begin
   break;
  end;
 until false;

 {
  * Pass Two: Walk back up the two spines, flip the pointers
  * and set max_free.  The subtrees of the root go at the
  * bottom of llist and rlist.
  }
 ltree:=root^.left;
 while (llist<>nil) do
 begin
  y:=llist^.right;
  llist^.right:=ltree;
  vm_track_entry_set_max_free(llist);
  ltree:=llist;
  llist:=y;
 end;
 rtree:=root^.right;
 while (rlist<>nil) do
 begin
  y:=rlist^.left;
  rlist^.left:=rtree;
  vm_track_entry_set_max_free(rlist);
  rtree:=rlist;
  rlist:=y;
 end;

 {
  * Final assembly: add ltree and rtree as subtrees of root.
  }
 root^.left:=ltree;
 root^.right:=rtree;
 vm_track_entry_set_max_free(root);

 Result:=(root);
end;

procedure vm_track_map_entry_link(
           map        :p_vm_track_map;
           after_where:p_vm_track_map_entry;
           entry      :p_vm_track_map_entry);
begin
 VM_MAP_ASSERT_LOCKED(map);

 Inc(map^.nentries);
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   vm_track_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left:=after_where;
  after_where^.right:=nil;
  after_where^.adj_free:=entry^.start - after_where^.__end;
  vm_track_entry_set_max_free(after_where);
 end else
 begin
  entry^.right:=map^.root;
  entry^.left:=nil;
 end;
 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;
 vm_track_entry_set_max_free(entry);
 map^.root:=entry;
end;

procedure vm_track_map_entry_unlink(
           map  :p_vm_track_map;
           entry:p_vm_track_map_entry);
var
 next,prev,root:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (entry<>map^.root) then
 begin
  vm_track_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_track_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
  if (root^.next=@map^.header) then
  begin
   root^.adj_free:=map^.max_offset-root^.__end;
  end else
  begin
   root^.adj_free:=entry^.next^.start-root^.__end;
  end;
  vm_track_entry_set_max_free(root);
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

procedure vm_track_map_entry_resize_free(map:p_vm_track_map;entry:p_vm_track_map_entry);
begin
 if (entry<>map^.root) then
 begin
  map^.root:=vm_track_entry_splay(entry^.start, map^.root);
 end;

 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;

 vm_track_entry_set_max_free(entry);
end;

function vm_track_map_lookup_entry(
           map    :p_vm_track_map;
           address:vm_offset_t;
           entry  :pp_vm_track_map_entry):Boolean;
var
 cur:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

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

  cur:=vm_track_entry_splay(address,cur);
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

function vm_track_map_insert_internal(
           map  :p_vm_track_map;
           after:p_vm_track_map_entry;
           start:vm_offset_t;
           __end:vm_offset_t):p_vm_track_map_entry;
var
 new_entry:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (after<>@map^.header) and
    (start<after^.__end) then
 begin
  start:=after^.__end;
 end;

 if (after^.next<>@map^.header) and
    (after^.next^.start<__end) then
 begin
  __end:=after^.next^.start;
 end;

 if (start>=__end) then
 begin
  Exit(after);
 end;

 {
  * Create a new after
  }
 new_entry:=vm_track_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 {
  * Insert the new after into the list
  }
 vm_track_map_entry_link(map, after, new_entry);
 map^.size:=map^.size+(new_entry^.__end - new_entry^.start);

 //vm_track_entry_simplify_entry(map, new_entry);

 Result:=new_entry;
end;

procedure vm_track_map_simplify_entry(map:p_vm_track_map;entry:p_vm_track_map_entry);
var
 next,prev:p_vm_track_map_entry;
begin
 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  if (prev^.__end=entry^.start) and
     compare_obj_list(prev^.instances,entry^.instances) then
  begin
   vm_track_map_entry_unlink(map, prev);
   entry^.start:=prev^.start;

   //change
   if (entry^.prev<>@map^.header) then
   begin
    vm_track_map_entry_resize_free(map, entry^.prev);
   end;

   vm_track_entry_dispose(map, prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  if (entry^.__end=next^.start) and
     compare_obj_list(entry^.instances,next^.instances) then
  begin
   vm_track_map_entry_unlink(map, next);
   entry^.__end:=next^.__end;

   //change
   vm_track_map_entry_resize_free(map, entry);

   vm_track_entry_dispose(map, next);
  end;
 end;
end;

procedure _vm_track_map_clip_start(map:p_vm_track_map;entry:p_vm_track_map_entry;start:vm_offset_t);
var
 new_entry:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 vm_track_map_simplify_entry(map, entry);

 new_entry:=vm_track_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;

 copy_obj_list(entry^.instances,new_entry);

 entry^.start:=start;

 vm_track_map_entry_link(map, entry^.prev, new_entry);
end;

procedure vm_track_map_clip_start(map:p_vm_track_map;entry:p_vm_track_map_entry;start:vm_offset_t); inline;
begin
 if (start>entry^.start) then
 begin
  _vm_track_map_clip_start(map,entry,start);
 end;
end;

procedure _vm_track_map_clip_end(map:p_vm_track_map;entry:p_vm_track_map_entry;__end:vm_offset_t);
var
 new_entry:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 new_entry:=vm_track_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;

 entry^.__end:=__end;

 copy_obj_list(entry^.instances,new_entry);

 vm_track_map_entry_link(map, entry, new_entry);
end;

procedure vm_track_map_clip_end(map:p_vm_track_map;entry:p_vm_track_map_entry;__end:vm_offset_t); inline;
begin
 if (__end<entry^.__end) then
 begin
  _vm_track_map_clip_end(map,entry,__end);
 end;
end;

function vm_track_map_insert(map:p_vm_track_map;start,__end:vm_offset_t;obj:p_vm_track_object):Integer;
var
 current,entry:p_vm_track_map_entry;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (map=nil) or (obj=nil) then
 begin
  Exit(KERN_INVALID_ARGUMENT);
 end;

 vm_track_map_lock(map);

 vm_track_map_RANGE_CHECK(map, start, __end);

 if (vm_track_map_lookup_entry(map, start, @entry)) then
 begin
  vm_track_map_clip_start(map, entry, start);
 end else
 begin
  entry:=vm_track_map_insert_internal(map,entry,start,__end);
 end;

 current:=entry;
 while (current<>@map^.header) and (current^.start<__end) do
 begin
  vm_track_map_clip_end(map, current, __end);

  current:=vm_track_map_insert_internal(map,current,current^.__end,__end);

  vm_track_entry_add_obj(current,obj);

  current:=current^.next;
 end;

 vm_track_map_unlock(map);

 Result:=(KERN_SUCCESS);
end;

procedure vm_track_map_entry_delete(map:p_vm_track_map;entry:p_vm_track_map_entry);
var
 size:vm_ooffset_t;
begin
 vm_track_map_entry_unlink(map, entry);

 size:=entry^.__end - entry^.start;
 map^.size:=map^.size-size;

 vm_track_entry_dispose(map, entry);
end;

{
function vm_track_map_delete(map:p_vm_track_map;start,__end:vm_offset_t;obj:p_vm_track_object):Integer;
var
 entry      :p_vm_track_map_entry;
 first_entry:p_vm_track_map_entry;
 next       :p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (not vm_track_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  vm_track_map_clip_start(map, entry, start);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  vm_track_map_clip_end(map, entry, __end);

  next:=entry^.next;

  if (obj=nil) then
  begin
   //all
   vm_track_entry_delete(map, entry);
  end else
  if vm_track_map_entry_del_obj(entry,obj) then
  begin
   //zero
   vm_track_entry_delete(map, entry);
  end else
  begin
   //exclude one

   vm_track_entry_simplify_entry(map,entry);

   next:=entry^.next;
  end;

  entry:=next;
 end;
 Result:=(KERN_SUCCESS);
end;
}

procedure vm_track_map_delete_object(map:p_vm_track_map;obj:p_vm_track_object);
var
 node,next:p_vm_track_object_instance;
 entry:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 node:=TAILQ_FIRST(@obj^.instances);

 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.obj_link);

  entry:=node^.entry;

  if _vm_track_entry_del_node(entry,node) then
  begin
   //zero
   vm_track_map_entry_delete(map, entry);
  end else
  begin
   //exclude one
   vm_track_map_simplify_entry(map,entry);
  end;

  node:=next;
 end;
end;

function vm_track_map_remove_object(map:p_vm_track_map;obj:p_vm_track_object):Integer;
begin
 if (map=nil) or (obj=nil) then
 begin
  Exit(KERN_INVALID_ARGUMENT);
 end;

 vm_track_map_lock(map);
  vm_track_map_delete_object(map, obj);
 vm_track_map_unlock(map);

 Result:=KERN_SUCCESS;
end;

function vm_track_map_delete_main(map:p_vm_track_map;entry:p_vm_track_map_entry;start,__end:vm_offset_t):Boolean;
var
 node,next:p_vm_track_object_instance;
 obj:p_vm_track_object;
begin
 Result:=False;

 VM_MAP_ASSERT_LOCKED(map);

 node:=TAILQ_FIRST(@entry^.instances);

 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.entry_link);

  obj:=node^.obj;

  //cross with main
  if (obj^.main.__end>start) and (obj^.main.start<__end) then
  begin
   //delete full object
   vm_track_map_delete_object(map,obj);

   Result:=True;
  end;

  node:=next;
 end;
end;

function vm_track_map_delete_memory(map:p_vm_track_map;start,__end:vm_offset_t):Integer;
label
 _repeat;
var
 entry      :p_vm_track_map_entry;
 first_entry:p_vm_track_map_entry;
 next       :p_vm_track_map_entry;
 last       :vm_offset_t;
begin
 VM_MAP_ASSERT_LOCKED(map);

 _repeat:

 if (start>=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (not vm_track_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  vm_track_map_clip_start(map, entry, start);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  vm_track_map_clip_end(map, entry, __end);

  next:=entry^.next;

  last:=entry^.__end;

  if vm_track_map_delete_main(map,entry,start,__end) then
  begin
   //unknow intervals is deleted, repeat lookup
   start:=last;
   goto _repeat;
  end else
  begin
   //delete entry
   vm_track_map_entry_delete(map, entry);
  end;

  entry:=next;
 end;

 Result:=(KERN_SUCCESS);
end;

function vm_track_map_remove_memory(map:p_vm_track_map;start,__end:vm_offset_t):Integer;
begin
 if (map=nil) then
 begin
  Exit(KERN_INVALID_ARGUMENT);
 end;

 vm_track_map_lock(map);
 vm_track_map_RANGE_CHECK(map, start, __end);
  Result:=vm_track_map_delete_memory(map, start, __end);
 vm_track_map_unlock(map);
end;

function vm_track_map_trigger(map:p_vm_track_map;start,__end:vm_offset_t):Integer;
var
 current,entry:p_vm_track_map_entry;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (map=nil) then
 begin
  Exit(KERN_INVALID_ARGUMENT);
 end;

 vm_track_map_lock(map);

 vm_track_map_RANGE_CHECK(map, start, __end);

 if (vm_track_map_lookup_entry(map, start, @entry)) then
 begin
  //
 end else
 begin
  entry:=entry^.next;
 end;

 current:=entry;
 while (current<>@map^.header) and (current^.start<__end) do
 begin

  vm_track_entry_trigger(map,current,start,__end);

  current:=current^.next;
 end;

 vm_track_map_unlock(map);

 Result:=(KERN_SUCCESS);
end;


end.









