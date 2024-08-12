unit vm_tracking_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 mqueue,
 vm_pmap_prot,
 kern_mtx;

type
 p_vm_track_interval=^t_vm_track_interval;
 t_vm_track_interval=record
  start:vm_offset_t;
  __end:vm_offset_t;
 end;

const
 PAGE_TRACK_R=vm_pmap_prot.PAGE_TRACK_R;
 PAGE_TRACK_W=vm_pmap_prot.PAGE_TRACK_W;

 DO_NOTHING  =0;
 DO_DELETE   =1;
 DO_INCREMENT=2;

type
 T_THANDLE_TYPE=(H_ZERO,H_JIT_CHUNK,H_GPU_IMAGE,H_GPU_BUF);
 T_TRIGGER_MODE=(M_CPU_WRITE,M_DMEM_WRITE,M_GPU_PLANNED,M_GPU_APPLY);

type
 t_on_destroy=function(handle:Pointer):Integer;
 t_on_trigger=function(handle:Pointer;mode:T_TRIGGER_MODE):Integer;
 t_on_overlap=function(handle:Pointer;data:Pointer):Integer;

 p_vm_track_object=^t_vm_track_object;
 t_vm_track_object=packed record
  del_link  :TAILQ_ENTRY; //p_vm_track_map->delete_deferred
  //iter_link :TAILQ_ENTRY;
  //
  handle    :Pointer;
  //
  main      :t_vm_track_interval;
  align     :t_vm_track_interval;
  instances :TAILQ_HEAD; //p_vm_track_object_instance
  //
  on_destroy:t_on_destroy;
  on_trigger:t_on_trigger;
  on_overlap:t_on_overlap;
  //
  ref_count :DWORD;
  htype     :T_THANDLE_TYPE;
  mark_del  :Byte;
  prot      :Byte;
 end;

 pp_vm_track_map_entry=^p_vm_track_map_entry;
 p_vm_track_map_entry=^t_vm_track_map_entry;
 t_vm_track_map_entry=packed record
  prev     :p_vm_track_map_entry; // previous entry
  next     :p_vm_track_map_entry; // next entry
  left     :p_vm_track_map_entry; // left child in binary search tree
  right    :p_vm_track_map_entry; // right child in binary search tree
  start    :vm_offset_t;          // start address
  __end    :vm_offset_t;          // end address
  instances:Pointer;              // p_vm_track_object_instance
  instcount:QWORD;
  //
  track_r  :DWORD;
  track_w  :DWORD;
  //
  prot     :Byte;
  mark_cpu :Boolean;
 end;

 p_vm_track_object_instance=^t_vm_track_object_instance;
 t_vm_track_object_instance=record
  pLeft     :p_vm_track_object_instance; //p_vm_track_map_entry->instances
  pRight    :p_vm_track_object_instance; //p_vm_track_map_entry->instances
  obj_link  :TAILQ_ENTRY;                //p_vm_track_object   ->instances
  entry     :p_vm_track_map_entry;
  obj       :p_vm_track_object;
  source    :vm_offset_t;                // source of mirror
 end;

 p_vm_track_deferred=^t_vm_track_deferred;
 t_vm_track_deferred=record
  entry :TAILQ_ENTRY;
  start :vm_offset_t;
  __end :vm_offset_t;
  source:vm_offset_t;
  obj   :p_vm_track_object;
 end;

 p_vm_track_map=^t_vm_track_map;
 t_vm_track_map=object
  header   :t_vm_track_map_entry; // List of entries
  lock     :mtx;                  // Lock for map data
  root     :p_vm_track_map_entry; // Root of a binary search tree
  nentries :Integer;              // Number of entries

  insert_deferred:record
   free:TAILQ_HEAD;               //p_vm_track_deferred
   list:TAILQ_HEAD;               //p_vm_track_deferred
   stub:array[0..3] of t_vm_track_deferred;
  end;

  delete_deferred:TAILQ_HEAD;     //p_vm_track_object

  pmap     :Pointer;
  property  min_offset:vm_offset_t read header.start write header.start;
  property  max_offset:vm_offset_t read header.__end write header.__end;
 end;

procedure vm_track_map_init(map:p_vm_track_map;min,max:vm_offset_t);

//

function  vm_track_object_allocate  (handle:Pointer;start,__end:vm_offset_t;htype:T_THANDLE_TYPE;prot:Byte):p_vm_track_object;
procedure vm_track_object_deallocate(obj:p_vm_track_object);
procedure vm_track_object_reference (obj:p_vm_track_object);

//

procedure vm_track_map_lock  (map:p_vm_track_map);
procedure vm_track_map_unlock(map:p_vm_track_map;def:Boolean=True);

function  _vm_track_map_insert(map:p_vm_track_map;start,__end,source:vm_offset_t;obj:p_vm_track_object):Integer;

procedure _vm_track_map_insert_deferred(map:p_vm_track_map;start,__end,source:vm_offset_t;obj:p_vm_track_object);
procedure _vm_track_map_delete_deferred(map:p_vm_track_map;obj:p_vm_track_object);

function  _vm_track_map_insert_mirror(map:p_vm_track_map;start,__end,dst:vm_offset_t):Integer;

//

function  vm_track_map_remove_object (map:p_vm_track_map;obj:p_vm_track_object):Integer;
function  vm_track_map_remove_memory (map:p_vm_track_map;start,__end:vm_offset_t):Integer;
function  vm_track_map_trigger       (map:p_vm_track_map;start,__end:vm_offset_t;exclude:Pointer;mode:T_TRIGGER_MODE):Integer;

function  vm_track_map_next_object   (map:p_vm_track_map;start:vm_offset_t;obj:p_vm_track_object;htype:T_THANDLE_TYPE):p_vm_track_object;

procedure vm_track_map_restore_object(map:p_vm_track_map;obj:p_vm_track_object);

procedure vm_track_map_set_prot      (map:p_vm_track_map;obj:p_vm_track_object;prot:Byte);

function  vm_track_map_overlap       (map:p_vm_track_map;obj:p_vm_track_object;data:Pointer):Integer;

implementation

procedure pmap_prot_track(pmap :Pointer;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prots:Byte); external;

function vm_track_object_allocate(handle:Pointer;start,__end:vm_offset_t;htype:T_THANDLE_TYPE;prot:Byte):p_vm_track_object;
begin
 Result:=AllocMem(SizeOf(t_vm_track_object));

 Result^.handle   :=handle;
 Result^.ref_count:=1;

 TAILQ_INIT(@Result^.instances);

 Result^.main.start:=start;
 Result^.main.__end:=__end;

 Result^.align.start:=start              and (not PMAPP_MASK);
 Result^.align.__end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 Result^.prot :=prot;
 Result^.htype:=htype;
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
   if ((obj^.on_destroy(obj^.handle) and DO_DELETE)=0) then
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

{
procedure vm_track_list_add_obj(var list:TAILQ_HEAD;obj:p_vm_track_object);
begin
 if (obj=nil) then Exit;

 if (obj^.mark_del<>0) then Exit;

 if (obj^.iter_link.tqe_prev<>nil) then Exit;

 TAILQ_INSERT_TAIL(@list,obj,@obj^.iter_link);
end;
}

function vm_track_object_trigger(obj:p_vm_track_object;start,__end:vm_offset_t;mode:T_TRIGGER_MODE):Integer;
begin
 Result:=DO_NOTHING;

 if (obj=nil) then Exit;

 if (obj^.mark_del<>0) then Exit;

 //cross with main
 if (obj^.main.__end>start) and (obj^.main.start<__end) then
 begin
  if (obj^.on_trigger<>nil) then
  begin
   Result:=obj^.on_trigger(obj^.handle,mode);
  end else
  begin
   Result:=DO_INCREMENT;
  end;
 end;

end;

//

procedure _vm_track_splay_instance(var root:p_vm_track_object_instance;obj:p_vm_track_object);
label
 _left,
 _right;
var
 llist,rlist:p_vm_track_object_instance;
 ltree,rtree:p_vm_track_object_instance;
 y          :p_vm_track_object_instance;
begin
 if (root=nil) or (obj=nil) then Exit;

 llist:=nil;
 rlist:=nil;
 repeat

  if (obj<root^.obj) then
  begin
   y:=root^.pLeft;
   if (y=nil) then break;
   if (y^.pLeft=nil) then
   begin
    _left:
    root^.pLeft:=rlist;
    rlist:=root;
    root:=y;
   end else
   if (obj<y^.obj) then
   begin
    root^.pLeft:=y^.pRight;
    y^.pRight:=root;
    root:=y^.pLeft;
    y^.pLeft:=rlist;
    rlist:=y;
   end else
   begin
    goto _left;
   end;
  end else
  if (obj>root^.obj) then
  begin
   y:=root^.pRight;
   if (y=nil) then break;
   if (y^.pRight=nil) then
   begin
    _right:
    root^.pRight:=llist;
    llist:=root;
    root:=y;
   end else
   if (obj>y^.obj) then
   begin
    root^.pRight:=y^.pLeft;
    y^.pLeft:=root;
    root:=y^.pRight;
    y^.pRight:=llist;
    llist:=y;
   end else
   begin
    goto _right;
   end;
  end else
  begin
   Break;
  end;
 until false;

 ltree:=root^.pLeft;
 while (llist<>nil) do
 begin
  y:=llist^.pRight;
  llist^.pRight:=ltree;
  ltree:=llist;
  llist:=y;
 end;

 rtree:=root^.pRight;
 while (rlist<>nil) do
 begin
  y:=rlist^.pLeft;
  rlist^.pLeft:=rtree;
  rtree:=rlist;
  rlist:=y;
 end;

 root^.pLeft :=ltree;
 root^.pRight:=rtree;
end;

procedure vm_track_insert_instance(var root:p_vm_track_object_instance;node:p_vm_track_object_instance);
begin
 _vm_track_splay_instance(root,node^.obj);

 if (root=nil) then
 begin
  //
 end else
 if (node^.obj>root^.obj) then
 begin
  node^.pRight:=root^.pRight;
  node^.pLeft :=root;
  root^.pRight:=nil;
 end else
 begin
  node^.pLeft :=root^.pLeft;
  node^.pRight:=root;
  root^.pLeft :=nil;
 end;

 root:=node;
end;

procedure vm_track_delete_instance(var root:p_vm_track_object_instance;node:p_vm_track_object_instance);
var
 pLeft :p_vm_track_object_instance;
 pRight:p_vm_track_object_instance;
 pMax  :p_vm_track_object_instance;
begin
 _vm_track_splay_instance(root,node^.obj);

 if (root=node) then
 begin
  pLeft :=root^.pLeft;
  pRight:=root^.pRight;

  if (pLeft<>nil) then
  begin
   pMax:=pLeft;
   while (pMax^.pRight<>nil) do
   begin
    pMax:=pMax^.pRight;
   end;

   root:=pLeft;

   _vm_track_splay_instance(root,pMax^.obj);

   root^.pRight:=pRight;
  end else
  begin
   root:=pRight;
  end;
 end;

end;

function vm_track_first_instance(root:p_vm_track_object_instance):p_vm_track_object_instance;
var
 node:p_vm_track_object_instance;
begin
 Result:=nil;
 node:=root;
 While (node<>nil) do
 begin
  Result:=node;
  node:=node^.pLeft;
 end;
end;

function vm_track_next_instance(root,node:p_vm_track_object_instance):p_vm_track_object_instance;
var
 y,r:p_vm_track_object_instance;
begin
 Result:=nil;

 if (root=nil) or (node=nil) then Exit;

 r:=root;
 y:=nil;

 if (node^.pRight<>nil) then
 begin
  y:=node^.pRight;
  while (y^.pLeft<>nil) do y:=y^.pLeft;
  Exit(y);
 end;

 while (r<>nil) do
 begin
  if (node^.obj=r^.obj) then
  begin
   Break;
  end else
  if (node^.obj<r^.obj) then
  begin
   y:=r;
   r:=r^.pLeft;
  end else
  begin
   r:=r^.pRight;
  end;
 end;

 Exit(y);
end;

procedure _vm_track_entry_change_prot(pmap:Pointer;entry:p_vm_track_map_entry;add_prot,del_prot:Byte);
var
 prot:Byte;
begin
 //update prot

 if (add_prot and PAGE_TRACK_R)<>0 then
 begin
  Inc(entry^.track_r);
 end;

 if (add_prot and PAGE_TRACK_W)<>0 then
 begin
  Inc(entry^.track_w);
 end;

 if (del_prot and PAGE_TRACK_R)<>0 then
 begin
  Dec(entry^.track_r);
 end;

 if (del_prot and PAGE_TRACK_W)<>0 then
 begin
  Dec(entry^.track_w);
 end;

 prot:=(ord(entry^.track_r<>0)*PAGE_TRACK_R) or
       (ord(entry^.track_w<>0)*PAGE_TRACK_W);

 if entry^.mark_cpu or
    (prot<>entry^.prot) then
 begin
  entry^.prot:=prot;

  if (pmap<>nil) then
  begin
   entry^.mark_cpu:=False;

   pmap_prot_track(pmap,entry^.start,entry^.__end,prot);
  end;
 end;

end;

procedure _vm_track_entry_add_obj(pmap:Pointer;entry:p_vm_track_map_entry;obj:p_vm_track_object;source:vm_offset_t);
var
 node:p_vm_track_object_instance;
begin
 node:=AllocMem(SizeOf(t_vm_track_object_instance));

 node^.entry :=entry;
 node^.obj   :=obj;
 node^.source:=source;

 vm_track_insert_instance(entry^.instances,node);
 Inc(entry^.instcount);

 TAILQ_INSERT_TAIL(@obj^.instances,node,@node^.obj_link);

 vm_track_object_reference(obj);

 //update prot
 if (pmap=nil) then //if not copy_obj_list
 begin
  _vm_track_entry_change_prot(pmap,entry,obj^.prot,0);
 end;
 //
end;

procedure vm_track_entry_add_obj(pmap:Pointer;entry:p_vm_track_map_entry;obj:p_vm_track_object;source:vm_offset_t);
var
 root:p_vm_track_object_instance;
begin
 _vm_track_splay_instance(entry^.instances,obj);
 root:=entry^.instances;

 if (root<>nil) then
 if (root^.obj=obj) then
 begin
  Exit;
 end;

 _vm_track_entry_add_obj(pmap,entry,obj,source);
end;

function _vm_track_entry_del_node(pmap:Pointer;entry:p_vm_track_map_entry;node:p_vm_track_object_instance):Boolean;
var
 obj:p_vm_track_object;
begin
 obj:=node^.obj;

 //update prot
 if (pmap<>nil) then //if not vm_track_map_simplify_entry -> vm_track_entry_dispose -> vm_track_entry_del_obj_all
 begin
  _vm_track_entry_change_prot(pmap,entry,0,obj^.prot);
 end;
 //

 Dec(entry^.instcount);
 vm_track_delete_instance(entry^.instances,node);

 TAILQ_REMOVE(@obj^.instances,node,@node^.obj_link);

 vm_track_object_deallocate(obj);

 FreeMem(node);

 Result:=(entry^.instances=nil);
end;

function vm_track_entry_del_obj(pmap:Pointer;entry:p_vm_track_map_entry;obj:p_vm_track_object):Boolean;
var
 root:p_vm_track_object_instance;
begin
 Result:=False;

 _vm_track_splay_instance(entry^.instances,obj);
 root:=entry^.instances;

 if (root=nil) then
 begin
  Exit;
 end;

 if (root^.obj<>obj) then
 begin
  Exit;
 end;

 Result:=_vm_track_entry_del_node(pmap,entry,root);
end;

procedure vm_track_entry_del_obj_all(pmap:Pointer;entry:p_vm_track_map_entry);
var
 node,next:p_vm_track_object_instance;
begin
 node:=vm_track_first_instance(entry^.instances);

 while (node<>nil) do
 begin
  next:=vm_track_next_instance(entry^.instances,node);

  _vm_track_entry_del_node(pmap,entry,node);

  node:=next;
 end;
end;

//

function in_obj_list(b:p_vm_track_map_entry;obj:p_vm_track_object;source:vm_offset_t):Boolean;
var
 root:p_vm_track_object_instance;
begin
 Result:=False;

 _vm_track_splay_instance(b^.instances,obj);
 root:=b^.instances;

 if (root=nil) then
 begin
  Exit;
 end;

 Result:=(root^.obj=obj) and (root^.source=source);
end;

function compare_obj_list(a:p_vm_track_map_entry;offset:vm_offset_t;b:p_vm_track_map_entry):Boolean;
var
 node:p_vm_track_object_instance;
begin
 if (a^.instcount<>b^.instcount) then
 begin
  Exit(False);
 end;

 node:=vm_track_first_instance(a^.instances);

 while (node<>nil) do
 begin

  if not in_obj_list(b,node^.obj,node^.source+offset) then
  begin
   Exit(False);
  end;

  node:=vm_track_next_instance(a^.instances,node);
 end;

 Result:=True;
end;

procedure inc_obj_list(src:p_vm_track_map_entry;offset:vm_offset_t);
var
 node:p_vm_track_object_instance;
begin
 node:=vm_track_first_instance(src^.instances);

 while (node<>nil) do
 begin
  node^.source:=node^.source+offset;

  node:=vm_track_next_instance(src^.instances,node);
 end;
end;

procedure dec_obj_list(src:p_vm_track_map_entry;offset:vm_offset_t);
var
 node:p_vm_track_object_instance;
begin
 node:=vm_track_first_instance(src^.instances);

 while (node<>nil) do
 begin
  node^.source:=node^.source-offset;

  node:=vm_track_next_instance(src^.instances,node);
 end;
end;

procedure copy_obj_list(src,dst:p_vm_track_map_entry;offset:vm_offset_t);
var
 node:p_vm_track_object_instance;
begin
 dst^.instances:=nil; //init
 dst^.instcount:=0;

 node:=vm_track_first_instance(src^.instances);

 while (node<>nil) do
 begin
  _vm_track_entry_add_obj(nil,dst,node^.obj,node^.source+offset);

  node:=vm_track_next_instance(src^.instances,node);
 end;
end;

//

procedure vm_track_map_RANGE_CHECK(map:p_vm_track_map;var start,__end:vm_offset_t); inline;
begin
 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

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
end;

procedure _vm_track_map_process_deferred(map:p_vm_track_map); forward;

procedure vm_track_map_unlock(map:p_vm_track_map;def:Boolean=True);
begin
 if def then
 begin
  _vm_track_map_process_deferred(map);
 end;
 mtx_unlock(map^.lock);
end;

function vm_track_locked(map:p_vm_track_map):Boolean; inline;
begin
 Result:=mtx_owned(map^.lock);
end;

procedure VM_MAP_ASSERT_LOCKED(map:p_vm_track_map); inline;
begin
 Assert(vm_track_locked(map));
end;

procedure vm_track_map_init(map:p_vm_track_map;min,max:vm_offset_t);
var
 i:Integer;
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.root:=nil;

 //
 TAILQ_INIT(@map^.insert_deferred.free);
 For i:=Low(map^.insert_deferred.stub) to High(map^.insert_deferred.stub) do
 begin
  TAILQ_INSERT_TAIL(@map^.insert_deferred.free,
                    @map^.insert_deferred.stub[i],
                    @map^.insert_deferred.stub[i].entry);
 end;

 TAILQ_INIT(@map^.insert_deferred.list);

 TAILQ_INIT(@map^.delete_deferred);

 //
 mtx_init(map^.lock,'vm_track_map');
end;

procedure vm_track_entry_dispose(map:p_vm_track_map;pmap:Pointer;entry:p_vm_track_map_entry); inline;
begin
 vm_track_entry_del_obj_all(pmap,entry);
 //
 FreeMem(entry);
end;

function vm_track_entry_create(map:p_vm_track_map):p_vm_track_map_entry;
var
 new_entry:p_vm_track_map_entry;
begin
 new_entry:=AllocMem(SizeOf(t_vm_track_map_entry));
 Assert((new_entry<>nil),'vm_track_map_entry_create: kernel resources exhausted');

 new_entry^.instances:=nil;

 Result:=new_entry;
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

 {
  * Final assembly: add ltree and rtree as subtrees of root.
  }
 root^.left:=ltree;
 root^.right:=rtree;

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
 end else
 begin
  entry^.right:=map^.root;
  entry^.left:=nil;
 end;

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
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
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
           map   :p_vm_track_map;
           after :p_vm_track_map_entry;
           start :vm_offset_t;
           __end :vm_offset_t):p_vm_track_map_entry;
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
 new_entry^.start :=start;
 new_entry^.__end :=__end;

 {
  * Insert the new after into the list
  }
 vm_track_map_entry_link(map, after, new_entry);

 //vm_track_entry_simplify_entry(map, new_entry);

 Result:=new_entry;
end;

procedure vm_track_map_simplify_entry(map:p_vm_track_map;entry:p_vm_track_map_entry);
var
 next,prev:p_vm_track_map_entry;
 prevsize,esize:vm_size_t;
begin
 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  prevsize:=prev^.__end - prev^.start;
  if (prev^.__end=entry^.start) and
     compare_obj_list(prev,prevsize,entry) then
  begin
   vm_track_map_entry_unlink(map, prev);
   entry^.start :=prev^.start;

   dec_obj_list(entry,prevsize);

   vm_track_entry_dispose(map, nil, prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  esize:=entry^.__end - entry^.start;
  if (entry^.__end=next^.start) and
     compare_obj_list(entry,esize,next) then
  begin
   vm_track_map_entry_unlink(map, next);
   entry^.__end:=next^.__end;

   vm_track_entry_dispose(map, nil, next);
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

 copy_obj_list(entry,new_entry,0);

 inc_obj_list(entry,(start - entry^.start));

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

 copy_obj_list(entry,new_entry,(__end - entry^.start));

 vm_track_map_entry_link(map, entry, new_entry);
end;

procedure vm_track_map_clip_end(map:p_vm_track_map;entry:p_vm_track_map_entry;__end:vm_offset_t); inline;
begin
 if (__end<entry^.__end) then
 begin
  _vm_track_map_clip_end(map,entry,__end);
 end;
end;

function _vm_track_map_insert(map:p_vm_track_map;start,__end,source:vm_offset_t;obj:p_vm_track_object):Integer;
var
 entry:p_vm_track_map_entry;
begin
 if (start>=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 VM_MAP_ASSERT_LOCKED(map);

 if (obj^.mark_del<>0) then Exit(KERN_SUCCESS);

 vm_track_map_RANGE_CHECK(map, start, __end);
 source:=source and (not PMAPP_MASK);

 if (vm_track_map_lookup_entry(map, start, @entry)) then
 begin
  vm_track_map_clip_start(map, entry, start);
 end else
 begin
  entry:=vm_track_map_insert_internal(map,entry,start,__end);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  vm_track_map_clip_end(map, entry, __end);

  entry:=vm_track_map_insert_internal(map,entry,
                                        entry^.__end, //start
                                        __end         //__end
                                       );

  vm_track_entry_add_obj(map^.pmap,entry,obj,source+(entry^.start-start));

  entry:=entry^.next;
 end;

 Result:=(KERN_SUCCESS);
end;

procedure vm_track_map_entry_delete(map:p_vm_track_map;entry:p_vm_track_map_entry);
begin
 vm_track_map_entry_unlink(map, entry);

 vm_track_entry_dispose(map, map^.pmap, entry);
end;

procedure vm_track_map_delete_object(map:p_vm_track_map;obj:p_vm_track_object);
var
 node,next:p_vm_track_object_instance;
 entry:p_vm_track_map_entry;
begin
 VM_MAP_ASSERT_LOCKED(map);

 obj^.mark_del:=1;

 node:=TAILQ_FIRST(@obj^.instances);

 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.obj_link);

  entry:=node^.entry;

  if _vm_track_entry_del_node(map^.pmap,entry,node) then
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

 obj^.mark_del:=1;

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

 node:=vm_track_first_instance(entry^.instances);

 while (node<>nil) do
 begin
  next:=vm_track_next_instance(entry^.instances,node);

  obj:=node^.obj;

  //cross with main
  if (obj^.align.__end>start) and (obj^.align.start<__end) then
  begin
   //delete full object
   _vm_track_map_delete_deferred(map,obj);

   Result:=True;
  end;

  node:=next;
 end;
end;

function vm_track_map_delete_memory(map:p_vm_track_map;start,__end:vm_offset_t):Integer;
var
 entry      :p_vm_track_map_entry;
 first_entry:p_vm_track_map_entry;
 next       :p_vm_track_map_entry;
begin
 vm_track_map_RANGE_CHECK(map,start,__end);

 VM_MAP_ASSERT_LOCKED(map);

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

  if vm_track_map_delete_main(map,entry,start,__end) then
  begin
   //delete intervals is deferred
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
  Result:=vm_track_map_delete_memory(map, start, __end);
 vm_track_map_unlock(map);
end;

function _new_deferred(map:p_vm_track_map):p_vm_track_deferred;
begin
 Result:=TAILQ_FIRST(@map^.insert_deferred.free);

 if (Result<>nil) then
 begin
  TAILQ_REMOVE(@map^.insert_deferred.free,Result,@Result^.entry);
 end else
 begin
  Result:=GetMem(SizeOf(t_vm_track_deferred));
 end;
end;

procedure _free_deferred(map:p_vm_track_map;node:p_vm_track_deferred);
begin

 if (Ptruint(node)>=Ptruint(@map^.insert_deferred.stub)) and
    (Ptruint(node)< (Ptruint(@map^.insert_deferred.stub)+SizeOf(map^.insert_deferred.stub))) then
 begin
  TAILQ_INSERT_TAIL(@map^.insert_deferred.free,node,@node^.entry);
 end else
 begin
  FreeMem(node);
 end;

end;

procedure _vm_track_map_insert_deferred(map:p_vm_track_map;start,__end,source:vm_offset_t;obj:p_vm_track_object);
var
 new:p_vm_track_deferred;
begin
 vm_track_map_RANGE_CHECK(map,start,__end);

 VM_MAP_ASSERT_LOCKED(map);

 if (obj^.mark_del<>0) then Exit;

 new:=_new_deferred(map);
 new^:=Default(t_vm_track_deferred);

 new^.start :=start;
 new^.__end :=__end;
 new^.source:=source;
 new^.obj   :=obj;

 TAILQ_INSERT_TAIL(@map^.insert_deferred.list,new,@new^.entry);
end;

procedure _vm_track_map_delete_deferred(map:p_vm_track_map;obj:p_vm_track_object);
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (obj=nil) then Exit;

 obj^.mark_del:=1;

 if (obj^.del_link.tqe_prev<>nil) then Exit;

 TAILQ_INSERT_TAIL(@map^.delete_deferred,obj,@obj^.del_link);
end;

procedure _vm_track_map_process_deferred(map:p_vm_track_map);
var
 inode,inext:p_vm_track_deferred;

 dnode,dnext:p_vm_track_object;
begin

 inode:=TAILQ_FIRST(@map^.insert_deferred.list);

 while (inode<>nil) do
 begin
  inext:=TAILQ_NEXT(inode,@inode^.entry);

  TAILQ_REMOVE(@map^.insert_deferred.list,inode,@inode^.entry);

  _vm_track_map_insert(map,inode^.start,inode^.__end,inode^.source,inode^.obj);

  _free_deferred(map,inode);

  inode:=inext;
 end;

 //

 dnode:=TAILQ_FIRST(@map^.delete_deferred);

 while (dnode<>nil) do
 begin
  dnext:=TAILQ_NEXT(dnode,@dnode^.del_link);

  TAILQ_REMOVE(@map^.delete_deferred,dnode,@dnode^.del_link);

  dnode^.del_link:=Default(TAILQ_ENTRY);

  vm_track_map_delete_object(map,dnode);

  dnode:=dnext;
 end;
end;

function _vm_track_map_insert_mirror(map:p_vm_track_map;start,__end,dst:vm_offset_t):Integer;
var
 e_start:vm_offset_t;
 e___end:vm_offset_t;
 d_start:vm_offset_t;
 d___end:vm_offset_t;
 entry:p_vm_track_map_entry;
 node,next:p_vm_track_object_instance;
 obj:p_vm_track_object;
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (start>=__end) then
 begin
  Exit;
 end;

 if (vm_track_map_lookup_entry(map, start, @entry)) then
 begin
  //
 end else
 begin
  entry:=entry^.next;
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  e_start:=entry^.start;
  e___end:=entry^.__end;

  if (e_start<start) then
  begin
   e_start:=start;
  end;

  if (e___end>__end) then
  begin
   e___end:=__end;
  end;

  if (e___end>e_start) then
  begin
   d_start:=dst    +(e_start-start);
   d___end:=d_start+(e___end-e_start);

   node:=vm_track_first_instance(entry^.instances);

   while (node<>nil) do
   begin
    next:=vm_track_next_instance(entry^.instances,node);

    obj:=node^.obj;

    //Don't try to add mirroring for mirroring
    if (obj^.align.__end>e_start) and (obj^.align.start<e___end) then
    begin
     _vm_track_map_insert_deferred(map,d_start,d___end,e_start,obj);
    end;

    node:=next;
   end;

  end;

  entry:=entry^.next;
 end;

 Result:=(KERN_SUCCESS);
end;

function vm_track_map_trigger(map:p_vm_track_map;start,__end:vm_offset_t;exclude:Pointer;mode:T_TRIGGER_MODE):Integer;
var
 entry:p_vm_track_map_entry;
 node:p_vm_track_object_instance;

 diff:vm_offset_t;
 size:vm_offset_t;

 s_start:vm_offset_t;
 s___end:vm_offset_t;

 ret:Integer;

 //list:TAILQ_HEAD;
 //onode,onext:p_vm_track_object;
begin
 Result:=0; //count

 if (start>=__end) or (map=nil) then
 begin
  Exit;
 end;

 size:=(__end-start);

 //TAILQ_INIT(@list);

 vm_track_map_lock(map);

 //vm_track_map_RANGE_CHECK(map, start, __end);

 if (vm_track_map_lookup_entry(map, start, @entry)) then
 begin
  //
 end else
 begin
  entry:=entry^.next;
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  node:=vm_track_first_instance(entry^.instances);

  while (node<>nil) do
  begin
   //vm_track_list_add_obj(list,node^.obj); //deferred

   //remap with source
   if (node^.obj<>exclude) then
   begin

    //remap with source
    diff:=entry^.start-start;
    //
    s_start:=node^.source-diff;
    s___end:=s_start+size;

    ret:=vm_track_object_trigger(node^.obj,s_start,s___end,mode);

    if ((ret and DO_DELETE)<>0) then
    begin
     //delete full object
     _vm_track_map_delete_deferred(map,node^.obj);
    end else
    if (mode=M_CPU_WRITE) then
    begin
     entry^.mark_cpu:=True;
    end;

    if ((ret and DO_INCREMENT)<>0) then
    begin
     Inc(Result);
    end;
   end;

   node:=vm_track_next_instance(entry^.instances,node);
  end;

  entry:=entry^.next;
 end;

 //iterate

{
 onode:=TAILQ_FIRST(@list);

 while (onode<>nil) do
 begin
  onext:=TAILQ_NEXT(onode,@onode^.iter_link);

  TAILQ_REMOVE(@list,onode,@onode^.iter_link);

  onode^.iter_link:=Default(TAILQ_ENTRY);

  ret:=vm_track_object_trigger(map,onode,start,__end);

  if ((ret and DO_DELETE)<>0) then
  begin
   //delete full object
   _vm_track_map_delete_deferred(map,onode);
  end;

  if ((ret and DO_INCREMENT)<>0) then
  begin
   Inc(Result);
  end;

  onode:=onext;
 end;
}

 //iterate

 vm_track_map_unlock(map);
end;

function vm_track_map_next_object(map:p_vm_track_map;start:vm_offset_t;obj:p_vm_track_object;htype:T_THANDLE_TYPE):p_vm_track_object;
var
 entry:p_vm_track_map_entry;
 node:p_vm_track_object_instance;
begin
 Result:=nil;

 if (map=nil) then Exit;

 vm_track_map_lock(map);

 vm_track_map_lookup_entry(map, start, @entry);

 if (entry<>@map^.header) then
 begin
  _vm_track_splay_instance(entry^.instances,obj);
  node:=entry^.instances;

  //find greater than
  if (node<>nil) then
  if (node^.obj<=obj) then
  begin
   node:=vm_track_next_instance(entry^.instances,node);
  end;

  while (node<>nil) do
  begin

   if (node^.obj^.htype=htype) then
   begin
    Result:=node^.obj;
    Break;
   end;

   node:=vm_track_next_instance(entry^.instances,node);
  end;

 end;

 //inc ref
 if (Result<>nil) then
 begin
  vm_track_object_reference(Result);
 end;

 vm_track_map_unlock(map);
end;

procedure vm_track_map_restore_object(map:p_vm_track_map;obj:p_vm_track_object);
var
 node:p_vm_track_object_instance;
 entry:p_vm_track_map_entry;
begin
 if (map=nil) or (obj=nil) then Exit;

 vm_track_map_lock(map);

 node:=TAILQ_FIRST(@obj^.instances);

 while (node<>nil) do
 begin
  entry:=node^.entry;

  if entry^.mark_cpu then
  begin
   entry^.mark_cpu:=False;

   pmap_prot_track(map^.pmap,entry^.start,entry^.__end,entry^.prot);
  end;

  node:=TAILQ_NEXT(node,@node^.obj_link);
 end;

 vm_track_map_unlock(map);
end;

procedure vm_track_map_set_prot(map:p_vm_track_map;obj:p_vm_track_object;prot:Byte);
var
 node:p_vm_track_object_instance;
 entry:p_vm_track_map_entry;
begin
 if (map=nil) or (obj=nil) then Exit;

 if (obj^.prot=prot) then Exit;

 vm_track_map_lock(map);

 node:=TAILQ_FIRST(@obj^.instances);

 while (node<>nil) do
 begin
  entry:=node^.entry;

  if entry^.mark_cpu then
  begin
   //dont change mark_cpu
   _vm_track_entry_change_prot(nil,entry,prot,obj^.prot);
  end else
  begin
   _vm_track_entry_change_prot(map^.pmap,entry,prot,obj^.prot);
  end;

  node:=TAILQ_NEXT(node,@node^.obj_link);
 end;

 obj^.prot:=prot;

 vm_track_map_unlock(map);
end;

function _vm_track_entry_overlap(entry:p_vm_track_map_entry;exclude,data:Pointer):Integer;
var
 node:p_vm_track_object_instance;
 obj :p_vm_track_object;
begin
 Result:=0;

 node:=vm_track_first_instance(entry^.instances);

 while (node<>nil) do
 begin
  obj:=node^.obj;

  if (obj<>exclude) then
  if (obj^.on_overlap<>nil) then
  begin
   Result:=Result+obj^.on_overlap(obj^.handle,data);
  end;

  node:=vm_track_next_instance(entry^.instances,node);
 end;
end;

function vm_track_map_overlap(map:p_vm_track_map;obj:p_vm_track_object;data:Pointer):Integer;
var
 node:p_vm_track_object_instance;
 entry:p_vm_track_map_entry;
begin
 Result:=0;
 if (map=nil) or (obj=nil) then Exit;

 vm_track_map_lock(map);

 node:=TAILQ_FIRST(@obj^.instances);

 while (node<>nil) do
 begin
  Result:=Result+_vm_track_entry_overlap(node^.entry,obj,data);

  node:=TAILQ_NEXT(node,@node^.obj_link);
 end;

 vm_track_map_unlock(map);
end;

end.









