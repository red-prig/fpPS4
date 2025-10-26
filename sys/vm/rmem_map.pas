unit rmem_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vm,
 kern_mtx;

type
 p_rmem_vaddr_instance=^t_rmem_vaddr_instance;
 t_rmem_vaddr_instance=record
  entry:TAILQ_ENTRY; //p_rmem_map_entry->vaddr
  vaddr:QWORD;
 end;

 pp_rmem_map_entry=^p_rmem_map_entry;
 p_rmem_map_entry=^t_rmem_map_entry;
 t_rmem_map_entry=packed record
  prev :p_rmem_map_entry; // previous entry
  next :p_rmem_map_entry; // next entry
  left :p_rmem_map_entry; // left child in binary search tree
  right:p_rmem_map_entry; // right child in binary search tree
  start:QWORD;            // start address
  __end:QWORD;            // end address
  vlist:TAILQ_HEAD;       // virtual addr mapping
  count:QWORD;
 end;

 p_rmem_map=^t_rmem_map;
 t_rmem_map=object
  header  :t_rmem_map_entry; // List of entries
  lock    :mtx;              // Lock for map data
  root    :p_rmem_map_entry; // Root of a binary search tree
  nentries:QWORD;            // Number of entries
  tmap    :Pointer;          // p_vm_track_map
  property min_offset:QWORD read header.start write header.start;
  property max_offset:QWORD read header.__end write header.__end;
 end;

procedure rmem_map_process_deferred;

procedure rmem_map_lock  (map:p_rmem_map);
procedure rmem_map_unlock(map:p_rmem_map;def:Boolean=True);
function  rmem_map_locked(map:p_rmem_map):Boolean; inline;

procedure rmem_map_init(map:p_rmem_map;min,max:QWORD);

function  rmem_map_lookup_entry(
            map    :p_rmem_map;
            address:QWORD;
            entry  :pp_rmem_map_entry):Boolean;

type
 t_rmem_test_mode=(rt_intersection,rt_continuity);

function  rmem_map_test(map:p_rmem_map;
                        start,__end:QWORD;
                        mode:t_rmem_test_mode):Boolean;

function  rmem_map_insert(map:p_rmem_map;
                          vaddr:QWORD;
                          start,__end:QWORD):Integer;

function  rmem_map_delete(map:p_rmem_map;
                          vaddr:QWORD;
                          start,__end:QWORD):Integer;

procedure rmem_map_track(map:p_rmem_map;
                         start,__end,source:QWORD;
                         tobj:Pointer);

implementation

uses
 kern_thr,
 vm_tracking_map;

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

//

procedure _rmem_entry_add_vaddr(entry:p_rmem_map_entry;vaddr:QWORD);
var
 node:p_rmem_vaddr_instance;
begin
 node:=AllocMem(SizeOf(t_rmem_vaddr_instance));

 node^.vaddr:=vaddr;

 TAILQ_INSERT_TAIL(@entry^.vlist,node,@node^.entry);
 Inc(entry^.count);
end;

function rmem_entry_add_vaddr(entry:p_rmem_map_entry;vaddr:QWORD):Boolean;
var
 node:p_rmem_vaddr_instance;
begin
 node:=TAILQ_FIRST(@entry^.vlist);

 while (node<>nil) do
 begin

  if (node^.vaddr=vaddr) then
  begin
   Exit(False);
  end;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;

 //if not one vaddr
 Result:=(TAILQ_FIRST(@entry^.vlist)<>nil);

 _rmem_entry_add_vaddr(entry,vaddr);
end;

procedure rmem_entry_add_track(tmap:Pointer;entry:p_rmem_map_entry;dst:QWORD);
var
 node:p_rmem_vaddr_instance;
 size:vm_offset_t;
 vaddr:QWORD;
begin
 //try add mirror track

 size:=(entry^.__end-entry^.start);

 vm_track_map_lock(tmap);

 node:=TAILQ_FIRST(@entry^.vlist);

 while (node<>nil) do
 begin

  vaddr:=node^.vaddr;
  if (vaddr<>dst) then
  begin
   _vm_track_map_insert_mirror(tmap,vaddr,vaddr+size,dst);
  end;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;

 vm_track_map_unlock(tmap);
end;

function _rmem_entry_del_node(entry:p_rmem_map_entry;node:p_rmem_vaddr_instance):Boolean;
begin
 Dec(entry^.count);
 TAILQ_REMOVE(@entry^.vlist,node,@node^.entry);

 FreeMem(node);

 Result:=(TAILQ_FIRST(@entry^.vlist)=nil);
end;

function rmem_entry_del_vaddr(entry:p_rmem_map_entry;vaddr:QWORD):Boolean;
var
 node:p_rmem_vaddr_instance;
begin
 node:=TAILQ_FIRST(@entry^.vlist);

 while (node<>nil) do
 begin

  if (node^.vaddr=vaddr) then
  begin
   Result:=_rmem_entry_del_node(entry,node);

   Exit;
  end;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;

 Result:=False;
end;

procedure rmem_entry_del_vaddr_all(entry:p_rmem_map_entry);
var
 node,next:p_rmem_vaddr_instance;
begin
 node:=TAILQ_FIRST(@entry^.vlist);

 while (node<>nil) do
 begin
  next:=TAILQ_NEXT(node,@node^.entry);

  _rmem_entry_del_node(entry,node);

  node:=next;
 end;
end;

//

function in_vaddr_list(const b:TAILQ_HEAD;vaddr:QWORD):Boolean;
var
 node:p_rmem_vaddr_instance;
begin
 Result:=False;

 node:=TAILQ_FIRST(@b);

 while (node<>nil) do
 begin

  if (node^.vaddr=vaddr) then
  begin
   Exit(True);
  end;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;
end;

function compare_vaddr_list(a:p_rmem_map_entry;offset:QWORD;b:p_rmem_map_entry):Boolean;
var
 node:p_rmem_vaddr_instance;
begin
 if (a^.count<>b^.count) then
 begin
  Exit(False);
 end;

 node:=TAILQ_FIRST(@a^.vlist);

 while (node<>nil) do
 begin

  if not in_vaddr_list(b^.vlist,node^.vaddr + offset) then
  begin
   Exit(False);
  end;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;

 Result:=True;
end;

procedure inc_vaddr_list(src:p_rmem_map_entry;offset:QWORD);
var
 node:p_rmem_vaddr_instance;
begin
 node:=TAILQ_FIRST(@src^.vlist);

 while (node<>nil) do
 begin
  node^.vaddr:=node^.vaddr + offset;

  node:=TAILQ_NEXT(node,@node^.entry);
 end;
end;

procedure copy_vaddr_list(src,dst:p_rmem_map_entry;offset:QWORD);
var
 node:p_rmem_vaddr_instance;
begin

 TAILQ_INIT(@dst^.vlist); //init
 dst^.count:=0;

 node:=TAILQ_FIRST(@src^.vlist);

 while (node<>nil) do
 begin
  _rmem_entry_add_vaddr(dst,node^.vaddr + offset);

  node:=TAILQ_NEXT(node,@node^.entry);
 end;
end;

//

procedure rmem_entry_deallocate(entry:p_rmem_map_entry);
begin
 rmem_entry_del_vaddr_all(entry);
 //
 Freemem(entry);
end;

procedure rmem_map_RANGE_CHECK(map:p_rmem_map;var start,__end:QWORD);
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

procedure rmem_map_lock(map:p_rmem_map);
begin
 mtx_lock(map^.lock);
end;

procedure rmem_map_process_deferred;
var
 td:p_kthread;
 entry,next:p_rmem_map_entry;
begin
 td:=curkthread;
 if (td=nil) then Exit;
 entry:=td^.td_rmap_def_user;
 td^.td_rmap_def_user:=nil;
 while (entry<>nil) do
 begin
  next:=entry^.next;
  rmem_entry_deallocate(entry);
  entry:=next;
 end;
end;

procedure rmem_map_unlock(map:p_rmem_map;def:Boolean=True);
begin
 mtx_unlock(map^.lock);
 if def then
 begin
  rmem_map_process_deferred;
 end;
end;

function rmem_map_locked(map:p_rmem_map):Boolean; inline;
begin
 Result:=mtx_owned(map^.lock);
end;

procedure RMEM_MAP_ASSERT_LOCKED(map:p_rmem_map); inline;
begin
 Assert(rmem_map_locked(map));
end;

procedure _rmem_map_init(map:p_rmem_map;min,max:QWORD); inline;
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset :=min;
 map^.max_offset :=max;
 map^.nentries:=0;
 map^.root:=nil;
end;

procedure rmem_map_init(map:p_rmem_map;min,max:QWORD);
begin
 _rmem_map_init(map, min, max);
 mtx_init(map^.lock,'rmap');
end;

function rmem_entry_create(map:p_rmem_map):p_rmem_map_entry;
var
 new_entry:p_rmem_map_entry;
begin
 new_entry:=AllocMem(SizeOf(t_rmem_map_entry));
 Assert((new_entry<>nil),'rmem_map_entry_create: kernel resources exhausted');

 TAILQ_INIT(@new_entry^.vlist);

 Result:=new_entry;
end;

function rmem_entry_splay(addr:QWORD;root:p_rmem_map_entry):p_rmem_map_entry;
var
 llist,rlist:p_rmem_map_entry;
 ltree,rtree:p_rmem_map_entry;
 y          :p_rmem_map_entry;
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

procedure rmem_entry_link(
           map        :p_rmem_map;
           after_where:p_rmem_map_entry;
           entry      :p_rmem_map_entry);
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 Inc(map^.nentries);
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   rmem_entry_splay(after_where^.start, map^.root);
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

procedure rmem_entry_unlink(
           map        :p_rmem_map;
           entry      :p_rmem_map_entry);
var
 next,prev,root:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 if (entry<>map^.root) then
 begin
  rmem_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=rmem_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

function rmem_map_lookup_entry(
           map    :p_rmem_map;
           address:QWORD;
           entry  :pp_rmem_map_entry):Boolean;
var
 cur:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

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

  cur:=rmem_entry_splay(address,cur);
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

function rmem_map_test(map:p_rmem_map;
                       start,__end:QWORD;
                       mode:t_rmem_test_mode):Boolean;
var
 entry:p_rmem_map_entry;
 prev:QWORD;
begin
 if rmem_map_lookup_entry(map,start,@entry) then
 begin
  //
 end else
 begin

  case mode of
   rt_continuity:
     begin
      Exit(False); //not continuity
     end;
   else;
  end;

  entry:=entry^.next;
 end;

 prev:=entry^.start;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  case mode of
   rt_intersection:
     begin
      if (__end>entry^.start) and (start<entry^.__end) then
      begin
       Exit(True); //found intersection
      end;
     end;
   rt_continuity:
     begin
      if (prev<>entry^.start) then
      begin
       Exit(False); //not continuity
      end;
      prev:=entry^.__end;
     end;
   else;
  end;

  entry:=entry^.next;
 end;

 case mode of
  rt_intersection:
    begin
     Result:=False; //not intersection
    end;
  rt_continuity:
    begin
     Result:=True;  //continuity
    end;
  else;
 end;

end;

function rmem_map_insert_internal(
           map  :p_rmem_map;
           after:p_rmem_map_entry;
           start,__end:QWORD):p_rmem_map_entry;
var
 new_entry:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

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
 new_entry:=rmem_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 {
  * Insert the new after into the list
  }
 rmem_entry_link(map, after, new_entry);

 //rmem_map_simplify_entry(map, new_entry);

 Result:=new_entry;
end;

procedure rmem_map_simplify_entry(map:p_rmem_map;entry:p_rmem_map_entry);
var
 next,prev:p_rmem_map_entry;
 prevsize,esize:QWORD;
begin
 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  prevsize:=prev^.__end - prev^.start;
  if (prev^.__end=entry^.start) and
     compare_vaddr_list(prev,prevsize,entry) then
  begin
   rmem_entry_unlink(map, prev);
   entry^.start:=prev^.start;

   //Move prev->entry
   rmem_entry_del_vaddr_all(entry);

   //move
   entry^.vlist:=prev^.vlist;
   entry^.count:=prev^.count;

   //zero
   TAILQ_INIT(@prev^.vlist);
   prev^.count:=0;

   rmem_entry_deallocate(prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  esize:=entry^.__end - entry^.start;
  if (entry^.__end=next^.start) and
     compare_vaddr_list(entry,esize,next) then
  begin
   rmem_entry_unlink(map, next);
   entry^.__end:=next^.__end;

   rmem_entry_deallocate(next);
  end;
 end;
end;

procedure _rmem_map_clip_start(map:p_rmem_map;entry:p_rmem_map_entry;start:QWORD);
var
 new_entry:p_rmem_map_entry;
 offset:QWORD;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 rmem_map_simplify_entry(map, entry);

 offset:=(start - entry^.start);

 //new_entry -> old_start..start
 //entry     -> start    ..old_end

 new_entry:=rmem_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;

 entry^.start:=start;

 copy_vaddr_list(entry,new_entry,0);

 inc_vaddr_list(entry,offset);

 rmem_entry_link(map, entry^.prev, new_entry);
end;

procedure rmem_map_clip_start(map:p_rmem_map;entry:p_rmem_map_entry;start:QWORD); inline;
begin
 if (start>entry^.start) then
 begin
  _rmem_map_clip_start(map,entry,start);
 end;
end;

procedure _rmem_map_clip_end(map:p_rmem_map;entry:p_rmem_map_entry;__end:QWORD);
var
 new_entry:p_rmem_map_entry;
 offset:QWORD;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 offset:=(__end - entry^.start);

 //entry     -> old_start..end
 //new_entry -> end      ..old_end

 new_entry:=rmem_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;

 entry^.__end:=__end;

 copy_vaddr_list(entry,new_entry,offset);

 rmem_entry_link(map, entry, new_entry);
end;

procedure rmem_map_clip_end(map:p_rmem_map;entry:p_rmem_map_entry;__end:QWORD); inline;
begin
 if (__end<entry^.__end) then
 begin
  _rmem_map_clip_end(map,entry,__end);
 end;
end;

function rmem_map_insert(map:p_rmem_map;
                         vaddr:QWORD;
                         start,__end:QWORD):Integer;
var
 entry:p_rmem_map_entry;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (map=nil) then
 begin
  Exit(KERN_INVALID_ARGUMENT);
 end;

 rmem_map_lock(map);

 rmem_map_RANGE_CHECK(map, start, __end);

 if (rmem_map_lookup_entry(map, start, @entry)) then
 begin
  rmem_map_clip_start(map, entry, start);
 end else
 begin
  entry:=rmem_map_insert_internal(map,entry,start,__end);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  rmem_map_clip_end(map, entry, __end);

  entry:=rmem_map_insert_internal(map,entry,entry^.__end,__end);

  if rmem_entry_add_vaddr(entry,vaddr) then
  begin
   rmem_entry_add_track(map^.tmap,entry,vaddr);
  end;

  entry:=entry^.next;
 end;

 rmem_map_unlock(map);

 Result:=(KERN_SUCCESS);
end;

procedure rmem_entry_delete(map:p_rmem_map;entry:p_rmem_map_entry);
begin
 rmem_entry_unlink(map, entry);

 begin
  entry^.next:=curkthread^.td_rmap_def_user;
  curkthread^.td_rmap_def_user:=entry;
 end;
end;

function rmem_map_delete(map:p_rmem_map;
                         vaddr:QWORD;
                         start,__end:QWORD):Integer;
var
 entry      :p_rmem_map_entry;
 first_entry:p_rmem_map_entry;
 next       :p_rmem_map_entry;
 offset     :QWORD;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (not rmem_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  rmem_map_clip_start(map, entry, start);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  rmem_map_clip_end(map, entry, __end);

  next:=entry^.next;

  offset:=entry^.start - start;

  if (vaddr=0) then
  begin
   //all
   rmem_entry_delete(map, entry);
  end else
  if rmem_entry_del_vaddr(entry,vaddr + offset) then
  begin
   //zero
   rmem_entry_delete(map, entry);
  end else
  begin
   //one

   rmem_map_simplify_entry(map,entry);

   next:=entry^.next;
  end;

  entry:=next;
 end;

 Result:=(KERN_SUCCESS);
end;

procedure rmem_entry_track(tmap:Pointer;entry:p_rmem_map_entry;diff,size,source:QWORD;tobj:Pointer);
var
 node:p_rmem_vaddr_instance;
 start:vm_offset_t;
 __end:vm_offset_t;
begin
 node:=TAILQ_FIRST(@entry^.vlist);

 while (node<>nil) do
 begin

  start:=node^.vaddr+diff;
  __end:=start+size;

  //Writeln('rmem_entry_track:',HexStr(start,16),'..',HexStr(__end,16),'..',HexStr(source,16));

  _vm_track_map_insert_deferred(tmap,start,__end,source,tobj);

  node:=TAILQ_NEXT(node,@node^.entry);
 end;
end;

procedure rmem_map_track(map:p_rmem_map;
                         start,__end,source:QWORD;
                         tobj:Pointer);
var
 entry:p_rmem_map_entry;

 e_start:QWORD;
 e___end:QWORD;

 diff:QWORD;
 size:QWORD;
begin
 rmem_map_lock(map);

 vm_track_map_lock(map^.tmap);

 if (rmem_map_lookup_entry(map, start, @entry)) then
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

  if (start>e_start) then
  begin
   e_start:=start;
  end;

  if (__end<e___end) then
  begin
   e___end:=__end;
  end;

  if (e___end>e_start) then
  begin
   diff:=(e_start-entry^.start);
   size:=(e___end-e_start);

   rmem_entry_track(map^.tmap,
                    entry,
                    diff,
                    size,
                    source+(e_start-start),
                    tobj);
  end;

  entry:=entry^.next;
 end;

 vm_track_map_unlock(map^.tmap);

 rmem_map_unlock(map);
end;




end.

