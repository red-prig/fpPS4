unit rmem_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam,
 kern_mtx;

type
 pp_rmem_map_entry=^p_rmem_map_entry;
 p_rmem_map_entry=^t_rmem_map_entry;
 t_rmem_map_entry=packed record
  prev          :p_rmem_map_entry; // previous entry
  next          :p_rmem_map_entry; // next entry
  left          :p_rmem_map_entry; // left child in binary search tree
  right         :p_rmem_map_entry; // right child in binary search tree
  start         :DWORD;            // start address
  __end         :DWORD;            // end address
  vaddr         :DWORD;            // virtual addr mapping
 end;

 p_rmem_map=^t_rmem_map;
 t_rmem_map=object
  header  :t_rmem_map_entry; // List of entries
  lock    :mtx;              // Lock for map data
  nentries:DWORD;            // Number of entries
  root    :p_rmem_map_entry; // Root of a binary search tree
  property min_offset:DWORD read header.start write header.start;
  property max_offset:DWORD read header.__end write header.__end;
 end;

procedure rmem_map_process_deferred;

procedure rmem_map_entry_deallocate(entry:p_rmem_map_entry);

procedure rmem_map_lock(map:p_rmem_map);
function  rmem_map_trylock(map:p_rmem_map):Boolean;
procedure rmem_map_unlock(map:p_rmem_map);
function  rmem_map_locked(map:p_rmem_map):Boolean; inline;

procedure rmem_map_init(map:p_rmem_map;min,max:QWORD);

procedure rmem_map_entry_dispose(map:p_rmem_map;entry:p_rmem_map_entry); inline;
function  rmem_map_entry_create(map:p_rmem_map):p_rmem_map_entry;

function  rmem_map_lookup_entry(
            map  :p_rmem_map;
            vaddr:DWORD;
            start:DWORD;
            entry:pp_rmem_map_entry):Boolean;

function  rmem_map_lookup_entry_any(
            map  :p_rmem_map;
            start:DWORD;
            entry:pp_rmem_map_entry):Boolean;

function  rmem_map_insert(
            map  :p_rmem_map;
            vaddr:DWORD;
            start:DWORD;
            __end:DWORD):Integer;

function  rmem_map_fixed(map   :p_rmem_map;
                         vaddr :DWORD;
                         start :DWORD;
                         length:DWORD):Integer;

procedure rmem_map_entry_delete(map:p_rmem_map;entry:p_rmem_map_entry);

function  rmem_map_delete(map  :p_rmem_map;
                          vaddr:DWORD;
                          start:DWORD;
                          __end:DWORD):Integer;

function  rmem_map_delete_any(map  :p_rmem_map;
                              start:DWORD;
                              __end:DWORD):Integer;

implementation

uses
 errno,
 kern_thr;

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

procedure rmem_map_entry_deallocate(entry:p_rmem_map_entry);
begin
 Freemem(entry);
end;

procedure rmem_map_RANGE_CHECK(map:p_rmem_map;var start,__end:DWORD);
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

function rmem_map_trylock(map:p_rmem_map):Boolean;
begin
 Result:=mtx_trylock(map^.lock);
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
  rmem_map_entry_deallocate(entry);
  entry:=next;
 end;
end;

procedure rmem_map_unlock(map:p_rmem_map);
begin
 mtx_unlock(map^.lock);
 rmem_map_process_deferred;
end;

function rmem_map_locked(map:p_rmem_map):Boolean; inline;
begin
 Result:=mtx_owned(map^.lock);
end;

procedure RMEM_MAP_ASSERT_LOCKED(map:p_rmem_map); inline;
begin
 Assert(rmem_map_locked(map));
end;

procedure _rmem_map_init(map:p_rmem_map;min,max:DWORD);
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
 _rmem_map_init(map, OFF_TO_IDX(min), OFF_TO_IDX(max));
 mtx_init(map^.lock,'rmap');
end;

procedure rmem_map_entry_dispose(map:p_rmem_map;entry:p_rmem_map_entry); inline;
begin
 FreeMem(entry);
end;

function rmem_map_entry_create(map:p_rmem_map):p_rmem_map_entry;
var
 new_entry:p_rmem_map_entry;
begin
 new_entry:=AllocMem(SizeOf(t_rmem_map_entry));
 Assert((new_entry<>nil),'rmem_map_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

//(start1<start2)
function cmp_lt(start1,vaddr1,start2,vaddr2:DWORD):Boolean; inline;
begin
 if (start1=start2) then
 begin
  Result:=(vaddr1<vaddr2);
 end else
 begin
  Result:=(start1<start2);
 end;
end;

//(start1>=start2)
function cmp_be(start1,vaddr1,start2,vaddr2:DWORD):Boolean; inline;
begin
 if (start1=start2) then
 begin
  Result:=(vaddr1>=vaddr2);
 end else
 begin
  Result:=(start1>=start2);
 end;
end;

function rmem_map_entry_splay(vaddr,start:DWORD;root:p_rmem_map_entry):p_rmem_map_entry;
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
  if cmp_lt(start,vaddr,root^.start,root^.vaddr) then
  begin
   y:=root^.left;
   if (y=nil) then break;
   if cmp_lt(start,vaddr,y^.start,y^.vaddr) and (y^.left<>nil) then
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
  if (start>=root^.__end) then
  begin
   y:=root^.right;
   if (y=nil) then break;
   if (start>=y^.__end) and (y^.right<>nil) then
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
 root^.left :=ltree;
 root^.right:=rtree;

 Result:=(root);
end;

procedure rmem_map_entry_link(
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
   rmem_map_entry_splay(after_where^.vaddr, after_where^.start, map^.root);
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

procedure rmem_map_entry_unlink(
           map        :p_rmem_map;
           entry      :p_rmem_map_entry);
var
 next,prev,root:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 if (entry<>map^.root) then
 begin
  rmem_map_entry_splay(entry^.vaddr, entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=rmem_map_entry_splay(entry^.vaddr, entry^.start, entry^.left);
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
           map  :p_rmem_map;
           vaddr:DWORD;
           start:DWORD;
           entry:pp_rmem_map_entry):Boolean;
var
 cur:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 {
  * If the map is empty, then the map entry immediately preceding
  * "start" is the map's header.
  }
 cur:=map^.root;
 if (cur=nil) then
 begin
  entry^:=@map^.header;
 end else
 //if (start>=cur^.start) and (cur^.__end>start) then
 if cmp_be(start,vaddr,cur^.start,cur^.vaddr) and
    (cur^.__end>start) then
 begin
  entry^:=cur;
  if (vaddr>=cur^.vaddr) and
     (vaddr<(cur^.vaddr+(cur^.__end-cur^.start))) then
  begin
   Exit(TRUE);
  end;
 end else
 begin
  {
   * Splay requires a write lock on the map.  However, it only
   * restructures the binary search tree; it does not otherwise
   * change the map.  Thus, the map's timestamp need not change
   * on a temporary upgrade.
   }
  cur:=rmem_map_entry_splay(vaddr,start,cur);
  map^.root:=cur;

  {
   * If "start" is contained within a map entry, the new root
   * is that map entry.  Otherwise, the new root is a map entry
   * immediately before or after "start".
   }
  if (start>=cur^.start) then
  begin
   entry^:=cur;
   if (cur^.__end>start) and
      (vaddr>=cur^.vaddr) and
      (vaddr<(cur^.vaddr+(cur^.__end-cur^.start))) then
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

function rmem_map_lookup_entry_any(
           map  :p_rmem_map;
           start:DWORD;
           entry:pp_rmem_map_entry):Boolean;
var
 cur:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 {
  * If the map is empty, then the map entry immediately preceding
  * "start" is the map's header.
  }
 cur:=map^.root;
 if (cur=nil) then
 begin
  entry^:=@map^.header;
 end else
 if (start>=cur^.start) and (cur^.__end>start) then
 begin
  entry^:=cur;
  Exit(TRUE);
 end else
 begin
  {
   * Splay requires a write lock on the map.  However, it only
   * restructures the binary search tree; it does not otherwise
   * change the map.  Thus, the map's timestamp need not change
   * on a temporary upgrade.
   }
  cur:=rmem_map_entry_splay(0,start,cur);
  map^.root:=cur;

  {
   * If "start" is contained within a map entry, the new root
   * is that map entry.  Otherwise, the new root is a map entry
   * immediately before or after "start".
   }
  if (start>=cur^.start) then
  begin
   entry^:=cur;
   if (cur^.__end>start) then
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

function rmem_map_insert(
           map  :p_rmem_map;
           vaddr:DWORD;
           start:DWORD;
           __end:DWORD):Integer;
var
 new_entry :p_rmem_map_entry;
 prev_entry:p_rmem_map_entry;
 temp_entry:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 {
  * Check that the start and end points are not bogus.
  }
 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 {
  * Find the entry prior to the proposed starting address; if it's part
  * of an existing entry, this range is bogus.
  }
 if rmem_map_lookup_entry(map,vaddr,start,@temp_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 {
  * Create a new entry
  }
 new_entry:=rmem_map_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 new_entry^.vaddr :=vaddr;

 {
  * Insert the new entry into the list
  }
 rmem_map_entry_link(map, prev_entry, new_entry);

 Result:=KERN_SUCCESS;
end;

function rmem_map_fixed(map   :p_rmem_map;
                        vaddr :DWORD;
                        start :DWORD;
                        length:DWORD):Integer;
var
 __end:DWORD;
begin
 __end:=start + length;
 rmem_map_lock(map);
 rmem_map_RANGE_CHECK(map, start, __end);
 Result:=rmem_map_insert(map, vaddr, start, __end);
 rmem_map_unlock(map);
end;

procedure _rmem_map_clip_start(map:p_rmem_map;entry:p_rmem_map_entry;start:DWORD);
var
 new_entry:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 new_entry:=rmem_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.vaddr:=entry^.vaddr + (start - entry^.start);
 entry^.start:=start;

 rmem_map_entry_link(map, entry^.prev, new_entry);
end;

procedure rmem_map_clip_start(map:p_rmem_map;entry:p_rmem_map_entry;start:DWORD);
begin
 if (start>entry^.start) then
 begin
  _rmem_map_clip_start(map,entry,start);
 end;
end;

procedure _rmem_map_clip_end(map:p_rmem_map;entry:p_rmem_map_entry;__end:DWORD);
var
 new_entry:p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 {
  * Create a new entry and insert it AFTER the specified entry
  }
 new_entry:=rmem_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end:=__end;
 new_entry^.vaddr:=new_entry^.vaddr + (__end - entry^.start);

 rmem_map_entry_link(map, entry, new_entry);
end;

procedure rmem_map_clip_end(map:p_rmem_map;entry:p_rmem_map_entry;__end:DWORD);
begin
 if (__end<entry^.__end) then
 begin
  _rmem_map_clip_end(map,entry,__end);
 end;
end;

procedure rmem_map_entry_delete(map:p_rmem_map;entry:p_rmem_map_entry);
begin
 rmem_map_entry_unlink(map, entry);

 entry^.next:=curkthread^.td_rmap_def_user;
 curkthread^.td_rmap_def_user:=entry;
end;

procedure unmap_dmem_gc(start,__end:DWORD); external;

procedure rmem_map_unmap_check(map  :p_rmem_map;
                               start:DWORD;
                               __end:DWORD);
var
 entry      :p_rmem_map_entry;
 first_entry:p_rmem_map_entry;
 s,e:DWORD;
begin

 if (not rmem_map_lookup_entry_any(map,start,@first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;
 end;

 repeat

  if (entry^.start>start) then
  begin
   s:=start;

   if (entry^.start>__end) then
   begin
    e:=__end;
   end else
   begin
    e:=entry^.start;
   end;

   if (s<>e) then
   begin
    unmap_dmem_gc(IDX_TO_OFF(s),IDX_TO_OFF(e));
   end;

   start:=e;
  end else
  if (entry^.__end>start) then
  begin
   start:=entry^.__end;
  end;

  if (start>=__end) or (entry=@map^.header) or (entry^.start>=__end) then
  begin
   Break;
  end;

  entry:=entry^.next;

 until false;
end;

function rmem_map_delete(map  :p_rmem_map;
                         vaddr:DWORD;
                         start:DWORD;
                         __end:DWORD):Integer;
var
 entry      :p_rmem_map_entry;
 first_entry:p_rmem_map_entry;
 next       :p_rmem_map_entry;
 vaend      :DWORD;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 if (start=__end) then
 begin
  Exit(0);
 end;

 vaend:=vaddr+(__end-start);

 {
  * Find the start of the region, and clip it
  }
 if (not rmem_map_lookup_entry(map,vaddr,start,@first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  if (vaend>entry^.vaddr) and (vaddr<entry^.vaddr+(entry^.__end-entry^.start)) then
  begin
   rmem_map_clip_start(map, entry, start);
  end;
 end;

 {
  * Step through all entries in this region
  }
 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  if (vaend>entry^.vaddr) and (vaddr<entry^.vaddr+(entry^.__end-entry^.start)) then
  begin
   rmem_map_clip_end(map, entry, __end);

   next:=entry^.next;

   rmem_map_entry_delete(map, entry);
  end else
  begin
   next:=entry^.next;
  end;

  entry:=next;
 end;

 rmem_map_unmap_check(map,start,__end);

 Result:=(0);
end;

function  rmem_map_delete_any(map  :p_rmem_map;
                              start:DWORD;
                              __end:DWORD):Integer;
var
 entry      :p_rmem_map_entry;
 first_entry:p_rmem_map_entry;
 next       :p_rmem_map_entry;
begin
 RMEM_MAP_ASSERT_LOCKED(map);

 if (start=__end) then
 begin
  Exit(0);
 end;

 {
  * Find the start of the region, and clip it
  }
 if (not rmem_map_lookup_entry(map,0,start,@first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;
  rmem_map_clip_start(map, entry, start);
 end;

 {
  * Step through all entries in this region
  }
 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  rmem_map_clip_end(map, entry, __end);

  next:=entry^.next;

  //_remove(*,entry^.start,entry^.__end);

  rmem_map_entry_delete(map, entry);

  entry:=next;
 end;

 unmap_dmem_gc(IDX_TO_OFF(start),IDX_TO_OFF(__end));

 Result:=(0);
end;


end.

