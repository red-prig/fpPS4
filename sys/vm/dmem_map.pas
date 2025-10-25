unit dmem_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vm,
 vmparam,
 vm_object,
 kern_mtx;

Const
 SCE_KERNEL_MAIN_DMEM_SIZE=$180000000; //6GB  //$120000000; 4GB (normal/pro?)

 max_valid_dmem=QWORD($5000000000);

type
 t_dest_acl=(acl_system,acl_app,acl_blockpool);

 t_dmem_entry_info=packed record
   m_type :Shortint;         // memory type
   m_acl  :Byte;             // who controls memory
   c_count:Byte;             // lock counter
   _align :Byte;
  end;

 pp_dmem_map_entry=^p_dmem_map_entry;
 p_dmem_map_entry=^t_dmem_map_entry;
 t_dmem_map_entry=packed record
  prev    :p_dmem_map_entry; // previous entry
  next    :p_dmem_map_entry; // next entry
  left    :p_dmem_map_entry; // left child in binary search tree
  right   :p_dmem_map_entry; // right child in binary search tree
  start   :DWORD;            // start address
  __end   :DWORD;            // end address
  adj_free:DWORD;            // amount of adjacent free space
  max_free:DWORD;            // max free space in subtree
  info    :t_dmem_entry_info;
 end;

 p_dmem_map=^t_dmem_map;
 t_dmem_map=object
  header  :t_dmem_map_entry; // List of entries
  lock    :mtx;              // Lock for map data
  nentries:DWORD;            // Number of entries
  size    :DWORD;            // size
  root    :p_dmem_map_entry; // Root of a binary search tree
  vmap    :Pointer;
  rmap    :Pointer;
  function  get_max_offset:DWORD;
  property  min_offset:DWORD read header.start   write header.start;
  property  max_offset:DWORD read get_max_offset write header.__end;
 end;

procedure dmem_map_entry_deallocate(entry:p_dmem_map_entry);

procedure dmem_map_lock   (map:p_dmem_map);
function  dmem_map_trylock(map:p_dmem_map):Boolean;
procedure dmem_map_unlock (map:p_dmem_map);
function  dmem_map_locked (map:p_dmem_map):Boolean; inline;

procedure dmem_map_init(map:p_dmem_map;min,max:QWORD);

procedure dmem_map_entry_dispose(map:p_dmem_map;entry:p_dmem_map_entry); inline;
function  dmem_map_entry_create (map:p_dmem_map):p_dmem_map_entry;

function  dmem_map_lookup_entry(
            map        :p_dmem_map;
            address    :DWORD;
            entry      :pp_dmem_map_entry):Boolean;

function  dmem_map_insert(
            map   :p_dmem_map;
            start :DWORD;
            __end :DWORD;
            m_type:DWORD;
            m_acl :t_dest_acl):Integer;

Function  dmem_map_query_available(map:p_dmem_map;start,__end,align:QWORD;var p_addr,p_size:QWORD):Integer;
Function  dmem_map_query          (map:p_dmem_map;offset:QWORD;flags,id:Integer;info:Pointer;size:QWORD):Integer;
Function  dmem_map_get_memory_type(map:p_dmem_map;info:Pointer):Integer;
Function  dmem_map_alloc          (map:p_dmem_map;start,__end,len,align:QWORD;mtype:Integer;m_acl:t_dest_acl;var p_addr:QWORD):Integer;
Function  dmem_map_release        (map:p_dmem_map;start,len:QWORD;m_acl:t_dest_acl;check:Boolean):Integer;

function  dmem_map_findspace(map   :p_dmem_map;
                             start :DWORD;
                             length:DWORD;
                             addr  :PDWORD):Integer;

procedure dmem_map_simplify_entry(map:p_dmem_map;entry:p_dmem_map_entry);

procedure dmem_map_entry_delete(map:p_dmem_map;entry:p_dmem_map_entry);

function  dmem_map_delete(map:p_dmem_map;start:DWORD;__end:DWORD;m_acl:t_dest_acl):Integer;

function  dmem_map_set_mtype(map  :p_dmem_map;
                             start:DWORD;
                             __end:DWORD;
                             mtype:Integer;
                             protw:Integer;
                             flags:Integer):Integer;

function  dmem_map_get_mtype(map  :p_dmem_map;
                             obj   :vm_object_t;
                             offset:QWORD;
                             pstart:PQWORD;
                             p__end:PQWORD;
                             pmtype:PInteger):Integer;

implementation

uses
 errno,
 kern_thr,
 systm,
 vm_map,
 rmem_map,
 kern_budget;

function IDX_TO_OFF(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function IsPowerOfTwo(x:QWORD):Boolean; inline;
begin
 Result:=(x and (x - 1))=0;
end;

function fastIntLog2(i:QWORD):QWORD; inline;
begin
 Result:=BsfQWORD(i);
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

function t_dmem_map.get_max_offset:DWORD;
begin
 if (header.__end > OFF_TO_IDX(kern_budget.DMEM_LIMIT)) then
 begin
  Result:=OFF_TO_IDX(kern_budget.DMEM_LIMIT);
 end else
 begin
  Result:=header.__end;
 end;
end;

procedure dmem_map_entry_deallocate(entry:p_dmem_map_entry);
begin
 Freemem(entry);
end;

procedure DMEM_MAP_RANGE_CHECK(map:p_dmem_map;var start,__end:DWORD);
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

procedure dmem_map_lock(map:p_dmem_map);
begin
 mtx_lock(map^.lock);
end;

function dmem_map_trylock(map:p_dmem_map):Boolean;
begin
 Result:=mtx_trylock(map^.lock);
end;

procedure dmem_map_unlock(map:p_dmem_map);
begin
 mtx_unlock(map^.lock);
end;

function dmem_map_locked(map:p_dmem_map):Boolean; inline;
begin
 Result:=mtx_owned(map^.lock);
end;

procedure DMEM_MAP_ASSERT_LOCKED(map:p_dmem_map); inline;
begin
 Assert(dmem_map_locked(map));
end;

procedure _dmem_map_init(map:p_dmem_map;min,max:DWORD);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset :=min;
 map^.max_offset :=max;
 map^.header.adj_free:=(max-min);
 map^.header.max_free:=(max-min);
 map^.nentries:=0;
 map^.size    :=0;
 map^.root    :=nil;
end;

procedure dmem_map_init(map:p_dmem_map;min,max:QWORD);
begin
 _dmem_map_init(map, OFF_TO_IDX(min), OFF_TO_IDX(max));
 mtx_init(map^.lock,'dmem');
end;

procedure dmem_map_entry_dispose(map:p_dmem_map;entry:p_dmem_map_entry); inline;
begin
 FreeMem(entry);
end;

function dmem_map_entry_create(map:p_dmem_map):p_dmem_map_entry;
var
 new_entry:p_dmem_map_entry;
begin
 new_entry:=AllocMem(SizeOf(t_dmem_map_entry));
 Assert((new_entry<>nil),'dmem_map_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

procedure dmem_map_entry_set_max_free(entry:p_dmem_map_entry);
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

function dmem_map_entry_splay(addr:DWORD;root:p_dmem_map_entry):p_dmem_map_entry;
var
 llist,rlist:p_dmem_map_entry;
 ltree,rtree:p_dmem_map_entry;
 y          :p_dmem_map_entry;
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
    dmem_map_entry_set_max_free(root);
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
    dmem_map_entry_set_max_free(root);
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
  dmem_map_entry_set_max_free(llist);
  ltree:=llist;
  llist:=y;
 end;
 rtree:=root^.right;
 while (rlist<>nil) do
 begin
  y:=rlist^.left;
  rlist^.left:=rtree;
  dmem_map_entry_set_max_free(rlist);
  rtree:=rlist;
  rlist:=y;
 end;

 {
  * Final assembly: add ltree and rtree as subtrees of root.
  }
 root^.left:=ltree;
 root^.right:=rtree;
 dmem_map_entry_set_max_free(root);

 Result:=(root);
end;

procedure dmem_map_entry_link(
           map        :p_dmem_map;
           after_where:p_dmem_map_entry;
           entry      :p_dmem_map_entry);
var
 i:DWORD;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 Inc(map^.nentries);
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   dmem_map_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left:=after_where;
  after_where^.right:=nil;
  after_where^.adj_free:=entry^.start - after_where^.__end;
  dmem_map_entry_set_max_free(after_where);
 end else
 begin
  entry^.right:=map^.root;
  entry^.left:=nil;
 end;
 if (entry^.next=@map^.header) then
 begin
  i:=map^.max_offset;
 end else
 begin
  i:=entry^.next^.start;
 end;
 entry^.adj_free:=i-entry^.__end;
 dmem_map_entry_set_max_free(entry);
 map^.root:=entry;
end;

procedure dmem_map_entry_unlink(
           map        :p_dmem_map;
           entry      :p_dmem_map_entry);
var
 next,prev,root:p_dmem_map_entry;
 i:DWORD;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 if (entry<>map^.root) then
 begin
  dmem_map_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=dmem_map_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
  if (entry^.next=@map^.header) then
  begin
   i:=map^.max_offset;
  end else
  begin
   i:=entry^.next^.start;
  end;
  root^.adj_free:=i-root^.__end;
  dmem_map_entry_set_max_free(root);
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

procedure dmem_map_entry_resize_free(map:p_dmem_map;entry:p_dmem_map_entry);
begin
 if (entry<>map^.root) then
 begin
  map^.root:=dmem_map_entry_splay(entry^.start, map^.root);
 end;

 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;
 dmem_map_entry_set_max_free(entry);
end;

function dmem_map_lookup_entry(
           map        :p_dmem_map;
           address    :DWORD;
           entry      :pp_dmem_map_entry):Boolean;
var
 cur:p_dmem_map_entry;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 {
  * If the map is empty, then the map entry immediately preceding
  * "address" is the map's header.
  }
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
  {
   * Splay requires a write lock on the map.  However, it only
   * restructures the binary search tree; it does not otherwise
   * change the map.  Thus, the map's timestamp need not change
   * on a temporary upgrade.
   }
  cur:=dmem_map_entry_splay(address,cur);
  map^.root:=cur;

  {
   * If "address" is contained within a map entry, the new root
   * is that map entry.  Otherwise, the new root is a map entry
   * immediately before or after "address".
   }
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

function dmem_map_insert(
           map   :p_dmem_map;
           start :DWORD;
           __end :DWORD;
           m_type:DWORD;
           m_acl :t_dest_acl):Integer;
var
 new_entry :p_dmem_map_entry;
 prev_entry:p_dmem_map_entry;
 temp_entry:p_dmem_map_entry;
 info      :t_dmem_entry_info;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 {
  * Check that the start and end points are not bogus.
  }
 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(EINVAL);
 end;

 {
  * Find the entry prior to the proposed starting address; if it's part
  * of an existing entry, this range is bogus.
  }
 if dmem_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(EAGAIN);
 end;

 prev_entry:=temp_entry;

 {
  * Assert that the next entry doesn't overlap the end point.
  }
 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(EAGAIN);
 end;

 info:=Default(t_dmem_entry_info);
 info.m_type:=m_type;
 info.m_acl :=ord(m_acl);

 if (prev_entry<>@map^.header) and
    (prev_entry^.__end=start) then
 begin
  {
   * We were able to extend the object.  Determine if we
   * can extend the previous map entry to include the
   * new range as well.
   }
  if (DWORD(prev_entry^.info)=DWORD(info)) then
  begin
   map^.size:=map^.size+(__end - prev_entry^.__end);
   prev_entry^.__end:=__end;
   //change size

   dmem_map_entry_resize_free(map, prev_entry);
   Exit(0);
  end;

 end;

 {
  * Create a new entry
  }
 new_entry:=dmem_map_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;
 new_entry^.info :=info;

 {
  * Insert the new entry into the list
  }
 dmem_map_entry_link(map, prev_entry, new_entry);
 map^.size:=map^.size+(new_entry^.__end - new_entry^.start);

 dmem_map_simplify_entry(map, new_entry);

 Result:=0;
end;

Function dmem_map_query_available(map:p_dmem_map;start,__end,align:QWORD;var p_addr,p_size:QWORD):Integer;
var
 entry:p_dmem_map_entry;

 r_addr,r_size:QWORD;

 t_aligned:QWORD;
 t_sizeof :QWORD;
 t_startof:QWORD;
 t_endof  :QWORD;
 t_freeof :QWORD;
begin
 Result:=0;

 if not IsPowerOfTwo(align) then
 begin
  Exit(EINVAL);
 end;

 if (align<PAGE_SIZE) then align:=PAGE_SIZE;

 start:=(not (start shr 63)) and start;
 if (start>max_valid_dmem) then start:=max_valid_dmem;

 __end:=(not (__end shr 63)) and __end;
 if (__end>max_valid_dmem) then __end:=max_valid_dmem;

 start:=AlignUp(start,align);

 dmem_map_lock(map);

 if (map^.root=nil) then
 begin
  if (start>=IDX_TO_OFF(map^.max_offset)) then
  begin
   r_addr:=0;
   r_size:=0;
   Result:=ENOMEM;
  end else
  begin
   r_addr:=start;
   r_size:=IDX_TO_OFF(map^.max_offset)-start;
   Result:=0;
  end;
 end else
 begin
  r_addr:=0;
  r_size:=0;
  Result:=ENOMEM;

  map^.root:=dmem_map_entry_splay(OFF_TO_IDX(start), map^.root);
  entry:=map^.root;

  while (entry<>nil) and (entry<>@map^.header) do
  begin
   if (entry^.adj_free<>0) then
   begin
    t_startof:=IDX_TO_OFF(entry^.__end);    //start of free space

    if (t_startof<start) then               //clamp
    begin
     t_aligned:=start;
    end else
    begin
     t_aligned:=AlignUp(t_startof,align);   //align strict
    end;

    if (__end<t_aligned) then Break;

    t_freeof:=IDX_TO_OFF(entry^.adj_free);  //size of free space
    t_endof:=t_startof+t_freeof;            //end of free space

    if (t_endof>__end) then                 //clamp
    begin
     t_endof:=__end;
    end;

    if (t_endof>t_aligned) then
    begin
     t_sizeof:=t_endof-t_aligned;           //size of aligned free space
     if (t_sizeof>r_size) then
     begin
      //save the larger space
      r_addr:=t_aligned;
      r_size:=t_sizeof;
     end;
     Result:=0;  //mark something found
    end;

   end;
   entry:=entry^.next;
  end;
 end;

 dmem_map_unlock(map);

 p_addr:=r_addr;
 p_size:=r_size;
end;

type
 p_dmem_query_info=^t_dmem_query_info;
 t_dmem_query_info=packed record
  start:QWORD;
  __end:QWORD;
  mtype:Integer;
  align:Integer;
 end;
 {$IF sizeof(t_dmem_query_info)<>24}{$STOP sizeof(t_dmem_query_info)<>24}{$ENDIF}

Function dmem_map_query(map:p_dmem_map;offset:QWORD;flags,id:Integer;info:Pointer;size:QWORD):Integer;
var
 data :t_dmem_query_info;
 entry:p_dmem_map_entry;
 index:DWORD;
begin
 Result:=0;

 if (flags>1) then
 begin
  Exit(EINVAL);
 end;

 Assert(id=0,'dmem_map_query (id<>0)');

 data:=Default(t_dmem_query_info);

 Result:=EACCES;

 dmem_map_lock(map);

 if (map^.root<>nil) then
 begin

  index:=OFF_TO_IDX(offset);

  map^.root:=dmem_map_entry_splay(index, map^.root);
  entry:=map^.root;

  if (entry=@map^.header) then
  begin
   //EACCES
  end else
  if (entry^.info.m_acl<>ord(acl_app)) then
  begin
   //EACCES
  end else
  begin

   if (entry^.start<=index) and
      (entry^.__end>index) then
   begin
    Result:=0;
   end;

   if (Result<>0) and
      ((flags and 1)<>0) then
   begin
    while (entry<>@map^.header) do
    begin
     if (entry^.info.m_acl<>ord(acl_app)) then
     begin
      //EACCES
      Break;
     end else
     if (entry^.info.m_type<>-1) and
        (entry^.__end>index) then
     begin
      Result:=0;
      Break;
     end;
     entry:=entry^.next;
    end;
   end;

  end;

  if (Result=0) then
  begin
   data.start:=IDX_TO_OFF(entry^.start);
   data.__end:=IDX_TO_OFF(entry^.__end);
   data.mtype:=entry^.info.m_type;
  end;

 end;

 dmem_map_unlock(map);

 if (Result<>0) then Exit;

 if (size>sizeof(t_dmem_query_info)) then
 begin
  size:=sizeof(t_dmem_query_info);
 end;

 Result:=copyout(@data,info,size);
end;

type
 PGetDirectMemoryType=^TGetDirectMemoryType;
 TGetDirectMemoryType=packed record
  start    :QWORD; //in
  start_out:QWORD; //out
  __end_out:QWORD; //out
  mtype_out:DWORD; //out
  align    :Integer;
 end;

Function dmem_map_get_memory_type(map:p_dmem_map;info:Pointer):Integer;
var
 data:PGetDirectMemoryType;
 entry:p_dmem_map_entry;
 index:DWORD;
begin
 data:=info;

 Result:=ENOENT;

 dmem_map_lock(map);

 if (map^.root<>nil) then
 begin

  index:=OFF_TO_IDX(data^.start);

  map^.root:=dmem_map_entry_splay(index, map^.root);
  entry:=map^.root;

  if (entry=@map^.header) then
  begin
   //ENOENT
  end else
  if (entry^.info.m_acl<>ord(acl_app)) then
  begin
   //ENOENT
  end else
  if (entry^.start<=index) and
     (entry^.__end>index) then
  begin
   Result:=0;
  end;

  if (Result=0) then
  begin
   data^.start_out:=IDX_TO_OFF(entry^.start);
   data^.__end_out:=IDX_TO_OFF(entry^.__end);
   data^.mtype_out:=entry^.info.m_type;
  end;

 end;

 dmem_map_unlock(map);
end;

Function dmem_map_alloc(map:p_dmem_map;start,__end,len,align:QWORD;mtype:Integer;m_acl:t_dest_acl;var p_addr:QWORD):Integer;
var
 adr_dw:DWORD;
begin
 Result:=0;

 if (Int64(__end or start) < 0) then
 begin
  Exit(EINVAL);
 end;

 if (Int64(len) < 1) then
 begin
  Exit(EINVAL);
 end;

 if (DWORD(mtype)>10) then
 begin
  Exit(EINVAL);
 end;

 if not IsPowerOfTwo(align) then
 begin
  Exit(EINVAL);
 end;

 if (( (align and QWORD($8000000000003fff)) or QWORD(len and QWORD(PAGE_MASK)) )<>0) then
 begin
  Exit(EINVAL);
 end;

 if (align=0) then align:=1;

 start:=(not (start shr 63)) and start;

 if (__end>max_valid_dmem) then __end:=max_valid_dmem;

 if (__end <= start) then
 begin
  Exit(EAGAIN);
 end;

 if (__end < len) then
 begin
  Exit(EAGAIN);
 end;

 if ((__end - len) < start) then
 begin
  Exit(EAGAIN);
 end;

 if (((align - 1) + __end) < __end) then
 begin
  Exit(EAGAIN);
 end;

 dmem_map_lock(map);

 repeat
  adr_dw:=0;

  if (dmem_map_findspace(map, OFF_TO_IDX(start), OFF_TO_IDX(len), @adr_dw)<>0) then
  begin
   dmem_map_unlock(map);
   Exit(EAGAIN);
  end;

  start:=IDX_TO_OFF(adr_dw);

  start:=AlignUp(start,align);

  if (start>=__end) then
  begin
   dmem_map_unlock(map);
   Exit(EAGAIN);
  end;

  if ((start+len)>__end) then
  begin
   dmem_map_unlock(map);
   Exit(EAGAIN);
  end;

  Result:=dmem_map_insert(map,OFF_TO_IDX(start),OFF_TO_IDX(start+len),mtype,m_acl);
 until (Result<>EAGAIN);

 dmem_map_unlock(map);

 if (Result=0) then
 begin
  p_addr:=start;
 end;
end;

function _dmem_map_test(map  :p_dmem_map;
                        start:DWORD;
                        __end:DWORD;
                        protw:Integer;
                        m_acl:t_dest_acl):Boolean;
var
 curr,next,entry:p_dmem_map_entry;
begin
 Result:=True;

 if dmem_map_lookup_entry(map,start,@entry) then
 begin
  //
 end else
 begin
  Exit(False);
 end;

 curr:=entry;
 while (curr<>@map^.header) and (curr^.start<__end) do
 begin

  if (curr^.info.m_acl<>ord(m_acl)) then
  begin
   Exit(False);
  end;

  if (protw<>0) and (curr^.info.m_type=SCE_KERNEL_WB_GARLIC) then
  begin
   Exit(False);
  end;

  next:=curr^.next;

  if (next=@map^.header) then
  begin
   if (curr^.__end<__end) then
   begin
    Exit(False);
   end;
  end else
  if (next^.info.m_acl<>ord(m_acl)) then
  begin
   Exit(False);
  end else
  if (curr^.__end<__end) and
     (curr^.__end<>next^.start) then
  begin
   Exit(False);
  end;

  curr:=next;
 end;

end;

procedure rmem_entry_del_vmap(vmap:vm_map_t;entry:p_rmem_map_entry);
var
 node:p_rmem_vaddr_instance;
 vaddr,size:QWORD;
begin
 size:=(entry^.__end-entry^.start);

 node:=TAILQ_FIRST(@entry^.vlist);

 while (node<>nil) do
 begin
  vaddr:=node^.vaddr;

  vm_map_delete(vmap, vaddr, vaddr + size, MAP_COW_NO_RMAP_FREE);

  node:=TAILQ_NEXT(node,@node^.entry);
 end;
end;

Function dmem_map_release(map:p_dmem_map;start,len:QWORD;m_acl:t_dest_acl;check:Boolean):Integer;
var
 offset:QWORD;
 rmap:p_rmem_map;
 vmap:vm_map_t;
 td:p_kthread;
 entry:p_rmem_map_entry;
begin
 if (((len or start) and QWORD($8000000000003fff))<>0) then
 begin
  Exit(EINVAL);
 end;

 if (Int64(start) >= max_valid_dmem) then
 begin
  Exit(0);
 end;

 offset:=max_valid_dmem - start;

 if (Int64(len) < Int64(offset)) then
 begin
  offset:=len;
 end;

 if (offset=0) then
 begin
  Exit(0);
 end;

 dmem_map_lock(map);

 if check then
 begin
  //test c_count?
  if not _dmem_map_test(map,OFF_TO_IDX(start),OFF_TO_IDX(start+len),0,m_acl) then
  begin
   dmem_map_unlock(map);
   Exit(ENOENT);
  end;
 end;

 Result:=dmem_map_delete(map,OFF_TO_IDX(start),OFF_TO_IDX(start+len),m_acl);

 dmem_map_unlock(map);

 if (Result=0) then
 begin
  rmap:=map^.rmap;

  rmem_map_process_deferred; //flush

  rmem_map_lock(rmap);

   Result:=rmem_map_delete(rmap,0,start,start+len);

  //dont call this rmem_map_process_deferred
  rmem_map_unlock(rmap,False);

  if (Result=0) then
  begin
   td:=curkthread;
   if (td<>nil) then
   if (td^.td_rmap_def_user<>nil) then
   begin
    vmap:=map^.vmap;

    //iterate rmem_map_process_deferred

    entry:=td^.td_rmap_def_user;

    vm_map_lock(vmap);

     while (entry<>nil) do
     begin
      rmem_entry_del_vmap(vmap,entry);

      entry:=entry^.next;
     end;

    vm_map_unlock(vmap);

    //free all
    rmem_map_process_deferred;
   end;
  end;
 end;

end;

function dmem_map_findspace(map   :p_dmem_map;
                            start :DWORD;
                            length:DWORD;
                            addr  :PDWORD):Integer;
label
 _nxt;
var
 entry:p_dmem_map_entry;
 st:DWORD;
begin
 {
  * Request must fit within min/max VM address and must avoid
  * address wrap.
  }
 if (start<map^.min_offset) then
 begin
  start:=map^.min_offset;
 end;
 if (start + length>map^.max_offset) or (start + length<start) then
 begin
  Exit(1);
 end;

 { Empty tree means wide open address space. }
 if (map^.root=nil) then
 begin
  addr^:=start;
  Exit(0);
 end;

 {
  * After splay, if start comes before root node, then there
  * must be a gap from start to the root.
  }
 map^.root:=dmem_map_entry_splay(start, map^.root);
 if (start + length<=map^.root^.start) then
 begin
  addr^:=start;
  Exit(0);
 end;

 {
  * Root is the last node that might begin its gap before
  * start, and this is the last comparison where address
  * wrap might be a problem.
  }

 if (start>map^.root^.__end) then
 begin
  st:=start;
 end else
 begin
  st:=map^.root^.__end;
 end;

 if (length<=map^.root^.__end + map^.root^.adj_free - st) then
 begin
  addr^:=st;
  Exit(0);
 end;

 { With max_free, can immediately tell if no solution. }
 entry:=map^.root^.right;

 if (entry=nil) then
 begin
  Exit(1);
 end;

 if (length>entry^.max_free) then
 begin
  Exit(1);
 end;

 {
  * Search the right subtree in the order: left subtree, root,
  * right subtree (first fit).  The previous splay implies that
  * all regions in the right subtree have addresses>start.
  }
 while (entry<>nil) do
 begin
  if (entry^.left<>nil) then
  begin
   if not (entry^.left^.max_free>=length) then goto _nxt;
   entry:=entry^.left;
  end else
  begin
   _nxt:
   if (entry^.adj_free>=length) then
   begin
    addr^:=entry^.__end;
    Exit(0);
   end else
   begin
    entry:=entry^.right;
   end;
  end;
 end;

 { Can't get here, so panic if we do. }
 Assert(false,'dmem_map_findspace: max_free corrupt');
end;

procedure dmem_map_simplify_entry(map:p_dmem_map;entry:p_dmem_map_entry);
var
 next,prev:p_dmem_map_entry;
begin
 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  if (prev^.__end=entry^.start) and
     (DWORD(prev^.info)=DWORD(entry^.info)) then
  begin
   dmem_map_entry_unlink(map, prev);
   entry^.start:=prev^.start;

   //change
   if (entry^.prev<>@map^.header) then
   begin
    dmem_map_entry_resize_free(map, entry^.prev);
   end;

   dmem_map_entry_dispose(map, prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  if (entry^.__end=next^.start) and
     (DWORD(next^.info)=DWORD(entry^.info)) then
  begin
   dmem_map_entry_unlink(map, next);
   entry^.__end:=next^.__end;
   //change
   dmem_map_entry_resize_free(map, entry);

   dmem_map_entry_dispose(map, next);
  end;
 end;
end;

procedure _dmem_map_clip_start(map:p_dmem_map;entry:p_dmem_map_entry;start:DWORD);
var
 new_entry:p_dmem_map_entry;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 dmem_map_simplify_entry(map, entry);

 new_entry:=dmem_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.start:=start;

 dmem_map_entry_link(map, entry^.prev, new_entry);
end;

procedure dmem_map_clip_start(map:p_dmem_map;entry:p_dmem_map_entry;start:DWORD);
begin
 if (start>entry^.start) then
 begin
  _dmem_map_clip_start(map,entry,start);
 end;
end;

procedure _dmem_map_clip_end(map:p_dmem_map;entry:p_dmem_map_entry;__end:DWORD);
var
 new_entry:p_dmem_map_entry;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 {
  * Create a new entry and insert it AFTER the specified entry
  }
 new_entry:=dmem_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end:=__end;

 dmem_map_entry_link(map, entry, new_entry);
end;

procedure dmem_map_clip_end(map:p_dmem_map;entry:p_dmem_map_entry;__end:DWORD);
begin
 if (__end<entry^.__end) then
 begin
  _dmem_map_clip_end(map,entry,__end);
 end;
end;

procedure dmem_map_entry_delete(map:p_dmem_map;entry:p_dmem_map_entry);
var
 size:DWORD;
begin
 dmem_map_entry_unlink(map, entry);
 size:=entry^.__end - entry^.start;
 map^.size:=map^.size-size;

 dmem_map_entry_deallocate(entry);
end;

function dmem_map_delete(map:p_dmem_map;start:DWORD;__end:DWORD;m_acl:t_dest_acl):Integer;
var
 entry      :p_dmem_map_entry;
 first_entry:p_dmem_map_entry;
 next       :p_dmem_map_entry;
begin
 DMEM_MAP_ASSERT_LOCKED(map);

 if (start=__end) then
 begin
  Exit(0);
 end;

 {
  * Find the start of the region, and clip it
  }
 if (not dmem_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;
  if (entry^.info.m_acl  =ord(m_acl)) and
     (entry^.info.c_count=0) then
  begin
   dmem_map_clip_start(map, entry, start);
  end;
 end;

 {
  * Step through all entries in this region
  }
 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  next:=entry^.next;

  if (entry^.info.m_acl  =ord(m_acl)) and
     (entry^.info.c_count=0) then
  begin
   dmem_map_clip_end(map, entry, __end);

   dmem_map_entry_delete(map, entry);
  end;

  entry:=next;
 end;
 Result:=(0);
end;

function dmem_map_set_mtype(map  :p_dmem_map;
                            start:DWORD;
                            __end:DWORD;
                            mtype:Integer;
                            protw:Integer;
                            flags:Integer):Integer; public;
label
 _EACCES;
var
 current,next:p_dmem_map_entry;
begin
 if (start=__end) then
 begin
  Exit(0);
 end;

 if ((flags and MAP_WRITABLE_WB_GARLIC)<>0) then
 begin
  //allow GPU write
  protw:=(protw and VM_PROT_WRITE);
 end else
 begin
  //don allow GPU write
  protw:=(protw and (VM_PROT_WRITE or VM_PROT_GPU_WRITE));
 end;

 if (mtype=SCE_KERNEL_WB_GARLIC) and (protw<>0) then
 begin
  Exit(EACCES);
 end;

 if (mtype<>-1) then
 begin
  //do not check if changed
  protw:=0;
 end;

 dmem_map_lock(map);

 DMEM_MAP_RANGE_CHECK(map, start, __end);

 if not _dmem_map_test(map,start,__end,protw,acl_app) then
 begin
  _EACCES:
   dmem_map_unlock(map);
   Exit(EACCES);
 end;

 if (mtype=-1) then
 begin
  dmem_map_unlock(map);
  Exit(0);
 end;

 if (dmem_map_lookup_entry(map, start, @current)) then
 begin
  //
  if (current^.info.m_type<>mtype) and
     (current^.info.c_count=0) then
  begin
   dmem_map_clip_start(map, current, start);
  end;
  //
 end else
 begin
  goto _EACCES;
 end;

 while ((current<>@map^.header) and (current^.start<__end)) do
 begin
  next:=current^.next;

  if (current^.info.m_type<>mtype) and
     (current^.info.c_count=0) then
  begin
   //
   dmem_map_clip_end(map, current, __end);

   current^.info.m_type:=mtype;

   if ((flags and MAP_NO_COALESCE)=0) then
   begin
    dmem_map_simplify_entry(map, current);
   end;
   //
  end;

  current:=next;
 end;

 dmem_map_unlock(map);

 Result:=0;
end;

function dmem_map_get_mtype(map  :p_dmem_map;
                            obj   :vm_object_t;
                            offset:QWORD;
                            pstart:PQWORD;
                            p__end:PQWORD;
                            pmtype:PInteger):Integer;
var
 entry:p_dmem_map_entry;
begin

 if ((obj^.flags and OBJ_DMEM_EXT)=0) then
 begin
  Exit(ENODEV);
 end;

 if (Int64(offset)<0) then
 begin
  Exit(EINVAL);
 end;

 Result:=ENOENT;

 dmem_map_lock(map);

  if (dmem_map_lookup_entry(map, OFF_TO_IDX(offset), @entry)) then
  begin
   if (entry^.info.m_acl=ord(acl_app)) then
   begin
    pstart^:=IDX_TO_OFF(entry^.start);
    p__end^:=IDX_TO_OFF(entry^.__end);
    pmtype^:=entry^.info.m_type;

    Result:=0;
   end;
  end;

 dmem_map_unlock(map);
end;


end.

