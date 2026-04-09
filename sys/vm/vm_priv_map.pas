unit vm_priv_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 uma,
 kern_mtx,
 md_map,
 vm_nt_map;

type
 pp_vm_priv_map_entry=^p_vm_priv_map_entry;
 p_vm_priv_map_entry=^t_vm_priv_map_entry;
 t_vm_priv_map_entry=packed record
  prev    :p_vm_priv_map_entry;
  next    :p_vm_priv_map_entry;
  left    :p_vm_priv_map_entry;
  right   :p_vm_priv_map_entry;
  start   :DWORD;
  __end   :DWORD;
  adj_free:DWORD;
  max_free:DWORD;
  refs    :DWORD;
  flags   :DWORD;
 end;

 vm_priv_zone=uma_zone_t;

 p_vm_priv_map=^t_vm_priv_map;
 t_vm_priv_map=object
  zone    :vm_priv_zone;
  header  :t_vm_priv_map_entry;
  root    :p_vm_priv_map_entry;
  size    :DWORD;
  nentries:DWORD;
  property  min_offset:DWORD read header.start write header.start;
  property  max_offset:DWORD read header.__end write header.__end;
 end;

 p_vm_priv_pool=^t_vm_priv_pool;
 t_vm_priv_pool=record
  zone:vm_priv_zone;
  lock:mtx;
  list:TAILQ_HEAD;
  size:QWORD;
  invm:QWORD;
 end;

 p_vm_priv_fd=^t_vm_priv_fd;
 t_vm_priv_fd=record
  elist:TAILQ_ENTRY;
  pool :p_vm_priv_pool;
  obj  :vm_nt_file_obj;
  map  :t_vm_priv_map;
  inval:DWORD;
 end;

 p_vm_priv_alloc=^t_vm_priv_alloc;
 t_vm_priv_alloc=record
  obj  :p_vm_nt_file_obj;
  start:DWORD;
  size :DWORD;
 end;

 t_vm_priv_space=packed record
  addr:DWORD;
  size:DWORD;
 end;

 t_vm_priv_fit=packed record
  max__free:t_vm_priv_space;
  max__invl:t_vm_priv_space;
  best_free:t_vm_priv_space;
  best_invl:t_vm_priv_space;
 end;

 t_vm_priv_cb=procedure(data:Pointer;entry:p_vm_priv_map_entry);

function  vm_priv_new_zone():vm_priv_zone;

procedure vm_priv_map_entry_delete(map:p_vm_priv_map;entry:p_vm_priv_map_entry);

procedure vm_priv_map_init     (map:p_vm_priv_map;zone:vm_priv_zone;min,max:DWORD);
function  vm_priv_map_insert   (map:p_vm_priv_map;start,__end:DWORD):Integer;
function  vm_priv_map_delete   (map:p_vm_priv_map;start,__end:DWORD):Integer;
function  vm_priv_map_foreach  (map:p_vm_priv_map;start,__end:DWORD;data:Pointer;cb:t_vm_priv_cb):Integer;
function  vm_priv_map_findspace(map:p_vm_priv_map;start,length:DWORD):t_vm_priv_space;
function  vm_priv_map_find_best(map:p_vm_priv_map;length:DWORD):t_vm_priv_fit;

procedure vm_priv_pool_init (pool:p_vm_priv_pool);
function  vm_priv_pool_alloc(pool:p_vm_priv_pool;length:DWORD;p_out:p_vm_priv_alloc):Integer;

implementation

const
 //Return values from the VM routines.
 KERN_SUCCESS        =0;
 KERN_INVALID_ADDRESS=1;
 KERN_NO_SPACE       =3;

function vm_priv_new_zone():vm_priv_zone;
begin
 Result:=uma_zcreate('MAP PRIV ENTRY', sizeof(t_vm_priv_map_entry), nil, nil, nil, nil, UMA_ALIGN_PTR, 0);
end;

procedure vm_priv_map_init(map:p_vm_priv_map;zone:vm_priv_zone;min,max:DWORD);
begin
 FillChar(map^,sizeof(t_vm_priv_map),0);
 map^.zone:=zone;
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.header.adj_free:=(max-min);
 map^.header.max_free:=(max-min);
end;

procedure vm_priv_map_entry_dispose(map:p_vm_priv_map;entry:p_vm_priv_map_entry); inline;
begin
 if (map^.zone=nil) then
 begin
  FreeMem(entry);
 end else
 begin
  uma_zfree(map^.zone, entry);
 end;
end;

function vm_priv_map_entry_create(map:p_vm_priv_map):p_vm_priv_map_entry;
var
 new_entry:p_vm_priv_map_entry;
begin
 if (map^.zone=nil) then
 begin
  new_entry:=AllocMem(sizeof(t_vm_priv_map_entry));
 end else
 begin
  new_entry:=uma_zalloc(map^.zone, M_WAITOK or M_ZERO);
 end;
 Assert((new_entry<>nil),'vm_priv_map_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

procedure vm_priv_map_entry_set_max_free(entry:p_vm_priv_map_entry);
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

function vm_priv_map_entry_splay(addr:DWORD;root:p_vm_priv_map_entry):p_vm_priv_map_entry;
var
 llist,rlist:p_vm_priv_map_entry;
 ltree,rtree:p_vm_priv_map_entry;
 y          :p_vm_priv_map_entry;
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
    vm_priv_map_entry_set_max_free(root);
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
    vm_priv_map_entry_set_max_free(root);
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
  vm_priv_map_entry_set_max_free(llist);
  ltree:=llist;
  llist:=y;
 end;
 rtree:=root^.right;
 while (rlist<>nil) do
 begin
  y:=rlist^.left;
  rlist^.left:=rtree;
  vm_priv_map_entry_set_max_free(rlist);
  rtree:=rlist;
  rlist:=y;
 end;

 root^.left:=ltree;
 root^.right:=rtree;
 vm_priv_map_entry_set_max_free(root);

 Result:=(root);
end;

procedure vm_priv_map_entry_link(
           map        :p_vm_priv_map;
           after_where:p_vm_priv_map_entry;
           entry      :p_vm_priv_map_entry);
var
 i:DWORD;
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
   vm_priv_map_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left:=after_where;
  after_where^.right:=nil;
  after_where^.adj_free:=entry^.start - after_where^.__end;
  vm_priv_map_entry_set_max_free(after_where);
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
 vm_priv_map_entry_set_max_free(entry);
 map^.root:=entry;
end;

procedure vm_priv_map_entry_unlink(
           map  :p_vm_priv_map;
           entry:p_vm_priv_map_entry);
var
 next,prev,root:p_vm_priv_map_entry;
 i:DWORD;
begin
 Assert(entry<>@map^.header);

 if (entry<>map^.root) then
 begin
  vm_priv_map_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_priv_map_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
  if (entry^.next=@map^.header) then
  begin
   i:=map^.max_offset;
  end else
  begin
   i:=entry^.next^.start;
  end;
  root^.adj_free:=i-root^.__end;
  vm_priv_map_entry_set_max_free(root);
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

procedure vm_priv_map_entry_resize_free(map:p_vm_priv_map;entry:p_vm_priv_map_entry);
begin
 if (entry<>map^.root) then
 begin
  map^.root:=vm_priv_map_entry_splay(entry^.start, map^.root);
 end;

 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;
 vm_priv_map_entry_set_max_free(entry);
end;

function vm_priv_map_lookup_entry(
           map    :p_vm_priv_map;
           address:DWORD;
           entry  :pp_vm_priv_map_entry):Boolean;
var
 cur:p_vm_priv_map_entry;
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

  cur:=vm_priv_map_entry_splay(address,cur);
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

procedure vm_priv_map_simplify_entry(map:p_vm_priv_map;entry:p_vm_priv_map_entry); forward;

function vm_priv_map_insert(map:p_vm_priv_map;start,__end:DWORD):Integer;
var
 new_entry  :p_vm_priv_map_entry;
 prev_entry :p_vm_priv_map_entry;
 temp_entry :p_vm_priv_map_entry;
begin

 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 if vm_priv_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 new_entry:=vm_priv_map_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 vm_priv_map_entry_link(map, prev_entry, new_entry);
 map^.size:=map^.size+(new_entry^.__end - new_entry^.start);

 vm_priv_map_simplify_entry(map, new_entry);

 Result:=(KERN_SUCCESS);
end;

function vm_priv_map_findspace(map:p_vm_priv_map;start,length:DWORD):t_vm_priv_space;
label
 _nxt;
var
 entry:p_vm_priv_map_entry;
begin

 if (start<map^.min_offset) then
 begin
  start:=map^.min_offset;
 end;
 if (start + length>map^.max_offset) or (start + length<start) then
 begin
  Exit(Default(t_vm_priv_space));
 end;

 if (map^.root=nil) then
 begin
  Result.addr:=start;
  Result.size:=map^.max_offset - start;
  Exit;
 end;

 map^.root:=vm_priv_map_entry_splay(start, map^.root);

 if ((start + length)<=map^.root^.start) then
 begin
  Result.addr:=start;
  Result.size:=map^.root^.start - start;
  Exit;
 end;

 if (start < map^.root^.__end) then
 begin
  start:=map^.root^.__end;
 end;

 if (length <= map^.root^.__end + map^.root^.adj_free - start) then
 begin
  Result.addr:=start;
  Result.size:=map^.root^.adj_free - (start - map^.root^.__end);
  Exit;
 end;

 entry:=map^.root^.right;

 if (entry=nil) then
 begin
  Exit(Default(t_vm_priv_space));
 end;

 if (length>entry^.max_free) then
 begin
  Exit(Default(t_vm_priv_space));
 end;

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
    Result.addr:=entry^.__end;
    Result.size:=entry^.adj_free;
    Exit;
   end else
   begin
    entry:=entry^.right;
   end;
  end;
 end;

 Assert(false,'vm_priv_map_findspace: max_free corrupt');
end;

procedure vm_priv_map_simplify_entry(map:p_vm_priv_map;entry:p_vm_priv_map_entry);
var
 next,prev:p_vm_priv_map_entry;
begin

 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  if (prev^.__end=entry^.start) and
     (prev^.refs =entry^.refs ) and
     (prev^.flags=entry^.flags) then
  begin
   vm_priv_map_entry_unlink(map, prev);
   entry^.start:=prev^.start;

   //change
   if (entry^.prev<>@map^.header) then
   begin
    vm_priv_map_entry_resize_free(map, entry^.prev);
   end;

   vm_priv_map_entry_dispose(map, prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  if (entry^.__end=next^.start) and
     (entry^.refs =next^.refs ) and
     (entry^.flags=next^.flags) then
  begin
   vm_priv_map_entry_unlink(map, next);
   entry^.__end:=next^.__end;

   //change
   vm_priv_map_entry_resize_free(map, entry);

   vm_priv_map_entry_dispose(map, next);
  end;
 end;

end;

procedure _vm_priv_map_clip_start(map:p_vm_priv_map;entry:p_vm_priv_map_entry;start:DWORD);
var
 new_entry:p_vm_priv_map_entry;
begin
 vm_priv_map_simplify_entry(map, entry);

 new_entry:=vm_priv_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.start:=start;

 vm_priv_map_entry_link(map, entry^.prev, new_entry);
end;

procedure vm_priv_map_clip_start(map:p_vm_priv_map;entry:p_vm_priv_map_entry;start:DWORD); inline;
begin
 if (start>entry^.start) then
 begin
  _vm_priv_map_clip_start(map,entry,start);
 end;
end;

procedure _vm_priv_map_clip_end(map:p_vm_priv_map;entry:p_vm_priv_map_entry;__end:DWORD);
var
 new_entry:p_vm_priv_map_entry;
begin
 new_entry:=vm_priv_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end:=__end;

 vm_priv_map_entry_link(map, entry, new_entry);
end;

procedure vm_priv_map_clip_end(map:p_vm_priv_map;entry:p_vm_priv_map_entry;__end:DWORD); inline;
begin
 if (__end<entry^.__end) then
 begin
  _vm_priv_map_clip_end(map,entry,__end);
 end;
end;

procedure vm_priv_map_entry_delete(map:p_vm_priv_map;entry:p_vm_priv_map_entry);
var
 size:DWORD;
begin
 vm_priv_map_entry_unlink(map, entry);
 size:=entry^.__end - entry^.start;
 map^.size:=map^.size-size;

 vm_priv_map_entry_dispose(map,entry);
end;

function vm_priv_map_delete(map:p_vm_priv_map;start,__end:DWORD):Integer;
var
 entry      :p_vm_priv_map_entry;
 first_entry:p_vm_priv_map_entry;
 next       :p_vm_priv_map_entry;
begin

 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (not vm_priv_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  vm_priv_map_clip_start(map, entry, start);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  vm_priv_map_clip_end(map, entry, __end);

  next:=entry^.next;

  vm_priv_map_entry_delete(map, entry);

  entry:=next;
 end;

 Result:=(KERN_SUCCESS);
end;

function vm_priv_map_foreach(map:p_vm_priv_map;start,__end:DWORD;data:Pointer;cb:t_vm_priv_cb):Integer;
var
 entry     :p_vm_priv_map_entry;
 temp_entry:p_vm_priv_map_entry;
begin

 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (vm_priv_map_lookup_entry(map, start, @temp_entry)) then
 begin
  entry:=temp_entry;
  vm_priv_map_clip_start(map, entry, start);
 end else
 begin
  entry:=temp_entry^.next;
 end;

 while ((entry<>@map^.header) and (entry^.start<__end)) do
 begin
  vm_priv_map_clip_end(map, entry, __end);

  cb(data,entry);

  vm_priv_map_simplify_entry(map, entry);
  entry:=entry^.next;
 end;

 Result:=(KERN_SUCCESS);
end;

function vm_priv_map_find_best(map:p_vm_priv_map;length:DWORD):t_vm_priv_fit;
var
 entry:p_vm_priv_map_entry;

 function apply(var space:t_vm_priv_fit;start,free,flag:DWORD):Boolean; inline;
 begin
  //
  if (flag=0) then
  begin
   if (space.max__free.size<free) then
   begin
    space.max__free.addr:=start;
    space.max__free.size:=free;
   end;
   //
   if (free>=length) then
   if (space.best_free.size=0) or
      (space.best_free.size>free) then
   begin
    space.best_free.addr:=start;
    space.best_free.size:=free;
   end;
   //
  end else
  begin
   if (space.max__invl.size<free) then
   begin
    space.max__invl.addr:=start;
    space.max__invl.size:=free;
   end;
   //
   if (free>=length) then
   if (space.best_invl.size=0) or
      (space.best_invl.size>free) then
   begin
    space.best_invl.addr:=start;
    space.best_invl.size:=free;
   end;
   //
  end;
  //
  Result:=(space.best_free.size=length);
 end;

begin

 if (map^.root=nil) then
 begin
  Result.max__free.addr:=map^.min_offset;
  Result.max__free.size:=map^.max_offset - map^.min_offset;
  Result.max__invl:=Default(t_vm_priv_space);
  Result.best_free:=Result.max__free;
  Result.best_invl:=Default(t_vm_priv_space);
  Exit;
 end;

 Result:=Default(t_vm_priv_fit);

 entry:=map^.header.next;

 apply(Result,map^.min_offset,entry^.start - map^.min_offset,0);

 while (entry<>@map^.header) do
 begin

  if (entry^.flags<>0) then
  begin
   apply(Result,entry^.start,entry^.__end - entry^.start,1);
  end;

  if apply(Result,entry^.__end,entry^.adj_free,0) then
  begin
   Break;
  end;

  entry:=entry^.next;
 end;

end;

///

procedure vm_priv_pool_init(pool:p_vm_priv_pool);
begin
 pool^.zone:=vm_priv_new_zone;
 mtx_init(pool^.lock,'vm_priv_pool');
 TAILQ_INIT(@pool^.list);
 pool^.size:=0;
 pool^.invm:=0;
end;

procedure on_free_priv (obj:p_vm_nt_file_obj); forward;
procedure on_mmmap_priv(obj:p_vm_nt_file_obj;start,offset,size:QWORD); forward;
procedure on_unmap_priv(obj:p_vm_nt_file_obj;start,offset,size:QWORD); forward;

type
 t_addr_cell=record
  node:p_vm_priv_fd;
  addr:DWORD;
  size:DWORD;
 end;

const
 MAX_PRIV_SIZE =128*1024*1024;
 frag_threshold=512*1024*1024;

function vm_priv_pool_alloc(pool:p_vm_priv_pool;length:DWORD;p_out:p_vm_priv_alloc):Integer;
label
 _new;
var
 frag:Boolean;

 node:p_vm_priv_fd;
 hfile:THandle;

 space:t_vm_priv_fit;

 max__free:t_addr_cell;
 max__invl:t_addr_cell;
 best_free:t_addr_cell;
 best_invl:t_addr_cell;

 function _insert(node:p_vm_priv_fd;addr,size,flag:DWORD):Integer;
 begin
  if (flag=0) then
  begin
   Result:=vm_priv_map_insert(@node^.map,addr,addr + size);
   Assert(Result=KERN_SUCCESS,'wtf');
  end;

  p_out^.obj  :=@node^.obj;
  p_out^.start:=addr;
  p_out^.size :=size;

  mtx_unlock(pool^.lock);
 end;

begin
 Assert(length<>0,'wtf');
 if (length>MAX_PRIV_SIZE) then length:=MAX_PRIV_SIZE;

 mtx_lock(pool^.lock);

 max__free:=Default(t_addr_cell);
 max__invl:=Default(t_addr_cell);
 best_free:=Default(t_addr_cell);
 best_invl:=Default(t_addr_cell);

 frag:=(pool^.size>=frag_threshold);

 //find best
 node:=TAILQ_FIRST(@pool^.list);
 while (node<>nil) do
 begin
  //
  if frag or ((MAX_PRIV_SIZE - node^.map.size + node^.inval)>=length) then
  begin
   //
   space:=vm_priv_map_find_best(@node^.map,length);

   if (max__free.size<space.max__free.size) then
   begin
    max__free.node:=node;
    max__free.addr:=space.max__free.addr;
    max__free.size:=space.max__free.size;
   end;
   //
   if (max__invl.size<space.max__invl.size) then
   begin
    max__invl.node:=node;
    max__invl.addr:=space.max__invl.addr;
    max__invl.size:=space.max__invl.size;
   end;
   //
   if (space.best_free.size<>0) then
   if (best_free.size=0) or
      (best_free.size>space.best_free.size) then
   begin
    best_free.node:=node;
    best_free.addr:=space.best_free.addr;
    best_free.size:=space.best_free.size;
   end;
   //
   if (space.best_invl.size<>0) then
   if (best_invl.size=0) or
      (best_invl.size>space.best_invl.size) then
   begin
    best_invl.node:=node;
    best_invl.addr:=space.best_invl.addr;
    best_invl.size:=space.best_invl.size;
   end;
   //
   if (best_free.size=length) then
   begin
    Exit(_insert(best_free.node,best_free.addr,length,0));
   end;
   //
  end;
  //
  node:=TAILQ_NEXT(node,@node^.elist);
 end;

 if (best_free.size<>0) then
 begin
  Exit(_insert(best_free.node,best_free.addr,length,0));
 end;

 if (max__free.size<>0) and frag then
 begin
  //fragmentation
  Exit(_insert(max__free.node,max__free.addr,max__free.size,0));
 end;

 if (best_invl.size<>0) then
 begin
  Exit(_insert(best_invl.node,best_invl.addr,length,1));
 end;

 if (max__invl.size<>0) and frag then
 begin
  //fragmentation
  Exit(_insert(max__invl.node,max__invl.addr,max__invl.size,1));
 end;

 _new:

  hfile:=0;
  Result:=md_memfd_create(hfile,MAX_PRIV_SIZE,VM_RW);
  if (Result<>0) then
  begin
   mtx_unlock(pool^.lock);
   Exit;
  end;

  node:=AllocMem(sizeof(t_vm_priv_fd));

  node^.pool:=pool;

  node^.obj.hfile:=hfile;
  node^.obj.free :=@on_free_priv;
  node^.obj.mmmap:=@on_mmmap_priv;
  node^.obj.unmap:=@on_unmap_priv;
  node^.obj.flags:=0;
  node^.obj.maxp :=VM_RW;
  vm_priv_map_init(@node^.map,pool^.zone,0,MAX_PRIV_SIZE);

  //insert list
  TAILQ_INSERT_TAIL(@pool^.list,node,@node^.elist);
  pool^.size:=pool^.size+MAX_PRIV_SIZE;

  Exit(_insert(node,0,length,0));
end;

//rdi, rsi
procedure ZeroPages(addr:Pointer;size:Ptruint); assembler nostackframe SysV_ABI_CDecl;
label
 _exit,
 _rep;
asm
 shr $7, %rsi // div 128
 jz _exit

  vpxor %ymm0, %ymm0, %ymm0 //zero

  _rep:

   vmovntdq %ymm0,   (%rdi)
   vmovntdq %ymm0, 32(%rdi)
   vmovntdq %ymm0, 64(%rdi)
   vmovntdq %ymm0, 96(%rdi)

   lea 128(%rdi),%rdi
   dec %rsi

  jnz _rep

  sfence
 _exit:
end;

type
 p_ctx_inv=^t_ctx_inv;
 t_ctx_inv=record
  node:p_vm_priv_fd;
  pool:p_vm_priv_pool;
  base:QWORD;
 end;

procedure on_activate(data:Pointer;entry:p_vm_priv_map_entry);
var
 pool:p_vm_priv_pool;
 node:p_vm_priv_fd;
 base:Pointer;
 size:DWORD;
 r:Integer;
begin

 if (entry^.flags<>0) then
 begin
  pool:=p_ctx_inv(data)^.pool;
  node:=p_ctx_inv(data)^.node;
  base:=Pointer(p_ctx_inv(data)^.base);
  size:=(entry^.__end - entry^.start);
  //

  base:=base + entry^.start;
  r:=md_willneed(base,size);
  if (r=0) then
  begin
   //fill the pages with zeros if the recovery is successful, meaning that no content was lost
   ZeroPages(base,size);
  end;

  //
  entry^.flags:=0;
  node^.inval:=node^.inval - size;
  pool^.invm :=pool^.invm  - size;
 end;

 Inc(entry^.refs);
end;

procedure on_invalidate(data:Pointer;entry:p_vm_priv_map_entry);
var
 pool:p_vm_priv_pool;
 node:p_vm_priv_fd;
 base:Pointer;
 size:DWORD;
begin

 if (entry^.refs=0) then
 begin
  Assert(False,'entry^.refs=0');
 end;

 if (entry^.refs<>0) then
 begin
  Dec(entry^.refs);

  if (entry^.refs=0) then
  begin
   pool:=p_ctx_inv(data)^.pool;
   node:=p_ctx_inv(data)^.node;
   base:=Pointer(p_ctx_inv(data)^.base);
   size:=(entry^.__end - entry^.start);
   //

   base:=base + entry^.start;

   //Don't check for errors here because parts of the page may already be in the desired state
   md_dontneed(base,size);

   entry^.flags:=1;
   node^.inval:=node^.inval + size;
   pool^.invm :=pool^.invm  + size;
  end;
 end;

end;

procedure on_mmmap_priv(obj:p_vm_nt_file_obj;start,offset,size:QWORD);
var
 ctx:t_ctx_inv;
begin
 if (size=0) then Exit;

 ctx.node:=POINTER(PTRUINT(obj)-PTRUINT(@p_vm_priv_fd(nil)^.obj));
 ctx.pool:=ctx.node^.pool;
 ctx.base:=QWORD(start) - offset;

 mtx_lock(ctx.pool^.lock);

  vm_priv_map_foreach(@ctx.node^.map,offset,offset + size,@ctx,@on_activate);

 mtx_unlock(ctx.pool^.lock);
end;

procedure on_unmap_priv(obj:p_vm_nt_file_obj;start,offset,size:QWORD);
var
 ctx:t_ctx_inv;
begin
 if (size=0) then Exit;

 ctx.node:=POINTER(PTRUINT(obj)-PTRUINT(@p_vm_priv_fd(nil)^.obj));
 ctx.pool:=ctx.node^.pool;
 ctx.base:=QWORD(start) - offset;

 mtx_lock(ctx.pool^.lock);

   vm_priv_map_foreach(@ctx.node^.map,offset,offset + size,@ctx,@on_invalidate);

 mtx_unlock(ctx.pool^.lock);
end;

procedure on_free_priv(obj:p_vm_nt_file_obj);
var
 node:p_vm_priv_fd;
 pool:p_vm_priv_pool;
 r:Integer;

 function is_last_free:Boolean; inline;
 var
  node:p_vm_priv_fd;
 begin
  Result:=False;
  node:=TAILQ_LAST(@pool^.list);
  if (node<>nil) then
  begin
   Result:=(node^.map.size=node^.inval);
  end;
 end;

begin
 node:=POINTER(PTRUINT(obj)-PTRUINT(@p_vm_priv_fd(nil)^.obj));
 pool:=node^.pool;

 if (node^.map.size=node^.inval) then
 begin
  mtx_lock(pool^.lock);

  if (node^.map.size=node^.inval) then
  begin

   if is_last_free then
   begin
    //free block
    TAILQ_REMOVE(@pool^.list,node,@node^.elist);
    pool^.size:=pool^.size-MAX_PRIV_SIZE;
    pool^.invm:=pool^.invm-node^.inval;

    vm_priv_map_delete(@node^.map,0,MAX_PRIV_SIZE);

    r:=md_memfd_close(node^.obj.hfile);
    if (r<>0) then
    begin
     Writeln('failed md_memfd_close(',node^.obj.hfile,'):0x',HexStr(r,8));
     Assert(false,'on_free_priv');
    end;

    FreeMem(node);

   end else
   begin
    //reinsert block
    TAILQ_REMOVE     (@pool^.list,node,@node^.elist);
    TAILQ_INSERT_TAIL(@pool^.list,node,@node^.elist);
   end;

  end;

  mtx_unlock(pool^.lock);
 end;

end;

end.



