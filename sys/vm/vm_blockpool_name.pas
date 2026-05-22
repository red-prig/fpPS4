unit vm_blockpool_name;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 vm;

type
 pp_vm_blockpool_name_entry=^p_vm_blockpool_name_entry;
 p_vm_blockpool_name_entry=^t_vm_blockpool_name_entry;
 t_vm_blockpool_name_entry=packed record
  prev :p_vm_blockpool_name_entry;
  next :p_vm_blockpool_name_entry;
  left :p_vm_blockpool_name_entry;
  right:p_vm_blockpool_name_entry;
  start:vm_offset_t;
  __end:vm_offset_t;
  name :t_entry_name;
 end;

 p_vm_blockpool_name_map=^t_vm_blockpool_name_map;
 t_vm_blockpool_name_map=object
  header:t_vm_blockpool_name_entry;
  root  :p_vm_blockpool_name_entry;
  property  min_offset:vm_offset_t read header.start write header.start;
  property  max_offset:vm_offset_t read header.__end write header.__end;
 end;

procedure vm_blockpool_name_map_init    (map:p_vm_blockpool_name_map;min,max:vm_offset_t);
procedure vm_blockpool_name_map_set_name(map:p_vm_blockpool_name_map;start,__end:vm_offset_t;name:PChar);

function  vm_blockpool_name_map_lookup_entry(
            map    :p_vm_blockpool_name_map;
            address:vm_offset_t;
            entry  :pp_vm_blockpool_name_entry):Boolean;

implementation

uses
 uma;

var
 mapentzone:uma_zone_t; external; //hack

function vm_blockpool_name_map_entry_create(map:p_vm_blockpool_name_map):p_vm_blockpool_name_entry;
var
 new_entry:p_vm_blockpool_name_entry;
begin
 new_entry:=uma_zalloc(mapentzone, M_WAITOK or M_ZERO);
 Assert((new_entry<>nil),'vm_map_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

procedure vm_blockpool_name_map_entry_dispose(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry); inline;
begin
 uma_zfree(mapentzone, entry);
end;

procedure VM_MAP_RANGE_CHECK(map:p_vm_blockpool_name_map;var start,__end:vm_offset_t); inline;
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

function vm_blockpool_name_map_insert(
           map  :p_vm_blockpool_name_map;
           start,__end:vm_offset_t):Integer;  forward;

procedure vm_blockpool_name_map_init(map:p_vm_blockpool_name_map;min,max:vm_offset_t);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.root:=nil;
 vm_blockpool_name_map_insert(map,min,max);
end;

function vm_blockpool_name_map_entry_splay(addr:vm_offset_t;root:p_vm_blockpool_name_entry):p_vm_blockpool_name_entry;
var
 llist,rlist:p_vm_blockpool_name_entry;
 ltree,rtree:p_vm_blockpool_name_entry;
 y          :p_vm_blockpool_name_entry;
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

procedure vm_blockpool_name_map_entry_link(
           map        :p_vm_blockpool_name_map;
           after_where:p_vm_blockpool_name_entry;
           entry      :p_vm_blockpool_name_entry);
begin

 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   vm_blockpool_name_map_entry_splay(after_where^.start, map^.root);
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

procedure vm_blockpool_name_map_entry_unlink(
           map  :p_vm_blockpool_name_map;
           entry:p_vm_blockpool_name_entry);
var
 next,prev,root:p_vm_blockpool_name_entry;
begin
 Assert(entry<>@map^.header);

 if (entry<>map^.root) then
 begin
  vm_blockpool_name_map_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_blockpool_name_map_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
end;

function vm_blockpool_name_map_lookup_entry(
           map    :p_vm_blockpool_name_map;
           address:vm_offset_t;
           entry  :pp_vm_blockpool_name_entry):Boolean;
var
 cur:p_vm_blockpool_name_entry;
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
  cur:=vm_blockpool_name_map_entry_splay(address,cur);
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

procedure vm_blockpool_name_map_simplify_entry(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry); forward;

function vm_blockpool_name_map_insert(
           map  :p_vm_blockpool_name_map;
           start,__end:vm_offset_t):Integer;
var
 new_entry :p_vm_blockpool_name_entry;
 prev_entry:p_vm_blockpool_name_entry;
begin

 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 if vm_blockpool_name_map_lookup_entry(map,start,@prev_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 new_entry:=vm_blockpool_name_map_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 vm_blockpool_name_map_entry_link(map, prev_entry, new_entry);

 vm_blockpool_name_map_simplify_entry(map, new_entry);
end;

procedure vm_blockpool_name_map_simplify_entry(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry);
var
 next,prev:p_vm_blockpool_name_entry;
begin
 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin

  if (prev^.__end=entry^.start) then
  if (strlcomp(pchar(@prev^.name),pchar(@entry^.name),sizeof(t_entry_name))=0) then
  begin
   vm_blockpool_name_map_entry_unlink(map, prev);
   entry^.start :=prev^.start;

   vm_blockpool_name_map_entry_dispose(map, prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin

  if (entry^.__end=next^.start) then
  begin
   if (strlcomp(pchar(@next^.name),pchar(@entry^.name),sizeof(t_entry_name))=0) then
   begin
    vm_blockpool_name_map_entry_unlink(map, next);
    entry^.__end:=next^.__end;

    vm_blockpool_name_map_entry_dispose(map, next);
   end;
  end;
 end;
end;

procedure _vm_blockpool_name_map_clip_start(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry;start:vm_offset_t);
var
 new_entry:p_vm_blockpool_name_entry;
begin
 vm_blockpool_name_map_simplify_entry(map, entry);

 new_entry:=vm_blockpool_name_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.start:=start;

 vm_blockpool_name_map_entry_link(map, entry^.prev, new_entry);
end;

procedure vm_blockpool_name_map_clip_start(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry;start:vm_offset_t); inline;
begin
 if (start>entry^.start) then
 begin
  _vm_blockpool_name_map_clip_start(map,entry,start);
 end;
end;

procedure _vm_blockpool_name_map_clip_end(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry;__end:vm_offset_t);
var
 new_entry:p_vm_blockpool_name_entry;
begin
 new_entry:=vm_blockpool_name_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end:=__end;

 vm_blockpool_name_map_entry_link(map, entry, new_entry);
end;

procedure vm_blockpool_name_map_clip_end(map:p_vm_blockpool_name_map;entry:p_vm_blockpool_name_entry;__end:vm_offset_t); inline;
begin
 if (__end<entry^.__end) then
 begin
  _vm_blockpool_name_map_clip_end(map,entry,__end);
 end;
end;

function vm_blockpool_name_map_delete(map:p_vm_blockpool_name_map;start,__end:vm_offset_t):Integer;
var
 entry      :p_vm_blockpool_name_entry;
 first_entry:p_vm_blockpool_name_entry;
 next       :p_vm_blockpool_name_entry;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (not vm_blockpool_name_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  vm_blockpool_name_map_clip_start(map, entry, start);
 end;

 {
  * Step through all entries in this region
  }
 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  vm_blockpool_name_map_clip_end(map, entry, __end);

  next:=entry^.next;

  vm_blockpool_name_map_entry_dispose(map, entry);

  entry:=next;
 end;
 Result:=(KERN_SUCCESS);
end;

procedure vm_blockpool_name_map_set_name(map:p_vm_blockpool_name_map;start,__end:vm_offset_t;name:PChar);
var
 current:p_vm_blockpool_name_entry;
 origin :p_vm_blockpool_name_entry;
 next   :p_vm_blockpool_name_entry;
begin
 if (start=__end) then
 begin
  Exit();
 end;

 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_blockpool_name_map_lookup_entry(map, start, @origin)) then
 begin
  vm_blockpool_name_map_clip_start(map, origin, start);
 end else
 begin
  origin:=origin^.next;
 end;

 current:=origin;
 while ((current<>@map^.header) and (current^.start<__end)) do
 begin
  vm_blockpool_name_map_clip_end(map,current,__end);

  current^.name:=Default(t_entry_name);

  if (name<>nil) then
  begin
   MoveChar0(name^,current^.name,sizeof(t_entry_name));
  end;

  next:=current^.next;

  vm_blockpool_name_map_simplify_entry(map, current);

  current:=next;
 end;
end;



end.

