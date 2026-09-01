unit tmpfs_seg;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_malloc,
 errno,
 vmparam,
 vm_internal_object,
 tmpfs;

const
 TMPFS_ALLOC_GRANULARITY=64*1024;      // MD_ALLOC_GRANULARITY
 TMPFS_MAX_SEG_SIZE     =16*1024*1024; // upper bound for one section

type
 pp_tmpfs_seg=^p_tmpfs_seg;
 p_tmpfs_seg=^t_tmpfs_seg;
 t_tmpfs_seg=packed record
  prev :p_tmpfs_seg;
  next :p_tmpfs_seg;
  left :p_tmpfs_seg;
  right:p_tmpfs_seg;
  start:QWORD;
  __end:QWORD;
  obj  :p_vm_int_obj;
  buf  :Pointer;
 end;

 p_tmpfs_seg_map=^t_tmpfs_seg_map;
 t_tmpfs_seg_map=packed object
  header   :t_tmpfs_seg;
  root     :p_tmpfs_seg;
  size     :QWORD;
  property  min_offset:QWORD read header.start write header.start;
  property  max_offset:QWORD read header.__end write header.__end;
 end;

function  tmpfs_seg_map_create (min,max:QWORD):p_tmpfs_seg_map;
procedure tmpfs_seg_map_destroy(map:p_tmpfs_seg_map);

function tmpfs_seg_lookup(
           map    :p_tmpfs_seg_map;
           address:QWORD;
           entry  :pp_tmpfs_seg):Boolean;

function tmpfs_seg_map_shrink(tmp:p_tmpfs_mount;map:p_tmpfs_seg_map;size:QWORD):Integer;

function tmpfs_seg_get_next_space(map:p_tmpfs_seg_map;entry:p_tmpfs_seg;offset:QWORD):QWORD;

function tmpfs_seg_map_fetch(tmp:p_tmpfs_mount;map:p_tmpfs_seg_map;start,__end:QWORD;entry:pp_tmpfs_seg):Integer;

implementation

uses
 md_map;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

procedure _tmpfs_seg_map_init(map:p_tmpfs_seg_map;min,max:QWORD);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.size      :=0;
end;

function tmpfs_seg_map_create(min,max:QWORD):p_tmpfs_seg_map;
begin
 Result:=calloc(SizeOf(t_tmpfs_seg_map));
 _tmpfs_seg_map_init(Result,min,max);
end;

procedure tmpfs_seg_map_destroy(map:p_tmpfs_seg_map);
begin
 free(map);
end;

function tmpfs_seg_create(map:p_tmpfs_seg_map):p_tmpfs_seg;
var
 new_entry:p_tmpfs_seg;
begin
 new_entry:=calloc(sizeof(t_tmpfs_seg_map));

 //new_entry:=uma_zalloc(mapentzone, M_WAITOK or M_ZERO);
 Assert((new_entry<>nil),'tmpfs_seg_create: kernel resources exhausted');
 Result:=new_entry;
end;

procedure tmpfs_seg_deallocate(map:p_tmpfs_seg_map;entry:p_tmpfs_seg);
var
 r:Integer;
begin
 if (entry^.obj<>nil) then
 begin
  vm_int_obj_deallocate(entry^.obj);
 end;

 if (entry^.buf<>nil) then
 begin
  r:=md_unmap(entry^.buf,(entry^.__end-entry^.start));
  if (r<>0) then
  begin
   LOG_CRITICAL(StdErr,'failed tmpfs_seg_deallocate(',HexStr(entry^.buf),'):0x',HexStr(r,8));
   Assert(false,'tmpfs_seg_deallocate');
  end;
 end;

 free(entry);

 //uma_zfree(mapentzone, entry);
end;

function tmpfs_seg_splay(addr:QWORD;root:p_tmpfs_seg):p_tmpfs_seg;
var
 llist,rlist:p_tmpfs_seg;
 ltree,rtree:p_tmpfs_seg;
 y          :p_tmpfs_seg;
begin
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

procedure tmpfs_seg_link(
           map        :p_tmpfs_seg_map;
           after_where:p_tmpfs_seg;
           entry      :p_tmpfs_seg);
begin
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   tmpfs_seg_splay(after_where^.start, map^.root);
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

procedure tmpfs_seg_unlink(
           map  :p_tmpfs_seg_map;
           entry:p_tmpfs_seg);
var
 next,prev,root:p_tmpfs_seg;
begin
 Assert(entry<>@map^.header);

 if (entry<>map^.root) then
 begin
  tmpfs_seg_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=tmpfs_seg_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
 end;

 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
end;

function tmpfs_seg_lookup(
           map    :p_tmpfs_seg_map;
           address:QWORD;
           entry  :pp_tmpfs_seg):Boolean;
var
 cur:p_tmpfs_seg;
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
  cur:=tmpfs_seg_splay(address,cur);
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

procedure tmpfs_int_obj_free(obj:p_vm_int_obj);
var
 r:Integer;
begin
 if (obj^.hfile<>0) then
 begin
  r:=md_memfd_close(obj^.hfile);
  if (r<>0) then
  begin
   LOG_CRITICAL(StdErr,'failed tmpfs_int_obj_free(',obj^.hfile,'):0x',HexStr(r,8));
   Assert(false,'tmpfs_int_obj_free');
  end;
  obj^.hfile:=0;
 end;
end;

const
 tmpfs_int_obj_vtable:vm_int_obj_vtable=(
  free:@tmpfs_int_obj_free;
 );

function tmpfs_seg_map_insert(map:p_tmpfs_seg_map;start,__end:QWORD;entry:pp_tmpfs_seg):Integer;
var
 new_entry :p_tmpfs_seg;
 prev_entry:p_tmpfs_seg;
 temp_entry:p_tmpfs_seg;
 //
 size:QWORD;
 base:Pointer;
 md  :THandle;
 obj :p_vm_int_obj;
 r   :Integer;
begin
 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(EINVAL);
 end;

 if tmpfs_seg_lookup(map,start,@temp_entry) then
 begin
  Exit(ENOSPC);
 end;

 prev_entry:=temp_entry;

 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(ENOSPC);
 end;

 size:=(__end-start);

 md:=0;
 r:=md_memfd_create(md, size, VM_RW);
 if (r<>0) then Exit(ENOSPC);

 base:=nil;
 r:=md_mmap(base, size, VM_RW, md, 0);
 if (r<>0) then
 begin
  md_memfd_close(md);
  Exit(ENOMEM);
 end;

 obj:=vm_int_obj_allocate(@tmpfs_int_obj_vtable,md,VM_RW);
 if (obj=nil) then
 begin
  md_unmap(base, size);
  md_memfd_close(md);
  Exit(ENOMEM);
 end;

 new_entry:=tmpfs_seg_create(map);

 new_entry^.start:=start;
 new_entry^.__end:=__end;
 new_entry^.obj  :=obj;
 new_entry^.buf  :=base;

 tmpfs_seg_link(map, prev_entry, new_entry);
 map^.size:=map^.size + size;

 entry^:=new_entry;
 Result:=0;
end;

procedure tmpfs_seg_delete(map:p_tmpfs_seg_map;entry:p_tmpfs_seg);
begin
 tmpfs_seg_unlink(map, entry);
 tmpfs_seg_deallocate(map,entry);
end;

function tmpfs_seg_map_shrink(tmp:p_tmpfs_mount;map:p_tmpfs_seg_map;size:QWORD):Integer;
var
 freed:QWORD;
 entry:p_tmpfs_seg;
 next :p_tmpfs_seg;
begin
 freed:=0;

 if (not tmpfs_seg_lookup(map, size, @entry)) then
 begin
  entry:=entry^.next;
 end else
 if (size>entry^.start) then
 begin
  FillChar((entry^.buf + (size - entry^.start))^, entry^.__end - size, 0);
  entry:=entry^.next;
 end;

 while (entry<>@map^.header) do
 begin
  next:=entry^.next;

  size:=(entry^.__end - entry^.start);

  freed:=freed + size;

  map^.size:=map^.size - size;
  tmpfs_seg_delete(map, entry);

  entry:=next;
 end;

 if (freed<>0) then
 begin
  TMPFS_LOCK(tmp);
  tmp^.tm_pages_used:=tmp^.tm_pages_used - (freed div PAGE_SIZE);
  TMPFS_UNLOCK(tmp);
 end;

 Result:=0;
end;

function tmpfs_seg_get_next_space(map:p_tmpfs_seg_map;entry:p_tmpfs_seg;offset:QWORD):QWORD;
var
 next:p_tmpfs_seg;
 pos:QWORD;
begin
 Result:=0;
 next:=entry^.next;

 if (entry=@map^.header) then
 begin
  pos:=map^.max_offset;
 end else
 begin
  pos:=next^.start;
 end;

 if (pos>offset) then
 begin
  Result:=pos - offset;
 end;
end;

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function tmpfs_seg_map_fetch(tmp:p_tmpfs_mount;map:p_tmpfs_seg_map;start,__end:QWORD;entry:pp_tmpfs_seg):Integer;
var
 size:QWORD;
 curr:p_tmpfs_seg;
begin
 Result:=0;
 start:=start and (not QWORD(TMPFS_ALLOC_GRANULARITY-1));

 if (not tmpfs_seg_lookup(map, start, @curr)) then
 begin
  __end:=(__end + (TMPFS_ALLOC_GRANULARITY-1)) and (not QWORD(TMPFS_ALLOC_GRANULARITY-1));

  size:=Min(__end-start, TMPFS_MAX_SEG_SIZE);
  size:=Min(size, tmpfs_seg_get_next_space(map, curr, __end));

  if (tmpfs_pages_check_avail(tmp, size div PAGE_SIZE)=0) then
  begin
   Exit(ENOSPC);
  end;

  Result:=tmpfs_seg_map_insert(map, start, start + size, entry);

  if (Result=0) then
  if (size<>0) then
  begin
   TMPFS_LOCK(tmp);
   tmp^.tm_pages_used:=tmp^.tm_pages_used + (size div PAGE_SIZE);
   TMPFS_UNLOCK(tmp);
  end;

 end else
 begin
  entry^:=curr;
  Result:=0;
 end;

end;


end.

