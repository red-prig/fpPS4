unit vm_nt_sub_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 uma,
 vm,
 vm_pmap_prot;

type
 pp_vm_nt_sub_entry=^p_vm_nt_sub_entry;
 p_vm_nt_sub_entry=^vm_nt_sub_entry;
 vm_nt_sub_entry=packed record
  prev :p_vm_nt_sub_entry; // previous entry
  next :p_vm_nt_sub_entry; // next entry
  left :p_vm_nt_sub_entry; // left child in binary search tree
  right:p_vm_nt_sub_entry; // right child in binary search tree
  start:vm_offset_t;       // start address
  __end:vm_offset_t;       // end address
  //
  decl_prot:Byte;          // declared protection (with tracking bits)
  real_prot:Byte;          // real page protection
  align    :Word;
 end;

 p_vm_nt_sub_map=^t_vm_nt_sub_map;
 t_vm_nt_sub_map=object
  header:vm_nt_sub_entry;   // List of entries
  root  :p_vm_nt_sub_entry; // Root of a binary search tree
  property  min_offset:vm_offset_t read header.start write header.start;
  property  max_offset:vm_offset_t read header.__end write header.__end;
 end;

procedure vm_nt_sub_map_init(map:p_vm_nt_sub_map;min,max:vm_offset_t);

function  vm_nt_sub_map_insert(
            map  :p_vm_nt_sub_map;
            start:vm_offset_t;
            __end:vm_offset_t;
            prot :Byte):Integer;

// Move nodes from one submap to another
procedure vm_nt_sub_map_move(dst,src:p_vm_nt_sub_map);
procedure vm_nt_sub_map_clip(dst,src:p_vm_nt_sub_map);

procedure vm_nt_sub_map_free(map:p_vm_nt_sub_map);

//Change protection taking into account tracking
function  vm_nt_sub_map_protect(map  :p_vm_nt_sub_map;
                                start:vm_offset_t;
                                __end:vm_offset_t;
                                prot :Byte):Integer;

//Change protection tracking
function vm_nt_sub_map_tracking(map  :p_vm_nt_sub_map;
                                start:vm_offset_t;
                                __end:vm_offset_t;
                                prot :Byte):Integer;

//Update page protection by mode
procedure vm_nt_sub_map_prot_fixup(map  :p_vm_nt_sub_map;
                                   start:vm_offset_t;
                                   __end:vm_offset_t;
                                   mode :Integer);

var
 vm_nt_sub_entry_zone:uma_zone_t=nil;

implementation

uses
 md_map;

procedure vm_nt_sub_map_init(map:p_vm_nt_sub_map;min,max:vm_offset_t); inline;
begin
 map^:=Default(t_vm_nt_sub_map);
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset :=min;
 map^.max_offset :=max;
end;

function vm_nt_sub_entry_create(map:p_vm_nt_sub_map):p_vm_nt_sub_entry;
var
 new_entry:p_vm_nt_sub_entry;
begin
 new_entry:=uma_zalloc(vm_nt_sub_entry_zone, M_WAITOK or M_ZERO);
 Assert((new_entry<>nil),'vm_nt_sub_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

procedure vm_nt_sub_entry_dispose(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry); inline;
begin
 uma_zfree(vm_nt_sub_entry_zone, entry);
end;

function vm_nt_sub_entry_splay(addr:vm_offset_t;root:p_vm_nt_sub_entry):p_vm_nt_sub_entry;
var
 llist,rlist:p_vm_nt_sub_entry;
 ltree,rtree:p_vm_nt_sub_entry;
 y          :p_vm_nt_sub_entry;
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

 root^.left :=ltree;
 root^.right:=rtree;

 Result:=(root);
end;

procedure vm_nt_sub_entry_link(
           map        :p_vm_nt_sub_map;
           after_where:p_vm_nt_sub_entry;
           entry      :p_vm_nt_sub_entry);
begin
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   vm_nt_sub_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left :=after_where;
  after_where^.right:=nil;
 end else
 begin
  entry^.right:=map^.root;
  entry^.left :=nil;
 end;

 map^.root:=entry;
end;

procedure vm_nt_sub_entry_unlink(
           map  :p_vm_nt_sub_map;
           entry:p_vm_nt_sub_entry);
var
 next,prev,root:p_vm_nt_sub_entry;
begin
 if (entry<>map^.root) then
 begin
  vm_nt_sub_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_nt_sub_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
 end;

 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
end;

function vm_nt_sub_map_lookup_entry(
           map    :p_vm_nt_sub_map;
           address:vm_offset_t;
           entry  :pp_vm_nt_sub_entry):Boolean;
var
 cur:p_vm_nt_sub_entry;
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
  cur:=vm_nt_sub_entry_splay(address,cur);
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

procedure vm_nt_sub_simplify_entry(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry); forward;

function  vm_nt_sub_map_insert(
            map  :p_vm_nt_sub_map;
            start:vm_offset_t;
            __end:vm_offset_t;
            prot :Byte):Integer;
var
 new_entry  :p_vm_nt_sub_entry;
 prev_entry :p_vm_nt_sub_entry;
 temp_entry :p_vm_nt_sub_entry;

begin
 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 if vm_nt_sub_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 if (prev_entry<>@map^.header) and
    (prev_entry^.decl_prot=prot) and
    (prev_entry^.real_prot=0) and
    (prev_entry^.__end=start) then
 begin
  prev_entry^.__end:=__end;

  vm_nt_sub_simplify_entry(map, prev_entry);

  Exit(KERN_SUCCESS);
 end;

 new_entry:=vm_nt_sub_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 new_entry^.decl_prot:=prot;
 new_entry^.real_prot:=0;   //do fixup

 vm_nt_sub_entry_link(map, prev_entry, new_entry);

 vm_nt_sub_simplify_entry(map, new_entry);

 Result:=KERN_SUCCESS;
end;

procedure vm_nt_sub_map_entry_delete(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry); inline;
begin
 vm_nt_sub_entry_unlink (map, entry);
 vm_nt_sub_entry_dispose(map, entry);
end;

procedure vm_nt_sub_simplify_entry(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry);
var
 next,prev:p_vm_nt_sub_entry;
begin
 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  if (prev^.__end=entry^.start) and
     (prev^.decl_prot=entry^.decl_prot) and
     (prev^.real_prot=entry^.real_prot) then
  begin
   vm_nt_sub_entry_unlink(map, prev);
   entry^.start:=prev^.start;

   vm_nt_sub_entry_dispose(map, prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  if (entry^.__end=next^.start) and
     (next^.decl_prot=entry^.decl_prot) and
     (next^.real_prot=entry^.real_prot) then
  begin
   vm_nt_sub_entry_unlink(map, next);
   entry^.__end:=next^.__end;

   vm_nt_sub_entry_dispose(map, next);
  end;
 end;
end;

procedure _vm_nt_sub_map_clip_start(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry;start:vm_offset_t);
var
 new_entry:p_vm_nt_sub_entry;
begin
 Assert(start<entry^.__end);

 vm_nt_sub_simplify_entry(map, entry);

 new_entry:=vm_nt_sub_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.start    :=start;

 vm_nt_sub_entry_link(map, entry^.prev, new_entry);
end;

procedure vm_nt_sub_map_clip_start(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry;start:vm_offset_t); inline;
begin
 if (start>entry^.start) then
 begin
  _vm_nt_sub_map_clip_start(map,entry,start);
 end;
end;

procedure _vm_nt_sub_map_clip_end(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry;__end:vm_offset_t);
var
 new_entry:p_vm_nt_sub_entry;
begin
 Assert(__end>entry^.start);

 new_entry:=vm_nt_sub_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end    :=__end;

 vm_nt_sub_entry_link(map, entry, new_entry);
end;

procedure vm_nt_sub_map_clip_end(map:p_vm_nt_sub_map;entry:p_vm_nt_sub_entry;__end:vm_offset_t); inline;
begin
 if (__end<entry^.__end) then
 begin
  _vm_nt_sub_map_clip_end(map,entry,__end);
 end;
end;

procedure vm_nt_sub_map_move(dst,src:p_vm_nt_sub_map);
var
 entry,next,after:p_vm_nt_sub_entry;
begin
 {
 Writeln('[vm_nt_sub_map_move]');
 Writeln(' dst:',HexStr(dst^.min_offset,16),':',HexStr(dst^.max_offset,16));
 Writeln(' src:',HexStr(src^.min_offset,16),':',HexStr(src^.max_offset,16));
 }

 entry:=src^.header.next;
 //
 after:=nil;
 //
 while (entry<>@src^.header) do
 begin
  //
  if (after=nil) then
  begin
   vm_nt_sub_map_lookup_entry(dst, entry^.__end, @after); //get first position of insert
  end;
  //
  next:=entry^.next;
  //
  vm_nt_sub_entry_unlink  (src,entry);
  vm_nt_sub_entry_link    (dst,after,entry);
  vm_nt_sub_simplify_entry(dst,entry);
  //
  after:=entry;
  //
  entry:=next;
 end;
end;

procedure vm_nt_sub_map_clip(dst,src:p_vm_nt_sub_map);
var
 entry,next,after:p_vm_nt_sub_entry;
begin
 Assert((dst^.min_offset=src^.min_offset) or (dst^.max_offset=src^.max_offset),'vm_nt_sub_map_clip');

 {
 Writeln('[vm_nt_sub_map_clip]');
 Writeln(' dst:',HexStr(dst^.min_offset,16),':',HexStr(dst^.max_offset,16));
 Writeln(' src:',HexStr(src^.min_offset,16),':',HexStr(src^.max_offset,16));
 }

 entry:=nil;
 if (vm_nt_sub_map_lookup_entry(src, dst^.min_offset, @entry)) then
 begin
  vm_nt_sub_map_clip_start(src, entry, dst^.min_offset); //devide src by dst
 end else
 begin
  entry:=entry^.next;
 end;
 //
 after:=nil;
 //
 while (entry<>@src^.header) and (entry^.start<dst^.max_offset) do
 begin
  //
  vm_nt_sub_map_clip_end(src, entry, dst^.max_offset); //devide src by dst
  //
  if (after=nil) then
  begin
   vm_nt_sub_map_lookup_entry(dst, entry^.__end, @after); //get first position of insert
  end;
  //
  next:=entry^.next;
  //
  vm_nt_sub_entry_unlink  (src,entry);
  vm_nt_sub_entry_link    (dst,after,entry);
  vm_nt_sub_simplify_entry(dst,entry);
  //
  after:=entry;
  //
  entry:=next;
 end;
end;

procedure vm_nt_sub_map_free(map:p_vm_nt_sub_map);
var
 entry,next:p_vm_nt_sub_entry;
begin
 entry:=map^.header.next;
 //
 while (entry<>@map^.header) do
 begin
  next:=entry^.next;
  //
  vm_nt_sub_map_entry_delete(map,entry);
  //
  entry:=next;
 end;
end;

//

function vm_nt_sub_map_protect(map  :p_vm_nt_sub_map;
                               start:vm_offset_t;
                               __end:vm_offset_t;
                               prot :Byte):Integer;
var
 entry:p_vm_nt_sub_entry;

 base,size:vm_size_t;
 mask:Integer;
 r:Integer;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (vm_nt_sub_map_lookup_entry(map, start, @entry)) then
 begin
  vm_nt_sub_map_clip_start(map, entry, start);
 end else
 begin
  entry:=entry^.next;
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  vm_nt_sub_map_clip_end(map, entry, __end);

  //save trac, change prot
  prot:=(prot and VM_RW) or (entry^.decl_prot and PAGE_TRACK_RWX);
  entry^.decl_prot:=prot;

  //masking
  mask:=not (prot shr PAGE_TRACK_SHIFT);
  prot:=(prot and VM_RW) and mask;

  if (prot<>(entry^.real_prot and VM_RW)) then
  begin
   base:=entry^.start;
   size:=entry^.__end-entry^.start;

   r:=md_protect(Pointer(base),size,prot);
   if (r<>0) then
   begin
    Writeln('failed md_protect(',HexStr(base,11),',',HexStr(base+size,11),'):0x',HexStr(r,8));
    Assert(false,'vm_nt_sub_map_protect');
   end;
   //save changes
   entry^.real_prot:=prot;
  end;

  vm_nt_sub_simplify_entry(map, entry);
  entry:=entry^.next;
 end;

 Result:=(KERN_SUCCESS);
end;

function vm_nt_sub_map_tracking(map  :p_vm_nt_sub_map;
                                start:vm_offset_t;
                                __end:vm_offset_t;
                                prot :Byte):Integer;
var
 entry:p_vm_nt_sub_entry;

 base,size:vm_size_t;
 mask:Integer;
 r:Integer;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 if (vm_nt_sub_map_lookup_entry(map, start, @entry)) then
 begin
  vm_nt_sub_map_clip_start(map, entry, start);
 end else
 begin
  entry:=entry^.next;
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  vm_nt_sub_map_clip_end(map, entry, __end);

  //save prot, change track
  prot:=(prot and PAGE_TRACK_RWX) or (entry^.decl_prot and VM_RW);
  entry^.decl_prot:=prot;

  //masking
  mask:=not (prot shr PAGE_TRACK_SHIFT);
  prot:=(prot and VM_RW) and mask;

  if (prot<>(entry^.real_prot and VM_RW)) then
  begin
   base:=entry^.start;
   size:=entry^.__end-entry^.start;

   r:=md_protect(Pointer(base),size,prot);
   if (r<>0) then
   begin
    Writeln('failed md_protect(',HexStr(base,11),',',HexStr(base+size,11),'):0x',HexStr(r,8));
    Assert(false,'vm_nt_sub_map_protect');
   end;
   //save changes
   entry^.real_prot:=prot;
  end;

  vm_nt_sub_simplify_entry(map, entry);
  entry:=entry^.next;
 end;

 Result:=(KERN_SUCCESS);
end;

procedure vm_nt_sub_map_prot_fixup(map  :p_vm_nt_sub_map;
                                   start:vm_offset_t;
                                   __end:vm_offset_t;
                                   mode :Integer);
var
 entry  :p_vm_nt_sub_entry;
 e_start:vm_offset_t;
 e___end:vm_offset_t;

 base,size:vm_size_t;
 prot:Integer;
 mask:Integer;
 r:Integer;
begin
 if (start=__end) then Exit;

 if (vm_nt_sub_map_lookup_entry(map, start, @entry)) then
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

  if (e_start<>e___end) then
  begin
   prot:=entry^.decl_prot;

   //TRACK_PROT Take tracking bits into account
   if ((mode and TRACK_PROT)=0) then
   begin
    prot:=(prot and VM_RW);
   end else
   begin
    mask:=not (prot shr PAGE_TRACK_SHIFT);
    prot:=(prot and VM_RW) and mask;
   end;

   //REMAP_PROT Ignore protect bit checking
   if ((mode and REMAP_PROT)<>0) or (prot<>(entry^.real_prot and VM_RW)) then
   begin
    base:=e_start;
    size:=e___end-e_start;

    r:=md_protect(Pointer(base),size,prot);
    if (r<>0) then
    begin
     Writeln('failed md_protect(',HexStr(base,11),',',HexStr(base+size,11),'):0x',HexStr(r,8));
     Assert(false,'vm_nt_sub_map_prot_fixup');
    end;
    //save changes
    entry^.real_prot:=prot;
   end;

  end;

  entry:=entry^.next;
 end;
end;


end.

