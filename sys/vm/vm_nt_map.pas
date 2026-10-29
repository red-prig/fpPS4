unit vm_nt_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 vm,
 uma,
 kern_mtx,
 systm,
 vm_pmap_prot,
 vm_internal_object,
 vm_nt_sub_map,
 vm_key_instance;

const
 MAX_UNION_SIZE=256*1024*1024;

type
 bit29=0..536870911;
 bit35=0..34359738367;

 t_danger_range=bitpacked record
  start:bit35; //47-12=35
  _size:bit29; //64-35=29
 end;

 t_danger_zone=object
  Flock :mtx;
  Frange:t_danger_range;
  procedure Init;
  procedure Done;
  function  in_range(addr,size:vm_offset_t):Boolean;
  procedure d_wait(addr,size:vm_offset_t);
  procedure lock(start,__end:vm_offset_t);
  procedure unlock;
 end;

 ///

 pp_vm_nt_entry=^p_vm_nt_entry;
 p_vm_nt_entry=^vm_nt_entry;
 p_vm_nt_mirror=^vm_nt_mirror;
 p_vm_nt_rmap_range=^t_vm_nt_rmap_range;

 //reverse map entry: vaddr range -> (obj,offset)
 t_vm_nt_rmap_range=packed record
  next  :p_vm_nt_rmap_range;
  obj   :p_vm_int_obj;
  offset:vm_ooffset_t;
  start :vm_offset_t;
  __end :vm_offset_t;
 end;

 t_vm_nt_rmap=object
  root:p_vm_key_instance; //p_vm_nt_rmap_obj
 end;

 vm_nt_entry=packed object
  prev  :p_vm_nt_entry;     // previous entry
  next  :p_vm_nt_entry;     // next entry
  left  :p_vm_nt_entry;     // left child in binary search tree
  right :p_vm_nt_entry;     // right child in binary search tree
  usize :vm_offset_t;       // unaligned size
  offset:vm_ooffset_t;      // offset into object
  obj   :p_vm_int_obj;      // object I point to
  sub   :t_vm_nt_sub_map;
  mirror:p_vm_nt_mirror;    // at most one mirror per entry
  property start:vm_offset_t read sub.header.start write sub.header.start; // start address
  property __end:vm_offset_t read sub.header.__end write sub.header.__end; // end address
 end;

 p_vm_nt_map=^t_vm_nt_map;
 t_vm_nt_map=object
  header     :vm_nt_entry;   // List of entries
  lock       :mtx;           // Lock for map data
  root       :p_vm_nt_entry; // Root of a binary search tree
  danger_zone:t_danger_zone;
  rmap       :t_vm_nt_rmap;  // (obj,offset) -> vaddr ranges
  rmap_on    :Boolean;       // reverse map is optional
  property min_offset:vm_offset_t read header.sub.header.start write header.sub.header.start;
  property max_offset:vm_offset_t read header.sub.header.__end write header.sub.header.__end;
 end;

 vm_nt_mirror=object
  refs :Integer;
  base :Pointer;      // nil after invalidate
  size :vm_size_t;
  start:vm_offset_t;  // source range
  __end:vm_offset_t;
 end;

 t_vm_nt_rmap_hit=packed record
  obj   :p_vm_int_obj;
  offset:vm_ooffset_t;
  start :vm_offset_t;
  __end :vm_offset_t;
 end;
 p_vm_nt_rmap_hit=^t_vm_nt_rmap_hit;

procedure sys_init_vm_nt;

function  vm_nt_map_max(map:p_vm_nt_map):vm_offset_t;
function  vm_nt_map_min(map:p_vm_nt_map):vm_offset_t;

procedure vm_nt_map_init(map:p_vm_nt_map;min,max:vm_offset_t;enable_rmap:Boolean=False);

procedure vm_nt_map_rmap_enable(map:p_vm_nt_map;enable:Boolean);

function  vm_nt_map_rmap_lookup(map:p_vm_nt_map;
                                obj:p_vm_int_obj;
                                offset,size:vm_ooffset_t;
                                hits:p_vm_nt_rmap_hit;
                                max:Integer):Integer;

function  vm_nt_map_insert(
             map   :p_vm_nt_map;
             obj   :p_vm_int_obj;
             offset:vm_ooffset_t;
             start :vm_offset_t;
             __end :vm_offset_t;
             usize :vm_offset_t;
             prot  :Integer):Integer;

function  vm_nt_map_delete(map:p_vm_nt_map;start:vm_offset_t;__end:vm_offset_t):Integer;

//Change protection taking into account tracking
procedure vm_nt_map_protect(map  :p_vm_nt_map;
                            start:vm_offset_t;
                            __end:vm_offset_t;
                            prot :Integer);

//Change protection tracking
procedure vm_nt_map_tracking(map  :p_vm_nt_map;
                             start:vm_offset_t;
                             __end:vm_offset_t;
                             prot :Integer);

//Update page protection by mode
procedure vm_nt_map_prot_fix(map  :p_vm_nt_map;
                             start:vm_offset_t;
                             __end:vm_offset_t;
                             mode :Integer);

procedure vm_nt_map_madvise(map   :p_vm_nt_map;
                            start :vm_offset_t;
                            __end :vm_offset_t;
                            advise:Integer);

//Create a memory mirror of the current state of the pages
function  vm_nt_map_mirror(map  :p_vm_nt_map;
                           start:vm_offset_t;
                           __end:vm_offset_t):Pointer;

//Create a mirror of the entry block containing addr
function  vm_nt_map_mirror2(map :p_vm_nt_map;
                            addr:vm_offset_t):p_vm_nt_mirror;

function  vm_nt_mirror_reference (mirror:p_vm_nt_mirror):p_vm_nt_mirror;
procedure vm_nt_mirror_deallocate(mirror:p_vm_nt_mirror);

function  vm_nt_map_fetch(map         :p_vm_nt_map;
                          start       :vm_offset_t;
                          __end       :vm_offset_t;
                          var p__start:vm_offset_t;
                          var p____end:vm_offset_t;
                          var p_offset:vm_offset_t;
                          var p____obj:p_vm_int_obj
                         ):Boolean;

implementation

uses
 time,
 kern_param,
 md_map;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

type
 p_range=^t_range;
 t_range=record
  start:vm_offset_t;
  __end:vm_offset_t;
 end;

 t_range_stat=record
  obj:p_vm_int_obj;
  //
  case Byte of
   0:(
      rprev:t_range;
      rcurr:t_range;
      rnext:t_range;
     );
   1:(
      range:array[0..2] of t_range;
     );
 end;

 p_vm_nt_rmap_obj=^t_vm_nt_rmap_obj;
 t_vm_nt_rmap_obj=object(t_vm_key_instance)
  ranges:p_vm_nt_rmap_range;
 end;

var
 vm_nt_entry_zone :uma_zone_t=nil;
 vm_nt_mirror_zone:uma_zone_t=nil;
 vm_nt_rmap_obj_zone:uma_zone_t=nil;
 vm_nt_rmap_range_zone:uma_zone_t=nil;

procedure sys_init_vm_nt;
begin
 vm_nt_entry_zone    :=uma_zcreate('vm_nt_entry'    , sizeof(vm_nt_entry)    , nil, nil, nil, nil, UMA_ALIGN_PTR, 0);
 vm_nt_sub_entry_zone:=uma_zcreate('vm_nt_sub_entry', sizeof(vm_nt_sub_entry), nil, nil, nil, nil, UMA_ALIGN_PTR, 0);
 vm_nt_mirror_zone   :=uma_zcreate('vm_nt_mirror'   , sizeof(vm_nt_mirror)   , nil, nil, nil, nil, UMA_ALIGN_PTR, 0);
 vm_nt_rmap_obj_zone :=uma_zcreate('vm_nt_rmap_obj' , sizeof(t_vm_nt_rmap_obj) , nil, nil, nil, nil, UMA_ALIGN_PTR, 0);
 vm_nt_rmap_range_zone:=uma_zcreate('vm_nt_rmap_range',sizeof(t_vm_nt_rmap_range),nil,nil,nil,nil,UMA_ALIGN_PTR,0);
end;

procedure _vm_nt_mirror_invalidate_entry(entry:p_vm_nt_entry); forward;

//

procedure vm_init_stat(var stat:t_range_stat;entry:p_vm_nt_entry); inline;
begin
 stat:=Default(t_range_stat);
 stat.obj        :=entry^.obj;
 stat.rcurr.start:=entry^.start;
 stat.rcurr.__end:=entry^.__end;
end;

procedure vm_get_space(map:p_vm_nt_map;entry:p_vm_nt_entry;var start,__end:vm_offset_t);
var
 prev:p_vm_nt_entry;
 next:p_vm_nt_entry;
begin
 prev:=entry^.prev;
 next:=entry^.next;

 if (prev=@map^.header) then
 begin
  start:=map^.min_offset;
 end else
 begin
  start:=prev^.__end;
 end;

 if (next=@map^.header) then
 begin
  __end:=map^.max_offset;
 end else
 begin
  __end:=next^.start;
 end;
end;

procedure vm_map(map  :p_vm_nt_map;
                 entry:p_vm_nt_entry;
                 prot :Integer);
var
 start:vm_offset_t;
 __end:vm_offset_t;
 size:vm_size_t;
 max:Integer;
 r:Integer;
begin
 if (entry^.obj<>nil) then
 begin
  size:=entry^.__end-entry^.start;

  vm_get_space(map,entry,start,__end);

  if (start<>__end) then
  if (entry^.start<>start) or
     (entry^.__end<>__end) then
  begin
   r:=md_placeholder_split(Pointer(entry^.start),size);
   if (r<>0) then
   begin
    LOG_CRITICAL(StdErr,'failed md_placeholder_split(',HexStr(entry^.start,11),',',HexStr(entry^.start+size,11),'):0x',HexStr(r,8));
    Assert(false,'vm_map');
   end;
  end;

  max:=entry^.obj^.maxp;

  if (entry^.obj^.hfile<>0) then
  begin
   r:=md_placeholder_commit(Pointer(entry^.start),
                            entry^.usize, //unaligned size
                            (max and VM_RW),
                            entry^.obj^.hfile,
                            entry^.offset);
   if (r<>0) then
   begin
    LOG_CRITICAL(StdErr,'failed md_placeholder_commit(',HexStr(entry^.start,11),',',HexStr(entry^.start+size,11),'):0x',HexStr(r,8));
    Assert(false,'vm_map');
   end;
  end;

  if (entry^.obj^.vtable^.mmmap<>nil) then
  begin
   entry^.obj^.vtable^.mmmap(entry^.obj,entry^.start,entry^.offset,entry^.usize);
  end;

  if ((prot and VM_RW)<>(max and VM_RW)) then
  begin
   r:=md_protect(Pointer(entry^.start),size,(prot and VM_RW));
   if (r<>0) then
   begin
    LOG_CRITICAL(StdErr,'failed md_protect(',HexStr(entry^.start,11),',',HexStr(entry^.start+size,11),'):0x',HexStr(r,8));
    Assert(false,'vm_map');
   end;
  end;

  LOG_TRACE('md_file_mmap(',HexStr(entry^.start,11),',',HexStr(entry^.start+size,11),'):0x',HexStr(r,8));
 end;
end;

//

function MD_IDX_TO_OFF(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl MD_PAGE_SHIFT;
end;

function MD_OFF_TO_IDX(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shr MD_PAGE_SHIFT;
end;

//

procedure t_danger_zone.Init;
begin
 mtx_init(Flock,'danger_zone');
end;

procedure t_danger_zone.Done;
begin
 mtx_destroy(Flock);
end;

function t_danger_zone.in_range(addr,size:vm_offset_t):Boolean;
var
 range:t_danger_range;
begin
 QWORD(range):=System.InterlockedExchangeAdd64(QWORD(Frange),0);

 Result:=(addr<MD_IDX_TO_OFF(range.start+range._size)) and ((addr+size)>MD_IDX_TO_OFF(range.start));
end;

procedure t_danger_zone.d_wait(addr,size:vm_offset_t);
begin
 mtx_lock(Flock);

  if in_range(addr,size) then
  begin
   msleep(@Self,@Flock,PCATCH,'danger_zone',hz);
  end;

 mtx_unlock(Flock);
end;

procedure t_danger_zone.lock(start,__end:vm_offset_t);
var
 range:t_danger_range;
begin
 range.start:=MD_OFF_TO_IDX(start);
 range._size:=MD_OFF_TO_IDX(__end-start);

 System.InterlockedExchange64(QWORD(Frange),QWORD(range));

 mtx_lock(Flock);
end;

procedure t_danger_zone.unlock;
begin
 System.InterlockedExchange64(QWORD(Frange),0);

 mtx_unlock(Flock);

 wakeup(@Self);
end;

//

function vm_remap(map:p_vm_nt_map;
                  entry1:p_vm_nt_entry;
                  entry2:p_vm_nt_entry;
                  entry3:p_vm_nt_entry;
                  var stat:t_range_stat):Boolean;
var
 ets:array[0..2] of p_vm_nt_entry;
 first:p_vm_nt_entry;
 curr :p_vm_nt_entry;

 e_count:Integer;
 r_count:Integer;

 start:vm_offset_t;
 __end:vm_offset_t;
 size:vm_size_t;

 max:Integer;

 p:p_range;
 i,r:Integer;
begin
 Result:=False;

 if (stat.obj=nil) then Exit(False);

 ets[0]:=entry1;
 ets[1]:=entry2;
 ets[2]:=entry3;

 //get first entry
 first:=nil;
 For i:=Low(ets) to High(ets) do
 begin
  curr:=ets[i];

  if (curr<>nil) then
  begin
   first:=curr;
   Break;
  end;
 end;

 if (first=nil) then Exit(False);

 if (stat.rcurr.start=first^.start) and
    (stat.rcurr.__end=first^.__end) then Exit(False);

 start:=0;
 __end:=0;
 e_count:=0;
 r_count:=0;

 //get range
 For i:=Low(ets) to High(ets) do
 begin
  curr:=ets[i];

  if (curr<>nil) then
  begin
   if (start=0) or (start>curr^.start) then
   begin
    start:=curr^.start;
   end;

   if (__end=0) or (__end<curr^.__end) then
   begin
    __end:=curr^.__end;
   end;

   Inc(e_count);
  end;
 end;

 //danger zone
 map^.danger_zone.lock(start,__end);

 //unmap all
 For i:=Low(stat.range) to High(stat.range) do
 begin
  p:=@stat.range[i];
  //
  if (p^.start<>0) and
     (p^.start<>p^.__end) then
  begin
   //
   if (stat.obj^.hfile<>0) then
   begin
    size:=p^.__end-p^.start;

    r:=md_placeholder_decommit(Pointer(p^.start),size);
    if (r<>0) then
    begin
     LOG_CRITICAL(StdErr,'failed md_placeholder_decommit(',HexStr(p^.start,11),',',HexStr(p^.__end,11),'):0x',HexStr(r,8));
     Assert(false,'vm_remap');
    end;
   end;
   //
   LOG_TRACE('md_file_unmap_ex(',HexStr(p^.start,11),',',HexStr(p^.__end,11),'):0x',HexStr(r,8));
   //
   Inc(r_count);
  end;
 end;

 //union parts
 if (r_count>1) then
 begin
  size:=__end-start;

  r:=md_placeholder_union(Pointer(start),size);
  if (r<>0) then
  begin
   LOG_CRITICAL(StdErr,'failed md_placeholder_union(',HexStr(start,11),',',HexStr(__end,11),'):0x',HexStr(r,8));
   Assert(false,'vm_remap');
  end;
  LOG_TRACE('md_union(',HexStr(start,11),',',HexStr(__end,11),'):0x',HexStr(r,8));
 end;

 //split to parts
 if (e_count>1) then
 For i:=Low(ets) to High(ets) do
 begin
  curr:=ets[i];

  if (curr<>nil) and (curr<>first) then
  begin
   size:=curr^.__end-curr^.start;

   r:=md_placeholder_split(Pointer(curr^.start),size);
   if (r<>0) then
   begin
    LOG_ERROR('failed md_placeholder_split(',HexStr(curr^.start,11),',',HexStr(curr^.__end,11),'):0x',HexStr(r,8));

    LOG_CRITICAL(StdErr,'(',HexStr(start,11),',',HexStr(__end,11),')');

    Assert(false,'vm_remap');
   end;

   LOG_TRACE('md_split(',HexStr(ets[i]^.start,11),',',HexStr(ets[i]^.__end,11),'):0x',HexStr(r,8));

   Break; //middle or last splt
  end;
 end;

 max:=stat.obj^.maxp;

 //map new parts
 For i:=Low(ets) to High(ets) do
 begin
  curr:=ets[i];

  if (curr<>nil) then
  begin
   //map new
   if (curr^.obj<>nil) then
   if (stat.obj^.hfile<>0) then
   begin
    r:=md_placeholder_commit(Pointer(curr^.start),
                             curr^.usize, //unaligned size
                             (max and VM_RW),
                             stat.obj^.hfile,
                             curr^.offset);
    if (r<>0) then
    begin
     LOG_CRITICAL(StdErr,'failed md_placeholder_commit(',HexStr(curr^.start,11),',',HexStr(curr^.__end,11),'):0x',HexStr(r,8));
     Assert(false,'vm_remap');
    end;

    LOG_TRACE('md_file_mmap_ex(',HexStr(ets[i]^.start,11),',',HexStr(ets[i]^.__end,11),'):0x',HexStr(r,8));
   end;
  end;
 end;

 //fix prot

 For i:=Low(ets) to High(ets) do
 begin
  curr:=ets[i];

  if (curr<>nil) then
  begin
   if (curr^.obj<>nil) then
   if (stat.obj^.hfile<>0) then
   begin
    vm_nt_sub_map_prot_fixup(@curr^.sub,
                             curr^.start,
                             curr^.__end,
                             TRACK_PROT or REMAP_PROT //untrack trigger or restore track?
                            );
   end;
  end;
 end;

 //danger zone
 map^.danger_zone.unlock;

 Result:=True;
end;

procedure vm_unmap(map:p_vm_nt_map;entry:p_vm_nt_entry);
var
 start:vm_offset_t;
 __end:vm_offset_t;
 size :vm_size_t;
 r:Integer;
begin
 if (entry^.obj<>nil) then
 begin

  if (entry^.obj^.vtable^.unmap<>nil) then
  begin
   entry^.obj^.vtable^.unmap(entry^.obj,entry^.start,entry^.offset,entry^.usize);
  end;

  if (entry^.obj^.hfile<>0) then
  begin
   size:=(entry^.__end-entry^.start);

   r:=md_placeholder_decommit(Pointer(entry^.start),size);
   if (r<>0) then
   begin
    LOG_CRITICAL(StdErr,'failed md_placeholder_decommit(',HexStr(entry^.start,11),',',HexStr(entry^.__end,11),'):0x',HexStr(r,8));
    Assert(false,'vm_unmap');
   end;
   LOG_TRACE('md_placeholder_decommit(',HexStr(entry^.start,11),',',HexStr(entry^.__end,11),'):0x',HexStr(r,8));
  end;

 end;

 vm_get_space(map,entry,start,__end);

 if (start<>__end) then
 if (entry^.start<>start) or
    (entry^.__end<>__end) then
 begin
  r:=md_placeholder_union(Pointer(start),__end-start);
  if (r<>0) then
  begin
   LOG_ERROR('failed md_placeholder_union(',HexStr(start,11),',',HexStr(__end,11),'):0x',HexStr(r,8));

   LOG_CRITICAL(StdErr,'(',HexStr(entry^.start,11),',',HexStr(entry^.__end,11),'):0x',HexStr(r,8));

   Assert(false,'vm_unmap');
  end;
 end;
end;

//

function vm_nt_map_max(map:p_vm_nt_map):vm_offset_t; inline;
begin
 Result:=map^.max_offset;
end;

function vm_nt_map_min(map:p_vm_nt_map):vm_offset_t; inline;
begin
 Result:=map^.min_offset;
end;

procedure VM_NT_MAP_RANGE_CHECK(map:p_vm_nt_map;var start,__end:vm_offset_t);
begin
 if (start<vm_nt_map_min(map)) then
 begin
  start:=vm_nt_map_min(map);
 end;
 if (__end>vm_nt_map_max(map)) then
 begin
  __end:=vm_nt_map_max(map);
 end;
 if (start>__end) then
 begin
  start:=__end;
 end;
end;

procedure vm_nt_map_init(map:p_vm_nt_map;min,max:vm_offset_t;enable_rmap:Boolean=False);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.root:=nil;
 map^.rmap.root:=nil;
 map^.rmap_on:=enable_rmap;
 map^.danger_zone.Init;
 mtx_init(map^.lock,'vm_nt_map');
end;

procedure vm_nt_map_lock(map:p_vm_nt_map); inline;
begin
 mtx_lock(map^.lock);
end;

procedure vm_nt_map_unlock(map:p_vm_nt_map); inline;
begin
 mtx_unlock(map^.lock);
end;

procedure vm_nt_entry_dispose(map:p_vm_nt_map;entry:p_vm_nt_entry); inline;
begin
 uma_zfree(vm_nt_entry_zone, entry);
end;

procedure vm_nt_entry_deallocate(map:p_vm_nt_map;entry:p_vm_nt_entry); inline;
begin
 _vm_nt_mirror_invalidate_entry(entry);

 vm_nt_sub_map_free       (@entry^.sub);
 vm_int_obj_deallocate(entry^.obj);
 vm_nt_entry_dispose      (map,entry);
end;

function vm_nt_entry_create(map:p_vm_nt_map):p_vm_nt_entry;
var
 new_entry:p_vm_nt_entry;
begin
 new_entry:=uma_zalloc(vm_nt_entry_zone, M_WAITOK or M_ZERO);
 Assert((new_entry<>nil),'vm_nt_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

//
//reverse map: (obj,offset) -> vaddr ranges
//

function vm_nt_rmap_obj_find(map:p_vm_nt_map;obj:p_vm_int_obj;create:Boolean):p_vm_nt_rmap_obj;
var
 node:p_vm_nt_rmap_obj;
begin
 Result:=nil;

 if (map=nil) or (obj=nil) then Exit;

 if (map^.rmap.root<>nil) then
 if vm_key_instance_find(map^.rmap.root,obj) then
 begin
  Exit(p_vm_nt_rmap_obj(map^.rmap.root));
 end;

 if not create then Exit;

 node:=uma_zalloc(vm_nt_rmap_obj_zone, M_WAITOK or M_ZERO);
 Assert((node<>nil),'vm_nt_rmap_obj_find: kernel resources exhausted');

 node^.key:=obj;

 vm_key_instance_insert(map^.rmap.root,node);

 Result:=node;
end;

function vm_nt_rmap_range_link(obj:p_vm_nt_rmap_obj;r:p_vm_nt_rmap_range):p_vm_nt_rmap_range; inline;
var
 curr,prev:p_vm_nt_rmap_range;
begin
 prev:=nil;
 curr:=obj^.ranges;

 while (curr<>nil) and
       ((curr^.start<r^.start) or
        ((curr^.start=r^.start) and (curr^.offset<r^.offset))) do
 begin
  prev:=curr;
  curr:=curr^.next;
 end;

 r^.next:=curr;

 if (prev=nil) then
 begin
  obj^.ranges:=r;
 end else
 begin
  prev^.next:=r;
 end;

 Result:=prev;
end;

procedure vm_nt_rmap_range_unlink(obj:p_vm_nt_rmap_obj;r:p_vm_nt_rmap_range);
var
 curr,prev:p_vm_nt_rmap_range;
begin
 prev:=nil;
 curr:=obj^.ranges;

 while (curr<>nil) do
 begin
  if (curr=r) then
  begin
   if (prev=nil) then
   begin
    obj^.ranges:=curr^.next;
   end else
   begin
    prev^.next:=curr^.next;
   end;

   curr^.next:=nil;
   Exit;
  end;

  prev:=curr;
  curr:=curr^.next;
 end;
end;

procedure vm_nt_rmap_add(map:p_vm_nt_map;
                         obj:p_vm_int_obj;
                         offset:vm_ooffset_t;
                         start,__end:vm_offset_t);
var
 robj:p_vm_nt_rmap_obj;
 r,prev,next:p_vm_nt_rmap_range;
begin
 if (map=nil) or (not map^.rmap_on) or (obj=nil) then Exit;
 if (start>=__end) then Exit;

 robj:=vm_nt_rmap_obj_find(map,obj,True);
 if (robj=nil) then Exit;

 r:=uma_zalloc(vm_nt_rmap_range_zone, M_WAITOK or M_ZERO);
 Assert((r<>nil),'vm_nt_rmap_add: kernel resources exhausted');

 r^.obj   :=obj;
 r^.offset:=offset;
 r^.start :=start;
 r^.__end :=__end;

 prev:=vm_nt_rmap_range_link(robj,r);

 //merge with the following range if it is contiguous
 next:=r^.next;
 if (next<>nil) and
    (r^.__end=next^.start) and
    (r^.offset+(r^.__end-r^.start)=next^.offset) then
 begin
  r^.__end:=next^.__end;

  vm_nt_rmap_range_unlink(robj,next);
  uma_zfree(vm_nt_rmap_range_zone,next);
 end;

 //merge with the previous range if it is contiguous
 if (prev<>nil) and
    (prev^.__end=r^.start) and
    (prev^.offset+(prev^.__end-prev^.start)=r^.offset) then
 begin
  prev^.__end:=r^.__end;

  vm_nt_rmap_range_unlink(robj,r);
  uma_zfree(vm_nt_rmap_range_zone,r);
 end;
end;

//Remove a part of the mapping.
procedure vm_nt_rmap_remove(map:p_vm_nt_map;
                            obj:p_vm_int_obj;
                            start,__end:vm_offset_t);
var
 robj:p_vm_nt_rmap_obj;
 r,next,newr:p_vm_nt_rmap_range;
 r_start,r_end:vm_offset_t;
 i_start,i_end:vm_offset_t;
begin
 if (map=nil) or (not map^.rmap_on) or (obj=nil) then Exit;
 if (start>=__end) then Exit;

 robj:=vm_nt_rmap_obj_find(map,obj,False);
 if (robj=nil) then Exit;

 r:=robj^.ranges;

 while (r<>nil) do
 begin
  next:=r^.next;

  r_start:=r^.start;
  r_end  :=r^.__end;

  i_start:=start;
  if (i_start<r_start) then
  begin
   i_start:=r_start;
  end;

  i_end:=__end;
  if (i_end>r_end) then
  begin
   i_end:=r_end;
  end;

  if (i_start<i_end) then
  begin
   if (i_start=r_start) and (i_end=r_end) then
   begin
    //full range
    vm_nt_rmap_range_unlink(robj,r);
    uma_zfree(vm_nt_rmap_range_zone,r);
   end else
   if (i_start=r_start) then
   begin
    //cut left part
    r^.offset:=r^.offset + (i_end-r_start);
    r^.start :=i_end;
   end else
   if (i_end=r_end) then
   begin
    //cut right part
    r^.__end:=i_start;
   end else
   begin
    //cut middle
    newr:=uma_zalloc(vm_nt_rmap_range_zone, M_WAITOK or M_ZERO);
    Assert((newr<>nil),'vm_nt_rmap_remove: kernel resources exhausted');

    newr^.obj   :=obj;
    newr^.offset:=r^.offset + (i_end-r_start);
    newr^.start :=i_end;
    newr^.__end :=r_end;

    newr^.next:=r^.next;
    r^.next   :=newr;

    r^.__end:=i_start;
   end;
  end;

  r:=next;
 end;

 if (robj^.ranges=nil) then
 begin
  vm_key_instance_delete(map^.rmap.root,robj);
  uma_zfree(vm_nt_rmap_obj_zone,robj);
 end;
end;

procedure vm_nt_map_rmap_enable(map:p_vm_nt_map;enable:Boolean);
var
 entry:p_vm_nt_entry;
begin
 if (map=nil) then Exit;

 vm_nt_map_lock(map);

  if enable then
  begin
   if (not map^.rmap_on) then
   begin
    map^.rmap_on:=True;

    entry:=map^.header.next;
    while (entry<>@map^.header) do
    begin
     vm_nt_rmap_add(map,entry^.obj,entry^.offset,entry^.start,entry^.__end);

     entry:=entry^.next;
    end;
   end;
  end else
  begin
   if map^.rmap_on then
   begin
    entry:=map^.header.next;
    while (entry<>@map^.header) do
    begin
     vm_nt_rmap_remove(map,entry^.obj,entry^.start,entry^.__end);

     entry:=entry^.next;
    end;

    map^.rmap_on:=False;
   end;
  end;

 vm_nt_map_unlock(map);
end;

function vm_nt_map_rmap_lookup(map:p_vm_nt_map;
                               obj:p_vm_int_obj;
                               offset,size:vm_ooffset_t;
                               hits:p_vm_nt_rmap_hit;
                               max:Integer):Integer;
var
 robj:p_vm_nt_rmap_obj;
 r:p_vm_nt_rmap_range;
 r_end:vm_ooffset_t;
 endoff:vm_ooffset_t;
begin
 Result:=0;

 if (map=nil) or (not map^.rmap_on) or (obj=nil) then Exit;
 if (size<=0) then Exit;

 endoff:=offset+size;

 vm_nt_map_lock(map);

  if (map^.rmap.root<>nil) then
  if vm_key_instance_find(map^.rmap.root,obj) then
  begin
   robj:=p_vm_nt_rmap_obj(map^.rmap.root);

   r:=robj^.ranges;
   while (r<>nil) do
   begin
    r_end:=r^.offset + (r^.__end-r^.start);

    if (r_end>offset) and (r^.offset<endoff) then
    begin
     if (hits<>nil) and (Result<max) then
     begin
      hits[Result].obj   :=obj;
      hits[Result].offset:=r^.offset;
      hits[Result].start :=r^.start;
      hits[Result].__end :=r^.__end;
     end;

     Inc(Result);
    end;

    r:=r^.next;
   end;
  end;

 vm_nt_map_unlock(map);
end;

function vm_nt_entry_splay(addr:vm_offset_t;root:p_vm_nt_entry):p_vm_nt_entry;
var
 llist,rlist:p_vm_nt_entry;
 ltree,rtree:p_vm_nt_entry;
 y          :p_vm_nt_entry;
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

procedure vm_nt_entry_link(
           map        :p_vm_nt_map;
           after_where:p_vm_nt_entry;
           entry      :p_vm_nt_entry);
begin
 entry^.prev:=after_where;
 entry^.next:=after_where^.next;
 entry^.next^.prev:=entry;
 after_where^.next:=entry;

 if (after_where<>@map^.header) then
 begin
  if (after_where<>map^.root) then
  begin
   vm_nt_entry_splay(after_where^.start, map^.root);
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

procedure vm_nt_entry_unlink(
           map        :p_vm_nt_map;
           entry      :p_vm_nt_entry);
var
 next,prev,root:p_vm_nt_entry;
begin
 if (entry<>map^.root) then
 begin
  vm_nt_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_nt_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
end;

function vm_nt_map_lookup_entry(
           map        :p_vm_nt_map;
           address    :vm_offset_t;
           entry      :pp_vm_nt_entry):Boolean;
var
 cur:p_vm_nt_entry;
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

  cur:=vm_nt_entry_splay(address,cur);
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

function vm_nt_map_simplify_entry(map:p_vm_nt_map;entry:p_vm_nt_entry;var sb:t_range_stat):Boolean; forward;

function _vm_nt_map_insert(
           map   :p_vm_nt_map;
           obj   :p_vm_int_obj;
           offset:vm_ooffset_t;
           start :vm_offset_t;
           __end :vm_offset_t;
           usize :vm_offset_t; //unaligned size
           prot  :Integer):Integer;
var
 new_entry :p_vm_nt_entry;
 prev_entry:p_vm_nt_entry;
 temp_entry:p_vm_nt_entry;
 stat      :t_range_stat;
begin
 if (start<map^.min_offset) or (__end>map^.max_offset) or (start>=__end) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 if vm_nt_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 new_entry:=vm_nt_entry_create(map);
 new_entry^.start :=start;
 new_entry^.__end :=__end;
 new_entry^.usize :=usize; //unaligned size
 new_entry^.obj   :=obj;
 new_entry^.offset:=offset;

 vm_nt_sub_map_init  (@new_entry^.sub,start,__end);

 //fill full map
 vm_nt_sub_map_insert(@new_entry^.sub,start,__end,prot);

 vm_nt_entry_link(map, prev_entry, new_entry);

 vm_nt_rmap_add(map,obj,offset,start,__end);

 vm_map(map,new_entry,prot);

 vm_nt_map_simplify_entry(map,new_entry,stat);

 vm_remap(map,new_entry,nil,nil,stat);

 Result:=KERN_SUCCESS;
end;

function vm_nt_map_insert(
           map   :p_vm_nt_map;
           obj   :p_vm_int_obj;
           offset:vm_ooffset_t;
           start :vm_offset_t;
           __end :vm_offset_t;
           usize :vm_offset_t; //unaligned size
           prot  :Integer):Integer;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 vm_nt_map_lock(map);

 Result:=_vm_nt_map_insert(
           map   ,
           obj   ,
           offset,
           start ,
           __end ,
           usize ,
           prot  );

 vm_nt_map_unlock(map);
end;

function vm_nt_map_simplify_entry(map:p_vm_nt_map;entry:p_vm_nt_entry;var sb:t_range_stat):Boolean;
var
 next,prev:p_vm_nt_entry;
 prevsize,esize:vm_size_t;
 stat:t_range_stat;
begin
 vm_init_stat(stat,entry);

 if (stat.obj<>nil) then
 if ((stat.obj^.flags and INT_UNION_OBJ)=0) then
 begin
  sb:=stat;
  Exit(False);
 end;

 if ((stat.rcurr.__end-stat.rcurr.start)>MAX_UNION_SIZE) then
 begin
  sb:=stat;
  Exit(False);
 end;

 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  prevsize:=prev^.__end - prev^.start;
  if (prev^.__end=stat.rcurr.start) and
     (prevsize<=MAX_UNION_SIZE) and
     (prev^.obj=stat.obj) and
     ((prev^.obj=nil) or (prev^.offset + prevsize=entry^.offset))
     then
  begin
   _vm_nt_mirror_invalidate_entry(prev);
   _vm_nt_mirror_invalidate_entry(entry);

   vm_nt_entry_unlink(map, prev);

   stat.rprev.start:=prev^.start;
   stat.rprev.__end:=prev^.__end;

   entry^.start :=prev^.start;
   entry^.offset:=prev^.offset;
   entry^.usize :=entry^.usize+prev^.usize; //unaligned size

   //update (implicitly)
   //entry^.sub.min_offset:=entry^.start;

   //move
   vm_nt_sub_map_move(@entry^.sub,@prev^.sub);

   vm_nt_entry_deallocate(map,prev);
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  esize:=stat.rcurr.__end - entry^.start;
  if (stat.rcurr.__end=next^.start) and
     (esize<=MAX_UNION_SIZE) and
     (next^.obj=stat.obj) and
     ((stat.obj=nil) or (entry^.offset + esize=next^.offset))
     then
  begin
   _vm_nt_mirror_invalidate_entry(next);
   _vm_nt_mirror_invalidate_entry(entry);

   vm_nt_entry_unlink(map, next);

   stat.rnext.start:=next^.start;
   stat.rnext.__end:=next^.__end;

   entry^.__end:=next^.__end;
   entry^.usize:=entry^.usize+next^.usize; //unaligned size

   //update (implicitly)
   //entry^.sub.max_offset:=entry^.__end;

   //move
   vm_nt_sub_map_move(@entry^.sub,@next^.sub);

   vm_nt_entry_deallocate(map,next);
  end;
 end;

 sb:=stat;
 Result:=True;
end;

procedure vm_nt_map_simplify_entry(map:p_vm_nt_map;entry:p_vm_nt_entry);
var
 stat:t_range_stat;
begin
 vm_nt_map_simplify_entry(map,entry,stat);

 vm_remap(map,entry,nil,nil,stat);
end;

procedure vm_nt_map_clip_start_end(map:p_vm_nt_map;entry:p_vm_nt_entry;start,__end:vm_offset_t);
var
 prev:p_vm_nt_entry;
 next:p_vm_nt_entry;
 stat:t_range_stat;
 del_end:vm_offset_t;
begin
 prev:=nil;
 next:=nil;

 _vm_nt_mirror_invalidate_entry(entry);

 vm_nt_map_simplify_entry(map,entry,stat);

 //entry[start           ,end]
 //prev [start  |   entry[end]

 if (start>entry^.start) then
 begin
  prev:=vm_nt_entry_create(map);
  prev^:=entry^;

  prev^.__end:=start;
  prev^.usize:=(prev^.__end-prev^.start); //unaligned size

  //new map
  vm_nt_sub_map_init(@prev^.sub,prev^.start,prev^.__end);

  //move
  vm_nt_sub_map_clip(@prev^.sub,@entry^.sub);

  entry^.offset:=entry^.offset + (start - entry^.start);
  entry^.start :=start;
  entry^.usize :=entry^.usize-prev^.usize; //unaligned size

  //update (implicitly)
  //prev ^.sub.max_offset:=start;
  //entry^.sub.min_offset:=start;

  vm_nt_entry_link(map, entry^.prev, prev);
  vm_int_obj_reference(prev^.obj);
 end;

 //entry[start          ,end]
 //entry[start  |   next[end]

 if (__end<entry^.__end) then
 begin
  next:=vm_nt_entry_create(map);
  next^:=entry^;

  next^.start :=__end;

  //new map
  vm_nt_sub_map_init(@next^.sub,next^.start,next^.__end);

  //move
  vm_nt_sub_map_clip(@next^.sub,@entry^.sub);

  entry^.__end:=__end;
  entry^.usize:=(entry^.__end-entry^.start); //unaligned size

  next^.offset:=next^.offset + (__end - entry^.start);
  next^.usize :=next^.usize-entry^.usize; //unaligned size

  //update (implicitly)
  //next ^.sub.min_offset:=__end;
  //entry^.sub.max_offset:=__end;

  vm_nt_entry_link(map, entry, next);
  vm_int_obj_reference(next^.obj);
 end;

 //

 if (prev<>nil) or (next<>nil) then
 begin
  //exclude entry
  del_end:=__end;
  if (del_end>entry^.__end) then
  begin
   del_end:=entry^.__end;
  end;

  vm_nt_rmap_remove(map,entry^.obj,entry^.start,del_end);

  vm_int_obj_deallocate(entry^.obj);
  entry^.obj:=nil;
 end;

 vm_remap(map,prev,entry,next,stat);

end;

procedure vm_nt_map_clip_end(map:p_vm_nt_map;entry:p_vm_nt_entry;__end:vm_offset_t);
var
 next:p_vm_nt_entry;
 stat:t_range_stat;
begin
 next:=nil;

 _vm_nt_mirror_invalidate_entry(entry);

 vm_init_stat(stat,entry);

 //

 //entry[start          ,end]
 //entry[start  |   next[end]

 if (__end<entry^.__end) then
 begin
  next:=vm_nt_entry_create(map);
  next^:=entry^;

  next^.start :=__end;

  //new map
  vm_nt_sub_map_init(@next^.sub,next^.start,next^.__end);

  //move
  vm_nt_sub_map_clip(@next^.sub,@entry^.sub);

  entry^.__end:=__end;
  entry^.usize:=(entry^.__end-entry^.start); //unaligned size

  next^.offset:=next^.offset + (__end - entry^.start);
  next^.usize :=next^.usize-entry^.usize; //unaligned size

  //update (implicitly)
  //next ^.sub.min_offset:=__end;
  //entry^.sub.max_offset:=__end;

  vm_nt_entry_link(map, entry, next);
  vm_int_obj_reference(next^.obj);
 end;

 //

 if (next<>nil) then
 begin
  //exclude entry
  vm_nt_rmap_remove(map,entry^.obj,entry^.start,entry^.__end);

  vm_int_obj_deallocate(entry^.obj);
  entry^.obj:=nil;
 end;

 vm_remap(map,nil,entry,next,stat);

end;

procedure vm_nt_entry_delete(map:p_vm_nt_map;entry:p_vm_nt_entry); inline;
begin
 vm_nt_entry_unlink    (map, entry);
 vm_nt_entry_deallocate(map, entry);
end;

function vm_nt_map_delete(map:p_vm_nt_map;start:vm_offset_t;__end:vm_offset_t):Integer;
var
 entry      :p_vm_nt_entry;
 first_entry:p_vm_nt_entry;
 next       :p_vm_nt_entry;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 vm_nt_map_lock(map);

 if (not vm_nt_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;

 end else
 begin
  entry:=first_entry;

  vm_nt_map_clip_start_end(map, entry, start, __end);
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  vm_nt_map_clip_end(map, entry, __end);

  next:=entry^.next;

  vm_nt_rmap_remove(map,entry^.obj,entry^.start,entry^.__end);

  vm_unmap(map,entry);

  vm_nt_entry_delete(map, entry);

  entry:=next;
 end;

 vm_nt_map_unlock(map);

 Result:=(KERN_SUCCESS);
end;

procedure vm_nt_map_protect(map  :p_vm_nt_map;
                            start:vm_offset_t;
                            __end:vm_offset_t;
                            prot :Integer);
var
 entry:p_vm_nt_entry;
 e_start:vm_offset_t;
 e___end:vm_offset_t;
 max:Integer;
begin
 if (start=__end) then Exit;

 vm_nt_map_lock(map);

 if (not vm_nt_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end else
 begin
  //
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

   if (entry^.obj<>nil) then
   begin
    max:=entry^.obj^.maxp;
   end else
   begin
    max:=0;
   end;

   vm_nt_sub_map_protect(@entry^.sub,e_start,e___end,(prot and max and VM_RW));
  end;

  entry:=entry^.next;
 end;

 vm_nt_map_unlock(map);
end;

procedure vm_nt_map_tracking(map  :p_vm_nt_map;
                             start:vm_offset_t;
                             __end:vm_offset_t;
                             prot :Integer);
var
 entry:p_vm_nt_entry;
 e_start:vm_offset_t;
 e___end:vm_offset_t;
begin
 if (start=__end) then Exit;

 vm_nt_map_lock(map);

 if (not vm_nt_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end else
 begin
  //
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
   vm_nt_sub_map_tracking(@entry^.sub,e_start,e___end,prot);
  end;

  entry:=entry^.next;
 end;

 vm_nt_map_unlock(map);
end;




procedure vm_nt_map_prot_fix(map  :p_vm_nt_map;
                             start:vm_offset_t;
                             __end:vm_offset_t;
                             mode :Integer);
var
 entry:p_vm_nt_entry;
 e_start:vm_offset_t;
 e___end:vm_offset_t;
begin
 if (start=__end) then Exit;

 vm_nt_map_lock(map);

 if (not vm_nt_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end else
 begin
  entry:=entry;
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  if (entry^.obj<>nil) then
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
    vm_nt_sub_map_prot_fixup(@entry^.sub,e_start,e___end,mode);
   end;
  end;

  entry:=entry^.next;
 end;

 vm_nt_map_unlock(map);
end;

procedure vm_nt_map_madvise(map   :p_vm_nt_map;
                            start :vm_offset_t;
                            __end :vm_offset_t;
                            advise:Integer);
var
 entry:p_vm_nt_entry;
 base,size:vm_size_t;
 mirror:Pointer;
begin
 if (start=__end) then Exit;

 vm_nt_map_lock(map);

 if (not vm_nt_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end else
 begin
  entry:=entry;
 end;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  base:=entry^.start;
  size:=entry^.__end;

  if (base<start) then
  begin
   base:=start;
  end;

  if (size>__end) then
  begin
   size:=__end;
  end;

  size:=size-base;

  case advise of
   MADV_WILLNEED:md_willneed(Pointer(base),size);
   //
   MADV_DONTNEED:md_dontneed(Pointer(base),size);
   MADV_FREE    :md_dontneed(Pointer(base),size);
   //
   else;
  end;

  //ignore errors

  entry:=entry^.next;
 end;

 vm_nt_map_unlock(map);
end;


function _vm_nt_map_mirror_locked(map  :p_vm_nt_map;
                                  start:vm_offset_t;
                                  __end:vm_offset_t):Pointer;
var
 entry:p_vm_nt_entry;
 base,b_end,size,prev:vm_size_t;
 offset:vm_ooffset_t;
 curr:Pointer;
 obj:p_vm_int_obj;
 max:Integer;
 r:Integer;
begin
 Result:=nil;
 if (start=__end) then Exit;

 r:=md_placeholder_mmap(Result,__end-start);
 if (r<>0) then
 begin
  LOG_CRITICAL(StdErr,'failed md_placeholder_mmap(',HexStr(__end-start,11),'):0x',HexStr(r,8));
  Assert(false,'vm_map');
  Exit;
 end;

 if (not vm_nt_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end;

 prev:=0;

 while (entry<>@map^.header) and (entry^.start<__end) do
 begin
  obj:=entry^.obj;

  if (obj<>nil) then
  if (obj^.hfile<>0) then
  begin
   base  :=entry^.start;
   b_end :=entry^.__end;
   offset:=entry^.offset;

   if (base<start) then
   begin
    offset:=offset+(start-base);
    base:=start;
   end;

   if (b_end>__end) then
   begin
    b_end:=__end;
   end;

   size:=b_end-base;

   //addr in mirror
   curr:=Result+(base-start);

   if ((base<>start) and (base<>prev)) or
      (b_end<>__end) then
   begin
    r:=md_placeholder_split(curr,size);
    if (r<>0) then
    begin
     LOG_CRITICAL(StdErr,'failed md_placeholder_split(',HexStr(curr),',',HexStr(curr+size),'):0x',HexStr(r,8));
     Assert(false,'vm_map');
    end;
   end;

   prev:=b_end;

   max:=obj^.maxp;

   r:=md_placeholder_commit(curr,
                            size,
                            (max and VM_RW),
                            obj^.hfile,
                            offset);
   if (r<>0) then
   begin
    LOG_CRITICAL(StdErr,'failed md_placeholder_commit(',HexStr(curr),',',HexStr(curr+size),'):0x',HexStr(r,8));
    Assert(false,'vm_map');
   end;

  end;

  entry:=entry^.next;
 end;
end;

function vm_nt_map_mirror(map  :p_vm_nt_map;
                          start:vm_offset_t;
                          __end:vm_offset_t):Pointer;
begin
 Result:=nil;
 if (start=__end) then Exit;

 vm_nt_map_lock(map);

 Result:=_vm_nt_map_mirror_locked(map,start,__end);

 vm_nt_map_unlock(map);
end;

procedure _vm_nt_mirror_invalidate(mirror:p_vm_nt_mirror);
var
 base:Pointer;
 size:vm_size_t;
begin
 base:=mirror^.base;
 size:=mirror^.size;

 mirror^.base:=nil;
 mirror^.size:=0;

 if (base<>nil) then
 begin
  md_placeholder_unmap(base,size);
 end;
end;

procedure vm_nt_mirror_destroy(mirror:p_vm_nt_mirror);
begin
 _vm_nt_mirror_invalidate(mirror);

 uma_zfree(vm_nt_mirror_zone,mirror);
end;

function vm_nt_mirror_reference(mirror:p_vm_nt_mirror):p_vm_nt_mirror;
begin
 if (mirror<>nil) then
 begin
  System.InterlockedIncrement(mirror^.refs);
 end;
 Result:=mirror;
end;

procedure vm_nt_mirror_deallocate(mirror:p_vm_nt_mirror);
begin
 if (mirror=nil) then Exit;

 if (System.InterlockedDecrement(mirror^.refs)=0) then
 begin
  vm_nt_mirror_destroy(mirror);
 end;
end;

procedure _vm_nt_mirror_invalidate_entry(entry:p_vm_nt_entry);
var
 mirror:p_vm_nt_mirror;
begin
 mirror:=entry^.mirror;
 if (mirror=nil) then Exit;

 entry^.mirror:=nil;

 _vm_nt_mirror_invalidate(mirror);

 vm_nt_mirror_deallocate(mirror); //entry reference
end;

function _vm_nt_mirror_create_locked(map:p_vm_nt_map;entry:p_vm_nt_entry):p_vm_nt_mirror;
var
 start:vm_offset_t;
 __end:vm_offset_t;
 size :vm_size_t;
 base :Pointer;
begin
 Result:=nil;

 if (entry^.obj=nil) or (entry^.obj^.hfile=0) then Exit;

 start:=entry^.start;
 __end:=entry^.__end;

 base:=_vm_nt_map_mirror_locked(map,start,__end);
 if (base=nil) then Exit;

 size:=(__end-start);

 Result:=uma_zalloc(vm_nt_mirror_zone, M_WAITOK or M_ZERO);
 Assert((Result<>nil),'vm_nt_mirror_create_locked: kernel resources exhausted');

 Result^.base :=base;
 Result^.size :=(size+MD_PAGE_MASK) and (not MD_PAGE_MASK);
 Result^.start:=start;
 Result^.__end:=__end;
 Result^.refs :=1; //entry

 entry^.mirror:=Result;
end;

function vm_nt_map_mirror2(map :p_vm_nt_map;
                           addr:vm_offset_t):p_vm_nt_mirror;
var
 entry:p_vm_nt_entry;
begin
 Result:=nil;

 vm_nt_map_lock(map);

  if vm_nt_map_lookup_entry(map, addr, @entry) then
  begin
   if (entry^.mirror<>nil) then
   begin
    Result:=vm_nt_mirror_reference(entry^.mirror);
   end else
   begin
    Result:=_vm_nt_mirror_create_locked(map,entry);
    if (Result<>nil) then
    begin
     Result:=vm_nt_mirror_reference(Result);
    end;
   end;
  end;

 vm_nt_map_unlock(map);
end;

function vm_nt_map_fetch(map         :p_vm_nt_map;
                         start       :vm_offset_t;
                         __end       :vm_offset_t;
                         var p__start:vm_offset_t;
                         var p____end:vm_offset_t;
                         var p_offset:vm_offset_t;
                         var p____obj:p_vm_int_obj
                        ):Boolean;
var
 entry:p_vm_nt_entry;
 obj:p_vm_int_obj;
begin
 Result:=False;
 if (start=__end) then Exit;

 vm_nt_map_lock(map);

 if (not vm_nt_map_lookup_entry(map, start, @entry)) then
 begin
  entry:=entry^.next;
 end;

 if (entry<>@map^.header) and (entry^.start<__end) then
 begin

  p__start:=entry^.start;
  p____end:=entry^.__end;
  p_offset:=entry^.offset;
  p____obj:=nil;

  obj:=entry^.obj;

  if (obj<>nil) then
  if (obj^.hfile<>0) then
  begin
   p____obj:=obj;
  end;

  Result:=True;
 end;

 vm_nt_map_unlock(map);
end;






end.

