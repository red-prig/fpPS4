unit vm_map;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 vm,
 vmparam,
 vm_pmap,
 sys_vm_object,
 vm_object,
 kern_mtx,
 kern_thr,
 sys_resource,
 kern_resource,
 vm_tracking_map;

type
 vm_flags_t =type Byte;
 vm_eflags_t=type Integer;

 vm_map_object=vm_object_t;

 p_vm_map_entry_t=^vm_map_entry_t;
 vm_map_entry_t=^vm_map_entry;
 vm_map_entry=packed record
  prev          :vm_map_entry_t;       // previous entry
  next          :vm_map_entry_t;       // next entry
  left          :vm_map_entry_t;       // left child in binary search tree
  right         :vm_map_entry_t;       // right child in binary search tree
  start         :vm_offset_t;          // start address
  __end         :vm_offset_t;          // end address
  avail_ssize   :vm_offset_t;          // amt can grow if this is a stack
  adj_free      :vm_offset_t;          // amount of adjacent free space
  max_free      :vm_offset_t;          // max free space in subtree
  vm_obj        :vm_map_object;        // object I point to
  offset        :vm_ooffset_t;         // offset into object
  eflags        :vm_eflags_t;          // map entry flags
  protection    :vm_prot_t;            // protection code
  max_protection:vm_prot_t;            // maximum protection
  inheritance   :vm_inherit_t;         // inheritance
  budget_id     :shortint;             // budget/ptype id
  name          :array[0..31] of Char; // entry name
  anon_addr     :Pointer;              // source code address
  entry_id      :QWORD;                // order id
 end;

 p_vm_map_t=^vm_map_t;
 vm_map_t=^_vm_map;
 _vm_map=object
  header   :vm_map_entry;   // List of entries
  lock     :mtx;            // Lock for map data
  size     :vm_size_t;      // virtual size
  nentries :Integer;        // Number of entries
  timestamp:DWORD;          // Version number
  flags    :vm_flags_t;     // flags for this vm_map
  busy     :Integer;
  root     :vm_map_entry_t; // Root of a binary search tree
  pmap     :pmap_t;         // (c) Physical map
  rmap     :Pointer;        // p_rmem_map
  entry_id :QWORD;
  property  min_offset:vm_offset_t read header.start write header.start;
  property  max_offset:vm_offset_t read header.__end write header.__end;
 end;

 p_vmspace=^vmspace;
 vmspace=packed record
  vm_map      :_vm_map; // VM address map
  //
  sv_usrstack :caddr_t; // USRSTACK
  sv_psstrings:caddr_t; // PS_STRINGS
  ps_strings  :Pointer;
  //
  vm_swrss    :segsz_t; // resident set size before last swap
  vm_tsize    :segsz_t; // text size (pages) XXX
  vm_dsize    :segsz_t; // data size (pages) XXX
  vm_ssize    :segsz_t; // stack size (pages)
  vm_taddr    :caddr_t; // (c) user virtual address of text
  vm_daddr    :caddr_t; // (c) user virtual address of data
  vm_maxsaddr :caddr_t; // user VA at max stack growth
  //
  vm_pmap     :t_pmap;  // private physical map
 end;

const
 MAP_ENTRY_NOSYNC    =$0001;
 MAP_ENTRY_IS_SUB_MAP=$0002;
 MAP_ENTRY_COW       =$0004;
 MAP_ENTRY_NEEDS_COPY=$0008;
 MAP_ENTRY_NOFAULT   =$0010;
 MAP_ENTRY_USER_WIRED=$0020;

 MAP_ENTRY_BEHAV_NORMAL    =$0000; // default behavior
 MAP_ENTRY_BEHAV_SEQUENTIAL=$0040; // expect sequential access
 MAP_ENTRY_BEHAV_RANDOM    =$0080; // expect random access
 MAP_ENTRY_BEHAV_RESERVED  =$00C0; // future use

 MAP_ENTRY_BEHAV_MASK=$00C0;

 MAP_ENTRY_IN_TRANSITION=$0100; // entry being changed
 MAP_ENTRY_NEEDS_WAKEUP =$0200; // waiters in transition
 MAP_ENTRY_NOCOREDUMP   =$0400; // don't include in a core

 MAP_ENTRY_GROWS_DOWN   =$1000; // Top-down stacks
 MAP_ENTRY_GROWS_UP     =$2000; // Bottom-up stacks

 MAP_ENTRY_WIRE_SKIPPED =$4000;

 MAP_ENTRY_VN_WRITECNT  =$10000; // writeable vnode mapping

                      //0x20000 not simplify ???
                      //0x40000
                      //0x80000

 MAP_ENTRY_IN_BUDGET    =$200000;

 MAP_ENTRY_NO_COALESCE  =$400000;

 //vm_flags_t values
 //MAP_WIREFUTURE =$01; // wire all future pages
 //MAP_BUSY_WAKEUP=$02;

 //Copy-on-write flags for vm_map operations
 MAP_INHERIT_SHARE   =$0001;
 MAP_COPY_ON_WRITE   =$0002;
 MAP_NOFAULT         =$0004;
 MAP_PREFAULT        =$0008;
 MAP_PREFAULT_PARTIAL=$0010;
 MAP_DISABLE_SYNCER  =$0020;
 MAP_DISABLE_COREDUMP=$0100;
 MAP_PREFAULT_MADVISE=$0200; // from (user) madvise request
 MAP_VN_WRITECOUNT   =$0400;
 MAP_STACK_GROWS_DOWN=$1000;
 MAP_STACK_GROWS_UP  =$2000;
 MAP_ACC_CHARGED     =$4000;
 MAP_ACC_NO_CHARGE   =$8000;

 MAP_COW_SYSTEM      =$10000;
 MAP_COW_NO_BUDGET   =$20000;
 MAP_COW_KERNEL      =$40000;

 MAP_COW_NO_COALESCE =$400000;

 //vm_fault option flags
 VM_FAULT_NORMAL       =0; // Nothing special
 VM_FAULT_CHANGE_WIRING=1; // Change the wiring as appropriate
 VM_FAULT_DIRTY        =2; // Dirty the page; use w/VM_PROT_COPY

 VMFS_NO_SPACE     =0; // don't find; use the given range
 VMFS_ANY_SPACE    =1; // find a range with any alignment
 VMFS_SUPER_SPACE  =2; // find a superpage-aligned range
 VMFS_OPTIMAL_SPACE=4; // find a range with optimal alignment

 //vm_map_wire and vm_map_unwire option flags
 //VM_MAP_WIRE_SYSTEM =0; // wiring in a kernel map
 //VM_MAP_WIRE_USER   =1; // wiring in a user map

 //VM_MAP_WIRE_NOHOLES=0; // region must not have holes
 //VM_MAP_WIRE_HOLESOK=2; // region may have holes

 //VM_MAP_WIRE_WRITE  =4; // Validate writable.

 VM_FAULT_READ_AHEAD_MIN = 7;
 VM_FAULT_READ_AHEAD_INIT=15;
 VM_FAULT_READ_AHEAD_MAX = 7;

function  VMFS_ALIGNED_SPACE(x:QWORD):QWORD; inline; // find a range with fixed alignment

function  vm_map_entry_behavior(entry:vm_map_entry_t):Integer;
function  vm_map_max(map:vm_map_t):vm_offset_t;
function  vm_map_min(map:vm_map_t):vm_offset_t;
function  vm_map_pmap(map:vm_map_t):pmap_t;
procedure vm_map_modflags(map:vm_map_t;_set,clear:vm_flags_t);

function  vm_map_lookup_entry(
            map        :vm_map_t;
            address    :vm_offset_t;
            entry      :p_vm_map_entry_t):Boolean;

function  vm_map_insert(
           map   :vm_map_t;
           obj   :vm_object_t;
           offset:vm_ooffset_t;
           start :vm_offset_t;
           __end :vm_offset_t;
           prot  :vm_prot_t;
           max   :vm_prot_t;
           cow   :Integer;
           alias :Boolean):Integer;

function  vm_map_findspace(map   :vm_map_t;
                           start :vm_offset_t;
                           length:vm_size_t;
                           addr  :p_vm_offset_t):Integer;

procedure vm_map_lookup_done(map:vm_map_t;entry:vm_map_entry_t);

function  vm_map_lookup(var_map    :p_vm_map_t;        { IN/OUT }
                        vaddr      :vm_offset_t;
                        fault_typea:vm_prot_t;
                        out_entry  :p_vm_map_entry_t;  { OUT }
                        vm_obj     :p_vm_object_t;     { OUT }
                        pindex     :p_vm_pindex_t;     { OUT }
                        out_prot   :p_vm_prot_t        { OUT }
                       ):Integer;

function  vm_map_lookup_locked(var_map    :p_vm_map_t;        { IN/OUT }
                               vaddr      :vm_offset_t;
                               fault_typea:vm_prot_t;
                               out_entry  :p_vm_map_entry_t;  { OUT }
                               vm_obj     :p_vm_object_t;     { OUT }
                               pindex     :p_vm_pindex_t;     { OUT }
                               out_prot   :p_vm_prot_t;       { OUT }
                               wired      :PBoolean           { OUT }
                              ):Integer;

function  vm_map_protect(map     :vm_map_t;
                         start   :vm_offset_t;
                         __end   :vm_offset_t;
                         new_prot:vm_prot_t;
                         set_max :Boolean):Integer;

function  vm_map_madvise(map  :vm_map_t;
                         start:vm_offset_t;
                         __end:vm_offset_t;
                         behav:Integer):Integer;

function  vm_map_sync(map       :vm_map_t;
                      start     :vm_offset_t;
                      __end     :vm_offset_t;
                      syncio    :Boolean;
                      invalidate:Boolean):Integer;

function  vm_map_find(map       :vm_map_t;
                      vm_obj    :vm_object_t;
                      offset    :vm_ooffset_t;
                      addr      :p_vm_offset_t;
                      length    :vm_size_t;
                      find_space:Integer;
                      prot      :vm_prot_t;
                      max       :vm_prot_t;
                      cow       :Integer):Integer;

procedure vm_map_simplify_entry(map:vm_map_t;entry:vm_map_entry_t);

function  vm_map_fixed(map    :vm_map_t;
                       vm_obj :vm_object_t;
                       offset :vm_ooffset_t;
                       start  :vm_offset_t;
                       length :vm_size_t;
                       prot   :vm_prot_t;
                       max    :vm_prot_t;
                       cow    :Integer;
                       overwr :Integer):Integer;

function  vm_map_stack(map      :vm_map_t;
                       addrbos  :vm_offset_t;
                       max_ssize:vm_size_t;
                       prot     :vm_prot_t;
                       max      :vm_prot_t;
                       cow      :Integer):Integer;

function  vm_map_growstack(map:vm_map_t;addr:vm_offset_t):Integer;
function  vmspace_exec(minuser,maxuser:vm_offset_t):Integer;

procedure vm_map_lock(map:vm_map_t;tm:Boolean=True);
function  vm_map_trylock(map:vm_map_t):Boolean;
procedure vm_map_unlock(map:vm_map_t;def:Boolean=True);

function  vm_map_delete(map:vm_map_t;start:vm_offset_t;__end:vm_offset_t;rmap_free:Boolean):Integer;
function  vm_map_remove(map:vm_map_t;start:vm_offset_t;__end:vm_offset_t):Integer;

procedure vm_map_set_name(map:vm_map_t;start,__end:vm_offset_t;name:PChar);
procedure vm_map_set_name_locked(map:vm_map_t;start,__end:vm_offset_t;name:PChar);
procedure vm_map_set_name_locked(map:vm_map_t;start,__end:vm_offset_t;name:PChar;i:vm_inherit_t);

procedure vm_map_track_insert(map:vm_map_t;tobj:Pointer);
procedure vm_map_track_remove(map:vm_map_t;tobj:Pointer);
function  vm_map_track_next  (map:vm_map_t;start:vm_offset_t;tobj:Pointer;htype:T_THANDLE_TYPE):Pointer;
procedure _vm_map_track_delete_deferred(map:vm_map_t;tobj:Pointer);
function  vm_map_track_trigger(map:vm_map_t;start,__end:vm_offset_t;exclude:Pointer;mode:T_TRIGGER_MODE):Integer;
procedure vm_map_track_restore(map:vm_map_t;tobj:Pointer);

function  vmspace_pmap(vm:p_vmspace):pmap_t; inline;

procedure vm_map_entry_deallocate(entry:vm_map_entry_t);

procedure vminit; //SYSINIT

implementation

uses
 md_map,
 kern_proc,
 rmem_map,
 kern_budget;

var
 sgrowsiz:QWORD=vmparam.SGROWSIZ;
 stack_guard_page:Integer=0;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function VMFS_ALIGNED_SPACE(x:QWORD):QWORD; inline; // find a range with fixed alignment
begin
 Result:=x shl 8;
end;

function vm_map_entry_behavior(entry:vm_map_entry_t):Integer; inline;
begin
 Result:=(entry^.eflags and MAP_ENTRY_BEHAV_MASK);
end;

function vm_map_max(map:vm_map_t):vm_offset_t; inline;
begin
 Result:=map^.max_offset;
end;

function vm_map_min(map:vm_map_t):vm_offset_t; inline;
begin
 Result:=map^.min_offset;
end;

function vm_map_pmap(map:vm_map_t):pmap_t; inline;
begin
 Result:=map^.pmap;
end;

procedure vm_map_modflags(map:vm_map_t;_set,clear:vm_flags_t); inline;
begin
 map^.flags:=(map^.flags or _set) and (not clear);
end;

{
 * VM_MAP_RANGE_CHECK: [ internal use only ]
 *
 * Asserts that the starting and ending region
 * addresses fall within the valid range of the map.
 }
procedure VM_MAP_RANGE_CHECK(map:vm_map_t;var start,__end:vm_offset_t);
begin
 if (start<vm_map_min(map)) then
 begin
  start:=vm_map_min(map);
 end;
 if (__end>vm_map_max(map)) then
 begin
  __end:=vm_map_max(map);
 end;
 if (start>__end) then
 begin
  start:=__end;
 end;
end;

function ENTRY_CHARGED(e:vm_map_entry_t):Boolean; inline;
begin
 Result:=(e^.vm_obj<>nil) and ((e^.eflags and MAP_ENTRY_NEEDS_COPY)=0);
end;

function vmspace_pmap(vm:p_vmspace):pmap_t; inline;
begin
 Result:=@vm^.vm_pmap;
end;

procedure vm_map_init(map:vm_map_t;pmap:pmap_t;min,max:vm_offset_t); forward;

var
 g_vmspace:vmspace;

{
 * Allocate a vmspace structure, including a vm_map and pmap,
 * and initialize those structures.  The refcnt is set to 1.
 }
function vmspace_alloc():p_vmspace;
var
 vm:p_vmspace;
 map:vm_map_t;
 i:Integer;
begin
 vm:=@g_vmspace;

 pmap_pinit(vmspace_pmap(vm));

 vm_map_init(@vm^.vm_map,vmspace_pmap(vm),VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS);

 //vm^.vm_refcnt:=1;
 //vm^.vm_shm:=nil;
 vm^.vm_swrss:=0;
 vm^.vm_tsize:=0;
 vm^.vm_dsize:=0;
 vm^.vm_ssize:=0;
 vm^.vm_taddr:=nil;
 vm^.vm_daddr:=nil;
 vm^.vm_maxsaddr:=nil;

 if Length(pmap_mem)>1 then
 begin
  map:=@vm^.vm_map;
  vm_map_lock(map);
   For i:=0 to High(pmap_mem)-1 do
   begin
    vm_map_insert         (map, nil, 0, pmap_mem[i].__end, pmap_mem[i+1].start, 0, 0, -1, false);
    vm_map_set_name_locked(map,         pmap_mem[i].__end, pmap_mem[i+1].start, '#hole', VM_INHERIT_HOLE);
   end;
  vm_map_unlock(map);
 end;

 Result:=vm;
end;

procedure vm_map_lock(map:vm_map_t;tm:Boolean=True); public;
begin
 mtx_lock(map^.lock);
 if tm then
 begin
  Inc(map^.timestamp);
 end;
end;

function vm_map_trylock(map:vm_map_t):Boolean;
begin
 Result:=mtx_trylock(map^.lock);
 if Result then
 begin
  Inc(map^.timestamp);
 end;
end;

procedure vm_map_process_deferred;
var
 td:p_kthread;
 entry,next:vm_map_entry_t;
begin
 td:=curkthread;
 if (td=nil) then Exit;
 entry:=td^.td_map_def_user;
 td^.td_map_def_user:=nil;
 while (entry<>nil) do
 begin
  next:=entry^.next;
  if ((entry^.eflags and MAP_ENTRY_VN_WRITECNT)<>0) then
  begin
   Assert((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0,'Submap with writecount');
  end;
  vm_map_entry_deallocate(entry);
  entry:=next;
 end;
end;

procedure vm_map_unlock(map:vm_map_t;def:Boolean=True); public;
begin
 mtx_unlock(map^.lock);
 if def then
 begin
  vm_map_process_deferred();
 end;
end;

{
 * vm_map_locked:
 *
 * Returns a non-zero value if the caller holds a write (exclusive) lock
 * on the specified map and the value "0" otherwise.
 }
function vm_map_locked(map:vm_map_t):Boolean; inline;
begin
 Result:=mtx_owned(map^.lock);
end;

procedure VM_MAP_ASSERT_LOCKED(map:vm_map_t); inline;
begin
 Assert(vm_map_locked(map));
end;

{
 * vm_map_create:
 *
 * Creates and returns a new empty VM map with
 * the given physical map structure, and having
 * the given lower and upper address bounds.
 }
function vm_map_create(pmap:pmap_t;min,max:vm_offset_t):vm_map_t;
begin
 Result:=AllocMem(SizeOf(_vm_map));
 vm_map_init(Result,pmap,min,max);
end;

{
 * Initialize an existing vm_map structure
 * such as that in the vmspace structure.
 }
procedure _vm_map_init(map:vm_map_t;pmap:pmap_t;min,max:vm_offset_t);
begin
 map^.header.next:=@map^.header;
 map^.header.prev:=@map^.header;
 map^.pmap:=pmap;
 map^.min_offset:=min;
 map^.max_offset:=max;
 map^.header.adj_free:=(max-min);
 map^.header.max_free:=(max-min);
 map^.flags:=0;
 map^.root:=nil;
 map^.timestamp:=0;
 map^.busy:=0;
end;

procedure vm_map_init(map:vm_map_t;pmap:pmap_t;min,max:vm_offset_t);
begin
 _vm_map_init(map, pmap, min, max);
 mtx_init(map^.lock,'user map');
end;

{
 * vm_map_entry_dispose: [ internal use only ]
 *
 * Inverse of vm_map_entry_create.
 }
procedure vm_map_entry_dispose(map:vm_map_t;entry:vm_map_entry_t); inline;
begin
 FreeMem(entry);
end;

{
 * vm_map_entry_create: [ internal use only ]
 *
 * Allocates a VM map entry for insertion.
 * No entry fields are filled in.
 }
function vm_map_entry_create(map:vm_map_t):vm_map_entry_t;
var
 new_entry:vm_map_entry_t;
begin
 new_entry:=AllocMem(SizeOf(vm_map_entry));
 Assert((new_entry<>nil),'vm_map_entry_create: kernel resources exhausted');
 Result:=new_entry;
end;

{
 * vm_map_entry_set_behavior:
 *
 * Set the expected access behavior, either normal, random, or
 * sequential.
 }
procedure vm_map_entry_set_behavior(entry:vm_map_entry_t;behavior:Byte); inline;
begin
 entry^.eflags:=(entry^.eflags and (not MAP_ENTRY_BEHAV_MASK)) or (behavior and MAP_ENTRY_BEHAV_MASK);
end;

{
 * vm_map_entry_set_max_free:
 *
 * Set the max_free field in a vm_map_entry.
 }
procedure vm_map_entry_set_max_free(entry:vm_map_entry_t);
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

{
 * vm_map_entry_splay:
 *
 * The Sleator and Tarjan top-down splay algorithm with the
 * following variation.  Max_free must be computed bottom-up, so
 * on the downward pass, maintain the left and right spines in
 * reverse order.  Then, make a second pass up each side to fix
 * the pointers and compute max_free.  The time bound is O(log n)
 * amortized.
 *
 * The new root is the vm_map_entry containing "addr", or else an
 * adjacent entry (lower or higher) if addr is not in the tree.
 *
 * The map must be locked, and leaves it so.
 *
 * Returns: the new root.
 }
function vm_map_entry_splay(addr:vm_offset_t;root:vm_map_entry_t):vm_map_entry_t;
var
 llist,rlist:vm_map_entry_t;
 ltree,rtree:vm_map_entry_t;
 y          :vm_map_entry_t;
begin
 { Special case of empty tree. }
 if (root=nil) then Exit(root);

 {
  * Pass One: Splay down the tree until we find addr or a nil
  * pointer where addr would go.  llist and rlist are the two
  * sides in reverse order (bottom-up), with llist linked by
  * the right pointer and rlist linked by the left pointer in
  * the vm_map_entry.  Wait until Pass Two to set max_free on
  * the two spines.
  }
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
    vm_map_entry_set_max_free(root);
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
    vm_map_entry_set_max_free(root);
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
  vm_map_entry_set_max_free(llist);
  ltree:=llist;
  llist:=y;
 end;
 rtree:=root^.right;
 while (rlist<>nil) do
 begin
  y:=rlist^.left;
  rlist^.left:=rtree;
  vm_map_entry_set_max_free(rlist);
  rtree:=rlist;
  rlist:=y;
 end;

 {
  * Final assembly: add ltree and rtree as subtrees of root.
  }
 root^.left:=ltree;
 root^.right:=rtree;
 vm_map_entry_set_max_free(root);

 Result:=(root);
end;

{
 * vm_map_entry_{un,}link:
 *
 * Insert/remove entries from maps.
 }
procedure vm_map_entry_link(
           map        :vm_map_t;
           after_where:vm_map_entry_t;
           entry      :vm_map_entry_t);
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
   vm_map_entry_splay(after_where^.start, map^.root);
  end;
  entry^.right:=after_where^.right;
  entry^.left:=after_where;
  after_where^.right:=nil;
  after_where^.adj_free:=entry^.start - after_where^.__end;
  vm_map_entry_set_max_free(after_where);
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
 vm_map_entry_set_max_free(entry);
 map^.root:=entry;
end;

procedure vm_map_entry_unlink(
           map        :vm_map_t;
           entry      :vm_map_entry_t);
var
 next,prev,root:vm_map_entry_t;
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (entry<>map^.root) then
 begin
  vm_map_entry_splay(entry^.start, map^.root);
 end;
 if (entry^.left=nil) then
 begin
  root:=entry^.right;
 end else
 begin
  root:=vm_map_entry_splay(entry^.start, entry^.left);
  root^.right:=entry^.right;
  if (root^.next=@map^.header) then
  begin
   root^.adj_free:=map^.max_offset-root^.__end;
  end else
  begin
   root^.adj_free:=entry^.next^.start-root^.__end;
  end;
  vm_map_entry_set_max_free(root);
 end;
 map^.root:=root;

 prev:=entry^.prev;
 next:=entry^.next;
 next^.prev:=prev;
 prev^.next:=next;
 Dec(map^.nentries);
end;

{
 * vm_map_entry_resize_free:
 *
 * Recompute the amount of free space following a vm_map_entry
 * and propagate that value up the tree.  Call this function after
 * resizing a map entry in-place, that is, without a call to
 * vm_map_entry_link() or _unlink().
 *
 * The map must be locked, and leaves it so.
 }
procedure vm_map_entry_resize_free(map:vm_map_t;entry:vm_map_entry_t);
begin

 {
  * Using splay trees without parent pointers, propagating
  * max_free up the tree is done by moving the entry to the
  * root and making the change there.
  }
 if (entry<>map^.root) then
 begin
  map^.root:=vm_map_entry_splay(entry^.start, map^.root);
 end;

 if (entry^.next=@map^.header) then
 begin
  entry^.adj_free:=map^.max_offset-entry^.__end;
 end else
 begin
  entry^.adj_free:=entry^.next^.start-entry^.__end;
 end;
 vm_map_entry_set_max_free(entry);
end;

{
 * vm_map_lookup_entry: [ internal use only ]
 *
 * Finds the map entry containing (or
 * immediately preceding) the specified address
 * in the given map; the entry is returned
 * in the "entry" parameter.  The boolean
 * result indicates whether the address is
 * actually contained in the map.
 }
function vm_map_lookup_entry(
           map        :vm_map_t;
           address    :vm_offset_t;
           entry      :p_vm_map_entry_t):Boolean;
var
 cur:vm_map_entry_t;
begin
 VM_MAP_ASSERT_LOCKED(map);

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
  cur:=vm_map_entry_splay(address,cur);
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

function vm_object_rmap_insert(map   :vm_map_t;
                               obj   :vm_object_t;
                               start :vm_offset_t;
                               __end :vm_offset_t;
                               offset:vm_ooffset_t;
                               alias :Boolean):Integer;
var
 rmap:p_rmem_map;
 length:vm_offset_t;
begin
 rmap:=map^.rmap;
 length:=__end-start;

 //transform by base addr
 offset:=offset + (QWORD(obj^.un_pager.map_base) - VM_MIN_GPU_ADDRESS);

 rmem_map_lock(rmap);

 if not alias then
 begin
  if rmem_map_test(rmap,OFF_TO_IDX(offset),OFF_TO_IDX(offset+length)) then
  begin
   rmem_map_unlock(rmap);
   Exit(KERN_NO_SPACE);
  end;
 end;

 Result:=rmem_map_insert(rmap, start, OFF_TO_IDX(offset), OFF_TO_IDX(offset+length));

 rmem_map_unlock(rmap);
end;

function vm_object_rmap_release(map   :vm_map_t;
                                obj   :vm_object_t;
                                start :vm_offset_t;
                                __end :vm_offset_t;
                                offset:vm_ooffset_t):Integer;
var
 rmap:p_rmem_map;
 length:vm_offset_t;
begin
 rmap:=map^.rmap;
 length:=__end-start;

 //transform by base addr
 offset:=offset + (QWORD(obj^.un_pager.map_base) - VM_MIN_GPU_ADDRESS);

 rmem_map_lock(rmap);

  Result:=rmem_map_delete(rmap, start, OFF_TO_IDX(offset), OFF_TO_IDX(offset+length));

 rmem_map_unlock(rmap);
end;

procedure vm_map_delete_internal(map:vm_map_t;entry:vm_map_entry_t;__end:vm_offset_t); forward;

function vm_map_insert_internal(
           map   :vm_map_t;
           obj   :vm_object_t;
           offset:vm_ooffset_t;
           start :vm_offset_t;
           __end :vm_offset_t;
           prot  :vm_prot_t;
           max   :vm_prot_t;
           cow   :Integer;
           alias :Boolean):Integer;
begin
 Result:=KERN_SUCCESS;

 if (cow=-1) then Exit;

 if (obj<>nil) then
 begin
  if ((obj^.flags and OBJ_DMEM_EXT)<>0) or
     (obj^.otype=OBJT_PHYSHM) then
  begin
   Result:=vm_object_rmap_insert(map,obj,start,__end,offset,alias);
  end;
 end;

 if (Result=KERN_SUCCESS) then
 begin

  if (obj=nil) and (max=0) and (prot=0) then
  begin
   //reserved only

   pmap_remove(map^.pmap,
               obj,
               start,
               __end);
  end else
  begin

   //force copy
   if ((cow and MAP_COPY_ON_WRITE)<>0) then
   begin
    prot:=prot or VM_PROT_COPY;
   end;

   pmap_enter_object(map^.pmap,
                     obj,
                     offset,
                     start,
                     __end,
                     prot);

  end;

 end;

end;

{
 * vm_map_insert:
 *
 * Inserts the given whole VM object into the target
 * map at the specified address range.  The object's
 * size should match that of the address range.
 *
 * Requires that the map be locked, and leaves it so.
 *
 * If object is non-nil, ref count must be bumped by caller
 * prior to making call to account for the new entry.
 }
function vm_map_insert(
           map   :vm_map_t;
           obj   :vm_object_t;
           offset:vm_ooffset_t;
           start :vm_offset_t;
           __end :vm_offset_t;
           prot  :vm_prot_t;
           max   :vm_prot_t;
           cow   :Integer;
           alias :Boolean):Integer;
label
 _budget,
 charged;
var
 new_entry  :vm_map_entry_t;
 prev_entry :vm_map_entry_t;
 temp_entry :vm_map_entry_t;
 protoeflags:vm_eflags_t;
 inheritance:vm_inherit_t;
 charge_prev_obj:Boolean;

begin
 VM_MAP_ASSERT_LOCKED(map);

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
 if vm_map_lookup_entry(map,start,@temp_entry) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 prev_entry:=temp_entry;

 {
  * Assert that the next entry doesn't overlap the end point.
  }
 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<__end) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 protoeflags:=0;
 charge_prev_obj:=FALSE;

 protoeflags:=protoeflags or (cow and (MAP_COW_NO_COALESCE or MAP_COW_NO_BUDGET));

 if ((cow and MAP_COPY_ON_WRITE)<>0) then
 begin
  protoeflags:=protoeflags or MAP_ENTRY_COW or MAP_ENTRY_NEEDS_COPY;
 end;

 if ((cow and MAP_NOFAULT)<>0) then
 begin
  protoeflags:=protoeflags or MAP_ENTRY_NOFAULT;

  Assert(obj=nil,'vm_map_insert: paradoxical MAP_NOFAULT request');
 end;

 if ((cow and MAP_DISABLE_SYNCER)<>0) then
 begin
  protoeflags:=protoeflags or MAP_ENTRY_NOSYNC;
 end;

 if ((cow and MAP_DISABLE_COREDUMP)<>0) then
 begin
  protoeflags:=protoeflags or MAP_ENTRY_NOCOREDUMP;
 end;

 if ((cow and MAP_VN_WRITECOUNT)<>0) then
 begin
  protoeflags:=protoeflags or MAP_ENTRY_VN_WRITECNT;
 end;

 if ((cow and MAP_INHERIT_SHARE)<>0) then
  inheritance:=VM_INHERIT_SHARE
 else
  inheritance:=VM_INHERIT_DEFAULT;

 if ((cow and (MAP_ACC_NO_CHARGE or MAP_NOFAULT))<>0) then
 begin
  goto charged;
 end;

 if ((cow and MAP_ACC_CHARGED)<>0) or (((prot and VM_PROT_WRITE)<>0) and
     (((protoeflags and MAP_ENTRY_NEEDS_COPY)<>0) or (obj=nil))) then
 begin
  if (obj=nil) and ((protoeflags and MAP_ENTRY_NEEDS_COPY)=0) then
  begin
   charge_prev_obj:=TRUE;
  end;
 end;

charged:

 //budget
 if (max=0) or
    ((cow and MAP_COW_NO_BUDGET)<>0) or
    (p_proc.p_budget_ptype=-1) then
 begin
   //
 end else
 if (obj=nil) then
 begin
  _budget:

  protoeflags:=protoeflags or MAP_ENTRY_IN_BUDGET;

  if (vm_budget_reserve(p_proc.p_budget_ptype,field_malloc,__end-start)<>0) then
  begin
   Exit(KERN_RESOURCE_SHORTAGE);
  end;

 end else
 if (obj^.otype in [OBJT_DEFAULT,OBJT_SWAP,OBJT_VNODE,OBJT_SELF]) and
    ((obj^.flags and OBJ_JITSHM_EXT)=0) then
 begin
  goto _budget;
 end;
 //

 if (obj<>nil) then
 begin
  {
   * OBJ_ONEMAPPING must be cleared unless this mapping
   * is trivially proven to be the only mapping for any
   * of the object's pages.  (Object granularity
   * reference counting is insufficient to recognize
   * aliases with precision.)
   }
  VM_OBJECT_LOCK(obj);
  if (obj^.ref_count>1) then
  begin
   vm_object_clear_flag(obj, OBJ_ONEMAPPING);
  end;
  VM_OBJECT_UNLOCK(obj);
 end else
 if ((prev_entry<>@map^.header) and
   (prev_entry^.eflags=protoeflags) and
   ((cow and (MAP_ENTRY_GROWS_DOWN or MAP_ENTRY_GROWS_UP or MAP_COW_NO_COALESCE))=0) and
   (prev_entry^.__end=start) and
   (prev_entry^.budget_id=p_proc.p_budget_ptype) and
     vm_object_coalesce(prev_entry^.vm_obj,
         prev_entry^.offset,
         vm_size_t(prev_entry^.__end - prev_entry^.start),
         vm_size_t(__end - prev_entry^.__end), charge_prev_obj)) then
 begin
  {
   * We were able to extend the object.  Determine if we
   * can extend the previous map entry to include the
   * new range as well.
   }
  if ((prev_entry^.inheritance=inheritance) and
      (prev_entry^.protection=prot) and
      (prev_entry^.max_protection=max)) then
  begin
   map^.size:=map^.size+(__end - prev_entry^.__end);
   prev_entry^.__end:=__end;
   //change size

   Result:=vm_map_insert_internal(
              map   ,
              obj   ,
              offset,
              start ,
              __end ,
              prot  ,
              max   ,
              cow   ,
              alias);

   if (Result=KERN_SUCCESS) then
   begin
    vm_map_entry_resize_free(map, prev_entry);
    vm_map_simplify_entry(map, prev_entry);
   end;

   Exit;
  end;

  {
   * If we can extend the object but cannot extend the
   * map entry, we have to create a new map entry.  We
   * must bump the ref count on the extended object to
   * account for it.  object may be nil.
   }
  obj:=prev_entry^.vm_obj;
  offset:=prev_entry^.offset + (prev_entry^.__end - prev_entry^.start);
  vm_object_reference(obj);
  if (obj<>nil) and
     ((prev_entry^.eflags and MAP_ENTRY_NEEDS_COPY)=0) then
  begin
   { Object already accounts for this uid. }
  end;
 end;

 {
  * NOTE: if conditionals fail, object can be nil here.  This occurs
  * in things like the buffer map where we manage kva but do not manage
  * backing objects.
  }

 {
  * Create a new entry
  }
 new_entry:=vm_map_entry_create(map);
 new_entry^.start:=start;
 new_entry^.__end:=__end;

 new_entry^.eflags:=protoeflags;
 new_entry^.vm_obj:=obj;
 new_entry^.offset:=offset;
 new_entry^.avail_ssize:=0;

 new_entry^.inheritance:=inheritance;
 new_entry^.protection:=prot;
 new_entry^.max_protection:=max;

 //new_entry^.wired_count = 0;
 new_entry^.budget_id:=p_proc.p_budget_ptype;

 new_entry^.entry_id:=map^.entry_id;
 Inc(map^.entry_id);

 {
  * Insert the new entry into the list
  }
 vm_map_entry_link(map, prev_entry, new_entry);
 map^.size:=map^.size+(new_entry^.__end - new_entry^.start);

 {
  * It may be possible to merge the new entry with the next and/or
  * previous entries.  However, due to MAP_STACK_* being a hack, a
  * panic can result from merging such entries.
  }
 if ((cow and (MAP_STACK_GROWS_DOWN or MAP_STACK_GROWS_UP or MAP_COW_NO_COALESCE))=0) then
 begin
  vm_map_simplify_entry(map, new_entry);
 end;

 Result:=vm_map_insert_internal(
            map   ,
            obj   ,
            offset,
            start ,
            __end ,
            prot  ,
            max   ,
            cow   ,
            alias);

 if (Result<>KERN_SUCCESS) then
 begin
  vm_map_delete_internal(map,new_entry,__end);
 end;
end;

{
 * vm_map_findspace:
 *
 * Find the first fit (lowest VM address) for "length" free bytes
 * beginning at address>=start in the given map.
 *
 * In a vm_map_entry, "adj_free" is the amount of free space
 * adjacent (higher address) to this entry, and "max_free" is the
 * maximum amount of contiguous free space in its subtree.  This
 * allows finding a free region in one path down the tree, so
 * O(log n) amortized with splay trees.
 *
 * The map must be locked, and leaves it so.
 *
 * Returns: 0 on success, and starting address in *addr,
 *   1 if insufficient space.
 }
function vm_map_findspace(map   :vm_map_t;
                          start :vm_offset_t;
                          length:vm_size_t;
                          addr  :p_vm_offset_t):Integer;
label
 _nxt;
var
 entry:vm_map_entry_t;
 st:vm_offset_t;
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
 map^.root:=vm_map_entry_splay(start, map^.root);
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
  st:=map^.root^.__end
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
 Assert(false,'vm_map_findspace: max_free corrupt');
end;

function vm_map_fixed(map    :vm_map_t;
                      vm_obj :vm_object_t;
                      offset :vm_ooffset_t;
                      start  :vm_offset_t;
                      length :vm_size_t;
                      prot   :vm_prot_t;
                      max    :vm_prot_t;
                      cow    :Integer;
                      overwr :Integer):Integer;
var
 __end:vm_offset_t;
begin
 __end:=start + length;
 vm_map_lock(map);
  VM_MAP_RANGE_CHECK(map, start, __end);
  if (overwr<>0) then
  begin
   vm_map_delete(map, start, __end, True);
  end;
  Result:=vm_map_insert(map, vm_obj, offset, start, __end, prot, max, cow, false);
 vm_map_unlock(map);
end;

{
 * vm_map_find finds an unallocated region in the target address
 * map with the given length.  The search is defined to be
 * first-fit from the specified address; the region found is
 * returned in the same parameter.
 *
 * If object is non-nil, ref count must be bumped by caller
 * prior to making call to account for the new entry.
 }
function vm_map_find(map       :vm_map_t;
                     vm_obj    :vm_object_t;
                     offset    :vm_ooffset_t;
                     addr      :p_vm_offset_t;
                     length    :vm_size_t;
                     find_space:Integer;
                     prot      :vm_prot_t;
                     max       :vm_prot_t;
                     cow       :Integer):Integer;
label
 again;
var
 alignment,initial_addr,start:vm_offset_t;
begin
 if (find_space=VMFS_OPTIMAL_SPACE) then
 begin
  if (vm_obj=nil) then
  begin
   find_space:=VMFS_ANY_SPACE;
  end else
  if ((vm_obj^.flags and OBJ_COLORED)=0) then
  begin
   find_space:=VMFS_ANY_SPACE;
  end;
 end;
 if ((find_space shr 8)<>0) then
 begin
  Assert((find_space and $ff)=0,'bad VMFS flags');
  alignment:=vm_offset_t(1) shl (find_space shr 8);
 end else
 begin
  alignment:=0;
 end;
 initial_addr:=addr^;
again:
 start:=initial_addr;
 vm_map_lock(map);
 repeat
  if (find_space<>VMFS_NO_SPACE) then
  begin
   if (vm_map_findspace(map, start, length, addr)<>0) then
   begin
    vm_map_unlock(map);
    if (find_space=VMFS_OPTIMAL_SPACE) then
    begin
     find_space:=VMFS_ANY_SPACE;
     goto again;
    end;
    Exit(KERN_NO_SPACE);
   end;

   case find_space of
    VMFS_SUPER_SPACE,
    VMFS_OPTIMAL_SPACE: pmap_align_superpage(vm_obj, offset, addr, length);
    VMFS_ANY_SPACE:;
   else
    if ((addr^ and (alignment - 1))<>0) then
    begin
     addr^:=addr^ and (not (alignment - 1));
     addr^:=addr^ + alignment;
    end;
   end;

   start:=addr^;
  end;
  Result:=vm_map_insert(map, vm_obj, offset, start, start + length, prot, max, cow, false);
 until not ((Result=KERN_NO_SPACE) and
            (find_space<>VMFS_NO_SPACE) and
            (find_space<>VMFS_ANY_SPACE));
 vm_map_unlock(map);
end;

{
 * vm_map_simplify_entry:
 *
 * Simplify the given map entry by merging with either neighbor.  This
 * routine also has the ability to merge with both neighbors.
 *
 * The map must be locked.
 *
 * This routine guarentees that the passed entry remains valid (though
 * possibly extended).  When merging, this routine may delete one or
 * both neighbors.
 }
procedure vm_map_simplify_entry(map:vm_map_t;entry:vm_map_entry_t);
var
 next,prev:vm_map_entry_t;
 prevsize,esize:vm_size_t;
 obj:vm_map_object;
 sdk_5:Boolean;
begin
 if ((entry^.eflags and (MAP_ENTRY_IS_SUB_MAP or $20000 or MAP_ENTRY_IN_TRANSITION))<>0) or
    (entry^.inheritance=VM_INHERIT_HOLE) then
 begin
  Exit;
 end;

 obj:=entry^.vm_obj;

 if (obj<>nil) then
 begin
  if (p_proc.p_sdk_version<=$1ffffff) and
     ((obj^.flags and OBJ_DMEM_EXT)<>0) then
  begin
   Exit;
  end;
  if (obj^.otype=OBJT_BLOCKPOOL) then
  begin
   Exit;
  end;
 end;

 sdk_5:=(p_proc.p_sdk_version>$54fffff);

 prev:=entry^.prev;
 if (prev<>@map^.header) then
 begin
  prevsize:=prev^.__end - prev^.start;
  if (prev^.__end=entry^.start) and
     (prev^.vm_obj=obj) and
     ((prev^.vm_obj=nil) or (prev^.offset + prevsize=entry^.offset)) and
     (prev^.eflags=entry^.eflags) and
     (prev^.protection=entry^.protection) and
     (prev^.max_protection=entry^.max_protection) and
     (prev^.inheritance=entry^.inheritance) and
     (prev^.budget_id=entry^.budget_id) and
     (sdk_5 or (prev^.anon_addr=entry^.anon_addr)) and
     (((prev^.eflags and MAP_ENTRY_NO_COALESCE)=0) or (prev^.entry_id=entry^.entry_id))
     then
  begin
   if (strlcomp(pchar(@prev^.name),pchar(@entry^.name),32)=0) then
   begin
    vm_map_entry_unlink(map, prev);
    entry^.start:=prev^.start;
    entry^.offset:=prev^.offset;
    //change
    if (entry^.prev<>@map^.header) then
    begin
     vm_map_entry_resize_free(map, entry^.prev);
    end;

    {
     * If the backing object is a vnode object,
     * vm_object_deallocate() calls vrele().
     * However, vrele() does not lock the vnode
     * because the vnode has additional
     * references.  Thus, the map lock can be kept
     * without causing a lock-order reversal with
     * the vnode lock.
     *
     * Since we count the number of virtual page
     * mappings in object^.un_pager.vnp.writemappings,
     * the writemappings value should not be adjusted
     * when the entry is disposed of.
     }
    vm_object_deallocate(prev^.vm_obj);
    vm_map_entry_dispose(map, prev);
   end;
  end;
 end;

 next:=entry^.next;
 if (next<>@map^.header) then
 begin
  esize:=entry^.__end - entry^.start;
  if (entry^.__end=next^.start) and
     (next^.vm_obj=obj) and
     ((obj=nil) or (entry^.offset + esize=next^.offset)) and
     (next^.eflags=entry^.eflags) and
     (next^.protection=entry^.protection) and
     (next^.max_protection=entry^.max_protection) and
     (next^.inheritance=entry^.inheritance) and
     (next^.budget_id=entry^.budget_id) and
     (sdk_5 or (next^.anon_addr=entry^.anon_addr)) and
     (((entry^.eflags and MAP_ENTRY_NO_COALESCE)=0) or (next^.entry_id=entry^.entry_id))
     then
  begin
   if (strlcomp(pchar(@next^.name),pchar(@entry^.name),32)=0) then
   begin
    vm_map_entry_unlink(map, next);
    entry^.__end:=next^.__end;
    //change
    vm_map_entry_resize_free(map, entry);

    vm_object_deallocate(next^.vm_obj);
    vm_map_entry_dispose(map, next);
   end;
  end;
 end;
end;

{
 * This routine is called only when it is known that
 * the entry must be split.
 }
procedure _vm_map_clip_start(map:vm_map_t;entry:vm_map_entry_t;start:vm_offset_t);
var
 new_entry:vm_map_entry_t;
begin
 VM_MAP_ASSERT_LOCKED(map);

 {
  * Split off the front portion -- note that we must insert the new
  * entry BEFORE this one, so that this entry has the specified
  * starting address.
  }
 vm_map_simplify_entry(map, entry);

 new_entry:=vm_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.__end:=start;
 entry^.offset:=entry^.offset + (start - entry^.start);
 entry^.start:=start;

 vm_map_entry_link(map, entry^.prev, new_entry);

 if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0) and
    (entry^.inheritance<>VM_INHERIT_HOLE) then
 begin
  vm_object_reference(new_entry^.vm_obj);
 end;
end;

{
 * vm_map_clip_start: [ internal use only ]
 *
 * Asserts that the given entry begins at or after
 * the specified address; if necessary,
 * it splits the entry into two.
 }
procedure vm_map_clip_start(map:vm_map_t;entry:vm_map_entry_t;start:vm_offset_t);
var
 obj:vm_object_t;
begin
 obj:=entry^.vm_obj;

 if (obj<>nil) then
 if (obj^.otype=OBJT_BLOCKPOOL) then Exit;

 if (start>entry^.start) then
 begin
  _vm_map_clip_start(map,entry,start);
 end;
end;

{
 * This routine is called only when it is known that
 * the entry must be split.
 }

procedure _vm_map_clip_end(map:vm_map_t;entry:vm_map_entry_t;__end:vm_offset_t);
var
 new_entry:vm_map_entry_t;
begin
 VM_MAP_ASSERT_LOCKED(map);

 {
  * Create a new entry and insert it AFTER the specified entry
  }
 new_entry:=vm_map_entry_create(map);
 new_entry^:=entry^;

 new_entry^.start:=__end;
 entry^.__end:=__end;
 new_entry^.offset:=new_entry^.offset + (__end - entry^.start);

 vm_map_entry_link(map, entry, new_entry);

 if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0) and
    (entry^.inheritance<>VM_INHERIT_HOLE) then
 begin
  vm_object_reference(new_entry^.vm_obj);
 end;
end;

{
 * vm_map_clip_end: [ internal use only ]
 *
 * Asserts that the given entry ends at or before
 * the specified address; if necessary,
 * it splits the entry into two.
 }
procedure vm_map_clip_end(map:vm_map_t;entry:vm_map_entry_t;__end:vm_offset_t);
begin
 if (__end<entry^.__end) then
 begin
  _vm_map_clip_end(map,entry,__end);
 end;
end;

{
 * vm_map_protect:
 *
 * Sets the protection of the specified address
 * region in the target map.  If "set_max" is
 * specified, the maximum protection is to be set;
 * otherwise, only the current protection is affected.
 }
function vm_map_protect(map     :vm_map_t;
                        start   :vm_offset_t;
                        __end   :vm_offset_t;
                        new_prot:vm_prot_t;
                        set_max :Boolean):Integer;

 function MASK(entry:vm_map_entry_t):vm_eflags_t; inline;
 begin
  if ((entry^.eflags and MAP_ENTRY_COW)<>0) then
   Result:=(not VM_PROT_WRITE)
  else
   Result:=VM_PROT_ALL;
 end;

label
 _continue;
var
 current,entry:vm_map_entry_t;
 obj:vm_object_t;
 old_prot:vm_prot_t;
 ext_flags:Byte;
begin
 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 vm_map_lock(map);

 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_map_lookup_entry(map, start, @entry)) then
 begin
  vm_map_clip_start(map, entry, start);
 end else
 begin
  entry:=entry^.next;
 end;

 {
  * Make a first pass to check for protection violations.
  }
 current:=entry;
 while ((current<>@map^.header) and (current^.start<__end)) do
 begin

  if ((current^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) or
     (current^.inheritance=VM_INHERIT_HOLE) then
  begin
   vm_map_unlock(map);
   Exit(KERN_INVALID_ARGUMENT);
  end;

  if set_max then
  begin
   obj:=current^.vm_obj;

   if (obj<>nil) then
   if (obj^.otype=OBJT_BLOCKPOOL) then
   begin
    vm_map_unlock(map);
    Exit(KERN_INVALID_ARGUMENT);
   end;
  end;

  ext_flags:=2; //current^.ext_flags

  if ((ext_flags and 2)<>0) then
  begin
   old_prot:=current^.max_protection;
  end else
  begin
   old_prot:=current^.max_protection and VM_PROT_GPU_ALL;
  end;

  if ((ext_flags and 1)<>0) then
  begin
   old_prot:=0;
  end;

  if ((new_prot and old_prot)<>new_prot) then
  begin
   vm_map_unlock(map);
   Exit(KERN_PROTECTION_FAILURE);
  end;

  current:=current^.next;
 end;

 {
  * Do an accounting pass for private read-only mappings that
  * now will do cow due to allowed write (e.g. debugger sets
  * breakpoint on text segment)
  }
 current:=entry;
 while (current<>@map^.header) and (current^.start<__end) do
 begin

  vm_map_clip_end(map, current, __end);

  if set_max or
     (((new_prot and (not current^.protection)) and VM_PROT_WRITE)=0) or
     ENTRY_CHARGED(current) then
  begin
   goto _continue;
  end;

  obj:=current^.vm_obj;

  if (obj=nil) or ((current^.eflags and MAP_ENTRY_NEEDS_COPY)<>0) then
  begin
   //swap_reserve
   goto _continue;
  end;

  VM_OBJECT_LOCK(obj);
  if (obj^.otype<>OBJT_DEFAULT) then
  begin
   VM_OBJECT_UNLOCK(obj);
   goto _continue;
  end;

  VM_OBJECT_UNLOCK(obj);

  _continue:
  current:=current^.next;
 end;

 {
  * Go back and fix up protections. [Note that clipping is not
  * necessary the second time.]
  }
 current:=entry;
 while ((current<>@map^.header) and (current^.start<__end)) do
 begin
  old_prot:=current^.protection;

  if set_max then
  begin
   current^.protection:=new_prot and old_prot;
   current^.max_protection:=current^.protection;
  end else
  begin
   current^.protection:=new_prot;
  end;

  if ((current^.eflags and (MAP_ENTRY_COW or MAP_ENTRY_USER_WIRED))=(MAP_ENTRY_COW or MAP_ENTRY_USER_WIRED)) and
     ((current^.protection and VM_PROT_WRITE)<>0) and
     ((old_prot and VM_PROT_WRITE)=0) then
  begin
   //vm_fault_copy_entry(map, map, current, current, nil);
  end;

  {
   * When restricting access, update the physical map.  Worry
   * about copy-on-write here.
   }
  if (old_prot<>current^.protection) then
  begin
   pmap_protect(map^.pmap,
                current^.vm_obj,
                current^.start,
                current^.__end,
                current^.protection and MASK(current));
  end;
  vm_map_simplify_entry(map, current);
  current:=current^.next;
 end;

 vm_map_unlock(map);
 Result:=(KERN_SUCCESS);
end;

{
 * vm_map_madvise:
 *
 * This routine traverses a processes map handling the madvise
 * system call.  Advisories are classified as either those effecting
 * the vm_map_entry structure, or those effecting the underlying
 * objects.
 }
function vm_map_madvise(map  :vm_map_t;
                        start:vm_offset_t;
                        __end:vm_offset_t;
                        behav:Integer):Integer;
var
 current,entry:vm_map_entry_t;
 modify_map:Integer;
 pstart,pend:vm_pindex_t;
 useStart:vm_offset_t;
begin
 modify_map:=0;

 {
  * Some madvise calls directly modify the vm_map_entry, in which case
  * we need to use an exclusive lock on the map and we need to perform
  * various clipping operations.  Otherwise we only need a read-lock
  * on the map.
  }
 case behav of
  MADV_NORMAL,
  MADV_SEQUENTIAL,
  MADV_RANDOM,
  MADV_NOSYNC,
  MADV_AUTOSYNC,
  MADV_NOCORE,
  MADV_CORE:
   begin
    if (start=__end) then
    begin
     Exit(KERN_SUCCESS);
    end;
    modify_map:=1;
   end;
  MADV_WILLNEED,
  MADV_DONTNEED,
  MADV_FREE:
  begin
   if (start=__end) then
   begin
    Exit(KERN_SUCCESS);
   end;
  end;
 else
  Exit(KERN_INVALID_ARGUMENT);
 end;

 vm_map_lock(map);

 {
  * Locate starting entry and clip if necessary.
  }
 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_map_lookup_entry(map,start,@entry)) then
 begin
  if (modify_map<>0) then
  begin
   vm_map_clip_start(map, entry, start);
  end;
 end else
 begin
  entry:=entry^.next;
 end;

 if (modify_map<>0) then
 begin
  {
   * madvise behaviors that are implemented in the vm_map_entry.
   *
   * We clip the vm_map_entry so that behavioral changes are
   * limited to the specified address range.
   }
  current:=entry;
  while (current<>@map^.header) and (current^.start<__end) do
  begin
   if ((current^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) or
      (current^.inheritance=VM_INHERIT_HOLE) then
   begin
    current:=current^.next;
    continue;
   end;

   vm_map_clip_end(map, current, __end);

   case behav of
    MADV_NORMAL:
     begin
      vm_map_entry_set_behavior(current, MAP_ENTRY_BEHAV_NORMAL);
     end;
    MADV_SEQUENTIAL:
     begin
      vm_map_entry_set_behavior(current, MAP_ENTRY_BEHAV_SEQUENTIAL);
     end;
    MADV_RANDOM:
     begin
      vm_map_entry_set_behavior(current, MAP_ENTRY_BEHAV_RANDOM);
     end;
    MADV_NOSYNC:
     begin
      current^.eflags:=current^.eflags or MAP_ENTRY_NOSYNC;
     end;
    MADV_AUTOSYNC:
     begin
      current^.eflags:=current^.eflags and (not MAP_ENTRY_NOSYNC);
     end;
    MADV_NOCORE:
     begin
      current^.eflags:=current^.eflags or MAP_ENTRY_NOCOREDUMP;
     end;
    MADV_CORE:
     begin
      current^.eflags:=current^.eflags and (not MAP_ENTRY_NOCOREDUMP);
     end;
    else;
   end;

   vm_map_simplify_entry(map, current);

   current:=current^.next;
  end;

  vm_map_unlock(map);
 end else
 begin
  {
   * madvise behaviors that are implemented in the underlying
   * vm_object.
   *
   * Since we don't clip the vm_map_entry, we have to clip
   * the vm_object pindex and count.
   }
  current:=entry;
  while (current<>@map^.header) and (current^.start<__end) do
  begin
   if ((current^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) or
      (current^.inheritance=VM_INHERIT_HOLE) then
   begin
    current:=current^.next;
    continue;
   end;

   pstart:=OFF_TO_IDX(current^.offset);
   pend  :=pstart + atop(current^.__end - current^.start);
   useStart:=current^.start;

   if (current^.start<start) then
   begin
    pstart:=pstart+atop(start - current^.start);
    useStart:=start;
   end;

   if (current^.__end>__end) then
   begin
    pend:=pend-atop(current^.__end - __end);
   end;

   if (pstart>=pend) then
   begin
    current:=current^.next;
    continue;
   end;

   vm_object_madvise(map^.pmap,
                     current^.vm_obj,
                     useStart,
                     useStart+ptoa(pend-pstart),
                     behav);

   if (behav=MADV_WILLNEED) then
   begin
    //re enter?
   end;

   current:=current^.next;
  end;

  vm_map_unlock(map);
 end;
 Result:=(0);
end;


{
 * vm_map_inherit:
 *
 * Sets the inheritance of the specified address
 * range in the target map.  Inheritance
 * affects how the map will be shared with
 * child maps at the time of vmspace_fork.
 }
function vm_map_inherit(map            :vm_map_t;
                        start          :vm_offset_t;
                        __end          :vm_offset_t;
                        new_inheritance:vm_inherit_t
                        ):Integer;
var
 entry     :vm_map_entry_t;
 temp_entry:vm_map_entry_t;
begin
 case new_inheritance of
  VM_INHERIT_SHARE,
  VM_INHERIT_COPY ,
  VM_INHERIT_NONE ,
  VM_INHERIT_PATCH,
  VM_INHERIT_HOLE :;
 else
  Exit(KERN_INVALID_ARGUMENT);
 end;

 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 vm_map_lock(map);
 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_map_lookup_entry(map, start, @temp_entry)) then
 begin
  entry:=temp_entry;
  vm_map_clip_start(map, entry, start);
 end else
 begin
  entry:=temp_entry^.next;
 end;

 while ((entry<>@map^.header) and (entry^.start<__end)) do
 begin
  vm_map_clip_end(map, entry, __end);
  entry^.inheritance:=new_inheritance;
  vm_map_simplify_entry(map, entry);
  entry:=entry^.next;
 end;

 vm_map_unlock(map);
 Result:=(KERN_SUCCESS);
end;

{
 * vm_map_sync
 *
 * Push any dirty cached pages in the address range to their pager.
 * If syncio is TRUE, dirty pages are written synchronously.
 * If invalidate is TRUE, any cached pages are freed as well.
 *
 * If the size of the region from start to __end is zero, we are
 * supposed to flush all modified pages within the region containing
 * start.  Unfortunately, a region can be split or coalesced with
 * neighboring regions, making it difficult to determine what the
 * original region was.  Therefore, we approximate this requirement by
 * flushing the current region containing start.
 *
 * Returns an error if any part of the specified range is not mapped.
 }
function vm_map_sync(map       :vm_map_t;
                     start     :vm_offset_t;
                     __end     :vm_offset_t;
                     syncio    :Boolean;
                     invalidate:Boolean):Integer;
var
 current:vm_map_entry_t;
 entry  :vm_map_entry_t;
 size   :vm_size_t;
 obj    :vm_object_t;
 offset :vm_ooffset_t;
 last_timestamp:DWORD;
 failed:Boolean;

 smap:vm_map_t;
 tentry:vm_map_entry_t;
 tsize:vm_size_t;
begin
 vm_map_lock(map);
 VM_MAP_RANGE_CHECK(map, start, __end);
 if (not vm_map_lookup_entry(map, start, @entry)) then
 begin
  vm_map_unlock(map);
  Exit(KERN_INVALID_ADDRESS);
 end else
 if (start=__end) then
 begin
  start:=entry^.start;
  __end:=entry^.__end;
 end;

 {
  * Make a first pass to check for user-wired memory and holes.
  }
 current:=entry;
 while (current<>@map^.header) and (current^.start<__end) do
 begin
  if invalidate and
     ((current^.eflags and MAP_ENTRY_USER_WIRED)<>0) then
  begin
   vm_map_unlock(map);
   Exit(KERN_INVALID_ARGUMENT);
  end;

  if (__end>current^.__end) and
     ((current^.next=@map^.header) or
      (current^.__end<>current^.next^.start)) then
  begin
   vm_map_unlock(map);
   Exit(KERN_INVALID_ADDRESS);
  end;

  current:=current^.next;
 end;

 if invalidate then
 begin
  md_cacheflush(Pointer(start),__end-start,ICACHE or DCACHE);
  //pmap_remove(map^.pmap, start, end);
 end;

 failed:=FALSE;

 {
  * Make a second pass, cleaning/uncaching pages from the indicated
  * objects as we go.
  }
 current:=entry;
 while (current<>@map^.header) and (current^.start<__end) do
 begin

  if (current^.inheritance=VM_INHERIT_HOLE) then
  begin
   current:=current^.next;
   continue;
  end;

  offset:=current^.offset + (start - current^.start);

  if (__end<=current^.__end) then
   size:=__end-start
  else
   size:=current^.__end-start;

  if ((current^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) then
  begin
   smap:=vm_map_t(current^.vm_obj);
   vm_map_lock(smap);
   vm_map_lookup_entry(smap, offset, @tentry);
   tsize:=tentry^.__end - offset;
   if (tsize<size) then
   begin
    size:=tsize;
   end;
   obj:=tentry^.vm_obj;
   offset:=tentry^.offset + (offset - tentry^.start);
   vm_map_unlock(smap);
  end else
  begin
   obj:=current^.vm_obj;
  end;

  vm_object_reference(obj);
  last_timestamp:=map^.timestamp;
  vm_map_unlock(map);

  if (not vm_object_sync(obj, offset, size, syncio, invalidate)) then
  begin
   failed:=TRUE;
  end;

  start:=start+size;
  vm_object_deallocate(obj);

  vm_map_lock(map,False);
  if (last_timestamp=map^.timestamp) or
     (not vm_map_lookup_entry(map, start, @current)) then
  begin
   current:=current^.next;
  end;
 end;

 vm_map_unlock(map);

 case failed of
  True :Result:=KERN_FAILURE;
  FAlse:Result:=KERN_SUCCESS;
 end;
end;

procedure vm_map_entry_deallocate(entry:vm_map_entry_t);
begin
 if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0) then
 begin
  vm_object_deallocate(entry^.vm_obj);
 end;
 Freemem(entry);
end;

{
 * vm_map_entry_delete: [ internal use only ]
 *
 * Deallocate the given entry from the target map.
 }
procedure vm_map_entry_delete(map:vm_map_t;entry:vm_map_entry_t);
var
 obj:vm_object_t;
 offidxstart,offidx_end,count:vm_pindex_t;
 size:vm_ooffset_t;
 budget_id:shortint;
begin

 vm_map_entry_unlink(map, entry);
 obj:=entry^.vm_obj;
 size:=entry^.__end - entry^.start;
 map^.size:=map^.size-size;

 //budget
 budget_id:=entry^.budget_id;
 if (budget_id<>-1) and
    ((entry^.eflags and (MAP_ENTRY_IN_BUDGET or $40000))=MAP_ENTRY_IN_BUDGET) then
 begin
  entry^.eflags:=entry^.eflags and $ffdfffff;
  vm_budget_release(budget_id,field_malloc,size);
 end;
 //

 if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)=0) and
    (obj<>nil) then
 if (obj^.otype<>OBJT_BLOCKPOOL) then
 begin
  count:=OFF_TO_IDX(size);
  offidxstart:=OFF_TO_IDX(entry^.offset);
  offidx_end:=offidxstart + count;
  VM_OBJECT_LOCK(obj);
  if (obj^.ref_count<>1) and
      (((obj^.flags and (OBJ_NOSPLIT or OBJ_ONEMAPPING))=OBJ_ONEMAPPING)) then
  begin
   vm_object_collapse(obj);

   {
    * The option OBJPR_NOTMAPPED can be passed here
    * because vm_map_delete() already performed
    * pmap_remove() on the only mapping to this range
    * of pages.
    }
   vm_object_page_remove(obj, offidxstart, offidx_end, OBJPR_NOTMAPPED);

   if (offidx_end>=obj^.size) and
      (offidxstart<obj^.size) then
   begin
    obj^.size:=offidxstart;
   end;
  end;
  VM_OBJECT_UNLOCK(obj);
 end else
 begin
  entry^.vm_obj:=nil;
 end;

 begin
  entry^.next:=curkthread^.td_map_def_user;
  curkthread^.td_map_def_user:=entry;
 end;
end;

procedure vm_map_delete_internal(map:vm_map_t;entry:vm_map_entry_t;__end:vm_offset_t);
var
 next:vm_map_entry_t;
begin
 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  if (entry^.inheritance=VM_INHERIT_HOLE) then
  begin
   entry:=entry^.next;
   continue;
  end;

  vm_map_clip_end(map, entry, __end);

  next:=entry^.next;

  vm_map_entry_delete(map, entry);
  entry:=next;
 end;
end;

//

//procedure unmap_jit_cache(start,__end:QWORD); external name 'kern_unmap_jit_cache';

//

{
 * vm_map_delete: [ internal use only ]
 *
 * Deallocates the given address range from the target
 * map.
 }
function vm_map_delete(map:vm_map_t;start:vm_offset_t;__end:vm_offset_t;rmap_free:Boolean):Integer;
var
 entry      :vm_map_entry_t;
 first_entry:vm_map_entry_t;
 next       :vm_map_entry_t;
 obj        :vm_object_t;
begin
 VM_MAP_ASSERT_LOCKED(map);

 if (start=__end) then
 begin
  Exit(KERN_SUCCESS);
 end;

 {
  * Find the start of the region, and clip it
  }
 if (not vm_map_lookup_entry(map, start, @first_entry)) then
 begin
  entry:=first_entry^.next;
 end else
 begin
  entry:=first_entry;

  if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) then
  begin
   Exit(KERN_INVALID_ARGUMENT);
  end;

  obj:=entry^.vm_obj;

  if (obj<>nil) then
  if (obj^.otype=OBJT_BLOCKPOOL) then
  begin
   Exit(KERN_INVALID_ARGUMENT);
  end;

  vm_map_clip_start(map, entry, start);
 end;

 //check
 next:=entry;
 while (next<>@map^.header) and (next^.start<__end) do
 begin

  if ((next^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) then
  begin
   Exit(KERN_INVALID_ARGUMENT);
  end;

  obj:=next^.vm_obj;

  if (obj<>nil) then
  if (obj^.otype=OBJT_BLOCKPOOL) then
  begin
   Exit(KERN_INVALID_ARGUMENT);
  end;

  if (next^.inheritance=VM_INHERIT_HOLE) then
  begin
   next:=next^.next;
   continue;
  end;

  next:=entry^.next;
 end;

 {
  * Step through all entries in this region
  }
 while (entry<>@map^.header) and (entry^.start<__end) do
 begin

  if (entry^.inheritance=VM_INHERIT_HOLE) then
  begin
   entry:=entry^.next;
   continue;
  end;

  vm_map_clip_end(map, entry, __end);

  next:=entry^.next;

  if rmap_free and (obj<>nil) then
  begin
   if ((obj^.flags and OBJ_DMEM_EXT)<>0) or
      (obj^.otype=OBJT_PHYSHM) then
   begin
    Result:=vm_object_rmap_release(map,
                                   obj,
                                   entry^.start,
                                   entry^.__end,
                                   entry^.offset);
   end;
  end;

  pmap_remove(map^.pmap,
              entry^.vm_obj,
              entry^.start,
              entry^.__end);

  //unmap_jit_cache(entry^.start,entry^.__end);

  //if (entry^.wired_count<>0) then
  //begin
  // vm_map_entry_unwire(map,entry);
  //end;

  {
   * Delete the entry only after removing all pmap
   * entries pointing to its pages.  (Otherwise, its
   * page frames may be reallocated, and any modify bits
   * will be set in the wrong object!)
   }
  vm_map_entry_delete(map, entry);

  entry:=next;
 end;
 Result:=(KERN_SUCCESS);
end;

{
 * vm_map_remove:
 *
 * Remove the given address range from the target map.
 * This is the exported form of vm_map_delete.
 }
function vm_map_remove(map:vm_map_t;start:vm_offset_t;__end:vm_offset_t):Integer;
begin
 vm_map_lock(map);
 VM_MAP_RANGE_CHECK(map, start, __end);
  Result:=vm_map_delete(map, start, __end, True);
 vm_map_unlock(map);
end;

{
 * vm_map_check_protection:
 *
 * Assert that the target map allows the specified privilege on the
 * entire address region given.  The entire region must be allocated.
 *
 * WARNING!  This code does not and should not check whether the
 * contents of the region is accessible.  For example a smaller file
 * might be mapped into a larger address space.
 *
 * NOTE!  This code is also called by munmap().
 *
 * The map must be locked.  A read lock is sufficient.
 }
function vm_map_check_protection(map:vm_map_t;
                                 start:vm_offset_t;
                                 __end:vm_offset_t;
                                 protection:vm_prot_t):boolean;
var
 entry    :vm_map_entry_t;
 tmp_entry:vm_map_entry_t;
begin
 if (not vm_map_lookup_entry(map, start, @tmp_entry)) then
 begin
  Exit(FALSE);
 end;

 entry:=tmp_entry;

 while (start<__end) do
 begin
  if (entry=@map^.header) then
  begin
   Exit (FALSE);
  end;
  {
   * No holes allowed!
   }
  if (start<entry^.start) then
  begin
   Exit(FALSE);
  end;
  {
   * Check protection associated with entry.
   }
  if ((entry^.protection and protection)<>protection) then
  begin
   Exit(FALSE);
  end;
  { go to next entry }
  start:=entry^.__end;
  entry:=entry^.next;
 end;
 Exit(TRUE);
end;

function vm_map_stack(map      :vm_map_t;
                      addrbos  :vm_offset_t;
                      max_ssize:vm_size_t;
                      prot     :vm_prot_t;
                      max      :vm_prot_t;
                      cow      :Integer):Integer;
var
 new_entry, prev_entry:vm_map_entry_t;
 bot, top:vm_offset_t;
 growsize, init_ssize:vm_size_t;
 orient, rv:Integer;
 vmemlim:QWORD;
begin
 {
  * The stack orientation is piggybacked with the cow argument.
  * Extract it into orient and mask the cow argument so that we
  * don't pass it around further.
  * NOTE: We explicitly allow bi-directional stacks.
  }
 orient:=cow and (MAP_STACK_GROWS_DOWN or MAP_STACK_GROWS_UP);
 Assert(orient<>0,'No stack grow direction');

 if (addrbos<vm_map_min(map)) or
    (addrbos>vm_map_max(map)) or
    (addrbos + max_ssize<addrbos) then
 begin
  Exit(KERN_NO_SPACE);
 end;

 growsize:=sgrowsiz;

 {
 if (max_ssize<growsize) then
  init_ssize:=max_ssize
 else
  init_ssize:=growsize;
 }

 //No need to trim the stack
 init_ssize:=max_ssize;

 vmemlim:=lim_cur(RLIMIT_VMEM);

 vm_map_lock(map);

 { If addr is already mapped, no go }
 if (vm_map_lookup_entry(map, addrbos, @prev_entry)) then
 begin
  vm_map_unlock(map);
  Exit(KERN_NO_SPACE);
 end;

 { If we would blow our VMEM resource limit, no go }
 if (map^.size + init_ssize>vmemlim) then
 begin
  vm_map_unlock(map);
  Exit(KERN_NO_SPACE);
 end;

 {
  * If we can't accomodate max_ssize in the current mapping, no go.
  * However, we need to be aware that subsequent user mappings might
  * map into the space we have reserved for stack, and currently this
  * space is not protected.
  *
  * Hopefully we will at least detect this condition when we try to
  * grow the stack.
  }
 if (prev_entry^.next<>@map^.header) and
    (prev_entry^.next^.start<addrbos + max_ssize) then
 begin
  vm_map_unlock(map);
  Exit(KERN_NO_SPACE);
 end;

 {
  * We initially map a stack of only init_ssize.  We will grow as
  * needed later.  Depending on the orientation of the stack (i.e.
  * the grow direction) we either map at the top of the range, the
  * bottom of the range or in the middle.
  *
  * Note: we would normally expect prot and max to be VM_PROT_ALL,
  * and cow to be 0.  Possibly we should eliminate these as input
  * parameters, and just pass these values here in the insert call.
  }
 if (orient=MAP_STACK_GROWS_DOWN) then
 begin
  bot:=addrbos + max_ssize - init_ssize;
 end else
 if (orient=MAP_STACK_GROWS_UP) then
 begin
  bot:=addrbos;
 end else
 begin
  bot:=round_page(addrbos + (max_ssize div 2) - (init_ssize div 2));
 end;

 top:=bot + init_ssize;
 rv:=vm_map_insert(map, nil, 0, bot, top, prot, max, cow, false);

 { Now set the avail_ssize amount. }
 if (rv=KERN_SUCCESS) then
 begin
  if (prev_entry<>@map^.header) then
  begin
   vm_map_clip_end(map, prev_entry, bot);
  end;

  new_entry:=prev_entry^.next;
  if (new_entry^.__end<>top) or (new_entry^.start<>bot) then
  begin
   Assert(false,'Bad entry start/end for new stack entry');
  end;

  new_entry^.avail_ssize:=max_ssize - init_ssize;
  if ((orient and MAP_STACK_GROWS_DOWN)<>0) then
  begin
   new_entry^.eflags:=new_entry^.eflags or MAP_ENTRY_GROWS_DOWN;
  end;

  if ((orient and MAP_STACK_GROWS_UP)<>0) then
  begin
   new_entry^.eflags:=new_entry^.eflags or MAP_ENTRY_GROWS_UP;
  end;
 end;

 vm_map_unlock(map);
 Result:=(rv);
end;

{ Attempts to grow a vm stack entry.  Returns KERN_SUCCESS if the
 * desired address is already mapped, or if we successfully grow
 * the stack.  Also returns KERN_SUCCESS if addr is outside the
 * stack range (this is strange, but preserves compatibility with
 * the grow function in vm_machdep.c).
 }
function vm_map_growstack(map:vm_map_t;addr:vm_offset_t):Integer;
label
 _or,
 _out;
var
 next_entry, prev_entry:vm_map_entry_t;
 new_entry, stack_entry:vm_map_entry_t;
 __end:vm_offset_t;
 growsize:vm_size_t;
 grow_amount, max_grow:QWORD;
 stacklim, vmemlim:QWORD;
 is_procstack, rv:Integer;

 function _stack_guard_page:QWORD; inline;
 begin
  if (stack_guard_page<>0) then
   Result:=PAGE_SIZE
  else
   Result:=0;
 end;

begin
 stacklim:=lim_cur(RLIMIT_STACK);
 vmemlim :=lim_cur(RLIMIT_VMEM);

 vm_map_lock(map);

 { If addr is already in the entry range, no need to grow.}
 if (vm_map_lookup_entry(map, addr, @prev_entry)) then
 begin
  vm_map_unlock(map);
  Exit(KERN_SUCCESS);
 end;

 next_entry:=prev_entry^.next;
 if ((prev_entry^.eflags and MAP_ENTRY_GROWS_UP)=0) then
 begin
  {
   * This entry does not grow upwards. Since the address lies
   * beyond this entry, the next entry (if one exists) has to
   * be a downward growable entry. The entry list header is
   * never a growable entry, so it suffices to check the flags.
   }
  if ((next_entry^.eflags and MAP_ENTRY_GROWS_DOWN)=0) then
  begin
   vm_map_unlock(map);
   Exit(KERN_SUCCESS);
  end;
  stack_entry:=next_entry;
 end else
begin
  {
   * This entry grows upward. If the next entry does not at
   * least grow downwards, this is the entry we need to grow.
   * otherwise we have two possible choices and we have to
   * select one.
   }
  if ((next_entry^.eflags and MAP_ENTRY_GROWS_DOWN)<>0) then
  begin
   {
    * We have two choices; grow the entry closest to
    * the address to minimize the amount of growth.
    }
   if (addr - prev_entry^.__end<=next_entry^.start - addr) then
    stack_entry:=prev_entry
   else
    stack_entry:=next_entry;

  end else
  begin
   stack_entry:=prev_entry;
  end;
 end;

 if (stack_entry=next_entry) then
 begin
  Assert((stack_entry^.eflags and MAP_ENTRY_GROWS_DOWN<>0), 'foo');
  Assert(addr<stack_entry^.start, 'foo');

  if (prev_entry<>@map^.header) then
  begin
   __end:=prev_entry^.__end;
  end else
  begin
   __end:=stack_entry^.start - stack_entry^.avail_ssize;
  end;

  grow_amount:=round_page(stack_entry^.start - addr);
  max_grow:=stack_entry^.start - __end;
 end else
 begin
  Assert((stack_entry^.eflags and MAP_ENTRY_GROWS_UP)<>0,'foo');
  Assert(addr>=stack_entry^.__end, 'foo');

  if (next_entry<>@map^.header) then
  begin
   __end:=next_entry^.start;
  end else
  begin
   __end:=stack_entry^.__end + stack_entry^.avail_ssize;
  end;

  grow_amount:=round_page(addr + 1 - stack_entry^.__end);
  max_grow:=__end - stack_entry^.__end;
 end;

 if (grow_amount>stack_entry^.avail_ssize) then
 begin
  vm_map_unlock(map);
  Exit(KERN_NO_SPACE);
 end;

 {
  * If there is no longer enough space between the entries nogo, and
  * adjust the available space.  Note: this  should only happen if the
  * user has mapped into the stack area after the stack was created,
  * and is probably an error.
  *
  * This also effectively destroys any guard page the user might have
  * intended by limiting the stack size.
  }
 if (grow_amount + _stack_guard_page>max_grow) then
 begin
  stack_entry^.avail_ssize:=max_grow;

  vm_map_unlock(map);
  Exit(KERN_NO_SPACE);
 end;

 if (addr>=vm_offset_t(g_vmspace.vm_maxsaddr)) then
  is_procstack:=1
 else
  is_procstack:=0;

 {
  * If this is the main process stack, see if we're over the stack
  * limit.
  }
 if ((is_procstack<>0) and (ctob(g_vmspace.vm_ssize) + grow_amount>stacklim)) then
 begin
  vm_map_unlock(map);
  Exit(KERN_NO_SPACE);
 end;

 { Round up the grow amount modulo sgrowsiz }
 growsize:=sgrowsiz;
 grow_amount:=roundup(grow_amount, growsize);
 if (grow_amount>stack_entry^.avail_ssize) then
 begin
  grow_amount:=stack_entry^.avail_ssize;
 end;
 if (is_procstack<>0) and (ctob(g_vmspace.vm_ssize) + grow_amount>stacklim) then
 begin
  grow_amount:=trunc_page(stacklim) - ctob(g_vmspace.vm_ssize);
 end;

 { If we would blow our VMEM resource limit, no go }
 if (map^.size + grow_amount>vmemlim) then
 begin
  vm_map_unlock(map);
  rv:=KERN_NO_SPACE;
  goto _out;
 end;

 if (stack_entry=next_entry) then
 begin
  {
   * Growing downward.
   }
  { Get the preliminary new entry start value }
  addr:=stack_entry^.start - grow_amount;

  {
   * If this puts us into the previous entry, cut back our
   * growth to the available space. Also, see the note above.
   }
  if (addr<__end) then
  begin
   stack_entry^.avail_ssize:=max_grow;
   addr:=__end;
   if (stack_guard_page<>0) then
   begin
    addr:=addr+PAGE_SIZE;
   end;
  end;

  rv:=vm_map_insert(map, nil, 0, addr, stack_entry^.start,
      next_entry^.protection, next_entry^.max_protection, 0, false);

  { Adjust the available stack space by the amount we grew. }
  if (rv=KERN_SUCCESS) then
  begin
   if (prev_entry<>@map^.header) then
   begin
    vm_map_clip_end(map, prev_entry, addr);
   end;
   new_entry:=prev_entry^.next;
   Assert(new_entry=stack_entry^.prev, 'foo');
   Assert(new_entry^.__end=stack_entry^.start, 'foo');
   Assert(new_entry^.start=addr, 'foo');
   grow_amount:=new_entry^.__end - new_entry^.start;
   new_entry^.avail_ssize:=stack_entry^.avail_ssize - grow_amount;
   stack_entry^.eflags:=stack_entry^.eflags and (not MAP_ENTRY_GROWS_DOWN);
   new_entry^.eflags:=new_entry^.eflags or MAP_ENTRY_GROWS_DOWN;
  end;
 end else
 begin
  {
   * Growing upward.
   }
  addr:=stack_entry^.__end + grow_amount;

  {
   * If this puts us into the next entry, cut back our growth
   * to the available space. Also, see the note above.
   }
  if (addr>__end) then
  begin
   stack_entry^.avail_ssize:=__end - stack_entry^.__end;
   addr:=__end;
   if (stack_guard_page<>0) then
   begin
    addr:=addr-PAGE_SIZE;
   end;
  end;

  grow_amount:=addr - stack_entry^.__end;
  { Grow the underlying object if applicable. }

  if (stack_entry^.vm_obj=nil) then goto _or;

  if vm_object_coalesce(stack_entry^.vm_obj,
                        stack_entry^.offset,
                        vm_size_t(stack_entry^.__end - stack_entry^.start),
                        vm_size_t(grow_amount), false) then
  begin
   _or:
   map^.size:=map^.size+(addr - stack_entry^.__end);
   { Update the current entry. }
   stack_entry^.__end:=addr;
   stack_entry^.avail_ssize:=stack_entry^.avail_ssize-grow_amount;
   vm_map_entry_resize_free(map, stack_entry);
   rv:=KERN_SUCCESS;

   if (next_entry<>@map^.header) then
   begin
    vm_map_clip_start(map, next_entry, addr);
   end;
  end else
  begin
   rv:=KERN_FAILURE;
  end;
 end;

 if (rv=KERN_SUCCESS) and (is_procstack<>0) then
 begin
  g_vmspace.vm_ssize:=g_vmspace.vm_ssize+btoc(grow_amount);
 end;

 vm_map_unlock(map);

_out:

 Result:=rv;
end;

function vmspace_exec(minuser,maxuser:vm_offset_t):Integer;
begin
 Assert((curkthread^.td_pflags and TDP_EXECVMSPC)=0, 'vmspace_exec recursed');

 //if (p=curkthread^.td_proc) then
 //begin
 // pmap_activate(curthread);
 //end;

 curkthread^.td_pflags:=curkthread^.td_pflags or TDP_EXECVMSPC;

 Exit(0);
end;

{
 * vm_map_lookup:
 *
 * Finds the VM object, offset, and
 * protection for a given virtual address in the
 * specified map, assuming a page fault of the
 * type specified.
 *
 * Leaves the map in question locked for read; return
 * values are guaranteed until a vm_map_lookup_done
 * call is performed.  Note that the map argument
 * is in/out; the returned map must be used in
 * the call to vm_map_lookup_done.
 *
 * A handle (out_entry) is returned for use in
 * vm_map_lookup_done, to make that fast.
 *
 * If a lookup is requested with "write protection"
 * specified, the map may be changed to perform virtual
 * copying operations, although the data referenced will
 * remain the same.
 }
function vm_map_lookup(var_map    :p_vm_map_t;        { IN/OUT }
                       vaddr      :vm_offset_t;
                       fault_typea:vm_prot_t;
                       out_entry  :p_vm_map_entry_t;  { OUT }
                       vm_obj     :p_vm_object_t;     { OUT }
                       pindex     :p_vm_pindex_t;     { OUT }
                       out_prot   :p_vm_prot_t        { OUT }
                      ):Integer;
label
 RetryLookup;
var
 entry:vm_map_entry_t;
 map:vm_map_t;
 prot:vm_prot_t;
 fault_type:vm_prot_t;
 size:vm_size_t;
 old_map:vm_map_t;
begin
 map:=var_map^;
 fault_type:=fault_typea;

RetryLookup:

 vm_map_lock(map);

 {
  * Lookup the faulting address.
  }
 if (not vm_map_lookup_entry(map, vaddr, out_entry)) then
 begin
  vm_map_unlock(map);
  Exit(KERN_INVALID_ADDRESS);
 end;

 entry:=out_entry^;

 {
  * Handle submaps.
  }
 if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) then
 begin
  old_map:=map;

  map:=vm_map_t(entry^.vm_obj);
  var_map^:=map;
  vm_map_unlock(old_map);
  goto RetryLookup;
 end;

 {
  * Check whether this task is allowed to have this page.
  }
 prot:=entry^.protection;
 fault_type:=fault_type and (VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE);
 if ((fault_type and prot)<>fault_type) or (prot=VM_PROT_NONE) then
 begin
  vm_map_unlock(map);
  Exit(KERN_PROTECTION_FAILURE);
 end;
 Assert(((prot and VM_PROT_WRITE)=0) or (
     (entry^.eflags and (MAP_ENTRY_NEEDS_COPY))<>
     (MAP_ENTRY_NEEDS_COPY)
     ),'entry %p flags %x');
 if ((fault_typea and VM_PROT_COPY)<>0) and
    ((entry^.max_protection and VM_PROT_WRITE)=0) and
    ((entry^.eflags and MAP_ENTRY_COW)=0) then
 begin
  vm_map_unlock(map);
  Exit(KERN_PROTECTION_FAILURE);
 end;

 size:=entry^.__end - entry^.start;
 {
  * If the entry was copy-on-write, we either ...
  }
 if ((entry^.eflags and MAP_ENTRY_NEEDS_COPY)<>0) then
 begin
  {
   * If we want to write the page, we may as well handle that
   * now since we've got the map locked.
   *
   * If we don't need to write the page, we just demote the
   * permissions allowed.
   }
  if ((fault_type and VM_PROT_WRITE)<>0) or
     ((fault_typea and VM_PROT_COPY)<>0) then
  begin
   {
    * Make a new object, and place it in the object
    * chain.  Note that no new references have appeared
    * -- one just moved from the map to the new
    * object.
    }
   //vm_object_shadow(@entry^.vm_obj, @entry^.offset, size);

   entry^.eflags:=entry^.eflags and (not MAP_ENTRY_NEEDS_COPY);
  end else
  begin
   {
    * We're attempting to read a copy-on-write page --
    * don't allow writes.
    }
   prot:=prot and (not VM_PROT_WRITE);
  end;
 end;

 {
  * Return the object/offset from this entry.  If the entry was
  * copy-on-write or empty, it has been fixed up.
  }
 pindex^:=OFF_TO_IDX((vaddr - entry^.start) + entry^.offset);
 vm_obj^:=entry^.vm_obj;

 out_prot^:=prot;
 Result:=(KERN_SUCCESS);
end;

{
 * vm_map_lookup_locked:
 *
 * Lookup the faulting address.  A version of vm_map_lookup that returns
 *      KERN_FAILURE instead of blocking on map lock or memory allocation.
 }
function vm_map_lookup_locked(var_map    :p_vm_map_t;        { IN/OUT }
                              vaddr      :vm_offset_t;
                              fault_typea:vm_prot_t;
                              out_entry  :p_vm_map_entry_t;  { OUT }
                              vm_obj     :p_vm_object_t;     { OUT }
                              pindex     :p_vm_pindex_t;     { OUT }
                              out_prot   :p_vm_prot_t;       { OUT }
                              wired      :PBoolean           { OUT }
                             ):Integer;
var
 entry:vm_map_entry_t;
 map:vm_map_t;
 prot:vm_prot_t;
 fault_type:vm_prot_t;
begin
 map:=var_map^;
 fault_type:=fault_typea;

 {
  * Lookup the faulting address.
  }
 if (not vm_map_lookup_entry(map, vaddr, out_entry)) then
 begin
  Exit(KERN_INVALID_ADDRESS);
 end;

 entry:=out_entry^;

 {
  * Fail if the entry refers to a submap.
  }
 if ((entry^.eflags and MAP_ENTRY_IS_SUB_MAP)<>0) then
 begin
  Exit(KERN_FAILURE);
 end;

 {
  * Check whether this task is allowed to have this page.
  }
 prot:=entry^.protection;
 fault_type:=fault_type and (VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE);
 if ((fault_type and prot)<>fault_type) then
 begin
  Exit(KERN_PROTECTION_FAILURE);
 end;

 if ((entry^.eflags and MAP_ENTRY_NEEDS_COPY)<>0) then
 begin
  {
   * Fail if the entry was copy-on-write for a write fault.
   }
  if ((fault_type and VM_PROT_WRITE)<>0) then
  begin
   Exit(KERN_FAILURE);
  end;
  {
   * We're attempting to read a copy-on-write page --
   * don't allow writes.
   }
  prot:=prot and (not VM_PROT_WRITE);
 end;

 {
  * Fail if an object should be created.
  }
 if (entry^.vm_obj=nil) then
 begin
  Exit(KERN_FAILURE);
 end;

 {
  * Return the object/offset from this entry.  If the entry was
  * copy-on-write or empty, it has been fixed up.
  }
 pindex^:=OFF_TO_IDX((vaddr - entry^.start) + entry^.offset);
 vm_obj^:=entry^.vm_obj;

 out_prot^:=prot;
 Result:=(KERN_SUCCESS);
end;

{
 * vm_map_lookup_done:
 *
 * Releases locks acquired by a vm_map_lookup
 * (according to the handle returned by that lookup).
 }
procedure vm_map_lookup_done(map:vm_map_t;entry:vm_map_entry_t);
begin
 {
  * Unlock the main-level map
  }
 vm_map_unlock(map);
end;

procedure vm_map_set_name_locked(map:vm_map_t;start,__end:vm_offset_t;name:PChar);
var
 current:vm_map_entry_t;
 entry:vm_map_entry_t;
begin
 if (start=__end) then
 begin
  Exit();
 end;

 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_map_lookup_entry(map, start,@entry)) then
 begin
  vm_map_clip_start(map, entry, start);
 end else
 begin
  entry:=entry^.next;
 end;

 current:=entry;
 while ((current<>@map^.header) and (current^.start<__end)) do
 begin
  vm_map_clip_end(map,current,__end);

  MoveChar0(name^,current^.name,32);

  vm_map_simplify_entry(map, current);

  current:=current^.next;
 end;
end;

procedure vm_map_set_name_locked(map:vm_map_t;start,__end:vm_offset_t;name:PChar;i:vm_inherit_t);
var
 current:vm_map_entry_t;
 entry:vm_map_entry_t;
begin
 if (start=__end) then
 begin
  Exit();
 end;

 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_map_lookup_entry(map, start,@entry)) then
 begin
  vm_map_clip_start(map, entry, start);
 end else
 begin
  entry:=entry^.next;
 end;

 current:=entry;
 while ((current<>@map^.header) and (current^.start<__end)) do
 begin
  vm_map_clip_end(map,current,__end);

  MoveChar0(name^,current^.name,32);
  current^.inheritance:=i;

  vm_map_simplify_entry(map, current);

  current:=current^.next;
 end;
end;

procedure vm_map_set_name(map:vm_map_t;start,__end:vm_offset_t;name:PChar);
begin
 vm_map_lock(map);
 vm_map_set_name_locked(map,start,__end,name);
 vm_map_unlock(map);
end;

procedure vm_map_track_insert(map:vm_map_t;tobj:Pointer);
var
 entry:vm_map_entry_t;
 obj:vm_object_t;

 start:vm_offset_t;
 __end:vm_offset_t;

 e_start:vm_offset_t;
 e___end:vm_offset_t;

 diff:QWORD;
 size:QWORD;
 offset:QWORD;
begin
 if (tobj=nil) then Exit;

 vm_map_lock(map);

 start:=p_vm_track_object(tobj)^.align.start;
 __end:=p_vm_track_object(tobj)^.align.__end;

 VM_MAP_RANGE_CHECK(map, start, __end);

 if (vm_map_lookup_entry(map, start, @entry)) then
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
   obj:=entry^.vm_obj;

   if (obj<>nil) then
   begin
    if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
    begin
     //ext rmap track

     diff:=e_start-entry^.start;
     size:=e___end-e_start;

     offset:=entry^.offset;

     //transform by base addr
     offset:=offset + (QWORD(obj^.un_pager.map_base) - VM_MIN_GPU_ADDRESS);

     offset:=offset+diff;

     //Writeln('rmem_map_track:  ',HexStr(e_start,16),'..',HexStr(e___end,16));

     rmem_map_track(map^.rmap,
                    offset,
                    offset+size,
                    e_start,
                    tobj);

     //next
     entry:=entry^.next;
     Continue;
    end;

    //file mirrors TODO
   end;

   //one map track
   vm_track_map_lock(@map^.pmap^.tr_map);
    _vm_track_map_insert(@map^.pmap^.tr_map,e_start,e___end,e_start,tobj);
   vm_track_map_unlock(@map^.pmap^.tr_map)
  end; //

  entry:=entry^.next;
 end;

 vm_map_unlock(map);
end;

procedure vm_map_track_remove(map:vm_map_t;tobj:Pointer);
begin
 if (tobj=nil) then Exit;

 vm_track_map_remove_object(@map^.pmap^.tr_map,tobj);
end;

function vm_map_track_next(map:vm_map_t;start:vm_offset_t;tobj:Pointer;htype:T_THANDLE_TYPE):Pointer;
begin
 Result:=vm_track_map_next_object(@map^.pmap^.tr_map,start,tobj,htype);
end;

procedure _vm_map_track_delete_deferred(map:vm_map_t;tobj:Pointer);
begin
 _vm_track_map_delete_deferred(@map^.pmap^.tr_map,tobj);
end;

function vm_map_track_trigger(map:vm_map_t;start,__end:vm_offset_t;exclude:Pointer;mode:T_TRIGGER_MODE):Integer;
begin
 Result:=vm_track_map_trigger(@map^.pmap^.tr_map,start,__end,exclude,mode);
end;

procedure vm_map_track_restore(map:vm_map_t;tobj:Pointer);
begin
 if (tobj=nil) then Exit;

 vm_track_map_restore_object(@map^.pmap^.tr_map,tobj);
end;

procedure vminit;
begin
 p_proc.p_vmspace:=vmspace_alloc();
end;


end.



