unit uma;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_mtx,
 md_map;

const
 M_NOWAIT=$0001; // do not block
 M_WAITOK=$0002; // ok to block
 M_ZERO  =$0100; // bzero the allocation
 M_NOVM  =$0200; // don't ask VM for pages
 M_NODUMP=$0800; // don't dump pages in this allocation

const
 UMA_SMALLEST_UNIT=(MD_PAGE_SIZE div 256); // Smallest item allocated

const
 UMA_ZONE_PAGEABLE   =$0001; { Return items not fully backed by
                                physical memory XXX Not yet }
 UMA_ZONE_ZINIT      =$0002; { Initialize with zeros }
 UMA_ZONE_STATIC     =$0004; { Statically sized zone }
 UMA_ZONE_OFFPAGE    =$0008; { Force the slab structure allocation
                               off of the real memory }
 UMA_ZONE_MALLOC     =$0010; { For use by malloc(9) only! }
 UMA_ZONE_NOFREE     =$0020; { Do not free slabs of this type! }
 UMA_ZONE_MTXCLASS   =$0040; { Create a new lock class }
 UMA_ZONE_VM         =$0080; {
                               Used for internal vm datastructures
                               only.
                              }
 UMA_ZONE_HASH       =$0100; {
                               Use a hash table instead of caching
                               information in the vm_page.
                              }
 UMA_ZONE_SECONDARY  =$0200; { Zone is a Secondary Zone }
 UMA_ZONE_REFCNT     =$0400; { Allocate refcnts in slabs }
 UMA_ZONE_MAXBUCKET  =$0800; { Use largest buckets }
 UMA_ZONE_CACHESPREAD=$1000; {
                               Spread memory start locations across
                               all possible cache lines.  May
                               require many virtually contiguous
                               backend pages and can fail early.
                              }
 UMA_ZONE_VTOSLAB    =$2000; { Zone uses vtoslab for lookup. }
 UMA_ZONE_NODUMP     =$4000; {
                               Zone's pages will not be included in
                               mini-dumps.
                              }

 UMA_ZONE_INHERIT=(UMA_ZONE_OFFPAGE or UMA_ZONE_MALLOC or  UMA_ZONE_HASH or UMA_ZONE_REFCNT or UMA_ZONE_VTOSLAB);

 UMA_ALIGN_PTR  =(sizeof(Pointer) - 1);
 UMA_ALIGN_LONG =(sizeof(DWORD) - 1);
 UMA_ALIGN_INT  =(sizeof(Integer) - 1);
 UMA_ALIGN_SHORT=(sizeof(Word) - 1);
 UMA_ALIGN_CHAR =(sizeof(Byte) - 1);
 UMA_ALIGN_CACHE=(0 - 1);

type
 _uma_zone_t=Pointer;

 uma_ctor=function (mem:Pointer;size:Integer;arg:Pointer;flags:Integer):Integer;
 uma_dtor=procedure(mem:Pointer;size:Integer;arg:Pointer);
 uma_init=function (mem:Pointer;size:Integer;flags:Integer):Integer;
 uma_fini=procedure(mem:Pointer;size:Integer);

type
 uma_alloc=function (zone:_uma_zone_t;size:Integer;pflag:pByte;wait:Integer):Pointer;
 uma_free =procedure(item:Pointer;size:Integer;flag:Byte);

const
 UMA_SLAB_BOOT  =$01;  // Slab alloced from boot pages
 UMA_SLAB_KMEM  =$02;  // Slab alloced from kmem_map
 //UMA_SLAB_KERNEL=$04;  // Slab alloced from kernel_map
 UMA_SLAB_PRIV  =$08;  // Slab alloced from priv allocator
 UMA_SLAB_OFFP  =$10;  // Slab is managed separately
 UMA_SLAB_MALLOC=$20;  // Slab is a large malloc slab

const
 UMA_STREAM_VERSION=$00000001;

type
 uma_stream_header=record
  ush_version:DWORD; // Stream format version.
  ush_maxcpus:DWORD; // Value of MAXCPU for stream.
  ush_count  :DWORD; // Number of records.
  _ush_pad   :DWORD; // Pad/reserved field.
 end;

const
 UTH_MAX_NAME=32;
 UTH_ZONE_SECONDARY=$00000001;

type
 uma_type_header=record
  // Static per-zone data, some extracted from the supporting keg.
  uth_name:array[0..UTH_MAX_NAME-1] of Char;
  uth_align   :DWORD; // Keg: alignment.
  uth_size    :DWORD; // Keg: requested size of item.
  uth_rsize   :DWORD; // Keg: real size of item.
  uth_maxpages:DWORD; // Keg: maximum number of pages.
  uth_limit   :DWORD; // Keg: max items to allocate.

  // Current dynamic zone/keg-derived statistics.
  uth_pages     :DWORD; // Keg: pages allocated.
  uth_keg_free  :DWORD; // Keg: items free.
  uth_zone_free :DWORD; // Zone: items free.
  uth_bucketsize:DWORD; // Zone: desired bucket size.
  uth_zone_flags:DWORD; // Zone: flags.

  uth_allocs:QWORD; // Zone: number of allocations.
  uth_frees :QWORD; // Zone: number of frees.
  uth_fails :QWORD; // Zone: number of alloc failures.
  uth_sleeps:QWORD; // Zone: number of alloc sleeps.
  _uth_reserved1:array[0..1] of QWORD; // Reserved.
 end;

 uma_percpu_stat=record
  ups_allocs    :QWORD; // Cache: number of allocations.
  ups_frees     :QWORD; // Cache: number of frees.
  ups_cache_free:QWORD; // Cache: free items in cache.
  _ups_reserved :array[0..4] of QWORD; // Reserved.
 end;

//uma_int

const
 UMA_SLAB_SIZE =MD_ALLOC_GRANULARITY;           // How big are our slabs?
 UMA_SLAB_MASK =(MD_ALLOC_GRANULARITY - 1);     // Mask to get back to the page
 UMA_SLAB_SHIFT=BsfQWORD(MD_ALLOC_GRANULARITY); // Number of bits PAGE_MASK

 UMA_SUB_PAGES=(MD_ALLOC_GRANULARITY div MD_PAGE_SIZE);

 UMA_BOOT_PAGES_CONST=64 div UMA_SUB_PAGES; // Pages allocated for startup

 UMA_MAX_WASTE=(UMA_SLAB_SIZE div 10); // Max waste before going to off page slab management

 UMA_HASH_SIZE_INIT=32;

type
 p_slabhead=^slabhead;
 slabhead=SLIST_HEAD; //uma_slab

 p_uma_hash=^uma_hash;
 uma_hash=record
  uh_slab_hash:p_slabhead;  // Hash table for slabs
  uh_hashsize :Integer;     // Current size of the hash table
  uh_hashmask :Integer;     // Mask used during hashing
 end;

const
 UMA_ALIGN=64;

type
 //Structures for per cpu queues.
 uma_bucket=record
  ub_link   :LIST_ENTRY; // (uma_bucket) Link into the zone
  ub_cnt    :WORD;       // Count of free items.
  ub_entries:WORD;       // Max items.
  ub_bucket :array[0..0] of Pointer; // actual allocation storage
 end;

 uma_bucket_t=^uma_bucket;

 uma_cache=record
  Case Byte of
   0:(uc_freebucket :uma_bucket_t; // Bucket we're freeing to
      uc_allocbucket:uma_bucket_t; // Bucket to allocate from
      uc_allocs     :QWORD;        // Count of allocations
      uc_frees      :QWORD;        // Count of frees
     );
   1:(_ALIGN:array[0..UMA_ALIGN-1] of Byte);
 end;

 uma_cache_t=^uma_cache;

 uma_keg=record
  uk_link:LIST_ENTRY; // (uma_keg)  List of all kegs

  uk_lock:mtx; // Lock for the keg
  uk_hash:uma_hash;

  uk_name:pchar;  // Name of creating zone.
  uk_zones    :LIST_HEAD; // (uma_zone) Keg's zones
  uk_part_slab:LIST_HEAD; // (uma_slab) partially allocated slabs
  uk_free_slab:LIST_HEAD; // (uma_slab) empty slab list
  uk_full_slab:LIST_HEAD; // (uma_slab) full slabs

  uk_recurse :DWORD; // Allocation recursion count
  uk_align   :DWORD; // Alignment mask
  uk_pages   :DWORD; // Total page count
  uk_free    :DWORD; // Count of items free in slabs
  uk_size    :DWORD; // Requested size of each item
  uk_rsize   :DWORD; // Real size of each item
  uk_maxpages:DWORD; // Maximum number of pages to alloc

  uk_init  :uma_init ; // Keg's init routine
  uk_fini  :uma_fini ; // Keg's fini routine
  uk_allocf:uma_alloc; // Allocation function
  uk_freef :uma_free ; // Free routine

  //uk_obj:vm_object_t;  // Zone specific object
  //uk_kva:vm_offset_t;  // Base kva for zones with objs
  uk_slabzone:_uma_zone_t; // Slab zone backing us, if OFFPAGE

  uk_pgoff:WORD;  // Offset to uma_slab struct
  uk_ppera:WORD;  // pages per allocation from backend
  uk_ipers:WORD;  // Items per slab
  {$IF UMA_SUB_PAGES>1}
  uk_ssizl:Byte;  // sub pages size (in log2)
  uk_ssubc:Byte;  // sub pages count
  uk_isubs:Byte;  // Items per sub page
  uk_isubl:Byte;  // Items in last sub page
  {$ENDIF}
  uk_flags:DWORD; // Internal flags
 end;
 uma_keg_t=^uma_keg;

const
 us_word_bitsize=8;

type
 us_word=Byte;

 us_free_info=packed record
  ui_count:us_word; // How many are free?
  ui_first:us_word; // First free item index
 end;

 // Page management structure

 // Sorry for the union, but space efficiency is important
 uma_slab_head=bitpacked record
  us_keg:uma_keg_t;   // Keg we live in
  us_type:record
   Case Byte of
    0:(_us_link:LIST_ENTRY); // (uma_slab) slabs in zone
    1:(_us_size:QWORD);      // Size of allocation
  end;
  us_hlink:SLIST_ENTRY; // (uma_slab) Link for hash table
  us_data     :pbyte;   // First item
  us_flags    :Byte;    // Page flags see uma.h
  us_free     :array[0..UMA_SUB_PAGES-1] of us_free_info;
 end;

 t_us_freelist_uma_slab=packed record
  us_item:us_word;
 end;

 // The standard slab structure
 uma_slab=packed object
  us_head    :uma_slab_head; // slab header data
  us_freelist:array[0..0] of t_us_freelist_uma_slab; //actual number bigger
  //
  property us_keg      :uma_keg_t   read us_head.us_keg            write us_head.us_keg          ;
  property us_link     :LIST_ENTRY  read us_head.us_type._us_link  write us_head.us_type._us_link;
  property us_size     :QWORD       read us_head.us_type._us_size  write us_head.us_type._us_size;
  property us_hlink    :SLIST_ENTRY read us_head.us_hlink          write us_head.us_hlink        ;
  property us_data     :pbyte       read us_head.us_data           write us_head.us_data         ;
  property us_flags    :Byte        read us_head.us_flags          write us_head.us_flags        ;
 end;

 {
   The slab structure for UMA_ZONE_REFCNT zones for whose items we
   maintain reference counters in the slab for.
 }

 t_us_freelist_uma_slab_refcnt=bitpacked record
  us_item  :us_word;
  us_refcnt:0..(1 shl (32-us_word_bitsize))-1;
 end;

 uma_slab_refcnt=packed object
  us_head    :uma_slab_head; // slab header data
  us_freelist:array[0..0] of t_us_freelist_uma_slab_refcnt; //actual number bigger
  //
  property us_keg      :uma_keg_t   read us_head.us_keg            write us_head.us_keg          ;
  property us_link     :LIST_ENTRY  read us_head.us_type._us_link  write us_head.us_type._us_link;
  property us_size     :QWORD       read us_head.us_type._us_size  write us_head.us_type._us_size;
  property us_hlink    :SLIST_ENTRY read us_head.us_hlink          write us_head.us_hlink        ;
  property us_data     :pbyte       read us_head.us_data           write us_head.us_data         ;
  property us_flags    :Byte        read us_head.us_flags          write us_head.us_flags        ;
 end;

 uma_slab_t      =^uma_slab;
 uma_slabrefcnt_t=^uma_slab_refcnt;

const
 UMA_FRITM_SZ   =(sizeof(uma_slab)        - sizeof(uma_slab_head));
 UMA_FRITMREF_SZ=(sizeof(uma_slab_refcnt) - sizeof(uma_slab_head));

type
 uma_klink=record
  kl_link:LIST_ENTRY; // (uma_klink)
  kl_keg :uma_keg_t;
 end;
 uma_klink_t=^uma_klink;

 uma_zone_t=^uma_zone;

 uma_slaballoc=function(z:uma_zone_t;k:uma_keg_t;i:Integer):uma_slab_t;

 // Zone management structure
 uma_zone=packed record
  uz_name:pchar; // Text name of the zone
  uz_lock:p_mtx; // Lock for the zone (keg's lock)

  uz_link       :LIST_ENTRY; // (uma_zone)   List of all zones in keg
  uz_full_bucket:LIST_HEAD ; // (uma_bucket) full buckets
  uz_free_bucket:LIST_HEAD ; // (uma_bucket) Buckets for frees

  uz_kegs :LIST_HEAD; // (uma_klink) List of kegs.
  uz_klink:uma_klink; // klink for first keg.

  uz_slab:uma_slaballoc; // Allocate a slab from the backend.
  uz_ctor:uma_ctor;      // Constructor for each allocation
  uz_dtor:uma_dtor;      // Destructor
  uz_init:uma_init;      // Initializer for each item
  uz_fini:uma_fini;      // Discards memory

  uz_flags:DWORD; // Flags inherited from kegs
  uz_size :DWORD; // Size inherited from kegs

  uz_allocs:QWORD; // (UMA_ALIGN) Total number of allocations
  uz_frees :QWORD; // Total number of frees
  uz_fails :QWORD; // Total number of alloc failures
  uz_sleeps:QWORD; // Total number of alloc sleeps
  uz_fills :WORD;  // Outstanding bucket fills
  uz_count :WORD;  // Highest value ub_ptr can have

  {
   * This HAS to be the last item because we adjust the zone size
   * based on NCPU and then allocate the space for the zones.
  }
  uz_cpu:array[0..0] of uma_cache; // Per cpu caches
 end;

const
 SIZEOF_UMA_ZONE=sizeof(uma_zone) - sizeof(uma_cache);

const
 UMA_ZFLAG_BUCKET   =$02000000; // Bucket zone.
 UMA_ZFLAG_MULTI    =$04000000; // Multiple kegs in the zone.
 UMA_ZFLAG_DRAINING =$08000000; // Running zone_drain.
 UMA_ZFLAG_PRIVALLOC=$10000000; // Use uz_allocf.
 UMA_ZFLAG_INTERNAL =$20000000; // No offpage no PCPU.
 UMA_ZFLAG_FULL     =$40000000; // Reached uz_maxpages
 UMA_ZFLAG_CACHEONLY=$80000000; // Don't ask VM for buckets.

 UMA_ZFLAG_INHERIT=(UMA_ZFLAG_INTERNAL or UMA_ZFLAG_CACHEONLY or UMA_ZFLAG_BUCKET);

///


procedure zone_drain(zone:uma_zone_t); external;

function uma_zcreate(name  :pchar;
                     size  :QWORD;
                     ctor  :uma_ctor;
                     dtor  :uma_dtor;
                     uminit:uma_init;
                     fini  :uma_fini;
                     align :Integer;
                     flags :DWORD
                    ):uma_zone_t; external;

function uma_zsecond_create(name  :pchar;
                            ctor  :uma_ctor;
                            dtor  :uma_dtor;
                            zinit :uma_init;
                            zfini :uma_fini;
                            master:uma_zone_t
                           ):uma_zone_t; external;

function  uma_zsecond_add(zone,master:uma_zone_t):Integer; external;

procedure uma_zdestroy  (zone:uma_zone_t); external;
function  uma_zalloc_arg(zone:uma_zone_t;udata:Pointer;flags:Integer):Pointer; external;
function  uma_zalloc    (zone:uma_zone_t;flags:Integer):Pointer; inline;
procedure uma_zfree_arg (zone:uma_zone_t;item,udata:Pointer); external;
procedure uma_zfree     (zone:uma_zone_t;item:Pointer); inline;

procedure uma_reclaim(); external;
procedure uma_set_align(align:Integer); external;

//int uma_zone_set_obj(uma_zone_t zone, struct vm_object *obj, int size);

function uma_zone_set_max(zone:uma_zone_t;nitems:Integer):Integer; external;
function uma_zone_get_max(zone:uma_zone_t):Integer; external;
function uma_zone_get_cur(zone:uma_zone_t):Integer; external;

procedure uma_zone_set_init(zone:uma_zone_t;uminit:uma_init); external;
procedure uma_zone_set_fini(zone:uma_zone_t;fini:uma_fini); external;

procedure uma_zone_set_zinit(zone:uma_zone_t;zinit:uma_init); external;
procedure uma_zone_set_zfini(zone:uma_zone_t;zfini:uma_fini); external;

procedure uma_zone_set_allocf(zone:uma_zone_t;allocf:uma_alloc); external;
procedure uma_zone_set_freef(zone:uma_zone_t;freef:uma_free); external;

procedure uma_prealloc(zone:uma_zone_t;items:Integer); external;
//u_int32_t *uma_find_refcnt(uma_zone_t zone, void *item);

function uma_zone_exhausted(zone:uma_zone_t):Integer; external;
function uma_zone_exhausted_nolock(zone:uma_zone_t):Integer; external;

///

function  UMA_HASH_(h:p_uma_hash;s:Pointer):DWORD; inline;
procedure UMA_HASH_INSERT(h:p_uma_hash;s:uma_slab_t;mem:Pointer); inline;
procedure UMA_HASH_REMOVE(h:p_uma_hash;s:uma_slab_t;mem:Pointer); inline;

function  hash_sfind(hash:p_uma_hash;data:pbyte):uma_slab_t;

procedure KEG_LOCK_INIT(k:uma_keg_t;lc:Integer); inline;
procedure KEG_LOCK_FINI(k:uma_keg_t); inline;
procedure KEG_LOCK(k:uma_keg_t); inline;
procedure KEG_UNLOCK(k:uma_keg_t); inline;
procedure ZONE_LOCK(z:uma_zone_t); inline;
procedure ZONE_UNLOCK(z:uma_zone_t); inline;

implementation

function uma_zalloc(zone:uma_zone_t;flags:Integer):Pointer; inline;
begin
 Result:=uma_zalloc_arg(zone, nil, flags);
end;

procedure uma_zfree(zone:uma_zone_t;item:Pointer); inline;
begin
 uma_zfree_arg(zone, item, nil);
end;

//

function UMA_HASH_(h:p_uma_hash;s:Pointer):DWORD; inline;
begin
 Result:=(DWORD(s) shr UMA_SLAB_SHIFT) and h^.uh_hashmask;
end;

procedure UMA_HASH_INSERT(h:p_uma_hash;s:uma_slab_t;mem:Pointer); inline;
begin
 SLIST_INSERT_HEAD(@h^.uh_slab_hash[UMA_HASH_(h, mem)],s,@s^.us_hlink);
end;

procedure UMA_HASH_REMOVE(h:p_uma_hash;s:uma_slab_t;mem:Pointer); inline;
begin
 SLIST_REMOVE(@h^.uh_slab_hash[UMA_HASH_(h, mem)],s,@s^.us_hlink);
end;

//

function hash_sfind(hash:p_uma_hash;data:pbyte):uma_slab_t;
var
 slab:uma_slab_t;
 hval:Integer;
begin
 hval:=UMA_HASH_(hash, data);

 slab:=SLIST_FIRST(@hash^.uh_slab_hash[hval]);
 while (slab<>nil) do
 begin
  if (slab^.us_data=data) then Exit(slab);
  //
  slab:=SLIST_NEXT(slab,@slab^.us_hlink);
 end;

 Result:=nil;
end;

//

procedure KEG_LOCK_INIT(k:uma_keg_t;lc:Integer); inline;
begin
 if (lc<>0) then
 begin
  mtx_init(k^.uk_lock, k^.uk_name); //k^.uk_name
 end else
 begin
  mtx_init(k^.uk_lock, k^.uk_name); //'UMA zone'
 end;
end;

procedure KEG_LOCK_FINI(k:uma_keg_t); inline;
begin
 mtx_destroy(k^.uk_lock);
end;

procedure KEG_LOCK(k:uma_keg_t); inline;
begin
 mtx_lock(k^.uk_lock);
end;

procedure KEG_UNLOCK(k:uma_keg_t); inline;
begin
 mtx_unlock(k^.uk_lock);
end;

procedure ZONE_LOCK(z:uma_zone_t); inline;
begin
 mtx_lock(z^.uz_lock^);
end;

procedure ZONE_UNLOCK(z:uma_zone_t); inline;
begin
 mtx_unlock(z^.uz_lock^);
end;


end.

