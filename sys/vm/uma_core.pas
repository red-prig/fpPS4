unit uma_core;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

{$OPTIMIZATION LEVEL3}
{$OPTIMIZATION LOOPUNROLL}
{$OPTIMIZATION REGVAR}

interface

uses
 mqueue,
 errno,
 uma,
 systm,
 kern_param,
 time,
 md_time,
 kern_mtx,
 md_map;

{
 * uma_core.c  Implementation of the Universal Memory allocator
 *
 * This allocator is intended to replace the multitude of similar object caches
 * in the standard FreeBSD kernel.  The intent is to be flexible as well as
 * effecient.  A primary design goal is to Exitunused memory to the rest of
 * the system.  This will make the system as a whole more flexible due to the
 * ability to move memory to subsystems which most need it instead of leaving
 * pools of reserved memory unused.
 *
 * The basic ideas stem from similar slab/zone based allocators whose algorithms
 * are well known.
 *
 }

{
 * This is the zone and keg from which all zones are spawned.  The idea is that
 * even the zone & keg heads are allocated from the allocator, so we use the
 * bss section to bootstrap us.
 }
var
 masterkeg   :uma_keg ;
 masterzone_k:uma_zone;
 masterzone_z:uma_zone;
 kegs :uma_zone_t=@masterzone_k;
 zones:uma_zone_t=@masterzone_z;

{ This is the zone from which all of uma_slab_t's are allocated. }
 slabzone:uma_zone_t;

{
 * The initial hash tables come out of this zone so they can be allocated
 * prior to malloc coming up.
 }
 hashzone:uma_zone_t;

{ The boot-time adjusted value for cache line alignment. }
 uma_align_cache_var:Integer=64 - 1;

 //static MALLOC_DEFINE(M_UMAHASH, 'UMAHash', 'UMA Hash Buckets');

{
 * Are we allowed to allocate buckets?
 }
 bucketdisable:Integer=1;

{ Linked list of all kegs in the system }
 uma_kegs:LIST_HEAD=(lh_first:nil); // (uma_keg)

{ This mutex protects the keg list }
 uma_mtx:mtx;

{ Linked list of boot time pages }
 uma_boot_pages:LIST_HEAD=(lh_first:nil); // (uma_slab)

{ This mutex protects the boot time pages list }
 uma_boot_pages_mtx:mtx;

{ Is the VM done starting up? }
 booted:Integer=0;

const
 UMA_STARTUP1_CONST=1;
 UMA_STARTUP2_CONST=2;

var
{ Maximum number of allowed items-per-slab if the slab header is OFFPAGE }
 uma_max_ipers    :DWORD;
 uma_max_ipers_ref:DWORD;

{
 * This is the handle used to schedule events that need to happen
 * outside of the allocation fast path.
 }
 uma_callout:Int64=0;

const
 UMA_TIMEOUT_CONST=20; { Seconds for callout interval. }

type
{
 * This structure is passed as the zone ctor arg so that I don't have to create
 * a special allocation function just for zones.
 }
 p_uma_zctor_args=^uma_zctor_args;
 uma_zctor_args=record
  name  :pchar;
  size  :QWORD;
  ctor  :uma_ctor;
  dtor  :uma_dtor;
  uminit:uma_init;
  fini  :uma_fini;
  keg   :uma_keg_t;
  align :Integer;
  flags :DWORD;
 end;

 p_uma_kctor_args=^uma_kctor_args;
 uma_kctor_args=record
  zone  :uma_zone_t;
  size  :QWORD;
  uminit:uma_init;
  fini  :uma_fini;
  align :Integer;
  flags :DWORD;
 end;

 p_uma_bucket_zone=^uma_bucket_zone;
 uma_bucket_zone=record
  ubz_zone   :uma_zone_t;
  ubz_name   :pchar;
  ubz_entries:Integer;
 end;

const
 BUCKET_MAX=128;

 bucket_zones:array[0..4] of uma_bucket_zone=(
  (ubz_zone:nil;ubz_name:'16 Bucket' ;ubz_entries:16),
  (ubz_zone:nil;ubz_name:'32 Bucket' ;ubz_entries:32),
  (ubz_zone:nil;ubz_name:'64 Bucket' ;ubz_entries:64),
  (ubz_zone:nil;ubz_name:'128 Bucket';ubz_entries:128),
  (ubz_zone:nil;ubz_name:nil;ubz_entries:0)
 );

const
 BUCKET_SHIFT=4;
 BUCKET_ZONES_CONST=((BUCKET_MAX shr BUCKET_SHIFT) + 1);

{
 * bucket_size[] maps requested bucket sizes to zones that allocate a bucket
 * of approximately the right size.
 }
var
 bucket_size:array[0..BUCKET_ZONES_CONST-1] of Byte;

{
 * Flags and enumerations to be passed to internal functions.
}
type
 zfreeskip=(SKIP_NONE,SKIP_DTOR,SKIP_FINI);

const
 ZFREE_STATFAIL=$00000001; { Update zone failure statistic. }
 ZFREE_STATFREE=$00000002; { Update zone free statistic. }

{ Prototypes.. }

type
 t_zfunc=procedure(z:uma_zone_t);

//static void *obj_alloc(uma_zone_t, int, u_int8_t *, int);
function  page_alloc(zone:uma_zone_t;bytes:Integer;pflag:pbyte;wait:Integer):Pointer;
function  startup_alloc(zone:uma_zone_t;bytes:Integer;pflag:pbyte;wait:Integer):Pointer;
procedure page_free(mem:Pointer;size:Integer;flags:Byte);
function  keg_alloc_slab(keg:uma_keg_t;zone:uma_zone_t;wait:Integer):uma_slab_t;
procedure cache_drain(zone:uma_zone_t);
procedure bucket_drain(zone:uma_zone_t;bucket:uma_bucket_t);
procedure bucket_cache_drain(zone:uma_zone_t);
function  keg_ctor(mem:Pointer;size:Integer;udata:Pointer;flags:Integer):Integer;
procedure keg_dtor(arg:Pointer;size:Integer;udata:Pointer);
function  zone_ctor(mem:Pointer;size:Integer;udata:Pointer;flags:Integer):Integer;
procedure zone_dtor(arg:Pointer;size:Integer;udata:Pointer);
function  zero_init(mem:Pointer;size,flags:Integer):Integer;
procedure keg_small_init(keg:uma_keg_t);
procedure keg_large_init(keg:uma_keg_t);
procedure zone_foreach(zfunc:t_zfunc);
procedure zone_timeout(zone:uma_zone_t);
function  hash_alloc(hash:p_uma_hash):Integer;
function  hash_expand(oldhash,newhash:p_uma_hash):Integer;
procedure hash_free(hash:p_uma_hash);
procedure uma_startup4();
function  zone_alloc_item(zone:uma_zone_t;udata:Pointer;flags:Integer):Pointer;
procedure zone_free_item(zone:uma_zone_t;item,udata:Pointer;skip:zfreeskip;flags:Integer);
procedure bucket_enable();
procedure bucket_init();
function  bucket_alloc(entries,bflags:Integer):uma_bucket_t;
procedure bucket_free(bucket:uma_bucket_t);
procedure bucket_zone_drain();
function  zone_alloc_bucket(zone:uma_zone_t;flags:Integer):Integer;
function  zone_fetch_slab(zone:uma_zone_t;keg:uma_keg_t;flags:Integer):uma_slab_t;
function  zone_fetch_slab_multi(zone:uma_zone_t;last:uma_keg_t;rflags:Integer):uma_slab_t;
function  slab_alloc_item(zone:uma_zone_t;slab:uma_slab_t):Pointer;
function  uma_kcreate(zone:uma_zone_t;size:QWORD;uminit:uma_init;fini:uma_fini;align:Integer;flags:DWORD):uma_keg_t;
procedure zone_relock(zone:uma_zone_t;keg:uma_keg_t);
procedure keg_relock(keg:uma_keg_t;zone:uma_zone_t);
//
procedure uma_print_zone(zone:uma_zone_t);
procedure uma_print_stats();

//static int sysctl_vm_zone_count(SYSCTL_HANDLER_ARGS);
//static int sysctl_vm_zone_stats(SYSCTL_HANDLER_ARGS);

{
SYSINIT(uma_startup3, SI_SUB_VM_CONF, SI_ORDER_SECOND, uma_startup3, nil);

SYSCTL_PROC(_vm, OID_AUTO, zone_count, CTLFLAG_RD|CTLTYPE_INT, 0, 0, sysctl_vm_zone_count, 'I', 'Number of UMA zones');

SYSCTL_PROC(_vm, OID_AUTO, zone_stats, CTLFLAG_RD|CTLTYPE_STRUCT, 0, 0, sysctl_vm_zone_stats, 's,struct uma_type_header', 'Zone Stats');
}

implementation

uses
 kern_thr,
 kern_daemon;

const
 UMA_SLAB_GRANULARITY=MD_ALLOC_GRANULARITY div MD_PAGE_SIZE;

//fake round robin per CPU

const
 mp_maxid=7;
 mp_ncpus=8;

var
 cpu_counter:DWORD=0;

 cpu_mtx:array[0..mp_maxid] of mtx;

function CPU_FOREACH(var i:Integer):Boolean; inline;
begin
 Result:=(i<=mp_maxid);
 Inc(i);
end;

procedure critical_enter();
var
 curcpu:Integer;
begin
 with curkthread^ do
 begin
  curcpu:=(pcb_curcpu-1);
  if (curcpu=-1) then
  begin
   curcpu:=System.InterlockedExchangeAdd(cpu_counter,1) mod mp_ncpus;
   pcb_curcpu:=(curcpu+1);
   pcb_cpuref:=0;
  end;
  Inc(pcb_cpuref);
 end;
 mtx_lock(cpu_mtx[curcpu]);
end;

procedure critical_exit();
var
 curcpu:Integer;
begin
 with curkthread^ do
 begin
  curcpu:=(pcb_curcpu-1);
  if (curcpu<>-1) then
  begin
   mtx_unlock(cpu_mtx[curcpu]);
   Dec(pcb_cpuref);
   if (pcb_cpuref=0) then
   begin
    pcb_curcpu:=0;
   end;
  end;
 end;
end;

function curcpu():Integer;
begin
 Result:=(curkthread^.pcb_curcpu-1);
end;

procedure curcpu_startup;
var
 i:Integer;
begin
 For i:=0 to mp_maxid do
 begin
  mtx_init(cpu_mtx[i],'PCPU');
 end;
end;

function uma_zcreate(name  :pchar;
                     size  :QWORD;
                     ctor  :uma_ctor;
                     dtor  :uma_dtor;
                     uminit:uma_init;
                     fini  :uma_fini;
                     align :Integer;
                     flags :DWORD
                    ):uma_zone_t; forward;

{
 * This routine checks to see whether or not it's safe to enable buckets.
 }

procedure bucket_enable();
begin
 bucketdisable:=0;
 //bucketdisable:=vm_page_count_min();
end;

function howmany(x,y:QWORD):QWORD; inline;
begin
 Result:=(x+(y-1)) div y;
end;

function roundup(x,y:QWORD):QWORD; inline;
begin
 Result:=((x+(y-1)) div y)*y;
end;

{
 * Initialize bucket_zones, the array of zones of buckets of various sizes.
 *
 * For each zone, calculate the memory required for each bucket, consisting
 * of the header and an array of pointers.  Initialize bucket_size[] to point
 * the range of appropriate bucket sizes at the zone.
 }
procedure bucket_init();
var
 ubz:p_uma_bucket_zone;
 i,j:Integer;
 size:Integer;
begin
 i:=0;
 j:=0;
 while (bucket_zones[j].ubz_entries<>0) do
 begin

   ubz:=@bucket_zones[j];
   size:=roundup(sizeof(uma_bucket), sizeof(Pointer));
   size:=size + sizeof(Pointer) * ubz^.ubz_entries;
   ubz^.ubz_zone:=uma_zcreate(ubz^.ubz_name, size,
       nil, nil, nil, nil, UMA_ALIGN_PTR,
       UMA_ZFLAG_INTERNAL or UMA_ZFLAG_BUCKET);

   while (i <= ubz^.ubz_entries) do
   begin
    bucket_size[i shr BUCKET_SHIFT]:=j;
    //
    i:=i + (1 shl BUCKET_SHIFT);
   end;

  //
  Inc(j);
 end;

end;

{
 * Given a desired number of entries for a bucket, Exitthe zone from which
 * to allocate the bucket.
 }
function bucket_zone_lookup(entries:Integer):p_uma_bucket_zone;
var
 idx:Integer;
begin
 idx:=howmany(entries, 1 shl BUCKET_SHIFT);
 Exit(@bucket_zones[bucket_size[idx]]);
end;

function bucket_alloc(entries,bflags:Integer):uma_bucket_t;
var
 ubz:p_uma_bucket_zone;
 bucket:uma_bucket_t;
begin
 {
  * This is to stop us from allocating per cpu buckets while we're
  * running out of vm.boot_pages.  Otherwise, we would exhaust the
  * boot pages.  This also prevents us from allocating buckets in
  * low memory situations.
  }
 if (bucketdisable<>0) then
 begin
  Exit(nil);
 end;

 ubz:=bucket_zone_lookup(entries);
 bucket:=zone_alloc_item(ubz^.ubz_zone, nil, bflags);
 if (bucket<>nil) then
 begin
  FillChar(bucket^.ub_bucket, sizeof(Pointer) * ubz^.ubz_entries, 0);
  bucket^.ub_cnt    :=0;
  bucket^.ub_entries:=ubz^.ubz_entries;
 end;

 Exit(bucket);
end;

procedure bucket_free(bucket:uma_bucket_t);
var
 ubz:p_uma_bucket_zone;
begin
 ubz:=bucket_zone_lookup(bucket^.ub_entries);
 zone_free_item(ubz^.ubz_zone, bucket, nil, SKIP_NONE, ZFREE_STATFREE);
end;

procedure zone_drain(zone:uma_zone_t); forward;

procedure bucket_zone_drain();
var
 ubz:p_uma_bucket_zone;
begin
 ubz:=@bucket_zones[0];
 while (ubz^.ubz_entries<>0) do
 begin
  zone_drain(ubz^.ubz_zone);
  //
  Inc(ubz);
 end;
end;

function zone_first_keg(zone:uma_zone_t):uma_keg_t; inline;
begin
 Exit(uma_klink_t(LIST_FIRST(@zone^.uz_kegs))^.kl_keg);
end;

type
 t_kegfn=procedure(k:uma_keg_t);

procedure zone_foreach_keg(zone:uma_zone_t;kegfn:t_kegfn);
var
 klink:uma_klink_t;
begin
 klink:=LIST_FIRST(@zone^.uz_kegs);
 while (klink<>nil) do
 begin
  kegfn(klink^.kl_keg);
  //
  klink:=LIST_NEXT(klink,@klink^.kl_link);
 end;
end;

{
 * Routine called by timeout which is used to fire off some time interval
 * based calculations.  (stats, hash size, etc.)
 *
 * Arguments:
 * arg   Unused
 *
 * Returns:
 * Nothing
 }
procedure uma_timeout();
begin
 if (uma_callout=0) then
 begin
  uma_callout:=get_unit_uptime;
 end;

 if (get_unit_uptime - uma_callout) >= (UMA_TIMEOUT_CONST * hz) then
 begin
  bucket_enable();
  zone_foreach(@zone_timeout);
  //
  uma_reclaim();
  //
  uma_callout:=get_unit_uptime;
 end;
end;

{
 * Routine to perform timeout driven calculations.  This expands the
 * hashes and does per cpu statistics aggregation.
 *
 *  Returns nothing.
 }
procedure keg_timeout(keg:uma_keg_t);
var
 newhash:uma_hash;
 oldhash:uma_hash;
 ret:Integer;
begin
 KEG_LOCK(keg);
 {
  * Expand the keg hash table.
  *
  * This is done if the number of slabs is larger than the hash size.
  * What I'm trying to do here is completely reduce collisions.  This
  * may be a little aggressive.  Should I allow for two collisions max?
  }
 if ((keg^.uk_flags and UMA_ZONE_HASH)<>0) and
    (keg^.uk_pages div keg^.uk_ppera >= keg^.uk_hash.uh_hashsize) then
 begin
  {
   * This is so involved because allocating and freeing
   * while the keg lock is held will lead to deadlock.
   * I have to do everything in stages and check for
   * races.
   }
  newhash:=keg^.uk_hash;
  KEG_UNLOCK(keg);
  ret:=hash_alloc(@newhash);
  KEG_LOCK(keg);
  if (ret<>0) then
  begin
   if (hash_expand(@keg^.uk_hash, @newhash)<>0) then
   begin
    oldhash:=keg^.uk_hash;
    keg^.uk_hash:=newhash;
   end else
   begin
    oldhash:=newhash;
   end;

   KEG_UNLOCK(keg);
   hash_free(@oldhash);
   KEG_LOCK(keg);
  end;
 end;
 KEG_UNLOCK(keg);
end;

procedure zone_timeout(zone:uma_zone_t);
begin
 zone_foreach_keg(zone, @keg_timeout);
end;

{
 * Allocate and zero fill the next sized hash table from the appropriate
 * backing store.
 *
 * Arguments:
 * hash  A new hash structure with the old hash size in uh_hashsize
 *
 * Returns:
 * 1 on sucess and 0 on failure.
 }
function hash_alloc(hash:p_uma_hash):Integer;
var
 oldsize:Integer;
 alloc  :Integer;
begin
 oldsize:=hash^.uh_hashsize;

 { We're just going to go to a power of two greater }
 if (oldsize<>0) then
 begin
  hash^.uh_hashsize:=oldsize * 2;
  alloc:=sizeof(hash^.uh_slab_hash[0]) * hash^.uh_hashsize;
  hash^.uh_slab_hash:=AllocMem(alloc);
 end else
 begin
  alloc:=sizeof(hash^.uh_slab_hash[0]) * UMA_HASH_SIZE_INIT;
  hash^.uh_slab_hash:=zone_alloc_item(hashzone, nil, M_WAITOK);
  hash^.uh_hashsize:=UMA_HASH_SIZE_INIT;
 end;

 if (hash^.uh_slab_hash<>nil) then
 begin
  FillChar(hash^.uh_slab_hash^, alloc, 0);
  hash^.uh_hashmask:=hash^.uh_hashsize - 1;
  Exit(1);
 end;

 Exit(0);
end;

{
 * Expands the hash table for HASH zones.  This is done from zone_timeout
 * to reduce collisions.  This must not be done in the regular allocation
 * path, otherwise, we can recurse on the vm while allocating pages.
 *
 * Arguments:
 * oldhash  The hash you want to expand
 * newhash  The hash structure for the new table
 *
 * Returns:
 * Nothing
 *
 * Discussion:
 }
function hash_expand(oldhash,newhash:p_uma_hash):Integer;
var
 slab:uma_slab_t;
 hval,i:Integer;
begin
 if (newhash^.uh_slab_hash=nil) then
 begin
  Exit(0);
 end;

 if (oldhash^.uh_hashsize >= newhash^.uh_hashsize) then
 begin
  Exit(0);
 end;

 {
  * I need to investigate hash algorithms for resizing without a
  * full rehash.
  }

 if (oldhash^.uh_hashsize<>0) then
 for i:=0 to oldhash^.uh_hashsize-1 do
  while (not SLIST_EMPTY(@oldhash^.uh_slab_hash[i])) do
  begin
   slab:=SLIST_FIRST(@oldhash^.uh_slab_hash[i]);
   SLIST_REMOVE_HEAD(@oldhash^.uh_slab_hash[i], @uma_slab_t(nil)^.us_hlink);
   hval:=UMA_HASH_(newhash, slab^.us_data);
   SLIST_INSERT_HEAD(@newhash^.uh_slab_hash[hval], slab, @slab^.us_hlink);
  end;

 Exit(1);
end;

{
 * Free the hash bucket to the appropriate backing store.
 *
 * Arguments:
 * slab_hash  The hash bucket we're freeing
 * hashsize   The number of entries in that hash bucket
 *
 * Returns:
 * Nothing
 }
procedure hash_free(hash:p_uma_hash);
begin
 if (hash^.uh_slab_hash=nil) then
 begin
  Exit;
 end;

 if (hash^.uh_hashsize=UMA_HASH_SIZE_INIT) then
 begin
  zone_free_item(hashzone, hash^.uh_slab_hash, nil, SKIP_NONE, ZFREE_STATFREE);
 end else
 begin
  FreeMem(hash^.uh_slab_hash);
 end;
end;

{
 * Frees all outstanding items in a bucket
 *
 * Arguments:
 * zone   The zone to free to, must be unlocked.
 * bucket The free/alloc bucket with items, cpu queue must be locked.
 *
 * Returns:
 * Nothing
 }

procedure bucket_drain(zone:uma_zone_t;bucket:uma_bucket_t);
var
 item:Pointer;
begin
 if (bucket=nil) then
 begin
  Exit;
 end;

 while (bucket^.ub_cnt > 0) do
 begin
  with bucket^ do
  begin
   Dec(ub_cnt);
   item:=ub_bucket[ub_cnt];
   ub_bucket[ub_cnt]:=nil;
  end;
  Assert(item<>nil, 'bucket_drain: botched ptr, item is nil');
  zone_free_item(zone, item, nil, SKIP_DTOR, 0);
 end;
end;

{
 * Drains the per cpu caches for a zone.
 *
 * NOTE: This may only be called while the zone is being turn down, and not
 * during normal operation.  This is necessary in order that we do not have
 * to migrate CPUs to drain the per-CPU caches.
 *
 * Arguments:
 * zone     The zone to drain, must be unlocked.
 *
 * Returns:
 * Nothing
 }
procedure cache_drain(zone:uma_zone_t);
var
 cache:uma_cache_t;
 cpu:Integer;
begin
 {
  * XXX: It is safe to not lock the per-CPU caches, because we're
  * tearing down the zone anyway.  I.e., there will be no further use
  * of the caches at this point.
  *
  * XXX: It would good to be able to assert that the zone is being
  * torn down to prevent improper use of cache_drain().
  *
  * XXX: We lock the zone before passing into bucket_cache_drain() as
  * it is used elsewhere.  Should the tear-down path be made special
  * there in some form?
  }
 cpu:=0;
 while CPU_FOREACH(cpu) do
 begin
  cache:=@zone^.uz_cpu[cpu];

  with cache^ do
  begin
   bucket_drain(zone, uc_allocbucket);
   bucket_drain(zone, uc_freebucket);
   if (uc_allocbucket<>nil) then
   begin
    bucket_free(uc_allocbucket);
   end;
   if (uc_freebucket<>nil) then
   begin
    bucket_free(uc_freebucket);
   end;
   uc_allocbucket:=nil;
   uc_freebucket :=nil;
  end;

 end;
 ZONE_LOCK(zone);
 bucket_cache_drain(zone);
 ZONE_UNLOCK(zone);
end;

{
 * Drain the cached buckets from a zone.  Expects a locked zone on entry.
 }
procedure bucket_cache_drain(zone:uma_zone_t);
var
 bucket:uma_bucket_t;
begin
 {
  * Drain the bucket queues and free the buckets, we just keep two per
  * cpu (alloc/free).
  }
 bucket:=LIST_FIRST(@zone^.uz_full_bucket);
 while (bucket<>nil) do
 begin
  LIST_REMOVE(bucket, @bucket^.ub_link);
  ZONE_UNLOCK(zone);
  bucket_drain(zone, bucket);
  bucket_free(bucket);
  ZONE_LOCK(zone);
  //
  bucket:=LIST_FIRST(@zone^.uz_full_bucket);
 end;

 { Now we do the free queue.. }
 bucket:=LIST_FIRST(@zone^.uz_free_bucket);
 while (bucket<>nil) do
 begin
  LIST_REMOVE(bucket, @bucket^.ub_link);
  bucket_free(bucket);
  //
  bucket:=LIST_FIRST(@zone^.uz_free_bucket);
 end;
end;

function get_item_addr(keg:uma_keg_t;slab:uma_slab_t;i:Integer):Pointer; inline;
begin
 Result:=slab^.us_data + (keg^.uk_rsize * i);
end;

{
 * Frees pages from a keg back to the system.  This is done on demand from
 * the pageout daemon.
 *
 * Returns nothing.
 }
procedure keg_drain(keg:uma_keg_t);
label
 finished;
var
 freeslabs:slabhead;
 slab:uma_slab_t;
 n:uma_slab_t;
 flags:byte;
 mem:pbyte;
 i:Integer;
begin
 freeslabs:=Default(slabhead);

 {
  * We don't want to take pages from statically allocated kegs at this
  * time
  }
 if ((keg^.uk_flags and UMA_ZONE_NOFREE)<>0) or (keg^.uk_freef=nil) then
 begin
  Exit;
 end;

 KEG_LOCK(keg);
 if (keg^.uk_free=0) then
 begin
  goto finished;
 end;

 slab:=LIST_FIRST(@keg^.uk_free_slab);
 while (slab<>nil) do
 begin
  n:=LIST_NEXT(slab, @slab^.us_link);

  { We have no where to free these to }
  if ((slab^.us_flags and UMA_SLAB_BOOT)<>0) then
  begin
   slab:=n;
   continue;
  end;

  LIST_REMOVE(slab, @slab^.us_link);
  keg^.uk_pages:=keg^.uk_pages - keg^.uk_ppera;
  keg^.uk_free :=keg^.uk_free  - keg^.uk_ipers;

  if ((keg^.uk_flags and UMA_ZONE_HASH)<>0) then
  begin
   UMA_HASH_REMOVE(@keg^.uk_hash, slab, slab^.us_data);
  end;

  SLIST_INSERT_HEAD(@freeslabs, slab, @slab^.us_hlink);

  slab:=n;
 end;

finished:
 KEG_UNLOCK(keg);

 slab:=SLIST_FIRST(@freeslabs);
 while (slab<>nil) do
 begin
  SLIST_REMOVE(@freeslabs, slab, @slab^.us_hlink);

  if (keg^.uk_fini<>nil) then
   if (keg^.uk_ipers<>0) then
   For i:=0 to keg^.uk_ipers-1 do
   begin
    keg^.uk_fini(get_item_addr(keg,slab,i), keg^.uk_size);
   end;

  flags:=slab^.us_flags;
  mem  :=slab^.us_data;

  if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)<>0) then
  begin
   zone_free_item(keg^.uk_slabzone, slab, nil, SKIP_NONE, ZFREE_STATFREE);
  end;

  keg^.uk_freef(mem, UMA_SLAB_SIZE * keg^.uk_ppera, flags);
  //
  slab:=SLIST_FIRST(@freeslabs);
 end;
end;

procedure zone_drain_wait(zone:uma_zone_t;waitok:Integer);
label
 _out;
begin

 {
  * Set draining to interlock with zone_dtor() so we can release our
  * locks as we go.  Only dtor() should do a WAITOK call since it
  * is the only call that knows the structure will still be available
  * when it wakes up.
  }
 ZONE_LOCK(zone);
 while ((zone^.uz_flags and UMA_ZFLAG_DRAINING)<>0) do
 begin
  if (waitok=M_NOWAIT) then
  begin
   goto _out;
  end;
  msleep(zone, zone^.uz_lock, PVM, 'zonedrain', 1);
 end;

 zone^.uz_flags:=zone^.uz_flags or UMA_ZFLAG_DRAINING;
 bucket_cache_drain(zone);
 ZONE_UNLOCK(zone);
 {
  * The DRAINING flag protects us from being freed while
  * we're running.  Normally the uma_mtx would protect us but we
  * must be able to release and acquire the right lock for each keg.
  }
 zone_foreach_keg(zone, @keg_drain);
 ZONE_LOCK(zone);
 zone^.uz_flags:=zone^.uz_flags and (not UMA_ZFLAG_DRAINING);
 wakeup(zone);
_out:
 ZONE_UNLOCK(zone);
end;

procedure zone_drain(zone:uma_zone_t); public;
begin
 zone_drain_wait(zone, M_NOWAIT);
end;

{
 * Allocate a new slab for a keg.  This does not insert the slab onto a list.
 *
 * Arguments:
 * wait  Shall we wait?
 *
 * Returns:
 * The slab that was allocated or nil if there is no memory and the
 * caller specified M_NOWAIT.
 }
function keg_alloc_slab(keg:uma_keg_t;zone:uma_zone_t;wait:Integer):uma_slab_t;
var
 allocf:uma_alloc;
 slab:uma_slab_t;
 mem:PByte;
 flags:Byte;
 i:Integer;
begin
 mtx_assert(keg^.uk_lock);
 slab:=nil;

 allocf:=keg^.uk_allocf;
 KEG_UNLOCK(keg);

 if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)<>0) then
 begin
  slab:=zone_alloc_item(keg^.uk_slabzone, nil, wait);
  if (slab=nil) then
  begin
   KEG_LOCK(keg);
   Exit(nil);
  end;
 end;

 {
  * This reproduces the old vm_zone behavior of zero filling pages the
  * first time they are added to a zone.
  *
  * Malloced items are zeroed in uma_zalloc.
  }
 wait:=wait or M_ZERO;

 if ((keg^.uk_flags and UMA_ZONE_NODUMP)<>0) then
 begin
  wait:=wait or M_NODUMP;
 end;

 { zone is passed for legacy reasons. }
 mem:=allocf(zone, keg^.uk_ppera * UMA_SLAB_SIZE, @flags, wait);
 if (mem=nil) then
 begin
  if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)<>0) then
  begin
   zone_free_item(keg^.uk_slabzone, slab, nil, SKIP_NONE, ZFREE_STATFREE);
  end;
  KEG_LOCK(keg);
  Exit(nil);
 end;

 { Point the slab into the allocated memory }
 if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)=0) then
 begin
  Assert(keg^.uk_pgoff<>0);
  slab:=uma_slab_t(mem + keg^.uk_pgoff);
 end;

 slab^.us_keg      :=keg;
 slab^.us_data     :=mem;
 slab^.us_flags    :=flags;
 slab^.us_freecount:=keg^.uk_ipers;
 slab^.us_firstfree:=0;

 if (keg^.uk_ipers<>0) then
 For i:=0 to keg^.uk_ipers-1 do
 begin
  slab^.us_freelist[i].us_item:=i+1;
 end;

 if (keg^.uk_init<>nil) then
 begin
  i:=0;
  while (i < keg^.uk_ipers) do
  begin
   if (keg^.uk_init(get_item_addr(keg,slab,i), keg^.uk_size, wait)<>0) then
   begin
    break;
   end;
   //
   Inc(i);
  end;

  if (i<>keg^.uk_ipers) then
  begin
   if (keg^.uk_fini<>nil) then
   begin
    Dec(i);
    while (i > -1) do
    begin
     keg^.uk_fini(get_item_addr(keg,slab,i), keg^.uk_size);
     //
     Dec(i);
    end;
   end;

   if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)<>0) then
   begin
    zone_free_item(keg^.uk_slabzone, slab, nil, SKIP_NONE, ZFREE_STATFREE);
   end;

   keg^.uk_freef(mem, UMA_SLAB_SIZE * keg^.uk_ppera, flags);
   KEG_LOCK(keg);
   Exit(nil);
  end;
 end;

 KEG_LOCK(keg);

 if ((keg^.uk_flags and UMA_ZONE_HASH)<>0) then
 begin
  UMA_HASH_INSERT(@keg^.uk_hash, slab, mem);
 end;

 keg^.uk_pages:=keg^.uk_pages + keg^.uk_ppera;
 keg^.uk_free :=keg^.uk_free  + keg^.uk_ipers;

 Exit(slab);
end;

{
 * This function is intended to be used early on in place of page_alloc() so
 * that we may use the boot time page cache to satisfy allocations before
 * the VM is ready.
 }
function startup_alloc(zone:uma_zone_t;bytes:Integer;pflag:pbyte;wait:Integer):Pointer;
var
 keg:uma_keg_t;
 tmps:uma_slab_t;
 pages, check_pages:Integer;
begin
 keg:=zone_first_keg(zone);
 pages:=howmany(bytes, UMA_SLAB_SIZE);
 check_pages:=pages - 1;
 Assert(pages > 0, 'startup_alloc can`t reserve 0 pages');

 {
  * Check our small startup cache to see if it has pages remaining.
  }
 mtx_lock(uma_boot_pages_mtx);

 { First check if we have enough room. }
 tmps:=LIST_FIRST(@uma_boot_pages);

 while (tmps<>nil) and (check_pages > 0) do
 begin
  Dec(check_pages);
  //
  tmps:=LIST_NEXT(tmps, @tmps^.us_link);
 end;

 if (tmps<>nil) then
 begin
  {
   * It's ok to lose tmps references.  The last one will
   * have tmps^.us_data pointing to the start address of
   * 'pages' contiguous pages of memory.
   }
  while (pages > 0) do
  begin
   Dec(pages);
   //
   tmps:=LIST_FIRST(@uma_boot_pages);
   LIST_REMOVE(tmps, @tmps^.us_link);
  end;
  mtx_unlock(uma_boot_pages_mtx);
  pflag^:=tmps^.us_flags;
  Exit(tmps^.us_data);
 end;
 mtx_unlock(uma_boot_pages_mtx);

 if (booted < UMA_STARTUP2_CONST) then
 begin
  Assert(False,'UMA: Increase vm.boot_pages');
 end;

 {
  * Now that we've booted reset these users to their real allocator.
  }
 keg^.uk_allocf:=uma_alloc(@page_alloc);

 Exit(keg^.uk_allocf(zone, bytes, pflag, wait));
end;

{$IF UMA_SLAB_GRANULARITY>1}
var
 uma_free_pages_lock:mtx;
 uma_free_pages_list:LIST_HEAD=(lh_first:nil);

procedure insert_freepage(p:Pointer); inline;
begin
 PDWORD(p+(MD_PAGE_SIZE-4))^:=$DEADBEEF;
 LIST_INSERT_HEAD(@uma_free_pages_list,p,p);
end;

procedure remove_freepage(p:Pointer); inline;
begin
 LIST_REMOVE(p,p);
 PDWORD(p+(MD_PAGE_SIZE-4))^:=0;
end;

function test_freepage(p:Pointer):Boolean; inline;
begin
 Result:=(PDWORD(p+(MD_PAGE_SIZE-4))^=$DEADBEEF);
end;

procedure drop_freepage(p:Pointer); inline;
begin
 LIST_REMOVE(p,p);
end;
{$ENDIF}

{
 * Allocates a number of pages from the system
 *
 * Arguments:
 * bytes  The number of bytes requested
 * wait  Shall we wait?
 *
 * Returns:
 * A pointer to the alloced memory or possibly
 * nil if M_NOWAIT is set.
 }
function page_alloc(zone:uma_zone_t;bytes:Integer;pflag:pbyte;wait:Integer):Pointer;
var
 p:Pointer; { Returned page }
 i:Integer;
begin
 pflag^:=UMA_SLAB_KMEM;

 {$IF UMA_SLAB_GRANULARITY>1}
  if (bytes=MD_PAGE_SIZE) then
  begin
   //sub allocator
   mtx_lock(uma_free_pages_lock);

    //get free?
    p:=LIST_FIRST(@uma_free_pages_list);

    if (p=nil) then
    begin
     p:=kmem_alloc(MD_ALLOC_GRANULARITY, VM_RW);
     //save to list
     For i:=UMA_SLAB_GRANULARITY-1 downto 1 do
     begin
      insert_freepage(p+MD_PAGE_SIZE*i)
     end;
    end else
    begin
     remove_freepage(p);
    end;

   mtx_unlock(uma_free_pages_lock);
   Exit(p);
  end;
 {$ENDIF}

 p:=kmem_alloc(bytes, VM_RW);

 Exit(p);
end;

{
 * Frees a number of pages to the system
 *
 * Arguments:
 * mem   A pointer to the memory to be freed
 * size  The size of the memory being freed
 * flags The original p^.us_flags field
 *
 * Returns:
 * Nothing
 }
procedure page_free(mem:Pointer;size:Integer;flags:Byte);
var
 i:Integer;
begin

 {$IF UMA_SLAB_GRANULARITY>1}
  if (size=MD_PAGE_SIZE) then
  begin

   //sub allocator
   mtx_lock(uma_free_pages_lock);

    insert_freepage(mem);

    //get base addres
    mem:=Pointer(QWORD(mem) and (not (MD_ALLOC_GRANULARITY-1)));

    //test
    For i:=0 to UMA_SLAB_GRANULARITY-1 do
    begin
     if not test_freepage(mem+MD_PAGE_SIZE*i) then
     begin
      //not all
      mtx_unlock(uma_free_pages_lock);
      Exit;
     end;
    end;

    //remove nodes
    For i:=0 to UMA_SLAB_GRANULARITY-1 do
    begin
     drop_freepage(mem+MD_PAGE_SIZE*i);
    end;

    //free all
    kmem_free(mem, MD_ALLOC_GRANULARITY);

   mtx_unlock(uma_free_pages_lock);
   Exit;
  end;
 {$ENDIF}

 kmem_free(mem, size);
end;

{
 * Zero fill initializer
 *
 * Arguments/Returns follow uma_init specifications
 }
function zero_init(mem:Pointer;size,flags:Integer):Integer;
begin
 FillChar(mem^, size, 0);
 Exit(0);
end;

function check_wasted(keg:uma_keg_t;rsize,shsize:DWORD):Boolean; inline;
var
 memused    :DWORD;
 wastedspace:DWORD;
begin
 memused    :=keg^.uk_ipers * rsize + shsize;
 wastedspace:=UMA_SLAB_SIZE - memused;

 Result:=(wastedspace >= UMA_MAX_WASTE) and (keg^.uk_ipers < (UMA_SLAB_SIZE div keg^.uk_rsize));
end;

{
 * Finish creating a small uma keg.  This calculates ipers, and the keg size.
 *
 * Arguments
 * keg  The zone we should initialize
 *
 * Returns
 * Nothing
 }
procedure keg_small_init(keg:uma_keg_t);
label
 _start;
var
 rsize :DWORD;
 linksz:DWORD;
 shsize:DWORD;
begin
 _start:

 Assert(keg<>nil, 'Keg is nil in keg_small_init');
 rsize:=keg^.uk_size;

 if (rsize < UMA_SMALLEST_UNIT) then
 begin
  rsize:=UMA_SMALLEST_UNIT;
 end;

 if ((rsize and keg^.uk_align)<>0) then
 begin
  rsize:=(rsize and (not keg^.uk_align)) + (keg^.uk_align + 1);
 end;

 Assert(rsize<>0);

 keg^.uk_rsize:=rsize;
 keg^.uk_ppera:=1;

 if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)<>0) then
 begin
  linksz:=0;
  shsize:=0;
 end else
 begin
  linksz:=UMA_FRITM_SZ; { Account for linkage }
  shsize:=sizeof(uma_slab);
 end;

 rsize :=rsize + linksz;

 keg^.uk_ipers:=(UMA_SLAB_SIZE - shsize) div rsize;

 Assert(keg^.uk_ipers<>0, 'keg_small_init: ipers is 0');

 {
  * We can't do OFFPAGE if we're internal or if we've been
  * asked to not go to the VM for buckets.  If we do this we
  * may end up going to the VM (kmem_map) for slabs which we
  * do not want to do if we're UMA_ZFLAG_CACHEONLY as a
  * result of UMA_ZONE_VM, which clearly forbids it.
  }
 if ((keg^.uk_flags and UMA_ZFLAG_INTERNAL )<>0) or
    ((keg^.uk_flags and UMA_ZFLAG_CACHEONLY)<>0) then
 begin
  Exit;
 end;

 if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)=0) then
 if check_wasted(keg,rsize,shsize) then
 begin

  //keg^.uk_ipers:=UMA_SLAB_SIZE div keg^.uk_rsize;
  //Assert(keg^.uk_ipers <= High(us_word), 'keg_small_init: keg^.uk_ipers too high!');

  keg^.uk_flags:=keg^.uk_flags or (UMA_ZONE_OFFPAGE or UMA_ZONE_HASH);

  goto _start;
 end;
end;

{
 * Finish creating a large (> UMA_SLAB_SIZE) uma kegs.  Just give in and do
 * OFFPAGE for now.  When I can allow for more dynamic slab sizes this will be
 * more complicated.
 *
 * Arguments
 * keg  The keg we should initialize
 *
 * Returns
 * Nothing
 }
procedure keg_large_init(keg:uma_keg_t);
var
 pages:Integer;
begin
 Assert(keg<>nil, 'Keg is nil in keg_large_init');
 Assert((keg^.uk_flags and UMA_ZFLAG_CACHEONLY)=0, 'keg_large_init: Cannot large-init a UMA_ZFLAG_CACHEONLY keg');

 pages:=keg^.uk_size div UMA_SLAB_SIZE;

 { Account for remainder }
 if ((pages * UMA_SLAB_SIZE) < keg^.uk_size) then
 begin
  Inc(pages);
 end;

 keg^.uk_ppera:=pages;
 keg^.uk_ipers:=1;
 keg^.uk_rsize:=keg^.uk_size;

 { We can't do OFFPAGE if we're internal, bail out here. }
 if ((keg^.uk_flags and UMA_ZFLAG_INTERNAL)<>0) then
 begin
  Exit;
 end;

 keg^.uk_flags:=keg^.uk_flags or (UMA_ZONE_OFFPAGE or UMA_ZONE_HASH);
end;

function Min(a,b:PtrUInt):PtrUInt; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function Max(a,b:PtrUInt):PtrUInt; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

{
 * Keg header ctor.  This initializes all fields, locks, etc.  And inserts
 * the keg onto the global keg list.
 *
 * Arguments/Returns follow uma_ctor specifications
 * udata  Actually uma_kctor_args
 }
function keg_ctor(mem:Pointer;size:Integer;udata:Pointer;flags:Integer):Integer;
var
 arg:p_uma_kctor_args;
 keg:uma_keg_t;
 zone:uma_zone_t;
 totsize:DWORD;
begin
 arg:=udata;
 keg:=mem;

 FillChar(keg^,size,0);
 keg^.uk_size    :=arg^.size;
 keg^.uk_init    :=arg^.uminit;
 keg^.uk_fini    :=arg^.fini;
 keg^.uk_align   :=arg^.align;
 keg^.uk_free    :=0;
 keg^.uk_pages   :=0;
 keg^.uk_flags   :=arg^.flags;
 keg^.uk_allocf  :=uma_alloc(@page_alloc);
 keg^.uk_freef   :=@page_free;
 keg^.uk_recurse :=0;
 keg^.uk_slabzone:=nil;

 {
  * The master zone is passed to us at keg-creation time.
  }
 zone:=arg^.zone;
 keg^.uk_name:=zone^.uz_name;

 if ((arg^.flags and UMA_ZONE_VM)<>0) then
 begin
  keg^.uk_flags:=keg^.uk_flags or UMA_ZFLAG_CACHEONLY;
 end;

 if ((arg^.flags and UMA_ZONE_ZINIT)<>0) then
 begin
  keg^.uk_init:=@zero_init;
 end;

 {
  * The +UMA_FRITM_SZ added to uk_size is to account for the
  * linkage that is added to the size in keg_small_init().  If
  * we don't account for this here then we may end up in
  * keg_small_init() with a calculated 'ipers' of 0.
  }
 if (keg^.uk_size+UMA_FRITM_SZ) > (UMA_SLAB_SIZE - sizeof(uma_slab)) then
 begin
  keg_large_init(keg);
 end else
 begin
  keg_small_init(keg);
 end;

 if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)<>0) then
 begin
  keg^.uk_slabzone:=slabzone;
 end;

 {
  * If we haven't booted yet we need allocations to go through the
  * startup cache until the vm is ready.
  }
 if (keg^.uk_ppera=1) then
 begin
  if (booted < UMA_STARTUP2_CONST) then
  begin
   keg^.uk_allocf:=uma_alloc(@startup_alloc);
  end;
 end else
 if (booted < UMA_STARTUP2_CONST) and ((keg^.uk_flags and UMA_ZFLAG_INTERNAL)<>0) then
 begin
  keg^.uk_allocf:=uma_alloc(@startup_alloc);
 end;

 {
  * Initialize keg's lock (shared among zones).
  }
 if ((arg^.flags and UMA_ZONE_MTXCLASS)<>0) then
  KEG_LOCK_INIT(keg, 1)
 else
  KEG_LOCK_INIT(keg, 0);

 {
  * If we're putting the slab header in the actual page we need to
  * figure out where in each page it goes.  This calculates a right
  * justified offset into the memory on an ALIGN_PTR boundary.
  }
 if ((keg^.uk_flags and UMA_ZONE_OFFPAGE)=0) then
 begin

  { Size of the slab struct and free list }
  totsize:=sizeof(uma_slab) + keg^.uk_ipers * UMA_FRITM_SZ;

  if ((totsize and UMA_ALIGN_PTR)<>0) then
  begin
   totsize:=(totsize and (not UMA_ALIGN_PTR)) + (UMA_ALIGN_PTR + 1);
  end;

  keg^.uk_pgoff:=(UMA_SLAB_SIZE * keg^.uk_ppera) - totsize;

  Assert(keg^.uk_pgoff<>0);

  totsize:=keg^.uk_pgoff + sizeof(uma_slab) + keg^.uk_ipers * UMA_FRITM_SZ;

  {
   * The only way the following is possible is if with our
   * UMA_ALIGN_PTR adjustments we are now bigger than
   * UMA_SLAB_SIZE.  I haven't checked whether this is
   * mathematically possible for all cases, so we make
   * sure here anyway.
   }
  if (totsize > UMA_SLAB_SIZE * keg^.uk_ppera) then
  begin
   Writeln(stderr,'zone ',zone^.uz_name,' ipers ',keg^.uk_ipers,' rsize ',keg^.uk_rsize,' size ',keg^.uk_size);
   Assert(False, 'UMA slab won`t fit.');
  end;
 end;

 if ((keg^.uk_flags and UMA_ZONE_HASH)<>0) then
 begin
  hash_alloc(@keg^.uk_hash);
 end;

 LIST_INSERT_HEAD(@keg^.uk_zones, zone, @zone^.uz_link);

 mtx_lock(uma_mtx);
 LIST_INSERT_HEAD(@uma_kegs, keg, @keg^.uk_link);
 mtx_unlock(uma_mtx);
 Exit(0);
end;

{
 * Zone header ctor.  This initializes all fields, locks, etc.
 *
 * Arguments/Returns follow uma_ctor specifications
 * udata  Actually uma_zctor_args
 }
function zone_ctor(mem:Pointer;size:Integer;udata:Pointer;flags:Integer):Integer;
var
 arg:p_uma_zctor_args;
 zone:uma_zone_t;
 z:uma_zone_t;
 keg:uma_keg_t;
 karg:uma_kctor_args;
 error:Integer;
 tmp:uma_klink_t;
begin
 arg:=udata;
 zone:=mem;

 FillChar(zone^, size, 0);
 zone^.uz_name  :=arg^.name;
 zone^.uz_ctor  :=arg^.ctor;
 zone^.uz_dtor  :=arg^.dtor;
 zone^.uz_slab  :=@zone_fetch_slab;
 zone^.uz_init  :=nil;
 zone^.uz_fini  :=nil;
 zone^.uz_allocs:=0;
 zone^.uz_frees :=0;
 zone^.uz_fails :=0;
 zone^.uz_sleeps:=0;
 zone^.uz_fills :=0;
 zone^.uz_count :=0;
 zone^.uz_flags :=0;
 keg:=arg^.keg;

 if ((arg^.flags and UMA_ZONE_SECONDARY)<>0) then
 begin
  Assert(arg^.keg<>nil, 'Secondary zone on zero`d keg');
  zone^.uz_init :=arg^.uminit;
  zone^.uz_fini :=arg^.fini;
  zone^.uz_lock :=@keg^.uk_lock;
  zone^.uz_flags:=zone^.uz_flags or UMA_ZONE_SECONDARY;
  mtx_lock(uma_mtx);
  ZONE_LOCK(zone);

  z:=LIST_FIRST(@keg^.uk_zones);
  while (z<>nil) do
  begin
   if (LIST_NEXT(z, @z^.uz_link)=nil) then
   begin
    LIST_INSERT_AFTER(z, zone, @zone^.uz_link);
    break;
   end;
   //
   z:=LIST_NEXT(z,@z^.uz_link);
  end;

  ZONE_UNLOCK(zone);
  mtx_unlock(uma_mtx);
 end else
 if (keg=nil) then
 begin
  keg:=uma_kcreate(zone, arg^.size, arg^.uminit, arg^.fini, arg^.align, arg^.flags);

  if (keg=nil) then
  begin
   Exit(ENOMEM);
  end;
 end else
 begin
  { We should only be here from uma_startup() }
  karg.size  :=arg^.size;
  karg.uminit:=arg^.uminit;
  karg.fini  :=arg^.fini;
  karg.align :=arg^.align;
  karg.flags :=arg^.flags;
  karg.zone  :=zone;
  error:=keg_ctor(arg^.keg, sizeof(uma_keg), @karg, flags);
  if (error<>0) then
  begin
   Exit(error);
  end;
 end;
 {
  * Link in the first keg.
  }
 zone^.uz_klink.kl_keg:=keg;

 tmp:=@zone^.uz_klink;
 LIST_INSERT_HEAD(@zone^.uz_kegs, tmp, @tmp^.kl_link);

 zone^.uz_lock :=@keg^.uk_lock;
 zone^.uz_size :=keg^.uk_size;
 zone^.uz_flags:=zone^.uz_flags or (keg^.uk_flags and (UMA_ZONE_INHERIT or UMA_ZFLAG_INHERIT));

 {
  * Some internal zones don't have room allocated for the per cpu
  * caches.  If we're internal, bail out here.
  }
 if ((keg^.uk_flags and UMA_ZFLAG_INTERNAL)<>0) then
 begin
  Assert((zone^.uz_flags and UMA_ZONE_SECONDARY)=0, 'Secondary zone requested UMA_ZFLAG_INTERNAL');
  Exit(0);
 end;

 if ((keg^.uk_flags and UMA_ZONE_MAXBUCKET)<>0) then
 begin
  zone^.uz_count:=BUCKET_MAX;
 end else
 if (keg^.uk_ipers <= BUCKET_MAX) then
 begin
  zone^.uz_count:=keg^.uk_ipers;
 end else
 begin
  zone^.uz_count:=BUCKET_MAX;
 end;
 Exit(0);
end;

{
 * Keg header dtor.  This frees all data, destroys locks, frees the hash
 * table and removes the keg from the global list.
 *
 * Arguments/Returns follow uma_dtor specifications
 * udata  unused
 }
procedure keg_dtor(arg:Pointer;size:Integer;udata:Pointer);
var
 keg:uma_keg_t;
begin
 keg:=arg;
 KEG_LOCK(keg);
 if (keg^.uk_free<>0) then
 begin
  Writeln('Freed UMA keg (',keg^.uk_name,') was not empty (',keg^.uk_free,' items). ',
          ' Lost ',keg^.uk_pages,' pages of memory.');
 end;
 KEG_UNLOCK(keg);

 hash_free(@keg^.uk_hash);

 KEG_LOCK_FINI(keg);
end;

{
 * Zone header dtor.
 *
 * Arguments/Returns follow uma_dtor specifications
 * udata  unused
 }
procedure zone_dtor(arg:Pointer;size:Integer;udata:Pointer);
var
 klink:uma_klink_t;
 zone:uma_zone_t;
 keg:uma_keg_t;
begin
 zone:=arg;
 keg:=zone_first_keg(zone);

 if ((zone^.uz_flags and UMA_ZFLAG_INTERNAL)=0) then
 begin
  cache_drain(zone);
 end;

 mtx_lock(uma_mtx);
 LIST_REMOVE(zone, @zone^.uz_link);
 mtx_unlock(uma_mtx);
 {
  * XXX there are some races here where
  * the zone can be drained but zone lock
  * released and then refilled before we
  * remove it... we dont care for now
  }
 zone_drain_wait(zone, M_WAITOK);
 {
  * Unlink all of our kegs.
  }
 klink:=LIST_FIRST(@zone^.uz_kegs);
 while (klink<>nil) do
 begin
  klink^.kl_keg:=nil;
  LIST_REMOVE(klink, @klink^.kl_link);
  if (klink=@zone^.uz_klink) then
  begin
   continue;
  end;
  FreeMem(klink);
  //
  klink:=LIST_FIRST(@zone^.uz_kegs);
 end;
 {
  * We only destroy kegs from non secondary zones.
  }
 if ((zone^.uz_flags and UMA_ZONE_SECONDARY)=0) then
 begin
  mtx_lock(uma_mtx);
  LIST_REMOVE(keg, @keg^.uk_link);
  mtx_unlock(uma_mtx);
  zone_free_item(kegs, keg, nil, SKIP_NONE, ZFREE_STATFREE);
 end;
end;

{
 * Traverses every zone in the system and calls a callback
 *
 * Arguments:
 * zfunc  A pointer to a function which accepts a zone
 *  as an argument.
 *
 * Returns:
 * Nothing
 }
procedure zone_foreach(zfunc:t_zfunc);
var
 keg:uma_keg_t;
 zone:uma_zone_t;
begin
 mtx_lock(uma_mtx);

 keg:=LIST_FIRST(@uma_kegs);
 while (keg<>nil) do
 begin
  zone:=LIST_FIRST(@keg^.uk_zones);
  while (zone<>nil) do
  begin
   zfunc(zone);
   //
   zone:=LIST_NEXT(zone,@zone^.uz_link);
  end;
  //
  keg:=LIST_NEXT(keg,@keg^.uk_link);
 end;

 mtx_unlock(uma_mtx);
end;

{ Public functions }
{ See uma.h }
procedure uma_startup(bootmem:Pointer;boot_pages:Integer);
var
 args:uma_zctor_args;
 slab:uma_slab_t;
 slabsize:DWORD;
 objsize,totsize,wsize:DWORD;
 i:Integer;
begin
 mtx_init(uma_mtx, 'UMA lock');

 {
  * Figure out the maximum number of items-per-slab we'll have if
  * we're using the OFFPAGE slab header to track free items, given
  * all possible object sizes and the maximum desired wastage
  * (UMA_MAX_WASTE).
  *
  * We iterate until we find an object size for
  * which the calculated wastage in keg_small_init() will be
  * enough to warrant OFFPAGE.  Since wastedspace versus objsize
  * is an overall increasing see-saw function, we find the smallest
  * objsize such that the wastage is always acceptable for objects
  * with that objsize or smaller.  Since a smaller objsize always
  * generates a larger possible uma_max_ipers, we use this computed
  * objsize to calculate the largest ipers possible.  Since the
  * ipers calculated for OFFPAGE slab headers is always larger than
  * the ipers initially calculated in keg_small_init(), we use
  * the former's equation (UMA_SLAB_SIZE div keg^.uk_rsize) to
  * obtain the maximum ipers possible for offpage slab headers.
  *
  * It should be noted that ipers versus objsize is an inversly
  * proportional function which drops off rather quickly so as
  * long as our UMA_MAX_WASTE is such that the objsize we calculate
  * falls into the portion of the inverse relation AFTER the steep
  * falloff, then uma_max_ipers shouldn't be too high (~10 on i386).
  *
  * Note that we have 8-bits (1 byte) to use as a freelist index
  * inside the actual slab header itself and this is enough to
  * accomodate us.  In the worst case, a UMA_SMALLEST_UNIT sized
  * object with offpage slab header would have ipers =
  * UMA_SLAB_SIZE div UMA_SMALLEST_UNIT (currently:=256), which is
  * 1 greater than what our byte-integer freelist index can
  * accomodate, but we know that this situation never occurs as
  * for UMA_SMALLEST_UNIT-sized objects, we will never calculate
  * that we need to go to offpage slab headers.  Or, if we do,
  * then we trap that condition below and panic in the INVARIANTS case.
  }
 wsize:=UMA_SLAB_SIZE - sizeof(uma_slab) - UMA_MAX_WASTE;
 totsize:=wsize;
 objsize:=UMA_SMALLEST_UNIT;
 while (totsize >= wsize) do
 begin
  totsize:=(UMA_SLAB_SIZE - sizeof(uma_slab)) div (objsize + UMA_FRITM_SZ);
  totsize:=totsize * (UMA_FRITM_SZ + objsize);
  Inc(objsize);
 end;

 if (objsize > UMA_SMALLEST_UNIT) then
 begin
  Dec(objsize);
 end;

 uma_max_ipers:=MAX(UMA_SLAB_SIZE div objsize, 64);

 wsize:=UMA_SLAB_SIZE - sizeof(uma_slab) - UMA_MAX_WASTE;
 totsize:=wsize;
 objsize:=UMA_SMALLEST_UNIT;

 while (totsize >= wsize) do
 begin
  totsize:=(UMA_SLAB_SIZE - sizeof(uma_slab)) div (objsize + UMA_FRITM_SZ);
  totsize:=totsize * (UMA_FRITM_SZ + objsize);
  Inc(objsize);
 end;

 if (objsize > UMA_SMALLEST_UNIT) then
 begin
  Dec(objsize);
 end;

 uma_max_ipers_ref:=MAX(UMA_SLAB_SIZE div objsize, 64);

 Assert((uma_max_ipers_ref <= High(Byte)) and (uma_max_ipers <= High(Byte)), 'uma_startup: calculated uma_max_ipers values too large!');

 { 'manually' create the initial zone }
 args.name  :='UMA Kegs';
 args.size  :=sizeof(uma_keg);
 args.ctor  :=@keg_ctor;
 args.dtor  :=@keg_dtor;
 args.uminit:=@zero_init;
 args.fini  :=nil;
 args.keg   :=@masterkeg;
 args.align :=32 - 1;
 args.flags :=UMA_ZFLAG_INTERNAL;
 { The initial zone has no Per cpu queues so it's smaller }
 zone_ctor(kegs, SIZEOF_UMA_ZONE, @args, M_WAITOK);

 if (boot_pages<>0) then
 For i:=0 to boot_pages-1 do
 begin
  slab:=(bootmem + (i * UMA_SLAB_SIZE));
  slab^.us_data :=pbyte(slab);
  slab^.us_flags:=UMA_SLAB_BOOT;
  LIST_INSERT_HEAD(@uma_boot_pages, slab, @slab^.us_link);
 end;

 mtx_init(uma_boot_pages_mtx, 'UMA boot pages');

 args.name  :='UMA Zones';
 args.size  :=SIZEOF_UMA_ZONE + (sizeof(uma_cache) * (mp_maxid + 1));
 args.ctor  :=@zone_ctor;
 args.dtor  :=@zone_dtor;
 args.uminit:=@zero_init;
 args.fini  :=nil;
 args.keg   :=nil;
 args.align :=32 - 1;
 args.flags :=UMA_ZFLAG_INTERNAL;
 { The initial zone has no Per cpu queues so it's smaller }
 zone_ctor(zones, SIZEOF_UMA_ZONE, @args, M_WAITOK);

 {
  * This is the max number of free list items we'll have with
  * offpage slabs.
  }
 slabsize:=uma_max_ipers * UMA_FRITM_SZ;
 slabsize:=slabsize + sizeof(uma_slab);

 { Now make a zone for slab headers }
 slabzone:=uma_zcreate('UMA Slabs',
    slabsize,
    nil, nil, nil, nil,
    UMA_ALIGN_PTR, UMA_ZFLAG_INTERNAL);

 hashzone:=uma_zcreate('UMA Hash',
     sizeof(Pointer) * UMA_HASH_SIZE_INIT,
     nil, nil, nil, nil,
     UMA_ALIGN_PTR, UMA_ZFLAG_INTERNAL);

 bucket_init();

 booted:=UMA_STARTUP1_CONST;
end;

{ see uma.h }
procedure uma_startup2();
begin
 booted:=UMA_STARTUP2_CONST;
 bucket_enable();
end;

{
 * Initialize our callout handle
 *
 }

var
 stub:t_daemon_node;

procedure uma_startup3();
begin
 sys_daemon_add_cbs(@stub,@uma_timeout);
end;

procedure uma_startup4();
begin
 curcpu_startup;
 uma_startup (kmem_alloc(UMA_BOOT_PAGES_CONST*UMA_SLAB_SIZE, VM_RW),UMA_BOOT_PAGES_CONST);
 uma_startup2();
 uma_startup3();
end;

function uma_kcreate(zone:uma_zone_t;size:QWORD;uminit:uma_init;fini:uma_fini;align:Integer;flags:DWORD):uma_keg_t;
var
 args:uma_kctor_args;
begin
 args.size  :=size;
 args.uminit:=uminit;
 args.fini  :=fini;

 if (align=UMA_ALIGN_CACHE) then
 begin
  args.align:=uma_align_cache_var;
 end else
 begin
  args.align:=align;
 end;

 args.flags :=flags;
 args.zone  :=zone;
 Exit(zone_alloc_item(kegs, @args, M_WAITOK));
end;

{ See uma.h }
procedure uma_set_align(align:Integer); public;
begin
 if (align<>UMA_ALIGN_CACHE) then
 begin
  uma_align_cache_var:=align;
 end;
end;

{ See uma.h }
function uma_zcreate(name  :pchar;
                     size  :QWORD;
                     ctor  :uma_ctor;
                     dtor  :uma_dtor;
                     uminit:uma_init;
                     fini  :uma_fini;
                     align :Integer;
                     flags :DWORD
                    ):uma_zone_t; public;
var
 args:uma_zctor_args;
begin

 if ((flags and UMA_ZONE_OFFPAGE)<>0) then
 if not ((flags and UMA_ZONE_HASH)<>0)  then
 begin
  Assert(false,'UMA_ZONE_OFFPAGE: requires UMA_ZONE_HASH');
 end;

 { This stuff is essential for the zone ctor }
 args.name  :=name;
 args.size  :=size;
 args.ctor  :=ctor;
 args.dtor  :=dtor;
 args.uminit:=uminit;
 args.fini  :=fini;
 args.align :=align;
 args.flags :=flags;
 args.keg   :=nil;

 Exit(zone_alloc_item(zones, @args, M_WAITOK));
end;

{ See uma.h }

function uma_zsecond_create(name  :pchar;
                            ctor  :uma_ctor;
                            dtor  :uma_dtor;
                            zinit :uma_init;
                            zfini :uma_fini;
                            master:uma_zone_t
                           ):uma_zone_t; public;
var
 args:uma_zctor_args;
 keg:uma_keg_t;
begin
 keg:=zone_first_keg(master);
 args.name  :=name;
 args.size  :=keg^.uk_size;
 args.ctor  :=ctor;
 args.dtor  :=dtor;
 args.uminit:=zinit;
 args.fini  :=zfini;
 args.align :=keg^.uk_align;
 args.flags :=keg^.uk_flags or UMA_ZONE_SECONDARY;
 args.keg   :=keg;

 { XXX Attaches only one keg of potentially many. }
 Exit(zone_alloc_item(zones, @args, M_WAITOK));
end;

procedure zone_lock_pair(a,b:uma_zone_t);
begin
 if (a < b) then
 begin
  ZONE_LOCK(a);
  mtx_lock(b^.uz_lock^);
 end else
 begin
  ZONE_LOCK(b);
  mtx_lock(a^.uz_lock^);
 end;
end;

procedure zone_unlock_pair(a,b:uma_zone_t);
begin
 ZONE_UNLOCK(a);
 ZONE_UNLOCK(b);
end;

{ See uma.h }
procedure uma_zdestroy(zone:uma_zone_t); public;
begin
 zone_free_item(zones, zone, nil, SKIP_NONE, ZFREE_STATFREE);
end;

{ See uma.h }
function uma_zalloc_arg(zone:uma_zone_t;udata:Pointer;flags:Integer):Pointer; public;
label
 zalloc_restart,
 zalloc_start,
 _zone_alloc_item;
var
 item:Pointer;
 cache:uma_cache_t;
 bucket:uma_bucket_t;
 cpu:Integer;
begin

 if (curkthread=nil) then
 begin
  goto _zone_alloc_item;
 end;

 {
  * If possible, allocate from the per-CPU cache.  There are two
  * requirements for safe access to the per-CPU cache: (1) the thread
  * accessing the cache must not be preempted or yield during access,
  * and (2) the thread must not migrate CPUs without switching which
  * cache it accesses.  We rely on a critical section to prevent
  * preemption and migration.  We release the critical section in
  * order to acquire the zone mutex if we are unable to allocate from
  * the current cache; when we re-acquire the critical section, we
  * must detect and handle migration if it has occurred.
  }
zalloc_restart:
 critical_enter();
 cpu:=curcpu;
 cache:=@zone^.uz_cpu[cpu];

zalloc_start:
 bucket:=cache^.uc_allocbucket;

 if (bucket<>nil) then
 begin
  if (bucket^.ub_cnt > 0) then
  begin

   with bucket^ do
   begin
    Dec(ub_cnt);
    item:=ub_bucket[ub_cnt];
    ub_bucket[ub_cnt]:=nil;
   end;

   Assert(item<>nil, 'uma_zalloc: Bucket pointer mangled.');
   Inc(cache^.uc_allocs);
   critical_exit();

   if (zone^.uz_ctor<>nil) then
   begin
    if (zone^.uz_ctor(item, zone^.uz_size, udata, flags)<>0) then
    begin
     zone_free_item(zone, item, udata, SKIP_DTOR, ZFREE_STATFAIL or ZFREE_STATFREE);
     Exit(nil);
    end;
   end;

   if ((flags and M_ZERO)<>0) then
   begin
    FillChar(item^, zone^.uz_size, 0);
   end;

   Exit(item);
  end else
  if (cache^.uc_freebucket<>nil) then
  begin
   {
    * We have run out of items in our allocbucket.
    * See if we can switch with our free bucket.
    }
   with cache^ do
   if (uc_freebucket^.ub_cnt > 0) then
   begin

    bucket:=uc_freebucket;
    uc_freebucket :=uc_allocbucket;
    uc_allocbucket:=bucket;

    goto zalloc_start;
   end;
  end;
 end;
 {
  * Attempt to retrieve the item from the per-CPU cache has failed, so
  * we must go back to the zone.  This requires the zone lock, so we
  * must drop the critical section, then re-acquire it when we go back
  * to the cache.  Since the critical section is released, we may be
  * preempted or migrate.  As such, make sure not to maintain any
  * thread-local state specific to the cache from prior to releasing
  * the critical section.
  }
 critical_exit();
 ZONE_LOCK(zone);
 critical_enter();
 cpu:=curcpu;
 cache:=@zone^.uz_cpu[cpu];
 bucket:=cache^.uc_allocbucket;
 if (bucket<>nil) then
 begin
  if (bucket^.ub_cnt > 0) then
  begin
   ZONE_UNLOCK(zone);
   goto zalloc_start;
  end;
  bucket:=cache^.uc_freebucket;
  if (bucket<>nil) and (bucket^.ub_cnt > 0) then
  begin
   ZONE_UNLOCK(zone);
   goto zalloc_start;
  end;
 end;

 { Since we have locked the zone we may as well send back our stats }
 with zone^ do
 with cache^ do
 begin
  uz_allocs:=uz_allocs + uc_allocs;
  uz_frees :=uz_frees  + uc_frees;
  uc_allocs:=0;
  uc_frees :=0;
 end;

 { Our old one is now a free bucket }
 with cache^ do
 if (uc_allocbucket<>nil) then
 begin
  Assert(uc_allocbucket^.ub_cnt=0, 'uma_zalloc_arg: Freeing a non free bucket.');
  LIST_INSERT_HEAD(@zone^.uz_free_bucket, uc_allocbucket, @uc_allocbucket^.ub_link);
  uc_allocbucket:=nil;
 end;

 { Check the free list for a new alloc bucket }
 bucket:=LIST_FIRST(@zone^.uz_full_bucket);
 if (bucket<>nil) then
 begin
  Assert(bucket^.ub_cnt<>0, 'uma_zalloc_arg: Returning an empty bucket.');

  LIST_REMOVE(bucket, @bucket^.ub_link);
  cache^.uc_allocbucket:=bucket;
  ZONE_UNLOCK(zone);
  goto zalloc_start;
 end;
 { We are no longer associated with this CPU. }
 critical_exit();

 { Bump up our uz_count so we get here less }
 if (zone^.uz_count < BUCKET_MAX) then
 begin
  Inc(zone^.uz_count);
 end;

 {
  * Now lets just fill a bucket and put it on the free list.  If that
  * works we'll restart the allocation from the begining.
  }
 if (zone_alloc_bucket(zone, flags)<>0) then
 begin
  ZONE_UNLOCK(zone);
  goto zalloc_restart;
 end;
 ZONE_UNLOCK(zone);

 {
  * We may not be able to get a bucket so return an actual item.
  }

 _zone_alloc_item:
  item:=zone_alloc_item(zone, udata, flags);
  Exit(item);
end;

function keg_fetch_slab(keg:uma_keg_t;zone:uma_zone_t;flags:Integer):uma_slab_t;
var
 slab:uma_slab_t;
begin
 mtx_assert(keg^.uk_lock);
 slab:=nil;

 while (True) do
 begin
  {
   * Find a slab with some space.  Prefer slabs that are partially
   * used over those that are totally full.  This helps to reduce
   * fragmentation.
   }
  if (keg^.uk_free<>0) then
  begin
   if (not LIST_EMPTY(@keg^.uk_part_slab)) then
   begin
    slab:=LIST_FIRST(@keg^.uk_part_slab);
   end else
   begin
    slab:=LIST_FIRST(@keg^.uk_free_slab);
    LIST_REMOVE(slab, @slab^.us_link);
    LIST_INSERT_HEAD(@keg^.uk_part_slab, slab, @slab^.us_link);
   end;
   Assert(slab^.us_keg=keg);
   Exit(slab);
  end;

  {
   * M_NOVM means don't ask at all!
   }
  if (flags and M_NOVM)<>0 then
   break;

  if (keg^.uk_maxpages<>0) and (keg^.uk_pages >= keg^.uk_maxpages) then
  begin
   keg^.uk_flags:=keg^.uk_flags or UMA_ZFLAG_FULL;
   {
    * If this is not a multi-zone, set the FULL bit.
    * Otherwise slab_multi() takes care of it.
    }
   if ((zone^.uz_flags and UMA_ZFLAG_MULTI)=0) then
    zone^.uz_flags:=zone^.uz_flags or UMA_ZFLAG_FULL;

   if ((flags and M_NOWAIT)<>0) then
    break;

   Inc(zone^.uz_sleeps);
   msleep(keg, @keg^.uk_lock, PVM, 'keglimit', 0);
   continue;
  end;

  Inc(keg^.uk_recurse);
  slab:=keg_alloc_slab(keg, zone, flags);
  Dec(keg^.uk_recurse);
  {
   * If we got a slab here it's safe to mark it partially used
   * and return.  We assume that the caller is going to remove
   * at least one item.
   }
  if (slab<>nil) then
  begin
   Assert(slab^.us_keg=keg);
   LIST_INSERT_HEAD(@keg^.uk_part_slab, slab, @slab^.us_link);
   Exit(slab);
  end;
  {
   * We might not have been able to get a slab but another cpu
   * could have while we were unlocked.  Check again before we
   * fail.
   }
  flags:=flags or M_NOVM;
 end;
 Exit(slab);
end;

procedure zone_relock(zone:uma_zone_t;keg:uma_keg_t);
begin
 if (zone^.uz_lock<>@keg^.uk_lock) then
 begin
  KEG_UNLOCK(keg);
  ZONE_LOCK(zone);
 end;
end;

procedure keg_relock(keg:uma_keg_t;zone:uma_zone_t);
begin
 if (zone^.uz_lock<>@keg^.uk_lock) then
 begin
  ZONE_UNLOCK(zone);
  KEG_LOCK(keg);
 end;
end;

function zone_fetch_slab(zone:uma_zone_t;keg:uma_keg_t;flags:Integer):uma_slab_t;
var
 slab:uma_slab_t;
begin
 if (keg=nil) then
  keg:=zone_first_keg(zone);
 {
  * This is to prevent us from recursively trying to allocate
  * buckets.  The problem is that if an allocation forces us to
  * grab a new bucket we will call page_alloc, which will go off
  * and cause the vm to allocate vm_map_entries.  If we need new
  * buckets there too we will recurse in kmem_alloc and bad
  * things happen.  So instead we Exita nil bucket, and make
  * the code that allocates buckets smart enough to deal with it
  }
 if ((keg^.uk_flags and UMA_ZFLAG_BUCKET)<>0) and (keg^.uk_recurse<>0) then
  Exit(nil);

 while (True) do
 begin
  slab:=keg_fetch_slab(keg, zone, flags);
  if (slab<>nil) then
   Exit(slab);
  if ((flags and (M_NOWAIT or M_NOVM))<>0) then
   break;
 end;

 Exit(nil);
end;

{
 * uma_zone_fetch_slab_multi:  Fetches a slab from one available keg.  Returns
 * with the keg locked.  Caller must call zone_relock() afterwards if the
 * zone lock is required.  On nil the zone lock is held.
 *
 * The last pointer is used to seed the search.  It is not required.
 }
function zone_fetch_slab_multi(zone:uma_zone_t;last:uma_keg_t;rflags:Integer):uma_slab_t;
var
 klink:uma_klink_t;
 slab:uma_slab_t;
 keg:uma_keg_t;
 flags:Integer;
 empty:Integer;
 full :Integer;
begin
 {
  * Don't wait on the first pass.  This will skip limit tests
  * as well.  We don't want to block if we can find a provider
  * without blocking.
  }
 flags:=(rflags and (not M_WAITOK)) or M_NOWAIT;
 {
  * Use the last slab allocated as a hint for where to start
  * the search.
  }
 if (last<>nil) then
 begin
  slab:=keg_fetch_slab(last, zone, flags);
  if (slab<>nil) then
   Exit(slab);
  zone_relock(zone, last);
  last:=nil;
 end;
 {
  * Loop until we have a slab incase of transient failures
  * while M_WAITOK is specified.  I'm not sure this is 100%
  * required but we've done it for so long now.
  }
 while (True) do
 begin
  empty:=0;
  full :=0;
  {
   * Search the available kegs for slabs.  Be careful to hold the
   * correct lock while calling into the keg layer.
   }
  klink:=LIST_FIRST(@zone^.uz_kegs);
  while (klink<>nil) do
  begin
   keg:=klink^.kl_keg;
   keg_relock(keg, zone);
   if ((keg^.uk_flags and UMA_ZFLAG_FULL)=0) then
   begin
    slab:=keg_fetch_slab(keg, zone, flags);
    if (slab<>nil) then
     Exit(slab);
   end;
   if ((keg^.uk_flags and UMA_ZFLAG_FULL)<>0) then
    Inc(full)
   else
    Inc(empty);
   zone_relock(zone, keg);
   //
   klink:=LIST_NEXT(klink,@klink^.kl_link);
  end;
  if ((rflags and (M_NOWAIT or M_NOVM))<>0) then
   break;

  flags:=rflags;
  {
   * All kegs are full.  XXX We can't atomically check all kegs
   * and sleep so just sleep for a short period and retry.
   }
  if (full<>0) and (empty=0) then
  begin
   zone^.uz_flags:=zone^.uz_flags or UMA_ZFLAG_FULL;
   Inc(zone^.uz_sleeps);
   msleep(zone, zone^.uz_lock, PVM, 'zonelimit', hz div 100);
   zone^.uz_flags:=zone^.uz_flags and (not UMA_ZFLAG_FULL);
   continue;
  end;
 end;
 Exit(nil);
end;

function slab_alloc_item(zone:uma_zone_t;slab:uma_slab_t):Pointer;
var
 keg:uma_keg_t;
 item:Pointer;
 freei:Integer;
begin
 keg:=slab^.us_keg;
 mtx_assert(keg^.uk_lock);

 freei:=slab^.us_firstfree;
 slab^.us_firstfree:=slab^.us_freelist[freei].us_item;

 item:=get_item_addr(keg,slab,freei);

 Dec(slab^.us_head.us_freecount);
 Dec(keg^.uk_free);

 { Move this slab to the full list }
 if (slab^.us_freecount=0) then
 begin
  LIST_REMOVE(slab, @slab^.us_link);
  LIST_INSERT_HEAD(@keg^.uk_full_slab, slab, @slab^.us_link);
 end;

 Exit(item);
end;

function zone_alloc_bucket(zone:uma_zone_t;flags:Integer):Integer;
label
 done;
var
 bucket:uma_bucket_t;
 slab:uma_slab_t;
 keg:uma_keg_t;
 saved:Word;
 max,origflags:Integer;
 bflags:Integer;
 i,j:Integer;
begin
 max:=flags;
 origflags:=flags;

 {
  * Try this zone's free list first so we don't allocate extra buckets.
  }
 bucket:=LIST_FIRST(@zone^.uz_free_bucket);
 if (bucket<>nil) then
 begin
  Assert(bucket^.ub_cnt=0, 'zone_alloc_bucket: Bucket on free list is not empty.');
  LIST_REMOVE(bucket, @bucket^.ub_link);
 end else
 begin
  bflags:=(flags and (not M_ZERO));
  if ((zone^.uz_flags and UMA_ZFLAG_CACHEONLY)<>0) then
  begin
   bflags:=bflags or M_NOVM;
  end;

  ZONE_UNLOCK(zone);
  bucket:=bucket_alloc(zone^.uz_count, bflags);
  ZONE_LOCK(zone);
 end;

 if (bucket=nil) then
 begin
  Exit(0);
 end;

 {
  * This code is here to limit the number of simultaneous bucket fills
  * for any given zone to the number of per cpu caches in this zone. This
  * is done so that we don't allocate more memory than we really need.
  }
 if (zone^.uz_fills >= mp_ncpus) then
  goto done;

 Inc(zone^.uz_fills);

 max:=MIN(bucket^.ub_entries, zone^.uz_count);
 { Try to keep the buckets totally full }
 saved:=bucket^.ub_cnt;
 slab:=nil;
 keg:=nil;
 while (bucket^.ub_cnt < max) do
 begin
  slab:=zone^.uz_slab(zone, keg, flags);
  if (slab=nil) then Break;

  keg:=slab^.us_keg;
  while (slab^.us_freecount<>0) and (bucket^.ub_cnt < max) do
  begin
   with bucket^ do
   begin
    ub_bucket[ub_cnt]:=slab_alloc_item(zone, slab);
    Inc(ub_cnt);
   end;
  end;

  { Don't block on the next fill }
  flags:=flags or M_NOWAIT;
 end;

 if (slab<>nil) then
 begin
  zone_relock(zone, keg);
 end;

 {
  * We unlock here because we need to call the zone's init.
  * It should be safe to unlock because the slab dealt with
  * above is already on the appropriate list within the keg
  * and the bucket we filled is not yet on any list, so we
  * own it.
  }
 if (zone^.uz_init<>nil) then
 begin
  ZONE_UNLOCK(zone);

  i:=saved;
  while (i < bucket^.ub_cnt) do
  begin
   if (zone^.uz_init(bucket^.ub_bucket[i], zone^.uz_size, origflags)<>0) then
   begin
    break;
   end;
   //
   Inc(i);
  end;
  {
   * If we couldn't initialize the whole bucket, put the
   * rest back onto the freelist.
   }
  if (i<>bucket^.ub_cnt) then
  begin
   j:=i;
   while (j < bucket^.ub_cnt) do
   begin
    with bucket^ do
    begin
     zone_free_item(zone, ub_bucket[j], nil, SKIP_FINI, 0);
     ub_bucket[j]:=nil;
    end;
    //
    Inc(j);
   end;
   bucket^.ub_cnt:=i;
  end;
  ZONE_LOCK(zone);
 end;

 Dec(zone^.uz_fills);
 if (bucket^.ub_cnt<>0) then
 begin
  LIST_INSERT_HEAD(@zone^.uz_full_bucket, bucket, @bucket^.ub_link);
  Exit(1);
 end;

done:
 bucket_free(bucket);

 Exit(0);
end;
{
 * Allocates an item for an internal zone
 *
 * Arguments
 * zone   The zone to alloc for.
 * udata  The data to be passed to the constructor.
 * flags  M_WAITOK, M_NOWAIT, M_ZERO.
 *
 * Returns
 * nil if there is no memory and M_NOWAIT is set
 * An item if successful
 }

function zone_alloc_item(zone:uma_zone_t;udata:Pointer;flags:Integer):Pointer;
var
 slab:uma_slab_t;
 item:Pointer;
begin
 item:=nil;

 ZONE_LOCK(zone);

 slab:=zone^.uz_slab(zone, nil, flags);
 if (slab=nil) then
 begin
  Inc(zone^.uz_fails);
  ZONE_UNLOCK(zone);
  Exit(nil);
 end;

 item:=slab_alloc_item(zone, slab);

 zone_relock(zone, slab^.us_keg);
 Inc(zone^.uz_allocs);
 ZONE_UNLOCK(zone);

 {
  * We have to call both the zone's init (not the keg's init)
  * and the zone's ctor.  This is because the item is going from
  * a keg slab directly to the user, and the user is expecting it
  * to be both zone-init'd as well as zone-ctor'd.
  }
 if (zone^.uz_init<>nil) then
 begin
  if (zone^.uz_init(item, zone^.uz_size, flags)<>0) then
  begin
   zone_free_item(zone, item, udata, SKIP_FINI, ZFREE_STATFAIL or ZFREE_STATFREE);
   Exit(nil);
  end;
 end;

 if (zone^.uz_ctor<>nil) then
 begin
  if (zone^.uz_ctor(item, zone^.uz_size, udata, flags)<>0) then
  begin
   zone_free_item(zone, item, udata, SKIP_DTOR, ZFREE_STATFAIL or ZFREE_STATFREE);
   Exit(nil);
  end;
 end;

 if ((flags and M_ZERO)<>0) then
 begin
  FillChar(item^, zone^.uz_size, 0);
 end;

 Exit(item);
end;

{ See uma.h }
procedure uma_zfree_arg(zone:uma_zone_t;item,udata:Pointer); public;
label
 zfree_internal,
 zfree_restart,
 zfree_start;
var
 cache:uma_cache_t;
 bucket:uma_bucket_t;
 bflags:Integer;
 cpu:Integer;
begin
 { uma_zfree(..., nil) does nothing, to match free(9). }
 if (item=nil) then
  Exit;

 if (zone^.uz_dtor<>nil) then
  zone^.uz_dtor(item, zone^.uz_size, udata);

 {
  * The race here is acceptable.  If we miss it we'll just have to wait
  * a little longer for the limits to be reset.
  }
 if ((zone^.uz_flags and UMA_ZFLAG_FULL)<>0) then
  goto zfree_internal;

 {
  * If possible, free to the per-CPU cache.  There are two
  * requirements for safe access to the per-CPU cache: (1) the thread
  * accessing the cache must not be preempted or yield during access,
  * and (2) the thread must not migrate CPUs without switching which
  * cache it accesses.  We rely on a critical section to prevent
  * preemption and migration.  We release the critical section in
  * order to acquire the zone mutex if we are unable to free to the
  * current cache; when we re-acquire the critical section, we must
  * detect and handle migration if it has occurred.
  }
zfree_restart:
 critical_enter();
 cpu:=curcpu;
 cache:=@zone^.uz_cpu[cpu];

zfree_start:
 bucket:=cache^.uc_freebucket;

 if (bucket<>nil) then
 begin
  {
   * Do we have room in our bucket? It is OK for this uz count
   * check to be slightly out of sync.
   }

  if (bucket^.ub_cnt < bucket^.ub_entries) then
  begin
   with bucket^ do
   begin
    Assert(ub_bucket[ub_cnt]=nil,'uma_zfree: Freeing to non free bucket index.');
    ub_bucket[ub_cnt]:=item;
    Inc(ub_cnt);
   end;
   Inc(cache^.uc_frees);
   critical_exit();
   Exit;
  end else
  if (cache^.uc_allocbucket<>nil) then
  begin

   {
    * We have run out of space in our freebucket.
    * See if we can switch with our alloc bucket.
    }
   with cache^ do
   if (uc_allocbucket^.ub_cnt < uc_freebucket^.ub_cnt) then
   begin
    bucket:=uc_freebucket;
    uc_freebucket :=uc_allocbucket;
    uc_allocbucket:=bucket;
    goto zfree_start;
   end;
  end;
 end;
 {
  * We can get here for two reasons:
  *
  * 1) The buckets are nil
  * 2) The alloc and free buckets are both somewhat full.
  *
  * We must go back the zone, which requires acquiring the zone lock,
  * which in turn means we must release and re-acquire the critical
  * section.  Since the critical section is released, we may be
  * preempted or migrate.  As such, make sure not to maintain any
  * thread-local state specific to the cache from prior to releasing
  * the critical section.
  }
 critical_exit();
 ZONE_LOCK(zone);
 critical_enter();
 cpu:=curcpu;
 cache:=@zone^.uz_cpu[cpu];

 with cache^ do
 if (uc_freebucket<>nil) then
 begin
  if (uc_freebucket^.ub_cnt < uc_freebucket^.ub_entries) then
  begin
   ZONE_UNLOCK(zone);
   goto zfree_start;
  end;
  if (uc_allocbucket<>nil) and
     (uc_allocbucket^.ub_cnt < uc_freebucket^.ub_cnt) then
  begin
   ZONE_UNLOCK(zone);
   goto zfree_start;
  end;
 end;

 { Since we have locked the zone we may as well send back our stats }
 with zone^ do
 with cache^ do
 begin
  uz_allocs:=uz_allocs + uc_allocs;
  uz_frees :=uz_frees  + uc_frees;
  uc_allocs:=0;
  uc_frees :=0;
 end;

 with cache^ do
 begin
  bucket:=uc_freebucket;
  uc_freebucket:=nil;
 end;

 { Can we throw this on the zone full list? }
 if (bucket<>nil) then
 begin
  { ub_cnt is pointing to the last free item }
  Assert(bucket^.ub_cnt<>0,'uma_zfree: Attempting to insert an empty bucket onto the full list.');
  LIST_INSERT_HEAD(@zone^.uz_full_bucket, bucket, @bucket^.ub_link);
 end;

 bucket:=LIST_FIRST(@zone^.uz_free_bucket);
 if (bucket<>nil) then
 begin
  LIST_REMOVE(bucket, @bucket^.ub_link);
  ZONE_UNLOCK(zone);
  cache^.uc_freebucket:=bucket;
  goto zfree_start;
 end;
 { We are no longer associated with this CPU. }
 critical_exit();

 { And the zone.. }
 ZONE_UNLOCK(zone);

 bflags:=M_NOWAIT;

 if ((zone^.uz_flags and UMA_ZFLAG_CACHEONLY)<>0) then
  bflags:=bflags or M_NOVM;

 bucket:=bucket_alloc(zone^.uz_count, bflags);
 if (bucket<>nil) then
 begin
  ZONE_LOCK(zone);
  LIST_INSERT_HEAD(@zone^.uz_free_bucket, bucket, @bucket^.ub_link);
  ZONE_UNLOCK(zone);
  goto zfree_restart;
 end;

 {
  * If nothing else caught this, we'll just do an internal free.
  }
zfree_internal:
 zone_free_item(zone, item, udata, SKIP_DTOR, ZFREE_STATFREE);

end;

{
 * Frees an item to an INTERNAL zone or allocates a free bucket
 *
 * Arguments:
 * zone   The zone to free to
 * item   The item we're freeing
 * udata  User supplied data for the dtor
 * skip   Skip dtors and finis
 }
procedure zone_free_item(zone:uma_zone_t;item,udata:Pointer;skip:zfreeskip;flags:Integer);
var
 slab:uma_slab_t;
 keg:uma_keg_t;
 mem:PByte;
 freei:Word;
 clearfull:Integer;
 freecount:Integer;
begin
 if (skip < SKIP_DTOR) and (zone^.uz_dtor<>nil) then
 begin
  zone^.uz_dtor(item, zone^.uz_size, udata);
 end;

 if (skip < SKIP_FINI) and (zone^.uz_fini<>nil) then
 begin
  zone^.uz_fini(item, zone^.uz_size);
 end;

 ZONE_LOCK(zone);

 if ((flags and ZFREE_STATFAIL)<>0) then
 begin
  Inc(zone^.uz_fails);
 end;
 if ((flags and ZFREE_STATFREE)<>0) then
 begin
  Inc(zone^.uz_frees);
 end;

 mem:=Pointer(QWORD(item) and (not UMA_SLAB_MASK));
 keg:=zone_first_keg(zone); { Must only be one. }
 if ((zone^.uz_flags and UMA_ZONE_HASH)<>0) then
 begin
  slab:=hash_sfind(@keg^.uk_hash, mem);
 end else
 begin
  mem:=mem + keg^.uk_pgoff;
  slab:=uma_slab_t(mem);
 end;

 Assert(keg=slab^.us_keg);

 freecount:=slab^.us_freecount;

 { Do we need to remove from any lists? }
 if (freecount+1=keg^.uk_ipers) then
 begin
  LIST_REMOVE(slab, @slab^.us_link);
  LIST_INSERT_HEAD(@keg^.uk_free_slab, slab, @slab^.us_link);
 end else
 if (freecount=0) then
 begin
  LIST_REMOVE(slab, @slab^.us_link);
  LIST_INSERT_HEAD(@keg^.uk_part_slab, slab, @slab^.us_link);
 end;

 { Slab management stuff }

 freei:=(QWORD(item) - QWORD(slab^.us_data)) div keg^.uk_rsize;

 slab^.us_freelist[freei].us_item:=slab^.us_firstfree;

 slab^.us_firstfree:=freei;
 Inc(slab^.us_head.us_freecount);


 { Zone statistics }
 Inc(keg^.uk_free);

 clearfull:=0;
 if ((keg^.uk_flags and UMA_ZFLAG_FULL)<>0) then
 begin
  if (keg^.uk_pages < keg^.uk_maxpages) then
  begin
   keg^.uk_flags:=keg^.uk_flags and (not UMA_ZFLAG_FULL);
   clearfull:=1;
  end;

  {
   * We can handle one more allocation. Since we're clearing ZFLAG_FULL,
   * wake up all procs blocked on pages. This should be uncommon, so
   * keeping this simple for now (rather than adding count of blocked
   * threads etc).
   }
  wakeup(keg);
 end;
 if (clearfull<>0) then
 begin
  zone_relock(zone, keg);
  zone^.uz_flags:=zone^.uz_flags and (not UMA_ZFLAG_FULL);
  wakeup(zone);
  ZONE_UNLOCK(zone);
 end else
  KEG_UNLOCK(keg);
end;

{ See uma.h }
function uma_zone_set_max(zone:uma_zone_t;nitems:Integer):Integer; public;
var
 keg:uma_keg_t;
begin
 ZONE_LOCK(zone);
 keg:=zone_first_keg(zone);
 keg^.uk_maxpages:=(nitems div keg^.uk_ipers) * keg^.uk_ppera;
 if (keg^.uk_maxpages * keg^.uk_ipers < nitems) then
 begin
  keg^.uk_maxpages:=keg^.uk_maxpages + keg^.uk_ppera;
 end;
 nitems:=keg^.uk_maxpages * keg^.uk_ipers;
 ZONE_UNLOCK(zone);

 Exit(nitems);
end;

{ See uma.h }
function uma_zone_get_max(zone:uma_zone_t):Integer; public;
var
 nitems:Integer;
 keg:uma_keg_t;
begin
 ZONE_LOCK(zone);
 keg:=zone_first_keg(zone);
 nitems:=keg^.uk_maxpages * keg^.uk_ipers;
 ZONE_UNLOCK(zone);

 Exit(nitems);
end;

{ See uma.h }
function uma_zone_get_cur(zone:uma_zone_t):Integer; public;
var
 nitems:Int64;
 i:Integer;
begin
 ZONE_LOCK(zone);
 nitems:=zone^.uz_allocs - zone^.uz_frees;
 i:=0;
 while CPU_FOREACH(i) do
 begin
  {
   * See the comment in sysctl_vm_zone_stats() regarding the
   * safety of accessing the per-cpu caches. With the zone lock
   * held, it is safe, but can potentially result in stale data.
   }
  nitems:=nitems + zone^.uz_cpu[i].uc_allocs - zone^.uz_cpu[i].uc_frees;
 end;
 ZONE_UNLOCK(zone);

 if (nitems < 0) then
  Exit(0)
 else
  Exit(nitems);
end;

{ See uma.h }
procedure uma_zone_set_init(zone:uma_zone_t;uminit:uma_init); public;
var
 keg:uma_keg_t;
begin
 ZONE_LOCK(zone);
 keg:=zone_first_keg(zone);
 Assert(keg^.uk_pages=0, 'uma_zone_set_init on non-empty keg');
 keg^.uk_init:=uminit;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
procedure uma_zone_set_fini(zone:uma_zone_t;fini:uma_fini); public;
var
 keg:uma_keg_t;
begin
 ZONE_LOCK(zone);
 keg:=zone_first_keg(zone);
 Assert(keg^.uk_pages=0, 'uma_zone_set_fini on non-empty keg');
 keg^.uk_fini:=fini;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
procedure uma_zone_set_zinit(zone:uma_zone_t;zinit:uma_init); public;
begin
 ZONE_LOCK(zone);
 Assert(zone_first_keg(zone)^.uk_pages=0, 'uma_zone_set_zinit on non-empty keg');
 zone^.uz_init:=zinit;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
procedure uma_zone_set_zfini(zone:uma_zone_t;zfini:uma_fini); public;
begin
 ZONE_LOCK(zone);
 Assert(zone_first_keg(zone)^.uk_pages=0, 'uma_zone_set_zfini on non-empty keg');
 zone^.uz_fini:=zfini;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
{ XXX uk_freef is not actually used with the zone locked }
procedure uma_zone_set_freef(zone:uma_zone_t;freef:uma_free); public;
begin
 ZONE_LOCK(zone);
 zone_first_keg(zone)^.uk_freef:=freef;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
{ XXX uk_allocf is not actually used with the zone locked }
procedure uma_zone_set_allocf(zone:uma_zone_t;allocf:uma_alloc);
var
 keg:uma_keg_t;
begin
 ZONE_LOCK(zone);
 keg:=zone_first_keg(zone);
 keg^.uk_flags :=keg^.uk_flags or UMA_ZFLAG_PRIVALLOC;
 keg^.uk_allocf:=allocf;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
procedure uma_prealloc(zone:uma_zone_t;items:Integer); public;
var
 slabs:Integer;
 slab:uma_slab_t;
 keg:uma_keg_t;
begin
 keg:=zone_first_keg(zone);
 ZONE_LOCK(zone);
 slabs:=items div keg^.uk_ipers;
 if ((slabs * keg^.uk_ipers) < items) then
 begin
  Inc(slabs);
 end;
 while (slabs > 0) do
 begin
  slab:=keg_alloc_slab(keg, zone, M_WAITOK);
  if (slab=nil) then
  begin
   break;
  end;
  Assert(slab^.us_keg=keg);
  LIST_INSERT_HEAD(@keg^.uk_free_slab, slab, @slab^.us_link);
  Dec(slabs);
 end;
 ZONE_UNLOCK(zone);
end;

{ See uma.h }
procedure uma_reclaim(); public;
begin
 bucket_enable();
 zone_foreach(@zone_drain);
 {
  * Some slabs may have been freed but this zone will be visited early
  * we visit again so that we can free pages that are empty once other
  * zones are drained.  We have to do the same for buckets.
  }
 zone_drain(slabzone);
 bucket_zone_drain();
end;

{ See uma.h }
function uma_zone_exhausted(zone:uma_zone_t):Integer; public;
var
 full:Integer;
begin
 ZONE_LOCK(zone);
 full:=(zone^.uz_flags and UMA_ZFLAG_FULL);
 ZONE_UNLOCK(zone);
 Exit(full);
end;

function uma_zone_exhausted_nolock(zone:uma_zone_t):Integer; public;
begin
 Exit(zone^.uz_flags and UMA_ZFLAG_FULL);
end;

procedure uma_print_stats();
begin
 zone_foreach(@uma_print_zone);
end;

procedure slab_print(slab:uma_slab_t);
begin
 Writeln('slab: keg ',HexStr(slab^.us_keg),', data ',HexStr(slab^.us_data),', freecount ',slab^.us_freecount);
end;

procedure cache_print(cache:uma_cache_t);

 function uc_allocbucket_ub_cnt:WORD; inline;
 begin
  if (cache^.uc_allocbucket<>nil) then
   Result:=cache^.uc_allocbucket^.ub_cnt
  else
   Result:=0;
 end;

 function uc_freebucket_ub_cnt:WORD; inline;
 begin
  if (cache^.uc_freebucket<>nil) then
   Result:=cache^.uc_freebucket^.ub_cnt
  else
   Result:=0;
 end;

begin
 Writeln('alloc: ',HexStr(cache^.uc_allocbucket),'(',uc_allocbucket_ub_cnt,'), free: ',HexStr(cache^.uc_freebucket),'(',uc_freebucket_ub_cnt,')');
end;

procedure LIST_FOREACH_slab(h:P_LIST_HEAD);
var
 slab:uma_slab_t;
begin
 slab:=LIST_FIRST(h);
 while (slab<>nil) do
 begin
  slab_print(slab);
  //
  slab:=LIST_NEXT(slab,@slab^.us_link);
 end;
end;

procedure uma_print_keg(keg:uma_keg_t);
begin
 Writeln('keg: ',keg^.uk_name,'(',HexStr(keg),') size ',keg^.uk_size,'(',keg^.uk_rsize,') flags ',HexStr(keg^.uk_flags,4),
         ' ipers ',keg^.uk_ipers,
         ' ppera ',keg^.uk_ppera,
         ' out ',(keg^.uk_ipers * keg^.uk_pages) - keg^.uk_free,
         ' free ', keg^.uk_free,
         ' limit ',(keg^.uk_maxpages div keg^.uk_ppera) * keg^.uk_ipers
        );
 Writeln('Part slabs:');
 LIST_FOREACH_slab(@keg^.uk_part_slab);
 Writeln('Free slabs:');
 LIST_FOREACH_slab(@keg^.uk_free_slab);
 Writeln('Full slabs:');
 LIST_FOREACH_slab(@keg^.uk_full_slab);
end;

procedure uma_print_zone(zone:uma_zone_t);
var
 cache:uma_cache_t;
 kl:uma_klink_t;
 i:Integer;
begin
 Writeln('zone: ',zone^.uz_name,'(',HexStr(zone),') size ',zone^.uz_size,' flags ',HexStr(zone^.uz_flags,4));

 kl:=LIST_FIRST(@zone^.uz_kegs);
 while (kl<>nil) do
 begin
  uma_print_keg(kl^.kl_keg);
  //
  kl:=LIST_NEXT(kl,@kl^.kl_link);
 end;

 i:=0;
 while CPU_FOREACH(i) do
 begin
  cache:=@zone^.uz_cpu[i];
  Writeln('CPU ',i,' Cache:');
  cache_print(cache);
 end;
end;


end.

