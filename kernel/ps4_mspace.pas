unit ps4_mspace;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_mutex,
 ps4_map_mm;

const
 SCE_LIBC_MSPACE_THREAD_UNSAFE =1;
 SCE_LIBC_MSPACE_ALLOC_BASE    =2;
 SCE_LIBC_MSPACE_DEBUG_SHORTAGE=4;
 SCE_LIBC_MSPACE_IN_ARRAY      =8;

type
 p_malloc_chunk=^malloc_chunk;
 malloc_chunk=packed record
  prev_foot:ptruint;
  head     :ptruint;
  fd:p_malloc_chunk;
  bk:p_malloc_chunk;
 end;

 mchunk   =malloc_chunk;
 mchunkptr=p_malloc_chunk;
 sbinptr  =p_malloc_chunk;
 bindex_t =integer;
 binmap_t =integer;
 flag_t   =DWORD;

 p_malloc_tree_chunk=^malloc_tree_chunk;
 malloc_tree_chunk=packed record
  prev_foot:ptruint;
  head     :ptruint;
  fd:p_malloc_tree_chunk;
  bk:p_malloc_tree_chunk;

  child:array[0..1] of p_malloc_tree_chunk;
  parent:p_malloc_tree_chunk;
  index:bindex_t;
  //
  _align:integer;
 end;

 tchunk   =malloc_tree_chunk;
 tchunkptr=p_malloc_tree_chunk;
 tbinptr  =p_malloc_tree_chunk;
 p_tbinptr=^tbinptr;

 p_malloc_segment=^malloc_segment;
 malloc_segment=packed record
  base:Pointer;
  size:ptruint;
  next:p_malloc_segment;
  sflags:flag_t;
  //
  _align:integer;
 end;

 msegment   =malloc_segment;
 msegmentptr=p_malloc_segment;

 pSceLibcMallocManagedSize=^SceLibcMallocManagedSize;
 SceLibcMallocManagedSize=packed record
  size             :word;
  version          :word;
  reserved1        :dword;
  maxSystemSize    :QWORD;
  currentSystemSize:QWORD;
  maxInuseSize     :QWORD;
  currentInuseSize :QWORD;
 end;

 //struct decompiled from libSceLibcInternal
 pSceLibcMspace=^SceLibcMspace;
 SceLibcMspace=packed record
  smallmap         :Integer;
  treemap          :Integer;
  dvsize           :QWORD;
  topsize          :QWORD;
  least_addr       :Pointer;
  dv               :mchunkptr;
  top              :mchunkptr;
  trim_check       :QWORD;
  magic            :QWORD;
  smallbins        :array[0..65] of mchunkptr;
  treebins         :array[0..31] of tbinptr;
  footprint        :QWORD; //currentSystemSize
  max_footprint    :QWORD; //maxSystemSize
  currentInuseSize :QWORD; //def:1440
  maxInuseSize     :QWORD; //def:1440
  mflags           :DWORD;
  _align1:Integer;
  mutex            :Pointer;
  seg              :malloc_segment;
  page_size        :QWORD; //1 << 14 = 16384
  granularity      :QWORD; //1 << 16 = 65536
  name             :array[0..31] of AnsiChar;
  HighAddressAlloc :QWORD;
  heap_bits        :DWORD;
  f_988:Integer;
  f_992:QWORD;
  cbs              :Pointer; //callbak actions
  msp_array_flags  :DWORD;   //save to array used
  msp_array_id     :DWORD;   //array pos + 1
  SystemSize       :QWORD;
  debug_flags      :DWORD;
  _align4:Integer;
  ptr_self         :Pointer; //self link in global var
  f_1040:QWORD;
  MutexPoolForMono :pSceLibcMspace; //place to save mono pool
  _last            :array[0..239] of Byte;
 end;

 malloc_state=SceLibcMspace;
 mstate      =^malloc_state;

implementation

uses
 sys_kernel,
 sys_signal;

//libc use nedmalloc modification

Const
 MALLOC_ALIGNMENT      =16;

 DEFAULT_GRANULARITY   =(64*1024);
 DEFAULT_PAGE_SIZE     =(16*1024);
 DEFAULT_TRIM_THRESHOLD=(2*1024*1024);
 DEFAULT_MMAP_THRESHOLD=(256*1024);

 DEFAULT_MAGIC         =$58585858;

 MAX_SIZE_T            =not ptruint(0);

 SIZE_T_SIZE           = sizeof(ptruint);
 SIZE_T_BITSIZE        = (sizeof(ptruint) shl 3);

 SIZE_T_ZERO           =ptruint(0);
 SIZE_T_ONE            =ptruint(1);
 SIZE_T_TWO            =ptruint(2);
 SIZE_T_FOUR           =ptruint(4);

 TWO_SIZE_T_SIZES      =(SIZE_T_SIZE shl 1);
 THREE_SIZE_T_SIZES    =(TWO_SIZE_T_SIZES+SIZE_T_SIZE);
 FOUR_SIZE_T_SIZES     =(SIZE_T_SIZE shl 2);
 SEVEN_SIZE_T_SIZES    =(FOUR_SIZE_T_SIZES+TWO_SIZE_T_SIZES+SIZE_T_SIZE);
 HALF_MAX_SIZE_T       =(MAX_SIZE_T div 2);

 CHUNK_ALIGN_MASK      =(MALLOC_ALIGNMENT-SIZE_T_ONE);

 CHUNK_OVERHEAD        =TWO_SIZE_T_SIZES;

 USE_MMAP_BIT          =SIZE_T_ONE;
 USE_LOCK_BIT          =2;
 USE_4_BIT             =4;
 EXTERN_BIT            =8;

 MMAP_CHUNK_OVERHEAD   =THREE_SIZE_T_SIZES;
 MMAP_FOOT_PAD         =FOUR_SIZE_T_SIZES;

 MCHUNK_SIZE           =sizeof(mchunk);

 MIN_CHUNK_SIZE        =(MCHUNK_SIZE+CHUNK_ALIGN_MASK) and (not CHUNK_ALIGN_MASK);

 MAX_REQUEST           =(-MIN_CHUNK_SIZE) shl 2;
 MIN_REQUEST           =MIN_CHUNK_SIZE-CHUNK_OVERHEAD-SIZE_T_ONE;

 PINUSE_BIT            =SIZE_T_ONE;
 CINUSE_BIT            =SIZE_T_TWO;
 FLAG4_BIT             =SIZE_T_FOUR;
 INUSE_BITS            =PINUSE_BIT or CINUSE_BIT;
 FLAG_BITS             =PINUSE_BIT or CINUSE_BIT or FLAG4_BIT;
 FENCEPOST_HEAD        =INUSE_BITS or SIZE_T_SIZE;

 NSMALLBINS            =32;
 NTREEBINS             =32;
 SMALLBIN_SHIFT        =3;
 SMALLBIN_WIDTH        =SIZE_T_ONE shl SMALLBIN_SHIFT;
 TREEBIN_SHIFT         =8;
 MIN_LARGE_SIZE        =SIZE_T_ONE shl TREEBIN_SHIFT;
 MAX_SMALL_SIZE        =MIN_LARGE_SIZE-SIZE_T_ONE;
 MAX_SMALL_REQUEST     =MAX_SMALL_SIZE-CHUNK_ALIGN_MASK-CHUNK_OVERHEAD;

 MIN_SMALL_INDEX       =MIN_CHUNK_SIZE shr SMALLBIN_SHIFT;

 DEFAULT_MFLAGS        =USE_LOCK_BIT or USE_MMAP_BIT;

function is_aligned(A:Pointer):boolean; inline;
begin
 Result:=(ptruint(A) and CHUNK_ALIGN_MASK)=0;
end;

function align_offset(A:Pointer):ptruint; inline;
begin
 if ((ptruint(A) and CHUNK_ALIGN_MASK)=0) then
 begin
  Result:=MALLOC_ALIGNMENT-(ptruint(A) and CHUNK_ALIGN_MASK);
 end else
 begin
  Result:=CHUNK_ALIGN_MASK;
 end;
end;

function chunk2mem(p:Pointer):Pointer; inline;
begin
 Result:=p+TWO_SIZE_T_SIZES;
end;

function mem2chunk(p:Pointer):mchunkptr; inline;
begin
 Result:=p-TWO_SIZE_T_SIZES;
end;

function align_as_chunk(A:Pointer):mchunkptr; inline;
begin
 Result:=mchunkptr(A+align_offset(chunk2mem(A)));
end;

function pad_request(req:ptruint):ptruint; inline;
begin
 Result:=(req+CHUNK_OVERHEAD+CHUNK_ALIGN_MASK) and (not CHUNK_ALIGN_MASK)
end;

function request2size(req:ptruint):ptruint; inline;
begin
 if (req<MIN_REQUEST) then
 begin
  Result:=MIN_CHUNK_SIZE;
 end else
 begin
  Result:=pad_request(req);
 end;
end;

function cinuse(p:mchunkptr):boolean; inline;
begin
 Result:=(p^.head and CINUSE_BIT)<>0;
end;

function pinuse(p:mchunkptr):boolean; inline;
begin
 Result:=(p^.head and PINUSE_BIT)<>0;
end;

function flag4inuse(p:mchunkptr):boolean; inline;
begin
 Result:=(p^.head and FLAG4_BIT)<>0;
end;

function is_inuse(p:mchunkptr):boolean; inline;
begin
 Result:=(p^.head and INUSE_BITS)<>PINUSE_BIT;
end;

function is_mmapped(p:mchunkptr):boolean; inline;
begin
 Result:=(p^.head and INUSE_BITS)=0;
end;

function calloc_must_clear(p:mchunkptr):boolean; inline;
begin
 Result:=True; //not is_mmapped(p);
end;

function chunksize(p:Pointer):ptruint; inline;
begin
 Result:=(mchunkptr(p)^.head and (not FLAG_BITS))
end;

procedure clear_pinuse(p:mchunkptr); inline;
begin
 p^.head:=(p^.head and (not PINUSE_BIT))
end;

procedure set_flag4(p:mchunkptr); inline;
begin
 p^.head:=(p^.head or FLAG4_BIT)
end;

procedure clear_flag4(p:mchunkptr); inline;
begin
 p^.head:=(p^.head and (not FLAG4_BIT))
end;

function chunk_plus_offset(p:Pointer;s:ptruint):mchunkptr; inline;
begin
 Result:=mchunkptr(p+s);
end;

function chunk_minus_offset(p:mchunkptr;s:ptruint):mchunkptr; inline;
begin
 Result:=mchunkptr(Pointer(p)-s);
end;

function next_chunk(p:mchunkptr):mchunkptr; inline;
begin
 Result:=mchunkptr(Pointer(p)+chunksize(p));
end;

function prev_chunk(p:mchunkptr):mchunkptr; inline;
begin
 Result:=mchunkptr(Pointer(p)-p^.prev_foot);
end;

function next_pinuse(p:mchunkptr):boolean; inline;
begin
 Result:=(next_chunk(p)^.head and PINUSE_BIT)<>0;
end;

function get_foot(p:mchunkptr;s:ptruint):ptruint; inline;
begin
 Result:=mchunkptr(Pointer(p)+s)^.prev_foot;
end;

procedure set_foot(p:mchunkptr;s:ptruint); inline;
begin
 mchunkptr(Pointer(p)+s)^.prev_foot:=s;
end;

procedure set_size_and_pinuse_of_free_chunk(p:mchunkptr;s:ptruint); inline;
begin
 p^.head:=s or PINUSE_BIT;
 set_foot(p,s);
end;

procedure set_free_with_pinuse(p:mchunkptr;s:ptruint;n:mchunkptr); inline;
begin
 clear_pinuse(n);
 set_size_and_pinuse_of_free_chunk(p,s);
end;

function overhead_for(p:mchunkptr):ptruint; inline;
begin
 if is_mmapped(p) then
 begin
  Result:=MMAP_CHUNK_OVERHEAD;
 end else
 begin
  Result:=CHUNK_OVERHEAD;
 end;
end;

function leftmost_child(t:tchunkptr):tchunkptr;
begin
 if (t^.child[0]<>nil) then
 begin
  Result:=t^.child[0];
 end else
 begin
  Result:=t^.child[1];
 end;
end;

function is_mmapped_segment(S:msegmentptr):boolean; inline;
begin
 Result:=(S^.sflags and USE_MMAP_BIT)<>0;
end;

function is_extern_segment(S:msegmentptr):boolean; inline;
begin
 Result:=(S^.sflags and EXTERN_BIT)<>0;
end;

function is_initialized(M:mstate):boolean; inline;
begin
 Result:=(M^.top<>nil)
end;

function use_lock(M:mstate):boolean; inline;
begin
 Result:=(M^.mflags and USE_LOCK_BIT)<>0
end;

procedure enable_lock(M:mstate); inline;
begin
 M^.mflags:=(M^.mflags or USE_LOCK_BIT);
end;

procedure disable_lock(M:mstate);inline;
begin
 M^.mflags:=(M^.mflags and (not USE_LOCK_BIT));
end;

function use_mmap(M:mstate):boolean; inline;
begin
 Result:=(M^.mflags and USE_MMAP_BIT)<>0
end;

procedure enable_mmap(M:mstate); inline;
begin
 M^.mflags:=(M^.mflags or USE_MMAP_BIT);
end;

procedure disable_mmap(M:mstate); inline;
begin
 M^.mflags:=(M^.mflags and (not USE_MMAP_BIT));
end;

procedure set_lock(M:mstate;L:Boolean);inline;
begin
 Case L of
  True :M^.mflags:=(M^.mflags or USE_LOCK_BIT);
  False:M^.mflags:=(M^.mflags and (not USE_LOCK_BIT));
 end;
end;

function page_align(S:ptruint):ptruint; inline;
begin
 Result:=(S+(DEFAULT_PAGE_SIZE-SIZE_T_ONE)) and (not (DEFAULT_PAGE_SIZE-SIZE_T_ONE));
end;

function granularity_align(S:ptruint):ptruint; inline;
begin
 Result:=(S+(DEFAULT_GRANULARITY-SIZE_T_ONE)) and (not (DEFAULT_GRANULARITY-SIZE_T_ONE));
end;

const
 mmapped_granularity=DEFAULT_GRANULARITY;

function mmap_align_size(S:ptruint):ptruint; inline;
begin
 Result:=(S+(mmapped_granularity-SIZE_T_ONE)) and (not (mmapped_granularity-SIZE_T_ONE));
end;

const
 align_offset_two =8;
 pad_m_segment    =48; //pad_request(sizeof(malloc_segment))

 TOP_FOOT_SIZE    =align_offset_two+pad_m_segment+MIN_CHUNK_SIZE;
 SYS_ALLOC_PADDING=TOP_FOOT_SIZE+MALLOC_ALIGNMENT;

function is_page_aligned(S:ptruint):Boolean; inline;
begin
 Result:=(S and (DEFAULT_PAGE_SIZE-SIZE_T_ONE))=0;
end;

function is_granularity_aligned(S:ptruint):Boolean; inline;
begin
 Result:=(S and (DEFAULT_GRANULARITY-SIZE_T_ONE))=0;
end;

function segment_holds(S:msegmentptr;A:Pointer):Boolean; inline;
begin
 Result:=(ptruint(A)>=ptruint(S^.base)) and (ptruint(A)<(ptruint(S^.base)+S^.size));
end;

function segment_holding(m:mstate;addr:Pointer):msegmentptr;
var
 sp:msegmentptr;
begin
 sp:=@m^.seg;
 repeat
  if (addr >= sp^.base) and (addr < (sp^.base + sp^.size)) then Exit(sp);
  sp:=sp^.next;
  if (sp=nil) then Exit(nil);
 until false;
end;

function has_segment_link(m:mstate;ss:msegmentptr):boolean;
var
 sp:msegmentptr;
begin
 sp:=@m^.seg;
 repeat
  if (Pointer(sp) >= ss^.base) and (Pointer(sp) < (ss^.base + ss^.size)) then Exit(True);
  sp:=sp^.next;
  if (sp=nil) then Exit(False);
 until false;
end;

function should_trim(m:mstate;s:ptruint):boolean; inline;
begin
 Result:=(s>m^.trim_check);
end;

function INITIAL_LOCK(m:mstate;name:Pchar):Integer; inline;
begin
 Result:=0;
 if use_lock(m) then
 begin
  Result:=ps4_scePthreadMutexInit(@m^.mutex,nil,name);
 end;
end;

function DESTROY_LOCK(m:mstate):Integer; inline;
begin
 Result:=0;
 if use_lock(m) then
 begin
  Result:=ps4_scePthreadMutexDestroy(@m^.mutex);
 end;
end;

function ACQUIRE_LOCK(m:mstate):Integer; inline;
begin
 Result:=ps4_scePthreadMutexLock(@m^.mutex);
end;

function RELEASE_LOCK(m:mstate):Integer; inline;
begin
 Result:=0;
 if use_lock(m) then
 begin
  Result:=ps4_scePthreadMutexUnlock(@m^.mutex);
 end;
end;

function PREACTION(m:mstate):Integer; inline;
begin
 if use_lock(m) then
 begin
  Result:=ACQUIRE_LOCK(m);
 end else
 begin
  Result:=0;
 end;
end;

function POSTACTION(m:mstate):Integer; inline;
begin
 if use_lock(m) then
 begin
  Result:=RELEASE_LOCK(m);
 end else
 begin
  Result:=0;
 end;
end;

procedure CORRUPTION_ERROR_ACTION(m:mstate); inline;
begin
 Assert(false,'CORRUPTION_ERROR mstate='+HexStr(m));
end;

procedure USAGE_ERROR_ACTION(m:mstate;p:Pointer); inline;
begin
 Assert(false,'USAGE_ERROR mstate='+HexStr(m)+' mem='+HexStr(p));
end;

procedure MALLOC_FAILURE_ACTION; inline;
begin
 Assert(false,'MALLOC_FAILURE');
end;

function is_small(s:ptruint):boolean; inline;
begin
 Result:=((s shl SMALLBIN_SHIFT) < NSMALLBINS);
end;

function small_index(s:ptruint):bindex_t; inline;
begin
 Result:=s shr SMALLBIN_SHIFT;
end;

function small_index2size(i:bindex_t):ptruint; inline;
begin
 Result:=i shl SMALLBIN_SHIFT;
end;

function smallbin_at(m:mstate;i:integer):sbinptr; inline;
begin
 Result:=sbinptr(@m^.smallbins[i shl 1]);
end;

function treebin_at(m:mstate;i:integer):p_tbinptr; inline;
begin
 Result:=@m^.treebins[i];
end;

procedure compute_tree_index(var S:ptruint;var i:bindex_t);
var
 X,K:ptruint;
begin
 X:=S shr TREEBIN_SHIFT;
 if (X=0) then
 begin
  I:=0;
 end else
 if (X > $FFFF) then
 begin
  I:=NTREEBINS-1;
 end else
 begin
  K:=BsfQWord(X);
  I:=((K shl 1) + ((S shr (K + (TREEBIN_SHIFT-1)) and 1)));
 end;
end;

function bit_for_tree_index(i:integer):ptruint; inline;
begin
 if (i = (NTREEBINS-1)) then
 begin
  Result:=SIZE_T_BITSIZE-1;
 end else
 begin
  Result:=(i shr 1) + TREEBIN_SHIFT - 2;
 end;
end;

function leftshift_for_tree_index(i:integer):ptruint; inline;
begin
 if (i = (NTREEBINS-1)) then
 begin
  Result:=0;
 end else
 begin
  Result:=(SIZE_T_BITSIZE-SIZE_T_ONE) - ((i shr 1) + TREEBIN_SHIFT - 2);
 end;
end;

function minsize_for_tree_index(i:integer):ptruint; inline;
begin
 Result:=(SIZE_T_ONE shl ((i shr 1) + TREEBIN_SHIFT)) or
  ((i and SIZE_T_ONE) shl ((i shr 1) + TREEBIN_SHIFT - 1));
end;

function idx2bit(i:integer):binmap_t; inline;
begin
 Result:=1 shl i;
end;

procedure mark_smallmap(m:mstate;i:integer); inline;
begin
 M^.smallmap:=M^.smallmap or idx2bit(i);
end;

procedure clear_smallmap(m:mstate;i:integer); inline;
begin
 M^.smallmap:=M^.smallmap and (not idx2bit(i));
end;

function smallmap_is_marked(m:mstate;i:integer):boolean; inline;
begin
 Result:=(M^.smallmap and idx2bit(i))<>0;
end;

procedure mark_treemap(m:mstate;i:integer); inline;
begin
 M^.treemap:=M^.treemap or idx2bit(i);
end;

procedure clear_treemap(m:mstate;i:integer); inline;
begin
 M^.treemap:=M^.treemap and (not idx2bit(i));
end;

function treemap_is_marked(m:mstate;i:integer):boolean; inline;
begin
 Result:=(M^.treemap and idx2bit(i))<>0;
end;

function least_bit(x:ptruint):ptruint; inline;
begin
 Result:=x and -x;
end;

function left_bits(x:ptruint):ptruint; inline;
begin
 Result:=(x shl 1) or -(x shl 1);
end;

function same_or_left_bits(x:ptruint):ptruint; inline;
begin
 Result:=x or -x;
end;

procedure compute_bit2idx(X:ptruint;var I:bindex_t); inline;
var
 J:ptruint;
begin
 J:=BsfQWord(X);
 I:=bindex_t(J);
end;

function ok_address(m:mstate;a:Pointer):boolean; inline;
begin
 Result:=(a >= m^.least_addr);
end;

function ok_next(p,n:Pointer):boolean; inline;
begin
 Result:=(p < n);
end;

function ok_inuse(p:Pointer):boolean; inline;
begin
 Result:=is_inuse(p);
end;

function ok_pinuse(p:Pointer):boolean; inline;
begin
 Result:=pinuse(p);
end;

function ok_magic(m:mstate):boolean; inline;
begin
 Result:=(m^.magic=DEFAULT_MAGIC);
end;

procedure mark_inuse_foot(m:mstate;p:Pointer;s:ptruint); inline;
begin
 mchunkptr(p + s)^.prev_foot:=ptruint(m) xor DEFAULT_MAGIC;
end;

function get_mstate_for(p:Pointer):mstate; inline;
begin
 Result:=mstate(mchunkptr(p + chunksize(p))^.prev_foot xor DEFAULT_MAGIC);
end;

procedure set_inuse(m:mstate;p:mchunkptr;s:ptruint); inline;
begin
 p^.head:=(p^.head and PINUSE_BIT) or s or CINUSE_BIT;

 with mchunkptr(p + s)^ do
  head:=head or PINUSE_BIT;

 mark_inuse_foot(M,p,s);
end;

procedure set_inuse_and_pinuse(m:mstate;p:Pointer;s:ptruint); inline;
begin
 mchunkptr(p)^.head:=s or PINUSE_BIT or CINUSE_BIT;

 with mchunkptr(p + s)^ do
  head:=head or PINUSE_BIT;

 mark_inuse_foot(M,p,s);
end;

procedure set_size_and_pinuse_of_inuse_chunk(m:mstate;p:Pointer;s:ptruint); inline;
begin
 mchunkptr(p)^.head:=s or PINUSE_BIT or CINUSE_BIT;

 mark_inuse_foot(M,p,s);
end;

{

maxSystemSize    :QWORD;
currentSystemSize:QWORD;
maxInuseSize     :QWORD;
currentInuseSize :QWORD;

static struct mallinfo internal_mallinfo(mstate m) {
  struct mallinfo nm = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  ensure_initialization();
  if (!PREACTION(m)) {
    check_malloc_state(m);
    if (is_initialized(m)) {
      size_t nfree = SIZE_T_ONE; /* top always free */
      size_t mfree = m->topsize + TOP_FOOT_SIZE;
      size_t sum = mfree;
      msegmentptr s = &m->seg;
      while (s <> 0) {
        mchunkptr q = align_as_chunk(s->base);
        while (segment_holds(s, q) &&
               q <> m->top && q->head <> FENCEPOST_HEAD) {
          size_t sz = chunksize(q);
          sum += sz;
          if (!is_inuse(q)) {
            mfree += sz;
            ++nfree;
          }
          q = next_chunk(q);
        }
        s = s->next;
      }

      nm.arena             = sum;
      nm.ordblks           = nfree;
      nm.hblkhd            = m->footprint - sum;
      nm.currentSystemSize = m->footprint
      nm.maxSystemSize     = m->max_footprint;
      nm.uordblks          = m->footprint - mfree;
      nm.fordblks          = mfree;
      nm.keepcost          = m->topsize;
    }

    POSTACTION(m);
  }
  return nm;
}

static void internal_malloc_stats(mstate m) {
  ensure_initialization();
  if (!PREACTION(m)) {
    size_t maxfp = 0;
    size_t fp = 0;
    size_t used = 0;
    check_malloc_state(m);
    if (is_initialized(m)) {
      msegmentptr s = &m->seg;
      maxfp = m->max_footprint;
      fp = m->footprint;
      used = fp - (m->topsize + TOP_FOOT_SIZE);

      while (s <> 0) {
        mchunkptr q = align_as_chunk(s->base);
        while (segment_holds(s, q) &&
               q <> m->top && q->head <> FENCEPOST_HEAD) {
          if (!is_inuse(q))
            used -= chunksize(q);
          q = next_chunk(q);
        }
        s = s->next;
      }
    }

    fprintf(stderr, "max system bytes = %10lu\n", (unsigned long)(maxfp));
    fprintf(stderr, "system bytes     = %10lu\n", (unsigned long)(fp));
    fprintf(stderr, "in use bytes     = %10lu\n", (unsigned long)(used));

    POSTACTION(m);
  }
}

}

procedure insert_small_chunk(m:mstate;p:mchunkptr;s:ptruint);
var
 I:bindex_t;
 B:mchunkptr;
 F:mchunkptr;
begin
 I:=small_index(S);
 B:=smallbin_at(M,I);
 F:=B;
 assert(S >= MIN_CHUNK_SIZE);

 if (not smallmap_is_marked(M, I)) then
 begin
  mark_smallmap(M, I);
 end else
 if ok_address(M, B^.fd) then
 begin
  F:=B^.fd;
 end else
 begin
  CORRUPTION_ERROR_ACTION(M);
 end;

 B^.fd:=P;
 F^.bk:=P;
 P^.fd:=F;
 P^.bk:=B;
end;

procedure unlink_small_chunk(m:mstate;p:mchunkptr;s:ptruint);
var
 F:mchunkptr;
 B:mchunkptr;
 I:bindex_t;
begin
 F:=P^.fd;
 B:=P^.bk;
 I:=small_index(S);
 assert(P<>B);
 assert(P<>F);
 assert(chunksize(P)=small_index2size(I));
 if (F=B) then
 begin
  clear_smallmap(M, I);
 end else
 if (
     ( (F = smallbin_at(M,I)) or ok_address(M, F) ) and
     ( (B = smallbin_at(M,I)) or ok_address(M, B) )
    ) then
 begin
  F^.bk:=B;
  B^.fd:=F;
 end else
 begin
  CORRUPTION_ERROR_ACTION(M);
 end;
end;

procedure unlink_first_small_chunk(m:mstate;B,P:mchunkptr;i:bindex_t);
var
 F:mchunkptr;
begin
 F:=P^.fd;
 assert(P<>B);
 assert(P<>F);
 assert(chunksize(P)=small_index2size(I));
 if (B=F) then
 begin
  clear_smallmap(M, I);
 end else
 if ok_address(M, F) then
 begin
  B^.fd:=F;
  F^.bk:=B;
 end else
 begin
  CORRUPTION_ERROR_ACTION(M);
 end;
end;

procedure replace_dv(m:mstate;p:mchunkptr;s:ptruint);
var
 DVS:ptruint;
 DV:mchunkptr;
begin
 DVS:=M^.dvsize;
 if (DVS<>0) then
 begin
  DV:=M^.dv;
  assert(is_small(DVS));
  insert_small_chunk(M, DV, DVS);
 end;
 M^.dvsize:=S;
 M^.dv    :=P;
end;

procedure insert_large_chunk(m:mstate;X:tbinptr;s:ptruint);
var
 H:p_tbinptr;
 I:bindex_t;
 T:tchunkptr;
 K:ptruint;
 C:^tchunkptr;
 F:tchunkptr;
begin
 I:=0;
 compute_tree_index(S, I);
 H:=treebin_at(M, I);
 X^.index:=I;
 X^.child[0]:=nil;
 X^.child[1]:=nil;
 if (not treemap_is_marked(M, I)) then
 begin
  mark_treemap(M, I);
  H^:=X;
  X^.parent:=tchunkptr(H);
  X^.fd:=X;
  X^.bk:=X;
 end else
 begin
  T:=H^;
  K:=S shl leftshift_for_tree_index(I);
  repeat
   if (chunksize(T)<>S) then
   begin
    C:=@T^.child[(K shl (SIZE_T_BITSIZE-SIZE_T_ONE)) and 1];
    K:=K shl 1;
    if (C^<>nil) then
    begin
     T:=C^;
    end else
    if ok_address(M, C) then
    begin
     C^:=X;
     X^.parent:=T;
     X^.fd:=X;
     X^.bk:=X;
     break;
    end else
    begin
     CORRUPTION_ERROR_ACTION(M);
     break;
    end;
   end else
   begin
    F:=T^.fd;
    if ok_address(M, T) and ok_address(M, F) then
    begin
     T^.fd:=X;
     F^.bk:=X;
     X^.fd:=F;
     X^.bk:=T;
     X^.parent:=nil;
     break;
    end else
    begin
     CORRUPTION_ERROR_ACTION(M);
     break;
    end
   end;
  until false;
 end
end;

procedure unlink_large_chunk(m:mstate;X:tbinptr);
var
 XP:tchunkptr;
 R:tchunkptr;
 F:tchunkptr;
 RP:^tchunkptr;
 CP:^tchunkptr;
 H:p_tbinptr;
 C0,C1:tchunkptr;
label
 _nxt;
begin
 XP:=X^.parent;
 if (X^.bk <> X) then
 begin
  F:=X^.fd;
  R:=X^.bk;
  if ok_address(M, F) then
  begin
   F^.bk:=R;
   R^.fd:=F;
  end else
  begin
   CORRUPTION_ERROR_ACTION(M);
  end;
 end else
 begin

  RP:=@X^.child[1];
  R:=RP^;

  if (R=nil) then
  begin
   RP:=@X^.child[0];
   R:=RP^;
   if (R=nil) then goto _nxt;
  end;

  begin
   while true do
   begin
    CP:=@R^.child[1];

    if (CP^=nil) then
    begin
     CP:=@R^.child[0];
     if (CP^=nil) then break;
    end;

    RP:=CP;
    R:=RP^;
   end;
   if ok_address(M, RP) then
   begin
    RP^:=nil;
   end else
   begin
    CORRUPTION_ERROR_ACTION(M);
   end;
  end;
  _nxt:

 end;

 if (XP<>nil) then
 begin
  H:=treebin_at(M, X^.index);
  if (X=H^) then
  begin
   H^:=R;
   if (R=nil) then
   begin
    clear_treemap(M, X^.index);
   end;
  end else
  if ok_address(M, XP) then
  begin
   if (XP^.child[0]=X) then
   begin
    XP^.child[0]:=R;
   end else
   begin
    XP^.child[1]:=R;
   end;
  end else
  begin
   CORRUPTION_ERROR_ACTION(M);
  end;

  if (R<>nil) then
  begin
   if ok_address(M, R) then
   begin
    R^.parent:=XP;
    C0:=X^.child[0];
    if (C0<>nil) then
    begin
     if ok_address(M, C0) then
     begin
      R^.child[0]:=C0;
      C0^.parent:=R;
     end else
     begin
      CORRUPTION_ERROR_ACTION(M);
     end;
    end;
    C1:=X^.child[1];
    if (C1<>nil) then
    begin
     if ok_address(M, C1) then
     begin
      R^.child[1]:=C1;
      C1^.parent:=R;
     end else
     begin
      CORRUPTION_ERROR_ACTION(M);
     end;
    end
   end else
   begin
    CORRUPTION_ERROR_ACTION(M);
   end;
  end
 end
end;

procedure insert_chunk(m:mstate;p:mchunkptr;s:ptruint);
var
 TP:tchunkptr;
begin
 if is_small(S) then
 begin
  insert_small_chunk(M, P, S);
 end else
 begin
  TP:=tchunkptr(P);
  insert_large_chunk(M, TP, S);
 end;
end;

procedure unlink_chunk(m:mstate;p:mchunkptr;s:ptruint);
var
 TP:tchunkptr;
begin
 if is_small(S) then
 begin
  unlink_small_chunk(M, P, S);
 end else
 begin
  TP:=tchunkptr(P);
  unlink_large_chunk(M, TP);
 end;
end;

function CALL_MMAP(m:mstate;size:ptruint):Pointer;
begin
 Result:=nil;
 ps4_sceKernelMapNamedFlexibleMemory(@Result,size,3,0,@m^.name);
end;

function CALL_MREMAP(m:mstate;ptr:Pointer;oldsize,newsize:ptruint):Pointer;
begin
 Result:=nil;
 ps4_sceKernelMapNamedFlexibleMemory(@Result,newsize,3,0,@m^.name);
 if (Result<>nil) then
 begin
  Move(ptr^,Result^,oldsize);
  ps4_sceKernelMunmap(ptr,oldsize);
 end;
end;

function CALL_MUNMAP(ptr:Pointer;size:ptruint):Integer;
begin
 Result:=ps4_sceKernelMunmap(ptr,size);
end;

function mmap_alloc(m:mstate;nb:ptruint):Pointer;
var
 mmsize:ptruint;
 mm:Pointer;
 offset:ptruint;
 psize:ptruint;
 p:mchunkptr;
begin
 mmsize:=mmap_align_size(nb + SEVEN_SIZE_T_SIZES + CHUNK_ALIGN_MASK);

 if (mmsize > nb) then
 begin
  mm:=CALL_MMAP(m,mmsize);
  if (mm<>nil) then
  begin
   offset:=MALLOC_ALIGNMENT + align_offset(chunk2mem(mm));
   psize :=mmsize - offset - MMAP_FOOT_PAD;
   p:=mchunkptr(mm + offset);
   p^.prev_foot:=offset;
   p^.head     :=psize;
   mark_inuse_foot(m, p, psize);

   chunk_plus_offset(p, psize)^.head:=FENCEPOST_HEAD;
   chunk_plus_offset(p, psize+SIZE_T_SIZE)^.head:=0;

   if (m^.least_addr=nil) or (mm < m^.least_addr) then
   begin
    m^.least_addr:=mm;
   end;
   m^.footprint:=m^.footprint+mmsize;
   if (m^.footprint > m^.max_footprint) then
   begin
    m^.max_footprint:=m^.footprint;
   end;
   assert(is_aligned(chunk2mem(p)));
   //check_mmapped_chunk(m, p); debug
   Result:=chunk2mem(p);
  end;
 end;

 Result:=nil;
end;

function mmap_resize(m:mstate;oldp:mchunkptr;nb:ptruint):Pointer;
var
 oldsize:ptruint;
 offset:ptruint;
 oldmmsize:ptruint;
 newmmsize:ptruint;
 mm:Pointer;
 cp:Pointer;
 newp:mchunkptr;
 psize:ptruint;
begin
 oldsize:=chunksize(oldp);
 if is_small(nb) then Exit(nil);

 if (oldsize >= nb + SIZE_T_SIZE) and
    ((oldsize - nb) <= (DEFAULT_GRANULARITY shl 1)) then
 begin
  Exit(oldp);
 end else
 begin
  offset:=oldp^.prev_foot;
  oldmmsize:=oldsize + offset + MMAP_FOOT_PAD;
  newmmsize:=mmap_align_size(nb + SEVEN_SIZE_T_SIZES + CHUNK_ALIGN_MASK);
  mm:=Pointer(oldp)-offset;
  cp:=CALL_MREMAP(m,mm,oldmmsize,newmmsize);
  if (cp<>nil) then
  begin
   newp :=mchunkptr(cp + offset);
   psize:=newmmsize - offset - MMAP_FOOT_PAD;
   newp^.head:=psize;
   mark_inuse_foot(m, newp, psize);

   chunk_plus_offset(newp, psize)^.head:=FENCEPOST_HEAD;
   chunk_plus_offset(newp, psize+SIZE_T_SIZE)^.head:=0;

   if (cp < m^.least_addr) then
   begin
    m^.least_addr:=cp;
   end;
   m^.footprint:=m^.footprint+(newmmsize-oldmmsize);
   if (m^.footprint > m^.max_footprint) then
   begin
    m^.max_footprint:=m^.footprint;
   end;
   //check_mmapped_chunk(m, newp); debug
   Result:=newp;
  end;
 end;

 Result:=nil;
end;

procedure init_top(m:mstate;p:mchunkptr;psize:ptruint);
var
 offset:ptruint;
begin
 offset:=align_offset(chunk2mem(p));
 p:=mchunkptr(pointer(p) + offset);
 psize:=psize-offset;

 m^.top    :=p;
 m^.topsize:=psize;
 p^.head   :=psize or PINUSE_BIT;

 chunk_plus_offset(p, psize)^.head:=TOP_FOOT_SIZE;

 m^.trim_check:=DEFAULT_TRIM_THRESHOLD;
end;

procedure init_bins(m:mstate);
var
 i:bindex_t;
 bin:sbinptr;
begin
 for i:=0 to NSMALLBINS-1 do
 begin
  bin:=smallbin_at(m,i);
  bin^.fd:=bin;
  bin^.bk:=bin;
 end;
end;

function prepend_alloc(m:mstate;newbase,oldbase:Pointer;nb:ptruint):Pointer;
var
 p:mchunkptr;
 oldfirst:mchunkptr;
 psize:ptruint;
 q:mchunkptr;
 qsize:ptruint;
 tsize:ptruint;
 dsize:ptruint;
 nsize:ptruint;
begin
 p:=align_as_chunk(newbase);
 oldfirst:=align_as_chunk(oldbase);
 psize:= Pointer(oldfirst) - Pointer(p);
 q:=chunk_plus_offset(p, nb);
 qsize:=psize - nb;
 set_size_and_pinuse_of_inuse_chunk(m, p, nb);

 assert(Pointer(oldfirst) > Pointer(q));
 assert(pinuse(oldfirst));
 assert(qsize >= MIN_CHUNK_SIZE);

 if (oldfirst=m^.top) then
 begin
  tsize:=m^.topsize+qsize;
  m^.topsize:=tsize;
  m^.top:=q;
  q^.head:=tsize or PINUSE_BIT;
  //check_top_chunk(m, q); debug
 end else
 if (oldfirst = m^.dv) then
 begin
  dsize:=m^.dvsize+qsize;
  m^.dvsize:=dsize;
  m^.dv:=q;
  set_size_and_pinuse_of_free_chunk(q, dsize);
 end else
 begin
  if (not is_inuse(oldfirst)) then
  begin
   nsize:=chunksize(oldfirst);
   unlink_chunk(m, oldfirst, nsize);
   oldfirst:=chunk_plus_offset(oldfirst, nsize);
   qsize:=qsize+nsize;
  end;
  set_free_with_pinuse(q, qsize, oldfirst);
  insert_chunk(m, q, qsize);
  //check_free_chunk(m, q); debug
 end;

 //check_malloced_chunk(m, chunk2mem(p), nb); debug
 Result:=chunk2mem(p);
end;

procedure add_segment(m:mstate;tbase:Pointer;tsize:ptruint;mmapped:flag_t);
var
 old_top:Pointer;
 oldsp:msegmentptr;
 old_end:Pointer;
 ssize:ptruint;
 rawsp:Pointer;
 offset:ptruint;
 asp:Pointer;
 csp:Pointer;
 sp:mchunkptr;
 ss:msegmentptr;
 tnext:mchunkptr;
 p:mchunkptr;
 nfences:integer;
 nextp:mchunkptr;
 q:mchunkptr;
 psize:ptruint;
 tn:mchunkptr;
begin
 old_top:=m^.top;
 oldsp:=segment_holding(m, old_top);
 old_end:=oldsp^.base + oldsp^.size;
 ssize:=pad_m_segment;
 rawsp:=old_end - (ssize + FOUR_SIZE_T_SIZES + CHUNK_ALIGN_MASK);
 offset:=align_offset(chunk2mem(rawsp));
 asp:=rawsp + offset;

 if (asp < (old_top + MIN_CHUNK_SIZE)) then
 begin
  csp:=old_top;
 end else
 begin
  csp:=asp
 end;

 sp:=mchunkptr(csp);
 ss:=msegmentptr(chunk2mem(sp));
 tnext:=chunk_plus_offset(sp, ssize);
 p:=tnext;
 nfences:=0;

 init_top(m, mchunkptr(tbase), tsize - TOP_FOOT_SIZE);

 assert(is_aligned(ss));
 set_size_and_pinuse_of_inuse_chunk(m, sp, ssize);
 ss^:=m^.seg;
 m^.seg.base  :=tbase;
 m^.seg.size  :=tsize;
 m^.seg.sflags:=mmapped;
 m^.seg.next  :=ss;

 repeat
  nextp:=chunk_plus_offset(p, SIZE_T_SIZE);
  p^.head:=FENCEPOST_HEAD;
  Inc(nfences);
  if (pointer(@nextp^.head) < old_end) then
  begin
   p:=nextp;
  end else
  begin
   break;
  end;
 until false;
 assert(nfences >= 2);

 if (csp <> old_top) then
 begin
  q:=mchunkptr(old_top);
  psize:=csp - old_top;
  tn:=chunk_plus_offset(q, psize);
  set_free_with_pinuse(q, psize, tn);
  insert_chunk(m, q, psize);
 end;

 //check_top_chunk(m, m^.top); debug
end;

function sys_alloc(m:mstate;nb:ptruint):Pointer;
var
 tbase:Pointer;
 tsize:ptruint;
 mmap_flag:flag_t;
 mem:Pointer;
 rsize:ptruint;
 mp:Pointer;
 mn:mchunkptr;
 sp:msegmentptr;
 oldbase:Pointer;
 p:mchunkptr;
 r:mchunkptr;
begin
 tbase:=nil;
 tsize:=0;
 mmap_flag:=0;

 if use_mmap(m) and
    (nb >= DEFAULT_MMAP_THRESHOLD) and
    (m^.topsize <> 0) then
 begin
  mem:=mmap_alloc(m, nb);
  if (mem<>nil) then
  begin
   Exit(mem);
  end;
 end;

 //if (not use_noncontiguous(m)) then skip

 if (tbase=nil) then
 begin
  rsize:=granularity_align(nb + SYS_ALLOC_PADDING);
  if (rsize > nb) then
  begin
   mp:=CALL_MMAP(m,rsize);
   if (mp<>nil) then
   begin
    tbase:=mp;
    tsize:=rsize;
    mmap_flag:=USE_MMAP_BIT;
   end;
  end;
 end;

 //if (HAVE_MORECORE && tbase == CMFAIL) then skip

 if (tbase<>nil) then
 begin

  m^.footprint:=m^.footprint+tsize;
  if (m^.footprint > m^.max_footprint) then
  begin
   m^.max_footprint:=m^.footprint;
  end;

  if (not is_initialized(m)) then
  begin
   if (m^.least_addr=nil) or (tbase < m^.least_addr) then
   begin
    m^.least_addr:=tbase;
   end;
   m^.seg.base  :=tbase;
   m^.seg.size  :=tsize;
   m^.seg.sflags:=mmap_flag;
   m^.magic     :=DEFAULT_MAGIC;
   init_bins(m);
   //#if !ONLY_MSPACES
   //     if (is_global(m)) then
   //     begin
   //       init_top(m, mchunkptr(tbase), tsize - TOP_FOOT_SIZE);
   //     end else
   //#endif
   begin
    mn:=next_chunk(mem2chunk(m));
    init_top(m, mn, ((tbase + tsize) - Pointer(mn)) -TOP_FOOT_SIZE);
   end;
  end else
  begin
   sp:=@m^.seg;

   while (sp <> nil) and (tbase <> (sp^.base + sp^.size)) do
   begin
    sp:=sp^.next;
   end;

   if (sp <> nil) and
      (not is_extern_segment(sp)) and
      ((sp^.sflags and USE_MMAP_BIT) = mmap_flag) and
      (segment_holds(sp, m^.top)) then
   begin
    sp^.size:=sp^.size+tsize;
    init_top(m, m^.top, m^.topsize + tsize);
   end else
   begin
    if (tbase < m^.least_addr) then
    begin
     m^.least_addr:=tbase;
    end;
    sp:=@m^.seg;
    while (sp <> nil) and (sp^.base <> (tbase + tsize)) do
    begin
     sp:=sp^.next;
    end;
    if (sp <> nil) and
       (not is_extern_segment(sp)) and
       ((sp^.sflags and USE_MMAP_BIT) = mmap_flag) then
    begin
     oldbase:=sp^.base;
     sp^.base:=tbase;
     sp^.size:=sp^.size+tsize;
     Exit(prepend_alloc(m, tbase, oldbase, nb));
    end else
    begin
     add_segment(m, tbase, tsize, mmap_flag);
    end;
   end;
  end;

  if (nb < m^.topsize) then
  begin
   rsize:=m^.topsize-nb;
   m^.topsize:=rsize;
   p:=m^.top;
   r:=chunk_plus_offset(p, nb);
   m^.top:=r;
   r^.head:=rsize or PINUSE_BIT;
   set_size_and_pinuse_of_inuse_chunk(m, p, nb);
   //check_top_chunk(m, m^.top); debug
   //check_malloced_chunk(m, chunk2mem(p), nb); debug
   Exit(chunk2mem(p));
  end;
 end;

 MALLOC_FAILURE_ACTION;
 Result:=nil;
end;

function release_unused_segments(m:mstate):ptruint;
var
 released:ptruint;
 nsegs:integer;
 pred:msegmentptr;
 sp:msegmentptr;
 base:Pointer;
 size:ptruint;
 next:msegmentptr;
 p:mchunkptr;
 psize:ptruint;
 tp:tchunkptr;
begin
 released:=0;
 nsegs:=0;
 pred:=@m^.seg;
 sp:=pred^.next;
 while (sp<>nil) do
 begin
  base:=sp^.base;
  size:=sp^.size;
  next:=sp^.next;
  inc(nsegs);
  if is_mmapped_segment(sp) and (not is_extern_segment(sp)) then
  begin
   p:=align_as_chunk(base);
   psize:=chunksize(p);

   if (not is_inuse(p)) and ((Pointer(p) + psize) >= (base + size - TOP_FOOT_SIZE)) then
   begin
    tp:=tchunkptr(p);
    assert(segment_holds(sp, sp));
    if (p=m^.dv) then
    begin
     m^.dv:=nil;
     m^.dvsize:=0;
    end else
    begin
     unlink_large_chunk(m, tp);
    end;
    if (CALL_MUNMAP(base, size)=0) then
    begin
     released:=released+size;
     m^.footprint:=m^.footprint-size;

     sp:=pred;
     sp^.next:=next;
    end else
    begin
     insert_large_chunk(m, tp, psize);
    end;
   end;
  end;
  pred:=sp;
  sp:=next;
 end;

 Result:=released;
end;

function sys_trim(m:mstate;pad:ptruint):Integer;
var
 released:ptruint;
 _unit:ptruint;
 extra:ptruint;
 sp:msegmentptr;
 newsize:ptruint;
begin
 released:=0;

 if (pad < ptruint(MAX_REQUEST)) and is_initialized(m) then
 begin
  pad:=pad+TOP_FOOT_SIZE;

  if (m^.topsize > pad) then
  begin

   _unit:=DEFAULT_GRANULARITY;
   extra:=((m^.topsize - pad + (_unit - SIZE_T_ONE)) div _unit - SIZE_T_ONE) * _unit;
   sp:=segment_holding(m, m^.top);

   if (not is_extern_segment(sp)) then
   begin
    if (is_mmapped_segment(sp)) then
    begin
      if (sp^.size >= extra) and
         (not has_segment_link(m, sp)) then
      begin
       newsize:=sp^.size - extra;

       if (CALL_MREMAP(m,sp^.base,sp^.size,newsize)<>nil) or
          (CALL_MUNMAP(sp^.base+newsize, extra)=0) then
       begin
        released:=extra;
       end;
      end;
    end;
    //else if (HAVE_MORECORE) then
   end;

   if (released <> 0) then
   begin
    sp^.size:=sp^.size-released;
    m^.footprint:= m^.footprint-released;
    init_top(m, m^.top, m^.topsize - released);
    //check_top_chunk(m, m^.top); debug
   end;
  end;

  //if (HAVE_MMAP)
   released:=released+release_unused_segments(m);

  if (released=0) and (m^.topsize > m^.trim_check) then
  begin
   m^.trim_check:=MAX_SIZE_T;
  end;
 end;

 if (released<>0) then
 begin
  Result:=1;
 end else
 begin
  Result:=0;
 end;
end;

function tmalloc_large(m:mstate;nb:ptruint):Pointer;
var
 v:tchunkptr;
 rsize:ptruint;
 t:tchunkptr;
 idx:bindex_t;
 sizebits:ptruint;
 rst:tchunkptr;
 rt:tchunkptr;
 trem:ptruint;
 leftbits:binmap_t;
 i:bindex_t;
 leastbit:binmap_t;
 r:mchunkptr;
begin
 v:=nil;
 rsize:=ptruint(-nb);
 idx:=0;
 compute_tree_index(nb, idx);
 t:=treebin_at(m, idx)^;
 if (t<>nil) then
 begin
  sizebits:=nb shl leftshift_for_tree_index(idx);
  rst:=nil;
  repeat
   trem:=chunksize(t) - nb;
   if (trem < rsize) then
   begin
    v:=t;
    rsize:=trem;
    if (rsize=0) then
    begin
     break;
    end;
   end;
   rt:=t^.child[1];
   t:=t^.child[(sizebits shr (SIZE_T_BITSIZE-SIZE_T_ONE)) and 1];
   if (rt<>nil) and (rt<>t) then
   begin
    rst:=rt;
   end;
   if (t=nil) then
   begin
    t:=rst;
    break;
   end;
   sizebits:=sizebits shl 1;
  until false;
 end;
 if (t=nil) and (v=nil) then
 begin
  leftbits:=left_bits(idx2bit(idx)) and m^.treemap;
  if (leftbits <> 0) then
  begin
   leastbit:=least_bit(leftbits);
   i:=0;
   compute_bit2idx(leastbit, i);
   t:=treebin_at(m, i)^;
  end;
 end;

 while (t<>nil) do
 begin
  trem:=chunksize(t) - nb;
  if (trem<rsize) then
  begin
   rsize:=trem;
   v:=t;
  end;
  t:=leftmost_child(t);
 end;

 if (v<>nil) and (rsize < (m^.dvsize - nb)) then
 begin
  if ok_address(m, v) then
  begin
   r:=chunk_plus_offset(v, nb);
   assert(chunksize(v)=(rsize + nb));
   if ok_next(v, r) then
   begin
    unlink_large_chunk(m, v);
    if (rsize < MIN_CHUNK_SIZE) then
    begin
     set_inuse_and_pinuse(m, v, (rsize + nb));
    end else
    begin
     set_size_and_pinuse_of_inuse_chunk(m, v, nb);
     set_size_and_pinuse_of_free_chunk(r, rsize);
     insert_chunk(m, r, rsize);
    end;
    Exit(chunk2mem(v));
   end;
  end;
  CORRUPTION_ERROR_ACTION(m);
 end;
 Result:=nil;
end;

function tmalloc_small(m:mstate;nb:ptruint):Pointer;
var
 t:tchunkptr;
 v:tchunkptr;
 rsize:ptruint;
 i:bindex_t;
 leastbit:bindex_t;
 trem:ptruint;
 r:mchunkptr;
begin
 leastbit:=least_bit(m^.treemap);
 i:=0;
 compute_bit2idx(leastbit, i);
 v:=treebin_at(m, i)^;
 t:=v;
 rsize:=chunksize(t) - nb;

 while true do
 begin
  t:=leftmost_child(t);
  if (t=nil) then Break;

  trem:=chunksize(t) - nb;
  if (trem < rsize) then
  begin
   rsize:=trem;
   v:=t;
  end;
 end;

 if ok_address(m, v) then
 begin
  r:=chunk_plus_offset(v, nb);
  assert(chunksize(v) = (rsize + nb));
  if ok_next(v, r) then
  begin
   unlink_large_chunk(m, v);
   if (rsize < MIN_CHUNK_SIZE) then
   begin
    set_inuse_and_pinuse(m, v, (rsize + nb));
   end else
   begin
    set_size_and_pinuse_of_inuse_chunk(m, v, nb);
    set_size_and_pinuse_of_free_chunk(r, rsize);
    replace_dv(m, r, rsize);
   end;
   Exit(chunk2mem(v));
  end;
 end;

 CORRUPTION_ERROR_ACTION(m);
 Result:=nil;
end;

function  mspace_malloc_implementation(ms:mstate;bytes:ptruint):Pointer; forward;
procedure mspace_free(ms:mstate;mem:Pointer); forward;

function  internal_memalign(m:mstate;alignment,bytes:ptruint):Pointer; forward;

function  internal_malloc(m:mstate;bytes:ptruint):Pointer; inline;
begin
 Result:=mspace_malloc_implementation(m,bytes);
end;

procedure internal_free(m:mstate;mem:Pointer); inline;
begin
 mspace_free(m,mem);
end;

function internal_realloc(m:mstate;oldmem:Pointer;bytes,alignment:ptruint):Pointer;
var
 oldp:mchunkptr;
 oldsize:ptruint;
 next:mchunkptr;
 newp:mchunkptr;
 extra:Pointer;
 nb:ptruint;
 rsize:ptruint;
 remainder:mchunkptr;
 newsize:ptruint;
 newtopsize:ptruint;
 newtop:mchunkptr;
 newmem:Pointer;
 oc:ptruint;
begin
 if (bytes >= ptruint(MAX_REQUEST)) then
 begin
  MALLOC_FAILURE_ACTION;
  Exit(nil);
 end;
 if (PREACTION(m)=0) then
 begin
  oldp:=mem2chunk(oldmem);
  oldsize:=chunksize(oldp);
  next:=chunk_plus_offset(oldp, oldsize);
  newp:=nil;
  extra:=nil;

  if ok_address(m, oldp) and ok_inuse(oldp) and
     ok_next(oldp, next) and ok_pinuse(next) then
  begin
   nb:=request2size(bytes);
   if is_mmapped(oldp) then
   begin
    newp:=mmap_resize(m, oldp, nb);
   end else
   if (oldsize >= nb) then
   begin
    rsize:=oldsize - nb;
    newp:=oldp;
    if (rsize >= MIN_CHUNK_SIZE) then
    begin
     remainder:=chunk_plus_offset(newp, nb);
     set_inuse(m, newp, nb);
     set_inuse_and_pinuse(m, remainder, rsize);
     extra:=chunk2mem(remainder);
    end;
   end else
   if (next = m^.top) and ((oldsize + m^.topsize) > nb) then
   begin
    newsize:=oldsize + m^.topsize;
    newtopsize:=newsize - nb;
    newtop:=chunk_plus_offset(oldp, nb);
    set_inuse(m, oldp, nb);
    newtop^.head:=newtopsize or PINUSE_BIT;
    m^.top:=newtop;
    m^.topsize:=newtopsize;
    newp:=oldp;
   end;
  end else
  begin
   USAGE_ERROR_ACTION(m, oldmem);
   POSTACTION(m);
   Exit(nil);
  end;
  //#if DEBUG
  //    if (newp != 0) begin
  //      check_inuse_chunk(m, newp);
  //    end
  //#endif

  POSTACTION(m);

  if (newp <> nil) then
  begin
   if (extra <> nil) then
   begin
    internal_free(m, extra);
   end;
   Exit(chunk2mem(newp));
  end else
  //if (not(flags and M2_PREVENT_MOVE)) then
  begin
   if (alignment > MALLOC_ALIGNMENT) then
   begin
    newmem:=internal_memalign(m,alignment,bytes)
   end else
   begin
    newmem:=internal_malloc(m,bytes);
   end;

   if (newmem<>nil) then
   begin
    oc:=oldsize - overhead_for(oldp);
    if (oc<bytes) then bytes:=oc;
    Move(oldmem^,newmem^,bytes);
    internal_free(m, oldmem);
   end;
   Exit(newmem);
  end;
 end;
 Result:=nil;
end;

function internal_memalign(m:mstate;alignment,bytes:ptruint):Pointer;
var
 a:ptruint;
 nb:ptruint;
 req:ptruint;
 mem:Pointer;
 leader:Pointer;
 trailer:Pointer;
 p:mchunkptr;
 br:Pointer;
 pos:Pointer;
 newp:mchunkptr;
 leadsize:ptruint;
 newsize:ptruint;
 size:ptruint;
 remainder_size:ptruint;
 remainder:mchunkptr;
begin
 if (alignment <= MALLOC_ALIGNMENT) then
 begin
  Exit(internal_malloc(m, bytes));
 end;
 if (alignment<MIN_CHUNK_SIZE) then
 begin
  alignment:=MIN_CHUNK_SIZE;
 end;
 if ((alignment and (alignment-SIZE_T_ONE)) <> 0) then
 begin
  a:=MALLOC_ALIGNMENT shl 1;
  while (a < alignment) do a:=a shl 1;
  alignment:=a;
 end;

 if (bytes >= (ptruint(MAX_REQUEST) - alignment)) then
 begin
  if (m<>nil) then
  begin
   MALLOC_FAILURE_ACTION;
  end;
 end else
 begin
  nb:=request2size(bytes);
  req:=nb + alignment + MIN_CHUNK_SIZE - CHUNK_OVERHEAD;
  mem:=internal_malloc(m, req);
  if (mem <> nil) then
  begin
   leader:=nil;
   trailer:=nil;
   p:=mem2chunk(mem);

   if (PREACTION(m)<>0) then Exit(nil);

   if ((ptruint(mem) and alignment) <> 0) then
   begin

    br:=mem2chunk(Pointer( ptruint(mem + alignment - SIZE_T_ONE) and ptruint(-alignment) ));

    if (ptruint(br - Pointer(p)) >= MIN_CHUNK_SIZE) then
    begin
     pos:=br;
    end else
    begin
     pos:=br+alignment;
    end;

    newp:=mchunkptr(pos);
    leadsize:=pos - Pointer(p);
    newsize:=chunksize(p) - leadsize;

    if is_mmapped(p) then
    begin
     newp^.prev_foot:=p^.prev_foot + leadsize;
     newp^.head:=newsize;
    end else
    begin
     set_inuse(m, newp, newsize);
     set_inuse(m, p, leadsize);
     leader:=chunk2mem(p);
    end;
    p:=newp;
   end;

   if (not is_mmapped(p)) then
   begin
    size:=chunksize(p);
    if (size > (nb + MIN_CHUNK_SIZE)) then
    begin
     remainder_size:=size - nb;
     remainder:=chunk_plus_offset(p, nb);
     set_inuse(m, p, nb);
     set_inuse(m, remainder, remainder_size);
     trailer:=chunk2mem(remainder);
    end;
   end;

   assert(chunksize(p) >= nb);
   assert((ptruint(chunk2mem(p)) and alignment) = 0);
   //check_inuse_chunk(m, p); debug
   POSTACTION(m);
   if (leader <> nil) then
   begin
    internal_free(m, leader);
   end;
   if (trailer <> nil) then
   begin
    internal_free(m, trailer);
   end;
   Exit(chunk2mem(p));
  end;
 end;
 Result:=nil;
end;

const
 pad_request_malloc_state=1312;

function init_user_mstate(tbase:Pointer;tsize:ptruint):mstate;
const
 msize=pad_request_malloc_state;
var
 mn:mchunkptr;
 msp:mchunkptr;
 m:mstate;
begin
 msp:=align_as_chunk(tbase);
 m:=chunk2mem(msp);
 FillChar(m^,msize,0);

 msp^.head:=(msize or INUSE_BITS);
 m^.seg.base        :=tbase;
 m^.least_addr      :=tbase;
 m^.seg.size        :=tsize;
 m^.footprint       :=tsize;
 m^.max_footprint   :=tsize;
 m^.SystemSize      :=tsize;

 init_bins(m);
 mn:=next_chunk(mem2chunk(m));
 init_top(m,mn,ptruint((tbase + tsize) - Pointer(mn)) - TOP_FOOT_SIZE);
 //check_top_chunk(m, m^.top); debug
 Result:=m;
end;

function destroy_mspace(ms:mstate):ptruint;
var
 sp:msegmentptr;
 base:Pointer;
 size:ptruint;
 flag:flag_t;
begin
 Result:=0;

 if (not ok_magic(ms)) then
 begin
  USAGE_ERROR_ACTION(ms,ms);
  Exit(0);
 end;

 DESTROY_LOCK(ms);

 //msp_array_id ignore

 if (ms^.mflags and EXTERN_BIT)<>0 then
 begin
  sp:=@ms^.seg;
  while (sp<>nil) do
  begin
   base:=sp^.base;
   size:=sp^.size;
   flag:=sp^.sflags;
   sp:=sp^.next;
   if ((flag and USE_MMAP_BIT)<>0) then
   begin
    CALL_MUNMAP(base,size);
    Result:=1;
   end;
  end;
 end;

 Result:=freed;
end;

function mspace_malloc_implementation(ms:mstate;bytes:ptruint):Pointer;
label
 _postaction;
var
 mem:Pointer;
 nb:ptruint;
 idx:bindex_t;
 smallbits:binmap_t;
 b:mchunkptr;
 p:mchunkptr;
 r:mchunkptr;
 rsize:ptruint;
 i:bindex_t;
 leftbits:binmap_t;
 leastbit:binmap_t;
 dvs:ptruint;
begin
 if (PREACTION(ms)=0) then
 begin
  nb:=bytes;
  //if ((flags and M2_ALWAYS_MMAP)=0) then
  begin
   if (bytes <= MAX_SMALL_REQUEST) then
   begin

    if (bytes < MIN_REQUEST) then
    begin
     nb:=MIN_CHUNK_SIZE;
    end else
    begin
     nb:=pad_request(bytes);
    end;

    idx:=small_index(nb);
    smallbits:=ms^.smallmap shr idx;

    if ((smallbits and 3) <> 0) then
    begin
     idx:=idx+((not smallbits) and 1);
     b:=smallbin_at(ms, idx);
     p:=b^.fd;
     assert(chunksize(p)=small_index2size(idx));
     unlink_first_small_chunk(ms, b, p, idx);
     set_inuse_and_pinuse(ms, p, small_index2size(idx));
     mem:=chunk2mem(p);
     //check_malloced_chunk(ms, mem, nb); debug
     goto _postaction;
    end else
    if (nb > ms^.dvsize) then
    begin
     if (smallbits <> 0) then
     begin
      leftbits:=(smallbits shl idx) and left_bits(idx2bit(idx));
      leastbit:=least_bit(leftbits);
      i:=0;
      compute_bit2idx(leastbit, i);
      b:=smallbin_at(ms, i);
      p:=b^.fd;
      assert(chunksize(p)=small_index2size(i));
      unlink_first_small_chunk(ms, b, p, i);
      rsize:=small_index2size(i) - nb;

      if (SIZE_T_SIZE <> 4) and (rsize < MIN_CHUNK_SIZE) then
      begin
       set_inuse_and_pinuse(ms, p, small_index2size(i));
      end else
      begin
       set_size_and_pinuse_of_inuse_chunk(ms, p, nb);
       r:=chunk_plus_offset(p, nb);
       set_size_and_pinuse_of_free_chunk(r, rsize);
       replace_dv(ms, r, rsize);
      end;
      mem:=chunk2mem(p);
      //check_malloced_chunk(ms, mem, nb); debug
      goto _postaction;
     end else
     begin
      mem:=tmalloc_small(ms, nb);
      if (ms^.treemap<>0) and (mem<>nil) then
      begin
       //check_malloced_chunk(ms, mem, nb); debug
       goto _postaction;
      end;
     end;
    end;
   end else
   if (bytes >= ptruint(MAX_REQUEST)) then
   begin
    nb:=MAX_SIZE_T;
   end else
   begin
    nb:=pad_request(bytes);
    mem:=tmalloc_large(ms, nb);
    if (ms^.treemap<>0) and (mem<>nil) then
    begin
     //check_malloced_chunk(ms, mem, nb); debug
     goto _postaction;
    end;
   end;

   if (nb <= ms^.dvsize) then
   begin
    rsize:=ms^.dvsize - nb;
    p:=ms^.dv;
    if (rsize >= MIN_CHUNK_SIZE) then
    begin
     r:=chunk_plus_offset(p, nb);
     ms^.dv:=r;
     ms^.dvsize:=rsize;
     set_size_and_pinuse_of_free_chunk(r, rsize);
     set_size_and_pinuse_of_inuse_chunk(ms, p, nb);
    end else
    begin
     dvs:=ms^.dvsize;
     ms^.dvsize:=0;
     ms^.dv:=nil;
     set_inuse_and_pinuse(ms, p, dvs);
    end;
    mem:=chunk2mem(p);
    //check_malloced_chunk(ms, mem, nb); debug
    goto _postaction;
   end else
   if (nb < ms^.topsize) then
   begin
    rsize:=ms^.topsize-nb;
    ms^.topsize:=rsize;
    p:=ms^.top;
    r:=chunk_plus_offset(p, nb);
    ms^.top:=r;
    r^.head:=rsize or PINUSE_BIT;
    set_size_and_pinuse_of_inuse_chunk(ms, p, nb);
    mem:=chunk2mem(p);
    //check_top_chunk(ms, ms^.top); debug
    //check_malloced_chunk(ms, mem, nb); debug
    goto _postaction;
   end;
  end;

  mem:=sys_alloc(ms,nb);

 _postaction:
  POSTACTION(ms);
  Exit(mem);
 end;

 Result:=nil;
end;

function mspace_malloc(ms:mstate;bytes:ptruint):Pointer;
begin
 if (not ok_magic(ms)) then
 begin
  USAGE_ERROR_ACTION(ms,ms);
  Exit(nil);
 end;
 Result:=mspace_malloc_implementation(ms,bytes);
end;

function mspace_malloc2(ms:mstate;bytes,alignment:ptruint):Pointer;
begin
 if (not ok_magic(ms)) then
 begin
  USAGE_ERROR_ACTION(ms,ms);
  Exit(nil);
 end;
 if (alignment <= MALLOC_ALIGNMENT) then
 begin
  Result:=mspace_malloc_implementation(ms, bytes);
 end else
 begin
  Result:=internal_memalign(ms, alignment, bytes);
 end;
end;

procedure mspace_free(ms:mstate;mem:Pointer);
label
 _postaction,
 _erroraction;
var
 p:mchunkptr;
 fm:mstate;
 psize:ptruint;
 next:mchunkptr;
 prevsize:ptruint;
 mm:Pointer;
 prev:mchunkptr;
 tsize:ptruint;
 dsize:ptruint;
 nsize:ptruint;
 tp:tchunkptr;
begin
 if (mem<>nil) then
 begin
  p:=mem2chunk(mem);

  fm:=get_mstate_for(p);
  Assert(fm=ms);

  if (not ok_magic(fm)) then
  begin
   USAGE_ERROR_ACTION(fm, p);
   Exit;
  end;

  if (PREACTION(fm)=0) then
  begin
   //check_inuse_chunk(fm, p); debug
   if ok_address(fm, p) and ok_inuse(p) then
   begin
    psize:=chunksize(p);
    next:=chunk_plus_offset(p, psize);
    if (not pinuse(p)) then
    begin
     prevsize:=p^.prev_foot;
     if (is_mmapped(p)) then
     begin
      mm:=Pointer(p) - prevsize;
      psize:=psize+prevsize+MMAP_FOOT_PAD;
      if (CALL_MUNMAP(mm, psize)=0) then
      begin
       fm^.footprint:=fm^.footprint-psize;
      end;
      goto _postaction;
     end else
     begin
      prev:=chunk_minus_offset(p, prevsize);
      psize:=psize+prevsize;
      p:=prev;
      if ok_address(fm, prev) then
      begin
       if (p<>fm^.dv) then
       begin
        unlink_chunk(fm, p, prevsize);
       end else
       if ((next^.head and INUSE_BITS) = INUSE_BITS) then
       begin
        fm^.dvsize:=psize;
        set_free_with_pinuse(p, psize, next);
        goto _postaction;
       end
      end else
      begin
       goto _erroraction;
      end;;
     end;
    end;

    if ok_next(p, next) and ok_pinuse(next) then
    begin
     if (not cinuse(next)) then
     begin
      if (next=fm^.top) then
      begin
       tsize:=fm^.topsize+psize;
       fm^.topsize:=tsize;
       fm^.top:=p;
       p^.head:=tsize or PINUSE_BIT;
       if (p=fm^.dv) then
       begin
        fm^.dv:=nil;
        fm^.dvsize:=0;
       end;
       if (should_trim(fm, tsize)) then
       begin
        sys_trim(fm, 0);
       end;
       goto _postaction;
      end else
      if (next=fm^.dv) then
      begin
       dsize:=fm^.dvsize+psize;
       fm^.dvsize:=dsize;
       fm^.dv:=p;
       set_size_and_pinuse_of_free_chunk(p, dsize);
       goto _postaction;
      end else
      begin
       nsize:=chunksize(next);
       psize:=psize+nsize;
       unlink_chunk(fm, next, nsize);
       set_size_and_pinuse_of_free_chunk(p, psize);
       if (p=fm^.dv) then
       begin
        fm^.dvsize:=psize;
        goto _postaction;
       end;
      end;
     end else
     begin
      set_free_with_pinuse(p, psize, next);
     end;

     if is_small(psize) then
     begin
      insert_small_chunk(fm, p, psize);
      //check_free_chunk(fm, p); debug
     end else
     begin
      tp:=tchunkptr(p);
      insert_large_chunk(fm, tp, psize);
      //check_free_chunk(fm, p); debug
      //if (--fm^.release_checks == 0)
      //  release_unused_segments(fm);
     end;
     goto _postaction;
    end;
   end;
  _erroraction:
   USAGE_ERROR_ACTION(fm, p);
  _postaction:
   POSTACTION(fm);
  end;
 end;
end;

function mspace_calloc(ms:mstate;n_elements,elem_size:ptruint):Pointer;
var
 mem:Pointer;
 req:ptruint;
 p:mchunkptr;
begin
 if (not ok_magic(ms)) then
 begin
  USAGE_ERROR_ACTION(ms,ms);
  Exit(nil);
 end;
 if (n_elements<>0) then
 begin
  req:=n_elements*elem_size;
  if boolean((n_elements or elem_size) and (not ptruint($ffff))) and
            ((req div n_elements) <> elem_size) then
  begin
   req:=MAX_SIZE_T;
  end;
 end;
 mem:=internal_malloc(ms,req);
 if (mem<>nil) then
 begin
  p:=mem2chunk(mem);
  if calloc_must_clear(p) then
  begin
   FillChar(mem^,chunksize(p) - overhead_for(p),0);
  end;
 end;
 Result:=mem;
end;

function mspace_realloc2(ms:mstate;oldmem:Pointer;bytes,alignment:ptruint):Pointer;
var
 p:mchunkptr;
 fm:mstate;
begin
 if (oldmem=nil) then
 begin
  Exit(mspace_malloc2(ms,bytes,alignment));
 end;

 if (bytes=0) then
 begin
  mspace_free(ms,oldmem);
  Exit(nil);
 end else
 begin
  p:=mem2chunk(oldmem);

  fm:=get_mstate_for(p);
  Assert(fm=ms);

  if (not ok_magic(fm)) then
  begin
   USAGE_ERROR_ACTION(fm,fm);
   Exit(nil);
  end;

  Exit(internal_realloc(fm,oldmem,bytes,alignment));
 end;
end;

function mspace_realloc(ms:mstate;oldmem:Pointer;bytes:ptruint):Pointer;
begin
 Result:=mspace_realloc2(ms,oldmem,bytes,0);
end;

function mspace_memalign(ms:mstate;alignment,bytes:ptruint):Pointer;
begin
 if (not ok_magic(ms)) then
 begin
  USAGE_ERROR_ACTION(ms,ms);
  Exit(nil);
 end;
 Result:=internal_memalign(ms,alignment,bytes);
end;

function mspace_trim(ms:mstate;pad:ptruint):Integer;
begin
 Result:=0;
 if (not ok_magic(ms)) then
 begin
  USAGE_ERROR_ACTION(ms,ms);
  Exit(0);
 end;
 if (PREACTION(ms)=0) then
 begin
  Result:=sys_trim(ms,pad);
  POSTACTION(ms);
 end;
end;

function mspace_usable_size(mem:Pointer):ptruint;
var
 p:mchunkptr;
begin
 if (mem<>nil) then
 begin
  p:=mem2chunk(mem);
  if is_inuse(p) then
  begin
   Exit(chunksize(p) - overhead_for(p));
  end;
 end;
 Result:=0;
end;

/////

function ps4_sceLibcMspaceCreate(name:PChar;base:Pointer;capacity:size_t;flag:Integer):pSceLibcMspace; SysV_ABI_CDecl;
var
 ALLOC_BASE:Boolean;
 is_alloc:Boolean;
 m_count:Integer;
 r:Integer;
 str:shortstring;
begin
 Result:=nil;

 m_count:=1440;
 if (SDK_VERSION<>0) and (SDK_VERSION<=$34fffff) then
 begin
  m_count:=1408;
 end;

 ALLOC_BASE:=(flag and SCE_LIBC_MSPACE_ALLOC_BASE)<>0;

 if ((not ALLOC_BASE) or (base<>nil)) and
    (
     (ptruint(base)<$400000) or
     (capacity>$fbffc00000)  or
     ((ptruint(base)+capacity)>$fbffffffff)
    ) then Exit;

 if (flag>15) then Exit;

 if (SDK_VERSION<>0) and (SDK_VERSION<=$34fffff) then
 begin
  if (capacity<=m_count) or (((ptruint(base) or capacity) and 7)<>0) then Exit;
 end else
 begin
  if (capacity<=m_count) then Exit;
 end;
 if (ptruint(-(m_count or $4000)) <= capacity) then Exit;

 is_alloc:=False;
 if ALLOC_BASE and (base=nil) then
 begin
  capacity:=Align(capacity,64*1024);
  r:=ps4_sceKernelMapNamedFlexibleMemory(@base,capacity,3,0,name);
  if (r<>0) then Exit;
  is_alloc:=True;
 end;

 Result:=init_user_mstate(base, capacity);

 Writeln(HexStr(Result),' ',HexStr(pSceLibcMspace(base+base_align+16)));

 if is_alloc then
 begin
  Result^.seg.sflags:=Result^.seg.sflags or USE_MMAP_BIT;
 end;

 Result^.currentInuseSize:=1440;
 Result^.maxInuseSize    :=1440;
 Result^.magic           :=DEFAULT_MAGIC;
 Result^.mflags          :=USE_MMAP_BIT or USE_LOCK_BIT or 4;
 Result^.page_size       :=16384;
 Result^.granularity     :=65536;

 INITIAL_LOCK(m);

 if (name=nil) then
 begin
  str:=Default(shortstring);
  str:='SceLibcMspace'+HexStr(base);
  name:=@str[1];
 end;
 MoveChar0(name^,Result^.name,32);
 Result^.name[31]:=#0;

 if (flag and SCE_LIBC_MSPACE_THREAD_UNSAFE)<>0 then
 begin
  disable_lock(Result);
 end;

 if ALLOC_BASE then
 begin
  Result^.mflags:=Result^.mflags or EXTERN_BIT;
  Result^.SystemSize:=ptruint($ffffffffffffffff);
 end;

 if (flag and SCE_LIBC_MSPACE_DEBUG_SHORTAGE)<>0 then
 begin
  Result^.debug_flags:= Result^.debug_flags or 4;
 end;

 if (flag and SCE_LIBC_MSPACE_IN_ARRAY)<>0 then
 begin
  Result^.msp_array_flags:=Result^.msp_array_flags or 1;
 end;

 if (
     (CompareChar0(name,'basic_ipc',9)  =0) or
     (CompareChar0(name,'SceLibSsl',9)  =0) or
     (CompareChar0(name,'SceLibHttp',10)=0) or
     (CompareChar0(name,'SceNp',5)      =0)
    ) and
    (CompareChar0(name,'SceNpPresence',14)<>0) then
 begin
  Result^.msp_array_flags:=0;
 end else
 begin
  Result^.msp_array_id:=1; //fake id
 end;

end;



end.



