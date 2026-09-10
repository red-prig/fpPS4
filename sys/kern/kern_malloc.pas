unit kern_malloc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

{
 * General purpose kernel memory allocator for variable length data.
 *
 * Transcribed from FreeBSD kern/kern_malloc.c.  Small allocations are
 * served from a set of fixed-size UMA buckets, so that repeatedly
 * allocating and freeing data of varying length (file names, paths,
 * symlink targets, ...) does not fragment the system heap.  Allocations
 * larger than the largest bucket fall back to the system heap allocator.
 *
 * UMA in this port requires the owning zone to be passed to the free
 * routine, so each returned block is preceded by a small header that
 * records its bucket index.  This replaces FreeBSD's vtoslab() lookup.
 *
 * The header width is variable and self-describing:
 *  - bucket blocks carry a single header byte: the bucket index (0..n).
 *  - heap blocks carry an 8-byte header whose LAST byte is the marker
 *    $FF; the preceding 56 bits hold the requested size in bytes.
 *  free()/realloc() disambiguate by inspecting the byte at addr-1, which
 *  is the bucket index for bucket blocks and the $FF marker for heap
 *  blocks (bucket indices never reach $FF).
}

interface

uses
 kern_mtx,
 uma;

const
 MALIGN_2=1;
 MALIGN_4=2;
 MALIGN_8=3;
 MSYSTM_8=4;

function  malloc (size:QWORD;flags:Integer=0):Pointer;
function  calloc (size:QWORD;flags:Integer=0):Pointer;
function  malloc0(size:QWORD;flags:Integer=0):Pointer;
function  msize  (addr:Pointer):QWORD;
function  realloc(addr:Pointer;size:QWORD;flags:Integer=0):Pointer;
procedure free   (addr:Pointer);

procedure malloc_init; //SYSINIT(kmem, SI_SUB_KMEM, SI_ORDER_FIRST, kmeminit, NULL);

implementation

type
 bit56=0..72057594037927935;

 p_bucket_hdr=^t_bucket_hdr;
 t_bucket_hdr=packed record   // 1 byte: bucket index
  indx:Byte;
 end;

 p_heap_hdr=^t_heap_hdr;
 t_heap_hdr=bitpacked record  // 8 bytes: 56-bit size + $FF marker
  size  :bit56;
  marker:Byte;
 end;

const
 HEAP_HEADER_MARKER_S=$FF;
 HEAP_HEADER_MARKER_8=$FE;
 HEAP_HEADER_MARKER_4=$FD;
 HEAP_HEADER_MARKER_2=$FC;

 { Header sizes }
 BUCKET_HDR=SizeOf(t_bucket_hdr);  // 1
 HEAP_HDR_8=SizeOf(t_heap_hdr);    // 8

{ Small malloc allocations are served from fixed-size UMA buckets.
 *
 * BUCKET_FULL_SIZES is the bucket ladder, sorted ascending.  Each entry is
 * the ALIGNED full item size, so it equals the true on-slab footprint (no
 * hidden alignment padding); payload capacity of a bucket is
 * BUCKET_FULL_SIZES[i]-BUCKET_HDR, reduced by the alignment offset for
 * aligned allocations.  Items smaller than UMA_SMALLEST_UNIT are clamped
 * up by keg, so buckets 8 and 16 share the same 16-byte slot layout.
 *
 * The ladder mixes three kinds of kegs:
 *  - fine-grained ONPAGE buckets (8..440): each slot divides the ~4060-byte
 *    usable slab almost evenly, utilization >= ~96%;
 *  - power-of-two buckets (512, 1024, 2048): these trip keg's automatic
 *    OFFPAGE mode (check_wasted in keg_small_init): the slab header moves
 *    to a dedicated slabzone and the page fills exactly, giving 100%
 *    utilization;
 *  - bucket 4096 goes through the multi-page keg path (keg_large_init) as
 *    a single whole page per item: again 100%.
 *
 * keg_large_init also supports true multi-page slabs (> 4096), but such a
 * bucket is only 100% efficient at multiples of 4096 and wastes up to a
 * whole slab in between, while the system heap fits those requests exactly;
 * so sizes above 4095 stay with the heap (see the header comment).
 }
const
 BUCKET_FULL_SIZES:array[0..28] of Integer=(
  16,24,32,40,48,56,64,72,88,104,120,136,
  168,200,232,264,328,392,440,512,648,808,1000,1024,
  1352,1992,2024,2048,4096);
 BUCKET_COUNT=High(BUCKET_FULL_SIZES)+1;

var
 kz_zone:array[0..BUCKET_COUNT-1] of uma_zone_t;

const
 BUCKET_NAMES:array[0..BUCKET_COUNT-1] of PChar=(
  'malloc_16','malloc_24','malloc_32','malloc_40','malloc_48',
  'malloc_56','malloc_64','malloc_72','malloc_88','malloc_104',
  'malloc_120','malloc_136','malloc_168','malloc_200','malloc_232',
  'malloc_264','malloc_328','malloc_392','malloc_440','malloc_512',
  'malloc_648','malloc_808','malloc_1000','malloc_1024','malloc_1352',
  'malloc_1992','malloc_2024','malloc_2048','malloc_4096');

type
 t_zone_cb=procedure(i:Integer);

procedure null_zone(i:Integer);
begin
 //
end;

var
 zone_mtx:mtx;
 kz_zone_init:t_zone_cb=@null_zone;

const
 ALIGN_OFFSET_BY_FLAG:array[0..3] of Byte=(0,1,3,7);
 MARKER_BY_FLAG      :array[0..3] of Byte=(0,HEAP_HEADER_MARKER_2,
                                             HEAP_HEADER_MARKER_4,
                                             HEAP_HEADER_MARKER_8);

 mkofs:array[0..255] of Byte=(
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,1,3,7,0);

procedure init_zone(i:Integer);
begin
 mtx_lock(zone_mtx);

 if (kz_zone[i]=nil) then
 begin
  kz_zone[i]:=uma_zcreate(BUCKET_NAMES[i], BUCKET_FULL_SIZES[i],
                          nil, nil, nil, nil,
                          UMA_ALIGN_PTR, 0);
 end;

 mtx_unlock(zone_mtx);
end;

procedure malloc_init;
begin
 mtx_init(zone_mtx,'kz_zone_mtx');
 kz_zone_init:=@init_zone;
end;

function bucket_of(reqsize:QWORD):Integer;
var
 lo,hi,mid:Integer;
begin
 lo:=0;
 hi:=BUCKET_COUNT-1;
 while (lo<hi) do
 begin
  mid:=(lo+hi) shr 1;
  if (reqsize>QWORD(BUCKET_FULL_SIZES[mid])) then
  begin
   lo:=mid+1;
  end else
  begin
   hi:=mid;
  end;
 end;
 if (reqsize<=QWORD(BUCKET_FULL_SIZES[lo])) then
  Result:=lo
 else
  Result:=-1;
end;

function malloc(size:QWORD;flags:Integer=0):Pointer;
var
 indx,offset,marker:Integer;
 bh:PByte;
 hh:p_heap_hdr;
begin
 if (size=0) then size:=1;

 if (flags<>MSYSTM_8) then
 begin

  if (flags<0) or (flags>3) then flags:=0;

  offset:=ALIGN_OFFSET_BY_FLAG[flags];
  marker:=MARKER_BY_FLAG[flags];

  { The zone must hold the payload plus alignment padding plus header. }
  indx:=bucket_of(size+offset+BUCKET_HDR);
  if (indx>=0) then
  begin
   if (kz_zone[indx]=nil) then
   begin
    kz_zone_init(indx);
   end;

   if (kz_zone[indx]<>nil) then
   begin
    bh:=uma_zalloc(kz_zone[indx], M_WAITOK);
    if (bh<>nil) then
    begin
     if (offset<>0) then bh[offset]:=marker;
     bh[0]:=indx;
     Exit(bh+offset+BUCKET_HDR);
    end;
   end;
  end;

 end; //(flags<>MSYSTM_8)

 hh:=GetMem(HEAP_HDR_8+size);
 hh^.size  :=size;
 hh^.marker:=HEAP_HEADER_MARKER_S;
 Exit(hh+1);
end;

function calloc(size:QWORD;flags:Integer=0):Pointer;
var
 p:Pointer;
begin
 p:=malloc(size,flags);
 if (p<>nil) then FillChar(p^,size,0);
 Result:=p;
end;

function malloc0(size:QWORD;flags:Integer=0):Pointer;
begin
 Result:=calloc(size,flags);
end;

function msize(addr:Pointer):QWORD;
var
 b:PByte;
 offset:Integer;
 hh:p_heap_hdr;
begin
 if (addr=nil) then Exit(0);
 b:=PByte(addr);

 if (b[-1]=HEAP_HEADER_MARKER_S) then
 begin
  hh:=p_heap_hdr(b-HEAP_HDR_8);
  Exit(hh^.size);
 end;

 offset:=mkofs[b[-1]];
 b:=b-offset;
 Result:=BUCKET_FULL_SIZES[b[-1]]-offset-BUCKET_HDR;
end;

function realloc(addr:Pointer;size:QWORD;flags:Integer=0):Pointer;
var
 oldsize:QWORD;
 p:Pointer;
begin
 if (addr=nil) then
 begin
  Exit(malloc(size,flags));
 end;

 oldsize:=msize(addr);

 { Reuse the original block when it fits and is not far larger. }
 if (size<=oldsize) and (size>(oldsize shr 1)) then
 begin
  Exit(addr);
 end;

 p:=malloc(size,flags);
 if (p=nil) then
 begin
  Exit(nil);
 end;

 if (oldsize>size) then oldsize:=size;
 Move(addr^,p^,oldsize);
 free(addr);

 Result:=p;
end;

procedure free(addr:Pointer);
var
 b:PByte;
begin
 if (addr=nil) then Exit;
 b:=PByte(addr);

 if (b[-1]=HEAP_HEADER_MARKER_S) then
 begin
  FreeMem(b-HEAP_HDR_8);
  Exit;
 end;

 b:=b-mkofs[b[-1]];
 uma_zfree(kz_zone[b[-1]], b-BUCKET_HDR);
end;


end.


