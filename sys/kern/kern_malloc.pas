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

function  malloc (size :QWORD):Pointer;
function  calloc (size :QWORD):Pointer;
function  malloc0(size :QWORD):Pointer;
function  realloc(addr:Pointer;size:QWORD):Pointer;
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
 HEAP_HEADER_MARKER=$FF;

 { Header sizes }
 BUCKET_HDR=SizeOf(t_bucket_hdr);  // 1
 HEAP_HDR  =SizeOf(t_heap_hdr);    // 8

{ Small malloc allocations are served from fixed-size UMA buckets.
 *
 * Bucket sizes were chosen so that, after adding the 1-byte bucket header
 * and linkage in keg_small_init(), each slot divides the ~4060-byte usable
 * slab as evenly as possible (utilization >= ~95%, most ~99%).  Small
 * allocations use a fine-grained ladder to minimise rounding waste; the
 * largest bucket is 4048 (a full ~4K slab, util ~99.9%) because a request
 * of 4056 plus the header cannot fit a slab
 }
const
 BUCKET_SIZES:array[0..29] of Integer=(
  16,24,32,40,48,56,64,80,96,112,128,
  160,192,224,256,320,384,432,640,800,992,
  1344,1984,2023,
  2048,2560,3072,3840,4032,4048);
 BUCKET_COUNT=High(BUCKET_SIZES)+1;

var
 kz_zone:array[0..BUCKET_COUNT-1] of uma_zone_t;

const
 BUCKET_NAMES:array[0..BUCKET_COUNT-1] of PChar=(
  'malloc_16','malloc_24','malloc_32','malloc_40','malloc_48','malloc_56',
  'malloc_64','malloc_80','malloc_96','malloc_112','malloc_128',
  'malloc_160','malloc_192','malloc_224','malloc_256','malloc_320',
  'malloc_384','malloc_432','malloc_640','malloc_800','malloc_992',
  'malloc_1344','malloc_1984','malloc_2023',
  'malloc_2048','malloc_2560','malloc_3072','malloc_3840','malloc_4032',
  'malloc_4048');

type
 t_zone_cb=procedure(i:Integer);

procedure null_zone(i:Integer);
begin
 //
end;

var
 zone_mtx:mtx;
 kz_zone_init:t_zone_cb=@null_zone;

procedure init_zone(i:Integer);
begin
 mtx_lock(zone_mtx);

 if (kz_zone[i]=nil) then
 begin
  kz_zone[i]:=uma_zcreate(BUCKET_NAMES[i], BUCKET_HDR+BUCKET_SIZES[i],
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

function bucket_of(size:QWORD):Integer;
var
 i:Integer;
begin
 for i:=0 to BUCKET_COUNT-1 do
 begin
  if (size<=QWORD(BUCKET_SIZES[i])) then
  begin
   Exit(i);
  end;
 end;
 Result:=-1;
end;

function malloc(size:QWORD):Pointer;
var
 indx:Integer;
 bh:p_bucket_hdr;
 hh:p_heap_hdr;
begin
 if (size=0) then size:=1;

 indx:=bucket_of(size);
 if (indx>=0) then
 begin
  if (kz_zone[indx]=nil) then
  begin
   kz_zone_init(indx);
  end;

  if (kz_zone[indx]<>nil) then
  begin
   bh:=p_bucket_hdr(uma_zalloc(kz_zone[indx], M_WAITOK));
   if (bh<>nil) then
   begin
    bh^.indx:=indx;
    Exit(bh+1);
   end;
  end;
 end;

 hh:=GetMem(HEAP_HDR+size);
 hh^.size  :=size;
 hh^.marker:=HEAP_HEADER_MARKER;
 Exit(hh+1);
end;

function calloc(size:QWORD):Pointer;
var
 p:Pointer;
begin
 p:=malloc(size);
 if (p<>nil) then FillChar(p^,size,0);
 Result:=p;
end;

function malloc0(size:QWORD):Pointer;
begin
 Result:=calloc(size);
end;

function realloc(addr:Pointer;size:QWORD):Pointer;
var
 b:PByte;
 indx:Integer;
 oldsize:QWORD;
 hh:p_heap_hdr;
 p:Pointer;
begin
 if (addr=nil) then
 begin
  Exit(malloc(size));
 end;

 b:=PByte(addr);

 if (b[-1]=HEAP_HEADER_MARKER) then
 begin
  hh:=p_heap_hdr(b-HEAP_HDR);
  oldsize:=hh^.size;
 end else
 begin
  indx:=Integer(b[-1]);
  oldsize:=BUCKET_SIZES[indx];
 end;

 { Reuse the original block when it fits and is not far larger. }
 if (size<=oldsize) and (size>(oldsize shr 1)) then
 begin
  Exit(addr);
 end;

 p:=malloc(size);
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

 if (b[-1]=HEAP_HEADER_MARKER) then
 begin
  FreeMem(b-HEAP_HDR);
 end else
 begin
  uma_zfree(kz_zone[b[-1]], b-1);
 end;

end;


end.


