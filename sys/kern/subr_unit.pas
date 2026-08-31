unit subr_unit;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_mtx,
 mqueue;

type
 p_unrhdr=^t_unrhdr;
 t_unrhdr=packed record
  head   :TAILQ_HEAD;   //TAILQ_HEAD(unrhd,unr)
  low    :DWORD;        //u_int low;  Lowest item
  high   :DWORD;        //u_int high; Highest item
  busy   :DWORD;        //u_int busy; Count of allocated items
  alloc  :DWORD;        //u_int alloc; Count of memory allocations
  first  :DWORD;        //u_int first; items in allocated from start
  last   :DWORD;        //u_int last; items free at end
  mtx    :p_mtx;        //struct mtx *mtx;
  ppfree :TAILQ_HEAD;   //ppfree; Items to be freed after mtx lock dropped
 end;

function  new_unrhdr      (low,high:Integer; mutex:p_mtx):p_unrhdr;
procedure delete_unrhdr   (uh:p_unrhdr);
function  alloc_unrl      (uh:p_unrhdr):Integer;
function  alloc_unr       (uh:p_unrhdr):Integer;
function  alloc_unr_specific(uh:p_unrhdr; item:DWORD):Integer;
procedure free_unr        (uh:p_unrhdr; item:DWORD);
procedure clean_unrhdr    (uh:p_unrhdr);
procedure clean_unrhdrl   (uh:p_unrhdr);

implementation

type
 p_unr=^t_unr;
 t_unr=packed record
  list:TAILQ_ENTRY;
  len :DWORD;
  ptr :Pointer;
 end;

 p_unrb=^t_unrb;
 t_unrb=packed record
  busy:Byte;
  map :array[0..SizeOf(t_unr)-2] of Byte; //bitstr_t map[sizeof(struct unr)-1]
 end;

{$IF SizeOf(t_unr)<>SizeOf(t_unrb)}{$STOP sizeof(t_unr)<>sizeof(t_unrb)}{$ENDIF}

const
 //Number of bits in the bitmap
 NBITS=((SizeOf(t_unr)-1)*8);

function bit_test(map:Pointer; bit:DWORD):Boolean; inline;
begin
 Result:=(PByte(map)[bit shr 3] and (1 shl (bit and 7)))<>0;
end;

procedure bit_set(map:Pointer; bit:DWORD); inline;
var
 m:Byte;
begin
 m:=1 shl (bit and 7);
 PByte(map)[bit shr 3]:=PByte(map)[bit shr 3] or m;
end;

procedure bit_clear(map:Pointer; bit:DWORD); inline;
var
 m:Byte;
begin
 m:=1 shl (bit and 7);
 PByte(map)[bit shr 3]:=PByte(map)[bit shr 3] and (not m);
end;

procedure bit_nset(map:Pointer; s,e:DWORD); inline;
var
 i:DWORD;
begin
 for i:=s to e do bit_set(map,i);
end;

procedure bit_nclear(map:Pointer; s,e:DWORD); inline;
var
 i:DWORD;
begin
 for i:=s to e do bit_clear(map,i);
end;

procedure bit_ffc(map:Pointer; nbits:DWORD; var ret:Integer); inline;
var
 i:DWORD;
begin
 for i:=0 to nbits-1 do
 begin
  if not bit_test(map,i) then
  begin
   ret:=Integer(i);
   Exit;
  end;
 end;
 ret:=-1;
end;

var
 unitmtx:mtx;

procedure check_unrhdr(uh:p_unrhdr; line:Integer);
var
 up:p_unr;
 ub:p_unrb;
 x,y,z,w:DWORD;
begin
 y:=uh^.first;
 z:=0;

 up:=TAILQ_FIRST(@uh^.head);
 while (up<>nil) do
 begin
  Inc(z);

  if (up^.ptr<>Pointer(uh)) and (up^.ptr<>nil) then
  begin
   ub:=p_unrb(Pointer(up^.ptr));

   Assert(up^.len<=DWORD(NBITS),'UNR inconsistency: len>NBITS');
   Inc(z);

   w:=0;
   for x:=0 to up^.len-1 do
   begin
    if bit_test(@ub^.map,x) then Inc(w);
   end;

   Assert(w=ub^.busy,'UNR inconsistency: busy<>found');
   Inc(y,w);
  end else
  if (up^.ptr<>nil) then
  begin
   Inc(y,up^.len);
  end;

  up:=TAILQ_NEXT(up,@up^.list);
 end;

 Assert(y=uh^.busy,'UNR inconsistency: items<>found');
 Assert(z=uh^.alloc,'UNR inconsistency: chunks<>found');
end;

function new_unr(uh:p_unrhdr; p1,pp2:PPointer):Pointer;
var
 p:Pointer;
begin
 Inc(uh^.alloc);
 Assert((p1^<>nil) or (pp2^<>nil),'UNR: Out of cached memory');

 if (p1^<>nil) then
 begin
  p:=p1^;
  p1^:=nil;
  Exit(p);
 end else
 begin
  p:=pp2^;
  pp2^:=nil;
  Exit(p);
 end;
end;

procedure delete_unr(uh:p_unrhdr; ptr:Pointer);
var
 up:p_unr;
begin
 Dec(uh^.alloc);
 up:=p_unr(ptr);
 TAILQ_INSERT_TAIL(@uh^.ppfree,up,@up^.list);
end;

procedure clean_unrhdrl(uh:p_unrhdr);
var
 up:p_unr;
begin
 mtx_assert(uh^.mtx^);

 up:=TAILQ_FIRST(@uh^.ppfree);
 while (up<>nil) do
 begin
  TAILQ_REMOVE(@uh^.ppfree,up,@up^.list);
  mtx_unlock(uh^.mtx^);
  FreeMem(up);
  mtx_lock(uh^.mtx^);
  up:=TAILQ_FIRST(@uh^.ppfree);
 end;
end;

procedure clean_unrhdr(uh:p_unrhdr);
begin
 mtx_lock(uh^.mtx^);
 clean_unrhdrl(uh);
 mtx_unlock(uh^.mtx^);
end;

function new_unrhdr(low,high:Integer; mutex:p_mtx):p_unrhdr;
var
 uh:p_unrhdr;
begin
 Assert((low>=0) and (low<=high),'UNR: use error: new_unrhdr');

 uh:=AllocMem(SizeOf(t_unrhdr));

 if (mutex<>nil) then
  uh^.mtx:=mutex
 else
  uh^.mtx:=@unitmtx;

 TAILQ_INIT(@uh^.head);
 TAILQ_INIT(@uh^.ppfree);

 uh^.low  :=DWORD(low);
 uh^.high :=DWORD(high);
 uh^.first:=0;
 uh^.last :=1+DWORD(high-low);

 check_unrhdr(uh,0);

 Result:=uh;
end;

procedure delete_unrhdr(uh:p_unrhdr);
begin
 check_unrhdr(uh,0);
 Assert(uh^.busy=0,'UNR memory leak: unrhdr has allocations');
 Assert(uh^.alloc=0,'UNR memory leak in delete_unrhdr');
 Assert(TAILQ_FIRST(@uh^.ppfree)=nil,'unrhdr has postponed item for free');
 FreeMem(uh);
end;

function is_bitmap(uh:p_unrhdr; up:p_unr):Boolean; inline;
begin
 Result:=(up^.ptr<>Pointer(uh)) and (up^.ptr<>nil);
end;

function optimize_unr(uh:p_unrhdr):Integer;
var
 up,uf,us:p_unr;
 ub,ubf:p_unrb;
 a,l,ba:DWORD;
begin
 us:=nil;
 ba:=0;

 uf:=TAILQ_FIRST(@uh^.head);
 while (uf<>nil) do
 begin
  if (uf^.len<DWORD(NBITS)) then
  begin
   a:=1;
   if is_bitmap(uh,uf) then Inc(a);
   l:=uf^.len;

   up:=uf;
   while True do
   begin
    up:=TAILQ_NEXT(up,@up^.list);
    if (up=nil) then break;
    if (up^.len+l>DWORD(NBITS)) then break;

    Inc(a);
    if is_bitmap(uh,up) then Inc(a);
    Inc(l,up^.len);
   end;

   if (a>ba) then
   begin
    ba:=a;
    us:=uf;
   end;
  end;

  uf:=TAILQ_NEXT(uf,@uf^.list);
 end;

 if (ba<3) then
 begin
  Exit(0);
 end;

 if (not is_bitmap(uh,us)) then
 begin
  uf:=TAILQ_NEXT(us,@us^.list);
  TAILQ_REMOVE(@uh^.head,us,@us^.list);

  a:=us^.len;
  if (us^.ptr=Pointer(uh)) then l:=1 else l:=0;

  ub:=p_unrb(Pointer(us));
  ub^.busy:=0;

  if (l<>0) then
  begin
   bit_nset(@ub^.map,0,a);
   Inc(ub^.busy,a);
  end else
  begin
   bit_nclear(@ub^.map,0,a);
  end;

  //uf is guaranteed non-NULL here because ba>=3 was found
  if (not is_bitmap(uh,uf)) then
  begin
   if (uf^.ptr=nil) then
    bit_nclear(@ub^.map,a,a+uf^.len-1)
   else
   begin
    bit_nset(@ub^.map,a,a+uf^.len-1);
    Inc(ub^.busy,uf^.len);
   end;
   uf^.ptr:=Pointer(ub);
   Inc(uf^.len,a);
   us:=uf;
  end else
  begin
   ubf:=p_unrb(Pointer(uf^.ptr));

   l:=0;
   while (l<uf^.len) do
   begin
    if bit_test(@ubf^.map,l) then
    begin
     bit_set(@ub^.map,a);
     Inc(ub^.busy);
    end else
    begin
     bit_clear(@ub^.map,a);
    end;
    Inc(l);
    Inc(a);
   end;

   uf^.len:=a;
   delete_unr(uh,uf^.ptr);
   uf^.ptr:=Pointer(ub);
   us:=uf;
  end;
 end;

 ub:=p_unrb(Pointer(us^.ptr));

 while True do
 begin
  uf:=TAILQ_NEXT(us,@us^.list);
  if (uf=nil) then
  begin
   Exit(1);
  end;

  if (uf^.len+us^.len>DWORD(NBITS)) then
  begin
   Exit(1);
  end;

  if (uf^.ptr=nil) then
  begin
   bit_nclear(@ub^.map,us^.len,us^.len+uf^.len-1);
   Inc(us^.len,uf^.len);
   TAILQ_REMOVE(@uh^.head,uf,@uf^.list);
   delete_unr(uh,uf);
  end else
  if (uf^.ptr=Pointer(uh)) then
  begin
   bit_nset(@ub^.map,us^.len,us^.len+uf^.len-1);
   Inc(ub^.busy,uf^.len);
   Inc(us^.len,uf^.len);
   TAILQ_REMOVE(@uh^.head,uf,@uf^.list);
   delete_unr(uh,uf);
  end else
  begin
   ubf:=p_unrb(Pointer(uf^.ptr));

   l:=0;
   while (l<uf^.len) do
   begin
    if bit_test(@ubf^.map,l) then
    begin
     bit_set(@ub^.map,us^.len);
     Inc(ub^.busy);
    end else
    begin
     bit_clear(@ub^.map,us^.len);
    end;
    Inc(l);
    Inc(us^.len);
   end;

   TAILQ_REMOVE(@uh^.head,uf,@uf^.list);
   delete_unr(uh,Pointer(ubf));
   delete_unr(uh,uf);
  end;
 end;
end;

procedure collapse_unr(uh:p_unrhdr; up0:p_unr);
var
 up,upp:p_unr;
 ub:p_unrb;
begin
 up:=up0;

 { If bitmap is all set or clear, change it to runlength }
 if is_bitmap(uh,up) then
 begin
  ub:=p_unrb(Pointer(up^.ptr));
  if (ub^.busy=up^.len) then
  begin
   delete_unr(uh,up^.ptr);
   up^.ptr:=Pointer(uh);
  end else
  if (ub^.busy=0) then
  begin
   delete_unr(uh,up^.ptr);
   up^.ptr:=nil;
  end;
 end;

 { If nothing left in runlength, delete it }
 if (up^.len=0) then
 begin
  upp:=TAILQ_PREV(up,@up^.list);
  if (upp=nil) then upp:=TAILQ_NEXT(up,@up^.list);
  TAILQ_REMOVE(@uh^.head,up,@up^.list);
  delete_unr(uh,up);
  up:=upp;
 end;

 { If we have a hot-spot still, merge with neighbor if possible }
 if (up<>nil) then
 begin
  upp:=TAILQ_PREV(up,@up^.list);
  if (upp<>nil) and (up^.ptr=upp^.ptr) then
  begin
   Inc(up^.len,upp^.len);
   TAILQ_REMOVE(@uh^.head,upp,@upp^.list);
   delete_unr(uh,upp);
  end;

  upp:=TAILQ_NEXT(up,@up^.list);
  if (upp<>nil) and (up^.ptr=upp^.ptr) then
  begin
   Inc(up^.len,upp^.len);
   TAILQ_REMOVE(@uh^.head,upp,@upp^.list);
   delete_unr(uh,upp);
  end;
 end;

 { Merge into ->first if possible }
 upp:=TAILQ_FIRST(@uh^.head);
 if (upp<>nil) and (upp^.ptr=Pointer(uh)) then
 begin
  Inc(uh^.first,upp^.len);
  TAILQ_REMOVE(@uh^.head,upp,@upp^.list);
  delete_unr(uh,upp);
  if (up=upp) then up:=nil;
 end;

 { Merge into ->last if possible }
 upp:=TAILQ_LAST(@uh^.head);
 if (upp<>nil) and (upp^.ptr=nil) then
 begin
  Inc(uh^.last,upp^.len);
  TAILQ_REMOVE(@uh^.head,upp,@upp^.list);
  delete_unr(uh,upp);
  if (up=upp) then up:=nil;
 end;

 { Try to make bitmaps }
 while (optimize_unr(uh)<>0) do ;
end;

function alloc_unrl(uh:p_unrhdr):Integer;
var
 up:p_unr;
 ub:p_unrb;
 x:DWORD;
 y:Integer;
begin
 mtx_assert(uh^.mtx^);
 check_unrhdr(uh,0);

 x:=uh^.low+uh^.first;

 up:=TAILQ_FIRST(@uh^.head);

 {
  * If we have an ideal split, just adjust the first+last
  }
 if (up=nil) and (uh^.last>0) then
 begin
  Inc(uh^.first);
  Dec(uh^.last);
  Inc(uh^.busy);
  Result:=Integer(x);
  Exit;
 end;

 {
  * We can always allocate from the first list element, so if we have
  * nothing on the list, we must have run out of unit numbers.
  }
 if (up=nil) then
 begin
  Result:=-1;
  Exit;
 end;

 Assert(up^.ptr<>Pointer(uh),'UNR first element is allocated');

 if (up^.ptr=nil) then
 begin { free run }
  Inc(uh^.first);
  Dec(up^.len);
 end else
 begin { bitmap }
  ub:=p_unrb(Pointer(up^.ptr));
  Assert(ub^.busy<up^.len,'UNR bitmap confusion');

  bit_ffc(@ub^.map,up^.len,y);
  Assert(y<>-1,'UNR corruption: No clear bit in bitmap.');

  bit_set(@ub^.map,DWORD(y));
  Inc(ub^.busy);
  Inc(x,DWORD(y));
 end;

 Inc(uh^.busy);
 collapse_unr(uh,up);

 Result:=Integer(x);
end;

function alloc_unr(uh:p_unrhdr):Integer;
var
 i:Integer;
begin
 mtx_lock(uh^.mtx^);
 i:=alloc_unrl(uh);
 clean_unrhdrl(uh);
 mtx_unlock(uh^.mtx^);
 Result:=i;
end;

function alloc_unr_specificl(uh:p_unrhdr; item:DWORD; p1,pp2:PPointer):Integer;
label
 done;
var
 up,upn:p_unr;
 ub:p_unrb;
 i,last,tl:DWORD;
begin
 mtx_assert(uh^.mtx^);

 if (item<uh^.low+uh^.first) or (item>uh^.high) then
 begin
  Result:=-1;
  Exit;
 end;

 up:=TAILQ_FIRST(@uh^.head);

 { Ideal split. }
 if (up=nil) and (item-uh^.low=uh^.first) then
 begin
  Inc(uh^.first);
  Dec(uh^.last);
  Inc(uh^.busy);
  check_unrhdr(uh,0);
  Result:=Integer(item);
  Exit;
 end;

 i:=item-uh^.low-uh^.first;

 if (up=nil) then
 begin
  up:=p_unr(new_unr(uh,p1,pp2));
  up^.ptr:=nil;
  up^.len:=i;
  TAILQ_INSERT_TAIL(@uh^.head,up,@up^.list);

  up:=p_unr(new_unr(uh,p1,pp2));
  up^.ptr:=Pointer(uh);
  up^.len:=1;
  TAILQ_INSERT_TAIL(@uh^.head,up,@up^.list);

  uh^.last:=uh^.high-uh^.low-i;
  Inc(uh^.busy);
  check_unrhdr(uh,0);
  Result:=Integer(item);
  Exit;
 end else
 begin
  up:=TAILQ_FIRST(@uh^.head);
  while (up<>nil) do
  begin
   if (up^.len>i) then break;
   Dec(i,up^.len);
   up:=TAILQ_NEXT(up,@up^.list);
  end;
 end;

 if (up=nil) then
 begin
  if (i>0) then
  begin
   up:=p_unr(new_unr(uh,p1,pp2));
   up^.ptr:=nil;
   up^.len:=i;
   TAILQ_INSERT_TAIL(@uh^.head,up,@up^.list);
  end;

  up:=p_unr(new_unr(uh,p1,pp2));
  up^.ptr:=Pointer(uh);
  up^.len:=1;
  TAILQ_INSERT_TAIL(@uh^.head,up,@up^.list);
  goto done;
 end;

 if is_bitmap(uh,up) then
 begin
  ub:=p_unrb(Pointer(up^.ptr));
  if bit_test(@ub^.map,i) then
  begin
   Result:=-1;
   Exit;
  end;
  bit_set(@ub^.map,i);
  Inc(ub^.busy);
  goto done;
 end;

 if (up^.ptr=Pointer(uh)) then
 begin
  Result:=-1;
  Exit;
 end;

 Assert(up^.ptr=nil,'alloc_unr_specificl: up->ptr != NULL');

 tl:=up^.len-(1+i);
 if (tl>0) then
 begin
  upn:=p_unr(new_unr(uh,p1,pp2));
  upn^.ptr:=nil;
  upn^.len:=tl;
  TAILQ_INSERT_AFTER(@uh^.head,up,upn,@upn^.list);
 end;

 if (i>0) then
 begin
  upn:=p_unr(new_unr(uh,p1,pp2));
  upn^.len:=i;
  upn^.ptr:=nil;
  TAILQ_INSERT_BEFORE(up,upn,@upn^.list);
 end;

 up^.len:=1;
 up^.ptr:=Pointer(uh);

done:
 last:=uh^.high-uh^.low-(item-uh^.low);
 if (uh^.last>last) then uh^.last:=last;

 Inc(uh^.busy);
 collapse_unr(uh,up);
 check_unrhdr(uh,0);

 Result:=Integer(item);
end;

function alloc_unr_specific(uh:p_unrhdr; item:DWORD):Integer;
var
 p1,pp2:Pointer;
 i:Integer;
begin
 p1 :=AllocMem(SizeOf(t_unr));
 pp2:=AllocMem(SizeOf(t_unr));

 mtx_lock(uh^.mtx^);
 i:=alloc_unr_specificl(uh,item,@p1,@pp2);
 mtx_unlock(uh^.mtx^);

 if (p1<>nil) then FreeMem(p1);
 if (pp2<>nil) then FreeMem(pp2);

 Result:=i;
end;

procedure free_unrl(uh:p_unrhdr; item:DWORD; p1,pp2:PPointer);
var
 up,upp,upn:p_unr;
 ub:p_unrb;
 pl:DWORD;
begin
 Assert((item>=uh^.low) and (item<=uh^.high),'UNR: free_unr out of range');
 check_unrhdr(uh,0);

 item:=item-uh^.low;

 upp:=TAILQ_FIRST(@uh^.head);

 if (item+1=uh^.first) and (upp=nil) then
 begin
  Inc(uh^.last);
  Dec(uh^.first);
  Dec(uh^.busy);
  check_unrhdr(uh,0);
  Exit;
 end;

 if (item<uh^.first) then
 begin
  up:=p_unr(new_unr(uh,p1,pp2));
  up^.ptr:=Pointer(uh);
  up^.len:=uh^.first-item;
  TAILQ_INSERT_HEAD(@uh^.head,up,@up^.list);
  Dec(uh^.first,up^.len);
 end;

 Dec(item,uh^.first);

 up:=TAILQ_FIRST(@uh^.head);
 while (up<>nil) do
 begin
  if (up^.len>item) then break;
  Dec(item,up^.len);
  up:=TAILQ_NEXT(up,@up^.list);
 end;

 { Handle bitmap items }
 if is_bitmap(uh,up) then
 begin
  ub:=p_unrb(Pointer(up^.ptr));

  Assert(bit_test(@ub^.map,item)<>False,'UNR: Freeing free item (bitmap)');
  bit_clear(@ub^.map,item);
  Dec(uh^.busy);
  Dec(ub^.busy);
  collapse_unr(uh,up);
  Exit;
 end;

 Assert(up^.ptr=Pointer(uh),'UNR: Freeing free item (run)');

 { Just this one left, reap it }
 if (up^.len=1) then
 begin
  up^.ptr:=nil;
  Dec(uh^.busy);
  collapse_unr(uh,up);
  Exit;
 end;

 { Shift item into the previous free run }
 upp:=TAILQ_PREV(up,@up^.list);
 if (item=0) and (upp<>nil) and (upp^.ptr=nil) then
 begin
  Inc(upp^.len);
  Dec(up^.len);
  Dec(uh^.busy);
  collapse_unr(uh,up);
  Exit;
 end;

 { Shift item to the next free run }
 upn:=TAILQ_NEXT(up,@up^.list);
 if (item=up^.len-1) and (upn<>nil) and (upn^.ptr=nil) then
 begin
  Inc(upn^.len);
  Dec(up^.len);
  Dec(uh^.busy);
  collapse_unr(uh,up);
  Exit;
 end;

 { Split off the tail end, if any. }
 pl:=up^.len-(1+item);
 if (pl>0) then
 begin
  upp:=p_unr(new_unr(uh,p1,pp2));
  upp^.ptr:=Pointer(uh);
  upp^.len:=pl;
  TAILQ_INSERT_AFTER(@uh^.head,up,upp,@upp^.list);
 end;

 { Split off head end, if any }
 if (item>0) then
 begin
  upp:=p_unr(new_unr(uh,p1,pp2));
  upp^.len:=item;
  upp^.ptr:=Pointer(uh);
  TAILQ_INSERT_BEFORE(up,upp,@upp^.list);
 end;

 up^.len:=1;
 up^.ptr:=nil;
 Dec(uh^.busy);
 collapse_unr(uh,up);
end;

procedure free_unr(uh:p_unrhdr; item:DWORD);
var
 p1,pp2:Pointer;
begin
 p1 :=AllocMem(SizeOf(t_unr));
 pp2:=AllocMem(SizeOf(t_unr));

 mtx_lock(uh^.mtx^);
 free_unrl(uh,item,@p1,@pp2);
 clean_unrhdrl(uh);
 mtx_unlock(uh^.mtx^);

 if (p1<>nil) then FreeMem(p1);
 if (pp2<>nil) then FreeMem(pp2);
end;

initialization
 mtx_init(unitmtx,'unit# allocation');

end.
