unit mm_adr_virtual;

{$mode ObjFPC}{$H+}

interface

uses
  Windows,
  Classes,
  SysUtils,
  g23tree,
  bittype,
  sys_types;

{
 alloc/free node:
 [
  offset 12..39:28
  size   12..39:28
  free    0..0 :1
  reserv  0..0 :1
  prot    0..5 :6

  addr   12..39:28  ->[direct addr]
  direct  0..0 :1
  stack   0..0 :1
  polled  0..0 :1
  mapped  0..0 :1

  block  Pointer    ->[alloc bloc]
 ]

 alloc block:
 [
  offset 12..39:28
  size   12..39:28
  btype    0..7:8  = free/private/mapped/gpu

  used   12..39:28
 ]
}

const
 BT_FREE=0;
 BT_PRIV=1;
 BT_GPUM=2;
 BT_FMAP=3;

type
 PVirtualAdrNode=^TVirtualAdrNode;

 PVirtualAdrBlock=^TVirtualAdrBlock;
 TVirtualAdrBlock=packed object
  private
   Function  GetOffset:Pointer;
   Procedure SetOffset(q:Pointer);
   Function  GetSize:QWORD;
   Procedure SetSize(q:QWORD);
   Function  GetUsed:QWORD;
   Procedure SetUsed(q:QWORD);
   Function  GetRsrv:QWORD;
   Procedure SetRsrv(q:QWORD);
  public
   F:bitpacked record
    Offset:bit28;
    Size  :bit28;
    btype :bit8;
    rsrv  :DWORD;
    used  :DWORD;
   end;
   Handle:Pointer; //gpu
  property  Offset:Pointer read GetOffset write SetOffset;
  property  Size:QWORD     read GetSize   write SetSize;
  property  Used:QWORD     read GetUsed   write SetUsed;
  property  Rsrv:QWORD     read GetRsrv   write SetRsrv;
  function  Commit(key:PVirtualAdrNode;prot:Integer):Integer;
  function  Free(key:PVirtualAdrNode):Integer;
  function  Reserved(key:PVirtualAdrNode):Integer;
  function  Protect(key:PVirtualAdrNode;prot:Integer):Integer;
 end;

 TVirtualAdrNode=packed object
  private
   //free:  [Size]  |[Offset]
   //alloc: [Offset]
   Function  GetOffset:Pointer;
   Procedure SetOffset(q:Pointer);
   Function  GetSize:QWORD;
   Procedure SetSize(q:QWORD);
   Function  GetAddr:QWORD;
   Procedure SetAddr(p:QWORD);
   Function  GetIsFree:Boolean;
   Procedure SetIsFree(b:Boolean);
  public
   F:bitpacked record
    Offset:bit28;
    Size  :bit28;
    Free  :bit1;
    reserv:bit1;
    prot  :bit6;
    addr  :bit28;
    direct:bit1;
    stack :bit1;
    polled:bit1;
    mapped:bit1;
   end;
   block:PVirtualAdrBlock;
   property Offset:Pointer read GetOffset write SetOffset;
   property Size:QWORD     read GetSize   write SetSize;
   property addr:QWORD     read GetAddr   write SetAddr;
   property IsFree:Boolean read GetIsFree write SetIsFree;
   Function cmp_merge(const n:TVirtualAdrNode):Boolean;
 end;

type
 TVirtualAdrFreeCompare=object
  function c(const a,b:TVirtualAdrNode):Integer; static;
 end;
 TVirtualAdrAllcCompare=object
  function c(const a,b:TVirtualAdrNode):Integer; static;
 end;

 TDirectUnmapCb=function(Offset,Size:QWORD):Integer;
 TDirectMtypeCb=function(Offset,Size:QWORD;mtype:Integer):Integer;

 TBlockCb=function(block:PVirtualAdrBlock):Integer;

 TVirtualManager=class
  private
   type
    TFreePoolNodeSet=specialize T23treeSet<TVirtualAdrNode,TVirtualAdrFreeCompare>;
    TAllcPoolNodeSet=specialize T23treeSet<TVirtualAdrNode,TVirtualAdrAllcCompare>;

   var
    Flo,Fhi:Pointer;
    FMaxSize:QWORD;

    FFreeSet:TFreePoolNodeSet;
    FAllcSet:TAllcPoolNodeSet;
  public
    property lo:Pointer read Flo;
    property hi:Pointer read Fhi;

    Constructor Create(_lo,_hi:QWORD);
  private
    procedure _Insert(const key:TVirtualAdrNode);
    procedure _Delete(const key:TVirtualAdrNode);
    Function  _FetchNode_m(mode:Byte;cmp:Pointer;var R:TVirtualAdrNode):Boolean;
    Function  _Find_m(mode:Byte;var R:TVirtualAdrNode):Boolean;

    procedure _Merge(key:TVirtualAdrNode);
    procedure _Devide(Offset:Pointer;Size:QWORD;var key:TVirtualAdrNode);
    function  _UnmapDirect(Offset,Size:QWORD):Integer;
    function  _MtypeDirect(Offset,Size:QWORD;mtype:Integer):Integer;
    function  _CreateBlock(block:PVirtualAdrBlock):Integer;
    function  _FreeBlock(block:PVirtualAdrBlock):Integer;
    Function  _FindFreeOffset(ss:Pointer;Size,Align:QWORD;prot:Byte;var AdrOut:Pointer):Integer;
    procedure _set_block(Offset:Pointer;Size:QWORD;block:PVirtualAdrBlock);
    procedure _mmap_addr(Offset:Pointer;Size,addr:QWORD;direct:Boolean);
  public
    var
     OnDirectUnmapCb:TDirectUnmapCb;
     OnDirectMtypeCb:TDirectMtypeCb;

     OnCreateBlockCb:TBlockCb;
     OnFreeBlockCb  :TBlockCb;

     stat:record
      flex:QWORD;
      direct:QWORD;
      cpu:QWORD;
      gpu:QWORD;
     end;

    procedure _mmap_sys(Offset:Pointer;Size:QWORD);

    Function  check_fixed(Offset:Pointer;Size:QWORD;flags:Byte;fd:Integer):Integer;
    Function  mmap(Offset:Pointer;Size,Align:QWORD;prot,flags:Byte;fd:Integer;addr:QWORD;var AdrOut:Pointer):Integer;

    Function  Protect(Offset:Pointer;Size:QWORD;prot:Integer):Integer;
    Function  Mtypeprotect(Offset:Pointer;Size:QWORD;mtype,prot:Integer):Integer;

    Function  Release(Offset:Pointer;Size:QWORD):Integer;

    Function  Query(Offset:Pointer;next:Boolean;var ROut:TVirtualAdrNode):Integer;
    Function  QueryProt(Offset:Pointer;var ROut:TVirtualAdrNode):Integer;

    Function  TryGetMapBlockByAddr(Offset:Pointer;var block:PVirtualAdrBlock):Boolean;

    procedure Print;
 end;

implementation

uses
 mmap;

const
 ENOMEM=12;
 EACCES=13;
 EINVAL=22;
 ENOSYS=78;

//

function NewAdrBlock(Offset:Pointer;Size:QWORD;prot:Integer;btype:Byte;fd:Integer;offst:size_t):PVirtualAdrBlock;
var
 FShift :QWORD;
 FOffset:Pointer;
 FSize  :QWORD;
 ASize  :QWORD;
 err    :Integer;
begin
 Result:=nil;

 FOffset:=AlignDw(Offset,GRANULAR_PAGE_SIZE);
 FShift :=Offset-FOffset;
 FSize  :=FShift+Size;
 ASize  :=AlignUp(FSize,GRANULAR_PAGE_SIZE);

 case btype of
  BT_PRIV,
  BT_GPUM:
   begin
    //err:=_VirtualReserve(Pointer(FOffset),ASize,prot);
    err:=_VirtualAlloc(Pointer(FOffset),ASize,0);
    if (err<>0) then
    begin
     Writeln(StdErr,'_VirtualReserve(',HexStr(FOffset),',',HexStr(ASize,16),'):',err);
     Exit;
    end;
   end;
  BT_FMAP:
   begin
    if (offst<FShift) then
    begin
     Writeln(StdErr,'offst<FShift:',offst,'<',FShift);
     Exit;
    end;
    err:=_VirtualMmap(Pointer(FOffset),FSize,prot,fd,(offst-FShift));
    if (err<>0) then
    begin
     Writeln(StdErr,'_VirtualMmap(',HexStr(FOffset),',',HexStr(FSize,16),',',fd,',',(offst-FShift),'):',err);
     Exit;
    end;
   end;
  else
       Exit;
 end;

 Result:=AllocMem(SizeOf(TVirtualAdrBlock));
 if (Result=nil) then Exit;

 Result^.F.btype :=btype;
 Result^.Offset  :=FOffset;
 Result^.Size    :=ASize;

 case btype of
  BT_PRIV,
  BT_GPUM:
   begin
    Result^.Rsrv:=ASize;
   end;
  BT_FMAP:
   begin
    Result^.Used:=ASize;
   end;
  else;
 end;

end;

//

function TVirtualAdrFreeCompare.c(const a,b:TVirtualAdrNode):Integer;
begin
 //1 FSize
 Result:=Integer(a.F.Size>b.F.Size)-Integer(a.F.Size<b.F.Size);
 if (Result<>0) then Exit;
 //2 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

function TVirtualAdrAllcCompare.c(const a,b:TVirtualAdrNode):Integer;
begin
 //1 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

//

function ia(b:Boolean;addr,Size:qword):qword; inline;
begin
 if b then
 begin
  Result:=addr+Size;
 end else
 begin
  Result:=0;
 end;
end;

function ia(var k:TVirtualAdrNode;addr,Size:qword):qword; inline;
begin
 Result:=ia((k.F.direct<>0) or (k.F.mapped<>0),addr,Size);
end;

function Max(a,b:Pointer):Pointer; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function Max(a,b:QWORD):QWORD; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

//

Function TVirtualAdrBlock.GetOffset:Pointer;
begin
 Result:=Pointer(QWORD(F.Offset) shl 12);
end;

Procedure TVirtualAdrBlock.SetOffset(q:Pointer);
begin
 F.Offset:=DWORD(QWORD(q) shr 12);
 Assert(GetOffset=q);
end;

Function TVirtualAdrBlock.GetSize:QWORD;
begin
 Result:=QWORD(F.Size) shl 12;
end;

Procedure TVirtualAdrBlock.SetSize(q:QWORD);
begin
 F.Size:=DWORD(q shr 12);
 Assert(GetSize=q);
end;

Function TVirtualAdrBlock.GetUsed:QWORD;
begin
 Result:=QWORD(F.used) shl 12;
end;

Procedure TVirtualAdrBlock.SetUsed(q:QWORD);
begin
 F.used:=DWORD(q shr 12);
 Assert(GetUsed=q);
end;

Function TVirtualAdrBlock.GetRsrv:QWORD;
begin
 Result:=QWORD(F.rsrv) shl 12;
end;

Procedure TVirtualAdrBlock.SetRsrv(q:QWORD);
begin
 F.rsrv:=DWORD(q shr 12);
 Assert(GetRsrv=q);
end;

function TVirtualAdrBlock.Commit(key:PVirtualAdrNode;prot:Integer):Integer;
begin
 Result:=0;
 if (key=nil) then Exit;

 Assert(key^.Offset           >= Offset);
 Assert(key^.Offset+key^.Size <= Offset+Size);

 if (key^.F.Free<>0) then //free->commit
 begin
  Assert((Used+key^.Size)<=Size);

  Used:=Used+key^.Size; //+
 end else
 if (key^.F.reserv<>0) then //reserved->commit
 begin
  Assert(Rsrv>=key^.Size);
  Assert((Used+key^.Size)<=Size);

  Rsrv:=Rsrv-key^.Size; //-
  Used:=Used+key^.Size; //+
 end else
 begin
  Exit;
 end;

 case F.btype of
  BT_PRIV,
  BT_GPUM:
   begin
    Result:=_VirtualProtect(Pointer(key^.Offset),key^.Size,prot);
    //Result:=_VirtualCommit(Pointer(key^.Offset),key^.Size,prot);
    if (Result=0) then
    begin
     key^.F.prot:=prot;
    end;
   end;
  else;
 end;

end;

function TVirtualAdrBlock.Free(key:PVirtualAdrNode):Integer;
begin
 Result:=0;
 if (key=nil) then Exit;
 if (key^.F.Free<>0) then Exit; //its free

 Assert(key^.Offset           >= Offset);
 Assert(key^.Offset+key^.Size <= Offset+Size);

 if (key^.F.reserv<>0) then //reserved->free
 begin
  Assert(Rsrv>=key^.Size);

  Rsrv:=Rsrv-key^.Size;    //-
 end else
 begin                     //commit->free
  Assert(Used>=key^.Size);

  Used:=Used-key^.Size; //-
 end;

 if not _iswrite(key^.F.prot) then
 begin
  _VirtualProtect(Pointer(key^.Offset),key^.Size,PROT_READ or PROT_WRITE);
  FillChar(key^.Offset^,key^.Size,0);
 end;

 Result:=_VirtualProtect(Pointer(key^.Offset),key^.Size,0);
 //Result:=_VirtualDecommit(Pointer(key^.Offset),key^.Size);
end;

function TVirtualAdrBlock.Reserved(key:PVirtualAdrNode):Integer;
begin
 Result:=0;
 if (key=nil) then Exit;
 if (key^.F.reserv<>0) then Exit; //its reserved

 Assert(key^.Offset           >= Offset);
 Assert(key^.Offset+key^.Size <= Offset+Size);

 if (key^.F.Free<>0) then //free->reserved
 begin
  Assert((Rsrv+key^.Size)<=Size);

  Rsrv:=Rsrv+key^.Size; //+
 end else
 begin                    //commit->reserved
  Assert(Used>=key^.Size);
  Assert((Rsrv+key^.Size)<=Size);

  Used:=Used-key^.Size; //-
  Rsrv:=Rsrv+key^.Size; //+
 end;

 if not _iswrite(key^.F.prot) then
 begin
  _VirtualProtect(Pointer(key^.Offset),key^.Size,PROT_READ or PROT_WRITE);
  FillChar(key^.Offset^,key^.Size,0);
 end;

 Result:=_VirtualProtect(Pointer(key^.Offset),key^.Size,0);

 //Result:=_VirtualDecommit(Pointer(key^.Offset),key^.Size);
 Result:=0;
end;

function TVirtualAdrBlock.Protect(key:PVirtualAdrNode;prot:Integer):Integer;
begin
 Result:=0;
 if (key=nil) then Exit;
 if (key^.F.Free<>0) then Exit; //its free

 Assert(key^.Offset           >= Offset);
 Assert(key^.Offset+key^.Size <= Offset+Size);

 if (key^.F.prot<>prot) then
 begin
  Result:=_VirtualProtect(Pointer(key^.Offset),key^.Size,prot);
  if (Result=0) then
  begin
   key^.F.prot:=prot;
  end;
 end;
end;

//

Function TVirtualAdrNode.GetOffset:Pointer;
begin
 Result:=Pointer(QWORD(F.Offset) shl 12);
end;

Procedure TVirtualAdrNode.SetOffset(q:Pointer);
begin
 F.Offset:=DWORD(QWORD(q) shr 12);
 Assert(GetOffset=q);
end;

Function TVirtualAdrNode.GetSize:QWORD;
begin
 Result:=QWORD(F.Size) shl 12;
end;

Procedure TVirtualAdrNode.SetSize(q:QWORD);
begin
 F.Size:=DWORD(q shr 12);
 Assert(GetSize=q);
end;

Function TVirtualAdrNode.GetAddr:QWORD;
begin
 Result:=QWORD(F.addr) shl 12;
end;

Procedure TVirtualAdrNode.SetAddr(p:QWORD);
begin
 F.addr:=DWORD(QWORD(p) shr 12);
 Assert(GetAddr=p);
end;

Function TVirtualAdrNode.GetIsFree:Boolean;
begin
 Result:=Boolean(F.Free);
end;

Procedure TVirtualAdrNode.SetIsFree(b:Boolean);
begin
 F.Free:=Byte(b);
end;

Function TVirtualAdrNode.cmp_merge(const n:TVirtualAdrNode):Boolean;
begin
 Result:=False;
 if (F.prot  <>n.F.prot  ) then Exit;
 if (F.reserv<>n.F.reserv) then Exit;
 if (F.direct<>n.F.direct) then Exit;
 if (F.stack <>n.F.stack ) then Exit;
 if (F.polled<>n.F.polled) then Exit;
 if (F.mapped<>n.F.mapped) then Exit;
 if (block   <>n.block   ) then Exit;
 Result:=True;
end;

///

Constructor TVirtualManager.Create(_lo,_hi:QWORD);
var
 key:TVirtualAdrNode;
begin
 Assert(_lo<_hi);

 Flo:=Pointer(_lo);
 Fhi:=Pointer(_hi);

 FMaxSize:=(_hi-_lo+1);

 key:=Default(TVirtualAdrNode);
 key.IsFree:=True;
 key.Offset:=Pointer(_lo);
 key.Size  :=FMaxSize;

 _Insert(key);
end;

procedure TVirtualManager._Insert(const key:TVirtualAdrNode);
begin
 Assert(key.Size<>0);

 if (key.F.direct<>0) then
 begin
  Assert(key.addr<$17FFFFFFF);
 end;

 if key.IsFree then
 begin
  if (key.F.mapped=0) then
  begin
   FFreeSet.Insert(key);
  end;
 end else
 if (key.block<>nil) then //not system
 begin
  if (key.F.direct=0) then //is flex
  begin
   Inc(stat.flex,key.Size);
  end else  //is direct
  begin
   Inc(stat.direct,key.Size);
  end;

  if _isgpu(key.F.prot) then
  begin
   Inc(stat.gpu,key.Size);
  end else
  begin
   Inc(stat.cpu,key.Size);
  end;
 end;
 FAllcSet.Insert(key);
end;

procedure TVirtualManager._Delete(const key:TVirtualAdrNode);
var
 It:TAllcPoolNodeSet.Iterator;
 rkey:TVirtualAdrNode;
begin
 It:=FAllcSet.find(key);
 if (It.Item<>nil) then
 begin
  rkey:=It.Item^;

  if not rkey.IsFree then
  if (rkey.block<>nil) then //not system
  begin
   if (rkey.F.direct=0) then //is flex
   begin
    Dec(stat.flex,rkey.Size);
   end else  //is direct
   begin
    Dec(stat.direct,rkey.Size);
   end;

   if _isgpu(rkey.F.prot) then
   begin
    Dec(stat.gpu,rkey.Size);
   end else
   begin
    Dec(stat.cpu,rkey.Size);
   end;
  end;
 end;

 FAllcSet.delete(key);
 FFreeSet.delete(key);
end;

const
 M_LE=0;
 M_BE=1;

 C_FR=2;

 C_UP=4;
 C_DW=8;

 C_LE=12;
 C_BE=16;

Function TVirtualManager._FetchNode_m(mode:Byte;cmp:Pointer;var R:TVirtualAdrNode):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
 key,rkey:TVirtualAdrNode;
begin
 Result:=false;

 key:=R;

 Case (mode and 1) of
  M_LE:It:=FAllcSet.find_le(key);
  M_BE:It:=FAllcSet.find_be(key);
  else
       Exit;
 end;

 if (It.Item=nil) then Exit;

 rkey:=It.Item^;

 if ((mode and C_FR)=C_FR) then
 begin
  if (rkey.IsFree<>key.IsFree) then Exit;
 end;

 Case (mode and (not 3)) of
  C_UP:
       begin
        if not rkey.cmp_merge(key)                  then Exit;
        if (ia(rkey,rkey.addr,rkey.Size)<>key.addr) then Exit;
        if ((rkey.Offset+rkey.Size)<>cmp)           then Exit;
       end;
  C_DW:
       begin
        if not rkey.cmp_merge(key)  then Exit;
        if (rkey.addr   <>key.addr) then Exit;
        if (rkey.Offset <>cmp)      then Exit;
       end;

  C_LE:if ((rkey.Offset+rkey.Size)<=cmp) then Exit;
  C_BE:if (rkey.Offset<=cmp) then Exit;

  else
       Exit;
 end;

 R:=rkey;
 _Delete(rkey);
 Result:=True;
end;

Function TVirtualManager._Find_m(mode:Byte;var R:TVirtualAdrNode):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
begin
 Result:=false;

 Case mode of
  M_LE:It:=FAllcSet.find_le(R);
  M_BE:It:=FAllcSet.find_be(R);
  else
       Exit;
 end;

 if (It.Item=nil) then Exit;
 R:=It.Item^;
 Result:=True;
end;

//

procedure TVirtualManager._Merge(key:TVirtualAdrNode);
var
 rkey:TVirtualAdrNode;
begin
 //prev union
 repeat
  rkey:=key;
  rkey.F.Offset:=rkey.F.Offset-1; //hack
  rkey.addr    :=key.addr;        //find addr

  if not _FetchNode_m(M_LE or C_FR or C_UP,key.Offset,rkey) then Break;

  key.F.Size  :=key.F.Size+(key.F.Offset-rkey.F.Offset); //hack
  key.F.Offset:=rkey.F.Offset;                           //hack
  key.addr    :=rkey.addr;                               //prev addr
 until false;

 //next union
 repeat
  rkey:=key;
  rkey.F.Offset:=rkey.F.Offset+rkey.F.Size; //hack
  rkey.addr    :=ia(key,key.addr,key.Size); //find addr

  if not _FetchNode_m(M_BE or C_FR or C_DW,(key.Offset+key.Size),rkey) then Break;

  key.F.Size  :=key.F.Size+rkey.F.Size; //hack
 until false;

 _Insert(key);
end;

procedure TVirtualManager._Devide(Offset:Pointer;Size:QWORD;var key:TVirtualAdrNode);
var
 FOffset:Pointer;
 FSize:QWORD;
 Faddr:QWORD;
 FEndN,FEndO:Pointer;
begin
 FOffset:=key.Offset;
 FSize  :=key.Size;
 Faddr  :=key.addr;

 FEndN:=Offset +Size;
 FEndO:=FOffset+FSize;

 if (Offset>FOffset) then //prev save
 begin
  key.Size:=Offset-FOffset;
  _Merge(key);
 end;

 if (FEndO>FEndN) then //next save
 begin
  key.Offset:=FEndN;
  key.Size  :=FEndO-FEndN;
  key.addr  :=ia(key,Faddr,(FEndN-FOffset));

  _Merge(key);
 end else
 if (FEndO<>FEndN) then //tunc size
 begin
  Size:=FEndO-Offset;
 end;

 //new save
 key.Offset :=Offset;
 key.Size   :=Size;
 key.addr   :=ia(key,Faddr,(Offset-FOffset));
end;

function TVirtualManager._UnmapDirect(Offset,Size:QWORD):Integer;
begin
 if (Size=0) then Exit(0);
 if (OnDirectUnmapCb=nil) then Exit(EINVAL);
 Result:=OnDirectUnmapCb(Offset,Size);
end;

function TVirtualManager._MtypeDirect(Offset,Size:QWORD;mtype:Integer):Integer;
begin
 if (Size=0) then Exit(0);
 if (OnDirectMtypeCb=nil) then Exit(EINVAL);
 Result:=OnDirectMtypeCb(Offset,Size,mtype);
end;

function TVirtualManager._CreateBlock(block:PVirtualAdrBlock):Integer;
begin
 if (OnCreateBlockCb=nil) then Exit(0);
 Result:=OnCreateBlockCb(block);
end;

function TVirtualManager._FreeBlock(block:PVirtualAdrBlock):Integer;
begin
 if (OnFreeBlockCb=nil) then Exit(0);
 Result:=OnFreeBlockCb(block);
end;

Function TVirtualManager._FindFreeOffset(ss:Pointer;Size,Align:QWORD;prot:Byte;var AdrOut:Pointer):Integer;
var
 It:TFreePoolNodeSet.Iterator;
 key:TVirtualAdrNode;
 Offset:Pointer;

 galign:QWORD;

 err:Integer;

 _qaddr:Pointer;
 _qsize:QWORD;
 _qflag:Integer;

label
 _start;

begin
 Result:=0;

 galign:=Max(Align,GRANULAR_PAGE_SIZE);

 _qaddr:=nil;
 _qsize:=0;
 _qflag:=0;

 _start:

 key:=Default(TVirtualAdrNode);
 key.Offset:=ss;
 key.Size  :=Size;

 It:=FFreeSet.find_be(key);
 if (It.Item=nil) then Exit;
 repeat
  key:=It.Item^;
  if (key.F.mapped=0) then
  begin

   if (key.block=nil) then
   begin
    Offset:=AlignUp(Max(key.Offset,ss),galign);
   end else
   begin
    Offset:=AlignUp(Max(key.Offset,ss),Align);
   end;

   if (Offset+Size)<=(key.Offset+key.Size) then
   begin

    if (key.block=nil) then
    begin
     err:=_VirtualQuery(Offset,@_qaddr,@_qsize,nil,@_qflag);
     if (err=0) then
     begin
      if ((_qflag and (MAP_FIXED or MAP_VOID))<>0) then //commit or reserved
      begin
       _mmap_sys(_qaddr,_qsize);
       ss:=(Offset+Size);
       Goto _start;
      end else
      if (_qsize<Size) then //not fit
      begin
       ss:=(Offset+Size);
       Goto _start;
      end;
     end else
     begin
      Assert(false,IntToStr(err));
     end;
    end else
    begin

     if _isgpu(prot) then
     begin
      if (key.block^.F.btype<>BT_GPUM) then
      begin
       Goto _start;
      end;
     end else
     begin
      if (key.block^.F.btype<>BT_PRIV) then
      begin
       Goto _start;
      end;
     end;

    end;

    AdrOut:=Offset;
    Exit;
   end;
  end;
 until not It.Next;
 Result:=ENOMEM;
end;

procedure TVirtualManager._set_block(Offset:Pointer;Size:QWORD;block:PVirtualAdrBlock);
var
 key:TVirtualAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_BE,(Offset+Size),key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end;
 end;

 function _map:Boolean;
 begin
  Result:=False;
  Assert(key.Size<>0);

  //new save
  key.block:=block;

  if (block=nil) then
  begin
   key.IsFree  :=True;
   key.F.prot  :=0;
   key.F.addr  :=0; //hack
   key.F.reserv:=0;
   key.F.direct:=0;
   key.F.stack :=0;
   key.F.polled:=0;
   key.F.mapped:=0;
  end else
  begin
   if (block^.F.btype=BT_FMAP) then
   begin
    key.F.mapped:=1;
   end;
  end;

  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin

 repeat

  key:=Default(TVirtualAdrNode);
  key.Offset:=Offset;

  if _fetch then
  begin
   if _map then Break;
  end else
  begin
   Break;
  end;

 until false;
end;

procedure TVirtualManager._mmap_addr(Offset:Pointer;Size,addr:QWORD;direct:Boolean);
var
 key:TVirtualAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_BE,(Offset+Size),key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end else
 end;

 function _map:Boolean;
 begin
  Result:=False;
  Assert(key.Size<>0);

  //new save
  key.addr:=addr;

  Case direct of
   True :key.F.direct:=1;
   False:key.F.mapped:=1;
  end;

  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  addr  :=addr  +FSize;
  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin

 repeat

  key:=Default(TVirtualAdrNode);
  key.Offset:=Offset;

  if _fetch then
  begin
   if _map then Break;
  end else
  begin
   Break;
  end;

 until false;
end;

procedure TVirtualManager._mmap_sys(Offset:Pointer;Size:QWORD);
var
 key:TVirtualAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_FR or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_FR or C_BE,(Offset+Size),key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end;
 end;

 function _map:Boolean;
 begin
  Result:=False;
  Assert(key.Size<>0);

  //new save
  key.IsFree  :=False;
  key.F.addr  :=0; //hack
  key.F.reserv:=0;
  key.F.direct:=0;
  key.F.stack :=0;
  key.F.polled:=0;
  key.F.mapped:=0;
  key.block   :=nil;
  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

 function _skip:Boolean; inline;
 begin
  Result:=False;

  FEndN:=Offset+Size;
  FEndO:=key.Offset+key.Size;

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 if (Size=0) then Exit;
 if (Offset<Flo) or (Offset>Fhi) then Exit;

 repeat

  key:=Default(TVirtualAdrNode);
  key.IsFree:=True;
  key.Offset:=Offset;

  if _fetch then
  begin
   if _map then Break;
  end else
  if _Find_m(M_LE,key) then
  begin
   if _skip then Break;
  end else
  if _Find_m(M_BE,key) then
  begin
   if _skip then Break;
  end else
  begin
   Break;
  end;

 until false;
end;

Function TVirtualManager.check_fixed(Offset:Pointer;Size:QWORD;flags:Byte;fd:Integer):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TVirtualAdrNode;
 FEndO:Pointer;

 function _overwrite:Boolean; inline;
 begin
  Result:=(flags and MAP_NO_OVERWRITE)=0;
 end;

 function _mapped:Boolean; inline;
 begin
  Result:=((flags and MAP_ANON)=0) and (fd>0);
 end;

begin
 Result:=0;
 if (Size=0) or (Size>FMaxSize)  then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=Offset+Size;

 key:=Default(TVirtualAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  if (Offset>=key.Offset) then
  begin
   if (key.F.mapped<>0) then Exit(ENOSYS);
   if key.IsFree then
   begin
    //
   end else
   begin
    if _mapped then Exit(ENOSYS);
    if (key.block=nil) then Exit(EINVAL);
    if (key.F.reserv=0) then
    begin
     if not _overwrite then Exit(ENOMEM);
    end;
   end;
  end;

  if (key.Offset>=FEndO) then Break;

  It.Next;
 end;
end;

Function TVirtualManager.mmap(Offset:Pointer;Size,Align:QWORD;prot,flags:Byte;fd:Integer;addr:QWORD;var AdrOut:Pointer):Integer;
var
 key:TVirtualAdrNode;
 start:Pointer;
 FEndN,FEndO:Pointer;
 ASize:QWORD;
 FSize:QWORD;
 err:Integer;
 btype:Byte;

 function _fixed:Boolean; inline;
 begin
  Result:=((flags and MAP_FIXED)<>0);
 end;

 function _commited:Boolean; inline;
 begin
  Result:=((flags and MAP_VOID)=0) and (((flags and MAP_ANON)<>0) or (fd<=0));
 end;

 function _reserv:Byte; inline;
 begin
  Result:=Byte((flags and MAP_VOID)<>0);
 end;

 function _direct:Byte; inline;
 begin
  Result:=Byte(((flags and MAP_ANON)=0) and (fd=0));
 end;

 function _mapped:Byte; inline;
 begin
  Result:=Byte(((flags and MAP_ANON)=0) and (fd>0));
 end;

 function _addres:Boolean; inline;
 begin
  Result:=((flags and MAP_ANON)=0);
 end;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+ASize;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,ASize,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_BE,(Offset+ASize),key) then
  begin
   FEndN:=Offset+ASize;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end;
 end;

 function _map:Boolean;
 begin
  Result:=False;
  Assert(key.Size<>0);

  if (_direct<>0) then
  begin
   Assert(addr<$17FFFFFFF);
  end;

  //new save
  key.IsFree  :=False;
  key.addr    :=addr;
  key.F.reserv:=_reserv;
  key.F.direct:=_direct;
  key.F.stack :=0;
  key.F.polled:=0;
  key.F.mapped:=_mapped;
  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  addr  :=ia(_addres,addr,FSize);
  Offset:=Offset+FSize;

  ASize :=ASize -FSize;
  Size  :=Size  -FSize;
 end;

 function _remap:Boolean;
 begin
  Result:=False;
  err:=0;

  if (key.Size<>0) then
  begin
   if (key.F.direct<>0) then
   begin
    err:=_UnmapDirect(key.addr,key.Size);
    if (err<>0) then Exit;
   end;

   //new save
   key.IsFree  :=False;
   key.addr    :=addr;
   key.F.reserv:=_reserv;
   key.F.direct:=_direct;
   key.F.stack :=0;
   key.F.polled:=0;
   key.F.mapped:=_mapped;
   _Merge(key);
  end;

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  addr  :=ia(_addres,addr,FSize);
  Offset:=Offset+FSize;

  ASize :=ASize -FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if (Size=0) or (Size>FMaxSize) then Exit(EINVAL);
 if (Offset>Fhi) then Exit(EINVAL);

 Align:=Max(Align,PHYSICAL_PAGE_SIZE);

 ASize:=AlignUp(Size,PHYSICAL_PAGE_SIZE);

 if _fixed then
 begin
  if (Offset<Flo) then Exit(EINVAL);
 end else
 begin
  Offset:=Max(Offset,Flo);

  if (_mapped<>0) then
  begin
   Align:=Max(Align,GRANULAR_PAGE_SIZE);
  end;

  Result:=_FindFreeOffset(Offset,ASize,Align,prot,Offset);
  if (Result<>0) then Exit;
 end;

 start:=Offset;

 if not _addres then addr:=0;

 if (_mapped<>0) then
 begin
  btype:=BT_FMAP;
 end else
 if _isgpu(prot) then
 begin
  btype:=BT_GPUM;
 end else
 begin
  btype:=BT_PRIV;
 end;

 Result:=check_fixed(Offset,Size,flags,fd);
 if (Result<>0) then Exit;

 repeat

  key:=Default(TVirtualAdrNode);
  key.Offset:=Offset;

  if _fetch then
  begin

   if key.IsFree then
   begin
    if (key.block=nil) then
    begin

     if (key.Offset>Offset) then
     begin
      FSize:=key.Offset-Offset;
      FSize:=Min(Size-FSize,key.Size);
     end else
     begin
      FSize:=Offset-key.Offset;
      FSize:=Min(Size+FSize,key.Size);
     end;

     key.block:=NewAdrBlock(key.Offset,FSize,prot,btype,fd,addr);

     if (key.block=nil) then
     begin
      _Merge(key); //undo
      Assert(False);
      Exit(ENOSYS);
     end;

     Result:=_CreateBlock(key.block);
     if (Result<>0) then Exit;

     _set_block(key.block^.Offset,key.block^.Size,key.block);

     if _addres then
     begin
      _mmap_addr(key.block^.Offset,key.block^.Size,addr,_direct<>0);
     end;

    end;

    if _commited then
    begin
     Result:=key.block^.Commit(@key,prot);
     if (Result<>0) then
     begin
      Assert(false,IntToStr(Result));
      Exit(EINVAL);
     end;
    end;

    if _map then Break;
   end else
   begin
    //overwrite

    if (btype=BT_FMAP) or (key.F.mapped<>0) then
    begin
     _Merge(key); //undo
     Assert(False);
     Exit(ENOSYS);
    end;

    if (key.block=nil) then
    begin
     _Merge(key); //undo
     Exit(EINVAL);
    end;

    if _commited then
    begin
     if (key.F.reserv=0) then
     begin
      Result:=key.block^.Protect(@key,prot);
     end else
     begin
      Result:=key.block^.Commit(@key,prot);
     end;
    end else
    begin
     Result:=key.block^.Reserved(@key);
    end;

    if (Result<>0) then
    begin
     _Merge(key); //undo
     Assert(false,IntToStr(Result));
     Exit(EINVAL);
    end;

    if _remap then Break;

    if (err<>0) then
    begin
     _Merge(key); //undo
     Assert(false,IntToStr(err));
     Exit(err);
    end;

    //overwrite
   end;

  end else
  begin
   Result:=EINVAL;
   Break;
  end;

 until false;

 if (Result=0) then
 begin
  AdrOut:=start;
 end;
end;

Function TVirtualManager.Protect(Offset:Pointer;Size:QWORD;prot:Integer):Integer;
var
 key:TVirtualAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_FR or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_FR or C_BE,(Offset+Size),key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end;
 end;

 function _map:Boolean;
 begin
  Result:=False;
  Assert(key.Size<>0);

  //new save
  if (key.F.reserv=0) then
  begin
   if (key.block=nil) then
   begin
    key.F.prot:=prot;
   end else
   begin
    key.block^.Protect(@key,prot);
   end;
  end;

  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

 function _skip:Boolean; inline;
 begin
  Result:=False;

  FEndN:=Offset+Size;
  FEndO:=key.Offset+key.Size;

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if ((prot and $ffffffc8)<>0) then Exit(EINVAL);

 if (Size>FMaxSize) then Exit(EINVAL);
 if (Offset>Fhi) then Exit(EINVAL);

 FEndO:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);
 Size:=Size+(Offset-FEndO);

 Offset:=FEndO;
 Size:=AlignUp(Size,PHYSICAL_PAGE_SIZE);

 repeat

  key:=Default(TVirtualAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _fetch then
  begin
   if _map then Break;
  end else
  if _Find_m(M_LE,key) then
  begin
   if _skip then Break;
  end else
  if _Find_m(M_BE,key) then
  begin
   if _skip then Break;
  end else
  begin
   Break;
  end;

 until false;
end;

Function TVirtualManager.Mtypeprotect(Offset:Pointer;Size:QWORD;mtype,prot:Integer):Integer;
var
 key:TVirtualAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_FR or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_FR or C_BE,(Offset+Size),key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end;
 end;

 function _map:Boolean;
 begin
  Result:=False;
  Assert(key.Size<>0);

  if (key.F.direct<>0) then
  begin
   _MtypeDirect(key.addr,key.Size,mtype);
  end;

  //new save
  if (key.F.reserv=0) then
  begin
   if (key.block=nil) then
   begin
    key.F.prot:=prot;
   end else
   begin
    key.block^.Protect(@key,prot);
   end;
  end;

  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

 function _skip:Boolean; inline;
 begin
  Result:=False;

  FEndN:=Offset+Size;
  FEndO:=key.Offset+key.Size;

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if ((prot and $ffffffc8)<>0) then Exit(EINVAL);

 if (Size>FMaxSize) then Exit(EINVAL);
 if (Offset>Fhi) then Exit(EINVAL);

 FEndO:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);
 Size:=Size+(Offset-FEndO);

 Offset:=FEndO;
 Size:=AlignUp(Size,PHYSICAL_PAGE_SIZE);

 repeat

  key:=Default(TVirtualAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _fetch then
  begin
   if _map then Break;
  end else
  if _Find_m(M_LE,key) then
  begin
   if _skip then Break;
  end else
  if _Find_m(M_BE,key) then
  begin
   if _skip then Break;
  end else
  begin
   Break;
  end;

 until false;
end;

Function TVirtualManager.Release(Offset:Pointer;Size:QWORD):Integer;
var
 key:TVirtualAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;
 err:Integer;

 function _fetch:Boolean;
 begin
  Result:=False;

  if _FetchNode_m(M_LE or C_FR or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   Result:=True;
  end else
  if _FetchNode_m(M_BE or C_FR or C_BE,(Offset+Size),key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   Result:=True;
  end;
 end;

 function _map:Boolean;
 var
  block:PVirtualAdrBlock;
 begin
  Result:=False;
  err:=0;
  Assert(key.Size<>0);

  if (key.F.direct<>0) then
  begin
   err:=_UnmapDirect(key.addr,key.Size);
   if (err<>0) then Exit;
  end;

  block:=key.block;

  block^.Free(@key);

  if (block^.Used=0) then
  begin

   _FreeBlock(block);

   if (block^.F.btype=BT_FMAP) then
   begin
    err:=_VirtualUnmap(Pointer(block^.Offset));
    if (err<>0) then
    begin
     _Merge(key); //undo
     Exit;
    end;
   end else
   begin
    err:=_VirtualFree(Pointer(block^.Offset));
    if (err<>0) then
    begin
     _Merge(key); //undo
     Exit;
    end;
   end;

   _set_block(block^.Offset,block^.Size,nil);
   FreeMem(block);

   key.block:=nil;
  end;

  //new save
  key.IsFree  :=True;
  key.F.prot  :=0;
  key.F.addr  :=0; //hack
  key.F.reserv:=0;
  key.F.direct:=0;
  key.F.stack :=0;
  key.F.polled:=0;
  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

 function _skip:Boolean; inline;
 begin
  Result:=False;

  FEndN:=Offset+Size;
  FEndO:=key.Offset+key.Size;

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if (Size=0) or (Size>FMaxSize)  then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);
 Size:=Size+(Offset-FEndO);

 Offset:=FEndO;
 Size:=AlignUp(Size,PHYSICAL_PAGE_SIZE);

 repeat

  key:=Default(TVirtualAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _fetch then
  begin

   if (key.block=nil) then
   begin
    if _skip then Break;
   end else
   begin
    if _map then Break;

    if (err<>0) then
    begin
     Assert(false,IntToStr(err));
     Exit(EINVAL);
    end;
   end;

  end else
  if _Find_m(M_LE,key) then
  begin
   if _skip then Break;
  end else
  if _Find_m(M_BE,key) then
  begin
   if _skip then Break;
  end else
  begin
   Break;
  end;

 until false;
end;

Function TVirtualManager.Query(Offset:Pointer;next:Boolean;var ROut:TVirtualAdrNode):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TVirtualAdrNode;
begin
 Result:=0;

 if (Offset>Fhi) then Exit(EINVAL);

 Offset:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);

 key:=Default(TVirtualAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);

 if (It.Item=nil) then
 begin
  if next then
  begin
   It:=FAllcSet.find_be(key);
  end else
  begin
   Exit(EINVAL);
  end;
 end;

 key:=It.Item^;

 if (Offset>=(key.Size+key.Offset)) then Exit(EINVAL);

 if next then
 begin

  repeat
   if (It.Item=nil) then Exit(EACCES);
   key:=It.Item^;
   if (not key.IsFree) then Break;
   It.Next;
  until false;

 end else
 begin
  if key.IsFree then Exit(EACCES);
 end;

 ROut:=key;
end;

Function TVirtualManager.QueryProt(Offset:Pointer;var ROut:TVirtualAdrNode):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TVirtualAdrNode;
begin
 Result:=0;

 if (Offset>Fhi) then Exit(EINVAL);

 Offset:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);

 key:=Default(TVirtualAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);

 if (It.Item=nil) then Exit(EINVAL);

 key:=It.Item^;

 if (Offset>=(key.Size+key.Offset)) then Exit(EINVAL);

 if key.IsFree then Exit(EACCES);

 ROut:=key;
end;

Function TVirtualManager.TryGetMapBlockByAddr(Offset:Pointer;var block:PVirtualAdrBlock):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TVirtualAdrNode;
begin
 Result:=False;

 if (Offset>Fhi) then Exit;

 Offset:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);

 key:=Default(TVirtualAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);

 if (It.Item=nil) then Exit;

 key:=It.Item^;

 if key.IsFree then Exit;
 if (key.block=nil) then Exit;

 block:=key.block;
 Result:=True;
end;

function _alloc_str(var key:TVirtualAdrNode):RawByteString;
begin
 if (key.F.Free<>0) then
 begin
  Result:='FREE';
 end else
 if (key.block=nil) then
 begin
  Result:='SYST';
 end else
 if (key.F.reserv<>0) then
 begin
  Result:='RSRV';
 end else
 if (key.F.direct<>0) then
 begin
  Result:='DRCT';
 end else
 if (key.F.stack<>0) then
 begin
  Result:='STCK';
 end else
 if (key.F.polled<>0) then
 begin
  Result:='POOL';
 end else
 if (key.F.mapped<>0) then
 begin
  Result:='FMAP';
 end else
 begin
  Result:='ALLC';
 end;
end;

procedure TVirtualManager.Print;
var
 key:TVirtualAdrNode;
 It:TAllcPoolNodeSet.Iterator;
begin
 It:=FAllcSet.cbegin;
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  Writeln(HexStr(QWORD(key.Offset),11),'..',
          HexStr(QWORD(key.Offset+key.Size),11),':',
          HexStr(key.Size,11),'#',
          HexStr(qword(key.addr),11),'#',
          _alloc_str(key),'#');

  It.Next;
 end;
end;

end.




