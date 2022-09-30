unit mm_adr_direct;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  g23tree,
  bittype;

{
 Direct node:
 [
  offset 12..39:28
  size   12..39:28
  free    0..0 :1
  mtype   0..6 :7

  addr   12..39:28
 ]
}

type
 TDirectAdrNode=packed object
  private
   //free:  [Size]  |[Offset]
   //alloc: [Offset]
   Function  GetOffset:QWORD;      inline;
   Procedure SetOffset(q:qword);   inline;
   Function  GetSize:QWORD;        inline;
   Procedure SetSize(q:qword);     inline;
   Function  GetAddr:Pointer;      inline;
   Procedure SetAddr(p:Pointer);   inline;
   Function  GetIsFree:Boolean;    inline;
   Procedure SetIsFree(b:Boolean); inline;
  public
   F:bitpacked record
    Offset:bit28;
    Size  :bit28;
    Free  :bit1;
    mtype :bit7;
    addr  :DWORD;
   end;
   property Offset:QWORD   read GetOffset write SetOffset;
   property Size:QWORD     read GetSize   write SetSize;
   property addr:Pointer   read GetAddr   write SetAddr;
   property IsFree:Boolean read GetIsFree write SetIsFree;
 end;

type
 TDirectAdrFreeCompare=object
  function c(const a,b:TDirectAdrNode):Integer; static;
 end;
 TDirectAdrAllcCompare=object
  function c(const a,b:TDirectAdrNode):Integer; static;
 end;

 TMemoryUnmapCb=procedure(addr:Pointer;Size:QWORD);

 TDirectManager=class
  private
   type
    TFreePoolNodeSet=specialize T23treeSet<TDirectAdrNode,TDirectAdrFreeCompare>;
    TAllcPoolNodeSet=specialize T23treeSet<TDirectAdrNode,TDirectAdrAllcCompare>;

   const
    Flo=0;
    Fhi=$17FFFFFFF;

   var
    FFreeSet:TFreePoolNodeSet;
    FAllcSet:TAllcPoolNodeSet;
  public
    Constructor Create;
  private
    procedure _Insert(const key:TDirectAdrNode);
    Function  _FetchFree_a(Size,Align:QWORD;var R:TDirectAdrNode):Boolean;
    Function  _FetchFree_s(ss,se,Size,Align:QWORD;var R:TDirectAdrNode):Boolean;
    Function  _FetchNode_m(mode:Byte;cmp:QWORD;var R:TDirectAdrNode):Boolean;
    Function  _Find_m(mode:Byte;var R:TDirectAdrNode):Boolean;

    procedure _Merge(key:TDirectAdrNode);
    procedure _Devide(Offset,Size:QWORD;var key:TDirectAdrNode);
    procedure _UnmapVirtual(addr:Pointer;Size:QWORD);
  public
    var
     OnMemoryUnmapCb:TMemoryUnmapCb;

    Function  Alloc_any(Size,Align:QWORD;mtype:Byte;var AdrOut:QWORD):Integer;
    Function  Alloc_search(ss,se,Size,Align:QWORD;mtype:Byte;var AdrOut:QWORD):Integer;
    Function  Query(Offset:QWORD;next:Boolean;var ROut:TDirectAdrNode):Integer;
    Function  QueryMType(Offset:QWORD;var ROut:TDirectAdrNode):Integer;
    Function  CheckedAvailable(ss,se,Align:QWORD;var AdrOut,SizeOut:QWORD):Integer;
    Function  CheckedAlloc(Offset,Size:QWORD):Integer;
    Function  CheckedMMap(Offset,Size:QWORD):Integer;
    Function  CheckedRelease(Offset,Size:QWORD):Integer;
    Function  Release(Offset,Size:QWORD):Integer;
    Function  mmap_addr(Offset,Size:QWORD;addr:Pointer):Integer;
    Function  mmap_addr2(Offset,Size:QWORD;addr:Pointer;mtype:Byte):Integer;
    Function  unmap_addr(Offset,Size:QWORD):Integer;

    procedure Print;
 end;

implementation

const
 ENOENT= 2;
 ENOMEM=12;
 EACCES=13;
 EBUSY =16;
 EINVAL=22;

//

function TDirectAdrFreeCompare.c(const a,b:TDirectAdrNode):Integer;
begin
 //1 FSize
 Result:=Integer(a.F.Size>b.F.Size)-Integer(a.F.Size<b.F.Size);
 if (Result<>0) then Exit;
 //2 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

function TDirectAdrAllcCompare.c(const a,b:TDirectAdrNode):Integer;
begin
 //1 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

//

function ia(addr:Pointer;Size:qword):Pointer; inline;
begin
 if (addr=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=addr+Size;
 end;
end;

function Max(a,b:QWORD):QWORD; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

//

Function TDirectAdrNode.GetOffset:QWORD; inline;
begin
 Result:=QWORD(F.Offset) shl 12;
end;

Procedure TDirectAdrNode.SetOffset(q:qword); inline;
begin
 F.Offset:=DWORD(q shr 12);
 Assert(GetOffset=q);
end;

Function TDirectAdrNode.GetSize:QWORD; inline;
begin
 Result:=QWORD(F.Size) shl 12;
end;

Procedure TDirectAdrNode.SetSize(q:qword); inline;
begin
 F.Size:=DWORD(q shr 12);
 Assert(GetSize=q);
end;

Function TDirectAdrNode.GetAddr:Pointer; inline;
begin
 Result:=Pointer(QWORD(F.addr) shl 12);
end;

Procedure TDirectAdrNode.SetAddr(p:Pointer); inline;
begin
 F.addr:=DWORD(QWORD(p) shr 12);
 Assert(GetAddr=p);
end;

Function TDirectAdrNode.GetIsFree:Boolean; inline;
begin
 Result:=Boolean(F.Free);
end;

Procedure TDirectAdrNode.SetIsFree(b:Boolean); inline;
begin
 F.Free:=Byte(b);
end;

///

Constructor TDirectManager.Create;
var
 key:TDirectAdrNode;
begin
 Assert(Flo<Fhi);

 key:=Default(TDirectAdrNode);
 key.IsFree:=True;
 key.Offset:=Flo;
 key.Size  :=(Fhi-Flo+1);

 _Insert(key);
end;

procedure TDirectManager._Insert(const key:TDirectAdrNode);
begin
 if key.IsFree then
 begin
  FFreeSet.Insert(key);
 end;
 FAllcSet.Insert(key);
end;

//free:  [Size]  |[Offset]
Function TDirectManager._FetchFree_a(Size,Align:QWORD;var R:TDirectAdrNode):Boolean;
var
 It:TFreePoolNodeSet.Iterator;
 key:TDirectAdrNode;
 Offset:QWORD;
begin
 Result:=false;
 key:=Default(TDirectAdrNode);
 key.Size:=Size;
 It:=FFreeSet.find_be(key);
 if (It.Item=nil) then Exit;
 repeat
  key:=It.Item^;
  Offset:=System.Align(key.Offset,Align);
  if (Offset+Size)<=(key.Offset+key.Size) then
  begin
   R:=key;
   FAllcSet.delete(key);
   FFreeSet.erase(It);
   Exit(True);
  end;
 until not It.Next;
end;

Function TDirectManager._FetchFree_s(ss,se,Size,Align:QWORD;var R:TDirectAdrNode):Boolean;
var
 It:TFreePoolNodeSet.Iterator;
 key:TDirectAdrNode;
 Offset:QWORD;
 FEndN,FEndO:QWORD;
begin
 Result:=false;
 key:=Default(TDirectAdrNode);
 key.Offset:=ss;
 key.Size  :=Size;
 It:=FFreeSet.find_be(key);
 if (It.Item=nil) then Exit;
 repeat
  key:=It.Item^;
  Offset:=System.Align(Max(key.Offset,ss),Align);
  if (se>=Offset) then
  begin
   FEndN:=key.Offset+key.Size;
   FEndO:=Offset+Size;
   if (FEndO<=FEndN) then
   begin
    R:=key;
    FAllcSet.delete(key);
    FFreeSet.erase(It);
    Exit(True);
   end;
  end;
 until not It.Next;
end;

const
 M_LE=0;
 M_BE=1;

 C_UP=2;
 C_DW=4;

 C_LE=6;
 C_BE=8;

Function TDirectManager._FetchNode_m(mode:Byte;cmp:QWORD;var R:TDirectAdrNode):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
 key,rkey:TDirectAdrNode;
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

 if (rkey.IsFree <>key.IsFree ) then Exit;
 if (rkey.F.mtype<>key.F.mtype) then Exit;

 Case (mode and (not 1)) of
  C_UP:
       begin
        if (ia(rkey.addr,rkey.Size)<>key.addr) then Exit;
        if ((rkey.Offset+rkey.Size)<>cmp     ) then Exit;
       end;
  C_DW:
       begin
        if (rkey.addr  <>key.addr) then Exit;
        if (rkey.Offset<>cmp     ) then Exit;
       end;

  C_LE:if ((rkey.Offset+rkey.Size)<cmp) then Exit;
  C_BE:if (key.Offset<cmp) then Exit;

  else
       Exit;
 end;

 R:=rkey;
 FAllcSet.erase(It);
 FFreeSet.delete(rkey);
 Result:=True;
end;

Function TDirectManager._Find_m(mode:Byte;var R:TDirectAdrNode):Boolean;
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

procedure TDirectManager._Merge(key:TDirectAdrNode);
var
 rkey:TDirectAdrNode;
begin

 //prev union
 repeat
  rkey:=key;
  rkey.F.Offset:=rkey.F.Offset-1; //hack
  rkey.addr    :=key.addr;        //find addr

  if not _FetchNode_m(M_LE or C_UP,key.Offset,rkey) then Break;

  key.F.Size  :=key.F.Size+(key.F.Offset-rkey.F.Offset); //hack
  key.F.Offset:=rkey.F.Offset;                           //hack
  key.addr    :=rkey.addr;                               //prev addr
 until false;

 //next union
 repeat
  rkey:=key;
  rkey.F.Offset:=rkey.F.Offset+rkey.F.Size; //hack
  rkey.addr    :=ia(key.addr,key.Size);     //find addr

  if not _FetchNode_m(M_BE or C_DW,(key.Offset+key.Size),rkey) then Break;

  key.F.Size  :=key.F.Size+rkey.F.Size; //hack
 until false;

 _Insert(key);
end;

procedure TDirectManager._Devide(Offset,Size:QWORD;var key:TDirectAdrNode);
var
 FOffset:QWORD;
 FSize:QWORD;
 Faddr:Pointer;
 FEndN,FEndO:QWORD;
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
  key.addr  :=ia(Faddr,(FEndN-FOffset));

  _Merge(key);
 end else
 if (FEndO<>FEndN) then //tunc size
 begin
  Size:=FEndO-Offset;
 end;

 //new save
 key.Offset :=Offset;
 key.Size   :=Size;
 key.addr   :=ia(Faddr,(Offset-FOffset));
end;

procedure TDirectManager._UnmapVirtual(addr:Pointer;Size:QWORD);
begin
 if (addr=nil) or (Size=0) then Exit;
 if (OnMemoryUnmapCb=nil) then Exit;
 OnMemoryUnmapCb(addr,Size);
end;

Function TDirectManager.Alloc_any(Size,Align:QWORD;mtype:Byte;var AdrOut:QWORD):Integer;
var
 key:TDirectAdrNode;
 Offset:QWORD;
begin
 Result:=0;
 if (Size=0) or (Align=0) then Exit(EINVAL);

 key:=Default(TDirectAdrNode);

 if _FetchFree_a(Size,Align,key) then
 begin
  Offset:=System.Align(key.Offset,Align);

  _Devide(Offset,Size,key);

  //new save
  key.IsFree :=False;
  key.F.mtype:=mtype;
  key.addr   :=nil;
  _Merge(key);

  AdrOut:=key.Offset;
  Result:=0;
 end else
 begin
  Result:=ENOMEM;
 end;
end;

Function TDirectManager.Alloc_search(ss,se,Size,Align:QWORD;mtype:Byte;var AdrOut:QWORD):Integer;
var
 key:TDirectAdrNode;
 Offset:QWORD;
begin
 Result:=0;
 if (Size=0) or (Align=0) then Exit(EINVAL);
 if (ss<Flo) or (ss>Fhi)  then Exit(EINVAL);
 if (se<Flo) or (se<ss)   then Exit(EINVAL);

 key:=Default(TDirectAdrNode);

 if _FetchFree_s(ss,se,Size,Align,key) then
 begin
  Offset:=System.Align(Max(key.Offset,ss),Align);

  _Devide(Offset,Size,key);

  //new save
  key.IsFree :=False;
  key.F.mtype:=mtype;
  key.addr   :=nil;
  _Merge(key);

  AdrOut:=key.Offset;
  Result:=0;
 end else
 begin
  Result:=ENOMEM;
 end;
end;

Function TDirectManager.Query(Offset:QWORD;next:Boolean;var ROut:TDirectAdrNode):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TDirectAdrNode;
begin
 Result:=0;
 key:=Default(TDirectAdrNode);
 key.Offset:=Offset;

 if next then
 begin
  It:=FAllcSet.find_be(key);
 end else
 begin
  It:=FAllcSet.find(key);
 end;

 if (It.Item=nil) then Exit(EACCES);

 key:=It.Item^;

 if key.IsFree then Exit(EACCES);

 ROut:=key;
end;

Function TDirectManager.QueryMType(Offset:QWORD;var ROut:TDirectAdrNode):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TDirectAdrNode;
begin
 Result:=0;
 key:=Default(TDirectAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);

 if (It.Item=nil) then Exit(ENOENT);

 key:=It.Item^;

 if key.IsFree then Exit(ENOENT);

 ROut:=key;
end;

Function TDirectManager.CheckedAvailable(ss,se,Align:QWORD;var AdrOut,SizeOut:QWORD):Integer;
var
 It:TFreePoolNodeSet.Iterator;
 key:TDirectAdrNode;
 Offset:QWORD;
 Size:QWORD;
begin
 Result:=ENOMEM;
 if (Align=0) then Exit(EINVAL);
 if (ss<Flo) or (ss>Fhi)  then Exit(EINVAL);
 if (se<Flo) or (se<ss)   then Exit(EINVAL);

 key:=Default(TDirectAdrNode);
 key.Offset:=ss;

 It:=FAllcSet.find_le(key);
 While (It.Item<>nil) do
 begin
  key:=It.Item^;
  Offset:=System.Align(Max(key.Offset,ss),Align);
  if (se>=Offset) then
  begin
   Size:=key.Size-(Offset-key.Offset);
   AdrOut :=Offset;
   SizeOut:=Size;
   Exit(0);
  end;
  It.Next
 end;
end;

Function TDirectManager.CheckedAlloc(Offset,Size:QWORD):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TDirectAdrNode;
 FEndO:QWORD;
begin
 Result:=0;
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=Offset+Size;

 key:=Default(TDirectAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  if (Offset>=key.Offset) then
  begin
   if not key.IsFree then
   begin
    Exit(ENOMEM);
   end;
  end;

  if (key.Offset>=FEndO) then Break;

  It.Next;
 end;
end;

Function TDirectManager.CheckedMMap(Offset,Size:QWORD):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TDirectAdrNode;
 FEndO:QWORD;
begin
 Result:=0;
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=Offset+Size;

 key:=Default(TDirectAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  if (Offset>=key.Offset) then
  begin
   if key.IsFree then
   begin
    Exit(EACCES);
   end;
   if (key.addr<>nil) then
   begin
    Exit(EBUSY);
   end;
  end;

  if (key.Offset>=FEndO) then Break;

  It.Next;
 end;
end;

Function TDirectManager.CheckedRelease(Offset,Size:QWORD):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TDirectAdrNode;
 FEndO:QWORD;
begin
 Result:=ENOENT;
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=Offset+Size;

 key:=Default(TDirectAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  if not key.IsFree then
  begin
   if (key.Offset>=FEndO) then Break;
   Result:=0;
   Break;
  end;

  It.Next;
 end;
end;

Function TDirectManager.Release(Offset,Size:QWORD):Integer;
var
 key:TDirectAdrNode;
 FEndN,FEndO:QWORD;
 FSize:QWORD;

 function _map:Boolean;
 begin
  Result:=False;

  //new save
  key.IsFree :=True;
  key.F.mtype:=0;
  key.addr   :=nil;
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
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 repeat

  key:=Default(TDirectAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   _UnmapVirtual(key.addr,key.Size);

   if _map then Break;
  end else
  if _FetchNode_m(M_BE or C_BE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   _UnmapVirtual(key.addr,key.Size);

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

Function TDirectManager.mmap_addr(Offset,Size:QWORD;addr:Pointer):Integer;
var
 key:TDirectAdrNode;
 FEndN,FEndO:QWORD;
 FSize:QWORD;

 function _map:Boolean;
 begin
  Result:=False;

  //new save
  key.addr   :=addr;
  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  addr  :=ia(addr,FSize);
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

  addr  :=ia(addr,FSize);
  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if (Size=0)                     then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 repeat

  key:=Default(TDirectAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   if _map then Break;
  end else
  if _FetchNode_m(M_BE or C_BE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

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

Function TDirectManager.mmap_addr2(Offset,Size:QWORD;addr:Pointer;mtype:Byte):Integer;
var
 key:TDirectAdrNode;
 FEndN,FEndO:QWORD;
 FSize:QWORD;

 function _map:Boolean;
 begin
  Result:=False;

  //new save
  key.F.mtype:=mtype;
  key.addr   :=addr;
  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  addr  :=ia(addr,FSize);
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

  addr  :=ia(addr,FSize);
  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if (Size=0)                     then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 repeat

  key:=Default(TDirectAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   if _map then Exit;
  end else
  if _FetchNode_m(M_BE or C_BE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   if _map then Exit;
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

Function TDirectManager.unmap_addr(Offset,Size:QWORD):Integer;
begin
 Result:=mmap_addr(Offset,Size,nil);
end;

function _alloc_str(IsFree:Boolean):RawByteString;
begin
 Case IsFree of
  True :Result:='FREE';
  FAlse:Result:='ALLC';
 end;
end;

procedure TDirectManager.Print;
var
 key:TDirectAdrNode;
 It:TAllcPoolNodeSet.Iterator;
begin
 It:=FAllcSet.cbegin;
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  Writeln(HexStr(key.Offset,10),'..',
          HexStr(key.Offset+key.Size,10),':',
          HexStr(key.Size,10),'#',
          HexStr(qword(key.addr),10),'#',
          _alloc_str(key.IsFree),'#',
          key.F.mtype);

  It.Next;
 end;
end;

procedure itest;
var
 test:TDirectManager;
 addr:array[0..5] of qword;
begin
 test:=TDirectManager.Create;

 test.Alloc_any(4*1024,1,0,addr[0]);
 Writeln(HexStr(addr[0],16));

 test.Alloc_any(4*1024,1,0,addr[1]);
 Writeln(HexStr(addr[1],16));

 test.Alloc_any(4*1024,1,0,addr[2]);
 Writeln(HexStr(addr[2],16));

 test.Alloc_any(4*1024,1,0,addr[3]);
 Writeln(HexStr(addr[3],16));

 test.Alloc_any(4*1024,1,0,addr[4]);
 Writeln(HexStr(addr[4],16));

 test.Alloc_any(4*1024,1,0,addr[5]);
 Writeln(HexStr(addr[5],16));

 writeln;
 test.Print;
 writeln;

 test.Release(addr[0],4*1024);
 test.Release(addr[2],4*1024);
 //test.Release(addr[4],4*1024);

 writeln;
 test.Print;
 writeln;

 //writeln(test.CheckedRelease(addr[1],4*1024*2));

 //test.Release(addr[1],4*1024*2);

 //test.Release(addr[0],4*1024);
 //test.Release(addr[2],4*1024);
 //test.Release(addr[1],4*1024);

 //test.Release(addr[0],4*1024);
 //test.Release(addr[1],4*1024);
 //test.Release(addr[2],4*1024);
 //test.Release(addr[2],4*1024);

 //writeln(test.CheckedRelease(addr[1],4*1024));
 //writeln(test.CheckedRelease(addr[2],4*1024));

 //test.Release(addr[3]+4*1024,4*1024);

 test.Release(addr[4],4*1024);

 writeln(test.CheckedMmap(addr[1],4*1024));

 test.mmap_addr(addr[0],4*1024*6,Pointer(4*1024));

 writeln(test.CheckedMmap(addr[1],4*1024));

 writeln;
 test.Print;
 writeln;

 test.Release(addr[0],4*1024*6);

 writeln;
 test.Print;
 writeln;

 //test.Alloc_any(4*1024,1,0,addr[0]);
 //Writeln(HexStr(addr[0],16));
 //
 //test.Alloc_any(4*1024,1,0,addr[1]);
 //Writeln(HexStr(addr[1],16));
 //
 //test.Alloc_any(4*1024,1,0,addr[2]);
 //Writeln(HexStr(addr[2],16));

 readln;
end;

initialization
 //itest;

end.




