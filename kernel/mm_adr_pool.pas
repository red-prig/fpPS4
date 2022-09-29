unit mm_adr_pool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  g23tree,
  bittype,
  spinlock;

{
 Free pool node:
 [
  offset 12..39:28
  size   12..39:28
  free    0..1 :1
  Ext     0..6 :7
 ]
}

type
 TPoolAdrNode=packed object
  private
   //free:  [Size]  |[Offset]
   //alloc: [Offset]
   Function  GetOffset:QWORD;      inline;
   Procedure SetOffset(q:qword);   inline;
   Function  GetSize:QWORD;        inline;
   Procedure SetSize(q:qword);     inline;
   Function  GetIsFree:Boolean;    inline;
   Procedure SetIsFree(b:Boolean); inline;
  public
   F:bitpacked record
    Offset:bit28;
    Size  :bit28;
    Free  :bit1;
    Ext   :bit7;
   end;
   property Offset:QWORD   read GetOffset write SetOffset;
   property Size:QWORD     read GetSize   write SetSize;
   property IsFree:Boolean read GetIsFree write SetIsFree;
 end;

type
 TPoolAdrFreeCompare=object
  function c(const a,b:TPoolAdrNode):Integer; static;
 end;
 TPoolAdrAllcCompare=object
  function c(const a,b:TPoolAdrNode):Integer; static;
 end;

 TPoolManager=class
  private
   type
    TFreePoolNodeSet=specialize T23treeSet<TPoolAdrNode,TPoolAdrFreeCompare>;
    TAllcPoolNodeSet=specialize T23treeSet<TPoolAdrNode,TPoolAdrAllcCompare>;

   var
    Flo,Fhi:QWORD;

    FFreeSet:TFreePoolNodeSet;
    FAllcSet:TAllcPoolNodeSet;
  public
    property lo:QWORD read Flo;
    property hi:QWORD read Fhi;

    Constructor Create(_lo,_hi:QWORD);
  private
    procedure _Insert(const key:TPoolAdrNode);
    Function  _FetchFree_a(Size,Align:QWORD;var R:TPoolAdrNode):Boolean;
    Function  _FetchFree_s(ss,se,Size,Align:QWORD;var R:TPoolAdrNode):Boolean;
    Function  _FetchNode_a(mode:Byte;var R:TPoolAdrNode):Boolean;
    Function  _FetchNode_m(mode:Byte;cmp:QWORD;var R:TPoolAdrNode):Boolean;
    Function  _Find_m(mode:Byte;var R:TPoolAdrNode):Boolean;

    procedure _Merge(key:TPoolAdrNode);
    procedure _Devide(Offset,Size:QWORD;var key:TPoolAdrNode);
  public
    Function  Alloc_any(Size,Align:QWORD;ext:Byte;var AdrOut:QWORD):Integer;
    Function  Alloc_search(ss,se,Size,Align:QWORD;ext:Byte;var AdrOut:QWORD):Integer;
    Function  CheckedAvailable(ss,se,Align:QWORD;var AdrOut,SizeOut:QWORD):Integer;
    Function  CheckedAlloc(Offset,Size:QWORD):Integer;
    Function  CheckedRelease(Offset,Size:QWORD):Integer;
    Function  Release(Offset,Size:QWORD):Integer;

    procedure Print;
 end;

implementation

const
 ENOENT= 2;
 ENOMEM=12;
 EINVAL=22;

//

function TPoolAdrFreeCompare.c(const a,b:TPoolAdrNode):Integer;
begin
 //1 FSize
 Result:=Integer(a.F.Size>b.F.Size)-Integer(a.F.Size<b.F.Size);
 if (Result<>0) then Exit;
 //2 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

function TPoolAdrAllcCompare.c(const a,b:TPoolAdrNode):Integer;
begin
 //1 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

//

function Max(a,b:QWORD):QWORD; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

//

Function TPoolAdrNode.GetOffset:QWORD; inline;
begin
 Result:=QWORD(F.Offset) shl 12;
end;

Procedure TPoolAdrNode.SetOffset(q:qword); inline;
begin
 F.Offset:=DWORD(q shr 12);
 Assert(GetOffset=q);
end;

Function TPoolAdrNode.GetSize:QWORD; inline;
begin
 Result:=QWORD(F.Size) shl 12;
end;

Procedure TPoolAdrNode.SetSize(q:qword); inline;
begin
 F.Size:=DWORD(q shr 12);
 Assert(GetSize=q);
end;

Function TPoolAdrNode.GetIsFree:Boolean; inline;
begin
 Result:=Boolean(F.Free);
end;

Procedure TPoolAdrNode.SetIsFree(b:Boolean); inline;
begin
 F.Free:=Byte(b);
end;

///

Constructor TPoolManager.Create(_lo,_hi:QWORD);
var
 key:TPoolAdrNode;
begin
 Assert(_lo<_hi);

 Flo:=_lo;
 Fhi:=_hi;

 key:=Default(TPoolAdrNode);
 key.IsFree:=True;
 key.Offset:=_lo;
 key.Size  :=(_hi-_lo+1);

 _Insert(key);
end;

procedure TPoolManager._Insert(const key:TPoolAdrNode);
begin
 if key.IsFree then
 begin
  FFreeSet.Insert(key);
 end;
 FAllcSet.Insert(key);
end;

//free:  [Size]  |[Offset]
Function TPoolManager._FetchFree_a(Size,Align:QWORD;var R:TPoolAdrNode):Boolean;
var
 It:TFreePoolNodeSet.Iterator;
 key:TPoolAdrNode;
 Offset:QWORD;
begin
 Result:=false;
 key:=Default(TPoolAdrNode);
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

Function TPoolManager._FetchFree_s(ss,se,Size,Align:QWORD;var R:TPoolAdrNode):Boolean;
var
 It:TFreePoolNodeSet.Iterator;
 key:TPoolAdrNode;
 Offset:QWORD;
 FEndN,FEndO:QWORD;
begin
 Result:=false;
 key:=Default(TPoolAdrNode);
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

//alloc: [Offset]
Function TPoolManager._FetchNode_a(mode:Byte;var R:TPoolAdrNode):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
 key,rkey:TPoolAdrNode;
begin
 Result:=false;

 key:=R;

 Case mode of
  M_LE:It:=FAllcSet.find_le(key);
  M_BE:It:=FAllcSet.find_be(key);
  else
       Exit;
 end;

 if (It.Item=nil) then Exit;

 rkey:=It.Item^;

 if (rkey.IsFree<>key.IsFree) then Exit;
 if (rkey.F.Ext <>key.F.Ext ) then Exit;

 R:=rkey;
 FAllcSet.erase(It);
 FFreeSet.delete(rkey);
 Result:=True;
end;

Function TPoolManager._FetchNode_m(mode:Byte;cmp:QWORD;var R:TPoolAdrNode):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
 key,rkey:TPoolAdrNode;
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

 if (rkey.IsFree<>key.IsFree) then Exit;
 if (rkey.F.Ext <>key.F.Ext ) then Exit;

 Case (mode and (not 1)) of
  C_UP:if ((rkey.Offset+rkey.Size)<>cmp) then Exit;
  C_DW:if (rkey.Offset<>cmp) then Exit;

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

Function TPoolManager._Find_m(mode:Byte;var R:TPoolAdrNode):Boolean;
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

procedure TPoolManager._Merge(key:TPoolAdrNode);
var
 rkey:TPoolAdrNode;
begin

 //prev union
 repeat
  rkey:=key;
  rkey.F.Offset:=rkey.F.Offset-1; //hack

  if not _FetchNode_m(M_LE or C_UP,key.Offset,rkey) then Break;

  key.F.Size  :=key.F.Size+(key.F.Offset-rkey.F.Offset); //hack
  key.F.Offset:=rkey.F.Offset;                           //hack
 until false;

 //next union
 repeat
  rkey:=key;
  rkey.F.Offset:=rkey.F.Offset+rkey.F.Size; //hack

  if not _FetchNode_m(M_BE or C_DW,(key.Offset+key.Size),rkey) then Break;

  key.F.Size  :=key.F.Size+rkey.F.Size; //hack
 until false;

 _Insert(key);
end;

procedure TPoolManager._Devide(Offset,Size:QWORD;var key:TPoolAdrNode);
var
 FOffset:QWORD;
 FSize:QWORD;
 FEndN,FEndO:QWORD;
begin
 FOffset:=key.Offset;
 FSize  :=key.Size;

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
  _Merge(key);
 end else
 if (FEndO<>FEndN) then //tunc size
 begin
  Size:=FEndO-Offset;
 end;

 //new save
 key.Offset:=Offset;
 key.Size  :=Size;
end;

Function TPoolManager.Alloc_any(Size,Align:QWORD;ext:Byte;var AdrOut:QWORD):Integer;
var
 key:TPoolAdrNode;
 Offset:QWORD;
begin
 Result:=0;
 if (Size=0) or (Align=0) then Exit(EINVAL);

 key:=Default(TPoolAdrNode);

 if _FetchFree_a(Size,Align,key) then
 begin
  Offset:=System.Align(key.Offset,Align);

  _Devide(Offset,Size,key);

  //new save
  key.IsFree:=False;
  key.F.Ext :=ext;
  _Merge(key);

  AdrOut:=key.Offset;
  Result:=0;
 end else
 begin
  Result:=ENOMEM;
 end;
end;

Function TPoolManager.Alloc_search(ss,se,Size,Align:QWORD;ext:Byte;var AdrOut:QWORD):Integer;
var
 key:TPoolAdrNode;
 Offset:QWORD;
begin
 Result:=0;
 if (Size=0) or (Align=0) then Exit(EINVAL);
 if (ss<Flo) or (ss>Fhi)  then Exit(EINVAL);
 if (se<Flo) or (se<ss)   then Exit(EINVAL);

 key:=Default(TPoolAdrNode);

 if _FetchFree_s(ss,se,Size,Align,key) then
 begin
  Offset:=System.Align(Max(key.Offset,ss),Align);

  _Devide(Offset,Size,key);

  //new save
  key.IsFree:=False;
  key.F.Ext :=ext;
  _Merge(key);

  AdrOut:=key.Offset;
  Result:=0;
 end else
 begin
  Result:=ENOMEM;
 end;
end;

Function TPoolManager.CheckedAvailable(ss,se,Align:QWORD;var AdrOut,SizeOut:QWORD):Integer;
var
 It:TFreePoolNodeSet.Iterator;
 key:TPoolAdrNode;
 Offset:QWORD;
 Size:QWORD;
begin
 Result:=ENOMEM;
 if (Align=0) then Exit(EINVAL);
 if (ss<Flo) or (ss>Fhi)  then Exit(EINVAL);
 if (se<Flo) or (se<ss)   then Exit(EINVAL);

 key:=Default(TPoolAdrNode);
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

Function TPoolManager.CheckedAlloc(Offset,Size:QWORD):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TPoolAdrNode;
 FEndN,FEndO:QWORD;
begin
 Result:=ENOMEM;
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 key:=Default(TPoolAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);
 if (It.Item<>nil) then
 begin
  key:=It.Item^;

  FEndN:=key.Offset+key.Size;

  if key.IsFree then
  if (Offset>=key.Offset) then
  begin
   FEndO:=Offset+Size;
   FEndN:=key.Offset+key.Size;

   if (FEndN>=FEndO) then
   begin
    Result:=0;
   end;
  end;

 end;
end;

Function TPoolManager.CheckedRelease(Offset,Size:QWORD):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TPoolAdrNode;
 FEndO:QWORD;
begin
 Result:=ENOENT;
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=Offset+Size;

 key:=Default(TPoolAdrNode);
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

Function TPoolManager.Release(Offset,Size:QWORD):Integer;
var
 key:TPoolAdrNode;
 FEndN,FEndO:QWORD;
 FSize:QWORD;
begin
 Result:=0;
 if (Size=0) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 repeat

  key:=Default(TPoolAdrNode);
  key.IsFree:=False;
  key.Offset:=Offset;

  if _FetchNode_m(M_LE or C_LE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(Offset,Size,key);

   //new save
   key.IsFree :=True;
   key.F.ext  :=0;
   _Merge(key);

   if (FEndO>=FEndN) then Break;

   FSize:=FEndO-Offset;

   Offset:=Offset+FSize;
   Size  :=Size  -FSize;
  end else
  if _FetchNode_m(M_BE or C_BE,Offset,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   _Devide(key.Offset,FEndN-key.Offset,key);

   //new save
   key.IsFree :=True;
   key.F.ext  :=0;
   _Merge(key);

   if (FEndO>=FEndN) then Break;

   FSize:=FEndO-Offset;

   Offset:=Offset+FSize;
   Size  :=Size  -FSize;
  end else
  if _Find_m(M_LE,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   if (FEndO>=FEndN) then Break;

   FSize:=FEndO-Offset;

   Offset:=Offset+FSize;
   Size  :=Size  -FSize;
  end else
  if _Find_m(M_BE,key) then
  begin
   FEndN:=Offset+Size;
   FEndO:=key.Offset+key.Size;

   if (FEndO>=FEndN) then Break;

   FSize:=FEndO-Offset;

   Offset:=Offset+FSize;
   Size  :=Size  -FSize;
  end else
  begin
   Break;
  end;

 until false;
end;

function _alloc_str(IsFree:Boolean):RawByteString;
begin
 Case IsFree of
  True :Result:='FREE';
  FAlse:Result:='ALLC';
 end;
end;

procedure TPoolManager.Print;
var
 key:TPoolAdrNode;
 It:TAllcPoolNodeSet.Iterator;
begin
 It:=FAllcSet.cbegin;
 While (It.Item<>nil) do
 begin
  key:=It.Item^;

  Writeln(HexStr(key.Offset,10),'..',
          HexStr(key.Offset+key.Size,10),':',
          HexStr(key.Size,10),'#',
          _alloc_str(key.IsFree),'#',
          key.F.Ext);

  It.Next;
 end;
end;

procedure itest;
var
 test:TPoolManager;
 addr:array[0..3] of qword;
begin
 test:=TPoolManager.Create($7FFFFC000,$FFFFFFFFF);

 test.Alloc_any(4*1024,1,0,addr[0]);
 Writeln(HexStr(addr[0],16));

 test.Alloc_any(4*1024,1,0,addr[1]);
 Writeln(HexStr(addr[1],16));

 test.Alloc_any(4*1024,1,0,addr[2]);
 Writeln(HexStr(addr[2],16));

 test.Alloc_any(4*1024,1,0,addr[3]);
 Writeln(HexStr(addr[3],16));

 writeln;
 test.Print;
 writeln;

 test.Release(addr[0],4*1024);
 test.Release(addr[1],4*1024);

 writeln;
 test.Print;
 writeln;

 writeln(test.CheckedRelease(addr[1],4*1024*2));

 test.Release(addr[1],4*1024*2);

 //test.Release(addr[0],4*1024);
 //test.Release(addr[2],4*1024);
 //test.Release(addr[1],4*1024);

 //test.Release(addr[0],4*1024);
 //test.Release(addr[1],4*1024);
 //test.Release(addr[2],4*1024);
 //test.Release(addr[2],4*1024);

 //writeln(test.CheckedRelease(addr[1],4*1024));
 //writeln(test.CheckedRelease(addr[2],4*1024));

 writeln(test.CheckedRelease(addr[3]+4*1024,4*1024));
 test.Release(addr[3]+4*1024,4*1024);

 writeln;
 test.Print;
 writeln;

 test.Alloc_any(4*1024,1,0,addr[0]);
 Writeln(HexStr(addr[0],16));

 test.Alloc_any(4*1024,1,0,addr[1]);
 Writeln(HexStr(addr[1],16));

 test.Alloc_any(4*1024,1,0,addr[2]);
 Writeln(HexStr(addr[2],16));

 readln;
end;

initialization
 //itest

end.




