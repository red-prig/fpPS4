unit mm_adr_name;

{$mode ObjFPC}{$H+}

interface

uses
  Windows,
  Classes,
  SysUtils,
  g23tree,
  sys_types;

{
 name node:
 [
  offset 12..39:28
  size   12..39:28

  name[32]
 ]

}

type
 TName=array[0..31] of AnsiChar;

 TNameAdrNode=packed object
  private
   Function  GetOffset:Pointer;
   Procedure SetOffset(q:Pointer);
   Function  GetSize:QWORD;
   Procedure SetSize(q:QWORD);
  public
   F:bitpacked record
    Offset:DWORD;
    Size  :DWORD;
   end;
   name:TName;
   property Offset:Pointer read GetOffset write SetOffset;
   property Size:QWORD     read GetSize   write SetSize;
   Function cmp_merge(const n:TNameAdrNode):Boolean;
 end;

type
 TNamedAdrAllcCompare=object
  function c(const a,b:TNameAdrNode):Integer; static;
 end;

 TNamedManager=class
  private
   type
    TAllcPoolNodeSet=specialize T23treeSet<TNameAdrNode,TNamedAdrAllcCompare>;

   var
    Flo,Fhi:Pointer;
    FMaxSize:QWORD;

    FAllcSet:TAllcPoolNodeSet;
  public
    property lo:Pointer read Flo;
    property hi:Pointer read Fhi;

    Constructor Create(_lo,_hi:QWORD);
  private
    procedure _Insert(const key:TNameAdrNode);
    Function  _FetchNode_m(mode:Byte;cmp:Pointer;var R:TNameAdrNode):Boolean;

    procedure _Merge(key:TNameAdrNode);
    procedure _Devide(Offset:Pointer;Size:QWORD;var key:TNameAdrNode);
  public
    Function  Mname(Offset:Pointer;Size:QWORD;pname:PChar):Integer;
    Function  Query(Offset:Pointer;pname:PChar):Integer;
 end;

implementation

const
 EINVAL=22;

//

function TNamedAdrAllcCompare.c(const a,b:TNameAdrNode):Integer;
begin
 //1 FOffset
 Result:=Integer(a.F.Offset>b.F.Offset)-Integer(a.F.Offset<b.F.Offset);
end;

//

function Max(a,b:Pointer):Pointer; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

//

Function TNameAdrNode.GetOffset:Pointer;
begin
 Result:=Pointer(QWORD(F.Offset) shl 12);
end;

Procedure TNameAdrNode.SetOffset(q:Pointer);
begin
 F.Offset:=DWORD(QWORD(q) shr 12);
 Assert(GetOffset=q);
end;

Function TNameAdrNode.GetSize:QWORD;
begin
 Result:=QWORD(F.Size) shl 12;
end;

Procedure TNameAdrNode.SetSize(q:QWORD);
begin
 F.Size:=DWORD(q shr 12);
 Assert(GetSize=q);
end;

Function TNameAdrNode.cmp_merge(const n:TNameAdrNode):Boolean;
begin
 Result:=False;
 if (CompareChar0(name,n.name,SizeOf(TName))<>0) then Exit;
 Result:=True;
end;

///

Constructor TNamedManager.Create(_lo,_hi:QWORD);
var
 key:TNameAdrNode;
begin
 Assert(_lo<_hi);

 Flo:=Pointer(_lo);
 Fhi:=Pointer(_hi);

 FMaxSize:=(_hi-_lo+1);

 key:=Default(TNameAdrNode);
 key.Offset:=Pointer(_lo);
 key.Size  :=FMaxSize;

 _Insert(key);
end;

procedure TNamedManager._Insert(const key:TNameAdrNode);
begin
 Assert(key.Size<>0);
 FAllcSet.Insert(key);
end;

const
 M_LE=0;
 M_BE=1;

 C_UP=2;
 C_DW=4;

 C_LE=6;
 C_BE=8;

Function TNamedManager._FetchNode_m(mode:Byte;cmp:Pointer;var R:TNameAdrNode):Boolean;
var
 It:TAllcPoolNodeSet.Iterator;
 key,rkey:TNameAdrNode;
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

 Case (mode and (not 1)) of
  C_UP:
       begin
        if not rkey.cmp_merge(key)        then Exit;
        if ((rkey.Offset+rkey.Size)<>cmp) then Exit;
       end;
  C_DW:
       begin
        if not rkey.cmp_merge(key) then Exit;
        if (rkey.Offset <>cmp    ) then Exit;
       end;

  C_LE:if ((rkey.Offset+rkey.Size)<=cmp) then Exit;
  C_BE:if (rkey.Offset<=cmp) then Exit;

  else
       Exit;
 end;

 R:=rkey;
 FAllcSet.erase(It);
 Result:=True;
end;

//

procedure TNamedManager._Merge(key:TNameAdrNode);
var
 rkey:TNameAdrNode;
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

procedure TNamedManager._Devide(Offset:Pointer;Size:QWORD;var key:TNameAdrNode);
var
 FOffset:Pointer;
 FSize:QWORD;
 FEndN,FEndO:Pointer;
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
 key.Offset :=Offset;
 key.Size   :=Size;
end;

Function TNamedManager.Mname(Offset:Pointer;Size:QWORD;pname:PChar):Integer;
var
 key:TNameAdrNode;
 FEndN,FEndO:Pointer;
 FSize:QWORD;
 name:TName;

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
  key.name:=name;
  _Merge(key);

  if (FEndO>=FEndN) then Exit(True);

  FSize:=FEndO-Offset;

  Offset:=Offset+FSize;
  Size  :=Size  -FSize;
 end;

begin
 Result:=0;
 if (Size=0) or (Size>FMaxSize) then Exit(EINVAL);
 if (Offset<Flo) or (Offset>Fhi) then Exit(EINVAL);

 FEndO:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);
 Size:=Size+(Offset-FEndO);

 Offset:=FEndO;
 Size:=AlignUp(Size,PHYSICAL_PAGE_SIZE);

 name:=Default(TName);
 if (pname<>nil) then
 begin
  MoveChar0(pname^,name,SizeOf(TName));
 end;

 repeat

  key:=Default(TNameAdrNode);
  key.Offset:=Offset;

  if _fetch then
  begin
   if _map then Break;
  end else
  begin
   Result:=EINVAL;
   Break;
  end;

 until false;
end;

Function TNamedManager.Query(Offset:Pointer;pname:PChar):Integer;
var
 It:TAllcPoolNodeSet.Iterator;
 key:TNameAdrNode;
begin
 Result:=0;

 if (pname=nil) then Exit(EINVAL);

 Offset:=AlignDw(Offset,PHYSICAL_PAGE_SIZE);

 key:=Default(TNameAdrNode);
 key.Offset:=Offset;

 It:=FAllcSet.find_le(key);

 if (It.Item=nil) then Exit(EINVAL);

 key:=It.Item^;

 if (Offset>=(key.Size+key.Offset)) then Exit(EINVAL);

 MoveChar0(key.name,pname^,SizeOf(TName));
end;

initialization

end.




