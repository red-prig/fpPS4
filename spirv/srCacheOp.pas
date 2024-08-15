unit srCacheOp;

{$mode objfpc}{$H+}

interface

uses
 ginodes,
 srNode,
 srCFGLabel,
 srType,
 srReg,
 srOp;

type
 PsrCacheOpKey=^TsrCacheOpKey;
 TsrCacheOpKey=packed record
  place:TsrOpBlock;
  OpId :DWORD;
  dtype:TsrDataType;
  count:DWORD;
  pData:PPsrRegNode;
 end;

 TsrCacheOp=class
  public
   pLeft,pRight:TsrCacheOp;
   class function c(n1,n2:PsrCacheOpKey):Integer; static;
  private
   key:TsrCacheOpKey;
  public
   pDst:TsrNode;
 end;

 PsrCacheOpList=^TsrCacheOpList;
 TsrCacheOpList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrCacheOp>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(place:TsrOpBlock;OpId:DWORD;rtype:TsrDataType;count:Byte;src:PPsrRegNode):TsrCacheOp;
 end;

function  _up_to_real(t:TsrOpBlock):TsrOpBlock;

implementation

function _up_to_real(t:TsrOpBlock):TsrOpBlock;
begin
 repeat
  if not t.IsType(ntOpBlock) then Break;
  Case t.Block.bType of
   btMain,
   btCond,
   btLoop:Break;
   else;
  end;
  t:=t.Parent;
 until false;
 Result:=t;
end;

//--

class function TsrCacheOp.c(n1,n2:PsrCacheOpKey):Integer;
begin
 //place (not need order sort)
 Result:=ord(ptruint(n1^.place)>ptruint(n2^.place))-ord(ptruint(n1^.place)<ptruint(n2^.place));
 if (Result<>0) then Exit;
 //OpId
 Result:=ord(n1^.OpId>n2^.OpId)-ord(n1^.OpId<n2^.OpId);
 if (Result<>0) then Exit;
 //dtype
 Result:=ord(n1^.dtype>n2^.dtype)-ord(n1^.dtype<n2^.dtype);
 if (Result<>0) then Exit;
 //count
 Result:=ord(n1^.count>n2^.count)-ord(n1^.count<n2^.count);
 if (Result<>0) then Exit;
 //(not need order sort)
 Result:=ComparePtruint(PPtruint(n1^.pData),PPtruint(n2^.pData),n1^.count);
end;

Procedure TsrCacheOpList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrCacheOpList.Fetch(place:TsrOpBlock;OpId:DWORD;rtype:TsrDataType;count:Byte;src:PPsrRegNode):TsrCacheOp;
var
 size:ptruint;
 key:TsrCacheOpKey;
begin
 Assert(place<>nil);
 place:=_up_to_real(place);
 Result:=nil;
 //
 key:=Default(TsrCacheOpKey);
 key.place:=place;
 key.OpId :=OpId;
 key.dtype:=rtype;
 key.count:=count;
 key.pData:=src;
 //
 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrCacheOp>;
  Result.key:=key;

  size:=count*SizeOf(Pointer);
  Result.key.pData:=FEmit.Alloc(size);
  Move(src^,Result.key.pData^,size);

  FTree.Insert(Result);
 end;
end;

end.

