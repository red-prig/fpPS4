unit srNode;

{$mode objfpc}{$H+}

interface

uses
 ginodes;

type
 TsrNode=class;
 PPsrNode=^TsrNode;

 TDependenceNode=class
  pLeft,pRight:TDependenceNode;
  //
  key:TsrNode;
  fread_count :DWORD;
  fwrite_count:DWORD;
  //
  class function c(n1,n2:PPsrNode):Integer; static;
  //
  property pNode:TsrNode         read key    write key;
  property pPrev:TDependenceNode read pLeft  write pLeft;
  property pNext:TDependenceNode read pRight write pRight;
 end;
 TDependenceNodeList=specialize TNodeListClass<TDependenceNode>;
 TDependenceNodeTree=specialize TNodeTreeClass<TDependenceNode>;

 PPrepTypeNode=^TPrepTypeNode;
 TPrepTypeNode=record
  dnode:TsrNode;
  rtype:Integer;
 end;

 TsrNodeType=class of TsrNode;

 TCustomEmit=class;

 TsrNode=class
  private
   fread_count :DWORD;
   fwrite_count:DWORD;
   FOrder      :Ptruint;
   FDependence :TDependenceNodeTree;
   FEmit       :TCustomEmit;
  public
   function  Order:Ptruint;
   property  Emit:TCustomEmit read FEmit;
   //
   function  NewDependence:TDependenceNode;
   procedure FreeDependence(D:TDependenceNode);
   function  FirstDependence:TDependenceNode;
   function  NextDependence(D:TDependenceNode):TDependenceNode;
   //
   Procedure add_read(src:TsrNode);          virtual;
   Procedure rem_read(src:TsrNode);          virtual;
   //
   Procedure add_write(src:TsrNode);         virtual;
   Procedure rem_write(src:TsrNode);         virtual;
   //
   Procedure _mark_read(src:TsrNode);        virtual;
   Procedure _mark_unread(src:TsrNode);      virtual;
   //
   Procedure _zero_read;                     virtual;
   Procedure _zero_unread;                   virtual;
   //
   Procedure _mark_write(src:TsrNode);       virtual;
   Procedure _mark_unwrite(src:TsrNode);     virtual;
   //
   Procedure _SetWriter(w,line:TsrNode);     virtual;
   Procedure _ResetWriter(w:TsrNode);        virtual;
   Function  _is_used:Boolean                virtual;
   Function  _GetPtype:TsrNode;              virtual;
   Function  _GetStorageClass:DWORD;         virtual;
   function  _GetStorageName:RawByteString;  virtual;
   function  _GetPrintName:RawByteString;    virtual;
   function  _GetPrintData:RawByteString;    virtual;
   function  _Down:TsrNode;                  virtual;
   function  _Next:TsrNode;                  virtual;
   function  _Prev:TsrNode;                  virtual;
   function  _Parent:TsrNode;                virtual;
   function  _First:TsrNode;                 virtual;
   function  _Last:TsrNode;                  virtual;
   Procedure _PrepType(node:PPrepTypeNode);  virtual;
   //
   function  _GetIndexCount:DWORD;           virtual;
   //
   function  _GetRef:Pointer;                virtual;
   //
   function  _GetData(data:Pointer):Ptruint; virtual;
   //
   function    ntype:TsrNodeType;
   property    read_count :DWORD read fread_count;
   property    write_count:DWORD read fwrite_count;
   function    IsType(t:TsrNodeType):Boolean;
   generic function AsType<T>:T;
   Procedure   mark_read   (src:TsrNode);
   Procedure   mark_unread (src:TsrNode);
   Procedure   mark_write  (src:TsrNode);
   Procedure   mark_unwrite(src:TsrNode);
   Procedure   SetWriter(w,line:TsrNode);
   Procedure   ResetWriter(w:TsrNode);
   function    IsUsed:Boolean;
   Function    pType:TsrNode;
   Function    GetStorageClass:DWORD;
   function    GetStorageName:RawByteString;
   function    GetPrintName:RawByteString;
   function    GetPrintData:RawByteString;
   function    Next:TsrNode;
   function    Prev:TsrNode;
   function    Parent:TsrNode;
   function    First:TsrNode;
   function    Last:TsrNode;
   procedure   PrepType(rtype:Integer);
   function    GetIndexCount:DWORD;
   function    GetRef:Pointer;
   function    GetData(data:Pointer):Ptruint;
 end;

 TCustomEmit=class
  private
   FOrder:Ptruint;
   FDependenceCache:TDependenceNodeList;
  public
  //
  Function  Alloc(Size:ptruint):Pointer;                   virtual abstract;
  generic Function New<T>:T;
  Function  GetDmem(P:Pointer) :Pointer;                   virtual abstract;
  Function  GetExecutionModel  :Word;                      virtual abstract;
  Function  GetConfig          :Pointer;                   virtual abstract;
  Function  GetCodeHeap        :Pointer;                   virtual abstract;
  Function  GetLiteralList     :Pointer;                   virtual abstract;
  Function  GetTypeList        :Pointer;                   virtual abstract;
  Function  GetConstList       :Pointer;                   virtual abstract;
  Function  GetRegsStory       :Pointer;                   virtual abstract;
  Function  GetCapabilityList  :Pointer;                   virtual abstract;
  Function  GetHeaderList      :TsrNode;                   virtual abstract;
  Function  GetDecorateList    :TsrNode;                   virtual abstract;
  Function  GetDebugInfoList   :TsrNode;                   virtual abstract;
  Function  GetVariableList    :Pointer;                   virtual abstract;
  Function  GetInputList       :Pointer;                   virtual abstract;
  Function  GetOutputList      :Pointer;                   virtual abstract;
  Function  GetDataLayoutList  :Pointer;                   virtual abstract;
  Function  GetVertLayoutList  :Pointer;                   virtual abstract;
  Function  GetFragLayoutList  :Pointer;                   virtual abstract;
  Function  GetBufferList      :Pointer;                   virtual abstract;
  Function  GetUniformList     :Pointer;                   virtual abstract;
  Function  GetBitcastList     :Pointer;                   virtual abstract;
  Function  GetCacheOpList     :Pointer;                   virtual abstract;
  Function  GetFuncList        :Pointer;                   virtual abstract;
  Function  GetCursor          :Pointer;                   virtual abstract;
  function  curr_line          :TsrNode;                   virtual abstract;
  function  init_line          :TsrNode;                   virtual abstract;
  function  NewRefNode         :TsrNode;                   virtual abstract;
  function  OpCast  (pLine,      dst,src:TsrNode):TsrNode; virtual abstract;
  function  OpLoad  (pLine,dtype,dst,src:TsrNode):TsrNode; virtual abstract;
  function  OpStore (pLine,      dst,src:TsrNode):TsrNode; virtual abstract;
  procedure PostLink(pLine,      dst:TsrNode);             virtual abstract;
  procedure emit_spi;                                      virtual abstract;
 end;

function CompareNodes(buf1,buf2:PPsrNode;count:PtrUint):Integer;

implementation

class function TDependenceNode.c(n1,n2:PPsrNode):Integer;
begin
 Result:=ord(ptruint(n1^)>ptruint(n2^))-ord(ptruint(n1^)<ptruint(n2^));
end;

function TsrNode.Order:Ptruint;
begin
 Result:=0;
 if (Self<>nil) then
 begin
  Result:=FOrder;
 end;
end;

function TsrNode.NewDependence:TDependenceNode;
begin
 if (Self=nil) then Exit(nil);
 Result:=FEmit.FDependenceCache.Pop_tail;
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TDependenceNode>;
 end;
end;

procedure TsrNode.FreeDependence(D:TDependenceNode);
begin
 if (Self=nil) then Exit;
 if (D=nil) then Exit;
 D.pNode:=nil;
 FEmit.FDependenceCache.Push_head(D);
end;

function TsrNode.FirstDependence:TDependenceNode;
begin
 Result:=FDependence.Min;
end;

function TsrNode.NextDependence(D:TDependenceNode):TDependenceNode;
begin
 Result:=FDependence.Next(D);
end;

Procedure TsrNode.add_read(src:TsrNode);
Var
 D:TDependenceNode;
begin
 D:=FDependence.Find(@src);
 if (D=nil) then
 begin
  D:=NewDependence;
  //
  D.pNode:=src;
  FDependence.Insert(D);
 end;

 Inc(D.fread_count);
 Inc(fread_count);
end;

Procedure TsrNode.rem_read(src:TsrNode);
Var
 D:TDependenceNode;
begin
 Assert(fread_count<>0,'zero read deref:'+ntype.ClassName);
 //
 D:=FDependence.Find(@src);
 if (D<>nil) then
 begin
  Assert(D.fread_count<>0,'zero read deref:'+ntype.ClassName);
  //
  Dec(D.fread_count);
  //
  if (D.fread_count=0) and (D.fwrite_count=0) then
  begin
   FDependence.Delete(D);
   //
   FreeDependence(D);
  end;
  //
  Dec(fread_count);
 end else
 begin
  Assert(false,'unknow read deref:'+ntype.ClassName);
 end;
end;

Procedure TsrNode.add_write(src:TsrNode);
Var
 D:TDependenceNode;
begin
 D:=FDependence.Find(@src);
 if (D=nil) then
 begin
  D:=NewDependence;
  //
  D.pNode:=src;
  FDependence.Insert(D);
 end;

 Inc(D.fwrite_count);
 Inc(fwrite_count);
end;

Procedure TsrNode.rem_write(src:TsrNode);
Var
 D:TDependenceNode;
begin
 Assert(fwrite_count<>0,'zero write deref:'+ntype.ClassName);
 //
 D:=FDependence.Find(@src);
 if (D<>nil) then
 begin
  Assert(D.fwrite_count<>0,'zero write deref:'+ntype.ClassName);
  //
  Dec(D.fwrite_count);
  //
  if (D.fread_count=0) and (D.fwrite_count=0) then
  begin
   FDependence.Delete(D);
   //
   FreeDependence(D);
  end;
  //
  Dec(fwrite_count);
 end else
 begin
  Assert(false,'unknow write deref:'+ntype.ClassName);
 end;
end;

Procedure TsrNode._mark_read(src:TsrNode);
var
 node:TsrNode;
 prv:Boolean;
begin
 node:=Self;
 repeat
  //Assert(node^.fntype<>nil);

  prv:=node.IsUsed;

  node.add_read(src);

  if (prv=node.IsUsed) then Exit;

  node._zero_read;

  src :=node;
  node:=node._Down;
 until (node=nil);
end;

Procedure TsrNode._mark_unread(src:TsrNode);
var
 node:TsrNode;
 prv:Boolean;
begin
 node:=Self;
 repeat
  //Assert(node^.fntype<>nil);

  prv:=node.IsUsed;

  node.rem_read(src);

  if (prv=node.IsUsed) then Exit;

  node._zero_unread;

  src :=node;
  node:=node._Down;
 until (node=nil);
end;

Procedure TsrNode._zero_read;
begin
 //
end;

Procedure TsrNode._zero_unread;
begin
 //
end;

Procedure TsrNode._mark_write(src:TsrNode);
var
 node:TsrNode;
 prv:Boolean;
begin
 node:=Self;
 repeat
  //Assert(node^.fntype<>nil);

  prv:=node.IsUsed;

  node.add_write(src);

  if (prv=node.IsUsed) then Exit;

  node._zero_read;

  src :=node;
  node:=node._Down;
 until (node=nil);
end;

Procedure TsrNode._mark_unwrite(src:TsrNode);
var
 node:TsrNode;
 prv:Boolean;
begin
 node:=Self;
 repeat
  //Assert(node^.fntype<>nil);

  prv:=node.IsUsed;

  node.rem_write(src);

  if (prv=node.IsUsed) then Exit;

  node._zero_unread;

  src :=node;
  node:=node._Down;
 until (node=nil);
end;

Procedure TsrNode._SetWriter(w,line:TsrNode);
begin
 w.mark_read(Self);
end;

Procedure TsrNode._ResetWriter(w:TsrNode);
begin
 w.mark_unread(Self);
end;

function TsrNode._is_used:Boolean;
begin
 Result:=(read_count<>0) or (write_count<>0);
end;

function TsrNode._GetPtype:TsrNode;
begin
 Result:=nil;
end;

function TsrNode._GetStorageClass:DWORD;
begin
 Result:=6;
end;

function TsrNode._GetStorageName:RawByteString;
begin
 Result:='';
end;

function TsrNode._GetPrintName:RawByteString;
begin
 Result:='';
end;

function TsrNode._GetPrintData:RawByteString;
begin
 Result:='';
end;

function TsrNode._Down:TsrNode;
begin
 Result:=nil;
end;

function TsrNode._Next:TsrNode;
begin
 Result:=nil;
end;

function TsrNode._Prev:TsrNode;
begin
 Result:=nil;
end;

function TsrNode._Parent:TsrNode;
begin
 Result:=nil;
end;

function TsrNode._First:TsrNode;
begin
 Result:=nil;
end;

function TsrNode._Last:TsrNode;
begin
 Result:=nil;
end;

Procedure TsrNode._PrepType(node:PPrepTypeNode);
begin
 node^.dnode:=nil;
end;

function TsrNode._GetIndexCount:DWORD;
begin
 Result:=0;
end;

function TsrNode._GetRef:Pointer;
begin
 Result:=nil;
end;

function TsrNode._GetData(data:Pointer):Ptruint;
begin
 Result:=0;
end;

//
function TsrNode.ntype:TsrNodeType;
begin
 Result:=TsrNodeType(ClassType);
end;

function TsrNode.IsType(t:TsrNodeType):Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 Result:=InheritsFrom(t);
end;

generic function TsrNode.AsType<T>:T;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 if not InheritsFrom(T) then Exit;
 Result:=T(Self);
end;

Procedure TsrNode.mark_read(src:TsrNode);
begin
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 _mark_read(src);
end;

Procedure TsrNode.mark_unread(src:TsrNode);
begin
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 _mark_unread(src);
end;

Procedure TsrNode.mark_write(src:TsrNode);
begin
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 _mark_write(src);
end;

Procedure TsrNode.mark_unwrite(src:TsrNode);
begin
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 _mark_unwrite(src);
end;

Procedure TsrNode.SetWriter(w,line:TsrNode);
begin
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 _SetWriter(w,line);
end;

Procedure TsrNode.ResetWriter(w:TsrNode);
begin
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 _ResetWriter(w);
end;

function TsrNode.IsUsed:Boolean;
begin
 Result:=False;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_is_used;
end;

Function TsrNode.pType:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetPtype;
end;

Function TsrNode.GetStorageClass:DWORD;
begin
 Result:=6;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetStorageClass;
end;

function TsrNode.GetStorageName:RawByteString;
begin
 Result:='';
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetStorageName;
end;

function TsrNode.GetPrintName:RawByteString;
begin
 Result:='';
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetPrintName;
end;

function TsrNode.GetPrintData:RawByteString;
begin
 Result:='';
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetPrintData;
end;

function TsrNode.Next:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_Next;
end;

function TsrNode.Prev:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_Prev;
end;

function TsrNode.Parent:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_Parent;
end;

function TsrNode.First:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_First;
end;

function TsrNode.Last:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_Last;
end;

procedure TsrNode.PrepType(rtype:Integer);
var
 node:TPrepTypeNode;
begin
 node:=Default(TPrepTypeNode);
 node.dnode:=Self;
 node.rtype:=rtype;
 While (node.dnode<>nil) do
 begin
  //Assert(node.dnode^.fntype<>nil);
  node.dnode._PrepType(@node);
 end;
end;

function TsrNode.GetIndexCount:DWORD;
begin
 Result:=0;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetIndexCount;
end;

function TsrNode.GetRef:Pointer;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetRef;
end;

function TsrNode.GetData(data:Pointer):Ptruint;
begin
 Result:=0;
 if (Self=nil) then Exit;
 //Assert(fntype<>nil);
 Result:=_GetData(data);
end;

//

generic Function TCustomEmit.New<T>:T;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 //
 Result:=T(Alloc(TObject(T).InstanceSize));
 TObject(T).InitInstance(Result);
 //
 if TObject(T).InheritsFrom(TsrNode) then
 begin
  Inc(FOrder);
  //
  TsrNode(Result).FOrder:=FOrder;
  TsrNode(Result).FEmit :=Self;
 end;
end;

function CompareNodes(buf1,buf2:PPsrNode;count:PtrUint):Integer;
begin
 Result:=0;
 While (count<>0) do
 begin
  Result:=Integer(buf1^.Order>buf2^.Order)-Integer(buf1^.Order<buf2^.Order);
  if (Result<>0) then Exit;
  Inc(buf1);
  Inc(buf2);
  Dec(count);
 end;
end;

end.

