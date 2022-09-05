unit srVariable;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 ginodes,
 srNode,
 srTypes,
 srRefId,
 srDecorate;

type
 ntVariable=class(TsrNodeVmt)
  class Procedure zero_read      (node:PsrNode);               override;
  class Procedure zero_unread    (node:PsrNode);               override;
  class Function  pwrite_count   (node:PsrNode):PDWORD;        override;
  class Function  GetStorageClass(node:PsrNode):DWORD;         override;
  class function  GetStorageName (node:PsrNode):RawByteString; override;
  class function  Down           (node:PsrNode):Pointer;       override;
  class function  Next           (node:PsrNode):Pointer;       override;
  class function  Prev           (node:PsrNode):Pointer;       override;
  class Procedure PrepType       (node:PPrepTypeNode);         override;
  class Function  GetPtype       (node:PsrNode):PsrNode;       override;
  class function  GetPrintName   (node:PsrNode):RawByteString; override;
  class function  GetRef         (node:PsrNode):Pointer;       override;
 end;

 PsrVariable=^TsrVariable;
 TsrVariable=packed object(TsrNode)
  private
   pPrev,pNext:PsrVariable;
   //
   fwrite_count:DWORD;
   ID:TsrRefId;      //post id
   FType:PsrType;
   FSource:PsrNode;  //ntInput,ntVertLayout,ntFragLayout,ntUniform,ntBuffer,ntOutput
   procedure SetType(t:PsrType);
   procedure SetSource(t:PsrNode);
  public
   property  pSource:PsrNode read FSource write SetSource;
   procedure UpdateType(Emit:TCustomEmit);
   Procedure Init; inline;
   function  GetPrintName:RawByteString;
 end;

 PsrVariableList=^TsrVariableList;
 TsrVariableList=object
  type
   TNodeList=specialize TNodeList<PsrVariable>;
  var
   FEmit:TCustomEmit;
   FList:TNodeList;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch:PsrVariable;
  function  First:PsrVariable; inline;
  procedure AllocName;
 end;

implementation

class Procedure ntVariable.zero_read(node:PsrNode);
begin
 With PsrVariable(node)^ do
 begin
  FType^.mark_read(@Self);
 end;
end;

class Procedure ntVariable.zero_unread(node:PsrNode);
begin
 With PsrVariable(node)^ do
 begin
  FType^.mark_unread(@Self);
 end;
end;

class Function ntVariable.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrVariable(node)^.fwrite_count;
end;

class Function ntVariable.GetStorageClass(node:PsrNode):DWORD;
begin
 Result:=inherited;
 With PsrVariable(node)^ do
 if (FSource<>nil) then
 if (FSource^.ntype<>nil) then
 begin
  Result:=FSource^.ntype.GetStorageClass(FSource);
 end;
end;

class function ntVariable.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=inherited;
 With PsrVariable(node)^ do
 if (FSource<>nil) then
 if (FSource^.ntype<>nil) then
 begin
  Result:=FSource^.ntype.GetStorageName(FSource);
 end;
end;

class function ntVariable.Down(node:PsrNode):Pointer;
begin
 Result:=PsrVariable(node)^.FSource;
end;

class function ntVariable.Next(node:PsrNode):Pointer;
begin
 Result:=PsrVariable(node)^.pNext;
end;

class function ntVariable.Prev(node:PsrNode):Pointer;
begin
 Result:=PsrVariable(node)^.pPrev;
end;

class Procedure ntVariable.PrepType(node:PPrepTypeNode);
begin
 node^.dnode:=PsrVariable(node^.dnode)^.FSource;
end;

class Function ntVariable.GetPtype(node:PsrNode):PsrNode;
begin
 Result:=PsrVariable(node)^.FType;
end;

class function ntVariable.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrVariable(node)^.GetPrintName;
end;

class function ntVariable.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PsrVariable(node)^.ID;
end;

//

Procedure TsrVariable.Init; inline;
begin
 fntype:=ntVariable;
end;

procedure TsrVariable.SetType(t:PsrType);
begin
 if (FType=t) then Exit;
 if IsUsed then
 begin
      t^.mark_read  (@Self);
  FType^.mark_unread(@Self);
 end;
 FType:=t;
end;

procedure TsrVariable.UpdateType(Emit:TCustomEmit);
var
 _Type:PsrType;
 pTypeList:PsrTypeList;
begin
 if (@Self=nil) then Exit;
 _Type:=FSource^.pType;
 if (_Type=nil) then
 begin
  SetType(nil);
 end else
 begin
  pTypeList:=Emit.GetTypeList;
  _Type:=pTypeList^.FetchPointer(_Type,FSource^.GetStorageClass);
  SetType(_Type);
 end;
end;

procedure TsrVariable.SetSource(t:PsrNode);
begin
 if (FSource=t) then Exit;
 if IsUsed then
 begin
        t^.mark_read  (@Self);
  FSource^.mark_unread(@Self);
 end;
 FSource:=t;
end;

function TsrVariable.GetPrintName:RawByteString;
begin
 Result:=GetStorageName;
 if (Result='') then
 begin
  Assert(ID.Alloc);
  Result:='v'+IntToStr(ID.ID);
 end;
end;

//

procedure TsrVariableList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrVariableList.Fetch:PsrVariable;
begin
 Result:=FEmit.Alloc(SizeOf(TsrVariable));
 Result^.Init;
 FList.Push_tail(Result);
end;

function TsrVariableList.First:PsrVariable; inline;
begin
 Result:=FList.pHead;
end;

procedure TsrVariableList.AllocName;
var
 FDebugInfo:PsrDebugInfoList;
 node:PsrVariable;
 n:RawByteString;
begin
 FDebugInfo:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  if (node^.pType<>nil) then
  begin
   n:=node^.GetStorageName;
   if (n<>'') then
   begin
    FDebugInfo^.OpName(node,n);
   end;
  end;
  node:=node^.Next;
 end;
end;

end.

