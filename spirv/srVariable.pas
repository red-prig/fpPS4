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
 TsrVariable=class(TsrNode)
  public
   pPrev,pNext:TsrVariable;
  private
   ID:TsrRefId;      //post id
   FType:TsrType;
   FSource:TsrNode;  //ntInput,ntVertLayout,ntFragLayout,ntUniform,ntBuffer,ntOutput
   procedure SetType(t:TsrType);
   procedure SetSource(t:TsrNode);
  public
   //
   Procedure _zero_read      ;                     override;
   Procedure _zero_unread    ;                     override;
   Function  _GetStorageClass:DWORD;               override;
   function  _GetStorageName :RawByteString;       override;
   function  _Down           :TsrNode;             override;
   function  _Next           :TsrNode;             override;
   function  _Prev           :TsrNode;             override;
   Procedure _PrepType       (node:PPrepTypeNode); override;
   Function  _GetPtype       :TsrNode;             override;
   function  _GetPrintName   :RawByteString;       override;
   function  _GetRef         :Pointer;             override;
   //
   property  pSource:TsrNode read FSource write SetSource;
   procedure UpdateType();
   function  GetPrintName:RawByteString;
 end;

 ntVariable=TsrVariable;

 PsrVariableList=^TsrVariableList;
 TsrVariableList=object
  type
   TNodeList=specialize TNodeListClass<TsrVariable>;
  var
   FEmit:TCustomEmit;
   FList:TNodeList;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch:TsrVariable;
  function  First:TsrVariable; inline;
  procedure AllocName;
 end;

operator := (i:TsrNode):TsrVariable; inline;

implementation

operator := (i:TsrNode):TsrVariable; inline;
begin
 Result:=TsrVariable(Pointer(i)); //typecast hack
end;

Procedure TsrVariable._zero_read;
begin
 FType.mark_read(Self);
end;

Procedure TsrVariable._zero_unread;
begin
 FType.mark_unread(Self);
end;

Function TsrVariable._GetStorageClass:DWORD;
begin
 Result:=inherited;
 if (FSource<>nil) then
 if (FSource.ntype<>nil) then
 begin
  Result:=FSource._GetStorageClass;
 end;
end;

function TsrVariable._GetStorageName:RawByteString;
begin
 Result:=inherited;
 if (FSource<>nil) then
 if (FSource.ntype<>nil) then
 begin
  Result:=FSource._GetStorageName;
 end;
end;

function TsrVariable._Down:TsrNode;
begin
 Result:=FSource;
end;

function TsrVariable._Next:TsrNode;
begin
 Result:=pNext;
end;

function TsrVariable._Prev:TsrNode;
begin
 Result:=pPrev;
end;

Procedure TsrVariable._PrepType(node:PPrepTypeNode);
begin
 node^.dnode:=TsrVariable(node^.dnode).FSource;
end;

Function TsrVariable._GetPtype:TsrNode;
begin
 Result:=FType;
end;

function TsrVariable._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrVariable._GetRef:Pointer;
begin
 Result:=@ID;
end;

//

procedure TsrVariable.SetType(t:TsrType);
begin
 if (FType=t) then Exit;
 if IsUsed then
 begin
      t.mark_read  (Self);
  FType.mark_unread(Self);
 end;
 FType:=t;
end;

procedure TsrVariable.UpdateType();
var
 _Type:TsrType;
 pTypeList:PsrTypeList;
begin
 if (Self=nil) then Exit;
 _Type:=FSource.pType;
 if (_Type=nil) then
 begin
  SetType(nil);
 end else
 begin
  pTypeList:=Emit.GetTypeList;
  _Type:=pTypeList^.FetchPointer(_Type,FSource.GetStorageClass);
  SetType(_Type);
 end;
end;

procedure TsrVariable.SetSource(t:TsrNode);
begin
 if (FSource=t) then Exit;
 if IsUsed then
 begin
        t.mark_read  (Self);
  FSource.mark_unread(Self);
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

function TsrVariableList.Fetch:TsrVariable;
begin
 Result:=FEmit.specialize New<TsrVariable>;
 FList.Push_tail(Result);
end;

function TsrVariableList.First:TsrVariable; inline;
begin
 Result:=FList.pHead;
end;

procedure TsrVariableList.AllocName;
var
 FDebugInfo:PsrDebugInfoList;
 node:TsrVariable;
 n:RawByteString;
begin
 FDebugInfo:=FEmit.GetDebugInfoList;
 node:=First;
 While (node<>nil) do
 begin
  if (node.pType<>nil) then
  begin
   n:=node.GetStorageName;
   if (n<>'') then
   begin
    FDebugInfo^.OpName(node,n);
   end;
  end;
  node:=node.Next;
 end;
end;

end.

