unit srDecorate;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  ginodes,
  srNode,
  srOp;

type
 TsrHeaderList=class(TsrOpBlockCustom)
  FGLSL_std_450:TSpirvOp;
  //
  FSPV_EXT_descriptor_indexing:Boolean;
  FSPV_KHR_workgroup_memory_explicit_layout:Boolean;
  //
  function  OpExtension(const n:RawByteString):TSpirvOp;
  function  OpExtInstImport(const n:RawByteString):TSpirvOp;
  //
  function  GLSL_std_450:TSpirvOp;
  //
  function  AddExecutionMode(Main:TSpirvFunc;mode:PtrUint):TSpirvOp;
  //
  procedure SPV_EXT_descriptor_indexing;
  procedure SPV_KHR_workgroup_memory_explicit_layout;
 end;

 PsrDecorateKey=^TsrDecorateKey;
 TsrDecorateKey=packed record
  data:TsrNode;
  param:array[0..2] of DWORD;
 end;

 TsrDecorate=class
  public
   pLeft,pRight:TsrDecorate;
   class function c(n1,n2:PsrDecorateKey):Integer; static;
  private
   key:TsrDecorateKey;
  public
   node:TSpirvOp;
 end;

 TsrDecorateList=class(TsrOpBlockCustom)
  type
   TNodeTree=specialize TNodeTreeClass<TsrDecorate>;
  var
   FTree:TNodeTree;
  function  Fetch(data:TsrNode;param1,param2,param3:DWORD):TsrDecorate;
  procedure OpDecorate(Data:TsrNode;dec_id,param:DWORD);
  procedure OpMember  (Data:TsrNode;index,offset:DWORD);
 end;

 TsrDebugInfoList=class(TsrOpBlockCustom)
  FFileName:TsrNode;
  //
  procedure OpSource(const n:RawByteString);
  procedure OpName  (Data:TsrNode;const n:RawByteString);
  function  OpString(const n:RawByteString):TsrNode;
  function  FileName:TsrNode;
 end;

operator := (i:TsrNode):TsrHeaderList; inline;
operator := (i:TsrNode):TsrDecorateList; inline;
operator := (i:TsrNode):TsrDebugInfoList; inline;

implementation

operator := (i:TsrNode):TsrHeaderList; inline;
begin
 Result:=TsrHeaderList(Pointer(i)); //typecast hack
end;

operator := (i:TsrNode):TsrDecorateList; inline;
begin
 Result:=TsrDecorateList(Pointer(i)); //typecast hack
end;

operator := (i:TsrNode):TsrDebugInfoList; inline;
begin
 Result:=TsrDebugInfoList(Pointer(i)); //typecast hack
end;

//

function TsrHeaderList.OpExtension(const n:RawByteString):TSpirvOp;
var
 node:TSpirvOp;
begin
 if (FGLSL_std_450<>nil) then
 begin
  node:=NewSpirvOp(Op.OpExtension);
  InsertBefore(FGLSL_std_450,node);
 end else
 begin
  node:=AddSpirvOp(Op.OpExtension);
 end;
 //
 node.AddString(n);
 Result:=node;
end;

function TsrHeaderList.OpExtInstImport(const n:RawByteString):TSpirvOp;
var
 node:TSpirvOp;
begin
 node:=AddSpirvOp(Op.OpExtInstImport);
 node.pDst:=Emit.NewRefNode;
 node.AddString(n);
 Result:=node;
end;

function TsrHeaderList.GLSL_std_450:TSpirvOp;
begin
 if (FGLSL_std_450=nil) then
 begin
  FGLSL_std_450:=OpExtInstImport('GLSL.std.450');
 end;
 Result:=FGLSL_std_450;
end;

function TsrHeaderList.AddExecutionMode(Main:TSpirvFunc;mode:PtrUint):TSpirvOp;
var
 node:TSpirvOp;
begin
 node:=AddSpirvOp(Op.OpExecutionMode);
 node.AddParam(Main);
 node.AddLiteral(mode,ExecutionMode.GetStr(mode));
 Result:=node;
end;

procedure TsrHeaderList.SPV_EXT_descriptor_indexing;
begin
 if not FSPV_EXT_descriptor_indexing then
 begin
  OpExtension('SPV_EXT_descriptor_indexing');
  FSPV_EXT_descriptor_indexing:=True;
 end;
end;

procedure TsrHeaderList.SPV_KHR_workgroup_memory_explicit_layout;
begin
 if not FSPV_KHR_workgroup_memory_explicit_layout then
 begin
  OpExtension('SPV_KHR_workgroup_memory_explicit_layout');
  FSPV_KHR_workgroup_memory_explicit_layout:=True;
 end;
end;

//

class function TsrDecorate.c(n1,n2:PsrDecorateKey):Integer;
var
 i:Byte;
begin
 //first data (order sort)
 Result:=ord(n1^.data.Order>n2^.data.Order)-ord(n1^.data.Order<n2^.data.Order);
 if (Result<>0) then Exit;

 //param[i]
 For i:=0 to 2 do
 begin
  Result:=ord(n1^.param[i]>n2^.param[i])-ord(n1^.param[i]<n2^.param[i]);
  if (Result<>0) then Exit;
 end;
end;

//

function TsrDecorateList.Fetch(data:TsrNode;param1,param2,param3:DWORD):TsrDecorate;
var
 key:TsrDecorateKey;
begin
 key:=Default(TsrDecorateKey);
 key.data:=data;
 key.param[0]:=param1;
 key.param[1]:=param2;
 key.param[2]:=param3;

 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=Emit.specialize New<TsrDecorate>;
  Result.key:=key;
  FTree.Insert(Result);
 end;
end;

procedure TsrDecorateList.OpDecorate(Data:TsrNode;dec_id,param:DWORD);
var
 deco:TsrDecorate;
 node:TSpirvOp;
begin
 deco:=Fetch(Data,Op.OpDecorate,dec_id,param);
 if (deco.node<>nil) then Exit;

 node:=AddSpirvOp(Op.OpDecorate);
 node.AddParam(Data);
 node.AddLiteral(dec_id,Decoration.GetStr(dec_id));

 Case dec_id of
  Decoration.BuiltIn:
   node.AddLiteral(param,BuiltIn.GetStr(param));

  Decoration.ArrayStride,
  Decoration.MatrixStride,
  Decoration.Location,
  Decoration.Index,
  Decoration.Binding,
  Decoration.DescriptorSet:
   node.AddLiteral(param);

  else;
 end;

 deco.node:=node;
end;

procedure TsrDecorateList.OpMember(Data:TsrNode;index,offset:DWORD);
var
 deco:TsrDecorate;
 node:TSpirvOp;
begin
 deco:=Fetch(Data,Op.OpMemberDecorate,index,offset);
 if (deco.node<>nil) then Exit;

 node:=AddSpirvOp(Op.OpMemberDecorate);
 node.AddParam(Data);
 node.AddLiteral(index);
 node.AddLiteral(Decoration.Offset,Decoration.GetStr(Decoration.Offset));
 node.AddLiteral(offset);

 deco.node:=node;
end;

//

procedure TsrDebugInfoList.OpSource(const n:RawByteString);
var
 node:TSpirvOp;
begin
 node:=AddSpirvOp(Op.OpSourceExtension);
 node.AddString(n);
end;

procedure TsrDebugInfoList.OpName(Data:TsrNode;const n:RawByteString);
var
 node:TSpirvOp;
begin
 node:=AddSpirvOp(Op.OpName);
 node.AddParam(Data);
 node.AddString(n);
end;

function TsrDebugInfoList.OpString(const n:RawByteString):TsrNode;
var
 node:TSpirvOp;
begin
 node:=AddSpirvOp(Op.OpString);
 node.pDst:=Emit.NewRefNode;
 node.AddString(n);
 Result:=node.pDst;
end;

function TsrDebugInfoList.FileName:TsrNode;
begin
 if (FFileName=nil) then
 begin
  FFileName:=OpString('GCN');
 end;
 //
 Result:=FFileName;
end;

//

end.

