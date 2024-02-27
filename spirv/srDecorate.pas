unit srDecorate;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  ginodes,
  srNode,
  srOp;

type
 PsrHeaderList=^TsrHeaderList;
 TsrHeaderList=object(TsrOpBlockCustom)
  FGLSL_std_450:PSpirvOp;
  function emit_glsl_ext:PSpirvOp;
 end;

 PsrDecorate=^TsrDecorate;
 TsrDecorate=object
  public
   pLeft,pRight:PsrDecorate;
   function  c(n1,n2:PsrDecorate):Integer; static;
  private
   key:packed record
    data:PsrNode;
    param:array[0..2] of DWORD;
   end;
  public
   node:PSpirvOp;
 end;

 TfemOp=(
  foDepthReplacing,
  foDepthGreater,
  foDepthLess,
  foDepthUnchanged
 );

 TfemOpSet=Set of TfemOp;

 PsrDecorateList=^TsrDecorateList;
 TsrDecorateList=object(TsrOpBlockCustom)
  type
   TNodeFetch=specialize TNodeFetch<PsrDecorate,TsrDecorate>;
  var
   FNTree:TNodeFetch;
   FfemOpSet:TfemOpSet;
  function  Fetch(data:PsrNode;param1,param2,param3:DWORD):PsrDecorate;
  procedure OpDecorate(Data:PsrNode;dec_id,param:DWORD);
  procedure OpMember  (Data:PsrNode;index,offset:DWORD);
 end;

 PsrDebugInfoList=^TsrDebugInfoList;
 TsrDebugInfoList=object(TsrOpBlockCustom)
  procedure OpSource(const n:RawByteString);
  procedure OpName  (Data:PsrNode;const n:RawByteString);
  function  OpString(const n:RawByteString):PsrNode;
 end;

implementation

//

function TsrHeaderList.emit_glsl_ext:PSpirvOp;
begin
 if (FGLSL_std_450=nil) then
 begin
  FGLSL_std_450:=AddSpirvOp(Op.OpExtInstImport);
  FGLSL_std_450^.pDst:=Emit.NewRefNode;
  FGLSL_std_450^.AddString('GLSL.std.450');
 end;
 Result:=FGLSL_std_450;
end;

//

function TsrDecorate.c(n1,n2:PsrDecorate):Integer;
var
 i:Byte;
begin
 //first data
 Result:=Integer(n1^.key.data>n2^.key.data)-Integer(n1^.key.data<n2^.key.data);
 if (Result<>0) then Exit;

 //param[i]
 For i:=0 to 2 do
 begin
  Result:=Integer(n1^.key.param[i]>n2^.key.param[i])-Integer(n1^.key.param[i]<n2^.key.param[i]);
  if (Result<>0) then Exit;
 end;
end;

//

function TsrDecorateList.Fetch(data:PsrNode;param1,param2,param3:DWORD):PsrDecorate;
var
 node:TsrDecorate;
begin
 node:=Default(TsrDecorate);
 node.key.data:=data;
 node.key.param[0]:=param1;
 node.key.param[1]:=param2;
 node.key.param[2]:=param3;

 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=Emit.Alloc(SizeOf(TsrDecorate));
  Move(node,Result^,SizeOf(TsrDecorate));
  FNTree.Insert(Result);
 end;
end;

procedure TsrDecorateList.OpDecorate(Data:PsrNode;dec_id,param:DWORD);
var
 deco:PsrDecorate;
 node:PSpirvOp;
begin
 deco:=Fetch(Data,Op.OpDecorate,dec_id,param);
 if (deco^.node<>nil) then Exit;

 node:=AddSpirvOp(Op.OpDecorate);
 node^.AddParam(Data);
 node^.AddLiteral(dec_id,Decoration.GetStr(dec_id));

 Case dec_id of
  Decoration.BuiltIn:
   node^.AddLiteral(param,BuiltIn.GetStr(param));

  Decoration.ArrayStride,
  Decoration.MatrixStride,
  Decoration.Location,
  Decoration.Index,
  Decoration.Binding,
  Decoration.DescriptorSet:
   node^.AddLiteral(param);

  else;
 end;

 deco^.node:=node;
end;

procedure TsrDecorateList.OpMember(Data:PsrNode;index,offset:DWORD);
var
 deco:PsrDecorate;
 node:PSpirvOp;
begin
 deco:=Fetch(Data,Op.OpMemberDecorate,index,offset);
 if (deco^.node<>nil) then Exit;

 node:=AddSpirvOp(Op.OpMemberDecorate);
 node^.AddParam(Data);
 node^.AddLiteral(index);
 node^.AddLiteral(Decoration.Offset,Decoration.GetStr(Decoration.Offset));
 node^.AddLiteral(offset);

 deco^.node:=node;
end;

//

procedure TsrDebugInfoList.OpSource(const n:RawByteString);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpSourceExtension);
 node^.AddString(n);
end;

procedure TsrDebugInfoList.OpName(Data:PsrNode;const n:RawByteString);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpName);
 node^.AddParam(Data);
 node^.AddString(n);
end;

function TsrDebugInfoList.OpString(const n:RawByteString):PsrNode;
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpString);
 node^.pDst:=Emit.NewRefNode;
 node^.AddString(n);
 Result:=node^.pDst;
end;

//

end.

