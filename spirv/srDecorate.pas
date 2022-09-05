unit srDecorate;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  srNode,
  srOp;

type
 PsrHeaderList=^TsrHeaderList;
 TsrHeaderList=object(TsrOpBlockCustom)
  FGLSL_std_450:PSpirvOp;
  function emit_glsl_ext:PSpirvOp;
 end;

 PsrDecorateList=^TsrDecorateList;
 TsrDecorateList=object(TsrOpBlockCustom)
  procedure OpDecorate(Data:PsrNode;dec_id,param:DWORD);
  procedure OpMemberDecorate(Data:PsrNode;index,offset:DWORD);
 end;

 PsrDebugInfoList=^TsrDebugInfoList;
 TsrDebugInfoList=object(TsrOpBlockCustom)
  procedure OpSourceExtension(const n:RawByteString);
  procedure OpName(Data:PsrNode;const n:RawByteString);
  function  OpString(const n:RawByteString):PsrNode;
 end;

implementation

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

procedure TsrDecorateList.OpDecorate(Data:PsrNode;dec_id,param:DWORD);
var
 node:PSpirvOp;
begin
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
end;

procedure TsrDecorateList.OpMemberDecorate(Data:PsrNode;index,offset:DWORD);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpMemberDecorate);
 node^.AddParam(Data);
 node^.AddLiteral(index);
 node^.AddLiteral(Decoration.Offset,Decoration.GetStr(Decoration.Offset));
 node^.AddLiteral(offset);
end;

procedure TsrDebugInfoList.OpSourceExtension(const n:RawByteString);
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

end.

