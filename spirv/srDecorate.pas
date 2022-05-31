unit srDecorate;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  srNodes,
  srOp;

type
 PsrDecorateList=^TsrDecorateList;
 TsrDecorateList=object(TsrOpBlockSimple)
  FDescriptorSet:DWORD;
  procedure emit_decorate(ntype:TsrNodeType;Data:Pointer;dec_id,param:DWORD);
  procedure emit_member_decorate(ntype:TsrNodeType;Data:Pointer;index,offset:DWORD);
 end;

 PsrDebugInfoList=^TsrDebugInfoList;
 TsrDebugInfoList=object(TsrOpBlockSimple)
  procedure emit_source_extension(const n:RawByteString);
  procedure emit_name(ntype:TsrNodeType;Data:Pointer;const n:RawByteString);
 end;

implementation

procedure TsrDecorateList.emit_decorate(ntype:TsrNodeType;Data:Pointer;dec_id,param:DWORD);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpDecorate);
 node^.AddParam(ntype,Data);
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

procedure TsrDecorateList.emit_member_decorate(ntype:TsrNodeType;Data:Pointer;index,offset:DWORD);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpMemberDecorate);
 node^.AddParam(ntype,Data);
 node^.AddLiteral(index);
 node^.AddLiteral(Decoration.Offset,Decoration.GetStr(Decoration.Offset));
 node^.AddLiteral(offset);
end;

procedure TsrDebugInfoList.emit_source_extension(const n:RawByteString);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpSourceExtension);
 node^.AddString(n);
end;

procedure TsrDebugInfoList.emit_name(ntype:TsrNodeType;Data:Pointer;const n:RawByteString);
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(Op.OpName);
 node^.AddParam(ntype,Data);
 node^.AddString(n);
end;

end.

