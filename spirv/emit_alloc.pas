unit emit_alloc;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  srNodes,
  srTypes,
  srConst,
  srRefId,
  srReg,
  srLayout,
  srBuffer,
  srVariable,
  srOp,
  srOpUtils,
  SprvEmit;

type
 TSprvEmit_alloc=object(TSprvEmit)
  procedure Alloc;
  procedure AllocSpirvID(P:PsrRefId);
  procedure AllocBinding;
  procedure AllocTypeBinding;
  procedure AllocSourceExtension;
  procedure AllocTypeName;
  Procedure AllocVarName;
  procedure AllocEntryPoint;
  procedure AllocHeader;
  procedure AllocOpListId(node:PspirvOp);
  procedure AllocHeaderId;
  procedure AllocTypesId;
  procedure AllocConstId;
  procedure AllocVariableId;
  procedure AllocOpSingle(const Param:TOpParamSingle);
  procedure AllocOpParamNode(node:POpParamNode);
  procedure AllocFuncId;
  procedure AllocOpId(node:PSpirvOp);
  procedure AllocOpBlock(pBlock:PsrOpBlock);
 end;

implementation

procedure TSprvEmit_alloc.Alloc;
begin
 AllocBinding;
 AllocTypeBinding;

 AllocHeader;

 AllocSourceExtension;
 AllocTypeName;
 AllocVarName;

 AllocHeaderId;

 AllocTypesId;
 AllocConstId;
 AllocVariableId;
 AllocFuncId;
end;

procedure TSprvEmit_alloc.AllocSpirvID(P:PsrRefId);
begin
 FSpirvIdAlloc.FetchSpirvID(P);
end;

procedure TSprvEmit_alloc.AllocBinding;
var
 FBinding:Integer;
begin
 FInputs     .AllocBinding(@FDecorates);
 FOutputs    .AllocBinding(@FDecorates);
 FVertLayouts.AllocBinding(@FDecorates);
 FFragLayouts.AllocBinding(@FDecorates);

 FBinding:=0;
 FUniforms.AllocBinding(FBinding,@FDecorates);
 FBuffers .AllocBinding(FBinding,@FDecorates);
end;

procedure TSprvEmit_alloc.AllocTypeBinding;
var
 node:PsrType;
 pField:PsrField;
begin
 node:=FSpirvTypes.FList.pHead;
 While (node<>nil) do
 begin

  case node^.dtype of
   dtTypeStruct:
    begin
     pField:=node^.key.ext.pField;
     if (pField<>nil) then
     begin
      if (pField^.GetStructDecorate<>DWORD(-1)) then
      begin
       FDecorates.emit_decorate(ntType,node,pField^.GetStructDecorate,0);
      end;
      pField^.AllocBinding(node,@FDecorates);
     end;
    end;

   dtTypeArray,
   dtTypeRuntimeArray:
    begin
     FDecorates.emit_decorate(ntType,node,Decoration.ArrayStride,node^.key.ext.array_stride);
    end;

   else;
  end;

  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocSourceExtension;
begin
 FDataLayouts.AllocSourceExtension(@FDebugInfo);
 FDataLayouts.AllocFuncExt(@FDebugInfo,FCodeHeap);
 FVertLayouts.AllocSourceExtension(@FDebugInfo);
 FUniforms   .AllocSourceExtension(@FDebugInfo);
 FBuffers    .AllocSourceExtension(@FDebugInfo);
end;

procedure TSprvEmit_alloc.AllocTypeName;
var
 node:PsrType;
 pField:PsrField;
begin
 node:=FSpirvTypes.FList.pHead;
 While (node<>nil) do
 begin

  case node^.dtype of
   dtTypeStruct:
    begin
     pField:=node^.key.ext.pField;
     if (pField<>nil) and
        (pField^.parent=nil) and
        (pField^.pBuffer<>nil) then
     begin
      FDebugInfo.emit_name(ntType,node,pField^.pBuffer^.GetStructName);
     end;
    end;

   else;
  end;

  node:=node^.pNext;
 end;
end;

Procedure TSprvEmit_alloc.AllocVarName;
var
 node:PsrVariable;
 n:RawByteString;
begin
 node:=FVariables.pHead;
 While (node<>nil) do
 begin
  if (node^.pType<>nil) then
  begin
   n:=node^.GetName;
   if (n<>'') then
   begin
    FDebugInfo.emit_name(ntVar,node,n);
   end;
  end;
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocOpListId(node:PspirvOp);
begin
 While (node<>nil) do
 begin
  AllocOpId(node);
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocEntryPoint;
var
 node:PSpirvOp;
begin
 node:=FHeader.AddSpirvOp(Op.OpEntryPoint);

 node^.AddLiteral(FExecutionModel,ExecutionModel.GetStr(FExecutionModel));

 node^.AddParam(ntFunc,FMain);
 node^.AddString(FMain^.name);

 FInputs     .AllocEntryPoint(node);
 FVertLayouts.AllocEntryPoint(node);
 FFragLayouts.AllocEntryPoint(node);
 FOutputs    .AllocEntryPoint(node);
end;

procedure TSprvEmit_alloc.AllocHeader;
var
 node:PSpirvOp;
begin
 node:=FHeader.AddSpirvOp(Op.OpMemoryModel);
 node^.AddLiteral(AddressingModel.Logical,AddressingModel.GetStr(AddressingModel.Logical));
 node^.AddLiteral(MemoryModel.GLSL450,MemoryModel.GetStr(MemoryModel.GLSL450));

 AllocEntryPoint;

 Case FExecutionModel of
  ExecutionModel.Fragment:
    begin
     node:=FHeader.AddSpirvOp(Op.OpExecutionMode);
     node^.AddParam(ntFunc,FMain);
     node^.AddLiteral(ExecutionMode.OriginUpperLeft,ExecutionMode.GetStr(ExecutionMode.OriginUpperLeft));
    end;
  ExecutionModel.GLCompute:
    begin
     node:=FHeader.AddSpirvOp(Op.OpExecutionMode);
     node^.AddParam(ntFunc,FMain);
     node^.AddLiteral(ExecutionMode.LocalSize,ExecutionMode.GetStr(ExecutionMode.LocalSize));
     node^.AddLiteral(FLocalSize.x);
     node^.AddLiteral(FLocalSize.y);
     node^.AddLiteral(FLocalSize.z);
    end;
 end;

end;

procedure TSprvEmit_alloc.AllocHeaderId;
begin
 AllocOpListId(FHeader.pHead);
 AllocOpListId(FDebugInfo.pHead);
 AllocOpListId(FDecorates.pHead);
end;

procedure TSprvEmit_alloc.AllocTypesId;
var
 node:PsrType;
 pConst:PsrConst;
begin
 node:=FSpirvTypes.FList.pHead;
 While (node<>nil) do
 begin
  case node^.dtype of
   dtTypeArray:
     begin
      //find a const
      pConst:=FConsts.Fetchi(dtUInt32,node^.key.ext.array_count);
      if (pConst^.pType=nil) then
      begin
       pConst^.pType:=FSpirvTypes.Fetch(pConst^.key.dtype);
       AllocSpirvID(@pConst^.pType^.ID);
      end;
      FConsts.FList.Remove(pConst);
      AllocSpirvID(@pConst^.ID);
     end;
   else;
  end;
  AllocSpirvID(@node^.ID);
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocConstId;
var
 node:PsrConst;
begin
 node:=FConsts.FList.pHead;
 While (node<>nil) do
 begin
  if (node^.key.dtype<>dtUnknow) then
  begin
   AllocSpirvID(@node^.ID)
  end;
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocVariableId;
var
 node:PsrVariable;
begin
 node:=FVariables.pHead;
 While (node<>nil) do
 begin
  if (node^.pType<>nil) then
  begin
   AllocSpirvID(@node^.ID);
   AllocSpirvID(@node^.pType^.ID);
  end;
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocOpSingle(const Param:TOpParamSingle);
var
 pReg:PsrRegNode;
begin
 Assert(Param.pData<>nil,'AllocOpSingle$1');
 Case Param.ntype of

  ntFunc:
   begin
    AllocSpirvID(@PSpirvFunc(Param.pData)^.ID);
   end;

  ntRefId:
    begin
     AllocSpirvID(Param.pData);
    end;

  ntType:
    begin
     AllocSpirvID(@PsrType(Param.pData)^.ID);
    end;

  ntReg:
    begin
     pReg:=Param.pData;
     Case pReg^.pWriter.ntype of
      ntConst:;
      ntOp:AllocSpirvID(@pReg^.ID);
      else
       Assert(false,'AllocOpSingle$2');
     end;
    end;

  ntVar:
    begin
     AllocSpirvID(@PsrVariable(Param.pData)^.ID);
    end;

  ntChain:
    begin
     AllocSpirvID(@PsrChain(Param.pData)^.ID);
    end;

   ntConst:
     begin
      AllocSpirvID(@PsrConst(Param.pData)^.ID)
     end;

  else
   Assert(false,'AllocOpSingle$3');
 end;

end;

procedure TSprvEmit_alloc.AllocOpParamNode(node:POpParamNode);
begin
 Case node^.ntype of
  ntLiteral:;
  ntString :;
  else
   begin
    AllocOpSingle(node^.AsParam);
   end;
 end;
end;

procedure TSprvEmit_alloc.AllocFuncId;
var
 pFunc:PSpirvFunc;
begin
 pFunc:=FSpirvFuncs.FList.pHead;
 While (pFunc<>nil) do
 begin
  AllocOpBlock(@pFunc^.FTop);
  pFunc:=pFunc^.pNext;
 end;
end;

procedure TSprvEmit_alloc.AllocOpId(node:PSpirvOp);
var
 Param:POpParamNode;
 Info:Op.TOpInfo;
begin
 if (node=nil) then Exit;

 Info:=Op.GetInfo(node^.OpId);

 if Info.result then //dst
 begin
  Assert(node^.dst.ntype<>ntUnknow,'AllocOp$1');
  Assert(node^.dst.pData<>nil     ,'AllocOp$2');
  AllocOpSingle(node^.dst);
 end else
 begin  //no dst
  if (node^.dst.ntype<>ntUnknow) then
  begin
   Assert(node^.dst.pData<>nil,'AllocOp$3');
   AllocOpSingle(node^.dst);
  end;
 end;

 if Info.rstype then //dst type
 begin
  Assert(node^.dst_type<>nil,'AllocOp$4');
 end;

 Param:=node^.pParam.pHead;
 While (Param<>nil) do
 begin
  AllocOpParamNode(Param);
  Param:=Param^.pNext;
 end;

end;

procedure TSprvEmit_alloc.AllocOpBlock(pBlock:PsrOpBlock);
var
 node:PSpirvOp;
begin
 if (pBlock=nil) then Exit;
 node:=pBlock^.pHead;
 While (node<>nil) do
 begin

  if (node^.OpId=OpBlock) then
  begin
   if (node^.dst.ntype=ntBlock) then
   begin
    AllocOpBlock(node^.dst.pData);
   end;
  end else
  begin
   AllocOpId(node);
  end;

  node:=node^.pNext;
 end;
end;


end.

