unit emit_op;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  srNodes,
  srTypes,
  srConst,
  srReg,
  srLayout,
  srVariable,
  srOp,
  srOpUtils,
  spirv,
  SprvEmit;

type
 PEmitOp=^TEmitOp;
 TEmitOp=object(TSprvEmit)
   procedure emit_OpBitcast(pLine:PspirvOp;dst,src:PsrRegNode);
   procedure emit_BoolToInt(pLine:PspirvOp;dst,src:PsrRegNode);
   procedure emit_IntToBool(pLine:PspirvOp;dst,src:PsrRegNode);
   procedure emit_OpCastExt(pLine:PspirvOp;dst,src:PsrRegNode);
   function  emit_OpLoad(pLine:PspirvOp;dtype:PsrType;dst:PsrRegNode;src:TOpParamSingle):PSpirvOp;
   procedure emit_OpLoad(pLine:PspirvOp;dst:PsrRegNode;src:PsrVariable);
   procedure emit_OpStore(pLine:PspirvOp;dst:TOpParamSingle;src:PsrRegNode);
   procedure emit_OpStore(pLine:PspirvOp;dst:PsrVariable;src:PsrRegNode);
   function  emit_OpCompExtract(pLine:PspirvOp;dst,src:PsrRegNode;id:DWORD):PSpirvOp;
   function  emit_OpCompConstruct(pLine:PspirvOp;dst:PsrRegNode):PSpirvOp;
   function  emit_OpAccessChain(pLine:PspirvOp;vType:PsrType;dst:PsrChain;src:PsrVariable):PSpirvOp;
   function  emit_OpExtInst(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
   function  emit_OpCondMerge(pLine,pLabel:PspirvOp):PSpirvOp;
   function  emit_OpLoopMerge(pLine,pLabel0,pLabel1:PspirvOp):PSpirvOp;
   function  emit_OpBranch(pLine,pLabel:PspirvOp):PSpirvOp;
   function  emit_OpBranchCond(pLine,pLabel0,pLabel1:PspirvOp;src:PsrRegNode):PSpirvOp;
   function  emit_OpReturnValue(pLine:PspirvOp;src:PsrRegNode):PSpirvOp;
   //
   function  emit_Op1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
   function  emit_Op2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
   function  emit_Op3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
   function  emit_OpExt1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
   function  emit_OpExt2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
   function  emit_OpExt3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
   procedure _emit_ConvFloatToHalf(dst:PsrRegSlot;src0,src1:PsrRegNode);
   //
   function  _emit_Op1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
   function  _emit_Op2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
   //
   function  _emit_OpExt1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
   function  _emit_OpExt2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
   //
   function  _emit_OpIsNan(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
   function  _emit_OpFAbs(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
   function  _emit_OpSAbs(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
   function  _emit_OpUMin(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpUMax(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpFMin(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpFMax(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpNMin(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpNMax(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;

   procedure emit_OpFloor(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpFract(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpSqrt(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpInverseSqrt(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpExp2(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpSin(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpFMin(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpFMax(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpFClamp(dst:PsrRegSlot;src,min,max:PsrRegNode);
   procedure emit_OpFmaF32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
   procedure emit_OpFmaI32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
   procedure emit_OpFmaU32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
   //
   procedure emit_OpCmpV(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpCmpS(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
   //
   procedure emit_OpSelect(dst:PsrRegSlot;src0,src1,cond:PsrRegNode);
   //
   function _emit_OpFNegate(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;

   procedure _emit_OpIAddC(pLine:PspirvOp;dst,car,src0,src1:PsrRegNode);
   function  _emit_OpIAdd(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   procedure emit_OpIAddExt(dst,car:PsrRegSlot;src0,src1:PsrRegNode);

   procedure _emit_OpISubB(pLine:PspirvOp;dst,bor,src0,src1:PsrRegNode);
   function  _emit_OpISub(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   procedure emit_OpISubExt(dst,bor:PsrRegSlot;src0,src1:PsrRegNode);

   function  _emit_OpAbsDiff(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   //
   function  _emit_OpIMul(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpUDiv(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpSDiv(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpShr(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpShrA(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   function  _emit_OpShl(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
   //
   procedure emit_WQM_32(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_MED3F(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
   //
   function  emit_OpMakeConstruct(pLine:PspirvOp;dst:PsrRegNode;count:Byte;src:PPsrRegNode):PSpirvOp;
   function  emit_OpMakeVec(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrRegNode;
   function  emit_OpMakeCube(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrRegNode;
   //
   function  emit_OpPackOfs(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PsrRegNode):PsrRegNode;
   //
   function  emit_OpSampledImage(pLine:PspirvOp;Tgrp,Sgrp:PsrRegNode;dtype:TsrDataType;info:TsrTypeImageInfo):PsrRegNode;
   //
   procedure emit_OpFAdd(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpFSub(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpIAdd(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpISub(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpIMul(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpFMul(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpUDiv(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpFDiv(dst:PsrRegSlot;src0,src1:PsrRegNode);
   //
   procedure emit_OpBfeU(dst:PsrRegSlot;base,offset,count:PsrRegNode);
   procedure emit_OpShl(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpShr(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpShrA(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpNot(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpLogicalNot(dst:PsrRegSlot;src:PsrRegNode);
   procedure emit_OpBitwiseOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpLogicalOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpBitwiseAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
   procedure emit_OpLogicalAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
   //
   function  emit_OpImageSampleImplicitLod(pLine:PspirvOp;dst,cmb,coord:PsrRegNode):PSpirvOp;
   function  emit_OpImageSampleExplicitLod(pLine:PspirvOp;dst,cmb,coord:PsrRegNode):PSpirvOp;
   function  emit_OpImageFetch(pLine:PspirvOp;Tgrp,dst,coord:PsrRegNode):PSpirvOp;
   function  emit_OpImageRead(pLine:PspirvOp;Tgrp,dst,idx:PsrRegNode):PspirvOp;
   function  emit_OpImageWrite(pLine:PspirvOp;Tgrp,idx,src:PsrRegNode):PspirvOp;
 end;

implementation

procedure TEmitOp.emit_OpBitcast(pLine:PspirvOp;dst,src:PsrRegNode);
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBitcast);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);
 node^.AddParam(ntReg,src);
end;

procedure TEmitOp.emit_BoolToInt(pLine:PspirvOp;dst,src:PsrRegNode);
Var
 node:PSpirvOp;
 src0,src1:PsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpSelect); //need first

 src0:=FetchReg(FConsts.Fetch(dst^.dtype,0));
 src1:=FetchReg(FConsts.Fetch(dst^.dtype,1));

 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);

 node^.AddParam(ntReg,src);
 node^.AddParam(ntReg,src1);
 node^.AddParam(ntReg,src0);
end;

procedure TEmitOp.emit_IntToBool(pLine:PspirvOp;dst,src:PsrRegNode);
Var
 node:PSpirvOp;
 src0:PsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpINotEqual); //need first

 node^.dst_type:=FSpirvTypes.Fetch(dtBool);
 node^.SetDstReg(dst);

 src0:=FetchReg(FConsts.Fetch(src^.dtype,0));

 node^.AddParam(ntReg,src);
 node^.AddParam(ntReg,src0);
end;

procedure TEmitOp.emit_OpCastExt(pLine:PspirvOp;dst,src:PsrRegNode);
begin
 if (src^.dtype=dtBool) then
 begin
  emit_BoolToInt(pLine,dst,src);
 end else
 if (dst^.dtype=dtBool) then
 begin
  Case src^.dtype of
   dtInt32,
   dtUint32:emit_IntToBool(pLine,dst,src);
   else
    Assert(false,'TODO');
  end;
 end else
 begin
  emit_OpBitcast(pLine,dst,src);
 end;
end;

function TEmitOp.emit_OpLoad(pLine:PspirvOp;dtype:PsrType;dst:PsrRegNode;src:TOpParamSingle):PSpirvOp;
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpLoad);
 //Assert(dtype<>nil);
 node^.dst_type:=dtype;
 node^.SetDstReg(dst);
 node^.AddParam(src);
 Result:=node;
end;

procedure TEmitOp.emit_OpLoad(pLine:PspirvOp;dst:PsrRegNode;src:PsrVariable);
Var
 p:TOpParamSingle;
 dtype:PsrType;
begin
 p.SetParam(ntVar,src);
 dtype:=FSpirvTypes.Fetch(dst^.dtype);
 emit_OpLoad(pLine,dtype,dst,p);
end;

procedure TEmitOp.emit_OpStore(pLine:PspirvOp;dst:TOpParamSingle;src:PsrRegNode);
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpStore);
 node^.dst:=dst;
 node^.AddParam(ntReg,src);
end;

procedure TEmitOp.emit_OpStore(pLine:PspirvOp;dst:PsrVariable;src:PsrRegNode);
Var
 p:TOpParamSingle;
begin
 dst^.mark_write;
 p.SetParam(ntVar,dst);
 emit_OpStore(pLine,p,src);
end;

function TEmitOp.emit_OpCompExtract(pLine:PspirvOp;dst,src:PsrRegNode;id:DWORD):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpCompositeExtract);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);
 node^.AddParam(ntReg,src);
 node^.AddLiteral(id);
 Result:=node;
end;

function TEmitOp.emit_OpCompConstruct(pLine:PspirvOp;dst:PsrRegNode):PSpirvOp;
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpCompositeConstruct);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 Assert(node^.dst_type<>nil);
 node^.SetDstReg(dst);
 Result:=node;
end;

function TEmitOp.emit_OpAccessChain(pLine:PspirvOp;vType:PsrType;dst:PsrChain;src:PsrVariable):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpAccessChain);

 Assert(vType<>nil);

 node^.dst_type:=FSpirvTypes.FetchPointer(vType,src^.GetStorageClass);
 node^.dst.SetParam(ntChain,dst);

 node^.AddParam(ntVar,src); //base
 //index sets later

 dst^.pLine:=node;

 Result:=node;
end;

function TEmitOp.emit_OpExtInst(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
var
 node:PSpirvOp;
begin
 if (FGLSL_std_450=nil) then
 begin
  FGLSL_std_450:=FHeader.AddSpirvOp(Op.OpExtInstImport);
  FGLSL_std_450^.dst.SetParam(ntRefId,NewRefId);
  FGLSL_std_450^.AddString('GLSL.std.450');
 end;

 node:=AddSpirvOp(pLine,Op.OpExtInst);
 node^.AddParam(ntRefId,FGLSL_std_450^.dst.AsRefId);
 node^.AddLiteral(OpId,GlslOp.GetStr(OpId));
 Result:=node;
end;

function TEmitOp.emit_OpCondMerge(pLine,pLabel:PspirvOp):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpSelectionMerge);
 node^.AddParam(ntRefId,pLabel^.dst.AsRefId);
 node^.AddLiteral(SelectionControl.None,'None');
 Result:=node;
end;

function TEmitOp.emit_OpLoopMerge(pLine,pLabel0,pLabel1:PspirvOp):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpLoopMerge);
 node^.AddParam(ntRefId,pLabel0^.dst.AsRefId);
 node^.AddParam(ntRefId,pLabel1^.dst.AsRefId);
 node^.AddLiteral(LoopControl.None,'None');
 Result:=node;
end;

function TEmitOp.emit_OpBranch(pLine,pLabel:PspirvOp):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBranch);
 node^.AddParam(ntRefId,pLabel^.dst.AsRefId);
 Result:=node;
end;

function TEmitOp.emit_OpBranchCond(pLine,pLabel0,pLabel1:PspirvOp;src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBranchConditional);
 node^.AddParam(ntReg,src);
 node^.AddParam(ntRefId,pLabel0^.dst.AsRefId);
 node^.AddParam(ntRefId,pLabel1^.dst.AsRefId);
 Result:=node;
end;

function TEmitOp.emit_OpReturnValue(pLine:PspirvOp;src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpReturnValue);
 node^.AddParam(ntReg,src);
 Result:=node;
end;

function TEmitOp.emit_Op1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(OpId); //need first

 dst^.New(line,rtype);

 node^.dst_type:=FSpirvTypes.Fetch(rtype);
 node^.SetDstReg(dst^.current);

 node^.AddParam(ntReg,src);
 Result:=node;
end;

function TEmitOp.emit_Op2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(OpId); //need first

 dst^.New(line,rtype);

 node^.dst_type:=FSpirvTypes.Fetch(rtype);
 node^.SetDstReg(dst^.current);

 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
 Result:=node;
end;

function TEmitOp.emit_Op3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(OpId); //need first

 dst^.New(line,rtype);

 node^.dst_type:=FSpirvTypes.Fetch(rtype);
 node^.SetDstReg(dst^.current);

 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
 node^.AddParam(ntReg,src2);
 Result:=node;
end;

function TEmitOp.emit_OpExt1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=emit_OpExtInst(line,OpId); //need first

 dst^.New(line,rtype);

 node^.dst_type:=FSpirvTypes.Fetch(rtype);
 node^.SetDstReg(dst^.current);

 node^.AddParam(ntReg,src);
 Result:=node;
end;

function TEmitOp.emit_OpExt2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=emit_OpExtInst(line,OpId); //need first

 dst^.New(line,rtype);

 node^.dst_type:=FSpirvTypes.Fetch(rtype);
 node^.SetDstReg(dst^.current);

 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
 Result:=node;
end;

function TEmitOp.emit_OpExt3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=emit_OpExtInst(line,OpId); //need first

 dst^.New(line,rtype);

 node^.dst_type:=FSpirvTypes.Fetch(rtype);
 node^.SetDstReg(dst^.current);

 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
 node^.AddParam(ntReg,src2);
 Result:=node;
end;

procedure TEmitOp._emit_ConvFloatToHalf(dst:PsrRegSlot;src0,src1:PsrRegNode);
var
 rsl:array[0..1] of PsrRegNode;
begin
 //vdst[15:00].hf = ConvertFloatToHalfFloat(vsrc0.f);
 //vdst[31:16].hf = ConvertFloatToHalfFloat(vsrc1.f);

 rsl[0]:=NewReg(dtHalf16);
 rsl[1]:=NewReg(dtHalf16);

 _emit_Op1(line,Op.OpFConvert,rsl[0],src0);
 _emit_Op1(line,Op.OpFConvert,rsl[1],src1);

 rsl[0]^.mark_read;
 rsl[1]^.mark_read;

 dst^.New(line,dtVec2h);

 emit_OpMakeConstruct(line,dst^.current,2,@rsl);
end;

function TEmitOp._emit_Op1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);
 node^.AddParam(ntReg,src);
 Result:=node;
end;

function TEmitOp._emit_Op2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);
 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
 Result:=node;
end;

//

function TEmitOp._emit_OpExt1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=emit_OpExtInst(pLine,OpId); //need first
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);
 node^.AddParam(ntReg,src);
 Result:=node;
end;

function TEmitOp._emit_OpExt2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=emit_OpExtInst(pLine,OpId); //need first
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);
 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
 Result:=node;
end;

//

function TEmitOp._emit_OpIsNan(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op1(pLine,Op.OpIsNan,dst,src);
end;

function TEmitOp._emit_OpFAbs(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt1(pLine,GlslOp.FAbs,dst,src);
end;

function TEmitOp._emit_OpSAbs(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt1(pLine,GlslOp.SAbs,dst,src);
end;

function TEmitOp._emit_OpUMin(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt2(pLine,GlslOp.UMin,dst,src0,src1);
end;

function TEmitOp._emit_OpUMax(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt2(pLine,GlslOp.UMax,dst,src0,src1);
end;

function TEmitOp._emit_OpFMin(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt2(pLine,GlslOp.FMin,dst,src0,src1);
end;

function TEmitOp._emit_OpFMax(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt2(pLine,GlslOp.FMax,dst,src0,src1);
end;

function TEmitOp._emit_OpNMin(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt2(pLine,GlslOp.NMin,dst,src0,src1);
end;

function TEmitOp._emit_OpNMax(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_OpExt2(pLine,GlslOp.NMax,dst,src0,src1);
end;

procedure TEmitOp.emit_OpFloor(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_OpExt1(GlslOp.Floor,dtFloat32,dst,src);
end;

procedure TEmitOp.emit_OpFract(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_OpExt1(GlslOp.Fract,dtFloat32,dst,src);
end;

procedure TEmitOp.emit_OpSqrt(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_OpExt1(GlslOp.Sqrt,dtFloat32,dst,src);
end;

procedure TEmitOp.emit_OpInverseSqrt(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_OpExt1(GlslOp.InverseSqrt,dtFloat32,dst,src);
end;

procedure TEmitOp.emit_OpExp2(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_OpExt1(GlslOp.Exp2,dtFloat32,dst,src);
end;

procedure TEmitOp.emit_OpSin(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_OpExt1(GlslOp.Sin,dtFloat32,dst,src);
end;

procedure TEmitOp.emit_OpFMin(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_OpExt2(GlslOp.FMin,dtFloat32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpFMax(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_OpExt2(GlslOp.FMax,dtFloat32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpFClamp(dst:PsrRegSlot;src,min,max:PsrRegNode);
begin
 emit_OpExt3(GlslOp.FClamp,dtFloat32,dst,src,min,max);
end;

procedure TEmitOp.emit_OpFmaF32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
begin
 //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
 //NoContraction  decoration
 emit_OpExt3(GlslOp.Fma,dtFloat32,dst,src0,src1,src2);
end;

procedure TEmitOp.emit_OpFmaI32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
var
 mul,sum:PsrRegNode;
begin
 //vdst = vsrc0.i * vsrc1.i + vdst.i
 mul:=NewReg(dtInt32);
 sum:=dst^.New(line,dtInt32);

 _emit_OpIMul(line,mul,src0,src1);

 mul^.mark_read;
 _emit_OpIAdd(line,sum,mul,src2);
end;

procedure TEmitOp.emit_OpFmaU32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
var
 mul,sum:PsrRegNode;
begin
 //vdst = vsrc0.u * vsrc1.u + vdst.u
 mul:=NewReg(dtUInt32);
 sum:=dst^.New(line,dtUInt32);

 _emit_OpIMul(line,mul,src0,src1);

 mul^.mark_read;
 _emit_OpIAdd(line,sum,mul,src2);
end;

//

procedure TEmitOp.emit_OpCmpV(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode); //sdst[thread_id:] = (vsrc1.u <cmp> vsrc2.u) & EXEC
Var
 tmp:PsrRegNode;
 exc:PsrRegNode;
begin
 emit_Op2(OpId,dtBool,@FRegsStory.FUnattach,src0,src1);

 tmp:=FRegsStory.FUnattach.current;
 tmp^.mark_read;

 exc:=MakeRead(@FRegsStory.EXEC[0],dtBool);
 emit_OpLogicalAnd(dst,tmp,exc);
end;

procedure TEmitOp.emit_OpCmpS(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(OpId,dtBool,dst,src0,src1);
end;

procedure TEmitOp.emit_OpSelect(dst:PsrRegSlot;src0,src1,cond:PsrRegNode);
begin
 emit_Op3(Op.OpSelect,LazyType2(src0^.dtype,src1^.dtype),dst,cond,src1,src0);
end;

function TEmitOp._emit_OpFNegate(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op1(pLine,Op.OpFNegate,dst,src);
end;

procedure TEmitOp._emit_OpIAddC(pLine:PspirvOp;dst,car,src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
 rsl:PsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpIAddCarry); //need first

 rsl:=NewReg(dtStruct2u);

 node^.dst_type:=FSpirvTypes.Fetch(dtStruct2u);
 node^.SetDstReg(rsl);

 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);

 rsl^.mark_read;
 rsl^.mark_read;

 pLine:=node;
 pLine:=emit_OpCompExtract(pLine,dst,rsl,0);
 pLine:=emit_OpCompExtract(pLine,car,rsl,1);
end;

function TEmitOp._emit_OpIAdd(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 {if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  dst^.dtype:=dtInt32;
 end else
 begin
  dst^.dtype:=dtUInt32;
 end;}
 Result:=_emit_Op2(pLine,Op.OpIAdd,dst,src0,src1);
end;

procedure TEmitOp.emit_OpIAddExt(dst,car:PsrRegSlot;src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(OpIAddExt); //need first

 dst^.New(line,dtUint32); //alloc
 car^.New(line,dtUint32); //alloc

 car^.current^.mark_read;       //mark dep
 node^.SetDstReg(car^.current); //set with dep
 node^.SetDstReg(dst^.current); //set dst

 node^.AddParam(ntReg,car^.current);
 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
end;

procedure TEmitOp.emit_OpISubExt(dst,bor:PsrRegSlot;src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(OpISubExt); //need first

 dst^.New(line,dtUint32); //alloc
 bor^.New(line,dtUint32); //alloc

 bor^.current^.mark_read;       //mark dep
 node^.SetDstReg(bor^.current); //set with dep
 node^.SetDstReg(dst^.current); //set dst

 node^.AddParam(ntReg,bor^.current);
 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);
end;

procedure TEmitOp._emit_OpISubB(pLine:PspirvOp;dst,bor,src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
 rsl:PsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpISubBorrow); //need first

 rsl:=NewReg(dtStruct2u);

 node^.dst_type:=FSpirvTypes.Fetch(dtStruct2u);
 node^.SetDstReg(rsl);

 node^.AddParam(ntReg,src0);
 node^.AddParam(ntReg,src1);

 rsl^.mark_read;
 rsl^.mark_read;

 pLine:=node;
 pLine:=emit_OpCompExtract(pLine,dst,rsl,0);
 pLine:=emit_OpCompExtract(pLine,bor,rsl,1);
end;

function TEmitOp._emit_OpISub(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 {if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  dst^.dtype:=dtInt32;
 end else
 begin
  dst^.dtype:=dtUInt32;
 end;}
 Result:=_emit_Op2(pLine,Op.OpISub,dst,src0,src1);
end;

function TEmitOp._emit_OpAbsDiff(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op2(pLine,OpAbsDiff,dst,src0,src1);
end;

function TEmitOp._emit_OpIMul(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 {if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  dst^.dtype:=dtInt32;
 end else
 begin
  dst^.dtype:=dtUInt32;
 end;}
 Result:=_emit_Op2(pLine,Op.OpIMul,dst,src0,src1);
end;

function TEmitOp._emit_OpUDiv(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op2(pLine,Op.OpUDiv,dst,src0,src1);
end;

function TEmitOp._emit_OpSDiv(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op2(pLine,Op.OpSDiv,dst,src0,src1);
end;

function TEmitOp._emit_OpShr(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op2(pLine,Op.OpShiftRightLogical,dst,src0,src1);
end;

function TEmitOp._emit_OpShrA(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op2(pLine,Op.OpShiftRightArithmetic,dst,src0,src1);
end;

function TEmitOp._emit_OpShl(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_emit_Op2(pLine,Op.OpShiftLeftLogical,dst,src0,src1);
end;

procedure TEmitOp.emit_WQM_32(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_Op1(OpWQM32,dtUnknow,dst,src);
end;

procedure TEmitOp.emit_MED3F(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
var
 min:PsrRegNode;
 max:PsrRegNode;
 tmp:PsrRegNode;
 ret:PsrRegNode;
begin
 //emit_Op3(OpMED3F,dtFloat32,dst,src0,src1,src2);

 min:=NewReg(dtFloat32);
 src0^.mark_read;
 src1^.mark_read;
 _emit_OpFMin(line,min,src0,src1); //min(s0,s1)

 max:=NewReg(dtFloat32);
 src0^.mark_read;
 src1^.mark_read;
 _emit_OpNMax(line,max,src0,src1); //max(s0,s1)

 tmp:=NewReg(dtFloat32);
 max^.mark_read;
 src2^.mark_read;
 _emit_OpFMin(line,tmp,max,src2); //min(max(s0,s1),s2)

 ret:=dst^.New(line,dtFloat32);
 min^.mark_read;
 tmp^.mark_read;
 _emit_OpNMax(line,ret,min,tmp); //max(min(s0,s1),min(max(s0,s1),s2))
end;

function TEmitOp.emit_OpMakeConstruct(pLine:PspirvOp;dst:PsrRegNode;count:Byte;src:PPsrRegNode):PSpirvOp;
Var
 p:PsrCacheOp;
 node:PSpirvOp;
 i:Byte;
begin
 Result:=pLine;
 Assert((count>0) and (count<=4));

 pLine:=GetMaxPlace(pLine,count,src);

 p:=FCacheOps.Fetch(pLine^.pParent,Op.OpCompositeConstruct,dst^.dtype,count,src);
 if (p^.pReg=nil) then
 begin
  node:=emit_OpCompConstruct(pLine,dst);
  For i:=0 to count-1 do
  begin
   node^.AddParam(ntReg,src[i]);
  end;
  p^.pReg:=dst; //save
  Result:=node;
 end else
 begin
  p^.pReg^.mark_read;
  dst^.pWriter.SetParam(ntReg,p^.pReg);
  //deref
  For i:=0 to count-1 do
  begin
   src[i]^.mark_unread;
  end;
 end;
end;

function TEmitOp.emit_OpMakeVec(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrRegNode;
Var
 p:PsrCacheOp;
 node:PSpirvOp;
 i:Byte;
begin
 Assert((count>0) and (count<=4));

 pLine:=GetMaxPlace(pLine,count,src);

 p:=FCacheOps.Fetch(pLine^.pParent,OpMakeVec,rtype,count,src);
 Result:=p^.pReg;
 if (Result=nil) then
 begin
  node:=AddSpirvOp(pLine,OpMakeVec); //need first

  Result:=NewReg(rtype);

  node^.SetDstReg(Result);
  node^.AddLiteral(ord(rtype));
  node^.AddLiteral(count);

  For i:=0 to count-1 do
  begin
   node^.AddParam(ntReg,src[i]);
  end;

  p^.pReg:=Result; //save
 end else
 begin
  //deref
  For i:=0 to count-1 do
  begin
   src[i]^.mark_unread;
  end;
 end;
end;


//x,y,face,slice
function TEmitOp.emit_OpMakeCube(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PPsrRegNode):PsrRegNode;
Var
 p:PsrCacheOp;
 node:PSpirvOp;
 i:Byte;
begin

 Assert((count=3) or (count=4));

 pLine:=GetMaxPlace(pLine,count,src);

 p:=FCacheOps.Fetch(pLine^.pParent,OpMakeCM,rtype,count,src);
 Result:=p^.pReg;
 if (Result=nil) then
 begin
  node:=AddSpirvOp(pLine,OpMakeCM); //need first

  Result:=NewReg(rtype);

  node^.SetDstReg(Result);
  node^.AddLiteral(ord(rtype));
  node^.AddLiteral(count);

  For i:=0 to count-1 do
  begin
   node^.AddParam(ntReg,src[i]);
  end;

  p^.pReg:=Result; //save
 end else
 begin
  //deref
  For i:=0 to count-1 do
  begin
   src[i]^.mark_unread;
  end;
 end;
end;

function TEmitOp.emit_OpPackOfs(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PsrRegNode):PsrRegNode;
Var
 p:PsrCacheOp;
 node:PSpirvOp;
begin
 Assert((count>0) and (count<=3));

 pLine:=GetMaxPlace(pLine,1,@src);

 p:=FCacheOps.Fetch(pLine^.pParent,OpPackOfs,rtype,1,@src);
 Result:=p^.pReg;
 if (Result=nil) then
 begin
  node:=AddSpirvOp(pLine,OpPackOfs); //need first

  Result:=NewReg(rtype);

  node^.SetDstReg(Result);
  node^.AddLiteral(ord(rtype));
  node^.AddLiteral(count);
  node^.AddParam(ntReg,src);

  p^.pReg:=Result; //save
 end else
 begin
  //deref
  src^.mark_unread;
 end;
end;

function TEmitOp.emit_OpSampledImage(pLine:PspirvOp;Tgrp,Sgrp:PsrRegNode;dtype:TsrDataType;info:TsrTypeImageInfo):PsrRegNode;
Var
 src:array[0..1] of PsrRegNode;
 pType:PsrType;
 p:PsrCacheOp;
 node:PSpirvOp;
begin
 src[0]:=Tgrp;
 src[1]:=Sgrp;

 p:=FCacheOps.Fetch(pLine^.pParent,Op.OpSampledImage,dtTypeSampledImage,2,@src);
 Result:=p^.pReg;
 if (Result=nil) then
 begin
  Tgrp^.mark_read;
  Sgrp^.mark_read;

  Result:=NewReg(dtTypeSampledImage);

  node:=AddSpirvOp(pLine,Op.OpSampledImage);

  pType:=FSpirvTypes.FetchImage(FSpirvTypes.Fetch(dtype),info);
  node^.dst_type:=FSpirvTypes.FetchSampledImage(pType);
  node^.SetDstReg(Result);

  node^.AddParam(ntReg,Tgrp);
  node^.AddParam(ntReg,Sgrp);

  p^.pReg:=Result; //save
 end;
end;

//

procedure TEmitOp.emit_OpFAdd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpFAdd,dtFloat32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpFSub(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpFSub,dtFloat32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpIAdd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  emit_Op2(Op.OpIAdd,dtInt32,dst,src0,src1);
 end else
 begin
  emit_Op2(Op.OpIAdd,dtUInt32,dst,src0,src1);
 end;
end;

procedure TEmitOp.emit_OpISub(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  emit_Op2(Op.OpISub,dtInt32,dst,src0,src1);
 end else
 begin
  emit_Op2(Op.OpISub,dtUInt32,dst,src0,src1);
 end;
end;

procedure TEmitOp.emit_OpIMul(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  emit_Op2(Op.OpIMul,dtInt32,dst,src0,src1);
 end else
 begin
  emit_Op2(Op.OpIMul,dtUInt32,dst,src0,src1);
 end;
end;

procedure TEmitOp.emit_OpFMul(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpFMul,dtFloat32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpUDiv(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpUDiv,dtUint32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpFDiv(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpFDiv,dtFloat32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpBfeU(dst:PsrRegSlot;base,offset,count:PsrRegNode);
begin
 emit_Op3(Op.OpBitFieldUExtract,dtUInt32,dst,base,offset,count);
end;

procedure TEmitOp.emit_OpShl(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpShiftLeftLogical,dtUint32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpShr(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpShiftRightLogical,dtUint32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpShrA(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpShiftRightArithmetic,dtInt32,dst,src0,src1);
end;

procedure TEmitOp.emit_OpNot(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_Op1(Op.OpNot,dtUnknow{dtUint32},dst,src);
end;

procedure TEmitOp.emit_OpLogicalNot(dst:PsrRegSlot;src:PsrRegNode);
begin
 emit_Op1(Op.OpLogicalNot,dtBool,dst,src);
end;

procedure TEmitOp.emit_OpBitwiseOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpBitwiseOr,dtUnknow{dtUint32},dst,src0,src1);
end;

procedure TEmitOp.emit_OpLogicalOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpLogicalOr,dtBool,dst,src0,src1);
end;

procedure TEmitOp.emit_OpBitwiseAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpBitwiseAnd,dtUnknow{dtUint32},dst,src0,src1);
end;

procedure TEmitOp.emit_OpLogicalAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 emit_Op2(Op.OpLogicalAnd,dtBool,dst,src0,src1);
end;

//

function TEmitOp.emit_OpImageSampleImplicitLod(pLine:PspirvOp;dst,cmb,coord:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageSampleImplicitLod);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);

 node^.AddParam(ntReg,cmb);
 node^.AddParam(ntReg,coord);

 Result:=node;
end;

function TEmitOp.emit_OpImageSampleExplicitLod(pLine:PspirvOp;dst,cmb,coord:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageSampleExplicitLod);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);

 node^.AddParam(ntReg,cmb);
 node^.AddParam(ntReg,coord);

 Result:=node;
end;

function TEmitOp.emit_OpImageFetch(pLine:PspirvOp;Tgrp,dst,coord:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 Tgrp^.mark_read;

 node:=AddSpirvOp(pLine,Op.OpImageFetch);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);

 node^.AddParam(ntReg,Tgrp);
 node^.AddParam(ntReg,coord);

 Result:=node;
end;

function TEmitOp.emit_OpImageRead(pLine:PspirvOp;Tgrp,dst,idx:PsrRegNode):PspirvOp;
Var
 node:PSpirvOp;
begin
 Tgrp^.mark_read;

 node:=AddSpirvOp(pLine,Op.OpImageRead);
 node^.dst_type:=FSpirvTypes.Fetch(dst^.dtype);
 node^.SetDstReg(dst);

 node^.AddParam(ntReg,Tgrp);

 if (idx<>nil) then
 begin
  node^.AddParam(ntReg,idx);
 end else
 begin
  node^.AddParam(ntConst,FConsts.Fetchi(dtUint32,0));
 end;

 Result:=node;
end;

function TEmitOp.emit_OpImageWrite(pLine:PspirvOp;Tgrp,idx,src:PsrRegNode):PspirvOp;
Var
 node:PSpirvOp;
begin
 Tgrp^.mark_read; //mark write

 node:=AddSpirvOp(pLine,Op.OpImageWrite);
 node^.dst.SetParam(ntReg,Tgrp);

 node^.AddParam(ntReg,Tgrp);

 if (idx<>nil) then
 begin
  node^.AddParam(ntReg,idx);
 end else
 begin
  node^.AddParam(ntConst,FConsts.Fetchi(dtUint32,0));
 end;

 node^.AddParam(ntReg,src);

 Result:=node;
end;


end.

