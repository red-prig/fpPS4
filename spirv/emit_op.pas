unit emit_op;

{$mode objfpc}{$H+}

interface

uses
 spirv,
 srNode,
 srType,
 srTypes,
 srConst,
 srReg,
 srLayout,
 srVariable,
 srOp,
 srOpUtils,
 srCacheOp,
 srInterface;

type
 TEmitOp=class(TEmitInterface)
  //
  function  _Op1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
  function  _Op2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
  function  _Op3(pLine:PspirvOp;OpId:DWORD;dst,src0,src1,src2:PsrRegNode):PSpirvOp;
  function  _Op4(pLine:PspirvOp;OpId:DWORD;dst,src0,src1,src2,src3:PsrRegNode):PSpirvOp;
  //
  function  _OpGlsl1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
  function  _OpGlsl2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
  function  _OpGlsl3(pLine:PspirvOp;OpId:DWORD;dst,src0,src1,src2:PsrRegNode):PSpirvOp;
  //
  function  Op1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
  function  Op2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
  function  Op3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
  function  Op4(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2,src3:PsrRegNode):PSpirvOp;
  //
  function  OpGlsl1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
  function  OpGlsl2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
  function  OpGlsl3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
  //
  function  OpBitcast(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
  function  OpBoolToInt(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
  function  OpIntToBool(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
  //
  function  OpCast (nLine,      dst,src:PsrNode):PsrNode; override;
  function  OpLoad (nLine,dtype,dst,src:PsrNode):PsrNode; override;
  function  OpStore(nLine,      dst,src:PsrNode):PsrNode; override;
  //
  function  OpLoad(pLine:PspirvOp;dst:PsrRegNode;src:PsrNode):PSpirvOp;
  //
  function  OpExtract(pLine:PspirvOp;dst,src:PsrRegNode;id:DWORD):PSpirvOp;
  function  OpConstruct(pLine:PspirvOp;dst:PsrRegNode):PSpirvOp;
  function  OpAccessChain(pLine:PspirvOp;vType:PsrType;dst:PsrChain;src:PsrVariable):PSpirvOp;
  function  OpCondMerge(pLine,pLabel:PspirvOp):PSpirvOp;
  function  OpLoopMerge(pLine,pLabel0,pLabel1:PspirvOp):PSpirvOp;
  function  OpBranch(pLine,pLabel:PspirvOp):PSpirvOp;
  function  OpBranchCond(pLine,pLabel0,pLabel1:PspirvOp;src:PsrRegNode):PSpirvOp;
  function  OpReturnValue(pLine:PspirvOp;src:PsrRegNode):PSpirvOp;
  //
  procedure OpFmaF32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
  procedure OpFmaI32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
  procedure OpFmaU32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
  //
  procedure OpSelect(dst:PsrRegSlot;src0,src1,cond:PsrRegNode);
  //
  procedure OpIAddCar(pLine:PspirvOp;dst,car,src0,src1:PsrRegNode);
  procedure OpIAddExt(dst,car:PsrRegSlot;src0,src1:PsrRegNode);
  //
  procedure OpISubBor(pLine:PspirvOp;dst,bor,src0,src1:PsrRegNode);
  procedure OpISubExt(dst,bor:PsrRegSlot;src0,src1:PsrRegNode);
  //
  function  OpAbsDiff(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
  procedure OpWQM32(dst:PsrRegSlot;src:PsrRegNode);
  //
  procedure OpBFEU32(dst:PsrRegSlot;base,src0,src1:PsrRegNode);
  procedure OpBFIB32(dst:PsrRegSlot;bitmsk,src0,src1:PsrRegNode);
  //
  function  OpBFITo(src0,src1,src2,src3:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  procedure OpPackAnc(dst:PsrRegSlot;prim,smid,rtid:PsrRegNode);
  //
  function  OpUMinTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpUMaxTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFMinTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFMaxTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpNMinTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpNMaxTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  procedure OpMED3F(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
  //
  function  OpPackOfs(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PsrRegNode):PsrRegNode;
  function  OpMakeCon(pLine:PspirvOp;dst:PsrRegNode;src:PPsrRegNode):PSpirvOp;
  function  OpMakeVec(pLine:PspirvOp;rtype:TsrDataType;src:PPsrRegNode):PsrRegNode;
  function  OpMakeCub(pLine:PspirvOp;rtype:TsrDataType;src:PPsrRegNode):PsrRegNode;
  function  OpSampledImage(pLine:PspirvOp;Tgrp,Sgrp:PsrNode;dtype:TsrDataType;info:TsrTypeImageInfo):PsrRegNode;
  //
  procedure OpIAdd(dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpISub(dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpIMul(dst:PsrRegSlot;src0,src1:PsrRegNode);
  //
  function  OpShlTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpShlTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpShrTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpShrTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  function  OpIAddTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpIAddTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpISubTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpISubTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpIMulTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpIMulTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpIDivTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpIDivTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  function  OpFAddTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFSubTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFMulTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFDivTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  function  OpFAddToS(src0:PsrRegNode;src1:Single;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFMulToS(src0:PsrRegNode;src1:Single;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  function  OpUToF(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpSToF(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpUToU(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpSToS(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpFToF(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  procedure OpNot(dst:PsrRegSlot;src:PsrRegNode);
  procedure OpLogicalNot(dst:PsrRegSlot;src:PsrRegNode);
  procedure OpBitwiseOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpBitwiseXor(dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpLogicalOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpBitwiseAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpLogicalAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
  //
  function  OpNotTo(src:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpBitwiseOrTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpBitwiseAndTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
  function  OpBitwiseAndTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  function  OpImageSampleImplicitLod(pLine:PspirvOp;img:PsrNode;dst,coord:PsrRegNode):PSpirvOp;
  function  OpImageSampleExplicitLod(pLine:PspirvOp;img:PsrNode;dst,coord:PsrRegNode):PSpirvOp;
  function  OpImageFetch(pLine:PspirvOp;Tgrp:PsrNode;dst,coord:PsrRegNode):PSpirvOp;
  function  OpImageRead(pLine:PspirvOp;Tgrp:PsrNode;dst,idx:PsrRegNode):PspirvOp;
  function  OpImageWrite(pLine:PspirvOp;Tgrp:PsrNode;idx,src:PsrRegNode):PspirvOp;
 end;

function isPowerOfTwo(x:QWORD):Boolean; inline;
function fastIntLog2(i:QWORD):QWORD; inline;

implementation

function isPowerOfTwo(x:QWORD):Boolean; inline;
begin
 Result:=((x-1) and x)=0;
end;

function fastIntLog2(i:QWORD):QWORD; inline;
begin
 Result:=BsfQWORD(i);
end;

procedure _set_line(ppLine:PPspirvOp;pLine:PspirvOp);
begin
 if (ppLine=nil) then Exit;
 ppLine^:=pLine;
end;

//

function TEmitOp._Op1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src);
 Result:=node;
end;

function TEmitOp._Op2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src0);
 node^.AddParam(src1);
 Result:=node;
end;

function TEmitOp._Op3(pLine:PspirvOp;OpId:DWORD;dst,src0,src1,src2:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src0);
 node^.AddParam(src1);
 node^.AddParam(src2);
 Result:=node;
end;

function TEmitOp._Op4(pLine:PspirvOp;OpId:DWORD;dst,src0,src1,src2,src3:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src0);
 node^.AddParam(src1);
 node^.AddParam(src2);
 node^.AddParam(src3);
 Result:=node;
end;

//

function TEmitOp._OpGlsl1(pLine:PspirvOp;OpId:DWORD;dst,src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSGlslOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src);
 Result:=node;
end;

function TEmitOp._OpGlsl2(pLine:PspirvOp;OpId:DWORD;dst,src0,src1:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSGlslOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src0);
 node^.AddParam(src1);
 Result:=node;
end;

function TEmitOp._OpGlsl3(pLine:PspirvOp;OpId:DWORD;dst,src0,src1,src2:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSGlslOp(pLine,OpId); //need first
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src0);
 node^.AddParam(src1);
 node^.AddParam(src2);
 Result:=node;
end;

//

function TEmitOp.Op1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
begin
 Result:=_Op1(line,OpId,dst^.New(line,rtype),src);
end;

function TEmitOp.Op2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_Op2(line,OpId,dst^.New(line,rtype),src0,src1);
end;

function TEmitOp.Op3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
begin
 Result:=_Op3(line,OpId,dst^.New(line,rtype),src0,src1,src2);
end;

function TEmitOp.Op4(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2,src3:PsrRegNode):PSpirvOp;
begin
 Result:=_Op4(line,OpId,dst^.New(line,rtype),src0,src1,src2,src3);
end;

function TEmitOp.OpGlsl1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:PsrRegNode):PSpirvOp;
begin
 Result:=_OpGlsl1(line,OpId,dst^.New(line,rtype),src);
end;

function TEmitOp.OpGlsl2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_OpGlsl2(line,OpId,dst^.New(line,rtype),src0,src1)
end;

function TEmitOp.OpGlsl3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:PsrRegNode):PSpirvOp;
begin
 Result:=_OpGlsl3(line,OpId,dst^.New(line,rtype),src0,src1,src2);
end;

function TEmitOp.OpBitcast(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
begin
 Result:=_Op1(pLine,Op.OpBitcast,dst,src);
end;

function TEmitOp.OpBoolToInt(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
Var
 src0,src1:PsrRegNode;
begin
 src0:=NewReg_q(dst^.dtype,0,@pLine);
 src1:=NewReg_q(dst^.dtype,1,@pLine);

 Result:=_Op3(pLine,Op.OpSelect,dst,src,src1,src0);
end;

function TEmitOp.OpIntToBool(pLine:PspirvOp;dst,src:PsrRegNode):PSpirvOp;
Var
 src0:PsrRegNode;
begin
 src0:=NewReg_q(src^.dtype,0,@pLine);

 Result:=_Op2(pLine,Op.OpINotEqual,dst,src,src0);
end;

function TEmitOp.OpCast(nLine,dst,src:PsrNode):PsrNode;
var
 rdst,rsrc,rtmp:PsrRegNode;
 pLine:PspirvOp;
begin
 pLine:=nLine^.AsType(ntOpCustom);
 rdst :=  dst^.AsType(ntReg);
 rsrc :=  src^.AsType(ntReg);

 Assert(pLine<>nil);
 Assert(dst<>nil);
 Assert(src<>nil);

 if (rsrc^.dtype=dtBool) then
 begin
  Case rdst^.dtype of
   dtInt32,
   dtUint32:Result:=OpBoolToInt(pLine,rdst,rsrc);
   else
    begin
     rtmp:=NewReg(dtUint32);
     OpBoolToInt(pLine,rtmp,rsrc);
     Result:=OpBitcast(pLine,rdst,rtmp);
    end;
  end;
 end else
 if (rdst^.dtype=dtBool) then
 begin
  Case rsrc^.dtype of
   dtInt32,
   dtUint32:Result:=OpIntToBool(pLine,rdst,rsrc);
   else
    begin
     rtmp:=NewReg(dtUint32);
     OpBitcast(pLine,rtmp,rsrc);
     Result:=OpIntToBool(pLine,rdst,rtmp);
    end;
  end;
 end else
 begin
  Result:=OpBitcast(pLine,rdst,rsrc);
 end;
end;

function TEmitOp.OpLoad(nLine,dtype,dst,src:PsrNode):PsrNode;
var
 pLine:PspirvOp;
 pType:PsrType;
 node:PSpirvOp;
begin
 pLine:=nLine^.AsType(ntOpCustom);
 pType:=dtype^.AsType(ntType);
 Assert(dst<>nil);
 Assert(src<>nil);
 Assert(pLine<>nil);
 //Assert(pType<>nil);
 node:=AddSpirvOp(pLine,Op.OpLoad);
 node^.pType:=pType;
 node^.pDst :=dst;
 node^.AddParam(src);
 Result:=node;
end;

function TEmitOp.OpStore(nLine,dst,src:PsrNode):PsrNode;
Var
 pLine:PspirvOp;
 node:PSpirvOp;
begin
 pLine:=nLine^.AsType(ntOpCustom);
 Assert(dst<>nil);
 Assert(src<>nil);
 Assert(pLine<>nil);
 node:=AddSpirvOp(pLine,Op.OpStore);
 node^.pDst:=dst; //write
 node^.AddParam(src);
 Result:=node;
end;

function TEmitOp.OpLoad(pLine:PspirvOp;dst:PsrRegNode;src:PsrNode):PSpirvOp;
Var
 dtype:PsrType;
begin
 dtype:=TypeList.Fetch(dst^.dtype);
 Result:=PSpirvOp(OpLoad(pLine,dtype,dst,src));
end;

function TEmitOp.OpExtract(pLine:PspirvOp;dst,src:PsrRegNode;id:DWORD):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpCompositeExtract);
 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;
 node^.AddParam(src);
 node^.AddLiteral(id);
 Result:=node;
end;

function TEmitOp.OpConstruct(pLine:PspirvOp;dst:PsrRegNode):PSpirvOp;
var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpCompositeConstruct);
 node^.pType:=TypeList.Fetch(dst^.dtype);
 Assert(node^.pType<>nil);
 node^.pDst:=dst;
 Result:=node;
 //child add later
end;

function TEmitOp.OpAccessChain(pLine:PspirvOp;vType:PsrType;dst:PsrChain;src:PsrVariable):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpAccessChain);
 Assert(vType<>nil);
 node^.pType:=TypeList.FetchPointer(vType,src^.GetStorageClass);
 node^.pDst:=dst;
 node^.AddParam(src); //base
 //dst^.pLine:=node;
 Result:=node;
 //index add later
end;

function TEmitOp.OpCondMerge(pLine,pLabel:PspirvOp):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpSelectionMerge);
 node^.AddParam(pLabel^.pDst);
 node^.AddLiteral(SelectionControl.None,'None');
 Result:=node;
end;

function TEmitOp.OpLoopMerge(pLine,pLabel0,pLabel1:PspirvOp):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpLoopMerge);
 node^.AddParam(pLabel0^.pDst);
 node^.AddParam(pLabel1^.pDst);
 node^.AddLiteral(LoopControl.None,'None');
 Result:=node;
end;

function TEmitOp.OpBranch(pLine,pLabel:PspirvOp):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBranch);
 node^.AddParam(pLabel^.pDst);
 Result:=node;
end;

function TEmitOp.OpBranchCond(pLine,pLabel0,pLabel1:PspirvOp;src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBranchConditional);
 node^.AddParam(src);
 node^.AddParam(pLabel0^.pDst);
 node^.AddParam(pLabel1^.pDst);
 Result:=node;
end;

function TEmitOp.OpReturnValue(pLine:PspirvOp;src:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpReturnValue);
 node^.AddParam(src);
 Result:=node;
end;

//

procedure TEmitOp.OpFmaF32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
begin
 //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
 //NoContraction  decoration
 OpGlsl3(GlslOp.Fma,dtFloat32,dst,src0,src1,src2);
end;

procedure TEmitOp.OpFmaI32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
var
 mul,sum:PsrRegNode;
begin
 //vdst = vsrc0.i * vsrc1.i + vdst.i
 mul:=NewReg(dtInt32);
 sum:=dst^.New(line,dtInt32);

 _Op2(line,Op.OpIMul,mul,src0,src1);
 _Op2(line,Op.OpIAdd,sum,mul,src2);
end;

procedure TEmitOp.OpFmaU32(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
var
 mul,sum:PsrRegNode;
begin
 //vdst = vsrc0.u * vsrc1.u + vdst.u
 mul:=NewReg(dtUInt32);
 sum:=dst^.New(line,dtUInt32);

 _Op2(line,Op.OpIMul,mul,src0,src1);
 _Op2(line,Op.OpIAdd,sum,mul,src2);
end;

//

procedure TEmitOp.OpSelect(dst:PsrRegSlot;src0,src1,cond:PsrRegNode);
begin
 Op3(Op.OpSelect,LazyType2(src0^.dtype,src1^.dtype),dst,cond,src1,src0);
end;

procedure TEmitOp.OpIAddCar(pLine:PspirvOp;dst,car,src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
 rsl:PsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpIAddCarry); //need first
 node^.AddParam(src0);
 node^.AddParam(src1);

 rsl:=NewReg(dtStruct2u);

 node^.pType:=TypeList.Fetch(dtStruct2u);
 node^.pDst:=rsl;

 dst:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,dst);
 car:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,car);

 pLine:=node;
 pLine:=OpExtract(pLine,dst,rsl,0);
 pLine:=OpExtract(pLine,car,rsl,1);
end;

procedure TEmitOp.OpIAddExt(dst,car:PsrRegSlot;src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
 rsl:PsrRegPair;
begin
 node:=AddSpirvOp(srOpUtils.OpIAddExt); //need first
 node^.AddParam(src0);
 node^.AddParam(src1);

 rsl:=NewRegPair;
 rsl^.pWriter:=node;
 node^.pDst:=rsl;

 rsl^.pDst0:=dst^.New(line,dtUint32);
 rsl^.pDst1:=car^.New(line,dtUint32);
end;

procedure TEmitOp.OpISubBor(pLine:PspirvOp;dst,bor,src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
 rsl:PsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpISubBorrow); //need first
 node^.AddParam(src0);
 node^.AddParam(src1);

 rsl:=NewReg(dtStruct2u);

 node^.pType:=TypeList.Fetch(dtStruct2u);
 node^.pDst:=rsl;

 dst:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,dst);
 bor:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,bor);

 pLine:=node;
 pLine:=OpExtract(pLine,dst,rsl,0);
 pLine:=OpExtract(pLine,bor,rsl,1);
end;

procedure TEmitOp.OpISubExt(dst,bor:PsrRegSlot;src0,src1:PsrRegNode);
Var
 node:PSpirvOp;
 rsl:PsrRegPair;
begin
 node:=AddSpirvOp(srOpUtils.OpISubExt); //need first
 node^.AddParam(src0);
 node^.AddParam(src1);

 rsl:=NewRegPair;
 rsl^.pWriter:=node;
 node^.pDst:=rsl;

 rsl^.pDst0:=dst^.New(line,dtUint32);
 rsl^.pDst1:=bor^.New(line,dtUint32);
end;

//

function TEmitOp.OpAbsDiff(pLine:PspirvOp;dst,src0,src1:PsrRegNode):PSpirvOp;
begin
 Result:=_Op2(pLine,srOpUtils.OpAbsDiff,dst,src0,src1);
end;

procedure TEmitOp.OpWQM32(dst:PsrRegSlot;src:PsrRegNode);
begin
 Op1(srOpUtils.OpWQM32,dtUnknow,dst,src);
end;

procedure TEmitOp.OpBFEU32(dst:PsrRegSlot;base,src0,src1:PsrRegNode);
begin
 Op3(srOpUtils.OpBFEU32,dtUint32,dst,base,src0,src1);
end;

procedure TEmitOp.OpBFIB32(dst:PsrRegSlot;bitmsk,src0,src1:PsrRegNode);
begin
 Op3(srOpUtils.OpBFIB32,dtUint32,dst,bitmsk,src0,src1);
end;

function TEmitOp.OpBFITo(src0,src1,src2,src3:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_Op4(_get_line(ppLine),Op.OpBitFieldInsert,Result,src0,src1,src2,src3));
end;

//

procedure TEmitOp.OpPackAnc(dst:PsrRegSlot;prim,smid,rtid:PsrRegNode);
begin
 Op3(srOpUtils.OpPackAnc,dtUint32,dst,prim,smid,rtid);
end;

//

function TEmitOp.OpUMinTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.UMin,Result,src0,src1));
end;

function TEmitOp.OpUMaxTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.UMax,Result,src0,src1));
end;

function TEmitOp.OpFMinTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.FMin,Result,src0,src1));
end;

function TEmitOp.OpFMaxTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.FMax,Result,src0,src1));
end;

function TEmitOp.OpNMinTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.NMin,Result,src0,src1));
end;

function TEmitOp.OpNMaxTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.NMax,Result,src0,src1));
end;

procedure TEmitOp.OpMED3F(dst:PsrRegSlot;src0,src1,src2:PsrRegNode);
var
 min:PsrRegNode;
 max:PsrRegNode;
 mmx:PsrRegNode;
begin
 min:=OpFMinTo(src0,src1); //min(s0,s1)
 max:=OpNMaxTo(src0,src1); //max(s0,s1)
 mmx:=OpFMinTo(max ,src2); //min(max(s0,s1),s2)

 OpGlsl2(GlslOp.NMax,src0^.dtype,dst,min,mmx); //max(min(s0,s1),min(max(s0,s1),s2))
end;

function TEmitOp.OpPackOfs(pLine:PspirvOp;rtype:TsrDataType;count:Byte;src:PsrRegNode):PsrRegNode;
Var
 p:PsrCacheOp;
 dst:PsrRegNode;
 node:PSpirvOp;
begin
 Assert(count<>0);
 Assert(src<>nil);

 pLine:=GetMaxPlace(pLine,1,@src);

 p:=CacheOpList.Fetch(pLine^.Parent,srOpUtils.OpPackOfs,rtype,1,@src);

 if (p^.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,srOpUtils.OpPackOfs); //need first

  dst:=NewReg(rtype);

  node^.pDst:=dst;
  node^.AddLiteral(count);
  node^.AddParam(src);

  p^.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=PsrRegNode(p^.pDst);
 end;
end;

function TEmitOp.OpMakeCon(pLine:PspirvOp;dst:PsrRegNode;src:PPsrRegNode):PSpirvOp;
Var
 p:PsrCacheOp;
 node:PSpirvOp;
 rtype:TsrDataType;
 i:Byte;
begin
 Result:=pLine;
 Assert(src<>nil);

 rtype:=dst^.dtype;

 pLine:=GetMaxPlace(pLine,rtype.Count,src);

 p:=CacheOpList.Fetch(pLine^.Parent,Op.OpCompositeConstruct,rtype,rtype.Count,src);

 if (p^.pDst=nil) then
 begin
  node:=OpConstruct(pLine,dst);
  For i:=0 to rtype.Count-1 do
  begin
   node^.AddParam(src[i]);
  end;
  p^.pDst:=dst; //save
  Result:=node;
 end else
 begin
  dst^.pWriter:=p^.pDst;
 end;
end;

function TEmitOp.OpMakeVec(pLine:PspirvOp;rtype:TsrDataType;src:PPsrRegNode):PsrRegNode;
Var
 p:PsrCacheOp;
 dst:PsrRegNode;
 node:PSpirvOp;
 i:Byte;
begin
 Assert(src<>nil);

 pLine:=GetMaxPlace(pLine,rtype.Count,src);

 p:=CacheOpList.Fetch(pLine^.Parent,srOpUtils.OpMakeVec,rtype,rtype.Count,src);

 if (p^.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,srOpUtils.OpMakeVec); //need first

  dst:=NewReg(rtype);

  node^.pDst:=dst;

  For i:=0 to rtype.Count-1 do
  begin
   node^.AddParam(src[i]);
  end;

  p^.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=PsrRegNode(p^.pDst);
 end;
end;

//x,y,face,slice
function TEmitOp.OpMakeCub(pLine:PspirvOp;rtype:TsrDataType;src:PPsrRegNode):PsrRegNode;
Var
 p:PsrCacheOp;
 dst:PsrRegNode;
 node:PSpirvOp;
 i:Byte;
begin
 Assert(src<>nil);

 pLine:=GetMaxPlace(pLine,rtype.Count,src);

 p:=CacheOpList.Fetch(pLine^.Parent,srOpUtils.OpMakeCub,rtype,rtype.Count,src);

 if (p^.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,srOpUtils.OpMakeCub); //need first

  dst:=NewReg(rtype);

  node^.pDst:=dst;

  For i:=0 to rtype.Count-1 do
  begin
   node^.AddParam(src[i]);
  end;

  p^.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=PsrRegNode(p^.pDst);
 end;
end;

function TEmitOp.OpSampledImage(pLine:PspirvOp;Tgrp,Sgrp:PsrNode;dtype:TsrDataType;info:TsrTypeImageInfo):PsrRegNode;
Var
 src:array[0..1] of PsrNode;
 pType:PsrType;
 p:PsrCacheOp;
 dst:PsrRegNode;
 node:PSpirvOp;
begin
 src[0]:=Tgrp;
 src[1]:=Sgrp;

 p:=CacheOpList.Fetch(pLine^.Parent,Op.OpSampledImage,dtTypeSampledImage,2,@src);

 if (p^.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,Op.OpSampledImage); //need first

  dst:=NewReg(dtTypeSampledImage);

  pType:=TypeList.Fetch(dtype);
  pType:=TypeList.FetchImage(pType,info);
  pType:=TypeList.FetchSampledImage(pType);

  node^.pType:=pType;
  node^.pDst:=dst;

  node^.AddParam(Tgrp);
  node^.AddParam(Sgrp);

  p^.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=PsrRegNode(p^.pDst);
 end;
end;

//

procedure TEmitOp.OpIAdd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  Op2(Op.OpIAdd,dtInt32,dst,src0,src1);
 end else
 begin
  Op2(Op.OpIAdd,dtUInt32,dst,src0,src1);
 end;
end;

procedure TEmitOp.OpISub(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  Op2(Op.OpISub,dtInt32,dst,src0,src1);
 end else
 begin
  Op2(Op.OpISub,dtUInt32,dst,src0,src1);
 end;
end;

procedure TEmitOp.OpIMul(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  Op2(Op.OpIMul,dtInt32,dst,src0,src1);
 end else
 begin
  Op2(Op.OpIMul,dtUInt32,dst,src0,src1);
 end;
end;

//

function TEmitOp.OpShlTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpShiftLeftLogical,Result,src0,src1));
end;

function TEmitOp.OpShlTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpShlTo(src0,NewReg_q(src0^.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpShrTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 if (src0^.dtype.Sign<>0) then
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpShiftRightArithmetic,Result,src0,src1));
 end else
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpShiftRightLogical   ,Result,src0,src1));
 end;
end;

function TEmitOp.OpShrTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpShrTo(src0,NewReg_q(src0^.dtype,src1,ppLine),ppLine);
end;

//

function TEmitOp.OpIAddTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  Result:=NewReg(dtInt32);
 end else
 begin
  Result:=NewReg(dtUInt32);
 end;

 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpIAdd,Result,src0,src1));
end;

function TEmitOp.OpIAddTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpIAddTo(src0,NewReg_q(src0^.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpISubTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  Result:=NewReg(dtInt32);
 end else
 begin
  Result:=NewReg(dtUInt32);
 end;

 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpISub,Result,src0,src1));
end;

function TEmitOp.OpISubTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpISubTo(src0,NewReg_q(src0^.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpIMulTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 if (src0^.dtype=dtInt32) or (src1^.dtype=dtInt32) then
 begin
  Result:=NewReg(dtInt32);
 end else
 begin
  Result:=NewReg(dtUInt32);
 end;

 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpIMul,Result,src0,src1));
end;

function TEmitOp.OpIMulTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1<=1) then Exit(src0);

 if isPowerOfTwo(src1) then
 begin
  src1:=fastIntLog2(src1);
  Result:=OpShlTo (src0,src1,ppLine);
 end else
 begin
  Result:=OpIMulTo(src0,src1,ppLine);
 end;
end;

function TEmitOp.OpIDivTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 if (src0^.dtype.Sign<>0) then
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpSDiv,Result,src0,src1));
 end else
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpUDiv,Result,src0,src1));
 end;
end;

function TEmitOp.OpIDivTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1<=1) then Exit(src0);

 if isPowerOfTwo(src1) then
 begin
  src1:=fastIntLog2(src1);
  Result:=OpShrTo (src0,src1,ppLine);
 end else
 begin
  Result:=OpIDivTo(src0,src1,ppLine);
 end;
end;

//

function TEmitOp.OpFAddTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFAdd,Result,src0,src1));
end;

function TEmitOp.OpFSubTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFSub,Result,src0,src1));
end;

function TEmitOp.OpFMulTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFMul,Result,src0,src1));
end;

function TEmitOp.OpFDivTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0^.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFDiv,Result,src0,src1));
end;

//

function TEmitOp.OpFAddToS(src0:PsrRegNode;src1:Single;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpFAddTo(src0,NewReg_s(src0^.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpFMulToS(src0:PsrRegNode;src1:Single;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) or (src1=0) or (src1=1) then Exit(src0);
 Result:=OpFMulTo(src0,NewReg_s(src0^.dtype,src1,ppLine),ppLine);
end;

//

function TEmitOp.OpUToF(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpConvertUToF,Result,src);
end;

function TEmitOp.OpSToF(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpConvertSToF,Result,src);
end;

function TEmitOp.OpUToU(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpUConvert,Result,src);
end;

function TEmitOp.OpSToS(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpSConvert,Result,src);
end;

function TEmitOp.OpFToF(src:PsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpFConvert,Result,src);
end;

//

procedure TEmitOp.OpNot(dst:PsrRegSlot;src:PsrRegNode);
begin
 Op1(Op.OpNot,dtUnknow,dst,src); //post type
end;

procedure TEmitOp.OpLogicalNot(dst:PsrRegSlot;src:PsrRegNode);
begin
 Op1(Op.OpLogicalNot,dtBool,dst,src);
end;

procedure TEmitOp.OpBitwiseOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 Op2(Op.OpBitwiseOr,dtUnknow,dst,src0,src1); //post type
end;

procedure TEmitOp.OpBitwiseXor(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 Op2(Op.OpBitwiseXor,dtUint32,dst,src0,src1); //post need?
end;

procedure TEmitOp.OpLogicalOr(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 Op2(Op.OpLogicalOr,dtBool,dst,src0,src1);
end;

procedure TEmitOp.OpBitwiseAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 Op2(Op.OpBitwiseAnd,dtUnknow,dst,src0,src1); //post type
end;

procedure TEmitOp.OpLogicalAnd(dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 Op2(Op.OpLogicalAnd,dtBool,dst,src0,src1);
end;

//

function TEmitOp.OpNotTo(src:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(dtUnknow);
 _set_line(ppLine,_Op1(_get_line(ppLine),Op.OpNot,Result,src)); //post type
end;

function TEmitOp.OpBitwiseOrTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(dtUnknow);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpBitwiseOr,Result,src0,src1)); //post type
end;

function TEmitOp.OpBitwiseAndTo(src0,src1:PsrRegNode;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(dtUnknow);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpBitwiseAnd,Result,src0,src1)); //post type
end;

function TEmitOp.OpBitwiseAndTo(src0:PsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (src0=nil) then Exit(src0);
 Result:=OpBitwiseAndTo(src0,NewReg_q(src0^.dtype,src1,ppLine),ppLine);
end;

//

function TEmitOp.OpImageSampleImplicitLod(pLine:PspirvOp;img:PsrNode;dst,coord:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageSampleImplicitLod); //need first

 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;

 node^.AddParam(img);
 node^.AddParam(coord);

 Result:=node;
end;

function TEmitOp.OpImageSampleExplicitLod(pLine:PspirvOp;img:PsrNode;dst,coord:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageSampleExplicitLod); //need first

 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;

 node^.AddParam(img);
 node^.AddParam(coord);

 Result:=node;
end;

function TEmitOp.OpImageFetch(pLine:PspirvOp;Tgrp:PsrNode;dst,coord:PsrRegNode):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageFetch); //need first

 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;

 node^.AddParam(Tgrp);
 node^.AddParam(coord);

 Result:=node;
end;

function TEmitOp.OpImageRead(pLine:PspirvOp;Tgrp:PsrNode;dst,idx:PsrRegNode):PspirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageRead); //need first

 node^.pType:=TypeList.Fetch(dst^.dtype);
 node^.pDst:=dst;

 node^.AddParam(Tgrp);

 if (idx<>nil) then
 begin
  node^.AddParam(idx);
 end else
 begin
  node^.AddParam(ConstList.Fetch_i(dtUint32,0));
 end;

 Result:=node;
end;

function TEmitOp.OpImageWrite(pLine:PspirvOp;Tgrp:PsrNode;idx,src:PsrRegNode):PspirvOp;
Var
 node:PSpirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageWrite); //need first
 node^.pDst:=Tgrp; //write

 if (idx<>nil) then
 begin
  node^.AddParam(idx);
 end else
 begin
  node^.AddParam(ConstList.Fetch_i(dtUint32,0));
 end;

 node^.AddParam(src);

 Result:=node;
end;


end.

