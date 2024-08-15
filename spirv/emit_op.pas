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
 srOp,
 srCFGLabel,
 srOpUtils,
 srCacheOp,
 srInterface;

type
 TEmitOp=class(TEmitInterface)
  //
  function  _Op1(pLine:TspirvOp;OpId:DWORD;dst,src:TsrRegNode):TspirvOp;
  function  _Op2(pLine:TspirvOp;OpId:DWORD;dst,src0,src1:TsrRegNode):TspirvOp;
  function  _Op3(pLine:TspirvOp;OpId:DWORD;dst,src0,src1,src2:TsrRegNode):TspirvOp;
  function  _Op4(pLine:TspirvOp;OpId:DWORD;dst,src0,src1,src2,src3:TsrRegNode):TspirvOp;
  //
  function  _OpGlsl1(pLine:TspirvOp;OpId:DWORD;dst,src:TsrRegNode):TspirvOp;
  function  _OpGlsl2(pLine:TspirvOp;OpId:DWORD;dst,src0,src1:TsrRegNode):TspirvOp;
  function  _OpGlsl3(pLine:TspirvOp;OpId:DWORD;dst,src0,src1,src2:TsrRegNode):TspirvOp;
  //
  function  Op1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:TsrRegNode):TspirvOp;
  function  Op2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:TsrRegNode):TspirvOp;
  function  Op3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:TsrRegNode):TspirvOp;
  function  Op4(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2,src3:TsrRegNode):TspirvOp;
  //
  function  OpGlsl1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:TsrRegNode):TspirvOp;
  function  OpGlsl2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:TsrRegNode):TspirvOp;
  function  OpGlsl3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:TsrRegNode):TspirvOp;
  //
  function  OpBitcast(pLine:TspirvOp;dst,src:TsrRegNode):TspirvOp;
  function  OpBitcast(pLine:TspirvOp;pType:TsrType;dst,src:TsrNode):TspirvOp;
  function  OpBoolToInt(pLine:TspirvOp;dst,src:TsrRegNode):TspirvOp;
  function  OpIntToBool(pLine:TspirvOp;dst,src:TsrRegNode):TspirvOp;
  //
  function  OpCast (nLine,      dst,src:TsrNode):TsrNode; override;
  function  OpLoad (nLine,dtype,dst,src:TsrNode):TsrNode; override;
  function  OpStore(nLine,      dst,src:TsrNode):TsrNode; override;
  //
  function  OpLoad(pLine:TspirvOp;dst:TsrRegNode;src:TsrNode):TspirvOp;
  function  OpLoadTo(pType:TsrType;src:TsrNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpLine(nLine:TsrNode;iLine,iColumn:DWORD):TsrNode;
  //
  function  OpExtract(pLine:TspirvOp;dst,src:TsrRegNode;id:DWORD):TspirvOp;
  function  OpConstruct(pLine:TspirvOp;dst:TsrRegNode):TspirvOp;
  function  OpAccessChain(pLine:TspirvOp;vType:TsrType;dst,src:TsrNode):TspirvOp;
  function  OpAccessChainTo(vType:TsrType;src:TsrNode;idx0:TsrNode;ppLine:PPspirvOp=nil):TsrNode;
  function  OpCondMerge(pLine,pLabel:TspirvOp):TspirvOp;
  function  OpLoopMerge(pLine,pLabel0,pLabel1:TspirvOp):TspirvOp;
  function  OpBranch(pLine,pLabel:TspirvOp):TspirvOp;
  function  OpBranchCond(pLine,pLabel0,pLabel1:TspirvOp;src:TsrRegNode):TspirvOp;
  function  OpReturnValue(pLine:TspirvOp;src:TsrRegNode):TspirvOp;
  //
  function  OpReturn(pLine:TspirvOp):TspirvOp;
  function  OpFunctionEnd(pLine:TspirvOp):TspirvOp;
  function  OpEmitVertex(pLine:TspirvOp):TspirvOp;
  function  OpEndPrimitive(pLine:TspirvOp):TspirvOp;
  //
  procedure OpFmaF32(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
  procedure OpFmaI32(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
  procedure OpFmaU32(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
  //
  procedure OpSelect(dst:PsrRegSlot;src0,src1,cond:TsrRegNode);
  function  OpSelectTo(src0,src1,cond:TsrRegNode):TsrRegNode;
  //
  procedure OpIAddCar(pLine:TspirvOp;dst,car,src0,src1:TsrRegNode);
  procedure OpIAddExt(dst,car:PsrRegSlot;src0,src1:TsrRegNode);
  //
  procedure OpISubBor(pLine:TspirvOp;dst,bor,src0,src1:TsrRegNode);
  procedure OpISubExt(dst,bor:PsrRegSlot;src0,src1:TsrRegNode);
  //
  function  OpAbsDiff(pLine:TspirvOp;dst,src0,src1:TsrRegNode):TspirvOp;
  procedure OpWQM32(dst:PsrRegSlot;src:TsrRegNode);
  //
  procedure OpBFE_32(dst:PsrRegSlot;base,src0,src1:TsrRegNode);
  procedure OpBFIB32(dst:PsrRegSlot;bitmsk,src0,src1:TsrRegNode);
  //
  function  OpBFITo(src0,src1,src2,src3:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  procedure OpPackAnc(dst:PsrRegSlot;prim,smid,rtid:TsrRegNode);
  //
  function  OpSMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpSMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpUMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpUMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpNMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpNMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  procedure OpMED3I(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
  procedure OpMED3U(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
  procedure OpMED3F(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
  //
  function  OpPackOfs(pLine:TspirvOp;rtype:TsrDataType;count:Byte;src:TsrRegNode):TsrRegNode;
  function  OpMakeCon(pLine:TspirvOp;dst:TsrRegNode;src:PPsrRegNode):TspirvOp;
  function  OpMakeVec(pLine:TspirvOp;rtype:TsrDataType;src:PPsrRegNode):TsrRegNode;
  function  OpMakeCub(pLine:TspirvOp;rtype:TsrDataType;src:PPsrRegNode):TsrRegNode;
  function  OpSampledImage(pLine:TspirvOp;Tgrp,Sgrp:TsrNode;dtype:TsrDataType;info:TsrTypeImageInfo):TsrRegNode;
  //
  procedure OpIAdd(dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpISub(dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpIMul(dst:PsrRegSlot;src0,src1:TsrRegNode);
  //
  function  OpShlTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpShlTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpShrTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpShrTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpIAddTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpIAddTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpISubTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpISubTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpIMulTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpIMulTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpIDivTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpIDivTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpIModTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpFAddTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFSubTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFMulTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFDivTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpFAddToS(src0:TsrRegNode;src1:Single;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFMulToS(src0:TsrRegNode;src1:Single;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpUToF(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFToU(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpSToF(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpUToU(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpSToS(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpFToF(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpFloorTo(src:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpPowTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  procedure OpNot(dst:PsrRegSlot;src:TsrRegNode);
  procedure OpLogicalNot(dst:PsrRegSlot;src:TsrRegNode);
  procedure OpBitwiseOr(dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpBitwiseXor(dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpLogicalOr(dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpBitwiseAnd(dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpLogicalAnd(dst:PsrRegSlot;src0,src1:TsrRegNode);
  //
  function  OpNotTo(src:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpOrTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpAndTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  function  OpAndTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpBitCountTo(src:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  OpImageSampleImplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord:TsrRegNode):TspirvOp;
  function  OpImageSampleExplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord:TsrRegNode):TspirvOp;
  function  OpImageSampleDrefImplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord,pcf:TsrRegNode):TspirvOp;
  function  OpImageSampleDrefExplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord,pcf:TsrRegNode):TspirvOp;
  function  OpImageGather(pLine:TspirvOp;img:TsrNode;dst,coord:TsrRegNode;id:Byte):TspirvOp;
  function  OpImageDrefGather(pLine:TspirvOp;img:TsrNode;dst,coord,pcf:TsrRegNode):TspirvOp;
  function  OpImageFetch(pLine:TspirvOp;Tgrp:TsrNode;dst,coord:TsrRegNode):TspirvOp;
  function  OpImageRead(pLine:TspirvOp;Tgrp:TsrNode;dst,idx:TsrRegNode):TspirvOp;
  function  OpImageWrite(pLine:TspirvOp;Tgrp:TsrNode;idx,src:TsrRegNode):TspirvOp;
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

procedure _set_line(ppLine:PPspirvOp;pLine:TspirvOp);
begin
 if (ppLine=nil) then Exit;
 ppLine^:=pLine;
end;

//

function TEmitOp._Op1(pLine:TspirvOp;OpId:DWORD;dst,src:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src);
 Result:=node;
end;

function TEmitOp._Op2(pLine:TspirvOp;OpId:DWORD;dst,src0,src1:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src0);
 node.AddParam(src1);
 Result:=node;
end;

function TEmitOp._Op3(pLine:TspirvOp;OpId:DWORD;dst,src0,src1,src2:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src0);
 node.AddParam(src1);
 node.AddParam(src2);
 Result:=node;
end;

function TEmitOp._Op4(pLine:TspirvOp;OpId:DWORD;dst,src0,src1,src2,src3:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src0);
 node.AddParam(src1);
 node.AddParam(src2);
 node.AddParam(src3);
 Result:=node;
end;

//

function TEmitOp._OpGlsl1(pLine:TspirvOp;OpId:DWORD;dst,src:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSGlslOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src);
 Result:=node;
end;

function TEmitOp._OpGlsl2(pLine:TspirvOp;OpId:DWORD;dst,src0,src1:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSGlslOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src0);
 node.AddParam(src1);
 Result:=node;
end;

function TEmitOp._OpGlsl3(pLine:TspirvOp;OpId:DWORD;dst,src0,src1,src2:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSGlslOp(pLine,OpId); //need first
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src0);
 node.AddParam(src1);
 node.AddParam(src2);
 Result:=node;
end;

//

function TEmitOp.Op1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:TsrRegNode):TspirvOp;
begin
 Result:=_Op1(line,OpId,dst^.New(line,rtype),src);
end;

function TEmitOp.Op2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:TsrRegNode):TspirvOp;
begin
 Result:=_Op2(line,OpId,dst^.New(line,rtype),src0,src1);
end;

function TEmitOp.Op3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:TsrRegNode):TspirvOp;
begin
 Result:=_Op3(line,OpId,dst^.New(line,rtype),src0,src1,src2);
end;

function TEmitOp.Op4(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2,src3:TsrRegNode):TspirvOp;
begin
 Result:=_Op4(line,OpId,dst^.New(line,rtype),src0,src1,src2,src3);
end;

function TEmitOp.OpGlsl1(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src:TsrRegNode):TspirvOp;
begin
 Result:=_OpGlsl1(line,OpId,dst^.New(line,rtype),src);
end;

function TEmitOp.OpGlsl2(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1:TsrRegNode):TspirvOp;
begin
 Result:=_OpGlsl2(line,OpId,dst^.New(line,rtype),src0,src1)
end;

function TEmitOp.OpGlsl3(OpId:DWORD;rtype:TsrDataType;dst:PsrRegSlot;src0,src1,src2:TsrRegNode):TspirvOp;
begin
 Result:=_OpGlsl3(line,OpId,dst^.New(line,rtype),src0,src1,src2);
end;

function TEmitOp.OpBitcast(pLine:TspirvOp;dst,src:TsrRegNode):TspirvOp;
begin
 Result:=_Op1(pLine,Op.OpBitcast,dst,src);
end;

function TEmitOp.OpBitcast(pLine:TspirvOp;pType:TsrType;dst,src:TsrNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBitcast); //need first
 node.pType:=pType;
 node.pDst:=dst;
 node.AddParam(src);
 Result:=node;
end;

function TEmitOp.OpBoolToInt(pLine:TspirvOp;dst,src:TsrRegNode):TspirvOp;
Var
 src0,src1:TsrRegNode;
begin
 src0:=NewReg_q(dst.dtype,0,@pLine);
 src1:=NewReg_q(dst.dtype,1,@pLine);

 Result:=_Op3(pLine,Op.OpSelect,dst,src,src1,src0);
end;

function TEmitOp.OpIntToBool(pLine:TspirvOp;dst,src:TsrRegNode):TspirvOp;
Var
 src0:TsrRegNode;
begin
 src0:=NewReg_q(src.dtype,0,@pLine);

 Result:=_Op2(pLine,Op.OpINotEqual,dst,src,src0);
end;

function TEmitOp.OpCast(nLine,dst,src:TsrNode):TsrNode;
var
 rdst,rsrc,rtmp:TsrRegNode;
 pLine:TspirvOp;
begin
 pLine:=nLine.specialize AsType<ntOpCustom>;
 rdst :=dst.specialize AsType<ntReg>;
 rsrc :=src.specialize AsType<ntReg>;

 Assert(pLine<>nil);
 Assert(dst<>nil);
 Assert(src<>nil);

 if (rsrc.dtype=dtBool) then
 begin
  Case rdst.dtype of
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
 if (rdst.dtype=dtBool) then
 begin
  Case rsrc.dtype of
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

function TEmitOp.OpLoad(nLine,dtype,dst,src:TsrNode):TsrNode;
var
 pLine:TspirvOp;
 pType:TsrType;
 node:TspirvOp;
begin
 pLine:=nLine.specialize AsType<ntOpCustom>;
 pType:=dtype.specialize AsType<ntType>;
 Assert(dst<>nil);
 Assert(src<>nil);
 Assert(pLine<>nil);
 //Assert(pType<>nil);
 node:=AddSpirvOp(pLine,Op.OpLoad);
 node.pType:=pType;
 node.pDst :=dst;
 node.AddParam(src);
 Result:=node;
end;

function TEmitOp.OpStore(nLine,dst,src:TsrNode):TsrNode;
Var
 pLine:TspirvOp;
 node:TspirvOp;
begin
 pLine:=nLine.specialize AsType<ntOpCustom>;
 Assert(dst<>nil);
 Assert(src<>nil);
 Assert(pLine<>nil);
 node:=AddSpirvOp(pLine,Op.OpStore);
 node.AddParam(dst); //write
 node.AddParam(src);
 Result:=node;
end;

function TEmitOp.OpLoad(pLine:TspirvOp;dst:TsrRegNode;src:TsrNode):TspirvOp;
Var
 dtype:TsrType;
begin
 dtype:=TypeList.Fetch(dst.dtype);
 Result:=OpLoad(pLine,dtype,dst,src);
end;

function TEmitOp.OpLoadTo(pType:TsrType;src:TsrNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(pType.dtype);
 //
 _set_line(ppLine,OpLoad(_get_line(ppLine),pType,Result,src));
end;

function TEmitOp.OpLine(nLine:TsrNode;iLine,iColumn:DWORD):TsrNode;
var
 pLine:TspirvOp;
 node:TspirvOp;
 nFile:TsrNode;
begin
 pLine:=nLine.specialize AsType<ntOpCustom>;
 Assert(pLine<>nil);
 nFile:=DebugInfoList.FileName;
 node:=AddSpirvOp(pLine,Op.OpLine);
 node.AddParam(nFile);
 node.AddLiteral(iLine);
 node.AddLiteral(iColumn);
 Result:=node;
end;

function TEmitOp.OpExtract(pLine:TspirvOp;dst,src:TsrRegNode;id:DWORD):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(src.dtype.isVector);
 node:=AddSpirvOp(pLine,Op.OpCompositeExtract);
 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;
 node.AddParam(src);
 node.AddLiteral(id);
 Result:=node;
end;

function TEmitOp.OpConstruct(pLine:TspirvOp;dst:TsrRegNode):TspirvOp;
var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpCompositeConstruct);
 node.pType:=TypeList.Fetch(dst.dtype);
 Assert(node.pType<>nil);
 node.pDst:=dst;
 Result:=node;
 //child add later
end;

function TEmitOp.OpAccessChain(pLine:TspirvOp;vType:TsrType;dst,src:TsrNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpAccessChain);
 Assert(vType<>nil);
 node.pType:=TypeList.FetchPointer(vType,src.GetStorageClass);
 node.pDst:=dst;
 node.AddParam(src); //base
 Result:=node;
 //index add later
end;

function TEmitOp.OpAccessChainTo(vType:TsrType;src:TsrNode;idx0:TsrNode;ppLine:PPspirvOp=nil):TsrNode;
Var
 node:TspirvOp;
begin
 Result:=NewRefNode;
 //
 node:=OpAccessChain(_get_line(ppLine),vType,Result,src);
 node.AddParam(idx0);
 _set_line(ppLine,node);
end;

function TEmitOp.OpCondMerge(pLine,pLabel:TspirvOp):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpSelectionMerge);
 node.AddParam(pLabel.pDst);
 node.AddLiteral(SelectionControl.None,'None');
 Result:=node;
end;

function TEmitOp.OpLoopMerge(pLine,pLabel0,pLabel1:TspirvOp):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpLoopMerge);
 node.AddParam(pLabel0.pDst);
 node.AddParam(pLabel1.pDst);
 node.AddLiteral(LoopControl.None,'None');
 Result:=node;
end;

function TEmitOp.OpBranch(pLine,pLabel:TspirvOp):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBranch);
 node.AddParam(pLabel.pDst);
 Result:=node;
end;

function TEmitOp.OpBranchCond(pLine,pLabel0,pLabel1:TspirvOp;src:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpBranchConditional);
 node.AddParam(src);
 node.AddParam(pLabel0.pDst);
 node.AddParam(pLabel1.pDst);
 Result:=node;
end;

function TEmitOp.OpReturnValue(pLine:TspirvOp;src:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpReturnValue);
 node.AddParam(src);
 Result:=node;
end;

//

function TEmitOp.OpReturn(pLine:TspirvOp):TspirvOp;
begin
 Result:=AddSpirvOp(pLine,Op.OpReturn);
end;

function TEmitOp.OpFunctionEnd(pLine:TspirvOp):TspirvOp;
begin
 Result:=AddSpirvOp(pLine,Op.OpFunctionEnd);
end;

function TEmitOp.OpEmitVertex(pLine:TspirvOp):TspirvOp;
begin
 Result:=AddSpirvOp(pLine,Op.OpEmitVertex);
end;

function TEmitOp.OpEndPrimitive(pLine:TspirvOp):TspirvOp;
begin
 Result:=AddSpirvOp(pLine,Op.OpEndPrimitive);
end;

//

procedure TEmitOp.OpFmaF32(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
begin
 //vdst = vsrc0.f * vsrc1.f + vdst.f  -> fma
 //NoContraction  decoration
 OpGlsl3(GlslOp.Fma,dtFloat32,dst,src0,src1,src2);
end;

procedure TEmitOp.OpFmaI32(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
var
 mul,sum:TsrRegNode;
begin
 //vdst = vsrc0.i * vsrc1.i + vdst.i
 mul:=NewReg(dtInt32);
 sum:=dst^.New(line,dtInt32);

 _Op2(line,Op.OpIMul,mul,src0,src1);
 _Op2(line,Op.OpIAdd,sum,mul,src2);
end;

procedure TEmitOp.OpFmaU32(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
var
 mul,sum:TsrRegNode;
begin
 //vdst = vsrc0.u * vsrc1.u + vdst.u
 mul:=NewReg(dtUInt32);
 sum:=dst^.New(line,dtUInt32);

 _Op2(line,Op.OpIMul,mul,src0,src1);
 _Op2(line,Op.OpIAdd,sum,mul,src2);
end;

//

procedure TEmitOp.OpSelect(dst:PsrRegSlot;src0,src1,cond:TsrRegNode);
begin
 Op3(Op.OpSelect,LazyType2(src0.dtype,src1.dtype),dst,cond,src1,src0);
end;

function TEmitOp.OpSelectTo(src0,src1,cond:TsrRegNode):TsrRegNode;
begin
 Result:=NewReg(LazyType2(src0.dtype,src1.dtype));
 //
 _Op3(line,Op.OpSelect,Result,cond,src1,src0);
end;

procedure TEmitOp.OpIAddCar(pLine:TspirvOp;dst,car,src0,src1:TsrRegNode);
Var
 node:TspirvOp;
 rsl:TsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpIAddCarry); //need first
 node.AddParam(src0);
 node.AddParam(src1);

 rsl:=NewReg(dtStruct2u);

 node.pType:=TypeList.Fetch(dtStruct2u);
 node.pDst:=rsl;

 dst:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,dst);
 car:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,car);

 pLine:=node;
 pLine:=OpExtract(pLine,dst,rsl,0);
 pLine:=OpExtract(pLine,car,rsl,1);
end;

procedure TEmitOp.OpIAddExt(dst,car:PsrRegSlot;src0,src1:TsrRegNode);
Var
 node:TspirvOp;
 rsl:TsrRegPair;
begin
 node:=AddSpirvOp(srOpUtils.OpIAddExt); //need first
 node.AddParam(src0);
 node.AddParam(src1);

 rsl:=NewRegPair;
 rsl.pWriter:=node;
 node.pDst:=rsl;

 rsl.pDst0:=dst^.New(line,dtUint32);
 rsl.pDst1:=car^.New(line,dtUint32);
end;

procedure TEmitOp.OpISubBor(pLine:TspirvOp;dst,bor,src0,src1:TsrRegNode);
Var
 node:TspirvOp;
 rsl:TsrRegNode;
begin
 node:=AddSpirvOp(pLine,Op.OpISubBorrow); //need first
 node.AddParam(src0);
 node.AddParam(src1);

 rsl:=NewReg(dtStruct2u);

 node.pType:=TypeList.Fetch(dtStruct2u);
 node.pDst:=rsl;

 dst:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,dst);
 bor:=BitcastList.FetchDstr(TsrDataType(dtStruct2u).Child,bor);

 pLine:=node;
 pLine:=OpExtract(pLine,dst,rsl,0);
 pLine:=OpExtract(pLine,bor,rsl,1);
end;

procedure TEmitOp.OpISubExt(dst,bor:PsrRegSlot;src0,src1:TsrRegNode);
Var
 node:TspirvOp;
 rsl:TsrRegPair;
begin
 node:=AddSpirvOp(srOpUtils.OpISubExt); //need first
 node.AddParam(src0);
 node.AddParam(src1);

 rsl:=NewRegPair;
 rsl.pWriter:=node;
 node.pDst:=rsl;

 rsl.pDst0:=dst^.New(line,dtUint32);
 rsl.pDst1:=bor^.New(line,dtUint32);
end;

//

function TEmitOp.OpAbsDiff(pLine:TspirvOp;dst,src0,src1:TsrRegNode):TspirvOp;
begin
 Result:=_Op2(pLine,srOpUtils.OpAbsDiff,dst,src0,src1);
end;

procedure TEmitOp.OpWQM32(dst:PsrRegSlot;src:TsrRegNode);
begin
 Op1(srOpUtils.OpWQM32,dtUnknow,dst,src);
end;

procedure TEmitOp.OpBFE_32(dst:PsrRegSlot;base,src0,src1:TsrRegNode);
begin
 Op3(srOpUtils.OpBFE_32,base.dtype,dst,base,src0,src1);
end;

procedure TEmitOp.OpBFIB32(dst:PsrRegSlot;bitmsk,src0,src1:TsrRegNode);
begin
 Op3(srOpUtils.OpBFIB32,dtUint32,dst,bitmsk,src0,src1);
end;

function TEmitOp.OpBFITo(src0,src1,src2,src3:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_Op4(_get_line(ppLine),Op.OpBitFieldInsert,Result,src0,src1,src2,src3));
end;

//

procedure TEmitOp.OpPackAnc(dst:PsrRegSlot;prim,smid,rtid:TsrRegNode);
begin
 Op3(srOpUtils.OpPackAnc,dtUint32,dst,prim,smid,rtid);
end;

//

function TEmitOp.OpSMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.SMin,Result,src0,src1));
end;

function TEmitOp.OpSMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.SMax,Result,src0,src1));
end;

function TEmitOp.OpUMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.UMin,Result,src0,src1));
end;

function TEmitOp.OpUMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.UMax,Result,src0,src1));
end;

function TEmitOp.OpFMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.FMin,Result,src0,src1));
end;

function TEmitOp.OpFMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.FMax,Result,src0,src1));
end;

function TEmitOp.OpNMinTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.NMin,Result,src0,src1));
end;

function TEmitOp.OpNMaxTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.NMax,Result,src0,src1));
end;

procedure TEmitOp.OpMED3I(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
var
 min:TsrRegNode;
 max:TsrRegNode;
 mmx:TsrRegNode;
begin
 min:=OpSMinTo(src0,src1); //min(s0,s1)
 max:=OpSMaxTo(src0,src1); //max(s0,s1)
 mmx:=OpSMinTo(max ,src2); //min(max(s0,s1),s2)

 OpGlsl2(GlslOp.SMax,src0.dtype,dst,min,mmx); //max(min(s0,s1),min(max(s0,s1),s2))
end;

procedure TEmitOp.OpMED3U(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
var
 min:TsrRegNode;
 max:TsrRegNode;
 mmx:TsrRegNode;
begin
 min:=OpUMinTo(src0,src1); //min(s0,s1)
 max:=OpUMaxTo(src0,src1); //max(s0,s1)
 mmx:=OpUMinTo(max ,src2); //min(max(s0,s1),s2)

 OpGlsl2(GlslOp.UMax,src0.dtype,dst,min,mmx); //max(min(s0,s1),min(max(s0,s1),s2))
end;

procedure TEmitOp.OpMED3F(dst:PsrRegSlot;src0,src1,src2:TsrRegNode);
var
 min:TsrRegNode;
 max:TsrRegNode;
 mmx:TsrRegNode;
begin
 min:=OpFMinTo(src0,src1); //min(s0,s1)
 max:=OpNMaxTo(src0,src1); //max(s0,s1)
 mmx:=OpFMinTo(max ,src2); //min(max(s0,s1),s2)

 OpGlsl2(GlslOp.NMax,src0.dtype,dst,min,mmx); //max(min(s0,s1),min(max(s0,s1),s2))
end;

function TEmitOp.OpPackOfs(pLine:TspirvOp;rtype:TsrDataType;count:Byte;src:TsrRegNode):TsrRegNode;
Var
 p:TsrCacheOp;
 dst:TsrRegNode;
 node:TspirvOp;
begin
 Assert(count<>0);
 Assert(src<>nil);

 pLine:=GetMaxPlace(pLine,1,@src);

 p:=CacheOpList.Fetch(pLine.Parent,srOpUtils.OpPackOfs,rtype,1,@src);

 if (p.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,srOpUtils.OpPackOfs); //need first

  dst:=NewReg(rtype);

  node.pDst:=dst;
  node.AddLiteral(count);
  node.AddParam(src);

  p.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=p.pDst;
 end;
end;

function TEmitOp.OpMakeCon(pLine:TspirvOp;dst:TsrRegNode;src:PPsrRegNode):TspirvOp;
Var
 p:TsrCacheOp;
 node:TspirvOp;
 rtype:TsrDataType;
 i:Byte;
begin
 Result:=pLine;
 Assert(src<>nil);

 rtype:=dst.dtype;

 pLine:=GetMaxPlace(pLine,rtype.Count,src);

 p:=CacheOpList.Fetch(pLine.Parent,Op.OpCompositeConstruct,rtype,rtype.Count,src);

 if (p.pDst=nil) then
 begin
  node:=OpConstruct(pLine,dst);
  For i:=0 to rtype.Count-1 do
  begin
   node.AddParam(src[i]);
  end;
  p.pDst:=dst; //save
  Result:=node;
 end else
 begin
  dst.pWriter:=p.pDst;
 end;
end;

function TEmitOp.OpMakeVec(pLine:TspirvOp;rtype:TsrDataType;src:PPsrRegNode):TsrRegNode;
Var
 p:TsrCacheOp;
 dst:TsrRegNode;
 node:TspirvOp;
 i:Byte;
begin
 Assert(src<>nil);

 pLine:=GetMaxPlace(pLine,rtype.Count,src);

 p:=CacheOpList.Fetch(pLine.Parent,srOpUtils.OpMakeVec,rtype,rtype.Count,src);

 if (p.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,srOpUtils.OpMakeVec); //need first

  dst:=NewReg(rtype);

  node.pDst:=dst;

  For i:=0 to rtype.Count-1 do
  begin
   node.AddParam(src[i]);
  end;

  p.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=p.pDst;
 end;
end;

//x,y,face,slice
function TEmitOp.OpMakeCub(pLine:TspirvOp;rtype:TsrDataType;src:PPsrRegNode):TsrRegNode;
Var
 p:TsrCacheOp;
 dst:TsrRegNode;
 node:TspirvOp;
 i:Byte;
begin
 Assert(src<>nil);

 pLine:=GetMaxPlace(pLine,rtype.Count,src);

 p:=CacheOpList.Fetch(pLine.Parent,srOpUtils.OpMakeCub,rtype,rtype.Count,src);

 if (p.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,srOpUtils.OpMakeCub); //need first

  dst:=NewReg(rtype);

  node.pDst:=dst;

  For i:=0 to rtype.Count-1 do
  begin
   node.AddParam(src[i]);
  end;

  p.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=p.pDst;
 end;
end;

function TEmitOp.OpSampledImage(pLine:TspirvOp;Tgrp,Sgrp:TsrNode;dtype:TsrDataType;info:TsrTypeImageInfo):TsrRegNode;
Var
 src:array[0..1] of TsrNode;
 pType:TsrType;
 p:TsrCacheOp;
 dst:TsrRegNode;
 node:TspirvOp;
 tmp:TspirvOp;
label
 _prev;
begin
 src[0]:=Tgrp;
 src[1]:=Sgrp;

 Assert(pLine<>nil);

 p:=CacheOpList.Fetch(pLine.Parent,Op.OpSampledImage,dtTypeSampledImage,2,@src);

 if (p.pDst<>nil) then //check before break block
 begin
  node:=pLine;
  repeat

   if (node.pDst=p.pDst) then
   begin
    Break; //end
   end;

   if node.IsType(ntOpBlock) then
   begin
    tmp:=nil;
    if (TsrOpBlock(node).Block.bType in [btCond,btLoop]) then
    begin
     p.pDst:=nil; //reset
     Break;
    end else
    begin
     tmp:=node.Last;
    end;
    if (tmp<>nil) then
    begin
     node:=tmp;
     Continue;
    end;
   end;

   _prev:

   tmp:=node.Prev;

   if (tmp=nil) then
   begin
    if (node.Parent=nil) or
       (node.Parent=pLine.Parent) then
    begin
     Break; //end
    end else
    begin
     node:=node.Parent;
     Goto _prev;
    end;
   end else
   begin
    node:=tmp;
   end;

  until false;
 end;

 if (p.pDst=nil) then
 begin
  node:=AddSpirvOp(pLine,Op.OpSampledImage); //need first

  dst:=NewReg(dtTypeSampledImage);

  pType:=TypeList.Fetch(dtype);
  pType:=TypeList.FetchImage(pType,info);
  pType:=TypeList.FetchSampledImage(pType);

  node.pType:=pType;
  node.pDst:=dst;

  node.AddParam(Tgrp);
  node.AddParam(Sgrp);

  p.pDst:=dst; //save
  Result:=dst;
 end else
 begin
  Result:=p.pDst;
 end;
end;

//

procedure TEmitOp.OpIAdd(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpIAdd,LazyIntType(src0.dtype,src1.dtype),dst,src0,src1);
end;

procedure TEmitOp.OpISub(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpISub,LazyIntType(src0.dtype,src1.dtype),dst,src0,src1);
end;

procedure TEmitOp.OpIMul(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpIMul,LazyIntType(src0.dtype,src1.dtype),dst,src0,src1);
end;

//

function TEmitOp.OpShlTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(LazyIntType(src0.dtype,src1.dtype));
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpShiftLeftLogical,Result,src0,src1));
end;

function TEmitOp.OpShlTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpShlTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpShrTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(LazyIntType(src0.dtype,src1.dtype));
 if (src0.dtype.Sign<>0) then
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpShiftRightArithmetic,Result,src0,src1));
 end else
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpShiftRightLogical   ,Result,src0,src1));
 end;
end;

function TEmitOp.OpShrTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpShrTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
end;

//

function TEmitOp.OpIAddTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(LazyIntType(src0.dtype,src1.dtype));

 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpIAdd,Result,src0,src1));
end;

function TEmitOp.OpIAddTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpIAddTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpISubTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(LazyIntType(src0.dtype,src1.dtype));

 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpISub,Result,src0,src1));
end;

function TEmitOp.OpISubTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpISubTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpIMulTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(LazyIntType(src0.dtype,src1.dtype));

 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpIMul,Result,src0,src1));
end;

function TEmitOp.OpIMulTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1<=1) then Exit(src0);

 if isPowerOfTwo(src1) then
 begin
  src1:=fastIntLog2(src1);
  Result:=OpShlTo (src0,src1,ppLine);
 end else
 begin
  Result:=OpIMulTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
 end;
end;

function TEmitOp.OpIDivTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0.dtype);
 if (src0.dtype.Sign<>0) then
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpSDiv,Result,src0,src1));
 end else
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpUDiv,Result,src0,src1));
 end;
end;

function TEmitOp.OpIDivTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1<=1) then Exit(src0);

 if isPowerOfTwo(src1) then
 begin
  src1:=fastIntLog2(src1);
  Result:=OpShrTo (src0,src1,ppLine);
 end else
 begin
  Result:=OpIDivTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
 end;
end;

function TEmitOp.OpIModTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0.dtype);
 if (src0.dtype.Sign<>0) then
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpSMod,Result,src0,src1));
 end else
 begin
  _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpUMod,Result,src0,src1));
 end;
end;

//

function TEmitOp.OpFAddTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFAdd,Result,src0,src1));
end;

function TEmitOp.OpFSubTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFSub,Result,src0,src1));
end;

function TEmitOp.OpFMulTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src1);
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFMul,Result,src0,src1));
end;

function TEmitOp.OpFDivTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src1=nil) then Exit(src0);

 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpFDiv,Result,src0,src1));
end;

//

function TEmitOp.OpFAddToS(src0:TsrRegNode;src1:Single;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1=0) then Exit(src0);
 Result:=OpFAddTo(src0,NewReg_s(src0.dtype,src1,ppLine),ppLine);
end;

function TEmitOp.OpFMulToS(src0:TsrRegNode;src1:Single;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) or (src1=0) or (src1=1) then Exit(src0);
 Result:=OpFMulTo(src0,NewReg_s(src0.dtype,src1,ppLine),ppLine);
end;

//

function TEmitOp.OpUToF(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpConvertUToF,Result,src);
end;

function TEmitOp.OpFToU(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpConvertFToU,Result,src);
end;

function TEmitOp.OpSToF(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpConvertSToF,Result,src);
end;

function TEmitOp.OpUToU(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpUConvert,Result,src);
end;

function TEmitOp.OpSToS(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpSConvert,Result,src);
end;

function TEmitOp.OpFToF(src:TsrRegNode;rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(rtype);
 _Op1(_get_line(ppLine),Op.OpFConvert,Result,src);
end;

//

function TEmitOp.OpFloorTo(src:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src=nil) then Exit(src);

 Result:=NewReg(src.dtype);
 _OpGlsl1(_get_line(ppLine),GlslOp.Floor,Result,src)
end;

function TEmitOp.OpPowTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src1=nil) then Exit(nil);

 Result:=NewReg(src0.dtype);
 _set_line(ppLine,_OpGlsl2(_get_line(ppLine),GlslOp.Pow,Result,src0,src1));
end;

//

procedure TEmitOp.OpNot(dst:PsrRegSlot;src:TsrRegNode);
begin
 Op1(Op.OpNot,dtUnknow,dst,src); //post type
end;

procedure TEmitOp.OpLogicalNot(dst:PsrRegSlot;src:TsrRegNode);
begin
 Op1(Op.OpLogicalNot,dtBool,dst,src);
end;

procedure TEmitOp.OpBitwiseOr(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpBitwiseOr,dtUnknow,dst,src0,src1); //post type
end;

procedure TEmitOp.OpBitwiseXor(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpBitwiseXor,dtUint32,dst,src0,src1); //post need?
end;

procedure TEmitOp.OpLogicalOr(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpLogicalOr,dtBool,dst,src0,src1);
end;

procedure TEmitOp.OpBitwiseAnd(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpBitwiseAnd,dtUnknow,dst,src0,src1); //post type
end;

procedure TEmitOp.OpLogicalAnd(dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(Op.OpLogicalAnd,dtBool,dst,src0,src1);
end;

//

function TEmitOp.OpNotTo(src:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(dtUnknow);
 _set_line(ppLine,_Op1(_get_line(ppLine),Op.OpNot,Result,src)); //post type
end;

function TEmitOp.OpOrTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(dtUnknow);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpBitwiseOr,Result,src0,src1)); //post type
end;

function TEmitOp.OpAndTo(src0,src1:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(dtUnknow);
 _set_line(ppLine,_Op2(_get_line(ppLine),Op.OpBitwiseAnd,Result,src0,src1)); //post type
end;

function TEmitOp.OpAndTo(src0:TsrRegNode;src1:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (src0=nil) then Exit(src0);
 Result:=OpAndTo(src0,NewReg_q(src0.dtype,src1,ppLine),ppLine);
end;

//

function TEmitOp.OpBitCountTo(src:TsrRegNode;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(src.dtype);
 _set_line(ppLine,_Op1(_get_line(ppLine),Op.OpBitCount,Result,src));
end;

//

function TEmitOp.OpImageSampleImplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(dst.dtype.isVector,'dst must be vector');

 node:=AddSpirvOp(pLine,Op.OpImageSampleImplicitLod); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(img);   //Sampled Image
 node.AddParam(coord); //Coordinate
                       //Image Operands

 Result:=node;
end;

function TEmitOp.OpImageSampleExplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(dst.dtype.isVector,'dst must be vector');

 node:=AddSpirvOp(pLine,Op.OpImageSampleExplicitLod); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(img);   //Sampled Image
 node.AddParam(coord); //Coordinate
                       //Image Operands
 Result:=node;
end;

function TEmitOp.OpImageSampleDrefImplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord,pcf:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(dst.dtype.isScalar,'dst must be scalar');

 node:=AddSpirvOp(pLine,Op.OpImageSampleDrefImplicitLod); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(img);   //Sampled Image
 node.AddParam(coord); //Coordinate
 node.AddParam(pcf);   //Dref
                       //Image Operands

 Result:=node;
end;

function TEmitOp.OpImageSampleDrefExplicitLod(pLine:TspirvOp;img:TsrNode;dst,coord,pcf:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(dst.dtype.isScalar,'dst must be scalar');

 node:=AddSpirvOp(pLine,Op.OpImageSampleDrefExplicitLod); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(img);   //Sampled Image
 node.AddParam(coord); //Coordinate
 node.AddParam(pcf);   //Dref
                       //Image Operands

 Result:=node;
end;

function TEmitOp.OpImageGather(pLine:TspirvOp;img:TsrNode;dst,coord:TsrRegNode;id:Byte):TspirvOp;
Var
 node:TspirvOp;
 comp:TsrRegNode;
begin
 Assert(dst.dtype.isVector,'dst must be vector');

 Case id of
  0..3:;
  else
   Assert(False,'unknow id');
 end;

 comp:=NewReg_i(dtUint32,id);

 node:=AddSpirvOp(pLine,Op.OpImageGather); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(img);   //Sampled Image
 node.AddParam(coord); //Coordinate
 node.AddParam(comp);  //Component
                       //Image Operands

 Result:=node;
end;

function TEmitOp.OpImageDrefGather(pLine:TspirvOp;img:TsrNode;dst,coord,pcf:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(dst.dtype.isVector,'dst must be vector');

 node:=AddSpirvOp(pLine,Op.OpImageDrefGather); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(img);   //Sampled Image
 node.AddParam(coord); //Coordinate
 node.AddParam(pcf);   //Dref
                       //Image Operands

 Result:=node;
end;

function TEmitOp.OpImageFetch(pLine:TspirvOp;Tgrp:TsrNode;dst,coord:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 Assert(dst.dtype.isVector,'dst must be vector');

 node:=AddSpirvOp(pLine,Op.OpImageFetch); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(Tgrp);
 node.AddParam(coord);

 Result:=node;
end;

function TEmitOp.OpImageRead(pLine:TspirvOp;Tgrp:TsrNode;dst,idx:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageRead); //need first

 node.pType:=TypeList.Fetch(dst.dtype);
 node.pDst:=dst;

 node.AddParam(Tgrp);

 if (idx<>nil) then
 begin
  node.AddParam(idx);
 end else
 begin
  node.AddParam(ConstList.Fetch_i(dtUint32,0));
 end;

 Result:=node;
end;

function TEmitOp.OpImageWrite(pLine:TspirvOp;Tgrp:TsrNode;idx,src:TsrRegNode):TspirvOp;
Var
 node:TspirvOp;
begin
 node:=AddSpirvOp(pLine,Op.OpImageWrite); //need first

 node.AddParam(Tgrp); //write

 if (idx<>nil) then
 begin
  node.AddParam(idx);
 end else
 begin
  node.AddParam(ConstList.Fetch_i(dtUint32,0));
 end;

 node.AddParam(src);

 Result:=node;
end;


end.

