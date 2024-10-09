unit emit_post_op;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  bittype,
  Half16,
  spirv,
  srCFGLabel,
  srNode,
  srType,
  srTypes,
  srRefId,
  srConst,
  srReg,
  srPrivate,
  srOp,
  srOpUtils,
  emit_fetch;

type
 TEmitPostOp=class(TEmitFetch)
  function  PostForward1(node:TSpirvOp):Integer;
  function  PostForward2(node:TSpirvOp):Integer;
  //
  function  OpConvert1(node:TSpirvOp):Integer;
  function  OnCompositeExtract1(node:TSpirvOp):Integer;
  function  OnFDiv1(node:TSpirvOp):Integer;
  function  OnIAdd1(node:TSpirvOp):Integer;
  function  OnISub1(node:TSpirvOp):Integer;
  function  OnShr1(node:TSpirvOp):Integer;
  function  _OnShr_ext1(node,pOp:TSpirvOp;pShrVal:TsrConst):Integer;
  function  _OnShr_ext_and(node:TSpirvOp;pShrVal,pAndVal:TsrConst):Integer;
  function  _OnShr_ext_add(node,pOp0,pOp1:TSpirvOp;pShrVal:TsrConst):Integer;
  function  OnAbsDiff1(node:TSpirvOp):Integer;
  function  OnWQM32__1(node:TSpirvOp):Integer;
  function  OnPackOfs1(node:TSpirvOp):Integer;
  function  _Fetch_PackAnc(node:TsrRegNode;index,count:Byte):TsrRegNode;
  function  OnBFE_32_1(node:TSpirvOp):Integer;
  function  OnBFIB32_1(node:TSpirvOp):Integer;
  function  OnMakeCub1(node:TSpirvOp):Integer;
  //
  function  OnBitwiseAnd1(node:TSpirvOp):Integer;
  function  OnLogicalAnd1(node:TSpirvOp):Integer;
  function  OnBitwiseOr1(node:TSpirvOp):Integer;
  function  OnLogicalOr1(node:TSpirvOp):Integer;
  function  OnNot1(node:TSpirvOp):Integer;
  function  OnLogicalNot1(node:TSpirvOp):Integer;
  function  OnBranchConditional1(node:TSpirvOp):Integer;
  //
  function  OpBitCount1(node:TSpirvOp):Integer;
  function  OpBitReverse1(node:TSpirvOp):Integer;
  //
  function  OnSelect1(node:TSpirvOp):Integer;
  //
  procedure MakeVecConst(rtype:TsrDataType;dst:TsrRegNode;src:PPsrRegNode);
  procedure MakeVecOne(dst:TsrRegNode;src:PPsrRegNode);
  function  MakeVecComp(pLine:TSpirvOp;rtype:TsrDataType;dst:TsrRegNode;src:PPsrRegNode):TSpirvOp;
  //
  function  OnMakeVec2(node:TSpirvOp):Integer;
  function  OnReturn_2(node:TSpirvOp):Integer;
  function  OnMakeExp2(node:TSpirvOp):Integer;
  function  OnIAddExt2(node:TSpirvOp):Integer;
  function  OnISubExt2(node:TSpirvOp):Integer;
  function  OnPackAnc2(node:TSpirvOp):Integer;
 end;

implementation

function TEmitPostOp.PostForward1(node:TSpirvOp):Integer;
begin
 Result:=0;

 Case node.OpId of

  Op.OpFConvert,
  Op.OpConvertFToU,
  Op.OpConvertFToS,
  Op.OpConvertSToF,
  Op.OpConvertUToF      :Result:=OpConvert1(node);

  Op.OpCompositeExtract :Result:=OnCompositeExtract1(node);

  Op.OpFDiv             :Result:=OnFDiv1(node);
  Op.OpIAdd             :Result:=OnIAdd1(node);
  Op.OpISub             :Result:=OnISub1(node);
  Op.OpShiftRightLogical,
  Op.OpShiftRightArithmetic:Result:=OnShr1(node);

  srOpUtils.OpAbsDiff   :Result:=OnAbsDiff1(node);
  srOpUtils.OpWQM32     :Result:=OnWQM32__1(node);
  srOpUtils.OpPackOfs   :Result:=OnPackOfs1(node);
  srOpUtils.OpBFE_32    :Result:=OnBFE_32_1(node);
  srOpUtils.OpBFIB32    :Result:=OnBFIB32_1(node);
  srOpUtils.OpMakeCub   :Result:=OnMakeCub1(node);

  Op.OpSelect           :Result:=OnSelect1(node);

  Op.OpBitwiseAnd       :Result:=OnBitwiseAnd1(node);
  Op.OpLogicalAnd       :Result:=OnLogicalAnd1(node);
  Op.OpBitwiseOr        :Result:=OnBitwiseOr1(node);
  Op.OpLogicalOr        :Result:=OnLogicalOr1(node);

  Op.OpNot              :Result:=OnNot1(node);
  Op.OpLogicalNot       :Result:=OnLogicalNot1(node);

  Op.OpBranchConditional:Result:=OnBranchConditional1(node);

  Op.OpBitCount         :Result:=OpBitCount1(node);
  Op.OpBitReverse       :Result:=OpBitReverse1(node);

  else;
 end;
end;

function TEmitPostOp.PostForward2(node:TSpirvOp):Integer;
begin
 Result:=0;

 Case node.OpId of

  srOpUtils.OpIAddExt:Result:=OnIAddExt2(node);
  srOpUtils.OpISubExt:Result:=OnISubExt2(node);
  srOpUtils.OpMakeVec:Result:=OnMakeVec2(node);

  srOpUtils.OpPackAnc:Result:=OnPackAnc2(node);

  Op.OpReturn:Result:=OnReturn_2(node);
  OpMakeExp  :Result:=OnMakeExp2(node);

  OpCUBEID:Assert(False,'TODO: CUBEID'); //TODO: CUBEID
  OpCUBESC:Assert(False,'TODO: CUBESC'); //TODO: CUBESC
  OpCUBETC:Assert(False,'TODO: CUBETC'); //TODO: CUBETC
  OpCUBEMA:Assert(False,'TODO: CUBEMA'); //TODO: CUBEMA

 end;

end;

function _classif_const(dtype:TsrDataType;value:QWORD):Integer;
begin
 if (value=0) then //always false
 begin
  Result:=0;
 end else
 if (value=dtype.High) then //always true
 begin
  Result:=1;
 end else
 begin
  Result:=-1;
 end;
end;

function _classif_const(p:TsrConst):Integer;
begin
 Result:=_classif_const(p.dtype,p.GetData);
end;

function BinType(t:TsrDataType):TsrDataType;
begin
 Case t of
  dtBool  :Result:=dtBool;
  dtInt32,
  dtUint32:Result:=dtUint32;
  else
           Result:=dtUnknow;
 end;
end;

function TEmitPostOp.OnBitwiseAnd1(node:TSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetOpType(OpId:DWORD;dtype:TsrDataType);
 begin
  Result:=0;
  if (node.OpId<>OpId) then
  begin
   node.OpId:=OpId;
   Inc(Result);
  end;
  //
  dst:=node.pDst.specialize AsType<ntReg>;
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
  //
  dtype:=dst.dtype;
  if (dtype=dtBool) then dst.dweak:=False;
  node.pType:=TypeList.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  dst:=node.pDst.specialize AsType<ntReg>;
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
  //
  dtype:=dst.dtype;
  //if (dtype=dtBool) then dst.dweak:=False;
  node.pType:=TypeList.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0].is_const and src[1].is_const then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  dtype:=LazyType3(dst.dtype,src[0].dtype,src[1].dtype);
  dtype:=LazyType2(dtype,dtUint32);

  _SetConst(dtype,data[0] and data[1]);
  Exit;
 end;

 if (dst.is_bool) or ((src[0].is_bool_or_const_bool) and (src[1].is_bool_or_const_bool)) then
 begin

  if (src[0].is_const) then
  begin
   Case src[0].AsConst.AsBool of
    True :_SetReg(src[1]);
    False:_SetConst(dtBool,0);
   end;
   Exit;
  end;
  if (src[1].is_const) then
  begin
   Case src[1].AsConst.AsBool of
    True :_SetReg(src[0]);
    False:_SetConst(dtBool,0);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalAnd,dtBool);
 end else
 begin
  dtype:=LazyType3(BinType(dst.dtype),BinType(src[0].dtype),BinType(src[1].dtype));
  dtype:=LazyType2(dtype,dtUint32);

  if (src[0].is_const) then
  begin
   case _classif_const(src[0].AsConst) of
    0:_SetConst(dtype,0); //always false
    1:_SetReg(src[1]);    //always true
   end;
  end;
  if (src[1].is_const) then
  begin
   case _classif_const(src[1].AsConst) of
    0:_SetConst(dtype,0); //always false
    1:_SetReg(src[0]);    //always true
   end;
  end;

  if (Result<>0) then Exit; //_SetConst/_SetReg
  _SetType(dtype);
 end;

 Result:=Result+PrepTypeParam(node.ParamNode(0),dst.dtype);
 Result:=Result+PrepTypeParam(node.ParamNode(1),dst.dtype);
end;

Function _FindNest_LAnd(node,src:TsrRegNode):Boolean;
var
 p:TSpirvOp;
 tmp:TsrRegNode;
begin
 Result:=False;
 if (node=nil) or (src=nil) then Exit;

 repeat

  p:=node.pWriter.specialize AsType<ntOp>;
  if (p<>nil) then
   if (p.OpId=Op.OpLogicalAnd) then
   begin
    tmp:=RegDown(p.ParamNode(0).AsReg);
    if (tmp=src) then Exit(True);
    Result:=_FindNest_LAnd(tmp,src); //recursion
    if Result then Exit(True);
    tmp:=RegDown(p.ParamNode(1).AsReg);
    if (tmp=src) then Exit(True);
    node:=tmp;
    Continue; //cycle
   end;
  Exit;

 until false;

end;

function TEmitPostOp.OnLogicalAnd1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 Assert(dst.dtype=dtBool);

 if src[0].is_const and src[1].is_const then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  _SetConst(dtBool,data[0] and data[1]);
  Exit;
 end;

 if (src[0].is_const) then
 begin
  Case src[0].AsConst.AsBool of
   True :_SetReg(src[1]);
   False:_SetConst(dtBool,0);
  end;
  Exit;
 end;
 if (src[1].is_const) then
 begin
  Case src[1].AsConst.AsBool of
   True :_SetReg(src[0]);
   False:_SetConst(dtBool,0);
  end;
  Exit;
 end;

 if _FindNest_LAnd(src[1],src[0]) then //Find src[0] in src[1]
 begin
  _SetReg(src[1]);
  Exit;
 end;

 if _FindNest_LAnd(src[0],src[1]) then //Find src[1] in src[0]
 begin
  _SetReg(src[0]);
  Exit;
 end;

 Result:=Result+PrepTypeParam(node.ParamNode(0),dtBool);
 Result:=Result+PrepTypeParam(node.ParamNode(1),dtBool);
end;

function _Fetch_BitwiseOr_Const(node:TsrRegNode):TsrConst;
var
 pLine:TSpirvOp;
 src:array[0..1] of TsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 pLine:=node.pWriter.specialize AsType<ntOp>;
 if (pLine=nil) then Exit;
 if (pLine.OpId<>Op.OpBitwiseOr) then Exit;

 src[0]:=RegDown(pLine.ParamNode(0).AsReg);
 src[1]:=RegDown(pLine.ParamNode(1).AsReg);

 if (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0].is_const and src[1].is_const then Exit;

 if src[0].is_const then
 begin
  Result:=src[0].AsConst;
 end else
 if src[1].is_const then
 begin
  Result:=src[1].AsConst;
 end;
end;

function _Fetch_BitwiseOr_Value(node:TsrRegNode):TsrRegNode;
var
 pLine:TSpirvOp;
 src:array[0..1] of TsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 pLine:=node.pWriter.specialize AsType<ntOp>;
 if (pLine=nil) then Exit;
 if (pLine.OpId<>Op.OpBitwiseOr) then Exit;

 src[0]:=RegDown(pLine.ParamNode(0).AsReg);
 src[1]:=RegDown(pLine.ParamNode(1).AsReg);

 if (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0].is_const and src[1].is_const then Exit;

 if src[0].is_const then
 begin
  Result:=src[1];
 end else
 if src[1].is_const then
 begin
  Result:=src[0];
 end;
end;

//

function TEmitPostOp.OnBitwiseOr1(node:TSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 pConst:TsrConst;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetOpType(OpId:DWORD;dtype:TsrDataType);
 begin
  Result:=0;
  if (node.OpId<>OpId) then
  begin
   node.OpId:=OpId;
   Inc(Result);
  end;
  //
  dst:=node.pDst.specialize AsType<ntReg>;
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
  //
  dtype:=dst.dtype;
  if (dtype=dtBool) then dst.dweak:=False;
  node.pType:=TypeList.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  dst:=node.pDst.specialize AsType<ntReg>;
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
  //
  dtype:=dst.dtype;
  //if (dtype=dtBool) then dst.dweak:=False;
  node.pType:=TypeList.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0].is_const and src[1].is_const then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  dtype:=LazyType3(dst.dtype,src[0].dtype,src[1].dtype);
  dtype:=LazyType2(dtype,dtUint32);

  _SetConst(dtype,data[0] or data[1]);
  Exit;
 end;

 if (dst.is_bool) or ((src[0].is_bool_or_const_bool) and (src[1].is_bool_or_const_bool)) then
 begin

  if (src[0].is_const) then
  begin
   Case src[0].AsConst.AsBool of
    True :_SetConst(dtBool,1);
    False:_SetReg(src[1]);
   end;
   Exit;
  end;
  if (src[1].is_const) then
  begin
   Case src[1].AsConst.AsBool of
    True :_SetConst(dtBool,1);
    False:_SetReg(src[0]);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalOr,dtBool);

  Result:=Result+PrepTypeParam(node.ParamNode(0),dst.dtype);
  Result:=Result+PrepTypeParam(node.ParamNode(1),dst.dtype);

  Exit;
 end;

 //
 pConst:=_Fetch_BitwiseOr_Const(src[0]);
 if (pConst<>nil) and src[1].is_const then
 begin
  //need a const calc
  data[0]:=        pConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  dtype:=LazyType3(dst.dtype,src[0].dtype,src[1].dtype);
  src[1]:=NewReg_q(dtype,data[0] or data[1],@Node);

  src[0]:=_Fetch_BitwiseOr_Value(src[0]);
  Assert(src[0]<>nil);

  node.ParamNode(0).Value:=src[0];
  node.ParamNode(1).Value:=src[1];
 end;

 //
 pConst:=_Fetch_BitwiseOr_Const(src[1]);
 if src[0].is_const and (pConst<>nil) then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=        pConst.GetData;

  dtype:=LazyType3(dst.dtype,src[0].dtype,src[1].dtype);
  src[0]:=NewReg_q(dtype,data[0] or data[1],@Node);

  src[1]:=_Fetch_BitwiseOr_Value(src[1]);
  Assert(src[1]<>nil);

  node.ParamNode(0).Value:=src[0];
  node.ParamNode(1).Value:=src[1];
 end;

 //else
 begin
  dtype:=LazyType3(BinType(dst.dtype),BinType(src[0].dtype),BinType(src[1].dtype));
  dtype:=LazyType2(dtype,dtUint32);

  if (src[0].is_const) then
  begin
   case _classif_const(src[0].AsConst) of
    0:_SetReg(src[1]);
    1:_SetConst(dtype,dtype.High); //is high
   end;
  end;
  if (src[1].is_const) then
  begin
   case _classif_const(src[1].AsConst) of
    0:_SetReg(src[0]);
    1:_SetConst(dtype,dtype.High); //is high
   end;
  end;

  if (Result<>0) then Exit; //_SetConst/_SetReg
  _SetType(dtype);
 end;

 Result:=Result+PrepTypeParam(node.ParamNode(0),dst.dtype);
 Result:=Result+PrepTypeParam(node.ParamNode(1),dst.dtype);
end;

function TEmitPostOp.OnLogicalOr1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 Assert(dst.dtype=dtBool);

 if src[0].is_const and src[1].is_const then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  _SetConst(dtBool,data[0] or data[1]);
  Exit;
 end;

 if (src[0].is_const) then
 begin
  Case src[0].AsConst.AsBool of
   True :_SetConst(dtBool,1);
   False:_SetReg(src[1]);
  end;
  Exit;
 end;
 if (src[1].is_const) then
 begin
  Case src[1].AsConst.AsBool of
   True :_SetConst(dtBool,1);
   False:_SetReg(src[0]);
  end;
  Exit;
 end;

 Result:=Result+PrepTypeParam(node.ParamNode(0),dtBool);
 Result:=Result+PrepTypeParam(node.ParamNode(1),dtBool);
end;

function TEmitPostOp.OnNot1(node:TSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
 src:TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetOpType(OpId:DWORD;dtype:TsrDataType);
 begin
  Result:=0;
  if (node.OpId<>OpId) then
  begin
   node.OpId:=OpId;
   Inc(Result);
  end;
  //
  dst:=node.pDst.specialize AsType<ntReg>;
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
  //
  dtype:=dst.dtype;
  if (dtype=dtBool) then dst.dweak:=False;
  node.pType:=TypeList.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  dst:=node.pDst.specialize AsType<ntReg>;
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
  //
  dtype:=dst.dtype;
  //if (dtype=dtBool) then dst.dweak:=False;
  node.pType:=TypeList.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (dst=nil) or (src=nil) then Exit;

 if src.is_const then
 begin
  dtype:=LazyType2(dst.dtype,src.dtype);
  dtype:=LazyType2(dtype,dtUint32);

  //need a const calc
  data[0]:=src.AsConst.GetData;
  data[1]:=dtype.High;

  _SetConst(dtype,(not data[0]) and data[1]);
  Exit;
 end;

 if (dst.is_bool) or (src.is_bool_or_const_bool) then
 begin

  if (src.is_const) then
  begin
   Case src.AsConst.AsBool of
    True :_SetConst(dtBool,0);
    False:_SetConst(dtBool,1);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalNot,dtBool);
 end else
 begin
  dtype:=LazyType2(BinType(dst.dtype),BinType(src.dtype));
  dtype:=LazyType2(dtype,dtUint32);
  _SetType(dtype);
 end;

 Result:=Result+PrepTypeParam(node.ParamNode(0),dst.dtype);
end;

function TEmitPostOp.OnLogicalNot1(node:TSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
 src:TsrRegNode;

 dst2:TsrRegNode;
 srp:array[0..1] of TsrRegNode;
 pop:TSpirvOp;
 cmp:DWORD;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (dst=nil) or (src=nil) then Exit;

 Assert(dst.dtype=dtBool);

 if (src.read_count>1) then Exit;

 pop:=src.pWriter.specialize AsType<ntOp>;

 if (pop=nil) then Exit;

 cmp:=pop.OpId;
 cmp:=get_inverse_not_cmp_op(cmp);

 if (cmp=0) then Exit;

 srp[0]:=pop.ParamNode(0).AsReg;
 srp[1]:=pop.ParamNode(1).AsReg;

 if (srp[0]=nil) or (srp[1]=nil) then Exit;

 dst2:=NewReg(dtBool);
 _Op2(pop,cmp,dst2,srp[0],srp[1]);

 _SetReg(dst2);
end;

Procedure mark_not_used_branch_op(pBlock:TsrOpBlock);
var
 node:TSpirvOp;
begin
 node:=pBlock.First;
 While (node<>nil) do
 begin
  if node.IsType(ntOp) then
  begin
   //
   Case node.OpId of
    Op.OpLabel,
    Op.OpSelectionMerge,
    Op.OpBranch,
    Op.OpBranchConditional:node.mark_not_used(True);
    else;
   end;
   //
  end;
  node:=node.Next;
 end;
end;

procedure _restore(var vctx:TsrVolatileContext);
var
 node:TsrVolatileNode;
 V:TsrVolatile;
 N:TsrRegNode;
begin
 node:=vctx.FList.pHead;
 while (node<>nil) do
 begin
  V:=TsrVolatile(node.V);
  N:=TsrRegNode(node.N);

  if (N.pWriter<>V) then
  begin
   Assert(false,'_restore:1');
  end;

  if (V.FBase=nil) then
  begin
   Assert(false,'_restore:2');
  end;

  //Preventing circular markings
  V.mark_read(nil);

  N.pWriter:=V.FBase;
  N.pWriter.PrepType(ord(N.dtype));

  V.mark_unread(nil);

  //
  node:=node.pNext;
 end;
end;

function TEmitPostOp.OnBranchConditional1(node:TSpirvOp):Integer;
var
 src,prv:TsrRegNode;
 pOpNot:TSpirvOp;
 pLabel:array[0..1] of TsrRefNode;
 pCond:TsrOpBlock;
 pMerg:TsrOpBlock;
 cst  :TsrConst;
begin
 Result:=0;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (src=nil) then Exit;

 if (src.is_const) then
 begin
  cst:=src.AsConst;
  if (cst<>nil) then
  begin
   pCond:=TsrOpBlock(node.Parent).FindUpCond;
   if (pCond<>nil) then
   if (pCond.pElse=nil) then //no else
   if (RegDown(pCond.Cond.pReg)=src) then
   if ((cst.AsBool=True)  and (pCond.Cond.FNormalOrder=True )) or   //if (true)
      ((cst.AsBool=False) and (pCond.Cond.FNormalOrder=False)) then //if (!false)
   begin
    //Remove the condition block

    //Clear ref in BranchConditional
    node.ParamNode(0).Value:=nil;

    //Get merge block
    pMerg:=pCond.Parent;
    Assert(pMerg.Block.bType=btMerg);

    //set type
    pMerg.Block.bType:=btOther;

    _restore(pCond.vctx);

    //simplification of connections
    //PrivateList.build_volatile_ctrue(pCond.pAfter,pCond.Regs.orig,pCond.Regs.prev,pCond.Regs.next);

    //set type
    pCond.Block.bType:=btOther;

    //clear instructions
    mark_not_used_branch_op(pMerg);
    mark_not_used_branch_op(pCond);

    Exit(1);
   end;
  end;
 end;

 pOpNot:=src.pWriter.specialize AsType<ntOp>;
 if (pOpNot=nil) then Exit;

 Case pOpNot.OpId of
  Op.OpLogicalNot:;
  else
   Exit;
 end;

 prv:=pOpNot.ParamNode(0).AsReg;
 if (prv=nil) then Exit;

 node.ParamNode(0).Value:=prv; //set new

 pLabel[0]:=node.ParamNode(1).Value.specialize AsType<ntRefId>; //read
 pLabel[1]:=node.ParamNode(2).Value.specialize AsType<ntRefId>; //read

 node.ParamNode(1).Value:=pLabel[1]; //swap
 node.ParamNode(2).Value:=pLabel[0]; //swap

 pCond:=TsrOpBlock(node.Parent).FindUpCond;
 if (pCond<>nil) then
 if (RegDown(pCond.Cond.pReg)=src) then
 begin
  //broken?
  //set new
  pCond.Cond.pReg:=prv;
  //invert mark
  pCond.Cond.FNormalOrder:=not pCond.Cond.FNormalOrder;
 end;

 Exit(1);
end;

function TEmitPostOp.OpBitCount1(node:TSpirvOp):Integer;
var
 dst,src:TsrRegNode;
 data:QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (dst=nil) or (src=nil) then Exit;

 if src.is_const then
 begin
  //need a const calc
  data:=src.AsConst.GetData;
  data:=PopCnt(data); //BitCount

  _SetConst(dst.dtype,data);
  Exit;
 end;

end;

Function ReverseBits(src:QWORD;count:Byte):QWORD;
var
 v:QWORD;
 i:Byte;
begin
 Result:=0;
 Assert(count<>0);
 dec(count);
 For i:=0 to count do
 begin
  v:=((src shr i) and 1); //get
  Result:=Result or (v shl (count-i)); //set
 end;
end;

function TEmitPostOp.OpBitReverse1(node:TSpirvOp):Integer;
var
 dst,src:TsrRegNode;
 data:QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (dst=nil) or (src=nil) then Exit;

 if src.is_const then
 begin
  //need a const calc
  data:=src.AsConst.GetData;

  data:=ReverseBits(data,src.dtype.BitSize);

  _SetConst(dst.dtype,data);
  Exit;
 end;

end;

function try_get_comp_bridge(var src:TsrRegNode):Integer; forward;

function TEmitPostOp.OpConvert1(node:TSpirvOp):Integer;
var
 i:Int64;
 dst,src,tmp:TsrRegNode;
 pc:TsrConst;
 pLine:TSpirvOp;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetConst_s(dtype:TsrDataType;value:Single);
 begin
  Assert(dtype=dtFloat32);
  dst.pWriter:=NewReg_s(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 function minz(i:Int64):QWORD;
 begin
  if (i>0) then Result:=i else Result:=0;
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (dst=nil) or (src=nil) then Exit;

 tmp:=src;
 While try_get_comp_bridge(tmp)<>0 do
 begin
  tmp:=RegDown(tmp);
 end;

 if (tmp<>src) then
 begin
  node.ParamNode(0).Value:=tmp;
  src:=tmp;
  Inc(Result);
 end;

 i:=0;
 if src.is_const then
 begin
  pc:=src.AsConst;
  Case node.OpId of

   Op.OpFConvert:
     case src.dtype of
      dtFloat32:
        case dst.dtype of
         dtHalf16:_SetConst(dst.dtype,WORD(THalf16(pc.AsFloat32)));
         else;
        end;
      dtHalf16:
        case dst.dtype of
         dtFloat32:_SetConst_s(dst.dtype,Single(pc.AsHalf16));
         else;
        end;
      else;
     end;

   Op.OpConvertFToU:
     case src.dtype of
      dtFloat32:
        if TryTruncInt64(pc.AsFloat32,i) then
        begin
         _SetConst(dst.dtype,minz(i));
        end;
      else;
     end;

   Op.OpConvertFToS:
     case src.dtype of
      dtFloat32:
       if TryTruncInt64(pc.AsFloat32,i) then
       begin
        _SetConst(dst.dtype,i);
       end;
      else;
     end;

   Op.OpConvertSToF:
     case src.dtype of
      dtInt32 :_SetConst_s(dst.dtype,pc.AsInt32);
      dtUint32:_SetConst_s(dst.dtype,pc.AsInt32);

      dtInt64 :_SetConst_s(dst.dtype,pc.AsInt64);
      dtUint64:_SetConst_s(dst.dtype,pc.AsInt64);
      else;
     end;

   Op.OpConvertUToF:
     case src.dtype of
      dtInt32 :_SetConst_s(dst.dtype,pc.AsUint32);
      dtUint32:_SetConst_s(dst.dtype,pc.AsUint32);

      dtInt64 :_SetConst_s(dst.dtype,pc.AsUint64);
      dtUint64:_SetConst_s(dst.dtype,pc.AsUint64);
      else;
     end;

  end;
 end else
 begin
  pLine:=src.pWriter.specialize AsType<ntOp>;
  if (pLine=nil) then Exit;

  Case pLine.OpId of
   Op.OpFConvert:
     begin
      tmp:=RegDown(pLine.ParamNode(0).AsReg);

      Case node.OpId of
       Op.OpFConvert:
         if (tmp.dtype=dst.dtype) then
         begin
          _SetReg(tmp);
         end;
       else;
      end;

     end;
   else;
  end;

 end;

end;

function TEmitPostOp.OnCompositeExtract1(node:TSpirvOp):Integer;
var
 pc:TsrConst;
 dst,org,src:TsrRegNode;
 pos:PtrUint;
begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 org:=node.ParamNode(0).AsReg;
 src:=RegDown(org);

 if (dst=nil) or (src=nil) then Exit;

 pos:=0;
 if not node.ParamNode(1).TryGetValue(pos) then Exit;

 if not src.is_const then Exit;

 pc:=src.AsConst;

 pc:=ConstList.Bitcast(org.dtype,pc);

 if (pos>=pc.Count) then Exit;

 pc:=pc.GetConst(pos);
 if (pc=nil) then Exit;

 dst.pWriter:=pc;

 node.mark_not_used;
 node.pDst:=nil;
 Inc(Result);
end;

{
function TEmitPostOp.OnSAbs(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:TsrRegNode;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;

 dst:=node^.dst.pData;
 src:=node^.Param(2).pData;

 if src^.is_const then
 begin
  Case dst^.dtype of
   dtInt32,
   dtUint32:_SetConst(dtUint32,Abs(src^.AsConst^.AsInt));

   dtInt64,
   dtUint64:_SetConst(dtUint64,Abs(src^.AsConst^.AsInt64));

   else;
  end;
 end;

end;
}

function TEmitPostOp.OnSelect1(node:TSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;

 cst:array[0..1] of TsrConst;

 pLine:TSpirvOp;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=node.ParamNode(1).AsReg;
 src[1]:=node.ParamNode(2).AsReg;

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 cst[0]:=RegDown(src[0]).AsConst;
 cst[1]:=RegDown(src[1]).AsConst;
 if (cst[0]<>nil) and (cst[1]<>nil) then
 begin

  if (cst[0].GetData=cst[1].GetData) then
  begin
   _SetConst(dst.dtype,cst[0].GetData);
   Exit;
  end;

  if (dst.dtype=dtBool) and
     (cst[0].dtype=dtBool) and
     (cst[1].dtype=dtBool) then
  begin
   if (cst[0].AsBool=True) and (cst[1].AsBool=False) then
   begin
    src[0]:=node.ParamNode(0).AsReg;
    _SetReg(src[0]);
    Exit;
   end else
   if (cst[0].AsBool=False) and (cst[1].AsBool=True) then
   begin
    src[0]:=node.ParamNode(0).AsReg;
    pLine:=src[0].pLine;
    src[1]:=OpNotTo(src[0],@pLine);
    src[1].PrepType(ord(dtBool));
    _SetReg(src[1]);
    Exit;
   end;
  end;


 end;

 dtype:=LazyType3(dst.dtype,src[0].dtype,src[1].dtype);

 if (dtype<>dtUnknow) and (node.pType.dtype<>dtype) then
 begin
  node.pType:=TypeList.Fetch(dtype);
  if (node.pType.dtype=dtype) then
  begin
   Inc(Result);
  end;
 end;

 if (dtype<>dtUnknow) and (dst.dtype<>dtype) then
 begin
  Result:=Result+PrepTypeDst(dst,dtype);
  node.pDst:=dst;
 end;

 Result:=Result+PrepTypeParam(node.ParamNode(1),dst.dtype);
 Result:=Result+PrepTypeParam(node.ParamNode(2),dst.dtype);
end;

function TEmitPostOp.OnFDiv1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 pCon:array[0..1] of TsrConst;

 procedure _SetConst_s(dtype:TsrDataType;value:Single);
 begin
  Assert(dtype=dtFloat32);
  dst.pWriter:=NewReg_s(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (src[0].is_const) and (src[1].is_const) then
 if (src[0].dtype=src[1].dtype) then
 begin
  //need a const calc

  pCon[0]:=src[0].AsConst;
  pCon[1]:=src[1].AsConst;

  Case src[0].dtype of
   dtFloat32:
     begin
      _SetConst_s(dst.dtype,pCon[0].AsFloat32/pCon[1].AsFloat32);
     end;
   else;
  end;

 end;
end;

function TEmitPostOp.OnIAdd1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (src[0].is_const) and (src[1].is_const) then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  _SetConst(dst.dtype,data[0]+data[1]);
 end else
 if (src[0].is_const) then
 begin
  if (src[0].AsConst.GetData=0) then
  begin
   _SetReg(src[1]);
  end;
 end else
 if (src[1].is_const) then
 begin
  if (src[1].AsConst.GetData=0) then
  begin
   _SetReg(src[0]);
  end;
 end;
end;

function TEmitPostOp.OnISub1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0].is_const and src[1].is_const then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  _SetConst(dst.dtype,data[0]-data[1]);
 end;
end;

function TEmitPostOp.OnShr1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (src[0].is_const) and (src[1].is_const) then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  _SetConst(dst.dtype,data[0] shr data[1]);
 end else
 if (src[1].is_const) then
 begin
  Result:=_OnShr_ext1(node,src[0].pWriter.specialize AsType<ntOp>,src[1].AsConst);
 end;
end;

function TEmitPostOp._OnShr_ext1(node,pOp:TSpirvOp;pShrVal:TsrConst):Integer;
var
 src:array[0..1] of TsrRegNode;

begin
 Result:=0;
 if (pOp=nil) then Exit;

 Case pOp.OpId of
  Op.OpBitwiseAnd:
    begin
     src[0]:=RegDown(pOp.ParamNode(0).AsReg);
     src[1]:=RegDown(pOp.ParamNode(1).AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[0].is_const) then
     begin
      Result:=_OnShr_ext_and(node,pShrVal,src[0].AsConst);
     end else
     if (src[1].is_const) then
     begin
      Result:=_OnShr_ext_and(node,pShrVal,src[1].AsConst);
     end;
    end;

  Op.OpIAdd:
    begin
     src[0]:=RegDown(pOp.ParamNode(0).AsReg);
     src[1]:=RegDown(pOp.ParamNode(1).AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     Result:=_OnShr_ext_add(node,src[0].pWriter.specialize AsType<ntOp>,src[1].pWriter.specialize AsType<ntOp>,pShrVal);
    end;

  else;
 end;

end;

function TEmitPostOp._OnShr_ext_and(node:TSpirvOp;pShrVal,pAndVal:TsrConst):Integer;
var
 dst:TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 data[0]:=pShrVal.GetData;
 data[1]:=pAndVal.GetData;

 data[0]:=High(QWORD) shl data[0];

 if (data[0] and data[1]=0) then
 begin
  dst:=node.pDst.specialize AsType<ntReg>;
  _SetConst(dst.dtype,0);
 end;
end;

function isPowerOfTwo(x:QWORD):Boolean; inline;
begin
 Result:=((x-1) and x)=0;
end;

function fastIntLog2(i:QWORD):QWORD; inline;
begin
 Result:=BsfQWord(i);
end;

function _OpCanBeShrOpt(pOp:TSpirvOp;ShrVal:QWORD;var Delta:QWORD):Boolean;
var
 src:array[0..1] of TsrRegNode;
 data:QWORD;
begin
 Result:=False;
 if (pOp=nil) then Exit;

 Case pOp.OpId of
  Op.OpIMul:
    begin
     src[0]:=RegDown(pOp.ParamNode(0).AsReg);
     src[1]:=RegDown(pOp.ParamNode(1).AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[0].is_const) then
     begin
      data:=src[0].AsConst.GetData;
     end else
     if (src[1].is_const) then
     begin
      data:=src[1].AsConst.GetData;
     end else
     begin
      Exit;
     end;

     if isPowerOfTwo(data) then
     begin
      data:=fastIntLog2(data);
      Result:=(data>=ShrVal);
      Delta:=data-ShrVal;
     end;

    end;

  Op.OpShiftLeftLogical:
    begin
     src[0]:=RegDown(pOp.ParamNode(0).AsReg);
     src[1]:=RegDown(pOp.ParamNode(1).AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[1].is_const) then
     begin
      data:=src[1].AsConst.GetData;

      Result:=(data>=ShrVal);
      Delta:=data-ShrVal;
     end;

    end;

  else;
 end;
end;

function _GetShrOptReg(pOp:TSpirvOp):TsrRegNode;
var
 src:array[0..1] of TsrRegNode;
begin
 Result:=nil;
 if (pOp=nil) then Exit;

 Case pOp.OpId of
  Op.OpIMul:
    begin
     src[0]:=RegDown(pOp.ParamNode(0).AsReg);
     src[1]:=RegDown(pOp.ParamNode(1).AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[0].is_const) then
     begin
      Result:=src[1];
     end else
     if (src[1].is_const) then
     begin
      Result:=src[0];
     end;
    end;

  Op.OpShiftLeftLogical:
    begin
     src[0]:=RegDown(pOp.ParamNode(0).AsReg);
     src[1]:=RegDown(pOp.ParamNode(1).AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     Result:=src[0];
    end;

  else;
 end;
end;

function TEmitPostOp._OnShr_ext_add(node,pOp0,pOp1:TSpirvOp;pShrVal:TsrConst):Integer;
var
 dst,src:TsrRegNode;
 dst_shr:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 data[0]:=0;
 data[1]:=0;

 if (not _OpCanBeShrOpt(pOp0,pShrVal.GetData,data[0])) and
    (not _OpCanBeShrOpt(pOp1,pShrVal.GetData,data[1])) then Exit;

 if (data[0]=0) then
 begin
  dst_shr[0]:=_GetShrOptReg(pOp0);
 end else
 begin
  dst:=pOp0.pDst.specialize AsType<ntReg>;

  src:=_GetShrOptReg(pOp0);

  PrepTypeNode(src,dtUInt32);

  dst_shr[0]:=OpShlTo(src,data[0],@pOp0)
 end;

 if (data[1]=0) then
 begin
  dst_shr[1]:=_GetShrOptReg(pOp1);
 end else
 begin
  dst:=pOp1.pDst.specialize AsType<ntReg>;

  src:=_GetShrOptReg(pOp1);

  PrepTypeNode(src,dtUInt32);

  dst_shr[1]:=OpShlTo(src,data[1],@pOp0)
 end;

 dst:=node.pDst.specialize AsType<ntReg>;
 pOp0:=dst.pWriter.specialize AsType<ntOp>; //OpIAdd

 PrepTypeNode(dst_shr[0],dtUInt32);
 PrepTypeNode(dst_shr[1],dtUInt32);

 src:=OpIAddTo(dst_shr[0],dst_shr[1],@pOp0);
 _SetReg(src);

 //Writeln(data[0],' ',data[1]);
 //writeln;
 Result:=1;
end;

function TEmitPostOp.OnAbsDiff1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 data:array[0..1] of QWORD;
 rmax,rmin:TsrRegNode;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src[0]:=RegDown(node.ParamNode(0).AsReg);
 src[1]:=RegDown(node.ParamNode(1).AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0].is_const and src[1].is_const then
 begin
  //need a const calc
  data[0]:=src[0].AsConst.GetData;
  data[1]:=src[1].AsConst.GetData;

  if (data[0]>data[1]) then
   _SetConst(dst.dtype,data[0]-data[1])
  else
   _SetConst(dst.dtype,data[1]-data[0]);
  Exit;
 end else
 if src[0].is_const then
 begin
  if src[0].AsConst.isZeroVal then
  begin
   //src[1]-0
   src[1]:=node.ParamNode(1).AsReg; //get original
   _SetReg(src[1]);
   Exit;
  end;
 end else
 if src[1].is_const then
 begin
  if src[1].AsConst.isZeroVal then
  begin
   //src[0]-0
   src[0]:=node.ParamNode(0).AsReg; //get original
   _SetReg(src[0]);
   Exit;
  end;
 end;

 //else

 node.mark_not_used;
 node.pDst:=nil;

 rmax:=OpUMaxTo(src[0],src[1],@node); //update line
 rmin:=OpUMinTo(src[0],src[1],@node); //update line

 _Op2(node,Op.OpISub,dst,rmax,rmin);
end;

function F_WQM_32(D:DWORD):DWORD;
var
 i:Byte;
begin
 Result:=0;
 if (D=0) then Exit;
 For i:=0 to 7 do
 begin
  if (((D shr (i*4)) and 15)<>0) then
  begin
   Result:=Result or ($F shl (i*4));
  end;
 end;
end;

function TEmitPostOp.OnWQM32__1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:TsrRegNode;
 data:QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst.pWriter:=NewReg_q(dtype,value,@node);
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:TsrRegNode);
 begin
  dst.pWriter:=src;
  node.mark_not_used;
  node.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 src:=RegDown(node.ParamNode(0).AsReg);

 if (dst=nil) or (src=nil) then Exit;

 if src.is_const then
 begin
  //need a const calc
  data:=src.AsConst.GetData;
  data:=F_WQM_32(data);
  _SetConst(dst.dtype,data);
 end else
 if (src.dtype=dtBool) then
 begin
  _SetReg(src);
 end else
 begin
  //TODO: WQM32
  _SetReg(src);
 end;
end;

type
 Ppacked_offset=^Tpacked_offset;
 Tpacked_offset=bitpacked record
   x:bit6; //0..5   (int6)
  a1:bit2;
   y:bit6; //8..13  (int6)
  a2:bit2;
   z:bit6; //16..21 (int6)
  a3:bit10;
 end;

function int6(b:Byte):Integer; inline;
const
 shift=BitSizeOf(Integer)-6;
begin
 Result:=SarLongint((Integer(b) shl shift),shift);
end;

function TEmitPostOp.OnPackOfs1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:TsrRegNode;
 data:QWORD;
 P:Ppacked_offset;

 count:PtrUint;

 ret:TsrConst;
 vec:array[0..2] of TsrConst;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 count:=0;
 if not node.ParamNode(0).TryGetValue(count) then Exit;

 src:=RegDown(node.ParamNode(1).AsReg);

 if (src=nil) then Exit;

 if src.is_const then
 begin
  //need a const calc
  data:=src.AsConst.GetData;
  P:=@data;

  ret:=nil;
  Case count of
   1:
     begin
      ret:=ConstList.Fetch_i(dtInt32,int6(P^.x));
     end;
   2:
     begin
      vec[0]:=ConstList.Fetch_i(dtInt32,int6(P^.x));
      vec[1]:=ConstList.Fetch_i(dtInt32,int6(P^.y));

      ret:=ConstList.FetchVector(dtVec2i,@vec,true);
     end;
   3:
     begin
      vec[0]:=ConstList.Fetch_i(dtInt32,int6(P^.x));
      vec[1]:=ConstList.Fetch_i(dtInt32,int6(P^.y));
      vec[2]:=ConstList.Fetch_i(dtInt32,int6(P^.z));

      ret:=ConstList.FetchVector(dtVec3i,@vec,true);
     end;
   else
    Assert(False);
  end;

  dst.pWriter:=ret;

  node.mark_not_used;
  node.pDst:=nil;

  Inc(Result);
 end else
 begin
  //TODO: non constant PackOfs
  Writeln('TODO: non constant PackOfs ',src.pWriter.ntype.ClassName);
  Assert(false,'TODO: non constant PackOfs');
 end;
end;

////////

function TEmitPostOp._Fetch_PackAnc(node:TsrRegNode;index,count:Byte):TsrRegNode;
var
 pLine:TSpirvOp;
 src:array[0..2] of TsrRegNode;
 prim,smid,rtid:Boolean;
begin
 Result:=nil;

 if (node=nil) then Exit;
 if (count=0) then Exit;

 pLine:=node.pWriter.specialize AsType<ntOp>;
 if (pLine=nil) then Exit;
 if (pLine.OpId<>srOpUtils.OpPackAnc) then Exit;

 src[0]:=pLine.ParamNode(0).AsReg;
 src[1]:=pLine.ParamNode(1).AsReg;
 src[2]:=pLine.ParamNode(2).AsReg;

 if (src[0]=nil) or (src[1]=nil) or (src[2]=nil) then Exit;

 prim:=                (index+count<= 2); //[ 1: 0]
 smid:=(index>= 8) and (index+count<=12); //[11: 8]
 rtid:=(index>=16) and (index+count<=27); //[26:16]

 count:=ord(prim)+ord(smid)+ord(rtid);

 if (count=0) then
 begin
  Result:=NewReg_q(node.dtype,0);
 end else
 if (count=1) then
 begin
  if prim then
  begin
   Result:=src[0];
  end else
  if smid then
  begin
   Result:=src[1];
  end else
  if rtid then
  begin
   Result:=src[2];
  end;
 end;

end;

function TEmitPostOp.OnBFE_32_1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 rBase,rIndex,rCount:TsrRegNode;
 rsl:TsrRegNode;
 num_31:TsrRegNode;
 data:array[0..1] of QWORD;
 index,count:Byte;
 dtype:TsrDataType;
begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 rBase :=node.ParamNode(0).AsReg;
 rIndex:=RegDown(node.ParamNode(1).AsReg);
 rCount:=RegDown(node.ParamNode(2).AsReg);

 if (rBase=nil) or (rIndex=nil) or (rCount=nil) then Exit;

 dtype:=rBase.dtype;

 //else
 node.mark_not_used;
 node.pDst:=nil;

 if (rIndex.is_const) and (rCount.is_const) then
 begin
  data[0]:=rIndex.AsConst.GetData;
  data[1]:=rCount.AsConst.GetData;
  //
  index:=Byte(data[0] and 31);
  count:=Byte(data[1] and 31);
  //
  rsl:=_Fetch_PackAnc(rBase,index,count);

  if (rsl<>nil) then
  begin

   rBase:=RegDown(rsl);
   if (rBase.is_const) then
   begin
    data[0]:=rBase.AsConst.GetData;
    data[1]:=(1 shl count)-1;
    data[0]:=data[0] and data[1];
    //
    if (data[0]<>rBase.AsConst.GetData) then
    begin
     rsl:=NewReg_q(dtUInt32,data[0],@Node);
    end;
   end else
   begin
    data[1]:=(1 shl count)-1;
    num_31:=NewReg_q(dtUInt32,data[1],@Node);
    //

    PrepTypeNode(rsl,dtype);

    rsl:=OpAndTo(rsl,num_31,@node);
    rsl.PrepType(ord(dtype));
   end;

   dst.pWriter:=rsl;
   dst.pLine  :=rsl.pLine;

   Exit;
  end;

 end;

 num_31:=nil;

 //
 if (rIndex.is_const) then
 begin
  data[0]:=rIndex.AsConst.GetData;
  data[0]:=data[0] and 31;
  //
  if (data[0]<>rIndex.AsConst.GetData) then
  begin
   rIndex:=NewReg_q(dtUInt32,data[0],@Node);
  end else
  begin
   rIndex:=node.ParamNode(1).AsReg; //orig
  end;
 end else
 begin
  num_31:=NewReg_q(dtUInt32,31,@Node);
  //
  rIndex:=node.ParamNode(1).AsReg; //orig

  PrepTypeNode(rIndex,dtUInt32);

  rIndex:=OpAndTo(rIndex,num_31,@node);
 end;

 //
 if (rCount.is_const) then
 begin
  data[1]:=rCount.AsConst.GetData;
  data[1]:=data[1] and 31;
  //
  if (data[1]<>rCount.AsConst.GetData) then
  begin
   rCount:=NewReg_q(dtUInt32,data[1],@Node);
  end else
  begin
   rCount:=node.ParamNode(2).AsReg; //orig
  end;
 end else
 begin
  if (num_31<>nil) then
  begin
   num_31:=NewReg_q(dtUInt32,31,@Node);
  end;
  //
  rCount:=node.ParamNode(2).AsReg; //orig

  PrepTypeNode(rCount,dtUInt32);

  rCount:=OpAndTo(rCount,num_31,@node);
 end;

 PrepTypeNode(rIndex,dtUInt32);
 PrepTypeNode(rCount,dtUInt32);

 case dtype of
  dtUint32:_Op3(node,Op.OpBitFieldUExtract,dst,rBase,rIndex,rCount);
  dtInt32 :_Op3(node,Op.OpBitFieldSExtract,dst,rBase,rIndex,rCount);
  else
   Assert(False);
 end;

end;

function TEmitPostOp.OnBFIB32_1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 bitmsk:TsrRegNode;
 src:array[0..1] of TsrRegNode;
 rIndex,rCount:TsrRegNode;
 data:array[0..1] of QWORD;
 index,count:DWORD;
begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 bitmsk:=RegDown(node.ParamNode(0).AsReg);
 src[0]:=RegDown(node.ParamNode(1).AsReg);
 src[1]:=RegDown(node.ParamNode(2).AsReg);

 if (bitmsk=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if bitmsk.is_const then
 begin
  data[0]:=bitmsk.AsConst.GetData;

  index:=BsfQWord(data[0]);
  count:=PopCnt  (data[0]);

  data[1]:=((1 shl count)-1) shl index;

  if (data[0]=data[1]) then
  begin
   node.mark_not_used;
   node.pDst:=nil;

   rIndex:=NewReg_q(dtUint32,index,@Node);
   rCount:=NewReg_q(dtUint32,count,@Node);

   src[0]:=node.ParamNode(1).AsReg;
   src[1]:=node.ParamNode(2).AsReg;

   _Op4(node,Op.OpBitFieldInsert,dst,src[1],src[0],rIndex,rCount);

   Exit;
  end;

 end;

 //else
 node.mark_not_used;
 node.pDst:=nil;

 src[0]:=node.ParamNode(1).AsReg;
 src[1]:=node.ParamNode(2).AsReg;

 src[0]:=OpAndTo(src[0],bitmsk,@node);
 src[0].PrepType(ord(dtUInt32));

 bitmsk:=OpNotTo(bitmsk,@node);
 bitmsk.PrepType(ord(dtUInt32));

 src[1]:=OpAndTo(src[1],bitmsk,@node);
 src[1].PrepType(ord(dtUInt32));

 _Op2(node,Op.OpBitwiseOr,dst,src[0],src[1]);
end;

function _IsFma(node:TSpirvOp):Boolean;
var
 OpId:PtrUint;
begin
 Result:=False;
 if (node=nil) then Exit;
 if (node.OpId<>Op.OpExtInst) then Exit;
 OpId:=0;
 node.ParamNode(1).TryGetValue(OpId);
 if (OpId<>GlslOp.Fma) then Exit;
 Result:=True;
end;

function _IsOp(node:TSpirvOp;OpId:DWORD):Boolean;
begin
 Result:=False;
 if (node=nil) then Exit;
 Result:=(node.OpId=OpId);
end;

function _IsConstFloat_1_0(pReg:TsrRegNode):Boolean;
var
 pConst:TsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pConst:=pReg.AsConst;
 if (pConst=nil) then Exit;
 if (pConst.dtype<>dtFloat32) then Exit;
 Result:=(Round(pConst.AsFloat32*10)=10);
end;

function _IsConstFloat_1_5(pReg:TsrRegNode):Boolean;
var
 pConst:TsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pConst:=pReg.AsConst;
 if (pConst=nil) then Exit;
 if (pConst.dtype<>dtFloat32) then Exit;
 Result:=(Round(pConst.AsFloat32*10)=15);
end;

function _Fetch_FAbs_Value(node:TSpirvOp):TSpirvOp;
var
 OpId:PtrUint;
 pReg:TsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 if (node.OpId<>Op.OpExtInst) then Exit;
 OpId:=0;
 node.ParamNode(1).TryGetValue(OpId);
 if (OpId<>GlslOp.FAbs) then Exit;
 pReg:=RegDown(node.ParamNode(2).AsReg);
 if (pReg=nil) then Exit;
 Result:=pReg.pWriter.specialize AsType<ntOp>;
end;

function _cmp_src_cube_op3(node0,node1:TSpirvOp):Boolean;
var
 src0:TsrRegNode;
 src1:TsrRegNode;
 i:Byte;
begin
 Result:=False;
 if (node0=nil) or (node1=nil) then Exit;
 For i:=0 to 2 do
 begin
  src0:=RegDown(node0.ParamNode(i).AsReg);
  src1:=RegDown(node1.ParamNode(i).AsReg);
  if (src0<>src1) then Exit(False);
 end;
 Result:=True;
end;

function is_all_const(src:PPsrRegNode;count:byte):Boolean;
var
 i:Byte;
begin
 Result:=True;
 For i:=0 to count-1 do
  if not src[i].is_const then Exit(false);
end;

function is_all_in_one_comp(src:PPsrRegNode;rtype:TsrDataType;count:byte):Boolean;
var
 i:Byte;
 pos:PtrUint;
 pLine:TSpirvOp;
 pReg,tmp:TsrRegNode;
begin
 pReg:=nil;
 Result:=True;
 For i:=0 to count-1 do
 begin
  pLine:=src[i].pWriter.specialize AsType<ntOp>;
  if (pLine=nil) then Exit(false);
  if (pLine.OpId<>Op.OpCompositeExtract) then Exit(false);

  pos:=0;
  if not pLine.ParamNode(1).TryGetValue(pos) then Exit;
  if (pos<>i) then Exit(false);

  tmp:=RegDown(pLine.ParamNode(0).AsReg);
  if (tmp=nil) then Exit(false);
  if (tmp.dtype<>rtype) then Exit(false);

  if (i=0) then
  begin
   pReg:=tmp;
  end else
  begin
   if (pReg<>tmp) then Exit(false);
  end;

 end;
end;

function try_get_comp_bridge(var src:TsrRegNode):Integer;
var
 pLine:TSpirvOp;
 pos:PtrUint;
 pReg:TsrRegNode;
begin
 Result:=0;
 pLine:=src.pWriter.specialize AsType<ntOp>;
 if (pLine=nil) then Exit;
 if (pLine.OpId<>Op.OpCompositeExtract) then Exit;

 pos:=0;
 if not pLine.ParamNode(1).TryGetValue(pos) then Exit;

 pReg:=RegDown(pLine.ParamNode(0).AsReg);
 if (pReg=nil) then Exit;

 pLine:=pReg.pWriter.specialize AsType<ntOp>;
 if (pLine=nil) then Exit;
 if (pLine.OpId<>Op.OpCompositeConstruct) then Exit;

 pReg:=RegDown(pLine.ParamNode(pos).AsReg);
 if (pReg=nil) then Exit;
 src:=pReg;
 Result:=1;
end;

function TEmitPostOp.OnMakeCub1(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..2] of TsrRegNode;

 m_CUBE_SC:TSpirvOp;
 m_CUBE_TC:TSpirvOp;
 m_CUBE_ID:TSpirvOp;

 m_x_CUBE_MA:TSpirvOp;
 m_y_CUBE_MA:TSpirvOp;

 m_x,m_y,m_f:TSpirvOp;

 pReg:TsrRegNode;
 pOp:TSpirvOp;

 rtype:TsrDataType;
 i:Byte;

begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 rtype:=dst.dtype;

 if (rtype.Child<>dtFloat32) then Assert(false,'TODO');
 if (rtype.Count=4) then Assert(false,'TODO');

 m_x:=RegDown(node.ParamNode(0).AsReg).pWriter.specialize AsType<ntOp>; //param1
 m_y:=RegDown(node.ParamNode(1).AsReg).pWriter.specialize AsType<ntOp>; //param2
 m_f:=RegDown(node.ParamNode(2).AsReg).pWriter.specialize AsType<ntOp>; //param3

 if not _IsFma(m_x) then Exit;
 if not _IsFma(m_y) then Exit;
 if not _IsOp(m_f,OpCUBEID) then Exit;

 m_CUBE_ID:=m_f;

 //m_x
 pReg:=RegDown(m_x.ParamNode(2).AsReg); //param1
 pOp:=pReg.pWriter.specialize AsType<ntOp>;
 if not _IsOp(pOp,OpCUBESC) then Exit;
 m_CUBE_SC:=pOp;

 pReg:=RegDown(m_x.ParamNode(3).AsReg); //param2
 pOp:=pReg.pWriter.specialize AsType<ntOp>;
 if not _IsOp(pOp,Op.OpFDiv) then Exit;

   pReg:=RegDown(pOp.ParamNode(0).AsReg);  //div
   if not _IsConstFloat_1_0(pReg) then Exit; //1.0

   pReg:=RegDown(pOp.ParamNode(1).AsReg);
   pOp:=_Fetch_FAbs_Value(pReg.pWriter.specialize AsType<ntOp>);
   if not _IsOp(pOp,OpCUBEMA) then Exit;
   m_x_CUBE_MA:=pOp;

 pReg:=RegDown(m_x.ParamNode(4).AsReg);  //param3
 if not _IsConstFloat_1_5(pReg) then Exit; //1.5

 //m_y
 pReg:=RegDown(m_y.ParamNode(2).AsReg); //param1
 pOp:=pReg.pWriter.specialize AsType<ntOp>;
 if not _IsOp(pOp,OpCUBETC) then Exit;
 m_CUBE_TC:=pOp;

 pReg:=RegDown(m_x.ParamNode(3).AsReg); //param2
 pOp:=pReg.pWriter.specialize AsType<ntOp>;
 if not _IsOp(pOp,Op.OpFDiv) then Exit;

   pReg:=RegDown(pOp.ParamNode(0).AsReg);  //div
   if not _IsConstFloat_1_0(pReg) then Exit; //1.0

   pReg:=RegDown(pOp.ParamNode(1).AsReg);
   pOp:=_Fetch_FAbs_Value(pReg.pWriter.specialize AsType<ntOp>);
   if not _IsOp(pOp,OpCUBEMA) then Exit;
   m_y_CUBE_MA:=pOp;

 pReg:=RegDown(m_y.ParamNode(4).AsReg);  //param3
 if not _IsConstFloat_1_5(pReg) then Exit; //1.5

 //

 if not _cmp_src_cube_op3(m_CUBE_SC,m_CUBE_TC  ) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_CUBE_ID  ) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_x_CUBE_MA) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_y_CUBE_MA) then Exit;

 For i:=0 to 2 do
 begin
  src[i]:=RegDown(m_CUBE_SC.ParamNode(i).AsReg);
 end;

 MakeVecComp(node,dtVec3f,dst,@src);

 node.mark_not_used;
 node.pDst:=nil;
 Result:=1;
end;

procedure TEmitPostOp.MakeVecConst(rtype:TsrDataType;dst:TsrRegNode;src:PPsrRegNode);
var
 nodes:array[0..3] of TsrConst;
 h:TsrConst;
 i:Byte;
begin
 For i:=0 to rtype.Count-1 do
 begin
  nodes[i]:=src[i].AsConst;
 end;

 h:=ConstList.FetchVector(rtype,@nodes,true);
 dst.pWriter:=h;
end;

procedure TEmitPostOp.MakeVecOne(dst:TsrRegNode;src:PPsrRegNode);
var
 pLine:TSpirvOp;
 rsrc:TsrRegNode;
begin
 pLine:=src[0].pWriter.specialize AsType<ntOp>;
 rsrc:=RegDown(pLine.ParamNode(0).AsReg);
 dst.pWriter:=rsrc;
end;

function TEmitPostOp.MakeVecComp(pLine:TSpirvOp;rtype:TsrDataType;dst:TsrRegNode;src:PPsrRegNode):TSpirvOp;
var
 r:Integer;
 i:Byte;
begin
 Result:=pLine;

 repeat
  r:=0;
  For i:=0 to rtype.Count-1 do
  begin
   r:=r+try_get_comp_bridge(src[i]);
  end;

  if is_all_const(src,rtype.Count) then
  begin
   MakeVecConst(rtype,dst,src);
   Exit; //
  end;

  if is_all_in_one_comp(src,rtype,rtype.Count) then
  begin
   MakeVecOne(dst,src);
   Exit; //
  end;

 until (r=0);

 Result:=OpMakeCon(pLine,dst,src);
end;

function TEmitPostOp.OnMakeVec2(node:TSpirvOp):Integer;
var
 pParam:POpParamNode;
 dst:TsrRegNode;
 src:array[0..3] of TsrRegNode;
 rtype:TsrDataType;
 i:Byte;
begin
 Result:=1;

 dst:=node.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 pParam:=node.ParamFirst;

 rtype:=dst.dtype;

 For i:=0 to rtype.Count-1 do
 begin
  src[i]:=pParam.AsReg;
  pParam:=pParam.Next;
  if (src[i]=nil) then Assert(false,'OnMakeVec2');
 end;

 MakeVecComp(node,rtype,dst,@src);

 node.mark_not_used;
 node.pDst:=nil;
end;

function TEmitPostOp.OnReturn_2(node:TSpirvOp):Integer;
begin
 Result:=0;

 if is_term_op(flow_down_prev_up(node)) then
 begin
  node.mark_not_used;
  Inc(Result);
 end;

end;

function TEmitPostOp.OnMakeExp2(node:TSpirvOp):Integer;
var
 pLine:TSpirvOp;
 pOpBlock:TsrOpBlock;
 pChild:TsrOpBlock;
 pBegOp,pEndOp,pMrgOp:TspirvOp;
 exc:TsrRegNode;
 b_adr:TSrcAdr;
begin
 Result:=1;

 pOpBlock:=node.Parent;

 exc:=RegDown(node.ParamNode(0).AsReg);
 if (exc=nil) then Exit;

 if exc.is_const then
 begin
  node.mark_not_used;

  Case exc.AsConst.AsBool of
   True :  //is always store
     begin
      pLine:=node.Next;
      if (pLine<>nil) then
      begin
       pChild:=pLine.specialize AsType<ntOpBlock>;
       if (pChild<>nil) then
       begin
        //up
        pOpBlock.Remove(pChild);
        pLine:=pOpBlock;
        pLine.InsertAfter(pChild);
        pChild.UpdateLevel;
       end;
      end;
     end;
   False:  //is always kill
     begin
      AddSpirvOp(pOpBlock.dummy,Op.OpKill); //add kill
      //clear all
      node:=pOpBlock.First;
      While (node<>nil) do
      begin
       if node.IsType(ntOpBlock) then
       begin
        pChild:=node.specialize AsType<ntOpBlock>;
        node:=pChild.First;
        Continue;
       end else
       begin
        Case node.OpId of
         Op.OpNop:;
         Op.OpKill:;
         else
          node.mark_not_used(True);
        end;
       end;
       node:=node.Next;
      end;
     end;
  end;
  Exit;
 end else
 begin
  //reread
  exc:=node.ParamNode(0).AsReg;

  node.mark_not_used;

  b_adr:=pOpBlock.Block.b_adr;

  pLine:=node.Next;
  if (pLine=nil) then //kill or nop
  begin

   pBegOp:=NewLabelOp(False); //current
   pEndOp:=NewLabelOp(False); //end
   pMrgOp:=pEndOp;            //merge

   pBegOp.Adr:=b_adr;
   pEndOp.Adr:=b_adr;

   pOpBlock.SetLabels(pBegOp,pEndOp,pMrgOp);
   pOpBlock.Block.bType:=btCond;
   pOpBlock.SetCond(exc,false); //reverse

   pLine:=node;
   pLine:=OpCondMerge (pLine,pMrgOp);
   pLine:=OpBranchCond(pLine,pEndOp,pBegOp,exc); //reverse
   pLine:=AddSpirvOp  (pLine,pBegOp);

     pChild:=AllocBlockOp; //create new
     pChild.SetInfo(btOther,b_adr,b_adr);
     pChild.dummy.OpId:=Op.OpKill; //set kill to dummy

   pOpBlock.pBody:=pChild;

   pLine:=InsertBlockOp(pLine,pChild);

   //OpBranch not need from kill
   pLine:=AddSpirvOp(pLine,pMrgOp);

  end else
  begin //kill or store
   Assert(pLine.IsType(ntOpBlock));

   pBegOp:=NewLabelOp(False); //current
   pEndOp:=NewLabelOp(False); //end
   pMrgOp:=NewLabelOp(False); //merge

   pBegOp.Adr:=b_adr;
   pEndOp.Adr:=b_adr;
   pMrgOp.Adr:=b_adr;

   pOpBlock.SetLabels(pBegOp,pEndOp,pMrgOp);
   pOpBlock.Block.bType:=btCond;
   pOpBlock.SetCond(exc,false); //reverse

   pOpBlock.pElse.Block.bType:=btElse;

   pLine:=node;
   pLine:=OpCondMerge (pLine,pMrgOp);
   pLine:=OpBranchCond(pLine,pEndOp,pBegOp,exc); //reverse
   pLine:=AddSpirvOp  (pLine,pBegOp);

     pChild:=AllocBlockOp; //create new
     pChild.SetInfo(btOther,b_adr,b_adr);
     pChild.dummy.OpId:=Op.OpKill; //set kill to dummy

   pOpBlock.pBody:=pChild;

   pLine:=InsertBlockOp(pLine,pChild);

   //OpBranch not need from kill
   pLine:=AddSpirvOp(pLine,pEndOp);

   //OpStore child

   pLine:=pOpBlock.Last;
   pLine:=OpBranch  (pLine,pMrgOp);
   pLine:=AddSpirvOp(pLine,pMrgOp); //end

  end;

 end;

end;

function TEmitPostOp.OnIAddExt2(node:TSpirvOp):Integer;
var
 rsl:TsrRegPair;
 dst,car:TsrRegNode;
 src:array[0..1] of TsrRegNode;
begin
 Result:=1;
 rsl:=node.pDst.specialize AsType<ntRegPair>;
 if (rsl=nil) then Exit;

 dst:=rsl.pDst0.specialize AsType<ntReg>;
 car:=rsl.pDst1.specialize AsType<ntReg>;
 if (dst=nil) or (car=nil) then Exit;

 src[0]:=node.ParamNode(0).AsReg;
 src[1]:=node.ParamNode(1).AsReg;

 if (src[0]=nil) or (src[1]=nil) then Exit;

 node.mark_not_used;
 node.pDst:=nil;

 if (car.IsUsed) then //carry is use
 begin
  OpIAddCar(node,dst,car,src[0],src[1]);
 end else
 begin
  _Op2(node,Op.OpIAdd,dst,src[0],src[1]);
 end;
end;

function TEmitPostOp.OnISubExt2(node:TSpirvOp):Integer;
var
 rsl:TsrRegPair;
 dst,bor:TsrRegNode;
 src:array[0..1] of TsrRegNode;
begin
 Result:=1;
 rsl:=node.pDst.specialize AsType<ntRegPair>;
 if (rsl=nil) then Exit;

 dst:=rsl.pDst0.specialize AsType<ntReg>;
 bor:=rsl.pDst1.specialize AsType<ntReg>;
 if (dst=nil) or (bor=nil) then Exit;

 src[0]:=node.ParamNode(0).AsReg;
 src[1]:=node.ParamNode(1).AsReg;

 if (src[0]=nil) or (src[1]=nil) then Exit;

 node.mark_not_used;
 node.pDst:=nil;

 if (bor.IsUsed) then //borrow is use
 begin
  OpISubBor(node,dst,bor,src[0],src[1]);
 end else
 begin
  _Op2(node,Op.OpISub,dst,src[0],src[1]);
 end;
end;

function TEmitPostOp.OnPackAnc2(node:TSpirvOp):Integer;
var
 dst:TsrRegNode;
 src:array[0..2] of TsrRegNode;
 num4 :TsrRegNode;
 num8 :TsrRegNode;
 num11:TsrRegNode;
 num16:TsrRegNode;
begin
 Result:=0;
 dst:=node.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 src[0]:=node.ParamNode(0).AsReg;
 src[1]:=node.ParamNode(1).AsReg;
 src[2]:=node.ParamNode(2).AsReg;

 if (src[0]=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 node.mark_not_used;
 node.pDst:=nil;

 num4 :=NewReg_q(dtUint32, 4);
 num8 :=NewReg_q(dtUint32, 8);
 num11:=NewReg_q(dtUint32,11);
 num16:=NewReg_q(dtUint32,16);

 //Base,Insert,Offset,Count
 src[0]:=OpBFITo(src[0],src[1],num8 ,num4 ,@node);
 src[0]:=OpBFITo(src[0],src[2],num16,num11,@node);

 dst.pWriter:=src[0];
 dst.pLine  :=src[0].pLine;
end;

//

end.

