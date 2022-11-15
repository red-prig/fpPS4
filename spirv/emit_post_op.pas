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
  srOp,
  srOpUtils,
  emit_fetch;

type
 TEmitPostOp=class(TEmitFetch)
  function  PostForward1(node:PSpirvOp):Integer;
  function  PostForward2(node:PSpirvOp):Integer;
  //
  function  OpConvert1(node:PSpirvOp):Integer;
  function  OnCompositeExtract1(node:PSpirvOp):Integer;
  function  OnFDiv1(node:PSpirvOp):Integer;
  function  OnIAdd1(node:PSpirvOp):Integer;
  function  OnISub1(node:PSpirvOp):Integer;
  function  OnShr1(node:PSpirvOp):Integer;
  function  _OnShr_ext1(node,pOp:PSpirvOp;pShrVal:PsrConst):Integer;
  function  _OnShr_ext_and(node:PSpirvOp;pShrVal,pAndVal:PsrConst):Integer;
  function  _OnShr_ext_add(node,pOp0,pOp1:PSpirvOp;pShrVal:PsrConst):Integer;
  function  OnAbsDiff1(node:PSpirvOp):Integer;
  function  OnWQM32__1(node:PSpirvOp):Integer;
  function  OnPackOfs1(node:PSpirvOp):Integer;
  function  _Fetch_PackAnc(node:PsrRegNode;index,count:Byte):PsrRegNode;
  function  OnBFEU32_1(node:PSpirvOp):Integer;
  function  OnBFIB32_1(node:PSpirvOp):Integer;
  function  OnMakeCub1(node:PSpirvOp):Integer;
  //
  function  OnBitwiseAnd1(node:PSpirvOp):Integer;
  function  OnLogicalAnd1(node:PSpirvOp):Integer;
  function  OnBitwiseOr1(node:PSpirvOp):Integer;
  function  OnLogicalOr1(node:PSpirvOp):Integer;
  function  OnNot1(node:PSpirvOp):Integer;
  function  OnBranchConditional1(node:PSpirvOp):Integer;
  //
  function  OnSelect1(node:PSpirvOp):Integer;
  //
  procedure MakeVecConst(rtype:TsrDataType;dst:PsrRegNode;src:PPsrRegNode);
  procedure MakeVecOne(dst:PsrRegNode;src:PPsrRegNode);
  function  MakeVecComp(pLine:PSpirvOp;rtype:TsrDataType;dst:PsrRegNode;src:PPsrRegNode):PSpirvOp;
  //
  function  OnMakeVec2(node:PSpirvOp):Integer;
  function  OnReturn_2(node:PSpirvOp):Integer;
  function  OnMakeExp2(node:PSpirvOp):Integer;
  function  OnIAddExt2(node:PSpirvOp):Integer;
  function  OnISubExt2(node:PSpirvOp):Integer;
  function  OnPackAnc2(node:PSpirvOp):Integer;
 end;

implementation

function TEmitPostOp.PostForward1(node:PSpirvOp):Integer;
begin
 Result:=0;

 Case node^.OpId of

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
  srOpUtils.OpBFEU32    :Result:=OnBFEU32_1(node);
  srOpUtils.OpBFIB32    :Result:=OnBFIB32_1(node);
  srOpUtils.OpMakeCub   :Result:=OnMakeCub1(node);

  Op.OpSelect           :Result:=OnSelect1(node);

  Op.OpBitwiseAnd       :Result:=OnBitwiseAnd1(node);
  Op.OpLogicalAnd       :Result:=OnLogicalAnd1(node);
  Op.OpBitwiseOr        :Result:=OnBitwiseOr1(node);
  Op.OpLogicalOr        :Result:=OnLogicalOr1(node);

  Op.OpNot              :Result:=OnNot1(node);

  Op.OpBranchConditional:Result:=OnBranchConditional1(node);

  else;
 end;
end;

function TEmitPostOp.PostForward2(node:PSpirvOp):Integer;
begin
 Result:=0;

 Case node^.OpId of

  srOpUtils.OpIAddExt:Result:=OnIAddExt2(node);
  srOpUtils.OpISubExt:Result:=OnISubExt2(node);
  srOpUtils.OpMakeVec:Result:=OnMakeVec2(node);

  srOpUtils.OpPackAnc:Result:=OnPackAnc2(node);

  Op.OpReturn:Result:=OnReturn_2(node);
  OpMakeExp  :Result:=OnMakeExp2(node);

  OpCUBEID:Assert(False,'TODO');
  OpCUBESC:Assert(False,'TODO');
  OpCUBETC:Assert(False,'TODO');
  OpCUBEMA:Assert(False,'TODO');

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

function _classif_const(p:PsrConst):Integer;
begin
 Result:=_classif_const(p^.dtype,p^.GetData);
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

function TEmitPostOp.OnBitwiseAnd1(node:PSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetOpType(OpId:DWORD;dtype:TsrDataType);
 begin
  Result:=0;
  if (node^.OpId<>OpId) then
  begin
   node^.OpId:=OpId;
   Inc(Result);
  end;
  dst:=node^.pDst^.AsType(ntReg);
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
  //
  dtype:=dst^.dtype;
  node^.pType:=TypeList.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  dst:=node^.pDst^.AsType(ntReg);
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
  //
  dtype:=dst^.dtype;
  node^.pType:=TypeList.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  dtype:=LazyType3(dst^.dtype,src[0]^.dtype,src[1]^.dtype);
  _SetConst(dtype,data[0] and data[1]);
  Exit;
 end;

 if (dst^.is_bool) or ((src[0]^.is_bool_or_const_bool) and (src[1]^.is_bool_or_const_bool)) then
 begin

  if (src[0]^.is_const) then
  begin
   Case src[0]^.AsConst^.AsBool of
    True :_SetReg(src[1]);
    False:_SetConst(dtBool,0);
   end;
   Exit;
  end;
  if (src[1]^.is_const) then
  begin
   Case src[1]^.AsConst^.AsBool of
    True :_SetReg(src[0]);
    False:_SetConst(dtBool,0);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalAnd,dtBool);
 end else
 begin
  dtype:=LazyType3(BinType(dst^.dtype),BinType(src[0]^.dtype),BinType(src[1]^.dtype));
  dtype:=LazyType2(dtype,dtUint32);

  if (src[0]^.is_const) then
  begin
   case _classif_const(src[0]^.AsConst) of
    0:_SetConst(dtype,0); //always false
    1:_SetReg(src[1]);    //always true
   end;
  end;
  if (src[1]^.is_const) then
  begin
   case _classif_const(src[1]^.AsConst) of
    0:_SetConst(dtype,0); //always false
    1:_SetReg(src[0]);    //always true
   end;
  end;

  if (Result<>0) then Exit; //_SetConst/_SetReg
  _SetType(dtype);
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(0),dst^.dtype);
 Result:=Result+PrepTypeParam(node^.ParamNode(1),dst^.dtype);
end;

Function _FindNest_LAnd(node,src:PsrRegNode):Boolean;
var
 p:PSpirvOp;
 tmp:PsrRegNode;
begin
 Result:=False;
 if (node=nil) or (src=nil) then Exit;

 repeat

  p:=node^.pWriter^.AsType(ntOp);
  if (p<>nil) then
   if (p^.OpId=Op.OpLogicalAnd) then
   begin
    tmp:=RegDown(p^.ParamNode(0)^.AsReg);
    if (tmp=src) then Exit(True);
    Result:=_FindNest_LAnd(tmp,src); //recursion
    if Result then Exit(True);
    tmp:=RegDown(p^.ParamNode(1)^.AsReg);
    if (tmp=src) then Exit(True);
    node:=tmp;
    Continue; //cycle
   end;
  Exit;

 until false;

end;

function TEmitPostOp.OnLogicalAnd1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  _SetConst(dtBool,data[0] and data[1]);
  Exit;
 end;

 if (src[0]^.is_const) then
 begin
  Case src[0]^.AsConst^.AsBool of
   True :_SetReg(src[1]);
   False:_SetConst(dtBool,0);
  end;
  Exit;
 end;
 if (src[1]^.is_const) then
 begin
  Case src[1]^.AsConst^.AsBool of
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

 Result:=Result+PrepTypeParam(node^.ParamNode(0),dtBool);
 Result:=Result+PrepTypeParam(node^.ParamNode(1),dtBool);
end;

function _Fetch_BitwiseOr_Const(node:PsrRegNode):PsrConst;
var
 pLine:PSpirvOp;
 src:array[0..1] of PsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 pLine:=node^.pWriter^.AsType(ntOp);
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>Op.OpBitwiseOr) then Exit;

 src[0]:=RegDown(pLine^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(pLine^.ParamNode(1)^.AsReg);

 if (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then Exit;

 if src[0]^.is_const then
 begin
  Result:=src[0]^.AsConst;
 end else
 if src[1]^.is_const then
 begin
  Result:=src[1]^.AsConst;
 end;
end;

function _Fetch_BitwiseOr_Value(node:PsrRegNode):PsrRegNode;
var
 pLine:PSpirvOp;
 src:array[0..1] of PsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 pLine:=node^.pWriter^.AsType(ntOp);
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>Op.OpBitwiseOr) then Exit;

 src[0]:=RegDown(pLine^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(pLine^.ParamNode(1)^.AsReg);

 if (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then Exit;

 if src[0]^.is_const then
 begin
  Result:=src[1];
 end else
 if src[1]^.is_const then
 begin
  Result:=src[0];
 end;
end;

//

function TEmitPostOp.OnBitwiseOr1(node:PSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 pConst:PsrConst;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetOpType(OpId:DWORD;dtype:TsrDataType);
 begin
  Result:=0;
  if (node^.OpId<>OpId) then
  begin
   node^.OpId:=OpId;
   Inc(Result);
  end;
  dst:=node^.pDst^.AsType(ntReg);
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
  //
  dtype:=dst^.dtype;
  node^.pType:=TypeList.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  dst:=node^.pDst^.AsType(ntReg);
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
  //
  dtype:=dst^.dtype;
  node^.pType:=TypeList.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  dtype:=LazyType3(dst^.dtype,src[0]^.dtype,src[1]^.dtype);
  _SetConst(dtype,data[0] or data[1]);
  Exit;
 end;

 if (dst^.is_bool) or ((src[0]^.is_bool_or_const_bool) and (src[1]^.is_bool_or_const_bool)) then
 begin

  if (src[0]^.is_const) then
  begin
   Case src[0]^.AsConst^.AsBool of
    True :_SetConst(dtBool,1);
    False:_SetReg(src[1]);
   end;
   Exit;
  end;
  if (src[1]^.is_const) then
  begin
   Case src[1]^.AsConst^.AsBool of
    True :_SetConst(dtBool,1);
    False:_SetReg(src[0]);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalOr,dtBool);

  Result:=Result+PrepTypeParam(node^.ParamNode(0),dst^.dtype);
  Result:=Result+PrepTypeParam(node^.ParamNode(1),dst^.dtype);

  Exit;
 end;

 //
 pConst:=_Fetch_BitwiseOr_Const(src[0]);
 if (pConst<>nil) and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=         pConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  dtype:=LazyType3(dst^.dtype,src[0]^.dtype,src[1]^.dtype);
  src[1]:=NewReg_q(dtype,data[0] or data[1],@Node);

  src[0]:=_Fetch_BitwiseOr_Value(src[0]);
  Assert(src[0]<>nil);

  node^.ParamNode(0)^.Value:=src[0];
  node^.ParamNode(1)^.Value:=src[1];
 end;

 //
 pConst:=_Fetch_BitwiseOr_Const(src[1]);
 if src[0]^.is_const and (pConst<>nil) then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=         pConst^.GetData;

  dtype:=LazyType3(dst^.dtype,src[0]^.dtype,src[1]^.dtype);
  src[0]:=NewReg_q(dtype,data[0] or data[1],@Node);

  src[1]:=_Fetch_BitwiseOr_Value(src[1]);
  Assert(src[1]<>nil);

  node^.ParamNode(0)^.Value:=src[0];
  node^.ParamNode(1)^.Value:=src[1];
 end;

 //else
 begin
  dtype:=LazyType3(BinType(dst^.dtype),BinType(src[0]^.dtype),BinType(src[1]^.dtype));
  dtype:=LazyType2(dtype,dtUint32);

  if (src[0]^.is_const) then
  begin
   case _classif_const(src[0]^.AsConst) of
    0:_SetReg(src[1]);
    1:_SetConst(dtype,dtype.High); //is high
   end;
  end;
  if (src[1]^.is_const) then
  begin
   case _classif_const(src[1]^.AsConst) of
    0:_SetReg(src[0]);
    1:_SetConst(dtype,dtype.High); //is high
   end;
  end;

  if (Result<>0) then Exit; //_SetConst/_SetReg
  _SetType(dtype);
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(0),dst^.dtype);
 Result:=Result+PrepTypeParam(node^.ParamNode(1),dst^.dtype);
end;

function TEmitPostOp.OnLogicalOr1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  _SetConst(dtBool,data[0] or data[1]);
  Exit;
 end;

 if (src[0]^.is_const) then
 begin
  Case src[0]^.AsConst^.AsBool of
   True :_SetConst(dtBool,1);
   False:_SetReg(src[1]);
  end;
  Exit;
 end;
 if (src[1]^.is_const) then
 begin
  Case src[1]^.AsConst^.AsBool of
   True :_SetConst(dtBool,1);
   False:_SetReg(src[0]);
  end;
  Exit;
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(0),dtBool);
 Result:=Result+PrepTypeParam(node^.ParamNode(1),dtBool);
end;

function TEmitPostOp.OnNot1(node:PSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
 src:PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetOpType(OpId:DWORD;dtype:TsrDataType);
 begin
  Result:=0;
  if (node^.OpId<>OpId) then
  begin
   node^.OpId:=OpId;
   Inc(Result);
  end;
  dst:=node^.pDst^.AsType(ntReg);
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
  //
  dtype:=dst^.dtype;
  node^.pType:=TypeList.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  dst:=node^.pDst^.AsType(ntReg);
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
  //
  dtype:=dst^.dtype;
  node^.pType:=TypeList.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src:=RegDown(node^.ParamNode(0)^.AsReg);

 if (dst=nil) or (src=nil) then Exit;

 if src^.is_const then
 begin
  dtype:=LazyType2(dst^.dtype,src^.dtype);

  //need a const calc
  data[0]:=src^.AsConst^.GetData;
  data[1]:=dtype.High;

  _SetConst(dtype,(not data[0]) and data[1]);
  Exit;
 end;

 if (dst^.is_bool) or (src^.is_bool_or_const_bool) then
 begin

  if (src^.is_const) then
  begin
   Case src^.AsConst^.AsBool of
    True :_SetConst(dtBool,0);
    False:_SetConst(dtBool,1);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalNot,dtBool);
 end else
 begin
  dtype:=LazyType2(BinType(dst^.dtype),BinType(src^.dtype));
  dtype:=LazyType2(dtype,dtUint32);
  _SetType(dtype);
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(0),dst^.dtype);
end;

function TEmitPostOp.OnBranchConditional1(node:PSpirvOp):Integer;
var
 src,prv:PsrRegNode;
 pOp:PSpirvOp;
 pLabel:array[0..1] of PsrRefNode;
begin
 Result:=0;
 src:=RegDown(node^.ParamNode(0)^.AsReg);

 if (src=nil) then Exit;

 pOp:=src^.pWriter^.AsType(ntOp);
 if (pOp=nil) then Exit;

 Case pOp^.OpId of
  Op.OpLogicalNot:;
  else
   Exit;
 end;

 prv:=pOp^.ParamNode(0)^.AsReg;
 if (prv=nil) then Exit;

 node^.ParamNode(0)^.Value:=prv; //set new

 pLabel[0]:=node^.ParamNode(1)^.Value^.AsType(ntRefId); //read
 pLabel[1]:=node^.ParamNode(2)^.Value^.AsType(ntRefId); //read

 node^.ParamNode(1)^.Value:=pLabel[1]; //swap
 node^.ParamNode(2)^.Value:=pLabel[0]; //swap

 Inc(Result);
end;

function try_get_comp_bridge(var src:PsrRegNode):Integer; forward;

function TEmitPostOp.OpConvert1(node:PSpirvOp):Integer;
var
 i:Int64;
 dst,src,tmp:PsrRegNode;
 pc:PsrConst;
 pLine:PspirvOp;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetConst_s(dtype:TsrDataType;value:Single);
 begin
  Assert(dtype=dtFloat32);
  dst^.pWriter:=ConstList.Fetch_s(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 function minz(i:Int64):QWORD;
 begin
  if (i>0) then Result:=i else Result:=0;
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src:=RegDown(node^.ParamNode(0)^.AsReg);

 if (dst=nil) or (src=nil) then Exit;

 tmp:=src;
 While try_get_comp_bridge(tmp)<>0 do
 begin
  tmp:=RegDown(tmp);
 end;

 if (tmp<>src) then
 begin
  node^.ParamNode(0)^.Value:=tmp;
  src:=tmp;
  Inc(Result);
 end;

 i:=0;
 if src^.is_const then
 begin
  pc:=src^.AsConst;
  Case node^.OpId of

   Op.OpFConvert:
     case src^.dtype of
      dtFloat32:
        case dst^.dtype of
         dtHalf16:_SetConst(dst^.dtype,WORD(THalf16(pc^.AsFloat32)));
         else;
        end;
      dtHalf16:
        case dst^.dtype of
         dtFloat32:_SetConst_s(dst^.dtype,Single(pc^.AsHalf16));
         else;
        end;
      else;
     end;

   Op.OpConvertFToU:
     case src^.dtype of
      dtFloat32:
        if TryTruncInt64(pc^.AsFloat32,i) then
        begin
         _SetConst(dst^.dtype,minz(i));
        end;
      else;
     end;

   Op.OpConvertFToS:
     case src^.dtype of
      dtFloat32:
       if TryTruncInt64(pc^.AsFloat32,i) then
       begin
        _SetConst(dst^.dtype,i);
       end;
      else;
     end;

   Op.OpConvertSToF:
     case src^.dtype of
      dtInt32 :_SetConst_s(dst^.dtype,pc^.AsInt32);
      dtUint32:_SetConst_s(dst^.dtype,pc^.AsInt32);

      dtInt64 :_SetConst_s(dst^.dtype,pc^.AsInt64);
      dtUint64:_SetConst_s(dst^.dtype,pc^.AsInt64);
      else;
     end;

   Op.OpConvertUToF:
     case src^.dtype of
      dtInt32 :_SetConst_s(dst^.dtype,pc^.AsUint32);
      dtUint32:_SetConst_s(dst^.dtype,pc^.AsUint32);

      dtInt64 :_SetConst_s(dst^.dtype,pc^.AsUint64);
      dtUint64:_SetConst_s(dst^.dtype,pc^.AsUint64);
      else;
     end;

  end;
 end else
 begin
  pLine:=src^.pWriter^.AsType(ntOp);
  if (pLine=nil) then Exit;

  Case pLine^.OpId of
   Op.OpFConvert:
     begin
      tmp:=RegDown(pLine^.ParamNode(0)^.AsReg);

      Case node^.OpId of
       Op.OpFConvert:
         if (tmp^.dtype=dst^.dtype) then
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

function TEmitPostOp.OnCompositeExtract1(node:PSpirvOp):Integer;
var
 pc:PsrConst;
 dst,src:PsrRegNode;
 pos:PtrUint;
begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src:=RegDown(node^.ParamNode(0)^.AsReg);

 if (dst=nil) or (src=nil) then Exit;

 pos:=0;
 if not node^.ParamNode(1)^.TryGetValue(pos) then Exit;

 if src^.is_const then
 begin
  pc:=src^.AsConst;
  if (pos<pc^.ItemCount) then
  begin
   pc:=pc^.GetConst(pos);
   dst^.pWriter:=pc;

   node^.mark_not_used;
   node^.pDst:=nil;
   Inc(Result);
  end;
 end;
end;

{
function TEmitPostOp.OnSAbs(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:PsrRegNode;

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

function TEmitPostOp.OnSelect1(node:PSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=node^.ParamNode(1)^.AsReg;
 src[1]:=node^.ParamNode(2)^.AsReg;

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 dtype:=LazyType3(dst^.dtype,src[0]^.dtype,src[1]^.dtype);

 if (node^.pType^.dtype<>dtype) then
 begin
  node^.pType:=TypeList.Fetch(dtype);
  if (node^.pType^.dtype=dtype) then
  begin
   Inc(Result);
  end;
 end;

 if (dst^.dtype<>dtype) then
 begin
  Result:=Result+PrepTypeDst(dst,dtype);
  node^.pDst:=dst;
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(1),dst^.dtype);
 Result:=Result+PrepTypeParam(node^.ParamNode(2),dst^.dtype);
end;

function TEmitPostOp.OnFDiv1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 pCon:array[0..1] of PsrConst;

 procedure _SetConst_s(dtype:TsrDataType;value:Single);
 begin
  Assert(dtype=dtFloat32);
  dst^.pWriter:=ConstList.Fetch_s(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (src[0]^.is_const) and (src[1]^.is_const) then
 if (src[0]^.dtype=src[1]^.dtype) then
 begin
  //need a const calc

  pCon[0]:=src[0]^.AsConst;
  pCon[1]:=src[1]^.AsConst;

  Case src[0]^.dtype of
   dtFloat32:
     begin
      _SetConst_s(dst^.dtype,pCon[0]^.AsFloat32/pCon[1]^.AsFloat32);
     end;
   else;
  end;

 end;
end;

function TEmitPostOp.OnIAdd1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (src[0]^.is_const) and (src[1]^.is_const) then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  _SetConst(dst^.dtype,data[0]+data[1]);
 end else
 if (src[0]^.is_const) then
 begin
  if (src[0]^.AsConst^.GetData=0) then
  begin
   _SetReg(src[1]);
  end;
 end else
 if (src[1]^.is_const) then
 begin
  if (src[1]^.AsConst^.GetData=0) then
  begin
   _SetReg(src[0]);
  end;
 end;
end;

function TEmitPostOp.OnISub1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  _SetConst(dst^.dtype,data[0]-data[1]);
 end;
end;

function TEmitPostOp.OnShr1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (src[0]^.is_const) and (src[1]^.is_const) then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  _SetConst(dst^.dtype,data[0] shr data[1]);
 end else
 if (src[1]^.is_const) then
 begin
  Result:=_OnShr_ext1(node,src[0]^.pWriter^.AsType(ntOp),src[1]^.AsConst);
 end;
end;

function TEmitPostOp._OnShr_ext1(node,pOp:PSpirvOp;pShrVal:PsrConst):Integer;
var
 src:array[0..1] of PsrRegNode;

begin
 Result:=0;
 if (pOp=nil) then Exit;

 Case pOp^.OpId of
  Op.OpBitwiseAnd:
    begin
     src[0]:=RegDown(pOp^.ParamNode(0)^.AsReg);
     src[1]:=RegDown(pOp^.ParamNode(1)^.AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[0]^.is_const) then
     begin
      Result:=_OnShr_ext_and(node,pShrVal,src[0]^.AsConst);
     end else
     if (src[1]^.is_const) then
     begin
      Result:=_OnShr_ext_and(node,pShrVal,src[1]^.AsConst);
     end;
    end;

  Op.OpIAdd:
    begin
     src[0]:=RegDown(pOp^.ParamNode(0)^.AsReg);
     src[1]:=RegDown(pOp^.ParamNode(1)^.AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     Result:=_OnShr_ext_add(node,src[0]^.pWriter^.AsType(ntOp),src[1]^.pWriter^.AsType(ntOp),pShrVal);
    end;

  else;
 end;

end;

function TEmitPostOp._OnShr_ext_and(node:PSpirvOp;pShrVal,pAndVal:PsrConst):Integer;
var
 dst:PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 data[0]:=pShrVal^.GetData;
 data[1]:=pAndVal^.GetData;

 data[0]:=High(QWORD) shl data[0];

 if (data[0] and data[1]=0) then
 begin
  dst:=node^.pDst^.AsType(ntReg);
  _SetConst(dst^.dtype,0);
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

function _OpCanBeShrOpt(pOp:PSpirvOp;ShrVal:QWORD;var Delta:QWORD):Boolean;
var
 src:array[0..1] of PsrRegNode;
 data:QWORD;
begin
 Result:=False;
 if (pOp=nil) then Exit;

 Case pOp^.OpId of
  Op.OpIMul:
    begin
     src[0]:=RegDown(pOp^.ParamNode(0)^.AsReg);
     src[1]:=RegDown(pOp^.ParamNode(1)^.AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[0]^.is_const) then
     begin
      data:=src[0]^.AsConst^.GetData;
     end else
     if (src[1]^.is_const) then
     begin
      data:=src[1]^.AsConst^.GetData;
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
     src[0]:=RegDown(pOp^.ParamNode(0)^.AsReg);
     src[1]:=RegDown(pOp^.ParamNode(1)^.AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[1]^.is_const) then
     begin
      data:=src[1]^.AsConst^.GetData;

      Result:=(data>=ShrVal);
      Delta:=data-ShrVal;
     end;

    end;

  else;
 end;
end;

function _GetShrOptReg(pOp:PSpirvOp):PsrRegNode;
var
 src:array[0..1] of PsrRegNode;
begin
 Result:=nil;
 if (pOp=nil) then Exit;

 Case pOp^.OpId of
  Op.OpIMul:
    begin
     src[0]:=RegDown(pOp^.ParamNode(0)^.AsReg);
     src[1]:=RegDown(pOp^.ParamNode(1)^.AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     if (src[0]^.is_const) then
     begin
      Result:=src[1];
     end else
     if (src[1]^.is_const) then
     begin
      Result:=src[0];
     end;
    end;

  Op.OpShiftLeftLogical:
    begin
     src[0]:=RegDown(pOp^.ParamNode(0)^.AsReg);
     src[1]:=RegDown(pOp^.ParamNode(1)^.AsReg);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     Result:=src[0];
    end;

  else;
 end;
end;

function TEmitPostOp._OnShr_ext_add(node,pOp0,pOp1:PSpirvOp;pShrVal:PsrConst):Integer;
var
 dst,src:PsrRegNode;
 dst_shr:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 data[0]:=0;
 data[1]:=0;

 if (not _OpCanBeShrOpt(pOp0,pShrVal^.GetData,data[0])) and
    (not _OpCanBeShrOpt(pOp1,pShrVal^.GetData,data[1])) then Exit;

 if (data[0]=0) then
 begin
  dst_shr[0]:=_GetShrOptReg(pOp0);
 end else
 begin
  dst:=pOp0^.pDst^.AsType(ntReg);

  src:=_GetShrOptReg(pOp0);
  dst_shr[0]:=OpShlTo(src,data[0],@pOp0)
 end;

 if (data[1]=0) then
 begin
  dst_shr[1]:=_GetShrOptReg(pOp1);
 end else
 begin
  dst:=pOp1^.pDst^.AsType(ntReg);

  src:=_GetShrOptReg(pOp1);
  dst_shr[1]:=OpShlTo(src,data[1],@pOp0)
 end;

 dst:=node^.pDst^.AsType(ntReg);
 pOp0:=dst^.pWriter^.AsType(ntOp); //OpIAdd


 src:=OpIAddTo(dst_shr[0],dst_shr[1],@pOp0);
 _SetReg(src);

 //Writeln(data[0],' ',data[1]);
 //writeln;
 Result:=1;
end;

function TEmitPostOp.OnAbsDiff1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;
 rmax,rmin:PsrRegNode;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src[0]:=RegDown(node^.ParamNode(0)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(1)^.AsReg);

 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if src[0]^.is_const and src[1]^.is_const then
 begin
  //need a const calc
  data[0]:=src[0]^.AsConst^.GetData;
  data[1]:=src[1]^.AsConst^.GetData;

  if (data[0]>data[1]) then
   _SetConst(dst^.dtype,data[0]-data[1])
  else
   _SetConst(dst^.dtype,data[1]-data[0]);
  Exit;
 end else
 if src[0]^.is_const then
 begin
  if src[0]^.AsConst^.isZeroVal then
  begin
   //src[1]-0
   src[1]:=node^.ParamNode(1)^.AsReg; //get original
   _SetReg(src[1]);
   Exit;
  end;
 end else
 if src[1]^.is_const then
 begin
  if src[1]^.AsConst^.isZeroVal then
  begin
   //src[0]-0
   src[0]:=node^.ParamNode(0)^.AsReg; //get original
   _SetReg(src[0]);
   Exit;
  end;
 end;

 //else

 node^.mark_not_used;
 node^.pDst:=nil;

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

function TEmitPostOp.OnWQM32__1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:PsrRegNode;
 data:QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.pWriter:=ConstList.Fetch(dtype,value);
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  dst^.pWriter:=src;
  node^.mark_not_used;
  node^.pDst:=nil;
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 src:=RegDown(node^.ParamNode(0)^.AsReg);

 if (dst=nil) or (src=nil) then Exit;

 if src^.is_const then
 begin
  //need a const calc
  data:=src^.AsConst^.GetData;
  data:=F_WQM_32(data);
  _SetConst(dst^.dtype,data);
 end else
 if (src^.dtype=dtBool) then
 begin
  _SetReg(src);
 end else
 begin
  Assert(false,'TODO')
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

function TEmitPostOp.OnPackOfs1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:PsrRegNode;
 data:QWORD;
 P:Ppacked_offset;

 count:PtrUint;

 ret:PsrConst;
 vec:array[0..2] of PsrConst;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 count:=0;
 if not node^.ParamNode(0)^.TryGetValue(count) then Exit;

 src:=RegDown(node^.ParamNode(1)^.AsReg);

 if (src=nil) then Exit;

 if src^.is_const then
 begin
  //need a const calc
  data:=src^.AsConst^.GetData;
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

  dst^.pWriter:=ret;

  node^.mark_not_used;
  node^.pDst:=nil;

  Inc(Result);
 end else
 begin
  Assert(false,'TODO');
 end;
end;

////////

function TEmitPostOp._Fetch_PackAnc(node:PsrRegNode;index,count:Byte):PsrRegNode;
var
 pLine:PSpirvOp;
 src:array[0..2] of PsrRegNode;
 prim,smid,rtid:Boolean;
begin
 Result:=nil;

 if (node=nil) then Exit;
 if (count=0) then Exit;

 pLine:=node^.pWriter^.AsType(ntOp);
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>srOpUtils.OpPackAnc) then Exit;

 src[0]:=pLine^.ParamNode(0)^.AsReg;
 src[1]:=pLine^.ParamNode(1)^.AsReg;
 src[2]:=pLine^.ParamNode(2)^.AsReg;

 if (src[0]=nil) or (src[1]=nil) or (src[2]=nil) then Exit;

 prim:=                (index+count<= 2); //[ 1: 0]
 smid:=(index>= 8) and (index+count<=12); //[11: 8]
 rtid:=(index>=16) and (index+count<=27); //[26:16]

 count:=ord(prim)+ord(smid)+ord(rtid);

 if (count=0) then
 begin
  Result:=NewReg_q(node^.dtype,0);
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

function TEmitPostOp.OnBFEU32_1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 rBase,rIndex,rCount:PsrRegNode;
 rsl:PsrRegNode;
 num_31:PsrRegNode;
 data:array[0..1] of QWORD;
 index,count:Byte;
begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 rBase :=node^.ParamNode(0)^.AsReg;
 rIndex:=RegDown(node^.ParamNode(1)^.AsReg);
 rCount:=RegDown(node^.ParamNode(2)^.AsReg);

 if (rBase=nil) or (rIndex=nil) or (rCount=nil) then Exit;

 //else
 node^.mark_not_used;
 node^.pDst:=nil;

 if (rIndex^.is_const) and (rCount^.is_const) then
 begin
  data[0]:=rIndex^.AsConst^.GetData;
  data[1]:=rCount^.AsConst^.GetData;
  //
  index:=Byte(data[0] and 31);
  count:=Byte(data[1] and 31);
  //
  rsl:=_Fetch_PackAnc(rBase,index,count);

  if (rsl<>nil) then
  begin

   rBase:=RegDown(rsl);
   if (rBase^.is_const) then
   begin
    data[0]:=rBase^.AsConst^.GetData;
    data[1]:=(1 shl count)-1;
    data[0]:=data[0] and data[1];
    //
    if (data[0]<>rBase^.AsConst^.GetData) then
    begin
     rsl:=NewReg_q(dtUInt32,data[0],@Node);
    end;
   end else
   begin
    data[1]:=(1 shl count)-1;
    num_31:=NewReg_q(dtUInt32,data[1],@Node);
    //
    rsl:=OpAndTo(rsl,num_31,@node);
    rsl^.PrepType(ord(dtUInt32));
   end;

   dst^.pWriter:=rsl;
   dst^.pLine  :=rsl^.pLine;

   Exit;
  end;

 end;

 num_31:=nil;

 //
 if (rIndex^.is_const) then
 begin
  data[0]:=rIndex^.AsConst^.GetData;
  data[0]:=data[0] and 31;
  //
  if (data[0]<>rIndex^.AsConst^.GetData) then
  begin
   rIndex:=NewReg_q(dtUInt32,data[0],@Node);
  end else
  begin
   rIndex:=node^.ParamNode(1)^.AsReg; //orig
  end;
 end else
 begin
  num_31:=NewReg_q(dtUInt32,31,@Node);
  //
  rIndex:=node^.ParamNode(1)^.AsReg; //orig
  rIndex:=OpAndTo(rIndex,num_31,@node);
  rIndex^.PrepType(ord(dtUInt32));
 end;

 //
 if (rCount^.is_const) then
 begin
  data[1]:=rCount^.AsConst^.GetData;
  data[1]:=data[1] and 31;
  //
  if (data[1]<>rCount^.AsConst^.GetData) then
  begin
   rCount:=NewReg_q(dtUInt32,data[1],@Node);
  end else
  begin
   rCount:=node^.ParamNode(2)^.AsReg; //orig
  end;
 end else
 begin
  if (num_31<>nil) then
  begin
   num_31:=NewReg_q(dtUInt32,31,@Node);
  end;
  //
  rCount:=node^.ParamNode(2)^.AsReg; //orig
  rCount:=OpAndTo(rCount,num_31,@node);
  rCount^.PrepType(ord(dtUInt32));
 end;

 _Op3(node,Op.OpBitFieldUExtract,dst,rBase,rIndex,rCount);
end;

function TEmitPostOp.OnBFIB32_1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 bitmsk:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 rIndex,rCount:PsrRegNode;
 data:array[0..1] of QWORD;
 index,count:DWORD;
begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 bitmsk:=RegDown(node^.ParamNode(0)^.AsReg);
 src[0]:=RegDown(node^.ParamNode(1)^.AsReg);
 src[1]:=RegDown(node^.ParamNode(2)^.AsReg);

 if (bitmsk=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if bitmsk^.is_const then
 begin
  data[0]:=bitmsk^.AsConst^.GetData;

  index:=BsfQWord(data[0]);
  count:=PopCnt  (data[0]);

  data[1]:=((1 shl count)-1) shl index;

  if (data[0]=data[1]) then
  begin

   if (index<>0) then
   begin
    Assert(false,'TODO');
   end;

   node^.mark_not_used;
   node^.pDst:=nil;

   rIndex:=NewReg_q(dtUint32,index,@Node);
   rCount:=NewReg_q(dtUint32,count,@Node);

   src[0]:=node^.ParamNode(1)^.AsReg;
   src[1]:=node^.ParamNode(2)^.AsReg;

   _Op4(node,Op.OpBitFieldInsert,dst,src[1],src[0],rIndex,rCount);

   Exit;
  end;

 end;

 //else
 node^.mark_not_used;
 node^.pDst:=nil;

 src[0]:=node^.ParamNode(1)^.AsReg;
 src[1]:=node^.ParamNode(2)^.AsReg;

 src[0]:=OpAndTo(src[0],bitmsk,@node);
 src[0]^.PrepType(ord(dtUInt32));

 bitmsk:=OpNotTo(bitmsk,@node);
 bitmsk^.PrepType(ord(dtUInt32));

 src[1]:=OpAndTo(src[1],bitmsk,@node);
 src[1]^.PrepType(ord(dtUInt32));

 _Op2(node,Op.OpBitwiseOr,dst,src[0],src[1]);
end;

function _IsFma(node:PSpirvOp):Boolean;
var
 OpId:PtrUint;
begin
 Result:=False;
 if (node=nil) then Exit;
 if (node^.OpId<>Op.OpExtInst) then Exit;
 OpId:=0;
 node^.ParamNode(1)^.TryGetValue(OpId);
 if (OpId<>GlslOp.Fma) then Exit;
 Result:=True;
end;

function _IsOp(node:PSpirvOp;OpId:DWORD):Boolean;
begin
 Result:=False;
 if (node=nil) then Exit;
 Result:=(node^.OpId=OpId);
end;

function _IsConstFloat_1_0(pReg:PsrRegNode):Boolean;
var
 pConst:PsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pConst:=pReg^.AsConst;
 if (pConst=nil) then Exit;
 if (pConst^.dtype<>dtFloat32) then Exit;
 Result:=(Round(pConst^.AsFloat32*10)=10);
end;

function _IsConstFloat_1_5(pReg:PsrRegNode):Boolean;
var
 pConst:PsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pConst:=pReg^.AsConst;
 if (pConst=nil) then Exit;
 if (pConst^.dtype<>dtFloat32) then Exit;
 Result:=(Round(pConst^.AsFloat32*10)=15);
end;

function _Fetch_FAbs_Value(node:PSpirvOp):PSpirvOp;
var
 OpId:PtrUint;
 pReg:PsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 if (node^.OpId<>Op.OpExtInst) then Exit;
 OpId:=0;
 node^.ParamNode(1)^.TryGetValue(OpId);
 if (OpId<>GlslOp.FAbs) then Exit;
 pReg:=RegDown(node^.ParamNode(2)^.AsReg);
 if (pReg=nil) then Exit;
 Result:=pReg^.pWriter^.AsType(ntOp);
end;

function _cmp_src_cube_op3(node0,node1:PSpirvOp):Boolean;
var
 src0:PsrRegNode;
 src1:PsrRegNode;
 i:Byte;
begin
 Result:=False;
 if (node0=nil) or (node1=nil) then Exit;
 For i:=0 to 2 do
 begin
  src0:=RegDown(node0^.ParamNode(i)^.AsReg);
  src1:=RegDown(node1^.ParamNode(i)^.AsReg);
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
  if not src[i]^.is_const then Exit(false);
end;

function is_all_in_one_comp(src:PPsrRegNode;rtype:TsrDataType;count:byte):Boolean;
var
 i:Byte;
 pos:PtrUint;
 pLine:PspirvOp;
 pReg,tmp:PsrRegNode;
begin
 pReg:=nil;
 Result:=True;
 For i:=0 to count-1 do
 begin
  pLine:=src[i]^.pWriter^.AsType(ntOp);
  if (pLine=nil) then Exit(false);
  if (pLine^.OpId<>Op.OpCompositeExtract) then Exit(false);

  pos:=0;
  if not pLine^.ParamNode(1)^.TryGetValue(pos) then Exit;
  if (pos<>i) then Exit(false);

  tmp:=RegDown(pLine^.ParamNode(0)^.AsReg);
  if (tmp=nil) then Exit(false);
  if (tmp^.dtype<>rtype) then Exit(false);

  if (i=0) then
  begin
   pReg:=tmp;
  end else
  begin
   if (pReg<>tmp) then Exit(false);
  end;

 end;
end;

function try_get_comp_bridge(var src:PsrRegNode):Integer;
var
 pLine:PspirvOp;
 pos:PtrUint;
 pReg:PsrRegNode;
begin
 Result:=0;
 pLine:=src^.pWriter^.AsType(ntOp);
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>Op.OpCompositeExtract) then Exit;

 pos:=0;
 if not pLine^.ParamNode(1)^.TryGetValue(pos) then Exit;

 pReg:=RegDown(pLine^.ParamNode(0)^.AsReg);
 if (pReg=nil) then Exit;

 pLine:=pReg^.pWriter^.AsType(ntOp);
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>Op.OpCompositeConstruct) then Exit;

 pReg:=RegDown(pLine^.ParamNode(pos)^.AsReg);
 if (pReg=nil) then Exit;
 src:=pReg;
 Result:=1;
end;

function TEmitPostOp.OnMakeCub1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..2] of PsrRegNode;

 m_CUBE_SC:PspirvOp;
 m_CUBE_TC:PspirvOp;
 m_CUBE_ID:PspirvOp;

 m_x_CUBE_MA:PspirvOp;
 m_y_CUBE_MA:PspirvOp;

 m_x,m_y,m_f:PspirvOp;

 pReg:PsrRegNode;
 pOp:PspirvOp;

 rtype:TsrDataType;
 i:Byte;

begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 rtype:=dst^.dtype;

 if (rtype.Child<>dtFloat32) then Assert(false,'TODO');
 if (rtype.Count=4) then Assert(false,'TODO');

 m_x:=RegDown(node^.ParamNode(0)^.AsReg)^.pWriter^.AsType(ntOp); //param1
 m_y:=RegDown(node^.ParamNode(1)^.AsReg)^.pWriter^.AsType(ntOp); //param2
 m_f:=RegDown(node^.ParamNode(2)^.AsReg)^.pWriter^.AsType(ntOp); //param3

 if not _IsFma(m_x) then Exit;
 if not _IsFma(m_y) then Exit;
 if not _IsOp(m_f,OpCUBEID) then Exit;

 m_CUBE_ID:=m_f;

 //m_x
 pReg:=RegDown(m_x^.ParamNode(2)^.AsReg); //param1
 pOp:=pReg^.pWriter^.AsType(ntOp);
 if not _IsOp(pOp,OpCUBESC) then Exit;
 m_CUBE_SC:=pOp;

 pReg:=RegDown(m_x^.ParamNode(3)^.AsReg); //param2
 pOp:=pReg^.pWriter^.AsType(ntOp);
 if not _IsOp(pOp,Op.OpFDiv) then Exit;

   pReg:=RegDown(pOp^.ParamNode(0)^.AsReg);  //div
   if not _IsConstFloat_1_0(pReg) then Exit; //1.0

   pReg:=RegDown(pOp^.ParamNode(1)^.AsReg);
   pOp:=_Fetch_FAbs_Value(pReg^.pWriter^.AsType(ntOp));
   if not _IsOp(pOp,OpCUBEMA) then Exit;
   m_x_CUBE_MA:=pOp;

 pReg:=RegDown(m_x^.ParamNode(4)^.AsReg);  //param3
 if not _IsConstFloat_1_5(pReg) then Exit; //1.5

 //m_y
 pReg:=RegDown(m_y^.ParamNode(2)^.AsReg); //param1
 pOp:=pReg^.pWriter^.AsType(ntOp);
 if not _IsOp(pOp,OpCUBETC) then Exit;
 m_CUBE_TC:=pOp;

 pReg:=RegDown(m_x^.ParamNode(3)^.AsReg); //param2
 pOp:=pReg^.pWriter^.AsType(ntOp);
 if not _IsOp(pOp,Op.OpFDiv) then Exit;

   pReg:=RegDown(pOp^.ParamNode(0)^.AsReg);  //div
   if not _IsConstFloat_1_0(pReg) then Exit; //1.0

   pReg:=RegDown(pOp^.ParamNode(1)^.AsReg);
   pOp:=_Fetch_FAbs_Value(pReg^.pWriter^.AsType(ntOp));
   if not _IsOp(pOp,OpCUBEMA) then Exit;
   m_y_CUBE_MA:=pOp;

 pReg:=RegDown(m_y^.ParamNode(4)^.AsReg);  //param3
 if not _IsConstFloat_1_5(pReg) then Exit; //1.5

 //

 if not _cmp_src_cube_op3(m_CUBE_SC,m_CUBE_TC  ) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_CUBE_ID  ) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_x_CUBE_MA) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_y_CUBE_MA) then Exit;

 For i:=0 to 2 do
 begin
  src[i]:=RegDown(m_CUBE_SC^.ParamNode(i)^.AsReg);
 end;

 MakeVecComp(node,dtVec3f,dst,@src);

 node^.mark_not_used;
 node^.pDst:=nil;
 Result:=1;
end;

procedure TEmitPostOp.MakeVecConst(rtype:TsrDataType;dst:PsrRegNode;src:PPsrRegNode);
var
 nodes:array[0..3] of PsrConst;
 h:PsrConst;
 i:Byte;
begin
 For i:=0 to rtype.Count-1 do
 begin
  nodes[i]:=src[i]^.AsConst;
 end;

 h:=ConstList.FetchVector(rtype,@nodes,true);
 dst^.pWriter:=h;
end;

procedure TEmitPostOp.MakeVecOne(dst:PsrRegNode;src:PPsrRegNode);
var
 pLine:PspirvOp;
 rsrc:PsrRegNode;
begin
 pLine:=src[0]^.pWriter^.AsType(ntOp);
 rsrc:=RegDown(pLine^.ParamNode(0)^.AsReg);
 dst^.pWriter:=rsrc;
end;

function TEmitPostOp.MakeVecComp(pLine:PSpirvOp;rtype:TsrDataType;dst:PsrRegNode;src:PPsrRegNode):PSpirvOp;
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

function TEmitPostOp.OnMakeVec2(node:PSpirvOp):Integer;
var
 pParam:POpParamNode;
 dst:PsrRegNode;
 src:array[0..3] of PsrRegNode;
 rtype:TsrDataType;
 i:Byte;
begin
 Result:=1;

 dst:=node^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 pParam:=node^.ParamFirst;

 rtype:=dst^.dtype;

 For i:=0 to rtype.Count-1 do
 begin
  src[i]:=pParam^.AsReg;
  pParam:=pParam^.Next;
  if (src[i]=nil) then Assert(false,'OnMakeVec2');
 end;

 MakeVecComp(node,rtype,dst,@src);

 node^.mark_not_used;
 node^.pDst:=nil;
end;

function TEmitPostOp.OnReturn_2(node:PSpirvOp):Integer;
begin
 Result:=0;

 if is_term_op(node^.Prev) then
 begin
  node^.mark_not_used;
  Inc(Result);
 end;

end;

function TEmitPostOp.OnMakeExp2(node:PSpirvOp):Integer;
var
 pLine:PspirvOp;
 pOpBlock:PsrOpBlock;
 pChild:PsrOpBlock;
 pOpLabel:array[0..2] of PspirvOp;
 exc:PsrRegNode;
 b_adr:TSrcAdr;
begin
 Result:=1;

 pOpBlock:=node^.Parent;

 exc:=RegDown(node^.ParamNode(0)^.AsReg);
 if (exc=nil) then Exit;

 if exc^.is_const then
 begin
  node^.mark_not_used;

  Case exc^.AsConst^.AsBool of
   True :  //is always store
     begin
      pLine:=node^.Next;
      if (pLine<>nil) then
      begin
       pChild:=pLine^.AsType(ntOpBlock);
       if (pChild<>nil) then
       begin
        //up
        pOpBlock^.Remove(pChild);
        pLine:=PspirvOp(pOpBlock);
        pLine^.InsertAfter(PspirvOp(pChild));
        pChild^.UpdateLevel;
       end;
      end;
     end;
   False:  //is always kill
     begin
      AddSpirvOp(@pOpBlock^.dummy,Op.OpKill); //add kill
      //clear all
      node:=pOpBlock^.First;
      While (node<>nil) do
      begin
       if node^.IsType(ntOpBlock) then
       begin
        pChild:=node^.AsType(ntOpBlock);
        node:=pChild^.First;
        Continue;
       end else
       begin
        Case node^.OpId of
         Op.OpNop:;
         Op.OpKill:;
         else
          node^.mark_not_used;
        end;
       end;
       node:=node^.Next;
      end;
     end;
  end;
  Exit;
 end else
 begin
  node^.mark_not_used;

  b_adr:=pOpBlock^.Block.b_adr;

  pLine:=node^.Next;
  if (pLine=nil) then //kill or nop
  begin

   pOpLabel[0]:=NewLabelOp(False); //current
   pOpLabel[1]:=NewLabelOp(False); //end

   pOpLabel[0]^.Adr:=b_adr;
   pOpLabel[1]^.Adr:=b_adr;

   pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[1],nil);
   pOpBlock^.Block.bType:=btCond;
   pOpBlock^.SetCond(nil,true);

   pLine:=node;
   pLine:=OpCondMerge(pLine,pOpLabel[1]);
   pLine:=OpBranchCond(pLine,pOpLabel[1],pOpLabel[0],exc); //reverse
   pLine:=AddSpirvOp(pLine,pOpLabel[0]);

     pChild:=AllocBlockOp; //create new
     pChild^.SetInfo(btOther,b_adr,b_adr);
     pChild^.dummy.OpId:=Op.OpKill; //set kill to dummy

   pLine:=InsertBlockOp(pLine,pChild);

   //OpBranch not need from kill
   pLine:=OpBranch(pLine,pOpLabel[1]);
   pLine:=AddSpirvOp(pLine,pOpLabel[1]);

  end else
  begin //kill or store
   Assert(pLine^.IsType(ntOpBlock));

   pOpLabel[0]:=NewLabelOp(False); //current
   pOpLabel[1]:=NewLabelOp(False); //else
   pOpLabel[2]:=NewLabelOp(False); //end

   pOpLabel[0]^.Adr:=b_adr;
   pOpLabel[1]^.Adr:=b_adr;
   pOpLabel[2]^.Adr:=b_adr;

   pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[2],pOpLabel[1]);
   pOpBlock^.Block.bType:=btCond;
   pOpBlock^.SetCond(nil,true);

   pLine:=node;
   pLine:=OpCondMerge(pLine,pOpLabel[2]);
   pLine:=OpBranchCond(pLine,pOpLabel[1],pOpLabel[0],exc); //reverse
   pLine:=AddSpirvOp(pLine,pOpLabel[0]);

     pChild:=AllocBlockOp; //create new
     pChild^.SetInfo(btOther,b_adr,b_adr);
     pChild^.dummy.OpId:=Op.OpKill; //set kill to dummy

   pLine:=InsertBlockOp(pLine,pChild);

   //OpBranch not need from kill
   pLine:=AddSpirvOp(pLine,pOpLabel[1]);

   //OpStore child

   pLine:=pOpBlock^.Last;
   pLine:=OpBranch(pLine,pOpLabel[2]);
   pLine:=AddSpirvOp(pLine,pOpLabel[2]); //end

  end;

 end;

end;

function TEmitPostOp.OnIAddExt2(node:PSpirvOp):Integer;
var
 rsl:PsrRegPair;
 dst,car:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 Result:=1;
 rsl:=node^.pDst^.AsType(ntRegPair);
 if (rsl=nil) then Exit;

 dst:=rsl^.pDst0^.AsType(ntReg);
 car:=rsl^.pDst1^.AsType(ntReg);
 if (dst=nil) or (car=nil) then Exit;

 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 if (src[0]=nil) or (src[1]=nil) then Exit;

 node^.mark_not_used;
 node^.pDst:=nil;

 if (car^.IsUsed) then //carry is use
 begin
  OpIAddCar(node,dst,car,src[0],src[1]);
 end else
 begin
  _Op2(node,Op.OpIAdd,dst,src[0],src[1]);
 end;
end;

function TEmitPostOp.OnISubExt2(node:PSpirvOp):Integer;
var
 rsl:PsrRegPair;
 dst,bor:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 Result:=1;
 rsl:=node^.pDst^.AsType(ntRegPair);
 if (rsl=nil) then Exit;

 dst:=rsl^.pDst0^.AsType(ntReg);
 bor:=rsl^.pDst1^.AsType(ntReg);
 if (dst=nil) or (bor=nil) then Exit;

 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 if (src[0]=nil) or (src[1]=nil) then Exit;

 node^.mark_not_used;
 node^.pDst:=nil;

 if (bor^.IsUsed) then //borrow is use
 begin
  OpISubBor(node,dst,bor,src[0],src[1]);
 end else
 begin
  _Op2(node,Op.OpISub,dst,src[0],src[1]);
 end;
end;

function TEmitPostOp.OnPackAnc2(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..2] of PsrRegNode;
 num4 :PsrRegNode;
 num8 :PsrRegNode;
 num11:PsrRegNode;
 num16:PsrRegNode;
begin
 Result:=0;
 dst:=node^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;
 src[2]:=node^.ParamNode(2)^.AsReg;

 if (src[0]=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 node^.mark_not_used;
 node^.pDst:=nil;

 num4 :=NewReg_q(dtUint32, 4);
 num8 :=NewReg_q(dtUint32, 8);
 num11:=NewReg_q(dtUint32,11);
 num16:=NewReg_q(dtUint32,16);

 //Base,Insert,Offset,Count
 src[0]:=OpBFITo(src[0],src[1],num8 ,num4 ,@node);
 src[0]:=OpBFITo(src[0],src[2],num16,num11,@node);

 dst^.pWriter:=src[0];
 dst^.pLine  :=src[0]^.pLine;
end;

//

end.

