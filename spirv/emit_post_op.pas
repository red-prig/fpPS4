unit emit_post_op;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  bittype,
  Half16,
  srLabel,
  srNodes,
  srTypes,
  srRefId,
  srConst,
  srReg,
  srOp,
  srOpUtils,
  spirv,
  SprvEmit,
  emit_op,
  emit_post;

type
 TEmitPostOp=object(TSprvEmit_post)
  function  PostForward1(node:PSpirvOp):Integer;
  function  PostForward2(node:PSpirvOp):Integer;
  //
  function  OpConvert1(node:PSpirvOp):Integer;
  function  OnCompositeExtract1(node:PSpirvOp):Integer;
  function  OnIAdd1(node:PSpirvOp):Integer;
  function  OnISub1(node:PSpirvOp):Integer;
  function  OnShr1(node:PSpirvOp):Integer;
  function  _OnShr_ext1(node,pOp:PSpirvOp;pShrVal:PsrConst):Integer;
  function  _OnShr_ext_and(node:PSpirvOp;pShrVal,pAndVal:PsrConst):Integer;
  function  _OnShr_ext_add(node,pOp0,pOp1:PSpirvOp;pShrVal:PsrConst):Integer;
  function  OnAbsDiff1(node:PSpirvOp):Integer;
  function  OnWQM32__1(node:PSpirvOp):Integer;
  function  OnPackOfs1(node:PSpirvOp):Integer;
  function  OnMakeCM1(node:PSpirvOp):Integer;
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
  procedure MakeVecConst(rtype:TsrDataType;dst:PsrRegNode;count:Byte;src:PPsrRegNode);
  procedure MakeVecOne(dst:PsrRegNode;src:PPsrRegNode);
  function  MakeVecComp(pLine:PSpirvOp;rtype:TsrDataType;dst:PsrRegNode;count:Byte;src:PPsrRegNode):PSpirvOp;
  //
  function  OnMakeVec2(node:PSpirvOp):Integer;
  function  OnReturn_2(node:PSpirvOp):Integer;
  function  OnMakeExp2(node:PSpirvOp):Integer;
  function  OnIAddExt2(node:PSpirvOp):Integer;
  function  OnISubExt2(node:PSpirvOp):Integer;
  //function  NewMED3F:PSpirvFunc;
  //function  OpMED3F__2(node:PSpirvOp):Integer;
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

  Op.OpIAdd             :Result:=OnIAdd1(node);
  Op.OpISub             :Result:=OnISub1(node);
  Op.OpShiftRightLogical,
  Op.OpShiftRightArithmetic:Result:=OnShr1(node);

  OpAbsDiff             :Result:=OnAbsDiff1(node);
  OpWQM32               :Result:=OnWQM32__1(node);
  OpPackOfs             :Result:=OnPackOfs1(node);
  OpMakeCM              :Result:=OnMakeCM1(node);

  Op.OpSelect           :Result:=Result+OnSelect1(node);

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

  //Op.OpIAdd             :Result:=OnIAdd1(node);
  //Op.OpISub             :Result:=OnISub1(node);

  OpIAddExt:Result:=OnIAddExt2(node);
  OpISubExt:Result:=OnISubExt2(node);
  OpMakeVec:Result:=OnMakeVec2(node);
      //OpMED3F  :Result:=OpMED3F__2(node);

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
 if (value=GetTypeHigh(dtype)) then //always true
 begin
  Result:=1;
 end else
 begin
  Result:=-1;
 end;
end;

function _classif_const(p:PsrConst):Integer;
begin
 Result:=_classif_const(p^.key.dtype,p^.GetData);
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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  src^.mark_read;
  dst^.pWriter.SetParam(ntReg,src);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
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
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  //
  dtype:=dst^.dtype;
  if (node^.dst_type<>nil) then
  begin
   node^.dst_type^.mark_unread;
  end;
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  //
  dtype:=dst^.dtype;
  if (node^.dst_type<>nil) then
  begin
   node^.dst_type^.mark_unread;
  end;
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
    True :_SetReg(node^.ParamNode(1)^.pData);
    False:_SetConst(dtBool,0);
   end;
   Exit;
  end;
  if (src[1]^.is_const) then
  begin
   Case src[1]^.AsConst^.AsBool of
    True :_SetReg(node^.ParamNode(0)^.pData);
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
    1:_SetReg(node^.ParamNode(1)^.pData);    //always true
   end;
  end;
  if (src[1]^.is_const) then
  begin
   case _classif_const(src[1]^.AsConst) of
    0:_SetConst(dtype,0); //always false
    1:_SetReg(node^.ParamNode(0)^.pData);    //always true
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

  p:=node^.AsOp;
  if (p<>nil) then
   if (p^.OpId=Op.OpLogicalAnd) then
   begin
    tmp:=p^.ParamNode(0)^.AsReg;
    tmp:=RegDown(tmp);
    if (tmp=src) then Exit(True);
    Result:=_FindNest_LAnd(tmp,src); //recursion
    if Result then Exit(True);
    tmp:=p^.ParamNode(1)^.AsReg;
    tmp:=RegDown(tmp);
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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  src^.mark_read;
  dst^.pWriter.SetParam(ntReg,src);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
   True :_SetReg(node^.ParamNode(1)^.pData);
   False:_SetConst(dtBool,0);
  end;
  Exit;
 end;
 if (src[1]^.is_const) then
 begin
  Case src[1]^.AsConst^.AsBool of
   True :_SetReg(node^.ParamNode(0)^.pData);
   False:_SetConst(dtBool,0);
  end;
  Exit;
 end;

 if _FindNest_LAnd(src[1],src[0]) then //Find src[0] in src[1]
 begin
  _SetReg(node^.ParamNode(1)^.pData);
  Exit;
 end;

 if _FindNest_LAnd(src[0],src[1]) then //Find src[1] in src[0]
 begin
  _SetReg(node^.ParamNode(0)^.pData);
  Exit;
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(0),dtBool);
 Result:=Result+PrepTypeParam(node^.ParamNode(1),dtBool);
end;

//

function TEmitPostOp.OnBitwiseOr1(node:PSpirvOp):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  src^.mark_read;
  dst^.pWriter.SetParam(ntReg,src);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
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
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  //
  dtype:=dst^.dtype;
  if (node^.dst_type<>nil) then
  begin
   node^.dst_type^.mark_unread;
  end;
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  //
  dtype:=dst^.dtype;
  if (node^.dst_type<>nil) then
  begin
   node^.dst_type^.mark_unread;
  end;
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
    False:_SetReg(node^.ParamNode(1)^.pData);
   end;
   Exit;
  end;
  if (src[1]^.is_const) then
  begin
   Case src[1]^.AsConst^.AsBool of
    True :_SetConst(dtBool,1);
    False:_SetReg(node^.ParamNode(0)^.pData);
   end;
   Exit;
  end;

  _SetOpType(Op.OpLogicalOr,dtBool);
 end else
 begin
  dtype:=LazyType3(BinType(dst^.dtype),BinType(src[0]^.dtype),BinType(src[1]^.dtype));
  dtype:=LazyType2(dtype,dtUint32);

  if (src[0]^.is_const) then
  begin
   case _classif_const(src[0]^.AsConst) of
    0:_SetReg(node^.ParamNode(1)^.pData);
    1:_SetConst(dtype,GetTypeHigh(dtype)); //is high
   end;
  end;
  if (src[1]^.is_const) then
  begin
   case _classif_const(src[1]^.AsConst) of
    0:_SetReg(node^.ParamNode(0)^.pData);
    1:_SetConst(dtype,GetTypeHigh(dtype)); //is high
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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  src^.mark_read;
  dst^.pWriter.SetParam(ntReg,src);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
   False:_SetReg(node^.ParamNode(1)^.pData);
  end;
  Exit;
 end;
 if (src[1]^.is_const) then
 begin
  Case src[1]^.AsConst^.AsBool of
   True :_SetConst(dtBool,1);
   False:_SetReg(node^.ParamNode(0)^.pData);
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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
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
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  //
  dtype:=dst^.dtype;
  if (node^.dst_type<>nil) then
  begin
   node^.dst_type^.mark_unread;
  end;
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
 end;

 procedure _SetType(dtype:TsrDataType);
 begin
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  //
  dtype:=dst^.dtype;
  if (node^.dst_type<>nil) then
  begin
   node^.dst_type^.mark_unread;
  end;
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src:=node^.ParamNode(0)^.AsReg;
 src:=RegDown(src);
 if (dst=nil) or (src=nil) then Exit;

 if src^.is_const then
 begin
  dtype:=LazyType2(dst^.dtype,src^.dtype);

  //need a const calc
  data[0]:=src^.AsConst^.GetData;
  data[1]:=GetTypeHigh(dtype);

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
 cur,src,prv:PsrRegNode;
 pOp:PSpirvOp;
 pLabel:array[0..1] of PsrRefId;
begin
 Result:=0;
 cur:=node^.ParamNode(0)^.AsReg;
 if (cur=nil) then Exit;
 src:=RegDown(cur);

 pOp:=src^.AsOp;
 if (pOp=nil) then Exit;

 Case pOp^.OpId of
  Op.OpLogicalNot:;
  //Op.OpNot:;
  else
   Exit;
 end;

 prv:=pOp^.ParamNode(0)^.AsReg;
 if (prv=nil) then Exit;

 node^.ParamNode(0)^.pData:=prv; //set new

 pLabel[0]:=node^.ParamNode(1)^.pData; //read
 pLabel[1]:=node^.ParamNode(2)^.pData; //read

 node^.ParamNode(1)^.pData:=pLabel[1]; //swap
 node^.ParamNode(2)^.pData:=pLabel[0]; //swap

 prv^.mark_read;
 RegUnmark(cur);

 Inc(Result);
end;

function TEmitPostOp.OpConvert1(node:PSpirvOp):Integer;
var
 dst,src:PsrRegNode;
 pc:PsrConst;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 procedure _SetConst(dtype:TsrDataType;value:double);
 begin
  Case dtype of
   dtFloat32:dst^.SetConst(FConsts.Fetchf(dtype,value));
   else
     Assert(false,'TODO');
  end;
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 function minz(i:Int64):QWORD;
 begin
  if (i>0) then Result:=i else Result:=0;
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src:=node^.ParamNode(0)^.AsReg;
 src:=RegDown(src);
 if (dst=nil) or (src=nil) then Exit;

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
      else;
     end;

   Op.OpConvertFToU:
     case src^.dtype of
      dtFloat32:_SetConst(dst^.dtype,minz(Trunc(pc^.AsFloat32)));
      else;
     end;

   Op.OpConvertFToS:
     case src^.dtype of
      dtFloat32:_SetConst(dst^.dtype,Trunc(pc^.AsFloat32));
      else;
     end;

   Op.OpConvertSToF:
     case src^.dtype of
      dtInt32 :_SetConst(dst^.dtype,double(pc^.AsInt));
      dtUint32:_SetConst(dst^.dtype,double(pc^.AsInt));

      dtInt64 :_SetConst(dst^.dtype,double(pc^.AsInt64));
      dtUint64:_SetConst(dst^.dtype,double(pc^.AsInt64));
      else;
     end;

   Op.OpConvertUToF:
     case src^.dtype of
      dtInt32 :_SetConst(dst^.dtype,double(pc^.AsUint));
      dtUint32:_SetConst(dst^.dtype,double(pc^.AsUint));

      dtInt64 :_SetConst(dst^.dtype,double(pc^.AsUint64));
      dtUint64:_SetConst(dst^.dtype,double(pc^.AsUint64));
      else;
     end;

  end;
 end;
end;

function TEmitPostOp.OnCompositeExtract1(node:PSpirvOp):Integer;
var
 pc:PsrConst;
 dst,src:PsrRegNode;
 pos:DWORD;
begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src:=node^.ParamNode(0)^.AsReg;
 src:=RegDown(src);
 if (dst=nil) or (src=nil) then Exit;
 pos:=0;
 if not node^.ParamNode(1)^.TryGetValue(pos) then Exit;

 if src^.is_const then
 begin
  pc:=src^.AsConst;
  if (pos<pc^.key.count) then
  begin
   pc:=pc^.GetCompItem(pos);
   pc^.mark_read;
   dst^.SetConst(pc);

   node^.OpId:=OpLinks; //mark remove
   node^.dst:=Default(TOpParamSingle);
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
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(1)^.AsReg;
 src[1]:=node^.ParamNode(2)^.AsReg;
 if (dst=nil) or (src[0]=nil) or (src[1]=nil) then Exit;

 if (node^.dst_type=nil) then
 begin
  dtype:=LazyType3(dst^.dtype,src[0]^.dtype,src[1]^.dtype);
  node^.dst_type:=FSpirvTypes.Fetch(dtype);
  Result:=Result+PrepTypeDst(node^.dst.pData,dtype);
  dst:=node^.dst.pData;
  Inc(Result);
 end;

 Result:=Result+PrepTypeParam(node^.ParamNode(1),dst^.dtype);
 Result:=Result+PrepTypeParam(node^.ParamNode(2),dst^.dtype);
end;

function TEmitPostOp.OnIAdd1(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 src:array[0..1] of PsrRegNode;
 data:array[0..1] of QWORD;

 procedure _SetConst(dtype:TsrDataType;value:QWORD);
 begin
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

 procedure _SetReg(src:PsrRegNode);
 begin
  src^.mark_read;
  dst^.pWriter.SetParam(ntReg,src);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
  Result:=_OnShr_ext1(node,src[0]^.AsOp,src[1]^.AsConst);
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
     src[0]:=pOp^.ParamNode(0)^.AsReg;
     src[1]:=pOp^.ParamNode(1)^.AsReg;

     src[0]:=RegDown(src[0]);
     src[1]:=RegDown(src[1]);

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
     src[0]:=pOp^.ParamNode(0)^.AsReg;
     src[1]:=pOp^.ParamNode(1)^.AsReg;

     src[0]:=RegDown(src[0]);
     src[1]:=RegDown(src[1]);

     if (src[0]=nil) or (src[1]=nil) then Exit;

     Result:=_OnShr_ext_add(node,src[0]^.AsOp,src[1]^.AsOp,pShrVal);
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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 data[0]:=pShrVal^.GetData;
 data[1]:=pAndVal^.GetData;

 data[0]:=High(QWORD) shl data[0];

 if (data[0] and data[1]=0) then
 begin
  dst:=node^.dst.AsReg;
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
     src[0]:=pOp^.ParamNode(0)^.AsReg;
     src[1]:=pOp^.ParamNode(1)^.AsReg;

     src[0]:=RegDown(src[0]);
     src[1]:=RegDown(src[1]);

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
     src[0]:=pOp^.ParamNode(0)^.AsReg;
     src[1]:=pOp^.ParamNode(1)^.AsReg;

     src[0]:=RegDown(src[0]);
     src[1]:=RegDown(src[1]);

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
     src[0]:=pOp^.ParamNode(0)^.AsReg;
     src[1]:=pOp^.ParamNode(1)^.AsReg;

     src[0]:=RegDown(src[0]);
     src[1]:=RegDown(src[1]);

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
     src[0]:=pOp^.ParamNode(0)^.AsReg;
     src[1]:=pOp^.ParamNode(1)^.AsReg;

     src[0]:=RegDown(src[0]);
     src[1]:=RegDown(src[1]);

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
  src^.mark_read;
  dst^.pWriter.SetParam(ntReg,src);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
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
  dst:=pOp0^.dst.AsReg;
  dst_shr[0]:=NewReg(dst^.dtype);
  src:=_GetShrOptReg(pOp0);
  src^.mark_read;
  _emit_OpShl(pOp0,dst_shr[0],src,FetchReg(FConsts.Fetch(dst^.dtype,data[0])));
 end;

 if (data[1]=0) then
 begin
  dst_shr[1]:=_GetShrOptReg(pOp1);
 end else
 begin
  dst:=pOp1^.dst.AsReg;
  dst_shr[1]:=NewReg(dst^.dtype);
  src:=_GetShrOptReg(pOp1);
  src^.mark_read;
  _emit_OpShl(pOp1,dst_shr[1],src,FetchReg(FConsts.Fetch(dst^.dtype,data[1])));
 end;

 dst:=node^.dst.AsReg;
 pOp0:=dst^.AsOp; //OpIAdd
 src:=NewReg(dst^.dtype);

 dst_shr[0]^.mark_read;
 dst_shr[1]^.mark_read;
 _emit_OpIAdd(pOp0,src,dst_shr[0],dst_shr[1]);

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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src[0]:=node^.ParamNode(0)^.AsReg;
 src[1]:=node^.ParamNode(1)^.AsReg;

 src[0]:=RegDown(src[0]);
 src[1]:=RegDown(src[1]);

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
 end else
 begin
  src[0]:=node^.ParamNode(0)^.AsReg; //get original
  src[1]:=node^.ParamNode(1)^.AsReg; //get original

  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);

  rmax:=NewReg(dst^.dtype);
  src[0]^.mark_read;
  src[1]^.mark_read;
  node:=_emit_OpUMax(node,rmax,src[0],src[1]);

  rmin:=NewReg(dst^.dtype);
  src[0]^.mark_read;
  src[1]^.mark_read;
  node:=_emit_OpUMin(node,rmin,src[0],src[1]);

  rmax^.mark_read;
  rmin^.mark_read;
  node:=_emit_OpISub(node,dst,rmax,rmin);
 end;
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
  dst^.SetConst(FConsts.Fetch(dtype,value));
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 src:=node^.ParamNode(0)^.AsReg;
 src:=RegDown(src);
 if (dst=nil) or (src=nil) then Exit;

 if src^.is_const then
 begin
  //need a const calc
  data:=src^.AsConst^.GetData;
  data:=F_WQM_32(data);
  _SetConst(dst^.dtype,data);
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

 rtype:TsrDataType;
 count:DWORD;

 ret:PsrConst;
 vec:array[0..2] of PsrConst;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 if (dst=nil) then Exit;

 count:=0;
 if not node^.ParamNode(0)^.TryGetValue(count) then Exit;
 rtype:=TsrDataType(count);

 count:=0;
 if not node^.ParamNode(1)^.TryGetValue(count) then Exit;

 src:=node^.ParamNode(2)^.AsReg;
 src:=RegDown(src);
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
      ret:=FConsts.Fetchi(dtInt32,int6(P^.x));
     end;
   2:
     begin
      vec[0]:=FConsts.Fetchi(dtInt32,int6(P^.x));
      vec[1]:=FConsts.Fetchi(dtInt32,int6(P^.y));

      ret:=FConsts.Fetch_vec(dtVec2i,2,@vec);
     end;
   3:
     begin
      vec[0]:=FConsts.Fetchi(dtInt32,int6(P^.x));
      vec[1]:=FConsts.Fetchi(dtInt32,int6(P^.y));
      vec[2]:=FConsts.Fetchi(dtInt32,int6(P^.z));

      ret:=FConsts.Fetch_vec(dtVec3i,3,@vec);
     end;
   else
    Assert(False);
  end;

  dst^.SetConst(ret);
  node^.OpId:=OpLinks; //mark remove
  node^.dst:=Default(TOpParamSingle);
  Inc(Result);
 end else
 begin
  Assert(false,'TODO');
 end;
end;

function _IsFma(node:PSpirvOp):Boolean;
var
 OpId:DWORD;
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
 pCon:PsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pCon:=pReg^.AsConst;
 if (pCon=nil) then Exit;
 if (pCon^.key.dtype<>dtFloat32) then Exit;
 Result:=(Round(PSingle(@pCon^.Data)^*10)=10);
end;

function _IsConstFloat_1_5(pReg:PsrRegNode):Boolean;
var
 pCon:PsrConst;
begin
 Result:=False;
 if (pReg=nil) then Exit;
 pCon:=pReg^.AsConst;
 if (pCon=nil) then Exit;
 if (pCon^.key.dtype<>dtFloat32) then Exit;
 Result:=(Round(PSingle(@pCon^.Data)^*10)=15);
end;

function _Fetch_FAbs_Value(node:PSpirvOp):PSpirvOp;
var
 OpId:DWORD;
 pReg:PsrRegNode;
begin
 Result:=nil;
 if (node=nil) then Exit;
 if (node^.OpId<>Op.OpExtInst) then Exit;
 OpId:=0;
 node^.ParamNode(1)^.TryGetValue(OpId);
 if (OpId<>GlslOp.FAbs) then Exit;
 pReg:=node^.ParamNode(2)^.AsReg;
 if (pReg=nil) then Exit;
 Result:=pReg^.AsOp;
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

function TEmitPostOp.OnMakeCM1(node:PSpirvOp):Integer;
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
 i,count:DWORD;

begin
 Result:=0;
 dst:=node^.dst.AsReg;
 if (dst=nil) then Exit;

 count:=0;
 if not node^.ParamNode(0)^.TryGetValue(count) then Exit;
 rtype:=TsrDataType(count);

 if GetVecChild(rtype)<>dtFloat32 then Assert(false,'TODO');

 count:=0;
 if not node^.ParamNode(1)^.TryGetValue(count) then Exit;
 if (count=0) then Exit;

 if (count=4) then Assert(false,'TODO');

 m_x:=RegDown(node^.ParamNode(2)^.AsReg)^.AsOp; //param1
 m_y:=RegDown(node^.ParamNode(3)^.AsReg)^.AsOp; //param2
 m_f:=RegDown(node^.ParamNode(4)^.AsReg)^.AsOp; //param3

 if not _IsFma(m_x) then Exit;
 if not _IsFma(m_y) then Exit;
 if not _IsOp(m_f,OpCUBEID) then Exit;

 m_CUBE_ID:=m_f;

 //m_x
 pReg:=RegDown(m_x^.ParamNode(2)^.AsReg); //param1
 pOp:=pReg^.AsOp;
 if not _IsOp(pOp,OpCUBESC) then Exit;
 m_CUBE_SC:=pOp;

 pReg:=RegDown(m_x^.ParamNode(3)^.AsReg); //param2
 pOp:=pReg^.AsOp;
 if not _IsOp(pOp,Op.OpFDiv) then Exit;

   pReg:=RegDown(pOp^.ParamNode(0)^.AsReg);  //div
   if not _IsConstFloat_1_0(pReg) then Exit; //1.0

   pReg:=RegDown(pOp^.ParamNode(1)^.AsReg);
   pOp:=_Fetch_FAbs_Value(pReg^.AsOp);
   if not _IsOp(pOp,OpCUBEMA) then Exit;
   m_x_CUBE_MA:=pOp;

 pReg:=RegDown(m_x^.ParamNode(4)^.AsReg);  //param3
 if not _IsConstFloat_1_5(pReg) then Exit; //1.5

 //m_y
 pReg:=RegDown(m_y^.ParamNode(2)^.AsReg); //param1
 pOp:=pReg^.AsOp;
 if not _IsOp(pOp,OpCUBETC) then Exit;
 m_CUBE_TC:=pOp;

 pReg:=RegDown(m_x^.ParamNode(3)^.AsReg); //param2
 pOp:=pReg^.AsOp;
 if not _IsOp(pOp,Op.OpFDiv) then Exit;

   pReg:=RegDown(pOp^.ParamNode(0)^.AsReg);  //div
   if not _IsConstFloat_1_0(pReg) then Exit; //1.0

   pReg:=RegDown(pOp^.ParamNode(1)^.AsReg);
   pOp:=_Fetch_FAbs_Value(pReg^.AsOp);
   if not _IsOp(pOp,OpCUBEMA) then Exit;
   m_y_CUBE_MA:=pOp;

 pReg:=RegDown(m_y^.ParamNode(4)^.AsReg);  //param3
 if not _IsConstFloat_1_5(pReg) then Exit; //1.5

 //

 if not _cmp_src_cube_op3(m_CUBE_SC,m_CUBE_TC) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_CUBE_ID) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_x_CUBE_MA) then Exit;
 if not _cmp_src_cube_op3(m_CUBE_SC,m_y_CUBE_MA) then Exit;

 For i:=0 to 2 do
 begin
  src[i]:=RegDown(m_CUBE_SC^.ParamNode(i)^.AsReg);
 end;

 MakeVecComp(node,dtVec3f,dst,3,@src);

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);
 Result:=1;

 {
 For i:=0 to count-1 do
 begin
  pOp[i]:=src[i]^.AsOp;

  Case pOp[i]^.OpId of
   OpCUBEID:Writeln('OpCUBEID');
   OpCUBESC:Writeln('OpCUBESC');
   OpCUBETC:Writeln('OpCUBETC');
   OpCUBEMA:Writeln('OpCUBEMA');
   else
            Writeln(Op.GetStr(pOp[i]^.OpId));
  end;

 end;
 }

end;

//

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
 pos:DWORD;
 pLine:PspirvOp;
 pReg,tmp:PsrRegNode;
begin
 pReg:=nil;
 Result:=True;
 For i:=0 to count-1 do
 begin
  pLine:=src[i]^.AsOp;
  if (pLine=nil) then Exit(false);
  if (pLine^.OpId<>Op.OpCompositeExtract) then Exit(false);

  pos:=0;
  if not pLine^.ParamNode(1)^.TryGetValue(pos) then Exit;
  if (pos<>i) then Exit(false);

  tmp:=pLine^.ParamNode(0)^.AsReg;
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
 pos:DWORD;
 pReg:PsrRegNode;
begin
 Result:=0;
 pLine:=src^.AsOp;
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>Op.OpCompositeExtract) then Exit;

 pos:=0;
 if not pLine^.ParamNode(1)^.TryGetValue(pos) then Exit;

 pReg:=pLine^.ParamNode(0)^.AsReg;
 if (pReg=nil) then Exit;

 pLine:=pReg^.AsOp;
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>Op.OpCompositeConstruct) then Exit;

 pReg:=pLine^.ParamNode(pos)^.AsReg;
 if (pReg=nil) then Exit;
 src:=pReg;
 Result:=1;
end;

procedure TEmitPostOp.MakeVecConst(rtype:TsrDataType;dst:PsrRegNode;count:Byte;src:PPsrRegNode);
var
 nodes:array[0..3] of PsrConst;
 h:PsrConst;
 i:Byte;
begin
 For i:=0 to count-1 do
 begin
  nodes[i]:=src[i]^.AsConst;
 end;

 h:=FConsts.Fetch_vec(rtype,count,@nodes);
 dst^.pWriter.SetParam(ntConst,h);
end;

procedure TEmitPostOp.MakeVecOne(dst:PsrRegNode;src:PPsrRegNode);
var
 pLine:PspirvOp;
 rsrc:PsrRegNode;
begin
 pLine:=src[0]^.AsOp;
 rsrc:=pLine^.ParamNode(0)^.AsReg;
 rsrc^.mark_read;
 dst^.pWriter.SetParam(ntReg,rsrc);
end;

function TEmitPostOp.MakeVecComp(pLine:PSpirvOp;rtype:TsrDataType;dst:PsrRegNode;count:Byte;src:PPsrRegNode):PSpirvOp;
var
 r:Integer;
 i:Byte;
begin
 Result:=pLine;

 repeat
  r:=0;
  For i:=0 to count-1 do
  begin
   r:=r+try_get_comp_bridge(src[i]);
  end;

  if is_all_const(src,count) then
  begin
   MakeVecConst(rtype,dst,count,src);
   Exit; //
  end;

  if is_all_in_one_comp(src,rtype,count) then
  begin
   MakeVecOne(dst,src);
   Exit; //
  end;

 until (r=0);

 For i:=0 to count-1 do
 begin
  src[i]^.mark_read; //strict type later
 end;
 Result:=emit_OpMakeConstruct(pLine,dst,count,src);
end;

function TEmitPostOp.OnMakeVec2(node:PSpirvOp):Integer;
var
 pParam:POpParamNode;
 dst:PsrRegNode;
 src:array[0..3] of PsrRegNode;
 rtype:TsrDataType;
 i,count:DWORD;
begin
 Result:=1;

 dst:=node^.dst.AsReg;
 if (dst=nil) then Exit;

 pParam:=node^.pParam.pHead;

 count:=0;
 if not pParam^.TryGetValue(count) then Exit;
 rtype:=TsrDataType(count);

 pParam:=pParam^.pNext;
 count:=0;
 if not pParam^.TryGetValue(count) then Exit;

 For i:=0 to count-1 do
 begin
  pParam:=pParam^.pNext;
  src[i]:=pParam^.AsReg;
  if (src[i]=nil) then Assert(false,'OnMakeVec2');
 end;

 MakeVecComp(node,rtype,dst,count,@src);

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);
end;

function TEmitPostOp.OnReturn_2(node:PSpirvOp):Integer;
begin
 Result:=0;

 if is_term_op(node^.pPrev) then
 begin
  node^.OpId:=OpLinks; //mark remove
  Inc(Result);
 end;

end;

function TEmitPostOp.OnMakeExp2(node:PSpirvOp):Integer;
var
 pLine:PspirvOp;
 pOpBlock:PsrOpBlock;
 pChild:PsrOpBlock;
 pOpLabel:array[0..2] of PspirvOp;
 exc,tmp:PsrRegNode;
 b_adr:TSrcAdr;
begin
 Result:=1;

 pOpBlock:=node^.pParent;

 exc:=node^.ParamNode(0)^.AsReg;
 if (exc=nil) then Exit;

 tmp:=RegDown(exc);
 if tmp^.is_const then
 begin
  node^.OpId:=OpLinks;
  //node^.OpId:=Op.OpNop;
  //NodeOpClear(node);    //clear

  Case tmp^.AsConst^.AsBool of
   True :  //is always store
     begin
      pLine:=node^.pNext;
      if (pLine<>nil) then
      begin
       pChild:=pLine^.dst.AsBlock;
       if (pChild<>nil) then
       begin
        Dec(pChild^.FLevel); //up
       end;
      end;
     end;
   False:  //is always kill
     begin
      pOpBlock^.dummy.OpId:=Op.OpKill; //add kill
      //clear all
      node:=pOpBlock^.pHead;
      While (node<>nil) do
      begin
       Case node^.OpId of
        Op.OpNop:;
        OpLinks:;
        Op.OpKill:;
        OpBlock:
          begin
           pChild:=node^.dst.AsBlock;
           node:=pChild^.pHead;
           Continue;
          end;
        else
         node^.OpId:=OpLinks; //mark remove
       end;
       node:=node^.pNext;
      end;
     end;
  end;
  Exit;
 end else
 begin
  node^.OpId:=OpLinks; //mark remove

  exc^.mark_read;
  b_adr:=pOpBlock^.Block.b_adr;

  pLine:=node^.pNext;
  if (pLine=nil) then //kill or nop
  begin

   pOpLabel[0]:=NewLabelOp; //current
   pOpLabel[1]:=NewLabelOp; //end

   pOpLabel[0]^.Adr:=b_adr;
   pOpLabel[1]^.Adr:=b_adr;

   pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[1],nil);
   pOpBlock^.Block.bType:=btCond;
   pOpBlock^.SetCond(nil,true);

   pLine:=node;
   pLine:=emit_OpCondMerge(pLine,pOpLabel[1]);
   pLine:=emit_OpBranchCond(pLine,pOpLabel[1],pOpLabel[0],exc); //reverse
   pLine:=AddSpirvOp(pLine,pOpLabel[0]);

     pChild:=AllocBlockOp; //create new
     pChild^.SetInfo(btOther,b_adr,b_adr);
     pChild^.dummy.OpId:=Op.OpKill;
   pLine:=InsertBlockOp(pLine,pChild);

   //emit_OpBranch not need from kill
   pLine:=emit_OpBranch(pLine,pOpLabel[1]);
   pLine:=AddSpirvOp(pLine,pOpLabel[1]);

  end else
  begin //kill or store
   Assert(pLine^.OpId=OpBlock);

   pOpLabel[0]:=NewLabelOp; //current
   pOpLabel[1]:=NewLabelOp; //else
   pOpLabel[2]:=NewLabelOp; //end

   pOpLabel[0]^.Adr:=b_adr;
   pOpLabel[1]^.Adr:=b_adr;
   pOpLabel[2]^.Adr:=b_adr;

   pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[2],pOpLabel[1]);
   pOpBlock^.Block.bType:=btCond;
   pOpBlock^.SetCond(nil,true);

   pLine:=node;
   pLine:=emit_OpCondMerge(pLine,pOpLabel[2]);
   pLine:=emit_OpBranchCond(pLine,pOpLabel[1],pOpLabel[0],exc); //reverse
   pLine:=AddSpirvOp(pLine,pOpLabel[0]);

     pChild:=AllocBlockOp; //create new
     pChild^.SetInfo(btOther,b_adr,b_adr);
     pChild^.dummy.OpId:=Op.OpKill;
   pLine:=InsertBlockOp(pLine,pChild);

   //emit_OpBranch not need from kill
   pLine:=AddSpirvOp(pLine,pOpLabel[1]);

   //OpStore child

   pLine:=pOpBlock^.pTail;
   pLine:=emit_OpBranch(pLine,pOpLabel[2]);
   pLine:=AddSpirvOp(pLine,pOpLabel[2]); //end

  end;

 end;

end;

function TEmitPostOp.OnIAddExt2(node:PSpirvOp):Integer;
var
 dst,car:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 Result:=1;
 dst:=node^.dst.AsReg;
 car:=node^.ParamNode(0)^.AsReg;
 if (dst=nil) or (car=nil) then Exit;

 src[0]:=node^.ParamNode(1)^.AsReg;
 src[1]:=node^.ParamNode(2)^.AsReg;
 if (src[0]=nil) or (src[1]=nil) then Exit;

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);

 if (car^.read_count>1) then //carry is use
 begin
  src[0]^.mark_read; //strict type later
  src[1]^.mark_read; //strict type later
  _emit_OpIAddC(node,dst,car,src[0],src[1]);
 end else
 begin
  src[0]^.mark_read;
  src[1]^.mark_read;
  _emit_OpIAdd(node,dst,src[0],src[1]);
 end;
end;

function TEmitPostOp.OnISubExt2(node:PSpirvOp):Integer;
var
 dst,bor:PsrRegNode;
 src:array[0..1] of PsrRegNode;
begin
 Result:=1;
 dst:=node^.dst.AsReg;
 bor:=node^.ParamNode(0)^.AsReg;
 if (dst=nil) or (bor=nil) then Exit;

 src[0]:=node^.ParamNode(1)^.AsReg;
 src[1]:=node^.ParamNode(2)^.AsReg;
 if (src[0]=nil) or (src[1]=nil) then Exit;

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);

 if (bor^.read_count>1) then //borrow is use
 begin
  src[0]^.mark_read; //strict type later
  src[1]^.mark_read; //strict type later
  _emit_OpISubB(node,dst,bor,src[0],src[1]);
 end else
 begin
  src[0]^.mark_read;
  src[1]^.mark_read;
  _emit_OpISub(node,dst,src[0],src[1]);
 end;
end;

{
If (isNan(S0.f) || isNan(S1.f) || isNan(S2.f))
  D.f = MIN3(S0.f, S1.f, S2.f)
Else if (MAX3(S0.f,S1.f,S2.f) == S0.f)
  D.f = MAX(S1.f, S2.f)
Else if (MAX3(S0.f,S1.f,S2.f) == S1.f)
  D.f = MAX(S0.f, S2.f)
Else
  D.f = MAX(S0.f, S1.f)
}

{
float MED3F(vec3 s) {

 float s0=s.x;
 float s1=s.y;
 float s2=s.z;

 if (any(isnan(s))) {
  return min(s0,min(s1,s2));
 }

 return max(min(s0,s1), min(max(s0,s1), s2));
}
}

//%MED3F_vf3_ = OpFunction %float None %9
//         %s = OpFunctionParameter %_ptr_Function_v3float
//        %12 = OpLabel

//        %17 = OpAccessChain %_ptr_Function_float %s %uint_0
//        %21 = OpAccessChain %_ptr_Function_float %s %uint_1
//        %25 = OpAccessChain %_ptr_Function_float %s %uint_2

//        %s0 = OpLoad %float %17
//        %s1 = OpLoad %float %21
//        %s2 = OpLoad %float %25

//        %27 = OpLoad %v3float %s
//        %30 = OpIsNan %v3bool %27
//        %31 = OpAny %bool %30

//              OpSelectionMerge %33 None
//              OpBranchConditional %31 %32 %33
//        %32 = OpLabel
//                %37 = OpExtInst %float %1 FMin %s1 %s2
//                %38 = OpExtInst %float %1 FMin %s0 %37
//                      OpReturnValue %38
//        %33 = OpLabel

//        %min = OpExtInst %float %1 FMin %s0 %s1
//        %max = OpExtInst %float %1 FMax %s0 %s1
//        %tmp = OpExtInst %float %1 FMin %max %s2
//        %ret = OpExtInst %float %1 FMax %min %tmp
//              OpReturnValue %ret
//              OpFunctionEnd

{
function TEmitPostOp.NewMED3F:PSpirvFunc;
var
 pFunc:PSpirvFunc;

 node:PSpirvOp;

 tvec3f:PsrType;
 pvec3f:PsrType;
 tfloat:PsrType;
 tftype:PsrType;

 vparam:TOpParamSingle;

 rparam:PsrRegNode;
 visnan:PsrRegNode;
 bisnan:PsrRegNode;
 max:PsrRegNode;
 min:PsrRegNode;
 tmp:PsrRegNode;
 ret:PsrRegNode;

 src:array[0..2] of PsrRegNode;

 pOpLabel:array[0..1] of PspirvOp;

begin
 pFunc:=FSpirvFuncs.Search('MED3F');
 if (pFunc<>nil) then
 begin
  pFunc^.mark_read;
  Exit(pFunc);
 end;

 pFunc:=Alloc(SizeOf(TSpirvFunc));
 pFunc^.Init('MED3F',@Alloc);
 pFunc^.mark_read;

 tvec3f:=FSpirvTypes.Fetch(dtVec3f);
 pvec3f:=FSpirvTypes.FetchPointer(tvec3f,StorageClass.Function_);
 tfloat:=FSpirvTypes.Fetch(dtFloat32);
 tftype:=FSpirvTypes.FetchFunction1(tfloat,pvec3f);

 //OpFunction
 node:=pFunc^.AddSpirvOp(Op.OpFunction);
 node^.dst_type:=FSpirvTypes.Fetch(dtFloat32);
 node^.dst.SetParam(ntFunc,pFunc); //self
 node^.AddLiteral(FunctionControl.None,'None');
 node^.AddParam(ntType,tftype);

 vparam:=Default(TOpParamSingle);
 vparam.SetParam(ntRefId,NewRefId);
 pvec3f^.mark_read;
 //OpFunctionParameter
 node:=pFunc^.AddSpirvOp(Op.OpFunctionParameter);
 node^.dst_type:=pvec3f;
 node^.dst:=vparam;

 //OpLabel
 node:=NewLabelOp;
 node^.Adr:=Default(TSrcAdr);
 pFunc^.AddSpirvOp(node);

 rparam:=NewReg(dtVec3f); //load
 tvec3f^.mark_read;
 emit_OpLoad(pFunc^.line,tvec3f,rparam,vparam)^.Adr:=Default(TSrcAdr);

 src[0]:=NewReg(dtFloat32);
 src[1]:=NewReg(dtFloat32);
 src[2]:=NewReg(dtFloat32);

 rparam^.mark_read;
 emit_OpCompExtract(pFunc^.line,src[0],rparam,0)^.Adr:=Default(TSrcAdr);

 rparam^.mark_read;
 emit_OpCompExtract(pFunc^.line,src[1],rparam,1)^.Adr:=Default(TSrcAdr);

 rparam^.mark_read;
 emit_OpCompExtract(pFunc^.line,src[2],rparam,2)^.Adr:=Default(TSrcAdr);

 //
 visnan:=NewReg(dtVec3b); //OpIsNan
 rparam^.mark_read;
 _emit_OpIsNan(pFunc^.line,visnan,rparam)^.Adr:=Default(TSrcAdr);

 bisnan:=NewReg(dtBool); //OpAny
 visnan^.mark_read;
 _emit_Op1(pFunc^.line,Op.OpAny,bisnan,visnan)^.Adr:=Default(TSrcAdr);

 //
 pOpLabel[0]:=NewLabelOp;
 pOpLabel[1]:=NewLabelOp;
 pOpLabel[0]^.Adr:=Default(TSrcAdr);
 pOpLabel[1]^.Adr:=Default(TSrcAdr);
 emit_OpCondMerge(pFunc^.line,pOpLabel[1])^.Adr:=Default(TSrcAdr); //merge
 bisnan^.mark_read;
 emit_OpBranchCond(pFunc^.line,pOpLabel[0],pOpLabel[1],bisnan)^.Adr:=Default(TSrcAdr); //branch
 pFunc^.AddSpirvOp(pOpLabel[0]); //OpLabel

 tmp:=NewReg(dtFloat32);
 src[0]^.mark_read;
 src[1]^.mark_read;
 _emit_OpFMin(pFunc^.line,tmp,src[0],src[1])^.Adr:=Default(TSrcAdr); //min(s0,s1)

 ret:=NewReg(dtFloat32);
 tmp^.mark_read;
 src[2]^.mark_read;
 _emit_OpFMin(pFunc^.line,ret,tmp,src[2])^.Adr:=Default(TSrcAdr); //min(min(s0,s1),s2)

 ret^.mark_read;
 emit_OpReturnValue(pFunc^.line,ret)^.Adr:=Default(TSrcAdr); //ret

 pFunc^.AddSpirvOp(pOpLabel[1]); //end branch

 //
 min:=NewReg(dtFloat32);
 src[0]^.mark_read;
 src[1]^.mark_read;
 _emit_OpFMin(pFunc^.line,min,src[0],src[1])^.Adr:=Default(TSrcAdr); //min(s0,s1)

 max:=NewReg(dtFloat32);
 src[0]^.mark_read;
 src[1]^.mark_read;
 _emit_OpFMax(pFunc^.line,max,src[0],src[1])^.Adr:=Default(TSrcAdr); //max(s0,s1)

 tmp:=NewReg(dtFloat32);
 max^.mark_read;
 src[2]^.mark_read;
 _emit_OpFMin(pFunc^.line,tmp,max,src[2])^.Adr:=Default(TSrcAdr); //min(max(s0,s1),s2)

 ret:=NewReg(dtFloat32);
 min^.mark_read;
 tmp^.mark_read;
 _emit_OpFMax(pFunc^.line,ret,min,tmp)^.Adr:=Default(TSrcAdr); //max(min(s0,s1),min(max(s0,s1),s2))

 ret^.mark_read;
 emit_OpReturnValue(pFunc^.line,ret)^.Adr:=Default(TSrcAdr); //ret

 pFunc^.AddSpirvOp(Op.OpFunctionEnd); //end

 FSpirvFuncs.Insert(pFunc);

 pFunc^.mark_read;
 Result:=pFunc;
end;
}

//%114 = OpFunctionCall %float %MED3F_vf3_ %param

{
function TEmitPostOp.OpMED3F__2(node:PSpirvOp):Integer;
var
 dst:PsrRegNode;
 vec:PsrRegNode;
 src:array[0..2] of PsrRegNode;
 pFunc:PSpirvFunc;
begin
 Result:=1;

 dst:=node^.dst.pData;
 src[0]:=node^.ParamNode(0)^.pData;
 src[1]:=node^.ParamNode(1)^.pData;
 src[2]:=node^.ParamNode(2)^.pData;

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);

 //build vec
 vec:=NewReg(dtVec3f);
 node:=MakeVecComp(node,dtVec3f,vec,3,@src);

 pFunc:=NewMED3F;

 vec^.mark_read;
 //OpFunctionCall
 node:=AddSpirvOp(node,Op.OpFunctionCall);
 node^.dst_type:=FSpirvTypes.Fetch(dtFloat32);
 node^.dst.SetParam(ntReg,dst); //dst
 node^.AddParam(ntFunc,pFunc);  //func id
 node^.AddParam(ntReg,vec);     //vec3f

end;
}

//

end.

