unit srOpUtils;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
 spirv,
 srNodes,
 srTypes,
 srReg,
 srConst,
 srLayout,
 srVariable,
 srOp;

const
 OpLinks  =DWORD(-1);
 OpBlock  =DWORD(-2);
 OpMakeExp=DWORD(-3);
 OpMakeVec=DWORD(-4);
 OpIAddExt=DWORD(-5);
 OpISubExt=DWORD(-6);
 OpAbsDiff=DWORD(-7);
 OpWQM32  =DWORD(-8);
 OpPackOfs=DWORD(-9);
 //OpMED3F

 OpCUBEID =DWORD(-10);
 OpCUBESC =DWORD(-11);
 OpCUBETC =DWORD(-12);
 OpCUBEMA =DWORD(-13);

 OpMakeCM =DWORD(-14);

type
 TOpParamNodeHelper = record helper for TOpParamNode
  function AsParam:TOpParamSingle;
  function AsReg:Pointer;
  function AsVar:Pointer;
  function AsChain:Pointer;
  function TryGetValue(var V:DWORD):Boolean;
 end;

function  InsSpirvOp(pLine,pNew:PspirvOp):PSpirvOp;
Procedure MarkUnreadParamSingle(const node:TOpParamSingle);
Function  get_inverse_cmp_op(OpId:DWORD):DWORD;
Function  is_term_op(OpId:DWORD):Boolean;
Function  is_merge_op(OpId:DWORD):Boolean;
Function  is_term_op(pLine:PspirvOp):Boolean;
procedure _up_merge_line(var pLine:PspirvOp);
function  FindUpSameOp(pLine,node:PspirvOp):PspirvOp;

function  GreaterThanLine(p1,p2:PspirvOp):Boolean; //(p1>p2)
function  GetMaxPlace(pLine:PSpirvOp;count:Byte;src:PPsrRegNode):PSpirvOp;

implementation

function TOpParamNodeHelper.AsParam:TOpParamSingle;
begin
 Result:=Default(TOpParamSingle);
 if (@Self=nil) then Exit;
 Result.ntype:=ntype;
 Result.pData:=pData;
end;

function TOpParamNodeHelper.AsReg:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntReg) then Exit;
 Result:=pData;
end;

function TOpParamNodeHelper.AsVar:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntVar) then Exit;
 Result:=pData;
end;

function TOpParamNodeHelper.AsChain:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (ntype<>ntChain) then Exit;
 Result:=pData;
end;

function TOpParamNodeHelper.TryGetValue(var V:DWORD):Boolean;
begin
 Result:=False;
 if (@Self=nil) then Exit;
 if (ntype<>ntLiteral) then Exit;
 V:=Value;
 Result:=True;
end;

//--

function InsSpirvOp(pLine,pNew:PspirvOp):PSpirvOp;
begin
 Result:=nil;
 Assert(pLine<>nil);
 if (pLine=nil) or (pNew=nil) then Exit;

 if (pLine^.pNext<>nil) then
 begin
  pNew^.adr:=pLine^.adr;
 end;

 pLine^.InsertAfter(pNew);
 Result:=pNew;
end;

Procedure MarkUnreadParamSingle(const node:TOpParamSingle);
begin
 Case node.ntype of
  ntLiteral:;
  ntConst:PsrConst   (node.pData)^.mark_unread;
  ntType :PsrType    (node.pData)^.mark_unread;
  ntFunc :PSpirvFunc (node.pData)^.mark_unread;
  ntVar  :PsrVariable(node.pData)^.mark_unread;
  ntRefId:;
  ntReg  :PsrRegNode (node.pData)^.mark_unread;
  //ntReg  :RegUnmark(node.pData);
  ntChain:PsrChain  (node.pData)^.mark_unread;
  else
    Assert(false);
 end;
end;

function GreaterThanLine(p1,p2:PspirvOp):Boolean; //(p1>p2)
begin
 Result:=False;
 p2:=p2^.pNext;
 While (p2<>nil) do
 begin
  if (p1=p2) then Exit(True);
  p2:=p2^.pNext;
 end;
end;

Function ParentOf(pLine,pCurr:PsrOpBlock):Boolean;
begin
 Result:=False;
 While (pLine<>nil) do
 begin
  if (pLine=pCurr) then Exit(True);
  pLine:=pLine^.pParent;
 end;
end;

function GetMaxPlace(pLine:PSpirvOp;count:Byte;src:PPsrRegNode):PSpirvOp;
var
 m,t:PSpirvOp;
 i:Byte;
begin
 Result:=pLine;
 if (count=0) or (src=nil) then Exit;
 m:=nil;
 For i:=0 to count-1 do
 begin
  t:=src[i]^.pLine;
  if not src[i]^.is_const then
  if ParentOf(pLine^.pParent,t^.pParent) then
  begin
   if (m=nil) then
   begin
    m:=t;
   end else
   if (m^.pParent=t^.pParent) then
   begin
    if GreaterThanLine(t,m) then
    begin
     m:=t;
    end;
   end else
   if (t^.pParent^.FLevel>m^.pParent^.FLevel) then
   begin
    m:=t;
   end;
  end;
 end;
 if (m<>nil) then
 begin
  Result:=m;
 end;
end;

Function get_inverse_cmp_op(OpId:DWORD):DWORD;
begin
 Result:=0;
 Case OpId of
  Op.OpFOrdLessThan          :Result:=Op.OpFOrdGreaterThan       ;
  Op.OpFOrdEqual             :Result:=Op.OpFOrdEqual             ;
  Op.OpFOrdLessThanEqual     :Result:=Op.OpFOrdGreaterThanEqual  ;
  Op.OpFOrdGreaterThan       :Result:=Op.OpFOrdLessThan          ;
  Op.OpFOrdNotEqual          :Result:=Op.OpFOrdNotEqual          ;
  Op.OpFOrdGreaterThanEqual  :Result:=Op.OpFOrdLessThanEqual     ;
  Op.OpOrdered               :Result:=Op.OpOrdered               ;
  Op.OpUnordered             :Result:=Op.OpUnordered             ;
  Op.OpFUnordLessThan        :Result:=Op.OpFUnordGreaterThan     ;
  Op.OpFUnordEqual           :Result:=Op.OpFUnordEqual           ;
  Op.OpFUnordLessThanEqual   :Result:=Op.OpFUnordGreaterThanEqual;
  Op.OpFUnordGreaterThan     :Result:=Op.OpFUnordLessThan        ;
  Op.OpFUnordNotEqual        :Result:=Op.OpFUnordNotEqual        ;
  Op.OpFUnordGreaterThanEqual:Result:=Op.OpFUnordLessThanEqual   ;

  Op.OpSLessThan             :Result:=Op.OpSGreaterThan          ;
  Op.OpIEqual                :Result:=Op.OpIEqual                ;
  Op.OpSLessThanEqual        :Result:=Op.OpSGreaterThanEqual     ;
  Op.OpSGreaterThan          :Result:=Op.OpSLessThan             ;
  Op.OpINotEqual             :Result:=Op.OpINotEqual             ;
  Op.OpSGreaterThanEqual     :Result:=Op.OpSLessThanEqual        ;

  Op.OpULessThan             :Result:=Op.OpUGreaterThan          ;
  Op.OpULessThanEqual        :Result:=Op.OpUGreaterThanEqual     ;
  Op.OpUGreaterThan          :Result:=Op.OpULessThan             ;
  Op.OpUGreaterThanEqual     :Result:=Op.OpULessThanEqual        ;
  else
   Assert(false);
 end;
end;

Function is_term_op(OpId:DWORD):Boolean;
begin
 Case OpId of
  Op.OpBranch,
  Op.OpBranchConditional,
  Op.OpSwitch,
  Op.OpReturn,
  Op.OpReturnValue,
  Op.OpKill,
  Op.OpTerminateInvocation,
  Op.OpDemoteToHelperInvocation,
  Op.OpUnreachable:Result:=True;
  else
   Result:=False;
 end;
end;

Function is_merge_op(OpId:DWORD):Boolean;
begin
 Case OpId of
  Op.OpSelectionMerge,
  Op.OpLoopMerge:Result:=True;
  else
   Result:=False;
 end;
end;

Function is_term_op(pLine:PspirvOp):Boolean;
begin
 Result:=False;
 if (pLine=nil) then Exit;

 repeat //up
  Case pLine^.OpId of
   Op.OpNop,
   OpLinks:pLine:=pLine^.pPrev;
   OpBlock:pLine:=PsrOpBlock(pLine^.dst.pData)^.pTail;
   else
    Break;
  end;
  if (pLine=nil) then Exit;
 until false;

 Result:=is_term_op(pLine^.OpId);
end;

procedure _up_merge_line(var pLine:PspirvOp);
begin
 repeat
  if is_merge_op(pLine^.OpId) then
  begin
   pLine:=pLine^.pPrev;
   Assert(pLine<>nil);
  end else
  begin
   Break;
  end;
 until false;
end;

function CompareParam(p1,p2:POpParamNode):Boolean;
begin
 Result:=(p1^.ntype=p2^.ntype) and (p1^.pData=p2^.pData);
end;

function CompareOp(p1,p2:PspirvOp):Boolean;
var
 n1,n2:POpParamNode;
begin
 Result:=False;
 if (p1^.OpId<>p2^.OpId) then Exit;
 if (p1^.dst_type<>p2^.dst_type) then Exit;

 n1:=p1^.pParam.pHead;
 n2:=p2^.pParam.pHead;

 While (n1<>nil) do
 begin
  if (n2=nil) then Exit;
  if not CompareParam(n1,n2) then Exit;
  n1:=n1^.pNext;
  n2:=n2^.pNext;
 end;

 Result:=(n2=nil);
end;

function FindUpSameOp(pLine,node:PspirvOp):PspirvOp;
var
 tmp:PspirvOp;
 pBlock:PsrOpBlock;
begin
 Result:=nil;
 if (pLine=nil) or (node=nil) then Exit;

 While (pLine<>nil) do
 begin
  if CompareOp(pLine,node) then Exit(pLine);
  tmp:=pLine^.pPrev;
  if (tmp=nil) then
  begin
   pBlock:=pLine^.pParent;
   if (pBlock=nil) then Exit;
   tmp:=pBlock^.pUpLine;
   if (tmp=nil) then
   begin
    pBlock:=pBlock^.pParent;
    if (pBlock=nil) then Exit;
    tmp:=pBlock^.pHead;
    While (tmp<>nil) do
    begin
     if (tmp^.OpId=OpBlock) and (tmp^.dst.pData=pBlock) then
     begin
      tmp:=tmp^.pPrev;
      Break;
     end;
     tmp:=tmp^.pNext;
    end;
   end else
   begin
    tmp:=tmp^.pPrev;
   end;
  end;
  pLine:=tmp;
 end;
end;


end.

