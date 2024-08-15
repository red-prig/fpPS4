unit srOpUtils;

{$mode objfpc}{$H+}

interface

uses
 spirv,
 srNode,
 srOp,
 srReg,
 srLayout,
 srVariable,
 srCFGLabel;

const
 OpIAddExt=DWORD(-1);
 OpISubExt=DWORD(-2);
 OpAbsDiff=DWORD(-3);
 OpWQM32  =DWORD(-4);

 OpBFE_32 =DWORD(-5);
 OpBFIB32 =DWORD(-6);

 OpPackAnc=DWORD(-7);
 OpPackOfs=DWORD(-8);
 OpMakeExp=DWORD(-9);
 OpMakeVec=DWORD(-10);
 OpMakeCub=DWORD(-11);

 OpCUBEID =DWORD(-12);
 OpCUBESC =DWORD(-13);
 OpCUBETC =DWORD(-14);
 OpCUBEMA =DWORD(-15);

function  InsSpirvOp(pLine,pNew:TSpirvOp):TSpirvOp;
Function  get_inverse_left_cmp_op(OpId:DWORD):DWORD;
Function  get_inverse_not_cmp_op(OpId:DWORD):DWORD;
Function  is_term_op(OpId:DWORD):Boolean;
Function  is_merge_op(OpId:DWORD):Boolean;
Function  is_term_op(pLine:TSpirvOp):Boolean;
procedure up_merge_line(var pLine:TSpirvOp);
function  FindUpSameOp(pLine,node:TSpirvOp):TSpirvOp;
function  IsDominUp(pNodeUp,pLine:TSpirvOp):Boolean;

function  GetGlobalIndex(pLine:TSpirvOp):DWORD;
function  GetGlobalIndexA(pLine:TSpirvOp):DWORD;
function  MaxLine(p1,p2:TSpirvOp):TSpirvOp;
function  MinLine(p1,p2:TSpirvOp):TSpirvOp;
Function  IsParentOf(pLine,pCurr:TsrOpBlock):Boolean;
Function  IsParentOfLine(pLine,pCurr:TSpirvOp):Boolean;
function  GetMaxPlace(pLine:TSpirvOp;count:Byte;src:PPsrRegNode):TSpirvOp;

function  GetChainRegNode (node:TsrRegNode):TsrChain;
function  GetSourceRegNode(node:TsrRegNode):TsrNode;

function  flow_down_next_up(pLine:TSpirvOp):TSpirvOp;
function  flow_down_prev_up(pLine:TSpirvOp):TSpirvOp;
function  flow_prev_up(pLine:TSpirvOp):TSpirvOp;

implementation

//--

function flow_down_next_up(pLine:TSpirvOp):TSpirvOp;
begin
 Result:=pLine.First; //down
 if (Result=nil) then
 begin
  repeat //up
   Result:=pLine.Next;
   pLine:=pLine.Parent;
  until (pLine=nil) or (Result<>nil);
 end;
end;

function flow_down_prev_up(pLine:TSpirvOp):TSpirvOp;
begin
 Result:=pLine.Last; //down
 if (Result=nil) then
 begin
  repeat //up
   Result:=pLine.Prev;
   pLine:=pLine.Parent;
  until (pLine=nil) or (Result<>nil);
 end;
end;

function flow_prev_up(pLine:TSpirvOp):TSpirvOp;

 function _last(p:TSpirvOp):TSpirvOp;
 begin
  Result:=nil;
  if (p<>nil) then
  if p.IsType(ntOpBlock) then
  if not (TsrOpBlock(p).Block.bType in [btCond,btLoop]) then
  begin
   Result:=p.Last;
  end;
 end;

begin
 Result:=_last(pLine); //down

 if (Result=nil) then
 begin
  repeat //up
   Result:=pLine.Prev;
   pLine:=pLine.Parent;
  until (pLine=nil) or (Result<>nil);
 end;
end;

function InsSpirvOp(pLine,pNew:TSpirvOp):TSpirvOp;
var
 tmp:TSpirvOp;
begin
 Result:=nil;
 Assert(pLine<>nil);
 if (pLine=nil) or (pNew=nil) then Exit;

 if (pLine.Next<>nil) then
 if pNew.IsType(ntOp) then
 begin
  tmp:=pLine;
  while (not tmp.IsType(ntOp)) do
  begin
   tmp:=flow_down_prev_up(tmp);
   Assert(tmp<>nil);
  end;
  pNew.Adr:=tmp.Adr;
 end;

 pLine.InsertAfter(pNew);
 Result:=pNew;
end;

function GetCurrentIndex(pLine:TSpirvOp):DWORD;
var
 pParent:TsrOpBlock;
 node:TSpirvOp;
begin
 Result:=0;
 if (pLine=nil) then Exit;
 pParent:=pLine.Parent;
 if (pParent=nil) then Exit;
 node:=pParent.First;
 While (node<>nil) and (node<>pLine) do
 begin
  Result:=Result+node.GetIndexCount;
  node:=node.Next;
 end;
end;

function GetGlobalIndex(pLine:TSpirvOp):DWORD;
var
 pParent:TsrOpBlock;
 node:TSpirvOp;
begin
 Result:=0;
 if (pLine=nil) then Exit;
 node:=pLine;
 pParent:=node.Parent;
 While (pParent<>nil) do
 begin
  Result:=Result+GetCurrentIndex(node);
  node:=pParent;
  pParent:=node.Parent;
 end;
end;

function GetGlobalIndexA(pLine:TSpirvOp):DWORD;
begin
 Result:=pLine.GetIndexCount+GetGlobalIndex(pLine);
end;

function isGTLine(p1,p2:TSpirvOp):Boolean; //(p1>p2)
begin
 Result:=False;
 p2:=p2.Next;
 While (p2<>nil) do
 begin
  if (p1=p2) then Exit(True);
  p2:=p2.Next;
 end;
end;

Function IsGTLevel(p1,p2:TSpirvOp):Boolean; //(p1>p2)
var
 pParent1:TsrOpBlock;
 pParent2:TsrOpBlock;
begin
 Result:=False;
 pParent1:=p1.Parent;
 pParent2:=p2.Parent;
 Result:=(pParent1.Level>pParent2.Level);
end;

function MaxLine(p1,p2:TSpirvOp):TSpirvOp;
var
 pParent:TsrOpBlock;
 node:TSpirvOp;
 i,w:DWORD;
begin
 Result:=nil;
 if (p1=nil) then Exit(p2);
 if (p2=nil) or (p1=p2) then Exit(p1);

 if IsGTLevel(p2,p1) then //(p2>p1)
 begin
  //swap
  node:=p1;
  p1:=p2;
  p2:=node;
 end;

 i:=0;
 node:=p1;
 pParent:=node.Parent;
 While (pParent<>nil) do
 begin
  if (pParent=p2.Parent) then
  begin
   if isGTLine(node,p2) then //(node>p2)
   begin
    //Assert(not isGTLine(p2,node));
    Exit(p1);
   end else
   begin
    //Assert(isGTLine(p2,node));
    Exit(p2);
   end;
  end;
  i:=i+GetCurrentIndex(node);
  node:=pParent;
  pParent:=node.Parent;
 end;

 w:=GetGlobalIndex(p2);
 if (i>w) then
 begin
  Result:=p1;
 end else
 begin
  Result:=p2;
 end;
end;

function MinLine(p1,p2:TSpirvOp):TSpirvOp;
begin
 if (MaxLine(p1,p2)=p1) then
 begin
  Result:=p2;
 end else
 begin
  Result:=p1;
 end;
end;

Function IsParentOf(pLine,pCurr:TsrOpBlock):Boolean;
begin
 Result:=False;
 if not pLine.IsType(ntOpBlock) then Exit;
 if not pCurr.IsType(ntOpBlock) then Exit;
 While (pLine<>nil) do
 begin
  if (pLine=pCurr) then Exit(True);
  pLine:=pLine.Parent;
 end;
end;

Function IsParentOfLine(pLine,pCurr:TSpirvOp):Boolean;
begin
 Result:=IsParentOf(pLine.Parent,pCurr.Parent);
end;

function GetMaxPlace(pLine:TSpirvOp;count:Byte;src:PPsrRegNode):TSpirvOp;
var
 m:TSpirvOp;
 i:Byte;
begin
 Result:=pLine;
 if (count=0) or (src=nil) then Exit;
 m:=nil;
 For i:=0 to count-1 do
 begin
  if (not src[i].is_const) then
  if IsParentOfLine(pLine,src[i].pLine) then
  begin
   m:=MaxLine(m,src[i].pLine);
  end;
 end;
 if (m<>nil) then
 begin
  Result:=m;
 end;
end;

Function get_inverse_left_cmp_op(OpId:DWORD):DWORD;
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

  Op.OpIEqual                :Result:=Op.OpIEqual                ;
  Op.OpINotEqual             :Result:=Op.OpINotEqual             ;

  Op.OpSLessThan             :Result:=Op.OpSGreaterThan          ;
  Op.OpSLessThanEqual        :Result:=Op.OpSGreaterThanEqual     ;
  Op.OpSGreaterThan          :Result:=Op.OpSLessThan             ;
  Op.OpSGreaterThanEqual     :Result:=Op.OpSLessThanEqual        ;

  Op.OpULessThan             :Result:=Op.OpUGreaterThan          ;
  Op.OpULessThanEqual        :Result:=Op.OpUGreaterThanEqual     ;
  Op.OpUGreaterThan          :Result:=Op.OpULessThan             ;
  Op.OpUGreaterThanEqual     :Result:=Op.OpULessThanEqual        ;
  else;
 end;
end;

Function get_inverse_not_cmp_op(OpId:DWORD):DWORD;
begin
 Result:=0;
 Case OpId of
  Op.OpFOrdLessThan          :Result:=Op.OpFUnordGreaterThanEqual;
  Op.OpFOrdEqual             :Result:=Op.OpFUnordNotEqual;
  Op.OpFOrdLessThanEqual     :Result:=Op.OpFUnordGreaterThan;
  Op.OpFOrdGreaterThan       :Result:=Op.OpFUnordLessThanEqual;
  Op.OpFOrdNotEqual          :Result:=Op.OpFUnordEqual;
  Op.OpFOrdGreaterThanEqual  :Result:=Op.OpFUnordLessThan;
  Op.OpOrdered               :Result:=Op.OpUnordered;
  Op.OpUnordered             :Result:=Op.OpOrdered;
  Op.OpFUnordLessThan        :Result:=Op.OpFOrdGreaterThanEqual;
  Op.OpFUnordEqual           :Result:=Op.OpFOrdNotEqual;
  Op.OpFUnordLessThanEqual   :Result:=Op.OpFOrdGreaterThan;
  Op.OpFUnordGreaterThan     :Result:=Op.OpFOrdLessThanEqual;
  Op.OpFUnordNotEqual        :Result:=Op.OpFOrdEqual;
  Op.OpFUnordGreaterThanEqual:Result:=Op.OpFOrdLessThan;

  Op.OpIEqual                :Result:=Op.OpINotEqual;
  Op.OpINotEqual             :Result:=Op.OpIEqual;

  Op.OpSLessThan             :Result:=Op.OpSGreaterThanEqual;
  Op.OpSLessThanEqual        :Result:=Op.OpSGreaterThan;
  Op.OpSGreaterThan          :Result:=Op.OpSLessThanEqual;
  Op.OpSGreaterThanEqual     :Result:=Op.OpSLessThan;

  Op.OpULessThan             :Result:=Op.OpUGreaterThanEqual;
  Op.OpULessThanEqual        :Result:=Op.OpUGreaterThan;
  Op.OpUGreaterThan          :Result:=Op.OpULessThanEqual;
  Op.OpUGreaterThanEqual     :Result:=Op.OpULessThan;
  else;
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

Function is_term_op(pLine:TSpirvOp):Boolean;
begin
 Result:=False;
 if (pLine=nil) then Exit;

 repeat //up

  if pLine.IsType(ntOpBlock) then
  begin
   //
  end else
  if pLine.IsType(ntOp) then
  begin
   if not pLine.is_cleared then
   begin
    Case pLine.OpId of
     Op.OpNop:; //
     else
      Break;
    end;
   end;
  end else
  begin
   Exit;
  end;

  pLine:=flow_down_prev_up(pLine);

 until false;

 Result:=is_term_op(pLine.OpId);
end;

procedure up_merge_line(var pLine:TSpirvOp);
var
 node:TSpirvOp;
begin
 repeat
  if pLine.IsType(ntOp) then
  begin
   if pLine.is_cleared or is_merge_op(pLine.OpId) then
   begin
    node:=pLine.Prev;
    if (node<>nil) then
    begin
     pLine:=node;
     Continue;
    end;
   end;
  end;
  Break;
 until false;
end;

function CompareParam(p1,p2:POpParamNode):Boolean;
begin
 Result:=(p1.Value=p2.Value);
end;

function CompareOp(p1,p2:TSpirvOp):Boolean;
var
 n1,n2:POpParamNode;
begin
 Result:=False;

 if not p1.IsType(ntOp) then Exit;
 if not p2.IsType(ntOp) then Exit;

 if (p1.OpId<>p2.OpId) then Exit;
 if (p1.pDst<>p2.pDst) then Exit;

 n1:=p1.ParamNode(0);
 n2:=p2.ParamNode(0);

 While (n1<>nil) do
 begin
  if (n2=nil) then Exit;
  if not CompareParam(n1,n2) then Exit;
  n1:=n1.Next;
  n2:=n2.Next;
 end;

 Result:=(n2=nil);
end;

function FindUpSameOp(pLine,node:TSpirvOp):TSpirvOp;
begin
 Result:=nil;
 if (pLine=nil) or (node=nil) then Exit;

 While (pLine<>nil) do
 begin
  if not pLine.is_cleared then
  begin
   if CompareOp(pLine,node) then Exit(pLine);
  end;
  pLine:=flow_prev_up(pLine);
 end;
end;

function IsDominUp(pNodeUp,pLine:TSpirvOp):Boolean;
begin
 Result:=False;
 if (pNodeUp=nil) or (pLine=nil) then Exit;

 While (pLine<>nil) do
 begin
  if (pNodeUp=pLine) then Exit(True);
  pLine:=flow_prev_up(pLine);
 end;
end;

function GetChainRegNode(node:TsrRegNode):TsrChain;
var
 pOp:TSpirvOp;
begin
 Result:=nil;
 node:=RegDown(node);
 pOp:=node.pWriter.specialize AsType<ntOp>;
 if (pOp=nil) then Exit;
 if (pOp.OpId<>Op.OpLoad) then Exit;
 Result:=pOp.ParamNode(0).Value.specialize AsType<ntChain>;
end;

function GetSourceRegNode(node:TsrRegNode):TsrNode;
var
 pOp:TSpirvOp;
 pVar:TsrVariable;
begin
 Result:=nil;
 node:=RegDown(node);
 pOp:=node.pWriter.specialize AsType<ntOp>;
 if (pOp=nil) then Exit;
 if (pOp.OpId<>Op.OpLoad) then Exit;
 pVar:=pOp.ParamNode(0).Value.specialize AsType<ntVariable>;
 if (pVar=nil) then Exit;
 Result:=pVar.pSource;
end;

end.

