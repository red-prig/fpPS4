unit emit_post;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  srNode,
  srType,
  srTypes,
  srConst,
  srReg,
  srVariable,
  srLayout,
  srBuffer,
  srBitcast,
  srPrivate,
  srOutput,
  srOp,
  srOpUtils,
  srDecorate,
  emit_fetch;

type
 TPostCb=function(node:TSpirvOp):Integer of object;
 TRegsCb=function(pLine:TspirvOp;var node:TsrRegNode):Integer of object;

 TSprvEmit_post=class(TEmitFetch)
  function  PostStage:Integer;

  function  RegFindCond(pLine:TspirvOp;var node:TsrRegNode):Integer;
  function  RegCollapse(pLine:TspirvOp;var node:TsrRegNode):Integer;
  function  RegVResolve(pLine:TspirvOp;var node:TsrRegNode):Integer;
  function  RegWResolve(pLine:TspirvOp;var node:TsrRegNode):Integer;
  function  RegTypecast(pLine:TspirvOp;var node:TsrRegNode):Integer;
  function  RegSTStrict(pLine:TspirvOp;var node:TsrRegNode):Integer;
  function  RegVTStrict(pLine:TspirvOp;var node:TsrRegNode):Integer;

  //function  NodeOpSameOp(node:TspirvOp):Integer;

  function  NodeOpStrict(node:TspirvOp):Integer;

  function  OnOpStep1(node:TspirvOp):Integer; //backward
  function  OnOpStep2(node:TspirvOp):Integer; //forward
  function  OnOpStep3(node:TspirvOp):Integer; //forward
  function  OnOpStep4(node:TspirvOp):Integer; //forward
  function  OnOpStep5(node:TspirvOp):Integer; //backward
  function  OnOpStep6(node:TspirvOp):Integer; //backward
  function  OnOpStep7(node:TspirvOp):Integer; //backward

  function  OnDecorate(node:TspirvOp):Integer;

  function  PostFuncAnalize:Integer;

  function  OnChainUpdate(node:TsrChain):Integer;

  function  PostAllocField:Integer;
  function  PostAllocBuffer:Integer;
  procedure ShiftIndex(pChain:TsrChain;const F:TFieldFetchValue;var _offset:PtrUint);
  function  FetchField(pChain:TsrChain):TsrField;
  function  OnChainField(node:TsrChain):Integer;
  procedure AdjustMaxSize(tbuf:TsrBuffer);
  function  LinkBitcast(pBuffer:TsrBuffer):TsrNode;
  function  OnChainAlloc(node:TsrChain):Integer;
  procedure OnFieldType(node:TsrField);
  function  PostConstAnalize:Integer;
  function  PostVariableAnalize:Integer;
  function  PostTypeAnalize:Integer;
 end;

function  EnumLineRegs(cb:TRegsCb;pLine:TspirvOp):Integer;
function  EnumFirstReg(cb:TRegsCb;pLine:TspirvOp):Integer;
function  EnumBlockOpForward(cb:TPostCb;pBlock:TsrOpBlock):Integer;
function  EnumBlockOpBackward(cb:TPostCb;pBlock:TsrOpBlock):Integer;

implementation

uses
 emit_post_op;

function TSprvEmit_post.PostStage:Integer;
begin
 Result:=0;
 InputList.Test;
 Result:=Result+PostFuncAnalize;
 Result:=Result+PostVariableAnalize;
 Result:=Result+PostConstAnalize;
 Result:=Result+PostTypeAnalize;
end;

function EnumLineRegs(cb:TRegsCb;pLine:TspirvOp):Integer;
var
 node:POpParamNode;
 pReg:TsrRegNode;
begin
 Result:=0;
 if (cb=nil) or (pLine=nil) then Exit;
 node:=pLine.ParamFirst;
 While (node<>nil) do
 begin
  if node.Value.IsType(ntReg) then
  begin
   pReg:=node.AsReg;
   Result:=Result+cb(pLine,pReg);
   node.Value:=pReg;
  end;
  node:=node.Next;
 end;
end;

function EnumFirstReg(cb:TRegsCb;pLine:TspirvOp):Integer;
var
 node:POpParamNode;
 pReg:TsrRegNode;
begin
 Result:=0;
 if (cb=nil) or (pLine=nil) then Exit;
 node:=pLine.ParamFirst;
 if (node<>nil) then
 begin
  if node.Value.IsType(ntReg) then
  begin
   pReg:=node.AsReg;
   Result:=Result+cb(pLine,pReg);
   node.Value:=pReg;
  end;
 end;
end;

function EnumBlockOpForward(cb:TPostCb;pBlock:TsrOpBlock):Integer;
var
 node,prev:TspirvOp;
begin
 Result:=0;
 if (pBlock=nil) or (cb=nil) then Exit;
 node:=pBlock.First;
 While (node<>nil) do
 begin
  prev:=node;
  node:=flow_down_next_up(node);
  if prev.IsType(ntOp) then
  begin
   Result:=Result+cb(prev);
  end;
 end;
end;

function EnumBlockOpBackward(cb:TPostCb;pBlock:TsrOpBlock):Integer;
var
 node,prev:TspirvOp;
begin
 Result:=0;
 if (pBlock=nil) or (cb=nil) then Exit;
 node:=pBlock.Last;
 While (node<>nil) do
 begin
  prev:=node;
  node:=flow_down_prev_up(node);
  if prev.IsType(ntOp) then
  begin
   Result:=Result+cb(prev);
  end;
 end;
end;

function TSprvEmit_post.RegFindCond(pLine:TspirvOp;var node:TsrRegNode):Integer;
var
 old:TsrRegNode;
 pBlock:TsrOpBlock;
 tmp:TsrOpBlock;
 src:TsrRegNode;
 p:TspirvOp;
 n:Boolean;
begin
 Result:=0;
 if (node=nil) then Exit;

 old:=node;

 pBlock:=pLine.Parent;
 src:=RegDown(node);
 n:=false;

 repeat
  if src.is_const then Exit;

  tmp:=pBlock.FindUpCond(src);
  if (tmp<>nil) then
  begin
   if (tmp=pBlock) and (pLine.OpId=Op.OpBranchConditional) then Exit;

   node:=NewReg(old.dtype,@pLine);
   node.pWriter:=NewReg_b(n xor tmp.Cond.FVal,@pLine);
   Exit(1);
  end;

  p:=src.pWriter.specialize AsType<ntOp>;
  if (p=nil) then Exit;

  Case p.OpId of
   Op.OpLogicalNot:;
   Op.OpNot:;
   else
    Exit;
  end;

  src:=p.ParamFirst.AsReg;
  if (src=nil) then Exit;
  src:=RegDown(src);
  n:=not n;

 until false;
end;

function TSprvEmit_post.RegCollapse(pLine:TspirvOp;var node:TsrRegNode):Integer;
var
 old:TsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (node.dtype=dtUnknow) then Exit;

 old:=node;
 node:=RegDown(old);

 if (old<>node) then //is change?
 begin
  if (node.dtype=dtUnknow) or CompareType(node.dtype,old.dtype) then
  begin
   Inc(Result);
  end else
  begin //save to another step
   old.pWriter:=node;
   node:=old;
  end;
 end;
end;

function TSprvEmit_post.RegVResolve(pLine:TspirvOp;var node:TsrRegNode):Integer;
var
 old:TsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;

 old:=node;

 node:=RegDown(old);
 if node.pWriter.IsType(ntVolatile) then
 begin
  //create load/store
  //use forward only
  node:=PrivateList.PrepVolatile(pLine,node);
  Inc(Result);
 end;

 if (old<>node) then //is change?
 begin
  if (node.dtype=dtUnknow) or (node.dtype=old.dtype) then
  begin
   Inc(Result);
  end else
  begin //save to another step
   old.pWriter:=node;
   node:=old;
  end;
 end;
end;

function GetDepType(src:TsrRegNode):TsrDataType;
var
 node:TDependenceNode;
 pReg:TsrRegNode;
 pLine:TspirvOp;

 Function CmpType(var ret:TsrDataType;dtype:TsrDataType):Boolean;
 begin
  Result:=False;
  if (dtype=dtUnknow) then Exit;
  if (ret=dtUnknow) then
  begin
   ret:=dtype;
  end else
  begin
   if (ret<>dtype) then
   begin
    ret:=dtUnknow;
    Result:=True; //Exit
   end;
  end;
 end;

begin
 Result:=dtUnknow;
 node:=src.FirstDependence;
 //
 While (node<>nil) do
 begin
  if node.pNode.IsType(ntReg) then
  begin
   pReg:=node.pNode.specialize AsType<ntReg>;
   if (pReg<>nil) then
   begin
    if CmpType(Result,pReg.dtype) then Exit;
   end;
  end else
  if node.pNode.IsType(ntOp) then
  begin
   pLine:=node.pNode.specialize AsType<ntOp>;

   if (classif_rw_op(pLine.OpId) and 2)<>0 then
   begin
    //ignore?
   end else
   begin
    if CmpType(Result,src.dtype) then Exit;
   end;

  end;
  //
  node:=src.NextDependence(node);
 end;
end;

function ResolveWeak(new:TsrRegNode):Integer;
var
 dtype:TsrDataType;
begin
 Result:=0;
 While (new<>nil) do
 begin

  if new.dweak then
  begin
   dtype:=GetDepType(new);
   if (dtype<>dtUnknow) then
   begin
    if (new.dtype<>dtype) then
    begin
     new.dtype:=dtype;
     new.dweak:=False;
     Inc(Result);
    end else
    begin
     new.dweak:=False;
    end;
   end;
  end;

  new:=new.AsReg; //next
 end;
end;

function TSprvEmit_post.RegWResolve(pLine:TspirvOp;var node:TsrRegNode):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 Result:=Result+ResolveWeak(node);
end;

function TSprvEmit_post.RegTypecast(pLine:TspirvOp;var node:TsrRegNode):Integer;
var
 old:TsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;

 old:=node;
 node:=RegDown(old);

 if (old<>node) then //is change?
 begin
  if (node.dtype=dtUnknow) or (node.dtype=old.dtype) then
  begin
   Inc(Result);
  end else
  begin //bitcast
   node:=BitcastList.FetchCast(old.dtype,node);
   Inc(Result);
  end;
 end;
end;

function TSprvEmit_post.RegSTStrict(pLine:TspirvOp;var node:TsrRegNode):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
begin
 Result:=0;
 if (node.dtype=dtBool) then Exit;
 dst:=pLine.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 dtype:=dst.dtype;
 if (dtype<>node.dtype) then
 begin
  node:=BitcastList.FetchCast(dtype,node); //strict type
  Inc(Result);
 end;
end;

function TSprvEmit_post.RegVTStrict(pLine:TspirvOp;var node:TsrRegNode):Integer;
var
 dtype:TsrDataType;
 dst:TsrRegNode;
begin
 Result:=0;
 dst:=pLine.pDst.specialize AsType<ntReg>;
 if (dst=nil) then Exit;

 dtype:=dst.dtype.Child;
 if (dtype<>node.dtype) then
 begin
  node:=BitcastList.FetchCast(dtype,node); //strict type
  Inc(Result);
 end;
end;

{
function TSprvEmit_post.NodeOpSameOp(node:TspirvOp):Integer;
var
 tmp:TspirvOp;
 dst,src:TsrRegNode;
begin
 Result:=0;
 if (node^.dst.ntype<>ntReg) then Exit; //is reg

 Case node^.OpId of
  Op.OpLoad:;
  Op.OpCompositeConstruct:;
  OpMakeExp:;
  OpMakeVec:;
  OpPackOfs:;
  else
    Exit;
 end;

 tmp:=FindUpSameOp(node^.pPrev,node);
 if (tmp=nil) then Exit;

 src:=tmp^.dst.AsReg;
 dst:=node^.dst.AsReg;

 if (src=nil) or (dst=nil) then Exit;

 dst^.SetReg(src);

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);

 Result:=1;
end;
}

function TSprvEmit_post.NodeOpStrict(node:TspirvOp):Integer;
begin
 Result:=0;
 if not node.pDst.IsType(ntReg) then Exit; //is reg

 Case node.OpId of
  Op.OpBitFieldSExtract  ,
  Op.OpBitFieldUExtract  :Result:=EnumFirstReg(@RegSTStrict,node);
  Op.OpSelect            :Result:=EnumLineRegs(@RegSTStrict,node);
  Op.OpIAddCarry         ,
  Op.OpISubBorrow        ,
  Op.OpUMulExtended      ,
  Op.OpSMulExtended      ,
  Op.OpCompositeConstruct:Result:=EnumLineRegs(@RegVTStrict,node);
  else;
 end;

end;

//

function TSprvEmit_post.OnOpStep1(node:TspirvOp):Integer; //backward
begin
 Result:=0;

 if node.is_cleared then Exit;

 if node.can_clear then
 begin
  if node.Clear then Inc(Result);
 end else
 begin
  //Result:=Result+NodeOpSameOp(node);
  Result:=Result+EnumLineRegs(@RegCollapse,node);
  Result:=Result+EnumLineRegs(@RegFindCond,node);
 end;
end;

function TSprvEmit_post.OnOpStep2(node:TspirvOp):Integer; //forward
begin
 Result:=0;

 if node.is_cleared then Exit;

 if node.can_clear then
 begin
  if node.Clear then Inc(Result);
 end else
 begin
  Result:=Result+TEmitPostOp(TObject(Self)).PostForward1(node);
 end;
end;

function TSprvEmit_post.OnOpStep3(node:TspirvOp):Integer; //forward
begin
 Result:=0;

 if node.is_cleared then Exit;

 if node.can_clear then
 begin
  if node.Clear then Inc(Result);
 end else
 begin
  Result:=Result+TEmitPostOp(TObject(Self)).PostForward2(node);
 end;
end;

function TSprvEmit_post.OnOpStep4(node:TspirvOp):Integer; //forward
begin
 Result:=0;

 if node.is_cleared then Exit;

 if node.can_clear then
 begin
  if node.Clear then Inc(Result);
 end else
 begin
  Result:=Result+EnumLineRegs(@RegVResolve,node);
 end;
end;

function TSprvEmit_post.OnOpStep5(node:TspirvOp):Integer; //backward
begin
 Result:=0;

 if node.is_cleared then Exit;

 if node.can_clear then
 begin
  if node.Clear then Inc(Result);
 end else
 begin
  Result:=Result+EnumLineRegs(@RegWResolve,node);
 end;
end;

function TSprvEmit_post.OnOpStep6(node:TspirvOp):Integer; //backward
begin
 Result:=0;

 if node.is_cleared then Exit;

 if node.can_clear then
 begin
  if node.Clear then Inc(Result);
 end else
 begin
  Result:=Result+EnumLineRegs(@RegTypecast,node);
  Result:=Result+NodeOpStrict(node);
  Result:=Result+EnumLineRegs(@RegFindCond,node);
 end;
end;

function TSprvEmit_post.OnOpStep7(node:TspirvOp):Integer; //backward
begin
 Result:=0;

 if node.is_cleared then
 begin
  Assert(node.read_count=0);
  node.Remove;
  Inc(Result);
 end else
 if node.can_clear then
 begin
  node.Clear;
  node.Remove;
  Inc(Result);
 end;
end;

function TSprvEmit_post.OnDecorate(node:TspirvOp):Integer;
begin
 Result:=0;

 case node.OpId of
  Op.OpFAdd,
  Op.OpFSub,
  Op.OpFMul,
  Op.OpFDiv,
  Op.OpFRem,
  Op.OpFMod:
    if (node.pDst<>nil) then
    begin
     DecorateList.OpDecorate(node.pDst,Decoration.NoContraction,0);
    end;
  Op.OpImageQuerySizeLod:
    begin
     AddCapability(Capability.ImageQuery);
    end;
  else;
 end;

end;

function TSprvEmit_post.PostFuncAnalize:Integer;
label
 _pass;
var
 pFunc:TSpirvFunc;
 data_layout:Boolean;
 i,r4:Integer;
begin
 Result:=0;
 data_layout:=false;
 //backward analize
 pFunc:=FuncList.FList.pTail;
 While (pFunc<>nil) do
 begin

  data_layout:=(Main=pFunc);

  _pass:

  repeat //OnOpStep5

   repeat //OnOpStep3

    repeat //OnOpStep2

     repeat //OnOpStep1
      i:=EnumBlockOpBackward(@OnOpStep1,pFunc.pTop); //OnOpStep1 Reg Collapse
      if (i=0) then Break;
      Result:=Result+i;
     until false;

     i:=EnumBlockOpForward(@OnOpStep2,pFunc.pTop); //OnOpStep2 PostForward1
     if (i=0) then Break;
     Result:=Result+i;
    until false;

    i:=EnumBlockOpForward(@OnOpStep3,pFunc.pTop); //OnOpStep3 PostForward2
    if (i=0) then Break;
    Result:=Result+i;
   until false;

   if data_layout then
   begin
    Result:=Result+PostAllocField;
   end;

   repeat //OnOpStep4 Volatile Reslove
    i:=EnumBlockOpForward(@OnOpStep4,pFunc.pTop);
    if (i=0) then Break;
    Result:=Result+i;
   until false;

   r4:=0;
   repeat //OnOpStep5 Weak Reslove
    i:=EnumBlockOpBackward(@OnOpStep5,pFunc.pTop);
    if (i=0) then Break;
    r4:=r4+i;
   until false;

   if (r4=0) then Break;
   Result:=Result+r4;
  until false;

  PrivateList.RemoveAllStore;

  if data_layout then
  begin
   Result:=Result+PostAllocBuffer;
  end;

  //UpdateRegType OpLoad/OpStore
  DataLayoutList.EnumChain(@OnChainUpdate);
  PrivateList.Post;

  if data_layout then
  begin
   OutputList.Post;
  end;

  //pass agian
  if data_layout then
  begin
   data_layout:=false;
   goto _pass;
  end;

  repeat //OnOpStep6 Typecast
   i:=EnumBlockOpBackward(@OnOpStep6,pFunc.pTop);
   if (i=0) then Break;
   Result:=Result+i;
  until false;

  Result:=Result+EnumBlockOpBackward(@OnOpStep7,pFunc.pTop); //OnOpStep7 Remove Lines

  EnumBlockOpForward(@OnDecorate,pFunc.pTop); //NoContraction

  pFunc:=pFunc.Prev;
 end;
end;

function TSprvEmit_post.OnChainUpdate(node:TsrChain):Integer;
var
 pField:TsrField;
begin
 Result:=0;
 //
 pField:=node.pField;
 if (pField<>nil) then
 if (pField.Fdtype<>dtUnknow) then
 if (node.dtype=dtUnknow) then
 begin
  node.dtype:=pField.Fdtype; //update type
 end;
 //
 node.UpdateRegType;
end;

function TSprvEmit_post.PostAllocField:Integer;
begin
 Result:=0;

 DataLayoutList.AllocID;

 Result:=Result+DataLayoutList.EnumChain(@OnChainField);
end;

function TSprvEmit_post.PostAllocBuffer:Integer;
begin
 Result:=0;

 BufferList.ApplyBufferType;
 BufferList.AlignOffset;
 BufferList.FillSpace;
 BufferList.AllocID;
 BufferList.EnumAllField(@OnFieldType);

 Result:=Result+DataLayoutList.EnumChain(@OnChainAlloc);
end;

procedure TSprvEmit_post.ShiftIndex(pChain:TsrChain;const F:TFieldFetchValue;var _offset:PtrUint);
var
 _stride,_count:PtrUint;
 pIndex:TsrRegNode;
 pLine:TsrNode;
begin
 _offset:=_offset-F.pField.offset;
 //stride relative
 _stride:=F.pField.stride;
 _count :=_offset div _stride;
 _offset:=_offset mod _stride;
 //
 if (_count<>0) or (pChain.pIndex=nil) then
 begin
  //save
  pChain.FUndoIndex :=pChain.pIndex;
  pChain.FUndoOffset:=pChain.offset;
  //shift
  pIndex:=pChain.pIndex;
  if (pIndex<>nil) then
  begin
   pLine :=pIndex.pLine;
   pIndex:=OpIAddTo(pIndex,_count,@pLine);
  end else
  begin
   pLine :=init_line;
   pIndex:=NewReg_q(dtUint32,_count,@pLine);
  end;
  pChain.pIndex:=pIndex;
  pChain.offset:=pChain.offset-(_count*_stride);
 end;
end;

procedure UndoIndex(pChain:TsrChain);
begin
 if (pChain.FUndoIndex<>nil) then
 begin
  //undo
  pChain.pIndex:=pChain.FUndoIndex;
  pChain.offset:=pChain.FUndoOffset;
  //clear
  pChain.FUndoIndex :=nil;
  pChain.FUndoOffset:=0;
 end;
end;

function TSprvEmit_post.FetchField(pChain:TsrChain):TsrField;
label
 _start,
 _resolve,
 _exit;
var
 buf:TsrBuffer;
 F:TFieldFetchValue;
 _offset:PtrUint;
 dtype:TsrDataType;

 max:DWORD;
begin
 buf:=nil;

 _start:

 dtype:=pChain.dtype;

 if (buf=nil) then
 begin
  buf:=BufferList.Fetch(pChain.parent,0,pChain.Flags.GLC,pChain.Flags.SLC);
 end else
 begin
  buf:=BufferList.NextAlias(buf,pChain.Flags.GLC,pChain.Flags.SLC);
 end;

 UndoIndex(pChain);
 _offset:=pChain.offset;

 if (pChain.pIndex<>nil) then
 begin
  //TODO: interval analize

  if pChain.Parent.IsLocalDataShare or
     pChain.Parent.IsGlobalDataShare then
  begin
   //fixed to 64KB

   //FLDS_SIZE

   max:=0;
   case pChain.Parent.key.rtype of
    rtLDS:
      begin
       max:=32*1024;
       if (max>FLDS_SIZE) then max:=FLDS_SIZE;
      end;
    rtGDS:max:=64*1024;
   end;

   if (Align(_offset,pChain.stride)>max) then
   begin
    Assert(false,'LDS/GDS big addresing?');
   end;

   F:=buf.FTop.FetchArray(_offset,(max-_offset),pChain.stride);
  end else
  begin
   F:=buf.FTop.FetchRuntimeArray(_offset,pChain.stride);
  end;

  goto _resolve;
 end else
 begin
  F.pField:=buf.FTop;
 end;

 repeat

  F:=F.pField.FetchValue(_offset,pChain.size,dtype,pChain.dweak);

  _resolve:
  Case F.fValue of
   frNotFit       :goto _start; //next alias
   frIdent        :goto _exit;
   frVectorAsValue:goto _exit;
   frValueInVector:
    begin
     _offset:=_offset-F.pField.offset;
     //patch dtype
     dtype:=F.pField.Fdtype.Child;
    end;
   frValueInArray :ShiftIndex(pChain,F,_offset);
  end;

 until false;

 _exit:

  Result:=F.pField;
end;

function TSprvEmit_post.OnChainField(node:TsrChain):Integer;
var
 pField:TsrField;
begin
 Result:=1;
 //Writeln('OnChainsField:',dtype,':',node^.key.offset);
 pField:=FetchField(node);
 Assert(pField<>nil);
 node.pField :=pField;         //save link
 node.dtype  :=pField.Fdtype;  //update type
 node.pBuffer:=pField.pBuffer; //save buffer
end;

procedure TSprvEmit_post.OnFieldType(node:TsrField);
var
 count:PtrUint;

 items:PPsrType;
 sType,vType:TsrType;

 child:TsrField;
begin
 if (node.vType<>nil) then Exit;

 if (node.Fdtype in [dtTypeStruct,dtTypeArray,dtTypeRuntimeArray]) then
 begin
  if node.IsStructNotUsed then
  begin
   child:=node.First;
   Assert(child<>nil);
   Assert(child.vType<>nil);
   sType:=child.vType;
  end else
  begin
   count:=node.FCount;
   Assert(count<>0);
   items:=Alloc(SizeOf(Pointer)*count);

   count:=0;
   child:=node.First;
   While (child<>nil) do
   begin
    Assert(child.vType<>nil);
    items[count]:=child.vType;
    Inc(count);
    child:=node.Next(child);
   end;

   if node.IsTop then
   begin
    //on top level stride is unknow
    sType:=TypeList.InsertStruct(count,items,False,node.Size); //unique
   end else
   begin
    sType:=TypeList.FetchStruct (count,items,False,node.stride);
   end;

  end;

  Case node.Fdtype of
   dtTypeArray:
     begin
      count:=node.count;
      vType:=TypeList.FetchArray(sType,count);
     end;
   dtTypeRuntimeArray:
     begin
      vType:=TypeList.FetchRuntimeArray(sType);
     end;
   else
     begin
      vType:=sType;
     end;
  end;

  node.sType:=sType;
  node.vType:=vType;

  if node.IsTop then
  begin
   //Alloc Type Var
   node.pBuffer.pType:=vType;
  end;

 end else
 begin
  node.sType:=nil;
  node.vType:=TypeList.Fetch(node.Fdtype);
 end;

end;

procedure TSprvEmit_post.AdjustMaxSize(tbuf:TsrBuffer);
var
 node:TsrBuffer;
 max,size:Ptruint;
begin
 node:=tbuf;
 max:=0;
 while (node<>nil) do
 begin
  size:=node.GetSize;
  if (size>max) then
  begin
   max:=size;
  end;
  //
  node:=node.pNextAlias;
 end;
 //
 size:=tbuf.GetSize;
 if (max>size) then
 begin
  if (max>=High(WORD)) then
  begin
   //runtime array fill
   tbuf.FTop.FetchRuntimeArray(size,4);
  end else
  begin
   //fixed fill
   tbuf.FTop.FillNode(size,max-size);
  end;
  //
  tbuf.EnumAllField(@OnFieldType);
 end;
end;

function TSprvEmit_post.LinkBitcast(pBuffer:TsrBuffer):TsrNode;
var
 pLine:TspirvOp;

 use:Boolean;

 tbuf:TsrBuffer;
 tref:TsrNode;
 pType:TsrType;
begin
 Result:=pBuffer.pVar; //direct

 if pBuffer.pLayout.UseBitcast then
 if pBuffer.Flags.Aliased then
 if (pBuffer.AliasId<>0) then
 begin
  pLine:=init_line;

  //zero alias
  tbuf:=BufferList.Fetch(pBuffer.pLayout,0,False,False);

  tref:=tbuf.tRef;
  if (tref=nil) then
  begin
   //calc max size
   AdjustMaxSize(tbuf);

   Assert(tbuf.FTop.vType<>nil);
   Assert(tbuf.pVar<>nil);

   //OpAccessChain storage class link
   tref:=specialize New<TsrChain>;
   TsrChain(tref).pBuffer:=tbuf;

   //self ref
   pLine:=OpAccessChain(pLine,tbuf.FTop.vType,tref,tbuf.pVar);
   //cache
   tbuf.tRef:=tref;
   tbuf.Flags.Bitcast:=True;
  end;

  Result:=pBuffer.tRef;
  if (Result=nil) then
  begin
   Assert(pBuffer.FTop.vType<>nil);
   Assert(pBuffer.pVar<>nil);

   //OpAccessChain storage class link
   Result:=specialize New<TsrChain>;
   TsrChain(Result).pBuffer:=pBuffer;

   //bitcast pointer to this alias
   pType:=TypeList.FetchPointer(pBuffer.FTop.vType,pBuffer.pVar.GetStorageClass);
   pLine:=OpBitcast(pLine,pType,Result,tref);
   //cache
   pBuffer.tRef:=Result;
   pBuffer.Flags.Bitcast:=True;
  end;

 end;
end;

function TSprvEmit_post.OnChainAlloc(node:TsrChain):Integer;
var
 pLine:TspirvOp;
 pIndex:TsrRegNode;
 pReg:TsrRegNode;
 pField:TsrField;
 Parent:TsrField;
 src:TsrNode;
 pParam:POpParamNode;
begin
 Result:=1;

 pField:=node.pField;
 Assert(pField<>nil);

 //Make Bitcast type or directly
 src:=LinkBitcast(pField.pBuffer);
 Assert(src<>nil);
 //

 //get line after LinkBitcast
 pIndex:=RegDown(node.pIndex);
 if (pIndex=nil) or (pIndex.is_const) then
 begin
  pLine:=init_line;
 end else
 begin
  pLine:=node.FirstLine;
  Assert(pLine<>nil);
  pLine:=pLine.Prev;
  Assert(pLine<>nil);
 end;

 pLine:=OpAccessChain(pLine,pField.vType,node,src);

 pParam:=pLine.ParamLast;
 repeat

  Parent:=pField.pParent;
  if (Parent<>nil) then
   Case Parent.Fdtype of
    dtTypeStruct:
      begin
       pReg:=NewReg_i(dtUint32,pField.FID,@pLine);
       pLine.AddParamAfter(pParam,pReg);
      end;
    dtTypeArray,
    dtTypeRuntimeArray:
      begin
       if not Parent.IsStructNotUsed then
       begin
        pReg:=NewReg_i(dtUint32,pField.FID,@pLine);
        pLine.AddParamAfter(pParam,pReg);
       end;
       Assert(pIndex<>nil);
       pLine.AddParamAfter(pParam,pIndex);
      end;
    else
     if Parent.Fdtype.isVector then
     begin
      pReg:=NewReg_i(dtUint32,pField.FID,@pLine);
      pLine.AddParamAfter(pParam,pReg);
     end;
   end;

  pField:=Parent;
 until (pField=nil);

end;

function TSprvEmit_post.PostConstAnalize:Integer;
var
 node:TsrConst;
begin
 Result:=0;
 node:=ConstList.FList.pTail;
 While (node<>nil) do
 begin
  if (not node.IsUsed) then
  begin
   ConstList.FList.Remove(node); //remove?
   Inc(Result);
  end;
  node:=node.Prev;
 end;
end;

function TSprvEmit_post.PostVariableAnalize:Integer;
var
 node:TsrVariable;
begin
 Result:=0;
 node:=VariableList.FList.pTail;
 While (node<>nil) do
 begin
  if (not node.IsUsed) then
  begin
   VariableList.FList.Remove(node); //remove?
   Inc(Result);
  end else
  begin
   node.UpdateType();
  end;
  node:=node.Prev;
 end;
end;

function TSprvEmit_post.PostTypeAnalize:Integer;
var
 node:TsrType;
begin
 Result:=0;
 node:=TypeList.FList.pTail;
 While (node<>nil) do
 begin
  if (not node.IsUsed) then
  begin
   TypeList.FList.Remove(node); //remove?
   Inc(Result);
  end;
  node:=node.Prev;
 end;
end;

end.

