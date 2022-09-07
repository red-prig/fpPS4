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
  srOp,
  srOpUtils,
  emit_fetch;

type
 TPostCb=function(node:PSpirvOp):Integer of object;
 TRegsCb=function(pLine:PspirvOp;var node:PsrRegNode):Integer of object;

 TSprvEmit_post=class(TEmitFetch)
  function  PostStage:Integer;

  function  RegFindCond(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegCollapse(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegVResolve(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegWResolve(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegTypecast(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegSTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegVTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;

  //function  NodeOpSameOp(node:PSpirvOp):Integer;
  function  NodeOpStrict(node:PSpirvOp):Integer;

  function  OnOpStep1(node:PSpirvOp):Integer; //backward
  function  OnOpStep2(node:PSpirvOp):Integer; //forward
  function  OnOpStep3(node:PSpirvOp):Integer; //forward
  function  OnOpStep4(node:PSpirvOp):Integer; //forward
  function  OnOpStep5(node:PSpirvOp):Integer; //backward
  function  OnOpStep6(node:PSpirvOp):Integer; //backward
  function  OnOpStep7(node:PSpirvOp):Integer; //backward

  function  PostFuncAnalize:Integer;

  function  OnChainUpdate(node:PsrChain):Integer;

  function  PostDataLayoutAnalize1:Integer;
  function  PostDataLayoutAnalize2:Integer;
  function  FetchField(var pChain:PsrChain;dtype:TsrDataType):PsrField;
  function  OnChainField(node:PsrChain):Integer;
  function  OnChainAlloc(node:PsrChain):Integer;
  procedure OnFieldType(node:PsrField);
  function  PostConstAnalize:Integer;
  function  PostVariableAnalize:Integer;
  function  PostTypeAnalize:Integer;
 end;

function  EnumLineRegs(cb:TRegsCb;pLine:PSpirvOp):Integer;
function  EnumBlockOpForward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
function  EnumBlockOpBackward(cb:TPostCb;pBlock:PsrOpBlock):Integer;

implementation

uses
 emit_post_op;

function TSprvEmit_post.PostStage:Integer;
begin
 Result:=0;
 Result:=Result+PostFuncAnalize;
 Result:=Result+PostVariableAnalize;
 Result:=Result+PostConstAnalize;
 Result:=Result+PostTypeAnalize;
end;

function EnumLineRegs(cb:TRegsCb;pLine:PSpirvOp):Integer;
var
 node:POpParamNode;
 pReg:PsrRegNode;
begin
 Result:=0;
 if (cb=nil) or (pLine=nil) then Exit;
 node:=pLine^.ParamFirst;
 While (node<>nil) do
 begin
  if node^.Value^.IsType(ntReg) then
  begin
   pReg:=node^.AsReg;
   Result:=Result+cb(pLine,pReg);
   node^.Value:=pReg;
  end;
  node:=node^.Next;
 end;
end;

function EnumBlockOpForward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
var
 node,prev:PSpirvOp;
begin
 Result:=0;
 if (pBlock=nil) or (cb=nil) then Exit;
 node:=pBlock^.First;
 While (node<>nil) do
 begin
  prev:=node;
  node:=flow_down_next_up(node);
  if prev^.IsType(ntOp) then
  begin
   Result:=Result+cb(prev);
  end;
 end;
end;

function EnumBlockOpBackward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
var
 node,prev:PSpirvOp;
begin
 Result:=0;
 if (pBlock=nil) or (cb=nil) then Exit;
 node:=pBlock^.Last;
 While (node<>nil) do
 begin
  prev:=node;
  node:=flow_down_prev_up(node);
  if prev^.IsType(ntOp) then
  begin
   Result:=Result+cb(prev);
  end;
 end;
end;

function TSprvEmit_post.RegFindCond(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 pBlock:PsrOpBlock;
 tmp:PsrOpBlock;
 src:PsrRegNode;
 p:PspirvOp;
 n:Boolean;
begin
 Result:=0;
 if (node=nil) then Exit;

 pBlock:=pLine^.Parent;
 src:=RegDown(node);
 n:=false;

 repeat
  if src^.is_const then Exit;

  tmp:=pBlock^.FindUpCond(src);
  if (tmp<>nil) then
  begin
   if (tmp=pBlock) and (pLine^.OpId=Op.OpBranchConditional) then Exit;

   node:=NewReg_b(n xor tmp^.Cond.FVal,@pLine);
   Exit(1);
  end;

  p:=src^.pWriter^.AsType(ntOp);
  if (p=nil) then Exit;

  Case p^.OpId of
   Op.OpLogicalNot:;
   Op.OpNot:;
   else
    Exit;
  end;

  src:=p^.ParamFirst^.AsReg;
  if (src=nil) then Exit;
  src:=RegDown(src);
  n:=not n;

 until false;
end;

function TSprvEmit_post.RegCollapse(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (node^.dtype=dtUnknow) then Exit;

 old:=node;
 node:=RegDown(old);

 if (old<>node) then //is change?
 begin
  if (node^.dtype=dtUnknow) or CompareType(node^.dtype,old^.dtype) then
  begin
   Inc(Result);
  end else
  begin //save to another step
   old^.pWriter:=node;
   node:=old;
  end;
 end;
end;

function TSprvEmit_post.RegVResolve(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;

 old:=node;

 repeat
  node:=RegDown(old);
  if node^.pWriter^.IsType(ntVolatile) then
  begin
   //create load/store
   //use forward only
   PrivateList.PrepVolatile(pLine,node);
   Inc(Result);
  end else
  begin
   Break;
  end;
 until false;

 if (old<>node) then //is change?
 begin
  if (node^.dtype=dtUnknow) or (node^.dtype=old^.dtype) then
  begin
   Inc(Result);
  end else
  begin //save to another step
   old^.pWriter:=node;
   node:=old;
  end;
 end;
end;

function GetDepType(src:PsrRegNode):TsrDataType;
var
 node:PRegDNode;
 pReg:PsrRegNode;
 pLine:PspirvOp;

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
 node:=src^.FirstDep;
 While (node<>nil) do
 begin
  if node^.pNode^.IsType(ntReg) then
  begin
   pReg:=node^.pNode^.AsType(ntReg);
   if (pReg<>nil) then
   begin
    if CmpType(Result,pReg^.dtype) then Exit;
   end;
  end else
  if node^.pNode^.IsType(ntOp) then
  begin
   pLine:=node^.pNode^.AsType(ntOp);
   Case pLine^.OpId of

    Op.OpStore:
     begin
      //ignore?
     end;

    else
     begin
      if CmpType(Result,src^.dtype) then Exit;
     end;
   end;
  end;
  node:=node^.pNext;
 end;
end;

function ResolveWeak(new:PsrRegNode):Integer;
var
 dtype:TsrDataType;
begin
 Result:=0;
 While (new<>nil) do
 begin

  if new^.Weak then
  begin
   dtype:=GetDepType(new);
   if (dtype<>dtUnknow) then
   begin
    if (new^.dtype<>dtype) then
    begin
     new^.dtype:=dtype;
     new^.Weak :=False;
     Inc(Result);
    end else
    begin
     new^.Weak :=False;
    end;
   end;
  end;

  new:=new^.AsReg; //next
 end;
end;

function TSprvEmit_post.RegWResolve(pLine:PspirvOp;var node:PsrRegNode):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 Result:=Result+ResolveWeak(node);
end;

function TSprvEmit_post.RegTypecast(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;

 old:=node;
 node:=RegDown(old);

 if (old<>node) then //is change?
 begin
  if (node^.dtype=dtUnknow) or (node^.dtype=old^.dtype) then
  begin
   Inc(Result);
  end else
  begin //bitcast
   node:=BitcastList.FetchCast(old^.dtype,node);
   Inc(Result);
  end;
 end;
end;

function TSprvEmit_post.RegSTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
begin
 Result:=0;
 if (node^.dtype=dtBool) then Exit;
 dst:=pLine^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 dtype:=dst^.dtype;
 if (dtype<>node^.dtype) then
 begin
  node:=BitcastList.FetchCast(dtype,node); //strict type
  Inc(Result);
 end;
end;

function TSprvEmit_post.RegVTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 dtype:TsrDataType;
 dst:PsrRegNode;
begin
 Result:=0;
 dst:=pLine^.pDst^.AsType(ntReg);
 if (dst=nil) then Exit;

 dtype:=dst^.dtype.Child;
 if (dtype<>node^.dtype) then
 begin
  node:=BitcastList.FetchCast(dtype,node); //strict type
  Inc(Result);
 end;
end;

{
function TSprvEmit_post.NodeOpSameOp(node:PSpirvOp):Integer;
var
 tmp:PspirvOp;
 dst,src:PsrRegNode;
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

function TSprvEmit_post.NodeOpStrict(node:PSpirvOp):Integer;
begin
 Result:=0;
 if not node^.pDst^.IsType(ntReg) then Exit; //is reg

 Case node^.OpId of
  Op.OpBitFieldUExtract  ,
  Op.OpSelect            :Result:=EnumLineRegs(@RegSTStrict,node);
  Op.OpIAddCarry         ,
  Op.OpISubBorrow        ,
  Op.OpCompositeConstruct:Result:=EnumLineRegs(@RegVTStrict,node);
  else;
 end;

end;

function TSprvEmit_post.OnOpStep1(node:PSpirvOp):Integer; //backward
begin
 Result:=0;

 if node^.is_cleared then Exit;

 if node^.can_clear then
 begin
  if node^.Clear then Inc(Result);
 end else
 begin
  //Result:=Result+NodeOpSameOp(node);
  Result:=Result+EnumLineRegs(@RegCollapse,node);
  Result:=Result+EnumLineRegs(@RegFindCond,node);
 end;
end;

function TSprvEmit_post.OnOpStep2(node:PSpirvOp):Integer; //forward
begin
 Result:=0;

 if node^.is_cleared then Exit;

 if node^.can_clear then
 begin
  if node^.Clear then Inc(Result);
 end else
 begin
  Result:=Result+TEmitPostOp(TObject(Self)).PostForward1(node);
 end;
end;

function TSprvEmit_post.OnOpStep3(node:PSpirvOp):Integer; //forward
begin
 Result:=0;

 if node^.is_cleared then Exit;

 if node^.can_clear then
 begin
  if node^.Clear then Inc(Result);
 end else
 begin
  Result:=Result+TEmitPostOp(TObject(Self)).PostForward2(node);
 end;
end;

function TSprvEmit_post.OnOpStep4(node:PSpirvOp):Integer; //forward
begin
 Result:=0;

 if node^.is_cleared then Exit;

 if node^.can_clear then
 begin
  if node^.Clear then Inc(Result);
 end else
 begin
  Result:=Result+EnumLineRegs(@RegVResolve,node);
 end;
end;

function TSprvEmit_post.OnOpStep5(node:PSpirvOp):Integer; //backward
begin
 Result:=0;

 if node^.is_cleared then Exit;

 if node^.can_clear then
 begin
  if node^.Clear then Inc(Result);
 end else
 begin
  Result:=Result+EnumLineRegs(@RegWResolve,node);
 end;
end;

function TSprvEmit_post.OnOpStep6(node:PSpirvOp):Integer; //backward
begin
 Result:=0;

 if node^.is_cleared then Exit;

 if node^.can_clear then
 begin
  if node^.Clear then Inc(Result);
 end else
 begin
  Result:=Result+EnumLineRegs(@RegTypecast,node);
  Result:=Result+NodeOpStrict(node);
  Result:=Result+EnumLineRegs(@RegFindCond,node);
 end;
end;

function TSprvEmit_post.OnOpStep7(node:PSpirvOp):Integer; //backward
begin
 Result:=0;

 if node^.is_cleared then
 begin
  Assert(node^.read_count=0);
  node^.Remove;
  Inc(Result);
 end else
 if node^.can_clear then
 begin
  node^.Clear;
  node^.Remove;
  Inc(Result);
 end;
end;

function TSprvEmit_post.PostFuncAnalize:Integer;
var
 pFunc:PSpirvFunc;
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

  repeat //OnOpStep5

   repeat //OnOpStep3

    repeat //OnOpStep2

     repeat //OnOpStep1
      i:=EnumBlockOpBackward(@OnOpStep1,pFunc^.pTop); //OnOpStep1 Reg Collapse
      if (i=0) then Break;
      Result:=Result+i;
     until false;

     i:=EnumBlockOpForward(@OnOpStep2,pFunc^.pTop); //OnOpStep2 PostForward1
     if (i=0) then Break;
     Result:=Result+i;
    until false;

    i:=EnumBlockOpForward(@OnOpStep3,pFunc^.pTop); //OnOpStep3 PostForward2
    if (i=0) then Break;
    Result:=Result+i;
   until false;

   if data_layout then
   begin
    Result:=Result+PostDataLayoutAnalize1;
    data_layout:=false;
   end;

   repeat //OnOpStep4 Volatile Reslove
    i:=EnumBlockOpForward(@OnOpStep4,pFunc^.pTop);
    if (i=0) then Break;
    Result:=Result+i;
   until false;

   r4:=0;
   repeat //OnOpStep5 Weak Reslove
    i:=EnumBlockOpBackward(@OnOpStep5,pFunc^.pTop);
    if (i=0) then Break;
    r4:=r4+i;
   until false;

   if (r4=0) then Break;
   Result:=Result+r4;
  until false;

  PrivateList.RemoveAllStore;

  data_layout:=(Main=pFunc);
  if data_layout then
  begin
   Result:=Result+PostDataLayoutAnalize2;
  end;

  //UpdateRegType OpLoad/OpStore
  DataLayoutList.EnumChain(@OnChainUpdate);
  PrivateList.Post;

  repeat //OnOpStep6 Typecast
   i:=EnumBlockOpBackward(@OnOpStep6,pFunc^.pTop);
   if (i=0) then Break;
   Result:=Result+i;
  until false;

  Result:=Result+EnumBlockOpBackward(@OnOpStep7,pFunc^.pTop); //OnOpStep7 Remove Lines

  pFunc:=pFunc^.Prev;
 end;
end;

function TSprvEmit_post.OnChainUpdate(node:PsrChain):Integer;
begin
 Result:=0;
 node^.UpdateRegType;
end;

function TSprvEmit_post.PostDataLayoutAnalize1:Integer;
begin
 Result:=0;

 DataLayoutList.AllocID;

 Result:=Result+DataLayoutList.EnumChain(@OnChainField);
end;

function TSprvEmit_post.PostDataLayoutAnalize2:Integer;
begin
 Result:=0;

 BufferList.ApplyBufferType;
 BufferList.AlignOffset;
 BufferList.FillSpace;
 BufferList.AllocID;
 BufferList.EnumAllField(@OnFieldType);

 Result:=Result+DataLayoutList.EnumChain(@OnChainAlloc);
end;

function TSprvEmit_post.FetchField(var pChain:PsrChain;dtype:TsrDataType):PsrField;
var
 buf:PsrBuffer;
 F:TFieldFetchValue;
 _offset,_stride:PtrUint;
 _count:PtrUint;
 //ext:TsrChainExt;
 //pNew:PsrChain;
 fset_index:Boolean;
begin
 _count:=0;
 _stride:=0;

 if (pChain^.pIndex<>nil) then
 begin
  //RuntimeArray

  buf:=BufferList.Fetch(pChain^.parent,0);
  repeat

   _offset:=pChain^.offset;
   F:=buf^.FTop.FetchRuntimeArray(_offset,pChain^.stride);
   repeat

    Case F.fValue of
     frNotFit       :Break;
     frIdent        :Exit(F.pField);
     frVectorAsValue:Exit(F.pField);
     frValueInVector:_offset:=_offset-F.pField^.offset;
     frValueInArray :_offset:=_offset-F.pField^.offset;
    end;
    F:=F.pField^.FetchValue(_offset,pChain^.size,dtype);

   until (F.fValue<>frNotFit);
   if (F.fValue<>frNotFit) then Break;

   buf:=BufferList.NextAlias(buf);
  until false;

 end else
 begin
  //Value/Vector

  buf:=BufferList.Fetch(pChain^.parent,0);
  fset_index:=False;
  repeat

   _offset:=pChain^.offset;
   F.pField:=@buf^.FTop;
   repeat

    F:=F.pField^.FetchValue(_offset,pChain^.size,dtype);
    Case F.fValue of
     frNotFit       :Break;
     frIdent,
     frVectorAsValue:
     begin
      if fset_index then
      begin
       Break;
       {
       Assert(pChain^.key.ext.pIndex=nil);

       ext:=Default(TsrChainExt);
       ext.pIndex:=NewReg_i(dtUint32,_count);
       ext.stride:=_stride;
       pNew:=pChain^.FParent^.Fetch(_offset,pChain^.key.size,@ext);

       pNew^.read_count :=pChain^.read_count ;
       pNew^.write_count:=pChain^.write_count;
       pNew^.rSlot      :=pChain^.rSlot      ;

       pChain^.read_count :=0;
       pChain^.write_count:=0;
       pChain^.rSlot      :=Default(TsrRegSlot);

       pChain:=pNew;
       }
      end;
      Exit(F.pField);
     end;
     frValueInVector:_offset:=_offset-F.pField^.offset;
     frValueInArray :
       begin
        Assert(not fset_index);
        _offset:=_offset-F.pField^.offset;
        _stride:=F.pField^.stride;
        _count :=_offset div _stride;
        _offset:=_offset mod _stride;
        fset_index:=true;
       end;
    end;

   until (F.fValue<>frNotFit);
   if (F.fValue<>frNotFit) then Break;

   buf:=BufferList.NextAlias(buf);
   fset_index:=False;
  until false;

 end;

 Result:=F.pField;
end;

function TSprvEmit_post.OnChainField(node:PsrChain):Integer;
var
 pField:PsrField;
 dtype:TsrDataType;
begin
 Result:=1;
 dtype:=node^.dtype;
 //Writeln('OnChainsField:',dtype,':',node^.key.offset);
 pField:=FetchField(node,dtype);
 Assert(pField<>nil);
 node^.pField:=pField;
 if (pField^.dtype<>dtype) then
 begin
  node^.dtype:=pField^.dtype;
 end;
 //pField^.pBuffer^.TakeChain(node);
 node^.pBuffer:=pField^.pBuffer;
end;

procedure TSprvEmit_post.OnFieldType(node:PsrField);
var
 count:PtrUint;

 items:PPsrType;
 sType,pType:PsrType;

 child:PsrField;
begin
 if (node^.pType<>nil) then Exit;

 if (node^.dtype in [dtTypeStruct,dtTypeArray,dtTypeRuntimeArray]) then
 begin
  if node^.IsStructNotUsed then
  begin
   child:=node^.First;
   Assert(child<>nil);
   Assert(child^.pType<>nil);
   sType:=child^.pType;
  end else
  begin
   count:=node^.FCount;
   Assert(count<>0);
   items:=Alloc(SizeOf(Pointer)*count);

   count:=0;
   child:=node^.First;
   While (child<>nil) do
   begin
    Assert(child^.pType<>nil);
    items[count]:=child^.pType;
    Inc(count);
    child:=node^.Next(child);
   end;

   if node^.IsTop then
   begin
    sType:=TypeList.InsertStruct(count,items,False,node^.GetSize); //unique
   end else
   begin
    sType:=TypeList.FetchStruct (count,items,False,node^.GetSize);
   end;

  end;

  Case node^.dtype of
   dtTypeArray:
     begin
      count:=node^.GetSize div node^.stride;
      pType:=TypeList.FetchArray(sType,count);
     end;
   dtTypeRuntimeArray:
     begin
      pType:=TypeList.FetchRuntimeArray(sType);
     end;
   else
     begin
      pType:=sType;
     end;
  end;

  node^.pType:=pType;

  if node^.IsTop then
  begin
   //Alloc Type Var
   node^.pBuffer^.pType:=pType;
  end;

 end else
 begin
  node^.pType:=TypeList.Fetch(node^.dtype);
 end;

end;

function TSprvEmit_post.OnChainAlloc(node:PsrChain):Integer;
var
 pLine:PspirvOp;
 pIndex:PsrRegNode;
 pReg:PsrRegNode;
 pField:PsrField;
 Parent:PsrField;
 src:PsrVariable;
 pParam:POpParamNode;
 //dtype:TsrDataType;
begin
 Result:=1;

 pIndex:=RegDown(node^.pIndex);
 if (pIndex=nil) or (pIndex^.is_const) then
 begin
  pLine:=init_line;
 end else
 begin
  pLine:=node^.FirstLine;
  Assert(pLine<>nil);
  pLine:=pLine^.Prev;
  Assert(pLine<>nil);
 end;
 pField:=node^.pField;
 Assert(pField<>nil);
 src:=pField^.pBuffer^.pVar;
 Assert(src<>nil);
 pLine:=OpAccessChain(pLine,pField^.pType,node,src);

 //dtype:=node^.GetRegType;
 //if (pField^.dtype<>dtype) then
 //begin
 // Writeln(pField^.dtype,'<>',dtype);
 // Assert(false,'TODO');
 //end;

 pParam:=pLine^.ParamLast;
 repeat

  Parent:=pField^.pParent;
  if (Parent<>nil) then
   Case Parent^.dtype of
    dtTypeStruct:
      begin
       pReg:=NewReg_i(dtUint32,pField^.FID,@pLine);
       pLine^.AddParamAfter(pParam,pReg);
      end;
    dtTypeArray,
    dtTypeRuntimeArray:
      begin
       if not Parent^.IsStructNotUsed then
       begin
        pReg:=NewReg_i(dtUint32,pField^.FID,@pLine);
        pLine^.AddParamAfter(pParam,pReg);
       end;
       Assert(pIndex<>nil);
       pLine^.AddParamAfter(pParam,pIndex);
      end;
    else
     if Parent^.dtype.isVector then
     begin
      pReg:=NewReg_i(dtUint32,pField^.FID,@pLine);
      pLine^.AddParamAfter(pParam,pReg);
     end;
   end;

  pField:=Parent;
 until (pField=nil);

end;

function TSprvEmit_post.PostConstAnalize:Integer;
var
 node:PsrConst;
begin
 Result:=0;
 node:=ConstList.FList.pTail;
 While (node<>nil) do
 begin
  if (not node^.IsUsed) then
  begin
   ConstList.FList.Remove(node); //remove?
   Inc(Result);
  end;
  node:=node^.Prev;
 end;
end;

function TSprvEmit_post.PostVariableAnalize:Integer;
var
 node:PsrVariable;
begin
 Result:=0;
 node:=VariableList.FList.pTail;
 While (node<>nil) do
 begin
  if (not node^.IsUsed) then
  begin
   VariableList.FList.Remove(node); //remove?
   Inc(Result);
  end else
  begin
   node^.UpdateType(Self);
  end;
  node:=node^.Prev;
 end;
end;

function TSprvEmit_post.PostTypeAnalize:Integer;
var
 node:PsrType;
begin
 Result:=0;
 node:=TypeList.FList.pTail;
 While (node<>nil) do
 begin
  if (not node^.IsUsed) then
  begin
   TypeList.FList.Remove(node); //remove?
   Inc(Result);
  end;
  node:=node^.Prev;
 end;
end;

end.

