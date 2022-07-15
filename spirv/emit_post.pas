unit emit_post;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  srNodes,
  srTypes,
  srConst,
  srReg,
  srVariable,
  srVolatile,
  srLayout,
  srBuffer,
  srOp,
  srOpUtils,
  SprvEmit,
  emit_op;

type
 TPostCb=function(node:PSpirvOp):Integer of object;
 TRegsCb=function(pLine:PspirvOp;var node:PsrRegNode):Integer of object;
 TChainCb=function(node:PsrChain):Integer of object;

 TSprvEmit_post=object(TEmitOp)
  function  Post:Integer;

  function  PrepTypeNode(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
  function  PrepTypeDst(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
  function  PrepTypeParam(node:POpParamNode;rtype:TsrDataType;relax:Boolean=true):Integer;

  function  RegFindCond(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegCollapse({%H-}pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegTypecast(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegSTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;
  function  RegVTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;

  function  NodeOpSameOp(node:PSpirvOp):Integer;
  function  NodeOpStrict(node:PSpirvOp):Integer;

  function  OnOpStep1(node:PSpirvOp):Integer; //backward
  function  OnOpStep2(node:PSpirvOp):Integer; //forward
  function  OnOpStep3(node:PSpirvOp):Integer; //forward
  function  OnOpStep4(node:PSpirvOp):Integer; //forward
  function  OnOpStep5(node:PSpirvOp):Integer; //backward

  function  PostFuncAnalize:Integer;

  function  EnumChain(p:PsrDataLayout;cb:TChainCb):Integer;
  function  EnumChain(cb:TChainCb):Integer;

  function  PostDataLayoutAnalize:Integer;
  function  FetchField(var pChain:PsrChain;dtype:TsrDataType):PsrField;
  Procedure SetLoadType(pChain:PsrChain;rtype:TsrDataType);
  function  OnChainField(node:PsrChain):Integer;
  function  OnChainAlloc(node:PsrChain):Integer;
  procedure OnFieldType(node:PsrField);
  procedure AllocBufferVar(node:PsrBuffer);
  function  PostConstAnalize:Integer;
  function  PostVariableAnalize:Integer;
  function  PostTypeAnalize(first:Boolean):Integer;
 end;

function  DstIsClear(const node:TOpParamSingle):Boolean;
function  EnumLineRegs(cb:TRegsCb;pLine:PSpirvOp):Integer;
function  EnumBlockOpForward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
function  EnumBlockOpBackward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
procedure RegUnmark(node:PsrRegNode);
Procedure NodeParamClear(node:POpParamNode);
function  NodeOpClear(p:PSpirvOp):Integer;

implementation

uses
 emit_post_op;

function TSprvEmit_post.Post:Integer;
begin
 Result:=0;
 Result:=Result+PostFuncAnalize;
 Result:=Result+PostConstAnalize;
 Result:=Result+PostTypeAnalize(True);   //1
 Result:=Result+PostVariableAnalize;
 Result:=Result+PostTypeAnalize(False);  //2

end;

function TSprvEmit_post.PrepTypeNode(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
var
 old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;

 if (node^.dtype=dtUnknow) then
 begin
  RegPrepType(node,rtype);
  Inc(Result);
 end else
 begin
  Case relax of
   True :relax:=CompareType(node^.dtype,rtype);
   False:relax:=(node^.dtype=rtype);
  end;
  if not relax then
  begin
   old:=node;
   node:=FBitcast.FetchRead(rtype,old);
   RegUnmark(old);
   Inc(Result);
  end;
 end;
end;

function TSprvEmit_post.PrepTypeDst(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
var
 old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;

 if (node^.dtype=dtUnknow) then
 begin
  RegPrepType(node,rtype);
  Inc(Result);
 end else
 begin
  Case relax of
   True :relax:=CompareType(node^.dtype,rtype);
   False:relax:=(node^.dtype=rtype);
  end;
  if not relax then
  begin
   old:=node;
   node:=FBitcast.FetchDst(rtype,old);
   Inc(Result);
  end;
 end;

end;

function TSprvEmit_post.PrepTypeParam(node:POpParamNode;rtype:TsrDataType;relax:Boolean=true):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;
 if (node^.ntype<>ntReg) then Exit;
 Result:=PrepTypeNode(node^.pData,rtype,relax);
end;

function DstIsClear(const node:TOpParamSingle):Boolean;
var
 pVar:PsrVariable;
 pReg:PsrRegNode;
 pFunc:PSpirvFunc;
 pChain:PsrChain;
begin
 Result:=False;
 Case node.ntype of
  ntUnknow:;//dst not used
  ntBlock:;
  ntRefId:;
  ntVar:
    begin
     pVar:=node.pData;
     Result:=(pVar^.read_count=0) and (pVar^.write_count=0);
    end;
  ntReg:
    begin
     pReg:=node.pData;
     Result:=(pReg^.read_count=0);
    end;
  ntFunc:
    begin
     pFunc:=node.pData;
     Result:=(pFunc^.read_count=0);
    end;
  ntChain:
    begin
     pChain:=node.pData;
     Result:=(pChain^.read_count=0) and (pChain^.write_count=0);
    end;
  else
   Assert(false,'DstIsClear');
 end;
end;

function EnumLineRegs(cb:TRegsCb;pLine:PSpirvOp):Integer;
var
 node:POpParamNode;
begin
 Result:=0;
 if (cb=nil) or (pLine=nil) then Exit;
 node:=pLine^.pParam.pHead;
 While (node<>nil) do
 begin
  Case node^.ntype of
   ntReg:
    begin
     Result:=Result+cb(pLine,node^.pData);
    end
   else;
  end;
  node:=node^.pNext;
 end;
end;

function EnumBlockOpForward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
var
 node:PSpirvOp;
begin
 Result:=0;
 if (pBlock=nil) or (cb=nil) then Exit;
 node:=pBlock^.pHead;
 repeat
  While (node<>nil) do
  begin
   Case node^.OpId of
    OpBlock:
      begin
       pBlock:=node^.dst.AsBlock; //down
       if (pBlock<>nil) then
       begin
        pBlock^.pUpLine:=node;
        node:=pBlock^.pHead;
        Continue;
       end;
      end;
    else
      begin
       Result:=Result+cb(node);
      end;
   end;
   node:=node^.pNext;
  end;
  node:=pBlock^.pUpLine;   //prev
  if (node=nil) then Break;
  pBlock:=node^.pParent;   //up
  if (pBlock=nil) then Break;
  node:=node^.pNext;
 until false;
end;

function EnumBlockOpBackward(cb:TPostCb;pBlock:PsrOpBlock):Integer;
var
 node:PSpirvOp;
begin
 Result:=0;
 if (pBlock=nil) or (cb=nil) then Exit;
 node:=pBlock^.pTail;
 repeat
  While (node<>nil) do
  begin
   Case node^.OpId of
    OpBlock:
      begin
       pBlock:=node^.dst.AsBlock; //down
       if (pBlock<>nil) then
       begin
        pBlock^.pUpLine:=node;
        node:=pBlock^.pTail;
        Continue;
       end;
      end;
    else
      begin
       Result:=Result+cb(node);
      end;
   end;
   node:=node^.pPrev;
  end;
  node:=pBlock^.pUpLine;   //prev
  if (node=nil) then Break;
  pBlock:=node^.pParent;   //up
  if (pBlock=nil) then Break;
  node:=node^.pPrev;
 until false;
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

 pBlock:=pLine^.pParent;
 src:=RegDown(node);
 n:=false;

 repeat
  if src^.is_const then Exit;

  tmp:=pBlock^.FindUpCond(src);
  if (tmp<>nil) then
  begin
   if (tmp=pBlock) and (pLine^.OpId=Op.OpBranchConditional) then Exit;
   RegUnmark(node);
   node:=FetchReg(FConsts.Fetchb(n xor tmp^.Cond.FVal));
   Exit(1);
  end;

  p:=src^.AsOp;
  if (p=nil) then Exit;

  Case p^.OpId of
   Op.OpLogicalNot:;
   Op.OpNot:;
   else
    Exit;
  end;

  src:=p^.ParamNode(0)^.AsReg;
  if (src=nil) then Exit;
  src:=RegDown(src);
  n:=not n;

 until false;
end;

function TSprvEmit_post.RegCollapse(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 new,old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 //backtrace
 old:=node;
 While (true) do
 begin
  Case old^.pWriter.ntype of
    ntReg:
     begin
      new:=old^.pWriter.pData; //next
      old^.mark_unread;
      if (old^.read_count<>0) then
      begin
       new^.mark_read;
      end;
      old:=new;
     end;
   else
    Break;
  end;
 end;

 if (old<>node) then //is change?
 begin
  if (node^.dtype=dtUnknow) or CompareType(node^.dtype,old^.dtype) then
  begin
   node:=old; //set new
   Inc(Result);
  end else
  begin //save to another step
   node^.mark_read;
   node^.SetReg(old);
  end;
 end;
end;

function TSprvEmit_post.RegTypecast(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 new,old:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 //backtrace
 old:=node;
 While (true) do
 begin
  Case old^.pWriter.ntype of
    ntReg:
     begin
      new:=old^.pWriter.pData; //next
      old^.mark_unread;
      if (old^.read_count<>0) then
      begin
       new^.mark_read;
      end;
      old:=new;
     end;
    ntVolatile:
     begin //create load/store
      TEmitVolatile(Self).PrepVolatile(pLine,old);
      Inc(Result);
     end
   else
    Break;
  end;
 end;

 if (old<>node) then //is change?
 begin
  if (node^.dtype=dtUnknow) or (node^.dtype=old^.dtype) then
  begin
   node:=old; //set new
   Inc(Result);
  end else
  begin //bitcast
   old^.mark_unread;
   node:=FBitcast.FetchCast(node^.dtype,old);
   Inc(Result);
  end;
 end;
end;

function TSprvEmit_post.RegSTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 dtype:TsrDataType;
 dst,old:PsrRegNode;
begin
 Result:=0;
 if (node^.dtype=dtBool) then Exit;
 dst:=pLine^.dst.AsReg;
 if (dst=nil) then Exit;

 {pLine:=node^.pLine;
 if (pLine^.dst_type^.key.dtype<>node^.dtype) then
 begin
  Writeln(Op.GetStr(pLine^.OpId),' ',pLine^.dst_type^.key.dtype,'<>',node^.dtype);
  Assert(false);
 end;}

 dtype:=dst^.dtype;
 if (dtype<>node^.dtype) then
 begin
  old:=node;
  node:=FBitcast.FetchCast(dtype,old); //strict type
  RegUnmark(old);
  Inc(Result);
 end;
end;

function TSprvEmit_post.RegVTStrict(pLine:PspirvOp;var node:PsrRegNode):Integer;
var
 dtype:TsrDataType;
 dst,old:PsrRegNode;
begin
 Result:=0;
 dst:=pLine^.dst.AsReg;
 if (dst=nil) then Exit;
 dtype:=GetVecChild(dst^.dtype);
 if (dtype<>node^.dtype) then
 begin
  old:=node;
  node:=FBitcast.FetchCast(dtype,old); //strict type
  RegUnmark(old);
  Inc(Result);
 end;
end;

procedure _RegUnmark(var st_lt:TsrVolatiles;node:PsrRegNode);
var
 n:PsrRegNode;
 pConst:PsrConst;
begin
 node^.mark_unread;
 //backtrace
 While (node^.read_count=0) do
 begin
  Case node^.pWriter.ntype of
    ntReg:
     begin
      n:=node^.pWriter.pData; //next
      n^.mark_unread;
      node:=n;
     end;
    ntVolatile:
     begin
      st_lt.Move_from(TsrVolatiles(node^.pWriter.pData));
      n:=st_lt.pop_reg;
      if (n=nil) then Break;
      n^.mark_unread;
      node:=n;
     end;
    ntConst:
     begin
      pConst:=node^.pWriter.pData;
      pConst^.mark_unread;
      Break;
     end;
    ntOp:Break;
    ntUnknow:Break; //nop
   else
    Assert(false,'_RegUnmark');
  end;
 end;
end;

procedure RegUnmark(node:PsrRegNode);
var
 st_lt:TsrVolatiles;
begin
 st_lt:=Default(TsrVolatiles);
 While (node<>nil) do
 begin
  _RegUnmark(st_lt,node);
  node:=st_lt.pop_reg;
 end;
end;

Procedure NodeParamClear(node:POpParamNode);
begin
 if (node=nil) then Exit;
 Case node^.ntype of
  ntLiteral:;
  ntConst:PsrConst   (node^.pData)^.mark_unread;
  ntType :PsrType    (node^.pData)^.mark_unread;
  ntFunc :PSpirvFunc (node^.pData)^.mark_unread;
  ntVar  :PsrVariable(node^.pData)^.mark_unread;
  ntRefId:;
  ntReg  :RegUnmark  (node^.pData);
  ntChain:PsrChain   (node^.pData)^.mark_unread;
  else
    Assert(false,'NodeParamClear');
 end;
end;

function NodeOpClear(p:PSpirvOp):Integer;
var
 node:POpParamNode;
begin
 Result:=0;
 if (p=nil) then Exit;
 if (p^.dst_type<>nil) then
 begin
  p^.dst_type^.mark_unread;
  p^.dst_type:=nil;
  Inc(Result);
 end;
 node:=p^.pParam.Pop_head;
 While (node<>nil) do
 begin
  NodeParamClear(node);
  node:=p^.pParam.Pop_head;
  Inc(Result);
 end;
end;

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

 src^.mark_read;
 dst^.SetReg(src);

 node^.OpId:=OpLinks; //mark remove
 node^.dst:=Default(TOpParamSingle);

 Result:=1;
end;

function TSprvEmit_post.NodeOpStrict(node:PSpirvOp):Integer;
begin
 Result:=0;
 if (node^.dst.ntype<>ntReg) then Exit; //is reg

 Case node^.OpId of
  Op.OpBitFieldUExtract,
  Op.OpSelect:
    Result:=EnumLineRegs(@RegSTStrict,node);
  Op.OpIAddCarry,
  Op.OpISubBorrow,
  Op.OpCompositeConstruct:
    Result:=EnumLineRegs(@RegVTStrict,node);
  else;
 end;

end;

function TSprvEmit_post.OnOpStep1(node:PSpirvOp):Integer; //backward
begin
 Result:=0;
 Case node^.OpId of
  Op.OpNop:; //ignore
  OpLinks:
    begin
     Result:=Result+NodeOpClear(node); //delete links
     node^.OpId:=Op.OpNop;             //mark remove
     Inc(Result);
    end;
  else
    begin
     if DstIsClear(node^.dst) then
     begin
      Result:=Result+NodeOpClear(node); //delete links
      node^.OpId:=Op.OpNop;             //mark remove
      Inc(Result);
     end else
     begin
      Result:=Result+NodeOpSameOp(node);
      Result:=Result+EnumLineRegs(@RegCollapse,node);
      Result:=Result+EnumLineRegs(@RegFindCond,node);
     end;
    end;
 end;
end;

function TSprvEmit_post.OnOpStep2(node:PSpirvOp):Integer; //forward
begin
 Result:=0;
 Case node^.OpId of
  Op.OpNop:; //ignore
  OpLinks:
    begin
     Result:=Result+NodeOpClear(node); //delete links
     node^.OpId:=Op.OpNop;             //mark remove
     Inc(Result);
    end;
  else
    begin
     if DstIsClear(node^.dst) then
     begin
      Result:=Result+NodeOpClear(node); //delete links
      node^.OpId:=Op.OpNop;             //mark remove
      Inc(Result);
     end else
     begin
      //Result:=Result+EnumLineRegs(@RegCond,node);
      //Result:=Result+EnumLineRegs(@RegCuts,node);
      Result:=Result+TEmitPostOp(Self).PostForward1(node);
     end;
    end;
 end;
end;

function TSprvEmit_post.OnOpStep3(node:PSpirvOp):Integer; //forward
begin
 Result:=0;
 Case node^.OpId of
  Op.OpNop:; //ignore
  OpLinks:
    begin
     Result:=Result+NodeOpClear(node); //delete links
     node^.OpId:=Op.OpNop;             //mark remove
     Inc(Result);
    end;
  else
    begin
     if DstIsClear(node^.dst) then
     begin
      Result:=Result+NodeOpClear(node); //delete links
      node^.OpId:=Op.OpNop;             //mark remove
      Inc(Result);
     end else
     begin
      //Result:=Result+EnumLineRegs(@RegCond,node);
      //Result:=Result+EnumLineRegs(@RegCuts,node);
      Result:=Result+TEmitPostOp(Self).PostForward2(node);
     end;
    end;
 end;
end;

function TSprvEmit_post.OnOpStep4(node:PSpirvOp):Integer; //forward
begin
 Result:=0;
 Case node^.OpId of
  Op.OpNop:; //ignore
  OpLinks:;  //ignore
  else
    begin
     if not DstIsClear(node^.dst) then
     begin
      Result:=Result+EnumLineRegs(@RegTypecast,node);
      Result:=Result+NodeOpStrict(node);
      Result:=Result+EnumLineRegs(@RegFindCond,node);
     end;
    end;
 end;
end;

function TSprvEmit_post.OnOpStep5(node:PSpirvOp):Integer; //backward
begin
 Result:=0;
 Case node^.OpId of
  Op.OpNop,
  OpLinks:
    begin
     Result:=Result+NodeOpClear(node); //delete links
     node^.Remove;                     //remove
     Inc(Result);
    end;
  else
    begin
     if DstIsClear(node^.dst) then
     begin
      Result:=Result+NodeOpClear(node); //delete links
      node^.Remove;                     //remove
      Inc(Result);
     end;
    end;
 end;
end;

function TSprvEmit_post.PostFuncAnalize:Integer;
var
 pFunc:PSpirvFunc;
 data_layout:Boolean;
 i:Integer;
begin
 Result:=0;
 data_layout:=false;
 //backward analize
 pFunc:=FSpirvFuncs.FList.pTail;
 While (pFunc<>nil) do
 begin

  data_layout:=(FMain=pFunc);

  repeat

   repeat

    repeat
     i:=EnumBlockOpBackward(@OnOpStep1,@pFunc^.FTop);
     if (i=0) then Break;
     Result:=Result+i;
    until false;

    i:=EnumBlockOpForward(@OnOpStep2,@pFunc^.FTop);
    if (i=0) then Break;
    Result:=Result+i;
   until false;

   i:=EnumBlockOpForward (@OnOpStep3,@pFunc^.FTop);
   if (i=0) then Break;
   Result:=Result+i;
  until false;

  if data_layout then
  begin
   Result:=Result+PostDataLayoutAnalize;
   data_layout:=false;
  end;

  repeat
   i:=EnumBlockOpForward (@OnOpStep4,@pFunc^.FTop);
   if (i=0) then Break;
   Result:=Result+i;
  until false;

  Result:=Result+EnumBlockOpBackward(@OnOpStep5,@pFunc^.FTop);

  pFunc:=pFunc^.pPrev;
 end;
end;

function TSprvEmit_post.EnumChain(p:PsrDataLayout;cb:TChainCb):Integer;
var
 node:PsrChain;
begin
 Result:=0;
 node:=p^.First;
 While (node<>nil) do
 begin
  if node^.IsUsed then
  begin
   Result:=Result+cb(node);
  end;
  node:=p^.Next(node);
 end;
end;

function TSprvEmit_post.EnumChain(cb:TChainCb):Integer;
var
 node:PsrDataLayout;
begin
 Result:=0;
 if (cb=nil) then Exit;
 node:=FDataLayouts.First;
 While (node<>nil) do
 begin
  Result:=Result+EnumChain(node,cb);
  node:=FDataLayouts.Next(node);
 end;
end;

function TSprvEmit_post.PostDataLayoutAnalize:Integer;
begin
 Result:=0;

 FDataLayouts.AllocID;

 Result:=Result+EnumChain(@OnChainField);

 FBuffers.ApplyBufferType;
 FBuffers.AlignOffset;
 FBuffers.FillSpace;
 FBuffers.AllocID;
 FBuffers.EnumAllField(@OnFieldType);

 Result:=Result+EnumChain(@OnChainAlloc);
end;

function TSprvEmit_post.FetchField(var pChain:PsrChain;dtype:TsrDataType):PsrField;
var
 buf:PsrBuffer;
 F:TFieldFetchValue;
 _offset,_count,_stride:PtrUint;
 ext:TsrChainExt;
 pNew:PsrChain;
 fset_index:Boolean;
begin
 _count:=0;
 _stride:=0;

 if (pChain^.key.ext.pIndex<>nil) then
 begin
  //RuntimeArray

  buf:=FBuffers.Fetch(pChain^.parent,0);
  repeat

   _offset:=pChain^.key.offset;
   F:=buf^.FTop.FetchRuntimeArray(_offset,pChain^.key.ext.stride);
   repeat

    Case F.fValue of
     frNotFit       :Break;
     frIdent        :Exit(F.pField);
     frVectorAsValue:Exit(F.pField);
     frValueInVector:_offset:=_offset-F.pField^.offset;
     frValueInArray :_offset:=_offset-F.pField^.offset;
    end;
    F:=F.pField^.FetchValue(_offset,pChain^.key.size,dtype);

   until (F.fValue<>frNotFit);
   if (F.fValue<>frNotFit) then Break;

   buf:=FBuffers.NextCast(buf);
  until false;

 end else
 begin
  //Value/Vector

  buf:=FBuffers.Fetch(pChain^.parent,0);
  fset_index:=False;
  repeat

   _offset:=pChain^.key.offset;
   F.pField:=@buf^.FTop;
   repeat

    F:=F.pField^.FetchValue(_offset,pChain^.key.size,dtype);
    Case F.fValue of
     frNotFit       :Break;
     frIdent,
     frVectorAsValue:
     begin
      if fset_index then
      begin
       Assert(pChain^.key.ext.pIndex=nil);

       ext:=Default(TsrChainExt);
       ext.pIndex:=FetchReg(FConsts.Fetchi(dtUint32,_count));
       ext.stride:=_stride;
       pNew:=pChain^.parent^.Fetch(_offset,pChain^.key.size,@ext);

       pNew^.read_count :=pChain^.read_count ;
       pNew^.write_count:=pChain^.write_count;
       pNew^.rSlot      :=pChain^.rSlot      ;

       pChain^.read_count :=0;
       pChain^.write_count:=0;
       pChain^.rSlot      :=Default(TsrRegSlot);

       pChain:=pNew;
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

   buf:=FBuffers.NextCast(buf);
   fset_index:=False;
  until false;

 end;

 Result:=F.pField;
end;

Procedure TSprvEmit_post.SetLoadType(pChain:PsrChain;rtype:TsrDataType);
var
 node:PsrRegNode;
 pLine:PspirvOp;
begin
 node:=pChain^.rSlot.pStory.pHead;
 While (node<>nil) do
 begin
  node^.dtype:=rtype;
  pLine:=node^.pWriter.AsOp;
  if (pLine<>nil) then
  if (pLine=node^.pLine) then //is current load
  if (pLine^.OpId=Op.OpLoad) then
  begin
   if (pLine^.dst_type<>nil) then
   begin
    pLine^.dst_type^.mark_unread;
   end;
   pLine^.dst_type:=FSpirvTypes.Fetch(rtype);
  end;
  node:=node^.pNext;
 end;
end;

function TSprvEmit_post.OnChainField(node:PsrChain):Integer;
var
 pField:PsrField;
 dtype:TsrDataType;
begin
 Result:=1;
 dtype:=node^.GetRegType;
 //Writeln('OnChainsField:',dtype,':',node^.key.offset);
 pField:=FetchField(node,dtype);
 node^.pField:=pField;
 if (pField^.dtype<>dtype) then
 begin
  dtype:=pField^.dtype;
  SetLoadType(node,dtype);

  //Writeln(pField^.dtype,'<>',dtype);
  //Assert(false,'TODO');
 end;
 FBuffers.UpdateStorage(node,pField^.pBuffer);
end;

procedure TSprvEmit_post.OnFieldType(node:PsrField);
var
 count:PtrUint;
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
   sType^.mark_read;
  end else
  begin
   count:=node^.FCount;
   Assert(count<>0);
   sType:=FSpirvTypes._New(count);

   count:=0;
   child:=node^.First;
   While (child<>nil) do
   begin
    Assert(child^.pType<>nil);
    child^.pType^.mark_read;
    sType^.SetCompItem(count,child^.pType);
    Inc(count);
    child:=node^.Next(child);
   end;

   sType:=FSpirvTypes.FetchStructNode(sType,count,node);
  end;

  Case node^.dtype of
   dtTypeArray:
     begin
      count:=node^.size div node^.stride;
      pType:=FSpirvTypes.FetchArray(sType,count,node^.stride);
     end;
   dtTypeRuntimeArray:
     begin
      pType:=FSpirvTypes.FetchRuntimeArray(sType,node^.stride);
     end;
   else
     begin
      pType:=sType;
     end;
  end;

  node^.pType:=pType;

  if (node^.parent=nil) then
  begin
   AllocBufferVar(node^.pBuffer);
  end;

 end else
 begin
  node^.pType:=FSpirvTypes.Fetch(node^.dtype);
 end;

end;

procedure TSprvEmit_post.AllocBufferVar(node:PsrBuffer);
var
 pVar:PsrVariable;
 pType:PsrType;
begin
 pType:=node^.FTop.pType;
 Assert(pType<>nil);

 pVar:=node^.pVar;
 if (pVar=nil) then
 begin
  pType^.mark_read;
  pType:=FSpirvTypes.FetchPointer(pType,node^.FStorage);

  pVar:=NewVariable;
  pVar^.dtype:=dtTypeStruct;
  pVar^.pType:=pType;
  pVar^.pSource.SetParam(ntBuffer,node);

  node^.pVar:=pVar;
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

 pIndex:=RegDown(node^.key.ext.pIndex);
 if (pIndex=nil) or (pIndex^.is_const) then
 begin
  pLine:=init_line;
 end else
 begin
  pLine:=node^.rSlot.first^.pLine;
  Assert(pLine<>nil);
  pLine:=pLine^.pPrev;
  Assert(pLine<>nil);
 end;
 pField:=node^.pField;
 Assert(pField<>nil);
 src:=pField^.pBuffer^.pVar;
 Assert(src<>nil);
 src^.mark_read;
 pLine:=TEmitOp(Self).emit_OpAccessChain(pLine,pField^.pType,node,src);

 //dtype:=node^.GetRegType;
 //if (pField^.dtype<>dtype) then
 //begin
 // Writeln(pField^.dtype,'<>',dtype);
 // Assert(false,'TODO');
 //end;

 pParam:=pLine^.pParam.pTail;
 repeat

  Parent:=pField^.parent;
  if (Parent<>nil) then
   Case Parent^.dtype of
    dtTypeStruct:
      begin
       pReg:=FetchReg(FConsts.Fetchi(dtUint32,pField^.FID));
       pLine^.AddParamAfter(pParam,ntReg,pReg);
      end;
    dtTypeArray,
    dtTypeRuntimeArray:
      begin
       if not Parent^.IsStructNotUsed then
       begin
        pReg:=FetchReg(FConsts.Fetchi(dtUint32,pField^.FID));
        pLine^.AddParamAfter(pParam,ntReg,pReg);
       end;
       Assert(pIndex<>nil);
       pIndex^.mark_read;
       pLine^.AddParamAfter(pParam,ntReg,pIndex);
      end;
    else
     if IsVector(Parent^.dtype) then
     begin
      pReg:=FetchReg(FConsts.Fetchi(dtUint32,pField^.FID));
      pLine^.AddParamAfter(pParam,ntReg,pReg);
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
 node:=FConsts.FList.pTail;
 While (node<>nil) do
 begin
  if (node^.read_count=0) then
  begin
   node^.Clear;  //delete links
   FConsts.FList.Remove(node); //remove?
   Inc(Result);
  end else
  if (node^.pType=nil) then
  begin
   node^.pType:=FSpirvTypes.Fetch(node^.key.dtype);
   //Assert(node^.pTypeAlloc<>nil);
  end;
  node:=node^.pPrev;
 end;
end;

function TSprvEmit_post.PostVariableAnalize:Integer;
var
 node:PsrVariable;
 pType:PsrType;
begin
 Result:=0;
 node:=FVariables.pTail;
 While (node<>nil) do
 begin
  if (node^.read_count=0) and (node^.write_count=0) then
  begin
   node^.Clear;  //delete links
   FVariables.Remove(node); //remove?
   Inc(Result);
  end else
  if (node^.pType=nil) then
  begin
   pType:=FSpirvTypes.Fetch(node^.dtype);
   Assert(pType<>nil);
   node^.pType:=FSpirvTypes.FetchPointer(pType,node^.GetStorageClass);
  end;
  node:=node^.pPrev;
 end;
end;

function TSprvEmit_post.PostTypeAnalize(first:Boolean):Integer;
var
 node:PsrType;
begin
 Result:=0;
 node:=FSpirvTypes.FList.pTail;
 While (node<>nil) do
 begin
  if (node^.read_count=0) then
  begin
   node^.Clear;  //delete links
   FSpirvTypes.FList.Remove(node); //remove?
   Inc(Result);
  end else
  begin
   case node^.key.OpId of

    Op.OpTypeFloat:
      begin
       case node^.key.ext.float_size of
        16:AddCap(Capability.Float16);
        64:AddCap(Capability.Float64);
        else;
       end;
      end;

    Op.OpTypeInt:
      begin
       case node^.key.ext.int_size of
         8:AddCap(Capability.Int8);
        16:AddCap(Capability.Int16);
        64:AddCap(Capability.Int64);
        else;
       end;
      end;

    Op.OpTypeImage:
      begin

       Case node^.key.ext.image.Dim of
        Dim.Dim1D:
         Case node^.key.ext.image.Sampled of
          0:;                             //runtime
          1:AddCap(Capability.Sampled1D); //sampling
          2:AddCap(Capability.Image1D);   //read/write
          else;
         end;
        Dim.Buffer:
         Case node^.key.ext.image.Sampled of
          0:;                                 //runtime
          1:AddCap(Capability.SampledBuffer); //sampling
          2:AddCap(Capability.ImageBuffer);   //read/write
          else;
         end;
        else;
       end;

       if (node^.key.ext.image.Sampled=2) and
          (node^.key.ext.image.Arrayed=1) then
       begin
        AddCap(Capability.ImageMSArray);
       end;

       Case node^.key.ext.image.Format of
        ImageFormat.Unknown     :AddCap(Capability.StorageImageReadWithoutFormat);

        ImageFormat.Rg32f       ,
        ImageFormat.Rg16f       ,
        ImageFormat.R11fG11fB10f,
        ImageFormat.R16f        ,
        ImageFormat.Rgba16      ,
        ImageFormat.Rgb10A2     ,
        ImageFormat.Rg16        ,
        ImageFormat.Rg8         ,
        ImageFormat.R16         ,
        ImageFormat.R8          ,
        ImageFormat.Rgba16Snorm ,
        ImageFormat.Rg16Snorm   ,
        ImageFormat.Rg8Snorm    ,
        ImageFormat.R16Snorm    ,
        ImageFormat.R8Snorm     ,
        ImageFormat.Rg32i       ,
        ImageFormat.Rg16i       ,
        ImageFormat.Rg8i        ,
        ImageFormat.R16i        ,
        ImageFormat.R8i         ,
        ImageFormat.Rgb10a2ui   ,
        ImageFormat.Rg32ui      ,
        ImageFormat.Rg16ui      ,
        ImageFormat.Rg8ui       ,
        ImageFormat.R16ui       ,
        ImageFormat.R8ui        :AddCap(Capability.StorageImageExtendedFormats);

        else;
       end;
      end;

    Op.OpTypeArray:
      if not first then
      begin
       //make a const of count
       if (node^.key.ext.array_count=0) then Assert(False);
       FConsts.Fetchi(dtUInt32,node^.key.ext.array_count); //just increment
      end;

    else;
   end;
  end;
  node:=node^.pPrev;
 end;
end;

end.

