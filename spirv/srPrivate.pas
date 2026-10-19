unit srPrivate;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 spirv,
 ginodes,
 srNode,
 srType,
 srTypes,
 srLayout,
 srOp,
 srOpUtils,
 srReg,
 srVariable,
 srConst,
 srRefId,
 srBitcast,
 srCFGParser;

type
 TsrPrivate=class;

 TsrPrivate=class(TsrDescriptor)
  public
   pPrev,pNext :TsrPrivate;
   pLeft,pRight:TsrPrivate;
   class function c(n1,n2:PPsrRegSlot):Integer; static;
  private
   //
   FPrivId:Integer;
   key:PsrRegSlot;
   //
   FLineList:TDependenceNodeList;
   //
   Procedure SetRegType(rtype:TsrDataType);
   function  GetRegType:TsrDataType;
  public
   //
   Procedure _PrepType(node:PPrepTypeNode); override;
   function  _GetStorageName:RawByteString; override;
   //
   property  dtype:TsrDataType read GetRegType write SetRegType;
   property  Source:PsrRegSlot read key;
   Procedure Init; inline;
   function  GetStorageName:RawByteString;
   function  adjust_type(new:TsrDataType):TsrDataType;
   Procedure UpdateRegType;
   Procedure PrepType(new:TsrDataType);
   Procedure SortLines;
   Procedure Optimize;
   procedure AddLine(pLine:TspirvOp);
   procedure AddLine(pLine:TspirvOp;node:TDependenceNode);
   procedure FetchLoad (pLine:TspirvOp;dst:TsrRegNode);
   Procedure FetchStore(pLine:TspirvOp;src:TsrRegNode);
 end;

 ntPrivate=TsrPrivate;

 PsrPrivateList=^TsrPrivateList;
 TsrPrivateList=object
  type
   TPrivList=specialize TNodeListClass<TsrPrivate>;
  var
   FEmit:TCustomEmit;
   FPrivList:TPrivList;
   FPrivId:Integer;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(pSource:PsrRegSlot):TsrPrivate;
  function  FetchCustom(dtype:TsrDataType):TsrPrivate;
  function  FetchArray(dtype:TsrDataType;array_count:DWORD):TsrPrivate;
  function  First:TsrPrivate; inline;
  //
  procedure build_slot_reset(pSlot:PsrRegSlot;var orig:TsrRegNode);
  procedure build_slot_endif(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_break(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_bloop(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_conti(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_volatile_reset(orig:TsrRegsSnapshot);
  procedure build_volatile_endif(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
  procedure build_volatile_break(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
  procedure build_volatile_bloop(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
  procedure build_volatile_conti(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
  procedure make_copy_slot(pSlot:PsrRegSlot);
  procedure make_copy_all;
  function  PrepPhi(pLine:TspirvOp):Integer;
  Procedure Post;
  procedure AllocEntryPoint(EntryPoint:TSpirvOp);
 end;

implementation

uses
 srInterface;

//

Procedure TsrPrivate._PrepType(node:PPrepTypeNode);
begin
 PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

function TsrPrivate._GetStorageName:RawByteString;
begin
 Result:=GetStorageName;
end;

//

class function TsrPrivate.c(n1,n2:PPsrRegSlot):Integer;
begin
 Result:=ord(n1^>n2^)-ord(n1^<n2^);
end;

Procedure TsrPrivate.Init; inline;
begin
 FStorage:=StorageClass.Private_;
 FBinding:=-1;
end;

function TsrPrivate.GetStorageName:RawByteString;
begin
 if (Source=nil) then
 begin
  Result:='h'+IntToStr(FPrivId);
 end else
 begin
  Result:='v'+Source^.Name+'_'+IntToStr(FPrivId);
 end;
end;

function TsrPrivate.adjust_type(new:TsrDataType):TsrDataType;
begin
 if (Self=nil) then Exit(new);

 if (Source<>nil) then
 begin
  if (Source^.isBoolOnly) then
  begin
   Exit(dtBool);
  end;
 end;

 if (FType<>nil) then
 begin
  Exit(FType.dtype);
 end;

 Result:=new;
end;

Procedure TsrPrivate.SetRegType(rtype:TsrDataType);
var
 pTypeList:PsrTypeList;
begin
 pTypeList:=Emit.GetTypeList;
 Ftype:=pTypeList^.Fetch(rtype);

 UpdateRegType;
end;

function TsrPrivate.GetRegType:TsrDataType;
begin
 Result:=FType.dtype;
end;

Procedure TsrPrivate.UpdateRegType;
var
 pBitcastList:PsrBitcastList;
 node:TDependenceNode;
 pLine:TspirvOp;
 Value:TsrNode;
 dst:TsrRegNode;
 old,rtype:TsrDataType;
 ConstList:PsrConstList;
 cst:TsrConst;
begin
 rtype:=FType.dtype;

 pBitcastList:=Emit.GetBitcastList;

 node:=FLineList.pHead;
 While (node<>nil) do
 begin
  pLine:=node.pNode;

  Case pLine.OpId of

   Op.OpLoad:
    begin
     Value:=pLine.pDst;
     Value.PrepType(ord(rtype));

     pLine.pType:=Ftype;

     dst:=Value.specialize AsType<ntReg>;
     if (dst<>nil) then
     begin
      old:=dst.dtype;
      if (old<>dtUnknow) and (rtype<>old) then
      begin
       //OpLoad -> new -> dst
       dst:=pBitcastList^.FetchDstr(rtype,dst);
       pLine.pDst:=dst;
      end;
     end;
    end;

   Op.OpStore:
    begin
     Value:=pLine.ParamNode(1).Value;
     Value.PrepType(ord(rtype));

     dst:=Value.specialize AsType<ntReg>;
     if (dst<>nil) then
     begin
      //
      if dst.is_const then
      begin
       cst:=dst.pWriter;
       if (dst.dtype<>cst.dtype) then
       begin
        ConstList:=Emit.GetConstList;
        //
        cst:=ConstList^.Bitcast(dst.dtype,cst);
        dst.pWriter:=cst;
       end;
      end;
      //
      old:=dst.dtype;
      if (old<>dtUnknow) and (rtype<>old) then
      begin
       //OpStore <- new <- dst
       dst:=pBitcastList^.FetchRead(rtype,dst);
       pLine.ParamNode(1).Value:=dst;
      end;
     end;


    end;

   else;
  end;

  node:=node.pNext;
 end;
end;

Procedure TsrPrivate.PrepType(new:TsrDataType);
var
 old:TsrDataType;
begin
 new:=adjust_type(new);
 if (new=dtUnknow) then exit;

 old:=GetRegType;
 if is_unprep_type(old,new,True) then
 begin
  SetRegType(new);
 end;
end;


{
procedure _update_store_line(pLine:TspirvOp);
var
 pReg:TsrRegNode;
 pCur:TspirvOp;
begin
 if (pLine.OpId<>Op.OpStore) then Exit;
 pReg:=RegDown(pLine.ParamNode(1).Value.specialize AsType<ntReg>);
 if (pReg=nil) then Exit;
 pCur:=pReg.pLine;

 if (pLine<>pCur) and (MaxLine(pLine,pCur)=pCur) then //pCur>pLine
 begin
  pLine.Remove;
  pCur.InsertAfter(pLine);
 end;
end;
}

Procedure TsrPrivate.SortLines;
var
 dnode,dnext:TDependenceNode;
 //pLine:array[0..1] of TspirvOp;
 nswp:Boolean;
begin
 //indexing
 dnode:=FLineList.pHead;
 While (dnode<>nil) do
 begin
  dnode.fread_count:=GetGlobalIndex(dnode.pNode);
  dnode:=dnode.pNext;
 end;
 //bubble sort
 repeat
  nswp:=True;
  dnode:=FLineList.pHead;
  While (dnode<>nil) do
  begin
   dnext:=dnode.pNext;
   if (dnext=nil) then Break;

   //pLine[0]:=dnode.pNode;
   //pLine[1]:=dnext.pNode;

   //_update_store_line(pLine[0]);
   //_update_store_line(pLine[1]);

   //if (MaxLine(pLine[0],pLine[1])=pLine[0]) then //dnode>dnext
   if (dnode.fread_count>dnext.fread_count) then
   begin
    //swap
    nswp:=False;
    FLineList.Remove(dnode);
    FLineList.InsertAfter(dnext,dnode);
   end else
   begin
    dnode:=dnext;
   end;

  end;
 until nswp;
end;

Function FindByHalfSpace(node,prev:TspirvOp):Boolean;
begin
 Result:=False;
 while (node<>nil) do
 begin
  if (node=prev) then Exit(True);
  //
  if node.IsType(ntOpBlock) then
  if IsReal(TsrOpBlock(node).bType) then
  begin
   Exit(False);
  end;
  //
  node:=flow_down_prev_up(node);
 end;
end;

Procedure TsrPrivate.Optimize;
var
 dnode,dprev:TDependenceNode;
 pLine:array[0..1] of TspirvOp;
 pRegs:array[0..1] of TsrRegNode;
begin
 //exit;

 dnode:=FLineList.pTail;
 While (dnode<>nil) do
 begin
  dprev:=dnode.pPrev;
  if (dprev=nil) then Break;

  pLine[0]:=dnode.pNode;
  pLine[1]:=dprev.pNode;

  if (pLine[0].Parent=pLine[1].Parent) then
  if FindByHalfSpace(pLine[0],pLine[1]) then
  begin
   //OpStore %v ; pLine[1] ; (dprev)
   //OpStore %v ; pLine[0] ; (dnode)
   if (pLine[0].OpId=Op.OpStore) and (pLine[1].OpId=Op.OpStore) then
   begin
    //Remove dprev
    FLineList.Remove(dprev);
    pLine[1].mark([soNotUsed]);
    Continue;
   end else
   if (pLine[0].OpId=Op.OpStore) and (pLine[1].OpId=Op.OpLoad) then
   begin
    //%r = OpLoad %type %v ; pLine[1] ; (dprev)
    //OpStore %v %r        ; pLine[0] ; (dnode)

    pRegs[0]:=RegDown(pLine[0].ParamNode(1).Value.specialize AsType<ntReg>);
    pRegs[1]:=RegDown(pLine[1].pDst.specialize AsType<ntReg>);

    if (pRegs[0]<>nil) and (pRegs[1]<>nil) and CompareReg(pRegs[0],pRegs[1]) then
    begin
     //Remove dnode
     FLineList.Remove(dnode);
     pLine[0].mark([soNotUsed]);

     dnode:=dprev;
     if (dnode.pNext<>nil) then
     begin
      dnode:=dnode.pNext;
     end;

     Continue;
    end;
   end else
   if (pLine[0].OpId=Op.OpLoad) and (pLine[1].OpId=Op.OpLoad) then
   begin
    //%r1 = OpLoad %type %v ; pLine[1] ; (dprev)
    //%r2 = OpLoad %type %v ; pLine[0] ; (dnode)

    pRegs[0]:=pLine[0].pDst.specialize AsType<ntReg>;
    pRegs[1]:=pLine[1].pDst.specialize AsType<ntReg>;

    if (pRegs[0]<>nil) and (pRegs[1]<>nil) then
    begin
     pRegs[0].pWriter:=pRegs[1];

     //Remove dnode
     FLineList.Remove(dnode);

     dnode:=dprev;
     if (dnode.pNext<>nil) then
     begin
      dnode:=dnode.pNext;
     end;

     Continue;
    end;

   end;

  end;

  dnode:=dprev;
 end;
end;

procedure TsrPrivate.AddLine(pLine:TspirvOp);
var
 node:TDependenceNode;
begin
 node:=Emit.NewDependence;
 node.pNode:=pLine;
 FLineList.Push_tail(node);
end;

procedure TsrPrivate.AddLine(pLine:TspirvOp;node:TDependenceNode);
begin
 //hack reuse
 node.pNode:=pLine;
 FLineList.Push_tail(node);
end;

procedure TsrPrivate.FetchLoad(pLine:TspirvOp;dst:TsrRegNode);
begin
 Assert(dst<>nil);

 pLine:=Emit.OpLoad(pLine,FType,dst,FVar);

 AddLine(pLine);
end;

Procedure TsrPrivate.FetchStore(pLine:TspirvOp;src:TsrRegNode);
begin
 if (src=nil) then Exit;

 pLine:=Emit.OpStore(pLine,FVar,src);

 AddLine(pLine);
end;

//

procedure TsrPrivateList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrPrivateList.Fetch(pSource:PsrRegSlot):TsrPrivate;
begin
 Assert(pSource<>nil);
 //
 Result:=FEmit.specialize New<TsrPrivate>;
 Result.Init;
 Result.FPrivId:=FPrivId; Inc(FPrivId);
 Result.key:=pSource;
 //
 FPrivList.Push_tail(Result);
end;

function TsrPrivateList.FetchCustom(dtype:TsrDataType):TsrPrivate;
var
 pTypeList:PsrTypeList;
begin
 Result:=FEmit.specialize New<TsrPrivate>;
 Result.Init;
 Result.FPrivId:=FPrivId; Inc(FPrivId);
 //
 if (dtype<>dtUnknow) then
 begin
  pTypeList:=FEmit.GetTypeList;
  Result.Ftype:=pTypeList^.Fetch(dtype);
 end;
 //
 FPrivList.Push_tail(Result);
end;

function TsrPrivateList.FetchArray(dtype:TsrDataType;array_count:DWORD):TsrPrivate;
var
 pTypeList:PsrTypeList;
begin
 Result:=FEmit.specialize New<TsrPrivate>;
 Result.Init;
 Result.InitVar; ////
 Result.FPrivId:=FPrivId; Inc(FPrivId);
 //
 if (dtype<>dtUnknow) then
 begin
  pTypeList:=FEmit.GetTypeList;
  Result.Ftype:=pTypeList^.Fetch(dtype);
  Result.Ftype:=pTypeList^.FetchArray(Result.Ftype,array_count);
 end;
 //
 FPrivList.Push_tail(Result);
end;

Function TsrPrivateList.First:TsrPrivate;
begin
 Result:=FPrivList.pHead;
end;

procedure TsrPrivateList.build_slot_reset(pSlot:PsrRegSlot;var orig:TsrRegNode);
begin
 pSlot^.current:=orig;
end;

type
 TsrTypeSelector=record
  main:TsrDataType;
  bool:TsrDataType;
 end;

function nBool(t:TsrDataType):TsrDataType; inline;
begin
 if (t=dtBool) then
  Result:=dtUnknow
 else
  Result:=t;
end;

function oBool(t:TsrDataType):TsrDataType; inline;
begin
 if (t=dtBool) then
  Result:=t
 else
  Result:=dtUnknow;
end;

operator := (i:TsrDataType):TsrTypeSelector;
begin
 Result.main:=nBool(i);
 Result.bool:=oBool(i);
end;

operator + (a,b:TsrTypeSelector):TsrTypeSelector;
begin
 Result.main:=LazyType2(nBool(a.main),nBool(b.main));
 Result.bool:=LazyType2(oBool(a.bool),oBool(b.bool));
end;

operator := (s:TsrTypeSelector):TsrDataType;
begin
 Result:=LazyType2(s.main,s.bool);
end;

function FindPredecessor(node:TSpirvOp):TSpirvOp;
begin
 Result:=nil;
 if (node=nil) then Exit;

 While (node<>nil) do
 begin
  if node.IsType(ntOp) then
  if (not node.is_cleared) and (not node.is_notused) then
  if (node.OpId=Op.OpLabel) then
  begin
   Exit(node);
  end;

  node:=flow_down_prev_up(node);
 end;

end;

procedure TsrPrivateList.build_slot_endif(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pPhi     :TSpirvOp;
 org,prv  :TsrRegNode;
 cur      :TsrRegNode;
 rtype    :TsrTypeSelector;
 cur_c_org:Boolean;
 cur_c_prv:Boolean;
begin
 org:=RegDownSlot(orig);
 prv:=RegDownSlot(prev);
 cur:=RegDownSlot(pSlot^.current);

 cur_c_org:=CompareReg(cur,org);
 cur_c_prv:=CompareReg(cur,prv);

 if cur_c_org and cur_c_prv then
 begin
  Exit;
 end;

 org:=orig;
 prv:=prev;
 cur:=pSlot^.current;

 pPhi:=TEmitInterface(FEmit).NewSpirvOp(Op.OpPhi);

 {
 [orig]
  if (eval)
  (

  [prev]
  ) else (

  ) fork orig->prev/curr -> next
 [next]
 }

 rtype:=dtUnknow;

 //hack: if value is not init, set current
 if (cur_c_prv) then
 if (org=nil) then
 begin
  org:=prv;
 end;

 if (org<>nil) then
 if (cur_c_prv) then //Do not add orig if prev changed
 begin

  //hack
  if org.IsType(TsrVectorArray) then
  begin
   org:=TsrVectorArray(org).FLanes[0];
  end;

  pPhi.AddParam(org);
  pPhi.AddParam(ctx.pPrdLabel);
 end;
 //
 if (prv<>nil) then
 if (not cur_c_prv) then //Do not add prev If the value has not changed
 begin

  //hack
  if prv.IsType(TsrVectorArray) then
  begin
   prv:=TsrVectorArray(prv).FLanes[0];
  end;

  pPhi.AddParam(prv);
  pPhi.AddParam(ctx.pPrvLabel);

  rtype:=rtype+prv.dtype;
 end;
 //
 if (cur<>nil) then
 begin

  //hack
  if cur.IsType(TsrVectorArray) then
  begin
   cur:=TsrVectorArray(cur).FLanes[0];
  end;

  pPhi.AddParam(cur);
  pPhi.AddParam(ctx.pCurLabel);

  rtype:=rtype+cur.dtype;
 end;
 //
 if (org<>nil) then
 if (cur_c_org<>cur_c_prv)  then //Do not add orig if both values are assigned
 begin
  //lower prio of type
  rtype:=rtype+org.dtype;
 end;

 //replace next
 _next:=pSlot^._New(rtype,pPhi); //<-The slot value will not be updated

 pPhi.pDst:=_next;

 TSpirvOp(ctx.pMrgOp).InsertAfter(pPhi);

 //fixed line
 _next.CustomLine:=pPhi;
end;

function _is_parent(pNode:TSpirvOp;pLabel:TsrNode):Boolean;
var
 pValue :POpParamNode;
 pParent:POpParamNode;
begin
 Result:=False;

 pValue:=pNode.ParamFirst;
 while (pValue<>nil) do
 begin
  pParent:=pValue.Next;
  if (pParent=nil) then Exit;

  if (pParent.Value=pLabel) then
  begin
   Exit(True);
  end;

  pValue:=pParent.Next;
 end;

end;

procedure TsrPrivateList.build_slot_break(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine    :TSpirvOp;
 pPhi     :TSpirvOp;
 cur,nxt  :TsrRegNode;
 new_vol  :Boolean;
 rtype    :TsrTypeSelector;
begin
 cur:=RegDownSlot(pSlot^.current);
 nxt:=RegDownSlot(_next);

 if CompareReg(cur,nxt) then
 begin
  Exit;
 end;

 cur:=pSlot^.current;
 nxt:=_next;

 {
 [orig]
 while () do
 [prev]
 (

  if (eval)
  (
   break; --\
  )         |
            |
 )<---------/
 [next]
 }

 pPhi:=nil;

 //get prev
 if (_next<>nil) then
 if _next.pWriter.IsType(ntOp) then
 begin
  pLine:=_next.pWriter;
  if (pLine.OpId=Op.OpPhi) then
  if (TSpirvOp(ctx.pMrgOp).Parent=pLine.Parent) then
  begin
   pPhi:=pLine;
  end;
 end;

 if (pPhi=nil) then
 begin
  pPhi:=TEmitInterface(FEmit).NewSpirvOp(Op.OpPhi);
  new_vol:=True;
 end else
 begin
  new_vol:=False;
 end;

 rtype:=dtUnknow;

 //
 if (cur<>nil) then
 begin

  //hack
  if cur.IsType(TsrVectorArray) then
  begin
   cur:=TsrVectorArray(cur).FLanes[0];
  end;

  pPhi.AddParam(cur);
  pPhi.AddParam(TSpirvOp(ctx.pAftOp).pDst);

  rtype:=rtype+cur.dtype;
 end;

 //save Volatile
 if new_vol then
 begin

  //replace next
  _next:=pSlot^._New(rtype,pPhi); //<-The slot value will not be updated

  pPhi.pDst:=_next;

  TSpirvOp(ctx.pMrgOp).InsertAfter(pPhi);

  //fixed line
  _next.CustomLine:=pPhi;
 end;

 if (_next<>nil) then
 begin
  //update pref type
  if (TsrDataType(rtype)<>dtUnknow) then
  if (_next.dtype=dtUnknow) then
  begin
   _next.PrepType(ord(TsrDataType(rtype)));
  end;
 end;

end;

procedure TsrPrivateList.build_slot_bloop(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine:TSpirvOp;
 pPhi :TSpirvOp;
 node :TDependenceNode;
begin
 pPhi:=nil;

 //get prev
 if (_next<>nil) then
 if _next.pWriter.IsType(ntOp) then
 begin
  pLine:=_next.pWriter;
  if (pLine.OpId=Op.OpPhi) then
  if (TSpirvOp(ctx.pMrgOp).Parent=pLine.Parent) then
  begin
   pPhi:=pLine;
  end;
 end;

 if (pPhi<>nil) and (orig<>nil) then
 begin
  node:=ctx.FBreaks.FList.pHead;
  while (node<>nil) do
  begin
   pLine:=FindPredecessor(node.pNode);

   if not _is_parent(pPhi,pLine.pDst) then
   begin
    pPhi.AddParam(orig);
    pPhi.AddParam(pLine.pDst);
   end;

   node:=node.pNext;
  end;
 end;

end;

procedure TsrPrivateList.build_slot_conti(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine    :TSpirvOp;
 pPhi     :TSpirvOp;
 cur,prv  :TsrRegNode;
 new_vol  :Boolean;
 rtype    :TsrTypeSelector;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(prev);

 if CompareReg(cur,prv) then
 begin
  Exit;
 end;

 cur:=pSlot^.current;
 prv:=prev;

 {
 [orig]
 while () do
 [prev]
 (<------------\
               |
  if (eval)    |
  (            |
   continue; --/
  )

 )
 [next]
 }

 pPhi:=nil;

 //get prev
 if (prev<>nil) then
 if prev.pWriter.IsType(ntOp) then
 begin
  pLine:=prev.pWriter;
  if (pLine.OpId=Op.OpPhi) then
  if (TSpirvOp(ctx.pPrvOp).Parent=pLine.Parent) then
  begin
   pPhi:=pLine;
  end;
 end;

 if (pPhi=nil) then
 begin
  pPhi:=TEmitInterface(FEmit).NewSpirvOp(Op.OpPhi);
  new_vol:=True;
 end else
 begin
  new_vol:=False;
 end;

 rtype:=dtUnknow;

 if (prv<>nil) then
 begin
  if new_vol then
  begin

   if (prv<>orig) then
   begin
    Assert(prv.pWriter=orig,'unknow loopback state');

    //need to use orig to create a loopback dependency
    prv:=orig;

    //save if new created

    pPhi.AddParam(prv);
    pPhi.AddParam(ctx.pPrdLabel);
   end;

  end;
  rtype:=rtype+prv.dtype;
 end;

 //
 if (cur<>nil) then
 begin
  pPhi.AddParam(cur);
  pPhi.AddParam(TSpirvOp(ctx.pAftOp).pDst); //continue

  rtype:=rtype+cur.dtype;
 end;

 //save Volatile
 if new_vol then
 begin

  if (prev=nil) then
  begin
   //The input register is not defined, should it be initialized to make_copy_slot?
   prev:=pSlot^._New(rtype,pPhi); //<-The slot value will not be updated
  end;

  //set backedge dependence

  pPhi.pDst:=prev;

  TSpirvOp(ctx.pPrvOp).InsertAfter(pPhi);

  //fixed line
  prev.CustomLine:=pPhi;
 end;

 if (prev<>nil) then
 begin
  //update pref type
  if (TsrDataType(rtype)<>dtUnknow) then
  if (prev.dtype=dtUnknow) then
  begin
   prev.PrepType(ord(TsrDataType(rtype)));
  end;
 end;

end;

procedure TsrPrivateList.build_volatile_reset(orig:TsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_reset,orig);
end;

procedure TsrPrivateList.build_volatile_endif(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_endif,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.build_volatile_break(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_break,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.build_volatile_bloop(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_bloop,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.build_volatile_conti(var ctx:TsrVolatileContext;orig,prev,_next:TsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_conti,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.make_copy_slot(pSlot:PsrRegSlot);
var
 p:Word;

 procedure _copy(pSlot:PsrRegSlot);
 var
  cur,node:TsrRegNode;
  pLine:TspirvOp;
 begin
  cur:=pSlot^.current;
  if (cur<>nil) then
  begin
   pLine:=FEmit.curr_line;

   node:=pSlot^.New(cur.dtype,pLine);
   node.pWriter:=cur;

   pLine:=FEmit.PostLink(pLine,node); //post processing

   //fixed line
   node.CustomLine:=pLine;
  end;
 end;

begin
 if (pSlot^.Lanes(0)<>nil) and
    (pSlot^.Category=cVectorArray) then
 begin
  //fill lanes
  For p:=0 to LaneCount-1 do
  begin
   _copy(pSlot^.Lanes(p));
  end;
  //
 end else
 begin
  _copy(pSlot);
 end;
end;

procedure TsrPrivateList.make_copy_all;
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSlot(@make_copy_slot);
end;

function get_load_from(r:TsrRegNode):TsrVariable;
var
 pOp:TspirvOp;
begin
 Result:=nil;
 pOp:=r.pWriter.specialize AsType<ntOp>;
 if (pOp=nil) then Exit;
 if (pOp.OpId<>Op.OpLoad) then Exit;
 Result:=pOp.ParamFirst.Value.specialize AsType<ntVariable>;
end;

function calc_most_used_type(dst:TspirvOp):TsrDataType;
type
 t_table=array[TsrDataType] of Integer;
var
 pValue :POpParamNode;
 pParent:POpParamNode;
 reg    :TsrRegNode;
 Table:t_table;
 Count:Integer;
 i,max:TsrDataType;
begin
 //
 Table:=Default(t_table);
 Count:=0;
 //
 pValue:=dst.ParamFirst;
 while (pValue<>nil) do
 begin
  pParent:=pValue.Next;
  if (pParent=nil) then Exit;

  reg:=pValue.AsReg;

  if (reg<>nil) then
  begin
   if (reg.dtype=dtUnknow) then
   begin
    reg:=RegDown(reg);
   end;

   Inc(Table[reg.dtype]);
   Inc(Count);
  end;

  pValue:=pParent.Next;
 end;
 //
 if (Count=0) then
 begin
  Result:=dtUnknow;
 end else
 if (Count=Table[dtBool]) then
 begin
  //Result:=dtBool;
  Result:=dtUint32;
 end else
 begin
  max:=dtUnknow;
  For i:=Low(TsrDataType) to High(TsrDataType) do
   if (i<>dtBool) then
   begin
    if ((max=dtUnknow) and (Table[i]<>0)) or
       (Table[i]>Table[max]) then
    begin
     max:=i;
    end;
   end;
  Result:=max;
 end;
end;

function TsrPrivateList.PrepPhi(pLine:TspirvOp):Integer; //use forward only
var
 pBitcastList:PsrBitcastList;
 dtype:TsrDataType;
 pValue:POpParamNode;
 pParent:POpParamNode;
 dst,src,fval:TsrRegNode;
 fone:Boolean;
 pLabel:TsrOpLabel;
begin
 Result:=0;
 if (pLine=nil) then Exit;

 dst:=pLine.pDst;
 if (dst=nil) then Exit;

 if dst.pSlot^.isBoolOnly then
 begin
  dtype:=dtBool;
 end else
 begin
  dtype:=calc_most_used_type(pLine);
  dtype:=lazyType2(dtype,StoreType(dst.dtype));
  dtype:=lazyType2(dtype,dtFloat32);
 end;

 if (dtype=dtUnknow) then Exit;

 pBitcastList:=FEmit.GetBitcastList;

 if (dst.dtype=dtUnknow) then
 begin
  dst.PrepType(ord(dtype));
  Inc(Result);
 end else
 if (dst.dtype<>dtype) then
 begin
  dst:=pBitcastList^.FetchDstr(dtype,dst);
  pLine.pDst:=dst;
  Inc(Result);
 end;

 pValue:=pLine.ParamFirst;

 fval:=RegDown(pValue.Value);
 fone:=True;

 while (pValue<>nil) do
 begin
  pParent:=pValue.Next;
  if (pParent=nil) then Exit;

  src:=pValue.AsReg;
  if (src<>nil) then
  begin
   if (src.dtype=dtUnknow) then
   begin
    src.PrepType(ord(dtype));
    Inc(Result);
   end else
   if (src.dtype<>dtype) then
   begin
    src:=pBitcastList^.FetchRead(dtype,src);
    pValue.Value:=src;
    Inc(Result);
   end;
  end;

  pLabel:=pParent.Value.specialize AsType<TsrOpLabel>;

  repeat

   if (pLabel<>nil) then
   if (pLabel.line<>nil) then
   if TspirvOp(pLabel.line).is_notused or
      TspirvOp(pLabel.line).is_cleared then
   begin
    pLabel:=TsrOpLabel(pLabel.pred);
    pParent.Value:=pLabel;
    Inc(Result);
    Continue;
   end;

   Break;
  until false;

  fone:=fone and CompareReg(fval,RegDown(pValue.Value));
  //
  pValue:=pParent.Next;
 end;

 //simplification
 if fone then
 begin
  dst.pWriter:=fval;
  dst.PrepType(ord(fval.dtype));
  pLine.mark([soNotUsed,soForce]);
  Inc(Result);
 end;

end;

Procedure TsrPrivateList.Post;
var
 node:TsrPrivate;
begin
 node:=FPrivList.pHead;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   node.SortLines;
   node.Optimize;
   node.UpdateRegType;
  end;
  node:=node.pNext;
 end;
end;

procedure TsrPrivateList.AllocEntryPoint(EntryPoint:TSpirvOp);
var
 node:TsrPrivate;
 pVar:TsrVariable;
begin
 node:=FPrivList.pHead;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) then
  if node.IsUsed then
  begin
   EntryPoint.AddParam(pVar);
  end;
  node:=node.pNext;
 end;
end;


end.


