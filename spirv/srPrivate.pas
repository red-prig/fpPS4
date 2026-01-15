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
 srBitcast,
 srCFGParser;

type
 TsrPrivate=class;

 TStoreNode=class(TsrNode)
  pPrev,pNext:TStoreNode;
  //
  src :TsrRegNode;
  //line:TspirvOp;
  //
 end;
 TStoreNodeList=specialize TNodeListClass<TStoreNode>;

 TsrVolatile=class(TsrNode)
  var
   pPrev,pNext:TsrVolatile;
   FPrivate :TsrPrivate;
   FSlot    :PsrRegSlot;
   FList    :TStoreNodeList;
   FBase    :TsrRegNode;
   FZeroRead:Boolean;
   ForceBool:Boolean;
  //
  Procedure _zero_read;                    override;
  Procedure _zero_unread;                  override;
  Procedure _PrepType(node:PPrepTypeNode); override;
  //
  function  ListCount:Integer;
  Procedure AddStore(src:TsrRegNode);
  Procedure PushStore(node:TStoreNode);
  Function  PopStore:TStoreNode;
  Procedure RemoveAllStore;
  procedure PrepType(new:TsrDataType);
 end;

 ntVolatile=TsrVolatile;

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
   TVoltList=specialize TNodeListClass<TsrVolatile>;
  var
   FEmit:TCustomEmit;
   FPrivList:TPrivList;
   FVoltList:TVoltList;
   FPrivId:Integer;
  procedure Init(Emit:TCustomEmit); inline;
  function  NewVolatile(Slot:PsrRegSlot):TsrVolatile;
  function  Fetch(pSource:PsrRegSlot):TsrPrivate;
  function  FetchCustom(dtype:TsrDataType):TsrPrivate;
  function  FetchArray(dtype:TsrDataType;array_count:DWORD):TsrPrivate;
  function  First:TsrPrivate; inline;
  //
  procedure build_slot_reset(pSlot:PsrRegSlot;var orig:TsrRegNode);
  procedure build_slot_ctrue(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_endif(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_break(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_conti(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_volatile_reset(orig:PsrRegsSnapshot);
  procedure build_volatile_ctrue(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
  procedure build_volatile_endif(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
  procedure build_volatile_break(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
  procedure build_volatile_conti(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
  procedure make_copy_slot(pSlot:PsrRegSlot);
  procedure make_copy_all;
  function  PrepVolatile(dst:TspirvOp;src:TsrRegNode):TsrRegNode;
  Procedure RemoveAllStore;
  Procedure Post;
  procedure AllocEntryPoint(EntryPoint:TSpirvOp);
 end;

implementation

Procedure TsrVolatile._zero_read;
var
 node:TStoreNode;
begin
 if FZeroRead then Exit;
 //
 node:=FList.pHead;
 While (node<>nil) do
 begin
  node.src.mark_read(Self);
  node:=node.pNext;
 end;
 //
 FZeroRead:=True;
end;

Procedure TsrVolatile._zero_unread;
var
 node:TStoreNode;
begin
 if not FZeroRead then Exit;
 //
 node:=FList.pHead;
 While (node<>nil) do
 begin
  node.src.mark_unread(Self);
  node:=node.pNext;
 end;
 //
 FZeroRead:=False;
end;

Procedure TsrVolatile._PrepType(node:PPrepTypeNode);
begin
 PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

//

procedure TsrVolatile.PrepType(new:TsrDataType);
var
 node:TStoreNode;
begin
 new:=FPrivate.adjust_type(new);
 if (new=dtUnknow) then exit;

 node:=FList.pHead;
 While (node<>nil) do
 begin

  if node.src.pWriter.IsType(TsrVolatile) then Break;

  node.src.PrepType(ord(new));
  node:=node.pNext;
 end;
end;

function TsrVolatile.ListCount:Integer;
var
 node:TStoreNode;
begin
 Result:=0;
 //
 node:=FList.pHead;
 While (node<>nil) do
 begin
  Inc(Result);
  //
  node:=node.pNext;
 end;
end;

Procedure TsrVolatile.AddStore(src:TsrRegNode);
var
 node:TStoreNode;
 pLine:TspirvOp;
begin
 if (src=nil) then Exit;

 if (src.IsType(TsrVectorArray)) then
 begin
  //Assert(false,'AddStore:TsrVectorArray');
 end else
 begin
  Assert(src.pLine<>nil);
  pLine:=src.pLine;
  Assert(pLine<>nil);
 end;

 node:=Emit.specialize New<TStoreNode>; //cache in free list?
 node.src :=src;

 //node.line:=pLine;

 if FZeroRead then
 begin
  src.mark_read(Self);
 end;
 FList.Push_head(node);
end;

Procedure TsrVolatile.PushStore(node:TStoreNode);
begin
 if (node=nil) then Exit;
 //Assert(node.src.pLine<>nil);
 //Assert(node.line<>nil);
 if FZeroRead then
 begin
  node.src.mark_read(Self);
 end;
 FList.Push_head(node);
end;

Function TsrVolatile.PopStore:TStoreNode;
begin
 Result:=FList.Pop_head;

 if (Result<>nil) then
 if FZeroRead then
 begin
  Result.src.mark_unread(Self);
 end;
end;

Procedure TsrVolatile.RemoveAllStore;
var
 node:TStoreNode;
begin
 repeat
  node:=PopStore;
 until (node=nil);
end;

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
 node:=NewDependence;
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

function TsrPrivateList.NewVolatile(Slot:PsrRegSlot):TsrVolatile;
begin
 Result:=FEmit.specialize New<TsrVolatile>;
 Result.FSlot:=Slot;
 FVoltList.Push_tail(Result);
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

procedure TsrPrivateList.build_slot_ctrue(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
begin
 //restore
 if (_next<>nil) then
 if (RegDownSlot(_next)<>RegDownSlot(prev)) then
 if _next.pWriter.IsType(ntVolatile) then
 begin
  _next.pWriter:=prev;
 end else
 begin
  Assert(false,'build_slot_ctrue');
 end;
end;

procedure TsrPrivateList.build_slot_endif(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine    :TSpirvOp;
 org,prv  :TsrRegNode;
 cur      :TsrRegNode;
 pVolatile:TsrVolatile;
 rtype    :TsrDataType;
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

 pVolatile:=NewVolatile(pSlot);

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

 if (org<>nil) then
 if (cur_c_org<>cur_c_prv)  then //Do not add orig if both values are assigned
 begin
  pVolatile.AddStore(org);
  rtype:=LazyType2(rtype,org.dtype);
 end;
 //
 if (prv<>nil) then
 if (not cur_c_prv) then //Do not add prev If the value has not changed
 begin
  pVolatile.AddStore(prv);
  rtype:=LazyType2(rtype,prv.dtype);
 end;
 //
 if (cur<>nil) then
 begin
  pVolatile.AddStore(cur);
  pVolatile.FBase:=cur;
  rtype:=LazyType2(rtype,cur.dtype);
 end;

 pLine:=ctx.after;

 //replace next
 _next:=pSlot^._New(rtype,pLine); //<-The slot value will not be updated
 _next.pWriter:=pVolatile;

 ctx.AddVolatile(pVolatile,_next);

 pLine.AddParam(_next); //post processing

 //fixed line
 _next.CustomLine:=pLine;
end;

procedure TsrPrivateList.build_slot_break(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine    :TSpirvOp;
 cur,nxt  :TsrRegNode;
 pVolatile:TsrVolatile;
 new_vol  :Boolean;
 rtype    :TsrDataType;
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

 pVolatile:=nil;

 //check Volatile
 if (pVolatile=nil) then
 begin
  pVolatile:=NewVolatile(pSlot);
  new_vol:=True;
 end else
 begin
  new_vol:=False;
 end;

 rtype:=dtUnknow;

 if (nxt<>nil) then
 begin
  if new_vol then
  begin
   //save if new created
   pVolatile.AddStore(nxt);
  end;
  rtype:=LazyType2(rtype,nxt.dtype);
 end;
 //
 if (cur<>nil) then
 begin
  pVolatile.AddStore(cur);
  pVolatile.FBase:=cur;
  rtype:=LazyType2(rtype,cur.dtype);
 end;

 //save Volatile
 if new_vol then
 begin
  pLine:=ctx.after;

  //replace next
  _next:=pSlot^._New(rtype,pLine); //<-The slot value will not be updated
  _next.pWriter:=pVolatile;

  ctx.AddVolatile(pVolatile,_next);

  pLine.AddParam(_next); //post processing

  //fixed line
  _next.CustomLine:=pLine;
 end;

 if (_next<>nil) then
 begin
  //update pref type
  if (rtype<>dtUnknow) then
  if (_next.dtype=dtUnknow) then
  begin
   _next.PrepType(ord(rtype));
  end;
 end;

end;

procedure TsrPrivateList.build_slot_conti(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine    :TSpirvOp;
 cur,prv  :TsrRegNode;
 pVolatile:TsrVolatile;
 new_vol  :Boolean;
 rtype    :TsrDataType;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(prev);

 //Assert(CompareReg(RegDownSlot(orig),RegDownSlot(prev)));

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

 pVolatile:=nil;

 //get Volatile
 if (prv<>nil) then
 begin
  if prv.pWriter.IsType(ntVolatile) then
  begin //prev is volatile
   pVolatile:=prv.pWriter.specialize AsType<ntVolatile>;

   if (pVolatile.FSlot<>pSlot) then
   begin
    //Reset if it is another register
    pVolatile:=nil;
   end;

  end;
 end;

 //check Volatile
 if (pVolatile=nil) then
 begin
  pVolatile:=NewVolatile(pSlot);
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
    pVolatile.AddStore(prv);
   end;

  end;
  rtype:=LazyType2(rtype,prv.dtype);
 end;

 {
 if new_vol then
 begin
  //use orig
  if (orig<>nil) then
  begin
   //save if new created
   pVolatile.AddStore(orig);
   rtype:=LazyType2(rtype,orig.dtype);
  end;
 end else
 begin
  //use prev
  if (prv<>nil) then
  begin
   rtype:=LazyType2(rtype,prv.dtype);
  end;
 end;
 }
 //
 if (cur<>nil) then
 begin
  pVolatile.AddStore(cur);
  pVolatile.FBase:=cur;
  rtype:=LazyType2(rtype,cur.dtype);
 end;

 //save Volatile
 if new_vol then
 begin
  pLine:=ctx.befor;

  if (prev=nil) then
  begin
   //The input register is not defined, should it be initialized to make_copy_slot?
   prev:=pSlot^._New(rtype,pLine); //<-The slot value will not be updated
  end;

  //set backedge dependence
  prev.pWriter:=pVolatile;

  ctx.AddVolatile(pVolatile,prev);

  pLine.AddParam(prev); //post processing

  //fixed line
  prev.CustomLine:=pLine;
 end;

 if (prev<>nil) then
 begin
  //update pref type
  if (rtype<>dtUnknow) then
  if (prev.dtype=dtUnknow) then
  begin
   prev.PrepType(ord(rtype));
  end;
 end;

end;

procedure TsrPrivateList.build_volatile_reset(orig:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_reset,orig);
end;

procedure TsrPrivateList.build_volatile_ctrue(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_ctrue,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.build_volatile_endif(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 //NextVolatileID; FVolatileID:SizeUint;
 pRegsStory^.ForEachSnap(@build_slot_endif,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.build_volatile_break(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 //NextVolatileID; FVolatileID:SizeUint;
 pRegsStory^.ForEachSnap(@build_slot_break,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.build_volatile_conti(var ctx:TsrVolatileContext;orig,prev,_next:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 pRegsStory:=FEmit.GetRegsStory;
 //NextVolatileID; FVolatileID:SizeUint;
 pRegsStory^.ForEachSnap(@build_slot_conti,ctx,orig,prev,_next);
end;

procedure TsrPrivateList.make_copy_slot(pSlot:PsrRegSlot);
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

function get_load_from2(r:TsrRegNode):TsrVolatile;
begin
 Result:=r.pWriter.specialize AsType<ntVolatile>;
end;

procedure _Move(dst,src:TsrVolatile);
var
 node:TStoreNode;
begin
 node:=src.PopStore;
 While (node<>nil) do
 begin
  dst.PushStore(node);
  node:=src.PopStore;
 end;
end;

function calc_most_used_type(V:TsrVolatile):TsrDataType;
type
 t_table=array[TsrDataType] of Integer;
var
 node :TStoreNode;
 Table:t_table;
 Count:Integer;
 i,max:TsrDataType;
begin
 if V.ForceBool then
 begin
  Exit(dtBool);
 end;
 //
 Table:=Default(t_table);
 Count:=0;
 //
 node:=V.FList.pHead;
 While (node<>nil) do
 begin
  Inc(Table[RegDown(node.src).dtype]);
  Inc(Count);
  //
  node:=node.pNext;
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

function TsrPrivateList.PrepVolatile(dst:TspirvOp;src:TsrRegNode):TsrRegNode; //use forward only
var
 tmp:TsrRegNode;
 pPrivate :TsrPrivate;
 pVolatile:TsrVolatile;
 node:TStoreNode;
 pLine:TspirvOp;
 //pTmp:TspirvOp;
 //vtmp:TsrVolatile;
 dtype:TsrDataType;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (not src.pWriter.IsType(ntVolatile)) then Exit;

 //move to prev
 Assert(dst<>nil);
 dst:=flow_prev_up(dst);
 Assert(dst<>nil);
 up_merge_line(dst);
 Assert(dst<>nil);

 pVolatile:=src.pWriter.specialize AsType<ntVolatile>;

 if pVolatile.FSlot^.iUnattach then
 begin
  pPrivate:=pVolatile.FPrivate;
 end else
 begin
  pPrivate:=TsrPrivate(pVolatile.FSlot^.FPrivate);
 end;

 if (pPrivate=nil) then
 begin
  pPrivate:=Fetch(pVolatile.FSlot);

  if pVolatile.FSlot^.iUnattach then
  begin
   pVolatile.FPrivate:=pPrivate;
  end else
  begin
   pVolatile.FSlot^.FPrivate:=pPrivate;
  end;

 end;

 dtype:=calc_most_used_type(pVolatile);
 dtype:=lazyType2(dtype,StoreType(src.dtype));
 dtype:=lazyType2(dtype,dtFloat32);

 if (dtype=dtBool) and (pVolatile.FSlot^.Name='S0') then
 begin
  calc_most_used_type(pVolatile);
 end;

 pPrivate.InitVar();

 pPrivate .PrepType(dtype);

 pVolatile.PrepType(dtype);

 if (pPrivate.GetRegType=dtUnknow) then
 begin
  Assert(false);
 end;

 node:=pVolatile.PopStore;
 While (node<>nil) do
 begin

  tmp:=RegDown(node.src);

  if (tmp.IsType(TsrVectorArray)) then
  begin
   Assert(false,'AddStore:TsrVectorArray');
  end;

  if (src<>tmp) {and (pPrivate^.pVar<>get_load_from(tmp))} then
  begin

   //pLine:=TsrRegNode(node.pNode).pLine;
   //pLine:=node.line;
   pLine:=node.src.pLine;
   Assert(pLine<>nil);

   up_merge_line(pLine);

   {///
   if (tmp^.pWriter^.IsType(ntVolatile)) then
   begin
    vtmp:=tmp^.pWriter^.AsType(ntVolatile);
    Assert(vtmp<>nil);

    _Move(pVolatile,vtmp);

    tmp^.pWriter:=src;

    Continue;
   end;
   }///

   //up_merge_line(pLine);

   pPrivate.FetchStore(pLine,node.src);
  end;

  node:=pVolatile.PopStore;
 end;

 //ntVolatile -> src -> next
 //Opload     -> new

 Result:=src.pSlot^._New(src.dtype,dst); //<-The slot value will not be updated

 pPrivate.FetchLoad(dst,Result); //before reg
end;

procedure TsrPrivateList.RemoveAllStore;
var
 node:TsrVolatile;
begin
 node:=FVoltList.pHead;
 While (node<>nil) do
 begin
  node.RemoveAllStore;
  node:=node.pNext;
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


