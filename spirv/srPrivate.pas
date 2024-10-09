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
 srBitcast;

type
 TsrPrivate=class;

 TsrVolatile=class(TsrNode)
  var
   pPrev,pNext:TsrVolatile;
   FSource:TsrPrivate;
   FList:TDependenceNodeList;
   FBase:TsrRegNode;
   FZeroRead:Boolean;
  //
  Procedure _zero_read;                    override;
  Procedure _zero_unread;                  override;
  Procedure _PrepType(node:PPrepTypeNode); override;
  //
  Procedure Init(Source:TsrPrivate); inline;
  Procedure AddStore(src:TsrRegNode);
  Procedure PushStore(node:TDependenceNode);
  Function  PopStore:TDependenceNode;
  Procedure RemoveAllStore;
  procedure PrepType(new:TsrDataType);
 end;

 ntVolatile=TsrVolatile;

 TsrPrivate=class(TsrDescriptor)
  type
   TVoltList=specialize TNodeListClass<TsrVolatile>;
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
   FVoltList:TVoltList;
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
   Procedure FetchStore(pLine:TspirvOp;node:TDependenceNode);
   function  NewVolatile:TsrVolatile;
   procedure RemoveAllStore;
 end;

 ntPrivate=TsrPrivate;

 PsrPrivateList=^TsrPrivateList;
 TsrPrivateList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrPrivate>;
   TNodeList=specialize TNodeListClass<TsrPrivate>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
   FList:TNodeList;
   FPrivId:Integer;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(pSource:PsrRegSlot):TsrPrivate;
  function  FetchCustom(dtype:TsrDataType):TsrPrivate;
  function  First:TsrPrivate; inline;
  //
  procedure build_slot_reset(pSlot:PsrRegSlot;var orig:TsrRegNode);
  procedure build_slot_ctrue(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_endif(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_break(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_slot_conti(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
  procedure build_test(pSlot:PsrRegSlot);
  procedure build_volatile_test;
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
 end;

implementation

Procedure TsrVolatile._zero_read;
var
 node:TDependenceNode;
begin
 if FZeroRead then Exit;
 //
 node:=FList.pHead;
 While (node<>nil) do
 begin
  TsrRegNode(node.pNode).mark_read(Self);
  node:=node.pNext;
 end;
 //
 FZeroRead:=True;
end;

Procedure TsrVolatile._zero_unread;
var
 node:TDependenceNode;
begin
 if not FZeroRead then Exit;
 //
 node:=FList.pHead;
 While (node<>nil) do
 begin
  TsrRegNode(node.pNode).mark_unread(Self);
  node:=node.pNext;
 end;
 //
 FZeroRead:=False;
end;

Procedure TsrVolatile._PrepType(node:PPrepTypeNode);
begin
 TsrVolatile(node^.dnode).PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

//

Procedure TsrVolatile.Init(Source:TsrPrivate); inline;
begin
 FSource:=Source;
end;

procedure TsrVolatile.PrepType(new:TsrDataType);
var
 node:TDependenceNode;
begin
 new:=FSource.adjust_type(new);
 if (new=dtUnknow) then exit;

 node:=FList.pHead;
 While (node<>nil) do
 begin
  TsrRegNode(node.pNode).PrepType(ord(new));
  node:=node.pNext;
 end;
end;

Procedure TsrVolatile.AddStore(src:TsrRegNode);
var
 node:TDependenceNode;
begin
 if (src=nil) then Exit;
 node:=NewDependence;
 node.pNode:=src;
 if FZeroRead then
 begin
  src.mark_read(Self);
 end;
 FList.Push_head(node);
end;

Procedure TsrVolatile.PushStore(node:TDependenceNode);
begin
 if (node=nil) then Exit;
 if FZeroRead then
 begin
  TsrRegNode(node.pNode).mark_read(Self);
 end;
 FList.Push_head(node);
end;

Function TsrVolatile.PopStore:TDependenceNode;
begin
 Result:=FList.Pop_head;

 if (Result<>nil) then
 if FZeroRead then
 begin
  TsrRegNode(Result.pNode).mark_unread(Self);
 end;
end;

Procedure TsrVolatile.RemoveAllStore;
var
 node:TDependenceNode;
begin
 repeat
  node:=PopStore;
 until (node=nil);
end;

//

Procedure TsrPrivate._PrepType(node:PPrepTypeNode);
begin
 TsrPrivate(node^.dnode).PrepType(TsrDataType(node^.rtype));
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
  Result:='v'+Source^.rid;
 end;
end;

function TsrPrivate.adjust_type(new:TsrDataType):TsrDataType;
begin
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
 node:TDependenceNode;
 pLine:TspirvOp;
begin
 pTypeList:=Emit.GetTypeList;
 Ftype:=pTypeList^.Fetch(rtype);

 UpdateRegType;

 {
 node:=FLineList.pHead;
 While (node<>nil) do
 begin
  pLine:=node.pNode;

  Case pLine.OpId of

   Op.OpLoad:
    begin
     pLine.pDst.PrepType(ord(rtype));
     pLine.pType:=Ftype;
    end;

   Op.OpStore:
    begin  f
     pLine.ParamNode(1).Value.PrepType(ord(rtype));
    end;

   else;
  end;

  node:=node.pNext;
 end;
 }
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
      if (old<>dtUnknow) and (not CompareType(rtype,old)) then
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

Procedure TsrPrivate.SortLines;
var
 dnode,dnext:TDependenceNode;
 pLine:array[0..1] of TspirvOp;
 nswp:Boolean;
begin
 repeat
  nswp:=True;
  dnode:=FLineList.pHead;
  While true do
  begin
   dnext:=dnode.pNext;
   if (dnext=nil) then Break;

   pLine[0]:=dnode.pNode;
   pLine[1]:=dnext.pNode;

   _update_store_line(pLine[0]);
   _update_store_line(pLine[1]);

   if (MaxLine(pLine[0],pLine[1])=pLine[0]) then //dnode>dnext
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

Procedure TsrPrivate.Optimize;
var
 dnode,dprev:TDependenceNode;
 pLine:array[0..1] of TspirvOp;
 pRegs:array[0..1] of TsrRegNode;
begin
 dnode:=FLineList.pTail;
 While true do
 begin
  dprev:=dnode.pPrev;
  if (dprev=nil) then Break;

  pLine[0]:=dnode.pNode;
  pLine[1]:=dprev.pNode;

  if (pLine[0].Parent=pLine[1].Parent) then
  begin
   //OpStore %v %r
   //OpStore %v %r
   if (pLine[0].OpId=Op.OpStore) and (pLine[1].OpId=Op.OpStore) then
   begin
    //Remove dprev
    FLineList.Remove(dprev);
    pLine[1].mark_not_used;
    Continue;
   end else
   if (pLine[0].OpId=Op.OpStore) and (pLine[1].OpId=Op.OpLoad) then
   begin
    //%r = OpLoad %type %v ; pLine[1] ; dprev
    //OpStore %v %r        ; pLine[0] ; dnode

    pRegs[0]:=RegDown(pLine[0].ParamNode(1).Value.specialize AsType<ntReg>);
    pRegs[1]:=RegDown(pLine[1].pDst.specialize AsType<ntReg>);

    if (pRegs[0]<>nil) and (pRegs[1]<>nil) and CompareReg(pRegs[0],pRegs[1]) then
    begin
     //Remove dnode
     FLineList.Remove(dnode);
     pLine[0].mark_not_used;

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
    //%r1 = OpLoad %type %v ; pLine[1] ; dprev
    //%r2 = OpLoad %type %v ; pLine[0] ; dnode

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

Procedure TsrPrivate.FetchStore(pLine:TspirvOp;node:TDependenceNode);
var
 src:TsrRegNode;
begin
 if (node=nil) then Exit;

 src:=TsrRegNode(node.pNode);
 if (src=nil) then Exit;

 pLine:=Emit.OpStore(pLine,FVar,src);

 AddLine(pLine,node);
end;

function TsrPrivate.NewVolatile:TsrVolatile;
begin
 Result:=Emit.specialize New<TsrVolatile>;
 Result.Init(Self);
 FVoltList.Push_tail(Result);
end;

procedure TsrPrivate.RemoveAllStore;
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

//

procedure TsrPrivateList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrPrivateList.Fetch(pSource:PsrRegSlot):TsrPrivate;
begin
 Assert(pSource<>nil);
 Result:=FTree.Find(@pSource);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrPrivate>;
  Result.Init;
  Result.FPrivId:=FPrivId; Inc(FPrivId);
  Result.key:=pSource;
  //
  FTree.Insert(Result);
  FList.Push_tail(Result);
 end;
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
 FList.Push_tail(Result);
end;

Function TsrPrivateList.First:TsrPrivate;
begin
 Result:=FList.pHead;
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
 pPrivate :TsrPrivate;
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

 pPrivate :=Fetch(pSlot);
 pVolatile:=pPrivate.NewVolatile;

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
 _next:=pSlot^.New(pLine,rtype);
 _next.pWriter:=pVolatile;

 ctx.AddVolatile(pVolatile,_next);

 pLine.AddParam(_next); //post processing
end;

procedure TsrPrivateList.build_slot_break(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,_next:TsrRegNode);
var
 pLine    :TSpirvOp;
 cur,nxt  :TsrRegNode;
 pPrivate :TsrPrivate;
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

 pPrivate:=Fetch(pSlot);

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

 //get Volatile
 if (nxt<>nil) then
 begin
  if nxt.pWriter.IsType(ntVolatile) then
  begin //next is volatile
   pVolatile:=nxt.pWriter.specialize AsType<ntVolatile>;

   if (pVolatile.FSource<>pPrivate) then
   begin
    //Reset if it is another register
    pVolatile:=nil;
   end;
  end;
 end;

 //check Volatile
 if (pVolatile=nil) then
 begin
  pVolatile:=pPrivate.NewVolatile;
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
  _next:=pSlot^.New(pLine,rtype);
  _next.pWriter:=pVolatile;

  ctx.AddVolatile(pVolatile,_next);

  pLine.AddParam(_next); //post processing
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
 pPrivate :TsrPrivate;
 pVolatile:TsrVolatile;
 new_vol  :Boolean;
 rtype    :TsrDataType;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(prev);

 if CompareReg(cur,prv) then
 begin
  Exit;
 end;

 cur:=pSlot^.current;
 prv:=prev;

 pPrivate:=Fetch(pSlot);

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

   if (pVolatile.FSource<>pPrivate) then
   begin
    //Reset if it is another register
    pVolatile:=nil;
   end;
  end;
 end;

 //check Volatile
 if (pVolatile=nil) then
 begin
  pVolatile:=pPrivate.NewVolatile;
  new_vol:=True;
 end else
 begin
  new_vol:=False;
 end;

 rtype:=dtUnknow;

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
   prev:=pSlot^.New(pLine,rtype);
  end;

  //set backedge dependence
  prev.pWriter:=pVolatile;

  ctx.AddVolatile(pVolatile,prev);

  pLine.AddParam(prev); //post processing
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

procedure TsrPrivateList.build_test(pSlot:PsrRegSlot);
var
 cur:TsrRegNode;
 pLine:TspirvOp;
begin
 cur:=pSlot^.current;
 if (cur=nil) then Exit;
 if (cur.pWriter=nil) then Exit;
 if (cur.pWriter.IsType(ntConst)) then Exit;

 pLine:=FEmit.curr_line;

 //if not IsDominUp(cur^.pLine,pLine) then
 //begin
 // Writeln(cur^.pWriter^.ntype.ClassName);
 // IsDominUp_print(cur^.pLine,FEmit.curr_line);
 //
 // Assert(IsDominUp(pLine,cur^.pLine)=false);
 //end;

end;

procedure TsrPrivateList.build_volatile_test;
var
 pRegsStory:PsrRegsStory;
begin
 //exit;
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSlot(@build_test);
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

  node:=pSlot^.New(pLine,cur.dtype);
  node.pWriter:=cur;
  FEmit.PostLink(pLine,node); //post processing

  node.pLine:=pLine;
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
 node:TDependenceNode;
begin
 node:=src.PopStore;
 While (node<>nil) do
 begin
  dst.PushStore(node);
  node:=src.PopStore;
 end;
end;

function TsrPrivateList.PrepVolatile(dst:TspirvOp;src:TsrRegNode):TsrRegNode; //use forward only
var
 tmp:TsrRegNode;
 pPrivate :TsrPrivate;
 pVolatile:TsrVolatile;
 node:TDependenceNode;
 pLine:TspirvOp;
 //pTmp:TspirvOp;
 //vtmp:TsrVolatile;
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
 pPrivate :=pVolatile.FSource;

 pPrivate.InitVar();

 pPrivate .PrepType(src.dtype);
 pVolatile.PrepType(pPrivate.GetRegType);

 if (pPrivate.GetRegType=dtUnknow) then
 begin
  Assert(false);
 end;

 node:=pVolatile.PopStore;
 While (node<>nil) do
 begin

  tmp:=RegDown{Slot}(node.pNode);

  if (src<>tmp) {and (pPrivate^.pVar<>get_load_from(tmp))} then
  begin

   pLine:=TsrRegNode(node.pNode).pLine;
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

   pPrivate.FetchStore(pLine,node);
  end;

  node:=pVolatile.PopStore;
 end;

 //ntVolatile -> src -> next
 //Opload     -> new

 Result:=src.pSlot^.New(dst,src.dtype);

 pPrivate.FetchLoad(dst,Result); //before reg
end;

Procedure TsrPrivateList.RemoveAllStore;
var
 node:TsrPrivate;
begin
 node:=FList.pHead;
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
 node:=FList.pHead;
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


end.


