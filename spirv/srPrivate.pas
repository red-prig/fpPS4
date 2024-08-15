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
  //
  Procedure _zero_read   ;                 override;
  Procedure _zero_unread;                  override;
  Procedure _PrepType(node:PPrepTypeNode); override;
  //
  Procedure Init(Source:TsrPrivate); inline;
  Procedure AddStore(src:TsrRegNode);
  Procedure PushStore(node:TDependenceNode);
  Function  PopStore:TDependenceNode;
  Procedure RemoveAllStore;
  Procedure zero_read;
  Procedure zero_unread;
  procedure PrepType(new:TsrDataType);
 end;

 ntVolatile=TsrVolatile;

 TsrPrivate=class(TsrDescriptor)
  type
   TVoltList=specialize TNodeListClass<TsrVolatile>;
  public
   pLeft,pRight:TsrPrivate;
   class function c(n1,n2:PPsrRegSlot):Integer; static;
  private
   //
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
   function  isBoolOnly:Boolean;
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
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(pSource:PsrRegSlot):TsrPrivate;
  function  First:TsrPrivate; inline;
  Function  Next(node:TsrPrivate):TsrPrivate;
  //
  procedure build_slot_dis(pSlot:PsrRegSlot;var old:TsrRegNode);
  procedure build_slot_cur(pSlot:PsrRegSlot;var old:TsrRegNode);
  procedure build_slot_brk(pSlot:PsrRegSlot;var old:TsrRegNode);
  procedure build_slot_old(pSlot:PsrRegSlot;var old:TsrRegNode);
  procedure build_test(pSlot:PsrRegSlot);
  procedure build_volatile_test;
  procedure build_volatile_dis(old:PsrRegsSnapshot);
  procedure build_volatile_cur(old:PsrRegsSnapshot);
  procedure build_volatile_brk(old:PsrRegsSnapshot);
  procedure build_volatile_old(old:PsrRegsSnapshot);
  procedure make_copy_slot(pSlot:PsrRegSlot);
  procedure make_copy_all;
  function  PrepVolatile(dst:TspirvOp;src:TsrRegNode):TsrRegNode;
  Procedure RemoveAllStore;
  Procedure Post;
 end;

implementation

Procedure TsrVolatile._zero_read;
begin
 zero_read;
end;

Procedure TsrVolatile._zero_unread;
begin
 zero_unread;
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
 if FSource.isBoolOnly then
 begin
  new:=dtBool;
 end;
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
 if IsUsed then
 begin
  src.mark_read(Self);
 end;
 FList.Push_head(node);
end;

Procedure TsrVolatile.PushStore(node:TDependenceNode);
begin
 if (node=nil) then Exit;
 if IsUsed then
 begin
  TsrRegNode(node.pNode).mark_read(Self);
 end;
 FList.Push_head(node);
end;

Function TsrVolatile.PopStore:TDependenceNode;
begin
 Result:=FList.Pop_head;

 if (Result<>nil) then
 if IsUsed then
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

Procedure TsrVolatile.zero_read;
var
 node:TDependenceNode;
begin
 node:=FList.pHead;
 While (node<>nil) do
 begin
  TsrRegNode(node.pNode).mark_read(Self);
  node:=node.pNext;
 end;
end;

Procedure TsrVolatile.zero_unread;
var
 node:TDependenceNode;
begin
 node:=FList.pHead;
 While (node<>nil) do
 begin
  TsrRegNode(node.pNode).mark_unread(Self);
  node:=node.pNext;
 end;
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
 Result:='v'+Source^.rid;
end;

function TsrPrivate.isBoolOnly:Boolean;
begin
 Result:=(Source^.isBoolOnly);
end;

Procedure TsrPrivate.SetRegType(rtype:TsrDataType);
var
 pTypeList:PsrTypeList;
 node:TDependenceNode;
 pLine:TspirvOp;
begin
 pTypeList:=Emit.GetTypeList;
 Ftype:=pTypeList^.Fetch(rtype);

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
    begin
     pLine.ParamNode(1).Value.PrepType(ord(rtype));
    end;

   else;
  end;

  node:=node.pNext;
 end;
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
 dst:TsrRegNode;
 old,rtype:TsrDataType;
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
     pLine.pDst.PrepType(ord(rtype));
     pLine.pType:=Ftype;

     dst:=pLine.pDst.specialize AsType<ntReg>;
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
     pLine.ParamNode(1).Value.PrepType(ord(rtype));

     dst:=pLine.ParamNode(1).Value.specialize AsType<ntReg>;
     if (dst<>nil) then
     begin
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
 if isBoolOnly then
 begin
  new:=dtBool;
 end;
 if (new=dtUnknow) then Exit;
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
 pnode,pnext:TDependenceNode;
 pLine:array[0..1] of TspirvOp;
 nswp:Boolean;
begin
 repeat
  nswp:=True;
  pnode:=FLineList.pHead;
  While true do
  begin
   pnext:=pnode.pNext;
   if (pnext=nil) then Break;

   pLine[0]:=pnode.pNode;
   pLine[1]:=pnext.pNode;

   _update_store_line(pLine[0]);
   _update_store_line(pLine[1]);

   if (MaxLine(pLine[0],pLine[1])=pLine[0]) then //pnode>pnext
   begin
    //swap
    nswp:=False;
    FLineList.Remove(pnode);
    FLineList.InsertAfter(pnext,pnode);
   end else
   begin
    pnode:=pnext;
   end;

  end;
 until nswp;
end;

Procedure TsrPrivate.Optimize;
var
 pnode,pprev:TDependenceNode;
 pLine:array[0..1] of TspirvOp;
 pRegs:array[0..1] of TsrRegNode;
begin
 pnode:=FLineList.pTail;
 While true do
 begin
  pprev:=pnode.pPrev;
  if (pprev=nil) then Break;

  pLine[0]:=pnode.pNode;
  pLine[1]:=pprev.pNode;

  if (pLine[0].Parent=pLine[1].Parent) then
  begin
   //OpStore %v %r
   //OpStore %v %r
   if (pLine[0].OpId=Op.OpStore) and (pLine[1].OpId=Op.OpStore) then
   begin
    //Remove pprev
    FLineList.Remove(pprev);
    pLine[1].mark_not_used;
    Continue;
   end else
   if (pLine[0].OpId=Op.OpStore) and (pLine[1].OpId=Op.OpLoad) then
   begin
    //%r = OpLoad %type %v ; pLine[1] ; pprev
    //OpStore %v %r        ; pLine[0] ; pnode

    pRegs[0]:=RegDown(pLine[0].ParamNode(1).Value.specialize AsType<ntReg>);
    pRegs[1]:=RegDown(pLine[1].pDst.specialize AsType<ntReg>);

    if (pRegs[0]<>nil) and (pRegs[1]<>nil) and CompareReg(pRegs[0],pRegs[1]) then
    begin
     //Remove pnode
     FLineList.Remove(pnode);
     pLine[0].mark_not_used;

     pnode:=pprev;
     if (pnode.pNext<>nil) then
     begin
      pnode:=pnode.pNext;
     end;

     Continue;
    end;
   end else
   if (pLine[0].OpId=Op.OpLoad) and (pLine[1].OpId=Op.OpLoad) then
   begin
    //%r1 = OpLoad %type %v ; pLine[1] ; pprev
    //%r2 = OpLoad %type %v ; pLine[0] ; pnode

    pRegs[0]:=pLine[0].pDst.specialize AsType<ntReg>;
    pRegs[1]:=pLine[1].pDst.specialize AsType<ntReg>;

    if (pRegs[0]<>nil) and (pRegs[1]<>nil) then
    begin
     pRegs[0].pWriter:=pRegs[1];

     //Remove pnode
     FLineList.Remove(pnode);

     pnode:=pprev;
     if (pnode.pNext<>nil) then
     begin
      pnode:=pnode.pNext;
     end;

     Continue;
    end;

   end;

  end;

  pnode:=pprev;
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
  Result.key:=pSource;
  //
  FTree.Insert(Result);
 end;
end;

Function TsrPrivateList.First:TsrPrivate;
begin
 Result:=FTree.Min;
end;

Function TsrPrivateList.Next(node:TsrPrivate):TsrPrivate;
begin
 Result:=FTree.Next(node);
end;

procedure TsrPrivateList.build_slot_dis(pSlot:PsrRegSlot;var old:TsrRegNode);
begin

 pSlot^.current:=old;

 //if (pSlot^.current<>nil) then
 //if not IsDominUp(pSlot^.current^.pLine,FEmit.curr_line) then
 //begin
 // Assert(IsDominUp(FEmit.curr_line,pSlot^.current^.pLine)=false);
 //end;

end;

//procedure IsDominUp_print(pNode,pLine:TspirvOp);
//begin
// if (pNode=nil) or (pLine=nil) then Exit;
//
// Writeln('pNode:',GetGlobalIndex(pNode):3,'..',GetGlobalIndexA(pNode):3,' ',PsrOpBlock(TspirvOp(pNode)^.Parent)^.Level:3);
// Writeln('pLine:',GetGlobalIndex(pLine):3,'..',GetGlobalIndexA(pLine):3,' ',PsrOpBlock(TspirvOp(pLine)^.Parent)^.Level:3);
//end;

procedure TsrPrivateList.build_slot_cur(pSlot:PsrRegSlot;var old:TsrRegNode);
var
 cur,prv:TsrRegNode;
 pPrivate :TsrPrivate;
 pVolatile:TsrVolatile;
 rtype:TsrDataType;

begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);

 if CompareReg(cur,prv) then
 begin

  //if (cur<>nil) then
  //begin
  // if not IsDominUp(cur^.pLine,FEmit.curr_line) then
  // begin
  //  Writeln(cur^.pWriter^.ntype.ClassName);
  //  IsDominUp_print(cur^.pLine,FEmit.curr_line);
  //  Assert(IsDominUp(FEmit.curr_line,cur^.pLine)=false);
  // end;
  //end;

  Exit;
 end;

 cur:=pSlot^.current;

 Assert(cur<>nil,'WTF');

 if (old=nil) then
 begin
  ////old:=pSlot^.New(cur^.pLine,cur^.dtype); //Unresolve
 end;

 pPrivate :=Fetch(pSlot);
 pVolatile:=pPrivate.NewVolatile;

 rtype:=dtUnknow;

 if (old<>nil) then
 begin
  pVolatile.AddStore(old);
  rtype:=old.dtype;
 end;
 //
 if (cur<>nil) then
 begin
  pVolatile.AddStore(cur);
  rtype:=cur.dtype;
 end;

 //writeln('cur:',pSlot^.rid,':',GetGlobalIndex(cur^.pLine),':',cur^.pWriter^.ntype.ClassName);

 //if (old<>nil) then
 //if not IsDominUp(old^.pLine,FEmit.curr_line) then
 //begin
 // Writeln(old^.pWriter^.ntype.ClassName);
 // IsDominUp_print(old^.pLine,FEmit.curr_line);
 //
 // Assert(IsDominUp(FEmit.curr_line,old^.pLine)=false);
 //end;

 old:=pSlot^.New(FEmit.curr_line,rtype);
 old.pWriter:=pVolatile;
 FEmit.PostLink(old.pLine,old); //post processing

 //if not IsDominUp(pSlot^.current^.pLine,FEmit.curr_line) then
 //begin
 // Assert(IsDominUp(FEmit.curr_line,pSlot^.current^.pLine)=false);
 //end;

end;

procedure TsrPrivateList.build_slot_brk(pSlot:PsrRegSlot;var old:TsrRegNode);
var
 cur,prv:TsrRegNode;
 pPrivate :TsrPrivate;
 pVol_old:TsrVolatile;
 pVolatile:TsrVolatile;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);

 if CompareReg(cur,prv) then
 begin

  //if (cur<>nil) then
  //if not IsDominUp(cur^.pLine,FEmit.curr_line) then
  //begin
  // Writeln(cur^.pWriter^.ntype.ClassName);
  // IsDominUp_print(cur^.pLine,FEmit.curr_line);
  // IsDominUp_print(prv^.pLine,FEmit.curr_line);
  // Assert(IsDominUp(FEmit.curr_line,cur^.pLine)=false);
  // CompareReg(cur,prv);
  //end;

  Exit;
 end;

 cur:=pSlot^.current;

 pPrivate:=Fetch(pSlot);

 prv:=nil;

 if (old<>nil) then
 begin

  if old.pWriter.IsType(ntVolatile) then
  begin //old is volatile
   pVol_old:=old.pWriter.specialize AsType<ntVolatile>;

   Assert(pVol_old.FSource=pPrivate,'WTF');

   pVol_old.AddStore(cur);

   //if (cur<>nil) then
   //if not IsDominUp(cur^.pLine,FEmit.curr_line) then
   //begin
   // Writeln(cur^.pWriter^.ntype.ClassName);
   // IsDominUp_print(cur^.pLine,FEmit.curr_line);
   // Assert(IsDominUp(FEmit.curr_line,cur^.pLine)=false);
   //end;

   //writeln('brk1:',pSlot^.rid);
  end else
  begin //new volatile
   prv:=old.pWriter.specialize AsType<ntReg>;
   Assert(prv<>nil);
   Assert(prv.pSlot=pSlot);

   pVolatile:=pPrivate.NewVolatile;

   pVolatile.AddStore(prv);
   pVolatile.AddStore(cur);

   old.pWriter:=pVolatile;

   //writeln('brk2:',pSlot^.rid,':',GetGlobalIndex(old^.pLine),':',cur^.pWriter^.ntype.ClassName);
  end;

  //if not IsDominUp(old^.pLine,FEmit.curr_line) then
  //begin
  // if prv<>nil then
  // begin
  //  Writeln(prv^.pWriter^.ntype.ClassName);
  //  IsDominUp_print(prv^.pLine,FEmit.curr_line);
  // end;
  //
  // Writeln(old^.pWriter^.ntype.ClassName);
  // IsDominUp_print(old^.pLine,FEmit.curr_line);
  //
  // Writeln(cur^.pWriter^.ntype.ClassName);
  // IsDominUp_print(cur^.pLine,FEmit.curr_line);
  //
  // Assert(IsDominUp(FEmit.curr_line,old^.pLine)=false);
  //end;

 end else
 if (cur<>nil) then
 begin
  pVolatile:=pPrivate.NewVolatile;
  pVolatile.AddStore(cur);

  //prev is unresolve
  old:=pSlot^.New(FEmit.curr_line,cur.dtype);
  old.pWriter:=pVolatile;
  FEmit.PostLink(old.pLine,old); //post processing

  pSlot^.current:=cur; //prev

  //writeln('brk3:',pSlot^.rid,':',cur^.pWriter^.ntype.ClassName);
 end;

 //if not IsDominUp(pSlot^.current^.pLine,FEmit.curr_line) then
 //begin
 // Assert(IsDominUp(FEmit.curr_line,pSlot^.current^.pLine)=false);
 //end;

end;

procedure TsrPrivateList.build_slot_old(pSlot:PsrRegSlot;var old:TsrRegNode);
var
 cur,prv:TsrRegNode;
 pPrivate :TsrPrivate;
 pVol_old:TsrVolatile;
 pVolatile:TsrVolatile;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);

 if CompareReg(cur,prv) then
 begin

  //if (cur<>nil) then
  //if not IsDominUp(cur^.pLine,FEmit.curr_line) then
  //begin
  // Writeln(cur^.pWriter^.ntype.ClassName);
  // IsDominUp_print(cur^.pLine,FEmit.curr_line);
  // IsDominUp_print(prv^.pLine,FEmit.curr_line);
  // Assert(IsDominUp(FEmit.curr_line,cur^.pLine)=false);
  // CompareReg(cur,prv);
  //end;

  Exit;
 end;

 cur:=pSlot^.current;

 pPrivate:=Fetch(pSlot);

 prv:=nil;

 if (old<>nil) then
 begin

  if old.pWriter.IsType(ntVolatile) then
  begin //old is volatile
   pVol_old:=old.pWriter.specialize AsType<ntVolatile>;

   Assert(pVol_old.FSource=pPrivate,'WTF');

   pVol_old.AddStore(cur);

   //writeln('old1:',pSlot^.rid);
  end else
  begin //new volatile
   prv:=old.pWriter.specialize AsType<ntReg>;
   Assert(prv<>nil);
   Assert(prv.pSlot=pSlot);

   pVolatile:=pPrivate.NewVolatile;

   pVolatile.AddStore(prv);
   pVolatile.AddStore(cur);

   old.pWriter:=pVolatile;

   //writeln('old2:',pSlot^.rid);
  end;

 end else
 if (cur<>nil) then
 begin
  pVolatile:=pPrivate.NewVolatile;
  pVolatile.AddStore(cur);

  //prev is unresolve
  old:=pSlot^.New(FEmit.curr_line,cur.dtype);
  old.pWriter:=pVolatile;
  FEmit.PostLink(old.pLine,old); //post processing

  //writeln('old3:',pSlot^.rid);
 end;

 pSlot^.current:=old; //reset

 //if not IsDominUp(old^.pLine,FEmit.curr_line) then
 //begin
 // Writeln(old^.pWriter^.ntype.ClassName);
 // IsDominUp_print(old^.pLine,FEmit.curr_line);
 //
 // Assert(IsDominUp(FEmit.curr_line,old^.pLine)=false);
 //end;

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

procedure TsrPrivateList.build_volatile_dis(old:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 //exit;
 pRegsStory:=FEmit.GetRegsStory;
 pRegsStory^.ForEachSnap(@build_slot_dis,old);
end;

procedure TsrPrivateList.build_volatile_cur(old:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 //exit;
 pRegsStory:=FEmit.GetRegsStory;
 //NextVolatileID; FVolatileID:SizeUint;
 pRegsStory^.ForEachSnap(@build_slot_cur,old);
end;

procedure TsrPrivateList.build_volatile_brk(old:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 //exit;
 pRegsStory:=FEmit.GetRegsStory;
 //NextVolatileID; FVolatileID:SizeUint;
 pRegsStory^.ForEachSnap(@build_slot_brk,old);
end;

procedure TsrPrivateList.build_volatile_old(old:PsrRegsSnapshot);
var
 pRegsStory:PsrRegsStory;
begin
 //exit;
 pRegsStory:=FEmit.GetRegsStory;
 //NextVolatileID; FVolatileID:SizeUint;
 pRegsStory^.ForEachSnap(@build_slot_old,old);
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
 //exit;
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
 pLine,pTmp:TspirvOp;
 vtmp:TsrVolatile;
begin
 Result:=src;
 if (src=nil) then Exit;
 if (not src.pWriter.IsType(ntVolatile)) then Exit;

 //move to prev
 Assert(dst<>nil);
 dst:=dst.Prev;
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
 node:=FTree.Min;
 While (node<>nil) do
 begin
  node.RemoveAllStore;
  node:=FTree.Next(node);
 end;
end;

Procedure TsrPrivateList.Post;
var
 node:TsrPrivate;
begin
 node:=FTree.Min;
 While (node<>nil) do
 begin
  if node.IsUsed then
  begin
   node.SortLines;
   node.Optimize;
   node.UpdateRegType;
  end;
  node:=FTree.Next(node);
 end;
end;


end.


