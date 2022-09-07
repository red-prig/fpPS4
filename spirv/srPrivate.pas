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
 srBitcast;

type
 ntPrivate=class(ntDescriptor)
  class Function  pwrite_count  (node:PsrNode):PDWORD;        override;
  class Procedure PrepType      (node:PPrepTypeNode);         override;
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 ntVolatile=class(TsrNodeVmt)
  class Procedure zero_read     (node:PsrNode);               override;
  class Procedure zero_unread   (node:PsrNode);               override;
  class Procedure PrepType      (node:PPrepTypeNode);         override;
 end;

 PsrPrivate=^TsrPrivate;

 PsrVolatile=^TsrVolatile;
 TsrVolatile=object(TsrNode)
  type
   PVNode=^TVNode;
   TVNode=record
    pPrev,pNext:PVNode;
    pReg:PsrRegNode;
   end;
   TNodeList=specialize TNodeList<PVNode>;
  var
   pPrev,pNext:PsrVolatile;
   FSource:PsrPrivate;
   FList:TNodeList;
  Procedure Init(Source:PsrPrivate); inline;
  Procedure AddStore(src:PsrRegNode);
  Procedure PushStore(node:PVNode);
  Function  PopStore:PVNode;
  Procedure RemoveAllStore;
  Procedure zero_read;
  Procedure zero_unread;
  procedure PrepType(new:TsrDataType);
 end;

 TsrPrivate=object(TsrDescriptor)
  type
   PVNode=^TVNode;
   TVNode=record
    pPrev,pNext:PVNode;
    pLine:PspirvOp;
   end;
   TNodeList=specialize TNodeList<PVNode>;
   TVoltList=specialize TNodeList<PsrVolatile>;
  private
   pLeft,pRight:PsrPrivate;
   //----
   fwrite_count:DWORD;
   //
   FSource:PsrRegSlot;
   //
   FLineList:TNodeList;
   FVoltList:TVoltList;
   function  c(n1,n2:PsrPrivate):Integer; static;
   Procedure SetRegType(rtype:TsrDataType);
   function  GetRegType:TsrDataType;
  public
   property  dtype:TsrDataType read GetRegType write SetRegType;
   property  Source:PsrRegSlot read FSource;
   Procedure Init; inline;
   function  Emit:TCustomEmit;
   function  GetStorageName:RawByteString;
   function  isBoolOnly:Boolean;
   Procedure UpdateRegType;
   Procedure PrepType(new:TsrDataType);
   Procedure SortLines;
   Procedure Optimize;
   procedure AddLine(pLine:PspirvOp);
   procedure AddLine(pLine:PspirvOp;node:TsrVolatile.PVNode);
   procedure FetchLoad(pLine:PspirvOp;dst:PsrRegNode);
   Procedure FetchStore(pLine:PspirvOp;node:TsrVolatile.PVNode);
   function  NewVolatile:PsrVolatile;
   procedure RemoveAllStore;
 end;

 PsrPrivateList=^TsrPrivateList;
 TsrPrivateList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrPrivate,TsrPrivate>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(pSource:PsrRegSlot):PsrPrivate;
  function  First:PsrPrivate; inline;
  Function  Next(node:PsrPrivate):PsrPrivate;
  //
  procedure build_slot_dis(pSlot:PsrRegSlot;var old:PsrRegNode);
  procedure build_slot_cur(pSlot:PsrRegSlot;var old:PsrRegNode);
  procedure build_slot_old(pSlot:PsrRegSlot;var old:PsrRegNode);
  procedure build_volatile_dis(old:PsrRegsSnapshot);
  procedure build_volatile_cur(old:PsrRegsSnapshot);
  procedure build_volatile_old(old:PsrRegsSnapshot);
  procedure make_copy_slot(pSlot:PsrRegSlot);
  procedure make_copy_all;
  procedure PrepVolatile(dst:PspirvOp;src:PsrRegNode);
  Procedure RemoveAllStore;
  Procedure Post;
 end;

implementation

class Procedure ntVolatile.zero_read(node:PsrNode);
begin
 PsrVolatile(node)^.zero_read;
end;

class Procedure ntVolatile.zero_unread(node:PsrNode);
begin
 PsrVolatile(node)^.zero_unread;
end;

class Procedure ntVolatile.PrepType(node:PPrepTypeNode);
begin
 PsrVolatile(node^.dnode)^.PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

//

Procedure TsrVolatile.Init(Source:PsrPrivate); inline;
begin
 fntype :=ntVolatile;
 FSource:=Source;
end;

procedure TsrVolatile.PrepType(new:TsrDataType);
var
 node:PVNode;
begin
 if FSource^.isBoolOnly then
 begin
  new:=dtBool;
 end;
 if (new=dtUnknow) then exit;

 node:=FList.pHead;
 While (node<>nil) do
 begin
  node^.pReg^.PrepType(ord(new));
  node:=node^.pNext;
 end;
end;

Procedure TsrVolatile.AddStore(src:PsrRegNode);
var
 node:PVNode;
begin
 if (src=nil) then Exit;
 node:=FSource^.Emit.Alloc(SizeOf(TVNode));
 node^.pReg:=src;
 if IsUsed then
 begin
  src^.mark_read(@Self);
 end;
 FList.Push_head(node);
end;

Procedure TsrVolatile.PushStore(node:PVNode);
begin
 if (node=nil) then Exit;
 if IsUsed then
 begin
  node^.pReg^.mark_read(@Self);
 end;
 FList.Push_head(node);
end;

Function TsrVolatile.PopStore:PVNode;
begin
 Result:=FList.Pop_head;
 if (Result<>nil) then
 if IsUsed then
 begin
  Result^.pReg^.mark_unread(@Self);
 end;
end;

Procedure TsrVolatile.RemoveAllStore;
var
 node:PVNode;
begin
 repeat
  node:=PopStore;
 until (node=nil);
end;

Procedure TsrVolatile.zero_read;
var
 node:PVNode;
begin
 node:=FList.pHead;
 While (node<>nil) do
 begin
  node^.pReg^.mark_read(@Self);
  node:=node^.pNext;
 end;
end;

Procedure TsrVolatile.zero_unread;
var
 node:PVNode;
begin
 node:=FList.pHead;
 While (node<>nil) do
 begin
  node^.pReg^.mark_unread(@Self);
  node:=node^.pNext;
 end;
end;

//

class Function ntPrivate.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrPrivate(node)^.fwrite_count;
end;

class Procedure ntPrivate.PrepType(node:PPrepTypeNode);
begin
 PsrPrivate(node^.dnode)^.PrepType(TsrDataType(node^.rtype));
 node^.dnode:=nil;
end;

class function ntPrivate.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrPrivate(node)^.GetStorageName;
end;

//

function TsrPrivate.c(n1,n2:PsrPrivate):Integer;
begin
 Result:=Integer(n1^.FSource>n2^.FSource)-Integer(n1^.FSource<n2^.FSource);
end;

Procedure TsrPrivate.Init; inline;
begin
 fntype  :=ntPrivate;
 FStorage:=StorageClass.Private_;
 FBinding:=-1;
end;

function TsrPrivate.Emit:TCustomEmit;
begin
 Result:=FSource^.FEmit;
end;

function TsrPrivate.GetStorageName:RawByteString;
begin
 Result:='v'+FSource^.rid;
end;

function TsrPrivate.isBoolOnly:Boolean;
begin
 Result:=(FSource^.isBoolOnly);
end;

Procedure TsrPrivate.SetRegType(rtype:TsrDataType);
var
 pTypeList:PsrTypeList;
 node:PVNode;
 pLine:PspirvOp;
begin
 pTypeList:=Emit.GetTypeList;
 Ftype:=pTypeList^.Fetch(rtype);

 node:=FLineList.pHead;
 While (node<>nil) do
 begin
  pLine:=node^.pLine;

  Case pLine^.OpId of

   Op.OpLoad:
    begin
     pLine^.pDst^.PrepType(ord(rtype));
     pLine^.pType:=Ftype;
    end;

   Op.OpStore:
    begin
     pLine^.ParamFirst^.Value^.PrepType(ord(rtype));
    end;

   else;
  end;

  node:=node^.pNext;
 end;
end;

function TsrPrivate.GetRegType:TsrDataType;
begin
 Result:=FType^.dtype;
end;

Procedure TsrPrivate.UpdateRegType;
var
 pBitcastList:PsrBitcastList;
 node:PVNode;
 pLine:PspirvOp;
 dst:PsrRegNode;
 old,rtype:TsrDataType;
begin
 rtype:=FType^.dtype;

 pBitcastList:=Emit.GetBitcastList;

 node:=FLineList.pHead;
 While (node<>nil) do
 begin
  pLine:=node^.pLine;

  Case pLine^.OpId of

   Op.OpLoad:
    begin
     pLine^.pDst^.PrepType(ord(rtype));
     pLine^.pType:=Ftype;

     dst:=pLine^.pDst^.AsType(ntReg);
     if (dst<>nil) then
     begin
      old:=dst^.dtype;
      if (old<>dtUnknow) and (not CompareType(rtype,old)) then
      begin
       //OpLoad -> new -> dst
       pBitcastList:=Emit.GetBitcastList;
       dst:=pBitcastList^.FetchDstr(rtype,dst);
       pLine^.pDst:=dst;
      end;
     end;
    end;

   Op.OpStore:
    begin
     pLine^.ParamFirst^.Value^.PrepType(ord(rtype));

     dst:=pLine^.ParamFirst^.Value^.AsType(ntReg);
     if (dst<>nil) then
     begin
      old:=dst^.dtype;
      if (old<>dtUnknow) and (rtype<>old) then
      begin
       //OpStore <- new <- dst
       pBitcastList:=Emit.GetBitcastList;
       dst:=pBitcastList^.FetchRead(rtype,dst);
       pLine^.ParamFirst^.Value:=dst;
      end;
     end;
    end;

   else;
  end;

  node:=node^.pNext;
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

Procedure TsrPrivate.SortLines;
var
 pnode,pnext:PVNode;
 pLine:array[0..1] of PspirvOp;
 nswp:Boolean;
begin
 repeat
  nswp:=True;
  pnode:=FLineList.pHead;
  While true do
  begin
   pnext:=pnode^.pNext;
   if (pnext=nil) then Break;

   pLine[0]:=pnode^.pLine;
   pLine[1]:=pnext^.pLine;

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
 pnode,pprev:PVNode;
 pLine:array[0..1] of PspirvOp;
begin
 pnode:=FLineList.pTail;
 While true do
 begin
  pprev:=pnode^.pPrev;
  if (pprev=nil) then Break;

  pLine[0]:=pnode^.pLine;
  pLine[1]:=pprev^.pLine;

  if (pLine[0]^.Parent=pLine[1]^.Parent) then
  begin
   //OpStore %v %r
   //OpStore %v %r
   if (pLine[0]^.OpId=Op.OpStore) and (pLine[1]^.OpId=Op.OpStore) then
   begin
    //Remove pprev
    FLineList.Remove(pprev);
    pLine[1]^.mark_not_used;
    Continue;
   end else
   if (pLine[0]^.OpId=Op.OpStore) and (pLine[1]^.OpId=Op.OpLoad) then
   begin
    //%r = OpLoad %type %v ; pLine[1] ; pprev
    //OpStore %v %r        ; pLine[0] ; pnode
    if CompareReg(RegDown(pLine[0]^.pDst^.AsType(ntReg)),RegDown(pLine[1]^.ParamFirst^.Value^.AsType(ntReg))) then
    begin
     //Remove pnode
     FLineList.Remove(pnode);
     pLine[0]^.mark_not_used;

     pnode:=pprev;
     Continue;
    end;
   end;
  end;

  pnode:=pprev;
 end;
end;

procedure TsrPrivate.AddLine(pLine:PspirvOp);
var
 node:PVNode;
begin
 node:=Emit.Alloc(SizeOf(TVNode));
 node^.pLine:=pLine;
 FLineList.Push_tail(node);
end;

procedure TsrPrivate.AddLine(pLine:PspirvOp;node:TsrVolatile.PVNode);
var
 _node:PVNode;
begin
 _node:=Pointer(node); //hack
 _node^:=Default(TVNode);
 _node^.pLine:=pLine;
 FLineList.Push_tail(_node);
end;

procedure TsrPrivate.FetchLoad(pLine:PspirvOp;dst:PsrRegNode);
begin
 Assert(dst<>nil);

 pLine:=PspirvOp(Emit.OpLoad(pLine,FType,dst,FVar));

 AddLine(pLine);
end;

Procedure TsrPrivate.FetchStore(pLine:PspirvOp;node:TsrVolatile.PVNode);
var
 src:PsrRegNode;
begin
 if (node=nil) then Exit;
 src:=node^.pReg;
 if (src=nil) then Exit;

 pLine:=PspirvOp(Emit.OpStore(pLine,FVar,src));

 AddLine(pLine,node);
end;

function TsrPrivate.NewVolatile:PsrVolatile;
begin
 Result:=Emit.Alloc(SizeOf(TsrVolatile));
 Result^.Init(@Self);
 FVoltList.Push_tail(Result);
end;

procedure TsrPrivate.RemoveAllStore;
var
 node:PsrVolatile;
begin
 node:=FVoltList.pHead;
 While (node<>nil) do
 begin
  node^.RemoveAllStore;
  node:=node^.pNext;
 end;
end;

//

procedure TsrPrivateList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrPrivateList.Fetch(pSource:PsrRegSlot):PsrPrivate;
var
 node:TsrPrivate;
begin
 Assert(pSource<>nil);
 node:=Default(TsrPrivate);
 node.Init;
 node.FSource:=pSource;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrPrivate));
  Move(node,Result^,SizeOf(TsrPrivate));
  //
  FNTree.Insert(Result);
 end;
end;

Function TsrPrivateList.First:PsrPrivate;
begin
 Result:=FNTree.Min;
end;

Function TsrPrivateList.Next(node:PsrPrivate):PsrPrivate;
begin
 Result:=FNTree.Next(node);
end;

procedure TsrPrivateList.build_slot_dis(pSlot:PsrRegSlot;var old:PsrRegNode);
var
 cur:PsrRegNode;
begin
 cur:=pSlot^.current;
 While (cur<>old) do
 begin
  pSlot^.Remove(cur);
  cur:=pSlot^.current;
 end;
end;

procedure TsrPrivateList.build_slot_cur(pSlot:PsrRegSlot;var old:PsrRegNode);
var
 cur,prv,new:PsrRegNode;
 pPrivate :PsrPrivate;
 pVolatile:PsrVolatile;
 rtype:TsrDataType;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);
 if CompareReg(cur,prv) then Exit;

 cur:=pSlot^.current;

 if (cur=nil) then
 begin
  Assert(false,'WTF');
 end;
 if (old=nil) then
 begin
  //////old:=pSlot^.New(line,rtype); //Unresolve
 end;

 pPrivate :=Fetch(pSlot);
 pVolatile:=pPrivate^.NewVolatile;

 rtype:=dtUnknow;

 if (old<>nil) then
 //if (pVolatile<>old^.pWriter^.AsType(ntVolatile)) then
 //if not old^.pWriter^.IsType(ntVolatile) then
 begin
  pVolatile^.AddStore(old);
  rtype:=old^.dtype;
 end;
 //
 if (cur<>nil) then
 //if (pVolatile<>cur^.pWriter^.AsType(ntVolatile)) then
 //if not cur^.pWriter^.IsType(ntVolatile) then
 begin
  pVolatile^.AddStore(cur);
  rtype:=cur^.dtype;
 end;

 //old^.PrepType(ord(pVolatile^.Ftype));
 //cur^.PrepType(ord(pVolatile^.Ftype));

 new:=pSlot^.New(cur^.pLine,rtype);
 new^.pWriter:=pVolatile;
 FEmit.PostLink(cur^.pLine,new); //post processing

 old:=new; //update snap
end;

procedure TsrPrivateList.build_slot_old(pSlot:PsrRegSlot;var old:PsrRegNode);
var
 cur,prv,new:PsrRegNode;
 pPrivate :PsrPrivate;
 pVolatile:PsrVolatile;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);
 if CompareReg(cur,prv) then Exit;

 cur:=pSlot^.current;

 if (old<>nil) then
 if old^.pWriter^.IsType(ntVolatile) then
 begin
  pVolatile:=old^.pWriter^.AsType(ntVolatile);
  //if (pVolatile<>cur^.pWriter^.AsType(ntVolatile)) then
  begin
   pVolatile^.AddStore(cur);
  end;
  Exit;
 end;

 prv:=nil;
 if (old<>nil) then
 begin
  prv:=old^.pWriter^.AsType(ntReg);
  Assert(prv<>nil);
 end;

 pPrivate :=Fetch(pSlot);
 pVolatile:=pPrivate^.NewVolatile;

 //if (pVolatile<>prv^.pWriter^.AsType(ntVolatile)) then
 begin
  pVolatile^.AddStore(prv);
 end;

 //if (pVolatile<>cur^.pWriter^.AsType(ntVolatile)) then
 begin
  pVolatile^.AddStore(cur);
 end;

 if (old<>nil) then
 begin
  old^.pWriter:=pVolatile;
 end else
 if (cur<>nil) then
 begin
  //prev is unresolve
  new:=pSlot^.New(cur^.pLine,cur^.dtype);
  new^.pWriter:=pVolatile;
  FEmit.PostLink(FEmit.curr_line,new); //post processing
 end;
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
 cur,node:PsrRegNode;
begin
 //exit;
 if (pSlot^.current<>nil) then
 begin
  cur:=pSlot^.current;

  //MakeCopy(pSlot,cur);
  node:=pSlot^.New(FEmit.curr_line,cur^.dtype);
  node^.pWriter:=cur;
  FEmit.PostLink(FEmit.curr_line,node); //post processing

  pSlot^.current^.pLine:=cur^.pLine;
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

function get_load_from(r:PsrRegNode):PsrVariable;
var
 pOp:PspirvOp;
begin
 Result:=nil;
 pOp:=r^.pWriter^.AsType(ntOp);
 if (pOp=nil) then Exit;
 if (pOp^.OpId<>Op.OpLoad) then Exit;
 Result:=pOp^.ParamFirst^.Value^.AsType(ntVariable);
end;

function get_load_from2(r:PsrRegNode):PsrVolatile;
begin
 Result:=r^.pWriter^.AsType(ntVolatile);
end;

procedure _Move(dst,src:PsrVolatile);
var
 node:TsrVolatile.PVNode;
begin
 node:=src^.PopStore;
 While (node<>nil) do
 begin
  dst^.PushStore(node);
  node:=src^.PopStore;
 end;
end;

procedure TsrPrivateList.PrepVolatile(dst:PspirvOp;src:PsrRegNode); //use forward only
var
 tmp:PsrRegNode;
 pPrivate :PsrPrivate;
 pVolatile:PsrVolatile;
 node:TsrVolatile.PVNode;
 pLine:PspirvOp;
 vtmp:PsrVolatile;
begin
 if (src=nil) then Exit;
 if (not src^.pWriter^.IsType(ntVolatile)) then Exit;

 pVolatile:=src^.pWriter^.AsType(ntVolatile);
 pPrivate :=pVolatile^.FSource;
 src^.pWriter:=nil;

 pPrivate^.InitVar(FEmit);

 pPrivate ^.PrepType(src^.dtype);
 pVolatile^.PrepType(pPrivate^.GetRegType);

 if (pPrivate^.GetRegType=dtUnknow) then
 begin
  Assert(false);
 end;

 node:=pVolatile^.PopStore;
 While (node<>nil) do
 begin

  tmp:=RegDown{Slot}(node^.pReg);

  if (src<>tmp) {and (pPrivate^.pVar<>get_load_from(tmp))} then
  begin
   pLine:=node^.pReg^.pLine;
   Assert(pLine<>nil);

   if (tmp^.pWriter^.IsType(ntVolatile)) then
   begin
    vtmp:=tmp^.pWriter^.AsType(ntVolatile);
    Assert(vtmp<>nil);

    _Move(pVolatile,vtmp);

    tmp^.pWriter:=src;

    Continue;
   end;

   up_merge_line(pLine);

   pPrivate^.FetchStore(pLine,node);
  end;

  node:=pVolatile^.PopStore;
 end;

 pLine:=dst;
 Assert(pLine<>nil);
 pLine:=pLine^.Prev;
 Assert(pLine<>nil);

 up_merge_line(pLine);

 Assert(pLine<>nil);

 //ntVolatile -> src -> next
 //Opload     -> src -> next

 pPrivate^.FetchLoad(pLine,src); //before reg
end;

Procedure TsrPrivateList.RemoveAllStore;
var
 node:PsrPrivate;
begin
 node:=FNTree.Min;
 While (node<>nil) do
 begin
  node^.RemoveAllStore;
  node:=FNTree.Next(node);
 end;
end;

Procedure TsrPrivateList.Post;
var
 node:PsrPrivate;
begin
 node:=FNTree.Min;
 While (node<>nil) do
 begin
  if node^.IsUsed then
  begin
   node^.SortLines;
   node^.Optimize;
   node^.UpdateRegType;
  end;
  node:=FNTree.Next(node);
 end;
end;


end.


