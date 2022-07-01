unit srVolatile;

{$mode ObjFPC}{$H+}

interface

uses
 spirv,
 srNodes,
 srTypes,
 srConst,
 srReg,
 srVariable,
 srOp,
 srOpUtils,
 SprvEmit;

type
 PsrVolatile=^TsrVolatile;
 TsrVolatile=object
  pNext:PsrVolatile;
  pReg:PsrRegNode;
  pPar:PsrOpBlock;
  FID:SizeUint;
 end;

 PsrVolatiles=^TsrVolatiles;
 _TsrVolatiles=specialize TNodeStack<PsrVolatile>;
 TsrVolatiles=object(_TsrVolatiles)
  function pop_reg:PsrRegNode;
  function find_reg(pReg:PsrRegNode):PsrVolatile;
 end;

 TEmitVolatile=object(TSprvEmit)
  procedure NextVolatileID;
  function  AllocVolatile:PsrVolatile;
  function  NewVolatile(pReg:PsrRegNode):PsrVolatile;
  procedure build_slot_dis(pSlot:PsrRegSlot;var old:PsrRegNode);
  procedure build_slot_cur(pSlot:PsrRegSlot;var old:PsrRegNode);
  procedure build_slot_old(pSlot:PsrRegSlot;var old:PsrRegNode);
  procedure build_volatile_dis(old:PsrRegsSnapshot);
  procedure build_volatile_cur(old:PsrRegsSnapshot);
  procedure build_volatile_old(old:PsrRegsSnapshot);
  procedure make_copy_slot(pSlot:PsrRegSlot);
  procedure make_copy_all;
  procedure PrepVolatile(dst:PspirvOp;src:PsrRegNode);
 end;

implementation

uses
 emit_post;

function CompareReg(r1,r2:PsrRegNode):Boolean;
begin
 r1:=RegDown(r1);
 r2:=RegDown(r2);
 Result:=(r1=r2);
 if not Result then
 begin
  Result:=CompareConst(r1^.AsConst,r2^.AsConst);
 end;
end;

function TsrVolatiles.pop_reg:PsrRegNode;
var
 node:PsrVolatile;
begin
 Result:=nil;
 node:=Pop_head;
 if (node=nil) then Exit;
 Result:=node^.pReg;
end;

function TsrVolatiles.find_reg(pReg:PsrRegNode):PsrVolatile;
var
 node:PsrVolatile;
begin
 Result:=nil;
 //pReg:=RegDownSlot(pReg);
 node:=pHead;
 While (node<>nil) do
 begin
  if (pReg={RegDownSlot}(node^.pReg)) then Exit(node);
  node:=node^.pNext;
 end;
end;

procedure TEmitVolatile.NextVolatileID;
begin
 Inc(FVolatileID);
end;

function TEmitVolatile.AllocVolatile:PsrVolatile;
begin
 Result:=Alloc(SizeOf(TsrVolatile));
end;

function TEmitVolatile.NewVolatile(pReg:PsrRegNode):PsrVolatile;
begin
 Result:=AllocVolatile;
 Result^.pReg:=pReg;
 Result^.pPar:=FMain^.pBlock;
 Result^.FID :=FVolatileID;
 pReg^.mark_read;
end;

procedure TEmitVolatile.build_slot_dis(pSlot:PsrRegSlot;var old:PsrRegNode);
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

procedure TEmitVolatile.build_slot_cur(pSlot:PsrRegSlot;var old:PsrRegNode);
var
 cur,prv,new:PsrRegNode;
 st_cur:TsrVolatiles;
 rtype:TsrDataType;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);
 if CompareReg(cur,prv) then Exit;

 cur:=pSlot^.current;

 if (cur<>nil) then
 begin
  rtype:=cur^.dtype;
 end else
 begin
  rtype:=old^.dtype;
 end;

 if (cur=nil) then
 begin
  Assert(false,'WTF');
  //cur:=pSlot^.New(rtype); //Unresolve
 end;
 if (old=nil) then
 begin
  //old:=pSlot^.New(rtype); //Unresolve
 end;

 st_cur:=Default(TsrVolatiles);

 if (old<>nil) then
 begin
  st_cur.Push_head(NewVolatile(old));
 end;

 st_cur.Push_head(NewVolatile(cur));

 new:=pSlot^.New(cur^.pLine,rtype);
 new^.pWriter.SetParam(ntVolatile,st_cur.pHead);
 new^.mark_read;
 PostReg(new);
 old:=new; //update snap
end;

procedure TEmitVolatile.build_slot_old(pSlot:PsrRegSlot;var old:PsrRegNode);
var
 cur,prv:PsrRegNode;
 st_old:TsrVolatiles;
begin
 cur:=RegDownSlot(pSlot^.current);
 prv:=RegDownSlot(old);
 if (prv=nil) or CompareReg(cur,prv) then Exit;

 cur:=pSlot^.current;

 if (old^.pWriter.ntype=ntVolatile) then
 begin
  st_old.pHead:=old^.pWriter.pData;
  st_old.Push_head(NewVolatile(cur));
  Exit;
 end;

 Assert(old^.pWriter.ntype=ntReg);
 Assert(old^.pWriter.pData<>nil);
 prv:=old^.pWriter.pData;

 st_old:=Default(TsrVolatiles);
 st_old.Push_head(NewVolatile(prv));
 st_old.Push_head(NewVolatile(cur));

 prv^.mark_unread;
 old^.pWriter.SetParam(ntVolatile,st_old.pHead);
end;

procedure TEmitVolatile.build_volatile_dis(old:PsrRegsSnapshot);
begin
 FRegsStory.ForEachSnap(@build_slot_dis,old);
end;

procedure TEmitVolatile.build_volatile_cur(old:PsrRegsSnapshot);
begin
 NextVolatileID;
 FRegsStory.ForEachSnap(@build_slot_cur,old);
end;

procedure TEmitVolatile.build_volatile_old(old:PsrRegsSnapshot);
begin
 NextVolatileID;
 FRegsStory.ForEachSnap(@build_slot_old,old);
end;

procedure TEmitVolatile.make_copy_slot(pSlot:PsrRegSlot);
var
 cur:PsrRegNode;
begin
 if (pSlot^.current<>nil) then
 begin
  cur:=pSlot^.current;
  MakeCopy(pSlot,cur);
  pSlot^.current^.pLine:=cur^.pLine;
 end;
end;

procedure TEmitVolatile.make_copy_all;
begin
 FRegsStory.ForEachSlot(@make_copy_slot);
end;


function get_load_from(r:PsrRegNode):PsrVariable;
var
 pOp:PspirvOp;
begin
 Result:=nil;
 pOp:=r^.AsOp;
 if (pOp=nil) then Exit;
 if (pOp^.OpId<>Op.OpLoad) then Exit;
 Result:=pOp^.pParam.pHead^.AsVar;
end;

type
 TsrTypesA=array[Low(TsrDataType)..High(TsrDataType)] of DWORD;

function calc_best_type(pSlot:PsrRegSlot;st:TsrVolatiles):TsrDataType;
var
 node:PsrVolatile;
 pReg:PsrRegNode;
 types:TsrTypesA;
 i,max_id:TsrDataType;
 bonly:Boolean;
begin
 Result:=dtUnknow;

 case pSlot^.rid of
  'SCC':Exit(dtBool); //only bool
 end;

 types:=Default(TsrTypesA);
 bonly:=true;
 node:=st.pHead;
 While (node<>nil) do
 begin
  if (node^.pReg<>nil) then
  begin
   pReg:=RegDown(node^.pReg);
   if (pReg^.dtype<>dtBool) then bonly:=false;
   Inc(types[pReg^.dtype]);
  end;
  node:=node^.pNext;
 end;

 case pSlot^.rid of
   'VCCL',
   'VCCH',
  'EXECL',
  'EXECH':if (types[dtBool]<>0) then Exit(dtBool); //prior bool
 end;

 if bonly and (types[dtBool]<>0) then
 begin
  Exit(dtBool);
 end else
 begin
  //types[dtBool]:=0;
 end;

 max_id:=dtUnknow;
 For i:=Low(TsrDataType) to High(TsrDataType) do
 begin
  if (max_id=dtUnknow) and (types[i]<>0) then
  begin
   max_id:=i;
  end else
  begin
   if (types[i]>types[max_id]) then max_id:=i
  end;
 end;
 Result:=max_id;
end;

{
procedure MoveVolatiles(dst,src:PsrVolatiles);
var
 node:PsrVolatile;
begin
 repeat
  node:=src^.Pop_head;
  if (node=nil) then Break;
  if (dst^.find_reg(node^.pReg)=nil) then
  begin
   //Writeln(node^.pReg^.GetName);
   dst^.Push_head(node);
  end else
  begin
   RegUnmark(node^.pReg);
  end;
 until false;
end;

function TEmitVolatile._PrepVolatile(src:PsrRegNode):Integer;
var
 st_tmp:TsrVolatiles;
 node:PsrVolatile;
 pReg:PsrRegNode;
begin
 Result:=0;
 if (src=nil) then Exit;
 if (src^.pWriter.ntype<>ntVolatile) then Exit;

 node:=src^.pWriter.pData;
 While (node<>nil) do
 begin
  pReg:=RegDownSlot(node^.pReg);

  if (pReg<>nil) then
  begin
   if (pReg^.pWriter.ntype=ntVolatile) then
   begin
    st_tmp.pHead:=pReg^.pWriter.pData;
    Assert(st_tmp.pHead<>nil);

    MoveVolatiles(@src^.pWriter.pData,@st_tmp);

    pReg^.pWriter.SetParam(ntReg,src);

    node^.pReg:=nil;
    Inc(Result);
   end;
  end;

  node:=node^.pNext;
 end;
end;
}

procedure TEmitVolatile.PrepVolatile(dst:PspirvOp;src:PsrRegNode);
var
 rtype:TsrDataType;
 pSlot:PsrRegSlot;
 pReg,tmp:PsrRegNode;
 v:PsrVariable;
 node:PsrVolatile;
 st,st_tmp:TsrVolatiles;
 pLine:PspirvOp;
begin
 if (src=nil) then Exit;
 if (src^.pWriter.ntype<>ntVolatile) then Exit;

 pSlot:=src^.pSlot;

 st.pHead:=src^.pWriter.pData;
 Assert(st.pHead<>nil);

 //While _PrepVolatile(src)<>0 do;

 v:=pSlot^.pVar;
 if (v=nil) then
 begin
  rtype:=calc_best_type(pSlot,st);

  v:=NewVariable;
  v^.dtype:=rtype;

  pSlot^.pVar:=v;
 end else
 begin
  rtype:=v^.dtype;
 end;

 repeat
  node:=st.Pop_head;
  if (node=nil) then Break;
  pReg:=node^.pReg;
  if (pReg=nil) then Continue;
  //Assert(pReg<>nil);
  //Assert(pReg^.pWriter.ntype<>ntVolatile);
  tmp:=RegDownSlot(pReg);
  if (v<>get_load_from(tmp)) then
  begin
   //mark_read volatile->emit_OpStore
   pLine:=tmp^.pLine;
   Assert(pLine<>nil);

   if (tmp^.pWriter.ntype=ntVolatile) then
   begin
    st_tmp.pHead:=tmp^.pWriter.pData;
    Assert(st_tmp.pHead<>nil);

    st.Move_from(st_tmp);

    src^.mark_read;
    tmp^.pWriter.SetParam(ntReg,src);

    //tmp^.pLine:=dst;
    //writeln;
    //PrepVolatile(pReg^.pLine,pReg);

    node^.pReg:=nil;
    Continue;
   end;

   {Case pLine^.OpId of
    OpLinks:Writeln('OpLinks');
    OpBlock:Writeln('OpBlock');
    else
     Writeln(Op.GetStr(pLine^.OpId));
   end;
   Writeln(HexStr(pLine^.Adr.Offdw*4,4));

   Assert(pReg^.pWriter.ntype<>ntVolatile);}

   TSprvEmit_post(Self).PrepTypeNode(pReg,rtype,False);

   {if pLine^.Adr.Offdw*4=$AC then
   begin
    writeln('dst:',Op.GetStr(dst^.OpId),' ',HexStr(dst^.Adr.Offdw*4,4));
    writeln;
   end;}

   _up_merge_line(pLine);

   TSprvEmit_post(Self).emit_OpStore(pLine,v,pReg); //after reg
  end else
  begin
   RegUnmark(pReg);
   //pReg^.mark_unread;
  end;
 until false;

 v^.mark_read;
 //src^.dtype:=rtype; //reuse
 pLine:=dst;
 Assert(pLine<>nil);
 pLine:=pLine^.pPrev;
 Assert(pLine<>nil);

 _up_merge_line(pLine);

 Assert(pLine<>nil);

 TSprvEmit_post(Self).PrepTypeDst(src,rtype);  //reuse

 TSprvEmit_post(Self).emit_OpLoad(pLine,src,v); //before reg
end;

end.

