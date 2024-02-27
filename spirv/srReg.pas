unit srReg;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 ginodes,
 srNode,
 srRefId,
 srType,
 srConst;

type
 ntReg=class(TsrNodeVmt)
  class Procedure add_read    (node,src:PsrNode);           override;
  class Procedure rem_read    (node,src:PsrNode);           override;
  class Procedure SetWriter   (node,w,line:PsrNode);        override;
  class Procedure ResetWriter (node,w:PsrNode);             override;
  class function  Down        (node:PsrNode):Pointer;       override;
  class function  Next        (node:PsrNode):Pointer;       override;
  class function  Prev        (node:PsrNode):Pointer;       override;
  class Procedure PrepType    (node:PPrepTypeNode);         override;
  class function  GetPrintName(node:PsrNode):RawByteString; override;
  class function  GetRef      (node:PsrNode):Pointer;       override;
 end;

 ntRegPair=class(TsrNodeVmt)
  class function  Down       (node:PsrNode):Pointer; override;
  class Procedure SetWriter  (node,w,line:PsrNode);  override;
  class Procedure ResetWriter(node,w:PsrNode);       override;
 end;

 PPsrRegSlot=^PsrRegSlot;
 PsrRegSlot=^TsrRegSlot;

 PPsrRegNode=^PsrRegNode;
 PsrRegNode=^TsrRegNode;

 PRegDNode=^TRegDNode;
 TRegDNode=record
  pNext:PRegDNode;
  pNode:PsrNode;
 end;
 TRegDNodeList=specialize TNodeStack<PRegDNode>;

 TsrRegNode=packed object(TsrNode)
  public
   pPrev,pNext:PsrRegNode;
  private
   ID:TsrRefId;            //post id
   F:bitpacked record
    dtype:TsrDataType;
    weak:Boolean;
   end;
   FSlot:PsrRegSlot;
   FWriter:PsrNode;        //ntReg,ntConst,ntOp,ntVolatile
   FDList:TRegDNodeList;
   Procedure AddDep(t:PsrNode);
   Procedure RemDep(t:PsrNode);
   function  GetDtype:TsrDataType;
   Procedure SetDtype(t:TsrDataType);
   function  GetWeak:Boolean;
   Procedure SetWeak(t:Boolean);
   Procedure SetWriter(t:PsrNode);
   Function  GetWriter:PsrNode;
  public
   pLine:Pointer;   //PspirvOp;
   property  pSlot  :PsrRegSlot  read FSlot;
   property  dtype  :TsrDataType read GetDtype  write SetDtype;
   property  Weak   :Boolean     read GetWeak   write SetWeak;
   property  pWriter:PsrNode     read GetWriter write SetWriter;
   Procedure Init; inline;
   function  FirstDep:PRegDNode;
   function  GetName:RawByteString;
   function  AsConst:PsrConst;
   function  AsReg:PsrRegNode;
   function  is_const:Boolean;
   function  is_bool:Boolean;
   function  is_bool_or_const_bool:Boolean;
   function  is_unknow:Boolean;
   function  GetPrintName:RawByteString;
 end;

 PsrRegPair=^TsrRegPair;
 TsrRegPair=object(TsrNode)
  private
   FWriter:PsrNode;
   FDst0:PsrNode;
   FDst1:PsrNode;
   Procedure SetWriter(t:PsrNode);
   Procedure SetDst0(r:PsrNode);
   Procedure SetDst1(r:PsrNode);
  public
   property  pLine:PsrNode   read FWriter;
   property  pWriter:PsrNode read FWriter write SetWriter;
   property  pDst0:PsrNode   read FDst0   write SetDst0;
   property  pDst1:PsrNode   read FDst1   write SetDst1;
   Procedure Init; inline;
 end;

 TRegStory=specialize TNodeList<PsrRegNode>;

 TString5=string[5];

 TsrRegSlot=object
  private
   FEmit:TCustomEmit;
   pStory:TRegStory;
   Frid:TString5;
  public
   current:PsrRegNode;
   property  Emit:TCustomEmit read FEmit;
   property  rid :TString5    read Frid;
   Procedure Init(e:TCustomEmit;const n:TString5);
   function  first:PsrRegNode;
   function  last :PsrRegNode;
   function  isBoolOnly:Boolean;
   function  New(pLine:Pointer;rtype:TsrDataType):PsrRegNode;
   function  NewAfter(rtype:TsrDataType;r:PsrRegNode):PsrRegNode;
   function  NewBefore(rtype:TsrDataType;r:PsrRegNode):PsrRegNode;
   procedure Insert(r:PsrRegNode);
   procedure Remove(r:PsrRegNode);
 end;

 PsrRegsSnapshot=^TsrRegsSnapshot;
 TsrRegsSnapshot=object
  SGRP:array[0..103] of PsrRegNode;
  VCC:array[0..1] of PsrRegNode;
  M0:PsrRegNode;
  EXEC:array[0..1] of PsrRegNode;
  SCC:PsrRegNode;
  VGRP:array[0..255] of PsrRegNode;
 end;

 TForEachSlot=procedure(pSlot:PsrRegSlot) of object;
 TForEachSnap=procedure(pSlot:PsrRegSlot;var old:PsrRegNode) of object;

 PsrRegsStory=^TsrRegsStory;
 TsrRegsStory=object
  SGRP:array[0..103] of TsrRegSlot;
  VCC:array[0..1] of TsrRegSlot;
  M0:TsrRegSlot;
  EXEC:array[0..1] of TsrRegSlot;
  SCC:TsrRegSlot;
  VGRP:array[0..255] of TsrRegSlot;
  FUnattach:TsrRegSlot;
  //
  FDList:TRegDNodeList;
  //
  Procedure Init(Emit:TCustomEmit);
  //
  Function  AllocDep:PRegDNode;
  Procedure FreeDep(node:PRegDNode);
  //
  function  get_sdst7(SDST:Byte):PsrRegSlot;
  function  get_sdst7_pair(SDST:Byte;dst:PPsrRegSlot):Boolean;
  function  get_ssrc8(SSRC:Byte):PsrRegSlot;
  function  get_ssrc9(SSRC:Word):PsrRegSlot;
  function  get_ssrc9_pair(SSRC:Word;src:PPsrRegSlot):Boolean;
  function  get_vsrc8(VSRC:Byte):PsrRegSlot;
  function  get_vdst8(VDST:Byte):PsrRegSlot;
  function  get_sbase(SBASE,count:Byte;src:PPsrRegSlot):Boolean;
  function  get_srsrc(SRSRC,count:Byte;src:PPsrRegSlot):Boolean;
  //
  function  get_snapshot:TsrRegsSnapshot;
  procedure ForEachSlot(cb:TForEachSlot);
  procedure ForEachSnap(cb:TForEachSnap;old:PsrRegsSnapshot);
 end;

function RegDown(node:PsrRegNode):PsrRegNode;
function RegDownSlot(node:PsrRegNode):PsrRegNode;
function CompareReg(r1,r2:PsrRegNode):Boolean;

implementation

//

class Procedure ntReg.add_read(node,src:PsrNode);
begin
 inherited;
 PsrRegNode(node)^.AddDep(src);
end;

class Procedure ntReg.rem_read(node,src:PsrNode);
begin
 inherited;
 PsrRegNode(node)^.RemDep(src);
end;

class Procedure ntReg.SetWriter(node,w,line:PsrNode);
begin
 With PsrRegNode(node)^ do
 begin
  SetWriter(w);
  pLine:=line;
 end;
end;

class Procedure ntReg.ResetWriter(node,w:PsrNode);
begin
 With PsrRegNode(node)^ do
 if (FWriter=w) then
 begin
  SetWriter(nil);
  pLine:=nil;
 end;
end;

class function ntReg.Down(node:PsrNode):Pointer;
begin
 Result:=PsrRegNode(node)^.FWriter;
end;

class function ntReg.Next(node:PsrNode):Pointer;
begin
 Result:=PsrRegNode(node)^.pNext;
end;

class function ntReg.Prev(node:PsrNode):Pointer;
begin
 Result:=PsrRegNode(node)^.pPrev;
end;

class Procedure ntReg.PrepType(node:PPrepTypeNode);
var
 new:TsrDataType;
 pConstList:PsrConstList;
begin
 With PsrRegNode(node^.dnode)^ do
 begin
  new:=TsrDataType(node^.rtype);
  if (new=dtUnknow) then
  begin
   node^.dnode:=nil;
   Exit;
  end;

  if FSlot^.isBoolOnly then
  begin
   //next
   node^.rtype:=ord(dtBool);
   node^.dnode:=pWriter;
   Exit;
  end else
  if is_unprep_type(dtype,new,Weak) then
  begin
   dtype:=new;
   Weak:=True;
   if is_const then
   begin
    pConstList:=FSlot^.FEmit.GetConstList;
    pWriter:=pConstList^.Bitcast(new,pWriter^.AsType(ntConst));
   end else
   begin
    //next
    node^.dnode:=pWriter;
    Exit;
   end;
  end;

 end;

 node^.dnode:=nil;
end;

class function ntReg.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrRegNode(node)^.GetPrintName;
end;

class function ntReg.GetRef(node:PsrNode):Pointer;
begin
 With PsrRegNode(node)^ do
 begin
  if is_const then
  begin
   Result:=AsConst^.GetRef;
  end else
  begin
   Result:=@ID;
  end;
 end;
end;

//

class function ntRegPair.Down(node:PsrNode):Pointer;
begin
 Result:=PsrRegPair(node)^.FWriter;
end;

class Procedure ntRegPair.SetWriter(node,w,line:PsrNode);
begin
 With PsrRegPair(node)^ do
 begin
  SetWriter(w);
 end;
end;

class Procedure ntRegPair.ResetWriter(node,w:PsrNode);
begin
 With PsrRegPair(node)^ do
 if (FWriter=w) then
 begin
  SetWriter(nil);
 end;
end;

//

Procedure TsrRegsStory.Init(Emit:TCustomEmit);
var
 i:Word;
 n:TString5;
begin
 FillChar(Self,SizeOf(TsrRegsStory),0);
 For i:=0 to 103 do
 begin
  Str(i,n);
  SGRP[i].Init(Emit,'S'+n);
 end;
 VCC[0].Init(Emit,'VCCL');
 VCC[1].Init(Emit,'VCCH');
 M0.Init(Emit,'M0');
 EXEC[0].Init(Emit,'EXECL');
 EXEC[1].Init(Emit,'EXECH');
 SCC.Init(Emit,'SCC');
 For i:=0 to 255 do
 begin
  Str(i,n);
  VGRP[i].Init(Emit,'V'+n);
 end;
 FUnattach.Init(Emit,'UNATT');
end;

Function TsrRegsStory.AllocDep:PRegDNode;
begin
 Result:=FDList.Pop_head;
 if (Result=nil) then
 begin
  Result:=FUnattach.FEmit.Alloc(SizeOf(TRegDNode));
 end;
end;

Procedure TsrRegsStory.FreeDep(node:PRegDNode);
begin
 if (node=nil) then Exit;
 node^:=Default(TRegDNode);
 FDList.Push_head(node);
end;

function TsrRegsStory.get_sdst7(SDST:Byte):PsrRegSlot;
begin
 Case SDST of
  0..103:Result:=@SGRP[SDST];
  106:Result:=@VCC[0];
  107:Result:=@VCC[1];
  124:Result:=@M0;
  126:Result:=@EXEC[0];
  127:Result:=@EXEC[1];
  else
      Result:=nil;
 end;
end;

function TsrRegsStory.get_sdst7_pair(SDST:Byte;dst:PPsrRegSlot):Boolean;
begin
 Result:=True;
 if (dst=nil) then Exit(false);
 Case SDST of
  0..102:
   begin
    dst[0]:=@SGRP[SDST];
    dst[1]:=@SGRP[SDST+1];
   end;
  106:
   begin
    dst[0]:=@VCC[0];
    dst[1]:=@VCC[1];
   end;
  126:
   begin
    dst[0]:=@EXEC[0];
    dst[1]:=@EXEC[1];
   end;
  else
      Result:=False;
 end;
end;

function TsrRegsStory.get_ssrc8(SSRC:Byte):PsrRegSlot;
begin
 Case SSRC of
  0..103:Result:=@SGRP[SSRC];
  106:Result:=@VCC[0];
  107:Result:=@VCC[1];
  124:Result:=@M0;
  126:Result:=@EXEC[0];
  127:Result:=@EXEC[1];

  //251:Result:=@VCC[0];  //VCCZ
  //252:Result:=@EXEC[0]; //EXECZ

  253:Result:=@SCC;
  //254:Write('LDS_DIRECT');
  else
      Result:=nil;
 end;
end;

function TsrRegsStory.get_ssrc9(SSRC:Word):PsrRegSlot;
begin
 Case SSRC of
  0..103:Result:=@SGRP[SSRC];
  106:Result:=@VCC[0];
  107:Result:=@VCC[1];
  124:Result:=@M0;
  126:Result:=@EXEC[0];
  127:Result:=@EXEC[1];

  //251:Result:=@VCC[0];  //VCCZ
  //252:Result:=@EXEC[0]; //EXECZ

  253:Result:=@SCC;
  //254:Write('LDS_DIRECT');
  256..511:Result:=@VGRP[SSRC-256];
  else
      Result:=nil;
 end;
end;

function TsrRegsStory.get_ssrc9_pair(SSRC:Word;src:PPsrRegSlot):Boolean;
begin
 Result:=True;
 if (src=nil) then Exit(False);
 Case SSRC of
  0..102:
   begin
    src[0]:=@SGRP[SSRC];
    src[1]:=@SGRP[SSRC+1];
   end;
  106:
   begin
    src[0]:=@VCC[0];
    src[1]:=@VCC[1];
   end;
  126:
   begin
    src[0]:=@EXEC[0];
    src[1]:=@EXEC[1];
   end;
  256..510:
   begin
    src[0]:=@VGRP[SSRC-256];
    src[1]:=@VGRP[SSRC-255];
   end;
  else
      Result:=False;
 end;
end;

function TsrRegsStory.get_vsrc8(VSRC:Byte):PsrRegSlot;
begin
 Result:=@VGRP[VSRC];
end;

function TsrRegsStory.get_vdst8(VDST:Byte):PsrRegSlot;
begin
 Result:=@VGRP[VDST];
end;

//SBASE 0..63
//SGRP:array[0..103] of TpsslRegSlot;
function TsrRegsStory.get_sbase(SBASE,count:Byte;src:PPsrRegSlot):Boolean;
var
 i,p:Byte;
begin
 Result:=True;
 if (SBASE>63) or (count=0) or (src=nil) then Exit(False);
 p:=SBASE*2;
 if ((p+count)>104) then Exit(False);
 For i:=0 to count-1 do
 begin
  src[i]:=@SGRP[p+i];
 end;
end;

//SRSRC 0..31
function TsrRegsStory.get_srsrc(SRSRC,count:Byte;src:PPsrRegSlot):Boolean;
var
 i,p:Byte;
begin
 Result:=True;
 if (SRSRC>31) or (count=0) or (src=nil) then Exit(False);
 p:=SRSRC*4;
 if ((p+count)>104) then Exit(False);
 For i:=0 to count-1 do
 begin
  src[i]:=@SGRP[p+i];
 end;
end;

//

function TsrRegsStory.get_snapshot:TsrRegsSnapshot;
var
 i:Word;
begin
 Result:=Default(TsrRegsSnapshot);
 For i:=0 to 103 do
 begin
  Result.SGRP[i]:=SGRP[i].current;
 end;
 For i:=0 to 1 do
 begin
  Result.VCC[i] :=VCC[i].current;
 end;
 Result.M0      :=M0.current;
 For i:=0 to 1 do
 begin
  Result.EXEC[i]:=EXEC[i].current;
 end;
 Result.SCC     :=SCC.current;
 For i:=0 to 255 do
 begin
  Result.VGRP[i]:=VGRP[i].current;
 end;
end;

procedure TsrRegsStory.ForEachSlot(cb:TForEachSlot);
var
 i:Word;
begin
 if (cb=nil) then Exit;
 For i:=0 to 103 do
 begin
  cb(@SGRP[i]);
 end;
 For i:=0 to 1 do
 begin
  cb(@VCC[i]);
 end;
 cb(@M0);
 For i:=0 to 1 do
 begin
  cb(@EXEC[i]);
 end;
 cb(@SCC);
 For i:=0 to 255 do
 begin
  cb(@VGRP[i]);
 end;
end;

procedure TsrRegsStory.ForEachSnap(cb:TForEachSnap;old:PsrRegsSnapshot);
var
 i:Word;
begin
 if (cb=nil) or (old=nil) then Exit;
 For i:=0 to 103 do
 begin
  cb(@SGRP[i],Old^.SGRP[i]);
 end;
 For i:=0 to 1 do
 begin
  cb(@VCC[i],Old^.VCC[i]);
 end;
 cb(@M0,Old^.M0);
 For i:=0 to 1 do
 begin
  cb(@EXEC[i],Old^.EXEC[i]);
 end;
 cb(@SCC,Old^.SCC);
 For i:=0 to 255 do
 begin
  cb(@VGRP[i],Old^.VGRP[i]);
 end;
end;

//--

Procedure TsrRegNode.Init; inline;
begin
 fntype:=ntReg;
end;

function TsrRegNode.GetName:RawByteString;
begin
 Result:='';
 if (FSlot<>nil) then Result:=FSlot^.rid;
end;

Procedure TsrRegNode.AddDep(t:PsrNode);
var
 pRegsStory:PsrRegsStory;
 node:PRegDNode;
begin
 if (t=nil) or (@Self=nil) then Exit;

 pRegsStory:=FSlot^.FEmit.GetRegsStory;
 node:=pRegsStory^.AllocDep;

 node^.pNode:=t;
 FDList.Push_head(node);
end;

Procedure TsrRegNode.RemDep(t:PsrNode);
var
 pRegsStory:PsrRegsStory;
 node,_prev:PRegDNode;
begin
 if (t=nil) or (@Self=nil) then Exit;
 node:=FDList.pHead;
 _prev:=nil;
 While (node<>nil) do
 begin
  if (node^.pNode=t) then
  begin
   if (_prev=nil) then
   begin
    FDList.pHead:=node^.pNext;
   end else
   begin
    _prev^.pNext:=node^.pNext;
   end;

   pRegsStory:=FSlot^.FEmit.GetRegsStory;
   pRegsStory^.FreeDep(node);

   Exit;
  end;
  _prev:=node;
  node:=node^.pNext;
 end;
 Assert(false,'not found!');
end;

function TsrRegNode.FirstDep:PRegDNode;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Result:=FDList.pHead;
end;

function TsrRegNode.GetDtype:TsrDataType;
begin
 Result:=F.dtype;
end;

Procedure TsrRegNode.SetDtype(t:TsrDataType);
begin
 F.dtype:=t;
end;

function TsrRegNode.GetWeak:Boolean;
begin
 Result:=F.weak;
end;

Procedure TsrRegNode.SetWeak(t:Boolean);
begin
 F.weak:=t;
end;

Procedure TsrRegNode.SetWriter(t:PsrNode);
begin
 if (@Self=nil) then Exit;
 if (FWriter=t) then Exit;

 Assert(RegDown(t^.AsType(ntReg))<>@Self,'Circular reference');

 if isUsed then
 begin
        t^.mark_read  (@Self);
  FWriter^.mark_unread(@Self);
 end;
 FWriter:=t;
end;

Function TsrRegNode.GetWriter:PsrNode;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Result:=FWriter;
end;

function TsrRegNode.AsConst:PsrConst;
begin
 if (@Self=nil) then Exit(nil);
 Result:=PsrConst(FWriter^.AsType(ntConst));
end;

function TsrRegNode.AsReg:PsrRegNode;
begin
 if (@Self=nil) then Exit(nil);
 Result:=PsrRegNode(FWriter^.AsType(ntReg));
end;

function TsrRegNode.is_const:Boolean;
begin
 if (@Self=nil) then Exit(False);
 Result:=FWriter^.IsType(ntConst);
end;

function TsrRegNode.is_bool:Boolean;
begin
 if (@Self=nil) then Exit(False);
 Result:=(dtype=dtBool);
end;

function TsrRegNode.is_bool_or_const_bool:Boolean;
begin
 if (@Self=nil) then Exit(False);
 Result:=is_bool or
  (is_const and AsConst^.isBoolVal);
end;

function TsrRegNode.is_unknow:Boolean;
begin
 if (@Self=nil) then Exit(True);
 Result:=(dtype=dtUnknow);
end;

function TsrRegNode.GetPrintName:RawByteString;
begin
 if is_const then
 begin
  Result:=AsConst^.GetPrintName;
 end else
 begin
  Assert(ID.Alloc);
  Result:='r'+IntToStr(ID.ID);
 end;
end;

//


Procedure TsrRegPair.SetWriter(t:PsrNode);
begin
 if (FWriter=t) then Exit;
 if isUsed then
 begin
        t^.mark_read  (@Self);
  FWriter^.mark_unread(@Self);
 end;
 FWriter:=t;
end;

Procedure TsrRegPair.SetDst0(r:PsrNode);
begin
 if (FDst0=r) then Exit;
 FDst0^.ResetWriter(@Self);
 FDst0:=r;
 FDst0^.SetWriter(@Self,pLine);
end;

Procedure TsrRegPair.SetDst1(r:PsrNode);
begin
 if (FDst1=r) then Exit;
 FDst1^.ResetWriter(@Self);
 FDst1:=r;
 FDst1^.SetWriter(@Self,pLine);
end;

Procedure TsrRegPair.Init; inline;
begin
 fntype:=ntRegPair;
end;

//--

Procedure TsrRegSlot.Init(e:TCustomEmit;const n:TString5);
begin
 FEmit:=e;
 Frid :=n;
end;

function TsrRegSlot.first:PsrRegNode;
begin
 Result:=pStory.pHead;
end;

function TsrRegSlot.last:PsrRegNode;
begin
 Result:=pStory.pTail;
end;

function TsrRegSlot.isBoolOnly:Boolean;
begin
 Result:=(rid='SCC');
end;

function TsrRegSlot.New(pLine:Pointer;rtype:TsrDataType):PsrRegNode;
var
 node:PsrRegNode;
begin
 if isBoolOnly then
 begin
  rtype:=dtBool;
 end;
 node:=FEmit.Alloc(SizeOf(TsrRegNode));
 node^.Init;
 node^.FSlot:=@Self;
 node^.dtype:=rtype;
 node^.pLine:=pLine;
 pStory.Push_tail(node);
 Result:=node;
 //update
 current:=pStory.pTail;
end;

function TsrRegSlot.NewAfter(rtype:TsrDataType;r:PsrRegNode):PsrRegNode;
var
 node:PsrRegNode;
begin
 if isBoolOnly then
 begin
  rtype:=dtBool;
 end;
 node:=FEmit.Alloc(SizeOf(TsrRegNode));
 node^.Init;
 node^.FSlot:=@Self;
 node^.dtype:=rtype;
 node^.pLine:=r^.pLine;
 pStory.InsertAfter(r,node);
 Result:=node;
 //update
 if (r=pStory.pTail) then
 begin
  current:=pStory.pTail;
 end;
end;

function TsrRegSlot.NewBefore(rtype:TsrDataType;r:PsrRegNode):PsrRegNode;
var
 node:PsrRegNode;
begin
 if isBoolOnly then
 begin
  rtype:=dtBool;
 end;
 node:=FEmit.Alloc(SizeOf(TsrRegNode));
 node^.Init;
 node^.FSlot:=@Self;
 node^.dtype:=rtype;
 node^.pLine:=r^.pLine;
 pStory.InsertBefore(r,node);
 Result:=node;
 //update
 if (r=pStory.pTail) then
 begin
  current:=pStory.pTail;
 end;
end;

procedure TsrRegSlot.Insert(r:PsrRegNode);
begin
 if (r=nil) then Exit;
 pStory.Push_tail(r);
 //update
 current:=pStory.pTail;
end;

procedure TsrRegSlot.Remove(r:PsrRegNode);
begin
 if (r=nil) then Exit;
 pStory.Remove(r);
 //update
 if (r=current) then
 begin
  current:=pStory.pTail;
 end;
end;

//

function RegDown(node:PsrRegNode):PsrRegNode;
var
 tmp:PsrRegNode;
begin
 //backtrace
 Result:=node;
 While (Result<>nil) do
 begin
  tmp:=Result^.AsReg; //next
  if (tmp=nil) then Break;
  Assert(tmp<>Result);
  Result:=tmp;
 end;
end;

function RegDownSlot(node:PsrRegNode):PsrRegNode;
var
 tmp:PsrRegNode;
begin
 //backtrace
 Result:=node;
 While (Result<>nil) do
 begin
  tmp:=Result^.AsReg; //next
  if (tmp=nil) then Break;
  Assert(tmp<>Result);
  if (tmp^.pSlot<>Result^.pSlot) then Break;
  Result:=tmp;
 end;
end;

function CompareReg(r1,r2:PsrRegNode):Boolean;
begin
 r1:=RegDownSlot(r1);
 r2:=RegDownSlot(r2);
 Result:=(r1=r2);
 if not Result then
 begin
  Result:=CompareConst(r1^.AsConst,r2^.AsConst);
 end;
end;

end.

