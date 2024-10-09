unit srReg;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 //TypInfo,
 ginodes,
 srNode,
 srRefId,
 srType,
 srConst;

type
 PPsrRegSlot=^PsrRegSlot;
 PsrRegSlot=^TsrRegSlot;

 TsrRegNode=class;

 PPsrRegNode=^TsrRegNode;

 TsrRegNode=class(TsrNode)
  public
   pPrev,pNext:TsrRegNode;
  private
   ID:TsrRefId;            //post id
   F:bitpacked record
    dtype:TsrDataType;
    dweak:Boolean;
   end;
   FSlot:PsrRegSlot;
   FWriter:TsrNode;        //ntReg,ntConst,ntOp,ntVolatile
   function  GetDtype:TsrDataType;
   Procedure SetDtype(t:TsrDataType);
   function  GetWeak:Boolean;
   Procedure SetWeak(t:Boolean);
   Procedure SetWriter(t:TsrNode);
   Function  GetWriter:TsrNode;
  public
   pLine:TsrNode;   //PspirvOp;
   //
   Procedure _SetWriter   (w,line:TsrNode);      override;
   Procedure _ResetWriter (w:TsrNode);           override;
   function  _Down        :TsrNode;              override;
   function  _Next        :TsrNode;              override;
   function  _Prev        :TsrNode;              override;
   Procedure _PrepType    (node:PPrepTypeNode);  override;
   function  _GetPrintName:RawByteString;        override;
   function  _GetRef      :Pointer;              override;
   //
   property  pSlot  :PsrRegSlot  read FSlot;
   property  dtype  :TsrDataType read GetDtype  write SetDtype;
   property  dweak  :Boolean     read GetWeak   write SetWeak;
   property  pWriter:TsrNode     read GetWriter write SetWriter;
   function  GetName:RawByteString;
   function  AsConst:TsrConst;
   function  AsReg:TsrRegNode;
   function  is_const:Boolean;
   function  is_bool:Boolean;
   function  is_bool_or_const_bool:Boolean;
   function  is_unknow:Boolean;
   function  GetPrintName:RawByteString;
 end;

 ntReg=TsrRegNode;

 TsrRegPair=class(TsrNode)
  private
   FWriter:TsrNode;
   FDst0:TsrNode;
   FDst1:TsrNode;
   Procedure SetWriter(t:TsrNode);
   Procedure SetDst0(r:TsrNode);
   Procedure SetDst1(r:TsrNode);
  public
   //
   function  _Down:TsrNode;                override;
   Procedure _SetWriter  (w,line:TsrNode); override;
   Procedure _ResetWriter(w:TsrNode);      override;
   //
   property  pLine:TsrNode   read FWriter;
   property  pWriter:TsrNode read FWriter write SetWriter;
   property  pDst0:TsrNode   read FDst0   write SetDst0;
   property  pDst1:TsrNode   read FDst1   write SetDst1;
 end;

 ntRegPair=TsrRegPair;

 TRegStory=specialize TNodeListClass<TsrRegNode>;

 TString5=string[5];

 TsrRegSlot=object
  private
   FEmit:TCustomEmit;
   pStory:TRegStory;
   Frid:TString5;
  public
   current:TsrRegNode;
   property  Emit:TCustomEmit read FEmit;
   property  rid :TString5    read Frid;
   Procedure Init(e:TCustomEmit;const n:TString5);
   function  first:TsrRegNode;
   function  last :TsrRegNode;
   function  isBoolOnly:Boolean;
   function  New(pLine:TsrNode;rtype:TsrDataType):TsrRegNode;
   function  NewAfter(rtype:TsrDataType;r:TsrRegNode):TsrRegNode;
   function  NewBefore(rtype:TsrDataType;r:TsrRegNode):TsrRegNode;
   procedure Insert(r:TsrRegNode);
   procedure Remove(r:TsrRegNode);
 end;

const
 RegCount=366;

type
 PsrRegsSnapshot=^TsrRegsSnapshot;
 TsrRegsSnapshot=record
  //366
  case Byte of
   0:(REGS:array[0..365] of TsrRegNode;);
   1:(
      SGRP:array[0..103] of TsrRegNode; //104
      VCC :array[0..1] of TsrRegNode;   //2
      M0  :TsrRegNode;                  //1
      EXEC:array[0..1] of TsrRegNode;   //2
      SCC :TsrRegNode;                  //1
      VGRP:array[0..255] of TsrRegNode; //256
     );
 end;

 TsrVolatileNode=class
  pPrev,pNext:TsrVolatileNode;
  //
  V:TsrNode;
  N:TsrNode;
 end;
 TsrVolatileNodeList=specialize TNodeListClass<TsrVolatileNode>;

 TsrVolatileContext=object
  Emit:TCustomEmit;
  //
  befor:TsrNode;
  after:TsrNode;
  //
  FList:TsrVolatileNodeList;
  //
  Procedure AddVolatile(V,N:TsrNode);
 end;

 TForEachSlot=procedure(pSlot:PsrRegSlot) of object;
 TForEachSnp1=procedure(pSlot:PsrRegSlot;var orig:TsrRegNode) of object;
 TForEachSnp3=procedure(var ctx:TsrVolatileContext;pSlot:PsrRegSlot;var orig,prev,next:TsrRegNode) of object;

 PsrRegsStory=^TsrRegsStory;
 TsrRegsStory=object
  //366
  SGRP:array[0..103] of TsrRegSlot; //104
  VCC :array[0..1] of TsrRegSlot;   //2
  M0  :TsrRegSlot;                  //1
  EXEC:array[0..1] of TsrRegSlot;   //2
  SCC :TsrRegSlot;                  //1
  VGRP:array[0..255] of TsrRegSlot; //256
  FUnattach:TsrRegSlot;
  //
  Procedure Init(Emit:TCustomEmit);
  Function  SLOT:PsrRegSlot; inline;
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
  procedure ForEachSnap(cb:TForEachSnp1;orig:PsrRegsSnapshot);
  procedure ForEachSnap(cb:TForEachSnp3;var ctx:TsrVolatileContext;orig,prev,next:PsrRegsSnapshot);
 end;

function RegDown(node:TsrRegNode):TsrRegNode;
function RegDownSlot(node:TsrRegNode):TsrRegNode;
function CompareReg(r1,r2:TsrRegNode):Boolean;

operator := (i:TsrNode):TsrRegNode; inline;

implementation

operator := (i:TsrNode):TsrRegNode; inline;
begin
 Result:=TsrRegNode(Pointer(i)); //typecast hack
end;

//

{
TsrVolatileContext=object
 befor:TsrNode;
 after:TsrNode;
 //
 FList:TDependenceNodeList;
 //
}

Procedure TsrVolatileContext.AddVolatile(V,N:TsrNode);
var
 node:TsrVolatileNode;
begin
 node:=Emit.specialize New<TsrVolatileNode>;
 node.V:=V;
 node.N:=N;
 //
 FList.Push_tail(node);
end;

//

Procedure TsrRegNode._SetWriter(w,line:TsrNode);
begin
 SetWriter(w);
 pLine:=line;
end;

Procedure TsrRegNode._ResetWriter(w:TsrNode);
begin
 if (FWriter=w) then
 begin
  SetWriter(nil);
  pLine:=nil;
 end;
end;

function TsrRegNode._Down:TsrNode;
begin
 Result:=FWriter;
end;

function TsrRegNode._Next:TsrNode;
begin
 Result:=pNext;
end;

function TsrRegNode._Prev:TsrNode;
begin
 Result:=pPrev;
end;

Procedure TsrRegNode._PrepType(node:PPrepTypeNode);
var
 new:TsrDataType;
 pConstList:PsrConstList;
begin
 With TsrRegNode(node^.dnode) do
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
  if is_unprep_type(dtype,new,dweak) then
  begin
   dtype:=new;
   dweak:=True;
   if is_const then
   begin
    pConstList:=FSlot^.FEmit.GetConstList;
    pWriter:=pConstList^.Bitcast(new,pWriter.specialize AsType<ntConst>);
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

function TsrRegNode._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrRegNode._GetRef:Pointer;
begin
 if is_const then
 begin
  Result:=AsConst.GetRef;
 end else
 begin
  Result:=@ID;
 end;
end;

//

function TsrRegPair._Down:TsrNode;
begin
 Result:=FWriter;
end;

Procedure TsrRegPair._SetWriter(w,line:TsrNode);
begin
 SetWriter(w);
end;

Procedure TsrRegPair._ResetWriter(w:TsrNode);
begin
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

Function TsrRegsStory.SLOT:PsrRegSlot; inline;
begin
 Result:=@Self;
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
 //
 For i:=0 to RegCount-1 do
 begin
  Result.REGS[i]:=SLOT[i].current;
 end;
end;

procedure TsrRegsStory.ForEachSlot(cb:TForEachSlot);
var
 i:Word;
 PTR:PsrRegSlot;
begin
 if (cb=nil) then Exit;
 //
 PTR:=SLOT;
 For i:=0 to RegCount-1 do
 begin
  cb(@PTR[i]);
 end;
end;

procedure TsrRegsStory.ForEachSnap(cb:TForEachSnp1;orig:PsrRegsSnapshot);
var
 i:Word;
 PTR:PsrRegSlot;
begin
 if (cb=nil) then Exit;
 //
 PTR:=SLOT;
 For i:=0 to RegCount-1 do
 begin
  cb(@PTR[i],orig^.REGS[i]);
 end;
end;

procedure TsrRegsStory.ForEachSnap(cb:TForEachSnp3;var ctx:TsrVolatileContext;orig,prev,next:PsrRegsSnapshot);
var
 i:Word;
 PTR:PsrRegSlot;
begin
 if (cb=nil) then Exit;
 //
 PTR:=SLOT;
 For i:=0 to RegCount-1 do
 begin
  cb(ctx,@PTR[i],orig^.REGS[i],prev^.REGS[i],next^.REGS[i]);
 end;
end;

//--

function TsrRegNode.GetName:RawByteString;
begin
 Result:='';
 if (FSlot<>nil) then Result:=FSlot^.rid;
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
 Result:=F.dweak;
end;

Procedure TsrRegNode.SetWeak(t:Boolean);
begin
 F.dweak:=t;
end;

Procedure TsrRegNode.SetWriter(t:TsrNode);
begin
 if (Self=nil) then Exit;
 if (FWriter=t) then Exit;

 {
 if t.IsType(ntConst) then
 if (dtype<>TsrConst(t).dtype) then
 begin
  Assert(false);
 end;
 }

 Assert(RegDown(t.specialize AsType<ntReg>)<>Self,'Circular reference');

 if isUsed then
 begin
        t.mark_read  (Self);
  FWriter.mark_unread(Self);
 end;
 FWriter:=t;
end;

Function TsrRegNode.GetWriter:TsrNode;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 Result:=FWriter;
end;

function TsrRegNode.AsConst:TsrConst;
begin
 if (Self=nil) then Exit(nil);
 Result:=FWriter.specialize AsType<ntConst>;
end;

function TsrRegNode.AsReg:TsrRegNode;
begin
 if (Self=nil) then Exit(nil);
 Result:=FWriter.specialize AsType<ntReg>;
end;

function TsrRegNode.is_const:Boolean;
begin
 if (Self=nil) then Exit(False);
 Result:=FWriter.IsType(ntConst);
end;

function TsrRegNode.is_bool:Boolean;
begin
 if (Self=nil) then Exit(False);
 Result:=(dtype=dtBool);
end;

function TsrRegNode.is_bool_or_const_bool:Boolean;
begin
 if (Self=nil) then Exit(False);
 Result:=is_bool or
  (is_const and AsConst.isBoolVal);
end;

function TsrRegNode.is_unknow:Boolean;
begin
 if (Self=nil) then Exit(True);
 Result:=(dtype=dtUnknow);
end;

function TsrRegNode.GetPrintName:RawByteString;
begin
 if is_const then
 begin
  Result:=AsConst.GetPrintName;
 end else
 begin
  Assert(ID.Alloc);
  Result:='r'+IntToStr(ID.ID){+'_'+GetEnumName(typeInfo(TsrDataType), Ord(dtype))};
 end;
end;

//


Procedure TsrRegPair.SetWriter(t:TsrNode);
begin
 if (FWriter=t) then Exit;
 if isUsed then
 begin
        t.mark_read  (Self);
  FWriter.mark_unread(Self);
 end;
 FWriter:=t;
end;

Procedure TsrRegPair.SetDst0(r:TsrNode);
begin
 if (FDst0=r) then Exit;
 FDst0.ResetWriter(Self);
 FDst0:=r;
 FDst0.SetWriter(Self,pLine);
end;

Procedure TsrRegPair.SetDst1(r:TsrNode);
begin
 if (FDst1=r) then Exit;
 FDst1.ResetWriter(Self);
 FDst1:=r;
 FDst1.SetWriter(Self,pLine);
end;

//--

Procedure TsrRegSlot.Init(e:TCustomEmit;const n:TString5);
begin
 FEmit:=e;
 Frid :=n;
end;

function TsrRegSlot.first:TsrRegNode;
begin
 Result:=pStory.pHead;
end;

function TsrRegSlot.last:TsrRegNode;
begin
 Result:=pStory.pTail;
end;

function TsrRegSlot.isBoolOnly:Boolean;
begin
 Result:=(rid='SCC');
end;

function TsrRegSlot.New(pLine:TsrNode;rtype:TsrDataType):TsrRegNode;
var
 node:TsrRegNode;
begin
 if isBoolOnly then
 begin
  rtype:=dtBool;
 end;
 node:=FEmit.specialize New<TsrRegNode>;
 node.FSlot:=@Self;
 node.dtype:=rtype;
 node.pLine:=pLine;
 pStory.Push_tail(node);
 Result:=node;
 //update
 current:=pStory.pTail;
end;

function TsrRegSlot.NewAfter(rtype:TsrDataType;r:TsrRegNode):TsrRegNode;
var
 node:TsrRegNode;
begin
 if isBoolOnly then
 begin
  rtype:=dtBool;
 end;
 node:=FEmit.specialize New<TsrRegNode>;
 node.FSlot:=@Self;
 node.dtype:=rtype;
 node.pLine:=r.pLine;
 pStory.InsertAfter(r,node);
 Result:=node;
 //update
 if (r=pStory.pTail) then
 begin
  current:=pStory.pTail;
 end;
end;

function TsrRegSlot.NewBefore(rtype:TsrDataType;r:TsrRegNode):TsrRegNode;
var
 node:TsrRegNode;
begin
 if isBoolOnly then
 begin
  rtype:=dtBool;
 end;
 node:=FEmit.specialize New<TsrRegNode>;
 node.FSlot:=@Self;
 node.dtype:=rtype;
 node.pLine:=r.pLine;
 pStory.InsertBefore(r,node);
 Result:=node;
 //update
 if (r=pStory.pTail) then
 begin
  current:=pStory.pTail;
 end;
end;

procedure TsrRegSlot.Insert(r:TsrRegNode);
begin
 if (r=nil) then Exit;
 pStory.Push_tail(r);
 //update
 current:=pStory.pTail;
end;

procedure TsrRegSlot.Remove(r:TsrRegNode);
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

function RegDown(node:TsrRegNode):TsrRegNode;
var
 tmp:TsrRegNode;
begin
 //backtrace
 Result:=node;
 While (Result<>nil) do
 begin
  tmp:=Result.AsReg; //next
  if (tmp=nil) then Break;
  Assert(tmp<>Result);
  Result:=tmp;
 end;
end;

function RegDownSlot(node:TsrRegNode):TsrRegNode;
var
 tmp:TsrRegNode;
begin
 //backtrace
 Result:=node;
 While (Result<>nil) do
 begin
  tmp:=Result.AsReg; //next
  if (tmp=nil) then Break;
  Assert(tmp<>Result);
  if (tmp.pSlot<>Result.pSlot) then Break;
  Result:=tmp;
 end;
end;

function CompareReg(r1,r2:TsrRegNode):Boolean;
begin
 r1:=RegDownSlot(r1);
 r2:=RegDownSlot(r2);
 Result:=(r1=r2);
 if not Result then
 begin
  Result:=CompareConst(r1.AsConst,r2.AsConst);
 end;
end;

end.

