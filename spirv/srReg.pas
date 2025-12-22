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

const
 RegCount =366;
 LaneCount=64;

type
 PPsrRegSlot=^PsrRegSlot;
 PsrRegSlot=^TsrRegSlot;

 TsrRegNode=class;

 PPsrRegNode=^TsrRegNode;

 TsrRegNode=class(TsrNode)
  private
   ID:TsrRefId;            //post id
   F:bitpacked record
    dtype:TsrDataType;
    dweak:Boolean;
    dwave:Boolean;
    dsbgr:Boolean;
   end;
   FSlot:PsrRegSlot;
   FWriter:TsrNode;        //ntReg,ntConst,ntOp,ntVolatile
   FCustomLine:TsrNode;    //PspirvOp;
   function  GetDtype:TsrDataType;
   Procedure SetDtype(t:TsrDataType);
   function  GetWeak:Boolean;
   Procedure SetWeak(t:Boolean);
   function  GetWave:Boolean;
   Procedure SetWave(t:Boolean);
   function  GetSubgr:Boolean;
   Procedure SetSubgr(t:Boolean);
   Procedure SetWriter(t:TsrNode);
   Function  GetWriter:TsrNode;
  public
   function  pLine:TsrNode;
   property  CustomLine:TsrNode read FCustomLine write FCustomLine;
   //
   Function  _GetPline    :TsrNode;              override;
   Procedure _SetWriter   (w,line:TsrNode);      override;
   Procedure _ResetWriter (w:TsrNode);           override;
   function  _Down        :TsrNode;              override;
   Procedure _PrepType    (node:PPrepTypeNode);  override;
   function  _GetPrintName:RawByteString;        override;
   function  _GetRef      :Pointer;              override;
   //
   property  pSlot  :PsrRegSlot  read FSlot     write FSlot;
   property  dtype  :TsrDataType read GetDtype  write SetDtype;
   property  dweak  :Boolean     read GetWeak   write SetWeak;
   property  dwave  :Boolean     read GetWave   write SetWave;
   property  dsbgr  :Boolean     read GetSubgr  write SetSubgr;
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
   Function  _GetPline:TsrNode;            override;
   //
   property  pLine:TsrNode   read FWriter;
   property  pWriter:TsrNode read FWriter write SetWriter;
   property  pDst0:TsrNode   read FDst0   write SetDst0;
   property  pDst1:TsrNode   read FDst1   write SetDst1;
 end;

 ntRegPair=TsrRegPair;

 TsrRegSampledImage=class(TsrRegNode)
  Tgrp :TsrNode;
  Sgrp :TsrNode;
  etype:TsrDataType;
  info :TsrTypeImageInfo;
 end;

 TsrVectorArray=class(TsrRegNode)
  FLanes:array[0..LaneCount-1] of TsrRegNode; //[0..63]
 end;

 TString7=string[7];

 TSlotCategory=(cNone,cUnattach,cScalar,cScc,cVector,cVectorArray,cLane);

 PsrBitKey=^TsrBitKey;
 TsrBitKey=bitpacked record
  id      :0..RegCount-1;
  Category:TSlotCategory;
  LaneId  :0..LaneCount-1;
  align   :Byte;
 end;

 TsrRegSlot=object
  private
   FEmit   :TCustomEmit;
   FCurrent:TsrRegNode;
   pLanes  :PsrRegSlot; //[0..63]
   FName   :TString7;
   FBits   :TsrBitKey;
   procedure set_current(c:TsrRegNode);
  public
   FPrivate:TsrNode;
   property  Emit    :TCustomEmit   read FEmit;
   property  current :TsrRegNode    read FCurrent write set_current;
   property  Name    :TString7      read FName;
   property  Category:TSlotCategory read FBits.Category;
   property  id      :SmallInt      read FBits.id;
   property  LaneId  :ShortInt      read FBits.LaneId;
   function  Lanes(i:Byte):PsrRegSlot;
   Procedure Init(e:TCustomEmit;const n:TString7;c:TSlotCategory;i:Word=0;l:Byte=0);
   function  ConvertToVectorArray  :Boolean;
   function  ConvertToVectorGeneral:Boolean;
   function  isBoolOnly:Boolean;
   function  isScalar:Boolean;
   function  iUnattach:Boolean;
   function  _New(rtype:TsrDataType;pLine:TsrRegNode):TsrRegNode;
   function  New(rtype:TsrDataType;pLine:TsrRegNode=nil):TsrRegNode;
 end;

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
  block:TsrNode;
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
  function  get_sdst8(SDST:Word):PsrRegSlot;
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
function is_dwave(node:TsrRegNode):Boolean;

operator := (i:TsrNode):TsrRegNode; inline;

implementation

operator := (i:TsrNode):TsrRegNode; inline;
begin
 Result:=TsrRegNode(Pointer(i)); //typecast hack
end;

//

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

Function TsrRegNode._GetPline:TsrNode;
var
 tmp:TsrRegNode;
begin
 Result:=nil;
 //
 if (FCustomLine<>nil) then
 begin
  Exit(FCustomLine);
 end;
 //
 tmp:=FWriter;
 while (tmp<>nil) do
 begin
  if tmp.IsType(ntReg) then
  begin
   if (tmp.FCustomLine<>nil) then
   begin
    Exit(tmp.FCustomLine);
   end;
  end else
  begin
   Exit(tmp._GetPline);
  end;
  tmp:=tmp._Down;
 end;
end;

function TsrRegNode.pLine:TsrNode;
begin
 if (Self=nil) then Exit(nil);
 Result:=_GetPline;
end;

Procedure TsrRegNode._SetWriter(w,line:TsrNode);
begin
 SetWriter(w);
end;

Procedure TsrRegNode._ResetWriter(w:TsrNode);
begin
 if (FWriter=w) then
 begin
  SetWriter(nil);
 end;
end;

function TsrRegNode._Down:TsrNode;
begin
 Result:=FWriter;
end;

Procedure TsrRegNode._PrepType(node:PPrepTypeNode);
var
 new:TsrDataType;
 pConstList:PsrConstList;
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

Function TsrRegPair._GetPline:TsrNode;
begin
 Result:=FWriter;
end;

//

Procedure TsrRegsStory.Init(Emit:TCustomEmit);
var
 i:Word;
 n:TString7;
begin
 FillChar(Self,SizeOf(TsrRegsStory),0);
 For i:=0 to 103 do
 begin
  Str(i,n);
  SGRP[i].Init(Emit,'S'+n,cScalar,i);
 end;
 VCC[0].Init(Emit,'VCCL',cScalar);
 VCC[1].Init(Emit,'VCCH',cScalar);
 M0.Init(Emit,'M0',cScalar);
 EXEC[0].Init(Emit,'EXECL',cScalar);
 EXEC[1].Init(Emit,'EXECH',cScalar);
 SCC.Init(Emit,'SCC',cScc);
 For i:=0 to 255 do
 begin
  Str(i,n);
  n:='V'+n;
  VGRP[i].Init(Emit,n,cVector);
 end;
 FUnattach.Init(Emit,'UNATT',cUnattach);
 //init order
 For i:=0 to RegCount-1 do
 begin
  SLOT[i].FBits.id:=i;
 end;
 //init order
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

function TsrRegsStory.get_sdst8(SDST:Word):PsrRegSlot;
begin
 Result:=get_ssrc8(SDST);
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
    //
    Assert(src[0]^.Category<>cVectorArray,'TODO:fetch_ssrc9_pair cVectorArray');
    Assert(src[1]^.Category<>cVectorArray,'TODO:fetch_ssrc9_pair cVectorArray');
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
 PTR :PsrRegSlot;
 Emit:TCustomEmit;
 node:TsrVectorArray;
 i,p:Word;
begin
 Emit:=SLOT[0].Emit;
 //
 For i:=0 to RegCount-1 do
 begin
  PTR:=@PsrRegSlot(SLOT)[i];

  if (PTR^.pLanes<>nil) and
     (PTR^.Category=cVectorArray) then
  begin
   node:=Emit.specialize New<TsrVectorArray>;
   //fill lanes
   For p:=0 to LaneCount-1 do
   begin
    node.FLanes[p]:=PTR^.pLanes[p].current;
   end;
   //
   Result.REGS[i]:=node;
  end else
  begin
   Result.REGS[i]:=PTR^.current;
  end;

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
 i,p:Word;
 PTR:PsrRegSlot;
 nodes:record
  orig:TsrVectorArray;
 end;
 regs:record
  orig:TsrRegNode;
 end;
begin
 if (cb=nil) then Exit;
 //
 For i:=0 to RegCount-1 do
 begin
  PTR:=@PsrRegSlot(SLOT)[i];

  if ((orig^.REGS[i]=nil) or (orig^.REGS[i].IsType(TsrVectorArray))) and
     (PTR^.Category=cVectorArray) then
  begin
   nodes.orig:=orig^.REGS[i].specialize AsType<TsrVectorArray>;

   For p:=0 to LaneCount-1 do
   begin

    if (nodes.orig<>nil) then
    begin
     regs.orig:=nodes.orig.FLanes[p];
    end else
    begin
     regs.orig:=nil;
    end;

    cb(@PTR^.pLanes[p],regs.orig);

    if (nodes.orig=nil) and (regs.orig<>nil) then
    begin
     nodes.orig:=SLOT[0].Emit.specialize New<TsrVectorArray>;
     orig^.REGS[i]:=nodes.orig;
    end;

    if (nodes.orig<>nil) then
    begin
     nodes.orig.FLanes[p]:=regs.orig;
    end;

   end;

  end else
  begin
   cb(PTR,orig^.REGS[i]);
  end;

 end;
end;

procedure TsrRegsStory.ForEachSnap(cb:TForEachSnp3;var ctx:TsrVolatileContext;orig,prev,next:PsrRegsSnapshot);
var
 i,p:Word;
 PTR:PsrRegSlot;
 nodes:record
  orig:TsrVectorArray;
  prev:TsrVectorArray;
  next:TsrVectorArray;
 end;
 regs:record
  orig:TsrRegNode;
  prev:TsrRegNode;
  next:TsrRegNode;
 end;
begin
 if (cb=nil) then Exit;
 //
 For i:=0 to RegCount-1 do
 begin
  PTR:=@PsrRegSlot(SLOT)[i];

  if ((orig^.REGS[i]=nil) or (orig^.REGS[i].IsType(TsrVectorArray))) and
     ((prev^.REGS[i]=nil) or (prev^.REGS[i].IsType(TsrVectorArray))) and
     ((next^.REGS[i]=nil) or (next^.REGS[i].IsType(TsrVectorArray))) and
     (PTR^.Category=cVectorArray) then
  begin
   nodes.orig:=orig^.REGS[i].specialize AsType<TsrVectorArray>;
   nodes.prev:=prev^.REGS[i].specialize AsType<TsrVectorArray>;
   nodes.next:=next^.REGS[i].specialize AsType<TsrVectorArray>;

   For p:=0 to LaneCount-1 do
   begin

    if (nodes.orig<>nil) then
    begin
     regs.orig:=nodes.orig.FLanes[p];
    end else
    begin
     regs.orig:=nil;
    end;

    if (nodes.prev<>nil) then
    begin
     regs.prev:=nodes.prev.FLanes[p];
    end else
    begin
     regs.prev:=nil;
    end;

    if (nodes.next<>nil) then
    begin
     regs.next:=nodes.next.FLanes[p];
    end else
    begin
     regs.next:=nil;
    end;

    cb(ctx,@PTR^.pLanes[p],regs.orig,regs.prev,regs.next);

    if (nodes.orig=nil) and (regs.orig<>nil) then
    begin
     nodes.orig:=SLOT[0].Emit.specialize New<TsrVectorArray>;
     orig^.REGS[i]:=nodes.orig;
    end;

    if (nodes.prev=nil) and (regs.prev<>nil) then
    begin
     nodes.prev:=SLOT[0].Emit.specialize New<TsrVectorArray>;
     prev^.REGS[i]:=nodes.prev;
    end;

    if (nodes.next=nil) and (regs.next<>nil) then
    begin
     nodes.next:=SLOT[0].Emit.specialize New<TsrVectorArray>;
     next^.REGS[i]:=nodes.next;
    end;

    if (nodes.orig<>nil) then
    begin
     nodes.orig.FLanes[p]:=regs.orig;
    end;

    if (nodes.prev<>nil) then
    begin
     nodes.prev.FLanes[p]:=regs.prev;
    end;

    if (nodes.next<>nil) then
    begin
     nodes.next.FLanes[p]:=regs.next;
    end;

   end;

  end else
  begin
   cb(ctx,PTR,orig^.REGS[i],prev^.REGS[i],next^.REGS[i]);
  end;

 end;
end;

//--

function TsrRegNode.GetName:RawByteString;
begin
 Result:='';
 if (FSlot<>nil) then Result:=FSlot^.Name;
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

function TsrRegNode.GetWave:Boolean;
begin
 Result:=F.dwave;
end;

Procedure TsrRegNode.SetWave(t:Boolean);
begin
 F.dwave:=t;
end;

function TsrRegNode.GetSubgr:Boolean;
begin
 Result:=F.dsbgr;
end;

Procedure TsrRegNode.SetSubgr(t:Boolean);
begin
 F.dsbgr:=t;
end;

Procedure TsrRegNode.SetWriter(t:TsrNode);
begin
 if (Self=nil) then Exit;
 if (FWriter=t) then Exit;

 Assert(RegDown(t.specialize AsType<ntReg>)<>Self,'Circular reference');

 if isUsed then
 begin
        t.mark_read  (Self);
  FWriter.mark_unread(Self);
 end;
 FWriter:=t;

 //update
 if (FWriter=nil) then
 begin
  FCustomLine:=nil;
 end else
 if (FWriter._GetPline<>nil) then
 begin
  FCustomLine:=nil;
 end;
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

procedure TsrRegSlot.set_current(c:TsrRegNode);
var
 p:Word;
 node:TsrVectorArray;
begin
 if c.IsType(TsrVectorArray) then
 begin
  ConvertToVectorArray;
  //
  node:=c.specialize AsType<TsrVectorArray>;
  //
  For p:=0 to LaneCount-1 do
  begin
   pLanes[p].FCurrent:=node.FLanes[p];
  end;
  //
  Exit;
 end else
 if (FBits.Category=cVectorArray) then
 begin
  if (c=nil) then
  begin
   //
   For p:=0 to LaneCount-1 do
   begin
    pLanes[p].FCurrent:=nil;
   end;
   //
   Exit;
  end;
  ConvertToVectorGeneral;
 end;
 //
 FCurrent:=c;
end;

function TsrRegSlot.Lanes(i:Byte):PsrRegSlot;
begin
 if (pLanes<>nil) and
    (FBits.Category=cVectorArray) then
 begin
  Result:=@pLanes[i and (LaneCount-1)];
 end else
 begin
  Result:=nil;
 end;
end;

Procedure TsrRegSlot.Init(e:TCustomEmit;const n:TString7;c:TSlotCategory;i:Word=0;l:Byte=0);
begin
 FEmit:=e;
 FName:=n;
 FBits.Category:=c;
 FBits.id      :=i;
 FBits.LaneId  :=l;
end;

function TsrRegSlot.ConvertToVectorArray:Boolean;
var
 i:Byte;
 n:TString7;
begin
 case FBits.Category of
  cVector:
   begin
    FBits.Category:=cVectorArray;
    //
    if (pLanes=nil) then
    begin
     pLanes:=FEmit.Alloc(SizeOf(TsrRegSlot)*LaneCount);
     //
     For i:=0 to LaneCount-1 do
     begin
      Str(i,n);
      n:=Self.Name+'.'+n;
      pLanes[i].Init(Emit,n,cLane,Self.id,i);
     end;
    end;
    //
    For i:=0 to LaneCount-1 do
    begin
     pLanes[i].FCurrent:=current;
    end;
    //
    FCurrent:=nil;
    Exit(True);
   end;
  cVectorArray:
   Exit(True);
  else
   Exit(False);
 end;
end;

function TsrRegSlot.ConvertToVectorGeneral:Boolean;
var
 i:Byte;
begin
 case FBits.Category of
  cVectorArray:
   begin
    FBits.Category:=cVector;
    //
    For i:=0 to LaneCount-1 do
    begin
     pLanes[i].FCurrent:=nil;
    end;
    //
    Exit(True);
   end;
  cVector:
   Exit(True);
  else
   Exit(False);
 end;
end;

function TsrRegSlot.isBoolOnly:Boolean;
begin
 Result:=(FBits.Category=cScc);
end;

function TsrRegSlot.isScalar:Boolean;
begin
 Result:=(FBits.Category in [cScalar,cScc]);
end;

function TsrRegSlot.iUnattach:Boolean;
begin
 Result:=(FBits.Category=cUnattach);
end;

function TsrRegSlot._New(rtype:TsrDataType;pLine:TsrRegNode):TsrRegNode;
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
 node.CustomLine:=pLine;
 Result:=node;
end;

function TsrRegSlot.New(rtype:TsrDataType;pLine:TsrRegNode=nil):TsrRegNode;
begin
 Result:=_New(rtype,pLine);
 //update
 current:=Result;
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
 Exit(RegDown(node));

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

function is_dwave(node:TsrRegNode):Boolean;
var
 tmp:TsrRegNode;
begin
 Result:=False;
 //backtrace
 While (node<>nil) do
 begin
  if node.dwave then
  begin
   Exit(True);
  end;
  //
  tmp:=node.AsReg; //next
  if (tmp=nil) then Break;
  Assert(tmp<>node);
  node:=tmp;
 end;
end;

end.

