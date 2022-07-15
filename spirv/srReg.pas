unit srReg;

{$mode objfpc}{$H+}

interface

uses
  srNodes,
  srRefId,
  srTypes,
  srConst;

type
 PPsrRegSlot=^PsrRegSlot;
 PsrRegSlot=^TsrRegSlot;

 PPsrRegNode=^PsrRegNode;
 PsrRegNode=^TsrRegNode;
 TsrRegNode=packed object
  pPrev,pNext:PsrRegNode;
  //----
  read_count:DWORD;
  ID:TsrRefId; //post id
  pSlot:PsrRegSlot;
  pWriter:TOpParamSingle; //ntReg,ntConst,ntOp,ntVolatile
  pLine:Pointer; //PspirvOp;
  dtype:TsrDataType;
  function  GetName:RawByteString;
  Procedure mark_read;
  Procedure mark_unread;
  procedure SetConst(pConst:PsrConst);
  procedure SetReg(pReg:PsrRegNode);
  function  AsConst:PsrConst;
  function  AsOp:Pointer;
  function  AsReg:PsrRegNode;
  function  is_const:Boolean;
  function  is_bool:Boolean;
  function  is_bool_or_const_bool:Boolean;
  function  is_unknow:Boolean;
 end;

 TRegStory=specialize TNodeList<PsrRegNode>;

 TString5=string[5];

 TsrRegSlot=object
  Alloc:TfnAlloc;
  pVar:Pointer;
  pStory:TRegStory;
  rid:TString5;
  Procedure Init(a:TfnAlloc;const n:TString5);
  function  first:PsrRegNode;
  function  current:PsrRegNode;
  function  New(pLine:Pointer;rtype:TsrDataType):PsrRegNode;
  function  NewAfter(rtype:TsrDataType;r:PsrRegNode):PsrRegNode;
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

 TsrRegsStory=object
  SGRP:array[0..103] of TsrRegSlot;
  VCC:array[0..1] of TsrRegSlot;
  M0:TsrRegSlot;
  EXEC:array[0..1] of TsrRegSlot;
  SCC:TsrRegSlot;
  VGRP:array[0..255] of TsrRegSlot;
  FUnattach:TsrRegSlot;
  Procedure Init(Alloc:TfnAlloc);
  function  get_sdst7_pair(SDST:Byte;dst:PPsrRegSlot):Boolean;
  function  get_sdst7(SDST:Byte):PsrRegSlot;
  function  get_ssrc8(SSRC:Byte):PsrRegSlot;
  function  get_ssrc9_pair(SSRC:Word;src:PPsrRegSlot):Boolean;
  function  get_ssrc9(SSRC:Word):PsrRegSlot;
  function  get_vsrc8(VSRC:Byte):PsrRegSlot;
  function  get_vdst8(VDST:Byte):PsrRegSlot;
  function  get_sbase(SBASE,count:Byte;src:PPsrRegSlot):Boolean;
  function  get_srsrc(SRSRC,count:Byte;src:PPsrRegSlot):Boolean;

  function  get_snapshot:TsrRegsSnapshot;
  procedure ForEachSlot(cb:TForEachSlot);
  procedure ForEachSnap(cb:TForEachSnap;old:PsrRegsSnapshot);
 end;

implementation

Procedure TsrRegsStory.Init(Alloc:TfnAlloc);
var
 i:Word;
 n:TString5;
begin
 FillChar(Self,SizeOf(TsrRegsStory),0);
 For i:=0 to 103 do
 begin
  Str(i,n);
  SGRP[i].Init(Alloc,'S'+n);
 end;
 VCC[0].Init(Alloc,'VCCL');
 VCC[1].Init(Alloc,'VCCH');
 M0.Init(Alloc,'M0');
 EXEC[0].Init(Alloc,'EXECL');
 EXEC[1].Init(Alloc,'EXECH');
 SCC.Init(Alloc,'SCC');
 For i:=0 to 255 do
 begin
  Str(i,n);
  VGRP[i].Init(Alloc,'V'+n);
 end;
 FUnattach.Init(Alloc,'UNATT');
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

function TsrRegNode.GetName:RawByteString;
begin
 Result:='';
 if (pSlot<>nil) then Result:=pSlot^.rid;
end;

Procedure TsrRegNode.mark_read;
begin
 Inc(read_count);
end;

Procedure TsrRegNode.mark_unread;
begin
 Assert(read_count<>0);
 if (read_count<>0) then Dec(read_count);
end;

procedure TsrRegNode.SetConst(pConst:PsrConst);
begin
 pWriter.SetParam(ntConst,pConst);
 dtype:=pConst^.key.dtype;
end;

procedure TsrRegNode.SetReg(pReg:PsrRegNode);
begin
 Assert(@Self<>pReg);
 pWriter.SetParam(ntReg,pReg);
end;

function TsrRegNode.AsConst:PsrConst;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (pWriter.ntype<>ntConst) then Exit;
 Result:=pWriter.pData;
end;

function TsrRegNode.AsOp:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (pWriter.ntype<>ntOp) then Exit;
 Result:=pWriter.pData;
end;

function TsrRegNode.AsReg:PsrRegNode;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if (pWriter.ntype<>ntReg) then Exit;
 Result:=pWriter.pData;
end;

function TsrRegNode.is_const:Boolean;
begin
 Result:=(pWriter.ntype=ntConst);
end;

function TsrRegNode.is_bool:Boolean;
begin
 Result:=(dtype=dtBool);
end;

function TsrRegNode.is_bool_or_const_bool:Boolean;
begin
 Result:=is_bool or
  (is_const and (AsConst<>nil) and AsConst^.isBoolVal);
end;

function TsrRegNode.is_unknow:Boolean;
begin
 Result:=(dtype=dtUnknow);
end;

//--

Procedure TsrRegSlot.Init(a:TfnAlloc;const n:TString5);
begin
 Alloc:=a;
 rid  :=n;
end;

function TsrRegSlot.first:PsrRegNode;
begin
 Result:=pStory.pHead;
end;

function TsrRegSlot.current:PsrRegNode;
begin
 Result:=pStory.pTail;
end;

function TsrRegSlot.New(pLine:Pointer;rtype:TsrDataType):PsrRegNode;
var
 node:PsrRegNode;
begin
 node:=Alloc(SizeOf(TsrRegNode));
 node^.pSlot:=@Self;
 node^.dtype:=rtype;
 node^.pLine:=pLine;
 pStory.Push_tail(node);
 Result:=node;
end;

function TsrRegSlot.NewAfter(rtype:TsrDataType;r:PsrRegNode):PsrRegNode;
var
 node:PsrRegNode;
begin
 node:=Alloc(SizeOf(TsrRegNode));
 node^.pSlot:=@Self;
 node^.dtype:=rtype;
 node^.pLine:=r^.pLine;
 pStory.InsertAfter(r,node);
 Result:=node;
end;

procedure TsrRegSlot.Remove(r:PsrRegNode);
begin
 if (r=nil) then Exit;
 pStory.Remove(r);
end;

end.

