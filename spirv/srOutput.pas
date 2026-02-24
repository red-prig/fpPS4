unit srOutput;

{$mode ObjFPC}{$H+}

interface

uses
 typinfo,
 spirv,
 srNode,
 srType,
 srOp,
 srReg,
 srLayout,
 srBitcast,
 srVariable,
 srDecorate;

type
 TgcnExportType=(
  etMrt0,etMrt1,etMrt2,etMrt3,
  etMrt4,etMrt5,etMrt6,etMrt7,
  etMrtz,
  etNull,
  etUnknow0,etUnknow1,
  etPos0,etPos1,etPos2,etPos3,
  etUnknow2,etUnknow3,
  etUnknow4,etUnknow5,
  etUnknow6,etUnknow7,
  etUnknow8,etUnknow9,
  etUnknow10,etUnknow11,
  etUnknow12,etUnknow13,
  etUnknow14,etUnknow15,
  etUnknow16,etUnknow17,
  etParam0 ,etParam1 ,etParam2 ,etParam3,
  etParam4 ,etParam5 ,etParam6 ,etParam7,
  etParam8 ,etParam9 ,etParam10,etParam11,
  etParam12,etParam13,etParam14,etParam15,
  etParam16,etParam17,etParam18,etParam19,
  etParam20,etParam21,etParam22,etParam23,
  etParam24,etParam25,etParam26,etParam27,
  etParam28,etParam29,etParam30,etParam31
 );

 TgcnPosType=(
  ptNone,
  ptPointSize,
  ptEdgeFlag,
  ptKillFlag,
  ptGsCutFlag,
  ptRenderTargetIndex,
  ptViewportIndex,
  ptCullDist0,
  ptCullDist1,
  ptCullDist2,
  ptCullDist3,
  ptCullDist4,
  ptCullDist5,
  ptCullDist6,
  ptCullDist7,
  ptClipDist0,
  ptClipDist1,
  ptClipDist2,
  ptClipDist3,
  ptClipDist4,
  ptClipDist5,
  ptClipDist6,
  ptClipDist7
 );

 TDepthMode=(
  foDepthNone,
  foDepthReplacing,
  foDepthGreater,
  foDepthLess,
  foDepthUnchanged
 );

 TsrOutput=class(TsrDescriptor)
  var
   etype:TgcnExportType;
   FLineList:TDependenceNodeList;
  //
  function  _GetStorageName:RawByteString; override;
  //
  function  GetStorageName:RawByteString;
  procedure AddLine(pLine:TSpirvOp);
  Procedure FetchStore(pLine:TSpirvOp;src:TsrRegNode);
  Procedure UpdateRegType;
 end;

 ntOutput=TsrOutput;

 TExportMrt=packed record
  RENDER_FORMAT:Byte;
  NUMBER_TYPE  :Byte;
  COMP_SWAP    :Byte;
  EXPORT_FORMAT:Byte;
 end;

 PExportPos=^TExportPos;
 TExportPos=array[0..3] of TgcnPosType;

 PsrOutputList=^TsrOutputList;
 TsrOutputList=object
  FEmit:TCustomEmit;
  FDepthMode :TDepthMode;
  FExportMrt:array[0..7] of TExportMrt;
  FExportPos:array[0..2] of TExportPos;
  FOUT_CNTL :DWORD;
  data:array[TgcnExportType] of TsrOutput;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(etype:TgcnExportType;rtype:TsrDataType):TsrOutput;
  function  GetExportPos(etype:TgcnExportType):PExportPos;
  procedure Post;
  function  GetExportMask:DWORD;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:TSpirvOp);
 end;

implementation

function TsrOutput._GetStorageName:RawByteString;
begin
 Result:=GetStorageName;
end;

//

function TsrOutput.GetStorageName:RawByteString;
begin
 Result:=GetEnumName(TypeInfo(TgcnExportType),ord(etype));
end;

procedure TsrOutput.AddLine(pLine:TSpirvOp);
var
 node:TDependenceNode;
begin
 node:=NewDependence;
 node.pNode:=pLine;
 FLineList.Push_tail(node);
end;

Procedure TsrOutput.FetchStore(pLine:TSpirvOp;src:TsrRegNode);
begin
 if (src=nil) then Exit;

 pLine:=Emit.OpStore(pLine,FVar,src);

 AddLine(pLine);
end;

Procedure TsrOutput.UpdateRegType;
var
 pBitcastList:PsrBitcastList;
 node:TDependenceNode;
 pLine:TSpirvOp;
 Value:TsrNode;
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

   Op.OpStore:
    begin
     Value:=pLine.ParamNode(1).Value;
     Value.PrepType(ord(rtype));

     dst:=Value.specialize AsType<ntReg>;
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

//

Procedure TsrOutputList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrOutputList.Fetch(etype:TgcnExportType;rtype:TsrDataType):TsrOutput;
begin
 Result:=data[etype];
 //
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrOutput>;
  Result.etype   :=etype;
  Result.FStorage:=StorageClass.Output;
  Result.FBinding:=-1;
  //
  data[etype]:=Result;
 end;
 //
 Result.InitType(rtype);
 Result.InitVar();
end;

function TsrOutputList.GetExportPos(etype:TgcnExportType):PExportPos;
begin
 Result:=nil;
 Case etype of
  etPos1,
  etPos2,
  etPos3:
   begin
    Result:=@FExportPos[ord(etype)-ord(etype)];
   end;
  else;
 end;
end;

//

procedure TsrOutputList.Post;
var
 i:TgcnExportType;
begin
 For i:=Low(TgcnExportType) to High(TgcnExportType) do
 if (data[i]<>nil) then
 begin
  if (data[i].pVar<>nil) and data[i].IsUsed then
  begin
   data[i].UpdateRegType;
  end;
 end;
end;

function TsrOutputList.GetExportMask:DWORD;
var
 i:TgcnExportType;
 pVar:TsrVariable;
begin
 Result:=0;
 For i:=Low(etMrt0) to High(etMrt7) do
 if (data[i]<>nil) then
 begin
  pVar:=data[i].pVar;
  if (pVar<>nil) and data[i].IsUsed then
  begin
   Result:=Result or (1 shl ord(i))
  end;
 end;
end;

procedure TsrOutputList.AllocBinding;
var
 pDecorateList:TsrDecorateList;
 i:TgcnExportType;
 pVar:TsrVariable;
 FLocation:Integer;
begin
 pDecorateList:=FEmit.GetDecorateList;
 For i:=Low(TgcnExportType) to High(TgcnExportType) do
 if (data[i]<>nil) then
 begin
  pVar:=data[i].pVar;
  if (pVar<>nil) and data[i].IsUsed then
  begin
   Case i of
    etMrt0..etMrt7:
      begin
       if (data[i].FBinding=-1) then //alloc
       begin
        FLocation:=ord(i)-ord(etMrt0);
        pDecorateList.OpDecorate(pVar,Decoration.Location,FLocation);
        data[i].FBinding:=FLocation;
       end;
       //Decoration.Index; ???
      end;
    etMrtz:
      begin
       //force Depth Replacing
       FDepthMode:=foDepthReplacing;
       //
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.FragDepth);
      end;
    etPos0:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.Position);
      end;
    etParam0..etParam31: //interpolate param
     begin
      FLocation:=ord(i)-ord(etParam0);
      pDecorateList.OpDecorate(pVar,Decoration.Location,FLocation);
      data[i].FBinding:=FLocation;
     end;
    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TgcnExportType),ord(i)));
   end;
  end;
 end;

end;

procedure TsrOutputList.AllocEntryPoint(EntryPoint:TSpirvOp);
var
 i:TgcnExportType;
 pVar:TsrVariable;
begin
 if (EntryPoint=nil) then Exit;
 For i:=Low(TgcnExportType) to High(TgcnExportType) do
 if (data[i]<>nil) then
 begin
  pVar:=data[i].pVar;
  if (pVar<>nil) and data[i].IsUsed then
  begin
   EntryPoint.AddParam(pVar);
  end;
 end;
end;

end.

