unit srOutput;

{$mode ObjFPC}{$H+}

interface

uses
 typinfo,
 spirv,
 ginodes,
 srNode,
 srType,
 srOp,
 srReg,
 srLayout,
 srBitcast,
 srVariable,
 srDecorate;

type
 TpsslExportType=(
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

 TDepthMode=(
  foDepthNone,
  foDepthReplacing,
  foDepthGreater,
  foDepthLess,
  foDepthUnchanged
 );

 TsrOutput=class(TsrDescriptor)
  var
   etype:TpsslExportType;
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

 PsrOutputList=^TsrOutputList;
 TsrOutputList=object
  FEmit:TCustomEmit;
  FDepthMode:TDepthMode;
  data:array[TpsslExportType] of TsrOutput;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(etype:TpsslExportType;rtype:TsrDataType):TsrOutput;
  procedure Post;
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
 Result:=GetEnumName(TypeInfo(TpsslExportType),ord(etype));
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

function TsrOutputList.Fetch(etype:TpsslExportType;rtype:TsrDataType):TsrOutput;
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

//

procedure TsrOutputList.Post;
var
 i:TpsslExportType;
begin
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
 if (data[i]<>nil) then
 begin
  if (data[i].pVar<>nil) and data[i].IsUsed then
  begin
   data[i].UpdateRegType;
  end;
 end;
end;

procedure TsrOutputList.AllocBinding;
var
 pDecorateList:TsrDecorateList;
 i:TpsslExportType;
 pVar:TsrVariable;
 FLocation:Integer;
begin
 pDecorateList:=FEmit.GetDecorateList;
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
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
    //etPos1..etPos3,
    etParam0..etParam31: //interpolate param
     begin
      FLocation:=ord(i)-ord(etParam0);
      pDecorateList.OpDecorate(pVar,Decoration.Location,FLocation);
      data[i].FBinding:=FLocation;
     end;
    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TpsslExportType),ord(i)));
   end;
  end;
 end;

end;

procedure TsrOutputList.AllocEntryPoint(EntryPoint:TSpirvOp);
var
 i:TpsslExportType;
 pVar:TsrVariable;
begin
 if (EntryPoint=nil) then Exit;
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
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

