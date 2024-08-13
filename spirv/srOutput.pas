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

 ntOutput=class(ntDescriptor)
  class Function  pwrite_count  (node:PsrNode):PDWORD;        override;
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 PsrOutput=^TsrOutput;
 TsrOutput=object(TsrDescriptor)
  type
   PVNode=^TVNode;
   TVNode=record
    pPrev,pNext:PVNode;
    pLine:PspirvOp;
   end;
   TNodeList=specialize TNodeList<PVNode>;
  var
   FEmit:TCustomEmit;
   fwrite_count:DWORD;
   etype:TpsslExportType;
   FLineList:TNodeList;
  function  GetStorageName:RawByteString;
  procedure AddLine(pLine:PspirvOp);
  Procedure FetchStore(pLine:PspirvOp;src:PsrRegNode);
  Procedure UpdateRegType;
 end;

 PsrOutputList=^TsrOutputList;
 TsrOutputList=object
  FEmit:TCustomEmit;
  FDepthMode:TDepthMode;
  data:array[TpsslExportType] of TsrOutput;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(etype:TpsslExportType;rtype:TsrDataType):PsrOutput;
  procedure Post;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
 end;

implementation

class Function ntOutput.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=@PsrOutput(node)^.fwrite_count;
end;

class function ntOutput.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrOutput(node)^.GetStorageName;
end;

//

function TsrOutput.GetStorageName:RawByteString;
begin
 Result:=GetEnumName(TypeInfo(TpsslExportType),ord(etype));
end;

procedure TsrOutput.AddLine(pLine:PspirvOp);
var
 node:PVNode;
begin
 node:=FEmit.Alloc(SizeOf(TVNode));
 node^.pLine:=pLine;
 FLineList.Push_tail(node);
end;

Procedure TsrOutput.FetchStore(pLine:PspirvOp;src:PsrRegNode);
begin
 if (src=nil) then Exit;

 pLine:=PspirvOp(FEmit.OpStore(pLine,FVar,src));

 AddLine(pLine);
end;

Procedure TsrOutput.UpdateRegType;
var
 pBitcastList:PsrBitcastList;
 node:PVNode;
 pLine:PspirvOp;
 dst:PsrRegNode;
 old,rtype:TsrDataType;
begin
 rtype:=FType^.dtype;

 pBitcastList:=FEmit.GetBitcastList;

 node:=FLineList.pHead;
 While (node<>nil) do
 begin
  pLine:=node^.pLine;

  Case pLine^.OpId of

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
       pBitcastList:=FEmit.GetBitcastList;
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

//

Procedure TsrOutputList.Init(Emit:TCustomEmit); inline;
var
 i:TpsslExportType;
begin
 FEmit:=Emit;
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
 begin
  data[i].fntype  :=ntOutput;
  data[i].etype   :=i;
  data[i].FStorage:=StorageClass.Output;
  data[i].FBinding:=-1;
 end;
end;

function TsrOutputList.Fetch(etype:TpsslExportType;rtype:TsrDataType):PsrOutput;
begin
 Result:=@data[etype];
 //
 Result^.FEmit:=FEmit;
 Result^.InitType(rtype,FEmit);
 Result^.InitVar(FEmit);
end;

//

procedure TsrOutputList.Post;
var
 i:TpsslExportType;
begin
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
 begin
  if (data[i].pVar<>nil) and data[i].IsUsed then
  begin
   data[i].UpdateRegType;
  end;
 end;
end;

procedure TsrOutputList.AllocBinding;
var
 pDecorateList:PsrDecorateList;
 i:TpsslExportType;
 pVar:PsrVariable;
 FLocation:Integer;
begin
 pDecorateList:=FEmit.GetDecorateList;
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
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
        pDecorateList^.OpDecorate(pVar,Decoration.Location,FLocation);
        data[i].FBinding:=FLocation;
       end;
       //Decoration.Index; ???
      end;
    etMrtz:
      begin
       //force Depth Replacing
       FDepthMode:=foDepthReplacing;
       //
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.FragDepth);
      end;
    etPos0:
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.Position);
      end;
    //etPos1..etPos3,
    etParam0..etParam31: //interpolate param
     begin
      FLocation:=ord(i)-ord(etParam0);
      pDecorateList^.OpDecorate(pVar,Decoration.Location,FLocation);
      data[i].FBinding:=FLocation;
     end;
    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TpsslExportType),ord(i)));
   end;
  end;
 end;

end;

procedure TsrOutputList.AllocEntryPoint(EntryPoint:PSpirvOp);
var
 i:TpsslExportType;
 pVar:PsrVariable;
begin
 if (EntryPoint=nil) then Exit;
 For i:=Low(TpsslExportType) to High(TpsslExportType) do
 begin
  pVar:=data[i].pVar;
  if (not data[i].IsUsed) and (pVar<>nil) then assert(false);
  if (pVar<>nil) and data[i].IsUsed then
  begin
   EntryPoint^.AddParam(pVar);
  end;
 end;
end;

end.

