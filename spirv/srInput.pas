unit srInput;

{$mode ObjFPC}{$H+}

interface

uses
 typinfo,
 sysutils,
 spirv,
 ginodes,
 srNode,
 srType,
 srTypes,
 srReg,
 srOp,
 srConst,
 srLayout,
 srVariable,
 srCapability,
 srDecorate;

type
 TpsslInputType=(
  itUnknow,

  itVsState,

  itWriteIndex,
  itOffset,
  itWaveId,
  itScratch,

  itVIndex,
  itVInstance,

  itPsState,
  itWaveCnt,

  itPerspSample,
  itPerspCenter,
  itPerspCentroid,
  itPerspW,
  itLinearSample,
  itLinearCenter,
  itLinearCentroid,
  itFlat,

  itFloatPos,
  itFrontFace,
  itSampleCoverage,
  itPosFixed,

  itTgid,
  itTgSize,

  itThreadId,

  //
  itSampleId,
  itLayer,

  itSubgroupLocalInvocationId,

  itPositions,
  itParameters
 );

 PsrInputKey=^TsrInputKey;
 TsrInputKey=packed record
  itype:TpsslInputType;
  typeid:Byte;
 end;

 TsrInput=class(TsrDescriptor)
  public
   pLeft,pRight:TsrInput;
   class function c(n1,n2:PsrInputKey):Integer; static;
  private
   key:TsrInputKey;
  public
   pReg:TsrRegNode;
   //
   function _GetStorageName:RawByteString; override;
   //
   property  itype:TpsslInputType read key.itype;
   property  typeid:Byte          read key.typeid;
   Procedure Init; inline;
   function  GetStorageName:RawByteString;
 end;

 ntInput=TsrInput;

 PsrInputList=^TsrInputList;
 TsrInputList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrInput>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Search(itype:TpsslInputType;id:Byte):TsrInput;
  function  Fetch(rtype:TsrDataType;itype:TpsslInputType;id:Byte):TsrInput;
  function  Fetch(pType:TsrType;itype:TpsslInputType;id:Byte):TsrInput;
  Function  First:TsrInput;
  Function  Next(node:TsrInput):TsrInput;
  procedure Test;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:TSpirvOp);
 end;

implementation

function TsrInput._GetStorageName:RawByteString;
begin
 Result:=GetStorageName;
end;

//

class function TsrInput.c(n1,n2:PsrInputKey):Integer;
begin
 //first itype
 Result:=ord(n1^.itype>n2^.itype)-ord(n1^.itype<n2^.itype);
 if (Result<>0) then Exit;
 //second typeid
 Result:=ord(n1^.typeid>n2^.typeid)-ord(n1^.typeid<n2^.typeid);
end;

Procedure TsrInput.Init; inline;
begin
 FStorage:=StorageClass.Input;
 FBinding:=-1;
end;

function TsrInput.GetStorageName:RawByteString;
begin
 Result:=GetEnumName(TypeInfo(TpsslInputType),ord(key.itype))+IntToStr(key.typeid);
end;

Procedure TsrInputList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrInputList.Search(itype:TpsslInputType;id:Byte):TsrInput;
var
 key:TsrInputKey;
begin
 key:=Default(TsrInputKey);
 key.itype :=itype;
 key.typeid:=id;
 Result:=FTree.Find(@key);
end;

function TsrInputList.Fetch(rtype:TsrDataType;itype:TpsslInputType;id:Byte):TsrInput;
var
 pTypeList:PsrTypeList;
begin
 pTypeList:=FEmit.GetTypeList;
 //
 Result:=Fetch(pTypeList^.Fetch(rtype),itype,id);
end;

function TsrInputList.Fetch(pType:TsrType;itype:TpsslInputType;id:Byte):TsrInput;
var
 key:TsrInputKey;
begin
 key:=Default(TsrInputKey);
 key.itype :=itype;
 key.typeid:=id;
 //
 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrInput>;
  Result.Init;
  Result.key:=key;
  //
  Result.pType:=pType;
  Result.InitVar;
  //
  FTree.Insert(Result);
 end;
end;

Function TsrInputList.First:TsrInput;
begin
 Result:=FTree.Min;
end;

Function TsrInputList.Next(node:TsrInput):TsrInput;
begin
 Result:=FTree.Next(node);
end;

procedure TsrInputList.Test;
var
 node:TsrInput;
 pVar:TsrVariable;

 procedure SetConst;
 var
  c:TsrConst;
  l:TSpirvOp;
 begin
  l:=node.pReg.pWriter.specialize AsType<ntOp>;
  l.mark_not_used;

  c:=PsrConstList(FEmit.GetConstList)^.Fetch(node.pReg.dtype,0);
  node.pReg.pWriter:=c;
 end;

begin
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) and node.IsUsed then
  begin

   Case node.key.itype of
    itPerspSample,
    itPerspCenter,
    itPerspCentroid,
    itLinearSample,
    itLinearCenter,
    itLinearCentroid:
      begin
       //TODO: barycentric
       //need support for barycentric coordinates, just a constant for now
       SetConst;
      end;

    else;
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrInputList.AllocBinding;
var
 pDecorateList:TsrDecorateList;
 pCapabilityList:PsrCapabilityList;
 node:TsrInput;
 pVar:TsrVariable;

 procedure TestFlat;
 begin
  //only pixel shader
  if (FEmit.GetExecutionModel=ExecutionModel.Fragment) then
  if (node.pReg<>nil) then
  if (node.pReg.dtype.isInt) then //only integer
  begin
   pDecorateList.OpDecorate(pVar,Decoration.Flat,0);
  end;
 end;

begin
 pDecorateList  :=FEmit.GetDecorateList;
 pCapabilityList:=FEmit.GetCapabilityList;

 node:=First;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) and node.IsUsed then
  begin

   Case node.key.itype of
    itFloatPos:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.FragCoord);
      end;
    itTgid:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.WorkgroupId);
      end;
    itThreadId:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.LocalInvocationID);
      end;
    itVIndex:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.VertexIndex);
      end;
    itVInstance:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.InstanceIndex);
      end;
    itFrontFace:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.FrontFacing);
      end;

    itLayer:
      begin
       TestFlat;
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.Layer);
       pCapabilityList^.Add(Capability.Geometry);
      end;
    itSampleId:
      begin
       TestFlat;
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.SampleId);
      end;

    itPerspSample,
    itPerspCenter,
    itPerspCentroid,
    itLinearSample,
    itLinearCenter,
    itLinearCentroid:
      begin
       //
      end;

    itPositions:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.Position);
      end;

    itParameters:
      begin
       pDecorateList.OpDecorate(pVar,Decoration.Location,node.key.typeid);
      end;

    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TpsslInputType),ord(node.key.itype)));
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrInputList.AllocEntryPoint(EntryPoint:TSpirvOp);
var
 node:TsrInput;
 pVar:TsrVariable;
begin
 if (EntryPoint=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) and node.IsUsed then
  begin
   EntryPoint.AddParam(pVar);
  end;
  node:=Next(node);
 end;
end;

end.

