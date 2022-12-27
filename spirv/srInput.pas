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

  itSubgroupLocalInvocationId
 );

 ntInput=class(ntDescriptor)
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 PsrInput=^TsrInput;

 TsrInput=object(TsrDescriptor)
  private
   pLeft,pRight:PsrInput;
   //----
   key:packed record
    itype:TpsslInputType;
    typeid:Byte;
   end;
   function  c(n1,n2:PsrInput):Integer; static;
  public
   pReg:PsrRegNode;
   property  itype:TpsslInputType read key.itype;
   property  typeid:Byte          read key.typeid;
   Procedure Init; inline;
   function  GetStorageName:RawByteString;
 end;

 PsrInputList=^TsrInputList;
 TsrInputList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrInput,TsrInput>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  Procedure Init(Emit:TCustomEmit); inline;
  function  Search(itype:TpsslInputType;id:Byte):PsrInput;
  function  Fetch(rtype:TsrDataType;itype:TpsslInputType;id:Byte):PsrInput;
  Function  First:PsrInput;
  Function  Next(node:PsrInput):PsrInput;
  procedure Test;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
 end;

implementation

class function ntInput.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrInput(node)^.GetStorageName;
end;

//

function TsrInput.c(n1,n2:PsrInput):Integer;
begin
 //first itype
 Result:=Integer(n1^.key.itype>n2^.key.itype)-Integer(n1^.key.itype<n2^.key.itype);
 if (Result<>0) then Exit;
 //second typeid
 Result:=Integer(n1^.key.typeid>n2^.key.typeid)-Integer(n1^.key.typeid<n2^.key.typeid);
end;

Procedure TsrInput.Init; inline;
begin
 fntype  :=ntInput;
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

function TsrInputList.Search(itype:TpsslInputType;id:Byte):PsrInput;
var
 node:TsrInput;
begin
 node:=Default(TsrInput);
 node.Init;
 node.key.itype:=itype;
 node.key.typeid:=id;
 Result:=FNTree.Find(@node);
end;

function TsrInputList.Fetch(rtype:TsrDataType;itype:TpsslInputType;id:Byte):PsrInput;
var
 node:TsrInput;
begin
 node:=Default(TsrInput);
 node.Init;
 node.key.itype:=itype;
 node.key.typeid:=id;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrInput));
  Move(node,Result^,SizeOf(TsrInput));
  //
  Result^.InitType(rtype,FEmit);
  Result^.InitVar(FEmit);
  //
  FNTree.Insert(Result);
 end;
end;

Function TsrInputList.First:PsrInput;
begin
 Result:=FNTree.Min;
end;

Function TsrInputList.Next(node:PsrInput):PsrInput;
begin
 Result:=FNTree.Next(node);
end;

procedure TsrInputList.Test;
var
 node:PsrInput;
 pVar:PsrVariable;

 procedure SetConst;
 var
  c:PsrConst;
  l:PspirvOp;
 begin
  l:=node^.pReg^.pWriter^.AsType(ntOp);
  l^.mark_not_used;

  c:=PsrConstList(FEmit.GetConstList)^.Fetch(node^.pReg^.dtype,0);
  node^.pReg^.pWriter:=c;
 end;

begin
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin

   Case node^.key.itype of
    itPerspSample,
    itPerspCenter,
    itPerspCentroid,
    itLinearSample,
    itLinearCenter,
    itLinearCentroid:
      begin
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
 pDecorateList:PsrDecorateList;
 pCapabilityList:PsrCapabilityList;
 node:PsrInput;
 pVar:PsrVariable;

 procedure TestFlat;
 begin
  //only pixel shader
  if (FEmit.GetExecutionModel=ExecutionModel.Fragment) then
  if (node^.pReg<>nil) then
  if (node^.pReg^.dtype.isInt) then //only integer
  begin
   pDecorateList^.OpDecorate(pVar,Decoration.Flat,0);
  end;
 end;

begin
 pDecorateList  :=FEmit.GetDecorateList;
 pCapabilityList:=FEmit.GetCapabilityList;

 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin

   Case node^.key.itype of
    itFloatPos:
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.FragCoord);
      end;
    itTgid:
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.WorkgroupId);
      end;
    itThreadId:
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.LocalInvocationID);
      end;
    itVIndex:
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.VertexIndex);
      end;
    itFrontFace:
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.FrontFacing);
      end;

    itLayer:
      begin
       TestFlat;
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.Layer);
       pCapabilityList^.Add(Capability.Geometry);
      end;
    itSampleId:
      begin
       TestFlat;
       pDecorateList^.OpDecorate(pVar,Decoration.BuiltIn,BuiltIn.SampleId);
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

    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TpsslInputType),ord(node^.key.itype)));
   end;
  end;
  node:=Next(node);
 end;
end;

procedure TsrInputList.AllocEntryPoint(EntryPoint:PSpirvOp);
var
 node:PsrInput;
 pVar:PsrVariable;
begin
 if (EntryPoint=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin
   EntryPoint^.AddParam(pVar);
  end;
  node:=Next(node);
 end;
end;

end.

