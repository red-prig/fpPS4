unit srInput;

{$mode ObjFPC}{$H+}

interface

uses
  typinfo,
  sysutils,
  spirv,
  srNodes,
  srReg,
  srOp,
  srLayout,
  srVariable,
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

  itFloatPos,
  itFrontFace,
  itAncillary,
  itSampleCoverage,
  itPosFixed,

  itTgid,
  itTgSize,

  itThreadId

 );

 PsrInput=^TsrInput;

 TsrInput=object(TsrDescriptor)
  pLeft,pRight:PsrInput;
  //----

  key:packed record
   itype:TpsslInputType;
   typeid:Byte;
  end;

  pReg:PsrRegNode;

  function c(n1,n2:PsrInput):Integer; static;
  function GetName:RawByteString;
 end;

 TsrInputList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrInput,TsrInput>;
  var
   Alloc:TfnAlloc;
   FNTree:TNodeFetch;
  procedure Init(cb:TfnAlloc);
  function  Search(itype:TpsslInputType;id:Byte=0):PsrInput;
  function  Fetch(itype:TpsslInputType;id:Byte=0):PsrInput;
  Function  First:PsrInput;
  Function  Next(node:PsrInput):PsrInput;
  procedure AllocBinding(Decorates:PsrDecorateList);
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
 end;

implementation

function TsrInput.c(n1,n2:PsrInput):Integer;
begin
 Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrInput.key));
end;

function TsrInput.GetName:RawByteString;
begin
 Result:=GetEnumName(TypeInfo(TpsslInputType),ord(key.itype))+IntToStr(key.typeid);
end;

procedure TsrInputList.Init(cb:TfnAlloc);
begin
 Alloc:=cb;
end;

function TsrInputList.Search(itype:TpsslInputType;id:Byte=0):PsrInput;
var
 node:TsrInput;
begin
 node:=Default(TsrInput);
 node.key.itype:=itype;
 node.key.typeid:=id;
 Result:=FNTree.Find(@node);
end;

function TsrInputList.Fetch(itype:TpsslInputType;id:Byte=0):PsrInput;
var
 node:TsrInput;
begin
 node:=Default(TsrInput);
 node.key.itype:=itype;
 node.key.typeid:=id;
 node.FStorage:=StorageClass.Input;
 node.FBinding:=-1;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(TsrInput));
  Move(node,Result^,SizeOf(TsrInput));
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

procedure TsrInputList.AllocBinding(Decorates:PsrDecorateList);
var
 node:PsrInput;
 pVar:PsrVariable;

begin
 if (Decorates=nil) then Exit;
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   Case node^.key.itype of
    itFloatPos:
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.BuiltIn,BuiltIn.FragCoord);
      end;
    itTgid:
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.BuiltIn,BuiltIn.WorkgroupId);
      end;
    itThreadId:
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.BuiltIn,BuiltIn.LocalInvocationID);
      end;
    itVIndex:
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.BuiltIn,BuiltIn.VertexIndex);
      end;
    else
     Assert(false,'AllocBinding');
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
  if (pVar<>nil) then
  begin
   EntryPoint^.AddParam(ntVar,pVar);
  end;
  node:=Next(node);
 end;
end;

end.

