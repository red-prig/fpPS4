unit srFragLayout;

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
 srLayout,
 srVariable,
 srInput,
 srCapability,
 srDecorate;

type
 //itPerspSample,    //Sample
 //itPerspCenter,
 //itPerspCentroid,  //Centroid
 //itLinearSample,   //NoPerspective Sample
 //itLinearCenter,   //NoPerspective
 //itLinearCentroid, //NoPerspective Centroid

 ntFragLayout=class(ntDescriptor)
  class function  GetStorageName(node:PsrNode):RawByteString; override;
 end;

 PsrFragLayout=^TsrFragLayout;
 TsrFragLayout=object(TsrDescriptor)
  private
   pLeft,pRight:PsrFragLayout;
   //----
   itype:TpsslInputType;
   function  c(n1,n2:PsrFragLayout):Integer; static;
  public
   pReg:PsrRegNode;
   Procedure Init; inline;
   function  GetStorageName:RawByteString;
 end;

 PsrFragLayoutList=^TsrFragLayoutList;
 TsrFragLayoutList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrFragLayout,TsrFragLayout>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(itype:TpsslInputType;location:DWORD;rtype:TsrDataType):PsrFragLayout;
  Function  First:PsrFragLayout;
  Function  Next(node:PsrFragLayout):PsrFragLayout;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
 end;

implementation

class function ntFragLayout.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:=PsrFragLayout(node)^.GetStorageName;
end;

//

function TsrFragLayout.c(n1,n2:PsrFragLayout):Integer;
begin
 //first itype
 Result:=Integer(n1^.itype>n2^.itype)-Integer(n1^.itype<n2^.itype);
 if (Result<>0) then Exit;
 //second location
 Result:=Integer(n1^.FBinding>n2^.FBinding)-Integer(n1^.FBinding<n2^.FBinding);
end;

Procedure TsrFragLayout.Init; inline;
begin
 fntype  :=ntFragLayout;
 FStorage:=StorageClass.Input;
end;

function TsrFragLayout.GetStorageName:RawByteString;
begin
 Case itype of
   itPerspSample:
    Result:='itPSample'+IntToStr(FBinding);
   itPerspCenter:
    Result:='itParam'+IntToStr(FBinding);
   itPerspCentroid:
    Result:='itPCentroid'+IntToStr(FBinding);
   itLinearSample:
    Result:='itLSample'+IntToStr(FBinding);
   itLinearCenter:
    Result:='itLCenter'+IntToStr(FBinding);
   itLinearCentroid:
    Result:='itLCentroid'+IntToStr(FBinding);
  else
   Result:='';
 end;
end;

procedure TsrFragLayoutList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrFragLayoutList.Fetch(itype:TpsslInputType;location:DWORD;rtype:TsrDataType):PsrFragLayout;
var
 node:TsrFragLayout;
begin
 node:=Default(TsrFragLayout);
 node.Init;
 node.itype:=itype;
 node.FBinding:=location;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrFragLayout));
  Move(node,Result^,SizeOf(TsrFragLayout));
  //
  Result^.InitType(rtype,FEmit);
  Result^.InitVar(FEmit);
  //
  FNTree.Insert(Result);
 end;
end;

Function TsrFragLayoutList.First:PsrFragLayout;
begin
 Result:=FNTree.Min;
end;

Function TsrFragLayoutList.Next(node:PsrFragLayout):PsrFragLayout;
begin
 Result:=FNTree.Next(node);
end;

procedure TsrFragLayoutList.AllocBinding;
var
 pDecorateList:PsrDecorateList;
 pCapabilityList:PsrCapabilityList;
 node:PsrFragLayout;
 pVar:PsrVariable;
begin
 pDecorateList:=FEmit.GetDecorateList;
 pCapabilityList:=FEmit.GetCapabilityList;
 //interpolate param
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) and node^.IsUsed then
  begin
   pDecorateList^.OpDecorate(pVar,Decoration.Location,node^.FBinding);

   case node^.itype of
    itPerspSample:    //Sample
      begin
       pCapabilityList^.Add(Capability.SampleRateShading);
       pDecorateList^.OpDecorate(pVar,Decoration.Sample,0);
      end;
    itPerspCenter:;   //default
    itPerspCentroid:  //Centroid
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.Centroid,0);
      end;
    itLinearSample:   //NoPerspective Sample
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.NoPerspective,0);
       pDecorateList^.OpDecorate(pVar,Decoration.Sample,0);
      end;
    itLinearCenter:   //NoPerspective
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.NoPerspective,0);
      end;
    itLinearCentroid: //NoPerspective Centroid
      begin
       pDecorateList^.OpDecorate(pVar,Decoration.NoPerspective,0);
       pDecorateList^.OpDecorate(pVar,Decoration.Centroid,0);
      end;
    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TpsslInputType),ord(node^.itype)));
   end;

  end;
  node:=Next(node);
 end;
end;

procedure TsrFragLayoutList.AllocEntryPoint(EntryPoint:PSpirvOp);
var
 node:PsrFragLayout;
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

