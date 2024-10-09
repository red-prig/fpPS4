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

 PsrFragLayoutKey=^TsrFragLayoutKey;
 TsrFragLayoutKey=packed record
  itype   :TpsslInputType;
  location:Integer;
 end;

 TsrFragLayout=class(TsrDescriptor)
  public
   pLeft,pRight:TsrFragLayout;
   key:TsrFragLayoutKey;
   class function c(n1,n2:PsrFragLayoutKey):Integer; static;
  public
   pReg:TsrRegNode;
   //
   function  _GetStorageName:RawByteString; override;
   //
   property  itype:TpsslInputType read key.itype;
   Procedure Init; inline;
   function  GetStorageName:RawByteString;
 end;

 ntFragLayout=TsrFragLayout;

 PsrFragLayoutList=^TsrFragLayoutList;
 TsrFragLayoutList=object
  type
   TNodeFetch=specialize TNodeTreeClass<TsrFragLayout>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeFetch;
  procedure Init(Emit:TCustomEmit); inline;
  function  Fetch(itype:TpsslInputType;location:DWORD;rtype:TsrDataType):TsrFragLayout;
  Function  First:TsrFragLayout;
  Function  Next(node:TsrFragLayout):TsrFragLayout;
  procedure AllocBinding;
  procedure AllocEntryPoint(EntryPoint:TSpirvOp);
 end;

implementation

function TsrFragLayout._GetStorageName:RawByteString;
begin
 Result:=GetStorageName;
end;

//

class function TsrFragLayout.c(n1,n2:PsrFragLayoutKey):Integer;
begin
 //first itype
 Result:=ord(n1^.itype>n2^.itype)-ord(n1^.itype<n2^.itype);
 if (Result<>0) then Exit;
 //second location
 Result:=ord(n1^.location>n2^.location)-ord(n1^.location<n2^.location);
end;

Procedure TsrFragLayout.Init; inline;
begin
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
   itFlat:
    Result:='itFlat'+IntToStr(FBinding);
  else
   Result:='';
 end;
end;

procedure TsrFragLayoutList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrFragLayoutList.Fetch(itype:TpsslInputType;location:DWORD;rtype:TsrDataType):TsrFragLayout;
var
 key:TsrFragLayoutKey;
begin
 key:=Default(TsrFragLayoutKey);
 key.itype   :=itype;
 key.location:=location;
 //
 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrFragLayout>;
  Result.Init;
  Result.key:=key;
  Result.FBinding:=location;
  //
  Result.InitType(rtype);
  Result.InitVar();
  //
  FTree.Insert(Result);
 end;
end;

Function TsrFragLayoutList.First:TsrFragLayout;
begin
 Result:=FTree.Min;
end;

Function TsrFragLayoutList.Next(node:TsrFragLayout):TsrFragLayout;
begin
 Result:=FTree.Next(node);
end;

procedure TsrFragLayoutList.AllocBinding;
var
 pDecorateList:TsrDecorateList;
 pCapabilityList:PsrCapabilityList;
 node:TsrFragLayout;
 pVar:TsrVariable;
begin
 pDecorateList:=FEmit.GetDecorateList;
 pCapabilityList:=FEmit.GetCapabilityList;
 //interpolate param
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node.pVar;
  if (pVar<>nil) and node.IsUsed then
  begin
   pDecorateList.OpDecorate(pVar,Decoration.Location,node.FBinding);

   case node.itype of
    itPerspSample:    //Sample
      begin
       pCapabilityList^.Add(Capability.SampleRateShading);
       pDecorateList.OpDecorate(pVar,Decoration.Sample,0);
      end;
    itPerspCenter:;   //default
    itPerspCentroid:  //Centroid
      begin
       pDecorateList.OpDecorate(pVar,Decoration.Centroid,0);
      end;
    itLinearSample:   //NoPerspective Sample
      begin
       pDecorateList.OpDecorate(pVar,Decoration.NoPerspective,0);
       pDecorateList.OpDecorate(pVar,Decoration.Sample,0);
      end;
    itLinearCenter:   //NoPerspective
      begin
       pDecorateList.OpDecorate(pVar,Decoration.NoPerspective,0);
      end;
    itLinearCentroid: //NoPerspective Centroid
      begin
       pDecorateList.OpDecorate(pVar,Decoration.NoPerspective,0);
       pDecorateList.OpDecorate(pVar,Decoration.Centroid,0);
      end;
    itFlat:
     begin
      pDecorateList.OpDecorate(pVar,Decoration.Flat,0);
     end;

    else
     Assert(false,'AllocBinding:'+GetEnumName(TypeInfo(TpsslInputType),ord(node.itype)));
   end;

  end;
  node:=Next(node);
 end;
end;

procedure TsrFragLayoutList.AllocEntryPoint(EntryPoint:TSpirvOp);
var
 node:TsrFragLayout;
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

