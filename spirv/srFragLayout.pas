unit srFragLayout;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  spirv,
  srNodes,
  srReg,
  srOp,
  srLayout,
  srVariable,
  srInput,
  srCap,
  srDecorate;

type
 //itPerspSample,    //Sample
 //itPerspCenter,
 //itPerspCentroid,  //Centroid
 //itLinearSample,   //NoPerspective Sample
 //itLinearCenter,   //NoPerspective
 //itLinearCentroid, //NoPerspective Centroid

 PsrFragLayout=^TsrFragLayout;
 TsrFragLayout=object(TsrDescriptor)
  pLeft,pRight:PsrFragLayout;
  //----
  itype:TpsslInputType;

  pReg:PsrRegNode;

  function c(n1,n2:PsrFragLayout):Integer; static;
  function GetName:RawByteString;
 end;

 TsrFragLayoutList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrFragLayout,TsrFragLayout>;
  var
   Alloc:TfnAlloc;
   FNTree:TNodeFetch;
  procedure Init(cb:TfnAlloc);
  function  Fetch(itype:TpsslInputType;location:DWORD):PsrFragLayout;
  Function  First:PsrFragLayout;
  Function  Next(node:PsrFragLayout):PsrFragLayout;
  procedure AllocBinding(Decorates:PsrDecorateList;Cap:PsrCapList);
  procedure AllocEntryPoint(EntryPoint:PSpirvOp);
 end;

implementation

function TsrFragLayout.c(n1,n2:PsrFragLayout):Integer;
begin
 //first itype
 Result:=Integer(n1^.itype>n2^.itype)-Integer(n1^.itype<n2^.itype);
 //second location
 Result:=Integer(n1^.FBinding>n2^.FBinding)-Integer(n1^.FBinding<n2^.FBinding);
 //Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrFragLayout.key));
end;

function TsrFragLayout.GetName:RawByteString;
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

procedure TsrFragLayoutList.Init(cb:TfnAlloc);
begin
 Alloc:=cb;
end;

function TsrFragLayoutList.Fetch(itype:TpsslInputType;location:DWORD):PsrFragLayout;
var
 node:TsrFragLayout;
begin
 node:=Default(TsrFragLayout);
 node.itype:=itype;
 node.FStorage:=StorageClass.Input;
 node.FBinding:=location;
 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=Alloc(SizeOf(TsrFragLayout));
  Move(node,Result^,SizeOf(TsrFragLayout));
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

procedure TsrFragLayoutList.AllocBinding(Decorates:PsrDecorateList;Cap:PsrCapList);
var
 node:PsrFragLayout;
 pVar:PsrVariable;
begin
 if (Decorates=nil) then Exit;
 //interpolate param
 node:=First;
 While (node<>nil) do
 begin
  pVar:=node^.pVar;
  if (pVar<>nil) then
  begin
   Decorates^.emit_decorate(ntVar,pVar,Decoration.Location,node^.FBinding);

   case node^.itype of
    itPerspSample:    //Sample
      begin
       Cap^.Add(Capability.SampleRateShading);
       Decorates^.emit_decorate(ntVar,pVar,Decoration.Sample,0);
      end;
    itPerspCenter:;   //default
    itPerspCentroid:  //Centroid
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.Centroid,0);
      end;
    itLinearSample:   //NoPerspective Sample
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.NoPerspective,0);
       Decorates^.emit_decorate(ntVar,pVar,Decoration.Sample,0);
      end;
    itLinearCenter:   //NoPerspective
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.NoPerspective,0);
      end;
    itLinearCentroid: //NoPerspective Centroid
      begin
       Decorates^.emit_decorate(ntVar,pVar,Decoration.NoPerspective,0);
       Decorates^.emit_decorate(ntVar,pVar,Decoration.Centroid,0);
      end;
    else
     Assert(false,'AllocBinding');
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
  if (pVar<>nil) then
  begin
   EntryPoint^.AddParam(ntVar,pVar);
  end;
  node:=Next(node);
 end;
end;

end.

