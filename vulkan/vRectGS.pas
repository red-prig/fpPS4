unit vRectGS;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,

 Vulkan,

 vRegs2Vulkan,

 spirv,
 srNode,
 srType,
 srTypes,
 srReg,
 srVariable,
 srOutput,

 SprvEmit;

function CompileRectangleGeometryShader(var GPU_REGS:TGPU_REGS):TMemoryStream;

implementation

function interpolate(SprvEmit:TSprvEmit;
                     rtype   :TsrDataType;
                     barycentric,inputs:PPsrRegNode):PsrRegNode;
var
 tmp:array[0..2] of PsrRegNode;
 //
 i:Byte;
begin
 For i:=0 to 2 do
 begin
  tmp[i]:=SprvEmit.NewReg(rtype);
  //
  SprvEmit._Op2(SprvEmit.line,Op.OpVectorTimesScalar,tmp[i],inputs[i],barycentric[i]);
 end;
 //
 Result:=SprvEmit.OpFAddTo(tmp[0],tmp[1]);
 Result:=SprvEmit.OpFAddTo(Result,tmp[2]);
end;

type
 TINPUT_INFO=packed record
  LAYOUT_ID  :Byte;
  FLAT_SHADE :Boolean;
 end;

//source: [amdilc_rect_gs_compiler.c]
function CompileRectangleGeometryShader(var GPU_REGS:TGPU_REGS):TMemoryStream;
var
 INPUT_INFO :array[0..31] of TINPUT_INFO;
 INPUT_COUNT:Byte;
 //
 SprvEmit:TSprvEmit;
 //
 pVec4f      :PsrType;
 InputPos    :PsrVariable;
 OutputPos   :PsrVariable;
 InputParams :array[0..31] of PsrVariable;
 OutputParams:array[0..31] of PsrVariable;
 //
 pChain      :PsrNode;
 UintId      :array[0..3] of PsrRegNode;
 Positions   :array[0..3] of PsrRegNode;
 positionsX  :array[0..2] of PsrRegNode;
 positionsY  :array[0..2] of PsrRegNode;
 CoordEqualX :array[0..2] of PsrRegNode;
 CoordEqualY :array[0..2] of PsrRegNode;
 EdgeVertex  :array[0..2] of PsrRegNode;
 barycentric :array[0..2] of PsrRegNode;
 Parameters  :array[0..2] of PsrRegNode;
 xyEqual     :PsrRegNode;
 yxEqual     :PsrRegNode;
 pOneId      :PsrRegNode;
 pMinusOne   :PsrRegNode;
 pIndex      :PsrRegNode;
 pLoaded     :PsrRegNode;
 //
 i,a:Byte;
begin
 Result:=nil;

 INPUT_COUNT:=GPU_REGS.CX_REG^.SPI_PS_IN_CONTROL.NUM_INTERP;
 if (INPUT_COUNT<>0) then
 begin
  a:=0;
  for i:=0 to INPUT_COUNT-1 do
  //if not default
  if ((GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].OFFSET shr 5)=0) then
  begin
   INPUT_INFO[a].LAYOUT_ID :=(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].OFFSET and 31);
   INPUT_INFO[a].FLAT_SHADE:=(GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL[i].FLAT_SHADE)<>0;
   //
   Inc(a);
  end;
  //update count
  INPUT_COUNT:=a;
 end;

 SprvEmit:=TSprvEmit.Create;

 SprvEmit.InitCustomGs();

 pVec4f   :=SprvEmit.TypeList.Fetch(dtVec4f);

 //postion i/o
 InputPos :=SprvEmit.AddPositionsInput(3);
 OutputPos:=SprvEmit.FetchOutput(etPos0,dtVec4f);

 //input param i/o
 if (INPUT_COUNT<>0) then
 begin
  for a:=0 to INPUT_COUNT-1 do
  begin
   InputParams [a]:=SprvEmit.AddParametersInput(INPUT_INFO[a].LAYOUT_ID,3);
   OutputParams[a]:=SprvEmit.FetchOutput(TpsslExportType(ord(etParam0)+INPUT_INFO[a].LAYOUT_ID),dtVec4f);
  end;
 end;

 //gen const id
 For i:=0 to 3 do
 begin
  UintId[i]:=SprvEmit.NewReg_i(dtUint32,i);
 end;

 //load positions
 For i:=0 to 2 do
 begin
  pChain:=SprvEmit.OpAccessChainTo(pVec4f,InputPos,UintId[i]);
  //
  Positions[i]:=SprvEmit.OpLoadTo(pVec4f,pChain);
 end;

 //extract
 For i:=0 to 2 do
 begin
  positionsX[i]:=SprvEmit.NewReg(dtFloat32);
  positionsY[i]:=SprvEmit.NewReg(dtFloat32);
  //
  SprvEmit.OpExtract(SprvEmit.line,positionsX[i],Positions[i],0);
  SprvEmit.OpExtract(SprvEmit.line,positionsY[i],Positions[i],1);
 end;

 //compare
 For i:=0 to 2 do
 begin
  CoordEqualX[i]:=SprvEmit.NewReg(dtBool);
  CoordEqualY[i]:=SprvEmit.NewReg(dtBool);
  //
  SprvEmit._Op2(SprvEmit.line,Op.OpFOrdEqual,CoordEqualX[i],positionsX[i],positionsX[(i + 1) mod 3]);
  SprvEmit._Op2(SprvEmit.line,Op.OpFOrdEqual,CoordEqualY[i],positionsY[i],positionsY[(i + 1) mod 3]);
 end;

 pOneId   :=SprvEmit.NewReg_s(dtFloat32, 1);
 pMinusOne:=SprvEmit.NewReg_s(dtFloat32,-1);

 //calc barycentric
 For i:=0 to 2 do
 begin
  xyEqual:=SprvEmit.OpAndTo(CoordEqualX[i], CoordEqualY[(i + 2) mod 3]);
  yxEqual:=SprvEmit.OpAndTo(CoordEqualY[i], CoordEqualX[(i + 2) mod 3]);
  //
  EdgeVertex[i]:=SprvEmit.OpOrTo(xyEqual,yxEqual);
  //
  barycentric[i]:=SprvEmit.OpSelectTo(pOneId,pMinusOne,EdgeVertex[i]);
 end;

 //calc last pos
 Positions[3]:=interpolate(SprvEmit,dtVec4f,@barycentric,@Positions);

 //select first index
 pIndex:=SprvEmit.OpSelectTo(UintId[0], UintId[1], EdgeVertex[1]);
 pIndex:=SprvEmit.OpSelectTo(pIndex   , UintId[2], EdgeVertex[2]);

 //Send vertex by index
 For i:=0 to 2 do
 begin
  pChain :=SprvEmit.OpAccessChainTo(pVec4f,InputPos,pIndex);
  pLoaded:=SprvEmit.OpLoadTo(pVec4f,pChain);
  SprvEmit.OpStore(SprvEmit.line,OutputPos,pLoaded);

  //emit input attr
  if (INPUT_COUNT<>0) then
  begin
   for a:=0 to INPUT_COUNT-1 do
   begin
    if INPUT_INFO[a].FLAT_SHADE then
    begin
     //get first
     pChain:=SprvEmit.OpAccessChainTo(pVec4f,InputParams[a],UintId[0]);
    end else
    begin
     pChain:=SprvEmit.OpAccessChainTo(pVec4f,InputParams[a],pIndex);
    end;

    pLoaded:=SprvEmit.OpLoadTo(pVec4f,pChain);
    SprvEmit.OpStore(SprvEmit.line,OutputParams[a],pLoaded);
   end;
  end;

  //end vertex
  SprvEmit.OpEmitVertex(SprvEmit.line);

  if (i<>2) then
  begin
   //next index
   pIndex:=SprvEmit.OpIAddTo(pIndex,UintId[1]);
   pIndex:=SprvEmit.OpIModTo(pIndex,UintId[3]);
  end;
 end; //for

 //send last vertex
 SprvEmit.OpStore(SprvEmit.line,OutputPos,Positions[3]);

 //emit input attr
 if (INPUT_COUNT<>0) then
 begin
  for a:=0 to INPUT_COUNT-1 do
  begin
   if INPUT_INFO[a].FLAT_SHADE then
   begin
    //get first
    pChain :=SprvEmit.OpAccessChainTo(pVec4f,InputParams[a],UintId[0]);
    pLoaded:=SprvEmit.OpLoadTo(pVec4f,pChain);
   end else
   begin
    //load parameters
    For i:=0 to 2 do
    begin
     pChain:=SprvEmit.OpAccessChainTo(pVec4f,InputParams[a],UintId[i]);
     //
     Parameters[i]:=SprvEmit.OpLoadTo(pVec4f,pChain);
    end;

    //calc last parameter
    pLoaded:=interpolate(SprvEmit,dtVec4f,@barycentric,@Parameters);
   end;

   SprvEmit.OpStore(SprvEmit.line,OutputParams[a],pLoaded);
  end;
 end;

 //end vertex
 SprvEmit.OpEmitVertex(SprvEmit.line);

 //end prim
 SprvEmit.OpEndPrimitive(SprvEmit.line);

 //end function
 SprvEmit.OpReturn(SprvEmit.line);
 SprvEmit.OpFunctionEnd(SprvEmit.line);

 //ending stage
 SprvEmit.PostStage;
 SprvEmit.AllocStage;

 //SprvEmit.Print;

 Result:=TMemoryStream.Create;
 SprvEmit.SaveToStream(Result);

 SprvEmit.Free;
end;

end.

