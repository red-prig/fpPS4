unit emit_VINTRP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  ps4_pssl,
  srType,
  srInterface,
  srInput,
  srReg,
  emit_fetch;

type
 TEmit_VINTRP=class(TEmitFetch)
  procedure emit_VINTRP;
 end;

implementation

const
 DEFAULT_VAL:array[0..3] of Tvec4f=(
  (0.0, 0.0, 0.0, 0.0),
  (0.0, 0.0, 0.0, 1.0),
  (1.0, 1.0, 1.0, 0.0),
  (1.0, 1.0, 1.0, 1.0)
 );

procedure TEmit_VINTRP.emit_VINTRP;
var
 inp_M0:TsrInput;

 src:PsrRegSlot;
 dst:PsrRegSlot;

 inp_SRC:TsrInput;
 itype:TpsslInputType;

 procedure ExportValue(itype:TpsslInputType);
 var
  InputCntl:PPSInputCntl;

  rsl,elm:TsrRegNode;
 begin
  //
  InputCntl:=@FPSInputCntl[FSPI.VINTRP.ATTR];

  if InputCntl^.USE_DEFAULT then
  begin
   elm:=NewReg_s(dtFloat32,DEFAULT_VAL[InputCntl^.DEFAULT_VAL][FSPI.VINTRP.ATTRCHAN]);

   MakeCopy(dst,elm);
  end else
  begin
   //force flat
   if InputCntl^.FLAT_SHADE then
   begin
    itype:=itFlat;
   end;

   //remap id
   rsl:=AddFragLayout(itype,dtVec4f,InputCntl^.OFFSET);

   elm:=dst^.New(line,dtFloat32);

   OpExtract(line,elm,rsl,FSPI.VINTRP.ATTRCHAN);
  end;
  //
 end;

begin
 Assert(FExecutionModel=ExecutionModel.Fragment); //only pixel shader

 dst:=get_vdst8(FSPI.VINTRP.VDST);

 inp_M0:=GetInputRegNode(get_m0^.current);

 //Assert(inp_M0<>nil);
 //Assert(inp_M0^.itype=itPsState);

 Case FSPI.VINTRP.OP of
  V_INTERP_P1_F32:
    begin
     src:=get_vsrc8(FSPI.VINTRP.VSRC);

     inp_SRC:=GetInputRegNode(src^.current);

     Assert(inp_SRC<>nil);

     itype:=inp_SRC.itype;

     Case itype of
      itPerspSample,
      itPerspCenter,   //barycentrics with perspective interpolation
      itPerspCentroid,
      itLinearSample,
      itLinearCenter,
      itLinearCentroid:;
      else
       Assert(false);
     end;

     Assert(inp_SRC.typeid=0);            //I

     ExportValue(itype);
    end;

  V_INTERP_P2_F32:
    begin
     src:=get_vsrc8(FSPI.VINTRP.VSRC);

     inp_SRC:=GetInputRegNode(src^.current);

     Assert(inp_SRC<>nil);

     itype:=inp_SRC.itype;

     Case itype of
      itPerspSample,
      itPerspCenter,   //barycentrics with perspective interpolation
      itPerspCentroid,
      itLinearSample,
      itLinearCenter,
      itLinearCentroid:;
      else
       Assert(false);
     end;

     Assert(inp_SRC.typeid=1);            //J

     ExportValue(itype);
    end;

   V_INTERP_MOV_F32:
    begin
     //src ignore

     //just use nointerpolation
     inp_SRC:=InputList.Fetch(dtFloat32,itFlat,0);

     Assert(inp_SRC<>nil);

     itype:=inp_SRC.itype;

     ExportValue(itype);
    end;


  else
   Assert(False,'VINTRP?'+IntToStr(FSPI.VINTRP.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

