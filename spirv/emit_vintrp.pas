unit emit_VINTRP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  ps4_pssl,
  srType,
  srInput,
  srReg,
  emit_fetch;

type
 TEmit_VINTRP=class(TEmitFetch)
  procedure emit_VINTRP;
 end;

implementation

procedure TEmit_VINTRP.emit_VINTRP;
var
 inp_M0:PsrInput;

 src:PsrRegSlot;
 dst:PsrRegSlot;

 inp_SRC:PsrInput;

 rsl:PsrRegNode;

begin
 Assert(FExecutionModel=ExecutionModel.Fragment); //only pixel shader

 dst:=get_vdst8(FSPI.VINTRP.VDST);

 inp_M0:=GetInputRegNode(get_m0^.current);

 Assert(inp_M0<>nil);
 Assert(inp_M0^.itype=itPsState);

 Case FSPI.VINTRP.OP of
  V_INTERP_P1_F32:
    begin
     src:=get_vsrc8(FSPI.VINTRP.VSRC);

     inp_SRC:=GetInputRegNode(src^.current);

     Assert(inp_SRC<>nil);

     Case inp_SRC^.itype of
      itPerspSample,
      itPerspCenter,   //barycentrics with perspective interpolation
      itPerspCentroid,
      itLinearSample,
      itLinearCenter,
      itLinearCentroid:;
      else
       Assert(false);
     end;

     Assert(inp_SRC^.typeid=0);            //I

     rsl:=AddFragLayout(inp_SRC^.itype,dtVec4f,FSPI.VINTRP.ATTR);

     dst^.New(line,dtFloat32);
     OpExtract(line,dst^.current,rsl,FSPI.VINTRP.ATTRCHAN);
    end;

  V_INTERP_P2_F32:
    begin
     src:=get_vsrc8(FSPI.VINTRP.VSRC);

     inp_SRC:=GetInputRegNode(src^.current);

     Assert(inp_SRC<>nil);

     Case inp_SRC^.itype of
      itPerspSample,
      itPerspCenter,   //barycentrics with perspective interpolation
      itPerspCentroid,
      itLinearSample,
      itLinearCenter,
      itLinearCentroid:;
      else
       Assert(false);
     end;

     Assert(inp_SRC^.typeid=1);            //J

     rsl:=AddFragLayout(inp_SRC^.itype,dtVec4f,FSPI.VINTRP.ATTR);

     dst^.New(line,dtFloat32);
     OpExtract(line,dst^.current,rsl,FSPI.VINTRP.ATTRCHAN);
    end;

   V_INTERP_MOV_F32:
    begin
     //src ignore

     //just use nointerpolation
     inp_SRC:=InputList.Fetch(dtVec4f,itFlat,0);

     rsl:=AddFragLayout(inp_SRC^.itype,dtVec4f,FSPI.VINTRP.ATTR);

     dst^.New(line,dtFloat32);
     OpExtract(line,dst^.current,rsl,FSPI.VINTRP.ATTRCHAN);
    end;


  else
   Assert(False,'VINTRP?'+IntToStr(FSPI.VINTRP.OP));
 end;

end;

end.

