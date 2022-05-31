unit emit_VINTRP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srTypes,
  srInput,
  srReg,
  spirv,
  SprvEmit,
  emit_op;

type
 TEmit_VINTRP=object(TEmitOp)
  procedure _emit_VINTRP;
 end;

implementation

procedure TEmit_VINTRP._emit_VINTRP;
var
 inp_M0:PsrInput;

 src:PsrRegSlot;
 dst:PsrRegSlot;

 inp_SRC:PsrInput;

 rsl:PsrRegNode;

begin
 Assert(FExecutionModel=ExecutionModel.Fragment); //only pixel shader

 dst:=FRegsStory.get_vdst8(FSPI.VINTRP.VDST);

 inp_M0:=GetInputRegNode(FRegsStory.M0.current);

 Assert(inp_M0<>nil);
 Assert(inp_M0^.key.itype=itPsState);

 Case FSPI.VINTRP.OP of
  V_INTERP_P1_F32:
    begin
     src:=FRegsStory.get_vsrc8(FSPI.VINTRP.VSRC);

     inp_SRC:=GetInputRegNode(src^.current);

     Assert(inp_SRC<>nil);

     Case inp_SRC^.key.itype of
      itPerspSample,
      itPerspCenter,   //barycentrics with perspective interpolation
      itPerspCentroid,
      itLinearSample,
      itLinearCenter,
      itLinearCentroid:;
      else
       Assert(false);
     end;

     Assert(inp_SRC^.key.typeid=0);            //I

     rsl:=AddFragLayout(inp_SRC^.key.itype,dtVec4f,FSPI.VINTRP.ATTR);
     rsl^.mark_read;

     dst^.New(line,dtFloat32);
     emit_OpCompExtract(line,dst^.current,rsl,FSPI.VINTRP.ATTRCHAN);

    end;
  V_INTERP_P2_F32:
    begin
     src:=FRegsStory.get_vsrc8(FSPI.VINTRP.VSRC);

     inp_SRC:=GetInputRegNode(src^.current);

     Assert(inp_SRC<>nil);

     Case inp_SRC^.key.itype of
      itPerspSample,
      itPerspCenter,   //barycentrics with perspective interpolation
      itPerspCentroid,
      itLinearSample,
      itLinearCenter,
      itLinearCentroid:;
      else
       Assert(false);
     end;

     Assert(inp_SRC^.key.typeid=1);            //J

     rsl:=AddFragLayout(inp_SRC^.key.itype,dtVec4f,FSPI.VINTRP.ATTR);
     rsl^.mark_read;

     dst^.New(line,dtFloat32);
     emit_OpCompExtract(line,dst^.current,rsl,FSPI.VINTRP.ATTRCHAN);

    end;

  else
   Assert(False,'VINTRP?'+IntToStr(FSPI.VINTRP.OP));
 end;

end;

end.

