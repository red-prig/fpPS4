unit emit_EXP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srNodes,
  srLabel,
  srTypes,
  srReg,
  srVariable,
  srOutput,
  srOp,
  srOpUtils,
  SprvEmit,
  emit_op;

type
 TEmit_EXP=object(TEmitOp)
  procedure _emit_EXP;
 end;

implementation

procedure TEmit_EXP._emit_EXP;
Var
 exc:PsrRegNode;
 node:PSpirvOp;
 pOpBlock:PsrOpBlock;

 dout:PsrVariable;
 dst:PsrRegNode;
 src:array[0..3] of PsrRegNode;
 rtype:TsrDataType;
 f,i,p:Byte;

begin
 //if (VM<>0) and (EXEC<>0) = set pixel else (if DONE=1) discard pixel /(PS only)

 pOpBlock:=nil;
 if (FSPI.EXP.VM<>0) and (FSPI.EXP.DONE<>0) then
 begin
  pOpBlock:=AllocBlockOp;
  pOpBlock^.SetInfo(btOther,FCursor.Adr,FCursor.Adr);
  PushBlockOp(line,pOpBlock,nil);

  exc:=MakeRead(@FRegsStory.EXEC[0],dtBool);
  node:=AddSpirvOp(OpMakeExp);
  node^.AddParam(ntReg,exc); //<-fetch read
 end;

 //before
 if (TpsslExportType(FSPI.EXP.TGT)=etNull) //only set kill mask
   or (FSPI.EXP.EN=0) then //nop
 begin
  if (pOpBlock<>nil) then //is pushed
  begin
   FMain^.PopBlock;
  end;
  Exit;
 end;

 pOpBlock:=AllocBlockOp; //down
 pOpBlock^.SetInfo(btOther,FCursor.Adr,FCursor.Adr);
 PushBlockOp(line,pOpBlock,nil);

 //output

 src[0]:=nil;
 src[1]:=nil;
 src[2]:=nil;
 src[3]:=nil;

 f:=FSPI.EXP.EN;

 if (FSPI.EXP.COMPR=0) then //float32
 begin

  p:=PopCnt(f);

  if (p=1) then
  begin
   Case f of
    $1:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC0,dtFloat32);
    $2:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC1,dtFloat32);
    $4:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC2,dtFloat32);
    $8:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC3,dtFloat32);
    else
     Assert(false);
   end;
   dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),dtFloat32); //output in FSPI.EXP.TGT
   emit_OpStore(line,dout,src[0]);
  end else
  begin

   Case p of
    0:Assert(false);
    2:rtype:=dtVec2f;
    3:rtype:=dtVec3f;
    4:rtype:=dtVec4f;
   end;

   i:=0;
   if (f and $1<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC0,dtFloat32);
    Inc(i);
   end;
   if (f and $2<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC1,dtFloat32);
    Inc(i);
   end;
   if (f and $4<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC2,dtFloat32);
    Inc(i);
   end;
   if (f and $8<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC3,dtFloat32);
    Inc(i);
   end;

   dst:=emit_OpMakeVec(line,rtype,p,@src);
   dst^.mark_read;

   dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
   emit_OpStore(line,dout,dst);
  end;

 end else
 begin //half16
  Case f of
    3,
    $F:
      begin
       fetch_vsrc8_vec2h(FSPI.EXP.VSRC0,src[0],src[1]);
       fetch_vsrc8_vec2h(FSPI.EXP.VSRC1,src[2],src[3]);
      end;
   $C:
      begin
       fetch_vsrc8_vec2h(FSPI.EXP.VSRC2,src[0],src[1]);
       fetch_vsrc8_vec2h(FSPI.EXP.VSRC3,src[2],src[3]);
      end;
   else
    Assert(false);
  end;

  dst:=emit_OpMakeVec(line,dtVec4h,4,@src);
  dst^.mark_read;

  dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),dtVec4h); //output in FSPI.EXP.TGT
  emit_OpStore(line,dout,dst);
 end;

 if (pOpBlock<>nil) then //is pushed
 begin
  FMain^.PopBlock;
  FMain^.PopBlock;
 end;
end;

end.

