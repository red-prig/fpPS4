unit emit_EXP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srCFGLabel,
  srConfig,
  srFlow,
  srType,
  srReg,
  srVariable,
  srOutput,
  srOp,
  srOpUtils,
  emit_fetch;

type
 TEmit_EXP=class(TEmitFetch)
  procedure emit_EXP;
 end;

implementation

procedure TEmit_EXP.emit_EXP;
Var
 exc:PsrRegNode;
 node:PSpirvOp;
 pOpBlock:PsrOpBlock;

 dout:PsrVariable;
 dst:PsrRegNode;
 src:array[0..3] of PsrRegNode;
 //rsl:array[0..3] of PsrRegNode;
 rtype:TsrDataType;
 f,i,p:DWORD;

 push_count:DWORD;
begin
 //if (VM<>0) and (EXEC<>0) = set pixel else (if DONE=1) discard pixel /(PS only)

 push_count:=0;

 pOpBlock:=nil;
 if (FSPI.EXP.VM<>0) and (FSPI.EXP.DONE<>0) then
 begin
  pOpBlock:=AllocBlockOp;
  pOpBlock^.SetInfo(btOther,Cursor.Adr,Cursor.Adr);

  PushBlockOp(line,pOpBlock,nil);
  Inc(push_count);

  exc:=MakeRead(get_exec0,dtBool);
  node:=AddSpirvOp(OpMakeExp);
  node^.AddParam(exc); //<-fetch read
 end;

 //before
 if (TpsslExportType(FSPI.EXP.TGT)=etNull) //only set kill mask
   or (FSPI.EXP.EN=0) then //nop
 begin

  While (push_count<>0) do
  begin
   Main^.PopBlock;
   Dec(push_count);
  end;

  Exit;
 end;

 pOpBlock:=AllocBlockOp; //down
 pOpBlock^.SetInfo(btOther,Cursor.Adr,Cursor.Adr);

 PushBlockOp(line,pOpBlock,nil);
 Inc(push_count);

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
   OpStore(line,dout,src[0]);
  end else
  begin

   Case p of
    2:rtype:=dtVec2f;
    3:rtype:=dtVec3f;
    4:rtype:=dtVec4f;
    else
      Assert(false,IntToStr(p));
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

   dst:=OpMakeVec(line,rtype,@src);

   dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
   OpStore(line,dout,dst);
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

  if Config.UseOutput16 then
  begin
   dst:=OpMakeVec(line,dtVec4h,@src);

   rtype:=dtVec4h;
  end else
  begin
   src[0]:=OpFToF(src[0],dtFloat32);
   src[1]:=OpFToF(src[1],dtFloat32);
   src[2]:=OpFToF(src[2],dtFloat32);
   src[3]:=OpFToF(src[3],dtFloat32);

   rtype:=dtVec4f;
  end;

  dst:=OpMakeVec(line,rtype,@src);

  dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
  OpStore(line,dout,dst);
 end;

 While (push_count<>0) do
 begin
  Main^.PopBlock;
  Dec(push_count);
 end;
end;

end.

