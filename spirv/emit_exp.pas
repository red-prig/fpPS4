unit emit_EXP;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  si_ci_vi_merged_enum,
  srCFGLabel,
  srConfig,
  srFlow,
  srType,
  srReg,
  srOutput,
  srOp,
  srOpUtils,
  emit_fetch;

type
 Tdst_sel=array[0..3] of Byte;

 TEmit_EXP=class(TEmitFetch)
  procedure emit_EXP;
  function  get_export_type(TGT:Byte):TsrDataType;
  function  is_bindless(TGT:Byte):Boolean;
  function  get_export_sel(TGT:Byte):Tdst_sel;
  procedure fetch_vsrc8_vec2h(VSRC:Word;var dst0,dst1:PsrRegNode);
  procedure shuffle(dst_sel:Tdst_sel;rtype:TsrDataType;src:PPsrRegNode;count:Byte);
 end;

implementation

const
 COLOR_COUNT:array[0..31] of Byte=(
  0, //COLOR_INVALID
  1, //COLOR_8
  1, //COLOR_16
  2, //COLOR_8_8
  1, //COLOR_32
  2, //COLOR_16_16
  3, //COLOR_10_11_11
  3, //COLOR_11_11_10
  4, //COLOR_10_10_10_2
  4, //COLOR_2_10_10_10
  4, //COLOR_8_8_8_8
  2, //COLOR_32_32
  4, //COLOR_16_16_16_16
  3, //COLOR_RESERVED_13    //32_32_32
  4, //COLOR_32_32_32_32
  0, //COLOR_RESERVED_15
  3, //COLOR_5_6_5
  4, //COLOR_1_5_5_5
  4, //COLOR_5_5_5_1
  4, //COLOR_4_4_4_4
  2, //COLOR_8_24
  2, //COLOR_24_8
  3, //COLOR_X24_8_32_FLOAT
  0, //COLOR_RESERVED_23
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
 );

const
 z=0;

 dst_sel_ident:Tdst_sel=(4,5,6,7);

 shader_swizzle_map:array[1..4,SWAP_STD..SWAP_ALT_REV] of Tdst_sel=(
  (
    (4,z,z,z),
    (z,4,z,z),
    (z,z,4,z),
    (z,z,z,4)
  ),(
    (4,5,z,z),
    (4,z,z,5),
    (5,4,z,z),
    (5,z,z,4)
  ),(
    (4,5,6,z),
    (4,5,z,6),
    (6,5,4,z),
    (6,5,z,4)
  ),(
    (4,5,6,7),
    (6,5,4,7),
    (7,6,5,4),
    (5,6,7,4)
  )
 );

function TEmit_EXP.get_export_type(TGT:Byte):TsrDataType;
begin
 Result:=dtFloat32;
 case TpsslExportType(TGT) of
  etMrt0..etMrt7:
   begin
    if is_bindless(TGT) then
    begin
     Result:=dtFloat32;
    end else
    case (FExportInfo[TGT].NUMBER_TYPE and 7) of
     NUMBER_UINT:Result:=dtUint32;
     NUMBER_SINT:Result:=dtInt32;
    else;
    end;
   end;
 else;
 end;
end;

function TEmit_EXP.is_bindless(TGT:Byte):Boolean;
begin
 Result:=False;
 case TpsslExportType(TGT) of
  etMrt0..etMrt7:
   begin
    Result:=COLOR_COUNT[FExportInfo[TGT].FORMAT and 31]=0;
   end;
 else;
 end;
end;

function TEmit_EXP.get_export_sel(TGT:Byte):Tdst_sel;
var
 i:Byte;
begin
 Result:=dst_sel_ident;
 case TpsslExportType(TGT) of
  etMrt0..etMrt7:
   begin
    if is_bindless(TGT) then
    begin
     Result:=dst_sel_ident;
    end else
    begin
     i:=COLOR_COUNT[FExportInfo[TGT].FORMAT and 31];
     //
     Result:=shader_swizzle_map[i,FExportInfo[TGT].COMP_SWAP and 3];
    end;
   end;
 else;
 end;
end;

procedure TEmit_EXP.fetch_vsrc8_vec2h(VSRC:Word;var dst0,dst1:PsrRegNode);
var
 pSlot:PsrRegSlot;
 dst:PsrRegNode;
begin
 pSlot:=RegsStory.get_vsrc8(VSRC);

 dst:=MakeRead(pSlot,dtVec2h);
 Assert(dst<>nil,'fetch_vsrc8_vec2h');

 dst0:=NewReg(dtHalf16);
 dst1:=NewReg(dtHalf16);

 OpExtract(line,dst0,dst,0);
 OpExtract(line,dst1,dst,1);
end;

procedure TEmit_EXP.shuffle(dst_sel:Tdst_sel;rtype:TsrDataType;src:PPsrRegNode;count:Byte);
var
 i:Byte;
 dst:array[0..3] of PsrRegNode;
begin
 For i:=0 to count-1 do
 begin

  case dst_sel[i] of
   0:dst[i]:=NewReg_i(rtype.Child,0);
   //1?

   4:dst[i]:=src[0];
   5:dst[i]:=src[1];
   6:dst[i]:=src[2];
   7:dst[i]:=src[3];
   else;
    Assert(False);
  end;

 end;
 //
 For i:=0 to count-1 do
 begin
  src[i]:=dst[i];
 end;
end;

procedure TEmit_EXP.emit_EXP;
Var
 exc:PsrRegNode;
 node:PSpirvOp;
 pOpBlock:PsrOpBlock;

 dout:PsrOutput;
 dst:PsrRegNode;
 src:array[0..3] of PsrRegNode;
 rtype:TsrDataType;
 f,i,p:DWORD;

 dst_sel:Tdst_sel;

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
 if (TpsslExportType(FSPI.EXP.TGT)=etNull) or //only set kill mask
    (FSPI.EXP.EN=0){ or                        //nop
    is_bindless(FSPI.EXP.TGT)} then            //not binded
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

 if (FSPI.EXP.COMPR=0) then //float32,int32,uint32
 begin

  p:=PopCnt(f);

  if (p=1) then
  begin
   //scalar

   rtype:=get_export_type(FSPI.EXP.TGT);

   Case f of
    $1:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC0,rtype);
    $2:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC1,rtype);
    $4:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC2,rtype);
    $8:src[0]:=fetch_vsrc8(FSPI.EXP.VSRC3,rtype);
    else
     Assert(false,'FSPI.EXP.COMPR='+HexStr(f,1));
   end;

   //shuffle ???

   dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
   dout^.FetchStore(line,src[0]);
  end else
  begin
   //vector

   rtype:=get_export_type(FSPI.EXP.TGT);

   rtype:=rtype.AsVector(p);

   i:=0;
   if (f and $1<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC0,rtype.Child);
    Inc(i);
   end;
   if (f and $2<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC1,rtype.Child);
    Inc(i);
   end;
   if (f and $4<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC2,rtype.Child);
    Inc(i);
   end;
   if (f and $8<>0) then
   begin
    src[i]:=fetch_vsrc8(FSPI.EXP.VSRC3,rtype.Child);
    Inc(i);
   end;

   dst_sel:=get_export_sel(FSPI.EXP.TGT);

   shuffle(dst_sel,rtype,@src,p);

   dst:=OpMakeVec(line,rtype,@src);

   dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
   dout^.FetchStore(line,dst);
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
    Assert(false,'FSPI.EXP.COMPR='+HexStr(f,1));
  end;

  if Config.UseOutput16 then
  begin
   rtype:=dtVec4h;
  end else
  begin
   src[0]:=OpFToF(src[0],dtFloat32);
   src[1]:=OpFToF(src[1],dtFloat32);
   src[2]:=OpFToF(src[2],dtFloat32);
   src[3]:=OpFToF(src[3],dtFloat32);

   rtype:=dtVec4f;
  end;

  dst_sel:=get_export_sel(FSPI.EXP.TGT);

  shuffle(dst_sel,rtype,@src,4);

  dst:=OpMakeVec(line,rtype,@src);

  dout:=FetchOutput(TpsslExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
  dout^.FetchStore(line,dst);
 end;

 While (push_count<>0) do
 begin
  Main^.PopBlock;
  Dec(push_count);
 end;
end;

end.

