unit emit_EXP;

{$mode objfpc}{$H+}

interface

uses
  typinfo,
  sysutils,
  spirv,
  ps4_pssl,
  si_ci_vi_merged_enum,
  srCFGCursor,
  srCFGParser,
  srConfig,
  srFlow,
  srType,
  srReg,
  srOutput,
  srOp,
  srOpInternal,
  emit_fetch;

type
 TsrNormMode=(Normal,UNorm16,SNorm16);

type
 TEmit_EXP=class(TEmitFetch)
  procedure emit_EXP;
  function  get_export_type(TGT:Byte):TsrDataType;
  function  get_export_type_compr(TGT:Byte):TsrDataType;
  function  get_export_norm_compr(TGT:Byte):TsrNormMode;
  function  is_bindless(TGT:Byte):Boolean;
  function  get_export_sel(TGT:Byte):Tdst_sel;
  procedure fetch_vsrc8_vec2(VSRC:Word;nmode:TsrNormMode;rtype:TsrDataType;var dst0,dst1:TsrRegNode);
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

 shader_swizzle_map:array[1..4,SWAP_STD..SWAP_ALT_REV] of Tdst_sel=(
  (
    (d:(4,z,z,7)),
    (d:(z,4,z,7)),
    (d:(z,z,4,7)),
    (d:(z,z,z,4))
  ),(
    (d:(4,5,z,7)),
    (d:(4,z,z,5)),
    (d:(5,4,z,7)),
    (d:(5,z,z,4))
  ),(
    (d:(4,5,6,7)),
    (d:(4,5,7,6)),
    (d:(6,5,4,7)),
    (d:(6,5,7,4))
  ),(
    (d:(4,5,6,7)),
    (d:(6,5,4,7)),
    (d:(7,6,5,4)),
    (d:(5,6,7,4))
  )
 );

// R   G   B    A
//SPI_SHADER_ZERO         0  -   -   -    -
//SPI_SHADER_32_R         1  R   -   -    -
//SPI_SHADER_32_GR        2  R   G   -    -
//SPI_SHADER_32_AR        3  R   -   -    A
//SPI_SHADER_32_ABGR      9  R   G   B    A
//SPI_SHADER_FP16_ABGR    4 R|G B|A
//SPI_SHADER_UNORM16_ABGR 5 R|G B|A
//SPI_SHADER_SNORM16_ABGR 6 R|G B|A
//SPI_SHADER_UINT16_ABGR  7 R|G B|A
//SPI_SHADER_SINT16_ABGR  8 R|G B|A

function TEmit_EXP.get_export_type(TGT:Byte):TsrDataType;
begin
 Result:=dtFloat32;
 case TgcnExportType(TGT) of
  etMrt0..etMrt7:
   begin
    if is_bindless(TGT) then
    begin
     //
    end else
    case (OutputList.FExportMrt[TGT].EXPORT_FORMAT and 15) of

     SPI_SHADER_32_R   ,
     SPI_SHADER_32_GR  ,
     SPI_SHADER_32_AR  ,
     SPI_SHADER_32_ABGR:
        case (OutputList.FExportMrt[TGT].NUMBER_TYPE and 7) of
         NUMBER_UINT:Result:=dtUint32;
         NUMBER_SINT:Result:=dtInt32;
        else;
        end;

     SPI_SHADER_FP16_ABGR   ,
     SPI_SHADER_UNORM16_ABGR,
     SPI_SHADER_SNORM16_ABGR,
     SPI_SHADER_UINT16_ABGR ,
     SPI_SHADER_SINT16_ABGR :
        Result:=dtUint32;

     else;
    end;

   end;
 else;
 end;
end;

function TEmit_EXP.get_export_type_compr(TGT:Byte):TsrDataType;
begin
 Result:=dtHalf16;
 case TgcnExportType(TGT) of
  etMrt0..etMrt7:
   begin
    if is_bindless(TGT) then
    begin
     //
    end else
    case (OutputList.FExportMrt[TGT].EXPORT_FORMAT and 15) of

     SPI_SHADER_32_R   ,
     SPI_SHADER_32_GR  ,
     SPI_SHADER_32_AR  ,
     SPI_SHADER_32_ABGR:
        Result:=dtUint16;

     SPI_SHADER_FP16_ABGR   :Result:=dtHalf16;
     SPI_SHADER_UNORM16_ABGR:Result:=dtFloat32;
     SPI_SHADER_SNORM16_ABGR:Result:=dtFloat32;
     SPI_SHADER_UINT16_ABGR :Result:=dtUint16;
     SPI_SHADER_SINT16_ABGR :Result:=dtInt16;

     else;
    end;

   end;
 else;
 end;
end;

function TEmit_EXP.get_export_norm_compr(TGT:Byte):TsrNormMode;
begin
 Result:=Normal;
 case TgcnExportType(TGT) of
  etMrt0..etMrt7:
   begin
    if is_bindless(TGT) then
    begin
     //
    end else
    case (OutputList.FExportMrt[TGT].EXPORT_FORMAT and 15) of

     SPI_SHADER_UNORM16_ABGR:Result:=UNorm16;
     SPI_SHADER_SNORM16_ABGR:Result:=SNorm16;

     else;
    end;

   end;
 else;
 end;
end;

function TEmit_EXP.is_bindless(TGT:Byte):Boolean;
begin
 Result:=False;
 case TgcnExportType(TGT) of
  etMrt0..etMrt7:
   begin
    Result:=(COLOR_COUNT[OutputList.FExportMrt[TGT].RENDER_FORMAT and 31]=0) or
            ((OutputList.FExportMrt[TGT].EXPORT_FORMAT and 15)=0);
   end;
 else;
 end;
end;

function TEmit_EXP.get_export_sel(TGT:Byte):Tdst_sel;
var
 i:Byte;
begin
 Result:=dst_sel_ident;
 case TgcnExportType(TGT) of
  etMrt0..etMrt7:
   begin
    if is_bindless(TGT) then
    begin
     Result:=dst_sel_ident;
    end else
    begin
     i:=COLOR_COUNT[OutputList.FExportMrt[TGT].RENDER_FORMAT and 31];
     //
     Result:=shader_swizzle_map[i,OutputList.FExportMrt[TGT].COMP_SWAP and 3];
    end;
   end;
 else;
 end;
end;

procedure TEmit_EXP.fetch_vsrc8_vec2(VSRC:Word;nmode:TsrNormMode;rtype:TsrDataType;var dst0,dst1:TsrRegNode);
var
 pSlot:PsrRegSlot;
 src,dst:TsrRegNode;
begin
 pSlot:=RegsStory.get_vsrc8(VSRC);

 case nmode of
  Normal:
    begin
     dst:=MakeRead(pSlot,rtype.AsVector(2));
     Assert(dst<>nil,'fetch_vsrc8_vec2');
    end;
  UNorm16:
    begin
     src:=MakeRead(pSlot,dtUint32);
     Assert(src<>nil,'fetch_vsrc8_vec2');
     dst:=NewReg(dtVec2f);
     _OpGlsl1(line,GlslOp.UnpackUnorm2x16,dst,src);
    end;
  SNorm16:
    begin
     src:=MakeRead(pSlot,dtUint32);
     Assert(src<>nil,'fetch_vsrc8_vec2');
     dst:=NewReg(dtVec2f);
     _OpGlsl1(line,GlslOp.UnpackSnorm2x16,dst,src);
    end;
 end;

 dst0:=NewReg(rtype);
 dst1:=NewReg(rtype);

 OpExtract(line,dst0,dst,0);
 OpExtract(line,dst1,dst,1);
end;

procedure TEmit_EXP.shuffle(dst_sel:Tdst_sel;rtype:TsrDataType;src:PPsrRegNode;count:Byte);
var
 i:Byte;
 dst:array[0..3] of TsrRegNode;
begin
 For i:=0 to count-1 do
 begin

  case dst_sel.d[i] of
   0:dst[i]:=NewImm_i(rtype.Child,0);
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
 exc:TsrRegNode;
 node:TSpirvOp;
 parent,pOpBlock:TsrOpBlock;

 dout:TsrOutput;
 dst:TsrRegNode;
 src:array[0..3] of TsrRegNode;
 rtype:TsrDataType;
 nmode:TsrNormMode;
 f,i,p:DWORD;

 dst_sel:Tdst_sel;

 misc:PExportPos;

 push_count:DWORD;
begin
 //if (VM<>0) and (EXEC<>0) = set pixel else (if DONE=1) discard pixel /(PS only)

 push_count:=0;

 parent:=nil;
 if (FSPI.EXP.VM<>0) and (FSPI.EXP.DONE<>0) then
 begin
  parent:=AllocBlockOp;
  parent.SetInfo(btOther);

  PushBlockOp(line,parent,Default(TsrCursor));
  Inc(push_count);

  exc:=GetThreadBit(get_exec0,get_exec1,dtBool);
  node:=AddSpirvOp(srOpInternal.OpExport);
  node.AddParam(exc); //<-fetch read
 end;

 //before
 if (TgcnExportType(FSPI.EXP.TGT)=etNull) or //only set kill mask
    (FSPI.EXP.EN=0){ or                       //nop
    is_bindless(FSPI.EXP.TGT)} then           //not binded
 begin

  While (push_count<>0) do
  begin
   Main.PopBlock;
   Dec(push_count);
  end;

  Exit;
 end;

 pOpBlock:=AllocBlockOp; //down
 pOpBlock.SetInfo(btOther);

 PushBlockOp(line,pOpBlock,Default(TsrCursor));
 Inc(push_count);

 if (parent<>nil) then
 begin
  parent  .pElse:=pOpBlock; //if->else
  pOpBlock.pIf  :=parent;   //else->if
 end;

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

   dst:=src[0];
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

   //TODO:SHADER_POS_FORMAT:accounting for parameters passed to the pixel shader
   //TODO:SHADER_COL_FORMAT:accounting for partial 32-bit formats and 16-bit formats

   dst_sel:=get_export_sel(FSPI.EXP.TGT);

   shuffle(dst_sel,rtype,@src,p);

   dst:=OpVectorTo(line,rtype,@src);
  end;

 end else
 begin //half16,unorm16,snorm16,uint16,sint16

  rtype:=get_export_type_compr(FSPI.EXP.TGT);
  nmode:=get_export_norm_compr(FSPI.EXP.TGT);

  //TODO:SHADER_COL_FORMAT:accounting for 32-bit formats
  Case f of
    3,
    $F:
      begin
       fetch_vsrc8_vec2(FSPI.EXP.VSRC0,nmode,rtype,src[0],src[1]);
       fetch_vsrc8_vec2(FSPI.EXP.VSRC1,nmode,rtype,src[2],src[3]);
      end;
   $C:
      begin
       fetch_vsrc8_vec2(FSPI.EXP.VSRC2,nmode,rtype,src[0],src[1]);
       fetch_vsrc8_vec2(FSPI.EXP.VSRC3,nmode,rtype,src[2],src[3]);
      end;
   else
    Assert(false,'FSPI.EXP.COMPR='+HexStr(f,1));
  end;

  //TODO:SHADER_POS_FORMAT:accounting for parameters passed to the pixel shader
  //TODO:SHADER_COL_FORMAT:accounting for 32-bit formats

  if Config.UseOutput16 then
  begin
   rtype:=rtype.AsVector(4);
  end else
  begin

   case rtype of
    dtHalf16:
      begin
       rtype:=dtFloat32;
       for i:=0 to 3 do src[i]:=OpFToF(src[i],rtype);
      end;
    dtUint16:
      begin
       rtype:=dtUint32;
       for i:=0 to 3 do src[i]:=OpUToU(src[i],rtype);
      end;
    dtInt16:
      begin
       rtype:=dtInt32;
       for i:=0 to 3 do src[i]:=OpSToS(src[i],rtype);
      end;
    else;
   end;

   rtype:=rtype.AsVector(4);
  end;

  dst_sel:=get_export_sel(FSPI.EXP.TGT);

  shuffle(dst_sel,rtype,@src,4);

  dst:=OpVectorTo(line,rtype,@src);
 end;

 misc:=OutputList.GetExportPos(TgcnExportType(FSPI.EXP.TGT));

 if (misc<>nil) then
 begin

  for i:=0 to 3 do
  begin
   case misc^[i] of
    ptNone:; //skip
    ptCullDist0..ptCullDist7:; //TODO: CullDist
    ptClipDist0..ptClipDist7:; //TODO: ClipDist
    else
     Assert(false,'Export:'+GetEnumName(TypeInfo(TgcnPosType),ord(misc^[i])));
   end;
  end;

 end else
 begin
  dout:=FetchOutput(TgcnExportType(FSPI.EXP.TGT),rtype); //output in FSPI.EXP.TGT
  dout.FetchStore(line,dst);
 end;

 While (push_count<>0) do
 begin
  Main.PopBlock;
  Dec(push_count);
 end;
end;

end.

