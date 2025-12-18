unit emit_VOPC;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srType,
  srReg,
  spirv,
  emit_fetch;

type
 TEmit_VOPC=class(TEmitFetch)
  procedure emit_VOPC;
  procedure emit_V_CMP  (OpId:DWORD;rtype:TsrDataType;x:Boolean);
  procedure emit_V_CMP_C(r,x:Boolean);
  procedure emit_V_CMP_CLASS_32(x:Boolean);
 end;

implementation

procedure TEmit_VOPC.emit_V_CMP(OpId:DWORD;rtype:TsrDataType;x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst[0]:=get_vcc0;
 dst[1]:=get_vcc1;


 if (rtype.BitSize=64) then
 begin
  src[0]:=fetch_ssrc9_64(FSPI.VOPC.SRC0 ,rtype);
  src[1]:=fetch_vsrc8_64(FSPI.VOPC.VSRC1,rtype);
 end else
 begin
  src[0]:=fetch_ssrc9(FSPI.VOPC.SRC0 ,rtype);
  src[1]:=fetch_vsrc8(FSPI.VOPC.VSRC1,rtype);
 end;

 OpCmpV(OpId,dst[0],dst[1],src[0],src[1]);

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  MakeCopy(get_exec1,dst[1]^.current);
 end;
end;

procedure TEmit_VOPC.emit_V_CMP_C(r,x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
begin
 dst[0]:=get_vcc0;
 dst[1]:=get_vcc1;

 SetThreadBit(dst[0],dst[1],NewImm_b(r));

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  MakeCopy(get_exec1,dst[1]^.current);
 end;
end;

procedure TEmit_VOPC.emit_V_CMP_CLASS_32(x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst[0]:=get_vcc0;
 dst[1]:=get_vcc1;

 src[0]:=fetch_ssrc9(FSPI.VOPC.SRC0 ,dtFloat32);
 src[1]:=fetch_vsrc8(FSPI.VOPC.VSRC1,dtUInt32);

 OpCmpClass(dst[0],dst[1],src[0],src[1]);

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  MakeCopy(get_exec1,dst[1]^.current);
 end;
end;

procedure TEmit_VOPC.emit_VOPC;
begin

 Case FSPI.VOPC.OP of

   V_CMP_F_F32,
   V_CMP_F_F64,
   V_CMP_F_I32,
   V_CMP_F_I64,
   V_CMP_F_U32,
   V_CMP_F_U64,
   V_CMPS_F_F32,
   V_CMPS_F_F64:emit_V_CMP_C(false,false);

   V_CMP_T_F32,
   V_CMP_T_F64,
   V_CMP_T_I32,
   V_CMP_T_I64,
   V_CMP_T_U32,
   V_CMP_T_U64,
   V_CMPS_T_F32,
   V_CMPS_T_F64:emit_V_CMP_C(true,false);

   V_CMPX_F_F32,
   V_CMPX_F_F64,
   V_CMPX_F_I32,
   V_CMPX_F_I64,
   V_CMPX_F_U32,
   V_CMPX_F_U64,
   V_CMPSX_F_F32,
   V_CMPSX_F_F64:emit_V_CMP_C(false,true);

   V_CMPX_T_F32,
   V_CMPX_T_F64,
   V_CMPX_T_I32,
   V_CMPX_T_I64,
   V_CMPX_T_U32,
   V_CMPX_T_U64,
   V_CMPSX_T_F32,
   V_CMPSX_T_F64:emit_V_CMP_C(true,true);

   //

   V_CMP_LT_F32    :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,false);
   V_CMP_EQ_F32    :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,false);
   V_CMP_LE_F32    :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
   V_CMP_GT_F32    :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,false);
   V_CMP_LG_F32    :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,false);
   V_CMP_GE_F32    :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
   V_CMP_O_F32     :emit_V_CMP(Op.OpOrdered               ,dtFloat32,false);
   V_CMP_U_F32     :emit_V_CMP(Op.OpUnordered             ,dtFloat32,false);
   V_CMP_NGE_F32   :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,false);
   V_CMP_NLG_F32   :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,false);
   V_CMP_NGT_F32   :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
   V_CMP_NLE_F32   :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,false);
   V_CMP_NEQ_F32   :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,false);
   V_CMP_NLT_F32   :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

   V_CMPX_LT_F32   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,true);
   V_CMPX_EQ_F32   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,true);
   V_CMPX_LE_F32   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
   V_CMPX_GT_F32   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,true);
   V_CMPX_LG_F32   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,true);
   V_CMPX_GE_F32   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
   V_CMPX_O_F32    :emit_V_CMP(Op.OpOrdered               ,dtFloat32,true);
   V_CMPX_U_F32    :emit_V_CMP(Op.OpUnordered             ,dtFloat32,true);
   V_CMPX_NGE_F32  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,true);
   V_CMPX_NLG_F32  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,true);
   V_CMPX_NGT_F32  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
   V_CMPX_NLE_F32  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,true);
   V_CMPX_NEQ_F32  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,true);
   V_CMPX_NLT_F32  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

   //

   V_CMPS_LT_F32   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,false);
   V_CMPS_EQ_F32   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,false);
   V_CMPS_LE_F32   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
   V_CMPS_GT_F32   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,false);
   V_CMPS_LG_F32   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,false);
   V_CMPS_GE_F32   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
   V_CMPS_O_F32    :emit_V_CMP(Op.OpOrdered               ,dtFloat32,false);
   V_CMPS_U_F32    :emit_V_CMP(Op.OpUnordered             ,dtFloat32,false);
   V_CMPS_NGE_F32  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,false);
   V_CMPS_NLG_F32  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,false);
   V_CMPS_NGT_F32  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
   V_CMPS_NLE_F32  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,false);
   V_CMPS_NEQ_F32  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,false);
   V_CMPS_NLT_F32  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

   V_CMPSX_LT_F32  :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat32,true);
   V_CMPSX_EQ_F32  :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat32,true);
   V_CMPSX_LE_F32  :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
   V_CMPSX_GT_F32  :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat32,true);
   V_CMPSX_LG_F32  :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat32,true);
   V_CMPSX_GE_F32  :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
   V_CMPSX_O_F32   :emit_V_CMP(Op.OpOrdered               ,dtFloat32,true);
   V_CMPSX_U_F32   :emit_V_CMP(Op.OpUnordered             ,dtFloat32,true);
   V_CMPSX_NGE_F32 :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat32,true);
   V_CMPSX_NLG_F32 :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat32,true);
   V_CMPSX_NGT_F32 :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
   V_CMPSX_NLE_F32 :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat32,true);
   V_CMPSX_NEQ_F32 :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat32,true);
   V_CMPSX_NLT_F32 :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

   //

   V_CMP_LT_I32    :emit_V_CMP(Op.OpSLessThan             ,dtInt32,false);
   V_CMP_EQ_I32    :emit_V_CMP(Op.OpIEqual                ,dtInt32,false);
   V_CMP_LE_I32    :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt32,false);
   V_CMP_GT_I32    :emit_V_CMP(Op.OpSGreaterThan          ,dtInt32,false);
   V_CMP_LG_I32    :emit_V_CMP(Op.OpINotEqual             ,dtInt32,false);
   V_CMP_GE_I32    :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt32,false);

   V_CMPX_LT_I32   :emit_V_CMP(Op.OpSLessThan             ,dtInt32,true);
   V_CMPX_EQ_I32   :emit_V_CMP(Op.OpIEqual                ,dtInt32,true);
   V_CMPX_LE_I32   :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt32,true);
   V_CMPX_GT_I32   :emit_V_CMP(Op.OpSGreaterThan          ,dtInt32,true);
   V_CMPX_LG_I32   :emit_V_CMP(Op.OpINotEqual             ,dtInt32,true);
   V_CMPX_GE_I32   :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt32,true);

   V_CMP_LT_U32    :emit_V_CMP(Op.OpULessThan             ,dtUint32,false);
   V_CMP_EQ_U32    :emit_V_CMP(Op.OpIEqual                ,dtUint32,false);
   V_CMP_LE_U32    :emit_V_CMP(Op.OpULessThanEqual        ,dtUint32,false);
   V_CMP_GT_U32    :emit_V_CMP(Op.OpUGreaterThan          ,dtUint32,false);
   V_CMP_LG_U32    :emit_V_CMP(Op.OpINotEqual             ,dtUint32,false);
   V_CMP_GE_U32    :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint32,false);

   V_CMPX_LT_U32   :emit_V_CMP(Op.OpULessThan             ,dtUint32,true);
   V_CMPX_EQ_U32   :emit_V_CMP(Op.OpIEqual                ,dtUint32,true);
   V_CMPX_LE_U32   :emit_V_CMP(Op.OpULessThanEqual        ,dtUint32,true);
   V_CMPX_GT_U32   :emit_V_CMP(Op.OpUGreaterThan          ,dtUint32,true);
   V_CMPX_LG_U32   :emit_V_CMP(Op.OpINotEqual             ,dtUint32,true);
   V_CMPX_GE_U32   :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint32,true);

   //

   V_CMP_LT_F64    :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,false);
   V_CMP_EQ_F64    :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,false);
   V_CMP_LE_F64    :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,false);
   V_CMP_GT_F64    :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,false);
   V_CMP_LG_F64    :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,false);
   V_CMP_GE_F64    :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,false);
   V_CMP_O_F64     :emit_V_CMP(Op.OpOrdered               ,dtFloat64,false);
   V_CMP_U_F64     :emit_V_CMP(Op.OpUnordered             ,dtFloat64,false);
   V_CMP_NGE_F64   :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,false);
   V_CMP_NLG_F64   :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,false);
   V_CMP_NGT_F64   :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,false);
   V_CMP_NLE_F64   :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,false);
   V_CMP_NEQ_F64   :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,false);
   V_CMP_NLT_F64   :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,false);

   V_CMPX_LT_F64   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,true);
   V_CMPX_EQ_F64   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,true);
   V_CMPX_LE_F64   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,true);
   V_CMPX_GT_F64   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,true);
   V_CMPX_LG_F64   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,true);
   V_CMPX_GE_F64   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,true);
   V_CMPX_O_F64    :emit_V_CMP(Op.OpOrdered               ,dtFloat64,true);
   V_CMPX_U_F64    :emit_V_CMP(Op.OpUnordered             ,dtFloat64,true);
   V_CMPX_NGE_F64  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,true);
   V_CMPX_NLG_F64  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,true);
   V_CMPX_NGT_F64  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,true);
   V_CMPX_NLE_F64  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,true);
   V_CMPX_NEQ_F64  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,true);
   V_CMPX_NLT_F64  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,true);

   //

   V_CMPS_LT_F64   :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,false);
   V_CMPS_EQ_F64   :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,false);
   V_CMPS_LE_F64   :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,false);
   V_CMPS_GT_F64   :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,false);
   V_CMPS_LG_F64   :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,false);
   V_CMPS_GE_F64   :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,false);
   V_CMPS_O_F64    :emit_V_CMP(Op.OpOrdered               ,dtFloat64,false);
   V_CMPS_U_F64    :emit_V_CMP(Op.OpUnordered             ,dtFloat64,false);
   V_CMPS_NGE_F64  :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,false);
   V_CMPS_NLG_F64  :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,false);
   V_CMPS_NGT_F64  :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,false);
   V_CMPS_NLE_F64  :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,false);
   V_CMPS_NEQ_F64  :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,false);
   V_CMPS_NLT_F64  :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,false);

   V_CMPSX_LT_F64  :emit_V_CMP(Op.OpFOrdLessThan          ,dtFloat64,true);
   V_CMPSX_EQ_F64  :emit_V_CMP(Op.OpFOrdEqual             ,dtFloat64,true);
   V_CMPSX_LE_F64  :emit_V_CMP(Op.OpFOrdLessThanEqual     ,dtFloat64,true);
   V_CMPSX_GT_F64  :emit_V_CMP(Op.OpFOrdGreaterThan       ,dtFloat64,true);
   V_CMPSX_LG_F64  :emit_V_CMP(Op.OpFOrdNotEqual          ,dtFloat64,true);
   V_CMPSX_GE_F64  :emit_V_CMP(Op.OpFOrdGreaterThanEqual  ,dtFloat64,true);
   V_CMPSX_O_F64   :emit_V_CMP(Op.OpOrdered               ,dtFloat64,true);
   V_CMPSX_U_F64   :emit_V_CMP(Op.OpUnordered             ,dtFloat64,true);
   V_CMPSX_NGE_F64 :emit_V_CMP(Op.OpFUnordLessThan        ,dtFloat64,true);
   V_CMPSX_NLG_F64 :emit_V_CMP(Op.OpFUnordEqual           ,dtFloat64,true);
   V_CMPSX_NGT_F64 :emit_V_CMP(Op.OpFUnordLessThanEqual   ,dtFloat64,true);
   V_CMPSX_NLE_F64 :emit_V_CMP(Op.OpFUnordGreaterThan     ,dtFloat64,true);
   V_CMPSX_NEQ_F64 :emit_V_CMP(Op.OpFUnordNotEqual        ,dtFloat64,true);
   V_CMPSX_NLT_F64 :emit_V_CMP(Op.OpFUnordGreaterThanEqual,dtFloat64,true);

   //

   V_CMP_LT_I64    :emit_V_CMP(Op.OpSLessThan             ,dtInt64,false);
   V_CMP_EQ_I64    :emit_V_CMP(Op.OpIEqual                ,dtInt64,false);
   V_CMP_LE_I64    :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt64,false);
   V_CMP_GT_I64    :emit_V_CMP(Op.OpSGreaterThan          ,dtInt64,false);
   V_CMP_LG_I64    :emit_V_CMP(Op.OpINotEqual             ,dtInt64,false);
   V_CMP_GE_I64    :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt64,false);

   V_CMPX_LT_I64   :emit_V_CMP(Op.OpSLessThan             ,dtInt64,true);
   V_CMPX_EQ_I64   :emit_V_CMP(Op.OpIEqual                ,dtInt64,true);
   V_CMPX_LE_I64   :emit_V_CMP(Op.OpSLessThanEqual        ,dtInt64,true);
   V_CMPX_GT_I64   :emit_V_CMP(Op.OpSGreaterThan          ,dtInt64,true);
   V_CMPX_LG_I64   :emit_V_CMP(Op.OpINotEqual             ,dtInt64,true);
   V_CMPX_GE_I64   :emit_V_CMP(Op.OpSGreaterThanEqual     ,dtInt64,true);

   V_CMP_LT_U64    :emit_V_CMP(Op.OpULessThan             ,dtUint64,false);
   V_CMP_EQ_U64    :emit_V_CMP(Op.OpIEqual                ,dtUint64,false);
   V_CMP_LE_U64    :emit_V_CMP(Op.OpULessThanEqual        ,dtUint64,false);
   V_CMP_GT_U64    :emit_V_CMP(Op.OpUGreaterThan          ,dtUint64,false);
   V_CMP_LG_U64    :emit_V_CMP(Op.OpINotEqual             ,dtUint64,false);
   V_CMP_GE_U64    :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint64,false);

   V_CMPX_LT_U64   :emit_V_CMP(Op.OpULessThan             ,dtUint64,true);
   V_CMPX_EQ_U64   :emit_V_CMP(Op.OpIEqual                ,dtUint64,true);
   V_CMPX_LE_U64   :emit_V_CMP(Op.OpULessThanEqual        ,dtUint64,true);
   V_CMPX_GT_U64   :emit_V_CMP(Op.OpUGreaterThan          ,dtUint64,true);
   V_CMPX_LG_U64   :emit_V_CMP(Op.OpINotEqual             ,dtUint64,true);
   V_CMPX_GE_U64   :emit_V_CMP(Op.OpUGreaterThanEqual     ,dtUint64,true);

   //

   V_CMP_CLASS_F32 :emit_V_CMP_CLASS_32(false);
   V_CMPX_CLASS_F32:emit_V_CMP_CLASS_32(true );

   //

  else
   Assert(false,'VOPC?'+IntToStr(FSPI.VOPC.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

