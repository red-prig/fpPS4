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
  procedure emit_V_CMP_32(OpId:DWORD;rtype:TsrDataType;x:Boolean);
  procedure emit_V_CMP_C (r,x:Boolean);
 end;

implementation

procedure TEmit_VOPC.emit_V_CMP_32(OpId:DWORD;rtype:TsrDataType;x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
 src:array[0..1] of TsrRegNode;
begin
 dst[0]:=get_vcc0;
 dst[1]:=get_vcc1;

 src[0]:=fetch_ssrc9(FSPI.VOPC.SRC0 ,rtype);
 src[1]:=fetch_vsrc8(FSPI.VOPC.VSRC1,rtype);
 OpCmpV(OpId,dst[0],src[0],src[1]);

 SetConst_q(dst[1],dtUnknow,0); //set zero

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  SetConst_q(get_exec1,dtUnknow,0);     //set zero
 end;
end;

procedure TEmit_VOPC.emit_V_CMP_C(r,x:Boolean);
Var
 dst:array[0..1] of PsrRegSlot;
begin
 dst[0]:=get_vcc0;
 dst[1]:=get_vcc1;

 SetConst_b(dst[0],r);
 SetConst_q(dst[1],dtUnknow,0); //set zero

 if x then
 begin
  MakeCopy(get_exec0,dst[0]^.current);
  SetConst_q(get_exec1,dtUnknow,0);     //set zero
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

   V_CMP_LT_F32    :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,false);
   V_CMP_EQ_F32    :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,false);
   V_CMP_LE_F32    :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
   V_CMP_GT_F32    :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,false);
   V_CMP_LG_F32    :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,false);
   V_CMP_GE_F32    :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
   V_CMP_O_F32     :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,false);
   V_CMP_U_F32     :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,false);
   V_CMP_NGE_F32   :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,false);
   V_CMP_NLG_F32   :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,false);
   V_CMP_NGT_F32   :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
   V_CMP_NLE_F32   :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,false);
   V_CMP_NEQ_F32   :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,false);
   V_CMP_NLT_F32   :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

   V_CMPX_LT_F32   :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,true);
   V_CMPX_EQ_F32   :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,true);
   V_CMPX_LE_F32   :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
   V_CMPX_GT_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,true);
   V_CMPX_LG_F32   :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,true);
   V_CMPX_GE_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
   V_CMPX_O_F32    :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,true);
   V_CMPX_U_F32    :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,true);
   V_CMPX_NGE_F32  :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,true);
   V_CMPX_NLG_F32  :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,true);
   V_CMPX_NGT_F32  :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
   V_CMPX_NLE_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,true);
   V_CMPX_NEQ_F32  :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,true);
   V_CMPX_NLT_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

   //

   V_CMPS_LT_F32   :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,false);
   V_CMPS_EQ_F32   :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,false);
   V_CMPS_LE_F32   :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,false);
   V_CMPS_GT_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,false);
   V_CMPS_LG_F32   :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,false);
   V_CMPS_GE_F32   :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,false);
   V_CMPS_O_F32    :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,false);
   V_CMPS_U_F32    :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,false);
   V_CMPS_NGE_F32  :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,false);
   V_CMPS_NLG_F32  :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,false);
   V_CMPS_NGT_F32  :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,false);
   V_CMPS_NLE_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,false);
   V_CMPS_NEQ_F32  :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,false);
   V_CMPS_NLT_F32  :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,false);

   V_CMPSX_LT_F32  :emit_V_CMP_32(Op.OpFOrdLessThan          ,dtFloat32,true);
   V_CMPSX_EQ_F32  :emit_V_CMP_32(Op.OpFOrdEqual             ,dtFloat32,true);
   V_CMPSX_LE_F32  :emit_V_CMP_32(Op.OpFOrdLessThanEqual     ,dtFloat32,true);
   V_CMPSX_GT_F32  :emit_V_CMP_32(Op.OpFOrdGreaterThan       ,dtFloat32,true);
   V_CMPSX_LG_F32  :emit_V_CMP_32(Op.OpFOrdNotEqual          ,dtFloat32,true);
   V_CMPSX_GE_F32  :emit_V_CMP_32(Op.OpFOrdGreaterThanEqual  ,dtFloat32,true);
   V_CMPSX_O_F32   :emit_V_CMP_32(Op.OpOrdered               ,dtFloat32,true);
   V_CMPSX_U_F32   :emit_V_CMP_32(Op.OpUnordered             ,dtFloat32,true);
   V_CMPSX_NGE_F32 :emit_V_CMP_32(Op.OpFUnordLessThan        ,dtFloat32,true);
   V_CMPSX_NLG_F32 :emit_V_CMP_32(Op.OpFUnordEqual           ,dtFloat32,true);
   V_CMPSX_NGT_F32 :emit_V_CMP_32(Op.OpFUnordLessThanEqual   ,dtFloat32,true);
   V_CMPSX_NLE_F32 :emit_V_CMP_32(Op.OpFUnordGreaterThan     ,dtFloat32,true);
   V_CMPSX_NEQ_F32 :emit_V_CMP_32(Op.OpFUnordNotEqual        ,dtFloat32,true);
   V_CMPSX_NLT_F32 :emit_V_CMP_32(Op.OpFUnordGreaterThanEqual,dtFloat32,true);

   //

   V_CMP_LT_I32    :emit_V_CMP_32(Op.OpSLessThan             ,dtInt32,false);
   V_CMP_EQ_I32    :emit_V_CMP_32(Op.OpIEqual                ,dtInt32,false);
   V_CMP_LE_I32    :emit_V_CMP_32(Op.OpSLessThanEqual        ,dtInt32,false);
   V_CMP_GT_I32    :emit_V_CMP_32(Op.OpSGreaterThan          ,dtInt32,false);
   V_CMP_LG_I32    :emit_V_CMP_32(Op.OpINotEqual             ,dtInt32,false);
   V_CMP_GE_I32    :emit_V_CMP_32(Op.OpSGreaterThanEqual     ,dtInt32,false);

   V_CMPX_LT_I32   :emit_V_CMP_32(Op.OpSLessThan             ,dtInt32,true);
   V_CMPX_EQ_I32   :emit_V_CMP_32(Op.OpIEqual                ,dtInt32,true);
   V_CMPX_LE_I32   :emit_V_CMP_32(Op.OpSLessThanEqual        ,dtInt32,true);
   V_CMPX_GT_I32   :emit_V_CMP_32(Op.OpSGreaterThan          ,dtInt32,true);
   V_CMPX_LG_I32   :emit_V_CMP_32(Op.OpINotEqual             ,dtInt32,true);
   V_CMPX_GE_I32   :emit_V_CMP_32(Op.OpSGreaterThanEqual     ,dtInt32,true);

   V_CMP_LT_U32    :emit_V_CMP_32(Op.OpULessThan             ,dtUint32,false);
   V_CMP_EQ_U32    :emit_V_CMP_32(Op.OpIEqual                ,dtUint32,false);
   V_CMP_LE_U32    :emit_V_CMP_32(Op.OpULessThanEqual        ,dtUint32,false);
   V_CMP_GT_U32    :emit_V_CMP_32(Op.OpUGreaterThan          ,dtUint32,false);
   V_CMP_LG_U32    :emit_V_CMP_32(Op.OpINotEqual             ,dtUint32,false);
   V_CMP_GE_U32    :emit_V_CMP_32(Op.OpUGreaterThanEqual     ,dtUint32,false);

   V_CMPX_LT_U32   :emit_V_CMP_32(Op.OpULessThan             ,dtUint32,true);
   V_CMPX_EQ_U32   :emit_V_CMP_32(Op.OpIEqual                ,dtUint32,true);
   V_CMPX_LE_U32   :emit_V_CMP_32(Op.OpULessThanEqual        ,dtUint32,true);
   V_CMPX_GT_U32   :emit_V_CMP_32(Op.OpUGreaterThan          ,dtUint32,true);
   V_CMPX_LG_U32   :emit_V_CMP_32(Op.OpINotEqual             ,dtUint32,true);
   V_CMPX_GE_U32   :emit_V_CMP_32(Op.OpUGreaterThanEqual     ,dtUint32,true);

  else
   Assert(false,'VOPC?');
 end;

end;

end.

