unit emit_SOPC;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  ps4_pssl,
  srTypes,
  srReg,
  SprvEmit,
  emit_op;

type
 TEmit_SOPC=object(TEmitOp)
  procedure _emit_SOPC;
  procedure _emit_S_CMP_32(OpId:DWORD;rtype:TsrDataType);
 end;

implementation

procedure TEmit_SOPC._emit_S_CMP_32(OpId:DWORD;rtype:TsrDataType);
Var
 dst:PsrRegSlot;
 src:array[0..1] of PsrRegNode;
begin
 dst:=@FRegsStory.SCC;

 src[0]:=fetch_ssrc9(FSPI.SOPC.SSRC0,rtype);
 src[1]:=fetch_ssrc9(FSPI.SOPC.SSRC1,rtype);

 emit_OpCmpS(OpId,dst,src[0],src[1]);
end;

procedure TEmit_SOPC._emit_SOPC;
begin
 Case FSPI.SOPC.OP of
  S_CMP_EQ_I32 :_emit_S_CMP_32(Op.OpIEqual           ,dtInt32);
  S_CMP_LG_I32 :_emit_S_CMP_32(Op.OpINotEqual        ,dtInt32);
  S_CMP_GT_I32 :_emit_S_CMP_32(Op.OpSGreaterThan     ,dtInt32);
  S_CMP_GE_I32 :_emit_S_CMP_32(Op.OpSGreaterThanEqual,dtInt32);
  S_CMP_LT_I32 :_emit_S_CMP_32(Op.OpSLessThan        ,dtInt32);
  S_CMP_LE_I32 :_emit_S_CMP_32(Op.OpSLessThanEqual   ,dtInt32);

  S_CMP_EQ_U32 :_emit_S_CMP_32(Op.OpIEqual           ,dtUint32);
  S_CMP_LG_U32 :_emit_S_CMP_32(Op.OpINotEqual        ,dtUint32);
  S_CMP_GT_U32 :_emit_S_CMP_32(Op.OpUGreaterThan     ,dtUint32);
  S_CMP_GE_U32 :_emit_S_CMP_32(Op.OpUGreaterThanEqual,dtUint32);
  S_CMP_LT_U32 :_emit_S_CMP_32(Op.OpULessThan        ,dtUint32);
  S_CMP_LE_U32 :_emit_S_CMP_32(Op.OpULessThanEqual   ,dtUint32);

  //S_BITCMP0_B32:;
  //S_BITCMP1_B32:;
  //S_BITCMP0_B64:;
  //S_BITCMP1_B64:;
  //S_SETVSKIP   :;
  else
    Assert(false,'SOPC?'+IntToStr(FSPI.SOPC.OP));
 end;

end;

end.

