unit emit_fetch;

{$mode objfpc}{$H+}

interface

uses
 ps4_shader,
 ps4_pssl,
 spirv,
 srNode,
 srBitcast,
 srType,
 srTypes,
 srConst,
 srReg,
 srOp,
 srOpInternal,
 srOpUtils,
 srLayout,
 srVertLayout,
 srFragLayout,
 srVariable,
 srInput,
 srOutput,
 srUniform,
 srFlow;

type
 TEmitFetch=class(TEmitFlow)
  //
  function  GroupingVImm  (regs:PPsrRegNode;rtype:TsrResourceType):TsrDataLayout;
  function  TryShortVSharp(regs:PPsrRegNode;rtype:TsrResourceType):TsrDataLayout;
  function  GroupingSharp (src :PPsrRegSlot;rtype:TsrResourceType):TsrDataLayout;
  //
  function  get_sdst7(SDST:Byte):PsrRegSlot;
  function  get_sdst7_pair(SDST:Byte;dst:PPsrRegSlot):Boolean;
  function  get_sdst8(SSRC:Byte):PsrRegSlot;
  function  get_ssrc8(SSRC:Byte):PsrRegSlot;
  function  get_ssrc9(SSRC:Word):PsrRegSlot;
  function  get_ssrc9_pair(SSRC:Word;src:PPsrRegSlot):Boolean;
  function  get_vsrc8(VSRC:Byte):PsrRegSlot;
  function  get_vdst8(VDST:Byte):PsrRegSlot;
  function  get_sbase(SBASE,count:Byte;src:PPsrRegSlot):Boolean;
  function  get_srsrc(SRSRC,count:Byte;src:PPsrRegSlot):Boolean;
  //
  function  get_snapshot:TsrRegsSnapshot;
  //
  function  fetch_ssrc8(SOFFSET:Word;rtype:TsrDataType):TsrRegNode;
  function  fetch_ssrc9(SSRC:Word;rtype:TsrDataType):TsrRegNode;
  function  fetch_ssrc9_pair(SSRC:Word;src:PPsrRegNode;rtype:TsrDataType):Boolean;
  function  fetch_ssrc9_64(SSRC:Word;rtype:TsrDataType):TsrRegNode;
  function  fetch_vsrc8(VSRC:Word;rtype:TsrDataType):TsrRegNode;
  function  fetch_vdst8(VDST:Word;rtype:TsrDataType):TsrRegNode;
  function  fetch_vdst8_64(VDST:Word;rtype:TsrDataType):TsrRegNode;
  //
  procedure OpCmpV(OpId:DWORD;dst0,dst1:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpCmpClass(dst0,dst1:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpCmpS(OpId:DWORD;dst:PsrRegSlot;src0,src1:TsrRegNode);
  procedure OpConvFloatToHalf2(dst:PsrRegSlot;src0,src1:TsrRegNode);
  //
  function  AddInput(dst:PsrRegSlot;rtype:TsrDataType;itype:TpsslInputType;id:Byte=0):TsrRegNode;
  function  AddInstance(dst:PsrRegSlot;id:Byte;step_rate:DWORD):TsrRegNode;
  function  AddAncillary(dst:PsrRegSlot):TsrRegNode;
  function  AddVertLayout(pLayout:TsrDataLayout;rtype:TsrDataType):TsrRegNode;
  function  AddFragLayout(itype:TpsslInputType;rtype:TsrDataType;location:DWORD):TsrRegNode;
  function  FetchLoad (pChain:TsrChain;rtype:TsrDataType):TsrRegNode;
  Procedure FetchStore(pChain:TsrChain;src:TsrRegNode);
  function  FetchAtomic(pChain:TsrChain;OpId:DWORD;dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
  procedure AddUserdata(dst:PsrRegSlot;offset_dw:Byte);
  function  FetchChain(grp:TsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrRegNode;
  function  MakeChain(pSlot:PsrRegSlot;grp:TsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrRegNode;
  Procedure AddVecInput(dst:PsrRegSlot;vtype,rtype:TsrDataType;itype:TpsslInputType;id:Byte);
  function  AddPositionsInput(count:Byte):TsrVariable;
  function  AddParametersInput(id,count:Byte):TsrVariable;
  function  FetchUniformSimple(src:TsrDataLayout;pType:TsrType;GLC,SLC:Boolean):TsrNode;
  function  FetchImage(src:TsrDataLayout;info:TsrImageInfo):TsrNode;
  function  FetchImageArray(src:TsrDataLayout;info:TsrImageInfo;array_count:DWORD):TsrNode;
  function  FetchImageRuntimeArray(src:TsrDataLayout;info:TsrImageInfo):TsrNode;
  function  FetchSampler(src:TsrDataLayout):TsrNode;
  function  FetchOutput(etype:TpsslExportType;rtype:TsrDataType):TsrOutput;
 end;

function  GetInputRegNode(node:TsrRegNode):TsrInput;

implementation

function GetInputRegNode(node:TsrRegNode):TsrInput;
var
 pSource:TsrNode;
begin
 pSource:=GetSourceRegNode(node);
 Result:=pSource.specialize AsType<ntInput>;
end;

//

Function GetRegPairOp(reg:TsrRegNode):TSpirvOp;
var
 pair:TsrRegPair;
begin
 Result:=nil;

 pair:=RegDown(reg).pWriter.specialize AsType<ntRegPair>;
 if (pair=nil) then Exit;

 Result:=pair.pWriter.specialize AsType<ntOp>;
end;

Function GetRegConst(reg:TsrRegNode):TsrConst;
begin
 Result:=RegDown(reg).pWriter.specialize AsType<ntConst>;
end;

Function GetRegPairConst(reg:TsrRegNode):TsrConst;
var
 pair:TsrRegPair;
begin
 Result:=nil;

 pair:=RegDown(reg).pWriter.specialize AsType<ntRegPair>;
 if (pair=nil) then Exit;

 Result:=pair.pWriter.specialize AsType<ntConst>;
end;

function TEmitFetch.GroupingVImm(regs:PPsrRegNode;rtype:TsrResourceType):TsrDataLayout;
var
 vsharp:TVSharpResource4;

 pop_1:array[0..1] of TSpirvOp;
 pop_2:array[0..1] of TSpirvOp;

 imms_p0:array[0..1] of TsrConst;
 imms_p1:array[0..1] of TsrConst;
 imms_p2:TsrConst;
 imms_p3:TsrConst;
begin
 Result:=nil;

 if (rtype<>rtVSharp4) then Exit;

 imms_p2:=GetRegConst(regs[2]);
 imms_p3:=GetRegConst(regs[3]);

 if (imms_p2=nil) then Exit;
 if (imms_p3=nil) then Exit;

 pop_1[0]:=GetRegPairOp(regs[0]);
 pop_1[1]:=GetRegPairOp(regs[1]);

 //S_GETPC_B64   s[0:1]
 //S_ADD_U32     s0, #0x0000011C, s0
 //S_ADDC_U32    s1, 0, s1
 //S_MOV_B32     s2, 20
 //S_MOV_B32     s3, #0x2000C004

 if (pop_1[0]=nil) then Exit;
 if (pop_1[1]=nil) then Exit;

 if (pop_1[0].OpId<>srOpInternal.OpIAddExt) then Exit;
 if (pop_1[1].OpId<>srOpInternal.OpIAddExt) then Exit;

 //S_ADD_U32
 imms_p0[0]:=GetRegConst(pop_1[0].ParamNode(0).AsReg); //const
 imms_p0[1]:=GetRegConst(pop_1[0].ParamNode(1).AsReg); //s0

 //(src0+src1)+SCC
 pop_2[0]:=GetRegPairOp(pop_1[1].ParamNode(0).AsReg); //(src0+src1)
 pop_2[1]:=GetRegPairOp(pop_1[1].ParamNode(1).AsReg); //SCC

 if (pop_1[0]<>pop_2[1]) then Exit;

 //S_ADDC_U32
 imms_p1[0]:=GetRegConst(pop_2[0].ParamNode(0).AsReg);
 imms_p1[1]:=GetRegConst(pop_2[0].ParamNode(1).AsReg);

 vsharp:=Default(TVSharpResource4);

 PQWORD(@vsharp)[0]:=imms_p0[0].AsInt32 + imms_p0[1].AsInt32;

 PDWORD(@vsharp)[1]:=PDWORD(@vsharp)[1] + imms_p1[0].AsInt32 + imms_p1[1].AsInt32;

 PDWORD(@vsharp)[2]:=imms_p2.AsInt32;
 PDWORD(@vsharp)[3]:=imms_p3.AsInt32;

 //print_vsharp(@vsharp);

 Result:=DataLayoutList.FetchImm(@vsharp,rtype);
end;

function TryDisableAnisoLod0(regs:PPsrRegNode;rtype:TsrResourceType):Boolean;
var
 reg:TsrRegNode;
 pSelect:TSpirvOp;
 cond:TsrRegNode;
 pCmp:TSpirvOp;
 pCmp_param0:TsrRegNode;
 pCmp_param1:TsrRegNode;
 pImm:TsrConst;
 pBitField:TSpirvOp;
 pBitwiseAnd:TSpirvOp;
begin
 Result:=False;
 if (rtype<>rtSSharp4) then Exit;

 reg:=RegDown(regs[0]);
 pSelect:=reg.pWriter.specialize AsType<ntOp>;

 if (pSelect=nil) then Exit;
 if (pSelect.OpId<>Op.OpSelect) then Exit;

 cond:=RegDown(pSelect.ParamNode(0).AsReg);
 if (cond=nil) then Exit;

 pCmp:=cond.pWriter.specialize AsType<ntOp>;
 if (pCmp=nil) then Exit;
 if (pCmp.OpId<>Op.OpIEqual) then Exit;

 pCmp_param0:=RegDown(pCmp.ParamNode(0).AsReg);
 pCmp_param1:=RegDown(pCmp.ParamNode(1).AsReg);

 if (pCmp_param0=nil) then Exit;
 if (pCmp_param1=nil) then Exit;

 reg:=nil;
 if pCmp_param0.pWriter.IsType(ntConst) then
 begin
  pImm:=pCmp_param0.pWriter.specialize AsType<ntConst>;
  if (pImm.AsUint32=0) then
  begin
   reg:=pCmp_param1;
  end;
 end;

 if (reg=nil) then
 if pCmp_param1.pWriter.IsType(ntConst) then
 begin
  pImm:=pCmp_param1.pWriter.specialize AsType<ntConst>;
  if (pImm.AsUint32=0) then
  begin
   reg:=pCmp_param0;
  end;
 end;

 if (reg=nil) then Exit;

 reg:=RegDown(reg);

 pBitField:=reg.pWriter.specialize AsType<ntOp>;

 if (pBitField.OpId<>Op.OpBitFieldUExtract) then Exit;

 pImm:=RegDown(pBitField.ParamNode(1).AsReg).pWriter.specialize AsType<ntConst>;
 if (pImm=nil) then Exit;
 if (pImm.AsUint32<>12) then Exit;

 pImm:=RegDown(pBitField.ParamNode(2).AsReg).pWriter.specialize AsType<ntConst>;
 if (pImm=nil) then Exit;
 if (pImm.AsUint32<>8) then Exit;

 reg:=RegDown(pSelect.ParamNode(1).AsReg);
 if (reg=nil) then Exit;

 pBitwiseAnd:=reg.pWriter.specialize AsType<ntOp>;

 if (pBitwiseAnd=nil) then Exit;
 if (pBitwiseAnd.OpId<>Op.OpBitwiseAnd) then Exit;

 pImm:=RegDown(pBitwiseAnd.ParamNode(0).AsReg).pWriter.specialize AsType<ntConst>;

 if (pImm<>nil) then
 begin
  if (pImm.AsUint32<>$FFFFF1FF) then Exit;
 end else
 begin
  pImm:=RegDown(pBitwiseAnd.ParamNode(1).AsReg).pWriter.specialize AsType<ntConst>;
  if (pImm=nil) then Exit;
  if (pImm.AsUint32<>$FFFFF1FF) then Exit;
 end;

 //final
 reg:=RegDown(pSelect.ParamNode(2).AsReg);
 if (reg=nil) then Exit;

 regs[0]:=reg;

 Result:=True;
end;

function TEmitFetch.TryShortVSharp(regs:PPsrRegNode;rtype:TsrResourceType):TsrDataLayout;
var
 chain:TsrChains;
 reg:TsrRegNode;
 pImm:TsrConst;
 V2,V3:DWORD;
begin
 Result:=nil;
 if (rtype<>rtVSharp4) then Exit;

 reg:=RegDown(regs[2]);

 pImm:=reg.AsConst;
 if (pImm=nil) then Exit;

 V2:=pImm.AsUint32;

 reg:=RegDown(regs[3]);

 pImm:=reg.AsConst;
 if (pImm=nil) then Exit;

 V3:=pImm.AsUint32;

 chain:=Default(TsrChains);
 chain[0]:=GetChainRegNode(regs[0]);
 chain[1]:=GetChainRegNode(regs[1]);

 Result:=DataLayoutList.Grouping(chain,rtVSharp2);

 if (Result<>nil) then
 begin
  Result.FData[2]:=V2;
  Result.FData[3]:=V3;
 end;
end;

function TEmitFetch.GroupingSharp(src:PPsrRegSlot;rtype:TsrResourceType):TsrDataLayout;
type
 TsrRegs=array[0..7] of TsrRegNode;
var
 regs:TsrRegs;
 chain:TsrChains;
 i,n:Byte;
begin
 Result:=nil;
 chain:=Default(TsrChains);
 regs :=Default(TsrRegs);
 n:=GetResourceSizeDw(rtype);

 For i:=0 to n-1 do
 begin
  regs[i]:=RegDown(src[i]^.current);
 end;

 Result:=GroupingVImm(@regs,rtype);
 if (Result<>nil) then Exit;

 Result:=TryShortVSharp(@regs,rtype);
 if (Result<>nil) then Exit;

 //TODO: mark "disable_aniso"
 TryDisableAnisoLod0(@regs,rtype);

 For i:=0 to n-1 do
 begin
  chain[i]:=GetChainRegNode(regs[i]);
 end;

 Result:=DataLayoutList.Grouping(chain,rtype);
end;

//

function TEmitFetch.get_sdst7(SDST:Byte):PsrRegSlot;
begin
 Result:=RegsStory.get_sdst7(SDST);
end;

function TEmitFetch.get_sdst7_pair(SDST:Byte;dst:PPsrRegSlot):Boolean;
begin
 Result:=RegsStory.get_sdst7_pair(SDST,dst);
end;

function TEmitFetch.get_sdst8(SSRC:Byte):PsrRegSlot;
begin
 Result:=RegsStory.get_sdst8(SSRC);
end;

function TEmitFetch.get_ssrc8(SSRC:Byte):PsrRegSlot;
begin
 Result:=RegsStory.get_ssrc8(SSRC);
end;

function TEmitFetch.get_ssrc9(SSRC:Word):PsrRegSlot;
begin
 Result:=RegsStory.get_ssrc9(SSRC);
end;

function TEmitFetch.get_ssrc9_pair(SSRC:Word;src:PPsrRegSlot):Boolean;
begin
 Result:=RegsStory.get_ssrc9_pair(SSRC,src);
end;

function TEmitFetch.get_vsrc8(VSRC:Byte):PsrRegSlot;
begin
 Result:=RegsStory.get_vsrc8(VSRC);
end;

function TEmitFetch.get_vdst8(VDST:Byte):PsrRegSlot;
begin
 Result:=RegsStory.get_vdst8(VDST);
end;

function TEmitFetch.get_sbase(SBASE,count:Byte;src:PPsrRegSlot):Boolean;
begin
 Result:=RegsStory.get_sbase(SBASE,count,src);
end;

function TEmitFetch.get_srsrc(SRSRC,count:Byte;src:PPsrRegSlot):Boolean;
begin
 Result:=RegsStory.get_srsrc(SRSRC,count,src);
end;

//

function TEmitFetch.get_snapshot:TsrRegsSnapshot;
begin
 Result:=RegsStory.get_snapshot;
end;

//

function TEmitFetch.fetch_ssrc8(SOFFSET:Word;rtype:TsrDataType):TsrRegNode;
Var
 src:PsrRegSlot;
 pConst:TsrConst;
begin
 if is_const_ssrc8(SOFFSET) then
 begin
  pConst:=ConstList.Fetch_ssrc8_const(SOFFSET,0,rtype);
  Result:=NewImm(pConst);
 end else
 begin
  src:=RegsStory.get_ssrc8(SOFFSET);
  Assert(src^.Category<>cVectorArray,'TODO:fetch_ssrc8 cVectorArray');
  Result:=MakeRead(src,rtype);
 end;
 Assert(Result<>nil,'fetch_ssrc8');
end;

function TEmitFetch.fetch_ssrc9(SSRC:Word;rtype:TsrDataType):TsrRegNode;
Var
 src:PsrRegSlot;
 pConst:TsrConst;
begin
 if is_const_ssrc9(SSRC) then
 begin
  pConst:=ConstList.Fetch_ssrc9_const(SSRC,FSPI.INLINE32,rtype);
  Result:=NewImm(pConst);
 end else
 begin
  src:=RegsStory.get_ssrc9(SSRC);
  Assert(src^.Category<>cVectorArray,'TODO:fetch_ssrc9 cVectorArray');
  Result:=MakeRead(src,rtype);
 end;
 Assert(Result<>nil,'fetch_ssrc9');
end;

function TEmitFetch.fetch_ssrc9_pair(SSRC:Word;src:PPsrRegNode;rtype:TsrDataType):Boolean;
Var
 pSlot:array[0..1] of PsrRegSlot;
begin
 if is_const_ssrc9(SSRC) then
 begin
  src[0]:=NewImm(ConstList.Fetch_ssrc9_const(SSRC,FSPI.INLINE32,rtype));
  src[1]:=NewImm(ConstList.Fetch(rtype,0));
  Result:=True;
 end else
 begin
  pSlot[0]:=nil;
  pSlot[1]:=nil;
  Result:=RegsStory.get_ssrc9_pair(SSRC,pSlot);
  if not Result then Exit;
  src[0]:=MakeRead(pSlot[0],rtype);
  src[1]:=MakeRead(pSlot[1],rtype);
 end;
end;

function TEmitFetch.fetch_ssrc9_64(SSRC:Word;rtype:TsrDataType):TsrRegNode;
var
 src:array[0..1] of TsrRegNode;
 dst:TsrRegNode;
begin
 if not fetch_ssrc9_pair(SSRC,@src,dtUint32) then Assert(false);

 dst:=NewReg(dtVec2u);
 OpMakeCon(line,dst,@src);

 Result:=BitcastList.FetchRead(rtype,dst);
end;

function TEmitFetch.fetch_vsrc8(VSRC:Word;rtype:TsrDataType):TsrRegNode;
var
 src:PsrRegSlot;
begin
 src:=RegsStory.get_vsrc8(VSRC);
 Assert(src^.Category<>cVectorArray,'TODO:fetch_vsrc8 cVectorArray');
 Result:=MakeRead(src,rtype);
 Assert(Result<>nil,'fetch_vsrc8');
end;

function TEmitFetch.fetch_vdst8(VDST:Word;rtype:TsrDataType):TsrRegNode;
var
 src:PsrRegSlot;
begin
 src:=RegsStory.get_vdst8(VDST);
 Result:=MakeRead(src,rtype);
 Assert(Result<>nil,'fetch_vdst8');
end;

function TEmitFetch.fetch_vdst8_64(VDST:Word;rtype:TsrDataType):TsrRegNode;
var
 src:array[0..1] of TsrRegNode;
begin
 src[0]:=fetch_vdst8(VDST+0,dtUint32);
 src[1]:=fetch_vdst8(VDST+1,dtUint32);

 if (src[0]=nil) or (src[1]=nil) then
 begin
  Assert(False);
 end;

 Result:=fetch64(@src,rtype);
end;

//

procedure TEmitFetch.OpCmpV(OpId:DWORD;dst0,dst1:PsrRegSlot;src0,src1:TsrRegNode);
Var
 tmp:TsrRegNode;
 exc:TsrRegNode;
begin
 tmp:=NewReg(dtBool);

 case OpId of
  Op.OpOrdered  :Assert(false,'TODO:OpOrdered');
  Op.OpUnordered:Assert(false,'TODO:OpUnordered');
  else;
 end;

 //Op.OpOrdered   -> (!isNan(S0) && !isNan(S1)) -> !(isNan(S0) || isNan(S1))
 //Op.OpUnordered -> (isNan(S0) || isNan(S1))

 _Op2(line,OpId,tmp,src0,src1);

 exc:=GetThreadBit(get_exec0,get_exec1,dtBool);

 exc:=OpLogicalAndTo(tmp,exc);

 SetThreadBit(dst0,dst1,exc);
end;

const
 fcSignalingNan     = 1 shl 0;
 fcQuietNan         = 1 shl 1;
 fcNegativeInfinity = 1 shl 2;
 fcNegativeNormal   = 1 shl 3;
 fcNegativeDenorm   = 1 shl 4;
 fcNegativeZero     = 1 shl 5;
 fcPositiveZero     = 1 shl 6;
 fcPositiveDenorm   = 1 shl 7;
 fcPositiveNormal   = 1 shl 8;
 fcPositiveInfinity = 1 shl 9;
 fcAny              = (1 shl 10)-1;

 fcNaN      = fcSignalingNan     or fcQuietNan;
 fcInfinity = fcPositiveInfinity or fcNegativeInfinity;
 fcNegative = fcNegativeInfinity or fcNegativeNormal or fcNegativeDenorm or fcNegativeZero;
 fcPositive = fcPositiveZero     or fcPositiveDenorm or fcPositiveNormal or fcPositiveInfinity;

procedure TEmitFetch.OpCmpClass(dst0,dst1:PsrRegSlot;src0,src1:TsrRegNode);
var
 rsl:TsrRegNode;
 ror:TsrRegNode;
 exc:TsrRegNode;

 msk:TsrConst;
 val:DWORD;

 function _test_group(val,mask:DWORD):Boolean; inline;
 var
  i:DWORD;
 begin
  i:=(val and mask);
  Result:=(i=0) or (i=mask);
 end;

begin

 msk:=src1.AsConst;

 if (msk<>nil) then
 begin

  val:=msk.AsUint32;

  if (val and fcAny)=fcAny then
  begin
   ror:=NewImm_b(True);
  end else
  if _test_group(val,fcNaN     ) or
     _test_group(val,fcInfinity) or
     _test_group(val,fcNegative) or
     _test_group(val,fcPositive) then
  begin
   ror:=nil;

   if (val and fcNaN)=fcNaN then
   begin
    rsl:=NewReg(dtBool);
    _Op1(line,Op.OpIsNan,rsl,src0);
    ror:=rsl;
   end;

   if (val and fcInfinity)=fcInfinity then
   begin
    rsl:=NewReg(dtBool);
    _Op1(line,Op.OpIsInf,rsl,src0);
    //
    if (ror=nil) then
    begin
     ror:=rsl;
    end else
    begin
     ror:=OpLogicalOrTo(ror,rsl);
    end;
    //
   end;

   if (val and fcNegative)=fcNegative then
   begin
    rsl:=NewReg(dtBool);
    _Op2(line,Op.OpFOrdLessThanEqual,rsl,src0,NewImm_s(dtFloat32,-0.0));
    //
    if (ror=nil) then
    begin
     ror:=rsl;
    end else
    begin
     ror:=OpLogicalOrTo(ror,rsl);
    end;
    //
   end;

   if (val and fcPositive)=fcPositive then
   begin
    rsl:=NewReg(dtBool);
    _Op2(line,Op.OpFOrdGreaterThanEqual,rsl,src0,NewImm_s(dtFloat32,+0.0));
    //
    if (ror=nil) then
    begin
     ror:=rsl;
    end else
    begin
     ror:=OpLogicalOrTo(ror,rsl);
    end;
    //
   end;

   if (ror=nil) then
   begin
    ror:=NewImm_b(False);
   end;

  end else
  begin
   Assert(false,'TODO: Unhandled mask V_CMP_CLASS_32:0x'+HexStr(val,2));
  end;

 end else
 begin
  Assert(false,'TODO: Non const V_CMP_CLASS_32');
 end;

 exc:=GetThreadBit(get_exec0,get_exec1,dtBool);

 exc:=OpLogicalAndTo(ror,exc);

 SetThreadBit(dst0,dst1,exc);
end;

procedure TEmitFetch.OpCmpS(OpId:DWORD;dst:PsrRegSlot;src0,src1:TsrRegNode);
begin
 Op2(OpId,dtBool,dst,src0,src1);
end;

procedure TEmitFetch.OpConvFloatToHalf2(dst:PsrRegSlot;src0,src1:TsrRegNode);
var
 rsl:array[0..1] of TsrRegNode;
begin
 rsl[0]:=OpFToF(src0,dtHalf16);
 rsl[1]:=OpFToF(src1,dtHalf16);

 dst^.New(dtVec2h);

 OpMakeCon(line,dst^.current,@rsl);
end;

//

function TEmitFetch.AddInput(dst:PsrRegSlot;rtype:TsrDataType;itype:TpsslInputType;id:Byte=0):TsrRegNode;
var
 i:TsrInput;
 v:TsrVariable;
 r:TsrRegNode;
begin
 i:=InputList.Fetch(rtype,itype,id);

 v:=i.pVar;
 r:=i.pReg;

 if (r=nil) then
 begin
  r:=dst^.New(rtype);
  i.pReg :=r;
  OpLoad(init_line,r,v); //one in any scope
 end else
 if (dst^.current<>r) then
 begin
  MakeCopy(dst,r);
 end;

 Result:=r;
end;

function TEmitFetch.AddInstance(dst:PsrRegSlot;id:Byte;step_rate:DWORD):TsrRegNode;
var
 src:TsrRegNode;
begin
 if (step_rate>1) then
 begin
  src:=AddInput(@RegsStory.FUnattach,dtUInt32,itVInstance,id);
  src:=OpIDivTo(src,step_rate);

  MakeCopy(dst,src);
 end else
 begin
  src:=AddInput(dst,dtUInt32,itVInstance,id);
 end;

 Result:=src;
end;

function TEmitFetch.AddAncillary(dst:PsrRegSlot):TsrRegNode;
var
 src:array[0..2] of TsrRegNode;
begin
 src[0]:=NewImm_q(dtUInt32,0); //vPrimType
 src[1]:=AddInput(@RegsStory.FUnattach,dtUInt32,itSampleId);
 src[2]:=AddInput(@RegsStory.FUnattach,dtUInt32,itLayer);

 OpPackAnc(dst,src[0],src[1],src[2]);

 //Render target array index[26:16], BuiltIn.Layer
 //Iterated sample number[11:8],     BuiltIn.SampleId
 //Primitive type[1:0]               0..3

 //IL_REGTYPE_PRIMTYPE vPrimType
 //The first component  has sign bit = 1, this is a point. Values in other bits are undefined.
 //The second component has sign bit = 1, this is a line.  Values in other bits are undefined.

 //PID       ISID      RTAI
 //01|234567|89AB|CDEF|0123456789A|
 //00|000000|    |0000|           |

 Result:=dst^.current;
end;

function TEmitFetch.AddVertLayout(pLayout:TsrDataLayout;rtype:TsrDataType):TsrRegNode;
var
 i:TsrVertLayout;
 v:TsrVariable;
 dst:TsrRegNode;
begin
 i:=VertLayoutList.Fetch(pLayout,rtype);
 v:=i.pVar;
 dst:=i.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(rtype);
  i.pReg:=dst;
  OpLoad(init_line,dst,v); //one in any scope
 end;
 Result:=dst;
end;

function TEmitFetch.AddFragLayout(itype:TpsslInputType;rtype:TsrDataType;location:DWORD):TsrRegNode;
var
 i:TsrFragLayout;
 v:TsrVariable;
 dst:TsrRegNode;
begin
 i:=FragLayoutList.Fetch(itype,location,rtype);
 v:=i.pVar;
 dst:=i.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(rtype);
  i.pReg:=dst;
  OpLoad(init_line,dst,v); //one in any scope
 end;
 Result:=dst;
end;

function TEmitFetch.FetchLoad(pChain:TsrChain;rtype:TsrDataType):TsrRegNode;
begin
 Result:=nil;
 if (pChain=nil) then Exit;

 Result:=NewReg(rtype);

 pChain.FetchLoad(line,Result);
end;

Procedure TEmitFetch.FetchStore(pChain:TsrChain;src:TsrRegNode);
begin
 if (pChain=nil) or (src=nil) then Exit;

 pChain.FetchStore(line,src);
end;

function TEmitFetch.FetchAtomic(pChain:TsrChain;OpId:DWORD;dtype:TsrDataType;src:TsrRegNode):TsrRegNode;
var
 pLine:TSpirvOp;
 iScope,iMemorySemantics:Integer;
begin

 if pChain.Parent.IsLocalDataShare then
 begin
  if (FExecutionModel=ExecutionModel.GLCompute) then
  begin
   //Workgroup
   iScope:=Scope.Workgroup;
   iMemorySemantics:=MemorySemantics.AcquireRelease or MemorySemantics.WorkgroupMemory;
  end else
  begin
   Writeln(stderr,'AtomicOP on Private memory?');
   //private
   iScope:=Scope.Invocation;
   iMemorySemantics:=MemorySemantics.None;
  end;
 end else
 begin
  //Storage/uniform
  iScope:=Scope.Invocation;
  iMemorySemantics:=MemorySemantics.None;
 end;

 pLine:=AddSpirvOp(OpId);
 pLine.pDst :=NewReg(dtype);
 pLine.pType:=TypeList.Fetch(dtype);

 pLine.AddParam(pChain);   //<-First
 pChain.AddLine(pLine);    //<-back link

 //scope
 pLine.AddParam(NewImm_i(dtInt32,iScope));

 //MemorySemantics
 pLine.AddParam(NewImm_i(dtInt32,iMemorySemantics));

 //val
 pLine.AddParam(src);

 Result:=pLine.pDst;
end;

procedure TEmitFetch.AddUserdata(dst:PsrRegSlot;offset_dw:Byte);
var
 lvl_0:TsrChainLvl_0;
 pLayout:TsrDataLayout;
 pChain:TsrChain;
 pReg:TsrRegNode;
begin
 pLayout:=DataLayoutList.pRoot;
 lvl_0.offset:=offset_dw*4;
 lvl_0.size  :=4;
 pChain:=pLayout.Fetch(@lvl_0,nil);
 pReg:=FetchLoad(pChain,dtUnknow);
 MakeCopy(dst,pReg);
end;

function TEmitFetch.FetchChain(grp:TsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrRegNode;
var
 pChain:TsrChain;
begin
 pChain:=grp.Fetch(lvl_0,lvl_1,cflags);
 Result:=FetchLoad(pChain,dtUnknow);
end;

function TEmitFetch.MakeChain(pSlot:PsrRegSlot;grp:TsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1;cflags:Byte=0):TsrRegNode;
var
 pChain:TsrChain;
 pReg:TsrRegNode;
begin
 pChain:=grp.Fetch(lvl_0,lvl_1,cflags);
 pReg:=FetchLoad(pChain,dtUnknow);
 MakeCopy(pSlot,pReg);
 Result:=pSlot^.current;
end;

Procedure TEmitFetch.AddVecInput(dst:PsrRegSlot;vtype,rtype:TsrDataType;itype:TpsslInputType;id:Byte);
var
 rsl:TsrRegNode;
begin
 rsl:=AddInput(@RegsStory.FUnattach,vtype,itype,0);
 OpExtract(init_line,dst^.New(rtype),rsl,id);
end;

function TEmitFetch.AddPositionsInput(count:Byte):TsrVariable;
var
 i:TsrInput;
 t:TsrType;
begin
 t:=TypeList.FetchArray(TypeList.Fetch(dtVec4f),count);

 i:=InputList.Fetch(t,itPositions,0);

 Result:=i.pVar;
end;

function TEmitFetch.AddParametersInput(id,count:Byte):TsrVariable;
var
 i:TsrInput;
 t:TsrType;
begin
 t:=TypeList.FetchArray(TypeList.Fetch(dtVec4f),count);

 i:=InputList.Fetch(t,itParameters,id);

 Result:=i.pVar;
end;

////

function TEmitFetch.FetchUniformSimple(src:TsrDataLayout;pType:TsrType;GLC,SLC:Boolean):TsrNode;
var
 u:TsrUniform;
 v:TsrVariable;
 r:TsrRegUniform;
begin
 u:=UniformList.Fetch(src,pType);
 //
 if (GLC) then
 begin
  u.Flags.Coherent:=True;
 end;
 //
 if (SLC) then
 begin
  u.Flags.Volatile:=True;
 end;
 //
 v:=u.pVar;
 r:=u.pReg;
 if (r.pLine=nil) then
 begin
  OpLoad(init_line,pType,r,v); //one in any scope
 end;
 Result:=r;
end;

function TEmitFetch.FetchImage(src:TsrDataLayout;info:TsrImageInfo):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(info.dtype);
 pType:=TypeList.FetchImage(pType,info.tinfo);
 Result:=FetchUniformSimple(src,pType,info.GLC,info.SLC);
end;

function TEmitFetch.FetchImageArray(src:TsrDataLayout;info:TsrImageInfo;array_count:DWORD):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(info.dtype);
 pType:=TypeList.FetchImage(pType,info.tinfo);
 pType:=TypeList.FetchArray(pType,array_count);
 Result:=UniformList.Fetch(src,pType);
 //
 if (info.GLC) then
 begin
  TsrUniform(Result).Flags.Coherent:=True;
 end;
 //
 if (info.SLC) then
 begin
  TsrUniform(Result).Flags.Volatile:=True;
 end;
end;

function TEmitFetch.FetchImageRuntimeArray(src:TsrDataLayout;info:TsrImageInfo):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(info.dtype);
 pType:=TypeList.FetchImage(pType,info.tinfo);
 pType:=TypeList.FetchRuntimeArray(pType);
 Result:=UniformList.Fetch(src,pType);
 //
 if (info.GLC) then
 begin
  TsrUniform(Result).Flags.Coherent:=True;
 end;
 //
 if (info.SLC) then
 begin
  TsrUniform(Result).Flags.Volatile:=True;
 end;
end;

function TEmitFetch.FetchSampler(src:TsrDataLayout):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(dtTypeSampler);
 Result:=FetchUniformSimple(src,pType,False,False);
end;

function TEmitFetch.FetchOutput(etype:TpsslExportType;rtype:TsrDataType):TsrOutput;
begin
 Result:=OutputList.Fetch(etype,rtype);
end;

end.

