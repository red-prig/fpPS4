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
  function  GroupingVImm(regs:PPsrRegNode;rtype:TsrResourceType):TsrDataLayout;
  function  GroupingSharp(src:PPsrRegSlot;rtype:TsrResourceType):TsrDataLayout;
  //
  procedure PrepTypeSlot(pSlot:PsrRegSlot;rtype:TsrDataType);
  function  MakeRead(pSlot:PsrRegSlot;rtype:TsrDataType):TsrRegNode;
  //
  function  PrepTypeNode(var node:TsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
  function  PrepTypeDst(var node:TsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
  function  PrepTypeParam(node:POpParamNode;rtype:TsrDataType;relax:Boolean=true):Integer;
  //
  function  get_vcc0:PsrRegSlot;
  function  get_vcc1:PsrRegSlot;
  function  get_m0:PsrRegSlot;
  function  get_exec0:PsrRegSlot;
  function  get_exec1:PsrRegSlot;
  function  get_scc:PsrRegSlot;
  //
  function  get_sdst7(SDST:Byte):PsrRegSlot;
  function  get_sdst7_pair(SDST:Byte;dst:PPsrRegSlot):Boolean;
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
  procedure MakeCopy64(dst0,dst1:PsrRegSlot;src:TsrRegNode);
  //
  procedure OpCmpV(OpId:DWORD;dst:PsrRegSlot;src0,src1:TsrRegNode);
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
  function  FetchUniformSimple(src:TsrDataLayout;pType:TsrType;GLC:Byte=0;SLC:Byte=0):TsrNode;
  function  FetchImage(src:TsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo;GLC:Byte=0;SLC:Byte=0):TsrNode;
  function  FetchImageArray(src:TsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo;array_count:DWORD;GLC:Byte=0;SLC:Byte=0):TsrNode;
  function  FetchImageRuntimeArray(src:TsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo;GLC:Byte=0;SLC:Byte=0):TsrNode;
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

 if (pop_1[0].OpId<>srOpUtils.OpIAddExt) then Exit;
 if (pop_1[1].OpId<>srOpUtils.OpIAddExt) then Exit;

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

 For i:=0 to n-1 do
 begin
  chain[i]:=GetChainRegNode(regs[i]);
 end;

 Result:=DataLayoutList.Grouping(chain,rtype);
end;

//

procedure TEmitFetch.PrepTypeSlot(pSlot:PsrRegSlot;rtype:TsrDataType);
begin
 if (pSlot=nil) then Exit;
 if (pSlot^.current=nil) then
 begin
  pSlot^.New(line,rtype); //Unresolve
  Exit;
 end;

 pSlot^.current.PrepType(ord(rtype));
end;

function TEmitFetch.MakeRead(pSlot:PsrRegSlot;rtype:TsrDataType):TsrRegNode;
var
 node:TsrRegNode;
begin
 Result:=nil;
 if (pSlot=nil) then Exit;
 PrepTypeSlot(pSlot,rtype);
 node:=pSlot^.current;
 if (rtype<>dtUnknow) and (not CompareType(node.dtype,rtype)) then
 begin
  Result:=BitcastList.FetchRead(rtype,node);
 end else
 begin
  Result:=node;
 end;
 if (rtype<>dtUnknow) then
 begin
  Result.dweak:=False;
 end;
end;

//

function TEmitFetch.PrepTypeNode(var node:TsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;

 if is_unprep_type(node.dtype,rtype,node.dweak) then
 begin
  node.PrepType(ord(rtype));
  Inc(Result);
 end else
 begin
  Case relax of
   True :relax:=CompareType(node.dtype,rtype);
   False:relax:=(node.dtype=rtype);
  end;
  if not relax then
  begin
   node:=BitcastList.FetchRead(rtype,node);
   Inc(Result);
  end;
 end;
end;

function TEmitFetch.PrepTypeDst(var node:TsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;

 if is_unprep_type(node.dtype,rtype,node.dweak) then
 begin
  node.PrepType(ord(rtype));
  Inc(Result);
 end else
 begin
  Case relax of
   True :relax:=CompareType(node.dtype,rtype);
   False:relax:=(node.dtype=rtype);
  end;
  if not relax then
  begin
   node:=BitcastList.FetchDstr(rtype,node);
   Inc(Result);
  end;
 end;
end;

function TEmitFetch.PrepTypeParam(node:POpParamNode;rtype:TsrDataType;relax:Boolean=true):Integer;
var
 pReg:TsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;
 if node.Value.IsType(ntReg) then
 begin
  pReg:=node.AsReg;
  Result:=PrepTypeNode(pReg,rtype,relax);
  node.Value:=pReg;
 end else
 begin
  node.Value.PrepType(ord(rtype));
 end;
end;

//

function TEmitFetch.get_vcc0:PsrRegSlot;
begin
 Result:=@RegsStory.VCC[0];
end;

function TEmitFetch.get_vcc1:PsrRegSlot;
begin
 Result:=@RegsStory.VCC[1];
end;

function TEmitFetch.get_m0:PsrRegSlot;
begin
 Result:=@RegsStory.M0;
end;

function TEmitFetch.get_exec0:PsrRegSlot;
begin
 Result:=@RegsStory.EXEC[0];
end;

function TEmitFetch.get_exec1:PsrRegSlot;
begin
 Result:=@RegsStory.EXEC[1];
end;

function TEmitFetch.get_scc:PsrRegSlot;
begin
 Result:=@RegsStory.SCC;
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
  Result:=NewReg(pConst);
 end else
 begin
  src:=RegsStory.get_ssrc8(SOFFSET);
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
  Result:=NewReg(pConst);
 end else
 begin
  src:=RegsStory.get_ssrc9(SSRC);
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
  src[0]:=NewReg(ConstList.Fetch_ssrc9_const(SSRC,FSPI.INLINE32,rtype));
  src[1]:=NewReg(ConstList.Fetch(dtUnknow,0));
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
 dst:TsrRegNode;
begin
 src[0]:=fetch_vdst8(VDST+0,dtUint32);
 src[1]:=fetch_vdst8(VDST+1,dtUint32);

 if (src[0]=nil) or (src[1]=nil) then
 begin
  Assert(False);
 end;

 dst:=NewReg(dtVec2u);
 OpMakeCon(line,dst,@src);

 Result:=BitcastList.FetchRead(rtype,dst);
end;

//

procedure TEmitFetch.MakeCopy64(dst0,dst1:PsrRegSlot;src:TsrRegNode);
var
 dst:TsrRegNode;
 node:array[0..1] of TsrRegNode;
begin
 dst:=BitcastList.FetchRead(dtVec2u,src);

 node[0]:=dst0^.New(line,dtUint32);
 node[1]:=dst1^.New(line,dtUint32);

 OpExtract(line,node[0],dst,0);
 OpExtract(line,node[1],dst,1);
end;

//

procedure TEmitFetch.OpCmpV(OpId:DWORD;dst:PsrRegSlot;src0,src1:TsrRegNode);
Var
 tmp:TsrRegNode;
 exc:TsrRegNode;
begin
 tmp:=NewReg(dtBool);

 _Op2(line,OpId,tmp,src0,src1);

 exc:=MakeRead(get_exec0,dtBool);
 OpLogicalAnd(dst,tmp,exc);
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

 dst^.New(line,dtVec2h);

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
  r:=dst^.New(line,rtype);
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
 src[0]:=NewReg_q(dtUInt32,0); //vPrimType
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
 pLine.AddParam(NewReg_i(dtInt32,iScope));

 //MemorySemantics
 pLine.AddParam(NewReg_i(dtInt32,iMemorySemantics));

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
 OpExtract(init_line,dst^.New(line,rtype),rsl,id);
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

function TEmitFetch.FetchUniformSimple(src:TsrDataLayout;pType:TsrType;GLC:Byte=0;SLC:Byte=0):TsrNode;
var
 u:TsrUniform;
 v:TsrVariable;
 r:TsrRegUniform;
begin
 u:=UniformList.Fetch(src,pType);
 //
 if (GLC<>0) then
 begin
  u.Flags.Coherent:=True;
 end;
 //
 if (SLC<>0) then
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

function TEmitFetch.FetchImage(src:TsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo;GLC:Byte=0;SLC:Byte=0):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(dtype);
 pType:=TypeList.FetchImage(pType,info);
 Result:=FetchUniformSimple(src,pType,GLC);
end;

function TEmitFetch.FetchImageArray(src:TsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo;array_count:DWORD;GLC:Byte=0;SLC:Byte=0):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(dtype);
 pType:=TypeList.FetchImage(pType,info);
 pType:=TypeList.FetchArray(pType,array_count);
 Result:=UniformList.Fetch(src,pType);
 //
 if (GLC<>0) then
 begin
  TsrUniform(Result).Flags.Coherent:=True;
 end;
 //
 if (SLC<>0) then
 begin
  TsrUniform(Result).Flags.Volatile:=True;
 end;
end;

function TEmitFetch.FetchImageRuntimeArray(src:TsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo;GLC:Byte=0;SLC:Byte=0):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(dtype);
 pType:=TypeList.FetchImage(pType,info);
 pType:=TypeList.FetchRuntimeArray(pType);
 Result:=UniformList.Fetch(src,pType);
 //
 if (GLC<>0) then
 begin
  TsrUniform(Result).Flags.Coherent:=True;
 end;
 //
 if (SLC<>0) then
 begin
  TsrUniform(Result).Flags.Volatile:=True;
 end;
end;

function TEmitFetch.FetchSampler(src:TsrDataLayout):TsrNode;
var
 pType:TsrType;
begin
 pType:=TypeList.Fetch(dtTypeSampler);
 Result:=FetchUniformSimple(src,pType);
end;

function TEmitFetch.FetchOutput(etype:TpsslExportType;rtype:TsrDataType):TsrOutput;
begin
 Result:=OutputList.Fetch(etype,rtype);
end;

end.

