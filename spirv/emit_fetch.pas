unit emit_fetch;

{$mode objfpc}{$H+}

interface

uses
 ps4_pssl,
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
  function  GroupingSharp(src:PPsrRegSlot;rtype:TsrResourceType):PsrDataLayout;
  //
  procedure PrepTypeSlot(pSlot:PsrRegSlot;rtype:TsrDataType);
  function  MakeRead(pSlot:PsrRegSlot;rtype:TsrDataType):PsrRegNode;
  //
  function  PrepTypeNode(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
  function  PrepTypeDst(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
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
  function  fetch_ssrc8(SOFFSET:Word;rtype:TsrDataType):PsrRegNode;
  function  fetch_ssrc9(SSRC:Word;rtype:TsrDataType):PsrRegNode;
  function  fetch_ssrc9_pair(SSRC:Word;src:PPsrRegNode;rtype:TsrDataType):Boolean;
  function  fetch_vsrc8(VSRC:Word;rtype:TsrDataType):PsrRegNode;
  function  fetch_vdst8(VDST:Word;rtype:TsrDataType):PsrRegNode;
  procedure fetch_vsrc8_vec2h(VSRC:Word;var dst0,dst1:PsrRegNode);
  //
  procedure OpCmpV(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpCmpS(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
  procedure OpConvFloatToHalf2(dst:PsrRegSlot;src0,src1:PsrRegNode);
  //
  function  AddInput(dst:PsrRegSlot;rtype:TsrDataType;itype:TpsslInputType;id:Byte=0):PsrRegNode;
  function  AddInstance(dst:PsrRegSlot;id:Byte;step_rate:DWORD):PsrRegNode;
  function  AddAncillary(dst:PsrRegSlot):PsrRegNode;
  function  AddVertLayout(pLayout:PsrDataLayout;rtype:TsrDataType):PsrRegNode;
  function  AddFragLayout(itype:TpsslInputType;rtype:TsrDataType;location:DWORD):PsrRegNode;
  function  FetchLoad(pChain:PsrChain;rtype:TsrDataType):PsrRegNode;
  Procedure FetchStore(pChain:PsrChain;src:PsrRegNode);
  procedure AddUserdata(dst:PsrRegSlot;offset_dw:Byte);
  function  FetchChain(grp:PsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1):PsrRegNode;
  function  MakeChain(pSlot:PsrRegSlot;grp:PsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1):PsrRegNode;
  Procedure AddVecInput(dst:PsrRegSlot;vtype,rtype:TsrDataType;itype:TpsslInputType;id:Byte);
  function  FetchUniform(src:PsrDataLayout;pType:PsrType):PsrNode;
  function  FetchImage(src:PsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo):PsrNode;
  function  FetchSampler(src:PsrDataLayout):PsrNode;
  function  FetchOutput(etype:TpsslExportType;rtype:TsrDataType):PsrVariable;
 end;

function  GetInputRegNode(node:PsrRegNode):PsrInput;

implementation

function GetInputRegNode(node:PsrRegNode):PsrInput;
var
 pSource:PsrNode;
begin
 pSource:=GetSourceRegNode(node);
 Result:=pSource^.AsType(ntInput);
end;

//

function TEmitFetch.GroupingSharp(src:PPsrRegSlot;rtype:TsrResourceType):PsrDataLayout;
var
 chain:TsrChains;
 i,n:Byte;
begin
 Result:=nil;
 chain:=Default(TsrChains);
 n:=GetResourceSizeDw(rtype);

 For i:=0 to n-1 do
 begin
  chain[i]:=GetChainRegNode(src[i]^.current);
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

 pSlot^.current^.PrepType(ord(rtype));
end;

function TEmitFetch.MakeRead(pSlot:PsrRegSlot;rtype:TsrDataType):PsrRegNode;
var
 node:PsrRegNode;
begin
 Result:=nil;
 if (pSlot=nil) then Exit;
 PrepTypeSlot(pSlot,rtype);
 node:=pSlot^.current;
 if (rtype<>dtUnknow) and (not CompareType(node^.dtype,rtype)) then
 begin
  Result:=BitcastList.FetchRead(rtype,node);
 end else
 begin
  Result:=node;
 end;
 if (rtype<>dtUnknow) then
 begin
  Result^.Weak:=False;
 end;
end;

//

function TEmitFetch.PrepTypeNode(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;

 if is_unprep_type(node^.dtype,rtype,node^.Weak) then
 begin
  node^.PrepType(ord(rtype));
  Inc(Result);
 end else
 begin
  Case relax of
   True :relax:=CompareType(node^.dtype,rtype);
   False:relax:=(node^.dtype=rtype);
  end;
  if not relax then
  begin
   node:=BitcastList.FetchRead(rtype,node);
   Inc(Result);
  end;
 end;
end;

function TEmitFetch.PrepTypeDst(var node:PsrRegNode;rtype:TsrDataType;relax:Boolean=true):Integer;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;

 if is_unprep_type(node^.dtype,rtype,node^.Weak) then
 begin
  node^.PrepType(ord(rtype));
  Inc(Result);
 end else
 begin
  Case relax of
   True :relax:=CompareType(node^.dtype,rtype);
   False:relax:=(node^.dtype=rtype);
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
 pReg:PsrRegNode;
begin
 Result:=0;
 if (node=nil) then Exit;
 if (rtype=dtUnknow) then Exit;
 if node^.Value^.IsType(ntReg) then
 begin
  pReg:=node^.AsReg;
  Result:=PrepTypeNode(pReg,rtype,relax);
  node^.Value:=pReg;
 end else
 begin
  node^.Value^.PrepType(ord(rtype));
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

function TEmitFetch.fetch_ssrc8(SOFFSET:Word;rtype:TsrDataType):PsrRegNode;
Var
 src:PsrRegSlot;
 pConst:PsrConst;
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

function TEmitFetch.fetch_ssrc9(SSRC:Word;rtype:TsrDataType):PsrRegNode;
Var
 src:PsrRegSlot;
 pConst:PsrConst;
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

function TEmitFetch.fetch_vsrc8(VSRC:Word;rtype:TsrDataType):PsrRegNode;
var
 src:PsrRegSlot;
begin
 src:=RegsStory.get_vsrc8(VSRC);
 Result:=MakeRead(src,rtype);
 Assert(Result<>nil,'fetch_vsrc8');
end;

function TEmitFetch.fetch_vdst8(VDST:Word;rtype:TsrDataType):PsrRegNode;
var
 src:PsrRegSlot;
begin
 src:=RegsStory.get_vdst8(VDST);
 Result:=MakeRead(src,rtype);
 Assert(Result<>nil,'fetch_vdst8');
end;

procedure TEmitFetch.fetch_vsrc8_vec2h(VSRC:Word;var dst0,dst1:PsrRegNode);
var
 pSlot:PsrRegSlot;
 dst:PsrRegNode;
begin
 pSlot:=RegsStory.get_vsrc8(VSRC);

 dst:=MakeRead(pSlot,dtVec2h);
 Assert(dst<>nil,'fetch_vsrc8_vec2h');

 dst0:=NewReg(dtHalf16);
 dst1:=NewReg(dtHalf16);;

 OpExtract(line,dst0,dst,0);
 OpExtract(line,dst1,dst,1);
end;

//

procedure TEmitFetch.OpCmpV(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
Var
 tmp:PsrRegNode;
 exc:PsrRegNode;
begin
 tmp:=NewReg(dtBool);

 _Op2(line,OpId,tmp,src0,src1);

 exc:=MakeRead(get_exec0,dtBool);
 OpLogicalAnd(dst,tmp,exc);
end;

procedure TEmitFetch.OpCmpS(OpId:DWORD;dst:PsrRegSlot;src0,src1:PsrRegNode);
begin
 Op2(OpId,dtBool,dst,src0,src1);
end;

procedure TEmitFetch.OpConvFloatToHalf2(dst:PsrRegSlot;src0,src1:PsrRegNode);
var
 rsl:array[0..1] of PsrRegNode;
begin
 rsl[0]:=OpFToF(src0,dtHalf16);
 rsl[1]:=OpFToF(src1,dtHalf16);

 dst^.New(line,dtVec2h);

 OpMakeCon(line,dst^.current,@rsl);
end;

//

function TEmitFetch.AddInput(dst:PsrRegSlot;rtype:TsrDataType;itype:TpsslInputType;id:Byte=0):PsrRegNode;
var
 i:PsrInput;
 v:PsrVariable;
 r:PsrRegNode;
begin
 i:=InputList.Fetch(rtype,itype,id);

 v:=i^.pVar;
 r:=i^.pReg;

 if (r=nil) then
 begin
  r:=dst^.New(line,rtype);
  i^.pReg :=r;
  OpLoad(init_line,r,v); //one in any scope
 end else
 if (dst^.current<>r) then
 begin
  MakeCopy(dst,r);
 end;

 Result:=r;
end;

function TEmitFetch.AddInstance(dst:PsrRegSlot;id:Byte;step_rate:DWORD):PsrRegNode;
var
 src:PsrRegNode;
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

function TEmitFetch.AddAncillary(dst:PsrRegSlot):PsrRegNode;
var
 src:array[0..2] of PsrRegNode;
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

function TEmitFetch.AddVertLayout(pLayout:PsrDataLayout;rtype:TsrDataType):PsrRegNode;
var
 i:PsrVertLayout;
 v:PsrVariable;
 dst:PsrRegNode;
begin
 i:=VertLayoutList.Fetch(pLayout,rtype);
 v:=i^.pVar;
 dst:=i^.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(rtype);
  i^.pReg:=dst;
  OpLoad(init_line,dst,v); //one in any scope
 end;
 Result:=dst;
end;

function TEmitFetch.AddFragLayout(itype:TpsslInputType;rtype:TsrDataType;location:DWORD):PsrRegNode;
var
 i:PsrFragLayout;
 v:PsrVariable;
 dst:PsrRegNode;
begin
 i:=FragLayoutList.Fetch(itype,location,rtype);
 v:=i^.pVar;
 dst:=i^.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(rtype);
  i^.pReg:=dst;
  OpLoad(init_line,dst,v); //one in any scope
 end;
 Result:=dst;
end;

function TEmitFetch.FetchLoad(pChain:PsrChain;rtype:TsrDataType):PsrRegNode;
begin
 Result:=nil;
 if (pChain=nil) then Exit;

 Result:=NewReg(rtype);

 pChain^.FetchLoad(line,Result);
end;

Procedure TEmitFetch.FetchStore(pChain:PsrChain;src:PsrRegNode);
begin
 if (pChain=nil) or (src=nil) then Exit;

 pChain^.FetchStore(line,src);
end;

procedure TEmitFetch.AddUserdata(dst:PsrRegSlot;offset_dw:Byte);
var
 lvl_0:TsrChainLvl_0;
 pLayout:PsrDataLayout;
 pChain:PsrChain;
 pReg:PsrRegNode;
begin
 pLayout:=DataLayoutList.pRoot;
 lvl_0.offset:=offset_dw*4;
 lvl_0.size  :=4;
 pChain:=pLayout^.Fetch(@lvl_0,nil);
 pReg:=FetchLoad(pChain,dtUnknow);
 MakeCopy(dst,pReg);
end;

function TEmitFetch.FetchChain(grp:PsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1):PsrRegNode;
var
 pChain:PsrChain;
begin
 pChain:=grp^.Fetch(lvl_0,lvl_1);
 Result:=FetchLoad(pChain,dtUnknow);
end;

function TEmitFetch.MakeChain(pSlot:PsrRegSlot;grp:PsrDataLayout;lvl_0:PsrChainLvl_0;lvl_1:PsrChainLvl_1):PsrRegNode;
var
 pChain:PsrChain;
 pReg:PsrRegNode;
begin
 pChain:=grp^.Fetch(lvl_0,lvl_1);
 pReg:=FetchLoad(pChain,dtUnknow);
 MakeCopy(pSlot,pReg);
 Result:=pSlot^.current;
end;

Procedure TEmitFetch.AddVecInput(dst:PsrRegSlot;vtype,rtype:TsrDataType;itype:TpsslInputType;id:Byte);
var
 rsl:PsrRegNode;
begin
 rsl:=AddInput(@RegsStory.FUnattach,vtype,itype,0);
 OpExtract(init_line,dst^.New(line,rtype),rsl,id);
end;

////

function TEmitFetch.FetchUniform(src:PsrDataLayout;pType:PsrType):PsrNode;
var
 u:PsrUniform;
 v:PsrVariable;
 r:PsrRegUniform;
begin
 u:=UniformList.Fetch(src,pType);
 v:=u^.pVar;
 r:=u^.pReg;
 if (r^.pLine=nil) then
 begin
  OpLoad(init_line,pType,r,v); //one in any scope
 end;
 Result:=r;
end;

function TEmitFetch.FetchImage(src:PsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo):PsrNode;
var
 pType:PsrType;
begin
 pType:=TypeList.Fetch(dtype);
 pType:=TypeList.FetchImage(pType,info);
 Result:=FetchUniform(src,pType);
end;

function TEmitFetch.FetchSampler(src:PsrDataLayout):PsrNode;
var
 pType:PsrType;
begin
 pType:=TypeList.Fetch(dtTypeSampler);
 Result:=FetchUniform(src,pType);
end;

function TEmitFetch.FetchOutput(etype:TpsslExportType;rtype:TsrDataType):PsrVariable;
var
 o:PsrOutput;
begin
 o:=OutputList.Fetch(etype,rtype);
 Result:=o^.pVar;
end;

end.

