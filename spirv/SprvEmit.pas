unit SprvEmit;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  si_ci_vi_merged_registers,
  ps4_pssl,
  srAllocator,
  srNodes,
  srBitcast,
  srLabel,
  srCFG,
  srParser,
  srTypes,
  srConst,
  srReg,
  srOp,
  srOpUtils,
  srCap,
  srRefId,
  srDecorate,
  srLayout,
  srVertLayout,
  srFragLayout,
  srBuffer,
  srVariable,
  srInput,
  srOutput,
  srUniform,
  spirv;

type
 TLocalSize=packed record
  x,y,z:DWORD;
 end;

 PSprvEmit=^TSprvEmit;
 TSprvEmit=object

  FAllocator:TsrAllocator;

  FCodeHeap:TsrCodeHeap;
  FCursor:TsrCursor;

  FLocalSize:TLocalSize;

  FExecutionModel:Word;

  FRegsStory:TsrRegsStory;
  FVolatileID:SizeUint;

  FInputs:TsrInputList;
  FDataLayouts:TsrDataLayoutList;
  FVertLayouts:TsrVertLayoutList;
  FFragLayouts:TsrFragLayoutList;
  FBuffers:TsrBufferList;

  FOutputs:TsrOutputList;
  FVariables:TsrVariableList;
  FUniforms:TsrUniformList;
  FCacheOps:TsrCacheOpList;

  FConsts:TsrConstList;
  FBitcast:TsrBitcastList;

  FSpirvIdAlloc:TsrRefIdAlloc;
  FSpirvTypes:TsrTypeList;
  FSpirvFuncs:TsrFuncList;
  FSpirvCaps:TsrCapList;

  FDebugInfo:TsrDebugInfoList;
  FDecorates:TsrDecorateList;

  FMain:PSpirvFunc;
  FInitLine:PsrOpBlock;

  FGLSL_std_450:PSpirvOp;

  FHeader:TsrOpBlockSimple;

  FSPI:TSPI;

  FPrintAsm:Boolean;
  FUseVertexInput:Boolean;
  FUseTexelBuffer:Boolean;
  FUseOutput16:Boolean;

  function  Alloc(Size:ptruint):Pointer;

  procedure SetPtr(base:Pointer;bType:TsrBlockType);
  function  Parse(base:Pointer):Byte;
  function  NextParse:Byte;
  function  IsFinalize:Boolean;
  function  FindLabel(Adr:TSrcAdr):PsrLabel;
  function  CheckBlockBeg:Boolean;
  function  CheckBlockEnd:Boolean;
  procedure Finalize;

  function  NewLabelOp:PSpirvOp;
  function  NewMain:PSpirvFunc;

  procedure AddCap(ID:DWORD);

  function  AllocBlockOp:PsrOpBlock;
  function  NewBlockOp(Snap:TsrRegsSnapshot):PsrOpBlock;
  function  InsertBlockOp(pLine:PspirvOp;pChild:PsrOpBlock):PspirvOp;
  Procedure PushBlockOp(pLine:PspirvOp;pOpBlock:PsrOpBlock;pLBlock:PsrCFGBlock);
  function  PopBlockOp:Boolean;

  function  NewSpirvOp(OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(pLine,pNew:PspirvOp):PSpirvOp;
  function  line:PSpirvOp;
  function  init_line:PSpirvOp;
  procedure PostLink(ntype:TsrNodeType;Data:Pointer);
  procedure PostReg(pReg:PsrRegNode);

  function  NewRefId:PsrRefId;
  function  NewVariable:PsrVariable;

  function  AddInput(dst:PsrRegSlot;rtype:TsrDataType;itype:TpsslInputType;id:Byte=0):PsrRegNode;
  function  AllocDescVar(p:Pointer;_ntype:TsrNodeType;rtype:TsrDataType):PsrVariable;
  function  AddVertLayout(pLayout:PsrDataLayout;rtype:TsrDataType):PsrRegNode;
  function  AddFragLayout(itype:TpsslInputType;rtype:TsrDataType;location:DWORD):PsrRegNode;
  function  FetchLoad(pChain:PsrChain;rtype:TsrDataType):PsrRegNode;
  Procedure FetchStore(pChain:PsrChain;src:PsrRegNode);
  procedure AddUserdata(dst:PsrRegSlot;offset_dw:Byte);
  function  FetchChain(grp:PsrDataLayout;offset,size:DWORD;ext:PsrChainExt):PsrRegNode;
  function  MakeChain(pSlot:PsrRegSlot;grp:PsrDataLayout;offset,size:PtrUint;ext:PsrChainExt):PsrRegNode;
  Procedure AddVecInput(dst:PsrRegSlot;vtype,rtype:TsrDataType;itype:TpsslInputType;id:Byte);
  function  FetchUniform(src:PsrDataLayout;pType:PsrType):PsrRegNode;
  function  FetchImage(src:PsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo):PsrRegNode;
  function  FetchSampler(src:PsrDataLayout):PsrRegNode;
  function  FetchOutput(etype:TpsslExportType;rtype:TsrDataType):PsrVariable;

  Procedure Init;
  procedure SetUserData(pData:Pointer);
  Procedure InitVs(RSRC2:TSPI_SHADER_PGM_RSRC2_VS;instance:Byte);
  Procedure InitPs(RSRC2:TSPI_SHADER_PGM_RSRC2_PS;ENA:TSPI_PS_INPUT_ENA);
  Procedure InitCs(RSRC2:TCOMPUTE_PGM_RSRC2;NTX:TCOMPUTE_NUM_THREAD_X;NTY:TCOMPUTE_NUM_THREAD_Y;NTZ:TCOMPUTE_NUM_THREAD_Z);

  Function  NewReg(rtype:TsrDataType):PsrRegNode;
  Function  FetchReg(pConst:PsrConst):PsrRegNode;
  Procedure SetConst(pSlot:PsrRegSlot;pConst:PsrConst);
  Procedure SetConst(pSlot:PsrRegSlot;dtype:TsrDataType;value:QWORD);

  function  fetch_soffset(SOFFSET:Word;rtype:TsrDataType):PsrRegNode;
  function  fetch_ssrc9(SSRC:Word;rtype:TsrDataType):PsrRegNode;
  function  fetch_ssrc9_pair(src:PPsrRegNode;SSRC:Word;rtype:TsrDataType):Boolean;
  function  fetch_vsrc8(VSRC:Word;rtype:TsrDataType):PsrRegNode;
  function  fetch_vdst8(VDST:Word;rtype:TsrDataType):PsrRegNode;
  procedure fetch_vsrc8_vec2h(VSRC:Word;var dst0,dst1:PsrRegNode);

  procedure _MakeCopy(dst:PsrRegSlot;src:PsrRegNode);
  procedure MakeCopy(dst:PsrRegSlot;src:PsrRegNode);

  function  GroupingSharp(src:PPsrRegSlot;rtype:TsrResourceType):PsrDataLayout;

  procedure PrepTypeSlot(pSlot:PsrRegSlot;rtype:TsrDataType);

  function  MakeRead(pSlot:PsrRegSlot;rtype:TsrDataType):PsrRegNode;

  procedure LoadPrepType(node:PsrRegNode;p:PspirvOp;rtype:TsrDataType);
  procedure RegPrepType(node:PsrRegNode;rtype:TsrDataType);

  procedure _emit_spi;
  procedure _emit_DS;
  procedure _emit_SOPK;
 end;

function  RegDown(node:PsrRegNode):PsrRegNode;
function  RegDownSlot(node:PsrRegNode):PsrRegNode;

Procedure mark_read(const node:TOpParamSingle);
Procedure mark_write(const node:TOpParamSingle);

function  GetChainRegNode(node:PsrRegNode):PsrChain;
function  GetSourceRegNode(node:PsrRegNode):TOpParamSingle;
function  GetInputRegNode(node:PsrRegNode):PsrInput;

implementation

uses
 srVolatile,
 emit_op,
 emit_SOP1,
 emit_SOP2,
 emit_SOPC,
 emit_SOPP,
 emit_VOP1,
 emit_VOP2,
 emit_VOP3,
 emit_VOPC,
 emit_MUBUF,
 emit_MTBUF,
 emit_EXP,
 emit_VINTRP,
 emit_SMRD,
 emit_MIMG,
 emit_DS;

function TSprvEmit.Alloc(Size:ptruint):Pointer;
begin
 Result:=FAllocator.Alloc(Size);
end;

procedure TSprvEmit.SetPtr(base:Pointer;bType:TsrBlockType);
begin
 if (FCursor.Adr.get_pc=base) then Exit;
 FCursor:=FCodeHeap.FetchByPtr(base,bType);
end;

function TSprvEmit.Parse(base:Pointer):Byte;
begin
 SetPtr(base,btMain);
 FMain^.FTop.SetCFGBlock(FCursor.pBlock);
 While (CheckBlockBeg) do;
 repeat
  Result:=NextParse;
  Case Result of
   0:;
   1:Break;
   else
     Break;
  end;
 until false;
end;

function TSprvEmit.NextParse:Byte;
var
 FLevel:DWORD;
begin
 if (FCursor.pCode=nil)  then Exit(2);
 if (FCursor.pBlock=nil) then Exit(3);
 if (FMain=nil)          then Exit(4);
 if (FMain^.pBlock=nil)  then Exit(5);


 if FPrintAsm then
 begin
  Write(HexStr(FCursor.OFFSET_DW*4,4));
  FLevel:=0;
  if (FMain<>nil) then
  if (FMain^.pBlock<>nil) then
  begin
   FLevel:=FMain^.pBlock^.FLevel;
  end;
  Write(Space(FLevel+1));
 end;

 Result:=FCursor.Next(FSPI);
 if (Result>1) then Exit;

 if FPrintAsm then
 begin
  print_spi(FSPI);
 end;

 _emit_spi;

 While (CheckBlockBeg) do;
 While (CheckBlockEnd) do;

 Result:=0;
 if IsFinalize then
 begin
  Finalize;
  Result:=1;
 end;

end;

function TSprvEmit.IsFinalize:Boolean;
begin
 Result:=False;
 if (FMain^.pBlock^.pParent=nil) then
  if FCursor.pBlock^.IsEndOf(FCursor.Adr) then
  begin
   Result:=True;
  end;
end;

function TSprvEmit.FindLabel(Adr:TSrcAdr):PsrLabel;
begin
 Result:=nil;
 if (FCursor.pCode=nil) then Exit;
 Result:=FCursor.pCode^.FindLabel(Adr);
end;

function TSprvEmit.CheckBlockBeg:Boolean;
var
 pLBlock:PsrCFGBlock;
 pOpLabel:array[0..3] of PspirvOp;
 pOpBlock:PsrOpBlock;
 pOpChild:PsrOpBlock;
 adr:TSrcAdr;
 Info:TsrBlockInfo;
begin
 Result:=False;
 adr:=FCursor.Adr;
 if (FindLabel(adr)=nil) then Exit;

 pLBlock:=FCursor.pBlock^.FindBlock(adr);
 if (pLBlock<>nil) then
  Case pLBlock^.bType of
   btLoop:
    begin
     TEmitVolatile(Self).make_copy_all;

     Info:=Default(TsrBlockInfo);
     Info.b_adr:=pLBlock^.pBLabel^.Adr;
     Info.e_adr:=pLBlock^.pELabel^.Adr;
     Info.bType:=btLoop;

     pOpLabel[0]:=NewLabelOp; //continue
     pOpLabel[1]:=NewLabelOp; //end
     pOpLabel[2]:=NewLabelOp; //cond
     pOpLabel[3]:=NewLabelOp; //start

     pOpLabel[0]^.Adr:=Info.b_adr;
     pOpLabel[1]^.Adr:=Info.e_adr;
     pOpLabel[2]^.Adr:=Info.e_adr;
     pOpLabel[3]^.Adr:=Info.b_adr;

     pOpBlock:=NewBlockOp(FRegsStory.get_snapshot);
     pOpBlock^.SetLabels(pOpLabel[0],pOpLabel[1],pOpLabel[2]);
     pOpBlock^.SetInfo(Info);

     PushBlockOp(line,pOpBlock,pLBlock);

     TEmitOp(Self).emit_OpBranch(line,pOpLabel[0]);
     AddSpirvOp(line,pOpLabel[0]);    //continue loop

     TEmitOp(Self).emit_OpLoopMerge(line,pOpLabel[1],pOpLabel[2]);
     TEmitOp(Self).emit_OpBranch(line,pOpLabel[3]);
     AddSpirvOp(line,pOpLabel[3]);

     //down group
     Info.bType:=btOther;
     pOpChild:=AllocBlockOp;
     pOpChild^.SetInfo(Info);
     PushBlockOp(line,pOpChild,nil);
    end;
   btAdr: //skip
    begin
     adr:=pLBlock^.pELabel^.Adr;
     FCursor.Adr:=adr;
    end;
   else;
  end;

end;

function TSprvEmit.CheckBlockEnd:Boolean;
begin
 Result:=False;
 if (FMain=nil) then Exit;
 if (FMain^.pBlock=nil) then Exit;

 if (FMain^.pBlock^.pParent<>nil) and
    FMain^.pBlock^.IsEndOf(FCursor.Adr) then
 begin
  Result:=PopBlockOp;
 end;
end;

procedure TSprvEmit.Finalize;
begin
 if (FMain=nil) then Exit;

 if (FMain^.pBlock<>nil) then
  While (FMain^.pBlock^.pParent<>nil) do
  begin
   PopBlockOp;
  end;

 AddSpirvOp(Op.OpFunctionEnd);
end;

//%void = OpTypeVoid;
//%f_void = OpTypeFunction %void;
//%f_main = OpFunction %void None %f_void;
//%l_main = OpLabel;

function TSprvEmit.NewLabelOp:PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=NewSpirvOp(Op.OpLabel);
 node^.dst.SetParam(ntRefId,NewRefId);
 Result:=node;
end;

function TSprvEmit.NewMain:PSpirvFunc;
var
 tvoid,tftype:PsrType;
 node:PspirvOp;

begin
 FMain:=Alloc(SizeOf(TSpirvFunc));
 FMain^.Init('main',@Alloc);
 FMain^.mark_read;

 //OpTypeVoid
 tvoid:=FSpirvTypes.Fetch(dtTypeVoid);
 //OpTypeFunction
 tftype:=FSpirvTypes.FetchFunction(tvoid);

 //OpFunction
 node:=@FMain^.FTop.dummy;
 node^.OpId:=Op.OpFunction;
 node^.dst_type:=tvoid;
 node^.dst.SetParam(ntFunc,FMain); //self
 node^.AddLiteral(FunctionControl.None,'None');
 node^.AddParam(ntType,tftype);

 //OpLabel
 node:=NewLabelOp;
 FMain^.AddSpirvOp(node);

 Result:=FMain;
end;

procedure TSprvEmit.AddCap(ID:DWORD);
begin
 FSpirvCaps.Add(ID);
end;

function TSprvEmit.AllocBlockOp:PsrOpBlock;
begin
 Result:=Alloc(SizeOf(TsrOpBlock));
 Result^.Alloc:=@Alloc;
 Result^.dummy.pParent:=Result;
 Result^.Push_head(@Result^.dummy);
end;

function TSprvEmit.NewBlockOp(Snap:TsrRegsSnapshot):PsrOpBlock;
begin
 Result:=AllocBlockOp;
 Result^.Regs.pSnap :=Alloc(SizeOf(TsrRegsSnapshot));
 Result^.Regs.pSnap^:=Snap;
 Result^.Regs.FVolMark:=vmNone;
 Result^.Cond.FUseCont:=false;
end;

function TSprvEmit.InsertBlockOp(pLine:PspirvOp;pChild:PsrOpBlock):PspirvOp;
begin
 pLine:=AddSpirvOp(pLine,OpBlock);
 pLine^.dst.SetParam(ntBlock,pChild);
 pChild^.pParent:=pLine^.pParent;
 pChild^.pUpLine:=pLine;
 pChild^.FLevel :=pLine^.pParent^.FLevel+1;
 Result:=pLine;
end;

Procedure TSprvEmit.PushBlockOp(pLine:PspirvOp;pOpBlock:PsrOpBlock;pLBlock:PsrCFGBlock);
var
 node:PSpirvOp;
begin
 pOpBlock^.FCursor:=FCursor; //prev

 node:=AddSpirvOp(pLine,OpBlock);
 node^.dst.SetParam(ntBlock,pOpBlock);
 pOpBlock^.pUpLine:=node;
 FMain^.PushBlock(pOpBlock);

 if (pLBlock<>nil) then
 begin
  FCursor.pBlock:=pLBlock; //push
 end;
end;

procedure UpdateVolMark(pBlock:PsrOpBlock);
var
 pLine:PspirvOp;
 pChild:PsrOpBlock;
begin
 if (pBlock=nil) then Exit;
 if (pBlock^.Regs.FVolMark<>vmNone) then Exit;
 pLine:=pBlock^.line;
 if (pLine=nil) then Exit;
 if (pLine^.OpId<>OpBlock) then Exit;
 pChild:=pLine^.dst.AsBlock;
 if (pChild=nil) then Exit;
 Case pChild^.Block.bType of
  btAdr,
  btOther:;
  else
   Exit;
 end;
 pBlock^.Regs.FVolMark:=pChild^.Regs.FVolMark;
end;

function TSprvEmit.PopBlockOp:Boolean;
var
 pOpBlock:PsrOpBlock;
 pOpChild:PsrOpBlock;
 pOpLabel:array[0..2] of PspirvOp;
begin
 Result:=False;
 if (FMain=nil) then Exit;
 if (FMain^.pBlock=nil) then Exit;

 pOpBlock:=FMain^.pBlock;
 UpdateVolMark(pOpBlock);

 pOpLabel[0]:=pOpBlock^.Labels.pBegOp;
 pOpLabel[1]:=pOpBlock^.Labels.pEndOp;
 pOpLabel[2]:=pOpBlock^.Labels.pMrgOp;

 Case pOpBlock^.Block.bType of
  btCond:
   begin

    Assert(pOpLabel[1]<>nil);

    Case pOpBlock^.Regs.FVolMark of
     vmNone:TEmitVolatile(Self).build_volatile_cur(pOpBlock^.Regs.pSnap);
     vmEnd :TEmitVolatile(Self).build_volatile_dis(pOpBlock^.Regs.pSnap);
     else;
    end;

    if not is_term_op(line) then
    begin
     TEmitOp(Self).emit_OpBranch(line,pOpLabel[1]);
    end;

    AddSpirvOp(line,pOpLabel[1]); //end

   end;
  btLoop:
   begin

    //add OpLoopMerge continue

    Assert(pOpLabel[0]<>nil);
    Assert(pOpLabel[1]<>nil);
    Assert(pOpLabel[2]<>nil);

    Case pOpBlock^.Regs.FVolMark of
     vmNone:TEmitVolatile(Self).build_volatile_old(pOpBlock^.Regs.pSnap);
     else;
    end;

    if pOpBlock^.Cond.FUseCont then //use continue
    begin

     if not is_term_op(line) then
     begin
      TEmitOp(Self).emit_OpBranch(line,pOpLabel[1]); //break
     end;

     AddSpirvOp(line,pOpLabel[2]); //OpLoopMerge end

       pOpChild:=AllocBlockOp;
       pOpChild^.SetInfo(btOther,FCursor.Adr,FCursor.Adr);
       PushBlockOp(line,pOpChild,nil);
       TEmitOp(Self).emit_OpBranch(line,pOpLabel[0]); //continue
     FMain^.PopBlock;

     AddSpirvOp(line,pOpLabel[1]); //end

    end else //dont used continue
    begin

     if not is_term_op(line) then
     begin
      AddSpirvOp(line,NewLabelOp); //devide
     end;

     AddSpirvOp(line,pOpLabel[2]); //OpLoopMerge end

       pOpChild:=AllocBlockOp;
       pOpChild^.SetInfo(btOther,FCursor.Adr,FCursor.Adr);
       PushBlockOp(line,pOpChild,nil);
       TEmitOp(Self).emit_OpBranch(line,pOpLabel[1]); //break
     FMain^.PopBlock;

     AddSpirvOp(line,pOpLabel[1]); //end

    end;

   end;
  else
   if (pOpLabel[1]<>nil) then
   begin
    if not is_term_op(line) then
    begin
     TEmitOp(Self).emit_OpBranch(line,pOpLabel[1]);
    end;
    AddSpirvOp(line,pOpLabel[1]);
   end;
 end;

 Case pOpBlock^.Block.bType of
  btAdr:
   begin
    FCursor:=pOpBlock^.FCursor;
   end;
  btOther:; //nop
  else
   begin
    FCursor.PopBlock;
   end;
 end;

 Result:=FMain^.PopBlock;
end;

function TSprvEmit.NewSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=Alloc(SizeOf(TSpirvOp));
 Result^.adr  :=FCursor.Adr;
 Result^.OpId :=OpId;
end;

function TSprvEmit.AddSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=AddSpirvOp(line,OpId);
end;

function TSprvEmit.AddSpirvOp(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
begin
 Result:=InsSpirvOp(pLine,NewSpirvOp(OpId));
end;

function TSprvEmit.AddSpirvOp(pLine,pNew:PspirvOp):PSpirvOp;
begin
 Result:=InsSpirvOp(pLine,pNew);
end;

function TSprvEmit.line:PSpirvOp;
begin
 Result:=nil;
 if (FMain<>nil) then
 begin
  Result:=FMain^.line;
 end;
end;

function TSprvEmit.init_line:PSpirvOp;
begin
 Assert(FInitLine<>nil);
 if (FInitLine^.dummy.pParent=nil) then
 begin
  FInitLine^.Alloc:=@Alloc;
  FInitLine^.dummy.pParent:=FInitLine;
  FInitLine^.Push_head(@FInitLine^.dummy);
 end;
 Result:=FInitLine^.line;
 Assert(Result<>nil);
end;

procedure TSprvEmit.PostLink(ntype:TsrNodeType;Data:Pointer);
var
 node:PspirvOp;
begin
 node:=line;
 if (node=nil) or (node^.OpId<>OpLinks) then
 begin
  node:=AddSpirvOp(OpLinks);
 end;
 node^.AddParam(ntype,Data);
end;

procedure TSprvEmit.PostReg(pReg:PsrRegNode);
begin
 PostLink(ntReg,pReg);
end;

function TSprvEmit.NewRefId:PsrRefId;
begin
 Result:=Alloc(SizeOf(TsrRefId));
end;

function TSprvEmit.NewVariable:PsrVariable;
begin
 Result:=Alloc(SizeOf(TsrVariable));
 FVariables.Push_tail(Result);
end;

function TSprvEmit.AddInput(dst:PsrRegSlot;rtype:TsrDataType;itype:TpsslInputType;id:Byte=0):PsrRegNode;
var
 i:PsrInput;
 v:PsrVariable;
 r:PsrRegNode;
begin
 i:=FInputs.Fetch(itype,id);
 v:=i^.pVar;
 r:=i^.pReg;

 if (v=nil) then
 begin
  v:=NewVariable;
  v^.dtype:=rtype;
  v^.pSource.SetParam(ntInput,i);
  i^.pVar:=v;
 end;

 if (r=nil) then
 begin
  r:=dst^.New(line,rtype);
  i^.pReg :=r;
  v^.mark_read;
  TEmitOp(Self).emit_OpLoad(init_line,r,v); //one in any scope
 end else
 if (dst^.current<>r) then
 begin
  MakeCopy(dst,r);
 end;

 Result:=r;
end;

function TSprvEmit.AllocDescVar(p:Pointer;_ntype:TsrNodeType;rtype:TsrDataType):PsrVariable;
var
 d:PsrDescriptor;
begin
 Result:=nil;
 if (p=nil) then Exit;
 d:=p;
 if (d^.pVar=nil) then
 begin
  d^.pVar:=NewVariable;
  d^.pVar^.dtype:=rtype;
  d^.pVar^.pSource.SetParam(_ntype,p);
 end;
 Result:=d^.pVar;
end;

function TSprvEmit.AddVertLayout(pLayout:PsrDataLayout;rtype:TsrDataType):PsrRegNode;
var
 i:PsrVertLayout;
 v:PsrVariable;
 dst:PsrRegNode;
begin
 i:=FVertLayouts.Fetch(pLayout);
 v:=AllocDescVar(i,ntVertLayout,rtype);
 dst:=i^.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(rtype);
  i^.pReg:=dst;
  v^.mark_read;
  TEmitOp(Self).emit_OpLoad(init_line,dst,v); //one in any scope
 end;
 Result:=dst;
end;

function TSprvEmit.AddFragLayout(itype:TpsslInputType;rtype:TsrDataType;location:DWORD):PsrRegNode;
var
 i:PsrFragLayout;
 v:PsrVariable;
 dst:PsrRegNode;
begin
 i:=FFragLayouts.Fetch(itype,location);
 v:=AllocDescVar(i,ntFragLayout,rtype);
 dst:=i^.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(rtype);
  i^.pReg:=dst;
  v^.mark_read;
  TEmitOp(Self).emit_OpLoad(init_line,dst,v); //one in any scope
 end;
 Result:=dst;
end;

function TSprvEmit.FetchLoad(pChain:PsrChain;rtype:TsrDataType):PsrRegNode;
var
 dtype:TsrDataType;
 src:TOpParamSingle;
begin
 Result:=nil;
 if (pChain=nil) then Exit;

 dtype:=pChain^.GetRegType;
 if (dtype=dtUnknow) and (rtype<>dtUnknow) then
 begin
  dtype:=rtype;
  pChain^.SetRegType(dtype);
 end;

 Result:=pChain^.rSlot.New(line,dtype);
 pChain^.mark_read;

 src:=Default(TOpParamSingle);
 src.SetParam(ntChain,pChain);
 TEmitOp(Self).emit_OpLoad(line,FSpirvTypes.Fetch(dtype),Result,src);

 if not CompareType(dtype,rtype) then
 begin
  MakeCopy(@FRegsStory.FUnattach,Result);
  Result:=FRegsStory.FUnattach.current;
  Result^.dtype:=rtype;
 end;
end;

Procedure TSprvEmit.FetchStore(pChain:PsrChain;src:PsrRegNode);
var
 dtype:TsrDataType;
 dst:TOpParamSingle;
 pReg:PsrRegNode;
begin
 if (pChain=nil) or (src=nil) then Exit;

 dtype:=pChain^.GetRegType;
 if (dtype=dtUnknow) and (src^.dtype<>dtUnknow) then
 begin
  dtype:=src^.dtype;
  pChain^.SetRegType(dtype);
 end;

 pReg:=pChain^.rSlot.New(line,dtype);
 pReg^.mark_read;
 pChain^.mark_write;

 dst:=Default(TOpParamSingle);
 dst.SetParam(ntChain,pChain);
 TEmitOp(Self).emit_OpStore(line,dst,pReg);
 pReg^.pLine:=line; //update line

 pReg^.pWriter.SetParam(ntReg,src);
end;

procedure TSprvEmit.AddUserdata(dst:PsrRegSlot;offset_dw:Byte);
var
 pLayout:PsrDataLayout;
 pChain:PsrChain;
 pReg:PsrRegNode;
begin
 pLayout:=FDataLayouts.Fetch(nil,0,rtBufPtr2);
 pChain:=pLayout^.Fetch(offset_dw*4,4,nil);
 pReg:=FetchLoad(pChain,dtUnknow);
 MakeCopy(dst,pReg);
end;

function TSprvEmit.FetchChain(grp:PsrDataLayout;offset,size:DWORD;ext:PsrChainExt):PsrRegNode;
var
 pChain:PsrChain;
begin
 pChain:=grp^.Fetch(offset,size,ext);
 Result:=FetchLoad(pChain,dtUnknow);
end;

function TSprvEmit.MakeChain(pSlot:PsrRegSlot;grp:PsrDataLayout;offset,size:PtrUint;ext:PsrChainExt):PsrRegNode;
var
 pChain:PsrChain;
 pReg:PsrRegNode;
begin
 pChain:=grp^.Fetch(offset,size,ext);
 pReg:=FetchLoad(pChain,dtUnknow);
 MakeCopy(pSlot,pReg);
 Result:=pSlot^.current;
end;

Procedure TSprvEmit.AddVecInput(dst:PsrRegSlot;vtype,rtype:TsrDataType;itype:TpsslInputType;id:Byte);
var
 rsl:PsrRegNode;
begin
 rsl:=AddInput(@FRegsStory.FUnattach,vtype,itype,0);
 rsl^.mark_read;
 dst^.New(line,rtype);
 TEmitOp(Self).emit_OpCompExtract(init_line,dst^.current,rsl,id);
end;

////

function TSprvEmit.FetchUniform(src:PsrDataLayout;pType:PsrType):PsrRegNode;
var
 pUniform:PsrUniform;
 v:PsrVariable;
 dst:PsrRegNode;
 p:TOpParamSingle;
begin
 pUniform:=FUniforms.Fetch(src,pType);
 v:=AllocDescVar(pUniform,ntUniform,pType^.dtype);
 if (v^.pType=nil) then
 begin
  v^.pType:=FSpirvTypes.FetchPointer(pType,pUniform^.FStorage);
 end;

 dst:=pUniform^.pReg;
 if (dst=nil) then
 begin
  dst:=NewReg(pType^.dtype);
  pUniform^.pReg:=dst;
  v^.mark_read;
  pType^.mark_read;
  p.SetParam(ntVar,v);
  TEmitOp(Self).emit_OpLoad(init_line,pType,dst,p); //one in any scope
 end;
 Result:=dst;
end;

function TSprvEmit.FetchImage(src:PsrDataLayout;dtype:TsrDataType;info:TsrTypeImageInfo):PsrRegNode;
var
 pType:PsrType;
begin
 pType:=FSpirvTypes.FetchImage(FSpirvTypes.Fetch(dtype),info);
 Result:=FetchUniform(src,pType);
end;

function TSprvEmit.FetchSampler(src:PsrDataLayout):PsrRegNode;
var
 pType:PsrType;
begin
 pType:=FSpirvTypes.Fetch(dtTypeSampler);
 Result:=FetchUniform(src,pType);
end;

function TSprvEmit.FetchOutput(etype:TpsslExportType;rtype:TsrDataType):PsrVariable;
var
 o:PsrOutput;
begin
 o:=FOutputs.Fetch(etype);
 if (o^.pVar=nil) then
 begin
  o^.pVar:=NewVariable;
  o^.pVar^.dtype:=rtype;
  o^.pVar^.pSource.SetParam(ntOutput,o);
 end;
 o^.pVar^.mark_read;
 Result:=o^.pVar;
end;

Procedure TSprvEmit.Init;
begin
 FillChar(Self,SizeOf(TSprvEmit),0);

 FUseVertexInput:=True;

 FRegsStory  .Init(@Alloc);
 FOutputs    .Init;
 FCodeHeap   .Alloc:=@Alloc;
 FInputs     .Init(@Alloc);
 FDataLayouts.Init(@Alloc);
 FVertLayouts.Init(@Alloc);
 FFragLayouts.Alloc:=@Alloc;
 FBuffers    .Init(@Alloc);
 FUniforms   .Alloc:=@Alloc;
 FCacheOps   .Alloc:=@Alloc;
 FConsts     .Alloc:=@Alloc;
 FBitcast    .pRoot:=@Self;
 FSpirvTypes .Alloc:=@Alloc;
 FSpirvCaps  .Alloc:=@Alloc;

 FDebugInfo  .Init(@Alloc);
 FDecorates  .Init(@Alloc);

 FHeader     .Init(@Alloc);

 FSpirvFuncs.Insert(NewMain);

 SetConst(@FRegsStory.VCC[0] ,dtBool,0);
 SetConst(@FRegsStory.VCC[1] ,dtBool,0);
 SetConst(@FRegsStory.EXEC[0],dtBool,1);
 SetConst(@FRegsStory.EXEC[1],dtBool,0);

 FInitLine:=AllocBlockOp;
 FInitLine^.SetInfo(btOther,FCursor.Adr,FCursor.Adr);
 PushBlockOp(line,FInitLine,nil);
 FMain^.PopBlock;
end;

procedure TSprvEmit.SetUserData(pData:Pointer);
begin
 FDataLayouts.SetUserData(pData);
end;

Procedure TSprvEmit.InitVs(RSRC2:TSPI_SHADER_PGM_RSRC2_VS;instance:Byte);
var
 p:Byte;
begin
 Init;

 FExecutionModel:=ExecutionModel.Vertex;

 //sgrp
 p:=0;
 if (RSRC2.USER_SGPR<>0) then
 For p:=p to RSRC2.USER_SGPR-1 do
 begin
  AddUserdata(@FRegsStory.SGRP[p],p);
 end;
 p:=RSRC2.USER_SGPR;

 if (RSRC2.SO_EN<>0) or
    (RSRC2.OC_LDS_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itVsState);
  p:=p+1;
  //s_vs_state
  // stream_id[1:0], is_offchip[2],
  // streamout_vtx_count[6:0],
  // streamout_enable[15:0]}
 end;

 if (RSRC2.SO_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itWriteIndex);
  p:=p+1;
  //s_so_write_index
  // streamout_write_index[31:0]
 end;

 if (RSRC2.SO_BASE0_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itOffset,0);
  p:=p+1;
  //s_so_base_offset0
  // streamout_offset0[31:0]
 end;

 if (RSRC2.SO_BASE1_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itOffset,1);
  p:=p+1;
  //s_so_base_offset1
  // streamout_offset1[31:0]
 end;

 if (RSRC2.SO_BASE2_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itOffset,2);
  p:=p+1;
  //s_so_base_offset2
  // streamout_offset2[31:0]
 end;

 if (RSRC2.SO_BASE3_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itOffset,3);
  p:=p+1;
  //s_so_base_offset3
  // streamout_offset3[31:0]
 end;

 Assert(RSRC2.OC_LDS_EN=0);

 if (RSRC2.DISPATCH_DRAW_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itWaveId);
  p:=p+1;
  //s_wave_id
  // wave_id [11:0] (dispatch draw term)
 end;

 if (RSRC2.SCRATCH_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itScratch);
  p:=p+1;
  //s_scratch
  // scratch offset (in bytes)
 end;

 //vgrp
 p:=1;
 AddInput(@FRegsStory.VGRP[0],dtUint32,itVIndex);

 if (instance>=1) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtUint32,itVInstance,1);
  p:=p+1;
 end;

 if (instance>=2) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtUint32,itVInstance,2);
  p:=p+1;
 end;

 if (instance>=3) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtUint32,itVInstance,0);
 end;

 AddCap(Capability.Shader);

end;

Procedure TSprvEmit.InitPs(RSRC2:TSPI_SHADER_PGM_RSRC2_PS;ENA:TSPI_PS_INPUT_ENA);
var
 p:Byte;
begin
 Init;

 FExecutionModel:=ExecutionModel.Fragment;

 //sgrp
 p:=0;
 if (RSRC2.USER_SGPR<>0) then
 For p:=p to RSRC2.USER_SGPR-1 do
 begin
  AddUserdata(@FRegsStory.SGRP[p],p);
 end;
 p:=RSRC2.USER_SGPR;

 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itPsState);
  p:=p+1;
  //s_ps_state
  // {bc_optimize, prim_mask[14:0],
  // lds_offset[15:0]}
 end;

 if (RSRC2.WAVE_CNT_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itWaveCnt);
  p:=p+1;
  //(s_wave_cnt)
  // {ps_wave_id[9:0], ps_strap_id,
  // ps_pkr_id}
 end;

 if (RSRC2.SCRATCH_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itScratch);
  p:=p+1;
  //s_scratch
  // scratch offset (in bytes)
 end;

 //vgrp
 p:=0;
 if (ENA.PERSP_SAMPLE_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspSample,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspSample,1);
  p:=p+1;
 end;

 if (ENA.PERSP_CENTER_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspCenter,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspCenter,1);
  p:=p+1;
 end;

 if (ENA.PERSP_CENTROID_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspCentroid,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspCentroid,1);
  p:=p+1;
 end;

 if (ENA.PERSP_PULL_MODEL_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspW,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspW,1);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itPerspW,2);
  p:=p+1;
 end;

 if (ENA.LINEAR_SAMPLE_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itLinearSample,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itLinearSample,1);
  p:=p+1;
 end;

 if (ENA.LINEAR_CENTER_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itLinearCenter,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itLinearCenter,1);
  p:=p+1;
 end;

 if (ENA.LINEAR_CENTROID_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itLinearCentroid,0);
  p:=p+1;
  AddInput(@FRegsStory.VGRP[p],dtFloat32,itLinearCentroid,1);
  p:=p+1;
 end;

 if (ENA.POS_X_FLOAT_ENA<>0) then
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,0);
  p:=p+1;
 end;

 if (ENA.POS_Y_FLOAT_ENA<>0) then
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,1);
  p:=p+1;
 end;

 if (ENA.POS_Z_FLOAT_ENA<>0) then
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,2);
  p:=p+1;
 end;

 if (ENA.POS_W_FLOAT_ENA<>0) then
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,3);
  p:=p+1;
 end;

 if (ENA.FRONT_FACE_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtUint32,itFrontFace);
  p:=p+1;
 end;

 if (ENA.ANCILLARY_ENA<>0) then
 begin
  SetConst(@FRegsStory.VGRP[p],dtUint32,2);
  //AddInput(@FRegsStory.VGRP[p],dtUint32,itAncillary);
  p:=p+1;
  //Render target array index[26:16], BuiltIn.Layer
  //Iterated sample number[11:8],     BuiltIn.SampleId
  //Primitive type[1:0]               0..3

  //POINT 00
  //LINE  01
  //TRI   10
  //RECT  11

  //PID       ISID      RTAI
  //01|234567|89AB|CDEF|0123456789A|
  //10|000000|    |0000|00000000000|
 end;

 if (ENA.SAMPLE_COVERAGE_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtUint32,itSampleCoverage);
  p:=p+1;
 end;

 if (ENA.POS_FIXED_PT_ENA<>0) then
 begin
  AddInput(@FRegsStory.VGRP[p],dtUint32,itPosFixed);
  p:=p+1;
  //Per-pixel fixed point position Y[31:16], X[15:0]
 end;

 AddCap(Capability.Shader);

end;

Procedure TSprvEmit.InitCs(RSRC2:TCOMPUTE_PGM_RSRC2;NTX:TCOMPUTE_NUM_THREAD_X;NTY:TCOMPUTE_NUM_THREAD_Y;NTZ:TCOMPUTE_NUM_THREAD_Z);
var
 p:Byte;
begin
 Init;

 FExecutionModel:=ExecutionModel.GLCompute;

 p:=0;
 if (RSRC2.USER_SGPR<>0) then
 For p:=p to RSRC2.USER_SGPR-1 do
 begin
  AddUserdata(@FRegsStory.SGRP[p],p);
 end;
 p:=RSRC2.USER_SGPR;

 if (RSRC2.TGID_X_EN<>0) then
 begin
  AddVecInput(@FRegsStory.SGRP[p],dtVec3u,dtUint32,itTgid,0);
  p:=p+1;
  //(s_tgid_x) threadgroup_id0[31:0] computePgmRsrc2.tgid_x_en;tgid_x_en(1)  //gl_WorkGroupID
 end;

 if (RSRC2.TGID_Y_EN<>0) then
 begin
  AddVecInput(@FRegsStory.SGRP[p],dtVec3u,dtUint32,itTgid,1);
  p:=p+1;
  //(s_tgid_y) threadgroup_id1[31:0] computePgmRsrc2.tgid_y_en;tgid_y_en(1)  //gl_WorkGroupID
 end;

 if (RSRC2.TGID_Z_EN<>0) then
 begin
  AddVecInput(@FRegsStory.SGRP[p],dtVec3u,dtUint32,itTgid,2);
  p:=p+1;
  //(s_tgid_z) threadgroup_id2[31:0] computePgmRsrc2.tgid_z_en;tgid_z_en(1)  //gl_WorkGroupID
 end;

 if (RSRC2.TG_SIZE_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itTgSize);
  p:=p+1;
  //(s_tg_size)                    //gl_NumWorkGroups ???
  {first_wave, 14’b0,
  ordered_append_term[10:0],
  threadgroup_size_in_waves[5:0]} //computePgmRsrc2.tg_size_en;tg_size_en(1)
 end;

 if (RSRC2.SCRATCH_EN<>0) then
 begin
  AddInput(@FRegsStory.SGRP[p],dtUint32,itScratch);
  p:=p+1;
  //s_scratch
  // scratch offset (in bytes)
 end;

 //vgrp
 p:=0;
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec3u,dtUint32,itThreadId,0);
  p:=p+1;
  //(v_thread_id_x) Thread ID in group 0  //gl_LocalInvocationID
 end;

 if (RSRC2.TIDIG_COMP_CNT>=1) then
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec3u,dtUint32,itThreadId,1);
  p:=p+1;
  //(v_thread_id_y) Thread ID in group 1  //gl_LocalInvocationID
 end;

 if (RSRC2.TIDIG_COMP_CNT>=2) then
 begin
  AddVecInput(@FRegsStory.VGRP[p],dtVec3u,dtUint32,itThreadId,2);
  p:=p+1;
  //(v_thread_id_z) Thread ID in group 2 //gl_LocalInvocationID
 end;

 FLocalSize.x:=NTX.NUM_THREAD_FULL+NTX.NUM_THREAD_PARTIAL;
 FLocalSize.y:=NTY.NUM_THREAD_FULL+NTY.NUM_THREAD_PARTIAL;
 FLocalSize.z:=NTZ.NUM_THREAD_FULL+NTZ.NUM_THREAD_PARTIAL;

 if (FLocalSize.x=0) then FLocalSize.x:=1;
 if (FLocalSize.y=0) then FLocalSize.y:=1;
 if (FLocalSize.z=0) then FLocalSize.z:=1;

 AddCap(Capability.Shader);
end;

Function TSprvEmit.NewReg(rtype:TsrDataType):PsrRegNode;
begin
 Result:=FRegsStory.FUnattach.New(line,rtype);
end;

Function TSprvEmit.FetchReg(pConst:PsrConst):PsrRegNode;
begin
 if (pConst=nil) then Exit(nil);
 Result:=NewReg(pConst^.key.dtype);
 Result^.SetConst(pConst);
 Result^.mark_read;
end;

Procedure TSprvEmit.SetConst(pSlot:PsrRegSlot;pConst:PsrConst);
var
 dst:PsrRegNode;
begin
 dst:=pSlot^.New(line,pConst^.key.dtype);
 dst^.SetConst(pConst);

 dst^.mark_read;
 PostReg(dst); //post processing
end;

Procedure TSprvEmit.SetConst(pSlot:PsrRegSlot;dtype:TsrDataType;value:QWORD);
begin
 SetConst(pSlot,FConsts.Fetch(dtype,value));
end;

function TSprvEmit.fetch_soffset(SOFFSET:Word;rtype:TsrDataType):PsrRegNode;
Var
 src:PsrRegSlot;
 pConst:PsrConst;
begin
 if is_const_ssrc8(SOFFSET) then
 begin
  pConst:=FConsts.Fetch_ssrc8_const(SOFFSET,0,rtype);
  Result:=FetchReg(pConst);
 end else
 begin
  src:=FRegsStory.get_ssrc8(SOFFSET);
  Result:=MakeRead(src,rtype);
 end;
end;


function TSprvEmit.fetch_ssrc9(SSRC:Word;rtype:TsrDataType):PsrRegNode;
Var
 src:PsrRegSlot;
 pConst:PsrConst;
begin
 if is_const_ssrc9(SSRC) then
 begin
  pConst:=FConsts.Fetch_ssrc9_const(SSRC,FSPI.INLINE32,rtype);
  Result:=FetchReg(pConst);
 end else
 begin
  src:=FRegsStory.get_ssrc9(SSRC);
  Result:=MakeRead(src,rtype);
 end;
end;

function TSprvEmit.fetch_ssrc9_pair(src:PPsrRegNode;SSRC:Word;rtype:TsrDataType):Boolean;
Var
 pSlot:array[0..1] of PsrRegSlot;
begin
 if is_const_ssrc9(SSRC) then
 begin
  src[0]:=FetchReg(FConsts.Fetch_ssrc9_const(SSRC,FSPI.INLINE32,rtype));
  src[1]:=FetchReg(FConsts.Fetch(dtUnknow,0));
  Result:=True;
 end else
 begin
  pSlot[0]:=nil;
  pSlot[1]:=nil;
  Result:=FRegsStory.get_ssrc9_pair(SSRC,pSlot);
  if not Result then Exit;
  src[0]:=MakeRead(pSlot[0],rtype);
  src[1]:=MakeRead(pSlot[1],rtype);
 end;
end;

function TSprvEmit.fetch_vsrc8(VSRC:Word;rtype:TsrDataType):PsrRegNode;
var
 src:PsrRegSlot;
begin
 src:=FRegsStory.get_vsrc8(VSRC);
 Result:=MakeRead(src,rtype);
end;

function TSprvEmit.fetch_vdst8(VDST:Word;rtype:TsrDataType):PsrRegNode;
var
 src:PsrRegSlot;
begin
 src:=FRegsStory.get_vdst8(VDST);
 Result:=MakeRead(src,rtype);
end;

procedure TSprvEmit.fetch_vsrc8_vec2h(VSRC:Word;var dst0,dst1:PsrRegNode);
var
 pSlot:PsrRegSlot;
 dst:PsrRegNode;
begin
 pSlot:=FRegsStory.get_vsrc8(VSRC);

 dst:=MakeRead(pSlot,dtVec2h);
 dst^.mark_read;

 dst0:=NewReg(dtHalf16);
 dst1:=NewReg(dtHalf16);

 dst0^.mark_read;
 dst1^.mark_read;

 TEmitOp(Self).emit_OpCompExtract(line,dst0,dst,0);
 TEmitOp(Self).emit_OpCompExtract(line,dst1,dst,1);
end;

procedure TSprvEmit._MakeCopy(dst:PsrRegSlot;src:PsrRegNode);
var
 node:PsrRegNode;
begin
 node:=dst^.New(line,src^.dtype);
 node^.pWriter.SetParam(ntReg,src);

 dst^.current^.mark_read;
 PostReg(dst^.current); //post processing
end;

procedure TSprvEmit.MakeCopy(dst:PsrRegSlot;src:PsrRegNode);
begin
 src^.mark_read;
 _MakeCopy(dst,src);
end;

function GetChainRegNode(node:PsrRegNode):PsrChain;
var
 pOp:PSpirvOp;
begin
 Result:=nil;
 node:=RegDown(node);
 pOp:=node^.AsOp;
 if (pOp=nil) then Exit;
 if (pOp^.OpId<>Op.OpLoad) then Exit;
 Result:=pOp^.pParam.pHead^.AsChain;
end;

function GetSourceRegNode(node:PsrRegNode):TOpParamSingle;
var
 pOp:PSpirvOp;
 pVar:PsrVariable;
begin
 Result:=Default(TOpParamSingle);
 node:=RegDown(node);
 pOp:=node^.AsOp;
 if (pOp=nil) then Exit;
 if (pOp^.OpId<>Op.OpLoad) then Exit;
 pVar:=pOp^.pParam.pHead^.AsVar;
 if (pVar=nil) then Exit;
 Result:=pVar^.pSource;
end;

function GetInputRegNode(node:PsrRegNode):PsrInput;
var
 pSource:TOpParamSingle;
begin
 pSource:=GetSourceRegNode(node);
 Result:=pSource.AsInput;
end;

//

function TSprvEmit.GroupingSharp(src:PPsrRegSlot;rtype:TsrResourceType):PsrDataLayout;
var
 i,n:Byte;
 chain:TsrChains;
begin
 Result:=nil;
 chain:=Default(TsrChains);
 n:=GetResourceSizeDw(rtype);
 For i:=0 to n-1 do
 begin
  chain[i]:=GetChainRegNode(src[i]^.current);
 end;
 Result:=FDataLayouts.Grouping(chain,rtype);
end;

procedure TSprvEmit.PrepTypeSlot(pSlot:PsrRegSlot;rtype:TsrDataType);
var
 node:PsrRegNode;
begin
 if (pSlot=nil) then Exit;
 if (pSlot^.current=nil) then
 begin
  pSlot^.New(line,rtype); //Unresolve
  Exit;
 end;

 if (rtype=dtUnknow) then Exit;

 node:=pSlot^.current;
 if (node^.dtype=dtUnknow) then
 begin
  RegPrepType(node,rtype);
 end;
end;

function TSprvEmit.MakeRead(pSlot:PsrRegSlot;rtype:TsrDataType):PsrRegNode;
var
 node:PsrRegNode;
begin
 Result:=nil;
 if (pSlot=nil) then Exit;
 PrepTypeSlot(pSlot,rtype);
 node:=pSlot^.current;
 if (rtype<>dtUnknow) and (not CompareType(node^.dtype,rtype)) then
 begin
  Result:=FBitcast.FetchRead(rtype,node);
 end else
 begin
  Result:=node;
  Result^.mark_read;
 end;
end;

procedure TSprvEmit.LoadPrepType(node:PsrRegNode;p:PspirvOp;rtype:TsrDataType);
var
 pVar:PsrVariable;
 pChain:PsrChain;
 ctype:TsrDataType;
 pReg:PsrRegNode;
begin
 if (p^.pParam.pHead^.pData<>nil) then
 Case p^.pParam.pHead^.ntype of
  ntVar:
   begin
    pVar:=p^.pParam.pHead^.AsVar;
    if (pVar^.dtype=dtUnknow) then
    begin
     pVar^.dtype:=rtype;
    end;
   end;
  ntChain:
   begin
    pChain:=p^.pParam.pHead^.AsChain;
    ctype:=pChain^.GetRegType;
    if (ctype=dtUnknow) then
    begin
     pChain^.SetRegType(rtype);
    end else
    if (ctype<>rtype) then
    begin
     pChain^.SetRegType(rtype);
     pReg:=NewReg(rtype);
     pReg^.pLine  :=node^.pLine;
     pReg^.pWriter:=node^.pWriter;
     pReg^.mark_read;
     node^.pWriter.SetParam(ntReg,pReg);
     rtype:=ctype;
    end;
   end;
  else;
 end;
 if (p^.dst_type=nil) then
 begin
  p^.dst_type:=FSpirvTypes.Fetch(rtype);
 end;
end;

procedure TSprvEmit.RegPrepType(node:PsrRegNode;rtype:TsrDataType);
var
 n:PsrRegNode;
 pConst:PsrConst;
 pOp:PspirvOp;
begin
 if (rtype<>dtUnknow) and (node^.dtype=dtUnknow) then
 begin
  node^.dtype:=rtype;
  //backtrace
  While (true) do
  begin
   Case node^.pWriter.ntype of
     ntVolatile:Break; //ignore
     ntReg:
      begin
       n:=node^.pWriter.pData; //next
       if (n^.dtype=dtUnknow) then
       begin
        n^.dtype:=rtype;
       end else
       begin
        Break;
       end;
       node:=n;
      end;
     ntConst:
      begin
       pConst:=node^.pWriter.pData;
       pConst^.mark_unread;
       pConst:=FConsts.Bitcast(rtype,pConst);
       node^.pWriter.pData:=pConst;
       Break;
      end;
     ntOp:
      begin
       pOp:=node^.AsOp;
       if (pOp<>nil) then
       Case pOp^.OpId of
         Op.OpLoad:
          begin
           LoadPrepType(node,pOp,rtype);
          end;
        else;
       end;
       Break;
      end;
    else
     Assert(false,'RegPrepType');
   end;
  end;
 end;
end;

function RegDown(node:PsrRegNode):PsrRegNode;
var
 tmp:PsrRegNode;
begin
 //backtrace
 Result:=node;
 While (Result<>nil) do
 begin
  tmp:=Result^.AsReg; //next
  if (tmp=nil) then Break;
  Result:=tmp;
 end;
end;

function RegDownSlot(node:PsrRegNode):PsrRegNode;
var
 tmp:PsrRegNode;
begin
 //backtrace
 Result:=node;
 While (Result<>nil) do
 begin
  tmp:=Result^.AsReg; //next
  if (tmp=nil) then Break;
  if (tmp^.pSlot<>Result^.pSlot) then Break;
  Result:=tmp;
 end;
end;

Procedure mark_read(const node:TOpParamSingle);
begin
 Case node.ntype of
  ntConst:PsrConst   (node.pData)^.mark_read;
  ntType :PsrType    (node.pData)^.mark_read;
  ntFunc :PSpirvFunc (node.pData)^.mark_read;
  ntVar  :PsrVariable(node.pData)^.mark_read;
  ntReg  :PsrRegNode (node.pData)^.mark_read;
  ntChain:PsrChain   (node.pData)^.mark_read;
  else
    Assert(false,'mark_read');
 end;
end;

Procedure mark_write(const node:TOpParamSingle);
begin
 Case node.ntype of
  ntConst:PsrConst   (node.pData)^.mark_read;
  ntType :PsrType    (node.pData)^.mark_read;
  ntFunc :PSpirvFunc (node.pData)^.mark_read;
  ntVar  :PsrVariable(node.pData)^.mark_write;
  ntReg  :PsrRegNode (node.pData)^.mark_read;
  ntChain:PsrChain   (node.pData)^.mark_write;
  else
    Assert(false,'mark_write');
 end;
end;

procedure TSprvEmit._emit_spi;
begin
 Case FSPI.CMD.EN of
  W_SOP1  :TEmit_SOP1(Self)._emit_SOP1;
  W_SOPC  :TEmit_SOPC(Self)._emit_SOPC;
  W_SOPP  :TEmit_SOPP(Self)._emit_SOPP;
  W_VOP1  :TEmit_VOP1(Self)._emit_VOP1;
  W_VOPC  :TEmit_VOPC(Self)._emit_VOPC;
  W_VOP3  :Case FSPI.VOP3a.OP of
              0..255:TEmit_VOP3(Self)._emit_VOP3c;
            293..298,
            365..366:TEmit_VOP3(Self)._emit_VOP3b;
            else
                     TEmit_VOP3(Self)._emit_VOP3a;
           end;
  W_DS    :TEmit_DS(Self)._emit_DS;
  W_MUBUF :TEmit_MUBUF(Self)._emit_MUBUF;
  W_MTBUF :TEmit_MTBUF(Self)._emit_MTBUF;
  W_EXP   :TEmit_EXP(Self)._emit_EXP;
  W_VINTRP:TEmit_VINTRP(Self)._emit_VINTRP;
  W_MIMG  :TEmit_MIMG(Self)._emit_MIMG;
  W_SMRD  :TEmit_SMRD(Self)._emit_SMRD;
  W_SOPK  :_emit_SOPK;
  W_SOP2  :TEmit_SOP2(Self)._emit_SOP2;
  W_VOP2  :TEmit_VOP2(Self)._emit_VOP2;
  else
   Assert(false);
 end;
end;

procedure TSprvEmit._emit_DS;
begin
 Assert(false,'DS?'+IntToStr(FSPI.DS.OP));
end;

procedure TSprvEmit._emit_SOPK;
begin
 Assert(false,'SOPK?'+IntToStr(FSPI.SOPK.OP));
end;

end.

