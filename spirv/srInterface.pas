unit srInterface;

{$mode objfpc}{$H+}

interface

uses
 ps4_pssl,
 spirv,
 srNode,
 srConfig,
 srAllocator,
 srCFGCursor,
 srRefId,
 srLiteral,
 srCapability,
 srDecorate,
 srType,
 srTypes,
 srConst,
 srReg,
 srVariable,
 srInput,
 srOutput,
 srLayout,
 srVertLayout,
 srFragLayout,
 srBuffer,
 srUniform,
 srPrivate,
 srBitcast,
 srOp,
 srOpUtils,
 srCacheOp;

type
 TLocalSize=packed record
  x,y,z:DWORD;
 end;

 TGeometryInfo=record
  outputVertCount:DWORD; //4
  invocationCount:DWORD; //1
  InputMode      :DWORD; //Triangles
  OutputMode     :DWORD; //OutputTriangleStrip
 end;

 PPSInputCntl=^TPSInputCntl;
 TPSInputCntl=packed record
  OFFSET     :Byte;
  USE_DEFAULT:Boolean;
  DEFAULT_VAL:Byte;
  FLAT_SHADE :Boolean;
 end;

 TExportInfo=packed record
  FORMAT     :Byte;
  NUMBER_TYPE:Byte;
  COMP_SWAP  :Byte;
 end;

 TEmitInterface=class(TCustomEmit)
  FExecutionModel    :Word;
  FEarlyFragmentTests:Boolean;
  FPSInputCntl       :array[0..31] of TPSInputCntl;
  FExportInfo        :array[0..7] of TExportInfo;
  FLocalSize         :TLocalSize;
  FLDS_SIZE          :DWORD;
  FGeometryInfo      :TGeometryInfo;
  //
  Config:TsrConfig;
  //
  FSPI:TSPI;
  //
  Allocator:TsrAllocator;
  //
  LiteralList   :TsrLiteralList;
  TypeList      :TsrTypeList;
  ConstList     :TsrConstList;
  RegsStory     :TsrRegsStory;
  CapabilityList:TsrCapabilityList;
  HeaderList    :TsrHeaderList;
  DecorateList  :TsrDecorateList;
  DebugInfoList :TsrDebugInfoList;
  VariableList  :TsrVariableList;
  //
  InputList     :TsrInputList;
  OutputList    :TsrOutputList;
  //
  DataLayoutList:TsrDataLayoutList;
  VertLayoutList:TsrVertLayoutList;
  FragLayoutList:TsrFragLayoutList;
  //
  BufferList    :TsrBufferList;
  UniformList   :TsrUniformList;
  PrivateList   :TsrPrivateList;
  //
  BitcastList   :TsrBitcastList;
  CacheOpList   :TsrCacheOpList;
  //
  FuncList      :TsrFuncList;
  //
  CodeHeap :TsrCodeHeap;
  Cursor   :TsrCursor;
  Main     :TSpirvFunc;
  InitBlock:TsrOpBlock;
  //
  RefIdAlloc:TsrRefIdAlloc;
  //
  function  Alloc(Size:ptruint):Pointer; override;
  Function  GetDmem(P:Pointer) :Pointer; override;
  Function  GetExecutionModel  :Word;    override;
  Function  GetConfig          :Pointer; override;
  Function  GetCodeHeap        :Pointer; override;

  Function  GetLiteralList     :Pointer; override;
  Function  GetTypeList        :Pointer; override;
  Function  GetConstList       :Pointer; override;
  Function  GetRegsStory       :Pointer; override;
  Function  GetCapabilityList  :Pointer; override;
  Function  GetHeaderList      :Pointer; override;
  Function  GetDecorateList    :Pointer; override;
  Function  GetDebugInfoList   :Pointer; override;
  Function  GetVariableList    :Pointer; override;
  Function  GetInputList       :Pointer; override;
  Function  GetOutputList      :Pointer; override;
  Function  GetDataLayoutList  :Pointer; override;
  Function  GetVertLayoutList  :Pointer; override;
  Function  GetFragLayoutList  :Pointer; override;
  Function  GetBufferList      :Pointer; override;
  Function  GetUniformList     :Pointer; override;
  Function  GetBitcastList     :Pointer; override;
  Function  GetCacheOpList     :Pointer; override;
  Function  GetFuncList        :Pointer; override;

  Function  GetCursor          :Pointer; override;

  function  NewRefNode         :TsrNode; override;
  //
  Function  line               :TspirvOp;
  Function  curr_line          :TsrNode; override;
  function  init_line          :TsrNode; override;
  //
  Procedure InitLists;
  //
  function  NewVariable:Pointer;
  //
  function  _get_line(ppLine:PPspirvOp):TspirvOp;
  //
  Function  NewRegPair:TsrRegPair;
  Function  NewReg(rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
  Function  NewReg(pConst:TsrConst;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  Function  NewReg_q(dtype:TsrDataType;value:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
  Function  NewReg_b(value:Boolean;ppLine:PPspirvOp=nil):TsrRegNode;
  Function  NewReg_i(dtype:TsrDataType;value:Integer;ppLine:PPspirvOp=nil):TsrRegNode;
  Function  NewReg_s(dtype:TsrDataType;value:Single;ppLine:PPspirvOp=nil):TsrRegNode;
  //
  function  NewSpirvOp(OpId:DWORD):TSpirvOp;
  function  NewLabelOp(sdep:Boolean):TSpirvOp;
  function  AddSpirvOp(OpId:DWORD):TSpirvOp;
  function  AddSpirvOp(pLine:TSpirvOp;OpId:DWORD):TSpirvOp;
  function  AddSpirvOp(pLine,pNew:TSpirvOp):TSpirvOp;
  function  AddSGlslOp(pLine:TSpirvOp;OpId:DWORD):TSpirvOp;
  //
  procedure PostLink(pLine,dst:TsrNode); override;
  procedure MakeCopy(dst:PsrRegSlot;src:TsrRegNode);
  //
  Procedure SetConst(pSlot:PsrRegSlot;pConst:TsrConst);
  Procedure SetConst_q(pSlot:PsrRegSlot;dtype:TsrDataType;value:QWORD);
  Procedure SetConst_b(pSlot:PsrRegSlot;value:Boolean);
  Procedure SetConst_i(pSlot:PsrRegSlot;dtype:TsrDataType;value:Integer);
  Procedure SetConst_s(pSlot:PsrRegSlot;dtype:TsrDataType;value:Single);
  //
  function  AllocBlockOp:TsrOpBlock;
  function  NewBlockOp(Snap:TsrRegsSnapshot):TsrOpBlock;
  function  InsertBlockOp(pLine:TSpirvOp;pChild:TsrOpBlock):TSpirvOp;
  //
  procedure AddCapability(ID:DWORD);
 end;

implementation

function TEmitInterface.Alloc(Size:ptruint):Pointer;
begin
 Result:=Allocator.Alloc(Size);
end;

Function TEmitInterface.GetDmem(P:Pointer):Pointer;
begin
 if (Config.OnGetDmem<>nil) then
 begin
  Result:=Config.OnGetDmem(P);
 end else
 begin
  Result:=P;
 end;
end;

Function TEmitInterface.GetExecutionModel:Word;
begin
 Result:=FExecutionModel;
end;

Function TEmitInterface.GetConfig:Pointer;
begin
 Result:=@Config;
end;

Function TEmitInterface.GetCodeHeap:Pointer;
begin
 Result:=@CodeHeap;
end;

Function TEmitInterface.GetLiteralList:Pointer;
begin
 Result:=@LiteralList;
end;

Function TEmitInterface.GetTypeList:Pointer;
begin
 Result:=@TypeList;
end;

Function TEmitInterface.GetConstList:Pointer;
begin
 Result:=@ConstList;
end;

Function TEmitInterface.GetRegsStory:Pointer;
begin
 Result:=@RegsStory;
end;

Function TEmitInterface.GetCapabilityList:Pointer;
begin
 Result:=@CapabilityList;
end;

Function TEmitInterface.GetHeaderList:Pointer;
begin
 Result:=@HeaderList;
end;

Function TEmitInterface.GetDecorateList:Pointer;
begin
 Result:=@DecorateList;
end;

Function TEmitInterface.GetDebugInfoList:Pointer;
begin
 Result:=@DebugInfoList;
end;

Function TEmitInterface.GetVariableList:Pointer;
begin
 Result:=@VariableList;
end;

Function TEmitInterface.GetInputList:Pointer;
begin
 Result:=@InputList;
end;

Function TEmitInterface.GetOutputList:Pointer;
begin
 Result:=@OutputList;
end;

Function TEmitInterface.GetDataLayoutList:Pointer;
begin
 Result:=@DataLayoutList;
end;

Function TEmitInterface.GetVertLayoutList:Pointer;
begin
 Result:=@VertLayoutList;
end;

Function TEmitInterface.GetFragLayoutList:Pointer;
begin
 Result:=@FragLayoutList;
end;

Function TEmitInterface.GetBufferList:Pointer;
begin
 Result:=@BufferList;
end;

Function TEmitInterface.GetUniformList:Pointer;
begin
 Result:=@UniformList;
end;

Function TEmitInterface.GetBitcastList:Pointer;
begin
 Result:=@BitcastList;
end;

Function TEmitInterface.GetCacheOpList:Pointer;
begin
 Result:=@CacheOpList;
end;

Function TEmitInterface.GetFuncList:Pointer;
begin
 Result:=@FuncList;
end;

Function TEmitInterface.GetCursor:Pointer;
begin
 Result:=@Cursor;
end;

//

function TEmitInterface.NewRefNode:TsrNode;
begin
 Result:=TsrNode(specialize New<TsrRefNode>);
end;

Function TEmitInterface.line:TSpirvOp;
begin
 Result:=nil;
 if (Main<>nil) then
 begin
  Result:=Main.line;
 end;
end;

function TEmitInterface.curr_line:TsrNode;
begin
 Result:=line;
end;

function TEmitInterface.init_line:TsrNode;
begin
 Assert(InitBlock<>nil);
 if (InitBlock.dummy.Parent=nil) then //is not init?
 begin
  InitBlock.Init();
 end;
 Result:=InitBlock.line;
 Assert(Result<>nil);
end;

Procedure TEmitInterface.InitLists;
begin
 Config.Init;

 LiteralList   .Init(Self);
 TypeList      .Init(Self);
 ConstList     .Init(Self);
 RegsStory     .Init(Self);
 CapabilityList.Init(Self);
 //
 HeaderList    :=specialize New<TsrHeaderList>;
 DecorateList  :=specialize New<TsrDecorateList>;
 DebugInfoList :=specialize New<TsrDebugInfoList>;
 //
 VariableList  .Init(Self);
 InputList     .Init(Self);
 OutputList    .Init(Self);
 DataLayoutList.Init(Self);
 VertLayoutList.Init(Self);
 FragLayoutList.Init(Self);
 BufferList    .Init(Self);
 UniformList   .Init(Self);
 PrivateList   .Init(Self);
 BitcastList   .Init(Self);
 CacheOpList   .Init(Self);
end;

function TEmitInterface.NewVariable:Pointer;
begin
 Result:=VariableList.Fetch;
end;

//

function TEmitInterface._get_line(ppLine:PPspirvOp):TSpirvOp;
begin
 if (ppLine=nil)  then Exit(line);
 if (ppLine^=nil) then Exit(line);
 Result:=ppLine^;
end;

//

Function TEmitInterface.NewRegPair:TsrRegPair;
begin
 Result:=specialize New<TsrRegPair>;
end;

Function TEmitInterface.NewReg(rtype:TsrDataType;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=RegsStory.FUnattach.New(line,rtype);
 Result.pLine:=_get_line(ppLine);
end;

Function TEmitInterface.NewReg(pConst:TsrConst;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 if (pConst=nil) then Exit(nil);
 Result:=NewReg(pConst.dtype);
 Result.pWriter:=pConst;
 Result.pLine:=_get_line(ppLine);
end;

//

Function TEmitInterface.NewReg_q(dtype:TsrDataType;value:QWORD;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch(dtype,value),ppLine);
end;

Function TEmitInterface.NewReg_b(value:Boolean;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch_b(value),ppLine);
end;

Function TEmitInterface.NewReg_i(dtype:TsrDataType;value:Integer;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch_i(dtype,value),ppLine);
end;

Function TEmitInterface.NewReg_s(dtype:TsrDataType;value:Single;ppLine:PPspirvOp=nil):TsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch_s(dtype,value),ppLine);
end;

function TEmitInterface.NewSpirvOp(OpId:DWORD):TSpirvOp;
begin
 Result:=specialize New<TSpirvOp>;
 Result.Init(OpId);
 Result.adr:=Cursor.Adr;
end;

function TEmitInterface.NewLabelOp(sdep:Boolean):TSpirvOp;
Var
 node:TSpirvOp;
begin
 node:=NewSpirvOp(Op.OpLabel);
 node.pDst:=NewRefNode;
 Result:=node;
 if sdep then node.pDst.mark_read(nil);
end;

function TEmitInterface.AddSpirvOp(OpId:DWORD):TSpirvOp;
begin
 Result:=AddSpirvOp(line,OpId);
end;

function TEmitInterface.AddSpirvOp(pLine:TSpirvOp;OpId:DWORD):TSpirvOp;
begin
 Result:=InsSpirvOp(pLine,NewSpirvOp(OpId));
end;

function TEmitInterface.AddSpirvOp(pLine,pNew:TSpirvOp):TSpirvOp;
begin
 Result:=InsSpirvOp(pLine,pNew);
end;

function TEmitInterface.AddSGlslOp(pLine:TSpirvOp;OpId:DWORD):TSpirvOp;
var
 ext,node:TSpirvOp;
begin
 ext:=HeaderList.GLSL_std_450;
 node:=AddSpirvOp(pLine,Op.OpExtInst);
 node.AddParam(ext.pDst);
 node.AddLiteral(OpId,GlslOp.GetStr(OpId));
 Result:=node;
end;

procedure TEmitInterface.PostLink(pLine,dst:TsrNode);
var
 node:TSpirvOp;
begin
 node:=pLine.specialize AsType<ntOpCustom>;
 Assert(node<>nil);

 if node.IsType(ntOp) then
 if (node.OpId=Op.OpNop) then
 begin
  node.AddParam(dst);
  Exit;
 end;

 node:=AddSpirvOp(node,Op.OpNop);
 node.AddParam(dst);
 node.mark_not_used;
end;

procedure TEmitInterface.MakeCopy(dst:PsrRegSlot;src:TsrRegNode);
var
 node:TsrRegNode;
begin
 node:=dst^.New(line,src.dtype);
 node.pWriter:=src;

 PostLink(line,node); //post processing
end;

Procedure TEmitInterface.SetConst(pSlot:PsrRegSlot;pConst:TsrConst);
var
 dst:TsrRegNode;
begin
 dst:=pSlot^.New(line,pConst.dtype);
 dst.pWriter:=pConst;

 PostLink(line,dst); //post processing
end;

Procedure TEmitInterface.SetConst_q(pSlot:PsrRegSlot;dtype:TsrDataType;value:QWORD);
begin
 SetConst(pSlot,ConstList.Fetch(dtype,value));
end;

Procedure TEmitInterface.SetConst_b(pSlot:PsrRegSlot;value:Boolean);
begin
 SetConst(pSlot,ConstList.Fetch_b(value));
end;

Procedure TEmitInterface.SetConst_i(pSlot:PsrRegSlot;dtype:TsrDataType;value:Integer);
begin
 SetConst(pSlot,ConstList.Fetch_i(dtype,value));
end;

Procedure TEmitInterface.SetConst_s(pSlot:PsrRegSlot;dtype:TsrDataType;value:Single);
begin
 SetConst(pSlot,ConstList.Fetch_s(dtype,value));
end;

function TEmitInterface.AllocBlockOp:TsrOpBlock;
begin
 Result:=specialize New<TsrOpBlock>;
 Result.Init();
end;

function TEmitInterface.NewBlockOp(Snap:TsrRegsSnapshot):TsrOpBlock;
begin
 Result:=AllocBlockOp;
 Result.Regs.pSnap_org :=Alloc(SizeOf(TsrRegsSnapshot));
 Result.Regs.pSnap_cur :=Alloc(SizeOf(TsrRegsSnapshot));
 Result.Regs.pSnap_org^:=Snap;
 Result.Regs.pSnap_cur^:=Snap;
 Result.Regs.FVolMark:=vmNone;
 Result.Cond.FUseCont:=false;
end;

function TEmitInterface.InsertBlockOp(pLine:TSpirvOp;pChild:TsrOpBlock):TSpirvOp;
begin
 pLine:=InsSpirvOp(pLine,pChild);
 pChild.UpdateLevel;
 Result:=pLine;
end;

//

procedure TEmitInterface.AddCapability(ID:DWORD);
begin
 CapabilityList.Add(ID);
end;

end.


