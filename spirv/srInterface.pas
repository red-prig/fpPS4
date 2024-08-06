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

 TEmitInterface=class(TCustomEmit)
  FExecutionModel:Word;
  FEarlyFragmentTests:Boolean;
  FPSInputCntl:array[0..31] of TPSInputCntl;
  FLocalSize:TLocalSize;
  FGeometryInfo:TGeometryInfo;
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
  Main     :PSpirvFunc;
  InitBlock:PsrOpBlock;
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

  function  NewRefNode         :PsrNode; override;
  //
  Function  line               :PspirvOp;
  Function  curr_line          :Pointer; override;
  function  init_line          :Pointer; override;
  //
  Procedure InitLists;
  //
  function  NewVariable:Pointer;
  //
  function  _get_line(ppLine:PPspirvOp):PspirvOp;
  //
  Function  NewRegPair:PsrRegPair;
  Function  NewReg(rtype:TsrDataType):PsrRegNode;
  Function  NewReg(pConst:PsrConst;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  Function  NewReg_q(dtype:TsrDataType;value:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
  Function  NewReg_b(value:Boolean;ppLine:PPspirvOp=nil):PsrRegNode;
  Function  NewReg_i(dtype:TsrDataType;value:Integer;ppLine:PPspirvOp=nil):PsrRegNode;
  Function  NewReg_s(dtype:TsrDataType;value:Single;ppLine:PPspirvOp=nil):PsrRegNode;
  //
  function  NewSpirvOp(OpId:DWORD):PSpirvOp;
  function  NewLabelOp(sdep:Boolean):PSpirvOp;
  function  AddSpirvOp(OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
  function  AddSpirvOp(pLine,pNew:PspirvOp):PSpirvOp;
  function  AddSGlslOp(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
  //
  procedure PostLink(pLine,dst:PsrNode); override;
  procedure MakeCopy(dst:PsrRegSlot;src:PsrRegNode);
  //
  Procedure SetConst(pSlot:PsrRegSlot;pConst:PsrConst);
  Procedure SetConst_q(pSlot:PsrRegSlot;dtype:TsrDataType;value:QWORD);
  Procedure SetConst_b(pSlot:PsrRegSlot;value:Boolean);
  Procedure SetConst_i(pSlot:PsrRegSlot;dtype:TsrDataType;value:Integer);
  Procedure SetConst_s(pSlot:PsrRegSlot;dtype:TsrDataType;value:Single);
  //
  function  AllocBlockOp:PsrOpBlock;
  function  NewBlockOp(Snap:TsrRegsSnapshot):PsrOpBlock;
  function  InsertBlockOp(pLine:PspirvOp;pChild:PsrOpBlock):PspirvOp;
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

function TEmitInterface.NewRefNode:PsrNode;
begin
 Result:=Alloc(SizeOf(TsrRefNode));
 PsrRefNode(Result)^.Init;
end;

Function TEmitInterface.line:PspirvOp;
begin
 Result:=nil;
 if (Main<>nil) then
 begin
  Result:=Main^.line;
 end;
end;

function TEmitInterface.curr_line:Pointer;
begin
 Result:=line;
end;

function TEmitInterface.init_line:Pointer;
begin
 Assert(InitBlock<>nil);
 if (InitBlock^.dummy.Parent=nil) then //is not init?
 begin
  InitBlock^.Init(Self);
 end;
 Result:=InitBlock^.line;
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
 HeaderList    .Init(Self);
 DecorateList  .Init(Self);
 DebugInfoList .Init(Self);
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

function TEmitInterface._get_line(ppLine:PPspirvOp):PspirvOp;
begin
 if (ppLine=nil)  then Exit(line);
 if (ppLine^=nil) then Exit(line);
 Result:=ppLine^;
end;

//

Function TEmitInterface.NewRegPair:PsrRegPair;
begin
 Result:=Alloc(SizeOf(TsrRegPair));
 Result^.Init;
end;

Function TEmitInterface.NewReg(rtype:TsrDataType):PsrRegNode;
begin
 Result:=RegsStory.FUnattach.New(line,rtype);
end;

Function TEmitInterface.NewReg(pConst:PsrConst;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 if (pConst=nil) then Exit(nil);
 Result:=NewReg(pConst^.dtype);
 Result^.pWriter:=pConst;
 Result^.pLine:=_get_line(ppLine);
end;

//

Function TEmitInterface.NewReg_q(dtype:TsrDataType;value:QWORD;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch(dtype,value),ppLine);
end;

Function TEmitInterface.NewReg_b(value:Boolean;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch_b(value),ppLine);
end;

Function TEmitInterface.NewReg_i(dtype:TsrDataType;value:Integer;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch_i(dtype,value),ppLine);
end;

Function TEmitInterface.NewReg_s(dtype:TsrDataType;value:Single;ppLine:PPspirvOp=nil):PsrRegNode;
begin
 Result:=NewReg(ConstList.Fetch_s(dtype,value),ppLine);
end;

function TEmitInterface.NewSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=Alloc(SizeOf(TSpirvOp));
 Result^.Init(OpId);
 Result^.adr:=Cursor.Adr;
end;

function TEmitInterface.NewLabelOp(sdep:Boolean):PSpirvOp;
Var
 node:PSpirvOp;
begin
 node:=NewSpirvOp(Op.OpLabel);
 node^.pDst:=NewRefNode;
 Result:=node;
 if sdep then node^.pDst^.mark_read(nil);
end;

function TEmitInterface.AddSpirvOp(OpId:DWORD):PSpirvOp;
begin
 Result:=AddSpirvOp(line,OpId);
end;

function TEmitInterface.AddSpirvOp(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
begin
 Result:=InsSpirvOp(pLine,NewSpirvOp(OpId));
end;

function TEmitInterface.AddSpirvOp(pLine,pNew:PspirvOp):PSpirvOp;
begin
 Result:=InsSpirvOp(pLine,pNew);
end;

function TEmitInterface.AddSGlslOp(pLine:PspirvOp;OpId:DWORD):PSpirvOp;
var
 ext,node:PSpirvOp;
begin
 ext:=HeaderList.emit_glsl_ext;
 node:=AddSpirvOp(pLine,Op.OpExtInst);
 node^.AddParam(ext^.pDst);
 node^.AddLiteral(OpId,GlslOp.GetStr(OpId));
 Result:=node;
end;

procedure TEmitInterface.PostLink(pLine,dst:PsrNode);
var
 node:PspirvOp;
begin
 node:=pLine^.AsType(ntOpCustom);
 Assert(node<>nil);

 if node^.IsType(ntOp) then
 if (node^.OpId=Op.OpNop) then
 begin
  node^.AddParam(dst);
  Exit;
 end;

 node:=AddSpirvOp(node,Op.OpNop);
 node^.AddParam(dst);
 node^.mark_not_used;
end;

procedure TEmitInterface.MakeCopy(dst:PsrRegSlot;src:PsrRegNode);
var
 node:PsrRegNode;
begin
 node:=dst^.New(line,src^.dtype);
 node^.pWriter:=src;

 PostLink(line,node); //post processing
end;

Procedure TEmitInterface.SetConst(pSlot:PsrRegSlot;pConst:PsrConst);
var
 dst:PsrRegNode;
begin
 dst:=pSlot^.New(line,pConst^.dtype);
 dst^.pWriter:=pConst;

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

function TEmitInterface.AllocBlockOp:PsrOpBlock;
begin
 Result:=Alloc(SizeOf(TsrOpBlock));
 Result^.Init(Self);
end;

function TEmitInterface.NewBlockOp(Snap:TsrRegsSnapshot):PsrOpBlock;
begin
 Result:=AllocBlockOp;
 Result^.Regs.pSnap_org :=Alloc(SizeOf(TsrRegsSnapshot));
 Result^.Regs.pSnap_cur :=Alloc(SizeOf(TsrRegsSnapshot));
 Result^.Regs.pSnap_org^:=Snap;
 Result^.Regs.pSnap_cur^:=Snap;
 Result^.Regs.FVolMark:=vmNone;
 Result^.Cond.FUseCont:=false;
end;

function TEmitInterface.InsertBlockOp(pLine:PspirvOp;pChild:PsrOpBlock):PspirvOp;
begin
 pLine:=InsSpirvOp(pLine,PspirvOp(pChild));
 pChild^.UpdateLevel;
 Result:=pLine;
end;

//

procedure TEmitInterface.AddCapability(ID:DWORD);
begin
 CapabilityList.Add(ID);
end;

end.


