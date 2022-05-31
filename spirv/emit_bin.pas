unit emit_bin;

{$mode ObjFPC}{$H+}

interface

uses
  SySutils,
  Classes,
  spirv,
  srNodes,
  srTypes,
  srConst,
  srReg,
  srLayout,
  srVariable,
  srOp,
  srOpUtils,
  srCap,
  srRefId,
  SprvEmit;

type
 TSPIRVHeader=packed record
  MAGIC:DWORD;
  VERSION:packed record
   MINOR:WORD;
   MAJOR:WORD;
  end;
  TOOL_VERSION:WORD;
  TOOL_ID:WORD;
  BOUND:DWORD;
  RESERVED:DWORD;
 end;

 PSPIRVInstruction=^TSPIRVInstruction;
 TSPIRVInstruction=packed record
  OP:WORD;
  COUNT:WORD;
 end;

type
 TSVInstrBuffer=object
  Data:array of DWORD;
  COUNT:DWORD;
  Procedure AllocData;
  Procedure NewOp(OpId:WORD);
  Procedure Flush(Stream:TStream);
  procedure AddParamId(P:DWORD);
  procedure AddString(const name:PChar);
  procedure AddRefId(P:PsrRefId);
  procedure AddConstId(P:PsrConst);
  procedure AddVarId(P:PsrVariable);
  procedure AddTypeId(P:PsrType);
  procedure AddFuncId(P:PSpirvFunc);
  procedure AddChainId(P:PsrChain);
  procedure AddRegId(P:PsrRegNode);
 end;

 TSprvEmit_bin=object(TSprvEmit)
  procedure SaveToStream(Stream:TStream);
  procedure SaveHeader(Stream:TStream;var Header:TSPIRVHeader);
  procedure SaveCaps(Stream:TStream);
  procedure SaveOpList(Stream:TStream;node:PspirvOp);
  procedure SaveHeaderInfo(Stream:TStream);
  procedure SaveTypes(Stream:TStream);
  procedure SaveConst(var buf:TSVInstrBuffer;node:PsrConst);
  procedure SaveConst(Stream:TStream);
  procedure SaveVariable(Stream:TStream);
  procedure SaveFunc(Stream:TStream);
  procedure SaveOp(Stream:TStream;node:PSpirvOp);
  procedure SaveOpBlock(Stream:TStream;pBlock:PsrOpBlock);
 end;

implementation

Procedure TSVInstrBuffer.AllocData;
begin
 if (Length(Data)<COUNT) then
 begin
  SetLength(Data,COUNT);
 end;
end;

Procedure TSVInstrBuffer.NewOp(OpId:WORD);
var
 I:TSPIRVInstruction;
begin
 Assert(COUNT=0,'prev op not flushed');

 COUNT:=1;
 AllocData;

 I.OP:=OpId;
 I.COUNT:=0;

 Data[0]:=DWORD(I);
end;

Procedure TSVInstrBuffer.Flush(Stream:TStream);
begin
 if (Stream=nil) or (COUNT=0) then Exit;
 TSPIRVInstruction(Data[0]).COUNT:=COUNT;
 Stream.Write(Data[0],COUNT*SizeOf(DWORD));
 COUNT:=0;
end;

procedure TSVInstrBuffer.AddParamId(P:DWORD);
var
 I:DWORD;
begin
 Assert(COUNT<>0,'new op not created');
 I:=COUNT;
 Inc(COUNT);
 AllocData;
 Data[i]:=P;
end;

procedure TSVInstrBuffer.AddString(const name:PChar);
var
 I,L,D:DWORD;
begin
 Assert(name<>nil);
 Assert(COUNT<>0,'new op not created');

 L:=StrLen(name);
 D:=(L+SizeOf(DWORD)) div SizeOf(DWORD);

 I:=COUNT;
 COUNT:=COUNT+D;
 AllocData;

 FillDWord(Data[i],D,0);
 Move(PChar(name)^,Data[i],L);
end;

procedure TSVInstrBuffer.AddRefId(P:PsrRefId);
begin
 Assert(P<>nil  ,'AddRefId$1');
 Assert(P^.Alloc,'AddRefId$2');
 AddParamId(P^.ID);
end;

procedure TSVInstrBuffer.AddConstId(P:PsrConst);
begin
 Assert(P<>nil,'AddConstId');
 AddRefId(@P^.ID);
end;

procedure TSVInstrBuffer.AddVarId(P:PsrVariable);
begin
 Assert(P<>nil,'AddVarId');
 AddRefId(@P^.ID);
end;

procedure TSVInstrBuffer.AddTypeId(P:PsrType);
begin
 Assert(P<>nil,'AddTypeId');
 AddRefId(@P^.ID);
end;

procedure TSVInstrBuffer.AddFuncId(P:PSpirvFunc);
begin
 Assert(P<>nil,'AddFuncId');
 AddRefId(@P^.ID);
end;

procedure TSVInstrBuffer.AddChainId(P:PsrChain);
begin
 Assert(P<>nil,'AddChainId');
 AddRefId(@P^.ID);
end;

procedure TSVInstrBuffer.AddRegId(P:PsrRegNode);
begin
 Assert(P<>nil,'AddRegId$1');
 Case P^.pWriter.ntype of
  ntConst:
    begin
     AddConstId(P^.pWriter.pData);
    end;
  ntOp:
    begin
     AddRefId(@P^.ID);
    end;
  else
   Assert(false,'AddRegId$2');
 end;
end;

procedure TSprvEmit_bin.SaveToStream(Stream:TStream);
var
 Header:TSPIRVHeader;
begin
 if (Stream=nil) then Exit;

 Header:=Default(TSPIRVHeader);

 Header.MAGIC:=spirv.MagicNumber;
 DWORD(Header.VERSION):=FBuffers.cfg.SpvVersion;
 Header.TOOL_VERSION:=1;
 Header.BOUND:=FSpirvIdAlloc.GetSpirvIDBound;

 SaveHeader(Stream,Header);

 SaveCaps(Stream);
 SaveHeaderInfo(Stream);
 SaveTypes(Stream);
 SaveConst(Stream);
 SaveVariable(Stream);
 SaveFunc(Stream);
end;

procedure TSprvEmit_bin.SaveHeader(Stream:TStream;var Header:TSPIRVHeader);
begin
 Stream.Write(Header,SizeOf(TSPIRVHeader));
end;

procedure TSprvEmit_bin.SaveCaps(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:PSpirvCap;
begin
 buf:=Default(TSVInstrBuffer);
 node:=FSpirvCaps.First;
 While (node<>nil) do
 begin
  buf.NewOp(Op.OpCapability);
  buf.AddParamId(node^.ID);
  buf.Flush(Stream);
  node:=FSpirvCaps.Next(node);
 end;
end;

procedure TSprvEmit_bin.SaveOpList(Stream:TStream;node:PspirvOp);
begin
 While (node<>nil) do
 begin
  SaveOp(Stream,node);
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_bin.SaveHeaderInfo(Stream:TStream);
begin
 SaveOpList(Stream,FHeader.pHead);
 SaveOpList(Stream,FDebugInfo.pHead);
 SaveOpList(Stream,FDecorates.pHead);
end;

procedure TSprvEmit_bin.SaveTypes(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:PsrType;
 pConst:PsrConst;
 i:dword;
 ie:Boolean;
begin
 buf:=Default(TSVInstrBuffer);
 node:=FSpirvTypes.FList.pHead;
 While (node<>nil) do
 begin
  ie:=True;

  pConst:=nil;
  case node^.dtype of
   dtTypeArray:
     begin
      //find a const
      pConst:=FConsts.Fetchi(dtUInt32,node^.key.ext.array_count);
      SaveConst(buf,pConst);
      buf.Flush(Stream);
     end;
   else;
  end;

  buf.NewOp(node^.key.OpId);
  buf.AddTypeId(node);

  case node^.key.OpId of

   Op.OpTypeFloat:
    begin
     buf.AddParamId(node^.key.ext.float_size);
    end;

   Op.OpTypeInt:
    begin
     buf.AddParamId(node^.key.ext.int_size);
     buf.AddParamId(node^.key.ext.int_sign);
    end;

   Op.OpTypeVector:
    begin
     ie:=False;
     buf.AddTypeId(node^.GetCompItem(0));
     buf.AddParamId(node^.key.ext.array_count);
    end;

   Op.OpTypePointer:
    begin
     buf.AddParamId(node^.key.ext.Storage_Class);
    end;

   Op.OpTypeArray:
    begin
     ie:=False;
     buf.AddTypeId(node^.GetCompItem(0));
     buf.AddConstId(pConst);
    end;

   Op.OpTypeRuntimeArray:
    begin
     ie:=False;
     buf.AddTypeId(node^.GetCompItem(0));
    end;

   Op.OpTypeImage:
    begin
     ie:=False;
     buf.AddTypeId(node^.GetCompItem(0));
     With node^.key.ext.image do
     begin
      buf.AddParamId(Dim);
      buf.AddParamId(Depth);
      buf.AddParamId(Arrayed);
      buf.AddParamId(MS);
      buf.AddParamId(Sampled);
      buf.AddParamId(Format);
     end;
    end;


  end;

  if ie then
   if (node^.key.count<>0) then
   begin
    For i:=0 to node^.key.count-1 do
    begin
     buf.AddTypeId(node^.GetCompItem(i));
    end;
   end;

  buf.Flush(Stream);

  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_bin.SaveConst(var buf:TSVInstrBuffer;node:PsrConst);
var
 i:dword;
begin

 if (node^.key.count=0) then
 begin
  if (node^.key.dtype=dtBool) then
  begin
   Case node^.AsBool of
    True :buf.NewOp(Op.OpConstantTrue);
    False:buf.NewOp(Op.OpConstantFalse);
   end;

   buf.AddTypeId(node^.pType);
   buf.AddConstId(node);
  end else
  begin
   buf.NewOp(Op.OpConstant);
   buf.AddTypeId(node^.pType);
   buf.AddConstId(node);

   Case BitSizeType(node^.key.dtype) of
     8:buf.AddParamId(node^.AsByte);
    16:buf.AddParamId(node^.AsWord);
    32:buf.AddParamId(node^.AsUint);
    64:begin
        buf.AddParamId(Lo(node^.AsUint64));
        buf.AddParamId(Hi(node^.AsUint64));
       end;
    else
      Assert(false,'SaveConst');
   end;

  end;
 end else
 begin
  buf.NewOp(Op.OpConstantComposite);
  buf.AddTypeId(node^.pType);
  buf.AddConstId(node);

  For i:=0 to node^.key.count-1 do
  begin
   buf.AddConstId(node^.GetCompItem(i));
  end;
 end;

end;

procedure TSprvEmit_bin.SaveConst(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:PsrConst;
begin
 buf:=Default(TSVInstrBuffer);
 node:=FConsts.FList.pHead;
 While (node<>nil) do
 begin
  if (node^.key.dtype<>dtUnknow) then
  begin
   SaveConst(buf,node);
   buf.Flush(Stream);
  end;
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_bin.SaveVariable(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:PsrVariable;
begin
 buf:=Default(TSVInstrBuffer);
 node:=FVariables.pHead;
 While (node<>nil) do
 begin
  if (node^.pType<>nil) then
  begin
   buf.NewOp(Op.OpVariable);
   buf.AddTypeId(node^.pType);
   buf.AddVarId(node);
   buf.AddParamId(node^.GetStorageClass);
   buf.Flush(Stream);
  end;
  node:=node^.pNext;
 end;
end;

procedure SaveOpSingle(var buf:TSVInstrBuffer;const Param:TOpParamSingle);
begin
 Assert(Param.pData<>nil,'SaveOpSingle$1');
 Case Param.ntype of
   ntFunc :buf.AddFuncId(Param.pData);
   ntRefId:buf.AddRefId(Param.pData);
   ntType :buf.AddTypeId(Param.pData);
   ntReg  :buf.AddRegId(Param.pData);
   ntVar  :buf.AddVarId(Param.pData);
   ntChain:buf.AddChainId(Param.pData);
   ntConst:buf.AddConstId(Param.pData);
  else
   Assert(false,'PrintOpSingle$3');
 end;
end;

procedure SaveOpParamNode(var buf:TSVInstrBuffer;node:POpParamNode);
begin
 Case node^.ntype of
  ntLiteral:
    begin
     buf.AddParamId(node^.Value);
    end;
  ntString:
    begin
     buf.AddString(@node^.name);
    end;
  else
   begin
    SaveOpSingle(buf,node^.AsParam);
   end;
 end;
end;

procedure TSprvEmit_bin.SaveFunc(Stream:TStream);
var
 pFunc:PSpirvFunc;
begin
 pFunc:=FSpirvFuncs.FList.pHead;
 While (pFunc<>nil) do
 begin
  SaveOpBlock(Stream,@pFunc^.FTop);
  pFunc:=pFunc^.pNext;
 end;
end;

procedure TSprvEmit_bin.SaveOp(Stream:TStream;node:PSpirvOp);
var
 buf:TSVInstrBuffer;
 Param:POpParamNode;
 Info:Op.TOpInfo;
begin
 if (node=nil) then Exit;
 buf:=Default(TSVInstrBuffer);

 Info:=Op.GetInfo(node^.OpId);

 buf.NewOp(node^.OpId);

 if Info.rstype then //dst type
 begin
  Assert(node^.dst_type<>nil,'SaveOp$4');
  buf.AddTypeId(node^.dst_type);
 end;

 if Info.result then //dst
 begin
  Assert(node^.dst.ntype<>ntUnknow,'SaveOp$1');
  Assert(node^.dst.pData<>nil,'SaveOp$2');
  SaveOpSingle(buf,node^.dst);
 end else
 begin  //no dst
  if (node^.dst.ntype<>ntUnknow) then
  begin
   Assert(node^.dst.pData<>nil,'SaveOp$3');
   SaveOpSingle(buf,node^.dst);
  end;
 end;

 Param:=node^.pParam.pHead;
 While (Param<>nil) do
 begin
  SaveOpParamNode(buf,Param);
  Param:=Param^.pNext;
 end;

 buf.Flush(Stream);
end;

procedure TSprvEmit_bin.SaveOpBlock(Stream:TStream;pBlock:PsrOpBlock);
var
 node:PSpirvOp;
begin
 if (pBlock=nil) then Exit;
 node:=pBlock^.pHead;
 While (node<>nil) do
 begin

  if (node^.OpId=OpBlock) then
  begin
   if (node^.dst.ntype=ntBlock) then
   begin
    SaveOpBlock(Stream,node^.dst.pData);
   end;
  end else
  begin
   SaveOp(Stream,node);
  end;

  node:=node^.pNext;
 end;
end;

end.



