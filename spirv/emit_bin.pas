unit emit_bin;

{$mode ObjFPC}{$H+}

interface

uses
  SySutils,
  Classes,
  spirv,
  srNode,
  srType,
  srTypes,
  srConst,
  srVariable,
  srOp,
  srOpUtils,
  srCapability,
  srRefId,
  emit_fetch;

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
  SCHEMA:DWORD;
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
  function  FetchData(dcount:DWORD):PDWORD;
  Procedure NewOp(OpId:WORD);
  Procedure Reset;
  Procedure Flush(Stream:TStream);
  procedure AddParam(P:DWORD);
  procedure AddNode(node:TsrNode);
 end;

 TSprvEmit_bin=class(TEmitFetch)
  procedure SaveToStream(Stream:TStream);
  procedure SaveHeader(Stream:TStream;var Header:TSPIRVHeader);
  procedure SaveCaps(Stream:TStream);
  procedure SaveOpBlock(Stream:TStream;pBlock:TsrOpBlock);
  procedure SaveHeaderInfo(Stream:TStream);
  procedure SaveTypes(Stream:TStream);
  procedure SaveConst(Stream:TStream);
  procedure SaveVariable(Stream:TStream);
  procedure SaveFunc(Stream:TStream);
  procedure SaveOp(Stream:TStream;node:TSpirvOp);
 end;

implementation

function TSVInstrBuffer.FetchData(dcount:DWORD):PDWORD;
var
 i:DWORD;
begin
 i:=COUNT;
 COUNT:=COUNT+dcount;
 //
 if (Length(Data)<COUNT) then
 begin
  SetLength(Data,COUNT);
 end;
 //
 Result:=@Data[i];
end;

Procedure TSVInstrBuffer.NewOp(OpId:WORD);
var
 I:TSPIRVInstruction;
begin
 Assert(COUNT=0,'prev op not flushed');
 COUNT:=0;

 I.OP:=OpId;
 I.COUNT:=0;

 FetchData(1)^:=DWORD(I);
end;

Procedure TSVInstrBuffer.Reset;
begin
 COUNT:=0;
end;

Procedure TSVInstrBuffer.Flush(Stream:TStream);
begin
 if (Stream=nil) or (COUNT=0) then Exit;
 TSPIRVInstruction(Data[0]).COUNT:=COUNT;
 Stream.Write(Data[0],COUNT*SizeOf(DWORD));
 Reset;
end;

procedure TSVInstrBuffer.AddParam(P:DWORD);
begin
 Assert(COUNT<>0,'new op not created');

 FetchData(1)^:=DWORD(P);
end;

procedure TSVInstrBuffer.AddNode(node:TsrNode);
var
 R:PsrRefId;
 L,D:DWORD;
 P:PDWORD;
begin
 Assert(node<>nil);
 Assert(COUNT<>0,'new op not created');

 R:=node.GetRef;
 if (R<>nil) then
 begin
  FetchData(1)^:=R^.ID;
 end else
 begin
  L:=node.GetData(nil);           //get size
  D:=(L+(SizeOf(DWORD)-1)) div 4; //align
  Assert(D<>0,'AddNode:'+node.ntype.ClassName);

  P:=FetchData(D);

  FillDWord(P^,D,0);
  node.GetData(P);
 end;
end;

procedure TSprvEmit_bin.SaveToStream(Stream:TStream);
var
 Header:TSPIRVHeader;
begin
 if (Stream=nil) then Exit;

 Header:=Default(TSPIRVHeader);

       Header.MAGIC       :=MagicNumber;
 DWORD(Header.VERSION)    :=Config.SpvVersion;
       Header.TOOL_VERSION:=2;
       Header.TOOL_ID     :=0;
       Header.BOUND       :=RefIdAlloc.GetSpirvIDBound;
       Header.SCHEMA      :=0;

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
 node:TsrCapability;
begin
 buf:=Default(TSVInstrBuffer);
 node:=CapabilityList.First;
 While (node<>nil) do
 begin
  buf.NewOp(Op.OpCapability);
  buf.AddParam(node.ID);
  buf.Flush(Stream);
  node:=CapabilityList.Next(node);
 end;
end;

procedure TSprvEmit_bin.SaveOpBlock(Stream:TStream;pBlock:TsrOpBlock);
var
 node:TSpirvOp;
begin
 if (pBlock=nil) then Exit;
 node:=pBlock.First;

 While (node<>nil) do
 begin
  if node.IsType(ntOp) then
  begin
   SaveOp(Stream,node);
  end;
  node:=flow_down_next_up(node);
 end;
end;

procedure TSprvEmit_bin.SaveHeaderInfo(Stream:TStream);
begin
 SaveOpBlock(Stream,HeaderList);
 SaveOpBlock(Stream,DebugInfoList);
 SaveOpBlock(Stream,DecorateList);
end;

procedure TSprvEmit_bin.SaveTypes(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:TsrType;
 i:Word;
begin
 buf:=Default(TSVInstrBuffer);
 node:=TypeList.First;
 While (node<>nil) do
 begin
  buf.NewOp(node.OpId);

  if (node.OpId=Op.OpConstant) then
  begin
   //Array Const
   if (node.ItemCount>0) then
   begin
    buf.AddNode(node.GetItem(0));
   end;

   buf.AddNode(node);

   if (node.ItemCount>1) then
   begin
    For i:=1 to node.ItemCount-1 do
    begin
     buf.AddNode(node.GetItem(i));
    end;
   end;
   //Array Const
  end else
  begin
   //Types
   buf.AddNode(node);

   if (node.ItemCount<>0) then
   begin
    For i:=0 to node.ItemCount-1 do
    begin
     buf.AddNode(node.GetItem(i));
    end;
   end;
   //Types
  end;

  buf.Flush(Stream);

  node:=node.Next;
 end;
end;

procedure TSprvEmit_bin.SaveConst(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:TsrConst;
 i:Word;
begin
 buf:=Default(TSVInstrBuffer);
 node:=ConstList.First;
 While (node<>nil) do
 begin
  if (node.dtype<>dtUnknow) then
  begin
   buf.NewOp(node.OpId);
   buf.AddNode(node.pType);
   buf.AddNode(node);

   if (node.dtype<>dtBool) and (node.ItemCount<>0) then
   begin
    For i:=0 to node.ItemCount-1 do
    begin
     buf.AddNode(node.GetItem(i));
    end;
   end;

   buf.Flush(Stream);
  end;
  node:=node.Next;
 end;
end;

procedure TSprvEmit_bin.SaveVariable(Stream:TStream);
var
 buf:TSVInstrBuffer;
 node:TsrVariable;
begin
 buf:=Default(TSVInstrBuffer);
 node:=VariableList.First;
 While (node<>nil) do
 begin
  if (node.pType<>nil) then
  begin
   buf.NewOp(Op.OpVariable);
   buf.AddNode(TsrNode(node.pType));
   buf.AddNode(node);
   buf.AddParam(node.GetStorageClass);

   buf.Flush(Stream);
  end;
  node:=node.Next;
 end;
end;

procedure TSprvEmit_bin.SaveFunc(Stream:TStream);
var
 pFunc:TSpirvFunc;
begin
 pFunc:=FuncList.First;
 While (pFunc<>nil) do
 begin
  SaveOpBlock(Stream,pFunc.pTop);
  pFunc:=pFunc.Next;
 end;
end;

procedure TSprvEmit_bin.SaveOp(Stream:TStream;node:TSpirvOp);
var
 buf:TSVInstrBuffer;
 Param:POpParamNode;
 Info:Op.TOpInfo;
begin
 if (node=nil) then Exit;
 buf:=Default(TSVInstrBuffer);

 Info:=Op.GetInfo(node.OpId);

 buf.NewOp(node.OpId);

 if Info.rstype then //dst type
 begin
  Assert(node.pType<>nil,'SaveOp$1');
  if (node.pType<>nil) then
  begin
   buf.AddNode(node.pType);
  end else
  begin
   buf.Reset;
   Exit;
  end;
 end;

 if Info.result then //dst
 begin
  Assert(node.pDst<>nil,'SaveOp$2');
  if (node.pDst<>nil) then
  begin
   buf.AddNode(node.pDst);
  end else
  begin
   buf.Reset;
   Exit;
  end;
 end else
 begin  //no dst
  if (node.pDst<>nil) then
  begin
   buf.AddNode(node.pDst);
  end;
 end;

 Param:=node.ParamFirst;
 While (Param<>nil) do
 begin
  buf.AddNode(Param.Value);
  Param:=Param.Next;
 end;

 buf.Flush(Stream);
end;

end.



