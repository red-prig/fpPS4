unit emit_print;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  srNode,
  srType,
  srTypes,
  srConst,
  srVariable,
  srOp,
  srOpUtils,
  strutils,
  srCapability,
  emit_fetch;

type
 TSprvEmit_print=class(TEmitFetch)
  procedure Print;
  procedure PrintCaps;
  procedure PrintOpBlock(pBlock:TsrOpBlock);
  procedure PrintHeaderInfo;
  procedure PrintTypes;
  procedure PrintConst;
  procedure PrintVariable;
  procedure PrintFunc;
  procedure PrintOp(node:TSpirvOp;print_offset:Boolean);
 end;

implementation

const
 PadAssign=0; //5

procedure TSprvEmit_print.Print;
begin
 PrintCaps;
 Writeln;
 PrintHeaderInfo;
 PrintTypes;
 Writeln;
 PrintConst;
 PrintVariable;
 PrintFunc;
end;

function GetNodeStr(Node:TsrNode):RawByteString;
var
 name:RawByteString;
begin
 Assert(Node<>nil,'GetNodeStr$1');
 name:=node.GetPrintName;
 if (name<>'') then
 begin
  Result:='%'+name;
 end else
 begin
  name:=node.GetPrintData;
  Assert(name<>'','GetNodeStr$2');
  Result:=name;
 end;
end;

procedure TSprvEmit_print.PrintCaps;
var
 node:TsrCapability;
begin
 node:=CapabilityList.First;
 While (node<>nil) do
 begin
  Writeln(Op.GetStr(Op.OpCapability),' ',Capability.GetStr(node.ID));
  node:=CapabilityList.Next(node);
 end;
end;

procedure TSprvEmit_print.PrintOpBlock(pBlock:TsrOpBlock);
var
 node:TSpirvOp;
begin
 if (pBlock=nil) then Exit;
 node:=pBlock.First;

 While (node<>nil) do
 begin
  if node.IsType(ntOp) then
  begin
   pBlock:=node.Parent;
   Write(Space(pBlock.Level));
   PrintOp(node,false);
  end;
  node:=flow_down_next_up(node);
 end;
end;

procedure TSprvEmit_print.PrintHeaderInfo;
begin
 PrintOpBlock(HeaderList);
 if (HeaderList.First<>nil) then Writeln;
 PrintOpBlock(DebugInfoList);
 if (DebugInfoList.First<>nil) then Writeln;
 PrintOpBlock(DecorateList);
 if (DecorateList.First<>nil) then Writeln;
end;

procedure TSprvEmit_print.PrintTypes;
var
 node:TsrType;
 i:Word;
begin
 node:=TypeList.First;
 While (node<>nil) do
 begin
  Write(PadRight(GetNodeStr(node),PadAssign),' = ',Op.GetStr(node.OpId));
  if (node.ItemCount<>0) then
  begin
   For i:=0 to node.ItemCount-1 do
   begin
    Write(' ',GetNodeStr(node.GetItem(i)));
   end;
  end;
  Writeln;
  node:=node.Next;
 end;
end;

procedure TSprvEmit_print.PrintConst;
var
 node:TsrConst;
 i:Word;
begin
 node:=ConstList.First;
 While (node<>nil) do
 begin
  if (node.dtype=dtUnknow) then
  begin
   Write('; ');
   Writeln(' = dtUnknow: read_count=',node.read_count,' value=',node.GetData);
  end else
  begin
   Write(PadRight(GetNodeStr(node),PadAssign),' = ',Op.GetStr(node.OpId),' ',GetNodeStr(node.pType));
   if (node.dtype<>dtBool) then
   begin
    For i:=0 to node.ItemCount-1 do
    begin
     Write(' ',GetNodeStr(node.GetItem(i)));
    end;
   end;
   Writeln;
  end;
  node:=node.Next;
 end;
 if (ConstList.First<>nil) then Writeln;
end;

procedure TSprvEmit_print.PrintVariable;
var
 node:TsrVariable;
begin
 node:=VariableList.First;
 While (node<>nil) do
 begin
  if (node.pType<>nil) then
  begin
   Writeln(PadRight(GetNodeStr(node),PadAssign),' = ',Op.GetStr(Op.OpVariable),
           ' ',GetNodeStr(node.pType),
           ' ',StorageClass.GetStr(node.GetStorageClass));
  end;
  node:=node.Next;
 end;
end;

procedure TSprvEmit_print.PrintFunc;
var
 pFunc:TSpirvFunc;
begin
 pFunc:=FuncList.First;
 While (pFunc<>nil) do
 begin
  Writeln;
  PrintOpBlock(pFunc.pTop);
  pFunc:=pFunc.Next;
 end;
end;

procedure TSprvEmit_print.PrintOp(node:TSpirvOp;print_offset:Boolean);
var
 Param:POpParamNode;
 Info:Op.TOpInfo;
begin
 if (node=nil) then Exit;

 Info:=Op.GetInfo(node.OpId);

 if Info.result then //dst
 begin
  Assert(node.pDst<>nil,'PrintOp$1');
  Write(PadRight(GetNodeStr(node.pDst),PadAssign),' = ',Op.GetStr(node.OpId));
 end else
 begin  //no dst
  Write(Op.GetStr(node.OpId));
  if (node.pDst<>nil) then
  begin
   Write(' ',GetNodeStr(node.pDst));
  end;
 end;

 if Info.rstype then //dst type
 begin
  Assert(node.pType<>nil,'PrintOp$2');
  if (node.pType<>nil) then
  begin
   Write(' ',GetNodeStr(node.pType));
  end;
 end;

 Param:=node.ParamFirst;
 While (Param<>nil) do
 begin
  Write(' ',GetNodeStr(Param.Value));
  Param:=Param.Next;
 end;

 if (node.OpId=Op.OpLabel) then
 begin
  print_offset:=true;
 end;

 Case print_offset of
  True :Writeln(' ;0x',HexStr(Node.Adr.Offdw*4,4){,' (',node.Order,')'});
  False:Writeln;
 end;
end;


end.

