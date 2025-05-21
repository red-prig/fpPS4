unit srCFGCursor;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 srCFGParser,
 ginodes,
 srNode;

type
 PsrCursor=^TsrCursor;
 TsrCursor=object
  pCode:TsrCodeRegion;
  pNode:TsrSourceNode;
  b_adr:TSrcAdr;
  e_adr:TSrcAdr;
  fnext:Boolean;
  procedure Init(Code:TsrCodeRegion;node:TsrSourceNode;base:Pointer);
  procedure UpdateAdr;
  function  AsBlock:TsrSourceBlock;
  function  PopBlock:Boolean;
 end;

 TsrCodeList=specialize TNodeQueueClass<TsrCodeRegion>;

 PsrCodeHeap=^TsrCodeHeap;
 TsrCodeHeap=object(TsrCodeList)
  FEmit:TCustomEmit;
  Procedure Init(Emit:TCustomEmit);
  function  FindByPtr (base:Pointer):TsrCodeRegion;
  function  FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
 end;

implementation

//

Procedure TsrCodeHeap.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

function TsrCodeHeap.FindByPtr(base:Pointer):TsrCodeRegion;
var
 node:TsrCodeRegion;
begin
 Result:=nil;
 node:=pHead;
 While (node<>nil) do
 begin
  if (PtrUint(node.Body)<=PtrUint(base)) and ((PtrUint(node.Body)+node.Size)>PtrUint(base)) then
  begin
   Exit(node);
  end;
  node:=node.pNext;
 end;
end;

function TsrCodeHeap.FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
var
 pCode:TsrCodeRegion;
 pNode:TsrSourceNode;
 p_err:Integer;
begin
 pCode:=FindByPtr(base);

 if (pCode<>nil) then
 begin
  pNode:=pCode.FindByPtr(base);
 end else
 begin
  pNode:=nil;
 end;

 if (pNode=nil) then
 begin
  p_err:=parse_code_cfg2(pCode,bType,base,FEmit.GetDmem(base),FEmit);
  if (p_err>1) then
  begin
   Assert(False,'parse_code_cfg:'+IntToStr(p_err));
  end;
  //
  Push_tail(pCode);
  //
  pNode:=pCode.FindByPtr(base);
 end;

 Result.Init(pCode,pNode,base);
end;

procedure TsrCursor.Init(Code:TsrCodeRegion;node:TsrSourceNode;base:Pointer);
begin
 pCode:=Code;
 pNode:=node;
 //
 b_adr.pCode :=Code;
 b_adr.Offset:=0;
 e_adr:=b_adr;
 //
 UpdateAdr;
 //
 fnext:=(e_adr.get_code_ptr=base);
end;

procedure TsrCursor.UpdateAdr;
begin
 if (pNode<>nil) then
 begin
  if pNode.InheritsFrom(TsrSourceAdr) then
  begin
   b_adr:=TsrSourceAdr(pNode).b_adr;
   e_adr:=TsrSourceAdr(pNode).e_adr;
  end;
 end;
end;

function TsrCursor.AsBlock:TsrSourceBlock;
begin
 Result:=nil;
 if (pNode<>nil) then
 if (pNode.ntype=TsrSourceBlock) then
 begin
  Exit(TsrSourceBlock(pNode));
 end;
end;

function TsrCursor.PopBlock:Boolean;
begin
 Result:=False;
 if (pNode=nil) then Exit;
 if (pNode.pParent=nil) then Exit;
 pNode:=pNode.pParent;
 Result:=True;
end;

end.

