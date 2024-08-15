unit srCFGCursor;

{$mode ObjFPC}{$H+}

interface

uses
 srCFGLabel,
 srCFGParser,
 ginodes,
 srNode;

type
 PsrCursor=^TsrCursor;
 TsrCursor=object(TsrLCursor)
  pCode :TsrCodeBlock;
  pBlock:TsrCFGBlock;
  procedure Init(Code:TsrCodeBlock);
  function  PopBlock:Boolean;
 end;

 TsrCodeList=specialize TNodeQueueClass<TsrCodeBlock>;

 PsrCodeHeap=^TsrCodeHeap;
 TsrCodeHeap=object(TsrCodeList)
  FEmit:TCustomEmit;
  Procedure Init(Emit:TCustomEmit);
  function  FindByPtr (base:Pointer):TsrCodeBlock;
  function  FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
 end;

implementation

//

Procedure TsrCodeHeap.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

function TsrCodeHeap.FindByPtr(base:Pointer):TsrCodeBlock;
var
 node:TsrCodeBlock;
begin
 Result:=nil;
 node:=pHead;
 While (node<>nil) do
 begin
  if node.IsContain(base) then
  begin
   Exit(node);
  end;
  node:=node.pNext;
 end;
end;

function TsrCodeHeap.FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
var
 pCode:TsrCodeBlock;
 adr:TSrcAdr;
begin
 pCode:=FindByPtr(base);
 if (pCode=nil) then
 begin
  pCode:=FEmit.specialize New<TsrCodeBlock>;
  pCode.FTop:=FEmit.specialize New<TsrCFGBlock>;
  pCode.FEmit:=FEmit;
  pCode.Body :=base;
  pCode.DMem :=FEmit.GetDmem(base);

  //
  if parse_code_cfg(bType,pCode)>1 then Assert(False);
  //
  Push_tail(pCode);
 end;

 Result:=Default(TsrCursor);
 Result.Init(pCode);

 adr:=Default(TSrcAdr);
 adr.pCode:=TsrLabelBlock(pCode);
 adr.Offdw:=(Pointer(base)-Pointer(pCode.Body)) div 4;

 Result.Adr:=adr;

 Result.pBlock:=Result.pBlock.DownBlock(adr);
end;

procedure TsrCursor.Init(Code:TsrCodeBlock);
begin
 inherited Init(TsrLabelBlock(Code));
 pCode :=Code;
 pBlock:=Code.FTop;
end;

function TsrCursor.PopBlock:Boolean;
begin
 Result:=False;
 if (pBlock=nil) then Exit;
 if (pBlock.pParent=nil) then Exit;
 pBlock:=pBlock.pParent;
 Result:=True;
end;

end.

