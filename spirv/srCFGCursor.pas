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
  pCode :PsrCodeBlock;
  pBlock:PsrCFGBlock;
  procedure Init(Code:PsrCodeBlock);
  function  PopBlock:Boolean;
 end;

 TsrCodeList=specialize TNodeQueue<PsrCodeBlock>;

 PsrCodeHeap=^TsrCodeHeap;
 TsrCodeHeap=object(TsrCodeList)
  FEmit:TCustomEmit;
  Procedure Init(Emit:TCustomEmit);
  function  FindByPtr (base:Pointer):PsrCodeBlock;
  function  FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
 end;

implementation

//

Procedure TsrCodeHeap.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

function TsrCodeHeap.FindByPtr(base:Pointer):PsrCodeBlock;
var
 node:PsrCodeBlock;
begin
 Result:=nil;
 node:=pHead;
 While (node<>nil) do
 begin
  if node^.IsContain(base) then
  begin
   Exit(node);
  end;
  node:=node^.pNext;
 end;
end;

function TsrCodeHeap.FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
var
 pCode:PsrCodeBlock;
 adr:TSrcAdr;
begin
 pCode:=FindByPtr(base);
 if (pCode=nil) then
 begin
  pCode:=FEmit.Alloc(SizeOf(TsrCodeBlock));
  pCode^.FEmit:=FEmit;
  pCode^.Body :=base;
  pCode^.DMem :=FEmit.GetDmem(base);

  //
  if parse_code_cfg(bType,pCode)>1 then Assert(False);
  //
  Push_tail(pCode);
 end;

 Result:=Default(TsrCursor);
 Result.Init(pCode);

 adr:=Default(TSrcAdr);
 adr.pCode:=pCode;
 adr.Offdw:=(Pointer(base)-Pointer(pCode^.Body)) div 4;

 Result.Adr:=adr;

 Result.pBlock:=Result.pBlock^.DownBlock(adr);
end;

procedure TsrCursor.Init(Code:PsrCodeBlock);
begin
 inherited Init(Pointer(Code));
 pCode :=Code;
 pBlock:=@Code^.FTop;
end;

function TsrCursor.PopBlock:Boolean;
begin
 Result:=False;
 if (pBlock=nil) then Exit;
 if (pBlock^.pParent=nil) then Exit;
 pBlock:=pBlock^.pParent;
 Result:=True;
end;

end.

