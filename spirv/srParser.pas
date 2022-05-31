unit srParser;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 srLabel,
 srCFG,
 srNodes;

type
 TsrCursor=object(TsrLCursor)
  pCode:PsrCodeBlock;
  pBlock:PsrCFGBlock;
  function PopBlock:Boolean;
 end;

 TsrCodeList=specialize TNodeQueue<PsrCodeBlock>;
 TsrCodeHeap=object(TsrCodeList)
  Alloc:TfnAlloc;
  function  FindByPtr(base:Pointer):PsrCodeBlock;
  function  FetchByPtr(base:Pointer;bType:TsrBlockType):TsrCursor;
 end;

implementation

//

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
 node:PsrCodeBlock;
 adr:TSrcAdr;
begin
 node:=FindByPtr(base);
 if (node=nil) then
 begin
  node:=Alloc(SizeOf(TsrCodeBlock));
  node^.Alloc:=Alloc;
  if parse_code_cfg(bType,base,node)>1 then Assert(False);
  Push_tail(node);
 end;

 Result:=Default(TsrCursor);
 Result.Init(node^.Body);
 Result.pCode :=node;
 Result.pBlock:=@node^.FTop;

 adr:=Default(TSrcAdr);
 adr.pBody:=node^.Body;
 adr.Offdw:=(Pointer(base)-Pointer(adr.pBody)) div 4;
 Result.Adr:=adr;

 Result.pBlock:=Result.pBlock^.DownBlock(adr);
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

