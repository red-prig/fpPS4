unit LineStream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  g_node_splay;

type
 PLineNode=^TLineNode;
 TLineNode=object
  pLeft :PLineNode;
  pRight:PLineNode;
  FIndex:Integer;
  FData :record end;
  function c(n1,n2:PLineNode):Integer; static;
 end;

 TLineCache=specialize TNodeSplay<TLineNode>;

 TLineStream = Class
  FStream   :TStream;
  FLineCache:TLineCache;
  FLineStart:Int64;
  FLine__End:Int64;
  FLineIndex:Integer;
  FLineCount:Integer;
  Constructor Create(Stream:TStream);
  Destructor  Destroy; override;
  procedure   Reset(full:Boolean);
  function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
  Function    ReadForward (var ABuffer; ACount : LongInt) : Integer;
  Function    ReadBackward(var ABuffer; ACount : LongInt) : Integer;
  Function    GetLine:RawByteString;
  Function    NextLine:Boolean;
  Function    PrevLine:Boolean;
  function    GetCount: integer;
  function    GetCache(Index: integer;var R:RawByteString): Boolean;
  procedure   AddCache(Index: integer;const R:RawByteString);
  procedure   DelCache(Index: integer);
  procedure   ClearCache;
  function    Get(Index: integer): RawByteString;
  function    Update:Integer;
 end;

implementation

function TLineNode.c(n1,n2:PLineNode):Integer;
begin
 Result:=Integer(n1^.FIndex>n2^.FIndex)-Integer(n1^.FIndex<n2^.FIndex);
end;

Constructor TLineStream.Create(Stream:TStream);
begin
 FStream:=Stream;
 Reset(False);
end;

Destructor TLineStream.Destroy;
begin
 ClearCache;
 inherited;
end;

procedure TLineStream.Reset(full:Boolean);
begin
 Seek(0,soBeginning);
 FLineStart:=-1;
 FLine__End:=0;
 FLineIndex:=-1;
 if full then
 begin
  FLineCount:=0;
  ClearCache;
 end;
end;

function TLineStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
 Result:=FStream.Seek(Offset,Origin);
end;

Function TLineStream.ReadForward(var ABuffer; ACount : LongInt) : Integer;
begin
 Result:=FStream.Read(ABuffer,ACount);
end;

Function TLineStream.ReadBackward(var ABuffer; ACount : LongInt) : Integer;
begin
 if (ACount<0) then Exit;
 if (FStream.Position<ACount) then
 begin
  ACount:=FStream.Position;
 end;
 FStream.Seek(-ACount,soCurrent);
 Result:=FStream.Read(ABuffer,ACount);
 if (Result>0) then
 begin
  FStream.Seek(-Result,soCurrent);
 end;
end;

Function TLineStream.GetLine:RawByteString;
var
 s:Int64;
 i:Integer;
begin
 Result:='';

 if (FLineStart<0) then Exit;

 s:=FLine__End;
 if (s<0) then s:=FStream.Size;

 i:=s-FLineStart;
 if (i=0) then Exit;

 SetLength(Result,i);

 Seek(FLineStart,soBeginning);
 i:=ReadForward(Pchar(Result)^,i);

 if (i<=0) then i:=0;

 while (i>0) do
 begin
  case Result[i] of
   #13:;
   #10:;
   else
      Break;
  end;
  Dec(i);
 end;

 SetLength(Result,i);
end;

Function TLineStream.NextLine:Boolean;
var
 f:Int64;
 i:Integer;
 fv,pv,ch:AnsiChar;
begin
 Result:=True;

 fv:=#0;
 pv:=#0;
 ch:=#0;

 //end line
 if (FLine__End>=0) then
 begin
  Seek(FLine__End,soBeginning);
 end else
 begin
  Exit(False);
 end;

 //read prev
 if (FLineIndex>=0) then
 begin
  Seek(-1,soCurrent);
  i:=ReadForward(fv,1);
  if (i<=0) then Exit(False);
 end;

 f:=FLineStart;

 if (FLineIndex<0) or (fv=#13) or (fv=#10) then
 begin
  FLineStart:=FStream.Position;
 end;

 repeat
  i:=ReadForward(ch,1);

  if (i<=0) then
  begin
   if (f=-1) or (FLineStart=f) then Exit(False);

   Break;
  end;

  //CRLF
  if (pv=#13) and (ch=#10) then
  begin
   Break;
  end else
  if (pv=#13) or (pv=#10) then
  begin
   Seek(-1,soCurrent);
   Break;
  end else
  if (fv=#13) and (ch=#10) then
  begin
   //
  end else
  begin
   pv:=ch;
  end;

  fv:=#0;

 until false;

 FLine__End:=FStream.Position;

 Inc(FLineIndex);
end;

Function TLineStream.PrevLine:Boolean;
var
 i:Integer;
 pv,ch:AnsiChar;
begin
 Result:=True;

 pv:=#0;
 ch:=#0;

 //start line
 if (FLineStart>=0) then
 begin
  Seek(FLineStart,soBeginning);
  FLine__End:=FLineStart;
 end else
 begin
  Exit(False);
 end;

 //prev caret
 repeat
  i:=ReadBackward(ch,1);

  if (i<=0) then
  begin
   Reset(False);
   Exit(False);
  end;

  //LFCR
  if (pv=#10) and (ch=#13) then
  begin
   Break;
  end else
  if (pv=#13) or (pv=#10) then
  begin
   Seek(1,soCurrent);
   Break;
  end;

  pv:=ch;

 until false;

 //read line
 repeat
  i:=ReadBackward(ch,1);

  if (i<=0) then Break;

  if (ch=#13) or (ch=#10) then
  begin
   Seek(1,soCurrent);
   Break;
  end;

 until false;

 FLineStart:=FStream.Position;
 Dec(FLineIndex);
end;

function TLineStream.GetCount: integer;
begin
 if (FLineCount<=0) then
 begin
  while NextLine do;
  FLineCount:=FLineIndex+1;
 end;
 Result:=FLineCount;
end;

function TLineStream.GetCache(Index: integer;var R:RawByteString): Boolean;
var
 _node:TLineNode;
 node :PLineNode;
begin
 Result:=False;
 if (Index<0) then Exit;

 _node:=Default(TLineNode);
 _node.FIndex:=Index;

 node:=FLineCache.Find(@_node);

 if (node=nil) then Exit();

 R:=PChar(@node^.FData);

 Result:=True;
end;

procedure TLineStream.AddCache(Index: integer;const R:RawByteString);
var
 node:PLineNode;
begin
 if (Index>=FLineCount) then Exit;

 node:=AllocMem(sizeof(TLineNode)+Length(R)+1);

 node^.FIndex:=Index;

 Move(PChar(R)^,PChar(@node^.FData)^,Length(R)+1);

 FLineCache.Insert(node);
end;

procedure TLineStream.DelCache(Index: integer);
var
 _node:TLineNode;
 node :PLineNode;
begin
 if (Index<0) then Exit;

 _node:=Default(TLineNode);
 _node.FIndex:=Index;

 node:=FLineCache.Find(@_node);

 if (node=nil) then Exit();

 FLineCache.Delete(node);

 FreeMem(node);
end;

procedure TLineStream.ClearCache;
var
 node:PLineNode;
begin
 node:=FLineCache.Min;

 while (node<>nil) do
 begin
  FLineCache.Delete(node);

  FreeMem(node);

  node:=FLineCache.Min;
 end;
end;

function TLineStream.Get(Index: integer): RawByteString;
label
 _next;
begin
 Result:='';

 if (Index<0) then Exit;

 if GetCache(Index,Result) then Exit;

 if (FLineIndex=Index) then
 begin
  Result:=GetLine;
  AddCache(Index,Result);
  Exit;
 end;

 if (FLineIndex>Index) then
 begin
  if (Index<=(FLineIndex div 2)) then
  begin
   Reset(False);
   goto _next;
  end;

  while PrevLine do
  begin
   if (FLineIndex=Index) then
   begin
    Result:=GetLine;
    AddCache(Index,Result);
    Exit;
   end;
  end;
 end else
 begin
  _next:
  while NextLine do
  begin
   if (FLineIndex=Index) then
   begin
    Result:=GetLine;
    AddCache(Index,Result);
    Exit;
   end;
  end;
 end;

end;

function TLineStream.Update:Integer;
var
 i:Integer;
 e:Int64;
begin
 e:=FLine__End;
 i:=GetCount;
 FLineCount:=0;
 PrevLine;
 Result:=GetCount-i;
 if (e<>FLine__End) then
 begin
  DelCache(i-1);
 end;
end;

end.
