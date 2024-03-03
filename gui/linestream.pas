unit LineStream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
 TLineStream = Class
  FStream   :TStream;
  FLineStart:Int64;
  FLine__End:Int64;
  FLineIndex:Integer;
  FLineCount:Integer;
  Constructor Create(Stream:TStream);
  procedure   Reset;
  function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
  Function    ReadForward (var ABuffer; ACount : LongInt) : Integer;
  Function    ReadBackward(var ABuffer; ACount : LongInt) : Integer;
  Function    GetLine:RawByteString;
  Function    NextLine:Boolean;
  Function    PrevLine:Boolean;
  function    GetCount: integer;
  function    Get(Index: integer): RawByteString;
  function    Update:Integer;
 end;

implementation

Constructor TLineStream.Create(Stream:TStream);
begin
 FStream:=Stream;
 Reset;
end;

procedure TLineStream.Reset;
begin
 Seek(0,soBeginning);
 FLineStart:=-1;
 FLine__End:=0;
 FLineIndex:=-1;
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

 f:=FLineStart;

 //read prev
 if (FLineIndex>=0) then
 begin
  Seek(-1,soCurrent);
  i:=ReadForward(fv,1);
  if (i<=0) then Exit(False);
 end;

 if (FLineIndex<0) or (fv=#13) or (fv=#10) then
 begin
  FLineStart:=FStream.Position;
 end;

 repeat
  i:=ReadForward(ch,1);

  if (i<=0) then
  begin
   if (FLineStart=f) then Exit(False);

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
   Reset;
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

function TLineStream.Get(Index: integer): RawByteString;
label
 _next;
begin
 Result:='';

 if (Index<0) then Exit;

 if (FLineIndex=Index) then
 begin
  Exit(GetLine);
 end;

 if (FLineIndex>Index) then
 begin
  if (Index<=(FLineIndex div 2)) then
  begin
   reset;
   goto _next;
  end;

  while PrevLine do
  begin
   if (FLineIndex=Index) then
   begin
    Exit(GetLine);
   end;
  end;
 end else
 begin
  _next:
  while NextLine do
  begin
   if (FLineIndex=Index) then
   begin
    Exit(GetLine);
   end;
  end;
 end;

end;

function TLineStream.Update:Integer;
begin
 Result:=GetCount;
 FLineCount:=0;
 PrevLine;
 Result:=GetCount-Result;
end;

end.

