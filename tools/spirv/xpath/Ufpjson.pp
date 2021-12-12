{
    This file is part of the Free Component Library

    JSON Data structures
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
//modifed by Red_prig
{$mode objfpc}
{$h+}
unit Ufpjson;

interface

uses
  {$ifdef fpc}
  variants,
  {$endif}
  {$ifdef pas2js}
  JS, RTLConsts, Types,
  {$endif}
  SysUtils,
  classes;

type
  TJSONtype = (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject);
  TJSONInstanceType = (
    jitUnknown,
    jitNumberInteger,
    {$ifdef fpc}
    jitNumberInt64,
    jitNumberQWord,
    {$endif}
    jitNumberFloat,
    jitString,
    jitBoolean,
    jitNull,
    jitArray,
    jitObject);
  TJSONFloat = Double;
  TJSONStringType = {$ifdef fpc}UTF8String{$else}string{$endif};
  TJSONUnicodeStringType = Unicodestring;
  {$ifdef fpc}
  TJSONCharType = AnsiChar;
  PJSONCharType = ^TJSONCharType;
  TJSONVariant = variant;
  TFPJSStream = TMemoryStream;
  {$else}
  TJSONCharType = char;
  TJSONVariant = jsvalue;
  TFPJSStream = TJSArray;
  {$endif}
  TFormatOption = (foSingleLineArray,   // Array without CR/LF : all on one line
                   foSingleLineObject,  // Object without CR/LF : all on one line
                   foDoNotQuoteMembers, // Do not quote object member names.
                   foUseTabchar,        // Use tab characters instead of spaces.
                   foSkipWhiteSpace,    // Do not use whitespace at all
                   foSkipWhiteSpaceOnlyLeading   //  When foSkipWhiteSpace is active, skip whitespace for object members only before :
                   );
  TFormatOptions = set of TFormatOption;

Const
  DefaultIndentSize = 2;
  DefaultFormat     = [];
  AsJSONFormat      = [foSingleLineArray,foSingleLineObject]; // These options make FormatJSON behave as AsJSON
  AsCompressedJSON  = [foSingleLineArray,foSingleLineObject,foskipWhiteSpace]; // These options make FormatJSON behave as AsJSON with TJSONData.CompressedJSON=True
  AsCompactJSON     = [foSingleLineArray,foSingleLineObject,foskipWhiteSpace,foDoNotQuoteMembers]; // These options make FormatJSON behave as AsJSON with TJSONData.CompressedJSON=True and TJSONObject.UnquotedMemberNames=True
  ValueJSONTypes    = [jtNumber, jtString, jtBoolean, jtNull];
  ActualValueJSONTypes = ValueJSONTypes - [jtNull];
  StructuredJSONTypes  = [jtArray,jtObject];

Type

  TJSONNumberType = (
    ntFloat,
    ntInteger
    {$ifdef fpc}
    ,ntInt64
    ,ntQWord
    {$endif}
    );

type
 TAddStr=object
  FStr:PChar;
  FLen:SizeInt;
  Procedure AddStr(Const S:RawByteString);
  Procedure AddChar(C:AnsiChar);
  Procedure Reset; inline;
  Procedure Free;  inline;
  function  GetStr:RawByteString; inline;
 end;
 TUtf8AddStr=object(TAddStr)
  FSP:SizeUInt;
  Procedure AddChar(C:AnsiChar); inline;
  Procedure AddWideChar(C:WideChar);
  Procedure Reset; inline;
 end;

Function  StringToJSONString(const S : TJSONStringType;Strict:Boolean=False):TJSONStringType; inline;
procedure _StringToJSONString(Var FAddStr:TAddStr;const S:TJSONStringType;Strict:Boolean=False); inline;
procedure __StringToJSONString(Var FAddStr:TAddStr;P:PJSONCharType;Len:SizeInt;Strict:Boolean=False);
Function  JSONStringToString(const S :TJSONStringType):TJSONStringType; inline;
procedure _JSONStringToString(Var FAddStr:TUtf8AddStr;const S:TJSONStringType); inline;
procedure __JSONStringToString(Var FAddStr:TUtf8AddStr;P:PJSONCharType;Len:SizeInt);
Function  JSONTypeName(JSONType:TJSONType):String;

implementation

Uses typinfo;

Procedure TAddStr.AddStr(Const S:RawByteString);
Var
 i:SizeInt;
begin
 if Length(S)>0 then
 For i:=1 to Length(S) do
  AddChar(S[i]);
end;

Procedure TAddStr.AddChar(C:AnsiChar);
Var
 i,MemLen:SizeInt;
begin

 if (FStr=nil) then
 begin
  MemLen:=0
 end else
 begin
  MemLen:=MemSize(FStr);
 end;

 i:=FLen;
 FLen:=FLen+1;
 if (MemLen<FLen) then
 begin
  Case FLen of
   0..SizeOf(Pointer)*4:
    FStr:=ReAllocMem(FStr,SizeOf(Pointer)*4);
   else
   begin
    FStr:=ReAllocMem(FStr,i+(i div 2));
   end;
  end;
 end;
 FStr[i]:=C;

end;

Procedure TUtf8AddStr.AddChar(C:AnsiChar); inline;
begin
 FSP:=0;
 inherited;
end;

Procedure TAddStr.Reset; inline;
begin
 FLen:=0;
end;

Procedure TAddStr.Free; inline;
begin
 FreeMem(FStr);
end;

function TAddStr.GetStr:RawByteString; inline;
begin
 SetLength(Result,FLen);
 Move(FStr^,Result[1],FLen);
end;

Procedure TUtf8AddStr.AddWideChar(C:WideChar);
Var
 lw:longword;
begin
 lw:=Ord(C);
 if FSP<>0 then
 begin
  case lw of
    $dc00..$dfff:
    {High Surrogates 2}
    begin
     { $d7c0 is ($d800 - ($10000 shr 10)) }
     lw:=(longword(FSP-$d7c0) shl 10) + (lw xor $dc00);
     inherited AddChar(AnsiChar($f0 or (lw shr 18)));
     inherited AddChar(AnsiChar($80 or ((lw shr 12) and $3f)));
     inherited AddChar(AnsiChar($80 or ((lw shr 6) and $3f)));
     inherited AddChar(AnsiChar($80 or (lw and $3f)));
    end;
  end;
  FSP:=0;
 end else
 begin
  case lw of
    0..$7f:
    begin
     inherited AddChar(AnsiChar(lw));
    end;
    $80..$7ff:
    begin
     inherited AddChar(AnsiChar($c0 or (lw shr 6)));
     inherited AddChar(AnsiChar($80 or (lw and $3f)));
    end;
    $800..$d7ff,$e000..$ffff:
    begin
     inherited AddChar(AnsiChar($e0 or (lw shr 12)));
     inherited AddChar(AnsiChar($80 or ((lw shr 6) and $3f)));
     inherited AddChar(AnsiChar($80 or (lw and $3f)));
    end;
    $d800..$dbff:
    {High Surrogates 1}
    begin
     FSP:=lw;
    end;
  end;
 end;
end;

Procedure TUtf8AddStr.Reset; inline;
begin
 inherited;
 FSP:=0;
end;

function StringToJSONString(const S:TJSONStringType;Strict:Boolean=False):TJSONStringType; inline;
Var
 FAddStr:TAddStr;
begin
 FAddStr:=Default(TAddStr);
 _StringToJSONString(FAddStr,S,Strict);
 Result:=FAddStr.GetStr;
 FAddStr.Free;
end;

procedure _StringToJSONString(Var FAddStr:TAddStr;const S:TJSONStringType;Strict:Boolean=False); inline;
begin
 __StringToJSONString(FAddStr,PJSONCharType(S),Length(S),Strict);
end;

procedure __StringToJSONString(Var FAddStr:TAddStr;P:PJSONCharType;Len:SizeInt;Strict:Boolean=False);
Var
 I:SizeInt;
 C,T:AnsiChar;
begin
 I:=0;
 if Strict then T:='/' else T:=#0;
 While (I<Len) do
 begin
  C:=AnsiChar(P^);
  if (C in ['"',T,'\',#0..#31]) then
  begin
   FAddStr.AddChar('\');
   Case C of
     '\',
     '/',
     '"' : FAddStr.AddChar(C);
     #8  : FAddStr.AddChar('b');
     #9  : FAddStr.AddChar('t');
     #10 : FAddStr.AddChar('n');
     #12 : FAddStr.AddChar('f');
     #13 : FAddStr.AddChar('r');
   else
    begin
     FAddStr.AddChar('u');
     FAddStr.AddStr(HexStr(Ord(C),4));
    end;
   end;
  end else
  begin
   FAddStr.AddChar(c);
  end;
  Inc(I);
  Inc(P);
 end;
end;

Function JSONStringToString(const S:TJSONStringType):TJSONStringType; inline;
Var
 FAddStr:TUtf8AddStr;
begin
 FAddStr:=Default(TUtf8AddStr);
 _JSONStringToString(FAddStr,S);
 Result:=FAddStr.GetStr;
 FAddStr.Free;
end;

procedure _JSONStringToString(Var FAddStr:TUtf8AddStr;const S:TJSONStringType); inline;
begin
 __JSONStringToString(FAddStr,PJSONCharType(S),Length(S));
end;

procedure __JSONStringToString(Var FAddStr:TUtf8AddStr;P:PJSONCharType;Len:SizeInt);
Const
 DifLo=Byte('a')-$A;
 DifHi=Byte('A')-$A;
Var
 I,State:SizeInt;
 w:Word;
begin
 State:=0;
 I:=0;
 While (I<Len) do
 begin
  Case State of
   0:begin
      if (P^='\') then
      begin
       State:=1;
      end else
      begin
       FAddStr.AddChar(P^);
      end;
     end;
   1:begin
      Case P^ of
       'b':FAddStr.AddChar(#8);
       't':FAddStr.AddChar(#9);
       'n':FAddStr.AddChar(#10);
       'f':FAddStr.AddChar(#12);
       'r':FAddStr.AddChar(#13);
       'u':begin
            State:=2;
            w:=0;
            Inc(I);
            Inc(P);
            Continue;
           end;
       else
           FAddStr.AddChar(P^);
      end;
      State:=0;
     end;
   2..5:
     begin
      Case P^ of
       '0'..'9':w:=(w shl 4) or (PByte(P)^ and $F);
       'a'..'f':w:=(w shl 4) or (PByte(P)^-DifLo);
       'A'..'F':w:=(w shl 4) or (PByte(P)^-DifHi);
       else     w:=(w shl 4);
      end;
      Inc(State);
      if (State=6) then
      begin
       FAddStr.AddWideChar(WideChar(W));
       State:=0;
      end;
     end;
  end;
  Inc(I);
  Inc(P);
 end;
end;

function JSONTypeName(JSONType: TJSONType): String;
begin
  Result:=GetEnumName(TypeInfo(TJSONType),Ord(JSONType));
end;

end.

