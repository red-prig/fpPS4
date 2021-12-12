{
    This file is part of the Free Component Library

    JSON SAX-like Reader
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
unit Ujsonreader;

interface

uses
  Classes, SysUtils, UfpJSON, Ujsonscanner;
  
Type

  { TBaseJSONReader }

  TBaseJSONReader = Class(TObject)
  Private
    FScanner : TJSONScanner;
    function GetO(AIndex: TJSONOption): Boolean;
    function GetOptions: TJSONOptions; inline;
    procedure SetO(AIndex: TJSONOption; AValue: Boolean);
    procedure SetOptions(AValue: TJSONOptions);
  Protected
    procedure DoError(const Msg: String);
    Procedure DoParse(AtCurrent,AllowEOF: Boolean);
    function GetNextToken: TJSONToken;
    function CurrentTokenString: RawByteString;
    function CurrentToken: TJSONToken; inline;

    Procedure KeyValue(Const AKey : TJSONStringType); virtual; abstract;
    Procedure StringValue(Const AValue : TJSONStringType);virtual; abstract;
    Procedure NullValue; virtual; abstract;
    Procedure FloatValue(Const AValue : Double); virtual; abstract;
    Procedure BooleanValue(Const AValue : Boolean); virtual; abstract;
    Procedure NumberValue(Const AValue : TJSONStringType); virtual; abstract;
    Procedure IntegerValue(Const AValue : integer); virtual; abstract;
    Procedure Int64Value(Const AValue : int64); virtual; abstract;
    Procedure QWordValue(Const AValue : QWord); virtual; abstract;
    Procedure StartArray; virtual; abstract;
    Procedure StartObject; virtual; abstract;
    Procedure EndArray; virtual; abstract;
    Procedure EndObject; virtual; abstract;

    Procedure ParseArray;
    Procedure ParseObject;
    Procedure ParseNumber;
    Procedure DoExecute;
    Property Scanner : TJSONScanner read FScanner;
  Public
    Constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload;deprecated 'use options form instead';
    Constructor Create(Source : TJSONStringType; AUseUTF8 : Boolean = True); overload;deprecated 'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    constructor Create(const Source: RawByteString; AOptions: TJSONOptions); overload;
    destructor Destroy();override;
    // Parsing options
    Property Options : TJSONOptions Read GetOptions Write SetOptions;
  end;

  EJSONParser = Class(EParserError);
  
implementation

Resourcestring
  SErrUnexpectedEOF   = 'Unexpected EOF encountered.';
  SErrUnexpectedToken = 'Unexpected token (%s) encountered.';
  SErrExpectedColon   = 'Expected colon (:), got token "%s".';
  //SErrEmptyElement = 'Empty element encountered.';
  SErrExpectedElementName    = 'Expected element name, got token "%s"';
  SExpectedCommaorBraceClose = 'Expected , or ], got token "%s".';
  SErrInvalidNumber          = 'Number is not an integer or real number: %s';
  SErrNoScanner = 'No scanner. No source specified ?';
  
{ TBaseJSONReader }


Procedure TBaseJSONReader.DoExecute;

begin
  if (FScanner=Nil) then
    DoError(SErrNoScanner);
  DoParse(False,True);
end;

{
  Consume next token and convert to JSON data structure.
  If AtCurrent is true, the current token is used. If false,
  a token is gotten from the scanner.
  If AllowEOF is false, encountering a tkEOF will result in an exception.
}

function TBaseJSONReader.CurrentToken: TJSONToken;

begin
  Result:=FScanner.CurToken;
end;

function TBaseJSONReader.CurrentTokenString: RawByteString;

begin
  If CurrentToken in [tkString,tkIdentifier,tkNumber,tkComment] then
    Result:=FScanner.CurTokenString
  else
    Result:=TokenInfos[CurrentToken];
end;

procedure TBaseJSONReader.DoParse(AtCurrent, AllowEOF: Boolean);

var
  T : TJSONToken;
  
begin
  If not AtCurrent then
    T:=GetNextToken
  else
    T:=FScanner.CurToken;
  Case T of
    tkEof : If Not AllowEof then
              DoError(SErrUnexpectedEOF);
    tkNull  : NullValue;
    tkTrue,
    tkFalse : BooleanValue(t=tkTrue);
    tkString : if (joUTF8 in Options) and (DefaultSystemCodePage<>CP_UTF8) then
                 StringValue(TJSONStringType(UTF8Decode(CurrentTokenString)))
               else
                 StringValue(CurrentTokenString);
    tkCurlyBraceOpen :
        ParseObject;
    tkCurlyBraceClose :
        DoError(SErrUnexpectedToken);
    tkSQuaredBraceOpen :
        ParseArray;
    tkSQuaredBraceClose :
        DoError(SErrUnexpectedToken);
    tkNumber :
        ParseNumber;
    tkComma :
        DoError(SErrUnexpectedToken);
    tkIdentifier :
        DoError(SErrUnexpectedToken);
  end;
end;


// Creates the correct JSON number type, based on the current token.
procedure TBaseJSONReader.ParseNumber;

Var
  I : Integer;
  I64 : Int64;
  QW  : QWord;
  F : TJSONFloat;
  S : String;

begin
  S:=CurrentTokenString;
  NumberValue(S);
  I:=0;
  if TryStrToQWord(S,QW) then
    begin
    if QW>qword(high(Int64)) then
      QWordValue(QW)
    else
      if QW>MaxInt then
      begin
        I64 := QW;
        Int64Value(I64);
      end
      else
      begin
        I:=QW;
        IntegerValue(I);
      end
    end
  else
    begin
    If TryStrToInt64(S,I64) then
      if (I64>Maxint) or (I64<-MaxInt) then
        Int64Value(I64)
      Else
        begin
        I:=I64;
        IntegerValue(I);
        end
    else
      begin
      I:=0;
      Val(S,F,I);
      If (I<>0) then
        DoError(SErrInvalidNumber);
      FloatValue(F);
      end;
    end;
end;

function TBaseJSONReader.GetO(AIndex: TJSONOption): Boolean;
begin
  Result:=AIndex in Options;
end;

function TBaseJSONReader.GetOptions: TJSONOptions;
begin
  Result:=FScanner.Options
end;

procedure TBaseJSONReader.SetO(AIndex: TJSONOption; AValue: Boolean);
begin
  if aValue then
    FScanner.Options:=FScanner.Options+[AINdex]
  else
    FScanner.Options:=FScanner.Options-[AINdex]
end;

procedure TBaseJSONReader.SetOptions(AValue: TJSONOptions);
begin
  FScanner.Options:=AValue;
end;


// Current token is {, on exit current token is }
Procedure TBaseJSONReader.ParseObject;

Var
  T : TJSONtoken;
  LastComma : Boolean;

begin
  LastComma:=False;
  StartObject;
  T:=GetNextToken;
  While T<>tkCurlyBraceClose do
    begin
    If (T<>tkString) and (T<>tkIdentifier) then
      DoError(SErrExpectedElementName);
    KeyValue(CurrentTokenString);
    T:=GetNextToken;
    If (T<>tkColon) then
      DoError(SErrExpectedColon);
    DoParse(False,False);
    T:=GetNextToken;
    If Not (T in [tkComma,tkCurlyBraceClose]) then
      DoError(SExpectedCommaorBraceClose);
    If T=tkComma then
      begin
      T:=GetNextToken;
      LastComma:=(t=tkCurlyBraceClose);
      end;
    end;
  If LastComma and ((joStrict in Options) or not (joIgnoreTrailingComma in Options))  then // Test for ,} case
    DoError(SErrUnExpectedToken);
  EndObject;
end;

// Current token is [, on exit current token is ]
Procedure TBaseJSONReader.ParseArray;

Var
  T : TJSONtoken;
  LastComma : Boolean;
  S : TJSONOPTions;

begin
  StartArray;
  LastComma:=False;
  Repeat
    T:=GetNextToken;
    If (T<>tkSquaredBraceClose) then
      begin
      DoParse(True,False);
      T:=GetNextToken;
      If Not (T in [tkComma,tkSquaredBraceClose]) then
        DoError(SExpectedCommaorBraceClose);
      LastComma:=(t=TkComma);
      end;
  Until (T=tkSquaredBraceClose);
  S:=Options;
  If LastComma and ((joStrict in S) or not (joIgnoreTrailingComma in S))  then // Test for ,] case
    DoError(SErrUnExpectedToken);
  EndArray;
end;

// Get next token, discarding whitespace
function TBaseJSONReader.GetNextToken: TJSONToken;

begin
  Repeat
    Result:=FScanner.FetchToken;
  Until (Not (Result in [tkComment,tkWhiteSpace]));
end;

procedure TBaseJSONReader.DoError(const Msg: String);

Var
  S : String;

begin
  S:=Format(Msg,[CurrentTokenString]);
  S:=Format('Error at line %d, Pos %d:',[FScanner.CurRow,FSCanner.CurColumn])+S;
  Raise EJSONParser.Create(S);
end;

constructor TBaseJSONReader.Create(Source: TStream; AUseUTF8 : Boolean = True);
begin
  Inherited Create;
  FScanner:=TJSONScanner.Create(Source,[joUTF8]);
  if AUseUTF8 then
   Options:=Options + [joUTF8];
end;

constructor TBaseJSONReader.Create(Source: TJSONStringType; AUseUTF8 : Boolean = True);
begin
  Inherited Create;
  FScanner:=TJSONScanner.Create(Source,[joUTF8]);
  if AUseUTF8 then
   Options:=Options + [joUTF8];
end;

constructor TBaseJSONReader.Create(Source: TStream; AOptions: TJSONOptions);
begin
  FScanner:=TJSONScanner.Create(Source,AOptions);
end;

constructor TBaseJSONReader.Create(const Source: RawByteString; AOptions: TJSONOptions);
begin
  FScanner:=TJSONScanner.Create(Source,AOptions);
end;

destructor TBaseJSONReader.Destroy();
begin
  FreeAndNil(FScanner);
  inherited Destroy();
end;


end.

