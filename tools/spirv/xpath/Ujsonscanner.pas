{
    This file is part of the Free Component Library

    JSON source lexical scanner
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

{$ifdef fpc}
  {$define UsePChar}
{$endif}

unit Ujsonscanner;

interface

uses SysUtils, Classes,bufstream,Ufpjson;

resourcestring
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d: ''%s''';
  SErrOpenString = 'string exceeds end of line %d';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkIdentifier,            // Any Javascript identifier
    tkComment,
    tkUnknown
    );

  EScannerError = class(EParserError);

  TJSONOption = (joUTF8,joStrict,joComments,joIgnoreTrailingComma);
  TJSONOptions = set of TJSONOption;

Const
  DefaultOptions = [joUTF8];

Type

  TJSONSInFlag=Set of (ifFreeSource,ifPrevChar,ifEOF);

  { TJSONScanner }

  TJSONScanner = class
  private
    FSource:TStream;
    FInFlag:TJSONSInFlag;

    FCurRow,FCurColumn: Integer;
    FCurToken: TJSONToken;
    FCurTokenString: RawBytestring;
    FCurChar:AnsiChar;

    FOptions : TJSONOptions;

    FAddStr:TUtf8AddStr;

    function GetO(AIndex: TJSONOption): Boolean;
    procedure SetO(AIndex: TJSONOption; AValue: Boolean);
  protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string;
      Const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif});overload;
    function DoFetchToken: TJSONToken; inline;
  public
    {$ifdef fpc}
    constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload; deprecated 'use options form instead';
    constructor Create(const Source : RawByteString; AUseUTF8 : Boolean = True); overload; deprecated  'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    {$endif}
    constructor Create(const Source: RawByteString; AOptions: TJSONOptions); overload;
    destructor Destroy; override;
    function FetchToken: TJSONToken;

    function  NextChar:Boolean;
    Procedure StepPrev; inline;

    //property CurLine: RawBytestring read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read FCurColumn;

    property CurToken: TJSONToken read FCurToken;
    property CurTokenString: RawBytestring read FCurTokenString;
    // Use strict JSON: " for strings, object members are strings, not identifiers
    Property Strict : Boolean Index joStrict Read GetO Write SetO ; deprecated 'use options instead';
    // if set to TRUE, then strings will be converted to UTF8 ansistrings, not system codepage ansistrings.
    Property UseUTF8 : Boolean index joUTF8 Read GetO Write SetO; deprecated 'Use options instead';
    // Parsing options
    Property Options : TJSONOptions Read FOptions Write FOptions;
  end;

const
  TokenInfos: array[TJSONToken] of RawBytestring = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    'identifier',
    'comment',
    ''
  );


implementation

{$ifdef fpc}
constructor TJSONScanner.Create(Source : TStream; AUseUTF8 : Boolean = True);

Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(Source,O);
end;

constructor TJSONScanner.Create(const Source : RawByteString; AUseUTF8 : Boolean = True);
Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(Source,O);
end;

constructor TJSONScanner.Create(Source: TStream; AOptions: TJSONOptions);
begin
  FAddStr:=Default(TUtf8AddStr);
  if Source.InheritsFrom(THandleStream) then
  begin
   FSource:=TReadBufStream.Create(Source,4*1024);
   FInFlag:=[ifFreeSource];
  end else
  begin
   FSource:=Source;
  end;

  FOptions:=AOptions;
end;
{$endif}

constructor TJSONScanner.Create(const Source: RawByteString; AOptions: TJSONOptions);
begin
  FAddStr:=Default(TUtf8AddStr);
  FSource:=TStringStream.Create(Source);
  FInFlag:=[ifFreeSource];
  FOptions:=AOptions;
end;

destructor TJSONScanner.Destroy;
begin
  FAddStr.Free;
  if ifFreeSource in FInFlag then
  begin
   FreeAndNil(FSource);
  end;
  Inherited;
end;

function TJSONScanner.NextChar:Boolean;
begin
 if ifEOF in FInFlag then
 begin
  Result:=False;
 end;

 if ifPrevChar in FInFlag then
 begin
  Result:=True;
  Exclude(FInFlag,ifPrevChar);
 end else
 begin
  Result:=FSource.Read(FCurChar,1)=1;
  if Result then
  begin
   Inc(FCurColumn);
  end else
  begin
   FCurToken:=tkEOF;
   Include(FInFlag,ifEOF);
  end;
 end;

end;

Procedure TJSONScanner.StepPrev; inline;
begin
 Include(FInFlag,ifPrevChar);
end;

function TJSONScanner.FetchToken: TJSONToken;
begin
  Result:=DoFetchToken;
end;

procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.Error(const Msg: string;
  const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif});
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TJSONScanner.DoFetchToken: TJSONToken;

 function CheckNextLine:Boolean; inline;
 begin
  Result:=False;
  if not NextChar then Exit;
  case FCurChar of
    #13:begin //next line
         Inc(FCurRow);
         if not NextChar then Exit;
         FCurColumn:=0;
         case FCurChar of
          #10:FCurColumn:=0;
          else
           StepPrev;
         end;
        end;
    #10:begin //next line
         Inc(FCurRow);
         if not NextChar then Exit;
         FCurColumn:=0;
         case FCurChar of
          #13:FCurColumn:=0;
          else
           StepPrev;
         end;
        end;
  end;
  Result:=True;
 end;

var
  it : TJSONToken;
  I : Integer;
  tstart,tcol, u2: Integer;
  C , c2: char;


begin

  if not NextChar then Exit(tkEOF);

  if (FCurRow=0) then
  begin
   FCurRow:=1;
   if (FCurColumn=1) then
   begin
    if (FCurChar=#$EF) then
    begin
     if not NextChar then Exit(tkEOF);
     if (FCurChar=#$BB) then
     begin
      if not NextChar then Exit(tkEOF);
      if (FCurChar=#$BF) then
      begin
       if not NextChar then Exit(tkEOF);
      end;
     end;
    end;
   end;
   FCurColumn:=1;
  end;

  FCurTokenString := '';

  case FCurChar of
    #13:begin //next line
         Result := tkWhitespace;
         Inc(FCurRow);
         if not NextChar then Exit;
         FCurColumn:=0;
         case FCurChar of
          #10:FCurColumn:=0;
          else
           StepPrev;
         end;
        end;
    #10:begin //next line
         Result := tkWhitespace;
         Inc(FCurRow);
         if not NextChar then Exit;
         FCurColumn:=0;
         case FCurChar of
          #13:FCurColumn:=0;
          else
           StepPrev;
         end;
        end;
    #0,#9, ' ':
      begin
       Result := tkWhitespace;
       repeat
        if not CheckNextLine then Exit;
       until not (FCurChar in [#0,#9, ' ']);
       StepPrev;
      end;
    '"','''':
    begin
     C:=FCurChar;
     If (C='''') and (joStrict in Options) then
       Error(SErrInvalidCharacter, [CurRow,CurColumn,FCurChar]);
     if not NextChar then Exit(tkEOF);
     FAddStr.Reset;
     while not (FCurChar in [#0,#13,#10,C]) do
     begin
      if (FCurChar='\') then
      begin
       if not NextChar then
       begin
        Error(SErrOpenString,[FCurRow]);
        Exit(tkEOF);
       end;
       Case FCurChar of
         't' : FAddStr.AddChar(#9);
         'b' : FAddStr.AddChar(#8);
         'n' : FAddStr.AddChar(#10);
         'r' : FAddStr.AddChar(#13);
         'f' : FAddStr.AddChar(#12);
         'u' : begin
                u2:=0;
                For I:=1 to 4 do
                begin
                  if not NextChar then Exit(tkEOF);
                  c2:=FCurChar;
                  Case c2 of
                    '0'..'9': u2:=u2*16+ord(c2)-ord('0');
                    'A'..'F': u2:=u2*16+ord(c2)-ord('A')+10;
                    'a'..'f': u2:=u2*16+ord(c2)-ord('a')+10;
                  else
                    Error(SErrInvalidCharacter, [CurRow,CurColumn,FCurChar]);
                  end;
                end;
                FAddStr.AddWideChar(WideChar(u2));
               end;
       else
         FAddStr.AddChar(FCurChar);
       end;
      end else
      begin
       FAddStr.AddChar(FCurChar);
      end;
      if not NextChar then Exit(tkEOF);
     end;
     if (FCurChar in [#0,#13,#10]) then
       Error(SErrOpenString,[FCurRow]);

     Result:=tkString;
     FCurTokenString:=FAddStr.GetStr;
    end;
    ',':
      begin
        Result := tkComma;
      end;
    '0'..'9','.','-':
      begin
        FAddStr.Reset;
        FAddStr.AddChar(FCurChar);
        while NextChar do
        begin

          case FCurChar of
            '.':
              begin
                FAddStr.AddChar(FCurChar);
                if not NextChar then Break;
                //FAddStr.AddChar(FCurChar);
                if FCurChar in ['0'..'9', 'e', 'E'] then
                begin
                 //if not NextChar then Break;
                 repeat
                  FAddStr.AddChar(FCurChar);
                  if not NextChar then Break;
                 until not (FCurChar in ['0'..'9', 'e', 'E','-','+']);
                end;
                StepPrev;
                break;
              end;
            '0'..'9':FAddStr.AddChar(FCurChar);
            'e', 'E':
              begin
                FAddStr.AddChar(FCurChar);
                if not NextChar then Break;
                FAddStr.AddChar(FCurChar);
                if FCurChar in ['-','+']  then
                begin
                 if not NextChar then Break;
                end;
                while FCurChar in ['0'..'9'] do
                begin
                 FAddStr.AddChar(FCurChar);
                 if not NextChar then Break;
                end;
                StepPrev;
                break;
              end;
          else
            if not (FCurChar in [#0,'}',']',',',#9,' ',#13,#10]) then
               Error(SErrInvalidCharacter, [CurRow,CurColumn,FCurChar]);
            StepPrev;
            break;
          end;
        end;

        FCurTokenString:=FAddStr.GetStr;
        If (FCurTokenString[1]='.') then
          FCurTokenString:='0'+FCurTokenString;
        Result := tkNumber;
      end;
    ':':
      begin
        Result := tkColon;
      end;
    '{':
      begin
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Result := tkCurlyBraceClose;
      end;  
    '[':
      begin
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Result := tkSquaredBraceClose;
      end;
    '/' :
      begin
       if Not (joComments in Options) then
         Error(SErrInvalidCharacter, [CurRow,CurCOlumn,FCurChar]);
       FAddStr.Reset;
       FAddStr.AddChar(FCurChar);
       Case FCurChar of
         '/' : begin
                i:=FCurRow;
                While (i=FCurRow) do
                begin
                 if not CheckNextLine then Break;
                 FAddStr.AddChar(FCurChar);
                end;
                FCurTokenString:=FAddStr.GetStr;
               end;
         '*' :
               begin

                if not CheckNextLine then
                begin
                 Error(SUnterminatedComment, [CurRow,CurCOlumn,FCurChar]);
                 Exit(tkEOF);
                end;

                repeat
                 FAddStr.AddChar(FCurChar);
                 c:=FCurChar;
                 if not CheckNextLine then
                 begin
                  Error(SUnterminatedComment, [CurRow,CurCOlumn,FCurChar]);
                  Break;
                 end;
                until (c='*') and (FCurChar='/');

                FCurTokenString:=FAddStr.GetStr;
               end;
       else
         Error(SErrInvalidCharacter, [CurRow,CurCOlumn,FCurChar]);
       end;
       Result:=tkComment;
      end;
    'a'..'z','A'..'Z','_':
      begin
       FAddStr.Reset;

       tStart:=CurRow;
       tcol  :=CurColumn;

       repeat
        FAddStr.AddChar(FCurChar);
        if not NextChar then Break;
       until not (FCurChar in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
       StepPrev;

       FCurTokenString:=FAddStr.GetStr;

        for it := tkTrue to tkNull do
          if CompareText(FCurTokenString, TokenInfos[it]) = 0 then
            begin
             Result := it;
             FCurToken := Result;
             exit;
            end;

        if (joStrict in Options) then
          Error(SErrInvalidCharacter, [tStart,tcol,FCurTokenString])
        else
          Result:=tkIdentifier;
      end;
  else
    Error(SErrInvalidCharacter, [CurRow,CurColumn,FCurChar]);
  end;

  FCurToken:=Result;
end;

function TJSONScanner.GetO(AIndex: TJSONOption): Boolean;
begin
  Result:=AIndex in FOptions;
end;

procedure TJSONScanner.SetO(AIndex: TJSONOption; AValue: Boolean);
begin
  If AValue then
    Include(Foptions,AIndex)
  else
    Exclude(Foptions,AIndex)
end;

end.
