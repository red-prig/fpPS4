unit srLiteral;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 Half16,
 srType,
 ginodes,
 srNode;

type
 PsrLiteralKey=^TsrLiteralKey;
 TsrLiteralKey=record
  ntype:TsrNodeType;
  dtype:TsrDataType;
  Value:PtrUint;
  name :PChar;
 end;

 TsrLiteral=class(TsrNode)
  public
   pLeft,pRight:TsrLiteral;
  public
   key:TsrLiteralKey;
   //
   function  _GetPrintData:RawByteString;    override;
   function  _GetData(data:Pointer):Ptruint; override;
   //
   class function c(n1,n2:PsrLiteralKey):Integer; static;
   property  Value:PtrUint read key.Value;
   Function  AsBool:Boolean;   inline;
   Function  AsUint8:Byte;     inline;
   Function  AsInt8:Shortint;  inline;
   Function  AsUint16:Word;    inline;
   Function  AsInt16:Smallint; inline;
   Function  AsUint32:DWORD;   inline;
   function  AsInt32:Integer;  inline;
   Function  AsUint64:QWORD;   inline;
   Function  AsInt64:Int64;    inline;
   Function  AsHalf16:THalf16; inline;
   Function  AsFloat32:Single; inline;
   Function  AsFloat64:Double; inline;
   Function  GetPrintData:RawByteString;
   Function  GetData(data:Pointer):Ptruint;
 end;

 ntLiteral=TsrLiteral;

 TsrLiteralString=class(TsrLiteral)
  //
  function  _GetPrintData:RawByteString;    override;
  function  _GetData(data:Pointer):Ptruint; override;
  //
  Function  GetPrintData:RawByteString;
  Function  GetData(data:Pointer):Ptruint;
 end;

 ntLiteralString=TsrLiteralString;

 TsrLiteralConst=class(TsrLiteral)
  function  _GetPrintData:RawByteString;    override;
  function  _GetData(data:Pointer):Ptruint; override;
  //
  Function  GetPrintData:RawByteString;
  Function  GetData(data:Pointer):Ptruint;
 end;

 ntLiteralConst=TsrLiteralConst;

 PsrLiteralList=^TsrLiteralList;
 TsrLiteralList=object
  type
   TNodeTree=specialize TNodeTreeClass<TsrLiteral>;
  var
   FEmit:TCustomEmit;
   FTree:TNodeTree;
  Procedure Init(Emit:TCustomEmit);
  function  FetchLiteral(Value:PtrUint;name:PChar):TsrLiteral;
  function  FetchString(name:PChar):TsrLiteralString;
  function  FetchConst(dtype:TsrDataType;Value:PtrUint):TsrLiteralConst;
 end;


implementation

function TsrLiteral._GetPrintData:RawByteString;
begin
 Result:=GetPrintData;
end;

function TsrLiteral._GetData(data:Pointer):Ptruint;
begin
 Result:=GetData(data);
end;

//

function TsrLiteralString._GetPrintData:RawByteString;
begin
 Result:=GetPrintData;
end;

function TsrLiteralString._GetData(data:Pointer):Ptruint;
begin
 Result:=GetData(data);
end;

//

function TsrLiteralConst._GetPrintData:RawByteString;
begin
 Result:=GetPrintData;
end;

function TsrLiteralConst._GetData(data:Pointer):Ptruint;
begin
 Result:=GetData(data);
end;

//

class function TsrLiteral.c(n1,n2:PsrLiteralKey):Integer;
begin
 Result:=ord(ptruint(n1^.ntype)>ptruint(n2^.ntype))-ord(ptruint(n1^.ntype)<ptruint(n2^.ntype));
 if (Result<>0) then Exit;
 //
 Result:=ord(ord(n1^.dtype)>ord(n2^.dtype))-ord(ord(n1^.dtype)<ord(n2^.dtype));
 if (Result<>0) then Exit;
 //
 Result:=ord(n1^.Value>n2^.Value)-ord(n1^.Value<n2^.Value);
 if (Result<>0) then Exit;
 //
 Result:=ComparePChar(n1^.name,n2^.name);
end;

//

Function TsrLiteral.AsBool:Boolean; inline;
begin
 Assert(Self<>nil);
 Result:=PBoolean(@key.Value)^;
end;

Function TsrLiteral.AsUint8:Byte; inline;
begin
 Assert(Self<>nil);
 Result:=PByte(@key.Value)^;
end;

Function TsrLiteral.AsInt8:Shortint; inline;
begin
 Assert(Self<>nil);
 Result:=PShortint(@key.Value)^;
end;

Function TsrLiteral.AsUint16:Word; inline;
begin
 Assert(Self<>nil);
 Result:=PWord(@key.Value)^;
end;

Function TsrLiteral.AsInt16:Smallint; inline;
begin
 Assert(Self<>nil);
 Result:=PSmallint(@key.Value)^;
end;

Function TsrLiteral.AsUint32:DWORD; inline;
begin
 Assert(Self<>nil);
 Result:=PDWORD(@key.Value)^;
end;

Function TsrLiteral.AsInt32:Integer; inline;
begin
 Assert(Self<>nil);
 Result:=PInteger(@key.Value)^;
end;

Function TsrLiteral.AsUint64:QWORD; inline;
begin
 Assert(Self<>nil);
 Result:=key.Value;
end;

Function TsrLiteral.AsInt64:Int64; inline;
begin
 Assert(Self<>nil);
 Result:=Int64(key.Value);
end;

Function TsrLiteral.AsHalf16:THalf16; inline;
begin
 Assert(Self<>nil);
 Result:=PHalf16(@key.Value)^
end;

Function TsrLiteral.AsFloat32:Single; inline;
begin
 Assert(Self<>nil);
 Result:=PSingle(@key.Value)^;
end;

Function TsrLiteral.AsFloat64:Double; inline;
begin
 Assert(Self<>nil);
 Result:=PDouble(@key.Value)^;
end;

Function TsrLiteral.GetPrintData:RawByteString;
begin
 if (key.name=nil) or (StrLen(key.name)=0) then
 begin
  Result:=IntToStr(key.Value);
 end else
 begin
  Result:=key.name;
 end;
end;

Function TsrLiteral.GetData(data:Pointer):Ptruint;
begin
 Result:=SizeOf(DWORD);
 if (data<>nil) then
 begin
  PDWORD(data)^:=DWORD(key.Value);
 end;
end;

//

Function TsrLiteralString.GetPrintData:RawByteString;
begin
 Result:='"'+key.name+'"';
end;

Function TsrLiteralString.GetData(data:Pointer):Ptruint;
begin
 Result:=StrLen(key.name)+1;
 if (data<>nil) then
 begin
  Move(key.name^,data^,Result);
 end;
end;

//

const
 DefaultFormatSettings : TFormatSettings = (
   CurrencyFormat: 1;
   NegCurrFormat: 5;
   ThousandSeparator: ',';
   DecimalSeparator: '.';
   CurrencyDecimals: 2;
   DateSeparator: '-';
   TimeSeparator: ':';
   ListSeparator: ',';
   CurrencyString: '$';
   ShortDateFormat: '';
   LongDateFormat: '';
   TimeAMString: '';
   TimePMString: '';
   ShortTimeFormat: '';
   LongTimeFormat : '';
   ShortMonthNames:('','','','','','','','','','','','');
   LongMonthNames :('','','','','','','','','','','','');
   ShortDayNames  :('','','','','','','');
   LongDayNames   :('','','','','','','');
   TwoDigitYearCenturyWindow: 50;
 );

Function TsrLiteralConst.GetPrintData:RawByteString;
begin
 Result:='';
 case key.dtype of
  dtUnknow :Result:='0x'+HexStr(key.Value,4);

  dtBool   :
   Case AsBool of
    true :Result:='true';
    false:Result:='false';
   end;

  dtHalf16 : Result:=FloatToStr(Single(AsHalf16),DefaultFormatSettings);
  dtFloat32: Result:=FloatToStr(       AsFloat32,DefaultFormatSettings);
  dtFloat64: Result:=FloatToStr(       AsFloat64,DefaultFormatSettings);

  dtInt8   : Result:=IntToStr(AsInt8);
  dtUint8  : Result:=IntToStr(AsUint8);

  dtInt16  : Result:=IntToStr(AsInt16);
  dtUint16 : Result:=IntToStr(AsUint16);

  dtInt32  : Result:=IntToStr(AsInt32);
  dtUint32 : Result:=IntToStr(AsUint32);

  dtInt64  : Result:=IntToStr(AsInt64);
  dtUint64 : Result:=IntToStr(AsUint64);
  else;
 end;
end;

Function TsrLiteralConst.GetData(data:Pointer):Ptruint;
begin
 Result:=0;
 Case key.dtype.BitSize of
    8:begin
       Result:=SizeOf(BYTE);
       if (data<>nil) then
       begin
        PBYTE(data)^:=BYTE(key.Value);
       end;
      end;
   16:begin
       Result:=SizeOf(WORD);
       if (data<>nil) then
       begin
        PWORD(data)^:=WORD(key.Value);
       end;
      end;
   32:begin
       Result:=SizeOf(DWORD);
       if (data<>nil) then
       begin
        PDWORD(data)^:=DWORD(key.Value);
       end;
      end;
   64:begin
       Result:=SizeOf(QWORD);
       if (data<>nil) then
       begin
        PQWORD(data)^:=key.Value;
       end;
      end;
  else;
 end;
end;

//

Procedure TsrLiteralList.Init(Emit:TCustomEmit);
begin
 FEmit:=Emit;
end;

function TsrLiteralList.FetchLiteral(Value:PtrUint;name:PChar):TsrLiteral;
var
 key:TsrLiteralKey;
 size:ptrint;
begin
 key:=Default(TsrLiteralKey);
 key.ntype:=ntLiteral;
 key.dtype:=dtUint32;
 key.Value:=Value;
 key.name :=name;

 Result:=FTree.Find(@key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrLiteral>;
  Result.key:=key;

  if (name<>nil) then
  begin
   size:=StrLen(name)+1;
   Result.key.name:=FEmit.Alloc(size);
   Move(name^,Result.key.name^,size);
  end;

  FTree.Insert(Result);
 end;
end;

function TsrLiteralList.FetchString(name:PChar):TsrLiteralString;
var
 key:TsrLiteralKey;
 size_o:ptrint;
 size_a:ptrint;
begin
 key:=Default(TsrLiteralKey);
 key.ntype:=ntLiteralString;
 key.dtype:=dtString;
 key.name :=name;

 Result:=TsrLiteralString(FTree.Find(@key));
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrLiteralString>;
  Result.key:=key;

  if (name<>nil) then
  begin
   size_o:=StrLen(name);
   size_a:=Align(size_o+1,SizeOf(DWORD)); //align DW

   Result.key.name:=FEmit.Alloc(size_a);
   Move(name^,Result.key.name^,size_o);
  end else
  begin
   Result.key.name:=FEmit.Alloc(SizeOf(DWORD));
   //space DW
  end;

  FTree.Insert(Result);
 end;
end;

function TsrLiteralList.FetchConst(dtype:TsrDataType;Value:PtrUint):TsrLiteralConst;
var
 key:TsrLiteralKey;
begin
 key:=Default(TsrLiteralKey);
 key.ntype:=ntLiteralConst;
 key.dtype:=dtype;
 key.Value:=Value;

 Result:=TsrLiteralConst(FTree.Find(@key));
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrLiteralConst>;
  Result.key:=key;

  FTree.Insert(Result);
 end;
end;

end.

