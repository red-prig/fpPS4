unit srLiteral;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 half16,
 srType,
 ginodes,
 srNode;

type
 ntLiteral=class(TsrNodeVmt)
  class function GetPrintData(node:PsrNode):RawByteString;   override;
  class function GetData(node:PsrNode;data:Pointer):Ptruint; override;
 end;

 ntLiteralString=class(ntLiteral)
  class function GetPrintData(node:PsrNode):RawByteString;   override;
  class function GetData(node:PsrNode;data:Pointer):Ptruint; override;
 end;

 ntLiteralConst=class(ntLiteral)
  class function GetPrintData(node:PsrNode):RawByteString;   override;
  class function GetData(node:PsrNode;data:Pointer):Ptruint; override;
 end;

 PsrLiteral=^TsrLiteral;
 TsrLiteral=object(TsrNode)
  private
   pLeft,pRight:PsrLiteral;
  public
   dtype:TsrDataType;
   Value:PtrUint;
   name:PChar;
   function  c(n1,n2:PsrLiteral):Integer; static;
   Procedure Init; inline;
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

 PsrLiteralString=^TsrLiteralString;
 TsrLiteralString=object(TsrLiteral)
  Procedure Init; inline;
  Function  GetPrintData:RawByteString;
  Function  GetData(data:Pointer):Ptruint;
 end;

 PsrLiteralConst=^TsrLiteralConst;
 TsrLiteralConst=object(TsrLiteral)
  Procedure Init; inline;
  Function  GetPrintData:RawByteString;
  Function  GetData(data:Pointer):Ptruint;
 end;

 PsrLiteralList=^TsrLiteralList;
 TsrLiteralList=object
  type
   TNodeFetch=specialize TNodeFetch<PsrLiteral,TsrLiteral>;
  var
   FEmit:TCustomEmit;
   FNTree:TNodeFetch;
  Procedure Init(Emit:TCustomEmit);
  function  FetchLiteral(Value:PtrUint;name:PChar):PsrLiteral;
  function  FetchString(name:PChar):PsrLiteralString;
  function  FetchConst(dtype:TsrDataType;Value:PtrUint):PsrLiteralConst;
 end;


implementation

class function ntLiteral.GetPrintData(node:PsrNode):RawByteString;
begin
 Result:=PsrLiteral(node)^.GetPrintData;
end;

class function ntLiteral.GetData(node:PsrNode;data:Pointer):Ptruint;
begin
 Result:=PsrLiteral(node)^.GetData(data);
end;

//

class function ntLiteralString.GetPrintData(node:PsrNode):RawByteString;
begin
 Result:=PsrLiteralString(node)^.GetPrintData;
end;

class function ntLiteralString.GetData(node:PsrNode;data:Pointer):Ptruint;
begin
 Result:=PsrLiteralString(node)^.GetData(data);
end;

//

class function ntLiteralConst.GetPrintData(node:PsrNode):RawByteString;
begin
 Result:=PsrLiteralConst(node)^.GetPrintData;
end;

class function ntLiteralConst.GetData(node:PsrNode;data:Pointer):Ptruint;
begin
 Result:=PsrLiteralConst(node)^.GetData(data);
end;

//

function TsrLiteral.c(n1,n2:PsrLiteral):Integer;
begin
 Result:=ord(ptruint(n1^.fntype)>ptruint(n2^.fntype))-ord(ptruint(n1^.fntype)<ptruint(n2^.fntype));
 if (Result<>0) then Exit;
 //
 Result:=Integer(ord(n1^.dtype)>ord(n2^.dtype))-Integer(ord(n1^.dtype)<ord(n2^.dtype));
 if (Result<>0) then Exit;
 //
 Result:=Integer(n1^.Value>n2^.Value)-Integer(n1^.Value<n2^.Value);
 if (Result<>0) then Exit;
 //
 Result:=ComparePChar(n1^.name,n2^.name);
end;

//

Procedure TsrLiteral.Init; inline;
begin
 fntype:=ntLiteral;
end;

Function TsrLiteral.AsBool:Boolean; inline;
begin
 Assert(@Self<>nil);
 Result:=PBoolean(@Value)^;
end;

Function TsrLiteral.AsUint8:Byte; inline;
begin
 Assert(@Self<>nil);
 Result:=PByte(@Value)^;
end;

Function TsrLiteral.AsInt8:Shortint; inline;
begin
 Assert(@Self<>nil);
 Result:=PShortint(@Value)^;
end;

Function TsrLiteral.AsUint16:Word; inline;
begin
 Assert(@Self<>nil);
 Result:=PWord(@Value)^;
end;

Function TsrLiteral.AsInt16:Smallint; inline;
begin
 Assert(@Self<>nil);
 Result:=PSmallint(@Value)^;
end;

Function TsrLiteral.AsUint32:DWORD; inline;
begin
 Assert(@Self<>nil);
 Result:=PDWORD(@Value)^;
end;

Function TsrLiteral.AsInt32:Integer; inline;
begin
 Assert(@Self<>nil);
 Result:=PInteger(@Value)^;
end;

Function TsrLiteral.AsUint64:QWORD; inline;
begin
 Assert(@Self<>nil);
 Result:=Value;
end;

Function TsrLiteral.AsInt64:Int64; inline;
begin
 Assert(@Self<>nil);
 Result:=Int64(Value);
end;

Function TsrLiteral.AsHalf16:THalf16; inline;
begin
 Assert(@Self<>nil);
 Result:=PHalf16(@Value)^
end;

Function TsrLiteral.AsFloat32:Single; inline;
begin
 Assert(@Self<>nil);
 Result:=PSingle(@Value)^;
end;

Function TsrLiteral.AsFloat64:Double; inline;
begin
 Assert(@Self<>nil);
 Result:=PDouble(@Value)^;
end;

Function TsrLiteral.GetPrintData:RawByteString;
begin
 if (name=nil) or (StrLen(name)=0) then
 begin
  Result:=IntToStr(Value);
 end else
 begin
  Result:=name;
 end;
end;

Function TsrLiteral.GetData(data:Pointer):Ptruint;
begin
 Result:=SizeOf(DWORD);
 if (data<>nil) then
 begin
  PDWORD(data)^:=DWORD(Value);
 end;
end;

//

Procedure TsrLiteralString.Init; inline;
begin
 fntype:=ntLiteralString;
end;

Function TsrLiteralString.GetPrintData:RawByteString;
begin
 Result:='"'+name+'"';
end;

Function TsrLiteralString.GetData(data:Pointer):Ptruint;
begin
 Result:=StrLen(name)+1;
 if (data<>nil) then
 begin
  Move(name^,data^,Result);
 end;
end;

//

Procedure TsrLiteralConst.Init; inline;
begin
 fntype:=ntLiteralConst;
end;

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
 case dtype of
  dtUnknow :Result:='0x'+HexStr(Value,4);

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
 Case dtype.BitSize of
    8:begin
       Result:=SizeOf(BYTE);
       if (data<>nil) then
       begin
        PBYTE(data)^:=BYTE(Value);
       end;
      end;
   16:begin
       Result:=SizeOf(WORD);
       if (data<>nil) then
       begin
        PWORD(data)^:=WORD(Value);
       end;
      end;
   32:begin
       Result:=SizeOf(DWORD);
       if (data<>nil) then
       begin
        PDWORD(data)^:=DWORD(Value);
       end;
      end;
   64:begin
       Result:=SizeOf(QWORD);
       if (data<>nil) then
       begin
        PQWORD(data)^:=Value;
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

function TsrLiteralList.FetchLiteral(Value:PtrUint;name:PChar):PsrLiteral;
var
 node:TsrLiteral;
 size:ptrint;
begin
 node:=Default(TsrLiteral);
 node.Init;
 node.dtype:=dtUint32;
 node.Value:=Value;
 node.name :=name;

 Result:=FNTree.Find(@node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrLiteral));
  Move(node,Result^,SizeOf(TsrLiteral));

  if (name<>nil) then
  begin
   size:=StrLen(name)+1;
   Result^.name:=FEmit.Alloc(size);
   Move(name^,Result^.name^,size);
  end;

  FNTree.Insert(Result);
 end;
end;

function TsrLiteralList.FetchString(name:PChar):PsrLiteralString;
var
 node:TsrLiteralString;
 size_o:ptrint;
 size_a:ptrint;
begin
 node:=Default(TsrLiteralString);
 node.Init;
 node.dtype:=dtString;
 node.name:=name;

 Result:=Pointer(FNTree.Find(Pointer(@node)));
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrLiteralString));
  Move(node,Result^,SizeOf(TsrLiteralString));

  if (name<>nil) then
  begin
   size_o:=StrLen(name);
   size_a:=Align(size_o+1,SizeOf(DWORD)); //align DW

   Result^.name:=FEmit.Alloc(size_a);
   Move(name^,Result^.name^,size_o);
  end else
  begin
   Result^.name:=FEmit.Alloc(SizeOf(DWORD));
   //space DW
  end;

  FNTree.Insert(Result);
 end;
end;

function TsrLiteralList.FetchConst(dtype:TsrDataType;Value:PtrUint):PsrLiteralConst;
var
 node:TsrLiteralConst;
begin
 node:=Default(TsrLiteralConst);
 node.Init;
 node.dtype:=dtype;
 node.Value:=Value;

 Result:=Pointer(FNTree.Find(Pointer(@node)));
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrLiteralConst));
  Move(node,Result^,SizeOf(TsrLiteralConst));

  FNTree.Insert(Result);
 end;
end;

end.

