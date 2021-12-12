{ Json Object Model implementation

  Copyright (C) 2018-2020 Red_prig

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

unit UJson;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

interface

uses
 SysUtils,Classes,DateUtils,Math,
 UfpJSON,Ujsonreader,Ujsonscanner;

type
 TPostCallback=Procedure(const Name,Value:RawByteString) of object;

 TJson=object
  protected
   Var
    P:Pointer;
   function  GetPath(Const APath:TJSONStringType):TJson;
   Procedure SetPath(Const APath:TJSONStringType;J:TJson);
   function  GetValues(Const APath:TJSONStringType):Variant;
   Procedure SetValues(Const APath:TJSONStringType;V:Variant);
   function  GetItem(i:SizeUInt):TJson;
   Procedure SetItem(i:SizeUInt;Data:TJson);
   function  GetName(i:SizeUInt):TJSONStringType;
   function  GetValue:Variant;
   Procedure SetValue(Const V:Variant);
   Procedure Reserve;
  public

   property  Path[Const APath:TJSONStringType]:TJson read GetPath write SetPath; default;
   property  Item[Index:SizeUInt]:TJson  read GetItem write SetItem;
   property  Name[Index:SizeUInt]:TJSONStringType read GetName;
   property  Values[Const APath:TJSONStringType]:Variant read GetValues write SetValues;
   property  Value:Variant read GetValue write SetValue;

   function  New:TJson; static;
   function  New(Const V:Variant):TJson; static;
   function  New(Source:TStream):TJson; static;
   function  NewFromFile(Const FName:RawByteString):TJson; static;
   procedure Dump(S:TStream);
   procedure DumpToFile(Const FName:RawByteString);
   procedure SaveToFile(Const FName:RawByteString);
   procedure Post(CB:TPostCallback);
   function  Clone:TJson;
   function  GetType:TJSONtype;
   function  isObject:Boolean;
   function  isArray:Boolean;
   function  isValue:Boolean;
   function  isNull:Boolean;
   function  isAssigned:Boolean;
   Procedure SetObject;
   Procedure SetArray;
   function  Count:SizeUInt;

   Function  Extract(Const APath:TJSONStringType):TJson;
   Procedure Delete(Const APath:TJSONStringType);
   Procedure Add(Data:TJson);
   Procedure Insert(Pos:SizeUInt;Data:TJson);
   function  Extract(i:SizeUInt):TJson;
   Procedure Delete(i:SizeUInt);
   function  AsBool(Def:Boolean=false):Boolean;
   function  AsUInt(Def:LongWord=0):LongWord;
   function  AsInt(Def:Longint=0):Longint;
   function  AsInt64(Def:Int64=0):Int64;
   function  AsQWORD(Def:QWORD=0):QWORD;
   function  AsStr(Def:TJSONStringType=''):TJSONStringType;
   function  AsFloat(Def:Double=0;Sep:Char='.'):Double;
   function  AsCurr(Def:Currency=0;Sep:Char='.'):Currency;
   Procedure Free;
 end;

 TJsonIterate=object
  private
   type
    PJPos=^TJPos;
    TJPos=record
     LD:TJson;
     LI:SizeUInt;
    end;
   Var
    FMas:array of TJPos;
    FCount:SizeInt;
   Procedure Push;
   Procedure Pop;
   Procedure Down;
  public
   Item:TJson;
  Function  Top:PJPos;
  function  New(This:TJson):TJsonIterate; static;
  function  Next:Boolean;
  Function  XPath:TJSONStringType;
 end;

function  TryStrToDateTime2(const S:RawByteString;out Value:TDateTime;const FormatSettings:TFormatSettings):Boolean;
function  DateTimeAsStr(DateTime:TDateTime;Const DFormat,TFormat:TJSONStringType;DSep,TSep:Char):TJSONStringType;
function  DateAsStr(Date:TDate;Const Format:TJSONStringType;Sep:Char):TJSONStringType;
function  TimeAsStr(Time:TTime;Const Format:TJSONStringType;Sep:Char):TJSONStringType;
function  CurrAsStr(Curr:Currency;Sep:Char):TJSONStringType;
function  CurrAsStrF(Curr:Currency;Sep:Char;Digits:Integer):TJSONStringType;
function  StrAsDate(Const Value,Format:TJSONStringType;Sep:Char;Def:TDate):TDate;
function  StrAsTime(Const Value,Format:TJSONStringType;Sep:Char;Def:TTime):TTime;
function  StrAsDateTime(Const Value,DFormat,TFormat:TJSONStringType;DSep,TSep,BSep:Char;Def:TDateTime):TDateTime;
function  StrAsDateTimeRus(Const Value:TJSONStringType):TDateTime;
function  IsNullValue(AValue:Double):Boolean;
function  Rfc3339toDateTime(Const S:RawByteString;inUTC:Boolean):TDateTime;
function  DateTimeToRfc3339(DateTime:TDateTime;inUTC:Boolean):RawByteString;
function  StrAsCurr(Const Value:TJSONStringType;Sep:Char;Def:Currency):Currency;
function  StrAsFloat(Const Value:TJSONStringType;Sep:Char;Def:Double):Double;
function  FloatAsStr(Value:Double;Sep:Char):TJSONStringType;

implementation

type
 PJSON_Data=^TJSON_Data;
 PPJSON_Data=^PJSON_Data;
 TJSON_Data=object
  protected
   Var
    Item:record
     Case jType:TJSONInstanceType of
      jitNumberInteger:(VInt:Integer);
      jitNumberInt64:(VInt64:Int64);
      jitNumberQWord:(VQWord:QWord);
      jitNumberFloat:(VFloat:Double);
      jitBoolean:(VBoolean:Boolean);
      jitString,
      jitArray,
      jitObject:(VData:Pointer);
    end;
   function  _IndexOf(Const S:TJSONStringType;Var ri:SizeUInt):Integer;
   function  _MakeIt(Const S:TJSONStringType):PPJSON_Data;
   function  _MakeIt(i:SizeUInt):PPJSON_Data;
   function  _MakeIt:PPJSON_Data;
   function  _Copy:PJSON_Data;
  public
   function  IndexOf(Const S:TJSONStringType):SizeInt;
   function  IndexOf(Data:PJSON_Data):SizeInt;
   function  GetItem(Const S:TJSONStringType):PJSON_Data;
   Procedure SetItem(Const S:TJSONStringType;Data:PJSON_Data);
   Function  Extract(Const S:TJSONStringType):PJSON_Data;
   function  GetName(i:SizeUInt):TJSONStringType;
   function  Count:SizeUInt;
   function  Add(Data:PJSON_Data):Boolean;
   function  Insert(Pos:SizeUInt;Data:PJSON_Data):Boolean;
   function  GetItem(i:SizeUInt):PJSON_Data;
   function  SetItem(i:SizeUInt;Data:PJSON_Data):Boolean;
   function  Extract(i:SizeUInt):PJSON_Data;
   function  GetValue:Variant;
   function  SetValue(Const V:Variant):Boolean;
   function  AsBool(Def:Boolean=false):Boolean;
   function  AsUInt(Def:LongWord=0):LongWord;
   function  AsInt(Def:Longint=0):Longint;
   function  AsInt64(Def:Int64=0):Int64;
   function  AsQWORD(Def:QWORD=0):QWORD;
   function  AsStr(Def:TJSONStringType=''):TJSONStringType;
   function  AsFloat(Def:Double=0;Sep:Char='.'):Double;
   function  AsCurr(Def:Currency=0;Sep:Char='.'):Currency;
   function  New:PJSON_Data; static;
   Procedure Clear;
 end;

 TJSON_DataObject=object
  Key:TJSONStringType;
  Value:PJSON_Data;
 end;

 TJSONArray=Array of PJSON_Data;
 TJSONObject=Array of TJSON_DataObject;

 TDAScan=object
  L,H,D,I:SizeInt;
  Procedure Scan(Const APath:TJSONStringType);
  Procedure Next;
  function  Str(Const APath:TJSONStringType):TJSONStringType;
  function  IsTrueArr:Boolean;
  function  IsNotArr:Boolean;
  function  GetType:Byte;
 end;
 TMakeInfo=object
  RD:PPJSON_Data;
  Procedure Init(Var J:PJSON_Data);
  Procedure ReSet(New:PJSON_Data);
  Procedure ReSetValue(V:Variant);
  Procedure ReSetArr;
  Procedure ReSetObj;
  function  GetType:TJSONInstanceType;
 end;

 TJSONReader=class(TBaseJSONReader)
   Var
    FMas:array of PJSON_Data;
    FCount:SizeUInt;
    FItem:PPJSON_Data;
    FRoot:PJSON_Data;

   Procedure Push;
   Procedure Pop;
   Function  Top:PPJSON_Data;

   Procedure KeyValue    (Const AKey:TJSONStringType);   override;
   Procedure ValueCheck;
   Procedure StringValue (Const AValue:TJSONStringType); override;
   Procedure NumberValue (Const AValue:TJSONStringType); override;
   Procedure NullValue;   override;
   Procedure FloatValue  (Const AValue:Double);  override;
   Procedure BooleanValue(Const AValue:Boolean); override;
   Procedure IntegerValue(Const AValue:integer); override;
   Procedure Int64Value  (Const AValue:int64);   override;
   Procedure QWordValue  (Const AValue:QWord);   override;
   Procedure StartArray;  override;
   Procedure StartObject; override;
   Procedure EndArray;    override;
   Procedure EndObject;   override;
 end;

 TJsonStack=object
  type
   PJPos=^TJPos;
   TJPos=record
    LD:PJSON_Data;
    LI:SizeUInt;
   end;
  Var
   FMas:array of TJPos;
   FCount:SizeInt;
  Function  Top:PJPos;
  Procedure Push;
  Procedure Pop;
 end;

 TJsonStack2=object
  type
   PJPos=^TJPos;
   TJPos=record
    LD,CD:PJSON_Data;
    LI:SizeUInt;
   end;
  Var
   FMas:array of TJPos;
   FCount:SizeInt;
  Function  Top:PJPos;
  Procedure Push;
  Procedure Pop;
 end;

Procedure TJsonIterate.Push;
begin
 Inc(FCount);
 if FCount>Length(FMas) then
  SetLength(FMas,FCount);
 FMas[FCount-1]:=Default(TJPos);
end;

Procedure TJsonIterate.Pop;
begin
 if FCount<>0 then
 begin
  Dec(FCount);
  FMas[FCount]:=Default(TJPos);
 end;
end;

Function TJsonIterate.Top:PJPos;
begin
 Result:=nil;
 if FCount>0 then
  Result:=@FMas[FCount-1];
end;

function TJsonIterate.New(This:TJson):TJsonIterate;
begin
 Result:=Default(TJsonIterate);
 Result.Item:=This;
 Result.Down;
end;

Procedure TJsonIterate.Down;
begin
 repeat
  Case Item.GetType of
   jtArray,
   jtObject:
    if Item.Count>0 then
    begin
     Push;
     With Top^ do
     begin
      LD:=Item;
      LI:=0;
      Item:=LD.GetItem(0);
     end;
     Continue;
    end;
  end;
  Break;
 until false;
end;

function TJsonIterate.Next:Boolean;
begin
 Result:=FCount<>0;
 if Result then
 begin
  With Top^ do
  begin
   Inc(LI);
   if LI<LD.Count then
   begin
    Item:=LD.GetItem(LI);
   end else
   begin
    Item:=LD;
    Pop;
    Exit;
   end;
  end;
  Down;
 end;
end;

Function TJsonIterate.XPath:TJSONStringType;
Var
 i:SizeUInt;
begin
 Result:='';
 if FCount<>0 then
  For i:=0 to FCount-1 do
   Case FMas[i].LD.GetType of
    jtArray :Result:=Result+'['+IntToStr(FMas[i].LI)+']';
    jtObject:
    begin
     if i<>0 then Result:=Result+'.';
     Result:=Result+FMas[i].LD.GetName(FMas[i].LI);
    end;
   end;
end;

function TryStrToDateTime2(const S:RawByteString;out Value:TDateTime;const FormatSettings:TFormatSettings):Boolean;
var
 I:SizeInt;
 dtdate,dttime:TDateTime;
begin
 result:=false;
 I:=Pos(FormatSettings.TimeSeparator,S);
 If (I>0) then
 begin
  While (I>0) and (S[I]<>FormatSettings.ListSeparator) do Dec(I);
  If I>0 then
  begin
   if not TryStrToDate(Copy(S,1,I-1),dtdate,Formatsettings) then
     exit;
   if not TryStrToTime(Copy(S,i+1, Length(S)-i),dttime,Formatsettings) then
     exit;
   Value:=ComposeDateTime(dtdate,dttime);
   result:=true;
  end else
   result:=TryStrToTime(s,Value,Formatsettings);
 end else
   result:=TryStrToDate(s,Value,Formatsettings);
end;

function DateTimeAsStr(DateTime:TDateTime;Const DFormat,TFormat:TJSONStringType;DSep,TSep:Char):TJSONStringType;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:=DFormat;
 FS.DateSeparator:=DSep;
 FS.ShortTimeFormat:=TFormat;
 FS.LongTimeFormat:=TFormat;
 FS.TimeSeparator:=TSep;
 Result:=DateTimeToStr(DateTime,FS,True);
end;

function DateAsStr(Date:TDate;Const Format:TJSONStringType;Sep:Char):TJSONStringType;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:=Format;
 FS.DateSeparator:=Sep;
 Result:=DateToStr(Date,FS);
end;

function TimeAsStr(Time:TTime;Const Format:TJSONStringType;Sep:Char):TJSONStringType;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortTimeFormat:=Format;

 FS.LongTimeFormat:=Format;

 FS.TimeSeparator:=Sep;
 Result:=TimeToStr(Time,FS);
end;

function CurrAsStr(Curr:Currency;Sep:Char):TJSONStringType;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.DecimalSeparator:=Sep;
 Result:=CurrToStr(Curr,FS);
end;

function CurrAsStrF(Curr:Currency;Sep:Char;Digits:Integer):TJSONStringType;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.DecimalSeparator:=Sep;
 Result:=CurrToStrF(Curr,ffFixed,Digits,FS);
end;

function StrAsDate(Const Value,Format:TJSONStringType;Sep:Char;Def:TDate):TDate;
begin
 //if not TryStrToDate(Value,Result,'y/m/d','-') then Result:=Def;
 if not TryStrToDate(Value,Result,Format,Sep) then Result:=Def;
end;

function StrAsTime(Const Value,Format:TJSONStringType;Sep:Char;Def:TTime):TTime;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortTimeFormat:=Format;
 FS.TimeSeparator:=Sep;
 FS.DecimalSeparator:='.';
 if not TryStrToTime(Value,Result,FS) then Result:=Def;
end;

function StrAsDateTime(Const Value,DFormat,TFormat:TJSONStringType;DSep,TSep,BSep:Char;Def:TDateTime):TDateTime;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:=DFormat;
 FS.DateSeparator:=DSep;
 FS.ShortTimeFormat:=TFormat;
 FS.LongTimeFormat:=TFormat;
 FS.TimeSeparator:=TSep;
 FS.ListSeparator:=BSep;
 FS.DecimalSeparator:='.';
 if not TryStrToDateTime2(Value,Result,FS) then Result:=Def;
end;

function StrAsDateTimeRus(Const Value:TJSONStringType):TDateTime;
begin
 Result:=StrAsDateTime(Value,'d/m/y','hh:nn:ss.zzz','.',':',' ',Nan);
end;

Function GetUTCOffsetStr(s:Integer):RawByteString;
Var
 G:Char;

 function IntToStrAlign(i:Integer):RawByteString; inline;
 begin
  Result:='';
  Case i of
   0..59:Result:=Char(Ord('0')+(i div 10))+Char(Ord('0')+(i mod 10));
  end;
 end;

begin
 G:='+';
 if (S<0) then
 begin
  G:='-';
  s:=Abs(s);
 end;
 Result:=G+IntToStrAlign(s div 60)+':'+IntToStrAlign(s mod 60);
end;

function IsNullValue(AValue:Double):Boolean;
begin
 Result:=IsNan(AValue) or IsInfinite(AValue);
end;

function TryEncodeTime(Hour,Min,Sec,MSec_len:Word;MSec:DWord;Out Time:TDateTime):boolean; inline;
begin
 Result:=(Hour<24) and (Min<60) and (Sec<60);
 If Result then
  Case MSec_len of
   0:MSec:=0;
   1:MSec:=MSec*100;
   2:MSec:=MSec*10;
   3:MSec:=MSec;
   4:MSec:=Round(MSec/10);
   5:MSec:=Round(MSec/100);
   6:MSec:=Round(MSec/1000);
   7:MSec:=Round(MSec/10000);
   8:MSec:=Round(MSec/100000);
   9:MSec:=Round(MSec/1000000);
   else
    Result:=False;
  end;
 If Result then
 begin
  Time:=TDateTime(cardinal(Hour)*3600000+cardinal(Min)*60000+cardinal(Sec)*1000+MSec)/MSecsPerDay;
 end;
end;

function Rfc3339toDateTime(Const S:RawByteString;inUTC:Boolean):TDateTime;

type
 TRfc3339DateTime=packed record
  Yer,Mon,Day,
  Hor,Min,Sec,
  Msl:Word;
  Ofs:SmallInt;
  Msc:DWord;
 end;

Var
 i:SizeInt;
 UTC,Sig:Boolean;
 Stt:Word;
 R:TRfc3339DateTime;
 Date,Time:TDateTime;
begin
 Result:=NaN;
 UTC:=false;
 Sig:=false;
 Stt:=0;
 R:=Default(TRfc3339DateTime);
 i:=1;
 while (i<=Length(S)) do
 begin
  case Stt of
   0:case S[i] of
      '0'..'9':R.Yer:=R.Yer*10+(Byte(S[i]) and $F);
      '-':Stt:=1;
      else Exit;
     end;
   1:case S[i] of
      '0'..'9':R.Mon:=R.Mon*10+(Byte(S[i]) and $F);
      '-':Stt:=2;
      else Exit;
     end;
   2:case S[i] of
      '0'..'9':R.Day:=R.Day*10+(Byte(S[i]) and $F);
      'T',
      't',
      ' ':Stt:=3;
      else Exit;
     end;
   3:case S[i] of
      '0'..'9':R.Hor:=R.Hor*10+(Byte(S[i]) and $F);
      ':':Stt:=4;
      else Exit;
     end;
   4:case S[i] of
      '0'..'9':R.Min:=R.Min*10+(Byte(S[i]) and $F);
      ':':Stt:=5;
      else Exit;
     end;
   5:case S[i] of
      '0'..'9':R.Sec:=R.Sec*10+(Byte(S[i]) and $F);
      '.':Stt:=6;
      'Z',
      'z':Stt:=9;
      '+':Stt:=7;
      '-':
      begin
       Sig:=true;
       Stt:=7;
      end;
      else Exit;
     end;
   6:case S[i] of
      '0'..'9':
      begin
       Inc(R.Msl);
       R.Msc:=R.Msc*10+(Byte(S[i]) and $F);
      end;
      'Z',
      'z':Stt:=9;
      '+':Stt:=7;
      '-':
      begin
       Sig:=true;
       Stt:=7;
      end;
      else Exit;
     end;

   7:case S[i] of
      '0'..'9':R.Ofs:=R.Ofs*600+(Byte(S[i]) and $F)*60;
      ':':Stt:=8;
      else Exit;
     end;

   8:case S[i] of
      '0'..'9':R.Ofs:=R.Ofs*10+(Byte(S[i]) and $F);
      else Exit;
     end;

  end;
  Inc(i);
 end;

 case Stt of
  5:;//end sec
  6:;//end ms
  9:UTC:=True;//Z
  7,8:;//+/-
  else
   Exit;
 end;

 if not TryEncodeDate(R.Yer,R.Mon,R.Day,Date) then Exit;
 if not TryEncodeTime(R.Hor,R.Min,R.Sec,R.Msl,R.Msc,Time) then Exit;
 Result:=ComposeDateTime(Date,Time);

 if not UTC then
 begin
  if sig then R.Ofs:=-R.Ofs;
  Result:=LocalTimeToUniversal(Result,R.Ofs);
 end;

 if (not inUTC) then
 begin
  Result:=UniversalTimeToLocal(Result);
 end;
end;

function DateTimeToRfc3339(DateTime:TDateTime;inUTC:Boolean):RawByteString;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.ShortDateFormat:='yyyy/mm/dd';
 FS.DateSeparator  :='-';
 FS.ShortTimeFormat:='hh:nn:ss.zzz';
 FS.LongTimeFormat :='hh:nn:ss.zzz';
 FS.TimeSeparator  :=':';

 if inUTC then
 begin
  DateTime:=LocalTimeToUniversal(DateTime);
  Result:=DateToStr(DateTime,FS)+'T'+TimeToStr(DateTime,FS)+'Z';
 end else
 begin
  Result:=DateToStr(DateTime,FS)+'T'+TimeToStr(DateTime,FS)+GetUTCOffsetStr(-GetLocalTimeOffset);
 end;
end;

function StrAsCurr(Const Value:TJSONStringType;Sep:Char;Def:Currency):Currency;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.DecimalSeparator:=Sep;
 Result:=StrToCurrDef(Value,Def,FS);
 if Result<0 then Result:=Def;
end;

function StrAsFloat(Const Value:TJSONStringType;Sep:Char;Def:Double):Double;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.DecimalSeparator:=Sep;
 Result:=StrToFloatDef(Value,Def,FS);
 if Result<0 then Result:=Def;
end;

function FloatAsStr(Value:Double;Sep:Char):TJSONStringType;
Var
 FS:TFormatSettings;
begin
 FS:=DefaultFormatSettings;
 FS.DecimalSeparator:=Sep;
 Result:=FloatToStr(Value,FS);
end;

function StrToBoolDef2(const S:TJSONStringType;Def:Boolean):Boolean;
begin
 Case S of
  '','0',#0:Result:=False;
  else Result:=StrToBoolDef(S,Def);
 end;
end;

Procedure TDAScan.Scan(Const APath:TJSONStringType); inline;
const
 WS  =[#0..' '];
 RST1=[#0..' ','.'];
 RST2=[#0..' ','.',']'];
 RST3=[#0..' ','.','['];
Var
 Len,T:SizeInt;
begin
 Len:=Length(APath);
 While (L<=Len) do
  if Not (APath[L] in RST1) then Break else Inc(L);
 H:=L;
 if (L<=Len) and (APath[L]='[') then
 begin
  T:=L;
  Inc(T);
  While (T<=Len) do
   if Not (APath[T] in WS) then Break else Inc(T);
  D:=T;
  While (D<=Len) do
   if (APath[D] in RST2) then Break else Inc(D);
  i:=StrToIntDef(Copy(APath,T,D-T),-1);
  While (D<=Len) do
   if Not (APath[D] in WS) then Break else Inc(D);
  if (D<=Len) and (APath[D]=']') then Inc(D);
 end else
 begin
  While (H<=Len) do
   if (APath[H] in RST3) then Break else Inc(H);
  D:=H;
  i:=0;
 end;
end;

Procedure TDAScan.Next; inline;
begin
 L:=D;
end;

function TDAScan.Str(Const APath:TJSONStringType):TJSONStringType; inline;
begin
 Result:=Copy(APath,L,H-L);
end;

function TDAScan.IsTrueArr:Boolean; inline;
begin
 Result:=(i>=0);
end;

function TDAScan.IsNotArr:Boolean; inline;
begin
 Result:=(i<0);
end;

{
0 - ''
1 - [1]
2 - obj
}
function TDAScan.GetType:Byte; inline;
begin
 if (L<>H) then
  Result:=2 else
 if (D>H) then
  Result:=1 else
  Result:=0;
end;

Procedure FreeAndNilJson(Var P:PJSON_Data);
begin
 if Assigned(P) then
 begin
  P^.Clear;
  FreeMem(P);
  P:=nil;
 end;
end;

Procedure TMakeInfo.Init(Var J:PJSON_Data); inline;
begin
 RD:=@J;
end;

Procedure TMakeInfo.ReSet(New:PJSON_Data);
begin
 if Assigned(RD) then
 if RD^<>New then
 begin
  FreeAndNilJson(RD^);
  RD^:=New;
 end;
end;

Procedure TMakeInfo.ReSetValue(V:Variant);
begin
 if Assigned(RD) then
 begin
  if not Assigned(RD^) then
  begin
   RD^:=TJSON_Data.New;
  end;
  RD^^.SetValue(V)
 end;
end;

Procedure TMakeInfo.ReSetArr;
begin
 if Assigned(RD) then
 begin
  if Assigned(RD^) then
  begin
   RD^^.Clear
  end else
  begin
   RD^:=TJSON_Data.New;
  end;
  RD^^.Item.jType:=jitArray;
 end;
end;

Procedure TMakeInfo.ReSetObj;
begin
 if Assigned(RD) then
 begin
  if Assigned(RD^) then
  begin
   RD^^.Clear
  end else
  begin
   RD^:=TJSON_Data.New;
  end;
  RD^^.Item.jType:=jitObject;
 end;
end;

function TMakeInfo.GetType:TJSONInstanceType;
begin
 Result:=jitUnknown;
 if Assigned(RD) then
 begin
  if Assigned(RD^) then
  begin
   Result:=RD^^.Item.jType;
  end;
 end;
end;

function _MakeIt(Var J:PJSON_Data;Const APath:TJSONStringType):TMakeInfo;
Var
 DAScan:TDAScan;
 MI:TMakeInfo;

begin
 Result:=Default(TMakeInfo);
 MI.Init(J);

 DAScan.L:=1;
 repeat
  DAScan.Scan(APath);
  Case DAScan.GetType of
   {0 - ''
   1 - [1]
   2 - obj}
   0:Exit(MI);
   1:if DAScan.IsNotArr then Exit else
     begin
      Case MI.GetType of
       jitArray:;
       jitObject:Exit;
       else
        begin
         MI.ReSetArr;
        end;
      end;
      MI.RD:=MI.RD^^._MakeIt(DAScan.I);
      DAScan.Next;
     end;
   2:begin
      Case MI.GetType of
       jitArray:Exit;
       jitObject:;
       else
        begin
         MI.ReSetObj;
        end;
      end;
      MI.RD:=MI.RD^^._MakeIt(DAScan.Str(APath));
      DAScan.Next;
     end;

  end;

 Until False;
end;

function TJSON_Data._IndexOf(Const S:TJSONStringType;Var ri:SizeUInt):Integer;
Var
 i:Integer;
 l,r,m:SizeUInt;
 P:^TJSONObject;
begin
 Result:=-1;
 case Item.jType of
  jitObject:
  begin
   P:=@Item.VData;
   i:=0;
   l:=0;
   r:=Length(P^);
   while (l<r) do
   begin
    m:=l+(r-l) div 2;
    i:=CompareStr(P^[m].Key,S);
    if i=0 then
    begin
     ri:=m;
     Exit(0);
    end;
    if i>0 then
    begin
     r:=m;
    end else
    begin
     l:=m+1;
    end;
   end;
   ri:=l;
   Result:=1;
  end;
 end;
end;

function  TJSON_Data._MakeIt(Const S:TJSONStringType):PPJSON_Data;
Var
 ri,L:SizeUInt;
 P:^TJSONObject;
begin
 Result:=nil;
 ri:=0;
 Case _IndexOf(S,ri) of
  0:begin
     P:=@Item.VData;
     Result:=@P^[ri].Value;
    end;
  1:begin
     P:=@Item.VData;
     L:=Length(P^);
     SetLength(P^,L+1);
     if (L-ri)<>0 then
     begin
      System.Move(P^[ri],P^[ri+1],(L-ri)*SizeOf(TJSON_DataObject));
      FillChar(P^[ri],SizeOf(TJSON_DataObject),0);
     end;
     P^[ri].Key:=S;
     P^[ri].Value:=nil;
     Result:=@P^[ri].Value;
    end;
 end;
end;

function  TJSON_Data._MakeIt(i:SizeUInt):PPJSON_Data;
Var
 PA:^TJSONArray;
 L:SizeUInt;
begin
 Result:=nil;
 case Item.jType of
  jitArray:
  begin
   PA:=@Item.VData;
   L:=Length(PA^);
   if i>=L then
   begin
    SetLength(PA^,i+1);
    FillChar(PA^[L],(i-L+1)*SizeOf(PJSON_Data),0);
   end;
   Result:=@PA^[i];
  end;
 end;
end;

function  TJSON_Data._MakeIt:PPJSON_Data;
Var
 PA:^TJSONArray;
 L:SizeUInt;
begin
 Result:=nil;
 case Item.jType of
  jitArray:
  begin
   PA:=@Item.VData;
   L:=Length(PA^);
   SetLength(PA^,L+1);
   FillChar(PA^[L],SizeOf(PJSON_Data),0);
   Result:=@PA^[L];
  end;
 end;
end;

function TJSON_Data.IndexOf(Const S:TJSONStringType):SizeInt;
Var
 ri:SizeUInt;
begin
 ri:=0;
 if _IndexOf(S,ri)=0 then
  Result:=ri
 else
  Result:=-1;
end;

function TJSON_Data.IndexOf(Data:PJSON_Data):SizeInt;
Var
 i:SizeInt;
 PO:^TJSONObject;
 PA:^TJSONArray;
begin
 Result:=-1;
 case Item.jType of
  jitObject:
  begin
   PO:=@Item.VData;
   if Length(PO^)>0 then
    For i:=0 to High(PO^) do
     if PO^[i].Value=Data then
      Exit(i);
  end;
  jitArray:
  begin
   PA:=@Item.VData;
   if i<Length(PA^) then
    For i:=0 to High(PA^) do
     if PA^[i]=Data then
      Exit(i);
  end;
 end;
end;

function TJSON_Data.GetItem(Const S:TJSONStringType):PJSON_Data;
Var
 ri:SizeUInt;
 P:^TJSONObject;
begin
 Result:=nil;
 ri:=0;
 case Item.jType of
  jitObject:
  begin
   P:=@Item.VData;
   if _IndexOf(S,ri)=0 then
    Result:=P^[ri].Value;
  end;
 end;
end;

Procedure TJSON_Data.SetItem(Const S:TJSONStringType;Data:PJSON_Data);
Var
 P:PPJSON_Data;
begin
 P:=_MakeIt(S);
 if Assigned(P) then
 begin
  FreeAndNilJson(P^);
  P^:=Data;
 end;
end;

Function TJSON_Data.Extract(Const S:TJSONStringType):PJSON_Data;
Var
 ri:SizeUInt;
 P:^TJSONObject;
begin
 Result:=nil;
  ri:=0;
 Case _IndexOf(S,ri) of
  0:begin
     P:=@Item.VData;
     SetLength(P^[ri].Key,0);
     Result:=P^[ri].Value;
     if (Length(P^)-ri)<>0 then
     begin
      System.Move(P^[ri+1],P^[ri],(Length(P^)-ri)*SizeOf(TJSON_DataObject));
      FillChar(P^[Length(P^)-ri],SizeOf(TJSON_DataObject),0);
     end;
     SetLength(P^,Length(P^)-1);
    end;
 end;
end;

function TJSON_Data.GetName(i:SizeUInt):TJSONStringType;
Var
 P:^TJSONObject;
begin
 case Item.jType of
  jitObject:
  begin
   P:=@Item.VData;
   if i<Length(P^) then
    Result:=P^[i].Key;
  end;
 end;
end;

function TJSON_Data.Count:SizeUInt;
begin
 Result:=0;
 case Item.jType of
  jitArray:
  begin
   Result:=Length(TJSONArray(Item.VData));
  end;
  jitObject:
  begin
   Result:=Length(TJSONObject(Item.VData));
  end;
 end;
end;

function TJSON_Data.Add(Data:PJSON_Data):Boolean;
Var
 P:PPJSON_Data;
begin
 P:=_MakeIt;
 if Assigned(P) then
 begin
  P^:=Data;
 end;
end;

function TJSON_Data.Insert(Pos:SizeUInt;Data:PJSON_Data):Boolean;
Var
 P:^TJSONArray;
 L:SizeUInt;
begin
 case Item.jType of
  jitArray:
  begin
   P:=@Item.VData;
   L:=Length(P^);
   if (Pos<L) then
   begin
    SetLength(P^,L+1);
    //if (L-Pos)<>0 then
    begin
     System.Move(P^[Pos],P^[Pos+1],(L-Pos)*SizeOf(PJSON_Data));
     FillChar(P^[Pos],SizeOf(PJSON_Data),0);
    end;
    P^[Pos]:=Data;
    Result:=True;
   end else
    Result:=SetItem(Pos,Data);
  end;
 end;
end;

function TJSON_Data.GetItem(i:SizeUInt):PJSON_Data;
Var
 PO:^TJSONObject;
 PA:^TJSONArray;
begin
 Result:=nil;
 case Item.jType of
  jitObject:
  begin
   PO:=@Item.VData;
   if i<Length(PO^) then
    Result:=PO^[i].Value;
  end;
  jitArray:
  begin
   PA:=@Item.VData;
   if i<Length(PA^) then
    Result:=PA^[i];
  end;
 end;
end;

function TJSON_Data.SetItem(i:SizeUInt;Data:PJSON_Data):Boolean;
Var
 PO:^TJSONObject;
 P:PPJSON_Data;
begin
 Result:=False;
 case Item.jType of
  jitObject:
  begin
   PO:=@Item.VData;
   if i<Length(PO^) then
   begin
    FreeAndNilJson(PO^[i].Value);
    PO^[i].Value:=Data;
    Result:=True;
   end;
  end;
  jitArray:
  begin
   P:=_MakeIt(i);
   Result:=Assigned(P);
   if Result then
   begin
    FreeAndNilJson(P^);
    P^:=Data;
   end;
  end;
 end;
end;

function TJSON_Data.Extract(i:SizeUInt):PJSON_Data;
Var
 PO:^TJSONObject;
 PA:^TJSONArray;
begin
 Result:=nil;
 case Item.jType of
  jitObject:
  begin
   PO:=@Item.VData;
   if (i<Length(PO^)) then
   begin
    SetLength(PO^[i].Key,0);
    Result:=PO^[i].Value;
    if (Length(PO^)-i)<>0 then
    begin
     System.Move(PO^[i+1],PO^[i],(Length(PO^)-i)*SizeOf(TJSON_DataObject));
     FillChar(PO^[Length(PO^)-1],SizeOf(TJSON_DataObject),0);
    end;
    SetLength(PO^,Length(PO^)-1);
   end;
  end;
  jitArray:
  begin
   PA:=@Item.VData;
   if (i<Length(PA^)) then
   begin
    Result:=PA^[i];
    if (Length(PA^)-i-1)<>0 then
    begin
     System.Move(PA^[i+1],PA^[i],(Length(PA^)-i-1)*SizeOf(PJSON_Data));
     FillChar(PA^[Length(PA^)-1],SizeOf(PJSON_Data),0);
    end;
    SetLength(PA^,Length(PA^)-1);
   end;
  end;
 end;
end;

function TJSON_Data.GetValue:Variant;
begin
 Result:=nil;
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Item.VFloat;
  jitString       :Result:=TJSONStringType(Item.VData);
  jitBoolean      :Result:=Item.VBoolean;
 end;
end;

function TJSON_Data.SetValue(Const V:Variant):Boolean;
begin
 Result:=False;
 Case TVarData(V).vtype of
  varnull:
  begin
   Clear;
   Item.jType:=jitNull;
   Result:=True;
  end;
  varsmallint:
  begin
   Clear;
   Item.jType:=jitNumberInteger;
   Item.VInt:=TVarData(V).vsmallint;
   Result:=True;
  end;
  varinteger:
  begin
   Clear;
   Item.jType:=jitNumberInteger;
   Item.VInt:=TVarData(V).vinteger;
   Result:=True;
  end;
  varsingle:
  begin
   Clear;
   Item.jType:=jitNumberFloat;
   Item.VFloat:=TVarData(V).vsingle;
   Result:=True;
  end;
  vardouble:
  begin
   Clear;
   Item.jType:=jitNumberFloat;
   Item.VFloat:=TVarData(V).vdouble;
   Result:=True;
  end;
  vardate:
  begin
   Clear;
   Item.jType:=jitString;
   TJSONStringType(Item.VData):=DateTimeToStr(TVarData(V).vdate);
   Result:=True;
  end;
  varcurrency:
  begin
   Clear;
   Item.jType:=jitString;
   TJSONStringType(Item.VData):=CurrToStr(TVarData(V).vcurrency);
   Result:=True;
  end;
  varboolean:
  begin
   Clear;
   Item.jType:=jitBoolean;
   Item.VBoolean:=TVarData(V).vboolean;
   Result:=True;
  end;
  varshortint:
  begin
   Clear;
   Item.jType:=jitNumberInteger;
   Item.VInt:=TVarData(V).vshortint;
   Result:=True;
  end;
  varbyte:
  begin
   Clear;
   Item.jType:=jitNumberInteger;
   Item.VInt:=TVarData(V).vbyte;
   Result:=True;
  end;
  varword:
  begin
   Clear;
   Item.jType:=jitNumberInteger;
   Item.VInt:=TVarData(V).vword;
   Result:=True;
  end;
  varlongword:
  begin
   Clear;
   Item.jType:=jitNumberQWord;
   Item.VQWord:=TVarData(V).vlongword;
   Result:=True;
  end;
  varint64:
  begin
   Clear;
   Item.jType:=jitNumberInt64;
   Item.VQWord:=TVarData(V).vint64;
   Result:=True;
  end;
  varqword:
  begin
   Clear;
   Item.jType:=jitNumberQWord;
   Item.VQWord:=TVarData(V).vqword;
   Result:=True;
  end;
  varolestr,
  varstring,
  varustring:
  begin
   Clear;
   Item.jType:=jitString;
   TJSONStringType(Item.VData):=V;
   Result:=True;
  end;
 end;
end;

function TJSON_Data.AsBool(Def:Boolean=false):Boolean;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt<>0;
  jitNumberInt64  :Result:=Item.VInt64<>0;
  jitNumberQWord  :Result:=Item.VQWord<>0;
  jitNumberFloat  :Result:=Round(Item.VFloat)<>0;
  jitString       :Result:=StrToBoolDef2(TJSONStringType(Item.VData),Def);
  jitBoolean      :Result:=Item.VBoolean;
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsUInt(Def:LongWord=0):LongWord;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Round(Item.VFloat);
  jitString       :Result:=StrToQWordDef(TJSONStringType(Item.VData),Def);
  jitBoolean      :Result:=Byte(Item.VBoolean);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsInt(Def:Longint=0):Longint;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Round(Item.VFloat);
  jitString       :Result:=StrToIntDef(TJSONStringType(Item.VData),Def);
  jitBoolean      :Result:=Byte(Item.VBoolean);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsInt64(Def:Int64=0):Int64;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Round(Item.VFloat);
  jitString       :Result:=StrToInt64Def(TJSONStringType(Item.VData),Def);
  jitBoolean      :Result:=Byte(Item.VBoolean);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsQWORD(Def:QWORD=0):QWORD;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Round(Item.VFloat);
  jitString       :Result:=StrToQWordDef(TJSONStringType(Item.VData),Def);
  jitBoolean      :Result:=Byte(Item.VBoolean);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsStr(Def:TJSONStringType=''):TJSONStringType;
begin
 case Item.jType of
  jitNumberInteger:Result:=IntToStr(Item.VInt);
  jitNumberInt64  :Result:=IntToStr(Item.VInt64);
  jitNumberQWord  :Result:=IntToStr(Item.VQWord);
  jitNumberFloat  :Result:=FloatAsStr(Item.VFloat,'.');
  jitString       :Result:=TJSONStringType(Item.VData);
  jitBoolean      :Result:=BoolToStr(Item.VBoolean,true);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsFloat(Def:Double=0;Sep:Char='.'):Double;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Item.VFloat;
  jitString       :Result:=StrAsFloat(TJSONStringType(Item.VData),Sep,Def);
  jitBoolean      :Result:=Byte(Item.VBoolean);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.AsCurr(Def:Currency=0;Sep:Char='.'):Currency;
begin
 case Item.jType of
  jitNumberInteger:Result:=Item.VInt;
  jitNumberInt64  :Result:=Item.VInt64;
  jitNumberQWord  :Result:=Item.VQWord;
  jitNumberFloat  :Result:=Item.VFloat;
  jitString       :Result:=StrAsCurr(TJSONStringType(Item.VData),Sep,Def);
  jitBoolean      :Result:=Byte(Item.VBoolean);
  else
   Result:=Def;
 end;
end;

function TJSON_Data.New:PJSON_Data; inline;
begin
 Result:=AllocMem(SizeOf(TJSON_Data));
end;

Procedure TJSON_Data.Clear;
Var
 PO:^TJSONObject;
 PA:^TJSONArray;
 P:PJSON_Data;
 JIter:TJSONIterate;
begin
 JIter:=TJSONIterate.New(TJson(@Self));
 repeat
  P:=JIter.Item.P;
  if Assigned(P) then
  begin
   case P^.Item.jType of
    jitString:SetLength(TJSONStringType(P^.Item.VData),0);
    jitArray :
    begin
     PA:=@P^.Item.VData;
     SetLength(PA^,0);
    end;
    jitObject:
    begin
     PO:=@P^.Item.VData;
     SetLength(PO^,0);
    end;
   end;
   if P<>@Self then
   begin
    FreeMem(P);
   end;
  end;
 until not JIter.Next;
 Self:=Default(TJSON_Data);
end;

function TJSON_Data._Copy:PJSON_Data;
Var
 i:SizeInt;
begin
 Result:=TJSON_Data.New;
 Result^.Item.jType:=Item.jType;
 case Item.jType of
  jitNumberInteger:Result^.Item.VInt         :=Item.VInt;
  jitNumberInt64  :Result^.Item.VInt64       :=Item.VInt64;
  jitNumberQWord  :Result^.Item.VQWord       :=Item.VQWord;
  jitNumberFloat  :Result^.Item.VFloat       :=Item.VFloat;
  jitString       :TJSONStringType(Result^.Item.VData):=TJSONStringType(Item.VData);
  jitBoolean      :Result^.Item.VBoolean     :=Item.VBoolean;
  jitArray:
  begin
   SetLength(TJSONArray(Result^.Item.VData),Length(TJSONArray(Item.VData)));
  end;
  jitObject:
  begin
   SetLength(TJSONObject(Result^.Item.VData),Length(TJSONObject(Item.VData)));
   if Length(TJSONObject(Item.VData))>0 then
    For i:=0 to Length(TJSONObject(Item.VData))-1 do
     TJSONObject(Result^.Item.VData)[i].Key:=TJSONObject(Item.VData)[i].Key;
  end;
 end;
end;

function TJson.GetPath(Const APath:TJSONStringType):TJson;
Var
 DAScan:TDAScan;
 This:PJSON_Data;
begin
 Result:=Default(TJson);
 This:=P;
 DAScan.L:=1;
 While Assigned(This) do
 begin
  DAScan.Scan(APath);
  Case DAScan.GetType of
   {0 - ''
   1 - [1]
   2 - obj}
   0:Exit(TJson(This));
   1:Case This^.Item.jType of
      jitArray:if DAScan.IsNotArr then Exit else
              begin
               This:=This^.GetItem(DAScan.I);
               DAScan.Next;
              end
      else Exit;
     end;
   2:Case This^.Item.jType of
      jitObject:
               begin
                DAScan.I:=This^.IndexOf(DAScan.Str(APath));
                if DAScan.IsNotArr then Exit;
                This:=This^.GetItem(DAScan.I);
                DAScan.Next;
               end
      else Exit;
     end;


  end;

 end;
end;

Procedure TJson.SetPath(Const APath:TJSONStringType;J:TJson);
Var
 MI:TMakeInfo;
begin
 MI:=_MakeIt(P,APath);
 MI.ReSet(J.P);
end;

function TJson.GetValues(Const APath:TJSONStringType):Variant;
Var
 This:PJSON_Data;
begin
 Result:=nil;
 This:=GetPath(APath).P;
 if Assigned(This) then
  Result:=This^.GetValue;
end;

Procedure TJson.SetValues(Const APath:TJSONStringType;V:Variant);
Var
 MI:TMakeInfo;
begin
 MI:=_MakeIt(P,APath);
 MI.ReSetValue(V);
end;

function TJson.New:TJson; inline;
begin
 Result:=Default(TJson);
end;

function TJson.New(Const V:Variant):TJson; inline;
begin
 Result:=Default(TJson);
 Result.SetValue(V);
end;

Procedure TJson.Reserve; inline;
begin
 if Assigned(P) then
 begin
  PJSON_Data(P)^.Clear;
 end else
 begin
  P:=TJSON_Data.New;
 end;
end;

function TJson.GetType:TJSONtype;
begin
 Result:=jtUnknown;
 if Assigned(P) then
 begin
  Case PJSON_Data(P)^.Item.jType of
   jitUnknown:Result:=jtUnknown;
   jitNumberInteger,
   jitNumberInt64,
   jitNumberQWord,
   jitNumberFloat:Result:=jtNumber;
   jitString:Result:=jtString;
   jitBoolean:Result:=jtBoolean;
   jitNull:Result:=jtNull;
   jitArray:Result:=jtArray;
   jitObject:Result:=jtObject;
  end;
 end;
end;

function TJson.isObject:Boolean; inline;
begin
 Result:=GetType=jtObject;
end;

function TJson.isArray:Boolean; inline;
begin
 Result:=GetType=jtArray;
end;

function TJson.isValue:Boolean; inline;
begin
 Result:=False;
 Case GetType of
  jtNumber,
  jtString,
  jtBoolean,
  jtNull:Result:=True;
 end;
end;

function TJson.isNull:Boolean; inline;
begin
 Result:=GetType=jtNull;
end;

function TJson.isAssigned:Boolean; inline;
begin
 Result:=Assigned(P);
end;

Procedure TJson.SetObject;
begin
 Reserve;
 PJSON_Data(P)^.Item.jType:=jitObject;
end;

Procedure TJson.SetArray;
begin
 Reserve;
 PJSON_Data(P)^.Item.jType:=jitArray;
end;

function TJson.Count:SizeUInt;
begin
 Result:=0;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.Count;
 end;
end;

Function TJson.Extract(Const APath:TJSONStringType):TJson;
Var
 DAScan:TDAScan;
 LI:SizeInt;
 LD:PJSON_Data;
 This:PJSON_Data;
begin
 Result:=Default(TJson);
 LI:=-1;
 LD:=nil;
 This:=P;
 DAScan.L:=1;
 While Assigned(This) do
 begin
  DAScan.Scan(APath);
  Case DAScan.GetType of
   {0 - ''
   1 - [1]
   2 - obj}
   0:begin
      if Assigned(LD) then
      begin
       Result.P:=LD^.Extract(LI);
       Exit;
      end else
      begin
       Result.P:=P;
       P:=nil;
       Exit;
      end;
     end;
   1:Case This^.Item.jType of
      jitArray:if DAScan.IsNotArr then Exit else
               begin
                LI:=DAScan.I;
                LD:=This;
                This:=This^.GetItem(DAScan.I);
                DAScan.Next;
               end
      else Exit;
     end;
   2:Case This^.Item.jType of
      jitObject:
               begin
                LI:=This^.IndexOf(DAScan.Str(APath));
                if LI<0 then Exit;
                LD:=This;
                This:=This^.GetItem(LI);
                DAScan.Next;
               end
      else Exit;
     end;

  end;

 end;
end;

Procedure TJson.Delete(Const APath:TJSONStringType); inline;
begin
 Extract(APath).Free;
end;

Procedure TJson.Add(Data:TJson);
begin
 if not Assigned(P) then
 begin
  SetArray;
 end;
 PJSON_Data(P)^.Add(Data.P);
end;

Procedure TJson.Insert(Pos:SizeUInt;Data:TJson);
begin
 if not Assigned(P) then
 begin
  SetArray;
 end;
 PJSON_Data(P)^.Insert(Pos,Data.P);
end;

function TJson.GetItem(i:SizeUInt):TJson;
begin
 Result.P:=nil;
 if Assigned(P) then
 begin
  Result.P:=PJSON_Data(P)^.GetItem(i);
 end;
end;

Procedure TJson.SetItem(i:SizeUInt;Data:TJson);
begin
 if not Assigned(P) then
 begin
  SetArray;
 end;
 PJSON_Data(P)^.SetItem(i,Data.P);
end;

function TJson.GetName(i:SizeUInt):TJSONStringType;
begin
 Result:='';
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.GetName(i);
 end;
end;

function TJson.Extract(i:SizeUInt):TJson;
begin
 Result.P:=nil;
 if Assigned(P) then
 begin
  Result.P:=PJSON_Data(P)^.Extract(i);
 end;
end;

Procedure TJson.Delete(i:SizeUInt); inline;
begin
 Extract(i).Free;
end;

function TJson.GetValue:Variant;
begin
 Result:=nil;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.GetValue;
 end;
end;

Procedure TJson.SetValue(Const V:Variant);
begin
 Reserve;
 PJSON_Data(P)^.SetValue(V);
end;

function  TJson.AsBool(Def:Boolean=false):Boolean;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsBool(Def);
 end;
end;

function  TJson.AsUInt(Def:LongWord=0):LongWord;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsUInt(Def);
 end;
end;

function  TJson.AsInt(Def:Longint=0):Longint;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsInt(Def);
 end;
end;

function  TJson.AsInt64(Def:Int64=0):Int64;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsInt64(Def);
 end;
end;

function TJson.AsQWORD(Def:QWORD=0):QWORD;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsQWORD(Def);
 end;
end;

function  TJson.AsStr(Def:TJSONStringType=''):TJSONStringType;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsStr(Def);
 end;
end;

function  TJson.AsFloat(Def:Double=0;Sep:Char='.'):Double;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsFloat(Def,Sep);
 end;
end;

function  TJson.AsCurr(Def:Currency=0;Sep:Char='.'):Currency;
begin
 Result:=Def;
 if Assigned(P) then
 begin
  Result:=PJSON_Data(P)^.AsCurr(Def,Sep);
 end;
end;

Procedure TJson.Free; inline;
begin
 FreeAndNilJson(PJSON_Data(P));
end;

function TJson.New(Source:TStream):TJson;
Var
 JR:TJSONReader;
begin
 JR:=TJSONReader.Create(Source,DefaultOptions);
 JR.DoExecute;
 Result.P:=JR.FRoot;
 JR.Free;
end;

function TJson.NewFromFile(Const FName:RawByteString):TJson;
Var
 S:TFileStream;
begin
 S:=TFileStream.Create(FName,fmOpenRead or fmShareDenyWrite);
 Result:=New(S);
 FreeAndNil(S);
end;

Procedure TJSONReader.Push;
begin
 Inc(FCount);
 if FCount>Length(FMas) then
  SetLength(FMas,FCount);
 FMas[FCount-1]:=Default(PJSON_Data);
end;

Procedure TJSONReader.Pop;
begin
 if FCount<>0 then
 begin
  Dec(FCount);
  FMas[FCount]:=Default(PJSON_Data);
 end;
end;

Function TJSONReader.Top:PPJSON_Data;
begin
 Result:=@FRoot;
 if FCount>0 then
  Result:=@FMas[FCount-1];
end;

Procedure TJSONReader.KeyValue(Const AKey:TJSONStringType);
Var
 P:PPJSON_Data;
begin
 //Writeln(AKey);
 P:=Top;
 if Assigned(P^) then
 begin
  FItem:=P^^._MakeIt(AKey);
 end else
 begin
  P^:=TJSON_Data.New;
  FItem:=P;
 end;
end;

Procedure TJSONReader.ValueCheck;
Var
 P:PPJSON_Data;
begin
 if not Assigned(FItem) then
 begin
  P:=Top;
  if Assigned(P^) then
  begin
   FItem:=P^^._MakeIt;
  end else
  begin
   P^:=TJSON_Data.New;
   FItem:=P;
  end;
 end;
 if not Assigned(FItem^) then
 begin
  FItem^:=TJSON_Data.New;
 end;
end;

Procedure TJSONReader.StringValue(Const AValue:TJSONStringType);
begin
 ValueCheck;
 FItem^^.SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONReader.NumberValue(Const AValue:TJSONStringType);
begin
end;

Procedure TJSONReader.NullValue;
begin
 ValueCheck;
 FItem^^.SetValue(Null);
 FItem:=nil;
 //Writeln(JStack.XPath,'=Null');
 //Readln;
end;

Procedure TJSONReader.FloatValue(Const AValue:Double);
begin
 ValueCheck;
 FItem^^.SetValue(AValue);
 FItem:=nil;
 //Writeln(JStack.XPath,'=',AValue);
end;

Procedure TJSONReader.BooleanValue(Const AValue:Boolean);
begin
 ValueCheck;
 FItem^^.SetValue(AValue);
 FItem:=nil;
 //Writeln(XPath+Key,'=',AValue);
end;

Procedure TJSONReader.IntegerValue(Const AValue:integer);
begin
 ValueCheck;
 FItem^^.SetValue(AValue);
 FItem:=nil;
 //Writeln(XPath+Key,'=',AValue);
end;

Procedure TJSONReader.Int64Value(Const AValue:int64);
begin
 ValueCheck;
 FItem^^.SetValue(AValue);
 FItem:=nil;
 //Writeln(XPath+Key,'=',AValue);
end;

Procedure TJSONReader.QWordValue(Const AValue:QWord);
begin
 ValueCheck;
 FItem^^.SetValue(AValue);
 FItem:=nil;
 //Writeln(XPath+Key,'=',AValue);
end;

Procedure TJSONReader.StartArray;
begin
 ValueCheck;
 Push;
 Top^:=FItem^;
 FItem:=nil;
 Top^^.Item.jType:=jitArray;
end;

Procedure TJSONReader.StartObject;
begin
 ValueCheck;
 Push;
 Top^:=FItem^;
 FItem:=nil;
 Top^^.Item.jType:=jitObject;
end;

Procedure TJSONReader.EndArray;
begin
 Pop;
end;

Procedure TJSONReader.EndObject;
begin
 Pop;
end;

Function TJsonStack.Top:PJPos;
begin
 Result:=nil;
 if FCount>0 then
  Result:=@FMas[FCount-1];
end;

Procedure TJsonStack.Push;
begin
 Inc(FCount);
 if FCount>Length(FMas) then
  SetLength(FMas,FCount);
 FMas[FCount-1]:=Default(TJPos);
end;

Procedure TJsonStack.Pop;
begin
 if FCount<>0 then
 begin
  Dec(FCount);
  FMas[FCount]:=Default(TJPos);
 end;
end;

procedure _Dump(I:PJSON_Data;S:TStream;Sp:Boolean);
Var
 J:TJsonStack;
 FAddStr:TUtf8AddStr;

 Procedure W(T:AnsiChar); inline;
 begin
  S.Write(T,SizeOf(AnsiChar));
 end;

 Procedure W(Const T:TJSONStringType); inline;
 begin
  S.Write(PChar(T)^,Length(T));
 end;

 Procedure W(Const T:TUtf8AddStr); inline;
 begin
  S.Write(T.FStr^,T.FLen);
 end;

begin
 FAddStr:=Default(TUtf8AddStr);
 J:=Default(TJsonStack);

 repeat

  if J.Top<>nil then
  begin
   With J.Top^ do
    if LD^.Item.jType=jitObject then
    begin

     if Sp then
     begin
      W(Space(J.FCount));
     end;

     FAddStr.Reset;
     FAddStr.AddChar('"');
     _StringToJSONString(FAddStr,LD^.GetName(LI));
     FAddStr.AddChar('"');
     FAddStr.AddChar(':');

     W(FAddStr);
    end;
  end;

  Case I^.Item.jType of
   jitArray:
   begin
    W('[');
    if I^.Count>0 then
    begin
     J.Push;
     With J.Top^ do
     begin
      LD:=I;
      LI:=0;
      I:=LD^.GetItem(0);
      if I=nil then Exit;
     end;
     Continue;
    end else
     W(']');
   end;
   jitObject:
   begin
    W('{');
    if I^.Count>0 then
    begin

     if Sp then
     begin
      W(#13#10);
     end;

     J.Push;
     With J.Top^ do
     begin
      LD:=I;
      LI:=0;
      I:=LD^.GetItem(0);
      if I=nil then Exit;
     end;
     Continue;
    end else
     W('}');
   end;
   jitNumberInteger:W(  IntToStr(I^.Item.VInt));
   jitNumberInt64  :W(  IntToStr(I^.Item.VInt64));
   jitNumberQWord  :W(  IntToStr(I^.Item.VQWord));
   jitNumberFloat  :W(FloatAsStr(I^.Item.VFloat,'.'));
   jitString:
   begin
    FAddStr.Reset;
    FAddStr.AddChar('"');
    _StringToJSONString(FAddStr,TJSONStringType(I^.Item.VData));
    FAddStr.AddChar('"');
    W(FAddStr);
   end;
   jitBoolean:
   begin
    Case I^.Item.VBoolean of
     True :W('true');
     False:W('false');
    end;
   end;
   jitNull:
   begin
    W('null');
   end;
  end;

  repeat
   if J.Top=nil then Exit;
   With J.Top^ do
   begin
    Inc(LI);
    if LI<LD^.Count then
    begin
     I:=LD^.GetItem(LI);
     if I=nil then Exit;
     W(',');

     if Sp then
     begin
      W(#13#10);
     end;

     Break;
    end else
    begin
     I:=LD;
     J.Pop;
     Case I^.Item.jType of
      jitArray :W(']');
      jitObject:W('}');
     end;
    end;
   end;
  until false;

 until false;
 FAddStr.Free;
end;

procedure TJson.Dump(S:TStream);
begin
 if (P=nil) or (S=nil) then Exit;
 _Dump(P,S,false);
end;

procedure TJson.DumpToFile(Const FName:RawByteString);
Var
 M:TMemoryStream;
begin
 if (P=nil) then Exit;
 M:=TMemoryStream.Create;
 _Dump(P,M,false);
 M.SaveToFile(FName);
 M.Free;
end;

procedure TJson.SaveToFile(Const FName:RawByteString);
Var
 M:TMemoryStream;
begin
 if (P=nil) then Exit;
 M:=TMemoryStream.Create;
 _Dump(P,M,true);
 M.SaveToFile(FName);
 M.Free;
end;

procedure TJson.Post(CB:TPostCallback);
Var
 I:TJsonIterate;
begin
 if (CB=nil) then Exit;
 I:=TJsonIterate.New(Self);
 repeat
  Case I.Item.GetType of
   jtNumber,
   jtString,
   jtBoolean,
   jtNull:CB(I.XPath,I.Item.AsStr);
  end;
 until not I.Next;
end;

Function TJsonStack2.Top:PJPos;
begin
 Result:=nil;
 if FCount>0 then
  Result:=@FMas[FCount-1];
end;

Procedure TJsonStack2.Push;
begin
 Inc(FCount);
 if FCount>Length(FMas) then
  SetLength(FMas,FCount);
 FMas[FCount-1]:=Default(TJPos);
end;

Procedure TJsonStack2.Pop;
begin
 if FCount<>0 then
 begin
  Dec(FCount);
  FMas[FCount]:=Default(TJPos);
 end;
end;

function TJson.Clone:TJson;
Var
 J:TJsonStack2;
 I,C:PJSON_Data;
begin
 Result.P:=nil;
 if (P=nil) then Exit;
 J:=Default(TJsonStack2);
 I:=P;
 C:=I^._Copy;
 Result.P:=C;

 repeat

  Case I^.Item.jType of
   jitArray,
   jitObject:
   begin
    if I^.Count>0 then
    begin
     J.Push;
     With J.Top^ do
     begin
      LD:=I;
      CD:=C;
      LI:=0;
      I:=LD^.GetItem(0);
      if I=nil then Exit;
      C:=I^._Copy;
      CD^.SetItem(0,C);
     end;
     Continue;
    end;
   end;
  end;

  repeat
   if J.Top=nil then Exit;
   With J.Top^ do
   begin
    Inc(LI);
    if LI<LD^.Count then
    begin
     I:=LD^.GetItem(LI);
     if I=nil then Exit;
     C:=I^._Copy;
     CD^.SetItem(LI,C);
     Break;
    end else
    begin
     I:=LD;
     C:=CD;
     J.Pop;
    end;
   end;
  until false;

 until false;
end;

end.


