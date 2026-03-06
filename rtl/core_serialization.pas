unit core_serialization;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  jsonreader,
  fpjson;

type
 TRttiPropertyIterator=object
  Ctx:TRTTIContext;
  A  :specialize TArray<TRttiProperty>;
  i  :Integer;
  Procedure Free;
  function  GetProperty:TRttiProperty;
  function  Next:Boolean;
 end;

 TJSONStreamWriter=class
  FLevel :Byte;
  FState :(swInit,swFirst,swNext);
  FStream:TStream;
  constructor Create(Stream:TStream);
  function    GetStateStr:RawByteString;
  Procedure   WriteStartObject(const name:RawByteString);
  Procedure   WriteStopObject;
  Procedure   WriteStartArray(const name:RawByteString);
  Procedure   WriteStopArray;
  Procedure   WriteValue(const name:RawByteString;Value:TValue);
 end;

 TSerializeObject=class
  Function    GetPropertyIterator:TRttiPropertyIterator;
  Procedure   Serialize  (Stream:TStream);       virtual;
  Procedure   Deserialize(Stream:TStream);       virtual;
  Procedure   CopyTo     (dst:TSerializeObject); virtual;
  Procedure   CreateSub;                         virtual;
  Procedure   DestroySub;                        virtual;
  Procedure   WriteJSON  (const name:RawByteString;Stream:TJSONStreamWriter); virtual;
  Constructor Create;  virtual;
  Destructor  Destroy; override;
 end;

 TSerializeObjectClass=class of TSerializeObject;

 TSerializeArray=class(TSerializeObject)
  Function  GetArrayCount:SizeInt;             virtual; abstract;
  Function  GetArrayItem(i:SizeInt):TValue;    virtual; abstract;
  Function  AddObject:TSerializeObject;        virtual; abstract;
  Function  AddArray :TSerializeArray;         virtual; abstract;
  procedure AddValue (Value:TValue);           virtual; abstract;
  //
  Procedure Serialize  (Stream:TStream);       override;
  Procedure Deserialize(Stream:TStream);       override;
  Procedure CopyTo     (dst:TSerializeObject); override;
  Procedure WriteJSON  (const name:RawByteString;Stream:TJSONStreamWriter); override;
 end;

 TJSONStreamReader=class(TBaseJSONReader)
   type
    PJNode=^TJNode;
    TJNode=record
     FObject:TSerializeObject;
     FRType :TRTTIType;
    end;
   Var
    FCtx  :TRTTIContext;
    FStack:array of TJNode;
    FCount:SizeUInt;
    FItem :TRttiProperty;
    FRoot :TJNode;

   Procedure  Execute(obj:TSerializeObject);

   Procedure  Push;
   Procedure  Pop;
   Function   Top:PJNode;

   Procedure  KeyValue    (Const AKey:TJSONStringType);   override;
   Procedure  SetValue    (Value:TValue);
   Procedure  StringValue (Const AValue:TJSONStringType); override;
   Procedure  NumberValue (Const AValue:TJSONStringType); override;
   Procedure  NullValue;   override;
   Procedure  FloatValue  (Const AValue:Double);  override;
   Procedure  BooleanValue(Const AValue:Boolean); override;
   Procedure  IntegerValue(Const AValue:integer); override;
   Procedure  Int64Value  (Const AValue:int64);   override;
   Procedure  QWordValue  (Const AValue:QWord);   override;
   Procedure  StartArray;  override;
   Procedure  StartObject; override;
   Procedure  EndArray;    override;
   Procedure  EndObject;   override;
   Destructor Destroy();   override;
 end;

 TSerializeStringArray=class(TSerializeArray)
  values:array of RawByteString;
  //
  Destructor Destroy; override;
  //
  Function   GetArrayCount:SizeInt;          override;
  Function   GetArrayItem(i:SizeInt):TValue; override;
  Function   AddObject:TSerializeObject;     override;
  Function   AddArray :TSerializeArray;      override;
  procedure  AddValue(Value:TValue);         override;
 end;

implementation

Procedure TRttiPropertyIterator.Free;
begin
 Ctx.Free;
end;

function TRttiPropertyIterator.GetProperty:TRttiProperty;
begin
 Result:=nil;
 if (i<Length(A)) then
 begin
  Result:=A[i];
 end;
end;

function TRttiPropertyIterator.Next:Boolean;
begin
 Result:=False;
 if (i<Length(A)) then
 begin
  Inc(i);
  Result:=(i<Length(A));
 end;
end;

//

Function TSerializeObject.GetPropertyIterator:TRttiPropertyIterator;
var
 RT:TRTTIType;
begin
 try
  Result.Ctx:=TRTTIContext.Create;
  //
  RT:=Result.Ctx.GetType(Self.ClassInfo);
  Result.A:=rt.GetProperties;
  Result.i:=0;
 finally
  //
 end;
end;

//

constructor TJSONStreamWriter.Create(Stream:TStream);
begin
 FState :=swInit;
 FStream:=Stream;
end;

function TJSONStreamWriter.GetStateStr:RawByteString;
begin
 case FState of
  swInit :Result:='';
  swFirst:Result:=#13#10;
  swNext :Result:=','#13#10;
 end;
end;

Procedure TJSONStreamWriter.WriteStartObject(const name:RawByteString);
var
 S:RawByteString;
begin
 S:=GetStateStr;

 if (name='') then
 begin
  S:=S+Space(FLevel)+'{';
 end else
 begin
  S:=S+Space(FLevel)+'"'+StringToJSONString(name,False)+'": {';
 end;

 Inc(FLevel);
 FState:=swFirst;
 FStream.Write(PChar(S)^,Length(S));
end;

Procedure TJSONStreamWriter.WriteStopObject;
var
 S:RawByteString;
begin
 Assert(FLevel>0,'WriteStopObject');

 Dec(FLevel);
 FState:=swNext;

 S:=#13#10+Space(FLevel)+'}';

 FStream.Write(PChar(S)^,Length(S));
end;

Procedure TJSONStreamWriter.WriteStartArray(const name:RawByteString);
var
 S:RawByteString;
begin
 S:=GetStateStr;

 if (name='') then
 begin
  S:=S+Space(FLevel)+'[';
 end else
 begin
  S:=S+Space(FLevel)+'"'+StringToJSONString(name,False)+'": [';
 end;

 Inc(FLevel);
 FState:=swFirst;
 FStream.Write(PChar(S)^,Length(S));
end;

Procedure TJSONStreamWriter.WriteStopArray;
var
 S:RawByteString;
begin
 Assert(FLevel>1,'WriteStopArray');

 Dec(FLevel);
 FState:=swNext;

 S:=#13#10+Space(FLevel)+']';

 FStream.Write(PChar(S)^,Length(S));
end;

Procedure TJSONStreamWriter.WriteValue(const name:RawByteString;Value:TValue);
var
 S:RawByteString;
begin
 S:=GetStateStr;

 if (name='') then
 begin
  S:=S+Space(FLevel);
 end else
 begin
  S:=S+Space(FLevel)+'"'+StringToJSONString(name,False)+'": ';
 end;

 case Value.Kind of

  tkSString,
  tkLString,
  tkAString:S:=S+'"'+StringToJSONString(value.AsString,False)+'"';

  tkInteger:S:=S+IntToStr(value.AsInteger);
  tkQWord  :S:=S+IntToStr(value.AsUInt64);

  tkBool   :S:=S+BoolToStr(value.AsBoolean,'true','false');

  else
   Assert(False);
 end;

 FState:=swNext;
 FStream.Write(PChar(S)^,Length(S));
end;

//

Procedure TJSONStreamReader.Execute(obj:TSerializeObject);
begin
 FRoot.FObject:=obj;
 DoExecute;
end;

Procedure TJSONStreamReader.Push;
begin
 Inc(FCount);
 if (FCount>Length(FStack)) then
 begin
  SetLength(FStack,FCount);
 end;
 FStack[FCount-1]:=Default(TJNode);
end;

Procedure TJSONStreamReader.Pop;
begin
 if FCount<>0 then
 begin
  Dec(FCount);
  FStack[FCount]:=Default(TJNode);
 end;
end;

Function TJSONStreamReader.Top:PJNode;
begin
 Result:=@FRoot;
 if (FCount>0) then
 begin
  Result:=@FStack[FCount-1];
 end;
end;

Procedure TJSONStreamReader.KeyValue(Const AKey:TJSONStringType);
Var
 P:PJNode;
begin
 P:=Top;
 if (P^.FObject<>nil) then
 begin
  //
  if (P^.FRType=nil) then
  begin
   if (CompareByte(FCtx,Default(TRTTIContext),SizeOf(TRTTIContext))=0) then
   begin
    FCtx:=TRTTIContext.Create;
   end;
   //
   P^.FRType:=FCtx.GetType(P^.FObject.ClassType);
  end;
  //
  FItem:=P^.FRType.GetProperty(AKey);
 end else
 begin
  FItem:=nil;
 end;
end;

Procedure TJSONStreamReader.SetValue(Value:TValue);
Var
 P:PJNode;
begin
 P:=Top;
 if (P^.FObject<>nil) then
 begin
  if (FItem<>nil) then
  begin
   FItem.SetValue(P^.FObject,Value);
  end else
  if P^.FObject.InheritsFrom(TSerializeArray) then
  begin
   TSerializeArray(P^.FObject).AddValue(Value);
  end;
 end;
end;

Procedure TJSONStreamReader.StringValue(Const AValue:TJSONStringType);
begin
 SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONStreamReader.NumberValue(Const AValue:TJSONStringType);
begin
end;

Procedure TJSONStreamReader.NullValue;
begin
 SetValue(TValue.Empty);
 FItem:=nil;
end;

Procedure TJSONStreamReader.FloatValue(Const AValue:Double);
begin
 SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONStreamReader.BooleanValue(Const AValue:Boolean);
begin
 SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONStreamReader.IntegerValue(Const AValue:integer);
begin
 SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONStreamReader.Int64Value(Const AValue:int64);
begin
 SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONStreamReader.QWordValue(Const AValue:QWord);
begin
 SetValue(AValue);
 FItem:=nil;
end;

Procedure TJSONStreamReader.StartArray;
Var
 P:PJNode;
 obj:TSerializeObject;
begin
 obj:=nil;
 P:=Top;
 if (P=@FRoot) then
 begin
  obj:=FRoot.FObject;
 end else
 if (P^.FObject<>nil) then
 begin
  if (FItem<>nil) then
  begin
   if (FItem.PropertyType.TypeKind=tkClass) then
   begin
    obj:=TSerializeObject(FItem.GetValue(P^.FObject).AsObject);
   end;
  end else
  if P^.FObject.InheritsFrom(TSerializeArray) then
  begin
   obj:=TSerializeArray(P^.FObject).AddArray;
  end;
 end;

 if (obj<>nil) then
 if (not obj.InheritsFrom(TSerializeArray)) then
 begin
  obj:=nil;
 end;

 Push;
 Top^.FObject:=obj;
 FItem:=nil;
end;

Procedure TJSONStreamReader.StartObject;
Var
 P:PJNode;
 obj:TSerializeObject;
begin
 obj:=nil;
 P:=Top;
 if (P=@FRoot) then
 begin
  obj:=FRoot.FObject;
 end else
 if (P^.FObject<>nil) then
 begin
  if (FItem<>nil) then
  begin
   if (FItem.PropertyType.TypeKind=tkClass) then
   begin
    obj:=TSerializeObject(FItem.GetValue(P^.FObject).AsObject);
   end;
  end else
  if P^.FObject.InheritsFrom(TSerializeArray) then
  begin
   obj:=TSerializeArray(P^.FObject).AddObject;
  end;
 end;

 if (obj<>nil) then
 if (not obj.InheritsFrom(TSerializeObject)) then
 begin
  obj:=nil;
 end;

 Push;
 Top^.FObject:=obj;
 FItem:=nil;
end;

Procedure TJSONStreamReader.EndArray;
begin
 Pop;
end;

Procedure TJSONStreamReader.EndObject;
begin
 Pop;
end;

Destructor TJSONStreamReader.Destroy();
begin
 FCtx.Free;
 inherited;
end;

//

Procedure TSerializeObject.Serialize(Stream:TStream);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
 TypeKind:TTypeKind;
begin
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString:Stream.WriteAnsiString(p.GetValue(Self).AsString);

    tkInteger:Stream.WriteDWord(p.GetValue(Self).AsInteger);
    tkQWord  :Stream.WriteQWord(p.GetValue(Self).AsInteger);

    tkBool   :Stream.WriteByte(Byte(p.GetValue(Self).AsBoolean));

    tkClass:
      begin
       obj:=p.GetValue(Self).AsObject;

       if (obj<>nil) then
       if obj.InheritsFrom(TSerializeObject) then
       begin
        TSerializeObject(obj).Serialize(Stream);
       end;
      end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

Procedure TSerializeObject.Deserialize(Stream:TStream);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
 TypeKind:TTypeKind;
begin
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin
   p:=i.GetProperty;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString:p.SetValue(Self,Stream.ReadAnsiString);

    tkInteger:p.SetValue(Self,Integer(Stream.ReadDWord));
    tkQWord  :p.SetValue(Self,QWord  (Stream.ReadQWord));

    tkBool   :p.SetValue(Self,Boolean(Stream.ReadByte));

    tkClass:
      begin
       obj:=p.GetValue(Self).AsObject;

       if (obj<>nil) then
       if obj.InheritsFrom(TSerializeObject) then
       begin
        TSerializeObject(obj).Deserialize(Stream);
       end;
      end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

Procedure TSerializeObject.CopyTo(dst:TSerializeObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj_src:TObject;
 obj_dst:TObject;
 TypeKind:TTypeKind;
begin
 if (dst=nil) then Exit;
 if (not dst.InheritsFrom(Self.ClassType)) then Exit;

 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of
    tkSString,
    tkLString,
    tkAString,
    tkInteger,
    tkQWord  ,
    tkBool   :p.SetValue(dst,p.GetValue(Self));

    tkClass:
      begin
       obj_src:=p.GetValue(Self).AsObject;
       obj_dst:=p.GetValue(dst ).AsObject;

       if (obj_src<>nil) and (obj_dst<>nil) then
       if obj_src.InheritsFrom(TSerializeObject) then
       if obj_dst.InheritsFrom(obj_src.ClassType) then
       begin
        TSerializeObject(obj_src).CopyTo(TSerializeObject(obj_dst));
       end;
      end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

Procedure TSerializeObject.CreateSub;
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 _class:tClass;
 obj:TObject;
begin
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   if (p.PropertyType.TypeKind=tkClass) then
   begin
    _class:=p.PropertyType.AsInstance.MetaClassType;

    if _class.InheritsFrom(TSerializeObject.ClassType) then
    begin
     obj:=TSerializeObjectClass(_class).Create;
    end else
    begin
     obj:=_class.Create;
    end;

    p.SetValue(Self,obj);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

Procedure TSerializeObject.DestroySub;
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
begin
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   if (p.PropertyType.TypeKind=tkClass) then
   begin
    obj:=p.GetValue(Self).AsObject;

    if (obj<>nil) then
    begin
     obj.Free;
    end;

   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

Constructor TSerializeObject.Create;
begin
 inherited;
 CreateSub;
end;

Destructor TSerializeObject.Destroy;
begin
 DestroySub;
 inherited;
end;

procedure TSerializeObject.WriteJSON(const name:RawByteString;Stream:TJSONStreamWriter);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
 TypeKind:TTypeKind;
begin
 Stream.WriteStartObject(Name);
 //
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString,
    tkInteger,
    tkQWord  ,
    tkBool   :Stream.WriteValue(p.Name,p.GetValue(Self));

    tkClass:
      begin
       obj:=p.GetValue(Self).AsObject;

       if (obj<>nil) then
       if obj.InheritsFrom(TSerializeObject) then
       begin
        TSerializeObject(obj).WriteJSON(p.Name,Stream);
       end;
      end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
 //
 Stream.WriteStopObject;
end;

Procedure TSerializeArray.Serialize(Stream:TStream);
var
 i,c:SizeInt;
 V:TValue;
 obj:TObject;
begin
 //property
 inherited Serialize(Stream);
 //property

 c:=GetArrayCount;

 Stream.WriteQWord(c); //Size Header

 if (c<>0) then
 For i:=0 to c-1 do
 begin
  V:=GetArrayItem(i);

  Stream.WriteDWord(DWORD(V.Kind)); //Type Header

  case V.Kind of

   tkSString,
   tkLString,
   tkAString:Stream.WriteAnsiString(V.AsString);

   tkInteger:Stream.WriteDWord(V.AsInteger);
   tkQWord  :Stream.WriteQWord(V.AsUInt64);

   tkBool   :Stream.WriteByte(Byte(V.AsBoolean));

   tkClass:
     begin
      //Use Class Header?
      obj:=V.AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TSerializeObject) then
      begin
       TSerializeObject(obj).Serialize(Stream);
      end;
     end;

   else
    Assert(false);
  end;

 end;
end;

Procedure TSerializeArray.Deserialize(Stream:TStream);
var
 i,c:SizeInt;
 Kind:TTypeKind;
 V:TValue;
 obj:TObject;
begin
 //property
 inherited Deserialize(Stream);
 //property

 c:=Stream.ReadQWord; //Size Header

 if (c<>0) then
 For i:=0 to c-1 do
 begin
  V:=Default(TValue);

  Kind:=TTypeKind(Stream.ReadDWord); //Type Header

  case Kind of

   tkSString,
   tkLString,
   tkAString:V:=Stream.ReadAnsiString;

   tkInteger:V:=Integer(Stream.ReadDWord);
   tkQWord  :V:=QWord  (Stream.ReadQWord);

   tkBool   :V:=Boolean(Stream.ReadByte);

   tkClass:
     begin
      //Use Class Header?
      V:=AddObject;

      obj:=V.AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TSerializeObject) then
      begin
       TSerializeObject(obj).Deserialize(Stream);
      end;
     end;

   else
    Assert(false);
  end;

  //save
  AddValue(V);
 end;

end;

Procedure TSerializeArray.CopyTo(dst:TSerializeObject);
begin
 Assert(false);
end;

Procedure TSerializeArray.WriteJSON(const name:RawByteString;Stream:TJSONStreamWriter);
var
 i,c:SizeInt;
 V:TValue;
 obj:TObject;
begin
 Stream.WriteStartArray(name);
 //
 c:=GetArrayCount;
 if (c<>0) then
 For i:=0 to c-1 do
 begin
  V:=GetArrayItem(i);

  case V.Kind of

   tkSString,
   tkLString,
   tkAString,
   tkInteger,
   tkQWord  ,
   tkBool   :Stream.WriteValue('',V);

   tkClass:
     begin
      obj:=V.AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TSerializeObject) then
      begin
       TSerializeObject(obj).WriteJSON('',Stream);
      end;
     end;

   else
    Assert(false);
  end;

 end;
 //
 Stream.WriteStopArray;
end;

//

Destructor TSerializeStringArray.Destroy;
begin
 SetLength(values,0);
 inherited;
end;

Function TSerializeStringArray.GetArrayCount:SizeInt;
begin
 Result:=Length(values);
end;

Function TSerializeStringArray.GetArrayItem(i:SizeInt):TValue;
begin
 if (i>=Length(values)) then
 begin
  Result:=TValue.Empty;
 end else
 begin
  Result:=values[i];
 end;
end;

Function TSerializeStringArray.AddObject:TSerializeObject;
begin
 Result:=nil;
end;

Function TSerializeStringArray.AddArray:TSerializeArray;
begin
 Result:=nil;
end;

procedure TSerializeStringArray.AddValue(Value:TValue);
begin
 Insert(Value.AsString,values,Length(values));
end;

end.

