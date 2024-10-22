unit game_info;

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

 TAbstractObject=class
  Function    GetPropertyIterator:TRttiPropertyIterator;
  Procedure   Serialize  (Stream:TStream);      virtual;
  Procedure   Deserialize(Stream:TStream);      virtual;
  Procedure   CopyTo     (dst:TAbstractObject); virtual;
  Procedure   CreateSub;                        virtual;
  Procedure   DestroySub;                       virtual;
  Procedure   WriteJSON  (const name:RawByteString;Stream:TJSONStreamWriter); virtual;
  Constructor Create;  virtual;
  Destructor  Destroy; override;
 end;

 TAbstractObjectClass=class of TAbstractObject;

 TAbstractArray=class(TAbstractObject)
  Function  GetArrayCount:SizeInt;            virtual; abstract;
  Function  GetArrayItem(i:SizeInt):TValue;   virtual; abstract;
  Function  AddObject:TAbstractObject;        virtual; abstract;
  Function  AddArray :TAbstractArray;         virtual; abstract;
  procedure AddValue (Value:TValue);          virtual; abstract;
  //
  Procedure Serialize  (Stream:TStream);      override;
  Procedure Deserialize(Stream:TStream);      override;
  Procedure CopyTo     (dst:TAbstractObject); override;
  Procedure WriteJSON  (const name:RawByteString;Stream:TJSONStreamWriter); override;
 end;

 TJSONStreamReader=class(TBaseJSONReader)
   type
    PJNode=^TJNode;
    TJNode=record
     FObject:TAbstractObject;
     FRType :TRTTIType;
    end;
   Var
    FCtx  :TRTTIContext;
    FStack:array of TJNode;
    FCount:SizeUInt;
    FItem :TRttiProperty;
    FRoot :TJNode;

   Procedure  Execute(obj:TAbstractObject);

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

 TBootParamInfo=class(TAbstractObject)
 private
  FNeo                :Boolean;
  Fhalt_on_exit       :Boolean;
  Fprint_guest_syscall:Boolean;
  Fprint_pmap         :Boolean;
  Fprint_jit_preload  :Boolean;
  Fprint_gpu_ops      :Boolean;
  Fprint_gpu_hint     :Boolean;
 published
  property neo                :Boolean read FNeo                 write FNeo                ;
  property halt_on_exit       :Boolean read Fhalt_on_exit        write Fhalt_on_exit       ;
  property print_guest_syscall:Boolean read Fprint_guest_syscall write Fprint_guest_syscall;
  property print_pmap         :Boolean read Fprint_pmap          write Fprint_pmap         ;
  property print_jit_preload  :Boolean read Fprint_jit_preload   write Fprint_jit_preload  ;
  property print_gpu_ops      :Boolean read Fprint_gpu_ops       write Fprint_gpu_ops      ;
  property print_gpu_hint     :Boolean read Fprint_gpu_hint      write Fprint_gpu_hint     ;
 end;

 TJITInfo=class(TAbstractObject)
 private
  Fprint_asm       :Boolean;
  Fdebug_info      :Boolean;
  Frelative_analize:Boolean;
  Fmemory_guard    :Boolean;
 published
  property print_asm       :Boolean read Fprint_asm        write Fprint_asm       ;
  property debug_info      :Boolean read Fdebug_info       write Fdebug_info      ;
  property relative_analize:Boolean read Frelative_analize write Frelative_analize;
  property memory_guard    :Boolean read Fmemory_guard     write Fmemory_guard    ;
 public
  Constructor Create; override;
 end;

 TMainInfo=class(TAbstractObject)
 private
  FLogFile  :RawByteString;
  Fsystem   :RawByteString;
  Fdata     :RawByteString;
  Ffork_proc:Boolean;
 published
  property LogFile  :RawByteString read FLogFile   write FLogFile;
  property system   :RawByteString read Fsystem    write Fsystem;
  property data     :RawByteString read Fdata      write Fdata;
  property fork_proc:Boolean       read Ffork_proc write Ffork_proc;
 public
  Constructor Create; override;
 end;

 TMiscInfo=class(TAbstractObject)
 private
  Fstrict_ps4_freq  :Boolean;
  Frenderdoc_capture:Boolean;
 published
  property strict_ps4_freq  :Boolean read Fstrict_ps4_freq   write Fstrict_ps4_freq;
  property renderdoc_capture:Boolean read Frenderdoc_capture write Frenderdoc_capture;
 end;

 TVulkanInfo=class(TAbstractObject)
 private
  Fdevice:RawByteString;
  Fapp_flags:DWORD;
 published
  property device:RawByteString read Fdevice write Fdevice;
  property app_flags:DWORD read Fapp_flags write Fapp_flags;
 end;

 TConfigInfo=class(TAbstractObject)
  private
   FMainInfo     :TMainInfo;
   FBootParamInfo:TBootParamInfo;
   FJITInfo      :TJITInfo;
   FMiscInfo     :TMiscInfo;
   FVulkanInfo   :TVulkanInfo;
  published
   property MainInfo     :TMainInfo      read FMainInfo      write FMainInfo;
   property BootParamInfo:TBootParamInfo read FBootParamInfo write FBootParamInfo;
   property JITInfo      :TJITInfo       read FJITInfo       write FJITInfo;
   property MiscInfo     :TMiscInfo      read FMiscInfo      write FMiscInfo;
   property VulkanInfo   :TVulkanInfo    read FVulkanInfo    write FVulkanInfo;
 end;

 TGameInfo=class(TAbstractObject)
 private
  FName   :RawByteString;
  FTitleId:RawByteString;
  FVersion:RawByteString;
  FExec   :RawByteString;
  FParam  :RawByteString;
 published
  property Name   :RawByteString read FName    write FName;
  property TitleId:RawByteString read FTitleId write FTitleId;
  property Version:RawByteString read FVersion write FVersion;
  property Exec   :RawByteString read FExec    write FExec;
  property Param  :RawByteString read FParam   write FParam;
 public
  Constructor Create; override;
 end;

 TMountList=class(TAbstractObject)
  private
   Fapp0  :RawByteString;
   Fsystem:RawByteString;
   Fdata  :RawByteString;
  published
   property app0  :RawByteString read Fapp0   write Fapp0  ;
   property system:RawByteString read Fsystem write Fsystem;
   property data  :RawByteString read Fdata   write Fdata  ;
  public
   Constructor Create; override;
 end;

 TGameItem=class(TAbstractObject)
  public
   FGameInfo :TGameInfo;
   FMountList:TMountList;
   FLock     :Boolean;
  published
   property GameInfo :TGameInfo  read FGameInfo  write FGameInfo;
   property MountList:TMountList read FMountList write FMountList;
 end;

 TGameStartupInfo=class(TAbstractObject)
  public
   FReader  :Boolean;
   FPipe    :THandle;
   FConfInfo:TConfigInfo;
   FGameItem:TGameItem;
  public
   Constructor Create(Reader:Boolean); reintroduce;
   Destructor  Destroy; override;
   Procedure   Serialize  (Stream:TStream); override;
   Procedure   Deserialize(Stream:TStream); override;
 end;

implementation

//

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

Function TAbstractObject.GetPropertyIterator:TRttiPropertyIterator;
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

Procedure TJSONStreamReader.Execute(obj:TAbstractObject);
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
  if P^.FObject.InheritsFrom(TAbstractArray) then
  begin
   TAbstractArray(P^.FObject).AddValue(Value);
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
 obj:TAbstractObject;
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
    obj:=TAbstractObject(FItem.GetValue(P^.FObject).AsObject);
   end;
  end else
  if P^.FObject.InheritsFrom(TAbstractArray) then
  begin
   obj:=TAbstractArray(P^.FObject).AddArray;
  end;
 end;

 if (obj<>nil) then
 if (not obj.InheritsFrom(TAbstractArray)) then
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
 obj:TAbstractObject;
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
    obj:=TAbstractObject(FItem.GetValue(P^.FObject).AsObject);
   end;
  end else
  if P^.FObject.InheritsFrom(TAbstractArray) then
  begin
   obj:=TAbstractArray(P^.FObject).AddObject;
  end;
 end;

 if (obj<>nil) then
 if (not obj.InheritsFrom(TAbstractObject)) then
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

Procedure TAbstractObject.Serialize(Stream:TStream);
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
       if obj.InheritsFrom(TAbstractObject) then
       begin
        TAbstractObject(obj).Serialize(Stream);
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

Procedure TAbstractObject.Deserialize(Stream:TStream);
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
       if obj.InheritsFrom(TAbstractObject) then
       begin
        TAbstractObject(obj).Deserialize(Stream);
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

Procedure TAbstractObject.CopyTo(dst:TAbstractObject);
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
       if obj_src.InheritsFrom(TAbstractObject) then
       if obj_dst.InheritsFrom(obj_src.ClassType) then
       begin
        TAbstractObject(obj_src).CopyTo(TAbstractObject(obj_dst));
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

Procedure TAbstractObject.CreateSub;
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

    if _class.InheritsFrom(TAbstractObject.ClassType) then
    begin
     obj:=TAbstractObjectClass(_class).Create;
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

Procedure TAbstractObject.DestroySub;
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

Constructor TAbstractObject.Create;
begin
 inherited;
 CreateSub;
end;

Destructor TAbstractObject.Destroy;
begin
 DestroySub;
 inherited;
end;

procedure TAbstractObject.WriteJSON(const name:RawByteString;Stream:TJSONStreamWriter);
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
       if obj.InheritsFrom(TAbstractObject) then
       begin
        TAbstractObject(obj).WriteJSON(p.Name,Stream);
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

Procedure TAbstractArray.Serialize(Stream:TStream);
var
 i,c:SizeInt;
 V:TValue;
 obj:TObject;
begin
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
      if obj.InheritsFrom(TAbstractObject) then
      begin
       TAbstractObject(obj).Serialize(Stream);
      end;
     end;

   else
    Assert(false);
  end;

 end;
end;

Procedure TAbstractArray.Deserialize(Stream:TStream);
var
 i,c:SizeInt;
 Kind:TTypeKind;
 V:TValue;
 obj:TObject;
begin
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
      if obj.InheritsFrom(TAbstractObject) then
      begin
       TAbstractObject(obj).Deserialize(Stream);
      end;
     end;

   else
    Assert(false);
  end;

  //save
  AddValue(V);
 end;

end;

Procedure TAbstractArray.CopyTo(dst:TAbstractObject);
begin
 Assert(false);
end;

Procedure TAbstractArray.WriteJSON(const name:RawByteString;Stream:TJSONStreamWriter);
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
      if obj.InheritsFrom(TAbstractObject) then
      begin
       TAbstractObject(obj).WriteJSON('',Stream);
      end;
     end;

   else
    Assert(false);
  end;

 end;
 //
 Stream.WriteStopArray;
end;

Constructor TJITInfo.Create;
begin
 inherited;
 Frelative_analize:=True;
end;

Constructor TMainInfo.Create;
begin
 inherited;
 FLogFile:='log.txt';
 Fsystem :=DirectorySeparator+'system';
 Fdata   :=DirectorySeparator+'data';
 Ffork_proc:=True;
end;

Constructor TGameInfo.Create;
begin
 inherited;
 FExec:='/app0/eboot.bin';
 FTitleId:='???';
 FVersion:='???';
end;

Constructor TMountList.Create;
begin
 inherited;
 Fapp0  :=DirectorySeparator;
 Fsystem:=DirectorySeparator+'system';
 Fdata  :=DirectorySeparator+'data';
end;

//

Constructor TGameStartupInfo.Create(Reader:Boolean);
begin
 inherited Create;
 FReader:=Reader;
 if FReader then
 begin
  FConfInfo:=TConfigInfo.Create;
  FGameItem:=TGameItem.Create;
 end;
end;

Destructor TGameStartupInfo.Destroy;
begin
 if FReader then
 begin
  FreeAndNil(FConfInfo);
  FreeAndNil(FGameItem);
 end;
 inherited;
end;

Procedure TGameStartupInfo.Serialize(Stream:TStream);
begin
 Stream.Write(FPipe,SizeOf(THandle));
 FConfInfo.Serialize(Stream);
 FGameItem.Serialize(Stream);
end;

Procedure TGameStartupInfo.Deserialize(Stream:TStream);
begin
 FPipe:=0;
 Stream.Read(FPipe,SizeOf(THandle));
 FConfInfo.Deserialize(Stream);
 FGameItem.Deserialize(Stream);
end;

end.


