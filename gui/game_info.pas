unit game_info;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Rtti,
  IniFiles,
  host_ipc;

type
 TRttiPropertyIterator=object
  Ctx:TRTTIContext;
  A  :specialize TArray<TRttiProperty>;
  i  :Integer;
  Procedure Free;
  function  GetProperty:TRttiProperty;
  function  Next:Boolean;
 end;

 TAbstractInfo=class
  Function    GetPropertyIterator:TRttiPropertyIterator;
  Procedure   Serialize  (Stream:TStream);    virtual;
  Procedure   Deserialize(Stream:TStream);    virtual;
  Procedure   CopyTo     (dst:TAbstractInfo); virtual;
  Procedure   CreateSub;                      virtual;
  Procedure   DestroySub;                     virtual;
  procedure   ReadIni    (INI:TIniFile;const Section:RawByteString);
  procedure   WriteIni   (INI:TIniFile;const Section:RawByteString);
  Constructor Create;  virtual;
  Destructor  Destroy; override;
 end;

 TAbstractInfoClass=class of TAbstractInfo;

 TBootParamInfo=class(TAbstractInfo)
 private
  FNeo                :Boolean;
  Fhalt_on_exit       :Boolean;
  Fprint_guest_syscall:Boolean;
  Fprint_pmap         :Boolean;
  Fprint_jit_preload  :Boolean;
 published
  property neo                :Boolean read FNeo                 write FNeo                ;
  property halt_on_exit       :Boolean read Fhalt_on_exit        write Fhalt_on_exit       ;
  property print_guest_syscall:Boolean read Fprint_guest_syscall write Fprint_guest_syscall;
  property print_pmap         :Boolean read Fprint_pmap          write Fprint_pmap         ;
  property print_jit_preload  :Boolean read Fprint_jit_preload   write Fprint_jit_preload  ;
 end;

 TMainInfo=class(TAbstractInfo)
 private
  FLogFile  :RawByteString;
  Ffork_proc:Boolean;
 published
  property LogFile  :RawByteString read FLogFile   write FLogFile;
  property fork_proc:Boolean       read Ffork_proc write Ffork_proc;
 public
  Constructor Create; override;
 end;

 TMainConfigInfo=class(TAbstractInfo)
  private
   FMainInfo     :TMainInfo;
   FBootParamInfo:TBootParamInfo;
  published
   property MainInfo     :TMainInfo      read FMainInfo write FMainInfo;
   property BootParamInfo:TBootParamInfo read FBootParamInfo write FBootParamInfo;
 end;

 TGameInfo=class(TAbstractInfo)
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

 TMountList=class(TAbstractInfo)
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

 TGameItem=class(TAbstractInfo)
  public
   FSecton   :RawByteString;
   FGameInfo :TGameInfo ;
   FMountList:TMountList;
   FLock     :Boolean;
  public
   Constructor Create; override;
   Destructor  Destroy; override;
   Procedure   Serialize(Stream:TStream); override;
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

Function TAbstractInfo.GetPropertyIterator:TRttiPropertyIterator;
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

Procedure TAbstractInfo.Serialize(Stream:TStream);
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

   case p.PropertyType.TypeKind of

    tkSString,
    tkLString,
    tkAString:Stream.WriteAnsiString(p.GetValue(Self).AsString);

    tkBool:Stream.WriteByte(Byte(p.GetValue(Self).AsBoolean));

    tkClass:
      begin
       obj:=p.GetValue(Self).AsObject;

       if (obj<>nil) then
       if obj.InheritsFrom(TAbstractInfo) then
       begin
        TAbstractInfo(obj).Serialize(Stream);
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

Procedure TAbstractInfo.Deserialize(Stream:TStream);
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

   case p.PropertyType.TypeKind of
    tkSString,
    tkLString,
    tkAString:p.SetValue(Self,Stream.ReadAnsiString);

    tkBool:p.SetValue(Self,Boolean(Stream.ReadByte));

    tkClass:
      begin
       obj:=p.GetValue(Self).AsObject;

       if (obj<>nil) then
       if obj.InheritsFrom(TAbstractInfo) then
       begin
        TAbstractInfo(obj).Deserialize(Stream);
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

Procedure TAbstractInfo.CopyTo(dst:TAbstractInfo);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj_src:TObject;
 obj_dst:TObject;
begin
 if (dst=nil) then Exit;
 if (not dst.InheritsFrom(Self.ClassType)) then Exit;

 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of
    tkSString,
    tkLString,
    tkAString,
    tkBool   :p.SetValue(dst,p.GetValue(Self));

    tkClass:
      begin
       obj_src:=p.GetValue(Self).AsObject;
       obj_dst:=p.GetValue(dst ).AsObject;

       if (obj_src<>nil) and (obj_dst<>nil) then
       if obj_src.InheritsFrom(TAbstractInfo) then
       if obj_dst.InheritsFrom(obj_src.ClassType) then
       begin
        TAbstractInfo(obj_src).CopyTo(TAbstractInfo(obj_dst));
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

Procedure TAbstractInfo.CreateSub;
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

    if _class.InheritsFrom(TAbstractInfo.ClassType) then
    begin
     obj:=TAbstractInfoClass(_class).Create;
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

Procedure TAbstractInfo.DestroySub;
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

    p.SetValue(Self,TObject(nil));
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

Constructor TAbstractInfo.Create;
begin
 inherited;
 CreateSub;
end;

Destructor TAbstractInfo.Destroy;
begin
 DestroySub;
 inherited;
end;

procedure TAbstractInfo.ReadIni(INI:TIniFile;const Section:RawByteString);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 V:RawByteString;
 B:Boolean;
begin
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of

    tkSString,
    tkLString,
    tkAString:
      begin
       V:=Trim(p.GetValue(Self).AsString);
       V:=Trim(INI.ReadString(Section,p.Name,V));
       p.SetValue(Self,V);
      end;

    tkBool:
      begin
       B:=p.GetValue(Self).AsBoolean;
       B:=INI.ReadBool(Section,p.Name,B);
       p.SetValue(Self,B);
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

procedure TAbstractInfo.WriteIni(INI:TIniFile;const Section:RawByteString);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 V:RawByteString;
 B:Boolean;
begin
 i:=GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of

    tkSString,
    tkLString,
    tkAString:
      begin
       V:=Trim(p.GetValue(Self).AsString);
       INI.WriteString(Section,p.Name,V);
      end;

    tkBool:
      begin
       B:=p.GetValue(Self).AsBoolean;
       INI.WriteBool(Section,p.Name,B);
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

Constructor TMainInfo.Create;
begin
 inherited;
 FLogFile:='log.txt';
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

Constructor TGameItem.Create;
begin
 inherited;
 FGameInfo :=TGameInfo .Create;
 FMountList:=TMountList.Create;
 inherited;
end;

Destructor TGameItem.Destroy;
begin
 FreeAndNil(FGameInfo );
 FreeAndNil(FMountList);
 inherited;
end;

Procedure TGameItem.Serialize(Stream:TStream);
begin
 FGameInfo .Serialize(Stream);
 FMountList.Serialize(Stream);
end;

Procedure TGameItem.Deserialize(Stream:TStream);
begin
 FGameInfo .Deserialize(Stream);
 FMountList.Deserialize(Stream);
end;

end.


