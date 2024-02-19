unit game_info;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  host_ipc;

type
 TAbstractInfo=class
  Procedure Serialize  (Stream:TStream); virtual;
  Procedure Deserialize(Stream:TStream); virtual;
  procedure ReadIni    (INI:TIniFile;const Section:RawByteString);
  procedure WriteIni   (INI:TIniFile;const Section:RawByteString);
 end;

 TMainInfo=class(TAbstractInfo)
 private
  FLogFile:RawByteString;
 published
  property LogFile:RawByteString read FLogFile write FLogFile;
 public
  Constructor Create;
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
  Constructor Create;
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
   Constructor Create;
 end;

 TGameItem=class(TAbstractInfo)
  public
   FSecton   :RawByteString;
   FGameInfo :TGameInfo ;
   FMountList:TMountList;
   FLock     :Boolean;
  public
   Constructor Create;
   Destructor  Destroy; override;
   Procedure   Serialize  (Stream:TStream); override;
   Procedure   Deserialize(Stream:TStream); override;
 end;

 TGameProcess=class
  g_ipc  :THostIpcConnect;
  g_proc :THandle;
  g_p_pid:Integer;
  g_fork :Boolean;
  function   is_terminated:Boolean; virtual;
  procedure  suspend; virtual;
  procedure  resume;  virtual;
  procedure  stop;    virtual;
  Destructor Destroy; override;
 end;

implementation

uses
 TypInfo,Rtti;

function TGameProcess.is_terminated:Boolean;
begin
 Result:=False;
end;

procedure TGameProcess.suspend;
begin
 //
end;

procedure TGameProcess.resume;
begin
 //
end;

procedure TGameProcess.stop;
begin
 //
end;

Destructor TGameProcess.Destroy;
begin
 FreeAndNil(g_ipc);
 inherited;
end;

Procedure TAbstractInfo.Serialize(Stream:TStream);
var
 i,c:Integer;
 //
 Ctx:TRTTIContext;
 RT :TRTTIType;
 A  :specialize TArray<TRttiProperty>;
begin
 Ctx:=TRTTIContext.Create;
 try
  ///
  RT:=Ctx.GetType(Self.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    case A[i].PropertyType.TypeKind of
     tkSString,
     tkLString,
     tkAString:Stream.WriteAnsiString(A[i].GetValue(Self).AsString);
     else
      Assert(false);
    end;
   end;
  end;
  ///
 finally
  Ctx.free;
 end;
end;

Procedure TAbstractInfo.Deserialize(Stream:TStream);
var
 i,c:Integer;
 //
 Ctx:TRTTIContext;
 RT :TRTTIType;
 A  :specialize TArray<TRttiProperty>;
begin
 Ctx:=TRTTIContext.Create;
 try
  ///
  RT:=Ctx.GetType(Self.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    case A[i].PropertyType.TypeKind of
     tkSString,
     tkLString,
     tkAString:A[i].SetValue(Self,Stream.ReadAnsiString);
     else
      Assert(false);
    end;
   end;
  end;
  ///
 finally
  Ctx.free;
 end;
end;

procedure TAbstractInfo.ReadIni(INI:TIniFile;const Section:RawByteString);
var
 i,c:Integer;
 //
 Ctx:TRTTIContext;
 RT :TRTTIType;
 A  :specialize TArray<TRttiProperty>;
 V  :RawByteString;
begin
 Ctx:=TRTTIContext.Create;
 try
  ///
  RT:=Ctx.GetType(Self.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    V:=Trim(A[i].GetValue(Self).AsString);
    V:=Trim(INI.ReadString(Section,A[i].Name,V));
    A[i].SetValue(Self,V);
   end;
  end;
  ///
 finally
  Ctx.free;
 end;
end;

procedure TAbstractInfo.WriteIni(INI:TIniFile;const Section:RawByteString);
var
 i,c:Integer;
 //
 Ctx:TRTTIContext;
 RT :TRTTIType;
 A  :specialize TArray<TRttiProperty>;
 V  :RawByteString;
begin
 Ctx:=TRTTIContext.Create;
 try
  ///
  RT:=Ctx.GetType(Self.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    V:=Trim(A[i].GetValue(Self).AsString);
    INI.WriteString(Section,A[i].Name,V);
   end;
  end;
  ///
 finally
  Ctx.free;
 end;
end;

Constructor TMainInfo.Create;
begin
 FLogFile:='log.txt';
end;

Constructor TGameInfo.Create;
begin
 FExec:='/app0/eboot.bin';
 FTitleId:='???';
 FVersion:='???';
end;

Constructor TMountList.Create;
begin
 Fapp0  :=DirectorySeparator;
 Fsystem:=DirectorySeparator+'system';
 Fdata  :=DirectorySeparator+'data';
end;

Constructor TGameItem.Create;
begin
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


