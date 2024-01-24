unit game_info;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
 TMainInfo=class
 private
  FLogFile:RawByteString;
 published
  property LogFile:RawByteString read FLogFile write FLogFile;
 public
  Constructor Create;
 end;

type
 TGameInfo=class
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

 TMountList=class
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

 TGameItem=class
  public
   FSecton   :RawByteString;
   FGameInfo :TGameInfo ;
   FMountList:TMountList;
   FLock     :Boolean;
  public
   Constructor Create;
   Destructor  Destroy; override;
 end;

implementation

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
 Fapp0  :='/';
 Fsystem:='/system';
 Fdata  :='/data';
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

end.


