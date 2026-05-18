unit game_info;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  core_serialization;

type
 TSerializeStringArray=core_serialization.TSerializeStringArray;

 TBootParamInfo=class(TSerializeObject)
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

 TJITInfo=class(TSerializeObject)
 private
  Fprint_asm       :Boolean;
  Fdebug_info      :Boolean;
  Frelative_analize:Boolean;
  Fscan_switchtable:Boolean;
  Fscan_nopsequence:Boolean;
  Fmemory_guard    :Boolean;
  Flazy_jit        :Boolean;
 published
  property print_asm       :Boolean read Fprint_asm        write Fprint_asm       ;
  property debug_info      :Boolean read Fdebug_info       write Fdebug_info      ;
  property relative_analize:Boolean read Frelative_analize write Frelative_analize;
  property scan_switchtable:Boolean read Fscan_switchtable write Fscan_switchtable;
  property scan_nopsequence:Boolean read Fscan_nopsequence write Fscan_nopsequence;
  property memory_guard    :Boolean read Fmemory_guard     write Fmemory_guard    ;
  property lazy_jit        :Boolean read Flazy_jit         write Flazy_jit        ;
 public
  Constructor Create; override;
 end;

 TMainInfo=class(TSerializeObject)
 private
  FLogFile        :RawByteString;
  FDefaultFirmware:RawByteString;
  FFirmwareList   :TSerializeStringArray;
 published
  property LogFile        :RawByteString         read FLogFile         write FLogFile;
  property DefaultFirmware:RawByteString         read FDefaultFirmware write FDefaultFirmware;
  property FirmwareList   :TSerializeStringArray read FFirmwareList    write FFirmwareList;
 public
  Constructor Create; override;
 end;

 TMiscInfo=class(TSerializeObject)
 private
  Ffork_proc        :Boolean;
  Fstrict_ps4_freq  :Boolean;
  Frenderdoc_capture:Boolean;
 published
  property fork_proc        :Boolean read Ffork_proc         write Ffork_proc;
  property strict_ps4_freq  :Boolean read Fstrict_ps4_freq   write Fstrict_ps4_freq;
  property renderdoc_capture:Boolean read Frenderdoc_capture write Frenderdoc_capture;
 public
  Constructor Create; override;
 end;

 TVulkanInfo=class(TSerializeObject)
 private
  Fdevice:RawByteString;
  Fapp_flags:DWORD;
 published
  property device:RawByteString read Fdevice write Fdevice;
  property app_flags:DWORD read Fapp_flags write Fapp_flags;
 end;

 TPS4SystemService=class(TSerializeObject)
 private
  FSystemName  :RawByteString;
  FLanguage    :ShortInt;
  FDateFormat  :ShortInt;
  FTimeFormat  :ShortInt;
  FButtonAssign:Byte;
 published
  property SystemName  :RawByteString read FSystemName   write FSystemName;
  property Language    :ShortInt      read FLanguage     write FLanguage;
  property DateFormat  :ShortInt      read FDateFormat   write FDateFormat;
  property TimeFormat  :ShortInt      read FTimeFormat   write FTimeFormat;
  property ButtonAssign:Byte          read FButtonAssign write FButtonAssign;
 public
  Constructor Create; override;
 end;

 TPS4Audio=class(TSerializeObject)
 private
  FMainDevice      :RawByteString;
  FHeadphoneDevice :RawByteString;
  FControllerDevice:RawByteString;
  FSpecialDevice   :RawByteString;
 published
  property MainDevice      :RawByteString read FMainDevice       write FMainDevice      ;
  property HeadphoneDevice :RawByteString read FHeadphoneDevice  write FHeadphoneDevice ;
  property ControllerDevice:RawByteString read FControllerDevice write FControllerDevice;
  property SpecialDevice   :RawByteString read FSpecialDevice    write FSpecialDevice   ;
 public
 end;

 TPS4LoadExec=class(TSerializeObject)
 private
  FPath:RawByteString;
  Fargv:TSerializeStringArray;
 published
  property Path:RawByteString         read FPath write FPath;
  property argv:TSerializeStringArray read Fargv write Fargv;
 public
 end;

 TConfigInfo=class(TSerializeObject)
  private
   FMainInfo        :TMainInfo;
   FBootParamInfo   :TBootParamInfo;
   FJITInfo         :TJITInfo;
   FMiscInfo        :TMiscInfo;
   FVulkanInfo      :TVulkanInfo;
   FPS4SystemService:TPS4SystemService;
   FPS4Audio        :TPS4Audio;
  published
   property MainInfo        :TMainInfo         read FMainInfo         write FMainInfo;
   property BootParamInfo   :TBootParamInfo    read FBootParamInfo    write FBootParamInfo;
   property JITInfo         :TJITInfo          read FJITInfo          write FJITInfo;
   property MiscInfo        :TMiscInfo         read FMiscInfo         write FMiscInfo;
   property VulkanInfo      :TVulkanInfo       read FVulkanInfo       write FVulkanInfo;
   property PS4SystemService:TPS4SystemService read FPS4SystemService write FPS4SystemService;
   property PS4Audio        :TPS4Audio         read FPS4Audio         write FPS4Audio;
 end;

 TGameInfo=class(TSerializeObject)
 private
  FName   :RawByteString;
  FTitleId:RawByteString;
  FVersion:RawByteString;
  FAppVer :RawByteString;
  FExec   :RawByteString;
 published
  property Name   :RawByteString read FName    write FName;
  property TitleId:RawByteString read FTitleId write FTitleId;
  property Version:RawByteString read FVersion write FVersion;
  property AppVer :RawByteString read FAppVer  write FAppVer;
  property Exec   :RawByteString read FExec    write FExec;
 public
  Constructor Create; override;
 end;

 TMountList=class(TSerializeObject)
  private
   Fgame    :RawByteString;
   Ffirmware:RawByteString;
  published
   property game    :RawByteString read Fgame     write Fgame    ;
   property firmware:RawByteString read Ffirmware write Ffirmware;
  public
   Constructor Create; override;
 end;

 TGameItem=class(TSerializeObject)
  public
   FGameInfo :TGameInfo;
   FMountList:TMountList;
   FLock     :Boolean;
  published
   property GameInfo :TGameInfo  read FGameInfo  write FGameInfo;
   property MountList:TMountList read FMountList write FMountList;
 end;

 TGameStartupInfo=class(TSerializeObject)
  public
   FReader               :Boolean;
   FhasParamSfo          :Byte;
   FRequiredHdcpType     :Byte;
   FLoadExec             :Boolean;
   FPipe                 :THandle;
   FConfInfo             :TConfigInfo;
   FGameItem             :TGameItem;
   FLocalDir             :RawByteString;
   FCATEGORY             :RawByteString;
   FTITLE                :RawByteString;
   FTITLE_ID             :RawByteString;
   FCONTENT_ID           :RawByteString;
   FINSTALL_DIR_SAVEDATA :RawByteString;
   FAPP_VER              :RawByteString;
   FSYSTEM_VER           :DWORD;
   FATTRIBUTE            :DWORD;
   FATTRIBUTE2           :DWORD;
   FATTRIBUTE_EXE        :DWORD;
   FSELF_2MIB_PAGE_AMOUNT:DWORD;
   FDownloadMb_0         :DWORD;
   FDownloadMb_1         :DWORD;
  published
   property    Pipe                 :THandle       read FPipe                  write FPipe;
   property    LocalDir             :RawByteString read FLocalDir              write FLocalDir;
   property    CATEGORY             :RawByteString read FCATEGORY              write FCATEGORY;
   property    TITLE                :RawByteString read FTITLE                 write FTITLE;
   property    TITLE_ID             :RawByteString read FTITLE_ID              write FTITLE_ID;
   property    CONTENT_ID           :RawByteString read FCONTENT_ID            write FCONTENT_ID;
   property    INSTALL_DIR_SAVEDATA :RawByteString read FINSTALL_DIR_SAVEDATA  write FINSTALL_DIR_SAVEDATA;
   property    APP_VER              :RawByteString read FAPP_VER               write FAPP_VER;
   property    SYSTEM_VER           :DWORD         read FSYSTEM_VER            write FSYSTEM_VER;
   property    ATTRIBUTE            :DWORD         read FATTRIBUTE             write FATTRIBUTE;
   property    ATTRIBUTE2           :DWORD         read FATTRIBUTE2            write FATTRIBUTE2;
   property    ATTRIBUTE_EXE        :DWORD         read FATTRIBUTE_EXE         write FATTRIBUTE_EXE;
   property    SELF_2MIB_PAGE_AMOUNT:DWORD         read FSELF_2MIB_PAGE_AMOUNT write FSELF_2MIB_PAGE_AMOUNT;
   property    DownloadMb_0         :DWORD         read FDownloadMb_0          write FDownloadMb_0;
   property    DownloadMb_1         :DWORD         read FDownloadMb_1          write FDownloadMb_1;
   property    hasParamSfo          :Byte          read FhasParamSfo           write FhasParamSfo;
   property    RequiredHdcpType     :Byte          read FRequiredHdcpType      write FRequiredHdcpType;
   property    LoadExec             :Boolean       read FLoadExec              write FLoadExec;
  public
   Constructor Create(Reader:Boolean); reintroduce;
   Destructor  Destroy; override;
   Procedure   Serialize  (Stream:TStream); override;
   Procedure   Deserialize(Stream:TStream); override;
 end;

implementation

//

Constructor TJITInfo.Create;
begin
 inherited;
 Frelative_analize:=True;
 Fscan_switchtable:=True;
 Fscan_nopsequence:=True;
end;

Constructor TMainInfo.Create;
begin
 inherited;
 FLogFile        :='log.txt';
 FDefaultFirmware:=DirectorySeparator+'firmware';
end;

Constructor TMiscInfo.Create;
begin
 inherited;
 Ffork_proc:=True;
end;

Constructor TPS4SystemService.Create;
begin
 inherited;
 FSystemName  :='PS4-123';
 FLanguage    :=-1;
 FDateFormat  :=-1;
 FTimeFormat  :=-1;
 FButtonAssign:=1;
end;

//

Constructor TGameInfo.Create;
begin
 inherited;
 FExec:='/app0/eboot.bin';
 FTitleId:='???';
 FVersion:='???';
 FAppVer :='???';
end;

Constructor TMountList.Create;
begin
 inherited;
 Fgame    :=DirectorySeparator;
 Ffirmware:=DirectorySeparator+'firmware';
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
 inherited Serialize(Stream);
 FConfInfo.Serialize(Stream);
 FGameItem.Serialize(Stream);
end;

Procedure TGameStartupInfo.Deserialize(Stream:TStream);
begin
 inherited Deserialize(Stream);
 FConfInfo.Deserialize(Stream);
 FGameItem.Deserialize(Stream);
end;

end.


