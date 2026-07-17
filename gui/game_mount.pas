unit game_mount;

{$mode ObjFPC}{$H+}

interface

uses
 core_serialization,
 game_info,
 kern_mtx;

type
 TGameMountConfig=class
  ATTRIBUTE :DWORD;
  Game      :RawByteString;
  LocalDir  :RawByteString;
  TitleId   :array[0..9] of AnsiChar;
  InstallDir:array[0..9] of AnsiChar;
  //
  mount_mtx:mtx;
  //
  TemporaryMount:Boolean;
  //
  DownloadKb:array[0..1] of QWORD;
  //
  Constructor Create;
  function GetTemporaryTitleIdFile:RawByteString;
  function GetAppTemporaryFolder:RawByteString;
  function GetAppDownloadFolder(i:Byte):RawByteString;
  function GetSaveDataFolder(user_id:Integer;_titleId,dirName:pchar):RawByteString;
  function GetSaveDataBackupDst(user_id:Integer;_titleId,dirName:pchar):RawByteString;
  function GetSaveDataBackupOld(user_id:Integer;_titleId,dirName:pchar):RawByteString;
  function GetSaveDataBackupNew(user_id:Integer;_titleId,dirName:pchar):RawByteString;
 end;

 TGameMountConfigExport=class(TSerializeObject)
 public
  FATTRIBUTE :DWORD;
  FGame      :RawByteString;
  FLocalDir  :RawByteString;
  FTitleId   :RawByteString;
  FInstallDir:RawByteString;
 published
  property ATTRIBUTE :DWORD         read FATTRIBUTE  write FATTRIBUTE;
  property Game      :RawByteString read FGame       write FGame;
  property LocalDir  :RawByteString read FLocalDir   write FLocalDir;
  property TitleId   :RawByteString read FTitleId    write FTitleId;
  property InstallDir:RawByteString read FInstallDir write FInstallDir;
 public
 end;

var
 GameMountConfig:TGameMountConfig;

procedure InitMount(GameStartupInfo:TGameStartupInfo);

function  GameMountConfigExport:TGameMountConfigExport;
procedure GameMountConfigImport(e:TGameMountConfigExport);

//

function TemporaryDataMount  (mountPoint:pchar;format:Boolean):Integer;
function TemporaryDataUnmount(mountPoint:pchar):Integer;
function TemporaryDataFormat (mountPoint:pchar):Integer;
function TemporaryDataGetAvailableSpaceKb(mountPoint:pchar;availableSpaceKb:PQWORD):Integer;
function DownloadDataGetAvailableSpaceKb (mountPoint:pchar;availableSpaceKb:PQWORD):Integer;

function DeleteFile     (const FileName:RawByteString):boolean;
function DeleteDirectory(const DirectoryName: RawByteString; OnlyChildren: boolean): boolean;
function TruncFile      (const DestFilename:RawByteString;size:Ptrint):Boolean;
function WriteToFile    (const DestFilename:RawByteString;buf:Pointer;size:Ptrint):Ptrint;
function ReadFromFile   (const SrcFilename:RawByteString;buf:Pointer;size:Ptrint):Ptrint;
function CopyFile       (const SrcFilename, DestFilename: RawByteString): boolean;
function CopyDirectory  (const src,dst:RawByteString): boolean;

function GetDirectorySizeLikePFS(const DirectoryName:RawByteString):Int64;

function FormatMount(const fs_src:RawByteString):Integer;

function unix_to_host(const name:RawByteString):RawByteString;

implementation

uses
 md_file,
 sysutils,
 strings,
 errno,
 kern_proc,
 vfs_mountroot,
 subr_backtrace;

function unix_to_host(const name:RawByteString):RawByteString;
var
 i:Integer;
begin
 Result:=name;
 if (DirectorySeparator<>'/') then
 For i:=1 to Length(Result) do
 begin
  if (Result[i]='/') then
  begin
   Result[i]:=DirectorySeparator;
  end;
 end;
end;

Constructor TGameMountConfig.Create;
begin
 LocalDir  :='';
 TitleId   :='?????????';
 InstallDir:='?????????';
 //
 mtx_init(mount_mtx,'mount_mtx');
end;

function TGameMountConfig.GetTemporaryTitleIdFile:RawByteString;
const
 TEMP_FILE='/system_data/game/tempdata.dat';
begin
 mtx_lock(mount_mtx);

 Result:=ExcludeTrailingPathDelimiter(LocalDir)+unix_to_host(TEMP_FILE);

 mtx_unlock(mount_mtx);
end;

function TGameMountConfig.GetAppTemporaryFolder:RawByteString;
const
 APP_TEMP ='/app_tmp/';
begin
 mtx_lock(mount_mtx);

 Result:=ExcludeTrailingPathDelimiter(LocalDir)+unix_to_host(APP_TEMP);

 mtx_unlock(mount_mtx);
end;

function TGameMountConfig.GetAppDownloadFolder(i:Byte):RawByteString;
const
 APP_DOWNLOAD='%s/user/download/%s/download%d.dat'; // On PS4 these are files, on the emulator, folders
begin
 mtx_lock(mount_mtx);

 Result:=Format(unix_to_host(APP_DOWNLOAD),[
  ExcludeTrailingPathDelimiter(LocalDir),
  TitleId,
  i
 ]);

 mtx_unlock(mount_mtx);
end;

function TGameMountConfig.GetSaveDataFolder(user_id:Integer;_titleId,dirName:pchar):RawByteString;
const
 APP_SAVE='%s/user/home/%s/savedata/%s/%s';
begin
 mtx_lock(mount_mtx);

 Result:=Format(unix_to_host(APP_SAVE),[
  ExcludeTrailingPathDelimiter(LocalDir),
  HexStr(user_id,8),
  _titleId,
  dirName
 ]);

 mtx_unlock(mount_mtx);
end;

function TGameMountConfig.GetSaveDataBackupDst(user_id:Integer;_titleId,dirName:pchar):RawByteString;
const
 APP_SAVE='%s/user/home/%s/savedata/%s/sce_bu_%s';
begin
 mtx_lock(mount_mtx);

 Result:=Format(unix_to_host(APP_SAVE),[
  ExcludeTrailingPathDelimiter(LocalDir),
  HexStr(user_id,8),
  _titleId,
  dirName
 ]);

 mtx_unlock(mount_mtx);
end;

function TGameMountConfig.GetSaveDataBackupOld(user_id:Integer;_titleId,dirName:pchar):RawByteString;
const
 APP_SAVE='%s/user/home/%s/savedata/%s/sce_bu_tmp_%s';
begin
 mtx_lock(mount_mtx);

 Result:=Format(unix_to_host(APP_SAVE),[
  ExcludeTrailingPathDelimiter(LocalDir),
  HexStr(user_id,8),
  _titleId,
  dirName
 ]);

 mtx_unlock(mount_mtx);
end;

function TGameMountConfig.GetSaveDataBackupNew(user_id:Integer;_titleId,dirName:pchar):RawByteString;
const
 APP_SAVE='%s/user/home/%s/savedata/sce_backup/%s_%s';
begin
 mtx_lock(mount_mtx);

 Result:=Format(unix_to_host(APP_SAVE),[
  ExcludeTrailingPathDelimiter(LocalDir),
  HexStr(user_id,8),
  _titleId,
  dirName
 ]);

 mtx_unlock(mount_mtx);
end;

function get_errno_str(err:Integer):RawByteString;
begin
 case err of
  EPERM  :Result:='Operation not permitted';
  ENOENT :Result:='No such file or directory';
  EACCES :Result:='Permission denied';
  EBUSY  :Result:='Directory is busy';
  EEXIST :Result:='Directory exists';
  ENOTDIR:Result:='Not a directory';
  else
          Result:=IntToStr(err);
 end;
end;

function mount_mkdir(path:PChar):Integer;
begin
 Result:=vfs_mountroot.mount_mkdir(path);
 if (Result<>0) then
 begin
  print_error_td('[mkdir error]'+#13#10+
                 ' path:"'+path+'"'#13#10+
                 '  err:'+get_errno_str(Result)
                ,True);
 end;
end;

function mount_into_sandbox(fstype,fspath,from,opts:PChar;flags:QWORD;ignore:Boolean):Integer;
begin
 Result:=vfs_mountroot.mount_into_sandbox(fstype,fspath,from,opts,flags);
 if (Result<>0) and (not ignore) then
 begin
  print_error_td('[mount error]'+#13#10+
                 ' from:"'+from+'"'#13#10+
                 '   to:"'+fspath+'"'#13#10+
                 '  err:'+get_errno_str(Result)
                ,True);
 end;
end;

const
 MM_CREATE  =-1;
 MM_GAME    =0;
 MM_FIRMWARE=1;
 MM_LOCAL   =2;
 MM_LAST    =2;

type
 t_mnt_flags=Set of (mfReadOnly,mfIgnoreErr,mfForceDir,mfPFS,mfBudget);

type
 pp_mount_dir=^p_mount_dir;
 p_mount_dir=^t_mount_dir;
 t_mount_dir=object
  dst   :pchar;
  src   :pchar;
  mode  :Shortint;
  flags :t_mnt_flags;
  term  :Boolean;
  childs:p_mount_dir;
 end;

 t_mount_dir_iterator=object
  _curr:pp_mount_dir;
  stack:array[0..3] of p_mount_dir;
  function  curr:p_mount_dir;          inline;
  procedure init(dir:p_mount_dir);     inline;
  function  next(err:Integer):Boolean; inline;
 end;

function t_mount_dir_iterator.curr:p_mount_dir; inline;
begin
 Result:=_curr^;
end;

procedure t_mount_dir_iterator.init(dir:p_mount_dir); inline;
begin
 stack[0]:=dir;
 _curr:=@stack[0];
end;

function t_mount_dir_iterator.next(err:Integer):Boolean; inline;
var
 prev:p_mount_dir;
begin
 if (_curr=nil) then Exit(False);

 prev:=_curr^;

 if (err=0) and (prev^.childs<>nil) then
 begin
  _curr:=_curr+1; //down
  _curr^:=prev^.childs;
 end else
 begin
  _curr^:=prev+1; //next
 end;

 repeat

  if (_curr^^.term) then
  begin
   if (_curr=@stack[0]) then
   begin
    _curr:=nil;
    Exit(False);
   end else
   begin
    _curr :=_curr -1; //up
    _curr^:=_curr^+1; //next
   end;
  end else
  begin
   Exit(True);
  end;

 until false;

end;

const
 SYSTEM_COMMON_DIRS:array[0..12] of t_mount_dir=(
  (dst:'/%s/common/cert'          ;src:'%s/system/common/cert'          ;mode:MM_FIRMWARE;flags:[mfReadOnly]),             // CA_LIST.cer
  (dst:'/%s/common/etc'           ;src:'%s/system/common/etc'           ;mode:MM_FIRMWARE;flags:[mfReadOnly,mfIgnoreErr]),
  (dst:'/%s/common/font'          ;src:'%s/preinst/common/font'         ;mode:MM_FIRMWARE;flags:[mfReadOnly]),             // *.ttf
  (dst:'/%s/common/font2'         ;src:'%s/system/common/font2'         ;mode:MM_FIRMWARE;flags:[mfReadOnly]),
  (dst:'/%s/common/httpcache'     ;src:'%s/system_data/common/httpcache';mode:MM_LOCAL   ;flags:[mfForceDir]),
  (dst:'/%s/common/lib'           ;src:'%s/system/common/lib'           ;mode:MM_FIRMWARE;flags:[mfReadOnly]),
  (dst:'/%s/common/mms'           ;src:'%s/system_data/common/mms/'     ;mode:MM_LOCAL   ;flags:[mfForceDir]),             // av_content.db
  (dst:'/%s/common/mms_ro'        ;src:'%s/system/common/mms_ro'        ;mode:MM_FIRMWARE;flags:[mfReadOnly,mfIgnoreErr]), // template_content.db
  (dst:'/%s/common/playready'     ;src:'%s/user/common/playready'       ;mode:MM_LOCAL   ;flags:[mfReadOnly,mfForceDir ]),
  (dst:'/%s/common/text_layout'   ;src:'%s/system/common/text_layout'   ;mode:MM_FIRMWARE;flags:[mfReadOnly,mfIgnoreErr]),
  (dst:'/%s/common/text_to_speech';src:'%s/system/common/text_to_speech';mode:MM_FIRMWARE;flags:[mfReadOnly,mfIgnoreErr]),
  (dst:'/%s/common/webkit'        ;src:'%s/system/common/webkit'        ;mode:MM_FIRMWARE;flags:[mfReadOnly,mfIgnoreErr]),
  (term:True)
 );

 SYSTEM_DIRS:array[0..5] of t_mount_dir=(
  (dst:'/%s/becore'     ;src:''              ;mode:MM_CREATE  ;flags:[]),           // system app only
  (dst:'/%s/common'     ;src:''              ;mode:MM_CREATE  ;flags:[];childs:@SYSTEM_COMMON_DIRS),
  (dst:'/%s/common_temp';src:''              ;mode:MM_CREATE  ;flags:[]),
  (dst:'/%s/priv'       ;src:'%s/system/priv';mode:MM_FIRMWARE;flags:[mfReadOnly]), // system app only
  (dst:'/%s/sqlite'     ;src:''              ;mode:MM_CREATE  ;flags:[]),
  (term:True)
 );

 SANDBOX_DIRS:array[0..7] of t_mount_dir=(
  (dst:'/app0'       ;src:'%s'                 ;mode:MM_GAME  ;flags:[mfReadOnly,mfPFS,mfBudget]),
  (dst:'/av_contents';src:'%s/user/av_contents';mode:MM_LOCAL ;flags:[mfForceDir]),
  (dst:'/data'       ;src:'%s/user/data'       ;mode:MM_LOCAL ;flags:[mfForceDir]),
  (dst:'/host'       ;src:''                   ;mode:MM_CREATE;flags:[mfReadOnly]),
  (dst:'/hostapp'    ;src:''                   ;mode:MM_CREATE;flags:[mfReadOnly]),
  (dst:'/system_tmp' ;src:'%s/system_tmp'      ;mode:MM_LOCAL ;flags:[mfForceDir]),
  (dst:'/%s'         ;src:''                   ;mode:MM_CREATE;flags:[mfReadOnly];childs:@SYSTEM_DIRS),
  (term:True)
 );

 DOWNLOAD_DIRS:array[0..1] of pchar=(
  '/download0',
  '/download1'
 );

procedure InitMount(GameStartupInfo:TGameStartupInfo);
var
 i,err:Integer;

 fs_iterator:t_mount_dir_iterator;

 fs_source:array[0..MM_LAST] of RawByteString;
 fs_dst:RawByteString;
 fs_src:RawByteString;
begin

 //save to global
 GameMountConfig:=TGameMountConfig.Create;

 GameMountConfig.ATTRIBUTE:=GameStartupInfo.ATTRIBUTE;
 GameMountConfig.Game     :=GameStartupInfo.FGameItem.FMountList.game;
 GameMountConfig.LocalDir :=GameStartupInfo.LocalDir;

 if (GameStartupInfo.TITLE_ID<>'') then
 begin
  GameMountConfig.TitleId:=GameStartupInfo.TITLE_ID;
 end else
 if (GameStartupInfo.FGameItem.GameInfo.TitleId<>'') then
 begin
  GameMountConfig.TitleId:=GameStartupInfo.FGameItem.GameInfo.TitleId;
 end;

 GameMountConfig.InstallDir:=GameMountConfig.TitleId;
 if (GameStartupInfo.INSTALL_DIR_SAVEDATA<>'') then
 begin
  GameMountConfig.InstallDir:=GameStartupInfo.INSTALL_DIR_SAVEDATA;
 end;

 GameMountConfig.DownloadKb[0]:=GameStartupInfo.DownloadMb_0*1024;
 GameMountConfig.DownloadKb[1]:=GameStartupInfo.DownloadMb_1*1024;
 //save to global

 fs_source[MM_GAME    ]:=ExcludeTrailingPathDelimiter(GameStartupInfo.FGameItem.FMountList.game    );
 fs_source[MM_FIRMWARE]:=ExcludeTrailingPathDelimiter(GameStartupInfo.FGameItem.FMountList.firmware);
 fs_source[MM_LOCAL   ]:=ExcludeTrailingPathDelimiter(GameStartupInfo.LocalDir);

 //--sandbox--
 fs_iterator.init(@SANDBOX_DIRS);
 repeat

  with fs_iterator.curr^ do
  begin

   fs_dst:=Format(dst,[p_proc.p_randomized_path]);

   if (mode=MM_CREATE) then
   begin
    err:=mount_mkdir(pchar(fs_dst));
   end else
   begin
    fs_src:=Format(unix_to_host(src),[fs_source[mode]]);

    if (mfForceDir in flags) then
    begin
     ForceDirectories(fs_src);
    end;

    err:=mount_into_sandbox('ufs',
                            pchar(fs_dst),
                            pchar(fs_src),
                            nil,
                            ord(mfReadOnly in flags)*MNT_RDONLY  or
                            ord(mfPFS      in flags)*MNT_PFS_64K or
                            ord(mfBudget   in flags)*MNT_BIG_APP,
                            mfIgnoreErr in flags);
   end;

  end;

 until (not fs_iterator.next(err));
 //--sandbox--

 //download
 For i:=0 to High(GameMountConfig.DownloadKb) do
 if (GameMountConfig.DownloadKb[i]<>0) then
 begin
  fs_src:=GameMountConfig.GetAppDownloadFolder(i);
  ForceDirectories(fs_src);

  err:=mount_into_sandbox('ufs',
                          DOWNLOAD_DIRS[i],
                          pchar(fs_src),
                          nil,
                          MNT_BIG_APP,
                          False);
 end;
 //download

 //UPDATE: sandbox root IS NOT read-only
 //err:=vfs_mount_path('ufs','/','/',nil,MNT_RDONLY or MNT_UPDATE);

end;

function GameMountConfigExport:TGameMountConfigExport;
begin
 if (GameMountConfig=nil) then Exit(nil);

 Result:=TGameMountConfigExport.Create;

 Result.ATTRIBUTE :=GameMountConfig.ATTRIBUTE;
 Result.Game      :=GameMountConfig.Game;
 Result.LocalDir  :=GameMountConfig.LocalDir;
 Result.TitleId   :=GameMountConfig.TitleId;
 Result.InstallDir:=GameMountConfig.InstallDir;
end;

procedure GameMountConfigImport(e:TGameMountConfigExport);
begin
 if (GameMountConfig=nil) then
 begin
  GameMountConfig:=TGameMountConfig.Create;
 end;

 GameMountConfig.ATTRIBUTE :=e.ATTRIBUTE;
 GameMountConfig.Game      :=e.Game;
 GameMountConfig.LocalDir  :=e.LocalDir;
 GameMountConfig.TitleId   :=e.TitleId;
 GameMountConfig.InstallDir:=e.InstallDir;
end;

const
 DIRNAME_MAXSIZE=32;
 MOUNT_MAXSIZE  =16;

 TEMP0:pchar='/temp0';

Function ReadTemporaryTitleId:RawByteString;
var
 fs_src:RawByteString;
 F:THandle;
 s:Integer;
begin
 fs_src:=GameMountConfig.GetTemporaryTitleIdFile;

 F:=FileOpen(fs_src,fmOpenRead);
 if (F=THandle(-1)) then Exit('');

 s:=FileSeek(F,0,fsFromEnd);
 if (s>9) then s:=9;

 if (s>0) then
 begin
  SetLength(Result,s);
  FillChar(pchar(Result)^,s,#0);
  FileSeek(F,0,fsFromBeginning);
  FileRead(F,pchar(Result)^,s);
  s:=strlen(pchar(Result));
  SetLength(Result,s);
 end;

 FileClose(F);
end;

procedure SaveTemporaryTitleId(const TitleId:RawByteString);
var
 fs_src:RawByteString;
 fs_dir:RawByteString;
 F:THandle;
begin
 fs_src:=GameMountConfig.GetTemporaryTitleIdFile;
 fs_dir:=ExtractFilePath(fs_src);
 ForceDirectories(fs_dir);

 F:=FileCreate(fs_src);
 if (F=THandle(-1)) then Exit;

 FileWrite(F,pchar(TitleId)^,Length(TitleId)+1);

 FileFlush(F);
 FileClose(F);
end;

function DeleteFile(const FileName:RawByteString):boolean;
begin
 Result:=md_delete_file(FileName)=0;
end;

const
  //Don't follow symlinks on *nix, just delete them
  FindMask = faAnyFile {$ifdef unix} or faSymLink{%H-} {$endif unix};

  {$IFDEF WINDOWS}
  GetAllFilesMask='*.*';
  {$ELSE}
  GetAllFilesMask='*';
  {$ENDIF}

function DeleteDirectory(const DirectoryName: RawByteString; OnlyChildren: boolean): boolean;
type
 PNode=^TNode;
 TNode=record
  N:PNode;
  S:RawByteString;
  B:Boolean;
 end;

var
 stack:PNode;

 procedure Push(const S:RawByteString;B:Boolean);
 var
  new:PNode;
 begin
  new:=GetMem(SizeOf(TNode));
  Initialize(new^);
  new^.N:=stack;
  new^.S:=S;
  new^.B:=B;
  stack:=new;
 end;

 Function Pop(var S:RawByteString;var B:Boolean):Boolean;
 var
  old:PNode;
 begin
  if (stack<>nil) then
  begin
   old:=stack;
   stack:=old^.N;
   S:=old^.S;
   B:=old^.B;
   Finalize(old^);
   FreeMem(old);
   Result:=True;
  end else
  begin
   Result:=False;
  end;
 end;

var
  FileInfo: TSearchRec;
  CurSrcDir: RawByteString;
  CurFilename: RawByteString;
label
  _next;
begin
  Result:=false;
  CurSrcDir:=IncludeTrailingPathDelimiter(DirectoryName);
  stack:=nil;
_next:
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,FindMask,FileInfo)=0 then
  begin
   repeat
     // check if special file
     if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
     begin
       continue;
     end;
     CurFilename:=CurSrcDir+FileInfo.Name;
     if ((FileInfo.Attr and faDirectory)>0)
        {$ifdef unix} and ((FileInfo.Attr and faSymLink{%H-})=0) {$endif unix} then
     begin
       Push(CurSrcDir,OnlyChildren);
       CurSrcDir:=IncludeTrailingPathDelimiter(CurFilename);
       OnlyChildren:=False;
       SysUtils.FindClose(FileInfo);
       goto _next;
     end else
     begin
      if not DeleteFile(CurFilename) then
      begin
       while Pop(CurSrcDir,OnlyChildren) do;
       Exit;
      end;
     end;
   until SysUtils.FindNext(FileInfo)<>0;
   SysUtils.FindClose(FileInfo);
  end;

  if (not OnlyChildren) then
  begin
   if (not SysUtils.RemoveDir(CurSrcDir)) then
   begin
    while Pop(CurSrcDir,OnlyChildren) do;
    Exit;
   end;
  end;

  if Pop(CurSrcDir,OnlyChildren) then
  begin
   goto _next;
  end;

  Result:=true;
end;

function TruncFile(const DestFilename:RawByteString;size:Ptrint):Boolean;
var
 DestHandle:THandle;
 err:Integer;
begin
 DestHandle:=0;
 err:=md_open(DestFilename,O_CREAT or O_TRUNC or O_RDWR,&0604,DestHandle);
 if (err<>0) then Exit(False);

 Result:=FileTruncate(DestHandle,size);

 FileFlush(DestHandle);
 FileClose(DestHandle);
end;

function WriteToFile(const DestFilename:RawByteString;buf:Pointer;size:Ptrint):Ptrint;
var
 DestHandle:THandle;
 err:Integer;
begin
 Result:=0;
 DestHandle:=0;
 err:=md_open(DestFilename,O_CREAT or O_TRUNC or O_RDWR,&0600,DestHandle);
 if (err<>0) then Exit(-1);

 Result:=FileWrite(DestHandle,buf^,size);

 FileFlush(DestHandle);
 FileClose(DestHandle);
end;

function ReadFromFile(const SrcFilename:RawByteString;buf:Pointer;size:Ptrint):Ptrint;
var
 SrcHandle:THandle;
 err:Integer;
begin
 Result:=0;
 SrcHandle:=0;
 err:=md_open(SrcFilename,0,0,SrcHandle);
 if (err<>0) then Exit(-1);

 Result:=FileRead(SrcHandle,buf^,size);

 FileClose(SrcHandle);
end;

function CopyFile(const SrcFilename, DestFilename: RawByteString): boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: Integer;
begin
  Result := False;

  // check directory
  if (not DirectoryExists(ExtractFilePath(DestFileName))) then
  begin
   if (not ForceDirectories(ExtractFilePath(DestFileName))) then Exit;
  end;

  TryCount := 0;
  While (TryCount <> 3) Do
  Begin
   SrcHandle := FileOpen(SrcFilename, fmOpenRead or fmShareDenyWrite);
   if (SrcHandle = feInvalidHandle) then
   Begin
     Inc(TryCount);
     Sleep(10);
   end else
   Begin
     TryCount := 0;
     Break;
   end;
  End;

  If (TryCount > 0) then Exit;

  try
    DestHandle := FileCreate(DestFileName);
    if (DestHandle = feInvalidHandle) then Exit;
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if (ReadCount<=0) then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if (WriteCount<ReadCount) then Exit;
      until false;
    finally
      FileFlush(DestHandle);
      FileClose(DestHandle);
    end;

    //
    FileSetDate(DestFilename, FileGetDate(SrcHandle));
    FileSetAttr(DestFilename, FileGetAttr(SrcFilename));
    //

    Result := True;
  finally
     FileClose(SrcHandle);
  end;
end;

function CopyDirectory(const src,dst:RawByteString): boolean;
type
 PNode=^TNode;
 TNode=record
  N:PNode;
  fdst:RawByteString;
  fsrc:RawByteString;
 end;

var
 stack:PNode;

 procedure Push(const dst,src:RawByteString);
 var
  new:PNode;
 begin
  new:=GetMem(SizeOf(TNode));
  Initialize(new^);
  new^.N:=stack;
  new^.fdst:=dst;
  new^.fsrc:=src;
  stack:=new;
 end;

 Function Pop(var dst,src:RawByteString):Boolean;
 var
  old:PNode;
 begin
  if (stack<>nil) then
  begin
   old:=stack;
   stack:=old^.N;
   dst:=old^.fdst;
   src:=old^.fsrc;
   Finalize(old^);
   FreeMem(old);
   Result:=True;
  end else
  begin
   Result:=False;
  end;
 end;

var
  FileInfo: TSearchRec;
  CurDstDir: RawByteString;
  CurSrcDir: RawByteString;
  NewDstDir: RawByteString;
  NewSrcDir: RawByteString;
label
  _next,
  _fail;
begin
  Result:=false;
  CurDstDir:=IncludeTrailingPathDelimiter(dst);
  CurSrcDir:=IncludeTrailingPathDelimiter(src);

  Result:=ForceDirectories(CurDstDir);
  if not Result then Exit;

  stack:=nil;
 _next:
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,FindMask,FileInfo)=0 then
  begin
   repeat
     // check if special file
     if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
     begin
       continue;
     end;
     //
     if ((FileInfo.Attr and faDirectory)>0)
        {$ifdef unix} and ((FileInfo.Attr and faSymLink{%H-})=0) {$endif unix} then
     begin
       NewDstDir:=IncludeTrailingPathDelimiter(CurDstDir+FileInfo.Name);
       NewSrcDir:=IncludeTrailingPathDelimiter(CurSrcDir+FileInfo.Name);
       Push(NewDstDir,NewSrcDir);
       //
       Result:=CreateDir(NewDstDir);
       if (not Result) then goto _fail;
     end else
     begin
      NewDstDir:=(CurDstDir+FileInfo.Name);
      NewSrcDir:=(CurSrcDir+FileInfo.Name);

      Result:=CopyFile(NewSrcDir,NewDstDir);
      if (not Result) then
      begin
       _fail:
        while Pop(CurDstDir,CurSrcDir) do;
        Exit;
      end;
     end;
   until SysUtils.FindNext(FileInfo)<>0;
   SysUtils.FindClose(FileInfo);
  end;

  if Pop(CurDstDir,CurSrcDir) then
  begin
   goto _next;
  end;

 Result:=True;
end;

procedure GetFileSize(const FileName:RawByteString;var Size:Int64);
Var
 F:THandle;
 i:Int64;
begin
 F:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
 if (F=feInvalidHandle) then Exit;
 i:=FileSeek(F,0,fsFromEnd);
 if (i>=0) then Size:=i;
 FileClose(F);
end;

function GetDirectorySizeLikePFS(const DirectoryName:RawByteString):Int64;
type
 PNode=^TNode;
 TNode=record
  N:PNode;
  S:RawByteString;
 end;

var
 stack:PNode;

 procedure Push(const S:RawByteString);
 var
  new:PNode;
 begin
  new:=GetMem(SizeOf(TNode));
  Initialize(new^);
  new^.N:=stack;
  new^.S:=S;
  stack:=new;
 end;

 Function Pop(var S:RawByteString):Boolean;
 var
  old:PNode;
 begin
  if (stack<>nil) then
  begin
   old:=stack;
   stack:=old^.N;
   S:=old^.S;
   Finalize(old^);
   FreeMem(old);
   Result:=True;
  end else
  begin
   Result:=False;
  end;
 end;

 function AlignUp(addr:Int64;alignment:Int64):Int64; inline;
 var
  tmp:Int64;
 begin
  if (alignment=0) then Exit(addr);
  tmp:=addr+Int64(alignment-1);
  Result:=tmp-(tmp mod alignment);
 end;

var
  files_size :Int64;
  inode_count:Int64;
  dirent_size:Int64;
  //
  FileInfo: TSearchRec;
  CurSrcDir: RawByteString;
  CurFilename: RawByteString;
const
  c_block_size =4*1024; //????
  c_dirent_size=8;
  c_inode_size =168;
  c_inodes_per_block=c_block_size div c_inode_size;
label
  _next;
begin
  Result:=0;
  files_size :=0;
  inode_count:=0;
  dirent_size:=0;
  CurSrcDir:=IncludeTrailingPathDelimiter(DirectoryName);
  stack:=nil;
 _next:
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,FindMask,FileInfo)=0 then
  begin
   repeat
     // check if special file
     if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
     begin
       continue;
     end;
     //
     CurFilename:=CurSrcDir+FileInfo.Name;
     //
     GetFileSize(CurFilename,FileInfo.Size); //file size isn't updated immediately, so we'll force it
     //
     dirent_size:=dirent_size+AlignUp(c_dirent_size+Length(FileInfo.Name)+1,8);
     //
     if ((FileInfo.Attr and faDirectory)>0)
        {$ifdef unix} and ((FileInfo.Attr and faSymLink{%H-})=0) {$endif unix} then
     begin
       Push(IncludeTrailingPathDelimiter(CurFilename));
     end else
     begin
       if (FileInfo.Size<>0) then
       begin
        inode_count:=inode_count+1;
        files_size:=files_size+AlignUp(FileInfo.Size,c_block_size);
       end;
     end;
   until SysUtils.FindNext(FileInfo)<>0;
   SysUtils.FindClose(FileInfo);
  end;

  if Pop(CurSrcDir) then
  begin
   goto _next;
  end;

  //Very approximate size
  Result:=c_block_size+
          files_size+
          AlignUp(dirent_size,c_block_size)+
          AlignUp(inode_count,c_inodes_per_block)*c_block_size;
end;

function FormatMount(const fs_src:RawByteString):Integer;
begin
 //Delete all content in directory
 if DeleteDirectory(fs_src,True) then
 begin
  Result:=0;
 end else
 begin
  Result:=EPERM;
 end;
end;

function TemporaryDataMount(mountPoint:pchar;format:Boolean):Integer;
var
 fs_src:RawByteString;
 ValidTitleId:Boolean;
begin
 mtx_lock(GameMountConfig.mount_mtx);

 if GameMountConfig.TemporaryMount then
 begin
  Result:=EBUSY;
 end else
 begin
  fs_src:=GameMountConfig.GetAppTemporaryFolder;
  ForceDirectories(fs_src);

  Result:=vfs_mountroot.mount_into_sandbox('ufs',
                                           TEMP0,
                                           pchar(fs_src),
                                           nil,
                                           0);

  if (Result=0) then
  begin
   strlcopy(mountPoint,TEMP0,MOUNT_MAXSIZE);
   GameMountConfig.TemporaryMount:=True;

   ValidTitleId:=(ReadTemporaryTitleId=GameMountConfig.TitleId);

   if format or (not ValidTitleId) then
   begin
    FormatMount(fs_src);
   end;

   if (not ValidTitleId) then
   begin
    SaveTemporaryTitleId(GameMountConfig.TitleId);
   end;

  end;

 end;

 mtx_unlock(GameMountConfig.mount_mtx);
end;

function TemporaryDataUnmount(mountPoint:pchar):Integer;
begin
 if (strlcomp(mountPoint,TEMP0,MOUNT_MAXSIZE)<>0) then Exit(ENOTDIR);

 mtx_lock(GameMountConfig.mount_mtx);

 if GameMountConfig.TemporaryMount then
 begin
  Result:=vfs_mountroot.unmount_from_sandbox(TEMP0,0);

  if (Result=0) then
  begin
   GameMountConfig.TemporaryMount:=False;
  end;

 end else
 begin
  Result:=ENOTDIR;
 end;

 mtx_unlock(GameMountConfig.mount_mtx);
end;

function TemporaryDataFormat(mountPoint:pchar):Integer;
var
 fs_src:RawByteString;
begin
 if (strlcomp(mountPoint,TEMP0,MOUNT_MAXSIZE)<>0) then Exit(ENOTDIR);

 mtx_lock(GameMountConfig.mount_mtx);

 if GameMountConfig.TemporaryMount then
 begin
  fs_src:=GameMountConfig.GetAppTemporaryFolder;

  Result:=FormatMount(fs_src);
 end else
 begin
  Result:=ENOTDIR;
 end;

 mtx_unlock(GameMountConfig.mount_mtx);
end;

function TemporaryDataGetAvailableSpaceKb(mountPoint:pchar;availableSpaceKb:PQWORD):Integer;
const
 MAX_SIZE_KB=1*1024*1024;
var
 size:QWORD;
 fs_src:RawByteString;
begin
 if (strlcomp(mountPoint,TEMP0,MOUNT_MAXSIZE)<>0) then Exit(ENOTDIR);

 mtx_lock(GameMountConfig.mount_mtx);

 if GameMountConfig.TemporaryMount then
 begin
  fs_src:=GameMountConfig.GetAppTemporaryFolder;

  size:=GetDirectorySizeLikePFS(fs_src);
  size:=size div 1024; //to KB

  if (size>MAX_SIZE_KB) then
  begin
   size:=0;
  end else
  begin
   size:=MAX_SIZE_KB-size;
  end;

  availableSpaceKb^:=size;

  Result:=0;
 end else
 begin
  Result:=ENOTDIR;
 end;

 mtx_unlock(GameMountConfig.mount_mtx);
end;

function DownloadDataGetAvailableSpaceKb(mountPoint:pchar;availableSpaceKb:PQWORD):Integer;
var
 i:Byte;
 size:QWORD;
 fs_src:RawByteString;
begin
 if (strlcomp(mountPoint,DOWNLOAD_DIRS[0],MOUNT_MAXSIZE)=0) then
 begin
  i:=0;
 end else
 if (strlcomp(mountPoint,DOWNLOAD_DIRS[1],MOUNT_MAXSIZE)=0) then
 begin
  i:=1;
 end else
 begin
  Exit(ENOTDIR);
 end;

 if (GameMountConfig.DownloadKb[i]=0) then
 begin
  Exit(ENOTDIR);
 end;

 mtx_lock(GameMountConfig.mount_mtx);

  fs_src:=GameMountConfig.GetAppDownloadFolder(i);

  size:=GetDirectorySizeLikePFS(fs_src);
  size:=size div 1024; //to KB

  if (size>GameMountConfig.DownloadKb[i]) then
  begin
   size:=0;
  end else
  begin
   size:=GameMountConfig.DownloadKb[i]-size;
  end;

  availableSpaceKb^:=size;

 mtx_unlock(GameMountConfig.mount_mtx);

 Result:=0;
end;

end.

