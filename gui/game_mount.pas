unit game_mount;

{$mode ObjFPC}{$H+}

interface

uses
 game_info;

var
 g_LocalDir:RawByteString='';
 g_TitleId :String[10]    ='?????????'#0;

procedure InitMount(GameStartupInfo:TGameStartupInfo);

//

function TemporaryDataMount  (mountPoint:pchar;format:Boolean):Integer;
function TemporaryDataUnmount(mountPoint:pchar):Integer;
function TemporaryDataFormat (mountPoint:pchar):Integer;

implementation

uses
 sysutils,
 fileutil,
 strings,
 errno,
 kern_mtx,
 vfs_mountroot,
 subr_backtrace;

var
 mount_mtx:mtx;

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

const
 MM_CREATE  =-1;
 MM_GAME    =0;
 MM_FIRMWARE=1;
 MM_LOCAL   =2;
 MM_LAST    =2;

type
 t_mnt_flags=Set of (mfReadOnly,mfIgnoreErr,mfForceDir);

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
  (dst:'/app0'       ;src:'%s'                 ;mode:MM_GAME  ;flags:[mfReadOnly]),
  (dst:'/av_contents';src:'%s/user/av_contents';mode:MM_LOCAL ;flags:[mfForceDir]),
  (dst:'/data'       ;src:'%s/user/data'       ;mode:MM_LOCAL ;flags:[mfForceDir]),
  (dst:'/host'       ;src:''                   ;mode:MM_CREATE;flags:[mfReadOnly]),
  (dst:'/hostapp'    ;src:''                   ;mode:MM_CREATE;flags:[mfReadOnly]),
  (dst:'/system_tmp' ;src:'%s/system_tmp'      ;mode:MM_LOCAL ;flags:[mfForceDir]),
  (dst:'/%s'         ;src:''                   ;mode:MM_CREATE;flags:[mfReadOnly];childs:@SYSTEM_DIRS),
  (term:True)
 );

procedure InitMount(GameStartupInfo:TGameStartupInfo);
var
 err:Integer;

 fs_iterator:t_mount_dir_iterator;

 fs_source:array[0..MM_LAST] of RawByteString;
 fs_dst:RawByteString;
 fs_src:RawByteString;
begin

 //temp hack
 err:=mount_into_sandbox('ufs','/savedata0','savedata',nil,0,True);

 fs_source[MM_GAME    ]:=ExcludeTrailingPathDelimiter(GameStartupInfo.FGameItem.FMountList.game    );
 fs_source[MM_FIRMWARE]:=ExcludeTrailingPathDelimiter(GameStartupInfo.FGameItem.FMountList.firmware);
 fs_source[MM_LOCAL   ]:=ExcludeTrailingPathDelimiter(GameStartupInfo.LocalDir);

 //save to global
 g_LocalDir:=GameStartupInfo.LocalDir;

 with GameStartupInfo.FGameItem.GameInfo do
 begin
  if (TitleId<>'') then
  begin
   g_TitleId:=TitleId;
  end;
 end;
 //save to global

 //--sandbox--
 fs_iterator.init(@SANDBOX_DIRS);
 repeat

  with fs_iterator.curr^ do
  begin

   fs_dst:=Format(dst,['system']);

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

    err:=mount_into_sandbox('ufs',pchar(fs_dst),pchar(fs_src),nil,ord(mfReadOnly in flags)*MNT_RDONLY,mfIgnoreErr in flags);
   end;

  end;

 until (not fs_iterator.next(err));
 //--sandbox--

 //UPDATE: sandbox root IS NOT read-only
 //err:=vfs_mount_path('ufs','/','/',nil,MNT_RDONLY or MNT_UPDATE);

 mtx_init(mount_mtx,'mount_mtx');
end;

const
 DIRNAME_MAXSIZE=32;
 MOUNT_MAXSIZE  =16;

 TEMP0:pchar='/temp0';

 TEMP_FILE='/system_data/game/tempdata.dat';
 APP_TEMP ='/app_tmp/';

var
 TemporaryMount:Boolean=False;

Function ReadTemporaryTitleId:RawByteString;
var
 fs_src:RawByteString;
 F:THandle;
 s:Integer;
begin
 fs_src:=ExcludeTrailingPathDelimiter(g_LocalDir)+unix_to_host(TEMP_FILE);

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
 fs_src:=ExcludeTrailingPathDelimiter(g_LocalDir)+unix_to_host(TEMP_FILE);
 fs_dir:=ExtractFilePath(fs_src);
 ForceDirectories(fs_dir);

 F:=FileCreate(fs_src);
 if (F=THandle(-1)) then Exit;

 FileWrite(F,pchar(TitleId)^,Length(TitleId)+1);

 FileClose(F);
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
 mtx_lock(mount_mtx);

 if TemporaryMount then
 begin
  Result:=EBUSY;
 end else
 begin
  fs_src:=ExcludeTrailingPathDelimiter(g_LocalDir)+unix_to_host(APP_TEMP);
  ForceDirectories(fs_src);

  Result:=vfs_mountroot.mount_into_sandbox('ufs',TEMP0,pchar(fs_src),nil,0);

  if (Result=0) then
  begin
   strlcopy(mountPoint,TEMP0,MOUNT_MAXSIZE);
   TemporaryMount:=True;

   ValidTitleId:=(ReadTemporaryTitleId=g_TitleId);

   if format or (not ValidTitleId) then
   begin
    FormatMount(fs_src);
   end;

   if (not ValidTitleId) then
   begin
    SaveTemporaryTitleId(g_TitleId);
   end;

  end;

 end;

 mtx_unlock(mount_mtx);
end;

function TemporaryDataUnmount(mountPoint:pchar):Integer;
begin
 if (strlcomp(mountPoint,TEMP0,MOUNT_MAXSIZE)<>0) then Exit(ENOTDIR);

 mtx_lock(mount_mtx);

 if TemporaryMount then
 begin
  Result:=vfs_mountroot.unmount_from_sandbox(TEMP0,0);

  if (Result=0) then
  begin
   TemporaryMount:=False;
  end;

 end else
 begin
  Result:=ENOTDIR;
 end;

 mtx_unlock(mount_mtx);
end;

function TemporaryDataFormat(mountPoint:pchar):Integer;
var
 fs_src:RawByteString;
begin
 if (strlcomp(mountPoint,TEMP0,MOUNT_MAXSIZE)<>0) then Exit(ENOTDIR);

 mtx_lock(mount_mtx);

 if TemporaryMount then
 begin
  fs_src:=ExcludeTrailingPathDelimiter(g_LocalDir)+unix_to_host(APP_TEMP);

  Result:=FormatMount(fs_src);
 end else
 begin
  Result:=ENOTDIR;
 end;

 mtx_unlock(mount_mtx);
end;

end.

