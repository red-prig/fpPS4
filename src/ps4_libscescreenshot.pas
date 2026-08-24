unit ps4_libSceScreenShot;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  subr_dynlib;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

const
 SCE_SCREENSHOT_MAX_FS_PATH            =1024;
 SCE_SCREENSHOT_MAX_PHOTO_TITLE_LENGTH =64;
 SCE_SCREENSHOT_MAX_PHOTO_TITLE_SIZE   =SCE_SCREENSHOT_MAX_PHOTO_TITLE_LENGTH*4;
 SCE_SCREENSHOT_MAX_GAME_TITLE_LENGTH  =64;
 SCE_SCREENSHOT_MAX_GAME_TITLE_SIZE    =SCE_SCREENSHOT_MAX_GAME_TITLE_LENGTH*4;
 SCE_SCREENSHOT_MAX_GAME_COMMENT_LENGTH=128;
 SCE_SCREENSHOT_MAX_GAME_COMMENT_SIZE  =SCE_SCREENSHOT_MAX_GAME_COMMENT_LENGTH*4;

 //SceScreenShotOrigin
 kSceScreenShotOriginLeftTop      = 1;
 kSceScreenShotOriginLeftCenter   = 2;
 kSceScreenShotOriginLeftBottom   = 3;
 kSceScreenShotOriginCenterTop    = 4;
 kSceScreenShotOriginCenterCenter = 5;
 kSceScreenShotOriginCenterBottom = 6;
 kSceScreenShotOriginRightTop     = 7;
 kSceScreenShotOriginRightCenter  = 8;
 kSceScreenShotOriginRightBottom  = 9;

type
 pSceScreenShotParam=^SceScreenShotParam;
 SceScreenShotParam=packed record
  thisSize   :DWORD;
  _align     :DWORD;
  photoTitle :PChar;
  gameTitle  :PChar;
  gameComment:PChar;
  reserved   :Pointer;
 end;

function ps4_sceScreenShotSetOverlayImage(
          filePath:PChar;
          offsetx:Integer;
          offsety:Integer
         ):Integer;
begin
 LOG_INFO('sceScreenShotSetOverlayImage:',filePath);
 Result:=0;
end;

function ps4_sceScreenShotSetOverlayImageWithOrigin(
          filePath:PChar;
          marginX:Integer;
          marginY:Integer;
          origin:Integer //SceScreenShotOrigin
         ):Integer;
begin
 LOG_INFO('sceScreenShotSetOverlayImageWithOrigin:',filePath);
 Result:=0;
end;

function ps4_sceScreenShotEnable():Integer;
begin
 LOG_INFO('sceScreenShotEnable');
 Result:=0;
end;

function ps4_sceScreenShotDisable():Integer;
begin
 LOG_INFO('sceScreenShotDisable');
 Result:=0;
end;

function ps4_sceScreenShotSetParam(param:pSceScreenShotParam):Integer;
begin
 LOG_INFO('sceScreenShotSetParam');
 Result:=0;
end;

{$WARN 4110 off}
function Load_libSceScreenShot(name:pchar):p_lib_info;
var
  lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceScreenShot');

 lib:=Result^.add_lib('libSceScreenShot');
 lib.set_proc($6A11E139FF903644,@ps4_sceScreenShotSetOverlayImage);
 lib.set_proc($EF7590E098F49C92,@ps4_sceScreenShotSetOverlayImageWithOrigin);
 lib.set_proc($DB1C54B6E0BF4731,@ps4_sceScreenShotEnable);
 lib.set_proc($B4861FD16E554E2F,@ps4_sceScreenShotDisable);
 lib.set_proc($1BB2A59886052197,@ps4_sceScreenShotSetParam);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceScreenShot.prx',@Load_libSceScreenShot);

end.


