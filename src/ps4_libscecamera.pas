unit ps4_libSceCamera;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  Classes,
  SysUtils;

const
  SCE_CAMERA_CONFIG_TYPE1 = $01;
  SCE_CAMERA_CONFIG_TYPE2 = $02;
  SCE_CAMERA_CONFIG_TYPE3 = $03;
  SCE_CAMERA_CONFIG_TYPE4 = $04;
  SCE_CAMERA_CONFIG_TYPE5 = $05;
  SCE_CAMERA_CONFIG_EXTENTION = $10;

  SCE_CAMERA_FORMAT_YUV422 = $0;
  SCE_CAMERA_FORMAT_NO_USE = $10;
  SCE_CAMERA_FORMAT_UNKNOWN= $FF;

  SCE_CAMERA_SCALE_FORMAT_YUV422 = $0;
  SCE_CAMERA_SCALE_FORMAT_Y16    = $3;
  SCE_CAMERA_SCALE_FORMAT_NO_USE = $10;
  SCE_CAMERA_SCALE_FORMAT_UNKNOWN= $FF;

  SCE_CAMERA_MAX_DEVICE_NUM=2;
  SCE_CAMERA_MAX_FORMAT_LEVEL_NUM=4;

  SCE_CAMERA_RESOLUTION_1280X800= $0;
  SCE_CAMERA_RESOLUTION_UNKNOWN = $FF;

  SCE_CAMERA_FRAMERATE_UNKNOWN=0;
  SCE_CAMERA_FRAMERATE_7_5    =7;
  SCE_CAMERA_FRAMERATE_15     =15;
  SCE_CAMERA_FRAMERATE_30     =30;
  SCE_CAMERA_FRAMERATE_60     =60;
  SCE_CAMERA_FRAMERATE_120    =120;
  SCE_CAMERA_FRAMERATE_240    =240;

type
 pSceCameraBaseFormat=^SceCameraBaseFormat;
 SceCameraBaseFormat=packed record
 end;

 pSceCameraScaleFormat=^SceCameraScaleFormat;
 SceCameraScaleFormat=packed record
 end;

 pSceCameraFormat=^SceCameraFormat;
 SceCameraFormat=packed record
  formatLevel0:SceCameraBaseFormat;
  formatLevel1:SceCameraScaleFormat;
  formatlevel2:SceCameraScaleFormat;
  formatlevel3:SceCameraScaleFormat;
 end;

 pSceCameraResolution=^SceCameraResolution;
 SceCameraResolution=packed record
 end;

 pSceCameraFramerate=^SceCameraFramerate;
 SceCameraFramerate=packed record
 end;

 pSceCameraConfigType=^SceCameraConfigType;
 SceCameraConfigType=packed record
 end;

 pSceCameraConfigExtention=^SceCameraConfigExtention;
 SceCameraConfigExtention=packed record
  format:SceCameraFormat;
  resolution:SceCameraResolution;
  framerate:SceCameraFramerate;
  width:DWORD;
  height:DWORD;
  reserved1:DWORD;
  pBaseOption:Pointer;
 end;

 pSceCameraConfig=^SceCameraConfigType;
 SceCameraConfig=packed record
  sizeThis:DWORD;
  configType:SceCameraConfigType;
  configExtention:SceCameraConfigExtention;
 end;

 pSceCameraVideoSyncParameter=^SceCameraVideoSyncParameter;
 SceCameraVideoSyncParameter=packed record
  sizeThis:DWORD;
  videoSyncMode:DWORD;
  pModeOption:Pointer;
 end;

 pSceCameraStartParameter=^SceCameraStartParameter;
 SceCameraStartParameter=packed record
  sizeThis:DWORD;
  formatlevel:DWORD;
  pStartOption:Pointer;
 end;

 pSceCameraFramePosition=^SceCameraFramePosition;
 SceCameraFramePosition=packed record
  x:DWORD;
  y:DWORD;
  xSize:DWORD;
  ySize:DWORD;
 end;

 pSceCameraExposureGain=^SceCameraExposureGain;
 SceCameraExposureGain=packed record
  exposureControl:DWORD;
  exposure:DWORD;
  gain:DWORD;
  mode:DWORD;
 end;

 pSceCameraWhiteBalance=^SceCameraWhiteBalance;
 SceCameraWhiteBalance=packed record
  whiteBalanceControl:DWORD;
  gainRed:DWORD;
  gainBlue:DWORD;
  gainGreen:DWORD;
 end;

 pSceCameraGamma=^SceCameraGamma;
 SceCameraGamma=packed record
  gammaControl:DWORD;
  value:DWORD;
  reserved:array[0..16] of Word;
 end;

 pSceFVector3=^SceFVector3;
 SceFVector3=packed record
 end;

 pSceCameraMeta=^SceCameraMeta;
 SceCameraMeta=packed record
  metaMode:DWORD;
  format:DWORD;
  frame:QWORD;
  timestamp:QWORD;
  deviceTimestamp:DWORD;
  exposureGain:SceCameraExposureGain;
  whiteBalance:SceCameraWhiteBalance;
  gamma:SceCameraGamma;
  luminance:DWORD;
  acceleration:SceFVector3;
  vcounter:QWORD;
  reserved:array[0..16] of DWORD;
 end;

 pSceCameraFrameData=^SceCameraFrameData;
 SceCameraFrameData=packed record
  sizeThis:DWORD;
  readMode:DWORD;
  framePosition:SceCameraFramePosition;
  pFramePointerList:Pointer;
  frameSize:DWORD;
  meta:SceCameraMeta;
  pFramePointerListGarlic:Pointer;
 end;  

implementation

function ps4_sceCameraIsAttached(index:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCameraSetConfig(handle:Integer;pConfig:pSceCameraConfig):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCameraSetVideoSync(handle:Integer;pVideoSync:SceCameraVideoSyncParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCameraStart(handle:Integer;pParam:pSceCameraStartParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCameraGetFrameData(handle:Integer;pFrameData:SceCameraFrameData):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end; 

function Load_libSceCamera(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceCamera');
 lib^.set_proc($A7A9F73698B7618E,@ps4_sceCameraIsAttached);
 lib^.set_proc($550FB9900AAC1364,@ps4_sceCameraSetConfig);
 lib^.set_proc($C297B217027E5042,@ps4_sceCameraSetVideoSync);
 lib^.set_proc($F44A5160CCBBAC75,@ps4_sceCameraStart);
 lib^.set_proc($9B180C991FB52ABD,@ps4_sceCameraGetFrameData);  
end;

initialization
 ps4_app.RegistredPreLoad('libSceCamera.prx',@Load_libSceCamera);

end.

