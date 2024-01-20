unit ps4_libSceVrTracker;

{$mode ObjFPC}{$H+}

interface


uses
  ps4_program,
  ps4_libSceCamera,
  Classes,
  SysUtils;

const
  SCE_CAMERA_MAX_DEVICE_NUM      =2;
  SCE_CAMERA_MAX_FORMAT_LEVEL_NUM=4;

  SCE_VR_TRACKER_MAX_LED_NUM=2;

  //SceVrTrackerDeviceType
  SCE_VR_TRACKER_DEVICE_HMD       =0;
  SCE_VR_TRACKER_DEVICE_DUALSHOCK4=1;
  SCE_VR_TRACKER_DEVICE_MOVE      =2;
  SCE_VR_TRACKER_DEVICE_GUN       =3;

  //SceVrTrackerUpdateMotionSensorDataOperationMode
  SCE_VR_TRACKER_UPDATE_MOTION_SENSORDATA_OPERATION_MODE_DEVICE_TYPE=0;
  SCE_VR_TRACKER_UPDATE_MOTION_SENSORDATA_OPERATION_MODE_HANDLE     =1;

  //SceVrTrackerResultType
  SCE_VR_TRACKER_RESULT_PREDICTED=0;

  //SceVrTrackerOrientationType
  SCE_VR_TRACKER_ORIENTATION_ABSOLUTE=0;
  SCE_VR_TRACKER_ORIENTATION_RELATIVE=1;

  //SceVrTrackerUsageType
  SCE_VR_TRACKER_USAGE_DEFAULT               =0;
  SCE_VR_TRACKER_USAGE_OPTIMIZED_FOR_HMD_USER=1;

  //SceVrTrackerDebugMarkerType
  SCE_VR_TRACKER_DEBUG_MARKER_UNSPECIFIED   =0;
  SCE_VR_TRACKER_DEBUG_MARKER_DEFAULT_UPDATE=1;
  SCE_VR_TRACKER_DEBUG_MARKER_FINAL_UPDATE  =2;
  SCE_VR_TRACKER_DEBUG_MARKER_OTHER         =3;

  //SceVrTrackerRecalibrateNecessityType
  SCE_VR_TRACKER_RECALIBRATE_NECESSITY_NOTHING =0;
  SCE_VR_TRACKER_RECALIBRATE_NECESSITY_POSITION=4;

  //SceVrTrackerPlayareaBrightnessRiskType
  SCE_VR_TRACKER_PLAYAREA_BRIGHTNESS_RISK_LOW =0;
  SCE_VR_TRACKER_PLAYAREA_BRIGHTNESS_RISK_HIGH=5;
  SCE_VR_TRACKER_PLAYAREA_BRIGHTNESS_RISK_MAX =10;

  //SceVrTrackerLedColor
  SCE_VR_TRACKER_LED_COLOR_BLUE   =0;
  SCE_VR_TRACKER_LED_COLOR_RED    =1;
  SCE_VR_TRACKER_LED_COLOR_CYAN   =2;
  SCE_VR_TRACKER_LED_COLOR_MAGENTA=3;
  SCE_VR_TRACKER_LED_COLOR_YELLOW =4;
  SCE_VR_TRACKER_LED_COLOR_GREEN  =2;

  //SceVrTrackerStatus
  SCE_VR_TRACKER_STATUS_NOT_STARTED =0;
  SCE_VR_TRACKER_STATUS_TRACKING    =1;
  SCE_VR_TRACKER_STATUS_NOT_TRACKING=2;
  SCE_VR_TRACKER_STATUS_CALIBRATING =3;

  //SceVrTrackerQuality
  SCE_VR_TRACKER_QUALITY_NONE       =0;
  SCE_VR_TRACKER_QUALITY_NOT_VISIBLE=3;
  SCE_VR_TRACKER_QUALITY_PARTIAL    =6;
  SCE_VR_TRACKER_QUALITY_FULL       =9;

  //SceVrTrackerLedAdjustmentStatus
  SCE_VR_TRACKER_LED_ADJUSTMENT_NOT_USED=0;
  SCE_VR_TRACKER_LED_ADJUSTMENT_USED    =1;

  //SceVrTrackerHmdRearTrackingStatus
  SCE_VR_TRACKER_REAR_TRACKING_NOT_READY=0;
  SCE_VR_TRACKER_REAR_TRACKING_READY    =1;

  //SceVrTrackerPreferenceType
  SCE_VR_TRACKER_PREFERENCE_FAR_POSITION   =0;
  SCE_VR_TRACKER_PREFERENCE_STABLE_POSITION=1;

  //SceVrTrackerCameraMetaCheckMode
  SCE_VR_TRACKER_CAMERA_META_CHECK_ENABLE =0;
  SCE_VR_TRACKER_CAMERA_META_CHECK_DISABLE=1;

  //SceVrTrackerDevicePermitType
  SCE_VR_TRACKER_DEVICE_PERMIT_ALL     =0;
  SCE_VR_TRACKER_DEVICE_PERMIT_HMD_ONLY=1;

  //SceVrTrackerRobustnessLevel
  SCE_VR_TRACKER_ROBUSTNESS_LEVEL_HIGH  =0;
  SCE_VR_TRACKER_ROBUSTNESS_LEVEL_LOW   =3;
  SCE_VR_TRACKER_ROBUSTNESS_LEVEL_MEDIUM=6;
  SCE_VR_TRACKER_ROBUSTNESS_LEVEL_LEGACY=99;

type
  pSceFVector3=^SceFVector3;
  SceFVector3=packed record
   x,y,z:Single;
  end;

  pSceFQuaternion=^SceFQuaternion;
  SceFQuaternion=packed record
   x,y,z,w:Single;
  end;

  pSceVrTrackerDeviceType=^SceVrTrackerDeviceType;
  SceVrTrackerDeviceType=Integer;

  pSceVrTrackerPreferenceType=^SceVrTrackerPreferenceType;
  SceVrTrackerPreferenceType=Integer;

  pSceVrTrackerCameraMetaCheckMode=^SceVrTrackerCameraMetaCheckMode;
  SceVrTrackerCameraMetaCheckMode=Integer;

  pSceVrTrackerDevicePermitType=^SceVrTrackerDevicePermitType;
  SceVrTrackerDevicePermitType=Integer;

  pSceVrTrackerRobustnessLevel=^SceVrTrackerRobustnessLevel;
  SceVrTrackerRobustnessLevel=Integer;

  pSceCameraFramePosition=^SceCameraFramePosition;
  SceCameraFramePosition=packed record
   x,y,xSize,ySize:DWORD;
  end;

  pSceCameraExposureGain=^SceCameraExposureGain;
  SceCameraExposureGain=packed record
   exposureControl,exposure,gain,mode:DWORD;
  end;

  pSceCameraWhiteBalance=^SceCameraWhiteBalance;
  SceCameraWhiteBalance=packed record
   whiteBalanceControl,gainRed,gainBlue,gainGreen:DWORD;
  end;

  pSceCameraGamma=^SceCameraGamma;
  SceCameraGamma=packed record
   gammaControl:DWORD;
   value:DWORD;
   reserved:array[0..15] of Byte;
  end;

  pSceCameraMeta=^SceCameraMeta;
  SceCameraMeta=packed record
   metaMode       :DWORD;
   format         :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_CAMERA_MAX_FORMAT_LEVEL_NUM-1] of DWORD;
   _align         :DWORD;
   frame          :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of QWORD;
   timestamp      :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of QWORD;
   deviceTimestamp:array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of DWORD;
   exposureGain   :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of SceCameraExposureGain;
   whiteBalance   :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of SceCameraWhiteBalance;
   gamma          :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of SceCameraGamma;
   luminance      :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of DWORD;
   acceleration   :SceFVector3;
   vcounter       :QWORD;
   reserved       :array[0..15] of DWORD;
  end;

  pSceCameraFrameData=^SceCameraFrameData;
  SceCameraFrameData=packed record
   sizeThis               :DWORD;
   readMode               :DWORD;
   framePosition          :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_CAMERA_MAX_FORMAT_LEVEL_NUM-1] of SceCameraFramePosition;
   pFramePointerList      :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_CAMERA_MAX_FORMAT_LEVEL_NUM-1] of Pointer;
   frameSize              :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_CAMERA_MAX_FORMAT_LEVEL_NUM-1] of DWORD;
   status                 :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_CAMERA_MAX_FORMAT_LEVEL_NUM-1] of DWORD;
   meta                   :SceCameraMeta;
   pFramePointerListGarlic:array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_CAMERA_MAX_FORMAT_LEVEL_NUM-1] of Pointer;
  end;

  pSceVrTrackerGpuSubmitParam=^SceVrTrackerGpuSubmitParam;
  SceVrTrackerGpuSubmitParam=packed record
   sizeOfThis              :Integer;
   padTrackingPreference   :SceVrTrackerPreferenceType;
   cameraMetaCheckMode     :SceVrTrackerCameraMetaCheckMode;
   trackingDevicePermitType:SceVrTrackerDevicePermitType;
   robustnessLevel         :SceVrTrackerRobustnessLevel;
   reserved00              :array[0..9] of DWORD;
   reserved01              :DWORD;
   cameraFrameData         :SceCameraFrameData;
  end;

  pSceVrTrackerResultType=^SceVrTrackerResultType;
  SceVrTrackerResultType=Integer;

  pSceVrTrackerOrientationType=^SceVrTrackerOrientationType;
  SceVrTrackerOrientationType=Integer;

  pSceVrTrackerUsageType=^SceVrTrackerUsageType;
  SceVrTrackerUsageType=Integer;

  pSceVrTrackerDebugMarkerType=^SceVrTrackerDebugMarkerType;
  SceVrTrackerDebugMarkerType=Integer;

  pSceVrTrackerGetResultParam=^SceVrTrackerGetResultParam;
  SceVrTrackerGetResultParam=packed record
   sizeOfThis     :Integer;
   handle         :Integer;
   resultType     :SceVrTrackerResultType;
   reserved00     :DWORD;
   predictionTime :QWORD;
   orientationType:SceVrTrackerOrientationType;
   reserved01     :DWORD;
   usageType      :SceVrTrackerUsageType;
   userFrameNumber:DWORD;
   debugMarkerType:SceVrTrackerDebugMarkerType;
   reserved02     :array[0..1] of DWORD;
  end;

  pSceVrTrackerRecalibrateNecessityType=^SceVrTrackerRecalibrateNecessityType;
  SceVrTrackerRecalibrateNecessityType=Integer;

  pSceVrTrackerPlayareaBrightnessRiskType=^SceVrTrackerPlayareaBrightnessRiskType;
  SceVrTrackerPlayareaBrightnessRiskType=Integer;

  pSceVrTrackerLedColor=^SceVrTrackerLedColor;
  SceVrTrackerLedColor=Integer;

  pSceVrTrackerStatus=^SceVrTrackerStatus;
  SceVrTrackerStatus=Integer;

  pSceVrTrackerQuality=^SceVrTrackerQuality;
  SceVrTrackerQuality=Integer;

  pSceVrTrackerHmdRearTrackingStatus=^SceVrTrackerHmdRearTrackingStatus;
  SceVrTrackerHmdRearTrackingStatus=packed record
  end;

  pSceVrTrackerPoseData=^SceVrTrackerPoseData;
  SceVrTrackerPoseData=packed record
   position   :SceFVector3;
   reserved00 :array[0..0] of DWORD;
   orientation:SceFQuaternion;
   reserved01 :array[0..7] of DWORD;
  end;

  pSceVrTrackerHmdInfo=^SceVrTrackerHmdInfo;
  SceVrTrackerHmdInfo=packed record
   devicePose               :SceVrTrackerPoseData;
   leftEyePose              :SceVrTrackerPoseData;
   rightEyePose             :SceVrTrackerPoseData;
   headPose                 :SceVrTrackerPoseData;
   rearTrackingStatus       :SceVrTrackerHmdRearTrackingStatus;
   reserved00               :array[0..2] of DWORD;
   sensorReadSystemTimestamp:QWORD;
   reserved01               :array[0..9] of DWORD;
  end;

  pSceVrTrackerPadInfo=^SceVrTrackerPadInfo;
  SceVrTrackerPadInfo=packed record
   devicePose:SceVrTrackerPoseData;
   reserved:array[0..63] of DWORD;
  end;

  pSceVrTrackerMoveInfo=^SceVrTrackerMoveInfo;
  SceVrTrackerMoveInfo=packed record
   devicePose:SceVrTrackerPoseData;
   reserved:array[0..63] of DWORD;
  end;

  pSceVrTrackerLedAdjustmentStatus=^SceVrTrackerLedAdjustmentStatus;
  SceVrTrackerLedAdjustmentStatus=Integer;

  pSceVrTrackerLedResult=^SceVrTrackerLedResult;
  SceVrTrackerLedResult=packed record
   x,y,rx,ry:single;
   reserved:array[0..3] of DWORD;
  end;

  pSceVrTrackerResultData=^SceVrTrackerResultData;
  SceVrTrackerResultData=packed record
   handle                            :Integer;
   connected                         :DWORD;
   reserved00                        :array[0..1] of DWORD;
   timestamp                         :QWORD;
   deviceTimestamp                   :QWORD;
   recalibrateNecessity              :SceVrTrackerRecalibrateNecessityType;
   playareaBrightnessRisk            :SceVrTrackerPlayareaBrightnessRiskType;
   reserved01                        :array[0..1] of QWORD;
   ledColor                          :SceVrTrackerLedColor;
   status                            :SceVrTrackerStatus;
   positionQuality,orientationQuality:SceVrTrackerQuality;
   velocity                          :SceFVector3;
   acceleration                      :SceFVector3;
   angularVelocity                   :SceFVector3;
   angularAcceleration               :SceFVector3;
   cameraOrientation                 :SceFQuaternion;
   hmdInfo                           :SceVrTrackerHmdInfo;
   padInfo                           :SceVrTrackerPadInfo;
   moveInfo                          :SceVrTrackerMoveInfo;
   gunInfo                           :SceVrTrackerMoveInfo;
   userFrameNumber                   :DWORD;
   ledAdjustmentStatus               :SceVrTrackerLedAdjustmentStatus;
   timestampOfLedResult              :QWORD;
   reserved04                        :array[0..1] of DWORD;
   numberOfLedResult                 :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1] of Integer;
   reserved05                        :array[0..3] of DWORD;
   led                               :array[0..SCE_CAMERA_MAX_DEVICE_NUM-1,0..SCE_VR_TRACKER_MAX_LED_NUM-1] of SceVrTrackerLedResult;
  end;

  pSceVrTrackerUpdateMotionSensorDataOperationMode=^SceVrTrackerUpdateMotionSensorDataOperationMode;
  SceVrTrackerUpdateMotionSensorDataOperationMode=Integer;

  pSceVrTrackerUpdateMotionSensorDataParam=^SceVrTrackerUpdateMotionSensorDataParam;
  SceVrTrackerUpdateMotionSensorDataParam=packed record
   sizeOfThis   :DWORD;
   deviceType   :SceVrTrackerDeviceType;
   operationMode:SceVrTrackerUpdateMotionSensorDataOperationMode;
   handle       :Integer;
   reserved     :array[0..3] of DWORD;
  end;

implementation

function ps4_sceVrTrackerQueryMemory(param,pResult:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerInit(param:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerTerm():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerRegisterDevice(const deviceType:SceVrTrackerDeviceType;
                                        const handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerGpuSubmit(const param:pSceVrTrackerGpuSubmitParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerGetTime(time:pQWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerGpuWaitAndCpuProcess():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerGetResult(const param:pSceVrTrackerGetResultParam;_result:pSceVrTrackerResultData):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceVrTrackerUpdateMotionSensorData(const param:pSceVrTrackerUpdateMotionSensorDataParam):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceVrTracker(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceVrTracker');
 lib^.set_proc($2BBCA162BB0804F7,@ps4_sceVrTrackerQueryMemory);
 lib^.set_proc($424465EE90114FD3,@ps4_sceVrTrackerInit);
 lib^.set_proc($201BF83F7AB5A50D,@ps4_sceVrTrackerTerm);
 lib^.set_proc($B0887C1B071EBDA4,@ps4_sceVrTrackerRegisterDevice);
 lib^.set_proc($4D57A00CC2DA041F,@ps4_sceVrTrackerGpuSubmit);
 lib^.set_proc($5E8796CD796B9CCC,@ps4_sceVrTrackerGetTime);
 lib^.set_proc($011860A57BF0A11D,@ps4_sceVrTrackerGpuWaitAndCpuProcess);
 lib^.set_proc($EFA381BEBAD05D47,@ps4_sceVrTrackerGetResult);
 lib^.set_proc($F5FBC73146ECA26E,@ps4_sceVrTrackerUpdateMotionSensorData); 
end;

initialization
 ps4_app.RegistredPreLoad('libSceVrTracker.prx',@Load_libSceVrTracker);

end.

