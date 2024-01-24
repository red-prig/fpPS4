unit ps4_libSceSystemGesture;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  ps4_libScePad,
  Classes,
  SysUtils;

const
  SCE_PAD_MAX_TOUCH_NUM=2;
  SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE=4;

  //SceSystemGestureType
  SCE_SYSTEM_GESTURE_TYPE_TAP         =$00000001;
  SCE_SYSTEM_GESTURE_TYPE_DRAG        =$00000002;
  SCE_SYSTEM_GESTURE_TYPE_TAP_AND_HOLD=$00000004;
  SCE_SYSTEM_GESTURE_TYPE_PINCH_OUT_IN=$00000008;
  SCE_SYSTEM_GESTURE_TYPE_ROTATION    =$00000010;
  SCE_SYSTEM_GESTURE_TYPE_FLICK       =$00000020;

type
  pSceFQuaternion=^SceFQuaternion;
  SceFQuaternion=packed record
   x,y,z,w:single;
  end;

  pSceFVector3=^SceFVector3;
  SceFVector3=packed record
   x,y,z:single;
  end;

  pScePadAnalogStick=^ScePadAnalogStick;
  ScePadAnalogStick=packed record
   x,y:Byte;
  end;

  pScePadAnalogButtons=^ScePadAnalogButtons;
  ScePadAnalogButtons=packed record
   l2,r2:Byte;
   padding:array[0..1] of Byte;
  end;

  pScePadTouch=^ScePadTouch;
  ScePadTouch=packed record
   x,y    :Word;
   id     :Byte;
   reserve:array[0..2] of Byte;
  end;

  pScePadTouchData=^ScePadTouchData;
  ScePadTouchData=packed record
   touchNum:Byte;
   reserve :array[0..2] of Byte;
   reserve1:QWORD;
   touch   :array[0..SCE_PAD_MAX_TOUCH_NUM-1] of ScePadTouch;
  end;

  pScePadExtensionUnitData=^ScePadExtensionUnitData;
  ScePadExtensionUnitData=packed record
   extensionUnitId:DWORD;
   reserve        :array[0..0] of Byte;
   dataLength     :Byte;
   data           :array[0..9] of Byte;
  end;

  pScePadData=^ScePadData;
  ScePadData=packed record
   buttons            :DWORD;
   leftStick          :ScePadAnalogStick;
   rightStick         :ScePadAnalogStick;
   analogButtons      :ScePadAnalogButtons;
   orientation        :SceFQuaternion;
   acceleration       :SceFVector3;
   angularVelocity    :SceFVector3;
   touchData          :ScePadTouchData;
   connected          :single;
   timestamp          :QWORD;
   extensionUnitData  :ScePadExtensionUnitData;
   connectedCount     :Byte;
   reserve            :array[0..1] of Byte;
   deviceUniqueDataLen:Byte;
   deviceUniqueData   :array[0..SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE-1] of Byte;
  end;

  pSceSystemGesturePrimitiveTouchRecognizerParameter=^SceSystemGesturePrimitiveTouchRecognizerParameter;
  SceSystemGesturePrimitiveTouchRecognizerParameter=packed record
   reserve:array[0..63] of Byte;
  end;

  pSceSystemGestureOpenParameter=^SceSystemGestureOpenParameter;
  SceSystemGestureOpenParameter=packed record
   reserve:array[0..7] of Byte;
  end;

  pSceSystemGestureTouchRecognizer=^SceSystemGestureTouchRecognizer;
  SceSystemGestureTouchRecognizer=packed record
   reserve:array[0..360] of QWORD;
  end;

  pSceSystemGestureType=^SceSystemGestureType;
  SceSystemGestureType=Integer;

  pSceSystemGestureRectangle=^SceSystemGestureRectangle;
  SceSystemGestureRectangle=packed record
   x,y,width,height:single;
   reserve:array[0..7] of Byte;
  end;

  pSceSystemGestureTapRecognizerParameter=^SceSystemGestureTapRecognizerParameter;
  SceSystemGestureTapRecognizerParameter=packed record
   maxTapCount:Byte;
   reserve:array[0..62] of Byte;
  end;

  pSceSystemGestureDragRecognizerParameter=^SceSystemGestureDragRecognizerParameter;
  SceSystemGestureDragRecognizerParameter=packed record
   reserve:array[0..63] of Byte;
  end;

  pSceSystemGestureTapAndHoldRecognizerParameter=^SceSystemGestureTapAndHoldRecognizerParameter;
  SceSystemGestureTapAndHoldRecognizerParameter=packed record
   timeToInvokeEvent:QWORD;
   reserve:array[0..55] of Byte;
  end;

  pSceSystemGesturePinchOutInRecognizerParameter=^SceSystemGesturePinchOutInRecognizerParameter;
  SceSystemGesturePinchOutInRecognizerParameter=packed record
   reserve:array[0..63] of Byte;
  end;

  pSceSystemGestureRotationRecognizerParameter=^SceSystemGestureRotationRecognizerParameter;
  SceSystemGestureRotationRecognizerParameter=packed record
   reserve:array[0..63] of Byte;
  end;

  pSceSystemGestureFlickRecognizerParameter=^SceSystemGestureFlickRecognizerParameter;
  SceSystemGestureFlickRecognizerParameter=packed record
   reserve:array[0..63] of Byte;
  end;

  pSceSystemGestureTouchRecognizerParameter=^SceSystemGestureTouchRecognizerParameter;
  SceSystemGestureTouchRecognizerParameter=packed record
   parameterBuf:array[0..63] of Byte;
   tap         :SceSystemGestureTapRecognizerParameter;
   drag        :SceSystemGestureDragRecognizerParameter;
   tapAndHold  :SceSystemGestureTapAndHoldRecognizerParameter;
   pinchOutIn  :SceSystemGesturePinchOutInRecognizerParameter;
   rotation    :SceSystemGestureRotationRecognizerParameter;
   flick       :SceSystemGestureFlickRecognizerParameter;
  end;

  pSceSystemGestureTouchPadData=^SceSystemGestureTouchPadData;
  SceSystemGestureTouchPadData=packed record
   padHandle    :Integer;
   reportNumber :Integer;
   padDataBuffer:pScePadData;
   reserve      :array[0..7] of Byte;
  end;

implementation

function ps4_sceSystemGestureInitializePrimitiveTouchRecognizer(parameter:pSceSystemGesturePrimitiveTouchRecognizerParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSystemGestureOpen(inputType:Integer;
                                  parameter:pSceSystemGestureOpenParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSystemGestureCreateTouchRecognizer(gestureHandle:Integer;
                                                   touchRecognizer:pSceSystemGestureTouchRecognizer;
                                                   gestureType:SceSystemGestureType;
                                                   rectangle:pSceSystemGestureRectangle;
                                                   touchRecognizerParameter:pSceSystemGestureTouchRecognizerParameter):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSystemGestureAppendTouchRecognizer(gestureHandle:Integer;
                                                   touchRecognizer:pSceSystemGestureTouchRecognizer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSystemGestureUpdatePrimitiveTouchRecognizer(gestureHandle:Integer;
                                                            const pInputData:pSceSystemGestureTouchPadData):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSystemGestureUpdateAllTouchRecognizer(gestureHandle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceSystemGestureGetTouchEventsCount(gestureHandle:Integer;
                                                 const touchRecognizer:pSceSystemGestureTouchRecognizer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceSystemGesture(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSystemGesture');
 lib^.set_proc($DE9700BE6C0A0AF3,@ps4_sceSystemGestureInitializePrimitiveTouchRecognizer);
 lib^.set_proc($AA9A3F9843B08DED,@ps4_sceSystemGestureOpen);
 lib^.set_proc($15617CCE486BF39E,@ps4_sceSystemGestureCreateTouchRecognizer);
 lib^.set_proc($D4C30AD16FE43200,@ps4_sceSystemGestureAppendTouchRecognizer);
 lib^.set_proc($1A014C6F6DAC6DB2,@ps4_sceSystemGestureUpdatePrimitiveTouchRecognizer);
 lib^.set_proc($C0F246C08D913362,@ps4_sceSystemGestureUpdateAllTouchRecognizer);
 lib^.set_proc($87CBA89E0701355B,@ps4_sceSystemGestureGetTouchEventsCount);
end;

initialization
 ps4_app.RegistredPreLoad('libSceSystemGesture.prx',@Load_libSceSystemGesture);

end.

