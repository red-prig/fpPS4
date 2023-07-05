unit sce_pad_types;

{$mode ObjFPC}{$H+}

interface

const
 SCE_PAD_ERROR_INVALID_ARG             =-2137915391; // 0x80920001
 SCE_PAD_ERROR_INVALID_PORT            =-2137915390; // 0x80920002
 SCE_PAD_ERROR_INVALID_HANDLE          =-2137915389; // 0x80920003
 SCE_PAD_ERROR_ALREADY_OPENED          =-2137915388; // 0x80920004
 SCE_PAD_ERROR_NOT_INITIALIZED         =-2137915387; // 0x80920005
 SCE_PAD_ERROR_INVALID_LIGHTBAR_SETTING=-2137915386; // 0x80920006
 SCE_PAD_ERROR_DEVICE_NOT_CONNECTED    =-2137915385; // 0x80920007
 SCE_PAD_ERROR_NO_HANDLE               =-2137915384; // 0x80920008
 SCE_PAD_ERROR_FATAL                   =-2137915137; // 0x809200FF

 //ScePadButtonDataOffset
 SCE_PAD_BUTTON_L3          = $00000002;
 SCE_PAD_BUTTON_R3          = $00000004;
 SCE_PAD_BUTTON_OPTIONS     = $00000008;
 SCE_PAD_BUTTON_UP          = $00000010;
 SCE_PAD_BUTTON_RIGHT       = $00000020;
 SCE_PAD_BUTTON_DOWN        = $00000040;
 SCE_PAD_BUTTON_LEFT        = $00000080;
 SCE_PAD_BUTTON_L2          = $00000100;
 SCE_PAD_BUTTON_R2          = $00000200;
 SCE_PAD_BUTTON_L1          = $00000400;
 SCE_PAD_BUTTON_R1          = $00000800;
 SCE_PAD_BUTTON_TRIANGLE    = $00001000;
 SCE_PAD_BUTTON_CIRCLE      = $00002000;
 SCE_PAD_BUTTON_CROSS       = $00004000;
 SCE_PAD_BUTTON_SQUARE      = $00008000;
 SCE_PAD_BUTTON_TOUCH_PAD   = $00100000;
 SCE_PAD_BUTTON_INTERCEPTED = $80000000;

 // This definietion is alias for support old style.
 SCE_PAD_BUTTON_START=SCE_PAD_BUTTON_OPTIONS;

 //Maximum number of touch points.
 SCE_PAD_MAX_TOUCH_NUM=2;

 //device unique data size
 SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE=12;

 SCE_PAD_CONNECTION_TYPE_LOCAL            =0;
 SCE_PAD_CONNECTION_TYPE_REMOTE           =1;
 SCE_PAD_CONNECTION_TYPE_REMOTE_VITA      =SCE_PAD_CONNECTION_TYPE_REMOTE;
 SCE_PAD_CONNECTION_TYPE_REMOTE_DUALSHOCK4=2;

 //ScePadDeviceClass
 SCE_PAD_DEVICE_CLASS_INVALID        = -1;
 SCE_PAD_DEVICE_CLASS_STANDARD       = 0;
 SCE_PAD_DEVICE_CLASS_GUITAR         = 1;
 SCE_PAD_DEVICE_CLASS_DRUM           = 2;
 SCE_PAD_DEVICE_CLASS_DJ_TURNTABLE   = 3;
 SCE_PAD_DEVICE_CLASS_DANCEMAT       = 4;
 SCE_PAD_DEVICE_CLASS_NAVIGATION     = 5;
 SCE_PAD_DEVICE_CLASS_STEERING_WHEEL = 6;
 SCE_PAD_DEVICE_CLASS_STICK          = 7;
 SCE_PAD_DEVICE_CLASS_FLIGHT_STICK   = 8;
 SCE_PAD_DEVICE_CLASS_GUN            = 9;

 // Personal user
 SCE_PAD_PORT_TYPE_STANDARD=0; // for standard controller
 SCE_PAD_PORT_TYPE_SPECIAL =2; // for special controller

 //SYSTEM user(SCE_USER_SERVICE_USER_ID_SYSTEM)
 SCE_PAD_PORT_TYPE_REMOTE_CONTROL=16; // for remote control(CEC remote control)

type
 Tvec_float3=packed record
  x,y,z:Single;
 end;

 Tvec_float4=packed record
  x,y,z,w:Single;
 end;

 ScePadAnalogStick=packed record
  x,y:Byte;
 end;

 ScePadAnalogButtons=packed record
  l2,r2:Byte;
  padding:Word;
 end;

 ScePadTouch=packed record
  x:Word;
  y:Word;
  id:Byte;
  reserve:array[0..2] of Byte;
 end;

 ScePadTouchData=packed record
  touchNum:Byte;
  reserve:array[0..2] of Byte;
  reserve1:DWORD;
  touch:array[0..SCE_PAD_MAX_TOUCH_NUM-1] of ScePadTouch;
 end;

 ScePadExtensionUnitData=packed record
  extensionUnitId:DWORD;
  reserve:Byte;
  dataLength:Byte;
  data:array[0..9] of Byte;
 end;

 PScePadData=^ScePadData;
 ScePadData=packed record
  buttons          :DWORD;
  leftStick        :ScePadAnalogStick;
  rightStick       :ScePadAnalogStick;
  analogButtons    :ScePadAnalogButtons;
  orientation      :Tvec_float4;
  acceleration     :Tvec_float3;
  angularVelocity  :Tvec_float3;
  touchData        :ScePadTouchData;
  connected        :Boolean;
  _align:array[0..2] of Byte;
  timestamp        :QWORD;
  extensionUnitData:ScePadExtensionUnitData;
  connectedCount   :Byte;
  reserve:array[0..1] of Byte;
  deviceUniqueDataLen:Byte;
  deviceUniqueData:array[0..SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE-1] of Byte;
 end;

 ScePadOpenParam=packed record
  reserve:array[0..7] of Byte;
 end;

 TPadColor=packed record
  r,g,b,a:Byte;
 end;

 PScePadVibrationParam=^ScePadVibrationParam;
 ScePadVibrationParam=packed record
  largeMotor:Byte;
  smallMotor:Byte;
 end;

 ScePadColor=packed record
  r:Byte;
  g:Byte;
  b:Byte;
  reserve:Byte;
 end;

 ScePadLightBarParam=ScePadColor;
 PScePadLightBarParam=^ScePadLightBarParam;

 ScePadTouchPadInformation=packed record
  pixelDensity:Single;
  resolution:packed record
   x,y:Word;
  end;
 end;

 ScePadStickInformation=packed record
  deadZoneLeft :Byte;
  deadZoneRight:Byte;
 end;

 PScePadControllerInformation=^ScePadControllerInformation;
 ScePadControllerInformation=packed record
  touchPadInfo:ScePadTouchPadInformation;
  stickInfo:ScePadStickInformation;
  connectionType:Byte;
  connectedCount:Byte;
  connected:Boolean;
  _align:array[0..2] of Byte;
  deviceClass:DWORD; //ScePadDeviceClass
  reserve:array[0..7] of Byte;
 end;

 pScePadDeviceClassExtendedInformation=^ScePadDeviceClassExtendedInformation;
 ScePadDeviceClassExtendedInformation=packed record
  deviceClass:DWORD; //ScePadDeviceClass
  reserved:DWORD;
  classData:packed record
   Case Byte of

    0:(steeringWheel:packed record
        capability:Byte;
        reserved1:Byte;
        maxPhysicalWheelAngle:Word;
        reserved2:QWORD;
       end);

    1:(guitar:packed record
        capability:Byte;
        quantityOfSelectorSwitch:Byte;
        reserved1:Word;
        reserved2:QWORD;
       end);

    2:(drum:packed record
        capability:Byte;
        reserved1:Byte;
        reserved2:Word;
        reserved3:QWORD;
       end);

    3:(flightStick:packed record
        capability:Byte;
        reserved1:Byte;
        reserved2:Word;
        reserved3:QWORD;
       end);

    4:(data:array[0..11] of Byte);

  end;
 end;

 pScePadDeviceClassData=^ScePadDeviceClassData;
 ScePadDeviceClassData=packed record
  deviceClass:DWORD; //ScePadDeviceClass
  bDataValid :Boolean;
  _align:array[0..2] of Byte;
  classData:packed record
   Case Byte of

    0:(steeringWheel:packed record
        steeringWheelAngle:Single;
        steeringWheel     :Word;
        acceleratorPedal  :Word;
        brakePedal        :Word;
        clutchPedal       :Word;
        handBlake         :Word;
        gear              :Byte;
        reserved          :Byte;
       end); //SCE_PAD_DEVICE_CLASS_STEERING_WHEEL

    1:(guitar:packed record
        toneNumber:Byte;
        whammyBar :Byte;
        tilt      :Byte;
        fret      :Byte;
        fretSolo  :Byte;
        reserved:array[0..10] of Byte;
       end); //SCE_PAD_DEVICE_CLASS_GUITAR

    2:(drum:packed record
        snare      :Byte;
        tom1       :Byte;
        tom2       :Byte;
        floorTom   :Byte;
        hihatCymbal:Byte;
        rideCymbal :Byte;
        crashCymbal:Byte;
        reserved:array[0..8] of Byte;
       end); //SCE_PAD_DEVICE_CLASS_DRUM

    3:(flightStick:packed record
        stickAxisX     :Word;
        stickAxisY     :Word;
        stickTwist     :Byte;
        throttle       :Byte;
        trigger        :Byte;
        rudderPedal    :Byte;
        brakePedalLeft :Byte;
        brakePedalRight:Byte;
        antennaKnob    :Byte;
        rangeKnob      :Byte;
        reserved:array[0..3] of Byte;
       end); //SCE_PAD_DEVICE_CLASS_FLIGHT_STICK

    4:(others:packed record
        dataLen:Byte;
        reserved:array[0..2] of Byte;
        data:array[0..SCE_PAD_MAX_DEVICE_UNIQUE_DATA_SIZE-1] of Byte;
       end); //Not Supported device

  end;
 end;

implementation

end.

