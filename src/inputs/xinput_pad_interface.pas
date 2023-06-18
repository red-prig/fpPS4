unit xinput_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 XInput,
 sce_pad_types,
 sce_pad_interface,
 kbm_pad_interface;

type
 TXInputPadHandle=class(TScePadHandle)
  var
   connected     :Boolean;
   connectedCount:Byte;
  function ReadState(data:PScePadData):Integer; override;
 end;

 TXInputPadInterface=class(TScePadInterface)
  class var
   xinput_init:Boolean;
  class function  Load:Boolean; override;
  class procedure Unload;       override;
  class function  Init:Integer; override;
  class function  Done:Integer; override;
  class function  Open(index:Integer;var handle:TScePadHandle):Integer; override;
 end;

implementation

uses
 formController,
 uMappableInputs;

class function TXInputPadInterface.Load:Boolean;
var
 i:Integer;
begin
 i:=XInput.XInput_Init();
 xinput_init:=(i=0);

 if xinput_init then
 begin
  Writeln('XInput initialized!');
 end else
 begin
  Writeln('XInput not initialized!');
 end;

 Result:=xinput_init;
end;

class procedure TXInputPadInterface.Unload;
begin
 if not xinput_init then Exit;
 XInput_Quit;
 xinput_init:=False;
 Writeln('XInput quit!');
end;

class function TXInputPadInterface.Init:Integer;
begin
 if (xinput_init) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_PAD_ERROR_NOT_INITIALIZED;
 end;
end;

class function TXInputPadInterface.Done:Integer;
begin
 Result:=0;
end;

class function TXInputPadInterface.Open(index:Integer;var handle:TScePadHandle):Integer;
begin
 Result:=0;
 if (index<0) or (index>15) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (pad_opened[index]<>nil) then Exit(SCE_PAD_ERROR_ALREADY_OPENED);

 handle:=TXInputPadHandle.Create;
 TXInputPadHandle(handle).index:=index;

 pad_opened[index]:=handle;
end;

function TXInputPadHandle.ReadState(data:PScePadData):Integer;
var
 cs:TXInputState;
 controllerIndex:DWORD;
 stateResult:DWORD;
begin
 Result:=0;

 if (index>=XUSER_MAX_COUNT) then
 begin
  data^.connected     :=False;
  data^.connectedCount:=0;
  Exit;
 end;

 cs:=Default(TXInputState);
 stateResult:=XInputGetState(index, cs);

 if (stateResult=ERROR_DEVICE_NOT_CONNECTED) then
 begin
  //disconnect
  connected:=False;
  //
  data^.connected     :=connected;
  data^.connectedCount:=connectedCount;
  Exit;
 end else
 begin
  //connect
  if not connected then
  begin
   connected:=True;
   Inc(connectedCount);
  end;
 end;

 data^.connected     :=connected;
 data^.connectedCount:=connectedCount;

 TMouseAsTouchpad.ReadState(data);

 //if not MappableInputs.XInputEnabled then break; ?????

 if MappableInputs.PS4IsPressed(miCross, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CROSS;
 if MappableInputs.PS4IsPressed(miCircle, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CIRCLE;
 if MappableInputs.PS4IsPressed(miSquare, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_SQUARE;
 if MappableInputs.PS4IsPressed(miTriangle, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TRIANGLE;

 if MappableInputs.PS4IsPressed(miOptions, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;
 if MappableInputs.PS4IsPressed(miTouchPad, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TOUCH_PAD;

 if MappableInputs.PS4IsPressed(miL1, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L1;
 if MappableInputs.PS4IsPressed(miR1, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R1;
 if MappableInputs.PS4IsPressed(miL3, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L3;
 if MappableInputs.PS4IsPressed(miR3, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R3;

 if MappableInputs.PS4IsPressed(miDPadUp, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_UP;
 if MappableInputs.PS4IsPressed(miDPadDown, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_DOWN;
 if MappableInputs.PS4IsPressed(miDPadLeft, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_LEFT;
 if MappableInputs.PS4IsPressed(miDPadRight, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_RIGHT;

 data^.leftStick.x:=Trunc(128+(MappableInputs.GetAnalog(miLJoyRight, cs)-MappableInputs.GetAnalog(miLJoyLeft, cs))*127);
 data^.leftStick.y:=Trunc(128+(MappableInputs.GetAnalog(miLJoyDown , cs)-MappableInputs.GetAnalog(miLJoyUp  , cs))*127);

 data^.rightStick.x:=Trunc(128+(MappableInputs.GetAnalog(miRJoyRight, cs)-MappableInputs.GetAnalog(miRJoyLeft, cs))*127);
 data^.rightStick.y:=Trunc(128+(MappableInputs.GetAnalog(miRJoyDown , cs)-MappableInputs.GetAnalog(miRJoyUp  , cs))*127);

 data^.analogButtons.l2:=Trunc(MappableInputs.GetAnalog(miL2, cs)*255);
 data^.analogButtons.r2:=Trunc(MappableInputs.GetAnalog(miR2, cs)*255);

 if MappableInputs.PS4IsPressed(miL2, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L2;
 if MappableInputs.PS4IsPressed(miR2, cs) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R2;
end;

end.



