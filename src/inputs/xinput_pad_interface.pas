unit xinput_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 spinlock,
 XInput,
 sce_pad_types,
 sce_pad_interface,
 kbm_pad_interface;

type
 TXInputPadHandle=class(TScePadHandle)
  var
   UserIndex     :Integer;
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
  class function  FindOpened(UserIndex,prev:Integer):Boolean;
  class function  FindDevice(prev:Integer):Integer;
  class function  Open(var handle:TScePadHandle):Integer; override;
 end;

implementation

uses
 formController,
 uMappableInputs;

class function TXInputPadInterface.Load:Boolean;
var
 i:Integer;
begin
 if xinput_init then Exit(True);

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

class function TXInputPadInterface.FindOpened(UserIndex,prev:Integer):Boolean;
var
 i:Integer;
 h:TXInputPadHandle;
begin
 Result:=False;
 spin_lock(pad_lock);
 For i:=0 to 15 do
  if (pad_opened[i]<>nil) then
  if (pad_opened[i].InheritsFrom(TXInputPadHandle)) then
  begin
   h:=TXInputPadHandle(pad_opened[i]);
   if (h.UserIndex<>prev) then
   begin
    Result:=(UserIndex=h.UserIndex);
    if Result then Break;
   end;
  end;
 spin_unlock(pad_lock);
end;

class function TXInputPadInterface.FindDevice(prev:Integer):Integer;
var
 cs:TXInputState;
 i:Integer;
 first,compared:Integer;
begin
 Result:=-1;
 first:=-1;
 compared:=-1;
 cs:=Default(TXInputState);
 For i:=0 to XUSER_MAX_COUNT-1 do
 begin
  if (XInputGetState(i, cs)=0) then
  if not FindOpened(i,prev) then
  begin
   if (first=-1) then first:=i;
   if (prev=-1) then
   begin
    compared:=i;
    Break;
   end else
   if (i=prev) then
   begin
    compared:=i;
    Break;
   end;
  end;
 end;
 //
 if (compared=-1) then compared:=first;
 if (compared<>-1) then
 begin
  Result:=compared;
 end;
end;

class function TXInputPadInterface.Open(var handle:TScePadHandle):Integer;
var
 UserIndex:Integer;
begin
 Result:=0;

 UserIndex:=FindDevice(-1);

 handle:=TXInputPadHandle.Create;
 TXInputPadHandle(handle).UserIndex:=UserIndex;
end;

function TXInputPadHandle.ReadState(data:PScePadData):Integer;
label
 _retry;
var
 cs:TXInputState;
 new:Integer;
 stateResult:DWORD;

 procedure DoConnect(new:DWORD); inline;
 begin
  UserIndex:=new;
  if not connected then
  begin
   connected:=True;
   Inc(connectedCount);
  end;
 end;

begin
 Result:=0;

 if (UserIndex=-1) then //not attached
 begin
  new:=TXInputPadInterface.FindDevice(UserIndex);
  if (new<>-1) then
  begin
   //connect
   DoConnect(new);
  end else
  begin
   //not connected
   data^.connected     :=False;
   data^.connectedCount:=0;
   Exit;
  end;
 end;

 _retry:
 cs:=Default(TXInputState);
 stateResult:=XInputGetState(UserIndex, cs);

 if (stateResult=ERROR_DEVICE_NOT_CONNECTED) then
 begin
  //disconnect
  connected:=False;
  //
  new:=TXInputPadInterface.FindDevice(UserIndex);
  if (new<>-1) then
  begin
   //connect
   DoConnect(new);
   goto _retry;
  end else
  begin
   //not connected
   data^.connected     :=False;
   data^.connectedCount:=0;
   Exit;
  end;
  //
  data^.connected     :=connected;
  data^.connectedCount:=connectedCount;
  Exit;
 end else
 begin
  //connect
  DoConnect(UserIndex);
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

finalization
 TXInputPadInterface.Unload;

end.



