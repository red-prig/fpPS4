unit uMappableInputs;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, XInput, IniFiles, Math;

const
 NUM_PS4_BUTTONS = 25;
 NUM_XINPUT_BUTTONS = 25;
 XINPUT_CONFIG_FILE = 'config/xinput.ini';

type
 // Mappable inputs of the PS4 controller
 EnumPS4Buttons = (
  miUnbound = 0,
  miLJoyUp = 1,
  miLJoyDown = 2,
  miLJoyLeft = 3,
  miLJoyRight = 4,

  miRJoyUp = 5,
  miRJoyDown = 6,
  miRJoyLeft = 7,
  miRJoyRight = 8,

  miDPadUp = 9,
  miDPadDown = 10,
  miDPadLeft = 11,
  miDPadRight = 12,

  miCross = 13,
  miCircle = 14,
  miSquare = 15,
  miTriangle = 16,

  miShare = 17,
  miTouchPad = 18,
  miOptions = 19,

  miL1 = 20,
  miL2 = 21,
  miL3 = 22,

  miR1 = 23,
  miR2 = 24,
  miR3 = 25
  );

 // XInput buttons enum
 EnumXInputButtons = (
  xiUnbound = 0,
  xiLJoyUp = 1,
  xiLJoyDown = 2,
  xiLJoyLeft = 3,
  xiLJoyRight = 4,

  xiRJoyUp = 5,
  xiRJoyDown = 6,
  xiRJoyLeft = 7,
  xiRJoyRight = 8,

  xiDPadUp = 9,
  xiDPadDown = 10,
  xiDPadLeft = 11,
  xiDPadRight = 12,

  xiA = 13,
  xiB = 14,
  xiX = 15,
  xiY = 16,

  xiSelect = 17,
  xiGuide = 18,
  xiStart = 19,

  xiL1 = 20,
  xiL2 = 21,
  xiL3 = 22,

  xiR1 = 23,
  xiR2 = 24,
  xiR3 = 25
  );

type
 TMappableInputs = class(TObject)
 public
  XInputEnabled: boolean;

  PS4toXInput: array[0..NUM_PS4_BUTTONS] of EnumXInputButtons;
  XInputToPS4: array[0..NUM_XINPUT_BUTTONS] of EnumPS4Buttons;

  XInputButtonsNames: array[0..NUM_XINPUT_BUTTONS] of string;
  XInputDeadzoneLeft, XInputDeadzoneRight: integer;
  XInputDeadzoneTrigger : Byte;

  function GetAnalog(input : EnumPS4Buttons; s : TXInputState) : Single;
  function PS4IsPressed(input: EnumPS4Buttons; s : TXInputState): boolean;

  function XInputStateToKey(state: TXInputState): EnumXInputButtons;
  function XInputIsTriggered(input: EnumXInputButtons; state: TXInputState): boolean;
  function XInputIsAnalog(input: EnumXInputButtons): boolean;
  procedure SetXInputMapping(mappableInput: EnumPS4Buttons; xinput: EnumXInputButtons);

  // Save / load
  function LoadFromFile(filePath: string): boolean;  
  function SaveToFile(filePath: string): boolean;
 end;

var
 MappableInputs: TMappableInputs;


implementation

uses TypInfo;

procedure TMappableInputs.SetXInputMapping(mappableInput: EnumPS4Buttons; xinput: EnumXInputButtons);
begin
 XInputToPS4[Ord(xinput)] := mappableInput;
 PS4toXInput[Ord(mappableInput)] := xinput;
end;

function TMappableInputs.XInputStateToKey(state: TXInputState): EnumXInputButtons;
var
 i: integer;
begin
 Result := xiUnbound;
 for i := 1 to NUM_XINPUT_BUTTONS do
 begin
  if XInputIsTriggered(EnumXInputButtons(i), state) then
  begin
   Result := EnumXInputButtons(i);
   exit;
  end;
 end;
end;

function TMappableInputs.XInputIsAnalog(input: EnumXInputButtons): boolean;
begin
 case (input) of
  xiLJoyUp, xiLJoyLeft, xiLJoyRight, xiLJoyDown: Result := True;
  xiRJoyUp, xiRJoyLeft, xiRJoyRight, xiRJoyDown: Result := True;  
  xiL2, xiR2: Result := True;
 else Result := false;
 end;
end;

function TMappableInputs.GetAnalog(input : EnumPS4Buttons; s : TXInputState) : Single;
var
 xinputButton : EnumXInputButtons;
 outOfDeadzoneL, outOfDeadzoneR : single;
begin
 xInputButton := PS4toXInput[Ord(input)];

 if XInputIsAnalog(xInputButton) then
 begin
  outOfDeadzoneL := IfThen(Trunc(sqrt(s.Gamepad.sThumbLX*s.Gamepad.sThumbLX+s.Gamepad.sThumbLY*s.Gamepad.sThumbLY)) > XInputDeadzoneLeft, 1, 0);  
  outOfDeadzoneR := IfThen(Trunc(sqrt(s.Gamepad.sThumbRX*s.Gamepad.sThumbRX+s.Gamepad.sThumbRY*s.Gamepad.sThumbRY)) > XInputDeadzoneRight, 1, 0);

  case (xInputButton) of
   xiLJoyUp   : Result := Max(s.Gamepad.sThumbLY, 0) * outOfDeadzoneL / 32767.0;
   xiLJoyDown : Result := Min(s.Gamepad.sThumbLY, 0) * outOfDeadzoneL / 32767.0;
   xiLJoyRight: Result := Max(s.Gamepad.sThumbLX, 0) * outOfDeadzoneL / 32767.0;
   xiLJoyLeft : Result := Min(s.Gamepad.sThumbLX, 0) * outOfDeadzoneL / 32767.0;

   xiRJoyUp   : Result := Max(s.Gamepad.sThumbRY, 0) * outOfDeadzoneR / 32767.0;
   xiRJoyDown : Result := Min(s.Gamepad.sThumbRY, 0) * outOfDeadzoneR / 32767.0;
   xiRJoyRight: Result := Max(s.Gamepad.sThumbRX, 0) * outOfDeadzoneR / 32767.0;
   xiRJoyLeft : Result := Min(s.Gamepad.sThumbRX, 0) * outOfDeadzoneR / 32767.0;

   xiL2: Result := IfThen(s.Gamepad.bLeftTrigger  > XInputDeadzoneTrigger, s.Gamepad.bLeftTrigger  / 255.0, 0);
   xiR2: Result := IfThen(s.Gamepad.bRightTrigger > XInputDeadzoneTrigger, s.Gamepad.bRightTrigger / 255.0, 0);
  else Result := 0;
  end;

 end else begin
   // is button
   Result := IfThen(XInputIsTriggered(xinputButton, s), 1.0, 0.0);
 end;

 // clamp between 0 and 1
 Result := Max(Min(Abs(Result), 1.0), 0.00);
end;

function TMappableInputs.PS4IsPressed(input: EnumPS4Buttons; s: TXInputState): boolean;
begin
 Result := XInputIsTriggered(PS4toXInput[Ord(input)], s);
end;

function TMappableInputs.LoadFromFile(filePath: string): boolean;
var
 iniFile: TIniFile;
 i: integer;
 xInputButton : Integer;
begin
 Result := False;

 if not FileExists(filePath) then exit;

 iniFile := TIniFile.Create(filePath);
 Self.XInputEnabled := iniFile.ReadBool('XInput', 'XInputEnabled', True); 
 Self.XInputDeadzoneLeft := iniFile.ReadInteger('XInput', 'XInputDeadzoneLeft', XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE);
 Self.XInputDeadzoneRight := iniFile.ReadInteger('XInput', 'XInputDeadzoneRight', XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE);  
 Self.XInputDeadzoneTrigger := iniFile.ReadInteger('XInput', 'XInputDeadzoneTrigger', XINPUT_GAMEPAD_TRIGGER_THRESHOLD);
 for i := 1 to NUM_PS4_BUTTONS do
 begin
  xInputButton := iniFile.ReadInteger('XInput', GetEnumName(TypeInfo(EnumPS4Buttons), i), 0);
  SetXInputMapping(EnumPS4Buttons(i), EnumXInputButtons(xInputButton));
 end;
 iniFile.Free;

 Result := True;
end;

function TMappableInputs.SaveToFile(filePath: string): boolean;
var
 iniFile: TIniFile;
 i: integer;
 xInputButton : Integer;
begin
 Result := False;

 iniFile := TIniFile.Create(filePath);
 iniFile.WriteBool('XInput', 'XInputEnabled', Self.XInputEnabled);
 iniFile.WriteInteger('XInput', 'XInputDeadzoneLeft',  Self.XInputDeadzoneLeft);
 iniFile.WriteInteger('XInput', 'XInputDeadzoneRight', Self.XInputDeadzoneRight);  
 iniFile.WriteInteger('XInput', 'XInputDeadzoneTrigger', Self.XInputDeadzoneTrigger);
 for i := 1 to NUM_PS4_BUTTONS do
 begin
  xInputButton := Ord(PS4toXInput[i]);
  iniFile.WriteInteger('XInput', GetEnumName(TypeInfo(EnumPS4Buttons), i), xInputButton);
 end;
 iniFile.Free;

 Result := True;
end;

function TMappableInputs.XInputIsTriggered(input: EnumXInputButtons; state: TXInputState): boolean;
begin
 case (input) of
  xiLJoyUp: Result := state.Gamepad.sThumbLY > self.XInputDeadzoneLeft;
  xiLJoyDown: Result := state.Gamepad.sThumbLY < -self.XInputDeadzoneLeft;
  xiLJoyLeft: Result := state.Gamepad.sThumbLX < -self.XInputDeadzoneLeft;
  xiLJoyRight: Result := state.Gamepad.sThumbLX > self.XInputDeadzoneLeft;

  xiRJoyUp: Result := state.Gamepad.sThumbRY > self.XInputDeadzoneRight;
  xiRJoyDown: Result := state.Gamepad.sThumbRY < -self.XInputDeadzoneRight;
  xiRJoyLeft: Result := state.Gamepad.sThumbRX < -self.XInputDeadzoneRight;
  xiRJoyRight: Result := state.Gamepad.sThumbRX > self.XInputDeadzoneRight;

  xiA: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_A) <> 0;
  xiB: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_B) <> 0;
  xiX: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_X) <> 0;
  xiY: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_Y) <> 0;

  xiDPadUp: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_UP) <> 0;
  xiDPadDown: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_DOWN) <> 0;
  xiDPadLeft: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_LEFT) <> 0;
  xiDPadRight: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_RIGHT) <> 0;

  xiSelect: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_BACK) <> 0;
  xiGuide: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_GUIDE) <> 0;
  xiStart: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_START) <> 0;

  xiL1: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_SHOULDER) <> 0;
  xiL2: Result := state.Gamepad.bLeftTrigger > self.XInputDeadzoneTrigger;
  xiL3: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_THUMB) <> 0;
  xiR1: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_SHOULDER) <> 0;
  xiR2: Result := state.Gamepad.bRightTrigger > self.XInputDeadzoneTrigger;
  xiR3: Result := (state.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_THUMB) <> 0;
 else Result := false;
 end;
end;

initialization

 MappableInputs := TMappableInputs.Create;

 MappableInputs.XInputButtonsNames[Ord(xiUnbound)] := 'Unbound';
 MappableInputs.XInputButtonsNames[Ord(xiLJoyUp)] := 'LJOY_UP';
 MappableInputs.XInputButtonsNames[Ord(xiLJoyDown)] := 'LJOY_DOWN';
 MappableInputs.XInputButtonsNames[Ord(xiLJoyLeft)] := 'LJOY_LEFT';
 MappableInputs.XInputButtonsNames[Ord(xiLJoyRight)] := 'LJOY_RIGHT';

 MappableInputs.XInputButtonsNames[Ord(xiRJoyUp)] := 'RJOY_UP';
 MappableInputs.XInputButtonsNames[Ord(xiRJoyDown)] := 'RJOY_DOWN';
 MappableInputs.XInputButtonsNames[Ord(xiRJoyLeft)] := 'RJOY_LEFT';
 MappableInputs.XInputButtonsNames[Ord(xiRJoyRight)] := 'RJOY_RIGHT';

 MappableInputs.XInputButtonsNames[Ord(xiDPadUp)] := 'DPAD_UP';
 MappableInputs.XInputButtonsNames[Ord(xiDPadDown)] := 'DPAD_DOWN';
 MappableInputs.XInputButtonsNames[Ord(xiDPadLeft)] := 'DPAD_LEFT';
 MappableInputs.XInputButtonsNames[Ord(xiDPadRight)] := 'DPAD_RIGHT';

 MappableInputs.XInputButtonsNames[Ord(xiA)] := 'A';
 MappableInputs.XInputButtonsNames[Ord(xiB)] := 'B';
 MappableInputs.XInputButtonsNames[Ord(xiX)] := 'X';
 MappableInputs.XInputButtonsNames[Ord(xiY)] := 'Y';

 MappableInputs.XInputButtonsNames[Ord(xiSelect)] := 'SELECT';
 MappableInputs.XInputButtonsNames[Ord(xiGuide)] := 'GUIDE';
 MappableInputs.XInputButtonsNames[Ord(xiStart)] := 'START';

 MappableInputs.XInputButtonsNames[Ord(xiL1)] := 'L1';
 MappableInputs.XInputButtonsNames[Ord(xiL2)] := 'L2';
 MappableInputs.XInputButtonsNames[Ord(xiL3)] := 'L3';

 MappableInputs.XInputButtonsNames[Ord(xiR1)] := 'R1';
 MappableInputs.XInputButtonsNames[Ord(xiR2)] := 'R2';
 MappableInputs.XInputButtonsNames[Ord(xiR3)] := 'R3';

 // Default mapping
 MappableInputs.XInputEnabled := True;    
 MappableInputs.XInputDeadzoneLeft :=  XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE;
 MappableInputs.XInputDeadzoneRight := XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE;
 MappableInputs.XInputDeadzoneTrigger := XINPUT_GAMEPAD_TRIGGER_THRESHOLD;
 MappableInputs.SetXInputMapping(miUnbound, xiUnbound);
 MappableInputs.SetXInputMapping(miLJoyUp, xiLJoyUp);
 MappableInputs.SetXInputMapping(miLJoyDown, xiLJoyDown);
 MappableInputs.SetXInputMapping(miLJoyLeft, xiLJoyLeft);
 MappableInputs.SetXInputMapping(miLJoyRight, xiLJoyRight);
 MappableInputs.SetXInputMapping(miRJoyUp, xiRJoyUp);
 MappableInputs.SetXInputMapping(miRJoyDown, xiRJoyDown);
 MappableInputs.SetXInputMapping(miRJoyLeft, xiRJoyLeft);
 MappableInputs.SetXInputMapping(miRJoyRight, xiRJoyRight);
 MappableInputs.SetXInputMapping(miDPadUp, xiDPadUp);
 MappableInputs.SetXInputMapping(miDPadDown, xiDPadDown);
 MappableInputs.SetXInputMapping(miDPadLeft, xiDPadLeft);
 MappableInputs.SetXInputMapping(miDPadRight, xiDPadRight);
 MappableInputs.SetXInputMapping(miCross, xiA);
 MappableInputs.SetXInputMapping(miCircle, xiB);
 MappableInputs.SetXInputMapping(miSquare, xiX);
 MappableInputs.SetXInputMapping(miTriangle, xiY);
 MappableInputs.SetXInputMapping(miShare, xiUnbound);
 MappableInputs.SetXInputMapping(miTouchPad, xiSelect);
 MappableInputs.SetXInputMapping(miOptions, xiStart);
 MappableInputs.SetXInputMapping(miL1, xiL1);
 MappableInputs.SetXInputMapping(miL2, xiL2);
 MappableInputs.SetXInputMapping(miL3, xiL3);
 MappableInputs.SetXInputMapping(miR1, xiR1);
 MappableInputs.SetXInputMapping(miR2, xiR2);
 MappableInputs.SetXInputMapping(miR3, xiR3);

 MappableInputs.LoadFromFile(XINPUT_CONFIG_FILE);
end.
