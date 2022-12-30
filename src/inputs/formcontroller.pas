unit formController;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
 Buttons, Interfaces, LResources, ComCtrls;

type

 { TFormControllers }

 TFormControllers = class(TForm)
  CheckEnabled: TCheckBox;
  GroupBox2: TGroupBox;
  Label26: TLabel;
  Label27: TLabel;
  Label28: TLabel;
  LJoyUp: TBitBtn;
  OpenDialog1: TOpenDialog;
  RJoyRight: TBitBtn;
  RJoyDown: TBitBtn;
  RJoyLeft: TBitBtn;
  dzLeft: TTrackBar;
  dzRight: TTrackBar;
  dzTriggers: TTrackBar;
  SaveDialog1: TSaveDialog;
  Triangle: TBitBtn;
  Circle: TBitBtn;
  Cross: TBitBtn;
  Square: TBitBtn;
  BitBtn17: TBitBtn;
  BitBtn18: TBitBtn;
  BitBtn19: TBitBtn;
  LJoyLeft: TBitBtn;
  L1: TBitBtn;
  L2: TBitBtn;
  L3: TBitBtn;
  R1: TBitBtn;
  R2: TBitBtn;
  R3: TBitBtn;
  Save: TBitBtn;
  Cancel: TBitBtn;
  SavePreset: TBitBtn;
  LoadPreset: TBitBtn;
  LJoyRight: TBitBtn;
  LJoyDown: TBitBtn;
  DPadUp: TBitBtn;
  DPadRight: TBitBtn;
  DPadDown: TBitBtn;
  DPadLeft: TBitBtn;
  RJoyUp: TBitBtn;
  GroupBox1: TGroupBox;
  GroupBox3: TGroupBox;
  GroupBox4: TGroupBox;
  GroupBox5: TGroupBox;
  GroupBox6: TGroupBox;
  Label1: TLabel;
  Label10: TLabel;
  Label11: TLabel;
  Label12: TLabel;
  Label13: TLabel;
  Label14: TLabel;
  Label15: TLabel;
  Label16: TLabel;
  Label17: TLabel;
  Label18: TLabel;
  Label19: TLabel;
  Label2: TLabel;
  Label20: TLabel;
  Label21: TLabel;
  Label22: TLabel;
  Label23: TLabel;
  Label24: TLabel;
  Label25: TLabel;
  Label3: TLabel;
  Label4: TLabel;
  Label5: TLabel;
  Label6: TLabel;
  Label7: TLabel;
  Label8: TLabel;
  Label9: TLabel;
  Shape1: TShape;
  Shape2: TShape;
  Shape3: TShape;
  Shape5: TShape;
  Shape6: TShape;
  Shape7: TShape;
  procedure CancelClick(Sender: TObject);
  procedure ChangeKeyBiding(Sender: TObject);
  procedure LoadPresetClick(Sender: TObject);
  procedure SaveConfig(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure SaveClick(Sender: TObject);
  procedure SavePresetClick(Sender: TObject);
 private
  ShouldSaveConfig: boolean;
  function FindControlByTag(control: TWinControl; pTag: integer): TWinControl;
 public

 end;

var
 FormControllers: TFormControllers;

implementation

{$R *.lfm}

uses formBindButton, uMappableInputs;

{ TFormControllers }

procedure TFormControllers.ChangeKeyBiding(Sender: TObject);
var
 keyToChange: integer;
begin
 if Sender is TBitBtn then
 begin
  keyToChange := TBitBtn(Sender).Tag;
  FormBindButtons := TFormBindButtons.Create(Self);
  Application.CreateForm(TFormBindButtons, FormBindButtons);
  FormBindButtons.ShowModal;

  if FormBindButtons.ModalResult = mrOk then
  begin
   MappableInputs.SetXInputMapping(EnumPS4Buttons(keyToChange), FormBindButtons.XInputKey);
   TBitBtn(Sender).Caption := MappableInputs.XInputButtonsNames[Ord(FormBindButtons.XInputKey)];
  end;

  FormBindButtons.Release;
  FormBindButtons.Free;
 end;
end;

procedure TFormControllers.LoadPresetClick(Sender: TObject);
begin
 //
 OpenDialog1.InitialDir:='config';
 if OpenDialog1.Execute then
 begin
   MappableInputs.LoadFromFile(OpenDialog1.FileName);
   ShowMessage('Config loaded from ' + OpenDialog1.FileName);
 end;
end;

procedure TFormControllers.SaveConfig(Sender: TObject);
begin
 if ShouldSaveConfig then
 begin
  MappableInputs.XInputEnabled := CheckEnabled.Checked;
  MappableInputs.XInputDeadzoneLeft := dzLeft.Position;
  MappableInputs.XInputDeadzoneRight := dzRight.Position;
  MappableInputs.XInputDeadzoneTrigger := dzTriggers.Position;
 end;
end;

procedure TFormControllers.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 MappableInputs.LoadFromFile(XINPUT_CONFIG_FILE);
 CloseAction := TCloseAction.caHide;
end;

procedure TFormControllers.CancelClick(Sender: TObject);
begin
 Self.Close;
end;

function TFormControllers.FindControlByTag(control: TWinControl; pTag: integer): TWinControl;
var
 i: integer;
begin
 Result := nil;
 for i := 0 to control.ControlCount - 1 do
 begin
  if control.Controls[i] is TWinControl then
  begin
   if control.Controls[i].Tag = pTag then
   begin
    Result := TWinControl(control.Controls[i]);
    exit;
   end;
   // recursively check children
   Result := FindControlByTag(TWinControl(control.Controls[i]), pTag);
   if Result <> nil then
    exit;
  end;
 end;
end;

procedure TFormControllers.FormCreate(Sender: TObject);
begin
end;

procedure TFormControllers.FormShow(Sender: TObject);
var
 i: integer;
 btn: TWinControl;
begin
 // Load mapped buttons into UI
 ShouldSaveConfig := False;
 MappableInputs.LoadFromFile(XINPUT_CONFIG_FILE);
 CheckEnabled.Checked := MappableInputs.XInputEnabled;

 dzTriggers.Max := 255;

 dzLeft.Max := smallint.MaxValue;
 dzRight.Max := smallint.MaxValue;

 dzLeft.Position := MappableInputs.XInputDeadzoneLeft;
 dzRight.Position := MappableInputs.XInputDeadzoneRight;
 dzTriggers.Position := MappableInputs.XInputDeadzoneTrigger;

 for i := 1 to NUM_PS4_BUTTONS do
 begin
  btn := FindControlByTag(Self, i);
  if btn <> nil then
  begin
   btn.Caption := MappableInputs.XInputButtonsNames[Ord(MappableInputs.PS4toXInput[i])];
  end;
 end;
 ShouldSaveConfig := True;
end;

procedure TFormControllers.SaveClick(Sender: TObject);
begin
 MappableInputs.SaveToFile(XINPUT_CONFIG_FILE);
 Self.Close;
end;

procedure TFormControllers.SavePresetClick(Sender: TObject);
begin
 //
 SaveDialog1.InitialDir:='config';
 if SaveDialog1.Execute then
 begin
   MappableInputs.SaveToFile(SaveDialog1.FileName);
   ShowMessage('Config saved to ' + SaveDialog1.FileName);
 end;
end;

end.
