unit formBindButton;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
 ExtCtrls, uMappableInputs, Windows;

type

 { TFormBindButtons }

 TFormBindButtons = class(TForm)
  BitBtn1: TBitBtn;
  BitBtn2: TBitBtn;
  CheckInput: TTimer;
  Label1: TLabel;
  procedure BitBtn1Click(Sender: TObject);
  procedure BitBtn2Click(Sender: TObject);
  procedure CheckInputTimer(Sender: TObject);
  procedure FormShow(Sender: TObject);
 private

 public
  XInputKey: EnumXInputButtons;
  ResetToDefault: boolean;
 end;

var
 FormBindButtons: TFormBindButtons;

implementation

uses XInput;

{$R *.lfm}

{ TFormBindButtons }

procedure TFormBindButtons.CheckInputTimer(Sender: TObject);
var
 i: integer;
 state: TXInputState;
 key: EnumXInputButtons;
begin
 ZeroMemory(@state, SizeOf(TXInputState));
 if XInputGetState(0, state) = ERROR_SUCCESS then
 begin
  key := MappableInputs.XInputStateToKey(state);
  if key <> EnumXInputButtons.xiUnbound then
  begin
   XInputKey := key;
   ModalResult := mrOk;
  end;
 end;
end;

procedure TFormBindButtons.BitBtn2Click(Sender: TObject);
begin
 XInputKey := xiUnbound;
 ModalResult := mrOk;
end;

procedure TFormBindButtons.BitBtn1Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TFormBindButtons.FormShow(Sender: TObject);
begin
 XInputKey := xiUnbound;
end;

end.
