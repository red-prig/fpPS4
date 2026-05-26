unit laz2ime;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 LCLType,
 LCLIntf,
 gui_dialog_fabric,
 md_time,
 ime_types,
 ps4_libSceImeDialog,
 ps4_libSceIme;

type
 TImeEventQueue=object
  rd_pos:Byte;
  wr_pos:Byte;
  data:array[0..7] of SceImeEvent;
  procedure Reset;
  procedure Push(const event:SceImeEvent);
  function  Pop (var event:SceImeEvent):Boolean;
  procedure PushOpen        (posx,posy:Single;width,height:DWORD);
  procedure PushUpdate      (caretIndex:DWORD);
  procedure PushPreedit     (caretIndex:DWORD);
  procedure PushChangeDevice(deviceType:SceImeDeviceType);
  procedure PushClose       ();
  procedure PushEnter       ();
  procedure PushKey         (key:Word;Chr:WideChar;down:Byte);
 end;

 TKeyStates=object
  ime_input :Boolean;
  Multiline :Boolean;
  ime_change:Boolean;
  kbd_input :Boolean;
  //
  input:WideString;
  //
  last_down_key  :Word;
  last_down_state:array[0..255] of Byte;
  last_down_chr  :array[0..255] of WideChar;
  //
  ime_queue:TImeEventQueue;
  //
  Procedure PushKey(key:Word;Chr:WideChar;down:Byte);
  //
  Procedure DoDown  (Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure DoPress (Sender: TObject; var UTF8Key: TUTF8Char);
  Procedure DoUp    (Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure DoChange(Sender: TObject);
  //
  Procedure ImeDlgOpen(Dialog:TImeDialog);
  Procedure ImeOpen   (Dialog:TImeDialog;const w:WideString);
 end;

implementation

procedure TImeEventQueue.Reset;
begin
 rd_pos:=0;
 wr_pos:=0;
end;

procedure TImeEventQueue.Push(const event:SceImeEvent);
begin
 data[wr_pos]:=event;

 wr_pos:=(wr_pos+1) mod Length(data);

 if (wr_pos=rd_pos) then
 begin
  rd_pos:=(rd_pos+1) mod Length(data);
 end;
end;

function TImeEventQueue.Pop(var event:SceImeEvent):Boolean;
begin
 if (wr_pos=rd_pos) then
 begin
  Result:=False;
 end else
 begin
  event:=data[rd_pos];

  rd_pos:=(rd_pos+1) mod Length(data);

  Result:=True;
 end;
end;

procedure TImeEventQueue.PushOpen(posx,posy:Single;width,height:DWORD);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_EVENT_OPEN;

 event.param.rect.x    :=posx;
 event.param.rect.y    :=posy;
 event.param.rect.width:=width;
 event.param.rect.height:=height;

 Push(event);
end;

procedure TImeEventQueue.PushUpdate(caretIndex:DWORD);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_EVENT_UPDATE_TEXT;

 event.param.text.caretIndex:=caretIndex;
 event.param.text.areaNum   :=1;
 event.param.text.textArea[0].mode:=SCE_IME_TEXT_AREA_MODE_EDIT;

 Push(event);
end;

procedure TImeEventQueue.PushPreedit(caretIndex:DWORD);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_EVENT_UPDATE_TEXT;

 event.param.text.caretIndex:=caretIndex;
 event.param.text.areaNum   :=2;
 event.param.text.textArea[0].mode:=SCE_IME_TEXT_AREA_MODE_EDIT;
 event.param.text.textArea[1].mode:=SCE_IME_TEXT_AREA_MODE_PREEDIT;
 event.param.text.textArea[1].index:=caretIndex;

 Push(event);
end;

procedure TImeEventQueue.PushChangeDevice(deviceType:SceImeDeviceType);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_EVENT_CHANGE_DEVICE;

 event.param.deviceType:=deviceType;

 Push(event);
end;

procedure TImeEventQueue.PushClose();
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_EVENT_PRESS_CLOSE;

 Push(event);
end;

procedure TImeEventQueue.PushEnter();
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_EVENT_PRESS_ENTER;

 Push(event);
end;

function vk_to_hid(key:Word):Word;
begin
 Result:=0;

 case key of

  VK_CANCEL             :Result:=SCE_IME_KEYCODE_CANCEL;

  VK_BACK               :Result:=SCE_IME_KEYCODE_BACKSPACE;

  VK_TAB                :Result:=SCE_IME_KEYCODE_TAB;
  VK_CLEAR              :Result:=SCE_IME_KEYCODE_CLEAR;
  VK_RETURN             :Result:=SCE_IME_KEYCODE_RETURN;

  VK_PAUSE              :Result:=SCE_IME_KEYCODE_PAUSE;
  VK_CAPITAL            :Result:=SCE_IME_KEYCODE_CAPSLOCK;

  VK_KANA               :Result:=SCE_IME_KEYCODE_LANG1;
  VK_JUNJA              :Result:=SCE_IME_KEYCODE_LANG2;
  VK_FINAL              :Result:=SCE_IME_KEYCODE_LANG3;
  VK_HANJA              :Result:=SCE_IME_KEYCODE_LANG4;

  VK_ESCAPE             :Result:=SCE_IME_KEYCODE_ESCAPE;
  VK_SPACE              :Result:=SCE_IME_KEYCODE_SPACEBAR;
  VK_PRIOR              :Result:=SCE_IME_KEYCODE_PAGEUP;
  VK_NEXT               :Result:=SCE_IME_KEYCODE_PAGEDOWN;
  VK_END                :Result:=SCE_IME_KEYCODE_END;
  VK_HOME               :Result:=SCE_IME_KEYCODE_HOME;
  VK_LEFT               :Result:=SCE_IME_KEYCODE_LEFTARROW;
  VK_UP                 :Result:=SCE_IME_KEYCODE_UPARROW;
  VK_RIGHT              :Result:=SCE_IME_KEYCODE_RIGHTARROW;
  VK_DOWN               :Result:=SCE_IME_KEYCODE_DOWNARROW;
  VK_SELECT             :Result:=SCE_IME_KEYCODE_SELECT;
  VK_PRINT              :Result:=SCE_IME_KEYCODE_PRINTSCREEN;
  VK_EXECUTE            :Result:=SCE_IME_KEYCODE_EXECUTE;
  VK_SNAPSHOT           :Result:=SCE_IME_KEYCODE_PRINTSCREEN;
  VK_INSERT             :Result:=SCE_IME_KEYCODE_INSERT;
  VK_DELETE             :Result:=SCE_IME_KEYCODE_DELETE;
  VK_HELP               :Result:=SCE_IME_KEYCODE_HELP;

  VK_0                  :Result:=SCE_IME_KEYCODE_0;
  VK_1..VK_9            :Result:=key-VK_1+SCE_IME_KEYCODE_1;
  VK_A..VK_Z            :Result:=key-VK_A+SCE_IME_KEYCODE_A;

  VK_LWIN               :Result:=SCE_IME_KEYCODE_LEFTGUI;
  VK_RWIN               :Result:=SCE_IME_KEYCODE_RIGHTGUI;
  VK_APPS               :Result:=SCE_IME_KEYCODE_APPLICATION;
  VK_SLEEP              :Result:=SCE_IME_KEYCODE_POWER;

  VK_NUMPAD0            :Result:=SCE_IME_KEYCODE_KEYPAD_0;
  VK_NUMPAD1..VK_NUMPAD9:Result:=key-VK_NUMPAD1+SCE_IME_KEYCODE_KEYPAD_1;

  VK_MULTIPLY           :Result:=SCE_IME_KEYCODE_KEYPAD_ASTERISK;
  VK_ADD                :Result:=SCE_IME_KEYCODE_KEYPAD_PLUS;
  VK_SEPARATOR          :Result:=SCE_IME_KEYCODE_KEYPAD_PERIOD;
  VK_SUBTRACT           :Result:=SCE_IME_KEYCODE_KEYPAD_MINUS;
  VK_DECIMAL            :Result:=SCE_IME_KEYCODE_KEYPAD_DECIMAL;
  VK_DIVIDE             :Result:=SCE_IME_KEYCODE_KEYPAD_SLASH;

  VK_F1..VK_F12         :Result:=key-VK_F1+SCE_IME_KEYCODE_F1;
  VK_F13..VK_F24        :Result:=key-VK_F13+SCE_IME_KEYCODE_F13;

  VK_NUMLOCK            :Result:=SCE_IME_KEYCODE_KEYPAD_NUMLOCK;
  VK_SCROLL             :Result:=SCE_IME_KEYCODE_SCROLLLOCK;

  186                   :Result:=SCE_IME_KEYCODE_SEMICOLON;
  187                   :Result:=SCE_IME_KEYCODE_EQUAL;
  188                   :Result:=SCE_IME_KEYCODE_COMMA;
  189                   :Result:=SCE_IME_KEYCODE_MINUS;
  190                   :Result:=SCE_IME_KEYCODE_PERIOD;
  191                   :Result:=SCE_IME_KEYCODE_SLASH;
  192                   :Result:=SCE_IME_KEYCODE_BACKQUOTE;
  219                   :Result:=SCE_IME_KEYCODE_LEFTBRACKET;
  220                   :Result:=SCE_IME_KEYCODE_BACKSLASH;
  221                   :Result:=SCE_IME_KEYCODE_RIGHTBRACKET;
  222                   :Result:=SCE_IME_KEYCODE_SINGLEQUOTE;
  223                   :Result:=SCE_IME_KEYCODE_NONUS_POUND;

  VK_LSHIFT             :Result:=SCE_IME_KEYCODE_LEFTSHIFT;
  VK_RSHIFT             :Result:=SCE_IME_KEYCODE_RIGHTSHIFT;
  VK_LCONTROL           :Result:=SCE_IME_KEYCODE_LEFTCONTROL;
  VK_RCONTROL           :Result:=SCE_IME_KEYCODE_RIGHTCONTROL;
  VK_LMENU              :Result:=SCE_IME_KEYCODE_LEFTALT;
  VK_RMENU              :Result:=SCE_IME_KEYCODE_RIGHTALT;

  VK_VOLUME_MUTE        :Result:=SCE_IME_KEYCODE_MUTE;
  VK_VOLUME_DOWN        :Result:=SCE_IME_KEYCODE_VOLUMEDOWN;
  VK_VOLUME_UP          :Result:=SCE_IME_KEYCODE_VOLUMEUP;

  VK_OEM_102            :Result:=SCE_IME_KEYCODE_NONUS_BACKSLASH;

  VK_ATTN               :Result:=SCE_IME_KEYCODE_SYSREQ;
  VK_CRSEL              :Result:=SCE_IME_KEYCODE_CRSEL_PROPS;
  VK_EXSEL              :Result:=SCE_IME_KEYCODE_EXSEL;
  VK_OEM_CLEAR          :Result:=SCE_IME_KEYCODE_CLEAR;

 end;
end;

procedure TImeEventQueue.PushKey(key:Word;Chr:WideChar;down:Byte);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);

 case down of
  0:event.id:=SCE_IME_KEYBOARD_EVENT_KEYCODE_DOWN;
  1:event.id:=SCE_IME_KEYBOARD_EVENT_KEYCODE_REPEAT;
  2:event.id:=SCE_IME_KEYBOARD_EVENT_KEYCODE_UP;
 end;

 event.param.keycode.keycode  :=vk_to_hid(key);
 event.param.keycode.character:=Chr;

 if (event.param.keycode.keycode<>0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_KEYCODE_VALID;
 end;

 if (Chr<>#0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_CHARACTER_VALID;
 end;

 if (GetKeyState(VK_LMENU)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_L_ALT;
 end;

 if (GetKeyState(VK_RMENU)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_R_ALT;
 end;

 if (GetKeyState(VK_LCONTROL)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_L_CTRL;
 end;

 if (GetKeyState(VK_RCONTROL)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_R_CTRL;
 end;

 if (GetKeyState(VK_LSHIFT)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_L_SHIFT;
 end;

 if (GetKeyState(VK_RSHIFT)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_R_SHIFT;
 end;

 if (GetKeyState(VK_LWIN)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_L_GUI;
 end;

 if (GetKeyState(VK_RWIN)<0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_MODIFIER_R_GUI;
 end;

 if ((GetKeyState(VK_NUMLOCK) and 1)<>0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_LED_NUM_LOCK;
 end;

 if ((GetKeyState(VK_CAPITAL) and 1)<>0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_LED_CAPS_LOCK;
 end;

 if ((GetKeyState(VK_SCROLL) and 1)<>0) then
 begin
  event.param.keycode.status:=event.param.keycode.status or SCE_IME_KEYCODE_STATE_LED_SCROLL_LOCK;
 end;

 event.param.keycode.ktype     :=SCE_IME_KEYBOARD_TYPE_ENGLISH_US; //TODO: keyboard lang
 event.param.keycode.userId    :=-1;
 event.param.keycode.resourceId:=1;
 event.param.keycode.timestamp :=GetRtcTick;

 Push(event);
end;

Procedure TKeyStates.PushKey(key:Word;Chr:WideChar;down:Byte);
begin

 if ime_input and ((down=0) or (down=1)) then
 if (key<>VK_RETURN) or Multiline then
 begin
  ime_queue.PushKey(key,Chr,down);
 end;

end;

procedure TKeyStates.DoChange(Sender:TObject);
begin
 ime_change:=true;
end;

Procedure TKeyStates.ImeDlgOpen(Dialog:TImeDialog);
begin
 ime_change:=False;

 Dialog.FMsgMemo.OnChange:=@DoChange;
 Dialog.FMsgMemo.SetFocus;
end;

Procedure TKeyStates.ImeOpen(Dialog:TImeDialog;const w:WideString);
begin
 ime_input:=True;
 ime_queue.Reset;

 Multiline:=Dialog.Multiline;

 input:=w;

 ime_queue.PushOpen(
  Dialog.GetPosX,
  Dialog.GetPosY,
  Trunc(Dialog.Fwidth ),
  Trunc(Dialog.Fheight)
 );

 ime_queue.PushUpdate      (Length(input));
 ime_queue.PushChangeDevice(SCE_IME_DEVICE_TYPE_CONTROLLER);

 Dialog.FMsgMemo.OnUTF8KeyPress:=@DoPress;
 Dialog.FMsgMemo.OnKeyDown     :=@DoDown;
 Dialog.FMsgMemo.OnKeyUp       :=@DoUp;
 Dialog.FMsgMemo.WantTabs      :=True;
 Dialog.FMsgMemo.ReadOnly      :=True;
 Dialog.FMsgMemo.SetFocus;
end;

function IsNotPrintable(Key:Word):Boolean; inline;
begin
 case Key of
  VK_TAB,
  VK_SHIFT,
  VK_CONTROL,
  VK_MENU,
  VK_PAUSE,
  VK_CAPITAL,
  VK_NUMLOCK,
  VK_SCROLL,
  VK_LSHIFT,
  VK_RSHIFT,
  VK_LCONTROL,
  VK_RCONTROL,
  VK_LMENU,
  VK_RMENU,
  VK_LWIN,
  VK_RWIN,
  VK_APPS,
  VK_CLEAR,
  VK_CANCEL,

  VK_F1..VK_F24,

  VK_PRIOR..VK_HELP,

  VK_BROWSER_BACK..VK_LAUNCH_APP2:
    Result:=True;

  else
    Result:=False;
 end;
end;

function GetSpecialKeyDown(Key:Word):Word; inline;
begin
 Result:=Key*ord(GetKeyState(Key)<0);
end;

Procedure TKeyStates.DoDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
 keys:array[0..1] of Word;

 procedure DoKeyDown(key:Word); inline;
 begin
  if (last_down_state[key]=0) then
  begin
   last_down_state[key]:=1;
   PushKey(key,#0,0);
  end else
  begin
   last_down_state[key]:=2;
   PushKey(key,#0,1);
  end;
 end;

begin
 if IsNotPrintable(Key) then
 begin

  keys[0]:=0;
  keys[1]:=0;

  case Key of
   VK_SHIFT:
    begin
     keys[0]:=GetSpecialKeyDown(VK_LSHIFT);
     keys[1]:=GetSpecialKeyDown(VK_RSHIFT);
    end;
   VK_CONTROL:
    begin
     keys[0]:=GetSpecialKeyDown(VK_LCONTROL);
     keys[1]:=GetSpecialKeyDown(VK_RCONTROL);
    end;
   VK_MENU:
    begin
     keys[0]:=GetSpecialKeyDown(VK_LMENU);
     keys[1]:=GetSpecialKeyDown(VK_RMENU);
    end;
   else
    keys[0]:=Key;
  end;


  if (keys[0]<>0) then DoKeyDown(keys[0]);
  if (keys[1]<>0) then DoKeyDown(keys[1]);

  last_down_key:=0;
 end else
 begin

  if (last_down_state[key]=0) then
  begin
   last_down_state[key]:=1;
  end else
  begin
   last_down_state[key]:=2;
  end;

  last_down_key:=key;
 end;

end;

procedure TKeyStates.DoPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
 chr:array[0..1] of WideChar;
begin
 if (last_down_key<>0) then
 begin
  chr[0]:=#0;
  chr[1]:=#0;
  Utf8ToUnicode(@chr,2,@UTF8Key[1],length(UTF8Key));

  case last_down_state[last_down_key] of
   1:PushKey(last_down_key,chr[0],0);
   2:PushKey(last_down_key,chr[0],1);
  end;

  last_down_chr[last_down_key]:=chr[0];
  last_down_key :=0;
 end;
end;


function GetSpecialKeyUp(Key:Word):Word; inline;
begin
 Result:=Key*ord(GetKeyState(Key)>=0);
end;

Procedure TKeyStates.DoUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
 keys:array[0..1] of Word;

 procedure DoKeyUp(key:Word); inline;
 begin
  if (last_down_state[key]<>0) then
  begin
   PushKey(key,last_down_chr[Key],2);
   last_down_state[key]:=0;
   last_down_chr  [Key]:=#0;
  end;
 end;

begin
 if IsNotPrintable(Key) then
 begin

  keys[0]:=0;
  keys[1]:=0;

  case Key of
   VK_SHIFT:
    begin
     keys[0]:=GetSpecialKeyUp(VK_LSHIFT);
     keys[1]:=GetSpecialKeyUp(VK_RSHIFT);
    end;
   VK_CONTROL:
    begin
     keys[0]:=GetSpecialKeyUp(VK_LCONTROL);
     keys[1]:=GetSpecialKeyUp(VK_RCONTROL);
    end;
   VK_MENU:
    begin
     keys[0]:=GetSpecialKeyUp(VK_LMENU);
     keys[1]:=GetSpecialKeyUp(VK_RMENU);
    end;
   else
    keys[0]:=Key;
  end;

  if (keys[0]<>0) then DoKeyUp(keys[0]);
  if (keys[1]<>0) then DoKeyUp(keys[1]);
 end else
 if (last_down_chr[Key]<>#0) then
 begin
  DoKeyUp(key);
 end;
end;

end.

