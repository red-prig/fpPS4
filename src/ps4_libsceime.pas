unit ps4_libSceIme;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  windows,
  kern_proc,
  subr_dynlib,
  syscalls,
  time,
  atomic,
  mpmc_queue,
  ime_types,
  Classes,
  SysUtils,
  ps4_libSceUserService;

const
 //USB HID USAGE ID
 SCE_IME_KEYCODE_NOEVENT                 =$0000; // No Event
 SCE_IME_KEYCODE_ERRORROLLOVER           =$0001; // Error Rollover
 SCE_IME_KEYCODE_POSTFAIL                =$0002; // Pos Fail
 SCE_IME_KEYCODE_ERRORUNDEFINED          =$0003; // Error Undefined
 SCE_IME_KEYCODE_A                       =$0004; // a and A
 SCE_IME_KEYCODE_B                       =$0005; // b and B
 SCE_IME_KEYCODE_C                       =$0006; // c and C
 SCE_IME_KEYCODE_D                       =$0007; // d and D
 SCE_IME_KEYCODE_E                       =$0008; // e and E
 SCE_IME_KEYCODE_F                       =$0009; // f and F
 SCE_IME_KEYCODE_G                       =$000A; // g and G
 SCE_IME_KEYCODE_H                       =$000B; // h and H
 SCE_IME_KEYCODE_I                       =$000C; // i and I
 SCE_IME_KEYCODE_J                       =$000D; // j and J
 SCE_IME_KEYCODE_K                       =$000E; // k and K
 SCE_IME_KEYCODE_L                       =$000F; // l and L
 SCE_IME_KEYCODE_M                       =$0010; // m and M
 SCE_IME_KEYCODE_N                       =$0011; // n and N
 SCE_IME_KEYCODE_O                       =$0012; // o and O
 SCE_IME_KEYCODE_P                       =$0013; // p and P
 SCE_IME_KEYCODE_Q                       =$0014; // q and Q
 SCE_IME_KEYCODE_R                       =$0015; // r and R
 SCE_IME_KEYCODE_S                       =$0016; // s and S
 SCE_IME_KEYCODE_T                       =$0017; // t and T
 SCE_IME_KEYCODE_U                       =$0018; // u and U
 SCE_IME_KEYCODE_V                       =$0019; // v and V
 SCE_IME_KEYCODE_W                       =$001A; // w and W
 SCE_IME_KEYCODE_X                       =$001B; // x and X
 SCE_IME_KEYCODE_Y                       =$001C; // y and Y
 SCE_IME_KEYCODE_Z                       =$001D; // z and Z
 SCE_IME_KEYCODE_1                       =$001E; // 1 and !
 SCE_IME_KEYCODE_2                       =$001F; // 2 and @
 SCE_IME_KEYCODE_3                       =$0020; // 3 and #
 SCE_IME_KEYCODE_4                       =$0021; // 4 and $
 SCE_IME_KEYCODE_5                       =$0022; // 5 and %
 SCE_IME_KEYCODE_6                       =$0023; // 6 and ^
 SCE_IME_KEYCODE_7                       =$0024; // 7 and &
 SCE_IME_KEYCODE_8                       =$0025; // 8 and *
 SCE_IME_KEYCODE_9                       =$0026; // 9 and (
 SCE_IME_KEYCODE_0                       =$0027; // 0 and )
 SCE_IME_KEYCODE_RETURN                  =$0028; // Return (Enter)
 SCE_IME_KEYCODE_ESCAPE                  =$0029; // Escape
 SCE_IME_KEYCODE_BACKSPACE               =$002A; // Delete and Backspace
 SCE_IME_KEYCODE_TAB                     =$002B; // Tab
 SCE_IME_KEYCODE_SPACEBAR                =$002C; // Spacebar
 SCE_IME_KEYCODE_MINUS                   =$002D; // - and _
 SCE_IME_KEYCODE_EQUAL                   =$002E; // = and +
 SCE_IME_KEYCODE_LEFTBRACKET             =$002F; // [ and {
 SCE_IME_KEYCODE_RIGHTBRACKET            =$0030; // ] and }
 SCE_IME_KEYCODE_BACKSLASH               =$0031; // \ and |
 SCE_IME_KEYCODE_NONUS_POUND             =$0032; // Non US # and ~
 SCE_IME_KEYCODE_SEMICOLON               =$0033; // ; and :
 SCE_IME_KEYCODE_SINGLEQUOTE             =$0034; // ' and "
 SCE_IME_KEYCODE_BACKQUOTE               =$0035; // ` and ~
 SCE_IME_KEYCODE_COMMA                   =$0036; // , and <
 SCE_IME_KEYCODE_PERIOD                  =$0037; // . and >
 SCE_IME_KEYCODE_SLASH                   =$0038; // / and ?
 SCE_IME_KEYCODE_CAPSLOCK                =$0039; // Caps Lock
 SCE_IME_KEYCODE_F1                      =$003A; // F1
 SCE_IME_KEYCODE_F2                      =$003B; // F2
 SCE_IME_KEYCODE_F3                      =$003C; // F3
 SCE_IME_KEYCODE_F4                      =$003D; // F4
 SCE_IME_KEYCODE_F5                      =$003E; // F5
 SCE_IME_KEYCODE_F6                      =$003F; // F6
 SCE_IME_KEYCODE_F7                      =$0040; // F7
 SCE_IME_KEYCODE_F8                      =$0041; // F8
 SCE_IME_KEYCODE_F9                      =$0042; // F9
 SCE_IME_KEYCODE_F10                     =$0043; // F10
 SCE_IME_KEYCODE_F11                     =$0044; // F11
 SCE_IME_KEYCODE_F12                     =$0045; // F12
 SCE_IME_KEYCODE_PRINTSCREEN             =$0046; // Print Screen
 SCE_IME_KEYCODE_SCROLLLOCK              =$0047; // Scroll Lock
 SCE_IME_KEYCODE_PAUSE                   =$0048; // Break
 SCE_IME_KEYCODE_INSERT                  =$0049; // Insert
 SCE_IME_KEYCODE_HOME                    =$004A; // Home
 SCE_IME_KEYCODE_PAGEUP                  =$004B; // Page Up
 SCE_IME_KEYCODE_DELETE                  =$004C; // Delete Forward
 SCE_IME_KEYCODE_END                     =$004D; // End
 SCE_IME_KEYCODE_PAGEDOWN                =$004E; // Page Down
 SCE_IME_KEYCODE_RIGHTARROW              =$004F; // RIght Arrow
 SCE_IME_KEYCODE_LEFTARROW               =$0050; // Left Arrow
 SCE_IME_KEYCODE_DOWNARROW               =$0051; // Down Arrow
 SCE_IME_KEYCODE_UPARROW                 =$0052; // Up Arrow
 SCE_IME_KEYCODE_KEYPAD_NUMLOCK          =$0053; // NumLock and Clear
 SCE_IME_KEYCODE_KEYPAD_SLASH            =$0054; // Keypad /
 SCE_IME_KEYCODE_KEYPAD_ASTERISK         =$0055; // Keypad *
 SCE_IME_KEYCODE_KEYPAD_MINUS            =$0056; // Keypad -
 SCE_IME_KEYCODE_KEYPAD_PLUS             =$0057; // Keypad +
 SCE_IME_KEYCODE_KEYPAD_ENTER            =$0058; // Keypad Enter
 SCE_IME_KEYCODE_KEYPAD_1                =$0059; // Keypad 1 and End
 SCE_IME_KEYCODE_KEYPAD_2                =$005A; // Keypad 2 and Down Arrow
 SCE_IME_KEYCODE_KEYPAD_3                =$005B; // Keypad 3 and Page Down
 SCE_IME_KEYCODE_KEYPAD_4                =$005C; // Keypad 4 and Left Arrow
 SCE_IME_KEYCODE_KEYPAD_5                =$005D; // Keypad 5
 SCE_IME_KEYCODE_KEYPAD_6                =$005E; // Keypad 6 and Right Arrow
 SCE_IME_KEYCODE_KEYPAD_7                =$005F; // Keypad 7 and Home
 SCE_IME_KEYCODE_KEYPAD_8                =$0060; // Keypad 8 and Up Arrow
 SCE_IME_KEYCODE_KEYPAD_9                =$0061; // Keypad 9 and Page Up
 SCE_IME_KEYCODE_KEYPAD_0                =$0062; // Keypad 0 and Insert
 SCE_IME_KEYCODE_KEYPAD_PERIOD           =$0063; // Keypad . and Delete
 SCE_IME_KEYCODE_NONUS_BACKSLASH         =$0064; // Non US \ and |
 SCE_IME_KEYCODE_APPLICATION             =$0065; // Application
 SCE_IME_KEYCODE_POWER                   =$0066; // Power
 SCE_IME_KEYCODE_KEYPAD_EQUAL            =$0067; // Keypad =
 SCE_IME_KEYCODE_F13                     =$0068; // F13
 SCE_IME_KEYCODE_F14                     =$0069; // F14
 SCE_IME_KEYCODE_F15                     =$006A; // F15
 SCE_IME_KEYCODE_F16                     =$006B; // F16
 SCE_IME_KEYCODE_F17                     =$006C; // F17
 SCE_IME_KEYCODE_F18                     =$006D; // F18
 SCE_IME_KEYCODE_F19                     =$006E; // F19
 SCE_IME_KEYCODE_F20                     =$006F; // F20
 SCE_IME_KEYCODE_F21                     =$0070; // F21
 SCE_IME_KEYCODE_F22                     =$0071; // F22
 SCE_IME_KEYCODE_F23                     =$0072; // F23
 SCE_IME_KEYCODE_F24                     =$0073; // F24
 SCE_IME_KEYCODE_EXECUTE                 =$0074; // Execute
 SCE_IME_KEYCODE_HELP                    =$0075; // Help
 SCE_IME_KEYCODE_MENU                    =$0076; // Menu
 SCE_IME_KEYCODE_SELECT                  =$0077; // Select
 SCE_IME_KEYCODE_STOP                    =$0078; // Stop
 SCE_IME_KEYCODE_AGAIN                   =$0079; // Again
 SCE_IME_KEYCODE_UNDO                    =$007A; // Undo
 SCE_IME_KEYCODE_CUT                     =$007B; // Cut
 SCE_IME_KEYCODE_COPY                    =$007C; // Copy
 SCE_IME_KEYCODE_PASTE                   =$007D; // Paste
 SCE_IME_KEYCODE_FIND                    =$007E; // Find
 SCE_IME_KEYCODE_MUTE                    =$007F; // Mute
 SCE_IME_KEYCODE_VOLUMEUP                =$0080; // Volume Up
 SCE_IME_KEYCODE_VOLUMEDOWN              =$0081; // Volume Down
 SCE_IME_KEYCODE_LOCKING_CAPSLOCK        =$0082; // Locking Caps Lock
 SCE_IME_KEYCODE_LOCKING_NUMLOCK         =$0083; // Locking Num Lock
 SCE_IME_KEYCODE_LOCKING_SCROLLLOCK      =$0084; // Locking Scroll Lock
 SCE_IME_KEYCODE_KEYPAD_COMMA            =$0085; // Keypad ,
 SCE_IME_KEYCODE_KEYPAD_EQUALSIGN        =$0086; // Keypad Equal Sign
 SCE_IME_KEYCODE_INTERNATIONAL1          =$0087; // International 1
 SCE_IME_KEYCODE_INTERNATIONAL2          =$0088; // International 2
 SCE_IME_KEYCODE_INTERNATIONAL3          =$0089; // International 3
 SCE_IME_KEYCODE_INTERNATIONAL4          =$008A; // International 4
 SCE_IME_KEYCODE_INTERNATIONAL5          =$008B; // International 5
 SCE_IME_KEYCODE_INTERNATIONAL6          =$008C; // International 6
 SCE_IME_KEYCODE_INTERNATIONAL7          =$008D; // International 7
 SCE_IME_KEYCODE_INTERNATIONAL8          =$008E; // International 8
 SCE_IME_KEYCODE_INTERNATIONAL9          =$008F; // International 9
 SCE_IME_KEYCODE_LANG1                   =$0090; // Language 1
 SCE_IME_KEYCODE_LANG2                   =$0091; // Language 2
 SCE_IME_KEYCODE_LANG3                   =$0092; // Language 3
 SCE_IME_KEYCODE_LANG4                   =$0093; // Language 4
 SCE_IME_KEYCODE_LANG5                   =$0094; // Language 5
 SCE_IME_KEYCODE_LANG6                   =$0095; // Language 6
 SCE_IME_KEYCODE_LANG7                   =$0096; // Language 7
 SCE_IME_KEYCODE_LANG8                   =$0097; // Language 8
 SCE_IME_KEYCODE_LANG9                   =$0098; // Language 9
 SCE_IME_KEYCODE_ALTERASE                =$0099; // Alternate Erase
 SCE_IME_KEYCODE_SYSREQ                  =$009A; // SysReq / Attention
 SCE_IME_KEYCODE_CANCEL                  =$009B; // Cancel
 SCE_IME_KEYCODE_CLEAR                   =$009C; // Clear
 SCE_IME_KEYCODE_PRIOR                   =$009D; // Prior
 SCE_IME_KEYCODE_RETURN2                 =$009E; // Return
 SCE_IME_KEYCODE_SEPARATOR               =$009F; // Separator
 SCE_IME_KEYCODE_OUT                     =$00A0; // Out
 SCE_IME_KEYCODE_OPER                    =$00A1; // Oper
 SCE_IME_KEYCODE_CLEAR_AGAIN             =$00A2; // Clear/Again
 SCE_IME_KEYCODE_CRSEL_PROPS             =$00A3; // CrSel/Props
 SCE_IME_KEYCODE_EXSEL                   =$00A4; // ExSel
 SCE_IME_KEYCODE_KEYPAD_00               =$00B0; // Keypad 00
 SCE_IME_KEYCODE_KEYPAD_000              =$00B1; // Keypad 000
 SCE_IME_KEYCODE_THOUSANDSSEPARATOR      =$00B2; // Thousands Separator
 SCE_IME_KEYCODE_DECIMALSEPARATOR        =$00B3; // Decimal Separator
 SCE_IME_KEYCODE_CURRENCYUNIT            =$00B4; // Currency Unit
 SCE_IME_KEYCODE_CURRENCYSUBUNIT         =$00B5; // Currency Sub-Unit
 SCE_IME_KEYCODE_KEYPAD_LEFTPARENTHESIS  =$00B6; // Keypad (
 SCE_IME_KEYCODE_KEYPAD_RIGHTPARENTHESIS =$00B7; // Keypad )
 SCE_IME_KEYCODE_KEYPAD_LEFTCURLYBRACKET =$00B8; // Keypad {
 SCE_IME_KEYCODE_KEYPAD_RIGHTCURLYBRACKET=$00B9; // Keypad }
 SCE_IME_KEYCODE_KEYPAD_TAB              =$00BA; // Keypad Tab
 SCE_IME_KEYCODE_KEYPAD_BACKSPACE        =$00BB; // Keypad BackSpace
 SCE_IME_KEYCODE_KEYPAD_A                =$00BC; // Keypad A
 SCE_IME_KEYCODE_KEYPAD_B                =$00BD; // Keypad B
 SCE_IME_KEYCODE_KEYPAD_C                =$00BE; // Keypad C
 SCE_IME_KEYCODE_KEYPAD_D                =$00BF; // Keypad D
 SCE_IME_KEYCODE_KEYPAD_E                =$00C0; // Keypad E
 SCE_IME_KEYCODE_KEYPAD_F                =$00C1; // Keypad F
 SCE_IME_KEYCODE_KEYPAD_XOR              =$00C2; // Keypad XOR
 SCE_IME_KEYCODE_KEYPAD_HAT              =$00C3; // Keypad ^
 SCE_IME_KEYCODE_KEYPAD_PERCENT          =$00C4; // Keypad %
 SCE_IME_KEYCODE_KEYPAD_LESSTHAN         =$00C5; // Keypad <
 SCE_IME_KEYCODE_KEYPAD_GREATERTHAN      =$00C6; // Keypad >
 SCE_IME_KEYCODE_KEYPAD_AND              =$00C7; // Keypad &
 SCE_IME_KEYCODE_KEYPAD_LOGICALAND       =$00C8; // Keypad &&
 SCE_IME_KEYCODE_KEYPAD_OR               =$00C9; // Keypad |
 SCE_IME_KEYCODE_KEYPAD_LOGICALOR        =$00CA; // Keypad ||
 SCE_IME_KEYCODE_KEYPAD_COLON            =$00CB; // Keypad :
 SCE_IME_KEYCODE_KEYPAD_NUMBER           =$00CC; // Keypad #
 SCE_IME_KEYCODE_KEYPAD_SPACE            =$00CD; // Keypad Space
 SCE_IME_KEYCODE_KEYPAD_ATSIGN           =$00CE; // Keypad @
 SCE_IME_KEYCODE_KEYPAD_EXCLAMATION      =$00CF; // Keypad !
 SCE_IME_KEYCODE_KEYPAD_MEMORY_STORE     =$00D0; // Keypad Memory Store
 SCE_IME_KEYCODE_KEYPAD_MEMORY_RECALL    =$00D1; // Keypad Memory Recall
 SCE_IME_KEYCODE_KEYPAD_MEMORY_CLEAR     =$00D2; // Keypad Memory Clear
 SCE_IME_KEYCODE_KEYPAD_MEMORY_ADD       =$00D3; // Keypad Memory Add
 SCE_IME_KEYCODE_KEYPAD_MEMORY_SUBTRACT  =$00D4; // Keypad Memory Subtract
 SCE_IME_KEYCODE_KEYPAD_MEMORY_MULTIPLY  =$00D5; // Keypad Memory Multiply
 SCE_IME_KEYCODE_KEYPAD_MEMORY_DIVIDE    =$00D6; // Keypad Memory Divide
 SCE_IME_KEYCODE_KEYPAD_PLUS_MINUS       =$00D7; // Keypad +/-
 SCE_IME_KEYCODE_KEYPAD_CLEAR            =$00D8; // Keypad Clear
 SCE_IME_KEYCODE_KEYPAD_CLEARENTRY       =$00D9; // Keypad Clear Entry
 SCE_IME_KEYCODE_KEYPAD_BINARY           =$00DA; // Keypad Binary
 SCE_IME_KEYCODE_KEYPAD_OCTAL            =$00DB; // Keypad Octal
 SCE_IME_KEYCODE_KEYPAD_DECIMAL          =$00DC; // Keypad Decimal
 SCE_IME_KEYCODE_KEYPAD_HEXADECIMAL      =$00DD; // Keypad Hexadecimal
 SCE_IME_KEYCODE_LEFTCONTROL             =$00E0; // Keypad Left Control
 SCE_IME_KEYCODE_LEFTSHIFT               =$00E1; // Keypad Left Shift
 SCE_IME_KEYCODE_LEFTALT                 =$00E2; // Keypad Left Alt
 SCE_IME_KEYCODE_LEFTGUI                 =$00E3; // Keypad Left Gui
 SCE_IME_KEYCODE_RIGHTCONTROL            =$00E4; // Keypad Right Control
 SCE_IME_KEYCODE_RIGHTSHIFT              =$00E5; // Keypad Right Shift
 SCE_IME_KEYCODE_RIGHTALT                =$00E6; // Keypad Right Alt
 SCE_IME_KEYCODE_RIGHTGUI                =$00E7; // Keypad Right Gui

implementation

type
 t_ime_event_queue=specialize mpmc_bounded_queue<SceImeEvent>;

var
 g_ime_event_queue:t_ime_event_queue;

 keyboard_init:QWORD=0;

 g_hook:HHOOK=0;

 g_handler:SceImeEventHandler=nil;
 g_cb_arg:Pointer=nil;

Procedure push_keyboard_open(userId:SceUserServiceUserId);
var
 event,tmp:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_KEYBOARD_EVENT_OPEN;
 event.param.resourceIdArray.userId:=userId;
 event.param.resourceIdArray.resourceId[0]:=1;

 while not g_ime_event_queue.enqueue(event) do
 begin
  g_ime_event_queue.dequeue(tmp); //drop first
 end;
end;

Procedure push_keyboard_code(id:Integer;var keycode:SceImeKeycode);
var
 event,tmp:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=id;
 event.param.keycode:=keycode;

 while not g_ime_event_queue.enqueue(event) do
 begin
  g_ime_event_queue.dequeue(tmp); //drop first
 end;
end;

function ToUnicodeEx(wVirtKey,wScanCode:UINT;lpKeyState:PByte;pwszBuff:PWideChar;cchBuff:Integer;
  wFlags:UINT;dwhkl:HKL):Integer; stdcall; external 'user32.dll';

function GetCurrentTick(pTick:PQWORD):Integer;
var
 time:timespec;
begin
 if (pTick=nil) then Exit(-1);

 Result:=clock_gettime(CLOCK_REALTIME,@time);

 if (Result>=0) then
 begin
  pTick^:=(time.tv_nsec div 1000) + (time.tv_sec*1000000) + QWORD($dcbffeff2bc000);
 end;

end;

function scan_code_to_hid(scanCode:Word):Word;
begin
 Result:=SCE_IME_KEYCODE_ERRORUNDEFINED;
 Case scanCode of
  $00FF:Result:=SCE_IME_KEYCODE_ERRORROLLOVER;
  $00FC:Result:=SCE_IME_KEYCODE_POSTFAIL ;
  //
  $001E:Result:=SCE_IME_KEYCODE_A;
  $0030:Result:=SCE_IME_KEYCODE_B;
  $002E:Result:=SCE_IME_KEYCODE_C;
  $0020:Result:=SCE_IME_KEYCODE_D;
  $0012:Result:=SCE_IME_KEYCODE_E;
  $0021:Result:=SCE_IME_KEYCODE_F;
  $0022:Result:=SCE_IME_KEYCODE_G;
  $0023:Result:=SCE_IME_KEYCODE_H;
  $0017:Result:=SCE_IME_KEYCODE_I;
  $0024:Result:=SCE_IME_KEYCODE_J;
  $0025:Result:=SCE_IME_KEYCODE_K;
  $0026:Result:=SCE_IME_KEYCODE_L;
  $0032:Result:=SCE_IME_KEYCODE_M;
  $0031:Result:=SCE_IME_KEYCODE_N;
  $0018:Result:=SCE_IME_KEYCODE_O;
  $0019:Result:=SCE_IME_KEYCODE_P;
  $0010:Result:=SCE_IME_KEYCODE_Q;
  $0013:Result:=SCE_IME_KEYCODE_R;
  $001F:Result:=SCE_IME_KEYCODE_S;
  $0014:Result:=SCE_IME_KEYCODE_T;
  $0016:Result:=SCE_IME_KEYCODE_U;
  $002F:Result:=SCE_IME_KEYCODE_V;
  $0011:Result:=SCE_IME_KEYCODE_W;
  $002D:Result:=SCE_IME_KEYCODE_X;
  $0015:Result:=SCE_IME_KEYCODE_Y;
  $002C:Result:=SCE_IME_KEYCODE_Z;
  $0002:Result:=SCE_IME_KEYCODE_1;
  $0003:Result:=SCE_IME_KEYCODE_2;
  $0004:Result:=SCE_IME_KEYCODE_3;
  $0005:Result:=SCE_IME_KEYCODE_4;
  $0006:Result:=SCE_IME_KEYCODE_5;
  $0007:Result:=SCE_IME_KEYCODE_6;
  $0008:Result:=SCE_IME_KEYCODE_7;
  $0009:Result:=SCE_IME_KEYCODE_8;
  $000A:Result:=SCE_IME_KEYCODE_9;
  $000B:Result:=SCE_IME_KEYCODE_0;
  $001C:Result:=SCE_IME_KEYCODE_RETURN;
  $0001:Result:=SCE_IME_KEYCODE_ESCAPE;
  $000E:Result:=SCE_IME_KEYCODE_BACKSPACE;
  $000F:Result:=SCE_IME_KEYCODE_TAB;
  $0039:Result:=SCE_IME_KEYCODE_SPACEBAR;
  $000C:Result:=SCE_IME_KEYCODE_MINUS;
  $000D:Result:=SCE_IME_KEYCODE_EQUAL;
  $001A:Result:=SCE_IME_KEYCODE_LEFTBRACKET;
  $001B:Result:=SCE_IME_KEYCODE_RIGHTBRACKET;
  $002B:Result:=SCE_IME_KEYCODE_BACKSLASH;
  //
  $0027:Result:=SCE_IME_KEYCODE_SEMICOLON;
  $0028:Result:=SCE_IME_KEYCODE_SINGLEQUOTE;
  $0029:Result:=SCE_IME_KEYCODE_BACKQUOTE;
  $0033:Result:=SCE_IME_KEYCODE_COMMA;
  $0034:Result:=SCE_IME_KEYCODE_PERIOD;
  $0035:Result:=SCE_IME_KEYCODE_SLASH;
  $003A:Result:=SCE_IME_KEYCODE_CAPSLOCK;
  $003B:Result:=SCE_IME_KEYCODE_F1;
  $003C:Result:=SCE_IME_KEYCODE_F2;
  $003D:Result:=SCE_IME_KEYCODE_F3;
  $003E:Result:=SCE_IME_KEYCODE_F4;
  $003F:Result:=SCE_IME_KEYCODE_F5;
  $0040:Result:=SCE_IME_KEYCODE_F6;
  $0041:Result:=SCE_IME_KEYCODE_F7;
  $0042:Result:=SCE_IME_KEYCODE_F8;
  $0043:Result:=SCE_IME_KEYCODE_F9;
  $0044:Result:=SCE_IME_KEYCODE_F10;
  $0057:Result:=SCE_IME_KEYCODE_F11;
  $0058:Result:=SCE_IME_KEYCODE_F12;
  $E037,
  $0054:Result:=SCE_IME_KEYCODE_PRINTSCREEN;
  $0046:Result:=SCE_IME_KEYCODE_SCROLLLOCK;
  $E11D,
  $E046:Result:=SCE_IME_KEYCODE_PAUSE;
  $E052:Result:=SCE_IME_KEYCODE_INSERT;
  $E047:Result:=SCE_IME_KEYCODE_HOME;
  $E049:Result:=SCE_IME_KEYCODE_PAGEUP;
  $E053:Result:=SCE_IME_KEYCODE_DELETE;
  $E04F:Result:=SCE_IME_KEYCODE_END;
  $E051:Result:=SCE_IME_KEYCODE_PAGEDOWN;
  $E04D:Result:=SCE_IME_KEYCODE_RIGHTARROW;
  $E04B:Result:=SCE_IME_KEYCODE_LEFTARROW;
  $E050:Result:=SCE_IME_KEYCODE_DOWNARROW;
  $E048:Result:=SCE_IME_KEYCODE_UPARROW;
  $0045:Result:=SCE_IME_KEYCODE_KEYPAD_NUMLOCK;
  $E035:Result:=SCE_IME_KEYCODE_KEYPAD_SLASH;
  $0037:Result:=SCE_IME_KEYCODE_KEYPAD_ASTERISK;
  $004A:Result:=SCE_IME_KEYCODE_KEYPAD_MINUS;
  $004E:Result:=SCE_IME_KEYCODE_KEYPAD_PLUS;
  $E01C:Result:=SCE_IME_KEYCODE_KEYPAD_ENTER;
  $004F:Result:=SCE_IME_KEYCODE_KEYPAD_1;
  $0050:Result:=SCE_IME_KEYCODE_KEYPAD_2;
  $0051:Result:=SCE_IME_KEYCODE_KEYPAD_3;
  $004B:Result:=SCE_IME_KEYCODE_KEYPAD_4;
  $004C:Result:=SCE_IME_KEYCODE_KEYPAD_5;
  $004D:Result:=SCE_IME_KEYCODE_KEYPAD_6;
  $0047:Result:=SCE_IME_KEYCODE_KEYPAD_7;
  $0048:Result:=SCE_IME_KEYCODE_KEYPAD_8;
  $0049:Result:=SCE_IME_KEYCODE_KEYPAD_9;
  $0052:Result:=SCE_IME_KEYCODE_KEYPAD_0;
  $0053:Result:=SCE_IME_KEYCODE_KEYPAD_PERIOD;
  $0056:Result:=SCE_IME_KEYCODE_NONUS_BACKSLASH;
  $E05D:Result:=SCE_IME_KEYCODE_APPLICATION;
  $E05E:Result:=SCE_IME_KEYCODE_POWER;
  $0059:Result:=SCE_IME_KEYCODE_KEYPAD_EQUAL;
  $0064:Result:=SCE_IME_KEYCODE_F13;
  $0065:Result:=SCE_IME_KEYCODE_F14;
  $0066:Result:=SCE_IME_KEYCODE_F15;
  $0067:Result:=SCE_IME_KEYCODE_F16;
  $0068:Result:=SCE_IME_KEYCODE_F17;
  $0069:Result:=SCE_IME_KEYCODE_F18;
  $006A:Result:=SCE_IME_KEYCODE_F19;
  $006B:Result:=SCE_IME_KEYCODE_F20;
  $006C:Result:=SCE_IME_KEYCODE_F21;
  $006D:Result:=SCE_IME_KEYCODE_F22;
  $006E:Result:=SCE_IME_KEYCODE_F23;
  $006F:Result:=SCE_IME_KEYCODE_F24;
  //
  $007E:Result:=SCE_IME_KEYCODE_KEYPAD_COMMA;
  //
  $0073:Result:=SCE_IME_KEYCODE_INTERNATIONAL1;
  $0070:Result:=SCE_IME_KEYCODE_INTERNATIONAL2;
  $007D:Result:=SCE_IME_KEYCODE_INTERNATIONAL3;
  $0079:Result:=SCE_IME_KEYCODE_INTERNATIONAL4;
  $007B:Result:=SCE_IME_KEYCODE_INTERNATIONAL5;
  $005C:Result:=SCE_IME_KEYCODE_INTERNATIONAL6;
  //
  $00F2:Result:=SCE_IME_KEYCODE_LANG1;
  $00F1:Result:=SCE_IME_KEYCODE_LANG2;
  $0078:Result:=SCE_IME_KEYCODE_LANG3;
  $0077:Result:=SCE_IME_KEYCODE_LANG4;
  $0076:Result:=SCE_IME_KEYCODE_LANG5;
  //
  $001D:Result:=SCE_IME_KEYCODE_LEFTCONTROL;
  $002A:Result:=SCE_IME_KEYCODE_LEFTSHIFT;
  $0038:Result:=SCE_IME_KEYCODE_LEFTALT;
  $E05B:Result:=SCE_IME_KEYCODE_LEFTGUI;
  $E01D:Result:=SCE_IME_KEYCODE_RIGHTCONTROL;
  $0036:Result:=SCE_IME_KEYCODE_RIGHTSHIFT;
  $E038:Result:=SCE_IME_KEYCODE_RIGHTALT;
  $E05C:Result:=SCE_IME_KEYCODE_RIGHTGUI;

  else;
 end;
end;

function keyboard_layout_to_type(Layout:HKL):Integer;
begin
 Result:=SCE_IME_KEYBOARD_TYPE_NONE;

 Layout:=Layout and $FFFF;

 Case (Layout and $3FF) of
  LANG_DANISH:Result:=SCE_IME_KEYBOARD_TYPE_DANISH;

  LANG_GERMAN:
   Case (Layout shr 10) of //SUBLANG_*
    SUBLANG_GERMAN_SWISS:Result:=SCE_IME_KEYBOARD_TYPE_GERMAN_SW;
    else                 Result:=SCE_IME_KEYBOARD_TYPE_GERMAN;
   end;

  LANG_ENGLISH:
   Case (Layout shr 10) of //SUBLANG_*
    SUBLANG_ENGLISH_UK:Result:=SCE_IME_KEYBOARD_TYPE_ENGLISH_GB;
    else               Result:=SCE_IME_KEYBOARD_TYPE_ENGLISH_US;
   end;

  LANG_SPANISH   :
   Case (Layout shr 10) of //SUBLANG_*
    SUBLANG_SPANISH,
    SUBLANG_SPANISH_MEXICAN,
    SUBLANG_SPANISH_MODERN:Result:=SCE_IME_KEYBOARD_TYPE_SPANISH;
    else
                           Result:=SCE_IME_KEYBOARD_TYPE_SPANISH_LA;
   end;

  LANG_FINNISH:Result:=SCE_IME_KEYBOARD_TYPE_FINNISH;

  LANG_FRENCH:
   Case (Layout shr 10) of //SUBLANG_*
    SUBLANG_FRENCH_BELGIAN :Result:=SCE_IME_KEYBOARD_TYPE_FRENCH_BR;
    SUBLANG_FRENCH_CANADIAN:Result:=SCE_IME_KEYBOARD_TYPE_FRENCH_CA;
    SUBLANG_FRENCH_SWISS   :Result:=SCE_IME_KEYBOARD_TYPE_FRENCH_SW;
    else                    Result:=SCE_IME_KEYBOARD_TYPE_FRENCH;
   end;

  LANG_ITALIAN  :Result:=SCE_IME_KEYBOARD_TYPE_ITALIAN;
  LANG_DUTCH    :Result:=SCE_IME_KEYBOARD_TYPE_DUTCH;
  LANG_NORWEGIAN:Result:=SCE_IME_KEYBOARD_TYPE_NORWEGIAN;
  LANG_POLISH   :Result:=SCE_IME_KEYBOARD_TYPE_POLISH;

  LANG_PORTUGUESE:
   Case (Layout shr 10) of //SUBLANG_*
    SUBLANG_PORTUGUESE:Result:=SCE_IME_KEYBOARD_TYPE_PORTUGUESE_PT;
    else               Result:=SCE_IME_KEYBOARD_TYPE_PORTUGUESE_BR;
   end;

  LANG_RUSSIAN :Result:=SCE_IME_KEYBOARD_TYPE_RUSSIAN;
  LANG_SWEDISH :Result:=SCE_IME_KEYBOARD_TYPE_SWEDISH;
  LANG_TURKISH :Result:=SCE_IME_KEYBOARD_TYPE_TURKISH;
  LANG_JAPANESE:Result:=SCE_IME_KEYBOARD_TYPE_JAPANESE_ROMAN;
  LANG_KOREAN  :Result:=SCE_IME_KEYBOARD_TYPE_KOREAN;


  LANG_CHINESE:
   Case (Layout shr 10) of //SUBLANG_*
    SUBLANG_CHINESE_SIMPLIFIED:Result:=SCE_IME_KEYBOARD_TYPE_SM_CHINESE;
    SUBLANG_CHINESE_HONGKONG  :Result:=SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_HK
    else                       Result:=SCE_IME_KEYBOARD_TYPE_TR_CHINESE_CG;
   end;

  LANG_ARABIC    :Result:=SCE_IME_KEYBOARD_TYPE_ARABIC_AR;
  LANG_THAI      :Result:=SCE_IME_KEYBOARD_TYPE_THAI;
  LANG_CZECH     :Result:=SCE_IME_KEYBOARD_TYPE_CZECH;
  LANG_GREEK     :Result:=SCE_IME_KEYBOARD_TYPE_GREEK;
  LANG_INDONESIAN:Result:=SCE_IME_KEYBOARD_TYPE_INDONESIAN;
  LANG_VIETNAMESE:Result:=SCE_IME_KEYBOARD_TYPE_VIETNAMESE;
  LANG_ROMANIAN  :Result:=SCE_IME_KEYBOARD_TYPE_ROMANIAN;
  LANG_HUNGARIAN :Result:=SCE_IME_KEYBOARD_TYPE_HUNGARIAN;

  else;
 end;

end;

function KeyboardHookCallback(nCode:longint;wParam:WPARAM;lParam:LPARAM):LRESULT; stdcall;
var
 KeyBoardState:TKeyboardState;
 ActiveThreadID:DWORD;
 KeyBoardLayout:HKL;
 ScanCode:DWORD;
 status:DWORD;
 AChr:array[0..1] of WideChar;

 keycode:SceImeKeycode;
begin
 if (nCode=HC_ACTION) then
 begin
  KeyBoardState:=Default(TKeyboardState);
  GetKeyboardState(KeyBoardState);

  ActiveThreadID:=GetWindowThreadProcessId(GetForegroundWindow,nil);
  KeyBoardLayout:=GetKeyboardLayout(ActiveThreadID);

  ScanCode:=MapVirtualKeyEx(wParam,0,KeyBoardLayout);

  status:=0;
  if (ScanCode<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_KEYCODE_VALID;
   AChr[0]:=#0;
   AChr[1]:=#0;
   ToUnicodeEx(wParam,ScanCode,@KeyBoardState,@AChr,SizeOf(Achr),0,KeyBoardLayout);
   if (AChr[0]<>#0) then
   begin
    status:=status or SCE_IME_KEYCODE_STATE_CHARACTER_VALID;
   end;
  end;

  if ((lParam and (KF_ALTDOWN shl 16))<>0) then
  begin
   if ((lParam and (KF_EXTENDED shl 16))=0) then
   begin
    status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_ALT;
   end else
   begin
    status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_ALT;
   end;
  end;

  if ((GetKeyState(VK_LCONTROL) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_CTRL;
  end;

  if ((GetKeyState(VK_RCONTROL) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_CTRL;
  end;

  if ((GetKeyState(VK_LSHIFT) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_SHIFT;
  end;

  if ((GetKeyState(VK_RSHIFT) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_SHIFT;
  end;

  if ((GetKeyState(VK_LWIN) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_GUI;
  end;

  if ((GetKeyState(VK_RWIN) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_GUI;
  end;

  if ((KeyBoardState[VK_NUMLOCK] and 1)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_LED_NUM_LOCK;
  end;

  if ((KeyBoardState[VK_CAPITAL] and 1)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_LED_CAPS_LOCK;
  end;

  if ((KeyBoardState[VK_SCROLL] and 1)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_LED_SCROLL_LOCK;
  end;

  keycode:=Default(SceImeKeycode);
  keycode.keycode   :=scan_code_to_hid(scanCode);
  keycode.character :=AChr[0];
  keycode.status    :=status;
  keycode.ktype     :=keyboard_layout_to_type(KeyBoardLayout);
  keycode.userId    :=-1;
  keycode.resourceId:=1;
  GetCurrentTick(@keycode.timestamp);

  if ((lParam and (KF_UP shl 16))=0) then
  begin
   push_keyboard_code(SCE_IME_KEYBOARD_EVENT_KEYCODE_DOWN,keycode);
  end else
  begin
   push_keyboard_code(SCE_IME_KEYBOARD_EVENT_KEYCODE_UP,keycode);
  end;

 end;
 Result:=CallNextHookEx(g_hook,nCode,wParam,lParam);
end;

function ps4_sceImeKeyboardOpen(
          userId:SceUserServiceUserId;
          param:pSceImeKeyboardParam
          ):Integer;
begin
 Writeln('sceImeKeyboardOpen:',userId,' ',HexStr(param));

 if (param=nil) then Exit(SCE_IME_ERROR_INVALID_ADDRESS);

 if ((param^.option and (not $1F))<>0) then Exit(SCE_IME_ERROR_INVALID_OPTION);
 if (param^.handler=nil) then Exit(SCE_IME_ERROR_INVALID_HANDLER);

 if not CAS(keyboard_init,0,1) then Exit(SCE_IME_ERROR_BUSY);

 //_sig_lock;

  if (g_hook=0) then
  begin
   //g_hook:=SetWindowsHookExW(WH_KEYBOARD,@KeyboardHookCallback,GetModuleHandle(nil),MainThreadID);
  end;

 //_sig_unlock;

 {
 if (g_hook=0) then
 begin
  store_release(keyboard_init,0);
  Exit(SCE_IME_ERROR_INTERNAL);
 end;
 }

 g_handler:=param^.handler;
 g_cb_arg :=param^.arg;

 push_keyboard_open(userId);

 store_release(keyboard_init,2);
 Result:=0;
end;

function ps4_sceImeKeyboardClose(userId:SceUserServiceUserId):Integer;
begin
 Writeln('sceImeKeyboardClose:',userId);

 if not CAS(keyboard_init,2,3) then Exit(SCE_IME_ERROR_NOT_OPENED);

 //_sig_lock;
  UnhookWindowsHookEx(g_hook);
 //_sig_unlock;

 store_release(g_hook,0);
 store_release(QWORD(g_handler),9);
 store_release(QWORD(g_cb_arg),0);

 store_release(keyboard_init,0);
 Result:=0;
end;

function ps4_sceImeUpdate(handler:SceImeEventHandler):Integer;
var
 i:Integer;
 event:SceImeEvent;
begin
 if (handler=nil) then Exit(SCE_IME_ERROR_INTERNAL);

 event:=Default(SceImeEvent);
 For i:=0 to 255 do
 begin
  if not g_ime_event_queue.dequeue(event) then Break;
  //TODO: ExecuteGuest
  //handler(g_cb_arg,@event);
 end;

 Result:=0;
end;

function ps4_sceImeKeyboardGetResourceId(userId:SceUserServiceUserId;resourceIdArray:pSceImeKeyboardResourceIdArray):Integer;
begin
 if (keyboard_init=0) then Exit(SCE_IME_ERROR_NOT_OPENED);
 if (resourceIdArray=nil) then Exit(SCE_IME_ERROR_INVALID_ADDRESS);

 resourceIdArray^:=Default(SceImeKeyboardResourceIdArray);
 resourceIdArray^.userId:=userId;
 resourceIdArray^.resourceId[0]:=1;
end;

function ps4_sceImeKeyboardGetInfo(resourceId:DWORD;info:pSceImeKeyboardInfo):Integer;
var
 ActiveThreadID:DWORD;
 KeyBoardLayout:HKL;
begin
 if (keyboard_init=0) then Exit(SCE_IME_ERROR_NOT_OPENED);
 if (info=nil) then Exit(SCE_IME_ERROR_INVALID_ADDRESS);

 //_sig_lock;

 ActiveThreadID:=GetWindowThreadProcessId(GetForegroundWindow,nil);
 KeyBoardLayout:=GetKeyboardLayout(ActiveThreadID);

 //_sig_unlock;

 info^:=Default(SceImeKeyboardInfo);

 info^.userId     :=-1;
 info^.device     :=SCE_IME_KEYBOARD_DEVICE_TYPE_KEYBOARD;
 info^.ktype      :=keyboard_layout_to_type(KeyBoardLayout);
 info^.repeatDelay:=1;
 info^.repeatRate :=1;
 info^.status     :=CE_IME_KEYBOARD_STATE_CONNECTED;
end;

procedure init_ime;
begin
 g_ime_event_queue.Create(256);
end;

procedure ps4_sceImeParamInit(param:pSceImeParam);
begin
 param^:=Default(SceImeParam);
 param^.userId:=-1;
end;

function ps4_sceImeOpen(param   :pSceImeParam;
                        extended:pSceImeParamExtended):Integer;
begin
 Result:=0;
 Assert(False,'TODO:sceImeOpen');
end;

function ps4_sceImeGetPanelSize(param   :pSceImeParam;
                                p_width :PDWORD;
                                p_height:PDWORD):Integer;
label
 _end;
var
 filter:DWORD;
 width :DWORD;
 height:DWORD;
begin

 if (param=nil) or
    (p_width=nil) or
    (p_height=nil) then
 begin
  Exit(SCE_IME_ERROR_INVALID_ADDRESS);
 end;

 if (SCE_IME_TYPE_NUMBER < DWORD(param^.ImeType)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_TYPE);
 end;

 filter:=ord(p_proc.p_sdk_version > $14fffff) shl 8;

 if (p_proc.p_sdk_version > $174ffff) then
 begin
  filter:=filter or $78ff;
 end else
 begin
  filter:=filter or $70ff;
 end;

 if (p_proc.p_sdk_version > $2ffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $69ff;
 end;

 if (p_proc.p_sdk_version > $34fffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $59ff;
 end;

 if (p_proc.p_sdk_version > $3ffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $39ff;
 end;

 if ((param^.option and (filter xor $fffffdff))<>0) then
 begin
  Exit(SCE_IME_ERROR_INVALID_OPTION);
 end;

 if (param^.ImeType=SCE_IME_TYPE_BASIC_LATIN) then
 begin
  width :=793;
  height:=408;
 end else
 begin
  if (param^.ImeType<>SCE_IME_TYPE_NUMBER) then
  begin
   if ((param^.option and $c0000004)<>SCE_IME_OPTION_PASSWORD) then
   begin
    height:=408;
    width :=793;
    goto _end;
   end else
   begin
    width :=793;
    height:=408;
   end;
  end else
  begin
   width :=370;
   height:=402;
  end;
 end;

 if (p_proc.p_sdk_version > $16fffff) then
 begin
  height:=height;
 end else
 begin
  height:=368;
 end;

 _end:
 if ((param^.option and SCE_IME_OPTION_USE_OVER_2K_COORDINATES)<>0) then
 begin
  width :=width  shl 1;
  height:=height shl 1;
 end;

 p_width^ :=width;
 p_height^:=height;
 Result:=0;
end;

function Load_libSceIme(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceIme');

 lib:=Result^.add_lib('libSceIme');
 lib.set_proc($79A1578DF26FDF1B,@ps4_sceImeKeyboardOpen);
 lib.set_proc($3CC55E85295F67DE,@ps4_sceImeKeyboardClose);
 lib.set_proc($FF81827D874D175B,@ps4_sceImeUpdate);
 lib.set_proc($74A69DA9916028A4,@ps4_sceImeKeyboardGetResourceId);
 lib.set_proc($564A8B3C0ADF15D7,@ps4_sceImeKeyboardGetInfo);
 //
 lib.set_proc($5A6603CDD0B81072,@ps4_sceImeParamInit);
 lib.set_proc($44FC9DBFF26BD5B7,@ps4_sceImeOpen);
 lib.set_proc($CE23C37088CED159,@ps4_sceImeGetPanelSize);

 init_ime;
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceIme.prx',@Load_libSceIme);

end.

