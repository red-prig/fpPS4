unit ps4_libSceIme;

{$mode ObjFPC}{$H+}

interface

uses
  windows,
  sys_types,
  ps4_time,
  ps4_program,
  atomic,
  mpmc_queue,
  Classes,
  SysUtils;

const
 SCE_IME_ERROR_BUSY                        =-2135162879; // 0x80BC0001
 SCE_IME_ERROR_NOT_OPENED                  =-2135162878; // 0x80BC0002
 SCE_IME_ERROR_NO_MEMORY                   =-2135162877; // 0x80BC0003
 SCE_IME_ERROR_CONNECTION_FAILED           =-2135162876; // 0x80BC0004
 SCE_IME_ERROR_TOO_MANY_REQUESTS           =-2135162875; // 0x80BC0005
 SCE_IME_ERROR_INVALID_TEXT                =-2135162874; // 0x80BC0006
 SCE_IME_ERROR_EVENT_OVERFLOW              =-2135162873; // 0x80BC0007
 SCE_IME_ERROR_NOT_ACTIVE                  =-2135162872; // 0x80BC0008
 SCE_IME_ERROR_IME_SUSPENDING              =-2135162871; // 0x80BC0009
 SCE_IME_ERROR_DEVICE_IN_USE               =-2135162870; // 0x80BC000A
 SCE_IME_ERROR_INVALID_USER_ID             =-2135162864; // 0x80BC0010
 SCE_IME_ERROR_INVALID_TYPE                =-2135162863; // 0x80BC0011
 SCE_IME_ERROR_INVALID_SUPPORTED_LANGUAGES =-2135162862; // 0x80BC0012
 SCE_IME_ERROR_INVALID_ENTER_LABEL         =-2135162861; // 0x80BC0013
 SCE_IME_ERROR_INVALID_INPUT_METHOD        =-2135162860; // 0x80BC0014
 SCE_IME_ERROR_INVALID_OPTION              =-2135162859; // 0x80BC0015
 SCE_IME_ERROR_INVALID_MAX_TEXT_LENGTH     =-2135162858; // 0x80BC0016
 SCE_IME_ERROR_INVALID_INPUT_TEXT_BUFFER   =-2135162857; // 0x80BC0017
 SCE_IME_ERROR_INVALID_POSX                =-2135162856; // 0x80BC0018
 SCE_IME_ERROR_INVALID_POSY                =-2135162855; // 0x80BC0019
 SCE_IME_ERROR_INVALID_HORIZONTAL_ALIGNMENT=-2135162854; // 0x80BC001A
 SCE_IME_ERROR_INVALID_VERTICAL_ALIGNMENT  =-2135162853; // 0x80BC001B
 SCE_IME_ERROR_INVALID_EXTENDED            =-2135162852; // 0x80BC001C
 SCE_IME_ERROR_INVALID_KEYBOARD_TYPE       =-2135162851; // 0x80BC001D
 SCE_IME_ERROR_INVALID_WORK                =-2135162848; // 0x80BC0020
 SCE_IME_ERROR_INVALID_ARG                 =-2135162847; // 0x80BC0021
 SCE_IME_ERROR_INVALID_HANDLER             =-2135162846; // 0x80BC0022
 SCE_IME_ERROR_NO_RESOURCE_ID              =-2135162845; // 0x80BC0023
 SCE_IME_ERROR_INVALID_MODE                =-2135162844; // 0x80BC0024
 SCE_IME_ERROR_INVALID_PARAM               =-2135162832; // 0x80BC0030
 SCE_IME_ERROR_INVALID_ADDRESS             =-2135162831; // 0x80BC0031
 SCE_IME_ERROR_INVALID_RESERVED            =-2135162830; // 0x80BC0032
 SCE_IME_ERROR_INVALID_TIMING              =-2135162829; // 0x80BC0033
 SCE_IME_ERROR_INTERNAL                    =-2135162625; // 0x80BC00FF


 SCE_IME_WORK_BUFFER_SIZE=(20*1024);

 //Various maximum sizes for the IME
 SCE_IME_MAX_PREEDIT_LENGTH         =30;
 SCE_IME_MAX_EXPANDED_PREEDIT_LENGTH=120;
 SCE_IME_MAX_TEXT_LENGTH            =2048;
 SCE_IME_MAX_TEXT_AREA              =4;
 SCE_IME_MAX_CANDIDATE_WORD_LENGTH  =55;
 SCE_IME_MAX_CANDIDATE_LIST_SIZE    =100;

 //OSK display area standard values
 SCE_IME_OSK_DISPLAY_SIZE_WIDTH =1920;
 SCE_IME_OSK_DISPLAY_SIZE_HEIGHT=1080;

 SCE_IME_OSK_OVER_2K_DISPLAY_SIZE_WIDTH =3840;
 SCE_IME_OSK_OVER_2K_DISPLAY_SIZE_HEIGHT=2160;

 //Languages supported by the IME
 SCE_IME_LANGUAGE_DANISH             =$0000000000000001;
 SCE_IME_LANGUAGE_GERMAN             =$0000000000000002;
 SCE_IME_LANGUAGE_ENGLISH_US         =$0000000000000004;
 SCE_IME_LANGUAGE_SPANISH            =$0000000000000008;
 SCE_IME_LANGUAGE_FRENCH             =$0000000000000010;
 SCE_IME_LANGUAGE_ITALIAN            =$0000000000000020;
 SCE_IME_LANGUAGE_DUTCH              =$0000000000000040;
 SCE_IME_LANGUAGE_NORWEGIAN          =$0000000000000080;
 SCE_IME_LANGUAGE_POLISH             =$0000000000000100;
 SCE_IME_LANGUAGE_PORTUGUESE_PT      =$0000000000000200;
 SCE_IME_LANGUAGE_RUSSIAN            =$0000000000000400;
 SCE_IME_LANGUAGE_FINNISH            =$0000000000000800;
 SCE_IME_LANGUAGE_SWEDISH            =$0000000000001000;
 SCE_IME_LANGUAGE_JAPANESE           =$0000000000002000;
 SCE_IME_LANGUAGE_KOREAN             =$0000000000004000;
 SCE_IME_LANGUAGE_SIMPLIFIED_CHINESE =$0000000000008000;
 SCE_IME_LANGUAGE_TRADITIONAL_CHINESE=$0000000000010000;
 SCE_IME_LANGUAGE_PORTUGUESE_BR      =$0000000000020000;
 SCE_IME_LANGUAGE_ENGLISH_GB         =$0000000000040000;
 SCE_IME_LANGUAGE_TURKISH            =$0000000000080000;
 SCE_IME_LANGUAGE_SPANISH_LA         =$0000000000100000;
 SCE_IME_LANGUAGE_ARABIC             =$0000000001000000;
 SCE_IME_LANGUAGE_FRENCH_CA          =$0000000002000000;
 SCE_IME_LANGUAGE_THAI               =$0000000004000000;
 SCE_IME_LANGUAGE_CZECH              =$0000000008000000;
 SCE_IME_LANGUAGE_GREEK              =$0000000010000000;
 SCE_IME_LANGUAGE_INDONESIAN         =$0000000020000000;
 SCE_IME_LANGUAGE_VIETNAMESE         =$0000000040000000;
 SCE_IME_LANGUAGE_ROMANIAN           =$0000000080000000;
 SCE_IME_LANGUAGE_HUNGARIAN          =$0000000100000000;

 //IME options
 SCE_IME_OPTION_DEFAULT                           =$00000000;
 SCE_IME_OPTION_MULTILINE                         =$00000001;
 SCE_IME_OPTION_NO_AUTO_CAPITALIZATION            =$00000002;
 SCE_IME_OPTION_PASSWORD                          =$00000004;
 SCE_IME_OPTION_LANGUAGES_FORCED                  =$00000008;
 SCE_IME_OPTION_EXT_KEYBOARD                      =$00000010;
 SCE_IME_OPTION_NO_LEARNING                       =$00000020;
 SCE_IME_OPTION_FIXED_POSITION                    =$00000040;
 SCE_IME_OPTION_DISABLE_COPY_PASTE                =$00000080;
 SCE_IME_OPTION_DISABLE_RESUME                    =$00000100;
 SCE_IME_OPTION_DISABLE_AUTO_SPACE                =$00000200;
 SCE_IME_OPTION_DISABLE_POSITION_ADJUSTMENT       =$00000800;
 SCE_IME_OPTION_EXPANDED_PREEDIT_BUFFER           =$00001000;
 SCE_IME_OPTION_USE_JAPANESE_EISUU_KEY_AS_CAPSLOCK=$00002000;
 SCE_IME_OPTION_USE_OVER_2K_COORDINATES           =$00004000;

 //IME extended options
 SCE_IME_EXT_OPTION_DEFAULT                      =$00000000;
 SCE_IME_EXT_OPTION_SET_COLOR                    =$00000001;
 SCE_IME_EXT_OPTION_SET_PRIORITY                 =$00000002;
 SCE_IME_EXT_OPTION_PRIORITY_SHIFT               =$00000004;
 SCE_IME_EXT_OPTION_PRIORITY_FULL_WIDTH          =$00000008;
 SCE_IME_EXT_OPTION_PRIORITY_FIXED_PANEL         =$00000010;
 SCE_IME_EXT_OPTION_DISABLE_POINTER              =$00000040;
 SCE_IME_EXT_OPTION_ENABLE_ADDITIONAL_DICTIONARY =$00000080;
 SCE_IME_EXT_OPTION_DISABLE_STARTUP_SE           =$00000100;
 SCE_IME_EXT_OPTION_DISABLE_LIST_FOR_EXT_KEYBOARD=$00000200;
 SCE_IME_EXT_OPTION_HIDE_KEYPANEL_IF_EXT_KEYBOARD=$00000400;
 SCE_IME_EXT_OPTION_INIT_EXT_KEYBOARD_MODE       =$00000800;

 SCE_IME_DIALOG_EXT_OPTION_ENABLE_ACCESSIBILITY        =$00001000;
 SCE_IME_DIALOG_EXT_OPTION_ACCESSIBILITY_PANEL_FORCED  =$00002000;
 SCE_IME_EXT_OPTION_ADDITIONAL_DICTIONARY_PRIORITY_MODE=$00004000;

 //Device not used with the IME
 SCE_IME_DISABLE_DEVICE_DEFAULT     =$00000000;
 SCE_IME_DISABLE_DEVICE_CONTROLLER  =$00000001;
 SCE_IME_DISABLE_DEVICE_EXT_KEYBOARD=$00000002;
 SCE_IME_DISABLE_DEVICE_REMOTE_OSK  =$00000004;

 // States of edited strings
 SCE_IME_INPUT_METHOD_STATE_PREEDIT   =$01000000;
 SCE_IME_INPUT_METHOD_STATE_SELECTED  =$02000000;
 SCE_IME_INPUT_METHOD_STATE_NATIVE    =$04000000;
 SCE_IME_INPUT_METHOD_STATE_NATIVE2   =$08000000;
 SCE_IME_INPUT_METHOD_STATE_FULL_WIDTH=$10000000;

 //External keyboard initial mode
 SCE_IME_INIT_EXT_KEYBOARD_MODE_DISABLE_ARABIC_INDIC_NUMERALS=$00000001;
 SCE_IME_INIT_EXT_KEYBOARD_MODE_ENABLE_FORMAT_CHARACTERS     =$00000002;
 SCE_IME_INIT_EXT_KEYBOARD_MODE_INPUT_METHOD_STATE_NATIVE    =SCE_IME_INPUT_METHOD_STATE_NATIVE;
 SCE_IME_INIT_EXT_KEYBOARD_MODE_INPUT_METHOD_STATE_NATIVE2   =SCE_IME_INPUT_METHOD_STATE_NATIVE2;
 SCE_IME_INIT_EXT_KEYBOARD_MODE_INPUT_METHOD_STATE_FULL_WIDTH=SCE_IME_INPUT_METHOD_STATE_FULL_WIDTH ;

 //Event IDs of IME event/keyboard event
 //SceImeEventId
 SCE_IME_EVENT_OPEN                      =  0;
 SCE_IME_EVENT_UPDATE_TEXT               =  1;
 SCE_IME_EVENT_UPDATE_CARET              =  2;
 SCE_IME_EVENT_CHANGE_SIZE               =  3;
 SCE_IME_EVENT_PRESS_CLOSE               =  4;
 SCE_IME_EVENT_PRESS_ENTER               =  5;
 SCE_IME_EVENT_ABORT                     =  6;
 SCE_IME_EVENT_CANDIDATE_LIST_START      =  7;
 SCE_IME_EVENT_CANDIDATE_LIST_END        =  8;
 SCE_IME_EVENT_CANDIDATE_WORD            =  9;
 SCE_IME_EVENT_CANDIDATE_INDEX           = 10;
 SCE_IME_EVENT_CANDIDATE_DONE            = 11;
 SCE_IME_EVENT_CANDIDATE_CANCEL          = 12;
 SCE_IME_EVENT_CHANGE_DEVICE             = 14;
 SCE_IME_EVENT_JUMP_TO_NEXT_OBJECT       = 15;
 SCE_IME_EVENT_JUMP_TO_BEFORE_OBJECT     = 16;
 SCE_IME_EVENT_CHANGE_WINDOW_TYPE        = 17;

 SCE_IME_EVENT_CHANGE_INPUT_METHOD_STATE = 18;

 SCE_IME_KEYBOARD_EVENT_OPEN             = 256;
 SCE_IME_KEYBOARD_EVENT_KEYCODE_DOWN     = 257;
 SCE_IME_KEYBOARD_EVENT_KEYCODE_UP       = 258;
 SCE_IME_KEYBOARD_EVENT_KEYCODE_REPEAT   = 259;
 SCE_IME_KEYBOARD_EVENT_CONNECTION       = 260;
 SCE_IME_KEYBOARD_EVENT_DISCONNECTION    = 261;
 SCE_IME_KEYBOARD_EVENT_ABORT            = 262;

 //IME horizontal display origins
 //SceImeHorizontalAlignment
 SCE_IME_HALIGN_LEFT     = 0;
 SCE_IME_HALIGN_CENTER   = 1;
 SCE_IME_HALIGN_RIGHT    = 2;

 //IME vertical display origins
 //SceImeVerticalAlignment
 SCE_IME_VALIGN_TOP      = 0;
 SCE_IME_VALIGN_CENTER   = 1;
 SCE_IME_VALIGN_BOTTOM   = 2;

 //IME Enter key labels
 //SceImeEnterLabel
 SCE_IME_ENTER_LABEL_DEFAULT = 0;
 SCE_IME_ENTER_LABEL_SEND    = 1;
 SCE_IME_ENTER_LABEL_SEARCH  = 2;
 SCE_IME_ENTER_LABEL_GO      = 3;

 //IME input UI types
 //SceImeType
 SCE_IME_TYPE_DEFAULT     = 0;
 SCE_IME_TYPE_BASIC_LATIN = 1;
 SCE_IME_TYPE_URL         = 2;
 SCE_IME_TYPE_MAIL        = 3;
 SCE_IME_TYPE_NUMBER      = 4;


 //IME prioritized input panel
 //SceImePanelPriority
 SCE_IME_PANEL_PRIORITY_DEFAULT  = 0;
 SCE_IME_PANEL_PRIORITY_ALPHABET = 1;
 SCE_IME_PANEL_PRIORITY_SYMBOL   = 2;
 SCE_IME_PANEL_PRIORITY_ACCENT   = 3;

 //IME input method
 //SceImeInputMethod
 SCE_IME_INPUT_METHOD_DEFAULT  = 0;

 //Caret movement position information
 //SceImeCaretMovementDirection
 SCE_IME_CARET_MOVE_STILL     =  0;
 SCE_IME_CARET_MOVE_LEFT      =  1;
 SCE_IME_CARET_MOVE_RIGHT     =  2;
 SCE_IME_CARET_MOVE_UP        =  3;
 SCE_IME_CARET_MOVE_DOWN      =  4;
 SCE_IME_CARET_MOVE_HOME      =  5;
 SCE_IME_CARET_MOVE_END       =  6;
 SCE_IME_CARET_MOVE_PAGE_UP   =  7;
 SCE_IME_CARET_MOVE_PAGE_DOWN =  8;
 SCE_IME_CARET_MOVE_TOP       =  9;
 SCE_IME_CARET_MOVE_BOTTOM    = 10;

 //Text area type
 //SceImeTextAreaMode
 SCE_IME_TEXT_AREA_MODE_DISABLE  = 0;
 SCE_IME_TEXT_AREA_MODE_EDIT     = 1;
 SCE_IME_TEXT_AREA_MODE_PREEDIT  = 2;
 SCE_IME_TEXT_AREA_MODE_SELECT   = 3;

 //Definitions for keyboard
 SCE_IME_KEYBOARD_MAX_NUMBER = 5;

 //Keyboard states
 SCE_IME_KEYCODE_STATE_KEYCODE_VALID     =$00000001;
 SCE_IME_KEYCODE_STATE_CHARACTER_VALID   =$00000002;
 SCE_IME_KEYCODE_STATE_WITH_IME          =$00000004;
 SCE_IME_KEYCODE_STATE_FROM_OSK          =$00000008;
 SCE_IME_KEYCODE_STATE_FROM_OSK_SHORTCUT =$00000010;
 SCE_IME_KEYCODE_STATE_FROM_IME_OPERATION=$00000020;
 SCE_IME_KEYCODE_STATE_REPLACE_CHARACTER =$00000040;
 SCE_IME_KEYCODE_STATE_CONTINUOUS_EVENT  =$00000080;
 SCE_IME_KEYCODE_STATE_MODIFIER_L_CTRL   =$00000100;
 SCE_IME_KEYCODE_STATE_MODIFIER_L_SHIFT  =$00000200;
 SCE_IME_KEYCODE_STATE_MODIFIER_L_ALT    =$00000400;
 SCE_IME_KEYCODE_STATE_MODIFIER_L_GUI    =$00000800;
 SCE_IME_KEYCODE_STATE_MODIFIER_R_CTRL   =$00001000;
 SCE_IME_KEYCODE_STATE_MODIFIER_R_SHIFT  =$00002000;
 SCE_IME_KEYCODE_STATE_MODIFIER_R_ALT    =$00004000;
 SCE_IME_KEYCODE_STATE_MODIFIER_R_GUI    =$00008000;
 SCE_IME_KEYCODE_STATE_LED_NUM_LOCK      =$00010000;
 SCE_IME_KEYCODE_STATE_LED_CAPS_LOCK     =$00020000;
 SCE_IME_KEYCODE_STATE_LED_SCROLL_LOCK   =$00040000;
 SCE_IME_KEYCODE_STATE_RESERVED1         =$00080000;
 SCE_IME_KEYCODE_STATE_RESERVED2         =$00100000;
 SCE_IME_KEYCODE_STATE_FROM_IME_INPUT    =$00200000;

 //Keyboard reception options
 SCE_IME_KEYBOARD_OPTION_DEFAULT           =$00000000;
 SCE_IME_KEYBOARD_OPTION_REPEAT            =$00000001;
 SCE_IME_KEYBOARD_OPTION_REPEAT_EACH_KEY   =$00000002;
 SCE_IME_KEYBOARD_OPTION_ADD_OSK           =$00000004;
 SCE_IME_KEYBOARD_OPTION_EFFECTIVE_WITH_IME=$00000008;
 SCE_IME_KEYBOARD_OPTION_DISABLE_RESUME    =$00000010;

 SCE_IME_KEYBOARD_OPTION_DISABLE_CAPSLOCK_WITHOUT_SHIFT=$00000020;

 //Keyboard output modes
 SCE_IME_KEYBOARD_MODE_AUTO                     =$00000000;
 SCE_IME_KEYBOARD_MODE_MANUAL                   =$00000001;
 SCE_IME_KEYBOARD_MODE_ALPHABET                 =$00000000;
 SCE_IME_KEYBOARD_MODE_NATIVE                   =$00000002;
 SCE_IME_KEYBOARD_MODE_PART                     =$00000004;
 SCE_IME_KEYBOARD_MODE_KATAKANA                 =$00000008;
 SCE_IME_KEYBOARD_MODE_HKANA                    =$00000010;
 SCE_IME_KEYBOARD_MODE_ARABIC_INDIC_NUMERALS    =$00000020;
 SCE_IME_KEYBOARD_MODE_DISABLE_FORMAT_CHARACTERS=$00000040;

 //Defined keyboard resource ID
 SCE_IME_KEYBOARD_RESOURCE_ID_INVALID=$00000000;
 SCE_IME_KEYBOARD_RESOURCE_ID_OSK    =$00000001;

 //Keyboard device statuses
 //SceImeKeyboardStatus
 CE_IME_KEYBOARD_STATE_DISCONNECTED = 0;
 CE_IME_KEYBOARD_STATE_CONNECTED    = 1;

 //Keyboard layout types for keyboard devices
 //SceImeKeyboardType
 SCE_IME_KEYBOARD_TYPE_NONE             =  0;
 SCE_IME_KEYBOARD_TYPE_DANISH           =  1;
 SCE_IME_KEYBOARD_TYPE_GERMAN           =  2;
 SCE_IME_KEYBOARD_TYPE_GERMAN_SW        =  3;
 SCE_IME_KEYBOARD_TYPE_ENGLISH_US       =  4;
 SCE_IME_KEYBOARD_TYPE_ENGLISH_GB       =  5;
 SCE_IME_KEYBOARD_TYPE_SPANISH          =  6;
 SCE_IME_KEYBOARD_TYPE_SPANISH_LA       =  7;
 SCE_IME_KEYBOARD_TYPE_FINNISH          =  8;
 SCE_IME_KEYBOARD_TYPE_FRENCH           =  9;
 SCE_IME_KEYBOARD_TYPE_FRENCH_BR        = 10;
 SCE_IME_KEYBOARD_TYPE_FRENCH_CA        = 11;
 SCE_IME_KEYBOARD_TYPE_FRENCH_SW        = 12;
 SCE_IME_KEYBOARD_TYPE_ITALIAN          = 13;
 SCE_IME_KEYBOARD_TYPE_DUTCH            = 14;
 SCE_IME_KEYBOARD_TYPE_NORWEGIAN        = 15;
 SCE_IME_KEYBOARD_TYPE_POLISH           = 16;
 SCE_IME_KEYBOARD_TYPE_PORTUGUESE_BR    = 17;
 SCE_IME_KEYBOARD_TYPE_PORTUGUESE_PT    = 18;
 SCE_IME_KEYBOARD_TYPE_RUSSIAN          = 19;
 SCE_IME_KEYBOARD_TYPE_SWEDISH          = 20;
 SCE_IME_KEYBOARD_TYPE_TURKISH          = 21;
 SCE_IME_KEYBOARD_TYPE_JAPANESE_ROMAN   = 22;
 SCE_IME_KEYBOARD_TYPE_JAPANESE_KANA    = 23;
 SCE_IME_KEYBOARD_TYPE_KOREAN           = 24;
 SCE_IME_KEYBOARD_TYPE_SM_CHINESE       = 25;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_ZY    = 26;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_HK = 27;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_TW = 28;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_CG    = 29;
 SCE_IME_KEYBOARD_TYPE_ARABIC_AR        = 30;
 SCE_IME_KEYBOARD_TYPE_THAI             = 31;
 SCE_IME_KEYBOARD_TYPE_CZECH            = 32;
 SCE_IME_KEYBOARD_TYPE_GREEK            = 33;
 SCE_IME_KEYBOARD_TYPE_INDONESIAN       = 34;
 SCE_IME_KEYBOARD_TYPE_VIETNAMESE       = 35;
 SCE_IME_KEYBOARD_TYPE_ROMANIAN         = 36;
 SCE_IME_KEYBOARD_TYPE_HUNGARIAN        = 37;

 //Keyboard device types
 //SceImeKeyboardDeviceType
 SCE_IME_KEYBOARD_DEVICE_TYPE_KEYBOARD = 0;
 SCE_IME_KEYBOARD_DEVICE_TYPE_OSK      = 1;

 //IME panel type
 //SceImePanelType
 SCE_IME_PANEL_TYPE_HIDE               = 0;
 SCE_IME_PANEL_TYPE_OSK                = 1;
 SCE_IME_PANEL_TYPE_DIALOG             = 2;
 SCE_IME_PANEL_TYPE_CANDIDATE          = 3;
 SCE_IME_PANEL_TYPE_EDIT               = 4;
 SCE_IME_PANEL_TYPE_EDIT_AND_CANDIDATE = 5;
 SCE_IME_PANEL_TYPE_ACCESSIBILITY      = 6;

 //Types of input devices that use the IME
 //SceImeDeviceType
 SCE_IME_DEVICE_TYPE_NONE         = 0;
 SCE_IME_DEVICE_TYPE_CONTROLLER   = 1;
 SCE_IME_DEVICE_TYPE_EXT_KEYBOARD = 2;
 SCE_IME_DEVICE_TYPE_REMOTE_OSK   = 3;

type
 SceImeTextAreaProperty=packed record
  mode:Integer; //SceImeTextAreaMode
  index:DWORD;
  length:Integer;
 end;

 SceImeEditText=packed record
  str:pWideChar;
  caretIndex:DWORD;
  areaNum:DWORD;
  textArea:array[0..SCE_IME_MAX_TEXT_AREA-1] of SceImeTextAreaProperty;
 end;

 SceImePositionAndForm=packed record
  _type:Integer; //SceImePanelType
  posx,posy:Single;
  horizontalAlignment:Integer; //SceImeHorizontalAlignment
  verticalAlignment  :Integer; //SceImeVerticalAlignment
  width,height:DWORD;
 end;

 SceImeRect=packed record
  x,y:Single;
  width,height:DWORD;
 end;

 SceImeTextGeometry=packed record
  x,y:Single;
  width,height:DWORD;
 end;

 SceImeCaret=packed record
  x,y:Single;
  height:DWORD;
  index:DWORD;
 end;

 SceImeColor=packed record
  r,g,b,a:Byte;
 end;

 pSceImeKeyboardInfo=^SceImeKeyboardInfo;
 SceImeKeyboardInfo=packed record
  userId:Integer;
  device:Integer; //SceImeKeyboardDeviceType
  _type:Integer; //SceImeKeyboardType
  repeatDelay:DWORD;
  repeatRate :DWORD;
  status:Integer; //SceImeKeyboardStatus
  reserved:array[0..11] of Byte;
 end;

 pSceImeKeyboardResourceIdArray=^SceImeKeyboardResourceIdArray;
 SceImeKeyboardResourceIdArray=packed record
  userId:Integer;
  resourceId:array[0..SCE_IME_KEYBOARD_MAX_NUMBER-1] of DWORD;
 end;

 SceImeKeycode=packed record
  keycode:Word;
  character:WideChar;
  status:DWORD;
  _type:Integer; //SceImeKeyboardType
  userId:Integer;
  resourceId:DWORD;
  _align:Integer;
  timestamp:QWORD; //SceRtcTick
 end;

 pSceImeEventParam=^SceImeEventParam;
 SceImeEventParam=packed record
  Case Byte of
   0:(rect:SceImeRect);
   1:(text:SceImeEditText);
   2:(caretMove:Integer); //SceImeCaretMovementDirection
   3:(keycode:SceImeKeycode);
   4:(resourceIdArray:SceImeKeyboardResourceIdArray);
   5:(candidateWord:PWideChar);
   6:(candidateIndex:Integer);
   7:(deviceType:Integer); //SceImeDeviceType
   8:(panelType:Integer);  //SceImePanelType
   9:(inputMethodState:DWORD);
  10:(reserved:array[0..63] of Byte);
 end;

 pSceImeEvent=^SceImeEvent;
 SceImeEvent=packed record
  id:Integer; //SceImeEventId
  _align:Integer;
  param:SceImeEventParam;
 end;

 SceImeEventHandler=procedure(arg:Pointer;e:pSceImeEvent); SysV_ABI_CDecl;

 pSceImeKeyboardParam=^SceImeKeyboardParam;
 SceImeKeyboardParam=packed record
  option:DWORD;
  reserved1:DWORD;
  arg:Pointer;
  handler:SceImeEventHandler;
  reserved2:QWORD;
 end;

implementation

type
 t_ime_event_queue=specialize mpmc_bounded_queue<SceImeEvent>;

var
 g_ime_event_queue:t_ime_event_queue;

 keyboard_init:QWORD=0;
 keyboard_fini:QWORD=0;

 g_hook:HHOOK;

 g_handler:SceImeEventHandler=nil;
 g_cb_arg:Pointer=nil;

Procedure push_keyboard_open(userId:Integer);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=SCE_IME_KEYBOARD_EVENT_OPEN;
 event.param.resourceIdArray.userId:=userId;
 event.param.resourceIdArray.resourceId[0]:=1;

 g_ime_event_queue.enqueue(event);
end;

Procedure push_keyboard_code(id:Integer;var keycode:SceImeKeycode);
var
 event:SceImeEvent;
begin
 event:=Default(SceImeEvent);
 event.id:=id;
 event.param.keycode:=keycode;

 g_ime_event_queue.enqueue(event);
end;

function ToUnicodeEx(wVirtKey,wScanCode:UINT;lpKeyState:PByte;pwszBuff:PWideChar;cchBuff:Integer;
  wFlags:UINT;dwhkl:HKL):Integer; stdcall; external 'user32.dll';

function GetCurrentTick(pTick:PQWORD):Integer;
var
 time:timespec;
begin
 if (pTick=nil) then Exit(-1);

 Result:=ps4_sceKernelClockGettime(0,@time);

 if (Result>=0) then
 begin
  pTick^:=(time.tv_nsec div 1000) + (time.tv_sec*1000000) + $dcbffeff2bc000;
 end
end;

function KeyboardHookCallback(nCode:longint;wParam:WPARAM;lParam:LPARAM):LRESULT; stdcall;
var
 KeyBoardState:TKeyboardState;
 ActiveThreadID:DWORD;
 KeyBoardLayOut:HKL;
 ScanCode:DWORD;
 status:DWORD;
 AChr:array[0..1] of WideChar;

 keycode:SceImeKeycode;
begin
 if (nCode=HC_ACTION) then
 begin
  KeyBoardState:=Default(TKeyboardState);
  GetKeyboardState(KeyBoardState);

  ActiveThreadID:=GetWindowThreadProcessId(GetForegroundWindow, nil);
  KeyBoardLayOut:=GetKeyboardLayout(ActiveThreadID);

  ScanCode:=MapVirtualKeyEx(wParam,0,KeyBoardLayOut);

  status:=0;
  if (ScanCode<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_KEYCODE_VALID;
   AChr[0]:=#0;
   AChr[1]:=#0;
   ToUnicodeEx(wParam,ScanCode,@KeyBoardState,@AChr,SizeOf(Achr),0,KeyBoardLayOut);
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

  if ((GetKeyState(VK_CONTROL) and $8000)<>0) then
  begin
   if ((lParam and (KF_EXTENDED shl 16))=0) then
   begin
    status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_CTRL;
   end else
   begin
    status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_CTRL;
   end;
  end;

  if ((GetKeyState(VK_SHIFT) and $8000)<>0) then
  begin
   if ((lParam and (KF_EXTENDED shl 16))=0) then
   begin
    status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_SHIFT;
   end else
   begin
    status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_SHIFT;
   end;
  end;

  if ((GetKeyState(VK_LWIN) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_L_GUI;
  end;

  if ((GetKeyState(VK_RWIN) and $8000)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_MODIFIER_R_GUI;
  end;

  if ((GetKeyState(VK_NUMLOCK) and 1)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_LED_NUM_LOCK;
  end;

  if ((GetKeyState(VK_CAPITAL) and 1)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_LED_CAPS_LOCK;
  end;

  if ((GetKeyState(VK_SCROLL) and 1)<>0) then
  begin
   status:=status or SCE_IME_KEYCODE_STATE_LED_SCROLL_LOCK;
  end;

  keycode:=Default(SceImeKeycode);
  keycode.keycode   :=ScanCode;
  keycode.character :=AChr[0];
  keycode.status    :=status;
  keycode._type     :=SCE_IME_KEYBOARD_TYPE_ENGLISH_US;
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
          userId:Integer;
          param:pSceImeKeyboardParam
          ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceImeKeyboardOpen:',userId,' ',HexStr(param));

 if (param=nil) then Exit(SCE_IME_ERROR_INVALID_ADDRESS);

 if ((param^.option and (not $1F))<>0) then Exit(SCE_IME_ERROR_INVALID_OPTION);
 if (param^.handler=nil) then Exit(SCE_IME_ERROR_INVALID_HANDLER);

 if not CAS(keyboard_init,0,1) then Exit(SCE_IME_ERROR_BUSY);

 g_hook:=SetWindowsHookExW(WH_KEYBOARD,@KeyboardHookCallback,GetModuleHandle(nil),MainThreadID);

 if (g_hook=0) then
 begin
  store_release(keyboard_init,0);
  Exit(SCE_IME_ERROR_INTERNAL);
 end;

 g_handler:=param^.handler;
 g_cb_arg :=param^.arg;

 push_keyboard_open(userId);

 store_release(keyboard_fini,1);

 Result:=0;
end;

function ps4_sceImeUpdate(handler:SceImeEventHandler):Integer; SysV_ABI_CDecl;
var
 i:Integer;
 event:SceImeEvent;
begin
 if (handler=nil) then Exit(SCE_IME_ERROR_INTERNAL);

 event:=Default(SceImeEvent);
 For i:=0 to 255 do
 begin
  if not g_ime_event_queue.dequeue(event) then Break;
  handler(g_cb_arg,@event);
 end;

 Result:=0;
end;

function ps4_sceImeKeyboardGetResourceId(userId:Integer;resourceIdArray:pSceImeKeyboardResourceIdArray):Integer; SysV_ABI_CDecl;
begin
 if (keyboard_init=0) then Exit(SCE_IME_ERROR_NOT_OPENED);
 if (resourceIdArray=nil) then Exit(SCE_IME_ERROR_INVALID_ADDRESS);

 resourceIdArray^:=Default(SceImeKeyboardResourceIdArray);
 resourceIdArray^.userId:=userId;
 resourceIdArray^.resourceId[0]:=1;
end;

function ps4_sceImeKeyboardGetInfo(resourceId:DWORD;info:pSceImeKeyboardInfo):Integer; SysV_ABI_CDecl;
begin
 if (keyboard_init=0) then Exit(SCE_IME_ERROR_NOT_OPENED);
 if (info=nil) then Exit(SCE_IME_ERROR_INVALID_ADDRESS);

 info^:=Default(SceImeKeyboardInfo);

 info^.userId:=-1;
 info^.device:=SCE_IME_KEYBOARD_DEVICE_TYPE_KEYBOARD;
 info^._type :=SCE_IME_KEYBOARD_TYPE_ENGLISH_US;
 info^.repeatDelay:=1;
 info^.repeatRate :=1;
 info^.status     :=CE_IME_KEYBOARD_STATE_CONNECTED;
end;

procedure init_ime;
begin
 g_ime_event_queue.Create(256);
end;

function Load_libSceIme(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceIme');
 lib^.set_proc($79A1578DF26FDF1B,@ps4_sceImeKeyboardOpen);
 lib^.set_proc($FF81827D874D175B,@ps4_sceImeUpdate);
 lib^.set_proc($74A69DA9916028A4,@ps4_sceImeKeyboardGetResourceId);
 lib^.set_proc($564A8B3C0ADF15D7,@ps4_sceImeKeyboardGetInfo);

 init_ime;
end;

initialization
 ps4_app.RegistredPreLoad('libSceIme.prx',@Load_libSceIme);

end.

