unit ime_types;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 ps4_libSceUserService;

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

 SCE_IME_DIALOG_ERROR_INVALID_TITLE        =-2135162623; // 0x80BC0101
 SCE_IME_DIALOG_ERROR_NOT_RUNNING          =-2135162619; // 0x80BC0105
 SCE_IME_DIALOG_ERROR_NOT_FINISHED         =-2135162618; // 0x80BC0106
 SCE_IME_DIALOG_ERROR_NOT_IN_USE           =-2135162617; // 0x80BC0107

 //Size of work buffer used by the Ime library
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
 SCE_IME_LANGUAGE_UKRAINIAN          =$0000000200000000;

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
 SCE_IME_KEYBOARD_STATE_DISCONNECTED = 0;
 SCE_IME_KEYBOARD_STATE_CONNECTED    = 1;

 //Keyboard layout types for keyboard devices
 //SceImeKeyboardType
 SCE_IME_KEYBOARD_TYPE_NONE             =  0;
 SCE_IME_KEYBOARD_TYPE_DANISH           =  1; //LANG_DANISH
 SCE_IME_KEYBOARD_TYPE_GERMAN           =  2; //LANG_GERMAN      else
 SCE_IME_KEYBOARD_TYPE_GERMAN_SW        =  3; //LANG_GERMAN      SUBLANG_GERMAN_SWISS
 SCE_IME_KEYBOARD_TYPE_ENGLISH_US       =  4; //LANG_ENGLISH     else
 SCE_IME_KEYBOARD_TYPE_ENGLISH_GB       =  5; //LANG_ENGLISH     SUBLANG_ENGLISH_UK
 SCE_IME_KEYBOARD_TYPE_SPANISH          =  6; //LANG_SPANISH     SUBLANG_SPANISH SUBLANG_SPANISH_MEXICAN SUBLANG_SPANISH_MODERN
 SCE_IME_KEYBOARD_TYPE_SPANISH_LA       =  7; //LANG_SPANISH     else
 SCE_IME_KEYBOARD_TYPE_FINNISH          =  8; //LANG_FINNISH
 SCE_IME_KEYBOARD_TYPE_FRENCH           =  9; //LANG_FRENCH      else
 SCE_IME_KEYBOARD_TYPE_FRENCH_BR        = 10; //LANG_FRENCH      SUBLANG_FRENCH_BELGIAN
 SCE_IME_KEYBOARD_TYPE_FRENCH_CA        = 11; //LANG_FRENCH      SUBLANG_FRENCH_CANADIAN
 SCE_IME_KEYBOARD_TYPE_FRENCH_SW        = 12; //LANG_FRENCH      SUBLANG_FRENCH_SWISS
 SCE_IME_KEYBOARD_TYPE_ITALIAN          = 13; //LANG_ITALIAN
 SCE_IME_KEYBOARD_TYPE_DUTCH            = 14; //LANG_DUTCH
 SCE_IME_KEYBOARD_TYPE_NORWEGIAN        = 15; //LANG_NORWEGIAN
 SCE_IME_KEYBOARD_TYPE_POLISH           = 16; //LANG_POLISH
 SCE_IME_KEYBOARD_TYPE_PORTUGUESE_BR    = 17; //LANG_PORTUGUESE  SUBLANG_PORTUGUESE_BRAZILIAN
 SCE_IME_KEYBOARD_TYPE_PORTUGUESE_PT    = 18; //LANG_PORTUGUESE  SUBLANG_PORTUGUESE
 SCE_IME_KEYBOARD_TYPE_RUSSIAN          = 19; //LANG_RUSSIAN
 SCE_IME_KEYBOARD_TYPE_SWEDISH          = 20; //LANG_SWEDISH
 SCE_IME_KEYBOARD_TYPE_TURKISH          = 21; //LANG_TURKISH
 SCE_IME_KEYBOARD_TYPE_JAPANESE_ROMAN   = 22; //LANG_JAPANESE
 SCE_IME_KEYBOARD_TYPE_JAPANESE_KANA    = 23; //LANG_JAPANESE    ?
 SCE_IME_KEYBOARD_TYPE_KOREAN           = 24; //LANG_KOREAN
 SCE_IME_KEYBOARD_TYPE_SM_CHINESE       = 25; //LANG_CHINESE     SUBLANG_CHINESE_SIMPLIFIED
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_ZY    = 26; //LANG_CHINESE     ?
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_HK = 27; //LANG_CHINESE     SUBLANG_CHINESE_HONGKONG
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_TW = 28; //LANG_CHINESE     ?
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_CG    = 29; //LANG_CHINESE     else
 SCE_IME_KEYBOARD_TYPE_ARABIC_AR        = 30; //LANG_ARABIC
 SCE_IME_KEYBOARD_TYPE_THAI             = 31; //LANG_THAI
 SCE_IME_KEYBOARD_TYPE_CZECH            = 32; //LANG_CZECH
 SCE_IME_KEYBOARD_TYPE_GREEK            = 33; //LANG_GREEK
 SCE_IME_KEYBOARD_TYPE_INDONESIAN       = 34; //LANG_INDONESIAN
 SCE_IME_KEYBOARD_TYPE_VIETNAMESE       = 35; //LANG_VIETNAMESE
 SCE_IME_KEYBOARD_TYPE_ROMANIAN         = 36; //LANG_ROMANIAN
 SCE_IME_KEYBOARD_TYPE_HUNGARIAN        = 37; //LANG_HUNGARIAN
 SCE_IME_KEYBOARD_TYPE_UKRAINIAN        = 38; //LANG_UKRAINIAN

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
 SceImeType                  =Integer;
 SceImeEnterLabel            =Integer;
 SceImeInputMethod           =Integer;
 SceImeHorizontalAlignment   =Integer;
 SceImeVerticalAlignment     =Integer;
 SceImePanelPriority         =Integer;
 SceImeKeyboardType          =Integer;
 SceImeKeyboardStatus        =Integer;
 SceImeTextAreaMode          =Integer;
 SceImeKeyboardDeviceType    =Integer;
 SceImePanelType             =Integer;
 SceImeCaretMovementDirection=Integer;
 SceImeDeviceType            =Integer;
 SceImeEventId               =Integer;
 SceImeDialogEndStatus       =Integer;

 SceImeTextAreaProperty=packed record
  mode  :SceImeTextAreaMode;
  index :DWORD;
  length:Integer;
 end;

 SceImeEditText=packed record
  str       :pWideChar;
  caretIndex:DWORD;
  areaNum   :DWORD;
  textArea  :array[0..SCE_IME_MAX_TEXT_AREA-1] of SceImeTextAreaProperty;
 end;

 pSceImePositionAndForm=^SceImePositionAndForm;
 SceImePositionAndForm=packed record
  PanelType          :SceImePanelType;
  posx               :Single;
  posy               :Single;
  horizontalAlignment:SceImeHorizontalAlignment;
  verticalAlignment  :SceImeVerticalAlignment;
  width              :DWORD;
  height             :DWORD;
 end;

 SceImeRect=packed record
  x     :Single;
  y     :Single;
  width :DWORD;
  height:DWORD;
 end;

 pSceImeTextGeometry=^SceImeTextGeometry;
 SceImeTextGeometry=packed record
  x     :Single;
  y     :Single;
  width :DWORD;
  height:DWORD;
 end;

 pSceImeCaret=^SceImeCaret;
 SceImeCaret=packed record
  x     :Single;
  y     :Single;
  height:DWORD;
  index :DWORD;
 end;

 SceImeColor=packed record
  r,g,b,a:Byte;
 end;

 pSceImeKeyboardInfo=^SceImeKeyboardInfo;
 SceImeKeyboardInfo=packed record
  userId     :SceUserServiceUserId;
  device     :SceImeKeyboardDeviceType;
  ktype      :SceImeKeyboardType;
  repeatDelay:DWORD;
  repeatRate :DWORD;
  status     :SceImeKeyboardStatus;
  reserved   :array[0..11] of Byte;
 end;

 pSceImeKeyboardResourceIdArray=^SceImeKeyboardResourceIdArray;
 SceImeKeyboardResourceIdArray=packed record
  userId    :SceUserServiceUserId;
  resourceId:array[0..SCE_IME_KEYBOARD_MAX_NUMBER-1] of DWORD;
 end;

 pSceImeKeycode=^SceImeKeycode;
 SceImeKeycode=packed record
  keycode   :Word;
  character :WideChar;
  status    :DWORD;
  ktype     :SceImeKeyboardType;
  userId    :SceUserServiceUserId;
  resourceId:DWORD;
  _align    :Integer;
  timestamp :QWORD; //SceRtcTick
 end;

 pSceImeEventParam=^SceImeEventParam;
 SceImeEventParam=packed record
  Case Byte of
   0:(rect            :SceImeRect);
   1:(text            :SceImeEditText);
   2:(caretMove       :SceImeCaretMovementDirection);
   3:(keycode         :SceImeKeycode);
   4:(resourceIdArray :SceImeKeyboardResourceIdArray);
   5:(candidateWord   :PWideChar);
   6:(candidateIndex  :Integer);
   7:(deviceType      :SceImeDeviceType);
   8:(panelType       :SceImePanelType);
   9:(inputMethodState:DWORD); //SCE_IME_INPUT_METHOD_STATE
  10:(reserved        :array[0..63] of Byte);
 end;

 pSceImeEvent=^SceImeEvent;
 SceImeEvent=packed record
  id    :SceImeEventId;
  _align:Integer;
  param :SceImeEventParam;
 end;

 SceImeTextFilter=function(
  outText      :PWideChar;
  outTextLength:PDWORD;
  srcText      :PWideChar;
  srcTextLength:DWORD
 ):Integer;


 SceImeExtKeyboardFilter=function(
  srcKeycode:pSceImeKeycode;
  outKeycode:PWord;
  outStatus :PDWORD;
  reserved  :Pointer
 ):Integer;

 SceImeEventHandler=procedure(arg:Pointer;e:pSceImeEvent);

 pSceImeParam=^SceImeParam;
 SceImeParam=packed record
  userId             :SceUserServiceUserId;
  ImeType            :SceImeType;
  supportedLanguages :QWORD;
  enterLabel         :SceImeEnterLabel;
  inputMethod        :SceImeInputMethod;
  filter             :SceImeTextFilter;
  option             :DWORD; // SCE_IME_OPTION
  maxTextLength      :DWORD;
  inputTextBuffer    :PWideChar;
  posx               :Single;
  posy               :Single;
  horizontalAlignment:SceImeHorizontalAlignment;
  verticalAlignment  :SceImeVerticalAlignment;
  work               :Pointer;
  arg                :Pointer;
  handler            :SceImeEventHandler;
  reserved           :QWORD;
 end;

 pSceImeParamExtended=^SceImeParamExtended;
 SceImeParamExtended=packed record
  option                  :DWORD; // SCE_IME_EXT_OPTION
  colorBase               :SceImeColor;
  colorLine               :SceImeColor;
  colorTextField          :SceImeColor;
  colorPreedit            :SceImeColor;
  colorButtonDefault      :SceImeColor;
  colorButtonFunction     :SceImeColor;
  colorButtonSymbol       :SceImeColor;
  colorText               :SceImeColor;
  colorSpecial            :SceImeColor;
  priority                :SceImePanelPriority;
  _align                  :Integer;
  additionalDictionaryPath:PChar;
  extKeyboardFilter       :SceImeExtKeyboardFilter;
  disableDevice           :DWORD; // SCE_IME_DISABLE_DEVICE
  extKeyboardMode         :DWORD; // SCE_IME_INIT_EXT_KEYBOARD_MODE
  reserved                :array[0..59] of Byte;
 end;

 pSceImeDialogParam=^SceImeDialogParam;
 SceImeDialogParam=packed record
  userId             :SceUserServiceUserId;
  ImeType            :SceImeType;
  supportedLanguages :QWORD;
  enterLabel         :SceImeEnterLabel;
  inputMethod        :SceImeInputMethod;
  filter             :SceImeTextFilter;
  option             :DWORD; // SCE_IME_OPTION
  maxTextLength      :DWORD;
  inputTextBuffer    :PWideChar;
  posx               :Single;
  posy               :Single;
  horizontalAlignment:SceImeHorizontalAlignment;
  verticalAlignment  :SceImeVerticalAlignment;
  placeholder        :PWideChar;
  title              :PWideChar;
  reserved           :array[0..15] of Byte;
 end;

 pSceImeDialogResult=^SceImeDialogResult;
 SceImeDialogResult=packed record
  endstatus:SceImeDialogEndStatus;
  reserved :array[0..11] of Byte;
 end;

 pSceImeKeyboardParam=^SceImeKeyboardParam;
 SceImeKeyboardParam=packed record
  option   :DWORD;
  reserved1:DWORD;
  arg      :Pointer;
  handler  :SceImeEventHandler;
  reserved2:QWORD;
 end;

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

end.

