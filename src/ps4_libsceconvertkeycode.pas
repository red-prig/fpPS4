unit ps4_libSceConvertKeycode;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

const
 SCE_IME_KEYBOARD_TYPE_NONE            =0;
 SCE_IME_KEYBOARD_TYPE_DANISH          =1;
 SCE_IME_KEYBOARD_TYPE_GERMAN          =2;
 SCE_IME_KEYBOARD_TYPE_GERMAN_SW       =3;
 SCE_IME_KEYBOARD_TYPE_ENGLISH_US      =4;
 SCE_IME_KEYBOARD_TYPE_ENGLISH_GB      =5;
 SCE_IME_KEYBOARD_TYPE_SPANISH         =6;
 SCE_IME_KEYBOARD_TYPE_SPANISH_LA      =7;
 SCE_IME_KEYBOARD_TYPE_FINNISH         =8;
 SCE_IME_KEYBOARD_TYPE_FRENCH          =9;
 SCE_IME_KEYBOARD_TYPE_FRENCH_BR       =10;
 SCE_IME_KEYBOARD_TYPE_FRENCH_CA       =11;
 SCE_IME_KEYBOARD_TYPE_FRENCH_SW       =12;
 SCE_IME_KEYBOARD_TYPE_ITALIAN         =13;
 SCE_IME_KEYBOARD_TYPE_DUTCH           =14;
 SCE_IME_KEYBOARD_TYPE_NORWEGIAN       =15;
 SCE_IME_KEYBOARD_TYPE_POLISH          =16;
 SCE_IME_KEYBOARD_TYPE_PORTUGUESE_BR   =17;
 SCE_IME_KEYBOARD_TYPE_PORTUGUESE_PT   =18;
 SCE_IME_KEYBOARD_TYPE_RUSSIAN         =19;
 SCE_IME_KEYBOARD_TYPE_SWEDISH         =20;
 SCE_IME_KEYBOARD_TYPE_TURKISH         =21;
 SCE_IME_KEYBOARD_TYPE_JAPANESE_ROMAN  =22;
 SCE_IME_KEYBOARD_TYPE_JAPANESE_KANA   =23;
 SCE_IME_KEYBOARD_TYPE_KOREAN          =24;
 SCE_IME_KEYBOARD_TYPE_SM_CHINESE      =25;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_ZY   =26;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_HK=27;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_PY_TW=28;
 SCE_IME_KEYBOARD_TYPE_TR_CHINESE_CG   =29;
 SCE_IME_KEYBOARD_TYPE_ARABIC_AR       =30;
 SCE_IME_KEYBOARD_TYPE_THAI            =31;
 SCE_IME_KEYBOARD_TYPE_CZECH           =32;
 SCE_IME_KEYBOARD_TYPE_GREEK           =33;
 SCE_IME_KEYBOARD_TYPE_INDONESIAN      =34;
 SCE_IME_KEYBOARD_TYPE_VIETNAMESE      =35;
 SCE_IME_KEYBOARD_TYPE_ROMANIAN        =36;
 SCE_IME_KEYBOARD_TYPE_HUNGARIAN       =37;

type
 pSceImeKeyboardType=^SceImeKeyboardType;
 SceImeKeyboardType=Integer;

implementation

function ps4_sceConvertKeycodeGetVirtualKeycode(keycode:Word;
                                                keyboardType:SceImeKeyboardType;
                                                vkeycode:PWord):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceConvertKeycode(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceConvertKeycode');
 lib^.set_proc($BC8B2826C2EFBE53,@ps4_sceConvertKeycodeGetVirtualKeycode);
end;

initialization
 ps4_app.RegistredPreLoad('libSceConvertKeycode.prx',@Load_libSceConvertKeycode);

end.

