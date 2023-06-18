unit kbm_pad_interface;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 sysutils,
 spinlock,
 sce_pad_types,
 sce_pad_interface;

type
 TKbmPadHandle=class(TScePadHandle)
  function ReadState(data:PScePadData):Integer; override;
 end;

 TKbmPadInterface=class(TScePadInterface)
  class function Open(index:Integer;var handle:TScePadHandle):Integer; override;
 end;

 TMouseAsTouchpad=class
  class var
   last_mouse_lock :Pointer;
   last_mouse_point:TPoint;
   last_mouse_init :Integer;
  class function ReadState(data:PScePadData):Integer;
 end;

implementation

class function TKbmPadInterface.Open(index:Integer;var handle:TScePadHandle):Integer;
begin
 Result:=0;
 if (index<0) or (index>15) then Exit(SCE_PAD_ERROR_INVALID_ARG);
 if (pad_opened[index]<>nil) then Exit(SCE_PAD_ERROR_ALREADY_OPENED);

 handle:=TKbmPadHandle.Create;
 TKbmPadHandle(handle).index:=index;

 pad_opened[index]:=handle;
end;

class function TMouseAsTouchpad.ReadState(data:PScePadData):Integer;
var
 mPoint,delta:TPoint;
begin
 Result:=0;

 //mouse as touch pad

 spin_lock(last_mouse_lock);

 GetCursorPos(mPoint);

  if (last_mouse_init=0) then
  begin
   last_mouse_init :=1;
   last_mouse_point:=mPoint;
  end else
  if QWORD(mPoint)<>QWORD(last_mouse_point) then
  begin
   data^.touchData.touchNum:=1;
   data^.touchData.touch[0].id:=0;

   delta:=mPoint;

   if (delta.X<0) then delta.X:=0;
   if (delta.Y<0) then delta.Y:=0;

   if (delta.X>1919) then delta.X:=1919;
   if (delta.Y>941)  then delta.Y:=941;

   data^.touchData.touch[0].x:=delta.X;
   data^.touchData.touch[0].y:=delta.Y;

   last_mouse_point:=mPoint;
  end;

 spin_unlock(last_mouse_lock);
end;

function GetAsyncKeyState(vKey:longint):Boolean; inline;
begin
 Result:=(Windows.GetKeyState(vKey) and $8000)<>0;
end;

function TKbmPadHandle.ReadState(data:PScePadData):Integer;
begin
 Result:=0;

 TMouseAsTouchpad.ReadState(data);

 //keymapping

 if GetAsyncKeyState(VK_W) then
  data^.leftStick.y:=0;

 if GetAsyncKeyState(VK_S) then
  data^.leftStick.y:=$FF;

 if GetAsyncKeyState(VK_A) then
  data^.leftStick.x:=0;

 if GetAsyncKeyState(VK_D) then
  data^.leftStick.x:=$FF;

 //

 if GetAsyncKeyState(VK_I) then
  data^.rightStick.y:=0;

 if GetAsyncKeyState(VK_K) then
  data^.rightStick.y:=$FF;

 if GetAsyncKeyState(VK_J) then
  data^.rightStick.x:=0;

 if GetAsyncKeyState(VK_L) then
  data^.rightStick.x:=$FF;

 //

 if GetAsyncKeyState(VK_LBUTTON) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TOUCH_PAD;

 if GetAsyncKeyState(VK_RETURN) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_OPTIONS;

 if GetAsyncKeyState(VK_UP) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_UP;

 if GetAsyncKeyState(VK_RIGHT) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_RIGHT;

 if GetAsyncKeyState(VK_DOWN) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_DOWN;

 if GetAsyncKeyState(VK_LEFT) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_LEFT;

 if GetAsyncKeyState(VK_NUMPAD8) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_TRIANGLE;

 if GetAsyncKeyState(VK_NUMPAD6) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CIRCLE;

 if GetAsyncKeyState(VK_NUMPAD2) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_CROSS;

 if GetAsyncKeyState(VK_NUMPAD4) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_SQUARE;

 if GetAsyncKeyState(VK_Q) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L1;

 if GetAsyncKeyState(VK_1) then
 begin
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L2;
  data^.analogButtons.l2:=255;
 end;

 if GetAsyncKeyState(VK_Z) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_L3;


 if GetAsyncKeyState(VK_E) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R1;

 if GetAsyncKeyState(VK_4) then
 begin
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R2;
  data^.analogButtons.r2:=255;
 end;

 if GetAsyncKeyState(VK_C) then
  data^.buttons:=data^.buttons or SCE_PAD_BUTTON_R3;

end;

end.

