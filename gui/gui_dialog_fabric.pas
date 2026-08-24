unit gui_dialog_fabric;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Graphics,
  Buttons,
  LCLType,
  LCLIntf,
  host_ipc;

type
 TDialogButtonsType=(
  btnOk,
  btnYesNo,
  btnOkCancel,
  btnCancel,
  btnNoYes,
  btnCancelYes,
  btn2Buttons,
  btnPurchaseCancel
 );

 TDialogButtonId=(
  btnIdCancel,
  btnIdOkYesBtn1,
  btnIdNoBtn2
 );

 PImeDialogAttributes=^TImeDialogAttributes;
 TImeDialogAttributes=record
  Multiline  :Boolean;
  Password   :Boolean;
  FixedPos   :Boolean;
  Over2kCoord:Boolean;
  NumbersOnly:Boolean;
  hAlign     :TAnchorSideReference;
  vAlign     :TAnchorSideReference;
  MaxLength  :DWORD;
  posx       :Single;
  posy       :Single;
  width      :Single;
  height     :Single;
  EditLabel  :RawByteString;
 end;

 TDialogAttributes=record
  AParent:TForm;
  AImages:TImageList;
  Caption:record
   Enable :Boolean;
   Message:RawByteString;
  end;
  CloseButton:record
   Enable:Boolean;
   btnId :TDialogButtonId;
  end;
  Custom:TWinControl;
  Memo:record
   Enable :Boolean;
   Message:RawByteString;
   Ime    :PImeDialogAttributes;
  end;
  ProgressBar:record
   Enable :Boolean;
  end;
  Buttons:record
   Enable :Boolean;
   BtnType:TDialogButtonsType;
   BtnMsg :array[0..1] of RawByteString;
  end;
  OnClick:TNotifyEvent;
  ALittleMore:Boolean;
 end;

 TDialogCustom=class(TPanel)
  public
   FClient :THostIpc;
   FMsgMemo:TMemo;
   FMsgPBar:TProgressBar;
   FCustom :TWinControl;
  public
   state :Byte;
   button:Byte;
  Destructor Destroy; override;
 end;

 TImeDialog=class(TDialogCustom)
  private
   procedure SetInfo(Ime:PImeDialogAttributes);
   procedure DoResize(Sender:TObject);
   procedure DoMouseMoveEvent(Sender:TObject;Shift:TShiftState;X,Y:Integer);
   procedure DoMouseDown(Sender:TObject;Btn:TMouseButton;Shift:TShiftState;X,Y:Integer);
   procedure DoMouseUp  (Sender:TObject;Btn:TMouseButton;Shift:TShiftState;X,Y:Integer);
  public
   Multiline  :Boolean;
   FixedPos   :Boolean;
   Over2kCoord:Boolean;
   IsMoved    :Boolean;
   hAlign     :TAnchorSideReference;
   vAlign     :TAnchorSideReference;
   Fposx      :Single;
   Fposy      :Single;
   Fwidth     :Single;
   Fheight    :Single;
   FLastMove  :TPoint;
   //
   function GetPosX:Single;
   function GetPosY:Single;
   function GetVirtualWidth:Single;
   function GetVirtualHeight:Single;
 end;

function NewDialogOpen(var Attributes:TDialogAttributes):TDialogCustom;
function GetAnchor(Align:Byte):TAnchorSideReference; inline;
function GetAlign(Side:TAnchorSideReference):Byte; inline;

implementation

uses
 MsgDlgExt;

function GetAnchor(Align:Byte):TAnchorSideReference; inline;
begin
 Result:=asrTop;
 case Align of
  0:Result:=asrTop;    // LEFT/TOP
  1:Result:=asrCenter; // CENTER
  2:Result:=asrBottom; // RIGHT/BOTTOM
 end;
end;

function GetAlign(Side:TAnchorSideReference):Byte; inline;
begin
 Result:=0;
 case Side of
  asrTop   :Result:=0; // LEFT/TOP
  asrCenter:Result:=1; // CENTER
  asrBottom:Result:=2; // RIGHT/BOTTOM
 end;
end;

Destructor TDialogCustom.Destroy;
begin
 FreeAndNil(FCustom);
 inherited;
end;

function NewBtn(MsgForm:TWinControl;DlgPos:TAnchorSideReference;const Caption:RawByteString;ModalResult:Integer;OnClick:TNotifyEvent):TButton;
var
 MsgBtnz:TButton;
begin
 MsgBtnz:=TButton.Create(MsgForm);

 case DlgPos of
  asrTop:
    begin
     MsgBtnz.Anchors:=[akLeft,akBottom];
     MsgBtnz.AnchorSide[akLeft  ].Control:=MsgForm;
     MsgBtnz.AnchorSide[akLeft  ].Side   :=asrTop;
     MsgBtnz.AnchorSide[akBottom].Control:=MsgForm;
     MsgBtnz.AnchorSide[akBottom].Side   :=asrBottom;
    end;
  asrBottom:
    begin
     MsgBtnz.Anchors:=[akRight,akBottom];
     MsgBtnz.AnchorSide[akRight ].Control:=MsgForm;
     MsgBtnz.AnchorSide[akRight ].Side   :=asrBottom;
     MsgBtnz.AnchorSide[akBottom].Control:=MsgForm;
     MsgBtnz.AnchorSide[akBottom].Side   :=asrBottom;
    end;
  asrCenter:
    begin
     MsgBtnz.Anchors:=[akLeft,akBottom];
     MsgBtnz.AnchorSide[akLeft  ].Control:=MsgForm;
     MsgBtnz.AnchorSide[akLeft  ].Side   :=asrCenter;
     MsgBtnz.AnchorSide[akBottom].Control:=MsgForm;
     MsgBtnz.AnchorSide[akBottom].Side   :=asrBottom;
    end;
 end;

 MsgBtnz.BorderSpacing.Around :=10;
 MsgBtnz.Constraints.MinHeight:=25;
 MsgBtnz.Constraints.MinWidth :=75;
 MsgBtnz.AutoSize   :=True;
 MsgBtnz.Caption    :=Caption;
 MsgBtnz.Parent     :=MsgForm;
 MsgBtnz.Tag        :=ModalResult;
 MsgBtnz.ModalResult:=ModalResult;
 MsgBtnz.OnClick    :=OnClick;

 Result:=MsgBtnz;
end;

//

procedure TImeDialog.SetInfo(Ime:PImeDialogAttributes);
begin
 state      :=1;
 //
 Multiline  :=Ime^.Multiline;
 FixedPos   :=Ime^.FixedPos;
 Over2kCoord:=Ime^.Over2kCoord;
 hAlign     :=Ime^.hAlign;
 vAlign     :=Ime^.vAlign;
 Fposx      :=Ime^.posx;
 Fposy      :=Ime^.posy;
 Fwidth     :=Ime^.width;
 Fheight    :=Ime^.height;
 //

 //reposition
 case hAlign of
  asrBottom:Fposx:=Fposx-Fwidth;
  asrCenter:Fposx:=Fposx-(Fwidth/2);
  else;
 end;

 //reposition
 case vAlign of
  asrBottom:Fposy:=Fposy-Fheight;
  asrCenter:Fposy:=Fposy-(Fheight/2);
  else;
 end;

 if (Fposx<0) then Fposx:=0;
 if (Fposy<0) then Fposy:=0;

 //fixup
 if ((Fposx+Fwidth)>GetVirtualWidth) then
 begin
  Fposx:=GetVirtualWidth-Fwidth;
 end;

 //fixup
 if ((Fposy+Fheight)>GetVirtualHeight) then
 begin
  Fposy:=GetVirtualHeight-Fheight;
 end;

 //
 OnResize   :=@DoResize;
 OnMouseDown:=@DoMouseDown;
 OnMouseUp  :=@DoMouseUp;
 OnMouseMove:=@DoMouseMoveEvent;
end;

function TImeDialog.GetPosX:Single;
begin
 Result:=Fposx;
 case hAlign of
  asrBottom:Result:=Result+Fwidth;
  asrCenter:Result:=Result+(Fwidth/2);
  else;
 end;
end;

function TImeDialog.GetPosY:Single;
begin
 Result:=Fposy;
 case vAlign of
  asrBottom:Result:=Result+Fheight;
  asrCenter:Result:=Result+(Fheight/2);
  else;
 end;
end;

function TImeDialog.GetVirtualWidth:Single;
begin
 if Over2kCoord then
 begin
  Result:=3840.0;
 end else
 begin
  Result:=1920.0;
 end;
end;

function TImeDialog.GetVirtualHeight:Single;
begin
 if Over2kCoord then
 begin
  Result:=2160.0;
 end else
 begin
  Result:=1080.0;
 end;
end;

procedure TImeDialog.DoResize(Sender:TObject);
var
 VRect:TRect;
begin
 //
 VRect.Left  :=Trunc((Fposx  /GetVirtualWidth )*Parent.Width );
 VRect.Top   :=Trunc((Fposy  /GetVirtualHeight)*Parent.Height);
 VRect.Width :=Trunc((FWidth /GetVirtualWidth )*Parent.Width );
 VRect.Height:=Trunc((FHeight/GetVirtualHeight)*Parent.Height);
 //
 BoundsRect:=VRect;
end;

procedure TImeDialog.DoMouseMoveEvent(Sender:TObject;Shift:TShiftState;X,Y:Integer);
var
 new,diff:TPoint;
begin
 if IsMoved then
 begin
  new:=Mouse.CursorPos;

  diff:=new.Subtract(FLastMove);
  FLastMove:=new;
  //
  new.X:=Left+diff.X;
  new.Y:=Top +diff.Y;

  if (new.X<0) then Exit;
  if (new.Y<0) then Exit;

  if (new.X+Width >Parent.Width ) then Exit;
  if (new.Y+Height>Parent.Height) then Exit;

  //
  Fposx:=(new.X/Parent.Width )*GetVirtualWidth;
  Fposy:=(new.Y/Parent.Height)*GetVirtualHeight;
  Left :=new.X;
  Top  :=new.Y;
 end;
end;

procedure TImeDialog.DoMouseDown(Sender:TObject;Btn:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if (not FixedPos) and (Btn=mbLeft) then
 begin
  FLastMove:=Mouse.CursorPos;
  IsMoved:=True;
 end;
end;

procedure TImeDialog.DoMouseUp(Sender:TObject;Btn:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 IsMoved:=False;
end;

function NewDialogOpen(var Attributes:TDialogAttributes):TDialogCustom;
var
 AParent:TForm;
 AImages:TImageList;
 MsgForm:TDialogCustom;
 MsgFTop:TPanel;
 MsgBody:TPanel;
 MCenter:TWinControl;
 MsgCapt:TLabel;
 MsgBtnz:TButton;
 MsgMemo:TMemo;
 MsgPBar:TProgressBar;
 MsgCncl:TSpeedButton;

 //Top           [Caption - X]
 //Custom        [           ]
 //Memo          [   Body    ]
 //ProgressBar   [           ]
 //Buttons       [  Y     N  ]

begin
 Result :=nil;
 AParent:=Attributes.AParent;
 AImages:=Attributes.AImages;

 MsgFTop:=nil;
 MsgBody:=nil;
 MCenter:=nil;
 MsgCapt:=nil;
 MsgBtnz:=nil;
 MsgMemo:=nil;
 MsgPBar:=nil;
 MsgCncl:=nil;

 if (Attributes.Memo.Ime<>nil) then
 begin
  MsgForm:=TImeDialog.Create(nil);
  //
  TImeDialog(MsgForm).SetInfo(Attributes.Memo.Ime);
 end else
 begin
  MsgForm:=TDialogCustom.Create(nil);
 end;

 try
  MsgForm.ParentBackground:=False;
  MsgForm.Anchors:=[akTop,akLeft,akRight,akBottom];
  MsgForm.AnchorSide[akTop   ].Control:=AParent;
  MsgForm.AnchorSide[akTop   ].Side   :=asrCenter;
  MsgForm.AnchorSide[akLeft  ].Control:=AParent;
  MsgForm.AnchorSide[akLeft  ].Side   :=asrCenter;
  MsgForm.AnchorSide[akRight ].Control:=AParent;
  MsgForm.AnchorSide[akRight ].Side   :=asrCenter;
  MsgForm.AnchorSide[akBottom].Control:=AParent;
  MsgForm.AnchorSide[akBottom].Side   :=asrCenter;
  MsgForm.Width :=400 + 200;
  MsgForm.Height:=200 + 200;

  if (Attributes.ALittleMore) then
  begin
   MsgForm.Width :=MsgForm.Width  + (MsgForm.Width  div 2);
   MsgForm.Height:=MsgForm.Height + (MsgForm.Height div 2);
  end;

  if (Attributes.Memo.Ime<>nil) then
  begin
   MsgForm.Anchors:=[];
  end;

  if Attributes.Caption.Enable or Attributes.CloseButton.Enable then
  begin
   MsgFTop:=TPanel.Create(MsgForm);
   MsgFTop.BorderStyle:=bsNone;
   MsgFTop.AutoSize:=True;
   MsgFTop.Anchors:=[akTop,akLeft,akRight];
   MsgFTop.AnchorSide[akTop   ].Control:=MsgForm;
   MsgFTop.AnchorSide[akTop   ].Side   :=asrTop;
   MsgFTop.AnchorSide[akLeft  ].Control:=MsgForm;
   MsgFTop.AnchorSide[akLeft  ].Side   :=asrTop;
   MsgFTop.AnchorSide[akRight ].Control:=MsgForm;
   MsgFTop.AnchorSide[akRight ].Side   :=asrBottom;
   MsgFTop.Parent:=MsgForm;
  end;

  if Attributes.Caption.Enable then
  begin
   MsgCapt:=TLabel.Create(MsgForm);
   MsgCapt.AutoSize:=True;
   MsgCapt.Font.Name:='Courier New';
   MsgCapt.Font.Size:=GetRealFontSize(AParent.Font) + 2 + 2;
   MsgCapt.Font.Bold:=True;
   MsgCapt.BorderSpacing.Left:=2;
   MsgCapt.BorderSpacing.Around:=2;
   MsgCapt.Anchors:=[akTop,akLeft];
   MsgCapt.AnchorSide[akTop ].Control:=MsgFTop;
   MsgCapt.AnchorSide[akTop ].Side   :=asrCenter;
   MsgCapt.AnchorSide[akLeft].Control:=MsgFTop;
   MsgCapt.AnchorSide[akLeft].Side   :=asrTop;
   MsgCapt.Parent :=MsgFTop;
   //
   MsgCapt.Caption:=Attributes.Caption.Message;
  end;

  if Attributes.CloseButton.Enable then
  begin
   MsgCncl:=TSpeedButton.Create(MsgFTop);
   MsgCncl.AutoSize:=True;
   MsgCncl.Images:=AImages;
   MsgCncl.ImageIndex:=3;
   MsgCncl.Anchors:=[akTop,akRight];
   MsgCncl.AnchorSide[akTop  ].Control:=MsgFTop;
   MsgCncl.AnchorSide[akTop  ].Side   :=asrCenter;
   MsgCncl.AnchorSide[akRight].Control:=MsgFTop;
   MsgCncl.AnchorSide[akRight].Side   :=asrBottom;
   MsgCncl.Tag    :=ord(Attributes.CloseButton.btnId);
   MsgCncl.OnClick:=Attributes.OnClick;
   MsgCncl.Parent :=MsgFTop;
  end;

  //body
  MsgBody:=TPanel.Create(MsgForm);
  MsgBody.BorderStyle:=bsNone;
  MsgBody.AutoSize:=True;
  MsgBody.Anchors:=[akTop,akLeft,akRight,akBottom];
  MsgBody.AnchorSide[akTop   ].Control:=MsgForm;
  MsgBody.AnchorSide[akTop   ].Side   :=asrTop;
  MsgBody.AnchorSide[akLeft  ].Control:=MsgForm;
  MsgBody.AnchorSide[akLeft  ].Side   :=asrTop;
  MsgBody.AnchorSide[akRight ].Control:=MsgForm;
  MsgBody.AnchorSide[akRight ].Side   :=asrBottom;
  MsgBody.AnchorSide[akBottom].Control:=MsgForm;
  MsgBody.AnchorSide[akBottom].Side   :=asrBottom;
  MsgBody.Parent:=MsgForm;
  //body

  if Attributes.Memo.Enable then
  begin
   MsgMemo:=TMemo.Create(MsgBody);
   MsgMemo.ReadOnly:=True;
   MsgMemo.Alignment:=taCenter;
   MsgMemo.Font.Name:='Courier New';
   MsgMemo.Font.Size:=GetRealFontSize(AParent.Font) + 2;
   MsgMemo.Anchors:=[akTop,akLeft,akRight,akBottom];
   MsgMemo.AnchorSide[akTop   ].Control:=MsgBody;
   MsgMemo.AnchorSide[akTop   ].Side   :=asrTop;
   MsgMemo.AnchorSide[akLeft  ].Control:=MsgBody;
   MsgMemo.AnchorSide[akLeft  ].Side   :=asrTop;
   MsgMemo.AnchorSide[akRight ].Control:=MsgBody;
   MsgMemo.AnchorSide[akRight ].Side   :=asrBottom;
   MsgMemo.AnchorSide[akBottom].Control:=MsgBody;
   MsgMemo.AnchorSide[akBottom].Side   :=asrBottom;
   MsgMemo.Text:=Attributes.Memo.Message;
   MsgMemo.Parent:=MsgBody;
   //
   MCenter:=MsgMemo;
   //
   if (Attributes.Memo.Ime<>nil) then
   begin
    MsgMemo.ReadOnly :=False;
    MsgMemo.Alignment:=taLeftJustify;
    //
    if not Attributes.Memo.Ime^.Multiline then
    begin
     MsgMemo.ScrollBars :=ssNone;
     MsgMemo.WantReturns:=False;
     MsgMemo.WordWrap   :=False;
    end;
    if Attributes.Memo.Ime^.Password then
    begin
     MsgMemo.PasswordChar:='*';
    end;
    MsgMemo.NumbersOnly:=Attributes.Memo.Ime^.NumbersOnly;
    MsgMemo.MaxLength  :=Attributes.Memo.Ime^.MaxLength;
   end;
   //
  end;

  if (Attributes.Custom<>nil) then
  begin
   Attributes.Custom.Parent:=MsgBody;
   //
   if (MCenter=nil) then
   begin
    Attributes.Custom.Anchors:=[akTop,akLeft,akRight,akBottom];
    Attributes.Custom.AnchorSide[akTop   ].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akTop   ].Side   :=asrTop;
    Attributes.Custom.AnchorSide[akLeft  ].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akLeft  ].Side   :=asrTop;
    Attributes.Custom.AnchorSide[akRight ].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akRight ].Side   :=asrBottom;
    Attributes.Custom.AnchorSide[akBottom].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akBottom].Side   :=asrBottom;
    //
    MCenter:=Attributes.Custom;
   end else
   begin
    Attributes.Custom.Anchors:=[akTop,akLeft,akRight];
    Attributes.Custom.AnchorSide[akTop  ].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akTop  ].Side   :=asrTop;
    Attributes.Custom.AnchorSide[akLeft ].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akLeft ].Side   :=asrTop;
    Attributes.Custom.AnchorSide[akRight].Control:=MsgBody;
    Attributes.Custom.AnchorSide[akRight].Side   :=asrBottom;
    //
    MCenter.BorderSpacing.Top:=5;
    MCenter.AnchorSide[akTop].Control:=Attributes.Custom;
    MCenter.AnchorSide[akTop].Side   :=asrBottom;
   end;
  end;

  if Attributes.ProgressBar.Enable then
  begin
   MsgPBar:=TProgressBar.Create(MsgBody);
   MsgPBar.Min:=0;
   MsgPBar.Max:=100;
   MsgPBar.Position:=0;
   MsgPBar.Smooth:=True;
   MsgPBar.BorderSpacing.Around:=5;
   MsgPBar.Anchors:=[akLeft,akRight,akBottom];
   MsgPBar.AnchorSide[akLeft  ].Control:=MsgBody;
   MsgPBar.AnchorSide[akLeft  ].Side   :=asrTop;
   MsgPBar.AnchorSide[akRight ].Control:=MsgBody;
   MsgPBar.AnchorSide[akRight ].Side   :=asrBottom;
   MsgPBar.AnchorSide[akBottom].Control:=MsgBody;
   MsgPBar.AnchorSide[akBottom].Side   :=asrBottom;
   MsgPBar.Parent:=MsgBody;
   //
   if (MCenter<>nil) then
   begin
    MCenter.AnchorSide[akBottom].Control:=MsgPBar;
    MCenter.AnchorSide[akBottom].Side   :=asrTop;
    MCenter.BorderSpacing.Bottom:=10;
   end;
  end;

  if (Attributes.Memo.Ime<>nil) then
  begin
   MsgBtnz:=NewBtn(MsgForm,asrBottom,'&'+Attributes.Memo.Ime^.EditLabel,ord(btnIdOkYesBtn1),Attributes.OnClick);
  end else
  if Attributes.Buttons.Enable then
  case Attributes.Buttons.BtnType of
   btnOk:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrCenter,'&OK' ,ord(btnIdOkYesBtn1),Attributes.OnClick);
    end;
   btnYesNo:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&Yes',ord(btnIdOkYesBtn1),Attributes.OnClick);
     MsgBtnz:=NewBtn(MsgForm,asrBottom,'&No' ,ord(btnIdNoBtn2)   ,Attributes.OnClick);
    end;
   btnOkCancel:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&OK'    ,ord(btnIdOkYesBtn1),Attributes.OnClick);
     MsgBtnz:=NewBtn(MsgForm,asrBottom,'&Cancel',ord(btnIdCancel)   ,Attributes.OnClick);
    end;
   btnCancel:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrCenter,'&Cancel',ord(btnIdCancel),Attributes.OnClick);
    end;
   btnNoYes:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&No' ,ord(btnIdNoBtn2)   ,Attributes.OnClick);
     MsgBtnz:=NewBtn(MsgForm,asrBottom,'&Yes',ord(btnIdOkYesBtn1),Attributes.OnClick);
    end;
   btnCancelYes:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&Cancel',ord(btnIdCancel)   ,Attributes.OnClick);
     MsgBtnz:=NewBtn(MsgForm,asrBottom,'&OK'    ,ord(btnIdOkYesBtn1),Attributes.OnClick);
    end;
   btn2Buttons:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&'+Attributes.Buttons.BtnMsg[0],ord(btnIdOkYesBtn1),Attributes.OnClick);
     MsgBtnz:=NewBtn(MsgForm,asrBottom,'&'+Attributes.Buttons.BtnMsg[1],ord(btnIdNoBtn2)   ,Attributes.OnClick);
    end;
   btnPurchaseCancel:
    begin
     MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&Purchase',ord(btnIdOkYesBtn1),Attributes.OnClick);
     MsgBtnz:=NewBtn(MsgForm,asrBottom,'&Cancel'  ,ord(btnIdCancel)   ,Attributes.OnClick);
    end;
   else;
  end;

  //MsgFTop
  //MsgBody
  //MsgBtnz

  if (MsgFTop<>nil) then
  begin
   MsgBody.AnchorSide[akTop].Control:=MsgFTop;
   MsgBody.AnchorSide[akTop].Side   :=asrBottom;

   if (Attributes.Memo.Ime<>nil) then
   begin
    MsgFTop.OnMouseDown:=@TImeDialog(MsgForm).DoMouseDown;
    MsgFTop.OnMouseUp  :=@TImeDialog(MsgForm).DoMouseUp;
    MsgFTop.OnMouseMove:=@TImeDialog(MsgForm).DoMouseMoveEvent;
   end;

   if (MsgCapt<>nil) then
   if (Attributes.Memo.Ime<>nil) then
   begin
    MsgCapt.OnMouseDown:=@TImeDialog(MsgForm).DoMouseDown;
    MsgCapt.OnMouseUp  :=@TImeDialog(MsgForm).DoMouseUp;
    MsgCapt.OnMouseMove:=@TImeDialog(MsgForm).DoMouseMoveEvent;
   end;

  end;

  if (MsgBtnz<>nil) then
  begin
   MsgBody.AnchorSide[akBottom].Control:=MsgBtnz;
   MsgBody.AnchorSide[akBottom].Side   :=asrTop;
  end;

  MsgForm.Parent:=AParent;
  MsgForm.Repaint; //Force Show

  //save
  MsgForm.FMsgMemo:=MsgMemo;
  MsgForm.FMsgPBar:=MsgPBar;
  MsgForm.FCustom :=Attributes.Custom;
  Result:=MsgForm;
  //save

 except
  MsgForm.Free;
  MsgForm:=nil;
 end;


end;


end.

