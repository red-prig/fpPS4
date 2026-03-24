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
  LCLIntf;

type
 TDialogButtonsType=(
  btnOk,
  btnYesNo,
  btnOkCancel,
  btnCancel,
  btnNoYes,
  btnCancelYes,
  btn2Buttons
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
 end;

 TDialogCustom=class(TPanel)
  public
   FMsgMemo:TMemo;
   FMsgPBar:TProgressBar;
   FCustom :TWinControl;
  Destructor Destroy; override;
 end;

 TImeDialog=class(TDialogCustom)
  public
   state      :Byte;
   button     :Byte;
   FixedPos   :Boolean;
   Over2kCoord:Boolean;
 end;

function NewDialogOpen(var Attributes:TDialogAttributes):TDialogCustom;

implementation

Destructor TDialogCustom.Destroy;
begin
 FreeAndNil(FCustom);
 inherited;
end;

function GetRealFontSize(Font:TFont):Integer;
var
 fd: TFontData;
begin
 fd := Graphics.GetFontData(Font.Handle);
 Result := ((-fd.Height) * 72) div Font.PixelsPerInch;
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

 VirtualWidth :Single;
 VirtualHeight:Single;
 VRect        :TRect;

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
  TImeDialog(MsgForm).state      :=1;
  TImeDialog(MsgForm).FixedPos   :=Attributes.Memo.Ime^.FixedPos;
  TImeDialog(MsgForm).Over2kCoord:=Attributes.Memo.Ime^.Over2kCoord;
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

  if (Attributes.Memo.Ime<>nil) then
  begin
   //TODO:Virtual window position adjustment and movement
   MsgForm.Anchors:=[];

   if Attributes.Memo.Ime^.Over2kCoord then
   begin
    VirtualWidth :=3840.0;
    VirtualHeight:=1920.0;
   end else
   begin
    VirtualWidth :=2160.0;
    VirtualHeight:=1080.0;
   end;

   VRect.Left  :=Trunc((Attributes.Memo.Ime^.posx  /VirtualWidth )*AParent.Width );
   VRect.Top   :=Trunc((Attributes.Memo.Ime^.posy  /VirtualHeight)*AParent.Height);
   VRect.Width :=Trunc((Attributes.Memo.Ime^.Width /VirtualWidth )*AParent.Width );
   VRect.Height:=Trunc((Attributes.Memo.Ime^.Height/VirtualHeight)*AParent.Height);

   //reposition
   case Attributes.Memo.Ime^.hAlign of
    asrBottom:VRect.SetLocation(VRect.Right        ,VRect.Top);
    asrCenter:VRect.SetLocation(VRect.CenterPoint.X,VRect.Top);
    else;
   end;

   //reposition
   case Attributes.Memo.Ime^.vAlign of
    asrBottom:VRect.SetLocation(VRect.Left,VRect.Bottom);
    asrCenter:VRect.SetLocation(VRect.Left,VRect.CenterPoint.Y);
    else;
   end;

   //fixup
   if (VRect.Right>AParent.Width) then
   begin
    VRect.Offset(AParent.Width-VRect.Right,0);
   end;

   //fixup
   if (VRect.Bottom>AParent.Height) then
   begin
    VRect.Offset(0,AParent.Height-VRect.Bottom);
   end;

   //
   MsgForm.Left  :=VRect.Left  ;
   MsgForm.Top   :=VRect.Top   ;
   MsgForm.Width :=VRect.Width ;
   MsgForm.Height:=VRect.Height;
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
    MsgMemo.MaxLength:=Attributes.Memo.Ime^.MaxLength;
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
   else;
  end;

  //MsgFTop
  //MsgBody
  //MsgBtnz

  if (MsgFTop<>nil) then
  begin
   MsgBody.AnchorSide[akTop].Control:=MsgFTop;
   MsgBody.AnchorSide[akTop].Side   :=asrBottom;
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

