unit gui_dialogs;

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
  Grids,
  LMessages,
  LCLType,
  LCLIntf,

  CharStream,

  host_ipc_interface,
  game_info,
  game_run_context,

  ps4_libSceMsgDialog,
  ps4_libSceSaveDataDialog;

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

 TDialogAttributes=record
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

type
 TGameMainForm=class(TForm)
  public
   caption_format:RawByteString;
   procedure SetCaptionFPS(Ffps:QWORD);
   procedure WMEraseBkgnd(var Message:TLMEraseBkgnd); message LM_ERASEBKGND;
 end;

 TDialogsManager=object
  FImages  :TImageList;
  pContext :PGameRunContext;
  FMainForm:TGameMainForm;
  FDialog  :TPanel;
  FMsgMemo :TMemo;
  FMsgPBar :TProgressBar;
  FCustom  :TWinControl;
  //
  function  get_caption_format:RawByteString;
  function  OpenMainWindows:THandle;
  Procedure CloseMainWindow();
  Procedure ShowMainWindow();
  Procedure HideMainWindow();
  procedure SetCaptionFPS(Ffps:QWORD);
  //
  procedure BindHandler(Handler:THostIpcHandler);
  function  OnCdlgSetMsg  (mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_MSG
  function  OnCdlgSetValue(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_VALUE
  function  OnCdlgClose   (mlen:DWORD;buf:Pointer):Ptruint; //CDLG_CLOSE
  procedure CloseDialog();
  procedure NewDialogOpen(var Attributes:TDialogAttributes);
  //
  procedure OnMsgDialogClick(Sender:TObject);
  function  OnMsgDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //MSG_DIALOG_OPEN
  //
  procedure OnSaveDialogClick(Sender:TObject);
  function  OnSaveDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //SAVE_DIALOG_OPEN
 end;

 function GetRealFontSize(Font:TFont):Integer;

implementation

function GetRealFontSize(Font:TFont):Integer;
var
 fd: TFontData;
begin
 fd := Graphics.GetFontData(Font.Handle);
 Result := ((-fd.Height) * 72) div Font.PixelsPerInch;
end;

//

procedure TGameMainForm.SetCaptionFPS(Ffps:QWORD);
begin
 Caption:=Format(caption_format,[Ffps]);
end;

procedure TGameMainForm.WMEraseBkgnd(var Message:TLMEraseBkgnd);
begin
 Message.Result:=1;
end;


//

function TDialogsManager.get_caption_format:RawByteString;
var
 TITLE,TITLE_ID,APP_VER:RawByteString;
begin
 Result:='';

 if (pContext^.FGameItem=nil) then Exit;

 TITLE   :=pContext^.FGameItem.FGameInfo.Name;
 TITLE_ID:=pContext^.FGameItem.FGameInfo.TitleId;
 APP_VER :=pContext^.FGameItem.FGameInfo.AppVer;

 if (TITLE='') then
 begin
  TITLE:=ExtractFileName(pContext^.FGameItem.FGameInfo.Exec);
 end;

 if (TITLE_ID<>'') then TITLE_ID:='-' +TITLE_ID;
 if (APP_VER <>'') then APP_VER :=':v'+APP_VER;

 Result:=Format('fpPS4 (%s) [%s%s%s]',[{$I tag.inc},TITLE,TITLE_ID,APP_VER])+' FPS:%d';
end;

function TDialogsManager.OpenMainWindows:THandle;
const
 pd_Width=1280;
 pd_Height=720;
begin
 if (FMainForm<>nil) then
 begin
  FMainForm.Show;
  Exit(FMainForm.Handle);
 end;

 FMainForm:=TGameMainForm.CreateNew(nil);
 FMainForm.ShowInTaskBar:=stAlways;
 FMainForm.DoubleBuffered:=False;
 FMainForm.ParentDoubleBuffered:=False;
 FMainForm.FormStyle:=fsNormal;
 FMainForm.SetBounds(100, 100, pd_Width, pd_Height);
 FMainForm.caption_format:=get_caption_format;
 FMainForm.SetCaptionFPS(0);
 //FMainForm.OnClose:=@FMainForm.CloseEvent;
 //FMainForm.OnKeyDown:=@FMainForm.KeyEvent;
 FMainForm.Position:=poScreenCenter;

 ///
 ///

 FMainForm.Show;

 Exit(FMainForm.Handle);
end;

Procedure TDialogsManager.CloseMainWindow();
begin
 CloseDialog();
 //
 FreeAndNil(FMainForm);
end;

Procedure TDialogsManager.ShowMainWindow();
begin
 if (FMainForm<>nil) then
 begin
  FMainForm.Show;
 end;
end;

Procedure TDialogsManager.HideMainWindow();
begin
 if (FMainForm<>nil) then
 begin
  FMainForm.Hide;
 end;
end;

procedure TDialogsManager.SetCaptionFPS(Ffps:QWORD);
begin
 if (FMainForm=nil) then Exit;

 FMainForm.SetCaptionFPS(Ffps);
end;

//

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

procedure TDialogsManager.BindHandler(Handler:THostIpcHandler);
begin
 Handler.AddCallback('CDLG_SET_MSG'    ,@OnCdlgSetMsg);
 Handler.AddCallback('CDLG_SET_VALUE'  ,@OnCdlgSetValue);
 Handler.AddCallback('CDLG_CLOSE'      ,@OnCdlgClose);
 Handler.AddCallback('MSG_DIALOG_OPEN' ,@OnMsgDialogOpen);
 Handler.AddCallback('SAVE_DIALOG_OPEN',@OnSaveDialogOpen);
end;

function TDialogsManager.OnCdlgSetMsg(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_MSG
var
 str:RawByteString;
begin
 Result:=0;

 str:='';
 SetLength(str,mlen);
 Move(buf^,str[1],mlen);

 if (FMsgMemo<>nil) then
 begin
  FMsgMemo.Text:=str;
 end;
end;

function TDialogsManager.OnCdlgSetValue(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_VALUE
var
 rate:DWORD;
begin
 Result:=0;

 if (mlen>SizeOf(DWORD)) then mlen:=SizeOf(DWORD);
 rate:=0;
 Move(buf^,rate,mlen);

 if (FMsgPBar<>nil) then
 begin
  FMsgPBar.Position:=rate;
 end;
end;

function TDialogsManager.OnCdlgClose(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_CLOSE
begin
 Result:=0;
 if (FDialog=nil) then Exit;

 //What should the result code be?
 pContext^.SendAsyn('CDLG_FINISH',0,nil);

 CloseDialog();
end;

procedure TDialogsManager.CloseDialog();
begin
 if (FDialog<>nil) then
 begin
  FreeAndNil(FDialog);
  FMsgMemo:=nil;
  FMsgPBar:=nil;
  FreeAndNil(FCustom);
 end;
end;

procedure TDialogsManager.NewDialogOpen(var Attributes:TDialogAttributes);
var
 AParent:TForm;
 MsgForm:TPanel;
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
 Assert(FDialog=nil);
 if (FDialog<>nil) then Exit;

 OpenMainWindows;
 AParent:=FMainForm;

 MsgFTop:=nil;
 MsgBody:=nil;
 MCenter:=nil;
 MsgCapt:=nil;
 MsgBtnz:=nil;
 MsgMemo:=nil;
 MsgPBar:=nil;
 MsgCncl:=nil;

 MsgForm:=TPanel.Create(nil);
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
   MsgCncl.Images:=FImages;
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
  FDialog :=MsgForm;
  FMsgMemo:=MsgMemo;
  FMsgPBar:=MsgPBar;
  FCustom :=Attributes.Custom;
  //save

 except
  MsgForm.Free;
 end;

end;

procedure TDialogsManager.OnMsgDialogClick(Sender:TObject);
var
 rzdata:TMsgDialogResult;
begin
 rzdata.resultId:=0;
 rzdata.buttonId:=TCustomButton(Sender).Tag;
 if (rzdata.buttonId=0) then
 begin
  rzdata.resultId:=1;
 end;

 pContext^.SendAsyn('CDLG_FINISH',SizeOf(rzdata),@rzdata);

 CloseDialog();
end;

function TDialogsManager.OnMsgDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //MSG_DIALOG_OPEN
var
 data:TMsgDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 Assert(FDialog=nil);
 if (FDialog<>nil) then Exit;

 if (mlen>SizeOf(data)) then mlen:=SizeOf(data);
 FillChar(data,SizeOf(data),0);
 Move(buf^,data,mlen);

 FillChar(Attributes,SizeOf(Attributes),0);
 Attributes.OnClick:=@OnMsgDialogClick;
 Attributes.CloseButton.Enable:=True;
 Attributes.CloseButton.btnId :=btnIdCancel;
 Attributes.Memo.Enable:=True;

 case data.mode of
  SCE_MSG_DIALOG_MODE_USER_MSG:
    begin
     //
     case data.buttonType of
      SCE_MSG_DIALOG_BUTTON_TYPE_OK:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnOk;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_YESNO:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnYesNo;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_NONE:
        begin
         Attributes.CloseButton.Enable:=False;
         //
         Attributes.Buttons.Enable:=False;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnOkCancel;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_WAIT:
        begin
         Attributes.CloseButton.Enable:=False;
         //
         Attributes.Caption.Enable :=True;
         Attributes.Caption.Message:='Wait';
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_WAIT_CANCEL:
        begin
         Attributes.Caption.Enable :=True;
         Attributes.Caption.Message:='Wait';
         //
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnCancel;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_YESNO_FOCUS_NO:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnNoYes;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL_FOCUS_CANCEL:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnCancelYes;
        end;
      SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btn2Buttons;
         Attributes.Buttons.BtnMsg[0]:=data.msg1;
         Attributes.Buttons.BtnMsg[1]:=data.msg2;
        end;
      else;
     end;
     //
     Attributes.Memo.Message:=data.msg;
    end;
  SCE_MSG_DIALOG_MODE_PROGRESS_BAR:
    begin
     Attributes.ProgressBar.Enable:=True;
     //
     if (data.barType=SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE_CANCEL) then
     begin
      Attributes.Buttons.Enable :=True;
      Attributes.Buttons.BtnType:=btnCancel;
     end else
     begin
      Attributes.CloseButton.Enable:=False;
     end;
     //
     Attributes.Memo.Message:=data.msg;
    end;
  SCE_MSG_DIALOG_MODE_SYSTEM_MSG:
    begin
     Attributes.Caption.Enable :=True;
     Attributes.Caption.Message:='System';
     //
     Attributes.Buttons.Enable :=True;
     Attributes.Buttons.BtnType:=btnOk;
     //
     case data.sysMsgType of
      SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_EMPTY_STORE:
        Attributes.Memo.Message:='No Product Available to Purchase';
      SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_CHAT_RESTRICTION:
        Attributes.Memo.Message:='Chat/messeges Restriction';
      SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_UGC_RESTRICTION:
        Attributes.Memo.Message:='User-Generated Media Restriction';
      SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_WARNING_SWITCH_TO_SIMULVIEW:
        Attributes.Memo.Message:='Video output mode switches to SimulView';
      SCE_MSG_DIALOG_SYSMSG_TYPE_CAMERA_NOT_CONNECTED:
        Attributes.Memo.Message:='PlayStation Camera is not connected';
      SCE_MSG_DIALOG_SYSMSG_TYPE_WARNING_PROFILE_PICTURE_AND_NAME_NOT_SHARED:
        Attributes.Memo.Message:='Name and profile not shared';
      SCE_MSG_DIALOG_SYSMSG_TYPE_PSN_COMMUNICATION_RESTRICTION:
        Attributes.Memo.Message:='Parental Control - Social Feature Restriction';
     end;
    end;
  else;
 end;

 NewDialogOpen(Attributes);
end;

procedure TDialogsManager.OnSaveDialogClick(Sender:TObject);
var
 rzdata:TSaveDialogResult;
begin
 FillChar(rzdata,SizeOf(rzdata),0);
 rzdata.buttonId:=TCustomButton(Sender).Tag;
 if (rzdata.buttonId=0) then
 begin
  rzdata.resultId:=1;
 end;

 //TODO:rzdata.dirName
 //TODO:rzdata.param

 //SceSaveDataParam=packed record
 // title    :array[0..SCE_SAVE_DATA_TITLE_MAXSIZE-1] of AnsiChar;
 // subTitle :array[0..SCE_SAVE_DATA_SUBTITLE_MAXSIZE-1] of AnsiChar;
 // detail   :array[0..SCE_SAVE_DATA_DETAIL_MAXSIZE-1] of AnsiChar;
 // userParam:DWORD;
 // align    :DWORD;
 // mtime    :QWORD;
 // reserved :array[0..31] of Byte;
 //end;

 pContext^.SendAsyn('CDLG_FINISH',SizeOf(rzdata),@rzdata);

 CloseDialog();
end;

type
 TSaveDataGrid=class(TStringGrid)
  procedure CustomDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
 end;

procedure TSaveDataGrid.CustomDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
var
 Icon:TCustomBitmap;
begin
 if (aCol=0) then
 begin
  //PNG
  Icon:=TCustomBitmap(Objects[0,aRow]);
  if Icon.InheritsFrom(TCustomBitmap) then
  begin
   Canvas.StretchDraw(aRect,Icon);
  end;
 end else
 begin
  DefaultDrawCell(aCol,aRow,aRect,aState);
 end;
end;

function TDialogsManager.OnSaveDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //SAVE_DIALOG_OPEN

const
 SysDispCaption:array[0..3] of PChar=(
  '',
  'Save',
  'Load',
  'Delete'
 );

 SysProgMsg:array[0..3] of PChar=(
  '',
  'Saving...',
  'Loading..',
  'Deleting...'
 );

 SaveSysMsg:array[0..15] of PChar=(
  {0 } '',
  {1 } 'There is no saved data.',
  {2 } 'Do you want to save?',
  {3 } 'Do you want to overwrite this saved data?',
  {4 } 'Cannot save. To continue using the current application, you must delete another application or video clip that is larger than %s on the system storage. If you delete multiple applications or video clips, the total size you have to delete might be larger than %s.',
  {5 } 'Saving...',
  {6 } 'The data is corrupted.',
  {7 } 'Saving complete.',
  {8 } 'Cannot save. To save your progress in the current application, you must delete another application or video clip that is larger than %s on the system storage. If you delete multiple applications or video clips, the total size you have to delete might be larger than %s.',
  {9 } 'The saved data is corrupted. Saved data that was backed up by this system will be restored.',
  {10} 'The saved data is corrupted. This saved data will be deleted.',
  {11} 'The saved data is corrupted. This saved data will be deleted, and new saved data will be created.',
  {12} 'unknow 12',
  {13} 'The following saved data is corrupted.'#13'The following saved data that was backed up by this system will be restored.',
  {14} 'Cannot create more saved data for this application.',
  {15} 'Cannot restore the saved data. To restore the saved data, you must delete another application or video clip that is larger than %s on the system storage. If you delete multiple applications or video clips, the total size you have to delete might be larger than %s.'
 );

 LoadSysMsg:array[0..15] of PChar=(
  {0 } '',
  {1 } 'There is no saved data.',
  {2 } 'Do you want to load this saved data?',
  {3 } '',
  {4 } '',
  {5 } 'Loading...',
  {6 } 'The data is corrupted.',
  {7 } 'Loading complete.',
  {8 } '',
  {9 } 'The saved data is corrupted. Saved data that was backed up by this system will be restored.',
  {10} 'The saved data is corrupted. This saved data will be deleted.',
  {11} 'The saved data is corrupted. This saved data will be deleted, and new saved data will be created.',
  {12} 'unknow 12',
  {13} 'The following saved data is corrupted.'#13'The following saved data that was backed up by this system will be restored.',
  {14} '',
  {15} 'Cannot restore the saved data. To restore the saved data, you must delete another application or video clip that is larger than %s on the system storage. If you delete multiple applications or video clips, the total size you have to delete might be larger than %s.'
 );

 DeleteSysMsg:array[0..15] of PChar=(
  {0 } '',
  {1 } 'There is no saved data.',
  {2 } 'Do you want to delete this saved data?',
  {3 } '',
  {4 } '',
  {5 } 'Deleting...',
  {6 } 'The data is corrupted.',
  {7 } 'Deletion complete.',
  {8 } '',
  {9 } '',
  {10} '',
  {11} '',
  {12} 'unknow 12',
  {13} '',
  {14} '',
  {15} ''
 );

var
 data:TSaveDialogOpen;
 Attributes:TDialogAttributes;
 Grid:TSaveDataGrid;
 Icon:TCustomBitmap;
 Stream:TPCharStream;
 i,p:Integer;
begin
 Result:=0;

 Assert(FDialog=nil);
 if (FDialog<>nil) then Exit;

 if (mlen>SizeOf(data)) then mlen:=SizeOf(data);
 FillChar(data,SizeOf(data),0);
 Move(buf^,data,mlen);

 FillChar(Attributes,SizeOf(Attributes),0);
 Attributes.OnClick:=@OnSaveDialogClick;

 if (data.back=SCE_SAVE_DATA_DIALOG_OPTION_BACK_ENABLE) then
 begin
  Attributes.CloseButton.Enable:=True;
  Attributes.CloseButton.btnId :=btnIdCancel;
 end;

 Attributes.Caption.Enable :=True;
 Attributes.Caption.Message:=SysDispCaption[data.dispType];

 if (data.is_new<>0) or
    (data.dirNameNum<>0) then
 begin
  Grid:=TSaveDataGrid.Create(nil);
  Grid.OnDrawCell:=@Grid.CustomDrawCell;
  Grid.AutoEdit:=False;
  Grid.AutoFillColumns:=TRue;
  Grid.BorderStyle:=bsNone;
  Grid.Constraints.MinWidth :=228;
  Grid.Constraints.MinHeight:=128;
  Grid.RowCount:=data.is_new + data.dirNameNum;
  Grid.ColCount:=2;
  Grid.FixedCols:=1;
  Grid.FixedRows:=0;
  Grid.GridLineWidth:=0;
  Grid.Options:=[goRowSelect,goThumbTracking,goSmoothScroll];
  //
  Grid.ColWidths[0]:=228;
  for i:=0 to Grid.RowCount-1 do
  begin
   Grid.RowHeights[i]:=128;
  end;
  //
  p:=0;
  if (data.is_new<>0) then
  begin
   if (data.new_item.iconSize<>0) then
   begin
    Stream:=TPCharStream.Create(@data.new_item.iconBuf,data.new_item.iconSize);

    Icon:=TPortableNetworkGraphic.Create;
    Icon.LoadFromStream(Stream);
    //TODO: Destructor!

    Grid.Objects[0,0]:=Icon;

    FreeAndNil(Stream);
    Icon:=nil;
   end;

   Grid.Cells[1,0]:=data.new_item.title;
   p:=1;
  end;
  //
  if (data.dirNameNum<>0) then
  for i:=0 to data.dirNameNum-1 do
  begin
   Grid.Cells[1,p+i]:=data.dirNames[i].data;
  end;

  Attributes.Custom:=Grid;
 end;

 //TODO:new data

 case data.mode of
  SCE_SAVE_DATA_DIALOG_MODE_LIST,
  SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST:
    begin
     //TODO:Attributes.Custom
     //list table
     //
     Attributes.Buttons.Enable :=True;
     Attributes.Buttons.BtnType:=btnOk;
     //
    end;
  SCE_SAVE_DATA_DIALOG_MODE_USER_MSG:
    begin
     //
     case data.buttonType of
      SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_OK:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnOk;
        end;
      SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_YESNO:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnYesNo;
        end;
      SCE_SAVE_DATA_DIALOG_BUTTON_TYPE_OKCANCEL:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnOkCancel;
        end;
      else;
     end;
     //
     //SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_NORMAL
     //SCE_SAVE_DATA_DIALOG_USERMSG_TYPE_ERROR
     //
     Attributes.Memo.Enable :=True;
     Attributes.Memo.Message:=data.user_msg;
     //
    end;
  SCE_SAVE_DATA_DIALOG_MODE_SYSTEM_MSG:
    begin
     //
     Attributes.Memo.Enable :=True;
     case data.dispType of
      SCE_SAVE_DATA_DIALOG_TYPE_SAVE:
        begin
         Attributes.Memo.Message:=Format(SaveSysMsg[data.sys_sysMsgType],[IntToStr(data.sys_value),IntToStr(data.sys_value)]);
        end;
      SCE_SAVE_DATA_DIALOG_TYPE_LOAD:
        begin
         Attributes.Memo.Message:=Format(LoadSysMsg[data.sys_sysMsgType],[IntToStr(data.sys_value),IntToStr(data.sys_value)]);
        end;
      SCE_SAVE_DATA_DIALOG_TYPE_DELETE:
        begin
         Attributes.Memo.Message:=Format(DeleteSysMsg[data.sys_sysMsgType],[IntToStr(data.sys_value),IntToStr(data.sys_value)]);
        end;
      else;
     end;
     //
     case data.sys_sysMsgType of
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NODATA,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_FILE_CORRUPTED,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_FINISHED,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_CONTINUABLE,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_RESTORE,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_TOTAL_SIZE_EXCEEDED,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_NOSPACE_RESTORE:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnOk;
        end;
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CONFIRM,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_OVERWRITE:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnYesNo;
        end;
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_DELETE,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_CREATE,
      SCE_SAVE_DATA_DIALOG_SYSMSG_TYPE_CORRUPTED_AND_RESTORE:
        begin
         Attributes.Buttons.Enable :=True;
         Attributes.Buttons.BtnType:=btnOkCancel;
        end;
      else;
     end;
     //
    end;
  SCE_SAVE_DATA_DIALOG_MODE_ERROR_CODE:
    begin
     //
     Attributes.Memo.Enable :=True;
     Attributes.Memo.Message:='An error has occurred.'#13'0x'+HexStr(data.errorCode,8);
     //
     Attributes.Buttons.Enable :=True;
     Attributes.Buttons.BtnType:=btnOk;
     //
    end;
  SCE_SAVE_DATA_DIALOG_MODE_PROGRESS_BAR:
    begin
     //
     Attributes.Memo.Enable :=True;
     case data.bar_sysMsgType of
      SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_PROGRESS:
        begin
         Attributes.Memo.Message:=SysProgMsg[data.dispType];
        end;
      SCE_SAVE_DATA_DIALOG_PRGRESS_SYSMSG_TYPE_RESTORE:
        begin
         Attributes.Memo.Message:='Restoring saved data...';
        end;
      else
        begin
         Attributes.Memo.Message:=data.bar_msg;
        end;
     end;
     //
     Attributes.ProgressBar.Enable:=True;
     //
    end;
  else;
 end;

 case data.mode of
  SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST   :Assert(false,'TODO:MODE_WIZARD_LIST   ');
  SCE_SAVE_DATA_DIALOG_MODE_WIZARD_CONFIRM:Assert(false,'TODO:MODE_WIZARD_CONFIRM');
  else;
 end;

 NewDialogOpen(Attributes);
end;

end.

