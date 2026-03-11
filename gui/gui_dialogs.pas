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
  LMessages,
  LCLType,
  LCLIntf,

  host_ipc_interface,
  game_info,
  game_run_context,

  ps4_libSceMsgDialog;

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
  procedure OnMsgDialogClick(Sender:TObject);
  function  OnMsgDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //MSG_DIALOG_OPEN
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
 Handler.AddCallback('CDLG_SET_MSG'   ,@OnCdlgSetMsg);
 Handler.AddCallback('CDLG_SET_VALUE' ,@OnCdlgSetValue);
 Handler.AddCallback('CDLG_CLOSE'     ,@OnCdlgClose);
 Handler.AddCallback('MSG_DIALOG_OPEN',@OnMsgDialogOpen);
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

 CloseDialog();
end;

procedure TDialogsManager.CloseDialog();
begin
 if (FDialog<>nil) then
 begin
  FreeAndNil(FDialog);
  FMsgMemo:=nil;
  FMsgPBar:=nil;
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

 pContext^.SendAsyn('CDLG_SET_RESULT',SizeOf(rzdata),@rzdata);

 CloseDialog();
end;

function TDialogsManager.OnMsgDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //MSG_DIALOG_OPEN
var
 data:TMsgDialogOpen;

 AParent:TForm;
 MsgBtnz:TButton;
 MsgForm:TPanel;
 MsgMemo:TMemo;
 MsgPBar:TProgressBar;
 MsgCncl:TSpeedButton;

begin
 Result:=0;

 if (FDialog<>nil) then Exit;

 if (mlen>SizeOf(data)) then mlen:=SizeOf(data);
 data:=Default(TMsgDialogOpen);
 Move(buf^,data,mlen);

 OpenMainWindows;
 AParent:=FMainForm;

 MsgBtnz:=nil;
 MsgPBar:=nil;

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

  MsgForm.Width :=400;
  MsgForm.Height:=200;

  MsgCncl:=TSpeedButton.Create(MsgForm);
  MsgCncl.AutoSize:=True;
  MsgCncl.Images:=FImages;
  MsgCncl.ImageIndex:=3;
  MsgCncl.Anchors:=[akTop,akRight];
  MsgCncl.AnchorSide[akTop  ].Control:=MsgForm;
  MsgCncl.AnchorSide[akTop  ].Side   :=asrTop;
  MsgCncl.AnchorSide[akRight].Control:=MsgForm;
  MsgCncl.AnchorSide[akRight].Side   :=asrBottom;
  MsgCncl.Tag    :=SCE_MSG_DIALOG_BUTTON_ID_INVALID;
  MsgCncl.OnClick:=@OnMsgDialogClick;
  MsgCncl.Parent:=MsgForm;


  case data.mode of
   SCE_MSG_DIALOG_MODE_USER_MSG:
     begin
      //
      case data.buttonType of
       SCE_MSG_DIALOG_BUTTON_TYPE_OK:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrCenter,'&OK' ,SCE_MSG_DIALOG_BUTTON_ID_OK,@OnMsgDialogClick);
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_YESNO:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&Yes',SCE_MSG_DIALOG_BUTTON_ID_YES,@OnMsgDialogClick);
          MsgBtnz:=NewBtn(MsgForm,asrBottom,'&No' ,SCE_MSG_DIALOG_BUTTON_ID_NO ,@OnMsgDialogClick);
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_NONE:
         begin
          //
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&OK'    ,SCE_MSG_DIALOG_BUTTON_ID_OK     ,@OnMsgDialogClick);
          MsgBtnz:=NewBtn(MsgForm,asrBottom,'&Cancel',SCE_MSG_DIALOG_BUTTON_ID_INVALID,@OnMsgDialogClick);
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_WAIT:
         begin
          //
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_WAIT_CANCEL:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrCenter,'&Cancel',SCE_MSG_DIALOG_BUTTON_ID_INVALID,@OnMsgDialogClick);
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_YESNO_FOCUS_NO:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&No' ,SCE_MSG_DIALOG_BUTTON_ID_NO ,@OnMsgDialogClick);
          MsgBtnz:=NewBtn(MsgForm,asrBottom,'&Yes',SCE_MSG_DIALOG_BUTTON_ID_YES,@OnMsgDialogClick);
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_OK_CANCEL_FOCUS_CANCEL:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&Cancel',SCE_MSG_DIALOG_BUTTON_ID_INVALID,@OnMsgDialogClick);
          MsgBtnz:=NewBtn(MsgForm,asrBottom,'&OK'    ,SCE_MSG_DIALOG_BUTTON_ID_OK     ,@OnMsgDialogClick);
         end;
       SCE_MSG_DIALOG_BUTTON_TYPE_2BUTTONS:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrTop   ,'&'+data.msg1,SCE_MSG_DIALOG_BUTTON_ID_BUTTON1,@OnMsgDialogClick);
          MsgBtnz:=NewBtn(MsgForm,asrBottom,'&'+data.msg2,SCE_MSG_DIALOG_BUTTON_ID_BUTTON2,@OnMsgDialogClick);
         end
       else;
      end;
      //
     end;
   SCE_MSG_DIALOG_MODE_PROGRESS_BAR:
     begin
      //

      MsgPBar:=TProgressBar.Create(MsgForm);
      MsgPBar.Min:=0;
      MsgPBar.Max:=100;
      MsgPBar.Position:=0;
      MsgPBar.Smooth:=True;
      MsgPBar.BorderSpacing.Left  :=5;
      MsgPBar.BorderSpacing.Right :=5;
      MsgPBar.BorderSpacing.Bottom:=5;

      case data.barType of
       SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE:;
       SCE_MSG_DIALOG_PROGRESSBAR_TYPE_PERCENTAGE_CANCEL:
         begin
          MsgBtnz:=NewBtn(MsgForm,asrCenter,'&Cancel',SCE_MSG_DIALOG_BUTTON_ID_INVALID,@OnMsgDialogClick);
         end;
       else;
      end;
      //
     end;
   SCE_MSG_DIALOG_MODE_SYSTEM_MSG:
     begin
      MsgBtnz:=NewBtn(MsgForm,asrCenter,'&OK',SCE_MSG_DIALOG_BUTTON_ID_OK,@OnMsgDialogClick);
     end;
   else;
  end;

  //
  MsgMemo:=TMemo.Create(MsgForm);
  MsgMemo.ReadOnly:=True;
  MsgMemo.Font.Name:='Courier New';
  MsgMemo.Font.Size:=GetRealFontSize(AParent.Font) + 2;
  //
  MsgMemo.Anchors:=[akTop,akLeft,akRight,akBottom];
  MsgMemo.AnchorSide[akTop   ].Control:=MsgCncl;
  MsgMemo.AnchorSide[akTop   ].Side   :=asrBottom;
  MsgMemo.AnchorSide[akLeft  ].Control:=MsgForm;
  MsgMemo.AnchorSide[akLeft  ].Side   :=asrTop;
  MsgMemo.AnchorSide[akRight ].Control:=MsgForm;
  MsgMemo.AnchorSide[akRight ].Side   :=asrBottom;
  MsgMemo.AnchorSide[akBottom].Control:=MsgForm;
  MsgMemo.AnchorSide[akBottom].Side   :=asrBottom;

  if (MsgPBar<>nil) then
  begin
   MsgPBar.Anchors:=[akLeft,akRight,akBottom];
   MsgPBar.AnchorSide[akLeft  ].Control:=MsgForm;
   MsgPBar.AnchorSide[akLeft  ].Side   :=asrTop;
   MsgPBar.AnchorSide[akRight ].Control:=MsgForm;
   MsgPBar.AnchorSide[akRight ].Side   :=asrBottom;
   //
   if (MsgBtnz<>nil) then
   begin
    MsgPBar.AnchorSide[akBottom].Control:=MsgBtnz;
    MsgPBar.AnchorSide[akBottom].Side   :=asrTop;
   end else
   begin
    MsgPBar.AnchorSide[akBottom].Control:=MsgForm;
    MsgPBar.AnchorSide[akBottom].Side   :=asrBottom;
   end;
   //
   MsgPBar.Parent:=MsgForm;
   //
   MsgMemo.AnchorSide[akBottom].Control:=MsgPBar;
   MsgMemo.AnchorSide[akBottom].Side   :=asrTop;
  end else
  if (MsgBtnz<>nil) then
  begin
   MsgMemo.AnchorSide[akBottom].Control:=MsgBtnz;
   MsgMemo.AnchorSide[akBottom].Side   :=asrTop;
  end;

  MsgMemo.BorderSpacing.Bottom:=10;

  case data.mode of
   SCE_MSG_DIALOG_MODE_USER_MSG:
     begin
      MsgMemo.Text:=data.msg;
     end;
   SCE_MSG_DIALOG_MODE_PROGRESS_BAR:
     begin
      MsgMemo.Text:=data.msg;
     end;
   SCE_MSG_DIALOG_MODE_SYSTEM_MSG:
     begin
      case data.sysMsgType of
       SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_EMPTY_STORE                            :MsgMemo.Text:='TRC_EMPTY_STORE';
       SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_CHAT_RESTRICTION                   :MsgMemo.Text:='TRC_PSN_CHAT_RESTRICTION';
       SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_PSN_UGC_RESTRICTION                    :MsgMemo.Text:='TRC_PSN_UGC_RESTRICTION';
       SCE_MSG_DIALOG_SYSMSG_TYPE_TRC_WARNING_SWITCH_TO_SIMULVIEW            :MsgMemo.Text:='TRC_WARNING_SWITCH_TO_SIMULVIEW';
       SCE_MSG_DIALOG_SYSMSG_TYPE_CAMERA_NOT_CONNECTED                       :MsgMemo.Text:='CAMERA_NOT_CONNECTED';
       SCE_MSG_DIALOG_SYSMSG_TYPE_WARNING_PROFILE_PICTURE_AND_NAME_NOT_SHARED:MsgMemo.Text:='WARNING_PROFILE_PICTURE_AND_NAME_NOT_SHARED';
       SCE_MSG_DIALOG_SYSMSG_TYPE_PSN_COMMUNICATION_RESTRICTION              :MsgMemo.Text:='PSN_COMMUNICATION_RESTRICTION';
      end;
     end;
   else;
  end;

  //
  MsgMemo.Parent:=MsgForm;
  //

  MsgForm.Parent:=AParent;
  MsgForm.Show;

  //save
  FDialog :=MsgForm;
  FMsgMemo:=MsgMemo;
  FMsgPBar:=MsgPBar;
  //save

 except
  MsgForm.Free;
 end;

end;


end.

