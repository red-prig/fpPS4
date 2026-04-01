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

  host_ipc,
  game_info,
  game_run_context,

  gui_dialog_fabric,

  ps4_libSceMsgDialog,
  ps4_libSceSaveDataDialog,
  ps4_libSceNpCommerce,
  ps4_libSceErrorDialog,
  ps4_libSceImeDialog,
  ps4_libSigninDialog;

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
  //
  FCommonDialog:TDialogCustom;
  FErrorDialog :TDialogCustom;
  FImeDialog   :TDialogCustom;
  FSigninDialog:TDialogCustom;
  //
  function  get_caption_format:RawByteString;
  procedure DoResize(Sender:TObject);
  function  OpenMainWindows:THandle;
  procedure CloseDialogs();
  Procedure CloseMainWindow();
  Procedure ShowMainWindow();
  Procedure HideMainWindow();
  procedure SetCaptionFPS(Ffps:QWORD);
  //
  procedure BindHandler(Handler:THostIpcHandler);
  function  OnCdlgSetMsg  (Value:TIpcValue):TIpcValue; //CDLG_SET_MSG
  function  OnCdlgSetValue(Value:TIpcValue):TIpcValue; //CDLG_SET_VALUE
  function  OnCdlgClose   (Value:TIpcValue):TIpcValue; //CDLG_CLOSE
  procedure NewDialogOpen(var Attributes:TDialogAttributes;var pResult:TDialogCustom);
  //
  procedure OnMsgDialogClick(Sender:TObject);
  function  OnMsgDialogOpen(Value:TIpcValue):TIpcValue; //MSG_DIALOG_OPEN
  //
  procedure OnSaveDialogClick(Sender:TObject);
  function  OnSaveDialogOpen(Value:TIpcValue):TIpcValue; //SAVE_DIALOG_OPEN
  //
  procedure OnNpCommerceDialogClick(Sender:TObject);
  function  OnNpCommerceDialogOpen(Value:TIpcValue):TIpcValue; //NPCOMMERCE_DIALOG_OPEN
  //
  procedure OnErrDlgClick(Sender:TObject);
  function  OnErrDlgOpen  (Value:TIpcValue):TIpcValue; //ERR_DIALOG_OPEN
  function  OnErrDlgClose (Value:TIpcValue):TIpcValue; //ERR_DIALOG_CLOSE
  function  OnErrDlgUpdate(Value:TIpcValue):TIpcValue; //ERR_DIALOG_UPDATE
  //
  procedure OnImeDialogClick(Sender:TObject);
  function  OnImeDlgOpen   (Value:TIpcValue):TIpcValue; //IME_DIALOG_OPEN
  function  OnImeDlgTerm   (Value:TIpcValue):TIpcValue; //IME_DIALOG_TERM
  function  OnImeDlgAbort  (Value:TIpcValue):TIpcValue; //IME_DIALOG_ABORT
  function  OnImeDlgUpdate (Value:TIpcValue):TIpcValue; //IME_DIALOG_UPDATE
  function  OnImeDlgResult (Value:TIpcValue):TIpcValue; //IME_DIALOG_RESULT
  function  OnImeDlgGetText(Value:TIpcValue):TIpcValue; //IME_DIALOG_GETTEXT
  function  OnImeDlgSetText(Value:TIpcValue):TIpcValue; //IME_DIALOG_SETTEXT
  function  OnImeDlgGetPos (Value:TIpcValue):TIpcValue; //IME_DIALOG_GETPOS
  //
  procedure OnSigninDlgClick(Sender:TObject);
  function  OnSigninDlgOpen  (Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_OPEN
  function  OnSigninDlgClose (Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_CLOSE
  function  OnSigninDlgTerm  (Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_TERM
  function  OnSigninDlgUpdate(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_UPDATE
  function  OnSigninDlgResult(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_RESULT
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

procedure TDialogsManager.DoResize(Sender:TObject);
begin
 if (FImeDialog<>nil) then
 if (FImeDialog.OnResize<>nil) then
 begin
  FImeDialog.OnResize(Sender);
 end;
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
 FMainForm.OnResize:=@DoResize;
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

procedure TDialogsManager.CloseDialogs();
begin
 FreeAndNil(FCommonDialog);
 FreeAndNil(FErrorDialog);
end;

Procedure TDialogsManager.CloseMainWindow();
begin
 CloseDialogs();
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

procedure TDialogsManager.BindHandler(Handler:THostIpcHandler);
begin
 Handler.AddCallback('CDLG_SET_MSG'          ,@OnCdlgSetMsg);
 Handler.AddCallback('CDLG_SET_VALUE'        ,@OnCdlgSetValue);
 Handler.AddCallback('CDLG_CLOSE'            ,@OnCdlgClose);
 Handler.AddCallback('MSG_DIALOG_OPEN'       ,@OnMsgDialogOpen);
 Handler.AddCallback('SAVE_DIALOG_OPEN'      ,@OnSaveDialogOpen);
 Handler.AddCallback('NPCOMMERCE_DIALOG_OPEN',@OnNpCommerceDialogOpen);
 //
 Handler.AddCallback('ERR_DIALOG_OPEN'  ,@OnErrDlgOpen);
 Handler.AddCallback('ERR_DIALOG_CLOSE' ,@OnErrDlgClose);
 Handler.AddCallback('ERR_DIALOG_UPDATE',@OnErrDlgUpdate);
 //
 Handler.AddCallback('IME_DIALOG_OPEN'   ,@OnImeDlgOpen);
 Handler.AddCallback('IME_DIALOG_TERM'   ,@OnImeDlgTerm);
 Handler.AddCallback('IME_DIALOG_ABORT'  ,@OnImeDlgAbort);
 Handler.AddCallback('IME_DIALOG_UPDATE' ,@OnImeDlgUpdate);
 Handler.AddCallback('IME_DIALOG_RESULT' ,@OnImeDlgResult);
 Handler.AddCallback('IME_DIALOG_GETTEXT',@OnImeDlgGetText);
 Handler.AddCallback('IME_DIALOG_SETTEXT',@OnImeDlgSetText);
 Handler.AddCallback('IME_DIALOG_GETPOS' ,@OnImeDlgGetPos);
 //
 Handler.AddCallback('SIGNIN_DIALOG_OPEN'  ,@OnSigninDlgOpen);
 Handler.AddCallback('SIGNIN_DIALOG_CLOSE' ,@OnSigninDlgClose);
 Handler.AddCallback('SIGNIN_DIALOG_TERM'  ,@OnSigninDlgTerm);
 Handler.AddCallback('SIGNIN_DIALOG_UPDATE',@OnSigninDlgUpdate);
 Handler.AddCallback('SIGNIN_DIALOG_RESULT',@OnSigninDlgResult);
end;

function TDialogsManager.OnCdlgSetMsg(Value:TIpcValue):TIpcValue; //CDLG_SET_MSG
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;
 if (FCommonDialog.FMsgMemo=nil) then Exit;

 FCommonDialog.FMsgMemo.Text:=Value.GetString;
end;

function TDialogsManager.OnCdlgSetValue(Value:TIpcValue):TIpcValue; //CDLG_SET_VALUE
var
 rate:DWORD;
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;
 if (FCommonDialog.FMsgPBar=nil) then Exit;

 rate:=Value.GetDWORD;

 if (rate<=100) then
 begin
  FCommonDialog.FMsgPBar.Position:=rate;
 end;
end;

function TDialogsManager.OnCdlgClose(Value:TIpcValue):TIpcValue; //CDLG_CLOSE
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;

 //What should the result code be?
 pContext^.InvokeAsyn('CDLG_FINISH',nil);

 FreeAndNil(FCommonDialog);
end;

procedure TDialogsManager.NewDialogOpen(var Attributes:TDialogAttributes;var pResult:TDialogCustom);
begin
 Assert(pResult=nil,'NewDialogOpen');

 OpenMainWindows;
 Attributes.AParent:=FMainForm;
 Attributes.AImages:=FImages;

 pResult:=gui_dialog_fabric.NewDialogOpen(Attributes);
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

 pContext^.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

function TDialogsManager.OnMsgDialogOpen(Value:TIpcValue):TIpcValue; //MSG_DIALOG_OPEN
var
 data:TMsgDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 Assert(FCommonDialog=nil);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

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

 NewDialogOpen(Attributes,FCommonDialog);
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

 pContext^.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

type
 TSaveDataGrid=class(TStringGrid)
  public
   procedure  CustomDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
   Destructor Destroy; override;
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

Destructor TSaveDataGrid.Destroy;
var
 i:Integer;
 o:TObject;
begin
 if RowCount<>0 then
 for i:=0 to RowCount-1 do
 begin
  o:=Objects[0,i];
  FreeAndNil(o);
 end;
 inherited;
end;

function TDialogsManager.OnSaveDialogOpen(Value:TIpcValue):TIpcValue; //SAVE_DIALOG_OPEN

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

 Assert(FCommonDialog=nil);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

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

 case data.mode of
  SCE_SAVE_DATA_DIALOG_MODE_LIST,
  SCE_SAVE_DATA_DIALOG_MODE_WIZARD_LIST:
    begin
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

 NewDialogOpen(Attributes,FCommonDialog);
end;

//

procedure TDialogsManager.OnNpCommerceDialogClick(Sender:TObject);
var
 rzdata:TNpCommerceDialogResult;
begin
 FillChar(rzdata,SizeOf(rzdata),0);

 case TDialogButtonId(TCustomButton(Sender).Tag) of
  btnIdCancel   :rzdata.resultId:=1;
  btnIdOkYesBtn1:rzdata.resultId:=2;
  else;
 end;

 rzdata.authorized:=False; //PS Plus features

 pContext^.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

function  TDialogsManager.OnNpCommerceDialogOpen(Value:TIpcValue):TIpcValue; //NPCOMMERCE_DIALOG_OPEN
var
 data:TNpCommerceDialogOpen;
 Attributes:TDialogAttributes;
 i:Integer;
begin
 Result:=0;

 Assert(FCommonDialog=nil);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 Attributes.OnClick:=@OnNpCommerceDialogClick;

 Attributes.Caption.Enable:=True;
 case data.mode of
  SCE_NP_COMMERCE_DIALOG_MODE_CATEGORY    :Attributes.Caption.Message:='CATEGORY';
  SCE_NP_COMMERCE_DIALOG_MODE_PRODUCT     :Attributes.Caption.Message:='PRODUCT';
  SCE_NP_COMMERCE_DIALOG_MODE_PRODUCT_CODE:Attributes.Caption.Message:='PRODUCT_CODE';
  SCE_NP_COMMERCE_DIALOG_MODE_CHECKOUT    :Attributes.Caption.Message:='CHECKOUT';
  SCE_NP_COMMERCE_DIALOG_MODE_DOWNLOADLIST:Attributes.Caption.Message:='DOWNLOADLIST';
  SCE_NP_COMMERCE_DIALOG_MODE_PLUS        :Attributes.Caption.Message:='PLUS';
 end;

 Attributes.CloseButton.Enable:=True;
 Attributes.CloseButton.btnId :=btnIdCancel;

 Attributes.Buttons.Enable :=True;
 Attributes.Buttons.BtnType:=btnPurchaseCancel;

 Attributes.Memo.Enable :=True;
 Attributes.Memo.Message:='';

 if (data.numTargets<>0) then
 For i:=0 to data.numTargets-1 do
 begin
  if (i<>0) then
   Attributes.Memo.Message:=Attributes.Memo.Message+#13#10;
  Attributes.Memo.Message:=Attributes.Memo.Message+data.targets[i];
 end;

 NewDialogOpen(Attributes,FCommonDialog);
end;

//

//
function TDialogsManager.OnErrDlgOpen(Value:TIpcValue):TIpcValue; //ERR_DIALOG_OPEN
var
 data:TErrDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 if (FErrorDialog<>nil) then Exit(-2);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 Attributes.OnClick:=@OnErrDlgClick;

 Attributes.Caption.Enable :=True;
 Attributes.Caption.Message:='Error';

 Attributes.CloseButton.Enable:=True;

 Attributes.Memo.Enable :=True;
 Attributes.Memo.Message:='An error has occurred.'#13'0x'+HexStr(data.errorCode,8);

 Attributes.Buttons.Enable :=True;
 Attributes.Buttons.BtnType:=btnOk;

 NewDialogOpen(Attributes,FErrorDialog);
end;

procedure TDialogsManager.OnErrDlgClick(Sender:TObject);
begin
 FreeAndNil(FErrorDialog);
end;

function TDialogsManager.OnErrDlgClose(Value:TIpcValue):TIpcValue; //ERR_DIALOG_CLOSE
begin
 Result:=0;
 FreeAndNil(FErrorDialog);
end;

function TDialogsManager.OnErrDlgUpdate(Value:TIpcValue):TIpcValue; //ERR_DIALOG_UPDATE
begin
 if (FErrorDialog<>nil) then
 begin
  Result:=0;
 end else
 begin
  Result:=1;
 end;
end;

/////

procedure TDialogsManager.OnImeDialogClick(Sender:TObject);
var
 buttonId:TDialogButtonId;
begin
 if (FImeDialog<>nil) then
 begin
  buttonId:=TDialogButtonId(TCustomButton(Sender).Tag);

  if (buttonId=btnIdCancel) then
  begin
   FImeDialog.button:=1; //STATUS_USER_CANCELED
  end else
  begin
   FImeDialog.button:=0; //STATUS_OK
  end;

  FImeDialog.state:=2;
  FImeDialog.Hide;
 end;
end;

function TDialogsManager.OnImeDlgOpen(Value:TIpcValue):TIpcValue; //IME_DIALOG_OPEN
var
 data:TImeDialogOpen;
 Attributes:TDialogAttributes;
 Ime:TImeDialogAttributes;

 function GetAnchor(Align:Byte):TAnchorSideReference; inline;
 begin
  Result:=asrTop;
  case Align of
   0:Result:=asrTop;    // LEFT/TOP
   1:Result:=asrCenter; // CENTER
   2:Result:=asrBottom; // RIGHT/BOTTOM
  end;
 end;

begin
 Result:=0;

 if (FImeDialog<>nil) then Exit(-2);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 FillChar(Ime,SizeOf(Ime),0);
 Attributes.OnClick:=@OnImeDialogClick;

 Attributes.Caption.Enable :=True;
 Attributes.Caption.Message:=UTF8Encode(WideString(data.title));
 //
 Attributes.CloseButton.Enable:=True;
 Attributes.CloseButton.btnId :=btnIdCancel;
 //
 Attributes.Memo.Enable :=True;
 Attributes.Memo.Message:=UTF8Encode(WideString(data.result.inputText));
 Attributes.Memo.Ime    :=@Ime;

 Ime.Multiline  :=(data.option and     1)<>0;
 Ime.Password   :=(data.option and     4)<>0;
 Ime.FixedPos   :=(data.option and   $40)<>0;
 Ime.Over2kCoord:=(data.option and $4000)<>0;
 Ime.NumbersOnly:=(data.ImeType=4);
 Ime.hAlign     :=GetAnchor(data.PosAndForm.horizontalAlignment);
 Ime.vAlign     :=GetAnchor(data.PosAndForm.verticalAlignment);
 Ime.MaxLength  :=data.maxTextLength;
 Ime.posx       :=data.PosAndForm.posx;
 Ime.posy       :=data.PosAndForm.posy;
 Ime.width      :=data.PosAndForm.width;
 Ime.height     :=data.PosAndForm.height;

 case data.enterLabel of
  0:Ime.EditLabel:='OK'    ; //DEFAULT
  1:Ime.EditLabel:='SEND'  ; //SEND
  2:Ime.EditLabel:='SEARCH'; //SEARCH
  3:Ime.EditLabel:='GO'    ; //GO
 end;

 NewDialogOpen(Attributes,TDialogCustom(FImeDialog));
end;

function TDialogsManager.OnImeDlgTerm(Value:TIpcValue):TIpcValue; //IME_DIALOG_TERM
begin
 Result:=0;
 FreeAndNil(FImeDialog);
end;

function TDialogsManager.OnImeDlgAbort(Value:TIpcValue):TIpcValue; //IME_DIALOG_ABORT
begin
 Result:=0;
 if (FImeDialog<>nil) then
 begin
  FImeDialog.button:=2; //STATUS_ABORTED
  FImeDialog.state :=2;
  FImeDialog.Hide;
 end;
end;

function TDialogsManager.OnImeDlgUpdate(Value:TIpcValue):TIpcValue; //IME_DIALOG_UPDATE
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  Result:=FImeDialog.state;
 end;
end;

function TDialogsManager.OnImeDlgResult(Value:TIpcValue):TIpcValue; //IME_DIALOG_RESULT
var
 data:TImeDialogResult;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  FillChar(data,SizeOf(data),0);
  data.endstatus:=FImeDialog.button;
  data.inputText:=UTF8Decode(FImeDialog.FMsgMemo.Text);
  Result:=TIpcValue.New(@data,SizeOf(data));
 end;
end;

function TDialogsManager.OnImeDlgGetText(Value:TIpcValue):TIpcValue; //IME_DIALOG_GETTEXT
var
 data:TImeDialogTextFilter;
 w:WideString;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  FillChar(data,SizeOf(data),0);
  //
  w:=UTF8Decode(FImeDialog.FMsgMemo.Text);
  if (Length(w)>120) then SetLength(w,120);
  //
  data.Text      :=w;
  data.TextLength:=Length(w);
  Result:=TIpcValue.New(@data,SizeOf(data));
 end;
end;

function TDialogsManager.OnImeDlgSetText(Value:TIpcValue):TIpcValue; //IME_DIALOG_SETTEXT
var
 data:TImeDialogTextFilter;
 w:WideString;
 s:RawByteString;
 CaretPos:TPoint;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  FillChar(data,SizeOf(data),0);
  Value.MoveTo(@data,sizeof(data));
  //
  if (data.result=0) then
  begin
   if (data.TextLength>120) then data.TextLength:=120;
   SetLength(w,data.TextLength);
   Move(data.Text,w[1],data.TextLength*sizeof(WideChar));
   s:=UTF8Encode(w);
   if (s<>FImeDialog.FMsgMemo.Text) then
   begin
    CaretPos:=FImeDialog.FMsgMemo.CaretPos;
    FImeDialog.FMsgMemo.Text:=s;
    FImeDialog.FMsgMemo.CaretPos:=CaretPos;
   end;
  end;
 end;
end;

function TDialogsManager.OnImeDlgGetPos(Value:TIpcValue):TIpcValue; //IME_DIALOG_GETPOS
var
 data:TImeDialogPosAndForm;

 function GetAlign(Side:TAnchorSideReference):Byte; inline;
 begin
  Result:=0;
  case Side of
   asrTop   :Result:=0; // LEFT/TOP
   asrCenter:Result:=1; // CENTER
   asrBottom:Result:=2; // RIGHT/BOTTOM
  end;
 end;

begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  data.PanelType          :=2;
  data.horizontalAlignment:=GetAlign(TImeDialog(FImeDialog).hAlign);
  data.verticalAlignment  :=GetAlign(TImeDialog(FImeDialog).vAlign);
  data.posx               :=TImeDialog(FImeDialog).GetPosX;
  data.posy               :=TImeDialog(FImeDialog).GetPosY;
  data.width              :=Trunc(TImeDialog(FImeDialog).Fwidth);
  data.height             :=Trunc(TImeDialog(FImeDialog).Fheight);
  //
  Result:=TIpcValue.New(@data,SizeOf(data));
 end;
end;

////

procedure TDialogsManager.OnSigninDlgClick(Sender:TObject);
var
 buttonId:TDialogButtonId;
begin
 if (FSigninDialog<>nil) then
 begin
  buttonId:=TDialogButtonId(TCustomButton(Sender).Tag);

  if (buttonId=btnIdCancel) then
  begin
   FSigninDialog.button:=1; //STATUS_USER_CANCELED
  end else
  begin
   FSigninDialog.button:=0; //STATUS_OK
  end;

  FSigninDialog.state:=1;
  FSigninDialog.Hide;
 end;
end;

function TDialogsManager.OnSigninDlgOpen(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_OPEN
var
 data:TSigninDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 if (FSigninDialog<>nil) then
 begin
  if (FSigninDialog.state=1) then
  begin
   //reopen
   FreeAndNil(FSigninDialog);
  end else
  begin
   Exit(-2);
  end;
 end;

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 Attributes.OnClick:=@OnSigninDlgClick;

 Attributes.Caption.Enable :=True;
 Attributes.Caption.Message:='Sign in';

 Attributes.CloseButton.Enable:=True;
 Attributes.CloseButton.btnId :=btnIdCancel;

 Attributes.Memo.Enable :=True;
 Attributes.Memo.Message:='Sign in to Network';

 Attributes.Buttons.Enable :=True;
 Attributes.Buttons.BtnType:=btnOkCancel;

 NewDialogOpen(Attributes,FSigninDialog);
end;

function TDialogsManager.OnSigninDlgClose(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_CLOSE
begin
 Result:=0;
 if (FSigninDialog<>nil) then
 begin
  FSigninDialog.button:=1; //STATUS_USER_CANCELED
  FSigninDialog.state :=1;
  FSigninDialog.Hide;
 end;
end;

function TDialogsManager.OnSigninDlgTerm(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_TERM
begin
 Result:=0;
 FreeAndNil(FSigninDialog);
end;

function TDialogsManager.OnSigninDlgUpdate(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_UPDATE
begin
 Result:=-1;
 if (FSigninDialog<>nil) then
 begin
  Result:=FSigninDialog.state;
 end;
end;

function TDialogsManager.OnSigninDlgResult(Value:TIpcValue):TIpcValue; //SIGNIN_DIALOG_RESULT
begin
 Result:=-1;
 if (FSigninDialog<>nil) then
 begin
  Result:=FSigninDialog.button;
 end;
end;


end.

