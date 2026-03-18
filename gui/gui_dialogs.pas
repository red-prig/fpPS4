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

  gui_dialog_fabric,

  ps4_libSceMsgDialog,
  ps4_libSceSaveDataDialog,
  ps4_libSceErrorDialog;

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
  //
  function  get_caption_format:RawByteString;
  function  OpenMainWindows:THandle;
  procedure CloseDialogs();
  Procedure CloseMainWindow();
  Procedure ShowMainWindow();
  Procedure HideMainWindow();
  procedure SetCaptionFPS(Ffps:QWORD);
  //
  procedure BindHandler(Handler:THostIpcHandler);
  function  OnCdlgSetMsg  (mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_MSG
  function  OnCdlgSetValue(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_VALUE
  function  OnCdlgClose   (mlen:DWORD;buf:Pointer):Ptruint; //CDLG_CLOSE
  procedure NewDialogOpen(var Attributes:TDialogAttributes;var pResult:TDialogCustom);
  //
  procedure OnMsgDialogClick(Sender:TObject);
  function  OnMsgDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //MSG_DIALOG_OPEN
  //
  procedure OnSaveDialogClick(Sender:TObject);
  function  OnSaveDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //SAVE_DIALOG_OPEN
  //
  procedure OnErrDlgClick(Sender:TObject);
  function  OnErrDlgOpen  (mlen:DWORD;buf:Pointer):Ptruint; //ERR_DIALOG_OPEN
  function  OnErrDlgClose (mlen:DWORD;buf:Pointer):Ptruint; //ERR_DIALOG_CLOSE
  function  OnErrDlgUpdate(mlen:DWORD;buf:Pointer):Ptruint; //ERR_DIALOG_UPDATE
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
 Handler.AddCallback('CDLG_SET_MSG'     ,@OnCdlgSetMsg);
 Handler.AddCallback('CDLG_SET_VALUE'   ,@OnCdlgSetValue);
 Handler.AddCallback('CDLG_CLOSE'       ,@OnCdlgClose);
 Handler.AddCallback('MSG_DIALOG_OPEN'  ,@OnMsgDialogOpen);
 Handler.AddCallback('SAVE_DIALOG_OPEN' ,@OnSaveDialogOpen);
 Handler.AddCallback('ERR_DIALOG_OPEN'  ,@OnErrDlgOpen);
 Handler.AddCallback('ERR_DIALOG_CLOSE' ,@OnErrDlgClose);
 Handler.AddCallback('ERR_DIALOG_UPDATE',@OnErrDlgUpdate);
end;

function TDialogsManager.OnCdlgSetMsg(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_MSG
var
 str:RawByteString;
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;
 if (FCommonDialog.FMsgMemo=nil) then Exit;

 str:='';
 SetLength(str,mlen);
 Move(buf^,str[1],mlen);

 FCommonDialog.FMsgMemo.Text:=str;
end;

function TDialogsManager.OnCdlgSetValue(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_SET_VALUE
var
 rate:DWORD;
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;
 if (FCommonDialog.FMsgPBar=nil) then Exit;

 if (mlen>SizeOf(DWORD)) then mlen:=SizeOf(DWORD);
 rate:=0;
 Move(buf^,rate,mlen);

 FCommonDialog.FMsgPBar.Position:=rate;
end;

function TDialogsManager.OnCdlgClose(mlen:DWORD;buf:Pointer):Ptruint; //CDLG_CLOSE
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;

 //What should the result code be?
 pContext^.SendAsyn('CDLG_FINISH',0,nil);

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

 pContext^.SendAsyn('CDLG_FINISH',SizeOf(rzdata),@rzdata);

 FreeAndNil(FCommonDialog);
end;

function TDialogsManager.OnMsgDialogOpen(mlen:DWORD;buf:Pointer):Ptruint; //MSG_DIALOG_OPEN
var
 data:TMsgDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 Assert(FCommonDialog=nil);

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

 pContext^.SendAsyn('CDLG_FINISH',SizeOf(rzdata),@rzdata);

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

 Assert(FCommonDialog=nil);

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

//
function TDialogsManager.OnErrDlgOpen(mlen:DWORD;buf:Pointer):Ptruint; //ERR_DIALOG_OPEN
var
 data:TErrDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 if (FErrorDialog<>nil) then Exit(-2);

 if (mlen>SizeOf(data)) then mlen:=SizeOf(data);
 FillChar(data,SizeOf(data),0);
 Move(buf^,data,mlen);

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

function TDialogsManager.OnErrDlgClose(mlen:DWORD;buf:Pointer):Ptruint; //ERR_DIALOG_CLOSE
begin
 Result:=0;
 FreeAndNil(FErrorDialog);
end;

function TDialogsManager.OnErrDlgUpdate(mlen:DWORD;buf:Pointer):Ptruint; //ERR_DIALOG_UPDATE
begin
 if (FErrorDialog<>nil) then
 begin
  Result:=0;
 end else
 begin
  Result:=1;
 end;
end;


end.

