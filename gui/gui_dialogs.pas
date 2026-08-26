unit gui_dialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  dateutils,
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
  ps4_libSceHmdSetupDialog,
  ps4_libSceErrorDialog,
  laz2ime,
  ps4_libSceImeDialog,
  ps4_libSceIme,
  ps4_libSigninDialog;

type
 TGameMainForm=class(TForm)
  public
   caption_format:RawByteString;
   procedure SetCaptionFPS(Ffps:QWORD);
   procedure WMEraseBkgnd(var Message:TLMEraseBkgnd); message LM_ERASEBKGND;
 end;

 {$M+}

 TDialogsManager=class
  public
   FImages  :TImageList;
   FContext :TGameRunContext;
   FMainForm:TGameMainForm;
   //
   FCommonDialog:TDialogCustom;
   FErrorDialog :TDialogCustom;
   FImeDialog   :TDialogCustom;
   FSigninDialog:TDialogCustom;
   //
   FImeData:TKeyStates;
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
   procedure NewDialogOpen(Client:THostIpc;var Attributes:TDialogAttributes;var pResult:TDialogCustom);
   procedure OnMsgDialogClick(Sender:TObject);
   procedure OnSaveDialogClick(Sender:TObject);
   function  SaveDialogLoadGrid(Client:THostIpc;var data:TSaveDialogOpen):TWinControl;
   procedure OnNpCommerceDialogClick(Sender:TObject);
   procedure OnHmdSetupDialogClick(Sender:TObject);
   procedure OnErrDlgClick(Sender:TObject);
   procedure OnImeDlgClick(Sender:TObject);
   procedure OnImeClick(Sender:TObject);
   procedure OnSigninDlgClick(Sender:TObject);
  published
   //All functions available for the IPC
   function  CDLG_SET_MSG  (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  CDLG_SET_VALUE(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  CDLG_CLOSE    (Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  MSG_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  SAVE_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  NPCOMMERCE_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  HMDSETUP_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  ERR_DIALOG_OPEN   (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  ERR_DIALOG_CLOSE  (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  ERR_DIALOG_UPDATE (Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  IME_DIALOG_OPEN   (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_TERM   (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_ABORT  (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_UPDATE (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_RESULT (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_GETTEXT(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_SETTEXT(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_DIALOG_GETPOS (Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  IME_OPEN     (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_CLOSE    (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_GETPOS   (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_UPDATE   (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_SET_CARET(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  IME_SET_TEXT (Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  SIGNIN_DIALOG_OPEN  (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  SIGNIN_DIALOG_CLOSE (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  SIGNIN_DIALOG_TERM  (Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  SIGNIN_DIALOG_UPDATE(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  SIGNIN_DIALOG_RESULT(Client:THostIpc;Value:TIpcValue):TIpcValue;
   //
   function  MAIN_WINDOWS(Client:THostIpc;Value:TIpcValue):TIpcValue;
   function  CAPTION_FPS (Client:THostIpc;Value:TIpcValue):TIpcValue;
  end;

 {$M-}

implementation

uses
 MsgDlgExt,
 game_mount,
 SceSaveData,
 SaveDataBackend;

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

 if (FContext.FGameItem=nil) then Exit;

 TITLE   :=FContext.FGameItem.FGameInfo.Name;
 TITLE_ID:=FContext.FGameItem.FGameInfo.TitleId;
 APP_VER :=FContext.FGameItem.FGameInfo.AppVer;

 if (TITLE='') then
 begin
  TITLE:=ExtractFileName(FContext.FGameItem.FGameInfo.Exec);
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
 FreeAndNil(FErrorDialog );
 FreeAndNil(FImeDialog   );
 FreeAndNil(FSigninDialog);
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
 Handler.AddPublished(Self);
end;

function TDialogsManager.CDLG_SET_MSG(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;
 if (FCommonDialog.FMsgMemo=nil) then Exit;

 FCommonDialog.FMsgMemo.Text:=Value.GetString;
end;

function TDialogsManager.CDLG_SET_VALUE(Client:THostIpc;Value:TIpcValue):TIpcValue;
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

function TDialogsManager.CDLG_CLOSE(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 if (FCommonDialog=nil) then Exit;

 //What should the result code be?
 Client.InvokeAsyn('CDLG_FINISH');

 FreeAndNil(FCommonDialog);
end;

procedure TDialogsManager.NewDialogOpen(Client:THostIpc;var Attributes:TDialogAttributes;var pResult:TDialogCustom);
begin
 Assert(pResult=nil,'NewDialogOpen');

 OpenMainWindows;
 Attributes.AParent:=FMainForm;
 Attributes.AImages:=FImages;

 pResult:=gui_dialog_fabric.NewDialogOpen(Attributes);
 pResult.FClient:=Client;
end;

procedure TDialogsManager.OnMsgDialogClick(Sender:TObject);
var
 rzdata:TMsgDialogResult;
begin
 if (FCommonDialog=nil) then Exit;

 rzdata.resultId:=0;
 rzdata.buttonId:=TCustomButton(Sender).Tag;
 if (rzdata.buttonId=0) then
 begin
  rzdata.resultId:=1;
 end;

 FCommonDialog.FClient.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

function TDialogsManager.MSG_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
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

 NewDialogOpen(Client,Attributes,FCommonDialog);
end;

type
 TSaveDataGridItem=class
  Icon   :TCustomBitmap;
  dirName:SceSaveDataDirName;
  params :SceSaveDataParam;
  infos  :SceSaveDataMountInfo;
  Destructor Destroy; override;
 end;

Destructor TSaveDataGridItem.Destroy;
begin
 FreeAndNil(Icon);
 inherited;
end;

type
 TSaveDataGrid=class(TStringGrid)
  public
   procedure  CustomDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
   procedure  PrepareCanvasEvent(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
   Destructor Destroy; override;
 end;

procedure TDialogsManager.OnSaveDialogClick(Sender:TObject);
var
 Grid  :TSaveDataGrid;
 Row   :Integer;
 Item  :TSaveDataGridItem;
 rzdata:TSaveDialogResult;
begin
 if (FCommonDialog=nil) then Exit;

 FillChar(rzdata,SizeOf(rzdata),0);
 rzdata.buttonId:=TCustomButton(Sender).Tag;
 if (rzdata.buttonId=0) then
 begin
  rzdata.resultId:=1;
 end;

 if (rzdata.resultId=0) then //OK
 if (FCommonDialog.FCustom<>nil) then
 begin
  Grid:=TSaveDataGrid(FCommonDialog.FCustom);
  Row:=Grid.Row;

  if (Row>=0) then
  begin
   Item:=TSaveDataGridItem(Grid.Objects[0,Row]);
   if (Item<>nil) then
   if (Item is TSaveDataGridItem) then
   begin
    rzdata.dirName:=Item.dirName;
    rzdata.params :=Item.params;
   end;
  end;

 end;

 FCommonDialog.FClient.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

procedure TSaveDataGrid.CustomDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
var
 Item:TSaveDataGridItem;
begin
 if (aCol=0) then
 begin
  Item:=TSaveDataGridItem(Objects[0,aRow]);
  if (Item<>nil) then
  if (Item is TSaveDataGridItem) then
  begin
   //PNG
   if (Item.Icon<>nil) then
   begin
    Canvas.StretchDraw(aRect,Item.Icon);
    Exit;
   end;
  end;
 end;
 //
 DefaultDrawCell(aCol,aRow,aRect,aState);
end;

procedure TSaveDataGrid.PrepareCanvasEvent(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
 ATextStyle: TTextStyle;
begin
 ATextStyle := Canvas.TextStyle;
 ATextStyle.SingleLine := false;
 ATextStyle.Wordbreak  := true;
 Canvas.TextStyle := ATextStyle;
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

procedure SaveDialogPrepare(Backend:TSaveDataBackendConnect;Client:THostIpc);
var
 Value :TIpcValue;
 Config:TGameMountConfigExport;
begin
 if (Backend=nil) then Exit;

 Value:=Client.InvokeSync('GetMountConfig');
 Config:=TGameMountConfigExport(Value.GetObject(TGameMountConfigExport));
 Value.Free;

 Backend.SendMountConfig(Config);
end;

procedure SaveDialogLoadDir(Backend :TSaveDataBackendConnect;
                            var data:TSaveDialogOpen;
                            dir_id  :Integer;
                            var Item:TSaveDataGridItem;
                            var Text:RawByteString);
var
 mount  :SceSaveDataMount;
 titleId:SceSaveDataTitleId;

 slot_id:Integer;
 i,ret:Integer;

 iconBuf:Pointer;
 icon:SceSaveDataIcon;

 Stream:TPCharStream;

 Params:SceSaveDataParam;
 infos :SceSaveDataMountInfo;
begin
 Item:=TSaveDataGridItem.Create;
 Item.dirName:=data.dirNames[dir_id];

 if (Backend=nil) then Exit;

 titleId.data:=data.titleId;

 mount:=Default(SceSaveDataMount);
 mount.userId   :=data.userId;
 mount.titleId  :=@titleId;
 mount.dirName  :=@data.dirNames[dir_id];
 mount.mountMode:=SDMM_RDONLY;

 slot_id:=0;
 for i:=0 to 9 do
 begin
  ret:=Backend.DoMountSys(@mount,slot_id);
  if (ret<>SCE_SAVE_DATA_ERROR_BACKUP_BUSY) then Break;
 end;

 if (ret<>0) then Exit;

 iconBuf:=AllocMem(116736);

 icon:=Default(SceSaveDataIcon);
 icon.buf     :=iconBuf;
 icon.bufSize :=116736;
 icon.dataSize:=116736;

 ret:=Backend.LoadIcon(slot_id,@icon,True);

 if (ret=0) and (icon.dataSize<>0) then
 begin
  Stream:=TPCharStream.Create(iconBuf,icon.dataSize);

  try
    Item.Icon:=TPortableNetworkGraphic.Create;
    Item.Icon.LoadFromStream(Stream);
  finally
    //
  end;

  FreeAndNil(Stream);
 end;

 FreeMem(iconBuf);

 ret:=Backend.GetParam(slot_id,
                       SCE_SAVE_DATA_PARAM_TYPE_ALL,
                       @Params,
                       $530,
                       nil);

 if (ret=0) then
 begin
  Item.params:=Params;

  infos:=Default(SceSaveDataMountInfo);
  ret:=Backend.GetMountInfoSys(slot_id,@infos);

  if (ret=0) then
  begin
   Item.infos:=infos;
  end;

  Text:=Params.title;

  if (data.itemStyle=SCE_SAVE_DATA_DIALOG_ITEM_STYLE_TITLE_SUBTITLE_DATESIZE) then
  begin
   Text:=Text + #13#10 + Params.subTitle;
  end;

  Text:=Text + #13#10 + DateTimeToStr(UnixToDateTime(Params.mtime,False))+' '+IntToStr((infos.blocks+31) div 32)+'MiB';

  if (data.itemStyle=SCE_SAVE_DATA_DIALOG_ITEM_STYLE_TITLE_DATESIZE_SUBTITLE) then
  begin
   Text:=Text + #13#10 + Params.subTitle;
  end;

 end;

 Backend.DoUmountSys(slot_id);
end;

function TDialogsManager.SaveDialogLoadGrid(Client:THostIpc;var data:TSaveDialogOpen):TWinControl;
var
 Grid:TSaveDataGrid;
 Item:TSaveDataGridItem;
 Stream:TPCharStream;
 i,p:Integer;
 Text:RawByteString;
begin
 SaveDialogPrepare(FContext.FetchSavdata,Client);

 Grid:=TSaveDataGrid.Create(nil);
 Grid.AutoSize:=True;
 Grid.OnDrawCell:=@Grid.CustomDrawCell;
 Grid.OnPrepareCanvas:=@Grid.PrepareCanvasEvent;
 Grid.AutoEdit:=False;
 Grid.AutoFillColumns:=TRue;
 Grid.BorderStyle:=bsNone;
 Grid.RowCount:=data.is_new + data.dirNameNum;
 Grid.ColCount:=2;
 Grid.FixedCols:=1;
 Grid.FixedRows:=0;
 Grid.GridLineWidth:=1;
 Grid.Options:=[goVertLine,goHorzLine,goRowSelect,goThumbTracking,goSmoothScroll];
 //
 Grid.Constraints.MinWidth :=228+Grid.GridLineWidth;
 Grid.Constraints.MinHeight:=128+Grid.GridLineWidth;
 //
 Grid.ColWidths[0]:=228+Grid.GridLineWidth;
 for i:=0 to Grid.RowCount-1 do
 begin
  Grid.RowHeights[i]:=128+Grid.GridLineWidth;
 end;
 //
 p:=0;
 if (data.is_new<>0) then
 begin
  Item:=TSaveDataGridItem.Create;

  if (data.new_item.iconSize<>0) then
  begin
   Stream:=TPCharStream.Create(@data.new_item.iconBuf,data.new_item.iconSize);

   try
     Item.Icon:=TPortableNetworkGraphic.Create;
     Item.Icon.LoadFromStream(Stream);
   finally
     //
   end;

   FreeAndNil(Stream);
  end;

  Grid.Cells  [1,0]:=data.new_item.title;
  Grid.Objects[0,0]:=Item;

  p:=1;
 end;
 //
 if (data.dirNameNum<>0) then
 for i:=0 to data.dirNameNum-1 do
 begin
  Item:=nil;
  Text :=data.dirNames[i].data;

  SaveDialogLoadDir(FContext.FetchSavdata,
                    data,
                    i,
                    Item,
                    Text);

  Grid.Cells  [1,p+i]:=Text;
  Grid.Objects[0,p+i]:=Item;
 end;

 //
 Result:=Grid;
end;

function TDialogsManager.SAVE_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;

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
  Attributes.Custom:=SaveDialogLoadGrid(Client,data);
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

 Attributes.ALittleMore:=True;

 NewDialogOpen(Client,Attributes,FCommonDialog);

 Client.InvokeAsyn('CDLG_READY');
end;

//

procedure TDialogsManager.OnNpCommerceDialogClick(Sender:TObject);
var
 rzdata:TNpCommerceDialogResult;
begin
 if (FCommonDialog=nil) then Exit;

 FillChar(rzdata,SizeOf(rzdata),0);

 case TDialogButtonId(TCustomButton(Sender).Tag) of
  btnIdCancel   :rzdata.resultId:=1;
  btnIdOkYesBtn1:rzdata.resultId:=2;
  else;
 end;

 rzdata.authorized:=False; //PS Plus features

 FCommonDialog.FClient.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

function  TDialogsManager.NPCOMMERCE_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
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
  begin
   Attributes.Memo.Message:=Attributes.Memo.Message+#13#10;
  end;
  Attributes.Memo.Message:=Attributes.Memo.Message+data.targets[i];
 end;

 NewDialogOpen(Client,Attributes,FCommonDialog);
end;

//

procedure TDialogsManager.OnHmdSetupDialogClick(Sender:TObject);
var
 rzdata:THmdSetupDialogResult;
begin
 if (FCommonDialog=nil) then Exit;

 FillChar(rzdata,SizeOf(rzdata),0);

 case TDialogButtonId(TCustomButton(Sender).Tag) of
  btnIdCancel   :rzdata.resultId:=1;
  btnIdOkYesBtn1:rzdata.resultId:=0;
  else;
 end;

 FCommonDialog.FClient.InvokeAsyn('CDLG_FINISH',@rzdata,SizeOf(rzdata));

 FreeAndNil(FCommonDialog);
end;

function TDialogsManager.HMDSETUP_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:THmdSetupDialogOpen;
 Attributes:TDialogAttributes;
begin
 Result:=0;

 Assert(FCommonDialog=nil);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 Attributes.OnClick:=@OnHmdSetupDialogClick;

 Attributes.Caption.Enable:=True;
 Attributes.Caption.Message:='Hmd Setup';

 Attributes.CloseButton.Enable:=True;
 Attributes.CloseButton.btnId :=btnIdCancel;

 Attributes.Buttons.Enable :=True;
 Attributes.Buttons.BtnType:=btnOk;

 Attributes.Memo.Enable :=True;
 Attributes.Memo.Message:='Connect VR and turn it on';

 NewDialogOpen(Client,Attributes,FCommonDialog);
end;

//

//
function TDialogsManager.ERR_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
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

 NewDialogOpen(Client,Attributes,FErrorDialog);
end;

procedure TDialogsManager.OnErrDlgClick(Sender:TObject);
begin
 FreeAndNil(FErrorDialog);
end;

function TDialogsManager.ERR_DIALOG_CLOSE(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 FreeAndNil(FErrorDialog);
end;

function TDialogsManager.ERR_DIALOG_UPDATE(Client:THostIpc;Value:TIpcValue):TIpcValue;
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

procedure TDialogsManager.OnImeDlgClick(Sender:TObject);
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

function TDialogsManager.IME_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeDialogOpen;
 Attributes:TDialogAttributes;
 Ime:TImeDialogAttributes;
begin
 Result:=0;

 if (FImeDialog<>nil) then Exit(-2);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 FillChar(Ime,SizeOf(Ime),0);
 Attributes.OnClick:=@OnImeDlgClick;

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

 NewDialogOpen(Client,Attributes,TDialogCustom(FImeDialog));

 FImeData.ImeDlgOpen(TImeDialog(FImeDialog));
end;

function TDialogsManager.IME_DIALOG_TERM(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 FreeAndNil(FImeDialog);
end;

function TDialogsManager.IME_DIALOG_ABORT(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 if (FImeDialog<>nil) then
 begin
  FImeDialog.button:=2; //STATUS_ABORTED
  FImeDialog.state :=2;
  FImeDialog.Hide;
 end;
end;

function TDialogsManager.IME_DIALOG_UPDATE(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  Result:=FImeDialog.state;
 end;
end;

function TDialogsManager.IME_DIALOG_RESULT(Client:THostIpc;Value:TIpcValue):TIpcValue;
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

function TDialogsManager.IME_DIALOG_GETTEXT(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeDialogTextToFilter;
 w:WideString;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  if FImeData.ime_change then
  begin
   FillChar(data,SizeOf(data),0);
   //
   w:=UTF8Decode(FImeDialog.FMsgMemo.Text);
   if (Length(w)>120) then SetLength(w,120);
   //
   data.result    :=1;
   data.Text      :=w;
   data.TextLength:=Length(w);
   Result:=TIpcValue.New(@data,SizeOf(data));
   //
   FImeData.ime_change:=False;
  end else
  begin
   Result:=0;
  end;
 end;
end;

function TDialogsManager.IME_DIALOG_SETTEXT(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeDialogTextToFilter;
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

function TDialogsManager.IME_DIALOG_GETPOS(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeDialogPosAndForm;
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

procedure TDialogsManager.OnImeClick(Sender:TObject);
var
 buttonId:TDialogButtonId;
begin
 if (FImeDialog<>nil) then
 begin
  buttonId:=TDialogButtonId(TCustomButton(Sender).Tag);

  if (buttonId=btnIdCancel) then
  begin
   FImeData.ime_queue.PushClose();
  end else
  begin
   FImeData.ime_queue.PushEnter();
  end;

  FImeData.ime_input:=False;
  FImeDialog.Hide;
 end;
end;

function TDialogsManager.IME_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeOpen;
 Attributes:TDialogAttributes;
 Ime:TImeDialogAttributes;
 w:WideString;
begin
 Result:=0;

 if (FImeDialog<>nil) then Exit(-2);

 FillChar(data,SizeOf(data),0);
 Value.MoveTo(@data,SizeOf(data));

 FillChar(Attributes,SizeOf(Attributes),0);
 FillChar(Ime,SizeOf(Ime),0);
 Attributes.OnClick:=@OnImeClick;

 w:=WideString(data.inputText);

 Attributes.Caption.Enable :=False;
 //
 Attributes.CloseButton.Enable:=True;
 Attributes.CloseButton.btnId :=btnIdCancel;
 //
 Attributes.Memo.Enable :=True;
 Attributes.Memo.Message:=UTF8Encode(w);
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

 NewDialogOpen(Client,Attributes,TDialogCustom(FImeDialog));

 FImeData.ImeOpen(TImeDialog(FImeDialog),w);
end;

function TDialogsManager.IME_CLOSE(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 FImeData.ime_input:=False;
 Result:=0;
 FreeAndNil(FImeDialog);
end;

function TDialogsManager.IME_GETPOS(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeDialogPosAndForm;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  data.PanelType          :=1;
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

function TDialogsManager.IME_UPDATE(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeEvent;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  if FImeData.ime_queue.Pop(data.event) then
  begin
   data.valid:=1;
   Result:=TIpcValue.New(@data,SizeOf(data));
  end else
  begin
   data.valid:=0;
   Result:=TIpcValue.New(@data.valid,SizeOf(data.valid));
  end;
 end;
end;

function TDialogsManager.IME_SET_CARET(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 data:TImeSetCaret;
 i:Integer;
 CaretPos:TPoint;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  Value.MoveTo(@data,SizeOf(data));

  CaretPos:=Default(TPoint);

  //calc multiline caret
  if (Length(FImeData.input)<>0) then
  For i:=0 to High(FImeData.input) do
  begin
   if (i=data.index) then Break;
   if (FImeData.input[i]=#13) then
   begin
    CaretPos.X:=0;
    Inc(CaretPos.Y);
   end else
   begin
    Inc(CaretPos.X);
   end;
  end;

  FImeDialog.FMsgMemo.CaretPos:=CaretPos;

  if (data.mode=1) then
  begin
   FImeData.ime_queue.PushPreedit(data.index);
  end;

  Result:=0;
 end;
end;

function TDialogsManager.IME_SET_TEXT(Client:THostIpc;Value:TIpcValue):TIpcValue;
var
 w:WideString;
 s:RawByteString;
 CaretPos:TPoint;
begin
 Result:=-1;
 if (FImeDialog<>nil) then
 begin
  SetLength(w,Value.GetLen div SizeOf(WideChar));
  Move(Value.GetBuf^,w[1],Value.GetLen);
  s:=UTF8Encode(w);

  FImeData.input:=w;

  if (s<>FImeDialog.FMsgMemo.Text) then
  begin
   CaretPos:=FImeDialog.FMsgMemo.CaretPos;
   FImeDialog.FMsgMemo.Text:=s;
   FImeDialog.FMsgMemo.CaretPos:=CaretPos;
  end;

  Result:=0;
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

function TDialogsManager.SIGNIN_DIALOG_OPEN(Client:THostIpc;Value:TIpcValue):TIpcValue;
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

 NewDialogOpen(Client,Attributes,FSigninDialog);
end;

function TDialogsManager.SIGNIN_DIALOG_CLOSE(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 if (FSigninDialog<>nil) then
 begin
  FSigninDialog.button:=1; //STATUS_USER_CANCELED
  FSigninDialog.state :=1;
  FSigninDialog.Hide;
 end;
end;

function TDialogsManager.SIGNIN_DIALOG_TERM(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 FreeAndNil(FSigninDialog);
end;

function TDialogsManager.SIGNIN_DIALOG_UPDATE(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=-1;
 if (FSigninDialog<>nil) then
 begin
  Result:=FSigninDialog.state;
 end;
end;

function TDialogsManager.SIGNIN_DIALOG_RESULT(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=-1;
 if (FSigninDialog<>nil) then
 begin
  Result:=FSigninDialog.button;
 end;
end;

//

function TDialogsManager.MAIN_WINDOWS(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=OpenMainWindows;
end;

function TDialogsManager.CAPTION_FPS(Client:THostIpc;Value:TIpcValue):TIpcValue;
begin
 Result:=0;
 SetCaptionFPS(Value.GetQWORD);
end;


end.

