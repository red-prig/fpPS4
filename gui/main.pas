unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids, Menus,

  StdCtrls,
  LCLType,
  LCLIntf,

  g_bufstream,
  LineStream,
  synlog,
  SynEditLineStream,
  LazSynEditText,
  SynEditMarkupBracket,

  TypInfo,
  jsonscanner,

  ms_shell_hack,

  core_serialization,
  host_ipc,
  game_info,
  game_edit,
  cfg_edit,
  game_run_context,
  game_run,

  param_sfo_gui,
  playgo_chunk_gui,

  core_shell,
  gui_dialogs,

  ps4_libSceMsgDialog;

type
  TMainButtonsState=(mbsStopped,
                     mdsStarted,
                     mdsRunned,
                     mdsSuspended);

  TGameList=class(TSerializeArray)
   FGrid: TStringGrid;
   //
   function  GetItem(i:SizeInt):TGameItem;
   function  GetItemRow(i:SizeInt):TGameItem;
   procedure AddItem(Item:TGameItem);
   procedure InsertItem(Item:TGameItem);
   procedure UpdateItem(i:SizeInt);
   procedure UpdateItem(Item:TGameItem);
   procedure DelItem(Item:TGameItem);
   //
   Function  GetArrayCount:SizeInt;          override;
   Function  GetArrayItem(i:SizeInt):TValue; override;
   Function  AddObject:TSerializeObject;     override;
   Function  AddArray :TSerializeArray;      override;
   procedure AddValue(Value:TValue);         override;
  end;

  TGameListObject=class(TSerializeObject)
   private
    FGameList:TGameList;
   published
    property GameList:TGameList read FGameList write FGameList;
   public
    Procedure CreateSub;  override;
    Procedure DestroySub; override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    MainImageList: TImageList;
    SmallImageList: TImageList;
    MIFind: TMenuItem;
    MIShowExplorer: TMenuItem;
    MIDevide3: TMenuItem;
    MIRun: TMenuItem;
    MIEdit: TMenuItem;
    MIDevide1: TMenuItem;
    MenuList: TPopupMenu;
    MIAdd: TMenuItem;
    MIAddFolder: TMenuItem;
    MIDel: TMenuItem;
    MIDevide2: TMenuItem;
    Pages: TPageControl;
    ListGrid: TStringGrid;
    TabList: TTabSheet;
    TabLog: TTabSheet;
    MainToolBar: TToolBar;
    TBPlay: TToolButton;
    TBPause: TToolButton;
    TBStop: TToolButton;
    TBConfig: TToolButton;
    TBSep1: TToolButton;
    TBAddFolder: TToolButton;
    TBSep2: TToolButton;
    TBDown: TToolButton;
    TBUp: TToolButton;
    TBSep3: TToolButton;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ListGridDblClick(Sender: TObject);
    procedure ListGridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListGridEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MIFindClick(Sender: TObject);
    procedure MIShowExplorerClick(Sender: TObject);
    procedure OnIdleUpdate(Sender:TObject;var Done:Boolean);
    procedure MIAddClick(Sender: TObject);
    procedure MIAddFolderClick(Sender: TObject);
    procedure MIDelClick(Sender: TObject);
    procedure MIEditClick(Sender: TObject);
    procedure TBConfigClick(Sender: TObject);
    procedure MIRunClick(Sender: TObject);
    procedure TBPauseClick(Sender: TObject);
    procedure TBPlayClick(Sender: TObject);
    procedure TBStopClick(Sender: TObject);
    procedure TBDownClick(Sender: TObject);
    procedure TBUpClick(Sender: TObject);
  private
    FDblClickRow:Integer;
  public
    FGameList:TGameList;
    FContext :TGameRunContext;

    FConfigInfo:TConfigInfo;

    FAddHandle:THandle;
    FGetHandle:THandle;

    FFile:TStream;
    FList:TSynEditLineStream;

    Fmlog:TCustomSynLog;

    FLogUpdateTime:QWORD;

    FMainButtonsState:TMainButtonsState;

    FDialogsManager:TDialogsManager;

    function  OnKevent       (Value:TIpcValue):TIpcValue; //KEV_EVENT
    function  OnMainWindows  (Value:TIpcValue):TIpcValue; //MAIN_WINDOWS
    function  OnCaptionFPS   (Value:TIpcValue):TIpcValue; //CAPTION_FPS
    function  OnError        (Value:TIpcValue):TIpcValue; //ERROR
    function  OnWarning      (Value:TIpcValue):TIpcValue; //WARNING
    function  OnParamSfoInit (Value:TIpcValue):TIpcValue; //PARAM_SFO_INIT
    function  OnPlaygoInit   (Value:TIpcValue):TIpcValue; //PLAYGO_INIT
    function  OnLoadExec     (Value:TIpcValue):TIpcValue; //LOAD_EXEC

    procedure OpenLog(Const LogFile:RawByteString);
    procedure ReadConfigFile;
    procedure SaveGameList;
    procedure DoAdd(Sender: TObject);
    procedure DoEdit(Sender: TObject);
    procedure DoConfigSave(Sender: TObject);
    procedure LogEnd;
    procedure ClearLog;
    function  GameProcessForked:Boolean;
    procedure SetButtonsState(s:TMainButtonsState);
  end;

var
  frmMain: TfrmMain;

implementation

uses

 game_find,

 md_file,

 md_arc4random,

 vDevice,

 sys_event;

//

{$R *.lfm}

Const
 fpps4File   ='fpps4.json';
 GameListFile='GameList.json';

type
 TMySynLog=class(TCustomSynLog)
  Form:TfrmMain;
  constructor Create(AOwner: TComponent; AForm:TfrmMain);
  function    LinesCreate:TSynEditStringListBase; override;
 end;

constructor TMySynLog.Create(AOwner: TComponent; AForm:TfrmMain);
begin
 Form:=AForm;
 inherited Create(AOwner);
end;

function TMySynLog.LinesCreate:TSynEditStringListBase;
begin
 Form.FList:=TSynEditLineStream.Create;

 Form.FList.FSynLog:=Self;
 Form.FList.FStream:=TLineStream.Create(Form.FFile);

 Result:=Form.FList;
end;

const
 section_prefix='game-';

const
 MsgDlgBtnToStr: array[TMsgDlgBtn] of PChar = (
  '&Yes',
  '&No',
  '&OK',
  '&Cancel',
  '&Abort',
  '&Retry',
  '&Ignore',
  '&All',
  '&NoToAll',
  '&YesToAll',
  '&Help',
  '&Close'
 );

 MsgDlgBtnToResult: array[TMsgDlgBtn] of Byte = (
  mrYes,
  mrNo,
  mrOK,
  mrCancel,
  mrAbort,
  mrRetry,
  mrIgnore,
  mrAll,
  mrNoToAll,
  mrYesToAll,
  mrNone, //Help
  mrClose
 );

type
 TMsgDlgAButtons=array of TMsgDlgBtn;

function MessageDlgEx(const AMsg:RawByteString;
                      const ACaption:RawByteString;
                      AButtons:TMsgDlgAButtons;
                      AParent:TForm):TModalResult;
var
 MsgForm:TForm;
 MsgMemo:TMemo;
 MsgBtnz:TButton;

 //(asrTop, asrBottom, asrCenter);
 Procedure NewBtn(DlgType:TMsgDlgBtn;DlgPos:TAnchorSideReference);
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
  MsgBtnz.Caption    :=MsgDlgBtnToStr[DlgType];
  MsgBtnz.Parent     :=MsgForm;
  MsgBtnz.ModalResult:=MsgDlgBtnToResult[DlgType];
 end;

begin
 MsgBtnz:=nil;

 MsgForm:=TForm.Create(nil);
 try
  MsgForm.Caption    :=ACaption;
  MsgForm.Position   :=poDesigned;
  MsgForm.BorderIcons:=[biSystemMenu];
  MsgForm.FormStyle  :=fsSystemStayOnTop;
  MsgForm.Left:= AParent.Left + (AParent.Width  - MsgForm.Width ) div 2;
  MsgForm.Top := AParent.Top  + (AParent.Height - MsgForm.Height) div 2;
  MsgForm.Width :=400;
  MsgForm.Height:=200;
  //
  Case Length(AButtons) of
   0:;
   1:
     begin
      NewBtn(AButtons[0],asrTop);
     end;
   2:
     begin
      NewBtn(AButtons[0],asrTop);
      NewBtn(AButtons[1],asrBottom);
     end;
   3:
     begin
      NewBtn(AButtons[0],asrTop);
      NewBtn(AButtons[1],asrCenter);
      NewBtn(AButtons[2],asrBottom);
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
  MsgMemo.AnchorSide[akTop   ].Control:=MsgForm;
  MsgMemo.AnchorSide[akTop   ].Side   :=asrTop;
  MsgMemo.AnchorSide[akLeft  ].Control:=MsgForm;
  MsgMemo.AnchorSide[akLeft  ].Side   :=asrTop;
  MsgMemo.AnchorSide[akRight ].Control:=MsgForm;
  MsgMemo.AnchorSide[akRight ].Side   :=asrBottom;
  MsgMemo.AnchorSide[akBottom].Control:=MsgForm;
  MsgMemo.AnchorSide[akBottom].Side   :=asrBottom;
  if (MsgBtnz<>nil) then
  begin
   MsgMemo.AnchorSide[akBottom].Control:=MsgBtnz;
   MsgMemo.AnchorSide[akBottom].Side   :=asrTop;
  end;
  MsgMemo.BorderSpacing.Bottom:=10;
  //
  MsgMemo.Text  :=AMsg;
  MsgMemo.Parent:=MsgForm;
  //
  Result:=MsgForm.ShowModal;
 finally
  MsgForm.Free;
 end;

 {
 MsgFrm:=CreateMessageDialog(AMsg, ADlgType, AButtons);
 try
  MsgFrm.Position :=poDefaultSizeOnly;
  MsgFrm.FormStyle:=fsSystemStayOnTop;
  MsgFrm.Left:= AParent.Left + (AParent.Width  - MsgFrm.Width ) div 2;
  MsgFrm.Top := AParent.Top  + (AParent.Height - MsgFrm.Height) div 2;
  Result:=MsgFrm.ShowModal;
 finally
  MsgFrm.Free
 end;
 }
end;

var
 IpcHandler:THostIpcHandler;

function TfrmMain.OnMainWindows(Value:TIpcValue):TIpcValue; //MAIN_WINDOWS
begin
 Result:=FDialogsManager.OpenMainWindows;
end;

function TfrmMain.OnCaptionFPS(Value:TIpcValue):TIpcValue; //CAPTION_FPS
begin
 Result:=0;
 FDialogsManager.SetCaptionFPS(Value.GetQWORD);
end;

function TfrmMain.OnKevent(Value:TIpcValue):TIpcValue; //KEV_EVENT
var
 kev:p_kevent;
 count:Integer;

 i:Integer;
begin
 Result:=0;

 kev  :=Value.GetBuf;
 count:=Value.GetLen div sizeof(t_kevent);

 i:=0;
 while (i<>count) do
 begin
  case kev[i].filter of
   EVFILT_PROC:
     begin
      if ((kev[i].fflags and NOTE_EXIT)<>0) then
      begin
       //ShowMessage('NOTE_EXIT pid:'+IntToStr(kev[i].ident));
       ShowMessage('The process reported exit!');
       FContext.Stop();
      end;
      if ((kev[i].fflags and NOTE_EXEC)<>0) then
      begin
       //ShowMessage('NOTE_EXEC pid:'+IntToStr(kev[i].ident));
       SetButtonsState(mdsRunned);
      end;
     end;

   else;
  end;

  Inc(i);
 end;

end;

function TfrmMain.OnError(Value:TIpcValue):TIpcValue; //ERROR
begin
 Result:=0;
 if (MessageDlgEx(Value.GetString,'Error',[mbOK,mbAbort],Self)=mrAbort) then
 begin
  FContext.Stop();
 end;
end;

function TfrmMain.OnWarning(Value:TIpcValue):TIpcValue; //WARNING
var
 i:Integer;
begin
 i:=MessageDlgEx(Value.GetString,'Warning',[mbYes,mbNo,mbAbort],Self);
 if (i=mrAbort) then
 begin
  FContext.Stop();
 end;
 if (i=mrYes) then
 begin
  i:=0;
 end;
 Result:=i;
end;

function LoadParamSfoFile2(const game:RawByteString):TParamSfoFile;
begin
 Result:=LoadParamSfoFile(ExcludeTrailingPathDelimiter(game)+
                          DirectorySeparator+
                          'sce_sys'+
                          DirectorySeparator+
                          'param.sfo');
end;

function TfrmMain.OnParamSfoInit(Value:TIpcValue):TIpcValue; //PARAM_SFO_INIT
var
 V:RawByteString;
begin
 Result:=0;

 if (FContext.FGameItem=nil) then Exit;

 if (FContext.FParamSfo=nil) then
 begin
  FContext.FParamSfo:=LoadParamSfoFile2(FContext.FGameItem.MountList.game);
 end;

 if (FContext.FParamSfo=nil) then
 begin
  V:='"{$GAME}/sce_sys/param.sfo" not found, continue?';

  if (MessageDlgEx(V,'Error',[mbOK,mbAbort],Self)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   TBStopClick(nil);
   Exit(0);
  end;
 end;

 Result:=TIpcValue.&Object(FContext.FParamSfo);
end;

function TfrmMain.OnPlaygoInit(Value:TIpcValue):TIpcValue; //PLAYGO_INIT
var
 playgo_file:TPlaygoFile;
 V:RawByteString;
begin
 Result:=0;

 if (FContext.FGameItem=nil) then Exit;

 V:=FContext.FGameItem.MountList.game;

 playgo_file:=LoadPlaygoFile(ExcludeTrailingPathDelimiter(V)+
                             DirectorySeparator+
                             'sce_sys'+
                             DirectorySeparator+
                             'playgo-chunk.dat');

 if (playgo_file=nil) then
 begin
  V:='"{$GAME}/sce_sys/playgo-chunk.dat" not found, continue?';

  if (MessageDlgEx(V,'Error',[mbOK,mbAbort],Self)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   TBStopClick(nil);
   Exit(0);
  end;
 end;

 Result:=TIpcValue.&Object(playgo_file);
 FreeAndNil(playgo_file);
end;

function TfrmMain.OnLoadExec(Value:TIpcValue):TIpcValue; //LOAD_EXEC
var
 data:TPS4LoadExec;
 cfg:TGameRunConfig;
 Item:TGameItem;
 r:Integer;
begin
 Result:=0;

 data:=TPS4LoadExec(Value.GetObject(TPS4LoadExec));
 if (data=nil) then Exit;

 if (FContext.FGameItem=nil) or
    (FContext.FGameProcess=nil) then
 begin
  FreeAndNil(data);
  Exit;
 end;

 if (UpperCase(data.Path)='EXIT') then
 begin
  FreeAndNil(data);
  FContext.Stop();
  Exit;
 end;

 if GameProcessForked then //only forked
 begin

  //terminate
  FContext.StopAndNil();
  //
  FDialogsManager.CloseMainWindow;
  //

  //re-run

  Item:=TGameItem.Create;
  FContext.FGameItem.CopyTo(Item);

  Item.GameInfo.Exec:=encode_shell(data.Path)+' '+encode_shell(data.argv);

  cfg:=Default(TGameRunConfig);

  cfg.hOutput:=FAddHandle;
  cfg.hError :=FAddHandle;

  cfg.FConfInfo:=FConfigInfo;
  cfg.FGameItem:=Item;
  cfg.FParamSfo:=FContext.FParamSfo;
  cfg.FLoadExec:=True;

  r:=run_item(cfg,FContext);
  if (r<>0) then
  begin
   ShowMessage('error run process code=0x'+HexStr(r,8));
  end;

  FContext.BindHandler(IpcHandler);

  FreeAndNil(Item);

  if (r<>0) then
  begin
   //stop on error
   TBStopClick(Self);
  end;

 end else
 begin
  MessageDlgEx('LoadExec is not supported for the current process','Error',[mbOK],Self);
 end;

 FreeAndNil(data);
end;

//ShowMessage(GetEnumName(TypeInfo(mtype),ord(mtype)));

//

function TGameList.GetItem(i:SizeInt):TGameItem;
begin
 i:=i+1;
 if (i<=0) or (i>=FGrid.RowCount) then
 begin
  Result:=nil;
 end else
 begin
  Result:=TGameItem(FGrid.Objects[0,i]);
 end;
end;

function TGameList.GetItemRow(i:SizeInt):TGameItem;
begin
 if (i<0) or (i>FGrid.RowCount) then
 begin
  Result:=nil;
 end else
 begin
  Result:=TGameItem(FGrid.Objects[0,i]);
 end;
end;

procedure TGameList.AddItem(Item:TGameItem);
var
 i:SizeInt;
begin
 i:=FGrid.RowCount;
 FGrid.RowCount:=i+1;
 //
 FGrid.Cells[0,i]:=Item.FGameInfo.Name;
 FGrid.Cells[1,i]:=Item.FGameInfo.TitleId;
 FGrid.Cells[2,i]:=Item.FGameInfo.AppVer;
 //
 FGrid.Objects[0,i]:=Item;
end;

procedure TGameList.InsertItem(Item:TGameItem);
var
 i:SizeInt;
begin
 i:=FGrid.Row;
 if (i<1) then i:=1;

 FGrid.InsertColRow(False,i);

 //
 FGrid.Cells[0,i]:=Item.FGameInfo.Name;
 FGrid.Cells[1,i]:=Item.FGameInfo.TitleId;
 FGrid.Cells[2,i]:=Item.FGameInfo.AppVer;
 //
 FGrid.Objects[0,i]:=Item;
 //
 FGrid.Row:=i;
end;

procedure TGameList.UpdateItem(i:SizeInt);
var
 Item:TGameItem;
begin
 i:=i+1;
 if (i<=0) or (i>=FGrid.RowCount) then Exit;
 //
 Item:=TGameItem(FGrid.Objects[0,i]);
 //
 FGrid.Cells[0,i]:=Item.FGameInfo.Name;
 FGrid.Cells[1,i]:=Item.FGameInfo.TitleId;
 FGrid.Cells[2,i]:=Item.FGameInfo.AppVer;
end;

procedure TGameList.UpdateItem(Item:TGameItem);
var
 i:SizeInt;
begin
 i:=FGrid.Cols[0].IndexOfObject(Item);
 if (i=-1) then Exit;
 //
 FGrid.Cells[0,i]:=Item.FGameInfo.Name;
 FGrid.Cells[1,i]:=Item.FGameInfo.TitleId;
 FGrid.Cells[2,i]:=Item.FGameInfo.AppVer;
end;

procedure TGameList.DelItem(Item:TGameItem);
var
 i:SizeInt;
begin
 i:=FGrid.Cols[0].IndexOfObject(Item);
 if (i=-1) then Exit;
 //
 FGrid.DeleteRow(i);
 //
 Item.Free;
end;

Function TGameList.GetArrayCount:SizeInt;
begin
 Result:=FGrid.RowCount;
 if (Result<>0) then Dec(Result);
end;

Function TGameList.GetArrayItem(i:SizeInt):TValue;
begin
 i:=i+1;
 if (i>=FGrid.RowCount) then
 begin
  Result:=TValue.Empty;
 end else
 begin
  Result:=TGameItem(FGrid.Objects[0,i]);
 end;
end;

Function TGameList.AddObject:TSerializeObject;
begin
 Result:=TGameItem.Create;
 //
 AddItem(TGameItem(Result));
end;

Function TGameList.AddArray:TSerializeArray;
begin
 Result:=nil;
end;

procedure TGameList.AddValue(Value:TValue);
begin
 //
end;

//

Procedure TGameListObject.CreateSub;
begin
 //
end;

Procedure TGameListObject.DestroySub;
begin
 //
end;

//

procedure TfrmMain.ReadConfigFile;
var
 m:TMemoryStream;
 JReader:TJSONStreamReader;
 obj:TGameListObject;

 i,c:Integer;
begin
 FConfigInfo:=TConfigInfo.Create;

 FGameList:=TGameList.Create;
 FGameList.FGrid:=ListGrid;

 //load config
 if FileExists(fpps4File) then
 begin
  m:=nil;
  JReader:=nil;
  try
   m:=TMemoryStream.Create;
   m.LoadFromFile(fpps4File);
   JReader:=TJSONStreamReader.Create(m,[joUTF8,joComments]);
   JReader.Execute(FConfigInfo);
  except
   on E: Exception do
     MessageDlgEx(E.Message,'Error',[mbOK],Self);
  end;
  FreeAndNil(JReader);
  FreeAndNil(m);
 end;

 //load game list
 if FileExists(GameListFile) then
 begin
  obj:=TGameListObject.Create;
  obj.GameList:=FGameList;

  m:=nil;
  JReader:=nil;
  try
   m:=TMemoryStream.Create;
   m.LoadFromFile(GameListFile);
   JReader:=TJSONStreamReader.Create(m,[joUTF8,joComments]);
   JReader.Execute(obj);
  except
   on E: Exception do
     MessageDlgEx(E.Message,'Error',[mbOK],Self);
  end;
  FreeAndNil(JReader);
  FreeAndNil(m);

  FreeAndNil(obj);
 end;

 //update grid
 C:=FGameList.GetArrayCount;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   FGameList.UpdateItem(i);
  end;
 end;
end;

//

procedure TfrmMain.SaveGameList;
var
 list:TGameList;
 m:TMemoryStream;
 jstream:TJSONStreamWriter;
begin
 list:=TGameList.Create;
 list.FGrid:=ListGrid;

 m:=TMemoryStream.Create;
 jstream:=TJSONStreamWriter.Create(m);

 jstream.WriteStartObject('');
 list.WriteJSON('GameList',jstream);
 jstream.WriteStopObject;
 FreeAndNil(jstream);
 FreeAndNil(list);

 try
  M.SaveToFile(GameListFile);
 except
  on E: Exception do
    MessageDlgEx(E.Message,'Error',[mbOK],Self);
 end;

 FreeAndNil(M);
end;

procedure TfrmMain.OpenLog(Const LogFile:RawByteString);
begin
 md_open(LogFile,O_RDWR or O_CREAT or O_TRUNC,&0777,FAddHandle);
 md_openat(FAddHandle,'',O_RDONLY,0,FGetHandle);

 FileSeek(FAddHandle,0,fsFromEnd);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
 r:RawByteString;
begin
 FDialogsManager.FImages :=SmallImageList;
 FDialogsManager.pContext:=@FContext;

  ListGrid.Canvas.Font.Size:=GetRealFontSize(ListGrid.Canvas.Font);

  with ListGrid.Columns[1] do
  begin
   MaxSize:=ListGrid.Canvas.GetTextWidth('MCUSA00000M');;
  end;

  with ListGrid.Columns[2] do
  begin
   MaxSize:=ListGrid.Canvas.GetTextWidth('M00.00M');
  end;

 IpcHandler:=THostIpcHandler.Create;

 IpcHandler.AddCallback('KEV_EVENT'      ,@OnKevent      );
 IpcHandler.AddCallback('MAIN_WINDOWS'   ,@OnMainWindows );
 IpcHandler.AddCallback('CAPTION_FPS'    ,@OnCaptionFPS  );
 IpcHandler.AddCallback('ERROR'          ,@OnError       );
 IpcHandler.AddCallback('WARNING',        @OnWarning     );
 IpcHandler.AddCallback('PARAM_SFO_INIT' ,@OnParamSfoInit);
 IpcHandler.AddCallback('PLAYGO_INIT'    ,@OnPlaygoInit  );
 IpcHandler.AddCallback('LOAD_EXEC'      ,@OnLoadExec    );

 FDialogsManager.BindHandler(IpcHandler);

 ReadConfigFile;

 OpenLog(FConfigInfo.MainInfo.LogFile);

 if (Application.Tag<>0) then
 begin
  r:='Critical error, memory could not be reserved! code=0x'+HexStr(Application.Tag,8)+#13#10;
  FileWrite(FAddHandle,PChar(r)^,Length(r));
  ShowMessage(r);
  Halt;
 end;

 FFile:=TBufferedFileStream.Create(FGetHandle);

 Fmlog:=TMySynLog.Create(TabLog,Self);
 Fmlog.Parent:=TabLog;

 Fmlog.Align:=alClient;

 Fmlog.BracketHighlightStyle:=sbhsBoth;

 Fmlog.Font.Style:=[];
 Fmlog.Font.Name:='Courier New';
 Fmlog.Font.Size:=GetRealFontSize(Font) + 2;

 Pages.ActivePageIndex:=0;

 Application.AddOnIdleHandler(@OnIdleUpdate,False);

 SetButtonsState(mbsStopped);

 //InitVulkan;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 TBPauseClick(Sender);
 //
 CloseAction:=caFree;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
 aRow:Integer;
begin
 if (Shift=[ssAlt]) then
 begin
  case Key of
   VK_R   :TBPlayClick (Sender);
   VK_S   :TBStopClick (Sender);
   VK_P   :TBPauseClick(Sender);
   VK_DOWN:
     begin
      aRow:=ListGrid.Row;
      TBDownClick(Sender);
      ListGrid.Row:=aRow;
     end;
   VK_UP:
     begin
      aRow:=ListGrid.Row;
      TBUpClick(Sender);
      ListGrid.Row:=aRow;
     end
   else;
  end;
 end else
 if (Shift=[ssCtrl]) then
 begin
  case Key of
   VK_F:MIFindClick(Sender);
  end;
 end else
 if (Shift=[]) then
 begin
  case Key of
   VK_RETURN:MIEditClick     (Sender);
   VK_INSERT:MIAddFolderClick(Sender);
   VK_DELETE:MIDelClick      (Sender);
   else;
  end;
 end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
 ListGrid.SetFocus;
end;

procedure TfrmMain.ListGridDblClick(Sender: TObject);
begin
 if (FDblClickRow=ListGrid.Row) then
 begin
  MIEditClick(Sender);
 end;
end;

procedure TfrmMain.ListGridMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 if (ssDouble in Shift) then
 begin
  FDblClickRow:=ListGrid.MouseToCell(TPoint.Create(X,Y)).Y;
 end;
end;

procedure TfrmMain.MIFindClick(Sender: TObject);
begin
 game_find.FrmFind.ListGrid:=ListGrid;
 game_find.FrmFind.Show;
end;

procedure TfrmMain.ListGridEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
 SaveGameList;
end;

procedure TfrmMain.ListGridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
 aRow:Integer;
 RowTo:Integer;
begin
 if (Sender=Source) then
 begin
  aRow:=ListGrid.Row;
  RowTo:=ListGrid.MouseToCell(TPoint.Create(X,Y)).Y;
  //
  if (RowTo>0) and
     (RowTo<ListGrid.RowCount) then
  begin
   Accept:=True;
   if (RowTo<>aRow) then
   begin
    ListGrid.ExchangeColRow(False,aRow,RowTo);
    ListGrid.Row:=RowTo;
   end;
  end else
  begin
   Accept:=False;
  end;
 end;
end;

procedure TfrmMain.OnIdleUpdate(Sender:TObject;var Done:Boolean);
var
 FProcess:TGameProcess;
begin
 Done:=True;

 if (GetTickCount64-FLogUpdateTime)>100 then
 begin
  if (FList<>nil) then
  begin
   FList.Update;
  end;
  FLogUpdateTime:=GetTickCount64;
 end;

 if (FContext.FGameProcess<>nil) then
 begin
  FProcess:=FContext.FGameProcess;
  FProcess.Acquire;

  if (FProcess.g_ipc<>nil) then
  begin
   FProcess.g_ipc.Update();
  end;

  if (FProcess.is_terminated) or
     (FProcess.is_stoped) then
  begin
   if (FContext.FGameProcess=FProcess) then
   begin
    TBStopClick(Sender);
   end else
   begin
    //
   end;
  end;

  FProcess.Release;
 end;

end;

procedure TfrmMain.MIAddClick(Sender: TObject);
var
 form:TfrmGameEditor;
begin
 form:=TfrmGameEditor.Create(Self);

 form.FConfigInfo:=FConfigInfo;
 form.FItem      :=TGameItem.Create;

 form.FItem.FMountList.firmware:=FConfigInfo.MainInfo.DefaultFirmware;

 form.OnSave:=@Self.DoAdd;

 form.FormInit(False);
end;

procedure TfrmMain.MIAddFolderClick(Sender: TObject);
var
 d:TSelectDirectoryDialog;
 form:TfrmGameEditor;

 Cookie:Pointer;
begin
 Cookie:=RegisterDllHack;

 d:=TSelectDirectoryDialog.Create(Self);

 //d.InitialDir:=

 d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];

 if d.Execute then
 begin
  form:=TfrmGameEditor.Create(Self);

  form.FConfigInfo:=FConfigInfo;
  form.FItem      :=TGameItem.Create;

  form.FItem.FMountList.firmware:=FConfigInfo.MainInfo.DefaultFirmware;

  form.FItem.FMountList.game:=d.FileName;

  form.OnSave:=@Self.DoAdd;

  form.FormInit(True);
 end;

 FreeAndNil(d);

 UnregisterDllHack(Cookie);
end;

procedure TfrmMain.MIEditClick(Sender: TObject);
var
 form:TfrmGameEditor;
 Item:TGameItem;
 aRow:Integer;
begin
 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=FGameList.GetItemRow(aRow);

 if Item.FLock then Exit;

 form:=TfrmGameEditor.Create(Self);

 form.FConfigInfo:=FConfigInfo;
 form.FItem:=Item;

 Item.FLock:=True;

 form.OnSave:=@Self.DoEdit;

 form.FormInit(True);
end;

procedure TfrmMain.TBConfigClick(Sender: TObject);
begin
 if (frmCfgEditor=nil) then
 begin
  frmCfgEditor:=TfrmCfgEditor.Create(Self);
  frmCfgEditor.OnSave:=@DoConfigSave;
  frmCfgEditor.FConfigInfo:=FConfigInfo;
 end;

 frmCfgEditor.FormInit;
end;

procedure TfrmMain.DoConfigSave(Sender: TObject);
var
 m:TMemoryStream;
 jstream:TJSONStreamWriter;
begin
 m:=TMemoryStream.Create;
 jstream:=TJSONStreamWriter.Create(m);

 FConfigInfo.WriteJSON('',jstream);
 FreeAndNil(jstream);

 try
  M.SaveToFile(fpps4File);
 except
  on E: Exception do
    MessageDlgEx(E.Message,'Error',[mbOK],Self);
 end;

 FreeAndNil(M);
end;

procedure TfrmMain.LogEnd;
begin
 Fmlog.TopLine:=Fmlog.Lines.Count;
end;

procedure TfrmMain.ClearLog;
begin
 //reset file
 FileTruncate(FAddHandle,0);
 FList.Reset(True);
 //
end;

procedure TfrmMain.MIShowExplorerClick(Sender: TObject);
var
 Item:TGameItem;
 aRow:Integer;
 S:RawByteString;
begin
 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=FGameList.GetItemRow(aRow);

 S:=ExtractRelativePath('/app0/',Item.GameInfo.Exec);

 if Length(S)<Length(Item.GameInfo.Exec) then
 begin
  S:=IncludeTrailingPathDelimiter(Item.MountList.game)+ExtractFilePath(S);
 end else
 begin
  S:=Item.MountList.game;
 end;

 OpenDocument(S);
end;

procedure TfrmMain.MIRunClick(Sender: TObject);
label
 _exit;
var
 Item:TGameItem;
 ParamSfo:TParamSfoFile;
 aRow:Integer;
 cfg:TGameRunConfig;
 a:Integer;
 r:Integer;
begin
 if (FContext.FGameProcess<>nil) then Exit;

 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=FGameList.GetItemRow(aRow);

 ParamSfo:=LoadParamSfoFile2(Item.MountList.game);

 LogEnd;
 ClearLog;

 if FConfigInfo.BootParamInfo.neo then
 if (ParamSfo<>nil) then
 begin
  a:=ParamSfo.GetUInt('ATTRIBUTE');

  if (a and $800000)=0 then
  begin

   if (MessageDlg('Question',
                  'Looks like "'+Item.FGameInfo.Name+'" doesn`t support PS4 Pro, Continue?',
                  mtConfirmation,
                  [mbYes, mbNo],
                  0)=mrNo) then
   begin
    goto _exit;
   end;

  end;

 end;

 Pages.ActivePage:=TabLog;

 cfg:=Default(TGameRunConfig);

 cfg.hOutput:=FAddHandle;
 cfg.hError :=FAddHandle;

 cfg.FConfInfo:=FConfigInfo;
 cfg.FGameItem:=Item;
 cfg.FParamSfo:=ParamSfo;

 if Item.FLock then Exit;

 r:=run_item(cfg,FContext);
 if (r<>0) then
 begin
  ShowMessage('error run process code=0x'+HexStr(r,8));
 end;

 if (r=0) then
 begin
  FContext.BindHandler(IpcHandler);

  ParamSfo:=nil; //do not free

  SetButtonsState(mdsStarted);
 end;

 _exit:
  FreeAndNil(ParamSfo);
end;

procedure TfrmMain.TBPlayClick(Sender: TObject);
begin
 if (FContext.FGameProcess<>nil) then
 begin
  if (not FContext.FGameProcess.g_fork) and
     (FContext.FGameProcess.is_stoped) then
  begin
   ShowMessage('Restart the emulator manually!');
  end else
  begin
   //resume
   FDialogsManager.ShowMainWindow();
   FContext.FGameProcess.resume;
   SetButtonsState(mdsRunned);
  end;
 end else
 begin
  //run
  MIRunClick(Sender);
 end;
end;

procedure TfrmMain.TBPauseClick(Sender: TObject);
begin
 if (FContext.FGameProcess<>nil) then
 begin
  //suspend
  FContext.FGameProcess.suspend;
  SetButtonsState(mdsSuspended);
 end;
end;

function TfrmMain.GameProcessForked:Boolean;
begin
 Result:=False;
 if (FContext.FGameProcess<>nil) then
 begin
  Result:=FContext.FGameProcess.g_fork;
 end;
end;

procedure TfrmMain.TBStopClick(Sender: TObject);
var
 exit_code:DWORD;
 r:RawByteString;
begin
 if (FContext.FGameProcess=nil) then Exit;

 if GameProcessForked then //only forked
 begin
  exit_code:=0;

  if FContext.FGameProcess.is_terminated then
  begin
   exit_code:=FContext.FGameProcess.exit_code;
  end;

  //terminate
  FContext.StopAndNil();
  //
  FreeAndNil(FContext.FParamSfo);
  //
  FContext.CloseItem();
  //
  FDialogsManager.CloseMainWindow;
  //
  SetButtonsState(mbsStopped);
  Pages.ActivePage:=TabList;

  if (exit_code<>0) then
  begin
   r:='Game process stopped with exit code:0x'+HexStr(exit_code,8);
   FileWrite(FAddHandle,PChar(r)^,Length(r));

   MessageDlgEx(r,'Error',[mbOK],Self);
  end;

 end else
 begin
  TBPauseClick(Sender);
 end;
end;

procedure TfrmMain.TBDownClick(Sender: TObject);
var
 aRow:Integer;
begin
 aRow:=ListGrid.Row;

 if (aRow<=0) then Exit;
 if ((aRow+1)>=ListGrid.RowCount) then Exit;

 ListGrid.ExchangeColRow(False,aRow,aRow+1);

 if (aRow+2)>(ListGrid.TopRow + ListGrid.VisibleRowCount) then
 begin
  ListGrid.TopRow:=ListGrid.TopRow+1;
 end;

 SaveGameList;
end;

procedure TfrmMain.TBUpClick(Sender: TObject);
var
 aRow:Integer;
begin
 aRow:=ListGrid.Row;

 if (aRow<=1) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 ListGrid.ExchangeColRow(False,aRow,aRow-1);

 if (aRow-1)<(ListGrid.TopRow) then
 begin
  ListGrid.TopRow:=ListGrid.TopRow-1;
 end;

 SaveGameList;
end;

procedure TfrmMain.MIDelClick(Sender: TObject);
var
 Item:TGameItem;
 aRow:Integer;
begin
 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=FGameList.GetItemRow(aRow);

 if (Item.FLock) then Exit;

 if (MessageDlg('Question',
                'Remove item "'+Item.FGameInfo.Name+'" from list?',
                mtConfirmation,
                [mbYes, mbNo],
                0)=mrYes) then
 begin
  FGameList.DelItem(Item);
  //
  SaveGameList;
 end;
end;

procedure TfrmMain.DoAdd(Sender: TObject);
var
 form:TfrmGameEditor;
 Item:TGameItem;
begin
 form:=TfrmGameEditor(Sender);

 Item:=form.FItem;

 form.FItem:=nil;

 FGameList.InsertItem(Item);
 //
 SaveGameList;
end;

procedure TfrmMain.DoEdit(Sender: TObject);
var
 form:TfrmGameEditor;
 Item:TGameItem;
begin
 form:=TfrmGameEditor(Sender);

 Item:=form.FItem;

 Item.FLock:=False;

 form.FItem:=nil;

 FGameList.UpdateItem(Item);
 //
 SaveGameList;
end;

procedure TfrmMain.SetButtonsState(s:TMainButtonsState);
begin
 FMainButtonsState:=s;

 case s of
  mbsStopped:
    begin
     TBPlay .Enabled:=True;
     TBPause.Enabled:=False;
     TBStop .Enabled:=False;
     //
     TBPlay .ImageIndex:=0;
     TBPause.ImageIndex:=1+3;
     TBStop .ImageIndex:=2+3;
    end;
  mdsStarted:
    begin
     TBPlay .Enabled:=False;
     TBPause.Enabled:=False;
     TBStop .Enabled:=False;
     //
     TBPlay .ImageIndex:=0+3;
     TBPause.ImageIndex:=1+3;
     TBStop .ImageIndex:=2+3;
    end;
  mdsRunned:
    begin
     TBPlay .Enabled:=False;
     TBPause.Enabled:=True;
     TBStop .Enabled:=False;
     //
     TBPlay .ImageIndex:=0+3;
     TBPause.ImageIndex:=1;
     TBStop .ImageIndex:=2+3;

     if GameProcessForked then //only forked
     begin
      TBStop .Enabled:=True;

      TBStop .ImageIndex:=2;
     end;
    end;
  mdsSuspended:
    begin
     TBPlay .Enabled:=True;
     TBPause.Enabled:=False;
     TBStop .Enabled:=False;
     //
     TBPlay .ImageIndex:=0;
     TBPause.ImageIndex:=1+3;
     TBStop .ImageIndex:=2+3;

     if GameProcessForked then //only forked
     begin
      TBStop .Enabled:=True;

      TBStop .ImageIndex:=2;
     end;
    end;
 end;

end;

end.


