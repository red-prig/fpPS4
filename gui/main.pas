unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids, Menus,

  LMessages,
  LCLType,
  LCLIntf, StdCtrls,

  g_bufstream,
  LineStream,
  synlog,
  SynEditLineStream,
  LazSynEditText,
  SynEditMarkupBracket,

  TypInfo,
  Rtti,
  jsonscanner,

  ms_shell_hack,

  host_ipc,
  game_info,
  game_edit,
  cfg_edit,
  game_run,

  param_sfo_gui,
  playgo_chunk_gui,

  host_ipc_interface;

type
  TMainButtonsState=(mbsStopped,
                     mdsStarted,
                     mdsRunned,
                     mdsSuspended);

  TGameMainForm=class(TForm)
   public
    caption_format:RawByteString;
    procedure SetCaptionFPS(Ffps:QWORD);
    procedure WMEraseBkgnd(var Message:TLMEraseBkgnd); message LM_ERASEBKGND;
  end;

  TGameList=class(TAbstractArray)
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
   Function  AddObject:TAbstractObject;      override;
   Function  AddArray :TAbstractArray;       override;
   procedure AddValue(Value:TValue);         override;
  end;

  TGameListObject=class(TAbstractObject)
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
    FGameList   :TGameList;
    FGameProcess:TGameProcess;
    FGameItem   :TGameItem;
    FParamSfo   :TParamSfoFile;

    FConfigInfo:TConfigInfo;

    FAddHandle:THandle;
    FGetHandle:THandle;

    FFile:TStream;
    FList:TSynEditLineStream;

    Fmlog:TCustomSynLog;

    FLogUpdateTime:QWORD;

    FMainButtonsState:TMainButtonsState;

    FGameMainForm:TGameMainForm;

    function  OnKevent      (mlen:DWORD;buf:Pointer):Ptruint; //KEV_EVENT
    function  OnMainWindows (mlen:DWORD;buf:Pointer):Ptruint; //MAIN_WINDOWS
    function  OnCaptionFPS  (mlen:DWORD;buf:Pointer):Ptruint; //CAPTION_FPS
    function  OnError       (mlen:DWORD;buf:Pointer):Ptruint; //ERROR
    function  OnWarning     (mlen:DWORD;buf:Pointer):Ptruint; //WARNING
    function  OnParamSfoInit(mlen:DWORD;buf:Pointer):Ptruint; //PARAM_SFO_INIT
    function  OnPlaygoInit  (mlen:DWORD;buf:Pointer):Ptruint; //PLAYGO_INIT
    function  OnLoadExec    (obj:TObject)           :Ptruint; //LOAD_EXEC

    function  get_caption_format:RawByteString;
    function  OpenMainWindows():THandle;
    Procedure CloseMainWindows();
    Procedure ShowMainWindows();
    Procedure HideMainWindows();
    procedure SetCaptionFPS(Ffps:QWORD);

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

 windows,

 md_arc4random,

 vDevice,

 sys_event;

//

{$R *.lfm}

Const
 fpps4File   ='fpps4.json';
 GameListFile='GameList.json';

procedure TGameMainForm.SetCaptionFPS(Ffps:QWORD);
begin
 Caption:=Format(caption_format,[Ffps]);
end;

procedure TGameMainForm.WMEraseBkgnd(var Message:TLMEraseBkgnd);
begin
 Message.Result:=1;
end;

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

function GetRealFontSize(Font:TFont):Integer;
var
 fd: TFontData;
begin
 fd := Graphics.GetFontData(Font.Handle);
 Result := ((-fd.Height) * 72) div Font.PixelsPerInch;
end;

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

function TfrmMain.OnMainWindows(mlen:DWORD;buf:Pointer):Ptruint; //MAIN_WINDOWS
begin
 Result:=OpenMainWindows();
end;

function TfrmMain.OnCaptionFPS(mlen:DWORD;buf:Pointer):Ptruint; //CAPTION_FPS
begin
 Result:=0;
 SetCaptionFPS(PQWORD(buf)^);
end;

function TfrmMain.OnKevent(mlen:DWORD;buf:Pointer):Ptruint; //KEV_EVENT
var
 kev:p_kevent;
 count:Integer;

 i:Integer;
begin
 Result:=0;

 kev  :=buf;
 count:=mlen div sizeof(t_kevent);

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

function TfrmMain.OnError(mlen:DWORD;buf:Pointer):Ptruint; //ERROR
begin
 Result:=0;
 if (MessageDlgEx(PChar(buf),'Error',[mbOK,mbAbort],Self)=mrAbort) then
 begin
  if (FGameProcess<>nil) then
  if (FGameProcess.g_ipc<>nil) then
  begin
   FGameProcess.g_ipc.FStop:=True;
  end;
 end;
end;

function TfrmMain.OnWarning(mlen:DWORD;buf:Pointer):Ptruint; //WARNING
begin
 Result:=MessageDlgEx(PChar(buf),'Warning',[mbYes,mbNo,mbAbort],Self);
 if (Result=mrAbort) then
 begin
  if (FGameProcess<>nil) then
  if (FGameProcess.g_ipc<>nil) then
  begin
   FGameProcess.g_ipc.FStop:=True;
  end;
 end;
 if (Result=mrYes) then
 begin
  Result:=0;
 end;
end;

function LoadParamSfoFile2(const game:RawByteString):TParamSfoFile;
begin
 Result:=LoadParamSfoFile(ExcludeTrailingPathDelimiter(game)+
                          DirectorySeparator+
                          'sce_sys'+
                          DirectorySeparator+
                          'param.sfo');
end;

function TfrmMain.OnParamSfoInit(mlen:DWORD;buf:Pointer):Ptruint; //PARAM_SFO_INIT
var
 V:RawByteString;
begin
 Result:=Ptruint(-1);

 if (FGameItem=nil) then Exit;

 if (FParamSfo=nil) then
 begin
  FParamSfo:=LoadParamSfoFile2(FGameItem.MountList.game);
 end;

 if (FParamSfo=nil) then
 begin
  V:='"{$GAME}/sce_sys/param.sfo" not found, continue?';

  if (MessageDlgEx(V,'Error',[mbOK,mbAbort],Self)=mrOK) then
  begin
   Exit(0);
  end else
  begin
   Exit(Ptruint(-1));
  end;
 end;

 if (FGameProcess<>nil) then
 if (FGameProcess.g_ipc<>nil) then
 begin
  FGameProcess.g_ipc.SendSync('PARAM_SFO_LOAD',FParamSfo);
 end;

 Result:=0;
end;

function TfrmMain.OnPlaygoInit(mlen:DWORD;buf:Pointer):Ptruint; //PLAYGO_INIT
var
 playgo_file:TPlaygoFile;
 V:RawByteString;
begin
 Result:=Ptruint(-1);

 if (FGameItem=nil) then Exit;

 V:=FGameItem.MountList.game;

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
   Exit(Ptruint(-1));
  end;
 end;

 if (FGameProcess<>nil) then
 if (FGameProcess.g_ipc<>nil) then
 begin
  FGameProcess.g_ipc.SendSync('PLAYGO_LOAD',playgo_file);
 end;

 FreeAndNil(playgo_file);
 Result:=0;
end;

function encode_shell(const src:RawByteString):RawByteString;
var
 i:Integer;
begin
 if (Pos(' ',src)=0) then
 begin
  Result:=src;
 end else
 if (Pos('"',src)=0) then
 begin
  Result:='"'+src+'"';
 end else
 if (Pos('''',src)=0) then
 begin
  Result:=''''+src+'''';
 end else
 begin
  Result:='"';
  For i:=1 to Length(src) do
  begin
   if (src[i]='"') then
   begin
    Result:=Result+'"'+'\"'+'"';
   end else
   begin
    Result:=Result+src[i];
   end;
  end;
  Result:=Result+'"';
 end;
end;

function encode_shell(argv:TStringArray):RawByteString;
var
 i:Integer;
begin
 Result:='';
 if Length(argv.values)<>0 then
 begin
  For i:=0 to High(argv.values) do
  begin
   if (Result<>'') then Result:=Result+' ';
   Result:=Result+encode_shell(argv.values[i]);
  end;
 end;
end;

function TfrmMain.OnLoadExec(obj:TObject):Ptruint; //LOAD_EXEC
var
 data:TPS4LoadExec;
 cfg:TGameRunConfig;
 Item:TGameItem;
begin
 Result:=Ptruint(-1);

 if (FGameItem=nil) then Exit;
 if (obj=nil) then Exit;

 data:=TPS4LoadExec(obj);

 if (UpperCase(data.Path)='EXIT') then
 begin
  FreeAndNil(data);
  TBStopClick(nil);
  Exit;
 end;

 if (FGameProcess=nil) then
 begin
  FreeAndNil(data);
  FreeAndNil(obj);
  Exit;
 end;

 if GameProcessForked then //only forked
 begin

  //terminate
  FGameProcess.stop;
  FreeAndNil(FGameProcess);
  //
  CloseMainWindows;
  //

  //re-run

  Item:=TGameItem.Create;
  FGameItem.CopyTo(Item);

  Item.GameInfo.Exec:=encode_shell(data.Path)+' '+encode_shell(data.argv);

  cfg:=Default(TGameRunConfig);

  cfg.hOutput:=FAddHandle;
  cfg.hError :=FAddHandle;

  cfg.FConfInfo:=FConfigInfo;
  cfg.FGameItem:=Item;
  cfg.FParamSfo:=FParamSfo;
  cfg.FLoadExec:=True;

  FGameProcess:=run_item(cfg);

  FreeAndNil(Item);

  if (FGameProcess=nil) then
  begin
   //stop on error
   TBStopClick(Self);
  end;

  if (FGameProcess.g_ipc<>nil) then
  begin
   FGameProcess.g_ipc.FHandler:=IpcHandler;
  end;

 end else
 begin
  MessageDlgEx('LoadExec is not supported for the current process','Error',[mbOK],Self);
 end;

 FreeAndNil(data);
 FreeAndNil(obj);
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

Function TGameList.AddObject:TAbstractObject;
begin
 Result:=TGameItem.Create;
 //
 AddItem(TGameItem(Result));
end;

Function TGameList.AddArray:TAbstractArray;
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
var
 FLogFileW:WideString;
begin
 FLogFileW:=UTF8Decode(LogFile);

 FAddHandle:=CreateFileW(PWideChar(FLogFileW),
                         GENERIC_READ or GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         OPEN_ALWAYS,
                         0,
                         0);

 FGetHandle:=CreateFileW(PWideChar(FLogFileW),
                         GENERIC_READ,
                         FILE_SHARE_READ or FILE_SHARE_WRITE,
                         nil,
                         OPEN_EXISTING,
                         0,
                         0);

 SetStdHandle(STD_OUTPUT_HANDLE,FAddHandle);
 SetStdHandle(STD_ERROR_HANDLE ,FAddHandle);

 FileSeek(FAddHandle,0,fsFromEnd);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
 r:RawByteString;
begin

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

 IpcHandler.AddCallback('KEV_EVENT'     ,@OnKevent      );
 IpcHandler.AddCallback('MAIN_WINDOWS'  ,@OnMainWindows );
 IpcHandler.AddCallback('CAPTION_FPS'   ,@OnCaptionFPS  );
 IpcHandler.AddCallback('ERROR'         ,@OnError       );
 IpcHandler.AddCallback('WARNING',       @OnWarning     );
 IpcHandler.AddCallback('PARAM_SFO_INIT',@OnParamSfoInit);
 IpcHandler.AddCallback('PLAYGO_INIT'   ,@OnPlaygoInit  );
 IpcHandler.AddCallback('LOAD_EXEC'     ,@OnLoadExec    ,TPS4LoadExec);

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

 if (FGameProcess<>nil) then
 begin

  if (FGameProcess.g_ipc<>nil) then
  begin
   FGameProcess.g_ipc.Update();
  end;

  if (FGameProcess<>nil) then       //recheck, must be free in Update()
  if (FGameProcess.g_ipc<>nil) then //recheck, must be free in Update()
  if (FGameProcess.is_terminated) or
     (FGameProcess.g_ipc.FStop) then
  begin
   TBStopClick(Sender);
  end;

 end;

end;

function TfrmMain.get_caption_format:RawByteString;
var
 TITLE,TITLE_ID,APP_VER:RawByteString;
begin
 Result:='';

 if (FGameItem=nil) then Exit;

 TITLE   :=FGameItem.FGameInfo.Name;
 TITLE_ID:=FGameItem.FGameInfo.TitleId;
 APP_VER :=FGameItem.FGameInfo.AppVer;

 if (TITLE='') then
 begin
  TITLE:=ExtractFileName(FGameItem.FGameInfo.Exec);
 end;

 if (TITLE_ID<>'') then TITLE_ID:='-' +TITLE_ID;
 if (APP_VER <>'') then APP_VER :=':v'+APP_VER;

 Result:=Format('fpPS4 (%s) [%s%s%s]',[{$I tag.inc},TITLE,TITLE_ID,APP_VER])+' FPS:%d';
end;

function TfrmMain.OpenMainWindows():THandle;
const
 pd_Width=1280;
 pd_Height=720;
begin
 if (FGameMainForm<>nil) then
 begin
  FGameMainForm.Show;
  Exit(FGameMainForm.Handle);
 end;

 FGameMainForm:=TGameMainForm.CreateNew(Self);
 FGameMainForm.ShowInTaskBar:=stAlways;
 FGameMainForm.DoubleBuffered:=False;
 FGameMainForm.ParentDoubleBuffered:=False;
 FGameMainForm.FormStyle:=fsNormal;
 FGameMainForm.SetBounds(100, 100, pd_Width, pd_Height);
 FGameMainForm.caption_format:=get_caption_format;
 FGameMainForm.SetCaptionFPS(0);
 //FGameMainForm.OnClose:=@FGameMainForm.CloseEvent;
 //FGameMainForm.OnKeyDown:=@FGameMainForm.KeyEvent;
 FGameMainForm.Position:=poScreenCenter;

 ///
 ///

 FGameMainForm.Show;

 Exit(FGameMainForm.Handle);
end;

Procedure TfrmMain.CloseMainWindows();
begin
 FreeAndNil(FGameMainForm);
end;

Procedure TfrmMain.ShowMainWindows();
begin
 if (FGameMainForm<>nil) then
 begin
  FGameMainForm.Show;
 end;
end;

Procedure TfrmMain.HideMainWindows();
begin
 if (FGameMainForm<>nil) then
 begin
  FGameMainForm.Hide;
 end;
end;

procedure TfrmMain.SetCaptionFPS(Ffps:QWORD);
begin
 if (FGameMainForm=nil) then Exit;

 FGameMainForm.SetCaptionFPS(Ffps);
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
var
 Item:TGameItem;
 ParamSfo:TParamSfoFile;
 aRow:Integer;
 cfg:TGameRunConfig;
begin
 if (FGameProcess<>nil) then Exit;

 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=FGameList.GetItemRow(aRow);

 ParamSfo:=LoadParamSfoFile2(Item.MountList.game);

 LogEnd;
 ClearLog;

 Pages.ActivePage:=TabLog;

 cfg:=Default(TGameRunConfig);

 cfg.hOutput:=FAddHandle;
 cfg.hError :=FAddHandle;

 cfg.FConfInfo:=FConfigInfo;
 cfg.FGameItem:=Item;
 cfg.FParamSfo:=ParamSfo;

 if Item.FLock then Exit;

 FGameProcess:=run_item(cfg);

 if (FGameProcess<>nil) then
 begin
  Item.FLock:=True;
  FGameItem:=Item;
  FParamSfo:=ParamSfo;
  ParamSfo:=nil;

  SetButtonsState(mdsStarted);

  if (FGameProcess.g_ipc<>nil) then
  begin
   FGameProcess.g_ipc.FHandler:=IpcHandler;
  end;
 end;

 FreeAndNil(ParamSfo);
end;

procedure TfrmMain.TBPlayClick(Sender: TObject);
begin
 if (FGameProcess<>nil) then
 begin
  //resume
  ShowMainWindows();
  FGameProcess.resume;
  SetButtonsState(mdsRunned);
 end else
 begin
  //run
  MIRunClick(Sender);
 end;
end;

procedure TfrmMain.TBPauseClick(Sender: TObject);
begin
 if (FGameProcess<>nil) then
 begin
  //suspend
  FGameProcess.suspend;
  SetButtonsState(mdsSuspended);
 end;
end;

function TfrmMain.GameProcessForked:Boolean;
begin
 Result:=False;
 if (FGameProcess<>nil) then
 begin
  Result:=FGameProcess.g_fork;
 end;
end;

procedure TfrmMain.TBStopClick(Sender: TObject);
var
 exit_code:DWORD;
 r:RawByteString;
begin
 if (FGameProcess=nil) then Exit;

 if GameProcessForked then //only forked
 begin
  exit_code:=0;

  if FGameProcess.is_terminated then
  begin
   exit_code:=FGameProcess.exit_code;
  end;

  //terminate
  FGameProcess.stop;
  SetButtonsState(mbsStopped);
  FreeAndNil(FGameProcess);
  //
  if (FGameItem<>nil) then
  begin
   FGameItem.FLock:=False;
   FGameItem:=nil;
  end;
  FreeAndNil(FParamSfo);
  //
  CloseMainWindows;
  //
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


