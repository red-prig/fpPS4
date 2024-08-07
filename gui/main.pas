unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids, Menus,

  LMessages,
  LCLType,
  LCLIntf,

  g_bufstream,
  LineStream,
  synlog,
  SynEditLineStream,
  LazSynEditText,
  SynEditMarkupBracket,

  TypInfo,
  Rtti,
  jsonscanner,

  host_ipc,
  game_info,
  game_edit,
  cfg_edit,
  game_run,

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
    procedure ListGridDblClick(Sender: TObject);
    procedure ListGridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListGridEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

    FConfigInfo:TConfigInfo;

    FAddHandle:THandle;
    FGetHandle:THandle;

    FFile:TStream;
    FList:TSynEditLineStream;

    Fmlog:TCustomSynLog;

    FLogUpdateTime:QWORD;

    FMainButtonsState:TMainButtonsState;

    FGameMainForm:TGameMainForm;

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

type
 TGuiIpcHandler=class(THostIpcHandler)
  Form:TfrmMain;
  function OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint; override;
  function OnKevent(kev:p_kevent;count:Integer):Ptruint;
 end;

function MessageDlgEx(const AMsg:RawByteString;
                      ADlgType:TMsgDlgType;
                      AButtons:TMsgDlgButtons;
                      AParent:TForm):TModalResult;
var
 MsgFrm:TForm;
begin
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
end;

function TGuiIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
begin
 Result:=0;
 case mtype of
  iKEV_EVENT   :Result:=OnKevent(buf,mlen div sizeof(t_kevent));
  iMAIN_WINDOWS:Result:=Form.OpenMainWindows();
  iCAPTION_FPS :Form.SetCaptionFPS(PQWORD(buf)^);
  iERROR       :MessageDlgEx(PChar(buf),mtError,[mbOK],Form);
  else;
   ShowMessage(GetEnumName(TypeInfo(mtype),ord(mtype)));
 end;
end;

function TGuiIpcHandler.OnKevent(kev:p_kevent;count:Integer):Ptruint;
var
 i:Integer;
begin
 Result:=0;

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
       Form.SetButtonsState(mdsRunned);
      end;
     end;

   else;
  end;

  Inc(i);
 end;

end;


var
 IpcHandler:TGuiIpcHandler;

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
 FGrid.Cells[2,i]:=Item.FGameInfo.Version;
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
 FGrid.Cells[2,i]:=Item.FGameInfo.Version;
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
 FGrid.Cells[2,i]:=Item.FGameInfo.Version;
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
 FGrid.Cells[2,i]:=Item.FGameInfo.Version;
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
     MessageDlgEx(E.Message,mtError,[mbOK],Self);
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
     MessageDlgEx(E.Message,mtError,[mbOK],Self);
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
    MessageDlgEx(E.Message,mtError,[mbOK],Self);
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
 IpcHandler:=TGuiIpcHandler.Create;
 IpcHandler.Form:=Self;

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
 Fmlog.Font.Name:='Consolas';
 Fmlog.Font.Size:=11;

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
   FGameProcess.g_ipc.Update(IpcHandler);
  end;

  if (FGameProcess.g_ipc<>nil) then //recheck, must be free in Update()
  if FGameProcess.is_terminated then
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
 APP_VER :=FGameItem.FGameInfo.Version;

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

 form.Item:=TGameItem.Create;

 form.Item.FMountList.system:=FConfigInfo.MainInfo.system;
 form.Item.FMountList.data  :=FConfigInfo.MainInfo.data;

 form.OnSave:=@Self.DoAdd;

 form.FormInit(False);
end;

procedure TfrmMain.MIAddFolderClick(Sender: TObject);
var
 d:TSelectDirectoryDialog;
 form:TfrmGameEditor;
begin
 d:=TSelectDirectoryDialog.Create(nil);

 //d.InitialDir:=

 d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];

 if d.Execute then
 begin
  form:=TfrmGameEditor.Create(Self);

  form.Item:=TGameItem.Create;

  form.Item.FMountList.system:=FConfigInfo.MainInfo.system;
  form.Item.FMountList.data  :=FConfigInfo.MainInfo.data;

  form.Item.FMountList.app0:=d.FileName;

  form.OnSave:=@Self.DoAdd;

  form.FormInit(True);
 end;

 d.Free;
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

 form.Item:=Item;

 Item.FLock:=True;

 form.OnSave:=@Self.DoEdit;

 form.FormInit(False);
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
    MessageDlgEx(E.Message,mtError,[mbOK],Self);
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
  S:=IncludeTrailingPathDelimiter(Item.MountList.app0)+ExtractFilePath(S);
 end else
 begin
  S:=Item.MountList.app0;
 end;

 OpenDocument(S);
end;

procedure TfrmMain.MIRunClick(Sender: TObject);
var
 Item:TGameItem;
 aRow:Integer;
 cfg:TGameRunConfig;
begin
 if (FGameProcess<>nil) then Exit;

 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=FGameList.GetItemRow(aRow);

 LogEnd;
 ClearLog;

 Pages.ActivePage:=TabLog;

 cfg.hOutput:=FAddHandle;
 cfg.hError :=FAddHandle;

 cfg.FConfInfo:=FConfigInfo;
 cfg.FGameItem:=Item;

 if Item.FLock then Exit;

 FGameProcess:=run_item(cfg);

 if (FGameProcess<>nil) then
 begin
  Item.FLock:=True;
  FGameItem:=Item;

  SetButtonsState(mdsStarted);
 end;
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
  //
  CloseMainWindows;
  //
  Pages.ActivePage:=TabList;

  if (exit_code<>0) then
  begin
   r:='Game process stopped with exit code:0x'+HexStr(exit_code,8);
   FileWrite(FAddHandle,PChar(r)^,Length(r));

   MessageDlgEx(r,mtError,[mbOK],Self);
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

 Item:=form.Item;

 form.Item:=nil;

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

 Item:=form.Item;

 Item.FLock:=False;

 form.Item:=nil;

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


