unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids, Menus,

  g_bufstream,
  LineStream,
  synlog,
  SynEditLineStream,
  LazSynEditText,
  SynEditMarkupBracket,

  IniFiles,

  game_info,
  game_edit,
  game_run,

  host_ipc;

type
  TMainButtonsState=(mbsStopped,
                     mdsStarted,
                     mdsRunned,
                     mdsSuspended);

  { TfrmMain }

  TfrmMain = class(TForm)
    MainImageList: TImageList;
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

    procedure FormCreate(Sender: TObject);
    procedure OnIdleUpdate(Sender:TObject;var Done:Boolean);
    procedure MIAddClick(Sender: TObject);
    procedure MIAddFolderClick(Sender: TObject);
    procedure MIDelClick(Sender: TObject);
    procedure MIEditClick(Sender: TObject);
    procedure MIRunClick(Sender: TObject);
    procedure TBPauseClick(Sender: TObject);
    procedure TBPlayClick(Sender: TObject);
    procedure TBStopClick(Sender: TObject);
  private

  public
    FGameProcess:TGameProcess;

    FIniFile:TIniFile;

    FMainInfo:TMainInfo;

    FAddHandle:THandle;
    FGetHandle:THandle;

    FFile:TStream;
    FList:TSynEditLineStream;

    Fmlog:TCustomSynLog;

    FLogUpdateTime:QWORD;

    FMainButtonsState:TMainButtonsState;

    procedure ReadIniFile;
    procedure LoadItemIni(Item:TGameItem);
    procedure SaveItemIni(Item:TGameItem);
    function  gen_section:RawByteString;
    function  GetItemRow(aRow:Integer):TGameItem;
    procedure AddItemRow(Item:TGameItem);
    procedure EditItemRow(Item:TGameItem);
    procedure DelItemRow(Item:TGameItem);
    procedure DoAdd(Sender: TObject);
    procedure DoEdit(Sender: TObject);
    procedure LogEnd;
    procedure ClearLog;
    procedure SetButtonsState(s:TMainButtonsState);
  end;

var
  frmMain: TfrmMain;

implementation

uses
 windows,

 md_arc4random,

 TypInfo,
 Rtti,

 sys_event;

//

{$R *.lfm}

{ TfrmMain }

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

function TGuiIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
begin
 Result:=0;
 case mtype of
  iKEV_EVENT:Result:=OnKevent(buf,mlen div sizeof(t_kevent));
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
       ShowMessage('NOTE_EXIT pid:'+IntToStr(kev[i].ident));
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

procedure TfrmMain.ReadIniFile;
var
 i,c:Integer;
 V:RawByteString;
 //
 List:TStringList;
 Item:TGameItem;
begin
 IpcHandler:=TGuiIpcHandler.Create;
 IpcHandler.Form:=Self;

 //main
 FMainInfo.ReadIni(FIniFile,'main');
 //main

 //games
 List:=TStringList.Create;
 FIniFile.ReadSections(List);

 c:=List.Count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   V:=Trim(List.Strings[i]);

   if (lowercase(copy(v,1,Length(section_prefix)))=section_prefix) then
   begin
    Item:=TGameItem.Create;
    Item.FSecton:=V;

    LoadItemIni(Item);
    //
    AddItemRow(Item);
   end;

  end;
 end;

 List.Free;
 //games
end;

procedure TfrmMain.LoadItemIni(Item:TGameItem);
begin
 Item.FGameInfo .ReadIni(FIniFile,Item.FSecton);
 Item.FMountList.ReadIni(FIniFile,Item.FSecton);
end;

procedure TfrmMain.SaveItemIni(Item:TGameItem);
begin
 if (Item.FSecton='') then
 begin
  Item.FSecton:=gen_section;
 end;

 Item.FGameInfo .WriteIni(FIniFile,Item.FSecton);
 Item.FMountList.WriteIni(FIniFile,Item.FSecton);
end;

function EncodeValue32(nVal:DWORD):RawByteString;
const
 nEncLenMax=5;
var
 i,nIdx:Integer;
begin
 SetLength(Result,nEncLenMax);
 For i:=nEncLenMax downto 1 do
 begin
  nIdx:=nVal and 63;
  nVal:=nVal shr 6;
  case nIdx of
    0..25:Result[i]:=Char(nIdx+Byte('A')-0);
   26..51:Result[i]:=Char(nIdx+Byte('a')-26);
   52..61:Result[i]:=Char(nIdx+Byte('0')-52);
       62:Result[i]:='+';
       63:Result[i]:='-';
  end;
 end;
end;

function TfrmMain.gen_section:RawByteString;
var
 nVal:DWORD;
 sVal:RawByteString;
begin
 nVal:=0;
 arc4rand(@nVal,SizeOf(nVal),0);

 sVal:=section_prefix+EncodeValue32(nVal);

 while FIniFile.SectionExists(sVal) do
 begin
  Inc(nVal);
  sVal:=section_prefix+EncodeValue32(nVal);
 end;

 Result:=sVal;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
 FLogFileW:WideString;
begin
 FMainInfo:=TMainInfo.Create;

 FIniFile:=TIniFile.Create('fpps4.ini');

 ReadIniFile;

 FLogFileW:=UTF8Decode(FMainInfo.LogFile);

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

 FileSeek(FAddHandle,0,fsFromEnd);

 FFile:=TBufferedFileStream.Create(FGetHandle);

 Fmlog:=TMySynLog.Create(TabLog,Self);
 Fmlog.Parent:=TabLog;

 Fmlog.Align:=alClient;

 Fmlog.BracketHighlightStyle:=sbhsBoth;
 Fmlog.Font.Style:=[];

 Pages.ActivePageIndex:=0;

 Application.AddOnIdleHandler(@OnIdleUpdate,False);

 SetButtonsState(mbsStopped);
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

  if FGameProcess.is_terminated then
  begin
   TBStopClick(Sender);
  end;

 end;

end;

procedure TfrmMain.MIAddClick(Sender: TObject);
var
 form:TfrmGameEditor;
begin
 form:=TfrmGameEditor.Create(Self);

 form.Item:=TGameItem.Create;

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

 Item:=GetItemRow(aRow);

 if Item.FLock then Exit;

 form:=TfrmGameEditor.Create(Self);

 form.Item:=Item;

 Item.FLock:=True;

 form.OnSave:=@Self.DoEdit;

 form.FormInit(False);
end;

procedure TfrmMain.LogEnd;
begin
 Fmlog.TopLine:=Fmlog.Lines.Count;
end;

procedure TfrmMain.ClearLog;
begin
 //reset file
 FileTruncate(FAddHandle,0);
 FList.Reset;
 //
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

 Item:=GetItemRow(aRow);

 LogEnd;
 ClearLog;

 Pages.ActivePage:=TabLog;

 cfg.hOutput:=FAddHandle;
 cfg.hError :=FAddHandle;

 cfg.fork_proc:=True;

 FGameProcess:=run_item(cfg,Item);

 SetButtonsState(mdsStarted);
end;

procedure TfrmMain.TBPlayClick(Sender: TObject);
begin
 if (FGameProcess<>nil) then
 begin
  //resume
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

procedure TfrmMain.TBStopClick(Sender: TObject);
begin
 if (FGameProcess<>nil) then
 if (FGameProcess.g_fork) then //only forked
 begin
  //terminate
  FGameProcess.stop;
  SetButtonsState(mbsStopped);
  FreeAndNil(FGameProcess);
  //
  Pages.ActivePage:=TabList;
 end;
end;

procedure TfrmMain.MIDelClick(Sender: TObject);
var
 Item:TGameItem;
 aRow:Integer;
begin
 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=GetItemRow(aRow);

 if (Item.FLock) then Exit;

 if (MessageDlg('Question',
                'Remove item "'+Item.FGameInfo.Name+'" from list?',
                mtConfirmation,
                [mbYes, mbNo],
                0)=mrYes) then
 begin
  FIniFile.EraseSection(Item.FSecton);
  //
  DelItemRow(Item);
 end;
end;

function TfrmMain.GetItemRow(aRow:Integer):TGameItem;
begin
 Result:=nil;
 //
 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;
 //
 Result:=TGameItem(ListGrid.Objects[0,aRow]);
end;

procedure TfrmMain.AddItemRow(Item:TGameItem);
var
 i:Integer;
begin
 i:=ListGrid.RowCount;
 ListGrid.RowCount:=i+1;
 ListGrid.Cells[0,i]:=Item.FGameInfo.Name;
 ListGrid.Cells[1,i]:=Item.FGameInfo.TitleId;
 ListGrid.Cells[2,i]:=Item.FGameInfo.Version;
 //
 ListGrid.Objects[0,i]:=Item;
end;

procedure TfrmMain.EditItemRow(Item:TGameItem);
var
 i:Integer;
begin
 i:=ListGrid.Cols[0].IndexOfObject(Item);
 if (i=-1) then Exit;
 //
 ListGrid.Cells[0,i]:=Item.FGameInfo.Name;
 ListGrid.Cells[1,i]:=Item.FGameInfo.TitleId;
 ListGrid.Cells[2,i]:=Item.FGameInfo.Version;
end;

procedure TfrmMain.DelItemRow(Item:TGameItem);
var
 i:Integer;
begin
 i:=ListGrid.Cols[0].IndexOfObject(Item);
 if (i=-1) then Exit;
 //
 ListGrid.DeleteRow(i);
 //
 Item.Free;
end;

procedure TfrmMain.DoAdd(Sender: TObject);
var
 form:TfrmGameEditor;
 Item:TGameItem;
begin
 form:=TfrmGameEditor(Sender);

 Item:=form.Item;

 form.Item:=nil;

 AddItemRow(Item);
 SaveItemIni(Item);
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

 EditItemRow(Item);
 SaveItemIni(Item);
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
     TBStop .Enabled:=True;
     //
     TBPlay .ImageIndex:=0+3;
     TBPause.ImageIndex:=1;
     TBStop .ImageIndex:=2;
    end;
  mdsSuspended:
    begin
     TBPlay .Enabled:=True;
     TBPause.Enabled:=False;
     TBStop .Enabled:=True;
     //
     TBPlay .ImageIndex:=0;
     TBPause.ImageIndex:=1+3;
     TBStop .ImageIndex:=2;
    end;
 end;

end;

end.

