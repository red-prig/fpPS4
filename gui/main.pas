unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids, Menus,

  game_info,
  game_edit,

  host_ipc;

type

  { TfrmMain }

  TfrmMain = class(TForm)
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

    procedure FormCreate(Sender: TObject);
    procedure OnIdleUpdate(Sender:TObject;var Done:Boolean);
    procedure MIAddClick(Sender: TObject);
    procedure MIAddFolderClick(Sender: TObject);
    procedure MIDelClick(Sender: TObject);
    procedure MIEditClick(Sender: TObject);
    procedure MIRunClick(Sender: TObject);
  private

  public
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
  end;

var
  frmMain: TfrmMain;

implementation

uses
 windows,

 md_arc4random,

 g_bufstream,
 LineStream,
 synlog,
 SynEditLineStream,
 LazSynEditText,
 SynEditMarkupBracket,

 IniFiles,

 TypInfo,
 Rtti,

 evbuffer,
 evpoll,

 game_run;

//

var
 FIniFile:TIniFile;

 FMainInfo:TMainInfo;

 FAddHandle:THandle;
 FGetHandle:THandle;

 FFile:TStream;
 FList:TSynEditLineStream;

{$R *.lfm}

{ TfrmMain }

type
 TMySynLog=class(TCustomSynLog)
  function LinesCreate:TSynEditStringListBase; override;
 end;

function TMySynLog.LinesCreate:TSynEditStringListBase;
begin
 FList:=TSynEditLineStream.Create;

 FList.FSynLog:=Self;
 FList.FStream:=TLineStream.Create(FFile);

 Result:=FList;
end;

var
 mlog:TMySynLog;

const
 section_prefix='game-';

type
 TGuiIpcHandler=class(THostIpcHandler)
  Form:TfrmMain;
  Procedure OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer); override;
 end;

Procedure TGuiIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 ShowMessage(GetEnumName(TypeInfo(mtype),ord(mtype)));
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

 mlog:=TMySynLog.Create(TabLog);
 mlog.Parent:=TabLog;

 mlog.Align:=alClient;

 mlog.BracketHighlightStyle:=sbhsBoth;
 mlog.Font.Style:=[];

 Pages.ActivePageIndex:=0;

 Application.AddOnIdleHandler(@OnIdleUpdate,False);
end;

Var
 FLogUpdateTime:QWORD=0;

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

 if (mgui_ipc<>nil) then
 begin
  mgui_ipc.Update(IpcHandler);
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

procedure TfrmMain.MIRunClick(Sender: TObject);
var
 Item:TGameItem;
 aRow:Integer;
begin
 aRow:=ListGrid.Row;

 if (aRow=0) then Exit;
 if (aRow>ListGrid.RowCount) then Exit;

 Item:=GetItemRow(aRow);

 t_wr_handle:=FAddHandle;

 FList.FSynLog.TopLine:=FList.FSynLog.Lines.Count;

 //reset file
 FileTruncate(FAddHandle,0);
 FList.Reset;
 //

 Pages.ActivePage:=TabLog;

 run_item(Item);
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


end.


