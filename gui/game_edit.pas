unit game_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Grids, Buttons,

  LCLIntf,

  ms_shell_hack,

  game_info,
  form_filler,
  param_sfo_gui;

type

  { TfrmGameEditor }

  TfrmGameEditor = class(TForm)
    BtnExpGame: TSpeedButton;
    BtnExpFw: TSpeedButton;
    BtnGameOpen: TButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    EditPages: TPageControl;
    Edt_GameInfo_Name: TEdit;
    Edt_GameInfo_Exec: TEdit;
    Edt_GameInfo_TitleId: TEdit;
    Edt_GameInfo_Version: TEdit;
    Edt_GameInfo_AppVer: TEdit;
    Edt_MountList_game: TEdit;
    Edt_MountList_firmware: TComboBox;
    GridParamSfo: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PanelHalf: TPanel;
    TabMain: TTabSheet;
    TabFolders: TTabSheet;
    TabParamSfo: TTabSheet;
    procedure BtnExpFwClick(Sender: TObject);
    procedure BtnExpGameClick(Sender: TObject);
    procedure BtnGameOpenClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure Edt_MountList_firmwareGetItems(Sender: TObject);
    procedure Edt_MountList_gameExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormInit(UpdateTitle:Boolean);
    procedure FormSave;
    procedure LoadParamSfo(UpdateTitle:Boolean);
  private
    Fgame:RawByteString;
  public
    OnSave     :TNotifyEvent;
    FConfigInfo:TConfigInfo;
    FItem      :TGameItem;
    FParamSfo  :TParamSfoFile;
  end;

var
  frmGameEditor: TfrmGameEditor;

implementation

{$R *.lfm}

uses
 TypInfo;

{ TfrmGameEditor }

function DoOpenDir(const Input,InitialDir:RawByteString):RawByteString;
var
 d:TSelectDirectoryDialog;
 Cookie:Pointer;
begin
 Cookie:=RegisterDllHack;

 Result:=Input;
 d:=nil;
 try
  d:=TSelectDirectoryDialog.Create(nil);
  d.InitialDir:=InitialDir;
  d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];
  if d.Execute then
  begin
   Result:=d.FileName;
  end;
 except
  //
 end;
 FreeAndNil(d);

 UnregisterDllHack(Cookie);
end;

procedure AddRow(Grid:TStringGrid;const name,value:RawByteString;obj:TObject);
var
 i:Integer;
begin
 i:=Grid.RowCount;
 Grid.RowCount:=i+1;
 Grid.Cells[0,i]:=name;
 Grid.Cells[1,i]:=value;
 Grid.Objects[0,i]:=obj;
end;

type
 TGameFormData=class(TFormDataProvider)
  procedure SetText(control:TComponent;const Text:RawByteString); override;
  function  GetText(control:TComponent):RawByteString;            override;
 end;

procedure TGameFormData.SetText(control:TComponent;const Text:RawByteString);
begin
 if control.InheritsFrom(TControl) then
 begin
  TMyControl(control).Text:=Text;
 end;
end;

function TGameFormData.GetText(control:TComponent):RawByteString;
begin
 Result:='';
 if control.InheritsFrom(TControl) then
 begin
  Result:=TMyControl(control).Text;
 end;
end;

procedure TfrmGameEditor.FormInit(UpdateTitle:Boolean);
var
 Provider:TGameFormData;
begin
 EditPages.ActivePageIndex:=0;

 Provider:=TGameFormData.Create;

 FormLoad(Self,Provider,FItem);

 Provider.Free;

 //////

 LoadParamSfo(UpdateTitle);

 Show;
end;

procedure TfrmGameEditor.FormSave;
var
 Provider:TGameFormData;
begin
 Provider:=TGameFormData.Create;

 form_filler.FormSave(Self,Provider,FItem);

 Provider.Free;
end;

procedure TfrmGameEditor.LoadParamSfo(UpdateTitle:Boolean);
var
 i:Integer;
 V:RawByteString;
begin
 V:=Edt_MountList_game.Text;
 if (Fgame=V) then Exit;

 FreeAndNil(FParamSfo);

 FParamSfo:=LoadParamSfoFile(ExcludeTrailingPathDelimiter(V)+
                             DirectorySeparator+
                             'sce_sys'+
                             DirectorySeparator+
                             'param.sfo');

 Fgame:=V;

 GridParamSfo.Clear;

 if (FParamSfo=nil) then
 begin
  Edt_GameInfo_TitleId.Text:='???';
  Edt_GameInfo_Version.Text:='???';
  Exit;
 end;

 if (Length(FParamSfo.params)=0) then Exit;
 For i:=0 to High(FParamSfo.params) do
 begin
  case FParamSfo.params[i].format of
   SFO_FORMAT_STRING_SPECIAL:V:=IntToStr(FParamSfo.params[i].GetUInt);
   SFO_FORMAT_STRING        :V:=Trim(FParamSfo.params[i].GetString);
   SFO_FORMAT_UINT32        :V:='0x'+HexStr(FParamSfo.params[i].GetUInt,8);
   else
    V:='???';
  end;
  AddRow(GridParamSfo,FParamSfo.params[i].name,V,nil);
 end;

 GridParamSfo.AutoSizeColumn(0);

 //
 if not UpdateTitle then Exit;

 V:=FParamSfo.GetString('TITLE');
 Edt_GameInfo_Name.Text:=V;

 V:=FParamSfo.GetString('TITLE_ID');
 Edt_GameInfo_TitleId.Text:=V;

 V:=FParamSfo.GetString('VERSION');
 Edt_GameInfo_Version.Text:=V;

 V:=FParamSfo.GetString('APP_VER');
 Edt_GameInfo_AppVer.Text:=V;
end;

procedure TfrmGameEditor.Edt_MountList_gameExit(Sender: TObject);
begin
 LoadParamSfo(True);
end;

procedure TfrmGameEditor.BtnGameOpenClick(Sender: TObject);
var
 new:RawByteString;
begin
 new:=DoOpenDir('',Edt_MountList_game.Text);
 if (new='') then Exit;

 Edt_MountList_game.Text:=new;
 LoadParamSfo(True);
end;

procedure TfrmGameEditor.BtnExpGameClick(Sender: TObject);
begin
 OpenDocument(Edt_MountList_game.Text);
end;

procedure TfrmGameEditor.BtnExpFwClick(Sender: TObject);
begin
 OpenDocument(Edt_MountList_firmware.Text);
end;

procedure TfrmGameEditor.BtnOkClick(Sender: TObject);
begin
 FormSave;
 Hide;
 if Assigned(OnSave) then
 begin
  OnSave(Self);
 end;
 Close;
end;

procedure TfrmGameEditor.BtnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TfrmGameEditor.Edt_MountList_firmwareGetItems(Sender: TObject);
var
 i,c:Integer;
 S:RawByteString;
begin
 if (FConfigInfo<>nil) then
 begin
  c:=FConfigInfo.MainInfo.FirmwareList.GetArrayCount;
  if (c<>0) and (Edt_MountList_firmware.Items.Count<>c) then
  begin
   Edt_MountList_firmware.Items.Clear;
   //preload
   For i:=0 to c-1 do
   begin
    S:=FConfigInfo.MainInfo.FirmwareList.values[i];
    Edt_MountList_firmware.Items.Add(S);
   end;
  end;
 end;
end;

procedure TfrmGameEditor.FormClose(Sender:TObject;var CloseAction:TCloseAction);
begin
 if (FItem<>nil) then
 begin
  if FItem.FLock then
  begin
   FItem.FLock:=False;
  end else
  begin
   FreeAndNil(FItem);
  end;
 end;
 //
 FreeAndNil(FParamSfo);
 //
 CloseAction:=caFree;
end;

end.

