unit game_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Grids, Buttons,

  LCLIntf,

  game_info,
  form_filler,
  param_sfo_gui;

type

  { TfrmGameEditor }

  TfrmGameEditor = class(TForm)
    BtnAddLayer: TSpeedButton;
    BtnExpGame: TSpeedButton;
    BtnExpFw: TSpeedButton;
    BtnGameOpen: TButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnRemLayer: TSpeedButton;
    BtnDwLayer: TSpeedButton;
    BtnUpLayer: TSpeedButton;
    EditPages: TPageControl;
    Edt_MountList_OverlayAuto: TCheckBox;
    Edt_GameInfo_Name: TEdit;
    Edt_GameInfo_Exec: TEdit;
    Edt_GameInfo_TitleId: TEdit;
    Edt_GameInfo_Version: TEdit;
    Edt_GameInfo_AppVer: TEdit;
    Edt_MountList_OverlayList: TListBox;
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
    Label8: TLabel;
    PanelHalf: TPanel;
    TabMain: TTabSheet;
    TabFolders: TTabSheet;
    TabParamSfo: TTabSheet;
    procedure BtnAddLayerClick(Sender: TObject);
    procedure BtnExpFwClick(Sender: TObject);
    procedure BtnExpGameClick(Sender: TObject);
    procedure BtnGameOpenClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnDwLayerClick(Sender: TObject);
    procedure BtnRemLayerClick(Sender: TObject);
    procedure BtnUpLayerClick(Sender: TObject);
    procedure Edt_MountList_OverlayAutoChange(Sender: TObject);
    procedure Edt_MountList_firmwareGetItems(Sender: TObject);
    procedure Edt_MountList_gameExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormInit(UpdateTitle:Boolean);
    procedure FormSave;
    procedure LoadParamSfo(UpdateTitle:Boolean);
    Procedure UpdateOverlays;
  private
    FOverlaysNotChanged:Boolean;
    Fgame:RawByteString;
    procedure DoMoveLayer(Dir:Integer);
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
 TypInfo,
 open_dialog;

{ TfrmGameEditor }

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
  procedure SetBool(control:TComponent;B:Boolean);                override;
  function  GetBool(control:TComponent):Boolean;                  override;
  procedure SetClass(control:TComponent;Obj:TObject);             override;
  procedure GetClass(control:TComponent;Obj:TObject);             override;
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

procedure TGameFormData.SetBool(control:TComponent;B:Boolean);
begin
 if control.InheritsFrom(TButtonControl) then
 begin
  TMyButtonControl(control).Checked:=B;
 end;
end;

function TGameFormData.GetBool(control:TComponent):Boolean;
begin
 Result:=False;
 if control.InheritsFrom(TButtonControl) then
 begin
  Result:=TMyButtonControl(control).Checked;
 end;
end;

procedure TGameFormData.SetClass(control:TComponent;Obj:TObject);
var
 A:TSerializeStringArray;
begin
 if control.InheritsFrom(TListBox) then
 begin
  A:=TSerializeStringArray(Obj);

  SerializeStringArray2Strings(A,TListBox(control).Items);
 end;
end;

procedure TGameFormData.GetClass(control:TComponent;Obj:TObject);
var
 A:TSerializeStringArray;
begin
 if control.InheritsFrom(TListBox) then
 begin
  A:=TSerializeStringArray(Obj);

  Strings2SerializeStringArray(TListBox(control).Items,A);
 end;
end;

//

procedure TfrmGameEditor.FormInit(UpdateTitle:Boolean);
var
 Provider:TGameFormData;
begin
 EditPages.ActivePageIndex:=0;

 Provider:=TGameFormData.Create;

 FormLoad(Self,Provider,FItem);

 Provider.Free;

 //////

 Edt_MountList_OverlayAutoChange(Self);
 LoadParamSfo(UpdateTitle);
 //reupdate
 FOverlaysNotChanged:=False;
 UpdateOverlays;
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
 if FOverlaysNotChanged and SameFileName(Fgame,V) then Exit;

 FreeAndNil(FParamSfo);

 FParamSfo:=LoadParamSfoByOverlays(V,Edt_MountList_OverlayList.Items);

 //update cache state
 Fgame:=V;
 FOverlaysNotChanged:=True;

 GridParamSfo.Clear;

 if (FParamSfo=nil) then
 begin
  Edt_GameInfo_TitleId.Text:='NPXS00000';
  Edt_GameInfo_Version.Text:='0.0';
  Edt_GameInfo_AppVer .Text:='0.0';
  Exit;
 end;

 if (Length(FParamSfo.params)=0) then Exit;
 For i:=0 to High(FParamSfo.params) do
 begin
  case FParamSfo.params[i].format of
   SFO_FORMAT_BLOB:
     case FParamSfo.params[i].GetLength of
      4:V:='0x'+HexStr(FParamSfo.params[i].GetUInt,8);
      8:V:='0x'+HexStr(FParamSfo.params[i].GetUInt64,16);
      else
        V:=FParamSfo.params[i].GetString;
     end;
   SFO_FORMAT_STRING:V:=Trim(FParamSfo.params[i].GetString);
   SFO_FORMAT_UINT32:V:='0x'+HexStr(FParamSfo.params[i].GetUInt,8);
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
 UpdateOverlays;
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
 //reupdate
 FOverlaysNotChanged:=False;
 UpdateOverlays;
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

procedure TfrmGameEditor.BtnAddLayerClick(Sender: TObject);
var
 new:RawByteString;
begin
 new:=DoOpenDir('','');
 if (new='') then Exit;

 Edt_MountList_OverlayList.Items.Add(new);

 FOverlaysNotChanged:=False;
 LoadParamSfo(True);
end;

procedure TfrmGameEditor.DoMoveLayer(Dir:Integer);
var
 c,n:Integer;
begin
 c:=Edt_MountList_OverlayList.ItemIndex;
 if (c<0) then Exit;

 n:=c+Dir;
 if (n<0) or (n>=Edt_MountList_OverlayList.Count) then Exit;

 Edt_MountList_OverlayList.Items.Move(c,n);
 Edt_MountList_OverlayList.ItemIndex:=n;

 FOverlaysNotChanged:=False;
 LoadParamSfo(True);
end;

procedure TfrmGameEditor.BtnDwLayerClick(Sender: TObject);
begin
 DoMoveLayer(1);
end;

procedure TfrmGameEditor.BtnUpLayerClick(Sender: TObject);
begin
 DoMoveLayer(-1);
end;

procedure TfrmGameEditor.BtnRemLayerClick(Sender: TObject);
var
 i:Integer;
begin
 i:=Edt_MountList_OverlayList.ItemIndex;
 if (i>=0) and (i<Edt_MountList_OverlayList.Count) then
 begin
  Edt_MountList_OverlayList.Items.Delete(i);

  FOverlaysNotChanged:=False;
  LoadParamSfo(True);
 end;
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

Procedure TfrmGameEditor.UpdateOverlays;
begin
 if FOverlaysNotChanged and SameFileName(Fgame,Edt_MountList_game.Text) then Exit;

 if Edt_MountList_OverlayAuto.Checked then
 begin
  //read auto list

  Edt_MountList_OverlayList.Clear;

  AutoDetectOverlays(Edt_MountList_game.Text,Edt_GameInfo_TitleId.Text,Edt_MountList_OverlayList.Items);

  FOverlaysNotChanged:=False;
 end else
 begin
  //
 end;
end;

procedure TfrmGameEditor.Edt_MountList_OverlayAutoChange(Sender: TObject);
var
 Checked:Boolean;
begin
 Checked:=Edt_MountList_OverlayAuto.Checked;

 BtnAddLayer.Enabled:=not Checked;
 BtnRemLayer.Enabled:=not Checked;
 Edt_MountList_OverlayList.Enabled:=not Checked;

 FOverlaysNotChanged:=False;
 UpdateOverlays;
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

