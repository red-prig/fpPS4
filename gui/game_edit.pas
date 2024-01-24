unit game_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Grids,

  game_info,
  param_sfo_gui;

type

  { TfrmGameEditor }

  TfrmGameEditor = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    EditPages: TPageControl;
    GridMain: TStringGrid;
    GridMounts: TStringGrid;
    GridParamSfo: TStringGrid;
    TabMain: TTabSheet;
    TabMounts: TTabSheet;
    TabParamSfo: TTabSheet;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormInit(UpdateTitle:Boolean);
    procedure FormSave;
    procedure LoadParamSfo(UpdateTitle:Boolean);
    procedure GridSelectEditor(Sender:TObject;aCol,aRow:Integer;var Editor:TWinControl);
    procedure GridEditingDone(Sender: TObject);
  private
    Fapp0p:RawByteString;
  public
    OnSave  :TNotifyEvent;
    Item    :TGameItem;
    ParamSfo:TParamSfoFile;
  end;

var
  frmGameEditor: TfrmGameEditor;

implementation

{$R *.lfm}

uses
 TypInfo,
 Rtti;

{ TfrmGameEditor }

type
 TFieldInfo=class(TComponent)
  procedure SelectEditor(Grid:TStringGrid;aCol,aRow:Integer;var Editor:TWinControl); virtual; abstract;
 end;

 TFieldInfoPath=class(TFieldInfo)
  procedure SelectEditor(Grid:TStringGrid;aCol,aRow:Integer;var Editor:TWinControl); override;
 end;

 TButtonPath=class(TButton)
  public
   aRow:Integer;
   Form  :TWinControl;
   Editor:TWinControl;
   procedure EditorExit(Sender:TObject);
   procedure OpenDir(Sender:TObject);
 end;

procedure TButtonPath.EditorExit(Sender:TObject);
begin
 Editor.OnEnter:=nil;
 Editor.OnExit :=nil;
 if not Focused then
 begin
  Hide;
 end;
end;

type
 TMyStringGrid=class(TStringGrid)
  function  NewBtn:TButtonPath;
  procedure EditorDirEnter(Sender:TObject);
 end;

procedure TButtonPath.OpenDir(Sender:TObject);
var
 d:TSelectDirectoryDialog;
begin
 d:=TSelectDirectoryDialog.Create(nil);

 with TMyStringGrid(Editor.Parent) do
 begin
  d.InitialDir:=Cells[1,aRow];
 end;

 d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];

 if d.Execute then
 with TMyStringGrid(Editor.Parent) do
 begin
  Cells[1,aRow]:=d.FileName;
 end;

 d.Free;

 TfrmGameEditor(Self.Form).LoadParamSfo(True);

 Hide;
end;

function TMyStringGrid.NewBtn:TButtonPath;
var
 i:Integer;
 btn:TButtonPath;
begin
 btn:=nil;

 if (ComponentCount<>0) then
 for i:=0 to ComponentCount-1 do
 begin
  if Components[i] is TButtonPath then
  begin
   btn:=TButtonPath(Components[i]);
   Break;
  end;
 end;

 if (btn=nil) then
 begin
  btn:=TButtonPath.Create(Self);

  btn.Form:=TWinControl(Self.Tag);

  btn.AutoSize:=True;
  btn.Caption:='...';

  btn.Parent:=Self;

  btn.BorderSpacing.Top   :=-3;
  btn.BorderSpacing.Right :=-3;
  btn.BorderSpacing.Bottom:=-3;
 end;

 btn.aRow:=Self.Row;

 btn.AnchorSide[akTop].Side:=asrTop;
 btn.AnchorSide[akTop].Control:=Editor;

 btn.AnchorSide[akRight].Side:=asrBottom;
 btn.AnchorSide[akRight].Control:=Editor;

 btn.AnchorSide[akBottom].Side:=asrBottom;
 btn.AnchorSide[akBottom].Control:=Editor;

 btn.Anchors:=[akTop,akRight,akBottom];

 btn.Editor:=Editor;

 Result:=btn;
end;

procedure TMyStringGrid.EditorDirEnter(Sender:TObject);
var
 btn:TButtonPath;
begin
 btn:=NewBtn;

 btn.OnClick:=@btn.OpenDir;

 Editor.OnExit:=@btn.EditorExit;

 btn.Show;
end;

procedure TFieldInfoPath.SelectEditor(Grid:TStringGrid;aCol,aRow:Integer;var Editor:TWinControl);
begin
 Editor.OnEnter:=@TMyStringGrid(Grid).EditorDirEnter;
end;

procedure AddRow(Grid:TStringGrid;const name,value:RawByteString;info:TFieldInfo);
var
 i:Integer;
begin
 i:=Grid.RowCount;
 Grid.RowCount:=i+1;
 Grid.Cells[0,i]:=name;
 Grid.Cells[1,i]:=value;
 Grid.Objects[0,i]:=info;
end;

procedure TfrmGameEditor.FormInit(UpdateTitle:Boolean);
var
 fip:TFieldInfoPath;

 i,c:Integer;

 Ctx:TRTTIContext;
 RT :TRTTIType;
 A  :specialize TArray<TRttiProperty>;
begin
 EditPages.ActivePageIndex:=0;

 GridMain.Clear;
 GridMounts.Clear;

 //TypInfo.SetRawByteStrProp();

 //AddRow(GridMain,'Name:',nil);

 //
 fip:=TFieldInfoPath.Create(Self);
 //

 Ctx:=TRTTIContext.Create;
 try
  ///
  RT:=Ctx.GetType(Item.FGameInfo.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    AddRow(GridMain,A[i].Name+':',A[i].GetValue(Item.FGameInfo).AsString,nil);
   end;
  end;
  ///
  RT:=Ctx.GetType(Item.FMountList.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    AddRow(GridMounts,'/'+A[i].Name,A[i].GetValue(Item.FMountList).AsString,fip);
   end;
  end;
  ///
 finally
  Ctx.free;
 end;

 //AddRow(GridMounts,'/app0'  ,ip);
 //AddRow(GridMounts,'/system',ip);
 //AddRow(GridMounts,'/data'  ,ip);

 LoadParamSfo(UpdateTitle);

 Show;
end;

procedure TfrmGameEditor.FormSave;
var
 i,c:Integer;

 Ctx:TRTTIContext;
 RT :TRTTIType;
 A  :specialize TArray<TRttiProperty>;
begin

 Ctx:=TRTTIContext.Create;
 try
  ///
  RT:=Ctx.GetType(Item.FGameInfo.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    A[i].SetValue(Item.FGameInfo,GridMain.Cells[1,i]);
   end;
  end;
  ///
  RT:=Ctx.GetType(Item.FMountList.ClassInfo);
  A:=rt.GetProperties;
  c:=Length(A);
  if (c<>0) then
  begin
   For i:=0 to c-1 do
   begin
    A[i].SetValue(Item.FMountList,GridMounts.Cells[1,i]);
   end;
  end;
  ///
 finally
  Ctx.free;
 end;
end;

function GetGridVal(Grid:TStringGrid;const name:RawByteString):RawByteString;
var
 i:Integer;
begin
 Result:='';
 i:=Grid.Cols[0].IndexOf(name);
 if (i=-1) then Exit;
 //
 Result:=Grid.Cells[1,i];
end;

procedure SetGridVal(Grid:TStringGrid;const name,value:RawByteString);
var
 i:Integer;
begin
 i:=Grid.Cols[0].IndexOf(name);
 if (i=-1) then Exit;
 //
 Grid.Cells[1,i]:=value;
end;

procedure TfrmGameEditor.LoadParamSfo(UpdateTitle:Boolean);
var
 i:Integer;
 V:RawByteString;
begin
 V:=GetGridVal(GridMounts,'/app0');

 if (Fapp0p=V) then Exit;

 FreeAndNil(ParamSfo);

 ParamSfo:=LoadParamSfoFile(ExcludeTrailingPathDelimiter(V)+
                            DirectorySeparator+
                            'sce_sys'+
                            DirectorySeparator+
                            'param.sfo');

 Fapp0p:=V;

 GridParamSfo.Clear;

 if (ParamSfo=nil) then Exit;

 if (Length(ParamSfo.params)=0) then Exit;
 For i:=0 to High(ParamSfo.params) do
 begin
  if (ParamSfo.params[i].format=SFO_FORMAT_UINT32) then
  begin
   V:='0x'+HexStr(ParamSfo.params[i].GetUInt,8);
  end else
  begin
   V:=Trim(ParamSfo.params[i].GetString);
  end;
  AddRow(GridParamSfo,ParamSfo.params[i].name,V,nil);
 end;

 GridParamSfo.AutoSizeColumn(0);

 //
 if not UpdateTitle then Exit;

 V:=ParamSfo.GetString('TITLE');
 SetGridVal(GridMain,'Name:',V);

 V:=ParamSfo.GetString('TITLE_ID');
 SetGridVal(GridMain,'TitleId:',V);

 V:=ParamSfo.GetString('VERSION');
 SetGridVal(GridMain,'Version:',V);
end;

procedure TfrmGameEditor.GridSelectEditor(Sender:TObject;aCol,aRow:Integer;var Editor:TWinControl);
var
 obj:TObject;
begin
 obj:=TStringGrid(Sender).Objects[0,aRow];
 if (obj=nil) then Exit;
 if not obj.InheritsFrom(TFieldInfo) then Exit;
 //
 TStringGrid(Sender).Tag:=PtrInt(Self);
 //
 TFieldInfo(obj).SelectEditor(TStringGrid(Sender),aCol,aRow,Editor);
end;

procedure TfrmGameEditor.GridEditingDone(Sender: TObject);
begin
 LoadParamSfo(True);
end;

procedure TfrmGameEditor.BtnCancelClick(Sender: TObject);
begin
 Close;
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

procedure TfrmGameEditor.FormClose(Sender:TObject;var CloseAction:TCloseAction);
begin
 if (Item<>nil) then
 begin
  if Item.FLock then
  begin
   Item.FLock:=False;
  end else
  begin
   FreeAndNil(Item);
  end;
 end;
 //
 FreeAndNil(ParamSfo);
 //
 CloseAction:=caFree;
end;

end.

