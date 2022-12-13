unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, Buttons, StdCtrls, ButtonPanel, Grids, Unit2,Unit3, process, INIFiles,
  FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    Panel1: TPanel;
    Process1: TProcess;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Page1BeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure GetAllFiles(Path, ExtMask: String; List: TStrings;SubFolder: Boolean);
    procedure ToolButton3Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  emup: String;
  gamep: String;
implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Page1BeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Close();
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject);
var
  row : integer;
  AProcess: TProcess;
  befehl,bf: String;
  pfademu: String;
begin
     row := StringGrid1.Row;
     befehl := emup + '\fpPS4.exe';
     //befehl := emup + '\game.bat';
     AProcess := TProcess.Create(nil);
     pfademu := '-e ' + gamep + '\' + StringGrid1.Cells[0,row] + '\eboot.bin';
     bf :=  befehl + ' ' + pfademu;
     chDir(emup);
     AProcess.CommandLine :=  bf;

     AProcess.Options := AProcess.Options + [poWaitOnExit];
     AProcess.Execute;
     AProcess.Free;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  Form3.Parent := Form1;
  Form3.Position:= poMainFormCenter;
  Form3.Show;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  Form2.Parent := Form1;
  Form2.Position:= poMainFormCenter;
  Form2.Show;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Close();
end;

procedure TForm1.FormCreate(Sender: TObject);
Var
    INI : TINIFile;
    Dirs : Array[0..127] of pchar;
    m,n,l,I,Count : longint;
    NewDir,temp,tempo,home : String;
    sl,sg : TStringList;
begin
  home := GetCurrentDir;
  home := home + '\Frofpp4.ini';
  INI    := TIniFile.Create(home);
  sl := TStringList.Create;
  sg := TStringList.Create;
  emup := INI.ReadString('PFAD', 'path', '');
  gamep   := INI.ReadString('PFAD', 'path1', '');

    try

    GetAllFiles(gamep,'*',sl,true);

    finally

    end;

    n := 0;

    sg.Sorted:= true;

    for I:=0 to sl.Count-1 do begin
       NewDir := sl[i];
       l := Length(gamep);
       temp := Copy(NewDir,l+2,Length(NewDir)-l);
       m := Pos('\',temp);

       if m <> 0 then
       begin

          temp := Copy(temp,0,m-1);
          sg.Add(temp);

       end;

    end;

    StringGrid1.Clear;
    StringGrid1.RowCount:=sg.Count;
    for I := 0 to sg.Count - 1 do
        StringGrid1.Cells[0, I] := sg[I];
    sl.free;
    sg.free;
end;
procedure TForm1.GetAllFiles(Path, ExtMask: String; List: TStrings;
  SubFolder: Boolean);
var
  i: Integer;
begin
  with TStringList.Create do
  try
    CommaText := ExtMask;
    for i := 0 to Count - 1 do
      FindAllFiles(List, Path, '*' + Strings[i], SubFolder);
  finally
    Free
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  Close;
end;

end.

