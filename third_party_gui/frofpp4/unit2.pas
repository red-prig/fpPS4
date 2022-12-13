unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  ExtCtrls, StdCtrls, ExtendedNotebook, INIFiles;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.Button2Click(Sender: TObject);
begin
  Close();
end;

procedure TForm2.Button1Click(Sender: TObject);
 var
INI:                  TINIFile;
path: string;
path1: string;
home: string;
begin
  home := GetCurrentDir;
  home := home + '\Frofpp4.ini';
  INI    := TIniFile.Create(home);
  path := Edit1.Text;
  path1 := Edit2.Text;

  INI.WriteString('PFAD', 'path', path);
  INI.WriteString('PFAD', 'path1', path1);
  INI.free;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  path: string;
begin
     if SelectDirectoryDialog1.Execute then
        begin
        path := SelectDirectoryDialog1.FileName;
        Edit1.Text := path;
        end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var path: string;
begin
       if SelectDirectoryDialog1.Execute then
        begin
        path := SelectDirectoryDialog1.FileName;
        Edit2.Text := path;
        end;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
 begin

 end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin

end;

procedure TForm2.FormShow(Sender: TObject);
var
INI:                  TINIFile;
path: string;
path1: string;
home: string;
begin
  home := GetCurrentDir;
  home := home + '\Frofpp4.ini';
  INI    := TIniFile.Create(home);

  path := INI.ReadString('PFAD', 'path', '');
  path1   := INI.ReadString('PFAD', 'path1', '');
  Edit1.Text := path;
  Edit2.Text := path1;
  INI.free;
end;

end.

