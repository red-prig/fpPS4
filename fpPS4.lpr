program fpPS4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  main,
  game_edit,
  vm_pmap;

{$R *.res}

begin
  vm_pmap.pmap_reserve(False);
  //
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //Application.CreateForm(TfrmGameEditor, frmGameEditor);
  Application.Run;
end.

