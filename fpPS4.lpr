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
  cfg_edit,
  sys_crt_gui,
  md_systm,
  vm_pmap;

{$R *.res}

begin
  md_systm.md_run_forked;
  Application.Tag:=vm_pmap.pmap_reserve(False);
  //
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //Application.CreateForm(TfrmCfgEditor, frmCfgEditor);
  //Application.CreateForm(TfrmGameEditor, frmGameEditor);
  Application.Run;
end.

