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
  game_find,
  logging,
  md_systm_fork,
  md_systm_reserve;

{$R *.res}

//{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

begin
  //set_log_filter('"*:Off"');
  //LOG_INFO('LOG_INFO1');
  //set_log_filter('"*:Off" "fpPS4:Info" "rec:Info"');
  //LOG_INFO('LOG_INFO2');

  md_systm_fork.md_run_forked;
  Application.Tag:=md_systm_reserve.md_map_reserve().error;
  //
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TFrmFind, FrmFind);
  //Application.CreateForm(TfrmCfgEditor, frmCfgEditor);
  //Application.CreateForm(TfrmGameEditor, frmGameEditor);
  Application.Run;
end.

