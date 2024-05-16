unit cfg_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls,

  game_info;

type

  { TfrmCfgEditor }

  TfrmCfgEditor = class(TForm)
    BtnCancel: TButton;
    BtnSysOpen: TButton;
    BtnOk: TButton;
    BtnLogOpen: TButton;
    BtnDataOpen: TButton;
    Edt_BootparamInfo_halt_on_exit: TCheckBox;
    Edt_JITInfo_debug_info: TCheckBox;
    Edt_MiscInfo_strict_ps4_freq: TCheckBox;
    Edt_JITInfo_relative_analize: TCheckBox;
    Edt_JITInfo_print_asm: TCheckBox;
    Edt_BootparamInfo_print_guest_syscall: TCheckBox;
    Edt_BootparamInfo_print_pmap: TCheckBox;
    Edt_BootparamInfo_print_jit_preload: TCheckBox;
    Edt_JITInfo_memory_guard: TCheckBox;
    Edt_MainInfo_fork_proc: TCheckBox;
    Edt_MainInfo_LogFile: TEdit;
    Edt_BootparamInfo_neo: TCheckBox;
    EditPages: TPageControl;
    Edt_MainInfo_system: TEdit;
    Edt_MainInfo_data: TEdit;
    Edt_MiscInfo_renderdoc_capture: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Tab_Misc: TTabSheet;
    Tab_JIT: TTabSheet;
    Tab_MainInfo: TTabSheet;
    Tab_BootparamInfo: TTabSheet;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnDataOpenClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnLogOpenClick(Sender: TObject);
    procedure BtnSysOpenClick(Sender: TObject);
    procedure PageInit(const TabName:RawByteString;obj:TAbstractObject);
    procedure PageSave(const TabName:RawByteString;obj:TAbstractObject);
    procedure FormInit;
    procedure FormSave;
  private

  public
   OnSave     :TNotifyEvent;
   FConfigInfo:TConfigInfo;
  end;

var
  frmCfgEditor: TfrmCfgEditor;

implementation

{$R *.lfm}

uses
 TypInfo,
 Rtti;

procedure TfrmCfgEditor.BtnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TfrmCfgEditor.BtnOkClick(Sender: TObject);
begin
 FormSave;
 Hide;
 if Assigned(OnSave) then
 begin
  OnSave(Self);
 end;
 Close;
end;

procedure DoOpenFile(Edit:TEdit);
var
 d:TOpenDialog;
begin
 d:=nil;
 try
  d:=TOpenDialog.Create(nil);
  d.InitialDir:=Edit.Text;
  d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];
  if d.Execute then
  begin
   Edit.Text:=d.FileName;
  end;
 except
  //
 end;
 FreeAndNil(d);
end;

procedure DoOpenDir(Edit:TEdit);
var
 d:TSelectDirectoryDialog;
begin
 d:=nil;
 try
  d:=TSelectDirectoryDialog.Create(nil);
  d.InitialDir:=Edit.Text;
  d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];
  if d.Execute then
  begin
   Edit.Text:=d.FileName;
  end;
 except
  //
 end;
 FreeAndNil(d);
end;

procedure TfrmCfgEditor.BtnLogOpenClick(Sender: TObject);
begin
 DoOpenFile(Edt_MainInfo_LogFile);
end;

procedure TfrmCfgEditor.BtnSysOpenClick(Sender: TObject);
begin
 DoOpenDir(Edt_MainInfo_system);
end;

procedure TfrmCfgEditor.BtnDataOpenClick(Sender: TObject);
begin
 DoOpenDir(Edt_MainInfo_data);
end;

type
 TMyControl=class(TControl)
  public
   property Text;
 end;

 TMyButtonControl=class(TButtonControl)
  public
   property Checked;
 end;

procedure SetText(control:TComponent;const Text:RawByteString);
begin
 if control.InheritsFrom(TControl) then
 begin
  TMyControl(control).Text:=Text;
 end;
end;

function GetText(control:TComponent):RawByteString;
begin
 Result:='';
 if control.InheritsFrom(TControl) then
 begin
  Result:=TMyControl(control).Text;
 end;
end;

procedure SetBool(control:TComponent;B:Boolean);
begin
 if control.InheritsFrom(TButtonControl) then
 begin
  TMyButtonControl(control).Checked:=B;
 end;
end;

function GetBool(control:TComponent):Boolean;
begin
 Result:=False;
 if control.InheritsFrom(TButtonControl) then
 begin
  Result:=TMyButtonControl(control).Checked;
 end;
end;

procedure TfrmCfgEditor.PageInit(const TabName:RawByteString;obj:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;

 control:TComponent;
begin

 i:=obj.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   control:=FindComponent('Edt_'+TabName+'_'+p.Name);
   Assert(control<>nil,'Edt_'+TabName+'_'+p.Name);

   case p.PropertyType.TypeKind of

    tkSString,
    tkLString,
    tkAString:
     begin
      SetText(control,p.GetValue(obj).AsString);
     end;

    tkBool:
     begin
      SetBool(control,p.GetValue(obj).AsBoolean);
     end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

procedure TfrmCfgEditor.PageSave(const TabName:RawByteString;obj:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;

 control:TComponent;
begin

 i:=obj.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   control:=FindComponent('Edt_'+TabName+'_'+p.Name);
   Assert(control<>nil,'Edt_'+TabName+'_'+p.Name);

   case p.PropertyType.TypeKind of

    tkSString,
    tkLString,
    tkAString:
     begin
      p.SetValue(obj,GetText(control));
     end;

    tkBool:
     begin
      p.SetValue(obj,GetBool(control));
     end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

procedure TfrmCfgEditor.FormInit;
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
begin
 EditPages.ActivePageIndex:=0;

 i:=FConfigInfo.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of

    tkClass:
     begin
      obj:=p.GetValue(FConfigInfo).AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TAbstractObject) then
      begin
       PageInit(p.Name,TAbstractObject(obj));
      end;
     end;

    else;
   end;

   i.Next;
  end;
 finally
  i.free;
 end;

 Show;
end;

procedure TfrmCfgEditor.FormSave;
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
begin
 i:=FConfigInfo.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of

    tkClass:
     begin
      obj:=p.GetValue(FConfigInfo).AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TAbstractObject) then
      begin
       PageSave(p.Name,TAbstractObject(obj));
      end;
     end;

    else;
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

end.

