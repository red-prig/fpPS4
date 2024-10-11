unit cfg_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls,

  Vulkan,
  vDevice,

  game_info;

type
  TVulkanDevGuid=class(TComponent)
   src: TComboBox;
   //
   Function  GetText:RawByteString;
   procedure SetText(const s:RawByteString);
  end;

  TVulkanAppFlags=class(TComponent)
   src: TCheckGroup;
   //
   Function  GetInteger:Integer;
   procedure SetInteger(v:Integer);
  end;

  { TfrmCfgEditor }

  TfrmCfgEditor = class(TForm)
    BtnCancel: TButton;
    BtnSysOpen: TButton;
    BtnOk: TButton;
    BtnLogOpen: TButton;
    BtnDataOpen: TButton;
    GrAppFlags: TCheckGroup;
    Edt_VulkanInfo_device_cmb: TComboBox;
    Edt_BootparamInfo_halt_on_exit: TCheckBox;
    Edt_BootparamInfo_print_gpu_ops: TCheckBox;
    Edt_BootparamInfo_print_gpu_hint: TCheckBox;
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
    Label4: TLabel;
    Tab_Vulkan: TTabSheet;
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
    procedure VulkanInit;
    procedure FormInit;
    procedure FormSave;
  private

  public
   VulkanInfo_device   :TVulkanDevGuid;
   VulkanInfo_app_flags:TVulkanAppFlags;
   //
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

var
 FVulkanDeviceInit:Boolean=False;
 FVulkanDeviceList:APhysicalDeviceProperties=nil;

Procedure InitVulkanDeviceList;
begin
 if FVulkanDeviceInit then Exit;

 if vDevice.LoadVulkan then
 begin
  FVulkanDeviceList:=GetPhysicalDeviceList();
 end;

 FVulkanDeviceInit:=True;
end;

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

//src: TComboBox;

Function TVulkanDevGuid.GetText:RawByteString;
var
 i:Integer;
 ptr:PVkPhysicalDeviceProperties;
begin
 Result:='';
 if (src=nil) then Exit;

 i:=src.ItemIndex;
 ptr:=PVkPhysicalDeviceProperties(src.Items.Objects[i]);

 if (ptr=nil) then Exit;

 Result:=GUIDToString(TGUID(ptr^.pipelineCacheUUID));
end;

procedure TVulkanDevGuid.SetText(const s:RawByteString);
var
 i:Integer;
 Guid:TGUID;
 ptr:PVkPhysicalDeviceProperties;
begin
 if (src=nil) then Exit;

 Guid:=Default(TGUID);
 TryStringToGUID(s,Guid);

 For i:=0 to src.Items.Count-1 do
 begin
  ptr:=PVkPhysicalDeviceProperties(src.Items.Objects[i]);
  if (ptr<>nil) then
  if CompareByte(Guid,TGUID(ptr^.pipelineCacheUUID),SizeOf(TGUID))=0 then
  begin
   src.ItemIndex:=i;
   Exit;
  end;
 end;

end;

//

Function TVulkanAppFlags.GetInteger:Integer;
var
 i:Integer;
begin
 Result:=0;
 if (src=nil) then Exit;

 For i:=0 to src.Items.Count-1 do
 if src.Checked[i] then
 begin
  Result:=Result or (1 shl i);
 end;
end;

procedure TVulkanAppFlags.SetInteger(v:Integer);
var
 i:Integer;
begin
 if (src=nil) then Exit;

 For i:=0 to src.Items.Count-1 do
 begin
  src.Checked[i]:=(v and (1 shl i))<>0;
 end;
end;

//

procedure SetText(control:TComponent;const Text:RawByteString);
begin
 if control.InheritsFrom(TControl) then
 begin
  TMyControl(control).Text:=Text;
 end else
 if (control is TVulkanDevGuid) then
 begin
  TVulkanDevGuid(control).SetText(Text);
 end;
end;

function GetText(control:TComponent):RawByteString;
begin
 Result:='';
 if control.InheritsFrom(TControl) then
 begin
  Result:=TMyControl(control).Text;
 end else
 if (control is TVulkanDevGuid) then
 begin
  Result:=TVulkanDevGuid(control).GetText;
 end;
end;

//

procedure SetInteger(control:TComponent;i:Integer);
begin
 if (control is TVulkanAppFlags) then
 begin
  TVulkanAppFlags(control).SetInteger(i);
 end;
end;

function GetInteger(control:TComponent):Integer;
begin
 Result:=0;
 if (control is TVulkanAppFlags) then
 begin
  Result:=TVulkanAppFlags(control).GetInteger;
 end;
end;

//

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

//

procedure TfrmCfgEditor.PageInit(const TabName:RawByteString;obj:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 TypeKind:TTypeKind;

 cname:RawByteString;
 control:TComponent;
begin

 i:=obj.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   cname:='Edt_'+TabName+'_'+p.Name;
   control:=FindComponent(cname);
   Assert(control<>nil,cname);
   if (control=nil) then Exit;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString:
     begin
      SetText(control,p.GetValue(obj).AsString);
     end;

    tkInteger:
     begin
      SetInteger(control,p.GetValue(obj).AsInteger);
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
 TypeKind:TTypeKind;

 cname:RawByteString;
 control:TComponent;
begin

 i:=obj.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   cname:='Edt_'+TabName+'_'+p.Name;
   control:=FindComponent(cname);
   Assert(control<>nil,cname);
   if (control=nil) then Exit;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString:
     begin
      p.SetValue(obj,GetText(control));
     end;

    tkInteger:
     begin
      p.SetValue(obj,GetInteger(control));
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

Function GetApiVersionStr(apiVersion:TVkUInt32):RawByteString;
begin
 Result:=IntToStr(VK_API_VERSION_MAJOR(apiVersion))+'.'+
         IntToStr(VK_API_VERSION_MINOR(apiVersion))+'.'+
         IntToStr(VK_API_VERSION_PATCH(apiVersion));
end;

Function GetDrvVersionStr(driverVersion,vendorID:TVkUInt32):RawByteString;
begin
 case vendorid of
  // NVIDIA
  4318:
   begin
    Result:=IntToStr((driverVersion shr 22) and $3ff)+'.'+
            IntToStr((driverVersion shr 14) and $0ff)+'.'+
            IntToStr((driverVersion shr  6) and $0ff)+'.'+
            IntToStr((driverVersion       ) and $03f);
    Exit;
   end;
  // Intel
  {$IFDEF WINDOWS}
  $8086:
   begin
    Result:=IntToStr((driverVersion shr 14)          )+'.'+
            IntToStr((driverVersion       ) and $3fff);
    Exit;
   end;
  {$ENDIF}
  else;
 end;

 // Use Vulkan version conventions if vendor mapping is not available
 Result:=IntToStr(VK_VERSION_MAJOR(driverVersion))+'.'+
         IntToStr(VK_VERSION_MINOR(driverVersion))+'.'+
         IntToStr(VK_VERSION_PATCH(driverVersion));
end;

procedure TfrmCfgEditor.VulkanInit;
var
 i:Integer;
 deviceName:RawByteString;
begin
 InitVulkanDeviceList;

 if (VulkanInfo_device=nil) then
 begin
  VulkanInfo_device:=TVulkanDevGuid.Create(Self);
  VulkanInfo_device.Name:='Edt_VulkanInfo_device'; //FindComponent
  VulkanInfo_device.src :=Edt_VulkanInfo_device_cmb;
 end;

 if (VulkanInfo_app_flags=nil) then
 begin
  VulkanInfo_app_flags:=TVulkanAppFlags.Create(Self);
  VulkanInfo_app_flags.Name:='Edt_VulkanInfo_app_flags'; //FindComponent
  VulkanInfo_app_flags.src :=GrAppFlags;
  //////
 end;

 Edt_VulkanInfo_device_cmb.Clear;
 Edt_VulkanInfo_device_cmb.ItemIndex:=-1;

 if Length(FVulkanDeviceList)=0 then Exit;

 Edt_VulkanInfo_device_cmb.AddItem('Auto',nil);
 Edt_VulkanInfo_device_cmb.ItemIndex:=0;

 For i:=0 to High(FVulkanDeviceList) do
 if (VK_API_VERSION_VARIANT(FVulkanDeviceList[i].apiVersion)=0) then
 begin
  deviceName:=RawByteString(FVulkanDeviceList[i].deviceName);

  deviceName:=deviceName+' ('+GetApiVersionStr(FVulkanDeviceList[i].apiVersion)+')';

  deviceName:=deviceName+' ('+GetDrvVersionStr(FVulkanDeviceList[i].driverVersion,
                                               FVulkanDeviceList[i].vendorID)+')';

  Edt_VulkanInfo_device_cmb.AddItem(deviceName,TObject(@FVulkanDeviceList[i]));
 end;

end;

procedure TfrmCfgEditor.FormInit;
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
begin
 VulkanInit;

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

