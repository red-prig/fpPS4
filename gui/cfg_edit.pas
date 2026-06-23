unit cfg_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons,

  lclintf,

  Vulkan,
  vDevice,

  SDL3,
  SDL3_audio,

  game_info,
  form_filler;

type
  TProxyComponent=class(TComponent)
   public
    Function  GetText:RawByteString;          virtual; abstract;
    procedure SetText(const s:RawByteString); virtual; abstract;
    //
    Function  GetInteger:Integer;             virtual; abstract;
    procedure SetInteger(v:Integer);          virtual; abstract;
  end;

  TVulkanDevGuid=class(TProxyComponent)
   public
    src: TComboBox;
    val: RawByteString;
    //
    Function  GetText:RawByteString;          override;
    procedure SetText(const s:RawByteString); override;
  end;

  TVulkanAppFlags=class(TProxyComponent)
   public
    src: TCheckGroup;
    //
    Function  GetInteger:Integer;    override;
    procedure SetInteger(v:Integer); override;
  end;

  TAudioDevice=class(TProxyComponent)
   public
    src: TComboBox;
    //
    Function  GetText:RawByteString;          override;
    procedure SetText(const s:RawByteString); override;
  end;

  { TfrmCfgEditor }

  TfrmCfgEditor = class(TForm)
    BtnExpSys: TSpeedButton;
    BtnRemFw: TSpeedButton;
    BtnCancel: TButton;
    BtnAddFw: TSpeedButton;
    BtnOk: TButton;
    BtnLogOpen: TButton;
    Edt_JITInfo_lazy_jit: TCheckBox;
    Edt_JITInfo_scan_nopsequence: TCheckBox;
    Edt_JITInfo_scan_switchtable: TCheckBox;
    Edt_MainInfo_DefaultFirmware: TComboBox;
    Edt_MiscInfo_fork_proc: TCheckBox;
    Edt_PS4Audio_SpecialDevice_cmb: TComboBox;
    Edt_PS4Audio_HeadphoneDevice_cmb: TComboBox;
    Edt_PS4Audio_ControllerDevice_cmb: TComboBox;
    Edt_PS4Audio_MainDevice_cmb: TComboBox;
    Edt_PS4SystemService_SystemName: TEdit;
    Edt_PS4SystemService_ButtonAssign: TComboBox;
    Edt_PS4SystemService_TimeFormat: TComboBox;
    Edt_PS4SystemService_Language: TComboBox;
    Edt_PS4SystemService_DateFormat: TComboBox;
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
    Edt_MainInfo_LogFile: TEdit;
    Edt_BootparamInfo_neo: TCheckBox;
    EditPages: TPageControl;
    Edt_MiscInfo_renderdoc_capture: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edt_MainInfo_FirmwareList: TListBox;
    PanelHalf: TPanel;
    BtnExpLog: TSpeedButton;
    Tab_PS4Audio: TTabSheet;
    Tab_PS4System: TTabSheet;
    Tab_Vulkan: TTabSheet;
    Tab_Misc: TTabSheet;
    Tab_JIT: TTabSheet;
    Tab_MainInfo: TTabSheet;
    Tab_BootparamInfo: TTabSheet;
    procedure BtnAddFwClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnExpLogClick(Sender: TObject);
    procedure BtnExpSysClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnLogOpenClick(Sender: TObject);
    procedure BtnRemFwClick(Sender: TObject);
    procedure Edt_MainInfo_DefaultFirmwareGetItems(Sender: TObject);
    procedure VulkanInit;
    procedure VulkanPostInit;
    procedure AudioInit;
    procedure FormInit;
    procedure FormSave;
    procedure OnIdleUpdate(Sender:TObject;var Done:Boolean);
    constructor Create(AOwner: TComponent); override;
  private

  public
   destructor  Destroy; override;
  public
   VulkanInfo_device:TVulkanDevGuid;
   VulkanInfo_aflags:TVulkanAppFlags;
   //
   PS4Audio_MainDevice      :TAudioDevice;
   PS4Audio_HeadphoneDevice :TAudioDevice;
   PS4Audio_ControllerDevice:TAudioDevice;
   PS4Audio_SpecialDevice   :TAudioDevice;
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

 ms_shell_hack,
 ps4_libSceSystemService;

var
 FVulkanDeviceInit:Integer=0;
 FVulkanDeviceList:APhysicalDeviceProperties=nil;

function VulkanInitIsFinished:Boolean; inline;
begin
 Result:=(FVulkanDeviceInit>=2);
end;

function VulkanInitCatchFinish:Boolean; inline;
begin
 Result:=(System.InterlockedCompareExchange(FVulkanDeviceInit,3,2)=2)
end;

function GetPhysicalDeviceList_thread(parameter:pointer):ptrint; register;
begin
 Result:=0;
 //
 if vDevice.LoadVulkan then
 begin
  FVulkanDeviceList:=GetPhysicalDeviceList();
 end;
 //
 FVulkanDeviceInit:=2;
 //
 if Assigned(Classes.WakeMainThread) then
 begin
  Classes.WakeMainThread(nil);
 end;
end;

Procedure InitVulkanDeviceList;
begin
 if (System.InterlockedCompareExchange(FVulkanDeviceInit,1,0) <> 0) then Exit;

 BeginThread(@GetPhysicalDeviceList_thread);
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

function DoOpenFile(const Input,InitialDir:RawByteString):RawByteString;
var
 d:TOpenDialog;
 Cookie:Pointer;
begin
 Cookie:=RegisterDllHack;

 Result:=Input;
 d:=nil;
 try
  d:=TOpenDialog.Create(nil);
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

procedure TfrmCfgEditor.BtnLogOpenClick(Sender: TObject);
begin
 Edt_MainInfo_LogFile.Text:=DoOpenFile(Edt_MainInfo_LogFile.Text,Edt_MainInfo_LogFile.Text);
end;

procedure TfrmCfgEditor.BtnAddFwClick(Sender:TObject);
var
 new:RawByteString;
begin
 new:=DoOpenDir('','');
 if (new='') then Exit;

 Edt_MainInfo_FirmwareList   .Items.Add(new);
 Edt_MainInfo_DefaultFirmware.Items.Add(new);
end;

procedure TfrmCfgEditor.BtnRemFwClick(Sender: TObject);
var
 i:Integer;
begin
 i:=Edt_MainInfo_FirmwareList.ItemIndex;
 if (i>=0) and (i<Edt_MainInfo_FirmwareList.Count) then
 begin
  Edt_MainInfo_FirmwareList   .Items.Delete(i);
  Edt_MainInfo_DefaultFirmware.Items.Delete(i);
 end;
end;

procedure TfrmCfgEditor.Edt_MainInfo_DefaultFirmwareGetItems(Sender: TObject);
var
 i:Integer;
 S:RawByteString;
begin
 if (Edt_MainInfo_DefaultFirmware.Items.Count=0) and
    (Edt_MainInfo_FirmwareList   .Items.Count<>0) then
 begin
  //preload
  For i:=0 to Edt_MainInfo_FirmwareList.Items.Count-1 do
  begin
   S:=Edt_MainInfo_FirmwareList.Items.Strings[i];
   Edt_MainInfo_DefaultFirmware.Items.Add(S);
  end;
 end;
end;

function OpenFolderOfFile(APath:RawByteString): Boolean;
begin
 APath:=ExtractFilePath(APath);
 if (Trim(APath)='') then
 begin
  APath:=GetCurrentDir;
 end;
 Result:=OpenDocument(APath);
end;

procedure TfrmCfgEditor.BtnExpLogClick(Sender: TObject);
begin
 OpenFolderOfFile(Edt_MainInfo_LogFile.Text);
end;

procedure TfrmCfgEditor.BtnExpSysClick(Sender: TObject);
begin
 OpenDocument(Edt_MainInfo_DefaultFirmware.Text);
end;

Function TVulkanDevGuid.GetText:RawByteString;
var
 i:Integer;
 ptr:PVkPhysicalDeviceProperties;
begin
 Result:=val;
 if (src=nil) then Exit;

 if not VulkanInitIsFinished then Exit;

 Result:='';

 i:=src.ItemIndex;
 if (i=-1) then Exit;

 ptr:=PVkPhysicalDeviceProperties(src.Items.Objects[i]);
 if (ptr=nil) then Exit;

 val:=GUIDToString(TGUID(ptr^.pipelineCacheUUID));
 Result:=val;
end;

procedure TVulkanDevGuid.SetText(const s:RawByteString);
var
 i:Integer;
 Guid:TGUID;
 ptr:PVkPhysicalDeviceProperties;
begin
 val:=s;
 if (src=nil) then Exit;

 if not VulkanInitIsFinished then Exit;

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

 if (src.Items.Count>0) then
 begin
  src.ItemIndex:=0;
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

const
 CZERO   :pchar='';
 CNULL   :pchar='[NULL]';
 CDEFAULT:pchar='[DEFAULT]';

Function TAudioDevice.GetText:RawByteString;
var
 i:Integer;
 ptr:pchar;
begin
 Result:='';
 if (src=nil) then Exit;

 i:=src.ItemIndex;
 if (i=-1) then Exit;

 ptr:=pchar(src.Items.Objects[i]);

 if (ptr<>nil) then
 begin
  Result:=ptr;
 end else
 begin
  Result:=src.Items.Strings[i];
 end;
end;

procedure TAudioDevice.SetText(const s:RawByteString);
var
 i:Integer;

 procedure _set_index(i:Integer); inline;
 begin
  if (i<src.Items.Count) then
  begin
   src.ItemIndex:=i;
  end;
 end;

begin
 if (src=nil) then Exit;

 if (s=CNULL) then
 begin
  _set_index(0);
  Exit;
 end else
 if (s=CDEFAULT) then
 begin
  _set_index(1);
  Exit;
 end;

 For i:=0 to src.Items.Count-1 do
 begin
  if (src.Items.Strings[i]=s) then
  begin
   _set_index(i);
   Exit;
  end;
 end;

end;

//
type
 TCfgFormData=class(TFormDataProvider)
  procedure SetText   (control:TComponent;const Text:RawByteString); override;
  function  GetText   (control:TComponent):RawByteString;            override;
  procedure SetInteger(control:TComponent;i:Integer);                override;
  function  GetInteger(control:TComponent):Integer;                  override;
  procedure SetBool   (control:TComponent;B:Boolean);                override;
  function  GetBool   (control:TComponent):Boolean;                  override;
  procedure SetClass  (control:TComponent;Obj:TObject);              override;
  procedure GetClass  (control:TComponent;Obj:TObject);              override;
 end;

procedure TCfgFormData.SetText(control:TComponent;const Text:RawByteString);
begin
 if control.InheritsFrom(TProxyComponent) then
 begin
  TProxyComponent(control).SetText(Text);
 end else
 if control.InheritsFrom(TControl) then
 begin
  TMyControl(control).Text:=Text;
 end;
end;

function TCfgFormData.GetText(control:TComponent):RawByteString;
begin
 Result:='';
 if control.InheritsFrom(TProxyComponent) then
 begin
  Result:=TProxyComponent(control).GetText;
 end else
 if control.InheritsFrom(TControl) then
 begin
  Result:=TMyControl(control).Text;
 end;
end;

//

procedure TCfgFormData.SetInteger(control:TComponent;i:Integer);
begin
 if control.InheritsFrom(TProxyComponent) then
 begin
  TProxyComponent(control).SetInteger(i);
 end else
 if control.InheritsFrom(TCustomComboBox) then
 begin
  if (i=-1) then
  begin
   //preload default
   if (control.Name='Edt_PS4SystemService_Language') then
   begin
    i:=GetHostSystemLang;
   end else
   if (control.Name='Edt_PS4SystemService_DateFormat') then
   begin
    i:=GetHostSystemDateFormat;
   end else
   if (control.Name='Edt_PS4SystemService_TimeFormat') then
   begin
    i:=GetHostSystemTimeFormat;
   end;
   //preload default
  end;

  TCustomComboBox(control).ItemIndex:=i;
 end;
end;

function TCfgFormData.GetInteger(control:TComponent):Integer;
begin
 Result:=0;
 if control.InheritsFrom(TProxyComponent) then
 begin
  Result:=TProxyComponent(control).GetInteger;
 end else
 if control.InheritsFrom(TCustomComboBox) then
 begin
  Result:=TCustomComboBox(control).ItemIndex;
 end;
end;

//

procedure TCfgFormData.SetBool(control:TComponent;B:Boolean);
begin
 if control.InheritsFrom(TButtonControl) then
 begin
  TMyButtonControl(control).Checked:=B;
 end;
end;

function TCfgFormData.GetBool(control:TComponent):Boolean;
begin
 Result:=False;
 if control.InheritsFrom(TButtonControl) then
 begin
  Result:=TMyButtonControl(control).Checked;
 end;
end;

procedure TCfgFormData.SetClass(control:TComponent;Obj:TObject);
var
 A:TSerializeStringArray;
 i:Integer;
begin
 if control.InheritsFrom(TListBox) then
 begin
  A:=TSerializeStringArray(Obj);

  TListBox(control).Items.Clear;

  if (Length(A.values)>0) then
  For i:=0 to High(A.values) do
  begin
   TListBox(control).Items.Add(A.values[i]);
  end;

 end;
end;

procedure TCfgFormData.GetClass(control:TComponent;Obj:TObject);
var
 A:TSerializeStringArray;
 i,c:Integer;
begin
 if control.InheritsFrom(TListBox) then
 begin
  A:=TSerializeStringArray(Obj);

  c:=TListBox(control).Items.Count;

  SetLength(A.values,c);

  if (c>0) then
  For i:=0 to c-1 do
  begin
   A.values[i]:=TListBox(control).Items.Strings[i];
  end;

 end;
end;

//

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
begin
 if (VulkanInfo_device=nil) then
 begin
  VulkanInfo_device:=TVulkanDevGuid.Create(Self);
  VulkanInfo_device.Name:='Edt_VulkanInfo_device'; //FindComponent
  VulkanInfo_device.src :=Edt_VulkanInfo_device_cmb;
 end;

 if (VulkanInfo_aflags=nil) then
 begin
  VulkanInfo_aflags:=TVulkanAppFlags.Create(Self);
  VulkanInfo_aflags.Name:='Edt_VulkanInfo_app_flags'; //FindComponent
  VulkanInfo_aflags.src :=GrAppFlags;
 end;

 Edt_VulkanInfo_device_cmb.Clear;
 Edt_VulkanInfo_device_cmb.ItemIndex:=-1;

 InitVulkanDeviceList;
 VulkanPostInit;
end;

procedure TfrmCfgEditor.VulkanPostInit;
var
 i:Integer;
 deviceName:RawByteString;
begin
 if not VulkanInitIsFinished then Exit;

 if Length(FVulkanDeviceList)=0 then Exit;

 Edt_VulkanInfo_device_cmb.Clear;
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

 //update
 VulkanInfo_device.SetText(VulkanInfo_device.val);
 //update
end;

procedure TfrmCfgEditor.AudioInit;
var
 SDL3Audio:TSDL3Audio;
 list:PSDL_AudioDeviceID;
 i,count:Integer;
 a_name:pchar;

 procedure _init_proxy(var A:TAudioDevice;src:TComboBox;const Name:RawByteString); inline;
 begin
  if (A=nil) then
  begin
   A:=TAudioDevice.Create(Self);
   A.Name:=Name; //FindComponent
   A.src :=src;
  end;
 end;

 procedure _add(const Item:RawByteString;AnObject:Pointer); inline;
 begin
  Edt_PS4Audio_MainDevice_cmb      .AddItem(Item,TObject(AnObject));
  Edt_PS4Audio_HeadphoneDevice_cmb .AddItem(Item,TObject(AnObject));
  Edt_PS4Audio_ControllerDevice_cmb.AddItem(Item,TObject(AnObject));
  Edt_PS4Audio_SpecialDevice_cmb   .AddItem(Item,TObject(AnObject));
 end;

 procedure _set_index(i:Integer); inline;
 begin
  Edt_PS4Audio_MainDevice_cmb      .ItemIndex:=i;
  Edt_PS4Audio_HeadphoneDevice_cmb .ItemIndex:=i;
  Edt_PS4Audio_ControllerDevice_cmb.ItemIndex:=i;
  Edt_PS4Audio_SpecialDevice_cmb   .ItemIndex:=i;
 end;

begin
 _init_proxy(PS4Audio_MainDevice      ,Edt_PS4Audio_MainDevice_cmb      ,'Edt_PS4Audio_MainDevice');
 _init_proxy(PS4Audio_HeadphoneDevice ,Edt_PS4Audio_HeadphoneDevice_cmb ,'Edt_PS4Audio_HeadphoneDevice');
 _init_proxy(PS4Audio_ControllerDevice,Edt_PS4Audio_ControllerDevice_cmb,'Edt_PS4Audio_ControllerDevice');
 _init_proxy(PS4Audio_SpecialDevice   ,Edt_PS4Audio_SpecialDevice_cmb   ,'Edt_PS4Audio_SpecialDevice');

 Edt_PS4Audio_MainDevice_cmb      .Clear;
 Edt_PS4Audio_HeadphoneDevice_cmb .Clear;
 Edt_PS4Audio_ControllerDevice_cmb.Clear;
 Edt_PS4Audio_SpecialDevice_cmb   .Clear;

 SDL3Audio:=SDL_InitAudio();

 if (SDL3Audio<>nil) then
 begin
  _add('[Disable]',CNULL);
  _add('[Default]',CDEFAULT);
  _set_index(1);

  count:=0;
  list :=SDL3Audio.SDL_GetAudioPlaybackDevices(@count);

  if (list<>nil) and (count<>0) then
  begin

   For i:=0 to count-1 do
   begin
    a_name:=SDL3Audio.SDL_GetAudioDeviceName(list[i]);
    _add(a_name,nil);
   end;
  end;

  SDL_free(list);
  //
  FreeAndNil(SDL3Audio);
 end else
 begin
  _add('[No audio]',CZERO);
  _set_index(0);
 end;

end;

procedure TfrmCfgEditor.OnIdleUpdate(Sender:TObject;var Done:Boolean);
begin
 if VulkanInitCatchFinish then
 begin
  //init list
  VulkanPostInit;
 end;
end;

constructor TfrmCfgEditor.Create(AOwner: TComponent);
begin
 inherited;
 Application.AddOnIdleHandler(@OnIdleUpdate,False);
end;

destructor TfrmCfgEditor.Destroy;
begin
 Application.RemoveOnIdleHandler(@OnIdleUpdate);
 inherited;
end;

procedure TfrmCfgEditor.FormInit;
var
 Provider:TCfgFormData;
begin
 VulkanInit;
 AudioInit;

 EditPages.ActivePageIndex:=0;

 Provider:=TCfgFormData.Create;

 FormLoad(Self,Provider,FConfigInfo);

 Provider.Free;

 Show;
end;

procedure TfrmCfgEditor.FormSave;
var
 Provider:TCfgFormData;
begin
 Provider:=TCfgFormData.Create;

 form_filler.FormSave(Self,Provider,FConfigInfo);

 Provider.Free;
end;

end.

