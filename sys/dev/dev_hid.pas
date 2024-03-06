unit dev_hid;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sys_conf;

procedure hid_init();

implementation

uses
 errno,
 systm,
 vuio,
 kern_thr,
 subr_backtrace;

var
 pad_counter:Byte=0;

function generateHandle(port_type,handle_id,proc_id:Byte):DWORD;
var
 count:Byte;
begin
 count:=pad_counter+1;
 if (count=0) then
 begin
  count:=pad_counter+2;
 end;
 pad_counter:=count;
 Result:=handle_id or
         ((port_type and $f) shl 24) or
         ((proc_id and $ff) shl  8) or
                     (count shl 16);
end;

//case 0: Mouse
//case 1: Keyboard
//case 2: Status
//case 3: Controller
//case 5: Auth

type
 p_open_port_args=^t_open_port_args;
 t_open_port_args=packed record
  port_userid:Integer;
  port_type  :Integer;
  port_index :Integer;
 end;

 p_pad_state_args=^t_pad_state_args;
 t_pad_state_args=packed record
  id    :Integer; // handle/device_id
  unknow:Integer;
  state :Pointer;
 end;


 t_pad_stick_info=packed record
  deadZoneLeft :Byte;
  deadZoneRight:Byte;
 end;

 t_touch_pad_info=packed record
  pixelDensity:DWORD;
  x           :Word;
  y           :Word;
 end;

 t_pad_ext_info=packed record
  case byte of
   0:(quantityOfSelectorSwitch:Byte);
   1:(maxPhysicalWheelAngle:Integer);
   2:(data:array[0..7] of Byte);
 end;

 p_pad_device_info=^t_pad_device_info;
 t_pad_device_info=packed record //0x40
  conn_type  :Byte;
  pad1       :array[0..2] of Byte;
  device_id  :Integer;
  unknow1    :QWORD;
  hid_vid    :Word;
  hid_did    :Word;
  unknow2    :array[0..11] of Byte;
  dualshock_i:Byte;
  pad2       :array[0..2] of Byte;
  capability1:Integer;
  dev_classid:Byte;
  unknow3    :Byte;
  capability2:Byte;
  unknow4    :Byte;
  ext_data   :t_pad_ext_info;
  touchpad   :t_touch_pad_info;
  stick_info :t_pad_stick_info;
  unknow5    :Word;
 end;
 {$IF sizeof(t_pad_device_info)<>64}{$STOP sizeof(t_pad_device_info)<>64}{$ENDIF}

 t_analog_stick=packed record
  x,y:Byte;
 end;

 t_analog_buttons=packed record
  l2,r2:Byte;
 end;

 t_touch=packed record
  id :Byte;
  pad:array[0..2] of Byte;
  x  :word;
  y  :word;
 end;

 t_touch_group=packed record
  reserve1:DWORD;
  touchNum:Byte;
  pad     :array[0..2] of Byte;
  touch   :array[0..1] of t_touch;
 end;

 p_pad_state=^t_pad_state;
 t_pad_state=packed record //0xA8
  timestamp    :QWORD;
  unk1         :DWORD;
  buttons      :DWORD;
  leftStick    :t_analog_stick;
  rightStick   :t_analog_stick;
  analogButtons:t_analog_buttons;
  unknow0      :Byte;
  touch_gcount :Byte;  //[0..3]
  touch_groups :array[0..3] of t_touch_group;
  unknow       :array[0..39] of Byte;
  unique_len   :Byte;
  unique_data  :array[0..6] of Byte;
 end;
 {$IF sizeof(t_pad_state)<>168}{$STOP sizeof(t_pad_state)<>168}{$ENDIF}

 p_read_state_args=^t_read_state_args;
 t_read_state_args=packed record
  handle   :Integer;
  _align1  :Integer;
  state    :p_pad_state; //num*0xA8
  num      :Integer;
  unk1     :Integer;
  device_id:PInteger;
  s_version:PInteger;
  read_type:Integer;
  unk3     :Integer;
 end;

const
 //ScePadButtonDataOffset
 SCE_PAD_BUTTON_L3          = $00000002;
 SCE_PAD_BUTTON_R3          = $00000004;
 SCE_PAD_BUTTON_OPTIONS     = $00000008;
 SCE_PAD_BUTTON_UP          = $00000010;
 SCE_PAD_BUTTON_RIGHT       = $00000020;
 SCE_PAD_BUTTON_DOWN        = $00000040;
 SCE_PAD_BUTTON_LEFT        = $00000080;
 SCE_PAD_BUTTON_L2          = $00000100;
 SCE_PAD_BUTTON_R2          = $00000200;
 SCE_PAD_BUTTON_L1          = $00000400;
 SCE_PAD_BUTTON_R1          = $00000800;
 SCE_PAD_BUTTON_TRIANGLE    = $00001000;
 SCE_PAD_BUTTON_CIRCLE      = $00002000;
 SCE_PAD_BUTTON_CROSS       = $00004000;
 SCE_PAD_BUTTON_SQUARE      = $00008000;
 SCE_PAD_BUTTON_TOUCH_PAD   = $00100000;
 SCE_PAD_BUTTON_INTERCEPTED = $80000000;

type
 p_motion_calib_state=^t_motion_calib_state;
 t_motion_calib_state=packed record
  field_0x0 :Word;
  field_0x2 :Integer;
  field_0x6 :Word;
  field_0x8 :Integer;
  field_0xc :Word;
  field_0xe :Integer;
  field_0x12:Word;
  field_0x14:Word;
  field_0x16:Integer;
  field_0x1a:Word;
  field_0x1c:Integer;
  field_0x20:Word;
  field_0x22:Word;
  field_0x24:Word;
  field_0x26:Word;
  field_0x28:Word;
 end;

Function hidIoctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 td:p_kthread;
 val:Integer;
 u:packed record
  case byte of
   0:(pad_device_info:t_pad_device_info);
   1:(pad_state:t_pad_state);
   2:(motion_calib_state:t_motion_calib_state);
 end;
begin
 Result:=0;

 td:=curkthread;
 if (td=nil) then Exit(-1);

 case cmd of

  $800c4802: //sceHidOpenPortForUser
    begin
     with p_open_port_args(data)^ do
     begin
      if (port_userid<>-1) or   //user_id
         (port_type  <> 3) or   //port_type
         (port_index <> 0) then //port_index
      begin
       Writeln('sceHidOpenPortForUser(',port_userid,',',
                                        port_type  ,',',
                                        port_index ,')');

      end;

      //handle
      td^.td_retval[0]:=generateHandle(port_type,0,0);

      Writeln('handle=0x',HexStr(td^.td_retval[0],8));
     end;
    end;

  $80104801: //sceHidGetDeviceInfoForUser
    begin
     with p_pad_state_args(data)^ do
     begin
      Writeln('device_id=0x',HexStr(id,8));

      u.pad_device_info:=Default(t_pad_device_info);

      u.pad_device_info.conn_type  :=0;
      u.pad_device_info.device_id  :=$60300;
      u.pad_device_info.hid_vid    :=$54c; //Vendor ID = 054C
      u.pad_device_info.hid_did    :=$5c4; //Device ID = 05C4 (DualShock 4 [CUH-ZCT1x])
      u.pad_device_info.dualshock_i:=4;
      u.pad_device_info.capability1:=$37;
      u.pad_device_info.dev_classid:=0;
      u.pad_device_info.touchpad.pixelDensity:=$1186; // (4.486)
      u.pad_device_info.touchpad.x:=$780; //1920
      u.pad_device_info.touchpad.y:=$3ae; //942
      u.pad_device_info.stick_info.deadZoneLeft :=$d;
      u.pad_device_info.stick_info.deadZoneRight:=$d;

      Result:=copyout(@u.pad_device_info,state,sizeof(t_pad_device_info));
     end;
    end;

  $80104823: //sceHidControllerMotionCalibReadForUser
    begin
     with p_pad_state_args(data)^ do
     begin
      Writeln('device_id=0x',HexStr(id,8));

      u.motion_calib_state:=Default(t_motion_calib_state);

      Result:=copyout(@u.motion_calib_state,state,sizeof(t_motion_calib_state));
     end;
    end;

  $8030482E: //sceHidControllerRead2ForUser
    begin
     with p_read_state_args(data)^ do
     begin

      Writeln('handle=0x',HexStr(handle,8));

      u.pad_state:=Default(t_pad_state);

      u.pad_state.timestamp:=$101010;
      u.pad_state.buttons:=SCE_PAD_BUTTON_UP or SCE_PAD_BUTTON_LEFT;

      u.pad_state.leftStick.x:=1;
      u.pad_state.leftStick.y:=2;

      u.pad_state.rightStick.x:=3;
      u.pad_state.rightStick.y:=4;

      u.pad_state.analogButtons.l2:=5;
      u.pad_state.analogButtons.r2:=6;


      u.pad_state.touch_gcount:=1;
      u.pad_state.touch_groups[0].touchNum:=2;

      u.pad_state.touch_groups[0].touch[0].id:=1;
      u.pad_state.touch_groups[0].touch[0].x :=2;
      u.pad_state.touch_groups[0].touch[0].y :=3;

      u.pad_state.touch_groups[0].touch[1].id:=4;
      u.pad_state.touch_groups[0].touch[1].x :=5;
      u.pad_state.touch_groups[0].touch[1].y :=6;


      u.pad_state.unique_len :=7;
      u.pad_state.unique_data[6]:=7;


      Result:=copyout(@u.pad_state,state,sizeof(t_pad_state));

      val:=$60300;
      copyout(@val,device_id,4);

      val:=1;
      copyout(@val,s_version,4);

      //num * 0xa8

      writeln;

      //num
      td^.td_retval[0]:=1;
     end;
    end

 else
  begin
   print_error_td('hidIoctl(0x'+HexStr(cmd,8)+')');
   Assert(False);
   Result:=EINVAL;
  end;
 end;

end;

Function hidRead(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
begin
 Result:=0;
end;

const
 hid_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :D_TRACKCLOSE;
  d_name        :'hid';
  d_open        :nil;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :@hidRead;
  d_write       :nil;
  d_ioctl       :@hidIoctl;
  d_poll        :nil;
  d_mmap        :nil;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :nil;
  d_purge       :nil;
  d_mmap_single :nil;
  d_mmap_single2:nil;
 );

procedure hid_init();
begin
 make_dev(@hid_cdevsw,0,0,0,&666,'hid',[]);
end;

end.

