unit dev_dce;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 vm,
 vmparam,
 sys_conf,
 sys_event,
 time,
 kern_mtx,
 display_interface;

procedure dce_initialize();

var
 dce_interface:TAbstractDisplay=TDisplayInterface;

 dce_handle:TDisplayHandle;

 dce_mtx:mtx;

 dce_page:Pointer;

 knlist_lock_flip:mtx;
 g_video_out_event_flip:t_knlist;

implementation

uses
 errno,
 systm,
 vm_pmap,
 subr_backtrace,
 sys_vm_object,
 vm_pager,
 md_time,
 kern_proc,
 kern_timeout;

const
 EVENTID_FLIP     =$0006;
 EVENTID_VBLANK   =$0007;
 EVENTID_SETMODE  =$0051;
 EVENTID_POSITION =$0058;
 EVENTID_PREVBLANK=$0059;

 //$0062 wZI4fmUJMlw / vXMpe7Murfc
 //$0063 sys_vblank

procedure knote_eventid(event_id:WORD;flipArg:QWORD;lockflags:Integer);
begin
 knote(@g_video_out_event_flip, event_id or (flipArg shl 16), lockflags);
end;

var
 callout_vblank:t_callout;
 callout_refs  :Int64=0;

procedure vblank_expire(arg:Pointer);
begin
 if (callout_refs<>0) then
 begin
  knote_eventid(EVENTID_VBLANK, 0, 1); //SCE_VIDEO_OUT_EVENT_VBLANK
  //
  callout_reset(@callout_vblank, callout_vblank.c_time, @vblank_expire, nil);
 end;
end;

procedure open_vblank;
var
 time:Int64;
begin
 if (System.InterlockedIncrement64(callout_refs)=1) then
 begin
  time:=round(hz/59.94);
  callout_reset(@callout_vblank, time, @vblank_expire, nil);
 end;
end;

procedure close_vblank;
begin
 System.InterlockedDecrement64(callout_refs);
end;

type
 p_flip_control_args=^t_flip_control_args;
 t_flip_control_args=packed record
  id   :DWORD;
  align:DWORD;
  arg2 :QWORD;
  arg3 :QWORD;
  arg4 :QWORD;
  arg5 :QWORD;
  arg6 :QWORD;
 end;

 p_resolution_status=^t_resolution_status;
 t_resolution_status=packed record
  width           :DWORD;
  heigth          :DWORD;
  paneWidth       :DWORD;
  paneHeight      :DWORD;
  refreshHz       :DWORD; //Single
  screenSizeInInch:DWORD;
  padding:array[0..19] of Byte;
 end;

 //SCE_VIDEO_OUT_REFRESH_RATE_UNKNOWN  = 0,
 //SCE_VIDEO_OUT_REFRESH_RATE_23_98HZ  = 1,
 //SCE_VIDEO_OUT_REFRESH_RATE_50HZ     = 2,
 //SCE_VIDEO_OUT_REFRESH_RATE_59_94HZ  = 3,
 //SCE_VIDEO_OUT_REFRESH_RATE_29_97HZ  = 6,
 //SCE_VIDEO_OUT_REFRESH_RATE_89_91HZ  = 35, 0x23
 //SCE_VIDEO_OUT_REFRESH_RATE_119_88HZ = 13, 0xD

 //refreshRate =    0                                        SCE_VIDEO_OUT_REFRESH_RATE_UNKNOWN
 //refreshRate =    3; result.refreshHz = 0x426fc28f( 59.94) SCE_VIDEO_OUT_REFRESH_RATE_59_94HZ
 //refreshRate =    2, result.refreshHz = 0x42480000( 50.00) SCE_VIDEO_OUT_REFRESH_RATE_50HZ
 //refreshRate =    1, result.refreshHz = 0x41bfd70a( 23.98) SCE_VIDEO_OUT_REFRESH_RATE_23_98HZ
 //refreshRate =    4, result.refreshHz = 0x41c00000( 24.00)
 //refreshRate =    5, result.refreshHz = 0x41f00000( 30.00)
 //refreshRate =    6, result.refreshHz = 0x41efc28f( 29.97) SCE_VIDEO_OUT_REFRESH_RATE_29_97HZ
 //refreshRate =    7, result.refreshHz = 0x41c80000( 25.00)
 //refreshRate =    9, result.refreshHz = 0x42700000( 60.00)
 //refreshRate =   10, result.refreshHz = 0x42400000( 48.00)
 //refreshRate =  0xb, result.refreshHz = 0x423fcccd( 47.95)
 //refreshRate =  0xc, result.refreshHz = 0x42c80000(100.00)
 //refreshRate =  0xd, result.refreshHz = 0x42efc28f(119.88) SCE_VIDEO_OUT_REFRESH_RATE_119_88HZ
 //refreshRate =  0xe, result.refreshHz = 0x42f00000(120.00)
 //refreshRate =  0xf, result.refreshHz = 0x43480000(200.00)
 //refreshRate = 0x10, result.refreshHz = 0x436fc28f(239.76)
 //refreshRate = 0x11, result.refreshHz = 0x43700000(240.00)
 //refreshRate = 0x14, result.refreshHz = 0x413fd70a( 11.99)
 //refreshRate = 0x15, result.refreshHz = 0x41400000( 12.00)
 //refreshRate = 0x16, result.refreshHz = 0x416fd70a( 14.99)
 //refreshRate = 0x17, result.refreshHz = 0x41700000( 15.00)
 //refreshRate = 0x23, result.refreshHz = 0x42b3d1ec( 89.91) SCE_VIDEO_OUT_REFRESH_RATE_89_91HZ

type
 p_flip_status=^t_flip_status;
 t_flip_status=packed record
  flipArg        :QWORD;
  flipArg2       :QWORD;
  count          :QWORD;
  processTime    :QWORD;
  tsc            :QWORD;
  currentBuffer  :DWORD;
  flipPendingNum0:DWORD;
  gcQueueNum     :DWORD;
  flipPendingNum1:DWORD;
  submitTsc      :QWORD;
  f_0x40         :QWORD;
 end;

var
 flipArg:QWORD=0;

type
 p_cursor_enable=^t_cursor_enable;
 t_cursor_enable=packed record
  rtype  :DWORD; //0
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //3 = enable; 1 = disable
  enable :DWORD; //1 = enable; 0 = disable
  addr_hi:DWORD;
  addr_lo:DWORD;
  padding:array[0..13] of DWORD;
 end;

 p_cursor_img_addr=^t_cursor_img_addr;
 t_cursor_img_addr=packed record
  rtype  :DWORD; //1
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //2
  addr_hi:DWORD;
  addr_lo:DWORD;
  padding:array[0..14] of DWORD;
 end;

 p_cursor_pos=^t_cursor_pos;
 t_cursor_pos=packed record
  rtype  :DWORD; //2
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //2
  posX   :DWORD;
  posY   :DWORD;
  padding:array[0..14] of DWORD;
 end;

 p_cursor_pos_stereo=^t_cursor_pos_stereo;
 t_cursor_pos_stereo=packed record
  rtype  :DWORD; //3
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //3
  posX   :DWORD;
  posY   :DWORD;
  offset :DWORD;
  padding:array[0..13] of DWORD;
 end;

 p_cursor_hot_spot=^t_cursor_hot_spot;
 t_cursor_hot_spot=packed record
  rtype  :DWORD; //4
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //2
  hotX   :DWORD;
  hotY   :DWORD;
  padding:array[0..14] of DWORD;
 end;

 p_cursor_magnify=^t_cursor_magnify;
 t_cursor_magnify=packed record
  rtype  :DWORD; //5
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //1
  enable :DWORD;
  padding:array[0..15] of DWORD;
 end;

 p_cursor_update_pending=^t_cursor_update_pending;
 t_cursor_update_pending=packed record
  rtype  :DWORD; //6
  index  :DWORD; //Cursor index (0-1)
  args   :DWORD; //1
  ptype  :DWORD;
  padding:array[0..14] of DWORD;
  result :DWORD; //0,1
 end;

Function dce_set_cursor_info(dev:p_cdev;canary:QWORD;arg5:DWORD;data:Pointer):Integer;
var
 info:t_cursor_enable;
 addr:qword;
begin
 info:=Default(t_cursor_enable);
 Result:=copyin(data,@info,80);
 if (Result<>0) then Exit;

 case info.rtype of
  0: //enable/disable
    begin
     case info.enable of
      1:
        begin
         addr:=info.addr_lo or (QWORD(info.addr_hi) shl 32);

         Writeln('dce_set_cursor_enable:',canary,' ',
                                          info.index,' ',
                                          HexStr(addr,16));
        end;
      0:
        begin
         Writeln('dce_set_cursor_disable:',canary,' ',
                                           info.index);
        end;
     end;
    end;

  1: //SetImageAddress
    begin
     addr:=p_cursor_img_addr(@info)^.addr_lo or (QWORD(p_cursor_img_addr(@info)^.addr_hi) shl 32);

     Writeln('dce_set_image_addr:',canary,' ',
                                   p_cursor_img_addr(@info)^.index,' ',
                                   HexStr(addr,16));
    end;

  2: //SetPosition
    begin
     Writeln('dce_set_cursor_pos:',canary,' ',
                                   p_cursor_pos(@info)^.index,' ',
                                   p_cursor_pos(@info)^.posX,' ',
                                   p_cursor_pos(@info)^.posY);
    end;

  3:; //SetPositionStereo
  4:; //SetHotSpot
  5:; //Set2xMagnify

  6: //IsUpdatePending
    begin
     Writeln('dce_is_update_pending:',canary,' ',
                                      p_cursor_update_pending(@info)^.index,' ',
                                      p_cursor_update_pending(@info)^.ptype);

     addr:=1;

     Result:=copyout(@addr,@p_cursor_update_pending(data)^.result,8);
    end;

  else
   Exit(EINVAL);
 end;

end;

Function dce_flip_control(dev:p_cdev;data:p_flip_control_args):Integer;
var
 ptr :Pointer;
 poff:PQWORD;
 plen:PQWORD;
 len:QWORD;
 u:record
  case byte of
   0:(r_status:t_resolution_status);
   1:(f_status:t_flip_status);
 end;
begin
 Result:=0;

 //id -> 0..0x24

 case data^.id of
  0:  //video open
    begin
     if (data^.arg6=0) and (data^.arg2=0) then
     begin

      //arg3 [0..1]
      case Integer(data^.arg3) of
       0:;
       1:begin
          //sceSblACMgrIsSystemUcred
          Exit(EPERM);
         end;
       else
         begin
          Exit(EINVAL);
         end;
      end;

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        dce_handle:=dce_interface.Open;
       end else
       begin
        Result:=EBUSY;
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      Writeln('dce_video_open');

      ptr:=Pointer(data^.arg5);
      len:=$a5a5; //canary
      copyout(@len,ptr,SizeOf(QWORD));

      open_vblank;

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  1: //video close
    begin
     if (data^.arg3=0) and (data^.arg4=0) and
        (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        Result:=EINVAL;
       end else
       begin
        FreeAndNil(dce_handle);
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      Writeln('dce_video_close:',data^.arg2);

      close_vblank;

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  5: //UnregisterBufferAttribute
    begin
     if (data^.arg4=0) and (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary
      //arg3 -> fid

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      if (Integer(data^.arg3)>15) then Exit(EINVAL);

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        Result:=EINVAL;
       end else
       begin
        Result:=dce_handle.UnregisterBuffer(Integer(data^.arg3));
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      Writeln('UnregisterBufferAttribute:',data^.arg2,' ',data^.arg3);

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  6: //SetFlipRate
    begin
     if (data^.arg4=0) and (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary
      //arg3 -> rate

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        Result:=EINVAL;
       end else
       begin
        Result:=dce_handle.SetFlipRate(Integer(data^.arg3));
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      Writeln('SetFlipRate:',data^.arg2,' ',data^.arg3);

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  9:
    begin //get page info
     if (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      poff:=Pointer(data^.arg3); //output offset  //4000..3FFC000
      plen:=Pointer(data^.arg4); //output len     //4000

      len:=$4000;

      Result:=copyout(@len,plen,SizeOf(QWORD));

      if (Result=0) then
      begin
       Result:=copyout(@len,poff,SizeOf(QWORD));
      end;

      Exit;
     end;
     Exit(EINVAL);
    end;

  10: //sceVideoOutGetFlipStatus
    begin
     if (data^.arg6=0) and (Integer(data^.arg4)>0) then
     begin
      //arg2 -> canary
      //arg3 = &result;
      //arg4 = 72

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      ptr:=Pointer(data^.arg3);

      len:=Integer(data^.arg4);
      if (len>SizeOf(t_flip_status)) then
      begin
       len:=SizeOf(t_flip_status);
      end;

      u.f_status:=Default(t_flip_status);
      u.f_status.flipArg        :=flipArg;
      u.f_status.count          :=0;
      u.f_status.processTime    :=0;
      u.f_status.tsc            :=0;
      u.f_status.currentBuffer  :=0;
      u.f_status.flipPendingNum0:=0;
      u.f_status.gcQueueNum     :=0;
      u.f_status.flipPendingNum1:=0;
      u.f_status.submitTsc      :=0;

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        Result:=EINVAL;
       end else
       begin
        Result:=dce_handle.GetFlipStatus(@u.f_status);
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      Result:=copyout(@u.f_status,ptr,len);

      Exit;
     end;

     Exit(EINVAL);
    end;

  12: //sceVideoOutSysUpdateScalerParameters
    begin
     if (data^.arg5=0) and (data^.arg6=0) then
     begin
      if (data^.arg4=$30) or (data^.arg4=$40) then
      begin
       //arg2 -> canary
       //arg3 -> ptr
       //arg4 -> 64

       if (data^.arg2<>$a5a5) then Exit(EINVAL);

       ptr:=Pointer(data^.arg3);

       len:=0;

       Writeln('dce_flip_control(',data^.id,'):get_data?');
       print_backtrace_td(stderr);

       Result:=copyout(@len,ptr,8);

       Exit;
      end;
     end;
     Exit(EINVAL);
    end;

  19: //sceVideoOutGetResolutionStatus
    begin
     if (data^.arg4=44) then
     begin
      //arg2 -> canary
      //arg3 = &result;
      //arg4 = 44;

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      ptr:=Pointer(data^.arg3);

      len:=Integer(data^.arg4);
      if (len>SizeOf(t_resolution_status)) then
      begin
       len:=SizeOf(t_resolution_status);
      end;

      u.r_status:=Default(t_resolution_status);
      u.r_status.width           :=1920;
      u.r_status.heigth          :=1080;
      u.r_status.paneWidth       :=1920;
      u.r_status.paneHeight      :=1080;
      u.r_status.refreshHz       :=$426fc28f;
      u.r_status.screenSizeInInch:=32;

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        Result:=EINVAL;
       end else
       begin
        Result:=dce_handle.GetResolutionStatus(@u.r_status);
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      Result:=copyout(@u.r_status,ptr,len);

      Exit;
     end;
     Exit(EINVAL);
    end;

  24: //sceVideoOutCursor*
    begin
     if (data^.arg6=0) and (data^.arg4=80) then
     begin
      //arg2 -> canary
      //arg4 = 80;

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      Result:=dce_set_cursor_info(dev,data^.arg2,data^.arg5,Pointer(data^.arg3));
      Exit;
     end;
     Exit(EINVAL);
    end;

  31: //sys
    begin
     //arg2 -> canary
     //arg3 <- subtype 0..13

     if (data^.arg2<>$a5a5) then Exit(EINVAL);

     if (data^.arg3>13) then Exit(EINVAL);

     Writeln('dce_flip_control(',data^.id,'):',data^.arg3,' 0x',HexStr(data^.arg4,16));
    end;

  else
   begin
    Writeln('dce_flip_control(',data^.id,')');
    print_backtrace_td(stderr);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

type
 p_register_buffer_attr_args=^t_register_buffer_attr_args;
 t_register_buffer_attr_args=packed record
  canary     :QWORD; //arg5 data in dce_flip_control:0:pointer(arg5)^
  attrid     :Byte;  //attribute id [0..3]
  submit     :Byte;  //0 = RegisterBuffers ; 1 = SubmitChangeBufferAttribute
  f_0xa      :WORD;
  pixelFormat:DWORD;
  tilingMode :DWORD;
  pitchPixel :DWORD;
  width      :DWORD;
  height     :DWORD;
  f_0x20     :Byte;
  f_0x21     :Byte;
  options    :WORD;
  reserved1  :QWORD;
  reserved2  :DWORD;
 end;

Function dce_register_buffer_attr(dev:p_cdev;data:p_register_buffer_attr_args):Integer;
begin
 Result:=0;

 if (data^.canary<>$a5a5) then Exit(EINVAL);

 if (data^.attrid>3) then Exit(EINVAL);
 if (data^.submit>1) then Exit(EINVAL);

 mtx_lock(dce_mtx);

  if (dce_handle=nil) then
  begin
   Result:=EINVAL;
  end else
  begin
   case data^.submit of
     0:Result:=dce_handle.RegisterBufferAttribute(data^.attrid,@data^.pixelFormat);
     1:Result:=dce_handle.SubmitBufferAttribute  (data^.attrid,@data^.pixelFormat);
    else;
   end;
  end;

 mtx_unlock(dce_mtx);

 Writeln('register_buffer_attr:',data^.attrid,' ',
                                 data^.submit,' ',
                                 '0x',HexStr(data^.pixelFormat,8),' ',
                                 data^.tilingMode,' ',
                                 data^.pitchPixel,' ',
                                 data^.width,' ',
                                 data^.height,' ',
                                 '0x',HexStr(data^.options,4));

end;

type
 p_register_buffer_ptrs=^t_register_buffer_ptrs;
 t_register_buffer_ptrs=packed record
  canary:QWORD;   //arg5 data in dce_flip_control:0:pointer(arg5)^
  index :DWORD;   //buffer index [0..15]
  attrid:DWORD;   //attribute id [0..3]
  left  :Pointer; //buffer ptr
  right :Pointer; //Stereo ptr
  unk   :Integer;
 end;

Function dce_register_buffer_ptrs(dev:p_cdev;data:p_register_buffer_ptrs):Integer;
begin
 Result:=0;

 if (data^.canary<>$a5a5) then Exit(EINVAL);

 if (data^.index>15) then Exit(EINVAL);
 if (data^.attrid>3) then Exit(EINVAL);

 mtx_lock(dce_mtx);

  if (dce_handle=nil) then
  begin
   Result:=EINVAL;
  end else
  begin
   Result:=dce_handle.RegisterBuffer(@data^.index);
  end;

 mtx_unlock(dce_mtx);

 Writeln('register_buffer_ptrs:',data^.attrid,' ',
                                 data^.index,' ',
                                 '0x',HexStr(data^.left),' ',
                                 '0x',HexStr(data^.right));

end;


type
 p_submit_flip_args=^t_submit_flip_args;
 t_submit_flip_args=packed record
  canary     :QWORD; //arg5 data in dce_flip_control:0:pointer(arg5)^
  bufferIndex:QWORD;
  flipMode   :DWORD;
  f_0x14     :DWORD;
  flipArg    :QWORD;
  flipArg2   :QWORD;
  eop_nz     :DWORD;
  f_0x2c     :DWORD;
  eop_val    :PQWORD;
  f_0x38     :QWORD;
  rout       :PQWORD; //extraout of result
 end;

var
 f_eop_count:Integer=0;

Function dce_submit_flip(dev:p_cdev;data:p_submit_flip_args):Integer;
var
 submit:t_submit_flip;
 submit_eop:QWORD;
 ures:QWORD;
begin
 Result:=0;

 if (data^.canary<>$a5a5) then Exit(EINVAL);

 if (data^.bufferIndex>15) then Exit(EINVAL);

 submit.bufferIndex:=data^.bufferIndex;
 submit.flipMode   :=data^.flipMode;
 submit.flipArg    :=data^.flipArg;
 submit.flipArg2   :=data^.flipArg2;

 mtx_lock(dce_mtx);

  if (dce_handle=nil) then
  begin
   Result:=EINVAL;
  end else
  begin

   if (data^.eop_nz=1) then
   begin
    //       count               canary
    //eop=0x[00000001] [ff] [00] [a5a5]

    f_eop_count:=f_eop_count+1;

    submit_eop:=(QWORD(f_eop_count) shl 32) or QWORD($ff00a5a5);

    Result:=dce_handle.SubmitFlipEop(@submit,submit_eop);
   end else
   begin
    Result:=dce_handle.SubmitFlip(@submit);
   end;

  end;

 mtx_unlock(dce_mtx);

 Writeln('submit_flip:',data^.bufferIndex,' ',
                        data^.flipMode,' ',
                        '0x',HexStr(data^.flipArg,16),' ',
                        '0x',HexStr(data^.eop_val));

 knote_eventid(EVENTID_FLIP, data^.flipArg, 0); //SCE_VIDEO_OUT_EVENT_FLIP

 flipArg:=data^.flipArg;

 if (data^.eop_nz=1) then
 begin
  Result:=copyout(@submit_eop,data^.eop_val,SizeOf(QWORD));
 end;

 if (data^.rout<>nil) then
 begin
  ures:=Result;
  copyout(@ures,data^.rout,SizeOf(QWORD));
 end;

end;

Function dce_deregister_ident(dev:p_cdev;data:PQWORD):Integer;
begin
 writeln('dce_deregister_ident:',HexStr(data^,16));

 kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,data^);

 Result:=0;
end;

Function dce_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('dce_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $C0308203:Result:=dce_flip_control        (dev,data); //SCE_SYS_DCE_IOCTL_FLIP_CONTROL
  $C0308207:Result:=dce_register_buffer_attr(dev,data); //SCE_SYS_DCE_IOCTL_REGISTER_BUFFER_ATTRIBUTE
  $C0308206:Result:=dce_register_buffer_ptrs(dev,data);
  $C0488204:Result:=dce_submit_flip         (dev,data);
  $80088209:Result:=dce_deregister_ident    (dev,data);

  else
   begin
    print_backtrace_td(stderr);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

Function dce_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
var
 foff:vm_ooffset_t;
begin
 Result:=0;

 if (offset > $3ffffff) then
 begin
  Exit(EINVAL);
 end;

 if ((nprot and $ffffffcc)<>0) then
 begin
  Exit(EINVAL);
 end;

 foff:=offset; //?

 offset:=((DWORD(offset) mod PAGE_SIZE) + foff);

 offset:=(foff and $1fffff) { or (uVar1 and $fffffffe00000)};

 paddr^:=DWORD(offset) {+ };
end;

Function dce_mmap_single(cdev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;pobj:p_vm_object_t;nprot:Integer):Integer;
var
 off:vm_ooffset_t;
 obj:vm_object_t;
begin
 Result:=0;

 if (size<>PAGE_SIZE) then
 begin
  Exit(EINVAL);
 end;

 off:=offset^;
 if ((off and $fffffffffc003fff)<>0) then //4000..3FFC000
 begin
  Exit(EINVAL);
 end;

 if (off<>PAGE_SIZE) then
 begin
  Assert(false);
 end;

 if (nprot<>$33) then
 begin
  Exit(EACCES);
 end;

 obj:=vm_pager_allocate(OBJT_DEVICE,cdev,PAGE_SIZE,$33,off);
 obj^.un_pager.map_base:=dce_page;

 if (obj=nil) then
 begin
  Exit(EINVAL);
 end;

 pobj^:=obj;
end;

Function dce_kqfilter(dev:p_cdev;kn:p_knote):Integer;
begin
 if (kn^.kn_kevent.filter=EVFILT_DISPLAY) then
 begin
  knlist_add(@g_video_out_event_flip,kn,1);
 end;
 Result:=EINVAL;
end;

const
 dce_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'dce';
  d_open        :nil;
  d_fdopen      :nil;
  d_close       :nil;
  d_read        :nil;
  d_write       :nil;
  d_ioctl       :@dce_ioctl;
  d_poll        :nil;
  d_mmap        :@dce_mmap;
  d_strategy    :nil;
  d_dump        :nil;
  d_kqfilter    :@dce_kqfilter;
  d_purge       :nil;
  d_mmap_single :@dce_mmap_single;
  d_mmap_single2:nil;
 );

function filt_display_attach(kn:p_knote):Integer;
var
 event_id:WORD;
begin
 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR; { automatically set }

 if ((kn^.kn_kevent.ident and $f00000000000)=0) then
 begin
  //_display_attach(&kn->kn_kevent);
 end;

 kn^.kn_hook:=Pointer(p_proc.p_pid);

 event_id:=kn^.kn_kevent.ident shr 48;

 case event_id of
   EVENTID_FLIP,      //SCE_VIDEO_OUT_EVENT_FLIP
   EVENTID_VBLANK,    //SCE_VIDEO_OUT_EVENT_VBLANK
   EVENTID_SETMODE,   //8
   EVENTID_POSITION,  //12
   EVENTID_PREVBLANK, //SCE_VIDEO_OUT_EVENT_PRE_VBLANK_START
   $0060,
   $0061,
   $0062,
   $0063:
     begin
      knlist_add(@g_video_out_event_flip,kn,0);
     end;
  else
   begin
    //Writeln(stderr,'filt_display_attach:',event_id);
    //Assert(false,'filt_display_attach');
    Result:=EINVAL;
   end;
 end;

 Exit(0);
end;

procedure filt_display_detach(kn:p_knote);
var
 event_id:WORD;
begin
 event_id:=kn^.kn_kevent.ident shr 48;

 case event_id of
   EVENTID_FLIP,      //SCE_VIDEO_OUT_EVENT_FLIP
   EVENTID_VBLANK,    //SCE_VIDEO_OUT_EVENT_VBLANK
   EVENTID_SETMODE,   //8
   EVENTID_POSITION,  //12
   EVENTID_PREVBLANK, //SCE_VIDEO_OUT_EVENT_PRE_VBLANK_START
   $0060,
   $0061,
   $0062,
   $0063:
     begin
      knlist_remove(@g_video_out_event_flip,kn,0)
     end;
  else;
 end;

 if ((kn^.kn_kevent.ident and $f00000000000)=0) then
 begin
  //_display_detach(&kn->kn_kevent);
 end;

end;

function filt_display_event(kn:p_knote;hint:QWORD):Integer;
var
 i:Integer;
 event_id:WORD;
 time,mask:QWORD;
 hint_h :DWORD;
 ident_h:DWORD;
begin
 if (hint=0) then
 begin
  i:=(kn^.kn_kevent.data shr 12) and $F;
 end else
 begin
  event_id:=kn^.kn_kevent.ident shr 48;

  hint_h :=DWORD(hint shr 8) and $ffffff;
  ident_h:=DWORD(kn^.kn_kevent.ident shr 40);

  i:=0;
  if ((DWORD(hint) and $ff)=event_id) and
     (event_id<>$fe) and
     (((hint_h xor ident_h) and $ff)=0) then
  begin
   time:=rdtsc();
   mask:=$f000;
   if ((DWORD(kn^.kn_kevent.data) and $f000)<>$f000) then
   begin
    mask:=(DWORD(kn^.kn_kevent.data) + $1000) and $f000;
   end;
   i:=1;
   kn^.kn_kevent.data:=mask or QWORD(DWORD(time) and $fff) or (hint and $ffffffffffff0000);
  end;
 end;

 Exit(i);
end;

const
 filterops_display:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filt_display_attach;
  f_detach:@filt_display_detach;
  f_event :@filt_display_event;
 );

procedure dce_initialize();
begin
 mtx_init(knlist_lock_flip,'knlist_lock_flip');
 mtx_init(dce_mtx,'dce');

 knlist_init_mtx(@g_video_out_event_flip,@knlist_lock_flip);

 callout_init_mtx(@callout_vblank,knlist_lock_flip,0);

 kqueue_add_filteropts(EVFILT_DISPLAY,@filterops_display);

 dce_page:=dev_mem_alloc(2);

 make_dev(@dce_cdevsw,0,0,0,0666,'dce',[]);
end;


end.
