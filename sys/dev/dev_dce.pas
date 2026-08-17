unit dev_dce;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 bittype,
 vm,
 vmparam,
 sys_conf,
 sys_event,
 time,
 kern_mtx,
 sys_bootparam,
 display_interface;

procedure dce_initialize();

function  TriggerFlipEop(submit_id:QWORD):Integer;

type
 p_dce_page=^t_dce_page;
 t_dce_page=packed record //0x170
  zero         :array[0..19] of QWORD;
  labels       :array[0..15] of QWORD; //sceVideoOutGetBufferLabelAddress
  label_       :QWORD;
  unk2         :QWORD;
  ident_flip   :QWORD;
  ident_vblank :QWORD;
  filter_id    :Integer;
  _align       :Integer;
  vopen_counter:QWORD;
  ident_setmode:QWORD;
  unk3         :array[0..23] of Byte;
 end;

 {$IF sizeof(t_dce_page)<>$170}{$STOP sizeof(t_dce_page)<>$170}{$ENDIF}

var
 dce_interface:TAbstractDisplay=TDisplayHandle;

 dce_handle:TDisplayHandle;

 dce_mtx:mtx;

 dce_page:p_dce_page;

 knlist_lock_flip:mtx;

 g_video_out_event_flip:t_knlist;

implementation

uses
 errno,
 systm,
 md_time,
 vm_pmap,
 subr_backtrace,
 vm_object,
 vm_pager,
 kern_proc,
 kern_timeout;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

const
 EVENTID_FLIP     =$0006;
 EVENTID_VBLANK   =$0007;
 EVENTID_SETMODE  =$0051;
 EVENTID_POSITION =$0058;
 EVENTID_PREVBLANK=$0059;

 //$0061 sys_update_scaler
 //$0062 sys_prevblank wZI4fmUJMlw / vXMpe7Murfc
 //$0063 sys_vblank

procedure knote_eventid(event_id:WORD;flipArg:QWORD;lockflags:Integer);
begin
 knote(@g_video_out_event_flip, event_id or (flipArg shl 16), lockflags);
end;

var
 vblank:record
  lock   :mtx;
  callout:t_callout;
  refs   :Int64;

  count  :QWORD;
  ptime  :QWORD;
  tsc    :QWORD;

  fps_cnt:QWORD;
  fps_tsc:QWORD;
 end;

procedure vblank_expire(arg:Pointer);
var
 i:QWORD;
 trigger:Boolean;
begin
 if (vblank.refs<>0) then
 begin

  knote_eventid(EVENTID_PREVBLANK, vblank.count, 0); //SCE_VIDEO_OUT_EVENT_PRE_VBLANK_START

  //mtx_lock(dce_mtx);

   trigger:=False;
   if (dce_handle<>nil) then
   begin
    trigger:=dce_handle.Vblank();
   end;
   trigger:=True;

   if trigger then
   begin
    vblank.ptime:=GetProcessTime;
    vblank.tsc  :=guest_rdtsc();

    i:=System.InterlockedIncrement64(vblank.count)-1;
   end;

  //mtx_unlock(dce_mtx);

  //
  callout_reset(@vblank.callout, vblank.callout.c_time, @vblank_expire, nil);
  //

  if trigger then
  begin
   knote_eventid(EVENTID_VBLANK   , i, 0); //SCE_VIDEO_OUT_EVENT_VBLANK
  end;

  if (vblank.fps_tsc=0) then
  begin
   vblank.fps_tsc:=guest_rdtsc();
   vblank.fps_cnt:=0;
  end else
  begin
   //Inc(fps_cnt);
   if ((guest_rdtsc()-vblank.fps_tsc) div guest_tsc_freq)>=1 then
   begin
    p_host_ipc.SetCaptionFPS(vblank.fps_cnt);
    vblank.fps_cnt:=0;
    vblank.fps_tsc:=guest_rdtsc();
   end;
  end;

 end;
end;

procedure open_vblank;
var
 time:Int64;
begin
 if (System.InterlockedIncrement64(vblank.refs)=1) then
 begin
  time:=round(hz/59.94);
  callout_reset(@vblank.callout, time, @vblank_expire, nil);
 end;
end;

procedure close_vblank;
begin
 System.InterlockedDecrement64(vblank.refs);
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
  screenSizeInInch:DWORD; //Single
  padding:array[0..19] of Byte;
 end;

 t_scaler_info=array[0..63] of Byte;

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

 p_vblank_args=^t_vblank_args;
 t_vblank_args=packed record
  count      :QWORD;
  processTime:QWORD;
  tsc        :QWORD;
  flags      :Byte;
  _align     :array[0..6] of Byte;
  reserved   :QWORD;
 end;

 pSceVideoOutColorSettings=^SceVideoOutColorSettings;
 SceVideoOutColorSettings=packed record
  gamma :array[0..2] of Single;
  option:DWORD;
 end;

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

         LOG_INFO('dce_set_cursor_enable:',canary,' ',
                                          info.index,' ',
                                          HexStr(addr,16));
        end;
      0:
        begin
         LOG_INFO('dce_set_cursor_disable:',canary,' ',
                                           info.index);
        end;
     end;
    end;

  1: //SetImageAddress
    begin
     addr:=p_cursor_img_addr(@info)^.addr_lo or (QWORD(p_cursor_img_addr(@info)^.addr_hi) shl 32);

     LOG_INFO('dce_set_image_addr:',canary,' ',
                                   p_cursor_img_addr(@info)^.index,' ',
                                   HexStr(addr,16));
    end;

  2: //SetPosition
    begin
     LOG_INFO('dce_set_cursor_pos:',canary,' ',
                                   p_cursor_pos(@info)^.index,' ',
                                   p_cursor_pos(@info)^.posX,' ',
                                   p_cursor_pos(@info)^.posY);
    end;

  3:; //SetPositionStereo
  4:; //SetHotSpot
  5:; //Set2xMagnify

  6: //IsUpdatePending
    begin
     LOG_INFO('dce_is_update_pending:',canary,' ',
                                      p_cursor_update_pending(@info)^.index,' ',
                                      p_cursor_update_pending(@info)^.ptype);

     addr:=1;

     Result:=copyout(@addr,@p_cursor_update_pending(data)^.result,8);
    end;

  else
   Exit(EINVAL);
 end;

end;

procedure FreeAndNilDce(var obj:TDisplayHandle);
var
 tmp:TDisplayHandle;
begin
 tmp:=obj;
 obj:=nil;
 mtx_unlock(dce_mtx);
  tmp.free;
 mtx_lock(dce_mtx);
end;

var
 f_vopen_counter:QWORD=0;
 f_eop_count:Integer=1;

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
   2:(i_scaler:t_scaler_info);
   3:(v_vblank:t_vblank_args);
   4:(color   :SceVideoOutColorSettings)
 end;
begin
 Result:=0;

 LOG_TRACE('dce_flip_control(',data^.id,')');

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
        dce_handle:=dce_interface.Create;

        if (dce_handle=nil) then
        begin
         Result:=EBUSY;
        end else
        begin
         dce_handle.event_flip:=@g_video_out_event_flip;
         dce_handle.labels    :=@dce_page^.labels;
         dce_handle.dce_mtx   :=@dce_mtx;
         dce_handle.p_fps_cnt :=@vblank.fps_cnt;
         Result:=dce_handle.Open();
        end;

       end else
       begin
        Result:=EBUSY;
       end;

       if (Result=0) then
       begin
        f_vopen_counter:=f_vopen_counter+1;

        FillChar(dce_page^,SizeOf(t_dce_page),0);

        dce_page^.filter_id:=-13;

        dce_page^.ident_flip   :=QWORD($06000000000000);
        dce_page^.ident_vblank :=QWORD($07000000000000);
        dce_page^.ident_setmode:=QWORD($51000000000000);
        dce_page^.vopen_counter:=f_vopen_counter;

        f_eop_count:=1;
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      LOG_INFO('dce_video_open');

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
        FreeAndNilDce(dce_handle);
       end;

       if (Result=0) then
       begin
        FillChar(dce_page^,SizeOf(t_dce_page),0);
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      LOG_INFO('dce_video_close:',data^.arg2);

      close_vblank;

      kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,QWORD($06000000000000));
      kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,QWORD($07000000000000));
      kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,QWORD($51000000000000));
      kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,QWORD($58000000000000));
      kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,QWORD($59000000000000));

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  4: //UnregisterBuffer
    begin
     if (data^.arg4=0) and (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary
      //arg3 -> buffer id

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      if (DWORD(data^.arg3)>15) then Exit(EINVAL);

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

      LOG_INFO('UnregisterBuffer:',data^.arg2,' ',data^.arg3);

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  5: //UnregisterBufferAttribute
    begin
     if (data^.arg4=0) and (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary
      //arg3 -> attr id

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      if (DWORD(data^.arg3)>3) then Exit(EINVAL);

      mtx_lock(dce_mtx);

       if (dce_handle=nil) then
       begin
        Result:=EINVAL;
       end else
       begin
        Result:=dce_handle.UnregisterBufferAttribute(Integer(data^.arg3));
       end;

      mtx_unlock(dce_mtx);

      if (Result<>0) then Exit;

      LOG_INFO('UnregisterBufferAttribute:',data^.arg2,' ',data^.arg3);

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  6: //SetFlipRate
    begin
     if (data^.arg4=0) and (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary
      //arg3 -> rate [0..7]

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      if (DWORD(data^.arg3)>7) then Exit(EINVAL);

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

      LOG_INFO('SetFlipRate:',data^.arg3);

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  7: //sceVideoOutSetWindowModeMargins
    begin
     if (data^.arg5=0) and (data^.arg6=0) then
     begin
      //arg2 -> canary
      //arg3 -> top
      //arg4 -> bottom

      if (DWORD(data^.arg3) > $fff) or (DWORD(data^.arg4) > $fff) then
      begin
       Exit(EINVAL);
      end;

      LOG_INFO('sceVideoOutSetWindowModeMargins:',DWORD(data^.arg3),' ',DWORD(data^.arg4));

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

      poff:=Pointer(data^.arg3); //output offset  //0..3FFC000
      plen:=Pointer(data^.arg4); //output len     //4000

      len:=$4000;
      Result:=copyout(@len,plen,SizeOf(QWORD));

      if (Result=0) then
      begin
       len:=0;
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

      len:=DWORD(data^.arg4);
      if (len>SizeOf(t_flip_status)) then
      begin
       len:=SizeOf(t_flip_status);
      end;

      u.f_status:=Default(t_flip_status);

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

  11: //sceVideoOutGetVblankStatus
    begin
     if (data^.arg5=0) and (data^.arg6=0) and (Integer(data^.arg4)>0) then
     begin
      //arg2 -> canary
      //arg3 = &result;
      //arg4 = 40

      LOG_TRACE('sceVideoOutGetVblankStatus');

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      ptr:=Pointer(data^.arg3);

      len:=DWORD(data^.arg4);
      if (len>SizeOf(t_vblank_args)) then
      begin
       len:=SizeOf(t_vblank_args);
      end;

      u.v_vblank:=Default(t_vblank_args);

      mtx_lock(vblank.lock);

       u.v_vblank.count      :=vblank.count;
       u.v_vblank.processTime:=vblank.ptime;
       u.v_vblank.tsc        :=vblank.tsc;

      mtx_unlock(vblank.lock);

      Result:=copyout(@u.v_vblank,ptr,len);

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
       len:=QWORD  (data^.arg4);

       u.i_scaler:=Default(t_scaler_info);

       LOG_TRACE('dce_flip_control(',data^.id,'):get_data?');
       //print_backtrace_td(stderr);

       Result:=copyout(@u.i_scaler,ptr,len);

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
      len:=QWORD  (data^.arg4);

      u.r_status:=Default(t_resolution_status);

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

  25: //sceVideoOutGetPortStatusInfo_
    begin
     if ((data^.arg4 or $10) = $30) then
     begin
      //arg2 -> canary
      //arg3 -> ptr
      //arg4 -> size

      if (data^.arg2<>$a5a5) then Exit(EINVAL);

      Exit(0);
     end;

     Exit(EINVAL);
    end;

  31: //sys
    begin
     //arg2 -> canary
     //arg3 <- subtype 0..13

     if (data^.arg2<>$a5a5) then Exit(EINVAL);

     if (data^.arg3>13) then Exit(EINVAL);

     case data^.arg3 of
      12:LOG_TRACE('SetLabelsAddr:',' 0x',HexStr(data^.arg4,10));
      else
         LOG_TRACE('dce_flip_control(',data^.id,'):',data^.arg3,' 0x',HexStr(data^.arg4,10));
     end;

    end;

  33: //sceVideoOutAdjustColor_
    begin
     //arg2 -> canary
     //arg3 -> pSetting;
     //arg4 -> size;

     if (data^.arg2<>$a5a5) then Exit(EINVAL);

     ptr:=Pointer(data^.arg3);
     len:=data^.arg4;

     if (len<>16) and (len<>12) then Exit(EINVAL);

     u.color:=Default(SceVideoOutColorSettings);
     if (len<>12) then
     begin
      u.color.option:=$ffffffff;
     end;

     Result:=copyin(ptr,@u.color,len);

     if (Result<>0) then Exit;

     LOG_INFO('sceVideoOutAdjustColor(',u.color.gamma[0]:0:2,',',
                                       u.color.gamma[1]:0:2,',',
                                       u.color.gamma[2]:0:2,',0x',HexStr(u.color.option,8)+')');

     Exit(0);
    end;

  else
   begin
    print_error_td('dce_flip_control('+IntToStr(data^.id)+')');
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

 t_pixelFormat=bitpacked record
  unknow_1           :bit8; //[0..7]   (always zero)
  GRPH_BLUE_CROSSBAR :bit2; //[8..9]   [BARG]
  GRPH_GREEN_CROSSBAR:bit2; //[10..11] [GBAR]
  GRPH_RED_CROSSBAR  :bit2; //[12..13] [RGBA]
  GRPH_ALPHA_CROSSBAR:bit2; //[14..15] [ARGB]
  COLORSPACE         :bit3; //[16..18] [SRGB=0,2,HDR=4,5,NORM=6]
  unknow_3           :bit1; //[19..19] (always zero)
  unknow_4           :bit4; //[20..23] [0,2,3,4,5,7,8]
  IS_FLOAT           :bit1; //[24..24] [0,1]
  unknow_5           :bit2; //[25..26] (always zero)
  GRPH_FORMAT        :bit3; //[27..29] [0,1,4,6,7]
  GRPH_DEPTH         :bit2; //[30..31] [16BPP=1,32BPP=2,3]
 end;

function getPixelFormatStr(pixelFormat:DWORD):RawBytestring;
begin
 case pixelFormat of
  SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB        :Result:='A8R8G8B8_SRGB';
  SCE_VIDEO_OUT_PIXEL_FORMAT_A16R16G16B16_FLOAT   :Result:='A16R16G16B16_FLOAT';
  SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB        :Result:='A8B8G8R8_SRGB';
  SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10          :Result:='A2R10G10B10';
  SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_SRGB     :Result:='A2R10G10B10_SRGB';
  SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_BT2020_PQ:Result:='A2R10G10B10_BT2020';
  else
   Result:=HexStr(pixelFormat,8);
 end;
end;

Function dce_register_buffer_attr(dev:p_cdev;data:p_register_buffer_attr_args):Integer;
var
 pixelFormat:DWORD;
 tilingMode:DWORD;
 options:WORD;
 GRPH_DEPT:Byte;
 big_buf:Boolean;

 align_size  :DWORD;
 align_height:DWORD;
 align_pitch :DWORD;
 kmem_size   :DWORD;

begin
 Result:=0;

 if (data^.canary<>$a5a5) then Exit(EINVAL);

 if (data^.attrid>3) then Exit(EINVAL);

 if (data^.reserved2<>0) or
    (data^.f_0x21<>0) or
    (data^.pitchPixel<data^.width) or
    (data^.f_0x20>1) then Exit(EINVAL);

 if (data^.pitchPixel>$2000) or
    (data^.width>$2000) or
    (data^.height>$2000) then Exit(EINVAL);

 //
 options:=data^.options;

 if ((options and 7)<>7) then
 begin
  options:=options;
 end else
 begin
  options:=options and $fff8;
 end;

 if ((options and 8)=0) then
 begin
  options:=options;
 end else
 begin
  options:=options and $ffe7;
 end;

 if ((options and $ffef)<>0) then Exit(EINVAL);
 //

 pixelFormat:=data^.pixelFormat;

 LOG_INFO('pixelFormat=',getPixelFormatStr(pixelFormat));

 case pixelFormat of
  $80000000:; //SCE_VIDEO_OUT_PIXEL_FORMAT_A8R8G8B8_SRGB
  $80002200:; //SCE_VIDEO_OUT_PIXEL_FORMAT_A8B8G8R8_SRGB
  $80060000:;
  $80062200:;
  $80220000:;
  $80320000:;
  $80420000:;
  $80520000:;
  $88000000:; //SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_SRGB
  $88060000:; //SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10
  $88720000:;
  $88740000:; //SCE_VIDEO_OUT_PIXEL_FORMAT_A2R10G10B10_BT2020_PQ
  $88750000:;
  $88820000:;
  $88840000:;
  $88850000:;
  $b0000000:;
  $b0060000:;
  $b0720000:;
  $b0740000:;
  $b0750000:;
  $b0820000:;
  $b0840000:;
  $b0850000:;
  $c1060000:; //SCE_VIDEO_OUT_PIXEL_FORMAT_A16R16G16B16_FLOAT
  $c1760000:;
  else
   Exit(EINVAL);
 end;

 GRPH_DEPT:=pixelFormat shr 30;

 big_buf:=(1080 < data^.height) or (1920 < data^.width);

 if (GRPH_DEPT=3) and (big_buf) then Exit(EINVAL);

 tilingMode:=data^.tilingMode;

 if (1 < tilingMode) then
 begin
   if (tilingMode<>2) or
      {(ReadFlagsInfo()=0) or}
      (GRPH_DEPT=3) then Exit(EINVAL);
 end;

 if (p_proc.p_sdk_version < $6500000) then
 begin
  align_size:=1;
  kmem_size:=(ord(GRPH_DEPT=3)*4+4)*data^.pitchPixel*data^.height;
 end else
 begin
  if (tilingMode=0) then
  begin
   align_height:=1;
   align_pitch :=$40;
   align_size  :=$100;
  end else
  if (tilingMode=1) then
  begin
   align_height:=$40;
   align_pitch :=$80;
   align_size  :=ord(GRPH_DEPT=3)*$8000+$8000;
  end else
  begin
   if ((tilingMode<>2) or (GRPH_DEPT=3)) then Exit(EINVAL);
   align_pitch :=$80;
   align_size  :=$10000;
   align_height:=$80;
  end;

  kmem_size:=
        (ord(GRPH_DEPT=3)*4+4)*
        Align(data^.height    ,align_height)*
        Align(data^.pitchPixel,align_pitch);

  kmem_size:=Align(kmem_size,align_size);
 end;

 LOG_TRACE('kmem_size=0x',HexStr(kmem_size,8));

 mtx_lock(dce_mtx);

  if (dce_handle=nil) then
  begin
   Result:=EINVAL;
  end else
  begin
   if (data^.submit=0) then
   begin
    Result:=dce_handle.RegisterBufferAttribute(data^.attrid,@data^.pixelFormat,kmem_size);
   end else
   begin
    Result:=dce_handle.SubmitBufferAttribute  (data^.attrid,@data^.pixelFormat,kmem_size);
   end;
  end;

 mtx_unlock(dce_mtx);

 LOG_INFO('register_buffer_attr:',data^.attrid,' ',
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
  stereo:Integer;
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

 LOG_INFO('register_buffer_ptrs:',data^.attrid,' ',
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

function TriggerFlipEop(submit_id:QWORD):Integer;
begin
 if (dce_handle=nil) then
 begin
  Result:=EINVAL;
 end else
 begin
  Result:=dce_handle.TriggerFlipEop(submit_id);
 end;
end;

Function dce_submit_flip(dev:p_cdev;data:p_submit_flip_args):Integer;
var
 submit:t_submit_flip;
 submit_eop:QWORD;
 ures:QWORD;
begin
 Result:=0;

 if (data^.canary<>$a5a5) then Exit(EINVAL);

 if (Integer(data^.bufferIndex)>15) or
    (Integer(data^.bufferIndex)<-1) then Exit(EINVAL);

 if (data^.flipMode>6) then Exit(EINVAL);

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

    submit_eop:=(QWORD(f_eop_count) shl 32) or QWORD($ff00a5a5);

    LOG_TRACE('submit_eop=0x',HexStr(submit_eop,16));

    f_eop_count:=f_eop_count+1;

    Result:=dce_handle.SubmitFlipEop(@submit,submit_eop);

    if (Result<>0) then
    begin
     LOG_INFO('SubmitFlipEopError=',Result);
    end;
   end else
   begin
    Result:=dce_handle.SubmitFlip(@submit);
    if (Result<>0) then
    begin
     LOG_INFO('SubmitFlipError=',Result);
    end;
   end;

  end;

 mtx_unlock(dce_mtx);

 //print_backtrace_td(stderr);

 LOG_TRACE('submit_flip: ','bufferIndex=',data^.bufferIndex,' ',
                              'flipMode=',data^.flipMode,' ',
                               'flipArg=','0x',HexStr(data^.flipArg,16),' ',
                               'eop_val=','0x',HexStr(data^.eop_val));
 if (Result=0) then
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
 LOG_TRACE('dce_deregister_ident:',HexStr(data^,16));

 kqueue_deregister(EVFILT_DISPLAY,p_proc.p_pid,data^);

 Result:=0;
end;

Function dce_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 LOG_TRACE('dce_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $C0308203:Result:=dce_flip_control        (dev,data); //SCE_SYS_DCE_IOCTL_FLIP_CONTROL
  $C0308207:Result:=dce_register_buffer_attr(dev,data); //SCE_SYS_DCE_IOCTL_REGISTER_BUFFER_ATTRIBUTE
  $C0308206:Result:=dce_register_buffer_ptrs(dev,data);
  $C0488204:Result:=dce_submit_flip         (dev,data);
  $80088209:Result:=dce_deregister_ident    (dev,data);

  else
   begin
    print_error_td('dce_ioctl(0x'+HexStr(cmd,8)+')');
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

 paddr^:=DWORD(offset) + QWORD(dce_page);
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
 if ((off and QWORD(not $3FFC000))<>0) then //0..3FFC000
 begin
  Exit(EINVAL);
 end;

 if (off<>0) then
 begin
  //only one page
  Exit(EACCES);
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
  d_ioctl       :@dce_ioctl;
  d_mmap        :@dce_mmap;
  d_kqfilter    :@dce_kqfilter;
  d_mmap_single :@dce_mmap_single;
 );

function filt_display_attach(kn:p_knote):Integer;
var
 event_id:WORD;
begin
 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR; { automatically set }

 if ((kn^.kn_kevent.ident and QWORD($f00000000000))=0) then
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
    LOG_ERROR(stderr,'filt_display_attach:',event_id);
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

 if ((kn^.kn_kevent.ident and QWORD($f00000000000))=0) then
 begin
  //_display_detach(&kn->kn_kevent);
 end;

end;

type
 t_dce_hint=bitpacked record
  event_id:bit8;
  video_id:bit8;
  flip_arg:bit48;
 end;

 t_dce_data=bitpacked record
  time    :bit12;
  counter :bit4;
  flip_arg:bit48;
 end;

function filt_display_event(kn:p_knote;hint:QWORD):Integer;
var
 event_id:WORD;
 video_id:BYTE;
 counter :BYTE;
 time    :QWORD;
begin
 if (hint=0) then
 begin
  Result:=t_dce_data(kn^.kn_kevent.data).counter;
 end else
 begin
  event_id:=kn^.kn_kevent.ident shr 48;
  video_id:=kn^.kn_kevent.ident shr 40;

  if (t_dce_hint(hint).event_id=event_id) and
     (event_id<>$fe) and
     ((t_dce_hint(hint).video_id xor video_id)=0) then
  begin
   time:=guest_rdtsc();
   counter:=t_dce_data(kn^.kn_kevent.data).counter;
   if (counter<>$f) then
   begin
    counter:=counter+1;
   end;
   kn^.kn_kevent.data:=(time and $fff) or                    //time
                       (counter shl 12) or                   //counter
                       (hint and QWORD($ffffffffffff0000));  //flip_arg
   Result:=1;
  end else
  begin
   Result:=0;
  end;
 end;
end;

const
 filterops_display:t_filterops=(
  f_isfd  :0;
  f_attach:@filt_display_attach;
  f_detach:@filt_display_detach;
  f_event :@filt_display_event;
 );

procedure dce_initialize();
begin
 mtx_init(knlist_lock_flip,'knlist_lock_flip');
 mtx_init(dce_mtx,'dce');

 knlist_init_mtx(@g_video_out_event_flip,@knlist_lock_flip);

 mtx_init(vblank.lock,'vblank.lock');
 callout_init_mtx(@vblank.callout,vblank.lock,0);

 kqueue_add_filteropts(EVFILT_DISPLAY,@filterops_display);

 dce_page:=dev_mem_alloc(1);

 make_dev(@dce_cdevsw,0,0,0,0666,'dce',[]);
end;


end.

