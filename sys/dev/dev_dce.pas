unit dev_dce;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf,
 sys_event;

procedure dce_initialize();

var
 g_video_out_event_flip  :t_knlist;
 g_video_out_event_vblank:t_knlist;

implementation

uses
 errno,
 systm,
 kern_thr,
 trap,
 vm,
 vm_object,
 vm_pager,
 kern_event,
 kern_mtx,
 md_time;

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

Function dce_flip_control(dev:p_cdev;data:p_flip_control_args):Integer;
var
 poff:PQWORD;
 plen:PQWORD;
 len:QWORD;
begin
 Result:=0;

 //id -> 0..0x24

 case data^.id of
  0:  //video open
    begin
     if ((data^.arg6=0) and (data^.arg2=0)) then
     begin

      plen:=Pointer(data^.arg5);
      len:=111; //canary
      copyout(@len,plen,SizeOf(QWORD));

      Exit(0);
     end;
     Exit(EINVAL);
    end;

  9:
    begin //get page info
     if ((data^.arg5=0) and (data^.arg6=0)) then
     begin
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

  12:
    begin
     if ((data^.arg5=0) and (data^.arg6=0)) then
     begin
      if (data^.arg4=$30) or (data^.arg4=$40) then
      begin
       Writeln('dce_flip_control(',data^.id,'):wait?');

       Exit(0);
      end;
     end;
     Exit(EINVAL);
    end;

  31: //set vaddr
    begin
     //data^.arg3 <- subtype 0..13

     if (data^.arg3>13) then Exit(EINVAL);

     Writeln('dce_flip_control(',data^.id,'):',data^.arg3,' ',HexStr(data^.arg4,16));
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
  arg5       :QWORD; //arg5 data in dce_flip_control:0:pointer(arg5)^
  vid        :Byte;  //video output port id ?
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

 Writeln('register_buffer_attr:',data^.vid,' ',
                                 data^.submit,' ',
                                 HexStr(data^.pixelFormat,8),' ',
                                 data^.tilingMode,' ',
                                 data^.pitchPixel,' ',
                                 data^.width,' ',
                                 data^.height,' ',
                                 HexStr(data^.options,4));

end;

Function dce_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('dce_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $C0308203:Result:=dce_flip_control        (dev,data); //SCE_SYS_DCE_IOCTL_FLIP_CONTROL
  $C0308207:Result:=dce_register_buffer_attr(dev,data); //SCE_SYS_DCE_IOCTL_REGISTER_BUFFER_ATTRIBUTE
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

 if ($3ffffff < offset) then
 begin
  Exit(EINVAL);
 end;

 if ((nprot and $ffffffcc)<>0) then
 begin
  Exit(EINVAL);
 end;

 foff:=offset; //?

 offset:=((DWORD(offset) mod $4000) + foff);

 offset:=(foff and $1fffff) { or (uVar1 and $fffffffe00000)};

 paddr^:=DWORD(offset);
end;

Function dce_mmap_single(cdev:p_cdev;offset:p_vm_ooffset_t;size:vm_size_t;pobj:p_vm_object_t;nprot:Integer):Integer;
var
 off:vm_ooffset_t;
 obj:vm_object_t;
begin
 Result:=0;

 if (size<>$4000) then
 begin
  Exit(EINVAL);
 end;

 off:=offset^;
 if ((off and $fffffffffc003fff)<>0) then //4000..3FFC000
 begin
  Exit(EINVAL);
 end;

 if (nprot<>$33) then
 begin
  Exit(EACCES);
 end;

 obj:=vm_pager_allocate(OBJT_DEVICE,cdev,$4000,$33,off);

 if (obj=nil) then
 begin
  Exit(EINVAL);
 end;

 pobj^:=obj;
end;

Function dce_kqfilter(dev:p_cdev;kn:p_knote):Integer;
begin
 Result:=0;

 Assert(false);
end;

const
 dce_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'dce';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :nil;
  d_write      :nil;
  d_ioctl      :@dce_ioctl;
  d_poll       :nil;
  d_mmap       :@dce_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :@dce_kqfilter;
  d_purge      :nil;
  d_mmap_single:@dce_mmap_single;
 );

function filt_display_attach(kn:p_knote):Integer;
var
 event_id:WORD;
begin
 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR;             { automatically set }

 if ((kn^.kn_kevent.ident and $f00000000000)=0) then
 begin
  //_display_attach(&kn->kn_kevent);
 end;

 //kn^.kn_hook = (long)p_pid;

 event_id:=kn^.kn_kevent.ident shr 48;

 case event_id of
   $0006: //SCE_VIDEO_OUT_EVENT_FLIP
     begin
      knlist_add(@g_video_out_event_flip,kn,0)
     end;
   $0007: //SCE_VIDEO_OUT_EVENT_VBLANK
     begin
      knlist_add(@g_video_out_event_vblank,kn,0)
     end;
   //$0051:Result:=8;
   //$0058:Result:=12;
  else
   begin
    Writeln(stderr,'filt_display_attach:',event_id);
    Assert(false,'filt_display_attach');
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
   $0006: //SCE_VIDEO_OUT_EVENT_FLIP
     begin
      knlist_remove(@g_video_out_event_flip,kn,0)
     end;
   $0007: //SCE_VIDEO_OUT_EVENT_VBLANK
     begin
      knlist_remove(@g_video_out_event_vblank,kn,0)
     end;
   //$0051:Result:=8;
   //$0058:Result:=12;
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
begin
 if (hint=0) then
 begin
  i:=(kn^.kn_kevent.data shr 12) and $F;
 end else
 begin
  event_id:=kn^.kn_kevent.ident shr 48;

  i:=0;
  if ((DWORD(hint) and $ff)=event_id) and
     (event_id<>$fe) and
     ((((DWORD(hint shr 8) and $ffffff) xor DWORD(kn^.kn_kevent.ident shr 40)) and $ff)=0) then
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

var
 knlist_lock_flip  :mtx;
 knlist_lock_vblank:mtx;

procedure dce_initialize();
begin
 mtx_init(knlist_lock_flip  ,'knlist_lock_flip');
 mtx_init(knlist_lock_vblank,'knlist_lock_vblank');

 knlist_init_mtx(@g_video_out_event_flip  ,@knlist_lock_flip);
 knlist_init_mtx(@g_video_out_event_vblank,@knlist_lock_vblank);

 kqueue_add_filteropts(EVFILT_DISPLAY,@filterops_display);

 make_dev(@dce_cdevsw,0,0,0,0666,'dce',[]);
end;


end.

