unit SDL3_audio_interface;

{$mode ObjFPC}{$H+}

interface

uses
 SDL3,
 SDL3_audio,
 audioout_interface;

type
 TAudioOutSDL3=class(TAudioOutHandle)
  //
  Device:TSDL_AudioDeviceID;
  Stream:PSDL_AudioStream;
  //
  convert:procedure(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
  //
  svolume:array[0..7] of Word;
  fvolume:array[0..7] of Single;
  //
  f_period   :QWORD;   //microseconds
  f_last_time:QWORD;   //microseconds
  f_next_time:QWORD;   //microseconds
  f_gbuf_size:DWORD;   //guest size
  f_ibuf_size:DWORD;   //internal size
  f_ibuf_ptr :Pointer; //internal buffer
  //
  Destructor Destroy; override;
  Function   Open(const device_id:RawByteString):Boolean; override;
  procedure  SetVolume(channel,vol:Integer);              override;
  procedure  SetMixLevelPadSpk(mixLevel:Integer);         override;
  function   GetLastOutputTime:QWORD;                     override;
  function   Output(ptr:Pointer):Integer;                 override;
  //
  class procedure Outputs(param:pAudioOutParam;num:DWORD); override;
 end;

Function Init_SDL3_interface():TAbstractAudioOut;

implementation

uses
 md_time,
 time,
 md_sleep,
 kern_thr,
 md_thread;

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

var
 SDL3Audio:TSDL3Audio=nil;

Function Init_SDL3_interface():TAbstractAudioOut;
begin
 SDL3Audio:=SDL_InitAudio();

 if (SDL3Audio=nil) then
 begin
  Exit(TAudioOutNull);
 end;

 Exit(TAudioOutSDL3);
end;

procedure usleep(usec:QWORD); inline; //microseconds
begin
 if (usec<>0)then
  msleep_td(USEC_TO_UNIT(usec));
end;

const
 VOLUME_0DB=(1 shl 15);

const
 _FL=0;
 _FR=1;
 _FC=2;
 _LF=3;
 _SL=4;
 _SR=5;
 _BL=6;
 _BR=7;

 STD_SL=6;
 STD_SR=7;
 STD_BL=4;
 STD_BR=5;

//S16

procedure VecMulS16M(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin
  PSingle(Dst)[0]:=(PSmallint(Src)[0]/VOLUME_0DB)*fvolume[0];
  Inc(Src,SizeOf(Smallint)*1);
  Inc(Dst,SizeOf(Single  )*1);
  Dec(count);
 end;
end;

procedure VecMulS16S(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin
  PSingle(Dst)[0]:=(PSmallint(Src)[0]/VOLUME_0DB)*fvolume[0];
  PSingle(Dst)[1]:=(PSmallint(Src)[1]/VOLUME_0DB)*fvolume[1];
  Inc(Src,SizeOf(Smallint)*2);
  Inc(Dst,SizeOf(Single  )*2);
  Dec(count);
 end;
end;

procedure VecMulS16CH8(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin

  PSingle(Dst)[_FL]:=(PSmallint(Src)[_FL]/VOLUME_0DB)*fvolume[_FL];
  PSingle(Dst)[_FR]:=(PSmallint(Src)[_FR]/VOLUME_0DB)*fvolume[_FR];
  PSingle(Dst)[_FC]:=(PSmallint(Src)[_FC]/VOLUME_0DB)*fvolume[_FC];
  PSingle(Dst)[_LF]:=(PSmallint(Src)[_LF]/VOLUME_0DB)*fvolume[_LF];
  PSingle(Dst)[_SL]:=(PSmallint(Src)[_SL]/VOLUME_0DB)*fvolume[_SL];
  PSingle(Dst)[_SR]:=(PSmallint(Src)[_SR]/VOLUME_0DB)*fvolume[_SR];
  PSingle(Dst)[_BL]:=(PSmallint(Src)[_BL]/VOLUME_0DB)*fvolume[_BL];
  PSingle(Dst)[_BR]:=(PSmallint(Src)[_BR]/VOLUME_0DB)*fvolume[_BR];

  Inc(Src,SizeOf(Smallint)*8);
  Inc(Dst,SizeOf(Single  )*8);

  Dec(count);
 end;
end;

//F32

procedure VecMulF32M(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin
  PSingle(Dst)[0]:=PSingle(Src)[0]*fvolume[0];
  Inc(Src,SizeOf(Single)*1);
  Inc(Dst,SizeOf(Single)*1);
  Dec(count);
 end;
end;

procedure VecMulF32S(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin
  PSingle(Dst)[0]:=PSingle(Src)[0]*fvolume[0];
  PSingle(Dst)[1]:=PSingle(Src)[1]*fvolume[1];
  Inc(Src,SizeOf(Single)*2);
  Inc(Dst,SizeOf(Single)*2);
  Dec(count);
 end;
end;

procedure VecMulF32CH8(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin

  PSingle(Dst)[_FL]:=PSingle(Src)[_FL]*fvolume[_FL];
  PSingle(Dst)[_FR]:=PSingle(Src)[_FR]*fvolume[_FR];
  PSingle(Dst)[_FC]:=PSingle(Src)[_FC]*fvolume[_FC];
  PSingle(Dst)[_LF]:=PSingle(Src)[_LF]*fvolume[_LF];
  PSingle(Dst)[_SL]:=PSingle(Src)[_SL]*fvolume[_SL];
  PSingle(Dst)[_SR]:=PSingle(Src)[_SR]*fvolume[_SR];
  PSingle(Dst)[_BL]:=PSingle(Src)[_BL]*fvolume[_BL];
  PSingle(Dst)[_BR]:=PSingle(Src)[_BR]*fvolume[_BR];

  Inc(Src,SizeOf(Single)*8);
  Inc(Dst,SizeOf(Single)*8);

  Dec(count);
 end;
end;

procedure VecMulF32CH8STD(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 While (count<>0) do
 begin

  PSingle(Dst)[_FL]:=PSingle(Src)[_FL]   *fvolume[_FL];
  PSingle(Dst)[_FR]:=PSingle(Src)[_FR]   *fvolume[_FR];
  PSingle(Dst)[_FC]:=PSingle(Src)[_FC]   *fvolume[_FC];
  PSingle(Dst)[_LF]:=PSingle(Src)[_LF]   *fvolume[_LF];
  PSingle(Dst)[_SL]:=PSingle(Src)[STD_SL]*fvolume[STD_SL];
  PSingle(Dst)[_SR]:=PSingle(Src)[STD_SR]*fvolume[STD_SR];
  PSingle(Dst)[_BL]:=PSingle(Src)[STD_BL]*fvolume[STD_BL];
  PSingle(Dst)[_BR]:=PSingle(Src)[STD_BR]*fvolume[STD_BR];

  Inc(Src,SizeOf(Single)*8);
  Inc(Dst,SizeOf(Single)*8);

  Dec(count);
 end;
end;

//F32

procedure Unknow(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
begin
 Assert(false,'Unknow');
end;

const
 FORMATS:array[Boolean] of Word=(SDL_AUDIO_S16LE,SDL_AUDIO_F32LE);
 SMSIZES:array[Boolean] of Byte=(2,4);

Destructor TAudioOutSDL3.Destroy;
begin
 FreeMem(f_ibuf_ptr);
 SDL3Audio.SDL_DestroyAudioStream(Stream);
 SDL3Audio.SDL_CloseAudioDevice  (Device);
 inherited;
end;

function FindOutDevice(const device_id:RawByteString):TSDL_AudioDeviceID;
var
 list:PSDL_AudioDeviceID;
 i,count:Integer;
 a_name:pchar;
begin
 case device_id of
  '','[DEFAULT]':Exit(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK);
  else;
 end;

 count:=0;
 list :=SDL3Audio.SDL_GetAudioPlaybackDevices(@count);

 if (list<>nil) and (count<>0) then
 begin

  Result:=SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK;
  For i:=0 to count-1 do
  begin
   a_name:=SDL3Audio.SDL_GetAudioDeviceName(list[i]);
   if (a_name=device_id) then
   begin
    Result:=list[i];
    Break;
   end;
  end;
 end;

 SDL_free(list);
end;

Function TAudioOutSDL3.Open(const device_id:RawByteString):Boolean;
var
 Spec:TSDL_AudioSpec;
begin
 f_period   :=((QWORD(1000000) * QWORD(f_len)) + (f_freq div 2)) div f_freq;
 f_last_time:=0;
 f_next_time:=0;
 f_gbuf_size:=f_len*SMSIZES[f_param.is_float]*f_param.channels;
 f_ibuf_size:=f_len*SMSIZES[True            ]*f_param.channels;
 f_ibuf_ptr :=GetMem(f_ibuf_size);

 Spec.freq    :=f_freq;
 Spec.channels:=f_param.channels;
 Spec.format  :=SDL_AUDIO_F32LE;

 convert:=@Unknow;
 if f_param.is_float then
 begin
  //F32
  case f_param.channels of
   1:convert:=@VecMulF32M;
   2:convert:=@VecMulF32S;
   8:case f_param.is_std of
      False:convert:=@VecMulF32CH8;
      True :convert:=@VecMulF32CH8STD;
     end;
   else;
  end;
 end else
 begin
  //S16
  case f_param.channels of
   1:convert:=@VecMulS16M;
   2:convert:=@VecMulS16S;
   8:convert:=@VecMulS16CH8;
   else;
  end;
 end;

 if f_param.is_restricted  then LOG_WARNING('TODO: is_restricted ');
 if f_param.is_mix_to_main then LOG_WARNING('TODO: is_mix_to_main');

 Device:=SDL3Audio.SDL_OpenAudioDevice(FindOutDevice(device_id),@Spec);
 if (Device=0) then
 begin
  Exit(False);
 end;

 Stream:=SDL3Audio.SDL_CreateAudioStream(@Spec,nil);
 if (Stream=nil) then
 begin
  Exit(False);
 end;

 if not SDL3Audio.SDL_BindAudioStream(Device,Stream) then
 begin
  Exit(False);
 end;

 SDL3Audio.SDL_ResumeAudioDevice(Device);

 Result:=True;
end;

procedure TAudioOutSDL3.SetVolume(channel,vol:Integer);
begin
 svolume[channel]:=vol;
 fvolume[channel]:=vol/VOLUME_0DB;
end;

procedure TAudioOutSDL3.SetMixLevelPadSpk(mixLevel:Integer);
begin
 //
end;

function TAudioOutSDL3.GetLastOutputTime:QWORD;
begin
 Result:=f_last_time;
end;

function TAudioOutSDL3.Output(ptr:Pointer):Integer;
var
 time,d:QWORD;
 f_need_wait:QWORD;
begin
 Result:=0;

 //increase priority
 cpu_set_priority(curkthread,64);

 time:=GetProcessTime;

 if (ptr<>nil) then
 begin
  //mix
  convert(ptr,f_ibuf_ptr,f_len,@fvolume);
 end;

 if (f_next_time=0) then
 begin
  //first send
  f_next_time:=(time+f_period);
  f_need_wait:=0;
 end else
 if (time>f_next_time) then
 begin
  //underflow
  f_next_time:=(time+f_period);
  f_need_wait:=0;
 end else
 begin
  //wait prev
  f_need_wait:=f_next_time;
  f_next_time:=(f_next_time+f_period);
 end;

 if (f_need_wait<>0) then
 begin
  repeat
   time:=GetProcessTime;
   if (time>=f_need_wait) then Break;
   //
   d:=f_need_wait-time;
   if d>10 then d:=d-10;
   //
   usleep(d);
  until false;
 end;

 if (ptr<>nil) then
 begin
  //copy f_ibuf_ptr to SDL3
  SDL3Audio.SDL_PutAudioStreamData(Stream, f_ibuf_ptr, f_ibuf_size);
 end;

 //restore priority
 cpu_set_priority(curkthread,curkthread^.td_priority);
end;

//
class procedure TAudioOutSDL3.Outputs(param:pAudioOutParam;num:DWORD);
var
 i:DWORD;
begin
 for i:=0 to num-1 do
 begin
  param[i].handle.Output(param[i].ptr);
 end;
end;

end.

