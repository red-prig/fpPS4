unit ps4_libSceAudioOut;

{$mode objfpc}{$H+}

{/$define silent}

interface

uses
  atomic,
  spinlock,
  libportaudio,
  ps4_handles,
  ps4_program,
  Classes,
  SysUtils;

implementation

uses
 sys_signal;

const
 SCE_AUDIO_OUT_ERROR_NOT_OPENED         =-2144993279; // 0x80260001
 SCE_AUDIO_OUT_ERROR_BUSY               =-2144993278; // 0x80260002
 SCE_AUDIO_OUT_ERROR_INVALID_PORT       =-2144993277; // 0x80260003
 SCE_AUDIO_OUT_ERROR_INVALID_POINTER    =-2144993276; // 0x80260004
 SCE_AUDIO_OUT_ERROR_PORT_FULL          =-2144993275; // 0x80260005
 SCE_AUDIO_OUT_ERROR_INVALID_SIZE       =-2144993274; // 0x80260006
 SCE_AUDIO_OUT_ERROR_INVALID_FORMAT     =-2144993273; // 0x80260007
 SCE_AUDIO_OUT_ERROR_INVALID_SAMPLE_FREQ=-2144993272; // 0x80260008
 SCE_AUDIO_OUT_ERROR_INVALID_VOLUME     =-2144993271; // 0x80260009
 SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE  =-2144993270; // 0x8026000A
 SCE_AUDIO_OUT_ERROR_INVALID_CONF_TYPE  =-2144993268; // 0x8026000C
 SCE_AUDIO_OUT_ERROR_OUT_OF_MEMORY      =-2144993267; // 0x8026000D
 SCE_AUDIO_OUT_ERROR_ALREADY_INIT       =-2144993266; // 0x8026000E
 SCE_AUDIO_OUT_ERROR_NOT_INIT           =-2144993265; // 0x8026000F
 SCE_AUDIO_OUT_ERROR_MEMORY             =-2144993264; // 0x80260010
 SCE_AUDIO_OUT_ERROR_SYSTEM_RESOURCE    =-2144993263; // 0x80260011

 SCE_AUDIO_OUT_ERROR_TRANS_EVENT        =-2144993262; // 0x80260012
 SCE_AUDIO_OUT_ERROR_INVALID_FLAG       =-2144993261; // 0x80260013
 SCE_AUDIO_OUT_ERROR_INVALID_MIXLEVEL   =-2144993260; // 0x80260014
 SCE_AUDIO_OUT_ERROR_INVALID_ARG        =-2144993259; // 0x80260015
 SCE_AUDIO_OUT_ERROR_INVALID_PARAM      =-2144993258; // 0x80260016

 SCE_AUDIO_MIN_LEN=256;
 SCE_AUDIO_MAX_LEN=(256*8);

 SCE_AUDIO_OUT_PORT_TYPE_MAIN     =0;
 SCE_AUDIO_OUT_PORT_TYPE_BGM      =1;
 SCE_AUDIO_OUT_PORT_TYPE_VOICE    =2;
 SCE_AUDIO_OUT_PORT_TYPE_PERSONAL =3;
 SCE_AUDIO_OUT_PORT_TYPE_PADSPK   =4;
 SCE_AUDIO_OUT_PORT_TYPE_AUX      =127;

 SCE_AUDIO_OUT_PARAM_FORMAT_S16_MONO     =0;
 SCE_AUDIO_OUT_PARAM_FORMAT_S16_STEREO   =1;
 SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH      =2;
 SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_MONO   =3;
 SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_STEREO =4;
 SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH    =5;

 SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD  =6;
 SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD=7;

 SCE_AUDIO_OUT_PARAM_FORMAT_MASK  =$000000FF;
 SCE_AUDIO_OUT_PARAM_FORMAT_SHIFT =0;

 SCE_AUDIO_OUT_PARAM_ATTR_RESTRICTED  =$00010000;
 SCE_AUDIO_OUT_PARAM_ATTR_MIX_TO_MAIN =$00020000;

 SCE_AUDIO_OUT_PARAM_ATTR_MASK  =$000F0000;
 SCE_AUDIO_OUT_PARAM_ATTR_SHIFT =16;

 SCE_AUDIO_VOLUME_SHIFT       =15;
 SCE_AUDIO_VOLUME_0DB         =(1<<SCE_AUDIO_VOLUME_SHIFT);
 SCE_AUDIO_VOLUME_FLAG_L_CH   =(1 shl 0);
 SCE_AUDIO_VOLUME_FLAG_R_CH   =(1 shl 1);
 SCE_AUDIO_VOLUME_FLAG_C_CH   =(1 shl 2);
 SCE_AUDIO_VOLUME_FLAG_LFE_CH =(1 shl 3);
 SCE_AUDIO_VOLUME_FLAG_LS_CH  =(1 shl 4);
 SCE_AUDIO_VOLUME_FLAG_RS_CH  =(1 shl 5);
 SCE_AUDIO_VOLUME_FLAG_LE_CH  =(1 shl 6);
 SCE_AUDIO_VOLUME_FLAG_RE_CH  =(1 shl 7);

var
 _lazy_init:Integer=0;
 _lazy_wait:Integer=0;
 HAudioOuts:TIntegerHandles;

function ps4_sceAudioOutInit():Integer; SysV_ABI_CDecl;
begin

 if XCHG(_lazy_init,1)=0 then
 begin
  _sig_lock;
  Result:=Pa_Initialize();
  _sig_unlock;
  if (Result<>0) then Exit(SCE_AUDIO_OUT_ERROR_TRANS_EVENT);
  _sig_lock;
  HAudioOuts:=TIntegerHandles.Create;
  _sig_unlock;
  fetch_add(_lazy_wait,1);
 end else
 begin
  wait_until_equal(_lazy_wait,0);
  Result:=SCE_AUDIO_OUT_ERROR_ALREADY_INIT;
 end;

 //Writeln('sceAudioOutInit');
 //Result:=111;
end;

{
userId
 User ID of the output destination

type
 Virtual device type

index
 Device index (unused; specify 0)

len
 Granularity (number of samples to be output at once; 256, 512, 768, 1024, 1280, 1536, 1792, or 2048)

freq
 Sampling frequency (Hz; specify 48000)

param
 Data format, etc.


}

type
 TAudioOutHandle=class(TClassHandle)
  userId,_type,index:Integer;
  len,freq,param:DWORD;
  volume:array[0..7] of Integer;

  pstream:PaStream;
  pnumOutputChannels:Integer;
  psampleFormat:PaSampleFormat;

  bufsize:Integer;
  buf:Pointer;

  //d:QWORD;

  Destructor Destroy; override;
 end;

Destructor TAudioOutHandle.Destroy;
begin
 if (pstream<>nil) then
 begin
  Pa_StopStream(pstream);
  Pa_CloseStream(pstream);
 end;
 FreeMem(buf);
 inherited;
end;

//int32_t SceUserServiceUserId;
function ps4_sceAudioOutOpen(userId,_type,index:Integer;
                         len,freq,param:DWORD):Integer; SysV_ABI_CDecl;
Var
 H:TAudioOutHandle;
 i:Byte;

 err:Integer;
 pstream:PaStream;
 pnumOutputChannels:Integer;
 psampleFormat:PaSampleFormat;

begin
 Result:=0;
 if (HAudioOuts=nil) then Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);

 case _type of
  SCE_AUDIO_OUT_PORT_TYPE_MAIN    :;
  SCE_AUDIO_OUT_PORT_TYPE_BGM     :;
  SCE_AUDIO_OUT_PORT_TYPE_VOICE   :;
  SCE_AUDIO_OUT_PORT_TYPE_PERSONAL:;
  SCE_AUDIO_OUT_PORT_TYPE_PADSPK  :;
  SCE_AUDIO_OUT_PORT_TYPE_AUX     :;
  else
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
 end;

 case len of
  256,
  512,
  768,
  1024,
  1280,
  1536,
  1792,
  2048:;
  else
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_SIZE);
 end;

 case freq of
  48000:;
  else
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_SAMPLE_FREQ);
 end;

 case (param and SCE_AUDIO_OUT_PARAM_FORMAT_MASK) of
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_MONO:
   begin
    pnumOutputChannels:=1;
    psampleFormat:=paInt16;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_STEREO:
   begin
    pnumOutputChannels:=2;
    psampleFormat:=paInt16;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH:
   begin
    pnumOutputChannels:=8;
    psampleFormat:=paInt16;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_MONO:
   begin
    pnumOutputChannels:=1;
    psampleFormat:=paFloat32;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_STEREO:
   begin
    pnumOutputChannels:=2;
    psampleFormat:=paFloat32;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH:
   begin
    pnumOutputChannels:=8;
    psampleFormat:=paFloat32;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD:
   begin
    pnumOutputChannels:=8;
    psampleFormat:=paInt16;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD:
   begin
    pnumOutputChannels:=8;
    psampleFormat:=paFloat32;
   end;
  else
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
 end;

 _sig_lock;
 err:=Pa_OpenDefaultStream(@pstream,
                           0,
                           pnumOutputChannels,
                           psampleFormat,
                           freq,
                           paFramesPerBufferUnspecified,nil,nil);
 _sig_unlock;

 if (err<>0) and (pnumOutputChannels>2) then
 begin
  pnumOutputChannels:=2;
  _sig_lock;
  err:=Pa_OpenDefaultStream(@pstream,
                            0,
                            pnumOutputChannels,
                            psampleFormat,
                            freq,
                            paFramesPerBufferUnspecified,nil,nil);
  _sig_unlock;
 end;

 if (err<>0) then
 begin
  Writeln('Pa_GetErrorText:',PaErrorCode(err),':',Pa_GetErrorText(err));
  //Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
  pstream:=nil;
 end;

 err:=0;
 if (pstream<>nil) then
 begin
  _sig_lock;
  err:=Pa_StartStream(pstream);
  _sig_unlock;
 end;

 if (err<>0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 _sig_lock;
 H:=TAudioOutHandle.Create;
 _sig_unlock;

 H.userId:=userId;
 H._type :=_type ;
 H.index :=index ;
 H.len   :=len   ;
 H.freq  :=freq  ;
 H.param :=param ;

 For i:=0 to 7 do
  H.volume[i]:=SCE_AUDIO_VOLUME_0DB;

 H.pstream           :=pstream;
 H.pnumOutputChannels:=pnumOutputChannels;
 H.psampleFormat     :=psampleFormat;

 _sig_lock;
 if not HAudioOuts.New(H,Result) then Result:=SCE_AUDIO_OUT_ERROR_PORT_FULL;
 _sig_unlock;

 Case QWORD(psampleFormat) of
  QWORD(paInt16  ):H.bufsize:=2*pnumOutputChannels*len;
  QWORD(paFloat32):H.bufsize:=4*pnumOutputChannels*len;
 end;

 _sig_lock;
 H.buf:=GetMem(H.bufsize);
 _sig_unlock;

 H.Release;

 Writeln('AudioOutOpen:',userId,':',_type,':',index,':',len,':',freq,':',param);
end;

function ps4_sceAudioOutClose(handle:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (HAudioOuts=nil) then Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 _sig_lock;
 if not HAudioOuts.Delete(handle) then Result:=SCE_AUDIO_OUT_ERROR_INVALID_PORT;
 _sig_unlock;
end;

function ps4_sceAudioOutSetVolume(handle,flag:Integer;vol:PInteger):Integer; SysV_ABI_CDecl;
Var
 H:TAudioOutHandle;
 i:Integer;
begin
 if (HAudioOuts=nil) then Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);

 if (vol=nil) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 i:=vol^;
 if (i>SCE_AUDIO_VOLUME_0DB) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_VOLUME);

 {$ifdef silent}if (i>800) then i:=800;{$endif}

 _sig_lock;
 H:=TAudioOutHandle(HAudioOuts.Acqure(handle));
 _sig_unlock;

 if (H=nil) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);

 if (flag and SCE_AUDIO_VOLUME_FLAG_L_CH  <>0) then H.volume[0]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_R_CH  <>0) then H.volume[1]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_C_CH  <>0) then H.volume[2]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_LFE_CH<>0) then H.volume[3]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_LS_CH <>0) then H.volume[4]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_RS_CH <>0) then H.volume[5]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_LE_CH <>0) then H.volume[6]:=i;
 if (flag and SCE_AUDIO_VOLUME_FLAG_RE_CH <>0) then H.volume[7]:=i;

 H.Release;
 Writeln('sceAudioOutSetVolume:',handle,':',flag);
 Result:=0;
end;

procedure _VecMulI16M(Src,Dst:Pointer;count:Integer;volume:Integer);// inline;
begin
 if volume=SCE_AUDIO_VOLUME_0DB then
 begin
  Move(Src^,Dst^,count*2);
 end else
 While (count>0) do
 begin
  PSmallInt(Dst)^:=(PSmallInt(Src)^*volume) div SCE_AUDIO_VOLUME_0DB;
  Inc(Src,2);
  Inc(Dst,2);
  Dec(count);
 end;
end;

procedure _VecMulI16S(Src,Dst:Pointer;count:Integer;volume:PInteger); inline;
begin
 if (volume[0]=SCE_AUDIO_VOLUME_0DB) and (volume[1]=SCE_AUDIO_VOLUME_0DB) then
 begin
  Move(Src^,Dst^,count*2*2);
 end else
 While (count>0) do
 begin
  PSmallInt(Dst)^:=(PSmallInt(Src)^*volume[0]) div SCE_AUDIO_VOLUME_0DB;
  Inc(Src,2);
  Inc(Dst,2);
  PSmallInt(Dst)^:=(PSmallInt(Src)^*volume[1]) div SCE_AUDIO_VOLUME_0DB;
  Inc(Src,2);
  Inc(Dst,2);
  Dec(count);
 end;
end;

procedure _VecMulF32M(Src,Dst:Pointer;count:Integer;volume:Integer); inline;
var
 fvolume:Single;
begin
 if volume=SCE_AUDIO_VOLUME_0DB then
 begin
  Move(Src^,Dst^,count*4);
 end else
 begin
  fvolume:=volume/SCE_AUDIO_VOLUME_0DB;
  While (count>0) do
  begin
   PSingle(Dst)^:=PSingle(Src)^*fvolume;
   Inc(Src,4);
   Inc(Dst,4);
   Dec(count);
  end;
 end;
end;

procedure _VecMulF32S(Src,Dst:Pointer;count:Integer;volume:PInteger); inline;
var
 fvolume:array[0..1] of Single;
begin
 if (volume[0]=SCE_AUDIO_VOLUME_0DB) and (volume[1]=SCE_AUDIO_VOLUME_0DB) then
 begin
  Move(Src^,Dst^,count*4*2);
 end else
 begin
  fvolume[0]:=volume[0]/SCE_AUDIO_VOLUME_0DB;
  fvolume[1]:=volume[1]/SCE_AUDIO_VOLUME_0DB;
  While (count>0) do
  begin
   PSingle(Dst)^:=PSingle(Src)^*fvolume[0];
   Inc(Src,4);
   Inc(Dst,4);
   PSingle(Dst)^:=PSingle(Src)^*fvolume[1];
   Inc(Src,4);
   Inc(Dst,4);
   Dec(count);
  end;
 end;
end;

//  1+3/√2
//L=FL+0.707*C+0.707*SL+0.707*BL
//R=FR+0.707*C+0.707*SR+0.707*BR
//1/√2

const
 _FL=0;
 _FR=1;
 _FC=2;
 _LF=3;
 _SL=4;
 _SR=5;
 _BL=6;
 _BR=7;

procedure _VecMulF32CH8ToS(Src,Dst:Pointer;count:Integer;volume:PInteger);
const
 fdiv1:Single=1+(3/Sqrt(2));
 fdiv2:Single=(1/Sqrt(2))*(1+(3/Sqrt(2)));
var
 fvolume:array[0..7] of Single;
 fL,fR:Single;
begin
 fvolume[_FL]:=(volume[_FL]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FR]:=(volume[_FR]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FC]:=(volume[_FC]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SL]:=(volume[_SL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SR]:=(volume[_SR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BL]:=(volume[_BL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BR]:=(volume[_BR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 While (count>0) do
 begin

  fL:=(PSingle(Src)[_FL]*fvolume[_FL])+
      (PSingle(Src)[_FC]*fvolume[_FC])+
      (PSingle(Src)[_SL]*fvolume[_SL])+
      (PSingle(Src)[_BL]*fvolume[_BL]);

  fR:=(PSingle(Src)[_FR]*fvolume[_FR])+
      (PSingle(Src)[_FC]*fvolume[_FC])+
      (PSingle(Src)[_SR]*fvolume[_SR])+
      (PSingle(Src)[_BR]*fvolume[_BR]);

  //fL:=fL*0.05;
  //fR:=fR*0.05;

  PSingle(Dst)^:=fL;
  Inc(Dst,4);
  PSingle(Dst)^:=fR;
  Inc(Dst,4);

  Inc(Src,4*8);

  Dec(count);
 end;
end;

function ps4_sceAudioOutOutput(handle:Integer;ptr:Pointer):Integer;  SysV_ABI_CDecl;
Var
 H:TAudioOutHandle;
 count,err:Integer;

begin
 if (HAudioOuts=nil) then Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);

 if (ptr=nil) then Exit(0);

 err:=0;
 _sig_lock;
 H:=TAudioOutHandle(HAudioOuts.Acqure(handle));
 _sig_unlock;

 if (H=nil) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);

 count:=H.len;

 if (H.pstream<>nil) then
 case (H.param and SCE_AUDIO_OUT_PARAM_FORMAT_MASK) of
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_MONO:
   begin
    _VecMulI16M(ptr,H.buf,count,H.volume[0]);
    _sig_lock;
    err:=Pa_WriteStream(H.pstream,H.buf,count);
    _sig_unlock;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_STEREO:
   begin
    _VecMulI16S(ptr,H.buf,count,@H.volume);
    _sig_lock;
    err:=Pa_WriteStream(H.pstream,H.buf,count);
    _sig_unlock;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH:
   begin
    Assert(false);
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_MONO:
   begin
    _VecMulF32M(ptr,H.buf,count,H.volume[0]);
    _sig_lock;
    err:=Pa_WriteStream(H.pstream,H.buf,count);
    _sig_unlock;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_STEREO:
   begin
    _VecMulF32S(ptr,H.buf,count,@H.volume);
    _sig_lock;
    err:=Pa_WriteStream(H.pstream,H.buf,count);
    _sig_unlock;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH:
   begin

    if H.pnumOutputChannels=2 then
    begin
     _VecMulF32CH8ToS(ptr,H.buf,count,@H.volume);
     _sig_lock;
     err:=Pa_WriteStream(H.pstream,H.buf,count);
     _sig_unlock;
    end else
    begin
     Assert(false);
    end;

   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD:
   begin
    Assert(false);
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD:
   begin
    Assert(false);
   end;
 end;

 Case err of
  0:;
  Integer(paOutputUnderflowed):;
  else
   Writeln('Pa_GetErrorText:',PaErrorCode(err),':',Pa_GetErrorText(err));
 end;

 //Writeln('sceAudioOutOutput:',handle,':',HexStr(ptr));

 _sig_lock;
 H.Release;
 _sig_unlock;

 Result:=0;
end;

type
 PSceAudioOutOutputParam=^SceAudioOutOutputParam;
 SceAudioOutOutputParam=packed record
  handle:Integer;
  align:Integer;
  ptr:Pointer;
 end;

function ps4_sceAudioOutOutputs(param:PSceAudioOutOutputParam;num:DWORD):Integer;  SysV_ABI_CDecl;
var
 i:DWORD;
begin
 if (param=nil) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 if (num=0) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_PARAM);
 For i:=0 to num-1 do
 begin
  Result:=ps4_sceAudioOutOutput(param[i].handle,param[i].ptr);
  if (Result<>0) then Exit;
 end;
end;

function Load_libSceAudioOut(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceAudioOut');

 lib^.set_proc($25F10F5D5C6116A0,@ps4_sceAudioOutInit);
 lib^.set_proc($7A436FB13DB6AEC6,@ps4_sceAudioOutOpen);
 lib^.set_proc($B35FFFB84F66045C,@ps4_sceAudioOutClose);
 lib^.set_proc($6FEB8057CF489711,@ps4_sceAudioOutSetVolume);
 lib^.set_proc($40E42D6DE0EAB13E,@ps4_sceAudioOutOutput);
 lib^.set_proc($C373DD6924D2C061,@ps4_sceAudioOutOutputs);
end;

const
 SCE_AUDIO_IN_ERROR_NOT_OPENED=$80260109;

function ps4_sceAudioInOpen(userID,busType,index,len,freq,param:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=Integer(SCE_AUDIO_IN_ERROR_NOT_OPENED);
end;

function Load_libSceAudioIn(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceAudioIn');
 lib^.set_proc($E4D13C4A373B542F,@ps4_sceAudioInOpen);
end;

initialization
 ps4_app.RegistredPreLoad('libSceAudioOut.prx',@Load_libSceAudioOut);
 ps4_app.RegistredPreLoad('libSceAudioIn.prx',@Load_libSceAudioIn);

end.

