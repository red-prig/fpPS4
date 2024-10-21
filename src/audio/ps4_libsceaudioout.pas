unit ps4_libSceAudioOut;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

{/$define silent}

interface

uses
  //libportaudio,
  subr_dynlib,
  audioout_interface;

implementation

uses
 sysutils,
 atomic,
 kern_rwlock,
 kern_proc,
 ps4_libSceMbus;

{
uses
 ps4_time,
 sys_signal;
}

var
 g_audioout_interface:TAbstractAudioOut=nil;

 g_port_table:array[0..24] of TAudioOutHandle;

 g_port_lock:Pointer=nil;

function alloc_port_id(a,b:Byte):Integer;
begin
 Result:=-1;
 For a:=a to b do
 if (g_port_table[a]=nil) then
 begin
  Exit(a);
 end;
end;

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

 SCE_AUDIO_OUT_STATE_OUTPUT_UNKNOWN            =$00;
 SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_PRIMARY  =$01;
 SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_SECONDARY=$02;
 SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_TERTIARY =$04;
 SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_HEADPHONE=$40;
 SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_EXTERNAL =$80;

 SCE_AUDIO_OUT_STATE_CHANNEL_UNKNOWN     =0;
 SCE_AUDIO_OUT_STATE_CHANNEL_DISCONNECTED=0;
 SCE_AUDIO_OUT_STATE_CHANNEL_1           =1;
 SCE_AUDIO_OUT_STATE_CHANNEL_2           =2;
 SCE_AUDIO_OUT_STATE_CHANNEL_6           =6;
 SCE_AUDIO_OUT_STATE_CHANNEL_8           =8;

type
 pSceAudioOutPortState=^SceAudioOutPortState;
 SceAudioOutPortState=packed record
  output        :Word;
  channel       :Byte;
  reserved8_1   :Byte;
  volume        :Smallint;
  rerouteCounter:Word;
  flag          :QWord;
  reserved64    :array[0..1] of QWORD;
 end;

 PSceAudioOutOutputParam=^SceAudioOutOutputParam;
 SceAudioOutOutputParam=packed record
  handle:Integer;
  align :Integer;
  ptr   :Pointer;
 end;

 pSceAudioOutSystemState=^SceAudioOutSystemState;
 SceAudioOutSystemState=packed record
  loudness  :single;
  reserved8 :array[0..3] of Byte;
  reserved64:array[0..2] of QWORD;
 end;

var
 _lazy_init:Integer=0;

function ps4_sceAudioOutInit():Integer;
begin

 if XCHG(_lazy_init,1)=0 then
 begin

  g_audioout_interface:=TAudioOutNull;

  Result:=0;
 end else
 begin
  Result:=SCE_AUDIO_OUT_ERROR_ALREADY_INIT;
 end;

end;

{
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

  last_time:QWORD;

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
}

function _out_open(userId,_type:Integer;
                   len,param:DWORD):Integer;
var
 port_id:Integer;
 f_channels:DWORD;
 handle:TAudioOutHandle;
begin
 //case   0: port_id[0..7]
 //case   1: port_id[8..8]
 //case   2: port_id[9..12]
 //case   3: port_id[13..16]
 //case   4: port_id[17..20]
 //case   5: port_id[21..21]
 //case   6: port_id[21..21]
 //case   7: port_id[21..21]
 //case   8: port_id[21..21]
 //case   9: break;
 //case  10: break;
 //case  11: break;
 //case  12: break;
 //case  13: break;
 //case  14: port_id[22..22]
 //case 125: port_id[24..24]
 //case 127: port_id[23..23]

 //alloc id stage
 case _type of
    0:port_id:=alloc_port_id(0 ,7);
    1:port_id:=alloc_port_id(8 ,8);
    2:port_id:=alloc_port_id(9 ,12);
    3:port_id:=alloc_port_id(13,16);
    4:port_id:=alloc_port_id(17,20);
    5:port_id:=alloc_port_id(21,21);
    6:port_id:=alloc_port_id(21,21);
    7:port_id:=alloc_port_id(21,21);
    8:port_id:=alloc_port_id(21,21);
   14:port_id:=alloc_port_id(22,22);
  125:port_id:=alloc_port_id(24,24);
  127:port_id:=alloc_port_id(23,23);
  else
      port_id:=-1;
 end;

 if (port_id<0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_PORT_FULL);
 end;

 f_channels:=0;

 case (param and SCE_AUDIO_OUT_PARAM_FORMAT_MASK) of
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_MONO:
   begin
    f_channels:=1;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_STEREO:
   begin
    f_channels:=2;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH:
   begin
    f_channels:=8;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_MONO:
   begin
    f_channels:=1;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_STEREO:
   begin
    f_channels:=2;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH:
   begin
    f_channels:=8;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD:
   begin
    f_channels:=8;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD:
   begin
    f_channels:=8;
   end;
  10..14:
   begin
    Assert(false,'Undocumented sample format! :'+IntToStr((param and SCE_AUDIO_OUT_PARAM_FORMAT_MASK)));
    Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
   end;
  else
   begin
    Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
   end;
 end;

 handle:=g_audioout_interface.Create;

 if (handle=nil) then
 begin
  Assert(false,'audioout_interface alloc failed');
  Exit(SCE_AUDIO_OUT_ERROR_OUT_OF_MEMORY);
 end;

 handle.f_userId   :=userId;
 handle.f_type     :=_type;
 handle.f_len      :=len;
 handle.f_param    :=param;
 handle.f_channels :=f_channels;

 if not handle.Open('') then
 begin
  FreeAndNil(handle);
  Assert(false,'audioout_interface open failed');
  Exit(SCE_AUDIO_OUT_ERROR_TRANS_EVENT);
 end;

 handle.SetVolume(0,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(1,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(2,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(3,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(4,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(5,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(6,SCE_AUDIO_VOLUME_0DB);
 handle.SetVolume(7,SCE_AUDIO_VOLUME_0DB);

 if (_type=SCE_AUDIO_OUT_PORT_TYPE_PADSPK) then
 begin
  handle.SetMixLevelPadSpk(11626);
 end;

 //save handle
 g_port_table[port_id]:=handle;

 Result:=port_id;
end;

//int32_t SceUserServiceUserId;
function ps4_sceAudioOutOpen(userId,_type,index:Integer;
                             len,freq,param:DWORD):Integer;
{
Var
 H:TAudioOutHandle;
 i:Byte;

 err:Integer;
 pstream:PaStream;
 pnumOutputChannels:Integer;
 psampleFormat:PaSampleFormat;
 }
begin
 Result:=0;

 if (_lazy_init=0) or (g_audioout_interface=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 if (len=0) or (len>2048) or ((len and $FF)<>0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_SIZE);
 end;

 if ((_type <> SCE_AUDIO_OUT_PORT_TYPE_PERSONAL) and ((param and $20000) <> 0)) then
 begin
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
 end;

 if ((_type <> SCE_AUDIO_OUT_PORT_TYPE_MAIN) and ((param and $70000000) <> 0)) then
 begin
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
 end;

 if ({(private = 0) and} ((param and $8ffcff00) <> 0)) then
 begin
   Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
 end;

 if (_type<0) then
 begin
  case DWORD(_type) of
   $80000000,
   $80000001,
   $80000002,
   $80000003,
   $80000004,
   $8000007f:
     begin
      if (freq <> 48000) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_SAMPLE_FREQ);
      end;
     end;
   else
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
  end;
 end else
 begin
  case DWORD(_type) of
   SCE_AUDIO_OUT_PORT_TYPE_MAIN,
   SCE_AUDIO_OUT_PORT_TYPE_BGM,
   SCE_AUDIO_OUT_PORT_TYPE_VOICE,
   SCE_AUDIO_OUT_PORT_TYPE_PERSONAL,
   SCE_AUDIO_OUT_PORT_TYPE_PADSPK,
   SCE_AUDIO_OUT_PORT_TYPE_AUX,
   14:
     begin
      if (freq <> 48000) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_SAMPLE_FREQ);
      end;
     end;
   5..13:
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
     end;
   126:
     begin
      Exit(0);
     end;
   else
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
     end;
  end;
 end;

 DWORD(_type):=DWORD(_type) and $7fffffff;

 rw_wlock(g_port_lock);
  Result:=_out_open(userId,_type,len,param);
 rw_wunlock(g_port_lock);

 if (Result<0) then Exit;

 Result:=(DWORD(_type) shl 16) or DWORD(Result) or $20000000;

 ps4_sceMbusAddHandleByUserId(1,Result,userId,_type,index,0);

{
 pstream:=nil;
 err:=0;
 if (_type=SCE_AUDIO_OUT_PORT_TYPE_MAIN) or (_type=SCE_AUDIO_OUT_PORT_TYPE_BGM) then //so far only MAIN/BGM
 begin
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
   Writeln(StdErr,'Pa_GetErrorText:',PaErrorCode(err),':',Pa_GetErrorText(err));
   //Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
  end;
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
 }
end;

function _out_close(port_id:Integer):Integer;
begin
 if (g_port_table[port_id]=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_OPENED);
 end;

 FreeAndNil(g_port_table[port_id]);
 Result:=0;
end;

function _get_port_id(handle:Integer):Integer; inline;
begin
 Result:=DWORD(handle) and $FF;

 if (Result > 25) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
 end;

 if ((DWORD(handle) and $3f000000) <> $20000000) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
 end;
end;

function ps4_sceAudioOutClose(handle:Integer):Integer;
var
 port_id  :Integer;
 port_type:Integer;
begin
 Result:=0;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 port_id:=_get_port_id(handle);
 if (port_id<0) then Exit(port_id);

 ps4_sceMbusRemoveHandle(1,handle);

 port_type:=Byte(handle shr 16);

 case DWORD(port_type) of
  0..4,14,127:
    begin
     //valid
    end;
  5..13:
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
  126:
    begin
     Exit(0);
    end;
  else
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
 end;

 rw_wlock(g_port_lock);
  Result:=_out_close(port_id);
 rw_wunlock(g_port_lock);
end;

function ps4_sceAudioOutGetPortState(handle:Integer;state:pSceAudioOutPortState):Integer;
var
 port_id  :Integer;
 port_type:Integer;
begin
 Result:=0;

 if (state=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 end;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 port_id:=_get_port_id(handle);
 if (port_id<0) then Exit(port_id);

 port_type:=Byte(handle shr 16);

 case DWORD(port_type) of
  0..4,14,125,127:
    begin
     //valid
    end;
  5..13:
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
    end;
  else
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
 end;

 rw_wlock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin

   case (g_port_table[port_id].f_type) of
    SCE_AUDIO_OUT_PORT_TYPE_MAIN,
    SCE_AUDIO_OUT_PORT_TYPE_BGM:
     begin
      state^.output:=SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_PRIMARY;
     end;
    SCE_AUDIO_OUT_PORT_TYPE_VOICE,
    SCE_AUDIO_OUT_PORT_TYPE_PERSONAL:
     begin
      state^.output:=SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_HEADPHONE;
     end;
    SCE_AUDIO_OUT_PORT_TYPE_PADSPK:
     begin
      state^.output:=SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_TERTIARY;
     end;
    SCE_AUDIO_OUT_PORT_TYPE_AUX:
     begin
      state^.output:=SCE_AUDIO_OUT_STATE_OUTPUT_CONNECTED_EXTERNAL;
     end;
    else
     begin
      state^.output:=SCE_AUDIO_OUT_STATE_OUTPUT_UNKNOWN;
     end;
   end;

   state^.channel:=Byte(g_port_table[port_id].f_channels);

   if (g_port_table[port_id].f_type=SCE_AUDIO_OUT_PORT_TYPE_PADSPK) then
   begin
    state^.volume:=127; //max
   end else
   begin
    state^.volume:=-1; //invalid
   end;

   state^.rerouteCounter:=0;
   state^.flag          :=0;

  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 rw_wunlock(g_port_lock);
end;

function ps4_sceAudioOutSetVolume(handle,flag:Integer;p_vol:PInteger):Integer;
var
 volume   :Integer;
 port_id  :Integer;
 port_type:Integer;
 ahandle  :TAudioOutHandle;
begin
 Result:=0;

 if (p_vol=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 end;

 volume:=p_vol^;

 if (volume>SCE_AUDIO_VOLUME_0DB) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_VOLUME);
 end;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 port_id:=_get_port_id(handle);
 if (port_id<0) then Exit(port_id);

 port_type:=Byte(handle shr 16);

 case DWORD(port_type) of
  0..4,14,125,127:
    begin
     //valid
    end;
  5..13:
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
    end;
  else
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
 end;

 {$ifdef silent}if (volume>800) then volume:=800;{$endif}

 rw_wlock(g_port_lock);

  ahandle:=g_port_table[port_id];

  if (ahandle<>nil) then
  begin

   if (flag and SCE_AUDIO_VOLUME_FLAG_L_CH  <>0) then ahandle.SetVolume(0,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_R_CH  <>0) then ahandle.SetVolume(1,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_C_CH  <>0) then ahandle.SetVolume(2,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_LFE_CH<>0) then ahandle.SetVolume(3,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_LS_CH <>0) then ahandle.SetVolume(4,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_RS_CH <>0) then ahandle.SetVolume(5,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_LE_CH <>0) then ahandle.SetVolume(6,volume);
   if (flag and SCE_AUDIO_VOLUME_FLAG_RE_CH <>0) then ahandle.SetVolume(7,volume);

  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 rw_wunlock(g_port_lock);

 Result:=0;
end;

function ps4_sceAudioOutSetMixLevelPadSpk(handle,mixLevel:Integer):Integer;
var
 port_id:Integer;
begin
 Result:=0;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 port_id:=_get_port_id(handle);
 if (port_id<0) then Exit(port_id);

 if (Byte(handle shr 16)<>SCE_AUDIO_OUT_PORT_TYPE_PADSPK) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
 end;

 if (mixLevel>SCE_AUDIO_VOLUME_0DB) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_MIXLEVEL);
 end;

 rw_wlock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin
   g_port_table[port_id].SetMixLevelPadSpk(mixLevel);
  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 rw_wunlock(g_port_lock);
end;

function ps4_sceAudioOutGetLastOutputTime(handle:Integer;outputTime:PQWORD):Integer;
var
 port_id  :Integer;
 port_type:Integer;
begin
 Result:=0;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 port_id:=_get_port_id(handle);
 if (port_id<0) then Exit(port_id);

 if (outputTime=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 end;

 port_type:=Byte(handle shr 16);

 case DWORD(port_type) of
  0..4,14,125,127:
    begin
     //valid
    end;
  5..13:
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
    end;
  else
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
 end;

 rw_wlock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin
   outputTime^:=g_port_table[port_id].GetLastOutputTime;
  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 rw_wunlock(g_port_lock);
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

 STD_SL=6;
 STD_SR=7;
 STD_BL=4;
 STD_BR=5;

procedure __VecMulF32CH8ToS(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
var
 fL,fR:Single;
begin
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

  PSingle(Dst)^:=fL;
  Inc(Dst,4);
  PSingle(Dst)^:=fR;
  Inc(Dst,4);

  Inc(Src,4*8);

  Dec(count);
 end;
end;

procedure _VecMulF32CH8ToS(Src,Dst:Pointer;count:Integer;volume:PInteger);
const
 fdiv1:Single=1+(3/Sqrt(2));
 fdiv2:Single=(1/Sqrt(2))*(1+(3/Sqrt(2)));
var
 fvolume:array[0..7] of Single;
begin
 fvolume[_FL]:=(volume[_FL]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FR]:=(volume[_FR]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FC]:=(volume[_FC]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SL]:=(volume[_SL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SR]:=(volume[_SR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BL]:=(volume[_BL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BR]:=(volume[_BR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 __VecMulF32CH8ToS(Src,Dst,count,@fvolume);
end;

procedure __VecMulS16CH8ToS(Src,Dst:Pointer;count:Integer;fvolume:PSingle);
var
 fL,fR:Single;
begin
 While (count>0) do
 begin

  fL:=(PSmallInt(Src)[_FL]*fvolume[_FL])+
      (PSmallInt(Src)[_FC]*fvolume[_FC])+
      (PSmallInt(Src)[_SL]*fvolume[_SL])+
      (PSmallInt(Src)[_BL]*fvolume[_BL]);

  fR:=(PSmallInt(Src)[_FR]*fvolume[_FR])+
      (PSmallInt(Src)[_FC]*fvolume[_FC])+
      (PSmallInt(Src)[_SR]*fvolume[_SR])+
      (PSmallInt(Src)[_BR]*fvolume[_BR]);

  PSmallInt(Dst)^:=Trunc(fL);
  Inc(Dst,2);
  PSmallInt(Dst)^:=Trunc(fR);
  Inc(Dst,2);

  Inc(Src,2*8);

  Dec(count);
 end;
end;

procedure _VecMulS32CH8ToS(Src,Dst:Pointer;count:Integer;volume:PInteger);
const
 fdiv1:Single=1+(3/Sqrt(2));
 fdiv2:Single=(1/Sqrt(2))*(1+(3/Sqrt(2)));
var
 fvolume:array[0..7] of Single;
begin
 fvolume[_FL]:=(volume[_FL]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FR]:=(volume[_FR]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FC]:=(volume[_FC]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SL]:=(volume[_SL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SR]:=(volume[_SR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BL]:=(volume[_BL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BR]:=(volume[_BR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 __VecMulS16CH8ToS(Src,Dst,count,@fvolume);
end;

procedure _VecMulF32CH8STDToS(Src,Dst:Pointer;count:Integer;volume:PInteger);
const
 fdiv1:Single=1+(3/Sqrt(2));
 fdiv2:Single=(1/Sqrt(2))*(1+(3/Sqrt(2)));
var
 fvolume:array[0..7] of Single;
begin
 fvolume[_FL]:=(volume[   _FL]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FR]:=(volume[   _FR]/SCE_AUDIO_VOLUME_0DB)*fdiv1;
 fvolume[_FC]:=(volume[   _FC]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SL]:=(volume[STD_SL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_SR]:=(volume[STD_SR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BL]:=(volume[STD_BL]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 fvolume[_BR]:=(volume[STD_BR]/SCE_AUDIO_VOLUME_0DB)*fdiv2;
 __VecMulF32CH8ToS(Src,Dst,count,@fvolume);
end;

function ps4_sceAudioOutOutput(handle:Integer;ptr:Pointer):Integer;
var
 port_id  :Integer;
 port_type:Integer;
begin
 Result:=0;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 port_id:=_get_port_id(handle);
 if (port_id<0) then Exit(port_id);

 port_type:=Byte(handle shr 16);

 case DWORD(port_type) of
  0..4,14,125,127:
    begin
     //valid
    end;
  5..13:
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
    end;
  else
    begin
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
    end;
 end;

 rw_wlock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin
   Result:=g_port_table[port_id].Output(ptr);
   if (Result<0) then Exit(SCE_AUDIO_OUT_ERROR_BUSY);
  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 rw_wunlock(g_port_lock);

 {
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

    if (H.pnumOutputChannels=2) then
    begin
     _VecMulS32CH8ToS(ptr,H.buf,count,@H.volume);
     _sig_lock;
     err:=Pa_WriteStream(H.pstream,H.buf,count);
     _sig_unlock;
    end else
    begin
     Assert(false,'SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH');
    end;

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

    if (H.pnumOutputChannels=2) then
    begin
     _VecMulF32CH8ToS(ptr,H.buf,count,@H.volume);
     _sig_lock;
     err:=Pa_WriteStream(H.pstream,H.buf,count);
     _sig_unlock;
    end else
    begin
     Assert(false,'SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH');
    end;

   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD:
   begin
    Assert(false,'SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD');
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD:
   begin

    if (H.pnumOutputChannels=2) then
    begin
     _VecMulF32CH8STDToS(ptr,H.buf,count,@H.volume);
     _sig_lock;
     err:=Pa_WriteStream(H.pstream,H.buf,count);
     _sig_unlock;
    end else
    begin
     Assert(false,'SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD');
    end;

   end;
 end;

 Case err of
  0:;
  Integer(paOutputUnderflowed):;
  else
   Writeln(StdErr,'Pa_GetErrorText:',PaErrorCode(err),':',Pa_GetErrorText(err));
 end;

 //Writeln('sceAudioOutOutput:',handle,':',HexStr(ptr));

 H.last_time:=ps4_sceKernelGetProcessTime;

 _sig_lock;
 H.Release;
 _sig_unlock;
 }

end;

function ps4_sceAudioOutOutputs(param:PSceAudioOutOutputParam;num:DWORD):Integer;
var
 handle   :Integer;
 port_id  :Integer;
 port_type:Integer;
 //
 i,f:DWORD;
 //
 params:array[0..24] of TAudioOutParam;
begin
 Result:=0;

  if (_lazy_init=0) or (g_audioout_interface=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 if (num=0) or (num>25) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_PORT_FULL);
 end;

 if (param=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 end;

 //test all
 For i:=0 to num-1 do
 begin
  handle:=param[i].handle;

  port_id:=_get_port_id(handle);
  if (port_id<0) then Exit(port_id);

  port_type:=Byte(handle shr 16);

  case DWORD(port_type) of
   0..4,14,125,127:
     begin
      //valid
     end;
   5..13:
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
     end;
   else
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT_TYPE);
     end;
  end;
 end;

 rw_wlock(g_port_lock);

  //test opened
  For i:=0 to num-1 do
  begin
   handle:=param[i].handle;

   port_id:=_get_port_id(handle);

   if (g_port_table[port_id]<>nil) then
   begin
    //test dublicate
    if (i<>0) and
       (num<>1) and
       (p_proc.p_sdk_version > $44fffff) then
    begin
     for f:=0 to num-1 do
      if (f<>i) then
      if (handle=param[f].handle) then
      begin
       Writeln(stderr,'[AudioOut] use same handles (handle[',i,']:0x',HexStr(handle,8),
                      ' handle[',f,']:0x',HexStr(handle,8),')');

       rw_wunlock(g_port_lock);
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
      end;
    end;
    //
    params[i].handle:=g_port_table[port_id];
    params[i].ptr   :=param[i].ptr;
   end else
   begin
    rw_wunlock(g_port_lock);
    Exit(SCE_AUDIO_OUT_ERROR_NOT_OPENED);
   end;

  end;

  //output all
  g_audioout_interface.Outputs(@params,num);

 rw_wunlock(g_port_lock);

 Result:=0;
end;

function ps4_sceAudioOutGetSystemState(state:pSceAudioOutSystemState):Integer;
begin
 Result:=0;

 if (state=nil) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 end;

 if (_lazy_init=0) then
 begin
  Exit(SCE_AUDIO_OUT_ERROR_NOT_INIT);
 end;

 state^.loudness:=1;
end;

function Load_libSceAudioOut(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAudioOut');

 lib:=Result^.add_lib('libSceAudioOut');
 lib.set_proc($25F10F5D5C6116A0,@ps4_sceAudioOutInit);
 lib.set_proc($7A436FB13DB6AEC6,@ps4_sceAudioOutOpen);
 lib.set_proc($B35FFFB84F66045C,@ps4_sceAudioOutClose);
 lib.set_proc($1AB43DB3822B35A4,@ps4_sceAudioOutGetPortState);
 lib.set_proc($6FEB8057CF489711,@ps4_sceAudioOutSetVolume);
 lib.set_proc($C15C0F539D294B57,@ps4_sceAudioOutSetMixLevelPadSpk);
 lib.set_proc($3ED96DB37DBAA5DB,@ps4_sceAudioOutGetLastOutputTime);
 lib.set_proc($40E42D6DE0EAB13E,@ps4_sceAudioOutOutput);
 lib.set_proc($C373DD6924D2C061,@ps4_sceAudioOutOutputs);
 lib.set_proc($47985E9A828A203F,@ps4_sceAudioOutGetSystemState);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceAudioOut.prx',@Load_libSceAudioOut);

end.

