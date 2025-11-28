unit ps4_libSceAudioOut;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  subr_dynlib,
  audioout_interface,
  SDL3_audio_interface;

var
 FMainDevice      :RawByteString='';
 FHeadphoneDevice :RawByteString='';
 FControllerDevice:RawByteString='';
 FSpecialDevice   :RawByteString='';

type
 pSceAudioOutOutputParam=^SceAudioOutOutputParam;
 SceAudioOutOutputParam=packed record
  handle:Integer;
  align :Integer;
  ptr   :Pointer;
 end;

function ps4_sceAudioOutOpen(userId,_type,index:Integer;
                             len,freq,param:DWORD):Integer;

function ps4_sceAudioOutOutput(handle:Integer;ptr:Pointer):Integer;
function ps4_sceAudioOutOutputs(param:pSceAudioOutOutputParam;num:DWORD):Integer;

implementation

//MAIN------->/===\
//            |Mix|-->|Mastering|-->/===\
//BGM-------->\===/                 |Mix|-->[Main Device]
//                                  \===/
//                                    ^
//VOICE-------------------------------+----/==========\
//                                    |    |Headphones|
//PERSONAL----------------------------+----\==========/
//PADSPK------------------------------+---->[Controller]
//AUX------> Default/Special

uses
 sysutils,
 atomic,
 kern_mtx,
 kern_proc,
 ps4_libSceMbus;

var
 g_audioout_interface:TAbstractAudioOut=nil;

 g_port_table:array[0..24] of TAudioOutHandle;

 g_port_lock:mtx;

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
 SCE_AUDIO_VOLUME_0DB         =(1 shl SCE_AUDIO_VOLUME_SHIFT);
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

  g_audioout_interface:=Init_SDL3_interface();

  mtx_init(g_port_lock,'AudioOut');

  Result:=0;
 end else
 begin
  Result:=SCE_AUDIO_OUT_ERROR_ALREADY_INIT;
 end;

end;

function _out_open(userId,_type:Integer;
                   len,param:DWORD):Integer;
var
 port_id:Integer;
 aparams:TAudioParams;
 device_id:RawByteString;
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

 aparams:=Default(TAudioParams);

 case (param and SCE_AUDIO_OUT_PARAM_FORMAT_MASK) of
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_MONO:
   begin
    //S16
    aparams.channels:=1;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_STEREO:
   begin
    //S16
    aparams.channels:=2;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH:
   begin
    //S16
    aparams.channels:=8;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_MONO:
   begin
    //float
    aparams.is_float:=True;
    aparams.channels:=1;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_STEREO:
   begin
    //float
    aparams.is_float:=True;
    aparams.channels:=2;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH:
   begin
    //float
    aparams.is_float:=True;
    aparams.channels:=8;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_S16_8CH_STD:
   begin
    //S16
    aparams.is_std  :=True;
    aparams.channels:=8;
   end;
  SCE_AUDIO_OUT_PARAM_FORMAT_FLOAT_8CH_STD:
   begin
    //float
    aparams.is_float:=True;
    aparams.is_std  :=True;
    aparams.channels:=8;
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

 aparams.is_restricted :=(param and SCE_AUDIO_OUT_PARAM_ATTR_RESTRICTED )<>0;
 aparams.is_mix_to_main:=(param and SCE_AUDIO_OUT_PARAM_ATTR_MIX_TO_MAIN)<>0;

 case _type of
  SCE_AUDIO_OUT_PORT_TYPE_MAIN    :device_id:=FMainDevice;
  SCE_AUDIO_OUT_PORT_TYPE_BGM     :device_id:=FMainDevice;
  SCE_AUDIO_OUT_PORT_TYPE_VOICE   :device_id:=FHeadphoneDevice;
  SCE_AUDIO_OUT_PORT_TYPE_PERSONAL:device_id:=FHeadphoneDevice;
  SCE_AUDIO_OUT_PORT_TYPE_PADSPK  :device_id:=FControllerDevice;
  else
                                   device_id:=FSpecialDevice;
 end;

 if (device_id='[NULL]') then
 begin
  handle:=TAudioOutNull.Create;
 end else
 begin
  handle:=g_audioout_interface.Create;
 end;

 if (handle=nil) then
 begin
  Assert(false,'audioout_interface alloc failed');
  Exit(SCE_AUDIO_OUT_ERROR_OUT_OF_MEMORY);
 end;

 handle.f_userId   :=userId;
 handle.f_type     :=_type;
 handle.f_len      :=len;
 handle.f_param    :=aparams;

 if not handle.Open(device_id) then
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

 Assert(port_id<Length(g_port_table));

 //save handle
 g_port_table[port_id]:=handle;

 Result:=port_id;
end;

//int32_t SceUserServiceUserId;
function ps4_sceAudioOutOpen(userId,_type,index:Integer;
                             len,freq,param:DWORD):Integer;
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

 mtx_lock(g_port_lock);
  Result:=_out_open(userId,_type,len,param);
 mtx_unlock(g_port_lock);

 if (Result<0) then Exit;

 Result:=(DWORD(_type) shl 16) or DWORD(Result) or $20000000;

 ps4_sceMbusAddHandleByUserId(1,Result,userId,_type,index,0);
end;

function ps4_sceAudioOutOpenEx(userId,_type,index,unknow:Integer;
                               len,freq,param:DWORD):Integer;
begin
 Result:=ps4_sceAudioOutOpen(userId,_type,index,len,freq,param);
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

 mtx_lock(g_port_lock);
  Result:=_out_close(port_id);
 mtx_unlock(g_port_lock);
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

 mtx_lock(g_port_lock);

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

   state^.channel:=Byte(g_port_table[port_id].f_param.channels);

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

 mtx_unlock(g_port_lock);
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

 mtx_lock(g_port_lock);

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

 mtx_unlock(g_port_lock);

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

 mtx_lock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin
   g_port_table[port_id].SetMixLevelPadSpk(mixLevel);
  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 mtx_unlock(g_port_lock);
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

 mtx_lock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin
   outputTime^:=g_port_table[port_id].GetLastOutputTime;
  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

 mtx_unlock(g_port_lock);
end;

function ps4_sceAudioOutOutput(handle:Integer;ptr:Pointer):Integer;
var
 port_id  :Integer;
 port_type:Integer;
begin
 Result:=0;

 //Writeln('sceAudioOutOutput->');

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

 mtx_lock(g_port_lock);

  if (g_port_table[port_id]<>nil) then
  begin
   Result:=g_port_table[port_id].Output(ptr);
   if (Result<0) then Result:=SCE_AUDIO_OUT_ERROR_BUSY;
  end else
  begin
   Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
  end;

  if (Result=0) then
  begin
   Result:=g_port_table[port_id].f_len;
  end;

 mtx_unlock(g_port_lock);
end;

function ps4_sceAudioOutOutputs(param:pSceAudioOutOutputParam;num:DWORD):Integer;
label
 _unlock;
var
 handle   :Integer;
 port_id  :Integer;
 port_type:Integer;
 //
 f_len    :DWORD;
 //
 i,f:DWORD;
 //
 params:array[0..24] of TAudioOutParam;
begin
 Result:=0;

 //Writeln('sceAudioOutOutputs->');

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

 Result:=0;

 mtx_lock(g_port_lock);

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
       (p_proc.p_sdk_version >= $4500000) then
    begin
     for f:=0 to num-1 do
      if (f<>i) then
      if (handle=param[f].handle) then
      begin
       Writeln(stderr,'[AudioOut] use same handles (handle[',i,']:0x',HexStr(handle,8),
                      ' handle[',f,']:0x',HexStr(handle,8),')');

       Result:=SCE_AUDIO_OUT_ERROR_INVALID_PORT;
       goto _unlock;
      end;
    end;
    //
    if (i=0) then
    begin
     f_len:=g_port_table[port_id].f_len;
    end else
    if (f_len<>g_port_table[port_id].f_len) then
    begin
     Result:=SCE_AUDIO_OUT_ERROR_INVALID_SIZE;
     goto _unlock;
    end;
    //
    params[i].handle:=g_port_table[port_id];
    params[i].ptr   :=param[i].ptr;
   end else
   begin
    Result:=SCE_AUDIO_OUT_ERROR_NOT_OPENED;
    goto _unlock;
   end;

  end;

  //output all
  g_audioout_interface.Outputs(@params,num);

 _unlock:

 //Writeln('sceAudioOutOutputs<-');

 mtx_unlock(g_port_lock);

 if (Result=0) then
 begin
  Result:=f_len;
 end;
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

function ps4_sceAudioOutMasteringInit(flags:Integer):Integer;
begin
 if (flags<>0) then Exit(Integer($80260201));

 Result:=0;
end;

function ps4_sceAudioOutMasteringTerm():Integer;
begin
 Result:=0;
end;

type
 pSceAudioOutMasteringParamsHeader=Pointer;

function ps4_sceAudioOutMasteringSetParam(param:pSceAudioOutMasteringParamsHeader;flags:DWORD):Integer;
begin
 if (param=nil)      then Exit(Integer($80260201));
 if (DWORD(flags)>1) then Exit(Integer($80260205));

 Result:=0;
end;

type
 pSceAudioOutSystemInfoEx=^SceAudioOutSystemInfoEx;
 SceAudioOutSystemInfoEx=packed record
  MAX      :Byte;
  unknown2 :Byte;
  CONF_TYPE:Byte;
  unknown4 :Byte;
  unknown5 :Byte;
  unknown6 :Byte;
  unknown7 :Byte;
  unknown8 :Byte;
  flags    :QWORD;
  unknown10:Byte;
  unknown11:Byte;
  unknown12:Byte;
  unknown13:Byte;
  unknown14:Byte;
  unknown15:Byte;
  unknown16:Byte;
  unknown17:Byte;
 end;
 {$IF sizeof(SceAudioOutSystemInfoEx)<>24}{$STOP sizeof(SceAudioOutSystemInfoEx)<>24}{$ENDIF}

function ps4_sceAudioOutExGetSystemInfo(port    :Integer;
                                        unused  :Pointer;
                                        info    :pSceAudioOutSystemInfoEx;
                                        infoSize:Integer):Integer;
begin
 if (port<>0)  then Exit(SCE_AUDIO_OUT_ERROR_INVALID_PORT);
 if (info=nil) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 if (infoSize<>24) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_SIZE);

 info^:=Default(SceAudioOutSystemInfoEx);

 Result:=0;
end;

type
 pAudioOutExMode=^AudioOutExMode;
 AudioOutExMode=packed record
  unknown1:Byte;
  unknown2:Byte;
  unknown3:Byte;
  unknown4:Byte;
  NUM     :Byte;
  unknown5:Byte;
  FORMAT  :Byte;
  unknown7:Byte;
 end;

function ps4_sceAudioOutExSystemInfoIsSupportedAudioOutExMode(info    :pSceAudioOutSystemInfoEx;
                                                              ExMode  :pAudioOutExMode;
                                                              infoSize:Integer):Integer;
const
 NUM_BYTES:array[0..6] of Byte=(
  $02, //2->0->0x02
  $FF, //3->1->0xFF
  $FF, //4->2->0xFF
  $FF, //5->3->0xFF
  $06, //6->4->0x06
  $FF, //7->5->0xFF
  $08  //8->6->0x08
 );
var
 NUM:Byte; //uVar2
begin
 if (info=nil)     then Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);
 if (infoSize<>24) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_SIZE);
 if (ExMode=nil)   then Exit(SCE_AUDIO_OUT_ERROR_INVALID_POINTER);

 NUM:=ExMode^.NUM - 2;
 if (NUM < 7) then
 begin
  NUM:=NUM_BYTES[NUM];
 end else
 begin
  NUM:=$FF;
 end;

 if (info^.CONF_TYPE <> 2) then
 begin

  if (info^.CONF_TYPE <> 1) then
  begin
   //info^.CONF_TYPE -> 0

   if (info^.CONF_TYPE <> 0) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_CONF_TYPE);
   if (ExMode^.FORMAT  <> 0) then Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);

   if (NUM > info^.MAX) then
   begin
    Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
   end;

   Exit(0);
  end;

  //info^.CONF_TYPE -> 1

  case ExMode^.FORMAT of
   0:
     begin
      //
      if (NUM > info^.MAX) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
      end;

      Exit(3);
     end;
   1:
     begin
      //
      if ((info^.flags and 2) = 0) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
      end;

      Exit(0);
     end;
   2:
     begin
      //
      if ((info^.flags and 4) = 0) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
      end;

      Exit(2);
     end;
   3:
     begin
      //
      if ((info^.flags and 8) = 0) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
      end;

      Exit(1);
     end;
   4:
     begin
      //
      if ((info^.flags and $10) = 0) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
      end;

      Exit(0);
     end;
   5:
     begin
      //
      if ((info^.flags and $20) = 0) then
      begin
       Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
      end;

      Exit(1);
     end;
   else
     Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
  end;

 end;

 //info^.CONF_TYPE -> 2

 case ExMode^.FORMAT of
  0:
    begin
     //
     if (NUM > info^.MAX) then
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
     end;

     Exit(3);
    end;
  1:
    begin
     //
     if ((info^.flags and 2) = 0) then
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
     end;

     Exit(1);
    end;
  2:
    begin
     //
     if ((info^.flags and 4) = 0) then
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
     end;

     Exit(2);
    end;
  3:
    begin
     //
     if ((info^.flags and 8) = 0) then
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
     end;

     Exit(0);
    end;
  4:
    begin
     //
     if ((info^.flags and $10) = 0) then
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
     end;

     Exit(1);
    end;
  5:
    begin
     //
     if ((info^.flags and $20) = 0) then
     begin
      Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
     end;

     Exit(0);
    end;
  else
    Exit(SCE_AUDIO_OUT_ERROR_INVALID_FORMAT);
 end;

end;

function Load_libSceAudioOut(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAudioOut');

 lib:=Result^.add_lib('libSceAudioOut');
 lib.set_proc($25F10F5D5C6116A0,@ps4_sceAudioOutInit);
 lib.set_proc($7A436FB13DB6AEC6,@ps4_sceAudioOutOpen);
 lib.set_proc($A8BA522BBE655C8E,@ps4_sceAudioOutOpenEx);
 lib.set_proc($B35FFFB84F66045C,@ps4_sceAudioOutClose);
 lib.set_proc($1AB43DB3822B35A4,@ps4_sceAudioOutGetPortState);
 lib.set_proc($6FEB8057CF489711,@ps4_sceAudioOutSetVolume);
 lib.set_proc($C15C0F539D294B57,@ps4_sceAudioOutSetMixLevelPadSpk);
 lib.set_proc($3ED96DB37DBAA5DB,@ps4_sceAudioOutGetLastOutputTime);
 lib.set_proc($40E42D6DE0EAB13E,@ps4_sceAudioOutOutput);
 lib.set_proc($C373DD6924D2C061,@ps4_sceAudioOutOutputs);
 lib.set_proc($47985E9A828A203F,@ps4_sceAudioOutGetSystemState);

 lib.set_proc($C57E112DE81AADB8,@ps4_sceAudioOutMasteringInit);
 lib.set_proc($4555AD520A227F9A,@ps4_sceAudioOutMasteringTerm);
 lib.set_proc($E34E79C9A520DC46,@ps4_sceAudioOutMasteringSetParam);

 lib.set_proc($C196A4450B161A8B,@ps4_sceAudioOutExGetSystemInfo);
 lib.set_proc($5DC8FC553B67670D,@ps4_sceAudioOutExSystemInfoIsSupportedAudioOutExMode);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceAudioOut.prx',@Load_libSceAudioOut);

end.

