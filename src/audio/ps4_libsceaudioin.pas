unit ps4_libSceAudioIn;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

const
 SCE_AUDIO_IN_ERROR_FATAL          =-2144993024; //0x80260100
 SCE_AUDIO_IN_ERROR_INVALID_HANDLE =-2144993023; //0x80260101
 SCE_AUDIO_IN_ERROR_INVALID_SIZE   =-2144993022; //0x80260102
 SCE_AUDIO_IN_ERROR_INVALID_FREQ   =-2144993021; //0x80260103
 SCE_AUDIO_IN_ERROR_INVALID_TYPE   =-2144993020; //0x80260104
 SCE_AUDIO_IN_ERROR_INVALID_POINTER=-2144993019; //0x80260105
 SCE_AUDIO_IN_ERROR_INVALID_PARAM  =-2144993018; //0x80260106
 SCE_AUDIO_IN_ERROR_PORT_FULL      =-2144993017; //0x80260107
 SCE_AUDIO_IN_ERROR_OUT_OF_MEMORY  =-2144993016; //0x80260108
 SCE_AUDIO_IN_ERROR_NOT_OPENED     =-2144993015; //0x80260109
 SCE_AUDIO_IN_ERROR_BUSY           =-2144993014; //0x8026010A
 SCE_AUDIO_IN_ERROR_SYSTEM_MEMORY  =-2144993013; //0x8026010B
 SCE_AUDIO_IN_ERROR_SYSTEM_IPC     =-2144993012; //0x8026010C


 SCE_AUDIO_IN_SILENT_STATE_DEVICE_NONE=$00000001;

function ps4_sceAudioInOpen(userID,busType,index,len,freq,param:Integer):Integer;
begin
 //Result:=SCE_AUDIO_IN_ERROR_PORT_FULL;
 Result:=0;
end;

function ps4_sceAudioInOpenEx(userID,busType,index,unknow,len,freq,param:Integer):Integer;
begin
 //Result:=SCE_AUDIO_IN_ERROR_PORT_FULL;
 Result:=0;
end;

function ps4_sceAudioInInput(handle:Integer;dest:Pointer):Integer;
begin
 Result:=0;
end;

function ps4_sceAudioInGetSilentState(handle:Integer):Integer;
begin
 Result:=SCE_AUDIO_IN_SILENT_STATE_DEVICE_NONE;
end;

type
 p_audioin_status=^t_audioin_status;
 t_audioin_status=packed record
  field0_0x0 :Integer;
  field1_0x4 :Integer;
  field2_0x8 :qword;
  field3_0x10:Integer;
  field4_0x14:Integer;
  field5_0x18:Integer;
 end;

function ps4_sceAudioInGetHandleStatusInfo(handle:Integer;p_status:p_audioin_status):Integer;
begin
 if (p_status<>nil) then
 begin
  p_status^:=Default(t_audioin_status);
 end;
 Result:=0;
end;

function Load_libSceAudioIn(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAudioIn');

 lib:=Result^.add_lib('libSceAudioIn');
 lib.set_proc($E4D13C4A373B542F,@ps4_sceAudioInOpen);
 lib.set_proc($F83634ECDC096F4B,@ps4_sceAudioInOpenEx);
 lib.set_proc($2E8CC4394F3E6A73,@ps4_sceAudioInInput);
 lib.set_proc($068844010EC39541,@ps4_sceAudioInGetSilentState);
 lib.set_proc($3496A6D7F17B94D6,@ps4_sceAudioInGetHandleStatusInfo);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceAudioIn.prx',@Load_libSceAudioIn);

end.

