unit ps4_libSceAudioIn;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

const
 SCE_AUDIO_IN_ERROR_NOT_OPENED=$80260109;

 SCE_AUDIO_IN_SILENT_STATE_DEVICE_NONE=$00000001;

function ps4_sceAudioInOpen(userID,busType,index,len,freq,param:Integer):Integer;
begin
 Result:=Integer(SCE_AUDIO_IN_ERROR_NOT_OPENED);
end;


function ps4_sceAudioInGetSilentState(handle:Integer):Integer;
begin
 Result:=SCE_AUDIO_IN_SILENT_STATE_DEVICE_NONE;
end;

function Load_libSceAudioIn(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceAudioIn');

 lib:=Result^.add_lib('libSceAudioIn');
 lib.set_proc($E4D13C4A373B542F,@ps4_sceAudioInOpen);
 lib.set_proc($068844010EC39541,@ps4_sceAudioInGetSilentState);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceAudioIn.prx',@Load_libSceAudioIn);

end.

