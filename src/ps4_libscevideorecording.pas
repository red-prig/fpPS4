unit ps4_libSceVideoRecording;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

uses
 kern_proc;

const
 SCE_VIDEO_RECORDING_INFO_SUBTITLE        =$0002;
 SCE_VIDEO_RECORDING_INFO_DESCRIPTION     =$0006;
 SCE_VIDEO_RECORDING_INFO_COMMENTS        =$0007;
 SCE_VIDEO_RECORDING_INFO_KEYWORDS        =$0008;
 SCE_VIDEO_RECORDING_INFO_CHAPTER         =$000D;
 SCE_VIDEO_RECORDING_INFO_COPYRIGHT       =$0A01;
 SCE_VIDEO_RECORDING_INFO_PERMISSION_LEVEL=$A007;
 SCE_VIDEO_RECORDING_INFO_GUARD_AREA      =$A008;
 SCE_VIDEO_RECORDING_INFO_USER_META       =$A009;

 SCE_VIDEO_RECORDING_STATUS_NONE   =0;
 SCE_VIDEO_RECORDING_STATUS_RUNNING=1;
 SCE_VIDEO_RECORDING_STATUS_PAUSED =2;

 SCE_VIDEO_RECORDING_CHAPTER_CHANGE  =0;
 SCE_VIDEO_RECORDING_CHAPTER_PROHIBIT=1;

 SCE_VIDEO_RECORDING_USER_META_CONSTANT=$0001;
 SCE_VIDEO_RECORDING_USER_META_TIMELINE=$0002;
 SCE_VIDEO_RECORDING_USER_META_VARIABLE=$0003;
 SCE_VIDEO_RECORDING_USER_META_DEBUG   =$8000;

 SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE=-2136473597; //0x80a80003

type
 SceVideoRecordingInfoGuardArea=packed record
  x1,x2,y1,y2:Single;
 end;
 PSceVideoRecordingInfoGuardArea=^SceVideoRecordingInfoGuardArea;

 SceVideoRecordingInfoUserMeta=packed record
  size :size_t;
  flags:Integer;
  name :array[0..31 ] of Char;
  data :array[0..127] of Char;
  align:Integer;
 end;
 PSceVideoRecordingInfoUserMeta=^SceVideoRecordingInfoUserMeta;

 SceVideoRecordingParam2=packed record
  size          :size_t;
  affinityMask  :QWORD;
  threadPriority:Integer;
  ringSec       :Integer;
 end;
 PSceVideoRecordingParam2=^SceVideoRecordingParam2;

function ps4_sceVideoRecordingSetInfo(info:Integer;pInfo:Pointer;infoLen:size_t):Integer;
begin
 Writeln('sceVideoRecordingSetInfo,info=',info,',infoLen=',infoLen);

 if (info<$9004) then
 begin
  case info of
   SCE_VIDEO_RECORDING_INFO_SUBTITLE   ,
   SCE_VIDEO_RECORDING_INFO_DESCRIPTION,
   SCE_VIDEO_RECORDING_INFO_COMMENTS   ,
   SCE_VIDEO_RECORDING_INFO_KEYWORDS   ,
   SCE_VIDEO_RECORDING_INFO_COPYRIGHT  :Exit(infoLen);

   SCE_VIDEO_RECORDING_INFO_CHAPTER    :Exit(0);

    3,
    4,
    5,
    9,
   10,
   11,
   12:Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);

   $8001,
   $8002,
   $8003,
   $8004,
   $8005:Exit(0); //debug?

   else
    Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);
  end;
 end else
 begin
  case info of
   $A004,
   $A006,
   $A00C,
   $9004:Exit(0); //???

   $A005,
   $A00A,
   $A00B:Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);

   SCE_VIDEO_RECORDING_INFO_PERMISSION_LEVEL,
   SCE_VIDEO_RECORDING_INFO_GUARD_AREA      :Exit(0);

   SCE_VIDEO_RECORDING_INFO_USER_META:
     begin
      if (p_proc.p_sdk_version > $24fffff) and
         ((infoLen < 176) or
          (PSceVideoRecordingInfoUserMeta(pInfo)^.size < 176) or
          ((PSceVideoRecordingInfoUserMeta(pInfo)^.flags or $ffff7ffc)<>0)
         ) then
      begin
       Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);
      end;
      Exit(0);
     end;
   else
    Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);
  end;
 end;
end;

function ps4_sceVideoRecordingClose(discard:Integer):Integer;
begin
 Writeln('sceVideoRecordingClose,discard=',discard);
 Result:=0;
end;

function ps4_sceVideoRecordingGetStatus:Integer;
begin
 Writeln('sceVideoRecordingGetStatus');
 Result:=SCE_VIDEO_RECORDING_STATUS_NONE;
end;

function ps4_sceVideoRecordingStart:Integer;
begin
 Writeln('sceVideoRecordingStart');
 Result:=0;
end;

function ps4_sceVideoRecordingStop:Integer;
begin
 Writeln('sceVideoRecordingStop');
 Result:=0;
end;

function ps4_sceVideoRecordingOpen2(pPath:Pchar;
                                    pParam:PSceVideoRecordingParam2;
                                    pHeap:Pointer;
                                    heapSize:Integer):Integer;
begin
 Writeln('sceVideoRecordingOpen2,pPath=',pPath);
 Result:=0;
end;

function ps4_sceVideoRecordingQueryMemSize2(pParam:PSceVideoRecordingParam2):Integer;
begin
 Writeln('sceVideoRecordingQueryMemSize2');
 Result:=1024;
end;

function Load_libSceVideoRecording(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceVideoRecording');

 lib:=Result^.add_lib('libSceVideoRecording');
 lib.set_proc($15CF2AC652883584,@ps4_sceVideoRecordingSetInfo);
 lib.set_proc($287BE43D024330B9,@ps4_sceVideoRecordingClose);
 lib.set_proc($7D9250CC52B81AFE,@ps4_sceVideoRecordingGetStatus);
 lib.set_proc($B56A1EF48946021B,@ps4_sceVideoRecordingStart);
 lib.set_proc($38E171ACC63E99F2,@ps4_sceVideoRecordingStop);
 lib.set_proc($B36F1D6A5070A768,@ps4_sceVideoRecordingOpen2);
 lib.set_proc($6C662463AAB76C8C,@ps4_sceVideoRecordingQueryMemSize2);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceVideoRecording.prx',@Load_libSceVideoRecording);

end.
