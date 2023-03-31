unit ps4_libSceVideoRecording;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 ps4_libkernel;

implementation

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

function ps4_sceVideoRecordingSetInfo(info:Integer;pInfo:Pointer;infoLen:size_t):Integer; SysV_ABI_CDecl;
var
 SDK_VERSION:DWORD;
 ret2:Integer;
begin
 Writeln('sceVideoRecordingSetInfo,info=',info,',infoLen=',infoLen);

 if (info<$9004) then
 begin
  case info of
   SCE_VIDEO_RECORDING_INFO_SUBTITLE   ,
   SCE_VIDEO_RECORDING_INFO_DESCRIPTION,
   SCE_VIDEO_RECORDING_INFO_COMMENTS   ,
   SCE_VIDEO_RECORDING_INFO_KEYWORDS   ,
   SCE_VIDEO_RECORDING_INFO_CHAPTER    ,
   SCE_VIDEO_RECORDING_INFO_COPYRIGHT  :Exit(0);

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
      SDK_VERSION:=0;
      ret2:=ps4_sceKernelGetCompiledSdkVersion(@SDK_VERSION);
      if ((ret2 < 0) or ($24fffff < SDK_VERSION)) and
         ((infoLen < 176) or
          (PSceVideoRecordingInfoUserMeta(pInfo)^.size < 176) or
          ((PSceVideoRecordingInfoUserMeta(pInfo)^.flags or $ffff7ffc)<>0)
         ) then Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);
      Exit(0);
     end;
   else
    Exit(SCE_VIDEO_RECORDING_ERROR_INVALID_VALUE);
  end;
 end;
end;

function ps4_sceVideoRecordingClose(discard:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceVideoRecordingClose,discard=',discard);
 Result:=0;
end;

function ps4_sceVideoRecordingGetStatus:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceVideoRecordingGetStatus');
 Result:=SCE_VIDEO_RECORDING_STATUS_NONE;
end;

function ps4_sceVideoRecordingStart:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceVideoRecordingStart');
 Result:=0;
end;

function ps4_sceVideoRecordingStop:Integer; SysV_ABI_CDecl;
begin
 Writeln('sceVideoRecordingStop');
 Result:=0;
end;

function ps4_sceVideoRecordingOpen2(pPath:Pchar;
                                    pParam:PSceVideoRecordingParam2;
                                    pHeap:Pointer;
                                    heapSize:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceVideoRecordingOpen2,pPath=',pPath);
 Result:=0;
end;

function ps4_sceVideoRecordingQueryMemSize2(pParam:PSceVideoRecordingParam2):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceVideoRecordingQueryMemSize2');
 Result:=1024;
end;

function Load_libSceVideoRecording(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceVideoRecording');

 lib^.set_proc($15CF2AC652883584,@ps4_sceVideoRecordingSetInfo);
 lib^.set_proc($287BE43D024330B9,@ps4_sceVideoRecordingClose);
 lib^.set_proc($7D9250CC52B81AFE,@ps4_sceVideoRecordingGetStatus);
 lib^.set_proc($B56A1EF48946021B,@ps4_sceVideoRecordingStart);
 lib^.set_proc($38E171ACC63E99F2,@ps4_sceVideoRecordingStop);
 lib^.set_proc($B36F1D6A5070A768,@ps4_sceVideoRecordingOpen2);
 lib^.set_proc($6C662463AAB76C8C,@ps4_sceVideoRecordingQueryMemSize2);
end;

initialization
 ps4_app.RegistredPreLoad('libSceVideoRecording.prx',@Load_libSceVideoRecording);

end.
