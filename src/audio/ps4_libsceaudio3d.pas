unit ps4_libSceAudio3d;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

const
 //SceAudio3dRate
 SCE_AUDIO3D_RATE_48000=0;

 //SceAudio3dBufferMode
 SCE_AUDIO3D_BUFFER_NO_ADVANCE      =0;
 SCE_AUDIO3D_BUFFER_ADVANCE_NO_PUSH =1;
 SCE_AUDIO3D_BUFFER_ADVANCE_AND_PUSH=2;

 //SceAudio3dBlocking
 SCE_AUDIO3D_BLOCKING_ASYNC=0;
 SCE_AUDIO3D_BLOCKING_SYNC =1;

type
 pSceAudio3dAttributeId=^SceAudio3dAttributeId;
 SceAudio3dAttributeId=DWORD;

 pSceAudio3dAttribute=^SceAudio3dAttribute;
 SceAudio3dAttribute=packed record
  uiAttributeId:SceAudio3dAttributeId;
  _align:Integer;
  pValue:Pointer;
  szValue:QWORD;
 end;

 pSceAudio3dBlocking=^SceAudio3dBlocking;
 SceAudio3dBlocking=Integer;

 pSceAudio3dBufferMode=^SceAudio3dBufferMode;
 SceAudio3dBufferMode=Integer;

 pSceAudio3dObjectId=^SceAudio3dObjectId;
 SceAudio3dObjectId=DWORD;

 pSceAudio3dPortId=^SceAudio3dPortId;
 SceAudio3dPortId=DWORD;

 pSceAudio3dRate=^SceAudio3dRate;
 SceAudio3dRate=Integer;

 pSceAudio3dOpenParameters=^SceAudio3dOpenParameters;
 SceAudio3dOpenParameters=packed record
  szSizeThis   :QWORD;
  uiGranularity:DWORD;
  eRate        :SceAudio3dRate;
  uiMaxObjects :DWORD;
  uiQueueDepth :DWORD;
  eBufferMode  :SceAudio3dBufferMode;
  _align       :Integer;
  uiNumBeds    :DWORD;
 end;

implementation

function ps4_sceAudio3dInitialize(iReserved:Int64):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dPortOpen(iUserId:Integer;
                                const pParameters:pSceAudio3dOpenParameters;
                                pId:pSceAudio3dPortId):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dObjectReserve(uiPortId:SceAudio3dPortId;
                                     pId:pSceAudio3dObjectId):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dObjectSetAttributes(uiPortId:SceAudio3dPortId;
                                           uiObjectId:SceAudio3dObjectId;
                                           szNumAttributes:QWORD;
                                           const pAttributesArray:pSceAudio3dAttribute):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dPortSetAttribute(uiPortId:SceAudio3dPortId;
                                        uiAttributeId:SceAudio3dAttributeId;
                                        const pAttribute:Pointer;
                                        szAttribute:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dPortFlush(uiPortId:SceAudio3dPortId):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dPortAdvance(uiPortId:SceAudio3dPortId):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dPortPush(uiPortId:SceAudio3dPortId;
                                eBlocking:SceAudio3dBlocking):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dAudioOutOpen(uiPortId:SceAudio3dPortId;
                                    userId,_type,index:Integer;
                                    len,freq,param:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceAudio3dPortGetQueueLevel(uiPortId:SceAudio3dPortId;
                                         pQueueLevel,pQueueAvailable:pWord):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceAudio3d(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceAudio3d');
 lib^.set_proc($5260AF8D29AE648C,@ps4_sceAudio3dInitialize);
 lib^.set_proc($5DE0C32B4C495900,@ps4_sceAudio3dPortOpen);
 lib^.set_proc($8CEDAD79CE1D2763,@ps4_sceAudio3dObjectReserve);
 lib^.set_proc($E2EC8737DAB865E5,@ps4_sceAudio3dObjectSetAttributes);
 lib^.set_proc($62AF5B7D4434B898,@ps4_sceAudio3dPortSetAttribute);
 lib^.set_proc($64E1ABC562E04331,@ps4_sceAudio3dPortFlush);
 lib^.set_proc($970D2AADD4A366DF,@ps4_sceAudio3dPortAdvance);
 lib^.set_proc($54456167DA9DE196,@ps4_sceAudio3dPortPush);
 lib^.set_proc($B9C12C8BADACA13A,@ps4_sceAudio3dAudioOutOpen);
 lib^.set_proc($61A6836C3C0AA453,@ps4_sceAudio3dPortGetQueueLevel);
end;

initialization
 ps4_app.RegistredPreLoad('libSceAudio3d.prx',@Load_libSceAudio3d);

end.

