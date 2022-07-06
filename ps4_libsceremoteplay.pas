unit ps4_libSceRemoteplay;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

const
 SCE_REMOTEPLAY_HEAP_SIZE=6*1024;

function ps4_sceRemoteplayInitialize(pHeap:Pointer;heapSize:QWORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceRemoteplayInitialize:',heapSize);
 Result:=0;
end;

function ps4_sceRemoteplayApprove:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 SCE_REMOTEPLAY_CONNECTION_STATUS_DISCONNECT=0;
 SCE_REMOTEPLAY_CONNECTION_STATUS_CONNECT   =1;

function ps4_sceRemoteplayGetConnectionStatus(userId:Integer;pStatus:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pStatus<>nil) then
 begin
  pStatus^:=SCE_REMOTEPLAY_CONNECTION_STATUS_DISCONNECT;
 end;
 Result:=0;
end;

function Load_libSceRemoteplay(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceRemoteplay');
 lib^.set_proc($9354B082431238CF,@ps4_sceRemoteplayInitialize);
 lib^.set_proc($C50788AF24D7EDD6,@ps4_sceRemoteplayApprove);
 lib^.set_proc($8373CD8D8296AA74,@ps4_sceRemoteplayGetConnectionStatus);
end;

initialization
 ps4_app.RegistredPreLoad('libSceRemoteplay.prx',@Load_libSceRemoteplay);

end.

