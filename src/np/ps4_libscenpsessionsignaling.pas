unit ps4_libSceNpSessionSignaling;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_dynlib;

implementation

type
 pSceNpSessionSignalingInitParam=^SceNpSessionSignalingInitParam;
 SceNpSessionSignalingInitParam=packed record
  libhttp2CtxId  :Integer;
  _align1        :Integer;
  poolSize       :QWORD;
  cpuAffinityMask:QWORD;
  threadPriority :Integer;
  _align2        :Integer;
  threadStackSize:QWORD;
 end;

function ps4_sceNpSessionSignalingInitialize(params:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceNpSessionSignaling(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpSessionSignaling');

 lib:=Result^.add_lib('libSceNpSessionSignaling');
 lib.set_proc($CAC9B0E89FCFF009,@ps4_sceNpSessionSignalingInitialize);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpSessionSignaling.prx',@Load_libSceNpSessionSignaling);

end.

