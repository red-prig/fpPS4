unit ps4_libSceCommonDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 core_serialization,
 subr_dynlib;

Const
 //SceCommonDialogStatus
 SCE_COMMON_DIALOG_STATUS_NONE         =0;
 SCE_COMMON_DIALOG_STATUS_INITIALIZED  =1;
 SCE_COMMON_DIALOG_STATUS_RUNNING      =2;
 SCE_COMMON_DIALOG_STATUS_FINISHED     =3;

 //SceCommonDialogResult
 SCE_COMMON_DIALOG_RESULT_OK           =0;
 SCE_COMMON_DIALOG_RESULT_USER_CANCELED=1;

 SCE_COMMON_DIALOG_MAGIC_NUMBER=$C0D1A109;

type
 SceCommonDialogBaseParam=packed record
  size    :QWORD;
  reserved:array[0..35] of Byte;
  magic   :DWORD;
 end;

const
 SCE_COMMON_DIALOG_ERROR_NOT_SYSTEM_INITIALIZED    =-2135425023; // 0x80B80001
 SCE_COMMON_DIALOG_ERROR_ALREADY_SYSTEM_INITIALIZED=-2135425022; // 0x80B80002
 SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED           =-2135425021; // 0x80B80003
 SCE_COMMON_DIALOG_ERROR_ALREADY_INITIALIZED       =-2135425020; // 0x80B80004
 SCE_COMMON_DIALOG_ERROR_NOT_FINISHED              =-2135425019; // 0x80B80005
 SCE_COMMON_DIALOG_ERROR_INVALID_STATE             =-2135425018; // 0x80B80006
 SCE_COMMON_DIALOG_ERROR_RESULT_NONE               =-2135425017; // 0x80B80007
 SCE_COMMON_DIALOG_ERROR_BUSY                      =-2135425016; // 0x80B80008
 SCE_COMMON_DIALOG_ERROR_OUT_OF_MEMORY             =-2135425015; // 0x80B80009
 SCE_COMMON_DIALOG_ERROR_PARAM_INVALID             =-2135425014; // 0x80B8000A
 SCE_COMMON_DIALOG_ERROR_NOT_RUNNING               =-2135425013; // 0x80B8000B
 SCE_COMMON_DIALOG_ERROR_ALREADY_CLOSE             =-2135425012; // 0x80B8000C
 SCE_COMMON_DIALOG_ERROR_ARG_NULL                  =-2135425011; // 0x80B8000D
 SCE_COMMON_DIALOG_ERROR_UNEXPECTED_FATAL          =-2135425010; // 0x80B8000E
 SCE_COMMON_DIALOG_ERROR_NOT_SUPPORTED             =-2135425009; // 0x80B8000F
 SCE_COMMON_DIALOG_ERROR_INHIBIT_SHAREPLAY_CLIENT  =-2135425008; // 0x80B80010

implementation

var
 g_common_dialog_init:Byte=0;
 g_common_dialog_mtx :mtx;

function ps4_sceCommonDialogInitialize():Integer;
begin
 Result:=0;
 Writeln('sceCommonDialogInitialize');

 if (g_common_dialog_init=0) then
 begin
  g_common_dialog_init:=1;
  mtx_lock(g_common_dialog_mtx);
  //DialogInitialize
  mtx_unlock(g_common_dialog_mtx);
 end else
 begin
  Result:=SCE_COMMON_DIALOG_ERROR_ALREADY_SYSTEM_INITIALIZED;
 end;

end;

function ps4_sceCommonDialogIsUsed():Boolean;
begin
 Result:=True;
end;

//

function Load_libSceCommonDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceCommonDialog');

 lib:=Result^.add_lib('libSceCommonDialog');
 lib.set_proc($BA85292C6364CA09,@ps4_sceCommonDialogInitialize);
 lib.set_proc($050DED7B2D099903,@ps4_sceCommonDialogIsUsed);

 mtx_init(g_common_dialog_mtx,'g_common_dialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceCommonDialog.prx',@Load_libSceCommonDialog);

end.

