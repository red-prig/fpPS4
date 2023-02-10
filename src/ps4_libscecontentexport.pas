unit ps4_libSceContentExport;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

uses
 sys_types;

const
 SCE_CONTENT_EXPORT_ERROR_CANCELED                    =$809D3001;
 SCE_CONTENT_EXPORT_ERROR_NOTACCEPT                   =$809D3002;
 SCE_CONTENT_EXPORT_ERROR_BUSY                        =$809D3003;
 SCE_CONTENT_EXPORT_ERROR_NOINIT                      =$809D3004;
 SCE_CONTENT_EXPORT_ERROR_MULTIPLEINIT                =$809D3005;
 SCE_CONTENT_EXPORT_ERROR_NOMEM                       =$809D3006;
 SCE_CONTENT_EXPORT_ERROR_FILE_NOT_FOUND              =$809D3011;
 SCE_CONTENT_EXPORT_ERROR_NOT_SUPPORTED_FORMAT        =$809D3012;
 SCE_CONTENT_EXPORT_ERROR_INVALDPARAM                 =$809D3016;
 SCE_CONTENT_EXPORT_ERROR_DISKFULL                    =$809D3017;
 SCE_CONTENT_EXPORT_ERROR_EXECUTION_MAX               =$809D3018;
 SCE_CONTENT_EXPORT_ERROR_NOT_SUPPORT_FORMAT_THUMBNAIL=$809D3019;
 SCE_CONTENT_EXPORT_ERROR_FILES_IN_USE                =$809D301A;
 SCE_CONTENT_EXPORT_ERROR_DATA_PROVIDE_FUNCTION       =$809D301B;
 SCE_CONTENT_EXPORT_ERROR_NOT_IMPLEMENTED             =$809D3FFF;

type
 sceContentExportMalloc=function(size:size_t;userdata:Pointer):Pointer;
 sceContentExportFree=procedure(p:Pointer;userdata:Pointer);

 SceContentExportInitParam=packed record
  mallocfunc:sceContentExportMalloc;
  freefunc  :sceContentExportFree;
  userdata  :Pointer;
 end;
 PSceContentExportInitParam=^SceContentExportInitParam;

var
 isInitialized:Boolean=False;

function ps4_sceContentExportInit(initParams:PSceContentExportInitParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceContentExportInit');
 Result:=SCE_CONTENT_EXPORT_ERROR_NOT_IMPLEMENTED;
end;

function Load_libContentExportPlay(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceContentExport');

 lib^.set_proc($1731167989C01652,@ps4_sceContentExportInit);
end;

initialization
 ps4_app.RegistredPreLoad('libSceContentExport.prx',@Load_libContentExportPlay);

end.

