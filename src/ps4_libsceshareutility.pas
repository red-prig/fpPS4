unit ps4_libSceShareUtility;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  subr_dynlib;

const
  SCE_SHARE_UTILITY_HEAP_SIZE=128*1024;

implementation

function ps4_sceShareUtilityInitialize(functionFlag:QWORD;
                                       heapSize:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceShareUtilityInitializeEx2(functionFlag:QWORD;
                                          heapSize:QWORD;
                                          threadPriority:Integer;
                                          affinityMask:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceShareUtilityAdvanceInitialize(functionFlag:QWORD;
                                              heapSize:QWORD;
                                              threadPriority:Integer;
                                              affinityMask:QWORD):Integer;
begin
 Result:=0;
end;

function ps4_sceShareUtilityAdvanceSetUploadContentData(const contentData:PChar;
                                                        contentDataLength:QWORD):Integer;
begin
 Result:=0;
end;

function Load_libSceShareUtility(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceShareUtility');

 lib:=Result^.add_lib('libSceShareUtility');
 lib.set_proc($265BFF958C449EF3,@ps4_sceShareUtilityInitialize);
 lib.set_proc($8FB0E56A50731E1F,@ps4_sceShareUtilityInitializeEx2);
 lib.set_proc($BB86C21A4AA1381E,@ps4_sceShareUtilityAdvanceInitialize);
 lib.set_proc($91AAE72616A474A8,@ps4_sceShareUtilityAdvanceSetUploadContentData);
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceShareUtility.prx',@Load_libSceShareUtility);

end.

