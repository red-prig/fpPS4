unit ps4_libSceMbus;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}
{$WARN 4110 off}

interface

uses
 subr_dynlib;

function ps4_sceMbusAddHandleByUserId(BusType:Integer;
                                      handle :Integer;
                                      userId :Integer;
                                      _type  :Integer;
                                      index  :Integer;
                                      unk2   :qword):Integer;

function ps4_sceMbusRemoveHandle(BusType:Integer;
                                 handle :Integer):Integer;

implementation

function ps4_sceMbusInit:Integer;
begin
 Result:=0;
end;

function ps4_sceMbusAddHandleByUserId(BusType:Integer;
                                      handle :Integer;
                                      userId :Integer;
                                      _type  :Integer;
                                      index  :Integer;
                                      unk2   :qword):Integer;
begin
 Result:=0;
end;

function ps4_sceMbusRemoveHandle(BusType:Integer;
                                 handle :Integer):Integer;
begin
 Result:=0;
end;

function Load_libSceMbus(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceMbus');

 lib:=Result^.add_lib('libSceMbus');

 lib.set_proc($C113D7306B643AAD,@ps4_sceMbusInit);
 lib.set_proc($0B00D5B063BA5374,@ps4_sceMbusAddHandleByUserId);
 lib.set_proc($FA56B4ED5EC4112D,@ps4_sceMbusRemoveHandle);
end;

var
 stub:t_int_file;

initialization
 reg_int_file(stub,'libSceMbus.prx',@Load_libSceMbus);

end.

