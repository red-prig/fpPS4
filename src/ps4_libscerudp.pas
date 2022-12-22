unit ps4_libSceRudp;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

uses
 ps4_time;

function ps4_sceRudpInit(pool:Pointer;poolSize:QWORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceRudpInit:',HexStr(pool),':',poolSize);
 Result:=0;
end;

function ps4_sceRudpSetEventHandler(handler:Pointer; //SceRudpEventHandler
                                    arg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceRudpSetEventHandler:',HexStr(handler),':',HexStr(arg));
 Result:=0;
end;

function ps4_sceRudpEnableInternalIOThread(stackSize,priority:DWORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceRudpEnableInternalIOThread:',stackSize,':',priority);
 Result:=0;
end;

function ps4_sceRudpNetFlush:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceRudpProcessEvents(timeout:QWORD // SceRudpUsec
                                 ):Integer; SysV_ABI_CDecl;
begin
 ps4_usleep(timeout);
 Result:=0;
end;

function Load_libSceRudp(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceRudp');

 lib^.set_proc($6A6B817C8FC041CE,@ps4_sceRudpInit);
 lib^.set_proc($4941157ACF20BE6C,@ps4_sceRudpSetEventHandler);
 lib^.set_proc($E8F04DA6C8326B1C,@ps4_sceRudpEnableInternalIOThread);
 lib^.set_proc($F8127DB2F0E68D8B,@ps4_sceRudpNetFlush);
 lib^.set_proc($F54F66D581F449C4,@ps4_sceRudpProcessEvents);
end;

initialization
 ps4_app.RegistredPreLoad('libSceRudp.prx',@Load_libSceRudp);

end.

