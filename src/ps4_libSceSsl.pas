unit ps4_libSceSsl;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

implementation

function ps4_sceSslInit(poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslInit:',poolSize);
 Result:=3;
end;

function Load_libSceSsl(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSsl');

 lib^.set_proc($85DA551140C55B7B,@ps4_sceSslInit);
end;

initialization
 //lower priority
 ps4_app.RegistredFinLoad('libSceSsl.prx',@Load_libSceSsl);

end.

