unit ps4_libSceSsl;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program;

type
 pSceSslData=^SceSslData;
 SceSslData=packed record
  ptr :PChar;
  size:QWORD;
 end;

 pSceSslCaCerts=^SceSslCaCerts;
 SceSslCaCerts=packed record
  certData   :pSceSslData;
  certDataNum:QWORD;
  pool       :Pointer;
 end;  

implementation

function ps4_sceSslInit(poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslInit:',poolSize);
 Result:=3;
end;

function ps4_sceSslTerm(ctxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslTerm:ctxId=',ctxId);
 Result:=0;
end;

function ps4_sceSslGetCaCerts(libsslCtxId:Integer;
                              caCerts:SceSslCaCerts):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslGetCaCerts:',libsslCtxId);
 Result:=0;
end;  

function ps4_sceSslFreeCaCerts(libsslCtxId:Integer;
                              caCerts:SceSslCaCerts):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceSslFreeCaCerts:',libsslCtxId);
 Result:=0;
end;   

function Load_libSceSsl(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceSsl');

 lib^.set_proc($85DA551140C55B7B,@ps4_sceSslInit);
 lib^.set_proc($D0AD7243A2EFFD87,@ps4_sceSslTerm);
 lib^.set_proc($4C37D0A8EFE031B6,@ps4_sceSslGetCaCerts);  
 lib^.set_proc($A88BCBB34818C62D,@ps4_sceSslFreeCaCerts);
end;

initialization
 //lower priority
 ps4_app.RegistredFinLoad('libSceSsl.prx' ,@Load_libSceSsl);
 ps4_app.RegistredFinLoad('libSceSsl2.prx',@Load_libSceSsl);

end.

