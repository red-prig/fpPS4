unit ps4_libSceNpInGameMessage;

{$mode objfpc}{$H+}

interface

uses
  windows,
  ps4_libSceNpCommon,
  ps4_program,
  np_error;


const

  NP_IN_GAME_MESSAGE_POOL_SIZE=(16 * 1024);

  SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX=512;


type
 pSceNpInGameMessageData=^SceNpInGameMessageData;
 SceNpInGameMessageData=packed record
  data:array[0..SCE_NP_IN_GAME_MESSAGE_DATA_SIZE_MAX-1] of AnsiChar;
  dataSize:QWORD;
 end;






 implementation

function ps4_SceNpInGameMessageInitialize(poolSize:QWORD;pOption:Pointer):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;


function ps4_SceNpInGameMessageTerminate(libCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
  Result:=0;
end;

function Load_libSceNpInGameMessage(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpInGameMessage');
 lib^.set_proc($1858555294666C71,@ps4_sceNpInGameMessageInitialize);
 ///lib^.set_proc($7D041F3FCEC8EE1B,@ps4_SceNpInGameMessageTerminate);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpInGameMessage.prx',@Load_libSceNpInGameMessage);

end.
