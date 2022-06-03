unit ps4_libScePlayGo;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

const
 SCE_PLAYGO_ERROR_FATAL              =-2135818238; // 0x80B20002
 SCE_PLAYGO_ERROR_NO_MEMORY          =-2135818237; // 0x80B20003
 SCE_PLAYGO_ERROR_INVALID_ARGUMENT   =-2135818236; // 0x80B20004
 SCE_PLAYGO_ERROR_NOT_INITIALIZED    =-2135818235; // 0x80B20005
 SCE_PLAYGO_ERROR_ALREADY_INITIALIZED=-2135818234; // 0x80B20006
 SCE_PLAYGO_ERROR_ALREADY_STARTED    =-2135818233; // 0x80B20007
 SCE_PLAYGO_ERROR_NOT_STARTED        =-2135818232; // 0x80B20008
 SCE_PLAYGO_ERROR_BAD_HANDLE         =-2135818231; // 0x80B20009
 SCE_PLAYGO_ERROR_BAD_POINTER        =-2135818230; // 0x80B2000A
 SCE_PLAYGO_ERROR_BAD_SIZE           =-2135818229; // 0x80B2000B
 SCE_PLAYGO_ERROR_BAD_CHUNK_ID       =-2135818228; // 0x80B2000C
 SCE_PLAYGO_ERROR_BAD_SPEED          =-2135818227; // 0x80B2000D
 SCE_PLAYGO_ERROR_NOT_SUPPORT_PLAYGO =-2135818226; // 0x80B2000E
 SCE_PLAYGO_ERROR_EPERM              =-2135818225; // 0x80B2000F
 SCE_PLAYGO_ERROR_BAD_LOCUS          =-2135818224; // 0x80B20010
 SCE_PLAYGO_ERROR_NEED_DATA_DISC     =-2135818223; // 0x80B20011

implementation

function ps4_scePlayGoInitialize(
          initParam:Pointer //ScePlayGoInitParams
          ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_scePlayGoOpen(
          outHandle:PInteger;
          param:Pointer
          ):Integer; SysV_ABI_CDecl;
begin
 if (outHandle=nil) then Exit(SCE_PLAYGO_ERROR_INVALID_ARGUMENT);
 outHandle^:=333;
 Result:=SCE_PLAYGO_ERROR_NOT_SUPPORT_PLAYGO;
end;

const
 //ScePlayGoLocusValue
 SCE_PLAYGO_LOCUS_NOT_DOWNLOADED =0;
 SCE_PLAYGO_LOCUS_LOCAL_SLOW     =2;
 SCE_PLAYGO_LOCUS_LOCAL_FAST     =3;

type
 ScePlayGoChunkId=Word;
 ScePlayGoLocus=Byte;

function ps4_scePlayGoGetLocus(
          handle:Integer;
          chunkIds:PWORD;  //ScePlayGoChunkId
          numberOfEntries:DWORD;
          outLoci:PByte    //ScePlayGoLocus
          ):Integer; SysV_ABI_CDecl;
var
 i:DWORD;
begin
 if (chunkIds=nil) or (outLoci=nil) then Exit(SCE_PLAYGO_ERROR_INVALID_ARGUMENT);
 if (numberOfEntries=0) then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);

 Writeln('scePlayGoGetLocus:',numberOfEntries,':',chunkIds[0]);

 For i:=0 to numberOfEntries-1 do
 begin
  outLoci[i]:=SCE_PLAYGO_LOCUS_NOT_DOWNLOADED;
 end;

 Result:=SCE_PLAYGO_ERROR_NOT_SUPPORT_PLAYGO;
end;

function Load_libScePlayGo(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libScePlayGo');
 lib^.set_proc($B6CE8695938A46B1,@ps4_scePlayGoInitialize);
 lib^.set_proc($3351A66B5A1CAC61,@ps4_scePlayGoOpen);
 lib^.set_proc($B962182C5924C2A9,@ps4_scePlayGoGetLocus);
end;

initialization
 ps4_app.RegistredPreLoad('libScePlayGo.prx',@Load_libScePlayGo);

end.

