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

uses
 atomic,
 spinlock,
 sys_signal,
 ps4_libSceSystemService,
 playgo_chunk_loader;

const
 SCE_PLAYGO_HEAP_SIZE=2*1024*1024;
 SCE_PLAYGO_CHUNK_INDEX_MAX=100;
 SCE_PLAYGO_MAX_ETA_VALUE=9223372036854775807;

type
 pScePlayGoInitParams=^ScePlayGoInitParams;
 ScePlayGoInitParams=packed record
  bufAddr:Pointer;
  bufSize:DWORD;
  reserved:DWORD;
 end;

 pScePlayGoHandle=^ScePlayGoHandle;
 ScePlayGoHandle=Integer;

 pScePlayGoChunkId=^ScePlayGoChunkId;
 ScePlayGoChunkId=Word;

 pScePlayGoLocus=^ScePlayGoLocus;
 ScePlayGoLocus=Byte;

 pScePlayGoInstallSpeed=^ScePlayGoInstallSpeed;
 ScePlayGoInstallSpeed=Integer;

 pScePlayGoLanguageMask=^ScePlayGoLanguageMask;
 ScePlayGoLanguageMask=QWORD;

 pScePlayGoEta=^ScePlayGoEta;
 ScePlayGoEta=Int64;

 pScePlayGoProgress=^ScePlayGoProgress;
 ScePlayGoProgress=packed record
  progressSize:QWORD;
  totalSize:QWORD;
 end;

 pScePlayGoToDo=^ScePlayGoToDo;
 ScePlayGoToDo=packed record
  chunkId:ScePlayGoChunkId;
  locus:ScePlayGoLocus;
  reserved:Byte;
 end;

const
 //ScePlayGoInstallSpeed
 SCE_PLAYGO_INSTALL_SPEED_SUSPENDED=0;
 SCE_PLAYGO_INSTALL_SPEED_TRICKLE  =1;
 SCE_PLAYGO_INSTALL_SPEED_FULL     =2;

 //ScePlayGoLocusValue
 SCE_PLAYGO_LOCUS_NOT_DOWNLOADED =0;
 SCE_PLAYGO_LOCUS_LOCAL_SLOW     =2;
 SCE_PLAYGO_LOCUS_LOCAL_FAST     =3;

var
 //init
 playgo_init:Integer=0;

 playgo_file:TPlaygoFile=nil;

 //speed
 playgo_speed_lock:Pointer=nil;
 playgo_speed:ScePlayGoInstallSpeed=SCE_PLAYGO_INSTALL_SPEED_TRICKLE;
 playgo_speed_tick:QWORD=0;

 //lang
 playgo_lang:ScePlayGoLanguageMask=0;

function _is_not_init:Boolean; inline;
begin
 Result:=(playgo_init<>2);
end;

function scePlayGoConvertLanguage(systemLang:Integer):ScePlayGoLanguageMask; inline;
begin
 if (systemLang>=0) and (systemLang<48) then
 begin
  Result:=(1 shl (64-systemLang-1));
 end else
 begin
  Result:=0;
 end;
end;

function ps4_scePlayGoInitialize(
          initParam:pScePlayGoInitParams
          ):Integer; SysV_ABI_CDecl;
var
 systemLang:Integer;
begin
 if CAS(playgo_init,0,1) then
 begin

  //get system lang
  systemLang:=0;
  ps4_sceSystemServiceParamGetInt(SCE_SYSTEM_SERVICE_PARAM_ID_LANG,@systemLang);
  playgo_lang:=scePlayGoConvertLanguage(systemLang);

  //load playgo-chunk.dat
  _sig_lock;
   playgo_file:=LoadPlaygoFile('/app0/sce_sys/playgo-chunk.dat');
  _sig_unlock;

  //end init
  playgo_init:=2;
  Result:=0;
 end else
 begin
  Result:=SCE_PLAYGO_ERROR_ALREADY_INITIALIZED;
 end;
end;

function ps4_scePlayGoTerminate:Integer; SysV_ABI_CDecl;
begin
 if CAS(playgo_init,2,3) then
 begin

  _sig_lock;
   FreeAndNil(playgo_file);
  _sig_unlock;

  //end fini
  playgo_init:=0;
  Result:=0;
 end else
 begin
  Result:=SCE_PLAYGO_ERROR_NOT_INITIALIZED;
 end;
end;

function ps4_scePlayGoOpen(
          outHandle:pScePlayGoHandle;
          param:Pointer
          ):Integer; SysV_ABI_CDecl;
begin
 if (outHandle=nil)   then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (param<>nil)      then Exit(SCE_PLAYGO_ERROR_INVALID_ARGUMENT);
 if _is_not_init      then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);
 if (playgo_file=nil) then Exit(SCE_PLAYGO_ERROR_NOT_SUPPORT_PLAYGO);

 outHandle^:=333;
 Result:=0;
end;

function ps4_scePlayGoClose(handle:ScePlayGoHandle):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333) then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if _is_not_init  then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);
 Result:=0;
end;

//

function ps4_scePlayGoGetLocus(
          handle:ScePlayGoHandle;
          chunkIds:pScePlayGoChunkId;
          numberOfEntries:DWORD;
          outLoci:pScePlayGoLocus
          ):Integer; SysV_ABI_CDecl;
var
 i:DWORD;
begin
 if (handle<>333)                   then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (chunkIds=nil) or (outLoci=nil) then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (numberOfEntries=0)             then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);
 if _is_not_init                    then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);
 if (playgo_file=nil)               then Exit(SCE_PLAYGO_ERROR_BAD_CHUNK_ID);

 For i:=0 to numberOfEntries-1 do
 begin
  if (chunkIds[i]<Length(playgo_file.chunks)) then
  begin
   outLoci[i]:=SCE_PLAYGO_LOCUS_LOCAL_FAST;
  end else
  begin
   outLoci[i]:=SCE_PLAYGO_LOCUS_NOT_DOWNLOADED;
   Exit(SCE_PLAYGO_ERROR_BAD_CHUNK_ID);
  end;
 end;

 Result:=0;
end;

function ps4_scePlayGoSetToDoList(
          handle:ScePlayGoHandle;
          todoList:pScePlayGoToDo;
          numberOfEntries:DWORD
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333)       then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (todoList=nil)      then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (numberOfEntries=0) then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);
 if _is_not_init        then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 Result:=0;
end;

function ps4_scePlayGoGetToDoList(
          handle:ScePlayGoHandle;
          outTodoList:pScePlayGoToDo;
          numberOfEntries:DWORD;
          outEntries:PDWORD
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333)                         then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (outTodoList=nil) or (outEntries=nil) then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (numberOfEntries=0)                   then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);
 if _is_not_init                          then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 outEntries^:=0;
 Result:=0;
end;

function ps4_scePlayGoPrefetch(
          handle:ScePlayGoHandle;
          chunkIds:pScePlayGoChunkId;
          numberOfEntries:DWORD;
          minimumLocus:ScePlayGoLocus
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333)       then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (chunkIds=nil)      then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (numberOfEntries=0) then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);

 Case minimumLocus of
  SCE_PLAYGO_LOCUS_NOT_DOWNLOADED:;
  SCE_PLAYGO_LOCUS_LOCAL_SLOW    :;
  SCE_PLAYGO_LOCUS_LOCAL_FAST    :;
  else
   Exit(SCE_PLAYGO_ERROR_BAD_LOCUS);
 end;

 if _is_not_init        then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 Result:=0;
end;

//

//

function ps4_scePlayGoGetEta(
          handle:ScePlayGoHandle;
          chunkIds:pScePlayGoChunkId;
          numberOfEntries:DWORD;
          outEta:pScePlayGoEta
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333)                  then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (chunkIds=nil) or (outEta=nil) then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (numberOfEntries=0)            then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);

 outEta^:=0; //all is loaded
 Result:=0;
end;

function ps4_scePlayGoGetProgress(
          handle:ScePlayGoHandle;
          chunkIds:pScePlayGoChunkId;
          numberOfEntries:DWORD;
          outProgress:pScePlayGoProgress
          ):Integer; SysV_ABI_CDecl;
var
 i,chunk_id:DWORD;
 total_size:QWORD;
begin
 if (handle<>333)                       then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (chunkIds=nil) or (outProgress=nil) then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (numberOfEntries=0)                 then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);
 if (playgo_file=nil)                   then Exit(SCE_PLAYGO_ERROR_BAD_CHUNK_ID);

 outProgress^:=Default(ScePlayGoProgress);

 total_size:=0;
 For i:=0 to numberOfEntries-1 do
 begin
  chunk_id:=chunkIds[i];
  if (chunk_id<Length(playgo_file.chunks)) then
  begin
   total_size:=total_size+playgo_file.chunks[chunk_id].total_size;
  end else
  begin
   Exit(SCE_PLAYGO_ERROR_BAD_CHUNK_ID);
  end;
 end;

 outProgress^.progressSize:=total_size;
 outProgress^.totalSize   :=total_size;

 Result:=0;
end;

function ps4_scePlayGoGetChunkId(
          handle:ScePlayGoHandle;
          outChunkIdList:pScePlayGoChunkId;
          numberOfEntries:DWORD;
          outEntries:PDWORD
          ):Integer; SysV_ABI_CDecl;
var
 i:DWORD;
begin
 if (handle<>333)                                 then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (outEntries=nil)                              then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if (outChunkIdList<>nil) and (numberOfEntries=0) then Exit(SCE_PLAYGO_ERROR_BAD_SIZE);

 if (playgo_file=nil) then
 begin
  outEntries^:=0;
 end else
 if (outChunkIdList=nil) then
 begin
  outEntries^:=Length(playgo_file.chunks);
 end else
 begin
  if (numberOfEntries>Length(playgo_file.chunks)) then
  begin
   numberOfEntries:=Length(playgo_file.chunks);
  end;

  if (numberOfEntries<>0) then
  For i:=0 to numberOfEntries-1 do
  begin
   outChunkIdList[i]:=i;
  end;

  outEntries^:=numberOfEntries;
 end;

 Result:=0;
end;

//

function ps4_scePlayGoGetInstallSpeed(
          handle:ScePlayGoHandle;
          speed:pScePlayGoInstallSpeed
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333) then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (speed=nil)   then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if _is_not_init  then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 spin_lock(playgo_speed_lock);
  if (playgo_speed=0) then
  begin
   _sig_lock;
   if ((GetTickCount64-playgo_speed_tick)>30*1000) then //30sec
   begin
    playgo_speed:=SCE_PLAYGO_INSTALL_SPEED_TRICKLE;
   end;
   _sig_unlock;
  end;
  speed^:=playgo_speed;
 spin_unlock(playgo_speed_lock);

 Result:=0;
end;

function ps4_scePlayGoSetInstallSpeed(
          handle:ScePlayGoHandle;
          speed:ScePlayGoInstallSpeed
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333) then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if _is_not_init then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 Case speed of
  SCE_PLAYGO_INSTALL_SPEED_SUSPENDED:;
  SCE_PLAYGO_INSTALL_SPEED_TRICKLE  :;
  SCE_PLAYGO_INSTALL_SPEED_FULL     :;
  else
    Exit(SCE_PLAYGO_ERROR_INVALID_ARGUMENT);
 end;

 spin_lock(playgo_speed_lock);
  playgo_speed:=speed;
  _sig_lock;
   playgo_speed_tick:=GetTickCount64;
  _sig_unlock;
 spin_unlock(playgo_speed_lock);

 Result:=0;
end;

//

function ps4_scePlayGoSetLanguageMask(
          handle:ScePlayGoHandle;
          languageMask:ScePlayGoLanguageMask
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333) then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if _is_not_init then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 playgo_lang:=languageMask;
 Result:=0;
end;

function ps4_scePlayGoGetLanguageMask(
          handle:ScePlayGoHandle;
          outLanguageMask:pScePlayGoLanguageMask
          ):Integer; SysV_ABI_CDecl;
begin
 if (handle<>333) then Exit(SCE_PLAYGO_ERROR_BAD_HANDLE);
 if (outLanguageMask=nil) then Exit(SCE_PLAYGO_ERROR_BAD_POINTER);
 if _is_not_init then Exit(SCE_PLAYGO_ERROR_NOT_INITIALIZED);

 outLanguageMask^:=playgo_lang;
 Result:=0;
end;

function Load_libScePlayGo(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libScePlayGo');
 lib^.set_proc($B6CE8695938A46B1,@ps4_scePlayGoInitialize);
 lib^.set_proc($30F7B411E04633F1,@ps4_scePlayGoTerminate);
 lib^.set_proc($3351A66B5A1CAC61,@ps4_scePlayGoOpen);
 lib^.set_proc($51CA352347650E2F,@ps4_scePlayGoClose);
 lib^.set_proc($B962182C5924C2A9,@ps4_scePlayGoGetLocus);
 lib^.set_proc($8143C688E435B664,@ps4_scePlayGoSetToDoList);
 lib^.set_proc($367EF32B09C0E6AD,@ps4_scePlayGoGetToDoList);
 lib^.set_proc($FD0D7FBB56BBA748,@ps4_scePlayGoPrefetch);
 lib^.set_proc($BFA119FD859174CB,@ps4_scePlayGoGetEta);
 lib^.set_proc($FD125634C2B77C2F,@ps4_scePlayGoGetProgress);
 lib^.set_proc($EF77C5D4C154F210,@ps4_scePlayGoGetChunkId);
 lib^.set_proc($AEF0527D38A67A31,@ps4_scePlayGoGetInstallSpeed);
 lib^.set_proc($E0001C4D4F51DD73,@ps4_scePlayGoSetInstallSpeed);
 lib^.set_proc($2E8B0B9473A936A4,@ps4_scePlayGoSetLanguageMask);
 lib^.set_proc($DCE31B61905A6B9D,@ps4_scePlayGoGetLanguageMask);
end;

initialization
 ps4_app.RegistredPreLoad('libScePlayGo.prx',@Load_libScePlayGo);

end.

