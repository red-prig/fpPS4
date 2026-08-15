unit SaveDataBackendSfo;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 game_mount,
 param_sfo,
 SceSaveData;

type
 t_sfo_param_params_s=packed record
  version          :DWORD;                // =0
  user_id          :DWORD;                //
  psid_hmac        :array[0..31] of Byte; //
  counter_id       :DWORD;                //  =1  2  3
  title_id_1       :array[0..15] of Char; //   |  |  |
  title_id_2       :array[0..15] of Char; //   |  |  |
  RETAIL_counter1  :DWORD;                // <-/  |  |
  DEX_TOOL_counter2:DWORD;                // <----/  |
  DEX_TOOL_counter3:DWORD;                // <-------/
  fake_owner       :DWORD;                // =0/1
  flags            :DWORD;                // =4
  archive_time1    :QWORD;
  archive_time2    :QWORD;
  corrupt_flag     :DWORD;                // =0/1
  padding          :array[0..907] of Byte;
 end;
 {$IF sizeof(t_sfo_param_params_s)<>$400}{$STOP sizeof(t_sfo_param_params_s)<>$400}{$ENDIF}

{
the flags parameter is cumulative

app0_dir_id                        flags
----------------------------------+------
unknow(error?)                    | 0x01
disc                       (0) -> | 0x08
PkgSpCore                  (1)    |
                PS_CLOUD:true  -> | 0x02
                PS_CLOUD:false -> | 0x04
debug                      (2) -> | 0x10
debug hostapp/app data/app (3) -> | 0x20
}

type
 p_savedata_sfo_values=^t_savedata_sfo_values;
 t_savedata_sfo_values=packed object
  //
  CATEGORY           :array[0..3] of Char;
  FORMAT             :array[0..3] of Char;
  TITLE_ID           :array[0..11] of Char;
  ATTRIBUTE          :DWORD;
  SAVEDATA_BLOCKS    :QWORD;
  PARAMS             :t_sfo_param_params_s;
  MAINTITLE          :array[0..127] of Char;
  SUBTITLE           :array[0..127] of Char;
  DETAIL             :array[0..1023] of Char;
  SAVEDATA_LIST_PARAM:DWORD;
  SAVEDATA_DIRECTORY :array[0..31] of Char;
  ACCOUNT_ID         :QWORD;
  //
  Procedure New(GameMountConfig:TGameMountConfig;userId:Integer;titleId,dirName:pchar;blocks:QWORD;systemLang:DWORD);
  function  SaveToFile(const fname:RawByteString):Boolean;
  function  LoadFromFile(const fname:RawByteString):Boolean;
  function  Verif(userId:Integer;dirName:pchar):Boolean;
  procedure SetParam(paramType   :SceSaveDataParamType;
                     paramBuf    :Pointer;
                     paramBufSize:QWORD);
  procedure GetParam(paramType   :SceSaveDataParamType;
                     paramBuf    :Pointer;
                     gotSize     :PDWORD;
                     mtime       :QWORD);
 end;

function GET_MAINTITLE_DEFAULT(systemLang:DWORD):PChar;

implementation

function GET_MAINTITLE_DEFAULT(systemLang:DWORD):PChar;
begin
 if (systemLang>High(MAINTITLE_DEFAULT)) then systemLang:=1;

 Result:=MAINTITLE_DEFAULT[systemLang];
end;

Procedure t_savedata_sfo_values.New(GameMountConfig:TGameMountConfig;userId:Integer;titleId,dirName:pchar;blocks:QWORD;systemLang:DWORD);
begin
 if (titleId[0]=#0) then
 begin
  titleId:=@GameMountConfig.TitleId;
 end;

 Self:=Default(t_savedata_sfo_values);
 //
 CATEGORY         :='sd';
 FORMAT           :='obs';
 ACCOUNT_ID       :=$6F6C6C6F706122E7;
 SAVEDATA_BLOCKS  :=blocks;
 params.user_id   :=userId;
 params.counter_id:=1;
 params.flags     :=4;

 strlcopy(@MAINTITLE,GET_MAINTITLE_DEFAULT(systemLang),SCE_SAVE_DATA_TITLE_MAXSIZE);

 strlcopy(@TITLE_ID         ,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);
 strlcopy(@params.title_id_1,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);
 strlcopy(@params.title_id_2,titleId,SCE_SAVE_DATA_TITLE_ID_DATA_SIZE);

 strlcopy(@SAVEDATA_DIRECTORY,dirName,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE);
end;

function t_savedata_sfo_values.SaveToFile(const fname:RawByteString):Boolean;
var
 F:TParamSfoFileLoader;
begin
 F.New(192,136,2380);

 F.AddNameValue('ACCOUNT_ID'         ,@ACCOUNT_ID         ,SFO_FORMAT_BLOB  ,sizeof(ACCOUNT_ID)                  ,sizeof(ACCOUNT_ID));
 F.AddNameValue('ATTRIBUTE'          ,@ATTRIBUTE          ,SFO_FORMAT_UINT32,sizeof(ATTRIBUTE)                   ,sizeof(ATTRIBUTE));
 F.AddNameValue('CATEGORY'           ,@CATEGORY           ,SFO_FORMAT_STRING,strlen(pchar(@CATEGORY ))+1         ,sizeof(CATEGORY));
 F.AddNameValue('DETAIL'             ,@DETAIL             ,SFO_FORMAT_STRING,strlen(pchar(@DETAIL   ))+1         ,sizeof(DETAIL   ));
 F.AddNameValue('FORMAT'             ,@FORMAT             ,SFO_FORMAT_STRING,strlen(pchar(@FORMAT   ))+1         ,sizeof(FORMAT   ));
 F.AddNameValue('MAINTITLE'          ,@MAINTITLE          ,SFO_FORMAT_STRING,strlen(pchar(@MAINTITLE))+1         ,sizeof(MAINTITLE));
 F.AddNameValue('PARAMS'             ,@PARAMS             ,SFO_FORMAT_BLOB  ,sizeof(PARAMS)                      ,sizeof(PARAMS));
 F.AddNameValue('SAVEDATA_BLOCKS'    ,@SAVEDATA_BLOCKS    ,SFO_FORMAT_BLOB  ,sizeof(SAVEDATA_BLOCKS)             ,sizeof(SAVEDATA_BLOCKS));
 F.AddNameValue('SAVEDATA_DIRECTORY' ,@SAVEDATA_DIRECTORY ,SFO_FORMAT_STRING,strlen(pchar(@SAVEDATA_DIRECTORY))+1,sizeof(SAVEDATA_DIRECTORY));
 F.AddNameValue('SAVEDATA_LIST_PARAM',@SAVEDATA_LIST_PARAM,SFO_FORMAT_UINT32,sizeof(SAVEDATA_LIST_PARAM)         ,sizeof(SAVEDATA_LIST_PARAM));
 F.AddNameValue('SUBTITLE'           ,@SUBTITLE           ,SFO_FORMAT_STRING,strlen(pchar(@SUBTITLE))+1          ,sizeof(SUBTITLE));
 F.AddNameValue('TITLE_ID'           ,@TITLE_ID           ,SFO_FORMAT_STRING,strlen(pchar(@TITLE_ID))+1          ,sizeof(TITLE_ID));

 Result:=F.save(fname);
 F.Free;
end;

procedure _on_load_sfo(userdata:Pointer;name,value:pchar;format:WORD;size,max_size,i:DWORD);

 procedure copy_value(dst:Pointer;field_format:WORD;max_field_size:DWORD); inline;
 begin
  if (field_format=format) then
  begin
   if (size>max_field_size) then size:=max_field_size;
   Move(value^,dst^,size);
  end;
 end;

begin
 with p_savedata_sfo_values(userdata)^ do
 begin
  case RawByteString(name) of
   'ACCOUNT_ID'         :copy_value(@ACCOUNT_ID         ,SFO_FORMAT_BLOB  ,sizeof(ACCOUNT_ID));
   'ATTRIBUTE'          :copy_value(@ATTRIBUTE          ,SFO_FORMAT_UINT32,sizeof(ATTRIBUTE));
   'CATEGORY'           :copy_value(@CATEGORY           ,SFO_FORMAT_STRING,sizeof(CATEGORY));
   'DETAIL'             :copy_value(@DETAIL             ,SFO_FORMAT_STRING,sizeof(DETAIL   ));
   'FORMAT'             :copy_value(@FORMAT             ,SFO_FORMAT_STRING,sizeof(FORMAT   ));
   'MAINTITLE'          :copy_value(@MAINTITLE          ,SFO_FORMAT_STRING,sizeof(MAINTITLE));
   'PARAMS'             :copy_value(@PARAMS             ,SFO_FORMAT_BLOB  ,sizeof(PARAMS));
   'SAVEDATA_BLOCKS'    :copy_value(@SAVEDATA_BLOCKS    ,SFO_FORMAT_BLOB  ,sizeof(SAVEDATA_BLOCKS));
   'SAVEDATA_DIRECTORY' :copy_value(@SAVEDATA_DIRECTORY ,SFO_FORMAT_STRING,sizeof(SAVEDATA_DIRECTORY));
   'SAVEDATA_LIST_PARAM':copy_value(@SAVEDATA_LIST_PARAM,SFO_FORMAT_UINT32,sizeof(SAVEDATA_LIST_PARAM));
   'SUBTITLE'           :copy_value(@SUBTITLE           ,SFO_FORMAT_STRING,sizeof(SUBTITLE));
   'TITLE_ID'           :copy_value(@TITLE_ID           ,SFO_FORMAT_STRING,sizeof(TITLE_ID));
   else;
  end;
 end;
end;

function t_savedata_sfo_values.LoadFromFile(const fname:RawByteString):Boolean;
var
 F:TParamSfoFileLoader;
begin
 Result:=False;

 if not F.open(fname) then
 begin
  Exit;
 end;

 if not F.parse() then
 begin
  F.Free;
  Exit;
 end;

 Self:=Default(t_savedata_sfo_values);
 F.ForAll(@_on_load_sfo,@Self);

 F.Free;

 Result:=True;
end;

function t_savedata_sfo_values.Verif(userId:Integer;dirName:pchar):Boolean;
begin
 Result:=False;

 if (CATEGORY<>'sd') or
    (FORMAT<>'obs') or
    (SAVEDATA_BLOCKS<96) then
 begin
  Exit;
 end;

 if CompareChar0(SAVEDATA_DIRECTORY,dirName^,SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)<>0 then
 begin
  Exit;
 end;

 if (PARAMS.version<>0) or
    (PARAMS.counter_id<>1) then
 begin
  Exit;
 end;

 if (PARAMS.corrupt_flag<>0) then
 begin
  Exit;
 end;

 //sfo.PARAMS.user_id

 Result:=True;
end;

function Min(a, b: QWORD): QWORD; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

procedure t_savedata_sfo_values.SetParam(paramType   :SceSaveDataParamType;
                                         paramBuf    :Pointer;
                                         paramBufSize:QWORD);
begin

 case paramType of
  SCE_SAVE_DATA_PARAM_TYPE_ALL:
    if (paramBufSize > 1327) then
    begin
     strlcopy(@MAINTITLE,@pSceSaveDataParam(paramBuf)^.title   ,sizeof(MAINTITLE));
     strlcopy(@SUBTITLE ,@pSceSaveDataParam(paramBuf)^.subTitle,sizeof(SUBTITLE));
     strlcopy(@DETAIL   ,@pSceSaveDataParam(paramBuf)^.detail  ,sizeof(DETAIL));
     //
     SAVEDATA_LIST_PARAM:=pSceSaveDataParam(paramBuf)^.userParam;
    end;
  SCE_SAVE_DATA_PARAM_TYPE_TITLE:
    begin
     strlcopy(@MAINTITLE,paramBuf,min(sizeof(MAINTITLE),paramBufSize));
    end;
  SCE_SAVE_DATA_PARAM_TYPE_SUB_TITLE:
    begin
     strlcopy(@SUBTITLE,paramBuf,min(sizeof(SUBTITLE),paramBufSize));
    end;
  SCE_SAVE_DATA_PARAM_TYPE_DETAIL:
    begin
     strlcopy(@DETAIL,paramBuf,min(sizeof(DETAIL),paramBufSize));
    end;
  SCE_SAVE_DATA_PARAM_TYPE_USER_PARAM:
    if (paramBufSize=4) then
    begin
     SAVEDATA_LIST_PARAM:=PDWORD(paramBuf)^;
    end;
  else;
 end;

end;

procedure t_savedata_sfo_values.GetParam(paramType:SceSaveDataParamType;
                                         paramBuf :Pointer;
                                         gotSize  :PDWORD;
                                         mtime    :QWORD);
begin

 case paramType of
  SCE_SAVE_DATA_PARAM_TYPE_ALL:
    begin
     pSceSaveDataParam(paramBuf)^:=Default(SceSaveDataParam);
     pSceSaveDataParam(paramBuf)^.title    :=MAINTITLE;
     pSceSaveDataParam(paramBuf)^.subTitle :=SUBTITLE;
     pSceSaveDataParam(paramBuf)^.detail   :=DETAIL;
     pSceSaveDataParam(paramBuf)^.userParam:=SAVEDATA_LIST_PARAM;
     pSceSaveDataParam(paramBuf)^.mtime    :=mtime;
     gotSize^:=sizeof(SceSaveDataParam);
    end;
  SCE_SAVE_DATA_PARAM_TYPE_TITLE:
    begin
     strlcopy(pchar(paramBuf),@MAINTITLE,sizeof(MAINTITLE));
     gotSize^:=sizeof(MAINTITLE);
    end;
  SCE_SAVE_DATA_PARAM_TYPE_SUB_TITLE:
    begin
     strlcopy(pchar(paramBuf),@SUBTITLE,sizeof(SUBTITLE));
     gotSize^:=sizeof(SUBTITLE);
    end;
  SCE_SAVE_DATA_PARAM_TYPE_DETAIL:
    begin
     strlcopy(pchar(paramBuf),@DETAIL,sizeof(DETAIL));
     gotSize^:=sizeof(DETAIL);
    end;
  SCE_SAVE_DATA_PARAM_TYPE_USER_PARAM:
    begin
     PDWORD(paramBuf)^:=SAVEDATA_LIST_PARAM;
     gotSize^:=sizeof(SAVEDATA_LIST_PARAM);
    end;
  SCE_SAVE_DATA_PARAM_TYPE_MTIME:
    begin
     PQWORD(paramBuf)^:=mtime;
     gotSize^:=sizeof(mtime);
    end;
  else;
 end;

end;



end.

