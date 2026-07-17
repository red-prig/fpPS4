unit SaveDataBackendUtils;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 fpimage,
 fpreadpng,
 CharStream,
 kern_mtx,
 md_file,
 g_node_splay,
 game_mount,
 ps4_libSceUserService,
 SceSaveData,
 SaveDataBackendSfo;

type
 TMountSlot=packed record
  active     :WORD;
  mountMode  :WORD; //SceSaveDataMountMode
  userId     :SceUserServiceUserId;
  titleId    :SceSaveDataTitleId;
  dirName    :SceSaveDataDirName;
  fingerprint:SceSaveDataFingerprint;
  max_blocks :SceSaveDataBlocks;
  //
  param_sfo  :t_savedata_sfo_values;
 end;

 TMountManager=object
  //
  MountSlots:array[0..15] of TMountSlot;
  //
  function  GetFreeSlotId(userId:Integer;titleId,dirName:pchar;var slot_id:Integer):Integer;
  function  IsActiveMount(userId:Integer;titleId,dirName:pchar):Boolean;
  function  IsActiveMount(slot_id:Integer):Boolean;
  function  IsReadOnly   (slot_id:Integer):Boolean;
  procedure SetMount     (slot_id:Integer;const data:TMountSlot);
  function  GetMount     (slot_id:Integer):TMountSlot;
  procedure FreeMount    (slot_id:Integer);
  procedure SetParam     (slot_id     :Integer;
                          paramType   :SceSaveDataParamType;
                          paramBuf    :Pointer;
                          paramBufSize:QWORD);
  procedure GetParam     (slot_id     :Integer;
                          paramType   :SceSaveDataParamType;
                          paramBuf    :Pointer;
                          gotSize     :PDWORD);
 end;

 PLockDirNode=^TLockDirNode;
 TLockDirNode=object
  //
  pLeft :PLockDirNode;
  pRight:PLockDirNode;
  //
  fs_src:RawByteString;
  //
  function c(n1,n2:PLockDirNode):Integer; static;
 end;

 TLockDirSplay=specialize TNodeSplay<TLockDirNode>;

 TLockDirManager=object(TLockDirSplay)
  mtx:mtx;
  Procedure Init;
  function  LockDir  (const fs_src:RawByteString):Boolean;
  function  UnLockDir(const fs_src:RawByteString):Boolean;
 end;

///

 TEventQueue=object
  mtx:mtx;
  rd_pos:Byte;
  wr_pos:Byte;
  data:array[0..19] of SceSaveDataEvent;
  procedure Init;
  procedure Push(const event:SceSaveDataEvent);
  procedure Push(_type,errorCode,userId:Integer;titleId:pSceSaveDataTitleId;dirName:pSceSaveDataDirName);
  function  Pop (var event:SceSaveDataEvent):Boolean;
 end;

///

type
 p_input_buf=^t_input_buf;
 t_input_buf=packed record
  slot:DWORD;
  size:DWORD;
  data:record end;
 end;

 p_output_buf=^t_output_buf;
 t_output_buf=packed record
  result:DWORD;
  size  :DWORD;
  data  :record end;
 end;

function CheckPng(data:Pointer;len:DWORD):Integer;
function SaveIcon(const fs_src:RawByteString;data:Pointer;len:DWORD):Boolean;

implementation

///

function TMountManager.GetFreeSlotId(userId:Integer;titleId,dirName:pchar;var slot_id:Integer):Integer;
var
 i,first_id:Integer;
begin

 first_id:=-1;

 For i:=0 to High(MountSlots) do
 if (MountSlots[i].active<>0) then
 begin

  if (MountSlots[i].userId=userId) then
  if (strncasecmp(@MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@MountSlots[i].dirName.data,
                  dirName,
                  SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)=0) then
  begin
   Exit(SCE_SAVE_DATA_ERROR_BUSY);
  end;

 end else
 if (first_id=-1) then
 begin
  first_id:=i;
 end;

 if (first_id=-1) then
 begin
  Exit(SCE_SAVE_DATA_ERROR_MOUNT_FULL);
 end;

 slot_id:=first_id;
 Result:=0;
end;

function TMountManager.IsActiveMount(userId:Integer;titleId,dirName:pchar):Boolean;
var
 i:Integer;
begin
 Result:=False;

 For i:=0 to High(MountSlots) do
 if (MountSlots[i].active<>0) then
 begin

  if (MountSlots[i].userId=userId) then
  if (strncasecmp(@MountSlots[i].titleId.data,
                  titleId,
                  SCE_SAVE_DATA_TITLE_ID_DATA_SIZE)=0) then
  if (strncasecmp(@MountSlots[i].dirName.data,
                  dirName,
                  SCE_SAVE_DATA_DIRNAME_DATA_MAXSIZE)=0) then
  begin
   Exit(True);
  end;

 end;

end;

function TMountManager.IsActiveMount(slot_id:Integer):Boolean;
begin
 Result:=MountSlots[slot_id].active<>0;
end;

function TMountManager.IsReadOnly(slot_id:Integer):Boolean;
begin
 Result:=(MountSlots[slot_id].mountMode and SDM_RDONLY)<>0;
end;

procedure TMountManager.SetMount(slot_id:Integer;const data:TMountSlot);
begin
 MountSlots[slot_id]:=data;
end;

function TMountManager.GetMount(slot_id:Integer):TMountSlot;
begin
 Result:=MountSlots[slot_id];
end;

procedure TMountManager.FreeMount(slot_id:Integer);
begin
 MountSlots[slot_id]:=Default(TMountSlot);
end;

procedure TMountManager.SetParam(slot_id     :Integer;
                                 paramType   :SceSaveDataParamType;
                                 paramBuf    :Pointer;
                                 paramBufSize:QWORD);
begin
 MountSlots[slot_id].param_sfo.SetParam(paramType,paramBuf,paramBufSize);
end;

procedure TMountManager.GetParam(slot_id  :Integer;
                                 paramType:SceSaveDataParamType;
                                 paramBuf :Pointer;
                                 gotSize  :PDWORD);
begin
 MountSlots[slot_id].param_sfo.GetParam(paramType,paramBuf,gotSize);
 //TODO: SCE_SAVE_DATA_PARAM_TYPE_MTIME
end;

///

function TLockDirNode.c(n1,n2:PLockDirNode):Integer;
begin
 Result:=CompareText(n1^.fs_src,n2^.fs_src);
end;

Procedure TLockDirManager.Init;
begin
 mtx_init(mtx,'LockDirMtx');
end;

function TLockDirManager.LockDir(const fs_src:RawByteString):Boolean;
var
 node:PLockDirNode;
begin
 node:=AllocMem(sizeof(TLockDirNode));
 node^.fs_src:=fs_src;

 mtx_lock(mtx);

  Result:=Insert(node);

 mtx_unlock(mtx);

 if Result then
 begin
  //
 end else
 begin
  Finalize(node^);
  FreeMem(node);
 end;
end;

function TLockDirManager.UnLockDir(const fs_src:RawByteString):Boolean;
var
 data:TLockDirNode;
 node:PLockDirNode;
begin
 data.fs_src:=fs_src;

 mtx_lock(mtx);

  node:=Find(@data);
  Result:=Delete(node);

 mtx_unlock(mtx);

 if (node<>nil) then
 begin
  Finalize(node^);
  FreeMem(node);
 end;
end;

///

procedure TEventQueue.Init;
begin
 mtx_init(mtx,'TEventQueue');
 rd_pos:=0;
 wr_pos:=0;
end;

procedure TEventQueue.Push(const event:SceSaveDataEvent);
begin
 mtx_lock(mtx);

 data[wr_pos]:=event;

 wr_pos:=(wr_pos+1) mod Length(data);

 if (wr_pos=rd_pos) then
 begin
  rd_pos:=(rd_pos+1) mod Length(data);
 end;

 mtx_unlock(mtx);
end;

procedure TEventQueue.Push(_type,errorCode,userId:Integer;titleId:pSceSaveDataTitleId;dirName:pSceSaveDataDirName);
var
 event:SceSaveDataEvent;
begin
 event:=Default(SceSaveDataEvent);
 event._type    :=_type;
 event.errorCode:=errorCode;
 event.userId   :=userId;
 event.titleId  :=titleId^;
 event.dirName  :=dirName^;
 //
 Push(event);
end;

function TEventQueue.Pop(var event:SceSaveDataEvent):Boolean;
begin
 mtx_lock(mtx);

 if (wr_pos=rd_pos) then
 begin
  Result:=False;
 end else
 begin
  event:=data[rd_pos];

  rd_pos:=(rd_pos+1) mod Length(data);

  Result:=True;
 end;

 mtx_unlock(mtx);
end;

///

function CheckPng(data:Pointer;len:DWORD):Integer;
var
 Mem:TPCharStream;
 Img:TFPMemoryImage;
 Reader:TFPReaderPNG;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;

 Mem:=TPCharStream.Create(data,len);
 Img:=TFPMemoryImage.Create(0,0);
 Reader:=TFPReaderPNG.Create;

 try
   Img.LoadFromStream(Mem, Reader);

   if (Img.Width=228) then
   if (Img.Height=128) then
   if (Reader.ColorType=2) then //is RGB
   if (Reader.BitDepth=8) then
   begin
    Result:=0;
   end;

 finally
   Reader.Free;
   Img.Free;
   Mem.Free;
 end;
end;

function SaveIcon(const fs_src:RawByteString;data:Pointer;len:DWORD):Boolean;
var
 fdir :RawByteString;
 ficon:RawByteString;
 fpng0:RawByteString;
 fpng1:RawByteString;
begin
 Result:=False;

 fdir:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys');
 Result:=ForceDirectories(fdir);
 if not Result then Exit;

 ficon:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/icon0.png');
 fpng0:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/sce_icon0png0');
 fpng1:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/sce_sys/sce_icon0png1');

 if FileExists(fpng0) then
 if not DeleteFile(fpng0) then
 begin
  Exit(False);
 end;

 if FileExists(fpng1) then
 if not DeleteFile(fpng1) then
 begin
  Exit(False);
 end;

 if WriteToFile(fpng0,data,len)<>len then
 begin
  Exit(False);
 end;

 if FileExists(ficon) then
 if not RenameFile(ficon,fpng1) then
 begin
  Exit(False);
 end;

 if not RenameFile(fpng0,ficon) then
 begin
  Exit(False);
 end;

 Result:=TruncFile(fpng1,$1c800);
end;


end.

