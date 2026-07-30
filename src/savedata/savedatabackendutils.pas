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
 md_time,
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
  //
  mtime      :QWORD;
 end;

 TMountManager=object
  const
   max=17; //16 for normal mount + 1 for ReadMemoryData
  //
  var
   MountSlots:array[0..max-1] of TMountSlot;
  //
  function  GetFreeSlotId(userId:Integer;titleId,dirName:pchar;Internal:Boolean;var slot_id:Integer):Integer;
  function  IsActiveMount(userId:Integer;titleId,dirName:pchar):Boolean;
  function  IsActiveMount(slot_id:Integer):Boolean;
  function  IsReadOnly   (slot_id:Integer):Boolean;
  procedure SetMount     (slot_id:Integer;const data:TMountSlot);
  function  GetMount     (slot_id:Integer):TMountSlot;
  procedure FreeMount    (slot_id:Integer);
  procedure SetMtime     (slot_id     :Integer;mtime:QWORD);
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

 TSetupMemory=packed record
  userId        :DWORD;
  slotId        :Byte;
  bufferNum     :Byte;
  paramSize     :WORD;
  memorySize    :DWORD;
  iconMemorySize:DWORD;
  //
  InitParams:packed record
   title        :array[0..SCE_SAVE_DATA_TITLE_MAXSIZE-1]    of AnsiChar;
   subTitle     :array[0..SCE_SAVE_DATA_SUBTITLE_MAXSIZE-1] of AnsiChar;
   detail       :array[0..SCE_SAVE_DATA_DETAIL_MAXSIZE-1]   of AnsiChar;
   userParam    :DWORD;
  end;
 end;

 PIconBufSize=^TIconBufSize;
 TIconBufSize=packed record //64
  max:QWORD;
  cur:QWORD;
  reserved:array[0..5] of QWORD;
 end;

 PSdMemoryBuffer=^TSdMemoryBuffer;
 TSdMemoryBuffer=object
  //shm
  Paddr:Pointer;
  Fsize:QWORD;
  //areas
  PmemoryData    :Pointer;
  FmemorySize    :QWORD;
  //
  PiconMemorySize:PIconBufSize;
  PiconData      :Pointer;
  //
  PParamData     :pSceSaveDataParam;
  //
  function  mmap_shm(size:QWORD):Integer;
  Procedure Free;
  function  CreateShm(memorySize    :DWORD;
                      iconMemorySize:DWORD;
                      paramSize     :DWORD):Integer;
 end;

 PSetupMemoryNode=^TSetupMemoryNode;
 TSetupMemoryNode=object
  //
  pLeft :PSetupMemoryNode;
  pRight:PSetupMemoryNode;
  //
  mtx   :mtx;
  //
  data  :TSetupMemory;
  //
  is_setup  :Boolean;
  is_writed :Boolean;
  FbufferId :Byte;
  job_count :DWORD;
  sd_buffers:array[0..1] of TSdMemoryBuffer;
  //
  function c(n1,n2:PSetupMemoryNode):Integer; static;
  //
  function CreateBuffers():Integer;
 end;

 TSetupMemorySplay=specialize TNodeSplay<TSetupMemoryNode>;

 TSetupMemoryManager=object(TSetupMemorySplay)
  mtx:mtx;
  Procedure Init;
  function  Setup(const data:TSetupMemory):PSetupMemoryNode;
  function  Get(userId,slotId:DWORD):PSetupMemoryNode;
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

function  SaveDataExists(const fs_src:RawByteString):Boolean;

function  CheckPng(data:Pointer;len:DWORD):Integer;
function  SaveIcon(const fs_src:RawByteString;data:Pointer;len:DWORD):Boolean;

function  SaveMemoryExists(const fs_src:RawByteString):Boolean;
function  SaveMemory(const fs_src:RawByteString;data:Pointer;len:DWORD):Boolean;

procedure load_mtime  (const fs_src:RawByteString;var mtime:QWORD);
procedure update_mtime(const fs_src:RawByteString;var mtime:QWORD);
procedure get_file_size(const fs_src:RawByteString;var size:QWORD);

implementation

///

function TMountManager.GetFreeSlotId(userId:Integer;titleId,dirName:pchar;Internal:Boolean;var slot_id:Integer):Integer;
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

 if not Internal then
 if (first_id=High(MountSlots)) then
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
 Result:=(MountSlots[slot_id].mountMode and SDMM_RDONLY)<>0;
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
 if (MountSlots[slot_id].active<>0) then
 begin
  MountSlots[slot_id]:=Default(TMountSlot);
 end;
end;

procedure TMountManager.SetMtime(slot_id:Integer;mtime:QWORD);
begin
 if (MountSlots[slot_id].active<>0) then
 begin
  MountSlots[slot_id].mtime:=mtime;
 end;
end;

procedure TMountManager.SetParam(slot_id     :Integer;
                                 paramType   :SceSaveDataParamType;
                                 paramBuf    :Pointer;
                                 paramBufSize:QWORD);
begin
 if (MountSlots[slot_id].active<>0) then
 begin
  MountSlots[slot_id].param_sfo.SetParam(paramType,paramBuf,paramBufSize);
 end;
end;

procedure TMountManager.GetParam(slot_id  :Integer;
                                 paramType:SceSaveDataParamType;
                                 paramBuf :Pointer;
                                 gotSize  :PDWORD);
begin
 if (MountSlots[slot_id].active<>0) then
 begin
  MountSlots[slot_id].param_sfo.GetParam(paramType,paramBuf,gotSize,MountSlots[slot_id].mtime);
 end;
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

function TSdMemoryBuffer.mmap_shm(size:QWORD):Integer;
begin
 //create psevdo shm

 Paddr:=AllocMem(size);
 Fsize:=size;

 if (Paddr=nil) then
 begin
  Result:=SCE_SAVE_DATA_ERROR_OUT_OF_MEMORY;
 end else
 begin
  Result:=0;
 end;

end;

Procedure TSdMemoryBuffer.Free;
begin
 if (Paddr<>nil) then
 begin
  FreeMem(Paddr,Fsize);
  self:=Default(TSdMemoryBuffer);
 end;
end;

function TSdMemoryBuffer.CreateShm(memorySize    :DWORD;
                                   iconMemorySize:DWORD;
                                   paramSize     :DWORD):Integer;
var
 mmapAddr      :Pointer;
 size:QWORD;
 err:Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;

 if (memorySize < $2000001) and
    (iconMemorySize < $1c801) and
    (paramSize < $531) then
 begin

  if (iconMemorySize=0) then
  begin
   size:=0;
  end else
  begin
   size:=iconMemorySize + 64;
  end;
  size:=size + paramSize + memorySize;

  err:=mmap_shm(size);
  if (err<>0) then Exit(SCE_SAVE_DATA_ERROR_OUT_OF_MEMORY);

  mmapAddr:=Paddr;

  PmemoryData:=mmapAddr;
  FmemorySize:=memorySize;

  if (iconMemorySize<>0) then
  begin
   PiconMemorySize:=(mmapAddr + memorySize);
   PiconData      :=(PiconMemorySize + 1);
   //
   PiconMemorySize^:=Default(TIconBufSize);
   PiconMemorySize^.max:=iconMemorySize;
  end;

  if (paramSize<>0) then
  begin
   mmapAddr:=PmemoryData;
   size    :=FmemorySize;
   if (iconMemorySize<>0) then
   begin
    mmapAddr:=PiconData;
    size    :=iconMemorySize;
   end;
   PParamData:=(mmapAddr + size);
  end;

  Result:=0;
 end;

end;

function TSetupMemoryNode.c(n1,n2:PSetupMemoryNode):Integer; static;
begin
 Result:=Integer(n1^.data.userId>n2^.data.userId)-Integer(n1^.data.userId<n2^.data.userId);
 if (Result<>0) then Exit;
 Result:=Integer(n1^.data.slotId>n2^.data.slotId)-Integer(n1^.data.slotId<n2^.data.slotId);
end;

function TSetupMemoryNode.CreateBuffers():Integer;
var
 i:Integer;
begin
 if is_setup then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);

 if (data.bufferNum<>1) and (data.bufferNum<>2) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 for i:=0 to data.bufferNum-1 do
 begin
  Result:=sd_buffers[i].CreateShm(data.memorySize,data.iconMemorySize,data.paramSize);
  if (Result<>0) then Exit;
 end;

 is_setup :=True;
 is_writed:=False;
 FbufferId:=0;

 mtx_init(mtx,'SetupMemory');

 Result:=0;
end;

Procedure TSetupMemoryManager.Init;
begin
 mtx_init(mtx,'SetupMemoryMtx');
end;

function TSetupMemoryManager.Setup(const data:TSetupMemory):PSetupMemoryNode;
var
 key :TSetupMemoryNode;
 node:PSetupMemoryNode;
begin
 key:=Default(TSetupMemoryNode);
 key.data:=data;

 mtx_lock(mtx);

  node:=Find(@key);

  if (node=nil) then
  begin
   node:=GetMem(sizeof(TSetupMemoryNode));
   node^:=key;
   Insert(node);
  end;

 mtx_unlock(mtx);

 Result:=node;
end;

function TSetupMemoryManager.Get(userId,slotId:DWORD):PSetupMemoryNode;
var
 key:TSetupMemoryNode;
begin
 key.data.userId:=userId;
 key.data.slotId:=slotId;

 mtx_lock(mtx);

  Result:=Find(@key);

 mtx_unlock(mtx);
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

function SaveDataExists(const fs_src:RawByteString):Boolean;
var
 fs_tmp:RawByteString;
begin
 fs_tmp:=fs_src+'_tmp_cp0';

 if DirectoryExists(fs_tmp) and (not DirectoryExists(fs_src)) then
 begin
  //try repair
  RenameFile(fs_tmp,fs_src);
 end;

 Result:=DirectoryExists(fs_src);
end;

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

function SaveMemoryExists(const fs_src:RawByteString):Boolean;
var
 fmem :RawByteString;
 fmem1:RawByteString;
begin
 fmem :=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/memory.dat');
 fmem1:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/memory1.dat');

 if FileExists(fmem1) and (not FileExists(fmem)) then
 begin
  //try repair
  RenameFile(fmem1,fmem);
 end;

 Result:=FileExists(fmem);
end;

function SaveMemory(const fs_src:RawByteString;data:Pointer;len:DWORD):Boolean;
var
 fmem :RawByteString;
 fmem0:RawByteString;
 fmem1:RawByteString;
begin
 Result:=False;

 fmem :=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/memory.dat');
 fmem0:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/memory0.dat');
 fmem1:=ExcludeTrailingPathDelimiter(fs_src)+unix_to_host('/memory1.dat');

 if FileExists(fmem0) then
 if not DeleteFile(fmem0) then
 begin
  Exit(False);
 end;

 if WriteToFile(fmem0,data,len)<>len then
 begin
  Exit(False);
 end;

 if FileExists(fmem1) then
 if not DeleteFile(fmem1) then
 begin
  Exit(False);
 end;

 if FileExists(fmem) then
 if not RenameFile(fmem,fmem1) then
 begin
  Exit(False);
 end;

 if not RenameFile(fmem0,fmem) then
 begin
  Exit(False);
 end;

 if FileExists(fmem1) then
 if not DeleteFile(fmem1) then
 begin
  Exit(False);
 end;
end;

procedure load_mtime(const fs_src:RawByteString;var mtime:QWORD);
var
 info:t_stat;
begin
 info:=Default(t_stat);

 md_stat(fs_src,@info);

 mtime:=info.st_mtim.tv_sec;
end;

procedure update_mtime(const fs_src:RawByteString;var mtime:QWORD);
var
 ts:array[0..1] of timespec;
begin
 mtime:=GetRtcTime;

 ts[0].tv_sec :=mtime;
 ts[0].tv_nsec:=0;

 ts[1].tv_sec :=mtime;
 ts[1].tv_nsec:=0;

 md_utimens(fs_src,@ts,2);
end;

procedure get_file_size(const fs_src:RawByteString;var size:QWORD);
var
 info:t_stat;
begin
 info:=Default(t_stat);

 md_stat(fs_src,@info);

 size:=info.st_size;
end;


end.

