unit vShaderManager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  murmurhash,
  g23tree,
  ps4_pssl,
  ps4_shader,

  vRegs2Vulkan,
  shader_dump,

  //ps4_program,

  vDevice,

  vShader,
  vShaderExt,

  SprvEmit,

  emit_post,
  emit_alloc,
  emit_print,
  emit_bin;

type
 TShaderFunc=packed object
  FLen :Ptruint;
  pData:PDWORD;
  function c(var a,b:TShaderFunc):Integer;
 end;

 PShaderDataKey=^TShaderDataKey;
 TShaderDataKey=packed object
  FStage:TvShaderStage;
  FLen  :Ptruint;
  pData :PDWORD;
  function  c(a,b:PShaderDataKey):Integer; static;
  Procedure Free;
 end;

 TShaderCodeCache=class
  key:TShaderDataKey;
  FShaders:array of TvShaderExt;
  function   AddShader(FDescSetId:Integer;Stream:TStream):TvShaderExt;
  Destructor Destroy; override;
 end;

 PPushConstAllocator=^TPushConstAllocator;
 TPushConstAllocator=object
  size  :DWORD;
  offset:DWORD;
  Procedure Init;
  function  GetAvailable:DWORD;
  procedure Apply(i:DWORD);
 end;

function FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderExt;
function FetchShaderGroup(F:PvShadersKey):TvShaderGroup;

implementation

uses
 kern_rwlock,
 kern_dmem;

type
 TShadersKeyCompare=object
  function c(a,b:PvShadersKey):Integer; static;
 end;

 _TShaderCacheSet=specialize T23treeSet<PShaderDataKey,TShaderDataKey>;
 TShaderCacheSet=object(_TShaderCacheSet)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

 _TShaderGroupSet=specialize T23treeSet<PvShadersKey,TShadersKeyCompare>;
 TShaderGroupSet=object(_TShaderGroupSet)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

var
 FShaderCacheSet:TShaderCacheSet;
 FShaderGroupSet:TShaderGroupSet;

Procedure TShaderCacheSet.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TShaderCacheSet.Unlock_wr;
begin
 rw_wunlock(lock);
end;

//

Procedure TShaderGroupSet.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TShaderGroupSet.Unlock_wr;
begin
 rw_wunlock(lock);
end;

function Max(a,b:PtrInt):PtrInt; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function TShaderDataKey.c(a,b:PShaderDataKey):Integer;
begin
 //1 FStage
 Result:=Integer(a^.FStage>b^.FStage)-Integer(a^.FStage<b^.FStage);
 if (Result<>0) then Exit;

 //2 FLen
 Result:=Integer((a^.FLen>b^.FLen) and (b^.FLen<>0))-Integer((a^.FLen<b^.FLen) and (a^.FLen<>0));
 if (Result<>0) then Exit;

 //3 pData
 Result:=CompareDWord(a^.pData^,b^.pData^,Max(a^.FLen,b^.FLen) div 4);
end;

function TShadersKeyCompare.c(a,b:PvShadersKey):Integer;
begin
 Result:=CompareByte(a^.FShaders,b^.FShaders,SizeOf(AvShaderStage));
end;

function TShaderCodeCache.AddShader(FDescSetId:Integer;Stream:TStream):TvShaderExt;
var
 i:Integer;
begin
 Result:=TvShaderExt.Create;
 Result.FDescSetId:=FDescSetId;
 Result.LoadFromStream(Stream);

 i:=Length(FShaders);
 SetLength(FShaders,i+1);
 FShaders[i]:=Result;
end;

Destructor TShaderCodeCache.Destroy;
begin
 Key.Free;
 inherited;
end;

function TShaderFunc.c(var a,b:TShaderFunc):Integer;
begin
 //1 FLen
 Result:=Integer((a.FLen>b.FLen) and (b.FLen<>0))-Integer((a.FLen<b.FLen) and (a.FLen<>0));
 if (Result<>0) then Exit;

 //2 pData
 Result:=CompareDWord(a.pData^,b.pData^,Max(a.FLen,b.FLen) div 4);
end;

Procedure TShaderDataKey.Free;
begin
 if (FLen<>0) and (pData<>nil) then
 begin
  FreeMem(pData);
 end;
end;

function _FindShaderCodeCache(const key:TShaderDataKey):TShaderCodeCache;
var
 i:TShaderCacheSet.Iterator;
begin
 Result:=nil;
 i:=FShaderCacheSet.find(@key);
 if (i.Item<>nil) then
 begin
  Result:=TShaderCodeCache(ptruint(i.Item^)-ptruint(@TShaderCodeCache(nil).key));
 end;
end;

function _FetchShaderCodeCache(const key:TShaderDataKey):TShaderCodeCache;
var
 t:TShaderCodeCache;
begin
 t:=_FindShaderCodeCache(key);

 if (t=nil) then
 begin
  t:=TShaderCodeCache.Create;
  t.key:=key;

  t.key.FStage:=key.FStage;
  t.key.FLen  :=_calc_shader_size(key.pData);
  t.key.pData :=AllocMem(t.key.FLen);

  Move(key.pData^,t.key.pData^,t.key.FLen);

  FShaderCacheSet.Insert(@t.key);
 end;

 Result:=t;
end;

Procedure DumpSpv(FStage:TvShaderStage;M:TMemoryStream);
var
 hash:QWORD;
 F:THandle;
 fname:RawByteString;
begin
 hash:=MurmurHash64A(M.Memory,M.Size,0);

 case FStage of
  vShaderStagePs:fname:='_ps_';
  vShaderStageVs:fname:='_vs_';
  vShaderStageCs:fname:='_cs_';
  else
    Exit;
 end;

 fname:='shader_dump\'+get_dev_progname+fname+HexStr(hash,16)+'.spv';

 if FileExists(fname) then Exit;

 CreateDir('shader_dump');

 F:=FileCreate(fname);
 FileWrite(F,M.Memory^,M.Size);
 FileClose(F);
end;

procedure TPushConstAllocator.Init;
begin
 Size:=limits.maxPushConstantsSize;
 offset:=0;
end;

function TPushConstAllocator.GetAvailable:DWORD;
begin
 Result:=0;
 if (offset<Size) then
 begin
  Result:=Size-offset;
 end;
end;

procedure TPushConstAllocator.Apply(i:DWORD);
begin
 offset:=offset+i;
end;

function ParseShader(FStage:TvShaderStage;pData:PDWORD;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TMemoryStream;
var
 SprvEmit:TSprvEmit;
begin
 Result:=nil;
 SprvEmit:=TSprvEmit.Create;

 case FStage of
  vShaderStagePs  :
  begin
   SprvEmit.InitPs(GPU_REGS.SH_REG^.SPI_SHADER_PGM_RSRC1_PS,
                   GPU_REGS.SH_REG^.SPI_SHADER_PGM_RSRC2_PS,
                   GPU_REGS.CX_REG^.SPI_PS_INPUT_ENA);

   SprvEmit.SetUserData(GPU_REGS.get_user_data(FStage));
  end;
  vShaderStageVs:
  begin
   SprvEmit.InitVs(GPU_REGS.SH_REG^.SPI_SHADER_PGM_RSRC1_VS,
                   GPU_REGS.SH_REG^.SPI_SHADER_PGM_RSRC2_VS,
                   GPU_REGS.CX_REG^.VGT_DMA_NUM_INSTANCES);

   SprvEmit.SetUserData(GPU_REGS.get_user_data(FStage));
  end;
  vShaderStageCs:
  begin
   SprvEmit.InitCs(GPU_REGS.SH_REG^.COMPUTE_PGM_RSRC1,
                   GPU_REGS.SH_REG^.COMPUTE_PGM_RSRC2,
                   GPU_REGS.SH_REG^.COMPUTE_NUM_THREAD_X,
                   GPU_REGS.SH_REG^.COMPUTE_NUM_THREAD_Y,
                   GPU_REGS.SH_REG^.COMPUTE_NUM_THREAD_Z);

   SprvEmit.SetUserData(GPU_REGS.get_user_data(FStage));
  end;

  else
    Exit;
 end;

 SprvEmit.Config.PrintAsm      :=False;
 SprvEmit.Config.UseVertexInput:=True;
 SprvEmit.Config.UseTexelBuffer:=False;
 SprvEmit.Config.UseOutput16   :=storageInputOutput16;

 SprvEmit.Config.maxUniformBufferRange          :=0; // $FFFF
 SprvEmit.Config.PushConstantsOffset            :=0; // 0
 SprvEmit.Config.minStorageBufferOffsetAlignment:=limits.minStorageBufferOffsetAlignment; // $10
 SprvEmit.Config.minUniformBufferOffsetAlignment:=limits.minUniformBufferOffsetAlignment; // $100

 SprvEmit.Config.maxPushConstantsSize:=0;
 if (pc<>nil) then
 begin
  SprvEmit.Config.PushConstantsOffset :=pc^.offset;
  SprvEmit.Config.maxPushConstantsSize:=pc^.GetAvailable;
 end;

 //SprvEmit.Config.UseVertexInput:=False;

 if (SprvEmit.ParseStage(pData)>1) then
 begin
  Writeln(StdErr,'Shader Parse Err');
  SprvEmit.Free;
  Exit;
 end;

 SprvEmit.PostStage;
 SprvEmit.AllocStage;

 Result:=TMemoryStream.Create;
 SprvEmit.SaveToStream(Result);

 SprvEmit.Free;

 //DumpSpv(FStage,Result);
end;

function _FetchShader(FStage:TvShaderStage;pData:PDWORD;FDescSetId:Integer;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderExt;
var
 F:TShaderDataKey;

 i:Integer;
 FShader:TvShaderExt;
 t:TShaderCodeCache;

 M:TMemoryStream;

 pUserData:Pointer;
 ch:TvBufOffsetChecker;


begin
 F:=Default(TShaderDataKey);
 F.FStage:=FStage;
 F.pData :=pData;

 t:=_FetchShaderCodeCache(F);

 if (Length(t.FShaders)<>0) then
 begin

  pUserData:=GPU_REGS.get_user_data(FStage);

  FShader:=nil;
  For i:=0 to High(t.FShaders) do
  begin
   FShader:=t.FShaders[i];
   ch.FResult:=True;
   FShader.EnumUnifLayout(@ch.AddAttr,FDescSetId,pUserData);
   if ch.FResult then //Checking offsets within a shader
   begin

    if (pc<>nil) then //push const allocator used?
    begin
     if (FShader.FPushConst.size<>0) then //push const used?
     begin
      if (FShader.FPushConst.offset=pc^.offset) and       //Checking offsets push constant
         (FShader.FPushConst.size<=pc^.GetAvailable) then //Is the remaining size sufficient?
      begin
       //found and apply with allocator
       pc^.Apply(FShader.FPushConst.size);
       Break;
      end else
      begin
       FShader:=nil; //reset with not found
      end;
     end;
    end else
    begin //push const allocator not used
     if (FShader.FPushConst.size<>0) then
     begin
      FShader:=nil; //reset with not found
     end else
     begin
      //found with no push const
      Break;
     end;
    end;

   end else
   begin
    FShader:=nil; //reset with not found
   end;
  end;

  if (FShader=nil) then //Rebuild with different parameters
  begin

   M:=ParseShader(FStage,pData,GPU_REGS,pc);
   Assert(M<>nil);

   FShader:=t.AddShader(FDescSetId,M);

   M.Free;

   if (FShader.FPushConst.size<>0) and (pc<>nil) then //push const used?
   begin
    FShader.FPushConst.offset:=pc^.offset;   //Save offset
    Dec(FShader.FPushConst.size,pc^.offset); //Move up size
    pc^.Apply(FShader.FPushConst.size);      //apply with allocator
   end;

  end;

 end else
 begin

  //first parse

  M:=ParseShader(FStage,pData,GPU_REGS,pc);
  Assert(M<>nil);

  FShader:=t.AddShader(FDescSetId,M);

  M.Free;

  if (FShader.FPushConst.size<>0) and (pc<>nil) then //push const used?
  begin
   FShader.FPushConst.offset:=pc^.offset;   //Save offset
   Dec(FShader.FPushConst.size,pc^.offset); //Move up size
   pc^.Apply(FShader.FPushConst.size);      //apply with allocator
  end;

 end;

 Result:=FShader;
end;

function FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderExt;
var
 pData0:PDWORD;
 pData1:PDWORD;
begin

 pData0:=GPU_REGS.get_code_addr(FStage);

 if (pData0=nil) then Exit(nil);
 //Assert(pData<>nil);

 pData1:=nil;
 if not get_dmem_ptr(pData0,@pData1,nil) then
 begin
  Assert(false,'get_dmem_ptr');
 end;

 FShaderCacheSet.Lock_wr;

 Result:=_FetchShader(FStage,pData1,FDescSetId,GPU_REGS,pc);

 FShaderCacheSet.Unlock_wr;
end;

//

function _FindShaderGroup(F:PvShadersKey):TvShaderGroup;
var
 i:TShaderGroupSet.Iterator;
begin
 Result:=nil;
 i:=FShaderGroupSet.find(F);
 if (i.Item<>nil) then
 begin
  Result:=TvShaderGroup(ptruint(i.Item^)-ptruint(@TvShaderGroup(nil).FKey));
 end;
end;

function _FetchShaderGroup(F:PvShadersKey):TvShaderGroup;
var
 t:TvShaderGroup;
begin
 Result:=nil;

 t:=_FindShaderGroup(F);

 if (t=nil) then
 begin

  t:=TvShaderGroup.Create;
  t.FKey:=F^;

  if not t.Compile then
  begin
   FreeAndNil(t);
  end else
  begin
   FShaderGroupSet.Insert(@t.FKey);
  end;
 end;

 Result:=t;
end;

function FetchShaderGroup(F:PvShadersKey):TvShaderGroup;
begin
 Result:=nil;
 if (F=nil) then Exit;

 FShaderGroupSet.Lock_wr;

 Result:=_FetchShaderGroup(F);

 FShaderGroupSet.Unlock_wr;
end;


end.

