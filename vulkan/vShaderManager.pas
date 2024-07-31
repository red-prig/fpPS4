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

  si_ci_vi_merged_registers,

  vDevice,

  vShader,
  vShaderExt,

  SprvEmit,
  emit_bin;

const
 STAGE_NAME:array[TvShaderStage] of PChar=(
  'Ls',
  'Hs',
  'Es',
  'Gs',
  'Vs',
  'Ps',
  'Cs');

type
 PShaderDataKey=^TShaderDataKey;
 TShaderDataKey=packed object
  FStage:TvShaderStage;
  FLen  :Ptruint;
  pData :PDWORD;
  FHash :QWORD;
  function  c(a,b:PShaderDataKey):Integer; static;
  Procedure SetData(Stage:TvShaderStage;Src:Pointer);
  Procedure Free;
 end;

 TShaderCodeCache=class
  key:TShaderDataKey;
  FShaderAliases:array of TvShaderExt;
  function   AddShader(FDescSetId:Integer;Stream:TStream;pUserData:Pointer):TvShaderExt;
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

{
 TShaderCacheSet
 ---------------
   [TShaderCodeCache]
   (
    [TShaderDataKey]
      FShaderAliases [TvShaderExt]
      ---------------

      ---------------
      ...............
      ---------------
   )
 ---------------
 ...............
 ---------------
}

function FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderExt;
function FetchShaderGroup(F:PvShadersKey):TvShaderGroup;
function FetchShaderGroupRT(var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderGroup;
function FetchShaderGroupCS(var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderGroup;

function GetDumpSpvName(FStage:TvShaderStage;hash:QWORD):RawByteString;

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

function TShaderCodeCache.AddShader(FDescSetId:Integer;Stream:TStream;pUserData:Pointer):TvShaderExt;
var
 i:Integer;
begin
 Result:=TvShaderExt.Create;
 Result.FHash_gcn:=key.FHash;

 Result.FDescSetId:=FDescSetId; //set before loading
 Result.LoadFromStream(Stream);
 Result.PreloadShaderFuncs(pUserData);

 i:=Length(FShaderAliases);
 SetLength(FShaderAliases,i+1);
 FShaderAliases[i]:=Result;
end;

Destructor TShaderCodeCache.Destroy;
begin
 Key.Free;
 inherited;
end;

Procedure TShaderDataKey.SetData(Stage:TvShaderStage;Src:Pointer);
begin
 Free;

 FStage:=Stage;
 FLen  :=_calc_shader_size(Src);
 pData :=AllocMem(FLen);

 Move(Src^,pData^,FLen);

 FHash:=MurmurHash64A(pData,FLen,0);
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

function _FetchShaderCodeCache(FStage:TvShaderStage;pData:PDWORD):TShaderCodeCache;
var
 key:TShaderDataKey;
 t:TShaderCodeCache;
begin

 key:=Default(TShaderDataKey);
 key.FStage:=FStage;

 if not get_dmem_ptr(pData,@key.pData,nil) then
 begin
  Assert(false,'_FetchShaderCodeCache');
 end;

 t:=_FindShaderCodeCache(key);

 if (t=nil) then
 begin
  t:=TShaderCodeCache.Create;
  t.key:=key;

  t.key.SetData(key.FStage,key.pData);

  FShaderCacheSet.Insert(@t.key);
 end;

 Result:=t;
end;

function GetDumpSpvName(FStage:TvShaderStage;hash:QWORD):RawByteString;
begin
 Result:=get_dev_progname+'_'+LowerCase(STAGE_NAME[FStage])+'_'+HexStr(hash,16)+'.spv';
end;

Procedure DumpSpv(FStage:TvShaderStage;M:TMemoryStream;hash:QWORD);
var
 F:THandle;
 fname:RawByteString;
begin
 if (hash=0) then
 begin
  hash:=MurmurHash64A(M.Memory,M.Size,0);
 end;

 fname:=GetDumpSpvName(FStage,hash);

 fname:='shader_dump\'+fname;

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

function _GetDmem(P:Pointer):Pointer; register;
begin
 Result:=nil;
 if not get_dmem_ptr(P,@Result,nil) then
 begin
  Assert(false,'_GetDmem');
 end;
end;

function ParseShader(FStage:TvShaderStage;pData:PDWORD;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TMemoryStream;
var
 SprvEmit:TSprvEmit;
begin
 Result:=nil;
 SprvEmit:=TSprvEmit.Create;

 case FStage of
  vShaderStagePs:
  begin
   SprvEmit.InitPs(GPU_REGS.SG_REG^.SPI_SHADER_PGM_RSRC1_PS,
                   GPU_REGS.SG_REG^.SPI_SHADER_PGM_RSRC2_PS,
                   GPU_REGS.CX_REG^.SPI_PS_INPUT_ENA);

   SprvEmit.SetUserData(GPU_REGS.get_user_data(FStage));

   SprvEmit.SET_SHADER_CONTROL(GPU_REGS.CX_REG^.DB_SHADER_CONTROL);
   SprvEmit.SET_INPUT_CNTL    (GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL,
                               GPU_REGS.CX_REG^.SPI_PS_IN_CONTROL.NUM_INTERP);
  end;
  vShaderStageVs:
  begin
   SprvEmit.InitVs(GPU_REGS.SG_REG^.SPI_SHADER_PGM_RSRC1_VS,
                   GPU_REGS.SG_REG^.SPI_SHADER_PGM_RSRC2_VS,
                   GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_0,
                   GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_1);

   SprvEmit.SetUserData(GPU_REGS.get_user_data(FStage));
  end;
  vShaderStageCs:
  begin
   SprvEmit.InitCs(GPU_REGS.SC_REG^.COMPUTE_PGM_RSRC1,
                   GPU_REGS.SC_REG^.COMPUTE_PGM_RSRC2,
                   GPU_REGS.SC_REG^.COMPUTE_NUM_THREAD_X,
                   GPU_REGS.SC_REG^.COMPUTE_NUM_THREAD_Y,
                   GPU_REGS.SC_REG^.COMPUTE_NUM_THREAD_Z);

   SprvEmit.SetUserData(GPU_REGS.get_user_data(FStage));
  end;

  else
    Assert(false,'TODO PARSE:'+STAGE_NAME[FStage]);
 end;

 SprvEmit.Config.PrintAsm      :=False;
 SprvEmit.Config.UseVertexInput:=True;
 SprvEmit.Config.UseTexelBuffer:=False;
 SprvEmit.Config.UseOutput16   :=storageInputOutput16;
 SprvEmit.Config.UseOnlyUserdataPushConst:=True;

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

 SprvEmit.Config.OnGetDmem:=@_GetDmem;

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
end;

function test_func(FShader:TvShaderExt;pUserData:Pointer):Boolean;
var
 key:TShaderFuncKey;

 P:Pointer;
 i:Integer;
begin
 Result:=True;
 if (Length(FShader.FShaderFuncs)=0) then Exit;

 key:=Default(TShaderFuncKey);

 For i:=0 to High(FShader.FShaderFuncs) do
 if (FShader.FShaderFuncs[i].pData<>nil) then
 begin

  P:=GetSharpByPatch(pUserData,FShader.GetImmData,FShader.FFuncLayouts[i].addr);

  if (P<>nil) then
  begin
   key.pData:=P;

   if (TShaderFuncKey.c(FShader.FShaderFuncs[i],key)<>0) then
   begin
    Exit(False);
   end;

  end;

 end;
end;

function test_instance(FShader:TvShaderExt;FStage:TvShaderStage;var GPU_REGS:TGPU_REGS):Boolean;
var
 VGPR_COMP_CNT:Byte;
begin
 if (FStage<>vShaderStageVs) then Exit(True);

 VGPR_COMP_CNT:=GPU_REGS.SG_REG^.SPI_SHADER_PGM_RSRC1_VS.VGPR_COMP_CNT;

 if (FShader.FParams.VGPR_COMP_CNT<>VGPR_COMP_CNT) then Exit(False);

 if (VGPR_COMP_CNT>=1) then
 begin
  if (FShader.FParams.STEP_RATE_0<>GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_0) then Exit(False);
 end;

 if (VGPR_COMP_CNT>=2) then
 begin
  if (FShader.FParams.STEP_RATE_1<>GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_1) then Exit(False);
 end;

 Result:=True;
end;

function test_ps_params(FShader:TvShaderExt;FStage:TvShaderStage;var GPU_REGS:TGPU_REGS):Boolean;
begin
 if (FStage<>vShaderStagePs) then Exit(True);

 if (DWORD(FShader.FParams.SHADER_CONTROL)<>DWORD(GPU_REGS.CX_REG^.DB_SHADER_CONTROL)) then Exit(False);

 if (FShader.FParams.NUM_INTERP<>GPU_REGS.CX_REG^.SPI_PS_IN_CONTROL.NUM_INTERP) then Exit(False);

 if (CompareByte(FShader.FParams.INPUT_CNTL,
                 GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL,
                 SizeOf(TSPI_PS_INPUT_CNTL_0)*FShader.FParams.NUM_INTERP)<>0) then Exit(False);

 Result:=True;
end;

function test_unif(FShader:TvShaderExt;FDescSetId:Integer;pUserData:Pointer):Boolean;
var
 ch:TvBufOffsetChecker;
begin
 if (FShader.FDescSetId<>FDescSetId) then Exit(False);
 ch.FResult:=True;
 FShader.EnumUnifLayout(@ch.AddAttr,FDescSetId,pUserData,FShader.GetImmData);
 Result:=ch.FResult;
end;

function test_push_const(FShader:TvShaderExt;pc_offset,pc_size:DWORD):Boolean;
begin
 with FShader.FPushConst do
 begin
  Result:=(offset       >=pc_offset) and  //Checking offsets push constant
          ((offset+size)<=pc_size);       //Is the remaining size sufficient?
 end;
end;

function _FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderExt;
var
 i:Integer;
 FShader:TvShaderExt;
 t:TShaderCodeCache;

 M:TMemoryStream;

 pData:PDWORD;
 pUserData:Pointer;

 pc_offset,pc_size,pc_diff:DWORD;

 FHash_spv:QWORD;
begin
 pData:=GPU_REGS.get_code_addr(FStage);

 {
  ...start <-\
             |
  ...offset  |
             |
  ...size  --/
 }

 if (pc<>nil) then //push const allocator used?
 begin
  pc_offset:=pc^.offset;
  pc_size  :=pc^.GetAvailable;
 end else
 begin
  pc_offset:=0;
  pc_size  :=0;
 end;

 t:=_FetchShaderCodeCache(FStage,pData);

 FShader:=nil;

 if (Length(t.FShaderAliases)<>0) then
 begin

  pUserData:=GPU_REGS.get_user_data(FStage);

  For i:=0 to High(t.FShaderAliases) do
  begin
   FShader:=t.FShaderAliases[i];

   if test_func(FShader,pUserData) then
   if test_instance(FShader,FStage,GPU_REGS) then
   if test_ps_params(FShader,FStage,GPU_REGS) then
   if test_unif(FShader,FDescSetId,pUserData) then //Checking offsets within a shader
   if test_push_const(FShader,pc_offset,pc_size) then
   begin
    Break; //found
   end;

   FShader:=nil; //reset with not found
  end;

 end;

 if (FShader=nil) then //Rebuild with different parameters
 begin

  M:=ParseShader(FStage,pData,GPU_REGS,pc);
  Assert(M<>nil);

  //hach/dump
  FHash_spv:=MurmurHash64A(M.Memory,M.Size,0);
  DumpSpv(FStage,M,FHash_spv);
  //

  pUserData:=GPU_REGS.get_user_data(FStage);

  FShader:=t.AddShader(FDescSetId,M,pUserData);

  //set hash
  FShader.FHash_spv:=FHash_spv;

  //free spv data
  M.Free;

  case FStage of
   vShaderStageVs:
    begin
     FShader.SetInstance(GPU_REGS.SG_REG^.SPI_SHADER_PGM_RSRC1_VS.VGPR_COMP_CNT,
                         GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_0,
                         GPU_REGS.CX_REG^.VGT_INSTANCE_STEP_RATE_1);
    end;
   vShaderStagePs:
    begin
     FShader.SET_SHADER_CONTROL(GPU_REGS.CX_REG^.DB_SHADER_CONTROL);
     FShader.SET_INPUT_CNTL    (GPU_REGS.CX_REG^.SPI_PS_INPUT_CNTL,
                                GPU_REGS.CX_REG^.SPI_PS_IN_CONTROL.NUM_INTERP);

    end;
   else;
  end;

  //dump gcn
  case FStage of
   vShaderStagePs:DumpPS(GPU_REGS,FShader.FHash_gcn);
   vShaderStageVs:DumpVS(GPU_REGS,FShader.FHash_gcn);
   vShaderStageCs:DumpCS(GPU_REGS,FShader.FHash_gcn);
   else;
  end;

  //

  if (FShader.FPushConst.size<>0) and (pc<>nil) then //push const used?
  begin
   FShader.FPushConst.offset:=pc_offset;   //Save offset
   Dec(FShader.FPushConst.size,pc_offset); //Move up size

   {
    ...start

    ...offset<-\
               |
    ...size  --/
   }
  end;

 end;

 if (FShader.FPushConst.size<>0) and (pc<>nil) then //push const used?
 begin
  pc_diff:=FShader.FPushConst.offset-pc_offset; //get diff offset
  pc^.Apply(pc_diff+FShader.FPushConst.size);   //apply with allocator
 end;

 Result:=FShader;
end;

function FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderExt;
begin
 FShaderCacheSet.Lock_wr;

 Result:=_FetchShader(FStage,FDescSetId,GPU_REGS,pc);

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

function FetchShaderGroupRT(var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderGroup;
var
 FShadersKey:TvShadersKey;
 i:TvShaderStage;
 FDescSetId:Integer;
begin
 FShadersKey:=Default(TvShadersKey);

 FDescSetId:=0;

 For i:=High(TvShaderStage) downto Low(TvShaderStage) do
 begin
  if (i<>vShaderStageCs) then
  if (GPU_REGS.get_code_addr(i)<>nil) then
  begin
   FShadersKey.FShaders[i]:=FetchShader(i,FDescSetId,GPU_REGS,pc);
   Inc(FDescSetId);
  end;
 end;

 Result:=FetchShaderGroup(@FShadersKey);
end;

function FetchShaderGroupCS(var GPU_REGS:TGPU_REGS;pc:PPushConstAllocator):TvShaderGroup;
var
 FShadersKey:TvShadersKey;
begin
 FShadersKey:=Default(TvShadersKey);

 FShadersKey.FShaders[vShaderStageCs]:=FetchShader(vShaderStageCs,0,GPU_REGS,pc);

 Result:=FetchShaderGroup(@FShadersKey);
end;

end.

