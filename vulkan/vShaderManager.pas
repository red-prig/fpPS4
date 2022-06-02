unit vShaderManager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  RWLock,
  g23tree,
  ps4_pssl,
  ps4_shader,
  ps4_gpu_regs,
  shader_dump,

  vDevice,

  vShaderExt,

  SprvEmit,

  emit_post,
  emit_alloc,
  emit_print,
  emit_bin;

type
 TShaderFunc=packed object
  FLen:Ptruint;
  pData:PDWORD;
  function c(var a,b:TShaderFunc):Integer;
 end;

{

}

 PShaderDataKey=^TShaderDataKey;
 TShaderDataKey=packed record
  FStage:TvShaderStage;
  FLen:Ptruint;
  pData:PDWORD;
 end;

 TShaderCache=class
  key:TShaderDataKey;
  FShaders:array of TvShaderExt;
  Destructor Destroy; override;
 end;

function FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS):TvShaderExt;

implementation

type
 TShaderCacheCompare=object
  function c(a,b:PShaderDataKey):Integer; static;
 end;

 _TShaderCacheSet=specialize T23treeSet<PShaderDataKey,TShaderCacheCompare>;
 TShaderCacheSet=object(_TShaderCacheSet)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

var
 FShaderCacheSet:TShaderCacheSet;

Procedure TShaderCacheSet.Init;
begin
 rwlock_init(lock);
end;

Procedure TShaderCacheSet.Lock_wr;
begin
 rwlock_wrlock(lock);
end;

Procedure TShaderCacheSet.Unlock;
begin
 rwlock_unlock(lock);
end;

function Max(a,b:PtrInt):PtrInt; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function TShaderCacheCompare.c(a,b:PShaderDataKey):Integer;
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

Destructor TShaderCache.Destroy;
begin
 if (Key.pData<>nil) then FreeMem(Key.pData);
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

function _Find(var F:TShaderDataKey):TShaderCache;
var
 i:TShaderCacheSet.Iterator;
begin
 Result:=nil;
 i:=FShaderCacheSet.find(@F);
 if (i.Item<>nil) then
 begin
  Result:=TShaderCache(ptruint(i.Item^)-ptruint(@TShaderCache(nil).key));
 end;
end;

function ParseShader(FStage:TvShaderStage;pData:PDWORD;var GPU_REGS:TGPU_REGS):TMemoryStream;
var
 SprvEmit:TSprvEmit;
begin
 Result:=nil;
 SprvEmit:=Default(TSprvEmit);

 case FStage of
  vShaderStagePs  :
  begin
   SprvEmit.InitPs(GPU_REGS.SPI.PS.RSRC2,GPU_REGS.SPI.PS.INPUT_ENA);
   SprvEmit.SetUserData(@GPU_REGS.SPI.PS.USER_DATA);
  end;
  vShaderStageVs:
  begin
   SprvEmit.InitVs(GPU_REGS.SPI.VS.RSRC2,GPU_REGS.VGT_NUM_INSTANCES);
   SprvEmit.SetUserData(@GPU_REGS.SPI.VS.USER_DATA);
  end;
  vShaderStageCs:
  begin
   SprvEmit.InitCs(GPU_REGS.SPI.CS.RSRC2,GPU_REGS.SPI.CS.NUM_THREAD_X,GPU_REGS.SPI.CS.NUM_THREAD_Y,GPU_REGS.SPI.CS.NUM_THREAD_Z);
   SprvEmit.SetUserData(@GPU_REGS.SPI.CS.USER_DATA);
  end;

  else
    Exit;
 end;

 SprvEmit.FPrintAsm      :=False;
 SprvEmit.FUseVertexInput:=True;
 SprvEmit.FUseTexelBuffer:=False;
 SprvEmit.FUseOutput16   :=storageInputOutput16;

 SprvEmit.FBuffers.cfg.maxUniformBufferRange          :=0; // $FFFF
 SprvEmit.FBuffers.cfg.PushConstantsOffset            :=0; // 0
 SprvEmit.FBuffers.cfg.maxPushConstantsSize           :=limits.maxPushConstantsSize; // 128
 SprvEmit.FBuffers.cfg.minStorageBufferOffsetAlignment:=limits.minStorageBufferOffsetAlignment; // $10
 SprvEmit.FBuffers.cfg.minUniformBufferOffsetAlignment:=limits.minUniformBufferOffsetAlignment; // $100

 //SprvEmit.FBuffers.cfg.maxPushConstantsSize:=0;
 //SprvEmit.FUseVertexInput:=False;

 if (SprvEmit.Parse(pData)>1) then
 begin
  Writeln(StdErr,'Shader Parse Err');
  SprvEmit.FAllocator.Free;
  Exit;
 end;

 TSprvEmit_post(SprvEmit).Post;
 TSprvEmit_alloc(SprvEmit).Alloc;

 //TSprvEmit_print(SprvEmit).Print;

 Result:=TMemoryStream.Create;
 TSprvEmit_bin(SprvEmit).SaveToStream(Result);

 SprvEmit.FAllocator.Free;
end;

function _Fetch(FStage:TvShaderStage;pData:PDWORD;FDescSetId:Integer;var GPU_REGS:TGPU_REGS):TvShaderExt;
var
 F:TShaderDataKey;

 i:Integer;
 FShader:TvShaderExt;
 t:TShaderCache;

 fdump:RawByteString;

 M:TMemoryStream;

 pUserData:Pointer;
 ch:TvBufOffsetChecker;


begin
 F:=Default(TShaderDataKey);
 F.FStage:=FStage;
 F.pData :=pData;

 t:=_Find(F);

 if (t<>nil) then
 begin

  Case FStage of
   vShaderStageVs:pUserData:=@GPU_REGS.SPI.VS.USER_DATA;
   vShaderStagePs:pUserData:=@GPU_REGS.SPI.PS.USER_DATA;
   vShaderStageCs:pUserData:=@GPU_REGS.SPI.CS.USER_DATA;
   else
     Assert(false);
  end;

  FShader:=nil;
  if Length(t.FShaders)<>0 then
  For i:=0 to High(t.FShaders) do
  begin
   FShader:=t.FShaders[i];
   ch.FResult:=True;
   FShader.EnumUnifLayout(@ch.AddAttr,FDescSetId,pUserData);
   if ch.FResult then
   begin
    Break;
   end else
   begin
    FShader:=nil;
   end;
  end;

  if (FShader=nil) then
  begin

   M:=ParseShader(FStage,pData,GPU_REGS);
   Assert(M<>nil);

   FShader:=TvShaderExt.Create;
   FShader.FDescSetId:=FDescSetId;
   FShader.LoadFromStream(M);

   M.Free;

   i:=Length(t.FShaders);
   SetLength(t.FShaders,i+1);
   t.FShaders[i]:=FShader;
  end;

 end else
 begin
  F.FLen:=_calc_shader_size(pData);
  F.pData:=AllocMem(F.FLen);

  Move(pData^,F.pData^,F.FLen);

  t:=TShaderCache.Create;
  t.key:=F;

 {
  Case FStage of
   vShaderStageVs:fdump:=DumpVS(GPU_REGS);
   vShaderStagePs:fdump:=DumpPS(GPU_REGS);
   vShaderStageCs:fdump:=DumpCS(GPU_REGS);
   else
     Assert(false);
  end;

  FShader:=TvShaderExt.Create;
  FShader.FDescSetId:=FDescSetId;
  FShader.LoadFromFile(ChangeFileExt(fdump,'.spv'));
 }


  M:=ParseShader(FStage,pData,GPU_REGS);
  Assert(M<>nil);

  FShader:=TvShaderExt.Create;
  FShader.FDescSetId:=FDescSetId;
  FShader.LoadFromStream(M);

  M.Free;


  SetLength(t.FShaders,1);
  t.FShaders[0]:=FShader;

  FShaderCacheSet.Insert(@t.key);
 end;

 Result:=FShader;
end;

function FetchShader(FStage:TvShaderStage;FDescSetId:Integer;var GPU_REGS:TGPU_REGS):TvShaderExt;
var
 pData:PDWORD;
begin

 Case FStage of
  vShaderStageVs:pData:=getCodeAddress(GPU_REGS.SPI.VS.LO,GPU_REGS.SPI.VS.HI);
  vShaderStagePs:pData:=getCodeAddress(GPU_REGS.SPI.PS.LO,GPU_REGS.SPI.PS.HI);
  vShaderStageCs:pData:=getCodeAddress(GPU_REGS.SPI.CS.LO,GPU_REGS.SPI.CS.HI);
  else
    Assert(false);
 end;

 if (pData=nil) then Exit(nil);
 //Assert(pData<>nil);

 FShaderCacheSet.Lock_wr;

 Result:=_Fetch(FStage,pData,FDescSetId,GPU_REGS);

 FShaderCacheSet.Unlock;
end;

initialization
 FShaderCacheSet.Init;

end.

