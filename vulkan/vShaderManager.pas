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

  ps4_program,

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
function FetchShaderGroup(F:PvShadersKey):TvShaderGroup;

implementation

type
 TShaderCacheCompare=object
  function c(a,b:PShaderDataKey):Integer; static;
 end;

 TShadersKeyCompare=object
  function c(a,b:PvShadersKey):Integer; static;
 end;

 _TShaderCacheSet=specialize T23treeSet<PShaderDataKey,TShaderCacheCompare>;
 TShaderCacheSet=object(_TShaderCacheSet)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

 _TShaderGroupSet=specialize T23treeSet<PvShadersKey,TShadersKeyCompare>;
 TShaderGroupSet=object(_TShaderGroupSet)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

var
 FShaderCacheSet:TShaderCacheSet;
 FShaderGroupSet:TShaderGroupSet;

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

//

Procedure TShaderGroupSet.Init;
begin
 rwlock_init(lock);
end;

Procedure TShaderGroupSet.Lock_wr;
begin
 rwlock_wrlock(lock);
end;

Procedure TShaderGroupSet.Unlock;
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

function TShadersKeyCompare.c(a,b:PvShadersKey):Integer;
begin
 Result:=CompareByte(a^.FShaders,b^.FShaders,SizeOf(AvShaderStage));
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

function _FindShaderCache(var F:TShaderDataKey):TShaderCache;
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

Procedure DumpSpv(FStage:TvShaderStage;M:TMemoryStream);
var
 hash:DWORD;
 F:THandle;
 fname:RawByteString;
begin
 hash:=FastHash(M.Memory,M.Size);

 case FStage of
  vShaderStagePs:fname:='_ps_';
  vShaderStageVs:fname:='_vs_';
  vShaderStageCs:fname:='_cs_';
  else
    Exit;
 end;

 fname:='shader_dump\'+get_dev_progname+fname+HexStr(hash,8)+'.spv';

 if FileExists(fname) then Exit;

 CreateDir('shader_dump');

 F:=FileCreate(fname);
 FileWrite(F,M.Memory^,M.Size);
 FileClose(F);
end;

function ParseShader(FStage:TvShaderStage;pData:PDWORD;var GPU_REGS:TGPU_REGS):TMemoryStream;
var
 SprvEmit:TSprvEmit;
begin
 Result:=nil;
 SprvEmit:=TSprvEmit.Create;

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

 SprvEmit.Config.PrintAsm      :=False;
 SprvEmit.Config.UseVertexInput:=True;
 SprvEmit.Config.UseTexelBuffer:=False;
 SprvEmit.Config.UseOutput16   :=storageInputOutput16;

 SprvEmit.Config.maxUniformBufferRange          :=0; // $FFFF
 SprvEmit.Config.PushConstantsOffset            :=0; // 0
 SprvEmit.Config.maxPushConstantsSize           :=limits.maxPushConstantsSize; // 128
 SprvEmit.Config.minStorageBufferOffsetAlignment:=limits.minStorageBufferOffsetAlignment; // $10
 SprvEmit.Config.minUniformBufferOffsetAlignment:=limits.minUniformBufferOffsetAlignment; // $100

 SprvEmit.Config.maxPushConstantsSize:=16*4;
 SprvEmit.Config.maxPushConstantsSize:=12;
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

function _FetchShader(FStage:TvShaderStage;pData:PDWORD;FDescSetId:Integer;var GPU_REGS:TGPU_REGS):TvShaderExt;
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

 t:=_FindShaderCache(F);

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

 Result:=_FetchShader(FStage,pData,FDescSetId,GPU_REGS);

 FShaderCacheSet.Unlock;
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

 FShaderGroupSet.Unlock;
end;

initialization
 FShaderCacheSet.Init;
 FShaderGroupSet.Init;

end.

