{$mode objfpc}{$H+}

Uses
 classes,
 Sysutils,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_registers,
 ps4_pssl,
 ps4_shader,

 srConfig,
 SprvEmit,

 emit_post,
 emit_alloc,
 emit_print,
 emit_bin;

var
 cfg:record
  FName:RawByteString;
  FSave:RawByteString;
  FPrintInfo:Boolean;
  FPrintSpv:Boolean;
  //
  cfg:TsrConfig;
 end;

type
 TDUMP_WORD=packed record
  REG,COUNT:WORD;
 end;

procedure SaveToFile_Spv(const FName:RawByteString;var SprvEmit:TSprvEmit);
var
 F:TFileStream;
begin
 F:=TFileStream.Create(FName,fmCreate);
 SprvEmit.SaveToStream(F);
 F.Free;
end;

type
 TSPI_USER_DATA=array[0..15] of DWORD;

var
 GPU_REGS:packed record

  PS:packed record
   Addr:Pointer;

   INPUT_CNTL:array[0..31] of TSPI_PS_INPUT_CNTL_0;

   RSRC1:TSPI_SHADER_PGM_RSRC1_PS;
   RSRC2:TSPI_SHADER_PGM_RSRC2_PS;
   RSRC3:TSPI_SHADER_PGM_RSRC3_PS;

   Z_FORMAT  :TSPI_SHADER_Z_FORMAT;
   COL_FORMAT:TSPI_SHADER_COL_FORMAT;

   INPUT_ENA :TSPI_PS_INPUT_ENA;
   INPUT_ADDR:TSPI_PS_INPUT_ADDR;
   IN_CONTROL:TSPI_PS_IN_CONTROL;
   BARYC_CNTL:TSPI_BARYC_CNTL;

   SHADER_CONTROL:TDB_SHADER_CONTROL;
   SHADER_MASK:TCB_SHADER_MASK;

   USER_DATA:TSPI_USER_DATA;
  end;

  VS:packed record
   Addr:Pointer;

   RSRC1:TSPI_SHADER_PGM_RSRC1_VS;
   RSRC2:TSPI_SHADER_PGM_RSRC2_VS;
   RSRC3:TSPI_SHADER_PGM_RSRC3_VS;

   OUT_CONFIG:TSPI_VS_OUT_CONFIG;
   POS_FORMAT:TSPI_SHADER_POS_FORMAT;
   OUT_CNTL  :TPA_CL_VS_OUT_CNTL;

   USER_DATA:TSPI_USER_DATA;
  end;

  CS:packed record
   Addr:Pointer;

   RSRC1:TCOMPUTE_PGM_RSRC1;
   RSRC2:TCOMPUTE_PGM_RSRC2;

   STATIC_THREAD_MGMT_SE0:TCOMPUTE_STATIC_THREAD_MGMT_SE0;
   STATIC_THREAD_MGMT_SE1:TCOMPUTE_STATIC_THREAD_MGMT_SE1;
   RESOURCE_LIMITS:TCOMPUTE_RESOURCE_LIMITS;

   NUM_THREAD_X:TCOMPUTE_NUM_THREAD_X;
   NUM_THREAD_Y:TCOMPUTE_NUM_THREAD_Y;
   NUM_THREAD_Z:TCOMPUTE_NUM_THREAD_Z;

   USER_DATA:TSPI_USER_DATA;
  end;

  VGT_NUM_INSTANCES:TVGT_NUM_INSTANCES;
 end;

 Blocks:TFPList;

procedure ClearBlocks;
var
 i:Integer;
begin
 if (Blocks=nil) then
 begin
  Blocks:=TFPList.Create;
  Exit;
 end;
 if (Blocks.Count<>0) then
 For i:=0 to Blocks.Count-1 do
 begin
  FreeMem(Blocks.Items[i]);
 end;
 Blocks.Clear;
end;

procedure _print_hex(addr:Pointer;size:DWORD);
var
 i:DWORD;
begin
 For i:=0 to size-1 do
 begin
  if (i<>0) and ((i mod 16)=0) then Writeln;
  Write(HexStr(PByte(addr)[i],2));
 end;
 if (i<>0) and ((i mod 16)<>0) then Writeln;
end;

procedure load_dump(const fname:RawByteString);
var
 M:TMemoryStream;
 W:TDUMP_WORD;
 V:DWORD;
 addr:Pointer;
 size,i:DWORD;
begin
 ClearBlocks;
 FillChar(GPU_REGS,SizeOf(GPU_REGS),0);
 M:=TMemoryStream.Create;
 M.LoadFromFile(fname);
 M.Position:=0;
 V:=0;
 W:=Default(TDUMP_WORD);
 repeat
  if M.Read(W,SizeOf(W))<>SizeOf(W) then Break;
  if (W.COUNT=0) then //simple
  begin
   if M.Read(V,SizeOf(V))<>SizeOf(V) then Break;

   Case W.REG of
    mmCOMPUTE_PGM_RSRC1   :DWORD(GPU_REGS.CS.RSRC1       ):=V;
    mmCOMPUTE_PGM_RSRC2   :DWORD(GPU_REGS.CS.RSRC2       ):=V;
    mmCOMPUTE_NUM_THREAD_X:DWORD(GPU_REGS.CS.NUM_THREAD_X):=V;
    mmCOMPUTE_NUM_THREAD_Y:DWORD(GPU_REGS.CS.NUM_THREAD_Y):=V;
    mmCOMPUTE_NUM_THREAD_Z:DWORD(GPU_REGS.CS.NUM_THREAD_Z):=V;
    //
    mmCOMPUTE_USER_DATA_0..mmCOMPUTE_USER_DATA_15:
      begin
       i:=W.REG-mmCOMPUTE_USER_DATA_0;
       GPU_REGS.CS.USER_DATA[i]:=V;
      end;
    //
    mmCOMPUTE_STATIC_THREAD_MGMT_SE0:DWORD(GPU_REGS.CS.STATIC_THREAD_MGMT_SE0):=V;
    mmCOMPUTE_STATIC_THREAD_MGMT_SE1:DWORD(GPU_REGS.CS.STATIC_THREAD_MGMT_SE1):=V;
    mmCOMPUTE_RESOURCE_LIMITS       :DWORD(GPU_REGS.CS.RESOURCE_LIMITS       ):=V;
    //
    mmSPI_SHADER_PGM_RSRC1_PS:DWORD(GPU_REGS.PS.RSRC1         ):=V;
    mmSPI_SHADER_PGM_RSRC2_PS:DWORD(GPU_REGS.PS.RSRC2         ):=V;
    mmSPI_SHADER_PGM_RSRC3_PS:DWORD(GPU_REGS.PS.RSRC3         ):=V;
    mmSPI_SHADER_Z_FORMAT    :DWORD(GPU_REGS.PS.Z_FORMAT      ):=V;
    mmSPI_SHADER_COL_FORMAT  :DWORD(GPU_REGS.PS.COL_FORMAT    ):=V;
    mmSPI_PS_INPUT_ENA       :DWORD(GPU_REGS.PS.INPUT_ENA     ):=V;
    mmSPI_PS_INPUT_ADDR      :DWORD(GPU_REGS.PS.INPUT_ADDR    ):=V;
    mmSPI_PS_IN_CONTROL      :DWORD(GPU_REGS.PS.IN_CONTROL    ):=V;
    mmSPI_BARYC_CNTL         :DWORD(GPU_REGS.PS.BARYC_CNTL    ):=V;
    mmDB_SHADER_CONTROL      :DWORD(GPU_REGS.PS.SHADER_CONTROL):=V;
    mmCB_SHADER_MASK         :DWORD(GPU_REGS.PS.SHADER_MASK   ):=V;
    //
    mmSPI_SHADER_USER_DATA_PS_0..mmSPI_SHADER_USER_DATA_PS_15:
      begin
       i:=W.REG-mmSPI_SHADER_USER_DATA_PS_0;
       GPU_REGS.PS.USER_DATA[i]:=V;
      end;
    //
    mmSPI_PS_INPUT_CNTL_0..mmSPI_PS_INPUT_CNTL_31:
      begin
       i:=W.REG-mmSPI_PS_INPUT_CNTL_0;
       DWORD(GPU_REGS.PS.INPUT_CNTL[i]):=V;
      end;
    //
    mmSPI_SHADER_PGM_RSRC1_VS:DWORD(GPU_REGS.VS.RSRC1     ):=V;
    mmSPI_SHADER_PGM_RSRC2_VS:DWORD(GPU_REGS.VS.RSRC2     ):=V;
    mmSPI_SHADER_PGM_RSRC3_VS:DWORD(GPU_REGS.VS.RSRC3     ):=V;
    mmSPI_VS_OUT_CONFIG      :DWORD(GPU_REGS.VS.OUT_CONFIG):=V;
    mmSPI_SHADER_POS_FORMAT  :DWORD(GPU_REGS.VS.POS_FORMAT):=V;
    mmPA_CL_VS_OUT_CNTL      :DWORD(GPU_REGS.VS.OUT_CNTL  ):=V;
    //
    mmSPI_SHADER_USER_DATA_VS_0..mmSPI_SHADER_USER_DATA_VS_15:
      begin
       i:=W.REG-mmSPI_SHADER_USER_DATA_VS_0;
       GPU_REGS.VS.USER_DATA[i]:=V;
      end;
    //
    mmVGT_NUM_INSTANCES      :DWORD(GPU_REGS.VGT_NUM_INSTANCES):=V;
   end;


  end else
  begin
   size:=(W.COUNT+1)*4;

   if cfg.FPrintInfo then
    if (W.COUNT<>0) then Writeln('block:',getRegName(W.REG),' size:',size);

   addr:=AllocMem(size+7);
   if M.Read(Align(addr,8)^,size)<>size then
   begin
    FreeMem(addr);
    Break;
   end else
   begin
    Blocks.Add(addr);
    addr:=Align(addr,8);
   end;

   //_print_hex(addr,size);

   Case W.REG of
    mmCOMPUTE_PGM_LO:
      begin
       GPU_REGS.CS.Addr:=addr;
      end;
    mmSPI_SHADER_PGM_LO_PS:
      begin
       GPU_REGS.PS.Addr:=addr;
      end;
    mmSPI_SHADER_PGM_LO_VS:
      begin
       GPU_REGS.VS.Addr:=addr;
      end;

    mmCOMPUTE_USER_DATA_0..mmCOMPUTE_USER_DATA_14:
      begin
       i:=W.REG-mmCOMPUTE_USER_DATA_0;
       GPU_REGS.CS.USER_DATA[i+0]:=DWORD({%H-}QWORD(addr));
       GPU_REGS.CS.USER_DATA[i+1]:=DWORD({%H-}QWORD(addr) shr 32);
      end;

    mmSPI_SHADER_USER_DATA_PS_0..mmSPI_SHADER_USER_DATA_PS_14:
      begin
       i:=W.REG-mmSPI_SHADER_USER_DATA_PS_0;
       GPU_REGS.PS.USER_DATA[i+0]:=DWORD({%H-}QWORD(addr));
       GPU_REGS.PS.USER_DATA[i+1]:=DWORD({%H-}QWORD(addr) shr 32);
      end;

    mmSPI_SHADER_USER_DATA_VS_0..mmSPI_SHADER_USER_DATA_VS_14:
      begin
       i:=W.REG-mmSPI_SHADER_USER_DATA_VS_0;
       GPU_REGS.VS.USER_DATA[i+0]:=DWORD({%H-}QWORD(addr));
       GPU_REGS.VS.USER_DATA[i+1]:=DWORD({%H-}QWORD(addr) shr 32);
      end;

   end;
  end;
 until false;
end;

procedure load_pssl(base:Pointer;ShaderType:Byte);
var
 info:PShaderBinaryInfo;
 Slots:PInputUsageSlot;

 SprvEmit:TSprvEmit;

 i:Byte;

begin
 if (base=nil) then Exit;

 info:=_calc_shader_info(base,MemSize(base) div 4);

 if cfg.FPrintInfo then
 if (info<>nil) then
 begin
  Writeln('signature               =',info^.signature               );
  Writeln('version                 =',info^.version                 );
  Writeln('pssl_or_cg              =',info^.pssl_or_cg              );
  Writeln('cached                  =',info^.cached                  );
  Writeln('m_type                  =',info^.m_type                  );
  Writeln('source_type             =',info^.source_type             );
  Writeln('length                  =',info^.length                  );
  Writeln('chunkUsageBaseOffsetInDW=',info^.chunkUsageBaseOffsetInDW);
  Writeln('numInputUsageSlots      =',info^.numInputUsageSlots      );
  Writeln('isSrt                   =',info^.isSrt                   );
  Writeln('isSrtUsedInfoValid      =',info^.isSrtUsedInfoValid      );
  Writeln('isExtendedUsageInfo     =',info^.isExtendedUsageInfo     );
  Writeln('reserved2               =',info^.reserved2               );
  Writeln('reserved3               =',info^.reserved3               );
  Writeln('shaderHash0             =','0x',HexStr(info^.shaderHash0,8));
  Writeln('shaderHash1             =','0x',HexStr(info^.shaderHash1,8));
  Writeln('crc32                   =','0x',HexStr(info^.crc32,8)      );
  writeln;

  if (info^.numInputUsageSlots<>0) then
  begin
   Slots:=_calc_shader_slot(info);

   For i:=0 to info^.numInputUsageSlots-1 do
   begin
    Writeln('Slot[',i,']');

    case Slots[i].m_usageType of
     kShaderInputUsageImmResource                     :Writeln(' ImmResource                     ');
     kShaderInputUsageImmSampler                      :Writeln(' ImmSampler                      ');
     kShaderInputUsageImmConstBuffer                  :Writeln(' ImmConstBuffer                  ');
     kShaderInputUsageImmVertexBuffer                 :Writeln(' ImmVertexBuffer                 ');
     kShaderInputUsageImmRwResource                   :Writeln(' ImmRwResource                   ');
     kShaderInputUsageImmAluFloatConst                :Writeln(' ImmAluFloatConst                ');
     kShaderInputUsageImmAluBool32Const               :Writeln(' ImmAluBool32Const               ');
     kShaderInputUsageImmGdsCounterRange              :Writeln(' ImmGdsCounterRange              ');
     kShaderInputUsageImmGdsMemoryRange               :Writeln(' ImmGdsMemoryRange               ');
     kShaderInputUsageImmGwsBase                      :Writeln(' ImmGwsBase                      ');
     kShaderInputUsageImmShaderResourceTable          :Writeln(' ImmShaderResourceTable          ');
     kShaderInputUsageImmLdsEsGsSize                  :Writeln(' ImmLdsEsGsSize                  ');
     kShaderInputUsageSubPtrFetchShader               :Writeln(' SubPtrFetchShader               ');
     kShaderInputUsagePtrResourceTable                :Writeln(' PtrResourceTable                ');
     kShaderInputUsagePtrInternalResourceTable        :Writeln(' PtrInternalResourceTable        ');
     kShaderInputUsagePtrSamplerTable                 :Writeln(' PtrSamplerTable                 ');
     kShaderInputUsagePtrConstBufferTable             :Writeln(' PtrConstBufferTable             ');
     kShaderInputUsagePtrVertexBufferTable            :Writeln(' PtrVertexBufferTable            ');
     kShaderInputUsagePtrSoBufferTable                :Writeln(' PtrSoBufferTable                ');
     kShaderInputUsagePtrRwResourceTable              :Writeln(' PtrRwResourceTable              ');
     kShaderInputUsagePtrInternalGlobalTable          :Writeln(' PtrInternalGlobalTable          ');
     kShaderInputUsagePtrExtendedUserData             :Writeln(' PtrExtendedUserData             ');
     kShaderInputUsagePtrIndirectResourceTable        :Writeln(' PtrIndirectResourceTable        ');
     kShaderInputUsagePtrIndirectInternalResourceTable:Writeln(' PtrIndirectInternalResourceTable');
     kShaderInputUsagePtrIndirectRwResourceTable      :Writeln(' PtrIndirectRwResourceTable      ');
     kShaderInputUsageImmGdsKickRingBufferOffse       :Writeln(' ImmGdsKickRingBufferOffse       ');
     kShaderInputUsageImmVertexRingBufferOffse        :Writeln(' ImmVertexRingBufferOffse        ');
     kShaderInputUsagePtrDispatchDraw                 :Writeln(' PtrDispatchDraw                 ');
     kShaderInputUsageImmDispatchDrawInstances        :Writeln(' ImmDispatchDrawInstances        ');
     else
      Writeln(' m_usageType=',Slots[i].m_usageType);
    end;

    Writeln(' apiSlot=',Slots[i].m_apiSlot);
    Writeln(' startRegister=',Slots[i].m_startRegister);

    Writeln(' param=',HexStr(Slots[i].m_srtSizeInDWordMinusOne,2));
   end;
   Writeln;
  end;
 end;

 SprvEmit:=TSprvEmit.Create;

 case ShaderType of
  kShaderTypePs  :
  begin
   if cfg.FPrintInfo then
   begin
    Writeln(' USGPR:',GPU_REGS.PS.RSRC2.USER_SGPR,
            ' VGPRS:',ConvertCountVGPRS(GPU_REGS.PS.RSRC1.VGPRS),
            ' SGPRS:',ConvertCountSGPRS(GPU_REGS.PS.RSRC1.SGPRS));
   end;

   SprvEmit.InitPs(GPU_REGS.PS.RSRC1,GPU_REGS.PS.RSRC2,GPU_REGS.PS.INPUT_ENA);
   SprvEmit.SetUserData(@GPU_REGS.PS.USER_DATA);

   SprvEmit.SET_SHADER_CONTROL(GPU_REGS.PS.SHADER_CONTROL);
   SprvEmit.SET_INPUT_CNTL    (@GPU_REGS.PS.INPUT_CNTL,GPU_REGS.PS.IN_CONTROL.NUM_INTERP);
  end;
  kShaderTypeVsVs:
  begin
   if cfg.FPrintInfo then
   begin
    Writeln(' USGPR:',GPU_REGS.VS.RSRC2.USER_SGPR,
            ' VGPRS:',ConvertCountVGPRS(GPU_REGS.VS.RSRC1.VGPRS),
            ' SGPRS:',ConvertCountSGPRS(GPU_REGS.VS.RSRC1.SGPRS));
   end;

   SprvEmit.InitVs(GPU_REGS.VS.RSRC1,GPU_REGS.VS.RSRC2,1,1);

   SprvEmit.SetUserData(@GPU_REGS.VS.USER_DATA);
  end;
  kShaderTypeCs:
  begin
   if cfg.FPrintInfo then
   begin
    Writeln(' USGPR:',GPU_REGS.CS.RSRC2.USER_SGPR,
            ' VGPRS:',ConvertCountVGPRS(GPU_REGS.CS.RSRC1.VGPRS),
            ' SGPRS:',ConvertCountSGPRS(GPU_REGS.CS.RSRC1.SGPRS));
    Writeln(' SCRATCH_EN:',GPU_REGS.CS.RSRC2.SCRATCH_EN);
   end;

   SprvEmit.InitCs(GPU_REGS.CS.RSRC1,GPU_REGS.CS.RSRC2,GPU_REGS.CS.NUM_THREAD_X,GPU_REGS.CS.NUM_THREAD_Y,GPU_REGS.CS.NUM_THREAD_Z);
   SprvEmit.SetUserData(@GPU_REGS.CS.USER_DATA);
  end;

  else
   begin
    _parse_print(base);
    Exit;
   end;
 end;

 SprvEmit.Config:=cfg.cfg;

 if (SprvEmit.ParseStage(base)>1) then
 begin
  Writeln(StdErr,'Shader Parse Err');
 end;

 if cfg.cfg.PrintAsm or cfg.FPrintSpv or (cfg.FSave<>'') then
 begin
  SprvEmit.PostStage;
  SprvEmit.AllocStage;
 end;

 if cfg.FPrintSpv then
 begin
  SprvEmit.Print;
  Writeln;
 end;

 if (cfg.FSave<>'') then
 begin
  SaveToFile_Spv(cfg.FSave,SprvEmit);
 end;

 if cfg.FPrintInfo then
  Writeln('used_size=',SprvEmit.Allocator.used_size);

 SprvEmit.Free;
end;

function ParseCmd:Boolean;
var
 i,n:Integer;
label
 promo;
begin
 if (ParamCount=0) then
 begin
  promo:

  Exit(False);
 end;

 cfg.FName:='';
 cfg.FSave:='';
 cfg.FPrintInfo:=False;
 cfg.FPrintSpv :=False;
 cfg.cfg.Init;

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
       '-i':cfg.FPrintInfo:=True;
       '-a':cfg.cfg.PrintAsm:=True;
       '-p':cfg.FPrintSpv:=True;

     '-eva':cfg.cfg.UseVertexInput:=True;
     '-dva':cfg.cfg.UseVertexInput:=False;

     '-etb':cfg.cfg.UseTexelBuffer:=True;
     '-dtb':cfg.cfg.UseTexelBuffer:=False;

     '-eoh':cfg.cfg.UseOutput16:=True;
     '-doh':cfg.cfg.UseOutput16:=False;

       '-b':n:=0;

    '-mubo':n:=1;//maxUniformBufferRange
     '-pco':n:=2;//PushConstantsOffset
     '-pcs':n:=3;//maxPushConstantsSize
    '-sboa':n:=4;//minStorageBufferOffsetAlignment
    '-uboa':n:=5;//minUniformBufferOffsetAlignment

   else
     begin
      Case n of
       -1:cfg.FName:=ParamStr(i);
        0:cfg.FSave:=ParamStr(i);
        1:cfg.cfg.maxUniformBufferRange          :=StrToInt64Def(ParamStr(i),0);
        2:cfg.cfg.PushConstantsOffset            :=StrToInt64Def(ParamStr(i),0);
        3:cfg.cfg.maxPushConstantsSize           :=StrToInt64Def(ParamStr(i),0);
        4:cfg.cfg.minStorageBufferOffsetAlignment:=StrToInt64Def(ParamStr(i),0);
        5:cfg.cfg.minUniformBufferOffsetAlignment:=StrToInt64Def(ParamStr(i),0);
      end;
      n:=-1;
     end;
  end;
 end;

 Result:=True;
end;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;

 FillChar(cfg,SizeOf(cfg),0);

 ParseCmd;

 if (cfg.FName='') then
 begin
  //load_dump('shader_dump\simplet-single-triangle_debug\simplet-single-triangle_debug_vs_78EF9008.dump');
  //load_dump('shader_dump\simplet-single-triangle_debug\simplet-single-triangle_debug_ps_FBCA196D.dump');

  //load_dump('shader_dump\simplet-simple-fs_debug\simplet-simple-fs_debug_vs_398C5BF6.dump');
  //load_dump('shader_dump\simplet-simple-fs_debug\simplet-simple-fs_debug_ps_F327ABD1.dump');

  //load_dump('shader_dump\WeAreDoomed\WeAreDoomed_ps4_cs_FEA061C5.dump');
  //load_dump('shader_dump\WeAreDoomed\WeAreDoomed_ps4_ps_B9680888.dump');
  //load_dump('shader_dump\WeAreDoomed\WeAreDoomed_ps4_ps_BA60EAD7.dump');
  //load_dump('shader_dump\WeAreDoomed\WeAreDoomed_ps4_vs_0C420DE0.dump');

  //load_dump('shader_dump\basic-sample\basic-sample_debug_ps_E125C5F7.dump');

  //load_dump('shader_dump\basic_quad_debug\basic_quad_debug_ps_A9F64695.dump');
  //load_dump('shader_dump\basic_quad_debug\basic_quad_debug_cs_3685EFC7.dump');

  //load_dump('shader_dump\SonicMania\SonicMania_ps_11DF2A32.dump');
  //load_dump('shader_dump\SonicMania\SonicMania_ps_3CC22A00.dump');

  //load_dump('shader_dump\MomodoraRUtM\MomodoraRUtM_ps_595F5E5D.dump');
  //load_dump('shader_dump\MomodoraRUtM\MomodoraRUtM_ps_B42E92C3.dump');
  //load_dump('shader_dump\MomodoraRUtM\MomodoraRUtM_vs_B9E80F51.dump');

  //load_dump('shader_dump\basic-sample\basic-sample_debug_cs_4303E70D.dump');
  //load_dump('shader_dump\basic-sample\basic-sample_debug_cs_C4EA2D91.dump');

  //load_dump('shader_dump\Cladun_Returns\ClassicDungeon3ORBIS_ps_B2ED8D0C.dump');
  //load_dump('shader_dump\Cladun_Returns\ClassicDungeon3ORBIS_ps_B39D802A.dump');

  //load_dump('shader_dump\depth-mode-sample\depth-mode-sample_debug_ps_E125C5F7.dump');

  //load_dump('shader_dump\tutorial_anti-aliasing_debug\tutorial_anti-aliasing_debug_ps_E820FFDE.dump');

  //load_dump('shader_dump\JETPACKJOYRIDE\mgjp_cs_2D246406.dump');

  //load_dump('shader_dump\Blazing_Chrome\Blazing_Chrome_ps_2F2B5050.dump');

  //load_dump('shader_dump\Font_eboot\eboot_vs_F591C6E7.dump');

  //load_dump('shader_dump\SEZ\SEZ_vs2012_cs_C9F09169.dump');

  //load_dump('shader_dump\light_eboot\eboot_ps_481FAD35.dump');

  //load_dump('shader_dump\pad\PS4Player_Mono_ps_93B1D493.dump');

  //load_dump('shader_dump\Taiko\Taiko_ps_D2A25152.dump');
  //load_dump('shader_dump\Taiko\Taiko_cs_A789A5D2.dump');

  //load_dump('shader_dump\RiskysRevenge\RiskysRevenge_ps_73CC5D93.dump');
  //load_dump('shader_dump\RiskysRevenge\RiskysRevenge_vs_BB563A55.dump');

  //load_dump('shader_dump\mmx\mmx_ps4_ps_38F2F46C.dump');
  //load_dump('shader_dump\mmx\mmx_ps4_ps_2E4189DF.dump');
  //load_dump('shader_dump\mmx\mmx_ps4_ps_3637A743.dump');

  //load_dump('shader_dump\Xeodrifter\miniroidPS4_ps_A63F8BB1.dump');

  //load_dump('shader_dump\bmx\eboot_vs_C35C72D8.dump'); //v_writelane_b32
  //load_dump('shader_dump\bmx\eboot_ps_0695A393.dump');

  //load_dump('shader_dump\mitsurugi\mitsurugi01_vs_6F5439B8.dump');

  //load_dump('shader_dump\mitsurugi\mitsurugi01_ps_B849A426.dump');
  //load_dump('shader_dump\mitsurugi\mitsurugi01_vs_F709D2CB.dump');

  //load_dump('shader_dump\mitsurugi\mitsurugi01_ps_C1D16780.dump');
  //load_dump('shader_dump\mitsurugi\mitsurugi01_vs_AC7998F3.dump');

  //load_dump('shader_dump\Goose\PS4Player_Il2cpp_ps_12B80986.dump');

  //load_dump('shader_dump\FINAL.FANTASY.VIII.Remastered\GuardianLauncher_ps_70061BED.dump');

  //load_dump('shader_dump\The_Last_Campfire\PS4Player_Il2cpp_ps_FA9B277A.dump');
  //load_dump('shader_dump\The_Last_Campfire\PS4Player_Il2cpp_vs_9AC28D93.dump');

  //load_dump('shader_dump\Bubsy.Paws.On.Fire\PS4Player_Il2cpp_ps_763848A1.dump');

  //load_dump('shader_dump\Roombo_First_Blood\PS4Player_Il2cpp_ps_C16AD4F5.dump');

  //load_dump('shader_dump\Corrido.Z\PS4Player_Mono_ps_12EBB640.dump');

  //load_dump('shader_dump\Life.in.Willowdale.Farm.Adventures\PS4Player_IL2CPP_ps_EDDEE9E4.dump');

  //load_dump('shader_dump\Sundered\PS4Player_Il2cpp_ps_B7A9B0A7.dump');

  //load_dump('shader_dump\Soccer.Story\PS4Player_IL2CPP_ps_A0F35020.dump');

  //load_dump('shader_dump\Inside\PS4Player_ps_001B7BD1.dump');

  //load_dump('shader_dump\Aegis.of.Earth.Protonovus.Assault\eboot_vs_818D8A4A.dump');

  //load_dump('shader_dump\UntoTheEnd\PS4Player_Il2cpp_ps_3D0F1862.dump');

  //load_dump('shader_dump\Knowledge.is.Power.Decades\PS4Player_Il2cpp_ps_4F0BF8F9.dump');

  //load_dump('shader_dump\Sonic_Colors_Ultimate\Rainbow_ps_30E8655E.dump');
  //load_dump('shader_dump\Sonic_Colors_Ultimate\Rainbow_ps_E9E8EE1B.dump');

  //load_dump('shader_dump\Tachyon.Project\PS4Player_Mono_ps_DC304D80.dump');

  //load_dump('shader_dump\bloodrayne\bloodrayne_vs_55EBA680.dump');

  //load_dump('shader_dump\Indivisible\Indivisible_vs_D35B10A2.dump');

  //load_dump('shader_dump\eboot_e105203\eboot_ps_E6D3BAC7.dump');

  //load_dump('shader_dump\rdr1\ep1004-cusa36843_00-redemption000001_ps_92FA802F.dump');
  //load_dump('shader_dump\rdr1\ep1004-cusa36843_00-redemption000001_vs_C159AD6A.dump');

  //load_dump('shader_dump\rdr1\ep1004-cusa36843_00-redemption000001_vs_E44F040B.dump');

  //load_dump('shader_dump\bb_nt\SPRJ_ps4_Master_ps_B956220F.dump');

  //load_dump('shader_dump\Frostpunk\FrostpunkPS4_cs_BF269D23.dump');

  //load_dump('shader_dump\Life_in_Willowdale_Farm_Adventures\PS4Player_IL2CPP_ps_EDDEE9E4.dump');

  load_dump('shader_dump\deferred-sample\deferred-sample_debug.elf_cs_C4FCEFBCE0193523.dump');

  //load_dump('shader_dump\Corrido_Z\PS4Player_Mono_ps_12EBB640.dump');

  //load_dump('shader_dump\bb_nt\SPRJ_ps4_Master_ps_600E5848.dump');
 end;

 //TODO: EmitDivergence
 //TODO: TsrField -> interval map
 //TODO: MUBUF nmft/dfmt cache
 //TODO: bitcast pointer on OpAtomic

 if (cfg.FName<>'') then
 begin
  load_dump(cfg.FName);
 end;

 load_pssl(GPU_REGS.CS.Addr,kShaderTypeCs);
 load_pssl(GPU_REGS.VS.Addr,kShaderTypeVsVs);
 load_pssl(GPU_REGS.PS.Addr,kShaderTypePs);

 if cfg.FPrintInfo then
 begin
  readln;
 end;
end.

{

///////////////////source ext

[USER_DATA]
  |
  v
//DATA LAYOUT
[PARENT_LAYOUT|DATA_TYPE|OFFSET|INDEX]
  ^                     ^ ^
  |                     | | //CHILD LAYOUT                   //UNIFORM
  |                     |[PARENT_LAYOUT|DATA_TYPE|OFFSET] <- [SOURCE_LAYOUT|HANDLE_TYPE|PVAR|PREG]
  |                     |                                                                ^    ^
  |                     |                                                                |    |
  |                     |                                      /-------------------------/    |
  |                     |                                      |                              |
  |                     |                                      v                              |
  |                     |                                    [PVAR] <- [opLoad]<--------------/
  |                     |
  |                     | //CHILD LAYOUT                   //BUFFER
  |                    [PARENT_LAYOUT|DATA_TYPE|OFFSET] <- [SOURCE_LAYOUT|CAST_NUM|PVAR] <-> [PVAR]<----------\
  |                                                          ^                                                |
  |                                                          | //BUFFER FIELD                                 |
  |                                                         [SOURCE_BUFFER|SOURCE_FIELD|ID|OFFSET|SIZE|PREG]  |
  |                                                                                     ^               |     |
  |                                                                        /------------/               |     |
  |                                                                        |                            |     |
  |                                                         [OpAccessChain|ID|ID...]<-------------------------/
  |                                                          ^                                          |
  |                                                          |                                          |
  |                                                         [opLoad]<-----------------------------------/
  |
  |[SET]
  |
  | //CHAIN LAYOUT
[SOURCE_LAYOUT|OFFSET|PREG] <-> [opLoad]

///////////////////


[FUserDataVar]
         ^
         |   /----------------------------------------v---------------------------\
[pArray|Source] <- [OpAccessChain|[Index]] <- [PChain|pWriter|ID] <- [opLoad] <- [pReg|ID]


      [PChain]*x
         ^ ^
         | |
[pGroup|chains]

//////////////////

[pUniform]
  |     ^
  v    /-----------------------\
[Forked_Var] <- [opLoad] <- [pReg|ID] <- [OpImageRead1D]

//////////////////

[OpVariable] [pointer struct] PushConstant
   ^           ^
   |           |
   |         [OpTypePointer] PushConstant [struct]
   |                                        ^
   |                                        |
   |                                      [OpTypeStruct] [item1:type] <- aType
   |                                         ^        ^    ^
   |                                         |        |    |
   |                                         |       [OpMemberDecorate] [struct] [item1:index] Offset 0
   |                                         |
   |                                      [OpDecorate] [struct] Block
   |
   |         [OpTypePointer] PushConstant [item1:type]
   |                |
   |                v
 [OpAccessChain] [pointer type] [pVar] [item1:index]

}


{

 x:=1 //write

  [opLabel]->

     y:=x+1  //read   ^make var

  [opLabel]<-

//////////////////////////////

 y:=0 //write

  [opLabel]->

     y:=1 //write

     y:=y+y //write <-

  [opLabel]<-

 x:=y+1; //read   ^make var

 y:=1    //write <- reset var

//////////////////////////////

 y:=0 //write

  [opLabel]->

     x:=1 //write

  [opLabel]<-

 x:=y+1; //read ^nop

//////////////////////////////

[OpCond]

IF[ [OpSelectionMerge] [OpBranchConditional] [OpLabel]

  //body

[OpLabel]  ]ENDIF

//////////////////////////////

IF[ [OpBranch] [OpLabel]

  WHILE[  [OpLoopMerge] [OpBranch] [OpLabel]

   //body

  [OpBranch] [OpLabel] ]WHILE(1)

  [OpCond]

[OpBranchConditional] [OpLabel] ]WHILE(2) ]ENDIF

//////////////////////////////

}






