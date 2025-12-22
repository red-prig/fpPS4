unit SprvEmit;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  classes,
  spirv,
  si_ci_vi_merged_registers,
  si_ci_vi_merged_groups,
  ps4_pssl,
  srInterface,
  srAllocator,
  srLiteral,
  srType,
  srTypes,
  srReg,
  srOp,
  srInput,
  srOutput,
  srLayout,
  srFlow,
  emit_fetch,
  //
  emit_post,
  emit_alloc,
  emit_print,
  emit_bin;

type
 PSPI_PS_INPUT_CNTL_0=^TSPI_PS_INPUT_CNTL_0;
 PRENDER_TARGET      =^TRENDER_TARGET;

 TSprvEmit=class(TEmitFetch)

  function    NewMain:TSpirvFunc;

  Destructor  Destroy; override;
  Constructor Create;

  procedure   SetUserData(pData:Pointer);
  procedure   FillGPR(VGPRS,USER_SGPR,SGPRS:Word);

  Procedure   InitVs(RSRC1:TSPI_SHADER_PGM_RSRC1_VS;
                     RSRC2:TSPI_SHADER_PGM_RSRC2_VS;
                     STEP_RATE_0,STEP_RATE_1:DWORD);

  Procedure   InitPs(RSRC1:TSPI_SHADER_PGM_RSRC1_PS;
                     RSRC2:TSPI_SHADER_PGM_RSRC2_PS;
                     ENA  :TSPI_PS_INPUT_ENA;
                     ADDR :TSPI_PS_INPUT_ADDR);

  Procedure   SET_SHADER_CONTROL(const SHADER_CONTROL:TDB_SHADER_CONTROL);
  Procedure   SET_INPUT_CNTL    (INPUT_CNTL:PSPI_PS_INPUT_CNTL_0;NUM_INTERP:Byte);
  Procedure   SET_RENDER_TARGETS(R:PRENDER_TARGET;COUNT:Byte);

  Procedure   InitCs(RSRC1:TCOMPUTE_PGM_RSRC1;
                     RSRC2:TCOMPUTE_PGM_RSRC2);

  Procedure   cs_set_initial_exec;

  Procedure   SET_NUM_THREADS(NTX:TCOMPUTE_NUM_THREAD_X;
                              NTY:TCOMPUTE_NUM_THREAD_Y;
                              NTZ:TCOMPUTE_NUM_THREAD_Z);

  Procedure   InitCustomGs();

  procedure   emit_spi; override;

  Procedure   PostStage;
  Procedure   AllocStage;
  Procedure   Print;
  Procedure   SaveToStream(Stream:TStream);
 end;

Function ConvertCountVGPRS(VGPRS:Byte):Byte;
Function ConvertCountSGPRS(SGPRS:Byte):Byte;

implementation

uses
 emit_SOP1,
 emit_SOPC,
 emit_SOPP,
 emit_VOP1,
 emit_VOPC,
 emit_VOP3,
 emit_DS,
 emit_MUBUF,
 emit_MTBUF,
 emit_EXP,
 emit_VINTRP,
 emit_MIMG,
 emit_SMRD,
 emit_SOPK,
 emit_SOP2,
 emit_VOP2;

//%void = OpTypeVoid;
//%f_void = OpTypeFunction %void;
//%f_main = OpFunction %void None %f_void;
//%l_main = OpLabel;

function TSprvEmit.NewMain:TSpirvFunc;
var
 tvoid,tftype:TsrType;
 node:TspirvOp;

begin
 Main:=specialize New<TSpirvFunc>;
 Main.Init('main');
 Main.mark_read(nil); //depended itself

 //OpTypeVoid
 tvoid:=TypeList.Fetch(dtTypeVoid);
 //OpTypeFunction
 tftype:=TypeList.FetchFunction(tvoid);

 //OpFunction
 node:=Main.pTop.dummy;
 node.OpId:=Op.OpFunction;
 node.pType:=tvoid;
 node.pDst :=Main; //self
 node.AddLiteral(FunctionControl.None,'None');
 node.AddParam(tftype);

 //OpLabel
 node:=NewLabelOp(True);
 Main.AddSpirvOp(node);

 Result:=Main;
end;

Destructor TSprvEmit.Destroy;
begin
 Allocator.Free;
 inherited;
end;

Constructor TSprvEmit.Create;
begin
 InitLists;

 FuncList.Insert(NewMain);

 SetConst_b(get_vcc0 ,false); //0
 SetConst_b(get_vcc1 ,false); //0
 SetConst_b(get_exec0,true ); //1
 SetConst_b(get_exec1,false); //0

 MarkWave(get_exec0);
 MarkWave(get_exec1);

 InitFlow;
end;

procedure TSprvEmit.SetUserData(pData:Pointer);
begin
 DataLayoutList.SetUserData(pData);
end;

procedure TSprvEmit.FillGPR(VGPRS,USER_SGPR,SGPRS:Word);
var
 p:Byte;
begin
 if (VGPRS>256)       then VGPRS:=256;
 if (USER_SGPR>16)    then USER_SGPR:=16;

 if (SGPRS>=2)        then SGPRS:=SGPRS-2; //VCC
 if (SGPRS>104)       then SGPRS:=104;
 if (USER_SGPR>SGPRS) then SGPRS:=USER_SGPR;

 FVGPRS:=VGPRS;
 FSGPRS:=SGPRS;

 if (VGPRS<>0) then
 For p:=0 to VGPRS-1 do
 begin
  if (RegsStory.VGRP[p].current=nil) then
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
 end;

 if (SGPRS>USER_SGPR) then
 For p:=USER_SGPR to SGPRS-1 do
 begin
  if (RegsStory.SGRP[p].current=nil) then
  begin
   SetConst_i(@RegsStory.SGRP[p],dtUnknow,0);
  end;
 end;
end;

Function ConvertCountVGPRS(VGPRS:Byte):Byte;
begin
 Result:=(VGPRS+1)*4
end;

Function ConvertCountSGPRS(SGPRS:Byte):Byte;
begin
 Result:=(SGPRS+1)*8;
end;

Procedure TSprvEmit.InitVs(RSRC1:TSPI_SHADER_PGM_RSRC1_VS;
                           RSRC2:TSPI_SHADER_PGM_RSRC2_VS;
                           STEP_RATE_0,STEP_RATE_1:DWORD);
var
 p:Byte;
begin
 FExecutionModel:=ExecutionModel.Vertex;

 //sgrp
 p:=0;
 if (RSRC2.USER_SGPR<>0) then
 For p:=p to RSRC2.USER_SGPR-1 do
 begin
  AddUserdata(@RegsStory.SGRP[p],p);
 end;
 p:=RSRC2.USER_SGPR;

 if (RSRC2.SO_EN<>0) or
    (RSRC2.OC_LDS_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itVsState);
  p:=p+1;
  //s_vs_state
  // stream_id[1:0], is_offchip[2],
  // streamout_vtx_count[6:0],
  // streamout_enable[15:0]}
 end;

 if (RSRC2.SO_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itWriteIndex);
  p:=p+1;
  //s_so_write_index
  // streamout_write_index[31:0]
 end;

 if (RSRC2.SO_BASE0_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itOffset,0);
  p:=p+1;
  //s_so_base_offset0
  // streamout_offset0[31:0]
 end;

 if (RSRC2.SO_BASE1_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itOffset,1);
  p:=p+1;
  //s_so_base_offset1
  // streamout_offset1[31:0]
 end;

 if (RSRC2.SO_BASE2_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itOffset,2);
  p:=p+1;
  //s_so_base_offset2
  // streamout_offset2[31:0]
 end;

 if (RSRC2.SO_BASE3_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itOffset,3);
  p:=p+1;
  //s_so_base_offset3
  // streamout_offset3[31:0]
 end;

 Assert(RSRC2.OC_LDS_EN=0);

 if (RSRC2.DISPATCH_DRAW_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itWaveId);
  p:=p+1;
  //s_wave_id
  // wave_id [11:0] (dispatch draw term)
 end;

 if (RSRC2.SCRATCH_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itScratch);
  p:=p+1;
  //s_scratch
  // scratch offset (in bytes)
 end;

 //vgrp
 p:=1;
 AddInput(@RegsStory.VGRP[0],dtUint32,itVIndex);

 //0 plain
 //1 step rate 0
 //2 step rate 1

 VGPR_COMP_CNT  :=RSRC1.VGPR_COMP_CNT;
 VGT_STEP_RATE_0:=STEP_RATE_0;
 VGT_STEP_RATE_1:=STEP_RATE_1;

 if (VGPR_COMP_CNT>=1) then
 begin
  AddInstance(@RegsStory.VGRP[p],1,STEP_RATE_0);
  p:=p+1;
 end;

 if (VGPR_COMP_CNT>=2) then
 begin
  AddInstance(@RegsStory.VGRP[p],2,STEP_RATE_1);
  p:=p+1;
 end;

 if (VGPR_COMP_CNT>=3) then
 begin
  AddInstance(@RegsStory.VGRP[p],0,1);
 end;

 FLDS_SIZE:=0;

 FillGPR(ConvertCountVGPRS(RSRC1.VGPRS),
         RSRC2.USER_SGPR,
         ConvertCountSGPRS(RSRC1.SGPRS));

 AddCapability(Capability.Shader);
end;

Procedure TSprvEmit.InitPs(RSRC1:TSPI_SHADER_PGM_RSRC1_PS;
                           RSRC2:TSPI_SHADER_PGM_RSRC2_PS;
                           ENA  :TSPI_PS_INPUT_ENA;
                           ADDR :TSPI_PS_INPUT_ADDR);
var
 p:Byte;
begin
 FExecutionModel:=ExecutionModel.Fragment;

 //sgrp
 p:=0;
 if (RSRC2.USER_SGPR<>0) then
 For p:=p to RSRC2.USER_SGPR-1 do
 begin
  AddUserdata(@RegsStory.SGRP[p],p);
 end;
 p:=RSRC2.USER_SGPR;

 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itPsState);
  p:=p+1;
  //s_ps_state
  // {bc_optimize, prim_mask[14:0],
  // lds_offset[15:0]}
 end;

 if (RSRC2.WAVE_CNT_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itWaveCnt);
  p:=p+1;
  //(s_wave_cnt)
  // {ps_wave_id[9:0], ps_strap_id,
  // ps_pkr_id}
 end;

 if (RSRC2.SCRATCH_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itScratch);
  p:=p+1;
  //s_scratch
  // scratch offset (in bytes)
 end;

 //vgrp
 p:=0;
 if (ADDR.PERSP_SAMPLE_ENA<>0) then
 begin
  if (ENA.PERSP_SAMPLE_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itPerspSample,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itPerspSample,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
  end;
  p:=p+2;
 end;

 if (ADDR.PERSP_CENTER_ENA<>0) then
 begin
  if (ENA.PERSP_CENTER_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itPerspCenter,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itPerspCenter,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
  end;
  p:=p+2;
 end;

 if (ADDR.PERSP_CENTROID_ENA<>0) then
 begin
  if (ENA.PERSP_CENTROID_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itPerspCentroid,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itPerspCentroid,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
  end;
  p:=p+2;
 end;

 if (ADDR.PERSP_PULL_MODEL_ENA<>0) then
 begin
  if (ENA.PERSP_PULL_MODEL_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itPerspW,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itPerspW,1);
   AddInput(@RegsStory.VGRP[p+2],dtFloat32,itPerspW,2);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+2],dtUnknow,0);
  end;
  p:=p+3;
 end;

 if (ADDR.LINEAR_SAMPLE_ENA<>0) then
 begin
  if (ENA.LINEAR_SAMPLE_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itLinearSample,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itLinearSample,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
  end;
  p:=p+2;
 end;

 if (ADDR.LINEAR_CENTER_ENA<>0) then
 begin
  if (ENA.LINEAR_CENTER_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itLinearCenter,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itLinearCenter,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
  end;
  p:=p+2;
 end;

 if (ADDR.LINEAR_CENTROID_ENA<>0) then
 begin
  if (ENA.LINEAR_CENTROID_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p+0],dtFloat32,itLinearCentroid,0);
   AddInput(@RegsStory.VGRP[p+1],dtFloat32,itLinearCentroid,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p+0],dtUnknow,0);
   SetConst_i(@RegsStory.VGRP[p+1],dtUnknow,0);
  end;
  p:=p+2;
 end;

 if (ADDR.POS_X_FLOAT_ENA<>0) then
 begin
  if (ENA.POS_X_FLOAT_ENA<>0) then
  begin
   AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,0);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.POS_Y_FLOAT_ENA<>0) then
 begin
  if (ENA.POS_Y_FLOAT_ENA<>0) then
  begin
   AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,1);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.POS_Z_FLOAT_ENA<>0) then
 begin
  if (ENA.POS_Z_FLOAT_ENA<>0) then
  begin
   AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,2);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.POS_W_FLOAT_ENA<>0) then
 begin
  if (ENA.POS_W_FLOAT_ENA<>0) then
  begin
   AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,3);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.FRONT_FACE_ENA<>0) then
 begin
  if (ENA.FRONT_FACE_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p],dtBool,itFrontFace);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.ANCILLARY_ENA<>0) then
 begin
  if (ENA.ANCILLARY_ENA<>0) then
  begin
   AddAncillary(@RegsStory.VGRP[p]);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.SAMPLE_COVERAGE_ENA<>0) then
 begin
  if (ENA.SAMPLE_COVERAGE_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p],dtUint32,itSampleCoverage);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
 end;

 if (ADDR.POS_FIXED_PT_ENA<>0) then
 begin
  if (ENA.POS_FIXED_PT_ENA<>0) then
  begin
   AddInput(@RegsStory.VGRP[p],dtUint32,itPosFixed);
  end else
  begin
   SetConst_i(@RegsStory.VGRP[p],dtUnknow,0);
  end;
  p:=p+1;
  //Per-pixel fixed point position Y[31:16], X[15:0]
 end;

 FLDS_SIZE:=RSRC2.EXTRA_LDS_SIZE*128*4;

 FillGPR(ConvertCountVGPRS(RSRC1.VGPRS),
         RSRC2.USER_SGPR,
         ConvertCountSGPRS(RSRC1.SGPRS));

 AddCapability(Capability.Shader);
end;

//ps_z_export_en           -> mrtz.R
//ps_stencil_val_export_en -> mrtz.G bits [0:7]
//ps_stencil_op_export_en  -> mrtz.G bits [8:15]
//ps_mask_export_en        -> mrtz.B
//ps_coverage_export_en    -> mrtz.A

Procedure TSprvEmit.SET_SHADER_CONTROL(const SHADER_CONTROL:TDB_SHADER_CONTROL);
begin
 DB_SHADER_CONTROL:=SHADER_CONTROL;
 //
 case SHADER_CONTROL.CONSERVATIVE_Z_EXPORT of
  1:OutputList.FDepthMode:=foDepthLess;    //EXPORT_LESS_THAN_Z
  2:OutputList.FDepthMode:=foDepthGreater; //EXPORT_GREATER_THAN_Z
  else;
 end;
 //
 FEarlyFragmentTests:=(SHADER_CONTROL.DEPTH_BEFORE_SHADER<>0);
end;

Procedure TSprvEmit.SET_INPUT_CNTL(INPUT_CNTL:PSPI_PS_INPUT_CNTL_0;NUM_INTERP:Byte);
var
 i:Byte;
begin
 PS_NUM_INTERP:=NUM_INTERP;
 //
 if (NUM_INTERP<>0) then
 for i:=0 to NUM_INTERP-1 do
 begin
  FPSInputCntl[i].DATA:=INPUT_CNTL[i];
 end;
end;

Procedure TSprvEmit.SET_RENDER_TARGETS(R:PRENDER_TARGET;COUNT:Byte);
var
 i:Byte;
begin
 EXPORT_COUNT:=COUNT;
 //
 if (COUNT<>0) then
 for i:=0 to COUNT-1 do
 begin
  FExportInfo[i].FORMAT     :=R[i].INFO.FORMAT;
  FExportInfo[i].NUMBER_TYPE:=R[i].INFO.NUMBER_TYPE;
  FExportInfo[i].COMP_SWAP  :=R[i].INFO.COMP_SWAP;
 end;
end;

Procedure TSprvEmit.InitCs(RSRC1:TCOMPUTE_PGM_RSRC1;
                           RSRC2:TCOMPUTE_PGM_RSRC2);
var
 p:Byte;
begin
 FExecutionModel:=ExecutionModel.GLCompute;

 p:=0;
 if (RSRC2.USER_SGPR<>0) then
 For p:=p to RSRC2.USER_SGPR-1 do
 begin
  AddUserdata(@RegsStory.SGRP[p],p);
 end;
 p:=RSRC2.USER_SGPR;

 if (RSRC2.TGID_X_EN<>0) then
 begin
  AddVecInput(@RegsStory.SGRP[p],dtVec3u,dtUint32,itTgid,0);
  p:=p+1;
  //(s_tgid_x) threadgroup_id0[31:0] computePgmRsrc2.tgid_x_en;tgid_x_en(1)  //gl_WorkGroupID
 end;

 if (RSRC2.TGID_Y_EN<>0) then
 begin
  AddVecInput(@RegsStory.SGRP[p],dtVec3u,dtUint32,itTgid,1);
  p:=p+1;
  //(s_tgid_y) threadgroup_id1[31:0] computePgmRsrc2.tgid_y_en;tgid_y_en(1)  //gl_WorkGroupID
 end;

 if (RSRC2.TGID_Z_EN<>0) then
 begin
  AddVecInput(@RegsStory.SGRP[p],dtVec3u,dtUint32,itTgid,2);
  p:=p+1;
  //(s_tgid_z) threadgroup_id2[31:0] computePgmRsrc2.tgid_z_en;tgid_z_en(1)  //gl_WorkGroupID
 end;

 if (RSRC2.TG_SIZE_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itTgSize);
  p:=p+1;
  //(s_tg_size)                    //gl_NumWorkGroups ???
  {first_wave, 14’b0,
  ordered_append_term[10:0],
  threadgroup_size_in_waves[5:0]} //computePgmRsrc2.tg_size_en;tg_size_en(1)
 end;

 if (RSRC2.SCRATCH_EN<>0) then
 begin
  AddInput(@RegsStory.SGRP[p],dtUint32,itScratch);
  p:=p+1;
  //s_scratch
  // scratch offset (in bytes)
 end;

 //vgrp
 p:=0;
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec3u,dtUint32,itThreadId,0);
  p:=p+1;
  //(v_thread_id_x) Thread ID in group 0  //gl_LocalInvocationID
 end;

 if (RSRC2.TIDIG_COMP_CNT>=1) then
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec3u,dtUint32,itThreadId,1);
  p:=p+1;
  //(v_thread_id_y) Thread ID in group 1  //gl_LocalInvocationID
 end;

 if (RSRC2.TIDIG_COMP_CNT>=2) then
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec3u,dtUint32,itThreadId,2);
  p:=p+1;
  //(v_thread_id_z) Thread ID in group 2 //gl_LocalInvocationID
 end;

 FLDS_SIZE:=RSRC2.LDS_SIZE*128*4;

 FillGPR(ConvertCountVGPRS(RSRC1.VGPRS),
         RSRC2.USER_SGPR,
         ConvertCountSGPRS(RSRC1.SGPRS));

 AddCapability(Capability.Shader);
end;

Procedure TSprvEmit.cs_set_initial_exec;
var
 total:PtrUint;
 val:PtrUint;

 vec:TsrRegNode;
 src:array[0..2] of TsrRegNode;
begin
 if not Config.UseExtendedEXECMask then Exit;

 total:=FLocalSize.x*FLocalSize.y*FLocalSize.z;
 val:=not PtrUint(0);

 if (total<64) then
 begin
  val:=(PtrUint(1) shl total)-1;
 end else
 if ((total mod 64)=0) then
 begin
  val:=not PtrUint(0);
 end else
 if (total>64) then
 begin
  Assert(false,'TODO: big unaligned total LocalSize');
 end;

 SetConst_q(get_exec0,dtUnknow,val and (not DWORD(0)));
 SetConst_q(get_exec1,dtUnknow,val shr 32);

 vec:=AddInput(@RegsStory.FUnattach,dtVec3u,itThreadId,0);

 src[0]:=RegsStory.FUnattach.New(dtUint32);
 src[1]:=RegsStory.FUnattach.New(dtUint32);
 src[2]:=RegsStory.FUnattach.New(dtUint32);

 OpExtract(init_line,src[0],vec,0);
 OpExtract(init_line,src[1],vec,1);
 OpExtract(init_line,src[2],vec,2);

 FThread_id:=OpIMulTo(OpIMulTo(src[0],src[1]),src[2]);

 if (total>64) then
 begin
  FThread_id:=OpAndTo(FThread_id,63);
 end;
end;

Procedure TSprvEmit.SET_NUM_THREADS(NTX:TCOMPUTE_NUM_THREAD_X;
                                    NTY:TCOMPUTE_NUM_THREAD_Y;
                                    NTZ:TCOMPUTE_NUM_THREAD_Z);
begin
 CS_NUM_THREAD_X:=DWORD(NTX);
 CS_NUM_THREAD_Y:=DWORD(NTY);
 CS_NUM_THREAD_Z:=DWORD(NTZ);
 //
 FLocalSize.x:=NTX.NUM_THREAD_FULL+NTX.NUM_THREAD_PARTIAL;
 FLocalSize.y:=NTY.NUM_THREAD_FULL+NTY.NUM_THREAD_PARTIAL;
 FLocalSize.z:=NTZ.NUM_THREAD_FULL+NTZ.NUM_THREAD_PARTIAL;
 //
 if (FLocalSize.x=0) then FLocalSize.x:=1;
 if (FLocalSize.y=0) then FLocalSize.y:=1;
 if (FLocalSize.z=0) then FLocalSize.z:=1;
 //
 cs_set_initial_exec;
end;

Procedure TSprvEmit.InitCustomGs();
begin
 FExecutionModel:=ExecutionModel.Geometry;

 FGeometryInfo.outputVertCount:=4;
 FGeometryInfo.invocationCount:=1;
 FGeometryInfo.InputMode      :=ExecutionMode.Triangles;
 FGeometryInfo.OutputMode     :=ExecutionMode.OutputTriangleStrip;

 AddCapability(Capability.Shader);
 AddCapability(Capability.Geometry);
end;

//

procedure TSprvEmit.emit_spi;
var
 obj:TObject absolute Self;
begin
 //OpLine(line,Cursor.prev_adr.Offdw*4,0);

 //
 Case FSPI.CMD.EN of
  W_SOP1  :TEmit_SOP1(obj).emit_SOP1;
  W_SOPC  :TEmit_SOPC(obj).emit_SOPC;
  W_SOPP  :TEmit_SOPP(obj).emit_SOPP;
  W_VOP1  :TEmit_VOP1(obj).emit_VOP1;
  W_VOPC  :TEmit_VOPC(obj).emit_VOPC;
  W_VOP3  :Case FSPI.VOP3a.OP of
              0..255:TEmit_VOP3(obj).emit_VOP3c;
            293..298,
            365..366:TEmit_VOP3(obj).emit_VOP3b;
            else
                     TEmit_VOP3(obj).emit_VOP3a;
           end;
  W_DS    :TEmit_DS    (obj).emit_DS;
  W_MUBUF :TEmit_MUBUF (obj).emit_MUBUF;
  W_MTBUF :TEmit_MTBUF (obj).emit_MTBUF;
  W_EXP   :TEmit_EXP   (obj).emit_EXP;
  W_VINTRP:TEmit_VINTRP(obj).emit_VINTRP;
  W_MIMG  :TEmit_MIMG  (obj).emit_MIMG;
  W_SMRD  :TEmit_SMRD  (obj).emit_SMRD;
  W_SOPK  :TEmit_SOPK  (obj).emit_SOPK;
  W_SOP2  :TEmit_SOP2  (obj).emit_SOP2;
  W_VOP2  :TEmit_VOP2  (obj).emit_VOP2;
  else
   Assert(false,'Unknow encode:0x'+HexStr(FSPI.CMD.EN,4));
 end;
end;

Procedure TSprvEmit.PostStage;
begin
 TSprvEmit_post(TObject(Self)).PostStage;
end;

Procedure TSprvEmit.AllocStage;
begin
 TSprvEmit_alloc(TObject(Self)).AllocStage;
end;

Procedure TSprvEmit.Print;
begin
 TSprvEmit_print(TObject(Self)).Print;
end;

Procedure TSprvEmit.SaveToStream(Stream:TStream);
begin
 TSprvEmit_bin(TObject(Self)).SaveToStream(Stream);
end;

end.

