unit SprvEmit;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  classes,
  spirv,
  si_ci_vi_merged_registers,
  ps4_pssl,
  srAllocator,
  srLiteral,
  srType,
  srTypes,
  srReg,
  srOp,
  srInput,
  srLayout,
  srFlow,
  emit_fetch,
  //
  emit_post,
  emit_alloc,
  emit_print,
  emit_bin;

type
 TSprvEmit=class(TEmitFetch)

  function    NewMain:PSpirvFunc;

  Destructor  Destroy; override;
  Constructor Create;

  procedure   SetUserData(pData:Pointer);
  Procedure   InitVs(RSRC2:TSPI_SHADER_PGM_RSRC2_VS;instance:Byte);
  Procedure   InitPs(RSRC2:TSPI_SHADER_PGM_RSRC2_PS;ENA:TSPI_PS_INPUT_ENA);
  Procedure   InitCs(RSRC2:TCOMPUTE_PGM_RSRC2;NTX:TCOMPUTE_NUM_THREAD_X;NTY:TCOMPUTE_NUM_THREAD_Y;NTZ:TCOMPUTE_NUM_THREAD_Z);

  procedure   emit_spi; override;

  Procedure   PostStage;
  Procedure   AllocStage;
  Procedure   Print;
  Procedure   SaveToStream(Stream:TStream);
 end;

implementation

uses
 emit_SOP1,
 emit_SOPC,
 emit_SOPP,
 emit_VOP1,
 emit_VOPC,
 emit_VOP3,
 //emit_DS,
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

function TSprvEmit.NewMain:PSpirvFunc;
var
 tvoid,tftype:PsrType;
 node:PspirvOp;

begin
 Main:=Alloc(SizeOf(TSpirvFunc));
 Main^.Init('main',Self);
 Main^.mark_read(nil); //depended itself

 //OpTypeVoid
 tvoid:=TypeList.Fetch(dtTypeVoid);
 //OpTypeFunction
 tftype:=TypeList.FetchFunction(tvoid);

 //OpFunction
 node:=@Main^.pTop^.dummy;
 node^.OpId:=Op.OpFunction;
 node^.pType:=tvoid;
 node^.pDst :=Main; //self
 node^.AddLiteral(FunctionControl.None,'None');
 node^.AddParam(tftype);

 //OpLabel
 node:=NewLabelOp(True);
 Main^.AddSpirvOp(node);

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

 InitFlow;
end;

procedure TSprvEmit.SetUserData(pData:Pointer);
begin
 DataLayoutList.SetUserData(pData);
end;

Procedure TSprvEmit.InitVs(RSRC2:TSPI_SHADER_PGM_RSRC2_VS;instance:Byte);
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

 if (instance>=1) then
 begin
  AddInput(@RegsStory.VGRP[p],dtUint32,itVInstance,1);
  p:=p+1;
 end;

 if (instance>=2) then
 begin
  AddInput(@RegsStory.VGRP[p],dtUint32,itVInstance,2);
  p:=p+1;
 end;

 if (instance>=3) then
 begin
  AddInput(@RegsStory.VGRP[p],dtUint32,itVInstance,0);
 end;

 AddCapability(Capability.Shader);

end;

Procedure TSprvEmit.InitPs(RSRC2:TSPI_SHADER_PGM_RSRC2_PS;ENA:TSPI_PS_INPUT_ENA);
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
 if (ENA.PERSP_SAMPLE_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspSample,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspSample,1);
  p:=p+1;
 end;

 if (ENA.PERSP_CENTER_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspCenter,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspCenter,1);
  p:=p+1;
 end;

 if (ENA.PERSP_CENTROID_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspCentroid,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspCentroid,1);
  p:=p+1;
 end;

 if (ENA.PERSP_PULL_MODEL_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspW,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspW,1);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itPerspW,2);
  p:=p+1;
 end;

 if (ENA.LINEAR_SAMPLE_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itLinearSample,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itLinearSample,1);
  p:=p+1;
 end;

 if (ENA.LINEAR_CENTER_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itLinearCenter,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itLinearCenter,1);
  p:=p+1;
 end;

 if (ENA.LINEAR_CENTROID_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtFloat32,itLinearCentroid,0);
  p:=p+1;
  AddInput(@RegsStory.VGRP[p],dtFloat32,itLinearCentroid,1);
  p:=p+1;
 end;

 if (ENA.POS_X_FLOAT_ENA<>0) then
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,0);
  p:=p+1;
 end;

 if (ENA.POS_Y_FLOAT_ENA<>0) then
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,1);
  p:=p+1;
 end;

 if (ENA.POS_Z_FLOAT_ENA<>0) then
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,2);
  p:=p+1;
 end;

 if (ENA.POS_W_FLOAT_ENA<>0) then
 begin
  AddVecInput(@RegsStory.VGRP[p],dtVec4f,dtFloat32,itFloatPos,3);
  p:=p+1;
 end;

 if (ENA.FRONT_FACE_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtBool,itFrontFace);
  p:=p+1;
 end;

 if (ENA.ANCILLARY_ENA<>0) then
 begin
  AddAncillary(@RegsStory.VGRP[p]);
  p:=p+1;
 end;

 if (ENA.SAMPLE_COVERAGE_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtUint32,itSampleCoverage);
  p:=p+1;
 end;

 if (ENA.POS_FIXED_PT_ENA<>0) then
 begin
  AddInput(@RegsStory.VGRP[p],dtUint32,itPosFixed);
  p:=p+1;
  //Per-pixel fixed point position Y[31:16], X[15:0]
 end;

 AddCapability(Capability.Shader);

end;

Procedure TSprvEmit.InitCs(RSRC2:TCOMPUTE_PGM_RSRC2;NTX:TCOMPUTE_NUM_THREAD_X;NTY:TCOMPUTE_NUM_THREAD_Y;NTZ:TCOMPUTE_NUM_THREAD_Z);
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

 FLocalSize.x:=NTX.NUM_THREAD_FULL+NTX.NUM_THREAD_PARTIAL;
 FLocalSize.y:=NTY.NUM_THREAD_FULL+NTY.NUM_THREAD_PARTIAL;
 FLocalSize.z:=NTZ.NUM_THREAD_FULL+NTZ.NUM_THREAD_PARTIAL;

 if (FLocalSize.x=0) then FLocalSize.x:=1;
 if (FLocalSize.y=0) then FLocalSize.y:=1;
 if (FLocalSize.z=0) then FLocalSize.z:=1;

 AddCapability(Capability.Shader);
end;

//

procedure TSprvEmit.emit_spi;
var
 obj:TObject absolute Self;
begin
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
  //W_DS    :TEmit_DS    (obj).emit_DS;
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

