unit si_ci_vi_merged_groups;

{$mode objfpc}{$H+}

interface

uses
 si_ci_vi_merged_registers;

type
 TRENDER_TARGET=packed record
  BASE       :TCB_COLOR0_BASE       ; //mmCB_COLOR0_BASE_DEFAULT
  PITCH      :TCB_COLOR0_PITCH      ; //mmCB_COLOR0_PITCH_DEFAULT
  SLICE      :TCB_COLOR0_SLICE      ; //mmCB_COLOR0_SLICE_DEFAULT
  VIEW       :TCB_COLOR0_VIEW       ; //mmCB_COLOR0_VIEW_DEFAULT
  INFO       :TCB_COLOR0_INFO       ; //mmCB_COLOR0_INFO_DEFAULT
  ATTRIB     :TCB_COLOR0_ATTRIB     ; //mmCB_COLOR0_ATTRIB_DEFAULT
  DCC_CONTROL:TCB_COLOR0_DCC_CONTROL; //mmCB_COLOR0_DCC_CONTROL_DEFAULT
  CMASK      :TCB_COLOR0_CMASK      ; //mmCB_COLOR0_CMASK_DEFAULT
  CMASK_SLICE:TCB_COLOR0_CMASK_SLICE; //mmCB_COLOR0_CMASK_SLICE_DEFAULT
  FMASK      :TCB_COLOR0_FMASK      ; //mmCB_COLOR0_FMASK_DEFAULT
  FMASK_SLICE:TCB_COLOR0_FMASK_SLICE; //mmCB_COLOR0_FMASK_SLICE_DEFAULT
  CLEAR_WORD :QWORD;                  //mmCB_COLOR0_CLEAR_WORD0_DEFAULT
                                      //mmCB_COLOR0_CLEAR_WORD1_DEFAULT
  DCC_BASE   :TCB_COLOR0_DCC_BASE   ; //mmCB_COLOR0_DCC_BASE_DEFAULT
  ALIGN      :DWORD;
 end;

 TGB_CLIP=packed record
  VERT_CLIP_ADJ:Single;
  VERT_DISC_ADJ:Single;
  HORZ_CLIP_ADJ:Single;
  HORZ_DISC_ADJ:Single;
 end; 

 TVPORT_SCISSOR=packed record
  TL:TPA_SC_VPORT_SCISSOR_0_TL;
  BR:TPA_SC_VPORT_SCISSOR_0_BR;
 end;

 TVPORT_ZMIN_MAX=packed record
  ZMIN:Single;
  ZMAX:Single;
 end;

 TVPORT_SCALE_OFFSET=packed record
  XSCALE :Single;
  XOFFSET:Single;
  YSCALE :Single;
  YOFFSET:Single;
  ZSCALE :Single;
  ZOFFSET:Single;
 end;

 TSPI_USER_DATA=array[0..15] of DWORD;

 TSH_REG_GROUP=bitpacked record
  SPI_SHADER_TBA_LO_PS          :TSPI_SHADER_TBA_LO_PS;           // 0x2C00
  SPI_SHADER_TBA_HI_PS          :TSPI_SHADER_TBA_HI_PS;           // 0x2C01
  SPI_SHADER_TMA_LO_PS          :TSPI_SHADER_TMA_LO_PS;           // 0x2C02
  SPI_SHADER_TMA_HI_PS          :TSPI_SHADER_TMA_HI_PS;           // 0x2C03
  REG_2C04_2C06                 :array[0..2] of DWORD;            // 0x2C04
  SPI_SHADER_PGM_RSRC3_PS       :TSPI_SHADER_PGM_RSRC3_PS;        // 0x2C07
  SPI_SHADER_PGM_LO_PS          :TSPI_SHADER_PGM_LO_PS;           // 0x2C08
  SPI_SHADER_PGM_HI_PS          :TSPI_SHADER_PGM_HI_PS;           // 0x2C09
  SPI_SHADER_PGM_RSRC1_PS       :TSPI_SHADER_PGM_RSRC1_PS;        // 0x2C0A
  SPI_SHADER_PGM_RSRC2_PS       :TSPI_SHADER_PGM_RSRC2_PS;        // 0x2C0B
  SPI_SHADER_USER_DATA_PS       :TSPI_USER_DATA;                  // 0x2C0C
  REG_2C1C_2C3F                 :array[0..35] of DWORD;           // 0x2C1C
  SPI_SHADER_TBA_LO_VS          :TSPI_SHADER_TBA_LO_VS;           // 0x2C40
  SPI_SHADER_TBA_HI_VS          :TSPI_SHADER_TBA_HI_VS;           // 0x2C41
  SPI_SHADER_TMA_LO_VS          :TSPI_SHADER_TMA_LO_VS;           // 0x2C42
  SPI_SHADER_TMA_HI_VS          :TSPI_SHADER_TMA_HI_VS;           // 0x2C43
  REG_2C44_2C45                 :array[0..1] of DWORD;            // 0x2C44
  SPI_SHADER_PGM_RSRC3_VS       :TSPI_SHADER_PGM_RSRC3_VS;        // 0x2C46
  SPI_SHADER_LATE_ALLOC_VS      :TSPI_SHADER_LATE_ALLOC_VS;       // 0x2C47
  SPI_SHADER_PGM_LO_VS          :TSPI_SHADER_PGM_LO_VS;           // 0x2C48
  SPI_SHADER_PGM_HI_VS          :TSPI_SHADER_PGM_HI_VS;           // 0x2C49
  SPI_SHADER_PGM_RSRC1_VS       :TSPI_SHADER_PGM_RSRC1_VS;        // 0x2C4A
  SPI_SHADER_PGM_RSRC2_VS       :TSPI_SHADER_PGM_RSRC2_VS;        // 0x2C4B
  SPI_SHADER_USER_DATA_VS       :TSPI_USER_DATA;                  // 0x2C4C
  REG_2C5C_2C7B                 :array[0..31] of DWORD;           // 0x2C5C
  SPI_SHADER_PGM_RSRC2_ES_VS    :TSPI_SHADER_PGM_RSRC2_ES_VS;     // 0x2C7C
  SPI_SHADER_PGM_RSRC2_LS_VS    :TSPI_SHADER_PGM_RSRC2_LS_VS;     // 0x2C7D
  REG_2C7E_2C7F                 :array[0..1] of DWORD;            // 0x2C7E
  SPI_SHADER_TBA_LO_GS          :TSPI_SHADER_TBA_LO_GS;           // 0x2C80
  SPI_SHADER_TBA_HI_GS          :TSPI_SHADER_TBA_HI_GS;           // 0x2C81
  SPI_SHADER_TMA_LO_GS          :TSPI_SHADER_TMA_LO_GS;           // 0x2C82
  SPI_SHADER_TMA_HI_GS          :TSPI_SHADER_TMA_HI_GS;           // 0x2C83
  REG_2C84_2C86                 :array[0..2] of DWORD;            // 0x2C84
  SPI_SHADER_PGM_RSRC3_GS       :TSPI_SHADER_PGM_RSRC3_GS;        // 0x2C87
  SPI_SHADER_PGM_LO_GS          :TSPI_SHADER_PGM_LO_GS;           // 0x2C88
  SPI_SHADER_PGM_HI_GS          :TSPI_SHADER_PGM_HI_GS;           // 0x2C89
  SPI_SHADER_PGM_RSRC1_GS       :TSPI_SHADER_PGM_RSRC1_GS;        // 0x2C8A
  SPI_SHADER_PGM_RSRC2_GS       :TSPI_SHADER_PGM_RSRC2_GS;        // 0x2C8B
  SPI_SHADER_USER_DATA_GS       :TSPI_USER_DATA;                  // 0x2C8C
  REG_2C9C_2CBB                 :array[0..31] of DWORD;           // 0x2C9C
  SPI_SHADER_PGM_RSRC2_ES_GS    :TSPI_SHADER_PGM_RSRC2_ES_GS;     // 0x2CBC
  REG_2CBD_2CBF                 :array[0..2] of DWORD;            // 0x2CBD
  SPI_SHADER_TBA_LO_ES          :TSPI_SHADER_TBA_LO_ES;           // 0x2CC0
  SPI_SHADER_TBA_HI_ES          :TSPI_SHADER_TBA_HI_ES;           // 0x2CC1
  SPI_SHADER_TMA_LO_ES          :TSPI_SHADER_TMA_LO_ES;           // 0x2CC2
  SPI_SHADER_TMA_HI_ES          :TSPI_SHADER_TMA_HI_ES;           // 0x2CC3
  REG_2CC4_2CC6                 :array[0..2] of DWORD;            // 0x2CC4
  SPI_SHADER_PGM_RSRC3_ES       :TSPI_SHADER_PGM_RSRC3_ES;        // 0x2CC7
  SPI_SHADER_PGM_LO_ES          :TSPI_SHADER_PGM_LO_ES;           // 0x2CC8
  SPI_SHADER_PGM_HI_ES          :TSPI_SHADER_PGM_HI_ES;           // 0x2CC9
  SPI_SHADER_PGM_RSRC1_ES       :TSPI_SHADER_PGM_RSRC1_ES;        // 0x2CCA
  SPI_SHADER_PGM_RSRC2_ES       :TSPI_SHADER_PGM_RSRC2_ES;        // 0x2CCB
  SPI_SHADER_USER_DATA_ES       :TSPI_USER_DATA;                  // 0x2CCC
  REG_2CDC_2CFC                 :array[0..32] of DWORD;           // 0x2CDC
  SPI_SHADER_PGM_RSRC2_LS_ES    :TSPI_SHADER_PGM_RSRC2_LS_ES;     // 0x2CFD
  REG_2CFE_2CFF                 :array[0..1] of DWORD;            // 0x2CFE
  SPI_SHADER_TBA_LO_HS          :TSPI_SHADER_TBA_LO_HS;           // 0x2D00
  SPI_SHADER_TBA_HI_HS          :TSPI_SHADER_TBA_HI_HS;           // 0x2D01
  SPI_SHADER_TMA_LO_HS          :TSPI_SHADER_TMA_LO_HS;           // 0x2D02
  SPI_SHADER_TMA_HI_HS          :TSPI_SHADER_TMA_HI_HS;           // 0x2D03
  REG_2D04_2D06                 :array[0..2] of DWORD;            // 0x2D04
  SPI_SHADER_PGM_RSRC3_HS       :TSPI_SHADER_PGM_RSRC3_HS;        // 0x2D07
  SPI_SHADER_PGM_LO_HS          :TSPI_SHADER_PGM_LO_HS;           // 0x2D08
  SPI_SHADER_PGM_HI_HS          :TSPI_SHADER_PGM_HI_HS;           // 0x2D09
  SPI_SHADER_PGM_RSRC1_HS       :TSPI_SHADER_PGM_RSRC1_HS;        // 0x2D0A
  SPI_SHADER_PGM_RSRC2_HS       :TSPI_SHADER_PGM_RSRC2_HS;        // 0x2D0B
  SPI_SHADER_USER_DATA_HS       :TSPI_USER_DATA;                  // 0x2D0C
  REG_2D1C_2D3C                 :array[0..32] of DWORD;           // 0x2D1C
  SPI_SHADER_PGM_RSRC2_LS_HS    :TSPI_SHADER_PGM_RSRC2_LS_HS;     // 0x2D3D
  REG_2D3E_2D3F                 :array[0..1] of DWORD;            // 0x2D3E
  SPI_SHADER_TBA_LO_LS          :TSPI_SHADER_TBA_LO_LS;           // 0x2D40
  SPI_SHADER_TBA_HI_LS          :TSPI_SHADER_TBA_HI_LS;           // 0x2D41
  SPI_SHADER_TMA_LO_LS          :TSPI_SHADER_TMA_LO_LS;           // 0x2D42
  SPI_SHADER_TMA_HI_LS          :TSPI_SHADER_TMA_HI_LS;           // 0x2D43
  REG_2D44_2D46                 :array[0..2] of DWORD;            // 0x2D44
  SPI_SHADER_PGM_RSRC3_LS       :TSPI_SHADER_PGM_RSRC3_LS;        // 0x2D47
  SPI_SHADER_PGM_LO_LS          :TSPI_SHADER_PGM_LO_LS;           // 0x2D48
  SPI_SHADER_PGM_HI_LS          :TSPI_SHADER_PGM_HI_LS;           // 0x2D49
  SPI_SHADER_PGM_RSRC1_LS       :TSPI_SHADER_PGM_RSRC1_LS;        // 0x2D4A
  SPI_SHADER_PGM_RSRC2_LS       :TSPI_SHADER_PGM_RSRC2_LS;        // 0x2D4B
  SPI_SHADER_USER_DATA_LS       :TSPI_USER_DATA;                  // 0x2D4C
  REG_2D5C_2DFF                 :array[0..163] of DWORD;          // 0x2D5C
  COMPUTE_DISPATCH_INITIATOR    :TCOMPUTE_DISPATCH_INITIATOR;     // 0x2E00
  COMPUTE_DIM_X                 :TCOMPUTE_DIM_X;                  // 0x2E01
  COMPUTE_DIM_Y                 :TCOMPUTE_DIM_Y;                  // 0x2E02
  COMPUTE_DIM_Z                 :TCOMPUTE_DIM_Z;                  // 0x2E03
  COMPUTE_START_X               :TCOMPUTE_START_X;                // 0x2E04
  COMPUTE_START_Y               :TCOMPUTE_START_Y;                // 0x2E05
  COMPUTE_START_Z               :TCOMPUTE_START_Z;                // 0x2E06
  COMPUTE_NUM_THREAD_X          :TCOMPUTE_NUM_THREAD_X;           // 0x2E07
  COMPUTE_NUM_THREAD_Y          :TCOMPUTE_NUM_THREAD_Y;           // 0x2E08
  COMPUTE_NUM_THREAD_Z          :TCOMPUTE_NUM_THREAD_Z;           // 0x2E09
  COMPUTE_PIPELINESTAT_ENABLE   :TCOMPUTE_PIPELINESTAT_ENABLE;    // 0x2E0A
  COMPUTE_PERFCOUNT_ENABLE      :TCOMPUTE_PERFCOUNT_ENABLE;       // 0x2E0B
  COMPUTE_PGM_LO                :TCOMPUTE_PGM_LO;                 // 0x2E0C
  COMPUTE_PGM_HI                :TCOMPUTE_PGM_HI;                 // 0x2E0D
  COMPUTE_TBA_LO                :TCOMPUTE_TBA_LO;                 // 0x2E0E
  COMPUTE_TBA_HI                :TCOMPUTE_TBA_HI;                 // 0x2E0F
  COMPUTE_TMA_LO                :TCOMPUTE_TMA_LO;                 // 0x2E10
  COMPUTE_TMA_HI                :TCOMPUTE_TMA_HI;                 // 0x2E11
  COMPUTE_PGM_RSRC1             :TCOMPUTE_PGM_RSRC1;              // 0x2E12
  COMPUTE_PGM_RSRC2             :TCOMPUTE_PGM_RSRC2;              // 0x2E13
  COMPUTE_VMID                  :TCOMPUTE_VMID;                   // 0x2E14
  COMPUTE_RESOURCE_LIMITS       :TCOMPUTE_RESOURCE_LIMITS;        // 0x2E15
  COMPUTE_STATIC_THREAD_MGMT_SE0:TCOMPUTE_STATIC_THREAD_MGMT_SE0; // 0x2E16
  COMPUTE_STATIC_THREAD_MGMT_SE1:TCOMPUTE_STATIC_THREAD_MGMT_SE1; // 0x2E17
  COMPUTE_TMPRING_SIZE          :TCOMPUTE_TMPRING_SIZE;           // 0x2E18
  COMPUTE_STATIC_THREAD_MGMT_SE2:TCOMPUTE_STATIC_THREAD_MGMT_SE2; // 0x2E19
  COMPUTE_STATIC_THREAD_MGMT_SE3:TCOMPUTE_STATIC_THREAD_MGMT_SE3; // 0x2E1A
  COMPUTE_RESTART_X             :TCOMPUTE_RESTART_X;              // 0x2E1B
  COMPUTE_RESTART_Y             :TCOMPUTE_RESTART_Y;              // 0x2E1C
  COMPUTE_RESTART_Z             :TCOMPUTE_RESTART_Z;              // 0x2E1D
  COMPUTE_THREAD_TRACE_ENABLE   :TCOMPUTE_THREAD_TRACE_ENABLE;    // 0x2E1E
  COMPUTE_MISC_RESERVED         :TCOMPUTE_MISC_RESERVED;          // 0x2E1F
  COMPUTE_DISPATCH_ID           :TCOMPUTE_DISPATCH_ID;            // 0x2E20
  COMPUTE_THREADGROUP_ID        :TCOMPUTE_THREADGROUP_ID;         // 0x2E21
  COMPUTE_RELAUNCH              :TCOMPUTE_RELAUNCH;               // 0x2E22
  COMPUTE_WAVE_RESTORE_ADDR_LO  :TCOMPUTE_WAVE_RESTORE_ADDR_LO;   // 0x2E23
  COMPUTE_WAVE_RESTORE_ADDR_HI  :TCOMPUTE_WAVE_RESTORE_ADDR_HI;   // 0x2E24
  COMPUTE_WAVE_RESTORE_CONTROL  :TCOMPUTE_WAVE_RESTORE_CONTROL;   // 0x2E25
  REG_2E26_2E3F                 :array[0..25] of DWORD;           // 0x2E26
  COMPUTE_USER_DATA             :TSPI_USER_DATA;                  // 0x2E40
  REG_2E50_2E7E                 :array[0..46] of DWORD;           // 0x2E50
  COMPUTE_NOWHERE               :TCOMPUTE_NOWHERE;                // 0x2E7F
 end;

 TCONTEXT_REG_GROUP=bitpacked record
  DB_RENDER_CONTROL                         :TDB_RENDER_CONTROL;                          // 0xA000
  DB_COUNT_CONTROL                          :TDB_COUNT_CONTROL;                           // 0xA001
  DB_DEPTH_VIEW                             :TDB_DEPTH_VIEW;                              // 0xA002
  DB_RENDER_OVERRIDE                        :TDB_RENDER_OVERRIDE;                         // 0xA003
  DB_RENDER_OVERRIDE2                       :TDB_RENDER_OVERRIDE2;                        // 0xA004
  DB_HTILE_DATA_BASE                        :TDB_HTILE_DATA_BASE;                         // 0xA005
  REG_A006_A007                             :array[0..1] of DWORD;                        // 0xA006
  DB_DEPTH_BOUNDS_MIN                       :TDB_DEPTH_BOUNDS_MIN;                        // 0xA008
  DB_DEPTH_BOUNDS_MAX                       :TDB_DEPTH_BOUNDS_MAX;                        // 0xA009
  DB_STENCIL_CLEAR                          :TDB_STENCIL_CLEAR;                           // 0xA00A
  DB_DEPTH_CLEAR                            :TDB_DEPTH_CLEAR;                             // 0xA00B
  PA_SC_SCREEN_SCISSOR_TL                   :TPA_SC_SCREEN_SCISSOR_TL;                    // 0xA00C
  PA_SC_SCREEN_SCISSOR_BR                   :TPA_SC_SCREEN_SCISSOR_BR;                    // 0xA00D
  REG_A00E                                  :DWORD;                                       // 0xA00E
  DB_DEPTH_INFO                             :TDB_DEPTH_INFO;                              // 0xA00F
  DB_Z_INFO                                 :TDB_Z_INFO;                                  // 0xA010
  DB_STENCIL_INFO                           :TDB_STENCIL_INFO;                            // 0xA011
  DB_Z_READ_BASE                            :TDB_Z_READ_BASE;                             // 0xA012
  DB_STENCIL_READ_BASE                      :TDB_STENCIL_READ_BASE;                       // 0xA013
  DB_Z_WRITE_BASE                           :TDB_Z_WRITE_BASE;                            // 0xA014
  DB_STENCIL_WRITE_BASE                     :TDB_STENCIL_WRITE_BASE;                      // 0xA015
  DB_DEPTH_SIZE                             :TDB_DEPTH_SIZE;                              // 0xA016
  DB_DEPTH_SLICE                            :TDB_DEPTH_SLICE;                             // 0xA017
  REG_A018_A01F                             :array[0..7] of DWORD;                        // 0xA018
  TA_BC_BASE_ADDR                           :TTA_BC_BASE_ADDR;                            // 0xA020
  TA_BC_BASE_ADDR_HI                        :TTA_BC_BASE_ADDR_HI;                         // 0xA021
  REG_A022_A07F                             :array[0..93] of DWORD;                       // 0xA022
  PA_SC_WINDOW_OFFSET                       :TPA_SC_WINDOW_OFFSET;                        // 0xA080
  PA_SC_WINDOW_SCISSOR_TL                   :TPA_SC_WINDOW_SCISSOR_TL;                    // 0xA081
  PA_SC_WINDOW_SCISSOR_BR                   :TPA_SC_WINDOW_SCISSOR_BR;                    // 0xA082
  PA_SC_CLIPRECT_RULE                       :TPA_SC_CLIPRECT_RULE;                        // 0xA083
  PA_SC_CLIPRECT_0_TL                       :TPA_SC_CLIPRECT_0_TL;                        // 0xA084
  PA_SC_CLIPRECT_0_BR                       :TPA_SC_CLIPRECT_0_BR;                        // 0xA085
  PA_SC_CLIPRECT_1_TL                       :TPA_SC_CLIPRECT_1_TL;                        // 0xA086
  PA_SC_CLIPRECT_1_BR                       :TPA_SC_CLIPRECT_1_BR;                        // 0xA087
  PA_SC_CLIPRECT_2_TL                       :TPA_SC_CLIPRECT_2_TL;                        // 0xA088
  PA_SC_CLIPRECT_2_BR                       :TPA_SC_CLIPRECT_2_BR;                        // 0xA089
  PA_SC_CLIPRECT_3_TL                       :TPA_SC_CLIPRECT_3_TL;                        // 0xA08A
  PA_SC_CLIPRECT_3_BR                       :TPA_SC_CLIPRECT_3_BR;                        // 0xA08B
  PA_SC_EDGERULE                            :TPA_SC_EDGERULE;                             // 0xA08C
  PA_SU_HARDWARE_SCREEN_OFFSET              :TPA_SU_HARDWARE_SCREEN_OFFSET;               // 0xA08D
  CB_TARGET_MASK                            :TCB_TARGET_MASK;                             // 0xA08E
  CB_SHADER_MASK                            :TCB_SHADER_MASK;                             // 0xA08F
  PA_SC_GENERIC_SCISSOR                     :TVPORT_SCISSOR;                              // 0xA090
  REG_A092_A093                             :array[0..1] of DWORD;                        // 0xA092
  PA_SC_VPORT_SCISSOR                       :array[0..15] of TVPORT_SCISSOR;              // 0xA094
  PA_SC_VPORT_ZMIN_MAX                      :array[0..15] of TVPORT_ZMIN_MAX;             // 0xA0B4
  PA_SC_RASTER_CONFIG                       :TPA_SC_RASTER_CONFIG;                        // 0xA0D4
  PA_SC_RASTER_CONFIG_1                     :TPA_SC_RASTER_CONFIG_1;                      // 0xA0D5
  REG_A0D6_A0D7                             :array[0..1] of DWORD;                        // 0xA0D6
  CP_PERFMON_CNTX_CNTL                      :TCP_PERFMON_CNTX_CNTL;                       // 0xA0D8
  CP_PIPEID                                 :TCP_PIPEID;                                  // 0xA0D9
  CP_VMID                                   :TCP_VMID;                                    // 0xA0DA
  REG_A0DB_A0FF                             :array[0..36] of DWORD;                       // 0xA0DB
  VGT_MAX_VTX_INDX                          :TVGT_MAX_VTX_INDX;                           // 0xA100
  VGT_MIN_VTX_INDX                          :TVGT_MIN_VTX_INDX;                           // 0xA101
  VGT_INDX_OFFSET                           :TVGT_INDX_OFFSET;                            // 0xA102
  VGT_MULTI_PRIM_IB_RESET_INDX              :TVGT_MULTI_PRIM_IB_RESET_INDX;               // 0xA103
  REG_A104                                  :DWORD;                                       // 0xA104
  CB_BLEND_RGBA                             :array[0..3] of Single;                       // 0xA105
  CB_DCC_CONTROL                            :TCB_DCC_CONTROL;                             // 0xA109
  REG_A10A                                  :DWORD;                                       // 0xA10A
  DB_STENCIL_CONTROL                        :TDB_STENCIL_CONTROL;                         // 0xA10B
  DB_STENCILREFMASK                         :TDB_STENCILREFMASK;                          // 0xA10C
  DB_STENCILREFMASK_BF                      :TDB_STENCILREFMASK_BF;                       // 0xA10D
  REG_A10E                                  :DWORD;                                       // 0xA10E
  PA_CL_VPORT_SCALE_OFFSET                  :array[0..15] of TVPORT_SCALE_OFFSET;         // 0xA10F
  PA_CL_UCP_0_X                             :TPA_CL_UCP_0_X;                              // 0xA16F
  PA_CL_UCP_0_Y                             :TPA_CL_UCP_0_Y;                              // 0xA170
  PA_CL_UCP_0_Z                             :TPA_CL_UCP_0_Z;                              // 0xA171
  PA_CL_UCP_0_W                             :TPA_CL_UCP_0_W;                              // 0xA172
  PA_CL_UCP_1_X                             :TPA_CL_UCP_1_X;                              // 0xA173
  PA_CL_UCP_1_Y                             :TPA_CL_UCP_1_Y;                              // 0xA174
  PA_CL_UCP_1_Z                             :TPA_CL_UCP_1_Z;                              // 0xA175
  PA_CL_UCP_1_W                             :TPA_CL_UCP_1_W;                              // 0xA176
  PA_CL_UCP_2_X                             :TPA_CL_UCP_2_X;                              // 0xA177
  PA_CL_UCP_2_Y                             :TPA_CL_UCP_2_Y;                              // 0xA178
  PA_CL_UCP_2_Z                             :TPA_CL_UCP_2_Z;                              // 0xA179
  PA_CL_UCP_2_W                             :TPA_CL_UCP_2_W;                              // 0xA17A
  PA_CL_UCP_3_X                             :TPA_CL_UCP_3_X;                              // 0xA17B
  PA_CL_UCP_3_Y                             :TPA_CL_UCP_3_Y;                              // 0xA17C
  PA_CL_UCP_3_Z                             :TPA_CL_UCP_3_Z;                              // 0xA17D
  PA_CL_UCP_3_W                             :TPA_CL_UCP_3_W;                              // 0xA17E
  PA_CL_UCP_4_X                             :TPA_CL_UCP_4_X;                              // 0xA17F
  PA_CL_UCP_4_Y                             :TPA_CL_UCP_4_Y;                              // 0xA180
  PA_CL_UCP_4_Z                             :TPA_CL_UCP_4_Z;                              // 0xA181
  PA_CL_UCP_4_W                             :TPA_CL_UCP_4_W;                              // 0xA182
  PA_CL_UCP_5_X                             :TPA_CL_UCP_5_X;                              // 0xA183
  PA_CL_UCP_5_Y                             :TPA_CL_UCP_5_Y;                              // 0xA184
  PA_CL_UCP_5_Z                             :TPA_CL_UCP_5_Z;                              // 0xA185
  PA_CL_UCP_5_W                             :TPA_CL_UCP_5_W;                              // 0xA186
  REG_A187_A190                             :array[0..9] of DWORD;                        // 0xA187
  SPI_PS_INPUT_CNTL                         :array[0..31] of TSPI_PS_INPUT_CNTL_0;        // 0xA191
  SPI_VS_OUT_CONFIG                         :TSPI_VS_OUT_CONFIG;                          // 0xA1B1
  REG_A1B2                                  :DWORD;                                       // 0xA1B2
  SPI_PS_INPUT_ENA                          :TSPI_PS_INPUT_ENA;                           // 0xA1B3
  SPI_PS_INPUT_ADDR                         :TSPI_PS_INPUT_ADDR;                          // 0xA1B4
  SPI_INTERP_CONTROL_0                      :TSPI_INTERP_CONTROL_0;                       // 0xA1B5
  SPI_PS_IN_CONTROL                         :TSPI_PS_IN_CONTROL;                          // 0xA1B6
  REG_A1B7                                  :DWORD;                                       // 0xA1B7
  SPI_BARYC_CNTL                            :TSPI_BARYC_CNTL;                             // 0xA1B8
  REG_A1B9                                  :DWORD;                                       // 0xA1B9
  SPI_TMPRING_SIZE                          :TSPI_TMPRING_SIZE;                           // 0xA1BA
  REG_A1BB_A1C2                             :array[0..7] of DWORD;                        // 0xA1BB
  SPI_SHADER_POS_FORMAT                     :TSPI_SHADER_POS_FORMAT;                      // 0xA1C3
  SPI_SHADER_Z_FORMAT                       :TSPI_SHADER_Z_FORMAT;                        // 0xA1C4
  SPI_SHADER_COL_FORMAT                     :TSPI_SHADER_COL_FORMAT;                      // 0xA1C5
  REG_A1C6_A1D4                             :array[0..14] of DWORD;                       // 0xA1C6
  SX_PS_DOWNCONVERT                         :TSX_PS_DOWNCONVERT;                          // 0xA1D5
  SX_BLEND_OPT_EPSILON                      :TSX_BLEND_OPT_EPSILON;                       // 0xA1D6
  SX_BLEND_OPT_CONTROL                      :TSX_BLEND_OPT_CONTROL;                       // 0xA1D7
  SX_MRT0_BLEND_OPT                         :TSX_MRT0_BLEND_OPT;                          // 0xA1D8
  SX_MRT1_BLEND_OPT                         :TSX_MRT1_BLEND_OPT;                          // 0xA1D9
  SX_MRT2_BLEND_OPT                         :TSX_MRT2_BLEND_OPT;                          // 0xA1DA
  SX_MRT3_BLEND_OPT                         :TSX_MRT3_BLEND_OPT;                          // 0xA1DB
  SX_MRT4_BLEND_OPT                         :TSX_MRT4_BLEND_OPT;                          // 0xA1DC
  SX_MRT5_BLEND_OPT                         :TSX_MRT5_BLEND_OPT;                          // 0xA1DD
  SX_MRT6_BLEND_OPT                         :TSX_MRT6_BLEND_OPT;                          // 0xA1DE
  SX_MRT7_BLEND_OPT                         :TSX_MRT7_BLEND_OPT;                          // 0xA1DF
  CB_BLEND_CONTROL                          :array[0..7] of TCB_BLEND0_CONTROL;           // 0xA1E0
  REG_A1E8_A1F4                             :array[0..12] of DWORD;                       // 0xA1E8
  PA_CL_POINT_X_RAD                         :TPA_CL_POINT_X_RAD;                          // 0xA1F5
  PA_CL_POINT_Y_RAD                         :TPA_CL_POINT_Y_RAD;                          // 0xA1F6
  PA_CL_POINT_SIZE                          :TPA_CL_POINT_SIZE;                           // 0xA1F7
  PA_CL_POINT_CULL_RAD                      :TPA_CL_POINT_CULL_RAD;                       // 0xA1F8
  VGT_DMA_BASE_HI                           :TVGT_DMA_BASE_HI;                            // 0xA1F9
  VGT_DMA_BASE                              :TVGT_DMA_BASE;                               // 0xA1FA
  REG_A1FB                                  :DWORD;                                       // 0xA1FB
  VGT_DRAW_INITIATOR                        :TVGT_DRAW_INITIATOR;                         // 0xA1FC
  VGT_IMMED_DATA                            :TVGT_IMMED_DATA;                             // 0xA1FD
  VGT_EVENT_ADDRESS_REG                     :TVGT_EVENT_ADDRESS_REG;                      // 0xA1FE
  REG_A1FF                                  :DWORD;                                       // 0xA1FF
  DB_DEPTH_CONTROL                          :TDB_DEPTH_CONTROL;                           // 0xA200
  DB_EQAA                                   :TDB_EQAA;                                    // 0xA201
  CB_COLOR_CONTROL                          :TCB_COLOR_CONTROL;                           // 0xA202
  DB_SHADER_CONTROL                         :TDB_SHADER_CONTROL;                          // 0xA203
  PA_CL_CLIP_CNTL                           :TPA_CL_CLIP_CNTL;                            // 0xA204
  PA_SU_SC_MODE_CNTL                        :TPA_SU_SC_MODE_CNTL;                         // 0xA205
  PA_CL_VTE_CNTL                            :TPA_CL_VTE_CNTL;                             // 0xA206
  PA_CL_VS_OUT_CNTL                         :TPA_CL_VS_OUT_CNTL;                          // 0xA207
  PA_CL_NANINF_CNTL                         :TPA_CL_NANINF_CNTL;                          // 0xA208
  PA_SU_LINE_STIPPLE_CNTL                   :TPA_SU_LINE_STIPPLE_CNTL;                    // 0xA209
  PA_SU_LINE_STIPPLE_SCALE                  :TPA_SU_LINE_STIPPLE_SCALE;                   // 0xA20A
  PA_SU_PRIM_FILTER_CNTL                    :TPA_SU_PRIM_FILTER_CNTL;                     // 0xA20B
  REG_A20C_A27F                             :array[0..115] of DWORD;                      // 0xA20C
  PA_SU_POINT_SIZE                          :TPA_SU_POINT_SIZE;                           // 0xA280
  PA_SU_POINT_MINMAX                        :TPA_SU_POINT_MINMAX;                         // 0xA281
  PA_SU_LINE_CNTL                           :TPA_SU_LINE_CNTL;                            // 0xA282
  PA_SC_LINE_STIPPLE                        :TPA_SC_LINE_STIPPLE;                         // 0xA283
  VGT_OUTPUT_PATH_CNTL                      :TVGT_OUTPUT_PATH_CNTL;                       // 0xA284
  VGT_HOS_CNTL                              :TVGT_HOS_CNTL;                               // 0xA285
  VGT_HOS_MAX_TESS_LEVEL                    :TVGT_HOS_MAX_TESS_LEVEL;                     // 0xA286
  VGT_HOS_MIN_TESS_LEVEL                    :TVGT_HOS_MIN_TESS_LEVEL;                     // 0xA287
  VGT_HOS_REUSE_DEPTH                       :TVGT_HOS_REUSE_DEPTH;                        // 0xA288
  VGT_GROUP_PRIM_TYPE                       :TVGT_GROUP_PRIM_TYPE;                        // 0xA289
  VGT_GROUP_FIRST_DECR                      :TVGT_GROUP_FIRST_DECR;                       // 0xA28A
  VGT_GROUP_DECR                            :TVGT_GROUP_DECR;                             // 0xA28B
  VGT_GROUP_VECT_0_CNTL                     :TVGT_GROUP_VECT_0_CNTL;                      // 0xA28C
  VGT_GROUP_VECT_1_CNTL                     :TVGT_GROUP_VECT_1_CNTL;                      // 0xA28D
  VGT_GROUP_VECT_0_FMT_CNTL                 :TVGT_GROUP_VECT_0_FMT_CNTL;                  // 0xA28E
  VGT_GROUP_VECT_1_FMT_CNTL                 :TVGT_GROUP_VECT_1_FMT_CNTL;                  // 0xA28F
  VGT_GS_MODE                               :TVGT_GS_MODE;                                // 0xA290
  VGT_GS_ONCHIP_CNTL                        :TVGT_GS_ONCHIP_CNTL;                         // 0xA291
  PA_SC_MODE_CNTL_0                         :TPA_SC_MODE_CNTL_0;                          // 0xA292
  PA_SC_MODE_CNTL_1                         :TPA_SC_MODE_CNTL_1;                          // 0xA293
  VGT_ENHANCE                               :TVGT_ENHANCE;                                // 0xA294
  VGT_GS_PER_ES                             :TVGT_GS_PER_ES;                              // 0xA295
  VGT_ES_PER_GS                             :TVGT_ES_PER_GS;                              // 0xA296
  VGT_GS_PER_VS                             :TVGT_GS_PER_VS;                              // 0xA297
  VGT_GSVS_RING_OFFSET_1                    :TVGT_GSVS_RING_OFFSET_1;                     // 0xA298
  VGT_GSVS_RING_OFFSET_2                    :TVGT_GSVS_RING_OFFSET_2;                     // 0xA299
  VGT_GSVS_RING_OFFSET_3                    :TVGT_GSVS_RING_OFFSET_3;                     // 0xA29A
  VGT_GS_OUT_PRIM_TYPE                      :TVGT_GS_OUT_PRIM_TYPE;                       // 0xA29B
  IA_ENHANCE                                :TIA_ENHANCE;                                 // 0xA29C
  VGT_DMA_SIZE                              :TVGT_DMA_SIZE;                               // 0xA29D
  VGT_DMA_MAX_SIZE                          :TVGT_DMA_MAX_SIZE;                           // 0xA29E
  VGT_DMA_INDEX_TYPE                        :TVGT_DMA_INDEX_TYPE;                         // 0xA29F
  WD_ENHANCE                                :TWD_ENHANCE;                                 // 0xA2A0
  VGT_PRIMITIVEID_EN                        :TVGT_PRIMITIVEID_EN;                         // 0xA2A1
  VGT_DMA_NUM_INSTANCES                     :TVGT_DMA_NUM_INSTANCES;                      // 0xA2A2
  VGT_PRIMITIVEID_RESET                     :TVGT_PRIMITIVEID_RESET;                      // 0xA2A3
  VGT_EVENT_INITIATOR                       :TVGT_EVENT_INITIATOR;                        // 0xA2A4
  VGT_MULTI_PRIM_IB_RESET_EN                :TVGT_MULTI_PRIM_IB_RESET_EN;                 // 0xA2A5
  REG_A2A6_A2A7                             :array[0..1] of DWORD;                        // 0xA2A6
  VGT_INSTANCE_STEP_RATE_0                  :TVGT_INSTANCE_STEP_RATE_0;                   // 0xA2A8
  VGT_INSTANCE_STEP_RATE_1                  :TVGT_INSTANCE_STEP_RATE_1;                   // 0xA2A9
  IA_MULTI_VGT_PARAM                        :TIA_MULTI_VGT_PARAM;                         // 0xA2AA
  VGT_ESGS_RING_ITEMSIZE                    :TVGT_ESGS_RING_ITEMSIZE;                     // 0xA2AB
  VGT_GSVS_RING_ITEMSIZE                    :TVGT_GSVS_RING_ITEMSIZE;                     // 0xA2AC
  VGT_REUSE_OFF                             :TVGT_REUSE_OFF;                              // 0xA2AD
  VGT_VTX_CNT_EN                            :TVGT_VTX_CNT_EN;                             // 0xA2AE
  DB_HTILE_SURFACE                          :TDB_HTILE_SURFACE;                           // 0xA2AF
  DB_SRESULTS_COMPARE_STATE0                :TDB_SRESULTS_COMPARE_STATE0;                 // 0xA2B0
  DB_SRESULTS_COMPARE_STATE1                :TDB_SRESULTS_COMPARE_STATE1;                 // 0xA2B1
  DB_PRELOAD_CONTROL                        :TDB_PRELOAD_CONTROL;                         // 0xA2B2
  REG_A2B3                                  :DWORD;                                       // 0xA2B3
  VGT_STRMOUT_BUFFER_SIZE_0                 :TVGT_STRMOUT_BUFFER_SIZE_0;                  // 0xA2B4
  VGT_STRMOUT_VTX_STRIDE_0                  :TVGT_STRMOUT_VTX_STRIDE_0;                   // 0xA2B5
  REG_A2B6                                  :DWORD;                                       // 0xA2B6
  VGT_STRMOUT_BUFFER_OFFSET_0               :TVGT_STRMOUT_BUFFER_OFFSET_0;                // 0xA2B7
  VGT_STRMOUT_BUFFER_SIZE_1                 :TVGT_STRMOUT_BUFFER_SIZE_1;                  // 0xA2B8
  VGT_STRMOUT_VTX_STRIDE_1                  :TVGT_STRMOUT_VTX_STRIDE_1;                   // 0xA2B9
  REG_A2BA                                  :DWORD;                                       // 0xA2BA
  VGT_STRMOUT_BUFFER_OFFSET_1               :TVGT_STRMOUT_BUFFER_OFFSET_1;                // 0xA2BB
  VGT_STRMOUT_BUFFER_SIZE_2                 :TVGT_STRMOUT_BUFFER_SIZE_2;                  // 0xA2BC
  VGT_STRMOUT_VTX_STRIDE_2                  :TVGT_STRMOUT_VTX_STRIDE_2;                   // 0xA2BD
  REG_A2BE                                  :DWORD;                                       // 0xA2BE
  VGT_STRMOUT_BUFFER_OFFSET_2               :TVGT_STRMOUT_BUFFER_OFFSET_2;                // 0xA2BF
  VGT_STRMOUT_BUFFER_SIZE_3                 :TVGT_STRMOUT_BUFFER_SIZE_3;                  // 0xA2C0
  VGT_STRMOUT_VTX_STRIDE_3                  :TVGT_STRMOUT_VTX_STRIDE_3;                   // 0xA2C1
  REG_A2C2                                  :DWORD;                                       // 0xA2C2
  VGT_STRMOUT_BUFFER_OFFSET_3               :TVGT_STRMOUT_BUFFER_OFFSET_3;                // 0xA2C3
  REG_A2C4_A2C9                             :array[0..5] of DWORD;                        // 0xA2C4
  VGT_STRMOUT_DRAW_OPAQUE_OFFSET            :TVGT_STRMOUT_DRAW_OPAQUE_OFFSET;             // 0xA2CA
  VGT_STRMOUT_DRAW_OPAQUE_BUFFER_FILLED_SIZE:TVGT_STRMOUT_DRAW_OPAQUE_BUFFER_FILLED_SIZE; // 0xA2CB
  VGT_STRMOUT_DRAW_OPAQUE_VERTEX_STRIDE     :TVGT_STRMOUT_DRAW_OPAQUE_VERTEX_STRIDE;      // 0xA2CC
  REG_A2CD                                  :DWORD;                                       // 0xA2CD
  VGT_GS_MAX_VERT_OUT                       :TVGT_GS_MAX_VERT_OUT;                        // 0xA2CE
  REG_A2CF_A2D3                             :array[0..4] of DWORD;                        // 0xA2CF
  VGT_TESS_DISTRIBUTION                     :TVGT_TESS_DISTRIBUTION;                      // 0xA2D4
  VGT_SHADER_STAGES_EN                      :TVGT_SHADER_STAGES_EN;                       // 0xA2D5
  VGT_LS_HS_CONFIG                          :TVGT_LS_HS_CONFIG;                           // 0xA2D6
  VGT_GS_VERT_ITEMSIZE                      :TVGT_GS_VERT_ITEMSIZE;                       // 0xA2D7
  VGT_GS_VERT_ITEMSIZE_1                    :TVGT_GS_VERT_ITEMSIZE_1;                     // 0xA2D8
  VGT_GS_VERT_ITEMSIZE_2                    :TVGT_GS_VERT_ITEMSIZE_2;                     // 0xA2D9
  VGT_GS_VERT_ITEMSIZE_3                    :TVGT_GS_VERT_ITEMSIZE_3;                     // 0xA2DA
  VGT_TF_PARAM                              :TVGT_TF_PARAM;                               // 0xA2DB
  DB_ALPHA_TO_MASK                          :TDB_ALPHA_TO_MASK;                           // 0xA2DC
  VGT_DISPATCH_DRAW_INDEX                   :TVGT_DISPATCH_DRAW_INDEX;                    // 0xA2DD
  PA_SU_POLY_OFFSET_DB_FMT_CNTL             :TPA_SU_POLY_OFFSET_DB_FMT_CNTL;              // 0xA2DE
  PA_SU_POLY_OFFSET_CLAMP                   :TPA_SU_POLY_OFFSET_CLAMP;                    // 0xA2DF
  PA_SU_POLY_OFFSET_FRONT_SCALE             :TPA_SU_POLY_OFFSET_FRONT_SCALE;              // 0xA2E0
  PA_SU_POLY_OFFSET_FRONT_OFFSET            :TPA_SU_POLY_OFFSET_FRONT_OFFSET;             // 0xA2E1
  PA_SU_POLY_OFFSET_BACK_SCALE              :TPA_SU_POLY_OFFSET_BACK_SCALE;               // 0xA2E2
  PA_SU_POLY_OFFSET_BACK_OFFSET             :TPA_SU_POLY_OFFSET_BACK_OFFSET;              // 0xA2E3
  VGT_GS_INSTANCE_CNT                       :TVGT_GS_INSTANCE_CNT;                        // 0xA2E4
  VGT_STRMOUT_CONFIG                        :TVGT_STRMOUT_CONFIG;                         // 0xA2E5
  VGT_STRMOUT_BUFFER_CONFIG                 :TVGT_STRMOUT_BUFFER_CONFIG;                  // 0xA2E6
  REG_A2E7_A2F4                             :array[0..13] of DWORD;                       // 0xA2E7
  PA_SC_CENTROID_PRIORITY_0                 :TPA_SC_CENTROID_PRIORITY_0;                  // 0xA2F5
  PA_SC_CENTROID_PRIORITY_1                 :TPA_SC_CENTROID_PRIORITY_1;                  // 0xA2F6
  PA_SC_LINE_CNTL                           :TPA_SC_LINE_CNTL;                            // 0xA2F7
  PA_SC_AA_CONFIG                           :TPA_SC_AA_CONFIG;                            // 0xA2F8
  PA_SU_VTX_CNTL                            :TPA_SU_VTX_CNTL;                             // 0xA2F9
  GB_CLIP                                   :TGB_CLIP;                                    // 0xA2FA
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_0         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_0;          // 0xA2FE
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_1         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_1;          // 0xA2FF
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_2         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_2;          // 0xA300
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_3         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_3;          // 0xA301
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_0         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_0;          // 0xA302
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_1         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_1;          // 0xA303
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_2         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_2;          // 0xA304
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_3         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_3;          // 0xA305
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_0         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_0;          // 0xA306
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_1         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_1;          // 0xA307
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_2         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_2;          // 0xA308
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_3         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_3;          // 0xA309
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_0         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_0;          // 0xA30A
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_1         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_1;          // 0xA30B
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_2         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_2;          // 0xA30C
  PA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_3         :TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_3;          // 0xA30D
  PA_SC_AA_MASK_X0Y0_X1Y0                   :TPA_SC_AA_MASK_X0Y0_X1Y0;                    // 0xA30E
  PA_SC_AA_MASK_X0Y1_X1Y1                   :TPA_SC_AA_MASK_X0Y1_X1Y1;                    // 0xA30F
  REG_A310_A315                             :array[0..5] of DWORD;                        // 0xA310
  VGT_VERTEX_REUSE_BLOCK_CNTL               :TVGT_VERTEX_REUSE_BLOCK_CNTL;                // 0xA316
  VGT_OUT_DEALLOC_CNTL                      :TVGT_OUT_DEALLOC_CNTL;                       // 0xA317
  RENDER_TARGET                             :array[0..7] of TRENDER_TARGET;               // 0xA318
 end;

 TCONFIG_SPACE_GROUP=bitpacked record
  GRBM_CNTL                     :TGRBM_CNTL;                      // 0x2000
  GRBM_SKEW_CNTL                :TGRBM_SKEW_CNTL;                 // 0x2001
  GRBM_STATUS2                  :TGRBM_STATUS2;                   // 0x2002
  GRBM_PWR_CNTL                 :TGRBM_PWR_CNTL;                  // 0x2003
  GRBM_STATUS                   :TGRBM_STATUS;                    // 0x2004
  GRBM_STATUS_SE0               :TGRBM_STATUS_SE0;                // 0x2005
  GRBM_STATUS_SE1               :TGRBM_STATUS_SE1;                // 0x2006
  REG_2007                      :DWORD;                           // 0x2007
  GRBM_SOFT_RESET               :TGRBM_SOFT_RESET;                // 0x2008
  GRBM_DEBUG_CNTL               :TGRBM_DEBUG_CNTL;                // 0x2009
  GRBM_DEBUG_DATA               :TGRBM_DEBUG_DATA;                // 0x200A
  REG_200B                      :DWORD;                           // 0x200B
  GRBM_GFX_CLKEN_CNTL           :TGRBM_GFX_CLKEN_CNTL;            // 0x200C
  GRBM_WAIT_IDLE_CLOCKS         :TGRBM_WAIT_IDLE_CLOCKS;          // 0x200D
  GRBM_STATUS_SE2               :TGRBM_STATUS_SE2;                // 0x200E
  GRBM_STATUS_SE3               :TGRBM_STATUS_SE3;                // 0x200F
  REG_2010_2013                 :array[0..3] of DWORD;            // 0x2010
  GRBM_DEBUG                    :TGRBM_DEBUG;                     // 0x2014
  GRBM_DEBUG_SNAPSHOT           :TGRBM_DEBUG_SNAPSHOT;            // 0x2015
  GRBM_READ_ERROR               :TGRBM_READ_ERROR;                // 0x2016
  GRBM_READ_ERROR2              :TGRBM_READ_ERROR2;               // 0x2017
  GRBM_INT_CNTL                 :TGRBM_INT_CNTL;                  // 0x2018
  GRBM_TRAP_OP                  :TGRBM_TRAP_OP;                   // 0x2019
  GRBM_TRAP_ADDR                :TGRBM_TRAP_ADDR;                 // 0x201A
  GRBM_TRAP_ADDR_MSK            :TGRBM_TRAP_ADDR_MSK;             // 0x201B
  GRBM_TRAP_WD                  :TGRBM_TRAP_WD;                   // 0x201C
  GRBM_TRAP_WD_MSK              :TGRBM_TRAP_WD_MSK;               // 0x201D
  GRBM_DSM_BYPASS               :TGRBM_DSM_BYPASS;                // 0x201E
  GRBM_WRITE_ERROR              :TGRBM_WRITE_ERROR;               // 0x201F
  REG_2020_203E                 :array[0..30] of DWORD;           // 0x2020
  GRBM_NOWHERE                  :TGRBM_NOWHERE;                   // 0x203F
  GRBM_SCRATCH_REG0             :TGRBM_SCRATCH_REG0;              // 0x2040
  GRBM_SCRATCH_REG1             :TGRBM_SCRATCH_REG1;              // 0x2041
  GRBM_SCRATCH_REG2             :TGRBM_SCRATCH_REG2;              // 0x2042
  GRBM_SCRATCH_REG3             :TGRBM_SCRATCH_REG3;              // 0x2043
  GRBM_SCRATCH_REG4             :TGRBM_SCRATCH_REG4;              // 0x2044
  GRBM_SCRATCH_REG5             :TGRBM_SCRATCH_REG5;              // 0x2045
  GRBM_SCRATCH_REG6             :TGRBM_SCRATCH_REG6;              // 0x2046
  GRBM_SCRATCH_REG7             :TGRBM_SCRATCH_REG7;              // 0x2047
  REG_2048_2083                 :array[0..59] of DWORD;           // 0x2048
  CP_CPC_STATUS                 :TCP_CPC_STATUS;                  // 0x2084
  CP_CPC_BUSY_STAT              :TCP_CPC_BUSY_STAT;               // 0x2085
  CP_CPC_STALLED_STAT1          :TCP_CPC_STALLED_STAT1;           // 0x2086
  CP_CPF_STATUS                 :TCP_CPF_STATUS;                  // 0x2087
  CP_CPF_BUSY_STAT              :TCP_CPF_BUSY_STAT;               // 0x2088
  CP_CPF_STALLED_STAT1          :TCP_CPF_STALLED_STAT1;           // 0x2089
  REG_208A                      :DWORD;                           // 0x208A
  CP_CPC_GRBM_FREE_COUNT        :TCP_CPC_GRBM_FREE_COUNT;         // 0x208B
  REG_208C                      :DWORD;                           // 0x208C
  CP_MEC_CNTL                   :TCP_MEC_CNTL;                    // 0x208D
  CP_MEC_ME1_HEADER_DUMP        :TCP_MEC_ME1_HEADER_DUMP;         // 0x208E
  CP_MEC_ME2_HEADER_DUMP        :TCP_MEC_ME2_HEADER_DUMP;         // 0x208F
  CP_CPC_SCRATCH_INDEX          :TCP_CPC_SCRATCH_INDEX;           // 0x2090
  CP_CPC_SCRATCH_DATA           :TCP_CPC_SCRATCH_DATA;            // 0x2091
  REG_2092_20A6                 :array[0..20] of DWORD;           // 0x2092
  CP_CPC_HALT_HYST_COUNT        :TCP_CPC_HALT_HYST_COUNT;         // 0x20A7
  REG_20A8_20AC                 :array[0..4] of DWORD;            // 0x20A8
  CP_PRT_LOD_STATS_CNTL0        :TCP_PRT_LOD_STATS_CNTL0;         // 0x20AD
  CP_PRT_LOD_STATS_CNTL1        :TCP_PRT_LOD_STATS_CNTL1;         // 0x20AE
  CP_PRT_LOD_STATS_CNTL2        :TCP_PRT_LOD_STATS_CNTL2;         // 0x20AF
  REG_20B0_20BF                 :array[0..15] of DWORD;           // 0x20B0
  CP_CE_COMPARE_COUNT           :TCP_CE_COMPARE_COUNT;            // 0x20C0
  CP_CE_DE_COUNT                :TCP_CE_DE_COUNT;                 // 0x20C1
  CP_DE_CE_COUNT                :TCP_DE_CE_COUNT;                 // 0x20C2
  CP_DE_LAST_INVAL_COUNT        :TCP_DE_LAST_INVAL_COUNT;         // 0x20C3
  CP_DE_DE_COUNT                :TCP_DE_DE_COUNT;                 // 0x20C4
  REG_20C5_219B                 :array[0..214] of DWORD;          // 0x20C5
  CP_STALLED_STAT3              :TCP_STALLED_STAT3;               // 0x219C
  CP_STALLED_STAT1              :TCP_STALLED_STAT1;               // 0x219D
  CP_STALLED_STAT2              :TCP_STALLED_STAT2;               // 0x219E
  CP_BUSY_STAT                  :TCP_BUSY_STAT;                   // 0x219F
  CP_STAT                       :TCP_STAT;                        // 0x21A0
  REG_21A1                      :DWORD;                           // 0x21A1
  CP_PFP_HEADER_DUMP            :TCP_PFP_HEADER_DUMP;             // 0x21A2
  CP_GRBM_FREE_COUNT            :TCP_GRBM_FREE_COUNT;             // 0x21A3
  CP_CE_HEADER_DUMP             :TCP_CE_HEADER_DUMP;              // 0x21A4
  REG_21A5_21B3                 :array[0..14] of DWORD;           // 0x21A5
  CP_CSF_STAT                   :TCP_CSF_STAT;                    // 0x21B4
  CP_CSF_CNTL                   :TCP_CSF_CNTL;                    // 0x21B5
  REG_21B6_21B7                 :array[0..1] of DWORD;            // 0x21B6
  CP_CNTX_STAT                  :TCP_CNTX_STAT;                   // 0x21B8
  REG_21B9_21BB                 :array[0..2] of DWORD;            // 0x21B9
  CP_ROQ_THRESHOLDS             :TCP_ROQ_THRESHOLDS;              // 0x21BC
  CP_MEQ_STQ_THRESHOLD          :TCP_MEQ_STQ_THRESHOLD;           // 0x21BD
  CP_RB2_RPTR                   :TCP_RB2_RPTR;                    // 0x21BE
  CP_RB1_RPTR                   :TCP_RB1_RPTR;                    // 0x21BF
  CP_RB0_RPTR                   :TCP_RB0_RPTR;                    // 0x21C0
  REG_21C1_21D4                 :array[0..19] of DWORD;           // 0x21C1
  CP_ROQ1_THRESHOLDS            :TCP_ROQ1_THRESHOLDS;             // 0x21D5
  CP_ROQ2_THRESHOLDS            :TCP_ROQ2_THRESHOLDS;             // 0x21D6
  CP_STQ_THRESHOLDS             :TCP_STQ_THRESHOLDS;              // 0x21D7
  CP_QUEUE_THRESHOLDS           :TCP_QUEUE_THRESHOLDS;            // 0x21D8
  CP_MEQ_THRESHOLDS             :TCP_MEQ_THRESHOLDS;              // 0x21D9
  CP_ROQ_AVAIL                  :TCP_ROQ_AVAIL;                   // 0x21DA
  CP_STQ_AVAIL                  :TCP_STQ_AVAIL;                   // 0x21DB
  CP_ROQ2_AVAIL                 :TCP_ROQ2_AVAIL;                  // 0x21DC
  CP_MEQ_AVAIL                  :TCP_MEQ_AVAIL;                   // 0x21DD
  CP_CMD_INDEX                  :TCP_CMD_INDEX;                   // 0x21DE
  CP_CMD_DATA                   :TCP_CMD_DATA;                    // 0x21DF
  CP_ROQ_RB_STAT                :TCP_ROQ_RB_STAT;                 // 0x21E0
  CP_ROQ_IB1_STAT               :TCP_ROQ_IB1_STAT;                // 0x21E1
  CP_ROQ_IB2_STAT               :TCP_ROQ_IB2_STAT;                // 0x21E2
  CP_STQ_STAT                   :TCP_STQ_STAT;                    // 0x21E3
  CP_STQ_WR_STAT                :TCP_STQ_WR_STAT;                 // 0x21E4
  CP_MEQ_STAT                   :TCP_MEQ_STAT;                    // 0x21E5
  CP_CEQ1_AVAIL                 :TCP_CEQ1_AVAIL;                  // 0x21E6
  CP_CEQ2_AVAIL                 :TCP_CEQ2_AVAIL;                  // 0x21E7
  CP_CE_ROQ_RB_STAT             :TCP_CE_ROQ_RB_STAT;              // 0x21E8
  CP_CE_ROQ_IB1_STAT            :TCP_CE_ROQ_IB1_STAT;             // 0x21E9
  CP_CE_ROQ_IB2_STAT            :TCP_CE_ROQ_IB2_STAT;             // 0x21EA
  REG_21EB_21F6                 :array[0..11] of DWORD;           // 0x21EB
  CP_INT_STAT_DEBUG             :TCP_INT_STAT_DEBUG;              // 0x21F7
  REG_21F8_222B                 :array[0..51] of DWORD;           // 0x21F8
  VGT_VTX_VECT_EJECT_REG        :TVGT_VTX_VECT_EJECT_REG;         // 0x222C
  VGT_DMA_DATA_FIFO_DEPTH       :TVGT_DMA_DATA_FIFO_DEPTH;        // 0x222D
  VGT_DMA_REQ_FIFO_DEPTH        :TVGT_DMA_REQ_FIFO_DEPTH;         // 0x222E
  VGT_DRAW_INIT_FIFO_DEPTH      :TVGT_DRAW_INIT_FIFO_DEPTH;       // 0x222F
  VGT_LAST_COPY_STATE           :TVGT_LAST_COPY_STATE;            // 0x2230
  VGT_CACHE_INVALIDATION        :TVGT_CACHE_INVALIDATION;         // 0x2231
  VGT_RESET_DEBUG               :TVGT_RESET_DEBUG;                // 0x2232
  VGT_STRMOUT_DELAY             :TVGT_STRMOUT_DELAY;              // 0x2233
  VGT_FIFO_DEPTHS               :TVGT_FIFO_DEPTHS;                // 0x2234
  VGT_GS_VERTEX_REUSE           :TVGT_GS_VERTEX_REUSE;            // 0x2235
  VGT_MC_LAT_CNTL               :TVGT_MC_LAT_CNTL;                // 0x2236
  IA_CNTL_STATUS                :TIA_CNTL_STATUS;                 // 0x2237
  VGT_DEBUG_CNTL                :TVGT_DEBUG_CNTL;                 // 0x2238
  VGT_DEBUG_DATA                :TVGT_DEBUG_DATA;                 // 0x2239
  IA_DEBUG_CNTL                 :TIA_DEBUG_CNTL;                  // 0x223A
  IA_DEBUG_DATA                 :TIA_DEBUG_DATA;                  // 0x223B
  VGT_CNTL_STATUS               :TVGT_CNTL_STATUS;                // 0x223C
  WD_DEBUG_CNTL                 :TWD_DEBUG_CNTL;                  // 0x223D
  WD_DEBUG_DATA                 :TWD_DEBUG_DATA;                  // 0x223E
  WD_CNTL_STATUS                :TWD_CNTL_STATUS;                 // 0x223F
  REG_2240_2241                 :array[0..1] of DWORD;            // 0x2240
  WD_QOS                        :TWD_QOS;                         // 0x2242
  REG_2243_2262                 :array[0..31] of DWORD;           // 0x2243
  VGT_SYS_CONFIG                :TVGT_SYS_CONFIG;                 // 0x2263
  REG_2264_2267                 :array[0..3] of DWORD;            // 0x2264
  VGT_VS_MAX_WAVE_ID            :TVGT_VS_MAX_WAVE_ID;             // 0x2268
  REG_2269_2270                 :array[0..7] of DWORD;            // 0x2269
  VGT_DMA_PRIMITIVE_TYPE        :TVGT_DMA_PRIMITIVE_TYPE;         // 0x2271
  VGT_DMA_CONTROL               :TVGT_DMA_CONTROL;                // 0x2272
  VGT_DMA_LS_HS_CONFIG          :TVGT_DMA_LS_HS_CONFIG;           // 0x2273
  REG_2274_227F                 :array[0..11] of DWORD;           // 0x2274
  PA_SU_DEBUG_CNTL              :TPA_SU_DEBUG_CNTL;               // 0x2280
  PA_SU_DEBUG_DATA              :TPA_SU_DEBUG_DATA;               // 0x2281
  REG_2282_2283                 :array[0..1] of DWORD;            // 0x2282
  PA_CL_CNTL_STATUS             :TPA_CL_CNTL_STATUS;              // 0x2284
  PA_CL_ENHANCE                 :TPA_CL_ENHANCE;                  // 0x2285
  PA_CL_RESET_DEBUG             :TPA_CL_RESET_DEBUG;              // 0x2286
  REG_2287_2293                 :array[0..12] of DWORD;           // 0x2287
  PA_SU_CNTL_STATUS             :TPA_SU_CNTL_STATUS;              // 0x2294
  PA_SC_FIFO_DEPTH_CNTL         :TPA_SC_FIFO_DEPTH_CNTL;          // 0x2295
  REG_2296_22BF                 :array[0..41] of DWORD;           // 0x2296
  PA_SC_P3D_TRAP_SCREEN_HV_LOCK :TPA_SC_P3D_TRAP_SCREEN_HV_LOCK;  // 0x22C0
  PA_SC_HP3D_TRAP_SCREEN_HV_LOCK:TPA_SC_HP3D_TRAP_SCREEN_HV_LOCK; // 0x22C1
  PA_SC_TRAP_SCREEN_HV_LOCK     :TPA_SC_TRAP_SCREEN_HV_LOCK;      // 0x22C2
  REG_22C3_22C8                 :array[0..5] of DWORD;            // 0x22C3
  PA_SC_FORCE_EOV_MAX_CNTS      :TPA_SC_FORCE_EOV_MAX_CNTS;       // 0x22C9
  REG_22CA_22F2                 :array[0..40] of DWORD;           // 0x22CA
  PA_SC_FIFO_SIZE               :TPA_SC_FIFO_SIZE;                // 0x22F3
  REG_22F4                      :DWORD;                           // 0x22F4
  PA_SC_IF_FIFO_SIZE            :TPA_SC_IF_FIFO_SIZE;             // 0x22F5
  PA_SC_DEBUG_CNTL              :TPA_SC_DEBUG_CNTL;               // 0x22F6
  PA_SC_DEBUG_DATA              :TPA_SC_DEBUG_DATA;               // 0x22F7
  REG_22F8_22FB                 :array[0..3] of DWORD;            // 0x22F8
  PA_SC_ENHANCE                 :TPA_SC_ENHANCE;                  // 0x22FC
  REG_22FD_238F                 :array[0..146] of DWORD;          // 0x22FD
  SQ_THREAD_TRACE_CNTR          :TSQ_THREAD_TRACE_CNTR;           // 0x2390
  REG_2391_23BF                 :array[0..46] of DWORD;           // 0x2391
  SQ_BUF_RSRC_WORD0             :TSQ_BUF_RSRC_WORD0;              // 0x23C0
  SQ_BUF_RSRC_WORD1             :TSQ_BUF_RSRC_WORD1;              // 0x23C1
  SQ_BUF_RSRC_WORD2             :TSQ_BUF_RSRC_WORD2;              // 0x23C2
  SQ_BUF_RSRC_WORD3             :TSQ_BUF_RSRC_WORD3;              // 0x23C3
  SQ_IMG_RSRC_WORD0             :TSQ_IMG_RSRC_WORD0;              // 0x23C4
  SQ_IMG_RSRC_WORD1             :TSQ_IMG_RSRC_WORD1;              // 0x23C5
  SQ_IMG_RSRC_WORD2             :TSQ_IMG_RSRC_WORD2;              // 0x23C6
  SQ_IMG_RSRC_WORD3             :TSQ_IMG_RSRC_WORD3;              // 0x23C7
  SQ_IMG_RSRC_WORD4             :TSQ_IMG_RSRC_WORD4;              // 0x23C8
  SQ_IMG_RSRC_WORD5             :TSQ_IMG_RSRC_WORD5;              // 0x23C9
  SQ_IMG_RSRC_WORD6             :TSQ_IMG_RSRC_WORD6;              // 0x23CA
  SQ_IMG_RSRC_WORD7             :TSQ_IMG_RSRC_WORD7;              // 0x23CB
  SQ_IMG_SAMP_WORD0             :TSQ_IMG_SAMP_WORD0;              // 0x23CC
  SQ_IMG_SAMP_WORD1             :TSQ_IMG_SAMP_WORD1;              // 0x23CD
  SQ_IMG_SAMP_WORD2             :TSQ_IMG_SAMP_WORD2;              // 0x23CE
  SQ_IMG_SAMP_WORD3             :TSQ_IMG_SAMP_WORD3;              // 0x23CF
  REG_23D0_2413                 :array[0..67] of DWORD;           // 0x23D0
  SX_DEBUG_BUSY                 :TSX_DEBUG_BUSY;                  // 0x2414
  SX_DEBUG_BUSY_2               :TSX_DEBUG_BUSY_2;                // 0x2415
  SX_DEBUG_BUSY_3               :TSX_DEBUG_BUSY_3;                // 0x2416
  SX_DEBUG_BUSY_4               :TSX_DEBUG_BUSY_4;                // 0x2417
  SX_DEBUG_1                    :TSX_DEBUG_1;                     // 0x2418
  REG_2419_2439                 :array[0..32] of DWORD;           // 0x2419
  SPI_PS_MAX_WAVE_ID            :TSPI_PS_MAX_WAVE_ID;             // 0x243A
  SPI_START_PHASE               :TSPI_START_PHASE;                // 0x243B
  SPI_GFX_CNTL                  :TSPI_GFX_CNTL;                   // 0x243C
  REG_243D_243F                 :array[0..2] of DWORD;            // 0x243D
  SPI_CONFIG_CNTL               :TSPI_CONFIG_CNTL;                // 0x2440
  SPI_DEBUG_CNTL                :TSPI_DEBUG_CNTL;                 // 0x2441
  SPI_DEBUG_READ                :TSPI_DEBUG_READ;                 // 0x2442
  SPI_DSM_CNTL                  :TSPI_DSM_CNTL;                   // 0x2443
  SPI_EDC_CNT                   :TSPI_EDC_CNT;                    // 0x2444
  REG_2445_244E                 :array[0..9] of DWORD;            // 0x2445
  SPI_CONFIG_CNTL_1             :TSPI_CONFIG_CNTL_1;              // 0x244F
  SPI_DEBUG_BUSY                :TSPI_DEBUG_BUSY;                 // 0x2450
  SPI_CONFIG_CNTL_2             :TSPI_CONFIG_CNTL_2;              // 0x2451
  REG_2452_24A9                 :array[0..87] of DWORD;           // 0x2452
  SPI_WF_LIFETIME_CNTL          :TSPI_WF_LIFETIME_CNTL;           // 0x24AA
  SPI_WF_LIFETIME_LIMIT_0       :TSPI_WF_LIFETIME_LIMIT_0;        // 0x24AB
  SPI_WF_LIFETIME_LIMIT_1       :TSPI_WF_LIFETIME_LIMIT_1;        // 0x24AC
  SPI_WF_LIFETIME_LIMIT_2       :TSPI_WF_LIFETIME_LIMIT_2;        // 0x24AD
  SPI_WF_LIFETIME_LIMIT_3       :TSPI_WF_LIFETIME_LIMIT_3;        // 0x24AE
  SPI_WF_LIFETIME_LIMIT_4       :TSPI_WF_LIFETIME_LIMIT_4;        // 0x24AF
  SPI_WF_LIFETIME_LIMIT_5       :TSPI_WF_LIFETIME_LIMIT_5;        // 0x24B0
  SPI_WF_LIFETIME_LIMIT_6       :TSPI_WF_LIFETIME_LIMIT_6;        // 0x24B1
  SPI_WF_LIFETIME_LIMIT_7       :TSPI_WF_LIFETIME_LIMIT_7;        // 0x24B2
  SPI_WF_LIFETIME_LIMIT_8       :TSPI_WF_LIFETIME_LIMIT_8;        // 0x24B3
  SPI_WF_LIFETIME_LIMIT_9       :TSPI_WF_LIFETIME_LIMIT_9;        // 0x24B4
  SPI_WF_LIFETIME_STATUS_0      :TSPI_WF_LIFETIME_STATUS_0;       // 0x24B5
  SPI_WF_LIFETIME_STATUS_1      :TSPI_WF_LIFETIME_STATUS_1;       // 0x24B6
  SPI_WF_LIFETIME_STATUS_2      :TSPI_WF_LIFETIME_STATUS_2;       // 0x24B7
  SPI_WF_LIFETIME_STATUS_3      :TSPI_WF_LIFETIME_STATUS_3;       // 0x24B8
  SPI_WF_LIFETIME_STATUS_4      :TSPI_WF_LIFETIME_STATUS_4;       // 0x24B9
  SPI_WF_LIFETIME_STATUS_5      :TSPI_WF_LIFETIME_STATUS_5;       // 0x24BA
  SPI_WF_LIFETIME_STATUS_6      :TSPI_WF_LIFETIME_STATUS_6;       // 0x24BB
  SPI_WF_LIFETIME_STATUS_7      :TSPI_WF_LIFETIME_STATUS_7;       // 0x24BC
  SPI_WF_LIFETIME_STATUS_8      :TSPI_WF_LIFETIME_STATUS_8;       // 0x24BD
  SPI_WF_LIFETIME_STATUS_9      :TSPI_WF_LIFETIME_STATUS_9;       // 0x24BE
  SPI_WF_LIFETIME_STATUS_10     :TSPI_WF_LIFETIME_STATUS_10;      // 0x24BF
  SPI_WF_LIFETIME_STATUS_11     :TSPI_WF_LIFETIME_STATUS_11;      // 0x24C0
  SPI_WF_LIFETIME_STATUS_12     :TSPI_WF_LIFETIME_STATUS_12;      // 0x24C1
  SPI_WF_LIFETIME_STATUS_13     :TSPI_WF_LIFETIME_STATUS_13;      // 0x24C2
  SPI_WF_LIFETIME_STATUS_14     :TSPI_WF_LIFETIME_STATUS_14;      // 0x24C3
  SPI_WF_LIFETIME_STATUS_15     :TSPI_WF_LIFETIME_STATUS_15;      // 0x24C4
  SPI_WF_LIFETIME_STATUS_16     :TSPI_WF_LIFETIME_STATUS_16;      // 0x24C5
  SPI_WF_LIFETIME_STATUS_17     :TSPI_WF_LIFETIME_STATUS_17;      // 0x24C6
  SPI_WF_LIFETIME_STATUS_18     :TSPI_WF_LIFETIME_STATUS_18;      // 0x24C7
  SPI_WF_LIFETIME_STATUS_19     :TSPI_WF_LIFETIME_STATUS_19;      // 0x24C8
  SPI_WF_LIFETIME_STATUS_20     :TSPI_WF_LIFETIME_STATUS_20;      // 0x24C9
  SPI_WF_LIFETIME_DEBUG         :TSPI_WF_LIFETIME_DEBUG;          // 0x24CA
  REG_24CB_24D2                 :array[0..7] of DWORD;            // 0x24CB
  SPI_SLAVE_DEBUG_BUSY          :TSPI_SLAVE_DEBUG_BUSY;           // 0x24D3
  SPI_LB_CTR_CTRL               :TSPI_LB_CTR_CTRL;                // 0x24D4
  SPI_LB_CU_MASK                :TSPI_LB_CU_MASK;                 // 0x24D5
  SPI_LB_DATA_REG               :TSPI_LB_DATA_REG;                // 0x24D6
  SPI_PG_ENABLE_STATIC_CU_MASK  :TSPI_PG_ENABLE_STATIC_CU_MASK;   // 0x24D7
  SPI_GDS_CREDITS               :TSPI_GDS_CREDITS;                // 0x24D8
  SPI_SX_EXPORT_BUFFER_SIZES    :TSPI_SX_EXPORT_BUFFER_SIZES;     // 0x24D9
  SPI_SX_SCOREBOARD_BUFFER_SIZES:TSPI_SX_SCOREBOARD_BUFFER_SIZES; // 0x24DA
  SPI_CSQ_WF_ACTIVE_STATUS      :TSPI_CSQ_WF_ACTIVE_STATUS;       // 0x24DB
  SPI_CSQ_WF_ACTIVE_COUNT_0     :TSPI_CSQ_WF_ACTIVE_COUNT_0;      // 0x24DC
  SPI_CSQ_WF_ACTIVE_COUNT_1     :TSPI_CSQ_WF_ACTIVE_COUNT_1;      // 0x24DD
  SPI_CSQ_WF_ACTIVE_COUNT_2     :TSPI_CSQ_WF_ACTIVE_COUNT_2;      // 0x24DE
  SPI_CSQ_WF_ACTIVE_COUNT_3     :TSPI_CSQ_WF_ACTIVE_COUNT_3;      // 0x24DF
  SPI_CSQ_WF_ACTIVE_COUNT_4     :TSPI_CSQ_WF_ACTIVE_COUNT_4;      // 0x24E0
  SPI_CSQ_WF_ACTIVE_COUNT_5     :TSPI_CSQ_WF_ACTIVE_COUNT_5;      // 0x24E1
  SPI_CSQ_WF_ACTIVE_COUNT_6     :TSPI_CSQ_WF_ACTIVE_COUNT_6;      // 0x24E2
  SPI_CSQ_WF_ACTIVE_COUNT_7     :TSPI_CSQ_WF_ACTIVE_COUNT_7;      // 0x24E3
  REG_24E4_24EB                 :array[0..7] of DWORD;            // 0x24E4
  SPI_P0_TRAP_SCREEN_PSBA_LO    :TSPI_P0_TRAP_SCREEN_PSBA_LO;     // 0x24EC
  SPI_P0_TRAP_SCREEN_PSBA_HI    :TSPI_P0_TRAP_SCREEN_PSBA_HI;     // 0x24ED
  SPI_P0_TRAP_SCREEN_PSMA_LO    :TSPI_P0_TRAP_SCREEN_PSMA_LO;     // 0x24EE
  SPI_P0_TRAP_SCREEN_PSMA_HI    :TSPI_P0_TRAP_SCREEN_PSMA_HI;     // 0x24EF
  SPI_P0_TRAP_SCREEN_GPR_MIN    :TSPI_P0_TRAP_SCREEN_GPR_MIN;     // 0x24F0
  SPI_P1_TRAP_SCREEN_PSBA_LO    :TSPI_P1_TRAP_SCREEN_PSBA_LO;     // 0x24F1
  SPI_P1_TRAP_SCREEN_PSBA_HI    :TSPI_P1_TRAP_SCREEN_PSBA_HI;     // 0x24F2
  SPI_P1_TRAP_SCREEN_PSMA_LO    :TSPI_P1_TRAP_SCREEN_PSMA_LO;     // 0x24F3
  SPI_P1_TRAP_SCREEN_PSMA_HI    :TSPI_P1_TRAP_SCREEN_PSMA_HI;     // 0x24F4
  SPI_P1_TRAP_SCREEN_GPR_MIN    :TSPI_P1_TRAP_SCREEN_GPR_MIN;     // 0x24F5
  REG_24F6_2524                 :array[0..46] of DWORD;           // 0x24F6
  TD_CNTL                       :TTD_CNTL;                        // 0x2525
  TD_STATUS                     :TTD_STATUS;                      // 0x2526
  REG_2527                      :DWORD;                           // 0x2527
  TD_DEBUG_INDEX                :TTD_DEBUG_INDEX;                 // 0x2528
  TD_DEBUG_DATA                 :TTD_DEBUG_DATA;                  // 0x2529
  REG_252A_252E                 :array[0..4] of DWORD;            // 0x252A
  TD_DSM_CNTL                   :TTD_DSM_CNTL;                    // 0x252F
  REG_2530_2532                 :array[0..2] of DWORD;            // 0x2530
  TD_SCRATCH                    :TTD_SCRATCH;                     // 0x2533
  REG_2534_2540                 :array[0..12] of DWORD;           // 0x2534
  TA_CNTL                       :TTA_CNTL;                        // 0x2541
  TA_CNTL_AUX                   :TTA_CNTL_AUX;                    // 0x2542
  TA_RESERVED_010C              :TTA_RESERVED_010C;               // 0x2543
  REG_2544_2547                 :array[0..3] of DWORD;            // 0x2544
  TA_STATUS                     :TTA_STATUS;                      // 0x2548
  REG_2549_254B                 :array[0..2] of DWORD;            // 0x2549
  TA_DEBUG_INDEX                :TTA_DEBUG_INDEX;                 // 0x254C
  TA_DEBUG_DATA                 :TTA_DEBUG_DATA;                  // 0x254D
  REG_254E_2563                 :array[0..21] of DWORD;           // 0x254E
  TA_SCRATCH                    :TTA_SCRATCH;                     // 0x2564
  REG_2565_25BF                 :array[0..90] of DWORD;           // 0x2565
  GDS_CONFIG                    :TGDS_CONFIG;                     // 0x25C0
  GDS_CNTL_STATUS               :TGDS_CNTL_STATUS;                // 0x25C1
  GDS_ENHANCE2                  :TGDS_ENHANCE2;                   // 0x25C2
  GDS_PROTECTION_FAULT          :TGDS_PROTECTION_FAULT;           // 0x25C3
  GDS_VM_PROTECTION_FAULT       :TGDS_VM_PROTECTION_FAULT;        // 0x25C4
  GDS_EDC_CNT                   :TGDS_EDC_CNT;                    // 0x25C5
  GDS_EDC_GRBM_CNT              :TGDS_EDC_GRBM_CNT;               // 0x25C6
  GDS_EDC_OA_DED                :TGDS_EDC_OA_DED;                 // 0x25C7
  GDS_DEBUG_CNTL                :TGDS_DEBUG_CNTL;                 // 0x25C8
  GDS_DEBUG_DATA                :TGDS_DEBUG_DATA;                 // 0x25C9
  GDS_DSM_CNTL                  :TGDS_DSM_CNTL;                   // 0x25CA
  REG_25CB_260B                 :array[0..64] of DWORD;           // 0x25CB
  DB_DEBUG                      :TDB_DEBUG;                       // 0x260C
  DB_DEBUG2                     :TDB_DEBUG2;                      // 0x260D
  DB_DEBUG3                     :TDB_DEBUG3;                      // 0x260E
  DB_DEBUG4                     :TDB_DEBUG4;                      // 0x260F
  REG_2610_2613                 :array[0..3] of DWORD;            // 0x2610
  DB_CREDIT_LIMIT               :TDB_CREDIT_LIMIT;                // 0x2614
  DB_WATERMARKS                 :TDB_WATERMARKS;                  // 0x2615
  DB_SUBTILE_CONTROL            :TDB_SUBTILE_CONTROL;             // 0x2616
  DB_FREE_CACHELINES            :TDB_FREE_CACHELINES;             // 0x2617
  DB_FIFO_DEPTH1                :TDB_FIFO_DEPTH1;                 // 0x2618
  DB_FIFO_DEPTH2                :TDB_FIFO_DEPTH2;                 // 0x2619
  REG_261A                      :DWORD;                           // 0x261A
  DB_RING_CONTROL               :TDB_RING_CONTROL;                // 0x261B
  REG_261C_261F                 :array[0..3] of DWORD;            // 0x261C
  DB_READ_DEBUG_0               :TDB_READ_DEBUG_0;                // 0x2620
  DB_READ_DEBUG_1               :TDB_READ_DEBUG_1;                // 0x2621
  DB_READ_DEBUG_2               :TDB_READ_DEBUG_2;                // 0x2622
  DB_READ_DEBUG_3               :TDB_READ_DEBUG_3;                // 0x2623
  DB_READ_DEBUG_4               :TDB_READ_DEBUG_4;                // 0x2624
  DB_READ_DEBUG_5               :TDB_READ_DEBUG_5;                // 0x2625
  DB_READ_DEBUG_6               :TDB_READ_DEBUG_6;                // 0x2626
  DB_READ_DEBUG_7               :TDB_READ_DEBUG_7;                // 0x2627
  DB_READ_DEBUG_8               :TDB_READ_DEBUG_8;                // 0x2628
  DB_READ_DEBUG_9               :TDB_READ_DEBUG_9;                // 0x2629
  DB_READ_DEBUG_A               :TDB_READ_DEBUG_A;                // 0x262A
  DB_READ_DEBUG_B               :TDB_READ_DEBUG_B;                // 0x262B
  DB_READ_DEBUG_C               :TDB_READ_DEBUG_C;                // 0x262C
  DB_READ_DEBUG_D               :TDB_READ_DEBUG_D;                // 0x262D
  DB_READ_DEBUG_E               :TDB_READ_DEBUG_E;                // 0x262E
  DB_READ_DEBUG_F               :TDB_READ_DEBUG_F;                // 0x262F
  REG_2630_2682                 :array[0..82] of DWORD;           // 0x2630
  CB_HW_CONTROL_3               :TCB_HW_CONTROL_3;                // 0x2683
  CB_HW_CONTROL                 :TCB_HW_CONTROL;                  // 0x2684
  CB_HW_CONTROL_1               :TCB_HW_CONTROL_1;                // 0x2685
  CB_HW_CONTROL_2               :TCB_HW_CONTROL_2;                // 0x2686
  CB_DCC_CONFIG                 :TCB_DCC_CONFIG;                  // 0x2687
  REG_2688_2698                 :array[0..16] of DWORD;           // 0x2688
  CB_DEBUG_BUS_1                :TCB_DEBUG_BUS_1;                 // 0x2699
  CB_DEBUG_BUS_2                :TCB_DEBUG_BUS_2;                 // 0x269A
  REG_269B_26A4                 :array[0..9] of DWORD;            // 0x269B
  CB_DEBUG_BUS_13               :TCB_DEBUG_BUS_13;                // 0x26A5
  CB_DEBUG_BUS_14               :TCB_DEBUG_BUS_14;                // 0x26A6
  CB_DEBUG_BUS_15               :TCB_DEBUG_BUS_15;                // 0x26A7
  CB_DEBUG_BUS_16               :TCB_DEBUG_BUS_16;                // 0x26A8
  CB_DEBUG_BUS_17               :TCB_DEBUG_BUS_17;                // 0x26A9
  CB_DEBUG_BUS_18               :TCB_DEBUG_BUS_18;                // 0x26AA
  CB_DEBUG_BUS_19               :TCB_DEBUG_BUS_19;                // 0x26AB
  CB_DEBUG_BUS_20               :TCB_DEBUG_BUS_20;                // 0x26AC
  CB_DEBUG_BUS_21               :TCB_DEBUG_BUS_21;                // 0x26AD
  CB_DEBUG_BUS_22               :TCB_DEBUG_BUS_22;                // 0x26AE
  REG_26AF_2AFF                 :array[0..1104] of DWORD;         // 0x26AF
  TCP_INVALIDATE                :TTCP_INVALIDATE;                 // 0x2B00
  TCP_STATUS                    :TTCP_STATUS;                     // 0x2B01
  TCP_CNTL                      :TTCP_CNTL;                       // 0x2B02
  TCP_CHAN_STEER_LO             :TTCP_CHAN_STEER_LO;              // 0x2B03
  TCP_CHAN_STEER_HI             :TTCP_CHAN_STEER_HI;              // 0x2B04
  TCP_ADDR_CONFIG               :TTCP_ADDR_CONFIG;                // 0x2B05
  TCP_CREDIT                    :TTCP_CREDIT;                     // 0x2B06
  REG_2B07_2B15                 :array[0..14] of DWORD;           // 0x2B07
  TCP_BUFFER_ADDR_HASH_CNTL     :TTCP_BUFFER_ADDR_HASH_CNTL;      // 0x2B16
  TCP_EDC_CNT                   :TTCP_EDC_CNT;                    // 0x2B17
  REG_2B18_2B7F                 :array[0..103] of DWORD;          // 0x2B18
  TCC_CTRL                      :TTCC_CTRL;                       // 0x2B80
  REG_2B81                      :DWORD;                           // 0x2B81
  TCC_EDC_CNT                   :TTCC_EDC_CNT;                    // 0x2B82
  TCC_REDUNDANCY                :TTCC_REDUNDANCY;                 // 0x2B83
  TCC_EXE_DISABLE               :TTCC_EXE_DISABLE;                // 0x2B84
  TCC_DSM_CNTL                  :TTCC_DSM_CNTL;                   // 0x2B85
  REG_2B86_2BBF                 :array[0..57] of DWORD;           // 0x2B86
  TCA_CTRL                      :TTCA_CTRL;                       // 0x2BC0
  REG_2BC1_301F                 :array[0..1118] of DWORD;         // 0x2BC1
  CP_DFY_CNTL                   :TCP_DFY_CNTL;                    // 0x3020
  CP_DFY_STAT                   :TCP_DFY_STAT;                    // 0x3021
  CP_DFY_ADDR_HI                :TCP_DFY_ADDR_HI;                 // 0x3022
  CP_DFY_ADDR_LO                :TCP_DFY_ADDR_LO;                 // 0x3023
  CP_DFY_DATA_0                 :TCP_DFY_DATA_0;                  // 0x3024
  CP_DFY_DATA_1                 :TCP_DFY_DATA_1;                  // 0x3025
  CP_DFY_DATA_2                 :TCP_DFY_DATA_2;                  // 0x3026
  CP_DFY_DATA_3                 :TCP_DFY_DATA_3;                  // 0x3027
  CP_DFY_DATA_4                 :TCP_DFY_DATA_4;                  // 0x3028
  CP_DFY_DATA_5                 :TCP_DFY_DATA_5;                  // 0x3029
  CP_DFY_DATA_6                 :TCP_DFY_DATA_6;                  // 0x302A
  CP_DFY_DATA_7                 :TCP_DFY_DATA_7;                  // 0x302B
  CP_DFY_DATA_8                 :TCP_DFY_DATA_8;                  // 0x302C
  CP_DFY_DATA_9                 :TCP_DFY_DATA_9;                  // 0x302D
  CP_DFY_DATA_10                :TCP_DFY_DATA_10;                 // 0x302E
  CP_DFY_DATA_11                :TCP_DFY_DATA_11;                 // 0x302F
  CP_DFY_DATA_12                :TCP_DFY_DATA_12;                 // 0x3030
  CP_DFY_DATA_13                :TCP_DFY_DATA_13;                 // 0x3031
  CP_DFY_DATA_14                :TCP_DFY_DATA_14;                 // 0x3032
  CP_DFY_DATA_15                :TCP_DFY_DATA_15;                 // 0x3033
  CP_DFY_CMD                    :TCP_DFY_CMD;                     // 0x3034
  REG_3035                      :DWORD;                           // 0x3035
  CP_CPC_MGCG_SYNC_CNTL         :TCP_CPC_MGCG_SYNC_CNTL;          // 0x3036
  REG_3037                      :DWORD;                           // 0x3037
  CP_VIRT_STATUS                :TCP_VIRT_STATUS;                 // 0x3038
  REG_3039_303F                 :array[0..6] of DWORD;            // 0x3039
  CP_RB0_BASE                   :TCP_RB0_BASE;                    // 0x3040
  CP_RB0_CNTL                   :TCP_RB0_CNTL;                    // 0x3041
  REG_3042                      :DWORD;                           // 0x3042
  CP_RB0_RPTR_ADDR              :TCP_RB0_RPTR_ADDR;               // 0x3043
  CP_RB0_RPTR_ADDR_HI           :TCP_RB0_RPTR_ADDR_HI;            // 0x3044
  CP_RB0_WPTR                   :TCP_RB0_WPTR;                    // 0x3045
  REG_3046_3048                 :array[0..2] of DWORD;            // 0x3046
  CP_INT_CNTL                   :TCP_INT_CNTL;                    // 0x3049
  CP_INT_STATUS                 :TCP_INT_STATUS;                  // 0x304A
  CP_DEVICE_ID                  :TCP_DEVICE_ID;                   // 0x304B
  CP_ME0_PIPE_PRIORITY_CNTS     :TCP_ME0_PIPE_PRIORITY_CNTS;      // 0x304C
  CP_ME0_PIPE0_PRIORITY         :TCP_ME0_PIPE0_PRIORITY;          // 0x304D
  CP_ME0_PIPE1_PRIORITY         :TCP_ME0_PIPE1_PRIORITY;          // 0x304E
  CP_ME0_PIPE2_PRIORITY         :TCP_ME0_PIPE2_PRIORITY;          // 0x304F
  CP_ENDIAN_SWAP                :TCP_ENDIAN_SWAP;                 // 0x3050
  REG_3051                      :DWORD;                           // 0x3051
  CP_ME0_PIPE0_VMID             :TCP_ME0_PIPE0_VMID;              // 0x3052
  CP_ME0_PIPE1_VMID             :TCP_ME0_PIPE1_VMID;              // 0x3053
  REG_3054_305B                 :array[0..7] of DWORD;            // 0x3054
  CP_MEC_DOORBELL_RANGE_LOWER   :TCP_MEC_DOORBELL_RANGE_LOWER;    // 0x305C
  CP_MEC_DOORBELL_RANGE_UPPER   :TCP_MEC_DOORBELL_RANGE_UPPER;    // 0x305D
  REG_305E_305F                 :array[0..1] of DWORD;            // 0x305E
  CP_RB1_BASE                   :TCP_RB1_BASE;                    // 0x3060
  CP_RB1_CNTL                   :TCP_RB1_CNTL;                    // 0x3061
  CP_RB1_RPTR_ADDR              :TCP_RB1_RPTR_ADDR;               // 0x3062
  CP_RB1_RPTR_ADDR_HI           :TCP_RB1_RPTR_ADDR_HI;            // 0x3063
  CP_RB1_WPTR                   :TCP_RB1_WPTR;                    // 0x3064
  CP_RB2_BASE                   :TCP_RB2_BASE;                    // 0x3065
  CP_RB2_CNTL                   :TCP_RB2_CNTL;                    // 0x3066
  CP_RB2_RPTR_ADDR              :TCP_RB2_RPTR_ADDR;               // 0x3067
  CP_RB2_RPTR_ADDR_HI           :TCP_RB2_RPTR_ADDR_HI;            // 0x3068
  CP_RB2_WPTR                   :TCP_RB2_WPTR;                    // 0x3069
  CP_INT_CNTL_RING0             :TCP_INT_CNTL_RING0;              // 0x306A
  CP_INT_CNTL_RING1             :TCP_INT_CNTL_RING1;              // 0x306B
  CP_INT_CNTL_RING2             :TCP_INT_CNTL_RING2;              // 0x306C
  CP_INT_STATUS_RING0           :TCP_INT_STATUS_RING0;            // 0x306D
  CP_INT_STATUS_RING1           :TCP_INT_STATUS_RING1;            // 0x306E
  CP_INT_STATUS_RING2           :TCP_INT_STATUS_RING2;            // 0x306F
  REG_3070_3077                 :array[0..7] of DWORD;            // 0x3070
  CP_PWR_CNTL                   :TCP_PWR_CNTL;                    // 0x3078
  CP_MEM_SLP_CNTL               :TCP_MEM_SLP_CNTL;                // 0x3079
  CP_ECC_FIRSTOCCURRENCE        :TCP_ECC_FIRSTOCCURRENCE;         // 0x307A
  CP_ECC_FIRSTOCCURRENCE_RING0  :TCP_ECC_FIRSTOCCURRENCE_RING0;   // 0x307B
  CP_ECC_FIRSTOCCURRENCE_RING1  :TCP_ECC_FIRSTOCCURRENCE_RING1;   // 0x307C
  CP_ECC_FIRSTOCCURRENCE_RING2  :TCP_ECC_FIRSTOCCURRENCE_RING2;   // 0x307D
  REG_307E                      :DWORD;                           // 0x307E
  CP_DEBUG                      :TCP_DEBUG;                       // 0x307F
  REG_3080_3082                 :array[0..2] of DWORD;            // 0x3080
  CP_PQ_WPTR_POLL_CNTL          :TCP_PQ_WPTR_POLL_CNTL;           // 0x3083
  CP_PQ_WPTR_POLL_CNTL1         :TCP_PQ_WPTR_POLL_CNTL1;          // 0x3084
  CP_ME1_PIPE0_INT_CNTL         :TCP_ME1_PIPE0_INT_CNTL;          // 0x3085
  CP_ME1_PIPE1_INT_CNTL         :TCP_ME1_PIPE1_INT_CNTL;          // 0x3086
  CP_ME1_PIPE2_INT_CNTL         :TCP_ME1_PIPE2_INT_CNTL;          // 0x3087
  CP_ME1_PIPE3_INT_CNTL         :TCP_ME1_PIPE3_INT_CNTL;          // 0x3088
  CP_ME2_PIPE0_INT_CNTL         :TCP_ME2_PIPE0_INT_CNTL;          // 0x3089
  CP_ME2_PIPE1_INT_CNTL         :TCP_ME2_PIPE1_INT_CNTL;          // 0x308A
  CP_ME2_PIPE2_INT_CNTL         :TCP_ME2_PIPE2_INT_CNTL;          // 0x308B
  CP_ME2_PIPE3_INT_CNTL         :TCP_ME2_PIPE3_INT_CNTL;          // 0x308C
  CP_ME1_PIPE0_INT_STATUS       :TCP_ME1_PIPE0_INT_STATUS;        // 0x308D
  CP_ME1_PIPE1_INT_STATUS       :TCP_ME1_PIPE1_INT_STATUS;        // 0x308E
  CP_ME1_PIPE2_INT_STATUS       :TCP_ME1_PIPE2_INT_STATUS;        // 0x308F
  CP_ME1_PIPE3_INT_STATUS       :TCP_ME1_PIPE3_INT_STATUS;        // 0x3090
  CP_ME2_PIPE0_INT_STATUS       :TCP_ME2_PIPE0_INT_STATUS;        // 0x3091
  CP_ME2_PIPE1_INT_STATUS       :TCP_ME2_PIPE1_INT_STATUS;        // 0x3092
  CP_ME2_PIPE2_INT_STATUS       :TCP_ME2_PIPE2_INT_STATUS;        // 0x3093
  CP_ME2_PIPE3_INT_STATUS       :TCP_ME2_PIPE3_INT_STATUS;        // 0x3094
  CP_ME1_INT_STAT_DEBUG         :TCP_ME1_INT_STAT_DEBUG;          // 0x3095
  CP_ME2_INT_STAT_DEBUG         :TCP_ME2_INT_STAT_DEBUG;          // 0x3096
  REG_3097_3098                 :array[0..1] of DWORD;            // 0x3097
  CP_ME1_PIPE_PRIORITY_CNTS     :TCP_ME1_PIPE_PRIORITY_CNTS;      // 0x3099
  CP_ME1_PIPE0_PRIORITY         :TCP_ME1_PIPE0_PRIORITY;          // 0x309A
  CP_ME1_PIPE1_PRIORITY         :TCP_ME1_PIPE1_PRIORITY;          // 0x309B
  CP_ME1_PIPE2_PRIORITY         :TCP_ME1_PIPE2_PRIORITY;          // 0x309C
  CP_ME1_PIPE3_PRIORITY         :TCP_ME1_PIPE3_PRIORITY;          // 0x309D
  CP_ME2_PIPE_PRIORITY_CNTS     :TCP_ME2_PIPE_PRIORITY_CNTS;      // 0x309E
  CP_ME2_PIPE0_PRIORITY         :TCP_ME2_PIPE0_PRIORITY;          // 0x309F
  CP_ME2_PIPE1_PRIORITY         :TCP_ME2_PIPE1_PRIORITY;          // 0x30A0
  CP_ME2_PIPE2_PRIORITY         :TCP_ME2_PIPE2_PRIORITY;          // 0x30A1
  CP_ME2_PIPE3_PRIORITY         :TCP_ME2_PIPE3_PRIORITY;          // 0x30A2
  CP_CE_PRGRM_CNTR_START        :TCP_CE_PRGRM_CNTR_START;         // 0x30A3
  CP_PFP_PRGRM_CNTR_START       :TCP_PFP_PRGRM_CNTR_START;        // 0x30A4
  REG_30A5                      :DWORD;                           // 0x30A5
  CP_MEC1_PRGRM_CNTR_START      :TCP_MEC1_PRGRM_CNTR_START;       // 0x30A6
  CP_MEC2_PRGRM_CNTR_START      :TCP_MEC2_PRGRM_CNTR_START;       // 0x30A7
  CP_CE_INTR_ROUTINE_START      :TCP_CE_INTR_ROUTINE_START;       // 0x30A8
  CP_PFP_INTR_ROUTINE_START     :TCP_PFP_INTR_ROUTINE_START;      // 0x30A9
  REG_30AA                      :DWORD;                           // 0x30AA
  CP_MEC1_INTR_ROUTINE_START    :TCP_MEC1_INTR_ROUTINE_START;     // 0x30AB
  CP_MEC2_INTR_ROUTINE_START    :TCP_MEC2_INTR_ROUTINE_START;     // 0x30AC
  CP_CONTEXT_CNTL               :TCP_CONTEXT_CNTL;                // 0x30AD
  CP_MAX_CONTEXT                :TCP_MAX_CONTEXT;                 // 0x30AE
  CP_IQ_WAIT_TIME1              :TCP_IQ_WAIT_TIME1;               // 0x30AF
  CP_IQ_WAIT_TIME2              :TCP_IQ_WAIT_TIME2;               // 0x30B0
  CP_RB0_BASE_HI                :TCP_RB0_BASE_HI;                 // 0x30B1
  CP_RB1_BASE_HI                :TCP_RB1_BASE_HI;                 // 0x30B2
  CP_VMID_RESET                 :TCP_VMID_RESET;                  // 0x30B3
  CPC_INT_CNTL                  :TCPC_INT_CNTL;                   // 0x30B4
  CPC_INT_STATUS                :TCPC_INT_STATUS;                 // 0x30B5
  CP_VMID_PREEMPT               :TCP_VMID_PREEMPT;                // 0x30B6
  CPC_INT_CNTX_ID               :TCPC_INT_CNTX_ID;                // 0x30B7
  CP_PQ_STATUS                  :TCP_PQ_STATUS;                   // 0x30B8
  CP_CPC_IC_BASE_LO             :TCP_CPC_IC_BASE_LO;              // 0x30B9
  CP_CPC_IC_BASE_HI             :TCP_CPC_IC_BASE_HI;              // 0x30BA
  CP_CPC_IC_BASE_CNTL           :TCP_CPC_IC_BASE_CNTL;            // 0x30BB
  CP_CPC_IC_OP_CNTL             :TCP_CPC_IC_OP_CNTL;              // 0x30BC
  CP_MEC1_F32_INT_DIS           :TCP_MEC1_F32_INT_DIS;            // 0x30BD
  CP_MEC2_F32_INT_DIS           :TCP_MEC2_F32_INT_DIS;            // 0x30BE
  CP_VMID_STATUS                :TCP_VMID_STATUS;                 // 0x30BF
  REG_30C0_31BF                 :array[0..255] of DWORD;          // 0x30C0
  SPI_ARB_PRIORITY              :TSPI_ARB_PRIORITY;               // 0x31C0
  SPI_ARB_CYCLES_0              :TSPI_ARB_CYCLES_0;               // 0x31C1
  SPI_ARB_CYCLES_1              :TSPI_ARB_CYCLES_1;               // 0x31C2
  SPI_CDBG_SYS_GFX              :TSPI_CDBG_SYS_GFX;               // 0x31C3
  SPI_CDBG_SYS_HP3D             :TSPI_CDBG_SYS_HP3D;              // 0x31C4
  SPI_CDBG_SYS_CS0              :TSPI_CDBG_SYS_CS0;               // 0x31C5
  SPI_CDBG_SYS_CS1              :TSPI_CDBG_SYS_CS1;               // 0x31C6
  SPI_WCL_PIPE_PERCENT_GFX      :TSPI_WCL_PIPE_PERCENT_GFX;       // 0x31C7
  SPI_WCL_PIPE_PERCENT_HP3D     :TSPI_WCL_PIPE_PERCENT_HP3D;      // 0x31C8
  SPI_WCL_PIPE_PERCENT_CS0      :TSPI_WCL_PIPE_PERCENT_CS0;       // 0x31C9
  SPI_WCL_PIPE_PERCENT_CS1      :TSPI_WCL_PIPE_PERCENT_CS1;       // 0x31CA
  SPI_WCL_PIPE_PERCENT_CS2      :TSPI_WCL_PIPE_PERCENT_CS2;       // 0x31CB
  SPI_WCL_PIPE_PERCENT_CS3      :TSPI_WCL_PIPE_PERCENT_CS3;       // 0x31CC
  SPI_WCL_PIPE_PERCENT_CS4      :TSPI_WCL_PIPE_PERCENT_CS4;       // 0x31CD
  SPI_WCL_PIPE_PERCENT_CS5      :TSPI_WCL_PIPE_PERCENT_CS5;       // 0x31CE
  SPI_WCL_PIPE_PERCENT_CS6      :TSPI_WCL_PIPE_PERCENT_CS6;       // 0x31CF
  SPI_WCL_PIPE_PERCENT_CS7      :TSPI_WCL_PIPE_PERCENT_CS7;       // 0x31D0
  SPI_GDBG_WAVE_CNTL            :TSPI_GDBG_WAVE_CNTL;             // 0x31D1
  SPI_GDBG_TRAP_CONFIG          :TSPI_GDBG_TRAP_CONFIG;           // 0x31D2
  SPI_GDBG_TRAP_MASK            :TSPI_GDBG_TRAP_MASK;             // 0x31D3
  SPI_GDBG_TBA_LO               :TSPI_GDBG_TBA_LO;                // 0x31D4
  SPI_GDBG_TBA_HI               :TSPI_GDBG_TBA_HI;                // 0x31D5
  SPI_GDBG_TMA_LO               :TSPI_GDBG_TMA_LO;                // 0x31D6
  SPI_GDBG_TMA_HI               :TSPI_GDBG_TMA_HI;                // 0x31D7
  SPI_GDBG_TRAP_DATA0           :TSPI_GDBG_TRAP_DATA0;            // 0x31D8
  SPI_GDBG_TRAP_DATA1           :TSPI_GDBG_TRAP_DATA1;            // 0x31D9
  SPI_RESET_DEBUG               :TSPI_RESET_DEBUG;                // 0x31DA
  SPI_COMPUTE_QUEUE_RESET       :TSPI_COMPUTE_QUEUE_RESET;        // 0x31DB
  SPI_RESOURCE_RESERVE_CU_0     :TSPI_RESOURCE_RESERVE_CU_0;      // 0x31DC
  SPI_RESOURCE_RESERVE_CU_1     :TSPI_RESOURCE_RESERVE_CU_1;      // 0x31DD
  SPI_RESOURCE_RESERVE_CU_2     :TSPI_RESOURCE_RESERVE_CU_2;      // 0x31DE
  SPI_RESOURCE_RESERVE_CU_3     :TSPI_RESOURCE_RESERVE_CU_3;      // 0x31DF
  SPI_RESOURCE_RESERVE_CU_4     :TSPI_RESOURCE_RESERVE_CU_4;      // 0x31E0
  SPI_RESOURCE_RESERVE_CU_5     :TSPI_RESOURCE_RESERVE_CU_5;      // 0x31E1
  SPI_RESOURCE_RESERVE_CU_6     :TSPI_RESOURCE_RESERVE_CU_6;      // 0x31E2
  SPI_RESOURCE_RESERVE_CU_7     :TSPI_RESOURCE_RESERVE_CU_7;      // 0x31E3
  SPI_RESOURCE_RESERVE_CU_8     :TSPI_RESOURCE_RESERVE_CU_8;      // 0x31E4
  SPI_RESOURCE_RESERVE_CU_9     :TSPI_RESOURCE_RESERVE_CU_9;      // 0x31E5
  SPI_RESOURCE_RESERVE_EN_CU_0  :TSPI_RESOURCE_RESERVE_EN_CU_0;   // 0x31E6
  SPI_RESOURCE_RESERVE_EN_CU_1  :TSPI_RESOURCE_RESERVE_EN_CU_1;   // 0x31E7
  SPI_RESOURCE_RESERVE_EN_CU_2  :TSPI_RESOURCE_RESERVE_EN_CU_2;   // 0x31E8
  SPI_RESOURCE_RESERVE_EN_CU_3  :TSPI_RESOURCE_RESERVE_EN_CU_3;   // 0x31E9
  SPI_RESOURCE_RESERVE_EN_CU_4  :TSPI_RESOURCE_RESERVE_EN_CU_4;   // 0x31EA
  SPI_RESOURCE_RESERVE_EN_CU_5  :TSPI_RESOURCE_RESERVE_EN_CU_5;   // 0x31EB
  SPI_RESOURCE_RESERVE_EN_CU_6  :TSPI_RESOURCE_RESERVE_EN_CU_6;   // 0x31EC
  SPI_RESOURCE_RESERVE_EN_CU_7  :TSPI_RESOURCE_RESERVE_EN_CU_7;   // 0x31ED
  SPI_RESOURCE_RESERVE_EN_CU_8  :TSPI_RESOURCE_RESERVE_EN_CU_8;   // 0x31EE
  SPI_RESOURCE_RESERVE_EN_CU_9  :TSPI_RESOURCE_RESERVE_EN_CU_9;   // 0x31EF
  SPI_RESOURCE_RESERVE_CU_10    :TSPI_RESOURCE_RESERVE_CU_10;     // 0x31F0
  SPI_RESOURCE_RESERVE_CU_11    :TSPI_RESOURCE_RESERVE_CU_11;     // 0x31F1
  SPI_RESOURCE_RESERVE_EN_CU_10 :TSPI_RESOURCE_RESERVE_EN_CU_10;  // 0x31F2
  SPI_RESOURCE_RESERVE_EN_CU_11 :TSPI_RESOURCE_RESERVE_EN_CU_11;  // 0x31F3
  SPI_RESOURCE_RESERVE_CU_12    :TSPI_RESOURCE_RESERVE_CU_12;     // 0x31F4
  SPI_RESOURCE_RESERVE_CU_13    :TSPI_RESOURCE_RESERVE_CU_13;     // 0x31F5
  SPI_RESOURCE_RESERVE_CU_14    :TSPI_RESOURCE_RESERVE_CU_14;     // 0x31F6
  SPI_RESOURCE_RESERVE_CU_15    :TSPI_RESOURCE_RESERVE_CU_15;     // 0x31F7
  SPI_RESOURCE_RESERVE_EN_CU_12 :TSPI_RESOURCE_RESERVE_EN_CU_12;  // 0x31F8
  SPI_RESOURCE_RESERVE_EN_CU_13 :TSPI_RESOURCE_RESERVE_EN_CU_13;  // 0x31F9
  SPI_RESOURCE_RESERVE_EN_CU_14 :TSPI_RESOURCE_RESERVE_EN_CU_14;  // 0x31FA
  SPI_RESOURCE_RESERVE_EN_CU_15 :TSPI_RESOURCE_RESERVE_EN_CU_15;  // 0x31FB
  SPI_COMPUTE_WF_CTX_SAVE       :TSPI_COMPUTE_WF_CTX_SAVE;        // 0x31FC
  REG_31FD_323F                 :array[0..66] of DWORD;           // 0x31FD
  CP_HPD_ROQ_OFFSETS            :TCP_HPD_ROQ_OFFSETS;             // 0x3240
  CP_HPD_STATUS0                :TCP_HPD_STATUS0;                 // 0x3241
  REG_3242_3244                 :array[0..2] of DWORD;            // 0x3242
  CP_MQD_BASE_ADDR              :TCP_MQD_BASE_ADDR;               // 0x3245
  CP_MQD_BASE_ADDR_HI           :TCP_MQD_BASE_ADDR_HI;            // 0x3246
  CP_HQD_ACTIVE                 :TCP_HQD_ACTIVE;                  // 0x3247
  CP_HQD_VMID                   :TCP_HQD_VMID;                    // 0x3248
  CP_HQD_PERSISTENT_STATE       :TCP_HQD_PERSISTENT_STATE;        // 0x3249
  CP_HQD_PIPE_PRIORITY          :TCP_HQD_PIPE_PRIORITY;           // 0x324A
  CP_HQD_QUEUE_PRIORITY         :TCP_HQD_QUEUE_PRIORITY;          // 0x324B
  CP_HQD_QUANTUM                :TCP_HQD_QUANTUM;                 // 0x324C
  CP_HQD_PQ_BASE                :TCP_HQD_PQ_BASE;                 // 0x324D
  CP_HQD_PQ_BASE_HI             :TCP_HQD_PQ_BASE_HI;              // 0x324E
  CP_HQD_PQ_RPTR                :TCP_HQD_PQ_RPTR;                 // 0x324F
  CP_HQD_PQ_RPTR_REPORT_ADDR    :TCP_HQD_PQ_RPTR_REPORT_ADDR;     // 0x3250
  CP_HQD_PQ_RPTR_REPORT_ADDR_HI :TCP_HQD_PQ_RPTR_REPORT_ADDR_HI;  // 0x3251
  CP_HQD_PQ_WPTR_POLL_ADDR      :TCP_HQD_PQ_WPTR_POLL_ADDR;       // 0x3252
  CP_HQD_PQ_WPTR_POLL_ADDR_HI   :TCP_HQD_PQ_WPTR_POLL_ADDR_HI;    // 0x3253
  CP_HQD_PQ_DOORBELL_CONTROL    :TCP_HQD_PQ_DOORBELL_CONTROL;     // 0x3254
  CP_HQD_PQ_WPTR                :TCP_HQD_PQ_WPTR;                 // 0x3255
  CP_HQD_PQ_CONTROL             :TCP_HQD_PQ_CONTROL;              // 0x3256
  CP_HQD_IB_BASE_ADDR           :TCP_HQD_IB_BASE_ADDR;            // 0x3257
  CP_HQD_IB_BASE_ADDR_HI        :TCP_HQD_IB_BASE_ADDR_HI;         // 0x3258
  CP_HQD_IB_RPTR                :TCP_HQD_IB_RPTR;                 // 0x3259
  CP_HQD_IB_CONTROL             :TCP_HQD_IB_CONTROL;              // 0x325A
  CP_HQD_IQ_TIMER               :TCP_HQD_IQ_TIMER;                // 0x325B
  CP_HQD_IQ_RPTR                :TCP_HQD_IQ_RPTR;                 // 0x325C
  CP_HQD_DEQUEUE_REQUEST        :TCP_HQD_DEQUEUE_REQUEST;         // 0x325D
  CP_HQD_OFFLOAD                :TCP_HQD_OFFLOAD;                 // 0x325E
  CP_HQD_SEMA_CMD               :TCP_HQD_SEMA_CMD;                // 0x325F
  CP_HQD_MSG_TYPE               :TCP_HQD_MSG_TYPE;                // 0x3260
  CP_HQD_ATOMIC0_PREOP_LO       :TCP_HQD_ATOMIC0_PREOP_LO;        // 0x3261
  CP_HQD_ATOMIC0_PREOP_HI       :TCP_HQD_ATOMIC0_PREOP_HI;        // 0x3262
  CP_HQD_ATOMIC1_PREOP_LO       :TCP_HQD_ATOMIC1_PREOP_LO;        // 0x3263
  CP_HQD_ATOMIC1_PREOP_HI       :TCP_HQD_ATOMIC1_PREOP_HI;        // 0x3264
  CP_HQD_HQ_STATUS0             :TCP_HQD_HQ_STATUS0;              // 0x3265
  CP_HQD_HQ_CONTROL0            :TCP_HQD_HQ_CONTROL0;             // 0x3266
  CP_MQD_CONTROL                :TCP_MQD_CONTROL;                 // 0x3267
  CP_HQD_HQ_STATUS1             :TCP_HQD_HQ_STATUS1;              // 0x3268
  CP_HQD_HQ_CONTROL1            :TCP_HQD_HQ_CONTROL1;             // 0x3269
  CP_HQD_EOP_BASE_ADDR          :TCP_HQD_EOP_BASE_ADDR;           // 0x326A
  CP_HQD_EOP_BASE_ADDR_HI       :TCP_HQD_EOP_BASE_ADDR_HI;        // 0x326B
  CP_HQD_EOP_CONTROL            :TCP_HQD_EOP_CONTROL;             // 0x326C
  CP_HQD_EOP_RPTR               :TCP_HQD_EOP_RPTR;                // 0x326D
  CP_HQD_EOP_WPTR               :TCP_HQD_EOP_WPTR;                // 0x326E
  CP_HQD_EOP_EVENTS             :TCP_HQD_EOP_EVENTS;              // 0x326F
  CP_HQD_CTX_SAVE_BASE_ADDR_LO  :TCP_HQD_CTX_SAVE_BASE_ADDR_LO;   // 0x3270
  CP_HQD_CTX_SAVE_BASE_ADDR_HI  :TCP_HQD_CTX_SAVE_BASE_ADDR_HI;   // 0x3271
  CP_HQD_CTX_SAVE_CONTROL       :TCP_HQD_CTX_SAVE_CONTROL;        // 0x3272
  CP_HQD_CNTL_STACK_OFFSET      :TCP_HQD_CNTL_STACK_OFFSET;       // 0x3273
  CP_HQD_CNTL_STACK_SIZE        :TCP_HQD_CNTL_STACK_SIZE;         // 0x3274
  CP_HQD_WG_STATE_OFFSET        :TCP_HQD_WG_STATE_OFFSET;         // 0x3275
  CP_HQD_CTX_SAVE_SIZE          :TCP_HQD_CTX_SAVE_SIZE;           // 0x3276
  CP_HQD_GDS_RESOURCE_STATE     :TCP_HQD_GDS_RESOURCE_STATE;      // 0x3277
  CP_HQD_ERROR                  :TCP_HQD_ERROR;                   // 0x3278
  CP_HQD_EOP_WPTR_MEM           :TCP_HQD_EOP_WPTR_MEM;            // 0x3279
  CP_HQD_EOP_DONES              :TCP_HQD_EOP_DONES;               // 0x327A
  REG_327B_329F                 :array[0..36] of DWORD;           // 0x327B
  TCP_WATCH0_ADDR_H             :TTCP_WATCH0_ADDR_H;              // 0x32A0
  TCP_WATCH0_ADDR_L             :TTCP_WATCH0_ADDR_L;              // 0x32A1
  TCP_WATCH0_CNTL               :TTCP_WATCH0_CNTL;                // 0x32A2
  TCP_WATCH1_ADDR_H             :TTCP_WATCH1_ADDR_H;              // 0x32A3
  TCP_WATCH1_ADDR_L             :TTCP_WATCH1_ADDR_L;              // 0x32A4
  TCP_WATCH1_CNTL               :TTCP_WATCH1_CNTL;                // 0x32A5
  TCP_WATCH2_ADDR_H             :TTCP_WATCH2_ADDR_H;              // 0x32A6
  TCP_WATCH2_ADDR_L             :TTCP_WATCH2_ADDR_L;              // 0x32A7
  TCP_WATCH2_CNTL               :TTCP_WATCH2_CNTL;                // 0x32A8
  TCP_WATCH3_ADDR_H             :TTCP_WATCH3_ADDR_H;              // 0x32A9
  TCP_WATCH3_ADDR_L             :TTCP_WATCH3_ADDR_L;              // 0x32AA
  TCP_WATCH3_CNTL               :TTCP_WATCH3_CNTL;                // 0x32AB
  REG_32AC_32AF                 :array[0..3] of DWORD;            // 0x32AC
  TCP_GATCL1_CNTL               :TTCP_GATCL1_CNTL;                // 0x32B0
  TCP_ATC_EDC_GATCL1_CNT        :TTCP_ATC_EDC_GATCL1_CNT;         // 0x32B1
  TCP_GATCL1_DSM_CNTL           :TTCP_GATCL1_DSM_CNTL;            // 0x32B2
  TCP_DSM_CNTL                  :TTCP_DSM_CNTL;                   // 0x32B3
  TCP_CNTL2                     :TTCP_CNTL2;                      // 0x32B4
  REG_32B5_32FF                 :array[0..74] of DWORD;           // 0x32B5
  GDS_VMID0_BASE                :TGDS_VMID0_BASE;                 // 0x3300
  GDS_VMID0_SIZE                :TGDS_VMID0_SIZE;                 // 0x3301
  GDS_VMID1_BASE                :TGDS_VMID1_BASE;                 // 0x3302
  GDS_VMID1_SIZE                :TGDS_VMID1_SIZE;                 // 0x3303
  GDS_VMID2_BASE                :TGDS_VMID2_BASE;                 // 0x3304
  GDS_VMID2_SIZE                :TGDS_VMID2_SIZE;                 // 0x3305
  GDS_VMID3_BASE                :TGDS_VMID3_BASE;                 // 0x3306
  GDS_VMID3_SIZE                :TGDS_VMID3_SIZE;                 // 0x3307
  GDS_VMID4_BASE                :TGDS_VMID4_BASE;                 // 0x3308
  GDS_VMID4_SIZE                :TGDS_VMID4_SIZE;                 // 0x3309
  GDS_VMID5_BASE                :TGDS_VMID5_BASE;                 // 0x330A
  GDS_VMID5_SIZE                :TGDS_VMID5_SIZE;                 // 0x330B
  GDS_VMID6_BASE                :TGDS_VMID6_BASE;                 // 0x330C
  GDS_VMID6_SIZE                :TGDS_VMID6_SIZE;                 // 0x330D
  GDS_VMID7_BASE                :TGDS_VMID7_BASE;                 // 0x330E
  GDS_VMID7_SIZE                :TGDS_VMID7_SIZE;                 // 0x330F
  GDS_VMID8_BASE                :TGDS_VMID8_BASE;                 // 0x3310
  GDS_VMID8_SIZE                :TGDS_VMID8_SIZE;                 // 0x3311
  GDS_VMID9_BASE                :TGDS_VMID9_BASE;                 // 0x3312
  GDS_VMID9_SIZE                :TGDS_VMID9_SIZE;                 // 0x3313
  GDS_VMID10_BASE               :TGDS_VMID10_BASE;                // 0x3314
  GDS_VMID10_SIZE               :TGDS_VMID10_SIZE;                // 0x3315
  GDS_VMID11_BASE               :TGDS_VMID11_BASE;                // 0x3316
  GDS_VMID11_SIZE               :TGDS_VMID11_SIZE;                // 0x3317
  GDS_VMID12_BASE               :TGDS_VMID12_BASE;                // 0x3318
  GDS_VMID12_SIZE               :TGDS_VMID12_SIZE;                // 0x3319
  GDS_VMID13_BASE               :TGDS_VMID13_BASE;                // 0x331A
  GDS_VMID13_SIZE               :TGDS_VMID13_SIZE;                // 0x331B
  GDS_VMID14_BASE               :TGDS_VMID14_BASE;                // 0x331C
  GDS_VMID14_SIZE               :TGDS_VMID14_SIZE;                // 0x331D
  GDS_VMID15_BASE               :TGDS_VMID15_BASE;                // 0x331E
  GDS_VMID15_SIZE               :TGDS_VMID15_SIZE;                // 0x331F
  GDS_GWS_VMID0                 :TGDS_GWS_VMID0;                  // 0x3320
  GDS_GWS_VMID1                 :TGDS_GWS_VMID1;                  // 0x3321
  GDS_GWS_VMID2                 :TGDS_GWS_VMID2;                  // 0x3322
  GDS_GWS_VMID3                 :TGDS_GWS_VMID3;                  // 0x3323
  GDS_GWS_VMID4                 :TGDS_GWS_VMID4;                  // 0x3324
  GDS_GWS_VMID5                 :TGDS_GWS_VMID5;                  // 0x3325
  GDS_GWS_VMID6                 :TGDS_GWS_VMID6;                  // 0x3326
  GDS_GWS_VMID7                 :TGDS_GWS_VMID7;                  // 0x3327
  GDS_GWS_VMID8                 :TGDS_GWS_VMID8;                  // 0x3328
  GDS_GWS_VMID9                 :TGDS_GWS_VMID9;                  // 0x3329
  GDS_GWS_VMID10                :TGDS_GWS_VMID10;                 // 0x332A
  GDS_GWS_VMID11                :TGDS_GWS_VMID11;                 // 0x332B
  GDS_GWS_VMID12                :TGDS_GWS_VMID12;                 // 0x332C
  GDS_GWS_VMID13                :TGDS_GWS_VMID13;                 // 0x332D
  GDS_GWS_VMID14                :TGDS_GWS_VMID14;                 // 0x332E
  GDS_GWS_VMID15                :TGDS_GWS_VMID15;                 // 0x332F
  GDS_OA_VMID0                  :TGDS_OA_VMID0;                   // 0x3330
  GDS_OA_VMID1                  :TGDS_OA_VMID1;                   // 0x3331
  GDS_OA_VMID2                  :TGDS_OA_VMID2;                   // 0x3332
  GDS_OA_VMID3                  :TGDS_OA_VMID3;                   // 0x3333
  GDS_OA_VMID4                  :TGDS_OA_VMID4;                   // 0x3334
  GDS_OA_VMID5                  :TGDS_OA_VMID5;                   // 0x3335
  GDS_OA_VMID6                  :TGDS_OA_VMID6;                   // 0x3336
  GDS_OA_VMID7                  :TGDS_OA_VMID7;                   // 0x3337
  GDS_OA_VMID8                  :TGDS_OA_VMID8;                   // 0x3338
  GDS_OA_VMID9                  :TGDS_OA_VMID9;                   // 0x3339
  GDS_OA_VMID10                 :TGDS_OA_VMID10;                  // 0x333A
  GDS_OA_VMID11                 :TGDS_OA_VMID11;                  // 0x333B
  GDS_OA_VMID12                 :TGDS_OA_VMID12;                  // 0x333C
  GDS_OA_VMID13                 :TGDS_OA_VMID13;                  // 0x333D
  GDS_OA_VMID14                 :TGDS_OA_VMID14;                  // 0x333E
  GDS_OA_VMID15                 :TGDS_OA_VMID15;                  // 0x333F
  REG_3340_3343                 :array[0..3] of DWORD;            // 0x3340
  GDS_GWS_RESET0                :TGDS_GWS_RESET0;                 // 0x3344
  GDS_GWS_RESET1                :TGDS_GWS_RESET1;                 // 0x3345
  GDS_GWS_RESOURCE_RESET        :TGDS_GWS_RESOURCE_RESET;         // 0x3346
  REG_3347                      :DWORD;                           // 0x3347
  GDS_COMPUTE_MAX_WAVE_ID       :TGDS_COMPUTE_MAX_WAVE_ID;        // 0x3348
  GDS_OA_RESET_MASK             :TGDS_OA_RESET_MASK;              // 0x3349
  GDS_OA_RESET                  :TGDS_OA_RESET;                   // 0x334A
  GDS_ENHANCE                   :TGDS_ENHANCE;                    // 0x334B
  GDS_OA_CGPG_RESTORE           :TGDS_OA_CGPG_RESTORE;            // 0x334C
  GDS_CS_CTXSW_STATUS           :TGDS_CS_CTXSW_STATUS;            // 0x334D
  GDS_CS_CTXSW_CNT0             :TGDS_CS_CTXSW_CNT0;              // 0x334E
  GDS_CS_CTXSW_CNT1             :TGDS_CS_CTXSW_CNT1;              // 0x334F
  GDS_CS_CTXSW_CNT2             :TGDS_CS_CTXSW_CNT2;              // 0x3350
  GDS_CS_CTXSW_CNT3             :TGDS_CS_CTXSW_CNT3;              // 0x3351
  GDS_GFX_CTXSW_STATUS          :TGDS_GFX_CTXSW_STATUS;           // 0x3352
  GDS_VS_CTXSW_CNT0             :TGDS_VS_CTXSW_CNT0;              // 0x3353
  GDS_VS_CTXSW_CNT1             :TGDS_VS_CTXSW_CNT1;              // 0x3354
  GDS_VS_CTXSW_CNT2             :TGDS_VS_CTXSW_CNT2;              // 0x3355
  GDS_VS_CTXSW_CNT3             :TGDS_VS_CTXSW_CNT3;              // 0x3356
  GDS_PS0_CTXSW_CNT0            :TGDS_PS0_CTXSW_CNT0;             // 0x3357
  GDS_PS0_CTXSW_CNT1            :TGDS_PS0_CTXSW_CNT1;             // 0x3358
  GDS_PS0_CTXSW_CNT2            :TGDS_PS0_CTXSW_CNT2;             // 0x3359
  GDS_PS0_CTXSW_CNT3            :TGDS_PS0_CTXSW_CNT3;             // 0x335A
  GDS_PS1_CTXSW_CNT0            :TGDS_PS1_CTXSW_CNT0;             // 0x335B
  GDS_PS1_CTXSW_CNT1            :TGDS_PS1_CTXSW_CNT1;             // 0x335C
  GDS_PS1_CTXSW_CNT2            :TGDS_PS1_CTXSW_CNT2;             // 0x335D
  GDS_PS1_CTXSW_CNT3            :TGDS_PS1_CTXSW_CNT3;             // 0x335E
  GDS_PS2_CTXSW_CNT0            :TGDS_PS2_CTXSW_CNT0;             // 0x335F
  GDS_PS2_CTXSW_CNT1            :TGDS_PS2_CTXSW_CNT1;             // 0x3360
  GDS_PS2_CTXSW_CNT2            :TGDS_PS2_CTXSW_CNT2;             // 0x3361
  GDS_PS2_CTXSW_CNT3            :TGDS_PS2_CTXSW_CNT3;             // 0x3362
  GDS_PS3_CTXSW_CNT0            :TGDS_PS3_CTXSW_CNT0;             // 0x3363
  GDS_PS3_CTXSW_CNT1            :TGDS_PS3_CTXSW_CNT1;             // 0x3364
  GDS_PS3_CTXSW_CNT2            :TGDS_PS3_CTXSW_CNT2;             // 0x3365
  GDS_PS3_CTXSW_CNT3            :TGDS_PS3_CTXSW_CNT3;             // 0x3366
  GDS_PS4_CTXSW_CNT0            :TGDS_PS4_CTXSW_CNT0;             // 0x3367
  GDS_PS4_CTXSW_CNT1            :TGDS_PS4_CTXSW_CNT1;             // 0x3368
  GDS_PS4_CTXSW_CNT2            :TGDS_PS4_CTXSW_CNT2;             // 0x3369
  GDS_PS4_CTXSW_CNT3            :TGDS_PS4_CTXSW_CNT3;             // 0x336A
  GDS_PS5_CTXSW_CNT0            :TGDS_PS5_CTXSW_CNT0;             // 0x336B
  GDS_PS5_CTXSW_CNT1            :TGDS_PS5_CTXSW_CNT1;             // 0x336C
  GDS_PS5_CTXSW_CNT2            :TGDS_PS5_CTXSW_CNT2;             // 0x336D
  GDS_PS5_CTXSW_CNT3            :TGDS_PS5_CTXSW_CNT3;             // 0x336E
  GDS_PS6_CTXSW_CNT0            :TGDS_PS6_CTXSW_CNT0;             // 0x336F
  GDS_PS6_CTXSW_CNT1            :TGDS_PS6_CTXSW_CNT1;             // 0x3370
  GDS_PS6_CTXSW_CNT2            :TGDS_PS6_CTXSW_CNT2;             // 0x3371
  GDS_PS6_CTXSW_CNT3            :TGDS_PS6_CTXSW_CNT3;             // 0x3372
  GDS_PS7_CTXSW_CNT0            :TGDS_PS7_CTXSW_CNT0;             // 0x3373
  GDS_PS7_CTXSW_CNT1            :TGDS_PS7_CTXSW_CNT1;             // 0x3374
  GDS_PS7_CTXSW_CNT2            :TGDS_PS7_CTXSW_CNT2;             // 0x3375
  GDS_PS7_CTXSW_CNT3            :TGDS_PS7_CTXSW_CNT3;             // 0x3376
 end;

 TUSERCONFIG_REG_GROUP=bitpacked record
  CP_EOP_DONE_ADDR_LO              :TCP_EOP_DONE_ADDR_LO;               // 0xC000
  CP_EOP_DONE_ADDR_HI              :TCP_EOP_DONE_ADDR_HI;               // 0xC001
  CP_EOP_DONE_DATA_LO              :TCP_EOP_DONE_DATA_LO;               // 0xC002
  CP_EOP_DONE_DATA_HI              :TCP_EOP_DONE_DATA_HI;               // 0xC003
  CP_EOP_LAST_FENCE_LO             :TCP_EOP_LAST_FENCE_LO;              // 0xC004
  CP_EOP_LAST_FENCE_HI             :TCP_EOP_LAST_FENCE_HI;              // 0xC005
  CP_STREAM_OUT_ADDR_LO            :TCP_STREAM_OUT_ADDR_LO;             // 0xC006
  CP_STREAM_OUT_ADDR_HI            :TCP_STREAM_OUT_ADDR_HI;             // 0xC007
  CP_NUM_PRIM_WRITTEN_COUNT0_LO    :TCP_NUM_PRIM_WRITTEN_COUNT0_LO;     // 0xC008
  CP_NUM_PRIM_WRITTEN_COUNT0_HI    :TCP_NUM_PRIM_WRITTEN_COUNT0_HI;     // 0xC009
  CP_NUM_PRIM_NEEDED_COUNT0_LO     :TCP_NUM_PRIM_NEEDED_COUNT0_LO;      // 0xC00A
  CP_NUM_PRIM_NEEDED_COUNT0_HI     :TCP_NUM_PRIM_NEEDED_COUNT0_HI;      // 0xC00B
  CP_NUM_PRIM_WRITTEN_COUNT1_LO    :TCP_NUM_PRIM_WRITTEN_COUNT1_LO;     // 0xC00C
  CP_NUM_PRIM_WRITTEN_COUNT1_HI    :TCP_NUM_PRIM_WRITTEN_COUNT1_HI;     // 0xC00D
  CP_NUM_PRIM_NEEDED_COUNT1_LO     :TCP_NUM_PRIM_NEEDED_COUNT1_LO;      // 0xC00E
  CP_NUM_PRIM_NEEDED_COUNT1_HI     :TCP_NUM_PRIM_NEEDED_COUNT1_HI;      // 0xC00F
  CP_NUM_PRIM_WRITTEN_COUNT2_LO    :TCP_NUM_PRIM_WRITTEN_COUNT2_LO;     // 0xC010
  CP_NUM_PRIM_WRITTEN_COUNT2_HI    :TCP_NUM_PRIM_WRITTEN_COUNT2_HI;     // 0xC011
  CP_NUM_PRIM_NEEDED_COUNT2_LO     :TCP_NUM_PRIM_NEEDED_COUNT2_LO;      // 0xC012
  CP_NUM_PRIM_NEEDED_COUNT2_HI     :TCP_NUM_PRIM_NEEDED_COUNT2_HI;      // 0xC013
  CP_NUM_PRIM_WRITTEN_COUNT3_LO    :TCP_NUM_PRIM_WRITTEN_COUNT3_LO;     // 0xC014
  CP_NUM_PRIM_WRITTEN_COUNT3_HI    :TCP_NUM_PRIM_WRITTEN_COUNT3_HI;     // 0xC015
  CP_NUM_PRIM_NEEDED_COUNT3_LO     :TCP_NUM_PRIM_NEEDED_COUNT3_LO;      // 0xC016
  CP_NUM_PRIM_NEEDED_COUNT3_HI     :TCP_NUM_PRIM_NEEDED_COUNT3_HI;      // 0xC017
  CP_PIPE_STATS_ADDR_LO            :TCP_PIPE_STATS_ADDR_LO;             // 0xC018
  CP_PIPE_STATS_ADDR_HI            :TCP_PIPE_STATS_ADDR_HI;             // 0xC019
  CP_VGT_IAVERT_COUNT_LO           :TCP_VGT_IAVERT_COUNT_LO;            // 0xC01A
  CP_VGT_IAVERT_COUNT_HI           :TCP_VGT_IAVERT_COUNT_HI;            // 0xC01B
  CP_VGT_IAPRIM_COUNT_LO           :TCP_VGT_IAPRIM_COUNT_LO;            // 0xC01C
  CP_VGT_IAPRIM_COUNT_HI           :TCP_VGT_IAPRIM_COUNT_HI;            // 0xC01D
  CP_VGT_GSPRIM_COUNT_LO           :TCP_VGT_GSPRIM_COUNT_LO;            // 0xC01E
  CP_VGT_GSPRIM_COUNT_HI           :TCP_VGT_GSPRIM_COUNT_HI;            // 0xC01F
  CP_VGT_VSINVOC_COUNT_LO          :TCP_VGT_VSINVOC_COUNT_LO;           // 0xC020
  CP_VGT_VSINVOC_COUNT_HI          :TCP_VGT_VSINVOC_COUNT_HI;           // 0xC021
  CP_VGT_GSINVOC_COUNT_LO          :TCP_VGT_GSINVOC_COUNT_LO;           // 0xC022
  CP_VGT_GSINVOC_COUNT_HI          :TCP_VGT_GSINVOC_COUNT_HI;           // 0xC023
  CP_VGT_HSINVOC_COUNT_LO          :TCP_VGT_HSINVOC_COUNT_LO;           // 0xC024
  CP_VGT_HSINVOC_COUNT_HI          :TCP_VGT_HSINVOC_COUNT_HI;           // 0xC025
  CP_VGT_DSINVOC_COUNT_LO          :TCP_VGT_DSINVOC_COUNT_LO;           // 0xC026
  CP_VGT_DSINVOC_COUNT_HI          :TCP_VGT_DSINVOC_COUNT_HI;           // 0xC027
  CP_PA_CINVOC_COUNT_LO            :TCP_PA_CINVOC_COUNT_LO;             // 0xC028
  CP_PA_CINVOC_COUNT_HI            :TCP_PA_CINVOC_COUNT_HI;             // 0xC029
  CP_PA_CPRIM_COUNT_LO             :TCP_PA_CPRIM_COUNT_LO;              // 0xC02A
  CP_PA_CPRIM_COUNT_HI             :TCP_PA_CPRIM_COUNT_HI;              // 0xC02B
  CP_SC_PSINVOC_COUNT0_LO          :TCP_SC_PSINVOC_COUNT0_LO;           // 0xC02C
  CP_SC_PSINVOC_COUNT0_HI          :TCP_SC_PSINVOC_COUNT0_HI;           // 0xC02D
  CP_SC_PSINVOC_COUNT1_LO          :TCP_SC_PSINVOC_COUNT1_LO;           // 0xC02E
  CP_SC_PSINVOC_COUNT1_HI          :TCP_SC_PSINVOC_COUNT1_HI;           // 0xC02F
  CP_VGT_CSINVOC_COUNT_LO          :TCP_VGT_CSINVOC_COUNT_LO;           // 0xC030
  CP_VGT_CSINVOC_COUNT_HI          :TCP_VGT_CSINVOC_COUNT_HI;           // 0xC031
  REG_C032_C03C                    :array[0..10] of DWORD;              // 0xC032
  CP_PIPE_STATS_CONTROL            :TCP_PIPE_STATS_CONTROL;             // 0xC03D
  CP_STREAM_OUT_CONTROL            :TCP_STREAM_OUT_CONTROL;             // 0xC03E
  CP_STRMOUT_CNTL                  :TCP_STRMOUT_CNTL;                   // 0xC03F
  REG_C040_C051                    :array[0..17] of DWORD;              // 0xC040
  CP_PFP_ATOMIC_PREOP_LO           :TCP_PFP_ATOMIC_PREOP_LO;            // 0xC052
  CP_PFP_ATOMIC_PREOP_HI           :TCP_PFP_ATOMIC_PREOP_HI;            // 0xC053
  CP_PFP_GDS_ATOMIC0_PREOP_LO      :TCP_PFP_GDS_ATOMIC0_PREOP_LO;       // 0xC054
  CP_PFP_GDS_ATOMIC0_PREOP_HI      :TCP_PFP_GDS_ATOMIC0_PREOP_HI;       // 0xC055
  CP_PFP_GDS_ATOMIC1_PREOP_LO      :TCP_PFP_GDS_ATOMIC1_PREOP_LO;       // 0xC056
  CP_PFP_GDS_ATOMIC1_PREOP_HI      :TCP_PFP_GDS_ATOMIC1_PREOP_HI;       // 0xC057
  CP_APPEND_ADDR_LO                :TCP_APPEND_ADDR_LO;                 // 0xC058
  CP_APPEND_ADDR_HI                :TCP_APPEND_ADDR_HI;                 // 0xC059
  CP_APPEND_DATA                   :TCP_APPEND_DATA;                    // 0xC05A
  CP_APPEND_LAST_CS_FENCE          :TCP_APPEND_LAST_CS_FENCE;           // 0xC05B
  CP_APPEND_LAST_PS_FENCE          :TCP_APPEND_LAST_PS_FENCE;           // 0xC05C
  CP_ATOMIC_PREOP_LO               :TCP_ATOMIC_PREOP_LO;                // 0xC05D
  CP_ATOMIC_PREOP_HI               :TCP_ATOMIC_PREOP_HI;                // 0xC05E
  CP_GDS_ATOMIC0_PREOP_LO          :TCP_GDS_ATOMIC0_PREOP_LO;           // 0xC05F
  CP_GDS_ATOMIC0_PREOP_HI          :TCP_GDS_ATOMIC0_PREOP_HI;           // 0xC060
  CP_GDS_ATOMIC1_PREOP_LO          :TCP_GDS_ATOMIC1_PREOP_LO;           // 0xC061
  CP_GDS_ATOMIC1_PREOP_HI          :TCP_GDS_ATOMIC1_PREOP_HI;           // 0xC062
  REG_C063_C06E                    :array[0..11] of DWORD;              // 0xC063
  CP_SEM_WAIT_TIMER                :TCP_SEM_WAIT_TIMER;                 // 0xC06F
  CP_SIG_SEM_ADDR_LO               :TCP_SIG_SEM_ADDR_LO;                // 0xC070
  CP_SIG_SEM_ADDR_HI               :TCP_SIG_SEM_ADDR_HI;                // 0xC071
  REG_C072_C073                    :array[0..1] of DWORD;               // 0xC072
  CP_WAIT_REG_MEM_TIMEOUT          :TCP_WAIT_REG_MEM_TIMEOUT;           // 0xC074
  CP_WAIT_SEM_ADDR_LO              :TCP_WAIT_SEM_ADDR_LO;               // 0xC075
  CP_WAIT_SEM_ADDR_HI              :TCP_WAIT_SEM_ADDR_HI;               // 0xC076
  CP_DMA_PFP_CONTROL               :TCP_DMA_PFP_CONTROL;                // 0xC077
  CP_DMA_ME_CONTROL                :TCP_DMA_ME_CONTROL;                 // 0xC078
  CP_COHER_BASE_HI                 :TCP_COHER_BASE_HI;                  // 0xC079
  REG_C07A                         :DWORD;                              // 0xC07A
  CP_COHER_START_DELAY             :TCP_COHER_START_DELAY;              // 0xC07B
  CP_COHER_CNTL                    :TCP_COHER_CNTL;                     // 0xC07C
  CP_COHER_SIZE                    :TCP_COHER_SIZE;                     // 0xC07D
  CP_COHER_BASE                    :TCP_COHER_BASE;                     // 0xC07E
  CP_COHER_STATUS                  :TCP_COHER_STATUS;                   // 0xC07F
  CP_DMA_ME_SRC_ADDR               :TCP_DMA_ME_SRC_ADDR;                // 0xC080
  CP_DMA_ME_SRC_ADDR_HI            :TCP_DMA_ME_SRC_ADDR_HI;             // 0xC081
  CP_DMA_ME_DST_ADDR               :TCP_DMA_ME_DST_ADDR;                // 0xC082
  CP_DMA_ME_DST_ADDR_HI            :TCP_DMA_ME_DST_ADDR_HI;             // 0xC083
  CP_DMA_ME_COMMAND                :TCP_DMA_ME_COMMAND;                 // 0xC084
  CP_DMA_PFP_SRC_ADDR              :TCP_DMA_PFP_SRC_ADDR;               // 0xC085
  CP_DMA_PFP_SRC_ADDR_HI           :TCP_DMA_PFP_SRC_ADDR_HI;            // 0xC086
  CP_DMA_PFP_DST_ADDR              :TCP_DMA_PFP_DST_ADDR;               // 0xC087
  CP_DMA_PFP_DST_ADDR_HI           :TCP_DMA_PFP_DST_ADDR_HI;            // 0xC088
  CP_DMA_PFP_COMMAND               :TCP_DMA_PFP_COMMAND;                // 0xC089
  CP_DMA_CNTL                      :TCP_DMA_CNTL;                       // 0xC08A
  CP_DMA_READ_TAGS                 :TCP_DMA_READ_TAGS;                  // 0xC08B
  CP_COHER_SIZE_HI                 :TCP_COHER_SIZE_HI;                  // 0xC08C
  CP_PFP_IB_CONTROL                :TCP_PFP_IB_CONTROL;                 // 0xC08D
  CP_PFP_LOAD_CONTROL              :TCP_PFP_LOAD_CONTROL;               // 0xC08E
  CP_SCRATCH_INDEX                 :TCP_SCRATCH_INDEX;                  // 0xC08F
  CP_SCRATCH_DATA                  :TCP_SCRATCH_DATA;                   // 0xC090
  REG_C091                         :DWORD;                              // 0xC091
  CP_IB1_OFFSET                    :TCP_IB1_OFFSET;                     // 0xC092
  CP_IB2_OFFSET                    :TCP_IB2_OFFSET;                     // 0xC093
  CP_IB1_PREAMBLE_BEGIN            :TCP_IB1_PREAMBLE_BEGIN;             // 0xC094
  CP_IB1_PREAMBLE_END              :TCP_IB1_PREAMBLE_END;               // 0xC095
  CP_IB2_PREAMBLE_BEGIN            :TCP_IB2_PREAMBLE_BEGIN;             // 0xC096
  CP_IB2_PREAMBLE_END              :TCP_IB2_PREAMBLE_END;               // 0xC097
  CP_CE_IB1_OFFSET                 :TCP_CE_IB1_OFFSET;                  // 0xC098
  CP_CE_IB2_OFFSET                 :TCP_CE_IB2_OFFSET;                  // 0xC099
  CP_CE_COUNTER                    :TCP_CE_COUNTER;                     // 0xC09A
  CP_CE_RB_OFFSET                  :TCP_CE_RB_OFFSET;                   // 0xC09B
  REG_C09C_C0C2                    :array[0..38] of DWORD;              // 0xC09C
  CP_CE_INIT_BASE_LO               :TCP_CE_INIT_BASE_LO;                // 0xC0C3
  CP_CE_INIT_BASE_HI               :TCP_CE_INIT_BASE_HI;                // 0xC0C4
  CP_CE_INIT_BUFSZ                 :TCP_CE_INIT_BUFSZ;                  // 0xC0C5
  CP_CE_IB1_BASE_LO                :TCP_CE_IB1_BASE_LO;                 // 0xC0C6
  CP_CE_IB1_BASE_HI                :TCP_CE_IB1_BASE_HI;                 // 0xC0C7
  CP_CE_IB1_BUFSZ                  :TCP_CE_IB1_BUFSZ;                   // 0xC0C8
  CP_CE_IB2_BASE_LO                :TCP_CE_IB2_BASE_LO;                 // 0xC0C9
  CP_CE_IB2_BASE_HI                :TCP_CE_IB2_BASE_HI;                 // 0xC0CA
  CP_CE_IB2_BUFSZ                  :TCP_CE_IB2_BUFSZ;                   // 0xC0CB
  CP_IB1_BASE_LO                   :TCP_IB1_BASE_LO;                    // 0xC0CC
  CP_IB1_BASE_HI                   :TCP_IB1_BASE_HI;                    // 0xC0CD
  CP_IB1_BUFSZ                     :TCP_IB1_BUFSZ;                      // 0xC0CE
  CP_IB2_BASE_LO                   :TCP_IB2_BASE_LO;                    // 0xC0CF
  CP_IB2_BASE_HI                   :TCP_IB2_BASE_HI;                    // 0xC0D0
  CP_IB2_BUFSZ                     :TCP_IB2_BUFSZ;                      // 0xC0D1
  CP_ST_BASE_LO                    :TCP_ST_BASE_LO;                     // 0xC0D2
  CP_ST_BASE_HI                    :TCP_ST_BASE_HI;                     // 0xC0D3
  CP_ST_BUFSZ                      :TCP_ST_BUFSZ;                       // 0xC0D4
  CP_EOP_DONE_EVENT_CNTL           :TCP_EOP_DONE_EVENT_CNTL;            // 0xC0D5
  CP_EOP_DONE_DATA_CNTL            :TCP_EOP_DONE_DATA_CNTL;             // 0xC0D6
  CP_EOP_DONE_CNTX_ID              :TCP_EOP_DONE_CNTX_ID;               // 0xC0D7
  REG_C0D8_C0EB                    :array[0..19] of DWORD;              // 0xC0D8
  CP_PFP_COMPLETION_STATUS         :TCP_PFP_COMPLETION_STATUS;          // 0xC0EC
  CP_CE_COMPLETION_STATUS          :TCP_CE_COMPLETION_STATUS;           // 0xC0ED
  CP_PRED_NOT_VISIBLE              :TCP_PRED_NOT_VISIBLE;               // 0xC0EE
  REG_C0EF                         :DWORD;                              // 0xC0EF
  CP_PFP_METADATA_BASE_ADDR        :TCP_PFP_METADATA_BASE_ADDR;         // 0xC0F0
  CP_PFP_METADATA_BASE_ADDR_HI     :TCP_PFP_METADATA_BASE_ADDR_HI;      // 0xC0F1
  CP_CE_METADATA_BASE_ADDR         :TCP_CE_METADATA_BASE_ADDR;          // 0xC0F2
  CP_CE_METADATA_BASE_ADDR_HI      :TCP_CE_METADATA_BASE_ADDR_HI;       // 0xC0F3
  CP_DRAW_INDX_INDR_ADDR           :TCP_DRAW_INDX_INDR_ADDR;            // 0xC0F4
  CP_DRAW_INDX_INDR_ADDR_HI        :TCP_DRAW_INDX_INDR_ADDR_HI;         // 0xC0F5
  CP_DISPATCH_INDR_ADDR            :TCP_DISPATCH_INDR_ADDR;             // 0xC0F6
  CP_DISPATCH_INDR_ADDR_HI         :TCP_DISPATCH_INDR_ADDR_HI;          // 0xC0F7
  CP_INDEX_BASE_ADDR               :TCP_INDEX_BASE_ADDR;                // 0xC0F8
  CP_INDEX_BASE_ADDR_HI            :TCP_INDEX_BASE_ADDR_HI;             // 0xC0F9
  CP_INDEX_TYPE                    :TCP_INDEX_TYPE;                     // 0xC0FA
  CP_GDS_BKUP_ADDR                 :TCP_GDS_BKUP_ADDR;                  // 0xC0FB
  CP_GDS_BKUP_ADDR_HI              :TCP_GDS_BKUP_ADDR_HI;               // 0xC0FC
  CP_SAMPLE_STATUS                 :TCP_SAMPLE_STATUS;                  // 0xC0FD
  REG_C0FE_C1FF                    :array[0..257] of DWORD;             // 0xC0FE
  GRBM_GFX_INDEX                   :TGRBM_GFX_INDEX;                    // 0xC200
  REG_C201_C23F                    :array[0..62] of DWORD;              // 0xC201
  VGT_ESGS_RING_SIZE               :TVGT_ESGS_RING_SIZE;                // 0xC240
  VGT_GSVS_RING_SIZE               :TVGT_GSVS_RING_SIZE;                // 0xC241
  VGT_PRIMITIVE_TYPE               :TVGT_PRIMITIVE_TYPE;                // 0xC242
  VGT_INDEX_TYPE                   :TVGT_INDEX_TYPE;                    // 0xC243
  VGT_STRMOUT_BUFFER_FILLED_SIZE_0 :TVGT_STRMOUT_BUFFER_FILLED_SIZE_0;  // 0xC244
  VGT_STRMOUT_BUFFER_FILLED_SIZE_1 :TVGT_STRMOUT_BUFFER_FILLED_SIZE_1;  // 0xC245
  VGT_STRMOUT_BUFFER_FILLED_SIZE_2 :TVGT_STRMOUT_BUFFER_FILLED_SIZE_2;  // 0xC246
  VGT_STRMOUT_BUFFER_FILLED_SIZE_3 :TVGT_STRMOUT_BUFFER_FILLED_SIZE_3;  // 0xC247
  REG_C248_C24B                    :array[0..3] of DWORD;               // 0xC248
  VGT_NUM_INDICES                  :TVGT_NUM_INDICES;                   // 0xC24C
  VGT_NUM_INSTANCES                :TVGT_NUM_INSTANCES;                 // 0xC24D
  VGT_TF_RING_SIZE                 :TVGT_TF_RING_SIZE;                  // 0xC24E
  VGT_HS_OFFCHIP_PARAM             :TVGT_HS_OFFCHIP_PARAM;              // 0xC24F
  VGT_TF_MEMORY_BASE               :TVGT_TF_MEMORY_BASE;                // 0xC250
  REG_C251_C27F                    :array[0..46] of DWORD;              // 0xC251
  PA_SU_LINE_STIPPLE_VALUE         :TPA_SU_LINE_STIPPLE_VALUE;          // 0xC280
  PA_SC_LINE_STIPPLE_STATE         :TPA_SC_LINE_STIPPLE_STATE;          // 0xC281
  REG_C282_C29F                    :array[0..29] of DWORD;              // 0xC282
  PA_SC_P3D_TRAP_SCREEN_HV_EN      :TPA_SC_P3D_TRAP_SCREEN_HV_EN;       // 0xC2A0
  PA_SC_P3D_TRAP_SCREEN_H          :TPA_SC_P3D_TRAP_SCREEN_H;           // 0xC2A1
  PA_SC_P3D_TRAP_SCREEN_V          :TPA_SC_P3D_TRAP_SCREEN_V;           // 0xC2A2
  PA_SC_P3D_TRAP_SCREEN_OCCURRENCE :TPA_SC_P3D_TRAP_SCREEN_OCCURRENCE;  // 0xC2A3
  PA_SC_P3D_TRAP_SCREEN_COUNT      :TPA_SC_P3D_TRAP_SCREEN_COUNT;       // 0xC2A4
  REG_C2A5_C2A7                    :array[0..2] of DWORD;               // 0xC2A5
  PA_SC_HP3D_TRAP_SCREEN_HV_EN     :TPA_SC_HP3D_TRAP_SCREEN_HV_EN;      // 0xC2A8
  PA_SC_HP3D_TRAP_SCREEN_H         :TPA_SC_HP3D_TRAP_SCREEN_H;          // 0xC2A9
  PA_SC_HP3D_TRAP_SCREEN_V         :TPA_SC_HP3D_TRAP_SCREEN_V;          // 0xC2AA
  PA_SC_HP3D_TRAP_SCREEN_OCCURRENCE:TPA_SC_HP3D_TRAP_SCREEN_OCCURRENCE; // 0xC2AB
  PA_SC_HP3D_TRAP_SCREEN_COUNT     :TPA_SC_HP3D_TRAP_SCREEN_COUNT;      // 0xC2AC
  REG_C2AD_C2AF                    :array[0..2] of DWORD;               // 0xC2AD
  PA_SC_TRAP_SCREEN_HV_EN          :TPA_SC_TRAP_SCREEN_HV_EN;           // 0xC2B0
  PA_SC_TRAP_SCREEN_H              :TPA_SC_TRAP_SCREEN_H;               // 0xC2B1
  PA_SC_TRAP_SCREEN_V              :TPA_SC_TRAP_SCREEN_V;               // 0xC2B2
  PA_SC_TRAP_SCREEN_OCCURRENCE     :TPA_SC_TRAP_SCREEN_OCCURRENCE;      // 0xC2B3
  PA_SC_TRAP_SCREEN_COUNT          :TPA_SC_TRAP_SCREEN_COUNT;           // 0xC2B4
  REG_C2B5_C32F                    :array[0..122] of DWORD;             // 0xC2B5
  SQ_THREAD_TRACE_BASE             :TSQ_THREAD_TRACE_BASE;              // 0xC330
  SQ_THREAD_TRACE_SIZE             :TSQ_THREAD_TRACE_SIZE;              // 0xC331
  SQ_THREAD_TRACE_MASK             :TSQ_THREAD_TRACE_MASK;              // 0xC332
  SQ_THREAD_TRACE_TOKEN_MASK       :TSQ_THREAD_TRACE_TOKEN_MASK;        // 0xC333
  SQ_THREAD_TRACE_PERF_MASK        :TSQ_THREAD_TRACE_PERF_MASK;         // 0xC334
  SQ_THREAD_TRACE_CTRL             :TSQ_THREAD_TRACE_CTRL;              // 0xC335
  SQ_THREAD_TRACE_MODE             :TSQ_THREAD_TRACE_MODE;              // 0xC336
  SQ_THREAD_TRACE_BASE2            :TSQ_THREAD_TRACE_BASE2;             // 0xC337
  SQ_THREAD_TRACE_TOKEN_MASK2      :TSQ_THREAD_TRACE_TOKEN_MASK2;       // 0xC338
  SQ_THREAD_TRACE_WPTR             :TSQ_THREAD_TRACE_WPTR;              // 0xC339
  SQ_THREAD_TRACE_STATUS           :TSQ_THREAD_TRACE_STATUS;            // 0xC33A
  SQ_THREAD_TRACE_HIWATER          :TSQ_THREAD_TRACE_HIWATER;           // 0xC33B
  REG_C33C_C33F                    :array[0..3] of DWORD;               // 0xC33C
  SQ_THREAD_TRACE_USERDATA_0       :TSQ_THREAD_TRACE_USERDATA_0;        // 0xC340
  SQ_THREAD_TRACE_USERDATA_1       :TSQ_THREAD_TRACE_USERDATA_1;        // 0xC341
  SQ_THREAD_TRACE_USERDATA_2       :TSQ_THREAD_TRACE_USERDATA_2;        // 0xC342
  SQ_THREAD_TRACE_USERDATA_3       :TSQ_THREAD_TRACE_USERDATA_3;        // 0xC343
  REG_C344_C37F                    :array[0..59] of DWORD;              // 0xC344
  TA_CS_BC_BASE_ADDR               :TTA_CS_BC_BASE_ADDR;                // 0xC380
  TA_CS_BC_BASE_ADDR_HI            :TTA_CS_BC_BASE_ADDR_HI;             // 0xC381
  REG_C382_C3BF                    :array[0..61] of DWORD;              // 0xC382
  DB_OCCLUSION_COUNT0_LOW          :TDB_OCCLUSION_COUNT0_LOW;           // 0xC3C0
  DB_OCCLUSION_COUNT0_HI           :TDB_OCCLUSION_COUNT0_HI;            // 0xC3C1
  DB_OCCLUSION_COUNT1_LOW          :TDB_OCCLUSION_COUNT1_LOW;           // 0xC3C2
  DB_OCCLUSION_COUNT1_HI           :TDB_OCCLUSION_COUNT1_HI;            // 0xC3C3
  DB_OCCLUSION_COUNT2_LOW          :TDB_OCCLUSION_COUNT2_LOW;           // 0xC3C4
  DB_OCCLUSION_COUNT2_HI           :TDB_OCCLUSION_COUNT2_HI;            // 0xC3C5
  DB_OCCLUSION_COUNT3_LOW          :TDB_OCCLUSION_COUNT3_LOW;           // 0xC3C6
  DB_OCCLUSION_COUNT3_HI           :TDB_OCCLUSION_COUNT3_HI;            // 0xC3C7
  REG_C3C8_C3FD                    :array[0..53] of DWORD;              // 0xC3C8
  DB_ZPASS_COUNT_LOW               :TDB_ZPASS_COUNT_LOW;                // 0xC3FE
  DB_ZPASS_COUNT_HI                :TDB_ZPASS_COUNT_HI;                 // 0xC3FF
  GDS_RD_ADDR                      :TGDS_RD_ADDR;                       // 0xC400
  GDS_RD_DATA                      :TGDS_RD_DATA;                       // 0xC401
  GDS_RD_BURST_ADDR                :TGDS_RD_BURST_ADDR;                 // 0xC402
  GDS_RD_BURST_COUNT               :TGDS_RD_BURST_COUNT;                // 0xC403
  GDS_RD_BURST_DATA                :TGDS_RD_BURST_DATA;                 // 0xC404
  GDS_WR_ADDR                      :TGDS_WR_ADDR;                       // 0xC405
  GDS_WR_DATA                      :TGDS_WR_DATA;                       // 0xC406
  GDS_WR_BURST_ADDR                :TGDS_WR_BURST_ADDR;                 // 0xC407
  GDS_WR_BURST_DATA                :TGDS_WR_BURST_DATA;                 // 0xC408
  GDS_WRITE_COMPLETE               :TGDS_WRITE_COMPLETE;                // 0xC409
  GDS_ATOM_CNTL                    :TGDS_ATOM_CNTL;                     // 0xC40A
  GDS_ATOM_COMPLETE                :TGDS_ATOM_COMPLETE;                 // 0xC40B
  GDS_ATOM_BASE                    :TGDS_ATOM_BASE;                     // 0xC40C
  GDS_ATOM_SIZE                    :TGDS_ATOM_SIZE;                     // 0xC40D
  GDS_ATOM_OFFSET0                 :TGDS_ATOM_OFFSET0;                  // 0xC40E
  GDS_ATOM_OFFSET1                 :TGDS_ATOM_OFFSET1;                  // 0xC40F
  GDS_ATOM_DST                     :TGDS_ATOM_DST;                      // 0xC410
  GDS_ATOM_OP                      :TGDS_ATOM_OP;                       // 0xC411
  GDS_ATOM_SRC0                    :TGDS_ATOM_SRC0;                     // 0xC412
  GDS_ATOM_SRC0_U                  :TGDS_ATOM_SRC0_U;                   // 0xC413
  GDS_ATOM_SRC1                    :TGDS_ATOM_SRC1;                     // 0xC414
  GDS_ATOM_SRC1_U                  :TGDS_ATOM_SRC1_U;                   // 0xC415
  GDS_ATOM_READ0                   :TGDS_ATOM_READ0;                    // 0xC416
  GDS_ATOM_READ0_U                 :TGDS_ATOM_READ0_U;                  // 0xC417
  GDS_ATOM_READ1                   :TGDS_ATOM_READ1;                    // 0xC418
  GDS_ATOM_READ1_U                 :TGDS_ATOM_READ1_U;                  // 0xC419
  GDS_GWS_RESOURCE_CNTL            :TGDS_GWS_RESOURCE_CNTL;             // 0xC41A
  GDS_GWS_RESOURCE                 :TGDS_GWS_RESOURCE;                  // 0xC41B
  GDS_GWS_RESOURCE_CNT             :TGDS_GWS_RESOURCE_CNT;              // 0xC41C
  GDS_OA_CNTL                      :TGDS_OA_CNTL;                       // 0xC41D
  GDS_OA_COUNTER                   :TGDS_OA_COUNTER;                    // 0xC41E
  GDS_OA_ADDRESS                   :TGDS_OA_ADDRESS;                    // 0xC41F
  GDS_OA_INCDEC                    :TGDS_OA_INCDEC;                     // 0xC420
  GDS_OA_RING_SIZE                 :TGDS_OA_RING_SIZE;                  // 0xC421
  REG_C422_CFFF                    :array[0..3037] of DWORD;            // 0xC422
  CPG_PERFCOUNTER1_LO              :TCPG_PERFCOUNTER1_LO;               // 0xD000
  CPG_PERFCOUNTER1_HI              :TCPG_PERFCOUNTER1_HI;               // 0xD001
  CPG_PERFCOUNTER0_LO              :TCPG_PERFCOUNTER0_LO;               // 0xD002
  CPG_PERFCOUNTER0_HI              :TCPG_PERFCOUNTER0_HI;               // 0xD003
  CPC_PERFCOUNTER1_LO              :TCPC_PERFCOUNTER1_LO;               // 0xD004
  CPC_PERFCOUNTER1_HI              :TCPC_PERFCOUNTER1_HI;               // 0xD005
  CPC_PERFCOUNTER0_LO              :TCPC_PERFCOUNTER0_LO;               // 0xD006
  CPC_PERFCOUNTER0_HI              :TCPC_PERFCOUNTER0_HI;               // 0xD007
  CPF_PERFCOUNTER1_LO              :TCPF_PERFCOUNTER1_LO;               // 0xD008
  CPF_PERFCOUNTER1_HI              :TCPF_PERFCOUNTER1_HI;               // 0xD009
  CPF_PERFCOUNTER0_LO              :TCPF_PERFCOUNTER0_LO;               // 0xD00A
  CPF_PERFCOUNTER0_HI              :TCPF_PERFCOUNTER0_HI;               // 0xD00B
  REG_D00C_D03F                    :array[0..51] of DWORD;              // 0xD00C
  GRBM_PERFCOUNTER0_LO             :TGRBM_PERFCOUNTER0_LO;              // 0xD040
  GRBM_PERFCOUNTER0_HI             :TGRBM_PERFCOUNTER0_HI;              // 0xD041
  REG_D042                         :DWORD;                              // 0xD042
  GRBM_PERFCOUNTER1_LO             :TGRBM_PERFCOUNTER1_LO;              // 0xD043
  GRBM_PERFCOUNTER1_HI             :TGRBM_PERFCOUNTER1_HI;              // 0xD044
  GRBM_SE0_PERFCOUNTER_LO          :TGRBM_SE0_PERFCOUNTER_LO;           // 0xD045
  GRBM_SE0_PERFCOUNTER_HI          :TGRBM_SE0_PERFCOUNTER_HI;           // 0xD046
  GRBM_SE1_PERFCOUNTER_LO          :TGRBM_SE1_PERFCOUNTER_LO;           // 0xD047
  GRBM_SE1_PERFCOUNTER_HI          :TGRBM_SE1_PERFCOUNTER_HI;           // 0xD048
  GRBM_SE2_PERFCOUNTER_LO          :TGRBM_SE2_PERFCOUNTER_LO;           // 0xD049
  GRBM_SE2_PERFCOUNTER_HI          :TGRBM_SE2_PERFCOUNTER_HI;           // 0xD04A
  GRBM_SE3_PERFCOUNTER_LO          :TGRBM_SE3_PERFCOUNTER_LO;           // 0xD04B
  GRBM_SE3_PERFCOUNTER_HI          :TGRBM_SE3_PERFCOUNTER_HI;           // 0xD04C
  REG_D04D_D07F                    :array[0..50] of DWORD;              // 0xD04D
  WD_PERFCOUNTER0_LO               :TWD_PERFCOUNTER0_LO;                // 0xD080
  WD_PERFCOUNTER0_HI               :TWD_PERFCOUNTER0_HI;                // 0xD081
  WD_PERFCOUNTER1_LO               :TWD_PERFCOUNTER1_LO;                // 0xD082
  WD_PERFCOUNTER1_HI               :TWD_PERFCOUNTER1_HI;                // 0xD083
  WD_PERFCOUNTER2_LO               :TWD_PERFCOUNTER2_LO;                // 0xD084
  WD_PERFCOUNTER2_HI               :TWD_PERFCOUNTER2_HI;                // 0xD085
  WD_PERFCOUNTER3_LO               :TWD_PERFCOUNTER3_LO;                // 0xD086
  WD_PERFCOUNTER3_HI               :TWD_PERFCOUNTER3_HI;                // 0xD087
  IA_PERFCOUNTER0_LO               :TIA_PERFCOUNTER0_LO;                // 0xD088
  IA_PERFCOUNTER0_HI               :TIA_PERFCOUNTER0_HI;                // 0xD089
  IA_PERFCOUNTER1_LO               :TIA_PERFCOUNTER1_LO;                // 0xD08A
  IA_PERFCOUNTER1_HI               :TIA_PERFCOUNTER1_HI;                // 0xD08B
  IA_PERFCOUNTER2_LO               :TIA_PERFCOUNTER2_LO;                // 0xD08C
  IA_PERFCOUNTER2_HI               :TIA_PERFCOUNTER2_HI;                // 0xD08D
  IA_PERFCOUNTER3_LO               :TIA_PERFCOUNTER3_LO;                // 0xD08E
  IA_PERFCOUNTER3_HI               :TIA_PERFCOUNTER3_HI;                // 0xD08F
  VGT_PERFCOUNTER0_LO              :TVGT_PERFCOUNTER0_LO;               // 0xD090
  VGT_PERFCOUNTER0_HI              :TVGT_PERFCOUNTER0_HI;               // 0xD091
  VGT_PERFCOUNTER1_LO              :TVGT_PERFCOUNTER1_LO;               // 0xD092
  VGT_PERFCOUNTER1_HI              :TVGT_PERFCOUNTER1_HI;               // 0xD093
  VGT_PERFCOUNTER2_LO              :TVGT_PERFCOUNTER2_LO;               // 0xD094
  VGT_PERFCOUNTER2_HI              :TVGT_PERFCOUNTER2_HI;               // 0xD095
  VGT_PERFCOUNTER3_LO              :TVGT_PERFCOUNTER3_LO;               // 0xD096
  VGT_PERFCOUNTER3_HI              :TVGT_PERFCOUNTER3_HI;               // 0xD097
  REG_D098_D0FF                    :array[0..103] of DWORD;             // 0xD098
  PA_SU_PERFCOUNTER0_LO            :TPA_SU_PERFCOUNTER0_LO;             // 0xD100
  PA_SU_PERFCOUNTER0_HI            :TPA_SU_PERFCOUNTER0_HI;             // 0xD101
  PA_SU_PERFCOUNTER1_LO            :TPA_SU_PERFCOUNTER1_LO;             // 0xD102
  PA_SU_PERFCOUNTER1_HI            :TPA_SU_PERFCOUNTER1_HI;             // 0xD103
  PA_SU_PERFCOUNTER2_LO            :TPA_SU_PERFCOUNTER2_LO;             // 0xD104
  PA_SU_PERFCOUNTER2_HI            :TPA_SU_PERFCOUNTER2_HI;             // 0xD105
  PA_SU_PERFCOUNTER3_LO            :TPA_SU_PERFCOUNTER3_LO;             // 0xD106
  PA_SU_PERFCOUNTER3_HI            :TPA_SU_PERFCOUNTER3_HI;             // 0xD107
  REG_D108_D13F                    :array[0..55] of DWORD;              // 0xD108
  PA_SC_PERFCOUNTER0_LO            :TPA_SC_PERFCOUNTER0_LO;             // 0xD140
  PA_SC_PERFCOUNTER0_HI            :TPA_SC_PERFCOUNTER0_HI;             // 0xD141
  PA_SC_PERFCOUNTER1_LO            :TPA_SC_PERFCOUNTER1_LO;             // 0xD142
  PA_SC_PERFCOUNTER1_HI            :TPA_SC_PERFCOUNTER1_HI;             // 0xD143
  PA_SC_PERFCOUNTER2_LO            :TPA_SC_PERFCOUNTER2_LO;             // 0xD144
  PA_SC_PERFCOUNTER2_HI            :TPA_SC_PERFCOUNTER2_HI;             // 0xD145
  PA_SC_PERFCOUNTER3_LO            :TPA_SC_PERFCOUNTER3_LO;             // 0xD146
  PA_SC_PERFCOUNTER3_HI            :TPA_SC_PERFCOUNTER3_HI;             // 0xD147
  PA_SC_PERFCOUNTER4_LO            :TPA_SC_PERFCOUNTER4_LO;             // 0xD148
  PA_SC_PERFCOUNTER4_HI            :TPA_SC_PERFCOUNTER4_HI;             // 0xD149
  PA_SC_PERFCOUNTER5_LO            :TPA_SC_PERFCOUNTER5_LO;             // 0xD14A
  PA_SC_PERFCOUNTER5_HI            :TPA_SC_PERFCOUNTER5_HI;             // 0xD14B
  PA_SC_PERFCOUNTER6_LO            :TPA_SC_PERFCOUNTER6_LO;             // 0xD14C
  PA_SC_PERFCOUNTER6_HI            :TPA_SC_PERFCOUNTER6_HI;             // 0xD14D
  PA_SC_PERFCOUNTER7_LO            :TPA_SC_PERFCOUNTER7_LO;             // 0xD14E
  PA_SC_PERFCOUNTER7_HI            :TPA_SC_PERFCOUNTER7_HI;             // 0xD14F
  REG_D150_D17F                    :array[0..47] of DWORD;              // 0xD150
  SPI_PERFCOUNTER0_HI              :TSPI_PERFCOUNTER0_HI;               // 0xD180
  SPI_PERFCOUNTER0_LO              :TSPI_PERFCOUNTER0_LO;               // 0xD181
  SPI_PERFCOUNTER1_HI              :TSPI_PERFCOUNTER1_HI;               // 0xD182
  SPI_PERFCOUNTER1_LO              :TSPI_PERFCOUNTER1_LO;               // 0xD183
  SPI_PERFCOUNTER2_HI              :TSPI_PERFCOUNTER2_HI;               // 0xD184
  SPI_PERFCOUNTER2_LO              :TSPI_PERFCOUNTER2_LO;               // 0xD185
  SPI_PERFCOUNTER3_HI              :TSPI_PERFCOUNTER3_HI;               // 0xD186
  SPI_PERFCOUNTER3_LO              :TSPI_PERFCOUNTER3_LO;               // 0xD187
  SPI_PERFCOUNTER4_HI              :TSPI_PERFCOUNTER4_HI;               // 0xD188
  SPI_PERFCOUNTER4_LO              :TSPI_PERFCOUNTER4_LO;               // 0xD189
  SPI_PERFCOUNTER5_HI              :TSPI_PERFCOUNTER5_HI;               // 0xD18A
  SPI_PERFCOUNTER5_LO              :TSPI_PERFCOUNTER5_LO;               // 0xD18B
  REG_D18C_D1BF                    :array[0..51] of DWORD;              // 0xD18C
  SQ_PERFCOUNTER0_LO               :TSQ_PERFCOUNTER0_LO;                // 0xD1C0
  SQ_PERFCOUNTER0_HI               :TSQ_PERFCOUNTER0_HI;                // 0xD1C1
  SQ_PERFCOUNTER1_LO               :TSQ_PERFCOUNTER1_LO;                // 0xD1C2
  SQ_PERFCOUNTER1_HI               :TSQ_PERFCOUNTER1_HI;                // 0xD1C3
  SQ_PERFCOUNTER2_LO               :TSQ_PERFCOUNTER2_LO;                // 0xD1C4
  SQ_PERFCOUNTER2_HI               :TSQ_PERFCOUNTER2_HI;                // 0xD1C5
  SQ_PERFCOUNTER3_LO               :TSQ_PERFCOUNTER3_LO;                // 0xD1C6
  SQ_PERFCOUNTER3_HI               :TSQ_PERFCOUNTER3_HI;                // 0xD1C7
  SQ_PERFCOUNTER4_LO               :TSQ_PERFCOUNTER4_LO;                // 0xD1C8
  SQ_PERFCOUNTER4_HI               :TSQ_PERFCOUNTER4_HI;                // 0xD1C9
  SQ_PERFCOUNTER5_LO               :TSQ_PERFCOUNTER5_LO;                // 0xD1CA
  SQ_PERFCOUNTER5_HI               :TSQ_PERFCOUNTER5_HI;                // 0xD1CB
  SQ_PERFCOUNTER6_LO               :TSQ_PERFCOUNTER6_LO;                // 0xD1CC
  SQ_PERFCOUNTER6_HI               :TSQ_PERFCOUNTER6_HI;                // 0xD1CD
  SQ_PERFCOUNTER7_LO               :TSQ_PERFCOUNTER7_LO;                // 0xD1CE
  SQ_PERFCOUNTER7_HI               :TSQ_PERFCOUNTER7_HI;                // 0xD1CF
  SQ_PERFCOUNTER8_LO               :TSQ_PERFCOUNTER8_LO;                // 0xD1D0
  SQ_PERFCOUNTER8_HI               :TSQ_PERFCOUNTER8_HI;                // 0xD1D1
  SQ_PERFCOUNTER9_LO               :TSQ_PERFCOUNTER9_LO;                // 0xD1D2
  SQ_PERFCOUNTER9_HI               :TSQ_PERFCOUNTER9_HI;                // 0xD1D3
  SQ_PERFCOUNTER10_LO              :TSQ_PERFCOUNTER10_LO;               // 0xD1D4
  SQ_PERFCOUNTER10_HI              :TSQ_PERFCOUNTER10_HI;               // 0xD1D5
  SQ_PERFCOUNTER11_LO              :TSQ_PERFCOUNTER11_LO;               // 0xD1D6
  SQ_PERFCOUNTER11_HI              :TSQ_PERFCOUNTER11_HI;               // 0xD1D7
  SQ_PERFCOUNTER12_LO              :TSQ_PERFCOUNTER12_LO;               // 0xD1D8
  SQ_PERFCOUNTER12_HI              :TSQ_PERFCOUNTER12_HI;               // 0xD1D9
  SQ_PERFCOUNTER13_LO              :TSQ_PERFCOUNTER13_LO;               // 0xD1DA
  SQ_PERFCOUNTER13_HI              :TSQ_PERFCOUNTER13_HI;               // 0xD1DB
  SQ_PERFCOUNTER14_LO              :TSQ_PERFCOUNTER14_LO;               // 0xD1DC
  SQ_PERFCOUNTER14_HI              :TSQ_PERFCOUNTER14_HI;               // 0xD1DD
  SQ_PERFCOUNTER15_LO              :TSQ_PERFCOUNTER15_LO;               // 0xD1DE
  SQ_PERFCOUNTER15_HI              :TSQ_PERFCOUNTER15_HI;               // 0xD1DF
  REG_D1E0_D23F                    :array[0..95] of DWORD;              // 0xD1E0
  SX_PERFCOUNTER0_LO               :TSX_PERFCOUNTER0_LO;                // 0xD240
  SX_PERFCOUNTER0_HI               :TSX_PERFCOUNTER0_HI;                // 0xD241
  SX_PERFCOUNTER1_LO               :TSX_PERFCOUNTER1_LO;                // 0xD242
  SX_PERFCOUNTER1_HI               :TSX_PERFCOUNTER1_HI;                // 0xD243
  SX_PERFCOUNTER2_LO               :TSX_PERFCOUNTER2_LO;                // 0xD244
  SX_PERFCOUNTER2_HI               :TSX_PERFCOUNTER2_HI;                // 0xD245
  SX_PERFCOUNTER3_LO               :TSX_PERFCOUNTER3_LO;                // 0xD246
  SX_PERFCOUNTER3_HI               :TSX_PERFCOUNTER3_HI;                // 0xD247
  REG_D248_D27F                    :array[0..55] of DWORD;              // 0xD248
  GDS_PERFCOUNTER0_LO              :TGDS_PERFCOUNTER0_LO;               // 0xD280
  GDS_PERFCOUNTER0_HI              :TGDS_PERFCOUNTER0_HI;               // 0xD281
  GDS_PERFCOUNTER1_LO              :TGDS_PERFCOUNTER1_LO;               // 0xD282
  GDS_PERFCOUNTER1_HI              :TGDS_PERFCOUNTER1_HI;               // 0xD283
  GDS_PERFCOUNTER2_LO              :TGDS_PERFCOUNTER2_LO;               // 0xD284
  GDS_PERFCOUNTER2_HI              :TGDS_PERFCOUNTER2_HI;               // 0xD285
  GDS_PERFCOUNTER3_LO              :TGDS_PERFCOUNTER3_LO;               // 0xD286
  GDS_PERFCOUNTER3_HI              :TGDS_PERFCOUNTER3_HI;               // 0xD287
  REG_D288_D2BF                    :array[0..55] of DWORD;              // 0xD288
  TA_PERFCOUNTER0_LO               :TTA_PERFCOUNTER0_LO;                // 0xD2C0
  TA_PERFCOUNTER0_HI               :TTA_PERFCOUNTER0_HI;                // 0xD2C1
  TA_PERFCOUNTER1_LO               :TTA_PERFCOUNTER1_LO;                // 0xD2C2
  TA_PERFCOUNTER1_HI               :TTA_PERFCOUNTER1_HI;                // 0xD2C3
  REG_D2C4_D2FF                    :array[0..59] of DWORD;              // 0xD2C4
  TD_PERFCOUNTER0_LO               :TTD_PERFCOUNTER0_LO;                // 0xD300
  TD_PERFCOUNTER0_HI               :TTD_PERFCOUNTER0_HI;                // 0xD301
  TD_PERFCOUNTER1_LO               :TTD_PERFCOUNTER1_LO;                // 0xD302
  TD_PERFCOUNTER1_HI               :TTD_PERFCOUNTER1_HI;                // 0xD303
  REG_D304_D33F                    :array[0..59] of DWORD;              // 0xD304
  TCP_PERFCOUNTER0_LO              :TTCP_PERFCOUNTER0_LO;               // 0xD340
  TCP_PERFCOUNTER0_HI              :TTCP_PERFCOUNTER0_HI;               // 0xD341
  TCP_PERFCOUNTER1_LO              :TTCP_PERFCOUNTER1_LO;               // 0xD342
  TCP_PERFCOUNTER1_HI              :TTCP_PERFCOUNTER1_HI;               // 0xD343
  TCP_PERFCOUNTER2_LO              :TTCP_PERFCOUNTER2_LO;               // 0xD344
  TCP_PERFCOUNTER2_HI              :TTCP_PERFCOUNTER2_HI;               // 0xD345
  TCP_PERFCOUNTER3_LO              :TTCP_PERFCOUNTER3_LO;               // 0xD346
  TCP_PERFCOUNTER3_HI              :TTCP_PERFCOUNTER3_HI;               // 0xD347
  REG_D348_D37F                    :array[0..55] of DWORD;              // 0xD348
  TCC_PERFCOUNTER0_LO              :TTCC_PERFCOUNTER0_LO;               // 0xD380
  TCC_PERFCOUNTER0_HI              :TTCC_PERFCOUNTER0_HI;               // 0xD381
  TCC_PERFCOUNTER1_LO              :TTCC_PERFCOUNTER1_LO;               // 0xD382
  TCC_PERFCOUNTER1_HI              :TTCC_PERFCOUNTER1_HI;               // 0xD383
  TCC_PERFCOUNTER2_LO              :TTCC_PERFCOUNTER2_LO;               // 0xD384
  TCC_PERFCOUNTER2_HI              :TTCC_PERFCOUNTER2_HI;               // 0xD385
  TCC_PERFCOUNTER3_LO              :TTCC_PERFCOUNTER3_LO;               // 0xD386
  TCC_PERFCOUNTER3_HI              :TTCC_PERFCOUNTER3_HI;               // 0xD387
  REG_D388_D38F                    :array[0..7] of DWORD;               // 0xD388
  TCA_PERFCOUNTER0_LO              :TTCA_PERFCOUNTER0_LO;               // 0xD390
  TCA_PERFCOUNTER0_HI              :TTCA_PERFCOUNTER0_HI;               // 0xD391
  TCA_PERFCOUNTER1_LO              :TTCA_PERFCOUNTER1_LO;               // 0xD392
  TCA_PERFCOUNTER1_HI              :TTCA_PERFCOUNTER1_HI;               // 0xD393
  TCA_PERFCOUNTER2_LO              :TTCA_PERFCOUNTER2_LO;               // 0xD394
  TCA_PERFCOUNTER2_HI              :TTCA_PERFCOUNTER2_HI;               // 0xD395
  TCA_PERFCOUNTER3_LO              :TTCA_PERFCOUNTER3_LO;               // 0xD396
  TCA_PERFCOUNTER3_HI              :TTCA_PERFCOUNTER3_HI;               // 0xD397
  REG_D398_D405                    :array[0..109] of DWORD;             // 0xD398
  CB_PERFCOUNTER0_LO               :TCB_PERFCOUNTER0_LO;                // 0xD406
  CB_PERFCOUNTER0_HI               :TCB_PERFCOUNTER0_HI;                // 0xD407
  CB_PERFCOUNTER1_LO               :TCB_PERFCOUNTER1_LO;                // 0xD408
  CB_PERFCOUNTER1_HI               :TCB_PERFCOUNTER1_HI;                // 0xD409
  CB_PERFCOUNTER2_LO               :TCB_PERFCOUNTER2_LO;                // 0xD40A
  CB_PERFCOUNTER2_HI               :TCB_PERFCOUNTER2_HI;                // 0xD40B
  CB_PERFCOUNTER3_LO               :TCB_PERFCOUNTER3_LO;                // 0xD40C
  CB_PERFCOUNTER3_HI               :TCB_PERFCOUNTER3_HI;                // 0xD40D
  REG_D40E_D43F                    :array[0..49] of DWORD;              // 0xD40E
  DB_PERFCOUNTER0_LO               :TDB_PERFCOUNTER0_LO;                // 0xD440
  DB_PERFCOUNTER0_HI               :TDB_PERFCOUNTER0_HI;                // 0xD441
  DB_PERFCOUNTER1_LO               :TDB_PERFCOUNTER1_LO;                // 0xD442
  DB_PERFCOUNTER1_HI               :TDB_PERFCOUNTER1_HI;                // 0xD443
  DB_PERFCOUNTER2_LO               :TDB_PERFCOUNTER2_LO;                // 0xD444
  DB_PERFCOUNTER2_HI               :TDB_PERFCOUNTER2_HI;                // 0xD445
  DB_PERFCOUNTER3_LO               :TDB_PERFCOUNTER3_LO;                // 0xD446
  DB_PERFCOUNTER3_HI               :TDB_PERFCOUNTER3_HI;                // 0xD447
  REG_D448_D7FF                    :array[0..951] of DWORD;             // 0xD448
  CPG_PERFCOUNTER1_SELECT          :TCPG_PERFCOUNTER1_SELECT;           // 0xD800
  CPG_PERFCOUNTER0_SELECT1         :TCPG_PERFCOUNTER0_SELECT1;          // 0xD801
  CPG_PERFCOUNTER0_SELECT          :TCPG_PERFCOUNTER0_SELECT;           // 0xD802
  CPC_PERFCOUNTER1_SELECT          :TCPC_PERFCOUNTER1_SELECT;           // 0xD803
  CPC_PERFCOUNTER0_SELECT1         :TCPC_PERFCOUNTER0_SELECT1;          // 0xD804
  CPF_PERFCOUNTER1_SELECT          :TCPF_PERFCOUNTER1_SELECT;           // 0xD805
  CPF_PERFCOUNTER0_SELECT1         :TCPF_PERFCOUNTER0_SELECT1;          // 0xD806
  CPF_PERFCOUNTER0_SELECT          :TCPF_PERFCOUNTER0_SELECT;           // 0xD807
  CP_PERFMON_CNTL                  :TCP_PERFMON_CNTL;                   // 0xD808
  CPC_PERFCOUNTER0_SELECT          :TCPC_PERFCOUNTER0_SELECT;           // 0xD809
  REG_D80A_D80F                    :array[0..5] of DWORD;               // 0xD80A
  CP_DRAW_OBJECT                   :TCP_DRAW_OBJECT;                    // 0xD810
  CP_DRAW_OBJECT_COUNTER           :TCP_DRAW_OBJECT_COUNTER;            // 0xD811
  CP_DRAW_WINDOW_MASK_HI           :TCP_DRAW_WINDOW_MASK_HI;            // 0xD812
  CP_DRAW_WINDOW_HI                :TCP_DRAW_WINDOW_HI;                 // 0xD813
  CP_DRAW_WINDOW_LO                :TCP_DRAW_WINDOW_LO;                 // 0xD814
  CP_DRAW_WINDOW_CNTL              :TCP_DRAW_WINDOW_CNTL;               // 0xD815
  REG_D816_D83F                    :array[0..41] of DWORD;              // 0xD816
  GRBM_PERFCOUNTER0_SELECT         :TGRBM_PERFCOUNTER0_SELECT;          // 0xD840
  GRBM_PERFCOUNTER1_SELECT         :TGRBM_PERFCOUNTER1_SELECT;          // 0xD841
  GRBM_SE0_PERFCOUNTER_SELECT      :TGRBM_SE0_PERFCOUNTER_SELECT;       // 0xD842
  GRBM_SE1_PERFCOUNTER_SELECT      :TGRBM_SE1_PERFCOUNTER_SELECT;       // 0xD843
  GRBM_SE2_PERFCOUNTER_SELECT      :TGRBM_SE2_PERFCOUNTER_SELECT;       // 0xD844
  GRBM_SE3_PERFCOUNTER_SELECT      :TGRBM_SE3_PERFCOUNTER_SELECT;       // 0xD845
  REG_D846_D87F                    :array[0..57] of DWORD;              // 0xD846
  WD_PERFCOUNTER0_SELECT           :TWD_PERFCOUNTER0_SELECT;            // 0xD880
  WD_PERFCOUNTER1_SELECT           :TWD_PERFCOUNTER1_SELECT;            // 0xD881
  WD_PERFCOUNTER2_SELECT           :TWD_PERFCOUNTER2_SELECT;            // 0xD882
  WD_PERFCOUNTER3_SELECT           :TWD_PERFCOUNTER3_SELECT;            // 0xD883
  IA_PERFCOUNTER0_SELECT           :TIA_PERFCOUNTER0_SELECT;            // 0xD884
  IA_PERFCOUNTER1_SELECT           :TIA_PERFCOUNTER1_SELECT;            // 0xD885
  IA_PERFCOUNTER2_SELECT           :TIA_PERFCOUNTER2_SELECT;            // 0xD886
  IA_PERFCOUNTER3_SELECT           :TIA_PERFCOUNTER3_SELECT;            // 0xD887
  IA_PERFCOUNTER0_SELECT1          :TIA_PERFCOUNTER0_SELECT1;           // 0xD888
  REG_D889_D88B                    :array[0..2] of DWORD;               // 0xD889
  VGT_PERFCOUNTER0_SELECT          :TVGT_PERFCOUNTER0_SELECT;           // 0xD88C
  VGT_PERFCOUNTER1_SELECT          :TVGT_PERFCOUNTER1_SELECT;           // 0xD88D
  VGT_PERFCOUNTER2_SELECT          :TVGT_PERFCOUNTER2_SELECT;           // 0xD88E
  VGT_PERFCOUNTER3_SELECT          :TVGT_PERFCOUNTER3_SELECT;           // 0xD88F
  VGT_PERFCOUNTER0_SELECT1         :TVGT_PERFCOUNTER0_SELECT1;          // 0xD890
  VGT_PERFCOUNTER1_SELECT1         :TVGT_PERFCOUNTER1_SELECT1;          // 0xD891
  REG_D892_D893                    :array[0..1] of DWORD;               // 0xD892
  VGT_PERFCOUNTER_SEID_MASK        :TVGT_PERFCOUNTER_SEID_MASK;         // 0xD894
  REG_D895_D8FF                    :array[0..106] of DWORD;             // 0xD895
  PA_SU_PERFCOUNTER0_SELECT        :TPA_SU_PERFCOUNTER0_SELECT;         // 0xD900
  PA_SU_PERFCOUNTER0_SELECT1       :TPA_SU_PERFCOUNTER0_SELECT1;        // 0xD901
  PA_SU_PERFCOUNTER1_SELECT        :TPA_SU_PERFCOUNTER1_SELECT;         // 0xD902
  PA_SU_PERFCOUNTER1_SELECT1       :TPA_SU_PERFCOUNTER1_SELECT1;        // 0xD903
  PA_SU_PERFCOUNTER2_SELECT        :TPA_SU_PERFCOUNTER2_SELECT;         // 0xD904
  PA_SU_PERFCOUNTER3_SELECT        :TPA_SU_PERFCOUNTER3_SELECT;         // 0xD905
  REG_D906_D93F                    :array[0..57] of DWORD;              // 0xD906
  PA_SC_PERFCOUNTER0_SELECT        :TPA_SC_PERFCOUNTER0_SELECT;         // 0xD940
  PA_SC_PERFCOUNTER0_SELECT1       :TPA_SC_PERFCOUNTER0_SELECT1;        // 0xD941
  PA_SC_PERFCOUNTER1_SELECT        :TPA_SC_PERFCOUNTER1_SELECT;         // 0xD942
  PA_SC_PERFCOUNTER2_SELECT        :TPA_SC_PERFCOUNTER2_SELECT;         // 0xD943
  PA_SC_PERFCOUNTER3_SELECT        :TPA_SC_PERFCOUNTER3_SELECT;         // 0xD944
  PA_SC_PERFCOUNTER4_SELECT        :TPA_SC_PERFCOUNTER4_SELECT;         // 0xD945
  PA_SC_PERFCOUNTER5_SELECT        :TPA_SC_PERFCOUNTER5_SELECT;         // 0xD946
  PA_SC_PERFCOUNTER6_SELECT        :TPA_SC_PERFCOUNTER6_SELECT;         // 0xD947
  PA_SC_PERFCOUNTER7_SELECT        :TPA_SC_PERFCOUNTER7_SELECT;         // 0xD948
  REG_D949_D97F                    :array[0..54] of DWORD;              // 0xD949
  SPI_PERFCOUNTER0_SELECT          :TSPI_PERFCOUNTER0_SELECT;           // 0xD980
  SPI_PERFCOUNTER1_SELECT          :TSPI_PERFCOUNTER1_SELECT;           // 0xD981
  SPI_PERFCOUNTER2_SELECT          :TSPI_PERFCOUNTER2_SELECT;           // 0xD982
  SPI_PERFCOUNTER3_SELECT          :TSPI_PERFCOUNTER3_SELECT;           // 0xD983
  SPI_PERFCOUNTER0_SELECT1         :TSPI_PERFCOUNTER0_SELECT1;          // 0xD984
  SPI_PERFCOUNTER1_SELECT1         :TSPI_PERFCOUNTER1_SELECT1;          // 0xD985
  SPI_PERFCOUNTER2_SELECT1         :TSPI_PERFCOUNTER2_SELECT1;          // 0xD986
  SPI_PERFCOUNTER3_SELECT1         :TSPI_PERFCOUNTER3_SELECT1;          // 0xD987
  SPI_PERFCOUNTER4_SELECT          :TSPI_PERFCOUNTER4_SELECT;           // 0xD988
  SPI_PERFCOUNTER5_SELECT          :TSPI_PERFCOUNTER5_SELECT;           // 0xD989
  SPI_PERFCOUNTER_BINS             :TSPI_PERFCOUNTER_BINS;              // 0xD98A
  REG_D98B_D9BF                    :array[0..52] of DWORD;              // 0xD98B
  SQ_PERFCOUNTER0_SELECT           :TSQ_PERFCOUNTER0_SELECT;            // 0xD9C0
  SQ_PERFCOUNTER1_SELECT           :TSQ_PERFCOUNTER1_SELECT;            // 0xD9C1
  SQ_PERFCOUNTER2_SELECT           :TSQ_PERFCOUNTER2_SELECT;            // 0xD9C2
  SQ_PERFCOUNTER3_SELECT           :TSQ_PERFCOUNTER3_SELECT;            // 0xD9C3
  SQ_PERFCOUNTER4_SELECT           :TSQ_PERFCOUNTER4_SELECT;            // 0xD9C4
  SQ_PERFCOUNTER5_SELECT           :TSQ_PERFCOUNTER5_SELECT;            // 0xD9C5
  SQ_PERFCOUNTER6_SELECT           :TSQ_PERFCOUNTER6_SELECT;            // 0xD9C6
  SQ_PERFCOUNTER7_SELECT           :TSQ_PERFCOUNTER7_SELECT;            // 0xD9C7
  SQ_PERFCOUNTER8_SELECT           :TSQ_PERFCOUNTER8_SELECT;            // 0xD9C8
  SQ_PERFCOUNTER9_SELECT           :TSQ_PERFCOUNTER9_SELECT;            // 0xD9C9
  SQ_PERFCOUNTER10_SELECT          :TSQ_PERFCOUNTER10_SELECT;           // 0xD9CA
  SQ_PERFCOUNTER11_SELECT          :TSQ_PERFCOUNTER11_SELECT;           // 0xD9CB
  SQ_PERFCOUNTER12_SELECT          :TSQ_PERFCOUNTER12_SELECT;           // 0xD9CC
  SQ_PERFCOUNTER13_SELECT          :TSQ_PERFCOUNTER13_SELECT;           // 0xD9CD
  SQ_PERFCOUNTER14_SELECT          :TSQ_PERFCOUNTER14_SELECT;           // 0xD9CE
  SQ_PERFCOUNTER15_SELECT          :TSQ_PERFCOUNTER15_SELECT;           // 0xD9CF
  REG_D9D0_D9DF                    :array[0..15] of DWORD;              // 0xD9D0
  SQ_PERFCOUNTER_CTRL              :TSQ_PERFCOUNTER_CTRL;               // 0xD9E0
  SQ_PERFCOUNTER_MASK              :TSQ_PERFCOUNTER_MASK;               // 0xD9E1
  SQ_PERFCOUNTER_CTRL2             :TSQ_PERFCOUNTER_CTRL2;              // 0xD9E2
  REG_D9E3_DA3F                    :array[0..92] of DWORD;              // 0xD9E3
  SX_PERFCOUNTER0_SELECT           :TSX_PERFCOUNTER0_SELECT;            // 0xDA40
  SX_PERFCOUNTER1_SELECT           :TSX_PERFCOUNTER1_SELECT;            // 0xDA41
  SX_PERFCOUNTER2_SELECT           :TSX_PERFCOUNTER2_SELECT;            // 0xDA42
  SX_PERFCOUNTER3_SELECT           :TSX_PERFCOUNTER3_SELECT;            // 0xDA43
  SX_PERFCOUNTER0_SELECT1          :TSX_PERFCOUNTER0_SELECT1;           // 0xDA44
  SX_PERFCOUNTER1_SELECT1          :TSX_PERFCOUNTER1_SELECT1;           // 0xDA45
  REG_DA46_DA7F                    :array[0..57] of DWORD;              // 0xDA46
  GDS_PERFCOUNTER0_SELECT          :TGDS_PERFCOUNTER0_SELECT;           // 0xDA80
  GDS_PERFCOUNTER1_SELECT          :TGDS_PERFCOUNTER1_SELECT;           // 0xDA81
  GDS_PERFCOUNTER2_SELECT          :TGDS_PERFCOUNTER2_SELECT;           // 0xDA82
  GDS_PERFCOUNTER3_SELECT          :TGDS_PERFCOUNTER3_SELECT;           // 0xDA83
  GDS_PERFCOUNTER0_SELECT1         :TGDS_PERFCOUNTER0_SELECT1;          // 0xDA84
  REG_DA85_DABF                    :array[0..58] of DWORD;              // 0xDA85
  TA_PERFCOUNTER0_SELECT           :TTA_PERFCOUNTER0_SELECT;            // 0xDAC0
  TA_PERFCOUNTER0_SELECT1          :TTA_PERFCOUNTER0_SELECT1;           // 0xDAC1
  TA_PERFCOUNTER1_SELECT           :TTA_PERFCOUNTER1_SELECT;            // 0xDAC2
  REG_DAC3_DAFF                    :array[0..60] of DWORD;              // 0xDAC3
  TD_PERFCOUNTER0_SELECT           :TTD_PERFCOUNTER0_SELECT;            // 0xDB00
  TD_PERFCOUNTER0_SELECT1          :TTD_PERFCOUNTER0_SELECT1;           // 0xDB01
  TD_PERFCOUNTER1_SELECT           :TTD_PERFCOUNTER1_SELECT;            // 0xDB02
  REG_DB03_DB3F                    :array[0..60] of DWORD;              // 0xDB03
  TCP_PERFCOUNTER0_SELECT          :TTCP_PERFCOUNTER0_SELECT;           // 0xDB40
  TCP_PERFCOUNTER0_SELECT1         :TTCP_PERFCOUNTER0_SELECT1;          // 0xDB41
  TCP_PERFCOUNTER1_SELECT          :TTCP_PERFCOUNTER1_SELECT;           // 0xDB42
  TCP_PERFCOUNTER1_SELECT1         :TTCP_PERFCOUNTER1_SELECT1;          // 0xDB43
  TCP_PERFCOUNTER2_SELECT          :TTCP_PERFCOUNTER2_SELECT;           // 0xDB44
  TCP_PERFCOUNTER3_SELECT          :TTCP_PERFCOUNTER3_SELECT;           // 0xDB45
  REG_DB46_DB7F                    :array[0..57] of DWORD;              // 0xDB46
  TCC_PERFCOUNTER0_SELECT          :TTCC_PERFCOUNTER0_SELECT;           // 0xDB80
  TCC_PERFCOUNTER0_SELECT1         :TTCC_PERFCOUNTER0_SELECT1;          // 0xDB81
  TCC_PERFCOUNTER1_SELECT          :TTCC_PERFCOUNTER1_SELECT;           // 0xDB82
  TCC_PERFCOUNTER1_SELECT1         :TTCC_PERFCOUNTER1_SELECT1;          // 0xDB83
  TCC_PERFCOUNTER2_SELECT          :TTCC_PERFCOUNTER2_SELECT;           // 0xDB84
  TCC_PERFCOUNTER3_SELECT          :TTCC_PERFCOUNTER3_SELECT;           // 0xDB85
  REG_DB86_DB8F                    :array[0..9] of DWORD;               // 0xDB86
  TCA_PERFCOUNTER0_SELECT          :TTCA_PERFCOUNTER0_SELECT;           // 0xDB90
  TCA_PERFCOUNTER0_SELECT1         :TTCA_PERFCOUNTER0_SELECT1;          // 0xDB91
  TCA_PERFCOUNTER1_SELECT          :TTCA_PERFCOUNTER1_SELECT;           // 0xDB92
  TCA_PERFCOUNTER1_SELECT1         :TTCA_PERFCOUNTER1_SELECT1;          // 0xDB93
  TCA_PERFCOUNTER2_SELECT          :TTCA_PERFCOUNTER2_SELECT;           // 0xDB94
  TCA_PERFCOUNTER3_SELECT          :TTCA_PERFCOUNTER3_SELECT;           // 0xDB95
  REG_DB96_DBFF                    :array[0..105] of DWORD;             // 0xDB96
  CB_PERFCOUNTER_FILTER            :TCB_PERFCOUNTER_FILTER;             // 0xDC00
  CB_PERFCOUNTER0_SELECT           :TCB_PERFCOUNTER0_SELECT;            // 0xDC01
  CB_PERFCOUNTER0_SELECT1          :TCB_PERFCOUNTER0_SELECT1;           // 0xDC02
  CB_PERFCOUNTER1_SELECT           :TCB_PERFCOUNTER1_SELECT;            // 0xDC03
  CB_PERFCOUNTER2_SELECT           :TCB_PERFCOUNTER2_SELECT;            // 0xDC04
  CB_PERFCOUNTER3_SELECT           :TCB_PERFCOUNTER3_SELECT;            // 0xDC05
  REG_DC06_DC3F                    :array[0..57] of DWORD;              // 0xDC06
  DB_PERFCOUNTER0_SELECT           :TDB_PERFCOUNTER0_SELECT;            // 0xDC40
  DB_PERFCOUNTER0_SELECT1          :TDB_PERFCOUNTER0_SELECT1;           // 0xDC41
  DB_PERFCOUNTER1_SELECT           :TDB_PERFCOUNTER1_SELECT;            // 0xDC42
  DB_PERFCOUNTER1_SELECT1          :TDB_PERFCOUNTER1_SELECT1;           // 0xDC43
  DB_PERFCOUNTER2_SELECT           :TDB_PERFCOUNTER2_SELECT;            // 0xDC44
  REG_DC45                         :DWORD;                              // 0xDC45
  DB_PERFCOUNTER3_SELECT           :TDB_PERFCOUNTER3_SELECT;            // 0xDC46
 end;


implementation

end.

