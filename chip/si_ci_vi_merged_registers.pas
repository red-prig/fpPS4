unit si_ci_vi_merged_registers;

{$mode objfpc}{$H+}

interface

uses
 bittype;

type
 TWD_QOS=bitpacked record
  DRAW_STALL:bit1;
  RESERVED0 :bit31;
 end;

 TCP_STAT=bitpacked record
  RESERVED0            :bit9;
  ROQ_RING_BUSY        :bit1;
  ROQ_INDIRECT1_BUSY   :bit1;
  ROQ_INDIRECT2_BUSY   :bit1;
  ROQ_STATE_BUSY       :bit1;
  DC_BUSY              :bit1;
  ATCL2IU_BUSY         :bit1;
  PFP_BUSY             :bit1;
  MEQ_BUSY             :bit1;
  ME_BUSY              :bit1;
  QUERY_BUSY           :bit1;
  SEMAPHORE_BUSY       :bit1;
  INTERRUPT_BUSY       :bit1;
  SURFACE_SYNC_BUSY    :bit1;
  DMA_BUSY             :bit1;
  RCIU_BUSY            :bit1;
  SCRATCH_RAM_BUSY     :bit1;
  CPC_CPG_BUSY         :bit1;
  CE_BUSY              :bit1;
  TCIU_BUSY            :bit1;
  ROQ_CE_RING_BUSY     :bit1;
  ROQ_CE_INDIRECT1_BUSY:bit1;
  ROQ_CE_INDIRECT2_BUSY:bit1;
  CP_BUSY              :bit1;
 end;

 TCP_VMID=bitpacked record
  VMID     :bit4;
  RESERVED0:bit28;
 end;

 TDB_EQAA=bitpacked record
  MAX_ANCHOR_SAMPLES            :bit3;
  RESERVED0                     :bit1;
  PS_ITER_SAMPLES               :bit3;
  RESERVED1                     :bit1;
  MASK_EXPORT_NUM_SAMPLES       :bit3;
  RESERVED2                     :bit1;
  ALPHA_TO_MASK_NUM_SAMPLES     :bit3;
  RESERVED3                     :bit1;
  HIGH_QUALITY_INTERSECTIONS    :bit1;
  INCOHERENT_EQAA_READS         :bit1;
  INTERPOLATE_COMP_Z            :bit1;
  INTERPOLATE_SRC_Z             :bit1;
  STATIC_ANCHOR_ASSOCIATIONS    :bit1;
  ALPHA_TO_MASK_EQAA_DISABLE    :bit1;
  RESERVED4                     :bit2;
  OVERRASTERIZATION_AMOUNT      :bit3;
  ENABLE_POSTZ_OVERRASTERIZATION:bit1;
  RESERVED5                     :bit4;
 end;

 TTA_CNTL=bitpacked record
  FX_XNACK_CREDIT:bit7;
  RESERVED0      :bit2;
  SQ_XNACK_CREDIT:bit4;
  TC_DATA_CREDIT :bit3;
  ALIGNER_CREDIT :bit5;
  RESERVED1      :bit1;
  TD_FIFO_CREDIT :bit10;
 end;

 TTD_CNTL=bitpacked record
  SYNC_PHASE_SH             :bit2;
  RESERVED0                 :bit2;
  SYNC_PHASE_VC_SMX         :bit2;
  RESERVED1                 :bit2;
  PAD_STALL_EN              :bit1;
  EXTEND_LDS_STALL          :bit2;
  LDS_STALL_PHASE_ADJUST    :bit2;
  RESERVED2                 :bit2;
  PRECISION_COMPATIBILITY   :bit1;
  GATHER4_FLOAT_MODE        :bit1;
  RESERVED3                 :bit1;
  LD_FLOAT_MODE             :bit1;
  GATHER4_DX9_MODE          :bit1;
  DISABLE_POWER_THROTTLE    :bit1;
  ENABLE_ROUND_TO_ZERO      :bit1;
  DISABLE_D16_PACKING       :bit1;
  DISABLE_2BIT_SIGNED_FORMAT:bit1;
  RESERVED4                 :bit8;
 end;

 TCP_DEBUG=bitpacked record
  RESERVED0 :bit6;
  RESERVED1 :bit1;
  RESERVED2 :bit8;
  RESERVED3 :bit1;
  RESERVED4 :bit3;
  RESERVED5 :bit2;
  RESERVED6 :bit1;
  RESERVED7 :bit1;
  RESERVED8 :bit1;
  RESERVED9 :bit1;
  RESERVED10:bit1;
  RESERVED11:bit1;
  RESERVED12:bit2;
  RESERVED13:bit1;
  RESERVED14:bit1;
  RESERVED15:bit1;
 end;

 TDB_DEBUG=bitpacked record
  DEBUG_STENCIL_COMPRESS_DISABLE       :bit1;
  DEBUG_DEPTH_COMPRESS_DISABLE         :bit1;
  FETCH_FULL_Z_TILE                    :bit1;
  FETCH_FULL_STENCIL_TILE              :bit1;
  FORCE_Z_MODE                         :bit2;
  DEBUG_FORCE_DEPTH_READ               :bit1;
  DEBUG_FORCE_STENCIL_READ             :bit1;
  DEBUG_FORCE_HIZ_ENABLE               :bit2;
  DEBUG_FORCE_HIS_ENABLE0              :bit2;
  DEBUG_FORCE_HIS_ENABLE1              :bit2;
  DEBUG_FAST_Z_DISABLE                 :bit1;
  DEBUG_FAST_STENCIL_DISABLE           :bit1;
  DEBUG_NOOP_CULL_DISABLE              :bit1;
  DISABLE_SUMM_SQUADS                  :bit1;
  DEPTH_CACHE_FORCE_MISS               :bit1;
  DEBUG_FORCE_FULL_Z_RANGE             :bit2;
  NEVER_FREE_Z_ONLY                    :bit1;
  ZPASS_COUNTS_LOOK_AT_PIPE_STAT_EVENTS:bit1;
  DISABLE_VPORT_ZPLANE_OPTIMIZATION    :bit1;
  DECOMPRESS_AFTER_N_ZPLANES           :bit4;
  ONE_FREE_IN_FLIGHT                   :bit1;
  FORCE_MISS_IF_NOT_INFLIGHT           :bit1;
  DISABLE_DEPTH_SURFACE_SYNC           :bit1;
  DISABLE_HTILE_SURFACE_SYNC           :bit1;
 end;

 TTCA_CTRL=bitpacked record
  HOLE_TIMEOUT:bit4;
  RESERVED0   :bit28;
 end;

 TTCC_CTRL=bitpacked record
  CACHE_SIZE                :bit2;
  RATE                      :bit2;
  WRITEBACK_MARGIN          :bit4;
  METADATA_LATENCY_FIFO_SIZE:bit4;
  SRC_FIFO_SIZE             :bit4;
  LATENCY_FIFO_SIZE         :bit4;
  WB_OR_INV_ALL_VMIDS       :bit1;
  RESERVED0                 :bit3;
  MDC_SIZE                  :bit2;
  MDC_SECTOR_SIZE           :bit2;
  MDC_SIDEBAND_FIFO_SIZE    :bit4;
 end;

 TTCP_CNTL=bitpacked record
  FORCE_HIT             :bit1;
  FORCE_MISS            :bit1;
  L1_SIZE               :bit2;
  FLAT_BUF_HASH_ENABLE  :bit1;
  FLAT_BUF_CACHE_SWIZZLE:bit1;
  RESERVED0             :bit9;
  FORCE_EOW_TOTAL_CNT   :bit6;
  RESERVED1             :bit1;
  FORCE_EOW_TAGRAM_CNT  :bit6;
  DISABLE_Z_MAP         :bit1;
  INV_ALL_VMIDS         :bit1;
  RESERVED2             :bit2;
 end;

 TCP_PIPEID=bitpacked record
  PIPE_ID  :bit2;
  RESERVED0:bit30;
 end;

 TDB_DEBUG2=bitpacked record
  ALLOW_COMPZ_BYTE_MASKING            :bit1;
  DISABLE_TC_ZRANGE_L0_CACHE          :bit1;
  DISABLE_TC_MASK_L0_CACHE            :bit1;
  DTR_ROUND_ROBIN_ARB                 :bit1;
  DTR_PREZ_STALLS_FOR_ETF_ROOM        :bit1;
  DISABLE_PREZL_LPF_STALL             :bit1;
  ENABLE_PREZL_CB_STALL               :bit1;
  DISABLE_PREZL_LPF_STALL_REZ         :bit1;
  DISABLE_PREZL_CB_STALL_REZ          :bit1;
  CLK_OFF_DELAY                       :bit5;
  DISABLE_TILE_COVERED_FOR_PS_ITER    :bit1;
  ENABLE_SUBTILE_GROUPING             :bit1;
  DISABLE_HTILE_PAIRED_PIPES          :bit1;
  DISABLE_NULL_EOT_FORWARDING         :bit1;
  DISABLE_DTT_DATA_FORWARDING         :bit1;
  DISABLE_QUAD_COHERENCY_STALL        :bit1;
  RESERVED0                           :bit8;
  ENABLE_PREZ_OF_REZ_SUMM             :bit1;
  DISABLE_PREZL_VIEWPORT_STALL        :bit1;
  DISABLE_SINGLE_STENCIL_QUAD_SUMM    :bit1;
  DISABLE_WRITE_STALL_ON_RDWR_CONFLICT:bit1;
 end;

 TDB_DEBUG3=bitpacked record
  RESERVED0                               :bit2;
  FORCE_DB_IS_GOOD                        :bit1;
  DISABLE_TL_SSO_NULL_SUPPRESSION         :bit1;
  DISABLE_HIZ_ON_VPORT_CLAMP              :bit1;
  EQAA_INTERPOLATE_COMP_Z                 :bit1;
  EQAA_INTERPOLATE_SRC_Z                  :bit1;
  DISABLE_TCP_CAM_BYPASS                  :bit1;
  DISABLE_ZCMP_DIRTY_SUPPRESSION          :bit1;
  DISABLE_REDUNDANT_PLANE_FLUSHES_OPT     :bit1;
  DISABLE_RECOMP_TO_1ZPLANE_WITHOUT_FASTOP:bit1;
  ENABLE_INCOHERENT_EQAA_READS            :bit1;
  DISABLE_OP_Z_DATA_FORWARDING            :bit1;
  DISABLE_OP_DF_BYPASS                    :bit1;
  DISABLE_OP_DF_WRITE_COMBINE             :bit1;
  DISABLE_OP_DF_DIRECT_FEEDBACK           :bit1;
  ALLOW_RF2P_RW_COLLISION                 :bit1;
  SLOW_PREZ_TO_A2M_OMASK_RATE             :bit1;
  DISABLE_OP_S_DATA_FORWARDING            :bit1;
  DISABLE_TC_UPDATE_WRITE_COMBINE         :bit1;
  DISABLE_HZ_TC_WRITE_COMBINE             :bit1;
  ENABLE_RECOMP_ZDIRTY_SUPPRESSION_OPT    :bit1;
  ENABLE_TC_MA_ROUND_ROBIN_ARB            :bit1;
  DISABLE_RAM_READ_SUPPRESION_ON_FWD      :bit1;
  DISABLE_EQAA_A2M_PERF_OPT               :bit1;
  DISABLE_DI_DT_STALL                     :bit1;
  ENABLE_DB_PROCESS_RESET                 :bit1;
  DISABLE_OVERRASTERIZATION_FIX           :bit1;
  DONT_INSERT_CONTEXT_SUSPEND             :bit1;
  DONT_DELETE_CONTEXT_SUSPEND             :bit1;
  DISABLE_4XAA_2P_DELAYED_WRITE           :bit1;
  DISABLE_4XAA_2P_INTERLEAVED_PMASK       :bit1;
 end;

 TDB_DEBUG4=bitpacked record
  DISABLE_QC_Z_MASK_SUMMATION            :bit1;
  DISABLE_QC_STENCIL_MASK_SUMMATION      :bit1;
  DISABLE_RESUMM_TO_SINGLE_STENCIL       :bit1;
  DISABLE_PREZ_POSTZ_DTILE_CONFLICT_STALL:bit1;
  DISABLE_4XAA_2P_ZD_HOLDOFF             :bit1;
  ENABLE_A2M_DQUAD_OPTIMIZATION          :bit1;
  DB_EXTRA_DEBUG4                        :bit26;
 end;

 TDB_Z_INFO=bitpacked record
  FORMAT                 :bit2;
  NUM_SAMPLES            :bit2;
  RESERVED0              :bit9;
  TILE_SPLIT             :bit3;
  RESERVED1              :bit4;
  TILE_MODE_INDEX        :bit3;
  DECOMPRESS_ON_N_ZPLANES:bit4;
  ALLOW_EXPCLEAR         :bit1;
  READ_SIZE              :bit1;
  TILE_SURFACE_ENABLE    :bit1;
  CLEAR_DISALLOWED       :bit1;
  ZRANGE_PRECISION       :bit1;
 end;

 TGRBM_CNTL=bitpacked record
  READ_TIMEOUT     :bit8;
  RESERVED0        :bit23;
  REPORT_LAST_RDERR:bit1;
 end;

 TTA_STATUS=bitpacked record
  RESERVED0      :bit12;
  FG_PFIFO_EMPTYB:bit1;
  FG_LFIFO_EMPTYB:bit1;
  FG_SFIFO_EMPTYB:bit1;
  RESERVED1      :bit1;
  FL_PFIFO_EMPTYB:bit1;
  FL_LFIFO_EMPTYB:bit1;
  FL_SFIFO_EMPTYB:bit1;
  RESERVED2      :bit1;
  FA_PFIFO_EMPTYB:bit1;
  FA_LFIFO_EMPTYB:bit1;
  FA_SFIFO_EMPTYB:bit1;
  RESERVED3      :bit1;
  IN_BUSY        :bit1;
  FG_BUSY        :bit1;
  LA_BUSY        :bit1;
  FL_BUSY        :bit1;
  TA_BUSY        :bit1;
  FA_BUSY        :bit1;
  AL_BUSY        :bit1;
  BUSY           :bit1;
 end;

 TTCP_CNTL2=bitpacked record
  LS_DISABLE_CLOCKS:bit8;
  RESERVED0        :bit24;
 end;

 TTD_STATUS=bitpacked record
  RESERVED0:bit31;
  BUSY     :bit1;
 end;

 TCP_DFY_CMD=bitpacked record
  OFFSET   :bit9;
  RESERVED0:bit7;
  SIZE     :bit16;
 end;

 TGDS_CONFIG=bitpacked record
  RESERVED0        :bit1;
  SH0_GPR_PHASE_SEL:bit2;
  SH1_GPR_PHASE_SEL:bit2;
  SH2_GPR_PHASE_SEL:bit2;
  SH3_GPR_PHASE_SEL:bit2;
  RESERVED1        :bit23;
 end;

 TGRBM_DEBUG=bitpacked record
  RESERVED0                :bit1;
  IGNORE_RDY               :bit1;
  RESERVED1                :bit3;
  IGNORE_FAO               :bit1;
  DISABLE_READ_TIMEOUT     :bit1;
  SNAPSHOT_FREE_CNTRS      :bit1;
  HYSTERESIS_GUI_ACTIVE    :bit4;
  GFX_CLOCK_DOMAIN_OVERRIDE:bit1;
  GRBM_TRAP_ENABLE         :bit1;
  RESERVED2                :bit17;
  DEBUG_BUS_FGCG_EN        :bit1;
 end;

 TIA_ENHANCE=bit32;

 TSX_DEBUG_1=bitpacked record
  SX_DB_QUAD_CREDIT              :bit7;
  RESERVED0                      :bit1;
  DISABLE_BLEND_OPT_DONT_RD_DST  :bit1;
  DISABLE_BLEND_OPT_BYPASS       :bit1;
  DISABLE_BLEND_OPT_DISCARD_PIXEL:bit1;
  DISABLE_QUAD_PAIR_OPT          :bit1;
  DISABLE_PIX_EN_ZERO_OPT        :bit1;
  DEBUG_DATA                     :bit19;
 end;

 TTA_SCRATCH=bit32;

 TTCP_CREDIT=bitpacked record
  LFIFO_CREDIT   :bit10;
  RESERVED0      :bit6;
  REQ_FIFO_CREDIT:bit7;
  RESERVED1      :bit6;
  TD_CREDIT      :bit3;
 end;

 TTCP_STATUS=bitpacked record
  TCP_BUSY    :bit1;
  INPUT_BUSY  :bit1;
  ADRS_BUSY   :bit1;
  TAGRAMS_BUSY:bit1;
  CNTRL_BUSY  :bit1;
  LFIFO_BUSY  :bit1;
  READ_BUSY   :bit1;
  FORMAT_BUSY :bit1;
  RESERVED0   :bit24;
 end;

 TTD_SCRATCH=bit32;

 TWD_ENHANCE=bit32;

 TCP_CMD_DATA=bit32;

 TCP_CSF_CNTL=bitpacked record
  FETCH_BUFFER_DEPTH:bit4;
  RESERVED0         :bit28;
 end;

 TCP_CSF_STAT=bitpacked record
  BUFFER_SLOTS_ALLOCATED:bit4;
  RESERVED0             :bit4;
  BUFFER_REQUEST_COUNT  :bit9;
  RESERVED1             :bit15;
 end;

 TCP_DFY_CNTL=bitpacked record
  POLICY    :bit1;
  RESERVED0 :bit1;
  MTYPE     :bit2;
  RESERVED1 :bit23;
  RESERVED2 :bit1;
  LFSR_RESET:bit1;
  MODE      :bit2;
  ENABLE    :bit1;
 end;

 TCP_DFY_STAT=bitpacked record
  BURST_COUNT :bit16;
  TAGS_PENDING:bit9;
  RESERVED0   :bit6;
  BUSY        :bit1;
 end;

 TCP_DMA_CNTL=bitpacked record
  RESERVED0     :bit4;
  MIN_AVAILSZ   :bit2;
  RESERVED1     :bit10;
  BUFFER_DEPTH  :bit4;
  RESERVED2     :bit8;
  PIO_FIFO_EMPTY:bit1;
  PIO_FIFO_FULL :bit1;
  PIO_COUNT     :bit2;
 end;

 TCP_HQD_VMID=bitpacked record
  VMID     :bit4;
  RESERVED0:bit4;
  IB_VMID  :bit4;
  RESERVED1:bit4;
  VQID     :bit10;
  RESERVED2:bit6;
 end;

 TCP_INT_CNTL=bitpacked record
  RESERVED0                    :bit11;
  CP_VM_DOORBELL_WR_INT_ENABLE :bit1;
  RESERVED1                    :bit2;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  RESERVED2                    :bit2;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  CMP_BUSY_INT_ENABLE          :bit1;
  CNTX_BUSY_INT_ENABLE         :bit1;
  CNTX_EMPTY_INT_ENABLE        :bit1;
  GFX_IDLE_INT_ENABLE          :bit1;
  PRIV_INSTR_INT_ENABLE        :bit1;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_MEC_CNTL=bitpacked record
  RESERVED0            :bit4;
  MEC_INVALIDATE_ICACHE:bit1;
  RESERVED1            :bit11;
  MEC_ME1_PIPE0_RESET  :bit1;
  MEC_ME1_PIPE1_RESET  :bit1;
  MEC_ME1_PIPE2_RESET  :bit1;
  MEC_ME1_PIPE3_RESET  :bit1;
  MEC_ME2_PIPE0_RESET  :bit1;
  MEC_ME2_PIPE1_RESET  :bit1;
  RESERVED2            :bit6;
  MEC_ME2_HALT         :bit1;
  MEC_ME2_STEP         :bit1;
  MEC_ME1_HALT         :bit1;
  MEC_ME1_STEP         :bit1;
 end;

 TCP_MEQ_STAT=bitpacked record
  MEQ_RPTR :bit10;
  RESERVED0:bit6;
  MEQ_WPTR :bit10;
  RESERVED1:bit6;
 end;

 TCP_PWR_CNTL=bitpacked record
  GFX_CLK_HALT_ME0_PIPE0:bit1;
  GFX_CLK_HALT_ME0_PIPE1:bit1;
  RESERVED0             :bit6;
  CMP_CLK_HALT_ME1_PIPE0:bit1;
  CMP_CLK_HALT_ME1_PIPE1:bit1;
  CMP_CLK_HALT_ME1_PIPE2:bit1;
  CMP_CLK_HALT_ME1_PIPE3:bit1;
  RESERVED1             :bit4;
  CMP_CLK_HALT_ME2_PIPE0:bit1;
  CMP_CLK_HALT_ME2_PIPE1:bit1;
  CMP_CLK_HALT_ME2_PIPE2:bit1;
  CMP_CLK_HALT_ME2_PIPE3:bit1;
  RESERVED2             :bit12;
 end;

 TCP_RB0_BASE=bit32;

 TCP_RB0_CNTL=bitpacked record
  RB_BUFSZ      :bit6;
  RESERVED0     :bit2;
  RB_BLKSZ      :bit6;
  RESERVED1     :bit1;
  MTYPE         :bit2;
  BUF_SWAP      :bit2;
  RESERVED2     :bit1;
  MIN_AVAILSZ   :bit2;
  MIN_IB_AVAILSZ:bit2;
  CACHE_POLICY  :bit1;
  RESERVED3     :bit2;
  RB_NO_UPDATE  :bit1;
  RESERVED4     :bit3;
  RB_RPTR_WR_ENA:bit1;
 end;

 TCP_RB0_RPTR=bitpacked record
  RB_RPTR  :bit20;
  RESERVED0:bit12;
 end;

 TCP_RB0_WPTR=bitpacked record
  RB_WPTR  :bit20;
  RESERVED0:bit12;
 end;

 TCP_RB1_BASE=bit32;

 TCP_RB1_CNTL=bitpacked record
  RB_BUFSZ      :bit6;
  RESERVED0     :bit2;
  RB_BLKSZ      :bit6;
  RESERVED1     :bit1;
  MTYPE         :bit2;
  RESERVED2     :bit3;
  MIN_AVAILSZ   :bit2;
  MIN_IB_AVAILSZ:bit2;
  CACHE_POLICY  :bit1;
  RESERVED3     :bit2;
  RB_NO_UPDATE  :bit1;
  RESERVED4     :bit3;
  RB_RPTR_WR_ENA:bit1;
 end;

 TCP_RB1_RPTR=bitpacked record
  RB_RPTR  :bit20;
  RESERVED0:bit12;
 end;

 TCP_RB1_WPTR=bitpacked record
  RB_WPTR  :bit20;
  RESERVED0:bit12;
 end;

 TCP_RB2_BASE=bit32;

 TCP_RB2_CNTL=bitpacked record
  RB_BUFSZ      :bit6;
  RESERVED0     :bit2;
  RB_BLKSZ      :bit6;
  RESERVED1     :bit1;
  MTYPE         :bit2;
  RESERVED2     :bit3;
  MIN_AVAILSZ   :bit2;
  MIN_IB_AVAILSZ:bit2;
  CACHE_POLICY  :bit1;
  RESERVED3     :bit2;
  RB_NO_UPDATE  :bit1;
  RESERVED4     :bit3;
  RB_RPTR_WR_ENA:bit1;
 end;

 TCP_RB2_RPTR=bitpacked record
  RB_RPTR  :bit20;
  RESERVED0:bit12;
 end;

 TCP_RB2_WPTR=bitpacked record
  RB_WPTR  :bit20;
  RESERVED0:bit12;
 end;

 TCP_STQ_STAT=bitpacked record
  STQ_RPTR :bit10;
  RESERVED0:bit6;
  RESERVED1:bit10;
  RESERVED2:bit6;
 end;

 TCP_ST_BUFSZ=bitpacked record
  ST_BUFSZ :bit20;
  RESERVED0:bit12;
 end;

 TGDS_ATOM_OP=bitpacked record
  OP    :bit8;
  UNUSED:bit24;
 end;

 TGDS_EDC_CNT=bitpacked record
  DED      :bit8;
  SED      :bit8;
  SEC      :bit8;
  RESERVED0:bit8;
 end;

 TGDS_ENHANCE=bitpacked record
  MISC          :bit16;
  AUTO_INC_INDEX:bit1;
  CGPG_RESTORE  :bit1;
  RESERVED0     :bit14;
 end;

 TGDS_OA_CNTL=bitpacked record
  INDEX    :bit4;
  RESERVED0:bit28;
 end;

 TGDS_RD_ADDR=bit32;

 TGDS_RD_DATA=bit32;

 TGDS_WR_ADDR=bit32;

 TGDS_WR_DATA=bit32;

 TGRBM_STATUS=bitpacked record
  ME0PIPE0_CMDFIFO_AVAIL:bit4;
  RESERVED0             :bit1;
  SRBM_RQ_PENDING       :bit1;
  RESERVED1             :bit1;
  ME0PIPE0_CF_RQ_PENDING:bit1;
  ME0PIPE0_PF_RQ_PENDING:bit1;
  GDS_DMA_RQ_PENDING    :bit1;
  RESERVED2             :bit2;
  DB_CLEAN              :bit1;
  CB_CLEAN              :bit1;
  TA_BUSY               :bit1;
  GDS_BUSY              :bit1;
  WD_BUSY_NO_DMA        :bit1;
  VGT_BUSY              :bit1;
  IA_BUSY_NO_DMA        :bit1;
  IA_BUSY               :bit1;
  SX_BUSY               :bit1;
  WD_BUSY               :bit1;
  SPI_BUSY              :bit1;
  BCI_BUSY              :bit1;
  SC_BUSY               :bit1;
  PA_BUSY               :bit1;
  DB_BUSY               :bit1;
  RESERVED3             :bit1;
  CP_COHERENCY_BUSY     :bit1;
  CP_BUSY               :bit1;
  CB_BUSY               :bit1;
  GUI_ACTIVE            :bit1;
 end;

 TSPI_EDC_CNT=bitpacked record
  SED      :bit8;
  RESERVED0:bit24;
 end;

 TTA_CNTL_AUX=bitpacked record
  SCOAL_DSWIZZLE_N  :bit1;
  RESERVED          :bit3;
  D16_PACK_DISABLE  :bit1;
  RESERVED0         :bit11;
  ANISO_WEIGHT_MODE :bit1;
  ANISO_RATIO_LUT   :bit1;
  ANISO_TAP         :bit1;
  ANISO_MIP_ADJ_MODE:bit1;
  RESERVED1         :bit12;
 end;

 TTCC_EDC_CNT=bitpacked record
  SEC_COUNT:bit8;
  RESERVED0:bit8;
  DED_COUNT:bit8;
  RESERVED1:bit8;
 end;

 TTCP_EDC_CNT=bitpacked record
  SEC_COUNT      :bit8;
  LFIFO_SED_COUNT:bit8;
  DED_COUNT      :bit8;
  UNUSED         :bit8;
 end;

 TTD_DSM_CNTL=bitpacked record
  FORCE_SEDB_0     :bit1;
  FORCE_SEDB_1     :bit1;
  EN_SINGLE_WR_SEDB:bit1;
  RESERVED0        :bit29;
 end;

 TVGT_ENHANCE=bit32;

 TVGT_GS_MODE=bitpacked record
  MODE              :bit3;
  RESERVED_0        :bit1;
  CUT_MODE          :bit2;
  RESERVED_1        :bit5;
  GS_C_PACK_EN      :bit1;
  RESERVED_2        :bit1;
  ES_PASSTHRU       :bit1;
  COMPUTE_MODE      :bit1;
  FAST_COMPUTE_MODE :bit1;
  ELEMENT_INFO_EN   :bit1;
  PARTIAL_THD_AT_EOI:bit1;
  SUPPRESS_CUTS     :bit1;
  ES_WRITE_OPTIMIZE :bit1;
  GS_WRITE_OPTIMIZE :bit1;
  ONCHIP            :bit2;
  RESERVED0         :bit9;
 end;

 TCB_BLEND_RED=bit32;

 TCOMPUTE_VMID=bitpacked record
  DATA     :bit4;
  RESERVED0:bit28;
 end;

 TCPC_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_BUSY_STAT=bitpacked record
  REG_BUS_FIFO_BUSY     :bit1;
  RESERVED0             :bit1;
  RESERVED1             :bit1;
  RESERVED2             :bit1;
  RESERVED3             :bit1;
  RESERVED4             :bit1;
  COHER_CNT_NEQ_ZERO    :bit1;
  PFP_PARSING_PACKETS   :bit1;
  ME_PARSING_PACKETS    :bit1;
  RCIU_PFP_BUSY         :bit1;
  RCIU_ME_BUSY          :bit1;
  RESERVED5             :bit1;
  SEM_CMDFIFO_NOT_EMPTY :bit1;
  SEM_FAILED_AND_HOLDING:bit1;
  SEM_POLLING_FOR_PASS  :bit1;
  GFX_CONTEXT_BUSY      :bit1;
  RESERVED6             :bit1;
  ME_PARSER_BUSY        :bit1;
  EOP_DONE_BUSY         :bit1;
  STRM_OUT_BUSY         :bit1;
  PIPE_STATS_BUSY       :bit1;
  RCIU_CE_BUSY          :bit1;
  CE_PARSING_PACKETS    :bit1;
  RESERVED7             :bit9;
 end;

 TCP_CMD_INDEX=bitpacked record
  CMD_INDEX    :bit11;
  RESERVED0    :bit1;
  CMD_ME_SEL   :bit2;
  RESERVED1    :bit2;
  CMD_QUEUE_SEL:bit3;
  RESERVED2    :bit13;
 end;

 TCP_CNTX_STAT=bitpacked record
  ACTIVE_HP3D_CONTEXTS:bit8;
  CURRENT_HP3D_CONTEXT:bit3;
  RESERVED0           :bit9;
  ACTIVE_GFX_CONTEXTS :bit8;
  CURRENT_GFX_CONTEXT :bit3;
  RESERVED1           :bit1;
 end;

 TCP_DEVICE_ID=bitpacked record
  DEVICE_ID:bit8;
  RESERVED0:bit24;
 end;

 TCP_HQD_ERROR=bitpacked record
  EDC_ERROR_ID:bit4;
  SUA_ERROR   :bit1;
  RESERVED0   :bit27;
 end;

 TCP_IB1_BUFSZ=bitpacked record
  IB1_BUFSZ:bit20;
  RESERVED0:bit12;
 end;

 TCP_IB2_BUFSZ=bitpacked record
  IB2_BUFSZ:bit20;
  RESERVED0:bit12;
 end;

 TCP_MEQ_AVAIL=bitpacked record
  MEQ_CNT  :bit10;
  RESERVED0:bit22;
 end;

 TCP_PQ_STATUS=bitpacked record
  DOORBELL_UPDATED:bit1;
  DOORBELL_ENABLE :bit1;
  RESERVED0       :bit30;
 end;

 TCP_ROQ_AVAIL=bitpacked record
  ROQ_CNT_RING:bit11;
  RESERVED0   :bit5;
  ROQ_CNT_IB1 :bit11;
  RESERVED1   :bit5;
 end;

 TCP_STQ_AVAIL=bitpacked record
  STQ_CNT  :bit9;
  RESERVED0:bit23;
 end;

 TGDS_ATOM_DST=bit32;

 TGDS_DSM_CNTL=bitpacked record
  SEL_DSM_GDS_IRRITATOR_DATA_A_0:bit1;
  SEL_DSM_GDS_IRRITATOR_DATA_A_1:bit1;
  GDS_ENABLE_SINGLE_WRITE_A     :bit1;
  SEL_DSM_GDS_IRRITATOR_DATA_B_0:bit1;
  SEL_DSM_GDS_IRRITATOR_DATA_B_1:bit1;
  GDS_ENABLE_SINGLE_WRITE_B     :bit1;
  UNUSED                        :bit26;
 end;

 TGDS_ENHANCE2=bitpacked record
  MISC     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_RESET=bitpacked record
  RESET    :bit1;
  RESERVED0:bit7;
  PIPE_ID  :bit8;
  RESERVED1:bit16;
 end;

 TGDS_OA_VMID0=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID1=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID2=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID3=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID4=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID5=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID6=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID7=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID8=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID9=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGRBM_NOWHERE=bit32;

 TGRBM_STATUS2=bitpacked record
  ME0PIPE1_CMDFIFO_AVAIL:bit4;
  ME0PIPE1_CF_RQ_PENDING:bit1;
  ME0PIPE1_PF_RQ_PENDING:bit1;
  ME1PIPE0_RQ_PENDING   :bit1;
  ME1PIPE1_RQ_PENDING   :bit1;
  ME1PIPE2_RQ_PENDING   :bit1;
  ME1PIPE3_RQ_PENDING   :bit1;
  ME2PIPE0_RQ_PENDING   :bit1;
  ME2PIPE1_RQ_PENDING   :bit1;
  ME2PIPE2_RQ_PENDING   :bit1;
  ME2PIPE3_RQ_PENDING   :bit1;
  RLC_RQ_PENDING        :bit1;
  RESERVED0             :bit9;
  RLC_BUSY              :bit1;
  TC_BUSY               :bit1;
  TCC_CC_RESIDENT       :bit1;
  RESERVED1             :bit1;
  CPF_BUSY              :bit1;
  CPC_BUSY              :bit1;
  CPG_BUSY              :bit1;
  RESERVED2             :bit1;
 end;

 TGRBM_TRAP_OP=bitpacked record
  RW       :bit1;
  RESERVED0:bit31;
 end;

 TGRBM_TRAP_WD=bit32;

 TSPI_DSM_CNTL=bitpacked record
  Sel_DSM_SPI_Irritator_data0:bit1;
  Sel_DSM_SPI_Irritator_data1:bit1;
  SPI_Enable_Single_Write    :bit1;
  UNUSED                     :bit29;
 end;

 TSPI_GFX_CNTL=bitpacked record
  RESET_COUNTS:bit1;
  RESERVED0   :bit31;
 end;

 TTA_CGTT_CTRL=bitpacked record
  ON_DELAY      :bit4;
  OFF_HYSTERESIS:bit8;
  RESERVED0     :bit12;
  SOFT_OVERRIDE7:bit1;
  SOFT_OVERRIDE6:bit1;
  SOFT_OVERRIDE5:bit1;
  SOFT_OVERRIDE4:bit1;
  SOFT_OVERRIDE3:bit1;
  SOFT_OVERRIDE2:bit1;
  SOFT_OVERRIDE1:bit1;
  SOFT_OVERRIDE0:bit1;
 end;

 TTCC_DSM_CNTL=bitpacked record
  CACHE_RAM_IRRITATOR_DATA_SEL    :bit2;
  CACHE_RAM_IRRITATOR_SINGLE_WRITE:bit1;
  RESERVED0                       :bit29;
 end;

 TTCP_DSM_CNTL=bitpacked record
  CACHE_RAM_IRRITATOR_DATA_SEL    :bit2;
  CACHE_RAM_IRRITATOR_SINGLE_WRITE:bit1;
  LFIFO_RAM_IRRITATOR_DATA_SEL    :bit2;
  LFIFO_RAM_IRRITATOR_SINGLE_WRITE:bit1;
  RESERVED0                       :bit26;
 end;

 TTD_CGTT_CTRL=bitpacked record
  ON_DELAY      :bit4;
  OFF_HYSTERESIS:bit8;
  RESERVED0     :bit12;
  SOFT_OVERRIDE7:bit1;
  SOFT_OVERRIDE6:bit1;
  SOFT_OVERRIDE5:bit1;
  SOFT_OVERRIDE4:bit1;
  SOFT_OVERRIDE3:bit1;
  SOFT_OVERRIDE2:bit1;
  SOFT_OVERRIDE1:bit1;
  SOFT_OVERRIDE0:bit1;
 end;

 TVGT_DMA_BASE=bit32;

 TVGT_DMA_SIZE=bit32;

 TVGT_HOS_CNTL=bitpacked record
  TESS_MODE:bit2;
  RESERVED0:bit30;
 end;

 TVGT_TF_PARAM=bitpacked record
  _TYPE                :bit2;
  PARTITIONING         :bit3;
  TOPOLOGY             :bit3;
  RESERVED0            :bit1;
  DEPRECATED           :bit1;
  NUM_DS_WAVES_PER_SIMD:bit4;
  DISABLE_DONUTS       :bit1;
  RDREQ_POLICY         :bit2;
  DISTRIBUTION_MODE    :bit2;
  MTYPE                :bit2;
  RESERVED1            :bit11;
 end;

 TCB_BLEND_BLUE=bit32;

 TCB_DCC_CONFIG=bitpacked record
  OVERWRITE_COMBINER_DEPTH         :bit5;
  OVERWRITE_COMBINER_DISABLE       :bit1;
  OVERWRITE_COMBINER_CC_POP_DISABLE:bit1;
  RESERVED0                        :bit1;
  FC_RDLAT_KEYID_FIFO_DEPTH        :bit8;
  READ_RETURN_SKID_FIFO_DEPTH      :bit7;
  RESERVED1                        :bit1;
  DCC_CACHE_EVICT_POINT            :bit4;
  DCC_CACHE_NUM_TAGS               :bit4;
 end;

 TCB_HW_CONTROL=bitpacked record
  CM_CACHE_EVICT_POINT                            :bit4;
  RESERVED0                                       :bit2;
  FC_CACHE_EVICT_POINT                            :bit4;
  RESERVED1                                       :bit2;
  CC_CACHE_EVICT_POINT                            :bit4;
  ALLOW_MRT_WITH_DUAL_SOURCE                      :bit1;
  RESERVED2                                       :bit1;
  DISABLE_INTNORM_LE11BPC_CLAMPING                :bit1;
  FORCE_NEEDS_DST                                 :bit1;
  FORCE_ALWAYS_TOGGLE                             :bit1;
  DISABLE_BLEND_OPT_RESULT_EQ_DEST                :bit1;
  DISABLE_FULL_WRITE_MASK                         :bit1;
  DISABLE_RESOLVE_OPT_FOR_SINGLE_FRAG             :bit1;
  DISABLE_BLEND_OPT_DONT_RD_DST                   :bit1;
  DISABLE_BLEND_OPT_BYPASS                        :bit1;
  DISABLE_BLEND_OPT_DISCARD_PIXEL                 :bit1;
  DISABLE_BLEND_OPT_WHEN_DISABLED_SRCALPHA_IS_USED:bit1;
  PRIORITIZE_FC_WR_OVER_FC_RD_ON_CMASK_CONFLICT   :bit1;
  PRIORITIZE_FC_EVICT_OVER_FOP_RD_ON_BANK_CONFLICT:bit1;
  DISABLE_CC_IB_SERIALIZER_STATE_OPT              :bit1;
  DISABLE_PIXEL_IN_QUAD_FIX_FOR_LINEAR_SURFACE    :bit1;
 end;

 TCOMPUTE_DIM_X=bit32;

 TCOMPUTE_DIM_Y=bit32;

 TCOMPUTE_DIM_Z=bit32;

 TCP_CEQ1_AVAIL=bitpacked record
  CEQ_CNT_RING:bit11;
  RESERVED0   :bit5;
  CEQ_CNT_IB1 :bit11;
  RESERVED1   :bit5;
 end;

 TCP_CEQ2_AVAIL=bitpacked record
  CEQ_CNT_IB2:bit11;
  RESERVED0  :bit21;
 end;

 TCP_CE_COUNTER=bit32;

 TCP_COHER_BASE=bit32;

 TCP_COHER_CNTL=bitpacked record
  DEST_BASE_0_ENA         :bit1;
  DEST_BASE_1_ENA         :bit1;
  TC_SD_ACTION_ENA        :bit1;
  TC_NC_ACTION_ENA        :bit1;
  RESERVED0               :bit2;
  CB0_DEST_BASE_ENA       :bit1;
  CB1_DEST_BASE_ENA       :bit1;
  CB2_DEST_BASE_ENA       :bit1;
  CB3_DEST_BASE_ENA       :bit1;
  CB4_DEST_BASE_ENA       :bit1;
  CB5_DEST_BASE_ENA       :bit1;
  CB6_DEST_BASE_ENA       :bit1;
  CB7_DEST_BASE_ENA       :bit1;
  DB_DEST_BASE_ENA        :bit1;
  TCL1_VOL_ACTION_ENA     :bit1;
  TC_VOL_ACTION_ENA__CI   :bit1;
  RESERVED1               :bit1;
  TC_WB_ACTION_ENA        :bit1;
  DEST_BASE_2_ENA         :bit1;
  RESERVED2               :bit1;
  DEST_BASE_3_ENA         :bit1;
  TCL1_ACTION_ENA         :bit1;
  TC_ACTION_ENA           :bit1;
  RESERVED3               :bit1;
  CB_ACTION_ENA           :bit1;
  DB_ACTION_ENA           :bit1;
  SH_KCACHE_ACTION_ENA    :bit1;
  SH_KCACHE_VOL_ACTION_ENA:bit1;
  SH_ICACHE_ACTION_ENA    :bit1;
  SH_KCACHE_WB_ACTION_ENA :bit1;
  SH_SD_ACTION_ENA        :bit1;
 end;

 TCP_COHER_SIZE=bit32;

 TCP_CONFIG__SI=bitpacked record
  RESERVED0   :bit8;
  CP_RDREQ_URG:bit4;
  RESERVED1   :bit4;
  CP_REQ_TRAN :bit1;
  RESERVED2   :bit15;
 end;

 TCP_CPC_STATUS=bitpacked record
  MEC1_BUSY             :bit1;
  MEC2_BUSY             :bit1;
  DC0_BUSY              :bit1;
  DC1_BUSY              :bit1;
  RCIU1_BUSY            :bit1;
  RCIU2_BUSY            :bit1;
  ROQ1_BUSY             :bit1;
  ROQ2_BUSY             :bit1;
  MIU_RDREQ_BUSY__SI__CI:bit1;
  MIU_WRREQ_BUSY__SI__CI:bit1;
  TCIU_BUSY             :bit1;
  SCRATCH_RAM_BUSY      :bit1;
  QU_BUSY               :bit1;
  ATCL2IU_BUSY          :bit1;
  RESERVED0             :bit15;
  CPG_CPC_BUSY          :bit1;
  CPF_CPC_BUSY          :bit1;
  CPC_BUSY              :bit1;
 end;

 TCP_CPF_STATUS=bitpacked record
  POST_WPTR_GFX_BUSY    :bit1;
  CSF_BUSY              :bit1;
  MIU_RDREQ_BUSY__SI__CI:bit1;
  MIU_WRREQ_BUSY__SI__CI:bit1;
  ROQ_ALIGN_BUSY        :bit1;
  ROQ_RING_BUSY         :bit1;
  ROQ_INDIRECT1_BUSY    :bit1;
  ROQ_INDIRECT2_BUSY    :bit1;
  ROQ_STATE_BUSY        :bit1;
  ROQ_CE_RING_BUSY      :bit1;
  ROQ_CE_INDIRECT1_BUSY :bit1;
  ROQ_CE_INDIRECT2_BUSY :bit1;
  SEMAPHORE_BUSY        :bit1;
  INTERRUPT_BUSY        :bit1;
  TCIU_BUSY             :bit1;
  HQD_BUSY              :bit1;
  PRT_BUSY              :bit1;
  ATCL2IU_BUSY          :bit1;
  RESERVED0             :bit8;
  CPF_GFX_BUSY          :bit1;
  CPF_CMP_BUSY          :bit1;
  GRBM_CPF_STAT_BUSY    :bit2;
  CPC_CPF_BUSY          :bit1;
  CPF_BUSY              :bit1;
 end;

 TCP_DFY_DATA_0=bit32;

 TCP_DFY_DATA_1=bit32;

 TCP_DFY_DATA_2=bit32;

 TCP_DFY_DATA_3=bit32;

 TCP_DFY_DATA_4=bit32;

 TCP_DFY_DATA_5=bit32;

 TCP_DFY_DATA_6=bit32;

 TCP_DFY_DATA_7=bit32;

 TCP_DFY_DATA_8=bit32;

 TCP_DFY_DATA_9=bit32;

 TCP_HQD_ACTIVE=bitpacked record
  ACTIVE   :bit1;
  BUSY_GATE:bit1;
  RESERVED0:bit30;
 end;

 TCP_IB1_OFFSET=bitpacked record
  IB1_OFFSET:bit20;
  RESERVED0 :bit12;
 end;

 TCP_IB2_OFFSET=bitpacked record
  IB2_OFFSET:bit20;
  RESERVED0 :bit12;
 end;

 TCP_INDEX_TYPE=bitpacked record
  INDEX_TYPE:bit2;
  RESERVED0 :bit30;
 end;

 TCP_INT_STATUS=bitpacked record
  RESERVED0                  :bit11;
  CP_VM_DOORBELL_WR_INT_STAT :bit1;
  RESERVED1                  :bit2;
  CP_ECC_ERROR_INT_STAT      :bit1;
  RESERVED2                  :bit2;
  WRM_POLL_TIMEOUT_INT_STAT  :bit1;
  CMP_BUSY_INT_STAT          :bit1;
  CNTX_BUSY_INT_STAT         :bit1;
  CNTX_EMPTY_INT_STAT        :bit1;
  GFX_IDLE_INT_STAT          :bit1;
  PRIV_INSTR_INT_STAT        :bit1;
  PRIV_REG_INT_STAT          :bit1;
  OPCODE_ERROR_INT_STAT      :bit1;
  RESERVED3                  :bit1;
  TIME_STAMP_INT_STAT        :bit1;
  RESERVED_BIT_ERROR_INT_STAT:bit1;
  RESERVED4                  :bit1;
  GENERIC2_INT_STAT          :bit1;
  GENERIC1_INT_STAT          :bit1;
  GENERIC0_INT_STAT          :bit1;
 end;

 TCP_ROQ2_AVAIL=bitpacked record
  ROQ_CNT_IB2:bit11;
  RESERVED0  :bit21;
 end;

 TCP_ST_BASE_HI=bitpacked record
  ST_BASE_HI:bit16;
  RESERVED0 :bit16;
 end;

 TCP_ST_BASE_LO=bitpacked record
  RESERVED0 :bit2;
  ST_BASE_LO:bit30;
 end;

 TCP_VMID_RESET=bitpacked record
  RESET_REQUEST:bit16;
  RESET_STATUS :bit16;
 end;

 TDB_DEPTH_INFO=bitpacked record
  ADDR5_SWIZZLE_MASK:bit4;
  ARRAY_MODE        :bit4;
  PIPE_CONFIG       :bit5;
  BANK_WIDTH        :bit2;
  BANK_HEIGHT       :bit2;
  MACRO_TILE_ASPECT :bit2;
  NUM_BANKS         :bit2;
  RESERVED0         :bit11;
 end;

 TDB_DEPTH_SIZE=bitpacked record
  PITCH_TILE_MAX :bit11;
  HEIGHT_TILE_MAX:bit11;
  RESERVED0      :bit10;
 end;

 TDB_DEPTH_VIEW=bitpacked record
  SLICE_START      :bit11;
  RESERVED0        :bit2;
  SLICE_MAX        :bit11;
  Z_READ_ONLY      :bit1;
  STENCIL_READ_ONLY:bit1;
  RESERVED1        :bit6;
 end;

 TDB_WATERMARKS=bitpacked record
  DEPTH_FREE           :bit5;
  DEPTH_FLUSH          :bit6;
  FORCE_SUMMARIZE      :bit4;
  DEPTH_PENDING_FREE   :bit5;
  DEPTH_CACHELINE_FREE :bit7;
  EARLY_Z_PANIC_DISABLE:bit1;
  LATE_Z_PANIC_DISABLE :bit1;
  RE_Z_PANIC_DISABLE   :bit1;
  AUTO_FLUSH_HTILE     :bit1;
  AUTO_FLUSH_QUAD      :bit1;
 end;

 TGDS_ATOM_BASE=bitpacked record
  BASE  :bit16;
  UNUSED:bit16;
 end;

 TGDS_ATOM_CNTL=bitpacked record
  AINC     :bit6;
  RESERVED0:bit2;
  DMODE    :bit2;
  RESERVED1:bit22;
 end;

 TGDS_ATOM_SIZE=bitpacked record
  SIZE  :bit16;
  UNUSED:bit16;
 end;

 TGDS_ATOM_SRC0=bit32;

 TGDS_ATOM_SRC1=bit32;

 TGDS_GWS_VMID0=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID1=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID2=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID3=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID4=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID5=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID6=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID7=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID8=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID9=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_OA_INCDEC=bitpacked record
  VALUE :bit31;
  INCDEC:bit1;
 end;

 TGDS_OA_VMID10=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID11=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID12=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID13=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID14=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_OA_VMID15=bitpacked record
  MASK     :bit16;
  RESERVED0:bit16;
 end;

 TGRBM_CAM_DATA=bitpacked record
  CAM_ADDR     :bit16;
  CAM_REMAPADDR:bit16;
 end;

 TGRBM_INT_CNTL=bitpacked record
  RDERR_INT_ENABLE   :bit1;
  RESERVED0          :bit18;
  GUI_IDLE_INT_ENABLE:bit1;
  RESERVED1          :bit12;
 end;

 TGRBM_PWR_CNTL=bitpacked record
  ALL_REQ_TYPE:bit2;
  GFX_REQ_TYPE:bit2;
  ALL_RSP_TYPE:bit2;
  GFX_RSP_TYPE:bit2;
  RESERVED0   :bit6;
  GFX_REQ_EN  :bit1;
  ALL_REQ_EN  :bit1;
  RESERVED1   :bit16;
 end;

 TIA_DEBUG_CNTL=bitpacked record
  IA_DEBUG_INDX     :bit6;
  IA_DEBUG_SEL_BUS_B:bit1;
  RESERVED0         :bit25;
 end;

 TIA_DEBUG_DATA=bit32;

 TIA_DEBUG_REG0=bitpacked record
  ia_busy_extended      :bit1;
  ia_nodma_busy_extended:bit1;
  ia_busy               :bit1;
  ia_nodma_busy         :bit1;
  SPARE0                :bit1;
  dma_req_busy          :bit1;
  dma_busy              :bit1;
  mc_xl8r_busy          :bit1;
  grp_busy              :bit1;
  SPARE1                :bit1;
  dma_grp_valid         :bit1;
  grp_dma_read          :bit1;
  dma_grp_hp_valid      :bit1;
  grp_dma_hp_read       :bit1;
  SPARE2                :bit10;
  reg_clk_busy          :bit1;
  core_clk_busy         :bit1;
  SPARE3                :bit1;
  SPARE4                :bit1;
  sclk_reg_vld          :bit1;
  sclk_core_vld         :bit1;
  SPARE5                :bit1;
  SPARE6                :bit1;
 end;

 TIA_DEBUG_REG1=bitpacked record
  dma_input_fifo_empty :bit1;
  dma_input_fifo_full  :bit1;
  start_new_packet     :bit1;
  dma_rdreq_dr_q       :bit1;
  dma_zero_indices_q   :bit1;
  dma_buf_type_q       :bit2;
  dma_req_path_q       :bit1;
  discard_1st_chunk    :bit1;
  discard_2nd_chunk    :bit1;
  second_tc_ret_data_q :bit1;
  dma_tc_ret_sel_q     :bit1;
  last_rdreq_in_dma_op :bit1;
  dma_mask_fifo_empty  :bit1;
  dma_data_fifo_empty_q:bit1;
  dma_data_fifo_full   :bit1;
  dma_req_fifo_empty   :bit1;
  dma_req_fifo_full    :bit1;
  stage2_dr            :bit1;
  stage2_rtr           :bit1;
  stage3_dr            :bit1;
  stage3_rtr           :bit1;
  stage4_dr            :bit1;
  stage4_rtr           :bit1;
  dma_skid_fifo_empty  :bit1;
  dma_skid_fifo_full   :bit1;
  dma_grp_valid        :bit1;
  grp_dma_read         :bit1;
  current_data_valid   :bit1;
  out_of_range_r2_q    :bit1;
  dma_mask_fifo_we     :bit1;
  dma_ret_data_we_q    :bit1;
 end;

 TIA_DEBUG_REG2=bitpacked record
  hp_dma_input_fifo_empty :bit1;
  hp_dma_input_fifo_full  :bit1;
  hp_start_new_packet     :bit1;
  hp_dma_rdreq_dr_q       :bit1;
  hp_dma_zero_indices_q   :bit1;
  hp_dma_buf_type_q       :bit2;
  hp_dma_req_path_q       :bit1;
  hp_discard_1st_chunk    :bit1;
  hp_discard_2nd_chunk    :bit1;
  hp_second_tc_ret_data_q :bit1;
  hp_dma_tc_ret_sel_q     :bit1;
  hp_last_rdreq_in_dma_op :bit1;
  hp_dma_mask_fifo_empty  :bit1;
  hp_dma_data_fifo_empty_q:bit1;
  hp_dma_data_fifo_full   :bit1;
  hp_dma_req_fifo_empty   :bit1;
  hp_dma_req_fifo_full    :bit1;
  hp_stage2_dr            :bit1;
  hp_stage2_rtr           :bit1;
  hp_stage3_dr            :bit1;
  hp_stage3_rtr           :bit1;
  hp_stage4_dr            :bit1;
  hp_stage4_rtr           :bit1;
  hp_dma_skid_fifo_empty  :bit1;
  hp_dma_skid_fifo_full   :bit1;
  hp_dma_grp_valid        :bit1;
  hp_grp_dma_read         :bit1;
  hp_current_data_valid   :bit1;
  hp_out_of_range_r2_q    :bit1;
  hp_dma_mask_fifo_we     :bit1;
  hp_dma_ret_data_we_q    :bit1;
 end;

 TIA_DEBUG_REG3=bitpacked record
  dma_pipe0_rdreq_valid     :bit1;
  dma_pipe0_rdreq_read      :bit1;
  dma_pipe0_rdreq_null_out  :bit1;
  dma_pipe0_rdreq_eop_out   :bit1;
  dma_pipe0_rdreq_use_tc_out:bit1;
  grp_dma_draw_is_pipe0     :bit1;
  must_service_pipe0_req    :bit1;
  send_pipe1_req            :bit1;
  dma_pipe1_rdreq_valid     :bit1;
  dma_pipe1_rdreq_read      :bit1;
  dma_pipe1_rdreq_null_out  :bit1;
  dma_pipe1_rdreq_eop_out   :bit1;
  dma_pipe1_rdreq_use_tc_out:bit1;
  ia_mc_rdreq_rtr_q         :bit1;
  mc_out_rtr                :bit1;
  dma_rdreq_send_out        :bit1;
  pipe0_dr                  :bit1;
  pipe0_rtr                 :bit1;
  ia_tc_rdreq_rtr_q         :bit1;
  tc_out_rtr                :bit1;
  pair0_valid_p1            :bit1;
  pair1_valid_p1            :bit1;
  pair2_valid_p1            :bit1;
  pair3_valid_p1            :bit1;
  tc_req_count_q            :bit2;
  discard_1st_chunk         :bit1;
  discard_2nd_chunk         :bit1;
  last_tc_req_p1            :bit1;
  IA_TC_rdreq_send_out      :bit1;
  TC_IA_rdret_valid_in      :bit1;
  TAP_IA_rdret_vld_in       :bit1;
 end;

 TIA_DEBUG_REG4=bitpacked record
  pipe0_dr                :bit1;
  pipe1_dr                :bit1;
  pipe2_dr                :bit1;
  pipe3_dr                :bit1;
  pipe4_dr                :bit1;
  pipe5_dr                :bit1;
  grp_se0_fifo_empty      :bit1;
  grp_se0_fifo_full       :bit1;
  pipe0_rtr               :bit1;
  pipe1_rtr               :bit1;
  pipe2_rtr               :bit1;
  pipe3_rtr               :bit1;
  pipe4_rtr               :bit1;
  pipe5_rtr               :bit1;
  ia_vgt_prim_rtr_q       :bit1;
  ia_se1vgt_prim_rtr_q    :bit1;
  di_major_mode_p1_q      :bit1;
  gs_mode_p1_q            :bit3;
  di_event_flag_p1_q      :bit1;
  di_state_sel_p1_q       :bit3;
  draw_opaq_en_p1_q       :bit1;
  draw_opaq_active_q      :bit1;
  di_source_select_p1_q   :bit2;
  ready_to_read_di        :bit1;
  di_first_group_of_draw_q:bit1;
  last_shift_of_draw      :bit1;
  current_shift_is_vect1_q:bit1;
 end;

 TIA_DEBUG_REG5=bitpacked record
  di_index_counter_q_15_0:bit16;
  instanceid_13_0        :bit14;
  draw_input_fifo_full   :bit1;
  draw_input_fifo_empty  :bit1;
 end;

 TIA_DEBUG_REG6=bitpacked record
  current_shift_q     :bit4;
  current_stride_pre  :bit4;
  current_stride_q    :bit5;
  first_group_partial :bit1;
  second_group_partial:bit1;
  curr_prim_partial   :bit1;
  next_stride_q       :bit5;
  next_group_partial  :bit1;
  after_group_partial :bit1;
  extract_group       :bit1;
  grp_shift_debug_data:bit8;
 end;

 TIA_DEBUG_REG7=bitpacked record
  reset_indx_state_q          :bit4;
  shift_vect_valid_p2_q       :bit4;
  shift_vect1_valid_p2_q      :bit4;
  shift_vect0_reset_match_p2_q:bit4;
  shift_vect1_reset_match_p2_q:bit4;
  num_indx_in_group_p2_q      :bit3;
  last_group_of_draw_p2_q     :bit1;
  shift_event_flag_p2_q       :bit1;
  indx_shift_is_one_p2_q      :bit1;
  indx_shift_is_two_p2_q      :bit1;
  indx_stride_is_four_p2_q    :bit1;
  shift_prim1_reset_p3_q      :bit1;
  shift_prim1_partial_p3_q    :bit1;
  shift_prim0_reset_p3_q      :bit1;
  shift_prim0_partial_p3_q    :bit1;
 end;

 TIA_DEBUG_REG8=bitpacked record
  di_prim_type_p1_q            :bit5;
  two_cycle_xfer_p1_q          :bit1;
  two_prim_input_p1_q          :bit1;
  shift_vect_end_of_packet_p5_q:bit1;
  last_group_of_inst_p5_q      :bit1;
  shift_prim1_null_flag_p5_q   :bit1;
  shift_prim0_null_flag_p5_q   :bit1;
  grp_continued                :bit1;
  grp_state_sel                :bit3;
  grp_sub_prim_type            :bit6;
  grp_output_path              :bit3;
  grp_null_primitive           :bit1;
  grp_eop                      :bit1;
  grp_eopg                     :bit1;
  grp_event_flag               :bit1;
  grp_components_valid         :bit4;
 end;

 TIA_DEBUG_REG9=bitpacked record
  send_to_se1_p6               :bit1;
  gfx_se_switch_p6             :bit1;
  null_eoi_xfer_prim1_p6       :bit1;
  null_eoi_xfer_prim0_p6       :bit1;
  prim1_eoi_p6                 :bit1;
  prim0_eoi_p6                 :bit1;
  prim1_valid_eopg_p6          :bit1;
  prim0_valid_eopg_p6          :bit1;
  prim1_to_other_se_p6         :bit1;
  eopg_on_last_prim_p6         :bit1;
  eopg_between_prims_p6        :bit1;
  prim_count_eq_group_size_p6  :bit1;
  prim_count_gt_group_size_p6  :bit1;
  two_prim_output_p5_q         :bit1;
  SPARE0                       :bit1;
  SPARE1                       :bit1;
  shift_vect_end_of_packet_p5_q:bit1;
  prim1_xfer_p6                :bit1;
  grp_se1_fifo_empty           :bit1;
  grp_se1_fifo_full            :bit1;
  prim_counter_q               :bit12;
 end;

 TPA_CL_ENHANCE=bitpacked record
  CLIP_VTX_REORDER_ENA  :bit1;
  NUM_CLIP_SEQ          :bit2;
  CLIPPED_PRIM_SEQ_STALL:bit1;
  VE_NAN_PROC_DISABLE   :bit1;
  XTRA_DEBUG_REG_SEL    :bit1;
  RESERVED0             :bit22;
  ECO_SPARE3            :bit1;
  ECO_SPARE2            :bit1;
  ECO_SPARE1            :bit1;
  ECO_SPARE0            :bit1;
 end;

 TPA_CL_UCP_0_W=bit32;

 TPA_CL_UCP_0_X=bit32;

 TPA_CL_UCP_0_Y=bit32;

 TPA_CL_UCP_0_Z=bit32;

 TPA_CL_UCP_1_W=bit32;

 TPA_CL_UCP_1_X=bit32;

 TPA_CL_UCP_1_Y=bit32;

 TPA_CL_UCP_1_Z=bit32;

 TPA_CL_UCP_2_W=bit32;

 TPA_CL_UCP_2_X=bit32;

 TPA_CL_UCP_2_Y=bit32;

 TPA_CL_UCP_2_Z=bit32;

 TPA_CL_UCP_3_W=bit32;

 TPA_CL_UCP_3_X=bit32;

 TPA_CL_UCP_3_Y=bit32;

 TPA_CL_UCP_3_Z=bit32;

 TPA_CL_UCP_4_W=bit32;

 TPA_CL_UCP_4_X=bit32;

 TPA_CL_UCP_4_Y=bit32;

 TPA_CL_UCP_4_Z=bit32;

 TPA_CL_UCP_5_W=bit32;

 TPA_CL_UCP_5_X=bit32;

 TPA_CL_UCP_5_Y=bit32;

 TPA_CL_UCP_5_Z=bit32;

 TPA_SC_ENHANCE=bitpacked record
  ENABLE_PA_SC_OUT_OF_ORDER                            :bit1;
  DISABLE_SC_DB_TILE_FIX                               :bit1;
  DISABLE_AA_MASK_FULL_FIX                             :bit1;
  ENABLE_1XMSAA_SAMPLE_LOCATIONS                       :bit1;
  ENABLE_1XMSAA_SAMPLE_LOC_CENTROID                    :bit1;
  DISABLE_SCISSOR_FIX                                  :bit1;
  DISABLE_PW_BUBBLE_COLLAPSE                           :bit2;
  SEND_UNLIT_STILES_TO_PACKER                          :bit1;
  DISABLE_DUALGRAD_PERF_OPTIMIZATION                   :bit1;
  DISABLE_SC_PROCESS_RESET_PRIM                        :bit1;
  DISABLE_SC_PROCESS_RESET_SUPERTILE                   :bit1;
  DISABLE_SC_PROCESS_RESET_TILE                        :bit1;
  DISABLE_PA_SC_GUIDANCE                               :bit1;
  DISABLE_EOV_ALL_CTRL_ONLY_COMBINATIONS               :bit1;
  ENABLE_MULTICYCLE_BUBBLE_FREEZE                      :bit1;
  DISABLE_OUT_OF_ORDER_PA_SC_GUIDANCE                  :bit1;
  ENABLE_OUT_OF_ORDER_POLY_MODE                        :bit1;
  DISABLE_OUT_OF_ORDER_EOP_SYNC_NULL_PRIMS_LAST        :bit1;
  DISABLE_OUT_OF_ORDER_THRESHOLD_SWITCHING             :bit1;
  ENABLE_OUT_OF_ORDER_THRESHOLD_SWITCH_AT_EOPG_ONLY    :bit1;
  DISABLE_OUT_OF_ORDER_DESIRED_FIFO_EMPTY_SWITCHING    :bit1;
  DISABLE_OUT_OF_ORDER_SELECTED_FIFO_EMPTY_SWITCHING   :bit1;
  DISABLE_OUT_OF_ORDER_EMPTY_SWITCHING_HYSTERYSIS      :bit1;
  ENABLE_OUT_OF_ORDER_DESIRED_FIFO_IS_NEXT_FEID        :bit1;
  DISABLE_OOO_NO_EOPG_SKEW_DESIRED_FIFO_IS_CURRENT_FIFO:bit1;
  OOO_DISABLE_EOP_ON_FIRST_LIVE_PRIM_HIT               :bit1;
  OOO_DISABLE_EOPG_SKEW_THRESHOLD_SWITCHING            :bit1;
  DISABLE_EOP_LINE_STIPPLE_RESET                       :bit1;
  DISABLE_VPZ_EOP_LINE_STIPPLE_RESET                   :bit1;
  ECO_SPARE1                                           :bit1;
  ECO_SPARE0                                           :bit1;
 end;

 TSX_DEBUG_BUSY=bitpacked record
  POS_FREE_OR_VALIDS:bit1;
  POS_REQUESTER_BUSY:bit1;
  PA_SX_BUSY        :bit1;
  POS_SCBD_BUSY     :bit1;
  POS_BANK3VAL3_BUSY:bit1;
  POS_BANK3VAL2_BUSY:bit1;
  POS_BANK3VAL1_BUSY:bit1;
  POS_BANK3VAL0_BUSY:bit1;
  POS_BANK2VAL3_BUSY:bit1;
  POS_BANK2VAL2_BUSY:bit1;
  POS_BANK2VAL1_BUSY:bit1;
  POS_BANK2VAL0_BUSY:bit1;
  POS_BANK1VAL3_BUSY:bit1;
  POS_BANK1VAL2_BUSY:bit1;
  POS_BANK1VAL1_BUSY:bit1;
  POS_BANK1VAL0_BUSY:bit1;
  POS_BANK0VAL3_BUSY:bit1;
  POS_BANK0VAL2_BUSY:bit1;
  POS_BANK0VAL1_BUSY:bit1;
  POS_BANK0VAL0_BUSY:bit1;
  POS_INMUX_VALID   :bit1;
  WRCTRL1_VALIDQ3   :bit1;
  WRCTRL1_VALIDQ2   :bit1;
  WRCTRL1_VALIDQ1   :bit1;
  WRCTRL0_VALIDQ3   :bit1;
  WRCTRL0_VALIDQ2   :bit1;
  WRCTRL0_VALIDQ1   :bit1;
  PCCMD_VALID       :bit1;
  VDATA1_VALID      :bit1;
  VDATA0_VALID      :bit1;
  CMD_BUSYORVAL     :bit1;
  ADDR_BUSYORVAL    :bit1;
 end;

 TTA_DEBUG_DATA=bit32;

 TTD_DEBUG_DATA=bit32;

 TVGT_ES_PER_GS=bitpacked record
  ES_PER_GS:bit11;
  RESERVED0:bit21;
 end;

 TVGT_GS_PER_ES=bitpacked record
  GS_PER_ES:bit11;
  RESERVED0:bit21;
 end;

 TVGT_GS_PER_VS=bitpacked record
  GS_PER_VS:bit4;
  RESERVED0:bit28;
 end;

 TVGT_REUSE_OFF=bitpacked record
  REUSE_OFF:bit1;
  RESERVED0:bit31;
 end;

 TWD_DEBUG_CNTL=bitpacked record
  WD_DEBUG_INDX     :bit6;
  WD_DEBUG_SEL_BUS_B:bit1;
  RESERVED0         :bit25;
 end;

 TWD_DEBUG_DATA=bit32;

 TWD_DEBUG_REG0=bitpacked record
  wd_busy_extended      :bit1;
  wd_nodma_busy_extended:bit1;
  wd_busy               :bit1;
  wd_nodma_busy         :bit1;
  rbiu_busy             :bit1;
  spl_dma_busy          :bit1;
  spl_di_busy           :bit1;
  vgt0_active_q         :bit1;
  vgt1_active_q         :bit1;
  spl_dma_p1_busy       :bit1;
  rbiu_dr_p1_fifo_busy  :bit1;
  rbiu_di_p1_fifo_busy  :bit1;
  SPARE2                :bit1;
  rbiu_dr_fifo_busy     :bit1;
  rbiu_spl_dr_valid     :bit1;
  spl_rbiu_dr_read      :bit1;
  SPARE3                :bit1;
  rbiu_di_fifo_busy     :bit1;
  rbiu_spl_di_valid     :bit1;
  spl_rbiu_di_read      :bit1;
  se0_synced_q          :bit1;
  se1_synced_q          :bit1;
  se2_synced_q          :bit1;
  se3_synced_q          :bit1;
  reg_clk_busy          :bit1;
  input_clk_busy        :bit1;
  core_clk_busy         :bit1;
  vgt2_active_q         :bit1;
  sclk_reg_vld          :bit1;
  sclk_input_vld        :bit1;
  sclk_core_vld         :bit1;
  vgt3_active_q         :bit1;
 end;

 TWD_DEBUG_REG1=bitpacked record
  grbm_fifo_empty        :bit1;
  grbm_fifo_full         :bit1;
  grbm_fifo_we           :bit1;
  grbm_fifo_re           :bit1;
  draw_initiator_valid_q :bit1;
  event_initiator_valid_q:bit1;
  event_addr_valid_q     :bit1;
  dma_request_valid_q    :bit1;
  SPARE0                 :bit1;
  min_indx_valid_q       :bit1;
  max_indx_valid_q       :bit1;
  indx_offset_valid_q    :bit1;
  grbm_fifo_rdata_reg_id :bit5;
  grbm_fifo_rdata_state  :bit3;
  free_cnt_q             :bit6;
  rbiu_di_fifo_we        :bit1;
  rbiu_dr_fifo_we        :bit1;
  rbiu_di_fifo_empty     :bit1;
  rbiu_di_fifo_full      :bit1;
  rbiu_dr_fifo_empty     :bit1;
  rbiu_dr_fifo_full      :bit1;
 end;

 TWD_DEBUG_REG2=bitpacked record
  p1_grbm_fifo_empty        :bit1;
  p1_grbm_fifo_full         :bit1;
  p1_grbm_fifo_we           :bit1;
  p1_grbm_fifo_re           :bit1;
  p1_draw_initiator_valid_q :bit1;
  p1_event_initiator_valid_q:bit1;
  p1_event_addr_valid_q     :bit1;
  p1_dma_request_valid_q    :bit1;
  SPARE0                    :bit1;
  p1_min_indx_valid_q       :bit1;
  p1_max_indx_valid_q       :bit1;
  p1_indx_offset_valid_q    :bit1;
  p1_grbm_fifo_rdata_reg_id :bit5;
  p1_grbm_fifo_rdata_state  :bit3;
  p1_free_cnt_q             :bit6;
  p1_rbiu_di_fifo_we        :bit1;
  p1_rbiu_dr_fifo_we        :bit1;
  p1_rbiu_di_fifo_empty     :bit1;
  p1_rbiu_di_fifo_full      :bit1;
  p1_rbiu_dr_fifo_empty     :bit1;
  p1_rbiu_dr_fifo_full      :bit1;
 end;

 TWD_DEBUG_REG3=bitpacked record
  rbiu_spl_dr_valid        :bit1;
  SPARE0                   :bit1;
  pipe0_dr                 :bit1;
  pipe0_rtr                :bit1;
  pipe1_dr                 :bit1;
  pipe1_rtr                :bit1;
  wd_subdma_fifo_empty     :bit1;
  wd_subdma_fifo_full      :bit1;
  dma_buf_type_p0_q        :bit2;
  dma_zero_indices_p0_q    :bit1;
  dma_req_path_p3_q        :bit1;
  dma_not_eop_p1_q         :bit1;
  out_of_range_p4          :bit1;
  last_sub_dma_p3_q        :bit1;
  last_rdreq_of_sub_dma_p4 :bit1;
  WD_IA_dma_send_d         :bit1;
  WD_IA_dma_rtr            :bit1;
  WD_IA1_dma_send_d        :bit1;
  WD_IA1_dma_rtr           :bit1;
  last_inst_of_dma_p2      :bit1;
  last_sd_of_inst_p2       :bit1;
  last_sd_of_dma_p2        :bit1;
  SPARE1                   :bit1;
  WD_IA_dma_busy           :bit1;
  WD_IA1_dma_busy          :bit1;
  send_to_ia1_p3_q         :bit1;
  dma_wd_switch_on_eop_p3_q:bit1;
  pipe3_dr                 :bit1;
  pipe3_rtr                :bit1;
  wd_dma2draw_fifo_empty   :bit1;
  wd_dma2draw_fifo_full    :bit1;
 end;

 TWD_DEBUG_REG4=bitpacked record
  rbiu_spl_di_valid       :bit1;
  spl_rbiu_di_read        :bit1;
  rbiu_spl_p1_di_valid    :bit1;
  spl_rbiu_p1_di_read     :bit1;
  pipe0_dr                :bit1;
  pipe0_rtr               :bit1;
  pipe1_dr                :bit1;
  pipe1_rtr               :bit1;
  pipe2_dr                :bit1;
  pipe2_rtr               :bit1;
  pipe3_ld                :bit1;
  pipe3_rtr               :bit1;
  WD_IA_draw_send_d       :bit1;
  WD_IA_draw_rtr          :bit1;
  di_type_p0              :bit2;
  di_state_sel_p1_q       :bit3;
  di_wd_switch_on_eop_p1_q:bit1;
  rbiu_spl_pipe0_lockout  :bit1;
  last_inst_of_di_p2      :bit1;
  last_sd_of_inst_p2      :bit1;
  last_sd_of_di_p2        :bit1;
  not_eop_wait_p1_q       :bit1;
  not_eop_wait_q          :bit1;
  ext_event_wait_p1_q     :bit1;
  ext_event_wait_q        :bit1;
  WD_IA1_draw_send_d      :bit1;
  WD_IA1_draw_rtr         :bit1;
  send_to_ia1_q           :bit1;
  dual_ia_mode            :bit1;
 end;

 TWD_DEBUG_REG5=bitpacked record
  p1_rbiu_spl_dr_valid        :bit1;
  SPARE0                      :bit1;
  p1_pipe0_dr                 :bit1;
  p1_pipe0_rtr                :bit1;
  p1_pipe1_dr                 :bit1;
  p1_pipe1_rtr                :bit1;
  p1_wd_subdma_fifo_empty     :bit1;
  p1_wd_subdma_fifo_full      :bit1;
  p1_dma_buf_type_p0_q        :bit2;
  p1_dma_zero_indices_p0_q    :bit1;
  p1_dma_req_path_p3_q        :bit1;
  p1_dma_not_eop_p1_q         :bit1;
  p1_out_of_range_p4          :bit1;
  p1_last_sub_dma_p3_q        :bit1;
  p1_last_rdreq_of_sub_dma_p4 :bit1;
  p1_WD_IA_dma_send_d         :bit1;
  p1_WD_IA_dma_rtr            :bit1;
  p1_WD_IA1_dma_send_d        :bit1;
  p1_WD_IA1_dma_rtr           :bit1;
  p1_last_inst_of_dma_p2      :bit1;
  p1_last_sd_of_inst_p2       :bit1;
  p1_last_sd_of_dma_p2        :bit1;
  SPARE1                      :bit1;
  p1_WD_IA_dma_busy           :bit1;
  p1_WD_IA1_dma_busy          :bit1;
  p1_send_to_ia1_p3_q         :bit1;
  p1_dma_wd_switch_on_eop_p3_q:bit1;
  p1_pipe3_dr                 :bit1;
  p1_pipe3_rtr                :bit1;
  p1_wd_dma2draw_fifo_empty   :bit1;
  p1_wd_dma2draw_fifo_full    :bit1;
 end;

 TWD_DEBUG_REG6=bit32;

 TWD_DEBUG_REG7=bitpacked record
  SE0VGT_WD_thdgrp_send_in   :bit1;
  wd_arb_se0_input_fifo_re   :bit1;
  wd_arb_se0_input_fifo_empty:bit1;
  wd_arb_se0_input_fifo_full :bit1;
  SE1VGT_WD_thdgrp_send_in   :bit1;
  wd_arb_se1_input_fifo_re   :bit1;
  wd_arb_se1_input_fifo_empty:bit1;
  wd_arb_se1_input_fifo_full :bit1;
  SPARE1                     :bit4;
  SPARE2                     :bit4;
  te11_arb_state_q           :bit3;
  SPARE5                     :bit1;
  se0_thdgrp_is_event        :bit1;
  se0_thdgrp_eop             :bit1;
  se1_thdgrp_is_event        :bit1;
  se1_thdgrp_eop             :bit1;
  SPARE6                     :bit4;
  tfreq_arb_tgroup_rtr       :bit1;
  arb_tfreq_tgroup_rts       :bit1;
  arb_tfreq_tgroup_event     :bit1;
  te11_arb_busy              :bit1;
 end;

 TWD_DEBUG_REG8=bitpacked record
  pipe0_dr               :bit1;
  pipe1_dr               :bit1;
  pipe0_rtr              :bit1;
  pipe1_rtr              :bit1;
  tfreq_tg_fifo_empty    :bit1;
  tfreq_tg_fifo_full     :bit1;
  tf_data_fifo_busy_q    :bit1;
  tf_data_fifo_rtr_q     :bit1;
  tf_skid_fifo_empty     :bit1;
  tf_skid_fifo_full      :bit1;
  wd_tc_rdreq_rtr_q      :bit1;
  last_req_of_tg_p2      :bit1;
  se0spi_wd_hs_done_cnt_q:bit6;
  event_flag_p1_q        :bit1;
  null_flag_p1_q         :bit1;
  tf_data_fifo_cnt_q     :bit7;
  second_tf_ret_data_q   :bit1;
  first_req_of_tg_p1_q   :bit1;
  WD_TC_rdreq_send_out   :bit1;
  WD_TC_rdnfo_stall_out  :bit1;
  TC_WD_rdret_valid_in   :bit1;
 end;

 TWD_DEBUG_REG9=bitpacked record
  pipe0_dr                :bit1;
  pipec_tf_dr             :bit1;
  pipe2_dr                :bit1;
  event_or_null_flags_p0_q:bit1;
  pipe0_rtr               :bit1;
  pipe1_rtr               :bit1;
  pipec_tf_rtr            :bit1;
  pipe2_rtr               :bit1;
  ttp_patch_fifo_full     :bit1;
  ttp_patch_fifo_empty    :bit1;
  ttp_tf_fifo_empty       :bit1;
  SPARE0                  :bit5;
  tf_fetch_state_q        :bit3;
  last_patch_of_tg        :bit1;
  tf_pointer_p0_q         :bit4;
  dynamic_hs_p0_q         :bit1;
  first_fetch_of_tg_p0_q  :bit1;
  mem_is_even             :bit1;
  SPARE1                  :bit1;
  SPARE2                  :bit2;
  pipe4_dr                :bit1;
  pipe4_rtr               :bit1;
 end;

 TCB_BLEND_ALPHA=bit32;

 TCB_BLEND_GREEN=bit32;

 TCB_COLOR0_BASE=bit32;

 TCB_COLOR0_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR0_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR1_BASE=bit32;

 TCB_COLOR1_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR1_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR2_BASE=bit32;

 TCB_COLOR2_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR2_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR3_BASE=bit32;

 TCB_COLOR3_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR3_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR4_BASE=bit32;

 TCB_COLOR4_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR4_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR5_BASE=bit32;

 TCB_COLOR5_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR5_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR6_BASE=bit32;

 TCB_COLOR6_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR6_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_COLOR7_BASE=bit32;

 TCB_COLOR7_INFO=bitpacked record
  ENDIAN                   :bit2;
  FORMAT                   :bit5;
  LINEAR_GENERAL           :bit1;
  NUMBER_TYPE              :bit3;
  COMP_SWAP                :bit2;
  FAST_CLEAR               :bit1;
  COMPRESSION              :bit1;
  BLEND_CLAMP              :bit1;
  BLEND_BYPASS             :bit1;
  SIMPLE_FLOAT             :bit1;
  ROUND_MODE               :bit1;
  CMASK_IS_LINEAR          :bit1;
  BLEND_OPT_DONT_RD_DST    :bit3;
  BLEND_OPT_DISCARD_PIXEL  :bit3;
  FMASK_COMPRESSION_DISABLE:bit1;
  FMASK_COMPRESS_1FRAG_ONLY:bit1;
  DCC_ENABLE               :bit1;
  CMASK_ADDR_TYPE          :bit2;
  RESERVED0                :bit1;
 end;

 TCB_COLOR7_VIEW=bitpacked record
  SLICE_START:bit11;
  RESERVED0  :bit2;
  SLICE_MAX  :bit11;
  RESERVED1  :bit8;
 end;

 TCB_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE            :bit1;
  OVERWRITE_COMBINER_MRT_SHARING_DISABLE:bit1;
  OVERWRITE_COMBINER_WATERMARK          :bit5;
  RESERVED0                             :bit25;
 end;

 TCB_DEBUG_BUS_1=bitpacked record
  CB_BUSY                   :bit1;
  DB_CB_TILE_VALID_READY    :bit1;
  DB_CB_TILE_VALID_READYB   :bit1;
  DB_CB_TILE_VALIDB_READY   :bit1;
  DB_CB_TILE_VALIDB_READYB  :bit1;
  DB_CB_LQUAD_VALID_READY   :bit1;
  DB_CB_LQUAD_VALID_READYB  :bit1;
  DB_CB_LQUAD_VALIDB_READY  :bit1;
  DB_CB_LQUAD_VALIDB_READYB :bit1;
  CB_TAP_WRREQ_VALID_READY  :bit1;
  CB_TAP_WRREQ_VALID_READYB :bit1;
  CB_TAP_WRREQ_VALIDB_READY :bit1;
  CB_TAP_WRREQ_VALIDB_READYB:bit1;
  CB_TAP_RDREQ_VALID_READY  :bit1;
  CB_TAP_RDREQ_VALID_READYB :bit1;
  CB_TAP_RDREQ_VALIDB_READY :bit1;
  CB_TAP_RDREQ_VALIDB_READYB:bit1;
  CM_FC_TILE_VALID_READY    :bit1;
  CM_FC_TILE_VALID_READYB   :bit1;
  CM_FC_TILE_VALIDB_READY   :bit1;
  CM_FC_TILE_VALIDB_READYB  :bit1;
  FC_CLEAR_QUAD_VALID_READY :bit1;
  FC_CLEAR_QUAD_VALID_READYB:bit1;
  FC_CLEAR_QUAD_VALIDB_READY:bit1;
  RESERVED0                 :bit8;
 end;

 TCB_DEBUG_BUS_2=bitpacked record
  FC_CLEAR_QUAD_VALIDB_READYB    :bit1;
  FC_QUAD_RESIDENCY_STALL        :bit1;
  FC_CC_QUADFRAG_VALID_READY     :bit1;
  FC_CC_QUADFRAG_VALID_READYB    :bit1;
  FC_CC_QUADFRAG_VALIDB_READY    :bit1;
  FC_CC_QUADFRAG_VALIDB_READYB   :bit1;
  FOP_IN_VALID_READY             :bit1;
  FOP_IN_VALID_READYB            :bit1;
  FOP_IN_VALIDB_READY            :bit1;
  FOP_IN_VALIDB_READYB           :bit1;
  FOP_FMASK_RAW_STALL            :bit1;
  FOP_FMASK_BYPASS_STALL         :bit1;
  CC_IB_TB_FRAG_VALID_READY      :bit1;
  CC_IB_TB_FRAG_VALID_READYB     :bit1;
  CC_IB_TB_FRAG_VALIDB_READY     :bit1;
  CC_IB_TB_FRAG_VALIDB_READYB    :bit1;
  CC_IB_SR_FRAG_VALID_READY      :bit1;
  CC_IB_SR_FRAG_VALID_READYB     :bit1;
  CC_IB_SR_FRAG_VALIDB_READY     :bit1;
  CC_IB_SR_FRAG_VALIDB_READYB    :bit1;
  CC_RB_BC_EVENFRAG_VALID_READY  :bit1;
  CC_RB_BC_EVENFRAG_VALID_READYB :bit1;
  CC_RB_BC_EVENFRAG_VALIDB_READY :bit1;
  CC_RB_BC_EVENFRAG_VALIDB_READYB:bit1;
  RESERVED0                      :bit8;
 end;

 TCB_SHADER_MASK=bitpacked record
  OUTPUT0_ENABLE:bit4;
  OUTPUT1_ENABLE:bit4;
  OUTPUT2_ENABLE:bit4;
  OUTPUT3_ENABLE:bit4;
  OUTPUT4_ENABLE:bit4;
  OUTPUT5_ENABLE:bit4;
  OUTPUT6_ENABLE:bit4;
  OUTPUT7_ENABLE:bit4;
 end;

 TCB_TARGET_MASK=bitpacked record
  TARGET0_ENABLE:bit4;
  TARGET1_ENABLE:bit4;
  TARGET2_ENABLE:bit4;
  TARGET3_ENABLE:bit4;
  TARGET4_ENABLE:bit4;
  TARGET5_ENABLE:bit4;
  TARGET6_ENABLE:bit4;
  TARGET7_ENABLE:bit4;
 end;

 TCOMPUTE_PGM_HI=bitpacked record
  DATA     :bit8;
  INST_ATC :bit1;
  RESERVED0:bit23;
 end;

 TCOMPUTE_PGM_LO=bit32;

 TCOMPUTE_TBA_HI=bitpacked record
  DATA     :bit8;
  RESERVED0:bit24;
 end;

 TCOMPUTE_TBA_LO=bit32;

 TCOMPUTE_TMA_HI=bitpacked record
  DATA     :bit8;
  RESERVED0:bit24;
 end;

 TCOMPUTE_TMA_LO=bit32;

 TCPC_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_APPEND_DATA=bit32;

 TCP_CE_DE_COUNT=bit32;

 TCP_DE_CE_COUNT=bit32;

 TCP_DE_DE_COUNT=bit32;

 TCP_DFY_ADDR_HI=bit32;

 TCP_DFY_ADDR_LO=bitpacked record
  RESERVED0:bit5;
  ADDR_LO  :bit27;
 end;

 TCP_DFY_DATA_10=bit32;

 TCP_DFY_DATA_11=bit32;

 TCP_DFY_DATA_12=bit32;

 TCP_DFY_DATA_13=bit32;

 TCP_DFY_DATA_14=bit32;

 TCP_DFY_DATA_15=bit32;

 TCP_DRAW_OBJECT=bit32;

 TCP_ENDIAN_SWAP=bitpacked record
  ENDIAN_SWAP:bit2;
  RESERVED0  :bit30;
 end;

 TCP_HPD_STATUS0=bitpacked record
  QUEUE_STATE    :bit5;
  MAPPED_QUEUE   :bit3;
  QUEUE_AVAILABLE:bit8;
  RESERVED0      :bit16;
 end;

 TCP_HQD_IB_RPTR=bitpacked record
  CONSUMED_OFFSET:bit20;
  RESERVED0      :bit12;
 end;

 TCP_HQD_IQ_RPTR=bitpacked record
  OFFSET   :bit6;
  RESERVED0:bit26;
 end;

 TCP_HQD_OFFLOAD=bitpacked record
  DMA_OFFLOAD   :bit1;
  DMA_OFFLOAD_EN:bit1;
  RESERVED0     :bit2;
  EOP_OFFLOAD   :bit1;
  EOP_OFFLOAD_EN:bit1;
  RESERVED1     :bit26;
 end;

 TCP_HQD_PQ_BASE=bit32;

 TCP_HQD_PQ_RPTR=bit32;

 TCP_HQD_PQ_WPTR=bit32;

 TCP_HQD_QUANTUM=bitpacked record
  QUANTUM_EN      :bit1;
  RESERVED0       :bit3;
  QUANTUM_SCALE   :bit1;
  RESERVED1       :bit3;
  QUANTUM_DURATION:bit6;
  RESERVED2       :bit17;
  QUANTUM_ACTIVE  :bit1;
 end;

 TCP_IB1_BASE_HI=bitpacked record
  IB1_BASE_HI:bit16;
  RESERVED0  :bit16;
 end;

 TCP_IB1_BASE_LO=bitpacked record
  RESERVED0  :bit2;
  IB1_BASE_LO:bit30;
 end;

 TCP_IB2_BASE_HI=bitpacked record
  IB2_BASE_HI:bit16;
  RESERVED0  :bit16;
 end;

 TCP_IB2_BASE_LO=bitpacked record
  RESERVED0  :bit2;
  IB2_BASE_LO:bit30;
 end;

 TCP_MAX_CONTEXT=bitpacked record
  MAX_CONTEXT:bit3;
  RESERVED0  :bit29;
 end;

 TCP_MQD_CONTROL=bitpacked record
  VMID             :bit4;
  RESERVED0        :bit4;
  RESERVED1        :bit1;
  RESERVED2        :bit3;
  PROCESSING_MQD   :bit1;
  PROCESSING_MQD_EN:bit1;
  RESERVED3        :bit9;
  MQD_ATC          :bit1;
  CACHE_POLICY     :bit1;
  RESERVED4        :bit2;
  MTYPE            :bit2;
  RESERVED5        :bit3;
 end;

 TCP_RB0_BASE_HI=bitpacked record
  RB_BASE_HI:bit8;
  RESERVED0 :bit24;
 end;

 TCP_RB1_BASE_HI=bitpacked record
  RB_BASE_HI:bit8;
  RESERVED0 :bit24;
 end;

 TCP_ROQ_RB_STAT=bitpacked record
  ROQ_RPTR_PRIMARY:bit10;
  RESERVED0       :bit6;
  ROQ_WPTR_PRIMARY:bit10;
  RESERVED1       :bit6;
 end;

 TCP_STQ_WR_STAT=bitpacked record
  STQ_WPTR :bit10;
  RESERVED0:bit22;
 end;

 TCP_VIRT_STATUS=bit32;

 TCP_VMID_STATUS=bitpacked record
  PREEMPT_DE_STATUS:bit16;
  PREEMPT_CE_STATUS:bit16;
 end;

 TDB_DEPTH_CLEAR=bit32;

 TDB_DEPTH_SLICE=bitpacked record
  SLICE_TILE_MAX:bit22;
  RESERVED0     :bit10;
 end;

 TDB_FIFO_DEPTH1=bitpacked record
  MI_RDREQ_FIFO_DEPTH   :bit5;
  MI_WRREQ_FIFO_DEPTH   :bit5;
  MCC_DEPTH             :bit6;
  QC_DEPTH              :bit5;
  LTILE_PROBE_FIFO_DEPTH:bit8;
  RESERVED0             :bit3;
 end;

 TDB_FIFO_DEPTH2=bitpacked record
  EQUAD_FIFO_DEPTH   :bit8;
  ETILE_OP_FIFO_DEPTH:bit7;
  LQUAD_FIFO_DEPTH   :bit10;
  LTILE_OP_FIFO_DEPTH:bit7;
 end;

 TDB_Z_READ_BASE=bit32;

 TGDS_ATOM_READ0=bit32;

 TGDS_ATOM_READ1=bit32;

 TGDS_DEBUG_CNTL=bitpacked record
  GDS_DEBUG_INDX:bit5;
  UNUSED        :bit27;
 end;

 TGDS_DEBUG_DATA=bit32;

 TGDS_DEBUG_REG0=bitpacked record
  spare1               :bit6;
  write_buff_valid     :bit1;
  wr_pixel_nxt_ptr     :bit5;
  last_pixel_ptr       :bit1;
  cstate               :bit4;
  buff_write           :bit1;
  flush_request        :bit1;
  wr_buffer_wr_complete:bit1;
  wbuf_fifo_empty      :bit1;
  wbuf_fifo_full       :bit1;
  spare                :bit10;
 end;

 TGDS_DEBUG_REG1=bitpacked record
  tag_hit        :bit1;
  tag_miss       :bit1;
  pixel_addr     :bit15;
  pixel_vld      :bit1;
  data_ready     :bit1;
  awaiting_data  :bit1;
  addr_fifo_full :bit1;
  addr_fifo_empty:bit1;
  buffer_loaded  :bit1;
  buffer_invalid :bit1;
  spare          :bit8;
 end;

 TGDS_DEBUG_REG2=bitpacked record
  ds_full        :bit1;
  ds_credit_avail:bit1;
  ord_idx_free   :bit1;
  cmd_write      :bit1;
  app_sel        :bit4;
  req            :bit15;
  spare          :bit9;
 end;

 TGDS_DEBUG_REG3=bitpacked record
  pipe_num_busy :bit11;
  pipe0_busy_num:bit4;
  spare         :bit17;
 end;

 TGDS_DEBUG_REG4=bitpacked record
  gws_busy           :bit1;
  gws_req            :bit1;
  gws_out_stall      :bit1;
  cur_reso           :bit6;
  cur_reso_head_valid:bit1;
  cur_reso_head_dirty:bit1;
  cur_reso_head_flag :bit1;
  cur_reso_fed       :bit1;
  cur_reso_barrier   :bit1;
  cur_reso_flag      :bit1;
  cur_reso_cnt_gt0   :bit1;
  credit_cnt_gt0     :bit1;
  cmd_write          :bit1;
  grbm_gws_reso_wr   :bit1;
  grbm_gws_reso_rd   :bit1;
  ram_read_busy      :bit1;
  gws_bulkfree       :bit1;
  ram_gws_re         :bit1;
  ram_gws_we         :bit1;
  spare              :bit8;
 end;

 TGDS_DEBUG_REG5=bitpacked record
  write_dis         :bit1;
  dec_error         :bit1;
  alloc_opco_error  :bit1;
  dealloc_opco_error:bit1;
  wrap_opco_error   :bit1;
  spare             :bit3;
  error_ds_address  :bit14;
  spare1            :bit10;
 end;

 TGDS_DEBUG_REG6=bitpacked record
  oa_busy         :bit1;
  counters_enabled:bit4;
  counters_busy   :bit16;
  spare           :bit11;
 end;

 TGDS_EDC_OA_DED=bitpacked record
  ME0_GFXHP3D_PIX_DED:bit1;
  ME0_GFXHP3D_VTX_DED:bit1;
  ME0_CS_DED         :bit1;
  UNUSED0            :bit1;
  ME1_PIPE0_DED      :bit1;
  ME1_PIPE1_DED      :bit1;
  ME1_PIPE2_DED      :bit1;
  ME1_PIPE3_DED      :bit1;
  ME2_PIPE0_DED      :bit1;
  ME2_PIPE1_DED      :bit1;
  ME2_PIPE2_DED      :bit1;
  ME2_PIPE3_DED      :bit1;
  UNUSED1            :bit20;
 end;

 TGDS_GWS_RESET0=bitpacked record
  RESOURCE0_RESET :bit1;
  RESOURCE1_RESET :bit1;
  RESOURCE2_RESET :bit1;
  RESOURCE3_RESET :bit1;
  RESOURCE4_RESET :bit1;
  RESOURCE5_RESET :bit1;
  RESOURCE6_RESET :bit1;
  RESOURCE7_RESET :bit1;
  RESOURCE8_RESET :bit1;
  RESOURCE9_RESET :bit1;
  RESOURCE10_RESET:bit1;
  RESOURCE11_RESET:bit1;
  RESOURCE12_RESET:bit1;
  RESOURCE13_RESET:bit1;
  RESOURCE14_RESET:bit1;
  RESOURCE15_RESET:bit1;
  RESOURCE16_RESET:bit1;
  RESOURCE17_RESET:bit1;
  RESOURCE18_RESET:bit1;
  RESOURCE19_RESET:bit1;
  RESOURCE20_RESET:bit1;
  RESOURCE21_RESET:bit1;
  RESOURCE22_RESET:bit1;
  RESOURCE23_RESET:bit1;
  RESOURCE24_RESET:bit1;
  RESOURCE25_RESET:bit1;
  RESOURCE26_RESET:bit1;
  RESOURCE27_RESET:bit1;
  RESOURCE28_RESET:bit1;
  RESOURCE29_RESET:bit1;
  RESOURCE30_RESET:bit1;
  RESOURCE31_RESET:bit1;
 end;

 TGDS_GWS_RESET1=bitpacked record
  RESOURCE32_RESET:bit1;
  RESOURCE33_RESET:bit1;
  RESOURCE34_RESET:bit1;
  RESOURCE35_RESET:bit1;
  RESOURCE36_RESET:bit1;
  RESOURCE37_RESET:bit1;
  RESOURCE38_RESET:bit1;
  RESOURCE39_RESET:bit1;
  RESOURCE40_RESET:bit1;
  RESOURCE41_RESET:bit1;
  RESOURCE42_RESET:bit1;
  RESOURCE43_RESET:bit1;
  RESOURCE44_RESET:bit1;
  RESOURCE45_RESET:bit1;
  RESOURCE46_RESET:bit1;
  RESOURCE47_RESET:bit1;
  RESOURCE48_RESET:bit1;
  RESOURCE49_RESET:bit1;
  RESOURCE50_RESET:bit1;
  RESOURCE51_RESET:bit1;
  RESOURCE52_RESET:bit1;
  RESOURCE53_RESET:bit1;
  RESOURCE54_RESET:bit1;
  RESOURCE55_RESET:bit1;
  RESOURCE56_RESET:bit1;
  RESOURCE57_RESET:bit1;
  RESOURCE58_RESET:bit1;
  RESOURCE59_RESET:bit1;
  RESOURCE60_RESET:bit1;
  RESOURCE61_RESET:bit1;
  RESOURCE62_RESET:bit1;
  RESOURCE63_RESET:bit1;
 end;

 TGDS_GWS_VMID10=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID11=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID12=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID13=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID14=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_GWS_VMID15=bitpacked record
  BASE     :bit6;
  RESERVED0:bit10;
  SIZE     :bit7;
  RESERVED1:bit9;
 end;

 TGDS_OA_ADDRESS=bitpacked record
  DS_ADDRESS  :bit16;
  CRAWLER     :bit4;
  CRAWLER_TYPE:bit2;
  UNUSED      :bit8;
  NO_ALLOC    :bit1;
  ENABLE      :bit1;
 end;

 TGDS_OA_COUNTER=bit32;

 TGDS_VMID0_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID0_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID1_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID1_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID2_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID2_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID3_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID3_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID4_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID4_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID5_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID5_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID6_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID6_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID7_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID7_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID8_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID8_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID9_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID9_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGRBM_CAM_INDEX=bitpacked record
  CAM_INDEX:bit3;
  RESERVED0:bit29;
 end;

 TGRBM_GFX_INDEX=bitpacked record
  INSTANCE_INDEX           :bit8;
  SH_INDEX                 :bit8;
  SE_INDEX                 :bit8;
  RESERVED0                :bit5;
  SH_BROADCAST_WRITES      :bit1;
  INSTANCE_BROADCAST_WRITES:bit1;
  SE_BROADCAST_WRITES      :bit1;
 end;

 TGRBM_SKEW_CNTL=bitpacked record
  SKEW_TOP_THRESHOLD:bit6;
  SKEW_COUNT        :bit6;
  RESERVED0         :bit20;
 end;

 TGRBM_TRAP_ADDR=bitpacked record
  DATA     :bit16;
  RESERVED0:bit16;
 end;

 TIA_CNTL_STATUS=bitpacked record
  IA_BUSY        :bit1;
  IA_DMA_BUSY    :bit1;
  IA_DMA_REQ_BUSY:bit1;
  IA_GRP_BUSY    :bit1;
  IA_ADC_BUSY    :bit1;
  RESERVED0      :bit27;
 end;

 TPA_CL_VTE_CNTL=bitpacked record
  VPORT_X_SCALE_ENA :bit1;
  VPORT_X_OFFSET_ENA:bit1;
  VPORT_Y_SCALE_ENA :bit1;
  VPORT_Y_OFFSET_ENA:bit1;
  VPORT_Z_SCALE_ENA :bit1;
  VPORT_Z_OFFSET_ENA:bit1;
  RESERVED0         :bit2;
  VTX_XY_FMT        :bit1;
  VTX_Z_FMT         :bit1;
  VTX_W0_FMT        :bit1;
  PERFCOUNTER_REF   :bit1;
  RESERVED1         :bit20;
 end;

 TPA_SC_EDGERULE=bitpacked record
  ER_TRI    :bit4;
  ER_POINT  :bit4;
  ER_RECT   :bit4;
  ER_LINE_LR:bit6;
  ER_LINE_RL:bit6;
  ER_LINE_TB:bit4;
  ER_LINE_BT:bit4;
 end;

 TPA_SU_VTX_CNTL=bitpacked record
  PIX_CENTER:bit1;
  ROUND_MODE:bit2;
  QUANT_MODE:bit3;
  RESERVED0 :bit26;
 end;

 TSPI_BARYC_CNTL=bitpacked record
  PERSP_CENTER_CNTL   :bit1;
  RESERVED0           :bit3;
  PERSP_CENTROID_CNTL :bit1;
  RESERVED1           :bit3;
  LINEAR_CENTER_CNTL  :bit1;
  RESERVED2           :bit3;
  LINEAR_CENTROID_CNTL:bit1;
  RESERVED3           :bit3;
  POS_FLOAT_LOCATION  :bit2;
  RESERVED4           :bit2;
  POS_FLOAT_ULC       :bit1;
  RESERVED5           :bit3;
  FRONT_FACE_ALL_BITS :bit1;
  RESERVED6           :bit7;
 end;

 TSPI_DEBUG_BUSY=bitpacked record
  LS_BUSY         :bit1;
  HS_BUSY         :bit1;
  ES_BUSY         :bit1;
  GS_BUSY         :bit1;
  VS_BUSY         :bit1;
  PS0_BUSY        :bit1;
  PS1_BUSY        :bit1;
  CSG_BUSY        :bit1;
  CS0_BUSY        :bit1;
  CS1_BUSY        :bit1;
  CS2_BUSY        :bit1;
  CS3_BUSY        :bit1;
  CS4_BUSY        :bit1;
  CS5_BUSY        :bit1;
  CS6_BUSY        :bit1;
  CS7_BUSY        :bit1;
  LDS_WR_CTL0_BUSY:bit1;
  LDS_WR_CTL1_BUSY:bit1;
  RSRC_ALLOC0_BUSY:bit1;
  RSRC_ALLOC1_BUSY:bit1;
  PC_DEALLOC_BUSY :bit1;
  EVENT_CLCTR_BUSY:bit1;
  GRBM_BUSY       :bit1;
  SPIS_BUSY       :bit1;
  RESERVED0       :bit8;
 end;

 TSPI_DEBUG_CNTL=bitpacked record
  DEBUG_GRBM_OVERRIDE  :bit1;
  DEBUG_THREAD_TYPE_SEL:bit3;
  DEBUG_GROUP_SEL      :bit6;
  DEBUG_SIMD_SEL       :bit6;
  DEBUG_SH_SEL         :bit1;
  SPI_ECO_SPARE_0      :bit1;
  SPI_ECO_SPARE_1      :bit1;
  SPI_ECO_SPARE_2      :bit1;
  SPI_ECO_SPARE_3      :bit1;
  SPI_ECO_SPARE_4      :bit1;
  SPI_ECO_SPARE_5      :bit1;
  SPI_ECO_SPARE_6      :bit1;
  SPI_ECO_SPARE_7      :bit1;
  DEBUG_PIPE_SEL       :bit3;
  RESERVED0            :bit3;
  DEBUG_REG_EN         :bit1;
 end;

 TSPI_DEBUG_READ=bitpacked record
  DATA     :bit24;
  RESERVED0:bit8;
 end;

 TSPI_LB_CU_MASK=bitpacked record
  CU_MASK  :bit16;
  RESERVED0:bit16;
 end;

 TTA_DEBUG_INDEX=bitpacked record
  INDEX    :bit5;
  RESERVED0:bit27;
 end;

 TTCC_REDUNDANCY=bitpacked record
  MC_SEL0  :bit1;
  MC_SEL1  :bit1;
  RESERVED0:bit30;
 end;

 TTCP_INVALIDATE=bitpacked record
  START    :bit1;
  RESERVED0:bit31;
 end;

 TTD_DEBUG_INDEX=bitpacked record
  INDEX    :bit5;
  RESERVED0:bit27;
 end;

 TVGT_DEBUG_CNTL=bitpacked record
  VGT_DEBUG_INDX     :bit6;
  VGT_DEBUG_SEL_BUS_B:bit1;
  RESERVED0          :bit25;
 end;

 TVGT_DEBUG_DATA=bit32;

 TVGT_DEBUG_REG0=bitpacked record
  vgt_busy_extended     :bit1;
  SPARE9                :bit1;
  vgt_busy              :bit1;
  SPARE8                :bit1;
  SPARE7                :bit1;
  SPARE6                :bit1;
  SPARE5                :bit1;
  SPARE4                :bit1;
  pi_busy               :bit1;
  vr_pi_busy            :bit1;
  pt_pi_busy            :bit1;
  te_pi_busy            :bit1;
  gs_busy               :bit1;
  rcm_busy              :bit1;
  tm_busy               :bit1;
  cm_busy               :bit1;
  gog_busy              :bit1;
  frmt_busy             :bit1;
  SPARE10               :bit1;
  te11_pi_busy          :bit1;
  SPARE3                :bit1;
  combined_out_busy     :bit1;
  spi_vs_interfaces_busy:bit1;
  pa_interfaces_busy    :bit1;
  reg_clk_busy          :bit1;
  SPARE2                :bit1;
  core_clk_busy         :bit1;
  gs_clk_busy           :bit1;
  SPARE1                :bit1;
  sclk_core_vld         :bit1;
  sclk_gs_vld           :bit1;
  SPARE0                :bit1;
 end;

 TVGT_DEBUG_REG1=bitpacked record
  SPARE9            :bit1;
  SPARE8            :bit1;
  SPARE7            :bit1;
  SPARE6            :bit1;
  SPARE5            :bit1;
  SPARE4            :bit1;
  SPARE3            :bit1;
  SPARE2            :bit1;
  SPARE1            :bit1;
  SPARE0            :bit1;
  pi_vr_valid       :bit1;
  vr_pi_read        :bit1;
  pi_pt_valid       :bit1;
  pt_pi_read        :bit1;
  pi_te_valid       :bit1;
  te_grp_read       :bit1;
  vr_out_indx_valid :bit1;
  SPARE12           :bit1;
  vr_out_prim_valid :bit1;
  SPARE11           :bit1;
  pt_out_indx_valid :bit1;
  SPARE10           :bit1;
  pt_out_prim_valid :bit1;
  SPARE23           :bit1;
  te_out_data_valid :bit1;
  SPARE25           :bit1;
  pi_gs_valid       :bit1;
  gs_pi_read        :bit1;
  gog_out_indx_valid:bit1;
  out_indx_read     :bit1;
  gog_out_prim_valid:bit1;
  out_prim_read     :bit1;
 end;

 TVGT_DEBUG_REG2=bitpacked record
  hs_grp_busy           :bit1;
  hs_noif_busy          :bit1;
  tfmmIsBusy            :bit1;
  lsVertIfBusy_0        :bit1;
  te11_hs_tess_input_rtr:bit1;
  lsWaveIfBusy_0        :bit1;
  hs_te11_tess_input_rts:bit1;
  grpModBusy            :bit1;
  lsVertFifoEmpty       :bit1;
  lsWaveFifoEmpty       :bit1;
  hsVertFifoEmpty       :bit1;
  hsWaveFifoEmpty       :bit1;
  hsInputFifoEmpty      :bit1;
  hsTifFifoEmpty        :bit1;
  lsVertFifoFull        :bit1;
  lsWaveFifoFull        :bit1;
  hsVertFifoFull        :bit1;
  hsWaveFifoFull        :bit1;
  hsInputFifoFull       :bit1;
  hsTifFifoFull         :bit1;
  p0_rtr                :bit1;
  p1_rtr                :bit1;
  p0_dr                 :bit1;
  p1_dr                 :bit1;
  p0_rts                :bit1;
  p1_rts                :bit1;
  ls_sh_id              :bit1;
  lsFwaveFlag           :bit1;
  lsWaveSendFlush       :bit1;
  SPARE                 :bit3;
 end;

 TVGT_DEBUG_REG3=bitpacked record
  lsTgRelInd  :bit12;
  lsWaveRelInd:bit6;
  lsPatchCnt  :bit8;
  hsWaveRelInd:bit6;
 end;

 TVGT_DEBUG_REG4=bitpacked record
  hsPatchCnt     :bit8;
  hsPrimId_15_0  :bit16;
  hsCpCnt        :bit5;
  hsWaveSendFlush:bit1;
  hsFwaveFlag    :bit1;
  SPARE          :bit1;
 end;

 TVGT_DEBUG_REG5=bitpacked record
  SPARE4           :bit3;
  hsWaveCreditCnt_0:bit5;
  SPARE3           :bit3;
  hsVertCreditCnt_0:bit5;
  SPARE2           :bit3;
  lsWaveCreditCnt_0:bit5;
  SPARE1           :bit3;
  lsVertCreditCnt_0:bit5;
 end;

 TVGT_DEBUG_REG6=bitpacked record
  debug_BASE:bit16;
  debug_SIZE:bit16;
 end;

 TVGT_DEBUG_REG7=bitpacked record
  debug_tfmmFifoEmpty:bit1;
  debug_tfmmFifoFull :bit1;
  hs_pipe0_dr        :bit1;
  hs_pipe0_rtr       :bit1;
  hs_pipe1_rtr       :bit1;
  SPARE              :bit11;
  TF_addr            :bit16;
 end;

 TVGT_DEBUG_REG8=bitpacked record
  rcm_busy_q            :bit1;
  rcm_noif_busy_q       :bit1;
  r1_inst_rtr           :bit1;
  spi_gsprim_fifo_busy_q:bit1;
  spi_esvert_fifo_busy_q:bit1;
  gs_tbl_valid_r3_q     :bit1;
  valid_r0_q            :bit1;
  valid_r1_q            :bit1;
  valid_r2              :bit1;
  valid_r2_q            :bit1;
  r0_rtr                :bit1;
  r1_rtr                :bit1;
  r2_indx_rtr           :bit1;
  r2_rtr                :bit1;
  es_gs_rtr             :bit1;
  gs_event_fifo_rtr     :bit1;
  tm_rcm_gs_event_rtr   :bit1;
  gs_tbl_r3_rtr         :bit1;
  prim_skid_fifo_empty  :bit1;
  VGT_SPI_gsprim_rtr_q  :bit1;
  tm_rcm_gs_tbl_rtr     :bit1;
  tm_rcm_es_tbl_rtr     :bit1;
  VGT_SPI_esvert_rtr_q  :bit1;
  r2_no_bp_rtr          :bit1;
  hold_for_es_flush     :bit1;
  gs_event_fifo_empty   :bit1;
  gsprim_buff_empty_q   :bit1;
  gsprim_buff_full_q    :bit1;
  te_prim_fifo_empty    :bit1;
  te_prim_fifo_full     :bit1;
  te_vert_fifo_empty    :bit1;
  te_vert_fifo_full     :bit1;
 end;

 TVGT_DEBUG_REG9=bitpacked record
  indices_to_send_r2_q           :bit2;
  valid_indices_r3               :bit1;
  gs_eov_r3                      :bit1;
  eop_indx_r3                    :bit1;
  eop_prim_r3                    :bit1;
  es_eov_r3                      :bit1;
  es_tbl_state_r3_q_0            :bit1;
  pending_es_send_r3_q           :bit1;
  pending_es_flush_r3            :bit1;
  gs_tbl_num_es_per_gs_r3_q_not_0:bit1;
  gs_tbl_prim_cnt_r3_q           :bit7;
  gs_tbl_eop_r3_q                :bit1;
  gs_tbl_state_r3_q              :bit3;
  gs_pending_state_r3_q          :bit1;
  invalidate_rb_roll_over_q      :bit1;
  gs_instancing_state_q          :bit1;
  es_per_gs_vert_cnt_r3_q_not_0  :bit1;
  gs_prim_per_es_ctr_r3_q_not_0  :bit1;
  pre_r0_rtr                     :bit1;
  valid_r3_q                     :bit1;
  valid_pre_r0_q                 :bit1;
  SPARE0                         :bit1;
  off_chip_hs_r2_q               :bit1;
 end;

 TVGT_GROUP_DECR=bitpacked record
  DECR     :bit4;
  RESERVED0:bit28;
 end;

 TVGT_IMMED_DATA=bit32;

 TVGT_INDEX_TYPE=bitpacked record
  INDEX_TYPE:bit2;
  RESERVED0 :bit30;
 end;

 TVGT_SYS_CONFIG=bitpacked record
  DUAL_CORE_EN            :bit1;
  MAX_LS_HS_THDGRP        :bit6;
  ADC_EVENT_FILTER_DISABLE:bit1;
  RESERVED0               :bit24;
 end;

 TVGT_VTX_CNT_EN=bitpacked record
  VTX_CNT_EN:bit1;
  RESERVED0 :bit31;
 end;

 TWD_CNTL_STATUS=bitpacked record
  WD_BUSY        :bit1;
  WD_SPL_DMA_BUSY:bit1;
  WD_SPL_DI_BUSY :bit1;
  WD_ADC_BUSY    :bit1;
  RESERVED0      :bit28;
 end;

 TWD_DEBUG_REG10=bitpacked record
  ttp_pd_patch_rts          :bit1;
  ttp_pd_is_event           :bit1;
  ttp_pd_eopg               :bit1;
  ttp_pd_eop                :bit1;
  pipe0_dr                  :bit1;
  pipe1_dr                  :bit1;
  pipe0_rtr                 :bit1;
  pipe1_rtr                 :bit1;
  donut_en_p1_q             :bit1;
  donut_se_switch_p2        :bit1;
  patch_se_switch_p2        :bit1;
  last_donut_switch_p2      :bit1;
  last_donut_of_patch_p2    :bit1;
  is_event_p1_q             :bit1;
  eopg_p1_q                 :bit1;
  eop_p1_q                  :bit1;
  patch_accum_q             :bit8;
  wd_te11_out_se0_fifo_full :bit1;
  wd_te11_out_se0_fifo_empty:bit1;
  wd_te11_out_se1_fifo_full :bit1;
  wd_te11_out_se1_fifo_empty:bit1;
  wd_te11_out_se2_fifo_full :bit1;
  wd_te11_out_se2_fifo_empty:bit1;
  wd_te11_out_se3_fifo_full :bit1;
  wd_te11_out_se3_fifo_empty:bit1;
 end;

 TCB_COLOR0_CMASK=bit32;

 TCB_COLOR0_FMASK=bit32;

 TCB_COLOR0_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR0_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR1_CMASK=bit32;

 TCB_COLOR1_FMASK=bit32;

 TCB_COLOR1_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR1_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR2_CMASK=bit32;

 TCB_COLOR2_FMASK=bit32;

 TCB_COLOR2_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR2_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR3_CMASK=bit32;

 TCB_COLOR3_FMASK=bit32;

 TCB_COLOR3_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR3_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR4_CMASK=bit32;

 TCB_COLOR4_FMASK=bit32;

 TCB_COLOR4_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR4_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR5_CMASK=bit32;

 TCB_COLOR5_FMASK=bit32;

 TCB_COLOR5_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR5_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR6_CMASK=bit32;

 TCB_COLOR6_FMASK=bit32;

 TCB_COLOR6_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR6_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR7_CMASK=bit32;

 TCB_COLOR7_FMASK=bit32;

 TCB_COLOR7_PITCH=bitpacked record
  TILE_MAX      :bit11;
  RESERVED0     :bit9;
  FMASK_TILE_MAX:bit11;
  RESERVED1     :bit1;
 end;

 TCB_COLOR7_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_DEBUG_BUS_13=bitpacked record
  FC_PF_FC_KEYID_RDLAT_FIFO_FULL                :bit1;
  FC_DOC_QTILE_CAM_MISS                         :bit1;
  FC_DOC_QTILE_CAM_HIT                          :bit1;
  FC_DOC_CLINE_CAM_MISS                         :bit1;
  FC_DOC_CLINE_CAM_HIT                          :bit1;
  FC_DOC_OVERWROTE_1_SECTOR                     :bit1;
  FC_DOC_OVERWROTE_2_SECTORS                    :bit1;
  FC_DOC_OVERWROTE_3_SECTORS                    :bit1;
  FC_DOC_OVERWROTE_4_SECTORS                    :bit1;
  FC_PF_DCC_CACHE_HIT                           :bit1;
  FC_PF_DCC_CACHE_TAG_MISS                      :bit1;
  FC_PF_DCC_CACHE_SECTOR_MISS                   :bit1;
  FC_PF_DCC_CACHE_REEVICTION_STALL              :bit1;
  FC_PF_DCC_CACHE_EVICT_NONZERO_INFLIGHT_STALL  :bit1;
  FC_PF_DCC_CACHE_REPLACE_PENDING_EVICT_STALL   :bit1;
  FC_PF_DCC_CACHE_INFLIGHT_COUNTER_MAXIMUM_STALL:bit1;
  FC_PF_DCC_CACHE_READ_OUTPUT_STALL             :bit1;
  FC_PF_DCC_CACHE_WRITE_OUTPUT_STALL            :bit1;
  FC_PF_DCC_CACHE_ACK_OUTPUT_STALL              :bit1;
  FC_PF_DCC_CACHE_STALL                         :bit1;
  FC_PF_DCC_CACHE_FLUSH                         :bit1;
  FC_PF_DCC_CACHE_SECTORS_FLUSHED               :bit1;
  FC_PF_DCC_CACHE_DIRTY_SECTORS_FLUSHED         :bit1;
  FC_PF_DCC_CACHE_TAGS_FLUSHED                  :bit1;
  RESERVED0                                     :bit8;
 end;

 TCB_DEBUG_BUS_14=bitpacked record
  FC_MC_DCC_WRITE_REQUESTS_IN_FLIGHT:bit11;
  FC_MC_DCC_READ_REQUESTS_IN_FLIGHT :bit11;
  CC_PF_DCC_BEYOND_TILE_SPLIT       :bit1;
  CC_PF_DCC_RDREQ_STALL             :bit1;
  RESERVED0                         :bit8;
 end;

 TCB_DEBUG_BUS_15=bitpacked record
  CC_PF_DCC_COMPRESS_RATIO_2TO1:bit3;
  CC_PF_DCC_COMPRESS_RATIO_4TO1:bit2;
  CC_PF_DCC_COMPRESS_RATIO_4TO2:bit2;
  CC_PF_DCC_COMPRESS_RATIO_4TO3:bit2;
  CC_PF_DCC_COMPRESS_RATIO_6TO1:bit2;
  CC_PF_DCC_COMPRESS_RATIO_6TO2:bit2;
  CC_PF_DCC_COMPRESS_RATIO_6TO3:bit2;
  CC_PF_DCC_COMPRESS_RATIO_6TO4:bit2;
  CC_PF_DCC_COMPRESS_RATIO_6TO5:bit2;
  RESERVED0                    :bit13;
 end;

 TCB_DEBUG_BUS_16=bitpacked record
  CC_PF_DCC_COMPRESS_RATIO_8TO1:bit1;
  CC_PF_DCC_COMPRESS_RATIO_8TO2:bit1;
  CC_PF_DCC_COMPRESS_RATIO_8TO3:bit1;
  CC_PF_DCC_COMPRESS_RATIO_8TO4:bit1;
  CC_PF_DCC_COMPRESS_RATIO_8TO5:bit1;
  CC_PF_DCC_COMPRESS_RATIO_8TO6:bit1;
  CC_PF_DCC_COMPRESS_RATIO_8TO7:bit1;
  RESERVED0                    :bit25;
 end;

 TCB_DEBUG_BUS_17=bitpacked record
  TILE_INTFC_BUSY   :bit1;
  MU_BUSY           :bit1;
  TQ_BUSY           :bit1;
  AC_BUSY           :bit1;
  CRW_BUSY          :bit1;
  CACHE_CTRL_BUSY   :bit1;
  MC_WR_PENDING     :bit1;
  FC_WR_PENDING     :bit1;
  FC_RD_PENDING     :bit1;
  EVICT_PENDING     :bit1;
  LAST_RD_ARB_WINNER:bit1;
  MU_STATE          :bit8;
  RESERVED0         :bit13;
 end;

 TCB_DEBUG_BUS_18=bitpacked record
  TILE_RETIREMENT_BUSY     :bit1;
  FOP_BUSY                 :bit1;
  CLEAR_BUSY               :bit1;
  LAT_BUSY                 :bit1;
  CACHE_CTL_BUSY           :bit1;
  ADDR_BUSY                :bit1;
  MERGE_BUSY               :bit1;
  QUAD_BUSY                :bit1;
  TILE_BUSY                :bit1;
  DCC_BUSY                 :bit1;
  DOC_BUSY                 :bit1;
  DAG_BUSY                 :bit1;
  DOC_STALL                :bit1;
  DOC_QT_CAM_FULL          :bit1;
  DOC_CL_CAM_FULL          :bit1;
  DOC_QUAD_PTR_FIFO_FULL   :bit1;
  DOC_SECTOR_MASK_FIFO_FULL:bit1;
  DCS_READ_WINNER_LAST     :bit1;
  DCS_READ_EV_PENDING      :bit1;
  DCS_WRITE_CC_PENDING     :bit1;
  DCS_READ_CC_PENDING      :bit1;
  DCS_WRITE_MC_PENDING     :bit1;
  RESERVED0                :bit10;
 end;

 TCB_DEBUG_BUS_19=bitpacked record
  SURF_SYNC_STATE    :bit2;
  SURF_SYNC_START    :bit1;
  SF_BUSY            :bit1;
  CS_BUSY            :bit1;
  RB_BUSY            :bit1;
  DS_BUSY            :bit1;
  TB_BUSY            :bit1;
  IB_BUSY            :bit1;
  DRR_BUSY           :bit1;
  DF_BUSY            :bit1;
  DD_BUSY            :bit1;
  DC_BUSY            :bit1;
  DK_BUSY            :bit1;
  DF_SKID_FIFO_EMPTY :bit1;
  DF_CLEAR_FIFO_EMPTY:bit1;
  DD_READY           :bit1;
  DC_FIFO_FULL       :bit1;
  DC_READY           :bit1;
  RESERVED0          :bit13;
 end;

 TCB_DEBUG_BUS_20=bitpacked record
  MC_RDREQ_CREDITS     :bit6;
  MC_WRREQ_CREDITS     :bit6;
  CC_RDREQ_HAD_ITS_TURN:bit1;
  FC_RDREQ_HAD_ITS_TURN:bit1;
  CM_RDREQ_HAD_ITS_TURN:bit1;
  RESERVED0            :bit1;
  CC_WRREQ_HAD_ITS_TURN:bit1;
  FC_WRREQ_HAD_ITS_TURN:bit1;
  CM_WRREQ_HAD_ITS_TURN:bit1;
  RESERVED1            :bit1;
  CC_WRREQ_FIFO_EMPTY  :bit1;
  FC_WRREQ_FIFO_EMPTY  :bit1;
  CM_WRREQ_FIFO_EMPTY  :bit1;
  DCC_WRREQ_FIFO_EMPTY :bit1;
  RESERVED2            :bit8;
 end;

 TCB_DEBUG_BUS_21=bitpacked record
  CM_BUSY      :bit1;
  FC_BUSY      :bit1;
  CC_BUSY      :bit1;
  BB_BUSY      :bit1;
  MA_BUSY      :bit1;
  CORE_SCLK_VLD:bit1;
  REG_SCLK1_VLD:bit1;
  REG_SCLK0_VLD:bit1;
  RESERVED0    :bit24;
 end;

 TCB_DEBUG_BUS_22=bitpacked record
  OUTSTANDING_MC_READS :bit12;
  OUTSTANDING_MC_WRITES:bit12;
  RESERVED0            :bit8;
 end;

 TCB_HW_CONTROL_1=bitpacked record
  CM_CACHE_NUM_TAGS :bit5;
  FC_CACHE_NUM_TAGS :bit6;
  CC_CACHE_NUM_TAGS :bit6;
  CM_TILE_FIFO_DEPTH:bit9;
  CHICKEN_BITS      :bit6;
 end;

 TCB_HW_CONTROL_2=bitpacked record
  CC_EVEN_ODD_FIFO_DEPTH     :bit8;
  FC_RDLAT_TILE_FIFO_DEPTH   :bit7;
  FC_RDLAT_QUAD_FIFO_DEPTH   :bit8;
  RESERVED0                  :bit1;
  DRR_ASSUMED_FIFO_DEPTH_DIV8:bit4;
  CHICKEN_BITS               :bit4;
 end;

 TCB_HW_CONTROL_3=bitpacked record
  DISABLE_SLOW_MODE_EMPTY_HALF_QUAD_KILL           :bit1;
  RAM_ADDRESS_CONFLICTS_DISALLOWED                 :bit1;
  DISABLE_FAST_CLEAR_FETCH_OPT                     :bit1;
  DISABLE_QUAD_MARKER_DROP_STOP                    :bit1;
  DISABLE_OVERWRITE_COMBINER_CAM_CLR               :bit1;
  DISABLE_CC_CACHE_OVWR_STATUS_ACCUM               :bit1;
  DISABLE_CC_CACHE_OVWR_KEY_MOD                    :bit1;
  DISABLE_CC_CACHE_PANIC_GATING                    :bit1;
  DISABLE_OVERWRITE_COMBINER_TARGET_MASK_VALIDATION:bit1;
  SPLIT_ALL_FAST_MODE_TRANSFERS                    :bit1;
  DISABLE_SHADER_BLEND_OPTS                        :bit1;
  DISABLE_CMASK_LAST_QUAD_INSERTION                :bit1;
  DISABLE_ROP3_FIXES_OF_BUG_511967                 :bit1;
  RESERVED0                                        :bit19;
 end;

 TCOMPUTE_NOWHERE=bit32;

 TCOMPUTE_START_X=bit32;

 TCOMPUTE_START_Y=bit32;

 TCOMPUTE_START_Z=bit32;

 TCPC_INT_CNTX_ID=bitpacked record
  CNTX_ID  :bit28;
  QUEUE_ID :bit3;
  RESERVED0:bit1;
 end;

 TCP_CE_IB1_BUFSZ=bitpacked record
  IB1_BUFSZ:bit20;
  RESERVED0:bit12;
 end;

 TCP_CE_IB2_BUFSZ=bitpacked record
  IB2_BUFSZ:bit20;
  RESERVED0:bit12;
 end;

 TCP_CE_RB_OFFSET=bitpacked record
  RB_OFFSET:bit20;
  RESERVED0:bit12;
 end;

 TCP_COHER_STATUS=bitpacked record
  MATCHING_GFX_CNTX:bit8;
  RESERVED0        :bit16;
  MEID             :bit2;
  RESERVED1        :bit4;
  PHASE1_STATUS    :bit1;
  STATUS           :bit1;
 end;

 TCP_CONTEXT_CNTL=bitpacked record
  ME0PIPE0_MAX_WD_CNTX  :bit3;
  RESERVED0             :bit1;
  ME0PIPE0_MAX_PIPE_CNTX:bit3;
  RESERVED1             :bit9;
  ME0PIPE1_MAX_WD_CNTX  :bit3;
  RESERVED2             :bit1;
  ME0PIPE1_MAX_PIPE_CNTX:bit3;
  RESERVED3             :bit9;
 end;

 TCP_HQD_EOP_RPTR=bitpacked record
  RPTR             :bit13;
  RESERVED0        :bit17;
  RPTR_EQ_CSMD_WPTR:bit1;
  INIT_FETCHER     :bit1;
 end;

 TCP_HQD_EOP_WPTR=bitpacked record
  WPTR     :bit13;
  RESERVED0:bit3;
  EOP_AVAIL:bit13;
  RESERVED1:bit3;
 end;

 TCP_HQD_IQ_TIMER=bitpacked record
  WAIT_TIME       :bit8;
  RETRY_TYPE      :bit3;
  IMMEDIATE_EXPIRE:bit1;
  INTERRUPT_TYPE  :bit2;
  CLOCK_COUNT     :bit2;
  INTERRUPT_SIZE  :bit6;
  QUANTUM_TIMER   :bit1;
  IQ_ATC          :bit1;
  CACHE_POLICY    :bit1;
  RESERVED0       :bit2;
  MTYPE           :bit2;
  PROCESS_IQ_EN   :bit1;
  PROCESSING_IQ   :bit1;
  ACTIVE          :bit1;
 end;

 TCP_HQD_MSG_TYPE=bitpacked record
  ACTION    :bit3;
  RESERVED0 :bit1;
  SAVE_STATE:bit3;
  RESERVED1 :bit25;
 end;

 TCP_HQD_SEMA_CMD=bitpacked record
  RETRY    :bit1;
  RESULT   :bit2;
  RESERVED0:bit29;
 end;

 TCP_MEM_SLP_CNTL=bitpacked record
  CP_MEM_LS_EN          :bit1;
  CP_MEM_DS_EN          :bit1;
  RESERVED              :bit5;
  CP_LS_DS_BUSY_OVERRIDE:bit1;
  CP_MEM_LS_ON_DELAY    :bit8;
  CP_MEM_LS_OFF_DELAY   :bit8;
  RESERVED1             :bit8;
 end;

 TCP_PERFMON_CNTL=bitpacked record
  PERFMON_STATE        :bit4;
  SPM_PERFMON_STATE    :bit4;
  PERFMON_ENABLE_MODE  :bit2;
  PERFMON_SAMPLE_ENABLE:bit1;
  RESERVED0            :bit21;
 end;

 TCP_ROQ_IB1_STAT=bitpacked record
  ROQ_RPTR_INDIRECT1:bit10;
  RESERVED0         :bit6;
  ROQ_WPTR_INDIRECT1:bit10;
  RESERVED1         :bit6;
 end;

 TCP_ROQ_IB2_STAT=bitpacked record
  ROQ_RPTR_INDIRECT2:bit10;
  RESERVED0         :bit6;
  ROQ_WPTR_INDIRECT2:bit10;
  RESERVED1         :bit6;
 end;

 TCP_SCRATCH_DATA=bit32;

 TCP_STRMOUT_CNTL=bitpacked record
  OFFSET_UPDATE_DONE:bit1;
  RESERVED0         :bit31;
 end;

 TCP_VMID_PREEMPT=bitpacked record
  PREEMPT_REQUEST:bit16;
  VIRT_COMMAND   :bit4;
  RESERVED0      :bit12;
 end;

 TDB_CREDIT_LIMIT=bitpacked record
  DB_SC_TILE_CREDITS :bit5;
  DB_SC_QUAD_CREDITS :bit5;
  DB_CB_LQUAD_CREDITS:bit3;
  RESERVED0          :bit11;
  DB_CB_TILE_CREDITS :bit7;
  RESERVED1          :bit1;
 end;

 TDB_READ_DEBUG_0=bit32;

 TDB_READ_DEBUG_1=bit32;

 TDB_READ_DEBUG_2=bit32;

 TDB_READ_DEBUG_3=bit32;

 TDB_READ_DEBUG_4=bit32;

 TDB_READ_DEBUG_5=bit32;

 TDB_READ_DEBUG_6=bit32;

 TDB_READ_DEBUG_7=bit32;

 TDB_READ_DEBUG_8=bit32;

 TDB_READ_DEBUG_9=bit32;

 TDB_READ_DEBUG_A=bit32;

 TDB_READ_DEBUG_B=bit32;

 TDB_READ_DEBUG_C=bit32;

 TDB_READ_DEBUG_D=bit32;

 TDB_READ_DEBUG_E=bit32;

 TDB_READ_DEBUG_F=bit32;

 TDB_RING_CONTROL=bitpacked record
  COUNTER_CONTROL:bit2;
  RESERVED0      :bit30;
 end;

 TDB_STENCIL_INFO=bitpacked record
  FORMAT              :bit1;
  RESERVED0           :bit12;
  TILE_SPLIT          :bit3;
  RESERVED1           :bit4;
  TILE_MODE_INDEX     :bit3;
  RESERVED2           :bit4;
  ALLOW_EXPCLEAR      :bit1;
  RESERVED3           :bit1;
  TILE_STENCIL_DISABLE:bit1;
  CLEAR_DISALLOWED    :bit1;
  RESERVED4           :bit1;
 end;

 TDB_Z_WRITE_BASE=bit32;

 TGDS_ATOM_SRC0_U=bit32;

 TGDS_ATOM_SRC1_U=bit32;

 TGDS_CNTL_STATUS=bitpacked record
  GDS_BUSY        :bit1;
  GRBM_WBUF_BUSY  :bit1;
  ORD_APP_BUSY    :bit1;
  DS_BANK_CONFLICT:bit1;
  DS_ADDR_CONFLICT:bit1;
  DS_WR_CLAMP     :bit1;
  DS_RD_CLAMP     :bit1;
  GRBM_RBUF_BUSY  :bit1;
  DS_BUSY         :bit1;
  GWS_BUSY        :bit1;
  ORD_FIFO_BUSY   :bit1;
  CREDIT_BUSY0    :bit1;
  CREDIT_BUSY1    :bit1;
  CREDIT_BUSY2    :bit1;
  CREDIT_BUSY3    :bit1;
  RESERVED0       :bit17;
 end;

 TGDS_VMID10_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID10_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID11_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID11_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID12_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID12_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID13_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID13_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID14_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID14_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGDS_VMID15_BASE=bitpacked record
  BASE     :bit16;
  RESERVED0:bit16;
 end;

 TGDS_VMID15_SIZE=bitpacked record
  SIZE     :bit17;
  RESERVED0:bit15;
 end;

 TGRBM_DEBUG_CNTL=bitpacked record
  GRBM_DEBUG_INDEX:bit6;
  RESERVED0       :bit26;
 end;

 TGRBM_DEBUG_DATA=bit32;

 TGRBM_DSM_BYPASS=bitpacked record
  BYPASS_BITS:bit2;
  BYPASS_EN  :bit1;
  RESERVED0  :bit29;
 end;

 TGRBM_READ_ERROR=bitpacked record
  RESERVED0   :bit2;
  READ_ADDRESS:bit16;
  RESERVED1   :bit2;
  READ_PIPEID :bit2;
  READ_MEID   :bit2;
  RESERVED2   :bit7;
  READ_ERROR  :bit1;
 end;

 TGRBM_SOFT_RESET=bitpacked record
  SOFT_RESET_CP :bit1;
  RESERVED0     :bit1;
  SOFT_RESET_RLC:bit1;
  RESERVED1     :bit1;
  RESERVED2     :bit1;
  RESERVED3     :bit1;
  RESERVED4     :bit1;
  RESERVED5     :bit1;
  RESERVED6     :bit1;
  RESERVED7     :bit1;
  RESERVED8     :bit1;
  RESERVED9     :bit1;
  RESERVED10    :bit1;
  RESERVED11    :bit1;
  RESERVED12    :bit1;
  RESERVED13    :bit1;
  SOFT_RESET_GFX:bit1;
  SOFT_RESET_CPF:bit1;
  SOFT_RESET_CPC:bit1;
  SOFT_RESET_CPG:bit1;
  SOFT_RESET_CAC:bit1;
  RESERVED14    :bit11;
 end;

 TGRBM_STATUS_SE0=bitpacked record
  RESERVED0:bit1;
  DB_CLEAN :bit1;
  CB_CLEAN :bit1;
  RESERVED1:bit19;
  BCI_BUSY :bit1;
  VGT_BUSY :bit1;
  PA_BUSY  :bit1;
  TA_BUSY  :bit1;
  SX_BUSY  :bit1;
  SPI_BUSY :bit1;
  RESERVED2:bit1;
  SC_BUSY  :bit1;
  DB_BUSY  :bit1;
  CB_BUSY  :bit1;
 end;

 TGRBM_STATUS_SE1=bitpacked record
  RESERVED0:bit1;
  DB_CLEAN :bit1;
  CB_CLEAN :bit1;
  RESERVED1:bit19;
  BCI_BUSY :bit1;
  VGT_BUSY :bit1;
  PA_BUSY  :bit1;
  TA_BUSY  :bit1;
  SX_BUSY  :bit1;
  SPI_BUSY :bit1;
  RESERVED2:bit1;
  SC_BUSY  :bit1;
  DB_BUSY  :bit1;
  CB_BUSY  :bit1;
 end;

 TGRBM_STATUS_SE2=bitpacked record
  RESERVED0:bit1;
  DB_CLEAN :bit1;
  CB_CLEAN :bit1;
  RESERVED1:bit19;
  BCI_BUSY :bit1;
  VGT_BUSY :bit1;
  PA_BUSY  :bit1;
  TA_BUSY  :bit1;
  SX_BUSY  :bit1;
  SPI_BUSY :bit1;
  RESERVED2:bit1;
  SC_BUSY  :bit1;
  DB_BUSY  :bit1;
  CB_BUSY  :bit1;
 end;

 TGRBM_STATUS_SE3=bitpacked record
  RESERVED0:bit1;
  DB_CLEAN :bit1;
  CB_CLEAN :bit1;
  RESERVED1:bit19;
  BCI_BUSY :bit1;
  VGT_BUSY :bit1;
  PA_BUSY  :bit1;
  TA_BUSY  :bit1;
  SX_BUSY  :bit1;
  SPI_BUSY :bit1;
  RESERVED2:bit1;
  SC_BUSY  :bit1;
  DB_BUSY  :bit1;
  CB_BUSY  :bit1;
 end;

 TPA_CL_CLIP_CNTL=bitpacked record
  UCP_ENA_0                :bit1;
  UCP_ENA_1                :bit1;
  UCP_ENA_2                :bit1;
  UCP_ENA_3                :bit1;
  UCP_ENA_4                :bit1;
  UCP_ENA_5                :bit1;
  RESERVED0                :bit7;
  PS_UCP_Y_SCALE_NEG       :bit1;
  PS_UCP_MODE              :bit2;
  CLIP_DISABLE             :bit1;
  UCP_CULL_ONLY_ENA        :bit1;
  BOUNDARY_EDGE_FLAG_ENA   :bit1;
  DX_CLIP_SPACE_DEF        :bit1;
  DIS_CLIP_ERR_DETECT      :bit1;
  VTX_KILL_OR              :bit1;
  DX_RASTERIZATION_KILL    :bit1;
  RESERVED1                :bit1;
  DX_LINEAR_ATTR_CLIP_ENA  :bit1;
  VTE_VPORT_PROVOKE_DISABLE:bit1;
  ZCLIP_NEAR_DISABLE       :bit1;
  ZCLIP_FAR_DISABLE        :bit1;
  RESERVED2                :bit4;
 end;

 TPA_SC_AA_CONFIG=bitpacked record
  MSAA_NUM_SAMPLES      :bit3;
  RESERVED0             :bit1;
  AA_MASK_CENTROID_DTMN :bit1;
  RESERVED1             :bit8;
  MAX_SAMPLE_DIST       :bit4;
  RESERVED2             :bit3;
  MSAA_EXPOSED_SAMPLES  :bit3;
  RESERVED3             :bit1;
  DETAIL_TO_EXPOSED_MODE:bit2;
  RESERVED4             :bit6;
 end;

 TPA_SC_FIFO_SIZE=bitpacked record
  SC_FRONTEND_PRIM_FIFO_SIZE:bit6;
  SC_BACKEND_PRIM_FIFO_SIZE :bit9;
  SC_HIZ_TILE_FIFO_SIZE     :bit6;
  RESERVED0                 :bit2;
  SC_EARLYZ_TILE_FIFO_SIZE  :bit9;
 end;

 TPA_SC_LINE_CNTL=bitpacked record
  RESERVED0               :bit9;
  EXPAND_LINE_WIDTH       :bit1;
  LAST_PIXEL              :bit1;
  PERPENDICULAR_ENDCAP_ENA:bit1;
  DX10_DIAMOND_TEST_ENA   :bit1;
  RESERVED1               :bit19;
 end;

 TPA_SU_LINE_CNTL=bitpacked record
  WIDTH    :bit16;
  RESERVED0:bit16;
 end;

 TSPI_CONFIG_CNTL=bitpacked record
  GPR_WRITE_PRIORITY   :bit21;
  EXP_PRIORITY_ORDER   :bit3;
  ENABLE_SQG_TOP_EVENTS:bit1;
  ENABLE_SQG_BOP_EVENTS:bit1;
  RSRC_MGMT_RESET      :bit1;
  TTRACE_STALL_ALL     :bit1;
  RESERVED0            :bit4;
 end;

 TSPI_GDBG_TBA_HI=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_GDBG_TBA_LO=bit32;

 TSPI_GDBG_TMA_HI=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_GDBG_TMA_LO=bit32;

 TSPI_GDS_CREDITS=bitpacked record
  DS_DATA_CREDITS:bit8;
  DS_CMD_CREDITS :bit8;
  UNUSED         :bit16;
 end;

 TSPI_LB_CTR_CTRL=bitpacked record
  LOAD     :bit1;
  RESERVED0:bit31;
 end;

 TSPI_LB_DATA_REG=bit32;

 TSPI_RESET_DEBUG=bitpacked record
  DISABLE_GFX_RESET         :bit1;
  DISABLE_GFX_RESET_PER_VMID:bit1;
  DISABLE_GFX_RESET_ALL_VMID:bit1;
  DISABLE_GFX_RESET_RESOURCE:bit1;
  DISABLE_GFX_RESET_PRIORITY:bit1;
  RESERVED0                 :bit27;
 end;

 TSPI_START_PHASE=bitpacked record
  VGPR_START_PHASE:bit2;
  SGPR_START_PHASE:bit2;
  WAVE_START_PHASE:bit2;
  RESERVED0       :bit26;
 end;

 TSX_DEBUG_BUSY_2=bitpacked record
  COL_SCBD_BUSY            :bit1;
  COL_REQ3_FREECNT_NE0     :bit1;
  COL_REQ3_IDLE            :bit1;
  COL_REQ3_BUSY            :bit1;
  COL_REQ2_FREECNT_NE0     :bit1;
  COL_REQ2_IDLE            :bit1;
  COL_REQ2_BUSY            :bit1;
  COL_REQ1_FREECNT_NE0     :bit1;
  COL_REQ1_IDLE            :bit1;
  COL_REQ1_BUSY            :bit1;
  COL_REQ0_FREECNT_NE0     :bit1;
  COL_REQ0_IDLE            :bit1;
  COL_REQ0_BUSY            :bit1;
  COL_DBIF3_SENDFREE_BUSY  :bit1;
  COL_DBIF3_FIFO_BUSY      :bit1;
  COL_DBIF3_READ_VALID     :bit1;
  COL_DBIF2_SENDFREE_BUSY  :bit1;
  COL_DBIF2_FIFO_BUSY      :bit1;
  COL_DBIF2_READ_VALID     :bit1;
  COL_DBIF1_SENDFREE_BUSY  :bit1;
  COL_DBIF1_FIFO_BUSY      :bit1;
  COL_DBIF1_READ_VALID     :bit1;
  COL_DBIF0_SENDFREE_BUSY  :bit1;
  COL_DBIF0_FIFO_BUSY      :bit1;
  COL_DBIF0_READ_VALID     :bit1;
  COL_BUFF3_BANK3_VAL3_BUSY:bit1;
  COL_BUFF3_BANK3_VAL2_BUSY:bit1;
  COL_BUFF3_BANK3_VAL1_BUSY:bit1;
  COL_BUFF3_BANK3_VAL0_BUSY:bit1;
  COL_BUFF3_BANK2_VAL3_BUSY:bit1;
  COL_BUFF3_BANK2_VAL2_BUSY:bit1;
  COL_BUFF3_BANK2_VAL1_BUSY:bit1;
 end;

 TSX_DEBUG_BUSY_3=bitpacked record
  COL_BUFF3_BANK2_VAL0_BUSY:bit1;
  COL_BUFF3_BANK1_VAL3_BUSY:bit1;
  COL_BUFF3_BANK1_VAL2_BUSY:bit1;
  COL_BUFF3_BANK1_VAL1_BUSY:bit1;
  COL_BUFF3_BANK1_VAL0_BUSY:bit1;
  COL_BUFF3_BANK0_VAL3_BUSY:bit1;
  COL_BUFF3_BANK0_VAL2_BUSY:bit1;
  COL_BUFF3_BANK0_VAL1_BUSY:bit1;
  COL_BUFF3_BANK0_VAL0_BUSY:bit1;
  COL_BUFF2_BANK3_VAL3_BUSY:bit1;
  COL_BUFF2_BANK3_VAL2_BUSY:bit1;
  COL_BUFF2_BANK3_VAL1_BUSY:bit1;
  COL_BUFF2_BANK3_VAL0_BUSY:bit1;
  COL_BUFF2_BANK2_VAL3_BUSY:bit1;
  COL_BUFF2_BANK2_VAL2_BUSY:bit1;
  COL_BUFF2_BANK2_VAL1_BUSY:bit1;
  COL_BUFF2_BANK2_VAL0_BUSY:bit1;
  COL_BUFF2_BANK1_VAL3_BUSY:bit1;
  COL_BUFF2_BANK1_VAL2_BUSY:bit1;
  COL_BUFF2_BANK1_VAL1_BUSY:bit1;
  COL_BUFF2_BANK1_VAL0_BUSY:bit1;
  COL_BUFF2_BANK0_VAL3_BUSY:bit1;
  COL_BUFF2_BANK0_VAL2_BUSY:bit1;
  COL_BUFF2_BANK0_VAL1_BUSY:bit1;
  COL_BUFF2_BANK0_VAL0_BUSY:bit1;
  COL_BUFF1_BANK3_VAL3_BUSY:bit1;
  COL_BUFF1_BANK3_VAL2_BUSY:bit1;
  COL_BUFF1_BANK3_VAL1_BUSY:bit1;
  COL_BUFF1_BANK3_VAL0_BUSY:bit1;
  COL_BUFF1_BANK2_VAL3_BUSY:bit1;
  COL_BUFF1_BANK2_VAL2_BUSY:bit1;
  COL_BUFF1_BANK2_VAL1_BUSY:bit1;
 end;

 TSX_DEBUG_BUSY_4=bitpacked record
  COL_BUFF1_BANK2_VAL0_BUSY:bit1;
  COL_BUFF1_BANK1_VAL3_BUSY:bit1;
  COL_BUFF1_BANK1_VAL2_BUSY:bit1;
  COL_BUFF1_BANK1_VAL1_BUSY:bit1;
  COL_BUFF1_BANK1_VAL0_BUSY:bit1;
  COL_BUFF1_BANK0_VAL3_BUSY:bit1;
  COL_BUFF1_BANK0_VAL2_BUSY:bit1;
  COL_BUFF1_BANK0_VAL1_BUSY:bit1;
  COL_BUFF1_BANK0_VAL0_BUSY:bit1;
  COL_BUFF0_BANK3_VAL3_BUSY:bit1;
  COL_BUFF0_BANK3_VAL2_BUSY:bit1;
  COL_BUFF0_BANK3_VAL1_BUSY:bit1;
  COL_BUFF0_BANK3_VAL0_BUSY:bit1;
  COL_BUFF0_BANK2_VAL3_BUSY:bit1;
  COL_BUFF0_BANK2_VAL2_BUSY:bit1;
  COL_BUFF0_BANK2_VAL1_BUSY:bit1;
  COL_BUFF0_BANK2_VAL0_BUSY:bit1;
  COL_BUFF0_BANK1_VAL3_BUSY:bit1;
  COL_BUFF0_BANK1_VAL2_BUSY:bit1;
  COL_BUFF0_BANK1_VAL1_BUSY:bit1;
  COL_BUFF0_BANK1_VAL0_BUSY:bit1;
  COL_BUFF0_BANK0_VAL3_BUSY:bit1;
  COL_BUFF0_BANK0_VAL2_BUSY:bit1;
  COL_BUFF0_BANK0_VAL1_BUSY:bit1;
  COL_BUFF0_BANK0_VAL0_BUSY:bit1;
  RESERVED                 :bit7;
 end;

 TTA_BC_BASE_ADDR=bit32;

 TTCC_EXE_DISABLE=bitpacked record
  RESERVED0  :bit1;
  EXE_DISABLE:bit1;
  RESERVED1  :bit30;
 end;

 TTCP_ADDR_CONFIG=bitpacked record
  NUM_TCC_BANKS :bit4;
  NUM_BANKS     :bit2;
  COLHI_WIDTH   :bit3;
  RB_SPLIT_COLHI:bit1;
  RESERVED0     :bit22;
 end;

 TTCP_GATCL1_CNTL=bitpacked record
  RESERVED0             :bit25;
  INVALIDATE_ALL_VMID   :bit1;
  FORCE_MISS            :bit1;
  FORCE_IN_ORDER        :bit1;
  REDUCE_FIFO_DEPTH_BY_2:bit2;
  REDUCE_CACHE_SIZE_BY_2:bit2;
 end;

 TTCP_WATCH0_CNTL=bitpacked record
  MASK :bit24;
  VMID :bit4;
  ATC  :bit1;
  MODE :bit2;
  VALID:bit1;
 end;

 TTCP_WATCH1_CNTL=bitpacked record
  MASK :bit24;
  VMID :bit4;
  ATC  :bit1;
  MODE :bit2;
  VALID:bit1;
 end;

 TTCP_WATCH2_CNTL=bitpacked record
  MASK :bit24;
  VMID :bit4;
  ATC  :bit1;
  MODE :bit2;
  VALID:bit1;
 end;

 TTCP_WATCH3_CNTL=bitpacked record
  MASK :bit24;
  VMID :bit4;
  ATC  :bit1;
  MODE :bit2;
  VALID:bit1;
 end;

 TVGT_CNTL_STATUS=bitpacked record
  VGT_BUSY         :bit1;
  VGT_OUT_INDX_BUSY:bit1;
  VGT_OUT_BUSY     :bit1;
  VGT_PT_BUSY      :bit1;
  VGT_TE_BUSY      :bit1;
  VGT_VR_BUSY      :bit1;
  VGT_PI_BUSY      :bit1;
  VGT_GS_BUSY      :bit1;
  VGT_HS_BUSY      :bit1;
  VGT_TE11_BUSY    :bit1;
  RESERVED0        :bit22;
 end;

 TVGT_DEBUG_REG10=bitpacked record
  index_buffer_depth_r1_q   :bit5;
  eopg_r2_q                 :bit1;
  eotg_r2_q                 :bit1;
  onchip_gs_en_r0_q         :bit2;
  SPARE2                    :bit2;
  rcm_mem_gsprim_re_qq      :bit1;
  rcm_mem_gsprim_re_q       :bit1;
  gs_rb_space_avail_r3_q_9_0:bit10;
  es_rb_space_avail_r2_q_8_0:bit9;
 end;

 TVGT_DEBUG_REG11=bitpacked record
  tm_busy_q              :bit1;
  tm_noif_busy_q         :bit1;
  tm_out_busy_q          :bit1;
  es_rb_dealloc_fifo_busy:bit1;
  vs_dealloc_tbl_busy    :bit1;
  SPARE1                 :bit1;
  spi_gsthread_fifo_busy :bit1;
  spi_esthread_fifo_busy :bit1;
  hold_eswave            :bit1;
  es_rb_roll_over_r3     :bit1;
  counters_busy_r0       :bit1;
  counters_avail_r0      :bit1;
  counters_available_r0  :bit1;
  vs_event_fifo_rtr      :bit1;
  VGT_SPI_gsthread_rtr_q :bit1;
  VGT_SPI_esthread_rtr_q :bit1;
  gs_issue_rtr           :bit1;
  tm_pt_event_rtr        :bit1;
  SPARE0                 :bit1;
  gs_r0_rtr              :bit1;
  es_r0_rtr              :bit1;
  gog_tm_vs_event_rtr    :bit1;
  tm_rcm_gs_event_rtr    :bit1;
  tm_rcm_gs_tbl_rtr      :bit1;
  tm_rcm_es_tbl_rtr      :bit1;
  vs_event_fifo_empty    :bit1;
  vs_event_fifo_full     :bit1;
  es_rb_dealloc_fifo_full:bit1;
  vs_dealloc_tbl_full    :bit1;
  send_event_q           :bit1;
  es_tbl_empty           :bit1;
  no_active_states_r0    :bit1;
 end;

 TVGT_DEBUG_REG12=bitpacked record
  gs_state0_r0_q :bit3;
  gs_state1_r0_q :bit3;
  gs_state2_r0_q :bit3;
  gs_state3_r0_q :bit3;
  gs_state4_r0_q :bit3;
  gs_state5_r0_q :bit3;
  gs_state6_r0_q :bit3;
  gs_state7_r0_q :bit3;
  gs_state8_r0_q :bit3;
  gs_state9_r0_q :bit3;
  hold_eswave_eop:bit1;
  SPARE0         :bit1;
 end;

 TVGT_DEBUG_REG13=bitpacked record
  gs_state10_r0_q              :bit3;
  gs_state11_r0_q              :bit3;
  gs_state12_r0_q              :bit3;
  gs_state13_r0_q              :bit3;
  gs_state14_r0_q              :bit3;
  gs_state15_r0_q              :bit3;
  gs_tbl_wrptr_r0_q_3_0        :bit4;
  gsfetch_done_fifo_cnt_q_not_0:bit1;
  gsfetch_done_cnt_q_not_0     :bit1;
  es_tbl_full                  :bit1;
  SPARE1                       :bit1;
  SPARE0                       :bit1;
  active_cm_sm_r0_q            :bit5;
 end;

 TVGT_DEBUG_REG14=bitpacked record
  SPARE3                      :bit4;
  gsfetch_done_fifo_full      :bit1;
  gs_rb_space_avail_r0        :bit1;
  smx_es_done_cnt_r0_q_not_0  :bit1;
  SPARE8                      :bit2;
  vs_done_cnt_q_not_0         :bit1;
  es_flush_cnt_busy_q         :bit1;
  gs_tbl_full_r0              :bit1;
  SPARE2                      :bit9;
  se1spi_gsthread_fifo_busy   :bit1;
  SPARE                       :bit3;
  VGT_SE1SPI_gsthread_rtr_q   :bit1;
  smx1_es_done_cnt_r0_q_not_0 :bit1;
  se1spi_esthread_fifo_busy   :bit1;
  SPARE1                      :bit1;
  gsfetch_done_se1_cnt_q_not_0:bit1;
  SPARE0                      :bit1;
  VGT_SE1SPI_esthread_rtr_q   :bit1;
 end;

 TVGT_DEBUG_REG15=bitpacked record
  cm_busy_q            :bit1;
  counters_busy_q      :bit1;
  output_fifo_empty    :bit1;
  output_fifo_full     :bit1;
  counters_full        :bit1;
  active_sm_q          :bit5;
  entry_rdptr_q        :bit5;
  cntr_tbl_wrptr_q     :bit5;
  SPARE25              :bit6;
  st_cut_mode_q        :bit2;
  gs_done_array_q_not_0:bit1;
  SPARE31              :bit3;
 end;

 TVGT_DEBUG_REG16=bitpacked record
  gog_busy                 :bit1;
  gog_state_q              :bit3;
  r0_rtr                   :bit1;
  r1_rtr                   :bit1;
  r1_upstream_rtr          :bit1;
  r2_vs_tbl_rtr            :bit1;
  r2_prim_rtr              :bit1;
  r2_indx_rtr              :bit1;
  r2_rtr                   :bit1;
  gog_tm_vs_event_rtr      :bit1;
  r3_force_vs_tbl_we_rtr   :bit1;
  indx_valid_r2_q          :bit1;
  prim_valid_r2_q          :bit1;
  valid_r2_q               :bit1;
  prim_valid_r1_q          :bit1;
  indx_valid_r1_q          :bit1;
  valid_r1_q               :bit1;
  indx_valid_r0_q          :bit1;
  prim_valid_r0_q          :bit1;
  valid_r0_q               :bit1;
  send_event_q             :bit1;
  SPARE24                  :bit1;
  vert_seen_since_sopg_r2_q:bit1;
  gog_out_prim_state_sel   :bit3;
  multiple_streams_en_r1_q :bit1;
  vs_vert_count_r2_q_not_0 :bit1;
  num_gs_r2_q_not_0        :bit1;
  new_vs_thread_r2         :bit1;
 end;

 TVGT_DEBUG_REG17=bitpacked record
  gog_out_prim_rel_indx2_5_0:bit6;
  gog_out_prim_rel_indx1_5_0:bit6;
  gog_out_prim_rel_indx0_5_0:bit6;
  gog_out_indx_13_0         :bit14;
 end;

 TVGT_DEBUG_REG18=bitpacked record
  grp_vr_valid               :bit1;
  pipe0_dr                   :bit1;
  pipe1_dr                   :bit1;
  vr_grp_read                :bit1;
  pipe0_rtr                  :bit1;
  pipe1_rtr                  :bit1;
  out_vr_indx_read           :bit1;
  out_vr_prim_read           :bit1;
  indices_to_send_q          :bit3;
  valid_indices              :bit1;
  last_indx_of_prim          :bit1;
  indx0_new_d                :bit1;
  indx1_new_d                :bit1;
  indx2_new_d                :bit1;
  indx2_hit_d                :bit1;
  indx1_hit_d                :bit1;
  indx0_hit_d                :bit1;
  st_vertex_reuse_off_r0_q   :bit1;
  last_group_of_instance_r0_q:bit1;
  null_primitive_r0_q        :bit1;
  eop_r0_q                   :bit1;
  eject_vtx_vect_r1_d        :bit1;
  sub_prim_type_r0_q         :bit3;
  gs_scenario_a_r0_q         :bit1;
  gs_scenario_b_r0_q         :bit1;
  components_valid_r0_q      :bit3;
 end;

 TVGT_DEBUG_REG19=bitpacked record
  separate_out_busy_q         :bit1;
  separate_out_indx_busy_q    :bit1;
  prim_buffer_empty           :bit1;
  prim_buffer_full            :bit1;
  pa_clips_fifo_busy_q        :bit1;
  pa_clipp_fifo_busy_q        :bit1;
  VGT_PA_clips_rtr_q          :bit1;
  VGT_PA_clipp_rtr_q          :bit1;
  spi_vsthread_fifo_busy_q    :bit1;
  spi_vsvert_fifo_busy_q      :bit1;
  pa_clipv_fifo_busy_q        :bit1;
  hold_prim                   :bit1;
  VGT_SPI_vsthread_rtr_q      :bit1;
  VGT_SPI_vsvert_rtr_q        :bit1;
  VGT_PA_clipv_rtr_q          :bit1;
  new_packet_q                :bit1;
  buffered_prim_event         :bit1;
  buffered_prim_null_primitive:bit1;
  buffered_prim_eop           :bit1;
  buffered_prim_eject_vtx_vect:bit1;
  buffered_prim_type_event    :bit6;
  VGT_SE1SPI_vswave_rtr_q     :bit1;
  VGT_SE1SPI_vsvert_rtr_q     :bit1;
  num_new_unique_rel_indx     :bit2;
  null_terminate_vtx_vector   :bit1;
  filter_event                :bit1;
 end;

 TVGT_DEBUG_REG20=bitpacked record
  dbg_VGT_SPI_vsthread_sovertexindex      :bit16;
  dbg_VGT_SPI_vsthread_sovertexcount_not_0:bit1;
  SPARE17                                 :bit1;
  alloc_counter_q                         :bit4;
  curr_dealloc_distance_q                 :bit7;
  new_allocate_q                          :bit1;
  curr_slot_in_vtx_vect_q_not_0           :bit1;
  int_vtx_counter_q_not_0                 :bit1;
 end;

 TVGT_DEBUG_REG21=bitpacked record
  out_indx_fifo_empty     :bit1;
  indx_side_fifo_empty    :bit1;
  pipe0_dr                :bit1;
  pipe1_dr                :bit1;
  pipe2_dr                :bit1;
  vsthread_buff_empty     :bit1;
  out_indx_fifo_full      :bit1;
  indx_side_fifo_full     :bit1;
  pipe0_rtr               :bit1;
  pipe1_rtr               :bit1;
  pipe2_rtr               :bit1;
  vsthread_buff_full      :bit1;
  interfaces_rtr          :bit1;
  indx_count_q_not_0      :bit1;
  wait_for_external_eopg_q:bit1;
  full_state_p1_q         :bit1;
  indx_side_indx_valid    :bit1;
  stateid_p0_q            :bit3;
  is_event_p0_q           :bit1;
  lshs_dealloc_p1         :bit1;
  stream_id_r2_q          :bit1;
  vtx_vect_counter_q_not_0:bit1;
  buff_full_p1            :bit1;
  strmout_valid_p1        :bit1;
  eotg_r2_q               :bit1;
  null_r2_q               :bit1;
  p0_dr                   :bit1;
  p0_rtr                  :bit1;
  eopg_p0_q               :bit1;
  p0_nobp                 :bit1;
 end;

 TVGT_DEBUG_REG22=bitpacked record
  cm_state16:bit2;
  cm_state17:bit2;
  cm_state18:bit2;
  cm_state19:bit2;
  cm_state20:bit2;
  cm_state21:bit2;
  cm_state22:bit2;
  cm_state23:bit2;
  cm_state24:bit2;
  cm_state25:bit2;
  cm_state26:bit2;
  cm_state27:bit2;
  cm_state28:bit2;
  cm_state29:bit2;
  cm_state30:bit2;
  cm_state31:bit2;
 end;

 TVGT_DEBUG_REG23=bitpacked record
  frmt_busy          :bit1;
  rcm_frmt_vert_rtr  :bit1;
  rcm_frmt_prim_rtr  :bit1;
  prim_r3_rtr        :bit1;
  prim_r2_rtr        :bit1;
  vert_r3_rtr        :bit1;
  vert_r2_rtr        :bit1;
  vert_r1_rtr        :bit1;
  vert_r0_rtr        :bit1;
  prim_fifo_empty    :bit1;
  prim_fifo_full     :bit1;
  vert_dr_r2_q       :bit1;
  prim_dr_r2_q       :bit1;
  vert_dr_r1_q       :bit1;
  vert_dr_r0_q       :bit1;
  new_verts_r2_q     :bit2;
  verts_sent_r2_q    :bit4;
  prim_state_sel_r2_q:bit3;
  SPARE              :bit8;
 end;

 TVGT_DEBUG_REG24=bitpacked record
  avail_es_rb_space_r0_q_23_0:bit24;
  dependent_st_cut_mode_q    :bit2;
  SPARE31                    :bit6;
 end;

 TVGT_DEBUG_REG25=bitpacked record
  avail_gs_rb_space_r0_q_25_0:bit26;
  active_sm_r0_q             :bit4;
  add_gs_rb_space_r1_q       :bit1;
  add_gs_rb_space_r0_q       :bit1;
 end;

 TVGT_DEBUG_REG26=bitpacked record
  cm_state0 :bit2;
  cm_state1 :bit2;
  cm_state2 :bit2;
  cm_state3 :bit2;
  cm_state4 :bit2;
  cm_state5 :bit2;
  cm_state6 :bit2;
  cm_state7 :bit2;
  cm_state8 :bit2;
  cm_state9 :bit2;
  cm_state10:bit2;
  cm_state11:bit2;
  cm_state12:bit2;
  cm_state13:bit2;
  cm_state14:bit2;
  cm_state15:bit2;
 end;

 TVGT_DEBUG_REG27=bitpacked record
  pipe0_dr                   :bit1;
  gsc0_dr                    :bit1;
  pipe1_dr                   :bit1;
  tm_pt_event_rtr            :bit1;
  pipe0_rtr                  :bit1;
  gsc0_rtr                   :bit1;
  pipe1_rtr                  :bit1;
  last_indx_of_prim_p1_q     :bit1;
  indices_to_send_p0_q       :bit2;
  event_flag_p1_q            :bit1;
  eop_p1_q                   :bit1;
  gs_out_prim_type_p0_q      :bit2;
  gsc_null_primitive_p0_q    :bit1;
  gsc_eop_p0_q               :bit1;
  gsc_2cycle_output          :bit1;
  gsc_2nd_cycle_p0_q         :bit1;
  last_indx_of_vsprim        :bit1;
  first_vsprim_of_gsprim_p0_q:bit1;
  gsc_indx_count_p0_q        :bit11;
  last_vsprim_of_gsprim      :bit1;
 end;

 TVGT_DEBUG_REG28=bitpacked record
  con_state_q                  :bit4;
  second_cycle_q               :bit1;
  process_tri_middle_p0_q      :bit1;
  process_tri_1st_2nd_half_p0_q:bit1;
  process_tri_center_poly_p0_q :bit1;
  pipe0_patch_dr               :bit1;
  pipe0_edge_dr                :bit1;
  pipe1_dr                     :bit1;
  pipe0_patch_rtr              :bit1;
  pipe0_edge_rtr               :bit1;
  pipe1_rtr                    :bit1;
  outer_parity_p0_q            :bit1;
  parallel_parity_p0_q         :bit1;
  first_ring_of_patch_p0_q     :bit1;
  last_ring_of_patch_p0_q      :bit1;
  last_edge_of_outer_ring_p0_q :bit1;
  last_point_of_outer_ring_p1  :bit1;
  last_point_of_inner_ring_p1  :bit1;
  outer_edge_tf_eq_one_p0_q    :bit1;
  advance_outer_point_p1       :bit1;
  advance_inner_point_p1       :bit1;
  next_ring_is_rect_p0_q       :bit1;
  pipe1_outer1_rtr             :bit1;
  pipe1_outer2_rtr             :bit1;
  pipe1_inner1_rtr             :bit1;
  pipe1_inner2_rtr             :bit1;
  pipe1_patch_rtr              :bit1;
  pipe1_edge_rtr               :bit1;
  use_stored_inner_q_ring2     :bit1;
 end;

 TVGT_DEBUG_REG29=bitpacked record
  con_state_q                  :bit4;
  second_cycle_q               :bit1;
  process_tri_middle_p0_q      :bit1;
  process_tri_1st_2nd_half_p0_q:bit1;
  process_tri_center_poly_p0_q :bit1;
  pipe0_patch_dr               :bit1;
  pipe0_edge_dr                :bit1;
  pipe1_dr                     :bit1;
  pipe0_patch_rtr              :bit1;
  pipe0_edge_rtr               :bit1;
  pipe1_rtr                    :bit1;
  outer_parity_p0_q            :bit1;
  parallel_parity_p0_q         :bit1;
  first_ring_of_patch_p0_q     :bit1;
  last_ring_of_patch_p0_q      :bit1;
  last_edge_of_outer_ring_p0_q :bit1;
  last_point_of_outer_ring_p1  :bit1;
  last_point_of_inner_ring_p1  :bit1;
  outer_edge_tf_eq_one_p0_q    :bit1;
  advance_outer_point_p1       :bit1;
  advance_inner_point_p1       :bit1;
  next_ring_is_rect_p0_q       :bit1;
  pipe1_outer1_rtr             :bit1;
  pipe1_outer2_rtr             :bit1;
  pipe1_inner1_rtr             :bit1;
  pipe1_inner2_rtr             :bit1;
  pipe1_patch_rtr              :bit1;
  pipe1_edge_rtr               :bit1;
  use_stored_inner_q_ring3     :bit1;
 end;

 TVGT_DEBUG_REG31=bitpacked record
  pipe0_dr                 :bit1;
  pipe0_rtr                :bit1;
  pipe1_outer_dr           :bit1;
  pipe1_inner_dr           :bit1;
  pipe2_outer_dr           :bit1;
  pipe2_inner_dr           :bit1;
  pipe3_outer_dr           :bit1;
  pipe3_inner_dr           :bit1;
  pipe4_outer_dr           :bit1;
  pipe4_inner_dr           :bit1;
  pipe5_outer_dr           :bit1;
  pipe5_inner_dr           :bit1;
  pipe2_outer_rtr          :bit1;
  pipe2_inner_rtr          :bit1;
  pipe3_outer_rtr          :bit1;
  pipe3_inner_rtr          :bit1;
  pipe4_outer_rtr          :bit1;
  pipe4_inner_rtr          :bit1;
  pipe5_outer_rtr          :bit1;
  pipe5_inner_rtr          :bit1;
  pg_con_outer_point1_rts  :bit1;
  pg_con_outer_point2_rts  :bit1;
  pg_con_inner_point1_rts  :bit1;
  pg_con_inner_point2_rts  :bit1;
  pg_patch_fifo_empty      :bit1;
  pg_edge_fifo_empty       :bit1;
  pg_inner3_perp_fifo_empty:bit1;
  pg_patch_fifo_full       :bit1;
  pg_edge_fifo_full        :bit1;
  pg_inner_perp_fifo_full  :bit1;
  outer_ring_done_q        :bit1;
  inner_ring_done_q        :bit1;
 end;

 TVGT_DEBUG_REG32=bitpacked record
  first_ring_of_patch      :bit1;
  last_ring_of_patch       :bit1;
  last_edge_of_outer_ring  :bit1;
  last_point_of_outer_edge :bit1;
  last_edge_of_inner_ring  :bit1;
  last_point_of_inner_edge :bit1;
  last_patch_of_tg_p0_q    :bit1;
  event_null_special_p0_q  :bit1;
  event_flag_p5_q          :bit1;
  first_point_of_patch_p5_q:bit1;
  first_point_of_edge_p5_q :bit1;
  last_patch_of_tg_p5_q    :bit1;
  tess_topology_p5_q       :bit2;
  pipe5_inner3_rtr         :bit1;
  pipe5_inner2_rtr         :bit1;
  pg_edge_fifo3_full       :bit1;
  pg_edge_fifo2_full       :bit1;
  pg_inner3_point_fifo_full:bit1;
  pg_outer3_point_fifo_full:bit1;
  pg_inner2_point_fifo_full:bit1;
  pg_outer2_point_fifo_full:bit1;
  pg_inner_point_fifo_full :bit1;
  pg_outer_point_fifo_full :bit1;
  inner2_fifos_rtr         :bit1;
  inner_fifos_rtr          :bit1;
  outer_fifos_rtr          :bit1;
  fifos_rtr                :bit1;
  SPARE                    :bit4;
 end;

 TVGT_DEBUG_REG33=bitpacked record
  pipe0_patch_dr       :bit1;
  ring3_pipe1_dr       :bit1;
  pipe1_dr             :bit1;
  pipe2_dr             :bit1;
  pipe0_patch_rtr      :bit1;
  ring2_pipe1_dr       :bit1;
  ring1_pipe1_dr       :bit1;
  pipe2_rtr            :bit1;
  pipe3_dr             :bit1;
  pipe3_rtr            :bit1;
  ring2_in_sync_q      :bit1;
  ring1_in_sync_q      :bit1;
  pipe1_patch_rtr      :bit1;
  ring3_in_sync_q      :bit1;
  tm_te11_event_rtr    :bit1;
  first_prim_of_patch_q:bit1;
  con_prim_fifo_full   :bit1;
  con_vert_fifo_full   :bit1;
  con_prim_fifo_empty  :bit1;
  con_vert_fifo_empty  :bit1;
  last_patch_of_tg_p0_q:bit1;
  ring3_valid_p2       :bit1;
  ring2_valid_p2       :bit1;
  ring1_valid_p2       :bit1;
  tess_type_p0_q       :bit2;
  tess_topology_p0_q   :bit2;
  te11_out_vert_gs_en  :bit1;
  con_ring3_busy       :bit1;
  con_ring2_busy       :bit1;
  con_ring1_busy       :bit1;
 end;

 TVGT_DEBUG_REG34=bitpacked record
  con_state_q                  :bit4;
  second_cycle_q               :bit1;
  process_tri_middle_p0_q      :bit1;
  process_tri_1st_2nd_half_p0_q:bit1;
  process_tri_center_poly_p0_q :bit1;
  pipe0_patch_dr               :bit1;
  pipe0_edge_dr                :bit1;
  pipe1_dr                     :bit1;
  pipe0_patch_rtr              :bit1;
  pipe0_edge_rtr               :bit1;
  pipe1_rtr                    :bit1;
  outer_parity_p0_q            :bit1;
  parallel_parity_p0_q         :bit1;
  first_ring_of_patch_p0_q     :bit1;
  last_ring_of_patch_p0_q      :bit1;
  last_edge_of_outer_ring_p0_q :bit1;
  last_point_of_outer_ring_p1  :bit1;
  last_point_of_inner_ring_p1  :bit1;
  outer_edge_tf_eq_one_p0_q    :bit1;
  advance_outer_point_p1       :bit1;
  advance_inner_point_p1       :bit1;
  next_ring_is_rect_p0_q       :bit1;
  pipe1_outer1_rtr             :bit1;
  pipe1_outer2_rtr             :bit1;
  pipe1_inner1_rtr             :bit1;
  pipe1_inner2_rtr             :bit1;
  pipe1_patch_rtr              :bit1;
  pipe1_edge_rtr               :bit1;
  use_stored_inner_q_ring1     :bit1;
 end;

 TVGT_DEBUG_REG36=bit32;

 TVGT_DMA_BASE_HI=bitpacked record
  BASE_ADDR:bit8;
  RESERVED0:bit24;
 end;

 TVGT_DMA_CONTROL=bitpacked record
  PRIMGROUP_SIZE  :bit16;
  RESERVED0       :bit1;
  IA_SWITCH_ON_EOP:bit1;
  RESERVED1       :bit2;
  WD_SWITCH_ON_EOP:bit1;
  RESERVED2       :bit11;
 end;

 TVGT_FIFO_DEPTHS=bitpacked record
  VS_DEALLOC_TBL_DEPTH:bit7;
  RESERVED0           :bit1;
  CLIPP_FIFO_DEPTH    :bit14;
  HSINPUT_FIFO_DEPTH  :bit6;
  RESERVED1           :bit4;
 end;

 TVGT_INDX_OFFSET=bit32;

 TVGT_MC_LAT_CNTL=bitpacked record
  MC_TIME_STAMP_RES:bit2;
  RESERVED0        :bit30;
 end;

 TVGT_NUM_INDICES=bit32;

 TVGT_RESET_DEBUG=bitpacked record
  GS_DISABLE  :bit1;
  TESS_DISABLE:bit1;
  WD_DISABLE  :bit1;
  RESERVED0   :bit29;
 end;

 TCB_COLOR0_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR1_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR2_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR3_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR4_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR5_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR6_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR7_ATTRIB=bitpacked record
  TILE_MODE_INDEX      :bit5;
  FMASK_TILE_MODE_INDEX:bit5;
  FMASK_BANK_HEIGHT    :bit2;
  NUM_SAMPLES          :bit3;
  NUM_FRAGMENTS        :bit2;
  FORCE_DST_ALPHA_1    :bit1;
  RESERVED0            :bit14;
 end;

 TCB_COLOR_CONTROL=bitpacked record
  DISABLE_DUAL_QUAD:bit1;
  RESERVED0        :bit2;
  DEGAMMA_ENABLE   :bit1;
  MODE             :bit3;
  RESERVED1        :bit9;
  ROP3             :bit8;
  RESERVED2        :bit8;
 end;

 TCOMPUTE_RELAUNCH=bitpacked record
  PAYLOAD :bit30;
  IS_EVENT:bit1;
  IS_STATE:bit1;
 end;

 TCP_CE_IB1_OFFSET=bitpacked record
  IB1_OFFSET:bit20;
  RESERVED0 :bit12;
 end;

 TCP_CE_IB2_OFFSET=bitpacked record
  IB2_OFFSET:bit20;
  RESERVED0 :bit12;
 end;

 TCP_CE_INIT_BUFSZ=bitpacked record
  INIT_BUFSZ:bit12;
  RESERVED0 :bit20;
 end;

 TCP_CE_UCODE_ADDR=bitpacked record
  UCODE_ADDR:bit12;
  RESERVED0 :bit20;
 end;

 TCP_CE_UCODE_DATA=bit32;

 TCP_COHER_BASE_HI=bitpacked record
  COHER_BASE_HI_256B:bit8;
  RESERVED0         :bit24;
 end;

 TCP_COHER_SIZE_HI=bitpacked record
  COHER_SIZE_HI_256B:bit8;
  RESERVED0         :bit24;
 end;

 TCP_CPC_BUSY_STAT=bitpacked record
  MEC1_LOAD_BUSY         :bit1;
  MEC1_SEMAPOHRE_BUSY    :bit1;
  MEC1_MUTEX_BUSY        :bit1;
  MEC1_MESSAGE_BUSY      :bit1;
  MEC1_EOP_QUEUE_BUSY    :bit1;
  MEC1_IQ_QUEUE_BUSY     :bit1;
  MEC1_IB_QUEUE_BUSY     :bit1;
  MEC1_TC_BUSY           :bit1;
  MEC1_DMA_BUSY          :bit1;
  MEC1_PARTIAL_FLUSH_BUSY:bit1;
  MEC1_PIPE0_BUSY        :bit1;
  MEC1_PIPE1_BUSY        :bit1;
  MEC1_PIPE2_BUSY        :bit1;
  MEC1_PIPE3_BUSY        :bit1;
  RESERVED0              :bit2;
  MEC2_LOAD_BUSY         :bit1;
  MEC2_SEMAPOHRE_BUSY    :bit1;
  MEC2_MUTEX_BUSY        :bit1;
  MEC2_MESSAGE_BUSY      :bit1;
  MEC2_EOP_QUEUE_BUSY    :bit1;
  MEC2_IQ_QUEUE_BUSY     :bit1;
  MEC2_IB_QUEUE_BUSY     :bit1;
  MEC2_TC_BUSY           :bit1;
  MEC2_DMA_BUSY          :bit1;
  MEC2_PARTIAL_FLUSH_BUSY:bit1;
  MEC2_PIPE0_BUSY        :bit1;
  MEC2_PIPE1_BUSY        :bit1;
  MEC2_PIPE2_BUSY        :bit1;
  MEC2_PIPE3_BUSY        :bit1;
  RESERVED1              :bit2;
 end;

 TCP_CPF_BUSY_STAT=bitpacked record
  REG_BUS_FIFO_BUSY        :bit1;
  CSF_RING_BUSY            :bit1;
  CSF_INDIRECT1_BUSY       :bit1;
  CSF_INDIRECT2_BUSY       :bit1;
  CSF_STATE_BUSY           :bit1;
  CSF_CE_INDR1_BUSY        :bit1;
  CSF_CE_INDR2_BUSY        :bit1;
  CSF_ARBITER_BUSY         :bit1;
  CSF_INPUT_BUSY           :bit1;
  OUTSTANDING_READ_TAGS    :bit1;
  RESERVED0                :bit1;
  HPD_PROCESSING_EOP_BUSY  :bit1;
  HQD_DISPATCH_BUSY        :bit1;
  HQD_IQ_TIMER_BUSY        :bit1;
  HQD_DMA_OFFLOAD_BUSY     :bit1;
  HQD_WAIT_SEMAPHORE_BUSY  :bit1;
  HQD_SIGNAL_SEMAPHORE_BUSY:bit1;
  HQD_MESSAGE_BUSY         :bit1;
  HQD_PQ_FETCHER_BUSY      :bit1;
  HQD_IB_FETCHER_BUSY      :bit1;
  HQD_IQ_FETCHER_BUSY      :bit1;
  HQD_EOP_FETCHER_BUSY     :bit1;
  HQD_CONSUMED_RPTR_BUSY   :bit1;
  HQD_FETCHER_ARB_BUSY     :bit1;
  HQD_ROQ_ALIGN_BUSY       :bit1;
  HQD_ROQ_EOP_BUSY         :bit1;
  HQD_ROQ_IQ_BUSY          :bit1;
  HQD_ROQ_PQ_BUSY          :bit1;
  HQD_ROQ_IB_BUSY          :bit1;
  HQD_WPTR_POLL_BUSY       :bit1;
  HQD_PQ_BUSY              :bit1;
  HQD_IB_BUSY              :bit1;
 end;

 TCP_DMA_READ_TAGS=bitpacked record
  DMA_READ_TAG      :bit26;
  RESERVED0         :bit2;
  DMA_READ_TAG_VALID:bit1;
  RESERVED1         :bit3;
 end;

 TCP_GDS_BKUP_ADDR=bit32;

 TCP_HQD_EOP_DONES=bit32;

 TCP_IQ_WAIT_TIME1=bitpacked record
  IB_OFFLOAD    :bit8;
  ATOMIC_OFFLOAD:bit8;
  WRM_OFFLOAD   :bit8;
  GWS           :bit8;
 end;

 TCP_IQ_WAIT_TIME2=bitpacked record
  QUE_SLEEP:bit8;
  SCH_WAVE :bit8;
  SEM_REARM:bit8;
  DEQ_RETRY:bit8;
 end;

 TCP_MQD_BASE_ADDR=bitpacked record
  RESERVED0:bit2;
  BASE_ADDR:bit30;
 end;

 TCP_RB0_RPTR_ADDR=bitpacked record
  RB_RPTR_SWAP:bit2;
  RB_RPTR_ADDR:bit30;
 end;

 TCP_RB1_RPTR_ADDR=bitpacked record
  RB_RPTR_SWAP:bit2;
  RB_RPTR_ADDR:bit30;
 end;

 TCP_RB2_RPTR_ADDR=bitpacked record
  RB_RPTR_SWAP:bit2;
  RB_RPTR_ADDR:bit30;
 end;

 TCP_SAMPLE_STATUS=bitpacked record
  Z_PASS_ACITVE       :bit1;
  STREAMOUT_ACTIVE    :bit1;
  PIPELINE_ACTIVE     :bit1;
  STIPPLE_ACTIVE      :bit1;
  VGT_BUFFERS_ACTIVE  :bit1;
  SCREEN_EXT_ACTIVE   :bit1;
  DRAW_INDIRECT_ACTIVE:bit1;
  DISP_INDIRECT_ACTIVE:bit1;
  RESERVED0           :bit24;
 end;

 TCP_SCRATCH_INDEX=bitpacked record
  SCRATCH_INDEX:bit8;
  RESERVED0    :bit24;
 end;

 TCP_STALLED_STAT1=bitpacked record
  RBIU_TO_DMA_NOT_RDY_TO_RCV   :bit1;
  RESERVED0                    :bit1;
  RBIU_TO_SEM_NOT_RDY_TO_RCV   :bit1;
  RESERVED1                    :bit1;
  RBIU_TO_MEMWR_NOT_RDY_TO_RCV :bit1;
  RESERVED2                    :bit5;
  ME_HAS_ACTIVE_CE_BUFFER_FLAG :bit1;
  ME_HAS_ACTIVE_DE_BUFFER_FLAG :bit1;
  ME_STALLED_ON_TC_WR_CONFIRM  :bit1;
  ME_STALLED_ON_ATOMIC_RTN_DATA:bit1;
  ME_WAITING_ON_TC_READ_DATA   :bit1;
  ME_WAITING_ON_REG_READ_DATA  :bit1;
  RESERVED3                    :bit7;
  RCIU_WAITING_ON_GDS_FREE     :bit1;
  RCIU_WAITING_ON_GRBM_FREE    :bit1;
  RCIU_WAITING_ON_VGT_FREE     :bit1;
  RCIU_STALLED_ON_ME_READ      :bit1;
  RCIU_STALLED_ON_DMA_READ     :bit1;
  RCIU_STALLED_ON_APPEND_READ  :bit1;
  RCIU_HALTED_BY_REG_VIOLATION :bit1;
  RESERVED4                    :bit2;
 end;

 TCP_STALLED_STAT2=bitpacked record
  PFP_TO_CSF_NOT_RDY_TO_RCV      :bit1;
  PFP_TO_MEQ_NOT_RDY_TO_RCV      :bit1;
  PFP_TO_RCIU_NOT_RDY_TO_RCV     :bit1;
  RESERVED0                      :bit1;
  PFP_TO_VGT_WRITES_PENDING      :bit1;
  PFP_RCIU_READ_PENDING          :bit1;
  PFP_MIU_READ_PENDING           :bit1;
  PFP_TO_MIU_WRITE_NOT_RDY_TO_RCV:bit1;
  PFP_WAITING_ON_BUFFER_DATA     :bit1;
  ME_WAIT_ON_CE_COUNTER          :bit1;
  ME_WAIT_ON_AVAIL_BUFFER        :bit1;
  GFX_CNTX_NOT_AVAIL_TO_ME       :bit1;
  ME_RCIU_NOT_RDY_TO_RCV         :bit1;
  ME_TO_CONST_NOT_RDY_TO_RCV     :bit1;
  ME_WAITING_DATA_FROM_PFP       :bit1;
  ME_WAITING_ON_PARTIAL_FLUSH    :bit1;
  MEQ_TO_ME_NOT_RDY_TO_RCV       :bit1;
  STQ_TO_ME_NOT_RDY_TO_RCV       :bit1;
  ME_WAITING_DATA_FROM_STQ       :bit1;
  PFP_STALLED_ON_TC_WR_CONFIRM   :bit1;
  PFP_STALLED_ON_ATOMIC_RTN_DATA :bit1;
  EOPD_FIFO_NEEDS_SC_EOP_DONE    :bit1;
  EOPD_FIFO_NEEDS_WR_CONFIRM     :bit1;
  STRMO_WR_OF_PRIM_DATA_PENDING  :bit1;
  PIPE_STATS_WR_DATA_PENDING     :bit1;
  APPEND_RDY_WAIT_ON_CS_DONE     :bit1;
  APPEND_RDY_WAIT_ON_PS_DONE     :bit1;
  APPEND_WAIT_ON_WR_CONFIRM      :bit1;
  APPEND_ACTIVE_PARTITION        :bit1;
  APPEND_WAITING_TO_SEND_MEMWRITE:bit1;
  SURF_SYNC_NEEDS_IDLE_CNTXS     :bit1;
  SURF_SYNC_NEEDS_ALL_CLEAN      :bit1;
 end;

 TCP_STALLED_STAT3=bitpacked record
  CE_TO_CSF_NOT_RDY_TO_RCV                :bit1;
  CE_TO_RAM_INIT_FETCHER_NOT_RDY_TO_RCV   :bit1;
  CE_WAITING_ON_DATA_FROM_RAM_INIT_FETCHER:bit1;
  CE_TO_RAM_INIT_NOT_RDY                  :bit1;
  CE_TO_RAM_DUMP_NOT_RDY                  :bit1;
  CE_TO_RAM_WRITE_NOT_RDY                 :bit1;
  CE_TO_INC_FIFO_NOT_RDY_TO_RCV           :bit1;
  CE_TO_WR_FIFO_NOT_RDY_TO_RCV            :bit1;
  CE_TO_MIU_WRITE_NOT_RDY_TO_RCV__SI__CI  :bit1;
  RESERVED0                               :bit1;
  CE_WAITING_ON_BUFFER_DATA               :bit1;
  CE_WAITING_ON_CE_BUFFER_FLAG            :bit1;
  CE_WAITING_ON_DE_COUNTER                :bit1;
  CE_WAITING_ON_DE_COUNTER_UNDERFLOW      :bit1;
  TCIU_WAITING_ON_FREE                    :bit1;
  TCIU_WAITING_ON_TAGS                    :bit1;
  CE_STALLED_ON_TC_WR_CONFIRM             :bit1;
  CE_STALLED_ON_ATOMIC_RTN_DATA           :bit1;
  ATCL2IU_WAITING_ON_FREE                 :bit1;
  ATCL2IU_WAITING_ON_TAGS                 :bit1;
  ATCL1_WAITING_ON_TRANS                  :bit1;
  RESERVED1                               :bit11;
 end;

 TDB_ALPHA_TO_MASK=bitpacked record
  ALPHA_TO_MASK_ENABLE :bit1;
  RESERVED0            :bit7;
  ALPHA_TO_MASK_OFFSET0:bit2;
  ALPHA_TO_MASK_OFFSET1:bit2;
  ALPHA_TO_MASK_OFFSET2:bit2;
  ALPHA_TO_MASK_OFFSET3:bit2;
  OFFSET_ROUND         :bit1;
  RESERVED1            :bit15;
 end;

 TDB_COUNT_CONTROL=bitpacked record
  ZPASS_INCREMENT_DISABLE:bit1;
  PERFECT_ZPASS_COUNTS   :bit1;
  RESERVED0              :bit2;
  SAMPLE_RATE            :bit3;
  RESERVED1              :bit1;
  ZPASS_ENABLE           :bit4;
  ZFAIL_ENABLE           :bit4;
  SFAIL_ENABLE           :bit4;
  DBFAIL_ENABLE          :bit4;
  SLICE_EVEN_ENABLE      :bit4;
  SLICE_ODD_ENABLE       :bit4;
 end;

 TDB_DEPTH_CONTROL=bitpacked record
  STENCIL_ENABLE                    :bit1;
  Z_ENABLE                          :bit1;
  Z_WRITE_ENABLE                    :bit1;
  DEPTH_BOUNDS_ENABLE               :bit1;
  ZFUNC                             :bit3;
  BACKFACE_ENABLE                   :bit1;
  STENCILFUNC                       :bit3;
  RESERVED0                         :bit9;
  STENCILFUNC_BF                    :bit3;
  RESERVED1                         :bit7;
  ENABLE_COLOR_WRITES_ON_DEPTH_FAIL :bit1;
  DISABLE_COLOR_WRITES_ON_DEPTH_PASS:bit1;
 end;

 TDB_HTILE_SURFACE=bitpacked record
  LINEAR                 :bit1;
  FULL_CACHE             :bit1;
  HTILE_USES_PRELOAD_WIN :bit1;
  PRELOAD                :bit1;
  PREFETCH_WIDTH         :bit6;
  PREFETCH_HEIGHT        :bit6;
  DST_OUTSIDE_ZERO_TO_ONE:bit1;
  TC_COMPATIBLE          :bit1;
  RESERVED0              :bit14;
 end;

 TDB_STENCIL_CLEAR=bitpacked record
  CLEAR    :bit8;
  RESERVED0:bit24;
 end;

 TGDS_ATOM_OFFSET0=bitpacked record
  OFFSET0:bit8;
  UNUSED :bit24;
 end;

 TGDS_ATOM_OFFSET1=bitpacked record
  OFFSET1:bit8;
  UNUSED :bit24;
 end;

 TGDS_ATOM_READ0_U=bit32;

 TGDS_ATOM_READ1_U=bit32;

 TGDS_EDC_GRBM_CNT=bitpacked record
  DED      :bit8;
  RESERVED0:bit8;
  SEC      :bit8;
  RESERVED1:bit8;
 end;

 TGDS_GWS_RESOURCE=bitpacked record
  FLAG       :bit1;
  COUNTER    :bit12;
  _TYPE      :bit1;
  DED        :bit1;
  RELEASE_ALL:bit1;
  HEAD_QUEUE :bit12;
  HEAD_VALID :bit1;
  HEAD_FLAG  :bit1;
  UNUSED1    :bit2;
 end;

 TGDS_OA_RING_SIZE=bit32;

 TGRBM_READ_ERROR2=bitpacked record
  RESERVED0                 :bit17;
  READ_REQUESTER_SRBM       :bit1;
  READ_REQUESTER_RLC        :bit1;
  READ_REQUESTER_GDS_DMA    :bit1;
  READ_REQUESTER_ME0PIPE0_CF:bit1;
  READ_REQUESTER_ME0PIPE0_PF:bit1;
  READ_REQUESTER_ME0PIPE1_CF:bit1;
  READ_REQUESTER_ME0PIPE1_PF:bit1;
  READ_REQUESTER_ME1PIPE0   :bit1;
  READ_REQUESTER_ME1PIPE1   :bit1;
  READ_REQUESTER_ME1PIPE2   :bit1;
  READ_REQUESTER_ME1PIPE3   :bit1;
  READ_REQUESTER_ME2PIPE0   :bit1;
  READ_REQUESTER_ME2PIPE1   :bit1;
  READ_REQUESTER_ME2PIPE2   :bit1;
  READ_REQUESTER_ME2PIPE3   :bit1;
 end;

 TGRBM_TRAP_WD_MSK=bit32;

 TGRBM_WRITE_ERROR=bitpacked record
  WRITE_REQUESTER_RLC :bit1;
  WRITE_REQUESTER_SRBM:bit1;
  WRITE_SSRCID        :bit3;
  WRITE_VFID          :bit4;
  RESERVED0           :bit3;
  WRITE_VF            :bit1;
  WRITE_VMID          :bit4;
  RESERVED1           :bit3;
  WRITE_PIPEID        :bit2;
  WRITE_MEID          :bit2;
  RESERVED2           :bit7;
  WRITE_ERROR         :bit1;
 end;

 TPA_CL_POINT_SIZE=bit32;

 TPA_SC_DEBUG_CNTL=bitpacked record
  SC_DEBUG_INDX:bit6;
  RESERVED0    :bit26;
 end;

 TPA_SC_DEBUG_DATA=bit32;

 TPA_SC_DEBUG_REG0=bitpacked record
  REG0_FIELD0:bit2;
  REG0_FIELD1:bit2;
  RESERVED0  :bit28;
 end;

 TPA_SC_DEBUG_REG1=bitpacked record
  REG1_FIELD0:bit2;
  REG1_FIELD1:bit2;
  RESERVED0  :bit28;
 end;

 TPA_SU_DEBUG_CNTL=bitpacked record
  SU_DEBUG_INDX:bit5;
  RESERVED0    :bit27;
 end;

 TPA_SU_DEBUG_DATA=bit32;

 TPA_SU_POINT_SIZE=bitpacked record
  HEIGHT:bit16;
  WIDTH :bit16;
 end;

 TSPI_ARB_CYCLES_0=bitpacked record
  TS0_DURATION:bit16;
  TS1_DURATION:bit16;
 end;

 TSPI_ARB_CYCLES_1=bitpacked record
  TS2_DURATION:bit16;
  TS3_DURATION:bit16;
 end;

 TSPI_ARB_PRIORITY=bitpacked record
  PIPE_ORDER_TS0:bit3;
  PIPE_ORDER_TS1:bit3;
  PIPE_ORDER_TS2:bit3;
  PIPE_ORDER_TS3:bit3;
  TS0_DUR_MULT  :bit2;
  TS1_DUR_MULT  :bit2;
  TS2_DUR_MULT  :bit2;
  TS3_DUR_MULT  :bit2;
  RESERVED0     :bit12;
 end;

 TSPI_CDBG_SYS_CS0=bitpacked record
  PIPE0:bit8;
  PIPE1:bit8;
  PIPE2:bit8;
  PIPE3:bit8;
 end;

 TSPI_CDBG_SYS_CS1=bitpacked record
  PIPE0:bit8;
  PIPE1:bit8;
  PIPE2:bit8;
  PIPE3:bit8;
 end;

 TSPI_CDBG_SYS_GFX=bitpacked record
  PS_EN    :bit1;
  VS_EN    :bit1;
  GS_EN    :bit1;
  ES_EN    :bit1;
  HS_EN    :bit1;
  LS_EN    :bit1;
  CS_EN    :bit1;
  RESERVED0:bit25;
 end;

 TSPI_PS_INPUT_ENA=bitpacked record
  PERSP_SAMPLE_ENA    :bit1;
  PERSP_CENTER_ENA    :bit1;
  PERSP_CENTROID_ENA  :bit1;
  PERSP_PULL_MODEL_ENA:bit1;
  LINEAR_SAMPLE_ENA   :bit1;
  LINEAR_CENTER_ENA   :bit1;
  LINEAR_CENTROID_ENA :bit1;
  LINE_STIPPLE_TEX_ENA:bit1;
  POS_X_FLOAT_ENA     :bit1;
  POS_Y_FLOAT_ENA     :bit1;
  POS_Z_FLOAT_ENA     :bit1;
  POS_W_FLOAT_ENA     :bit1;
  FRONT_FACE_ENA      :bit1;
  ANCILLARY_ENA       :bit1;
  SAMPLE_COVERAGE_ENA :bit1;
  POS_FIXED_PT_ENA    :bit1;
  RESERVED0           :bit16;
 end;

 TSPI_TMPRING_SIZE=bitpacked record
  WAVES    :bit12;
  WAVESIZE :bit13;
  RESERVED0:bit7;
 end;

 TTA_RESERVED_010C=bit32;

 TVGT_DMA_MAX_SIZE=bit32;

 TVGT_LS_HS_CONFIG=bitpacked record
  NUM_PATCHES     :bit8;
  HS_NUM_INPUT_CP :bit6;
  HS_NUM_OUTPUT_CP:bit6;
  RESERVED0       :bit12;
 end;

 TVGT_MAX_VTX_INDX=bit32;

 TVGT_MIN_VTX_INDX=bit32;

 TVGT_TF_RING_SIZE=bitpacked record
  SIZE     :bit16;
  RESERVED0:bit16;
 end;

 TCB_BLEND0_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND1_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND2_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND3_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND4_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND5_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND6_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_BLEND7_CONTROL=bitpacked record
  COLOR_SRCBLEND      :bit5;
  COLOR_COMB_FCN      :bit3;
  COLOR_DESTBLEND     :bit5;
  RESERVED0           :bit3;
  ALPHA_SRCBLEND      :bit5;
  ALPHA_COMB_FCN      :bit3;
  ALPHA_DESTBLEND     :bit5;
  SEPARATE_ALPHA_BLEND:bit1;
  ENABLE              :bit1;
  DISABLE_ROP3        :bit1;
 end;

 TCB_CGTT_SCLK_CTRL=bitpacked record
  ON_DELAY      :bit4;
  OFF_HYSTERESIS:bit8;
  RESERVED0     :bit12;
  SOFT_OVERRIDE7:bit1;
  SOFT_OVERRIDE6:bit1;
  SOFT_OVERRIDE5:bit1;
  SOFT_OVERRIDE4:bit1;
  SOFT_OVERRIDE3:bit1;
  SOFT_OVERRIDE2:bit1;
  SOFT_OVERRIDE1:bit1;
  SOFT_OVERRIDE0:bit1;
 end;

 TCOMPUTE_PGM_RSRC1=bitpacked record
  VGPRS     :bit6;
  SGPRS     :bit4;
  PRIORITY  :bit2;
  FLOAT_MODE:bit8;
  PRIV      :bit1;
  DX10_CLAMP:bit1;
  DEBUG_MODE:bit1;
  IEEE_MODE :bit1;
  BULKY     :bit1;
  CDBG_USER :bit1;
  RESERVED0 :bit6;
 end;

 TCOMPUTE_PGM_RSRC2=bitpacked record
  SCRATCH_EN    :bit1;
  USER_SGPR     :bit5;
  TRAP_PRESENT  :bit1;
  TGID_X_EN     :bit1;
  TGID_Y_EN     :bit1;
  TGID_Z_EN     :bit1;
  TG_SIZE_EN    :bit1;
  TIDIG_COMP_CNT:bit2;
  EXCP_EN_MSB   :bit2;
  LDS_SIZE      :bit9;
  EXCP_EN       :bit7;
  RESERVED0     :bit1;
 end;

 TCOMPUTE_RESTART_X=bit32;

 TCOMPUTE_RESTART_Y=bit32;

 TCOMPUTE_RESTART_Z=bit32;

 TCP_APPEND_ADDR_HI=bitpacked record
  MEM_ADDR_HI :bit16;
  CS_PS_SEL   :bit1;
  RESERVED0   :bit8;
  CACHE_POLICY:bit1;
  RESERVED1   :bit1;
  MTYPE       :bit2;
  COMMAND     :bit3;
 end;

 TCP_APPEND_ADDR_LO=bitpacked record
  RESERVED0  :bit2;
  MEM_ADDR_LO:bit30;
 end;

 TCP_CE_HEADER_DUMP=bit32;

 TCP_CE_IB1_BASE_HI=bitpacked record
  IB1_BASE_HI:bit16;
  RESERVED0  :bit16;
 end;

 TCP_CE_IB1_BASE_LO=bitpacked record
  RESERVED0  :bit2;
  IB1_BASE_LO:bit30;
 end;

 TCP_CE_IB2_BASE_HI=bitpacked record
  IB2_BASE_HI:bit16;
  RESERVED0  :bit16;
 end;

 TCP_CE_IB2_BASE_LO=bitpacked record
  RESERVED0  :bit2;
  IB2_BASE_LO:bit30;
 end;

 TCP_CE_ROQ_RB_STAT=bitpacked record
  CEQ_RPTR_PRIMARY:bit10;
  RESERVED0       :bit6;
  CEQ_WPTR_PRIMARY:bit10;
  RESERVED1       :bit6;
 end;

 TCP_CPC_IC_BASE_HI=bitpacked record
  IC_BASE_HI:bit16;
  RESERVED0 :bit16;
 end;

 TCP_CPC_IC_BASE_LO=bitpacked record
  RESERVED0 :bit12;
  IC_BASE_LO:bit20;
 end;

 TCP_CPC_IC_OP_CNTL=bitpacked record
  INVALIDATE_CACHE:bit1;
  RESERVED0       :bit3;
  PRIME_ICACHE    :bit1;
  ICACHE_PRIMED   :bit1;
  RESERVED1       :bit26;
 end;

 TCP_DMA_ME_COMMAND=bitpacked record
  BYTE_COUNT:bit21;
  DIS_WC    :bit1;
  SRC_SWAP  :bit2;
  DST_SWAP  :bit2;
  SAS       :bit1;
  DAS       :bit1;
  SAIC      :bit1;
  DAIC      :bit1;
  RAW_WAIT  :bit1;
  RESERVED0 :bit1;
 end;

 TCP_DMA_ME_CONTROL=bitpacked record
  RESERVED0       :bit10;
  SRC_MTYPE       :bit2;
  SRC_ATC         :bit1;
  SRC_CACHE_POLICY:bit1;
  RESERVED1       :bit6;
  DST_SELECT      :bit2;
  DST_MTYPE       :bit2;
  DST_ATC         :bit1;
  DST_CACHE_POLICY:bit1;
  RESERVED2       :bit3;
  SRC_SELECT      :bit2;
  RESERVED3       :bit1;
 end;

 TCP_DRAW_WINDOW_HI=bit32;

 TCP_DRAW_WINDOW_LO=bitpacked record
  MIN:bit16;
  MAX:bit16;
 end;

 TCP_HQD_EOP_EVENTS=bitpacked record
  EVENT_COUNT          :bit12;
  RESERVED0            :bit4;
  CS_PARTIAL_FLUSH_PEND:bit1;
  RESERVED1            :bit15;
 end;

 TCP_HQD_HQ_STATUS0=bitpacked record
  DEQUEUE_STATUS   :bit2;
  DEQUEUE_RETRY_CNT:bit2;
  RSV_6_4          :bit3;
  SCRATCH_RAM_INIT :bit1;
  TCL2_DIRTY       :bit1;
  PG_ACTIVATED     :bit1;
  RSVR_31_10       :bit22;
 end;

 TCP_HQD_HQ_STATUS1=bit32;

 TCP_HQD_IB_CONTROL=bitpacked record
  IB_SIZE          :bit20;
  MIN_IB_AVAIL_SIZE:bit2;
  RESERVED0        :bit1;
  IB_ATC           :bit1;
  IB_CACHE_POLICY  :bit1;
  RESERVED1        :bit2;
  MTYPE            :bit2;
  RESERVED2        :bit1;
  RESERVED3        :bit1;
  PROCESSING_IB    :bit1;
 end;

 TCP_HQD_PQ_BASE_HI=bitpacked record
  ADDR_HI  :bit8;
  RESERVED0:bit24;
 end;

 TCP_HQD_PQ_CONTROL=bitpacked record
  QUEUE_SIZE     :bit6;
  RESERVED0      :bit2;
  RPTR_BLOCK_SIZE:bit6;
  RESERVED1      :bit1;
  MTYPE          :bit2;
  ENDIAN_SWAP    :bit2;
  RESERVED2      :bit1;
  MIN_AVAIL_SIZE :bit2;
  RESERVED3      :bit1;
  PQ_ATC         :bit1;
  CACHE_POLICY   :bit1;
  SLOT_BASED_WPTR:bit2;
  NO_UPDATE_RPTR :bit1;
  UNORD_DISPATCH :bit1;
  ROQ_PQ_IB_FLIP :bit1;
  PRIV_STATE     :bit1;
  KMD_QUEUE      :bit1;
 end;

 TCP_INT_CNTL_RING0=bitpacked record
  RESERVED0                    :bit11;
  CP_VM_DOORBELL_WR_INT_ENABLE :bit1;
  RESERVED1                    :bit2;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  RESERVED2                    :bit2;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  CMP_BUSY_INT_ENABLE          :bit1;
  CNTX_BUSY_INT_ENABLE         :bit1;
  CNTX_EMPTY_INT_ENABLE        :bit1;
  GFX_IDLE_INT_ENABLE          :bit1;
  PRIV_INSTR_INT_ENABLE        :bit1;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_INT_CNTL_RING1=bitpacked record
  RESERVED0                    :bit11;
  CP_VM_DOORBELL_WR_INT_ENABLE :bit1;
  RESERVED1                    :bit2;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  RESERVED2                    :bit2;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  CMP_BUSY_INT_ENABLE          :bit1;
  CNTX_BUSY_INT_ENABLE         :bit1;
  CNTX_EMPTY_INT_ENABLE        :bit1;
  GFX_IDLE_INT_ENABLE          :bit1;
  PRIV_INSTR_INT_ENABLE        :bit1;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_INT_CNTL_RING2=bitpacked record
  RESERVED0                    :bit11;
  CP_VM_DOORBELL_WR_INT_ENABLE :bit1;
  RESERVED1                    :bit2;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  RESERVED2                    :bit2;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  CMP_BUSY_INT_ENABLE          :bit1;
  CNTX_BUSY_INT_ENABLE         :bit1;
  CNTX_EMPTY_INT_ENABLE        :bit1;
  GFX_IDLE_INT_ENABLE          :bit1;
  PRIV_INSTR_INT_ENABLE        :bit1;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_INT_STAT_DEBUG=bitpacked record
  RESERVED0                      :bit11;
  CP_VM_DOORBELL_WR_INT_ASSERTED :bit1;
  RESERVED1                      :bit2;
  CP_ECC_ERROR_INT_ASSERTED      :bit1;
  RESERVED2                      :bit2;
  WRM_POLL_TIMEOUT_INT_ASSERTED  :bit1;
  CMP_BUSY_INT_ASSERTED          :bit1;
  CNTX_BUSY_INT_ASSERTED         :bit1;
  CNTX_EMPTY_INT_ASSERTED        :bit1;
  GFX_IDLE_INT_ASSERTED          :bit1;
  PRIV_INSTR_INT_ASSERTED        :bit1;
  PRIV_REG_INT_ASSERTED          :bit1;
  OPCODE_ERROR_INT_ASSERTED      :bit1;
  RESERVED3                      :bit1;
  TIME_STAMP_INT_ASSERTED        :bit1;
  RESERVED_BIT_ERROR_INT_ASSERTED:bit1;
  RESERVED4                      :bit1;
  GENERIC2_INT_ASSERTED          :bit1;
  GENERIC1_INT_ASSERTED          :bit1;
  GENERIC0_INT_ASSERTED          :bit1;
 end;

 TCP_ME0_PIPE0_VMID=bitpacked record
  VMID     :bit4;
  RESERVED0:bit28;
 end;

 TCP_ME0_PIPE1_VMID=bitpacked record
  VMID     :bit4;
  RESERVED0:bit28;
 end;

 TCP_MEQ_THRESHOLDS=bitpacked record
  MEQ1_START:bit8;
  MEQ2_START:bit8;
  RESERVED0 :bit16;
 end;

 TCP_PFP_IB_CONTROL=bitpacked record
  IB_EN    :bit8;
  RESERVED0:bit24;
 end;

 TCP_PFP_UCODE_ADDR=bitpacked record
  UCODE_ADDR:bit13;
  RESERVED0 :bit19;
 end;

 TCP_PFP_UCODE_DATA=bit32;

 TCP_ROQ_THRESHOLDS=bitpacked record
  IB1_START:bit8;
  IB2_START:bit8;
  RESERVED0:bit16;
 end;

 TCP_SEM_WAIT_TIMER=bit32;

 TCP_STQ_THRESHOLDS=bitpacked record
  STQ0_START:bit8;
  STQ1_START:bit8;
  STQ2_START:bit8;
  RESERVED0 :bit8;
 end;

 TDB_RENDER_CONTROL=bitpacked record
  DEPTH_CLEAR_ENABLE      :bit1;
  STENCIL_CLEAR_ENABLE    :bit1;
  DEPTH_COPY              :bit1;
  STENCIL_COPY            :bit1;
  RESUMMARIZE_ENABLE      :bit1;
  STENCIL_COMPRESS_DISABLE:bit1;
  DEPTH_COMPRESS_DISABLE  :bit1;
  COPY_CENTROID           :bit1;
  COPY_SAMPLE             :bit4;
  DECOMPRESS_ENABLE       :bit1;
  RESERVED0               :bit19;
 end;

 TDB_SHADER_CONTROL=bitpacked record
  Z_EXPORT_ENABLE               :bit1;
  STENCIL_TEST_VAL_EXPORT_ENABLE:bit1;
  STENCIL_OP_VAL_EXPORT_ENABLE  :bit1;
  RESERVED0                     :bit1;
  Z_ORDER                       :bit2;
  KILL_ENABLE                   :bit1;
  COVERAGE_TO_MASK_ENABLE       :bit1;
  MASK_EXPORT_ENABLE            :bit1;
  EXEC_ON_HIER_FAIL             :bit1;
  EXEC_ON_NOOP                  :bit1;
  ALPHA_TO_MASK_DISABLE         :bit1;
  DEPTH_BEFORE_SHADER           :bit1;
  CONSERVATIVE_Z_EXPORT         :bit2;
  DUAL_QUAD_DISABLE             :bit1;
  RESERVED1                     :bit16;
 end;

 TDB_STENCILREFMASK=bitpacked record
  STENCILTESTVAL  :bit8;
  STENCILMASK     :bit8;
  STENCILWRITEMASK:bit8;
  STENCILOPVAL    :bit8;
 end;

 TDB_ZPASS_COUNT_HI=bitpacked record
  COUNT_HI :bit31;
  RESERVED0:bit1;
 end;

 TGDS_ATOM_COMPLETE=bitpacked record
  COMPLETE:bit1;
  UNUSED  :bit31;
 end;

 TGDS_CS_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_CS_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_CS_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_CS_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_OA_RESET_MASK=bitpacked record
  ME0_GFXHP3D_PIX_RESET:bit1;
  ME0_GFXHP3D_VTX_RESET:bit1;
  ME0_CS_RESET         :bit1;
  RESERVED0            :bit1;
  ME1_PIPE0_RESET      :bit1;
  ME1_PIPE1_RESET      :bit1;
  ME1_PIPE2_RESET      :bit1;
  ME1_PIPE3_RESET      :bit1;
  ME2_PIPE0_RESET      :bit1;
  ME2_PIPE1_RESET      :bit1;
  ME2_PIPE2_RESET      :bit1;
  ME2_PIPE3_RESET      :bit1;
  RESERVED1            :bit20;
 end;

 TGDS_RD_BURST_ADDR=bit32;

 TGDS_RD_BURST_DATA=bit32;

 TGDS_VS_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_VS_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_VS_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_VS_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_WR_BURST_ADDR=bit32;

 TGDS_WR_BURST_DATA=bit32;

 TGRBM_HYP_CAM_DATA=bitpacked record
  CAM_ADDR     :bit16;
  CAM_REMAPADDR:bit16;
 end;

 TGRBM_SCRATCH_REG0=bit32;

 TGRBM_SCRATCH_REG1=bit32;

 TGRBM_SCRATCH_REG2=bit32;

 TGRBM_SCRATCH_REG3=bit32;

 TGRBM_SCRATCH_REG4=bit32;

 TGRBM_SCRATCH_REG5=bit32;

 TGRBM_SCRATCH_REG6=bit32;

 TGRBM_SCRATCH_REG7=bit32;

 TPA_CL_CNTL_STATUS=bitpacked record
  RESERVED0:bit31;
  CL_BUSY  :bit1;
 end;

 TPA_CL_NANINF_CNTL=bitpacked record
  VTE_XY_INF_DISCARD      :bit1;
  VTE_Z_INF_DISCARD       :bit1;
  VTE_W_INF_DISCARD       :bit1;
  VTE_0XNANINF_IS_0       :bit1;
  VTE_XY_NAN_RETAIN       :bit1;
  VTE_Z_NAN_RETAIN        :bit1;
  VTE_W_NAN_RETAIN        :bit1;
  VTE_W_RECIP_NAN_IS_0    :bit1;
  VS_XY_NAN_TO_INF        :bit1;
  VS_XY_INF_RETAIN        :bit1;
  VS_Z_NAN_TO_INF         :bit1;
  VS_Z_INF_RETAIN         :bit1;
  VS_W_NAN_TO_INF         :bit1;
  VS_W_INF_RETAIN         :bit1;
  VS_CLIP_DIST_INF_DISCARD:bit1;
  RESERVED0               :bit5;
  VTE_NO_OUTPUT_NEG_0     :bit1;
  RESERVED1               :bit11;
 end;

 TPA_CL_POINT_X_RAD=bit32;

 TPA_CL_POINT_Y_RAD=bit32;

 TPA_CL_RESET_DEBUG=bitpacked record
  CL_TRIV_DISC_DISABLE:bit1;
  RESERVED0           :bit31;
 end;

 TPA_CL_VS_OUT_CNTL=bitpacked record
  CLIP_DIST_ENA_0           :bit1;
  CLIP_DIST_ENA_1           :bit1;
  CLIP_DIST_ENA_2           :bit1;
  CLIP_DIST_ENA_3           :bit1;
  CLIP_DIST_ENA_4           :bit1;
  CLIP_DIST_ENA_5           :bit1;
  CLIP_DIST_ENA_6           :bit1;
  CLIP_DIST_ENA_7           :bit1;
  CULL_DIST_ENA_0           :bit1;
  CULL_DIST_ENA_1           :bit1;
  CULL_DIST_ENA_2           :bit1;
  CULL_DIST_ENA_3           :bit1;
  CULL_DIST_ENA_4           :bit1;
  CULL_DIST_ENA_5           :bit1;
  CULL_DIST_ENA_6           :bit1;
  CULL_DIST_ENA_7           :bit1;
  USE_VTX_POINT_SIZE        :bit1;
  USE_VTX_EDGE_FLAG         :bit1;
  USE_VTX_RENDER_TARGET_INDX:bit1;
  USE_VTX_VIEWPORT_INDX     :bit1;
  USE_VTX_KILL_FLAG         :bit1;
  VS_OUT_MISC_VEC_ENA       :bit1;
  VS_OUT_CCDIST0_VEC_ENA    :bit1;
  VS_OUT_CCDIST1_VEC_ENA    :bit1;
  VS_OUT_MISC_SIDE_BUS_ENA  :bit1;
  USE_VTX_GS_CUT_FLAG       :bit1;
  USE_VTX_LINE_WIDTH        :bit1;
  RESERVED0                 :bit5;
 end;

 TPA_SC_MODE_CNTL_0=bitpacked record
  MSAA_ENABLE             :bit1;
  VPORT_SCISSOR_ENABLE    :bit1;
  LINE_STIPPLE_ENABLE     :bit1;
  SEND_UNLIT_STILES_TO_PKR:bit1;
  RESERVED0               :bit28;
 end;

 TPA_SC_MODE_CNTL_1=bitpacked record
  WALK_SIZE                              :bit1;
  WALK_ALIGNMENT                         :bit1;
  WALK_ALIGN8_PRIM_FITS_ST               :bit1;
  WALK_FENCE_ENABLE                      :bit1;
  WALK_FENCE_SIZE                        :bit3;
  SUPERTILE_WALK_ORDER_ENABLE            :bit1;
  TILE_WALK_ORDER_ENABLE                 :bit1;
  TILE_COVER_DISABLE                     :bit1;
  TILE_COVER_NO_SCISSOR                  :bit1;
  ZMM_LINE_EXTENT                        :bit1;
  ZMM_LINE_OFFSET                        :bit1;
  ZMM_RECT_EXTENT                        :bit1;
  KILL_PIX_POST_HI_Z                     :bit1;
  KILL_PIX_POST_DETAIL_MASK              :bit1;
  PS_ITER_SAMPLE                         :bit1;
  MULTI_SHADER_ENGINE_PRIM_DISCARD_ENABLE:bit1;
  MULTI_GPU_SUPERTILE_ENABLE             :bit1;
  GPU_ID_OVERRIDE_ENABLE                 :bit1;
  GPU_ID_OVERRIDE                        :bit4;
  MULTI_GPU_PRIM_DISCARD_ENABLE          :bit1;
  FORCE_EOV_CNTDWN_ENABLE                :bit1;
  FORCE_EOV_REZ_ENABLE                   :bit1;
  OUT_OF_ORDER_PRIMITIVE_ENABLE          :bit1;
  OUT_OF_ORDER_WATER_MARK                :bit3;
  RESERVED0                              :bit1;
 end;

 TPA_SU_CNTL_STATUS=bitpacked record
  RESERVED0:bit31;
  SU_BUSY  :bit1;
 end;

 TSPI_CDBG_SYS_HP3D=bitpacked record
  PS_EN    :bit1;
  VS_EN    :bit1;
  GS_EN    :bit1;
  ES_EN    :bit1;
  HS_EN    :bit1;
  LS_EN    :bit1;
  RESERVED0:bit26;
 end;

 TSPI_CONFIG_CNTL_1=bitpacked record
  VTX_DONE_DELAY           :bit4;
  INTERP_ONE_PRIM_PER_ROW  :bit1;
  RESERVED0                :bit1;
  PC_LIMIT_ENABLE          :bit1;
  PC_LIMIT_STRICT          :bit1;
  CRC_SIMD_ID_WADDR_DISABLE:bit1;
  LBPW_CU_CHK_MODE         :bit1;
  LBPW_CU_CHK_CNT          :bit4;
  RESERVED1                :bit2;
  PC_LIMIT_SIZE            :bit16;
 end;

 TSPI_CONFIG_CNTL_2=bitpacked record
  CONTEXT_SAVE_WAIT_GDS_REQUEST_CYCLE_OVHD:bit4;
  CONTEXT_SAVE_WAIT_GDS_GRANT_CYCLE_OVHD  :bit4;
  RESERVED0                               :bit24;
 end;

 TSPI_PS_INPUT_ADDR=bitpacked record
  PERSP_SAMPLE_ENA    :bit1;
  PERSP_CENTER_ENA    :bit1;
  PERSP_CENTROID_ENA  :bit1;
  PERSP_PULL_MODEL_ENA:bit1;
  LINEAR_SAMPLE_ENA   :bit1;
  LINEAR_CENTER_ENA   :bit1;
  LINEAR_CENTROID_ENA :bit1;
  LINE_STIPPLE_TEX_ENA:bit1;
  POS_X_FLOAT_ENA     :bit1;
  POS_Y_FLOAT_ENA     :bit1;
  POS_Z_FLOAT_ENA     :bit1;
  POS_W_FLOAT_ENA     :bit1;
  FRONT_FACE_ENA      :bit1;
  ANCILLARY_ENA       :bit1;
  SAMPLE_COVERAGE_ENA :bit1;
  POS_FIXED_PT_ENA    :bit1;
  RESERVED0           :bit16;
 end;

 TSPI_PS_IN_CONTROL=bitpacked record
  NUM_INTERP         :bit6;
  PARAM_GEN          :bit1;
  RESERVED0          :bit7;
  BC_OPTIMIZE_DISABLE:bit1;
  RESERVED1          :bit1;
  RESERVED2          :bit16;
 end;

 TSPI_VS_OUT_CONFIG=bitpacked record
  RESERVED0      :bit1;
  VS_EXPORT_COUNT:bit5;
  VS_HALF_PACK   :bit1;
  RESERVED1      :bit1;
  RESERVED2      :bit5;
  RESERVED3      :bit19;
 end;

 TSQ_BUF_RSRC_WORD0=bit32;

 TSQ_BUF_RSRC_WORD1=bitpacked record
  BASE_ADDRESS_HI:bit16;
  STRIDE         :bit14;
  CACHE_SWIZZLE  :bit1;
  SWIZZLE_ENABLE :bit1;
 end;

 TSQ_BUF_RSRC_WORD2=bit32;

 TSQ_BUF_RSRC_WORD3=bitpacked record
  DST_SEL_X     :bit3;
  DST_SEL_Y     :bit3;
  DST_SEL_Z     :bit3;
  DST_SEL_W     :bit3;
  NUM_FORMAT    :bit3;
  DATA_FORMAT   :bit4;
  ELEMENT_SIZE  :bit2;
  INDEX_STRIDE  :bit2;
  ADD_TID_ENABLE:bit1;
  ATC           :bit1;
  HASH_ENABLE   :bit1;
  HEAP          :bit1;
  MTYPE         :bit3;
  _TYPE         :bit2;
 end;

 TSQ_IMG_RSRC_WORD0=bit32;

 TSQ_IMG_RSRC_WORD1=bitpacked record
  BASE_ADDRESS_HI:bit8;
  MIN_LOD        :bit12;
  DATA_FORMAT    :bit6;
  NUM_FORMAT     :bit4;
  MTYPE          :bit2;
 end;

 TSQ_IMG_RSRC_WORD2=bitpacked record
  WIDTH     :bit14;
  HEIGHT    :bit14;
  PERF_MOD  :bit3;
  INTERLACED:bit1;
 end;

 TSQ_IMG_RSRC_WORD3=bitpacked record
  DST_SEL_X   :bit3;
  DST_SEL_Y   :bit3;
  DST_SEL_Z   :bit3;
  DST_SEL_W   :bit3;
  BASE_LEVEL  :bit4;
  LAST_LEVEL  :bit4;
  TILING_INDEX:bit5;
  POW2_PAD    :bit1;
  MTYPE       :bit1;
  ATC         :bit1;
  _TYPE       :bit4;
 end;

 TSQ_IMG_RSRC_WORD4=bitpacked record
  DEPTH    :bit13;
  PITCH    :bit14;
  RESERVED0:bit5;
 end;

 TSQ_IMG_RSRC_WORD5=bitpacked record
  BASE_ARRAY:bit13;
  LAST_ARRAY:bit13;
  RESERVED0 :bit6;
 end;

 TSQ_IMG_RSRC_WORD6=bitpacked record
  MIN_LOD_WARN   :bit12;
  COUNTER_BANK_ID:bit8;
  LOD_HDW_CNT_EN :bit1;
  COMPRESSION_EN :bit1;
  ALPHA_IS_ON_MSB:bit1;
  COLOR_TRANSFORM:bit1;
  LOST_ALPHA_BITS:bit4;
  LOST_COLOR_BITS:bit4;
 end;

 TSQ_IMG_RSRC_WORD7=bit32;

 TSQ_IMG_SAMP_WORD0=bitpacked record
  CLAMP_X           :bit3;
  CLAMP_Y           :bit3;
  CLAMP_Z           :bit3;
  MAX_ANISO_RATIO   :bit3;
  DEPTH_COMPARE_FUNC:bit3;
  FORCE_UNNORMALIZED:bit1;
  ANISO_THRESHOLD   :bit3;
  MC_COORD_TRUNC    :bit1;
  FORCE_DEGAMMA     :bit1;
  ANISO_BIAS        :bit6;
  TRUNC_COORD       :bit1;
  DISABLE_CUBE_WRAP :bit1;
  FILTER_MODE       :bit2;
  COMPAT_MODE       :bit1;
 end;

 TSQ_IMG_SAMP_WORD1=bitpacked record
  MIN_LOD :bit12;
  MAX_LOD :bit12;
  PERF_MIP:bit4;
  PERF_Z  :bit4;
 end;

 TSQ_IMG_SAMP_WORD2=bitpacked record
  LOD_BIAS          :bit14;
  LOD_BIAS_SEC      :bit6;
  XY_MAG_FILTER     :bit2;
  XY_MIN_FILTER     :bit2;
  Z_FILTER          :bit2;
  MIP_FILTER        :bit2;
  MIP_POINT_PRECLAMP:bit1;
  DISABLE_LSB_CEIL  :bit1;
  FILTER_PREC_FIX   :bit1;
  ANISO_OVERRIDE    :bit1;
 end;

 TSQ_IMG_SAMP_WORD3=bitpacked record
  BORDER_COLOR_PTR :bit12;
  RESERVED0        :bit18;
  BORDER_COLOR_TYPE:bit2;
 end;

 TSX_MRT0_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT1_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT2_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT3_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT4_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT5_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT6_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_MRT7_BLEND_OPT=bitpacked record
  COLOR_SRC_OPT :bit3;
  RESERVED0     :bit1;
  COLOR_DST_OPT :bit3;
  RESERVED1     :bit1;
  COLOR_COMB_FCN:bit3;
  RESERVED2     :bit5;
  ALPHA_SRC_OPT :bit3;
  RESERVED3     :bit1;
  ALPHA_DST_OPT :bit3;
  RESERVED4     :bit1;
  ALPHA_COMB_FCN:bit3;
  RESERVED5     :bit5;
 end;

 TSX_PS_DOWNCONVERT=bitpacked record
  MRT0:bit4;
  MRT1:bit4;
  MRT2:bit4;
  MRT3:bit4;
  MRT4:bit4;
  MRT5:bit4;
  MRT6:bit4;
  MRT7:bit4;
 end;

 TTCP_CHAN_STEER_HI=bitpacked record
  CHAN8:bit4;
  CHAN9:bit4;
  CHANA:bit4;
  CHANB:bit4;
  CHANC:bit4;
  CHAND:bit4;
  CHANE:bit4;
  CHANF:bit4;
 end;

 TTCP_CHAN_STEER_LO=bitpacked record
  CHAN0:bit4;
  CHAN1:bit4;
  CHAN2:bit4;
  CHAN3:bit4;
  CHAN4:bit4;
  CHAN5:bit4;
  CHAN6:bit4;
  CHAN7:bit4;
 end;

 TTCP_WATCH0_ADDR_H=bitpacked record
  ADDR     :bit16;
  RESERVED0:bit16;
 end;

 TTCP_WATCH0_ADDR_L=bitpacked record
  RESERVED0:bit6;
  ADDR     :bit26;
 end;

 TTCP_WATCH1_ADDR_H=bitpacked record
  ADDR     :bit16;
  RESERVED0:bit16;
 end;

 TTCP_WATCH1_ADDR_L=bitpacked record
  RESERVED0:bit6;
  ADDR     :bit26;
 end;

 TTCP_WATCH2_ADDR_H=bitpacked record
  ADDR     :bit16;
  RESERVED0:bit16;
 end;

 TTCP_WATCH2_ADDR_L=bitpacked record
  RESERVED0:bit6;
  ADDR     :bit26;
 end;

 TTCP_WATCH3_ADDR_H=bitpacked record
  ADDR     :bit16;
  RESERVED0:bit16;
 end;

 TTCP_WATCH3_ADDR_L=bitpacked record
  RESERVED0:bit6;
  ADDR     :bit26;
 end;

 TVGT_NUM_INSTANCES=bit32;

 TVGT_STRMOUT_DELAY=bitpacked record
  SKIP_DELAY  :bit8;
  SE0_WD_DELAY:bit3;
  SE1_WD_DELAY:bit3;
  SE2_WD_DELAY:bit3;
  SE3_WD_DELAY:bit3;
  RESERVED0   :bit12;
 end;

 TCB_COLOR0_DCC_BASE=bit32;

 TCB_COLOR1_DCC_BASE=bit32;

 TCB_COLOR2_DCC_BASE=bit32;

 TCB_COLOR3_DCC_BASE=bit32;

 TCB_COLOR4_DCC_BASE=bit32;

 TCB_COLOR5_DCC_BASE=bit32;

 TCB_COLOR6_DCC_BASE=bit32;

 TCB_COLOR7_DCC_BASE=bit32;

 TCB_PERFCOUNTER0_HI=bit32;

 TCB_PERFCOUNTER0_LO=bit32;

 TCB_PERFCOUNTER1_HI=bit32;

 TCB_PERFCOUNTER1_LO=bit32;

 TCB_PERFCOUNTER2_HI=bit32;

 TCB_PERFCOUNTER2_LO=bit32;

 TCB_PERFCOUNTER3_HI=bit32;

 TCB_PERFCOUNTER3_LO=bit32;

 TCP_ATOMIC_PREOP_HI=bit32;

 TCP_ATOMIC_PREOP_LO=bit32;

 TCP_CE_INIT_BASE_HI=bitpacked record
  INIT_BASE_HI:bit16;
  RESERVED0   :bit16;
 end;

 TCP_CE_INIT_BASE_LO=bitpacked record
  RESERVED0   :bit5;
  INIT_BASE_LO:bit27;
 end;

 TCP_CE_ROQ_IB1_STAT=bitpacked record
  CEQ_RPTR_INDIRECT1:bit10;
  RESERVED0         :bit6;
  CEQ_WPTR_INDIRECT1:bit10;
  RESERVED1         :bit6;
 end;

 TCP_CE_ROQ_IB2_STAT=bitpacked record
  CEQ_RPTR_INDIRECT2:bit10;
  RESERVED0         :bit6;
  CEQ_WPTR_INDIRECT2:bit10;
  RESERVED1         :bit6;
 end;

 TCP_DMA_ME_DST_ADDR=bit32;

 TCP_DMA_ME_SRC_ADDR=bit32;

 TCP_DMA_PFP_COMMAND=bitpacked record
  BYTE_COUNT:bit21;
  DIS_WC    :bit1;
  SRC_SWAP  :bit2;
  DST_SWAP  :bit2;
  SAS       :bit1;
  DAS       :bit1;
  SAIC      :bit1;
  DAIC      :bit1;
  RAW_WAIT  :bit1;
  RESERVED0 :bit1;
 end;

 TCP_DMA_PFP_CONTROL=bitpacked record
  RESERVED0       :bit10;
  SRC_MTYPE       :bit2;
  SRC_ATC         :bit1;
  SRC_CACHE_POLICY:bit1;
  RESERVED1       :bit6;
  DST_SELECT      :bit2;
  DST_MTYPE       :bit2;
  DST_ATC         :bit1;
  DST_CACHE_POLICY:bit1;
  RESERVED2       :bit3;
  SRC_SELECT      :bit2;
  RESERVED3       :bit1;
 end;

 TCP_DMA_PIO_COMMAND=bitpacked record
  RESERVED0:bit21;
  RESERVED1:bit1;
  RESERVED2:bit2;
  RESERVED3:bit2;
  RESERVED4:bit1;
  RESERVED5:bit1;
  RESERVED6:bit1;
  RESERVED7:bit1;
  RESERVED8:bit1;
  RESERVED9:bit1;
 end;

 TCP_GRBM_FREE_COUNT=bitpacked record
  FREE_COUNT    :bit6;
  RESERVED0     :bit2;
  FREE_COUNT_GDS:bit6;
  RESERVED1     :bit2;
  FREE_COUNT_PFP:bit6;
  RESERVED2     :bit10;
 end;

 TCP_HPD_ROQ_OFFSETS=bitpacked record
  IQ_OFFSET:bit3;
  RESERVED0:bit5;
  PQ_OFFSET:bit6;
  RESERVED1:bit2;
  IB_OFFSET:bit6;
  RESERVED2:bit10;
 end;

 TCP_HQD_DMA_OFFLOAD=bitpacked record
  DMA_OFFLOAD:bit1;
  RESERVED0  :bit31;
 end;

 TCP_HQD_EOP_CONTROL=bitpacked record
  EOP_SIZE        :bit6;
  RESERVED0       :bit2;
  PROCESSING_EOP  :bit1;
  RESERVED1       :bit3;
  PROCESS_EOP_EN  :bit1;
  PROCESSING_EOPIB:bit1;
  PROCESS_EOPIB_EN:bit1;
  MTYPE           :bit2;
  RESERVED2       :bit6;
  EOP_ATC         :bit1;
  CACHE_POLICY    :bit1;
  RESERVED3       :bit4;
  SIG_SEM_RESULT  :bit2;
  PEND_SIG_SEM    :bit1;
 end;

 TCP_HQD_HQ_CONTROL0=bit32;

 TCP_HQD_HQ_CONTROL1=bit32;

 TCP_INDEX_BASE_ADDR=bit32;

 TCP_PFP_HEADER_DUMP=bit32;

 TCP_ROQ1_THRESHOLDS=bitpacked record
  RB1_START   :bit8;
  RB2_START   :bit8;
  R0_IB1_START:bit8;
  R1_IB1_START:bit8;
 end;

 TCP_ROQ2_THRESHOLDS=bitpacked record
  R2_IB1_START:bit8;
  R0_IB2_START:bit8;
  R1_IB2_START:bit8;
  R2_IB2_START:bit8;
 end;

 TCP_SIG_SEM_ADDR_HI=bitpacked record
  SEM_ADDR_HI    :bit16;
  SEM_USE_MAILBOX:bit1;
  RESERVED0      :bit3;
  SEM_SIGNAL_TYPE:bit1;
  RESERVED1      :bit3;
  SEM_CLIENT_CODE:bit2;
  RESERVED2      :bit3;
  SEM_SELECT     :bit3;
 end;

 TCP_SIG_SEM_ADDR_LO=bitpacked record
  SEM_ADDR_SWAP:bit2;
  RESERVED0    :bit1;
  SEM_ADDR_LO  :bit29;
 end;

 TDB_CGTT_CLK_CTRL_0=bitpacked record
  ON_DELAY      :bit4;
  OFF_HYSTERESIS:bit8;
  RESERVED      :bit12;
  SOFT_OVERRIDE7:bit1;
  SOFT_OVERRIDE6:bit1;
  SOFT_OVERRIDE5:bit1;
  SOFT_OVERRIDE4:bit1;
  SOFT_OVERRIDE3:bit1;
  SOFT_OVERRIDE2:bit1;
  SOFT_OVERRIDE1:bit1;
  SOFT_OVERRIDE0:bit1;
 end;

 TDB_FREE_CACHELINES=bitpacked record
  FREE_DTILE_DEPTH:bit7;
  FREE_PLANE_DEPTH:bit7;
  FREE_Z_DEPTH    :bit7;
  FREE_HTILE_DEPTH:bit4;
  QUAD_READ_REQS  :bit7;
 end;

 TDB_HTILE_DATA_BASE=bit32;

 TDB_PERFCOUNTER0_HI=bit32;

 TDB_PERFCOUNTER0_LO=bit32;

 TDB_PERFCOUNTER1_HI=bit32;

 TDB_PERFCOUNTER1_LO=bit32;

 TDB_PERFCOUNTER2_HI=bit32;

 TDB_PERFCOUNTER2_LO=bit32;

 TDB_PERFCOUNTER3_HI=bit32;

 TDB_PERFCOUNTER3_LO=bit32;

 TDB_PRELOAD_CONTROL=bitpacked record
  START_X:bit8;
  START_Y:bit8;
  MAX_X  :bit8;
  MAX_Y  :bit8;
 end;

 TDB_RENDER_OVERRIDE=bitpacked record
  FORCE_HIZ_ENABLE       :bit2;
  FORCE_HIS_ENABLE0      :bit2;
  FORCE_HIS_ENABLE1      :bit2;
  FORCE_SHADER_Z_ORDER   :bit1;
  FAST_Z_DISABLE         :bit1;
  FAST_STENCIL_DISABLE   :bit1;
  NOOP_CULL_DISABLE      :bit1;
  FORCE_COLOR_KILL       :bit1;
  FORCE_Z_READ           :bit1;
  FORCE_STENCIL_READ     :bit1;
  FORCE_FULL_Z_RANGE     :bit2;
  FORCE_QC_SMASK_CONFLICT:bit1;
  DISABLE_VIEWPORT_CLAMP :bit1;
  IGNORE_SC_ZRANGE       :bit1;
  DISABLE_FULLY_COVERED  :bit1;
  FORCE_Z_LIMIT_SUMM     :bit2;
  MAX_TILES_IN_DTT       :bit5;
  DISABLE_TILE_RATE_TILES:bit1;
  FORCE_Z_DIRTY          :bit1;
  FORCE_STENCIL_DIRTY    :bit1;
  FORCE_Z_VALID          :bit1;
  FORCE_STENCIL_VALID    :bit1;
  PRESERVE_COMPRESSION   :bit1;
 end;

 TDB_STENCIL_CONTROL=bitpacked record
  STENCILFAIL    :bit4;
  STENCILZPASS   :bit4;
  STENCILZFAIL   :bit4;
  STENCILFAIL_BF :bit4;
  STENCILZPASS_BF:bit4;
  STENCILZFAIL_BF:bit4;
  RESERVED0      :bit8;
 end;

 TDB_SUBTILE_CONTROL=bitpacked record
  MSAA1_X  :bit2;
  MSAA1_Y  :bit2;
  MSAA2_X  :bit2;
  MSAA2_Y  :bit2;
  MSAA4_X  :bit2;
  MSAA4_Y  :bit2;
  MSAA8_X  :bit2;
  MSAA8_Y  :bit2;
  MSAA16_X :bit2;
  MSAA16_Y :bit2;
  RESERVED0:bit12;
 end;

 TDB_ZPASS_COUNT_LOW=bit32;

 TGDS_PS0_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS0_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS0_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS0_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS1_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS1_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS1_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS1_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS2_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS2_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS2_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS2_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS3_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS3_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS3_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS3_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS4_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS4_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS4_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS4_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS5_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS5_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS5_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS5_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS6_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS6_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS6_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS6_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS7_CTXSW_CNT0=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS7_CTXSW_CNT1=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS7_CTXSW_CNT2=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_PS7_CTXSW_CNT3=bitpacked record
  UPDN:bit16;
  PTR :bit16;
 end;

 TGDS_RD_BURST_COUNT=bit32;

 TGDS_WRITE_COMPLETE=bit32;

 TGRBM_HYP_CAM_INDEX=bitpacked record
  CAM_INDEX:bit3;
  RESERVED0:bit29;
 end;

 TGRBM_TRAP_ADDR_MSK=bitpacked record
  DATA     :bit16;
  RESERVED0:bit16;
 end;

 TIA_MULTI_VGT_PARAM=bitpacked record
  PRIMGROUP_SIZE     :bit16;
  PARTIAL_VS_WAVE_ON :bit1;
  SWITCH_ON_EOP      :bit1;
  PARTIAL_ES_WAVE_ON :bit1;
  SWITCH_ON_EOI      :bit1;
  WD_SWITCH_ON_EOP   :bit1;
  RESERVED0          :bit7;
  MAX_PRIMGRP_IN_WAVE:bit4;
 end;

 TIA_PERFCOUNTER0_HI=bit32;

 TIA_PERFCOUNTER0_LO=bit32;

 TIA_PERFCOUNTER1_HI=bit32;

 TIA_PERFCOUNTER1_LO=bit32;

 TIA_PERFCOUNTER2_HI=bit32;

 TIA_PERFCOUNTER2_LO=bit32;

 TIA_PERFCOUNTER3_HI=bit32;

 TIA_PERFCOUNTER3_LO=bit32;

 TPA_CL_VPORT_XSCALE=bit32;

 TPA_CL_VPORT_YSCALE=bit32;

 TPA_CL_VPORT_ZSCALE=bit32;

 TPA_SC_IF_FIFO_SIZE=bitpacked record
  SC_DB_TILE_IF_FIFO_SIZE:bit6;
  SC_DB_QUAD_IF_FIFO_SIZE:bit6;
  SC_SPI_IF_FIFO_SIZE    :bit6;
  SC_BCI_IF_FIFO_SIZE    :bit6;
  RESERVED0              :bit8;
 end;

 TPA_SC_LINE_STIPPLE=bitpacked record
  LINE_PATTERN     :bit16;
  REPEAT_COUNT     :bit8;
  RESERVED0        :bit4;
  PATTERN_BIT_ORDER:bit1;
  AUTO_RESET_CNTL  :bit2;
  RESERVED1        :bit1;
 end;

 TPA_SC_VPORT_ZMAX_0=bit32;

 TPA_SC_VPORT_ZMAX_1=bit32;

 TPA_SC_VPORT_ZMAX_2=bit32;

 TPA_SC_VPORT_ZMAX_3=bit32;

 TPA_SC_VPORT_ZMAX_4=bit32;

 TPA_SC_VPORT_ZMAX_5=bit32;

 TPA_SC_VPORT_ZMAX_6=bit32;

 TPA_SC_VPORT_ZMAX_7=bit32;

 TPA_SC_VPORT_ZMAX_8=bit32;

 TPA_SC_VPORT_ZMAX_9=bit32;

 TPA_SC_VPORT_ZMIN_0=bit32;

 TPA_SC_VPORT_ZMIN_1=bit32;

 TPA_SC_VPORT_ZMIN_2=bit32;

 TPA_SC_VPORT_ZMIN_3=bit32;

 TPA_SC_VPORT_ZMIN_4=bit32;

 TPA_SC_VPORT_ZMIN_5=bit32;

 TPA_SC_VPORT_ZMIN_6=bit32;

 TPA_SC_VPORT_ZMIN_7=bit32;

 TPA_SC_VPORT_ZMIN_8=bit32;

 TPA_SC_VPORT_ZMIN_9=bit32;

 TPA_SU_POINT_MINMAX=bitpacked record
  MIN_SIZE:bit16;
  MAX_SIZE:bit16;
 end;

 TPA_SU_SC_MODE_CNTL=bitpacked record
  CULL_FRONT              :bit1;
  CULL_BACK               :bit1;
  FACE                    :bit1;
  POLY_MODE               :bit2;
  POLYMODE_FRONT_PTYPE    :bit3;
  POLYMODE_BACK_PTYPE     :bit3;
  POLY_OFFSET_FRONT_ENABLE:bit1;
  POLY_OFFSET_BACK_ENABLE :bit1;
  POLY_OFFSET_PARA_ENABLE :bit1;
  RESERVED0               :bit2;
  VTX_WINDOW_OFFSET_ENABLE:bit1;
  RESERVED1               :bit2;
  PROVOKING_VTX_LAST      :bit1;
  PERSP_CORR_DIS          :bit1;
  MULTI_PRIM_IB_ENA       :bit1;
  RESERVED2               :bit10;
 end;

 TSPI_GDBG_TRAP_MASK=bitpacked record
  EXCP_EN  :bit9;
  REPLACE  :bit1;
  RESERVED0:bit22;
 end;

 TSPI_GDBG_WAVE_CNTL=bitpacked record
  STALL_RA  :bit1;
  STALL_VMID:bit16;
  RESERVED0 :bit15;
 end;

 TSPI_PS_MAX_WAVE_ID=bitpacked record
  MAX_WAVE_ID:bit12;
  RESERVED0  :bit20;
 end;

 TSQ_PERFCOUNTER0_HI=bit32;

 TSQ_PERFCOUNTER0_LO=bit32;

 TSQ_PERFCOUNTER1_HI=bit32;

 TSQ_PERFCOUNTER1_LO=bit32;

 TSQ_PERFCOUNTER2_HI=bit32;

 TSQ_PERFCOUNTER2_LO=bit32;

 TSQ_PERFCOUNTER3_HI=bit32;

 TSQ_PERFCOUNTER3_LO=bit32;

 TSQ_PERFCOUNTER4_HI=bit32;

 TSQ_PERFCOUNTER4_LO=bit32;

 TSQ_PERFCOUNTER5_HI=bit32;

 TSQ_PERFCOUNTER5_LO=bit32;

 TSQ_PERFCOUNTER6_HI=bit32;

 TSQ_PERFCOUNTER6_LO=bit32;

 TSQ_PERFCOUNTER7_HI=bit32;

 TSQ_PERFCOUNTER7_LO=bit32;

 TSQ_PERFCOUNTER8_HI=bit32;

 TSQ_PERFCOUNTER8_LO=bit32;

 TSQ_PERFCOUNTER9_HI=bit32;

 TSQ_PERFCOUNTER9_LO=bit32;

 TSX_PERFCOUNTER0_HI=bit32;

 TSX_PERFCOUNTER0_LO=bit32;

 TSX_PERFCOUNTER1_HI=bit32;

 TSX_PERFCOUNTER1_LO=bit32;

 TSX_PERFCOUNTER2_HI=bit32;

 TSX_PERFCOUNTER2_LO=bit32;

 TSX_PERFCOUNTER3_HI=bit32;

 TSX_PERFCOUNTER3_LO=bit32;

 TTA_BC_BASE_ADDR_HI=bitpacked record
  ADDRESS  :bit8;
  RESERVED0:bit24;
 end;

 TTA_CS_BC_BASE_ADDR=bit32;

 TTA_PERFCOUNTER0_HI=bit32;

 TTA_PERFCOUNTER0_LO=bit32;

 TTA_PERFCOUNTER1_HI=bit32;

 TTA_PERFCOUNTER1_LO=bit32;

 TTCA_CGTT_SCLK_CTRL=bitpacked record
  ON_DELAY      :bit4;
  OFF_HYSTERESIS:bit8;
  RESERVED0     :bit12;
  SOFT_OVERRIDE7:bit1;
  SOFT_OVERRIDE6:bit1;
  SOFT_OVERRIDE5:bit1;
  SOFT_OVERRIDE4:bit1;
  SOFT_OVERRIDE3:bit1;
  SOFT_OVERRIDE2:bit1;
  SOFT_OVERRIDE1:bit1;
  SOFT_OVERRIDE0:bit1;
 end;

 TTCC_CGTT_SCLK_CTRL=bitpacked record
  ON_DELAY      :bit4;
  OFF_HYSTERESIS:bit8;
  RESERVED0     :bit12;
  SOFT_OVERRIDE7:bit1;
  SOFT_OVERRIDE6:bit1;
  SOFT_OVERRIDE5:bit1;
  SOFT_OVERRIDE4:bit1;
  SOFT_OVERRIDE3:bit1;
  SOFT_OVERRIDE2:bit1;
  SOFT_OVERRIDE1:bit1;
  SOFT_OVERRIDE0:bit1;
 end;

 TTD_PERFCOUNTER0_HI=bit32;

 TTD_PERFCOUNTER0_LO=bit32;

 TTD_PERFCOUNTER1_HI=bit32;

 TTD_PERFCOUNTER1_LO=bit32;

 TVGT_DMA_INDEX_TYPE=bitpacked record
  INDEX_TYPE  :bit2;
  SWAP_MODE   :bit2;
  BUF_TYPE    :bit2;
  RDREQ_POLICY:bit1;
  RESERVED0   :bit2;
  NOT_EOP     :bit1;
  REQ_PATH    :bit1;
  MTYPE       :bit2;
  RESERVED1   :bit19;
 end;

 TVGT_DRAW_INITIATOR=bitpacked record
  SOURCE_SELECT :bit2;
  MAJOR_MODE    :bit2;
  SPRITE_EN_R6XX:bit1;
  NOT_EOP       :bit1;
  USE_OPAQUE    :bit1;
  RESERVED0     :bit25;
 end;

 TVGT_ESGS_RING_SIZE=bit32;

 TVGT_GSVS_RING_SIZE=bit32;

 TVGT_GS_ONCHIP_CNTL=bitpacked record
  ES_VERTS_PER_SUBGRP:bit11;
  GS_PRIMS_PER_SUBGRP:bit11;
  RESERVED0          :bit10;
 end;

 TVGT_PRIMITIVEID_EN=bitpacked record
  PRIMITIVEID_EN      :bit1;
  DISABLE_RESET_ON_EOI:bit1;
  RESERVED0           :bit30;
 end;

 TVGT_PRIMITIVE_TYPE=bitpacked record
  PRIM_TYPE:bit6;
  RESERVED0:bit26;
 end;

 TVGT_STRMOUT_CONFIG=bitpacked record
  STREAMOUT_0_EN      :bit1;
  STREAMOUT_1_EN      :bit1;
  STREAMOUT_2_EN      :bit1;
  STREAMOUT_3_EN      :bit1;
  RAST_STREAM         :bit3;
  RESERVED0           :bit1;
  RAST_STREAM_MASK    :bit4;
  RESERVED1           :bit19;
  USE_RAST_STREAM_MASK:bit1;
 end;

 TVGT_TF_MEMORY_BASE=bit32;

 TVGT_VS_MAX_WAVE_ID=bitpacked record
  MAX_WAVE_ID:bit12;
  RESERVED0  :bit20;
 end;

 TWD_PERFCOUNTER0_HI=bit32;

 TWD_PERFCOUNTER0_LO=bit32;

 TWD_PERFCOUNTER1_HI=bit32;

 TWD_PERFCOUNTER1_LO=bit32;

 TWD_PERFCOUNTER2_HI=bit32;

 TWD_PERFCOUNTER2_LO=bit32;

 TWD_PERFCOUNTER3_HI=bit32;

 TWD_PERFCOUNTER3_LO=bit32;

 TCOMPUTE_DISPATCH_ID=bit32;

 TCOMPUTE_USER_DATA_0=bit32;

 TCOMPUTE_USER_DATA_1=bit32;

 TCOMPUTE_USER_DATA_2=bit32;

 TCOMPUTE_USER_DATA_3=bit32;

 TCOMPUTE_USER_DATA_4=bit32;

 TCOMPUTE_USER_DATA_5=bit32;

 TCOMPUTE_USER_DATA_6=bit32;

 TCOMPUTE_USER_DATA_7=bit32;

 TCOMPUTE_USER_DATA_8=bit32;

 TCOMPUTE_USER_DATA_9=bit32;

 TCPC_PERFCOUNTER0_HI=bit32;

 TCPC_PERFCOUNTER0_LO=bit32;

 TCPC_PERFCOUNTER1_HI=bit32;

 TCPC_PERFCOUNTER1_LO=bit32;

 TCPF_PERFCOUNTER0_HI=bit32;

 TCPF_PERFCOUNTER0_LO=bit32;

 TCPF_PERFCOUNTER1_HI=bit32;

 TCPF_PERFCOUNTER1_LO=bit32;

 TCPG_PERFCOUNTER0_HI=bit32;

 TCPG_PERFCOUNTER0_LO=bit32;

 TCPG_PERFCOUNTER1_HI=bit32;

 TCPG_PERFCOUNTER1_LO=bit32;

 TCP_CE_COMPARE_COUNT=bit32;

 TCP_CPC_IC_BASE_CNTL=bitpacked record
  VMID        :bit4;
  RESERVED0   :bit19;
  ATC         :bit1;
  CACHE_POLICY:bit1;
  RESERVED1   :bit2;
  MTYPE       :bit2;
  RESERVED2   :bit3;
 end;

 TCP_CPC_SCRATCH_DATA=bit32;

 TCP_DMA_PFP_DST_ADDR=bit32;

 TCP_DMA_PFP_SRC_ADDR=bit32;

 TCP_DRAW_WINDOW_CNTL=bitpacked record
  DISABLE_DRAW_WINDOW_LO_MAX:bit1;
  DISABLE_DRAW_WINDOW_LO_MIN:bit1;
  DISABLE_DRAW_WINDOW_HI    :bit1;
  RESERVED0                 :bit5;
  MODE                      :bit1;
  RESERVED1                 :bit23;
 end;

 TCP_EOP_DONE_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_EOP_DONE_ADDR_LO=bitpacked record
  ADDR_SWAP__SI__CI:bit2;
  ADDR_LO          :bit30;
 end;

 TCP_EOP_DONE_CNTX_ID=bitpacked record
  CNTX_ID  :bit28;
  RESERVED0:bit4;
 end;

 TCP_EOP_DONE_DATA_HI=bit32;

 TCP_EOP_DONE_DATA_LO=bit32;

 TCP_GDS_BKUP_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_HQD_EOP_WPTR_MEM=bitpacked record
  WPTR     :bit13;
  RESERVED0:bit19;
 end;

 TCP_HQD_IB_BASE_ADDR=bitpacked record
  RESERVED0   :bit2;
  IB_BASE_ADDR:bit30;
 end;

 TCP_IB1_PREAMBLE_END=bitpacked record
  IB1_PREAMBLE_END:bit20;
  RESERVED0       :bit12;
 end;

 TCP_IB2_PREAMBLE_END=bitpacked record
  IB2_PREAMBLE_END:bit20;
  RESERVED0       :bit12;
 end;

 TCP_INT_STATUS_RING0=bitpacked record
  RESERVED0                  :bit11;
  CP_VM_DOORBELL_WR_INT_STAT :bit1;
  RESERVED1                  :bit2;
  CP_ECC_ERROR_INT_STAT      :bit1;
  RESERVED2                  :bit2;
  WRM_POLL_TIMEOUT_INT_STAT  :bit1;
  CMP_BUSY_INT_STAT          :bit1;
  GCNTX_BUSY_INT_STAT        :bit1;
  CNTX_EMPTY_INT_STAT        :bit1;
  GFX_IDLE_INT_STAT          :bit1;
  PRIV_INSTR_INT_STAT        :bit1;
  PRIV_REG_INT_STAT          :bit1;
  OPCODE_ERROR_INT_STAT      :bit1;
  RESERVED3                  :bit1;
  TIME_STAMP_INT_STAT        :bit1;
  RESERVED_BIT_ERROR_INT_STAT:bit1;
  RESERVED4                  :bit1;
  GENERIC2_INT_STAT          :bit1;
  GENERIC1_INT_STAT          :bit1;
  GENERIC0_INT_STAT          :bit1;
 end;

 TCP_INT_STATUS_RING1=bitpacked record
  RESERVED0                  :bit11;
  CP_VM_DOORBELL_WR_INT_STAT :bit1;
  RESERVED1                  :bit2;
  CP_ECC_ERROR_INT_STAT      :bit1;
  RESERVED2                  :bit2;
  WRM_POLL_TIMEOUT_INT_STAT  :bit1;
  CMP_BUSY_INT_STAT          :bit1;
  CNTX_BUSY_INT_STAT         :bit1;
  CNTX_EMPTY_INT_STAT        :bit1;
  GFX_IDLE_INT_STAT          :bit1;
  PRIV_INSTR_INT_STAT        :bit1;
  PRIV_REG_INT_STAT          :bit1;
  OPCODE_ERROR_INT_STAT      :bit1;
  RESERVED3                  :bit1;
  TIME_STAMP_INT_STAT        :bit1;
  RESERVED_BIT_ERROR_INT_STAT:bit1;
  RESERVED4                  :bit1;
  GENERIC2_INT_STAT          :bit1;
  GENERIC1_INT_STAT          :bit1;
  GENERIC0_INT_STAT          :bit1;
 end;

 TCP_INT_STATUS_RING2=bitpacked record
  RESERVED0                  :bit11;
  CP_VM_DOORBELL_WR_INT_STAT :bit1;
  RESERVED1                  :bit2;
  CP_ECC_ERROR_INT_STAT      :bit1;
  RESERVED2                  :bit2;
  WRM_POLL_TIMEOUT_INT_STAT  :bit1;
  CMP_BUSY_INT_STAT          :bit1;
  CNTX_BUSY_INT_STAT         :bit1;
  CNTX_EMPTY_INT_STAT        :bit1;
  GFX_IDLE_INT_STAT          :bit1;
  PRIV_INSTR_INT_STAT        :bit1;
  PRIV_REG_INT_STAT          :bit1;
  OPCODE_ERROR_INT_STAT      :bit1;
  RESERVED3                  :bit1;
  TIME_STAMP_INT_STAT        :bit1;
  RESERVED_BIT_ERROR_INT_STAT:bit1;
  RESERVED4                  :bit1;
  GENERIC2_INT_STAT          :bit1;
  GENERIC1_INT_STAT          :bit1;
  GENERIC0_INT_STAT          :bit1;
 end;

 TCP_MEC1_F32_INT_DIS=bitpacked record
  EDC_ROQ_FED_INT     :bit1;
  PRIV_REG_INT        :bit1;
  RESERVED_BIT_ERR_INT:bit1;
  EDC_TC_FED_INT      :bit1;
  EDC_GDS_FED_INT     :bit1;
  EDC_SCRATCH_FED_INT :bit1;
  WAVE_RESTORE_INT    :bit1;
  SUA_VIOLATION_INT   :bit1;
  EDC_DMA_FED_INT     :bit1;
  IQ_TIMER_INT        :bit1;
  RESERVED0           :bit22;
 end;

 TCP_MEC2_F32_INT_DIS=bitpacked record
  EDC_ROQ_FED_INT     :bit1;
  PRIV_REG_INT        :bit1;
  RESERVED_BIT_ERR_INT:bit1;
  EDC_TC_FED_INT      :bit1;
  EDC_GDS_FED_INT     :bit1;
  EDC_SCRATCH_FED_INT :bit1;
  WAVE_RESTORE_INT    :bit1;
  SUA_VIOLATION_INT   :bit1;
  EDC_DMA_FED_INT     :bit1;
  IQ_TIMER_INT        :bit1;
  RESERVED0           :bit22;
 end;

 TCP_MQD_BASE_ADDR_HI=bitpacked record
  BASE_ADDR_HI:bit16;
  RESERVED0   :bit16;
 end;

 TCP_PFP_LOAD_CONTROL=bitpacked record
  CONFIG_REG_EN:bit1;
  CNTX_REG_EN  :bit1;
  RESERVED0    :bit14;
  SH_GFX_REG_EN:bit1;
  RESERVED1    :bit7;
  SH_CS_REG_EN :bit1;
  RESERVED2    :bit7;
 end;

 TCP_PRED_NOT_VISIBLE=bitpacked record
  NOT_VISIBLE:bit1;
  RESERVED0  :bit31;
 end;

 TCP_QUEUE_THRESHOLDS=bitpacked record
  ROQ_IB1_START:bit6;
  RESERVED0    :bit2;
  ROQ_IB2_START:bit6;
  RESERVED1    :bit18;
 end;

 TCP_RB0_RPTR_ADDR_HI=bitpacked record
  RB_RPTR_ADDR_HI:bit16;
  RESERVED0      :bit16;
 end;

 TCP_RB1_RPTR_ADDR_HI=bitpacked record
  RB_RPTR_ADDR_HI:bit16;
  RESERVED0      :bit16;
 end;

 TCP_RB2_RPTR_ADDR_HI=bitpacked record
  RB_RPTR_ADDR_HI:bit16;
  RESERVED0      :bit16;
 end;

 TCP_WAIT_SEM_ADDR_HI=bitpacked record
  SEM_ADDR_HI    :bit16;
  SEM_USE_MAILBOX:bit1;
  RESERVED0      :bit3;
  SEM_SIGNAL_TYPE:bit1;
  RESERVED1      :bit3;
  SEM_CLIENT_CODE:bit2;
  RESERVED2      :bit3;
  SEM_SELECT     :bit3;
 end;

 TCP_WAIT_SEM_ADDR_LO=bitpacked record
  SEM_ADDR_SWAP:bit2;
  RESERVED0    :bit1;
  SEM_ADDR_LO  :bit29;
 end;

 TDB_DEPTH_BOUNDS_MAX=bit32;

 TDB_DEPTH_BOUNDS_MIN=bit32;

 TDB_RENDER_OVERRIDE2=bitpacked record
  PARTIAL_SQUAD_LAUNCH_CONTROL       :bit2;
  PARTIAL_SQUAD_LAUNCH_COUNTDOWN     :bit3;
  DISABLE_ZMASK_EXPCLEAR_OPTIMIZATION:bit1;
  DISABLE_SMEM_EXPCLEAR_OPTIMIZATION :bit1;
  DISABLE_COLOR_ON_VALIDATION        :bit1;
  DECOMPRESS_Z_ON_FLUSH              :bit1;
  DISABLE_REG_SNOOP                  :bit1;
  DEPTH_BOUNDS_HIER_DEPTH_DISABLE    :bit1;
  SEPARATE_HIZS_FUNC_ENABLE          :bit1;
  HIZ_ZFUNC                          :bit3;
  HIS_SFUNC_FF                       :bit3;
  HIS_SFUNC_BF                       :bit3;
  PRESERVE_ZRANGE                    :bit1;
  PRESERVE_SRESULTS                  :bit1;
  DISABLE_FAST_PASS                  :bit1;
  RESERVED0                          :bit8;
 end;

 TGDS_CS_CTXSW_STATUS=bitpacked record
  R     :bit1;
  W     :bit1;
  UNUSED:bit30;
 end;

 TGDS_OA_CGPG_RESTORE=bitpacked record
  VMID   :bit8;
  MEID   :bit4;
  PIPEID :bit4;
  QUEUEID:bit4;
  UNUSED :bit12;
 end;

 TGDS_PERFCOUNTER0_HI=bit32;

 TGDS_PERFCOUNTER0_LO=bit32;

 TGDS_PERFCOUNTER1_HI=bit32;

 TGDS_PERFCOUNTER1_LO=bit32;

 TGDS_PERFCOUNTER2_HI=bit32;

 TGDS_PERFCOUNTER2_LO=bit32;

 TGDS_PERFCOUNTER3_HI=bit32;

 TGDS_PERFCOUNTER3_LO=bit32;

 TGRBM_DEBUG_SNAPSHOT=bitpacked record
  CPF_RDY             :bit1;
  CPG_RDY             :bit1;
  SRBM_RDY            :bit1;
  WD_ME0PIPE0_RDY     :bit1;
  WD_ME0PIPE1_RDY     :bit1;
  GDS_RDY             :bit1;
  SE0SPI_ME0PIPE0_RDY0:bit1;
  SE0SPI_ME0PIPE1_RDY0:bit1;
  SE1SPI_ME0PIPE0_RDY0:bit1;
  SE1SPI_ME0PIPE1_RDY0:bit1;
  SE2SPI_ME0PIPE0_RDY0:bit1;
  SE2SPI_ME0PIPE1_RDY0:bit1;
  SE3SPI_ME0PIPE0_RDY0:bit1;
  SE3SPI_ME0PIPE1_RDY0:bit1;
  SE0SPI_ME0PIPE0_RDY1:bit1;
  SE0SPI_ME0PIPE1_RDY1:bit1;
  SE1SPI_ME0PIPE0_RDY1:bit1;
  SE1SPI_ME0PIPE1_RDY1:bit1;
  SE2SPI_ME0PIPE0_RDY1:bit1;
  SE2SPI_ME0PIPE1_RDY1:bit1;
  SE3SPI_ME0PIPE0_RDY1:bit1;
  SE3SPI_ME0PIPE1_RDY1:bit1;
  RESERVED0           :bit10;
 end;

 TGRBM_GFX_CLKEN_CNTL=bitpacked record
  PREFIX_DELAY_CNT:bit4;
  RESERVED0       :bit4;
  POST_DELAY_CNT  :bit5;
  RESERVED1       :bit19;
 end;

 TPA_CL_VPORT_XOFFSET=bit32;

 TPA_CL_VPORT_YOFFSET=bit32;

 TPA_CL_VPORT_ZOFFSET=bit32;

 TPA_SC_CLIPRECT_0_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_0_TL=bitpacked record
  TL_X     :bit15;
  RESERVED0:bit1;
  TL_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_1_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_1_TL=bitpacked record
  TL_X     :bit15;
  RESERVED0:bit1;
  TL_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_2_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_2_TL=bitpacked record
  TL_X     :bit15;
  RESERVED0:bit1;
  TL_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_3_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_3_TL=bitpacked record
  TL_X     :bit15;
  RESERVED0:bit1;
  TL_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_CLIPRECT_RULE=bitpacked record
  CLIP_RULE:bit16;
  RESERVED0:bit16;
 end;

 TPA_SC_RASTER_CONFIG=bitpacked record
  RB_MAP_PKR0:bit2;
  RB_MAP_PKR1:bit2;
  RB_XSEL2   :bit2;
  RB_XSEL    :bit1;
  RB_YSEL    :bit1;
  PKR_MAP    :bit2;
  PKR_XSEL   :bit2;
  PKR_YSEL   :bit2;
  PKR_XSEL2  :bit2;
  SC_MAP     :bit2;
  SC_XSEL    :bit2;
  SC_YSEL    :bit2;
  RESERVED0  :bit2;
  SE_MAP     :bit2;
  SE_XSEL    :bit2;
  SE_YSEL    :bit2;
  RESERVED1  :bit2;
 end;

 TPA_SC_TRAP_SCREEN_H=bitpacked record
  X_COORD  :bit14;
  RESERVED0:bit18;
 end;

 TPA_SC_TRAP_SCREEN_V=bitpacked record
  Y_COORD  :bit14;
  RESERVED0:bit18;
 end;

 TPA_SC_VPORT_ZMAX_10=bit32;

 TPA_SC_VPORT_ZMAX_11=bit32;

 TPA_SC_VPORT_ZMAX_12=bit32;

 TPA_SC_VPORT_ZMAX_13=bit32;

 TPA_SC_VPORT_ZMAX_14=bit32;

 TPA_SC_VPORT_ZMAX_15=bit32;

 TPA_SC_VPORT_ZMIN_10=bit32;

 TPA_SC_VPORT_ZMIN_11=bit32;

 TPA_SC_VPORT_ZMIN_12=bit32;

 TPA_SC_VPORT_ZMIN_13=bit32;

 TPA_SC_VPORT_ZMIN_14=bit32;

 TPA_SC_VPORT_ZMIN_15=bit32;

 TPA_SC_WINDOW_OFFSET=bitpacked record
  WINDOW_X_OFFSET:bit16;
  WINDOW_Y_OFFSET:bit16;
 end;

 TSPI_GDBG_TRAP_DATA0=bit32;

 TSPI_GDBG_TRAP_DATA1=bit32;

 TSPI_PERFCOUNTER0_HI=bit32;

 TSPI_PERFCOUNTER0_LO=bit32;

 TSPI_PERFCOUNTER1_HI=bit32;

 TSPI_PERFCOUNTER1_LO=bit32;

 TSPI_PERFCOUNTER2_HI=bit32;

 TSPI_PERFCOUNTER2_LO=bit32;

 TSPI_PERFCOUNTER3_HI=bit32;

 TSPI_PERFCOUNTER3_LO=bit32;

 TSPI_PERFCOUNTER4_HI=bit32;

 TSPI_PERFCOUNTER4_LO=bit32;

 TSPI_PERFCOUNTER5_HI=bit32;

 TSPI_PERFCOUNTER5_LO=bit32;

 TSPI_PS_INPUT_CNTL_0=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_1=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_2=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_3=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_4=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_5=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_6=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_7=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_8=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_9=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_SHADER_Z_FORMAT=bitpacked record
  Z_EXPORT_FORMAT:bit4;
  RESERVED0      :bit28;
 end;

 TSQ_PERFCOUNTER10_HI=bit32;

 TSQ_PERFCOUNTER10_LO=bit32;

 TSQ_PERFCOUNTER11_HI=bit32;

 TSQ_PERFCOUNTER11_LO=bit32;

 TSQ_PERFCOUNTER12_HI=bit32;

 TSQ_PERFCOUNTER12_LO=bit32;

 TSQ_PERFCOUNTER13_HI=bit32;

 TSQ_PERFCOUNTER13_LO=bit32;

 TSQ_PERFCOUNTER14_HI=bit32;

 TSQ_PERFCOUNTER14_LO=bit32;

 TSQ_PERFCOUNTER15_HI=bit32;

 TSQ_PERFCOUNTER15_LO=bit32;

 TSQ_PERFCOUNTER_CTRL=bitpacked record
  PS_EN        :bit1;
  VS_EN        :bit1;
  GS_EN        :bit1;
  ES_EN        :bit1;
  HS_EN        :bit1;
  LS_EN        :bit1;
  CS_EN        :bit1;
  RESERVED0    :bit1;
  CNTR_RATE    :bit5;
  DISABLE_FLUSH:bit1;
  RESERVED1    :bit18;
 end;

 TSQ_PERFCOUNTER_MASK=bitpacked record
  SH0_MASK:bit16;
  SH1_MASK:bit16;
 end;

 TTCA_PERFCOUNTER0_HI=bit32;

 TTCA_PERFCOUNTER0_LO=bit32;

 TTCA_PERFCOUNTER1_HI=bit32;

 TTCA_PERFCOUNTER1_LO=bit32;

 TTCA_PERFCOUNTER2_HI=bit32;

 TTCA_PERFCOUNTER2_LO=bit32;

 TTCA_PERFCOUNTER3_HI=bit32;

 TTCA_PERFCOUNTER3_LO=bit32;

 TTCC_PERFCOUNTER0_HI=bit32;

 TTCC_PERFCOUNTER0_LO=bit32;

 TTCC_PERFCOUNTER1_HI=bit32;

 TTCC_PERFCOUNTER1_LO=bit32;

 TTCC_PERFCOUNTER2_HI=bit32;

 TTCC_PERFCOUNTER2_LO=bit32;

 TTCC_PERFCOUNTER3_HI=bit32;

 TTCC_PERFCOUNTER3_LO=bit32;

 TTCP_GATCL1_DSM_CNTL=bitpacked record
  SEL_DSM_TCP_GATCL1_IRRITATOR_DATA_A0:bit1;
  SEL_DSM_TCP_GATCL1_IRRITATOR_DATA_A1:bit1;
  TCP_GATCL1_ENABLE_SINGLE_WRITE_A    :bit1;
  RESERVED0                           :bit29;
 end;

 TTCP_PERFCOUNTER0_HI=bit32;

 TTCP_PERFCOUNTER0_LO=bit32;

 TTCP_PERFCOUNTER1_HI=bit32;

 TTCP_PERFCOUNTER1_LO=bit32;

 TTCP_PERFCOUNTER2_HI=bit32;

 TTCP_PERFCOUNTER2_LO=bit32;

 TTCP_PERFCOUNTER3_HI=bit32;

 TTCP_PERFCOUNTER3_LO=bit32;

 TVGT_EVENT_INITIATOR=bitpacked record
  EVENT_TYPE    :bit6;
  RESERVED0     :bit12;
  ADDRESS_HI    :bit9;
  EXTENDED_EVENT:bit1;
  RESERVED1     :bit4;
 end;

 TVGT_GROUP_PRIM_TYPE=bitpacked record
  PRIM_TYPE   :bit5;
  RESERVED0   :bit9;
  RETAIN_ORDER:bit1;
  RETAIN_QUADS:bit1;
  PRIM_ORDER  :bit3;
  RESERVED1   :bit13;
 end;

 TVGT_GS_INSTANCE_CNT=bitpacked record
  ENABLE   :bit1;
  RESERVED0:bit1;
  CNT      :bit7;
  RESERVED1:bit23;
 end;

 TVGT_GS_MAX_VERT_OUT=bitpacked record
  MAX_VERT_OUT:bit11;
  RESERVED0   :bit21;
 end;

 TVGT_GS_VERTEX_REUSE=bitpacked record
  VERT_REUSE:bit5;
  RESERVED0 :bit27;
 end;

 TVGT_HOS_REUSE_DEPTH=bitpacked record
  REUSE_DEPTH:bit8;
  RESERVED0  :bit24;
 end;

 TVGT_LAST_COPY_STATE=bitpacked record
  SRC_STATE_ID:bit3;
  RESERVED0   :bit13;
  DST_STATE_ID:bit3;
  RESERVED1   :bit13;
 end;

 TVGT_PERFCOUNTER0_HI=bit32;

 TVGT_PERFCOUNTER0_LO=bit32;

 TVGT_PERFCOUNTER1_HI=bit32;

 TVGT_PERFCOUNTER1_LO=bit32;

 TVGT_PERFCOUNTER2_HI=bit32;

 TVGT_PERFCOUNTER2_LO=bit32;

 TVGT_PERFCOUNTER3_HI=bit32;

 TVGT_PERFCOUNTER3_LO=bit32;

 TCOMPUTE_NUM_THREAD_X=bitpacked record
  NUM_THREAD_FULL   :bit16;
  NUM_THREAD_PARTIAL:bit16;
 end;

 TCOMPUTE_NUM_THREAD_Y=bitpacked record
  NUM_THREAD_FULL   :bit16;
  NUM_THREAD_PARTIAL:bit16;
 end;

 TCOMPUTE_NUM_THREAD_Z=bitpacked record
  NUM_THREAD_FULL   :bit16;
  NUM_THREAD_PARTIAL:bit16;
 end;

 TCOMPUTE_TMPRING_SIZE=bitpacked record
  WAVES    :bit12;
  WAVESIZE :bit13;
  RESERVED0:bit7;
 end;

 TCOMPUTE_USER_DATA_10=bit32;

 TCOMPUTE_USER_DATA_11=bit32;

 TCOMPUTE_USER_DATA_12=bit32;

 TCOMPUTE_USER_DATA_13=bit32;

 TCOMPUTE_USER_DATA_14=bit32;

 TCOMPUTE_USER_DATA_15=bit32;

 TCP_COHER_START_DELAY=bitpacked record
  START_DELAY_COUNT:bit6;
  RESERVED0        :bit26;
 end;

 TCP_CPC_SCRATCH_INDEX=bitpacked record
  SCRATCH_INDEX:bit9;
  RESERVED0    :bit23;
 end;

 TCP_CPC_STALLED_STAT1=bitpacked record
  MIU_RDREQ_FREE_STALL__SI__CI  :bit1;
  MIU_WRREQ_FREE_STALL__SI__CI  :bit1;
  RESERVED0                     :bit1;
  RCIU_TX_FREE_STALL            :bit1;
  RCIU_PRIV_VIOLATION           :bit1;
  RESERVED1                     :bit1;
  TCIU_TX_FREE_STALL            :bit1;
  RESERVED2                     :bit1;
  MEC1_DECODING_PACKET          :bit1;
  MEC1_WAIT_ON_RCIU             :bit1;
  MEC1_WAIT_ON_RCIU_READ        :bit1;
  MEC1_WAIT_ON_MC_READ__SI__CI  :bit1;
  MEC1_WAIT_ON_MC_WR_ACK__SI__CI:bit1;
  MEC1_WAIT_ON_ROQ_DATA         :bit1;
  RESERVED3                     :bit2;
  MEC2_DECODING_PACKET          :bit1;
  MEC2_WAIT_ON_RCIU             :bit1;
  MEC2_WAIT_ON_RCIU_READ        :bit1;
  MEC2_WAIT_ON_MC_READ__SI__CI  :bit1;
  MEC2_WAIT_ON_MC_WR_ACK__SI__CI:bit1;
  MEC2_WAIT_ON_ROQ_DATA         :bit1;
  ATCL2IU_WAITING_ON_FREE       :bit1;
  ATCL2IU_WAITING_ON_TAGS       :bit1;
  ATCL1_WAITING_ON_TRANS        :bit1;
  RESERVED4                     :bit7;
 end;

 TCP_CPF_STALLED_STAT1=bitpacked record
  RING_FETCHING_DATA               :bit1;
  INDR1_FETCHING_DATA              :bit1;
  INDR2_FETCHING_DATA              :bit1;
  STATE_FETCHING_DATA              :bit1;
  MIU_WAITING_ON_RDREQ_FREE__SI__CI:bit1;
  TCIU_WAITING_ON_FREE             :bit1;
  TCIU_WAITING_ON_TAGS             :bit1;
  ATCL2IU_WAITING_ON_FREE          :bit1;
  ATCL2IU_WAITING_ON_TAGS          :bit1;
  ATCL1_WAITING_ON_TRANS           :bit1;
  RESERVED0                        :bit22;
 end;

 TCP_EOP_LAST_FENCE_HI=bit32;

 TCP_EOP_LAST_FENCE_LO=bit32;

 TCP_HQD_CTX_SAVE_SIZE=bitpacked record
  RESERVED0:bit12;
  SIZE     :bit13;
  RESERVED1:bit7;
 end;

 TCP_HQD_EOP_BASE_ADDR=bit32;

 TCP_HQD_HQ_SCHEDULER0=bit32;

 TCP_HQD_HQ_SCHEDULER1=bit32;

 TCP_HQD_PIPE_PRIORITY=bitpacked record
  PIPE_PRIORITY:bit2;
  RESERVED0    :bit30;
 end;

 TCP_MEQ_STQ_THRESHOLD=bitpacked record
  STQ_START:bit8;
  RESERVED0:bit24;
 end;

 TCP_PA_CPRIM_COUNT_HI=bit32;

 TCP_PA_CPRIM_COUNT_LO=bit32;

 TCP_PERFMON_CNTX_CNTL=bitpacked record
  RESERVED0     :bit31;
  PERFMON_ENABLE:bit1;
 end;

 TCP_PFP_F32_INTERRUPT=bitpacked record
  RESERVED0   :bit1;
  PRIV_REG_INT:bit1;
  RESERVED1   :bit1;
  RESERVED2   :bit1;
  RESERVED3   :bit28;
 end;

 TCP_PQ_WPTR_POLL_CNTL=bitpacked record
  PERIOD     :bit8;
  RESERVED0  :bit22;
  POLL_ACTIVE:bit1;
  EN         :bit1;
 end;

 TDB_STENCILREFMASK_BF=bitpacked record
  STENCILTESTVAL_BF  :bit8;
  STENCILMASK_BF     :bit8;
  STENCILWRITEMASK_BF:bit8;
  STENCILOPVAL_BF    :bit8;
 end;

 TDB_STENCIL_READ_BASE=bit32;

 TGDS_GFX_CTXSW_STATUS=bitpacked record
  R     :bit1;
  W     :bit1;
  UNUSED:bit30;
 end;

 TGDS_GWS_RESOURCE_CNT=bitpacked record
  RESOURCE_CNT:bit16;
  RESERVED0   :bit16;
 end;

 TGDS_PROTECTION_FAULT=bitpacked record
  WRITE_DIS     :bit1;
  FAULT_DETECTED:bit1;
  GRBM          :bit1;
  SH_ID         :bit3;
  CU_ID         :bit4;
  SIMD_ID       :bit2;
  WAVE_ID       :bit4;
  ADDRESS       :bit16;
 end;

 TGRBM_PERFCOUNTER0_HI=bit32;

 TGRBM_PERFCOUNTER0_LO=bit32;

 TGRBM_PERFCOUNTER1_HI=bit32;

 TGRBM_PERFCOUNTER1_LO=bit32;

 TPA_CL_POINT_CULL_RAD=bit32;

 TPA_CL_VPORT_XSCALE_1=bit32;

 TPA_CL_VPORT_XSCALE_2=bit32;

 TPA_CL_VPORT_XSCALE_3=bit32;

 TPA_CL_VPORT_XSCALE_4=bit32;

 TPA_CL_VPORT_XSCALE_5=bit32;

 TPA_CL_VPORT_XSCALE_6=bit32;

 TPA_CL_VPORT_XSCALE_7=bit32;

 TPA_CL_VPORT_XSCALE_8=bit32;

 TPA_CL_VPORT_XSCALE_9=bit32;

 TPA_CL_VPORT_YSCALE_1=bit32;

 TPA_CL_VPORT_YSCALE_2=bit32;

 TPA_CL_VPORT_YSCALE_3=bit32;

 TPA_CL_VPORT_YSCALE_4=bit32;

 TPA_CL_VPORT_YSCALE_5=bit32;

 TPA_CL_VPORT_YSCALE_6=bit32;

 TPA_CL_VPORT_YSCALE_7=bit32;

 TPA_CL_VPORT_YSCALE_8=bit32;

 TPA_CL_VPORT_YSCALE_9=bit32;

 TPA_CL_VPORT_ZSCALE_1=bit32;

 TPA_CL_VPORT_ZSCALE_2=bit32;

 TPA_CL_VPORT_ZSCALE_3=bit32;

 TPA_CL_VPORT_ZSCALE_4=bit32;

 TPA_CL_VPORT_ZSCALE_5=bit32;

 TPA_CL_VPORT_ZSCALE_6=bit32;

 TPA_CL_VPORT_ZSCALE_7=bit32;

 TPA_CL_VPORT_ZSCALE_8=bit32;

 TPA_CL_VPORT_ZSCALE_9=bit32;

 TSPI_GDBG_TRAP_CONFIG=bitpacked record
  ME_SEL     :bit2;
  PIPE_SEL   :bit2;
  QUEUE_SEL  :bit3;
  ME_MATCH   :bit1;
  PIPE_MATCH :bit1;
  QUEUE_MATCH:bit1;
  RESERVED0  :bit5;
  TRAP_EN    :bit1;
  VMID_SEL   :bit16;
 end;

 TSPI_INTERP_CONTROL_0=bitpacked record
  FLAT_SHADE_ENA   :bit1;
  PNT_SPRITE_ENA   :bit1;
  PNT_SPRITE_OVRD_X:bit3;
  PNT_SPRITE_OVRD_Y:bit3;
  PNT_SPRITE_OVRD_Z:bit3;
  PNT_SPRITE_OVRD_W:bit3;
  PNT_SPRITE_TOP_1 :bit1;
  RESERVED0        :bit17;
 end;

 TSPI_PERFCOUNTER_BINS=bitpacked record
  BIN0_MIN:bit4;
  BIN0_MAX:bit4;
  BIN1_MIN:bit4;
  BIN1_MAX:bit4;
  BIN2_MIN:bit4;
  BIN2_MAX:bit4;
  BIN3_MIN:bit4;
  BIN3_MAX:bit4;
 end;

 TSPI_PS_INPUT_CNTL_10=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_11=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_12=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_13=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_14=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_15=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_16=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_17=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_18=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_19=bitpacked record
  OFFSET             :bit6;
  RESERVED0          :bit2;
  DEFAULT_VAL        :bit2;
  FLAT_SHADE         :bit1;
  RESERVED1          :bit2;
  CYL_WRAP           :bit4;
  PT_SPRITE_TEX      :bit1;
  DUP                :bit1;
  FP16_INTERP_MODE   :bit1;
  USE_DEFAULT_ATTR1  :bit1;
  DEFAULT_VAL_ATTR1  :bit2;
  PT_SPRITE_TEX_ATTR1:bit1;
  ATTR0_VALID        :bit1;
  ATTR1_VALID        :bit1;
  RESERVED2          :bit6;
 end;

 TSPI_PS_INPUT_CNTL_20=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_21=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_22=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_23=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_24=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_25=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_26=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_27=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_28=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_29=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_30=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_PS_INPUT_CNTL_31=bitpacked record
  OFFSET           :bit6;
  RESERVED0        :bit2;
  DEFAULT_VAL      :bit2;
  FLAT_SHADE       :bit1;
  RESERVED1        :bit7;
  DUP              :bit1;
  FP16_INTERP_MODE :bit1;
  USE_DEFAULT_ATTR1:bit1;
  DEFAULT_VAL_ATTR1:bit2;
  RESERVED2        :bit1;
  ATTR0_VALID      :bit1;
  ATTR1_VALID      :bit1;
  RESERVED3        :bit6;
 end;

 TSPI_SHADER_PGM_HI_ES=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_HI_GS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_HI_HS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_HI_LS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_HI_PS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_HI_VS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_LO_ES=bit32;

 TSPI_SHADER_PGM_LO_GS=bit32;

 TSPI_SHADER_PGM_LO_HS=bit32;

 TSPI_SHADER_PGM_LO_LS=bit32;

 TSPI_SHADER_PGM_LO_PS=bit32;

 TSPI_SHADER_PGM_LO_VS=bit32;

 TSPI_SHADER_TBA_HI_ES=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TBA_HI_GS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TBA_HI_HS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TBA_HI_LS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TBA_HI_PS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TBA_HI_VS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TBA_LO_ES=bit32;

 TSPI_SHADER_TBA_LO_GS=bit32;

 TSPI_SHADER_TBA_LO_HS=bit32;

 TSPI_SHADER_TBA_LO_LS=bit32;

 TSPI_SHADER_TBA_LO_PS=bit32;

 TSPI_SHADER_TBA_LO_VS=bit32;

 TSPI_SHADER_TMA_HI_ES=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TMA_HI_GS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TMA_HI_HS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TMA_HI_LS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TMA_HI_PS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TMA_HI_VS=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_TMA_LO_ES=bit32;

 TSPI_SHADER_TMA_LO_GS=bit32;

 TSPI_SHADER_TMA_LO_HS=bit32;

 TSPI_SHADER_TMA_LO_LS=bit32;

 TSPI_SHADER_TMA_LO_PS=bit32;

 TSPI_SHADER_TMA_LO_VS=bit32;

 TSPI_SLAVE_DEBUG_BUSY=bitpacked record
  LS_VTX_BUSY     :bit1;
  HS_VTX_BUSY     :bit1;
  ES_VTX_BUSY     :bit1;
  GS_VTX_BUSY     :bit1;
  VS_VTX_BUSY     :bit1;
  VGPR_WC00_BUSY  :bit1;
  VGPR_WC01_BUSY  :bit1;
  VGPR_WC10_BUSY  :bit1;
  VGPR_WC11_BUSY  :bit1;
  SGPR_WC00_BUSY  :bit1;
  SGPR_WC01_BUSY  :bit1;
  SGPR_WC02_BUSY  :bit1;
  SGPR_WC03_BUSY  :bit1;
  SGPR_WC10_BUSY  :bit1;
  SGPR_WC11_BUSY  :bit1;
  SGPR_WC12_BUSY  :bit1;
  SGPR_WC13_BUSY  :bit1;
  WAVEBUFFER0_BUSY:bit1;
  WAVEBUFFER1_BUSY:bit1;
  WAVE_WC0_BUSY   :bit1;
  WAVE_WC1_BUSY   :bit1;
  EVENT_CNTL_BUSY :bit1;
  SAVE_CTX_BUSY   :bit1;
  RESERVED0       :bit9;
 end;

 TSPI_WF_LIFETIME_CNTL=bitpacked record
  SAMPLE_PERIOD:bit4;
  EN           :bit1;
  RESERVED0    :bit27;
 end;

 TSQ_PERFCOUNTER_CTRL2=bitpacked record
  FORCE_EN :bit1;
  RESERVED0:bit31;
 end;

 TSQ_THREAD_TRACE_BASE=bit32;

 TSQ_THREAD_TRACE_CNTR=bit32;

 TSQ_THREAD_TRACE_CTRL=bitpacked record
  RESERVED0   :bit31;
  RESET_BUFFER:bit1;
 end;

 TSQ_THREAD_TRACE_MASK=bitpacked record
  CU_SEL      :bit5;
  SH_SEL      :bit1;
  RESERVED0   :bit1;
  REG_STALL_EN:bit1;
  SIMD_EN     :bit4;
  VM_ID_MASK  :bit2;
  SPI_STALL_EN:bit1;
  SQ_STALL_EN :bit1;
  RANDOM_SEED :bit16;
 end;

 TSQ_THREAD_TRACE_MODE=bitpacked record
  MASK_PS     :bit3;
  MASK_VS     :bit3;
  MASK_GS     :bit3;
  MASK_ES     :bit3;
  MASK_HS     :bit3;
  MASK_LS     :bit3;
  MASK_CS     :bit3;
  MODE        :bit2;
  CAPTURE_MODE:bit2;
  AUTOFLUSH_EN:bit1;
  PRIV        :bit1;
  ISSUE_MASK  :bit2;
  TEST_MODE   :bit1;
  INTERRUPT_EN:bit1;
  WRAP        :bit1;
 end;

 TSQ_THREAD_TRACE_SIZE=bitpacked record
  SIZE     :bit22;
  RESERVED0:bit10;
 end;

 TSQ_THREAD_TRACE_WPTR=bitpacked record
  WPTR       :bit30;
  READ_OFFSET:bit2;
 end;

 TSX_BLEND_OPT_CONTROL=bitpacked record
  MRT0_COLOR_OPT_DISABLE:bit1;
  MRT0_ALPHA_OPT_DISABLE:bit1;
  RESERVED0             :bit2;
  MRT1_COLOR_OPT_DISABLE:bit1;
  MRT1_ALPHA_OPT_DISABLE:bit1;
  RESERVED1             :bit2;
  MRT2_COLOR_OPT_DISABLE:bit1;
  MRT2_ALPHA_OPT_DISABLE:bit1;
  RESERVED2             :bit2;
  MRT3_COLOR_OPT_DISABLE:bit1;
  MRT3_ALPHA_OPT_DISABLE:bit1;
  RESERVED3             :bit2;
  MRT4_COLOR_OPT_DISABLE:bit1;
  MRT4_ALPHA_OPT_DISABLE:bit1;
  RESERVED4             :bit2;
  MRT5_COLOR_OPT_DISABLE:bit1;
  MRT5_ALPHA_OPT_DISABLE:bit1;
  RESERVED5             :bit2;
  MRT6_COLOR_OPT_DISABLE:bit1;
  MRT6_ALPHA_OPT_DISABLE:bit1;
  RESERVED6             :bit2;
  MRT7_COLOR_OPT_DISABLE:bit1;
  MRT7_ALPHA_OPT_DISABLE:bit1;
  RESERVED7             :bit1;
  PIXEN_ZERO_OPT_DISABLE:bit1;
 end;

 TSX_BLEND_OPT_EPSILON=bitpacked record
  MRT0_EPSILON:bit4;
  MRT1_EPSILON:bit4;
  MRT2_EPSILON:bit4;
  MRT3_EPSILON:bit4;
  MRT4_EPSILON:bit4;
  MRT5_EPSILON:bit4;
  MRT6_EPSILON:bit4;
  MRT7_EPSILON:bit4;
 end;

 TVGT_DMA_LS_HS_CONFIG=bitpacked record
  RESERVED0      :bit8;
  HS_NUM_INPUT_CP:bit6;
  RESERVED1      :bit18;
 end;

 TVGT_GROUP_FIRST_DECR=bitpacked record
  FIRST_DECR:bit4;
  RESERVED0 :bit28;
 end;

 TVGT_GS_OUT_PRIM_TYPE=bitpacked record
  OUTPRIM_TYPE          :bit6;
  RESERVED0             :bit2;
  OUTPRIM_TYPE_1        :bit6;
  RESERVED1             :bit2;
  OUTPRIM_TYPE_2        :bit6;
  OUTPRIM_TYPE_3        :bit6;
  RESERVED2             :bit3;
  UNIQUE_TYPE_PER_STREAM:bit1;
 end;

 TVGT_GS_VERT_ITEMSIZE=bitpacked record
  ITEMSIZE :bit15;
  RESERVED0:bit17;
 end;

 TVGT_HS_OFFCHIP_PARAM=bitpacked record
  OFFCHIP_BUFFERING  :bit9;
  OFFCHIP_GRANULARITY:bit2;
  RESERVED0          :bit21;
 end;

 TVGT_OUTPUT_PATH_CNTL=bitpacked record
  PATH_SELECT:bit3;
  RESERVED0  :bit29;
 end;

 TVGT_OUT_DEALLOC_CNTL=bitpacked record
  DEALLOC_DIST:bit7;
  RESERVED0   :bit25;
 end;

 TVGT_SHADER_STAGES_EN=bitpacked record
  LS_EN              :bit2;
  HS_EN              :bit1;
  ES_EN              :bit2;
  GS_EN              :bit1;
  VS_EN              :bit2;
  DYNAMIC_HS         :bit1;
  DISPATCH_DRAW_EN   :bit1;
  DIS_DEALLOC_ACCUM_0:bit1;
  DIS_DEALLOC_ACCUM_1:bit1;
  VS_WAVE_ID_EN      :bit1;
  RESERVED0          :bit19;
 end;

 TCB_COLOR0_CLEAR_WORD0=bit32;

 TCB_COLOR0_CLEAR_WORD1=bit32;

 TCB_COLOR0_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR0_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR0_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR1_CLEAR_WORD0=bit32;

 TCB_COLOR1_CLEAR_WORD1=bit32;

 TCB_COLOR1_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR1_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR1_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR2_CLEAR_WORD0=bit32;

 TCB_COLOR2_CLEAR_WORD1=bit32;

 TCB_COLOR2_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR2_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR2_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR3_CLEAR_WORD0=bit32;

 TCB_COLOR3_CLEAR_WORD1=bit32;

 TCB_COLOR3_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR3_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR3_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR4_CLEAR_WORD0=bit32;

 TCB_COLOR4_CLEAR_WORD1=bit32;

 TCB_COLOR4_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR4_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR4_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR5_CLEAR_WORD0=bit32;

 TCB_COLOR5_CLEAR_WORD1=bit32;

 TCB_COLOR5_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR5_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR5_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR6_CLEAR_WORD0=bit32;

 TCB_COLOR6_CLEAR_WORD1=bit32;

 TCB_COLOR6_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR6_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR6_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_COLOR7_CLEAR_WORD0=bit32;

 TCB_COLOR7_CLEAR_WORD1=bit32;

 TCB_COLOR7_CMASK_SLICE=bitpacked record
  TILE_MAX :bit14;
  RESERVED0:bit18;
 end;

 TCB_COLOR7_DCC_CONTROL=bitpacked record
  OVERWRITE_COMBINER_DISABLE :bit1;
  KEY_CLEAR_ENABLE           :bit1;
  MAX_UNCOMPRESSED_BLOCK_SIZE:bit2;
  MIN_COMPRESSED_BLOCK_SIZE  :bit1;
  MAX_COMPRESSED_BLOCK_SIZE  :bit2;
  COLOR_TRANSFORM            :bit2;
  INDEPENDENT_64B_BLOCKS     :bit1;
  LOSSY_RGB_PRECISION        :bit4;
  LOSSY_ALPHA_PRECISION      :bit4;
  RESERVED0                  :bit14;
 end;

 TCB_COLOR7_FMASK_SLICE=bitpacked record
  TILE_MAX :bit22;
  RESERVED0:bit10;
 end;

 TCB_PERFCOUNTER_FILTER=bitpacked record
  OP_FILTER_ENABLE           :bit1;
  OP_FILTER_SEL              :bit3;
  FORMAT_FILTER_ENABLE       :bit1;
  FORMAT_FILTER_SEL          :bit5;
  CLEAR_FILTER_ENABLE        :bit1;
  CLEAR_FILTER_SEL           :bit1;
  MRT_FILTER_ENABLE          :bit1;
  MRT_FILTER_SEL             :bit3;
  RESERVED0                  :bit1;
  NUM_SAMPLES_FILTER_ENABLE  :bit1;
  NUM_SAMPLES_FILTER_SEL     :bit3;
  NUM_FRAGMENTS_FILTER_ENABLE:bit1;
  NUM_FRAGMENTS_FILTER_SEL   :bit2;
  RESERVED1                  :bit8;
 end;

 TCOMPUTE_MISC_RESERVED=bitpacked record
  SEND_SEID   :bit2;
  RESERVED0   :bit3;
  WAVE_ID_BASE:bit12;
  RESERVED1   :bit15;
 end;

 TCP_CPC_MGCG_SYNC_CNTL=bitpacked record
  COOLDOWN_PERIOD:bit8;
  WARMUP_PERIOD  :bit8;
  RESERVED0      :bit16;
 end;

 TCP_DISPATCH_INDR_ADDR=bit32;

 TCP_DMA_ME_DST_ADDR_HI=bitpacked record
  DST_ADDR_HI:bit16;
  RESERVED0  :bit16;
 end;

 TCP_DMA_ME_SRC_ADDR_HI=bitpacked record
  SRC_ADDR_HI:bit16;
  RESERVED0  :bit4;
  RESERVED1  :bit2;
  RESERVED2  :bit7;
  RESERVED3  :bit2;
  RESERVED4  :bit1;
 end;

 TCP_EOP_DONE_DATA_CNTL=bitpacked record
  CNTX_ID  :bit16;
  DST_SEL  :bit2;
  RESERVED0:bit6;
  INT_SEL  :bit3;
  RESERVED1:bit2;
  DATA_SEL :bit3;
 end;

 TCP_HQD_QUEUE_PRIORITY=bitpacked record
  PRIORITY_LEVEL:bit4;
  RESERVED0     :bit28;
 end;

 TCP_IB1_PREAMBLE_BEGIN=bitpacked record
  IB1_PREAMBLE_BEGIN:bit20;
  RESERVED0         :bit12;
 end;

 TCP_IB2_PREAMBLE_BEGIN=bitpacked record
  IB2_PREAMBLE_BEGIN:bit20;
  RESERVED0         :bit12;
 end;

 TCP_INDEX_BASE_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_ME0_PIPE0_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME0_PIPE1_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME0_PIPE2_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME1_INT_STAT_DEBUG=bitpacked record
  RESERVED0                      :bit12;
  CMP_QUERY_STATUS_INT_ASSERTED  :bit1;
  DEQUEUE_REQUEST_INT_ASSERTED   :bit1;
  CP_ECC_ERROR_INT_ASSERTED      :bit1;
  SUA_VIOLATION_INT_STATUS       :bit1;
  RESERVED1                      :bit1;
  WRM_POLL_TIMEOUT_INT_ASSERTED  :bit1;
  RESERVED2                      :bit5;
  PRIV_REG_INT_ASSERTED          :bit1;
  OPCODE_ERROR_INT_ASSERTED      :bit1;
  RESERVED3                      :bit1;
  TIME_STAMP_INT_ASSERTED        :bit1;
  RESERVED_BIT_ERROR_INT_ASSERTED:bit1;
  RESERVED4                      :bit1;
  GENERIC2_INT_ASSERTED          :bit1;
  GENERIC1_INT_ASSERTED          :bit1;
  GENERIC0_INT_ASSERTED          :bit1;
 end;

 TCP_ME1_PIPE0_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME1_PIPE0_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME1_PIPE1_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME1_PIPE1_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME1_PIPE2_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME1_PIPE2_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME1_PIPE3_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME1_PIPE3_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME2_INT_STAT_DEBUG=bitpacked record
  RESERVED0                      :bit12;
  CMP_QUERY_STATUS_INT_ASSERTED  :bit1;
  DEQUEUE_REQUEST_INT_ASSERTED   :bit1;
  CP_ECC_ERROR_INT_ASSERTED      :bit1;
  SUA_VIOLATION_INT_STATUS       :bit1;
  RESERVED1                      :bit1;
  WRM_POLL_TIMEOUT_INT_ASSERTED  :bit1;
  RESERVED2                      :bit5;
  PRIV_REG_INT_ASSERTED          :bit1;
  OPCODE_ERROR_INT_ASSERTED      :bit1;
  RESERVED3                      :bit1;
  TIME_STAMP_INT_ASSERTED        :bit1;
  RESERVED_BIT_ERROR_INT_ASSERTED:bit1;
  RESERVED4                      :bit1;
  GENERIC2_INT_ASSERTED          :bit1;
  GENERIC1_INT_ASSERTED          :bit1;
  GENERIC0_INT_ASSERTED          :bit1;
 end;

 TCP_ME2_PIPE0_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME2_PIPE0_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME2_PIPE1_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME2_PIPE1_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME2_PIPE2_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME2_PIPE2_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_ME2_PIPE3_INT_CNTL=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_ENABLE  :bit1;
  DEQUEUE_REQUEST_INT_ENABLE   :bit1;
  CP_ECC_ERROR_INT_ENABLE      :bit1;
  SUA_VIOLATION_INT_ENABLE     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_ENABLE  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_ENABLE          :bit1;
  OPCODE_ERROR_INT_ENABLE      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_ENABLE        :bit1;
  RESERVED_BIT_ERROR_INT_ENABLE:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_ENABLE          :bit1;
  GENERIC1_INT_ENABLE          :bit1;
  GENERIC0_INT_ENABLE          :bit1;
 end;

 TCP_ME2_PIPE3_PRIORITY=bitpacked record
  PRIORITY :bit2;
  RESERVED0:bit30;
 end;

 TCP_MEC1_F32_INTERRUPT=bitpacked record
  RESERVED0   :bit1;
  PRIV_REG_INT:bit1;
  RESERVED1   :bit1;
  RESERVED2   :bit1;
  RESERVED3   :bit1;
  RESERVED4   :bit1;
  RESERVED5   :bit1;
  RESERVED6   :bit1;
  RESERVED7   :bit1;
  RESERVED8   :bit1;
  RESERVED9   :bit22;
 end;

 TCP_MEC2_F32_INTERRUPT=bitpacked record
  RESERVED0   :bit1;
  PRIV_REG_INT:bit1;
  RESERVED1   :bit1;
  RESERVED2   :bit1;
  RESERVED3   :bit1;
  RESERVED4   :bit1;
  RESERVED5   :bit1;
  RESERVED6   :bit1;
  RESERVED7   :bit1;
  RESERVED8   :bit1;
  RESERVED9   :bit22;
 end;

 TCP_MEC_ME1_UCODE_ADDR=bitpacked record
  UCODE_ADDR:bit17;
  RESERVED0 :bit15;
 end;

 TCP_MEC_ME1_UCODE_DATA=bit32;

 TCP_MEC_ME2_UCODE_ADDR=bitpacked record
  UCODE_ADDR:bit17;
  RESERVED0 :bit15;
 end;

 TCP_MEC_ME2_UCODE_DATA=bit32;

 TCP_PA_CINVOC_COUNT_HI=bit32;

 TCP_PA_CINVOC_COUNT_LO=bit32;

 TCP_PIPE_STATS_ADDR_HI=bitpacked record
  PIPE_STATS_ADDR_HI:bit16;
  RESERVED0         :bit16;
 end;

 TCP_PIPE_STATS_ADDR_LO=bitpacked record
  PIPE_STATS_ADDR_SWAP__SI__CI:bit2;
  PIPE_STATS_ADDR_LO          :bit30;
 end;

 TCP_PIPE_STATS_CONTROL=bitpacked record
  RESERVED0    :bit25;
  CACHE_CONTROL:bit1;
  RESERVED1    :bit1;
  MTYPE        :bit2;
  RESERVED2    :bit3;
 end;

 TCP_PQ_WPTR_POLL_CNTL1=bit32;

 TCP_STREAM_OUT_ADDR_HI=bitpacked record
  STREAM_OUT_ADDR_HI:bit16;
  RESERVED0         :bit16;
 end;

 TCP_STREAM_OUT_ADDR_LO=bitpacked record
  STREAM_OUT_ADDR_SWAP__SI__CI:bit2;
  STREAM_OUT_ADDR_LO          :bit30;
 end;

 TCP_STREAM_OUT_CONTROL=bitpacked record
  RESERVED0    :bit25;
  CACHE_CONTROL:bit1;
  RESERVED1    :bit1;
  MTYPE        :bit2;
  RESERVED2    :bit3;
 end;

 TDB_STENCIL_WRITE_BASE=bit32;

 TGDS_GWS_RESOURCE_CNTL=bitpacked record
  INDEX :bit6;
  UNUSED:bit26;
 end;

 TGRBM_WAIT_IDLE_CLOCKS=bitpacked record
  WAIT_IDLE_CLOCKS:bit8;
  RESERVED0       :bit24;
 end;

 TPA_CL_VPORT_XOFFSET_1=bit32;

 TPA_CL_VPORT_XOFFSET_2=bit32;

 TPA_CL_VPORT_XOFFSET_3=bit32;

 TPA_CL_VPORT_XOFFSET_4=bit32;

 TPA_CL_VPORT_XOFFSET_5=bit32;

 TPA_CL_VPORT_XOFFSET_6=bit32;

 TPA_CL_VPORT_XOFFSET_7=bit32;

 TPA_CL_VPORT_XOFFSET_8=bit32;

 TPA_CL_VPORT_XOFFSET_9=bit32;

 TPA_CL_VPORT_XSCALE_10=bit32;

 TPA_CL_VPORT_XSCALE_11=bit32;

 TPA_CL_VPORT_XSCALE_12=bit32;

 TPA_CL_VPORT_XSCALE_13=bit32;

 TPA_CL_VPORT_XSCALE_14=bit32;

 TPA_CL_VPORT_XSCALE_15=bit32;

 TPA_CL_VPORT_YOFFSET_1=bit32;

 TPA_CL_VPORT_YOFFSET_2=bit32;

 TPA_CL_VPORT_YOFFSET_3=bit32;

 TPA_CL_VPORT_YOFFSET_4=bit32;

 TPA_CL_VPORT_YOFFSET_5=bit32;

 TPA_CL_VPORT_YOFFSET_6=bit32;

 TPA_CL_VPORT_YOFFSET_7=bit32;

 TPA_CL_VPORT_YOFFSET_8=bit32;

 TPA_CL_VPORT_YOFFSET_9=bit32;

 TPA_CL_VPORT_YSCALE_10=bit32;

 TPA_CL_VPORT_YSCALE_11=bit32;

 TPA_CL_VPORT_YSCALE_12=bit32;

 TPA_CL_VPORT_YSCALE_13=bit32;

 TPA_CL_VPORT_YSCALE_14=bit32;

 TPA_CL_VPORT_YSCALE_15=bit32;

 TPA_CL_VPORT_ZOFFSET_1=bit32;

 TPA_CL_VPORT_ZOFFSET_2=bit32;

 TPA_CL_VPORT_ZOFFSET_3=bit32;

 TPA_CL_VPORT_ZOFFSET_4=bit32;

 TPA_CL_VPORT_ZOFFSET_5=bit32;

 TPA_CL_VPORT_ZOFFSET_6=bit32;

 TPA_CL_VPORT_ZOFFSET_7=bit32;

 TPA_CL_VPORT_ZOFFSET_8=bit32;

 TPA_CL_VPORT_ZOFFSET_9=bit32;

 TPA_CL_VPORT_ZSCALE_10=bit32;

 TPA_CL_VPORT_ZSCALE_11=bit32;

 TPA_CL_VPORT_ZSCALE_12=bit32;

 TPA_CL_VPORT_ZSCALE_13=bit32;

 TPA_CL_VPORT_ZSCALE_14=bit32;

 TPA_CL_VPORT_ZSCALE_15=bit32;

 TPA_SC_FIFO_DEPTH_CNTL=bitpacked record
  DEPTH    :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER0_HI=bit32;

 TPA_SC_PERFCOUNTER0_LO=bit32;

 TPA_SC_PERFCOUNTER1_HI=bit32;

 TPA_SC_PERFCOUNTER1_LO=bit32;

 TPA_SC_PERFCOUNTER2_HI=bit32;

 TPA_SC_PERFCOUNTER2_LO=bit32;

 TPA_SC_PERFCOUNTER3_HI=bit32;

 TPA_SC_PERFCOUNTER3_LO=bit32;

 TPA_SC_PERFCOUNTER4_HI=bit32;

 TPA_SC_PERFCOUNTER4_LO=bit32;

 TPA_SC_PERFCOUNTER5_HI=bit32;

 TPA_SC_PERFCOUNTER5_LO=bit32;

 TPA_SC_PERFCOUNTER6_HI=bit32;

 TPA_SC_PERFCOUNTER6_LO=bit32;

 TPA_SC_PERFCOUNTER7_HI=bit32;

 TPA_SC_PERFCOUNTER7_LO=bit32;

 TPA_SC_RASTER_CONFIG_1=bitpacked record
  SE_PAIR_MAP :bit2;
  SE_PAIR_XSEL:bit2;
  SE_PAIR_YSEL:bit2;
  RESERVED0   :bit26;
 end;

 TPA_SU_PERFCOUNTER0_HI=bitpacked record
  PERFCOUNTER_HI:bit16;
  RESERVED0     :bit16;
 end;

 TPA_SU_PERFCOUNTER0_LO=bit32;

 TPA_SU_PERFCOUNTER1_HI=bitpacked record
  PERFCOUNTER_HI:bit16;
  RESERVED0     :bit16;
 end;

 TPA_SU_PERFCOUNTER1_LO=bit32;

 TPA_SU_PERFCOUNTER2_HI=bitpacked record
  PERFCOUNTER_HI:bit16;
  RESERVED0     :bit16;
 end;

 TPA_SU_PERFCOUNTER2_LO=bit32;

 TPA_SU_PERFCOUNTER3_HI=bitpacked record
  PERFCOUNTER_HI:bit16;
  RESERVED0     :bit16;
 end;

 TPA_SU_PERFCOUNTER3_LO=bit32;

 TSPI_SHADER_COL_FORMAT=bitpacked record
  COL0_EXPORT_FORMAT:bit4;
  COL1_EXPORT_FORMAT:bit4;
  COL2_EXPORT_FORMAT:bit4;
  COL3_EXPORT_FORMAT:bit4;
  COL4_EXPORT_FORMAT:bit4;
  COL5_EXPORT_FORMAT:bit4;
  COL6_EXPORT_FORMAT:bit4;
  COL7_EXPORT_FORMAT:bit4;
 end;

 TSPI_SHADER_POS_FORMAT=bitpacked record
  POS0_EXPORT_FORMAT:bit4;
  POS1_EXPORT_FORMAT:bit4;
  POS2_EXPORT_FORMAT:bit4;
  POS3_EXPORT_FORMAT:bit4;
  RESERVED0         :bit16;
 end;

 TSPI_WF_LIFETIME_DEBUG=bitpacked record
  START_VALUE:bit31;
  OVERRIDE_EN:bit1;
 end;

 TSQ_THREAD_TRACE_BASE2=bitpacked record
  ADDR_HI  :bit4;
  ATC      :bit1;
  RESERVED0:bit27;
 end;

 TTA_CS_BC_BASE_ADDR_HI=bitpacked record
  ADDRESS  :bit8;
  RESERVED0:bit24;
 end;

 TVGT_DMA_NUM_INSTANCES=bit32;

 TVGT_EVENT_ADDRESS_REG=bitpacked record
  ADDRESS_LOW:bit28;
  RESERVED0  :bit4;
 end;

 TVGT_GROUP_VECT_0_CNTL=bitpacked record
  COMP_X_EN:bit1;
  COMP_Y_EN:bit1;
  COMP_Z_EN:bit1;
  COMP_W_EN:bit1;
  RESERVED0:bit4;
  STRIDE   :bit8;
  SHIFT    :bit8;
  RESERVED1:bit8;
 end;

 TVGT_GROUP_VECT_1_CNTL=bitpacked record
  COMP_X_EN:bit1;
  COMP_Y_EN:bit1;
  COMP_Z_EN:bit1;
  COMP_W_EN:bit1;
  RESERVED0:bit4;
  STRIDE   :bit8;
  SHIFT    :bit8;
  RESERVED1:bit8;
 end;

 TVGT_PRIMITIVEID_RESET=bit32;

 TVGT_TESS_DISTRIBUTION=bitpacked record
  ACCUM_ISOLINE:bit8;
  ACCUM_TRI    :bit8;
  ACCUM_QUAD   :bit8;
  DONUT_SPLIT  :bit5;
  TRAP_SPLIT   :bit3;
 end;

 TCB_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit9;
  RESERVED0 :bit1;
  PERF_SEL1 :bit9;
  RESERVED1 :bit1;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TCB_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit9;
  RESERVED0:bit19;
  PERF_MODE:bit4;
 end;

 TCB_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit9;
  RESERVED0:bit19;
  PERF_MODE:bit4;
 end;

 TCB_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit9;
  RESERVED0:bit19;
  PERF_MODE:bit4;
 end;

 TCOMPUTE_THREADGROUP_ID=bit32;

 TCP_CE_PRGRM_CNTR_START=bitpacked record
  IP_START :bit11;
  RESERVED0:bit21;
 end;

 TCP_CPC_GRBM_FREE_COUNT=bitpacked record
  FREE_COUNT:bit6;
  RESERVED0 :bit26;
 end;

 TCP_CPC_HALT_HYST_COUNT=bitpacked record
  COUNT    :bit4;
  RESERVED0:bit28;
 end;

 TCP_DE_LAST_INVAL_COUNT=bit32;

 TCP_DMA_PFP_DST_ADDR_HI=bitpacked record
  DST_ADDR_HI:bit16;
  RESERVED0  :bit16;
 end;

 TCP_DMA_PFP_SRC_ADDR_HI=bitpacked record
  SRC_ADDR_HI:bit16;
  RESERVED0  :bit4;
  RESERVED1  :bit2;
  RESERVED2  :bit7;
  RESERVED3  :bit2;
  RESERVED4  :bit1;
 end;

 TCP_DRAW_INDX_INDR_ADDR=bit32;

 TCP_DRAW_OBJECT_COUNTER=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TCP_DRAW_WINDOW_MASK_HI=bit32;

 TCP_ECC_FIRSTOCCURRENCE=bitpacked record
  _INTERFACE:bit2;
  RESERVED0 :bit2;
  CLIENT    :bit4;
  ME        :bit2;
  PIPE      :bit2;
  QUEUE     :bit3;
  RESERVED1 :bit1;
  VMID      :bit4;
  RESERVED2 :bit12;
 end;

 TCP_EOP_DONE_EVENT_CNTL=bitpacked record
  WBINV_TC_OP     :bit7;
  RESERVED0       :bit5;
  WBINV_ACTION_ENA:bit6;
  RESERVED1       :bit7;
  CACHE_CONTROL   :bit1;
  RESERVED2       :bit1;
  MTYPE           :bit2;
  RESERVED3       :bit3;
 end;

 TCP_HQD_CNTL_STACK_SIZE=bitpacked record
  RESERVED0:bit12;
  SIZE     :bit3;
  RESERVED1:bit17;
 end;

 TCP_HQD_DEQUEUE_REQUEST=bitpacked record
  DEQUEUE_REQ   :bit3;
  RESERVED0     :bit1;
  IQ_REQ_PEND   :bit1;
  RESERVED1     :bit3;
  DEQUEUE_INT   :bit1;
  IQ_REQ_PEND_EN:bit1;
  DEQUEUE_REQ_EN:bit1;
  RESERVED2     :bit21;
 end;

 TCP_HQD_IB_BASE_ADDR_HI=bitpacked record
  IB_BASE_ADDR_HI:bit16;
  RESERVED0      :bit16;
 end;

 TCP_HQD_WG_STATE_OFFSET=bitpacked record
  RESERVED0:bit2;
  OFFSET   :bit23;
  RESERVED1:bit7;
 end;

 TCP_MEC_ME1_HEADER_DUMP=bit32;

 TCP_MEC_ME2_HEADER_DUMP=bit32;

 TCP_PFP_ATOMIC_PREOP_HI=bit32;

 TCP_PFP_ATOMIC_PREOP_LO=bit32;

 TCP_PRT_LOD_STATS_CNTL0=bit32;

 TCP_PRT_LOD_STATS_CNTL1=bit32;

 TCP_PRT_LOD_STATS_CNTL2=bitpacked record
  BASE_HI               :bit2;
  INTERVAL              :bit8;
  RESET_CNT             :bit8;
  RESET_FORCE           :bit1;
  REPORT_AND_RESET      :bit1;
  MC_ENDIAN_SWAP__SI__CI:bit2;
  RESERVED0             :bit1;
  MC_VMID               :bit4;
  RESERVED1             :bit1;
  CACHE_POLICY          :bit1;
  RESERVED2             :bit1;
  MTYPE                 :bit2;
 end;

 TCP_VGT_GSPRIM_COUNT_HI=bit32;

 TCP_VGT_GSPRIM_COUNT_LO=bit32;

 TCP_VGT_IAPRIM_COUNT_HI=bit32;

 TCP_VGT_IAPRIM_COUNT_LO=bit32;

 TCP_VGT_IAVERT_COUNT_HI=bit32;

 TCP_VGT_IAVERT_COUNT_LO=bit32;

 TDB_OCCLUSION_COUNT0_HI=bitpacked record
  COUNT_HI :bit31;
  RESERVED0:bit1;
 end;

 TDB_OCCLUSION_COUNT1_HI=bitpacked record
  COUNT_HI :bit31;
  RESERVED0:bit1;
 end;

 TDB_OCCLUSION_COUNT2_HI=bitpacked record
  COUNT_HI :bit31;
  RESERVED0:bit1;
 end;

 TDB_OCCLUSION_COUNT3_HI=bitpacked record
  COUNT_HI :bit31;
  RESERVED0:bit1;
 end;

 TDB_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TDB_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TDB_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TDB_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TGDS_GWS_RESOURCE_RESET=bitpacked record
  RESET      :bit1;
  RESERVED0  :bit7;
  RESOURCE_ID:bit8;
  RESERVED1  :bit16;
 end;

 TIA_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TIA_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TIA_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TIA_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TPA_CL_GB_HORZ_CLIP_ADJ=bit32;

 TPA_CL_GB_HORZ_DISC_ADJ=bit32;

 TPA_CL_GB_VERT_CLIP_ADJ=bit32;

 TPA_CL_GB_VERT_DISC_ADJ=bit32;

 TPA_CL_VPORT_XOFFSET_10=bit32;

 TPA_CL_VPORT_XOFFSET_11=bit32;

 TPA_CL_VPORT_XOFFSET_12=bit32;

 TPA_CL_VPORT_XOFFSET_13=bit32;

 TPA_CL_VPORT_XOFFSET_14=bit32;

 TPA_CL_VPORT_XOFFSET_15=bit32;

 TPA_CL_VPORT_YOFFSET_10=bit32;

 TPA_CL_VPORT_YOFFSET_11=bit32;

 TPA_CL_VPORT_YOFFSET_12=bit32;

 TPA_CL_VPORT_YOFFSET_13=bit32;

 TPA_CL_VPORT_YOFFSET_14=bit32;

 TPA_CL_VPORT_YOFFSET_15=bit32;

 TPA_CL_VPORT_ZOFFSET_10=bit32;

 TPA_CL_VPORT_ZOFFSET_11=bit32;

 TPA_CL_VPORT_ZOFFSET_12=bit32;

 TPA_CL_VPORT_ZOFFSET_13=bit32;

 TPA_CL_VPORT_ZOFFSET_14=bit32;

 TPA_CL_VPORT_ZOFFSET_15=bit32;

 TPA_SU_PRIM_FILTER_CNTL=bitpacked record
  TRIANGLE_FILTER_DISABLE :bit1;
  LINE_FILTER_DISABLE     :bit1;
  POINT_FILTER_DISABLE    :bit1;
  RECTANGLE_FILTER_DISABLE:bit1;
  TRIANGLE_EXPAND_ENA     :bit1;
  LINE_EXPAND_ENA         :bit1;
  POINT_EXPAND_ENA        :bit1;
  RECTANGLE_EXPAND_ENA    :bit1;
  PRIM_EXPAND_CONSTANT    :bit8;
  RESERVED0               :bit14;
  XMAX_RIGHT_EXCLUSION    :bit1;
  YMAX_BOTTOM_EXCLUSION   :bit1;
 end;

 TSQ_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER4_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER5_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER6_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER7_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER8_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER9_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_THREAD_TRACE_STATUS=bitpacked record
  FINISH_PENDING:bit10;
  RESERVED0     :bit6;
  FINISH_DONE   :bit10;
  RESERVED1     :bit3;
  NEW_BUF       :bit1;
  BUSY          :bit1;
  FULL          :bit1;
 end;

 TSX_PERFCOUNTER0_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TSX_PERFCOUNTER1_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TSX_PERFCOUNTER2_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TSX_PERFCOUNTER3_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TTA_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit8;
  RESERVED0 :bit2;
  PERF_SEL1 :bit8;
  RESERVED1 :bit2;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTA_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit8;
  RESERVED0 :bit2;
  PERF_SEL1 :bit8;
  RESERVED1 :bit2;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCP_ATC_EDC_GATCL1_CNT=bitpacked record
  DATA_SEC :bit8;
  RESERVED0:bit24;
 end;

 TTD_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit8;
  RESERVED0 :bit2;
  PERF_SEL1 :bit8;
  RESERVED1 :bit2;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTD_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit8;
  RESERVED0 :bit2;
  PERF_SEL1 :bit8;
  RESERVED1 :bit2;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TVGT_CACHE_INVALIDATION=bitpacked record
  CACHE_INVALIDATION  :bit2;
  RESERVED0           :bit2;
  DIS_INSTANCING_OPT  :bit1;
  VS_NO_EXTRA_BUFFER  :bit1;
  AUTO_INVLD_EN       :bit2;
  RESERVED1           :bit1;
  USE_GS_DONE         :bit1;
  RESERVED2           :bit1;
  DIS_RANGE_FULL_INVLD:bit1;
  GS_LATE_ALLOC_EN    :bit1;
  STREAMOUT_FULL_FLUSH:bit1;
  RESERVED3           :bit2;
  ES_LIMIT            :bit5;
  RESERVED4           :bit11;
 end;

 TVGT_DMA_PRIMITIVE_TYPE=bitpacked record
  PRIM_TYPE:bit6;
  RESERVED0:bit26;
 end;

 TVGT_DMA_REQ_FIFO_DEPTH=bitpacked record
  DMA_REQ_FIFO_DEPTH:bit6;
  RESERVED0         :bit26;
 end;

 TVGT_ESGS_RING_ITEMSIZE=bitpacked record
  ITEMSIZE :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GSVS_RING_ITEMSIZE=bitpacked record
  ITEMSIZE :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GSVS_RING_OFFSET_1=bitpacked record
  OFFSET   :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GSVS_RING_OFFSET_2=bitpacked record
  OFFSET   :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GSVS_RING_OFFSET_3=bitpacked record
  OFFSET   :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GS_VERT_ITEMSIZE_1=bitpacked record
  ITEMSIZE :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GS_VERT_ITEMSIZE_2=bitpacked record
  ITEMSIZE :bit15;
  RESERVED0:bit17;
 end;

 TVGT_GS_VERT_ITEMSIZE_3=bitpacked record
  ITEMSIZE :bit15;
  RESERVED0:bit17;
 end;

 TVGT_HOS_MAX_TESS_LEVEL=bit32;

 TVGT_HOS_MIN_TESS_LEVEL=bit32;

 TVGT_VTX_VECT_EJECT_REG=bitpacked record
  PRIM_COUNT:bit10;
  RESERVED0 :bit22;
 end;

 TWD_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TWD_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TWD_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TWD_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TCB_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit9;
  RESERVED0 :bit1;
  PERF_SEL3 :bit9;
  RESERVED1 :bit5;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TCOMPUTE_RESOURCE_LIMITS=bitpacked record
  WAVES_PER_SH   :bit10;
  RESERVED0      :bit2;
  TG_PER_CU      :bit4;
  LOCK_THRESHOLD :bit6;
  SIMD_DEST_CNTL :bit1;
  FORCE_SIMD_DIST:bit1;
  CU_GROUP_COUNT :bit3;
  RESERVED1      :bit5;
 end;

 TCPC_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit6;
  RESERVED0:bit4;
  PERF_SEL1:bit6;
  RESERVED1:bit4;
  CNTR_MODE:bit4;
  RESERVED2:bit8;
 end;

 TCPC_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit6;
  RESERVED0:bit26;
 end;

 TCPF_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit6;
  RESERVED0:bit4;
  PERF_SEL1:bit6;
  RESERVED1:bit4;
  CNTR_MODE:bit4;
  RESERVED2:bit8;
 end;

 TCPF_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit6;
  RESERVED0:bit26;
 end;

 TCPG_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit6;
  RESERVED0:bit4;
  PERF_SEL1:bit6;
  RESERVED1:bit4;
  CNTR_MODE:bit4;
  RESERVED2:bit8;
 end;

 TCPG_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit6;
  RESERVED0:bit26;
 end;

 TCP_APPEND_LAST_CS_FENCE=bit32;

 TCP_APPEND_LAST_PS_FENCE=bit32;

 TCP_CE_COMPLETION_STATUS=bitpacked record
  STATUS   :bit2;
  RESERVED0:bit30;
 end;

 TCP_GDS_ATOMIC0_PREOP_HI=bit32;

 TCP_GDS_ATOMIC0_PREOP_LO=bit32;

 TCP_GDS_ATOMIC1_PREOP_HI=bit32;

 TCP_GDS_ATOMIC1_PREOP_LO=bit32;

 TCP_HQD_ATOMIC0_PREOP_HI=bit32;

 TCP_HQD_ATOMIC0_PREOP_LO=bit32;

 TCP_HQD_ATOMIC1_PREOP_HI=bit32;

 TCP_HQD_ATOMIC1_PREOP_LO=bit32;

 TCP_HQD_CTX_SAVE_CONTROL=bitpacked record
  ATC      :bit1;
  MTYPE    :bit2;
  POLICY   :bit1;
  RESERVED0:bit28;
 end;

 TCP_HQD_EOP_BASE_ADDR_HI=bitpacked record
  BASE_ADDR_HI:bit8;
  RESERVED0   :bit24;
 end;

 TCP_HQD_PERSISTENT_STATE=bitpacked record
  PRELOAD_REQ   :bit1;
  RESERVED0     :bit7;
  PRELOAD_SIZE  :bit10;
  RESERVED1     :bit10;
  RESTORE_ACTIVE:bit1;
  RELAUNCH_WAVES:bit1;
  QSWITCH_MODE  :bit1;
  DISP_ACTIVE   :bit1;
 end;

 TCP_ME1_PIPE0_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME1_PIPE1_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME1_PIPE2_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME1_PIPE3_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME2_PIPE0_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME2_PIPE1_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME2_PIPE2_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_ME2_PIPE3_INT_STATUS=bitpacked record
  RESERVED0                    :bit12;
  CMP_QUERY_STATUS_INT_STATUS  :bit1;
  DEQUEUE_REQUEST_INT_STATUS   :bit1;
  CP_ECC_ERROR_INT_STATUS      :bit1;
  SUA_VIOLATION_INT_STATUS     :bit1;
  RESERVED1                    :bit1;
  WRM_POLL_TIMEOUT_INT_STATUS  :bit1;
  RESERVED2                    :bit5;
  PRIV_REG_INT_STATUS          :bit1;
  OPCODE_ERROR_INT_STATUS      :bit1;
  RESERVED3                    :bit1;
  TIME_STAMP_INT_STATUS        :bit1;
  RESERVED_BIT_ERROR_INT_STATUS:bit1;
  RESERVED4                    :bit1;
  GENERIC2_INT_STATUS          :bit1;
  GENERIC1_INT_STATUS          :bit1;
  GENERIC0_INT_STATUS          :bit1;
 end;

 TCP_PFP_PRGRM_CNTR_START=bitpacked record
  IP_START :bit12;
  RESERVED0:bit20;
 end;

 TCP_SC_PSINVOC_COUNT0_HI=bit32;

 TCP_SC_PSINVOC_COUNT0_LO=bit32;

 TCP_SC_PSINVOC_COUNT1_HI=bit32;

 TCP_SC_PSINVOC_COUNT1_LO=bit32;

 TCP_VGT_CSINVOC_COUNT_HI=bit32;

 TCP_VGT_CSINVOC_COUNT_LO=bit32;

 TCP_VGT_DSINVOC_COUNT_HI=bit32;

 TCP_VGT_DSINVOC_COUNT_LO=bit32;

 TCP_VGT_GSINVOC_COUNT_HI=bit32;

 TCP_VGT_GSINVOC_COUNT_LO=bit32;

 TCP_VGT_HSINVOC_COUNT_HI=bit32;

 TCP_VGT_HSINVOC_COUNT_LO=bit32;

 TCP_VGT_VSINVOC_COUNT_HI=bit32;

 TCP_VGT_VSINVOC_COUNT_LO=bit32;

 TCP_WAIT_REG_MEM_TIMEOUT=bit32;

 TDB_OCCLUSION_COUNT0_LOW=bit32;

 TDB_OCCLUSION_COUNT1_LOW=bit32;

 TDB_OCCLUSION_COUNT2_LOW=bit32;

 TDB_OCCLUSION_COUNT3_LOW=bit32;

 TDB_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TDB_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TGDS_COMPUTE_MAX_WAVE_ID=bitpacked record
  MAX_WAVE_ID:bit12;
  RESERVED0  :bit20;
 end;

 TGDS_PERFCOUNTER0_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TGDS_PERFCOUNTER1_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TGDS_PERFCOUNTER2_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TGDS_PERFCOUNTER3_SELECT=bitpacked record
  PERFCOUNTER_SELECT :bit10;
  PERFCOUNTER_SELECT1:bit10;
  CNTR_MODE          :bit4;
  RESERVED0          :bit8;
 end;

 TGDS_VM_PROTECTION_FAULT=bitpacked record
  WRITE_DIS     :bit1;
  FAULT_DETECTED:bit1;
  GWS           :bit1;
  OA            :bit1;
  GRBM          :bit1;
  RESERVED0     :bit3;
  VMID          :bit4;
  RESERVED1     :bit4;
  ADDRESS       :bit16;
 end;

 TGRBM_SE0_PERFCOUNTER_HI=bit32;

 TGRBM_SE0_PERFCOUNTER_LO=bit32;

 TGRBM_SE1_PERFCOUNTER_HI=bit32;

 TGRBM_SE1_PERFCOUNTER_LO=bit32;

 TGRBM_SE2_PERFCOUNTER_HI=bit32;

 TGRBM_SE2_PERFCOUNTER_LO=bit32;

 TGRBM_SE3_PERFCOUNTER_HI=bit32;

 TGRBM_SE3_PERFCOUNTER_LO=bit32;

 TIA_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TPA_SC_AA_MASK_X0Y0_X1Y0=bitpacked record
  AA_MASK_X0Y0:bit16;
  AA_MASK_X1Y0:bit16;
 end;

 TPA_SC_AA_MASK_X0Y1_X1Y1=bitpacked record
  AA_MASK_X0Y1:bit16;
  AA_MASK_X1Y1:bit16;
 end;

 TPA_SC_P3D_TRAP_SCREEN_H=bitpacked record
  X_COORD  :bit14;
  RESERVED0:bit18;
 end;

 TPA_SC_P3D_TRAP_SCREEN_V=bitpacked record
  Y_COORD  :bit14;
  RESERVED0:bit18;
 end;

 TPA_SC_SCREEN_SCISSOR_BR=bitpacked record
  BR_X:bit16;
  BR_Y:bit16;
 end;

 TPA_SC_SCREEN_SCISSOR_TL=bitpacked record
  TL_X:bit16;
  TL_Y:bit16;
 end;

 TPA_SC_TRAP_SCREEN_COUNT=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TPA_SC_TRAP_SCREEN_HV_EN=bitpacked record
  ENABLE_HV_PRE_SHADER       :bit1;
  FORCE_PRE_SHADER_ALL_PIXELS:bit1;
  RESERVED0                  :bit30;
 end;

 TPA_SC_WINDOW_SCISSOR_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_WINDOW_SCISSOR_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SU_LINE_STIPPLE_CNTL=bitpacked record
  LINE_STIPPLE_RESET:bit2;
  EXPAND_FULL_LENGTH:bit1;
  FRACTIONAL_ACCUM  :bit1;
  DIAMOND_ADJUST    :bit1;
  RESERVED0         :bit27;
 end;

 TPA_SU_POLY_OFFSET_CLAMP=bit32;

 TSPI_COMPUTE_QUEUE_RESET=bitpacked record
  RESET    :bit1;
  RESERVED0:bit31;
 end;

 TSPI_COMPUTE_WF_CTX_SAVE=bitpacked record
  INITIATE         :bit1;
  GDS_INTERRUPT_EN :bit1;
  DONE_INTERRUPT_EN:bit1;
  RESERVED0        :bit27;
  GDS_REQ_BUSY     :bit1;
  SAVE_BUSY        :bit1;
 end;

 TSPI_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TSPI_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TSPI_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TSPI_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TSPI_PERFCOUNTER4_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit24;
 end;

 TSPI_PERFCOUNTER5_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit24;
 end;

 TSPI_SHADER_PGM_RSRC1_ES=bitpacked record
  VGPRS          :bit6;
  SGPRS          :bit4;
  PRIORITY       :bit2;
  FLOAT_MODE     :bit8;
  PRIV           :bit1;
  DX10_CLAMP     :bit1;
  DEBUG_MODE     :bit1;
  IEEE_MODE      :bit1;
  VGPR_COMP_CNT  :bit2;
  CU_GROUP_ENABLE:bit1;
  CACHE_CTL      :bit3;
  CDBG_USER      :bit1;
  RESERVED0      :bit1;
 end;

 TSPI_SHADER_PGM_RSRC1_GS=bitpacked record
  VGPRS          :bit6;
  SGPRS          :bit4;
  PRIORITY       :bit2;
  FLOAT_MODE     :bit8;
  PRIV           :bit1;
  DX10_CLAMP     :bit1;
  DEBUG_MODE     :bit1;
  IEEE_MODE      :bit1;
  CU_GROUP_ENABLE:bit1;
  CACHE_CTL      :bit3;
  CDBG_USER      :bit1;
  RESERVED0      :bit3;
 end;

 TSPI_SHADER_PGM_RSRC1_HS=bitpacked record
  VGPRS     :bit6;
  SGPRS     :bit4;
  PRIORITY  :bit2;
  FLOAT_MODE:bit8;
  PRIV      :bit1;
  DX10_CLAMP:bit1;
  DEBUG_MODE:bit1;
  IEEE_MODE :bit1;
  CACHE_CTL :bit3;
  CDBG_USER :bit1;
  RESERVED0 :bit4;
 end;

 TSPI_SHADER_PGM_RSRC1_LS=bitpacked record
  VGPRS        :bit6;
  SGPRS        :bit4;
  PRIORITY     :bit2;
  FLOAT_MODE   :bit8;
  PRIV         :bit1;
  DX10_CLAMP   :bit1;
  DEBUG_MODE   :bit1;
  IEEE_MODE    :bit1;
  VGPR_COMP_CNT:bit2;
  CACHE_CTL    :bit3;
  CDBG_USER    :bit1;
  RESERVED0    :bit2;
 end;

 TSPI_SHADER_PGM_RSRC1_PS=bitpacked record
  VGPRS           :bit6;
  SGPRS           :bit4;
  PRIORITY        :bit2;
  FLOAT_MODE      :bit8;
  PRIV            :bit1;
  DX10_CLAMP      :bit1;
  DEBUG_MODE      :bit1;
  IEEE_MODE       :bit1;
  CU_GROUP_DISABLE:bit1;
  CACHE_CTL       :bit3;
  CDBG_USER       :bit1;
  RESERVED0       :bit3;
 end;

 TSPI_SHADER_PGM_RSRC1_VS=bitpacked record
  VGPRS          :bit6;
  SGPRS          :bit4;
  PRIORITY       :bit2;
  FLOAT_MODE     :bit8;
  PRIV           :bit1;
  DX10_CLAMP     :bit1;
  DEBUG_MODE     :bit1;
  IEEE_MODE      :bit1;
  VGPR_COMP_CNT  :bit2;
  CU_GROUP_ENABLE:bit1;
  CACHE_CTL      :bit3;
  CDBG_USER      :bit1;
  RESERVED0      :bit1;
 end;

 TSPI_SHADER_PGM_RSRC2_ES=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  OC_LDS_EN   :bit1;
  EXCP_EN     :bit9;
  RESERVED0   :bit3;
  LDS_SIZE    :bit9;
  RESERVED1   :bit3;
 end;

 TSPI_SHADER_PGM_RSRC2_GS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  EXCP_EN     :bit9;
  RESERVED0   :bit16;
 end;

 TSPI_SHADER_PGM_RSRC2_HS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  OC_LDS_EN   :bit1;
  TG_SIZE_EN  :bit1;
  EXCP_EN     :bit9;
  RESERVED0   :bit14;
 end;

 TSPI_SHADER_PGM_RSRC2_LS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  LDS_SIZE    :bit9;
  EXCP_EN     :bit9;
  RESERVED0   :bit7;
 end;

 TSPI_SHADER_PGM_RSRC2_PS=bitpacked record
  SCRATCH_EN    :bit1;
  USER_SGPR     :bit5;
  TRAP_PRESENT  :bit1;
  WAVE_CNT_EN   :bit1;
  EXTRA_LDS_SIZE:bit8;
  EXCP_EN       :bit9;
  RESERVED0     :bit7;
 end;

 TSPI_SHADER_PGM_RSRC2_VS=bitpacked record
  SCRATCH_EN      :bit1;
  USER_SGPR       :bit5;
  TRAP_PRESENT    :bit1;
  OC_LDS_EN       :bit1;
  SO_BASE0_EN     :bit1;
  SO_BASE1_EN     :bit1;
  SO_BASE2_EN     :bit1;
  SO_BASE3_EN     :bit1;
  SO_EN           :bit1;
  EXCP_EN         :bit9;
  RESERVED0       :bit2;
  DISPATCH_DRAW_EN:bit1;
  RESERVED1       :bit7;
 end;

 TSPI_SHADER_PGM_RSRC3_ES=bitpacked record
  CU_EN             :bit16;
  WAVE_LIMIT        :bit6;
  LOCK_LOW_THRESHOLD:bit4;
  GROUP_FIFO_DEPTH  :bit6;
 end;

 TSPI_SHADER_PGM_RSRC3_GS=bitpacked record
  CU_EN             :bit16;
  WAVE_LIMIT        :bit6;
  LOCK_LOW_THRESHOLD:bit4;
  GROUP_FIFO_DEPTH  :bit6;
 end;

 TSPI_SHADER_PGM_RSRC3_HS=bitpacked record
  WAVE_LIMIT        :bit6;
  LOCK_LOW_THRESHOLD:bit4;
  GROUP_FIFO_DEPTH  :bit6;
  RESERVED0         :bit16;
 end;

 TSPI_SHADER_PGM_RSRC3_LS=bitpacked record
  CU_EN             :bit16;
  WAVE_LIMIT        :bit6;
  LOCK_LOW_THRESHOLD:bit4;
  GROUP_FIFO_DEPTH  :bit6;
 end;

 TSPI_SHADER_PGM_RSRC3_PS=bitpacked record
  CU_EN             :bit16;
  WAVE_LIMIT        :bit6;
  LOCK_LOW_THRESHOLD:bit4;
  RESERVED0         :bit6;
 end;

 TSPI_SHADER_PGM_RSRC3_VS=bitpacked record
  CU_EN             :bit16;
  WAVE_LIMIT        :bit6;
  LOCK_LOW_THRESHOLD:bit4;
  RESERVED0         :bit6;
 end;

 TSPI_WF_LIFETIME_LIMIT_0=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_1=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_2=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_3=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_4=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_5=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_6=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_7=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_8=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSPI_WF_LIFETIME_LIMIT_9=bitpacked record
  MAX_CNT:bit31;
  EN_WARN:bit1;
 end;

 TSQ_PERFCOUNTER10_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER11_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER12_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER13_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER14_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_PERFCOUNTER15_SELECT=bitpacked record
  PERF_SEL       :bit9;
  RESERVED0      :bit3;
  SQC_BANK_MASK  :bit4;
  SQC_CLIENT_MASK:bit4;
  SPM_MODE       :bit4;
  SIMD_MASK      :bit4;
  PERF_MODE      :bit4;
 end;

 TSQ_THREAD_TRACE_HIWATER=bitpacked record
  HIWATER  :bit3;
  RESERVED0:bit29;
 end;

 TSX_PERFCOUNTER0_SELECT1=bitpacked record
  PERFCOUNTER_SELECT2:bit10;
  PERFCOUNTER_SELECT3:bit10;
  RESERVED0          :bit12;
 end;

 TSX_PERFCOUNTER1_SELECT1=bitpacked record
  PERFCOUNTER_SELECT2:bit10;
  PERFCOUNTER_SELECT3:bit10;
  RESERVED0          :bit12;
 end;

 TTA_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit8;
  RESERVED0 :bit2;
  PERF_SEL3 :bit8;
  RESERVED1 :bit6;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TTCA_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCA_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCA_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit4;
  PERF_MODE:bit4;
 end;

 TTCA_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit4;
  PERF_MODE:bit4;
 end;

 TTCC_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCC_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCC_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit4;
  PERF_MODE:bit4;
 end;

 TTCC_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit4;
  PERF_MODE:bit4;
 end;

 TTCP_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCP_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TTCP_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit4;
  PERF_MODE:bit4;
 end;

 TTCP_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit4;
  PERF_MODE:bit4;
 end;

 TTD_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit8;
  RESERVED0 :bit2;
  PERF_SEL3 :bit8;
  RESERVED1 :bit6;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TVGT_DISPATCH_DRAW_INDEX=bit32;

 TVGT_DMA_DATA_FIFO_DEPTH=bitpacked record
  DMA_DATA_FIFO_DEPTH:bit9;
  DMA2DRAW_FIFO_DEPTH:bit10;
  RESERVED0          :bit13;
 end;

 TVGT_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TVGT_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL  :bit10;
  PERF_SEL1 :bit10;
  CNTR_MODE :bit4;
  PERF_MODE1:bit4;
  PERF_MODE :bit4;
 end;

 TVGT_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TVGT_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit8;
  RESERVED0:bit20;
  PERF_MODE:bit4;
 end;

 TCOMPUTE_PERFCOUNT_ENABLE=bitpacked record
  PERFCOUNT_ENABLE:bit1;
  RESERVED0       :bit31;
 end;

 TCPC_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2:bit6;
  RESERVED0:bit4;
  PERF_SEL3:bit6;
  RESERVED1:bit16;
 end;

 TCPF_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2:bit6;
  RESERVED0:bit4;
  PERF_SEL3:bit6;
  RESERVED1:bit16;
 end;

 TCPG_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2:bit6;
  RESERVED0:bit4;
  PERF_SEL3:bit6;
  RESERVED1:bit16;
 end;

 TCP_CE_INTR_ROUTINE_START=bitpacked record
  IR_START :bit11;
  RESERVED0:bit21;
 end;

 TCP_CE_METADATA_BASE_ADDR=bit32;

 TCP_DISPATCH_INDR_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_HQD_CNTL_STACK_OFFSET=bitpacked record
  RESERVED0:bit2;
  OFFSET   :bit13;
  RESERVED1:bit17;
 end;

 TCP_HQD_PQ_WPTR_POLL_ADDR=bitpacked record
  RESERVED0:bit2;
  WPTR_ADDR:bit30;
 end;

 TCP_MEC1_PRGRM_CNTR_START=bitpacked record
  IP_START :bit16;
  RESERVED0:bit16;
 end;

 TCP_MEC2_PRGRM_CNTR_START=bitpacked record
  IP_START :bit16;
  RESERVED0:bit16;
 end;

 TCP_PFP_COMPLETION_STATUS=bitpacked record
  STATUS   :bit2;
  RESERVED0:bit30;
 end;

 TGDS_PERFCOUNTER0_SELECT1=bitpacked record
  PERFCOUNTER_SELECT2:bit10;
  PERFCOUNTER_SELECT3:bit10;
  RESERVED0          :bit12;
 end;

 TGRBM_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL                   :bit6;
  RESERVED0                  :bit4;
  DB_CLEAN_USER_DEFINED_MASK :bit1;
  CB_CLEAN_USER_DEFINED_MASK :bit1;
  VGT_BUSY_USER_DEFINED_MASK :bit1;
  TA_BUSY_USER_DEFINED_MASK  :bit1;
  SX_BUSY_USER_DEFINED_MASK  :bit1;
  RESERVED1                  :bit1;
  SPI_BUSY_USER_DEFINED_MASK :bit1;
  SC_BUSY_USER_DEFINED_MASK  :bit1;
  PA_BUSY_USER_DEFINED_MASK  :bit1;
  GRBM_BUSY_USER_DEFINED_MASK:bit1;
  DB_BUSY_USER_DEFINED_MASK  :bit1;
  CB_BUSY_USER_DEFINED_MASK  :bit1;
  CP_BUSY_USER_DEFINED_MASK  :bit1;
  IA_BUSY_USER_DEFINED_MASK  :bit1;
  GDS_BUSY_USER_DEFINED_MASK :bit1;
  BCI_BUSY_USER_DEFINED_MASK :bit1;
  RLC_BUSY_USER_DEFINED_MASK :bit1;
  TC_BUSY_USER_DEFINED_MASK  :bit1;
  WD_BUSY_USER_DEFINED_MASK  :bit1;
  RESERVED2                  :bit3;
 end;

 TGRBM_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL                   :bit6;
  RESERVED0                  :bit4;
  DB_CLEAN_USER_DEFINED_MASK :bit1;
  CB_CLEAN_USER_DEFINED_MASK :bit1;
  VGT_BUSY_USER_DEFINED_MASK :bit1;
  TA_BUSY_USER_DEFINED_MASK  :bit1;
  SX_BUSY_USER_DEFINED_MASK  :bit1;
  RESERVED1                  :bit1;
  SPI_BUSY_USER_DEFINED_MASK :bit1;
  SC_BUSY_USER_DEFINED_MASK  :bit1;
  PA_BUSY_USER_DEFINED_MASK  :bit1;
  GRBM_BUSY_USER_DEFINED_MASK:bit1;
  DB_BUSY_USER_DEFINED_MASK  :bit1;
  CB_BUSY_USER_DEFINED_MASK  :bit1;
  CP_BUSY_USER_DEFINED_MASK  :bit1;
  IA_BUSY_USER_DEFINED_MASK  :bit1;
  GDS_BUSY_USER_DEFINED_MASK :bit1;
  BCI_BUSY_USER_DEFINED_MASK :bit1;
  RLC_BUSY_USER_DEFINED_MASK :bit1;
  TC_BUSY_USER_DEFINED_MASK  :bit1;
  WD_BUSY_USER_DEFINED_MASK  :bit1;
  RESERVED2                  :bit3;
 end;

 TPA_SC_FORCE_EOV_MAX_CNTS=bitpacked record
  FORCE_EOV_MAX_CLK_CNT:bit16;
  FORCE_EOV_MAX_REZ_CNT:bit16;
 end;

 TPA_SC_GENERIC_SCISSOR_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_GENERIC_SCISSOR_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_HP3D_TRAP_SCREEN_H=bitpacked record
  X_COORD  :bit14;
  RESERVED0:bit18;
 end;

 TPA_SC_HP3D_TRAP_SCREEN_V=bitpacked record
  Y_COORD  :bit14;
  RESERVED0:bit18;
 end;

 TPA_SC_LINE_STIPPLE_STATE=bitpacked record
  CURRENT_PTR  :bit4;
  RESERVED0    :bit4;
  CURRENT_COUNT:bit8;
  RESERVED1    :bit16;
 end;

 TPA_SC_VPORT_SCISSOR_0_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_0_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_1_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_1_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_2_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_2_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_3_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_3_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_4_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_4_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_5_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_5_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_6_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_6_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_7_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_7_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_8_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_8_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_9_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_9_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SU_LINE_STIPPLE_SCALE=bit32;

 TPA_SU_LINE_STIPPLE_VALUE=bitpacked record
  LINE_STIPPLE_VALUE:bit24;
  RESERVED0         :bit8;
 end;

 TSPI_CSQ_WF_ACTIVE_STATUS=bit32;

 TSPI_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TSPI_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TSPI_PERFCOUNTER2_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TSPI_PERFCOUNTER3_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TSPI_SHADER_LATE_ALLOC_VS=bitpacked record
  LIMIT    :bit6;
  RESERVED0:bit26;
 end;

 TSPI_WCL_PIPE_PERCENT_CS0=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS1=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS2=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS3=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS4=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS5=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS6=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_CS7=bitpacked record
  VALUE    :bit7;
  RESERVED0:bit25;
 end;

 TSPI_WCL_PIPE_PERCENT_GFX=bitpacked record
  VALUE       :bit7;
  LS_GRP_VALUE:bit5;
  HS_GRP_VALUE:bit5;
  ES_GRP_VALUE:bit5;
  GS_GRP_VALUE:bit5;
  RESERVED0   :bit5;
 end;

 TSPI_WF_LIFETIME_STATUS_0=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_1=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_2=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_3=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_4=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_5=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_6=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_7=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_8=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_9=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TTCA_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE2:bit4;
  PERF_MODE3:bit4;
 end;

 TTCA_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE2:bit4;
  PERF_MODE3:bit4;
 end;

 TTCC_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE2:bit4;
  PERF_MODE3:bit4;
 end;

 TTCC_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE2:bit4;
  PERF_MODE3:bit4;
 end;

 TTCP_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TTCP_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TVGT_DRAW_INIT_FIFO_DEPTH=bitpacked record
  DRAW_INIT_FIFO_DEPTH:bit6;
  RESERVED0           :bit26;
 end;

 TVGT_INSTANCE_STEP_RATE_0=bit32;

 TVGT_INSTANCE_STEP_RATE_1=bit32;

 TVGT_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TVGT_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2 :bit10;
  PERF_SEL3 :bit10;
  RESERVED0 :bit4;
  PERF_MODE3:bit4;
  PERF_MODE2:bit4;
 end;

 TVGT_STRMOUT_VTX_STRIDE_0=bitpacked record
  STRIDE   :bit10;
  RESERVED0:bit22;
 end;

 TVGT_STRMOUT_VTX_STRIDE_1=bitpacked record
  STRIDE   :bit10;
  RESERVED0:bit22;
 end;

 TVGT_STRMOUT_VTX_STRIDE_2=bitpacked record
  STRIDE   :bit10;
  RESERVED0:bit22;
 end;

 TVGT_STRMOUT_VTX_STRIDE_3=bitpacked record
  STRIDE   :bit10;
  RESERVED0:bit22;
 end;

 TCP_DRAW_INDX_INDR_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_HQD_GDS_RESOURCE_STATE=bitpacked record
  OA_REQUIRED:bit1;
  OA_ACQUIRED:bit1;
  RESERVED0  :bit2;
  GWS_SIZE   :bit6;
  RESERVED1  :bit2;
  GWS_PNTR   :bit6;
  RESERVED2  :bit14;
 end;

 TCP_ME0_PIPE_PRIORITY_CNTS=bitpacked record
  PRIORITY1_CNT :bit8;
  PRIORITY2A_CNT:bit8;
  PRIORITY2B_CNT:bit8;
  PRIORITY3_CNT :bit8;
 end;

 TCP_ME1_PIPE_PRIORITY_CNTS=bitpacked record
  PRIORITY1_CNT :bit8;
  PRIORITY2A_CNT:bit8;
  PRIORITY2B_CNT:bit8;
  PRIORITY3_CNT :bit8;
 end;

 TCP_ME2_PIPE_PRIORITY_CNTS=bitpacked record
  PRIORITY1_CNT :bit8;
  PRIORITY2A_CNT:bit8;
  PRIORITY2B_CNT:bit8;
  PRIORITY3_CNT :bit8;
 end;

 TCP_PFP_INTR_ROUTINE_START=bitpacked record
  IR_START :bit12;
  RESERVED0:bit20;
 end;

 TCP_PFP_METADATA_BASE_ADDR=bit32;

 TPA_SC_CENTROID_PRIORITY_0=bitpacked record
  DISTANCE_0:bit4;
  DISTANCE_1:bit4;
  DISTANCE_2:bit4;
  DISTANCE_3:bit4;
  DISTANCE_4:bit4;
  DISTANCE_5:bit4;
  DISTANCE_6:bit4;
  DISTANCE_7:bit4;
 end;

 TPA_SC_CENTROID_PRIORITY_1=bitpacked record
  DISTANCE_8 :bit4;
  DISTANCE_9 :bit4;
  DISTANCE_10:bit4;
  DISTANCE_11:bit4;
  DISTANCE_12:bit4;
  DISTANCE_13:bit4;
  DISTANCE_14:bit4;
  DISTANCE_15:bit4;
 end;

 TPA_SC_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TPA_SC_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER4_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER5_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER6_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_PERFCOUNTER7_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit22;
 end;

 TPA_SC_TRAP_SCREEN_HV_LOCK=bitpacked record
  DISABLE_NON_PRIV_WRITES:bit1;
  RESERVED0              :bit31;
 end;

 TPA_SC_VPORT_SCISSOR_10_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_10_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_11_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_11_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_12_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_12_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_13_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_13_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_14_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_14_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_15_BR=bitpacked record
  BR_X     :bit15;
  RESERVED0:bit1;
  BR_Y     :bit15;
  RESERVED1:bit1;
 end;

 TPA_SC_VPORT_SCISSOR_15_TL=bitpacked record
  TL_X                 :bit15;
  RESERVED0            :bit1;
  TL_Y                 :bit15;
  WINDOW_OFFSET_DISABLE:bit1;
 end;

 TPA_SU_PERFCOUNTER0_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TPA_SU_PERFCOUNTER1_SELECT=bitpacked record
  PERF_SEL :bit10;
  PERF_SEL1:bit10;
  CNTR_MODE:bit4;
  RESERVED0:bit8;
 end;

 TPA_SU_PERFCOUNTER2_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit8;
 end;

 TPA_SU_PERFCOUNTER3_SELECT=bitpacked record
  PERF_SEL :bit10;
  RESERVED0:bit10;
  CNTR_MODE:bit4;
  RESERVED1:bit8;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_0=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_1=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_2=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_3=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_4=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_5=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_6=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_CSQ_WF_ACTIVE_COUNT_7=bitpacked record
  COUNT    :bit11;
  RESERVED0:bit21;
 end;

 TSPI_RESOURCE_RESERVE_CU_0=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_1=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_2=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_3=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_4=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_5=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_6=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_7=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_8=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_9=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_SHADER_USER_DATA_ES_0=bit32;

 TSPI_SHADER_USER_DATA_ES_1=bit32;

 TSPI_SHADER_USER_DATA_ES_2=bit32;

 TSPI_SHADER_USER_DATA_ES_3=bit32;

 TSPI_SHADER_USER_DATA_ES_4=bit32;

 TSPI_SHADER_USER_DATA_ES_5=bit32;

 TSPI_SHADER_USER_DATA_ES_6=bit32;

 TSPI_SHADER_USER_DATA_ES_7=bit32;

 TSPI_SHADER_USER_DATA_ES_8=bit32;

 TSPI_SHADER_USER_DATA_ES_9=bit32;

 TSPI_SHADER_USER_DATA_GS_0=bit32;

 TSPI_SHADER_USER_DATA_GS_1=bit32;

 TSPI_SHADER_USER_DATA_GS_2=bit32;

 TSPI_SHADER_USER_DATA_GS_3=bit32;

 TSPI_SHADER_USER_DATA_GS_4=bit32;

 TSPI_SHADER_USER_DATA_GS_5=bit32;

 TSPI_SHADER_USER_DATA_GS_6=bit32;

 TSPI_SHADER_USER_DATA_GS_7=bit32;

 TSPI_SHADER_USER_DATA_GS_8=bit32;

 TSPI_SHADER_USER_DATA_GS_9=bit32;

 TSPI_SHADER_USER_DATA_HS_0=bit32;

 TSPI_SHADER_USER_DATA_HS_1=bit32;

 TSPI_SHADER_USER_DATA_HS_2=bit32;

 TSPI_SHADER_USER_DATA_HS_3=bit32;

 TSPI_SHADER_USER_DATA_HS_4=bit32;

 TSPI_SHADER_USER_DATA_HS_5=bit32;

 TSPI_SHADER_USER_DATA_HS_6=bit32;

 TSPI_SHADER_USER_DATA_HS_7=bit32;

 TSPI_SHADER_USER_DATA_HS_8=bit32;

 TSPI_SHADER_USER_DATA_HS_9=bit32;

 TSPI_SHADER_USER_DATA_LS_0=bit32;

 TSPI_SHADER_USER_DATA_LS_1=bit32;

 TSPI_SHADER_USER_DATA_LS_2=bit32;

 TSPI_SHADER_USER_DATA_LS_3=bit32;

 TSPI_SHADER_USER_DATA_LS_4=bit32;

 TSPI_SHADER_USER_DATA_LS_5=bit32;

 TSPI_SHADER_USER_DATA_LS_6=bit32;

 TSPI_SHADER_USER_DATA_LS_7=bit32;

 TSPI_SHADER_USER_DATA_LS_8=bit32;

 TSPI_SHADER_USER_DATA_LS_9=bit32;

 TSPI_SHADER_USER_DATA_PS_0=bit32;

 TSPI_SHADER_USER_DATA_PS_1=bit32;

 TSPI_SHADER_USER_DATA_PS_2=bit32;

 TSPI_SHADER_USER_DATA_PS_3=bit32;

 TSPI_SHADER_USER_DATA_PS_4=bit32;

 TSPI_SHADER_USER_DATA_PS_5=bit32;

 TSPI_SHADER_USER_DATA_PS_6=bit32;

 TSPI_SHADER_USER_DATA_PS_7=bit32;

 TSPI_SHADER_USER_DATA_PS_8=bit32;

 TSPI_SHADER_USER_DATA_PS_9=bit32;

 TSPI_SHADER_USER_DATA_VS_0=bit32;

 TSPI_SHADER_USER_DATA_VS_1=bit32;

 TSPI_SHADER_USER_DATA_VS_2=bit32;

 TSPI_SHADER_USER_DATA_VS_3=bit32;

 TSPI_SHADER_USER_DATA_VS_4=bit32;

 TSPI_SHADER_USER_DATA_VS_5=bit32;

 TSPI_SHADER_USER_DATA_VS_6=bit32;

 TSPI_SHADER_USER_DATA_VS_7=bit32;

 TSPI_SHADER_USER_DATA_VS_8=bit32;

 TSPI_SHADER_USER_DATA_VS_9=bit32;

 TSPI_WCL_PIPE_PERCENT_HP3D=bitpacked record
  VALUE       :bit7;
  LS_GRP_VALUE:bit5;
  HS_GRP_VALUE:bit5;
  ES_GRP_VALUE:bit5;
  GS_GRP_VALUE:bit5;
  RESERVED0   :bit5;
 end;

 TSPI_WF_LIFETIME_STATUS_10=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_11=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_12=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_13=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_14=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_15=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_16=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_17=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_18=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_19=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSPI_WF_LIFETIME_STATUS_20=bitpacked record
  MAX_CNT :bit31;
  INT_SENT:bit1;
 end;

 TSQ_THREAD_TRACE_PERF_MASK=bitpacked record
  SH0_MASK:bit16;
  SH1_MASK:bit16;
 end;

 TTCP_BUFFER_ADDR_HASH_CNTL=bitpacked record
  CHANNEL_BITS     :bit3;
  RESERVED0        :bit5;
  BANK_BITS        :bit3;
  RESERVED1        :bit5;
  CHANNEL_XOR_COUNT:bit3;
  RESERVED2        :bit5;
  BANK_XOR_COUNT   :bit3;
  RESERVED3        :bit5;
 end;

 TVGT_GROUP_VECT_0_FMT_CNTL=bitpacked record
  X_CONV  :bit4;
  X_OFFSET:bit4;
  Y_CONV  :bit4;
  Y_OFFSET:bit4;
  Z_CONV  :bit4;
  Z_OFFSET:bit4;
  W_CONV  :bit4;
  W_OFFSET:bit4;
 end;

 TVGT_GROUP_VECT_1_FMT_CNTL=bitpacked record
  X_CONV  :bit4;
  X_OFFSET:bit4;
  Y_CONV  :bit4;
  Y_OFFSET:bit4;
  Z_CONV  :bit4;
  Z_OFFSET:bit4;
  W_CONV  :bit4;
  W_OFFSET:bit4;
 end;

 TVGT_PERFCOUNTER_SEID_MASK=bitpacked record
  PERF_SEID_IGNORE_MASK:bit8;
  RESERVED0            :bit24;
 end;

 TVGT_STRMOUT_BUFFER_CONFIG=bitpacked record
  STREAM_0_BUFFER_EN:bit4;
  STREAM_1_BUFFER_EN:bit4;
  STREAM_2_BUFFER_EN:bit4;
  STREAM_3_BUFFER_EN:bit4;
  RESERVED0         :bit16;
 end;

 TVGT_STRMOUT_BUFFER_SIZE_0=bit32;

 TVGT_STRMOUT_BUFFER_SIZE_1=bit32;

 TVGT_STRMOUT_BUFFER_SIZE_2=bit32;

 TVGT_STRMOUT_BUFFER_SIZE_3=bit32;

 TCOMPUTE_DISPATCH_INITIATOR=bitpacked record
  COMPUTE_SHADER_EN    :bit1;
  PARTIAL_TG_EN        :bit1;
  FORCE_START_AT_000   :bit1;
  ORDERED_APPEND_ENBL  :bit1;
  ORDERED_APPEND_MODE  :bit1;
  USE_THREAD_DIMENSIONS:bit1;
  ORDER_MODE           :bit1;
  DISPATCH_CACHE_CNTL  :bit3;
  SCALAR_L1_INV_VOL    :bit1;
  VECTOR_L1_INV_VOL    :bit1;
  DATA_ATC             :bit1;
  RESERVED0            :bit1;
  RESTORE              :bit1;
  RESERVED1            :bit17;
 end;

 TCP_HQD_PQ_DOORBELL_CONTROL=bitpacked record
  DOORBELL_MODE      :bit1;
  DOORBELL_BIF_DROP  :bit1;
  DOORBELL_OFFSET    :bit21;
  DOORBELL_CARRY_BITS:bit3;
  RESERVED0          :bit2;
  DOORBELL_SOURCE    :bit1;
  DOORBELL_SCHD_HIT  :bit1;
  DOORBELL_EN        :bit1;
  DOORBELL_HIT       :bit1;
 end;

 TCP_HQD_PQ_RPTR_REPORT_ADDR=bitpacked record
  RESERVED0       :bit2;
  RPTR_REPORT_ADDR:bit30;
 end;

 TCP_MEC1_INTR_ROUTINE_START=bitpacked record
  IR_START :bit16;
  RESERVED0:bit16;
 end;

 TCP_MEC2_INTR_ROUTINE_START=bitpacked record
  IR_START :bit16;
  RESERVED0:bit16;
 end;

 TDB_SRESULTS_COMPARE_STATE0=bitpacked record
  COMPAREFUNC0 :bit3;
  RESERVED0    :bit1;
  COMPAREVALUE0:bit8;
  COMPAREMASK0 :bit8;
  RESERVED1    :bit4;
  ENABLE0      :bit1;
  RESERVED2    :bit7;
 end;

 TDB_SRESULTS_COMPARE_STATE1=bitpacked record
  COMPAREFUNC1 :bit3;
  RESERVED0    :bit1;
  COMPAREVALUE1:bit8;
  COMPAREMASK1 :bit8;
  RESERVED1    :bit4;
  ENABLE1      :bit1;
  RESERVED2    :bit7;
 end;

 TPA_SC_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TPA_SU_PERFCOUNTER0_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TPA_SU_PERFCOUNTER1_SELECT1=bitpacked record
  PERF_SEL2:bit10;
  PERF_SEL3:bit10;
  RESERVED0:bit12;
 end;

 TSPI_P0_TRAP_SCREEN_GPR_MIN=bitpacked record
  VGPR_MIN :bit6;
  SGPR_MIN :bit4;
  RESERVED0:bit22;
 end;

 TSPI_P0_TRAP_SCREEN_PSBA_HI=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_P0_TRAP_SCREEN_PSBA_LO=bit32;

 TSPI_P0_TRAP_SCREEN_PSMA_HI=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_P0_TRAP_SCREEN_PSMA_LO=bit32;

 TSPI_P1_TRAP_SCREEN_GPR_MIN=bitpacked record
  VGPR_MIN :bit6;
  SGPR_MIN :bit4;
  RESERVED0:bit22;
 end;

 TSPI_P1_TRAP_SCREEN_PSBA_HI=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_P1_TRAP_SCREEN_PSBA_LO=bit32;

 TSPI_P1_TRAP_SCREEN_PSMA_HI=bitpacked record
  MEM_BASE :bit8;
  RESERVED0:bit24;
 end;

 TSPI_P1_TRAP_SCREEN_PSMA_LO=bit32;

 TSPI_RESOURCE_RESERVE_CU_10=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_11=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_12=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_13=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_14=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_RESOURCE_RESERVE_CU_15=bitpacked record
  VGPR     :bit4;
  SGPR     :bit4;
  LDS      :bit4;
  WAVES    :bit3;
  BARRIERS :bit4;
  RESERVED0:bit13;
 end;

 TSPI_SHADER_PGM_RSRC2_ES_GS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  OC_LDS_EN   :bit1;
  EXCP_EN     :bit9;
  RESERVED0   :bit3;
  LDS_SIZE    :bit9;
  RESERVED1   :bit3;
 end;

 TSPI_SHADER_PGM_RSRC2_ES_VS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  OC_LDS_EN   :bit1;
  EXCP_EN     :bit9;
  RESERVED0   :bit3;
  LDS_SIZE    :bit9;
  RESERVED1   :bit3;
 end;

 TSPI_SHADER_PGM_RSRC2_LS_ES=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  LDS_SIZE    :bit9;
  EXCP_EN     :bit9;
  RESERVED0   :bit7;
 end;

 TSPI_SHADER_PGM_RSRC2_LS_HS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  LDS_SIZE    :bit9;
  EXCP_EN     :bit9;
  RESERVED0   :bit7;
 end;

 TSPI_SHADER_PGM_RSRC2_LS_VS=bitpacked record
  SCRATCH_EN  :bit1;
  USER_SGPR   :bit5;
  TRAP_PRESENT:bit1;
  LDS_SIZE    :bit9;
  EXCP_EN     :bit9;
  RESERVED0   :bit7;
 end;

 TSPI_SHADER_USER_DATA_ES_10=bit32;

 TSPI_SHADER_USER_DATA_ES_11=bit32;

 TSPI_SHADER_USER_DATA_ES_12=bit32;

 TSPI_SHADER_USER_DATA_ES_13=bit32;

 TSPI_SHADER_USER_DATA_ES_14=bit32;

 TSPI_SHADER_USER_DATA_ES_15=bit32;

 TSPI_SHADER_USER_DATA_GS_10=bit32;

 TSPI_SHADER_USER_DATA_GS_11=bit32;

 TSPI_SHADER_USER_DATA_GS_12=bit32;

 TSPI_SHADER_USER_DATA_GS_13=bit32;

 TSPI_SHADER_USER_DATA_GS_14=bit32;

 TSPI_SHADER_USER_DATA_GS_15=bit32;

 TSPI_SHADER_USER_DATA_HS_10=bit32;

 TSPI_SHADER_USER_DATA_HS_11=bit32;

 TSPI_SHADER_USER_DATA_HS_12=bit32;

 TSPI_SHADER_USER_DATA_HS_13=bit32;

 TSPI_SHADER_USER_DATA_HS_14=bit32;

 TSPI_SHADER_USER_DATA_HS_15=bit32;

 TSPI_SHADER_USER_DATA_LS_10=bit32;

 TSPI_SHADER_USER_DATA_LS_11=bit32;

 TSPI_SHADER_USER_DATA_LS_12=bit32;

 TSPI_SHADER_USER_DATA_LS_13=bit32;

 TSPI_SHADER_USER_DATA_LS_14=bit32;

 TSPI_SHADER_USER_DATA_LS_15=bit32;

 TSPI_SHADER_USER_DATA_PS_10=bit32;

 TSPI_SHADER_USER_DATA_PS_11=bit32;

 TSPI_SHADER_USER_DATA_PS_12=bit32;

 TSPI_SHADER_USER_DATA_PS_13=bit32;

 TSPI_SHADER_USER_DATA_PS_14=bit32;

 TSPI_SHADER_USER_DATA_PS_15=bit32;

 TSPI_SHADER_USER_DATA_VS_10=bit32;

 TSPI_SHADER_USER_DATA_VS_11=bit32;

 TSPI_SHADER_USER_DATA_VS_12=bit32;

 TSPI_SHADER_USER_DATA_VS_13=bit32;

 TSPI_SHADER_USER_DATA_VS_14=bit32;

 TSPI_SHADER_USER_DATA_VS_15=bit32;

 TSPI_SX_EXPORT_BUFFER_SIZES=bitpacked record
  COLOR_BUFFER_SIZE   :bit16;
  POSITION_BUFFER_SIZE:bit16;
 end;

 TSQ_THREAD_TRACE_TOKEN_MASK=bitpacked record
  TOKEN_MASK       :bit16;
  REG_MASK         :bit8;
  REG_DROP_ON_STALL:bit1;
  RESERVED0        :bit7;
 end;

 TSQ_THREAD_TRACE_USERDATA_0=bit32;

 TSQ_THREAD_TRACE_USERDATA_1=bit32;

 TSQ_THREAD_TRACE_USERDATA_2=bit32;

 TSQ_THREAD_TRACE_USERDATA_3=bit32;

 TVGT_MULTI_PRIM_IB_RESET_EN=bitpacked record
  RESET_EN :bit1;
  RESERVED0:bit31;
 end;

 TCOMPUTE_PIPELINESTAT_ENABLE=bitpacked record
  PIPELINESTAT_ENABLE:bit1;
  RESERVED0          :bit31;
 end;

 TCOMPUTE_THREAD_TRACE_ENABLE=bitpacked record
  THREAD_TRACE_ENABLE:bit1;
  RESERVED0          :bit31;
 end;

 TCP_CE_METADATA_BASE_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_HQD_PQ_WPTR_POLL_ADDR_HI=bitpacked record
  WPTR_ADDR_HI:bit16;
  RESERVED0   :bit16;
 end;

 TCP_MEC_DOORBELL_RANGE_LOWER=bitpacked record
  RESERVED0           :bit2;
  DOORBELL_RANGE_LOWER:bit21;
  RESERVED1           :bit9;
 end;

 TCP_MEC_DOORBELL_RANGE_UPPER=bitpacked record
  RESERVED0           :bit2;
  DOORBELL_RANGE_UPPER:bit21;
  RESERVED1           :bit9;
 end;

 TCP_PFP_GDS_ATOMIC0_PREOP_HI=bit32;

 TCP_PFP_GDS_ATOMIC0_PREOP_LO=bit32;

 TCP_PFP_GDS_ATOMIC1_PREOP_HI=bit32;

 TCP_PFP_GDS_ATOMIC1_PREOP_LO=bit32;

 TGRBM_SE0_PERFCOUNTER_SELECT=bitpacked record
  PERF_SEL                  :bit6;
  RESERVED0                 :bit4;
  DB_CLEAN_USER_DEFINED_MASK:bit1;
  CB_CLEAN_USER_DEFINED_MASK:bit1;
  TA_BUSY_USER_DEFINED_MASK :bit1;
  SX_BUSY_USER_DEFINED_MASK :bit1;
  RESERVED1                 :bit1;
  SPI_BUSY_USER_DEFINED_MASK:bit1;
  SC_BUSY_USER_DEFINED_MASK :bit1;
  DB_BUSY_USER_DEFINED_MASK :bit1;
  CB_BUSY_USER_DEFINED_MASK :bit1;
  VGT_BUSY_USER_DEFINED_MASK:bit1;
  PA_BUSY_USER_DEFINED_MASK :bit1;
  BCI_BUSY_USER_DEFINED_MASK:bit1;
  RESERVED2                 :bit10;
 end;

 TGRBM_SE1_PERFCOUNTER_SELECT=bitpacked record
  PERF_SEL                  :bit6;
  RESERVED0                 :bit4;
  DB_CLEAN_USER_DEFINED_MASK:bit1;
  CB_CLEAN_USER_DEFINED_MASK:bit1;
  TA_BUSY_USER_DEFINED_MASK :bit1;
  SX_BUSY_USER_DEFINED_MASK :bit1;
  RESERVED1                 :bit1;
  SPI_BUSY_USER_DEFINED_MASK:bit1;
  SC_BUSY_USER_DEFINED_MASK :bit1;
  DB_BUSY_USER_DEFINED_MASK :bit1;
  CB_BUSY_USER_DEFINED_MASK :bit1;
  VGT_BUSY_USER_DEFINED_MASK:bit1;
  PA_BUSY_USER_DEFINED_MASK :bit1;
  BCI_BUSY_USER_DEFINED_MASK:bit1;
  RESERVED2                 :bit10;
 end;

 TGRBM_SE2_PERFCOUNTER_SELECT=bitpacked record
  PERF_SEL                  :bit6;
  RESERVED0                 :bit4;
  DB_CLEAN_USER_DEFINED_MASK:bit1;
  CB_CLEAN_USER_DEFINED_MASK:bit1;
  TA_BUSY_USER_DEFINED_MASK :bit1;
  SX_BUSY_USER_DEFINED_MASK :bit1;
  RESERVED1                 :bit1;
  SPI_BUSY_USER_DEFINED_MASK:bit1;
  SC_BUSY_USER_DEFINED_MASK :bit1;
  DB_BUSY_USER_DEFINED_MASK :bit1;
  CB_BUSY_USER_DEFINED_MASK :bit1;
  VGT_BUSY_USER_DEFINED_MASK:bit1;
  PA_BUSY_USER_DEFINED_MASK :bit1;
  BCI_BUSY_USER_DEFINED_MASK:bit1;
  RESERVED2                 :bit10;
 end;

 TGRBM_SE3_PERFCOUNTER_SELECT=bitpacked record
  PERF_SEL                  :bit6;
  RESERVED0                 :bit4;
  DB_CLEAN_USER_DEFINED_MASK:bit1;
  CB_CLEAN_USER_DEFINED_MASK:bit1;
  TA_BUSY_USER_DEFINED_MASK :bit1;
  SX_BUSY_USER_DEFINED_MASK :bit1;
  RESERVED1                 :bit1;
  SPI_BUSY_USER_DEFINED_MASK:bit1;
  SC_BUSY_USER_DEFINED_MASK :bit1;
  DB_BUSY_USER_DEFINED_MASK :bit1;
  CB_BUSY_USER_DEFINED_MASK :bit1;
  VGT_BUSY_USER_DEFINED_MASK:bit1;
  PA_BUSY_USER_DEFINED_MASK :bit1;
  BCI_BUSY_USER_DEFINED_MASK:bit1;
  RESERVED2                 :bit10;
 end;

 TPA_SC_P3D_TRAP_SCREEN_COUNT=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TPA_SC_P3D_TRAP_SCREEN_HV_EN=bitpacked record
  ENABLE_HV_PRE_SHADER       :bit1;
  FORCE_PRE_SHADER_ALL_PIXELS:bit1;
  RESERVED0                  :bit30;
 end;

 TSQ_THREAD_TRACE_TOKEN_MASK2=bit32;

 TVGT_STRMOUT_BUFFER_OFFSET_0=bit32;

 TVGT_STRMOUT_BUFFER_OFFSET_1=bit32;

 TVGT_STRMOUT_BUFFER_OFFSET_2=bit32;

 TVGT_STRMOUT_BUFFER_OFFSET_3=bit32;

 TVGT_VERTEX_REUSE_BLOCK_CNTL=bitpacked record
  VTX_REUSE_DEPTH:bit8;
  RESERVED0      :bit24;
 end;

 TCOMPUTE_WAVE_RESTORE_ADDR_HI=bitpacked record
  ADDR     :bit16;
  RESERVED0:bit16;
 end;

 TCOMPUTE_WAVE_RESTORE_ADDR_LO=bit32;

 TCOMPUTE_WAVE_RESTORE_CONTROL=bitpacked record
  ATC      :bit1;
  MTYPE    :bit2;
  RESERVED0:bit29;
 end;

 TCP_ECC_FIRSTOCCURRENCE_RING0=bit32;

 TCP_ECC_FIRSTOCCURRENCE_RING1=bit32;

 TCP_ECC_FIRSTOCCURRENCE_RING2=bit32;

 TCP_HQD_CTX_SAVE_BASE_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TCP_HQD_CTX_SAVE_BASE_ADDR_LO=bitpacked record
  RESERVED0:bit12;
  ADDR     :bit20;
 end;

 TCP_NUM_PRIM_NEEDED_COUNT0_HI=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT0_LO=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT1_HI=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT1_LO=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT2_HI=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT2_LO=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT3_HI=bit32;

 TCP_NUM_PRIM_NEEDED_COUNT3_LO=bit32;

 TCP_PFP_METADATA_BASE_ADDR_HI=bitpacked record
  ADDR_HI  :bit16;
  RESERVED0:bit16;
 end;

 TPA_SC_HP3D_TRAP_SCREEN_COUNT=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TPA_SC_HP3D_TRAP_SCREEN_HV_EN=bitpacked record
  ENABLE_HV_PRE_SHADER       :bit1;
  FORCE_PRE_SHADER_ALL_PIXELS:bit1;
  RESERVED0                  :bit30;
 end;

 TPA_SC_TRAP_SCREEN_OCCURRENCE=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TPA_SU_HARDWARE_SCREEN_OFFSET=bitpacked record
  HW_SCREEN_OFFSET_X:bit9;
  RESERVED0         :bit7;
  HW_SCREEN_OFFSET_Y:bit9;
  RESERVED1         :bit7;
 end;

 TPA_SU_POLY_OFFSET_BACK_SCALE=bit32;

 TSPI_PG_ENABLE_STATIC_CU_MASK=bitpacked record
  CU_MASK  :bit16;
  RESERVED0:bit16;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_0=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_1=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_2=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_3=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_4=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_5=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_6=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_7=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_8=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_9=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TVGT_MULTI_PRIM_IB_RESET_INDX=bit32;

 TCP_HQD_PQ_RPTR_REPORT_ADDR_HI=bitpacked record
  RPTR_REPORT_ADDR_HI:bit16;
  RESERVED0          :bit16;
 end;

 TCP_NUM_PRIM_WRITTEN_COUNT0_HI=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT0_LO=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT1_HI=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT1_LO=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT2_HI=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT2_LO=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT3_HI=bit32;

 TCP_NUM_PRIM_WRITTEN_COUNT3_LO=bit32;

 TPA_SC_P3D_TRAP_SCREEN_HV_LOCK=bitpacked record
  DISABLE_NON_PRIV_WRITES:bit1;
  RESERVED0              :bit31;
 end;

 TPA_SU_POLY_OFFSET_BACK_OFFSET=bit32;

 TPA_SU_POLY_OFFSET_DB_FMT_CNTL=bitpacked record
  POLY_OFFSET_NEG_NUM_DB_BITS:bit8;
  POLY_OFFSET_DB_IS_FLOAT_FMT:bit1;
  RESERVED0                  :bit23;
 end;

 TPA_SU_POLY_OFFSET_FRONT_SCALE=bit32;

 TSPI_RESOURCE_RESERVE_EN_CU_10=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_11=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_12=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_13=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_14=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TSPI_RESOURCE_RESERVE_EN_CU_15=bitpacked record
  EN                :bit1;
  TYPE_MASK         :bit15;
  QUEUE_MASK        :bit8;
  RESERVE_SPACE_ONLY:bit1;
  RESERVED0         :bit7;
 end;

 TCOMPUTE_STATIC_THREAD_MGMT_SE0=bitpacked record
  SH0_CU_EN:bit16;
  SH1_CU_EN:bit16;
 end;

 TCOMPUTE_STATIC_THREAD_MGMT_SE1=bitpacked record
  SH0_CU_EN:bit16;
  SH1_CU_EN:bit16;
 end;

 TCOMPUTE_STATIC_THREAD_MGMT_SE2=bitpacked record
  SH0_CU_EN:bit16;
  SH1_CU_EN:bit16;
 end;

 TCOMPUTE_STATIC_THREAD_MGMT_SE3=bitpacked record
  SH0_CU_EN:bit16;
  SH1_CU_EN:bit16;
 end;

 TPA_SC_HP3D_TRAP_SCREEN_HV_LOCK=bitpacked record
  DISABLE_NON_PRIV_WRITES:bit1;
  RESERVED0              :bit31;
 end;

 TPA_SU_POLY_OFFSET_FRONT_OFFSET=bit32;

 TSPI_SX_SCOREBOARD_BUFFER_SIZES=bitpacked record
  COLOR_SCOREBOARD_SIZE   :bit16;
  POSITION_SCOREBOARD_SIZE:bit16;
 end;

 TVGT_STRMOUT_DRAW_OPAQUE_OFFSET=bit32;

 TPA_SC_P3D_TRAP_SCREEN_OCCURRENCE=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TVGT_STRMOUT_BUFFER_FILLED_SIZE_0=bit32;

 TVGT_STRMOUT_BUFFER_FILLED_SIZE_1=bit32;

 TVGT_STRMOUT_BUFFER_FILLED_SIZE_2=bit32;

 TVGT_STRMOUT_BUFFER_FILLED_SIZE_3=bit32;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_0=bitpacked record
  S0_X:bit4;
  S0_Y:bit4;
  S1_X:bit4;
  S1_Y:bit4;
  S2_X:bit4;
  S2_Y:bit4;
  S3_X:bit4;
  S3_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_1=bitpacked record
  S4_X:bit4;
  S4_Y:bit4;
  S5_X:bit4;
  S5_Y:bit4;
  S6_X:bit4;
  S6_Y:bit4;
  S7_X:bit4;
  S7_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_2=bitpacked record
  S8_X :bit4;
  S8_Y :bit4;
  S9_X :bit4;
  S9_Y :bit4;
  S10_X:bit4;
  S10_Y:bit4;
  S11_X:bit4;
  S11_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y0_3=bitpacked record
  S12_X:bit4;
  S12_Y:bit4;
  S13_X:bit4;
  S13_Y:bit4;
  S14_X:bit4;
  S14_Y:bit4;
  S15_X:bit4;
  S15_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_0=bitpacked record
  S0_X:bit4;
  S0_Y:bit4;
  S1_X:bit4;
  S1_Y:bit4;
  S2_X:bit4;
  S2_Y:bit4;
  S3_X:bit4;
  S3_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_1=bitpacked record
  S4_X:bit4;
  S4_Y:bit4;
  S5_X:bit4;
  S5_Y:bit4;
  S6_X:bit4;
  S6_Y:bit4;
  S7_X:bit4;
  S7_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_2=bitpacked record
  S8_X :bit4;
  S8_Y :bit4;
  S9_X :bit4;
  S9_Y :bit4;
  S10_X:bit4;
  S10_Y:bit4;
  S11_X:bit4;
  S11_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X0Y1_3=bitpacked record
  S12_X:bit4;
  S12_Y:bit4;
  S13_X:bit4;
  S13_Y:bit4;
  S14_X:bit4;
  S14_Y:bit4;
  S15_X:bit4;
  S15_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_0=bitpacked record
  S0_X:bit4;
  S0_Y:bit4;
  S1_X:bit4;
  S1_Y:bit4;
  S2_X:bit4;
  S2_Y:bit4;
  S3_X:bit4;
  S3_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_1=bitpacked record
  S4_X:bit4;
  S4_Y:bit4;
  S5_X:bit4;
  S5_Y:bit4;
  S6_X:bit4;
  S6_Y:bit4;
  S7_X:bit4;
  S7_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_2=bitpacked record
  S8_X :bit4;
  S8_Y :bit4;
  S9_X :bit4;
  S9_Y :bit4;
  S10_X:bit4;
  S10_Y:bit4;
  S11_X:bit4;
  S11_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y0_3=bitpacked record
  S12_X:bit4;
  S12_Y:bit4;
  S13_X:bit4;
  S13_Y:bit4;
  S14_X:bit4;
  S14_Y:bit4;
  S15_X:bit4;
  S15_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_0=bitpacked record
  S0_X:bit4;
  S0_Y:bit4;
  S1_X:bit4;
  S1_Y:bit4;
  S2_X:bit4;
  S2_Y:bit4;
  S3_X:bit4;
  S3_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_1=bitpacked record
  S4_X:bit4;
  S4_Y:bit4;
  S5_X:bit4;
  S5_Y:bit4;
  S6_X:bit4;
  S6_Y:bit4;
  S7_X:bit4;
  S7_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_2=bitpacked record
  S8_X :bit4;
  S8_Y :bit4;
  S9_X :bit4;
  S9_Y :bit4;
  S10_X:bit4;
  S10_Y:bit4;
  S11_X:bit4;
  S11_Y:bit4;
 end;

 TPA_SC_AA_SAMPLE_LOCS_PIXEL_X1Y1_3=bitpacked record
  S12_X:bit4;
  S12_Y:bit4;
  S13_X:bit4;
  S13_Y:bit4;
  S14_X:bit4;
  S14_Y:bit4;
  S15_X:bit4;
  S15_Y:bit4;
 end;

 TPA_SC_HP3D_TRAP_SCREEN_OCCURRENCE=bitpacked record
  COUNT    :bit16;
  RESERVED0:bit16;
 end;

 TVGT_STRMOUT_DRAW_OPAQUE_VERTEX_STRIDE=bitpacked record
  VERTEX_STRIDE:bit9;
  RESERVED0    :bit23;
 end;

 TVGT_STRMOUT_DRAW_OPAQUE_BUFFER_FILLED_SIZE=bit32;


implementation

end.

