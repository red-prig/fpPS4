unit ps4_pssl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bittype;

const
 LAST_9BIT=$FF800000;
 LAST_7BIT=$FE000000;
 LAST_6BIT=$FC000000;
 LAST_5BIT=$F8000000;
 LAST_4BIT=$F0000000;
 LAST_2BIT=$C0000000;
 LAST_1BIT=$80000000;

 H_SOP1  =%101111101;//9
 H_SOPC  =%101111110;//9
 H_SOPP  =%101111111;//9

 H_VOP1  =%0111111;  //7
 H_VOPC  =%0111110;  //7

 H_VOP3  =%110100;   //6
 H_DS    =%110110;   //6
 H_MUBUF =%111000;   //6
 H_MTBUF =%111010;   //6
 H_EXP   =%111110;   //6
 H_VINTRP=%110010;   //6
 H_MIMG  =%111100;   //6

 H_SMRD  =%11000;    //5

 H_SOPK  =%1011;     //4

 H_SOP2  =%10;       //2

 H_VOP2  =%0;        //1

 //DWORD
 DW_SOP1  =H_SOP1 shl 23; //9
 DW_SOPC  =H_SOPC shl 23; //9
 DW_SOPP  =H_SOPP shl 23; //9

 DW_VOP1  =H_VOP1 shl 25; //7
 DW_VOPC  =H_VOPC shl 25; //7

 DW_VOP3  =H_VOP3   shl 26; //6
 DW_DS    =H_DS     shl 26; //6
 DW_MUBUF =H_MUBUF  shl 26; //6
 DW_MTBUF =H_MTBUF  shl 26; //6
 DW_EXP   =H_EXP    shl 26; //6
 DW_VINTRP=H_VINTRP shl 26; //6
 DW_MIMG  =H_MIMG   shl 26; //6

 DW_SMRD  =H_SMRD shl 27; //5

 DW_SOPK  =H_SOPK shl 28; //4

 DW_SOP2  =H_SOP2 shl 30; //2

 DW_VOP2  =H_VOP2 shl 31; //1

 //WORD
 W_SOP1  =H_SOP1 shl 7; //9
 W_SOPC  =H_SOPC shl 7; //9
 W_SOPP  =H_SOPP shl 7; //9

 W_VOP1  =H_VOP1 shl 9; //7
 W_VOPC  =H_VOPC shl 9; //7

 W_VOP3  =H_VOP3   shl 10; //6
 W_DS    =H_DS     shl 10; //6
 W_MUBUF =H_MUBUF  shl 10; //6
 W_MTBUF =H_MTBUF  shl 10; //6
 W_EXP   =H_EXP    shl 10; //6
 W_VINTRP=H_VINTRP shl 10; //6
 W_MIMG  =H_MIMG   shl 10; //6

 W_SMRD  =H_SMRD shl 11; //5

 W_SOPK  =H_SOPK shl 12; //4

 W_SOP2  =H_SOP2 shl 14; //2

 W_VOP2  =H_VOP2 shl 15; //1

 //SOP1
 S_MOV_B32           =$03;
 S_MOV_B64           =$04;
 S_CMOV_B32          =$05;
 S_CMOV_B64          =$06;
 S_NOT_B32           =$07;
 S_NOT_B64           =$08;
 S_WQM_B32           =$09;
 S_WQM_B64           =$0A;
 S_BREV_B32          =$0B;
 S_BREV_B64          =$0C;
 S_BCNT0_I32_B32     =$0D;
 S_BCNT0_I32_B64     =$0E;
 S_BCNT1_I32_B32     =$0F;
 S_BCNT1_I32_B64     =$10;
 S_FF0_I32_B32       =$11;
 S_FF0_I32_B64       =$12;
 S_FF1_I32_B32       =$13;
 S_FF1_I32_B64       =$14;
 S_FLBIT_I32_B32     =$15;
 S_FLBIT_I32_B64     =$16;
 S_FLBIT_I32         =$17;
 S_FLBIT_I32_I64     =$18;
 S_SEXT_I32_I8       =$19;
 S_SEXT_I32_I16      =$1A;
 S_BITSET0_B32       =$1B;
 S_BITSET0_B64       =$1C;
 S_BITSET1_B32       =$1D;
 S_BITSET1_B64       =$1E;
 S_GETPC_B64         =$1F;
 S_SETPC_B64         =$20; //BRANCH
 S_SWAPPC_B64        =$21; //BRANCH

 S_AND_SAVEEXEC_B64  =$24;
 S_OR_SAVEEXEC_B64   =$25;
 S_XOR_SAVEEXEC_B64  =$26;
 S_ANDN2_SAVEEXEC_B64=$27;
 S_ORN2_SAVEEXEC_B64 =$28;
 S_NAND_SAVEEXEC_B64 =$29;
 S_NOR_SAVEEXEC_B64  =$2A;
 S_XNOR_SAVEEXEC_B64 =$2B;
 S_QUADMASK_B32      =$2C;
 S_QUADMASK_B64      =$2D;
 S_MOVRELS_B32       =$2E;
 S_MOVRELS_B64       =$2F;
 S_MOVRELD_B32       =$30;
 S_MOVRELD_B64       =$31;
 S_CBRANCH_JOIN      =$32;

 S_ABS_I32           =$34;


 //SOP2
 S_ADD_U32       =$00;
 S_SUB_U32       =$01;
 S_ADD_I32       =$02;
 S_SUB_I32       =$03;
 S_ADDC_U32      =$04;
 S_SUBB_U32      =$05;
 S_MIN_I32       =$06;
 S_MIN_U32       =$07;
 S_MAX_I32       =$08;
 S_MAX_U32       =$09;
 S_CSELECT_B32   =$0A;
 S_CSELECT_B64   =$0B;

 S_AND_B32       =$0E;
 S_AND_B64       =$0F;
 S_OR_B32        =$10;
 S_OR_B64        =$11;
 S_XOR_B32       =$12;
 S_XOR_B64       =$13;
 S_ANDN2_B32     =$14;
 S_ANDN2_B64     =$15;
 S_ORN2_B32      =$16;
 S_ORN2_B64      =$17;
 S_NAND_B32      =$18;
 S_NAND_B64      =$19;
 S_NOR_B32       =$1A;
 S_NOR_B64       =$1B;
 S_XNOR_B32      =$1C;
 S_XNOR_B64      =$1D;
 S_LSHL_B32      =$1E;
 S_LSHL_B64      =$1F;
 S_LSHR_B32      =$20;
 S_LSHR_B64      =$21;
 S_ASHR_I32      =$22;
 S_ASHR_I64      =$23;
 S_BFM_B32       =$24;
 S_BFM_B64       =$25;
 S_MUL_I32       =$26;
 S_BFE_U32       =$27;
 S_BFE_I32       =$28;
 S_BFE_U64       =$29;
 S_BFE_I64       =$2A;
 S_CBRANCH_G_FORK=$2B;
 S_ABSDIFF_I32   =$2C;


 //SOPP
 S_NOP           =$00;
 S_ENDPGM        =$01;
 S_BRANCH        =$02;

 S_CBRANCH_SCC0  =$04;
 S_CBRANCH_SCC1  =$05;
 S_CBRANCH_VCCZ  =$06;
 S_CBRANCH_VCCNZ =$07;
 S_CBRANCH_EXECZ =$08;
 S_CBRANCH_EXECNZ=$09;
 S_BARRIER       =$0A;

 S_WAITCNT       =$0C;

 S_SLEEP         =$0E;
 S_SETPRIO       =$0F;
 S_SENDMSG       =$10;

 S_ICACHE_INV    =$13;
 S_INCPERFLEVEL  =$14;
 S_DECPERFLEVEL  =$15;
 S_TTRACEDATA    =$16;

 //SOPC
 S_CMP_EQ_I32 =$00;
 S_CMP_LG_I32 =$01;
 S_CMP_GT_I32 =$02;
 S_CMP_GE_I32 =$03;
 S_CMP_LT_I32 =$04;
 S_CMP_LE_I32 =$05;
 S_CMP_EQ_U32 =$06;
 S_CMP_LG_U32 =$07;
 S_CMP_GT_U32 =$08;
 S_CMP_GE_U32 =$09;
 S_CMP_LT_U32 =$0A;
 S_CMP_LE_U32 =$0B;
 S_BITCMP0_B32=$0C;
 S_BITCMP1_B32=$0D;
 S_BITCMP0_B64=$0E;
 S_BITCMP1_B64=$0F;
 S_SETVSKIP   =$10;

 //SOPK
 S_MOVK_I32        =$00;
 S_MOVK_HI_I32     =$01;
 S_CMOVK_I32       =$02;
 S_CMPK_EQ_I32     =$03;
 S_CMPK_LG_I32     =$04;
 S_CMPK_GT_I32     =$05;
 S_CMPK_GE_I32     =$06;
 S_CMPK_LT_I32     =$07;
 S_CMPK_LE_I32     =$08;
 S_CMPK_EQ_U32     =$09;
 S_CMPK_LG_U32     =$0A;
 S_CMPK_GT_U32     =$0B;
 S_CMPK_GE_U32     =$0C;
 S_CMPK_LT_U32     =$0D;
 S_CMPK_LE_U32     =$0E;
 S_ADDK_I32        =$0F;
 S_MULK_I32        =$10;
 S_CBRANCH_I_FORK  =$11;
 S_GETREG_B32      =$12;
 S_SETREG_B32      =$13;
 S_SETREG_IMM32_B32=$15;

 //VOP2
 V_CNDMASK_B32       =$00;
 V_READLANE_B32      =$01;
 V_WRITELANE_B32     =$02;
 V_ADD_F32           =$03;
 V_SUB_F32           =$04;
 V_SUBREV_F32        =$05;
 V_MAC_LEGACY_F32    =$06;
 V_MUL_LEGACY_F32    =$07;
 V_MUL_F32           =$08;
 V_MUL_I32_I24       =$09;
 V_MUL_HI_I32_I24    =$0A;
 V_MUL_U32_U24       =$0B;
 V_MUL_HI_U32_U24    =$0C;
 V_MIN_LEGACY_F32    =$0D;
 V_MAX_LEGACY_F32    =$0E;
 V_MIN_F32           =$0F;
 V_MAX_F32           =$10;
 V_MIN_I32           =$11;
 V_MAX_I32           =$12;
 V_MIN_U32           =$13;
 V_MAX_U32           =$14;
 V_LSHR_B32          =$15;
 V_LSHRREV_B32       =$16;
 V_ASHR_I32          =$17;
 V_ASHRREV_I32       =$18;
 V_LSHL_B32          =$19;
 V_LSHLREV_B32       =$1A;
 V_AND_B32           =$1B;
 V_OR_B32            =$1C;
 V_XOR_B32           =$1D;
 V_BFM_B32           =$1E;
 V_MAC_F32           =$1F;
 V_MADMK_F32         =$20;
 V_MADAK_F32         =$21;
 V_BCNT_U32_B32      =$22;
 V_MBCNT_LO_U32_B32  =$23;
 V_MBCNT_HI_U32_B32  =$24;
 V_ADD_I32           =$25;
 V_SUB_I32           =$26;
 V_SUBREV_I32        =$27;
 V_ADDC_U32          =$28;
 V_SUBB_U32          =$29;
 V_SUBBREV_U32       =$2A;
 V_LDEXP_F32         =$2B;
 V_CVT_PKACCUM_U8_F32=$2C;
 V_CVT_PKNORM_I16_F32=$2D;
 V_CVT_PKNORM_U16_F32=$2E;
 V_CVT_PKRTZ_F16_F32 =$2F;
 V_CVT_PK_U16_U32    =$30;
 V_CVT_PK_I16_I32    =$31;

 //VOP3a
 V_MAD_LEGACY_F32 =$140;
 V_MAD_F32        =$141;
 V_MAD_I32_I24    =$142;
 V_MAD_U32_U24    =$143;
 V_CUBEID_F32     =$144;
 V_CUBESC_F32     =$145;
 V_CUBETC_F32     =$146;
 V_CUBEMA_F32     =$147;
 V_BFE_U32        =$148;
 V_BFE_I32        =$149;
 V_BFI_B32        =$14A;
 V_FMA_F32        =$14B;
 V_FMA_F64        =$14C;
 V_LERP_U8        =$14D;
 V_ALIGNBIT_B32   =$14E;
 V_ALIGNBYTE_B32  =$14F;
 V_MULLIT_F32     =$150;
 V_MIN3_F32       =$151;
 V_MIN3_I32       =$152;
 V_MIN3_U32       =$153;
 V_MAX3_F32       =$154;
 V_MAX3_I32       =$155;
 V_MAX3_U32       =$156;
 V_MED3_F32       =$157;
 V_MED3_I32       =$158;
 V_MED3_U32       =$159;
 V_SAD_U8         =$15A;
 V_SAD_HI_U8      =$15B;
 V_SAD_U16        =$15C;
 V_SAD_U32        =$15D;
 V_CVT_PK_U8_F32  =$15E;
 V_DIV_FIXUP_F32  =$15F;
 V_DIV_FIXUP_F64  =$160;
 V_LSHL_B64       =$161;
 V_LSHR_B64       =$162;
 V_ASHR_I64       =$163;
 V_ADD_F64        =$164;
 V_MUL_F64        =$165;
 V_MIN_F64        =$166;
 V_MAX_F64        =$167;
 V_LDEXP_F64      =$168;
 V_MUL_LO_U32     =$169;
 V_MUL_HI_U32     =$16A;
 V_MUL_LO_I32     =$16B;
 V_MUL_HI_I32     =$16C;

 V_DIV_FMAS_F32   =$16F;
 V_DIV_FMAS_F64   =$170;
 V_MSAD_U8        =$171;
 V_QSAD_PK_U16_U8 =$172;
 V_MQSAD_PK_U16_U8=$173;
 V_TRIG_PREOP_F64 =$174;
 V_MQSAD_U32_U8   =$175;
 V_MAD_U64_U32    =$176;
 V_MAD_I64_I32    =$177;

 //VOP3b
 V_DIV_SCALE_F32  =$16D;
 V_DIV_SCALE_F64  =$16E;

 //SMRD
 S_LOAD_DWORD          =$00;
 S_LOAD_DWORDX2        =$01;
 S_LOAD_DWORDX4        =$02;
 S_LOAD_DWORDX8        =$03;
 S_LOAD_DWORDX16       =$04;

 S_BUFFER_LOAD_DWORD   =$08;
 S_BUFFER_LOAD_DWORDX2 =$09;
 S_BUFFER_LOAD_DWORDX4 =$0A;
 S_BUFFER_LOAD_DWORDX8 =$0B;
 S_BUFFER_LOAD_DWORDX16=$0C;

 S_MEMTIME             =$1E;
 S_DCACHE_INV          =$1F;

 //VOPC

 {OP16}
 //F   = 0
 //LT  = 1
 //EQ  = 2
 //LE  = 3
 //GT  = 4
 //LG  = 5
 //GE  = 6
 //O   = 7
 //U   = 8
 //NGE = 9
 //NLG = A
 //NGT = B
 //NLE = C
 //NEQ = D
 //NLT = E
 //T   = F

 {OP8}
 //F   = 0;
 //LT  = 1;
 //EQ  = 2;
 //LE  = 3;
 //GT  = 4;
 //LG  = 5;
 //GE  = 6;
 //T   = 7;

 //V_CMP_{OP16}_F32   0x00
 //V_CMPX_{OP16}_F32  0x10
 //V_CMP_{OP16}_F64   0x20
 //V_CMPX_{OP16}_F64  0x30
 //V_CMPS_{OP16}_F32  0x40
 //V_CMPSX_{OP16}_F32 0x50
 //V_CMPS_{OP16}_F64  0x60
 //V_CMPSX_{OP16}_F64 0x70

 //V_CMP_{OP8}_I32    0x80
 //V_CMPX_{OP8}_I32   0x90
 //V_CMP_{OP8}_I64    0xA0
 //V_CMPX_{OP8}_I64   0xB0
 //V_CMP_{OP8}_U32    0xC0
 //V_CMPX_{OP8}_U32   0xD0
 //V_CMP_{OP8}_U64    0xE0
 //V_CMPX_{OP8}_U64   0xF0

 V_CMP_F_F32     =$00;
 V_CMP_LT_F32    =$01;
 V_CMP_EQ_F32    =$02;
 V_CMP_LE_F32    =$03;
 V_CMP_GT_F32    =$04;
 V_CMP_LG_F32    =$05;
 V_CMP_GE_F32    =$06;
 V_CMP_O_F32     =$07;
 V_CMP_U_F32     =$08;
 V_CMP_NGE_F32   =$09;
 V_CMP_NLG_F32   =$0A;
 V_CMP_NGT_F32   =$0B;
 V_CMP_NLE_F32   =$0C;
 V_CMP_NEQ_F32   =$0D;
 V_CMP_NLT_F32   =$0E;
 V_CMP_T_F32     =$0F;

 V_CMPX_F_F32    =$10;
 V_CMPX_LT_F32   =$11;
 V_CMPX_EQ_F32   =$12;
 V_CMPX_LE_F32   =$13;
 V_CMPX_GT_F32   =$14;
 V_CMPX_LG_F32   =$15;
 V_CMPX_GE_F32   =$16;
 V_CMPX_O_F32    =$17;
 V_CMPX_U_F32    =$18;
 V_CMPX_NGE_F32  =$19;
 V_CMPX_NLG_F32  =$1A;
 V_CMPX_NGT_F32  =$1B;
 V_CMPX_NLE_F32  =$1C;
 V_CMPX_NEQ_F32  =$1D;
 V_CMPX_NLT_F32  =$1E;
 V_CMPX_T_F32    =$1F;

 V_CMP_F_F64     =$20;
 V_CMP_LT_F64    =$21;
 V_CMP_EQ_F64    =$22;
 V_CMP_LE_F64    =$23;
 V_CMP_GT_F64    =$24;
 V_CMP_LG_F64    =$25;
 V_CMP_GE_F64    =$26;
 V_CMP_O_F64     =$27;
 V_CMP_U_F64     =$28;
 V_CMP_NGE_F64   =$29;
 V_CMP_NLG_F64   =$2A;
 V_CMP_NGT_F64   =$2B;
 V_CMP_NLE_F64   =$2C;
 V_CMP_NEQ_F64   =$2D;
 V_CMP_NLT_F64   =$2E;
 V_CMP_T_F64     =$2F;

 V_CMPX_F_F64    =$30;
 V_CMPX_LT_F64   =$31;
 V_CMPX_EQ_F64   =$32;
 V_CMPX_LE_F64   =$33;
 V_CMPX_GT_F64   =$34;
 V_CMPX_LG_F64   =$35;
 V_CMPX_GE_F64   =$36;
 V_CMPX_O_F64    =$37;
 V_CMPX_U_F64    =$38;
 V_CMPX_NGE_F64  =$39;
 V_CMPX_NLG_F64  =$3A;
 V_CMPX_NGT_F64  =$3B;
 V_CMPX_NLE_F64  =$3C;
 V_CMPX_NEQ_F64  =$3D;
 V_CMPX_NLT_F64  =$3E;
 V_CMPX_T_F64    =$3F;


 V_CMPS_F_F32    =$40;
 V_CMPS_LT_F32   =$41;
 V_CMPS_EQ_F32   =$42;
 V_CMPS_LE_F32   =$43;
 V_CMPS_GT_F32   =$44;
 V_CMPS_LG_F32   =$45;
 V_CMPS_GE_F32   =$46;
 V_CMPS_O_F32    =$47;
 V_CMPS_U_F32    =$48;
 V_CMPS_NGE_F32  =$49;
 V_CMPS_NLG_F32  =$4A;
 V_CMPS_NGT_F32  =$4B;
 V_CMPS_NLE_F32  =$4C;
 V_CMPS_NEQ_F32  =$4D;
 V_CMPS_NLT_F32  =$4E;
 V_CMPS_T_F32    =$4F;

 V_CMPSX_F_F32   =$50;
 V_CMPSX_LT_F32  =$51;
 V_CMPSX_EQ_F32  =$52;
 V_CMPSX_LE_F32  =$53;
 V_CMPSX_GT_F32  =$54;
 V_CMPSX_LG_F32  =$55;
 V_CMPSX_GE_F32  =$56;
 V_CMPSX_O_F32   =$57;
 V_CMPSX_U_F32   =$58;
 V_CMPSX_NGE_F32 =$59;
 V_CMPSX_NLG_F32 =$5A;
 V_CMPSX_NGT_F32 =$5B;
 V_CMPSX_NLE_F32 =$5C;
 V_CMPSX_NEQ_F32 =$5D;
 V_CMPSX_NLT_F32 =$5E;
 V_CMPSX_T_F32   =$5F;

 V_CMPS_F_F64    =$60;
 V_CMPS_LT_F64   =$61;
 V_CMPS_EQ_F64   =$62;
 V_CMPS_LE_F64   =$63;
 V_CMPS_GT_F64   =$64;
 V_CMPS_LG_F64   =$65;
 V_CMPS_GE_F64   =$66;
 V_CMPS_O_F64    =$67;
 V_CMPS_U_F64    =$68;
 V_CMPS_NGE_F64  =$69;
 V_CMPS_NLG_F64  =$6A;
 V_CMPS_NGT_F64  =$6B;
 V_CMPS_NLE_F64  =$6C;
 V_CMPS_NEQ_F64  =$6D;
 V_CMPS_NLT_F64  =$6E;
 V_CMPS_T_F64    =$6F;

 V_CMPSX_F_F64   =$70;
 V_CMPSX_LT_F64  =$71;
 V_CMPSX_EQ_F64  =$72;
 V_CMPSX_LE_F64  =$73;
 V_CMPSX_GT_F64  =$74;
 V_CMPSX_LG_F64  =$75;
 V_CMPSX_GE_F64  =$76;
 V_CMPSX_O_F64   =$77;
 V_CMPSX_U_F64   =$78;
 V_CMPSX_NGE_F64 =$79;
 V_CMPSX_NLG_F64 =$7A;
 V_CMPSX_NGT_F64 =$7B;
 V_CMPSX_NLE_F64 =$7C;
 V_CMPSX_NEQ_F64 =$7D;
 V_CMPSX_NLT_F64 =$7E;
 V_CMPSX_T_F64   =$7F;

 //

 V_CMP_F_I32     =$80;
 V_CMP_LT_I32    =$81;
 V_CMP_EQ_I32    =$82;
 V_CMP_LE_I32    =$83;
 V_CMP_GT_I32    =$84;
 V_CMP_LG_I32    =$85;
 V_CMP_GE_I32    =$86;
 V_CMP_T_I32     =$87;

 V_CMPX_F_I32    =$90;
 V_CMPX_LT_I32   =$91;
 V_CMPX_EQ_I32   =$92;
 V_CMPX_LE_I32   =$93;
 V_CMPX_GT_I32   =$94;
 V_CMPX_LG_I32   =$95;
 V_CMPX_GE_I32   =$96;
 V_CMPX_T_I32    =$97;

 V_CMP_F_I64     =$A0;
 V_CMP_LT_I64    =$A1;
 V_CMP_EQ_I64    =$A2;
 V_CMP_LE_I64    =$A3;
 V_CMP_GT_I64    =$A4;
 V_CMP_LG_I64    =$A5;
 V_CMP_GE_I64    =$A6;
 V_CMP_T_I64     =$A7;

 V_CMPX_F_I64    =$B0;
 V_CMPX_LT_I64   =$B1;
 V_CMPX_EQ_I64   =$B2;
 V_CMPX_LE_I64   =$B3;
 V_CMPX_GT_I64   =$B4;
 V_CMPX_LG_I64   =$B5;
 V_CMPX_GE_I64   =$B6;
 V_CMPX_T_I64    =$B7;

 V_CMP_F_U32     =$C0;
 V_CMP_LT_U32    =$C1;
 V_CMP_EQ_U32    =$C2;
 V_CMP_LE_U32    =$C3;
 V_CMP_GT_U32    =$C4;
 V_CMP_LG_U32    =$C5;
 V_CMP_GE_U32    =$C6;
 V_CMP_T_U32     =$C7;

 V_CMPX_F_U32    =$D0;
 V_CMPX_LT_U32   =$D1;
 V_CMPX_EQ_U32   =$D2;
 V_CMPX_LE_U32   =$D3;
 V_CMPX_GT_U32   =$D4;
 V_CMPX_LG_U32   =$D5;
 V_CMPX_GE_U32   =$D6;
 V_CMPX_T_U32    =$D7;

 V_CMP_F_U64     =$E0;
 V_CMP_LT_U64    =$E1;
 V_CMP_EQ_U64    =$E2;
 V_CMP_LE_U64    =$E3;
 V_CMP_GT_U64    =$E4;
 V_CMP_LG_U64    =$E5;
 V_CMP_GE_U64    =$E6;
 V_CMP_T_U64     =$E7;

 V_CMPX_F_U64    =$F0;
 V_CMPX_LT_U64   =$F1;
 V_CMPX_EQ_U64   =$F2;
 V_CMPX_LE_U64   =$F3;
 V_CMPX_GT_U64   =$F4;
 V_CMPX_LG_U64   =$F5;
 V_CMPX_GE_U64   =$F6;
 V_CMPX_T_U64    =$F7;

 V_CMP_CLASS_F32 =$88;
 V_CMPX_CLASS_F32=$98;
 V_CMP_CLASS_F64 =$A8;
 V_CMPX_CLASS_F64=$B8;

 //VOP1
 V_NOP              =$00;
 V_MOV_B32          =$01;
 V_READFIRSTLANE_B32=$02;
 V_CVT_I32_F64      =$03;
 V_CVT_F64_I32      =$04;
 V_CVT_F32_I32      =$05;
 V_CVT_F32_U32      =$06;
 V_CVT_U32_F32      =$07;
 V_CVT_I32_F32      =$08;
 V_MOV_FED_B32      =$09;
 V_CVT_F16_F32      =$0A;
 V_CVT_F32_F16      =$0B;
 V_CVT_RPI_I32_F32  =$0C;
 V_CVT_FLR_I32_F32  =$0D;
 V_CVT_OFF_F32_I4   =$0E;
 V_CVT_F32_F64      =$0F;
 V_CVT_F64_F32      =$10;
 V_CVT_F32_UBYTE0   =$11;
 V_CVT_F32_UBYTE1   =$12;
 V_CVT_F32_UBYTE2   =$13;
 V_CVT_F32_UBYTE3   =$14;
 V_CVT_U32_F64      =$15;
 V_CVT_F64_U32      =$16;
 V_TRUNC_F64        =$17;
 V_CEIL_F64         =$18;
 V_RNDNE_F64        =$19;
 V_FLOOR_F64        =$1A;
 V_FRACT_F32        =$20;
 V_TRUNC_F32        =$21;
 V_CEIL_F32         =$22;
 V_RNDNE_F32        =$23;
 V_FLOOR_F32        =$24;
 V_EXP_F32          =$25;
 V_LOG_CLAMP_F32    =$26;
 V_LOG_F32          =$27;
 V_RCP_CLAMP_F32    =$28;
 V_RCP_LEGACY_F32   =$29;
 V_RCP_F32          =$2A;
 V_RCP_IFLAG_F32    =$2B;
 V_RSQ_CLAMP_F32    =$2C;
 V_RSQ_LEGACY_F32   =$2D;
 V_RSQ_F32          =$2E;
 V_RCP_F64          =$2F;
 V_RCP_CLAMP_F64    =$30;
 V_RSQ_F64          =$31;
 V_RSQ_CLAMP_F64    =$32;
 V_SQRT_F32         =$33;
 V_SQRT_F64         =$34;
 V_SIN_F32          =$35;
 V_COS_F32          =$36;
 V_NOT_B32          =$37;
 V_BFREV_B32        =$38;
 V_FFBH_U32         =$39;
 V_FFBL_B32         =$3A;
 V_FFBH_I32         =$3B;
 V_FREXP_EXP_I32_F64=$3C;
 V_FREXP_MANT_F64   =$3D;
 V_FRACT_F64        =$3E;
 V_FREXP_EXP_I32_F32=$3F;
 V_FREXP_MANT_F32   =$40;
 V_CLREXCP          =$41;
 V_MOVRELD_B32      =$42;
 V_MOVRELS_B32      =$43;
 V_MOVRELSD_B32     =$44;

 //VINTRP
 V_INTERP_P1_F32 =0;
 V_INTERP_P2_F32 =1;
 V_INTERP_MOV_F32=2;

 //MUBUF
 BUFFER_LOAD_FORMAT_X    =$00;
 BUFFER_LOAD_FORMAT_XY   =$01;
 BUFFER_LOAD_FORMAT_XYZ  =$02;
 BUFFER_LOAD_FORMAT_XYZW =$03;
 BUFFER_STORE_FORMAT_X   =$04;
 BUFFER_STORE_FORMAT_XY  =$05;
 BUFFER_STORE_FORMAT_XYZ =$06;
 BUFFER_STORE_FORMAT_XYZW=$07;

 BUFFER_LOAD_UBYTE       =$08;
 BUFFER_LOAD_SBYTE       =$09;
 BUFFER_LOAD_USHORT      =$0A;
 BUFFER_LOAD_SSHORT      =$0B;
 BUFFER_LOAD_DWORD       =$0C;
 BUFFER_LOAD_DWORDX2     =$0D;
 BUFFER_LOAD_DWORDX4     =$0E;
 BUFFER_LOAD_DWORDX3     =$0F;

 BUFFER_STORE_BYTE       =$18;
 BUFFER_STORE_SHORT      =$1A;
 BUFFER_STORE_DWORD      =$1C;
 BUFFER_STORE_DWORDX2    =$1D;
 BUFFER_STORE_DWORDX4    =$1E;
 BUFFER_STORE_DWORDX3    =$1F;

 //MTBUF
 TBUFFER_LOAD_FORMAT_X    =$00;
 TBUFFER_LOAD_FORMAT_XY   =$01;
 TBUFFER_LOAD_FORMAT_XYZ  =$02;
 TBUFFER_LOAD_FORMAT_XYZW =$03;

 TBUFFER_STORE_FORMAT_X   =$04;
 TBUFFER_STORE_FORMAT_XY  =$05;
 TBUFFER_STORE_FORMAT_XYZ =$06;
 TBUFFER_STORE_FORMAT_XYZW=$07;

 // BUF_DATA_FORMAT
 BUF_DATA_FORMAT_INVALID    =$00;
 BUF_DATA_FORMAT_8          =$01;
 BUF_DATA_FORMAT_16         =$02;
 BUF_DATA_FORMAT_8_8        =$03;
 BUF_DATA_FORMAT_32         =$04;
 BUF_DATA_FORMAT_16_16      =$05;
 BUF_DATA_FORMAT_10_11_11   =$06;
 BUF_DATA_FORMAT_11_11_10   =$07;
 BUF_DATA_FORMAT_10_10_10_2 =$08;
 BUF_DATA_FORMAT_2_10_10_10 =$09;
 BUF_DATA_FORMAT_8_8_8_8    =$0a;
 BUF_DATA_FORMAT_32_32      =$0b;
 BUF_DATA_FORMAT_16_16_16_16=$0c;
 BUF_DATA_FORMAT_32_32_32   =$0d;
 BUF_DATA_FORMAT_32_32_32_32=$0e;
 BUF_DATA_FORMAT_RESERVED   =$0f;

 // BUF_NUM_FORMAT
 BUF_NUM_FORMAT_UNORM       =$00;
 BUF_NUM_FORMAT_SNORM       =$01;
 BUF_NUM_FORMAT_USCALED     =$02;
 BUF_NUM_FORMAT_SSCALED     =$03;
 BUF_NUM_FORMAT_UINT        =$04;
 BUF_NUM_FORMAT_SINT        =$05;
 BUF_NUM_FORMAT_SNORM_NZ    =$06;
 BUF_NUM_FORMAT_FLOAT       =$07;

 //MIMG
 IMAGE_LOAD            =$00;
 IMAGE_LOAD_MIP        =$01;
 IMAGE_LOAD_PCK        =$02;
 IMAGE_LOAD_PCK_SGN    =$03;
 IMAGE_LOAD_MIP_PCK    =$04;
 IMAGE_LOAD_MIP_PCK_SGN=$05;

 IMAGE_STORE           =$08;
 IMAGE_STORE_MIP       =$09;
 IMAGE_STORE_PCK       =$0a;
 IMAGE_STORE_MIP_PCK   =$0b;

 IMAGE_GET_RESINFO     =$0e;

 IMAGE_ATOMIC_SWAP     =$0f;
 IMAGE_ATOMIC_CMPSWAP  =$10;
 IMAGE_ATOMIC_ADD      =$11;
 IMAGE_ATOMIC_SUB      =$12;
 IMAGE_ATOMIC_SMIN     =$14;
 IMAGE_ATOMIC_UMIN     =$15;
 IMAGE_ATOMIC_SMAX     =$16;
 IMAGE_ATOMIC_UMAX     =$17;
 IMAGE_ATOMIC_AND      =$18;
 IMAGE_ATOMIC_OR       =$19;
 IMAGE_ATOMIC_XOR      =$1a;
 IMAGE_ATOMIC_INC      =$1b;
 IMAGE_ATOMIC_DEC      =$1c;
 IMAGE_ATOMIC_FCMPSWAP =$1d;
 IMAGE_ATOMIC_FMIN     =$1e;
 IMAGE_ATOMIC_FMAX     =$1f;

 IMAGE_SAMPLE          =$20;
 IMAGE_SAMPLE_CL       =$21;
 IMAGE_SAMPLE_D        =$22;
 IMAGE_SAMPLE_D_CL     =$23;
 IMAGE_SAMPLE_L        =$24;
 IMAGE_SAMPLE_B        =$25;
 IMAGE_SAMPLE_B_CL     =$26;
 IMAGE_SAMPLE_LZ       =$27;
 IMAGE_SAMPLE_C        =$28;
 IMAGE_SAMPLE_C_CL     =$29;
 IMAGE_SAMPLE_C_D      =$2a;
 IMAGE_SAMPLE_C_D_CL   =$2b;
 IMAGE_SAMPLE_C_L      =$2c;
 IMAGE_SAMPLE_C_B      =$2d;
 IMAGE_SAMPLE_C_B_CL   =$2e;
 IMAGE_SAMPLE_C_LZ     =$2f;
 IMAGE_SAMPLE_O        =$30;
 IMAGE_SAMPLE_CL_O     =$31;
 IMAGE_SAMPLE_D_O      =$32;
 IMAGE_SAMPLE_D_CL_O   =$33;
 IMAGE_SAMPLE_L_O      =$34;
 IMAGE_SAMPLE_B_O      =$35;
 IMAGE_SAMPLE_B_CL_O   =$36;
 IMAGE_SAMPLE_LZ_O     =$37;
 IMAGE_SAMPLE_C_O      =$38;
 IMAGE_SAMPLE_C_CL_O   =$39;
 IMAGE_SAMPLE_C_D_O    =$3a;
 IMAGE_SAMPLE_C_D_CL_O =$3b;
 IMAGE_SAMPLE_C_L_O    =$3c;
 IMAGE_SAMPLE_C_B_O    =$3d;
 IMAGE_SAMPLE_C_B_CL_O =$3e;
 IMAGE_SAMPLE_C_LZ_O   =$3f;

 IMAGE_GATHER4         =$40;
 IMAGE_GATHER4_CL      =$41;
 IMAGE_GATHER4_L       =$44;
 IMAGE_GATHER4_B       =$45;
 IMAGE_GATHER4_B_CL    =$46;
 IMAGE_GATHER4_LZ      =$47;
 IMAGE_GATHER4_C       =$48;
 IMAGE_GATHER4_C_CL    =$49;
 IMAGE_GATHER4_C_L     =$4c;
 IMAGE_GATHER4_C_B     =$4d;
 IMAGE_GATHER4_C_B_CL  =$4e;
 IMAGE_GATHER4_C_LZ    =$4f;
 IMAGE_GATHER4_O       =$50;
 IMAGE_GATHER4_CL_O    =$51;
 IMAGE_GATHER4_L_O     =$54;
 IMAGE_GATHER4_B_O     =$55;
 IMAGE_GATHER4_B_CL_O  =$56;
 IMAGE_GATHER4_LZ_O    =$57;
 IMAGE_GATHER4_C_O     =$58;
 IMAGE_GATHER4_C_CL_O  =$59;
 IMAGE_GATHER4_C_L_O   =$5c;
 IMAGE_GATHER4_C_B_O   =$5d;
 IMAGE_GATHER4_C_B_CL_O=$5e;
 IMAGE_GATHER4_C_LZ_O  =$5f;

 IMAGE_GET_LOD         =$60;

 IMAGE_SAMPLE_CD       =$68;
 IMAGE_SAMPLE_CD_CL    =$69;
 IMAGE_SAMPLE_C_CD     =$6a;
 IMAGE_SAMPLE_C_CD_CL  =$6b;
 IMAGE_SAMPLE_CD_O     =$6c;
 IMAGE_SAMPLE_CD_CL_O  =$6d;
 IMAGE_SAMPLE_C_CD_O   =$6e;
 IMAGE_SAMPLE_C_CD_CL_O=$6f;


 //DS
 DS_ADD_U32            =$00;
 DS_SUB_U32            =$01;
 DS_RSUB_U32           =$02;
 DS_INC_U32            =$03;
 DS_DEC_U32            =$04;
 DS_MIN_I32            =$05;
 DS_MAX_I32            =$06;
 DS_MIN_U32            =$07;
 DS_MAX_U32            =$08;
 DS_AND_B32            =$09;
 DS_OR_B32             =$0A;
 DS_XOR_B32            =$0B;
 DS_MSKOR_B32          =$0C;
 DS_WRITE_B32          =$0D;
 DS_WRITE2_B32         =$0E;
 DS_WRITE2ST64_B32     =$0F;
 DS_CMPST_B32          =$10;
 DS_CMPST_F32          =$11;
 DS_MIN_F32            =$12;
 DS_MAX_F32            =$13;
 DS_NOP                =$14;
 DS_GWS_INIT           =$19;
 DS_GWS_SEMA_V         =$1A;
 DS_GWS_SEMA_BR        =$1B;
 DS_GWS_SEMA_P         =$1C;
 DS_GWS_BARRIER        =$1D;
 DS_WRITE_B8           =$1E;
 DS_WRITE_B16          =$1F;
 DS_ADD_RTN_U32        =$20;
 DS_SUB_RTN_U32        =$21;
 DS_RSUB_RTN_U32       =$22;
 DS_INC_RTN_U32        =$23;
 DS_DEC_RTN_U32        =$24;
 DS_MIN_RTN_I32        =$25;
 DS_MAX_RTN_I32        =$26;
 DS_MIN_RTN_U32        =$27;
 DS_MAX_RTN_U32        =$28;
 DS_AND_RTN_B32        =$29;
 DS_OR_RTN_B32         =$2A;
 DS_XOR_RTN_B32        =$2B;
 DS_MSKOR_RTN_B32      =$2C;
 DS_WRXCHG_RTN_B32     =$2D;
 DS_WRXCHG2_RTN_B32    =$2E;
 DS_WRXCHG2ST64_RTN_B32=$2F;
 DS_CMPST_RTN_B32      =$30;
 DS_CMPST_RTN_F32      =$31;
 DS_MIN_RTN_F32        =$32;
 DS_MAX_RTN_F32        =$33;
 DS_WRAP_RTN_B32       =$34;
 DS_SWIZZLE_B32        =$35;
 DS_READ_B32           =$36;
 DS_READ2_B32          =$37;
 DS_READ2ST64_B32      =$38;
 DS_READ_I8            =$39;
 DS_READ_U8            =$3A;
 DS_READ_I16           =$3B;
 DS_READ_U16           =$3C;
 DS_CONSUME            =$3D;
 DS_APPEND             =$3E;
 DS_ORDERED_COUNT      =$3F;
 DS_ADD_U64            =$40;
 DS_SUB_U64            =$41;
 DS_RSUB_U64           =$42;
 DS_INC_U64            =$43;
 DS_DEC_U64            =$44;
 DS_MIN_I64            =$45;
 DS_MAX_I64            =$46;
 DS_MIN_U64            =$47;
 DS_MAX_U64            =$48;
 DS_OR_B64             =$4A;
 DS_XOR_B64            =$4B;
 DS_MSKOR_B64          =$4C;
 DS_WRITE_B64          =$4D;
 DS_WRITE2_B64         =$4E;
 DS_WRITE2ST64_B64     =$4F;
 DS_CMPST_B64          =$50;
 DS_CMPST_F64          =$51;
 DS_MIN_F64            =$52;
 DS_MAX_F64            =$53;
 DS_ADD_RTN_U64        =$60;
 DS_SUB_RTN_U64        =$61;
 DS_RSUB_RTN_U64       =$62;
 DS_INC_RTN_U64        =$63;
 DS_DEC_RTN_U64        =$64;
 DS_MIN_RTN_I64        =$65;
 DS_MAX_RTN_I64        =$66;
 DS_MIN_RTN_U64        =$67;
 DS_MAX_RTN_U64        =$68;
 DS_AND_RTN_B64        =$69;
 DS_OR_RTN_B64         =$6A;
 DS_XOR_RTN_B64        =$6B;
 DS_MSKOR_RTN_B64      =$6C;
 DS_WRXCHG_RTN_B64     =$6D;
 DS_WRXCHG2_RTN_B64    =$6E;
 DS_WRXCHG2ST64_RTN_B64=$6F;
 DS_CMPST_RTN_B64      =$70;
 DS_CMPST_RTN_F64      =$71;
 DS_MIN_RTN_F64        =$72;
 DS_MAX_RTN_F64        =$73;
 DS_READ_B64           =$76;
 DS_READ2_B64          =$77;
 DS_READ2ST64_B64      =$78;
 DS_CONDXCHG32_RTN_B64 =$7E;
 DS_ADD_SRC2_U32       =$80;
 DS_SUB_SRC2_U32       =$81;
 DS_RSUB_SRC2_U32      =$82;
 DS_INC_SRC2_U32B      =$83;
 DS_DEC_SRC2_U32       =$84;
 DS_MIN_SRC2_I32       =$85;
 DS_MAX_SRC2_I32       =$86;
 DS_MIN_SRC2_U32       =$87;
 DS_MAX_SRC2_U32       =$88;
 DS_AND_SRC2_B32B      =$89;
 DS_OR_SRC2_B32        =$8A;
 DS_XOR_SRC2_B32       =$8B;
 DS_WRITE_SRC2_B32     =$8C;
 DS_MIN_SRC2_F32       =$92;
 DS_MAX_SRC2_F32       =$93;
 DS_ADD_SRC2_U64       =$C0;
 DS_SUB_SRC2_U64       =$C1;
 DS_RSUB_SRC2_U64      =$C2;
 DS_INC_SRC2_U64       =$C3;
 DS_DEC_SRC2_U64       =$C4;
 DS_MIN_SRC2_I64       =$C5;
 DS_MAX_SRC2_I64       =$C6;
 DS_MIN_SRC2_U64       =$C7;
 DS_MAX_SRC2_U64       =$C8;
 DS_AND_SRC2_B64       =$C9;
 DS_OR_SRC2_B64        =$CA;
 DS_XOR_SRC2_B64       =$CB;
 DS_MIN_SRC2_F64       =$D2;
 DS_MAX_SRC2_F64       =$D3;

type
 TSOP2=bitpacked record
  SSRC0:Byte;  //8
  SSRC1:Byte;  //8
  SDST:bit7;   //7
  OP:bit7;     //7
  ENCODE:bit2; //2
 end;

 TSOP1=bitpacked record
  SSRC:Byte;   //8
  OP:Byte;     //8
  SDST:bit7;   //7
  ENCODE:bit9; //9
 end;

 TSOPP=bitpacked record
  SIMM:Word;   //16
  OP:bit7;     //7
  ENCODE:bit9; //9
 end;

 TSOPK=bitpacked record
  SIMM:Word;
  SDST:bit7;
  OP:bit5;
  ENCODE:bit4;
 end;

 TSOPC=bitpacked record
  SSRC0:Byte;
  SSRC1:Byte;
  OP:bit7;
  ENCODE:bit9;
 end;

 TVOP2=bitpacked record
  SRC0:bit9;   //9
  VSRC1:Byte;  //8
  VDST:Byte;   //8
  OP:bit6;     //6
  ENCODE:bit1; //1
 end;

 TVOPC=bitpacked record
  SRC0:bit9;   //9
  VSRC1:Byte;  //8
  OP:Byte;     //8
  ENCODE:bit7; //7
 end;

 TVOP1=bitpacked record
  SRC0:bit9;   //9
  OP:Byte;     //8
  VDST:Byte;   //8
  ENCODE:bit7; //7
 end;

 TSMRD=bitpacked record
  OFFSET:Byte; //8
  IMM:bit1;    //1
  SBASE:bit6;  //6
  SDST:bit7;   //7
  OP:bit5;     //5
  ENCODE:bit5; //5
 end;

 Twaitcnt_simm=bitpacked record
  vmcnt:bit4;     //0..3
  expcnt:bit3;    //4..6
  reserved1:bit1; //7
  lgkmcnt:bit4;   //8..11
  reserved2:bit4; //12..15
 end;

 TVOP3a=bitpacked record
  VDST:Byte;    //8
  ABS:bit3;     //3
  CLAMP:bit1;   //1
  reserved:bit5;//5
  OP:bit9;      //9
  ENCODE:bit6;  //6

  SRC0:bit9;    //9
  SRC1:bit9;    //9
  SRC2:bit9;    //9
  OMOD:bit2;    //2
  NEG:bit3;     //3
 end;

 TVOP3b=bitpacked record //with SDST
  VDST:Byte;    //8
  SDST:bit7;    //7
  reserved:bit2;//2
  OP:bit9;      //9
  ENCODE:bit6;  //6

  SRC0:bit9;    //9
  SRC1:bit9;    //9
  SRC2:bit9;    //9
  OMOD:bit2;    //2
  NEG:bit3;     //3
 end;

 TMUBUF=bitpacked record
  OFFSET:bit12;  //12
  OFFEN:bit1;    //1
  IDXEN:bit1;    //1
  GLC:bit1;      //1
  reserved1:bit1;//1
  LDS:bit1;      //1
  reserved2:bit1;//1
  OP:bit7;       //7
  reserved3:bit1;//1
  ENCODE:bit6;   //6

  VADDR:Byte;    //8
  VDATA:Byte;    //8
  SRSRC:bit5;    //5
  reserved4:bit1;//1
  SLC:bit1;      //1
  TFE:bit1;      //1
  SOFFSET:Byte;  //8
 end;

 TMTBUF=bitpacked record
  OFFSET:bit12;
  OFFEN:bit1;
  IDXEN:bit1;
  GLC:bit1;
  reserved1:bit1;
  OP:bit3;
  DFMT:bit4;
  NFMT:bit3;
  ENCODE:bit6;

  VADDR:Byte;
  VDATA:Byte;
  SRSRC:bit5;
  reserved4:bit1;
  SLC:bit1;
  TFE:bit1;
  SOFFSET:Byte;
 end;

 TEXP=bitpacked record
  EN:bit4;
  TGT:bit6;
  COMPR:bit1;
  DONE:bit1;
  VM:bit1;
  reserved:bit13;
  ENCODING:bit6;

  VSRC0:Byte;
  VSRC1:Byte;
  VSRC2:Byte;
  VSRC3:Byte;
 end;

 TVINTRP=bitpacked record
  VSRC:Byte;
  ATTRCHAN:bit2;
  ATTR:bit6;
  OP:bit2;
  VDST:Byte;
  ENCODING:bit6;
 end;

 TMIMG=bitpacked record
  reserved1:Byte;
  DMASK:bit4;
  UNRM:bit1;
  GLC:bit1;
  DA:bit1;
  R128:bit1;
  TFE:bit1;
  LWE:bit1;
  OP:bit7;
  SLC:bit1;
  ENCODING:bit6;

  VADDR:Byte;
  VDATA:Byte;
  SRSRC:bit5;
  SSAMP:bit5;
  reserved2:bit6;
 end;

 TDS=bitpacked record
  OFFSET0:Byte;
  OFFSET1:Byte;
  reserved1:bit1;
  GDS:bit1;
  OP:Byte;
  ENCODING:bit6;

  ADDR:Byte;  //(vbindex)
  DATA0:Byte; //(vsrc0)
  DATA1:Byte; //(vsrc1)
  VDST:Byte;
 end;

{
SOP2 32+
SOPK 32
SOP1 32+
SOPC 32+
SOPP 32

SMRD 32+

VOP2 32+
VOP1 32+
VOPC 32+
VOP3 64
VOP3 64

VINTRP 32

DS 64

MUBUF 64
MTBUF 64
MIMG 64
EXP 64
}

type
 PSPI=^TSPI;
 TSPI=packed record
  OFFSET_DW:DWORD;
  CMD:packed record
   Case Byte of
    0:(ID:DWORD);
    1:(OP,EN:Word);
  end;
  Case Byte of
    0:(INST64:QWORD);
    1:(INST32,INLINE32:DWORD);
    2:(SOP2:TSOP2);
    3:(SOPK:TSOPK);
    4:(SOP1:TSOP1);
    5:(SOPC:TSOPC);
    6:(SOPP:TSOPP);
    7:(SMRD:TSMRD);
    8:(VOP2:TVOP2);
    9:(VOP1:TVOP1);
   10:(VOPC:TVOPC);
   11:(VOP3a:TVOP3a);
   12:(VOP3b:TVOP3b);
   13:(VINTRP:TVINTRP);
   14:(DS:TDS);
   15:(MUBUF:TMUBUF);
   16:(MTBUF:TMTBUF);
   17:(MIMG:TMIMG);
   18:(EXP:TEXP);
 end;

 TShaderParser=object
  Body:PDWORD;
  OFFSET_DW,MAX_BRANCH_DW:DWORD;
  Function Next(Var SPI:TSPI):Integer;
 end;

function  _parse_print(Body:Pointer;size_dw:DWORD=0;setpc:Boolean=false):Pointer;
function  _parse_size (Body:Pointer;size_dw:DWORD=0;setpc:Boolean=false):Pointer;
procedure print_spi(Var SPI:TSPI);

implementation

type
 TVOP3_32=bitpacked record
  a1:Word;
  a2:bit1;
  OP:bit9;
  ENCODE:bit6;
 end;

 TDS_32=bitpacked record
  a1:Word;
  a2:bit2;
  OP:Byte;
  ENCODING:bit6;
 end;

 TMUBUF_32=bitpacked record
  a1:Word;
  a2:bit2;
  OP:bit7;
  a3:bit1;
  ENCODE:bit6;
 end;

 TMTBUF_32=bitpacked record
  a1:Word;
  OP:bit3;
  a2:bit7;
  ENCODE:bit6;
 end;

 TMIMG_32=bitpacked record
  a1:Word;
  a2:bit2;
  OP:bit7;
  a3:bit1;
  ENCODING:bit6;
 end;

Function TShaderParser.Next(Var SPI:TSPI):Integer;
Var
 ptr:Pointer;
 H,T:DWord;

 procedure pack4(OP:WORD); inline;
 begin
  SPI.OFFSET_DW:=OFFSET_DW;
  SPI.CMD.ID:=T or OP;
  SPI.INST32:=H;
  SPI.INLINE32:=0;
  Inc(OFFSET_DW);
 end;

 procedure pack8(OP:WORD); inline;
 begin
  SPI.OFFSET_DW:=OFFSET_DW;
  SPI.CMD.ID:=T or OP;
  SPI.INST64:=PQWORD(ptr)^;
  Inc(OFFSET_DW,2);
 end;

 Procedure update_max_branch(S:Smallint); inline;
 Var
  i:DWORD;
 begin
  if (S>0) then
  begin
   i:=OFFSET_DW+S+1;
   if (i>MAX_BRANCH_DW) then MAX_BRANCH_DW:=i;
  end;
 end;

begin
 if (Body=nil) then Exit(-2);
 Result:=0;
 ptr:=@PDWORD(Body)[OFFSET_DW];
 H:=PDWORD(ptr)^;
 T:=H and LAST_9BIT;
 Case T of //9
  DW_SOP1:if (TSOP1(H).SSRC=$FF) then pack8(TSOP1(H).OP) else pack4(TSOP1(H).OP);
  DW_SOPC:if (TSOPC(H).SSRC0=$FF) or
             (TSOPC(H).SSRC1=$FF) then pack8(TSOPC(H).OP) else pack4(TSOPC(H).OP);
  DW_SOPP:
    begin
     Case TSOPP(H).OP of
      S_BRANCH,
      S_CBRANCH_SCC0,
      S_CBRANCH_SCC1,
      S_CBRANCH_VCCZ,
      S_CBRANCH_VCCNZ,
      S_CBRANCH_EXECZ,
      S_CBRANCH_EXECNZ:
        begin
         update_max_branch(TSOPP(H).SIMM);
        end;
      S_ENDPGM:
        begin
         if (OFFSET_DW>=MAX_BRANCH_DW) then Result:=1;
        end;
      else;
     end;
     pack4(TSOPP(H).OP);
    end
  else
   begin
    T:=H and LAST_7BIT;
    Case T of //7
     DW_VOP1:if (TVOP1(H).SRC0=$FF) then pack8(TVOP1(H).OP) else pack4(TVOP1(H).OP);
     DW_VOPC:if (TVOPC(H).SRC0=$FF) then pack8(TVOPC(H).OP) else pack4(TVOPC(H).OP);
     else
       begin
        T:=H and LAST_6BIT;
        Case T of //6
         DW_VOP3  :pack8(TVOP3_32(H).OP);
         DW_DS    :pack8(TDS_32(H).OP);
         DW_VINTRP:pack4(TVINTRP(H).OP);
         DW_EXP   :pack8(0);
         DW_MUBUF :pack8(TMUBUF_32(H).OP);
         DW_MTBUF :pack8(TMTBUF_32(H).OP);
         DW_MIMG  :pack8(TMIMG_32(H).OP);
         else
          begin
           T:=H and LAST_5BIT;
           if (T=DW_SMRD) then //5
           begin
            if (TSMRD(H).IMM=0) and
               (TSMRD(H).OFFSET=$FF) then pack8(TSMRD(H).OP) else pack4(TSMRD(H).OP);
           end else
           begin
            T:=H and LAST_4BIT;
            if (T=DW_SOPK) then //4
            begin
             if (TSOPK(H).OP=S_CBRANCH_I_FORK) then
             begin
              update_max_branch(TSOPK(H).SIMM);
             end;
             pack4(TSOPK(H).OP);
            end else
            begin
             T:=H and LAST_2BIT;
             if (T=DW_SOP2) then //2
             begin
              if (TSOP2(H).SSRC0=$FF) or
                 (TSOP2(H).SSRC1=$FF) then pack8(TSOP2(H).OP) else pack4(TSOP2(H).OP);
             end else
             begin
              T:=H and LAST_1BIT;
              if (T=DW_VOP2) then //1
              begin
               if (TVOP2(H).OP=V_MADMK_F32) or
                  (TVOP2(H).OP=V_MADAK_F32) or
                  (TVOP2(H).SRC0=$FF) then pack8(TVOP2(H).OP) else pack4(TVOP2(H).OP);
              end else
               Result:=-1;
             end;
            end;
           end;
          end;
        end;
       end;
    end;
   end;
 end;
end;

procedure _print_sdst7(SDST:Byte);
begin
 Case SDST of
  0..103:Write('s',SDST);
  106:Write('VCC_LO');
  107:Write('VCC_HI');
  124:Write('M0');
  126:Write('EXEC_LO');
  127:Write('EXEC_HI');
  else
      Write('?');
 end;
end;

procedure _print_ssrc8(SSRC:Byte);
begin
 Case SSRC of
  0..103:Write('s',SSRC);
  106:Write('VCC_LO');
  107:Write('VCC_HI');
  124:Write('M0');
  126:Write('EXEC_LO');
  127:Write('EXEC_HI');

  128..192:Write(SSRC-128);
  193..208:Write(-(SSRC-192));
  240:Write('0.5');
  241:Write('-0.5');
  242:Write('1.0');
  243:Write('-1.0');
  244:Write('2.0');
  245:Write('-2.0');
  246:Write('4.0');
  247:Write('-4.0');

  251:Write('VCCZ');
  252:Write('EXECZ');
  253:Write('SCC');
  254:Write('LDS_DIRECT');
  else
      Write('?');
 end;
end;

procedure _print_ssrc8(SSRC:Byte;d2:DWORD);
begin
 Case SSRC of
  0..103:Write('s',SSRC);
  106:Write('VCC_LO');
  107:Write('VCC_HI');
  124:Write('M0');
  126:Write('EXEC_LO');
  127:Write('EXEC_HI');

  128..192:Write(SSRC-128);
  193..208:Write(-(SSRC-192));
  240:Write('0.5');
  241:Write('-0.5');
  242:Write('1.0');
  243:Write('-1.0');
  244:Write('2.0');
  245:Write('-2.0');
  246:Write('4.0');
  247:Write('-4.0');

  251:Write('VCCZ');
  252:Write('EXECZ');
  253:Write('SCC');
  254:Write('LDS_DIRECT');
  255:Write('#0x',HexStr(d2,8));
  else
      Write('?');
 end;
end;

procedure _print_ssrc9(SSRC:Word);
begin
 Case SSRC of
  0..103:Write('s',SSRC);
  106:Write('VCC_LO');
  107:Write('VCC_HI');
  124:Write('M0');
  126:Write('EXEC_LO');
  127:Write('EXEC_HI');

  128..192:Write(SSRC-128);
  193..208:Write(-(SSRC-192));
  240:Write('0.5');
  241:Write('-0.5');
  242:Write('1.0');
  243:Write('-1.0');
  244:Write('2.0');
  245:Write('-2.0');
  246:Write('4.0');
  247:Write('-4.0');

  251:Write('VCCZ');
  252:Write('EXECZ');
  253:Write('SCC');
  254:Write('LDS_DIRECT');

  256..511:
      Write('v',SSRC-256);

  else
      Write('?');
 end;
end;

procedure _print_ssrc9(SSRC:Word;d2:DWORD);
begin
 Case SSRC of
  0..103:Write('s',SSRC);
  106:Write('VCC_LO');
  107:Write('VCC_HI');
  124:Write('M0');
  126:Write('EXEC_LO');
  127:Write('EXEC_HI');

  128..192:Write(SSRC-128);
  193..208:Write(-(SSRC-192));
  240:Write('0.5');
  241:Write('-0.5');
  242:Write('1.0');
  243:Write('-1.0');
  244:Write('2.0');
  245:Write('-2.0');
  246:Write('4.0');
  247:Write('-4.0');

  251:Write('VCCZ');
  252:Write('EXECZ');
  253:Write('SCC');
  254:Write('LDS_DIRECT');
  255:Write('#0x',HexStr(d2,8));
  256..511:
      Write('v',SSRC-256);

  else
      Write('?');
 end;
end;

procedure _print_vdst8(SDST:Byte); inline;
begin
 Write('v'+IntToStr(SDST));
end;

procedure _print_SOP2(Var SPI:TSPI);
begin
 Case SPI.SOP2.OP of

  S_ADD_U32       :Write('S_ADD_U32');
  S_SUB_U32       :Write('S_SUB_U32');
  S_ADD_I32       :Write('S_ADD_I32');
  S_SUB_I32       :Write('S_SUB_I32');
  S_ADDC_U32      :Write('S_ADDC_U32');
  S_SUBB_U32      :Write('S_SUBB_U32');
  S_MIN_I32       :Write('S_MIN_I32');
  S_MIN_U32       :Write('S_MIN_U32');
  S_MAX_I32       :Write('S_MAX_I32');
  S_MAX_U32       :Write('S_MAX_U32');
  S_CSELECT_B32   :Write('S_CSELECT_B32');
  S_CSELECT_B64   :Write('S_CSELECT_B64');

  S_AND_B32       :Write('S_AND_B32');
  S_AND_B64       :Write('S_AND_B64');
  S_OR_B32        :Write('S_OR_B32');
  S_OR_B64        :Write('S_OR_B64');
  S_XOR_B32       :Write('S_XOR_B32');
  S_XOR_B64       :Write('S_XOR_B64');
  S_ANDN2_B32     :Write('S_ANDN2_B32');
  S_ANDN2_B64     :Write('S_ANDN2_B64');
  S_ORN2_B32      :Write('S_ORN2_B32');
  S_ORN2_B64      :Write('S_ORN2_B64');
  S_NAND_B32      :Write('S_NAND_B32');
  S_NAND_B64      :Write('S_NAND_B64');
  S_NOR_B32       :Write('S_NOR_B32');
  S_NOR_B64       :Write('S_NOR_B64');
  S_XNOR_B32      :Write('S_XNOR_B32');
  S_XNOR_B64      :Write('S_XNOR_B64');
  S_LSHL_B32      :Write('S_LSHL_B32');
  S_LSHL_B64      :Write('S_LSHL_B64');
  S_LSHR_B32      :Write('S_LSHR_B32');
  S_LSHR_B64      :Write('S_LSHR_B64');
  S_ASHR_I32      :Write('S_ASHR_I32');
  S_ASHR_I64      :Write('S_ASHR_I64');
  S_BFM_B32       :Write('S_BFM_B32');
  S_BFM_B64       :Write('S_BFM_B64');
  S_MUL_I32       :Write('S_MUL_I32');
  S_BFE_U32       :Write('S_BFE_U32');
  S_BFE_I32       :Write('S_BFE_I32');
  S_BFE_U64       :Write('S_BFE_U64');
  S_BFE_I64       :Write('S_BFE_I64');
  S_CBRANCH_G_FORK:Write('S_CBRANCH_G_FORK');
  S_ABSDIFF_I32   :Write('S_ABSDIFF_I32');

  else
      Write('SOP2?',SPI.SOP2.OP);
 end;
 Write(' ');

 _print_sdst7(SPI.SOP2.SDST);
 Write(', ');
 _print_ssrc8(SPI.SOP2.SSRC0,SPI.INLINE32);
 Write(', ');
 _print_ssrc8(SPI.SOP2.SSRC1,SPI.INLINE32);

 Writeln;
end;

procedure _print_SOP1(Var SPI:TSPI);
begin
 Case SPI.SOP1.OP of
  S_MOV_B32           :Write('S_MOV_B32');
  S_MOV_B64           :Write('S_MOV_B64');
  S_CMOV_B32          :Write('S_CMOV_B32');
  S_CMOV_B64          :Write('S_CMOV_B64');
  S_NOT_B32           :Write('S_NOT_B32');
  S_NOT_B64           :Write('S_NOT_B64');
  S_WQM_B32           :Write('S_WQM_B32');
  S_WQM_B64           :Write('S_WQM_B64');
  S_BREV_B32          :Write('S_BREV_B32');
  S_BREV_B64          :Write('S_BREV_B64');
  S_BCNT0_I32_B32     :Write('S_BCNT0_I32_B32');
  S_BCNT0_I32_B64     :Write('S_BCNT0_I32_B64');
  S_BCNT1_I32_B32     :Write('S_BCNT1_I32_B32');
  S_BCNT1_I32_B64     :Write('S_BCNT1_I32_B64');
  S_FF0_I32_B32       :Write('S_FF0_I32_B32');
  S_FF0_I32_B64       :Write('S_FF0_I32_B64');
  S_FF1_I32_B32       :Write('S_FF1_I32_B32');
  S_FF1_I32_B64       :Write('S_FF1_I32_B64');
  S_FLBIT_I32_B32     :Write('S_FLBIT_I32_B32');
  S_FLBIT_I32_B64     :Write('S_FLBIT_I32_B64');
  S_FLBIT_I32         :Write('S_FLBIT_I32');
  S_FLBIT_I32_I64     :Write('S_FLBIT_I32_I64');
  S_SEXT_I32_I8       :Write('S_SEXT_I32_I8');
  S_SEXT_I32_I16      :Write('S_SEXT_I32_I16');
  S_BITSET0_B32       :Write('S_BITSET0_B32');
  S_BITSET0_B64       :Write('S_BITSET0_B64');
  S_BITSET1_B32       :Write('S_BITSET1_B32');
  S_BITSET1_B64       :Write('S_BITSET1_B64');
  S_GETPC_B64         :Write('S_GETPC_B64');
  S_SETPC_B64         :Write('S_SETPC_B64');
  S_SWAPPC_B64        :Write('S_SWAPPC_B64');

  S_AND_SAVEEXEC_B64  :Write('S_AND_SAVEEXEC_B64');
  S_OR_SAVEEXEC_B64   :Write('S_OR_SAVEEXEC_B64');
  S_XOR_SAVEEXEC_B64  :Write('S_XOR_SAVEEXEC_B64');
  S_ANDN2_SAVEEXEC_B64:Write('S_ANDN2_SAVEEXEC_B64');
  S_ORN2_SAVEEXEC_B64 :Write('S_ORN2_SAVEEXEC_B64');
  S_NAND_SAVEEXEC_B64 :Write('S_NAND_SAVEEXEC_B64');
  S_NOR_SAVEEXEC_B64  :Write('S_NOR_SAVEEXEC_B64');
  S_XNOR_SAVEEXEC_B64 :Write('S_XNOR_SAVEEXEC_B64');
  S_QUADMASK_B32      :Write('S_QUADMASK_B32');
  S_QUADMASK_B64      :Write('S_QUADMASK_B64');
  S_MOVRELS_B32       :Write('S_MOVRELS_B32');
  S_MOVRELS_B64       :Write('S_MOVRELS_B64');
  S_MOVRELD_B32       :Write('S_MOVRELD_B32');
  S_MOVRELD_B64       :Write('S_MOVRELD_B64');
  S_CBRANCH_JOIN      :Write('S_CBRANCH_JOIN');

  S_ABS_I32           :Write('S_ABS_I32');


  else
      Write('SOP1?',SPI.SOP1.OP);
 end;
 Write(' ');

 _print_sdst7(SPI.SOP1.SDST);
 Write(', ');
 _print_ssrc8(SPI.SOP1.SSRC,SPI.INLINE32);

 Writeln;
end;

procedure _print_SOPC(Var SPI:TSPI);
begin
 Case SPI.SOPC.OP of
  S_CMP_EQ_I32 :Write('S_CMP_EQ_I32');
  S_CMP_LG_I32 :Write('S_CMP_LG_I32');
  S_CMP_GT_I32 :Write('S_CMP_GT_I32');
  S_CMP_GE_I32 :Write('S_CMP_GE_I32');
  S_CMP_LT_I32 :Write('S_CMP_LT_I32');
  S_CMP_LE_I32 :Write('S_CMP_LE_I32');
  S_CMP_EQ_U32 :Write('S_CMP_EQ_U32');
  S_CMP_LG_U32 :Write('S_CMP_LG_U32');
  S_CMP_GT_U32 :Write('S_CMP_GT_U32');
  S_CMP_GE_U32 :Write('S_CMP_GE_U32');
  S_CMP_LT_U32 :Write('S_CMP_LT_U32');
  S_CMP_LE_U32 :Write('S_CMP_LE_U32');
  S_BITCMP0_B32:Write('S_BITCMP0_B32');
  S_BITCMP1_B32:Write('S_BITCMP1_B32');
  S_BITCMP0_B64:Write('S_BITCMP0_B64');
  S_BITCMP1_B64:Write('S_BITCMP1_B64');
  S_SETVSKIP   :Write('S_SETVSKIP');
  else
      Write('SOPC?',SPI.SOPC.OP);
 end;
 Write(' ');

 _print_ssrc8(SPI.SOPC.SSRC0,SPI.INLINE32);
 Write(', ');
 _print_ssrc8(SPI.SOPC.SSRC1,SPI.INLINE32);

 Writeln;
end;

function _text_branch_offset(OFFSET_DW:DWORD;S:SmallInt):RawByteString;
begin
 Result:='#0x'+HexStr((OFFSET_DW+S+1)*4,8);
end;

procedure _print_SOPP(Var SPI:TSPI);
begin
 Case SPI.SOPP.OP of
  S_NOP:Writeln('S_NOP');
  S_ENDPGM:Writeln('S_ENDPGM');

  S_BRANCH        :Writeln('S_BRANCH '        ,_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));
  S_CBRANCH_SCC0  :Writeln('S_CBRANCH_SCC0 '  ,_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));
  S_CBRANCH_SCC1  :Writeln('S_CBRANCH_SCC1 '  ,_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));
  S_CBRANCH_VCCZ  :Writeln('S_CBRANCH_VCCZ '  ,_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));
  S_CBRANCH_VCCNZ :Writeln('S_CBRANCH_VCCNZ ' ,_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));
  S_CBRANCH_EXECZ :Writeln('S_CBRANCH_EXECZ ' ,_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));
  S_CBRANCH_EXECNZ:Writeln('S_CBRANCH_EXECNZ ',_text_branch_offset(SPI.OFFSET_DW,SPI.SOPP.SIMM));

  S_BARRIER:Writeln('S_BARRIER');

  S_WAITCNT:
    begin
     Write('S_WAITCNT ');
     if Twaitcnt_simm(SPI.SOPP.SIMM).lgkmcnt<>15 then
      Write('lgkmcnt(',Twaitcnt_simm(SPI.SOPP.SIMM).lgkmcnt,') ');
     if Twaitcnt_simm(SPI.SOPP.SIMM).expcnt<>7 then
      Write('expcnt(',Twaitcnt_simm(SPI.SOPP.SIMM).expcnt,') ');
     if Twaitcnt_simm(SPI.SOPP.SIMM).vmcnt<>15 then
      Write('vmcnt(',Twaitcnt_simm(SPI.SOPP.SIMM).vmcnt,')');
     Writeln;
    end;

  S_SLEEP         :Writeln('S_SLEEP');
  S_SETPRIO       :Writeln('S_SETPRIO');
  S_SENDMSG       :Writeln('S_SENDMSG');

  S_ICACHE_INV    :Writeln('S_ICACHE_INV');
  S_INCPERFLEVEL  :Writeln('S_INCPERFLEVEL');
  S_DECPERFLEVEL  :Writeln('S_DECPERFLEVEL');
  S_TTRACEDATA    :Writeln('S_TTRACEDATA');

  else
      Writeln('SOPP?',SPI.SOPP.OP);
 end;
end;

procedure _print_SMRD(Var SPI:TSPI);
var
 t1,t2:Byte;
begin
 t1:=0;
 t2:=0;

 Case SPI.SMRD.OP of

  S_LOAD_DWORD:
    begin
     Write('S_LOAD_DWORD');
     t1:=1;
     t2:=2;
    end;

  S_LOAD_DWORDX2:
    begin
     Write('S_LOAD_DWORDX2');
     t1:=2;
     t2:=2;
    end;

  S_LOAD_DWORDX4:
    begin
     Write('S_LOAD_DWORDX4');
     t1:=4;
     t2:=2;
    end;

   S_LOAD_DWORDX8:
    begin
     Write('S_LOAD_DWORDX8');
     t1:=8;
     t2:=2;
    end;

  S_LOAD_DWORDX16:
    begin
     Write('S_LOAD_DWORDX16');
     t1:=16;
     t2:=2;
    end;

  //--

  S_BUFFER_LOAD_DWORD:
    begin
     Write('S_BUFFER_LOAD_DWORD');
     t1:=1;
     t2:=4;
    end;

  S_BUFFER_LOAD_DWORDX2:
    begin
     Write('S_BUFFER_LOAD_DWORDX2');
     t1:=2;
     t2:=4;
    end;

  S_BUFFER_LOAD_DWORDX4:
    begin
     Write('S_BUFFER_LOAD_DWORDX4');
     t1:=4;
     t2:=4;
    end;

  S_BUFFER_LOAD_DWORDX8:
    begin
     Write('S_BUFFER_LOAD_DWORDX8');
     t1:=8;
     t2:=4;
    end;

  S_BUFFER_LOAD_DWORDX16:
    begin
     Write('S_BUFFER_LOAD_DWORDX16');
     t1:=16;
     t2:=4;
    end;

  S_MEMTIME:
    begin
     Write('S_MEMTIME');
     Write(' ');
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+1,']');
     Writeln;
     Exit;
    end;
  S_DCACHE_INV:
   begin
    Writeln('S_DCACHE_INV');
    Exit;
   end

  else
      Write('SMRD?',SPI.SMRD.OP);
 end;
 Write(' ');


 case t1 of
  1:begin
     With SPI.SMRD do
      Write('s[',SDST,'], ');
    end;
  2:begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+1,'], ');
    end;
  4:begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+3,'], ');
    end;
  8:begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+7,'], ');
    end;
  16:
    begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+15,'], ');
    end;
 end;

 case t2 of
  2:begin
     With SPI.SMRD do
      Write('s[',SBASE*2,':',SBASE*2+1,'], ');
    end;
  4:begin
     With SPI.SMRD do
      Write('s[',SBASE*2,':',SBASE*2+3,'], ');
    end;
 end;

 With SPI.SMRD do
  Case IMM of
   0:_print_ssrc8(OFFSET,SPI.INLINE32);
   1:Write('0x',HexStr(OFFSET,2));
  end;

 Writeln;
end;

procedure _print_SOPK(Var SPI:TSPI);
begin
 Case SPI.SOPK.OP of

  S_MOVK_I32        :Write('S_MOVK_I32');
  S_MOVK_HI_I32     :Write('S_MOVK_HI_I32');
  S_CMOVK_I32       :Write('S_CMOVK_I32');
  S_CMPK_EQ_I32     :Write('S_CMPK_EQ_I32');
  S_CMPK_LG_I32     :Write('S_CMPK_LG_I32');
  S_CMPK_GT_I32     :Write('S_CMPK_GT_I32');
  S_CMPK_GE_I32     :Write('S_CMPK_GE_I32');
  S_CMPK_LT_I32     :Write('S_CMPK_LT_I32');
  S_CMPK_LE_I32     :Write('S_CMPK_LE_I32');
  S_CMPK_EQ_U32     :Write('S_CMPK_EQ_U32');
  S_CMPK_LG_U32     :Write('S_CMPK_LG_U32');
  S_CMPK_GT_U32     :Write('S_CMPK_GT_U32');
  S_CMPK_GE_U32     :Write('S_CMPK_GE_U32');
  S_CMPK_LT_U32     :Write('S_CMPK_LT_U32');
  S_CMPK_LE_U32     :Write('S_CMPK_LE_U32');
  S_ADDK_I32        :Write('S_ADDK_I32');
  S_MULK_I32        :Write('S_MULK_I32');
  S_CBRANCH_I_FORK  :Write('S_CBRANCH_I_FORK');
  S_GETREG_B32      :Write('S_GETREG_B32');
  S_SETREG_B32      :Write('S_SETREG_B32');
  S_SETREG_IMM32_B32:Write('S_SETREG_IMM32_B32');

  else
      Write('SOPK?',SPI.SOPK.OP);
 end;
 Write(' ');
 _print_sdst7(SPI.SOPK.SDST);
 Write(', ');

 Case SPI.SOPK.OP of
  S_MOVK_I32   ,
  S_MOVK_HI_I32,
  S_CMOVK_I32  ,
  S_CMPK_EQ_I32,
  S_CMPK_LG_I32,
  S_CMPK_GT_I32,
  S_CMPK_GE_I32,
  S_CMPK_LT_I32,
  S_CMPK_LE_I32,
  S_ADDK_I32   ,
  S_MULK_I32   :Write(SmallInt(SPI.SOPK.SIMM));

  S_CMPK_EQ_U32,
  S_CMPK_LG_U32,
  S_CMPK_GT_U32,
  S_CMPK_GE_U32,
  S_CMPK_LT_U32,
  S_CMPK_LE_U32:Write(SPI.SOPK.SIMM);

  else
                Write('#0x',HexStr(SPI.SOPK.SIMM,8));
 end;

 Writeln;
end;

procedure _print_VOP3c(Var VOP3:TVOP3a);
begin
 Case VOP3.OP of

  V_CMP_F_F32     :Write('V_CMP_F_F32');
  V_CMP_LT_F32    :Write('V_CMP_LT_F32');
  V_CMP_EQ_F32    :Write('V_CMP_EQ_F32');
  V_CMP_LE_F32    :Write('V_CMP_LE_F32');
  V_CMP_GT_F32    :Write('V_CMP_GT_F32');
  V_CMP_LG_F32    :Write('V_CMP_LG_F32');
  V_CMP_GE_F32    :Write('V_CMP_GE_F32');
  V_CMP_O_F32     :Write('V_CMP_O_F32');
  V_CMP_U_F32     :Write('V_CMP_U_F32');
  V_CMP_NGE_F32   :Write('V_CMP_NGE_F32');
  V_CMP_NLG_F32   :Write('V_CMP_NLG_F32');
  V_CMP_NGT_F32   :Write('V_CMP_NGT_F32');
  V_CMP_NLE_F32   :Write('V_CMP_NLE_F32');
  V_CMP_NEQ_F32   :Write('V_CMP_NEQ_F32');
  V_CMP_NLT_F32   :Write('V_CMP_NLT_F32');
  V_CMP_T_F32     :Write('V_CMP_T_F32');

  V_CMPX_F_F32    :Write('V_CMPX_F_F32');
  V_CMPX_LT_F32   :Write('V_CMPX_LT_F32');
  V_CMPX_EQ_F32   :Write('V_CMPX_EQ_F32');
  V_CMPX_LE_F32   :Write('V_CMPX_LE_F32');
  V_CMPX_GT_F32   :Write('V_CMPX_GT_F32');
  V_CMPX_LG_F32   :Write('V_CMPX_LG_F32');
  V_CMPX_GE_F32   :Write('V_CMPX_GE_F32');
  V_CMPX_O_F32    :Write('V_CMPX_O_F32');
  V_CMPX_U_F32    :Write('V_CMPX_U_F32');
  V_CMPX_NGE_F32  :Write('V_CMPX_NGE_F32');
  V_CMPX_NLG_F32  :Write('V_CMPX_NLG_F32');
  V_CMPX_NGT_F32  :Write('V_CMPX_NGT_F32');
  V_CMPX_NLE_F32  :Write('V_CMPX_NLE_F32');
  V_CMPX_NEQ_F32  :Write('V_CMPX_NEQ_F32');
  V_CMPX_NLT_F32  :Write('V_CMPX_NLT_F32');
  V_CMPX_T_F32    :Write('V_CMPX_T_F32');

  V_CMP_F_F64     :Write('V_CMP_F_F64');
  V_CMP_LT_F64    :Write('V_CMP_LT_F64');
  V_CMP_EQ_F64    :Write('V_CMP_EQ_F64');
  V_CMP_LE_F64    :Write('V_CMP_LE_F64');
  V_CMP_GT_F64    :Write('V_CMP_GT_F64');
  V_CMP_LG_F64    :Write('V_CMP_LG_F64');
  V_CMP_GE_F64    :Write('V_CMP_GE_F64');
  V_CMP_O_F64     :Write('V_CMP_O_F64');
  V_CMP_U_F64     :Write('V_CMP_U_F64');
  V_CMP_NGE_F64   :Write('V_CMP_NGE_F64');
  V_CMP_NLG_F64   :Write('V_CMP_NLG_F64');
  V_CMP_NGT_F64   :Write('V_CMP_NGT_F64');
  V_CMP_NLE_F64   :Write('V_CMP_NLE_F64');
  V_CMP_NEQ_F64   :Write('V_CMP_NEQ_F64');
  V_CMP_NLT_F64   :Write('V_CMP_NLT_F64');
  V_CMP_T_F64     :Write('V_CMP_T_F64');

  V_CMPX_F_F64    :Write('V_CMPX_F_F64');
  V_CMPX_LT_F64   :Write('V_CMPX_LT_F64');
  V_CMPX_EQ_F64   :Write('V_CMPX_EQ_F64');
  V_CMPX_LE_F64   :Write('V_CMPX_LE_F64');
  V_CMPX_GT_F64   :Write('V_CMPX_GT_F64');
  V_CMPX_LG_F64   :Write('V_CMPX_LG_F64');
  V_CMPX_GE_F64   :Write('V_CMPX_GE_F64');
  V_CMPX_O_F64    :Write('V_CMPX_O_F64');
  V_CMPX_U_F64    :Write('V_CMPX_U_F64');
  V_CMPX_NGE_F64  :Write('V_CMPX_NGE_F64');
  V_CMPX_NLG_F64  :Write('V_CMPX_NLG_F64');
  V_CMPX_NGT_F64  :Write('V_CMPX_NGT_F64');
  V_CMPX_NLE_F64  :Write('V_CMPX_NLE_F64');
  V_CMPX_NEQ_F64  :Write('V_CMPX_NEQ_F64');
  V_CMPX_NLT_F64  :Write('V_CMPX_NLT_F64');
  V_CMPX_T_F64    :Write('V_CMPX_T_F64');


  V_CMPS_F_F32    :Write('V_CMPS_F_F32');
  V_CMPS_LT_F32   :Write('V_CMPS_LT_F32');
  V_CMPS_EQ_F32   :Write('V_CMPS_EQ_F32');
  V_CMPS_LE_F32   :Write('V_CMPS_LE_F32');
  V_CMPS_GT_F32   :Write('V_CMPS_GT_F32');
  V_CMPS_LG_F32   :Write('V_CMPS_LG_F32');
  V_CMPS_GE_F32   :Write('V_CMPS_GE_F32');
  V_CMPS_O_F32    :Write('V_CMPS_O_F32');
  V_CMPS_U_F32    :Write('V_CMPS_U_F32');
  V_CMPS_NGE_F32  :Write('V_CMPS_NGE_F32');
  V_CMPS_NLG_F32  :Write('V_CMPS_NLG_F32');
  V_CMPS_NGT_F32  :Write('V_CMPS_NGT_F32');
  V_CMPS_NLE_F32  :Write('V_CMPS_NLE_F32');
  V_CMPS_NEQ_F32  :Write('V_CMPS_NEQ_F32');
  V_CMPS_NLT_F32  :Write('V_CMPS_NLT_F32');
  V_CMPS_T_F32    :Write('V_CMPS_T_F32');

  V_CMPSX_F_F32   :Write('V_CMPSX_F_F32');
  V_CMPSX_LT_F32  :Write('V_CMPSX_LT_F32');
  V_CMPSX_EQ_F32  :Write('V_CMPSX_EQ_F32');
  V_CMPSX_LE_F32  :Write('V_CMPSX_LE_F32');
  V_CMPSX_GT_F32  :Write('V_CMPSX_GT_F32');
  V_CMPSX_LG_F32  :Write('V_CMPSX_LG_F32');
  V_CMPSX_GE_F32  :Write('V_CMPSX_GE_F32');
  V_CMPSX_O_F32   :Write('V_CMPSX_O_F32');
  V_CMPSX_U_F32   :Write('V_CMPSX_U_F32');
  V_CMPSX_NGE_F32 :Write('V_CMPSX_NGE_F32');
  V_CMPSX_NLG_F32 :Write('V_CMPSX_NLG_F32');
  V_CMPSX_NGT_F32 :Write('V_CMPSX_NGT_F32');
  V_CMPSX_NLE_F32 :Write('V_CMPSX_NLE_F32');
  V_CMPSX_NEQ_F32 :Write('V_CMPSX_NEQ_F32');
  V_CMPSX_NLT_F32 :Write('V_CMPSX_NLT_F32');
  V_CMPSX_T_F32   :Write('V_CMPSX_T_F32');

  V_CMPS_F_F64    :Write('V_CMPS_F_F64');
  V_CMPS_LT_F64   :Write('V_CMPS_LT_F64');
  V_CMPS_EQ_F64   :Write('V_CMPS_EQ_F64');
  V_CMPS_LE_F64   :Write('V_CMPS_LE_F64');
  V_CMPS_GT_F64   :Write('V_CMPS_GT_F64');
  V_CMPS_LG_F64   :Write('V_CMPS_LG_F64');
  V_CMPS_GE_F64   :Write('V_CMPS_GE_F64');
  V_CMPS_O_F64    :Write('V_CMPS_O_F64');
  V_CMPS_U_F64    :Write('V_CMPS_U_F64');
  V_CMPS_NGE_F64  :Write('V_CMPS_NGE_F64');
  V_CMPS_NLG_F64  :Write('V_CMPS_NLG_F64');
  V_CMPS_NGT_F64  :Write('V_CMPS_NGT_F64');
  V_CMPS_NLE_F64  :Write('V_CMPS_NLE_F64');
  V_CMPS_NEQ_F64  :Write('V_CMPS_NEQ_F64');
  V_CMPS_NLT_F64  :Write('V_CMPS_NLT_F64');
  V_CMPS_T_F64    :Write('V_CMPS_T_F64');

  V_CMPSX_F_F64   :Write('V_CMPSX_F_F64');
  V_CMPSX_LT_F64  :Write('V_CMPSX_LT_F64');
  V_CMPSX_EQ_F64  :Write('V_CMPSX_EQ_F64');
  V_CMPSX_LE_F64  :Write('V_CMPSX_LE_F64');
  V_CMPSX_GT_F64  :Write('V_CMPSX_GT_F64');
  V_CMPSX_LG_F64  :Write('V_CMPSX_LG_F64');
  V_CMPSX_GE_F64  :Write('V_CMPSX_GE_F64');
  V_CMPSX_O_F64   :Write('V_CMPSX_O_F64');
  V_CMPSX_U_F64   :Write('V_CMPSX_U_F64');
  V_CMPSX_NGE_F64 :Write('V_CMPSX_NGE_F64');
  V_CMPSX_NLG_F64 :Write('V_CMPSX_NLG_F64');
  V_CMPSX_NGT_F64 :Write('V_CMPSX_NGT_F64');
  V_CMPSX_NLE_F64 :Write('V_CMPSX_NLE_F64');
  V_CMPSX_NEQ_F64 :Write('V_CMPSX_NEQ_F64');
  V_CMPSX_NLT_F64 :Write('V_CMPSX_NLT_F64');
  V_CMPSX_T_F64   :Write('V_CMPSX_T_F64');


  V_CMP_F_I32     :Write('V_CMP_F_I32');
  V_CMP_LT_I32    :Write('V_CMP_LT_I32');
  V_CMP_EQ_I32    :Write('V_CMP_EQ_I32');
  V_CMP_LE_I32    :Write('V_CMP_LE_I32');
  V_CMP_GT_I32    :Write('V_CMP_GT_I32');
  V_CMP_LG_I32    :Write('V_CMP_LG_I32');
  V_CMP_GE_I32    :Write('V_CMP_GE_I32');
  V_CMP_T_I32     :Write('V_CMP_T_I32');

  V_CMPX_F_I32    :Write('V_CMPX_F_I32');
  V_CMPX_LT_I32   :Write('V_CMPX_LT_I32');
  V_CMPX_EQ_I32   :Write('V_CMPX_EQ_I32');
  V_CMPX_LE_I32   :Write('V_CMPX_LE_I32');
  V_CMPX_GT_I32   :Write('V_CMPX_GT_I32');
  V_CMPX_LG_I32   :Write('V_CMPX_LG_I32');
  V_CMPX_GE_I32   :Write('V_CMPX_GE_I32');
  V_CMPX_T_I32    :Write('V_CMPX_T_I32');

  V_CMP_F_I64     :Write('V_CMP_F_I64');
  V_CMP_LT_I64    :Write('V_CMP_LT_I64');
  V_CMP_EQ_I64    :Write('V_CMP_EQ_I64');
  V_CMP_LE_I64    :Write('V_CMP_LE_I64');
  V_CMP_GT_I64    :Write('V_CMP_GT_I64');
  V_CMP_LG_I64    :Write('V_CMP_LG_I64');
  V_CMP_GE_I64    :Write('V_CMP_GE_I64');
  V_CMP_T_I64     :Write('V_CMP_T_I64');

  V_CMPX_F_I64    :Write('V_CMPX_F_I64');
  V_CMPX_LT_I64   :Write('V_CMPX_LT_I64');
  V_CMPX_EQ_I64   :Write('V_CMPX_EQ_I64');
  V_CMPX_LE_I64   :Write('V_CMPX_LE_I64');
  V_CMPX_GT_I64   :Write('V_CMPX_GT_I64');
  V_CMPX_LG_I64   :Write('V_CMPX_LG_I64');
  V_CMPX_GE_I64   :Write('V_CMPX_GE_I64');
  V_CMPX_T_I64    :Write('V_CMPX_T_I64');

  V_CMP_F_U32     :Write('V_CMP_F_U32');
  V_CMP_LT_U32    :Write('V_CMP_LT_U32');
  V_CMP_EQ_U32    :Write('V_CMP_EQ_U32');
  V_CMP_LE_U32    :Write('V_CMP_LE_U32');
  V_CMP_GT_U32    :Write('V_CMP_GT_U32');
  V_CMP_LG_U32    :Write('V_CMP_LG_U32');
  V_CMP_GE_U32    :Write('V_CMP_GE_U32');
  V_CMP_T_U32     :Write('V_CMP_T_U32');

  V_CMPX_F_U32    :Write('V_CMPX_F_U32');
  V_CMPX_LT_U32   :Write('V_CMPX_LT_U32');
  V_CMPX_EQ_U32   :Write('V_CMPX_EQ_U32');
  V_CMPX_LE_U32   :Write('V_CMPX_LE_U32');
  V_CMPX_GT_U32   :Write('V_CMPX_GT_U32');
  V_CMPX_LG_U32   :Write('V_CMPX_LG_U32');
  V_CMPX_GE_U32   :Write('V_CMPX_GE_U32');
  V_CMPX_T_U32    :Write('V_CMPX_T_U32');

  V_CMP_F_U64     :Write('V_CMP_F_U64');
  V_CMP_LT_U64    :Write('V_CMP_LT_U64');
  V_CMP_EQ_U64    :Write('V_CMP_EQ_U64');
  V_CMP_LE_U64    :Write('V_CMP_LE_U64');
  V_CMP_GT_U64    :Write('V_CMP_GT_U64');
  V_CMP_LG_U64    :Write('V_CMP_LG_U64');
  V_CMP_GE_U64    :Write('V_CMP_GE_U64');
  V_CMP_T_U64     :Write('V_CMP_T_U64');

  V_CMPX_F_U64    :Write('V_CMPX_F_U64');
  V_CMPX_LT_U64   :Write('V_CMPX_LT_U64');
  V_CMPX_EQ_U64   :Write('V_CMPX_EQ_U64');
  V_CMPX_LE_U64   :Write('V_CMPX_LE_U64');
  V_CMPX_GT_U64   :Write('V_CMPX_GT_U64');
  V_CMPX_LG_U64   :Write('V_CMPX_LG_U64');
  V_CMPX_GE_U64   :Write('V_CMPX_GE_U64');
  V_CMPX_T_U64    :Write('V_CMPX_T_U64');

  V_CMP_CLASS_F32 :Write('V_CMP_CLASS_F32');
  V_CMPX_CLASS_F32:Write('V_CMPX_CLASS_F32');
  V_CMP_CLASS_F64 :Write('V_CMP_CLASS_F64');
  V_CMPX_CLASS_F64:Write('V_CMPX_CLASS_F64');

  else
      Write('VOP3c?',VOP3.OP);
 end;
 Write(' ');

 _print_sdst7(VOP3.VDST);

 Write(', ');
 if Byte(VOP3.NEG).TestBit(0) then Write('-');
 if Byte(VOP3.ABS).TestBit(0) then Write('abs(');
 _print_ssrc9(VOP3.SRC0);
 if Byte(VOP3.ABS).TestBit(0) then Write(')');
 Write(', ');
 if Byte(VOP3.NEG).TestBit(1) then Write('-');
 if Byte(VOP3.ABS).TestBit(1) then Write('abs(');
 _print_ssrc9(VOP3.SRC1);
 if Byte(VOP3.ABS).TestBit(1) then Write(')');

 Writeln(' ; VOP3c');
end;

procedure _print_VOP3a(Var VOP3:TVOP3a);
begin
 Case VOP3.OP of

  256+V_CNDMASK_B32       :Write('V_CNDMASK_B32');
  256+V_READLANE_B32      :Write('V_READLANE_B32');
  256+V_WRITELANE_B32     :Write('V_WRITELANE_B32');
  256+V_ADD_F32           :Write('V_ADD_F32');
  256+V_SUB_F32           :Write('V_SUB_F32');
  256+V_SUBREV_F32        :Write('V_SUBREV_F32');
  256+V_MAC_LEGACY_F32    :Write('V_MAC_LEGACY_F32');
  256+V_MUL_LEGACY_F32    :Write('V_MUL_LEGACY_F32');
  256+V_MUL_F32           :Write('V_MUL_F32');
  256+V_MUL_I32_I24       :Write('V_MUL_I32_I24');
  256+V_MUL_HI_I32_I24    :Write('V_MUL_HI_I32_I24');
  256+V_MUL_U32_U24       :Write('V_MUL_U32_U24');
  256+V_MUL_HI_U32_U24    :Write('V_MUL_HI_U32_U24');
  256+V_MIN_LEGACY_F32    :Write('V_MIN_LEGACY_F32');
  256+V_MAX_LEGACY_F32    :Write('V_MAX_LEGACY_F32');
  256+V_MIN_F32           :Write('V_MIN_F32');
  256+V_MAX_F32           :Write('V_MAX_F32');
  256+V_MIN_I32           :Write('V_MIN_I32');
  256+V_MAX_I32           :Write('V_MAX_I32');
  256+V_MIN_U32           :Write('V_MIN_U32');
  256+V_MAX_U32           :Write('V_MAX_U32');
  256+V_LSHR_B32          :Write('V_LSHR_B32');
  256+V_LSHRREV_B32       :Write('V_LSHRREV_B32');
  256+V_ASHR_I32          :Write('V_ASHR_I32');
  256+V_ASHRREV_I32       :Write('V_ASHRREV_I32');
  256+V_LSHL_B32          :Write('V_LSHL_B32');
  256+V_LSHLREV_B32       :Write('V_LSHLREV_B32');
  256+V_AND_B32           :Write('V_AND_B32');
  256+V_OR_B32            :Write('V_OR_B32');
  256+V_XOR_B32           :Write('V_XOR_B32');
  256+V_BFM_B32           :Write('V_BFM_B32');
  256+V_MAC_F32           :Write('V_MAC_F32');

  256+V_BCNT_U32_B32      :Write('V_BCNT_U32_B32');
  256+V_MBCNT_LO_U32_B32  :Write('V_MBCNT_LO_U32_B32');
  256+V_MBCNT_HI_U32_B32  :Write('V_MBCNT_HI_U32_B32');

  256+V_LDEXP_F32         :Write('V_LDEXP_F32');
  256+V_CVT_PKACCUM_U8_F32:Write('V_CVT_PKACCUM_U8_F32');
  256+V_CVT_PKNORM_I16_F32:Write('V_CVT_PKNORM_I16_F32');
  256+V_CVT_PKNORM_U16_F32:Write('V_CVT_PKNORM_U16_F32');
  256+V_CVT_PKRTZ_F16_F32 :Write('V_CVT_PKRTZ_F16_F32');
  256+V_CVT_PK_U16_U32    :Write('V_CVT_PK_U16_U32');
  256+V_CVT_PK_I16_I32    :Write('V_CVT_PK_I16_I32');

  V_MAD_LEGACY_F32        :Write('V_MAD_LEGACY_F32');
  V_MAD_F32               :Write('V_MAD_F32');
  V_MAD_I32_I24           :Write('V_MAD_I32_I24');
  V_MAD_U32_U24           :Write('V_MAD_U32_U24');
  V_CUBEID_F32            :Write('V_CUBEID_F32');
  V_CUBESC_F32            :Write('V_CUBESC_F32');
  V_CUBETC_F32            :Write('V_CUBETC_F32');
  V_CUBEMA_F32            :Write('V_CUBEMA_F32');
  V_BFE_U32               :Write('V_BFE_U32');
  V_BFE_I32               :Write('V_BFE_I32');
  V_BFI_B32               :Write('V_BFI_B32');
  V_FMA_F32               :Write('V_FMA_F32');
  V_FMA_F64               :Write('V_FMA_F64');
  V_LERP_U8               :Write('V_LERP_U8');
  V_ALIGNBIT_B32          :Write('V_ALIGNBIT_B32');
  V_ALIGNBYTE_B32         :Write('V_ALIGNBYTE_B32');
  V_MULLIT_F32            :Write('V_MULLIT_F32');
  V_MIN3_F32              :Write('V_MIN3_F32');
  V_MIN3_I32              :Write('V_MIN3_I32');
  V_MIN3_U32              :Write('V_MIN3_U32');
  V_MAX3_F32              :Write('V_MAX3_F32');
  V_MAX3_I32              :Write('V_MAX3_I32');
  V_MAX3_U32              :Write('V_MAX3_U32');
  V_MED3_F32              :Write('V_MED3_F32');
  V_MED3_I32              :Write('V_MED3_I32');
  V_MED3_U32              :Write('V_MED3_U32');
  V_SAD_U8                :Write('V_SAD_U8');
  V_SAD_HI_U8             :Write('V_SAD_HI_U8');
  V_SAD_U16               :Write('V_SAD_U16');
  V_SAD_U32               :Write('V_SAD_U32');
  V_CVT_PK_U8_F32         :Write('V_CVT_PK_U8_F32');
  V_DIV_FIXUP_F32         :Write('V_DIV_FIXUP_F32');
  V_DIV_FIXUP_F64         :Write('V_DIV_FIXUP_F64');
  V_LSHL_B64              :Write('V_LSHL_B64');
  V_LSHR_B64              :Write('V_LSHR_B64');
  V_ASHR_I64              :Write('V_ASHR_I64');
  V_ADD_F64               :Write('V_ADD_F64');
  V_MUL_F64               :Write('V_MUL_F64');
  V_MIN_F64               :Write('V_MIN_F64');
  V_MAX_F64               :Write('V_MAX_F64');
  V_LDEXP_F64             :Write('V_LDEXP_F64');
  V_MUL_LO_U32            :Write('V_MUL_LO_U32');
  V_MUL_HI_U32            :Write('V_MUL_HI_U32');
  V_MUL_LO_I32            :Write('V_MUL_LO_I32');
  V_MUL_HI_I32            :Write('V_MUL_HI_I32');

  V_DIV_FMAS_F32          :Write('V_DIV_FMAS_F32');
  V_DIV_FMAS_F64          :Write('V_DIV_FMAS_F64');
  V_MSAD_U8               :Write('V_MSAD_U8');
  V_QSAD_PK_U16_U8        :Write('V_QSAD_PK_U16_U8');
  V_MQSAD_PK_U16_U8       :Write('V_MQSAD_PK_U16_U8');
  V_TRIG_PREOP_F64        :Write('V_TRIG_PREOP_F64');
  V_MQSAD_U32_U8          :Write('V_MQSAD_U32_U8');
  V_MAD_U64_U32           :Write('V_MAD_U64_U32');
  V_MAD_I64_I32           :Write('V_MAD_I64_I32');

  384+V_NOP               :Write('V_NOP');
  384+V_MOV_B32           :Write('V_MOV_B32');
  384+V_READFIRSTLANE_B32 :Write('V_READFIRSTLANE_B32');
  384+V_CVT_I32_F64       :Write('V_CVT_I32_F64');
  384+V_CVT_F64_I32       :Write('V_CVT_F64_I32');
  384+V_CVT_F32_I32       :Write('V_CVT_F32_I32');
  384+V_CVT_F32_U32       :Write('V_CVT_F32_U32');
  384+V_CVT_U32_F32       :Write('V_CVT_U32_F32');
  384+V_CVT_I32_F32       :Write('V_CVT_I32_F32');
  384+V_MOV_FED_B32       :Write('V_MOV_FED_B32');
  384+V_CVT_F16_F32       :Write('V_CVT_F16_F32');
  384+V_CVT_F32_F16       :Write('V_CVT_F32_F16');
  384+V_CVT_RPI_I32_F32   :Write('V_CVT_RPI_I32_F32');
  384+V_CVT_FLR_I32_F32   :Write('V_CVT_FLR_I32_F32');
  384+V_CVT_OFF_F32_I4    :Write('V_CVT_OFF_F32_I4');
  384+V_CVT_F32_F64       :Write('V_CVT_F32_F64');
  384+V_CVT_F64_F32       :Write('V_CVT_F64_F32');
  384+V_CVT_F32_UBYTE0    :Write('V_CVT_F32_UBYTE0');
  384+V_CVT_F32_UBYTE1    :Write('V_CVT_F32_UBYTE1');
  384+V_CVT_F32_UBYTE2    :Write('V_CVT_F32_UBYTE2');
  384+V_CVT_F32_UBYTE3    :Write('V_CVT_F32_UBYTE3');
  384+V_CVT_U32_F64       :Write('V_CVT_U32_F64');
  384+V_CVT_F64_U32       :Write('V_CVT_F64_U32');
  384+V_TRUNC_F64         :Write('V_TRUNC_F64');
  384+V_CEIL_F64          :Write('V_CEIL_F64');
  384+V_RNDNE_F64         :Write('V_RNDNE_F64');
  384+V_FLOOR_F64         :Write('V_FLOOR_F64');
  384+V_FRACT_F32         :Write('V_FRACT_F32');
  384+V_TRUNC_F32         :Write('V_TRUNC_F32');
  384+V_CEIL_F32          :Write('V_CEIL_F32');
  384+V_RNDNE_F32         :Write('V_RNDNE_F32');
  384+V_FLOOR_F32         :Write('V_FLOOR_F32');
  384+V_EXP_F32           :Write('V_EXP_F32');
  384+V_LOG_CLAMP_F32     :Write('V_LOG_CLAMP_F32');
  384+V_LOG_F32           :Write('V_LOG_F32');
  384+V_RCP_CLAMP_F32     :Write('V_RCP_CLAMP_F32');
  384+V_RCP_LEGACY_F32    :Write('V_RCP_LEGACY_F32');
  384+V_RCP_F32           :Write('V_RCP_F32');
  384+V_RCP_IFLAG_F32     :Write('V_RCP_IFLAG_F32');
  384+V_RSQ_CLAMP_F32     :Write('V_RSQ_CLAMP_F32');
  384+V_RSQ_LEGACY_F32    :Write('V_RSQ_LEGACY_F32');
  384+V_RSQ_F32           :Write('V_RSQ_F32');
  384+V_RCP_F64           :Write('V_RCP_F64');
  384+V_RCP_CLAMP_F64     :Write('V_RCP_CLAMP_F64');
  384+V_RSQ_F64           :Write('V_RSQ_F64');
  384+V_RSQ_CLAMP_F64     :Write('V_RSQ_CLAMP_F64');
  384+V_SQRT_F32          :Write('V_SQRT_F32');
  384+V_SQRT_F64          :Write('V_SQRT_F64');
  384+V_SIN_F32           :Write('V_SIN_F32');
  384+V_COS_F32           :Write('V_COS_F32');
  384+V_NOT_B32           :Write('V_NOT_B32');
  384+V_BFREV_B32         :Write('V_BFREV_B32');
  384+V_FFBH_U32          :Write('V_FFBH_U32');
  384+V_FFBL_B32          :Write('V_FFBL_B32');
  384+V_FFBH_I32          :Write('V_FFBH_I32');
  384+V_FREXP_EXP_I32_F64 :Write('V_FREXP_EXP_I32_F64');
  384+V_FREXP_MANT_F64    :Write('V_FREXP_MANT_F64');
  384+V_FRACT_F64         :Write('V_FRACT_F64');
  384+V_FREXP_EXP_I32_F32 :Write('V_FREXP_EXP_I32_F32');
  384+V_FREXP_MANT_F32    :Write('V_FREXP_MANT_F32');
  384+V_CLREXCP           :Write('V_CLREXCP');
  384+V_MOVRELD_B32       :Write('V_MOVRELD_B32');
  384+V_MOVRELS_B32       :Write('V_MOVRELS_B32');
  384+V_MOVRELSD_B32      :Write('V_MOVRELSD_B32');

  else
      Write('VOP3a?',VOP3.OP);
 end;
 Write(' ');

 _print_vdst8(VOP3.VDST);

 Case VOP3.OMOD of
  0:;
  1:Write('*2');
  2:Write('*4');
  3:Write('/2');
 end;

 Write(', ');
 if Byte(VOP3.NEG).TestBit(0) then Write('-');
 if Byte(VOP3.ABS).TestBit(0) then Write('abs(');
 _print_ssrc9(VOP3.SRC0);
 if Byte(VOP3.ABS).TestBit(0) then Write(')');

 Case VOP3.OP of
  384+V_NOP..384+V_MOVRELSD_B32:;
  else
   begin
    Write(', ');
    if Byte(VOP3.NEG).TestBit(1) then Write('-');
    if Byte(VOP3.ABS).TestBit(1) then Write('abs(');
    _print_ssrc9(VOP3.SRC1);
    if Byte(VOP3.ABS).TestBit(1) then Write(')');
   end;
 end;

 Case VOP3.OP of
  V_MAD_LEGACY_F32..V_DIV_FIXUP_F64,
  V_DIV_FMAS_F32..V_MAD_I64_I32:
    begin
     Write(', ');
     if Byte(VOP3.NEG).TestBit(2) then Write('-');
     if Byte(VOP3.ABS).TestBit(2) then Write('abs(');
     _print_ssrc9(VOP3.SRC2);
     if Byte(VOP3.ABS).TestBit(2) then Write(')');
    end;
  else;
 end;

 if (VOP3.CLAMP<>0) then
  Write(' clamp');

 Writeln(' ; VOP3a');
end;

procedure _print_VOP3b(Var VOP3:TVOP3b);
begin
 Case VOP3.OP of

  256+V_ADD_I32    :Write('V_ADD_I32');
  256+V_SUB_I32    :Write('V_SUB_I32');
  256+V_SUBREV_I32 :Write('V_SUBREV_I32');
  256+V_ADDC_U32   :Write('V_ADDC_U32');
  256+V_SUBB_U32   :Write('V_SUBB_U32');
  256+V_SUBBREV_U32:Write('V_SUBBREV_U32');

  V_DIV_SCALE_F32  :Write('V_DIV_SCALE_F32');
  V_DIV_SCALE_F64  :Write('V_DIV_SCALE_F64');

  else
      Write('VOP3b?',VOP3.OP);
 end;
 Write(' ');

 _print_vdst8(VOP3.VDST);

 Case VOP3.OMOD of
  0:;
  1:Write('*2');
  2:Write('*4');
  3:Write('/2');
 end;

 Write(', ');
 _print_sdst7(VOP3.SDST);

 Write(', ');
 if Byte(VOP3.NEG).TestBit(0) then Write('-');
 _print_ssrc9(VOP3.SRC0);
 Write(', ');
 if Byte(VOP3.NEG).TestBit(1) then Write('-');
 _print_ssrc9(VOP3.SRC1);
 Write(', ');
 if Byte(VOP3.NEG).TestBit(2) then Write('-');
 _print_ssrc9(VOP3.SRC2);

 Writeln(' ; VOP3b');
end;

procedure _print_VOP3(Var SPI:TSPI);
begin
 Case SPI.VOP3a.OP of
    0..255:_print_VOP3c(SPI.VOP3a);
  293..298,
  365..366:_print_VOP3b(SPI.VOP3b);
  else
           _print_VOP3a(SPI.VOP3a);
 end;
end;

procedure _print_VOP2(Var SPI:TSPI);
begin
 Case SPI.VOP2.OP of
  V_CNDMASK_B32       :Write('V_CNDMASK_B32');
  V_READLANE_B32      :Write('V_READLANE_B32');
  V_WRITELANE_B32     :Write('V_WRITELANE_B32');
  V_ADD_F32           :Write('V_ADD_F32');
  V_SUB_F32           :Write('V_SUB_F32');
  V_SUBREV_F32        :Write('V_SUBREV_F32');
  V_MAC_LEGACY_F32    :Write('V_MAC_LEGACY_F32');
  V_MUL_LEGACY_F32    :Write('V_MUL_LEGACY_F32');
  V_MUL_F32           :Write('V_MUL_F32');
  V_MUL_I32_I24       :Write('V_MUL_I32_I24');
  V_MUL_HI_I32_I24    :Write('V_MUL_HI_I32_I24');
  V_MUL_U32_U24       :Write('V_MUL_U32_U24');
  V_MUL_HI_U32_U24    :Write('V_MUL_HI_U32_U24');
  V_MIN_LEGACY_F32    :Write('V_MIN_LEGACY_F32');
  V_MAX_LEGACY_F32    :Write('V_MAX_LEGACY_F32');
  V_MIN_F32           :Write('V_MIN_F32');
  V_MAX_F32           :Write('V_MAX_F32');
  V_MIN_I32           :Write('V_MIN_I32');
  V_MAX_I32           :Write('V_MAX_I32');
  V_MIN_U32           :Write('V_MIN_U32');
  V_MAX_U32           :Write('V_MAX_U32');
  V_LSHR_B32          :Write('V_LSHR_B32');
  V_LSHRREV_B32       :Write('V_LSHRREV_B32');
  V_ASHR_I32          :Write('V_ASHR_I32');
  V_ASHRREV_I32       :Write('V_ASHRREV_I32');
  V_LSHL_B32          :Write('V_LSHL_B32');
  V_LSHLREV_B32       :Write('V_LSHLREV_B32');
  V_AND_B32           :Write('V_AND_B32');
  V_OR_B32            :Write('V_OR_B32');
  V_XOR_B32           :Write('V_XOR_B32');
  V_BFM_B32           :Write('V_BFM_B32');
  V_MAC_F32           :Write('V_MAC_F32');
  V_MADMK_F32         :Write('V_MADMK_F32');
  V_MADAK_F32         :Write('V_MADAK_F32');
  V_BCNT_U32_B32      :Write('V_BCNT_U32_B32');
  V_MBCNT_LO_U32_B32  :Write('V_MBCNT_LO_U32_B32');
  V_MBCNT_HI_U32_B32  :Write('V_MBCNT_HI_U32_B32');
  V_ADD_I32           :Write('V_ADD_I32');
  V_SUB_I32           :Write('V_SUB_I32');
  V_SUBREV_I32        :Write('V_SUBREV_I32');
  V_ADDC_U32          :Write('V_ADDC_U32');
  V_SUBB_U32          :Write('V_SUBB_U32');
  V_SUBBREV_U32       :Write('V_SUBBREV_U32');
  V_LDEXP_F32         :Write('V_LDEXP_F32');
  V_CVT_PKACCUM_U8_F32:Write('V_CVT_PKACCUM_U8_F32');
  V_CVT_PKNORM_I16_F32:Write('V_CVT_PKNORM_I16_F32');
  V_CVT_PKNORM_U16_F32:Write('V_CVT_PKNORM_U16_F32');
  V_CVT_PKRTZ_F16_F32 :Write('V_CVT_PKRTZ_F16_F32');
  V_CVT_PK_U16_U32    :Write('V_CVT_PK_U16_U32');
  V_CVT_PK_I16_I32    :Write('V_CVT_PK_I16_I32');
  else
      Write('VOP2?',SPI.VOP2.OP);
 end;
 Write(' ');

 _print_vdst8(SPI.VOP2.VDST);
 Write(', ');
 _print_ssrc9(SPI.VOP2.SRC0,SPI.INLINE32);
 Write(', ');

 Case SPI.VOP2.OP of
  V_READLANE_B32,
  V_WRITELANE_B32:
    begin
     _print_ssrc8(SPI.VOP2.VSRC1);
    end;
  else
   begin
    _print_vdst8(SPI.VOP2.VSRC1);
   end;
 end;

 Writeln;
end;

procedure _print_VOPC(Var SPI:TSPI);
begin
 Case SPI.VOPC.OP of

  V_CMP_F_F32     :Write('V_CMP_F_F32');
  V_CMP_LT_F32    :Write('V_CMP_LT_F32');
  V_CMP_EQ_F32    :Write('V_CMP_EQ_F32');
  V_CMP_LE_F32    :Write('V_CMP_LE_F32');
  V_CMP_GT_F32    :Write('V_CMP_GT_F32');
  V_CMP_LG_F32    :Write('V_CMP_LG_F32');
  V_CMP_GE_F32    :Write('V_CMP_GE_F32');
  V_CMP_O_F32     :Write('V_CMP_O_F32');
  V_CMP_U_F32     :Write('V_CMP_U_F32');
  V_CMP_NGE_F32   :Write('V_CMP_NGE_F32');
  V_CMP_NLG_F32   :Write('V_CMP_NLG_F32');
  V_CMP_NGT_F32   :Write('V_CMP_NGT_F32');
  V_CMP_NLE_F32   :Write('V_CMP_NLE_F32');
  V_CMP_NEQ_F32   :Write('V_CMP_NEQ_F32');
  V_CMP_NLT_F32   :Write('V_CMP_NLT_F32');
  V_CMP_T_F32     :Write('V_CMP_T_F32');

  V_CMPX_F_F32    :Write('V_CMPX_F_F32');
  V_CMPX_LT_F32   :Write('V_CMPX_LT_F32');
  V_CMPX_EQ_F32   :Write('V_CMPX_EQ_F32');
  V_CMPX_LE_F32   :Write('V_CMPX_LE_F32');
  V_CMPX_GT_F32   :Write('V_CMPX_GT_F32');
  V_CMPX_LG_F32   :Write('V_CMPX_LG_F32');
  V_CMPX_GE_F32   :Write('V_CMPX_GE_F32');
  V_CMPX_O_F32    :Write('V_CMPX_O_F32');
  V_CMPX_U_F32    :Write('V_CMPX_U_F32');
  V_CMPX_NGE_F32  :Write('V_CMPX_NGE_F32');
  V_CMPX_NLG_F32  :Write('V_CMPX_NLG_F32');
  V_CMPX_NGT_F32  :Write('V_CMPX_NGT_F32');
  V_CMPX_NLE_F32  :Write('V_CMPX_NLE_F32');
  V_CMPX_NEQ_F32  :Write('V_CMPX_NEQ_F32');
  V_CMPX_NLT_F32  :Write('V_CMPX_NLT_F32');
  V_CMPX_T_F32    :Write('V_CMPX_T_F32');

  V_CMP_F_F64     :Write('V_CMP_F_F64');
  V_CMP_LT_F64    :Write('V_CMP_LT_F64');
  V_CMP_EQ_F64    :Write('V_CMP_EQ_F64');
  V_CMP_LE_F64    :Write('V_CMP_LE_F64');
  V_CMP_GT_F64    :Write('V_CMP_GT_F64');
  V_CMP_LG_F64    :Write('V_CMP_LG_F64');
  V_CMP_GE_F64    :Write('V_CMP_GE_F64');
  V_CMP_O_F64     :Write('V_CMP_O_F64');
  V_CMP_U_F64     :Write('V_CMP_U_F64');
  V_CMP_NGE_F64   :Write('V_CMP_NGE_F64');
  V_CMP_NLG_F64   :Write('V_CMP_NLG_F64');
  V_CMP_NGT_F64   :Write('V_CMP_NGT_F64');
  V_CMP_NLE_F64   :Write('V_CMP_NLE_F64');
  V_CMP_NEQ_F64   :Write('V_CMP_NEQ_F64');
  V_CMP_NLT_F64   :Write('V_CMP_NLT_F64');
  V_CMP_T_F64     :Write('V_CMP_T_F64');

  V_CMPX_F_F64    :Write('V_CMPX_F_F64');
  V_CMPX_LT_F64   :Write('V_CMPX_LT_F64');
  V_CMPX_EQ_F64   :Write('V_CMPX_EQ_F64');
  V_CMPX_LE_F64   :Write('V_CMPX_LE_F64');
  V_CMPX_GT_F64   :Write('V_CMPX_GT_F64');
  V_CMPX_LG_F64   :Write('V_CMPX_LG_F64');
  V_CMPX_GE_F64   :Write('V_CMPX_GE_F64');
  V_CMPX_O_F64    :Write('V_CMPX_O_F64');
  V_CMPX_U_F64    :Write('V_CMPX_U_F64');
  V_CMPX_NGE_F64  :Write('V_CMPX_NGE_F64');
  V_CMPX_NLG_F64  :Write('V_CMPX_NLG_F64');
  V_CMPX_NGT_F64  :Write('V_CMPX_NGT_F64');
  V_CMPX_NLE_F64  :Write('V_CMPX_NLE_F64');
  V_CMPX_NEQ_F64  :Write('V_CMPX_NEQ_F64');
  V_CMPX_NLT_F64  :Write('V_CMPX_NLT_F64');
  V_CMPX_T_F64    :Write('V_CMPX_T_F64');


  V_CMPS_F_F32    :Write('V_CMPS_F_F32');
  V_CMPS_LT_F32   :Write('V_CMPS_LT_F32');
  V_CMPS_EQ_F32   :Write('V_CMPS_EQ_F32');
  V_CMPS_LE_F32   :Write('V_CMPS_LE_F32');
  V_CMPS_GT_F32   :Write('V_CMPS_GT_F32');
  V_CMPS_LG_F32   :Write('V_CMPS_LG_F32');
  V_CMPS_GE_F32   :Write('V_CMPS_GE_F32');
  V_CMPS_O_F32    :Write('V_CMPS_O_F32');
  V_CMPS_U_F32    :Write('V_CMPS_U_F32');
  V_CMPS_NGE_F32  :Write('V_CMPS_NGE_F32');
  V_CMPS_NLG_F32  :Write('V_CMPS_NLG_F32');
  V_CMPS_NGT_F32  :Write('V_CMPS_NGT_F32');
  V_CMPS_NLE_F32  :Write('V_CMPS_NLE_F32');
  V_CMPS_NEQ_F32  :Write('V_CMPS_NEQ_F32');
  V_CMPS_NLT_F32  :Write('V_CMPS_NLT_F32');
  V_CMPS_T_F32    :Write('V_CMPS_T_F32');

  V_CMPSX_F_F32   :Write('V_CMPSX_F_F32');
  V_CMPSX_LT_F32  :Write('V_CMPSX_LT_F32');
  V_CMPSX_EQ_F32  :Write('V_CMPSX_EQ_F32');
  V_CMPSX_LE_F32  :Write('V_CMPSX_LE_F32');
  V_CMPSX_GT_F32  :Write('V_CMPSX_GT_F32');
  V_CMPSX_LG_F32  :Write('V_CMPSX_LG_F32');
  V_CMPSX_GE_F32  :Write('V_CMPSX_GE_F32');
  V_CMPSX_O_F32   :Write('V_CMPSX_O_F32');
  V_CMPSX_U_F32   :Write('V_CMPSX_U_F32');
  V_CMPSX_NGE_F32 :Write('V_CMPSX_NGE_F32');
  V_CMPSX_NLG_F32 :Write('V_CMPSX_NLG_F32');
  V_CMPSX_NGT_F32 :Write('V_CMPSX_NGT_F32');
  V_CMPSX_NLE_F32 :Write('V_CMPSX_NLE_F32');
  V_CMPSX_NEQ_F32 :Write('V_CMPSX_NEQ_F32');
  V_CMPSX_NLT_F32 :Write('V_CMPSX_NLT_F32');
  V_CMPSX_T_F32   :Write('V_CMPSX_T_F32');

  V_CMPS_F_F64    :Write('V_CMPS_F_F64');
  V_CMPS_LT_F64   :Write('V_CMPS_LT_F64');
  V_CMPS_EQ_F64   :Write('V_CMPS_EQ_F64');
  V_CMPS_LE_F64   :Write('V_CMPS_LE_F64');
  V_CMPS_GT_F64   :Write('V_CMPS_GT_F64');
  V_CMPS_LG_F64   :Write('V_CMPS_LG_F64');
  V_CMPS_GE_F64   :Write('V_CMPS_GE_F64');
  V_CMPS_O_F64    :Write('V_CMPS_O_F64');
  V_CMPS_U_F64    :Write('V_CMPS_U_F64');
  V_CMPS_NGE_F64  :Write('V_CMPS_NGE_F64');
  V_CMPS_NLG_F64  :Write('V_CMPS_NLG_F64');
  V_CMPS_NGT_F64  :Write('V_CMPS_NGT_F64');
  V_CMPS_NLE_F64  :Write('V_CMPS_NLE_F64');
  V_CMPS_NEQ_F64  :Write('V_CMPS_NEQ_F64');
  V_CMPS_NLT_F64  :Write('V_CMPS_NLT_F64');
  V_CMPS_T_F64    :Write('V_CMPS_T_F64');

  V_CMPSX_F_F64   :Write('V_CMPSX_F_F64');
  V_CMPSX_LT_F64  :Write('V_CMPSX_LT_F64');
  V_CMPSX_EQ_F64  :Write('V_CMPSX_EQ_F64');
  V_CMPSX_LE_F64  :Write('V_CMPSX_LE_F64');
  V_CMPSX_GT_F64  :Write('V_CMPSX_GT_F64');
  V_CMPSX_LG_F64  :Write('V_CMPSX_LG_F64');
  V_CMPSX_GE_F64  :Write('V_CMPSX_GE_F64');
  V_CMPSX_O_F64   :Write('V_CMPSX_O_F64');
  V_CMPSX_U_F64   :Write('V_CMPSX_U_F64');
  V_CMPSX_NGE_F64 :Write('V_CMPSX_NGE_F64');
  V_CMPSX_NLG_F64 :Write('V_CMPSX_NLG_F64');
  V_CMPSX_NGT_F64 :Write('V_CMPSX_NGT_F64');
  V_CMPSX_NLE_F64 :Write('V_CMPSX_NLE_F64');
  V_CMPSX_NEQ_F64 :Write('V_CMPSX_NEQ_F64');
  V_CMPSX_NLT_F64 :Write('V_CMPSX_NLT_F64');
  V_CMPSX_T_F64   :Write('V_CMPSX_T_F64');


  V_CMP_F_I32     :Write('V_CMP_F_I32');
  V_CMP_LT_I32    :Write('V_CMP_LT_I32');
  V_CMP_EQ_I32    :Write('V_CMP_EQ_I32');
  V_CMP_LE_I32    :Write('V_CMP_LE_I32');
  V_CMP_GT_I32    :Write('V_CMP_GT_I32');
  V_CMP_LG_I32    :Write('V_CMP_LG_I32');
  V_CMP_GE_I32    :Write('V_CMP_GE_I32');
  V_CMP_T_I32     :Write('V_CMP_T_I32');

  V_CMPX_F_I32    :Write('V_CMPX_F_I32');
  V_CMPX_LT_I32   :Write('V_CMPX_LT_I32');
  V_CMPX_EQ_I32   :Write('V_CMPX_EQ_I32');
  V_CMPX_LE_I32   :Write('V_CMPX_LE_I32');
  V_CMPX_GT_I32   :Write('V_CMPX_GT_I32');
  V_CMPX_LG_I32   :Write('V_CMPX_LG_I32');
  V_CMPX_GE_I32   :Write('V_CMPX_GE_I32');
  V_CMPX_T_I32    :Write('V_CMPX_T_I32');

  V_CMP_F_I64     :Write('V_CMP_F_I64');
  V_CMP_LT_I64    :Write('V_CMP_LT_I64');
  V_CMP_EQ_I64    :Write('V_CMP_EQ_I64');
  V_CMP_LE_I64    :Write('V_CMP_LE_I64');
  V_CMP_GT_I64    :Write('V_CMP_GT_I64');
  V_CMP_LG_I64    :Write('V_CMP_LG_I64');
  V_CMP_GE_I64    :Write('V_CMP_GE_I64');
  V_CMP_T_I64     :Write('V_CMP_T_I64');

  V_CMPX_F_I64    :Write('V_CMPX_F_I64');
  V_CMPX_LT_I64   :Write('V_CMPX_LT_I64');
  V_CMPX_EQ_I64   :Write('V_CMPX_EQ_I64');
  V_CMPX_LE_I64   :Write('V_CMPX_LE_I64');
  V_CMPX_GT_I64   :Write('V_CMPX_GT_I64');
  V_CMPX_LG_I64   :Write('V_CMPX_LG_I64');
  V_CMPX_GE_I64   :Write('V_CMPX_GE_I64');
  V_CMPX_T_I64    :Write('V_CMPX_T_I64');

  V_CMP_F_U32     :Write('V_CMP_F_U32');
  V_CMP_LT_U32    :Write('V_CMP_LT_U32');
  V_CMP_EQ_U32    :Write('V_CMP_EQ_U32');
  V_CMP_LE_U32    :Write('V_CMP_LE_U32');
  V_CMP_GT_U32    :Write('V_CMP_GT_U32');
  V_CMP_LG_U32    :Write('V_CMP_LG_U32');
  V_CMP_GE_U32    :Write('V_CMP_GE_U32');
  V_CMP_T_U32     :Write('V_CMP_T_U32');

  V_CMPX_F_U32    :Write('V_CMPX_F_U32');
  V_CMPX_LT_U32   :Write('V_CMPX_LT_U32');
  V_CMPX_EQ_U32   :Write('V_CMPX_EQ_U32');
  V_CMPX_LE_U32   :Write('V_CMPX_LE_U32');
  V_CMPX_GT_U32   :Write('V_CMPX_GT_U32');
  V_CMPX_LG_U32   :Write('V_CMPX_LG_U32');
  V_CMPX_GE_U32   :Write('V_CMPX_GE_U32');
  V_CMPX_T_U32    :Write('V_CMPX_T_U32');

  V_CMP_F_U64     :Write('V_CMP_F_U64');
  V_CMP_LT_U64    :Write('V_CMP_LT_U64');
  V_CMP_EQ_U64    :Write('V_CMP_EQ_U64');
  V_CMP_LE_U64    :Write('V_CMP_LE_U64');
  V_CMP_GT_U64    :Write('V_CMP_GT_U64');
  V_CMP_LG_U64    :Write('V_CMP_LG_U64');
  V_CMP_GE_U64    :Write('V_CMP_GE_U64');
  V_CMP_T_U64     :Write('V_CMP_T_U64');

  V_CMPX_F_U64    :Write('V_CMPX_F_U64');
  V_CMPX_LT_U64   :Write('V_CMPX_LT_U64');
  V_CMPX_EQ_U64   :Write('V_CMPX_EQ_U64');
  V_CMPX_LE_U64   :Write('V_CMPX_LE_U64');
  V_CMPX_GT_U64   :Write('V_CMPX_GT_U64');
  V_CMPX_LG_U64   :Write('V_CMPX_LG_U64');
  V_CMPX_GE_U64   :Write('V_CMPX_GE_U64');
  V_CMPX_T_U64    :Write('V_CMPX_T_U64');

  V_CMP_CLASS_F32 :Write('V_CMP_CLASS_F32');
  V_CMPX_CLASS_F32:Write('V_CMPX_CLASS_F32');
  V_CMP_CLASS_F64 :Write('V_CMP_CLASS_F64');
  V_CMPX_CLASS_F64:Write('V_CMPX_CLASS_F64');

  else
      Write('VOPC?',SPI.VOPC.OP);
 end;
 Write(' VCC, ');

 _print_ssrc9(SPI.VOPC.SRC0,SPI.INLINE32);
 Write(', ');
 _print_vdst8(SPI.VOPC.VSRC1);

 Writeln;
end;

procedure _print_VOP1(Var SPI:TSPI);
begin

 Case SPI.VOP1.OP of

  V_NOP              :Write('V_NOP');
  V_MOV_B32          :Write('V_MOV_B32');
  V_READFIRSTLANE_B32:Write('V_READFIRSTLANE_B32');
  V_CVT_I32_F64      :Write('V_CVT_I32_F64');
  V_CVT_F64_I32      :Write('V_CVT_F64_I32');
  V_CVT_F32_I32      :Write('V_CVT_F32_I32');
  V_CVT_F32_U32      :Write('V_CVT_F32_U32');
  V_CVT_U32_F32      :Write('V_CVT_U32_F32');
  V_CVT_I32_F32      :Write('V_CVT_I32_F32');
  V_MOV_FED_B32      :Write('V_MOV_FED_B32');
  V_CVT_F16_F32      :Write('V_CVT_F16_F32');
  V_CVT_F32_F16      :Write('V_CVT_F32_F16');
  V_CVT_RPI_I32_F32  :Write('V_CVT_RPI_I32_F32');
  V_CVT_FLR_I32_F32  :Write('V_CVT_FLR_I32_F32');
  V_CVT_OFF_F32_I4   :Write('V_CVT_OFF_F32_I4');
  V_CVT_F32_F64      :Write('V_CVT_F32_F64');
  V_CVT_F64_F32      :Write('V_CVT_F64_F32');
  V_CVT_F32_UBYTE0   :Write('V_CVT_F32_UBYTE0');
  V_CVT_F32_UBYTE1   :Write('V_CVT_F32_UBYTE1');
  V_CVT_F32_UBYTE2   :Write('V_CVT_F32_UBYTE2');
  V_CVT_F32_UBYTE3   :Write('V_CVT_F32_UBYTE3');
  V_CVT_U32_F64      :Write('V_CVT_U32_F64');
  V_CVT_F64_U32      :Write('V_CVT_F64_U32');
  V_TRUNC_F64        :Write('V_TRUNC_F64');
  V_CEIL_F64         :Write('V_CEIL_F64');
  V_RNDNE_F64        :Write('V_RNDNE_F64');
  V_FLOOR_F64        :Write('V_FLOOR_F64');
  V_FRACT_F32        :Write('V_FRACT_F32');
  V_TRUNC_F32        :Write('V_TRUNC_F32');
  V_CEIL_F32         :Write('V_CEIL_F32');
  V_RNDNE_F32        :Write('V_RNDNE_F32');
  V_FLOOR_F32        :Write('V_FLOOR_F32');
  V_EXP_F32          :Write('V_EXP_F32');
  V_LOG_CLAMP_F32    :Write('V_LOG_CLAMP_F32');
  V_LOG_F32          :Write('V_LOG_F32');
  V_RCP_CLAMP_F32    :Write('V_RCP_CLAMP_F32');
  V_RCP_LEGACY_F32   :Write('V_RCP_LEGACY_F32');
  V_RCP_F32          :Write('V_RCP_F32');
  V_RCP_IFLAG_F32    :Write('V_RCP_IFLAG_F32');
  V_RSQ_CLAMP_F32    :Write('V_RSQ_CLAMP_F32');
  V_RSQ_LEGACY_F32   :Write('V_RSQ_LEGACY_F32');
  V_RSQ_F32          :Write('V_RSQ_F32');
  V_RCP_F64          :Write('V_RCP_F64');
  V_RCP_CLAMP_F64    :Write('V_RCP_CLAMP_F64');
  V_RSQ_F64          :Write('V_RSQ_F64');
  V_RSQ_CLAMP_F64    :Write('V_RSQ_CLAMP_F64');
  V_SQRT_F32         :Write('V_SQRT_F32');
  V_SQRT_F64         :Write('V_SQRT_F64');
  V_SIN_F32          :Write('V_SIN_F32');
  V_COS_F32          :Write('V_COS_F32');
  V_NOT_B32          :Write('V_NOT_B32');
  V_BFREV_B32        :Write('V_BFREV_B32');
  V_FFBH_U32         :Write('V_FFBH_U32');
  V_FFBL_B32         :Write('V_FFBL_B32');
  V_FFBH_I32         :Write('V_FFBH_I32');
  V_FREXP_EXP_I32_F64:Write('V_FREXP_EXP_I32_F64');
  V_FREXP_MANT_F64   :Write('V_FREXP_MANT_F64');
  V_FRACT_F64        :Write('V_FRACT_F64');
  V_FREXP_EXP_I32_F32:Write('V_FREXP_EXP_I32_F32');
  V_FREXP_MANT_F32   :Write('V_FREXP_MANT_F32');
  V_CLREXCP          :Write('V_CLREXCP');
  V_MOVRELD_B32      :Write('V_MOVRELD_B32');
  V_MOVRELS_B32      :Write('V_MOVRELS_B32');
  V_MOVRELSD_B32     :Write('V_MOVRELSD_B32');

  else
      Write('VOP1?',SPI.VOP1.OP);
 end;
 Write(' ');

 _print_vdst8(SPI.VOP1.VDST);
 Write(', ');
 _print_ssrc9(SPI.VOP1.SRC0,SPI.INLINE32);

 Writeln;
end;

procedure _print_MUBUF(Var SPI:TSPI);
begin
 case SPI.MUBUF.OP of
  BUFFER_LOAD_FORMAT_X    :Write('BUFFER_LOAD_FORMAT_X');
  BUFFER_LOAD_FORMAT_XY   :Write('BUFFER_LOAD_FORMAT_XY');
  BUFFER_LOAD_FORMAT_XYZ  :Write('BUFFER_LOAD_FORMAT_XYZ');
  BUFFER_LOAD_FORMAT_XYZW :Write('BUFFER_LOAD_FORMAT_XYZW');
  BUFFER_STORE_FORMAT_X   :Write('BUFFER_STORE_FORMAT_X');
  BUFFER_STORE_FORMAT_XY  :Write('BUFFER_STORE_FORMAT_XY');
  BUFFER_STORE_FORMAT_XYZ :Write('BUFFER_STORE_FORMAT_XYZ');
  BUFFER_STORE_FORMAT_XYZW:Write('BUFFER_STORE_FORMAT_XYZW');

  BUFFER_LOAD_UBYTE       :Write('BUFFER_LOAD_UBYTE');
  BUFFER_LOAD_SBYTE       :Write('BUFFER_LOAD_SBYTE');
  BUFFER_LOAD_USHORT      :Write('BUFFER_LOAD_USHORT');
  BUFFER_LOAD_SSHORT      :Write('BUFFER_LOAD_SSHORT');
  BUFFER_LOAD_DWORD       :Write('BUFFER_LOAD_DWORD');
  BUFFER_LOAD_DWORDX2     :Write('BUFFER_LOAD_DWORDX2');
  BUFFER_LOAD_DWORDX4     :Write('BUFFER_LOAD_DWORDX4');
  BUFFER_LOAD_DWORDX3     :Write('BUFFER_LOAD_DWORDX3');

  BUFFER_STORE_BYTE       :Write('BUFFER_STORE_BYTE');
  BUFFER_STORE_SHORT      :Write('BUFFER_STORE_SHORT');
  BUFFER_STORE_DWORD      :Write('BUFFER_STORE_DWORD');
  BUFFER_STORE_DWORDX2    :Write('BUFFER_STORE_DWORDX2');
  BUFFER_STORE_DWORDX4    :Write('BUFFER_STORE_DWORDX4');
  BUFFER_STORE_DWORDX3    :Write('BUFFER_STORE_DWORDX3');
  else
      Write('MUBUF?',SPI.MUBUF.OP);
 end;
 Write(' ');

 _print_vdst8(SPI.MUBUF.VDATA);
 Write(', ');
 _print_vdst8(SPI.MUBUF.VADDR);
 Write(', ');
 Write('s[',SPI.MUBUF.SRSRC*4,':',SPI.MUBUF.SRSRC*4+3,']');
 Write(', ');
 Write(SPI.MUBUF.OFFSET);
 Write(', ');
 Write('[');
 _print_ssrc8(SPI.MUBUF.SOFFSET);
 Write(']');

 if SPI.MUBUF.OFFEN=1 then
  Write(' OFFEN');
 if SPI.MUBUF.IDXEN=1 then
  Write(' IDXEN');
 if SPI.MUBUF.GLC=1 then
  Write(' GLC');
 if SPI.MUBUF.LDS=1 then
  Write(' LDS');
 if SPI.MUBUF.SLC=1 then
  Write(' SLC');
 if SPI.MUBUF.TFE=1 then
  Write(' TFE');

 Writeln;
end;

procedure _print_MTBUF(Var SPI:TSPI);
begin
 case SPI.MTBUF.OP of
  TBUFFER_LOAD_FORMAT_X    :Write('TBUFFER_LOAD_FORMAT_X');
  TBUFFER_LOAD_FORMAT_XY   :Write('TBUFFER_LOAD_FORMAT_XY');
  TBUFFER_LOAD_FORMAT_XYZ  :Write('TBUFFER_LOAD_FORMAT_XYZ');
  TBUFFER_LOAD_FORMAT_XYZW :Write('TBUFFER_LOAD_FORMAT_XYZ');

  TBUFFER_STORE_FORMAT_X   :Write('TBUFFER_STORE_FORMAT_X');
  TBUFFER_STORE_FORMAT_XY  :Write('TBUFFER_STORE_FORMAT_XY');
  TBUFFER_STORE_FORMAT_XYZ :Write('TBUFFER_STORE_FORMAT_XYZ');
  TBUFFER_STORE_FORMAT_XYZW:Write('TBUFFER_STORE_FORMAT_XYZW');
 end;
 Write(' ');

 _print_vdst8(SPI.MTBUF.VDATA);
 Write(', ');
 _print_vdst8(SPI.MTBUF.VADDR);
 Write(', ');
 Write('s[',SPI.MTBUF.SRSRC*4,':',SPI.MTBUF.SRSRC*4+3,']');
 Write(', ');
 Write(SPI.MTBUF.OFFSET);
 Write(', ');
 Write('[');
 _print_ssrc8(SPI.MTBUF.SOFFSET);
 Write(']');

 if SPI.MTBUF.OFFEN=1 then
  Write(' OFFEN');
 if SPI.MTBUF.IDXEN=1 then
  Write(' IDXEN');
 if SPI.MTBUF.GLC=1 then
  Write(' GLC');
 if SPI.MTBUF.SLC=1 then
  Write(' SLC');
 if SPI.MTBUF.TFE=1 then
  Write(' TFE');

 Write(' format:[');

 Case SPI.MTBUF.DFMT of
  BUF_DATA_FORMAT_INVALID    :Write('INVALID');
  BUF_DATA_FORMAT_8          :Write('8');
  BUF_DATA_FORMAT_16         :Write('16');
  BUF_DATA_FORMAT_8_8        :Write('8_8');
  BUF_DATA_FORMAT_32         :Write('32');
  BUF_DATA_FORMAT_16_16      :Write('16_16');
  BUF_DATA_FORMAT_10_11_11   :Write('10_11_11');
  BUF_DATA_FORMAT_11_11_10   :Write('11_11_10');
  BUF_DATA_FORMAT_10_10_10_2 :Write('10_10_10_2');
  BUF_DATA_FORMAT_2_10_10_10 :Write('2_10_10_10');
  BUF_DATA_FORMAT_8_8_8_8    :Write('8_8_8_8');
  BUF_DATA_FORMAT_32_32      :Write('32_32');
  BUF_DATA_FORMAT_16_16_16_16:Write('16_16_16_16');
  BUF_DATA_FORMAT_32_32_32   :Write('32_32_32');
  BUF_DATA_FORMAT_32_32_32_32:Write('32_32_32_32');
  BUF_DATA_FORMAT_RESERVED   :Write('RESERVED');
 end;

 Write(',');

 Case SPI.MTBUF.NFMT of
  BUF_NUM_FORMAT_UNORM   :Write('UNORM');
  BUF_NUM_FORMAT_SNORM   :Write('SNORM');
  BUF_NUM_FORMAT_USCALED :Write('USCALED');
  BUF_NUM_FORMAT_SSCALED :Write('SSCALED');
  BUF_NUM_FORMAT_UINT    :Write('UINT');
  BUF_NUM_FORMAT_SINT    :Write('SINT');
  BUF_NUM_FORMAT_SNORM_NZ:Write('SNORM_NZ');
  BUF_NUM_FORMAT_FLOAT   :Write('FLOAT');
 end;

 Write(']');

 Writeln;
end;

procedure _print_MIMG(Var SPI:TSPI);
begin
 Case SPI.MIMG.OP of

  IMAGE_LOAD            :Write('IMAGE_LOAD');
  IMAGE_LOAD_MIP        :Write('IMAGE_LOAD_MIP');
  IMAGE_LOAD_PCK        :Write('IMAGE_LOAD_PCK');
  IMAGE_LOAD_PCK_SGN    :Write('IMAGE_LOAD_PCK_SGN');
  IMAGE_LOAD_MIP_PCK    :Write('IMAGE_LOAD_MIP_PCK');
  IMAGE_LOAD_MIP_PCK_SGN:Write('IMAGE_LOAD_MIP_PCK_SGN');
  IMAGE_STORE           :Write('IMAGE_STORE');
  IMAGE_STORE_MIP       :Write('IMAGE_STORE_MIP');
  IMAGE_STORE_PCK       :Write('IMAGE_STORE_PCK');
  IMAGE_STORE_MIP_PCK   :Write('IMAGE_STORE_MIP_PCK');
  IMAGE_GET_RESINFO     :Write('IMAGE_GET_RESINFO');
  IMAGE_ATOMIC_SWAP     :Write('IMAGE_ATOMIC_SWAP');
  IMAGE_ATOMIC_CMPSWAP  :Write('IMAGE_ATOMIC_CMPSWAP');
  IMAGE_ATOMIC_ADD      :Write('IMAGE_ATOMIC_ADD');
  IMAGE_ATOMIC_SUB      :Write('IMAGE_ATOMIC_SUB');
  IMAGE_ATOMIC_SMIN     :Write('IMAGE_ATOMIC_SMIN');
  IMAGE_ATOMIC_UMIN     :Write('IMAGE_ATOMIC_UMIN');
  IMAGE_ATOMIC_SMAX     :Write('IMAGE_ATOMIC_SMAX');
  IMAGE_ATOMIC_UMAX     :Write('IMAGE_ATOMIC_UMAX');
  IMAGE_ATOMIC_AND      :Write('IMAGE_ATOMIC_AND');
  IMAGE_ATOMIC_OR       :Write('IMAGE_ATOMIC_OR');
  IMAGE_ATOMIC_XOR      :Write('IMAGE_ATOMIC_XOR');
  IMAGE_ATOMIC_INC      :Write('IMAGE_ATOMIC_INC');
  IMAGE_ATOMIC_DEC      :Write('IMAGE_ATOMIC_DEC');
  IMAGE_ATOMIC_FCMPSWAP :Write('IMAGE_ATOMIC_FCMPSWAP');
  IMAGE_ATOMIC_FMIN     :Write('IMAGE_ATOMIC_FMIN');
  IMAGE_ATOMIC_FMAX     :Write('IMAGE_ATOMIC_FMAX');
  IMAGE_SAMPLE          :Write('IMAGE_SAMPLE');
  IMAGE_SAMPLE_CL       :Write('IMAGE_SAMPLE_CL');
  IMAGE_SAMPLE_D        :Write('IMAGE_SAMPLE_D');
  IMAGE_SAMPLE_D_CL     :Write('IMAGE_SAMPLE_D_CL');
  IMAGE_SAMPLE_L        :Write('IMAGE_SAMPLE_L');
  IMAGE_SAMPLE_B        :Write('IMAGE_SAMPLE_B');
  IMAGE_SAMPLE_B_CL     :Write('IMAGE_SAMPLE_B_CL');
  IMAGE_SAMPLE_LZ       :Write('IMAGE_SAMPLE_LZ');
  IMAGE_SAMPLE_C        :Write('IMAGE_SAMPLE_C');
  IMAGE_SAMPLE_C_CL     :Write('IMAGE_SAMPLE_C_CL');
  IMAGE_SAMPLE_C_D      :Write('IMAGE_SAMPLE_C_D');
  IMAGE_SAMPLE_C_D_CL   :Write('IMAGE_SAMPLE_C_D_CL');
  IMAGE_SAMPLE_C_L      :Write('IMAGE_SAMPLE_C_L');
  IMAGE_SAMPLE_C_B      :Write('IMAGE_SAMPLE_C_B');
  IMAGE_SAMPLE_C_B_CL   :Write('IMAGE_SAMPLE_C_B_CL');
  IMAGE_SAMPLE_C_LZ     :Write('IMAGE_SAMPLE_C_LZ');
  IMAGE_SAMPLE_O        :Write('IMAGE_SAMPLE_O');
  IMAGE_SAMPLE_CL_O     :Write('IMAGE_SAMPLE_CL_O');
  IMAGE_SAMPLE_D_O      :Write('IMAGE_SAMPLE_D_O');
  IMAGE_SAMPLE_D_CL_O   :Write('IMAGE_SAMPLE_D_CL_O');
  IMAGE_SAMPLE_L_O      :Write('IMAGE_SAMPLE_L_O');
  IMAGE_SAMPLE_B_O      :Write('IMAGE_SAMPLE_B_O');
  IMAGE_SAMPLE_B_CL_O   :Write('IMAGE_SAMPLE_B_CL_O');
  IMAGE_SAMPLE_LZ_O     :Write('IMAGE_SAMPLE_LZ_O');
  IMAGE_SAMPLE_C_O      :Write('IMAGE_SAMPLE_C_O');
  IMAGE_SAMPLE_C_CL_O   :Write('IMAGE_SAMPLE_C_CL_O');
  IMAGE_SAMPLE_C_D_O    :Write('IMAGE_SAMPLE_C_D_O');
  IMAGE_SAMPLE_C_D_CL_O :Write('IMAGE_SAMPLE_C_D_CL_O');
  IMAGE_SAMPLE_C_L_O    :Write('IMAGE_SAMPLE_C_L_O');
  IMAGE_SAMPLE_C_B_O    :Write('IMAGE_SAMPLE_C_B_O');
  IMAGE_SAMPLE_C_B_CL_O :Write('IMAGE_SAMPLE_C_B_CL_O');
  IMAGE_SAMPLE_C_LZ_O   :Write('IMAGE_SAMPLE_C_LZ_O');
  IMAGE_GATHER4         :Write('IMAGE_GATHER4');
  IMAGE_GATHER4_CL      :Write('IMAGE_GATHER4_CL');
  IMAGE_GATHER4_L       :Write('IMAGE_GATHER4_L');
  IMAGE_GATHER4_B       :Write('IMAGE_GATHER4_B');
  IMAGE_GATHER4_B_CL    :Write('IMAGE_GATHER4_B_CL');
  IMAGE_GATHER4_LZ      :Write('IMAGE_GATHER4_LZ');
  IMAGE_GATHER4_C       :Write('IMAGE_GATHER4_C');
  IMAGE_GATHER4_C_CL    :Write('IMAGE_GATHER4_C_CL');
  IMAGE_GATHER4_C_L     :Write('IMAGE_GATHER4_C_L');
  IMAGE_GATHER4_C_B     :Write('IMAGE_GATHER4_C_B');
  IMAGE_GATHER4_C_B_CL  :Write('IMAGE_GATHER4_C_B_CL');
  IMAGE_GATHER4_C_LZ    :Write('IMAGE_GATHER4_C_LZ');
  IMAGE_GATHER4_O       :Write('IMAGE_GATHER4_O');
  IMAGE_GATHER4_CL_O    :Write('IMAGE_GATHER4_CL_O');
  IMAGE_GATHER4_L_O     :Write('IMAGE_GATHER4_L_O');
  IMAGE_GATHER4_B_O     :Write('IMAGE_GATHER4_B_O');
  IMAGE_GATHER4_B_CL_O  :Write('IMAGE_GATHER4_B_CL_O');
  IMAGE_GATHER4_LZ_O    :Write('IMAGE_GATHER4_LZ_O');
  IMAGE_GATHER4_C_O     :Write('IMAGE_GATHER4_C_O');
  IMAGE_GATHER4_C_CL_O  :Write('IMAGE_GATHER4_C_CL_O');
  IMAGE_GATHER4_C_L_O   :Write('IMAGE_GATHER4_C_L_O');
  IMAGE_GATHER4_C_B_O   :Write('IMAGE_GATHER4_C_B_O');
  IMAGE_GATHER4_C_B_CL_O:Write('IMAGE_GATHER4_C_B_CL_O');
  IMAGE_GATHER4_C_LZ_O  :Write('IMAGE_GATHER4_C_LZ_O');
  IMAGE_GET_LOD         :Write('IMAGE_GET_LOD');
  IMAGE_SAMPLE_CD       :Write('IMAGE_SAMPLE_CD');
  IMAGE_SAMPLE_CD_CL    :Write('IMAGE_SAMPLE_CD_CL');
  IMAGE_SAMPLE_C_CD     :Write('IMAGE_SAMPLE_C_CD');
  IMAGE_SAMPLE_C_CD_CL  :Write('IMAGE_SAMPLE_C_CD_CL');
  IMAGE_SAMPLE_CD_O     :Write('IMAGE_SAMPLE_CD_O');
  IMAGE_SAMPLE_CD_CL_O  :Write('IMAGE_SAMPLE_CD_CL_O');
  IMAGE_SAMPLE_C_CD_O   :Write('IMAGE_SAMPLE_C_CD_O');
  IMAGE_SAMPLE_C_CD_CL_O:Write('IMAGE_SAMPLE_C_CD_CL_O');

  else
      Write('MIMG?',SPI.MIMG.OP);
 end;
 Write(' ');

 Case PopCnt(SPI.MIMG.DMASK) of
  2:begin
     Write('v[',SPI.MIMG.VDATA,':',SPI.MIMG.VDATA+1,']');
     Write(', ');
     Write('v[',SPI.MIMG.VADDR,':',SPI.MIMG.VADDR+1,']');
    end;
  3:begin
     Write('v[',SPI.MIMG.VDATA,':',SPI.MIMG.VDATA+2,']');
     Write(', ');
     Write('v[',SPI.MIMG.VADDR,':',SPI.MIMG.VADDR+2,']');
    end;
  4:begin
     Write('v[',SPI.MIMG.VDATA,':',SPI.MIMG.VDATA+3,']');
     Write(', ');
     Write('v[',SPI.MIMG.VADDR,':',SPI.MIMG.VADDR+3,']');
    end;
  else
    begin
     _print_vdst8(SPI.MIMG.VDATA);
     Write(', ');
     _print_vdst8(SPI.MIMG.VADDR);
    end;
 end;

 Write(', ');
 Write('s[',SPI.MIMG.SRSRC*4,':',SPI.MIMG.SRSRC*4+7,']');

 Case SPI.MIMG.OP of
  IMAGE_SAMPLE..IMAGE_SAMPLE_C_CD_CL_O:
   begin
    Write(', ');
    Write('s[',SPI.MIMG.SSAMP*4,':',SPI.MIMG.SSAMP*4+3,']');
   end;
  else;
 end;

 Write(' dmask:0x',HexStr(SPI.MIMG.DMASK,1));

 Writeln;
end;

procedure _print_EXP(Var SPI:TSPI);
var
 f:Byte;
begin
 Write('EXP ');

 case SPI.EXP.TGT of
    0..7:Write('mrt',IntToStr(SPI.EXP.TGT));
       8:Write('mrtz');
       9:Write('null');
  12..15:Write('pos',IntToStr(SPI.EXP.TGT-12));
  32..63:Write('param',IntToStr(SPI.EXP.TGT-32));
  else
   Write('?');
 end;

 //half 0=0000 3=0011 C=1100 F=1111

 f:=SPI.EXP.EN;

 Case SPI.EXP.COMPR of
  0:
    begin

     Write(', ');
     if (f and $1<>0) then
     begin
      _print_vdst8(SPI.EXP.VSRC0);
     end else
     begin
      Write('off');
     end;

     Write(', ');
     if (f and $2<>0) then
     begin
      _print_vdst8(SPI.EXP.VSRC1);
     end else
     begin
      Write('off');
     end;

     Write(', ');
     if (f and $4<>0) then
     begin
      _print_vdst8(SPI.EXP.VSRC2);
     end else
     begin
      Write('off');
     end;

     Write(', ');
     if (f and $8<>0) then
     begin
      _print_vdst8(SPI.EXP.VSRC3);
     end else
     begin
      Write('off');
     end;

    end;
  1: //is half16 compressed
    begin

     Write(', ');
     if (f and $1<>0) then
     begin
      _print_vdst8(SPI.EXP.VSRC0);
     end else
     begin
      Write('off');
     end;

     Write(', ');
     if (f and $2<>0) then
     begin
      _print_vdst8(SPI.EXP.VSRC1);
     end else
     begin
      Write('off');
     end;

    end;
 end;

 if SPI.EXP.COMPR<>0 then
  Write(' compr');       //is half float compressed

 if SPI.EXP.vm<>0 then
  Write(' vm');

 {
 Valid Mask. When set to 1, this indicates that the EXEC mask represents the valid-mask
 for this wavefront. It can be sent multiple times per shader (the final value is used), but
 must be sent at least once per pixel shader.
 }

 {
 The EXEC mask is applied to all exports. Only pixels with the corresponding
 if VM<>0 and EXEC<>0 = set pixel else discard pixel
 }

 if SPI.EXP.DONE<>0 then //this is the last MRT
  Write(' done');

 Writeln;
end;

procedure _print_VINTRP(Var SPI:TSPI);
begin
 Case SPI.VINTRP.OP of
  V_INTERP_P1_F32 :Write('V_INTERP_P1_F32');
  V_INTERP_P2_F32 :Write('V_INTERP_P2_F32');
  V_INTERP_MOV_F32:Write('V_INTERP_MOV_F32');
  else
      Write('VINTRP?',SPI.VINTRP.OP);
 end;
 Write(' ');

 _print_vdst8(SPI.VINTRP.VDST);
 Write(', ');
 _print_vdst8(SPI.VINTRP.VSRC);

 Write(', attr',SPI.VINTRP.ATTR,'.');

 case SPI.VINTRP.ATTRCHAN of
  0:Write('x');
  1:Write('y');
  2:Write('z');
  3:Write('w');
 end;

 Writeln;
end;

procedure _print_DS(Var SPI:TSPI);
begin
 Case SPI.DS.OP of

  DS_ADD_U32            :Write('DS_ADD_U32');
  DS_SUB_U32            :Write('DS_SUB_U32');
  DS_RSUB_U32           :Write('DS_RSUB_U32');
  DS_INC_U32            :Write('DS_INC_U32');
  DS_DEC_U32            :Write('DS_DEC_U32');
  DS_MIN_I32            :Write('DS_MIN_I32');
  DS_MAX_I32            :Write('DS_MAX_I32');
  DS_MIN_U32            :Write('DS_MIN_U32');
  DS_MAX_U32            :Write('DS_MAX_U32');
  DS_AND_B32            :Write('DS_AND_B32');
  DS_OR_B32             :Write('DS_OR_B32');
  DS_XOR_B32            :Write('DS_XOR_B32');
  DS_MSKOR_B32          :Write('DS_MSKOR_B32');
  DS_WRITE_B32          :Write('DS_WRITE_B32');
  DS_WRITE2_B32         :Write('DS_WRITE2_B32');
  DS_WRITE2ST64_B32     :Write('DS_WRITE2ST64_B32');
  DS_CMPST_B32          :Write('DS_CMPST_B32');
  DS_CMPST_F32          :Write('DS_CMPST_F32');
  DS_MIN_F32            :Write('DS_MIN_F32');
  DS_MAX_F32            :Write('DS_MAX_F32');
  DS_NOP                :Write('DS_NOP');
  DS_GWS_INIT           :Write('DS_GWS_INIT');
  DS_GWS_SEMA_V         :Write('DS_GWS_SEMA_V');
  DS_GWS_SEMA_BR        :Write('DS_GWS_SEMA_BR');
  DS_GWS_SEMA_P         :Write('DS_GWS_SEMA_P');
  DS_GWS_BARRIER        :Write('DS_GWS_BARRIER');
  DS_WRITE_B8           :Write('DS_WRITE_B8');
  DS_WRITE_B16          :Write('DS_WRITE_B16');
  DS_ADD_RTN_U32        :Write('DS_ADD_RTN_U32');
  DS_SUB_RTN_U32        :Write('DS_SUB_RTN_U32');
  DS_RSUB_RTN_U32       :Write('DS_RSUB_RTN_U32');
  DS_INC_RTN_U32        :Write('DS_INC_RTN_U32');
  DS_DEC_RTN_U32        :Write('DS_DEC_RTN_U32');
  DS_MIN_RTN_I32        :Write('DS_MIN_RTN_I32');
  DS_MAX_RTN_I32        :Write('DS_MAX_RTN_I32');
  DS_MIN_RTN_U32        :Write('DS_MIN_RTN_U32');
  DS_MAX_RTN_U32        :Write('DS_MAX_RTN_U32');
  DS_AND_RTN_B32        :Write('DS_AND_RTN_B32');
  DS_OR_RTN_B32         :Write('DS_OR_RTN_B32');
  DS_XOR_RTN_B32        :Write('DS_XOR_RTN_B32');
  DS_MSKOR_RTN_B32      :Write('DS_MSKOR_RTN_B32');
  DS_WRXCHG_RTN_B32     :Write('DS_WRXCHG_RTN_B32');
  DS_WRXCHG2_RTN_B32    :Write('DS_WRXCHG2_RTN_B32');
  DS_WRXCHG2ST64_RTN_B32:Write('DS_WRXCHG2ST64_RTN_B32');
  DS_CMPST_RTN_B32      :Write('DS_CMPST_RTN_B32');
  DS_CMPST_RTN_F32      :Write('DS_CMPST_RTN_F32');
  DS_MIN_RTN_F32        :Write('DS_MIN_RTN_F32');
  DS_MAX_RTN_F32        :Write('DS_MAX_RTN_F32');
  DS_WRAP_RTN_B32       :Write('DS_WRAP_RTN_B32');
  DS_SWIZZLE_B32        :Write('DS_SWIZZLE_B32');
  DS_READ_B32           :Write('DS_READ_B32');
  DS_READ2_B32          :Write('DS_READ2_B32');
  DS_READ2ST64_B32      :Write('DS_READ2ST64_B32');
  DS_READ_I8            :Write('DS_READ_I8');
  DS_READ_U8            :Write('DS_READ_U8');
  DS_READ_I16           :Write('DS_READ_I16');
  DS_READ_U16           :Write('DS_READ_U16');
  DS_CONSUME            :Write('DS_CONSUME');
  DS_APPEND             :Write('DS_APPEND');
  DS_ORDERED_COUNT      :Write('DS_ORDERED_COUNT');
  DS_ADD_U64            :Write('DS_ADD_U64');
  DS_SUB_U64            :Write('DS_SUB_U64');
  DS_RSUB_U64           :Write('DS_RSUB_U64');
  DS_INC_U64            :Write('DS_INC_U64');
  DS_DEC_U64            :Write('DS_DEC_U64');
  DS_MIN_I64            :Write('DS_MIN_I64');
  DS_MAX_I64            :Write('DS_MAX_I64');
  DS_MIN_U64            :Write('DS_MIN_U64');
  DS_MAX_U64            :Write('DS_MAX_U64');
  DS_OR_B64             :Write('DS_OR_B64');
  DS_XOR_B64            :Write('DS_XOR_B64');
  DS_MSKOR_B64          :Write('DS_MSKOR_B64');
  DS_WRITE_B64          :Write('DS_WRITE_B64');
  DS_WRITE2_B64         :Write('DS_WRITE2_B64');
  DS_WRITE2ST64_B64     :Write('DS_WRITE2ST64_B64');
  DS_CMPST_B64          :Write('DS_CMPST_B64');
  DS_CMPST_F64          :Write('DS_CMPST_F64');
  DS_MIN_F64            :Write('DS_MIN_F64');
  DS_MAX_F64            :Write('DS_MAX_F64');
  DS_ADD_RTN_U64        :Write('DS_ADD_RTN_U64');
  DS_SUB_RTN_U64        :Write('DS_SUB_RTN_U64');
  DS_RSUB_RTN_U64       :Write('DS_RSUB_RTN_U64');
  DS_INC_RTN_U64        :Write('DS_INC_RTN_U64');
  DS_DEC_RTN_U64        :Write('DS_DEC_RTN_U64');
  DS_MIN_RTN_I64        :Write('DS_MIN_RTN_I64');
  DS_MAX_RTN_I64        :Write('DS_MAX_RTN_I64');
  DS_MIN_RTN_U64        :Write('DS_MIN_RTN_U64');
  DS_MAX_RTN_U64        :Write('DS_MAX_RTN_U64');
  DS_AND_RTN_B64        :Write('DS_AND_RTN_B64');
  DS_OR_RTN_B64         :Write('DS_OR_RTN_B64');
  DS_XOR_RTN_B64        :Write('DS_XOR_RTN_B64');
  DS_MSKOR_RTN_B64      :Write('DS_MSKOR_RTN_B64');
  DS_WRXCHG_RTN_B64     :Write('DS_WRXCHG_RTN_B64');
  DS_WRXCHG2_RTN_B64    :Write('DS_WRXCHG2_RTN_B64');
  DS_WRXCHG2ST64_RTN_B64:Write('DS_WRXCHG2ST64_RTN_B64');
  DS_CMPST_RTN_B64      :Write('DS_CMPST_RTN_B64');
  DS_CMPST_RTN_F64      :Write('DS_CMPST_RTN_F64');
  DS_MIN_RTN_F64        :Write('DS_MIN_RTN_F64');
  DS_MAX_RTN_F64        :Write('DS_MAX_RTN_F64');
  DS_READ_B64           :Write('DS_READ_B64');
  DS_READ2_B64          :Write('DS_READ2_B64');
  DS_READ2ST64_B64      :Write('DS_READ2ST64_B64');
  DS_CONDXCHG32_RTN_B64 :Write('DS_CONDXCHG32_RTN_B64');
  DS_ADD_SRC2_U32       :Write('DS_ADD_SRC2_U32');
  DS_SUB_SRC2_U32       :Write('DS_SUB_SRC2_U32');
  DS_RSUB_SRC2_U32      :Write('DS_RSUB_SRC2_U32');
  DS_INC_SRC2_U32B      :Write('DS_INC_SRC2_U32B');
  DS_DEC_SRC2_U32       :Write('DS_DEC_SRC2_U32');
  DS_MIN_SRC2_I32       :Write('DS_MIN_SRC2_I32');
  DS_MAX_SRC2_I32       :Write('DS_MAX_SRC2_I32');
  DS_MIN_SRC2_U32       :Write('DS_MIN_SRC2_U32');
  DS_MAX_SRC2_U32       :Write('DS_MAX_SRC2_U32');
  DS_AND_SRC2_B32B      :Write('DS_AND_SRC2_B32B');
  DS_OR_SRC2_B32        :Write('DS_OR_SRC2_B32');
  DS_XOR_SRC2_B32       :Write('DS_XOR_SRC2_B32');
  DS_WRITE_SRC2_B32     :Write('DS_WRITE_SRC2_B32');
  DS_MIN_SRC2_F32       :Write('DS_MIN_SRC2_F32');
  DS_MAX_SRC2_F32       :Write('DS_MAX_SRC2_F32');
  DS_ADD_SRC2_U64       :Write('DS_ADD_SRC2_U64');
  DS_SUB_SRC2_U64       :Write('DS_SUB_SRC2_U64');
  DS_RSUB_SRC2_U64      :Write('DS_RSUB_SRC2_U64');
  DS_INC_SRC2_U64       :Write('DS_INC_SRC2_U64');
  DS_DEC_SRC2_U64       :Write('DS_DEC_SRC2_U64');
  DS_MIN_SRC2_I64       :Write('DS_MIN_SRC2_I64');
  DS_MAX_SRC2_I64       :Write('DS_MAX_SRC2_I64');
  DS_MIN_SRC2_U64       :Write('DS_MIN_SRC2_U64');
  DS_MAX_SRC2_U64       :Write('DS_MAX_SRC2_U64');
  DS_AND_SRC2_B64       :Write('DS_AND_SRC2_B64');
  DS_OR_SRC2_B64        :Write('DS_OR_SRC2_B64');
  DS_XOR_SRC2_B64       :Write('DS_XOR_SRC2_B64');
  DS_MIN_SRC2_F64       :Write('DS_MIN_SRC2_F64');
  DS_MAX_SRC2_F64       :Write('DS_MAX_SRC2_F64');

  else
      Writeln('DS?',SPI.DS.OP);
 end;
 Write(' ');

 //VDST vbindex vsrc0 vsrc1 OFFSET0 OFFSET1 GDS

 _print_vdst8(SPI.DS.VDST);
 Write(' ');

 //vbindex
 _print_vdst8(SPI.DS.ADDR);
 Write(' ');

 //vsrc0
 _print_vdst8(SPI.DS.DATA0);
 Write(' ');

 //vsrc1
 _print_vdst8(SPI.DS.DATA1);
 Write(' ');

 Write('OFFSET:0x',HexStr(SPI.DS.OFFSET1,2),HexStr(SPI.DS.OFFSET0,2),' ');

 Write('GDS:',SPI.DS.GDS);

 Writeln;
end;


procedure print_spi(Var SPI:TSPI);
begin
 Case SPI.CMD.EN of
  W_SOP1  :_print_SOP1(SPI);
  W_SOPC  :_print_SOPC(SPI);
  W_SOPP  :_print_SOPP(SPI);
  W_VOP1  :_print_VOP1(SPI);
  W_VOPC  :_print_VOPC(SPI);
  W_VOP3  :_print_VOP3(SPI);
  W_DS    :_print_DS(SPI);
  W_MUBUF :_print_MUBUF(SPI);
  W_MTBUF :_print_MTBUF(SPI);
  W_EXP   :_print_EXP(SPI);
  W_VINTRP:_print_VINTRP(SPI);
  W_MIMG  :_print_MIMG(SPI);
  W_SMRD  :_print_SMRD(SPI);
  W_SOPK  :_print_SOPK(SPI);
  W_SOP2  :_print_SOP2(SPI);
  W_VOP2  :_print_VOP2(SPI);
  else
   Writeln('???');
 end;
end;

function _parse_print(Body:Pointer;size_dw:DWORD=0;setpc:Boolean=false):Pointer;
var
 ShaderParser:TShaderParser;
 SPI:TSPI;
begin
 ShaderParser:=Default(TShaderParser);
 ShaderParser.Body:=Body;
 SPI:=Default(TSPI);
 repeat
  if (size_dw<>0) then
  begin
   if (ShaderParser.OFFSET_DW>=size_dw) then Break;
  end;
  Case ShaderParser.Next(SPI) of
   0:begin
      if setpc then
      begin
       Case SPI.CMD.EN of
        W_SOP1:
          Case SPI.SOP1.OP of
           S_SETPC_B64:
             begin
              print_spi(SPI);
              Break;
             end;
          end;
       end;
      end;
      print_spi(SPI);
     end;
   1:
     begin
      print_spi(SPI);
      Break;
     end;
   else
     begin
      Writeln('Shader Parse Err:',HexStr(ShaderParser.Body[ShaderParser.OFFSET_DW],8));
      Break;
     end;
  end;
 until false;
 Result:=@ShaderParser.Body[ShaderParser.OFFSET_DW];
end;

function _parse_size(Body:Pointer;size_dw:DWORD=0;setpc:Boolean=false):Pointer;
var
 ShaderParser:TShaderParser;
 SPI:TSPI;
begin
 ShaderParser:=Default(TShaderParser);
 ShaderParser.Body:=Body;
 SPI:=Default(TSPI);
 repeat
  if (size_dw<>0) then
  begin
   if (ShaderParser.OFFSET_DW>=size_dw) then Break;
  end;
  Case ShaderParser.Next(SPI) of
   0:begin
      if setpc then
      begin
       Case SPI.CMD.EN of
        W_SOP1:
          Case SPI.SOP1.OP of
           S_SETPC_B64:Break;
          end;
       end;
      end;
     end;
   1:Break;
   else
     Break;
  end;
 until false;
 Result:=@ShaderParser.Body[ShaderParser.OFFSET_DW];
end;

end.

