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

 //H_SOP1
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


 //H_SOP2
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


 //H_SOPP
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

 //SOPK
 S_CBRANCH_I_FORK=$11;

 //VOP2
 V_CNDMASK_B32       = 0;
 V_READLANE_B32      = 1;
 V_ADD_F32           = 3;
 V_SUB_F32           = 4;
 V_SUBREV_F32        = 5;
 V_MAC_LEGACY_F32    = 6;
 V_MUL_LEGACY_F32    = 7;
 V_MUL_F32           = 8;
 V_MUL_I32_I24       = 9;
 V_MUL_HI_I32_I24    =10;
 V_MUL_U32_U24       =11;
 V_MUL_HI_U32_U24    =12;
 V_MIN_LEGACY_F32    =13;
 V_MAX_LEGACY_F32    =14;
 V_MIN_F32           =15;
 V_MAX_F32           =16;
 V_MIN_I32           =17;
 V_MAX_I32           =18;
 V_MIN_U32           =19;
 V_MAX_U32           =20;
 V_LSHR_B32          =21;
 V_LSHRREV_B32       =22;
 V_ASHR_I32          =23;
 V_ASHRREV_I32       =24;
 V_LSHL_B32          =25;
 V_LSHLREV_B32       =26;
 V_AND_B32           =27;
 V_OR_B32            =28;
 V_XOR_B32           =29;
 V_BFM_B32           =30;
 V_MAC_F32           =31;
 V_MADMK_F32         =32;
 V_MADAK_F32         =33;
 V_BCNT_U32_B32      =34;
 V_MBCNT_LO_U32_B32  =35;
 V_MBCNT_HI_U32_B32  =36;
 V_ADD_I32           =37;
 V_SUB_I32           =38;
 V_SUBREV_I32        =39;
 V_ADDC_U32          =40;
 V_SUBB_U32          =41;
 V_SUBBREV_U32       =42;
 V_LDEXP_F32         =43;
 V_CVT_PKACCUM_U8_F32=44;
 V_CVT_PKNORM_I16_F32=45;
 V_CVT_PKNORM_U16_F32=46;
 V_CVT_PKRTZ_F16_F32 =47;
 V_CVT_PK_U16_U32    =48;
 V_CVT_PK_I16_I32    =49;

 //VOP3a
 V_SAD_U32=349;

 //VOP3b
 V_DIV_SCALE_F32 =365;
 V_DIV_SCALE_F64 =366;

 //SMRD
 S_LOAD_DWORD          =$0;
 S_LOAD_DWORDX2        =$1;
 S_LOAD_DWORDX4        =$2;
 S_LOAD_DWORDX8        =$3;
 S_LOAD_DWORDX16       =$4;

 S_BUFFER_LOAD_DWORD   =$8;
 S_BUFFER_LOAD_DWORDX2 =$9;
 S_BUFFER_LOAD_DWORDX4 =$A;
 S_BUFFER_LOAD_DWORDX16=$C;

 S_MEMTIME             =$1E;
 S_DCACHE_INV          =$1F;

 //VOPC
 V_CMP_F_U32   =$C0;
 V_CMP_LT_U32  =$C1;
 V_CMP_EQ_U32  =$C2;
 V_CMP_LE_U32  =$C3;
 V_CMP_GT_U32  =$C4;
 V_CMP_LG_U32  =$C5;
 V_CMP_GE_U32  =$C6;
 V_CMP_TRU_U32 =$C7;

 V_CMPX_F_U32  =$D0;
 V_CMPX_LT_U32 =$D1;
 V_CMPX_EQ_U32 =$D2;
 V_CMPX_LE_U32 =$D3;
 V_CMPX_GT_U32 =$D4;
 V_CMPX_LG_U32 =$D5;
 V_CMPX_GE_U32 =$D6;
 V_CMPX_TRU_U32=$D7;

 //VOP1
 V_MOV_B32 = $1;
 V_CVT_F32_I32=$5;

 //VINTRP
 V_INTERP_P1_F32=0;
 V_INTERP_P2_F32=1;
 V_INTERP_MOV_F32=2;

 //MUBUF
 BUFFER_LOAD_FORMAT_X    =$0;
 BUFFER_LOAD_FORMAT_XY   =$1;
 BUFFER_LOAD_FORMAT_XYZ  =$2;
 BUFFER_LOAD_FORMAT_XYZW =$3;
 BUFFER_STORE_FORMAT_X   =$4;
 BUFFER_STORE_FORMAT_XY  =$5;
 BUFFER_STORE_FORMAT_XYZ =$6;
 BUFFER_STORE_FORMAT_XYZW=$7;

 BUFFER_LOAD_UBYTE  =$8;
 BUFFER_LOAD_SBYTE  =$9;
 BUFFER_LOAD_USHORT =$A;
 BUFFER_LOAD_SSHORT =$B;
 BUFFER_LOAD_DWORD  =$C;
 BUFFER_LOAD_DWORDX2=$D;
 BUFFER_LOAD_DWORDX4=$E;
 BUFFER_LOAD_DWORDX3=$F;

 BUFFER_STORE_BYTE   =$18;
 BUFFER_STORE_SHORT  =$1A;
 BUFFER_STORE_DWORD  =$1C;
 BUFFER_STORE_DWORDX2=$1D;
 BUFFER_STORE_DWORDX4=$1E;
 BUFFER_STORE_DWORDX3=$1F;

 //MIMG
 IMAGE_LOAD_MIP=1;
 IMAGE_STORE   =8;
 IMAGE_SAMPLE  =$20;

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
  omod:bit2;    //2
  neg:bit3;     //3
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

  ADDR:Byte;
  DATA0:Byte;
  DATA1:Byte;
  VDST:Byte;
 end;

{
SOP2 32+
SOPK 32
SOP1 32+
SOPC 32+
SOPP 32

SMRD 32

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
  DW_SOPC:if (TSOPC(H).SSRC0=$FF) or (TSOPC(H).SSRC1=$FF) then pack8(TSOPC(H).OP) else pack4(TSOPC(H).OP);
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
            pack4(TSMRD(H).OP);
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
              if (TSOP2(H).SSRC0=$FF) or (TSOP2(H).SSRC1=$FF) then pack8(TSOP2(H).OP) else pack4(TSOP2(H).OP);
             end else
             begin
              T:=H and LAST_1BIT;
              if (T=DW_VOP2) then //1
              begin
               if (TVOP2(H).SRC0=$FF) then pack8(TVOP2(H).OP) else pack4(TVOP2(H).OP);
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
  255:Write(HexStr(d2,8));
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
  255:Write(HexStr(d2,8));
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
 Writeln('SOPC?',SPI.SOPC.OP);
end;

function _text_branch_offset(OFFSET_DW:DWORD;S:SmallInt):RawByteString;
begin
 Result:=HexStr((OFFSET_DW+S+1)*4,8);
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
 t:Byte;
begin
 t:=0;

 Case SPI.SMRD.OP of

  S_LOAD_DWORD:
    begin
     Write('S_LOAD_DWORD');
     t:=1;
    end;

  S_LOAD_DWORDX2:
    begin
     Write('S_LOAD_DWORDX2');
     t:=2;
    end;

  S_LOAD_DWORDX4:
    begin
     Write('S_LOAD_DWORDX4');
     t:=4;
    end;

  S_LOAD_DWORDX16:
    begin
     Write('S_LOAD_DWORDX16');
     t:=16;
    end;

  //--

  S_BUFFER_LOAD_DWORD:
    begin
     Write('S_BUFFER_LOAD_DWORD');
     t:=1;
    end;

  S_BUFFER_LOAD_DWORDX2:
    begin
     Write('S_BUFFER_LOAD_DWORDX2');
     t:=2;
    end;

  S_BUFFER_LOAD_DWORDX4:
    begin
     Write('S_BUFFER_LOAD_DWORDX4');
     t:=4;
    end;

  S_BUFFER_LOAD_DWORDX16:
    begin
     Write('S_BUFFER_LOAD_DWORDX16');
     t:=16;
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

 case t of
  1:begin
     With SPI.SMRD do
      Write('s[',SDST,'], s[',SBASE*2,':',SBASE*2+3,'], ');
    end;
  2:begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+1,'], s[',SBASE*2,':',SBASE*2+3,'], ');
    end;
  4:begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+3,'], s[',SBASE*2,':',SBASE*2+3,'], ');
    end;
  16:
    begin
     With SPI.SMRD do
      Write('s[',SDST,':',SDST+15,'], s[',SBASE*2,':',SBASE*2+3,'], ');
    end;
 end;

 Write(' ');

 With SPI.SMRD do
  Case IMM of
   0:Write('s[',HexStr(OFFSET,2),']');
   1:Write(HexStr(OFFSET,2));
  end;

 Writeln;
end;

procedure _print_SOPK(Var SPI:TSPI);
begin
 Writeln('SOPK?',SPI.SOPK.OP);
end;

procedure _print_VOP3a(Var VOP3:TVOP3a);
begin
 Case VOP3.OP of
  256+V_CNDMASK_B32       :Write('V_CNDMASK_B32');
  256+V_READLANE_B32      :Write('V_READLANE_B32');
  256+V_ADD_F32           :Write('V_ADD_F32');
  256+V_SUB_F32           :Write('V_SUB_F32');
  256+V_SUBREV_F32        :Write('V_SUBREV_F32');
  256+V_MAC_LEGACY_F32    :Write('V_MAC_LEGACY_F32');
  256+V_MUL_LEGACY_F32    :Write('V_MUL_LEGACY_F32');
  256+V_MUL_F32           :Write('V_MUL_F32');
  256+V_MUL_I32_I24       :Write('V_MUL_I32_I24');
  256+V_MUL_HI_I32_I24    :Write('V_MUL_HI_I32_I24');
  256+V_MUL_U32_U24       :Write('V_MUL_U32_U24 ');
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
  256+V_MADMK_F32         :Write('V_MADMK_F32');
  256+V_MADAK_F32         :Write('V_MADAK_F32');
  256+V_BCNT_U32_B32      :Write('V_BCNT_U32_B32');
  256+V_MBCNT_LO_U32_B32  :Write('V_MBCNT_LO_U32_B32');
  256+V_MBCNT_HI_U32_B32  :Write('V_MBCNT_HI_U32_B32');
  256+V_ADD_I32           :Write('V_ADD_I32');
  256+V_SUB_I32           :Write('V_SUB_I32');
  256+V_SUBREV_I32        :Write('V_SUBREV_I32');
  256+V_ADDC_U32          :Write('V_ADDC_U32');
  256+V_SUBB_U32          :Write('V_SUBB_U32');
  256+V_SUBBREV_U32       :Write('V_SUBBREV_U32');
  256+V_LDEXP_F32         :Write('V_LDEXP_F32');
  256+V_CVT_PKACCUM_U8_F32:Write('V_CVT_PKACCUM_U8_F32');
  256+V_CVT_PKNORM_I16_F32:Write('V_CVT_PKNORM_I16_F32');
  256+V_CVT_PKNORM_U16_F32:Write('V_CVT_PKNORM_U16_F32');
  256+V_CVT_PKRTZ_F16_F32 :Write('V_CVT_PKRTZ_F16_F32');
  256+V_CVT_PK_U16_U32    :Write('V_CVT_PK_U16_U32');
  256+V_CVT_PK_I16_I32    :Write('V_CVT_PK_I16_I32');

  V_SAD_U32:Write('V_SAD_U32');

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
 _print_ssrc9(VOP3.SRC0);
 Write(', ');
 _print_ssrc9(VOP3.SRC1);
 Write(', ');
 _print_ssrc9(VOP3.SRC2);

 if (VOP3.ABS<>0) then
  Write(' abs('+BinStr(VOP3.ABS,3)+')');

 if (VOP3.CLAMP<>0) then
  Write(' clamp');

 if (VOP3.NEG<>0) then
  Write(' neg('+BinStr(VOP3.NEG,3)+')');

 Writeln;
end;

function VOP3b_3in2out(OP:Word):Boolean; inline;
begin
 Case OP of
  V_DIV_SCALE_F32,
  V_DIV_SCALE_F64:Result:=True;
  else
   Result:=False;
 end;
end;

procedure _print_VOP3b(Var VOP3:TVOP3b);
begin
 Case VOP3.OP of

  V_CMP_F_U32   :Write('V_CMP_F_U32');
  V_CMP_LT_U32  :Write('V_CMP_LT_U32');
  V_CMP_EQ_U32  :Write('V_CMP_EQ_U32');
  V_CMP_LE_U32  :Write('V_CMP_LE_U32');
  V_CMP_GT_U32  :Write('V_CMP_GT_U32');
  V_CMP_LG_U32  :Write('V_CMP_LG_U32');
  V_CMP_GE_U32  :Write('V_CMP_GE_U32');
  V_CMP_TRU_U32 :Write('V_CMP_TRU_U32');

  V_CMPX_F_U32  :Write('V_CMPX_F_U32');
  V_CMPX_LT_U32 :Write('V_CMPX_LT_U32');
  V_CMPX_EQ_U32 :Write('V_CMPX_EQ_U32');
  V_CMPX_LE_U32 :Write('V_CMPX_LE_U32');
  V_CMPX_GT_U32 :Write('V_CMPX_GT_U32');
  V_CMPX_LG_U32 :Write('V_CMPX_LG_U32');
  V_CMPX_GE_U32 :Write('V_CMPX_GE_U32');
  V_CMPX_TRU_U32:Write('V_CMPX_TRU_U32');

  V_DIV_SCALE_F32:Write('V_DIV_SCALE_F32');
  V_DIV_SCALE_F64:Write('V_DIV_SCALE_F64');

  else
      Write('VOP3b?',VOP3.OP);
 end;
 Write(' ');

 _print_sdst7(VOP3.SDST);

 if VOP3b_3in2out(VOP3.OP) then
 begin
  Write(', ');
  _print_vdst8(VOP3.VDST);
 end;

 Case VOP3.OMOD of
  0:;
  1:Write('*2');
  2:Write('*4');
  3:Write('/2');
 end;

 Write(', ');
 _print_ssrc9(VOP3.SRC0);
 Write(', ');
 _print_ssrc9(VOP3.SRC1);

 if VOP3b_3in2out(VOP3.OP) then
 begin
  Write(', ');
  _print_ssrc9(VOP3.SRC2);
 end;

 if (VOP3.NEG<>0) then
  Write(' neg('+BinStr(VOP3.NEG,3)+')');

 Writeln;
end;

procedure _print_VOP3(Var SPI:TSPI);
begin
 Case SPI.VOP3a.OP of
    0..255,
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
  V_ADD_F32           :Write('V_ADD_F32');
  V_SUB_F32           :Write('V_SUB_F32');
  V_SUBREV_F32        :Write('V_SUBREV_F32');
  V_MAC_LEGACY_F32    :Write('V_MAC_LEGACY_F32');
  V_MUL_LEGACY_F32    :Write('V_MUL_LEGACY_F32');
  V_MUL_F32           :Write('V_MUL_F32');
  V_MUL_I32_I24       :Write('V_MUL_I32_I24');
  V_MUL_HI_I32_I24    :Write('V_MUL_HI_I32_I24');
  V_MUL_U32_U24       :Write('V_MUL_U32_U24 ');
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
 _print_vdst8(SPI.VOP2.VSRC1);

 Writeln;
end;

procedure _print_VOPC(Var SPI:TSPI);
begin
 Case SPI.VOPC.OP of

  V_CMP_F_U32   :Write('V_CMP_F_U32');
  V_CMP_LT_U32  :Write('V_CMP_LT_U32');
  V_CMP_EQ_U32  :Write('V_CMP_EQ_U32');
  V_CMP_LE_U32  :Write('V_CMP_LE_U32');
  V_CMP_GT_U32  :Write('V_CMP_GT_U32');
  V_CMP_LG_U32  :Write('V_CMP_LG_U32');
  V_CMP_GE_U32  :Write('V_CMP_GE_U32');
  V_CMP_TRU_U32 :Write('V_CMP_TRU_U32');

  V_CMPX_F_U32  :Write('V_CMPX_F_U32');
  V_CMPX_LT_U32 :Write('V_CMPX_LT_U32');
  V_CMPX_EQ_U32 :Write('V_CMPX_EQ_U32');
  V_CMPX_LE_U32 :Write('V_CMPX_LE_U32');
  V_CMPX_GT_U32 :Write('V_CMPX_GT_U32');
  V_CMPX_LG_U32 :Write('V_CMPX_LG_U32');
  V_CMPX_GE_U32 :Write('V_CMPX_GE_U32');
  V_CMPX_TRU_U32:Write('V_CMPX_TRU_U32');
  else
      Write('VOPC?',SPI.VOPC.OP);
 end;
 Write(' vcc, ');

 _print_ssrc9(SPI.VOPC.SRC0,SPI.INLINE32);
 Write(', ');
 _print_vdst8(SPI.VOPC.VSRC1);

 Writeln;
end;

procedure _print_VOP1(Var SPI:TSPI);
begin

 Case SPI.VOP1.OP of
  V_MOV_B32:Write('V_MOV_B32');
  V_CVT_F32_I32:Write('V_CVT_F32_I32');
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
 Writeln('MTBUF?',SPI.MTBUF.OP);
end;

procedure _print_MIMG(Var SPI:TSPI);
begin
 Case SPI.MIMG.OP of
   IMAGE_LOAD_MIP:Write('IMAGE_LOAD_MIP');
   IMAGE_STORE   :Write('IMAGE_STORE');
   IMAGE_SAMPLE  :Write('IMAGE_SAMPLE');
  else
      Write('MIMG?',SPI.MIMG.OP);
 end;
 Write(' ');

 _print_vdst8(SPI.MIMG.VDATA);
 Write(', ');
 _print_vdst8(SPI.MIMG.VADDR);
 Write(', ');

 Write('s[',SPI.MIMG.SRSRC*8,':',SPI.MIMG.SRSRC*8+7,']');
 Write(', ');
 Write('s[',SPI.MIMG.SSAMP*4,':',SPI.MIMG.SSAMP*4+3,']');

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
 if (SPI.EXP.COMPR<>0) and (f=$F) then f:=3;

 if (f and $1<>0) then
 begin
  Write(', ');
  _print_vdst8(SPI.EXP.VSRC0);
 end;
 if (f and $2<>0) then
 begin
  Write(', ');
  _print_vdst8(SPI.EXP.VSRC1);
 end;
 if (f and $4<>0) then
 begin
  Write(', ');
  _print_vdst8(SPI.EXP.VSRC2);
 end;
 if (f and $8<>0) then
 begin
  Write(', ');
  _print_vdst8(SPI.EXP.VSRC3);
 end;

 if SPI.EXP.COMPR<>0 then
  Write(' compr');       //is half float compressed

 if SPI.EXP.vm<>0 then   //final valid mask for this target
  Write(' vm');

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
 Writeln('DS?',SPI.DS.OP);
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

