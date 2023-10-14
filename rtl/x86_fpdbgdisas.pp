{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdisasx86.pp  -  Native Freepascal debugger - x86 Disassembler
 ---------------------------------------------------------------------------

 This unit contains a x86 disassembler for the Native Freepascal debugger

 ---------------------------------------------------------------------------

 @created(Mon Apr 22th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit x86_fpdbgdisas;
{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
interface

{.$define debug_OperandSize}

uses
  SysUtils;

{                   
  The function Disassemble decodes the instruction at the given address.
  After decoding, the address increased to the next instruction.
  The following chars are used to indicate problems with instruction
  sequenses:
  ** invalid opcode
  -- reserved opcode
  () ignored opcode
  ?? unspecified
  !! internal error, a group got called for an opcode which wasn't decoded there

  The disassembler starts to decode the first byte according to

  Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
  Table A-2. One-byte Opcode Map: (00H — F7H)
  and
  Table A-2. One-byte Opcode Map: (08H — FFH)

  The 3DNow!(tm) instructions are decoded according to
  AMD64 Architecture Programmer’s Manual Volume 3:
  Table A-13. Immediate Byte for 3DNow!(tm) Opcodes, Low Nibble 0–7h
  and
  Table A-14. Immediate Byte for 3DNow!(tm) Opcodes, Low Nibble 8–Fh

  The routines Addxx use the same abbriviations as used in those tables

}  

type
  TFPDMode = (dm32, dm64);

  THexValueFormatFlag = (hvfSigned, hvfPrefixPositive, hvfIncludeHexchar);
  THexValueFormatFlags = set of THexValueFormatFlag;

  TDBGPtr=Pointer;

  TDbgInstInfo = record
   InstrType: (itAny, itJump);
   InstrTargetOffs: Int64; // offset from the START address of instruction
  end;

  TDbgProcess=class
   Mode: TFPDMode;
   Constructor Create(M: TFPDMode);
   function ReadData(AAdress:TDbgPtr;ASize:Cardinal;out AData):Boolean; virtual;
   function ReadData(AAdress:TDbgPtr;ASize:Cardinal;out AData;out APartSize:Cardinal):Boolean; virtual;
  end;

type
  // rexB, rexX, rexR, rexW see
  // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
  // Table 2-4. REX Prefix Fields [BITS: 0100WRXB]

  TFlag = (
    flagRex, // $4x: set for any $4X
    flagSib,
    flagModRM,
    rexB,    // $4x bit 0:  Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
    rexX,    // $4x bit 1:  Extension of the SIB index field
    rexR,    // $4x bit 2:  Extension of the ModR/M reg field
    rexW,    // $4x bit 3:  64 Bit Operand Size
    pre66,   // $66:  	Operand-size override prefix  (32 and 64 bit) or SIMD prefix
    preAdr,  // $67:    Address-size override prefix
    preLock,
    preF2,   // $F2: Repeat String/input/output or SIMD prefix
    preF3,   // $F3: Repeat String/input/output or SIMD prefix
    oprDefault64,  // In 64bit mode set default operand size to 64 (d64 note in Table A-x)
    oprForce64,    // In 64bit mode set operand size to 64 (f64 note in Table A-x)
    flagVex,
    flagEvex,
    evexB,   // Broadcast/RC/SAE Context (this is NOT a rexB extension)
    evexV,   // Extension of VEX.index
    evexR,   // rexR extension
    evexX,   // rexB extension (when no SIB present)
    evexZ    // Zeroing/Merging
  );
  TFlags = set of TFlag;
  
  // Keep 8,16,32,64 together
  TOperandSize = (os0, os8, os16, os32, os64, os48, os80, os128, os256, os512, os4096);
  TAddressSize = (as16, as32, as64);
  TRegisterType = (regNone, regRip, regOne, regGeneral, regGeneralH {upper half of general register}, regMm, regXmm, regSegment, regControl, regDebug, regX87, regFlags, regBounds, regInvalid);
  TModRMType = (modReg, modMem);
  TModRMTypes = set of TModRMType;
  TSimdOpcode = (soInvalid, soNone, so66, soF2, soF3);
  TSimdOpcodes = set of TSimdOpcode;

  TInstructionFlag = (
    ifOnly32, ifOnly64, ifOnlyVex,
    ifPrefixLock, ifPrefixRep, ifPrefixRepE, ifPrefixRepNe
  );

  TOpCode = (
    OPX_InternalUnknown,
    OPX_Invalid,
    OPX_InvalidX87, OPX_ReservedX87, OPX_Not87,
    OPX_InvalidVex,
    OPX_3dnow,
    OPX_Group1a,
    OPX_Group4,
    OPX_Group5,
    OPX_Group6,
    OPX_Group7,
    OPX_Group8,
    OPX_Group9,
    OPX_Group11,
    OPX_Group12,
    OPX_Group13,
    OPX_Group14,
    OPX_Group15,
    OPX_Group16,
    OPX_GroupP,

    OPaaa, OPaad, OPaam, OPaas, OPadc, OPadcx, OPadd, OPaddsub, OPadox, OPaesdec,
    OPaesdec128kl, OPaesdec256kl, OPaesdeclast, OPaesdecwide128kl, OPaesdecwide256kl,
    OPaesenc, OPaesenc128kl, OPaesenc256kl, OPaesenclast, OPaesencwide128kl,
    OPaesencwide256kl, OPaesimc, OPaeskeygenassist, OPand, OPandn, OParpl,

    OPbextr, OPblend, OPblendv, OPblsi, OPblsmsk, OPblsr, OPbndcl, OPbndcn,
    OPbndcu, OPbndldx, OPbndmk, OPbndmov, OPbndstx, OPbound, OPbsf, OPbsr,
    OPbswap, OPbt, OPbtc, OPbtr, OPbts, OPbzhi,

    OPcall, OPcbw, OPcdq, OPcdqe, OPclac, OPclc, OPcld, OPcldemote, OPclflush,
    OPclflushopt, OPclgi, OPcli, OPclrssbsy, OPclts, OPclwb, OPcmc, OPcmov__, OPcmp,
    OPcmps, OPcmpxchg, OPcomi, OPcpuid, OPcqo, OPcrc32, OPcvtdq2, OPcvtpd2, OPcvtpi2,
    OPcvtps2, OPcvtsd2, OPcvtsi2, OPcvtss2, OPcvttpd2, OPcvttps2, OPcvttsd2,
    OPcvttss2, OPcwd, OPcwde,

    OPdaa, OPdas, OPdec, OPdiv, OPdp,

    OPemms, OPencls, OPenclu, OPencodekey128, OPencodekey256, OPendbr32, OPendbr64,
    OPenter, OPextract,

    OPf2xm1, OPfabs, OPfadd, OPfbld, OPfbstp, OPfchs, OPfclex, OPfcmov__, OPfcom,
    OPfcos, OPfdecstp, OPfdiv, OPfdivr, OPfemms, OPffree, OPfiadd, OPficom, OPfidiv, OPfidivr,
    OPfild, OPfimul, OPfincstp, OPfinit, OPfist, OPfisttp, OPfisub, OPfisubr, OPfld,
    OPfld1, OPfldcw, OPfldenv, OPfldl2e, OPfldl2t, OPfldlg2, OPfldln2, OPfldpi,
    OPfldz, OPfmul, OPfnclex, OPfninit, OPfnop, OPfnsave, OPfnstcw, OPfnstenv,
    OPfnstsw, OPfpatan, OPfprem, OPfprem1, OPfptan, OPfrndint, OPfrstor, OPfsave,
    OPfscale, OPfsin, OPfsincos, OPfsqrt, OPfst, OPfstcw, OPfstenv, OPfstsw, OPfsub,
    OPfsubr, OPftst, OPfucom, OPfwait, OPfxam, OPfxch, OPfxrstor, OPfxsave, OPfxtract,
    OPfyl2x, OPfyl2xp1, OPgf2p8affineinvqb, OPgf2p8affineqb, OPgf2p8mulb,

    OPgetbv, OPgetsec,

    OPhadd, OPhlt, OPhreset, OPhsub,

    OPidiv, OPimul, OPin, OPinc, OPincss, OPins, OPinsert, OPint, OPint1, OPint3,
    OPinto, OPinvd, OPinvept, OPinvlpg, OPinvlpga, OPinvpcid, OPinvvpid, OPiret,

    OPj__, OPjcxz, OPjecxz, OPjmp, OPjmpe, OPjrcxz,

    OPkadd, OPkand, OPkandn, OPkmov, OPknot, OPkor, OPkortest, OPkshiftl, OPkshiftr,
    OPktest, OPkunpckbw, OPkunpckdq, OPkunpckwd, OPkxnor, OPkxor,

    OPlahf, OPlar, OPlddqu, OPldmxcsr, OPlds, OPlea, OPleave, OPles, OPlfence,
    OPlfs, OPlgdt, OPlgs, OPlidt, OPlldt, OPlmsw, OPloadiwkey, OPlock, OPlods,
    OPloop, OPlsl, OPlss, OPltr, OPlzcnt,

    OPmaskmov, OPmax, OPmcommit, OPmfence, OPmin, OPmonitor, OPmov, OPmova, OPmovbe,
    OPmovddup, OPmovdir64b, OPmovdiri, OPmovdq2q, OPmovh, OPmovhlps, OPmovl, OPmovlh,
    OPmovmsk, OPmovnt, OPmovq2dq, OPmovs, OPmovshdup, OPmovsldup, OPmovsx,
    OPmovu, OPmovzx, OPmpsadbw, OPmul, OPmwait,

    OPneg, OPnop, OPnot,

    OPor, OPout, OPouts,

    OPextrq,

    OPpabs, OPpackssdw, OPpacksswb, OPpackusdw, OPpackuswb, OPpadd, OPpadds,
    OPpaddus, OPpalignr, OPpand, OPpandn, OPpause, OPpavg, OPpavgusb, OPpblendvb,
    OPpblend, OPpclmulqdq, OPpcmpeq, OPpcmpestri, OPpcmpestrm, OPpcmpgt, OPpcmpistri,
    OPpcmpistrm, OPpconfig, OPpdep, OPpext, OPpextr, OPpf2id, OPpf2iw, OPpfacc,
    OPpfadd, OPpfcmpeq, OPpfcmpge, OPpfcmpgt, OPpfmax, OPpfmin, OPpfmul, OPpfnacc,
    OPpfpnacc, OPpfrcp, OPpfrcpit1, OPpfrcpit2, OPpfrsqit1, OPpfrsqrt, OPpfsub,
    OPpfsubr, OPphadd, OPphaddsw, OPphminposuw, OPphsub, OPphsubsw, OPpi2fd,
    OPpi2fw, OPpinsr, OPpmaddubsw, OPpmaddwd, OPpmaxs, OPpmaxu, OPpmins, OPpminu,
    OPpmovmskb, OPpmovsx, OPpmovzx, OPpmuldq, OPpmulhrsw, OPpmulhrw, OPpmulhuw,
    OPpmulhw, OPpmull, OPpmuludq, OPpop, OPpopa, OPpopad, OPpopcnt, OPpopf, OPpor,
    OPprefetch, OPpsadbw, OPpshuf, OPpsign, OPpsll, OPpsmash, OPpsra,
    OPpsrl, OPpsub, OPpsubs, OPpsubus, OPpswapd, OPptest, OPptwrite, OPpunpckhbw,
    OPpunpckhdq, OPpunpckhqdq, OPpunpckhwd, OPpunpcklbw, OPpunpckldq, OPpunpcklqdq,
    OPpunpcklwd, OPpush, OPpusha, OPpushf, OPpvalidate, OPpxor,

    OPrcl, OPrcp, OPrcr, OPrdfsbase, OPrdgsbase, OPrdmsr, OPrdpid, OPrdpkru,
    OPrdpmc, OPrdpru, OPrdrand, OPrdseed, OPrdss, OPrdtsc, OPrep, OPret, OPretf,
    OPrmpadjust, OPrmpupdate,OProl, OPror, OProrx, OPround, OPrsm, OPrsqrt, OPrstorssp,

    OPsahf, OPsal, OPsalc, OPsar, OPsaveprevssp, OPsbb, OPscas, OPserialize, OPset__,
    OPsetbv, OPsetssbsy, OPsfence, OPsgdt, OPsha1msg1, OPsha1msg2, OPsha1nexte,
    OPsha1rnds4, OPsha256msg1, OPsha256msg2, OPsha256rnds2, OPshl, OPshr, OPshuf,
    OPsidt, OPskinit, OPsldt, OPsmsw, OPsqrt, OPstac, OPstc, OPstd, OPstgi, OPsti,
    OPstmxcsr, OPstos, OPstr, OPsub, OPswapgs, OPsyscall, OPsysenter, OPsysexit,
    OPsysret,

    OPtest, OPtpause, OPtzcnt,

    OPucomi, OPud0, OPud1, OPud2, OPumonitor, OPumwait, OPunpckh, OPunpckl,

    OPvalign, OPvblendm, OPvbroadcast, OPvcompress, OPvcvtne2ps2bf16, OPvcvtneps2bf16,
    OPvcvtph2ps, OPvcvtqq2, OPvcvtsd2usi, OPvcvtss2usi,
    OPvcvttpd2, OPvcvttps2, OPvcvttsd2usi, OPvcvttss2usi, OPvcvtudq2, OPvcvtuqq2,
    OPvcvtusi2, OPvdbpsadbw, OPvdpbf16ps, OPverr, OPverw, OPvexpand,
    OPvfixupimm, OPvfmadd132, OPvfmadd213, OPvfmadd231, OPvfmaddsub132, OPvfmaddsub213,
    OPvfmaddsub231, OPvfmsub132, OPvfmsub213, OPvfmsub231, OPvfmsubadd132,
    OPvfmsubadd213, OPvfmsubadd231, OPvfnmadd132, OPvfnmadd213, OPvfnmadd231,
    OPvfnmsub132, OPvfnmsub213, OPvfnmsub231, OPvfpclass, OPvgatherd, OPvgatherq,
    OPvgetexp, OPvgetmant, OPvmaskmov, OPvmcall, OPvmclear, OPvmfunc,
    OPvmgexit, OPvmlaunch, OPvmload, OPvmmcall, OPvmptrld, OPvmptrst, OPvmread,
    OPvmresume, OPvmrun, OPvmsave, OPvmwrite, OPvmxoff, OPvmxon, OPvp2intersect,
    OPvpblendm, OPvpbroadcast, OPvpbroadcastm, OPvpcmp, OPvpcmpu, OPvpcompress,
    OPvpconflict, OPvpdpbusd, OPvpdpbusds, OPvpdpwssd, OPvpdpwssds, OPvperm, OPvperm2,
    OPvpermi2, OPvpermil, OPvpermt2, OPvpexpand,OPvpgather, OPvplzcnt, OPvpmadd52huq,
    OPvpmadd52luq, OPvpmaskmov, OPvpmovb2m, OPvpmovd, OPvpmovd2m, OPvpmovm2, OPvpmovq,
    OPvpmovq2m, OPvpmovs, OPvpmovus,
    OPvpmovw2m, OPvpmovwb, OPvpmultishiftqb, OPvpopcnt, OPvprol, OPvprolv, OPvpror,
    OPvprorv, OPvpscatter, OPvpshld, OPvpshldv, OPvpshrd, OPvpshrdv, OPvpshufbitqmb,
    OPvpsllv, OPvpsrav, OPvpsrlv, OPvpternlog, OPvptestm, OPvptestnm, OPvrange,
    OPvrcp14, OPvreduce, OPvrndscale, OPvrsqrt14, OPvscalef, OPvscatterd, OPvscatterq,
    OPvshuf, OPvtest, OPvzeroall, OPvzeroupper,

    OPwbinvd, OPwbnoinvd, OPwrfsbase, OPwrgsbase, OPwrmsr, OPwrpkru, OPwrss,
    OPwruss,

    OPxabort, OPxacquire, OPxadd, OPxbegin, OPxchg, OPxend, OPxgetbv, OPxlat, OPxor,
    OPxrelease, OPxrstor, OPxrstors, OPxsave, OPxsavec, OPxsaveopt, OPxsaves, OPxsetbv,
    OPxtest
  );

  TOpCodeSuffix = (
    OPSnone,
    // Condition
    OPSc_o, OPSc_no, OPSc_b, OPSc_nb, OPSc_z, OPSc_nz, OPSc_be, OPSc_nbe,
    OPSc_s, OPSc_ns, OPSc_p, OPSc_np, OPSc_l, OPSc_nl, OPSc_le, OPSc_nle,
    OPSc_e, OPSc_ne, OPSc_u, OPSc_nu,
    // Prefetch
    OPSp_t0, OPSp_t1, OPSp_t2, OPSp_nta, OPSp_w,
    // Generic size
    OPSx_8b, OPSx_16b, OPSx_b, OPSx_d, OPSx_dd, OPSx_dq, OPSx_dqa, OPSx_dqa32,
    OPSx_dqa64, OPSx_dqu, OPSx_dqu8, OPSx_dqu16, OPSx_dqu32, OPSx_dqu64,
    OPSx_f32x4, OPSx_f32x8, OPSx_f64x2, OPSx_f64x4, OPSx_f128, OPSx_hw, OPSx_i,
    OPSx_i32x4, OPSx_i32x8, OPSx_i64x2, OPSx_i64x4, OPSx_i128, OPSx_ip, OPSx_lw,
    OPSx_p, OPSx_pd, OPSx_ph, OPSx_pi, OPSx_pp, OPSx_pq, OPSx_ps, OPSx_q, OPSx_qd,
    OPSx_qq, OPSx_sd, OPSx_si, OPSx_ss, OPSx_udq, OPSx_uqq, OPSx_w, OPSx_x,
    // Conversion
    OPSv_bw, OPSv_bd, OPSv_bq, OPSv_wd, OPSv_wq, OPSv_dq
  );

  TOpcodePrefix = (
    OPPnone,
    OPPv
  );

  TFullOpcode = record
    Prefix: TOpcodePrefix;
    Opcode: TOpcode;
    Suffix: TOpcodeSuffix;
  end;

  TOperandFlag = (ofMemory);
  TOperandFlags = set of TOperandFlag;

  TRegValue = object
   AType : TRegisterType;
   ASize : TOperandSize;
   AIndex: Byte;
   AScale: Byte;
   //
   function StrValue:RawByteString;
  end;

  TRegValues=array[0..1] of TRegValue;

  TOperand = object
    CodeIndex: integer;
    RegValue: TRegValues;
    Size: TOperandSize;
    ByteCount: Word;
    ByteCount2: Byte;
    FormatFlags: THexValueFormatFlags;
    Flags: TOperandFlags;
    //
    function StrValue:RawByteString;
  end;

  TInstruction = object
    OpCode: TFullOpcode;
    Flags: set of TInstructionFlag;
    SegmentReg: ShortInt;
    MaskIndex: Byte;

    Operand: array[1..4] of TOperand;
    OperCnt: Integer;

    ParseFlags: TFlags;
    //
    function SegmentStr:RawByteString;
  end;
  PInstruction = ^TInstruction;

  { TX86Disassembler }

  TX86Disassembler = object
  public
    ProcessMode: TFPDMode;
    Code: PByte;
    opcode: Byte;
    CodeIdx: Byte;
    OperIdx: Integer;
    ModRMIdx: Byte;
    mm: Byte;
    Flags: TFlags;
    SimdOpcode: TSimdOpcode;
    ModRM: record
      Mode: Byte;
      Index: Byte;
      RM: Byte;
    end;
    Vex: record
      Index: Byte;
      VectorLength: TOperandSize;
      MaskIndex: Byte;
    end;
    AddrSize: TAddressSize;
    Sib: record
      Scale, Index, Base: Byte;
    end;

    //--- result ---
    Instruction: PInstruction;

    //--- add operands ---
    // the the Zz as used in AddZz routines are encoded according to:
    //
    // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
    //  A.2 KEY TO ABBREVIATIONS
    //
    // and
    //
    // AMD64 Architecture Programmer’s Manual Volume 3:
    //  Appendix A Opcode and Operand Encodings
    //
    //---
    procedure AddAp;
    procedure AddBy;
    procedure AddCd;
    procedure AddDd;
    procedure AddEb;
    procedure AddEd;
    procedure AddEp;
    procedure AddEv;
    procedure AddEw;
    procedure AddEy;
    procedure AddFv;
    procedure AddGb;
    procedure AddGd;
    procedure AddGv;
    procedure AddGw;
    procedure AddGy;
    procedure AddGz;
    procedure AddHdq;
    procedure AddHpd;
    procedure AddHps;
    procedure AddHsd;
    procedure AddHss;
    procedure AddHx;
    procedure AddHq;
    procedure AddHqq;
    procedure AddIb;
    procedure AddIv;
    procedure AddIw;
    procedure AddIz;
    procedure AddJb;
    procedure AddJz;
    procedure AddLx;
    procedure AddM;
    procedure AddMa;
    procedure AddMb;
    procedure AddMd;
    procedure AddMdq;
    procedure AddMp;
    procedure AddMps;
    procedure AddMpd;
    procedure AddMq;
    procedure AddMs;
    procedure AddMw;
    procedure AddMw_Rv;
    procedure AddMx;
    procedure AddMy;
    procedure AddNq;
    procedure AddOb;
    procedure AddOv;
    procedure AddPd;
    procedure AddPpi;
    procedure AddPq;
    procedure AddPy;
    procedure AddQd;
    procedure AddQpi;
    procedure AddQq;
    procedure AddRd;
    procedure AddRd_Mb;
    procedure AddRd_q;
    procedure AddRy;
    procedure AddRy_Mb;
    procedure AddRy_Mw;
    procedure AddRv;
    procedure AddSw;
    procedure AddUdq;
    procedure AddUdq_Md;
    procedure AddUpd;
    procedure AddUps;
    procedure AddUq;
    procedure AddUx;
    procedure AddUx_Md;
    procedure AddUx_Mq;
    procedure AddUx_Mw;
    procedure AddVdq;
    procedure AddVpd;
    procedure AddVps;
    procedure AddVq;
    procedure AddVqq;
    procedure AddVsd;
    procedure AddVss;
    procedure AddVx;
    procedure AddVy;
    procedure AddWd;
    procedure AddWdq;
    procedure AddWpd;
    procedure AddWps;
    procedure AddWq;
    procedure AddWqq;
    procedure AddWsd;
    procedure AddWss;
    procedure AddWx;
    //---

    procedure AddReg(AType: TRegisterType; ASize: TOperandSize; AIndex: Byte);
    procedure AddOpcReg(AType: TRegisterType; ASize: TOperandSize; AIndex: Byte);
    procedure AddModReg(AType: TRegisterType; ASize: TOperandSize);
    procedure AddModRM(AReqTypes: TModRMTypes; ASize: TOperandSize; AType: TRegisterType);
    procedure AddVexReg(AType: TRegisterType; ASize: TOperandSize);
    procedure AddOperand(RegValue:TRegValues; ASize: TOperandSize; AByteCount: Word = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []; AByteCount2: Byte = 0);
    procedure AddOperand(RegValue:TRegValues; AByteCount: Word=0;  AFormatFlags: THexValueFormatFlags=[]; AFlags: TOperandFlags=[]);
    procedure AddStdOperands(AIndex: Byte);

    procedure Check32;
    procedure Check64;
    procedure CheckVex;
    procedure CheckLock;
    procedure CheckRepeat;
    procedure CheckRepeatX;

    procedure ClearSIMDPrefix;
    procedure DecodeSIMD(AValidClear: TSimdOpcodes = [soNone, so66, soF2, soF3]; ASizeOK: Boolean = False);
    procedure DecodeModRM;

    procedure Do2ByteOpcode;
    procedure Do3ByteOpcode38;
    procedure Do3ByteOpcode3A;
    procedure Do3DNow;
    procedure DoDisassemble;
    procedure DoGroup1;
    procedure DoGroup2;
    procedure DoGroup3;
    procedure DoGroup4;
    procedure DoGroup5;
    procedure DoGroup6;
    procedure DoGroup7;
    procedure DoGroup8;
    procedure DoGroup9;
    procedure DoGroup10;
    procedure DoGroup11;
    procedure DoGroup12;
    procedure DoGroup13;
    procedure DoGroup14;
    procedure DoGroup15;
    procedure DoGroup16;
    procedure DoGroup17;
    procedure DoGroupP;
    procedure DoX87;
    procedure DoVex(ASize: Byte);

    function AddressSize: TAddressSize;
    function HasOpcode: Boolean;
    procedure SetOpcode(AOpcode: TOpcode; ASuffix: TOpCodeSuffix = OPSnone; APrefixV: Boolean = False);
    procedure Default64;
    procedure Force64;
    function Ignore64Reg(s: ShortInt): ShortInt;
    function OperandSize: TOperandSize;
    function StdCond(AIndex: Byte): TOpCodeSuffix;
    function VectorSize: TOperandSize;

    function OPS_Wxx(AWIG, AW0, AW1: TOpCodeSuffix): TOpCodeSuffix;
    function OPS_d_q: TOpCodeSuffix;
    function OPS_ps_d: TOpCodeSuffix;
    function OPS_ss_d: TOpCodeSuffix;
  public
    procedure Disassemble(AMode: TFPDMode; var AAddress: Pointer; out AnInstruction: TInstruction);
  end;

  TX86AsmDecoder = class;

  { TX86AsmInstruction }

  TX86AsmInstruction = class
  const
    INSTR_CODEBIN_LEN = 16;
  private
    FProcess: TDbgProcess;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FInstruction: TInstruction;
    FInstrLen: Integer;
    FFlags: set of (diCodeRead, diCodeReadError, diDisAss);
  protected
    procedure ReadCode; inline;
    procedure Disassemble; inline;
  public
    constructor Create(AProcess: TDbgProcess);
    procedure SetAddress(AnAddress: TDBGPtr);
    function IsCallInstruction: boolean;
    function IsReturnInstruction: boolean;
    function IsLeaveStackFrame: boolean;
    function ModifiesStackPointer: boolean;
    function IsJumpInstruction(IncludeConditional: Boolean = True; IncludeUncoditional: Boolean = True): boolean;
    function InstructionLength: Integer;
    function X86OpCode: TOpCode;
    property X86Instruction: TInstruction read FInstruction; // only valid after call to X86OpCode
  end;

  TDbgAsmInstruction=TX86AsmInstruction;

  { TX86AsmDecoder }

  TX86AsmDecoder = class
  private const
    MAX_CODEBIN_LEN = 50;
    //FMaxInstructionSize = 16;
    //FMinInstructionSize = 1;
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TX86AsmInstruction;
  protected
    function GetLastErrorWasMemReadErr: Boolean;
    function GetMaxInstrSize: integer;
    function GetMinInstrSize: integer;
    function GetCanReverseDisassemble: boolean;
    function ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal): Boolean; inline;
  public
    Disassembler: TX86Disassembler;
    Instr: TInstruction;

    constructor Create(AProcess: TDbgProcess);

    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: RawByteString; out ACode: RawByteString; out AnInfo: TDbgInstInfo);
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: RawByteString; out ACode: RawByteString);
    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction;

    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out
      AnIsOutsideFrame: Boolean): Boolean;
  end;

function RegName(AType: TRegisterType; ASize: TOperandSize; AIndex: Byte): RawByteString;

implementation

const
  ADDRESS_BYTES: array[TAddressSize] of Byte = (2, 4, 8);
  OPERAND_BYTES: array[TOperandSize] of Word = (0, 1,  2,  4,  8,   6, 10,  16,  32,  64,  512);
  OPERAND_BITS:  array[TOperandSize] of Word = (0, 8, 16, 32, 64,  48, 80, 128, 256, 512, 4096);
  ADDRESS_SIZE:  array[TAddressSize] of TOperandSize = (os16, os32, os64);
  REXOFFSET: array[Boolean] of Byte = (0, 8);
  VEXOFFSET: array[Boolean] of Byte = (0, 16);
  MODE_SIZE:  array[TFPDMode] of TOperandSize = (os32, os64);

  REG_A = 0;
  REG_C = 1;
  REG_D = 2;
  REG_B = 3;
  REG_SP = 4;
  REG_BP = 5;
  REG_SI = 6;
  REG_DI = 7;

  REG_ES = 0;
  REG_CS = 1;
  REG_SS = 2;
  REG_DS = 3;
  REG_FS = 4;
  REG_GS = 5;

  OPCODE_NAME: array [TOpCode] of RawByteString = (
       '???', // OPX_InternalUnknown
       '???',
      '**x87**', '-x87-', '!x87!',
      '**vex**',
      '-3dnow-',
      '**group1a**',
      '**group4**',
      '**group5**',
      '**group6**',
      '**group7**',
      '**group8**',
      '**group9**',
      '**group11**',
      '**group12**',
      '**group13**',
      '**group14**',
      '**group15**',
      '**group16**',
      '**groupp**',

      'aaa', 'aad', 'aam', 'aas', 'adc', 'adcx', 'add', 'addsub', 'adox', 'aesdec',
      'aesdec128kl', 'aesdec256kl', 'aesdeclast', 'aesdecwide128kl', 'aesdecwide256kl',
      'aesenc', 'aesenc128kl', 'aesenc256kl', 'aesenclast', 'aesencwide128kl',
      'aesencwide256kl', 'aesimc', 'aeskeygenassist', 'and', 'andn', 'arpl',

      'bextr', 'blend', 'blendv', 'blsi', 'blsmsk', 'blsr', 'bndcl', 'bndcn',
      'bndcu', 'bndldx', 'bndmk', 'bndmov', 'bndstx', 'bound', 'bsf', 'bsr',
      'bswap', 'bt', 'btc', 'btr', 'bts', 'bzhi',

      'call', 'cbw', 'cdq', 'cdqe', 'clac', 'clc', 'cld', 'cldemote', 'clflush',
      'clflushopt', 'clgi', 'cli', 'clrssbsy', 'clts', 'clwb', 'cmc', 'cmov',
      'cmp', 'cmps', 'cmpxchg', 'comi', 'cpuid', 'cqo', 'crc32', 'cvtdq2', 'cvtpd2',
      'cvtpi2', 'cvtps2', 'cvtsd2', 'cvtsi2', 'cvtss2', 'cvttpd2', 'cvttps2',
      'cvttsd2','cvttss2', 'cwd', 'cwde',

      'daa', 'das', 'dec', 'div', 'dp',

      'emms', 'encls', 'enclu', 'encodekey128', 'encodekey256', 'endbr32', 'endbr64',
      'enter', 'extract',

      'f2xm1', 'fabs', 'fadd', 'fbld', 'fbstp', 'fchs', 'fclex', 'fcmov', 'fcom',
      'fcos', 'fdecstp', 'fdiv', 'fdivr', 'femms', 'ffree', 'fiadd', 'ficom', 'fidiv', 'fidivr',
      'fild', 'fimul', 'fincstp', 'finit', 'fist', 'fisttp', 'fisub', 'fisubr', 'fld',
      'fld1', 'fldcw', 'fldenv', 'fldl2e', 'fldl2t', 'fldlg2', 'fldln2', 'fldpi',
      'fldz', 'fmul', 'fnclex', 'fninit', 'fnop', 'fnsave', 'fnstcw', 'fnstenv',
      'fnstsw', 'fpatan', 'fprem', 'fprem1', 'fptan', 'frndint', 'frstor', 'fsave',
      'fscale', 'fsin', 'fsincos', 'fsqrt', 'fst', 'fstcw', 'fstenv', 'fstsw', 'fsub',
      'fsubr', 'ftst', 'fucom', 'fwait', 'fxam', 'fxch', 'fxrstor', 'fxsave', 'fxtract',
      'fyl2x', 'fyl2xp1', 'gf2p8affineinvqb', 'gf2p8affineqb', 'gf2p8mulb',

      'getbv', 'getsec',

      'hadd', 'hlt', 'hreset', 'hsub',

      'idiv', 'imul', 'in', 'inc', 'incss', 'ins', 'insert', 'int', 'int1', 'int3',
      'into', 'invd', 'invept', 'invlpg', 'invlpga', 'invpcid', 'invvpid', 'iret',

      'j', 'jcxz', 'jecxz', 'jmp', 'jmpe', 'jrcxz',

      'kadd', 'kand', 'kandn', 'kmov', 'knot', 'kor', 'kortest', 'kshiftl', 'kshiftr',
      'ktest', 'kunpckbw', 'kunpckdq', 'kunpckwd', 'kxnor', 'kxor',

      'lahf', 'lar', 'lddqu', 'ldmxcsr', 'lds', 'lea', 'leave', 'les', 'lfence',
      'lfs', 'lgdt', 'lgs', 'lidt', 'lldt', 'lmsw', 'loadiwkey', 'lock', 'lods',
      'loop', 'lsl', 'lss', 'ltr', 'lzcnt',

      'maskmov', 'max', 'mcommit', 'mfence', 'min', 'monitor', 'mov', 'mova', 'movbe',
      'movddup', 'movdir64b', 'movdiri', 'movdq2q', 'movh', 'movhlps', 'movl', 'movlh',
      'movmsk', 'movnt', 'movq2dq', 'movs', 'movshdup', 'movsldup', 'movsx',
      'movu', 'movzx', 'mpsadbw', 'mul', 'mwait',

      'neg', 'nop', 'not',

      'or', 'out', 'outs',

      'extrq',

      'pabs', 'packssdw', 'packsswb', 'packusdw', 'packuswb', 'padd', 'padds',
      'paddus', 'palignr', 'pand', 'pandn', 'pause', 'pavg', 'pavgusb', 'pblendvb',
      'pblend', 'pclmulqdq', 'pcmpeq', 'pcmpestri', 'pcmpestrm', 'pcmpgt', 'pcmpistri',
      'pcmpistrm', 'pconfig', 'pdep', 'pext', 'pextr', 'pf2id', 'pf2iw', 'pfacc',
      'pfadd', 'pfcmpeq', 'pfcmpge', 'pfcmpgt', 'pfmax', 'pfmin', 'pfmul', 'pfnacc',
      'pfpnacc', 'pfrcp', 'pfrcpit1', 'pfrcpit2', 'pfrsqit1', 'pfrsqrt', 'pfsub',
      'pfsubr', 'phadd', 'phaddsw', 'phminposuw', 'phsub', 'phsubsw', 'pi2fd',
      'pi2fw', 'pinsr', 'pmaddubsw', 'pmaddwd', 'pmaxs', 'pmaxu', 'pmins', 'pminu',
      'pmovmskb', 'pmovsx', 'pmovzx', 'pmuldq', 'pmulhrsw', 'pmulhrw', 'pmulhuw',
      'pmulhw', 'pmull', 'pmuludq', 'pop', 'popa', 'popad', 'popcnt', 'popf', 'por',
      'prefetch', 'psadbw', 'pshuf', 'psign', 'psll', 'psmash', 'psra',
      'psrl', 'psub', 'psubs', 'psubus', 'pswapd', 'ptest', 'ptwrite', 'punpckhbw',
      'punpckhdq', 'punpckhqdq', 'punpckhwd', 'punpcklbw', 'punpckldq', 'punpcklqdq',
      'punpcklwd', 'push', 'pusha', 'pushf', 'pvalidate', 'pxor',

      'rcl', 'rcp', 'rcr', 'rdfsbase', 'rdgsbase', 'rdmsr', 'rdpid', 'rdpkru',
      'rdpmc', 'rdpru', 'rdrand', 'rdseed', 'rdss', 'rdtsc', 'rep', 'ret', 'retf',
      'rmpadjust', 'rmpupdate','rol', 'ror', 'rorx', 'round', 'rsm', 'rsqrt', 'rstorssp',

      'sahf', 'sal', 'salc', 'sar', 'saveprevssp', 'sbb', 'scas', 'serialize', 'set',
      'setbv', 'setssbsy', 'sfence', 'sgdt', 'sha1msg1', 'sha1msg2', 'sha1nexte',
      'sha1rnds4', 'sha256msg1', 'sha256msg2', 'sha256rnds2', 'shl', 'shr', 'shuf',
      'sidt', 'skinit', 'sldt', 'smsw', 'sqrt', 'stac', 'stc', 'std', 'stgi', 'sti',
      'stmxcsr', 'stos', 'str', 'sub', 'swapgs', 'syscall', 'sysenter', 'sysexit',
      'sysret',

      'test', 'tpause', 'tzcnt',

      'ucomi', 'ud0', 'ud1', 'ud2', 'umonitor', 'umwait', 'unpckh', 'unpckl',

      'valign', 'vblendm', 'vbroadcast', 'vcompress', 'vcvtne2ps2bf16', 'vcvtneps2bf16',
      'vcvtph2ps', 'vcvtqq2', 'vcvtsd2usi', 'vcvtss2usi',
      'vcvttpd2', 'vcvttps2', 'vcvttsd2usi', 'vcvttss2usi', 'vcvtudq2', 'vcvtuqq2',
      'vcvtusi2', 'vdbpsadbw', 'vdpbf16ps', 'verr', 'verw', 'vexpand',
      'vfixupimm', 'vfmadd132', 'vfmadd213', 'vfmadd231', 'vfmaddsub132', 'vfmaddsub213',
      'vfmaddsub231', 'vfmsub132', 'vfmsub213', 'vfmsub231', 'vfmsubadd132',
      'vfmsubadd213', 'vfmsubadd231', 'vfnmadd132', 'vfnmadd213', 'vfnmadd231',
      'vfnmsub132', 'vfnmsub213', 'vfnmsub231', 'vfpclass', 'vgatherd', 'vgatherq',
      'vgetexp', 'vgetmant', 'vmaskmov', 'vmcall', 'vmclear', 'vmfunc',
      'vmgexit', 'vmlaunch', 'vmload', 'vmmcall', 'vmptrld', 'vmptrst', 'vmread',
      'vmresume', 'vmrun', 'vmsave', 'vmwrite', 'vmxoff', 'vmxon', 'vp2intersect',
      'vpblendm', 'vpbroadcast', 'vpbroadcastm', 'vpcmp', 'vpcmpu',
      'vpcompress', 'vpconflict', 'vpdpbusd', 'vpdpbusds', 'vpdpwssd', 'vpdpwssds',
      'vperm', 'vperm2', 'vpermi2', 'vpermil', 'vpermt2', 'vpexpand','vpgather',
      'vplzcnt', 'vpmadd52huq', 'vpmadd52luq', 'vpmaskmov', 'vpmovb2m', 'vpmovd',
      'vpmovd2m', 'vpmovm2', 'vpmovq', 'vpmovq2m', 'vpmovs', 'vpmovus', 'vpmovw2m',
      'vpmovwb', 'vpmultishiftqb', 'vpopcnt', 'vprol', 'vprolv', 'vpror', 'vprorv',
      'vpscatter', 'vpshld', 'vpshldv', 'vpshrd', 'vpshrdv', 'vpshufbitqmb', 'vpsllv',
      'vpsrav', 'vpsrlv', 'vpternlog', 'vptestm', 'vptestnm', 'vrange', 'vrcp14',
      'vreduce', 'vrndscale', 'vrsqrt14', 'vscalef', 'vscatterd', 'vscatterq',
      'vshuf', 'vtest', 'vzeroall', 'vzeroupper',

      'wbinvd', 'wbnoinvd', 'wrfsbase', 'wrgsbase', 'wrmsr', 'wrpkru', 'wrss',
      'wruss',

      'xabort', 'xacquire', 'xadd', 'xbegin', 'xchg', 'xend', 'xgetbv', 'xlat', 'xor',
      'xrelease', 'xrstor', 'xrstors', 'xsave', 'xsavec', 'xsaveopt', 'xsaves', 'xsetbv',
      'xtest'
  );
  OPCODE_SUFFIX: array [TOpCodeSuffix] of RawByteString = (
    '',
    'o', 'no', 'b', 'nb', 'z', 'nz', 'be', 'nbe', 's', 'ns', 'p', 'np', 'l', 'nl', 'le', 'nle',
    'e', 'ne', 'u', 'nu',
    //
    't0', 't1', 't2', 'nta', 'w',
    '8b', '16b', 'b', 'd', 'dd', 'dq', 'dqa', 'dqa32', 'dqa64', 'dqu', 'dqu8',
    'dqu16', 'dqu32', 'dqu64', 'f32x4', 'f32x8', 'f64x2', 'f64x4', 'f128', 'hw',
    'i', 'i32x4', 'i32x8', 'i64x2', 'i64x4', 'i128', 'ip', 'lw', 'p', 'pd', 'ph',
    'pi', 'pp', 'pq', 'ps', 'q', 'qd', 'qq', 'sd', 'si', 'ss', 'udq', 'uqq', 'w', 'x',
    //
    'bw', 'bd', 'bq', 'wd', 'wq', 'dq'
  );
  OPCODE_PREFIX: array [TOpCodePrefix] of RawByteString = (
    '', 'v'
  );

operator = (Left, Right: TFullOpcode): Boolean;
begin
  Result := (Left.Opcode = Right.Opcode) and (Left.Suffix = Right.Suffix);
end;


{ TX86Disassembler }

procedure TX86Disassembler.Check32;
begin
  // only valid in 32-bit ProcessMode
  if (ProcessMode = dm64) then
    Include(Instruction^.Flags, ifOnly32);
end;

procedure TX86Disassembler.Check64;
begin
  // only valid in 64-bit ProcessMode
  if (ProcessMode = dm32) then
    Include(Instruction^.Flags, ifOnly64);
end;

procedure TX86Disassembler.CheckVex;
begin
  if not (flagVex in Flags)
  then Include(Instruction^.Flags, ifOnlyVex);
end;

function TX86Disassembler.Ignore64Reg(s: ShortInt): ShortInt;
begin
  // ignored in 64-bit ProcessMode
  if (ProcessMode = dm64) then
    Result := -1
  else
    Result := s;
end;

procedure TX86Disassembler.Default64;
begin
  if ProcessMode = dm64 then
    Include(Flags, oprDefault64);
end;

procedure TX86Disassembler.Force64;
begin
  if ProcessMode = dm64 then
    Include(Flags, oprForce64);
end;


procedure TX86Disassembler.CheckLock;
  function CheckMem: boolean;
  var
    n: Byte;
  begin
    Result := True;
    for n := 1 to OperIdx do
      if ofMemory in Instruction^.Operand[n].Flags then Exit;
    Result := False;
  end;
begin
  if (preLock in Flags) and CheckMem
  then begin
    Exclude(Flags, preLock);
    Include(Instruction^.Flags, ifPrefixLock);
  end;
end;

procedure TX86Disassembler.CheckRepeat;
begin
  if preF3 in Flags
  then begin
    Exclude(Flags, preF3);
    Include(Instruction^.Flags, ifPrefixRep);
  end;
end;

procedure TX86Disassembler.CheckRepeatX;
begin
  if preF3 in Flags
  then begin
    Exclude(Flags, preF3);
    Include(Instruction^.Flags, ifPrefixRepE);
    Exit;
  end;
  if preF2 in Flags
  then begin
    Exclude(Flags, preF2);
    Include(Instruction^.Flags, ifPrefixRepNe);
    Exit;
  end;
end;

procedure TX86Disassembler.DecodeSIMD(AValidClear: TSimdOpcodes; ASizeOK: Boolean);
var
  check: TFlags;
begin
  if flagVex in Flags then Exit; // simd is part of the (e)vex prefix

  check := Flags * [pre66, preF3, preF2];
  if ASizeOK and (check <> [pre66])  // size prefix is allowed
  then check := check - [pre66];

  if check = []
  then SimdOpcode := soNone
  else if check - [preF3] = []
  then SimdOpcode := soF3
  else if check - [preF2] = []
  then SimdOpcode := soF2
  else if check - [pre66] = []
  then SimdOpcode := so66
  else SimdOpcode := soInvalid;

  if SimdOpcode in AValidClear
  then Flags := Flags - check;
end;

procedure TX86Disassembler.DecodeModRM;
begin
  Include(Flags, flagModRM);
  ModRM.Mode := (Code[ModRMIdx] shr 6) and 3;
  ModRM.Index:= (Code[ModRMIdx] shr 3) and 7;
  ModRM.RM   := (Code[ModRMIdx]      ) and 7;
end;

procedure TX86Disassembler.ClearSIMDPrefix;
begin
  Flags := Flags - [pre66, preF2, preF3];
end;

procedure TX86Disassembler.SetOpcode(AOpcode: TOpcode; ASuffix: TOpCodeSuffix; APrefixV: Boolean);
begin
  opcode:=Code[CodeIdx];

  if (flagVex in Flags) and APrefixV
  then Instruction^.OpCode.Prefix := OPPv
  else Instruction^.OpCode.Prefix := OPPnone;
  Instruction^.OpCode.Opcode := AOpcode;
  Instruction^.OpCode.Suffix := ASuffix;
end;

function TX86Disassembler.AddressSize: TAddressSize;
begin
  // effective address size for default 32 AnInstruction.operand size
  if (ProcessMode = dm64)
  then begin
    if preAdr in Flags
    then Result := as32
    else Result := as64;
  end
  else begin
    if preAdr in Flags
    then Result := as16
    else Result := as32;
  end;
end;

function TX86Disassembler.HasOpcode: Boolean;
begin
  Result := Instruction^.OpCode.Opcode <> OPX_InternalUnknown;
end;

function TX86Disassembler.OperandSize: TOperandSize;
begin
  // effective AnInstruction.operand size

  // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 1:
  // 3.6 OPERAND-SIZE AND ADDRESS-SIZE ATTRIBUTES
  //
  // Table 3-3 D-flag = 1 for 32 bit processes ->
  // default 32, prefix 16
  //
  // Table 3-4
  // REX.W 64, default 32, prefix 16 (REX.W overrules prefix)
  //
  // So for both dm32 and dm64 the default size is 32 unless overridden by flags

  // A.3 ONE, TWO, AND THREE-BYTE Instruction^.Opcode MAPS
  // Some instructions default or force to 64bit in dm64

  if [oprForce64, rexW] * Flags <> []
  then begin
    Result := os64;
  end
  else begin
    if pre66 in Flags
    then Result := os16
    else if oprDefault64 in Flags
    then Result := os64
    else Result := os32;
  end;
end;

function TX86Disassembler.VectorSize: TOperandSize;
begin
  if flagVex in Flags
  then Result := Vex.VectorLength
  else Result := os128;
end;

procedure TX86Disassembler.AddOperand(RegValue:TRegValues; ASize: TOperandSize; AByteCount: Word = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []; AByteCount2: Byte = 0);
var
  idx: Byte;
begin
  Inc(OperIdx);
  if OperIdx > High(Instruction^.Operand)
  then begin
    Writeln(Format('AddOperand: Only %d operands supported, got %d', [High(Instruction^.Operand), OperIdx]));
    Exit;
  end;

  idx := CodeIdx;
  if flagModRM in Flags then Inc(idx);
  if flagSib in Flags then Inc(idx);

  Instruction^.Operand[OperIdx].CodeIndex := idx;
  Instruction^.Operand[OperIdx].Size := ASize;
  Instruction^.Operand[OperIdx].ByteCount := AByteCount;
  Instruction^.Operand[OperIdx].ByteCount2 := AByteCount2;
  Instruction^.Operand[OperIdx].FormatFlags := AFormatFlags;
  Instruction^.Operand[OperIdx].RegValue := RegValue;

  Instruction^.Operand[OperIdx].Flags := AFlags;
end;

procedure TX86Disassembler.AddOperand(RegValue:TRegValues; AByteCount: Word = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []);
begin
  AddOperand(RegValue, OperandSize, AByteCount, AFormatFlags, AFlags);
end;

function TX86Disassembler.StdCond(AIndex: Byte): TOpCodeSuffix;
const
  COND: array[0..$F] of TOpCodeSuffix = (
    OPSc_o, OPSc_no, OPSc_b, OPSc_nb, OPSc_z, OPSc_nz, OPSc_be, OPSc_nbe, OPSc_s, OPSc_ns, OPSc_p, OPSc_np, OPSc_l, OPSc_nl, OPSc_le, OPSc_nle
  );
begin
  Result := COND[AIndex and $F];
end;

function RegName(AType: TRegisterType; ASize: TOperandSize; AIndex: Byte): RawByteString;
const
  REGS: array[0..7] of RawByteString = ('ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di');
  REG8: array[0..7] of RawByteString = ('a', 'c', 'd', 'b', 'sp', 'bp', 'si', 'di');
  SREG: array[0..5] of RawByteString = ('es', 'cs', 'ss', 'ds', 'fs', 'gs');
begin
  case AType of
    regNone: begin
      Result := '';
    end;
    regRip: begin
      Result := 'rip';
    end;
    regOne: begin
      Result := '1';
    end;
    regGeneral: begin
      case ASize of
        os8: begin
          if AIndex <= High(REG8)
          then Result := REG8[AIndex] + 'l'
          else Result := Format('r%ub', [AIndex]);
        end;
        os16: begin
          if AIndex <= High(REG8)
          then Result := REGS[AIndex]
          else Result := Format('r%uw', [AIndex]);
        end;
        os32: begin
          if AIndex <= High(REG8)
          then Result := 'e' + REGS[AIndex]
          else Result := Format('r%ud', [AIndex]);
        end;
        os64: begin
          if AIndex <= High(REG8)
          then Result := 'r' + REGS[AIndex]
          else Result := Format('r%u', [AIndex]);
        end;
      else
        Result := Format('*r%u.%u', [AIndex, OPERAND_BITS[ASize]]);
      end;
    end;
    regGeneralH: begin
      case ASize of
        os8: begin
          if AIndex <= 3
          then Result := REG8[AIndex] + 'h'
          else Result := Format('*r%uh', [AIndex]);
        end;
      else
        Result := Format('*r%uh.%u', [AIndex, OPERAND_BITS[ASize]]);
      end;
    end;
    regMm: begin
      Result := Format('mm%u', [AIndex]);
    end;
    regXmm: begin
      case ASize of
        os32,
        os64,
        os128: begin
          Result := Format('xmm%u', [AIndex]);
        end;
        os256: begin
          Result := Format('ymm%u', [AIndex]);
        end;
        os512: begin
          Result := Format('zmm%u', [AIndex]);
        end;
      else
        Result := Format('*mm%u.%u', [AIndex, OPERAND_BITS[ASize]]);
      end;
    end;
    regSegment: begin
      if AIndex <= High(SREG)
      then Result := SREG[AIndex]
      else Result := Format('*s%u', [AIndex]);
    end;
    regControl: begin
      Result := Format('cr%u', [AIndex]);
    end;
    regDebug: begin
      Result := Format('dr%u', [AIndex]);
    end;
    regX87: begin
      Result := Format('st(%u)', [AIndex]);
    end;
    regFlags: begin
      case ASize of
        os16: Result := 'flags';
        os32: Result := 'eflags';
        os64: Result := 'rflags';
      else
        Result := Format('*flags.%u', [OPERAND_BITS[ASize]]);
      end;
    end;
    regBounds: begin
      Result := Format('bnd%u', [AIndex]);
    end;
  else
    Result := Format('**%u', [AIndex]);
  end;
end;

function TX86Disassembler.OPS_Wxx(AWIG, AW0, AW1: TOpCodeSuffix): TOpCodeSuffix;
begin
  if flagEvex in Flags
  then begin
    if rexW in Flags
    then Result := AW1
    else Result := AW0;
  end
  else Result := AWIG;
end;

function TX86Disassembler.OPS_d_q: TOpCodeSuffix;
const
  MAP: array[Boolean] of TOpCodeSuffix = (OPSx_d, OPSx_q);
begin
  Result := MAP[rexW in Flags];
end;

function TX86Disassembler.OPS_ps_d: TOpCodeSuffix;
const
  MAP: array[Boolean] of TOpCodeSuffix = (OPSx_ps, OPSx_d);
begin
  Result := MAP[rexW in Flags];
end;

function TX86Disassembler.OPS_ss_d: TOpCodeSuffix;
const
  MAP: array[Boolean] of TOpCodeSuffix = (OPSx_ss, OPSx_d);
begin
  Result := MAP[rexW in Flags];
end;

Function RegValue(AType: TRegisterType; ASize : TOperandSize=os0; AIndex: Byte=0):TRegValues; Inline;
begin
 Result:=Default(TRegValues);
 Result[0].AType :=AType;
 Result[0].ASize :=ASize;
 Result[0].AIndex:=AIndex;
 Result[0].AScale:=1;
end;

procedure TX86Disassembler.AddReg(AType: TRegisterType; ASize: TOperandSize; AIndex: Byte);
begin
  if not (flagRex in Flags) and (AType = regGeneral) and (ASize = os8) and (AIndex >= 4)
  then begin
    // in 'legacy' mode the 8 bit registers are encoded as
    //  AL, CL, DL, BL, AH, CH, DH, BH
    // in 'extended' mode the 8 bit registers are encoded as
    //  AL, CL, DL, BL, SP, BP, SI, DI
    //
    // The 8 bit general purpose 8 bit default to the 'extended' registers
    // So for 'legacy' mode we change the type and adjust the offset
    AddOperand(RegValue(regGeneralH, ASize, AIndex - 4), ASize)
  end
  else AddOperand(RegValue(AType, ASize,  AIndex), ASize);
end;

procedure TX86Disassembler.AddOpcReg(AType: TRegisterType; ASize: TOperandSize; AIndex: Byte);
begin
  AddReg(AType, ASize, AIndex + REXOFFSET[rexB in Flags] + VEXOFFSET[evexX in Flags]);
end;

procedure TX86Disassembler.AddModReg(AType: TRegisterType; ASize: TOperandSize);
begin
  DecodeModRM;
  AddReg(AType, ASize, ModRM.Index + REXOFFSET[rexR in Flags] + VEXOFFSET[evexR in Flags]);
end;

procedure AddRegValue(var RegValue:TRegValues; AType: TRegisterType; ASize : TOperandSize; AIndex: Byte=0; AScale: Byte=1);
begin
 if (RegValue[0].AType=regNone) then
 begin
  RegValue[0].AType :=AType;
  RegValue[0].ASize :=ASize;
  RegValue[0].AIndex:=AIndex;
  RegValue[0].AScale:=AScale;
 end else
 if (RegValue[1].AType=regNone) then
 begin
  //shift
  RegValue[1]:=RegValue[0];
  //
  RegValue[0].AType :=AType;
  RegValue[0].ASize :=ASize;
  RegValue[0].AIndex:=AIndex;
  RegValue[0].AScale:=AScale;
 end else
 begin
  Assert(false,'AddRegValue overflow');
 end;
end;

procedure TX86Disassembler.AddModRM(AReqTypes: TModRMTypes; ASize: TOperandSize; AType: TRegisterType);

  procedure Mem16;
  const
    REGS16_REG: array[0..7] of TRegValues = (
     (//0
      (AType: regGeneral; ASize:os16; AIndex: REG_B ; AScale:1),
      (AType: regGeneral; ASize:os16; AIndex: REG_SI; AScale:1)
     ),
     (//1
      (AType: regGeneral; ASize:os16; AIndex: REG_B ; AScale:1),
      (AType: regGeneral; ASize:os16; AIndex: REG_DI; AScale:1)
     ),
     (//2
      (AType: regGeneral; ASize:os16; AIndex: REG_BP; AScale:1),
      (AType: regGeneral; ASize:os16; AIndex: REG_SI; AScale:1)
     ),
     (//3
      (AType: regGeneral; ASize:os16; AIndex: REG_BP; AScale:1),
      (AType: regGeneral; ASize:os16; AIndex: REG_DI; AScale:1)
     ),
     (//4
      (AType: regGeneral; ASize:os16; AIndex: REG_SI; AScale:1),
      (AType: regNone   )
     ),
     (//5
      (AType: regGeneral; ASize:os16; AIndex: REG_DI; AScale:1),
      (AType: regNone   )
     ),
     (//6
      (AType: regGeneral; ASize:os16; AIndex: REG_BP; AScale:1),
      (AType: regNone   )
     ),
     (//7
      (AType: regGeneral; ASize:os16; AIndex: REG_B ; AScale:1),
      (AType: regNone   )
     )
    );
  begin
    case ModRM.Mode of
      0:
      begin
        if (ModRM.RM = 6) then // disp16 -> exception to the regs
         AddOperand(RegValue(regNone),  ASize, 2, [hvfSigned, hvfIncludeHexchar], [ofMemory])
        else
         AddOperand(REGS16_REG[ModRM.RM], ASize, 0, [], [ofMemory]);
      end;
      1: AddOperand(REGS16_REG[ModRM.RM], ASize, 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar], [ofMemory]);
      2: AddOperand(REGS16_REG[ModRM.RM], ASize, 2, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar], [ofMemory]);
    end;
  end;

type
 TOper = record
   Size: Byte;
   Flags: THexValueFormatFlags;
   RegValue: TRegValues;
 end;

var
  Oper: TOper;
begin
  DecodeModRM;

  // Check for reg (ProcessMode = 3) first;
  if (ModRM.Mode = 3) then
  begin
    if modReg in AReqTypes then
     AddReg(AType, ASize, ModRM.RM + REXOFFSET[rexB in Flags] + VEXOFFSET[evexX in Flags])
    else
     AddReg(regInvalid, ASize, ModRM.RM + REXOFFSET[rexB in Flags] + VEXOFFSET[evexX in Flags]);
    Exit;
  end;

  // Check if mem is allowed
  if not (modMem in AReqTypes) then
  begin
    AddOperand(RegValue(regInvalid), OPERAND_BYTES[ASize], [], [ofMemory]);
    Exit;
  end;

  Oper:=Default(TOper);

  // Here only mem access
  AddrSize := AddressSize;
  if AddrSize = as16 then
  begin
    Mem16;
    Exit;
  end;

  if ModRM.RM = 4 then
  begin
    // sib folows
    Include(Flags, flagSib);
    sib.Scale := Code[ModRMIdx+1] shr 6;
    sib.Index := (Code[ModRMIdx+1] shr 3) and $7;
    sib.Base  := Code[ModRMIdx+1] and $7;

    // base
    if (ModRM.Mode = 0) and (sib.Base = 5) then
    begin
      // disp32
      Oper.Size := 4;
      if (sib.Index <> 4) or (rexX in Flags) then
       Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar] // [reg + base]
      else
       Oper.Flags := [hvfSigned, hvfIncludeHexchar];                   // [base]
    end else
    begin
      Oper.RegValue := RegValue(regGeneral, ADDRESS_SIZE[AddrSize], sib.Base + REXOFFSET[rexB in Flags]);
    end;

    // reg
    if (rexX in Flags) or (sib.Index <> 4) then
    begin
      // get index
      AddRegValue(Oper.RegValue, regGeneral, ADDRESS_SIZE[AddrSize], sib.Index + REXOFFSET[rexX in Flags], 1 shl sib.Scale);
    end;
  end else
  begin
    // no sib
    Oper.RegValue := RegValue(regGeneral, ADDRESS_SIZE[AddrSize], ModRM.RM + REXOFFSET[rexB in Flags]);
  end;

  case ModRM.Mode of
    0:
    begin
      // exceptions to std encoding
      if (ModRM.RM = 5) then
      begin
        // disp32
        if AddrSize = as64 then
        begin
          Oper.RegValue := RegValue(regRip, os64);
          Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar];
        end else
        begin
          Oper.RegValue := RegValue(regNone);
          Oper.Flags := [hvfSigned, hvfIncludeHexchar];
        end;
        Oper.Size := 4;
      end;
    end;
    1:
    begin
      Oper.Size := 1;
      Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar];
    end;
    2:
    begin
      Oper.Size := 4;
      Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar];
    end;
  end;

  AddOperand(Oper.RegValue, ASize, Oper.Size, Oper.Flags, [ofMemory]);
end;

procedure TX86Disassembler.AddVexReg(AType: TRegisterType; ASize: TOperandSize);
begin
  if flagVex in Flags
  then AddOperand(RegValue(AType, ASize, Vex.Index + VEXOFFSET[evexV in Flags]), ASize)
  else AddOperand(RegValue(regInvalid), ASize);
end;

procedure TX86Disassembler.AddAp;
begin
  if OperandSize = os16 //XXXX:XXXX
  then AddOperand(RegValue(regNone), os32, 2, [], [], 2)
  else AddOperand(RegValue(regNone), os48, 4, [], [], 2)
end;

procedure TX86Disassembler.AddBy;
begin
  if OperandSize = os64
  then AddVexReg(regGeneral, os64)
  else AddVexReg(regGeneral, os32);
end;

procedure TX86Disassembler.AddCd;
begin
  AddModReg(regControl, MODE_SIZE[ProcessMode]);
end;

procedure TX86Disassembler.AddDd;
begin
  AddModReg(regDebug, MODE_SIZE[ProcessMode]);
end;

procedure TX86Disassembler.AddEb;
begin
  AddModRM([modReg, modMem], os8, regGeneral);
end;

procedure TX86Disassembler.AddEd;
begin
  AddModRM([modReg, modMem], os32, regGeneral);
end;

procedure TX86Disassembler.AddEp;
begin
  case OperandSize of
    os16: AddModRM([modReg, modMem], os32, regGeneral);
    os32: AddModRM([modReg, modMem], os48, regGeneral);
    os64: AddModRM([modReg, modMem], os80, regGeneral);
  end;
end;

procedure TX86Disassembler.AddEv;
begin
  AddModRM([modReg, modMem], OperandSize, regGeneral);
end;

procedure TX86Disassembler.AddEw;
begin
  AddModRM([modReg, modMem], os16, regGeneral);
end;

procedure TX86Disassembler.AddEy;
begin
  if OperandSize = os64
  then AddModRM([modReg, modMem], os64, regGeneral)
  else AddModRM([modReg, modMem], os32, regGeneral);
end;

procedure TX86Disassembler.AddFv;
begin
  AddReg(regFlags, OperandSize, 0);
end;

procedure TX86Disassembler.AddGb;
begin
  AddModReg(regGeneral, os8);
end;

procedure TX86Disassembler.AddGd;
begin
  AddModReg(regGeneral, os32);
end;

procedure TX86Disassembler.AddGv;
begin
  AddModReg(regGeneral, OperandSize);
end;

procedure TX86Disassembler.AddGw;
begin
  AddModReg(regGeneral, os16);
end;

procedure TX86Disassembler.AddGy;
begin
  if OperandSize = os64
  then AddModReg(regGeneral, os64)
  else AddModReg(regGeneral, os32);
end;

procedure TX86Disassembler.AddGz;
begin
  if OperandSize = os16
  then AddModReg(regGeneral, os16)
  else AddModReg(regGeneral, os32);
end;

procedure TX86Disassembler.AddHdq;
begin
  AddVexReg(regXmm, Vex.VectorLength);
end;

procedure TX86Disassembler.AddHpd;
begin
  if flagVex in Flags
  then begin
    if Vex.VectorLength = os128
    then AddVexReg(regXmm, os128)
    else AddVexReg(regXmm, os256);
  end;
end;

procedure TX86Disassembler.AddHps;
begin
  if flagVex in Flags
  then begin
    if Vex.VectorLength = os128
    then AddVexReg(regXmm, os128)
    else AddVexReg(regXmm, os256);
  end;
end;

procedure TX86Disassembler.AddHsd;
begin
  if flagVex in Flags
  then AddVexReg(regXmm, os64);
end;

procedure TX86Disassembler.AddHss;
begin
  if flagVex in Flags
  then AddVexReg(regXmm, os32);
end;

procedure TX86Disassembler.AddHx;
begin
  if flagVex in Flags
  then AddVexReg(regXmm, Vex.VectorLength);
end;

procedure TX86Disassembler.AddHq;
begin
  if flagVex in Flags
  then AddVexReg(regXmm, os64);
end;

procedure TX86Disassembler.AddHqq;
begin
  if flagVex in Flags
  then begin
    if Vex.VectorLength = os512
    then AddVexReg(regXmm, os512)
    else AddVexReg(regXmm, os256);
  end;
end;

procedure TX86Disassembler.AddIb;
begin
  AddOperand(RegValue(regNone), os8, 1, [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddIv;
begin
  AddOperand(RegValue(regNone), OPERAND_BYTES[OperandSize], [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddIw;
begin
  AddOperand(RegValue(regNone), os16, 2, [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddIz;
begin
 case OperandSize of
   os16:AddOperand(RegValue(regNone), os16, 2, [hvfIncludeHexchar]);
   os64:AddOperand(RegValue(regNone), os32, 4, [hvfSigned, hvfIncludeHexchar]);
   else
        AddOperand(RegValue(regNone), os32, 4, [hvfIncludeHexchar]);
 end;
end;

procedure TX86Disassembler.AddJb;
begin
  AddOperand(RegValue(regNone), os8, 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddJz;
begin
  if OperandSize = os16
  then AddOperand(RegValue(regNone), os16, 2, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar])
  else AddOperand(RegValue(regNone), os32, 4, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddLx;
var
  idx: Byte;
  n: Integer;
begin
  // calc index of imm_opcode
  idx := 0;
  if flagModRM in Flags then Inc(idx);
  if flagSib in Flags then Inc(idx);
  for n := 1 to OperIdx do
  begin
    Inc(idx, Instruction^.Operand[n].ByteCount);
    Inc(idx, Instruction^.Operand[n].ByteCount2);
  end;

  //@@@ todo: check, Instruction^.Operand[OperIdx].CodeIndex + ByteCount(2) should be the same as CodeIdx + idx;
  idx := Code[CodeIdx + idx + 1] shr 4; // bit 7..4 -> reg
  if ProcessMode = dm32
  then idx := idx and $07;

  AddOperand(RegValue(regXmm, Vex.VectorLength, idx), Vex.VectorLength, 1);
end;

procedure TX86Disassembler.AddM;
begin
  AddModRM([modMem], OperandSize, regNone);
end;

procedure TX86Disassembler.AddMa;
begin
  AddModRM([modMem], OperandSize, regNone);
end;

procedure TX86Disassembler.AddMb;
begin
  AddModRM([modMem], os8, regNone);
end;

procedure TX86Disassembler.AddMd;
begin
  AddModRM([modMem], os32, regNone);
end;

procedure TX86Disassembler.AddMdq;
begin
  AddModRM([modMem], VectorSize, regNone)
end;

procedure TX86Disassembler.AddMp;
begin
  if OperandSize = os16 //XXXX:XXXX
  then AddModRM([modMem], os32, regNone)
  else AddModRM([modMem], os48, regNone);
end;

procedure TX86Disassembler.AddMps;
begin
  AddModRM([modMem], VectorSize, regNone)
end;

procedure TX86Disassembler.AddMpd;
begin
  AddModRM([modMem], VectorSize, regNone)
end;

procedure TX86Disassembler.AddMq;
begin
  AddModRM([modMem], os64, regNone);
end;

procedure TX86Disassembler.AddMs;
begin
  if (ProcessMode = dm64)
  then AddModRM([modMem], os80, regNone)
  else AddModRM([modMem], os48, regNone);
end;

procedure TX86Disassembler.AddMw;
begin
  AddModRM([modMem], os16, regNone);
end;

procedure TX86Disassembler.AddMw_Rv;
begin
  DecodeModRM;

  if ModRM.Mode = 3 // reg
  then AddModRM([modReg], OperandSize, regGeneral)
  else AddModRM([modMem], os16, regNone);
end;

procedure TX86Disassembler.AddMx;
begin
  if flagVex in Flags
  then AddModRM([modMem], Vex.VectorLength, regXmm)
  else AddModRM([modMem], os128, regXmm)
end;

procedure TX86Disassembler.AddMy;
begin
  if OperandSize = os64
  then AddModRM([modMem], os64, regNone)
  else AddModRM([modMem], os32, regNone);
end;

procedure TX86Disassembler.AddNq;
begin
  AddModRM([modReg], os64, regMm);
end;

procedure TX86Disassembler.AddOb;
begin
  AddOperand(RegValue(regNone), os8, ADDRESS_BYTES[AddressSize], [hvfIncludeHexchar], [ofMemory])
end;

procedure TX86Disassembler.AddOv;
begin
  AddOperand(RegValue(regNone), ADDRESS_BYTES[AddressSize], [hvfIncludeHexchar], [ofMemory])
end;

procedure TX86Disassembler.AddPd;
begin
  AddModReg(regMm, os32);
end;

procedure TX86Disassembler.AddPpi;
begin
  AddModReg(regMm, os64);
end;

procedure TX86Disassembler.AddPy;
begin
  if OperandSize = os64
  then AddModReg(regMm, os64)
  else AddModReg(regMm, os32);
end;

procedure TX86Disassembler.AddPq;
begin
  AddModReg(regMm, os64);
end;

procedure TX86Disassembler.AddQd;
begin
  AddModRM([modReg, modMem], os32, regMm);
end;

procedure TX86Disassembler.AddQpi;
begin
  AddModRM([modReg, modMem], os64, regMm);
end;

procedure TX86Disassembler.AddQq;
begin
  AddModRM([modReg, modMem], os64, regMm);
end;

procedure TX86Disassembler.AddRd;
begin
  AddModRM([modReg], MODE_SIZE[ProcessMode], regGeneral);
end;

procedure TX86Disassembler.AddRd_Mb;
begin
  if ModRM.Mode = 3 // reg
  then AddModRM([modReg], os32, regGeneral)
  else AddModRM([modMem], os8, regNone);
end;

procedure TX86Disassembler.AddRd_q;
begin
  AddModRM([modReg], MODE_SIZE[ProcessMode], regGeneral);
end;

procedure TX86Disassembler.AddRy;
begin
  if OperandSize = os64
  then AddModRM([modReg], os64, regGeneral)
  else AddModRM([modReg], os32, regGeneral);
end;

procedure TX86Disassembler.AddRy_Mb;
begin
  if ModRM.Mode = 3 // reg
  then AddModRM([modReg], OperandSize, regGeneral)
  else AddModRM([modMem], os8, regNone);
end;

procedure TX86Disassembler.AddRy_Mw;
begin
  if ModRM.Mode = 3 // reg
  then AddModRM([modReg], OperandSize, regGeneral)
  else AddModRM([modMem], os16, regNone);
end;

procedure TX86Disassembler.AddRv;
begin
  AddModRM([modReg], OperandSize, regGeneral);
end;

procedure TX86Disassembler.AddSw;
begin
  AddModReg(regSegment, os16);
end;

procedure TX86Disassembler.AddUdq;
begin
  AddModRM([modReg], os128, regXmm);
end;

procedure TX86Disassembler.AddUdq_Md;
begin
  if ModRM.Mode = 3 // reg
  then AddModRM([modReg], os128, regXmm)
  else AddModRM([modMem], os32, regNone);
end;

procedure TX86Disassembler.AddUpd;
begin
  AddModRM([modReg], VectorSize, regXmm);
end;

procedure TX86Disassembler.AddUps;
begin
  AddModRM([modReg], VectorSize, regXmm);
end;

procedure TX86Disassembler.AddUq;
begin
  AddModRM([modReg], os64, regXmm);
end;

procedure TX86Disassembler.AddUx;
begin
  if flagVex in Flags
  then AddModRM([modReg], Vex.VectorLength, regXmm)
  else AddModRM([modReg], os128, regXmm);
end;

procedure TX86Disassembler.AddUx_Md;
var
  Size: TOperandSize;
begin
  if ModRM.Mode = 3 // reg
  then begin
    case Vex.VectorLength of
      os512: Size := os128;
      os256: Size := os64;
    else
      Size := os32;
    end;
    AddModRM([modReg], Size, regXmm);
  end
  else AddModRM([modMem], os32, regNone);
end;

procedure TX86Disassembler.AddUx_Mq;
var
  Size: TOperandSize;
begin
  if ModRM.Mode = 3 // reg
  then begin
    case Vex.VectorLength of
      os512: Size := os256;
      os256: Size := os128;
    else
      Size := os64;
    end;
    AddModRM([modReg], Size, regXmm);
  end
  else AddModRM([modMem], os64, regNone);
end;

procedure TX86Disassembler.AddUx_Mw;
var
  Size: TOperandSize;
begin
  if ModRM.Mode = 3 // reg
  then begin
    case Vex.VectorLength of
      os512: Size := os64;
      os256: Size := os32;
    else
      Size := os16;
    end;
    AddModRM([modReg], Size, regXmm);
  end
  else AddModRM([modMem], os16, regNone);
end;

procedure TX86Disassembler.AddVdq;
begin
  AddModReg(regXmm, VectorSize);
end;

procedure TX86Disassembler.AddVpd;
begin
  AddModReg(regXmm, VectorSize);
end;

procedure TX86Disassembler.AddVps;
begin
  AddModReg(regXmm, VectorSize);
end;

procedure TX86Disassembler.AddVq;
begin
  AddModReg(regXmm, os128);
end;

procedure TX86Disassembler.AddVqq;
begin
  if Vex.VectorLength = os512
  then AddModReg(regXmm, os512)
  else AddModReg(regXmm, os256);
end;

procedure TX86Disassembler.AddVsd;
begin
  AddModReg(regXmm, os64);
end;

procedure TX86Disassembler.AddVss;
begin
  AddModReg(regXmm, os32);
end;

procedure TX86Disassembler.AddVx;
begin
  if flagVex in Flags
  then AddModReg(regXmm, Vex.VectorLength)
  else AddModReg(regXmm, os128)
end;

procedure TX86Disassembler.AddVy;
begin
  if OperandSize = os64
  then AddModReg(regXmm, os64)
  else AddModReg(regXmm, os32);
end;

procedure TX86Disassembler.AddWd;
begin
  AddModRM([modReg, modMem], os32, regXmm);
end;

procedure TX86Disassembler.AddWdq;
begin
  AddModRM([modReg, modMem], VectorSize, regXmm);
end;

procedure TX86Disassembler.AddWpd;
begin
  AddModRM([modReg, modMem], VectorSize, regXmm);
end;

procedure TX86Disassembler.AddWps;
begin
  AddModRM([modReg, modMem], VectorSize, regXmm);
end;

procedure TX86Disassembler.AddWq;
begin
  AddModRM([modReg, modMem], os64, regXmm);
end;

procedure TX86Disassembler.AddWqq;
begin
  if Vex.VectorLength = os512
  then AddModRM([modReg, modMem], os512, regXmm)
  else AddModRM([modReg, modMem], os256, regXmm);
end;

procedure TX86Disassembler.AddWsd;
begin
  AddModRM([modReg, modMem], os64, regXmm);
end;

procedure TX86Disassembler.AddWss;
begin
  AddModRM([modReg, modMem], os32, regXmm);
end;

procedure TX86Disassembler.AddWx;
begin
  AddModRM([modReg, modMem], VectorSize, regXmm);
end;

procedure TX86Disassembler.AddStdOperands(AIndex: Byte);
begin
  case AIndex and $7 of
    0: begin AddEb; AddGb; end;
    1: begin AddEv; AddGv; end;
    2: begin AddGb; AddEb; end;
    3: begin AddGv; AddEv; end;
    4: begin AddReg(regGeneral, os8, REG_A); AddIb; end;
    5: begin AddReg(regGeneral, OperandSize, REG_A); AddIz; end;
  else
    AddOperand({'!!',} RegValue(regInvalid));
  end;
end;

procedure TX86Disassembler.DoX87;

  procedure AddMem14_28Env;
  begin
    AddModRM([modMem], OperandSize, regNone);
  end;

  procedure AddMem98_108Env;
  begin
    AddModRM([modMem], OperandSize, regNone);
  end;

  procedure AddMem16;
  begin
    AddModRM([modMem], os16, regNone);
  end;

  procedure AddMem32;
  begin
    AddModRM([modMem], os32, regNone);
  end;

  procedure AddMem64;
  begin
    AddModRM([modMem], os64, regNone);
  end;

  procedure AddMem80;
  begin
    AddModRM([modMem], os80, regNone);
  end;

  procedure AddReg0;
  begin
    AddReg(regX87, os80, 0);
  end;

  procedure AddRegN;
  begin
    AddReg(regX87, os80, ModRM.RM);
  end;

  procedure DoD8;
  const
    OPC: array[0..7] of TOpCode =       (OPfadd,  OPfmul,  OPfcom,  OPfcom, OPfsub,  OPfsubr, OPfdiv,  OPfdivr);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone, OPSnone, OPSx_p, OPSnone, OPSnone, OPSnone, OPSnone);
  begin
    SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
    case Code[ModRMIdx] of
      $00..$BF: AddMem32
    else
      AddReg0; AddRegN;
    end;
  end;

  procedure DoD9;
  const
    OPC: array[0..7] of TOpCode =       (OPfld,   OPfxch,  OPfst,   OPfst,  OPfldenv, OPfldcw, OPfnstenv, OPfnstcw);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone, OPSnone, OPSx_p, OPSnone,  OPSnone, OPSnone,   OPSnone);
    OPCx: array[0..$1F] of TOpCode = (
      OPfchs, OPfabs, OPX_InvalidX87, OPX_InvalidX87, OPftst, OPfxam, OPX_InvalidX87, OPX_InvalidX87,
      OPfld1, OPfldl2t, OPfldl2e, OPfldpi, OPfldlg2, OPfldln2, OPfldz, OPX_InvalidX87,
      OPf2xm1, OPfyl2x, OPfptan, OPfpatan, OPfxtract, OPfprem1, OPfdecstp, OPfincstp,
      OPfprem, OPfyl2xp1, OPfsqrt, OPfsincos, OPfrndint, OPfscale, OPfsin, OPfcos
    );
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        case ModRM.Index of
          0, 2, 3: AddMem32;
          1: SetOpcode(OPX_InvalidX87);
          4, 6: AddMem14_28Env;
          5, 7: AddMem16;
        end;
      end;
      $C0..$CF: begin SetOpcode(OPC[ModRM.Index]); AddReg0; AddRegN; end;
      $D0:      begin SetOpcode(OPnop); end;
      $D8..$DF: begin SetOpcode(OPX_ReservedX87); end;
      $E0..$FF: begin SetOpcode(OPCx[Code[ModRMIdx] and $1F]); end;
    else
      SetOpcode(OPX_InvalidX87);
    end;
  end;

  procedure DoDA;
  const
    OPC: array[0..7] of TOpCode =       (OPfiadd, OPfimul, OPficom, OPficom, OPfisub, OPfisubr, OPfidiv, OPfidivr);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone, OPSnone, OPSx_p,  OPSnone, OPSnone,  OPSnone, OPSnone);
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        AddMem32;
      end;
      $C0..$C7: begin
        SetOpcode(OPfcmov__, OPSc_b);
        AddReg0; AddRegN;
      end;
      $C8..$CF: begin
        SetOpcode(OPfcmov__, OPSc_e);
        AddReg0; AddRegN;
      end;
      $D0..$D7: begin
        SetOpcode(OPfcmov__, OPSc_be);
        AddReg0; AddRegN;
      end;
      $D8..$DF: begin
        SetOpcode(OPfcmov__, OPSc_u);
        AddReg0; AddRegN;
      end;
      $E9: begin
        SetOpcode(OPfucom, OPSx_p);
      end;
    else
      SetOpcode(OPX_InvalidX87);
    end;
  end;

  procedure DoDB;
  const
    OPC: array[0..7] of TOpCode =       (OPfild,  OPfisttp, OPfist,  OPfist, OPX_InvalidX87, OPfld,    OPX_InvalidX87, OPfst);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone,  OPSnone, OPSx_p, OPSnone,        OPSnone,  OPSnone,        OPSx_p);
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        case ModRM.Index of
          0..3: AddMem32;
          5, 7: AddMem80;
        end;
      end;
      $C0..$DF,
      $E8..$F7: begin
        case ModRM.Index of
          0: SetOpcode(OPfcmov__, OPSc_nb );
          1: SetOpcode(OPfcmov__, OPSc_ne );
          2: SetOpcode(OPfcmov__, OPSc_nbe);
          3: SetOpcode(OPfcmov__, OPSc_nu );
          5: SetOpcode(OPfucom,   OPSx_i  );
          6: SetOpcode(OPfcom,    OPSx_i  );
        else
          SetOpcode(OPX_InvalidX87);
        end;
        AddReg0; AddRegN;
      end;
      $E0..$E1: begin SetOpcode(OPX_ReservedX87); end;
      $E2:      begin SetOpcode(OPfnclex);        end;
      $E3:      begin SetOpcode(OPfninit);        end;
      $E4:      begin SetOpcode(OPX_ReservedX87); end;
    else
      SetOpcode(OPX_InvalidX87);
    end;
  end;

  procedure DoDC;
  const
    OPC: array[0..7] of TOpCode =       (OPfadd,  OPfmul,   OPfcom,  OPfcom, OPfsub,  OPfsubr, OPfdiv,  OPfdivr);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone,  OPSnone, OPSx_p, OPSnone, OPSnone, OPSnone, OPSnone);
    OPCx: array[0..7] of TOpCode = (OPfadd, OPfmul, OPX_InvalidX87, OPX_InvalidX87, OPfsubr, OPfsub, OPfdivr, OPfdiv);
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        AddMem64;
      end;
      $C0..$CF,
      $E0..$FF: begin
        SetOpcode(OPCx[ModRM.Index]);
        AddRegN; AddReg0;
      end;
    else
      SetOpcode(OPX_ReservedX87);
    end;
  end;

  procedure DoDD;
  const
    OPC: array[0..7] of TOpCode =       (OPfld,   OPfisttp, OPfst,   OPfst,  OPfrstor, OPX_InvalidX87, OPfnsave, OPfnstsw);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone,  OPSnone, OPSx_p, OPSnone,  OPSnone,        OPSnone,  OPSnone);
    OPCx: array[0..7] of TOpCode =       (OPffree, OPX_InvalidX87, OPfst,   OPfst,  OPX_InvalidX87, OPfucom, OPX_InvalidX87, OPX_InvalidX87);
    OPSx: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone,        OPSnone, OPSx_p, OPSnone,        OPSx_p,  OPSnone,        OPSnone);
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        case ModRM.Index of
          0..3: begin AddMem64; end;
          4, 6: begin AddMem98_108Env; end;
          7:    begin AddMem16; end;
        end;
      end;
      $C0..$C7,
      $D0..$DF,
      $E8..$EF: begin
        SetOpcode(OPCx[ModRM.Index], OPSx[ModRM.Index]);
        AddRegN;
      end;
      $E0..$E7: begin
        SetOpcode(OPCx[ModRM.Index], OPSx[ModRM.Index]);
        AddRegN; AddReg0;
      end;
      $C8..$CF: SetOpcode(OPX_ReservedX87);
    else
      SetOpcode(OPX_InvalidX87);
    end;
  end;

  procedure DoDE;
  const
    OPC: array[0..7] of TOpCode =       (OPfiadd, OPfimul, OPficom, OPficom, OPfisub, OPfisubr, OPfidiv, OPfidivr);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone, OPSnone, OPSx_p,  OPSnone, OPSnone,  OPSnone, OPSnone);
    OPCx: array[0..7] of TOpCode =       (OPfadd, OPfmul, OPX_InvalidX87, OPX_InvalidX87, OPfsubr, OPfsub, OPfdivr, OPfdiv);
    OPSx: array[0..7] of TOpCodeSuffix = (OPSx_p, OPSx_p, OPSnone,        OPSnone,        OPSx_p,  OPSx_p, OPSx_p,  OPSx_p);
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        AddMem16;
      end;
      $C0..$CF,
      $E0..$FF: begin
        SetOpcode(OPCx[ModRM.Index],OPSx[ModRM.Index]);
        AddRegN; AddReg0;
      end;
      $D9:      begin
        SetOpcode(OPfcom, OPSx_pp);
      end;
      $D0..$D7: begin
        SetOpcode(OPX_ReservedX87);
      end;
    else
      SetOpcode(OPX_InvalidX87);
    end;
  end;

  procedure DoDF;
  const
    OPC: array[0..7] of TOpCode =       (OPfild,  OPfisttp, OPfist,  OPfist, OPfbld,  OPfild,  OPfbstp, OPfist);
    OPS: array[0..7] of TOpCodeSuffix = (OPSnone, OPSnone,  OPSnone, OPSx_p, OPSnone, OPSnone, OPSnone, OPSx_p);
  begin
    case Code[ModRMIdx] of
      $00..$BF: begin
        SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);
        case ModRM.Index of
          0..3: begin AddMem16; end;
          4, 6: begin AddMem80; end;
          5, 7: begin AddMem64; end;
        end;
      end;
      $E0:      begin
        SetOpcode(OPfnstsw);
        AddOperand({'ax',} RegValue(regGeneral, os16, 0), os16);
      end;
      $E8..$EF: begin
        SetOpcode(OPfucom, OPSx_ip);
        AddReg0; AddRegN;
      end;
      $F0..$F7: begin
        SetOpcode(OPfcom, OPSx_ip);
        AddReg0; AddRegN;
      end;
      $C0..$DF: begin
        SetOpcode(OPX_ReservedX87);
      end;
    else
      SetOpcode(OPX_InvalidX87);
    end;
  end;

begin
  DecodeModRM;
  case Code[CodeIdx] of
    $D8: DoD8;
    $D9: DoD9;
    $DA: DoDA;
    $DB: DoDB;
    $DC: DoDC;
    $DD: DoDD;
    $DE: DoDE;
    $DF: DoDF;
  else
    SetOpcode(OPX_Not87);
  end;
end;

procedure TX86Disassembler.Do3DNow;
var
  n, idx: Byte;
begin
  // 0Fh 0Fh [ModRM] [SIB] [displacement] imm8_opcode
  // sigh, we need to get the operands first, luckely they are all te same.
  AddPq;
  AddQq;
  // to adjust the instruction length, add an empty AnInstruction.operand for the Instruction^.Opcode
  AddOperand({'',} RegValue(regNone), 1);
  // calc index of imm_opcode
  idx := 0;
  if flagModRM in Flags then Inc(idx);
  if flagSib in Flags then Inc(idx);
  for n := 1 to OperIdx do
  begin
    Inc(idx, Instruction^.Operand[n].ByteCount);
    Inc(idx, Instruction^.Operand[n].ByteCount2);
  end;
  // now we can lookup the Instruction^.Opcode
  case Code[CodeIdx + idx] of
    $0C: SetOpcode(OPpi2fw);
    $0D: SetOpcode(OPpi2fd);
    $1C: SetOpcode(OPpf2iw);
    $1D: SetOpcode(OPpf2id);
    $8A: SetOpcode(OPpfnacc);
    $8E: SetOpcode(OPpfpnacc);
    $90: SetOpcode(OPpfcmpge);
    $94: SetOpcode(OPpfmin);
    $96: SetOpcode(OPpfrcp);
    $97: SetOpcode(OPpfrsqrt);
    $9A: SetOpcode(OPpfsub);
    $9E: SetOpcode(OPpfadd);
    $A0: SetOpcode(OPpfcmpgt);
    $A4: SetOpcode(OPpfmax);
    $A6: SetOpcode(OPpfrcpit1);
    $A7: SetOpcode(OPpfrsqit1);
    $AA: SetOpcode(OPpfsubr);
    $AE: SetOpcode(OPpfacc);
    $B0: SetOpcode(OPpfcmpeq);
    $B4: SetOpcode(OPpfmul);
    $B6: SetOpcode(OPpfrcpit2);
    $B7: SetOpcode(OPpmulhrw);
    $BB: SetOpcode(OPpswapd);
    $BF: SetOpcode(OPpavgusb);
  else
    SetOpcode(OPX_3dnow);
  end;
end;

procedure TX86Disassembler.DoGroup1;
const
  OPC: array[0..7] of TOpCode = (OPadd, OPor, OPadc, OPsbb, OPand, OPsub, OPxor, OPcmp);
begin
  Assert(Code[CodeIdx] in [$80..$83,$8F], 'Not group 1');

  DecodeModRM;

  // group 1a
  if Code[CodeIdx] = $8F
  then begin
    Default64;
    if ModRM.Index = 0
    then begin
      SetOpcode(OPpop);
      AddEv;
    end
    else SetOpcode(OPX_group1a);
    Exit;
  end;

  // Group 1
  SetOpcode(OPC[ModRM.Index]);
  case Code[CodeIdx] of
    $80: begin AddEb; AddIb; end;
    $81: begin AddEv; AddIz; end;
    $82: begin AddEb; AddIb; Check32; end;
    $83: begin AddEv; AddIb; end;
  else
    Exit;
  end;
  if (ModRM.Index <> 7)
  then CheckLock;
end;

procedure TX86Disassembler.DoGroup2;
const
  OPC: array[0..7] of TOpCode = (OProl, OPror, OPrcl, OPrcr, OPshl, OPshr, OPsal, OPsar);
begin
  Assert(Code[CodeIdx] in [$C0, $C1, $D0..$D3], 'Not group 2');

  DecodeModRM;

  SetOpcode(OPC[ModRM.Index]);
  case Code[CodeIdx] of
    $C0: begin AddEb; AddIb; end;
    $C1: begin AddEv; AddIb; end;
    $D0: begin AddEb; AddOperand({'1',} RegValue(regOne, os8), os8); end;
    $D1: begin AddEv; AddOperand({'1',} RegValue(regOne, os8), os8); end;
    $D2: begin AddEb; AddReg(regGeneral, os8, REG_C); end;
    $D3: begin AddEv; AddReg(regGeneral, os8, REG_C); end;
  else
    Exit;
  end;
end;

procedure TX86Disassembler.DoGroup3;
const
  OPC: array[0..7] of TOpCode = (OPtest, OPtest, OPnot, OPneg, OPmul, OPimul, OPdiv, OPidiv);
begin
  Assert(Code[CodeIdx] in [$F6,$F7], 'Not group 3');

  DecodeModRM;

  SetOpcode(OPC[ModRM.Index]);
  case Code[CodeIdx] of
    $F6: begin
      if ModRM.Index in [0, 1]
      then begin
        AddEb; AddIb;
      end
      else begin
        AddEb;
      end;
    end;
    $F7: begin
      if ModRM.Index in [0, 1]
      then begin
        AddEv; AddIz;
      end
      else begin
        AddEv;
      end;
    end;
  end;
  if ModRM.Index in [2, 3]
  then CheckLock;
end;

procedure TX86Disassembler.DoGroup4;
begin
  Assert(Code[CodeIdx] = $FE, 'Not group 4');

  DecodeModRM;

  case ModRM.Index of
    0: SetOpcode(OPinc);
    1: SetOpcode(OPdec);
  else
    SetOpcode(OPX_Group4);
    Exit;
  end;
  AddEb;
  CheckLock;
end;

procedure TX86Disassembler.DoGroup5;
begin
  Assert(Code[CodeIdx] = $FF, 'Not group 5');

  DecodeModRM;

  case ModRM.Index of
    0: begin            SetOpcode(OPinc);  AddEv; CheckLock; end;
    1: begin            SetOpcode(OPdec);  AddEv; CheckLock; end;
    2: begin Force64;   SetOpcode(OPcall); AddEv; end;
    3: begin            SetOpcode(OPcall); AddEp; end;
    4: begin Force64;   SetOpcode(OPjmp);  AddEv; end;
    5: begin            SetOpcode(OPjmp);  AddMp; end;
    6: begin Default64; SetOpcode(OPpush); AddEv; end;
  else
    SetOpcode(OPX_Group5);
  end;
end;

procedure TX86Disassembler.DoGroup6;
begin
  Assert(Code[CodeIdx] = $00, 'Not group 6');

  DecodeModRM;

  case ModRM.Index of
    0: begin SetOpcode(OPsldt); AddMw_Rv; end;
    1: begin SetOpcode(OPstr);  AddMw_Rv; end;
    2: begin SetOpcode(OPlldt); AddEw;    end;
    3: begin SetOpcode(OPltr);  AddEw;    end;
    4: begin SetOpcode(OPverr); AddEw;    end;
    5: begin SetOpcode(OPverw); AddEw;    end;
  else
    SetOpcode(OPX_Group6);
  end;
end;

procedure TX86Disassembler.DoGroup7;

{
  Intel and AMD have their own (nonoverlapping) instructions in this group.
  Decoding is based on:

  Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
  Table A-6. Opcode Extensions for One- and Two-byte Opcodes by Group Number

  AMD64 Architecture Programmer’s Manual Volume 3:
  Table A-7. ModRM.reg Extensions for the Secondary Opcode Map
  and
  Table A-8. Opcode 01h ModRM Extensions
}

const
  RM0: array [0..7] of TOpCode = (OPX_Group7, OPvmcall, OPvmlaunch, OPvmresume, OPvmxoff, OPX_Group7, OPX_Group7, OPX_Group7);
  RM1: array [0..7] of TOpCode = (OPmonitor, OPmwait, OPclac, OPstac, OPX_Group7, OPX_Group7, OPX_Group7, OPencls);
  RM2: array [0..7] of TOpCode = (OPgetbv, OPsetbv, OPX_Group7, OPX_Group7, OPvmfunc, OPxend, OPxtest, OPenclu);
  RM3: array [0..7] of TOpCode = (OPvmrun, OPvmmcall, OPvmload, OPvmsave, OPstgi, OPclgi, OPskinit, OPinvlpga);
begin
  Assert(Code[CodeIdx] = $01, 'Not group 7');

  DecodeModRM;

  SetOpcode(OPX_Group7);

  if ModRM.Mode = 3
  then begin
    case ModRM.Index of
      0: begin SetOpcode(RM0[ModRM.RM]); end;
      1: begin SetOpcode(RM1[ModRM.RM]); end;
      2: begin SetOpcode(RM2[ModRM.RM]); end;
      3: begin SetOpcode(RM3[ModRM.RM]); end;
      4: begin SetOpcode(OPsmsw); AddMw_Rv; end;
      5: begin
        if preF3 in Flags
        then begin
          Exclude(Flags, preF3);
          case ModRM.RM of
            0: SetOpcode(OPsetssbsy);
            2: SetOpcode(OPsaveprevssp);
          end;
        end
        else if not (preF2 in Flags)
        then begin
          case ModRM.RM of
            6: SetOpcode(OPrdpkru);
            7: SetOpcode(OPwrpkru);
          end;
        end;
      end;
      6: begin SetOpcode(OPlmsw); AddEw; end;
      7: begin
        case ModRM.RM of
          0: begin Check64; SetOpcode(OPswapgs); end;
          1: begin SetOpcode(OPrdtsc, OPSx_p); end;
          2: begin
            if preF3 in Flags
            then begin
              Exclude(Flags, preF3);
              SetOpcode(OPmonitor, OPSx_x);
            end
            else if not (preF2 in Flags)
            then begin
              SetOpcode(OPmcommit);
            end;
          end;
          3: begin
            if [preF2, preF3] * Flags = []
            then begin
              SetOpcode(OPmwait, OPSx_x);
            end;
          end;
          5: begin
            if [preF2, preF3] * Flags = []
            then begin
              SetOpcode(OPrdpru);
            end;
          end;
          6: begin
            if [preF2, preF3] * Flags = [preF3]
            then begin
              Exclude(Flags, preF3);
              SetOpcode(OPrmpadjust);
            end
            else if [preF2, preF3] * Flags = [preF2]
            then begin
              Exclude(Flags, preF2);
              SetOpcode(OPrmpupdate);
            end;
          end;
          7: begin
            if [preF2, preF3] * Flags = [preF3]
            then begin
              Exclude(Flags, preF3);
              SetOpcode(OPpsmash);
            end
            else if [preF2, preF3] * Flags = [preF2]
            then begin
              Exclude(Flags, preF2);
              SetOpcode(OPpvalidate);
            end;
          end;
        end;
      end;
    end;
  end
  else begin
    case ModRM.Index of
      0: begin SetOpcode(OPsgdt);   AddMs; end;
      1: begin SetOpcode(OPsidt);   AddMs; end;
      2: begin SetOpcode(OPlgdt);   AddMs; end;
      3: begin SetOpcode(OPlidt);   AddMs; end;
      4: begin SetOpcode(OPsmsw);   AddMw_Rv; end;
      //5 : invalid
      6: begin SetOpcode(OPlmsw);   AddEw; end;
      7: begin SetOpcode(OPinvlpg); AddMb; end;
    end;
  end;
end;

procedure TX86Disassembler.DoGroup8;
const
  RM8: array [0..7] of TOpCode = (OPX_Group8, OPX_Group8, OPX_Group8, OPX_Group8, OPbt, OPbts, OPbtr, OPbtc);
begin
  Assert(Code[CodeIdx] = $BA, 'Not group 8');

  DecodeModRM;

  if ModRM.Index < 4
  then SetOpcode(OPX_Group8)
  else SetOpcode(RM8[ModRM.Index]);
  AddEv; AddIb;

  if ModRM.Index in [5..7]
  then CheckLock;
end;

procedure TX86Disassembler.DoGroup9;
begin
  Assert(Code[CodeIdx] = $C7, 'Not group 9');

  DecodeModRM;

  SetOpcode(OPX_Group9);
  case ModRM.Index of
    1: begin
      DecodeSIMD([soNone]);
      if SimdOpcode = soNone
      then begin
        if OperandSize = os64
        then begin
          SetOpcode(OPcmpxchg, OPSx_16b);
          AddMdq;
        end
        else begin
          SetOpcode(OPcmpxchg, OPSx_8b);
          AddMq;
        end;
        CheckLock;
      end;
    end;
    6: begin
      if ModRM.Mode = 3
      then begin
        DecodeSIMD([soNone]);
        case SimdOpcode of
          soNone: begin SetOpcode(OPrdrand); AddRv; end;
        end;
      end
      else begin
        DecodeSIMD([soNone, so66, soF3]);
        case SimdOpcode of
          soNone: begin SetOpcode(OPvmptrld); AddMq; end;
          so66:   begin SetOpcode(OPvmclear); AddMq; end;
          soF3:   begin SetOpcode(OPvmxon);   AddMq; end;
        end;
      end;
    end;
    7: begin
      if ModRM.Mode = 3
      then begin
        DecodeSIMD([soNone, soF3]);
        case SimdOpcode of
          soNone: begin SetOpcode(OPrdseed); AddRv; end;
          soF3:   begin SetOpcode(OPrdpid);  AddRd_q; end;
        end;
      end
      else begin
        DecodeSIMD([soNone]);
        case SimdOpcode of
          soNone: begin SetOpcode(OPvmptrst); AddMq; end;
        end;
      end;
    end;
  end;
end;

procedure TX86Disassembler.DoGroup10;
begin
  Assert(Code[CodeIdx] = $B9, 'Not group 10');

  DecodeModRM;

  SetOpcode(OPud1);
  AddGv;AddEv;
end;

procedure TX86Disassembler.DoGroup11;
begin
  Assert(Code[CodeIdx] in [$C6,$C7], 'Not group 11');

  DecodeModRM;

  SetOpcode(OPX_Group11);

  case ModRM.Index of
    0: begin
      case Code[CodeIdx] of
        $C6: begin SetOpcode(OPmov); AddEb; AddIb; end;
        $C7: begin SetOpcode(OPmov); AddEv; AddIz; end;
      end;
    end;
    7: begin
      if (ModRm.Mode = 3) and (ModRM.RM = 0)
      then begin
        case Code[CodeIdx] of
          $C6: begin SetOpcode(OPxabort); AddIb; end;
          $C7: begin SetOpcode(OPxbegin); AddJz; end;
        end;
      end;
    end;
  end;
end;

procedure TX86Disassembler.DoGroup12;
const
  OPC: array[0..7] of TOpCode = (OPX_Invalid, OPX_Invalid, OPpsrl, OPX_Invalid, OPpsra, OPX_Invalid, OPpsll, OPX_Invalid);
begin
  Assert(Code[CodeIdx] = $71, 'Not group 12');

  DecodeModRM;

  DecodeSIMD([soNone, so66]);
  if (SimdOpcode in [soNone, so66]) and (OPC[ModRM.Index] <> OPX_Invalid)
  then begin
    case SimdOpcode of
      soNone: begin SetOpcode(OPC[ModRM.Index], OPSx_w      ); AddNq;        AddIb; end;
      so66:   begin SetOpcode(OPC[ModRM.Index], OPSx_w, True); AddHx; AddUx; AddIb; end;
    end;
  end
  else begin
    SetOpcode(OPX_Group12);
  end;
end;

procedure TX86Disassembler.DoGroup13;
const
  OPC: array[0..7] of TOpCode = (OPX_Invalid, OPX_Invalid, OPpsrl, OPX_Invalid, OPpsra, OPX_Invalid, OPpsll, OPX_Invalid);
begin
  Assert(Code[CodeIdx] = $72, 'Not group 13');

  DecodeModRM;

  DecodeSIMD([soNone, so66]);
  if (SimdOpcode in [soNone, so66]) and (OPC[ModRM.Index] <> OPX_Invalid)
  then begin
    case SimdOpcode of
      soNone: begin SetOpcode(OPC[ModRM.Index], OPSx_d      ); AddNq;        AddIb; end;
      so66:   begin SetOpcode(OPC[ModRM.Index], OPSx_d, True); AddHx; AddUx; AddIb; end;
    end;
  end
  else begin
    SetOpcode(OPX_Group12);
  end;
end;

procedure TX86Disassembler.DoGroup14;
const
  OPC: array[0..7] of TOpCode       = (OPX_Invalid, OPX_Invalid, OPpsrl, OPpsrl,  OPX_Invalid, OPX_Invalid, OPpsll, OPpsll);
  OPS: array[0..7] of TOpCodeSuffix = (OPSnone,     OPSnone,     OPSx_q, OPSx_dq, OPSnone,     OPSnone,     OPSx_q, OPSx_dq);
begin
  Assert(Code[CodeIdx] = $73, 'Not group 14');

  DecodeModRM;
  SetOpcode(OPX_Group14);

  if OPC[ModRM.Index] <> OPX_Invalid
  then begin
    DecodeSIMD([soNone, so66]);

    SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]);

    case SimdOpcode of
      soNone: begin
        if ModRM.Index in [2,6]
        then begin SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index]); AddNq; AddIb; end;
      end;
      so66: begin SetOpcode(OPC[ModRM.Index], OPS[ModRM.Index], True); AddHx; AddUx; AddIb; end;
    end;
  end;
end;

procedure TX86Disassembler.DoGroup15;
begin
  Assert(Code[CodeIdx] = $AE, 'Not group 15');

  DecodeModRM;
  DecodeSIMD([soNone, soF3]);

  SetOpcode(OPX_Group15);

  case SimdOpcode of
    soNone: begin
      if ModRM.Mode = 3
      then begin
        case ModRM.Index of
          5: begin SetOpcode(OPlfence);         end;
          6: begin SetOpcode(OPmfence);         end;
          7: begin SetOpcode(OPsfence);         end
        end;
      end
      else begin
        //@@@ check rex -> xsave64
        case ModRM.Index of
          0: begin SetOpcode(OPfxsave                ); AddModRM([modMem], os4096, regNone); end;
          1: begin SetOpcode(OPfxrstor               ); AddModRM([modMem], os4096, regNone); end;
          2: begin SetOpcode(OPldmxcsr, OPSnone, True); AddMd; end;
          3: begin SetOpcode(OPstmxcsr, OPSnone, True); AddMd; end;
          4: begin SetOpcode(OPxsave                 ); AddModRM([modMem], os4096, regNone); end; // the data is variable in size.
          5: begin SetOpcode(OPxrstor                ); AddModRM([modMem], os4096, regNone); end; // the first 512 bytes are the same as fxsave
          6: begin SetOpcode(OPxsaveopt              ); AddModRM([modMem], os4096, regNone); end; //
          7: begin SetOpcode(OPclflush               ); AddMb; end
        end;
      end;
    end;
    soF3: begin
      if ModRM.Mode = 3
      then begin
        case ModRM.Index of
          0: begin SetOpcode(OPrdfsbase); AddRy; end;
          1: begin SetOpcode(OPrdgsbase); AddRy; end;
          2: begin SetOpcode(OPwrfsbase); AddRy; end;
          3: begin SetOpcode(OPwrgsbase); AddRy; end;
        end;
      end;
    end;
  end;
end;

procedure TX86Disassembler.DoGroup16;
const
  OPS: array[0..3] of TOpCodeSuffix = (OPSp_nta, OPSp_t0, OPSp_t1, OPSp_t2);
begin
  Assert(Code[CodeIdx] = $18, 'Not group 16');

  DecodeModRM;

  if (ModRM.Mode <> 3) and (ModRM.Index < 4)
  then begin
    SetOpcode(OPprefetch, OPS[ModRM.Index]);
    AddMb;
  end
  else SetOpcode(OPnop);
end;

procedure TX86Disassembler.DoGroup17;
begin
  Assert(Code[CodeIdx] = $F3, 'Not group 17');

  DecodeModRM;

  case ModRM.Index of
    1: SetOpcode(OPblsr);
    2: SetOpcode(OPblsmsk);
    3: SetOpcode(OPblsi);
  else
    Exit;
  end;
  CheckVex;
  AddBy; AddEy;
end;

procedure TX86Disassembler.DoGroupP;
begin
  Assert(Code[CodeIdx] = $0D, 'Not group P');

  DecodeModRM;

  if (ModRM.Mode <> 3) and (ModRM.Index < 2)
  then begin
    if ModRM.Index = 0
    then SetOpcode(OPprefetch)
    else SetOpcode(OPprefetch, OPSp_w);
    AddMb;
  end;
end;

procedure TX86Disassembler.Do2ByteOpcode;
const
  OPC_5x: array[0..$F] of TOpcode = (
    OPX_Invalid, OPsqrt, OPrsqrt, OPrcp,
    OPand, OPandn, OPor, OPxor,
    OPadd, OPmul, OPX_Invalid, OPX_Invalid,
    OPsub, OPmin, OPdiv, OPmax
  );
  OPC_6x: array[0..$F] of TOpCode = (
    OPpunpcklbw, OPpunpcklwd, OPpunpckldq, OPpacksswb,
    OPpcmpgt, OPpcmpgt, OPpcmpgt, OPpackuswb,
    OPpunpckhbw, OPpunpckhwd, OPpunpckhdq, OPpackssdw,
    OPpunpcklqdq, OPpunpckhqdq, OPX_Invalid, OPX_Invalid
  );
  OPS_6x: array[0..$F] of TOpCodeSuffix = (
    OPSnone, OPSnone, OPSnone, OPSnone,
    OPSx_b, OPSx_w, OPSx_d, OPSnone,
    OPSnone, OPSnone, OPSnone, OPSnone,
    OPSnone, OPSnone, OPSnone, OPSnone
  );
  OPC_Dx: array[0..$F] of TOpCode = (
    OPX_Invalid, OPpsrl, OPpsrl, OPpsrl,
    OPpadd, OPpmull, OPX_Invalid, OPX_Invalid,
    OPpsubus, OPpsubus, OPpminu, OPpand,
    OPpaddus, OPpaddus, OPpmaxu, OPpandn
  );
  OPS_Dx: array[0..$F] of TOpCodeSuffix = (
    OPSnone, OPSx_w, OPSx_d, OPSx_q,
    OPSx_q, OPSx_w, OPSnone, OPSnone,
    OPSx_b, OPSx_w, OPSx_b, OPSnone,
    OPSx_b, OPSx_w, OPSx_b, OPSnone
  );
  OPC_Ex: array[0..$F] of TOpCode = (
    OPpavg, OPpsra, OPpsra, OPpavg,
    OPpmulhuw, OPpmulhw, OPX_Invalid, OPX_Invalid,
    OPpsubs, OPpsubs, OPpmins, OPpor,
    OPpadds, OPpadds, OPpmaxs, OPpxor
  );
  OPS_Ex: array[0..$F] of TOpCodeSuffix = (
    OPSx_b, OPSx_w, OPSx_d, OPSx_w,
    OPSnone, OPSnone, OPSnone, OPSnone,
    OPSx_b, OPSx_w, OPSx_w, OPSnone,
    OPSx_b, OPSx_w, OPSx_w, OPSnone
  );
  OPC_Fx: array[0..$F] of TOpCode = (
    OPX_Invalid, OPpsll, OPpsll, OPpsll,
    OPpmuludq, OPpmaddwd, OPpsadbw, OPX_Invalid,
    OPpsub, OPpsub, OPpsub, OPpsub,
    OPpadd, OPpadd, OPpadd, OPX_Invalid
  );
  OPS_Fx: array[0..$F] of TOpCodeSuffix = (
    OPSnone, OPSx_w, OPSx_d, OPSx_q,
    OPSnone, OPSnone, OPSnone, OPSnone,
    OPSx_b, OPSx_w, OPSx_d, OPSx_q,
    OPSx_b, OPSx_w, OPSx_d, OPSnone
  );
var
  idx: Integer;
  ValidSimd: TSimdOpcodes;
begin
  case Code[CodeIdx] of
    $00: begin
      DoGroup6;
    end;
    $01: begin
      DoGroup7;
    end;
    $02: begin
      SetOpcode(OPlar);
      AddGv; AddEw;
    end;
    $03: begin
      SetOpcode(OPlsl);
      AddGv; AddEw;
    end;
    // $04: invalid
    $05: begin
      Check64;
      SetOpcode(OPsyscall);
    end;
    $06: begin
      SetOpcode(OPclts);
    end;
    $07: begin
      Check64;
      SetOpcode(OPsysret);
    end;
    $08: begin
      SetOpcode(OPinvd);
    end;
    $09: begin
      SetOpcode(OPwbinvd);
    end;
    // $0A: invalid
    $0B: begin
      SetOpcode(OPud2);
    end;
    // $0C: invalid
    $0D: begin
      DoGroupP;
    end;
    $0E: begin
      // AMD
      SetOpcode(OPfemms);
    end;
    $0F: begin
      // AMD
      Do3DNow;
    end;
    //---
    $10: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovu, OPSx_ps, True); AddVps; AddWps;         end;
        so66:   begin SetOpcode(OPmovu, OPSx_pd, True); AddVpd; AddWpd;         end;
        soF2:   begin
                 DecodeModRM;
                 if ModRM.Mode = 3
                 then begin SetOpcode(OPmov,  OPSx_sd, True); AddVx;  AddHx;  AddWsd; end
                 else begin SetOpcode(OPmov,  OPSx_sd, True); AddVx;          AddWsd; end;
                end;
        soF3:   begin
                 DecodeModRM;
                 if ModRM.Mode = 3
                 then begin SetOpcode(OPmov,  OPSx_ss, True); AddVx;  AddHx;  AddWss; end
                 else begin SetOpcode(OPmov,  OPSx_ss, True); AddVx;          AddWss; end;
                end;
      end;
    end;
    $11: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovu, OPSx_ps, True); AddWps; AddVps;         end;
        so66:   begin SetOpcode(OPmovu, OPSx_pd, True); AddWpd; AddVpd;         end;
        soF2:   begin
                 DecodeModRM;
                 if ModRM.Mode = 3
                 then begin SetOpcode(OPmov,  OPSx_sd, True); AddWsd; AddHx; AddVsd; end
                 else begin SetOpcode(OPmov,  OPSx_sd, True); AddWsd;        AddVsd; end;
                end;
        soF3:   begin
                 DecodeModRM;
                 if ModRM.Mode = 3
                 then begin SetOpcode(OPmov,  OPSx_ss, True); AddWss; AddHx; AddVss; end
                 else begin SetOpcode(OPmov,  OPSx_ss, True); AddWss;        AddVss; end;
                end;
      end;
    end;
    $12: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin
          DecodeModRM;
          if ModRM.Mode = 3
          then begin SetOpcode(OPmovhlps, OPSnone, True); AddVq; AddHq; AddUq end
          else begin SetOpcode(OPmovl,    OPSx_ps, True); AddVq; AddHq; AddMq end;
        end;
        so66: begin SetOpcode(OPmovl,     OPSx_pd, True); AddVq; AddHq; AddMq; end;
        soF2: begin SetOpcode(OPmovddup,  OPSnone, True); AddVx; AddWx;        end;
        soF3: begin SetOpcode(OPmovsldup, OPSnone, True); AddVx; AddWx;        end;
      end;
    end;
    $13: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovl, OPSx_ps, True); AddMq; AddVps; end;
        so66:   begin SetOpcode(OPmovl, OPSx_pd, True); AddMq; AddVsd; end;
      end;
    end;
    $14: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPunpckl, OPSx_ps, True); AddVx; AddHx; AddWx; end;
        so66:   begin SetOpcode(OPunpckl, OPSx_pd, True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $15: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPunpckh, OPSx_ps, True); AddVx; AddHx; AddWx; end;
        so66:   begin SetOpcode(OPunpckh, OPSx_pd, True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $16: begin
      DecodeSIMD([soNone, so66, soF3]);
      case SimdOpcode of
        soNone: begin
          DecodeModRM;
          if ModRM.Mode = 3
          then begin SetOpcode(OPmovlh, OPSx_ps, True); AddVdq; AddHq; AddUq end
          else begin SetOpcode(OPmovh,  OPSx_ps, True); AddVdq; AddHq; AddMq end;
        end;
        so66: begin SetOpcode(OPmovh,     OPSx_pd, True); AddVdq; AddHq; AddMq end;
        soF3: begin SetOpcode(OPmovshdup, OPSnone, True); AddVx;  AddWx;       end;
      end;
    end;
    $17: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovh, OPSx_ps, True); AddMq; AddVq; end;
        so66:   begin SetOpcode(OPmovh, OPSx_pd, True); AddMq; AddVq; end;
      end;
    end;
    $18: begin
      DoGroup16;
    end;
    $19: begin
      SetOpcode(OPnop);
      AddEv;
    end;
    $1A: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPbndldx); AddModReg(regBounds, os128); AddModRM([        modMem], os128, regBounds); end;
        so66:   begin SetOpcode(OPbndmov); AddModReg(regBounds, os128); AddModRM([modReg, modMem], os128, regBounds); end;
        soF2:   begin SetOpcode(OPbndcu ); AddModReg(regBounds, os128); AddModRM([modReg, modMem], os128, regBounds); end;
        soF3:   begin SetOpcode(OPbndcl ); AddModReg(regBounds, os128); AddModRM([modReg, modMem], os128, regBounds); end;
      end;
    end;
    $1B: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPbndstx); AddModRM([        modMem], os128, regBounds); AddModReg(regBounds, os128); end;
        so66:   begin SetOpcode(OPbndmov); AddModRM([modReg, modMem], os128, regBounds); AddModReg(regBounds, os128); end;
        soF2:   begin SetOpcode(OPbndmk ); AddModReg(regBounds, os128); AddModRM([        modMem], os128, regBounds); end;
        soF3:   begin SetOpcode(OPbndcn ); AddModReg(regBounds, os128); AddModRM([modReg, modMem], os128, regBounds); end;
      end;
    end;
    $1C..$1D: begin
      SetOpcode(OPnop);
      AddEv;
    end;
    $1E: begin
      DecodeSIMD([soNone, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPNop);          AddEv; end;
        soF3:   begin SetOpcode(OPrdss, OPSx_p); AddRy; end;
      end;
    end;
    $1F: begin
      SetOpcode(OPnop);
      AddEv;
    end;
    //---
    $20: begin
      SetOpcode(OPmov);
      AddRd; AddCd;
    end;
    $21: begin
      SetOpcode(OPmov);
      AddRd; AddDd;
    end;
    $22: begin
      SetOpcode(OPmov);
      AddCd; AddRd;
    end;
    $23: begin
      SetOpcode(OPmov);
      AddDd; AddRd;
    end;
    // $24..$27: OPX_Invalid
    $28: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmova, OPSx_ps, True); AddVps; AddWps; end;
        so66:   begin SetOpcode(OPmova, OPSx_pd, True); AddVpd; AddWpd; end;
      end;
    end;
    $29: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmova, OPSx_ps, True); AddWps; AddVps; end;
        so66:   begin SetOpcode(OPmova, OPSx_pd, True); AddWpd; AddVpd; end;
      end;
    end;
    $2A: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPcvtpi2, OPSx_ps      ); AddVps; AddQpi; end;
        so66:   begin SetOpcode(OPcvtpi2, OPSx_pd      ); AddVpd; AddQpi; end;
        soF2:   begin SetOpcode(OPcvtsi2, OPSx_sd, True); AddVsd; AddHsd; AddEy; end;
        soF3:   begin SetOpcode(OPcvtsi2, OPSx_ss, True); AddVss; AddHss; AddEy; end;
      end;
    end;
    $2B: begin
      DecodeSIMD([soNone, so66, soF2, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovnt, OPSx_ps, True); AddMps; AddVps; end;
        so66:   begin SetOpcode(OPmovnt, OPSx_pd, True); AddMpd; AddVpd; end;
        soF2:   begin SetOpcode(OPmovnt, OPSx_sd, True); AddMpd; AddVpd; end;
        soF3:   begin SetOpcode(OPmovnt, OPSx_ss, True); AddMpd; AddVpd; end;
      end;
    end;
    $2C: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPcvttps2, OPSx_pi      ); AddPpi; AddWps; end;
        so66:   begin SetOpcode(OPcvttpd2, OPSx_pi      ); AddPpi; AddWpd; end;
        soF2:   begin SetOpcode(OPcvttsd2, OPSx_si, True); AddGy;  AddWsd; end;
        soF3:   begin SetOpcode(OPcvttss2, OPSx_si, True); AddGy;  AddWss; end;
      end;
    end;
    $2D: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPcvtps2, OPSx_pi      ); AddPpi; AddWps; end;
        so66:   begin SetOpcode(OPcvtpd2, OPSx_pi      ); AddPpi; AddWpd; end;
        soF2:   begin SetOpcode(OPcvtsd2, OPSx_si, True); AddGy;  AddWsd; end;
        soF3:   begin SetOpcode(OPcvtss2, OPSx_si, True); AddGy;  AddWss; end;
      end;
    end;
    $2E: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPucomi, OPSx_ss, True); AddVss; AddWss; end;
        so66:   begin SetOpcode(OPucomi, OPSx_sd, True); AddVsd; AddWsd; end;
      end;
    end;
    $2F: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPcomi, OPSx_ss, True); AddVss; AddWss; end;
        so66:   begin SetOpcode(OPcomi, OPSx_sd, True); AddVsd; AddWsd; end;
      end;
    end;
    //---
    $30: begin
      SetOpcode(OPwrmsr);
    end;
    $31: begin
      SetOpcode(OPrdtsc);
    end;
    $32: begin
      SetOpcode(OPrdmsr);
    end;
    $33: begin
      SetOpcode(OPrdpmc);
    end;
    $34: begin
      SetOpcode(OPsysenter);
    end;
    $35: begin
      SetOpcode(OPsysexit);
    end;
    // $36: OPX_Invalid
    $37: begin
      SetOpcode(OPgetsec);
    end;
    $38: begin
      Inc(CodeIdx);
      Inc(ModRMIdx);
      Do3ByteOpcode38;
    end;
    // $39: OPX_Invalid
    $3A: begin
      Inc(CodeIdx);
      Inc(ModRMIdx);
      Do3ByteOpcode3A;
    end;
    // $3B..$3F: OPX_Invalid
    //---
    $40..$4F: begin
      SetOpcode(OPcmov__, StdCond(Code[CodeIdx]));
      AddGv; AddEv;
    end;
    //---
    $50: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovmsk, OPSx_ps, True); AddGy; AddUps; end;
        so66:   begin SetOpcode(OPmovmsk, OPSx_pd, True); AddGy; AddUpd; end;
      end;
    end;
    $51..$53: begin
      case Code[CodeIdx] of
        $52: ValidSimd := [soNone, soF3];
        $53: ValidSimd := [soNone, soF3];
      else
        ValidSimd := [soNone, so66, soF2, soF3];
      end;
      DecodeSIMD(ValidSimd);

      if SimdOpcode in ValidSimd
      then begin
        case SimdOpcode of
          soNone: begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_ps, True); AddVps; AddWps; end;
          so66:   begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_pd, True); AddVpd; AddWpd; end;
          soF2:   begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_sd, True); AddVsd; AddHsd; AddWsd; end;
          soF3:   begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_ss, True); AddVss; AddHss; AddWss; end;
        end;
      end;
    end;
    $54..$59, $5C..$5F: begin
      case Code[CodeIdx] of
        $54: ValidSimd := [soNone, so66];
        $55: ValidSimd := [soNone, so66];
        $56: ValidSimd := [soNone, so66];
        $57: ValidSimd := [soNone, so66];
      else
        ValidSimd := [soNone, so66, soF2, soF3];
      end;
      DecodeSIMD(ValidSimd);

      if SimdOpcode in ValidSimd
      then begin
        case SimdOpcode of
          soNone: begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_ps, True); AddVps; AddHps; AddWps; end;
          so66:   begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_pd, True); AddVpd; AddHpd; AddWpd; end;
          soF2:   begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_sd, True); AddVsd; AddHsd; AddWsd; end;
          soF3:   begin SetOpcode(OPC_5x[Code[CodeIdx] and $F], OPSx_ss, True); AddVss; AddHss; AddWss; end;
        end;
      end;
    end;
    $5A: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPcvtps2, OPSx_pd, True); AddVpd; AddWps; end;
        so66:   begin SetOpcode(OPcvtpd2, OPSx_ps, True); AddVps; AddWpd; end;
        soF2:   begin SetOpcode(OPcvtsd2, OPSx_ss, True); AddVss; AddHx; AddWsd; end;
        soF3:   begin SetOpcode(OPcvtss2, OPSx_sd, True); AddVsd; AddHx; AddWss; end;
      end;
    end;
    $5B: begin
      DecodeSIMD([soNone, so66, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPcvtdq2,  OPSx_ps, True); AddVps; AddWdq; end;
        so66:   begin SetOpcode(OPcvtps2,  OPSx_dq, True); AddVdq; AddWps; end;
        soF3:   begin SetOpcode(OPcvttps2, OPSx_dq, True); AddVdq; AddWps; end;
      end;
    end;
    // $5C..$5F: see $54
    //---
    $60..$6B: begin
      idx := Code[CodeIdx] and $F;
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPC_6x[idx], OPS_6x[idx]      ); AddPq;  AddQd; end;
        so66:   begin SetOpcode(OPC_6x[idx], OPS_6x[idx], True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $6C..$6D: begin
      idx := Code[CodeIdx] and $F;
      DecodeSIMD([so66]);
      if SimdOpcode = so66
      then begin SetOpcode(OPC_6x[idx], OPS_6x[idx], True); AddVx; AddHx; AddWx; end;
    end;
    $6E: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmov, OPS_d_q      ); AddPd; AddEy; end;
        so66:   begin SetOpcode(OPmov, OPS_d_q, True); AddVy; AddEy; end;
      end;
    end;
    $6F: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPmov, OPSx_q        ); AddPq; AddQq; end;
        so66:   begin SetOpcode(OPmov, OPS_Wxx(OPSx_dqa, OPSx_dqa32, OPSx_dqa64), True); AddVx; AddWx; end;
        soF2:   begin SetOpcode(OPmov, OPS_Wxx(OPSx_dqu, OPSx_dqu8,  OPSx_dqu16), True); AddVx; AddWx; end;
        soF3:   begin SetOpcode(OPmov, OPS_Wxx(OPSx_dqu, OPSx_dqu32, OPSx_dqu64), True); AddVx; AddWx; end;
      end;
    end;
    //---
    $70: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPpshuf, OPSx_w       ); AddPq; AddQq; AddIb; end;
        so66:   begin SetOpcode(OPpshuf, OPSx_d,  True); AddVx; AddWx; AddIb; end;
        soF2:   begin SetOpcode(OPpshuf, OPSx_lw, True); AddVx; AddWx; AddIb; end;
        soF3:   begin SetOpcode(OPpshuf, OPSx_hw, True); AddVx; AddWx; AddIb; end;
      end;
    end;
    $71: begin
      DoGroup12
    end;
    $72: begin
      DoGroup13
    end;
    $73: begin
      DoGroup14
    end;
    $74: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPpcmpeq, OPSx_b      ); AddPq; AddQq; end;
        so66:   begin SetOpcode(OPpcmpeq, OPSx_b, True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $75: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPpcmpeq, OPSx_w      ); AddPq; AddQq; end;
        so66:   begin SetOpcode(OPpcmpeq, OPSx_w, True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $76: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPpcmpeq, OPSx_d      ); AddPq; AddQq; end;
        so66:   begin SetOpcode(OPpcmpeq, OPSx_d, True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $77: begin
      DecodeSIMD([soNone]);
      if SimdOpcode = soNone
      then begin
        if flagVex in Flags
        then begin
          Exclude(Flags,flagModRM);
          if Vex.VectorLength = os128
          then SetOpcode(OPvzeroupper)
          else SetOpcode(OPvzeroall);
        end
        else SetOpcode(OPemms);
      end;
    end;
    $78: begin
      DecodeSIMD([soNone, so66, soF2]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPvmread); AddEy; AddGy; end;
        so66:   begin
                 DecodeModRM;
                 if ModRM.Index = 0
                 then begin SetOpcode(OPextrq); AddVq; AddIb; AddIb; end;
                end;
        soF2:   begin SetOpcode(OPinsert, OPSx_q); AddVq; AddUdq; AddIb; AddIb; end;
      end;
    end;
    $79: begin
      DecodeSIMD([soNone, so66, soF2]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPvmwrite);        AddGy; AddEy;  end;
        so66:   begin SetOpcode(OPextrq);          AddVq; AddUdq; end;
        soF2:   begin SetOpcode(OPinsert, OPSx_q); AddVq; AddUdq; end;
      end;
    end;
    // $7A..$7B: OPX_Invalid
    $7C: begin
      DecodeSIMD([so66, soF2]);
      case SimdOpcode of
        so66: begin SetOpcode(OPhadd, OPSx_pd, True); AddVpd; AddHpd; AddWpd; end;
        soF2: begin SetOpcode(OPhadd, OPSx_ps, True); AddVps; AddHps; AddWps; end;
      end;
    end;
    $7D: begin
      DecodeSIMD([so66, soF2]);
      case SimdOpcode of
        so66: begin SetOpcode(OPsub, OPSx_pd, True); AddVpd; AddHpd; AddWpd; end;
        soF2: begin SetOpcode(OPsub, OPSx_ps, True); AddVps; AddHps; AddWps; end;
      end;
    end;
    $7E: begin
      DecodeSIMD([soNone, so66, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmov, OPS_d_q      ); AddEy; AddPy; end;
        so66:   begin SetOpcode(OPmov, OPS_d_q, True); AddEy; AddVy; end;
        soF3:   begin SetOpcode(OPmov, OPSx_q , True); AddVq; AddWq; end;
      end;
    end;
    $7F: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPmov, OPSx_q        ); AddQq; AddPq; end;
        so66:   begin SetOpcode(OPmov, OPS_Wxx(OPSx_dqa, OPSx_dqa32, OPSx_dqa64), True); AddWx; AddVx; end;
        soF2:   begin SetOpcode(OPmov, OPS_Wxx(OPSx_dqu, OPSx_dqu8,  OPSx_dqu16), True); AddWx; AddVx; end;
        soF3:   begin SetOpcode(OPmov, OPS_Wxx(OPSx_dqu, OPSx_dqu32, OPSx_dqu64), True); AddWx; AddVx; end;
      end;
    end;
    //---
    $80..$8F: begin
      Force64;
      SetOpcode(OPj__, StdCond(Code[CodeIdx]));
      AddJz;
    end;
    //---
    $90..$9F: begin
      SetOpcode(OPset__, StdCond(Code[CodeIdx]));
      AddEb;
    end;
    //---
    $A0: begin
      Default64;
      SetOpcode(OPpush);
      AddReg(regSegment, MODE_SIZE[ProcessMode], REG_FS);
    end;
    $A1: begin
      Default64;
      SetOpcode(OPpop);
      AddReg(regSegment, MODE_SIZE[ProcessMode], REG_FS);
    end;
    $A2: begin
      SetOpcode(OPcpuid);
    end;
    $A3: begin
      SetOpcode(OPbt);
      AddEv; AddGv;
    end;
    $A4: begin
      SetOpcode(OPshl, OPSx_d);
      AddEv; AddGv; AddIb;
    end;
    $A5: begin
      SetOpcode(OPshl, OPSx_d);
      AddEv; AddGv;
      AddReg(regGeneral, os8, REG_C);
    end;
    // $A6..$A7: OPX_Invalid
    $A8: begin
      Default64;
      SetOpcode(OPpush);
      AddReg(regSegment, MODE_SIZE[ProcessMode], REG_GS);
    end;
    $A9: begin
      Default64;
      SetOpcode(OPpop);
      AddReg(regSegment, MODE_SIZE[ProcessMode], REG_GS);
    end;
    $AA: begin
      SetOpcode(OPrsm);
    end;
    $AB: begin
      SetOpcode(OPbts);
      AddEv; AddGv;
    end;
    $AC: begin
      SetOpcode(OPshr, OPSx_d);
      AddEv; AddGv; AddIb;
    end;
    $AD: begin
      SetOpcode(OPshl, OPSx_d);
      AddEv; AddGv;
      AddReg(regGeneral, os8, REG_C);
    end;
    $AE: begin
      DoGroup15;
    end;
    $AF: begin
      SetOpcode(OPimul);
      AddGv; AddEv;
    end;
    //---
    $B0: begin
      SetOpcode(OPcmpxchg);
      AddEb; AddGb;
      CheckLock;
    end;
    $B1: begin
      SetOpcode(OPcmpxchg);
      AddEv; AddGv;
      CheckLock;
    end;
    $B2: begin
      SetOpcode(OPlss);
      AddGv; AddMp;
    end;
    $B3: begin
      SetOpcode(OPbtr);
      AddEv; AddGv;
    end;
    $B4: begin
      SetOpcode(OPlfs);
      AddGv; AddMp;
    end;
    $B5: begin
      SetOpcode(OPlgs);
      AddGv; AddMp;
    end;
    $B6: begin
      SetOpcode(OPmovzx);
      AddGv; AddEb;
    end;
    $B7: begin
      SetOpcode(Opmovzx);
      AddGv; AddEw;
    end;
    $B8: begin
      DecodeSIMD([soNone, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPjmpe  ); AddIz;        end;  // Itanium SDM, volume 4, page 256.
        soF3:   begin SetOpcode(OPpopcnt); AddGv; AddEv; end;
      end;
    end;
    $B9: begin
      DoGroup10;
    end;
    $BA: begin
      DoGroup8;
    end;
    $BB: begin
      SetOpcode(OPbtc);
      AddEv; AddGv;
    end;
    $BC: begin
      DecodeSIMD([soNone, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPbsf  ); AddGv; AddEv; end;
        soF3:   begin SetOpcode(OPtzcnt); AddGv; AddEv; end;
      end;
    end;
    $BD: begin
      DecodeSIMD([soNone, soF3]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPbsr  ); AddGv; AddEv; end;
        soF3:   begin SetOpcode(OPlzcnt); AddGv; AddEv; end;
      end;
    end;
    $BE: begin
      SetOpcode(OPmovsx);
      AddGv; AddEb;
    end;
    $BF: begin
      SetOpcode(OPmovsx);
      AddGv; AddEw;
    end;
    //---
    $C0: begin
      DecodeSIMD([soNone]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPxadd); AddEb; AddGb; CheckLock; end;
      end;
    end;
    $C1: begin
      DecodeSIMD([soNone]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPxadd); AddEv; AddGv; CheckLock; end;
        so66:   begin SetOpcode(OPxadd); AddEw; AddGw; CheckLock; end;
      end;
    end;
    $C2: begin
      DecodeSIMD;
      case SimdOpcode of
        soNone: begin SetOpcode(OPcmp, OPSx_ps, True); AddVps; AddHps; AddWps; AddIb end;
        so66:   begin SetOpcode(OPcmp, OPSx_pd, True); AddVpd; AddHpd; AddWpd; AddIb end;
        soF2:   begin SetOpcode(OPcmp, OPSx_sd, True); AddVsd; AddHsd; AddWsd; AddIb end;
        soF3:   begin SetOpcode(OPcmp, OPSx_ss, True); AddVss; AddHss; AddWss; AddIb end;
      end;
    end;
    $C3: begin
      DecodeSIMD([soNone]);
      if SimdOpcode = soNone
      then begin
        SetOpcode(OPmovnt, OPSx_i);
        AddMy; AddGy;
      end;
    end;
    $C4: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPpinsr, OPSx_w      ); AddPq;          AddRy_Mw; AddIb end;
        so66:   begin SetOpcode(OPpinsr, OPSx_w, True); AddVdq; AddHdq; AddRy_Mw; AddIb end;
      end;
    end;
    $C5: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPpextr, OPSx_w      ); AddGd; AddNq;  AddIb end;
        so66:   begin SetOpcode(OPpextr, OPSx_w, True); AddGd; AddUdq; AddIb end;
      end;
    end;
    $C6: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPshuf, OPSx_ps, True); AddVps; AddHps; AddWps; AddIb end;
        so66:   begin SetOpcode(OPshuf, OPSx_pd, True); AddVpd; AddHpd; AddWpd; AddIb end;
      end;
    end;
    $C7: begin
      DoGroup9;
    end;
    $C8..$CF: begin
      SetOpcode(OPbswap);
      AddOpcReg(regGeneral, OperandSize, Code[CodeIdx] and $07);
    end;
    //---
    $D0: begin
      DecodeSIMD([so66, soF2]);
      case SimdOpcode of
        so66: begin SetOpcode(OPaddsub, OPSx_pd, True); AddVpd; AddHpd; AddWpd; end;
        soF2: begin SetOpcode(OPaddsub, OPSx_ps, True); AddVps; AddHps; AddWps; end;
      end;
    end;
    $D1..$D5, $D8..$DF: begin
      idx := Code[CodeIdx] and $F;
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPC_Dx[idx], OPS_Dx[idx]      ); AddPq;        AddQq; end;
        so66:   begin SetOpcode(OPC_Dx[idx], OPS_Dx[idx], True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $D6: begin
      DecodeSIMD([so66, soF2, soF3]);
      case SimdOpcode of
        so66: begin SetOpcode(OPmov, OPSx_q, True); AddWq;  AddVq; end;
        soF2: begin SetOpcode(OPmovdq2q          ); AddPq;  AddUq; end;
        soF3: begin SetOpcode(OPmovq2dq          ); AddVdq; AddNq; end;
      end;
    end;
    $D7: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPpmovmskb               ); AddGd; AddNq; end;
        so66:   begin SetOpcode(OPpmovmskb, OPSnone, True); AddGd; AddUx; end;
      end;
    end;
    // $D8..$DF: see $D1
    //---
    $E0..$E5, $E8..$EF: begin
      idx := Code[CodeIdx] and $F;
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPC_Ex[idx], OPS_Ex[idx]      ); AddPq;        AddQq; end;
        so66:   begin SetOpcode(OPC_Ex[idx], OPS_Ex[idx], True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $E6: begin
      DecodeSIMD([so66, soF2, soF3]);
      case SimdOpcode of
        so66: begin SetOpcode(OPcvttpd2, OPSx_dq, True); AddVx; AddWpd; end;
        soF2: begin SetOpcode(OPcvtpd2,  OPSx_dq, True); AddVx; AddWpd; end;
        soF3: begin SetOpcode(OPcvtdq2,  OPSx_pd, True); AddVx; AddWpd; end;
      end;
    end;
    $E7: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmovnt, OPSx_q       ); AddMq; AddPq; end;
        so66:   begin SetOpcode(OPmovnt, OPSx_dq, True); AddMx; AddVx; end;
      end;
    end;
    // $E8..$EF: see $E0
    $F0: begin
      DecodeSIMD([soF2]);
      if SimdOpcode = soF2
      then begin SetOpcode(OPlddqu, OPSnone, True); AddVx; AddMx; end;
    end;
    $F1..$F6, $F8..$FE: begin
      idx := Code[CodeIdx] and $F;
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPC_Fx[idx], OPS_Fx[idx]      ); AddPq;        AddQq; end;
        so66:   begin SetOpcode(OPC_Fx[idx], OPS_Fx[idx], True); AddVx; AddHx; AddWx; end;
      end;
    end;
    $F7: begin
      DecodeSIMD([soNone, so66]);
      case SimdOpcode of
        soNone: begin SetOpcode(OPmaskmov, OPSx_q        ); AddPq;  AddNq;  end;
        so66:   begin SetOpcode(OPmaskmov, OPSx_dqu, True); AddVdq; AddUdq; end;
      end;
    end;
    // $F8..$FE: see $F1
    $FF: SetOpcode(OPud1);
  end;

  if not HasOpcode
  then SetOpcode(OPX_Invalid);
end;

procedure TX86Disassembler.Do3ByteOpcode38;
begin
  if Code[CodeIdx] = $F3
  then begin
    DoGroup17;
    Exit;
  end;

  DecodeSIMD([], Code[CodeIdx] in [$F0, $F1]);
  case SimdOpcode of
    soNone: begin
      case Code[CodeIdx] of
        $00: begin SetOpcode(OPpshuf,       OPSx_b        ); AddPq;  AddQq;                    end;
        $01: begin SetOpcode(OPphadd,       OPSx_w        ); AddPq;  AddQq;                    end;
        $02: begin SetOpcode(OPphadd,       OPSx_d        ); AddPq;  AddQq;                    end;
        $03: begin SetOpcode(OPphaddsw                    ); AddPq;  AddQq;                    end;
        $04: begin SetOpcode(OPpmaddubsw                  ); AddPq;  AddQq;                    end;
        $05: begin SetOpcode(OPphsub,       OPSx_w        ); AddPq;  AddQq;                    end;
        $06: begin SetOpcode(OPphsub,       OPSx_d        ); AddPq;  AddQq;                    end;
        $07: begin SetOpcode(OPphsubsw                    ); AddPq;  AddQq;                    end;
        $08: begin SetOpcode(OPpsign,       OPSx_b        ); AddPq;  AddQq;                    end;
        $09: begin SetOpcode(OPpsign,       OPSx_w        ); AddPq;  AddQq;                    end;
        $0A: begin SetOpcode(OPpsign,       OPSx_d        ); AddPq;  AddQq;                    end;
        $0B: begin SetOpcode(OPpmulhrsw                   ); AddPq;  AddQq;                    end;
        $1C: begin SetOpcode(OPpabs,        OPSx_b        ); AddPq;  AddQq;                    end;
        $1D: begin SetOpcode(OPpabs,        OPSx_w        ); AddPq;  AddQq;                    end;
        $1E: begin SetOpcode(OPpabs,        OPSx_d        ); AddPq;  AddQq;                    end;
        $C8: begin SetOpcode(OPsha1nexte                  ); AddVdq; AddWdq;                   end;
        $C9: begin SetOpcode(OPsha1msg1                   ); AddVdq; AddWdq;                   end;
        $CA: begin SetOpcode(OPsha1msg2                   ); AddVdq; AddWdq;                   end;
        $CB: begin SetOpcode(OPsha256rnds2                ); AddVdq; AddWdq;                   end;
        $CC: begin SetOpcode(OPsha256msg1                 ); AddVdq; AddWdq;                   end;
        $CD: begin SetOpcode(OPsha256msg2                 ); AddVdq; AddWdq;                   end;
        $F0: begin SetOpcode(OPmov,         OPSc_be       ); AddGy;  AddMy;                    end;
        $F1: begin SetOpcode(OPmov,         OPSc_be       ); AddMy;  AddGy;                    end;
        $F2: begin SetOpcode(OPandn                       ); AddGy;  AddBy;  AddEy;  CheckVex; end;
        $F5: begin SetOpcode(OPbzhi                       ); AddGy;  AddEy;  AddBy;  CheckVex; end;
        $F7: begin SetOpcode(OPbextr                      ); AddGy;  AddEy;  AddBy;  CheckVex; end;
      end;
    end;
    so66: begin
      case Code[CodeIdx] of
        $00: begin SetOpcode(OPpshuf,       OPSx_b,   True); AddVx;  AddHx;  AddWx;            end;
        $01: begin SetOpcode(OPphadd,       OPSx_w,   True); AddVx;  AddHx;  AddWx;            end;
        $02: begin SetOpcode(OPphadd,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $03: begin SetOpcode(OPphaddsw,     OPSnone,  True); AddVx;  AddHx;  AddWx;            end;
        $04: begin SetOpcode(OPpmaddubsw,   OPSnone,  True); AddVx;  AddHx;  AddWx;            end;
        $05: begin SetOpcode(OPphsub,       OPSx_w,   True); AddVx;  AddHx;  AddWx;            end;
        $06: begin SetOpcode(OPphsub,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $07: begin SetOpcode(OPphsubsw,     OPSnone,  True); AddVx;  AddHx;  AddWx;            end;
        $08: begin SetOpcode(OPpsign,       OPSx_b,   True); AddVx;  AddHx;  AddWx;            end;
        $09: begin SetOpcode(OPpsign,       OPSx_w,   True); AddVx;  AddHx;  AddWx;            end;
        $0A: begin SetOpcode(OPpsign,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $0B: begin SetOpcode(OPpmulhrsw,    OPSnone,  True); AddVx;  AddHx;  AddWx;            end;
        $0C: begin SetOpcode(OPvpermil,     OPSx_ps       ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $0D: begin SetOpcode(OPvpermil,     OPSx_pd       ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $0E: begin SetOpcode(OPvtest,       OPSx_ps       ); AddVx;          AddWx;  CheckVex; end;
        $0F: begin SetOpcode(OPvtest,       OPSx_pd       ); AddVx;          AddWx;  CheckVex; end;
        $10: begin SetOpcode(OPpblendvb                   ); AddVdq; AddWdq;                   end;
        $13: begin SetOpcode(OPvcvtph2ps                  ); AddVx;  AddWx;  AddIb;  CheckVex; end;
        $14: begin SetOpcode(OPblendv,      OPSx_ps       ); AddVdq; AddWdq; AddReg(regXmm, os128, 0); end;
        $15: begin SetOpcode(OPblendv,      OPSx_pd       ); AddVdq; AddWdq; AddReg(regXmm, os128, 0); end;
        $16: begin SetOpcode(OPvperm,       OPSx_ps       ); AddVqq; AddHqq; AddWqq; CheckVex; end;
        $17: begin SetOpcode(OPptest,       OPSnone,  True); AddVx;  AddWx;                    end;
        $18: begin SetOpcode(OPvbroadcast,  OPSx_ss       ); AddVx;  AddWd;          CheckVex; end;
        $19: begin SetOpcode(OPvbroadcast,  OPSx_sd       ); AddVqq; AddWq;          CheckVex; end;
        $1A: begin SetOpcode(OPvbroadcast,  OPSx_f128     ); AddVqq; AddMdq;         CheckVex; end;
        $1C: begin SetOpcode(OPpabs,        OPSx_b,   True); AddVx;  AddWx;                    end;
        $1D: begin SetOpcode(OPpabs,        OPSx_w,   True); AddVx;  AddWx;                    end;
        $1E: begin SetOpcode(OPpabs,        OPSx_d,   True); AddVx;  AddWx;                    end;
        $20: begin SetOpcode(OPpmovsx,      OPSv_bw,  True); AddVx;  AddUx_Mq;                 end;
        $21: begin SetOpcode(OPpmovsx,      OPSv_bd,  True); AddVx;  AddUx_Md;                 end;
        $22: begin SetOpcode(OPpmovsx,      OPSv_bq,  True); AddVx;  AddUx_Mw;                 end;
        $23: begin SetOpcode(OPpmovsx,      OPSv_wd,  True); AddVx;  AddUx_Mq;                 end;
        $24: begin SetOpcode(OPpmovsx,      OPSv_wq,  True); AddVx;  AddUx_Md;                 end;
        $25: begin SetOpcode(OPpmovsx,      OPSv_dq,  True); AddVx;  AddUx_Mq;                 end;
        $28: begin SetOpcode(OPpmuldq,      OPSnone,  True); AddVx;  AddHx;  AddWx;            end;
        $29: begin SetOpcode(OPpcmpeq,      OPSx_q,   True); AddVx;  AddHx;  AddWx;            end;
        $2A: begin SetOpcode(OPmovnt,       OPSx_dqa, True); AddVx;  AddMx;                    end;
        $2B: begin SetOpcode(OPpackusdw,    OPSnone,  True); AddVx;  AddHx;  AddWx;            end;
        $2C: begin SetOpcode(OPmaskmov,     OPSx_ps,  True); AddVx;  AddHx;  AddMx;  CheckVex; end;
        $2D: begin SetOpcode(OPmaskmov,     OPSx_pd,  True); AddVx;  AddHx;  AddMx;  CheckVex; end;
        $2E: begin SetOpcode(OPmaskmov,     OPSx_ps,  True); AddMx;  AddHx;  AddVx;  CheckVex; end;
        $2F: begin SetOpcode(OPmaskmov,     OPSx_pd,  True); AddMx;  AddHx;  AddVx;  CheckVex; end;
        $30: begin SetOpcode(OPpmovzx,      OPSv_bw,  True); AddVx;  AddUx_Mq;                 end;
        $31: begin SetOpcode(OPpmovzx,      OPSv_bd,  True); AddVx;  AddUx_Md;                 end;
        $32: begin SetOpcode(OPpmovzx,      OPSv_bq,  True); AddVx;  AddUx_Mw;                 end;
        $33: begin SetOpcode(OPpmovzx,      OPSv_wd,  True); AddVx;  AddUx_Mq;                 end;
        $34: begin SetOpcode(OPpmovzx,      OPSv_wq,  True); AddVx;  AddUx_Md;                 end;
        $35: begin SetOpcode(OPpmovzx,      OPSv_dq,  True); AddVx;  AddUx_Mq;                 end;
        $36: begin SetOpcode(OPvperm,       OPSx_d        ); AddVqq; AddHqq; AddWqq; CheckVex; end;
        $37: begin SetOpcode(OPpcmpgt,      OPSx_q,   True); AddVx;  AddHx;  AddWx;            end;
        $38: begin SetOpcode(OPpmins,       OPSx_b,   True); AddVx;  AddHx;  AddWx;            end;
        $39: begin SetOpcode(OPpmins,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $3A: begin SetOpcode(OPpminu,       OPSx_w,   True); AddVx;  AddHx;  AddWx;            end;
        $3B: begin SetOpcode(OPpminu,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $3C: begin SetOpcode(OPpmaxs,       OPSx_b,   True); AddVx;  AddHx;  AddWx;            end;
        $3D: begin SetOpcode(OPpmaxs,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $3E: begin SetOpcode(OPpmaxu,       OPSx_w,   True); AddVx;  AddHx;  AddWx;            end;
        $3F: begin SetOpcode(OPpmaxu,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $40: begin SetOpcode(OPpmull,       OPSx_d,   True); AddVx;  AddHx;  AddWx;            end;
        $41: begin SetOpcode(OPphminposuw,  OPSnone,  True); AddVdq; AddWdq;                   end;
        $45: begin SetOpcode(OPvpsrlv,      OPS_d_q       ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $46: begin SetOpcode(OPvpsrav,      OPSx_d        ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $47: begin SetOpcode(OPvpsllv,      OPS_d_q       ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $58: begin SetOpcode(OPvpbroadcast, OPSx_d        ); AddVx;  AddWx;          CheckVex; end;
        $59: begin SetOpcode(OPvpbroadcast, OPSx_q        ); AddVx;  AddWx;          CheckVex; end;
        $5A: begin SetOpcode(OPvpbroadcast, OPSx_i128     ); AddVqq; AddMdq;         CheckVex; end;
        $78: begin SetOpcode(OPvpbroadcast, OPSx_b        ); AddVx;  AddWx;          CheckVex; end;
        $79: begin SetOpcode(OPvpbroadcast, OPSx_w        ); AddVx;  AddWx;          CheckVex; end;
        $80: begin SetOpcode(OPinvept                     ); AddGy;  AddMdq;                   end;
        $81: begin SetOpcode(OPinvvpid                    ); AddGy;  AddMdq;                   end;
        $82: begin SetOpcode(OPinvpcid                    ); AddGy;  AddMdq;                   end;
        $8C: begin SetOpcode(OPvpmaskmov,   OPS_d_q       ); AddVx;  AddHx;  AddMx;  CheckVex; end;
        $8E: begin SetOpcode(OPvpmaskmov,   OPS_d_q       ); AddMx;  AddHx;  AddVx;  CheckVex; end;
        $90: begin SetOpcode(OPvgatherd,    OPS_d_q       ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $91: begin SetOpcode(OPvgatherq,    OPS_d_q       ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $92: begin SetOpcode(OPvgatherd,    OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $93: begin SetOpcode(OPvgatherq,    OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $96: begin SetOpcode(OPvfmaddsub132,OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $97: begin SetOpcode(OPvfmsubadd132,OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $98: begin SetOpcode(OPvfmadd132,   OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $99: begin SetOpcode(OPvfmadd132,   OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $9A: begin SetOpcode(OPvfmsub132,   OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $9B: begin SetOpcode(OPvfmsub132,   OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $9C: begin SetOpcode(OPvfnmadd132,  OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $9D: begin SetOpcode(OPvfnmadd132,  OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $9E: begin SetOpcode(OPvfnmsub132,  OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $9F: begin SetOpcode(OPvfnmsub132,  OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $A6: begin SetOpcode(OPvfmaddsub213,OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $A7: begin SetOpcode(OPvfmsubadd213,OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $A8: begin SetOpcode(OPvfmadd213,   OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $A9: begin SetOpcode(OPvfmadd213,   OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $AA: begin SetOpcode(OPvfmsub213,   OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $AB: begin SetOpcode(OPvfmsub213,   OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $AC: begin SetOpcode(OPvfnmadd213,  OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $AD: begin SetOpcode(OPvfnmadd213,  OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $AE: begin SetOpcode(OPvfnmsub213,  OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $AF: begin SetOpcode(OPvfnmsub213,  OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $B6: begin SetOpcode(OPvfmaddsub231,OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $B7: begin SetOpcode(OPvfmsubadd231,OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $B8: begin SetOpcode(OPvfmadd231,   OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $B9: begin SetOpcode(OPvfmadd231,   OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $BA: begin SetOpcode(OPvfmsub231,   OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $BB: begin SetOpcode(OPvfmsub231,   OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $BC: begin SetOpcode(OPvfnmadd231,  OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $BD: begin SetOpcode(OPvfnmadd231,  OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $BE: begin SetOpcode(OPvfnmsub231,  OPS_ps_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $BF: begin SetOpcode(OPvfnmsub231,  OPS_ss_d      ); AddVx;  AddHx;  AddWx;  CheckVex; end;
        $DB: begin SetOpcode(OPaesimc,      OPSnone,  True); AddVdq; AddWdq;                   end;
        $DC: begin SetOpcode(OPaesenc,      OPSnone,  True); AddVdq; AddHdq; AddWdq;           end;
        $DD: begin SetOpcode(OPaesenclast,  OPSnone,  True); AddVdq; AddHdq; AddWdq;           end;
        $DE: begin SetOpcode(OPaesdec,      OPSnone,  True); AddVdq; AddHdq; AddWdq;           end;
        $DF: begin SetOpcode(OPaesdeclast,  OPSnone,  True); AddVdq; AddHdq; AddWdq;           end;
        $F0: begin SetOpcode(OPmov,         OPSc_be       ); AddGw;  AddMw;                    end;
        $F1: begin SetOpcode(OPmov,         OPSc_be       ); AddMw;  AddGw;                    end;
        $F6: begin SetOpcode(OPadcx                       ); AddGy;  AddEy;                    end;
        $F7: begin SetOpcode(OPshl,         OPSx_x        ); AddGy;  AddEy;  AddBy;  CheckVex; end;
      end;
    end;
    soF2: begin
      case Code[CodeIdx] of
        $F0: begin SetOpcode(OPcrc32                      ); AddGd;  AddEb;                    end;
        $F1: begin
          SetOpcode(OPcrc32);
          AddGd;
          if pre66 in Flags then AddEw else AddEy;
          Flags := Flags - [pre66];
        end;
        $F5: begin SetOpcode(OPpdep                       ); AddGy;  AddBy;  AddEy;  CheckVex; end;
        $F6: begin SetOpcode(OPmul,         OPSx_x        ); AddBy;  AddGy;  AddReg(regGeneral, MODE_SIZE[ProcessMode], REG_D); AddEy; CheckVex; end;
        $F7: begin SetOpcode(OPshr,         OPSx_x        ); AddGy;  AddEy;  AddBy;  CheckVex; end;
      end;
    end;
    soF3: begin
      case Code[CodeIdx] of
        $F5: begin SetOpcode(OPpext                       ); AddGy;  AddBy;  AddEy;  CheckVex; end;
        $F6: begin SetOpcode(OPadox                       ); AddGy;  AddEy;                    end;
        $F7: begin SetOpcode(OPsar,         OPSx_x        ); AddGy;  AddEy;  AddBy;  CheckVex; end;
      end;
    end;
  end;
  if HasOpcode
  then ClearSIMDPrefix;
end;

procedure TX86Disassembler.Do3ByteOpcode3A;
begin
  DecodeSIMD([], Code[CodeIdx] in [$F0, $F1]);
  case SimdOpcode of
    soNone: begin
      case Code[CodeIdx] of
        $0F: begin SetOpcode(OPpalignr,     OPSnone       ); AddPq;    AddQq;  AddIb;                    end;
        $CC: begin SetOpcode(OPsha1rnds4,   OPSnone       ); AddVdq;   AddWdq; AddIb;                    end;
      end;
    end;
    so66: begin
      case Code[CodeIdx] of
        $00: begin SetOpcode(OPvperm,       OPSx_q        ); AddVqq;   AddWqq; AddIb;            CheckVex; end;
        $01: begin SetOpcode(OPvperm,       OPSx_pd       ); AddVqq;   AddWqq; AddIb;            CheckVex; end;
        $02: begin SetOpcode(OPpblend,      OPSx_d,   True); AddVx;    AddHx;  AddWx;     AddIb; CheckVex; end;
        $03: begin SetOpcode(OPvalign,      OPS_d_q       ); AddVqq;   AddWqq; AddIb;            CheckVex; end;
        $04: begin SetOpcode(OPvpermil,     OPSx_ps       ); AddVx;    AddWx;  AddIb;            CheckVex; end;
        $05: begin SetOpcode(OPvpermil,     OPSx_pd       ); AddVx;    AddWx;  AddIb;            CheckVex; end;
        $06: begin SetOpcode(OPvperm2,      OPSx_f128     ); AddVqq;   AddHqq; AddWqq;    AddIb; CheckVex; end;
        $08: begin SetOpcode(OPround,       OPSx_ps,  True); AddVx;    AddWx;  AddIb;                      end;
        $09: begin SetOpcode(OPround,       OPSx_pd,  True); AddVx;    AddWx;  AddIb;                      end;
        $0A: begin SetOpcode(OPround,       OPSx_ss,  True); AddVss;   AddHx;  AddWss;    AddIb;           end;
        $0B: begin SetOpcode(OPround,       OPSx_sd,  True); AddVsd;   AddHx;  AddWsd;    AddIb;           end;
        $0C: begin SetOpcode(OPblend,       OPSx_ps,  True); AddVx;    AddHx;  AddWx;     AddIb;           end;
        $0D: begin SetOpcode(OPblend,       OPSx_pd,  True); AddVx;    AddHx;  AddWx;     AddIb;           end;
        $0E: begin SetOpcode(OPpblend,      OPSx_w,   True); AddVx;    AddHx;  AddWx;     AddIb;           end;
        $0F: begin SetOpcode(OPpalignr,     OPSnone,  True); AddVx;    AddHx;  AddWx;     AddIb;           end;
        $14: begin SetOpcode(OPpextr,       OPSx_b,   True); AddRd_Mb; AddVqq; AddIb;                      end;
        $15: begin SetOpcode(OPpextr,       OPSx_w,   True); AddRd_Mb; AddVqq; AddIb;                      end;
        $16: begin SetOpcode(OPpextr,       OPS_d_q,  True); AddEy;    AddVdq; AddIb;                      end;
        $17: begin SetOpcode(OPextract,     OPSx_ps,  True); AddEd;    AddVdq; AddIb;                      end;
        $18: begin SetOpcode(OPinsert,      OPSx_f128,True); AddVqq;   AddHqq; AddWqq;    AddIb; CheckVex; end;
        $19: begin SetOpcode(OPextract,     OPSx_f128,True); AddWdq;   AddVqq; AddIb;            CheckVex; end;
        $1D: begin SetOpcode(OPcvtps2,      OPSx_ph,  True); AddWx;    AddVx;  AddIb;            CheckVex; end;
        $20: begin SetOpcode(OPpinsr,       OPSx_b,   True); AddVdq;   AddHdq; AddRy_Mb;  AddIb;           end;
        $21: begin SetOpcode(OPinsert,      OPSx_ps,  True); AddVdq;   AddHdq; AddUdq_Md; AddIb;           end;
        $22: begin SetOpcode(OPpinsr,       OPS_d_q,  True); AddVdq;   AddHdq; AddEy;     AddIb;           end;
        $38: begin SetOpcode(OPinsert,      OPSx_i128,True); AddVqq;   AddHqq; AddWqq;    AddIb; CheckVex; end;
        $39: begin SetOpcode(OPextract,     OPSx_i128,True); AddWdq;   AddVqq; AddIb;            CheckVex; end;
        $40: begin SetOpcode(OPdp,          OPSx_ps,  True); AddVx;    AddHx;  AddWx;     AddIb;           end;
        $41: begin SetOpcode(OPdp,          OPSx_pd,  True); AddVdq;   AddHdq; AddWdq;    AddIb;           end;
        $42: begin SetOpcode(OPmpsadbw,     OPSnone,  True); AddVx;    AddHx;  AddWx;     AddIb;           end;
        $44: begin SetOpcode(OPpclmulqdq,   OPSnone,  True); AddVdq;   AddHdq; AddWdq;    AddIb;           end;
        $46: begin SetOpcode(OPvperm2,      OPSx_i128     ); AddVqq;   AddHqq; AddWqq;    AddIb; CheckVex; end;
        $4A: begin SetOpcode(OPblendv,      OPSx_ps,  True); AddVx;    AddHx;  AddWx;     AddLx; CheckVex; end;
        $4B: begin SetOpcode(OPblendv,      OPSx_pd,  True); AddVx;    AddHx;  AddWx;     AddLx; CheckVex; end;
        $4C: begin SetOpcode(OPpblendvb,    OPSnone,  True); AddVx;    AddHx;  AddWx;     AddLx; CheckVex; end;
        $60: begin SetOpcode(OPpcmpestrm,   OPSnone,  True); AddVdq;   AddWdq; AddIb;                      end;
        $61: begin SetOpcode(OPpcmpestri,   OPSnone,  True); AddVdq;   AddWdq; AddIb;                      end;
        $62: begin SetOpcode(OPpcmpistrm,   OPSnone,  True); AddVdq;   AddWdq; AddIb;                      end;
        $63: begin SetOpcode(OPpcmpistri,   OPSnone,  True); AddVdq;   AddWdq; AddIb;                      end;
        $DF: begin SetOpcode(OPaeskeygenassist,OPSnone,True);AddVdq;   AddWdq; AddIb;                      end;
      end;
    end;
    soF2: begin
      case Code[CodeIdx] of
        $F0: begin SetOpcode(OProrx                       ); AddGy;    AddEy;  AddIb;          CheckVex; end;
      end;
    end;
  end;
end;

procedure TX86Disassembler.DoVex(ASize: Byte);
const
  SIMDMAP: array[0..3] of TSimdOpcode = (soNone, so66, soF3, soF2);
  LENMAP: array[0..3] of TOperandSize = (os128, os256, os512, os0);
var
  idx: Byte;
begin
  Assert(ASize in [2..4], Format('Invalid VEX size: %u', [ASize]));

  // remove rexRXBW flags, they are illegal. the presence of flagRex wil signal this
  Flags := Flags - [rexR, rexX, rexB, rexW];

  idx := CodeIdx + 1;
  case ASize of
    2: begin
      // VEX 2
      // Rvvv vLpp
      if Code[idx] and $80 = 0 then Include(Flags, rexR);
      Vex.Index := ((Code[idx] shr 3) and $0F) xor $0F;
      Vex.VectorLength := LENMAP[(Code[idx] shr 2) and $01];
      SimdOpcode := SIMDMAP[Code[idx] and $03];
      mm := 1;
    end;
    3: begin
      // VEX 3
      // RXBm mmmm | Wvvv vLpp
      if Code[idx] and $80 = 0 then Include(Flags, rexR);
      if Code[idx] and $40 = 0 then Include(Flags, rexX);
      if Code[idx] and $20 = 0 then Include(Flags, rexB);
      mm := Code[idx] and $1F;

      Inc(idx);
      if Code[idx] and $80 <> 0 then Include(Flags, rexW);
      Vex.Index := ((Code[idx] shr 3) and $0F) xor $0F;
      Vex.VectorLength := LENMAP[(Code[idx] shr 2) and $01];
      SimdOpcode := SIMDMAP[Code[idx] and $03];
    end;
    4: begin
      // EVEX
      // RXBR' 00mm | Wvvv v1pp | zL'Lb V'aaa
     //  the RXB fields are mapped to the rex RXB flags
      if Code[idx] and $80 = 0 then Include(Flags, rexR);
      // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
      // Table 2-30. EVEX Prefix Bit Field Functional Grouping seems to indicate
      // that rexX is inverted and evexX not, but they appear to be the same
      if Code[idx] and $40 = 0 then Flags := Flags + [rexX, evexX];
      if Code[idx] and $20 = 0 then Include(Flags, rexB);
      if Code[idx] and $10 = 0 then Include(Flags, evexR);
      mm := Code[idx] and $03;

      Inc(idx);
      if Code[idx] and $80 <> 0 then Include(Flags, rexW);
      Vex.Index := ((Code[idx] shr 3) and $0F) xor $0F;
      SimdOpcode := SIMDMAP[Code[idx] and $03];

      Inc(idx);
      if Code[idx] and $80 <> 0 then Include(Flags, evexZ);
      Vex.VectorLength := LENMAP[(Code[idx] shr 5) and $03];
      if Code[idx] and $10 <> 0 then Include(Flags, evexB);
      if Code[idx] and $08 = 0 then Include(Flags, evexV);
      Vex.MaskIndex := Code[idx] and $07;

      Include(Flags, flagEvex);
    end;
  else
    Exit;
  end;

  Include(Flags, flagVex);
  Inc(CodeIdx, ASize);
  Inc(ModRMIdx, ASize);

  case mm of
    1: Do2ByteOpcode;
    2: Do3ByteOpcode38;
    3: Do3ByteOpcode3A;
  else
    SetOpcode(OPX_InvalidVex);
  end;
end;

procedure TX86Disassembler.DoDisassemble;
begin
  SetOpcode(OPX_InternalUnknown);
  repeat
    ModRMIdx := CodeIdx + 1;
    case Code[CodeIdx] of
      $00..$05: begin
        SetOpcode(OPadd);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $06: begin
        SetOpcode(OPpush); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_ES);
      end;
      $07: begin
        SetOpcode(OPpop); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_ES);
      end;
      $08..$0D: begin
        SetOpcode(OPor);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $0E: begin
        SetOpcode(OPpush); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_CS);
      end;
      $0F: begin
        Inc(CodeIdx);
        Inc(ModRMIdx);
        Do2ByteOpcode;
      end;
      //---
      $10..$15: begin
        SetOpcode(OPadc);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $16: begin
        SetOpcode(OPpush); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_SS);
      end;
      $17: begin
        SetOpcode(OPpop); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_SS);
      end;
      $18..$1D: begin
        SetOpcode(OPsbb);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $1E: begin
        SetOpcode(OPpush); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_DS);
      end;
      $1F: begin
        SetOpcode(OPpop); Check32;
        AddReg(regSegment, MODE_SIZE[ProcessMode], REG_DS);
      end;
      //---
      $20..$25: begin
        SetOpcode(OPand);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $26: begin
        Instruction^.SegmentReg := Ignore64Reg(REG_ES);
      end;
      $27: begin
        SetOpcode(OPdaa); Check32;
      end;
      $28..$2D: begin
        SetOpcode(OPsub);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $2E: begin
        Instruction^.SegmentReg := Ignore64Reg(REG_CS);
      end;
      $2F: begin
        SetOpcode(OPdas); Check32;
      end;
      //---
      $30..$35: begin
        SetOpcode(OPxor);
        AddStdOperands(Code[CodeIdx]);
        CheckLock;
      end;
      $36: begin
        Instruction^.SegmentReg := Ignore64Reg(REG_SS);
      end;
      $37: begin
        SetOpcode(OPaaa); Check32;
      end;
      $38..$3D: begin
        SetOpcode(OPcmp);
        AddStdOperands(Code[CodeIdx]);
      end;
      $3E: begin
        Instruction^.SegmentReg := Ignore64Reg(REG_DS);
      end;
      $3F: begin
        SetOpcode(OPaas); Check32;
      end;
      //---
      $40..$4F: begin
        if (ProcessMode = dm64)
        then begin
          if (Code[CodeIdx] and 1) <> 0 then Include(Flags, rexB);
          if (Code[CodeIdx] and 2) <> 0 then Include(Flags, rexX);
          if (Code[CodeIdx] and 4) <> 0 then Include(Flags, rexR);
          if (Code[CodeIdx] and 8) <> 0 then Include(Flags, rexW);
          Include(Flags, flagRex);
        end
        else begin
          if Code[CodeIdx] <= $47
          then SetOpcode(OPinc)
          else SetOpcode(OPdec);
          AddOpcReg(regGeneral, OperandSize, Code[CodeIdx] and $07);
          CheckLock;
        end;
      end;
      //---
      $50..$57: begin
        Default64;
        SetOpcode(OPpush);
        AddOpcReg(regGeneral, OperandSize, Code[CodeIdx] and $07);
      end;
      $58..$5F: begin
        Default64;
        SetOpcode(OPpop);
        AddOpcReg(regGeneral, OperandSize, Code[CodeIdx] and $07);
      end;
      //---
      $60: begin
        if OperandSize = os16
        then SetOpcode(OPpusha)
        else SetOpcode(OPpusha, OPSx_d);
        Check32;
      end;
      $61: begin
        if OperandSize = os16
        then SetOpcode(OPpopa)
        else SetOpcode(OPpopa, OPSx_d);
        Check32;
      end;
      $62: begin
        DecodeModRM;
        if (ProcessMode = dm32) and (ModRm.Mode <> 3)
        then begin
          SetOpcode(OPbound);
          AddGv; AddMa;
        end
        else begin
          DoVex(4);
        end;
      end;
      $63: begin
        if ProcessMode = dm64
        then begin
          SetOpcode(OPmovsx, OPSx_d);
          AddGv; AddEd;
        end
        else begin
          SetOpcode(OParpl);
          AddEw; AddGw;
        end;
      end;
      $64: begin
        Instruction^.SegmentReg := REG_FS;
      end;
      $65: begin
        Instruction^.SegmentReg := REG_GS;
      end;
      $66: begin
        Include(FLags, pre66);
      end;
      $67: begin
        Include(FLags, preAdr);
      end;
      $68: begin
        Default64;
        SetOpcode(OPpush);
        AddIz;
      end;
      $69: begin
        SetOpcode(OPimul);
        AddGv; AddEv; AddIz;
      end;
      $6A: begin
        Default64;
        SetOpcode(OPpush);
        AddIb;
      end;
      $6B: begin
        SetOpcode(OPimul);
        AddGv; AddEv; AddIb;
      end;
      $6C: begin
        SetOpcode(OPins, OPSx_b); CheckRepeat;
      end;
      $6D: begin
        if OperandSize = os16
        then SetOpcode(OPins, OPSx_w)
        else SetOpcode(OPins, OPSx_d);
        CheckRepeat;
      end;
      $6E: begin
        SetOpcode(OPouts, OPSx_b); CheckRepeat;
      end;
      $6F: begin
        if OperandSize = os16
        then SetOpcode(OPouts, OPSx_w)
        else SetOpcode(OPouts, OPSx_d);
        CheckRepeat;
      end;
      $70..$7F: begin
        Force64;
        SetOpcode(OPj__, StdCond(Code[CodeIdx]));
        AddJb;
      end;
      //---
      $80..$83: begin
        DoGroup1;
      end;
      $84: begin
        SetOpcode(OPtest);
        AddEb; AddGb;
      end;
      $85: begin
        SetOpcode(OPtest);
        AddEv; AddGv;
      end;
      $86: begin
        SetOpcode(OPxchg);
        AddEb; AddGb;
        CheckLock;
      end;
      $87: begin
        SetOpcode(OPxchg);
        AddEv; AddGv;
        CheckLock;
      end;
      $88..$8B: begin
        SetOpcode(OPmov);
        AddStdOperands(Code[CodeIdx]);
      end;
      $8C: begin
        SetOpcode(OPmov);
        AddEv; AddSw;
      end;
      $8D: begin
        SetOpcode(OPlea);
        AddGv; AddM;
      end;
      $8E: begin
        SetOpcode(OPmov);
        AddSw; AddEw;
      end;
      $8F: begin
        DoGroup1;
      end;
      //---
      $90: begin
        if preF3 in Flags
        then SetOpcode(OPpause)
        else if rexB in Flags
        then begin
          SetOpcode(OPxchg);
          AddReg(regGeneral, OperandSize, 8);
          AddReg(regGeneral, OperandSize, REG_A);
        end
        else SetOpcode(OPnop);
      end;
      $91..$97: begin
        SetOpcode(OPxchg);
        AddOpcReg(regGeneral, OperandSize, Code[CodeIdx] and $07);
        AddReg(regGeneral, OperandSize, REG_A);
      end;
      $98: begin
        case OperandSize of
          os64: SetOpcode(OPcdqe);
          os32: SetOpcode(OPcwde);
        else
          SetOpcode(OPcbw);
        end;
      end;
      $99: begin
        case OperandSize of
          os64: SetOpcode(OPcqo);
          os32: SetOpcode(OPcdq);
        else
          SetOpcode(OPcwd);
        end;
      end;
      $9A: begin
        SetOpcode(OPcall); Check32;
        AddAp;
      end;
      $9B: begin
        SetOpcode(OPfwait);
      end;
      $9C: begin
        Default64;
        case OperandSize of
          os64: SetOpcode(OPpushf, OPSx_q);
          os32: SetOpcode(OPpushf, OPSx_d);
        else
          SetOpcode(OPpushf);
        end;
        AddFv;
      end;
      $9D: begin
        Default64;
        case OperandSize of
          os64: SetOpcode(OPpopf, OPSx_q);
          os32: SetOpcode(OPpopf, OPSx_d);
        else
          SetOpcode(OPpopf);
        end;
        AddFv;
      end;
      $9E: begin
        SetOpcode(OPsahf);
      end;
      $9F: begin
        SetOpcode(OPlahf);
      end;
      //---
      $A0: begin
        SetOpcode(OPmov);
        AddReg(regGeneral, os8, REG_A);
        AddOb;
      end;
      $A1: begin
        SetOpcode(OPmov);
        AddReg(regGeneral, OperandSize, REG_A);
        AddOv;
      end;
      $A2: begin
        SetOpcode(OPmov);
        AddOb;
        AddReg(regGeneral, os8, REG_A);
      end;
      $A3: begin
        SetOpcode(OPmov);
        AddOv;
        AddReg(regGeneral, OperandSize, REG_A);
      end;
      $A4: begin
        SetOpcode(OPmovs, OPSx_b); CheckRepeat;
      end;
      $A5: begin
        case OperandSize of
          os64: SetOpcode(OPmovs, OPSx_q);
          os32: SetOpcode(OPmovs, OPSx_d);
        else
          SetOpcode(OPmovs, OPSx_w);
        end;
        CheckRepeat;
      end;
      $A6: begin
        SetOpcode(OPcmps, OPSx_b); CheckRepeatX;
      end;
      $A7: begin
        case OperandSize of
          os64: SetOpcode(OPcmps, OPSx_q);
          os32: SetOpcode(OPcmps, OPSx_d);
        else
          SetOpcode(OPcmps, OPSx_w);
        end;
        CheckRepeatX;
      end;
      $A8: begin
        SetOpcode(OPtest);
        AddReg(regGeneral, os8, REG_A);
        AddIb;
      end;
      $A9: begin
        SetOpcode(OPtest);
        AddReg(regGeneral, OperandSize, REG_A);
        AddIz;
      end;
      $AA: begin
        SetOpcode(OPstos, OPSx_b); CheckRepeat;
      end;
      $AB: begin
        case OperandSize of
          os64: SetOpcode(OPstos, OPSx_q);
          os32: SetOpcode(OPstos, OPSx_d);
        else
          SetOpcode(OPstos, OPSx_w);
        end;
        CheckRepeat;;
      end;
      $AC: begin
        SetOpcode(OPlods, OPSx_b); CheckRepeat;
      end;
      $AD: begin
        case OperandSize of
          os64: SetOpcode(OPlods, OPSx_q);
          os32: SetOpcode(OPlods, OPSx_d);
        else
          SetOpcode(OPlods, OPSx_w);
        end;
        CheckRepeat;
      end;
      $AE: begin
        SetOpcode(OPscas, OPSx_b); CheckRepeatX;
      end;
      $AF: begin
        case OperandSize of
          os64: SetOpcode(OPscas, OPSx_q);
          os32: SetOpcode(OPscas, OPSx_d);
        else
          SetOpcode(OPscas, OPSx_w);
        end;
        CheckRepeatX;
      end;
      //---
      $B0..$B7: begin
        SetOpcode(OPmov);
        AddOpcReg(regGeneral, os8, Code[CodeIdx] and $07);
        AddIb;
      end;
      $B8..$BF: begin
        SetOpcode(OPmov);
        AddOpcReg(regGeneral, OperandSize, Code[CodeIdx] and $07);
        AddIv;
      end;
      //---
      $C0..$C1: begin
        DoGroup2;
      end;
      $C2: begin
        Force64;
        SetOpcode(OPret);
        AddIw;
      end;
      $C3: begin
        Force64;
        SetOpcode(OPret);
      end;
      $C4: begin
        DecodeModRM;
        if (ProcessMode = dm32) and (ModRm.Mode <> 3)
        then begin
          SetOpcode(OPles);
          AddGz; AddMp;
        end
        else begin
          DoVex(3);
        end;
      end;
      $C5: begin
        DecodeModRM;
        if (ProcessMode = dm32) and (ModRm.Mode <> 3)
        then begin
          SetOpcode(OPlds);
          AddGz; AddMp;
        end
        else begin
          DoVex(2);
        end;
      end;
      $C6..$C7: begin
        DoGroup11;
      end;
      $C8: begin
        SetOpcode(OPenter);
        AddIw; AddIb;
      end;
      $C9: begin
        Default64;
        SetOpcode(OPleave);
      end;
      $CA: begin
        SetOpcode(OPretf);
        AddIw;
      end;
      $CB: begin
        SetOpcode(OPretf);
      end;
      $CC: begin
        SetOpcode(OPint3);
      end;
      $CD: begin
        SetOpcode(OPint);
        AddIb;
      end;
      $CE: begin
        SetOpcode(OPinto); Check32;
      end;
      $CF: begin
        case OperandSize of
          os64: SetOpcode(OPiret, OPSx_q);
          os32: SetOpcode(OPiret, OPSx_d);
        else
          SetOpcode(OPiret);
        end;
      end;
      //---
      $D0..$D3: begin
        DoGroup2;
      end;
      $D4: begin
        SetOpcode(OPaam); Check32;
      end;
      $D5: begin
        SetOpcode(OPaad); Check32;
      end;
      $D6: begin
        // AMD (old)
        SetOpcode(OPsalc); Check32;
      end;
      $D7: begin
        SetOpcode(OPxlat);
      end;
      $D8..$DF: begin
        DoX87;
      end;
      //---
      $E0: begin
        Force64;
        SetOpcode(OPloop, OPSc_ne);
        AddJb;
      end;
      $E1: begin
        Force64;
        SetOpcode(OPloop, OPSc_e);
        AddJb;
      end;
      $E2: begin
        Force64;
        SetOpcode(OPloop);
        AddJb;
      end;
      $E3: begin
        case AddressSize of
          as16: begin
            Check32;
            SetOpcode(OPjcxz);
          end;
          as32: begin
            SetOpcode(OPjecxz);
          end;
          as64: begin
            Check64;
            SetOpcode(OPjrcxz);
          end;
        end;
        AddJb;
      end;
      $E4: begin
        SetOpcode(OPin);
        AddReg(regGeneral, os8, REG_A);
        AddIb;
      end;
      $E5: begin
        SetOpcode(OPin);
        AddReg(regGeneral, OperandSize, REG_A);
        AddIb;
      end;
      $E6: begin
        SetOpcode(OPout);
        AddIb;
        AddReg(regGeneral, os8, REG_A);
      end;
      $E7: begin
        SetOpcode(OPout);
        AddIb;
        AddReg(regGeneral, OperandSize, REG_A);
      end;
      $E8: begin
        Force64;
        SetOpcode(OPcall);
        AddJz;
      end;
      $E9: begin
        Force64;
        SetOpcode(OPjmp);
        AddJz;
      end;
      $EA: begin
        SetOpcode(OPjmp); Check32;
        AddAp;
      end;
      $EB: begin
        Force64;
        SetOpcode(OPjmp);
        AddJb;
      end;
      $EC: begin
        SetOpcode(OPin);
        AddReg(regGeneral,  os8, REG_A);
        AddReg(regGeneral, os16, REG_D);
      end;
      $ED: begin
        SetOpcode(OPin);
        AddReg(regGeneral, OperandSize, REG_A);
        AddReg(regGeneral, os16, REG_D);
      end;
      $EE: begin
        SetOpcode(OPout);
        AddReg(regGeneral, os16, REG_D);
        AddReg(regGeneral,  os8, REG_A);
      end;
      $EF: begin
        SetOpcode(OPout);
        AddReg(regGeneral, os16, REG_D);
        AddReg(regGeneral, OperandSize, REG_A);
      end;
      $F0: begin
        Include(Flags, preLock);
      end;
      $F1: begin
        SetOpcode(OPint1);
      end;
      $F2: begin
        Include(Flags, preF2);
      end;
      $F3: begin
        Include(Flags, preF3);
      end;
      $F4: begin
        SetOpcode(OPhlt);
      end;
      $F5: begin
        SetOpcode(OPcmc);
      end;
      $F6..$F7: begin
        DoGroup3;
      end;
      $F8: begin
        SetOpcode(OPclc);
      end;
      $F9: begin
        SetOpcode(OPstc);
      end;
      $FA: begin
        SetOpcode(OPcli);
      end;
      $FB: begin
        SetOpcode(OPsti);
      end;
      $FC: begin
        SetOpcode(OPcld);
      end;
      $FD: begin
        SetOpcode(OPstd);
      end;
      $FE: begin
        DoGroup4;
      end;
      $FF: begin
        DoGroup5;
      end;
    else
      SetOpcode(OPX_Invalid);
    end;

    Inc(CodeIdx);
    if CodeIdx > 16 // max instruction length
    then begin
      Writeln('Disassemble: instruction longer than 16 bytes');
      Exit;
    end;
  until Instruction^.Opcode.Opcode <> OPX_InternalUnknown;
end;

procedure TX86Disassembler.Disassemble(AMode: TFPDMode; var AAddress: Pointer; out AnInstruction: TInstruction);
var
  n: Integer;
begin
  ProcessMode := AMode;
  Code := AAddress;

  AnInstruction:=Default(TInstruction);
  AnInstruction.SegmentReg:=-1;

  Instruction := @AnInstruction;

  SetOpcode(OPX_Invalid);

  opcode := 0;
  Flags := [];
  CodeIdx := 0;
  OperIdx := 0;
  SimdOpcode := soInvalid;
  Vex.MaskIndex := 0;
  mm:=0;

  Sib.Scale:=0;
  Sib.Index:=0;
  Sib.Base :=0;

  DoDisassemble;

  Instruction^.OperCnt    := OperIdx;
  Instruction^.ParseFlags := Flags;
  Instruction^.MaskIndex  := Vex.MaskIndex;

  if flagModRM in Flags then Inc(CodeIdx);
  if flagSib in Flags then Inc(CodeIdx);

  for n := 1 to OperIdx do
  begin
    AnInstruction.Operand[n].CodeIndex := CodeIdx;
    Inc(CodeIdx, AnInstruction.Operand[n].ByteCount);
    Inc(CodeIdx, AnInstruction.Operand[n].ByteCount2);
  end;
  Inc(AAddress, CodeIdx);
end;

{ TDbgProcess }

Constructor TDbgProcess.Create(M: TFPDMode);
begin
 Mode:=M;
end;

function TDbgProcess.ReadData(AAdress:TDbgPtr;ASize:Cardinal;out AData):Boolean;
begin
 Move(AAdress^,AData,ASize);
 Result:=True;
end;

function TDbgProcess.ReadData(AAdress:TDbgPtr;ASize:Cardinal;out AData;out APartSize:Cardinal):Boolean;
begin
 Move(AAdress^,AData,ASize);
 APartSize:=ASize;
 Result:=True;
end;

{ TX86AsmInstruction }

procedure TX86AsmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
    if not FProcess.ReadData(FAddress, INSTR_CODEBIN_LEN, FCodeBin) then
      Include(FFlags, diCodeReadError);
    Include(FFlags, diCodeRead);
  end;
end;

procedure TX86AsmInstruction.Disassemble;
var
  a: PByte;
  Disassembler: TX86Disassembler;
begin
  if not (diDisAss in FFlags) then begin
    ReadCode;
    if diCodeReadError in FFlags then
      exit;
    a := @FCodeBin[0];
    Disassembler.Disassemble(FProcess.Mode, a, FInstruction);
    FInstrLen := a - @FCodeBin[0];
    Include(FFlags, diDisAss);
  end;
end;

constructor TX86AsmInstruction.Create(AProcess: TDbgProcess);
begin
 FProcess:=AProcess;
end;

procedure TX86AsmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TX86AsmInstruction.IsCallInstruction: boolean;
var
  a: PByte;
begin
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;
  a := @FCodeBin[0];

  if (FProcess.Mode = dm64) then begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$40..$4F, $64..$67]) do
      inc(a);
    if not (a^ in [$E8, $FF]) then
      exit;
  end
  else begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$26, $2E, $36, $3E, $64..$67]) do
      inc(a);
    if not (a^ in [$9A, $E8, $FF]) then
      exit;
  end;

  Disassemble;
  Result := FInstruction.OpCode.Opcode = OPcall;
end;

function TX86AsmInstruction.IsReturnInstruction: boolean;
var
  a: PByte;
begin
  ReadCode;
  if diCodeReadError in FFlags then
    exit(False);
  a := @FCodeBin[0];

  // CF: IRET
  Result := (a^ in [$C2, $C3, $CA, $CB, $CF]);
end;

function TX86AsmInstruction.IsLeaveStackFrame: boolean;
var
  a: PByte;
begin
  ReadCode;
  if diCodeReadError in FFlags then
    exit(False);
  a := @FCodeBin[0];
  // C9: leave
  Result := (a^ = $C9);
  if Result then
    exit;
  if (FProcess.Mode = dm64) then begin
    Result :=
      // 48 8D 65 00 / 5D: lea rsp,[rbp+$00] / pop ebp
      ( (a^ = $48) and (a[1] = $8D) and (a[2] = $65) and (a[3] = $00)
        and (a[4] = $5D)
      ) or
      // 48 89 ec / 5D: mov esp,ebp / pop ebp
      ( (a^ = $48) and (a[1] = $89) and (a[2] = $EC)
        and (a[3] = $5D)
      );
  end
  else begin
    Result :=
      // 8D 65 00 / 5D: lea rsp,[rbp+$00] / pop ebp
      ( (a[0] = $8D) and (a[1] = $65) and (a[2] = $00)
       and (a[3] = $5D)
      ) or
      // 89 ec / 5D: mov esp,ebp / pop ebp
      ( (a[0] = $89) and (a[1] = $EC)
       and (a[2] = $5D)
      );
  end;
end;

function TX86AsmInstruction.ModifiesStackPointer: boolean;
var
  a: PByte;
begin
  (* Enter, Leave
     mov sp, ...
     lea sp, ...
     pop / push

     BUT NOT ret
  *)
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;
  a := @FCodeBin[0];

  if (FProcess.Mode = dm64) then begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$40..$4F, $64..$67]) do
      inc(a);

    // Pop/Push
    if (a^ in [$50..$61, $68, $8F, $9C, $9d])
    then
      exit(True);
  end
  else begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$26, $2E, $36, $3E, $64..$67]) do
      inc(a);

    // Pop/Push
    if (a^ in [$06, $07, $0E, $16, $17, $1E, $1F, $50..$61, $68, $6A, $8F, $9C, $9d])
    then
      exit(True);
  end;

  // Pop/Push
  if (a^ in [$FF])
  then begin
    Disassemble;
    exit(FInstruction.OpCode.Opcode = OPpush);
  end;

  if (a^ = $0F) and (a[1] in [$A0, $A1, $A8, $A9]) then
    exit(True);

  // Enter/Leave
  if (a^ in [$C8, $C9])
  then
    exit(True);

  // Mov/Lea
  if (a^ in [$89, $8B, $8D]) and
     (  ((a[1] and $38) = $20) or ((a[1] and $03) = $04)  )  // SP is involved
  then begin
    //Disassemble;
    exit(True);  // does report some "false positives"
  end;
end;

function TX86AsmInstruction.IsJumpInstruction(IncludeConditional: Boolean;
  IncludeUncoditional: Boolean): boolean;
var
  a: PByte;
begin
  (* Excluding
     E1, E2  loop
     E3   JCXZ   Jump short if eCX register is 0
  *)
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;
  a := @FCodeBin[0];

  if IncludeConditional and (a^ in [$70..$7F]) then
    exit(True);
  if IncludeConditional and (a^ = $0F) and (a[1] in [$80..$8F]) then
    exit(True);

  if IncludeUncoditional and (a^ in [$E9..$EB]) then
    exit(True);

  if IncludeUncoditional and (a^ in [$FF]) then begin
    Disassemble;
    exit(FInstruction.OpCode.Opcode = OPjmp);
  end;

end;

function TX86AsmInstruction.InstructionLength: Integer;
begin
  Disassemble;
  if diCodeReadError in FFlags then
    exit(0);
  Result := FInstrLen;
end;

function TX86AsmInstruction.X86OpCode: TOpCode;
begin
  Disassemble;
  if diCodeReadError in FFlags then
    exit(OPX_Invalid);
  Result := FInstruction.OpCode.Opcode;
end;

function HexValue(const AValue; ASize: Byte; AFlags: THexValueFormatFlags): RawByteString;
var
  i: Int64;
  p: PByte;
begin
  Result := '';
  if ASize > 8
  then begin
    Result := 'HexValue: size to large';
    Exit;
  end;
  if ASize = 0
  then begin
    Exit;
  end;

  p := @AValue;
  if p[ASize - 1] < $80
  then Exclude(AFlags, hvfSigned);

  if hvfSigned in AFlags
  then i := -1
  else i := 0;

  Move(AValue, i, ASize);
  if hvfSigned in AFlags
  then begin
    i := not i + 1;
    Result := '-';
  end
  else begin
    if hvfPrefixPositive in AFlags
    then Result := '+';
  end;
  if hvfIncludeHexchar in AFlags
  then Result := Result + '$';

  Result := Result + HexStr(i, ASize * 2);
end;

function TRegValue.StrValue:RawByteString;
begin
 if (AType<>regNone) then
 begin
  Result:=RegName(AType, ASize, AIndex);

  if (AScale>1) then
  begin
   Result:=Result+'*'+IntToStr(AScale);
  end;
 end else
 begin
  Result:='';
 end;
end;

function TOperand.StrValue:RawByteString;
begin
 if (ByteCount2 = 0) then
 begin
  Result:=RegValue[0].StrValue;

  if (RegValue[1].AType<>regNone) then
  begin
   Result:=Result+'+'+RegValue[1].StrValue;
  end;

  if (hvfIncludeHexchar in FormatFlags) then
  begin
   Result:=Result+'%s';
  end;
 end else
 begin
  Result:='$%1:s:%0:s';
 end;
end;

function TInstruction.SegmentStr:RawByteString;
begin
 case SegmentReg of
  0..5:Result:=RegName(regSegment, os16, SegmentReg)+':';
  else
       Result:='';
 end;
end;

procedure TX86AsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: RawByteString; out ACode: RawByteString; out AnInfo: TDbgInstInfo);
const
  MEMPTR: array[TOperandSize] of RawByteString = (
    { os0    } '',
    { os8    } 'byte ptr ',
    { os16   } 'word ptr ',
    { os32   } 'dword ptr ',
    { os64   } 'qword ptr ',
    { os48   } '',
    { os80   } 'tbyte ptr ',
    { os128  } 'xmmword ptr ',
    { os256  } 'ymmword ptr ',
    { os512  } 'zmmword ptr ',
    { os4096 } ''
  );
{$ifdef debug_OperandSize}
  OSTEXT: array[TOperandSize] of RawByteString = ('os8', 'os16', 'os32', 'os64', 'os48', 'os80', 'os128');
{$endif}
var
  S, Soper: RawByteString;
  n, i: Integer;
  TargetAddrOffs: Int64;
  HasMem: Boolean;
  OpcodeName: RawByteString;
  Code: PByte;
begin
  AnInfo := default(TDbgInstInfo);
  Code := AAddress;
  Disassembler.Disassemble(FProcess.Mode, AAddress, Instr);

  Soper := '';
  HasMem := False;
  for n := 1 to Instr.OperCnt do
  begin
    if Instr.Operand[n].ByteCount = 0
    then S := Instr.Operand[n].StrValue
    else begin
      i := Instr.Operand[n].CodeIndex;
      if Instr.Operand[n].ByteCount2 = 0
      then S := Format(Instr.Operand[n].StrValue, [HexValue(Code[i], Instr.Operand[n].ByteCount, Instr.Operand[n].FormatFlags)])
      else S := Format(Instr.Operand[n].StrValue, [HexValue(Code[i], Instr.Operand[n].ByteCount, Instr.Operand[n].FormatFlags), HexValue(Code[i + Instr.Operand[n].ByteCount], Instr.Operand[n].ByteCount2, Instr.Operand[n].FormatFlags)])
    end;
    if S = '' then Continue; // 3DNow adds a dummy operand to adjust the size

    if Soper <> '' then Soper := Soper + ',';
    if ofMemory in Instr.Operand[n].Flags
    then begin
      S := Instr.SegmentStr + '[' + S + ']';
      if (Instr.OperCnt = 1)
//      or (Instr.Operand[n].Size <> os32)
      or (Instr.Operand[1].Size <> Instr.Operand[2].Size)
      then S := MEMPTR[Instr.Operand[n].Size] + S;
      HasMem := True;
    end;

    if Soper = ''
    then begin
      Soper := Soper + S;
      if Instr.MaskIndex > 0
      then Soper := Soper + Format('{k%u}', [Instr.MaskIndex]);
      if evexZ in Instr.ParseFlags
      then Soper := Soper + '{z}'
    end
    else Soper := Soper + S;
  end;
{$ifdef debug_OperandSize}
  Soper := Soper + ' | ';
  for n := 1 to OperIdx do
  begin
    Soper := Soper + ' ' + OSTEXT[Instr.Operand[n].Size];
  end;
{$endif}


  if (Instr.OpCode.Opcode in [OPcall, OPj__, OPjcxz, OPjecxz, OPjmp, OPjmpe, OPjrcxz]) and
     (Instr.OperCnt = 1) and
     (hvfSigned in Instr.Operand[1].FormatFlags) and
     (Instr.Operand[1].StrValue = '%s')  // no register
  then begin
    TargetAddrOffs := 1;
    i := Instr.Operand[1].CodeIndex;
    case Instr.Operand[1].ByteCount of
      1: TargetAddrOffs := PShortint(@Code[i])^;
      2: TargetAddrOffs := PSmallint(@Code[i])^;
      4: TargetAddrOffs := PInteger (@Code[i])^;
      8: TargetAddrOffs := PInt64   (@Code[i])^;
    end;
    if TargetAddrOffs <> 1 then begin
      AnInfo.InstrType := itJump;
      {$PUSH}{$R-}{$Q-}
      AnInfo.InstrTargetOffs := Int64(TDbgPtr(AAddress) - TDbgPtr(Code)) + TargetAddrOffs;
      {$POP}
    end;
  end;


  OpcodeName := OPCODE_PREFIX[Instr.OpCode.Prefix];
  OpcodeName := OpcodeName + OPCODE_NAME[Instr.OpCode.Opcode];
  OpcodeName := OpcodeName + OPCODE_SUFFIX[Instr.OpCode.Suffix];
  if Instr.Flags * [ifOnly32, ifOnly64, ifOnlyVex] <> [] then
    OpcodeName := '**'+OpcodeName + '**';
  if ifPrefixRep in Instr.Flags then
    OpcodeName := 'rep '+OpcodeName ;
  if ifPrefixRepE in Instr.Flags then
    OpcodeName := 'repe '+OpcodeName ;
  if ifPrefixRepNe in Instr.Flags then
    OpcodeName := 'repne '+OpcodeName ;
  if ifPrefixLock in Instr.Flags then
    OpcodeName := 'lock '+OpcodeName ;

  S := '';
  if preLock in Instr.ParseFlags then S := S + '**lock**';
  if preF3 in Instr.ParseFlags then S := S + '?rep?';
  if preF2 in Instr.ParseFlags then S := S + '?repne?';
  S := S + OpcodeName;
  if not HasMem and (Instr.SegmentReg <> -1) then S := S + ' ?' + Instr.SegmentStr + '?';
  ACode := S + ' ' + Soper;

  // memory
  S := '';
  while Code < AAddress do
  begin
    S := S + HexStr(Code^, 2);
    inc(Code);
  end;
  ACodeBytes := S;
end;

procedure TX86AsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: RawByteString; out ACode: RawByteString);
var
  AnInfo: TDbgInstInfo;
begin
  Disassemble(AAddress, ACodeBytes, ACode, AnInfo);
end;

function TX86AsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction;
begin
  FLastInstr.SetAddress(AnAddress);
  Result := FLastInstr;
end;

{ TX86AsmDecoder }

function TX86AsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := FLastErrWasMem;
end;

function TX86AsmDecoder.GetMaxInstrSize: integer;
begin
  Result := 16;
end;

function TX86AsmDecoder.GetMinInstrSize: integer;
begin
  Result := 1;
end;

function TX86AsmDecoder.GetCanReverseDisassemble: boolean;
begin
  {$IFDEF FPDEBUG_WITH_REVERSE_DISASM}
  Result := true;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TX86AsmDecoder.ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal
  ): Boolean;
begin
  Result := FProcess.ReadData(AnAddress, ALen, FCodeBin[0], ALen);
  FLastErrWasMem := not Result;
end;

constructor TX86AsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

function TX86AsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
begin
  ADataLen := MAX_CODEBIN_LEN;
  Result := False;
  if not ReadCodeAt(AnAddress, ADataLen) then
    exit;
  AData := @FCodeBin[0];

  while (ADataLen > 0) and (AData^ = $90) do begin // nop
    inc(AData);
    dec(ADataLen);
  end;
  Result := ADataLen > 0;
  if not Result then
    exit;

  AnIsOutsideFrame := False;
  if AData^ = $55 then begin // push ebp
    AnIsOutsideFrame := True;
    exit;
  end;

  if AData^ = $C3 then begin // ret
    AnIsOutsideFrame := True;
    exit;
  end;

  if AData^ in [$50..$54, $56..$57] then begin // push
    while (ADataLen > 1) and (AData^ in [$50..$57]) do begin
      inc(AData);
      dec(ADataLen);
    end;
    if AData^ = $55 then begin // push ebp
      AnIsOutsideFrame := True;
      exit;
    end;
    //48 8D A4 24 50FBFFFF         lea rsp,[rsp-$000004B0]
    //48 8D 64 24 C0               lea rsp,[rsp-$40]
    //but NOT  48 8D A4 24 B040000         lea rsp,[rsp+$000004B0]
    if (ADataLen >= 4) and (AData[0] = $48) and (AData[1] = $8D) and (AData[3] = $24) and (
         (                     (AData[2] = $64) and ((AData[4] and $80) <> 0) ) or
         ( (ADataLen >= 8) and (AData[2] = $A4) and ((AData[7] and $80) <> 0) )
       )
    then begin // mov rbp,rsp // AFTER push ebp
      AnIsOutsideFrame := True;
      exit;
    end;
  end;

  //if (ADataLen >= 2) and (AData[0] = $89) and (AData[1] = $E5) // 32 bit mov ebp, esp
  if (ADataLen >= 3) and (AData[0] = $48) and (AData[1] = $89) and (AData[2] = $E5)
  then begin // mov rbp,rsp // AFTER push ebp
    // Need 1 byte before, to check for "push ebp"
    exit;
  end;
end;

end.
