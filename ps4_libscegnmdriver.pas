unit ps4_libSceGnmDriver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bittype, ps4_videodrv;

procedure post_event_eop;

implementation

uses
 hamt,
 ps4_program,
 ps4_queue,
 ps4_libkernel,
 ps4_libSceVideoOut{, ps4_pssl};

const
 InitDefault200_stub1:array[0..11] of DWORD=(
  $c0012800,
  $80000000,
  $80000000,
  $c0001200,
  0,
  $c0055800,
  $2ec47fc0,
  $ffffffff,
  0,
  0,
  0,
  $A
 );

 InitDefault200_stub2:array[0..117] of DWORD=(
  $c0017600,
  $216,
  $ffffffff,
  $c0017600,
  $217,
  $ffffffff,
  $c0017600,
  $215,
  0,
  $c0016900,
  $2f9,
  $2d,
  $c0016900,
  $282,
  8,
  $c0016900,
  $280,
  $80008,
  $c0016900,
  $281,
  $ffff0000,
  $c0016900,
  $204,
  0,
  $c0016900,
  $206,
  $43f,
  $c0016900,
  $83,
  $ffff,
  $c0016900,
  $317,
  $10,
  $c0016900,
  $2fa,
  $3f800000,
  $c0016900,
  $2fc,
  $3f800000,
  $c0016900,
  $2fb,
  $3f800000,
  $c0016900,
  $2fd,
  $3f800000,
  $c0016900,
  $202,
  $cc0010,
  $c0016900,
  $30e,
  $ffffffff,
  $c0016900,
  $30f,
  $ffffffff,
  $c0002f00,
  1,
  $c0017600,
  7,
  $1701ff,
  $c0017600,
  $46,
  $1701fd,
  $c0017600,
  $87,
  $1701ff,
  $c0017600,
  199,
  $1701fd,
  $c0017600,
  $107,
  $17,
  $c0017600,
  $147,
  $1701fd,
  $c0017600,
  $47,
  $1c,
  $c0016900,
  $1b1,
  2,
  $c0016900,
  $101,
  0,
  $c0016900,
  $100,
  $ffffffff,
  $c0016900,
  $103,
  0,
  $c0016900,
  $284,
  0,
  $c0016900,
  $290,
  0,
  $c0016900,
  $2ae,
  0,
  $c0016900,
  $292,
  0,
  $c0016900,
  $293,
  $6020000,
  $c0016900,
  $2f8,
  0,
  $c0016900,
  $2de,
  $1e9,
  $c0036900,
  $295,
  $100,
  $100,
  4,
  $c0017900,
  $00000200,
  $E0000000
 );


function ps4__sceGnmDrawInitDefaultHardwareState200(cmdBuffer:PDWORD;numDwords:QWORD;param_3:Integer):DWORD; SysV_ABI_CDecl;
var
 _cmdBuffer:PDWORD;
 count:QWORD;
begin
 Result:=0;

 if (numDwords>$FF) then
 begin
  _cmdBuffer:=cmdBuffer;
  if (param_3<>0) then
  begin
   _cmdBuffer:=cmdBuffer+$C;
   Move(InitDefault200_stub1,cmdBuffer^,SizeOf(InitDefault200_stub1));
  end;

  Move(InitDefault200_stub2,_cmdBuffer^,SizeOf(InitDefault200_stub2));

  count:=$100-(QWORD(Int64(_cmdBuffer)+($1d8-Int64(cmdBuffer))) shr 2);  //ftw
  Result:=(Int64(_cmdBuffer) + ((count*4+$1d8)-Int64(cmdBuffer))) shr 2;

  _cmdBuffer[$76]:=(count*$10000+$3ffe0000) or $c0001000;
  _cmdBuffer[$77]:=0;
 end;

end;
                   //sce:Gnm:Draw:initializeDefaultHardwareState
function ps4_sceGnmDrawInitDefaultHardwareState200(cmdBuffer:PDWORD;numDwords:QWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=ps4__sceGnmDrawInitDefaultHardwareState200(cmdBuffer,numDwords,1);
end;

const
 InitDefault350_stub:array[0..137] of DWORD=(
   $c0012800,
   $80000000,
   $80000000,
   $c0001200,
   0,
   $c0055800,
   $2ec47fc0,
   $ffffffff,
   0,
   0,
   0,
   $0000000A,
   $c0017600,
   $216,
   $ffffffff,
   $c0017600,
   $217,
   $ffffffff,
   $c0017600,
   $215,
   0,
   $c0016900,
   $2f9,
   $2d,
   $c0016900,
   $282,
   8,
   $c0016900,
   $280,
   $80008,
   $c0016900,
   $281,
   $ffff0000,
   $c0016900,
   $204,
   0,
   $c0016900,
   $206,
   $43f,
   $c0016900,
   $83,
   $ffff,
   $c0016900,
   $317,
   $10,
   $c0016900,
   $2fa,
   $3f800000,
   $c0016900,
   $2fc,
   $3f800000,
   $c0016900,
   $2fb,
   $3f800000,
   $c0016900,
   $2fd,
   $3f800000,
   $c0016900,
   $202,
   $cc0010,
   $c0016900,
   $30e,
   $ffffffff,
   $c0016900,
   $30f,
   $ffffffff,
   $c0002f00,
   1,
   $c0017600,
   7,
   $1701ff,
   $c0017600,
   $46,
   $1701fd,
   $c0017600,
   $87,
   $1701ff,
   $c0017600,
   199,
   $1701fd,
   $c0017600,
   $107,
   $17,
   $c0017600,
   $147,
   $1701fd,
   $c0017600,
   $47,
   $1c,
   $c0016900,
   $1b1,
   2,
   $c0016900,
   $101,
   0,
   $c0016900,
   $100,
   $ffffffff,
   $c0016900,
   $103,
   0,
   $c0016900,
   $284,
   0,
   $c0016900,
   $290,
   0,
   $c0016900,
   $2ae,
   0,
   $c0016900,
   $102,
   0,
   $c0016900,
   $292,
   0,
   $c0016900,
   $293,
   $6020000,
   $c0016900,
   $2f8,
   0,
   $c0016900,
   $2de,
   $1e9,
   $c0036900,
   $295,
   $100,
   $100,
   4,
   $c0017900,
   $200,
   $E0000000,
   $C0016900,
   $000002AA,
   $000000FF,
   $c0761000,
   0);

function ps4_sceGnmDrawInitDefaultHardwareState350(cmdBuffer:PDWORD;numDwords:QWORD):DWORD; SysV_ABI_CDecl;
begin
 assert(numDwords>$100);
 Move(InitDefault350_stub,cmdBuffer^,SizeOf(InitDefault350_stub));
 Result:=$100;
end;

function ps4_sceGnmInsertPushMarker(cmdBuffer:PDWORD;numDwords:QWORD;param:PChar):Integer; SysV_ABI_CDecl;
var
 cmdSize,len,len3,len4:DWORD;
begin
 len:=StrLen(param);
 len3:=(len + $c) shr 3;
 len4:=(len + $8) shr 2;

 cmdSize:=len4+len3*2;
 Assert(cmdSize+2=numDwords);

 cmdBuffer[0]:=cmdSize*$10000 or $c0001000; //NOP
 cmdBuffer[1]:=$68750001;

 len3:=len+1;
 Move(param^,cmdBuffer[2],len3);
 FillChar(PByte(@cmdBuffer[2])[len3],numDwords*SizeOf(DWORD)-len3,0);

 Result:=0;
end;

function ps4_sceGnmInsertPopMarker(cmdBuffer:PDWORD;numDwords:QWORD):Integer; SysV_ABI_CDecl;
begin
 if (numDwords<>6) then Exit(-1);
 cmdBuffer[0]:=$c0041000; //NOP
 cmdBuffer[1]:=$68750002;
 cmdBuffer[2]:=0;
 cmdBuffer[3]:=0;
 cmdBuffer[4]:=0;
 cmdBuffer[5]:=0;
 Result:=0;
end;

// called in waitUntilSafeForRendering
function ps4_sceGnmInsertWaitFlipDone(cmdBuffer:PDWORD;numDwords:QWORD;videoOutHandle,displayBufferIndex:Integer):Integer; SysV_ABI_CDecl;
var
 addr:Pointer;
begin
 Result:=-1;
 if (numDwords<>7) then Exit;
 addr:=_VideoOutGetBufferAdr(videoOutHandle,displayBufferIndex);
 if (addr=nil) then Exit;
 cmdBuffer[0]:=$c0053c00;
 cmdBuffer[1]:=$13;
 cmdBuffer[2]:=QWORD(addr) and $fffffffc;
 cmdBuffer[3]:=(QWORD(addr) shr $20) and $ffff;
 cmdBuffer[4]:=0;
 cmdBuffer[5]:=$ffffffff;
 cmdBuffer[6]:=10;
 Result:=0;
end;

const
 kAlignmentOfShaderInBytes=256;

function getCodeAddress(PgmHi,PgmLo:DWORD):Pointer;
begin
 Result:=Pointer(((QWORD(PgmHi) shl 40) or (QWORD(PgmLo) shl 8)));
end;

procedure patchShaderGpuAddress(gpuAddress:Pointer;var PgmHi,PgmLo:DWORD);
begin
 Assert(gpuAddress<>nil,'gpuAddress must not be NULL.');
 Assert(QWORD(gpuAddress) and (kAlignmentOfShaderInBytes-1)=0,'Shader''s gpu address (0x%p) needs to be 256 bytes aligned');

 PgmLo:=DWORD(QWORD(gpuAddress) shr  8);
 PgmHi:=DWORD(QWORD(gpuAddress) shr 40);
end;

type
 PCsStageRegisters=^CsStageRegisters;
 CsStageRegisters=packed record
  m_computePgmLo,      //0< A pointer to shader program (bits 39:8)
  m_computePgmHi,      //1< A pointer to shader program (bits 47:40). This must be set to zero.
  m_computePgmRsrc1,   //2
  m_computePgmRsrc2,   //3
  m_computeNumThreadX, //4< The number of threads per thread group in the X dimension as defined in the shader source.
  m_computeNumThreadY, //5< The number of threads per thread group in the Y dimension as defined in the shader source.
  m_computeNumThreadZ: //6< The number of threads per thread group in the Z dimension as defined in the shader source.
   DWORD;
 end;

function ps4_sceGnmSetCsShader(cmdBuffer:PDWORD;numDwords:QWORD;csRegs:PCsStageRegisters):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<25) or (csRegs=nil) then Exit;
 cmdBuffer[0]:=$c0027602;
 cmdBuffer[1]:=$20c;
 cmdBuffer[2]:=csRegs^.m_computePgmLo;
 cmdBuffer[3]:=csRegs^.m_computePgmHi;
 cmdBuffer[4]:=$c0027602;
 cmdBuffer[5]:=$212;
 cmdBuffer[6]:=csRegs^.m_computePgmRsrc1;
 cmdBuffer[7]:=csRegs^.m_computePgmRsrc2;
 cmdBuffer[8]:=$c0037602;
 cmdBuffer[9]:=$207;
 cmdBuffer[10]:=csRegs^.m_computeNumThreadX;
 cmdBuffer[$b]:=csRegs^.m_computeNumThreadY;
 cmdBuffer[$c]:=csRegs^.m_computeNumThreadZ;
 cmdBuffer[$d]:=$c00a1000;
 cmdBuffer[$e]:=0;
 Result:=0;
end;

function ps4_sceGnmSetCsShaderWithModifier(cmdBuffer:PDWORD;numDwords:QWORD;csRegs:PCsStageRegisters;shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<=24) or (csRegs=nil) or ((shaderModifier and $fffffc3f)<>0) then Exit;
 cmdBuffer[0]:=$c0027602;      //IT_SET_SH_REG
 cmdBuffer[1]:=$20c;
 cmdBuffer[2]:=csRegs^.m_computePgmLo;
 cmdBuffer[3]:=csRegs^.m_computePgmHi;
 cmdBuffer[4]:=$c0027602;      //IT_SET_SH_REG
 cmdBuffer[5]:=$212;
 if (shaderModifier=0) then
 begin
  cmdBuffer[6]:=csRegs^.m_computePgmRsrc1;
 end else
 begin
  cmdBuffer[6]:=(csRegs^.m_computePgmRsrc1 and $fffffc3f) or shaderModifier;
 end;
 cmdBuffer[7] :=csRegs^.m_computePgmRsrc2;
 cmdBuffer[8] :=$c0037602;   //IT_SET_SH_REG
 cmdBuffer[9] :=$207;
 cmdBuffer[10]:=csRegs^.m_computeNumThreadX;
 cmdBuffer[$b]:=csRegs^.m_computeNumThreadY;
 cmdBuffer[$c]:=csRegs^.m_computeNumThreadZ;
 cmdBuffer[$d]:=$c00a1000;   //IT_NOP    COUNT = 10,
 cmdBuffer[$e]:=0;
 Result:=0;
end;


{Contains pointer to shader code for vertex stage (VS), plus additional register settings as determined by the shader compiler.}

type
 PVsStageRegisters=^VsStageRegisters;
 VsStageRegisters=packed record
  m_spiShaderPgmLoVs,     //0< The pointer to shader program (bits 39:8).
  m_spiShaderPgmHiVs,     //1< The pointer to shader program (bits 47:40). This must be set to zero.
  m_spiShaderPgmRsrc1Vs,  //2
  m_spiShaderPgmRsrc2Vs,  //3
  m_spiVsOutConfig,       //4
  m_spiShaderPosFormat,   //5
  m_paClVsOutCntl:DWORD;  //6
 end;

 //EmbeddedVsShader=(kEmbeddedVsShaderFullScreen,kNumEmbeddedVsShaders);

function ps4_sceGnmSetVsShader(cmdBuffer:PDWORD;numDwords:QWORD;vsRegs:PVsStageRegisters;shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
var
 m:DWORD;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<29) or (vsRegs=nil) or ((shaderModifier and $fcfffc3f)<>0) then Exit;

 cmdBuffer[0]:=$c0027600;
 cmdBuffer[1]:=$48;
 cmdBuffer[2]:=vsRegs^.m_spiShaderPgmLoVs;
 cmdBuffer[3]:=vsRegs^.m_spiShaderPgmHiVs;
 cmdBuffer[4]:=$c0027600;
 cmdBuffer[5]:=$4a;

 if (shaderModifier=0) then
  m:=vsRegs^.m_spiShaderPgmRsrc1Vs
 else
  m:=vsRegs^.m_spiShaderPgmRsrc1Vs and $fcfffc3f or shaderModifier;

 cmdBuffer[6]  :=m;
 cmdBuffer[7]  :=vsRegs^.m_spiShaderPgmRsrc2Vs;
 cmdBuffer[8]  :=$c0016900;
 cmdBuffer[9]  :=$207;
 cmdBuffer[10] :=vsRegs^.m_paClVsOutCntl;
 cmdBuffer[$b] :=$c0016900;
 cmdBuffer[$c] :=$1b1;
 cmdBuffer[$d] :=vsRegs^.m_spiVsOutConfig;
 cmdBuffer[$e] :=$c0016900;
 cmdBuffer[$f] :=$1c3;
 cmdBuffer[$10]:=vsRegs^.m_spiShaderPosFormat;
 cmdBuffer[$11]:=$c00a1000;
 cmdBuffer[$12]:=0;

 Result:=0;
end;

//Contains pointer to shader code for pixel stage (PS), plus additional register settings as determined by the shader compiler.
type
 PPsStageRegisters=^PsStageRegisters;
 PsStageRegisters=packed record
  m_spiShaderPgmLoPs,     //0< A pointer to shader program (bits 39:8).
  m_spiShaderPgmHiPs,     //1< A pointer to shader program (bits 47:40). This must be set to zero.
                          //
  m_spiShaderPgmRsrc1Ps,  //2
  m_spiShaderPgmRsrc2Ps,  //3
                          //
  m_spiShaderZFormat,     //4
  m_spiShaderColFormat,   //5
                          //
  m_spiPsInputEna,        //6
  m_spiPsInputAddr,       //7
                          //
  m_spiPsInControl,       //8
  m_spiBarycCntl,         //9
                          //
  m_dbShaderControl,      //10
  m_cbShaderMask:DWORD;   //11  0..11 ??
 end;

function ps4_sceGnmSetPsShader(cmdBuffer:PDWORD;numDwords:QWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<29) then Exit;

 if (psRegs=nil) then
 begin
  cmdBuffer[0]:=$c0027600;
  cmdBuffer[1]:=8;
  cmdBuffer[2]:=0;
  cmdBuffer[3]:=0;
  cmdBuffer[4]:=$c0016900;
  cmdBuffer[5]:=$203;
  cmdBuffer[6]:=0;
  cmdBuffer[7]:=$c01f1000;
  cmdBuffer[8]:=0;
 end else
 begin
  cmdBuffer[0]:=$c0027600;
  cmdBuffer[1]:=8;
  cmdBuffer[2]:=psRegs^.m_spiShaderPgmLoPs;
  cmdBuffer[3]:=psRegs^.m_spiShaderPgmHiPs;
  cmdBuffer[4]:=$c0027600;
  cmdBuffer[5]:=10;
  cmdBuffer[6]:=psRegs^.m_spiShaderPgmRsrc1Ps;
  cmdBuffer[7]:=psRegs^.m_spiShaderPgmRsrc2Ps;
  cmdBuffer[8]:=$c0026900;
  cmdBuffer[9]:=$1c4;
  cmdBuffer[10]:=psRegs^.m_spiShaderZFormat;
  cmdBuffer[$b]:=psRegs^.m_spiShaderColFormat;
  cmdBuffer[$c]:=$c0026900;
  cmdBuffer[$d]:=$1b3;
  cmdBuffer[$e]:=psRegs^.m_spiPsInputEna;
  cmdBuffer[$f]:=psRegs^.m_spiPsInputAddr;
  cmdBuffer[$10]:=$c0016900;
  cmdBuffer[$11]:=$1b6;
  cmdBuffer[$12]:=psRegs^.m_spiPsInControl;
  cmdBuffer[$13]:=$c0016900;
  cmdBuffer[$14]:=$1b8;
  cmdBuffer[$15]:=psRegs^.m_spiBarycCntl;
  cmdBuffer[$16]:=$c0016900;
  cmdBuffer[$17]:=$203;
  cmdBuffer[$18]:=psRegs^.m_dbShaderControl;
  cmdBuffer[$19]:=$c0016900;
  cmdBuffer[$1a]:=$8f;
  cmdBuffer[$1b]:=psRegs^.m_cbShaderMask;
  cmdBuffer[$1c]:=$c00a1000;
  cmdBuffer[$1d]:=0;
 end;
 Result:=0;
end;

function ps4_sceGnmSetPsShader350(cmdBuffer:PDWORD;numDwords:QWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<29) then Exit;

 if (psRegs=nil) then
 begin
  cmdBuffer[0] :=$c0027600; //IT_SET_SH_REG
  cmdBuffer[1] :=8;
  cmdBuffer[2] :=0;
  cmdBuffer[3] :=0;
  cmdBuffer[4] :=$c0016900;  //IT_SET_CONTEXT_REG
  cmdBuffer[5] :=$203;
  cmdBuffer[6] :=0;
  cmdBuffer[7] :=$c0016900; //IT_SET_CONTEXT_REG
  cmdBuffer[8] :=$8f;
  cmdBuffer[9] :=$f;
  cmdBuffer[10]:=$c01c1000; //IT_NOP
  cmdBuffer[11]:=0;
 end else
 begin
  cmdBuffer[0]:=$c0027600;  //IT_SET_SH_REG
  cmdBuffer[1]:=8;
  cmdBuffer[2]:=psRegs^.m_spiShaderPgmLoPs;
  cmdBuffer[3]:=psRegs^.m_spiShaderPgmHiPs;

  cmdBuffer[4]:=$c0027600;  //IT_SET_SH_REG
  cmdBuffer[5]:=10;
  cmdBuffer[6]:=psRegs^.m_spiShaderPgmRsrc1Ps;
  cmdBuffer[7]:=psRegs^.m_spiShaderPgmRsrc2Ps;

  cmdBuffer[8]  :=$c0026900; //IT_SET_CONTEXT_REG
  cmdBuffer[9]  :=$1c4;
  cmdBuffer[$a]:=psRegs^.m_spiShaderZFormat;
  cmdBuffer[$b]:=psRegs^.m_spiShaderColFormat;

  cmdBuffer[$c]:=$c0026900; //IT_SET_CONTEXT_REG
  cmdBuffer[$d]:=$1b3;
  cmdBuffer[$e]:=psRegs^.m_spiPsInputEna;
  cmdBuffer[$f]:=psRegs^.m_spiPsInputAddr;

  cmdBuffer[$10]:=$c0016900; //IT_SET_CONTEXT_REG
  cmdBuffer[$11]:=$1b6;
  cmdBuffer[$12]:=psRegs^.m_spiPsInControl;

  cmdBuffer[$13]:=$c0016900; //IT_SET_CONTEXT_REG
  cmdBuffer[$14]:=$1b8;
  cmdBuffer[$15]:=psRegs^.m_spiBarycCntl;

  cmdBuffer[$16]:=$c0016900; //IT_SET_CONTEXT_REG
  cmdBuffer[$17]:=$203;
  cmdBuffer[$18]:=psRegs^.m_dbShaderControl;

  cmdBuffer[$19]:=$c0016900; //IT_SET_CONTEXT_REG
  cmdBuffer[$1a]:=$8f;
  cmdBuffer[$1b]:=psRegs^.m_cbShaderMask;
  cmdBuffer[$1c]:=$c00a1000; //IT_NOP
  cmdBuffer[$1d]:=0;

 end;
 Result:=0;
end;

const
 kEmbeddedVsShaderFullScreen = 0;
 kNumEmbeddedVsShaders       = 1;


// if (shaderId != 0) {
//   thunk_FUN_000001de(0,"sce::Gnm::setEmbeddedVsShader() error: Unknown shaderId %u passed.\n");
//   return -0x7111ff01;
// }
// sceGnmSetVsShader(cmdBuffer,numDwords,EmbVsRegsPtr,shaderModifier);

//EmbVsRegs
//[0]                FE000F1h,           0h,       C0000h,           4h
//[4]                      0h,           4h,           0h

//_fe000f100_EmbVsShader0 = 0xbeeb03ff;
//uRam0000000fe000f104 = 7;
//uRam0000000fe000f108 = 0x36020081;
//uRam0000000fe000f10c = 0x34020281;
//uRam0000000fe000f110 = 0x360000c2;
//uRam0000000fe000f114 = 0x4a0202c1;
//uRam0000000fe000f118 = 0x4a0000c1;
//uRam0000000fe000f11c = 0x7e020b01;
//_fe000f120_EmbVsShader1 = 0x7e000b00;
//uRam0000000fe000f124 = 0x7e040280;
//uRam0000000fe000f128 = 0x7e0602f2;
//uRam0000000fe000f12c = 0xf80008cf;
//uRam0000000fe000f130 = 0x3020001;
//uRam0000000fe000f134 = 0xf800020f;
//uRam0000000fe000f138 = 0x3030303;
//_fe000f13c_EmbVsShader2 = 0xbf810000;
//uRam0000000fe000f140 = 0x5362724f;
//uRam0000000fe000f144 = 0x7726468;
//uRam0000000fe000f148 = 0x4047;
//uRam0000000fe000f14c = 0;
//uRam0000000fe000f150 = 0x47f8c29f;
//uRam0000000fe000f154 = 0x9b2da5cf;
//uRam0000000fe000f158 = 0xff7c5b7d;

const
 EmbVsRegs:VsStageRegisters=(
  m_spiShaderPgmLoVs   :$FE000F1;
  m_spiShaderPgmHiVs   :0;
  m_spiShaderPgmRsrc1Vs:$C0000;
  m_spiShaderPgmRsrc2Vs:4;
  m_spiVsOutConfig     :0;
  m_spiShaderPosFormat :4;
  m_paClVsOutCntl      :0
 );

 EmbVsShader:array[0..22] of DWORD=(
   $beeb03ff,
   7,
   $36020081,
   $34020281,
   $360000c2,
   $4a0202c1,
   $4a0000c1,
   $7e020b01,
   $7e000b00,
   $7e040280,
   $7e0602f2,
   $f80008cf,
   $3020001,
   $f800020f,
   $3030303,
   $bf810000,

   $5362724f,
   $7726468,
   $4047,
   0,
   $47f8c29f,
   $9b2da5cf,
   $ff7c5b7d
 );

var
 EmbVsShaderPtr:Pointer;

function ps4_sceGnmSetEmbeddedVsShader(cmdBuffer:PDWORD;numDwords:QWORD;shaderId,shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
var
 VsRegs:VsStageRegisters;
 //F:THandle;
begin

 Assert(shaderId=0,'error: Unknown shaderId passed.');

 VsRegs:=EmbVsRegs;

 if (EmbVsShaderPtr=nil) then
 begin
  EmbVsShaderPtr:=AllocMem(kAlignmentOfShaderInBytes*2-1);
  Move(EmbVsShader,Align(EmbVsShaderPtr,kAlignmentOfShaderInBytes)^,SizeOf(EmbVsShader));
 end;

 patchShaderGpuAddress(Align(EmbVsShaderPtr,kAlignmentOfShaderInBytes),VsRegs.m_spiShaderPgmHiVs,VsRegs.m_spiShaderPgmLoVs);

 Result:=ps4_sceGnmSetVsShader(cmdBuffer,numDwords,@VsRegs,shaderModifier);

 //F:=FileCreate('EmbVsShader.bin');
 //FileWrite(F,EmbVsShader,SizeOf(EmbVsShader));
 //FileClose(F);
end;

function ps4_sceGnmUpdateVsShader(cmdBuffer:PDWORD;numDwords:QWORD;vsRegs:PVsStageRegisters;shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (vsRegs=nil) or (numDwords<29) or ((shaderModifier and $fcfffc3f)<>0) then Exit;

 cmdBuffer[0]:=$c0027600;
 cmdBuffer[1]:=$48;
 cmdBuffer[2]:=vsRegs^.m_spiShaderPgmLoVs;
 cmdBuffer[3]:=vsRegs^.m_spiShaderPgmHiVs;
 cmdBuffer[4]:=$c0027600;
 cmdBuffer[5]:=$4a;

 if (shaderModifier=0) then
 begin
  cmdBuffer[6]:=vsRegs^.m_spiShaderPgmRsrc1Vs;
 end else
 begin
  cmdBuffer[6]:=(vsRegs^.m_spiShaderPgmRsrc1Vs and $fcfffc3f) or shaderModifier;
 end;

 cmdBuffer[7]  :=vsRegs^.m_spiShaderPgmRsrc2Vs;
 cmdBuffer[8]  :=$c0011000;
 cmdBuffer[9]  :=$c01e0207;
 cmdBuffer[10] :=vsRegs^.m_paClVsOutCntl;
 cmdBuffer[$b] :=$c0011000;
 cmdBuffer[$c] :=$c01e01b1;
 cmdBuffer[$d] :=vsRegs^.m_spiVsOutConfig;
 cmdBuffer[$e] :=$c0011000;
 cmdBuffer[$f] :=$c01e01c3;
 cmdBuffer[$10]:=vsRegs^.m_spiShaderPosFormat;
 cmdBuffer[$11]:=$c00a1000;
 cmdBuffer[$12]:=0;
 Result:=0;
end;

function ps4_sceGnmUpdatePsShader(cmdBuffer:PDWORD;numDwords:QWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<29) then Exit;
 if (psRegs=nil) then
 begin
  cmdBuffer[0]:=$c0027600;
  cmdBuffer[1]:=8;
  cmdBuffer[2]:=0;
  cmdBuffer[3]:=0;
  cmdBuffer[4]:=$c0011000;
  cmdBuffer[5]:=$c01e0203;
  cmdBuffer[6]:=0;
  cmdBuffer[7]:=$c01f1000;
  cmdBuffer[8]:=0;
 end else
 begin
  cmdBuffer[0]  :=$c0027600;
  cmdBuffer[1]  :=8;
  cmdBuffer[2]  :=psRegs^.m_spiShaderPgmLoPs;
  cmdBuffer[3]  :=psRegs^.m_spiShaderPgmHiPs;
  cmdBuffer[4]  :=$c0027600;
  cmdBuffer[5]  :=10;
  cmdBuffer[6]  :=psRegs^.m_spiShaderPgmRsrc1Ps;
  cmdBuffer[7]  :=psRegs^.m_spiShaderPgmRsrc2Ps;
  cmdBuffer[8]  :=$c0021000;
  cmdBuffer[9]  :=$c01e01c4;
  cmdBuffer[10] :=psRegs^.m_spiShaderZFormat;
  cmdBuffer[$b] :=psRegs^.m_spiShaderColFormat;
  cmdBuffer[$c] :=$c0021000;
  cmdBuffer[$d] :=$c01e01b3;
  cmdBuffer[$e] :=psRegs^.m_spiPsInputEna;
  cmdBuffer[$f] :=psRegs^.m_spiPsInputAddr;
  cmdBuffer[$10]:=$c0011000;
  cmdBuffer[$11]:=$c01e01b6;
  cmdBuffer[$12]:=psRegs^.m_spiPsInControl;
  cmdBuffer[$13]:=$c0011000;
  cmdBuffer[$14]:=$c01e01b8;
  cmdBuffer[$15]:=psRegs^.m_spiBarycCntl;
  cmdBuffer[$16]:=$c0011000;
  cmdBuffer[$17]:=$c01e0203;
  cmdBuffer[$18]:=psRegs^.m_dbShaderControl;
  cmdBuffer[$19]:=$c0011000;
  cmdBuffer[$1a]:=$c01e008f;
  cmdBuffer[$1b]:=psRegs^.m_cbShaderMask;
  cmdBuffer[$1c]:=$c00a1000;
  cmdBuffer[$1d]:=0;
 end;
 Result:=0;
end;

function ps4_sceGnmUpdatePsShader350(cmdBuffer:PDWORD;numDwords:QWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<29) then Exit;
 if (psRegs=nil) then
 begin
  cmdBuffer[0]:=$c0027600;
  cmdBuffer[1]:=8;
  cmdBuffer[2]:=0;
  cmdBuffer[3]:=0;
  cmdBuffer[4]:=$c0011000;
  cmdBuffer[5]:=$c01e0203;
  cmdBuffer[6]:=0;
  cmdBuffer[7]:=$c0016900;
  cmdBuffer[8]:=$0000008f;
  cmdBuffer[9]:=$f;
  cmdBuffer[10]:=$c01c1000;
  cmdBuffer[11]:=0
 end else
 begin
  cmdBuffer[0]  :=$c0027600;
  cmdBuffer[1]  :=8;
  cmdBuffer[2]  :=psRegs^.m_spiShaderPgmLoPs;
  cmdBuffer[3]  :=psRegs^.m_spiShaderPgmHiPs;
  cmdBuffer[4]  :=$c0027600;
  cmdBuffer[5]  :=10;
  cmdBuffer[6]  :=psRegs^.m_spiShaderPgmRsrc1Ps;
  cmdBuffer[7]  :=psRegs^.m_spiShaderPgmRsrc2Ps;
  cmdBuffer[8]  :=$c0021000;
  cmdBuffer[9]  :=$c01e01c4;
  cmdBuffer[10] :=psRegs^.m_spiShaderZFormat;
  cmdBuffer[$b] :=psRegs^.m_spiShaderColFormat;
  cmdBuffer[$c] :=$c0021000;
  cmdBuffer[$d] :=$c01e01b3;
  cmdBuffer[$e] :=psRegs^.m_spiPsInputEna;
  cmdBuffer[$f] :=psRegs^.m_spiPsInputAddr;
  cmdBuffer[$10]:=$c0011000;
  cmdBuffer[$11]:=$c01e01b6;
  cmdBuffer[$12]:=psRegs^.m_spiPsInControl;
  cmdBuffer[$13]:=$c0011000;
  cmdBuffer[$14]:=$c01e01b8;
  cmdBuffer[$15]:=psRegs^.m_spiBarycCntl;
  cmdBuffer[$16]:=$c0011000;
  cmdBuffer[$17]:=$c01e0203;
  cmdBuffer[$18]:=psRegs^.m_dbShaderControl;
  cmdBuffer[$19]:=$c0011000;
  cmdBuffer[$1a]:=$c01e008f;
  cmdBuffer[$1b]:=psRegs^.m_cbShaderMask;
  cmdBuffer[$1c]:=$c00a1000;
  cmdBuffer[$1d]:=0;
 end;
 Result:=0;
end;

function ps4_sceGnmDispatchDirect(cmdBuffer:PDWORD;numDwords:QWORD;
          threadGroupX,threadGroupY,threadGroupZ,modifier:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<9) or (integer(threadGroupY or threadGroupX or threadGroupZ)<=-1) then Exit;
 cmdBuffer[0]:=(modifier and 1) or $c0031502;
 cmdBuffer[1]:=threadGroupX;
 cmdBuffer[2]:=threadGroupY;
 cmdBuffer[3]:=threadGroupZ;
 cmdBuffer[4]:=(modifier and $18) or 1;
 cmdBuffer[5]:=$c0021000;
 cmdBuffer[6]:=0;
 Result:=0;
end;

//Draws a set of primitives using indices auto-generated by the VGT
function ps4_sceGnmDrawIndexAuto(cmdBuffer:PDWORD;numDwords:QWORD;
          indexCount,modifier:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<7) or ((modifier and $1ffffffe)<>0) then Exit;
 cmdBuffer[0]:=(modifier and 1) or $c0012d00;
 cmdBuffer[1]:=indexCount;
 cmdBuffer[2]:=2;
 cmdBuffer[3]:=$c0021000;
 cmdBuffer[4]:=0;
 Result:=0;
end;

function ps4_sceGnmDrawIndex(cmdBuffer:PDWORD;numDwords:QWORD;
          indexCount:DWORD;indexAddr:Pointer;modifier,inlineMode:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (cmdBuffer=nil) or (numDwords<10) or (indexAddr=nil) or
    ((ptruint(indexAddr) and 1)<>0) or
    ((modifier and $1ffffffe)<>0) then Exit(-1);

 cmdBuffer[0]:=(modifier and 1) or $c0042700;
 cmdBuffer[1]:=indexCount;
 cmdBuffer[2]:=(ptruint(indexAddr) and $fffffffe);
 cmdBuffer[3]:=(ptruint(indexAddr) shr $20);
 cmdBuffer[4]:=indexCount;
 cmdBuffer[5]:=0;
 cmdBuffer[6]:=$c0021000;
 cmdBuffer[7]:=0;
end;

//Checks if performance counters are available for use by user applications.
function ps4_sceGnmIsUserPaEnabled:Boolean; SysV_ABI_CDecl;
begin
 Result:=False;
end;

function ps4_sceGnmSubmitCommandBuffers(
          count:DWORD;                     //1
          dcbGpuAddrs:PPointer;            //2
          dcbSizesInBytes:PDWORD;          //3
          ccbGpuAddrs:PPointer;            //4
          ccbSizesInBytes:PDWORD):Integer; SysV_ABI_CDecl; //5
begin
 //exit(0);

 vSubmitCommandBuffers(count,dcbGpuAddrs,dcbSizesInBytes,ccbGpuAddrs,ccbSizesInBytes,nil);
 Result:=0;
end;

function ps4_sceGnmSubmitAndFlipCommandBuffers(
          count:DWORD;                     //1
          dcbGpuAddrs:PPointer;            //2
          dcbSizesInBytes:PDWORD;          //3
          ccbGpuAddrs:PPointer;            //4
          ccbSizesInBytes:PDWORD;          //5
          videoOutHandle:Integer;          //6
          displayBufferIndex:Integer;      //7
          flipMode:Integer;                //8
          flipArg:QWORD):Integer; SysV_ABI_CDecl;    //9
var
 Flip:TqcFlipInfo;
begin
 //exit(0);

 Flip.hVideo     :=videoOutHandle;
 Flip.bufferIndex:=displayBufferIndex;
 Flip.flipMode   :=flipMode;
 Flip.flipArg    :=flipArg;

 vSubmitCommandBuffers(count,dcbGpuAddrs,dcbSizesInBytes,ccbGpuAddrs,ccbSizesInBytes,@Flip);
 Result:=0;
end;

//Signals the system that every graphics and asynchronous compute command buffer for this frame has been submitted.
function ps4_sceGnmSubmitDone:Integer; SysV_ABI_CDecl;
begin
 //exit(0);

 //Writeln('SubmitDone');
 vSubmitDone;
 Result:=0;
end;

procedure ps4_sceGnmFlushGarlic(); SysV_ABI_CDecl;
begin
 //flush data to GPU
 System.ReadWriteBarrier;
end;

//sce::Gnm::getTessellationFactorRingBufferBaseAddress(void)
//kTfRingSizeInBytes = 0x20000
function ps4_sceGnmGetTheTessellationFactorRingBufferBaseAddress:Pointer; SysV_ABI_CDecl;
begin
 Result:=Pointer($ff0000000);
end;

const
 SCE_GNM_ERROR_VALIDATION_NOT_ENABLED=$80d13fff;

function ps4_sceGnmValidateCommandBuffers:Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

//A value of true is returned if submit/dingdong is allowed; otherwise false is returned.
function ps4_sceGnmAreSubmitsAllowed:Boolean; SysV_ABI_CDecl;
begin
 Result:=true;
end;

const
//EqEventType
 kEqEventCompute0RelMem = $00; ///< ReleaseMem event from the compute pipe 0.
 kEqEventCompute1RelMem = $01; ///< ReleaseMem event from the compute pipe 1.
 kEqEventCompute2RelMem = $02; ///< ReleaseMem event from the compute pipe 2.
 kEqEventCompute3RelMem = $03; ///< ReleaseMem event from the compute pipe 3.
 kEqEventCompute4RelMem = $04; ///< ReleaseMem event from the compute pipe 4.
 kEqEventCompute5RelMem = $05; ///< ReleaseMem event from the compute pipe 5.
 kEqEventCompute6RelMem = $06; ///< ReleaseMem event from the compute pipe 6.
 kEqEventGfxEop         = $40; ///< EOP event from the Gfx pipe.

var
 EopEvents:Thamt64locked;

function ps4_sceGnmAddEqEvent(eq:SceKernelEqueue;id:Integer;udata:Pointer):Integer; SysV_ABI_CDecl;
var
 P:PPointer;
 node:PKEventNode;
begin
 Writeln('sceGnmAddEqEvent:',id);
 if (id<>kEqEventGfxEop) then Assert(false);

 EopEvents.LockWr;
 P:=HAMT_search64(@EopEvents.hamt,QWORD(eq));
 if (P<>nil) then
 begin
  node:=P^;
  node^.ev.udata:=udata;
 end else
 begin
  node:=_alloc_kevent_node(eq,SizeOf(TKEventNode));
  if (node=Pointer(1)) then
  begin
   EopEvents.Unlock;
   Exit(SCE_KERNEL_ERROR_EBADF);
  end;
  if (node=nil) then
  begin
   EopEvents.Unlock;
   Exit(SCE_KERNEL_ERROR_ENOMEM);
  end;
  node^.ev.filter:=SCE_KERNEL_EVFILT_GNM;
  node^.ev.data  :=id;
  node^.ev.udata :=udata;
  HAMT_insert64(@EopEvents.hamt,QWORD(eq),node);
 end;
 EopEvents.Unlock;

 Result:=0;
end;

procedure _on_trigger_eop(data,userdata:Pointer);
var
 node:PKEventNode;
begin
 node:=data;
 if (node=nil) then Exit;
 _trigger_kevent_node(node,nil,nil);
end;

procedure post_event_eop;
begin
 EopEvents.LockRd;
 HAMT_traverse64(@EopEvents.hamt,@_on_trigger_eop,nil);
 EopEvents.Unlock;
end;

function Load_libSceGnmDriver(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:='libSceGnmDriver.prx';

 lib:=Result._add_lib('libSceGnmDriver');

 lib^.set_proc($D07DAF0586D32C72,@ps4_sceGnmDrawInitDefaultHardwareState200);
 lib^.set_proc($C9BD9C4616A00F52,@ps4_sceGnmDrawInitDefaultHardwareState350);

 lib^.set_proc($5B512D8FF8E55BB6,@ps4_sceGnmInsertPushMarker);
 lib^.set_proc($EEA65536012EF926,@ps4_sceGnmInsertPopMarker);

 lib^.set_proc($D6A5CB1C8A5138F1,@ps4_sceGnmInsertWaitFlipDone);
 lib^.set_proc($29796D9C2C042474,@ps4_sceGnmSetCsShader);

 lib^.set_proc($2B1FE1FE759027C0,@ps4_sceGnmSetCsShaderWithModifier);

 lib^.set_proc($8008429FA5225386,@ps4_sceGnmSetVsShader);
 lib^.set_proc($6D055DE58CC26A5D,@ps4_sceGnmSetPsShader);

 lib^.set_proc($E6E14A7248896113,@ps4_sceGnmSetPsShader350);

 lib^.set_proc($F8016F3845EB2899,@ps4_sceGnmSetEmbeddedVsShader);

 lib^.set_proc($577D55D3552249C6,@ps4_sceGnmUpdateVsShader);
 lib^.set_proc($E0C811C3F6D53505,@ps4_sceGnmUpdatePsShader);
 lib^.set_proc($98B54BECDEC15418,@ps4_sceGnmUpdatePsShader350);

 lib^.set_proc($D01CCB1A58DCC01A,@ps4_sceGnmDispatchDirect);

 lib^.set_proc($186B27EE3313C70E,@ps4_sceGnmDrawIndexAuto);
 lib^.set_proc($1E54CFA19FE863B6,@ps4_sceGnmDrawIndex);

 lib^.set_proc($8E0DF7AC428B7D5B,@ps4_sceGnmIsUserPaEnabled);

 lib^.set_proc($CF0634615F754D32,@ps4_sceGnmSubmitCommandBuffers);
 lib^.set_proc($C5BC4D6AD6B0A217,@ps4_sceGnmSubmitAndFlipCommandBuffers);
 lib^.set_proc($CAF67BDEE414AAB9,@ps4_sceGnmSubmitDone);
 lib^.set_proc($881B7739ED342AF7,@ps4_sceGnmFlushGarlic);

 lib^.set_proc($967DF7CE306B7E39,@ps4_sceGnmGetTheTessellationFactorRingBufferBaseAddress);
 lib^.set_proc($8823BCD38660CDD0,@ps4_sceGnmValidateCommandBuffers);
 lib^.set_proc($6F4C729659D563F2,@ps4_sceGnmAddEqEvent);

 lib^.set_proc($6F4F0082D3E51CF8,@ps4_sceGnmAreSubmitsAllowed);

  //nop nid:libSceGnmDriver:DBDA0ABCA5F3119A:sceGnmMapComputeQueue

end;

initialization
 EopEvents.Init;
 ps4_app.RegistredPreLoad('libSceGnmDriver.prx',@Load_libSceGnmDriver);
 ps4_app.RegistredPreLoad('libSceGnmDriver_padebug.prx',@Load_libSceGnmDriver);


end.

