unit ps4_libSceGnmDriver;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ps4_videodrv;

procedure post_event_eop;

function  ps4_sceGnmInsertThreadTraceMarker(cmdBuffer:PDWORD;numDwords:DWORD;param1:DWORD;param2:Pointer):Integer; SysV_ABI_CDecl;

implementation

uses
 hamt,
 atomic,
 ps4_program,
 sys_signal,
 sys_kernel,
 ps4_queue,
 ps4_libkernel,
 ps4_libSceVideoOut{, ps4_pssl};

const
 InitDefault_stub:array[0..114] of DWORD=(
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
  $1ff,
  $c0017600,
  $46,
  $1ff,
  $c0017600,
  $87,
  $1ff,
  $c0017600,
  199,
  $1ff,
  $c0017600,
  $107,
  0,
  $c0017600,
  $147,
  $1ff,
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
  $6000000,
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
  $e0000000
 );

function _sceGnmDrawInitDefaultHardwareState(cmdBuffer:PDWORD;numDwords:DWORD;param_3:Integer):DWORD;
var
 _cmdBuffer:PDWORD;
 count:DWORD;
begin
 Result:=0;

 if (numDwords>$FF) then
 begin
  _cmdBuffer:=cmdBuffer;

  if (param_3<>0) then
  begin
   _cmdBuffer:=cmdBuffer+$C;
   cmdBuffer[0]:=$c0012800;
   cmdBuffer[1]:=$80000000;
   cmdBuffer[2]:=$80000000;
   cmdBuffer[3]:=$c0001200;
   cmdBuffer[4]:=0;
   cmdBuffer[5]:=$c0055800;
   cmdBuffer[6]:=$2ec47fc0;
   cmdBuffer[7]:=$ffffffff;
   cmdBuffer[8]:=0;
   cmdBuffer[9]:=0;
   cmdBuffer[10]:=0;
   cmdBuffer[11]:=$a;
  end;

  Move(InitDefault_stub,cmdBuffer^,SizeOf(InitDefault_stub));

  count:=$100-(QWORD(Int64(_cmdBuffer)+($1cc-Int64(cmdBuffer))) shr 2);  //ftw
  Result:=(Int64(_cmdBuffer) + ((count*4+$1cc)-Int64(cmdBuffer))) shr 2;

  _cmdBuffer[$73]:=(count*$10000+$3ffe0000) or $c0001000;
  _cmdBuffer[$74]:=0;
 end;

end;

function ps4_sceGnmDrawInitDefaultHardwareState(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=_sceGnmDrawInitDefaultHardwareState(cmdBuffer,numDwords,1);
end;

const
 InitDefault175_stub:array[0..128] of DWORD=(
  $c0012800,
  $80000000,
  $80000000,
  $c0001200,
  $0,
  $c0055800,
  $2ec47fc0,
  $ffffffff,
  $0,
  $0,
  $0,
  $10,
  $c0017600,
  $216,
  $ffffffff,
  $c0017600,
  $217,
  $ffffffff,
  $c0017600,
  $215,
  $0,
  $c0016900,
  $2f9,
  $2d,
  $c0016900,
  $282,
  $8,
  $c0016900,
  $280,
  $80008,
  $c0016900,
  $281,
  $ffff0000,
  $c0016900,
  $204,
  $0,
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
  $1,
  $c0017600,
  $7,
  $1ff,
  $c0017600,
  $46,
  $1ff,
  $c0017600,
  $87,
  $1ff,
  $c0017600,
  $199,
  $1ff,
  $c0017600,
  $107,
  $0,
  $c0017600,
  $147,
  $1ff,
  $c0016900,
  $1b1,
  $2,
  $c0016900,
  $101,
  $0,
  $c0016900,
  $100,
  $ffffffff,
  $c0016900,
  $103,
  $0,
  $c0016900,
  $284,
  $0,
  $c0016900,
  $290,
  $0,
  $c0016900,
  $2ae,
  $0,
  $c0016900,
  $292,
  $0,
  $c0016900,
  $293,
  $6020000,
  $c0016900,
  $2f8,
  $0,
  $c0016900,
  $2de,
  $1e9,
  $c0036900,
  $295,
  $100,
  $100,
  $4,
  $c0017900,
  $200,
  $e0000000,
  $c07f1000,
  $0
 );

function ps4_sceGnmDrawInitDefaultHardwareState175(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=0;
 if (numDwords>$FF) then
 begin
  Move(InitDefault175_stub,cmdBuffer^,SizeOf(InitDefault175_stub));
  Result:=$100;
 end;
end;

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


function _sceGnmDrawInitDefaultHardwareState200(cmdBuffer:PDWORD;numDwords:DWORD;param_3:Integer):DWORD;
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
function ps4_sceGnmDrawInitDefaultHardwareState200(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=_sceGnmDrawInitDefaultHardwareState200(cmdBuffer,numDwords,1);
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

function ps4_sceGnmDrawInitDefaultHardwareState350(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=0;
 if (numDwords>$FF) then
 begin
  Move(InitDefault350_stub,cmdBuffer^,SizeOf(InitDefault350_stub));
  Result:=$100;
 end;
end;

const
 DispatchInitDefaultState_stub:array[0..17] of DWORD=(
  $C0017602,
  $216,
  $FFFFFFFF,
  $C0017602,
  $217,
  $FFFFFFFF,
  $C0017602,
  $215,
  $170,
  $c0055800,
  $28000000,
  0,
  0,
  0,
  0,
  $0000000a,
  $c0ee1000,
  0
 );

function ps4_sceGnmDispatchInitDefaultHardwareState(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=0;
 if (numDwords>$FF) then
 begin
  Move(DispatchInitDefaultState_stub,cmdBuffer^,SizeOf(DispatchInitDefaultState_stub));
  Result:=$100;
 end;
end;

const
 DefaultContextState_stub:array[0..14] of DWORD=(
  $c0012800,
  $80000000,
  $80000000,
  $c0001200,
  0,
  $c0002f00,
  1,
  $c0016900,
  $00000102,
  0,
  $c0016900,
  $00000202,
  $00cc0010,
  $c0111000,
  0
 );

function ps4_sceGnmDrawInitToDefaultContextState(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=0;
 if (numDwords>$20) then
 begin
  Move(DefaultContextState_stub,cmdBuffer^,SizeOf(DefaultContextState_stub));
  Result:=$20;
 end;
end;

const
 DefaultContextState400_stub:array[0..97] of DWORD=(
  $c0012800,
  $80000000,
  $80000000,
  $c0001200,
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
  $c0016900,
  $2aa,
  $ff,
  $c09e1000,
  0
 );

function ps4_sceGnmDrawInitToDefaultContextState400(cmdBuffer:PDWORD;numDwords:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=0;
 if (numDwords=$100) then
 begin
  Move(DefaultContextState400_stub,cmdBuffer^,SizeOf(DefaultContextState400_stub));
  Result:=$100;
 end;
end;

function ps4_sceGnmInsertPushMarker(cmdBuffer:PDWORD;numDwords:DWORD;name:PChar):Integer; SysV_ABI_CDecl;
var
 cmdSize,len,len3,len4:DWORD;
begin
 if (cmdBuffer=nil) or (name=nil) then Exit(-1);

 len:=StrLen(name);
 len3:=(len + $c) shr 3;
 len4:=(len + $8) shr 2;

 cmdSize:=len4+len3*2;
 if ((cmdSize+2)<>numDwords) then Exit(-1);

 cmdBuffer[0]:=cmdSize*$10000 or $c0001000; //NOP
 cmdBuffer[1]:=$68750001;

 len3:=len+1;
 Move(name^,cmdBuffer[2],len3);
 FillChar(PByte(@cmdBuffer[2])[len3],cmdSize*SizeOf(DWORD)-len3,0);

 Result:=0;
end;

function ps4_sceGnmInsertPopMarker(cmdBuffer:PDWORD;numDwords:DWORD):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmInsertSetMarker(cmdBuffer:PDWORD;numDwords:DWORD;name:PChar):Integer; SysV_ABI_CDecl;
var
 cmdSize,len,len3,len4:DWORD;
begin
 if (cmdBuffer=nil) or (name=nil) then Exit(-1);

 len:=StrLen(name);
 len3:=(len + $c) shr 3;
 len4:=(len + $8) shr 2;

 cmdSize:=len4+len3*2;
 if ((cmdSize+2)<>numDwords) then Exit(-1);

 cmdBuffer[0]:=cmdSize*$10000 or $c0001000; //NOP
 cmdBuffer[1]:=$68750003;

 len3:=len+1;
 Move(name^,cmdBuffer[2],len3);
 FillChar(PByte(@cmdBuffer[2])[len3],cmdSize*SizeOf(DWORD)-len3,0);

 Result:=0;
end;

function ps4_sceGnmInsertPushColorMarker(cmdBuffer:PDWORD;numDwords:DWORD;name:PChar;color:DWORD):Integer; SysV_ABI_CDecl;
var
 cmdSize,len,len3,len2:DWORD;
begin
 if (cmdBuffer=nil) or (name=nil) then Exit(-1);

 len:=StrLen(name);
 len3:=(len + $10) shr 3;
 len2:=(len + $c ) shr 2;

 cmdSize:=len2+len3*2;
 if ((cmdSize+2)<>numDwords) then Exit(-1);

 cmdBuffer[0]:=cmdSize*$10000 or $c0001000; //NOP
 cmdBuffer[1]:=$6875000e;

 len3:=len+1;
 Move(name^,cmdBuffer[2],len3);
 PDWORD(PByte(cmdBuffer)+len3+8)^:=color;
 FillChar(PByte(@cmdBuffer[3])[len3],cmdSize*SizeOf(DWORD)-len-5,0);

 Result:=0;
end;

function ps4_sceGnmInsertSetColorMarker(cmdBuffer:PDWORD;numDwords:DWORD;name:PChar;color:DWORD):Integer; SysV_ABI_CDecl;
var
 cmdSize,len,len3,len2:DWORD;
begin
 if (cmdBuffer=nil) or (name=nil) then Exit(-1);

 len:=StrLen(name);
 len3:=(len + $10) shr 3;
 len2:=(len + $c ) shr 2;

 cmdSize:=len2+len3*2;
 if ((cmdSize+2)<>numDwords) then Exit(-1);

 cmdBuffer[0]:=cmdSize*$10000 or $c0001000;//NOP
 cmdBuffer[1]:=$6875000f;

 len3:=len+1;
 Move(name^,cmdBuffer[2],len3);
 PDWORD(PByte(cmdBuffer)+len3+8)^:=color;
 FillChar(PByte(@cmdBuffer[3])[len3],cmdSize*SizeOf(DWORD)-len-5,0);

 Result:=0;
end;

function ps4_sceGnmInsertThreadTraceMarker(cmdBuffer:PDWORD;numDwords:DWORD;param1:DWORD;param2:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

// called in waitUntilSafeForRendering
function ps4_sceGnmInsertWaitFlipDone(cmdBuffer:PDWORD;numDwords:DWORD;videoOutHandle,displayBufferIndex:Integer):Integer; SysV_ABI_CDecl;
var
 addr:PInt64;
begin
 Result:=-1;
 if (numDwords<>7) then Exit;

 Result:=ps4_sceVideoOutGetBufferLabelAddress(videoOutHandle,@addr);
 if (Result<>0) then Exit(-1);

 addr:=@addr[displayBufferIndex];

 cmdBuffer[0]:=$c0053c00; //IT_WAIT_REG_MEM
 cmdBuffer[1]:=$13;
 cmdBuffer[2]:=QWORD(addr);
 cmdBuffer[3]:=(QWORD(addr) shr $20);
 cmdBuffer[4]:=0;
 cmdBuffer[5]:=$ffffffff;
 cmdBuffer[6]:=10;
 Result:=0;
end;

const
 kAlignmentOfShaderInBytes=256;

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

function ps4_sceGnmSetCsShader(cmdBuffer:PDWORD;numDwords:DWORD;csRegs:PCsStageRegisters):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmSetCsShaderWithModifier(cmdBuffer:PDWORD;numDwords:DWORD;csRegs:PCsStageRegisters;shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmSetVsShader(cmdBuffer:PDWORD;numDwords:DWORD;vsRegs:PVsStageRegisters;shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmSetPsShader(cmdBuffer:PDWORD;numDwords:DWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmSetPsShader350(cmdBuffer:PDWORD;numDwords:DWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
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
 kEmbeddedVsShaderFullScreen  = 0;
 kNumEmbeddedVsShaders        = 1;

 kEmbeddedPsShaderDummy       = 0;
 kEmbeddedPsShaderDummyG32R32 = 1;
 kNumEmbeddedPsShaders        = 2;

const
 EmbPs0Shader:array[0..15] of DWORD=(
  $beeb03ff,
  3,
  $7e000280,
  $5e000100,
  $bf800000,
  $f8001c0f,
  0,
  0,
  $bf810000,
  $5362724f,
  $7726468,
  $2043,
  0,
  $b0a45b2b,
  $1d39766d,
  $72044b7b
 );

 EmbPs1Shader:array[0..15] of DWORD=(
  $beeb03ff,
  3,
  $7e040280,
  $f8001803,
  $2020202,
  $bf810000,
  $302,
  0,
  $d81c987,
  $5362724f,
  $7726468,
  $1841,
  $4080002,
  $98b9cb94,
  0,
  $6f130734
 );

 EmbVsShader:array[0..23] of DWORD=(
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
  0,
  $bf810000,
  $5362724f,
  $7726468,
  $4047,
  0,
  $47f8c29f,
  $9b2da5cf,
  $ff7c5b7d
 );

type
 PEmbShaders=^TEmbShaders;
 TEmbShaders=packed record
  EmbPsShader0:array[0..63] of DWORD;
  EmbPsShader1:array[0..63] of DWORD;
  EmbVsShader :array[0..63] of DWORD;
  align       :array[0..63] of DWORD;
 end;

var
 EmbShaders:TEmbShaders;

 EmbPsShader0Ptr:Pointer;
 EmbPsShader1Ptr:Pointer;
 EmbVsShaderPtr :Pointer;

procedure GnmInitEmbedded;
var
 s:PEmbShaders;
begin
 s:=Align(@EmbShaders,kAlignmentOfShaderInBytes);

 Move(EmbPs0Shader,s^.EmbPsShader0,SizeOf(EmbPs0Shader));
 Move(EmbPs1Shader,s^.EmbPsShader1,SizeOf(EmbPs1Shader));
 Move(EmbVsShader ,s^.EmbVsShader ,SizeOf(EmbVsShader));

 EmbPsShader0Ptr:=@s^.EmbPsShader0;
 EmbPsShader1Ptr:=@s^.EmbPsShader1;
 EmbVsShaderPtr :=@s^.EmbVsShader ;
end;

const
 EmbVsRegs:VsStageRegisters=(
  m_spiShaderPgmLoVs   :0;
  m_spiShaderPgmHiVs   :0;
  m_spiShaderPgmRsrc1Vs:$C0000;
  m_spiShaderPgmRsrc2Vs:4;
  m_spiVsOutConfig     :0;
  m_spiShaderPosFormat :4;
  m_paClVsOutCntl      :0
 );

 EmbPsRegs0:PsStageRegisters=(
  m_spiShaderPgmLoPs   :0;
  m_spiShaderPgmHiPs   :0;
  m_spiShaderPgmRsrc1Ps:$C0000;
  m_spiShaderPgmRsrc2Ps:4;
  m_spiShaderZFormat   :0;
  m_spiShaderColFormat :4;
  m_spiPsInputEna      :2;
  m_spiPsInputAddr     :2;
  m_spiPsInControl     :0;
  m_spiBarycCntl       :0;
  m_dbShaderControl    :$10;
  m_cbShaderMask       :$F;
 );

 EmbPsRegs1:PsStageRegisters=(
  m_spiShaderPgmLoPs   :0;
  m_spiShaderPgmHiPs   :0;
  m_spiShaderPgmRsrc1Ps:$200000;
  m_spiShaderPgmRsrc2Ps:0;
  m_spiShaderZFormat   :0;
  m_spiShaderColFormat :2;
  m_spiPsInputEna      :2;
  m_spiPsInputAddr     :2;
  m_spiPsInControl     :0;
  m_spiBarycCntl       :0;
  m_dbShaderControl    :$10;
  m_cbShaderMask       :3;
 );

function ps4_sceGnmSetEmbeddedVsShader(cmdBuffer:PDWORD;numDwords:DWORD;shaderId,shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
var
 VsRegs:VsStageRegisters;
begin
 Assert(shaderId=0,'error: Unknown shaderId passed.');

 VsRegs:=EmbVsRegs;

 patchShaderGpuAddress(EmbVsShaderPtr,VsRegs.m_spiShaderPgmHiVs,VsRegs.m_spiShaderPgmLoVs);

 Result:=ps4_sceGnmSetVsShader(cmdBuffer,numDwords,@VsRegs,shaderModifier);
end;

function ps4_sceGnmSetEmbeddedPsShader(cmdBuffer:PDWORD;numDwords:DWORD;shaderId:DWORD):Integer; SysV_ABI_CDecl;
var
 PsRegs:PsStageRegisters;
begin

 Case shaderId of
  0:begin
     PsRegs:=EmbPsRegs0;
     patchShaderGpuAddress(EmbPsShader0Ptr,PsRegs.m_spiShaderPgmHiPs,PsRegs.m_spiShaderPgmLoPs);
    end;
  1:begin
     PsRegs:=EmbPsRegs1;
     patchShaderGpuAddress(EmbPsShader1Ptr,PsRegs.m_spiShaderPgmHiPs,PsRegs.m_spiShaderPgmLoPs);
    end;
  else
   Assert(false,'error: Unknown shaderId passed.');
 end;

 Result:=ps4_sceGnmSetPsShader350(cmdBuffer,numDwords,@PsRegs);
end;

function ps4_sceGnmUpdateVsShader(cmdBuffer:PDWORD;numDwords:DWORD;vsRegs:PVsStageRegisters;shaderModifier:DWORD):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmUpdatePsShader(cmdBuffer:PDWORD;numDwords:DWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmUpdatePsShader350(cmdBuffer:PDWORD;numDwords:DWORD;psRegs:PPsStageRegisters):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmSetVgtControl(cmdBuffer:PDWORD;numDwords:DWORD;
                                 primGroupSizeMinusOne:DWORD;
                                 partialVsWaveMode    :DWORD;
                                 wdSwitchOnlyOnEopMode:DWORD):Integer; SysV_ABI_CDecl;
begin

 Result:=-1;

 if (cmdBuffer<>nil) and
    (numDwords=3) and
    (primGroupSizeMinusOne<$100) and
    ((wdSwitchOnlyOnEopMode or partialVsWaveMode)<2) then
 begin
  Result:=0;
  cmdBuffer[0]:=$c0016900;
  cmdBuffer[1]:=$2aa;
  cmdBuffer[2]:=((partialVsWaveMode and 1) shl $10) or (primGroupSizeMinusOne and $ffff);
 end;
end;

function ps4_sceGnmResetVgtControl(cmdBuffer:PDWORD;param:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer<>nil) and (param=3) then
 begin
  Result:=0;
  cmdBuffer[0]:=$c0016900;
  cmdBuffer[1]:=$2aa;
  cmdBuffer[2]:=$ff;
 end;
end;

function ps4_sceGnmDispatchDirect(cmdBuffer:PDWORD;numDwords:DWORD;
                                  threadGroupX,
                                  threadGroupY,
                                  threadGroupZ,
                                  modifier:DWORD):Integer; SysV_ABI_CDecl;
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
function ps4_sceGnmDrawIndexAuto(cmdBuffer:PDWORD;
                                 numDwords:DWORD;
                                 indexCount,
                                 modifier:DWORD):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmDrawIndex(cmdBuffer:PDWORD;
                             numDwords:DWORD;
                             indexCount:DWORD;
                             indexAddr:Pointer;
                             modifier,
                             inlineMode:DWORD):Integer; SysV_ABI_CDecl;
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

function ps4_sceGnmDrawIndexOffset(cmdBuffer:PDWORD;
                                   numDwords:DWORD;
                                   indexOffset:DWORD;
                                   indexCount:DWORD;
                                   modifier:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=-1;
 if (cmdBuffer=nil) or (numDwords<>9) then Exit;

 cmdBuffer[0]:=(modifier and 1) or $c0033500;
 cmdBuffer[1]:=indexCount;
 cmdBuffer[2]:=indexOffset;
 cmdBuffer[3]:=indexCount;
 cmdBuffer[4]:=0;
 cmdBuffer[5]:=$c0021000;
 cmdBuffer[6]:=0;
 Result:=0;
end;

//Checks if performance counters are available for use by user applications.
function ps4_sceGnmIsUserPaEnabled:Boolean; SysV_ABI_CDecl;
begin
 Result:=False;
end;

//

var
 SceSubmitDoneGame:TRTLCriticalSection;

function ps4_sceGnmSubmitCommandBuffersForWorkload(
          workload:QWORD;
          count:DWORD;
          dcbGpuAddrs:PPointer;
          dcbSizesInBytes:PDWORD;
          ccbGpuAddrs:PPointer;
          ccbSizesInBytes:PDWORD):Integer; SysV_ABI_CDecl;
var
 Submit:TvSubmitInfo;
begin
 if (count=0) then Exit(SCE_KERNEL_ERROR_EINVAL);

 count:=count and $ffffffff;

 Submit:=Default(TvSubmitInfo);
 Submit.count          :=count          ;
 Submit.dcbGpuAddrs    :=dcbGpuAddrs    ;
 Submit.dcbSizesInBytes:=dcbSizesInBytes;
 Submit.ccbGpuAddrs    :=ccbGpuAddrs    ;
 Submit.ccbSizesInBytes:=ccbSizesInBytes;

 _sig_lock;
 EnterCriticalSection(SceSubmitDoneGame);

 Result:=vSubmitCommandBuffers(@Submit,nil);

 LeaveCriticalSection(SceSubmitDoneGame);
 _sig_unlock;

 Result:=0;
end;

function ps4_sceGnmSubmitCommandBuffers(
          count:DWORD;
          dcbGpuAddrs:PPointer;
          dcbSizesInBytes:PDWORD;
          ccbGpuAddrs:PPointer;
          ccbSizesInBytes:PDWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceGnmSubmitCommandBuffersForWorkload(
          count,
          count and $ffffffff,
          dcbGpuAddrs,
          dcbSizesInBytes,
          ccbGpuAddrs,
          ccbSizesInBytes);
end;

//

function ps4_sceGnmSubmitAndFlipCommandBuffersForWorkload(
          workload:QWORD;
          count:DWORD;
          dcbGpuAddrs:PPointer;
          dcbSizesInBytes:PDWORD;
          ccbGpuAddrs:PPointer;
          ccbSizesInBytes:PDWORD;
          videoOutHandle:Integer;
          displayBufferIndex:Integer;
          flipMode:Integer;
          flipArg:QWORD):Integer; SysV_ABI_CDecl; //10
var
 Submit:TvSubmitInfo;
 Flip:TqcFlipInfo;
begin
 if (count=0) or
    (dcbGpuAddrs=nil) or
    (dcbSizesInBytes=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 count:=count and $ffffffff;

 Submit:=Default(TvSubmitInfo);
 Submit.count          :=count          ;
 Submit.dcbGpuAddrs    :=dcbGpuAddrs    ;
 Submit.dcbSizesInBytes:=dcbSizesInBytes;
 Submit.ccbGpuAddrs    :=ccbGpuAddrs    ;
 Submit.ccbSizesInBytes:=ccbSizesInBytes;

 Flip.hVideo     :=videoOutHandle;
 Flip.bufferIndex:=displayBufferIndex;
 Flip.flipMode   :=flipMode;
 Flip.flipArg    :=flipArg;

 _sig_lock;
 EnterCriticalSection(SceSubmitDoneGame);

 Result:=vSubmitCommandBuffers(@Submit,@Flip);

 LeaveCriticalSection(SceSubmitDoneGame);
 _sig_unlock;

 Result:=0;
end;

function ps4_sceGnmSubmitAndFlipCommandBuffers(
          count:DWORD;
          dcbGpuAddrs:PPointer;
          dcbSizesInBytes:PDWORD;
          ccbGpuAddrs:PPointer;
          ccbSizesInBytes:PDWORD;
          videoOutHandle:Integer;
          displayBufferIndex:Integer;
          flipMode:Integer;
          flipArg:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceGnmSubmitAndFlipCommandBuffersForWorkload(
           count,
           count and $ffffffff,
           dcbGpuAddrs,
           dcbSizesInBytes,
           ccbGpuAddrs,
           ccbSizesInBytes,
           videoOutHandle,
           displayBufferIndex,
           flipMode,
           flipArg);
end;

//

const
 SCE_GNM_ERROR_SUBMISSION_NOT_ENOUGH_RESOURCES=-2133782527;

function ps4_sceGnmRequestFlipAndSubmitDoneForWorkload(
          workload:QWORD;
          gpuAddr:Pointer;
          gpuAddrSizeInBytes:DWORD;
          videoOutHandle:Integer;
          displayBufferIndex:Integer;
          flipMode:Integer;
          flipArg:QWORD):Integer; SysV_ABI_CDecl;
var
 Submit:TvSubmitInfo;
 Flip:TqcFlipInfo;
 dcbGpuAddrs:Pointer;
 dcbSizesInBytes:DWORD;
begin
 if (gpuAddr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 if (gpuAddrSizeInBytes<=$FF) then Exit(SCE_GNM_ERROR_SUBMISSION_NOT_ENOUGH_RESOURCES);

 PQWORD(gpuAddr)^:=$68750777c03e1000; //prepare flip?

 dcbGpuAddrs    :=gpuAddr;
 dcbSizesInBytes:=$100;

 Submit:=Default(TvSubmitInfo);
 Submit.count          :=1;
 Submit.dcbGpuAddrs    :=@dcbGpuAddrs;
 Submit.dcbSizesInBytes:=@dcbSizesInBytes;

 Flip.hVideo     :=videoOutHandle;
 Flip.bufferIndex:=displayBufferIndex;
 Flip.flipMode   :=flipMode;
 Flip.flipArg    :=flipArg;

 _sig_lock;
 EnterCriticalSection(SceSubmitDoneGame);

 Result:=vSubmitCommandBuffers(@Submit,@Flip);
 vSubmitDone;

 LeaveCriticalSection(SceSubmitDoneGame);
 _sig_unlock;

 Result:=0;
end;

function ps4_sceGnmRequestFlipAndSubmitDone(
          gpuAddr:Pointer;
          gpuAddrSizeInBytes:DWORD;
          videoOutHandle:Integer;
          displayBufferIndex:Integer;
          flipMode:Integer;
          flipArg:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceGnmRequestFlipAndSubmitDoneForWorkload(
           qword(gpuAddr),
           gpuAddr,
           gpuAddrSizeInBytes,
           videoOutHandle,
           displayBufferIndex,
           flipMode,
           flipArg);
end;

//

//Signals the system that every graphics and asynchronous compute command buffer for this frame has been submitted.
function ps4_sceGnmSubmitDone:Integer; SysV_ABI_CDecl;
begin
 //exit(0);

 _sig_lock;
 EnterCriticalSection(SceSubmitDoneGame);

 vSubmitDone;

 LeaveCriticalSection(SceSubmitDoneGame);
 _sig_unlock;

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

function ps4_sceGnmGetOffChipTessellationBufferSize:QWORD; SysV_ABI_CDecl;
begin
 Result:=$800000
end;

const
 SCE_GNM_ERROR_VALIDATION_NOT_ENABLED=-2133770241; // $80d13fff;

function ps4_sceGnmValidateCommandBuffers(count:DWORD;
                                          dcbGpuAddrs:PPointer;
                                          dcbSizesInBytes:PDWORD;
                                          ccbGpuAddrs:PPointer;
                                          ccbSizesInBytes:PDWORD
                                         ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateDispatchCommandBuffers(count:DWORD;
                                                  dcbGpuAddrs:PPointer;
                                                  dcbSizesInBytes:PDWORD
                                                 ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateDrawCommandBuffers(count:DWORD;
                                              dcbGpuAddrs:PPointer;
                                              dcbSizesInBytes:PDWORD;
                                              ccbGpuAddrs:PPointer;
                                              ccbSizesInBytes:PDWORD
                                             ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateDisableDiagnostics(count:DWORD;
                                              data:Pointer
                                             ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateDisableDiagnostics2(count:DWORD;
                                               diagList:PDWORD
                                              ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateGetDiagnosticInfo(query:Integer;         //ValidationDiagnosticQuery
                                             diagnosticInfo:Pointer //ValidationDiagnosticInfo
                                            ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateGetDiagnostics(query:Integer;            //ValidationDiagnosticQuery
                                          diagnosticOutputs:Pointer
                                         ):Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateResetState:Integer; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_VALIDATION_NOT_ENABLED;
end;

function ps4_sceGnmValidateGetVersion:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceGnmValidateOnSubmitEnabled:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceRazorIsLoaded:Int64; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceGnmDriverCaptureInProgress:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//A value of true is returned if submit/dingdong is allowed; otherwise false is returned.
function ps4_sceGnmAreSubmitsAllowed:Boolean; SysV_ABI_CDecl;
begin
 Result:=vSubmitsAllowed;
end;

const
 SCE_GNM_ERROR_FAILURE=-1897004801; // $8eee00ff;

function ps4_sceGnmGetResourceRegistrationBuffers({params?}):Int64; SysV_ABI_CDecl;
begin
 Result:=SCE_GNM_ERROR_FAILURE;
end;

function ps4_sceGnmRegisterOwner(pOwnerHandle:PInteger;ownerName:Pchar):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGnmRegisterOwner:',ownerName);
 Result:=SCE_GNM_ERROR_FAILURE;
end;

function ps4_sceGnmRegisterResource(pResourceHandle:PInteger; //ResourceHandle
                                    ownerHandle:Integer;      //OwnerHandle
                                    pMemory:Pointer;
                                    sizeInBytes:QWORD;
                                    resourceName:Pchar;
                                    resourceType:Integer;     //ResourceType
                                    userData:Pointer
                                    ):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGnmRegisterResource:',resourceName);
 Result:=SCE_GNM_ERROR_FAILURE;
end;

function ps4_sceGnmUnregisterResource(resourceHandle:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGnmUnregisterResource:',resourceHandle);
 Result:=SCE_GNM_ERROR_FAILURE;
end;

function ps4_sceGnmUnregisterOwnerAndResources(ownerHandle:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceGnmUnregisterOwnerAndResources:',ownerHandle);
 Result:=SCE_GNM_ERROR_FAILURE;
end;

const
 kWorkloadStatusOk                 = 0;
 kWorkloadStatusInvalidStream      = 1;
 kWorkloadStatusInvalidWorkload    = 2;
 kWorkloadStatusInvalidPointer     = 3;
 kWorkloadStatusTooManyStreams     = 4;
 kWorkloadStatusTooManyWorkloads   = 5;
 kWorkloadStatusStreamNotAllocated = 6;
 kWorkloadStatusInternalError      = 7;

function ps4_sceGnmCreateWorkloadStream(name:Pchar;workloadStream:PDWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=kWorkloadStatusInvalidPointer;
 if (name<>nil) and (workloadStream<>nil) then
 begin
  Writeln('sceGnmCreateWorkloadStream:',name);
  workloadStream^:=1;
  Result:=kWorkloadStatusOk;
 end;
end;

function ps4_sceGnmBeginWorkload(stream:DWORD;workload:PQWORD):Integer; SysV_ABI_CDecl;
begin
 if (workload<>nil) then
 begin
  workload^:=QWORD(-DWORD(stream < $10) and 1);
  Exit(DWORD($f < stream));
 end;
 Result:=kWorkloadStatusInvalidPointer;
end;

function ps4_sceGnmEndWorkload(workload:QWORD):Integer; SysV_ABI_CDecl;
begin
 if (workload<>0) then
 begin
  Result:=DWORD($f < DWORD(workload shr $38)) * 2;
 end;
end;

function ps4_sceGnmGetGpuCoreClockFrequency:QWORD; SysV_ABI_CDecl;
begin
 Result:=800000000;
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

 ComputeEvents:array[0..6] of Thamt64locked;

function _sceGnmAddEqEvent(eq:SceKernelEqueue;id:Integer;udata:Pointer):Integer;
var
 pEvents:Phamt64locked;
 P:PPointer;
 node:PKEventNode;
begin
 Writeln('sceGnmAddEqEvent:',id);

 Case id of
  kEqEventCompute0RelMem..kEqEventCompute6RelMem

                        :pEvents:=@ComputeEvents[id];
  kEqEventGfxEop        :pEvents:=@EopEvents;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 pEvents^.LockWr;
 P:=HAMT_search64(@pEvents^.hamt,QWORD(eq));
 if (P<>nil) then
 begin
  node:=P^;
  node^.ev.udata:=udata;
 end else
 begin
  node:=_alloc_kevent_node(eq,SizeOf(TKEventNode));
  if (node=Pointer(1)) then
  begin
   pEvents^.Unlock;
   Exit(SCE_KERNEL_ERROR_EBADF);
  end;
  if (node=nil) then
  begin
   pEvents^.Unlock;
   Exit(SCE_KERNEL_ERROR_ENOMEM);
  end;
  node^.ev.filter:=SCE_KERNEL_EVFILT_GNM;
  node^.ev.flags :=EV_CLEAR;
  node^.ev.data  :=id;
  node^.ev.udata :=udata;
  HAMT_insert64(@pEvents^.hamt,QWORD(eq),node);
 end;
 pEvents^.Unlock;

 Result:=0;
end;

function ps4_sceGnmAddEqEvent(eq:SceKernelEqueue;id:Integer;udata:Pointer):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
  Result:=_sceGnmAddEqEvent(eq,id,udata);
 _sig_unlock;
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
 //Writeln('post_event_eop');
 _sig_lock;
 EopEvents.LockRd;
 HAMT_traverse64(@EopEvents.hamt,@_on_trigger_eop,nil);
 EopEvents.Unlock;
 _sig_unlock;
end;

var
 libGnm_init:Integer=0;

function Load_libSceGnmDriver(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 if CAS(libGnm_init,0,1) then
 begin
  //start load
 end else
 begin
  //never add same lib name
  Exit;
 end;

 lib:=Result._add_lib('libSceGnmDriver');

 lib^.set_proc($21D7DFC1FDF287CB,@ps4_sceGnmDrawInitDefaultHardwareState);
 lib^.set_proc($4219F245EB5E2753,@ps4_sceGnmDrawInitDefaultHardwareState175);
 lib^.set_proc($D07DAF0586D32C72,@ps4_sceGnmDrawInitDefaultHardwareState200);
 lib^.set_proc($C9BD9C4616A00F52,@ps4_sceGnmDrawInitDefaultHardwareState350);
 lib^.set_proc($9C5E9B1515014405,@ps4_sceGnmDispatchInitDefaultHardwareState);
 lib^.set_proc($F251F9E2C7E37E65,@ps4_sceGnmDrawInitToDefaultContextState);
 lib^.set_proc($8A6D99B88B5A6EEE,@ps4_sceGnmDrawInitToDefaultContextState400);

 lib^.set_proc($5B512D8FF8E55BB6,@ps4_sceGnmInsertPushMarker);
 lib^.set_proc($EEA65536012EF926,@ps4_sceGnmInsertPopMarker);
 lib^.set_proc($8E222DCD2EBEDB68,@ps4_sceGnmInsertSetMarker);
 lib^.set_proc($68F2192535C2F9C5,@ps4_sceGnmInsertPushColorMarker);
 lib^.set_proc($6A3DCBFE26859B29,@ps4_sceGnmInsertSetColorMarker);

 lib^.set_proc($D6A5CB1C8A5138F1,@ps4_sceGnmInsertWaitFlipDone);
 lib^.set_proc($29796D9C2C042474,@ps4_sceGnmSetCsShader);

 lib^.set_proc($2B1FE1FE759027C0,@ps4_sceGnmSetCsShaderWithModifier);

 lib^.set_proc($8008429FA5225386,@ps4_sceGnmSetVsShader);
 lib^.set_proc($6D055DE58CC26A5D,@ps4_sceGnmSetPsShader);

 lib^.set_proc($E6E14A7248896113,@ps4_sceGnmSetPsShader350);

 lib^.set_proc($F8016F3845EB2899,@ps4_sceGnmSetEmbeddedVsShader);
 lib^.set_proc($5FD3A6C3D770BF93,@ps4_sceGnmSetEmbeddedPsShader);

 lib^.set_proc($577D55D3552249C6,@ps4_sceGnmUpdateVsShader);
 lib^.set_proc($E0C811C3F6D53505,@ps4_sceGnmUpdatePsShader);
 lib^.set_proc($98B54BECDEC15418,@ps4_sceGnmUpdatePsShader350);

 lib^.set_proc($7050A9D0D5FCC1FD,@ps4_sceGnmSetVgtControl);
 lib^.set_proc($31846D621A2329D0,@ps4_sceGnmResetVgtControl);

 lib^.set_proc($D01CCB1A58DCC01A,@ps4_sceGnmDispatchDirect);

 lib^.set_proc($186B27EE3313C70E,@ps4_sceGnmDrawIndexAuto);
 lib^.set_proc($1E54CFA19FE863B6,@ps4_sceGnmDrawIndex);
 lib^.set_proc($A1833E6337C29B66,@ps4_sceGnmDrawIndexOffset);

 lib^.set_proc($8E0DF7AC428B7D5B,@ps4_sceGnmIsUserPaEnabled);

 lib^.set_proc($8D1708F157204F3E,@ps4_sceGnmSubmitCommandBuffersForWorkload);
 lib^.set_proc($CF0634615F754D32,@ps4_sceGnmSubmitCommandBuffers);
 lib^.set_proc($19AEABEC7E98D112,@ps4_sceGnmSubmitAndFlipCommandBuffersForWorkload);
 lib^.set_proc($C5BC4D6AD6B0A217,@ps4_sceGnmSubmitAndFlipCommandBuffers);
 lib^.set_proc($E98447861E661C2B,@ps4_sceGnmRequestFlipAndSubmitDoneForWorkload);
 lib^.set_proc($80E6CE0E58BF387F,@ps4_sceGnmRequestFlipAndSubmitDone);

 lib^.set_proc($CAF67BDEE414AAB9,@ps4_sceGnmSubmitDone);
 lib^.set_proc($881B7739ED342AF7,@ps4_sceGnmFlushGarlic);

 lib^.set_proc($967DF7CE306B7E39,@ps4_sceGnmGetTheTessellationFactorRingBufferBaseAddress);
 lib^.set_proc($145559702BB7CD65,@ps4_sceGnmGetOffChipTessellationBufferSize);

 lib^.set_proc($8823BCD38660CDD0,@ps4_sceGnmValidateCommandBuffers);
 lib^.set_proc($A863FBE13E4E5897,@ps4_sceGnmValidateDispatchCommandBuffers);
 lib^.set_proc($86C64F7F594E37B1,@ps4_sceGnmValidateDrawCommandBuffers);
 lib^.set_proc($497C387591248290,@ps4_sceGnmValidateDisableDiagnostics);
 lib^.set_proc($060337B772EF70D9,@ps4_sceGnmValidateDisableDiagnostics2);
 lib^.set_proc($457ED708D49A2FA2,@ps4_sceGnmValidateGetDiagnosticInfo);
 lib^.set_proc($E521C63702D7055E,@ps4_sceGnmValidateGetDiagnostics);
 lib^.set_proc($30131AE8416EE0AA,@ps4_sceGnmValidateResetState);
 lib^.set_proc($1F330DEC036A6047,@ps4_sceGnmValidateGetVersion);
 lib^.set_proc($AD3215D759CC42E3,@ps4_sceGnmValidateOnSubmitEnabled);

 lib^.set_proc($7F7DCEAEBB9061B3,@ps4_sceRazorIsLoaded);

 lib^.set_proc($4CB5789ACC226780,@ps4_sceGnmDriverCaptureInProgress);

 lib^.set_proc($6F4C729659D563F2,@ps4_sceGnmAddEqEvent);

 lib^.set_proc($6F4F0082D3E51CF8,@ps4_sceGnmAreSubmitsAllowed);

 lib^.set_proc($78B41B36C29E4E45,@ps4_sceGnmGetResourceRegistrationBuffers);
 lib^.set_proc($645A8A165DB768C7,@ps4_sceGnmRegisterOwner);
 lib^.set_proc($9EF1307D8008993B,@ps4_sceGnmRegisterResource);
 lib^.set_proc($93C11792120FFA53,@ps4_sceGnmUnregisterResource);
 lib^.set_proc($7E12B0095563F679,@ps4_sceGnmUnregisterOwnerAndResources);

 lib^.set_proc($E6E7409BEE9BA158,@ps4_sceGnmCreateWorkloadStream);
 lib^.set_proc($8A1C6B6ECA122967,@ps4_sceGnmBeginWorkload);
 lib^.set_proc($15ADF1EF938E2D10,@ps4_sceGnmEndWorkload);

 lib^.set_proc($170BE1FBE9BD2102,@ps4_sceGnmGetGpuCoreClockFrequency);

  //nop nid:libSceGnmDriver:DBDA0ABCA5F3119A:sceGnmMapComputeQueue

 libGnm_init:=2; //end load
end;

initialization
 GnmInitEmbedded;

 InitCriticalSection(SceSubmitDoneGame);

 EopEvents.Init;
 ComputeEvents[0].Init;
 ComputeEvents[1].Init;
 ComputeEvents[2].Init;
 ComputeEvents[3].Init;
 ComputeEvents[4].Init;
 ComputeEvents[5].Init;
 ComputeEvents[6].Init;

 ps4_app.RegistredPreLoad('libSceGnmDriver.prx'        ,@Load_libSceGnmDriver);
 ps4_app.RegistredPreLoad('libSceGnmDriver_padebug.prx',@Load_libSceGnmDriver);


end.

