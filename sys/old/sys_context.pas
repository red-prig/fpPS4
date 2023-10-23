unit sys_context;

{$mode objfpc}{$H+}

interface

uses
 Windows;

const
 XSTATE_LEGACY_FLOATING_POINT=0;
 XSTATE_LEGACY_SSE           =1;
 XSTATE_GSSE                 =2;
 XSTATE_AVX                  =XSTATE_GSSE;
 XSTATE_MPX_BNDREGS          =3;
 XSTATE_MPX_BNDCSR           =4;
 XSTATE_AVX512_KMASK         =5;
 XSTATE_AVX512_ZMM_H         =6;
 XSTATE_AVX512_ZMM           =7;
 XSTATE_IPT                  =8;
 XSTATE_CET_U                =11;
 XSTATE_LWP                  =62;
 MAXIMUM_XSTATE_FEATURES     =64;

 XSTATE_MASK_LEGACY_FLOATING_POINT=(1 shl XSTATE_LEGACY_FLOATING_POINT);
 XSTATE_MASK_LEGACY_SSE           =(1 shl XSTATE_LEGACY_SSE);
 XSTATE_MASK_LEGACY               =(XSTATE_MASK_LEGACY_FLOATING_POINT or XSTATE_MASK_LEGACY_SSE);
 XSTATE_MASK_GSSE                 =(1 shl XSTATE_GSSE);
 XSTATE_MASK_AVX                  =(XSTATE_MASK_GSSE);

const
 CONTEXT_XSTATE =(CONTEXT_AMD64 or $0040);
 CONTEXT_ALLX   =(CONTEXT_ALL or CONTEXT_XSTATE);

type
 PYMMCONTEXT=^TYMMCONTEXT;
 TYMMCONTEXT=packed record
  Ymm0 :M128A;
  Ymm1 :M128A;
  Ymm2 :M128A;
  Ymm3 :M128A;
  Ymm4 :M128A;
  Ymm5 :M128A;
  Ymm6 :M128A;
  Ymm7 :M128A;
  Ymm8 :M128A;
  Ymm9 :M128A;
  Ymm10:M128A;
  Ymm11:M128A;
  Ymm12:M128A;
  Ymm13:M128A;
  Ymm14:M128A;
  Ymm15:M128A;
 end;

 PXSTATE=^TXSTATE;
 TXSTATE=packed record
  Mask:QWORD;
  CompactionMask:QWORD;
  Reserved:array[0..5] of QWORD;
  YmmContext:TYMMCONTEXT;
 end;

 PCONTEXT_CHUNK=^TCONTEXT_CHUNK;
 TCONTEXT_CHUNK=packed record
  Offset:LONG;
  Length:ULONG;
 end;

 PCONTEXT_EX=^TCONTEXT_EX;
 TCONTEXT_EX=packed record
  All   :TCONTEXT_CHUNK;
  Legacy:TCONTEXT_CHUNK;
  XState:TCONTEXT_CHUNK;
  _align:QWORD;
 end;

 PPCONTEXT=^PCONTEXT;

const
 CONTEXT_STUB_SIZE=4*1024-64; //I hope this will be enough

type
 TCONTEXT_STUB=array[0..CONTEXT_STUB_SIZE-1] of Byte;

 PCONTEXT_EXTENDED=^TCONTEXT_EXTENDED;
 TCONTEXT_EXTENDED=packed record
  CONTEXT:PCONTEXT;
  STUB:TCONTEXT_STUB;
 end;

function GetEnabledXStateFeatures:QWORD; stdcall external 'kernel32';

function InitializeContext(
          Buffer:Pointer;
          ContextFlags:DWORD;
          Context:PPCONTEXT;
          ContextLength:PDWORD
         ):BOOL; stdcall external 'kernel32';

function GetXStateFeaturesMask(
          Context:PCONTEXT;
          FeatureMask:PQWORD
         ):BOOL; stdcall external 'kernel32';

function LocateXStateFeature(
          Context:PCONTEXT;
          FeatureId:DWORD;
          Length:PDWORD
         ):Pointer; stdcall external 'kernel32';

function SetXStateFeaturesMask(
          Context:PCONTEXT;
          FeatureMask:QWORD
         ):BOOL; stdcall external 'kernel32';

function CopyContext(
          Destination:PCONTEXT;
          ContextFlags:DWORD;
          Source:PCONTEXT
         ):BOOL; stdcall external 'kernel32';

Function GetContextXSize:DWORD;
function InitializeContextExtended(lpContext:PCONTEXT_EXTENDED):Boolean;
function CopyContextExtended(src,dst:PCONTEXT_EXTENDED):Boolean;
function IS_SYSCALL(rip:qword):Boolean;

implementation

Function GetContextXSize:DWORD;
begin
 Result:=0;
 InitializeContext(nil,
                   CONTEXT_ALLX,
                   nil,
                   @Result);
end;

function InitializeContextExtended(lpContext:PCONTEXT_EXTENDED):Boolean;
var
 ContextSize:DWORD;
 FeatureMask:QWORD;
begin
 Result:=False;

 if (lpContext=nil) then
 begin
  SetLastError(ERROR_INVALID_PARAMETER);
  Exit(False);
 end;

 ContextSize:=GetContextXSize;
 if (ContextSize=0) then Exit(False);

 if (ContextSize>SizeOf(TCONTEXT_EXTENDED)) then
 begin
  Writeln('Not enough context size:',ContextSize,'>',SizeOf(TCONTEXT_EXTENDED));
  Assert(false);
  SetLastError(ERROR_NOT_ENOUGH_MEMORY);
  Exit(False);
 end;

 lpContext^:=Default(TCONTEXT_EXTENDED);
 if not InitializeContext(@lpContext^.STUB,
                          CONTEXT_ALLX,
                          @lpContext^.CONTEXT,
                          @ContextSize) then
 begin
  Exit(False);
 end;

 FeatureMask:=GetEnabledXStateFeatures;
 if ((FeatureMask and XSTATE_MASK_AVX)<>0) then
  if not SetXStateFeaturesMask(lpContext^.CONTEXT,XSTATE_MASK_AVX) then
  begin
   Exit(False);
  end;

 Result:=True;
end;

function CopyContextExtended(src,dst:PCONTEXT_EXTENDED):Boolean;
begin
 if (src=nil) or (dst=nil) then Exit(False);
 if (src^.CONTEXT=nil) then Exit(False);
 if not InitializeContextExtended(dst) then Exit(False);
 Result:=CopyContext(dst^.CONTEXT,src^.CONTEXT^.ContextFlags,src^.CONTEXT);
end;

function IS_SYSCALL(rip:qword):Boolean;
var
 w:Word;
 n:ptruint;
begin
 Result:=False;
 if (rip<>0) then
 begin
  w:=0;
  ReadProcessMemory(GetCurrentProcess,@PWord(Rip)[-1],@w,SizeOf(Word),@n);
  Result:=(w=$050F);
 end;
end;

end.



