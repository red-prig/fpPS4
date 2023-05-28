unit md_context;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 ntapi,
 signal,
 ucontext,
 machdep;

const
 XSTATE_LEGACY_FLOATING_POINT=0;
 XSTATE_LEGACY_SSE           =1;
 XSTATE_GSSE                 =2;
 XSTATE_AVX                  =XSTATE_GSSE;

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

Function  GetContextSize(ContextFlags:DWORD):QWORD;
function  InitializeContextExtended(data:Pointer;ContextFlags:DWORD):Pointer;

function  _get_ctx_flags(src:p_ucontext_t):DWORD;

procedure _get_fpcontext(src:PCONTEXT;xstate:Pointer);
procedure _set_fpcontext(dst:PCONTEXT;xstate:Pointer);

procedure _get_ucontext(src:PCONTEXT;dst:p_ucontext_t);
procedure _set_ucontext(dst:PCONTEXT;src:p_ucontext_t);

implementation

function GetEnabledXStateFeatures:QWORD; stdcall external 'kernel32';

function InitializeContext(
          Buffer:Pointer;
          ContextFlags:DWORD;
          Context:Pointer;
          ContextLength:PDWORD
         ):BOOL; stdcall external 'kernel32';

function SetXStateFeaturesMask(
          Context:PCONTEXT;
          FeatureMask:QWORD
         ):BOOL; stdcall external 'kernel32';

Function GetContextSize(ContextFlags:DWORD):QWORD;
begin
 Result:=0;
 InitializeContext(nil,
                   ContextFlags,
                   nil,
                   @Result);
end;

function InitializeContextExtended(data:Pointer;ContextFlags:DWORD):Pointer;
var
 ContextSize:DWORD;
 FeatureMask:QWORD;
begin
 Result:=nil;

 if (data=nil) then Exit;

 ContextSize:=GetContextSize(ContextFlags);
 if (ContextSize=0) then Exit;

 FillChar(data^,ContextSize,0);
 if not InitializeContext(data,
                          ContextFlags,
                          @Result,
                          @ContextSize) then
 begin
  Exit(nil);
 end;

 FeatureMask:=GetEnabledXStateFeatures;

 if ((ContextFlags and $40)<>0) then
 if ((FeatureMask and XSTATE_MASK_AVX)<>0) then
 begin
  if not SetXStateFeaturesMask(Result,XSTATE_MASK_AVX) then
  begin
   Exit(nil);
  end;
 end;
end;

function _get_ctx_flags(src:p_ucontext_t):DWORD;
begin
 Result:=0;
 if ((src^.uc_flags and _UC_CPU)<>0) then
 begin
  Result:=Result or CONTEXT_INTEGER or CONTEXT_CONTROL;
 end;
 if ((src^.uc_mcontext.mc_flags and _MC_HASSEGS)<>0) then
 begin
  Result:=Result or CONTEXT_SEGMENTS;
 end;
 if ((src^.uc_mcontext.mc_flags and _MC_HASFPXSTATE)<>0) then
 begin
  Result:=Result or CONTEXT_FLOATING_POINT or CONTEXT_XSTATE;
 end;
 Result:=Result and (not CONTEXT_AMD64);
end;

procedure _get_fpcontext(src:PCONTEXT;xstate:Pointer);
var
 context_ex:PCONTEXT_EX;
 xs:PXSTATE;

 uc_xsave :PXmmSaveArea;
 uc_xstate:PXSTATE;
begin
 if (src=nil) or (xstate=nil) then Exit;

 context_ex:=PCONTEXT_EX(src+1);
 xs:=PXSTATE(PByte(context_ex)+context_ex^.XState.Offset);

 uc_xsave :=PXmmSaveArea(xstate);
 uc_xstate:=PXSTATE(uc_xsave+1);

  uc_xsave^:=src^.FltSave;
 uc_xstate^:=xs^;
end;

procedure _set_fpcontext(dst:PCONTEXT;xstate:Pointer);
var
 context_ex:PCONTEXT_EX;
 xs:PXSTATE;

 uc_xsave :PXmmSaveArea;
 uc_xstate:PXSTATE;
begin
 if (dst=nil) or (xstate=nil) then Exit;

 context_ex:=PCONTEXT_EX(dst+1);
 xs:=PXSTATE(PByte(context_ex)+context_ex^.XState.Offset);

 uc_xsave :=PXmmSaveArea(xstate);
 uc_xstate:=PXSTATE(uc_xsave+1);

 dst^.FltSave:=uc_xsave^;
          xs^:=uc_xstate^;
end;

procedure _get_ucontext(src:PCONTEXT;dst:p_ucontext_t);
var
 flags:DWORD;
begin
 if (src=nil) or (dst=nil) then Exit;

 flags:=src^.ContextFlags and (not CONTEXT_AMD64);

 if ((flags and CONTEXT_INTEGER)<>0) then
 begin
  dst^.uc_flags:=dst^.uc_flags or _UC_CPU;

  dst^.uc_mcontext.mc_rax:=src^.Rax;
  dst^.uc_mcontext.mc_rbx:=src^.Rbx;
  dst^.uc_mcontext.mc_rcx:=src^.Rcx;
  dst^.uc_mcontext.mc_rdx:=src^.Rdx;
  dst^.uc_mcontext.mc_rsi:=src^.Rsi;
  dst^.uc_mcontext.mc_rdi:=src^.Rdi;
  dst^.uc_mcontext.mc_r8 :=src^.R8 ;
  dst^.uc_mcontext.mc_r9 :=src^.R9 ;
  dst^.uc_mcontext.mc_r10:=src^.R10;
  dst^.uc_mcontext.mc_r11:=src^.R11;
  dst^.uc_mcontext.mc_r12:=src^.R12;
  dst^.uc_mcontext.mc_r13:=src^.R13;
  dst^.uc_mcontext.mc_r14:=src^.R14;
  dst^.uc_mcontext.mc_r15:=src^.R15;
  dst^.uc_mcontext.mc_rbp:=src^.Rbp;
 end;

 if ((flags and CONTEXT_CONTROL)<>0) then
 begin
  dst^.uc_flags:=dst^.uc_flags or _UC_CPU;

  dst^.uc_mcontext.mc_rsp   :=src^.Rsp;
  dst^.uc_mcontext.mc_rip   :=src^.Rip;
  dst^.uc_mcontext.mc_rflags:=src^.EFlags;
  dst^.uc_mcontext.mc_cs    :=_ucodesel;
  dst^.uc_mcontext.mc_ss    :=_udatasel;
 end;

 if ((flags and CONTEXT_SEGMENTS)<>0) then
 begin
  dst^.uc_mcontext.mc_flags:=dst^.uc_mcontext.mc_flags or _MC_HASSEGS;

  dst^.uc_mcontext.mc_ds:=_udatasel;
  dst^.uc_mcontext.mc_es:=_udatasel;
  dst^.uc_mcontext.mc_fs:=_ufssel;
  dst^.uc_mcontext.mc_gs:=_ugssel;
 end;

 if ((flags and CONTEXT_XSTATE)<>0) then
 begin
  _get_fpcontext(src,@dst^.uc_mcontext.mc_fpstate);

  dst^.uc_mcontext.mc_fpformat:=_MC_FPFMT_XMM;
  dst^.uc_mcontext.mc_ownedfp :=_MC_FPOWNED_FPU;

  dst^.uc_mcontext.mc_flags:=dst^.uc_mcontext.mc_flags or _MC_HASFPXSTATE;
 end;

 dst^.uc_mcontext.mc_len:=SizeOf(mcontext_t);
end;

procedure _set_ucontext(dst:PCONTEXT;src:p_ucontext_t);
var
 flags:DWORD;
begin
 if (src=nil) or (dst=nil) then Exit;

 flags:=_get_ctx_flags(src);

 flags:=flags and dst^.ContextFlags; //filter
 dst^.ContextFlags:=flags or CONTEXT_AMD64; //update

 if ((flags and CONTEXT_INTEGER)<>0) then
 begin
  dst^.Rax:=src^.uc_mcontext.mc_rax;
  dst^.Rbx:=src^.uc_mcontext.mc_rbx;
  dst^.Rcx:=src^.uc_mcontext.mc_rcx;
  dst^.Rdx:=src^.uc_mcontext.mc_rdx;
  dst^.Rsi:=src^.uc_mcontext.mc_rsi;
  dst^.Rdi:=src^.uc_mcontext.mc_rdi;
  dst^.R8 :=src^.uc_mcontext.mc_r8;
  dst^.R9 :=src^.uc_mcontext.mc_r9;
  dst^.R10:=src^.uc_mcontext.mc_r10;
  dst^.R11:=src^.uc_mcontext.mc_r11;
  dst^.R12:=src^.uc_mcontext.mc_r12;
  dst^.R13:=src^.uc_mcontext.mc_r13;
  dst^.R14:=src^.uc_mcontext.mc_r14;
  dst^.R15:=src^.uc_mcontext.mc_r15;
  dst^.Rbp:=src^.uc_mcontext.mc_rbp;
 end;

 if ((flags and CONTEXT_CONTROL)<>0) then
 begin
  dst^.Rsp   :=src^.uc_mcontext.mc_rsp;
  dst^.Rip   :=src^.uc_mcontext.mc_rip;
  dst^.EFlags:=src^.uc_mcontext.mc_rflags;

  dst^.SegCs :=KGDT64_R3_CODE  or RPL_MASK;
  dst^.SegSs :=KGDT64_R3_DATA  or RPL_MASK;
 end;

 if ((flags and CONTEXT_SEGMENTS)<>0) then
 begin
  dst^.SegDs:=KGDT64_R3_DATA  or RPL_MASK;
  dst^.SegEs:=KGDT64_R3_DATA  or RPL_MASK;
  dst^.SegFs:=KGDT64_R3_CMTEB or RPL_MASK;
  dst^.SegGs:=KGDT64_R3_DATA  or RPL_MASK;
 end;

 if ((flags and CONTEXT_FLOATING_POINT)<>0) or
    ((flags and CONTEXT_XSTATE)<>0) then
 begin
  _set_fpcontext(dst,@src^.uc_mcontext.mc_fpstate);
 end;

end;

end.

