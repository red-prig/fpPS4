unit md_context;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 Windows,
 ntapi,
 ucontext,
 kern_thr,
 kern_proc;

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

 TXmmSaveArea = Windows.TXmmSaveArea;
 PXmmSaveArea = Windows.PXmmSaveArea;

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

procedure teb_set_kernel(td:p_kthread);
procedure teb_set_user  (td:p_kthread);

Function  GetContextSize(ContextFlags:DWORD):QWORD;
function  InitializeContextExtended(data:Pointer;ContextFlags:DWORD):Pointer;

function  _get_ctx_flags(src:p_ucontext_t):DWORD;

procedure _get_fpcontext(src:PCONTEXT;xstate:Pointer);
procedure _set_fpcontext(dst:PCONTEXT;xstate:Pointer);

procedure _get_frame(src:PCONTEXT;dst:p_trapframe;xstate:Pointer);
procedure _set_frame(dst:PCONTEXT;src:p_trapframe;xstate:Pointer);

procedure _get_ucontext(src:PCONTEXT;dst:p_ucontext_t);
procedure _set_ucontext(dst:PCONTEXT;src:p_ucontext_t);

function  md_get_fpcontext(td:p_kthread;mcp:p_mcontext_t;xstate:Pointer):Integer;

procedure md_test_alert;

procedure ipi_sigreturn;
function  ipi_send_cpu(td:p_kthread):Integer;

implementation

uses
 errno,
 systm,
 kern_psl,
 signal;

//

procedure teb_set_kernel(td:p_kthread);
begin
 //teb stack
 if ((td^.pcb_flags and PCB_IS_JIT)=0) then
 begin
  td^.td_teb^.sttop:=td^.td_kstack.sttop;
  td^.td_teb^.stack:=td^.td_kstack.stack;
 end;
 //teb stack
end;

procedure teb_set_user(td:p_kthread);
begin
 //teb stack
 if ((td^.pcb_flags and PCB_IS_JIT)=0) then
 begin
  if (sigonstack(td^.td_frame.tf_rsp)<>0) then
  begin
   td^.td_teb^.stack:=td^.td_sigstk.ss_sp;
   td^.td_teb^.sttop:=td^.td_sigstk.ss_sp-td^.td_sigstk.ss_size;
  end else
  begin
   td^.td_teb^.stack:=td^.td_ustack.stack;
   td^.td_teb^.sttop:=td^.td_ustack.sttop;
  end;
 end;
 //teb stack
end;

const
 _ucodesel=(8 shl 3) or 3;
 _udatasel=(7 shl 3) or 3;
 _ufssel  =(2 shl 3) or 3;
 _ugssel  =(3 shl 3) or 3;

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

function _get_ctx_flags(src:p_trapframe):DWORD;
begin
 Result:=CONTEXT_INTEGER or CONTEXT_CONTROL;
 if ((src^.tf_flags and _MC_HASSEGS)<>0) then
 begin
  Result:=Result or CONTEXT_SEGMENTS;
 end;
 if ((src^.tf_flags and _MC_HASFPXSTATE)<>0) then
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

procedure _get_frame(src:PCONTEXT;dst:p_trapframe;xstate:Pointer);
var
 flags:DWORD;
begin
 if (src=nil) or (dst=nil) then Exit;

 flags:=src^.ContextFlags and (not CONTEXT_AMD64);

 if ((flags and CONTEXT_INTEGER)<>0) then
 begin
  dst^.tf_rax:=src^.Rax;
  dst^.tf_rbx:=src^.Rbx;
  dst^.tf_rcx:=src^.Rcx;
  dst^.tf_rdx:=src^.Rdx;
  dst^.tf_rsi:=src^.Rsi;
  dst^.tf_rdi:=src^.Rdi;
  dst^.tf_r8 :=src^.R8 ;
  dst^.tf_r9 :=src^.R9 ;
  dst^.tf_r10:=src^.R10;
  dst^.tf_r11:=src^.R11;
  dst^.tf_r12:=src^.R12;
  dst^.tf_r13:=src^.R13;
  dst^.tf_r14:=src^.R14;
  dst^.tf_r15:=src^.R15;
  dst^.tf_rbp:=src^.Rbp;
 end;

 if ((flags and CONTEXT_CONTROL)<>0) then
 begin
  dst^.tf_rsp   :=src^.Rsp;
  dst^.tf_rip   :=src^.Rip;
  dst^.tf_rflags:=src^.EFlags;
 end;

 if ((flags and CONTEXT_XSTATE)<>0) then
 begin
  _get_fpcontext(src,xstate);

  dst^.tf_flags:=dst^.tf_flags or _MC_HASFPXSTATE;
 end;
end;

procedure _set_frame(dst:PCONTEXT;src:p_trapframe;xstate:Pointer);
var
 flags:DWORD;
begin
 if (src=nil) or (dst=nil) then Exit;

 flags:=_get_ctx_flags(src);

 flags:=flags and dst^.ContextFlags; //filter
 dst^.ContextFlags:=flags or CONTEXT_AMD64; //update

 if ((flags and CONTEXT_INTEGER)<>0) then
 begin
  dst^.Rax:=src^.tf_rax;
  dst^.Rbx:=src^.tf_rbx;
  dst^.Rcx:=src^.tf_rcx;
  dst^.Rdx:=src^.tf_rdx;
  dst^.Rsi:=src^.tf_rsi;
  dst^.Rdi:=src^.tf_rdi;
  dst^.R8 :=src^.tf_r8;
  dst^.R9 :=src^.tf_r9;
  dst^.R10:=src^.tf_r10;
  dst^.R11:=src^.tf_r11;
  dst^.R12:=src^.tf_r12;
  dst^.R13:=src^.tf_r13;
  dst^.R14:=src^.tf_r14;
  dst^.R15:=src^.tf_r15;
  dst^.Rbp:=src^.tf_rbp;
 end;

 if ((flags and CONTEXT_CONTROL)<>0) then
 begin
  dst^.Rsp   :=src^.tf_rsp;
  dst^.Rip   :=src^.tf_rip;
  dst^.EFlags:=src^.tf_rflags;
 end;

 if ((flags and CONTEXT_FLOATING_POINT)<>0) or
    ((flags and CONTEXT_XSTATE)<>0) then
 begin
  _set_fpcontext(dst,xstate);
 end;
end;

//

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

function cpu_get_iflag(td:p_kthread):PInteger; inline;
begin
 Result:=@td^.td_teb^.iflag;
end;

function IS_TRAP_FUNC(rip:qword):Boolean; external;
function IS_JIT_FUNC (rip:qword):Boolean; external;

function IS_SYSTEM_STACK(td:p_kthread;rsp:qword):Boolean; inline;
begin
 Result:=(rsp<=QWORD(td^.td_kstack.stack)) and (rsp>(QWORD(td^.td_kstack.sttop)));
end;

function IS_SYSCALL(rip:qword):Boolean;
var
 w:Word;
begin
 Result:=False;
 if (rip<>0) then
 begin
  w:=0;
  NtReadVirtualMemory(NtCurrentProcess,@PWord(Rip)[-1],@w,SizeOf(Word),nil);
  Result:=(w=$050F);
 end;
end;

function get_top_mem_td(td:p_kthread;size,align:qword):Pointer;
begin
 Result:=System.Align(td^.td_kstack.sttop,align);

 if (SPtr>td^.td_kstack.sttop) and (SPtr<=td^.td_kstack.stack) then
 begin
  if ((Result+size)>=SPtr) then Exit(nil);
 end;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function md_get_fpcontext(td:p_kthread;mcp:p_mcontext_t;xstate:Pointer):Integer;
var
 td_handle:THandle;
 Context  :PCONTEXT;
begin
 Result   :=0;
 td_handle:=td^.td_handle;

 Context:=get_top_mem_td(td,GetContextSize(CONTEXT_XSTATE),16);
 Assert(Context<>nil);
 Context:=InitializeContextExtended(Context,CONTEXT_XSTATE);

 if (NtGetContextThread(td_handle,Context)<>STATUS_SUCCESS) then
 begin
  Exit(ESRCH)
 end;

 //xmm,ymm
 _get_fpcontext(Context,xstate);

 mcp^.mc_flags   :=mcp^.mc_flags or _MC_HASFPXSTATE;
 mcp^.mc_fpformat:=_MC_FPFMT_XMM;
 mcp^.mc_ownedfp :=_MC_FPOWNED_FPU;
 //xmm,ymm
end;

procedure md_test_alert;
begin
 NtTestAlert();
end;

procedure ipi_sigreturn;
var
 td:p_kthread;
 Context:PCONTEXT;
 regs:p_trapframe;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 //teb stack
 teb_set_kernel(td);
 //teb stack

 regs:=@td^.td_frame;

 if ((regs^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  //xmm,ymm
  Context:=get_top_mem_td(td,GetContextSize(CONTEXT_ALLX),16);
  Assert(Context<>nil);
  Context:=InitializeContextExtended(Context,CONTEXT_ALLX);
 end else
 begin
  //simple
  Context:=get_top_mem_td(td,SizeOf(TCONTEXT),16);
  Assert(Context<>nil);
  Context^:=Default(TCONTEXT);
  Context^.ContextFlags:=CONTEXT_INTEGER or CONTEXT_CONTROL;
 end;

 Context^.Rdi:=regs^.tf_rdi;
 Context^.Rsi:=regs^.tf_rsi;
 Context^.Rdx:=regs^.tf_rdx;
 Context^.Rcx:=regs^.tf_rcx;
 Context^.R8 :=regs^.tf_r8 ;
 Context^.R9 :=regs^.tf_r9 ;
 Context^.Rax:=regs^.tf_rax;
 Context^.Rbx:=regs^.tf_rbx;
 Context^.Rbp:=regs^.tf_rbp;
 Context^.R10:=regs^.tf_r10;
 Context^.R11:=regs^.tf_r11;
 Context^.R12:=regs^.tf_r12;
 Context^.R13:=regs^.tf_r13;
 Context^.R14:=regs^.tf_r14;
 Context^.R15:=regs^.tf_r15;

 Context^.Rip   :=regs^.tf_rip;
 Context^.EFlags:=regs^.tf_rflags;
 Context^.Rsp   :=regs^.tf_rsp;

 Context^.SegGs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegEs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegDs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegCs:=KGDT64_R3_CODE  or RPL_MASK;
 Context^.SegSs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegFs:=KGDT64_R3_CMTEB or RPL_MASK;

 //xmm,ymm
 if ((regs^.tf_flags and TF_HASFPXSTATE)<>0) then
 begin
  _set_fpcontext(Context,@td^.td_fpstate);

  regs^.tf_flags:=regs^.tf_flags and (not TF_HASFPXSTATE);
 end;
 //xmm,ymm

 //teb stack
 teb_set_user(td);
 //teb stack

 NtContinue(Context,False);
end;

procedure sigipi; external;

function ipi_send_cpu(td:p_kthread):Integer;
label
 resume,
 tryagain;
var
 td_handle:THandle;
 iflag    :PInteger;
 Context  :PCONTEXT;
 w:LARGE_INTEGER;
 sf:sigframe;
 sfp:p_sigframe;
 regs:p_trapframe;
 sp:QWORD;
 oonstack:Integer;
begin
 Result   :=0;
 td_handle:=td^.td_handle;
 iflag    :=cpu_get_iflag(td);

 PROC_LOCK;

 tryagain:

 if (NtSuspendThread(td_handle,nil)<>STATUS_SUCCESS) then
 begin
  PROC_UNLOCK;
  Exit(ESRCH);
 end;

 w.QuadPart:=0;
 if (NtWaitForSingleObject(td_handle,False,@w)<>STATUS_TIMEOUT) then
 begin
  Result:=ESRCH;
  goto resume;
 end;

 if ((iflag^ and SIG_ALTERABLE)<>0) then //alterable?
 begin
  NtQueueApcThread(td_handle,@_apc_null,nil,nil,0);
  Result:=0;
  goto resume;
 end else
 if (iflag^<>0) then //locked?
 begin
  Result:=0;
  goto resume;
 end;

 {
 if (td^.td_teb^.jit_rsp<>nil) then //jit call?
 begin
  Result:=0;
  goto resume;
 end;
 }

 Context:=get_top_mem_td(td,GetContextSize(CONTEXT_ALLX),16);
 Assert(Context<>nil);
 Context:=InitializeContextExtended(Context,CONTEXT_ALLX);

 if (NtGetContextThread(td_handle,Context)<>STATUS_SUCCESS) then
 begin
  Result:=ESRCH;
  goto resume;
 end;

 if IS_SYSTEM_STACK(td,Context^.Rsp) or //system?
    IS_TRAP_FUNC(Context^.Rip) or       //syscall func?
    IS_JIT_FUNC (Context^.Rip) then     //syscall func?
 begin
  Result:=0;
  goto resume;
 end;

 if IS_SYSCALL(Context^.Rip) then //system call in code without blocking
 begin
  NtResumeThread(td_handle,nil);
  w.QuadPart:=-10000;
  NtDelayExecution(False,@w); //100ms
  goto tryagain;
 end;

 regs:=@td^.td_frame;
 oonstack:=sigonstack(Context^.Rsp);

 // Save user context.
 sf:=Default(sigframe);

 sf.sf_uc.uc_sigmask:=td^.td_sigmask;
 sf.sf_uc.uc_stack  :=td^.td_sigstk;

 if ((td^.td_pflags and TDP_ALTSTACK)<>0) then
 begin
  if (oonstack<>0) then
  begin
   sf.sf_uc.uc_stack.ss_flags:=SS_ONSTACK;
  end else
  begin
   sf.sf_uc.uc_stack.ss_flags:=0;
  end;
 end else
 begin
  sf.sf_uc.uc_stack.ss_flags:=SS_DISABLE;
 end;

 //copy frame
 regs^.tf_rdi:=Context^.Rdi;
 regs^.tf_rsi:=Context^.Rsi;
 regs^.tf_rdx:=Context^.Rdx;
 regs^.tf_rcx:=Context^.Rcx;
 regs^.tf_r8 :=Context^.R8 ;
 regs^.tf_r9 :=Context^.R9 ;
 regs^.tf_rax:=Context^.Rax;
 regs^.tf_rbx:=Context^.Rbx;
 regs^.tf_rbp:=Context^.Rbp;
 regs^.tf_r10:=Context^.R10;
 regs^.tf_r11:=Context^.R11;
 regs^.tf_r12:=Context^.R12;
 regs^.tf_r13:=Context^.R13;
 regs^.tf_r14:=Context^.R14;
 regs^.tf_r15:=Context^.R15;

 regs^.tf_rip   :=Context^.Rip;
 regs^.tf_rflags:=Context^.EFlags;
 regs^.tf_rsp   :=Context^.Rsp;

 regs^.tf_cs:=_ucodesel;
 regs^.tf_ds:=_udatasel;
 regs^.tf_ss:=_udatasel;
 regs^.tf_es:=_udatasel;
 regs^.tf_fs:=_ufssel;
 regs^.tf_gs:=_ugssel;
 regs^.tf_flags:=TF_HASSEGS;
 //copy frame

 sf.sf_uc.uc_mcontext.mc_onstack:=oonstack;

 Move(regs^.tf_rdi,sf.sf_uc.uc_mcontext.mc_rdi,tf_copy_1);
 Move(regs^.tf_err,sf.sf_uc.uc_mcontext.mc_err,tf_copy_2);

 sf.sf_uc.uc_mcontext.mc_len:=sizeof(mcontext_t);

 sf.sf_uc.uc_mcontext.mc_fsbase:=QWORD(td^.pcb_fsbase);
 sf.sf_uc.uc_mcontext.mc_gsbase:=QWORD(td^.pcb_gsbase);

 sf.sf_uc.uc_mcontext.mc_flags:=_MC_HASSEGS or _MC_HASBASES or _MC_HASFPXSTATE;

 //xmm,ymm
 _get_fpcontext(Context,@sf.sf_uc.uc_mcontext.mc_fpstate);

 sf.sf_uc.uc_mcontext.mc_fpformat:=_MC_FPFMT_XMM;
 sf.sf_uc.uc_mcontext.mc_ownedfp :=_MC_FPOWNED_FPU;
 //xmm,ymm

 sp:=QWORD(td^.td_kstack.stack);

 sp:=sp-sizeof(sigframe);

 sfp:=p_sigframe(sp and (not $1F));

 if (copyout(@sf,sfp,sizeof(sigframe))<>0) then
 begin
  Result:=EFAULT;
  goto resume;
 end;

 Context^.Rsp:=QWORD(sfp);
 Context^.Rip:=QWORD(@sigipi);
 Context^.EFlags:=Context^.EFlags and (not (PSL_T or PSL_D));

 set_pcb_flags(td,PCB_FULL_IRET);

 if (NtSetContextThread(td_handle,Context)<>STATUS_SUCCESS) then
 begin
  Result:=ESRCH;
  goto resume;
 end;

 //teb stack
 teb_set_user(td);
 //teb stack

 resume:
  NtResumeThread(td_handle,nil);
  PROC_UNLOCK;
end;

end.

