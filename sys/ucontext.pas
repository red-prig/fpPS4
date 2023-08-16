unit ucontext;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 signal;

type
 sigcontext=packed record //0x4C0(1216)
  sc_mask:sigset_t; //signal mask to restore =1 if (SS_ONSTACK)
  _align1:array[0..5] of QWORD; //6(qword) 12(int)
  sc_onstack:QWORD; //sigstack state to restore
  sc_rdi:QWORD;     //machine state (struct trapframe)
  sc_rsi:QWORD;
  sc_rdx:QWORD;
  sc_rcx:QWORD;
  sc_r8 :QWORD;
  sc_r9 :QWORD;
  sc_rax:QWORD;
  sc_rbx:QWORD;
  sc_rbp:QWORD;
  sc_r10:QWORD;
  sc_r11:QWORD;
  sc_r12:QWORD;
  sc_r13:QWORD;
  sc_r14:QWORD;
  sc_r15:QWORD;

  sc_trapno:Integer;
  sc_fs:Word;
  sc_gs:Word;

  sc_addr:QWORD;

  sc_flags:Integer; //_MC_HASSEGS
  sc_es:Word;
  sc_ds:Word;

  sc_err   :QWORD; //errno
  sc_rip   :QWORD;
  sc_cs    :QWORD;
  sc_rflags:QWORD; //EFlags
  sc_rsp   :QWORD;
  sc_ss    :QWORD;
  sc_len   :QWORD; //sizeof(mcontext_t)

  {
   * XXX - See <machine/ucontext.h> and <machine/fpu.h> for
   *       the following fields.
  }

  sc_fpformat:QWORD; //_MC_FPFMT_XMM
  sc_ownedfp :QWORD; //_MC_FPOWNED_FPU
  sc_lbrfrom :QWORD; //LastBranchFromRip
  sc_lbrto   :QWORD; //LastBranchToRip
  sc_aux1    :QWORD;
  sc_aux2    :QWORD;

  sc_fpstate:array[0..103] of QWORD; //__aligned(16); =XMM_SAVE_AREA32+XSTATE

  sc_fsbase:QWORD;
  sc_gsbase:QWORD;

  sc_spare:array[0..5] of QWORD; //6(qword) 12(int)
 end;
 {$IF sizeof(sigcontext)<>1216}{$STOP sizeof(sigcontext)<>1216}{$ENDIF}

 p_trapframe=^trapframe;
 trapframe=packed record
  tf_rdi:QWORD;
  tf_rsi:QWORD;
  tf_rdx:QWORD;
  tf_rcx:QWORD;
  tf_r8 :QWORD;
  tf_r9 :QWORD;
  tf_rax:QWORD;
  tf_rbx:QWORD;
  tf_rbp:QWORD;
  tf_r10:QWORD;
  tf_r11:QWORD;
  tf_r12:QWORD;
  tf_r13:QWORD;
  tf_r14:QWORD;
  tf_r15:QWORD;

  tf_trapno:Integer;
  tf_fs:Word;
  tf_gs:Word;

  tf_addr:QWORD;

  tf_flags:Integer;
  tf_es:Word;
  tf_ds:Word;

  tf_err   :QWORD; //errno
  tf_rip   :QWORD;
  tf_cs    :QWORD;
  tf_rflags:QWORD; //EFlags
  tf_rsp   :QWORD;
  tf_ss    :QWORD;
 end;

 p_mcontext_t=^mcontext_t;
 mcontext_t=packed record //0x480(1152)
  mc_onstack:QWORD; //sigstack state to restore  =1 if (SS_ONSTACK)
  mc_rdi:QWORD;     //machine state (struct trapframe)
  mc_rsi:QWORD;
  mc_rdx:QWORD;
  mc_rcx:QWORD;
  mc_r8 :QWORD;
  mc_r9 :QWORD;
  mc_rax:QWORD;
  mc_rbx:QWORD;
  mc_rbp:QWORD;
  mc_r10:QWORD;
  mc_r11:QWORD;
  mc_r12:QWORD;
  mc_r13:QWORD;
  mc_r14:QWORD;
  mc_r15:QWORD;

  mc_trapno:Integer;
  mc_fs:Word;
  mc_gs:Word;

  mc_addr:QWORD;

  mc_flags:Integer; //_MC_HASSEGS
  mc_es:Word;
  mc_ds:Word;

  mc_err   :QWORD; //errno
  mc_rip   :QWORD;
  mc_cs    :QWORD;
  mc_rflags:QWORD; //EFlags
  mc_rsp   :QWORD;
  mc_ss    :QWORD;
  mc_len   :QWORD; //sizeof(mcontext_t)

  {
   * XXX - See <machine/ucontext.h> and <machine/fpu.h> for
   *       the following fields.
  }

  mc_fpformat:QWORD; //_MC_FPFMT_XMM
  mc_ownedfp :QWORD; //_MC_FPOWNED_FPU
  mc_lbrfrom :QWORD; //LastBranchFromRip
  mc_lbrto   :QWORD; //LastBranchToRip
  mc_aux1    :QWORD;
  mc_aux2    :QWORD;

  mc_fpstate:array[0..103] of QWORD; //__aligned(16); =XMM_SAVE_AREA32+XSTATE

  mc_fsbase:QWORD;
  mc_gsbase:QWORD;

  mc_spare:array[0..5] of QWORD; //6(qword) 12(int)
 end;
 {$IF sizeof(mcontext_t)<>1152}{$STOP sizeof(mcontext_t)<>1152}{$ENDIF}

 p_ucontext_t=^ucontext_t;
 ucontext_t=packed record  //size=0x500(1280)

  //[sigcontext]
  uc_sigmask:sigset_t;             //2(qword) 4(int)
  _align1   :array[0..5] of QWORD; //6(qword) 12(int)
  uc_mcontext:mcontext_t;
  //[sigcontext]

  //<-UC_COPY_SIZE=1216
  uc_link:p_ucontext_t; //(not used)
  uc_stack:stack_t;     //
  uc_flags:Integer;     //(not used)

  __spare:array[0..1] of QWORD; //2(qword) 4(int)
  _align2:array[0..2] of DWORD; //3(int)
 end;
 {$IF sizeof(ucontext_t)<>1280}{$STOP sizeof(ucontext_t)<>1280}{$ENDIF}

 p_sigframe=^sigframe;
 sigframe=packed record
  sf_ahu:Pointer;
  sf_uc :ucontext_t;
  sf_si :siginfo_t;
 end;

const
 // used by signal delivery to indicate status of signal stack
 _UC_SETSTACK=$00020000;
 _UC_CLRSTACK=$00040000;

 UCF_SWAPPED=$00000001; // Used by swapcontext(3)

 GET_MC_CLEAR_RET=1;

implementation

end.

