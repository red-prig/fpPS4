unit trap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 ucontext,
 kern_thr;

const
 T_PRIVINFLT = 1; // privileged instruction
 T_BPTFLT    = 3; // breakpoint instruction
 T_ARITHTRAP = 6; // arithmetic trap
 T_PROTFLT   = 9; // protection fault
 T_TRCTRAP   =10; // debug exception (sic)
 T_PAGEFLT   =12; // page fault
 T_ALIGNFLT  =14; // alignment fault

 T_DIVIDE    =18; // integer divide fault
 T_NMI       =19; // non-maskable trap
 T_OFLOW     =20; // overflow trap
 T_BOUND     =21; // bound instruction fault
 T_DNA       =22; // device not available fault
 T_DOUBLEFLT =23; // double fault
 T_FPOPFLT   =24; // fp coprocessor operand fetch fault
 T_TSSFLT    =25; // invalid tss fault
 T_SEGNPFLT  =26; // segment not present fault
 T_STKFLT    =27; // stack fault
 T_MCHK      =28; // machine check trap
 T_XMMFLT    =29; // SIMD floating-point exception
 T_RESERVED  =30; // reserved (unknown)
 T_DTRACE_RET=32; // DTrace pid return

  // XXX most of the following codes aren't used, but could be.

  // definitions for <sys/signal.h>
 ILL_PRIVIN_FAULT=T_PRIVINFLT;
 ILL_ALIGN_FAULT =T_ALIGNFLT ;
 ILL_FPOP_FAULT  =T_FPOPFLT  ; // coprocessor operand fault

  // codes for SIGBUS
 BUS_PAGE_FAULT =T_PAGEFLT ; // page fault protection base
 BUS_SEGNP_FAULT=T_SEGNPFLT; // segment not present
 BUS_STK_FAULT  =T_STKFLT  ; // stack segment
 BUS_SEGM_FAULT =T_RESERVED; // segment protection base

  // Trap's coming from user mode
 T_USER=$100;

 MAX_TRAP_MSG=32;

 trap_msg:array[0..MAX_TRAP_MSG] of PChar=(
  '',                              //  0 unused
  'privileged instruction fault',  //  1 T_PRIVINFLT
  '',                              //  2 unused
  'breakpoint instruction fault',  //  3 T_BPTFLT
  '',                              //  4 unused
  '',                              //  5 unused
  'arithmetic trap',               //  6 T_ARITHTRAP
  '',                              //  7 unused
  '',                              //  8 unused
  'general protection fault',      //  9 T_PROTFLT
  'trace trap',                    // 10 T_TRCTRAP
  '',                              // 11 unused
  'page fault',                    // 12 T_PAGEFLT
  '',                              // 13 unused
  'alignment fault',               // 14 T_ALIGNFLT
  '',                              // 15 unused
  '',                              // 16 unused
  '',                              // 17 unused
  'integer divide fault',          // 18 T_DIVIDE
  'non-maskable interrupt trap',   // 19 T_NMI
  'overflow trap',                 // 20 T_OFLOW
  'FPU bounds check fault',        // 21 T_BOUND
  'FPU device not available',      // 22 T_DNA
  'double fault',                  // 23 T_DOUBLEFLT
  'FPU operand fetch fault',       // 24 T_FPOPFLT
  'invalid TSS fault',             // 25 T_TSSFLT
  'segment not present fault',     // 26 T_SEGNPFLT
  'stack fault',                   // 27 T_STKFLT
  'machine check trap',            // 28 T_MCHK
  'SIMD floating-point exception', // 29 T_XMMFLT
  'reserved (unknown) fault',      // 30 T_RESERVED
  '',                              // 31 unused (reserved)
  'DTrace pid return trap'         // 32 T_DTRACE_RET
 );

const
 PCB_FULL_IRET=1;

 SIG_ALTERABLE=$80000000;
 SIG_STI_LOCK =$40000000;

procedure set_pcb_flags(td:p_kthread;f:Integer);

procedure _sig_lock;
procedure _sig_unlock;

procedure sig_lock;
procedure sig_unlock;

procedure sig_sta;
procedure sig_cla;

procedure sig_sti;
procedure sig_cli;

procedure fast_syscall;
procedure sigcode;
procedure sigipi;

implementation

uses
 errno,
 vmparam,
 machdep,
 md_context,
 kern_sig,
 sysent;

const
 NOT_PCB_FULL_IRET=not PCB_FULL_IRET;
 NOT_SIG_ALTERABLE=not SIG_ALTERABLE;
 NOT_SIG_STI_LOCK =not SIG_STI_LOCK;
 TDF_AST=TDF_ASTPENDING or TDF_NEEDRESCHED;

procedure _sig_lock; assembler; nostackframe;
asm
 pushf
 lock incl %gs:teb.iflag   //lock interrupt
 popf
end;

procedure _sig_unlock; assembler; nostackframe;
asm
 pushf
 lock decl %gs:teb.iflag   //unlock interrupt
 popf
end;

procedure sig_lock; assembler; nostackframe;
label
 _exit;
asm
 //prolog (debugger)
 pushq %rbp
 movq  %rsp,%rbp
 pushq %rax
 pushf

 movq $1,%rax
 lock xadd %rax,%gs:teb.iflag //lock interrupt
 test %rax,%rax
 jnz _exit

 movqq %gs:teb.thread,%rax     //curkthread
 testl TDF_AST,kthread.td_flags(%rax)
 je _exit

 mov  $0,%rax
 call fast_syscall

 _exit:
 //epilog (debugger)
 popf
 popq  %rax
 popq  %rbp
end;

procedure sig_unlock; assembler; nostackframe;
label
 _exit;
asm
 //prolog (debugger)
 pushq %rbp
 movq  %rsp,%rbp
 pushq %rax
 pushf

 lock decl %gs:teb.iflag   //unlock interrupt
 jnz _exit

 movqq %gs:teb.thread,%rax  //curkthread
 testl TDF_AST,kthread.td_flags(%rax)
 je _exit

 mov  $0,%rax
 call fast_syscall

 _exit:
 //epilog (debugger)
 popf
 popq  %rax
 popq  %rbp
end;

procedure sig_sta; assembler; nostackframe;
asm
 lock orl SIG_ALTERABLE,%gs:teb.iflag
end;

procedure sig_cla; assembler; nostackframe;
asm
 lock andl NOT_SIG_ALTERABLE,%gs:teb.iflag
end;

procedure sig_sti; assembler; nostackframe;
asm
 lock orl SIG_STI_LOCK,%gs:teb.iflag
end;

procedure sig_cli; assembler; nostackframe;
asm
 lock andl NOT_SIG_STI_LOCK,%gs:teb.iflag
end;

procedure set_pcb_flags(td:p_kthread;f:Integer);
begin
 td^.pcb_flags:=f;
end;

type
 tsyscall=function(rdi,rsi,rdx,rcx,r8,r9:QWORD):Integer;

procedure amd64_syscall;
var
 td:p_kthread;
 td_frame:p_trapframe;
 scall:tsyscall;
 error:Integer;
begin
 //Call directly to the address or make an ID table?

 td:=curkthread;
 td_frame:=@td^.td_frame;

 cpu_fetch_syscall_args(td);

 error:=0;
 scall:=nil;

 if (td_frame^.tf_rax<=High(sysent_table)) then
 begin
  scall:=tsyscall(sysent_table[td_frame^.tf_rax].sy_call);
  if (scall=nil) then
  begin
   Writeln('Unhandled syscall:',td_frame^.tf_rax,':',sysent_table[td_frame^.tf_rax].sy_name);

   Writeln(' [1]:0x',HexStr(td_frame^.tf_rdi,16));
   Writeln(' [2]:0x',HexStr(td_frame^.tf_rsi,16));
   Writeln(' [3]:0x',HexStr(td_frame^.tf_rdx,16));
   Writeln(' [4]:0x',HexStr(td_frame^.tf_r10,16));
   Writeln(' [5]:0x',HexStr(td_frame^.tf_r8 ,16));
   Writeln(' [6]:0x',HexStr(td_frame^.tf_r9 ,16));

   Assert(false,sysent_table[td_frame^.tf_rax].sy_name);
  end;
 end else
 if (td_frame^.tf_rax<=$1000) then
 begin
  Writeln('Unhandled syscall:',td_frame^.tf_rax);

  Writeln(' [1]:0x',HexStr(td_frame^.tf_rdi,16));
  Writeln(' [2]:0x',HexStr(td_frame^.tf_rsi,16));
  Writeln(' [3]:0x',HexStr(td_frame^.tf_rdx,16));
  Writeln(' [4]:0x',HexStr(td_frame^.tf_r10,16));
  Writeln(' [5]:0x',HexStr(td_frame^.tf_r8 ,16));
  Writeln(' [6]:0x',HexStr(td_frame^.tf_r9 ,16));

  Assert(false,IntToStr(td_frame^.tf_rax));
 end else
 begin
  scall:=tsyscall(td_frame^.tf_rax);
 end;

 if (scall=nil) then
 begin
  error:=ENOSYS;
 end else
 begin
  if (td_frame^.tf_rax<=High(sysent_table)) then
  if is_guest_addr(td_frame^.tf_rip) then
  begin
   Writeln('Guest syscall:',sysent_table[td_frame^.tf_rax].sy_name);
   Writeln(' [1]:0x',HexStr(td_frame^.tf_rdi,16));
   Writeln(' [2]:0x',HexStr(td_frame^.tf_rsi,16));
   Writeln(' [3]:0x',HexStr(td_frame^.tf_rdx,16));
   Writeln(' [4]:0x',HexStr(td_frame^.tf_r10,16));
   Writeln(' [5]:0x',HexStr(td_frame^.tf_r8 ,16));
   Writeln(' [6]:0x',HexStr(td_frame^.tf_r9 ,16));
  end;

  error:=scall(td_frame^.tf_rdi,
               td_frame^.tf_rsi,
               td_frame^.tf_rdx,
               td_frame^.tf_r10,
               td_frame^.tf_r8 ,
               td_frame^.tf_r9 );

 end;

 if ((td^.td_pflags and TDP_NERRNO)=0) then
 begin
  td^.td_errno:=error;
 end;

 cpu_set_syscall_retval(td,error);
end;

procedure fast_syscall; assembler; nostackframe;
label
 _after_call,
 _doreti,
 _fail,
 _ast,
 _doreti_exit;
asm
 //prolog (debugger)
 pushq %rbp
 movq  %rsp,%rbp

 movqq %rax,%r11 //save rax
 movqq %rcx,%r10 //save rcx

 lahf  //load to AH
 shr   $8,%rax
 andl  $0xFF,%rax //filter
 movqq %rax,%rcx  //save flags

 movqq %gs:teb.thread,%rax //curkthread
 test  %rax,%rax
 jz    _fail

 movqq kthread.td_kstack.stack(%rax),%rsp //td_kstack (Implicit lock interrupt)
 andq  $-32,%rsp //align stack

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%rax) //clear PCB_FULL_IRET

 movqq %rdi,kthread.td_frame.tf_rdi   (%rax)
 movqq %rsi,kthread.td_frame.tf_rsi   (%rax)
 movqq %rdx,kthread.td_frame.tf_rdx   (%rax)
 movqq   $0,kthread.td_frame.tf_rcx   (%rax)
 movqq %r8 ,kthread.td_frame.tf_r8    (%rax)
 movqq %r9 ,kthread.td_frame.tf_r9    (%rax)
 movqq %r11,kthread.td_frame.tf_rax   (%rax)
 movqq %rbx,kthread.td_frame.tf_rbx   (%rax)
 movqq %r10,kthread.td_frame.tf_r10   (%rax)
 movqq   $0,kthread.td_frame.tf_r11   (%rax)
 movqq %r12,kthread.td_frame.tf_r12   (%rax)
 movqq %r13,kthread.td_frame.tf_r13   (%rax)
 movqq %r14,kthread.td_frame.tf_r14   (%rax)
 movqq %r15,kthread.td_frame.tf_r15   (%rax)
 movqq %rcx,kthread.td_frame.tf_rflags(%rax)

 movqq $0  ,kthread.td_frame.tf_trapno(%rax)
 movqq $0  ,kthread.td_frame.tf_addr  (%rax)
 movqq $0  ,kthread.td_frame.tf_flags (%rax)
 movqq $5  ,kthread.td_frame.tf_err   (%rax) //sizeof(call $32)

 movqq (%rbp),%r11 //get prev rbp
 movqq %r11,kthread.td_frame.tf_rbp(%rax)

 lea   16(%rbp),%r11 //get prev rsp
 movqq %r11,kthread.td_frame.tf_rsp(%rax)

 movqq 8(%rbp),%r11 //get prev rip
 movqq %r11,kthread.td_frame.tf_rip(%rax)

 call amd64_syscall

 _after_call:

 movqq %gs:teb.thread,%rcx          //curkthread

 //Requested full context restore
 testl PCB_FULL_IRET,kthread.pcb_flags(%rcx)
 jnz _doreti

 testl TDF_AST,kthread.td_flags(%rcx)
 jne _ast

 //Restore preserved registers.
 movqq kthread.td_frame.tf_rflags(%rcx),%rax
 shl   $8,%rax
 sahf  //restore flags

 movqq kthread.td_frame.tf_rdi(%rcx),%rdi
 movqq kthread.td_frame.tf_rsi(%rcx),%rsi
 movqq kthread.td_frame.tf_rdx(%rcx),%rdx
 movqq kthread.td_frame.tf_rax(%rcx),%rax

 movqq kthread.td_frame.tf_rsp(%rcx),%r11
 lea  -16(%r11),%r11

 movqq %r11,%rsp //restore rsp (Implicit unlock interrupt)

 movqq $0,%rcx
 movqq $0,%r11

 //epilog (debugger)
 popq  %rbp
 ret

 //fail:
 _fail:

 movqq %rcx,%rax //get flags
 shl   $8,%rax
 or    $1,%ah //CF
 sahf  //restore flags

 movqq $14,%rax //EFAULT
 movqq  $0,%rdx
 movqq  $0,%rcx
 movqq  $0,%r11

 popq  %rbp
 ret

 //ast
 _ast:

 call ast
 jmp _after_call

 //doreti
 _doreti:

  //%rcx=curkthread
  testl TDF_AST,kthread.td_flags(%rcx)
  je _doreti_exit

  call ast
  jmp _doreti

 _doreti_exit:

  //Restore full.
  call  ipi_sigreturn
  hlt
end;

procedure sigcode; assembler; nostackframe;
asm
 call  sigframe.sf_ahu(%rsp)
 lea   sigframe.sf_uc(%rsp),%rdi
 pushq $0
 movqq sys_sigreturn,%rax
 call  fast_syscall
 hlt
end;

procedure sigipi; assembler; nostackframe;
label
 _ast,
 _ast_exit;
asm
 lea   sigframe.sf_uc(%rsp),%rdi
 call  sys_sigreturn

 //ast
 _ast:

  movqq %gs:teb.thread,%rax           //curkthread
  testl TDF_AST,kthread.td_flags(%rax)
  je _ast_exit

  call ast
  jmp _ast

 _ast_exit:
  call  ipi_sigreturn
  hlt
end;


end.

