unit trap;

{$mode ObjFPC}{$H+}

interface

uses
 ucontext,
 kern_thread;

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

procedure fast_syscall; assembler;

implementation


procedure fast_syscall; assembler; nostackframe;
asm
 movq %rax,-16(%rsp)  //save rax

 movq %gs:(0x700),%rax            //curkthread
 movq kthread.td_frame(%rax),%rax //td_frame

 movq %rdi,trapframe.tf_rdi(%rax)
 movq %rsi,trapframe.tf_rsi(%rax)
 movq %rdx,trapframe.tf_rdx(%rax)
 movq %rcx,trapframe.tf_rcx(%rax)
 movq %r8 ,trapframe.tf_r8 (%rax)
 movq %r9 ,trapframe.tf_r9 (%rax)
 movq %rbx,trapframe.tf_rbx(%rax)
 movq %rbp,trapframe.tf_rbp(%rax)
 movq %r10,trapframe.tf_r10(%rax)
 movq %r11,trapframe.tf_r11(%rax)
 movq %r12,trapframe.tf_r12(%rax)
 movq %r13,trapframe.tf_r13(%rax)
 movq %r14,trapframe.tf_r14(%rax)
 movq %r15,trapframe.tf_r15(%rax)
 movq %rsp,trapframe.tf_rsp(%rax)

 movq -16(%rsp),%r11 //get rax
 movq %r11,trapframe.tf_rax(%rax)

 pushfq    //push FLAGS
 pop  %r11 //get  FLAGS
 movq %r11,trapframe.tf_rflags(%rax)

 movq (%rsp),%r11 //get caller addr
 movq %r11,trapframe.tf_rip(%rax)

 movqw %fs,trapframe.tf_fs(%rax)
 movqw %gs,trapframe.tf_gs(%rax)
 movqw %es,trapframe.tf_es(%rax)
 movqw %ds,trapframe.tf_ds(%rax)
 movqw %cs,trapframe.tf_cs(%rax)
 movqw %ss,trapframe.tf_ss(%rax)

 movq $0,trapframe.tf_trapno(%rax)
 movq $0,trapframe.tf_addr  (%rax)
 movq $0,trapframe.tf_flags (%rax)
 movq $0,trapframe.tf_err   (%rax)

 movq -16(%rsp),%rax //restore rax

 movq %gs:(0x700),%rsp             //curkthread
 movq kthread.td_kstack(%rsp),%rsp //td_kstack

 push %r11 //rip callstack (debugger)

 call %rax //Call directly to the address or make an ID table?

 //Restore preserved registers.
 movq %gs:(0x700),%rax            //curkthread
 movq kthread.td_frame(%rax),%rax //td_frame

 movq trapframe.tf_rflags(%rax),%r11
 push %r11 //set FLAGS
 popfq     //pop FLAGS

 movq trapframe.tf_rdi(%rax),%rdi
 movq trapframe.tf_rsi(%rax),%rsi
 movq trapframe.tf_rdx(%rax),%rdx
 movq trapframe.tf_rsp(%rax),%rsp

 //restore rip?
 movq trapframe.tf_rip(%rax),%r11
 movq %r11,(%rsp)

 movq trapframe.tf_rsp(%rax),%rax //restore rax
end;

//2: /* AST scheduled. */
//sti
//movq %rsp,%rdi
//call ast
//jmp 1b
//
//3: /* Requested full context restore, use doreti for that. */
//MEXITCOUNT
//jmp doreti





end.

