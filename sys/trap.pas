unit trap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

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

const
 PCB_FULL_IRET=1;

procedure set_pcb_flags(td:p_kthread;f:Integer);
procedure fast_syscall;

implementation

uses
 vm_machdep;

procedure set_pcb_flags(td:p_kthread;f:Integer); inline;
begin
 td^.pcb_flags:=f;
end;

type
 tsyscall=function(rdi,rsi,rdx,rcx,r8,r9:QWORD):Integer;

procedure amd64_syscall;
var
 td:p_kthread;
 td_frame:p_trapframe;
 error:Integer;
begin
 //Call directly to the address or make an ID table?

 td:=curkthread;
 td_frame:=td^.td_frame;

 error:=tsyscall(td_frame^.tf_rax)
                (td_frame^.tf_rdi,
                 td_frame^.tf_rsi,
                 td_frame^.tf_rdx,
                 td_frame^.tf_rcx,
                 td_frame^.tf_r8,
                 td_frame^.tf_r9);

 if ((td^.td_pflags and TDP_NERRNO)=0) then
 begin
  td^.td_errno:=error;
 end;

 cpu_set_syscall_retval(td,error);
end;

const
 NOT_PCB_FULL_IRET=not PCB_FULL_IRET;

procedure fast_syscall; assembler; nostackframe;
label
 doreti;
asm
 //prolog (debugger)
 pushq %rbp
 movq  %rsp,%rbp

 pushf
 lock incl %gs:(0x710)   //lock interrupt
 popf

 movqq %rax,-16(%rsp)  //save rax

 movqq %gs:(0x700),%rax            //curkthread

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%rax) //clear PCB_FULL_IRET

 movqq kthread.td_frame(%rax),%rax //td_frame

 movqq %rdi,trapframe.tf_rdi(%rax)
 movqq %rsi,trapframe.tf_rsi(%rax)
 movqq %rdx,trapframe.tf_rdx(%rax)
 movqq %rcx,trapframe.tf_rcx(%rax)
 movqq %r8 ,trapframe.tf_r8 (%rax)
 movqq %r9 ,trapframe.tf_r9 (%rax)
 movqq %rbx,trapframe.tf_rbx(%rax)
 movqq %r10,trapframe.tf_r10(%rax)
 movqq %r11,trapframe.tf_r11(%rax)
 movqq %r12,trapframe.tf_r12(%rax)
 movqq %r13,trapframe.tf_r13(%rax)
 movqq %r14,trapframe.tf_r14(%rax)
 movqq %r15,trapframe.tf_r15(%rax)

 movqw %fs,trapframe.tf_fs(%rax)
 movqw %gs,trapframe.tf_gs(%rax)
 movqw %es,trapframe.tf_es(%rax)
 movqw %ds,trapframe.tf_ds(%rax)
 movqw %cs,trapframe.tf_cs(%rax)
 movqw %ss,trapframe.tf_ss(%rax)

 movqq $0,trapframe.tf_trapno(%rax)
 movqq $0,trapframe.tf_addr  (%rax)
 movqq $1,trapframe.tf_flags (%rax)
 movqq $5,trapframe.tf_err   (%rax) //sizeof(call $32)

 movqq (%rsp),%r11 //get prev rbp
 movqq %r11,trapframe.tf_rbp(%rax)

 movqq %rsp,%r11
 lea   16(%r11),%r11 //get prev rsp
 movqq %r11,trapframe.tf_rsp(%rax)

 movqq 8(%rsp),%r11 //get prev rip
 movqq %r11,trapframe.tf_rip(%rax)

 movqq -16(%rsp),%r11 //get rax
 movqq %r11,trapframe.tf_rax(%rax)

 pushfq     //push FLAGS
 popq  %r11 //get  FLAGS
 movqq %r11,trapframe.tf_rflags(%rax)

 movqq %gs:(0x700),%rsp             //curkthread
 movqq kthread.td_kstack(%rsp),%rsp //td_kstack

 andq $-32,%rsp //align stack

 call amd64_syscall

 movqq %gs:(0x700),%rax            //curkthread

 //Requested full context restore
 testl	PCB_FULL_IRET,kthread.pcb_flags(%rax)
 jnz	doreti

 movqq kthread.td_frame(%rax),%rax //td_frame

 //Restore preserved registers.
 movqq trapframe.tf_rflags(%rax),%r11
 pushq %r11 //set FLAGS
 popfq      //pop FLAGS

 movqq trapframe.tf_rdi(%rax),%rdi
 movqq trapframe.tf_rsi(%rax),%rsi
 movqq trapframe.tf_rdx(%rax),%rdx

 movqq trapframe.tf_rsp(%rax),%rsp
 lea  -16(%rsp),%rsp //restore rsp

 movqq trapframe.tf_rax(%rax),%rax //restore rax

 pushf
 lock decl %gs:(0x710)   //unlock interrupt
 popf

 //epilog (debugger)
 popq  %rbp
 ret

 //doreti
 doreti:

 movqq kthread.td_frame(%rax),%rax //td_frame

 //Restore full.
 movqq trapframe.tf_rflags(%rax),%r11
 pushq %r11 //set FLAGS
 popfq      //pop FLAGS

 movqq trapframe.tf_rsp(%rax),%rsp //restore stack

 movqq trapframe.tf_rip(%rax),%r11
 pushq %r11 //save rip

 movqq trapframe.tf_rdi(%rax),%rdi
 movqq trapframe.tf_rsi(%rax),%rsi
 movqq trapframe.tf_rdx(%rax),%rdx
 movqq trapframe.tf_rcx(%rax),%rcx
 movqq trapframe.tf_r8 (%rax),%r8
 movqq trapframe.tf_r9 (%rax),%r9
 movqq trapframe.tf_rbx(%rax),%rbx
 movqq trapframe.tf_rbp(%rax),%rbp
 movqq trapframe.tf_r10(%rax),%r10
 movqq trapframe.tf_r11(%rax),%r11
 movqq trapframe.tf_r12(%rax),%r12
 movqq trapframe.tf_r13(%rax),%r13
 movqq trapframe.tf_r14(%rax),%r14
 movqq trapframe.tf_r15(%rax),%r15
 movqq trapframe.tf_rax(%rax),%rax //restore rax

 pushf
 lock decl %gs:(0x710)   //unlock interrupt
 popf

 //restore rip
end;

//testl	$TDF_ASTPENDING | TDF_NEEDRESCHED,TD_FLAGS(%rax)
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

