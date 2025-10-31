unit kern_jit_asm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 ucontext,
 x86_fpdbgdisas;

{$DEFINE USE_XSAVE}

{
change: rsp,rbp,rip

eflahs: temp change

change: push/pop

frame:  r13

temp:   r14,r15
}

const
 jit_frame_offset:Integer=Integer(@kthread(nil^).td_frame)+Integer(@trapframe(nil^).tf_r13);

type
 //kthread.td_frame.tf_r13

 p_jit_frame=^jit_frame;
 jit_frame=packed record
  tf__00:QWORD;      //00 (tf_r13)
  tf_r14:QWORD;      //08
  tf_r15:QWORD;      //10
  tf_r13:QWORD;      //18 (tf_trapno)
  tf_adr:QWORD;      //20 (tf_addr)
  tf__28:QWORD;      //28 (tf_flags)
  tf_rsp:QWORD;      //30 (tf_BrF)
  tf_rbp:QWORD;      //38 (tf_BrT)
  tf_err:QWORD;      //40 (tf_err)
  tf_rip:QWORD;      //48 (tf_rip)
 end;

 p_jplt_cache_asm=^t_jplt_cache_asm;
 t_jplt_cache_asm=object
  plt:Pointer;
  src:Pointer;
  neg:Pointer; //(-src)
  dst:Pointer;
 end;

procedure jit_syscall;       assembler;
procedure jit_jmp_plt_cache; assembler;
procedure jit_jmp_dispatch;  assembler;

procedure jit_hle_trace; assembler;

procedure jit_jmp_internal;  assembler;

function  GET_JIT_FUNC(rip:qword):Byte;

procedure jit_save_ctx;
procedure jit_load_ctx;

function  get_jit_ctx_state(td_frame:p_trapframe):Boolean;
procedure set_jit_ctx_state(td_frame:p_trapframe;state:Boolean);

procedure jit_cpuid; assembler;

procedure strict_ps4_rdtsc_jit;  assembler;
procedure strict_ps4_rdtscp_jit; assembler;

procedure jit_interrupt_nop; assembler;

implementation

uses
 time,
 md_time,
 trap,
 md_context,
 signal,
 sys_bootparam,
 subr_backtrace;

//

function jmp_dispatcher(addr,plt,from:Pointer):Pointer; external;

//

//output:
//rax: (ret)  rbx: (rbx)
//rcx: (rip)  rdx: (rdx/ret)
//rsi: (rsi)  rdi: (rdi)
//rbp: (rbp)  rsp: (rsp)
//r8 : (0)    r9 : (0)
//r10: (0)    r11: word(eflags) CF->error
//r12: (r12)  r13: (r13)
//r14: (r14)  r15: (r15)
//rip: (rip)  eflags: (eflags)  CF->error

//in:tf_rip
procedure jit_syscall; assembler; nostackframe;
label
 _after_call,
 _doreti,
 _fail,
 _ast,
 _doreti_exit;
asm
 //prolog (debugger)
 pushq %rbp
 movqq %rsp,%rbp

 pushf
 pop %r14

 movqq %gs:teb.thread,%r15 //curkthread
 test  %r15,%r15
 jz    _fail

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%r15) //clear PCB_FULL_IRET

 andq  $-16,%rsp //align stack

 movqq %r14,kthread.td_frame.tf_rflags(%r15) //save flags

 movqq %rdi,kthread.td_frame.tf_rdi(%r15)
 movqq %rsi,kthread.td_frame.tf_rsi(%r15)
 movqq %rdx,kthread.td_frame.tf_rdx(%r15)
 movqq %r8 ,kthread.td_frame.tf_r8 (%r15)
 movqq %r9 ,kthread.td_frame.tf_r9 (%r15)
 movqq %rax,kthread.td_frame.tf_rax(%r15)
 movqq %rbx,kthread.td_frame.tf_rbx(%r15)
 movqq %r10,kthread.td_frame.tf_r10(%r15)
 movqq   $0,kthread.td_frame.tf_r11(%r15)
 movqq %r12,kthread.td_frame.tf_r12(%r15)

 //tf_rcx <- tf_rip
 movqq      kthread.td_frame.tf_rip(%r15),%r14
 movqq %r14,kthread.td_frame.tf_rcx(%r15)

 //tf_r14=tf_r14
 //tf_r15=tf_r15

 //tf_r13
 movqq             jit_frame.tf_r13(%r13),%r14
 movqq %r14,kthread.td_frame.tf_r13(%r15)

 //tf_rsp
 movqq             jit_frame.tf_rsp(%r13),%r14
 movqq %r14,kthread.td_frame.tf_rsp(%r15)

 //tf_rbp
 movqq             jit_frame.tf_rbp(%r13),%r14
 movqq %r14,kthread.td_frame.tf_rbp(%r15)

 movqq   $1,kthread.td_frame.tf_trapno(%r15)
 movqq   $0,kthread.td_frame.tf_addr  (%r15)
 movqq   $0,kthread.td_frame.tf_flags (%r15)
 movqq   $2,kthread.td_frame.tf_err   (%r15) //sizeof(syscall)

 call amd64_syscall

 _after_call:

 movq %gs:teb.thread               ,%r15 //curkthread
 leaq kthread.td_frame.tf_r13(%r15),%r13 //jit_frame

 //Requested full context restore
 testl PCB_FULL_IRET,kthread.pcb_flags(%r15)
 jnz _doreti

 testl TDF_AST,kthread.td_flags(%r15)
 jne _ast

 //Restore preserved registers.

 //get flags
 movqq kthread.td_frame.tf_rflags(%r15),%r14
 push %r14
 popf

 movzwq %r14w,%r11 //r11 <- tf_rflags

 movqq kthread.td_frame.tf_rdi(%r15),%rdi
 movqq kthread.td_frame.tf_rsi(%r15),%rsi
 movqq kthread.td_frame.tf_rdx(%r15),%rdx
 movqq kthread.td_frame.tf_rax(%r15),%rax
 movqq kthread.td_frame.tf_rcx(%r15),%rcx

 //tf_r14=tf_r14
 //tf_r15=tf_r15

 //tf_r13
 movqq kthread.td_frame.tf_r13(%r15),%r14
 movqq   %r14,jit_frame.tf_r13(%r13)

 //tf_rsp
 movqq kthread.td_frame.tf_rsp(%r15),%r14
 movqq   %r14,jit_frame.tf_rsp(%r13)

 //tf_rbp
 movqq kthread.td_frame.tf_rbp(%r15),%r14
 movqq   %r14,jit_frame.tf_rbp(%r13)

 movqq $0,%r8
 movqq $0,%r9
 movqq $0,%r10

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 //interrupt/ret
 jmp %gs:teb.jit_trp

 //fail (curkthread=nil)
 _fail:

 or $1,%r14 //set CF
 push  %r14
 popf

 movzwq %r14w,%r11 //r11 <- tf_rflags

 movqq $14,%rax //EFAULT
 movqq  $0,%rcx //rip is unknow

 movqq $0,%r8
 movqq $0,%r9
 movqq $0,%r10

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 //interrupt/ret
 jmp %gs:teb.jit_trp

 //ast
 _ast:

  call ast
  jmp _after_call

 //doreti
 _doreti:

  //%r15=curkthread
  testl TDF_AST,kthread.td_flags(%r15)

  //interrupt guard set
  movq $1,%gs:teb.iflag

  je _doreti_exit

  //interrupt guard clear
  movq $0,%gs:teb.iflag

  call ast
  jmp _doreti

 _doreti_exit:

  //Restore full.
  call  ipi_sigreturn
  hlt

 //marker
 .globl .endof_jit_syscall
end;

procedure jit_ctx_to_sys_ctx(td_frame:p_trapframe); inline;
var
 frame:p_jit_frame;
begin
 if ((td_frame^.tf_flags and TF_JIT_CTX)<>0) then
 begin
  frame:=@td_frame^.tf_r13;

  //tf_rip ?????

  //tf_r14 not need to move
  //tf_r15 not need to move

  td_frame^.tf_r13:=frame^.tf_r13;
  td_frame^.tf_rsp:=frame^.tf_rsp;
  td_frame^.tf_rbp:=frame^.tf_rbp;

  td_frame^.tf_trapno:=0;
  td_frame^.tf_BrF   :=0;
  td_frame^.tf_BrT   :=0;

  td_frame^.tf_flags:=td_frame^.tf_flags and (not TF_JIT_CTX);
 end;
end;

procedure sys_ctx_to_jit_ctx(td_frame:p_trapframe); inline;
var
 frame:p_jit_frame;
begin
 if ((td_frame^.tf_flags and TF_JIT_CTX)=0) then
 begin
  frame:=@td_frame^.tf_r13;

  //tf_rip ?????

  //tf_r14 not need to move
  //tf_r15 not need to move

  frame^.tf_r13:=td_frame^.tf_r13;
  frame^.tf_rsp:=td_frame^.tf_rsp;
  frame^.tf_rbp:=td_frame^.tf_rbp;

  td_frame^.tf_flags:=td_frame^.tf_flags or TF_JIT_CTX;
 end;
end;

function get_jit_ctx_state(td_frame:p_trapframe):Boolean; public;
begin
 Result:=((td_frame^.tf_flags and TF_JIT_CTX)<>0);
end;

procedure set_jit_ctx_state(td_frame:p_trapframe;state:Boolean); public;
begin
 case state of
  False:jit_ctx_to_sys_ctx(td_frame);
  True :sys_ctx_to_jit_ctx(td_frame);
 end;
end;

procedure jit_save_ctx; assembler; nostackframe;
asm
 movqq TF_JIT_AST, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_flags(%r13)

 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r13)
 movqq %rsi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r13)
 movqq %rdx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r13)
 movqq %rcx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r13)
 movqq %r8 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r13)
 movqq %r9 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r13)
 movqq %rbx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbx(%r13)
 movqq %rax, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r13)
 movqq %r10, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r13)
 movqq %r11, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r13)
 movqq %r12, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r12(%r13)

 //tf_r14=tf_r14
 //tf_r15=tf_r15

 {
 //tf_r13
 movqq jit_frame.tf_r13(%r13),%rdi
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r13(%r13)

 //tf_rsp
 movqq jit_frame.tf_rsp(%r13),%rdi
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsp(%r13)

 //tf_rbp
 movqq jit_frame.tf_rbp(%r13),%rdi
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbp(%r13)
 }

 //tf_rflags
 pushf
 pop   %rdi
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rflags(%r13);

 lea - kthread.td_frame.tf_r13 + kthread.td_fpstate(%r13), %rdi
 //and $-32,%rdi

 {$IFDEF USE_XSAVE}
  movqq $0,t_fpstate.XSTATE_BV(%rdi)
  movqq $0,t_fpstate.XCOMP_BV (%rdi)

  mov   $7,%eax
  xor %edx,%edx
  //xsave64 (%rdi) //480FAE27
  .byte 0x48, 0x0F, 0xAE, 0x27
  //
 {$ELSE}
  vmovdqa %ymm0 ,0x000(%rdi)
  vmovdqa %ymm1 ,0x020(%rdi)
  vmovdqa %ymm2 ,0x040(%rdi)
  vmovdqa %ymm3 ,0x060(%rdi)
  vmovdqa %ymm4 ,0x080(%rdi)
  vmovdqa %ymm5 ,0x0A0(%rdi)
  vmovdqa %ymm6 ,0x0C0(%rdi)
  vmovdqa %ymm7 ,0x0E0(%rdi)
  vmovdqa %ymm8 ,0x100(%rdi)
  vmovdqa %ymm9 ,0x120(%rdi)
  vmovdqa %ymm10,0x140(%rdi)
  vmovdqa %ymm11,0x160(%rdi)
  vmovdqa %ymm12,0x180(%rdi)
  vmovdqa %ymm13,0x1A0(%rdi)
  vmovdqa %ymm14,0x1C0(%rdi)
  vmovdqa %ymm15,0x1E0(%rdi)
 {$ENDIF}
end;

procedure jit_load_ctx; assembler; nostackframe;
asm
 lea - kthread.td_frame.tf_r13 + kthread.td_fpstate(%r13), %rdi
 //and $-32,%rdi

 {$IFDEF USE_XSAVE}
  mov   $7,%eax
  xor %edx,%edx

  and __INITIAL_MXCSR_MASK__, t_fpstate.XMM_SAVE_AREA.MxCsr_Mask(%rdi)

  //xrstor (%rdi) //0FAE2F
  .byte 0x0F, 0xAE, 0x2F
  //
 {$ELSE}
  vmovdqa 0x000(%rdi),%ymm0
  vmovdqa 0x020(%rdi),%ymm1
  vmovdqa 0x040(%rdi),%ymm2
  vmovdqa 0x060(%rdi),%ymm3
  vmovdqa 0x080(%rdi),%ymm4
  vmovdqa 0x0A0(%rdi),%ymm5
  vmovdqa 0x0C0(%rdi),%ymm6
  vmovdqa 0x0E0(%rdi),%ymm7
  vmovdqa 0x100(%rdi),%ymm8
  vmovdqa 0x120(%rdi),%ymm9
  vmovdqa 0x140(%rdi),%ymm10
  vmovdqa 0x160(%rdi),%ymm11
  vmovdqa 0x180(%rdi),%ymm12
  vmovdqa 0x1A0(%rdi),%ymm13
  vmovdqa 0x1C0(%rdi),%ymm14
  vmovdqa 0x1E0(%rdi),%ymm15
 {$ENDIF}

 {
 //tf_r13
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r13(%r13),%rdi
 movqq  %rdi,jit_frame.tf_r13(%r13)

 //tf_rsp
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsp(%r13),%rdi
 movqq %rdi,jit_frame.tf_rsp(%r13)

 //tf_rbp
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbp(%r13),%rdi
 movqq %rdi,jit_frame.tf_rbp(%r13)
 }

 //tf_rflags
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rflags(%r13), %rdi
 push %rdi
 popf

 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r13), %rdi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r13), %rsi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r13), %rdx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r13), %rcx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r13), %r8
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r13), %r9
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbx(%r13), %rbx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r13), %rax
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r13), %r10
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r13), %r11
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r12(%r13), %r12
 //tf_r14=tf_r14
 //tf_r15=tf_r15
end;

//in:r14(addr) r15(plt) out:r14(addr)
procedure jit_jmp_plt_cache; assembler; nostackframe;
label
 _exit;
asm
 //load cache
 pushf
 push %rbp

 movq  (%r15),%rbp //plt^

 cmpq  t_jplt_cache_asm.src(%rbp),%r14

 jne  _exit

 //get blk
 //movq t_jplt_cache_asm.blk(%rbp),%r14

 //save current block
 //movqq %r14, - kthread.td_frame.tf_r13 + kthread.td_jctx.block(%r13)

 //get dst
 movq t_jplt_cache_asm.dst(%rbp),%r14

 pop %rbp
 popf

 //restore rbp
 //movq %rsp,%rbp
 //leaq 8(%rbp),%rbp

 //interrupt/ret
 jmp %gs:teb.jit_trp

 _exit:

 pop %rbp
 popf

 //restore rbp
 //movq %rsp,%rbp
 //leaq 8(%rbp),%rbp

 jmp jit_jmp_dispatch
 //marker
 .globl .endof_jit_jmp_plt_cache
end;

//in:r14(addr) r15(plt) out:r14(addr)
procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 movq %gs:teb.thread,%r13                //curkthread
 leaq kthread.td_frame.tf_r13(%r13),%r13 //jit_frame

 call jit_save_ctx // -> pushf

 andq  $-16,%rsp //align stack

 //rdi,rsi,rdx
 mov    %r14,%rdi
 mov    %r15,%rsi
 mov 8(%rbp),%rdx

 call jmp_dispatcher

 mov  %rax,%r14

 call jit_load_ctx // -> popf

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 //interrupt/ret
 jmp %gs:teb.jit_trp
 //marker
 .globl .endof_jit_jmp_dispatch
end;

//in:r14(nid) r15(caller)
procedure jit_hle_trace; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 call jit_save_ctx // -> pushf

 andq  $-16,%rsp //align stack

 //rdi
 mov %r14,%rdi

 call %r15

 call jit_load_ctx // -> popf

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 //interrupt/ret
 jmp %gs:teb.jit_trp
 //marker
 .globl .endof_jit_hle_trace
end;

//unused
procedure stack_set_user; assembler; nostackframe;
asm
 //switch stack
 movqq %rsp, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsp(%r13)
 movqq %rbp, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbp(%r13)

 movq jit_frame.tf_rsp(%r13),%rsp
 movq jit_frame.tf_rbp(%r13),%rbp
 //switch stack

 //teb
 movq - kthread.td_frame.tf_r13 + kthread.td_kstack.sttop(%r13) ,%r14
 movq - kthread.td_frame.tf_r13 + kthread.td_kstack.stack(%r13) ,%r15

 movq %r14,%gs:teb.sttop
 movq %r15,%gs:teb.stack
 //teb
end;

//unused
procedure stack_set_jit; assembler; nostackframe;
asm
 //switch stack
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsp(%r13), %rsp
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbp(%r13), %rbp
 //switch stack

 //teb
 movq - kthread.td_frame.tf_r13 + kthread.td_ustack.sttop(%r13) ,%r14
 movq - kthread.td_frame.tf_r13 + kthread.td_ustack.stack(%r13) ,%r15

 movq %r14,%gs:teb.sttop
 movq %r15,%gs:teb.stack
 //teb

 //uplift %rsp/%rbp ???
end;

//unused
procedure jit_jmp_internal; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 //call stack_set_user

 //movqq jit_frame.tf_r14(%r13),%r14
 //movqq jit_frame.tf_r15(%r13),%r15
 //movqq jit_frame.tf_r13(%r13),%r13

 call %gs:teb.jitcall

 //restore guard
 movq %gs:teb.thread               ,%r13 //curkthread
 leaq kthread.td_frame.tf_r13(%r13),%r13 //jit_frame

 //call stack_set_jit

 //epilog
 pop  %rbp

 //pop host call
 mov jit_frame.tf_rsp(%r13),%r14

 //uplift_jit

 //get addr
 mov  (%r14),%r14

 //lea rsp,[rsp+8]
 mov jit_frame.tf_rsp(%r13),%r15
 lea 8(%r15),%r15
 mov %r15,jit_frame.tf_rsp(%r13)

 //set zero plt
 mov $0, %r15

 jmp  jit_jmp_dispatch
end;

//unused
procedure _jit_cpuid(tf_rip,rax:qword);
var
 td:p_kthread;
begin
 td:=curkthread;
 jit_ctx_to_sys_ctx(@td^.td_frame);
 td^.td_frame.tf_rip:=tf_rip;
 print_error_td('TODO:jit_cpuid:0x'+HexStr(rax,8));
 Assert(False);
end;

//cpuid(0x00000000,0x0):eax=0x0000000d ebx=0x68747541 ecx=0x444d4163 edx=0x69746e65
//cpuid(0x00000001,0x0):eax=0x00710f31 ebx=0x07080800 ecx=0x3ed8220b edx=0x178bfbff
//cpuid(0x00000002,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000003,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000004,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000005,0x0):eax=0x00000040 ebx=0x00000040 ecx=0x00000003 edx=0x00000000
//cpuid(0x00000006,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000001 edx=0x00000000
//cpuid(0x00000007,0x0):eax=0x00000000 ebx=0x00000008 ecx=0x00000000 edx=0x00000000
//cpuid(0x00000007,0x1):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000007,0x2):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x0000000b,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x0000000d,0x0):eax=0x00000007 ebx=0x00000340 ecx=0x00000340 edx=0x00000000
//cpuid(0x00000012,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000014,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000014,0x1):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000015,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000016,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000017,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x00000019,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x0000001d,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x40000000,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x40000010,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x80000000,0x0):eax=0x8000001e ebx=0x68747541 ecx=0x444d4163 edx=0x69746e65
//cpuid(0x80000001,0x0):eax=0x00710f31 ebx=0x00000000 ecx=0x154837ff edx=0x2fd3fbff
//cpuid(0x80000002,0x0):eax=0x31314744 ebx=0x4b533130 ecx=0x48343846 edx=0x20202056
//cpuid(0x80000003,0x0):eax=0x20202020 ebx=0x20202020 ecx=0x20202020 edx=0x20202020
//cpuid(0x80000004,0x0):eax=0x20202020 ebx=0x20202020 ecx=0x20202020 edx=0x00202020
//cpuid(0x80000005,0x0):eax=0xff08ff08 ebx=0xff28ff20 ecx=0x20080140 edx=0x20020140
//cpuid(0x80000006,0x0):eax=0x21000000 ebx=0x42004200 ecx=0x08008140 edx=0x00000000
//cpuid(0x80000007,0x0):eax=0x0d6b0101 ebx=0x00000001 ecx=0x00000000 edx=0x000009d9
//cpuid(0x80000008,0x0):eax=0x00003028 ebx=0x00000000 ecx=0x00003007 edx=0x00000000
//cpuid(0x8000000a,0x0):eax=0x00000001 ebx=0x00000008 ecx=0x00000000 edx=0x00001cdf
//cpuid(0x8000001d,0x0):eax=0x00000121 ebx=0x01c0003f ecx=0x0000003f edx=0x00000000
//cpuid(0x8000001f,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x80000021,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0x8fffffff,0x0):eax=0x4c4c4548 ebx=0x494b204f ecx=0x21595454 edx=0x5e2d5e20
//cpuid(0xc0000000,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero
//cpuid(0xc0000001,0x0):eax=0x00000000 ebx=0x00000000 ecx=0x00000000 edx=0x00000000 //all zero

procedure jit_cpuid; assembler; nostackframe;
label
 _zero,
 _cpuid_0,
 _cpuid_1,
 _cpuid_5,
 _cpuid_6,
 _cpuid_7,
 _cpuid_80000000,
 _cpuid_80000001,
 _cpuid_80000002,
 _cpuid_80000003,
 _cpuid_80000005,
 _cpuid_80000006,
 _cpuid_80000007,
 _cpuid_80000008,
 _cpuid_8000000a,
 _cpuid_8000001d,
 _cpuid_8fffffff,
 _exit;
asm
 movq %rax, %r14
 seto %al
 lahf
 xchg %rax, %r14

 cmp $0,%eax
 je _cpuid_0

 cmp $1,%eax
 je _cpuid_1

 cmp $5,%eax
 je _cpuid_5

 cmp $6,%eax
 je _cpuid_6

 cmp $7,%eax
 je _cpuid_7

 cmp $0x80000000,%eax
 je _cpuid_80000000

 cmp $0x80000001,%eax
 je _cpuid_80000001

 cmp $0x80000002,%eax
 je _cpuid_80000002

 cmp $0x80000003,%eax
 je _cpuid_80000003

 cmp $0x80000004,%eax
 je _cpuid_80000003

 cmp $0x80000005,%eax
 je _cpuid_80000005

 cmp $0x80000006,%eax
 je _cpuid_80000006

 cmp $0x80000007,%eax
 je _cpuid_80000007

 cmp $0x80000008,%eax
 je _cpuid_80000008

 cmp $0x8000000a,%eax
 je _cpuid_8000000a

 cmp $0x8000001d,%eax
 je _cpuid_8000001d

 cmp $0x8fffffff,%eax
 je _cpuid_8fffffff

 //unknow to zero ->
 {
 //unknow id

 xchg %r14, %rax
 addb $127, %al
 sahf

 mov  %r14, %r15
 call jit_save_ctx
 mov  %r14, %rdi
 mov  %r15, %rsi
 jmp  _jit_cpuid

 //not reach
 }

 _zero:

 mov $0x0,%eax
 mov $0x0,%ebx
 mov $0x0,%edx
 mov $0x0,%ecx

 jmp _exit

 //

 _cpuid_0:

 //cpu_high
 mov $0xD,%eax

 //cpu_vendor
 mov $0x68747541,%ebx
 mov $0x69746E65,%edx
 mov $0x444D4163,%ecx

 jmp _exit

 //

 _cpuid_1:

 //get host
 cpuid

 //if ((cpu_id & 0xffffff80) == 0x740f00) then
 //if "machdep.bootparams.base_ps4_mode" then sceKernelHasNeoMode

 //if ((cpu_id & 0xffffff80) == 0x740f00) then sceKernelIsAuthenticNeo

 mov p_cpuid    ,%eax //cpu_id

 mov $0x178bfbff,%edx //cpu_feature
 mov $0x3ed8220b,%ecx //cpu_feature2

//                    0x07080800
//CPUID_BRAND_INDEX   0x000000ff
//CPUID_CLFUSH_SIZE   0x0000ff00
//CPUID_HTT_CORES     0x00ff0000
//CPUID_LOCAL_APIC_ID 0xff000000  //sceKernelGetCurrentCpu 0..7

 and $0x07000000,%ebx //filter CPUID_LOCAL_APIC_ID 0..7

 or  $0x00080800,%ebx //cpu_procinfo

 jmp _exit

 //

 _cpuid_5:

 mov $0x00000040,%eax
 mov $0x00000040,%ebx
 mov $0x00000000,%edx
 mov $0x00000003,%ecx

 jmp _exit

 _cpuid_6:

 mov $0x00000000,%eax
 mov $0x00000000,%ebx
 mov $0x00000000,%edx
 mov $0x00000001,%ecx

 jmp _exit

 _cpuid_7:

 test %ecx,%ecx //set bits if ecx=0

 mov $0x0,%eax
 mov $0x0,%ebx
 mov $0x0,%edx
 mov $0x0,%ecx

 mov    $0x8,%r15d
 cmove %r15d,%ebx  //if ecx=0 {ebx=8}

 jmp _exit

 //

 _cpuid_80000000:

 //cpu_exthigh
 mov $0x8000001E,%eax

 //cpu_vendor
 mov $0x68747541,%ebx
 mov $0x69746e65,%edx
 mov $0x444d4163,%ecx

 jmp _exit

 //

 _cpuid_80000001:

 mov $0x00710f31,%eax
 mov $0x00000000,%ebx
 mov $0x2fd3fbff,%edx //amd_feature
 mov $0x154837ff,%ecx //amd_feature2

 jmp _exit

 //

 _cpuid_80000002:

 mov $0x31314744,%eax
 mov $0x4b533130,%ebx
 mov $0x20202056,%edx
 mov $0x48343846,%ecx

 jmp _exit

 //

 _cpuid_80000003:

 mov $0x20202020,%eax
 mov $0x20202020,%ebx
 mov $0x20202020,%edx
 mov $0x20202020,%ecx

 jmp _exit

 //

 _cpuid_80000005:

 mov $0xff08ff08,%eax
 mov $0xff28ff20,%ebx
 mov $0x20020140,%edx
 mov $0x20080140,%ecx

 jmp _exit

 //

 _cpuid_80000006:

 mov $0x21000000,%eax
 mov $0x42004200,%ebx
 mov $0x00000000,%edx
 mov $0x08008140,%ecx

 jmp _exit

 //

 _cpuid_80000007:

 mov $0x0d6b0101,%eax
 mov $0x00000001,%ebx
 mov $0x000009d9,%edx
 mov $0x00000000,%ecx

 jmp _exit

 //

 _cpuid_80000008:

 mov $0x00003028,%eax
 mov $0x00000000,%ebx
 mov $0x00000000,%edx
 mov $0x00003007,%ecx //cpu_procinfo2

 jmp _exit

 //

 _cpuid_8000000a:

 mov $0x00000001,%eax
 mov $0x00000008,%ebx
 mov $0x00001cdf,%edx
 mov $0x00000000,%ecx

 jmp _exit

 _cpuid_8000001d:

 mov $0x00000121,%eax
 mov $0x01c0003f,%ebx
 mov $0x00000000,%edx
 mov $0x0000003f,%ecx

 jmp _exit

 _cpuid_8fffffff:

 mov $0x4c4c4548,%eax
 mov $0x494b204f,%ebx
 mov $0x5e2d5e20,%edx
 mov $0x21595454,%ecx

 _exit:

 xchg %r14, %rax
 addb $127, %al
 sahf
 movq %r14, %rax

 //interrupt/ret
 jmp %gs:teb.jit_trp
 //marker
 .globl .endof_jit_cpuid
end;

procedure strict_ps4_rdtsc_jit; assembler; nostackframe;
asm
 seto %al
 lahf
 movq %rax, %r14
 //
 rdtsc
 //
 shl  $32, %rdx
 or  %rdx, %rax
 //
 //replacing div with mul, the result in %rdx
 mulq md_rev_guest(%rip)
 //
 mov %edx, %eax //get lo
 shr  $32, %rdx //get hi
 //
 xchg %r14, %rax
 addb $127, %al
 sahf
 movq %r14, %rax
end;

procedure strict_ps4_rdtscp_jit; assembler; nostackframe;
asm
 //
 seto %al
 lahf
 movq %rax, %r15
 //
 movq %rbx, %r14
 //
 mov  $1, %eax
 cpuid
 //
 shr $6, %ebx
 and $7, %ebx
 //
 mov $7  , %ecx
 sub %ebx, %ecx
 //
 mov %r14, %rbx
 //
 lfence
 rdtsc
 lfence
 //
 shl  $32, %rdx
 or  %rdx, %rax
 //
 //replacing div with mul, the result in %rdx
 mulq md_rev_guest(%rip)
 //
 mov %edx, %eax //get lo
 shr  $32, %rdx //get hi
 //
 xchg %r15, %rax
 addb $127, %al
 sahf
 movq %r15, %rax
end;

procedure jit_interrupt_nop; assembler; nostackframe;
asm
end;

procedure rev_dispatcher(addr:Pointer); external;

procedure jit_interrupt_ud2; assembler; nostackframe; public;
asm
 ud2
end;

procedure jit_interrupt_ast; assembler; nostackframe; public;
label
 _doreti,
 _doreti_exit;
asm
 //called when the end of the instruction is confirmed!

 //clear handler
 leaq jit_interrupt_nop(%rip),%r14
 movq %r14,%gs:teb.jit_trp

 movq %gs:teb.thread,%r13                //curkthread
 leaq kthread.td_frame.tf_r13(%r13),%r13 //jit_frame

 call jit_save_ctx // -> pushf

 pop   %rdi //ret
 push  %rdi

 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call  rev_dispatcher

 _doreti:

  movq  %gs:teb.thread,%r15                 //curkthread

  //%r15=curkthread
  testl TDF_AST,kthread.td_flags(%r15)

  //interrupt guard set
  movq $1,%gs:teb.iflag

  je _doreti_exit

  //interrupt guard clear
  movq $0,%gs:teb.iflag

  call ast
  jmp _doreti

  _doreti_exit:

  //Restore full.
  call  ipi_sigreturn
  hlt

 //marker
 .globl .endof_jit_interrupt_ast
end;

procedure endof_jit_syscall      ; external name '.endof_jit_syscall';
procedure endof_jit_jmp_plt_cache; external name '.endof_jit_jmp_plt_cache';
procedure endof_jit_jmp_dispatch ; external name '.endof_jit_jmp_dispatch';
procedure endof_jit_hle_trace    ; external name '.endof_jit_hle_trace';
procedure endof_jit_cpuid        ; external name '.endof_jit_cpuid';
procedure endof_jit_interrupt_ast; external name '.endof_jit_interrupt_ast';

procedure ipi_interrupt_nop;       external name 'ipi_interrupt_nop';
procedure endof_ipi_interrupt_nop; external name '.endof_ipi_interrupt_nop';

function GET_JIT_FUNC(rip:qword):Byte; public;
begin
 if
    (
     (rip>=QWORD(@jit_syscall)) and
     (rip<=(QWORD(@endof_jit_syscall)))
    ) or
    (
     (rip>=QWORD(@jit_jmp_plt_cache)) and
     (rip<=(QWORD(@endof_jit_jmp_plt_cache)))
    ) or
    (
     (rip>=QWORD(@jit_jmp_dispatch)) and
     (rip<=(QWORD(@endof_jit_jmp_dispatch)))
    ) or
    (
     (rip>=QWORD(@jit_hle_trace)) and
     (rip<=(QWORD(@endof_jit_hle_trace)))
    ) or
    (
     (rip>=QWORD(@jit_cpuid)) and
     (rip<=(QWORD(@endof_jit_cpuid)))
    ) or
    (
     (rip>=QWORD(@jit_interrupt_ast)) and
     (rip<=(QWORD(@endof_jit_interrupt_ast)))
    ) then
 begin
  Exit(1);
 end else
 if (rip>=QWORD(@ipi_interrupt_nop)) and
    (rip<=(QWORD(@endof_ipi_interrupt_nop))) then
 begin
  Exit(3);
 end else
 if (rip=QWORD(@jit_interrupt_nop)) or
    (rip=QWORD(@jit_interrupt_ud2)) then
 begin
  Exit(2);
 end else
 begin
  Exit(0);
 end;
end;

end.



