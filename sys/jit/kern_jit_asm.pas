unit kern_jit_asm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 x86_fpdbgdisas;

{$DEFINE USE_XSAVE}

{
change: rsp,rbp,rip

eflahs: temp change

change: push/pop

frame:  r13

temp:   r14,r15
}

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
  dst:Pointer;
  blk:Pointer;
 end;

procedure jit_syscall;       assembler;
procedure jit_plt_cache;     assembler;
procedure jit_jmp_dispatch;  assembler;

procedure jit_jmp_internal;  assembler;

function  IS_JIT_FUNC(rip:qword):Boolean;

procedure jit_save_ctx;
procedure jit_load_ctx;

procedure jit_save_to_sys_save(td:p_kthread);
procedure sys_save_to_jit_save(td:p_kthread);

implementation

uses
 trap,
 ucontext,
 md_context,
 signal,
 subr_backtrace;

//

function jmp_dispatcher(addr,plt,from:Pointer):Pointer; external;

//

procedure jit_sigsegv(addr:Pointer);
begin
 print_error_td('jit_sigsegv:0x'+HexStr(addr));
 Assert(False);
end;

procedure jit_simple_save_ctx; assembler; nostackframe;
asm
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r13)
 movqq %rsi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r13)
 movqq %rdx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r13)
 movqq %rcx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r13)
 movqq %r8 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r13)
 movqq %r9 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r13)
 movqq %rax, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r13)
 movqq %r10, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r13)
 movqq %r11, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r13)
end;

procedure jit_simple_load_ctx; assembler; nostackframe;
asm
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r13), %rdi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r13), %rsi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r13), %rdx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r13), %rcx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r13), %r8
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r13), %r9
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r13), %rax
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r13), %r10
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r13), %r11
end;

//

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

 andq  $-16,%rsp //align stack

 pushf
 pop %r14

 movqq %gs:teb.thread,%r15 //curkthread
 test  %r15,%r15
 jz    _fail

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%r15) //clear PCB_FULL_IRET

 movqq %r14,kthread.td_frame.tf_rflags(%r15) //save flags

 movqq %rdi,kthread.td_frame.tf_rdi(%r15)
 movqq %rsi,kthread.td_frame.tf_rsi(%r15)
 movqq %rdx,kthread.td_frame.tf_rdx(%r15)
 movqq   $0,kthread.td_frame.tf_rcx(%r15)
 movqq %r8 ,kthread.td_frame.tf_r8 (%r15)
 movqq %r9 ,kthread.td_frame.tf_r9 (%r15)
 movqq %rax,kthread.td_frame.tf_rax(%r15)
 movqq %rbx,kthread.td_frame.tf_rbx(%r15)
 movqq %r10,kthread.td_frame.tf_r10(%r15)
 movqq   $0,kthread.td_frame.tf_r11(%r15)
 movqq %r12,kthread.td_frame.tf_r12(%r15)

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

 movqq kthread.td_frame.tf_rdi(%r15),%rdi
 movqq kthread.td_frame.tf_rsi(%r15),%rsi
 movqq kthread.td_frame.tf_rdx(%r15),%rdx
 movqq kthread.td_frame.tf_rax(%r15),%rax

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

 movqq $0,%rcx
 movqq $0,%r11

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 ret

 //fail (curkthread=nil)
 _fail:

 or $1,%r14 //set CF
 push  %r14
 popf

 movqq $14,%rax //EFAULT
 movqq  $0,%rdx
 movqq  $0,%rcx
 movqq  $0,%r11

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 ret

 //ast
 _ast:

  call ast
  jmp _after_call

 //doreti
 _doreti:

  //%r15=curkthread
  testl TDF_AST,kthread.td_flags(%r15)
  je _doreti_exit

  call ast
  jmp _doreti

 _doreti_exit:

  //Restore full.
  call  ipi_sigreturn
  hlt
end;

procedure jit_save_to_sys_save(td:p_kthread); public;
var
 frame:p_jit_frame;
begin
 frame:=@td^.td_frame.tf_r13;

 //tf_rip ?????

 td^.td_frame.tf_r13:=frame^.tf_r13;
 td^.td_frame.tf_rsp:=frame^.tf_rsp;
 td^.td_frame.tf_rbp:=frame^.tf_rbp;

 td^.td_frame.tf_trapno:=0;
 td^.td_frame.tf_BrF   :=0;
 td^.td_frame.tf_BrT   :=0;
end;

procedure sys_save_to_jit_save(td:p_kthread); public;
var
 frame:p_jit_frame;
begin
 frame:=@td^.td_frame.tf_r13;

 frame^.tf_r13:=td^.td_frame.tf_r13;
 frame^.tf_rsp:=td^.td_frame.tf_rsp;
 frame^.tf_rbp:=td^.td_frame.tf_rbp;
end;

procedure jit_save_ctx; assembler; nostackframe;
asm
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

//in:r14(addr) r15(plt)
procedure jit_plt_cache; assembler; nostackframe;
label
 _exit;
asm
 //load cache
 pushf
 movq  (%r15),%rbp //plt^

 cmpq  t_jplt_cache_asm.src(%rbp),%r14

 jne  _exit

 //get blk
 movq t_jplt_cache_asm.blk(%rbp),%r14

 //save current block
 movqq %r14, - kthread.td_frame.tf_r13 + kthread.td_jctx.block(%r13)

 //get dst
 movq t_jplt_cache_asm.dst(%rbp),%r14

 popf

 //pop internal
 lea  8(%rsp),%rsp

 //restore rbp
 movq %rsp,%rbp

 jmp  %r14

 _exit:

 popf

 //restore rbp
 movq %rsp,%rbp
 leaq 8(%rbp),%rbp

 jmp jit_jmp_dispatch
end;

//in:r14(addr) r15(plt)
procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 //rdi,rsi,rdx
 mov    %r14,%rdi
 mov    %r15,%rsi
 mov 8(%rbp),%rdx

 call jmp_dispatcher

 mov  %rax,%r14

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 //pop internal
 lea  8(%rsp),%rsp
 jmp  %r14
end;

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

procedure jit_jmp_internal; assembler; nostackframe;
asm
 //push internal call
 lea  -8(%rsp),%rsp

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

function IS_JIT_FUNC(rip:qword):Boolean; public;
begin
 Result:=(
          (rip>=QWORD(@jit_syscall)) and
          (rip<=(QWORD(@jit_syscall)+$1A5)) //jit_syscall func size
         ) or
         (
          (rip>=QWORD(@jit_jmp_dispatch)) and
          (rip<=(QWORD(@jit_jmp_dispatch)+$2C)) //jit_jmp_dispatch func size
         ) or
         (
          (rip>=QWORD(@jit_plt_cache)) and
          (rip<=(QWORD(@jit_plt_cache)+$33)) //jit_plt_cache func size
         );
end;

end.



