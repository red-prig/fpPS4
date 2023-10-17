unit kern_jit_asm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

{
change: rsp,rbp,rip
rax?

eflahs? temp change?

change: push/pop

thread: r15

}

type
 p_jit_frame=^jit_frame;
 jit_frame=packed record
  tf_rax:QWORD; //00
  tf_rsp:QWORD; //08
  tf_rbp:QWORD; //10
  tf_r14:QWORD; //18
  tf_r15:QWORD; //20
  tf_rip:QWORD; //28
 end;

//in/out:rax uses:r14
procedure uplift_jit; assembler;

//in:rax(addr),r14b:(mem_size) out:ZF
procedure page_test; assembler;

//in:rax(addr),r14b:(size)
procedure copyout_mov; assembler;

//in:rax(addr),r14b:(size) out:rax
procedure copyin_mov; assembler;

procedure jit_syscall; assembler;
procedure jit_jmp_dispatch; assembler;
procedure jit_call_dispatch; assembler;

implementation

uses
 vmparam,
 systm,
 vm_pmap,
 trap,
 md_context,
 signal,
 kern_thr;

//

function jmp_dispatcher(addr:Pointer;is_call:Boolean):Pointer; external;

//

procedure sigsegv(addr:Pointer);
begin
 Writeln('sigsegv:0x',HexStr(addr));
 Assert(False);
end;

//in/out:rax uses:r14
procedure uplift_jit; assembler; nostackframe;
const
 VM_MAX_D=VM_MAXUSER_ADDRESS shr 32;
label
 _sigsegv;
asm
 pushfq
 push %r14
 push %rax
 //
 mov VM_MAX_D,%r14
 shl $32,%r14
 cmp %r14,%rax
 ja _sigsegv
 //
 //low addr (r14)
 mov %rax,%r14
 and PAGE_MASK,%r14
 //high addr (rax)
 shr PAGE_SHIFT   ,%rax
 and PAGE_MAP_MASK,%rax
 //uplift (rax)
 lea (,%rax,4),%rax
 add PAGE_MAP(%rip),%rax
 mov (%rax),%eax
 //filter (rax)
 and PAGE_OFS_MASK,%rax
 jz _sigsegv
 //combine (rax|r14)
 shl PAGE_SHIFT,%rax
 or  %r14,%rax
 //
 pop %r14
 pop %r14
 popfq
 ret

 _sigsegv:
 pop %rdi

 pop %r14
 popfq

 call sigsegv

 ret
end;

//in:rax(addr),r14b:(mem_size) out:ZF
procedure page_test; assembler; nostackframe;
label
 _exit;
asm
 push %rdi
 push %rsi
 //
 mov %rax,%rdi
 movzbq %r14b,%rsi
 sub $1,%rsi
 //addr2:=addr+mem_high (rsi)
 add %rdi,%rsi
 //high addr (rdi,rsi)
 shr PAGE_SHIFT   ,%rdi
 shr PAGE_SHIFT   ,%rsi
 and PAGE_MAP_MASK,%rdi
 and PAGE_MAP_MASK,%rsi
 //
 cmp %rdi,%rsi
 je _exit
 //uplift (rdi,rsi)
 lea (,%rdi,4),%rdi
 lea (,%rsi,4),%rsi
 //
 add PAGE_MAP(%rip),%rdi
 add PAGE_MAP(%rip),%rsi
 //
 mov (%rdi),%edi
 mov (%rsi),%esi
 //filter (rdi,rsi)
 and PAGE_OFS_MASK,%rdi
 and PAGE_OFS_MASK,%rsi
 //
 inc %rdi
 cmp %rdi,%rsi
 _exit:
 //
 pop %rsi
 pop %rdi
end;

//in:rax(addr),r14b:(size)
procedure copyout_mov; assembler;
label
 _simple,
 _exit;
var
 addr:Pointer;
 data:array[0..31] of Byte;
asm
 pushfq
 //
 call page_test
 je _simple

  popfq //restore flags before call

  push %rdi
  push %rsi
  push %rdx
  push %rcx
  push %r8
  push %r9
  push %r10
  push %r11

  movq %rax,addr

  lea  data,%rax    //vaddr:=data

  mov  8(%rbp),%rdi //ret addr
  lea  2(%rdi),%rdi //jmp near

  call %rdi         //reg->data

  movq    addr,%rsi //vaddr
  lea     data,%rdi //data
  movzbq %r14b,%rdx //size

  pushfq

  call copyout

  popfq

  pop  %r11
  pop  %r10
  pop  %r9
  pop  %r8
  pop  %rcx
  pop  %rdx
  pop  %rsi
  pop  %rdi

  jmp _exit
 _simple:

  call uplift_jit

  mov  8(%rbp),%r14 //ret addr
  lea  2(%r14),%r14 //jmp near

  popfq //restore flags before call

  call %r14         //reg->data

 _exit:
end;

//in:rax(addr),r14b:(size) out:rax
procedure copyin_mov; assembler;
label
 _simple,
 _exit;
var
 data:array[0..31] of Byte;
asm
 pushfq
 //
 call page_test
 je _simple

  push %rdi
  push %rsi
  push %rdx
  push %rcx
  push %r8
  push %r9
  push %r10
  push %r11

  mov     %rax,%rdi //vaddr
  lea     data,%rsi //data
  movzbq %r14b,%rdx //size

  call copyin

  pop  %r11
  pop  %r10
  pop  %r9
  pop  %r8
  pop  %rcx
  pop  %rdx
  pop  %rsi
  pop  %rdi

  lea data,%rax //vaddr:=data

  jmp _exit
 _simple:

  call uplift_jit

 _exit:
 //
 popfq
end;

//

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

 movqq %rax,jit_frame.tf_rip(%r15) //save %rax to tf_rip

 pushf
 pop %rax

 movqq %gs:teb.thread,%r14 //curkthread
 test  %r14,%r14
 jz    _fail

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%r14) //clear PCB_FULL_IRET

 movqq %rax,kthread.td_frame.tf_rflags(%r14) //save flags

 movqq %rdi,kthread.td_frame.tf_rdi(%r14)
 movqq %rsi,kthread.td_frame.tf_rsi(%r14)
 movqq %rdx,kthread.td_frame.tf_rdx(%r14)
 movqq   $0,kthread.td_frame.tf_rcx(%r14)
 movqq %r8 ,kthread.td_frame.tf_r8 (%r14)
 movqq %r9 ,kthread.td_frame.tf_r9 (%r14)
 movqq %rbx,kthread.td_frame.tf_rbx(%r14)
 movqq %r10,kthread.td_frame.tf_r10(%r14)
 movqq   $0,kthread.td_frame.tf_r11(%r14)
 movqq %r12,kthread.td_frame.tf_r12(%r14)
 movqq %r13,kthread.td_frame.tf_r13(%r14)

 movqq   $1,kthread.td_frame.tf_trapno(%r14)
 movqq   $0,kthread.td_frame.tf_addr  (%r14)
 movqq   $0,kthread.td_frame.tf_flags (%r14)
 movqq   $2,kthread.td_frame.tf_err   (%r14) //sizeof(syscall)

 movqq jit_frame.tf_rax(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rax(%r14)

 movqq jit_frame.tf_rsp(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rsp(%r14)

 movqq jit_frame.tf_rbp(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rbp(%r14)

 movqq jit_frame.tf_r14(%r15),%rax
 movqq %rax,kthread.td_frame.tf_r14(%r14)

 movqq jit_frame.tf_r15(%r15),%rax
 movqq %rax,kthread.td_frame.tf_r15(%r14)

 movqq jit_frame.tf_rip(%r15),%rax
 movqq %rax,kthread.td_frame.tf_rip(%r14)

 call amd64_syscall

 _after_call:

 movqq %gs:teb.thread          ,%r14 //curkthread
 movqq kthread.td_jit_ctx(%r14),%r15 //jit_frame

 //Requested full context restore
 testl PCB_FULL_IRET,kthread.pcb_flags(%r14)
 jnz _doreti

 testl TDF_AST,kthread.td_flags(%r14)
 jne _ast

 //Restore preserved registers.

 //get flags
 movqq kthread.td_frame.tf_rflags(%r14),%rax
 push %rax
 popf

 movqq kthread.td_frame.tf_rdi(%r14),%rdi
 movqq kthread.td_frame.tf_rsi(%r14),%rsi
 movqq kthread.td_frame.tf_rdx(%r14),%rdx

 movqq kthread.td_frame.tf_rax(%r14),%rax
 movqq %rax,jit_frame.tf_rax(%r15)

 movqq kthread.td_frame.tf_rsp(%r14),%rax
 movqq %rax,jit_frame.tf_rsp(%r15)

 movqq kthread.td_frame.tf_rbp(%r14),%rax
 movqq %rax,jit_frame.tf_rbp(%r15)

 movqq $0,%rcx
 movqq $0,%r11

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 ret

 //fail (curkthread=nil)
 _fail:

 or $1,%rax //set CF
 push %rax
 popf

 movqq $14,jit_frame.tf_rax(%r15) //EFAULT
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

  //%r14=curkthread
  testl TDF_AST,kthread.td_flags(%r14)
  je _doreti_exit

  call ast
  jmp _doreti

 _doreti_exit:

  //Restore full.
  call  ipi_sigreturn
  hlt
end;

procedure jit_save_ctx; assembler; nostackframe;
asm
 push %rax

 movqq %gs:teb.thread,%rax //curkthread

 movqq %rdi,kthread.td_frame.tf_rdi(%rax)
 movqq %rsi,kthread.td_frame.tf_rsi(%rax)
 movqq %rdx,kthread.td_frame.tf_rdx(%rax)
 movqq %rcx,kthread.td_frame.tf_rcx(%rax)
 movqq %r8 ,kthread.td_frame.tf_r8 (%rax)
 movqq %r9 ,kthread.td_frame.tf_r9 (%rax)
 movqq %rbx,kthread.td_frame.tf_rbx(%rax)
 movqq %r10,kthread.td_frame.tf_r10(%rax)
 movqq %r11,kthread.td_frame.tf_r11(%rax)
 movqq %r12,kthread.td_frame.tf_r12(%rax)
 movqq %r13,kthread.td_frame.tf_r13(%rax)

 pushf
 pop %rdi
 movqq %rdi,kthread.td_frame.tf_rflags(%rax);

 lea kthread.td_fpstate(%rax),%rdi
 and $-32,%rdi

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

 pop %rax
end;

procedure jit_load_ctx; assembler; nostackframe;
asm
 push %rax

 movqq %gs:teb.thread,%rax //curkthread

 lea kthread.td_fpstate(%rax),%rdi
 and $-32,%rdi

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

 movqq kthread.td_frame.tf_rflags(%rax),%rdi
 push %rdi
 popf

 movqq kthread.td_frame.tf_rdi(%rax),%rdi
 movqq kthread.td_frame.tf_rsi(%rax),%rsi
 movqq kthread.td_frame.tf_rdx(%rax),%rdx
 movqq kthread.td_frame.tf_rcx(%rax),%rcx
 movqq kthread.td_frame.tf_r8 (%rax),%r8
 movqq kthread.td_frame.tf_r9 (%rax),%r9
 movqq kthread.td_frame.tf_rbx(%rax),%rbx
 movqq kthread.td_frame.tf_r10(%rax),%r10
 movqq kthread.td_frame.tf_r11(%rax),%r11
 movqq kthread.td_frame.tf_r12(%rax),%r12
 movqq kthread.td_frame.tf_r13(%rax),%r13

 pop %rax
end;

procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %rax,%rdi
 mov    $0,%rsi

 call jmp_dispatcher

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %rax
end;

procedure jit_call_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %rax,%rdi
 mov    $1,%rsi

 call jmp_dispatcher

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %rax
end;

end.

