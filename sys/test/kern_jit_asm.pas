unit kern_jit_asm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr;

{
change: rsp,rbp,rip

eflahs: temp change

change: push/pop

temp:   r13,r14

frame:  r15
}

type
 //kthread.td_frame.tf_r13

 p_jit_frame=^jit_frame;
 jit_frame=packed record
  tf_r13:QWORD;      //00
  tf_r14:QWORD;      //08
  tf__10:QWORD;      //10 (tf_r15)
  tf_r15:QWORD;      //18 (tf_trapno)
  tf_rbp:QWORD;      //20 (tf_addr)
  tf__28:QWORD;      //28 (tf_flags)
  tf_rsp:QWORD;      //30 (tf_err)
 end;


procedure uplift_jit;  assembler;
procedure page_test;   assembler;
procedure copyout_mov; assembler;
procedure copyin_mov;  assembler;

procedure jit_syscall;       assembler;
procedure jit_jmp_dispatch;  assembler;
procedure jit_call_dispatch; assembler;

procedure jit_call_internal; assembler;
procedure jit_jmp_internal;  assembler;

implementation

uses
 vmparam,
 systm,
 vm_pmap,
 trap,
 md_context,
 signal;

//

function jmp_dispatcher(addr:Pointer;is_call:Boolean):Pointer; external;

//

procedure jit_sigsegv(addr:Pointer);
begin
 Writeln('jit_sigsegv:0x',HexStr(addr));
 Assert(False);
end;

//in/out:r13
procedure uplift_jit; assembler; nostackframe;
const
 VM_MAX_D=VM_MAXUSER_ADDRESS shr 32;
label
 _sigsegv;
asm
 pushfq
 push %r14
 push %r13
 //
 mov VM_MAX_D,%r14
 shl  $32,%r14
 cmp %r14,%r13
 ja _sigsegv
 //
 //low addr (r14)
 mov %r13,%r14
 and PAGE_MASK,%r14
 //high addr (r13)
 shr PAGE_SHIFT   ,%r13
 and PAGE_MAP_MASK,%r13
 //uplift (r13)
 lea (,%r13,4),%r13
 add PAGE_MAP(%rip),%r13
 mov (%r13),%r13d
 //filter (r13)
 and PAGE_OFS_MASK,%r13
 jz _sigsegv
 //combine (r13|r14)
 shl PAGE_SHIFT,%r13
 or  %r14,%r13
 //
 pop %r14 //origin
 pop %r14
 popfq
 ret

 _sigsegv:
 pop %rdi //origin

 pop %r14
 popfq

 call jit_sigsegv

 ret
end;

//in:r13(addr),r14b:(mem_size) out:ZF
procedure page_test; assembler; nostackframe;
label
 _exit;
asm
 push %rdi
 push %rsi
 //
 mov    %r13 ,%rdi
 movzbq %r14b,%rsi
 lea -1(%rsi),%rsi //-1
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

procedure jit_simple_save_ctx; assembler; nostackframe;
asm
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r15)
 movqq %rsi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r15)
 movqq %rdx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r15)
 movqq %rcx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r15)
 movqq %r8 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r15)
 movqq %r9 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r15)
 movqq %rax, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r15)
 movqq %r10, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r15)
 movqq %r11, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r15)
end;

procedure jit_simple_load_ctx; assembler; nostackframe;
asm
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r15), %rdi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r15), %rsi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r15), %rdx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r15), %rcx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r15), %r8
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r15), %r9
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r15), %rax
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r15), %r10
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r15), %r11
end;

//in:r13(addr),r14b:(size)
procedure copyout_mov; assembler;
label
 _simple,
 _exit;
var
 addr:Pointer;
 size:QWORD;
 data:array[0..31] of Byte;
asm
 pushfq
 //
 call page_test
 je _simple

  //cross page

  movq %r13,addr    //save orig addr
  movq %r14,size    //save orig size

  lea  data,%r13    //vaddr:=data

  mov  8(%rbp),%r14 //get ret addr
  lea  2(%rdi),%r14 //add jmp near

  popfq  //restore flags before call

  call %r14         //reg->data

  pushfq //save again

  call jit_simple_save_ctx

  movq    addr,%rsi //vaddr
  lea     data,%rdi //data
  movq    size,%r14
  movzbq %r14b,%rdx //size

  call copyout

  call jit_simple_load_ctx

  popfq

  jmp _exit
 _simple:

  call uplift_jit

  mov  8(%rbp),%r14 //ret addr
  lea  2(%r14),%r14 //jmp near

  popfq //restore flags before call

  call %r14         //reg->data

 _exit:
end;

//in:r13(addr),r14b:(size) out:r13
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

  //cross page

  call jit_simple_save_ctx

  mov     %r13,%rdi //vaddr
  lea     data,%rsi //data
  movzbq %r14b,%rdx //size

  call copyin

  call jit_simple_load_ctx

  lea data,%r13 //vaddr:=data

  jmp _exit
 _simple:

  call uplift_jit

 _exit:
 //
 popfq
end;

//

//in:r13(addr)
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

 movqq %r13, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rip (%r15) //save %r13 to tf_rip

 pushf
 pop %r13

 movqq %gs:teb.thread,%r14 //curkthread
 test  %r14,%r14
 jz    _fail

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%r14) //clear PCB_FULL_IRET

 movqq %r13,kthread.td_frame.tf_rflags(%r14) //save flags

 movqq %rdi,kthread.td_frame.tf_rdi(%r14)
 movqq %rsi,kthread.td_frame.tf_rsi(%r14)
 movqq %rdx,kthread.td_frame.tf_rdx(%r14)
 movqq   $0,kthread.td_frame.tf_rcx(%r14)
 movqq %r8 ,kthread.td_frame.tf_r8 (%r14)
 movqq %r9 ,kthread.td_frame.tf_r9 (%r14)
 movqq %rax,kthread.td_frame.tf_rax(%r14)
 movqq %rbx,kthread.td_frame.tf_rbx(%r14)
 movqq %r10,kthread.td_frame.tf_r10(%r14)
 movqq   $0,kthread.td_frame.tf_r11(%r14)
 movqq %r12,kthread.td_frame.tf_r12(%r14)

 //tf_r13=tf_r13
 //tf_r14=tf_r14

 movqq             jit_frame.tf_r15(%r15),%r13
 movqq %r13,kthread.td_frame.tf_r15(%r14)

 movqq             jit_frame.tf_rsp(%r15),%r13
 movqq %r13,kthread.td_frame.tf_rsp(%r14)

 movqq             jit_frame.tf_rbp(%r15),%r13
 movqq %r13,kthread.td_frame.tf_rbp(%r14)

 movqq   $1,kthread.td_frame.tf_trapno(%r14)
 movqq   $0,kthread.td_frame.tf_addr  (%r14)
 movqq   $0,kthread.td_frame.tf_flags (%r14)
 movqq   $2,kthread.td_frame.tf_err   (%r14) //sizeof(syscall)

 call amd64_syscall

 _after_call:

 movq %gs:teb.thread               ,%r14 //curkthread
 leaq kthread.td_frame.tf_r13(%r14),%r15 //jit_frame

 //Requested full context restore
 testl PCB_FULL_IRET,kthread.pcb_flags(%r14)
 jnz _doreti

 testl TDF_AST,kthread.td_flags(%r14)
 jne _ast

 //Restore preserved registers.

 //get flags
 movqq kthread.td_frame.tf_rflags(%r14),%r13
 push %r13
 popf

 movqq kthread.td_frame.tf_rdi(%r14),%rdi
 movqq kthread.td_frame.tf_rsi(%r14),%rsi
 movqq kthread.td_frame.tf_rdx(%r14),%rdx
 movqq kthread.td_frame.tf_rax(%r14),%rax

 //tf_r13=tf_r13
 //tf_r14=tf_r14

 movqq kthread.td_frame.tf_r15(%r14),%r13
 movqq   %r13,jit_frame.tf_r15(%r15)

 movqq kthread.td_frame.tf_rsp(%r14),%r13
 movqq   %r13,jit_frame.tf_rsp(%r15)

 movqq kthread.td_frame.tf_rbp(%r14),%r13
 movqq   %r13,jit_frame.tf_rbp(%r15)

 movqq $0,%rcx
 movqq $0,%r11

 //epilog (debugger)
 movq  %rbp,%rsp
 popq  %rbp
 ret

 //fail (curkthread=nil)
 _fail:

 or $1,%r13 //set CF
 push  %r13
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
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r15)
 movqq %rsi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r15)
 movqq %rdx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r15)
 movqq %rcx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r15)
 movqq %r8 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r15)
 movqq %r9 , - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r15)
 movqq %rbx, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbx(%r15)
 movqq %rax, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r15)
 movqq %r10, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r15)
 movqq %r11, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r15)
 movqq %r12, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r12(%r15)

 pushf
 pop   %rdi
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rflags(%r15);

 lea - kthread.td_frame.tf_r13 + kthread.td_fpstate(%r15), %rdi
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
end;

procedure jit_load_ctx; assembler; nostackframe;
asm
 lea - kthread.td_frame.tf_r13 + kthread.td_fpstate(%r15), %rdi
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

 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rflags(%r15), %rdi
 push %rdi
 popf

 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdi(%r15), %rdi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rsi(%r15), %rsi
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rdx(%r15), %rdx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rcx(%r15), %rcx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r8 (%r15), %r8
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r9 (%r15), %r9
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rbx(%r15), %rbx
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rax(%r15), %rax
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r10(%r15), %r10
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r11(%r15), %r11
 movqq - kthread.td_frame.tf_r13 + kthread.td_frame.tf_r12(%r15), %r12
end;

procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %r13,%rdi
 mov    $0,%rsi

 call jmp_dispatcher

 mov  %rax,%r13

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %r13
end;

procedure jit_call_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %r13,%rdi
 mov    $1,%rsi

 call jmp_dispatcher

 mov  %rax,%r13

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %r13
end;

procedure jit_call_internal; assembler; nostackframe;
asm
 //pop host call
 mov jit_frame.tf_rsp(%r15),%r13
 lea 8(%r13),%r13
 mov %r13,jit_frame.tf_rsp(%r15)

 //push internal call
 lea  -8(%rsp),%rsp

 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 //set
 //%r13 ABI preserve the registers
 //%r14 ABI preserve the registers
 //%r15 ABI preserve the registers

 //%rsp???
 //%rbp???

 call %gs:teb.jitcall

 //restore guard
 movq %gs:teb.thread               ,%r15 //curkthread
 leaq kthread.td_frame.tf_r13(%r15),%r15 //jit_frame

 //%r13 ABI preserve the registers
 //%r14 ABI preserve the registers
 //%r15 ABI preserve the registers

 //%rsp???
 //%rbp???

 //epilog
 pop  %rbp
end;

procedure jit_jmp_internal; assembler; nostackframe;
asm
 //set

 //%rsp???
 //%rbp???

 movqq jit_frame.tf_r13(%r15),%r13
 movqq jit_frame.tf_r14(%r15),%r14
 //movqq jit_frame.tf_r15(%r15),%r15

 jmp %gs:teb.jitcall
end;


end.



