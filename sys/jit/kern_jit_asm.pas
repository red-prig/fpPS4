unit kern_jit_asm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 x86_fpdbgdisas;

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
  tf_rsp:QWORD;      //18 (tf_trapno)
  tf_rbp:QWORD;      //20 (tf_addr)
  tf__28:QWORD;      //28 (tf_flags)
  tf_r13:QWORD;      //30 (tf_err)
 end;


procedure uplift_jit;  assembler;

procedure copyout_mov_1;   assembler;
procedure copyout_mov_2;   assembler;
procedure copyout_mov_4;   assembler;
procedure copyout_mov_8;   assembler;
procedure copyout_mov_6;   assembler;
procedure copyout_mov_10;  assembler;
procedure copyout_mov_16;  assembler;
procedure copyout_mov_32;  assembler;
procedure copyout_mov_64;  assembler;
procedure copyout_mov_512; assembler;

procedure copyin_mov_1;   assembler;
procedure copyin_mov_2;   assembler;
procedure copyin_mov_4;   assembler;
procedure copyin_mov_8;   assembler;
procedure copyin_mov_6;   assembler;
procedure copyin_mov_10;  assembler;
procedure copyin_mov_16;  assembler;
procedure copyin_mov_32;  assembler;
procedure copyin_mov_64;  assembler;
procedure copyin_mov_512; assembler;

procedure jit_syscall;       assembler;
procedure jit_jmp_dispatch;  assembler;
procedure jit_call_dispatch; assembler;

procedure jit_call_internal; assembler;
procedure jit_jmp_internal;  assembler;

function  IS_JIT_FUNC(rip:qword):Boolean;

const
 copyout_mov_size:array[TOperandSize] of Pointer=(
  @copyout_mov_1 ,
  @copyout_mov_1 ,
  @copyout_mov_2 ,
  @copyout_mov_4 ,
  @copyout_mov_8 ,
  @copyout_mov_6 ,
  @copyout_mov_10,
  @copyout_mov_16,
  @copyout_mov_32,
  @copyout_mov_64,
  @copyout_mov_512
 );

 copyin_mov_size:array[TOperandSize] of Pointer=(
  @copyin_mov_1 ,
  @copyin_mov_1 ,
  @copyin_mov_2 ,
  @copyin_mov_4 ,
  @copyin_mov_8 ,
  @copyin_mov_6 ,
  @copyin_mov_10,
  @copyin_mov_16,
  @copyin_mov_32,
  @copyin_mov_64,
  @copyin_mov_512
 );

implementation

uses
 vmparam,
 systm,
 vm_pmap,
 trap,
 md_context,
 signal,
 subr_backtrace;

//

function jmp_dispatcher(addr:Pointer;is_call:Boolean):Pointer; external;

//

procedure jit_save_ctx; forward;
procedure jit_load_ctx; forward;

procedure jit_sigsegv(addr:Pointer);
begin
 print_backtrace_td(stderr);
 Writeln('jit_sigsegv:0x',HexStr(addr));
 Assert(False);
end;

//in/out:r14
procedure uplift_jit_notsafe; assembler;
const
 VM_MAX_D=VM_MAXUSER_ADDRESS shr 32;
label
 _ret,
 _sigsegv;
var
 addr:QWORD;
 tmp2:QWORD;
asm
 mov %r15,tmp2
 mov %r14,addr //origin
 //
 mov VM_MAX_D,%r15
 shl  $32,%r15
 cmp %r15,%r14
 ja _sigsegv
 //
 //low addr (r15)
 mov %r14,%r15
 and PAGE_MASK,%r15
 //high addr (r14)
 shr PAGE_SHIFT   ,%r14
 and PAGE_MAP_MASK,%r14
 //uplift (r14)
 lea (,%r14,4),%r14
 add PAGE_MAP(%rip),%r14
 mov (%r14),%r14d
 //filter (r14)
 and PAGE_OFS_MASK,%r14
 jz _sigsegv
 //combine (r14|r15)
 shl PAGE_SHIFT,%r14
 or  %r15,%r14
 //

 mov tmp2,%r15 //restore

 jmp _ret

 _sigsegv:

 mov tmp2,%r15 //restore

 call jit_save_ctx

 mov  addr,%rdi //origin

 call jit_sigsegv

 call jit_load_ctx

 _ret:
end;

//in/out:r14
procedure uplift_jit; assembler; nostackframe;
asm
 pushfq //
 call uplift_jit_notsafe
 popfq  //
end;

//in:r14(addr),r15:(mem_size) out:ZF
procedure page_test; assembler;
label
 _exit;
var
 rdi:QWORD;
 rsi:QWORD;
asm
 mov %rdi,rdi
 mov %rsi,rsi
 //
 mov %r14,%rdi
 mov %r15,%rsi
 //addr2:=addr+mem_size-1 (rsi)
 lea -1(%rsi,%rdi),%rsi
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
 mov rdi,%rdi
 mov rsi,%rsi
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

//in:r14(addr),r15:(size)
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

  movq %r14,addr    //save orig addr
  movq %r15,size    //save orig size

  lea  data,%r14    //vaddr:=data

  mov  8(%rbp),%r15 //get ret addr
  lea  2(%rdi),%r15 //add jmp near

  popfq  //restore flags before call

  call %r15         //reg->data

  pushfq //save again

  call jit_simple_save_ctx

  movq    addr,%rsi //vaddr
  lea     data,%rdi //data
  movq    size,%r15
  movq    %r15,%rdx //size

  call copyout

  call jit_simple_load_ctx

  popfq //restore

  jmp _exit
 _simple:

  call uplift_jit_notsafe

  mov  8(%rbp),%r15 //ret addr
  lea  2(%r15),%r15 //jmp near

  popfq //restore flags before call

  call %r15         //reg->data

 _exit:
end;

procedure copyout_mov_1; assembler; nostackframe;
asm
 movq $1,%r15
 jmp copyout_mov
end;

procedure copyout_mov_2; assembler; nostackframe;
asm
 movq $2,%r15
 jmp copyout_mov
end;

procedure copyout_mov_4; assembler; nostackframe;
asm
 movq $4,%r15
 jmp copyout_mov
end;

procedure copyout_mov_8; assembler; nostackframe;
asm
 movq $8,%r15
 jmp copyout_mov
end;

procedure copyout_mov_6; assembler; nostackframe;
asm
 movq $6,%r15
 jmp copyout_mov
end;

procedure copyout_mov_10; assembler; nostackframe;
asm
 movq $10,%r15
 jmp copyout_mov
end;

procedure copyout_mov_16; assembler; nostackframe;
asm
 movq $16,%r15
 jmp copyout_mov
end;

procedure copyout_mov_32; assembler; nostackframe;
asm
 movq $32,%r15
 jmp copyout_mov
end;

procedure copyout_mov_64; assembler; nostackframe;
asm
 movq $64,%r15
 jmp copyout_mov
end;

procedure copyout_mov_512; assembler; nostackframe;
asm
 movq $512,%r15
 jmp copyout_mov
end;

//in:r14(addr),r15:(size) out:r14
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

  mov     %r14,%rdi //vaddr
  lea     data,%rsi //data
  mov     %r15,%rdx //size

  call copyin

  call jit_simple_load_ctx

  lea data,%r14 //vaddr:=data

  jmp _exit
 _simple:

  call uplift_jit_notsafe

 _exit:
 //
 popfq
end;

procedure copyin_mov_1; assembler; nostackframe;
asm
 movq $1,%r15
 jmp copyin_mov
end;

procedure copyin_mov_2; assembler; nostackframe;
asm
 movq $2,%r15
 jmp copyin_mov
end;

procedure copyin_mov_4; assembler; nostackframe;
asm
 movq $4,%r15
 jmp copyin_mov
end;

procedure copyin_mov_8; assembler; nostackframe;
asm
 movq $8,%r15
 jmp copyin_mov
end;

procedure copyin_mov_6; assembler; nostackframe;
asm
 movq $6,%r15
 jmp copyin_mov
end;

procedure copyin_mov_10; assembler; nostackframe;
asm
 movq $10,%r15
 jmp copyin_mov
end;

procedure copyin_mov_16; assembler; nostackframe;
asm
 movq $16,%r15
 jmp copyin_mov
end;

procedure copyin_mov_32; assembler; nostackframe;
asm
 movq $32,%r15
 jmp copyin_mov
end;

procedure copyin_mov_64; assembler; nostackframe;
asm
 movq $64,%r15
 jmp copyin_mov
end;

procedure copyin_mov_512; assembler; nostackframe;
asm
 movq $512,%r15
 jmp copyin_mov
end;

//

//in:r14(addr)
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

 movqq %r14, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rip(%r13) //save %r14 to tf_rip

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

 movqq             jit_frame.tf_r13(%r13),%r14
 movqq %r14,kthread.td_frame.tf_r13(%r15)

 movqq             jit_frame.tf_rsp(%r13),%r14
 movqq %r14,kthread.td_frame.tf_rsp(%r15)

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

 movqq kthread.td_frame.tf_r13(%r15),%r14
 movqq   %r14,jit_frame.tf_r13(%r13)

 movqq kthread.td_frame.tf_rsp(%r15),%r14
 movqq   %r14,jit_frame.tf_rsp(%r13)

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

 pushf
 pop   %rdi
 movqq %rdi, - kthread.td_frame.tf_r13 + kthread.td_frame.tf_rflags(%r13);

 lea - kthread.td_frame.tf_r13 + kthread.td_fpstate(%r13), %rdi
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
 lea - kthread.td_frame.tf_r13 + kthread.td_fpstate(%r13), %rdi
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
end;

//in:r14(addr)
procedure jit_jmp_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %r14,%rdi
 mov    $0,%rsi

 call jmp_dispatcher

 mov  %rax,%r14

 call jit_load_ctx

 //epilog
 movq %rbp,%rsp
 pop  %rbp

 lea  8(%rsp),%rsp
 jmp  %r14
end;

//in:r14(addr)
procedure jit_call_dispatch; assembler; nostackframe;
asm
 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 andq  $-16,%rsp //align stack

 call jit_save_ctx

 mov  %r14,%rdi
 mov    $1,%rsi

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
 movq %rsp,%gs:teb.jit_rsp
 movq %rbp,%gs:teb.jit_rbp

 movq jit_frame.tf_rsp(%r13),%rsp
 movq jit_frame.tf_rbp(%r13),%rbp

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
 movq %gs:teb.jit_rsp,%rsp
 movq %gs:teb.jit_rbp,%rbp

 //teb
 movq - kthread.td_frame.tf_r13 + kthread.td_ustack.sttop(%r13) ,%r14
 movq - kthread.td_frame.tf_r13 + kthread.td_ustack.stack(%r13) ,%r15

 movq %r14,%gs:teb.sttop
 movq %r15,%gs:teb.stack
 //teb

 //uplift %rsp/%rbp ???
end;

procedure jit_call_internal; assembler; nostackframe;
asm
 //pop host call
 mov jit_frame.tf_rsp(%r13),%r14
 lea 8(%r14),%r14
 mov %r14,jit_frame.tf_rsp(%r13)

 //push internal call
 lea  -8(%rsp),%rsp

 //prolog (debugger)
 push %rbp
 movq %rsp,%rbp

 //set
 //%r13 ABI preserve the registers
 //%r14 ABI preserve the registers
 //%r15 ABI preserve the registers

 //call stack_set_user

 //call
 call %gs:teb.jitcall

 //restore guard
 movq %gs:teb.thread               ,%r13 //curkthread
 leaq kthread.td_frame.tf_r13(%r13),%r13 //jit_frame

 //call stack_set_jit

 //%r13 ABI preserve the registers
 //%r14 ABI preserve the registers
 //%r15 ABI preserve the registers

 //epilog
 pop  %rbp
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
 mov jit_frame.tf_rsp(%r13),%r15
 mov  (%r15),%r14
 lea 8(%r15),%r15
 mov %r15,jit_frame.tf_rsp(%r13)

 jmp  jit_call_dispatch
end;

function IS_JIT_FUNC(rip:qword):Boolean; public;
begin
 Result:=(
          (rip>=QWORD(@jit_syscall)) and
          (rip<=(QWORD(@jit_syscall)+$1A5)) //jit_syscall func size
         ) or
         (
          (rip>=QWORD(@jit_jmp_dispatch)) and
          (rip<=(QWORD(@jit_jmp_dispatch)+$30)) //jit_jmp_dispatch func size
         ) or
         (
          (rip>=QWORD(@jit_call_dispatch)) and
          (rip<=(QWORD(@jit_call_dispatch)+$30)) //jit_call_dispatch func size
         );
end;

end.



