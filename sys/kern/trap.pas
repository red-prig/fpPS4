unit trap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
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

procedure print_backtrace(var f:text;rip,rbp:Pointer;skipframes:sizeint);
procedure print_backtrace_td(var f:text);

procedure fast_syscall;
procedure sigcode;
procedure sigipi;

procedure jit_call;

type
 p_jit_frame=^t_jit_frame;
 t_jit_frame=packed record
  call:Pointer;
  addr:Pointer;
  reta:Pointer;
 end;

 p_jit_prolog=^t_jit_prolog;
 t_jit_prolog=packed record
  mov0   :array[0..4] of Byte;
  jit_rsp:DWORD;
  mov1   :array[0..1] of Byte;
  jframe :Pointer;
  jmp    :array[0..1] of Byte;
  jmp_ofs:DWORD;
  jitcall:Pointer;
 end;

const
 c_jit_prolog:t_jit_prolog=(
  mov0   :($65,$48,$89,$24,$25);
  jit_rsp:DWORD(QWORD(@p_teb(nil)^.jit_rsp));
  mov1   :($48,$BC);
  jframe :nil;
  jmp    :($FF,$25);
  jmp_ofs:0;
  jitcall:@jit_call;
 );

procedure switch_to_jit(frame:p_jit_frame);

function  IS_TRAP_FUNC(rip:qword):Boolean;

function  trap(frame:p_trapframe):Integer;
function  trap_pfault(frame:p_trapframe;usermode:Boolean):Integer;

implementation

uses
 errno,
 md_systm,
 vm,
 vmparam,
 vm_map,
 vm_fault,
 machdep,
 md_context,
 signal,
 kern_sig,
 sysent,
 kern_named_id,
 subr_dynlib,
 elf_nid_utils,
 ps4libdoc;

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

function fuptr(var base:Pointer):Pointer;
begin
 Result:=nil;
 md_copyin(@base,@Result,SizeOf(Pointer),nil);
end;

function fuptr(var base:QWORD):QWORD;
begin
 Result:=0;
 md_copyin(@base,@Result,SizeOf(QWORD),nil);
end;

function CaptureBacktrace(td:p_kthread;rbp:PPointer;skipframes,count:sizeint;frames:PCodePointer):sizeint;
label
 _next;
var
 adr:Pointer;

 procedure push; inline;
 begin
  if (skipframes<>0) then
  begin
   Dec(skipframes);
  end else
  if (count<>0) then
  begin
   frames[0]:=adr;
   Dec(count);
   Inc(frames);
   Inc(Result);
  end;
 end;

begin
 Result:=0;

 while (rbp<>nil) and
       (rbp<>Pointer(QWORD(-1))) and
       (count<>0) do
 begin
  adr:=fuptr(rbp[1]);
  rbp:=fuptr(rbp[0]);

  _next:

  if (adr<>nil) then
  begin
   push;
  end else
  begin
   Break;
  end;

  if (td<>nil) and
     IS_TRAP_FUNC(QWORD(adr)) then
  begin
   adr:=Pointer(td^.td_frame.tf_rip);
   rbp:=Pointer(td^.td_frame.tf_rbp);
   goto _next;
  end;

 end;
end;

type
 TLQRec=record
  Base   :Pointer;
  Addr   :Pointer;
  LastAdr:Pointer;
  LastNid:QWORD;
 end;

Function trav_proc(h_entry:p_sym_hash_entry;var r:TLQRec):Integer;
var
 adr:Pointer;
begin
 Result:=0;
 adr:=r.Base+fuptr(h_entry^.sym.st_value);
 if (adr<=r.Addr) then
 if (adr>r.LastAdr) then
 begin
  r.LastAdr:=adr;
  r.LastNid:=fuptr(h_entry^.nid);
  Result:=1;
 end;
end;

Function find_proc_lib_entry(lib_entry:p_Lib_Entry;var r:TLQRec):Integer;
var
 h_entry:p_sym_hash_entry;
begin
 Result:=0;
 h_entry:=fuptr(lib_entry^.syms.tqh_first);
 while (h_entry<>nil) do
 begin
  Result:=Result+trav_proc(h_entry,r);
  h_entry:=fuptr(h_entry^.link.tqe_next);
 end;
end;

Function find_proc_obj(obj:p_lib_info;var r:TLQRec):Integer;
var
 lib_entry:p_Lib_Entry;
begin
 Result:=0;
 lib_entry:=fuptr(obj^.lib_table.tqh_first);
 while (lib_entry<>nil) do
 begin
  Result:=Result+find_proc_lib_entry(lib_entry,r);
  lib_entry:=fuptr(lib_entry^.link.tqe_next);
 end;
end;

type
 TDynlibLineInfo=record
  func     :shortstring;
  source   :t_id_name;
  base_addr:ptruint;
  func_addr:ptruint;
 end;

function GetDynlibLineInfo(addr:ptruint;var info:TDynlibLineInfo):boolean;
var
 obj:p_lib_info;
 r:TLQRec;
 adr:QWORD;
begin
 Result:=False;
 dynlibs_lock;

 obj:=fuptr(dynlibs_info.obj_list.tqh_first);
 while (obj<>nil) do
 begin
  if (Pointer(addr)>=obj^.map_base) and
     (Pointer(addr)<(obj^.map_base+obj^.map_size)) then
  begin
   r:=Default(TLQRec);
   r.Addr:=Pointer(addr);
   r.Base:=fuptr(obj^.map_base);

   info.base_addr:=QWORD(r.Base);

   info.source:=Default(t_id_name);
   md_copyin(@obj^.name,@info.source,SizeOf(t_id_name),nil);

   if (find_proc_obj(obj,r)<>0) then
   begin
    info.func:=ps4libdoc.GetFunctName(r.LastNid);
    if (info.func='Unknow') then
    begin
     info.func:=EncodeValue64(r.LastNid);
    end;
    info.func_addr:=QWORD(r.LastAdr);
    Result:=True;
   end else
   begin
    info.func_addr:=0;

    adr:=QWORD(obj^.init_proc_addr);
    if (adr<=QWORD(r.Addr)) then
    if (adr>info.func_addr) then
    begin
     info.func:='dtInit';
     info.func_addr:=adr;
     Result:=True;
    end;

    adr:=QWORD(obj^.fini_proc_addr);
    if (adr<=QWORD(r.Addr)) then
    if (adr>info.func_addr) then
    begin
     info.func:='dtFini';
     info.func_addr:=adr;
     Result:=True;
    end;

    adr:=QWORD(obj^.entry_addr);
    if (adr<=QWORD(r.Addr)) then
    if (adr>info.func_addr) then
    begin
     info.func:='Entry';
     info.func_addr:=adr;
     Result:=True;
    end;

   end;

   dynlibs_unlock;
   Exit;
  end;
  //
  obj:=fuptr(obj^.link.tqe_next);
 end;

 dynlibs_unlock;
end;

function find_obj_by_handle(id:Integer):p_lib_info;
var
 obj:p_lib_info;
begin
 Result:=nil;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if (obj^.id=id) then
  begin
   Exit(obj);
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;
end;

procedure print_frame(var f:text;frame:Pointer);
var
 info:TDynlibLineInfo;
 offset1:QWORD;
 offset2:QWORD;
begin
 if is_guest_addr(ptruint(frame)) then
 begin
  info:=Default(TDynlibLineInfo);

  if GetDynlibLineInfo(ptruint(frame),info) then
  begin
   offset1:=QWORD(frame)-QWORD(info.base_addr);
   offset2:=QWORD(frame)-QWORD(info.func_addr);

   Writeln(f,'  offset $00X',HexStr(offset1,6),'  ',info.source,':',info.func,'+$',HexStr(offset2,6));
  end else
  begin
   if (info.base_addr<>0) then
   begin
    offset1:=QWORD(frame)-QWORD(info.base_addr);

    Writeln(f,'  offset $00X',HexStr(offset1,6),'  ',info.source);
   end else
   begin
    Writeln(f,'  $',HexStr(frame),'  ',info.source);
   end;
  end;
 end else
 if (BackTraceStrFunc<>nil) then
 begin
  Writeln(f,BackTraceStrFunc(frame));
 end else
 begin
  Writeln(f,'  $',HexStr(frame));
 end;
end;

procedure print_backtrace(var f:text;rip,rbp:Pointer;skipframes:sizeint);
var
 i,count:sizeint;
 frames:array [0..255] of codepointer;
begin
 count:=max_frame_dump;
 count:=30;

 print_frame(f,rip);

 count:=CaptureBacktrace(curkthread,rbp,skipframes,count,@frames[0]);

 if (count<>0) then
 for i:=0 to count-1 do
 begin
  print_frame(f,frames[i]);
 end;
end;

procedure print_backtrace_td(var f:text);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;
 //
 print_backtrace(stderr,Pointer(td^.td_frame.tf_rip),Pointer(td^.td_frame.tf_rbp),0);
end;

type
 tsyscall=function(rdi,rsi,rdx,rcx,r8,r9:QWORD):Integer;

var
 sys_args_idx:array[0..5] of Byte=(
  Byte(ptruint(@p_trapframe(nil)^.tf_rdi) div SizeOf(QWORD)),
  Byte(ptruint(@p_trapframe(nil)^.tf_rsi) div SizeOf(QWORD)),
  Byte(ptruint(@p_trapframe(nil)^.tf_rdx) div SizeOf(QWORD)),
  Byte(ptruint(@p_trapframe(nil)^.tf_r10) div SizeOf(QWORD)),
  Byte(ptruint(@p_trapframe(nil)^.tf_r8 ) div SizeOf(QWORD)),
  Byte(ptruint(@p_trapframe(nil)^.tf_r9 ) div SizeOf(QWORD))
 );

procedure amd64_syscall;
var
 td:p_kthread;
 td_frame:p_trapframe;
 scall:tsyscall;
 error:Integer;
 i,count:Integer;
 is_guest:Boolean;
begin
 //Call directly to the address or make an ID table?

 td:=curkthread;
 td_frame:=@td^.td_frame;

 cpu_fetch_syscall_args(td);

 error:=0;
 scall:=nil;
 is_guest:=False;

 if (td_frame^.tf_rax<=High(sysent_table)) then
 begin
  scall:=tsyscall(sysent_table[td_frame^.tf_rax].sy_call);
  if (scall=nil) then
  begin
   Writeln('Unhandled syscall:',td_frame^.tf_rax,':',sysent_table[td_frame^.tf_rax].sy_name);

   count:=sysent_table[td_frame^.tf_rax].sy_narg;
   Assert(count<=6);

   if (count<>0) then
   For i:=0 to count-1 do
   begin
    Writeln(' [',i+1,']:0x',HexStr(PQWORD(td_frame)[sys_args_idx[i]],16));
   end;

   print_backtrace(StdErr,Pointer(td_frame^.tf_rip),Pointer(td_frame^.tf_rbp),0);

   Assert(false,sysent_table[td_frame^.tf_rax].sy_name);
  end;
 end else
 if (td_frame^.tf_rax<=$1000) then
 begin
  Writeln('Unhandled syscall:',td_frame^.tf_rax);

  count:=sysent_table[td_frame^.tf_rax].sy_narg;
  Assert(count<=6);

  if (count<>0) then
  For i:=0 to count-1 do
  begin
   Writeln(' [',i+1,']:0x',HexStr(PQWORD(td_frame)[sys_args_idx[i]],16));
  end;

  print_backtrace(StdErr,Pointer(td_frame^.tf_rip),Pointer(td_frame^.tf_rbp),0);

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
   is_guest:=True;
   Writeln('Guest syscall:',sysent_table[td_frame^.tf_rax].sy_name);

   //count:=sysent_table[td_frame^.tf_rax].sy_narg;
   //Assert(count<=6);
   //
   //if (count<>0) then
   //For i:=0 to count-1 do
   //begin
   // Writeln(' [',i+1,']:0x',HexStr(PQWORD(td_frame)[sys_args_idx[i]],16));
   //end;

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

 if is_guest then
 Case error of
  0:;
  ERESTART:;
  EJUSTRETURN:; //nothing
  else
    begin
     Writeln('Guest syscall:',sysent_table[td_frame^.tf_rax].sy_name,' error:',error);
    end;
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

 lahf           //load to AH
 movb  %ah,%ch  //save flags to CH

 movqq %gs:teb.thread,%rax //curkthread
 test  %rax,%rax
 jz    _fail

 movqq kthread.td_kstack.stack(%rax),%rsp //td_kstack (Implicit lock interrupt)
 andq  $-32,%rsp //align stack

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%rax) //clear PCB_FULL_IRET

 movqq $0  ,kthread.td_frame.tf_rflags(%rax) //clear
 movb  %ch ,kthread.td_frame.tf_rflags(%rax) //save flags

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

 //get flags
 movb  kthread.td_frame.tf_rflags(%rcx),%ah
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

 //fail (curkthread=nil)
 _fail:

 movb  %ch,%ah //get flags
 or    $1,%ah  //set CF
 sahf          //restore flags

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
 lea   sigframe.sf_uc (%rsp),%rdi
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

////

{
 //jit prolog
 asm
  6548892425 [C8010000]   movqq %rsp,%gs:teb.jit_rsp //Implicit lock interrupt
  48BC [0000000000000000] movqq $0,%rsp              //Move p_jit_frame to %rsp
  FF25       [00000000]   jmp   (%rip)               //Jump to jit_call
  [addr64 jit_call]
 end;
}

procedure switch_to_jit(frame:p_jit_frame);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;

 td^.td_teb^.jit_rsp:=Pointer(td^.td_frame.tf_rsp);
 td^.td_frame.tf_rsp:=QWORD(frame);
 td^.td_frame.tf_rip:=QWORD(@jit_call);
end;

type
 t_proc=Procedure();

procedure jit_frame_call(frame:p_jit_frame);
begin
 cpu_init_jit(curkthread);

 if QWORD(frame^.reta)-QWORD(frame^.addr)=3 then
 begin
  Writeln('tf_rcx>:',HexStr(curkthread^.td_frame.tf_rcx,16));
 end;

 t_proc(frame^.call)();

 if QWORD(frame^.reta)-QWORD(frame^.addr)=3 then
 begin
  Writeln('tf_rcx<:',HexStr(curkthread^.td_frame.tf_rcx,16));
 end;

 cpu_fini_jit(curkthread);
end;

{
//rdi
procedure jit_frame_call(frame:p_jit_frame); assembler;
var
 rdi:QWORD;
asm
 movqq %rdi,rdi

 movqq %gs:teb.thread,%rdi
 call cpu_init_jit

 movqq rdi,%rdi
 movqq t_jit_frame.call(%rdi),%rax
 callq %rax

 movqq %gs:teb.thread,%rdi
 call cpu_fini_jit
end;
}

//input:
// 1: %gs:teb.jit_rsp (original %rsp)
// 2: %rsp            (jitcall  addr)
procedure jit_call; assembler; nostackframe;
label
 _after_call,
 _fail,
 _ast;
asm
 //%rsp must be saved in %gs:teb.jit_rsp upon enter (Implicit lock interrupt)

 //save jitcall
 movqq %rsp,%gs:teb.jitcall

 //save %rax
 movqq %rax,%gs:teb.jit_rax

 lahf  //load flags to AH

 movqq %gs:teb.thread,%rsp //curkthread
 test  %rsp,%rsp
 jz    _fail

 movqq $0 ,kthread.td_frame.tf_rflags(%rsp) //clear
 movb  %ah,kthread.td_frame.tf_rflags(%rsp) //save flags

 movqq %gs:teb.jit_rax,%rax //load %rax
 movqq %rax,kthread.td_frame.tf_rax(%rsp) //save %rax

 movqq %gs:teb.jit_rsp,%rax //load %rsp
 movqq %rax,kthread.td_frame.tf_rsp(%rsp) //save %rsp

 movqq %rsp,%rax //move td to %rax
 movqq kthread.td_kstack.stack(%rax),%rsp //td_kstack (Implicit lock interrupt)
 andq  $-32,%rsp //align stack

 andl  NOT_PCB_FULL_IRET,kthread.pcb_flags(%rax) //clear PCB_FULL_IRET

 movqq %rdi,kthread.td_frame.tf_rdi(%rax)
 movqq %rsi,kthread.td_frame.tf_rsi(%rax)
 movqq %rdx,kthread.td_frame.tf_rdx(%rax)
 movqq %rcx,kthread.td_frame.tf_rcx(%rax)
 movqq %r8 ,kthread.td_frame.tf_r8 (%rax)
 movqq %r9 ,kthread.td_frame.tf_r9 (%rax)
 movqq %rbx,kthread.td_frame.tf_rbx(%rax)
 movqq %rbp,kthread.td_frame.tf_rbp(%rax)
 movqq %r10,kthread.td_frame.tf_r10(%rax)
 movqq %r11,kthread.td_frame.tf_r11(%rax)
 movqq %r12,kthread.td_frame.tf_r12(%rax)
 movqq %r13,kthread.td_frame.tf_r13(%rax)
 movqq %r14,kthread.td_frame.tf_r14(%rax)
 movqq %r15,kthread.td_frame.tf_r15(%rax)

 movqq %gs:teb.jitcall,%rdi                //get struct

 movqq t_jit_frame.reta(%rdi),%rsi         //get ret addr
 movqq %rsi,kthread.td_frame.tf_rip(%rax)  //save ret

 movqq t_jit_frame.addr(%rdi),%rsi         //get src addr
 movqq %rsi,kthread.td_frame.tf_addr(%rax) //save addr

 movqq $0  ,kthread.td_frame.tf_trapno(%rax)
 movqq $0  ,kthread.td_frame.tf_flags (%rax)
 movqq $0  ,kthread.td_frame.tf_err   (%rax)

 //clear teb.jitcall,teb.jit_rsp
 xor   %rax,%rax
 movqq %rax,%gs:teb.jitcall
 movqq %rax,%gs:teb.jit_rsp

 call  jit_frame_call //call jit code

 _after_call:

 movqq %gs:teb.thread,%rcx //curkthread

 testl TDF_AST,kthread.td_flags(%rcx)
 jne _ast

 //Restore preserved registers.
 movqq kthread.td_frame.tf_rip(%rcx),%rax //get ret addr
 movqq %rax,%gs:teb.jitcall               //save ret

 //get flags
 movb  kthread.td_frame.tf_rflags(%rcx),%ah
 sahf  //restore flags

 movqq kthread.td_frame.tf_rdi(%rcx),%rdi
 movqq kthread.td_frame.tf_rsi(%rcx),%rsi
 movqq kthread.td_frame.tf_rdx(%rcx),%rdx
 movqq kthread.td_frame.tf_r8 (%rcx),%r8
 movqq kthread.td_frame.tf_r9 (%rcx),%r9
 movqq kthread.td_frame.tf_rax(%rcx),%rax
 movqq kthread.td_frame.tf_rbx(%rcx),%rbx
 movqq kthread.td_frame.tf_rbp(%rcx),%rbp
 movqq kthread.td_frame.tf_r10(%rcx),%r10
 movqq kthread.td_frame.tf_r11(%rcx),%r11
 movqq kthread.td_frame.tf_r12(%rcx),%r12
 movqq kthread.td_frame.tf_r13(%rcx),%r13
 movqq kthread.td_frame.tf_r14(%rcx),%r14
 movqq kthread.td_frame.tf_r15(%rcx),%r15
 movqq kthread.td_frame.tf_rsp(%rcx),%rsp

 //last restore
 movqq kthread.td_frame.tf_rcx(%rcx),%rcx

 //ret
 jmpq  %gs:teb.jitcall

 //fail (curkthread=nil)
 _fail:

 sahf  //restore flags from AH

 movqq %gs:teb.jitcall       ,%rax //get struct
 movqq t_jit_frame.reta(%rax),%rax //get ret addr
 movqq %rax,%gs:teb.jitcall        //save ret

 movqq %gs:teb.jit_rax,%rax //restore %rax
 xchgq %gs:teb.jit_rsp,%rsp //restore %rsp (and also set teb.jit_rsp=0)

 //ret
 jmpq  %gs:teb.jitcall

 //ast
 _ast:

  call ast
  jmp _after_call
end;

function IS_TRAP_FUNC(rip:qword):Boolean;
begin
 Result:=(
          (rip>=QWORD(@fast_syscall)) and
          (rip<=(QWORD(@fast_syscall)+$19C)) //fast_syscall func size
         ) or
         (
          (rip>=QWORD(@jit_call)) and
          (rip<=(QWORD(@jit_call)+$219)) //jit_call func size
         );
end;

function IS_USERMODE(td:p_kthread;frame:p_trapframe):Boolean; inline;
begin
 Result:=(frame^.tf_rsp>QWORD(td^.td_kstack.stack)) or (frame^.tf_rsp<=(QWORD(td^.td_kstack.sttop)));
end;

function trap(frame:p_trapframe):Integer;
begin
 Result:=0;

 case frame^.tf_trapno of
  T_PAGEFLT:
    begin
     Result:=trap_pfault(frame,IS_USERMODE(curkthread,frame));

     //print_backtrace_td(stderr);
     //writeln;

    end;

 end;

end;

procedure trap_fatal(frame:p_trapframe;eva:vm_offset_t);
var
 td:p_kthread;
 trapno:Integer;
 msg,msg2:pchar;
begin
 trapno:=frame^.tf_trapno;

 if (trapno <= MAX_TRAP_MSG) then
  msg:=trap_msg[trapno]
 else
  msg:='UNKNOWN';

 if IS_USERMODE(curkthread,frame) then
  msg2:='user'
 else
  msg2:='kernel';

 Writeln(StdErr,'Fatal trap ',trapno,': ',msg,' while in ',msg2,' mode');

 Writeln(StdErr,'fault virtual address = 0x',HexStr(eva,16));
 Writeln(StdErr,'instruction pointer   = 0x',HexStr(frame^.tf_rip,16));
 Writeln(StdErr,'stack pointer         = 0x',HexStr(frame^.tf_rsp,16));
 Writeln(StdErr,'frame pointer         = 0x',HexStr(frame^.tf_rbp,16));

 td:=curkthread;
 if (td<>nil) then
 begin
 Writeln(StdErr,'td_ustack.stack       = 0x',HexStr(td^.td_ustack.stack));
 Writeln(StdErr,'td_ustack.sttop       = 0x',HexStr(td^.td_ustack.sttop));
 Writeln(StdErr,'td_kstack.stack       = 0x',HexStr(td^.td_kstack.stack));
 Writeln(StdErr,'td_kstack.sttop       = 0x',HexStr(td^.td_kstack.sttop));
 end;
end;

function trap_pfault(frame:p_trapframe;usermode:Boolean):Integer;
var
 td:p_kthread;
 eva:vm_offset_t;
 map:vm_map_t;
 rv:Integer;
begin
 Result:=SIGSEGV;

 td:=curkthread;
 eva:=frame^.tf_addr;

 if ((td^.td_pflags and TDP_NOFAULTING)<>0) then
 begin
  Writeln('TDP_NOFAULTING:',curkthread^.td_name);
  Exit(SIGSEGV);
 end;

 if is_guest_addr(eva) then
 begin
  map:=@g_vmspace.vm_map;

  rv:=vm_fault.vm_fault(map,
                        frame^.tf_addr,
                        frame^.tf_rip,
                        frame^.tf_err,
                        VM_FAULT_NORMAL);

  case rv of
                         0:Exit(0);
   KERN_PROTECTION_FAILURE:Result:=SIGBUS;
   else
                           Result:=SIGSEGV;
  end;

 end else
 begin
  Writeln('not is_guest_addr:0x',HexStr(eva,16),':',curkthread^.td_name);
  Result:=SIGSEGV;
 end;

 if (not usermode) then
 begin
  if (td^.pcb_onfault<>nil) then
  begin
   Writeln('pcb_onfault<:',HexStr(td^.pcb_onfault),':',curkthread^.td_name);
   frame^.tf_rip:=QWORD(td^.pcb_onfault);
   Exit(0);
  end else
  begin
   trap_fatal(frame, eva);
   Exit(-1);
  end;
 end;

end;



end.

