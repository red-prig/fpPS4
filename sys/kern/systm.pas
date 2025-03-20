unit systm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx;

function copystr(from,_to:pchar;maxlen:ptruint;lencopied:pptruint):Integer;
function copyin(udaddr,kaddr:Pointer;len:ptruint):Integer;
function copyin_nofault(udaddr,kaddr:Pointer;len:ptruint):Integer;
function copyinstr(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
function copyout(kaddr,udaddr:Pointer;len:ptruint):Integer;
function copyout_nofault(kaddr,udaddr:Pointer;len:ptruint):Integer;
function fubyte(var base:Byte):Byte;
function fuword32(var base:DWORD):DWORD;
function fuword64(var base:QWORD):QWORD;
function fuword(var base:Pointer):Pointer; external name 'fuword64';
function casuword32(var base:DWORD;oldval,newval:DWORD):DWORD;
function casuword64(var base:QWORD;oldval,newval:QWORD):QWORD;
function suword32(var base:DWORD;word:DWORD):DWORD;
function suword64(var base:QWORD;word:QWORD):QWORD;
function suword(var base:Pointer;word:Pointer):Pointer; external name 'suword64';

function fuptr(var base:Pointer):Pointer;
function fuptr(var base:QWORD):QWORD;

///

function  msleep(ident   :Pointer;
                 lock    :p_mtx;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer; external;

function  tsleep(ident   :Pointer;
                 priority:Integer;
                 wmesg   :PChar;
                 timo    :Int64):Integer; external;

function  pause(wmesg:PChar;timo:Int64):Integer; external;

procedure wakeup(ident:Pointer); external;
procedure wakeup_one(ident:Pointer); external;

implementation

uses
 errno,
 md_systm,
 vmparam,
 kern_thr;

{
 * copystr(from, to, maxlen, int *lencopied) - MP SAFE
 *         %rdi, %rsi, %rdx, %rcx
}
function copystr(from,_to:pchar;maxlen:ptruint;lencopied:pptruint):Integer; assembler; nostackframe;
label
 v1b,
 v4f,
 v6f,
 v7f;
asm
        movq        %rdx,%r8                        { %r8 = maxlen }

        xchgq       %rdi,%rsi
        incq        %rdx
        cld
v1b:
        decq        %rdx
        jz          v4f
        lodsb
        stosb
        orb         %al,%al
        jnz         v1b

        { Success -- 0 byte reached }
        decq        %rdx
        xorl        %eax,%eax
        jmp         v6f
v4f:
        { rdx is zero -- return ENAMETOOLONG }
        movq        $ENAMETOOLONG,%rax

v6f:

        testq       %rcx,%rcx
        jz          v7f
        { set *lencopied and return %rax }
        subq        %rdx,%r8
        movq        %r8,(%rcx)
v7f:
        //ret
end;

{
 * copyin(from_user, to_kernel, len) - MP SAFE
 *        %rdi,      %rsi,      %rdx
}
function copyin(udaddr,kaddr:Pointer;len:ptruint):Integer; assembler; nostackframe;
label
 copyin_fault,
 done_copyin;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         copyin_fault

        movqq       %gs:teb.thread,%rax
        leaq        copyin_fault(%rip),%rcx
        movqq       %rcx,kthread.pcb_onfault(%rax)
        testq       %rdx,%rdx                        { anything to do? }
        jz          done_copyin

        {
         * make sure address is valid
        }
        movq        %rdi,%rax
        addq        %rdx,%rax
        jc          copyin_fault
        movq        $VM_MAXUSER_ADDRESS,%rcx
        cmpq        %rcx,%rax
        ja          copyin_fault

        xchgq       %rdi,%rsi
        movq        %rdx,%rcx
        movb        %cl,%al
        shrq        $3,%rcx                               { copy longword-wise }
        cld
        rep
        movsq
        movb        %al,%cl
        andb        $7,%cl                                { copy remaining bytes }
        rep
        movsb

done_copyin:
        xorl        %eax,%eax
        movq        %gs:teb.thread,%rdx
        movq        %rax,kthread.pcb_onfault(%rdx)
        ret

        //ALIGN_TEXT
        nop; nop; nop; nop;
        nop; nop; nop; nop;

copyin_fault:
        movq        %gs:teb.thread,%rdx
        movq        $0,kthread.pcb_onfault(%rdx)
        movq        $EFAULT,%rax
        //ret
end;

function copyin_nofault(udaddr,kaddr:Pointer;len:ptruint):Integer;
begin
 Result:=md_copyin(udaddr,kaddr,len,nil);
end;

{
 * copyinstr(from, to, maxlen, int *lencopied) - MP SAFE
 *           %rdi, %rsi, %rdx, %rcx
 *
 *        copy a string from from to to, stop when a 0 character is reached.
 *        return ENAMETOOLONG if string is longer than maxlen, and
 *        EFAULT on protection violations. If lencopied is non-zero,
 *        return the actual length in *lencopied.
}
function copyinstr(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer; assembler; nostackframe;
label
 cpystrflt,
 cpystrflt_x,
 v1f,
 v2b,
 v3f,
 v4f;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         cpystrflt

        movq        %rdx,%r8                        { %r8 = maxlen }
        movq        %rcx,%r9                        { %r9 = *len }
        xchgq       %rdi,%rsi                       { %rdi = from, %rsi = to }
        movq        %gs:teb.thread,%rcx
        leaq        cpystrflt(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS,%rax

        { make sure 'from' is within bounds }
        subq        %rsi,%rax
        jbe         cpystrflt

        { restrict maxlen to <= VM_MAXUSER_ADDRESS-from }
        cmpq        %rdx,%rax
        jae         v1f
        movq        %rax,%rdx
        movq        %rax,%r8
v1f:
        incq        %rdx
        cld

v2b:
        decq        %rdx
        jz          v3f

        lodsb
        stosb
        orb         %al,%al
        jnz         v2b

        { Success -- 0 byte reached }
        decq        %rdx
        xorl        %eax,%eax
        jmp         cpystrflt_x
v3f:
        { rdx is zero - return ENAMETOOLONG or EFAULT }
        movq        $VM_MAXUSER_ADDRESS,%rax
        cmpq        %rax,%rsi
        jae         cpystrflt

        movq        $ENAMETOOLONG,%rax
        jmp         cpystrflt_x

cpystrflt:
        movq        $EFAULT,%rax

cpystrflt_x:
        { set *lencopied and return %eax }
        movq        %gs:teb.thread,%rcx
        movq        $0,kthread.pcb_onfault(%rcx)

        testq       %r9,%r9
        jz          v4f
        subq        %rdx,%r8
        movq        %r8,(%r9)
v4f:
        //ret
end;

{
 * copyout(from_kernel, to_user, len)  - MP SAFE
 *         %rdi,        %rsi,    %rdx
}
function copyout(kaddr,udaddr:Pointer;len:ptruint):Integer; assembler; nostackframe;
label
 copyout_fault,
 done_copyout;
asm
        cmpq        $0x4000,%rsi //rsi <= 0x4000
        jle         copyout_fault

        movqq       %gs:teb.thread,%rax
        leaq        copyout_fault(%rip),%rcx
        movqq       %rcx,kthread.pcb_onfault(%rax)
        testq       %rdx,%rdx                        { anything to do? }
        jz          done_copyout

        {
         * First, prevent address wrapping.
        }
        movq        %rsi,%rax
        addq        %rdx,%rax
        jc          copyout_fault

        movq        $VM_MAXUSER_ADDRESS,%rcx
        cmpq        %rcx,%rax
        ja          copyout_fault

        xchgq        %rdi,%rsi
        { bcopy(%rsi, %rdi, %rdx) }
        movq        %rdx,%rcx

        shrq        $3,%rcx
        cld
        rep
        movsq
        movb        %dl,%cl
        andb        $7,%cl
        rep
        movsb

done_copyout:
        xorl        %eax,%eax
        movq        %gs:teb.thread,%rdx
        movq        %rax,kthread.pcb_onfault(%rdx)
        ret

        //ALIGN_TEXT
        nop; nop; nop; nop;
        nop; nop; nop; nop;
        nop; nop;

copyout_fault:
        movqq       %gs:teb.thread,%rax
        movq        $0,kthread.pcb_onfault(%rdx)
        movq        $EFAULT,%rax
        //ret
end;

function copyout_nofault(kaddr,udaddr:Pointer;len:ptruint):Integer;
begin
 Result:=md_copyout(kaddr,udaddr,len,nil);
end;

function fusufault:QWORD; assembler; nostackframe;
asm
        movq        %gs:teb.thread,%rcx
        xorl        %eax,%eax
        movq        %rax,kthread.pcb_onfault(%rcx)
        decq        %rax
        //ret
end;

function fubyte(var base:Byte):Byte; assembler; nostackframe;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-1,%rax
        cmpq        %rax,%rdi
        ja          fusufault

        movzbl      (%rdi),%eax
        movq        $0,kthread.pcb_onfault(%rcx)
        //ret
end;

function fuword32(var base:DWORD):DWORD; assembler; nostackframe;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-4,%rax
        cmpq        %rax,%rdi                        { verify address is valid }
        ja          fusufault

        movl        (%rdi),%eax
        movq        $0,kthread.pcb_onfault(%rcx)
        //ret
end;


function fuword64(var base:QWORD):QWORD; assembler; nostackframe; public;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-8,%rax
        cmpq        %rax,%rdi                        { verify address is valid }
        ja          fusufault

        movq        (%rdi),%rax
        movq        $0,kthread.pcb_onfault(%rcx)
        //ret
end;

{
 * casuword32.  Compare and set user integer.  Returns -1 or the current value.
 *        dst = %rdi, old = %rsi, new = %rdx
}
function casuword32(var base:DWORD;oldval,newval:DWORD):DWORD; assembler; nostackframe;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-4,%rax
        cmpq        %rax,%rdi                        { verify address is valid }
        ja          fusufault

        movl        %esi,%eax                        { old }
        lock
        cmpxchgl    %edx,(%rdi)                      { new = %edx }

        {
         * The old value is in %eax.  If the store succeeded it will be the
         * value we expected (old) from before the store, otherwise it will
         * be the current value.
        }

        movq        %gs:teb.thread,%rcx
        movq        $0,kthread.pcb_onfault(%rcx)
        //ret
end;

{
 * casuword.  Compare and set user word.  Returns -1 or the current value.
 *        dst = %rdi, old = %rsi, new = %rdx
}
function casuword64(var base:QWORD;oldval,newval:QWORD):QWORD; assembler; nostackframe;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-4,%rax
        cmpq        %rax,%rdi                        { verify address is valid }
        ja          fusufault

        movq        %rsi,%rax                        { old }
        lock
        cmpxchgq    %rdx,(%rdi)                      { new = %rdx }

        {
         * The old value is in %eax.  If the store succeeded it will be the
         * value we expected (old) from before the store, otherwise it will
         * be the current value.
        }

        movq        %gs:teb.thread,%rcx
        movq        $0,kthread.pcb_onfault(%rcx)
        //ret
end;

{
 * Store a 32-bit word to
 * user memory.  All these functions are MPSAFE.
 * addr = %rdi, value = %rsi
}
function suword32(var base:DWORD;word:DWORD):DWORD; assembler; nostackframe;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-4,%rax
        cmpq        %rax,%rdi                        { verify address validity }
        ja          fusufault

        movl        %esi,(%rdi)
        xorl        %eax,%eax
        movq        %gs:teb.thread,%rcx
        movq        %rax,kthread.pcb_onfault(%rcx)
        //ret
end;

{
 * Store a 64-bit word to
 * user memory.  All these functions are MPSAFE.
 * addr = %rdi, value = %rsi
}
function suword64(var base:QWORD;word:QWORD):QWORD; assembler; nostackframe; public;
asm
        cmpq        $0x4000,%rdi //rdi <= 0x4000
        jle         fusufault

        movq        %gs:teb.thread,%rcx
        leaq        fusufault(%rip),%rax
        movq        %rax,kthread.pcb_onfault(%rcx)

        movq        $VM_MAXUSER_ADDRESS-8,%rax
        cmpq        %rax,%rdi                        { verify address validity }
        ja          fusufault

        movq        %rsi,(%rdi)
        xorl        %eax,%eax
        movq        %gs:teb.thread,%rcx
        movq        %rax,kthread.pcb_onfault(%rcx)
        //ret
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

end.



