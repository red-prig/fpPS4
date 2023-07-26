unit kern_patcher;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_stub;

procedure patcher_process_section(_obj,data,vaddr:Pointer;filesz:QWORD);

implementation

uses
 kern_thr,
 vm_pmap,
 vm_patch_link,
 trap;

{
64 48 A1 [0000000000000000] mov rax,fs:[$0000000000000000] -> 65 48 A1 [0807000000000000] mov rax,gs:[$0000000000000708]
64 48 8B 04 25 [00000000]   mov rax,fs:[$00000000]         -> 65 48 8B 04 25 [08070000]   mov rax,gs:[$00000708]
64 48 8B 0C 25 [00000000]   mov rcx,fs:[$00000000]         -> 65 48 8B 0C 25 [08070000]   mov rcx,gs:[$00000708]
64 48 8B 14 25 [00000000]   mov rdx,fs:[$00000000]         -> 65 48 8B 14 25 [08070000]   mov rdx,gs:[$00000708]
64 48 8B 1C 25 [00000000]   mov rbx,fs:[$00000000]         -> 65 48 8B 1C 25 [08070000]   mov rbx,gs:[$00000708]
64 48 8B 24 25 [00000000]   mov rsp,fs:[$00000000]         -> 65 48 8B 24 25 [08070000]   mov rsp,gs:[$00000708]
64 48 8B 2C 25 [00000000]   mov rbp,fs:[$00000000]         -> 65 48 8B 2C 25 [08070000]   mov rbp,gs:[$00000708]
64 48 8B 34 25 [00000000]   mov rsi,fs:[$00000000]         -> 65 48 8B 34 25 [08070000]   mov rsi,gs:[$00000708]
64 48 8B 3C 25 [00000000]   mov rdi,fs:[$00000000]         -> 65 48 8B 3C 25 [08070000]   mov rdi,gs:[$00000708]
64 4C 8B 04 25 [00000000]   mov r8 ,fs:[$00000000]         -> 65 4C 8B 04 25 [08070000]   mov r8 ,gs:[$00000708]
64 4C 8B 0C 25 [00000000]   mov r9 ,fs:[$00000000]         -> 65 4C 8B 0C 25 [08070000]   mov r9 ,gs:[$00000708]
64 4C 8B 14 25 [00000000]   mov r10,fs:[$00000000]         -> 65 4C 8B 14 25 [08070000]   mov r10,gs:[$00000708]
64 4C 8B 1C 25 [00000000]   mov r11,fs:[$00000000]         -> 65 4C 8B 1C 25 [08070000]   mov r11,gs:[$00000708]
64 4C 8B 24 25 [00000000]   mov r12,fs:[$00000000]         -> 65 4C 8B 24 25 [08070000]   mov r12,gs:[$00000708]
64 4C 8B 2C 25 [00000000]   mov r13,fs:[$00000000]         -> 65 4C 8B 2C 25 [08070000]   mov r13,gs:[$00000708]
64 4C 8B 34 25 [00000000]   mov r14,fs:[$00000000]         -> 65 4C 8B 34 25 [08070000]   mov r14,gs:[$00000708]
64 4C 8B 3C 25 [00000000]   mov r15,fs:[$00000000]         -> 65 4C 8B 3C 25 [08070000]   mov r15,gs:[$00000708]
}

{
[48 b8 | 10 01 00 00 00 00 00 00]:MOV RAX,0x110

[49 89 ca]:MOV R10,RCX

[0f 05]:SYSCALL

[72 01]:JC LAB

[c3]:RET

///////////////////

[48 c7 c0 | dd 01 00 00]:MOV RAX,0x1dd

[49 89 ca]:MOV R10,RCX

[0f 05]:SYSCALL

[72 01]:JC LAB

[c3]:RET

//////////

48 c7 c0 01 | 00 00 00 MOV        RAX,0x1
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
41 5c                  POP        R12
5b                     POP        RBX

[00 00 00 | 49 89 ca | 0f 05 | 41 5c]

//////////

48 c7 c0 fb | 00 00 00 MOV        RAX,0xfb
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
72 1f                  JC         LAB_
83 fa 00               CMP        EDX,0x0
75 04                  JNZ        LAB_
41 5c                  POP        R12

[00 00 00 | 49 89 ca | 0f 05 | 72 1f]

//////////

48 c7 c0 01 | 00 00 00 MOV        RAX,0x1
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
41 5c                  POP        R12
5b                     POP        RBX

//////////

48 c7 c0 3b | 00 00 00 MOV        RAX,0x3b
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
48 8d 15 c0            LEA        RDX,[FUN_]
ff ff ff
ff e2                  JMP        RDX=>FUN_

[00 00 00 | 49 89 ca | 0f 05 | 48 8d]

//////////

48 c7 c0 a5 | 01 00 00 MOV        RAX,0x1a5
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
72 06                  JC         LAB_
48 83 c4 08            ADD        RSP,0x8
ff e6                  JMP        RSI

[01 00 00 | 49 89 ca | 0f 05 | 72 06]

//////////

48 c7 c0 2a | 00 00 00 MOV        RAX,0x2a
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
72 0d                  JC         LAB_
89 07                  MOV        dword ptr

[00 00 00 | 49 89 ca | 0f 05 | 72 0d]

//////////

48 c7 c0 37 | 00 00 00 MOV        RAX,0x37
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
72 02                  JC         LAB_
48 cf                  IRETQ

[00 00 00 | 49 89 ca | 0f 05 | 72 02]

//////////

48 c7 c0 32 | 00 00 00 MOV        RAX,0x32
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
72 0e                  JC         LAB_
48 8d 15 a3 e1 08 00   LEA        RDX,[DAT_]

[00 00 00 | 49 89 ca | 0f 05 | 72 0e]

//////////

48 c7 c0 42 | 00 00 00 MOV        RAX,0x42
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
72 02                  JC         LAB_
ff e6                  JMP        RSI

[00 00 00 | 49 89 ca | 0f 05 | 72 02]

//////////

48 c7 c0 54 | 01 00 00 MOV        RAX,0x154
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
5f                     POP        RDI
48 81 bf 18            CMP        qword ptr [RDI + 0x118],0x20001
01 00 00 01
00 02 00

[01 00 00 | 49 89 ca | 0f 05 | 5f 48]

//////////

48 c7 c0 c6 | 01 00 00 MOV        RAX,0x1c6
49 89 ca               MOV        R10,RCX
0f 05                  SYSCALL
c3                     RET
90                     ??         90h

[01 00 00 | 49 89 ca | 0f 05 | c3 90]

//////////

  0  1  2    3  4  5    6  7    8  9
[0X 00 00 | 49 89 ca | 0f 05 | 41 5c]
[0X 00 00 | 49 89 ca | 0f 05 | 72 01]
[0X 00 00 | 49 89 ca | 0f 05 | 72 1f]
[0X 00 00 | 49 89 ca | 0f 05 | 48 8d]
[0X 00 00 | 49 89 ca | 0f 05 | 72 06]
[0X 00 00 | 49 89 ca | 0f 05 | 72 0d]
[0X 00 00 | 49 89 ca | 0f 05 | 72 02]
[0X 00 00 | 49 89 ca | 0f 05 | 72 0e]
[0X 00 00 | 49 89 ca | 0f 05 | 5f 48]
[0X 00 00 | 49 89 ca | 0f 05 | c3 90]
  X=0,1,2
}

{
 jmpq  0(%rip) -> [FF 25 | 00 00 00 00]
 callq 0(%rip) -> [FF 15 | 00 00 00 00]

 jmpl  [32]    -> [E9 | 00 00 00 00]
 calll [32]    -> [E8 | 00 00 00 00]
}

type
 p_patch_base_long=^t_patch_base_long;
 t_patch_base_long=packed record
  len :Byte ; //12
  inst:array[0..2] of Byte;
  addr:Int64; //teb_tcb/teb_gsbase
 end;

 p_patch_base_short=^t_patch_base_short;
 t_patch_base_short=packed record
  len :Byte ; //9
  inst:array[0..4] of Byte;
  addr:Integer; //teb_tcb/teb_gsbase
 end;

 t_patch_inst=packed record
  case Byte of
   0:(A:t_patch_base_long);
   1:(B:t_patch_base_short);
   2:(C:array[0..11] of Byte);
 end;

 p_jmpq64_trampoline=^t_jmpq64_trampoline;
 t_jmpq64_trampoline=packed record
  inst  :Word;  //FF 25
  offset:DWORD; //00
  addr  :QWORD;
 end;

 p_call32_trampoline=^t_call32_trampoline;
 t_call32_trampoline=packed record
  inst:Byte;    //E8
  addr:Integer;
 end;

const
 patch_table:array[0..33] of t_patch_inst=(
  (B:(len: 9;inst:($65,$48,$8B,$04,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$0C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$14,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$1C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$24,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$2C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$34,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$48,$8B,$3C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$04,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$0C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$14,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$1C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$24,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$2C,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$34,$25);addr:teb_tcb   )),
  (B:(len: 9;inst:($65,$4C,$8B,$3C,$25);addr:teb_tcb   )),
  (A:(len:12;inst:($65,$48,$A1        );addr:teb_tcb   )),
  //
  (B:(len: 9;inst:($65,$48,$8B,$04,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$0C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$14,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$1C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$24,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$2C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$34,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$48,$8B,$3C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$04,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$0C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$14,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$1C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$24,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$2C,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$34,$25);addr:teb_gsbase)),
  (B:(len: 9;inst:($65,$4C,$8B,$3C,$25);addr:teb_gsbase)),
  (A:(len:12;inst:($65,$48,$A1        );addr:teb_gsbase))
 );

 c_jmpq64_trampoline:t_jmpq64_trampoline=(inst:$25FF;offset:0;addr:0);
 c_call32_trampoline:t_call32_trampoline=(inst:$E8;addr:0);

function IndexInstr(var pbuf:Pointer;pend:Pointer;var offset:Int64):Integer;
var
 psrc:Pointer;
 W:DWORD;
begin
 Result:=-1;
 psrc:=pbuf;
 while (psrc<pend) do
 begin
  W:=PDWORD(psrc)^;

  Case W of
   $048B4864:Result:= 0; //fs_rax
   $0C8B4864:Result:= 1; //fs_rcx
   $148B4864:Result:= 2; //fs_rdx
   $1C8B4864:Result:= 3; //fs_rbx
   $248B4864:Result:= 4; //fs_rsp
   $2C8B4864:Result:= 5; //fs_rbp
   $348B4864:Result:= 6; //fs_rsi
   $3C8B4864:Result:= 7; //fs_rdi
   $048B4C64:Result:= 8; //fs_r8
   $0C8B4C64:Result:= 9; //fs_r9
   $148B4C64:Result:=10; //fs_r10
   $1C8B4C64:Result:=11; //fs_r11
   $248B4C64:Result:=12; //fs_r12
   $2C8B4C64:Result:=13; //fs_r13
   $348B4C64:Result:=14; //fs_r14
   $3C8B4C64:Result:=15; //fs_r15
   $00A14864:Result:=16; //fs_rax64

   $048B4865:Result:=17; //gs_rax
   $0C8B4865:Result:=18; //gs_rcx
   $148B4865:Result:=19; //gs_rdx
   $1C8B4865:Result:=20; //gs_rbx
   $248B4865:Result:=21; //gs_rsp
   $2C8B4865:Result:=22; //gs_rbp
   $348B4865:Result:=23; //gs_rsi
   $3C8B4865:Result:=24; //gs_rdi
   $048B4C65:Result:=25; //gs_r8
   $0C8B4C65:Result:=26; //gs_r9
   $148B4C65:Result:=27; //gs_r10
   $1C8B4C65:Result:=28; //gs_r11
   $248B4C65:Result:=29; //gs_r12
   $2C8B4C65:Result:=30; //gs_r13
   $348B4C65:Result:=31; //gs_r14
   $3C8B4C65:Result:=32; //gs_r15
   $00A14865:Result:=33; //gs_rax64

   $49000000,
   $49000001,
   $49000002:Result:=34; //syscall

   //shft 8
   $8B486400..$8B4864FF, //fs
   $8B4C6400..$8B4C64FF, //fs
   $A1486400..$A14864FF, //fs

   $8B486500..$8B4865FF, //gs
   $8B4C6500..$8B4C65FF, //gs
   $A1486500..$A14865FF, //gs

   $00000000..$000002FF: //sv
    begin
     Inc(psrc,1);
     Continue;
    end;

   //shft 16
   $48640000..$4864FFFF, //fs
   $4C640000..$4C64FFFF, //fs

   $48650000..$4865FFFF, //gs
   $4C650000..$4C65FFFF, //gs

   $00000300..$0002FFFF: //sv
    begin
     Inc(psrc,2);
     Continue;
    end;

   //shft 24
   $64000000..$64FFFFFF, //fs

   $65000000..$65FFFFFF, //gs

   $00030000..$00A14863, //sv
   $00A14866..$02FFFFFF: //sv
    begin
     Inc(psrc,3);
     Continue;
    end;

   else
    begin
     Inc(psrc,4);
     Continue;
    end;
  end;

  Case Result of
    0..15,
   17..32:
          begin
           if (PBYTE(psrc)[4]<>$25) then
           begin
            Inc(psrc,4);
            Continue;
           end;

           offset:=PInteger(@PBYTE(psrc)[5])^;

           if (offset>+65536) or
              (offset<-65536) then
           begin
            Inc(psrc,4);
            Continue;
           end;
          end;
       16,
       33:
          begin
           offset:=PInt64(@PBYTE(psrc)[3])^;

           if (offset>+65536) or
              (offset<-65536) then
           begin
            Inc(psrc,4);
            Continue;
           end;
          end;
       34:
          begin
           offset:=0;

           if (PDWORD(@PBYTE(psrc)[4])^<>$050FCA89) then
           begin
            Inc(psrc,4);
            Continue;
           end;

           //Write(HexStr(PBYTE(psrc)[0],2),' ');
           //Write(HexStr(PBYTE(psrc)[1],2),' ');
           //Write(HexStr(PBYTE(psrc)[2],2),' ');
           //Write(HexStr(PBYTE(psrc)[3],2),' ');
           //Write(HexStr(PBYTE(psrc)[4],2),' ');
           //Write(HexStr(PBYTE(psrc)[5],2),' ');
           //Write(HexStr(PBYTE(psrc)[6],2),' ');
           //Write(HexStr(PBYTE(psrc)[7],2),' ');
           //Write(HexStr(PBYTE(psrc)[8],2),' ');
           //Write(HexStr(PBYTE(psrc)[9],2),' ');

           Case PBYTE(psrc)[8] of
            $41,
            $48,
            $5f,
            $72,
            $c3:{Writeln('True')};
            else
             begin
              //Writeln('False');
              Inc(psrc,4);
              Continue;
             end;
           end;
          end;
   else;
  end;

  if (Result<>-1) then
  begin
   pbuf:=psrc;
   exit;
  end;

  Inc(psrc);
 end;
 Result:=-1;
end;

procedure vm_add_syscall_patch(_obj,vaddr,addr_out:Pointer);
var
 stub:p_stub_chunk;

 jmpq64_trampoline:t_jmpq64_trampoline;
 call32_trampoline:t_call32_trampoline;

 delta:Int64;
begin
 stub:=p_alloc(vaddr,SizeOf(t_jmpq64_trampoline));

 delta:=Int64(@stub^.body)-(Int64(vaddr)+SizeOf(t_call32_trampoline));
 Assert(delta<High(Integer),'vm_add_syscall_patch');

 jmpq64_trampoline:=c_jmpq64_trampoline;
 call32_trampoline:=c_call32_trampoline;

 jmpq64_trampoline.addr:=QWORD(@fast_syscall);
 call32_trampoline.addr:=Integer(delta);

 p_jmpq64_trampoline(@stub^.body)^:=jmpq64_trampoline;
 p_call32_trampoline(addr_out)^:=call32_trampoline;

 md_cacheflush(@stub^.body,SizeOf(t_jmpq64_trampoline),ICACHE);

 vm_add_patch_link(_obj,vaddr,pt_syscall,stub);
end;

procedure patcher_process_section(_obj,data,vaddr:Pointer;filesz:QWORD);
var
 addr:Pointer;
 pend:Pointer;

 offset:Int64;

 i,len:Integer;

 fs_count:Integer;
 gs_count:Integer;
 sv_count:Integer;

 procedure do_patch_base_zero(addr:PByte;i:Integer;ptype:t_patch_type);
 var
  v:Pointer;
 begin
  Move(patch_table[i].C[1],addr^,patch_table[i].C[0]);

  v:=vaddr+(Int64(addr)-Int64(data));
  vm_add_patch_link(_obj,v,ptype,nil);
 end;

 procedure do_patch_syscall(addr:PByte);
 var
  v:Pointer;
 begin
  v:=vaddr+(Int64(addr)-Int64(data));

  vm_add_syscall_patch(_obj,v,addr);
 end;

begin
 Assert(_obj<>nil,'patcher_process_section');

 fs_count:=0;
 gs_count:=0;
 sv_count:=0;

 addr:=data;
 pend:=addr+filesz;
 repeat
  offset:=0;
  i:=IndexInstr(addr,pend,offset);

  if (i=-1) then Break;

  Case i of
    0..33:
          begin
           len:=patch_table[i].C[0];
           //

           if (offset=0) then
           begin
            Case i of
              0..16:
                    begin
                     Inc(fs_count);
                     do_patch_base_zero(addr,i,pt_fsbase);
                    end;
             17..33:
                    begin
                     Inc(gs_count);
                     do_patch_base_zero(addr,i,pt_gsbase);
                    end
             else;
            end;
           end else
           begin
            //Writeln('patch with offset:',offset);
           end;
          end;
       34:
          begin
           Inc(sv_count);
           //
           do_patch_syscall(addr+3);
           len:=9;
          end
   else
    Assert(False);
  end;

  Inc(addr,len);

 until false;

 Writeln('[patcher_vaddr]:0x',HexStr(qword(vaddr),12),'..',HexStr(qword(vaddr)+filesz,12));
 Writeln('  fs_count:',fs_count);
 Writeln('  gs_count:',gs_count);
 Writeln('  sv_count:',sv_count);

 Writeln;

 //readln;
end;



end.

