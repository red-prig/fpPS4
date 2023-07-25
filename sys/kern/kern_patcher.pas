unit kern_patcher;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

procedure patcher_process_section(_obj,data,vaddr:Pointer;filesz:QWORD);

implementation

uses
 windows,

 mqueue,
 hamt,
 kern_rwlock,
 kern_thr,
 vm,
 vm_map,
 vm_mmap,
 vm_object,
 vm_pmap,
 trap,
 kern_stub;

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
 t_patch_base_long=packed record
  len :Byte ; //12
  inst:array[0..2] of Byte;
  addr:QWORD; //teb_tcb/teb_gsbase
 end;

 t_patch_base_short=packed record
  len :Byte ; //9
  inst:array[0..4] of Byte;
  addr:DWORD; //teb_tcb/teb_gsbase
 end;

 t_patch_inst=packed record
  case Byte of
   0:(A:t_patch_base_long);
   1:(B:t_patch_base_short);
   2:(C:array[0..11] of Byte);
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

function IndexInstr(var pbuf:Pointer;pend:Pointer):Integer;
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
           if (PBYTE(psrc)[4]<>$25) or
              (PDWORD(@PBYTE(psrc)[5])^<>$00000000) then
           begin
            Inc(psrc,4);
            Continue;
           end;
          end;
       16,
       33:
          begin
           if (PQWORD(@PBYTE(psrc)[3])^<>$0000000000000000) then
           begin
            Inc(psrc,4);
            Continue;
           end;
          end;
       34:
          begin
           if (PDWORD(@PBYTE(psrc)[4])^<>$050FCA89) then
           begin
            Inc(psrc,4);
            Continue;
           end;

           Write(HexStr(PBYTE(psrc)[0],2),' ');
           Write(HexStr(PBYTE(psrc)[1],2),' ');
           Write(HexStr(PBYTE(psrc)[2],2),' ');
           Write(HexStr(PBYTE(psrc)[3],2),' ');
           Write(HexStr(PBYTE(psrc)[4],2),' ');
           Write(HexStr(PBYTE(psrc)[5],2),' ');
           Write(HexStr(PBYTE(psrc)[6],2),' ');
           Write(HexStr(PBYTE(psrc)[7],2),' ');
           Write(HexStr(PBYTE(psrc)[8],2),' ');
           Write(HexStr(PBYTE(psrc)[9],2),' ');

           Case PBYTE(psrc)[8] of
            $41,
            $48,
            $5f,
            $72,
            $c3:Writeln('True');
            else
             begin
              Writeln('False');
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

procedure patcher_process_section(_obj,data,vaddr:Pointer;filesz:QWORD);
var
 obj:vm_object_t;
 addr:Pointer;
 pend:Pointer;
 i,len:Integer;

 fs_count:Integer;
 gs_count:Integer;
 sv_count:Integer;

 procedure do_patch_base(P:PByte;i:Integer); inline;
 begin
  Move(patch_table[i].C[1],P^,patch_table[i].C[0]);
 end;

begin
 Assert(_obj<>nil,'patcher_process_section');
 obj:=_obj;

 fs_count:=0;
 gs_count:=0;
 sv_count:=0;

 addr:=data;
 pend:=addr+filesz;
 repeat
  i:=IndexInstr(addr,pend);

  if (i=-1) then Break;

  Case i of
    0..16:
          begin
           Inc(fs_count);
           //
           len:=patch_table[i].C[0];
           do_patch_base(addr,i);
          end;
   17..33:
          begin
           Inc(gs_count);
           //
           len:=patch_table[i].C[0];
           do_patch_base(addr,i);
          end;
       34:
          begin
           Inc(sv_count);
           //
           len:=9;
          end
   else
    Assert(False);
  end;

  Inc(addr,len);

 until false;

 Writeln('[patcher_vaddr]:0x',HexStr(vaddr));
 Writeln('  fs_count:',fs_count);
 Writeln('  gs_count:',gs_count);
 Writeln('  sv_count:',sv_count);

 Writeln;

 //readln;
end;



end.

