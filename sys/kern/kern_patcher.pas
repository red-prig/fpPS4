unit kern_patcher;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_stub;

procedure patcher_process_section(_obj,data,vaddr:Pointer;filesz:QWORD);

implementation

uses
 kern_thr,
 vm_pmap,
 vm_patch_link,
 trap,
 x86_index_instr;

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
90488B80 [11111111]  mov rax,[rax+$11111111]
90488B89 [11111111]  mov rcx,[rcx+$11111111]
90488B92 [11111111]  mov rdx,[rdx+$11111111]
90488B9B [11111111]  mov rbx,[rbx+$11111111]
488BA424 [11111111]  mov rsp,[rsp+$11111111]
90488BAD [11111111]  mov rbp,[rbp+$11111111]
90488BB6 [11111111]  mov rsi,[rsi+$11111111]
90488BBF [11111111]  mov rdi,[rdi+$11111111]
904D8B80 [11111111]  mov r8 ,[r8 +$11111111]
904D8B89 [11111111]  mov r9 ,[r9 +$11111111]
904D8B92 [11111111]  mov r10,[r10+$11111111]
904D8B9B [11111111]  mov r11,[r11+$11111111]
4D8BA424 [11111111]  mov r12,[r12+$11111111]
904D8BAD [11111111]  mov r13,[r13+$11111111]
904D8BB6 [11111111]  mov r14,[r14+$11111111]
904D8BBF [11111111]  mov r15,[r15+$11111111]
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
 {
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
 }

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

 p_mov_rel_base32=^t_mov_rel_base32;
 t_mov_rel_base32=packed record
  inst:array[0..4] of Byte;
  addr:Integer; //teb_tcb/teb_gsbase
 end;

 p_mov_rel32=^t_mov_rel32;
 t_mov_rel32=packed record
  inst:array[0..3] of Byte;
  addr:Integer; //offset
 end;

 p_base_data_trampoline=^t_base_data_trampoline;
 t_base_data_trampoline=packed record
  bseg:t_mov_rel_base32;
  data:t_mov_rel32;
  bjmp:t_jmpq64_trampoline;
 end;

const
  {
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
 }

 patch_fs_table:array[0..15] of t_mov_rel_base32=(
  (inst:($65,$48,$8B,$04,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$0C,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$14,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$1C,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$24,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$2C,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$34,$25);addr:teb_tcb),
  (inst:($65,$48,$8B,$3C,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$04,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$0C,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$14,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$1C,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$24,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$2C,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$34,$25);addr:teb_tcb),
  (inst:($65,$4C,$8B,$3C,$25);addr:teb_tcb)
 );

 patch_gs_table:array[0..15] of t_mov_rel_base32=(
  (inst:($65,$48,$8B,$04,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$0C,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$14,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$1C,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$24,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$2C,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$34,$25);addr:teb_gsbase),
  (inst:($65,$48,$8B,$3C,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$04,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$0C,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$14,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$1C,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$24,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$2C,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$34,$25);addr:teb_gsbase),
  (inst:($65,$4C,$8B,$3C,$25);addr:teb_gsbase)
 );

 patch_mov_rel_table:array[0..15] of t_mov_rel32=(
  (inst:($90,$48,$8B,$80);addr:0),
  (inst:($90,$48,$8B,$89);addr:0),
  (inst:($90,$48,$8B,$92);addr:0),
  (inst:($90,$48,$8B,$9B);addr:0),
  (inst:($48,$8B,$A4,$24);addr:0),
  (inst:($90,$48,$8B,$AD);addr:0),
  (inst:($90,$48,$8B,$B6);addr:0),
  (inst:($90,$48,$8B,$BF);addr:0),
  (inst:($90,$4D,$8B,$80);addr:0),
  (inst:($90,$4D,$8B,$89);addr:0),
  (inst:($90,$4D,$8B,$92);addr:0),
  (inst:($90,$4D,$8B,$9B);addr:0),
  (inst:($4D,$8B,$A4,$24);addr:0),
  (inst:($90,$4D,$8B,$AD);addr:0),
  (inst:($90,$4D,$8B,$B6);addr:0),
  (inst:($90,$4D,$8B,$BF);addr:0)
 );

 c_jmpq64_trampoline:t_jmpq64_trampoline=(inst:$25FF;offset:0;addr:0);
 c_call32_trampoline:t_call32_trampoline=(inst:$E8;addr:0);
 c_jmpl32_trampoline:t_call32_trampoline=(inst:$E9;addr:0);

procedure patch_original(const info:t_instr_index_info;delta:Integer;addr_out:Pointer);
var
 trampoline:t_call32_trampoline;
begin
 //fill nop
 FillChar(addr_out^,info.inlen,$90);

 case info.instr of
  I_MOV_BASE32:trampoline:=c_jmpl32_trampoline;
  I_MOV_BASE64:trampoline:=c_jmpl32_trampoline;
  I_SYSCALL   :trampoline:=c_call32_trampoline;
  else;
 end;

 trampoline.addr:=delta;
 p_call32_trampoline(addr_out)^:=trampoline;
end;

procedure vm_add_mov_base_patch(const info:t_instr_index_info;_obj,vaddr,addr_out:Pointer);
var
 stub:p_stub_chunk;

 trampoline:t_base_data_trampoline;

 delta:Int64;
begin
 stub:=p_alloc(vaddr,SizeOf(t_base_data_trampoline));

 delta:=Int64(@stub^.body)-(Int64(vaddr)+SizeOf(t_call32_trampoline));
 Assert(delta<High(Integer),'vm_add_mov_base_patch');

 patch_original(info,Integer(delta),addr_out);

 case info.sbase of
  FSBASE:trampoline.bseg:=patch_fs_table[info.inreg];
  GSBASE:trampoline.bseg:=patch_gs_table[info.inreg];
  else;
 end;

 Assert(info.osize=4,'info.osize=4');

 trampoline.data:=patch_mov_rel_table[info.inreg];
 trampoline.data.addr:=Integer(info.offst);

 trampoline.bjmp:=c_jmpq64_trampoline;
 trampoline.bjmp.addr:=(Int64(vaddr)+SizeOf(t_call32_trampoline));

 p_base_data_trampoline(@stub^.body)^:=trampoline;

 md_cacheflush(@stub^.body,SizeOf(trampoline),ICACHE);

 case info.sbase of
  FSBASE:vm_add_patch_link(_obj,vaddr,pt_fsbase,stub);
  GSBASE:vm_add_patch_link(_obj,vaddr,pt_gsbase,stub);
  else;
 end;
end;

procedure vm_add_syscall_patch(const info:t_instr_index_info;_obj,vaddr,addr_out:Pointer);
var
 stub:p_stub_chunk;
 trampoline:t_jmpq64_trampoline;
 delta:Int64;
begin
 stub:=p_alloc(vaddr,SizeOf(t_jmpq64_trampoline));

 delta:=Int64(@stub^.body)-(Int64(vaddr)+SizeOf(t_call32_trampoline));
 Assert(delta<High(Integer),'vm_add_syscall_patch');

 patch_original(info,Integer(delta),addr_out);

 trampoline:=c_jmpq64_trampoline;
 trampoline.addr:=QWORD(@fast_syscall);
 p_jmpq64_trampoline(@stub^.body)^:=trampoline;

 md_cacheflush(@stub^.body,SizeOf(trampoline),ICACHE);

 vm_add_patch_link(_obj,vaddr,pt_syscall,stub);
end;

procedure vm_add_patch(const info:t_instr_index_info;_obj,vaddr,addr_out:Pointer);
begin
 case info.instr of
  I_MOV_BASE32:vm_add_mov_base_patch(info,_obj,vaddr,addr_out);
  I_MOV_BASE64:vm_add_mov_base_patch(info,_obj,vaddr,addr_out);
  I_SYSCALL   :vm_add_syscall_patch (info,_obj,vaddr,addr_out);
  else;
 end;
end;

procedure patcher_process_section(_obj,data,vaddr:Pointer;filesz:QWORD);
var
 addr:Pointer;
 pend:Pointer;

 info:t_instr_index_info;

 b:Boolean;

 fs_count:Integer;
 gs_count:Integer;
 sv_count:Integer;

 {
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
 }

  procedure do_patch_syscall(addr:PByte);
  var
   v:Pointer;
  begin
   v:=vaddr+(Int64(addr)-Int64(data));

   vm_add_patch(info,_obj,v,addr);
  end;

begin
 Assert(_obj<>nil,'patcher_process_section');

 fs_count:=0;
 gs_count:=0;
 sv_count:=0;

 addr:=data;
 pend:=addr+filesz;
 repeat
  info:=Default(t_instr_index_info);

  b:=IndexInstr(addr,pend,info);

  if (not b) then Break;

  do_patch_syscall(addr);

  case info.instr of
   I_MOV_BASE32:Inc(fs_count);
   I_MOV_BASE64:Inc(gs_count);
   I_SYSCALL   :Inc(sv_count);
   else;
  end;

  Inc(addr,info.inlen);

 until false;

 Writeln('[patcher_vaddr]:0x',HexStr(qword(vaddr),12),'..',HexStr(qword(vaddr)+filesz,12));
 Writeln('  fs_count:',fs_count);
 Writeln('  gs_count:',gs_count);
 Writeln('  sv_count:',sv_count);

 Writeln;

 //readln;
end;



end.

