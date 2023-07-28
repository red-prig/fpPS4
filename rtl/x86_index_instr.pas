unit x86_index_instr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 //instruction reg
 RAX= 0;
 RCX= 1;
 RDX= 2;
 RBX= 3;
 RSP= 4;
 RBP= 5;
 RSI= 6;
 RDI= 7;
 R8 = 8;
 R9 = 9;
 R10=10;
 R11=11;
 R12=12;
 R13=13;
 R14=14;
 R15=15;

 //instruction segment base
 FSBASE=16;
 GSBASE=17;

 //instruction id
 I_MOV_BASE32=0;
 I_MOV_BASE64=1;
 I_SYSCALL   =2;

type
 p_instr_index_info=^t_instr_index_info;
 t_instr_index_info=record
  inlen:Byte;  //instruction len
  instr:Byte;  //instruction id
  inreg:Byte;  //instruction reg
  sbase:Byte;  //instruction segment base
  osize:Byte;  //instruction data offset size
  offst:Int64; //instruction data offset
 end;

function IndexInstr(var pbuf:Pointer;pend:Pointer;var info:t_instr_index_info):Boolean;

implementation

function IndexInstr(var pbuf:Pointer;pend:Pointer;var info:t_instr_index_info):Boolean;
var
 psrc:Pointer;
 W:DWORD;
 i:Integer;
 offset:Int64;
begin
 Result:=False;

 i:=-1;

 psrc:=pbuf;
 while (psrc<pend) do
 begin
  W:=PDWORD(psrc)^;

  Case W of
   $048B4864:i:= 0; //fs_rax
   $0C8B4864:i:= 1; //fs_rcx
   $148B4864:i:= 2; //fs_rdx
   $1C8B4864:i:= 3; //fs_rbx
   $248B4864:i:= 4; //fs_rsp
   $2C8B4864:i:= 5; //fs_rbp
   $348B4864:i:= 6; //fs_rsi
   $3C8B4864:i:= 7; //fs_rdi
   $048B4C64:i:= 8; //fs_r8
   $0C8B4C64:i:= 9; //fs_r9
   $148B4C64:i:=10; //fs_r10
   $1C8B4C64:i:=11; //fs_r11
   $248B4C64:i:=12; //fs_r12
   $2C8B4C64:i:=13; //fs_r13
   $348B4C64:i:=14; //fs_r14
   $3C8B4C64:i:=15; //fs_r15
   $00A14864:i:=16; //fs_rax64

   $048B4865:i:=17; //gs_rax
   $0C8B4865:i:=18; //gs_rcx
   $148B4865:i:=19; //gs_rdx
   $1C8B4865:i:=20; //gs_rbx
   $248B4865:i:=21; //gs_rsp
   $2C8B4865:i:=22; //gs_rbp
   $348B4865:i:=23; //gs_rsi
   $3C8B4865:i:=24; //gs_rdi
   $048B4C65:i:=25; //gs_r8
   $0C8B4C65:i:=26; //gs_r9
   $148B4C65:i:=27; //gs_r10
   $1C8B4C65:i:=28; //gs_r11
   $248B4C65:i:=29; //gs_r12
   $2C8B4C65:i:=30; //gs_r13
   $348B4C65:i:=31; //gs_r14
   $3C8B4C65:i:=32; //gs_r15
   $00A14865:i:=33; //gs_rax64

   $49000000,
   $49000001,
   $49000002:i:=34; //syscall

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

  Case i of
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

           info.inlen:=9;
           info.instr:=I_MOV_BASE32;

           case i of
             0..15:info.inreg:=i;
            17..32:info.inreg:=i-17;
            else;
           end;

           case i of
             0..15:info.sbase:=FSBASE;
            17..32:info.sbase:=GSBASE;
            else;
           end;

           info.osize:=4;
           info.offst:=offset;
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

           info.inlen:=12;
           info.instr:=I_MOV_BASE64;
           info.inreg:=RAX;

           case i of
             16:info.sbase:=FSBASE;
             33:info.sbase:=GSBASE;
            else;
           end;

           info.osize:=8;
           info.offst:=offset;
          end;
       34:
          begin
           if (PDWORD(@PBYTE(psrc)[4])^<>$050FCA89) then
           begin
            Inc(psrc,4);
            Continue;
           end;

           Case PBYTE(psrc)[8] of
            $41,
            $48,
            $5f,
            $72,
            $c3:;
            else
             begin
              Inc(psrc,4);
              Continue;
             end;
           end;

           Inc(psrc,3);

           info.inlen:=5;
           info.instr:=I_SYSCALL;
           info.sbase:=0;
           info.osize:=0;
           info.offst:=0;
          end;
   else;
  end;

  if (i<>-1) then
  begin
   pbuf:=psrc;
   Exit(True);
  end;

  Inc(psrc);
 end; //while
end;

end.

