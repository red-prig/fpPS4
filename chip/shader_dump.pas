unit shader_dump;

{$mode objfpc}{$H+}

interface

uses
 Classes,
 SysUtils,
 ps4_program,
 si_ci_vi_merged_offset,
 ps4_shader,
 ps4_gpu_regs;

type
 TDUMP_WORD=packed record
  REG,COUNT:WORD;
 end;

Function  FastHash(data:PByte;len:DWORD):DWORD;
Procedure DUMP_BLOCK(F:THandle;REG:WORD;P:Pointer;Size:DWORD);
Procedure DumpCS(var GPU_REGS:TGPU_REGS);
Procedure DumpPS(var GPU_REGS:TGPU_REGS);
Procedure DumpVS(var GPU_REGS:TGPU_REGS);

implementation

Function FastHash(data:PByte;len:DWORD):DWORD;
var
 hash,tmp,rem:DWORD;
begin
 if (len=0) or (data=nil) then Exit(0);
 hash:=len;

 rem:=len and 3;
 len:=len shr 2;

 While (len>0) do
 begin
  tmp :=PDWORD(data)[0];
  hash:=hash+PWORD(@tmp)[0];
  tmp :=(PWORD(@tmp)[1] shl 11) xor hash;
  hash:=(hash shl 16) xor tmp;
  data:=@PWORD(data)[2];
  hash:=hash+(hash shr 11);
  Dec(len);
 end;

 Case rem of
  3:
   begin
    hash:=hash+PWORD(data)[0];
    hash:=hash xor (hash shl 16);
    hash:=hash xor (PShortint(data)[2] shl 18);
    hash:=hash+(hash shr 11);
   end;
  2:
   begin
    hash:=hash+PWORD(data)[0];
    hash:=hash xor (hash shl 11);
    hash:=hash+(hash shr 17);
   end;
  1:
   begin
    hash:=hash+PShortint(data)[0];
    hash:=hash xor (hash shl 10);
    hash:=hash+(hash shr 1);
   end;
 end;

 hash:=hash xor (hash shl 3);
 hash:=hash+(hash shr 5);
 hash:=hash xor (hash shl 4);
 hash:=hash+(hash shr 17);
 hash:=hash xor (hash shl 25);
 hash:=hash+(hash shr 6);

 Result:=hash;
end;

Procedure DUMP_BLOCK(F:THandle;REG:WORD;P:Pointer;Size:DWORD);
const
 MAX_SIZE=($FFFF+1)*4;
var
 W:TDUMP_WORD;
begin
 if (F=feInvalidHandle) then Exit;
 if (Size=0) or (P=nil) then Exit;
 if (Size>MAX_SIZE) then Size:=MAX_SIZE;
 W.REG  :=REG;
 W.COUNT:=((Size+3) div 4)-1;
 FileWrite(F,W,SizeOf(W));
 FileWrite(F,P^,System.Align(Size,4));
end;

function getCodeAddress(lo,hi:DWORD):Pointer;
begin
 Result:=Pointer(((QWORD(hi) shl 40) or (QWORD(lo) shl 8)));
end;

function getFetchAddress(P:PDWORD):Pointer;
begin
 Result:=Pointer(((QWORD(P[1]) shl 32) or (QWORD(P[0]) and (not 3))));
end;

type
 TUSER_DATA_USEAGE=array[0..15] of Byte;

Procedure _calc_usage(info:PShaderBinaryInfo;var USER_DATA:TSPI_USER_DATA;var USEAGE_DATA:TUSER_DATA_USEAGE);
var
 i:Integer;
 Slots:PInputUsageSlot;
 r:Byte;
begin
 USEAGE_DATA:=Default(TUSER_DATA_USEAGE);
 if (info<>nil) then
 begin
  Slots:=_calc_shader_slot(info);
  if (Slots<>nil) then
   For i:=0 to info^.numInputUsageSlots-1 do
    if (Slots[i].m_usageType=kShaderInputUsageSubPtrFetchShader) then
    begin
     r:=Slots[i].m_startRegister;
     Assert(r<15);
     USEAGE_DATA[r]:=2;
     USEAGE_DATA[r+1]:=1;
    end;
 end;
 For i:=0 to 15 do
  if (USEAGE_DATA[i]=0) and (USER_DATA[i]=0) then
  begin
   USEAGE_DATA[i]:=1;
  end;
end;

Procedure DumpCS(var GPU_REGS:TGPU_REGS);
var
 i:Integer;
 size,hash:DWORD;
 base,Fetch:Pointer;
 F:THandle;
 fname:RawByteString;
 USEAGE_DATA:TUSER_DATA_USEAGE;
begin
 base:=getCodeAddress(GPU_REGS.SPI.CS.LO,GPU_REGS.SPI.CS.HI);
 if (base<>nil) then
 begin
  size:=_calc_shader_size(base);

  _calc_usage(_calc_shader_info(base),GPU_REGS.SPI.CS.USER_DATA,USEAGE_DATA);

  hash:=FastHash(base,size);
  fname:='shader_dump\'+get_dev_progname+'_cs_'+HexStr(hash,8)+'.dump';

  CreateDir('shader_dump');
  F:=FileCreate(fname);
  DUMP_BLOCK(F,mmCOMPUTE_PGM_LO,base,size);

  DUMP_BLOCK(F,mmCOMPUTE_PGM_RSRC1   ,@GPU_REGS.SPI.CS.RSRC1       ,SizeOf(DWORD));
  DUMP_BLOCK(F,mmCOMPUTE_PGM_RSRC2   ,@GPU_REGS.SPI.CS.RSRC2       ,SizeOf(DWORD));

  DUMP_BLOCK(F,mmCOMPUTE_NUM_THREAD_X,@GPU_REGS.SPI.CS.NUM_THREAD_X,SizeOf(DWORD));
  DUMP_BLOCK(F,mmCOMPUTE_NUM_THREAD_Y,@GPU_REGS.SPI.CS.NUM_THREAD_Y,SizeOf(DWORD));
  DUMP_BLOCK(F,mmCOMPUTE_NUM_THREAD_Z,@GPU_REGS.SPI.CS.NUM_THREAD_Z,SizeOf(DWORD));

  For i:=0 to 15 do
  begin
   Case USEAGE_DATA[i] of
    0:DUMP_BLOCK(F,mmCOMPUTE_USER_DATA_0+i,@GPU_REGS.SPI.CS.USER_DATA[i],SizeOf(DWORD));
    2:
     begin
      Fetch:=getFetchAddress(@GPU_REGS.SPI.CS.USER_DATA[i]);
      if (Fetch<>nil) then
      begin
       size:=_calc_shader_size(Fetch,0,True);
       DUMP_BLOCK(F,mmCOMPUTE_USER_DATA_0+i,Fetch,size);
      end;
     end;
   end;
  end;

  DUMP_BLOCK(F,mmCOMPUTE_STATIC_THREAD_MGMT_SE0,@GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE0,SizeOf(DWORD));
  DUMP_BLOCK(F,mmCOMPUTE_STATIC_THREAD_MGMT_SE1,@GPU_REGS.SPI.CS.STATIC_THREAD_MGMT_SE1,SizeOf(DWORD));
  DUMP_BLOCK(F,mmCOMPUTE_RESOURCE_LIMITS       ,@GPU_REGS.SPI.CS.RESOURCE_LIMITS       ,SizeOf(DWORD));

  FileClose(F);

 end;
end;

Procedure DumpPS(var GPU_REGS:TGPU_REGS);
var
 i:Integer;
 size,hash:DWORD;
 base,Fetch:Pointer;
 F:THandle;
 fname:RawByteString;
 USEAGE_DATA:TUSER_DATA_USEAGE;
begin
 base:=getCodeAddress(GPU_REGS.SPI.PS.LO,GPU_REGS.SPI.PS.HI);
 if (base<>nil) then
 begin
  size:=_calc_shader_size(base);

  _calc_usage(_calc_shader_info(base),GPU_REGS.SPI.PS.USER_DATA,USEAGE_DATA);

  hash:=FastHash(base,size);
  fname:='shader_dump\'+get_dev_progname+'_ps_'+HexStr(hash,8)+'.dump';

  CreateDir('shader_dump');
  F:=FileCreate(fname);
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_LO_PS,base,size);

  DUMP_BLOCK(F,mmSPI_SHADER_PGM_RSRC1_PS,@GPU_REGS.SPI.PS.RSRC1,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_RSRC2_PS,@GPU_REGS.SPI.PS.RSRC2,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_RSRC3_PS,@GPU_REGS.SPI.PS.RSRC3,SizeOf(DWORD));

  DUMP_BLOCK(F,mmSPI_SHADER_Z_FORMAT    ,@GPU_REGS.SPI.PS.Z_FORMAT  ,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_SHADER_COL_FORMAT  ,@GPU_REGS.SPI.PS.COL_FORMAT,SizeOf(DWORD));

  DUMP_BLOCK(F,mmSPI_PS_INPUT_ENA       ,@GPU_REGS.SPI.PS.INPUT_ENA ,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_PS_INPUT_ADDR      ,@GPU_REGS.SPI.PS.INPUT_ADDR,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_PS_IN_CONTROL      ,@GPU_REGS.SPI.PS.IN_CONTROL,SizeOf(DWORD));

  DUMP_BLOCK(F,mmSPI_BARYC_CNTL         ,@GPU_REGS.SPI.PS.BARYC_CNTL,SizeOf(DWORD));

  DUMP_BLOCK(F,mmDB_SHADER_CONTROL      ,@GPU_REGS.SPI.PS.SHADER_CONTROL,SizeOf(DWORD));
  DUMP_BLOCK(F,mmCB_SHADER_MASK         ,@GPU_REGS.SPI.PS.SHADER_MASK   ,SizeOf(DWORD));

  For i:=0 to 15 do
  begin
   Case USEAGE_DATA[i] of
    0:DUMP_BLOCK(F,mmSPI_SHADER_USER_DATA_PS_0+i,@GPU_REGS.SPI.PS.USER_DATA[i],SizeOf(DWORD));
    2:
     begin
      Fetch:=getFetchAddress(@GPU_REGS.SPI.PS.USER_DATA[i]);
      if (Fetch<>nil) then
      begin
       size:=_calc_shader_size(Fetch,0,True);
       DUMP_BLOCK(F,mmSPI_SHADER_USER_DATA_PS_0+i,Fetch,size);
      end;
     end;
   end;
  end;

  DUMP_BLOCK(F,mmSPI_PS_INPUT_CNTL_0,@GPU_REGS.SPI.PS.INPUT_CNTL_0,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_PS_INPUT_CNTL_1,@GPU_REGS.SPI.PS.INPUT_CNTL_1,SizeOf(DWORD));

  FileClose(F);

 end;
end;

Procedure DumpVS(var GPU_REGS:TGPU_REGS);
var
 i:Integer;
 size,hash:DWORD;
 base,Fetch:Pointer;
 F:THandle;
 fname:RawByteString;
 USEAGE_DATA:TUSER_DATA_USEAGE;
begin
 base:=getCodeAddress(GPU_REGS.SPI.VS.LO,GPU_REGS.SPI.VS.HI);
 if (base<>nil) then
 begin
  size:=_calc_shader_size(base);

  _calc_usage(_calc_shader_info(base),GPU_REGS.SPI.VS.USER_DATA,USEAGE_DATA);

  hash:=FastHash(base,size);
  fname:='shader_dump\'+get_dev_progname+'_vs_'+HexStr(hash,8)+'.dump';

  CreateDir('shader_dump');
  F:=FileCreate(fname);
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_LO_VS,base,size);

  DUMP_BLOCK(F,mmSPI_SHADER_PGM_RSRC1_VS,@GPU_REGS.SPI.VS.RSRC1,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_RSRC2_VS,@GPU_REGS.SPI.VS.RSRC2,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_RSRC3_VS,@GPU_REGS.SPI.VS.RSRC3,SizeOf(DWORD));

  DUMP_BLOCK(F,mmSPI_VS_OUT_CONFIG    ,@GPU_REGS.SPI.VS.OUT_CONFIG,SizeOf(DWORD));
  DUMP_BLOCK(F,mmSPI_SHADER_POS_FORMAT,@GPU_REGS.SPI.VS.POS_FORMAT,SizeOf(DWORD));
  DUMP_BLOCK(F,mmPA_CL_VS_OUT_CNTL    ,@GPU_REGS.SPI.VS.OUT_CNTL  ,SizeOf(DWORD));

  For i:=0 to 15 do
  begin
   Case USEAGE_DATA[i] of
    0:DUMP_BLOCK(F,mmSPI_SHADER_USER_DATA_VS_0+i,@GPU_REGS.SPI.VS.USER_DATA[i],SizeOf(DWORD));
    2:
     begin
      Fetch:=getFetchAddress(@GPU_REGS.SPI.VS.USER_DATA[i]);
      if (Fetch<>nil) then
      begin
       size:=_calc_shader_size(Fetch,0,True);
       DUMP_BLOCK(F,mmSPI_SHADER_USER_DATA_VS_0+i,Fetch,size);
      end;
     end;
   end;
  end;

  DUMP_BLOCK(F,mmVGT_NUM_INSTANCES    ,@GPU_REGS.VGT_NUM_INSTANCES,SizeOf(DWORD));

  FileClose(F);

 end;
end;

end.

