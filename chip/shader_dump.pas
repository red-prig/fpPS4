unit shader_dump;

{$mode objfpc}{$H+}

interface

uses
 Classes,
 SysUtils,
 kern_authinfo,
 kern_proc,
 murmurhash,
 si_ci_vi_merged_offset,
 ps4_shader,
 vShader,
 vRegs2Vulkan;

type
 TDUMP_WORD=packed record
  REG,COUNT:WORD;
 end;

Procedure DUMP_BLOCK(F:THandle;REG:WORD;P:Pointer;Size:DWORD);

Function  get_dev_progname:RawByteString;

function  DumpCS(var GPU_REGS:TGPU_REGS):RawByteString;
function  DumpPS(var GPU_REGS:TGPU_REGS):RawByteString;
function  DumpVS(var GPU_REGS:TGPU_REGS):RawByteString;

implementation

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

Procedure DUMP_REG(F:THandle;REG:WORD;var GPU_REGS:TGPU_REGS);
var
 DATA:DWORD;
begin
 DATA:=GPU_REGS.get_reg(REG);
 DUMP_BLOCK(F,REG,@DATA,SizeOf(DWORD));
end;

type
 TUSER_DATA_USEAGE=array[0..15] of Byte;

function _calc_usage(info:PShaderBinaryInfo;USER_DATA:PDWORD):TUSER_DATA_USEAGE;
var
 i:Integer;
 Slots:PInputUsageSlot;
 r:Byte;
begin
 Result:=Default(TUSER_DATA_USEAGE);
 if (info<>nil) then
 begin
  Slots:=_calc_shader_slot(info);
  if (Slots<>nil) then
   For i:=0 to info^.numInputUsageSlots-1 do
    Case Slots[i].m_usageType of
     kShaderInputUsageSubPtrFetchShader:
      begin
       r:=Slots[i].m_startRegister;
       Assert(r<15);
       Result[r]:=2;   //getFetchAddress
       Result[r+1]:=1; //skip
      end;
     kShaderInputUsagePtrResourceTable,
     kShaderInputUsagePtrInternalResourceTable,
     kShaderInputUsagePtrSamplerTable,
     kShaderInputUsagePtrConstBufferTable,
     kShaderInputUsagePtrVertexBufferTable,
     kShaderInputUsagePtrSoBufferTable,
     kShaderInputUsagePtrRwResourceTable,
     kShaderInputUsagePtrInternalGlobalTable,
     kShaderInputUsagePtrExtendedUserData,
     kShaderInputUsagePtrIndirectResourceTable,
     kShaderInputUsagePtrIndirectInternalResourceTable,
     kShaderInputUsagePtrIndirectRwResourceTable:
      begin
       r:=Slots[i].m_startRegister;
       Assert(r<15);
       Result[r]:=3;   //getBufferAddress
       Result[r+1]:=1; //skip
      end;
    end;
 end;
 For i:=0 to 15 do
  if (Result[i]=0) and (USER_DATA[i]=0) then
  begin
   Result[i]:=1;
  end;
end;

Procedure DUMP_USER_DATA(F:THandle;base:Pointer;REG:WORD;USER_DATA:PDWORD);
var
 i:Integer;
 buf:Pointer;
 size:DWORD;
 USEAGE_DATA:TUSER_DATA_USEAGE;
begin
 USEAGE_DATA:=_calc_usage(_calc_shader_info(base),USER_DATA);
 For i:=0 to 15 do
 begin
  Case USEAGE_DATA[i] of
   0:DUMP_BLOCK(F,REG+i,@USER_DATA[i],SizeOf(DWORD));
   2:
    begin
     buf:=getFetchAddress(USER_DATA[i],USER_DATA[i+1]);
     if (buf<>nil) then
     begin
      size:=_calc_shader_size(buf,0,True);
      DUMP_BLOCK(F,REG+i,buf,size);
     end;
    end;
   3:
    begin
     buf:=getBufferAddress(USER_DATA[i],USER_DATA[i+1]);
     if (buf<>nil) then
     begin
      size:=256; //size is unknow
      DUMP_BLOCK(F,REG+i,buf,size);
     end;
    end;

  end;
 end;
end;

function Trim(const S: RawByteString): RawByteString;
var
  Ofs, Len: sizeint;
begin
  len := Length(S);
  while (Len>0) and ((S[Len]<=' ') or (S[Len]='?')) do
   dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and ((S[Ofs]<=' ') or (S[Ofs]='?')) do
    Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end;

Function get_dev_progname:RawByteString;
begin
 Result:=Trim(g_appinfo.CUSANAME);
 if (Result='') then
 begin
  Result:=Trim(p_proc.p_comm);
 end;
end;

function DumpCS(var GPU_REGS:TGPU_REGS):RawByteString;
var
 size,hash:DWORD;
 base:Pointer;
 F:THandle;
 fname:RawByteString;
begin
 Result:='';

 base:=GPU_REGS.get_code_addr(vShaderStageCs);
 if (base<>nil) then
 begin
  size:=_calc_shader_size(base);

  hash:=MurmurHash64A(base,size,0);
  fname:='shader_dump\'+get_dev_progname+'_cs_'+HexStr(hash,8)+'.dump';
  Result:=fname;

  if FileExists(fname) then Exit;

  CreateDir('shader_dump');
  F:=FileCreate(fname);
  DUMP_BLOCK(F,mmCOMPUTE_PGM_LO,base,size);

  DUMP_REG(F,mmCOMPUTE_PGM_RSRC1   ,GPU_REGS);
  DUMP_REG(F,mmCOMPUTE_PGM_RSRC2   ,GPU_REGS);

  DUMP_REG(F,mmCOMPUTE_NUM_THREAD_X,GPU_REGS);
  DUMP_REG(F,mmCOMPUTE_NUM_THREAD_Y,GPU_REGS);
  DUMP_REG(F,mmCOMPUTE_NUM_THREAD_Z,GPU_REGS);

  DUMP_USER_DATA(F,base,mmCOMPUTE_USER_DATA_0,@GPU_REGS.SH_REG^.COMPUTE_USER_DATA);

  DUMP_REG(F,mmCOMPUTE_STATIC_THREAD_MGMT_SE0,GPU_REGS);
  DUMP_REG(F,mmCOMPUTE_STATIC_THREAD_MGMT_SE1,GPU_REGS);
  DUMP_REG(F,mmCOMPUTE_RESOURCE_LIMITS       ,GPU_REGS);

  FileClose(F);

 end;
end;

function DumpPS(var GPU_REGS:TGPU_REGS):RawByteString;
var
 i:Integer;
 size,hash:DWORD;
 base:Pointer;
 F:THandle;
 fname:RawByteString;
begin
 Result:='';
 base:=GPU_REGS.get_code_addr(vShaderStagePs);
 if (base<>nil) then
 begin
  size:=_calc_shader_size(base);

  hash:=MurmurHash64A(base,size,0);
  fname:='shader_dump\'+get_dev_progname+'_ps_'+HexStr(hash,8)+'.dump';
  Result:=fname;

  if FileExists(fname) then Exit;

  CreateDir('shader_dump');
  F:=FileCreate(fname);
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_LO_PS,base,size);

  DUMP_REG(F,mmSPI_SHADER_PGM_RSRC1_PS,GPU_REGS);
  DUMP_REG(F,mmSPI_SHADER_PGM_RSRC2_PS,GPU_REGS);
  DUMP_REG(F,mmSPI_SHADER_PGM_RSRC3_PS,GPU_REGS);

  DUMP_REG(F,mmSPI_SHADER_Z_FORMAT    ,GPU_REGS);
  DUMP_REG(F,mmSPI_SHADER_COL_FORMAT  ,GPU_REGS);

  DUMP_REG(F,mmSPI_PS_INPUT_ENA       ,GPU_REGS);
  DUMP_REG(F,mmSPI_PS_INPUT_ADDR      ,GPU_REGS);
  DUMP_REG(F,mmSPI_PS_IN_CONTROL      ,GPU_REGS);

  DUMP_REG(F,mmSPI_BARYC_CNTL         ,GPU_REGS);

  DUMP_REG(F,mmDB_SHADER_CONTROL      ,GPU_REGS);
  DUMP_REG(F,mmCB_SHADER_MASK         ,GPU_REGS);

  DUMP_USER_DATA(F,base,mmSPI_SHADER_USER_DATA_PS_0,@GPU_REGS.SH_REG^.SPI_SHADER_USER_DATA_PS);

  For i:=0 to 31 do
  begin
   DUMP_REG(F,mmSPI_PS_INPUT_CNTL_0+i,GPU_REGS);
  end;

  FileClose(F);

 end;
end;

function DumpVS(var GPU_REGS:TGPU_REGS):RawByteString;
var
 size,hash:DWORD;
 base:Pointer;
 F:THandle;
 fname:RawByteString;
begin
 Result:='';
 base:=GPU_REGS.get_code_addr(vShaderStageVs);
 if (base<>nil) then
 begin
  size:=_calc_shader_size(base);

  hash:=MurmurHash64A(base,size,0);
  fname:='shader_dump\'+get_dev_progname+'_vs_'+HexStr(hash,8)+'.dump';
  Result:=fname;

  if FileExists(fname) then Exit;

  CreateDir('shader_dump');
  F:=FileCreate(fname);
  DUMP_BLOCK(F,mmSPI_SHADER_PGM_LO_VS,base,size);

  DUMP_REG(F,mmSPI_SHADER_PGM_RSRC1_VS,GPU_REGS);
  DUMP_REG(F,mmSPI_SHADER_PGM_RSRC2_VS,GPU_REGS);
  DUMP_REG(F,mmSPI_SHADER_PGM_RSRC3_VS,GPU_REGS);

  DUMP_REG(F,mmSPI_VS_OUT_CONFIG    ,GPU_REGS);
  DUMP_REG(F,mmSPI_SHADER_POS_FORMAT,GPU_REGS);
  DUMP_REG(F,mmPA_CL_VS_OUT_CNTL    ,GPU_REGS);

  DUMP_USER_DATA(F,base,mmSPI_SHADER_USER_DATA_VS_0,@GPU_REGS.SH_REG^.SPI_SHADER_USER_DATA_VS);

  DUMP_REG(F,mmVGT_DMA_NUM_INSTANCES,GPU_REGS);

  //DUMP_REG(F,mmVGT_NUM_INSTANCES,@GPU_REGS.VGT_NUM_INSTANCES,SizeOf(DWORD));


  FileClose(F);

 end;
end;

end.

