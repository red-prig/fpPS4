unit ps4_libSceNpCommon;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 np_error;

const
 SCE_NP_ONLINEID_MIN_LENGTH=3;
 SCE_NP_ONLINEID_MAX_LENGTH=16;

type
 pSceNpAccountId=^SceNpAccountId;
 SceNpAccountId=QWORD;

 pSceNpOnlineId=^SceNpOnlineId;
 SceNpOnlineId=packed record
  data:array[0..SCE_NP_ONLINEID_MAX_LENGTH-1] of AnsiChar;
  term:AnsiChar;
  dummy:array[0..2] of AnsiChar;
 end;

 PSceNpId=^SceNpId;
 SceNpId=packed record
  handle:SceNpOnlineId;
  opt:array[0..7] of Byte;
  reserved:array[0..7] of Byte;
 end;

implementation

uses
 ps4_map_mm;

function ps4_sceNpCmpNpId(npid1,npid2:PSceNpId):Integer; SysV_ABI_CDecl;
begin
 if (npid1=nil) or (npid2=nil) then Exit(SCE_NP_ERROR_INVALID_ARGUMENT);

 if (CompareChar0(npid1^.handle,npid2^.handle,SCE_NP_ONLINEID_MAX_LENGTH)=0) and
    (QWORD(npid1^.opt)=QWORD(npid2^.opt)) then
 begin
  Result:=0;
 end else
 begin
  Result:=SCE_NP_UTIL_ERROR_NOT_MATCH;
 end;

end;

function ps4_sceNpCmpOnlineId(str1,str2:PChar):Integer; SysV_ABI_CDecl;
begin
 if (str1=nil) or (str2=nil) then
  Exit(SCE_NP_ERROR_INVALID_ARGUMENT);
 if CompareChar0(str1,str2,SCE_NP_ONLINEID_MAX_LENGTH)=0 then
  Result:=0
 else
  Result:=SCE_NP_UTIL_ERROR_NOT_MATCH;
end;

type
 pnp_mem=^np_mem;
 np_mem=packed record
  len:qword;
  unknow:qword;
  ptr:Pointer;
 end;

function ps4_sceNpAllocateKernelMemoryWithAlignment(
          len:qword;
          name:Pchar;
          ptr_out:PPointer;
          mem_out:pnp_mem):Integer; SysV_ABI_CDecl;
var
 pad_len:qword;
begin
 if (mem_out=nil) then
 begin
  Exit(-$7faa7ffb); //NP-32268-1
 end;

 mem_out^.unknow:=0;
 pad_len:=0;
 if (len and $3fff)<>0 then
 begin
  pad_len:=$4000-(len and $3fff);
 end;
 mem_out^.len:=pad_len+len;

 Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result>-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

function ps4_sceNpAllocateKernelMemoryNoAlignment(
          len:qword;
          name:Pchar;
          ptr_out:PPointer;
          mem_out:pnp_mem):Integer; SysV_ABI_CDecl;
begin
 if (mem_out=nil) then
 begin
  Exit(-$7faa7ffb); //NP-32268-1
 end;

 mem_out^.unknow:=0;
 mem_out^.len:=len;

 Result:=ps4_sceKernelMapNamedFlexibleMemory(@mem_out^.ptr,mem_out^.len,3,0,name);

 if (ptr_out<>nil) and (Result>-1) then
 begin
  ptr_out^:=mem_out^.ptr;
 end;
end;

function Load_libSceNpCommon(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceNpCommon');
 lib^.set_proc($8BC5265D34AAECDE,@ps4_sceNpCmpNpId);
 lib^.set_proc($763F8EE5A0F66B44,@ps4_sceNpCmpOnlineId);
 lib^.set_proc($80C958E9E7B0AFF7,@ps4_sceNpAllocateKernelMemoryWithAlignment);
 lib^.set_proc($3163CE92ACD8B2CD,@ps4_sceNpAllocateKernelMemoryNoAlignment);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNpCommon.prx' ,@Load_libSceNpCommon);

end.

