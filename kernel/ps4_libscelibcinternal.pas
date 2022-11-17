unit ps4_libSceLibcInternal;

{$mode ObjFPC}{$H+}

interface

uses
  Windows,
  RWLock,
  ps4_map_mm,
  ps4_pthread,
  ps4_time,
  ps4_program,
  Classes,
  SysUtils;

implementation

uses
 sys_kernel,
 sys_signal;

function ps4_memset(ptr:Pointer;value:Integer;num:size_t):Pointer; SysV_ABI_CDecl;
begin
 FillByte(ptr^,num,Byte(value));
 Result:=ptr;
end;

function ps4_memcmp(buf1,buf2:Pointer;count:size_t):Integer; SysV_ABI_CDecl;
begin
 Result:=CompareByte(buf1^,buf2^,count);
end;

function ps4_memcpy_s(dst:Pointer;dstSize:size_t;src:Pointer;count:size_t):Integer; SysV_ABI_CDecl;
begin
 if (count=0) then Exit(0);

 if (dst=nil) or (src=nil) then
 begin
  if (dst<>nil) then FillChar(dst^,dstSize,0);
  _set_errno(EINVAL);
  Exit(EINVAL);
 end;

 if (dstSize<count) then
 begin
  FillChar(dst^,dstSize,0);
  _set_errno(ERANGE);
  Exit(ERANGE);
 end;

 Move(src^,dst^,count);
 Result:=0;
end;

function ps4_strcpy_s(dst:PChar;destSize:size_t;src:PChar):Integer; SysV_ABI_CDecl;
var
 count:size_t;
begin
 if (dst=nil) or (src=nil) then
 begin
  if (dst<>nil) then dst[0]:=#0;
  _set_errno(EINVAL);
  Exit(EINVAL);
 end;

 count:=System.strlen(src)+1;
 if (count>destSize) then
 begin
  dst[0]:=#0;
  _set_errno(ERANGE);
  Exit(ERANGE);
 end;

 Move(src^,dst^,count);
 Result:=0;
end;

function ps4_memcpy(dst,src:Pointer;len:size_t):Pointer; SysV_ABI_CDecl;
begin
 Move(src^,dst^,len);
 Result:=dst;
end;

const
 SCE_LIBC_MSPACE_THREAD_UNSAFE  =($00000001);
 SCE_LIBC_MSPACE_DEBUG_SHORTAGE =($00000004);

type
 SceLibcMspace=^TSceLibcMspace;
 TSceLibcMspace=packed record
  base:Pointer;
  capacity:size_t;
  count:size_t;
  lock:TRWLock;
 end;

function ps4_sceLibcMspaceCreate(name:PChar;base:Pointer;capacity:size_t;flag:Integer):SceLibcMspace; SysV_ABI_CDecl;
begin
 Result:=nil;

 if ((QWORD(base) and $F)<>0) or ((capacity and $F)<>0) then Exit;
 if (capacity<=1440) then Exit;

 Writeln('sceLibcMspaceCreate:',name,':',HexStr(base),'..',HexStr(base+capacity));
 Result:=SwAllocMem(SizeOf(TSceLibcMspace));
 Result^.base:=base;
 Result^.capacity:=capacity;
 Result^.count:=1440;

 _sig_lock;
 rwlock_init(Result^.lock);
 _sig_unlock;
end;

function ps4_sceLibcMspaceMalloc(msp:SceLibcMspace;size:size_t):Pointer; SysV_ABI_CDecl;
begin
 Result:=nil;
 if (msp=nil) then Exit;
 _sig_lock;
 rwlock_wrlock(msp^.lock);
 if ((msp^.count+size)<=msp^.capacity) then
 begin
  Result:=msp^.base+msp^.count;
  msp^.count:=msp^.count+size;
 end;
 rwlock_unlock(msp^.lock);
 _sig_unlock;
end;

function ps4_expf(x:Single):Single; SysV_ABI_CDecl;
begin
 Result:=System.Exp(x);
end;

type
 PGetTraceInfo=^TGetTraceInfo;
 TGetTraceInfo=packed record
  Size:QWORD;                    //32
  flag:DWORD;                    //1
  get_segment_info:DWORD;        //0
  mspace_atomic_id_mask:PQWORD;
  mstate_table:PQWORD;
 end;

var
 g_mspace_atomic_id_mask:QWORD=0;
 g_mstate_table:array[0..63] of QWORD;

//mysterious function
procedure ps4_sceLibcHeapGetTraceInfo(P:PGetTraceInfo); SysV_ABI_CDecl;
begin
 P^.get_segment_info     :=0;
 P^.mspace_atomic_id_mask:=@g_mspace_atomic_id_mask;
 P^.mstate_table         :=@g_mstate_table;
end;

function ps4___cxa_atexit(func:atexit_func;arg:Pointer;dso_handle:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln('__cxa_atexit:',HexStr(func));
 Result:=0;
end;

Const
 Need_sceLibcInternal:QWORD=1;

 _Stdin :QWORD=0;
 _Stdout:QWORD=1;
 _Stderr:QWORD=2;

function _get_proc_libSceLibcInternal(src:PLIBRARY;nid:QWORD):Pointer;
var
 lib:PLIBRARY;
begin
 Result:=nil;
 lib:=ps4_app.GetLib('libc');
 if (lib<>nil) then
 begin
  Result:=lib^.get_proc(Nid);
 end;
 if (Result=nil) then
 begin
  Result:=src^._get_proc(nid);
 end;
end;

{
function _get_proc_libSceLibcInternal(src:PLIBRARY;nid:QWORD):Pointer;
var
 lib:PLIBRARY;
begin
 Result:=src^._get_proc(nid);
 if (Result=nil) then
 begin
  Case nid of
   $78B743C3A974FDB5: //snprintf
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;
   $F33B2ED385CDB19E: //expf
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
    if (Result=nil) then
    begin
     Result:=@ps4_expf;
    end;
   end;
  $DC63E98D0740313C: //__cxa_guard_acquire
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;
  $F6B01E00D4F6B721: //__cxa_guard_release
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;
  end;
  if (Result<>nil) then
  begin
   src^.set_proc(nid,Result);
  end;
 end;
end;
}

function Load_libSceLibcInternal(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceLibcInternal');

 lib^.Fget_proc_cb:=@_get_proc_libSceLibcInternal;

 lib^.set_proc($653E0E0C3D93B3DA,@Need_sceLibcInternal);

 lib^.set_proc($D530E8FC89AA9097,@_Stdin );
 lib^.set_proc($DAC5B3858A851F81,@_Stdout);
 lib^.set_proc($1FC029ACA799B4D8,@_Stderr);

 lib^.set_proc($F334C5BC120020DF,@ps4_memset);
 lib^.set_proc($0DF8AF3C0AE1B9C8,@ps4_memcmp);
 lib^.set_proc($3452ECF9D44918D8,@ps4_memcpy_s);
 lib^.set_proc($E576B600234409DA,@ps4_strcpy_s);
 lib^.set_proc($437541C425E1507B,@ps4_memcpy);
 lib^.set_proc($FE19F5B5C547AB94,@ps4_sceLibcMspaceCreate);
 lib^.set_proc($3898E6FD03881E52,@ps4_sceLibcMspaceMalloc);

 lib^.set_proc($B6CBC49A77A7CF8F,@ps4___cxa_atexit);

 lib:=Result._add_lib('libSceLibcInternalExt');

 lib^.set_proc($356B53375D1C2731,@ps4_sceLibcHeapGetTraceInfo);
end;

initialization
 ps4_app.RegistredPreLoad('libSceLibcInternal.prx',@Load_libSceLibcInternal);

end.

