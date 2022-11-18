unit ps4_libSceLibcInternal;

{$mode ObjFPC}{$H+}

interface

uses
  Windows,
  RWLock,
  ps4libdoc,
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
 {
            <- base
  ......
  [base]:8  <-p_mspace_chunk
  [size]:8
  [next]:8  <-SceLibcMspace (1312)
  [aflag]:4
  ......
 }

 p_mspace_chunk=^t_mspace_chunk;
 t_mspace_chunk=packed record
  base:Pointer;
  size:QWORD;
  next:Pointer;
  aflag:Integer;
 end;

 SceLibcMspace=^TSceLibcMspace;
 TSceLibcMspace=packed record
  _next:Pointer;
  _aflag:Integer;

  //Next comes the structure is not identical to the original
  _align:Integer;

  flags:Integer;
  aflag:Integer;

  magic:QWORD;   //0x58585858
  lock:TRWLock;

  base:Pointer;
  size:QWORD;

  list:p_mspace_chunk;

  alloc_addr:Pointer;
  alloc_size:QWORD;

  name:array[0..31] of AnsiChar;
 end;

function ps4_sceLibcMspaceCreate(name:PChar;base:Pointer;capacity:size_t;flag:Integer):SceLibcMspace; SysV_ABI_CDecl;
var
 m_count:Integer;
 unk_flag:Integer;
 aflag:Integer;
 r:Integer;

 base_offst:Integer;
 base_align:QWORD;

 chunk:p_mspace_chunk;

begin
 Result:=nil;

 m_count:=1440;
 if (SDK_VERSION<>0) and (SDK_VERSION<=$34fffff) then
 begin
  m_count:=1408;
 end;

 unk_flag:=flag and 2;

 if ((unk_flag=0) or (base<>nil)) and
    (
     (ptruint(base)<$400000) or
     (capacity>$fbffc00000)  or
     ((ptruint(base)+capacity)>$fbffffffff)
    ) then Exit;

 if (flag>15) then Exit;

 if (SDK_VERSION<>0) and (SDK_VERSION<=$34fffff) then
 begin
  if (capacity<=m_count) or (((ptruint(base) or capacity) and 7)<>0) then Exit;
 end else
 begin
  if (capacity<=m_count) then Exit;
 end;

 Writeln('sceLibcMspaceCreate:',name,':',HexStr(base),'..',HexStr(base+capacity));

 aflag:=0;
 if (base=nil) then
 begin
  capacity:=Align(capacity,64*1024);
  r:=ps4_sceKernelMapNamedFlexibleMemory(@base,capacity,3,0,name);
  if (r<>0) then Exit;
  aflag:=1;
 end;

 base_offst:=Integer(ptruint(base)+16);
 base_align:=Qword((-base_offst) and 31);
 if ((base_offst and 31)=0) then
 begin
  base_align:=(base_offst and 31);
 end;

 chunk:=p_mspace_chunk(base+base_align);

 chunk^.base :=nil;  //root
 chunk^.size :=1312; //root
 chunk^.aflag:=0;    //root

 Result:=SceLibcMspace(@chunk^.next); //+16

 if (name=nil) then name:='SceLibcMspace'; //+hexstr(base) ???

 FillChar(Result^,1312,0);

 Result^.flags:=flag;
 Result^.aflag:=aflag;

 Result^.magic:=$58585858;

 Result^.base:=base;     //real data of root
 Result^.size:=capacity; //real data of root

 Result^.alloc_addr:=Pointer(Result)+1312;
 Result^.alloc_size:=capacity-(Pointer(Result)-base);

 MoveChar0(name^,Result^.name,32);

 _sig_lock;
  rwlock_init(Result^.lock);
 _sig_unlock;
end;

function ps4_sceLibcMspaceDestroy(msp:SceLibcMspace):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 Assert(False,'sceLibcMspaceDestroy');
end;

function ps4_sceLibcMspaceMalloc(msp:SceLibcMspace;size:size_t):Pointer; SysV_ABI_CDecl;
begin
 Result:=nil;
 if (msp=nil) then Exit;
 _sig_lock;
 rwlock_wrlock(msp^.lock);
  if (msp^.alloc_size>=size) then //linear (-_-)
  begin
   Result:=msp^.alloc_addr;
   msp^.alloc_addr:=msp^.alloc_addr+size;
   msp^.alloc_size:=msp^.alloc_size-size;
  end;
 rwlock_unlock(msp^.lock);
 _sig_unlock;
end;

function ps4_sceLibcMspaceCalloc(msp:SceLibcMspace;nelem,size:size_t):Pointer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceLibcMspaceMalloc(msp,nelem*size);
 if (Result<>nil) then
 begin
  FillChar(Result^,nelem*size,0);
 end;
end;

function ps4_sceLibcMspaceFree(msp:SceLibcMspace;ptr:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 Assert(False,'sceLibcMspaceFree');
end;

//void * sceLibcMspaceMemalign(SceLibcMspace *msp,size_t boundary,size_t size)
//int sceLibcMspacePosixMemalign(SceLibcMspace *msp,void **ptr,size_t boundary,size_t size)
//void * sceLibcMspaceRealloc(SceLibcMspace *msp,void *ptr,size_t size)
//void * sceLibcMspaceReallocalign(SceLibcMspace *msp,void *ptr,size_t boundary,size_t size)

//size_t sceLibcMspaceMallocUsableSize(void *ptr)

//int sceLibcMspaceSetMallocCallback(SceLibcMspace *msp,void *cbs)
//int sceLibcMspaceGetAddressRanges(SceLibcMspace *msp)
//int sceLibcMspaceIsHeapEmpty(SceLibcMspace *msp)

//int sceLibcMspaceMallocStats(SceLibcMspace *msp,SceLibcMallocManagedSize *mmsize)
//int sceLibcMspaceMallocStatsFast(SceLibcMspace *msp,SceLibcMallocManagedSize *mmsize)


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
 Result:=src^._get_proc(nid);
 if (Result=nil) then //redirect to libc
 begin
  Case nid of

   //Variadic
   $C3537144142A7E64, //printf_s
   $FAA8AD3046E44969, //vsprintf_s
   $43657E8AABE3802D, //vsnprintf
   $85CB90803E775313, //printf
   $18CA6FC4F156F76E, //vprintf
   $78B743C3A974FDB5: //snprintf
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;

  $DC63E98D0740313C, //__cxa_guard_acquire
  $F6B01E00D4F6B721: //__cxa_guard_release
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Result:=lib^.get_proc(Nid);
    end;
   end;

  end; //Case

  //TODO redirect
  if (Result=nil) then
  begin
   lib:=ps4_app.GetLib('libc');
   if (lib<>nil) then
   begin
    Writeln(StdErr,'Redirected:',HexStr(Nid,16),':',ps4libdoc.GetFunctName(Nid));
    Result:=lib^.get_proc(Nid);
   end;
  end;

  if (Result<>nil) then //save new
  begin
   src^.set_proc(nid,Result);
  end;
 end;
end;

function Load_libSceLibcInternal(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceLibcInternal');

 lib^.Fget_proc_cb:=@_get_proc_libSceLibcInternal;

 lib^.set_proc($653E0E0C3D93B3DA,@Need_sceLibcInternal);

 //lib^.set_proc($D530E8FC89AA9097,@_Stdin );
 //lib^.set_proc($DAC5B3858A851F81,@_Stdout);
 //lib^.set_proc($1FC029ACA799B4D8,@_Stderr);

 lib^.set_proc($F334C5BC120020DF,@ps4_memset);
 lib^.set_proc($0DF8AF3C0AE1B9C8,@ps4_memcmp);
 lib^.set_proc($3452ECF9D44918D8,@ps4_memcpy_s);
 lib^.set_proc($E576B600234409DA,@ps4_strcpy_s);
 lib^.set_proc($437541C425E1507B,@ps4_memcpy);

 //lib^.set_proc($FE19F5B5C547AB94,@ps4_sceLibcMspaceCreate);
 //lib^.set_proc($3898E6FD03881E52,@ps4_sceLibcMspaceMalloc);

 //5BA4A25528820ED2:sceLibcMspaceDestroy

 //lib^.set_proc($B6CBC49A77A7CF8F,@ps4___cxa_atexit);

 lib:=Result._add_lib('libSceLibcInternalExt');

 lib^.set_proc($356B53375D1C2731,@ps4_sceLibcHeapGetTraceInfo);
end;

initialization
 ps4_app.RegistredPreLoad('libSceLibcInternal.prx',@Load_libSceLibcInternal);

end.

