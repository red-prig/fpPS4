unit ps4_libSceLibcInternal;

{$mode ObjFPC}{$H+}

interface

uses
  ps4libdoc,
  ps4_map_mm,
  ps4_pthread,
  ps4_time,
  ps4_program,
  Classes,
  SysUtils;

implementation

uses
 ps4_mspace,
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

function ps4_memmove(dst,src:Pointer;len:size_t):Pointer; SysV_ABI_CDecl;
begin
 Move(src^,dst^,len);
 Result:=dst;
end;

function ps4___cxa_atexit(func:atexit_func;arg:Pointer;dso_handle:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln('__cxa_atexit:',HexStr(func));
 Result:=0;
end;

procedure ps4__ZNSt8ios_base4InitC1Ev(this:Pointer); //void __thiscall std::ios_base::Init::Init(Init *this)
begin
 //
end;

procedure ps4__ZNSt6_WinitC1Ev(this:Pointer); //void __thiscall std::_Winit::_Winit(_Winit *this)
begin
 //
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

  $B6CBC49A77A7CF8F, //__cxa_atexit
  $1F67BCB7949C4067, //__cxa_finalize
  $CEBD3DE04437F56C, //__cxa_pure_virtual
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
   if (Copy(ps4libdoc.GetFunctName(Nid),1,2)<>'_Z') then
   begin
    lib:=ps4_app.GetLib('libc');
    if (lib<>nil) then
    begin
     Writeln(StdErr,'Redirected:',HexStr(Nid,16),':',ps4libdoc.GetFunctName(Nid));
     Result:=lib^.get_proc(Nid);
    end;
   end else
   begin
    Writeln(StdErr,'Operator:',HexStr(Nid,16),':',ps4libdoc.GetFunctName(Nid));
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
 lib^.set_proc($F8FE854461F82DF0,@ps4_memmove);

 //lib^.set_proc($B6CBC49A77A7CF8F,@ps4___cxa_atexit);

 lib^.set_proc($B2A5B2B678587448,@ps4__ZNSt8ios_base4InitC1Ev);
 lib^.set_proc($FC197DFD26769E87,@ps4__ZNSt6_WinitC1Ev);

 //mspace

 lib^.set_proc($DB4714934A97F73A,@ps4_malloc_init_lv2);
 lib^.set_proc($A304FACCB271AD3B,@ps4_malloc_initialize);
 lib^.set_proc($CFC18F890C1A0046,@ps4_malloc_init);
 lib^.set_proc($467AA5BC49AF91DC,@ps4_malloc_finalize_lv2);
 lib^.set_proc($27A16814DC9DA452,@ps4_malloc_finalize);
 lib^.set_proc($DB5285844403277B,@ps4_malloc_fini);
 lib^.set_proc($68B6324B82B1EAB4,@ps4_malloc_prefork);
 lib^.set_proc($57DE292EBB9DB8B8,@ps4_malloc_postfork);
 lib^.set_proc($4A8A6D85BF73B592,@ps4_malloc_thread_cleanup);

 lib^.set_proc($FE19F5B5C547AB94,@ps4_sceLibcMspaceCreate);
 lib^.set_proc($5BA4A25528820ED2,@ps4_sceLibcMspaceDestroy);
 lib^.set_proc($3898E6FD03881E52,@ps4_sceLibcMspaceMalloc);
 lib^.set_proc($5656BF67E797971A,@ps4_sceLibcMspaceFree);
 lib^.set_proc($2D8A371A1225077F,@ps4_sceLibcMspaceCalloc);
 lib^.set_proc($82282854766F54F1,@ps4_sceLibcMspaceRealloc);
 lib^.set_proc($885D62407CF10495,@ps4_sceLibcMspaceMemalign);
 lib^.set_proc($A7A96B456F3F30B6,@ps4_sceLibcMspaceReallocalign);
 lib^.set_proc($A961129725CC2371,@ps4_sceLibcMspacePosixMemalign);
 lib^.set_proc($7C4A16E8126C3EDE,@ps4_sceLibcMspaceMallocUsableSize);
 lib^.set_proc($99F1DD25322F86EA,@ps4_sceLibcMspaceMallocStats);
 lib^.set_proc($934E232D7BB7F887,@ps4_sceLibcMspaceMallocStatsFast);
 lib^.set_proc($A7351AECA128C9DC,@ps4_sceLibcMspaceIsHeapEmpty);

 lib^.set_proc($8105FEE060D08E93,@ps4_malloc);
 lib^.set_proc($B4886CAA3D2AB051,@ps4_free);
 lib^.set_proc($D97E5A8058CAC4C7,@ps4_calloc);
 lib^.set_proc($63B689D6EC9D3CCA,@ps4_realloc);
 lib^.set_proc($60C64EF428596F41,@ps4_reallocf);
 lib^.set_proc($5237F72B332F4662,@ps4_memalign);
 lib^.set_proc($386C9B56E3C08406,@ps4_reallocalign);
 lib^.set_proc($7154A4F72F1445B7,@ps4_posix_memalign);
 lib^.set_proc($3437127DC619442F,@ps4_malloc_usable_size);
 lib^.set_proc($082FC12CC06EF7F2,@ps4_malloc_stats);
 lib^.set_proc($2AE3AE0F9F21AA7E,@ps4_malloc_stats_fast);
 lib^.set_proc($3667B37352E1ED34,@ps4_malloc_get_malloc_state);
 lib^.set_proc($4A51B514DFF2D0DD,@ps4_malloc_get_footer_value);

 lib^.set_proc($F826F019132795F5,@ps4_sceLibcMspaceSetMallocCallback);
 lib^.set_proc($2D7AADE3B1AF6910,@ps4_sceLibcInternalSetMallocCallback);
 lib^.set_proc($E223B3725A6FD4CD,@ps4_sceLibcHeapSetAddressRangeCallback);
 lib^.set_proc($ADA4608AE41FBD69,@ps4_sceLibcMspaceGetAddressRanges);
 lib^.set_proc($419F5881393ECAB1,@ps4_QZ9YgTk_yrE);
 lib^.set_proc($4A3DDF286ECCC0C9,@ps4_sceLibcHeapGetAddressRanges);
 lib^.set_proc($C14A89D29B148C3A,@ps4_wUqJ0psUjDo);
 lib^.set_proc($33AAA26349E1939E,@ps4_sceLibcHeapSetTraceMarker);
 lib^.set_proc($46584954060B4AA5,@ps4_sceLibcHeapUnsetTraceMarker);
 lib^.set_proc($D676785DF9F2A77F,@ps4_sceLibcGetMallocParam);

 lib^.set_proc($62B2FFD72EAF7F2A,@ps4_sceLibcInternalMemoryGetWakeAddr);
 lib^.set_proc($87C8EAF5E7B88797,@ps4_sceLibcInternalMemoryMutexEnable);
 lib^.set_proc($186EB8E3525D6240,@ps4_GG6441JdYkA);
 lib^.set_proc($6C6B8377791654A4,@ps4_bGuDd3kWVKQ);
 lib^.set_proc($7FD2D5C8DF0ACBC8,@ps4_f9LVyN8Ky8g);

 lib^.set_proc($46A784ACEDDC1475,@ps4__ZSt15set_new_handlerPFvvE);
 lib^.set_proc($0A46340156BF7EB8,@ps4__ZSt15get_new_handlerv);

 lib^.set_proc($85D9B461F31AED34,@ps4__Znam);
 lib^.set_proc($7C99E9B955416CA9,@ps4__Znwm);
 lib^.set_proc($AF25310FFEB46CA3,@ps4__ZnwmRKSt9nothrow_t);
 lib^.set_proc($261E6A51CC224849,@ps4__ZnamRKSt9nothrow_t);

 lib^.set_proc($30B5A5F7448558D1,@ps4__ZdaPv);
 lib^.set_proc($14EB79E5935A5499,@ps4__ZdaPvm);
 lib^.set_proc($EE50A2848D7C37D2,@ps4__ZdaPvmRKSt9nothrow_t);
 lib^.set_proc($9BF7D2A3711BC4D0,@ps4__ZdaPvRKSt9nothrow_t);
 lib^.set_proc($CFE3FEC429D62C19,@ps4__ZdlPv);
 lib^.set_proc($9580F3055139999B,@ps4__ZdlPvm);
 lib^.set_proc($ED53C8605A705360,@ps4__ZdlPvmRKSt9nothrow_t);
 lib^.set_proc($31CB069EA57AC911,@ps4__ZdlPvRKSt9nothrow_t);
 lib^.set_proc($4AE73C5B440FC63C,@ps4__ZdaPvS_);
 lib^.set_proc($D6FA3AAAAA9AF45E,@ps4__ZdlPvS_);

 lib:=Result._add_lib('libSceLibcInternalExt');

 lib^.set_proc($356B53375D1C2731,@ps4_sceLibcHeapGetTraceInfo);
end;

initialization
 ps4_app.RegistredPreLoad('libSceLibcInternal.prx',@Load_libSceLibcInternal);

end.

