unit ps4_libSceLibcInternal;

{$mode ObjFPC}{$H+}

interface

uses
  ps4libdoc,
  sys_crt,
  ps4_map_mm,
  ps4_pthread,
  ps4_time,
  ps4_program,
  Classes,
  SysUtils;

implementation

uses
 ps4_mspace_internal,
 ps4_atexit_internal,
 ps4_guard_internal,
 ps4_mtx_internal,
 ps4_libkernel,
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

function ps4_strlen(p:PChar):sizeint; SysV_ABI_CDecl;
begin
 Result:=StrLen(p);
end;

function ps4_strncpy(dst,src:PChar;len:sizeint):PChar; SysV_ABI_CDecl;
begin
 Result:=strlcopy(dst,src,len);
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

function ps4_strcmp(a,b:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=StrComp(a,b);
end;

function ps4_strncmp(a,b:PChar;n:ptrint):Integer; SysV_ABI_CDecl;
begin
 Result:=StrLComp(a,b,n);
end;

function ps4_strstr(str,sub:PChar):PChar; SysV_ABI_CDecl;
begin
 Result:=StrPos(str,sub);
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

procedure ps4_bzero(s:Pointer;n:size_t); SysV_ABI_CDecl;
begin
 FillChar(s^,n,0);
end;

function ps4_getenv(name:PChar):PChar; SysV_ABI_CDecl;
label
 _err;
var
 n:PChar;
 _environ:PPchar;
 nlen:ptrint;
 r:Integer;
begin
 if (name=nil) then goto _err;

 nlen:=0;
 n:=name;
 While (n^<>#0) do
 begin
  if (n^='=') then goto _err;
  Inc(n);
  Inc(nlen);
 end;

 if (nlen=0) then goto _err;

 if (environ=nil) then Exit(nil);

 _environ:=environ;
 n:=_environ^;
 if (n=nil) then Exit(nil);

 repeat
  r:=StrLComp(n,name,nlen);

  if (r=0) and (n[nlen]='=') then Break;

  Inc(_environ);
  n:=_environ^;

  if (n=nil) then Exit(nil);

 until false;

 Exit(@n[nlen+1]);

 _err:
  _set_errno(EINVAL);
  Result:=nil;
end;

procedure ps4__init_env; SysV_ABI_CDecl;
begin
 //
end;

procedure ps4__init_tls; SysV_ABI_CDecl;
begin
 //
end;

////void __thiscall std::ios_base::Init::Init(Init *this)
procedure ps4__ZNSt8ios_base4InitC1Ev(this:Pointer); SysV_ABI_CDecl;
begin
 //
end;

////void __thiscall std::_Winit::_Winit(_Winit *this)
procedure ps4__ZNSt6_WinitC1Ev(this:Pointer); SysV_ABI_CDecl;
begin
 //
end;

//void std::_Throw_Cpp_error(int err)
procedure ps4__ZSt16_Throw_Cpp_errori(err:Integer); SysV_ABI_CDecl;
begin
 Assert(false,'Cpp error:'+IntToStr(err));
 asm
  ud2
 end;
end;

//void std::_Throw_C_error(int err)
procedure ps4__ZSt14_Throw_C_errori(err:Integer); SysV_ABI_CDecl;
begin
 if (err - 1 < 2) then
 begin
  ps4__ZSt16_Throw_Cpp_errori(6);
  asm
   ud2
  end;
 end;
 if (err = 3) then
 begin
   ps4__ZSt16_Throw_Cpp_errori(0);
   asm
    ud2
   end;
 end;
 if (err <> 4) then Exit;
 ps4__ZSt16_Throw_Cpp_errori(1);
 asm
  ud2
 end;
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

  $CEBD3DE04437F56C: //__cxa_pure_virtual
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
     Writeln(StdWrn,'Redirected:',HexStr(Nid,16),':',ps4libdoc.GetFunctName(Nid));
     Result:=lib^.get_proc(Nid);
    end;
   end else
   begin
    Writeln(StdWrn,'Operator:',HexStr(Nid,16),':',ps4libdoc.GetFunctName(Nid));
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
 lib^.set_proc($8F856258D1C4830C,@ps4_strlen);
 lib^.set_proc($EAC256896491BAA9,@ps4_strncpy);
 lib^.set_proc($E576B600234409DA,@ps4_strcpy_s);
 lib^.set_proc($3AF6F675224E02E1,@ps4_strcmp);
 lib^.set_proc($69EB328EB1D55B2E,@ps4_strncmp);
 lib^.set_proc($BE28B014C68D6A60,@ps4_strstr);
 lib^.set_proc($437541C425E1507B,@ps4_memcpy);
 lib^.set_proc($F8FE854461F82DF0,@ps4_memmove);
 lib^.set_proc($F68897D64C9E79D0,@ps4_bzero);

 lib^.set_proc($B266D0BA47F16093,@ps4_getenv);

 lib^.set_proc($6F3404C72D7CF592,@ps4__init_env);
 lib^.set_proc($E8D08EAABDDC0FBE,@ps4__init_tls);

 lib^.set_proc($B2A5B2B678587448,@ps4__ZNSt8ios_base4InitC1Ev);
 lib^.set_proc($FC197DFD26769E87,@ps4__ZNSt6_WinitC1Ev);

 lib^.set_proc($5B48FABC2C61F4F7,@ps4__ZSt16_Throw_Cpp_errori);
 lib^.set_proc($6D1BA3221796941D,@ps4__ZSt14_Throw_C_errori);

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

 //mspace

 //atexit
 lib^.set_proc($F06D8B07E037AF38,@ps4_atexit);
 lib^.set_proc($B6CBC49A77A7CF8F,@ps4___cxa_atexit);
 lib^.set_proc($1F67BCB7949C4067,@ps4___cxa_finalize);
 //atexit

 //guard
 lib^.set_proc($DC63E98D0740313C,@ps4___cxa_guard_acquire);
 lib^.set_proc($D9E99A6A5B96CD4C,@ps4___cxa_guard_abort);
 lib^.set_proc($F6B01E00D4F6B721,@ps4___cxa_guard_release);
 //guard

 //mtx
 lib^.set_proc($CFB493785E9A6EE5,@ps4__Mtxinit);
 lib^.set_proc($2DA3DA03A998037F,@ps4__Mtxdst);
 lib^.set_proc($A44E0EB7709F7D6D,@ps4__Mtxlock);
 lib^.set_proc($70CC204929A9139A,@ps4__Mtxunlock);
 //
 lib^.set_proc($B608A81A92AD99B1,@ps4__Mtx_init_with_name);
 lib^.set_proc($61A1DCDC64BBCBB8,@ps4__Mtx_init);
 lib^.set_proc($E4B7F9D63BE88534,@ps4__Mtx_destroy);
 lib^.set_proc($55843016CE020B86,@ps4__Mtx_current_owns);
 lib^.set_proc($892E1A59B5289E5D,@ps4__Mtx_lock);
 lib^.set_proc($93AA4634CC09074F,@ps4__Mtx_trylock);
 lib^.set_proc($84FCD849DE4D6AC7,@ps4__Mtx_timedlock);
 lib^.set_proc($813B974303FDAEBB,@ps4__Mtx_unlock);
 //mtx

 lib:=Result._add_lib('libSceLibcInternalExt');

 lib^.set_proc($356B53375D1C2731,@ps4_sceLibcHeapGetTraceInfo);
end;

initialization
 ps4_app.RegistredPreLoad('libSceLibcInternal.prx',@Load_libSceLibcInternal);

end.

