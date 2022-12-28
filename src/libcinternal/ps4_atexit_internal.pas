unit ps4_atexit_internal;

{$mode ObjFPC}{$H+}

interface

uses
 sys_types,
 ps4_libkernel,
 ps4_mutex;

const
 ATEXIT_FN_EMPTY=0;
 ATEXIT_FN_STD  =1;
 ATEXIT_FN_CXA  =2;

 ATEXIT_SIZE    =32;

type
 t_std_func=Procedure; SysV_ABI_CDecl;
 t_cxa_func=Procedure(arg:Pointer); SysV_ABI_CDecl;

 p_atexit_fn=^atexit_fn;
 atexit_fn=packed record
  fn_type:Integer;
  _align:Integer;
  fn_ptr:Pointer;
  fn_arg:Pointer;
  fn_dso:Pointer;
 end;

 p_atexit=^t_atexit;
 t_atexit=packed record
  next:p_atexit;
  ind:Integer;
  _align:Integer;
  fns:array[0..ATEXIT_SIZE-1] of atexit_fn;
 end;

function ps4_atexit(func:Pointer):Integer; SysV_ABI_CDecl;
function ps4___cxa_atexit(func,arg,dso:Pointer):Integer; SysV_ABI_CDecl;
function ps4___cxa_finalize(dso:Pointer):Integer; SysV_ABI_CDecl;

implementation

var
 atexit_mutex:pthread_mutex=nil;
 __atexit:p_atexit=nil;
 __atexit0:t_atexit;
 global_exit:Integer=0;

procedure _MUTEX_LOCK; inline;
begin
 ps4_pthread_mutex_lock(@atexit_mutex);
end;

procedure _MUTEX_UNLOCK; inline;
begin
 ps4_pthread_mutex_unlock(@atexit_mutex);
end;

procedure _MUTEX_DESTROY; inline;
begin
 ps4_pthread_mutex_destroy(@atexit_mutex);
end;

function atexit_register(fptr:p_atexit_fn):Integer; SysV_ABI_CDecl;
var
 p:p_atexit;
 old__atexit:p_atexit;
begin
 _MUTEX_LOCK;
  p:=__atexit;
  if (p=nil) then
  begin
   p:=@__atexit0;
   __atexit:=p;
  end else
  While (p^.ind>=ATEXIT_SIZE) do
  begin
   old__atexit:=__atexit;
   _MUTEX_UNLOCK;
   p:=AllocMem(SizeOf(t_atexit));
   if (p=nil) then Exit(-1);
   _MUTEX_LOCK;
   if (old__atexit<>__atexit) then
   begin
    _MUTEX_UNLOCK;
    FreeMem(p);
    _MUTEX_LOCK;
    p:=__atexit;
    continue
   end;
   p^.ind:=0;
   p^.next:=__atexit;
   __atexit:=p;
  end;
  p^.fns[p^.ind]:=fptr^;
  p^.ind:=p^.ind+1;
 _MUTEX_UNLOCK;
 Result:=0;
end;

function ps4_atexit(func:Pointer):Integer; SysV_ABI_CDecl;
var
 fn:atexit_fn;
begin
 fn.fn_type:=ATEXIT_FN_STD;
 fn.fn_ptr :=func;
 fn.fn_arg :=nil;
 fn.fn_dso :=nil;
 Result:=atexit_register(@fn);
end;

function ps4___cxa_atexit(func,arg,dso:Pointer):Integer; SysV_ABI_CDecl;
var
 fn:atexit_fn;
begin
 fn.fn_type:=ATEXIT_FN_CXA;
 fn.fn_ptr :=func;
 fn.fn_arg :=arg;
 fn.fn_dso :=dso;
 Result:=atexit_register(@fn);
end;

function ps4___cxa_finalize(dso:Pointer):Integer; SysV_ABI_CDecl;
var
 phdr_info:SceKernelModuleInfoEx;
 p:p_atexit;
 fn:atexit_fn;
 n,has_phdr:Integer;
begin

 if (dso<>nil) then
 begin
  has_phdr:=ps4_sceKernelGetModuleInfoFromAddr(dso,2,@phdr_info);
 end else
 begin
  has_phdr:=0;
  global_exit:=1;
 end;

 _MUTEX_LOCK;
  p:=__atexit;
  While (p<>nil) do
  begin

   For n:=p^.ind downto 0 do
   begin
    if (p^.fns[n].fn_type=ATEXIT_FN_EMPTY) then Continue;

    fn:=p^.fns[n];

    if (dso<>nil) and (dso<>fn.fn_dso) then
    begin
     if ((not Boolean(has_phdr)) or
          Boolean(global_exit) or
         (not Boolean(ps4___elf_phdr_match_addr(@phdr_info,fn.fn_ptr)))) then
     begin
      continue;
     end;

    end;

    p^.fns[n].fn_type:=ATEXIT_FN_EMPTY;
    _MUTEX_UNLOCK;

    Case fn.fn_type of
     ATEXIT_FN_CXA:t_cxa_func(fn.fn_ptr)(fn.fn_arg);
     ATEXIT_FN_STD:t_std_func(fn.fn_ptr)();
     else;
    end;

    _MUTEX_LOCK;
   end;

   p:=p^.next;
  end;
 _MUTEX_UNLOCK;

 if (dso=nil) then
 begin
  _MUTEX_DESTROY;
 end;

 if Boolean(has_phdr) and
    (not Boolean(global_exit)) then
 begin
  ps4___pthread_cxa_finalize(@phdr_info);
 end;
end;


end.

