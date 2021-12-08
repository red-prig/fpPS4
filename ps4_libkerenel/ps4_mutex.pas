unit ps4_mutex;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  sysutils,
  ps4_types;

type
 Ppthread_mutex_attr=^pthread_mutex_attr_t;
 pthread_mutex_attr_t=bitpacked record
  _type:0..7;           //3
  _shared:0..1;         //1
  _protocol:0..3;       //2
  _align:0..67108863;   //26
  _prioceiling:Integer; //32
 end;

 Ppthread_mutex=^pthread_mutex;
 pthread_mutex=^pthread_mutex_t;
 pthread_mutex_t=packed record
  valid:DWORD;
  state:DWORD;
  _type:DWORD;
  rec_lock:DWORD;
  owner:DWORD;
  event:THandle;
  name:array[0..31] of AnsiChar;
 end;

 ScePthreadMutex=pthread_mutex;
 PScePthreadMutex=Ppthread_mutex;

const
 SCE_PTHREAD_MUTEX_ERRORCHECK = 1; // Default POSIX mutex
 SCE_PTHREAD_MUTEX_RECURSIVE  = 2; // Recursive mutex
 SCE_PTHREAD_MUTEX_NORMAL     = 3; // No error checking
 SCE_PTHREAD_MUTEX_ADAPTIVE   = 4; // Adaptive mutex, spins briefly before blocking on lock
 SCE_PTHREAD_MUTEX_DEFAULT    = SCE_PTHREAD_MUTEX_ERRORCHECK;

 PTHREAD_MUTEX_ERRORCHECK = SCE_PTHREAD_MUTEX_ERRORCHECK;
 PTHREAD_MUTEX_RECURSIVE  = SCE_PTHREAD_MUTEX_RECURSIVE;
 PTHREAD_MUTEX_NORMAL     = SCE_PTHREAD_MUTEX_NORMAL;
 PTHREAD_MUTEX_ADAPTIVE   = SCE_PTHREAD_MUTEX_ADAPTIVE;
 PTHREAD_MUTEX_DEFAULT    = SCE_PTHREAD_MUTEX_ERRORCHECK;{SCE_PTHREAD_MUTEX_RECURSIVE;}//PTHREAD_MUTEX_ERRORCHECK;

 PTHREAD_PROCESS_PRIVATE=0;
 PTHREAD_PROCESS_SHARED =1;

 PTHREAD_PRIO_NONE      =0;
 PTHREAD_PRIO_INHERIT   =1;
 PTHREAD_PRIO_PROTECT   =2;

 PTHREAD_MUTEX_INITIALIZER            =0;
 PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP=1;

function ps4_pthread_mutexattr_init(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_destroy(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_gettype(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_settype(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_getpshared(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_setpshared(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_getprotocol(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_setprotocol(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_getprioceiling(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_setprioceiling(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;

function ps4_pthread_mutex_lock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_timedlock(pMutex:Ppthread_mutex;ts:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_trylock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_unlock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_init(pMutex:Ppthread_mutex;pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_destroy(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;

function ps4_scePthreadMutexattrInit(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrDestroy(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrGettype(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrSettype(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrGetprotocol(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrSetprotocol(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrGetprioceiling(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrSetprioceiling(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;

function ps4_scePthreadMutexInit(pMutex:PScePthreadMutex;pAttr:Ppthread_mutex_attr;str:PChar):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexLock(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexTimedlock(pMutex:PScePthreadMutex;usec:DWORD):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexTrylock(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexUnlock(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexDestroy(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;

//pthread_mutexattr_setkind_np

implementation

Uses
 spinlock,
 ps4_sema,
 ps4_libkernel,
 ps4_time;

function ps4_pthread_mutexattr_init(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=Default(pthread_mutex_attr_t);
 pAttr^._type:=PTHREAD_MUTEX_DEFAULT;
 Result:=0;
end;

function ps4_pthread_mutexattr_destroy(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Result:=0;
end;

function ps4_pthread_mutexattr_gettype(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._type;
 Result:=0;
end;

function ps4_pthread_mutexattr_settype(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_MUTEX_ERRORCHECK:;
  PTHREAD_MUTEX_RECURSIVE :;
  PTHREAD_MUTEX_NORMAL    :;
  PTHREAD_MUTEX_ADAPTIVE  :;
  else
   Exit(EINVAL);
 end;
 pAttr^._type:=t;
 Result:=0;
end;

function ps4_pthread_mutexattr_getpshared(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._shared;
 Result:=0;
end;

function ps4_pthread_mutexattr_setpshared(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_PROCESS_PRIVATE:;
  PTHREAD_PROCESS_SHARED :;
  else
   Exit(EINVAL);
 end;
 pAttr^._shared:=t;
 Result:=0;
end;

function ps4_pthread_mutexattr_getprotocol(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._protocol;
 Result:=0;
end;

function ps4_pthread_mutexattr_setprotocol(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_PRIO_NONE   :;
  PTHREAD_PRIO_INHERIT:;
  PTHREAD_PRIO_PROTECT:;
  else
   Exit(EINVAL);
 end;
 pAttr^._protocol:=t;
 Result:=0;
end;

function ps4_pthread_mutexattr_getprioceiling(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._prioceiling;
 Result:=0;
end;

function ps4_pthread_mutexattr_setprioceiling(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^._prioceiling:=t;
 Result:=0;
end;

///////////////////////////

Const
 LIFE_MUTEX=$BAB1F00D;
 DEAD_MUTEX=$DEADBEEF;

 MS_Unlocked=0;
 MS_Locked  =1;
 MS_Waiting =2;

function STATIC_INITIALIZER(m:pthread_mutex):Boolean; inline;
begin
 Result:=False;
 Case PtrUInt(m) of
  PTHREAD_MUTEX_INITIALIZER,
  PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP:Result:=True;
 end;
end;

function CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function CAS(Var addr:SizeUInt;Comp,New:SizeUInt):Boolean; inline;
begin
 Result:=system.InterlockedCompareExchange(Pointer(addr),Pointer(New),Pointer(Comp))=Pointer(Comp);
end;

function CAS(Var addr:DWORD;Comp,New:DWORD):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

function XCHG(Var addr:DWORD;New:DWORD):DWORD; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

function mutex_impl_init(m:Ppthread_mutex;mi:pthread_mutex;_type:Integer):pthread_mutex;
var
 new_mi:pthread_mutex;
begin
 new_mi:=AllocMem(SizeOf(pthread_mutex_t));
 if (new_mi=nil) then Exit(new_mi);

 new_mi^.valid:=LIFE_MUTEX;
 new_mi^.state:=MS_Unlocked;
 new_mi^._type:=_type;
 new_mi^.owner:=DWORD(-1);

 if CAS(m^,mi,new_mi) then
 begin
  Result:=new_mi;
 end else
 begin
  FreeMem(new_mi);
  Result:=m^;
 end;
end;

function mutex_impl(m:Ppthread_mutex;var mi:pthread_mutex;default:Integer):Integer;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 Case PtrUInt(mi) of
  PTHREAD_MUTEX_INITIALIZER            :mi:=mutex_impl_init(m,mi,default);
  PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP:mi:=mutex_impl_init(m,mi,PTHREAD_MUTEX_ADAPTIVE);
 end;
 if (mi=nil) then Exit(ENOMEM);
 if (mi^.valid<>LIFE_MUTEX) then Exit(EINVAL);
 Result:=0;
end;

function pthread_mutex_lock_intern(m:Ppthread_mutex;timeout:DWORD;default:Integer):Integer;
var
 mi:pthread_mutex;
 old_state:DWORD;
 ev:THandle;
begin
 Result:=mutex_impl(m,mi,default);
 if (Result<>0) then Exit;

 old_state:=XCHG(mi^.state,MS_Locked);
 if (old_state<>MS_Unlocked) then
 begin
  if (mi^._type<>PTHREAD_MUTEX_NORMAL) then
  if (mi^.owner=GetCurrentThreadId) then
  begin
   CAS(mi^.state,MS_Locked,old_state);
   if (mi^._type=PTHREAD_MUTEX_RECURSIVE) then
   begin
    Inc(mi^.rec_lock);
    Exit(0);
   end else
   begin
    Exit(EDEADLK);
   end;
  end;

  if (mi^.event=0) then
  begin
   ev:=CreateEvent(nil,false,false,nil);
   if (ev=0) then
    Case GetLastError of
     ERROR_ACCESS_DENIED:Exit(EPERM);
     else
                         Exit(EPERM);
    end;

   if not CAS(mi^.event,0,ev) then
   begin
    CloseHandle(ev);
   end;
  end;

  While (XCHG(mi^.state,MS_Waiting)<>MS_Unlocked) do
  begin
   Result:=do_sema_b_wait_intern(mi^.event,timeout);
   if (Result<>0) then Exit;
  end;
 end;

 if (mi^._type<>PTHREAD_MUTEX_NORMAL) then
 begin
  mi^.owner:=GetCurrentThreadId;
 end;

 Result:=0;
end;

function ps4_pthread_mutex_lock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_lock_intern(pMutex,INFINITE,PTHREAD_MUTEX_DEFAULT);
end;

function ps4_pthread_mutex_timedlock(pMutex:Ppthread_mutex;ts:Ptimespec):Integer; SysV_ABI_CDecl;
var
 t:DWORD;
begin
 if (ts=nil) then
 begin
  t:=INFINITE;
 end else
 begin
  t:=dwMilliSecs(_pthread_rel_time_in_ms(ts^));
 end;
 Result:=pthread_mutex_lock_intern(pMutex,t,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_unlock(m:Ppthread_mutex;default:Integer):Integer;
var
 mi:pthread_mutex;
begin
 Result:=mutex_impl(m,mi,default);
 if (Result<>0) then Exit;

 if (mi^._type<>PTHREAD_MUTEX_NORMAL) then
 begin
  if (mi^.state=MS_Unlocked) then Exit(EPERM);
  if (mi^.owner<>GetCurrentThreadId) then Exit(EPERM);

  if (mi^.rec_lock>0) then
  begin
   Dec(mi^.rec_lock);
   Exit(0);
  end;
  mi^.owner:=DWORD(-1);
 end;

 if XCHG(mi^.state,MS_Unlocked)=MS_Waiting then
 begin
  if not SetEvent(mi^.event) then Exit(EPERM);
 end;

 Result:=0;
end;

function ps4_pthread_mutex_unlock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_unlock(pMutex,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_trylock(m:Ppthread_mutex;default:Integer):Integer;
var
 mi:pthread_mutex;
begin
 Result:=mutex_impl(m,mi,default);
 if (Result<>0) then Exit;

 if CAS(mi^.state,MS_Unlocked,MS_Locked) then
 begin
  if (mi^._type<>PTHREAD_MUTEX_NORMAL) then
  begin
   mi^.owner:=GetCurrentThreadId;
  end;
  Exit(0);
 end else
 begin
  if (mi^._type=PTHREAD_MUTEX_RECURSIVE) and (mi^.owner=GetCurrentThreadId) then
  begin
   Inc(mi^.rec_lock);
   Exit(0);
  end;
  Exit(EBUSY);
 end;
end;

function ps4_pthread_mutex_trylock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_trylock(pMutex,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_init(m:Ppthread_mutex;a:Ppthread_mutex_attr;str:PChar;default:Integer):Integer;
var
 mi:pthread_mutex;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 if (a<>nil) then
 begin
  mi:=mutex_impl_init(m,mi,a^._type);
 end else
 begin
  mi:=mutex_impl_init(m,mi,default);
 end;
 if (mi=nil) then Exit(ENOMEM);
 if (mi^.valid<>LIFE_MUTEX) then Exit(EINVAL);

 if (str<>nil) then MoveChar0(str^,mi^.name,32);

 Result:=0;
end;

function ps4_pthread_mutex_init(pMutex:Ppthread_mutex;pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_init(pMutex,pAttr,nil,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_destroy(m:Ppthread_mutex):Integer;
var
 mi:pthread_mutex;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 if not STATIC_INITIALIZER(mi) then
 begin
  mi:=XCHG(m^,nil);
  if STATIC_INITIALIZER(mi) then Exit(0);
  if not CAS(mi^.valid,LIFE_MUTEX,DEAD_MUTEX) then Exit(EINVAL);
  if (mi^.event<>0) then CloseHandle(mi^.event);
  FreeMem(mi);
 end;
 Result:=0;
end;

function ps4_pthread_mutex_destroy(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_destroy(pMutex);
end;

//---------------------------------------------------------
//sce

function ps4_scePthreadMutexattrInit(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^:=Default(pthread_mutex_attr_t);
 pAttr^._type:=SCE_PTHREAD_MUTEX_DEFAULT;
 Result:=0;
end;

function ps4_scePthreadMutexattrDestroy(pAttr:Ppthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Result:=0;
end;

function ps4_scePthreadMutexattrGettype(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 t^:=pAttr^._type;
 Result:=0;
end;

function ps4_scePthreadMutexattrSettype(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Case t of
  SCE_PTHREAD_MUTEX_ERRORCHECK:;
  SCE_PTHREAD_MUTEX_RECURSIVE :;
  SCE_PTHREAD_MUTEX_NORMAL    :;
  SCE_PTHREAD_MUTEX_ADAPTIVE  :;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;
 pAttr^._type:=t;
 Result:=0;
end;

function ps4_scePthreadMutexattrGetprotocol(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 t^:=pAttr^._protocol;
 Result:=0;
end;

function ps4_scePthreadMutexattrSetprotocol(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Case t of
  PTHREAD_PRIO_NONE   :;
  PTHREAD_PRIO_INHERIT:;
  PTHREAD_PRIO_PROTECT:;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;
 pAttr^._protocol:=t;
 Result:=0;
end;

function ps4_scePthreadMutexattrGetprioceiling(pAttr:Ppthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 t^:=pAttr^._prioceiling;
 Result:=0;
end;

function ps4_scePthreadMutexattrSetprioceiling(pAttr:Ppthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^._prioceiling:=t;
 Result:=0;
end;

//////////////

function ps4_scePthreadMutexLock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 //Writeln('scePthreadMutexLock:',HexStr(pMutex));
 Result:=px2sce(pthread_mutex_lock_intern(pMutex,INFINITE,SCE_PTHREAD_MUTEX_DEFAULT));
 //if (Result<>0) then Writeln('scePthreadMutexLock:',HexStr(pMutex),':',HexStr(Result,8));
end;

function ps4_scePthreadMutexTimedlock(pMutex:PScePthreadMutex;usec:DWORD):Integer; SysV_ABI_CDecl;
begin
 //Writeln('scePthreadMutexTimedlock:',HexStr(pMutex));
 Result:=px2sce(pthread_mutex_lock_intern(pMutex,_usec2msec(usec),SCE_PTHREAD_MUTEX_DEFAULT));
 //if (Result<>0) then Writeln('scePthreadMutexTimedlock:',HexStr(pMutex),':',HexStr(Result,8));
end;

function ps4_scePthreadMutexUnlock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 //Writeln('scePthreadMutexUnlock:',HexStr(pMutex));
 Result:=px2sce(pthread_mutex_unlock(pMutex,SCE_PTHREAD_MUTEX_DEFAULT));
 //if (Result<>0) then Writeln('scePthreadMutexUnlock:',HexStr(pMutex),':',pMutex^^.name,':',HexStr(Result,8));
end;

function ps4_scePthreadMutexTrylock(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 //Writeln('scePthreadMutexTrylock:',HexStr(pMutex));
 Result:=px2sce(pthread_mutex_trylock(pMutex,SCE_PTHREAD_MUTEX_DEFAULT));
 //if (Result<>0) then Writeln('scePthreadMutexTrylock:',HexStr(pMutex),':',HexStr(Result,8));
end;

function ps4_scePthreadMutexInit(pMutex:PScePthreadMutex;pAttr:Ppthread_mutex_attr;str:PChar):Integer; SysV_ABI_CDecl;
begin
 //if str='NP CVariable Mutex' then
 // writeln;
 Result:=px2sce(pthread_mutex_init(pMutex,pAttr,str,SCE_PTHREAD_MUTEX_DEFAULT));
end;

function ps4_scePthreadMutexDestroy(pMutex:Ppthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_mutex_destroy(pMutex));
end;

end.

