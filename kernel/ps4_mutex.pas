unit ps4_mutex;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  sysutils,
  sys_types;

type
 p_pthread_mutex_attr=^pthread_mutex_attr_t;
 pthread_mutex_attr_t=^pthread_mutex_attr;
 pthread_mutex_attr=bitpacked record
  _type:0..7;           //3
  _shared:0..1;         //1
  _protocol:0..3;       //2
  _align:0..67108863;   //26
  _prioceiling:Integer; //32
 end;

 p_pthread_mutex=^pthread_mutex;
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
 PScePthreadMutex=p_pthread_mutex;

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

function ps4_pthread_mutexattr_init(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_destroy(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_gettype(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_settype(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_getpshared(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_setpshared(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_getprotocol(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_setprotocol(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_getprioceiling(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutexattr_setprioceiling(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;

function ps4_pthread_mutex_lock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_timedlock(pMutex:p_pthread_mutex;ts:Ptimespec):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_trylock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_unlock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_init(pMutex:p_pthread_mutex;pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_pthread_mutex_destroy(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;

function ps4_scePthreadMutexattrInit(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrDestroy(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrGettype(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrSettype(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrGetprotocol(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrSetprotocol(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrGetprioceiling(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexattrSetprioceiling(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;

function ps4_scePthreadMutexInit(pMutex:PScePthreadMutex;pAttr:p_pthread_mutex_attr;str:PChar):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexLock(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexTimedlock(pMutex:PScePthreadMutex;usec:DWORD):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexTrylock(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexUnlock(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;
function ps4_scePthreadMutexDestroy(pMutex:PScePthreadMutex):Integer; SysV_ABI_CDecl;

//pthread_mutexattr_setkind_np

implementation

Uses
 atomic,
 sys_kernel,
 sys_signal,
 sys_time,
 ps4_sema,
 ps4_time;

function ps4_pthread_mutexattr_init(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
var
 attr:pthread_mutex_attr_t;
begin
 if (pAttr=nil) then Exit(EINVAL);
 attr:=AllocMem(SizeOf(pthread_mutex_attr));
 if (attr=nil) then Exit(ENOMEM);
 attr^:=Default(pthread_mutex_attr);
 attr^._type:=PTHREAD_MUTEX_DEFAULT;
 pAttr^:=attr;
 Result:=0;
end;

function ps4_pthread_mutexattr_destroy(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
var
 attr:pthread_mutex_attr_t;
begin
 if (pAttr=nil) then Exit(EINVAL);
 attr:=pAttr^;
 if (attr=nil) then Exit(EINVAL);
 FreeMem(attr);
 Result:=0;
end;

function ps4_pthread_mutexattr_gettype(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 t^:=pAttr^^._type;
 Result:=0;
end;

function ps4_pthread_mutexattr_settype(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_MUTEX_ERRORCHECK:;
  PTHREAD_MUTEX_RECURSIVE :;
  PTHREAD_MUTEX_NORMAL    :;
  PTHREAD_MUTEX_ADAPTIVE  :;
  else
   Exit(EINVAL);
 end;
 pAttr^^._type:=t;
 Result:=0;
end;

function ps4_pthread_mutexattr_getpshared(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 t^:=pAttr^^._shared;
 Result:=0;
end;

function ps4_pthread_mutexattr_setpshared(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_PROCESS_PRIVATE:;
  PTHREAD_PROCESS_SHARED :;
  else
   Exit(EINVAL);
 end;
 pAttr^^._shared:=t;
 Result:=0;
end;

function ps4_pthread_mutexattr_getprotocol(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 t^:=pAttr^^._protocol;
 Result:=0;
end;

function ps4_pthread_mutexattr_setprotocol(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 Case t of
  PTHREAD_PRIO_NONE   :;
  PTHREAD_PRIO_INHERIT:;
  PTHREAD_PRIO_PROTECT:;
  else
   Exit(EINVAL);
 end;
 pAttr^^._protocol:=t;
 Result:=0;
end;

function ps4_pthread_mutexattr_getprioceiling(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 t^:=pAttr^^._prioceiling;
 Result:=0;
end;

function ps4_pthread_mutexattr_setprioceiling(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 if (pAttr^=nil) then Exit(EINVAL);
 pAttr^^._prioceiling:=t;
 Result:=0;
end;

///////////////////////////

Const
 LIFE_MUTEX=$BAB1F00D;
 DEAD_MUTEX=$DEADBEEF;

 MS_Unlocked=0;
 MS_Locked  =1;
 MS_Waiting =2;

const
 PTHREAD_MUTEX_FREE=2;

function STATIC_INITIALIZER(m:pthread_mutex):Boolean; inline;
begin
 Result:=False;
 Case PtrUInt(m) of
  PTHREAD_MUTEX_INITIALIZER,
  PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP:Result:=True;
 end;
end;

function mutex_impl_init(m:p_pthread_mutex;mi:pthread_mutex;_type:Integer):pthread_mutex;
var
 new_mi:pthread_mutex;
begin
 new_mi:=SwAllocMem(SizeOf(pthread_mutex_t));
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
  SwFreeMem(new_mi);
  Result:=m^;
 end;
end;

function mutex_impl(m:p_pthread_mutex;var mi:pthread_mutex;default:Integer):Integer;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 Case PtrUInt(mi) of
  PTHREAD_MUTEX_INITIALIZER            :mi:=mutex_impl_init(m,mi,default);
  PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP:mi:=mutex_impl_init(m,mi,PTHREAD_MUTEX_ADAPTIVE);
  PTHREAD_MUTEX_FREE                   :Exit(EINVAL);
 end;
 if (mi=nil) then Exit(ENOMEM);
 if not safe_test(mi^.valid,LIFE_MUTEX) then Exit(EINVAL);
 Result:=0;
end;

function pthread_mutex_lock_intern(m:p_pthread_mutex;pTimeout:PQWORD;default:Integer):Integer;
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
   Result:=do_sema_b_wait_intern(mi^.event,pTimeout);
   if (Result<>0) then Exit;
  end;
 end;

 if (mi^._type<>PTHREAD_MUTEX_NORMAL) then
 begin
  mi^.owner:=GetCurrentThreadId;
 end;

 Result:=0;
end;

function ps4_pthread_mutex_lock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=pthread_mutex_lock_intern(pMutex,nil,PTHREAD_MUTEX_DEFAULT);
 _sig_unlock;
end;

function ps4_pthread_mutex_timedlock(pMutex:p_pthread_mutex;ts:Ptimespec):Integer; SysV_ABI_CDecl;
var
 t:QWORD;
begin
 if (ts=nil) then
 begin
  _sig_lock;
  Result:=pthread_mutex_lock_intern(pMutex,nil,PTHREAD_MUTEX_DEFAULT);
  _sig_unlock;
 end else
 begin
  t:=_pthread_rel_time_in_ns(ts^);
  _sig_lock;
  Result:=pthread_mutex_lock_intern(pMutex,@t,PTHREAD_MUTEX_DEFAULT);
  _sig_unlock;
 end;
end;

function pthread_mutex_unlock(m:p_pthread_mutex;default:Integer):Integer;
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
  _sig_lock;
  if not SetEvent(mi^.event) then Result:=EPERM;
  _sig_unlock;
 end;

end;

function ps4_pthread_mutex_unlock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_unlock(pMutex,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_trylock(m:p_pthread_mutex;default:Integer):Integer;
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

function ps4_pthread_mutex_trylock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_trylock(pMutex,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_init(m:p_pthread_mutex;a:p_pthread_mutex_attr;str:PChar;default:Integer):Integer;
var
 mi:pthread_mutex;
 attr:pthread_mutex_attr_t;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;

 attr:=nil;
 if (a<>nil) then
 begin
  attr:=a^;
 end;

 if (attr<>nil) then
 begin
  mi:=mutex_impl_init(m,mi,attr^._type);
 end else
 begin
  mi:=mutex_impl_init(m,mi,default);
 end;
 if (mi=nil) then Exit(ENOMEM);
 if not safe_test(mi^.valid,LIFE_MUTEX) then Exit(EINVAL);

 if (str<>nil) then MoveChar0(str^,mi^.name,32);

 Result:=0;
end;

function ps4_pthread_mutex_init(pMutex:p_pthread_mutex;pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_init(pMutex,pAttr,nil,PTHREAD_MUTEX_DEFAULT);
end;

function pthread_mutex_destroy(m:p_pthread_mutex):Integer;
var
 mi:pthread_mutex;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 if not STATIC_INITIALIZER(mi) then
 begin
  mi:=XCHG(m^,Pointer(PTHREAD_MUTEX_FREE));
  if (mi=Pointer(PTHREAD_MUTEX_FREE)) then Exit(EINVAL);
  if STATIC_INITIALIZER(mi) then Exit(0);
  if not CAS(mi^.valid,LIFE_MUTEX,DEAD_MUTEX) then Exit(EINVAL);
  _sig_lock;
  if (mi^.event<>0) then CloseHandle(mi^.event);
  FreeMem(mi);
  _sig_unlock;
 end;
 Result:=0;
end;

function ps4_pthread_mutex_destroy(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_mutex_destroy(pMutex);
end;

//---------------------------------------------------------
//sce

function ps4_scePthreadMutexattrInit(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_init(pAttr));
end;

function ps4_scePthreadMutexattrDestroy(pAttr:p_pthread_mutex_attr):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_destroy(pAttr));
end;

function ps4_scePthreadMutexattrGettype(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_gettype(pAttr,t));
end;

function ps4_scePthreadMutexattrSettype(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_settype(pAttr,t));
end;

function ps4_scePthreadMutexattrGetprotocol(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_getprotocol(pAttr,t));
end;

function ps4_scePthreadMutexattrSetprotocol(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_setprotocol(pAttr,t));
end;

function ps4_scePthreadMutexattrGetprioceiling(pAttr:p_pthread_mutex_attr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_getprioceiling(pAttr,t));

end;

function ps4_scePthreadMutexattrSetprioceiling(pAttr:p_pthread_mutex_attr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_mutexattr_setprioceiling(pAttr,t));
end;

//////////////

function ps4_scePthreadMutexLock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=px2sce(pthread_mutex_lock_intern(pMutex,nil,SCE_PTHREAD_MUTEX_DEFAULT));
 _sig_unlock;
end;

function ps4_scePthreadMutexTimedlock(pMutex:PScePthreadMutex;usec:DWORD):Integer; SysV_ABI_CDecl;
var
 t:QWORD;
begin
 t:=_usec2nsec(usec);
 _sig_lock;
 Result:=px2sce(pthread_mutex_lock_intern(pMutex,@t,SCE_PTHREAD_MUTEX_DEFAULT));
 _sig_unlock;
end;

function ps4_scePthreadMutexUnlock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_mutex_unlock(pMutex,SCE_PTHREAD_MUTEX_DEFAULT));
end;

function ps4_scePthreadMutexTrylock(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_mutex_trylock(pMutex,SCE_PTHREAD_MUTEX_DEFAULT));
end;

function ps4_scePthreadMutexInit(pMutex:PScePthreadMutex;pAttr:p_pthread_mutex_attr;str:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_mutex_init(pMutex,pAttr,str,SCE_PTHREAD_MUTEX_DEFAULT));
end;

function ps4_scePthreadMutexDestroy(pMutex:p_pthread_mutex):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_mutex_destroy(pMutex));
end;

end.

