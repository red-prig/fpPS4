unit ps4_rwlock;

{$mode objfpc}{$H+}

interface

uses
 RWLock,
 ps4_types;

const
 PTHREAD_RWLOCK_INITIALIZER=nil;

 SCE_PTHREAD_RWLOCK_NORMAL        = 1; // Default POSIX rwlock
 SCE_PTHREAD_RWLOCK_PREFER_READER = 2; // Reader preferred rwlock

type
 Ppthread_rwlock=^pthread_rwlock;
 pthread_rwlock=^pthread_rwlock_t;
 pthread_rwlock_t=record
  valid:DWORD;
  Lock:TRWLock;
  name:array[0..31] of AnsiChar;
 end;

 Ppthread_rwlockattr=^pthread_rwlockattr_t;
 pthread_rwlockattr_t=packed record
  _type:0..3;           //2
  _shared:0..1;         //1
  _align:0..536870911;  //29
  _align2:Integer;      //32
 end;

function ps4_pthread_rwlockattr_init(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlockattr_destroy(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlockattr_gettype_np(pAttr:Ppthread_rwlockattr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlockattr_settype_np(pAttr:Ppthread_rwlockattr;t:Integer):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlockattr_getpshared(pAttr:Ppthread_rwlockattr;t:PInteger):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlockattr_setpshared(pAttr:Ppthread_rwlockattr;t:Integer):Integer; SysV_ABI_CDecl;

function ps4_pthread_rwlock_init(pRwlock:Ppthread_rwlock;pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlock_destroy(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlock_rdlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlock_wrlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlock_tryrdlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlock_trywrlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_pthread_rwlock_unlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;

function ps4_scePthreadRwlockattrInit(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockattrDestroy(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockattrGettype(pAttr:Ppthread_rwlockattr;t:Pinteger):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockattrSettype(pAttr:Ppthread_rwlockattr;t:integer):Integer; SysV_ABI_CDecl;

function ps4_scePthreadRwlockInit(pRwlock:Ppthread_rwlock;pAttr:Ppthread_rwlockattr;str:PChar):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockDestroy(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockRdlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockWrlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockTryrdlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockTrywrlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
function ps4_scePthreadRwlockUnlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;

//function ps4_scePthreadRwlockTimedrdlock(pRwlock:Ppthread_rwlock;usec:DWORD):Integer; SysV_ABI_CDecl;
//function ps4_scePthreadRwlockTimedwrlock(pRwlock:Ppthread_rwlock;usec:DWORD):Integer; SysV_ABI_CDecl;

implementation

Uses
 spinlock,
 ps4_mutex,
 ps4_libkernel;

//int pthread_rwlock_timedrdlock(pthread_rwlock_t *,const struct timespec *);
//int pthread_rwlock_timedwrlock(pthread_rwlock_t *,const struct timespec *);

//int pthread_rwlockattr_getkind_np(const pthread_rwlockattr_t *,int *);
//int pthread_rwlockattr_setkind_np(pthread_rwlockattr_t *, int);

function ps4_pthread_rwlockattr_init(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 pAttr^:=Default(pthread_rwlockattr_t);
 pAttr^._type:=SCE_PTHREAD_RWLOCK_NORMAL;
 Result:=0;
end;

function ps4_pthread_rwlockattr_destroy(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Result:=0;
end;

function ps4_pthread_rwlockattr_gettype_np(pAttr:Ppthread_rwlockattr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._type;
 Result:=0;
end;

function ps4_pthread_rwlockattr_settype_np(pAttr:Ppthread_rwlockattr;t:Integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(EINVAL);
 Case t of
  SCE_PTHREAD_RWLOCK_NORMAL       :;
  SCE_PTHREAD_RWLOCK_PREFER_READER:;
  else
   Exit(EINVAL);
 end;
 pAttr^._type:=t;
 Result:=0;
end;

function ps4_pthread_rwlockattr_getpshared(pAttr:Ppthread_rwlockattr;t:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(EINVAL);
 t^:=pAttr^._shared;
 Result:=0;
end;

function ps4_pthread_rwlockattr_setpshared(pAttr:Ppthread_rwlockattr;t:Integer):Integer; SysV_ABI_CDecl;
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

Const
 LIFE_RWLOCK=$BAB1F0ED;
 DEAD_RWLOCK=$DEADB0EF;

function STATIC_RWL_INITIALIZER(x:pthread_rwlock):Boolean; inline;
begin
 Result:=(x=PTHREAD_RWLOCK_INITIALIZER);
end;

function CAS(Var addr:Pointer;Comp,New:Pointer):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function CAS(Var addr:DWORD;Comp,New:DWORD):Boolean; inline;
begin
 Result:=System.InterlockedCompareExchange(addr,New,Comp)=Comp;
end;

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

function rwlock_impl_init(m:Ppthread_rwlock;mi:pthread_rwlock):pthread_rwlock;
var
 new_mi:pthread_rwlock;
begin
 new_mi:=AllocMem(SizeOf(pthread_rwlock_t));
 if (new_mi=nil) then Exit(new_mi);

 new_mi^.valid:=LIFE_RWLOCK;
 rwlock_init(new_mi^.Lock);

 if CAS(m^,mi,new_mi) then
 begin
  Result:=new_mi;
 end else
 begin
  rwlock_destroy(new_mi^.Lock);
  FreeMem(new_mi);
  Result:=m^;
 end;
end;

function rwlock_impl(m:Ppthread_rwlock;var mi:pthread_rwlock):Integer;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 if STATIC_RWL_INITIALIZER(mi) then
 begin
  mi:=rwlock_impl_init(m,mi);
  if (mi=nil) then Exit(ENOMEM);
 end;
 if (mi^.valid<>LIFE_RWLOCK) then Exit(EINVAL);
end;

function pthread_rwlock_init(m:Ppthread_rwlock;a:Ppthread_rwlockattr;str:PChar):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 Result:=rwlock_impl(m,mi);
 if (Result<>0) then Exit;
 if (str<>nil) then MoveChar0(str^,mi^.name,32);
end;

function ps4_pthread_rwlock_init(pRwlock:Ppthread_rwlock;pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
begin
 Result:=pthread_rwlock_init(pRwlock,pAttr,nil);
end;

function ps4_pthread_rwlock_destroy(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 if (m=nil) then Exit(EINVAL);
 mi:=m^;
 if not STATIC_RWL_INITIALIZER(mi) then
 begin
  mi:=XCHG(m^,nil);
  if STATIC_RWL_INITIALIZER(mi) then Exit(0);
  if not CAS(mi^.valid,LIFE_RWLOCK,DEAD_RWLOCK) then Exit(EINVAL);
  rwlock_destroy(mi^.Lock);
  FreeMem(mi);
 end;
 Result:=0;
end;

function ps4_pthread_rwlock_rdlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 Result:=rwlock_impl(m,mi);
 if (Result<>0) then Exit;
 rwlock_rdlock(mi^.Lock);
end;

function ps4_pthread_rwlock_wrlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 Result:=rwlock_impl(m,mi);
 if (Result<>0) then Exit;
 rwlock_wrlock(mi^.Lock);
end;

function ps4_pthread_rwlock_tryrdlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 Result:=rwlock_impl(m,mi);
 if (Result<>0) then Exit;
 if not rwlock_tryrdlock(mi^.Lock) then Result:=EBUSY;
end;

function ps4_pthread_rwlock_trywrlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 Result:=rwlock_impl(m,mi);
 if (Result<>0) then Exit;
 if not rwlock_trywrlock(mi^.Lock) then Result:=EBUSY;
end;

function ps4_pthread_rwlock_unlock(m:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
var
 mi:pthread_rwlock;
begin
 Result:=rwlock_impl(m,mi);
 if (Result<>0) then Exit;
 rwlock_unlock(mi^.Lock);
end;

///////////

function ps4_scePthreadRwlockattrInit(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 pAttr^:=Default(pthread_rwlockattr_t);
 pAttr^._type:=SCE_PTHREAD_RWLOCK_NORMAL;
 Result:=0;
end;

function ps4_scePthreadRwlockattrDestroy(pAttr:Ppthread_rwlockattr):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Result:=0;
end;

function ps4_scePthreadRwlockattrGettype(pAttr:Ppthread_rwlockattr;t:Pinteger):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) or (t=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 t^:=pAttr^._type;
 Result:=0;
end;

function ps4_scePthreadRwlockattrSettype(pAttr:Ppthread_rwlockattr;t:integer):Integer; SysV_ABI_CDecl;
begin
 if (pAttr=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);
 Case t of
  SCE_PTHREAD_RWLOCK_NORMAL       :;
  SCE_PTHREAD_RWLOCK_PREFER_READER:;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;
 pAttr^._type:=t;
 Result:=0;
end;

//

function ps4_scePthreadRwlockInit(pRwlock:Ppthread_rwlock;pAttr:Ppthread_rwlockattr;str:PChar):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(pthread_rwlock_init(pRwlock,pAttr,str));
end;

function ps4_scePthreadRwlockDestroy(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rwlock_destroy(pRwlock));
end;

function ps4_scePthreadRwlockRdlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rwlock_rdlock(pRwlock));
end;

function ps4_scePthreadRwlockWrlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rwlock_wrlock(pRwlock));
end;

function ps4_scePthreadRwlockTryrdlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rwlock_tryrdlock(pRwlock));
end;

function ps4_scePthreadRwlockTrywrlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rwlock_trywrlock(pRwlock));
end;

function ps4_scePthreadRwlockUnlock(pRwlock:Ppthread_rwlock):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_rwlock_unlock(pRwlock));
end;

end.

