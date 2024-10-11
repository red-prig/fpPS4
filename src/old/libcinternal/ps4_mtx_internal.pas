unit ps4_mtx_internal;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 sys_types,
 sys_kernel,
 sys_pthread,
 sys_time,
 ps4_time,
 ps4_mutex,
 ps4_cond,
 ps4_mspace_internal;

type
 pp_mtx=^p_mtx;
 p_mtx=^t_mtx;
 t_mtx=packed record
  param:DWORD;
  _align1:DWORD;
  mutex:Pointer;
  cond:Pointer;
  thread:Pointer;
  count:DWORD;
  _align2:DWORD;
 end;

function  ps4__Mtxinit(mutex:Pointer;name:Pchar):Boolean; SysV_ABI_CDecl;
function  ps4__Mtxdst(mutex:Pointer):Boolean; SysV_ABI_CDecl;
function  ps4__Mtxlock(mutex:Pointer):Boolean; SysV_ABI_CDecl;
function  ps4__Mtxunlock(mutex:Pointer):Boolean; SysV_ABI_CDecl;
//
function  ps4__Mtx_init_with_name(pmtx:pp_mtx;param:DWORD;name:Pchar):Integer; SysV_ABI_CDecl;
function  ps4__Mtx_init(pmtx:pp_mtx;param:DWORD):Integer; SysV_ABI_CDecl;
procedure ps4__Mtx_destroy(pmtx:pp_mtx); SysV_ABI_CDecl;
function  ps4__Mtx_current_owns(pmtx:pp_mtx):Boolean; SysV_ABI_CDecl;
function  ps4__Mtx_lock(pmtx:pp_mtx):Integer; SysV_ABI_CDecl;
function  ps4__Mtx_trylock(pmtx:pp_mtx):Integer; SysV_ABI_CDecl;
function  ps4__Mtx_timedlock(pmtx:pp_mtx;time:Ptimespec):Integer; SysV_ABI_CDecl;
function  ps4__Mtx_unlock(pmtx:pp_mtx):Integer; SysV_ABI_CDecl;

implementation

procedure _snprintf_param(_out:PChar;maxlen:size_t;param:PChar);
begin
 if (param<>nil) then
 begin
  strlcopy(_out,'SceLibcI_',maxlen);
  strlcat(_out,param,maxlen);
  Exit;
 end;
 strlcopy(_out,'SceLibcI',maxlen);
end;

function ps4__Mtxinit(mutex:Pointer;name:Pchar):Boolean; SysV_ABI_CDecl;
var
 ret,ret2:Integer;
 attr:Pointer;
 str:array[0..39] of Char;
begin
 attr:=nil;
 _snprintf_param(str,32,name);
 ret:=ps4_scePthreadMutexattrInit(attr);
 Result:=True;
 if (ret=0) then
 begin
  ret:=ps4_scePthreadMutexattrSettype(attr,SCE_PTHREAD_MUTEX_RECURSIVE);
  if (ret=0) then
  begin
   ret:=ps4_scePthreadMutexInit(mutex,attr,str);
   ret2:=ps4_scePthreadMutexattrDestroy(attr);
   if (ret=0) then
   begin
    Result:=(ret2<>0);
   end;
  end else
  begin
   ps4_scePthreadMutexattrDestroy(attr);
  end;
 end;
end;

function ps4__Mtxdst(mutex:Pointer):Boolean; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexDestroy(mutex)<>0;
end;

function ps4__Mtxlock(mutex:Pointer):Boolean; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexLock(mutex)<>0;
end;

function ps4__Mtxunlock(mutex:Pointer):Boolean; SysV_ABI_CDecl;
begin
 Result:=ps4_scePthreadMutexUnlock(mutex)<>0;
end;

//

function ps4__Mtx_init_with_name(pmtx:pp_mtx;param:DWORD;name:Pchar):Integer; SysV_ABI_CDecl;
var
 ret:Integer;
 _mtx:p_mtx;
begin
 pmtx^:=nil;
 _mtx:=ps4_calloc(1,SizeOf(t_mtx));
 ret:=1;
 if (_mtx<>nil) then
 begin
  if ((param and $104)<>0) then
  begin
   ret:=ps4_scePthreadCondInit(@_mtx^.cond,nil,name);
   if (ret<>0) then
   begin
    ps4_free(_mtx);
    Exit(4);
   end;
  end;
  ret:=0;
  ps4_scePthreadMutexInit(@_mtx^.mutex,nil,name);
  _mtx^.param:=param;
  pmtx^:=_mtx;
 end;
 Result:=ret;
end;

function ps4__Mtx_init(pmtx:pp_mtx;param:DWORD):Integer; SysV_ABI_CDecl;
var
 str:array[0..39] of Char;
begin
 _snprintf_param(str,32,'Mtx');
 Result:=ps4__Mtx_init_with_name(pmtx,param,str);
end;

procedure ps4__Mtx_destroy(pmtx:pp_mtx); SysV_ABI_CDecl;
var
 _mtx:p_mtx;
begin
 if (pmtx=nil) then Exit;
 _mtx:=pmtx^;
 if (_mtx=nil) then Exit;
 ps4_scePthreadMutexDestroy(@_mtx^.mutex);
 if ((_mtx^.param and $104)<>0) then
 begin
  ps4_scePthreadCondDestroy(@_mtx^.cond);
 end;
 ps4_free(_mtx);
end;

function ps4__Mtx_current_owns(pmtx:pp_mtx):Boolean; SysV_ABI_CDecl;
var
 _mtx:p_mtx;
begin
 _mtx:=pmtx^;
 if (_mtx^.count=0) then
 begin
  Result:=False;
 end else
 begin
  Result:=(_mtx^.thread=_get_curthread);
 end;
end;

function ps4__Mtx_lock(pmtx:pp_mtx):Integer; SysV_ABI_CDecl;
var
 ret:Integer;
 _mtx:p_mtx;
begin
 _mtx:=pmtx^;
 ret:=ps4_scePthreadMutexLock(@_mtx^.mutex);
 if (ret=0) then
 begin
  if ((_mtx^.param and $104)<>0) then
  begin
   Inc(_mtx^.count);
   _mtx^.thread:=_get_curthread;
  end else
  begin
   if (_mtx^.count<>0) then
   begin
    if (_mtx^.thread<>_get_curthread) then
    begin
     while (_mtx^.count<>0) do
     begin
      ps4_scePthreadCondWait(@_mtx^.cond,@_mtx^.mutex);
      _mtx:=pmtx^;
     end;
    end;
   end;
   Inc(_mtx^.count);
   _mtx^.thread:=_get_curthread;
   ps4_scePthreadMutexUnlock(@_mtx^.mutex);
  end;
  ret:=0;
 end else
 begin
  ret:=ord(ret<>SCE_KERNEL_ERROR_EDEADLK)+3;
 end;
 Result:=ret;
end;

function ps4__Mtx_trylock(pmtx:pp_mtx):Integer; SysV_ABI_CDecl;
label
 _exit;
var
 ret:Integer;
 _mtx:p_mtx;
begin
 _mtx:=pmtx^;
 if ((_mtx^.param and $104)<>0) then
 begin
  ret:=ps4_scePthreadMutexTrylock(@_mtx^.mutex);
  if (ret=SCE_KERNEL_ERROR_EBUSY) then Exit(3);
  if (ret<>0) then Exit(4);
  _mtx^.thread:=_get_curthread;
  Inc(_mtx^.count);
  Exit(0);
 end;
 ret:=ps4_scePthreadMutexLock(@_mtx^.mutex);
 if (ret<>0) then Exit(4);
 if (_mtx^.count<>0) then
 begin
  ret:=3;
  if ((_mtx^.param and $100)=0) then goto _exit;
  if (_mtx^.thread<>_get_curthread) then goto _exit;
 end;
 ret:=0;
 _mtx^.thread:=_get_curthread;
 Inc(_mtx^.count);
_exit:
 ps4_scePthreadMutexUnlock(@_mtx^.mutex);
 Result:=ret;
end;

function ps4__Mtx_timedlock(pmtx:pp_mtx;time:Ptimespec):Integer; SysV_ABI_CDecl;
label
 _exit;
var
 ret:Integer;
 usec:DWORD;
 _mtx:p_mtx;
begin
 _mtx:=pmtx^;
 ret:=ps4_scePthreadMutexLock(@_mtx^.mutex);
 if (ret<>0) then Exit(4);
 if (_mtx^.count<>0) then
 begin
  if (_mtx^.thread<>_get_curthread) then
  begin
   usec:=_msec2usec(_pthread_rel_time_in_ms(time^));
   if (_mtx^.count<>0) then
   begin
    repeat
     ret:=ps4_scePthreadCondTimedwait(@_mtx^.cond,@_mtx^.mutex,usec);
     _mtx:=pmtx^;
     if (_mtx^.count=0) then break;
    until (ret=SCE_KERNEL_ERROR_ETIMEDOUT);
    Result:=2;
    if (ret=SCE_KERNEL_ERROR_ETIMEDOUT) then goto _exit;
   end;
  end else
  begin
   Result:=2;
   if ((_mtx^.param and $100)=0) then goto _exit;
  end;
 end;
 Result:=0;
 _mtx^.thread:=_get_curthread;
 Inc(_mtx^.count);
_exit:
 ps4_scePthreadMutexUnlock(@_mtx^.mutex);
end;

function ps4__Mtx_unlock(pmtx:pp_mtx):Integer; SysV_ABI_CDecl;
var
 ret:Integer;
 _mtx:p_mtx;
begin
_mtx:=pmtx^;
 if ((_mtx^.param and $104)<>0) then
 begin
  Inc(_mtx^.count);
 end else
 begin
  ret:=ps4_scePthreadMutexLock(@_mtx^.mutex);
  if (ret<>0) then Exit(4);
  Dec(_mtx^.count);
  if (_mtx^.count=0) then
  begin
   ps4_scePthreadCondSignal(@_mtx^.cond);
  end;
 end;
 ps4_scePthreadMutexUnlock(@_mtx^.mutex);
 Result:=0;
end;


end.

