unit ps4_event_flag;

{$mode objfpc}{$H+}

interface

uses
  windows,
  Classes,
  SysUtils,
  spinlock;

const
 SCE_KERNEL_EVF_ATTR_TH_FIFO=$01;
 SCE_KERNEL_EVF_ATTR_TH_PRIO=$02;
 SCE_KERNEL_EVF_ATTR_SINGLE =$10;
 SCE_KERNEL_EVF_ATTR_MULTI  =$20;

 SCE_KERNEL_EVF_WAITMODE_AND      =$01;
 SCE_KERNEL_EVF_WAITMODE_OR       =$02;
 SCE_KERNEL_EVF_WAITMODE_CLEAR_ALL=$10;
 SCE_KERNEL_EVF_WAITMODE_CLEAR_PAT=$20;

 SCE_KERNEL_EVF_ID_INVALID=High(QWORD)-1;

type
 PSceKernelEventFlagOptParam=^SceKernelEventFlagOptParam;
 SceKernelEventFlagOptParam=packed record
  size:QWORD;
 end;

 pwef_node=^wef_node;
 wef_node=packed record
  pNext,pPrev:pwef_node;
  //
  thread:Pointer;
  event:THandle;
  bitPattern:QWORD;
  ResultPat:QWORD;
  waitMode:DWORD;
  ret:Integer;
 end;

 wef_list=object
  pHead,pTail:pwef_node;
  procedure Insert(Node:pwef_node);
  procedure Remove(node:pwef_node);
 end;

 PSceKernelEventFlag=^SceKernelEventFlag;
 SceKernelEventFlag=^SceKernelEventFlag_t;
 SceKernelEventFlag_t=packed record
  valid:DWORD;
  attr:DWORD;
  refs:DWORD;
  lock_sing:r_spin_lock;
  lock_list:r_spin_lock;
  bitPattern:QWORD;
  list:wef_list;
  name:array[0..31] of AnsiChar;
 end;

 function ps4_sceKernelCreateEventFlag(
   ef:PSceKernelEventFlag;
   pName:PChar;
   attr,init:QWORD;
   pOptParam:PSceKernelEventFlagOptParam):Integer; SysV_ABI_CDecl;

function ps4_sceKernelWaitEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD;
  pTimeout:PDWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelPollEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD):Integer; SysV_ABI_CDecl;

function ps4_sceKernelSetEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
function ps4_sceKernelClearEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
function ps4_sceKernelDeleteEventFlag(ef:SceKernelEventFlag):Integer; SysV_ABI_CDecl;

function ps4_sceKernelCancelEventFlag(ef:SceKernelEventFlag;
                                      bitPattern:QWORD;
                                      pNumWaitThreads:Pinteger):Integer; SysV_ABI_CDecl;

implementation

uses
 atomic,
 ntapi,
 sys_kernel,
 sys_pthread,
 sys_signal,
 sys_time;

const
 LIFE_EQ=$BAB1F00D;
 DEAD_EQ=$DEADBEEF;

 EVF_TH_PRIO=SCE_KERNEL_EVF_ATTR_TH_FIFO or SCE_KERNEL_EVF_ATTR_TH_PRIO;
 EVF_TH_LOCK=SCE_KERNEL_EVF_ATTR_SINGLE  or SCE_KERNEL_EVF_ATTR_MULTI;
 WOP_MODES  =SCE_KERNEL_EVF_WAITMODE_AND or SCE_KERNEL_EVF_WAITMODE_OR;

procedure wef_list.Insert(Node:pwef_node);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node^.pPrev:=nil;
 end else
 begin
  pTail^.pNext:=node;
  node^.pPrev:=pTail;
 end;
 node^.pNext:=nil;
 pTail:=node;
end;

procedure wef_list.Remove(node:pwef_node);
begin
 if (node^.pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node^.pNext;
  end;
 end else
 begin
  node^.pPrev^.pNext:=node^.pNext;
 end;
 if (node^.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=node^.pPrev;
  end;
 end else
 begin
  node^.pNext^.pPrev:=node^.pPrev;
 end;
end;

function _test_by_mode(var bits:QWORD;bitPattern:QWORD;waitMode:DWORD):Boolean; inline;
begin
 Result:=False;
 Case (waitMode and WOP_MODES) of
  SCE_KERNEL_EVF_WAITMODE_AND:Result:=((bits and bitPattern)=bitPattern);
  SCE_KERNEL_EVF_WAITMODE_OR :Result:=((bits and bitPattern)<>0)
  else;
 end;
end;

function _change_by_mode(var bits:QWORD;bitPattern:QWORD;waitMode:DWORD):Boolean; inline;
var
 prev:QWORD;
begin
 Result:=False;
 if ((waitMode and SCE_KERNEL_EVF_WAITMODE_CLEAR_ALL)<>0) then
 begin
  prev:=bits;
  bits:=0;
  Result:=(prev<>bits);
 end else
 if ((waitMode and SCE_KERNEL_EVF_WAITMODE_CLEAR_PAT)<>0) then
 begin
  prev:=bits;
  bits:=bits and (not bitPattern);
  Result:=(prev<>bits);
 end;
end;

function _test_and_set(ef:SceKernelEventFlag;bitPattern:QWORD;waitMode:DWORD;pOut:PQWORD):Boolean;
var
 bits:QWORD;
begin
 Result:=False;

  bits:=load_acq_rel(ef^.bitPattern);
  if _test_by_mode(bits,bitPattern,waitMode) then
  begin
   Result:=True;
   if _change_by_mode(bits,bitPattern,waitMode) then
   begin
    store_seq_cst(ef^.bitPattern,bits);
   end;
  end;

 if Result and (pOut<>nil) then
 begin
  pOut^:=bits;
 end;
end;

function _is_single(attr:DWORD):Boolean;
begin
 Result:=(attr and SCE_KERNEL_EVF_ATTR_MULTI)=0;
end;


function ef_enter(ef:SceKernelEventFlag):Integer;
begin
 if (ef=nil) then Exit(ESRCH);
 if not safe_test(ef^.valid,LIFE_EQ) then Exit(EACCES);
 System.InterlockedIncrement(ef^.refs);
 Result:=0;
end;

procedure ef_leave(ef:SceKernelEventFlag);
begin
 if (System.InterlockedDecrement(ef^.refs)=0) then
 begin
  SwFreeMem(ef);
 end;
end;

function _sceKernelCreateEventFlag(ef:PSceKernelEventFlag;
                                   pName:PChar;
                                   attr,init:QWORD):Integer;
var
 data:SceKernelEventFlag;
begin
 if (ef=nil) then Exit(EINVAL);

 data:=SwAllocMem(SizeOf(SceKernelEventFlag_t));
 if (data=nil) then
 begin
  Exit(ENOMEM);
 end;

 if ((attr and EVF_TH_PRIO)=0) then
 begin
  attr:=attr or SCE_KERNEL_EVF_ATTR_TH_FIFO;
 end;

 if ((attr and EVF_TH_LOCK)=0) then
 begin
  attr:=attr or SCE_KERNEL_EVF_ATTR_SINGLE;
 end;

 data^.valid     :=LIFE_EQ;
 data^.attr      :=attr;
 data^.bitPattern:=init;
 data^.refs      :=1;

 if (pName<>nil) then MoveChar0(pName^,data^.name,32);

 ef^:=data;

 Result:=0;
end;

function ps4_sceKernelCreateEventFlag(
  ef:PSceKernelEventFlag;
  pName:PChar;
  attr,init:QWORD;
  pOptParam:PSceKernelEventFlagOptParam):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'sceKernelCreateEventFlag:',pName);

 if (pOptParam<>nil) then
 begin
  _set_errno(EINVAL);
  Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 Result:=_sceKernelCreateEventFlag(ef,pName,attr,init);
 _set_errno(Result);

 Result:=px2sce(Result);
end;

//

function _sceKernelWaitEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD;
  pTimeout:PDWORD):Integer;
var
 t:pthread;
 attr:DWORD;
 timeout:Int64;
 passed :Int64;
 QTIME:QWORD;
 node:wef_node;
 insert:Boolean;
begin
 if (bitPattern=0) then Exit(EINVAL);

 Case (waitMode and WOP_MODES) of
  SCE_KERNEL_EVF_WAITMODE_AND:;
  SCE_KERNEL_EVF_WAITMODE_OR :;
  else
   Exit(EINVAL);
 end;

 t:=_get_curthread;
 if (t=nil) then Exit(ESRCH);

 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 //Writeln('>sceKernelWaitEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 if (pTimeout<>nil) then
 begin
  timeout:=(pTimeout^ div 100);
 end else
 begin
  timeout:=NT_INFINITE;
 end;

 insert:=False;
 node:=Default(wef_node);
 node.thread    :=t;
 node.event     :=CreateEvent(nil,True,false,nil);
 node.bitPattern:=bitPattern;
 node.waitMode  :=waitMode;
 node.ret       :=1;

 attr:=ef^.attr;
 if _is_single(attr) then
 begin
  spin_lock(ef^.lock_list);
    if not spin_trylock(ef^.lock_sing) then
    begin
     spin_unlock(ef^.lock_list);
     ef_leave(ef);
     Exit(EPERM);
    end;
  spin_unlock(ef^.lock_list);
 end;

 Result:=0;
 repeat

  spin_lock(ef^.lock_list);
    if (node.ret<>1) then //is signaled
    begin
     if (pResultPat<>nil) then
     begin
      pResultPat^:=node.ResultPat;
     end;
     Result:=node.ret;
     spin_unlock(ef^.lock_list);
     Break;
    end else
    if (Result=EINTR) then
    begin
     spin_unlock(ef^.lock_list);
     Break;
    end;

    if _test_and_set(ef,bitPattern,waitMode,pResultPat) then
    begin
     store_seq_cst(node.ret,0);
     spin_unlock(ef^.lock_list);
     Result:=0;
     Break;
    end else
    if not insert then
    begin
     ef^.list.Insert(@node);
     insert:=True;
    end;
  spin_unlock(ef^.lock_list);

  if (pTimeout<>nil) then
  begin
   if (timeout=0) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end;

   QTIME:=0;
   SwSaveTime(QTIME);

   timeout:=-timeout;
  end;

  Case SwWaitForSingleObject(node.event,@timeout,True) of
   STATUS_SUCCESS:
    begin
     Result:=0;
    end;
   STATUS_USER_APC,
   STATUS_KERNEL_APC,
   STATUS_ALERTED: //signal interrupt
    begin
     Result:=EINTR;
    end;
   else
    begin
     Result:=ETIMEDOUT;
     Break;
    end;
  end;

  if (pTimeout<>nil) then
  begin
   timeout:=-timeout;

   passed:=SwTimePassedUnits(QTIME);

   if (passed>=timeout) then
   begin
    Result:=ETIMEDOUT;
    Break;
   end else
   begin
    timeout:=timeout-passed;
   end;
  end;

 until false;

 if (pTimeout<>nil) then
 begin
  if (Result=ETIMEDOUT) then
  begin
   pTimeout^:=0;
  end else
  begin
   passed:=SwTimePassedUnits(QTIME);
   pTimeout^:=passed*100;
  end;
 end;

 if insert then
 begin
  spin_lock(ef^.lock_list);
    ef^.list.Remove(@node);
  spin_unlock(ef^.lock_list);
 end;

 CloseHandle(node.event);

 //Writeln('<sceKernelWaitEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 ef_leave(ef);
end;

function ps4_sceKernelWaitEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD;
  pTimeout:PDWORD):Integer; SysV_ABI_CDecl;
begin
 repeat
  _sig_lock;
  Result:=_sceKernelWaitEventFlag(ef,bitPattern,waitMode,pResultPat,pTimeout);
  _sig_unlock;
 until (Result<>EINTR);

 _set_errno(Result);
 Result:=px2sce(Result);
end;

function _sceKernelPollEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD):Integer;
begin
 if (bitPattern=0) then Exit(EINVAL);

 Case (waitMode and WOP_MODES) of
  SCE_KERNEL_EVF_WAITMODE_AND:;
  SCE_KERNEL_EVF_WAITMODE_OR :;
  else
   Exit(EINVAL);
 end;

 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 if _is_single(ef^.attr) then
 begin
  spin_lock(ef^.lock_list);
    if not spin_trylock(ef^.lock_sing) then
    begin
     spin_unlock(ef^.lock_list);
     ef_leave(ef);
     Exit(EPERM);
    end;
  spin_unlock(ef^.lock_list);
 end;

 spin_lock(ef^.lock_list);
   if _test_and_set(ef,bitPattern,waitMode,pResultPat) then
   begin
    Result:=0;
   end else
   begin
    Result:=EBUSY;
   end;
 spin_unlock(ef^.lock_list);

 ef_leave(ef);
end;

function ps4_sceKernelPollEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD):Integer; SysV_ABI_CDecl;
begin
 repeat
  _sig_lock;
  Result:=_sceKernelPollEventFlag(ef,bitPattern,waitMode,pResultPat);
  _sig_unlock;
 until (Result<>EINTR);

 _set_errno(Result);
 Result:=px2sce(Result);
end;

//

procedure _wakeup(node:pwef_node;ret:Integer);
begin
 store_seq_cst(node^.ret,ret);
 SetEvent(node^.event);
end;

function _sceKernelSetEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer;
var
 node:pwef_node;
 bits:QWORD;
 prev:QWORD;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 //Writeln('>sceKernelSetEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 spin_lock(ef^.lock_list);

 bits:=load_acq_rel(ef^.bitPattern) or bitPattern;

 node:=ef^.list.pHead;
 While (node<>nil) do
 begin
  if (node^.ret=1) then
   if _test_by_mode(bits,node^.bitPattern,node^.waitMode) then
   begin
    prev:=bits;
    if _change_by_mode(bits,node^.bitPattern,node^.waitMode) then
    begin
     store_seq_cst(ef^.bitPattern,bits);
    end;
    node^.ResultPat:=prev;

    _wakeup(node,0);
   end;
  node:=node^.pNext;
 end;

 store_seq_cst(ef^.bitPattern,bits);

 spin_unlock(ef^.lock_list);

 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelSetEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceKernelSetEventFlag(ef,bitPattern);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

//

function _sceKernelClearEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 //Writeln(SysLogPrefix, 'sceKernelClearEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 spin_lock(ef^.lock_list);
  fetch_and(ef^.bitPattern,bitPattern);
 spin_unlock(ef^.lock_list);

 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelClearEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceKernelClearEventFlag(ef,bitPattern);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

//

function _sceKernelDeleteEventFlag(ef:SceKernelEventFlag):Integer;
var
 node:pwef_node;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 if not CAS(ef^.valid,LIFE_EQ,DEAD_EQ) then
 begin
  ef_leave(ef);
  Exit(ESRCH);
 end;

 Writeln('>sceKernelDeleteEventFlag:',HexStr(ef),':',ef^.name);

 spin_lock(ef^.lock_list);

 //cancel all
 node:=ef^.list.pHead;
 While (node<>nil) do
 begin
  if (node^.ret=1) then
  begin
   node^.ResultPat:=ef^.bitPattern;

   _wakeup(node,EACCES);
  end;
  node:=node^.pNext;
 end;

 spin_unlock(ef^.lock_list);

 System.InterlockedDecrement(ef^.refs);
 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelDeleteEventFlag(ef:SceKernelEventFlag):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceKernelDeleteEventFlag(ef);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;

//

function _sceKernelCancelEventFlag(ef:SceKernelEventFlag;
                                      bitPattern:QWORD;
                                      pNumWaitThreads:Pinteger):Integer;
var
 node:pwef_node;
 count:Integer;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 Writeln('>sceKernelCancelEventFlag:',HexStr(ef),':',ef^.name);

 spin_lock(ef^.lock_list);

 count:=0;

 //cancel all
 fetch_or(ef^.bitPattern,bitPattern);

 node:=ef^.list.pHead;
 While (node<>nil) do
 begin
  if (node^.ret=1) then
  begin
   node^.ResultPat:=ef^.bitPattern;

   _wakeup(node,ECANCELED);

   Inc(count);
  end;
  node:=node^.pNext;
 end;

 spin_unlock(ef^.lock_list);

 if (pNumWaitThreads<>nil) then
 begin
  pNumWaitThreads^:=count;
 end;

 System.InterlockedDecrement(ef^.refs);
 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelCancelEventFlag(ef:SceKernelEventFlag;
                                      bitPattern:QWORD;
                                      pNumWaitThreads:Pinteger):Integer; SysV_ABI_CDecl;
begin
 _sig_lock;
 Result:=_sceKernelCancelEventFlag(ef,bitPattern,pNumWaitThreads);
 _sig_unlock;

 _set_errno(Result);
 Result:=px2sce(Result);
end;


end.

