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
  thread:THandle;
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
  single:record
   thread:THandle;
   ret:Integer;
  end;
  name:array[0..31] of AnsiChar;
 end;

function ps4_sceKernelCreateEventFlag(
  ef:PSceKernelEventFlag;
  pName:PChar;
  attr:DWORD;
  initPattern:QWORD;
  pOptParam:PSceKernelEventFlagOptParam):Integer; SysV_ABI_CDecl;

function ps4_sceKernelWaitEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD;
  pTimeout:PDWORD):Integer; SysV_ABI_CDecl;

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

function ps4_sceKernelCreateEventFlag(
  ef:PSceKernelEventFlag;
  pName:PChar;
  attr:DWORD;
  initPattern:QWORD;
  pOptParam:PSceKernelEventFlagOptParam):Integer; SysV_ABI_CDecl;
var
 data:SceKernelEventFlag;
begin
 Writeln('sceKernelCreateEventFlag:',pName);
 if (ef=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 data:=SwAllocMem(SizeOf(SceKernelEventFlag_t));
 if (data=nil) then
 begin
  Exit(SCE_KERNEL_ERROR_ENOMEM);
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
 data^.bitPattern:=initPattern;

 if (pName<>nil) then MoveChar0(pName^,data^.name,32);

 data^.refs:=1;

 if not CAS(ef^,ef^,data) then
 begin
  FreeMem(data);
 end;

 Result:=0;
end;

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
 if (ef=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);
 if not safe_test(ef^.valid,LIFE_EQ) then Exit(SCE_KERNEL_ERROR_EACCES);
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

function ps4_sceKernelWaitEventFlag(
  ef:SceKernelEventFlag;
  bitPattern:QWORD;
  waitMode:DWORD;
  pResultPat:PQWORD;
  pTimeout:PDWORD):Integer; SysV_ABI_CDecl;
var
 t:pthread;
 attr:DWORD;
 timeout:Int64;
 passed :Int64;
 START:QWORD;
 QTIME:QWORD;
 node:wef_node;
begin
 if (bitPattern=0) then Exit(SCE_KERNEL_ERROR_EINVAL);

 Case (waitMode and WOP_MODES) of
  SCE_KERNEL_EVF_WAITMODE_AND:;
  SCE_KERNEL_EVF_WAITMODE_OR :;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 t:=_get_curthread;
 if (t=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);

 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 _sig_lock;

 //Writeln('>sceKernelWaitEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 if (pTimeout<>nil) then
 begin
  timeout:=(pTimeout^ div 100);
  SwSaveTime(START);
 end else
 begin
  timeout:=NT_INFINITE;
 end;

 attr:=ef^.attr;
 if _is_single(attr) then
 begin
  spin_lock(ef^.lock_list);
    if not spin_trylock(ef^.lock_sing) then
    begin
     spin_unlock(ef^.lock_list);
     _sig_unlock;
     ef_leave(ef);
     Exit(SCE_KERNEL_ERROR_EPERM);
    end;
    ef^.single.thread:=t^.handle;
    ef^.single.ret:=1;
  spin_unlock(ef^.lock_list);
 end else
 begin
  node:=Default(wef_node);
  node.thread    :=t^.handle;
  node.bitPattern:=bitPattern;
  node.waitMode  :=waitMode;
  node.ret       :=1;

  spin_lock(ef^.lock_list);
    ef^.list.Insert(@node);
  spin_unlock(ef^.lock_list);
 end;

 repeat

   if _is_single(attr) then
   begin

    if _test_and_set(ef,bitPattern,waitMode,pResultPat) then
    begin
     Break;
    end;

   end else
   begin
    if (node.ret<>1) then //is signaled
    begin
     if (pResultPat<>nil) then
     begin
      pResultPat^:=node.ResultPat;
     end;
     Result:=node.ret;
     Break;
    end;

    spin_lock(ef^.lock_list);
      if _test_and_set(ef,bitPattern,waitMode,pResultPat) then
      begin
       spin_unlock(ef^.lock_list);
       Break;
      end;
    spin_unlock(ef^.lock_list);

   end;

   if (pTimeout<>nil) then
   begin
    if (timeout=0) then
    begin
     Result:=SCE_KERNEL_ERROR_ETIMEDOUT;
     Break;
    end;

    SwSaveTime(QTIME);

    timeout:=-timeout;
    SwDelayExecution(True,@timeout);
    timeout:=-timeout;

    passed:=SwTimePassedUnits(QTIME);

    if (passed>=timeout) then
    begin
     Result:=SCE_KERNEL_ERROR_ETIMEDOUT;
     Break;
    end else
    begin
     timeout:=timeout-passed;
    end;

   end else
   begin
    //timeout:=-10000;
    SwDelayExecution(True,@timeout);
   end;


 until false;

 if (pTimeout<>nil) then
 begin
  if (Result=SCE_KERNEL_ERROR_ETIMEDOUT) then
  begin
   pTimeout^:=0;
  end else
  begin
   passed:=SwTimePassedUnits(QTIME);
   pTimeout^:=passed*100;
  end;
 end;

 if _is_single(attr) then
 begin
  ef^.single.thread:=0;
  Result:=ef^.single.ret;
  spin_unlock(ef^.lock_sing);
 end else
 begin
  spin_lock(ef^.lock_list);
    ef^.list.Remove(@node);
  spin_unlock(ef^.lock_list);
 end;

 //Writeln('<sceKernelWaitEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 _sig_unlock;
 ef_leave(ef);
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function ps4_sceKernelSetEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
var
 node:pwef_node;
 bits:QWORD;
 attr:DWORD;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 _sig_lock;

 //Writeln('>sceKernelSetEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 spin_lock(ef^.lock_list);

 attr:=ef^.attr;

 if _is_single(attr) then
 begin
  fetch_or(ef^.bitPattern,bitPattern);
  ef^.single.ret:=0;
  NtQueueApcThread(ef^.single.thread,@_apc_null,0,nil,0);
 end else
 begin

  //Writeln('!sceKernelSetEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

  bits:=load_acq_rel(ef^.bitPattern) or bitPattern;

  node:=ef^.list.pHead;
  While (node<>nil) do
  begin
   if (node^.ret=1) then
    if _test_by_mode(bits,node^.bitPattern,node^.waitMode) then
    begin
     if _change_by_mode(bits,node^.bitPattern,node^.waitMode) then
     begin
      store_seq_cst(ef^.bitPattern,bits);
     end;
     node^.ResultPat:=bits;
     node^.ret:=0;
     NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);
    end;
   node:=node^.pNext;
  end;

  store_seq_cst(ef^.bitPattern,bits);

 end;

 spin_unlock(ef^.lock_list);

 _sig_unlock;
 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelClearEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit;

 //Writeln('sceKernelClearEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 spin_lock(ef^.lock_list);
  fetch_and(ef^.bitPattern,bitPattern);
 spin_unlock(ef^.lock_list);

 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelDeleteEventFlag(ef:SceKernelEventFlag):Integer; SysV_ABI_CDecl;
var
 node:pwef_node;
 attr:DWORD;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit(SCE_KERNEL_ERROR_ESRCH);

 if not CAS(ef^.valid,LIFE_EQ,DEAD_EQ) then
 begin
  ef_leave(ef);
  Exit(SCE_KERNEL_ERROR_ESRCH);
 end;

 _sig_lock;

 Writeln('>sceKernelDeleteEventFlag:',HexStr(ef),':',ef^.name);

 spin_lock(ef^.lock_list);

 attr:=ef^.attr;

 //cancel all
 if _is_single(attr) then
 begin
  ef^.single.ret:=SCE_KERNEL_ERROR_EACCES;
  NtQueueApcThread(ef^.single.thread,@_apc_null,0,nil,0);
 end else
 begin
  node:=ef^.list.pHead;
  While (node<>nil) do
  begin
   if (node^.ret=1) then
   begin
    node^.ResultPat:=ef^.bitPattern;
    node^.ret:=SCE_KERNEL_ERROR_EACCES;
    NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);
   end;
   node:=node^.pNext;
  end;
 end;

 spin_unlock(ef^.lock_list);

 _sig_unlock;

 System.InterlockedDecrement(ef^.refs);
 ef_leave(ef);
 Result:=0;
end;

function ps4_sceKernelCancelEventFlag(ef:SceKernelEventFlag;
                                      bitPattern:QWORD;
                                      pNumWaitThreads:Pinteger):Integer; SysV_ABI_CDecl;
var
 node:pwef_node;
 attr:DWORD;
 count:Integer;
begin
 Result:=ef_enter(ef);
 if (Result<>0) then Exit(SCE_KERNEL_ERROR_ESRCH);

 _sig_lock;

 Writeln('>sceKernelCancelEventFlag:',HexStr(ef),':',ef^.name);

 spin_lock(ef^.lock_list);

 attr:=ef^.attr;

 count:=0;

 //cancel all
 if _is_single(attr) then
 begin
  fetch_or(ef^.bitPattern,bitPattern);

  ef^.single.ret:=SCE_KERNEL_ERROR_ECANCELED;
  NtQueueApcThread(ef^.single.thread,@_apc_null,0,nil,0);

  count:=1;
 end else
 begin
  fetch_or(ef^.bitPattern,bitPattern);

  node:=ef^.list.pHead;
  While (node<>nil) do
  begin
   if (node^.ret=1) then
   begin
    node^.ResultPat:=ef^.bitPattern;
    node^.ret:=SCE_KERNEL_ERROR_ECANCELED;
    NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);

    Inc(count);
   end;
   node:=node^.pNext;
  end;
 end;

 spin_unlock(ef^.lock_list);

 _sig_unlock;

 if (pNumWaitThreads<>nil) then
 begin
  pNumWaitThreads^:=count;
 end;

 System.InterlockedDecrement(ef^.refs);
 ef_leave(ef);
 Result:=0;
end;

{
int sceKernelPollEventFlag(SceKernelEventFlag ef, uint64_t bitPattern,
			   uint32_t waitMode, uint64_t *pResultPat);
                             }

end.

