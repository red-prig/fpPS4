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
 wef_node=record
  pNext,pPrev:pwef_node;
  pParent:Pointer;
  //
  thread:THandle;
  bitPattern:QWORD;
  ResultPat:QWORD;
  waitMode:DWORD;
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
  bitPattern:QWORD;
  lock_sing:r_spin_lock;
  lock_list:r_spin_lock;
  list:wef_list;
  thread:THandle;
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

 ef^:=data;
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
 Result:=0;

 if (ef=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);
 if (ef^.valid<>LIFE_EQ) then Exit(SCE_KERNEL_ERROR_EACCES);

 if (bitPattern=0) then Exit(SCE_KERNEL_ERROR_EINVAL);

 Case (waitMode and WOP_MODES) of
  SCE_KERNEL_EVF_WAITMODE_AND:;
  SCE_KERNEL_EVF_WAITMODE_OR :;
  else
   Exit(SCE_KERNEL_ERROR_EINVAL);
 end;

 t:=_get_curthread;
 if (t=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);

 _sig_lock;

 Writeln('>sceKernelWaitEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

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
     Exit(SCE_KERNEL_ERROR_EPERM);
    end;
    ef^.thread:=t^.handle;
  spin_unlock(ef^.lock_list);
 end else
 begin
  node:=Default(wef_node);
  node.pParent   :=ef;
  node.thread    :=t^.handle;
  node.bitPattern:=bitPattern;
  node.waitMode  :=waitMode;

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
    if (node.pParent=nil) then //is signaled
    begin
     if (pResultPat<>nil) then
     begin
      pResultPat^:=node.ResultPat;
     end;
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
  ef^.thread:=0;
  spin_unlock(ef^.lock_sing);
 end else
 begin
  spin_lock(ef^.lock_list);
    ef^.list.Remove(@node);
  spin_unlock(ef^.lock_list);
 end;

 Writeln('<sceKernelWaitEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 _sig_unlock;
end;

procedure _apc_null(dwParam:PTRUINT); stdcall;
begin
end;

function ps4_sceKernelSetEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
var
 node:pwef_node;
 prev,bits,count:QWORD;
 AllPattern:QWORD;
 AllwaitMode:DWORD;
 attr:DWORD;
begin
 if (ef=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);
 if (ef^.valid<>LIFE_EQ) then Exit(SCE_KERNEL_ERROR_ESRCH);

 _sig_lock;

 Writeln('>sceKernelSetEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 count:=0;
 AllPattern:=0;
 AllwaitMode:=0;

 attr:=ef^.attr;

 spin_lock(ef^.lock_list);

 if _is_single(attr) then
 begin
  fetch_or(ef^.bitPattern,bitPattern);
  NtQueueApcThread(ef^.thread,@_apc_null,0,nil,0);
 end else
 begin

  Writeln('!sceKernelSetEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

  bits:=load_acq_rel(ef^.bitPattern) or bitPattern;

  node:=ef^.list.pHead;
  While (node<>nil) do
  begin
   if (node^.pParent<>nil) then
    if _test_by_mode(bits,node^.bitPattern,node^.waitMode) then
    begin
     AllPattern :=AllPattern  or node^.bitPattern;
     AllwaitMode:=AllwaitMode or node^.waitMode;
     Inc(count);
    end;
   node:=node^.pNext;
  end;

  if (count<>0) then
  begin
   prev:=bits;

   _change_by_mode(bits,AllPattern,AllwaitMode);
   store_seq_cst(ef^.bitPattern,bits);

   node:=ef^.list.pHead;
   While (node<>nil) do
   begin
    if (node^.pParent<>nil) then
     if _test_by_mode(prev,node^.bitPattern,node^.waitMode) then
     begin
      node^.ResultPat:=bits;
      node^.pParent  :=nil;
      NtQueueApcThread(node^.thread,@_apc_null,0,nil,0);
     end;
    node:=node^.pNext;
   end;

  end else
  begin
   store_seq_cst(ef^.bitPattern,bits);
  end;

 end;

 spin_unlock(ef^.lock_list);

 _sig_unlock;
 Result:=0;
end;

function ps4_sceKernelClearEventFlag(ef:SceKernelEventFlag;bitPattern:QWORD):Integer; SysV_ABI_CDecl;
var
 t:Int64;
 //pt:pthread;
begin
 if (ef=nil) then Exit(SCE_KERNEL_ERROR_ESRCH);
 if (ef^.valid<>LIFE_EQ) then Exit(SCE_KERNEL_ERROR_ESRCH);

 Writeln('sceKernelClearEventFlag:',HexStr(ef),':',ef^.name,':',HexStr(bitPattern,16),':',ThreadID);

 //pt:=ps4_pthread_self;
 //if pt^.name='main' then
 //begin
 // DebugBreak;
 //end;

 //t:=-10000;
 //SwDelayExecution(False,@t); //100ms

 spin_lock(ef^.lock_list);
  fetch_and(ef^.bitPattern,bitPattern);
 spin_unlock(ef^.lock_list);

 Result:=0;
end;

{
int sceKernelDeleteEventFlag(SceKernelEventFlag ef);

int sceKernelPollEventFlag(SceKernelEventFlag ef, uint64_t bitPattern,
			   uint32_t waitMode, uint64_t *pResultPat);
int sceKernelCancelEventFlag(SceKernelEventFlag ef, uint64_t setPattern,
			     int *pNumWaitThreads);
                             }

end.

