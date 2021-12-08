unit ps4_queue;

{$mode objfpc}{$H+}

interface

uses
  windows,
  Classes, SysUtils;

const
 EVFILT_READ                =(-1) ;
 EVFILT_WRITE               =(-2) ;
 EVFILT_AIO                 =(-3) ;   // attached to aio requests
 EVFILT_VNODE               =(-4) ;   // attached to vnodes
 EVFILT_PROC                =(-5) ;   // attached to struct proc
 EVFILT_SIGNAL              =(-6) ;   // attached to struct proc
 EVFILT_TIMER               =(-7) ;   // timers
 EVFILT_FS                  =(-9) ;   // filesystem events
 EVFILT_LIO                 =(-10);   // attached to lio requests
 EVFILT_USER                =(-11);   // User events
 EVFILT_POLLING             =(-12);
 EVFILT_DISPLAY             =(-13);
 EVFILT_GRAPHICS_CORE       =(-14);
 EVFILT_HRTIMER             =(-15);
 EVFILT_UVD_TRAP            =(-16);
 EVFILT_VCE_TRAP            =(-17);
 EVFILT_SDMA_TRAP           =(-18);
 EVFILT_REG_EV		    =(-19);
 EVFILT_GPU_EXCEPTION	    =(-20);
 EVFILT_GPU_SYSTEM_EXCEPTION=(-21);
 EVFILT_GPU_DBGGC_EV	    =(-22);
 EVFILT_SYSCOUNT	    =(22) ;

// actions
 EV_ADD		=$0001;		// add event to kq (implies enable)
 EV_DELETE	=$0002;		// delete event from kq
 EV_ENABLE	=$0004;		// enable event
 EV_DISABLE	=$0008;		// disable event (not reported)

// flags
 EV_ONESHOT	=$0010;		// only report one occurrence
 EV_CLEAR	=$0020;		// clear event state after reporting
 EV_RECEIPT	=$0040;		// force EV_ERROR on success, data=0
 EV_DISPATCH	=$0080;		// disable event after reporting

 EV_SYSFLAGS	=$F000;		// reserved by system
 EV_FLAG1	=$2000;		// filter-specific flag

// returned values
 EV_EOF		=$8000;		// EOF detected
 EV_ERROR	=$4000;		// error, data contains errno

 NOTE_DELETE	=$0001;	        // vnode was removed
 NOTE_WRITE	=$0002;	        // data contents changed
 NOTE_EXTEND	=$0004;	        // size increased
 NOTE_ATTRIB	=$0008;	        // attributes changed
 NOTE_LINK	=$0010;	        // link count changed
 NOTE_RENAME	=$0020;	        // vnode was renamed
 NOTE_REVOKE	=$0040;	        // vnode access was revoked

 SCE_KERNEL_EVFILT_TIMER     =EVFILT_TIMER        ;
 SCE_KERNEL_EVFILT_READ      =EVFILT_READ         ;
 SCE_KERNEL_EVFILT_WRITE     =EVFILT_WRITE        ;
 SCE_KERNEL_EVFILT_USER      =EVFILT_USER         ;
 SCE_KERNEL_EVFILT_FILE      =EVFILT_VNODE        ;
 SCE_KERNEL_EVFILT_GNM       =EVFILT_GRAPHICS_CORE;
 SCE_KERNEL_EVFILT_VIDEO_OUT =EVFILT_DISPLAY      ;
 SCE_KERNEL_EVFILT_HRTIMER   =EVFILT_HRTIMER      ;

 SCE_KERNEL_EVNOTE_DELETE    =NOTE_DELETE         ;
 SCE_KERNEL_EVNOTE_WRITE     =NOTE_WRITE          ;
 SCE_KERNEL_EVNOTE_EXTEND    =NOTE_EXTEND         ;
 SCE_KERNEL_EVNOTE_ATTRIB    =NOTE_ATTRIB         ;
 SCE_KERNEL_EVNOTE_RENAME    =NOTE_RENAME         ;
 SCE_KERNEL_EVNOTE_REVOKE    =NOTE_REVOKE         ;

 SCE_KERNEL_EVFLAG_EOF       =EV_EOF              ;
 SCE_KERNEL_EVFLAG_ERROR     =EV_ERROR            ;

 SCE_VIDEO_OUT_EVENT_FLIP            =0; //Flip completion event
 SCE_VIDEO_OUT_EVENT_VBLANK          =1; //Vblank event
 SCE_VIDEO_OUT_EVENT_PRE_VBLANK_START=2; //PreVblankStart event

type
 PSceKernelEvent=^SceKernelEvent;
 SceKernelEvent=packed record
  ident:PtrUint;   // identifier for this event
  filter:SmallInt; // filter for event
  flags:Word;      // action flags for kqueue
  fflags:DWORD;    // filter flag value
  data:Ptrint;     // filter data value
  udata:Pointer;   // opaque user data identifier
 end;

type
 PSceKernelEqueue=^SceKernelEqueue;
 SceKernelEqueue=^SceKernelEqueue_t;
 SceKernelEqueue_t=record
  valid:DWORD;
  FRefs:DWORD;
  hIOCP:Thandle;
  name:array[0..31] of AnsiChar;
 end;

 PKEventNode=^TKEventNode;
 TKEventNode=object
  refs:DWORD;
  lock:DWORD;
  eq:SceKernelEqueue;
  ev:SceKernelEvent;
 end;

function ps4_sceKernelCreateEqueue(outEq:PSceKernelEqueue;name:PChar):Integer; SysV_ABI_CDecl;
function ps4_sceKernelWaitEqueue(
          eq:SceKernelEqueue;
          ev:PSceKernelEvent;
          num:Integer;
          out_num:PInteger;
          timo:PDWORD):Integer; SysV_ABI_CDecl;
function ps4_sceKernelGetEventUserData(ev:PSceKernelEvent):Pointer; SysV_ABI_CDecl;

type
 TKFetchEvent=function(node:PKEventNode;ev:PSceKernelEvent):Boolean;
 TKAfterEvent=function(node:PKEventNode;data:Pointer):Boolean;

function  _acqure_equeue(eq:SceKernelEqueue):SceKernelEqueue;
procedure _release_equeue(eq:SceKernelEqueue);
function  _post_event(eq:SceKernelEqueue;node:Pointer;cb:TKFetchEvent):Boolean;

function  _alloc_kevent_node(eq:SceKernelEqueue;size:qword):Pointer;
procedure _free_kevent_node(node:PKEventNode);
function  _get_kevent_node(node:PKEventNode;ev:PSceKernelEvent):Boolean;
function  _trigger_kevent_node(node:PKEventNode;after:TKAfterEvent;data:Pointer):Boolean;

implementation

uses
 spinlock,
 ps4_time,
 ps4_libkernel;

const
 LIFE_EQ=$BAB1F00D;
 DEAD_EQ=$DEADBEEF;

function _alloc_kevent_node(eq:SceKernelEqueue;size:qword):Pointer;
begin
 eq:=_acqure_equeue(eq);
 if (eq=nil) then Exit(Pointer(1));
 Result:=AllocMem(size);
 if (Result=nil) then
 begin
  _release_equeue(eq);
  Exit;
 end;
 PKEventNode(Result)^.eq  :=eq;
 PKEventNode(Result)^.refs:=1;
end;

procedure _free_kevent_node(node:PKEventNode);
begin
 if (node=nil) then Exit;
 _release_equeue(System.InterlockedExchange(node^.eq,nil));
 if System.InterlockedDecrement(node^.refs)=0 then
 begin
  FreeMem(node);
 end;
end;

function _get_kevent_node(node:PKEventNode;ev:PSceKernelEvent):Boolean;
var
 tmp:SceKernelEvent;
begin
 Result:=false;
 if (node=nil) or (ev=nil) then Exit;
 tmp:=node^.ev;
 spin_unlock(node^.lock);
 if System.InterlockedDecrement(node^.refs)=0 then
 begin
  FreeMem(node);
  Exit;
 end;
 ev^:=tmp;
 Result:=True;
end;

function _trigger_kevent_node(node:PKEventNode;after:TKAfterEvent;data:Pointer):Boolean;
var
 eq:SceKernelEqueue;
begin
 Result:=False;
 if (node=nil) then Exit;
 eq:=node^.eq;
 if (eq=nil) or (eq^.valid<>LIFE_EQ) then Exit;
 if spin_trylock(node^.lock) then
 begin
  System.InterlockedIncrement(node^.refs);
  if (after<>nil) then after(node,data);
  Result:=_post_event(eq,node,@_get_kevent_node);
  if not Result then
  begin
   spin_unlock(node^.lock);
   if System.InterlockedDecrement(node^.refs)=0 then
   begin
    FreeMem(node);
    Exit;
   end;
  end;
 end;
end;

function ps4_sceKernelCreateEqueue(outEq:PSceKernelEqueue;name:PChar):Integer; SysV_ABI_CDecl;
var
 hIOCP:Thandle;
 data:SceKernelEqueue;
begin
 Writeln('sceKernelCreateEqueue:',name);
 if (outEq=nil) then Exit(SCE_KERNEL_ERROR_EINVAL);

 data:=AllocMem(SizeOf(SceKernelEqueue_t));
 if (data=nil) then
 begin
  Exit(SCE_KERNEL_ERROR_ENOMEM);
 end;
 hIOCP:=CreateIoCompletionPort(INVALID_HANDLE_VALUE,0,0,High(Integer));
 if (hIOCP=0) then
 begin
  Exit(SCE_KERNEL_ERROR_EMFILE);
 end;

 data^.valid:=LIFE_EQ;
 data^.FRefs:=1;
 data^.hIOCP:=hIOCP;
 if (name<>nil) then MoveChar0(name^,data^.name,32);

 outEq^:=data;
 Result:=0;
end;

function _post_event(eq:SceKernelEqueue;node:Pointer;cb:TKFetchEvent):Boolean;
begin
 Result:=False;
 if (eq=nil) then Exit;
 if (eq^.valid<>LIFE_EQ) then Exit;
 Result:=PostQueuedCompletionStatus(eq^.hIOCP,1,ULONG_PTR(cb),node);
end;

function  _acqure_equeue(eq:SceKernelEqueue):SceKernelEqueue;
begin
 Result:=nil;
 if (eq=nil) then Exit;
 if (eq^.valid<>LIFE_EQ) then Exit;
 System.InterlockedIncrement(eq^.FRefs);
 Result:=eq;
end;

procedure _release_equeue(eq:SceKernelEqueue);
begin
 if (eq=nil) then Exit;
 if System.InterlockedDecrement(eq^.FRefs)=0 then
 begin
  FreeMem(eq);
 end;
end;

type
 TOVERLAPPED_ENTRY=record
  lpCompletionKey:ULONG_PTR;
  lpOverlapped:POverlapped;
  Internal:ULONG_PTR;
  dwNumberOfBytesTransferred:DWORD;
 end;
 POVERLAPPED_ENTRY=^TOVERLAPPED_ENTRY;

function GetQueuedCompletionStatusEx(CompletionPort:THandle;
                                     lpCompletionPortEntries:POVERLAPPED_ENTRY;
                                     ulCount:ULONG;
                                     var ulNumEntriesRemoved:ULONG;
                                     dwMilliseconds:DWORD;
                                     fAlertable:BOOL):BOOL; stdcall; external kernel32;

function ps4_sceKernelWaitEqueue(
          eq:SceKernelEqueue;
          ev:PSceKernelEvent;
          num:Integer;
          out_num:PInteger;
          timo:PDWORD):Integer; SysV_ABI_CDecl;
Var
 QTIME:DWORD;
 LTIME:DWORD;
 OE:array[0..15] of TOVERLAPPED_ENTRY;
 i,ulNum,olNum:ULONG;
 CTXProc:TKFetchEvent;
 Q:Boolean;
begin
 if (eq=nil) then Exit(SCE_KERNEL_ERROR_EBADF);
 if (eq^.valid<>LIFE_EQ) then Exit(SCE_KERNEL_ERROR_EBADF);
 if (ev=nil) then Exit(SCE_KERNEL_ERROR_EFAULT);
 if (num<1)  then Exit(SCE_KERNEL_ERROR_EINVAL);

 //Writeln('>sceKernelWaitEqueue');

 if (timo<>nil) then
 begin
  //LTIME:=dwMilliSecs(timo^);
  LTIME:=_usec2msec(timo^);
 end else
 begin
  LTIME:=INFINITE;
 end;

 if (num>16) then num:=16;

 Result:=0;
 out_num^:=0;
 FillChar(OE,SizeOf(OE),0);
 CTXProc:=nil;
 Repeat
  ulNum:=0;
  if (LTIME<>INFINITE) then QTIME:=Windows.GetTickCount;
  Q:=GetQueuedCompletionStatusEX(eq^.hIOCP,@OE,num,ulNum,LTIME,True);
  if (LTIME<>INFINITE) then
  begin
   QTIME:=Windows.GetTickCount-QTIME;
   if (QTIME>LTIME) then
    LTIME:=0
   else
    LTIME:=LTIME-QTIME;
  end;
  if not Q then
  begin
   Case GetLastError of
    ERROR_INVALID_PARAMETER,
    ERROR_INVALID_HANDLE :Exit(SCE_KERNEL_ERROR_EBADF);
    WAIT_TIMEOUT         :Exit(SCE_KERNEL_ERROR_ETIMEDOUT);
   end;
  end;
  if (ulNum<>0) then
  begin
   olNum:=0;
   For i:=0 to ulNum-1 do
   begin
    CTXProc:=TKFetchEvent(OE[i].lpCompletionKey);
    if Assigned(CTXProc) then
    begin
     if CTXProc(PKEventNode(OE[i].lpOverlapped),@ev[olNum]) then Inc(olNum);
    end;
   end;
   if (olNum<>0) then
   begin
    out_num^:=olNum;
    //Writeln('<sceKernelWaitEqueue');
    Exit(0);
   end;
  end;
 Until false;
end;

function ps4_sceKernelGetEventUserData(ev:PSceKernelEvent):Pointer; SysV_ABI_CDecl;
begin
 if (ev=nil) then Exit(nil);
 Result:=ev^.udata;
end;


end.

