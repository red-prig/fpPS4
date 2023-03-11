unit rtprio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 PRI_ITHD     =1; // Interrupt thread.
 PRI_REALTIME =2; // Real time process.
 PRI_TIMESHARE=3; // Time sharing process.
 PRI_IDLE     =4; // Idle process.
 PRI_FIFO_BIT =8;
 PRI_FIFO     =10;

 PRI_MIN=0;
 PRI_MAX=960;

 PRI_MIN_TIMESHARE=256;
 PRI_MAX_TIMESHARE=767;

 PUSER=PRI_MIN_TIMESHARE;

 RTP_PRIO_REALTIME =PRI_REALTIME;
 RTP_PRIO_NORMAL   =PRI_TIMESHARE;
 RTP_PRIO_IDLE     =PRI_IDLE;
 RTP_PRIO_FIFO     =PRI_FIFO;

 RTP_PRIO_MIN=PRI_MIN;
 RTP_PRIO_MAX=PRI_MAX;

 RTP_LOOKUP=0;
 RTP_SET   =1;

type
 p_rtprio=^t_rtprio;
 t_rtprio=packed record
  _type:Word;
  _prio:Word;
 end;

function  PRI_BASE(P:Word):Word; inline;
function  RTP_PRIO_BASE(P:Word):Word; inline;

function  rtprio_thread(func,tid:Integer;rtp:p_rtprio):Integer;
function  _rtprio(func,pid:Integer;rtp:p_rtprio):Integer;

implementation

uses
 trap,
 thr_error,
 kern_rtprio;

function PRI_BASE(P:Word):Word; inline;
begin
 Result:=P and (not PRI_FIFO_BIT);
end;

function RTP_PRIO_BASE(P:Word):Word; inline;
begin
 Result:=P and (not PRI_FIFO_BIT);
end;

function rtprio_thread(func,tid:Integer;rtp:p_rtprio):Integer; assembler; nostackframe;
asm
 movq  sys_rtprio_thread,%rax
 call  fast_syscall
 jmp   cerror
end;

function _rtprio(func,pid:Integer;rtp:p_rtprio):Integer; assembler; nostackframe;
asm
 movq  sys_rtprio,%rax
 call  fast_syscall
 jmp   cerror
end;

end.

