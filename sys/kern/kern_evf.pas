unit kern_evf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 EVF_ATTR_TH_FIFO      =$01;
 EVF_ATTR_TH_PRIO      =$02;
 EVF_ATTR_SINGLE       =$10;
 EVF_ATTR_MULTI        =$20;

 EVF_WAITMODE_AND      =$01;
 EVF_WAITMODE_OR       =$02;
 EVF_WAITMODE_CLEAR_ALL=$10;
 EVF_WAITMODE_CLEAR_PAT=$20;

implementation

uses
 mqueue,
 errno,
 systm,
 time,
 kern_time,
 kern_mtx,
 kern_thr,
 kern_condvar,
 kern_id;

const
 EVF_ATTR_DELF      =$1000;

 EVF_WAITMODE_CANCEL=$100;
 EVF_WAITMODE_DELETE=$200;

type
 p_evf=^t_evf;
 t_evf=packed record
  desc      :t_id_desc;
  bitPattern:QWORD;
  mtx       :mtx;
  cv        :t_cv;
  list      :TAILQ_HEAD;
  attr      :DWORD;
  wait_count:Integer;
  name      :array[0..31] of Char;
 end;

 p_evf_node=^t_evf_node;
 t_evf_node=packed record
  entry   :TAILQ_ENTRY;
  td      :p_kthread;
  bpattern:QWORD;
  rpattern:QWORD;
  waitMode:DWORD;
 end;

function evf_alloc:p_evf; inline;
begin
 Result:=AllocMem(SizeOf(t_evf));
end;

procedure evf_free(data:pointer);
begin
 mtx_destroy(p_evf(data)^.mtx);
 FreeMem(data);
end;

function evf_init(evf:p_evf;attr:DWORD;initPattern:QWORD):Integer;
begin
 evf^.bitPattern:=initPattern;
 mtx_init(evf^.mtx);
 cv_init(@evf^.cv,'evf cv');
 TAILQ_INIT(@evf^.list);
 evf^.wait_count:=0;
 evf^.attr:=attr;
 Result:=0;
end;

procedure evf_delete(evf:p_evf);
var
 node,next:p_evf_node;
begin
 mtx_lock(evf^.mtx);

 if ((evf^.attr and EVF_ATTR_DELF)=0) then
 begin
  evf^.attr:=evf^.attr or EVF_ATTR_DELF;
  node:=TAILQ_FIRST(@evf^.list);
  while (node<>nil) do
  begin
   next:=node^.entry.tqe_next;
   if (next=nil) then
   begin
    evf^.list.tqh_last:=node^.entry.tqe_prev;
   end else
   begin
    next^.entry.tqe_prev:=node^.entry.tqe_prev;
   end;
   node^.entry.tqe_prev^:=node^.entry.tqe_next;
   node^.waitMode:=node^.waitMode or EVF_WAITMODE_DELETE;
   node:=next;
  end;
  cv_broadcastpri(@evf^.cv,0);
  while (evf^.wait_count<>0) do
  begin
   _cv_wait_sig(@evf^.cv,@evf^.mtx);
  end;
 end;

 mtx_unlock(evf^.mtx);
end;

function evf_cancel(evf:p_evf;setPattern:QWORD):Integer;
var
 node,next:p_evf_node;
begin
 mtx_lock(evf^.mtx);

 if ((evf^.attr and EVF_ATTR_DELF)=0) then
 begin
  evf^.bitPattern:=setPattern;
  Result:=evf^.wait_count;
  node:=TAILQ_FIRST(@evf^.list);
  while (node<>nil) do
  begin
   next:=node^.entry.tqe_next;
   if (next=nil) then
   begin
    evf^.list.tqh_last:=node^.entry.tqe_prev;
   end else
   begin
    next^.entry.tqe_prev:=node^.entry.tqe_prev;
   end;
   node^.entry.tqe_prev^:=node^.entry.tqe_next;
   node^.waitMode:=node^.waitMode or EVF_WAITMODE_CANCEL;
   node:=next;
  end;
  cv_broadcastpri(@evf^.cv,0);

  mtx_unlock(evf^.mtx);
 end else
 begin
  mtx_unlock(evf^.mtx);

  Result:=0;
 end;
end;

procedure evf_clear(evf:p_evf;bitPattern:QWORD);
begin
 mtx_lock(evf^.mtx);

 evf^.bitPattern:=evf^.bitPattern and bitPattern;

 mtx_unlock(evf^.mtx);
end;

procedure evf_set(evf:p_evf;bitPattern:QWORD);
label
 _LAST,
 _SIGNAL;
var
 node,next:p_evf_node;
 prev:Pointer;
 pattern  :QWORD;
 npattern :QWORD;
 nwaitMode:DWORD;
begin
 mtx_lock(evf^.mtx);

 if ((evf^.attr and EVF_ATTR_DELF)=0) then
 begin
  pattern:=bitPattern or evf^.bitPattern;
  evf^.bitPattern:=pattern;
  node:=TAILQ_FIRST(@evf^.list);
  repeat
   if (node=nil) then break;
   next:=node^.entry.tqe_next;
   npattern :=node^.bpattern;
   nwaitMode:=node^.waitMode;
   if ((nwaitMode and EVF_WAITMODE_AND)=0) then
   begin
    //PRIO
    if ((npattern and Pattern)<>0) then
    begin
     node^.rpattern:=Pattern;
     if ((nwaitMode and EVF_ATTR_SINGLE)=0) then
     begin
      if ((nwaitMode and EVF_ATTR_MULTI)<>0) then
      begin
       evf^.bitPattern:=(not npattern) and evf^.bitPattern;
      end;
     end else
     begin
      evf^.bitPattern:=0;
     end;
     prev:=node^.entry.tqe_prev;
     if (node^.entry.tqe_next=nil) then
     begin
      _LAST:
       evf^.list.tqh_last:=prev;
     end else
     begin
      p_evf_node(node^.entry.tqe_next)^.entry.tqe_prev:=prev;
     end;
     _SIGNAL:
      node^.entry.tqe_prev^:=node^.entry.tqe_next;

      cv_signalto(@evf^.cv,node^.td);
    end;
   end else
   if ((npattern and Pattern)=npattern) then
   begin
    //FIFO
    node^.rpattern:=Pattern;
    if ((nwaitMode and EVF_ATTR_SINGLE)=0) then
    begin
     if ((nwaitMode and EVF_ATTR_MULTI)<>0) then
     begin
      evf^.bitPattern:=(not npattern) and evf^.bitPattern;
     end;
    end else
    begin
     evf^.bitPattern:=0;
    end;
    prev:=node^.entry.tqe_prev;
    if (node^.entry.tqe_next=nil) then goto _LAST;
    p_evf_node(node^.entry.tqe_next)^.entry.tqe_prev:=prev;
    goto _SIGNAL;
   end;
   Pattern:=evf^.bitPattern;
   node:=next;
  until (pattern=0);
 end;

 mtx_unlock(evf^.mtx);
end;

function evf_trywait(evf:p_evf;bitPattern:QWORD;waitMode:DWORD;pRes:PQWORD):Integer;
label
 _RESULT;
var
 pattern:QWORD;
begin
 mtx_lock(evf^.mtx);

 if ((evf^.attr and EVF_ATTR_DELF)=0) then
 begin
  if (((evf^.attr and EVF_ATTR_SINGLE)=0) or (evf^.wait_count=0)) then
  begin
   pattern:=evf^.bitPattern;

   if ((waitMode and EVF_WAITMODE_AND)=0) then
   begin
    if ((pattern and bitPattern)=0) then goto _RESULT;
    if (pRes<>nil) then
    begin
     pRes^:=pattern;
    end;
    if ((waitMode and EVF_ATTR_SINGLE)=0) then
    begin
     if ((waitMode and EVF_ATTR_MULTI)<>0) then
     begin
      evf^.bitPattern:=(not bitPattern) and evf^.bitPattern;
     end;
    end else
    begin
     evf^.bitPattern:=0;
    end;
   end else
   begin
    if ((pattern and bitPattern)<>bitPattern) then
    begin
     _RESULT:
      if (pRes<>nil) then
      begin
       pRes^:=pattern;
      end;

      mtx_unlock(evf^.mtx);

      Exit(EBUSY);
    end;
    if (pRes<>nil) then
    begin
     pRes^:=pattern;
    end;
    if ((waitMode and EVF_ATTR_SINGLE)=0) then
    begin
     if ((waitMode and EVF_ATTR_MULTI)<>0) then
     begin
      evf^.bitPattern:=(not bitPattern) and evf^.bitPattern;
     end;
    end else
    begin
     evf^.bitPattern:=0;
    end;
   end;

   mtx_unlock(evf^.mtx);

   Exit(0);
  end;

  mtx_unlock(evf^.mtx);

  Result:=EPERM;
 end else
 begin
  mtx_unlock(evf^.mtx);

  Result:=EACCES;
 end;
end;

function evf_wait(evf:p_evf;bitPattern:QWORD;waitMode:DWORD;pRes:PQWORD;timeout:PDWORD):Integer;
label
 _WAIT,
 _FIFO,
 _SIGNAL;
var
 td:p_kthread;
 node:t_evf_node;
 node2,node3:p_evf_node;
 ts:timespec;
 t1,t2,t3:Int64;
 pattern:QWORD;
 attr:DWORD;
 _pri:Integer;
begin
 td:=curkthread;

 mtx_lock(evf^.mtx);

 attr:=evf^.attr;
 if ((attr and EVF_ATTR_DELF)=0) then
 begin
  if (((attr and EVF_ATTR_SINGLE)<>0) and (evf^.wait_count<>0)) then
  begin
   mtx_unlock(evf^.mtx);

   Exit(EPERM);
  end;
  pattern:=evf^.bitPattern;
  if ((waitMode and EVF_WAITMODE_AND)=0) then
  begin
   if ((pattern and bitPattern)=0) then goto _WAIT;
   if (pRes<>nil) then
   begin
    pRes^:=pattern;
   end;
   if ((waitMode and EVF_ATTR_SINGLE)=0) then
   begin
    if ((waitMode and EVF_ATTR_MULTI)<>0) then
    begin
     evf^.bitPattern:=(not bitPattern) and evf^.bitPattern;
    end;
   end else
   begin
    evf^.bitPattern:=0;
   end;

   mtx_unlock(evf^.mtx);

   Exit(0);
  end else
  begin
   if ((pattern and bitPattern)<>bitPattern) then
   begin
    _WAIT:

    node:=Default(t_evf_node);
    node.td:=td;
    node.bpattern:=bitPattern;
    node.waitMode:=waitMode;

    if ((attr and EVF_ATTR_TH_FIFO)=0) then
    begin
     //PRIO
     node.entry.tqe_next:=TAILQ_FIRST(@evf^.list);
     repeat
      node.entry.tqe_next:=p_evf_node(node.entry.tqe_next)^.entry.tqe_next;
      if (node.entry.tqe_next=nil) then goto _FIFO;
      _pri:=p_evf_node(node.entry.tqe_next)^.td^.td_base_pri;
     until not ((_pri<td^.td_base_pri) or (_pri=td^.td_base_pri));

     // INSERT BETWEN
     node.entry.tqe_prev:=p_evf_node(node.entry.tqe_next)^.entry.tqe_prev;
     p_evf_node(node.entry.tqe_next)^.entry.tqe_prev^:=@node;
     p_evf_node(node.entry.tqe_next)^.entry.tqe_prev:=@node.entry.tqe_next;
    end else
    begin
     _FIFO:
     // INSERT LAST
     node.entry.tqe_next:=nil;
     node.entry.tqe_prev:=evf^.list.tqh_last;
     node.entry.tqe_prev^:=@node;
     evf^.list.tqh_last:=@node.entry.tqe_next;
    end;
    evf^.wait_count:=evf^.wait_count+1;

    if (timeout=nil) then
    begin
     Result:=_cv_wait_sig(@evf^.cv,@evf^.mtx);
    end else
    begin
     usec2timespec(@ts,timeout^);
     t1:=get_unit_uptime;
     Result:=_cv_timedwait_sig_proctime(@evf^.cv,@evf^.mtx,@ts);
     t2:=get_unit_uptime;
     t3:=cputick2usec(t2-t1);
     if (t3<timeout^) then
     begin
      timeout^:=timeout^-DWORD(t3);
     end else
     begin
      timeout^:=0;
     end;
    end;
    if (Result<>0) then
    begin
     node2:=evf^.list.tqh_first;
     repeat
      node3:=node2;
      if (node3=nil) then
      begin
       Result:=0;
       goto _SIGNAL;
      end;
      node2:=node3^.entry.tqe_next;
     until (node3=@node);
     if (node2=nil) then
     begin
      evf^.list.tqh_last:=node.entry.tqe_prev;
     end else
     begin
      node2^.entry.tqe_prev:=node.entry.tqe_prev;
     end;
     node.entry.tqe_prev^:=node3^.entry.tqe_next;
     if (Result=EWOULDBLOCK) then
     begin
      timeout^:=0;
      Result:=ETIMEDOUT;
     end;
     node.rpattern:=evf^.bitPattern;
    end;
    _SIGNAL:

    Result:=evf^.wait_count-1;
    evf^.wait_count:=Result;

    if ((evf^.attr and EVF_ATTR_DELF)<>0) and (Result=0) then
    begin
     cv_signal(@evf^.cv);
    end;

    mtx_unlock(evf^.mtx);

    if (pRes<>nil) then
    begin
     pRes^:=node.rpattern;
    end;

    if ((node.waitMode and EVF_WAITMODE_CANCEL)=0) then
    begin
     if ((node.waitMode and EVF_WAITMODE_DELETE)<>0) then
     begin
      Result:=EACCES;
     end;
    end else
    begin
     Result:=ECANCELED;
    end;

    Exit;
   end;
   if (pRes<>nil) then
   begin
    pRes^:=pattern;
   end;
   if ((waitMode and EVF_ATTR_SINGLE)=0) then
   begin
    if ((waitMode and EVF_ATTR_MULTI)<>0) then
    begin
     evf^.bitPattern:=(not bitPattern) and evf^.bitPattern;
    end;
   end else
   begin
    evf^.bitPattern:=0;
   end;

   mtx_unlock(evf^.mtx);

   Exit(0);
  end;
 end else
 begin
  mtx_unlock(evf^.mtx);

  Exit(EACCES);
 end;

end;




end.

