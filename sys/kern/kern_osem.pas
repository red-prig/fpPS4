unit kern_osem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_mtx,
 kern_thr,
 kern_condvar;

const
 SEMA_ATTR_FIFO=$1;
 SEMA_ATTR_PRIO=$2;
 SEMA_ATTR_DELF=$1000;

type
 p_osem=^t_osem;
 t_osem=packed record
  mtx       :mtx;
  cv        :t_cv;
  list      :TAILQ_HEAD;
  count     :Integer;
  attr      :DWORD;
  init_count:Integer;
  max_count :Integer;
  wait_count:Integer;
 end;

 p_osem_node=^t_osem_node;
 t_osem_node=packed record
  entry :TAILQ_ENTRY;
  td    :p_kthread;
  count :Integer;
  retval:Integer;
 end;

implementation

uses
 errno,
 time,
 kern_time;

function osem_init(sem:p_osem;attr:DWORD;initCount,max_count:Integer):Integer;
begin
 sem^.count     :=initCount;
 sem^.init_count:=initCount;
 sem^.max_count :=max_count;
 mtx_init(sem^.mtx);
 cv_init(@sem^.cv,'osem cv');
 TAILQ_INIT(@sem^.list);
 sem^.wait_count:=0;
 sem^.attr:=attr;
 Result:=0;
end;

procedure osem_delete(sem:p_osem);
var
 node,next:p_osem_node;
begin
 mtx_lock(sem^.mtx);
 if ((sem^.attr and SEMA_ATTR_DELF)=0) then
 begin
  sem^.attr:=sem^.attr or SEMA_ATTR_DELF;

  node:=TAILQ_FIRST(@sem^.list);
  if (node<>nil) then
  begin
   repeat
    next:=node^.entry.tqe_next;
    if (next=nil) then break;
    next^.entry.tqe_prev :=node^.entry.tqe_prev;
    node^.entry.tqe_prev^:=node^.entry.tqe_next;
    node^.retval:=EACCES;
    node:=next;
   until false;
   sem^.list.tqh_last:=node^.entry.tqe_prev;
   node^.entry.tqe_prev^:=node^.entry.tqe_next;
   node^.retval:=EACCES;
  end;
  cv_broadcastpri(@sem^.cv,0);
  while (sem^.wait_count<>0) do
  begin
   _cv_wait_sig(@sem^.cv,@sem^.mtx);
  end;
 end;
 mtx_unlock(sem^.mtx);
end;

function osem_cancel(sem:p_osem;setCount:Integer;pNumWait:PInteger):Integer;
var
 node,next:p_osem_node;
begin
 Result:=0;
 mtx_lock(sem^.mtx);
 if ((sem^.attr and SEMA_ATTR_DELF)=0) then
 begin
  if (sem^.max_count<setCount) then
  begin
   mtx_unlock(sem^.mtx);
   Exit(EINVAL);
  end else
  begin
   if (setCount<0) then
   begin
    sem^.count:=sem^.init_count;
   end else
   begin
    sem^.count:=setCount;
   end;
   pNumWait^:=sem^.wait_count;

   node:=TAILQ_FIRST(@sem^.list);
   if (node<>nil) then
   begin
    repeat
     next:=node^.entry.tqe_next;
     if (next=nil) then break;
     next^.entry.tqe_prev :=node^.entry.tqe_prev;
     node^.entry.tqe_prev^:=node^.entry.tqe_next;
     node^.retval:=ECANCELED;
     node:=next;
    until false;
    sem^.list.tqh_last:=node^.entry.tqe_prev;
    node^.entry.tqe_prev^:=node^.entry.tqe_next;
    node^.retval:=ECANCELED;
   end;

   cv_broadcastpri(@sem^.cv,0);

   mtx_unlock(sem^.mtx);
  end;
 end else
 begin
  mtx_unlock(sem^.mtx);
  pNumWait^:=0;
 end;
end;

function osem_post(sem:p_osem;signalCount:Integer):Integer;
var
 node,next:p_osem_node;
 count:Integer;
begin
 Result:=0;
 mtx_lock(sem^.mtx);

 if ((sem^.attr and SEMA_ATTR_DELF)=0) then
 begin
  count:=sem^.count+signalCount;
  if (sem^.max_count<count) then
  begin
   mtx_unlock(sem^.mtx);
   Exit(EINVAL);
  end;
  sem^.count:=count;
  if (signalCount<1) then
  begin
   //0
  end else
  begin
   node:=TAILQ_FIRST(@sem^.list);
   repeat
    if ((node=nil) or (count<node^.count)) then break;
    next:=node^.entry.tqe_next;
    sem^.count:=count-node^.count;
    if (node^.entry.tqe_next=nil) then
    begin
     sem^.list.tqh_last:=node^.entry.tqe_prev;
    end else
    begin
     p_osem_node(node^.entry.tqe_next)^.entry.tqe_prev:=node^.entry.tqe_prev;
    end;
    node^.entry.tqe_prev^:=node^.entry.tqe_next;

    cv_signalto(@sem^.cv,node^.td);

    count:=sem^.count;
    node:=next;
   until (count<=0);
  end;
 end;

 mtx_unlock(sem^.mtx);
end;

function osem_trywait(sem:p_osem;needCount:Integer):Integer;
var
 max_count:Integer;
begin
 Result:=0;
 mtx_lock(sem^.mtx);

 if (sem^.count<needCount) then
 begin
  max_count:=sem^.max_count;

  mtx_unlock(sem^.mtx);

  if (max_count<needCount) then
  begin
   Result:=EINVAL;
  end else
  begin
   Result:=EBUSY;
  end;
 end else
 begin
  sem^.count:=sem^.count-needCount;

  mtx_unlock(sem^.mtx);
 end;
end;

function osem_wait(sem:p_osem;needCount:Integer;timeout:PDWORD):Integer;
label
 _FIFO,_LAB;
var
 td:p_kthread;
 node:t_osem_node;
 node2,node3:p_osem_node;
 ts:timespec;
 t1,t2,t3:Int64;
 _pri:Integer;
 count:Integer;
begin
 Result:=0;
 td:=curkthread;
 mtx_lock(sem^.mtx);

 if ((sem^.attr and SEMA_ATTR_DELF)=0) then
 begin
  if (sem^.count<needCount) then
  begin
   if (needCount<=sem^.max_count) then
   begin
    node.retval:=0;
    if ((sem^.attr and SEMA_ATTR_FIFO)=0) then
    begin
     //_PRIO
     node.entry.tqe_next:=@sem^.list.tqh_first;
     repeat
      node.entry.tqe_next:=p_osem_node(node.entry.tqe_next)^.entry.tqe_next;
      if (node.entry.tqe_next=nil) then goto _FIFO;
      _pri:=p_osem_node(node.entry.tqe_next)^.td^.td_base_pri;
     until not ((_pri<td^.td_base_pri) or (_pri=td^.td_base_pri));

     // INSERT BETWEN
     node.entry.tqe_prev:=p_osem_node(node.entry.tqe_next)^.entry.tqe_prev;
     p_osem_node(node.entry.tqe_next)^.entry.tqe_prev^:=@node;
     p_osem_node(node.entry.tqe_next)^.entry.tqe_prev:=@node.entry.tqe_next;
    end else
    begin
     _FIFO:
     // INSERT LAST
     node.entry.tqe_next:=nil;
     node.entry.tqe_prev:=sem^.list.tqh_last;
     node.entry.tqe_prev^:=@node;
     sem^.list.tqh_last:=@node.entry.tqe_next;
    end;
    sem^.wait_count:=sem^.wait_count+1;
    node.td:=td;
    node.count:=needCount;
    if (timeout=nil) then
    begin
     Result:=_cv_wait_sig(@sem^.cv,@sem^.mtx);
    end else
    begin
     usec2timespec(@ts,timeout^);
     t1:=get_unit_uptime;
     Result:=_cv_timedwait_sig_proctime(@sem^.cv,@sem^.mtx,@ts);
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
    if (Result=0) then
    begin
     //
    end else
    begin
     node2:=sem^.list.tqh_first;
     repeat
      node3:=node2;
      if (node3=nil) then
      begin
       Result:=0;
       goto _LAB;
      end;
      node2:=node3^.entry.tqe_next;
     until (node3=@node);
     if (node2=nil) then
     begin
      sem^.list.tqh_last:=node.entry.tqe_prev;
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
    end;

    _LAB:
    count:=sem^.wait_count-1;
    sem^.wait_count:=count;
    if ((sem^.attr and SEMA_ATTR_DELF)<>0) then
    begin
     Result:=EACCES;
     if (count=0) then
     begin
      cv_signal(@sem^.cv);
     end;
    end;

    mtx_unlock(sem^.mtx);

    if (Result=0) then
    begin
     Result:=node.retval;
    end;
    Exit;
   end;

   mtx_unlock(sem^.mtx);
   Exit(EINVAL);
  end;

  sem^.count:=sem^.count-needCount;

  mtx_unlock(sem^.mtx);
  Exit(0);
 end else
 begin
  mtx_unlock(sem^.mtx);
  Exit(EACCES);
 end;
end;



end.

