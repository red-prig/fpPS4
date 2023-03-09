unit kern_osem;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 SEMA_ATTR_FIFO=$1;
 SEMA_ATTR_PRIO=$2;
 SEMA_ATTR_SHRD=$1000;

function sys_osem_create(name:PChar;attr:DWORD;initCount,maxCount:Integer):Integer;
function sys_osem_delete(key:Integer):Integer;
function sys_osem_cancel(key,setCount:Integer;pNumWait:PInteger):Integer;
function sys_osem_post(key,signalCount:Integer):Integer;
function sys_osem_trywait(key,needCount:Integer):Integer;
function sys_osem_wait(key,needCount:Integer;pTimeout:PDWORD):Integer;

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
 SEMA_ATTR_DELF=$1000;

type
 p_osem=^t_osem;
 t_osem=packed record
  desc      :t_id_desc;
  mtx       :mtx;
  cv        :t_cv;
  list      :TAILQ_HEAD;
  count     :Integer;
  attr      :DWORD;
  init_count:Integer;
  max_count :Integer;
  wait_count:Integer;
  name      :array[0..31] of Char;
 end;

 p_osem_node=^t_osem_node;
 t_osem_node=packed record
  entry :TAILQ_ENTRY;
  td    :p_kthread;
  count :Integer;
  retval:Integer;
 end;

var
 osem_table:t_id_desc_table;

function osem_alloc:p_osem; inline;
begin
 Result:=AllocMem(SizeOf(t_osem));
end;

procedure osem_free(data:pointer);
begin
 mtx_destroy(p_osem(data)^.mtx);
 FreeMem(data);
end;

function osem_init(sem:p_osem;attr:DWORD;initCount,max_count:Integer):Integer;
begin
 sem^.desc.free :=@osem_free;
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
    node:=Default(t_osem_node);
    node.td   :=td;
    node.count:=needCount;
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

//

function sys_osem_create(name:PChar;attr:DWORD;initCount,maxCount:Integer):Integer;
var
 td:p_kthread;
 _name:array[0..31] of Char;
 sem:p_osem;
 key:Integer;
begin
 Result:=EINVAL;
 td:=curkthread;

 if ((attr and $fffffefc)<>0) or ((attr and 3)=3) then Exit;

 //process shared osem not support
 if ((attr and SEMA_ATTR_SHRD)<>0) then Exit(EPERM);

 if (initCount<0) or
    (maxCount<=0) or
    (initCount>maxCount) or
    (name=nil) then Exit;

 if ((attr and 3)=0) then
 begin
  attr:=attr or 1;
 end;

 FillChar(_name,32,0);
 if (copyinstr(name,@_name,32,nil)<>0) then Exit;

 sem:=osem_alloc;
 if (sem=nil) then Exit(ENOMEM); //EAGAIN

 osem_init(sem,attr,initCount,maxCount);
 sem^.name:=_name;

 if not id_new(@osem_table,@sem^.desc,@key) then
 begin
  osem_free(sem);
  Exit(EAGAIN);
 end;
 id_release(@sem^.desc);

 td^.td_retval[0]:=key;

 Result:=0;
end;

function sys_osem_delete(key:Integer):Integer;
var
 sem:p_osem;
begin
 Result:=ESRCH;

 sem:=p_osem(id_get(@osem_table,key));
 if (sem=nil) then Exit;

 osem_delete(sem);
 id_release(@sem^.desc);

 if not id_del(@osem_table,key) then Exit;

 Result:=0;
end;

function sys_osem_cancel(key,setCount:Integer;pNumWait:PInteger):Integer;
var
 sem:p_osem;
 num:Integer;
 r:Integer;
begin
 Result:=ESRCH;
 num:=0;

 sem:=p_osem(id_get(@osem_table,key));
 if (sem=nil) then Exit;

 Result:=osem_cancel(sem,setCount,@num);
 id_release(@sem^.desc);

 if (Result=0) then
 begin
  if (pNumWait<>nil) then
  begin
   r:=copyout(@num,pNumWait,SizeOf(Integer));
   if (r<>0) then Result:=EFAULT;
  end;
 end;
end;

function sys_osem_post(key,signalCount:Integer):Integer;
var
 sem:p_osem;
begin
 Result:=EINVAL;
 if (signalCount<=0) then Exit;

 Result:=ESRCH;

 sem:=p_osem(id_get(@osem_table,key));
 if (sem=nil) then Exit;

 Result:=osem_post(sem,signalCount);
 id_release(@sem^.desc);
end;

function sys_osem_trywait(key,needCount:Integer):Integer;
var
 sem:p_osem;
begin
 Result:=EINVAL;
 if (needCount<=0) then Exit;

 Result:=ESRCH;

 sem:=p_osem(id_get(@osem_table,key));
 if (sem=nil) then Exit;

 Result:=osem_trywait(sem,needCount);
 id_release(@sem^.desc);
end;

function sys_osem_wait(key,needCount:Integer;pTimeout:PDWORD):Integer;
var
 sem:p_osem;
 timeout:PDWORD;
 time:DWORD;
 r:Integer;
begin
 Result:=EINVAL;
 if (needCount<=0) then Exit;

 time:=0;
 timeout:=nil;

 if (pTimeout<>nil) then
 begin
  Result:=copyin(pTimeout,@time,SizeOf(DWORD));
  if (Result<>0) then Exit;
  timeout:=@time;
 end;

 Result:=ESRCH;

 sem:=p_osem(id_get(@osem_table,key));
 if (sem=nil) then Exit;

 Result:=osem_wait(sem,needCount,timeout);
 id_release(@sem^.desc);

 if (pTimeout<>nil) then
 begin
  r:=copyout(@time,pTimeout,SizeOf(DWORD));
  if (r<>0) then Result:=EFAULT;
 end;
end;

initialization
 id_table_init(@osem_table,1);

finalization
 id_table_fini(@osem_table);

end.

