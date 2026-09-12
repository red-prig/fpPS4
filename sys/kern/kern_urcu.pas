unit kern_urcu;

{$mode ObjFPC}{$H+}

interface

uses
 mqueue,
 kern_hamt;

type
 t_urcu_free=procedure(P:Pointer);

procedure urcu_flush_deferred;

procedure urcu_synchronize_rcu(my_epoch:QWORD);

procedure urcu_free(node:Pointer;free:t_urcu_free);

function  urcu_hamt_alloc(size:QWORD):Pointer;
procedure urcu_hamt_free (node:Pointer);
function  urcu_hamt_msize(node:Pointer):QWORD;

const
 urcu_hamt_allocator:THAL=(
  alloc:@urcu_hamt_alloc;
  free :@urcu_hamt_free;
  msize:@urcu_hamt_msize
 );

var
 urcu_global_epoch:Int64=0;

procedure kern_urcu_init;

implementation

uses
 LFQueue,
 kern_thr,
 time,
 md_sleep,
 kern_malloc,
 kern_daemon;

type
 p_urcu_node=^t_urcu_node;
 t_urcu_node=record
  entry:LIST_ENTRY;
  //
  cnode:Pointer;
  cfree:t_urcu_free;
  //
  epoch:QWORD;
 end;

var
 rlist_lf:TIntrusiveMPSCQueue;
 rlist_bs:LIST_HEAD=(lh_first:nil);
 rcount  :Integer=0;

procedure urcu_scan(smForce:Boolean);
label
 _again;
var
 p_node:p_urcu_node;
 r_node:p_urcu_node;
 ttd:p_kthread;
 min_epoch:QWORD;
 f_list:LIST_HEAD;
begin

 _again:

 r_node:=nil;
 f_list:=Default(LIST_HEAD);

 if (smForce) then
 begin
  threads_lock;
 end else
 begin
  if not threads_trylock then Exit;
 end;

 //flush to base list
 while rlist_lf.Pop(r_node) do
 begin
  LIST_INSERT_HEAD(@rlist_bs,r_node,@r_node^.entry);
 end;

 r_node:=LIST_FIRST(@rlist_bs);
 if (r_node=nil) then
 begin
  threads_unlock;
  Exit;
 end;

 //find minimum non-zero epoch
 min_epoch:=QWORD(-1);
 ttd:=TAILQ_FIRST(get_p_threads);
 while (ttd<>nil) do
 begin
  if (ttd^.td_urcu_epoch<>0) and
     (ttd^.td_urcu_epoch<min_epoch) then
  begin
   min_epoch:=ttd^.td_urcu_epoch;
  end;
  ttd:=TAILQ_NEXT(ttd,@ttd^.td_plist);
 end;

 //collect safe nodes
 r_node:=LIST_FIRST(@rlist_bs);
 while (r_node<>nil) do
 begin
  p_node:=LIST_NEXT(r_node,@r_node^.entry);
  //
  if (r_node^.epoch<min_epoch) then
  begin
   LIST_REMOVE(r_node,@r_node^.entry);
   LIST_INSERT_HEAD(@f_list,r_node,@r_node^.entry);
  end;
  //
  r_node:=p_node;
 end;

 threads_unlock;

 //free nodes
 r_node:=LIST_FIRST(@f_list);
 while (r_node<>nil) do
 begin
  LIST_REMOVE(r_node,@r_node^.entry);
  //free element
  if (r_node^.cfree<>nil) then
  begin
   r_node^.cfree(r_node^.cnode);
  end;
  //free node
  System.InterlockedDecrement(rcount);
  free(r_node);
  //
  r_node:=LIST_FIRST(@f_list);
 end;

 if (smForce) and
     (LIST_FIRST(@rlist_bs)<>nil) then
 begin
  msleep_td(hz div 10000);
  goto _again;
 end;

end;

procedure urcu_flush_deferred;
begin
 urcu_scan(True);
end;

//

procedure urcu_synchronize_rcu(my_epoch:QWORD);
var
 td:p_kthread;
 all_clear:Boolean;
begin
 repeat
  all_clear:=True;
  threads_lock;
  td:=TAILQ_FIRST(get_p_threads);
  while (td<>nil) do
  begin
   if (td^.td_urcu_epoch<>0) and (td^.td_urcu_epoch<=my_epoch) then
   begin
    all_clear:=False;
    Break;
   end;
   td:=TAILQ_NEXT(td,@td^.td_plist);
  end;
  threads_unlock;
  if not all_clear then
  begin
   md_yield;
  end;
 until all_clear;
end;

procedure urcu_free(node:Pointer;free:t_urcu_free);
var
 defer:p_urcu_node;
begin
 if (node=nil) then Exit;

 defer:=calloc(SizeOf(t_urcu_node));
 defer^.cnode:=node;
 defer^.cfree:=free;
 defer^.epoch:=QWORD(urcu_global_epoch);

 rlist_lf.Push(defer);
 System.InterlockedIncrement(rcount);
 //
 if rcount>(4*256) then
 begin
  urcu_scan(False);
 end;
end;

//

function urcu_hamt_alloc(size:QWORD):Pointer;
begin
 Result:=default_hamt_alloc(size);
end;

procedure urcu_hamt_free(node:Pointer);
begin
 urcu_free(node,@default_hamt_free);
end;

function urcu_hamt_msize(node:Pointer):QWORD;
begin
 Result:=default_hamt_msize(node);
end;

//

var
 daemon_stub:t_daemon_node;

procedure urcu_daemon_scan; SysV_ABI_CDecl;
begin
 urcu_scan(False);
end;

procedure kern_urcu_init;
begin
 rlist_lf.Create;
 sys_daemon_add_cbs(@daemon_stub,@urcu_daemon_scan);
end;

end.


