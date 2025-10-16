unit dev_sce_zlib;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam,
 sys_conf,
 sys_event;

Function zlib_modevent(_mod,_type:Integer):Integer;

implementation

uses
 paszlib,
 errno,
 mqueue,
 kern_mtx,
 kern_condvar,
 vm_map,
 vm_object,
 kern_proc,
 kern_thr,
 subr_backtrace;

type
 p_zlib_node=^t_zlib_node;
 t_zlib_node=record
  entry    :TAILQ_ENTRY;
  requestId:QWORD;
  src      :Pointer;
  dst      :Pointer;
  src_len  :DWORD;
  dst_len  :DWORD;
  status   :Integer;
 end;

 p_zlib_context=^t_zlib_context;
 t_zlib_context=record
  td:p_kthread;

  free__list:TAILQ_HEAD;
  queue_list:TAILQ_HEAD;
  stash_list:TAILQ_HEAD;

  queue_cv:t_cv;

  nodes:array[0..63] of t_zlib_node;

  mark_free  :Byte;
  free__count:Byte;
  queue_count:Byte;
  stash_count:Byte;
 end;

var
 zlib_init   :Integer=0;
 zlib_dev    :p_cdev=nil;
 zlib_sc_mtx :mtx;

 zlib_knl_mtx:mtx;
 zlib_knlist :t_knlist;

procedure zlib_proc(context:p_zlib_context); forward; SysV_ABI_CDecl;

Function zlib_open(dev:p_cdev;oflags,devtype:Integer):Integer;
var
 context:p_zlib_context;
 i:Integer;
begin
 mtx_lock(zlib_sc_mtx);

 if (zlib_init=0) then
 begin
  //max queue = 64

  context:=AllocMem(SizeOf(t_zlib_context));

  with context^ do
  begin
   TAILQ_INIT(@free__list);
   TAILQ_INIT(@queue_list);
   TAILQ_INIT(@stash_list);

   cv_init(@queue_cv,'zlib_queue_cv');

   for i:=0 to High(nodes) do
   begin
    TAILQ_INSERT_TAIL(@free__list,@nodes[i],@nodes[i].entry);
   end;

   free__count:=Length(nodes);
  end;

  Result:=kthread_add(@zlib_proc,context,@context^.td,(64*1024) div (16*1024),'[ZLIB]');
  if (Result=0) then
  begin
   dev^.si_drv1:=context;
   zlib_init   :=1;
  end else
  begin
   FreeMem(context);
  end;

 end else
 begin
  Result:=EBUSY;
 end;

 mtx_unlock(zlib_sc_mtx);
end;

Function zlib_close(dev:p_cdev;fflag,devtype:Integer):Integer;
var
 context:p_zlib_context;
begin
 mtx_lock(zlib_sc_mtx);

 if (zlib_init<>0) then
 begin
  //flush queue

  context:=dev^.si_drv1;

  with context^ do
  begin
   mark_free:=1;         //mark free
   cv_signal(@queue_cv); //wakeup
  end;
  //free in thread

  zlib_init:=0;
 end;

 mtx_unlock(zlib_sc_mtx);

 Result:=0;
end;

type
 p_data_zlib_init=^t_data_zlib_init;
 t_data_zlib_init=packed record
  buffer:Pointer;
  length:DWORD;
  _align:Integer;
  unused:Integer;
 end;

 p_data_zlib_inflate=^t_data_zlib_inflate;
 t_data_zlib_inflate=packed record
          requestId:QWORD;
                cmd:Integer;
            _align1:Integer;
             source:Pointer;
        destination:Pointer;
       sourceLength:Integer;
  destinationLength:Integer;
             status:Integer;
            _align2:Integer;
 end;

Function zlib_ioctl_init(data:p_data_zlib_init):Integer;
var
 buffer_lo,buffer_hi:QWORD;
 map  :vm_map_t;
 entry:vm_map_entry_t;
begin
 if (data^.length=0) or
    (data^.buffer=nil) then Exit(EINVAL);

 buffer_lo:=QWORD(data^.buffer) and QWORD(not PAGE_MASK);
 buffer_hi:=(QWORD(data^.buffer) + data^.length + PAGE_MASK) and QWORD(not PAGE_MASK);

 map:=p_proc.p_vmspace;

 if (buffer_hi < buffer_lo) or
    (buffer_lo < map^.header.start) or
    (map^.header.__end < buffer_hi) then Exit(EACCES);

 if (buffer_hi <= buffer_lo) then Exit(EINVAL);

 vm_map_lock(map,False);

 if not vm_map_lookup_entry(map,buffer_lo,@entry) then
 begin
  vm_map_unlock(map);
  Exit(EACCES);
 end;

 vm_map_unlock(map);

 Result:=0;
end;

Function zlib_ioctl_fini(data:p_data_zlib_init):Integer;
begin
 Result:=0;
end;

var
 id_gen:QWORD=0;

function gen_requestId:QWORD; inline;
var
 tmp:QWORD;
begin
 tmp:=(id_gen + 1) div ($ffffffffff);
 id_gen:=id_gen + 1 + (tmp - (tmp << 40));
 Result:=id_gen * $10000;
end;

function zlib_alloc_node(context:p_zlib_context):p_zlib_node; inline;
begin
 with context^ do
 begin
  Result:=TAILQ_FIRST(@free__list);
  if (Result<>nil) then
  begin
   Dec(free__count);
   TAILQ_REMOVE(@free__list,Result,@Result^.entry);
  end;
 end;
end;

procedure zlib_free_node(context:p_zlib_context;node:p_zlib_node); inline;
begin
 with context^ do
 begin
  Inc(free__count);
  TAILQ_INSERT_HEAD(@free__list,node,@node^.entry);
 end;
end;

procedure zlib_restore_node(context:p_zlib_context;node:p_zlib_node); inline;
begin
 with context^ do
 begin
  Dec(stash_count);
  Inc(free__count);
  TAILQ_REMOVE     (@stash_list,node,@node^.entry);
  TAILQ_INSERT_TAIL(@free__list,node,@node^.entry);
 end;
end;

procedure zlib_stash_node(context:p_zlib_context;node:p_zlib_node); inline;
begin
 with context^ do
 begin
  Inc(stash_count);
  TAILQ_INSERT_HEAD(@stash_list,node,@node^.entry);
 end;
end;

procedure zlib_send_node(context:p_zlib_context;node:p_zlib_node); inline;
begin
 with context^ do
 begin
  Inc(queue_count);
  TAILQ_INSERT_TAIL(@queue_list,node,@node^.entry);
  cv_signal(@queue_cv);
 end;
end;

function zlib_recv_node(context:p_zlib_context):p_zlib_node; inline;
begin
 with context^ do
 begin
  Result:=TAILQ_LAST(@queue_list);
  if (Result<>nil) then
  begin
   Dec(queue_count);
   TAILQ_REMOVE(@queue_list,Result,@Result^.entry);
  end;
 end;
end;

procedure zlib_proc(context:p_zlib_context); SysV_ABI_CDecl;
var
 node:p_zlib_node;
begin
 repeat

  mtx_lock(zlib_sc_mtx);

   repeat

    node:=zlib_recv_node(context);
    if (node<>nil) then Break;

    _cv_wait_sig(@context^.queue_cv,@zlib_sc_mtx);

   until (context^.mark_free<>0);

  mtx_unlock(zlib_sc_mtx);

  if (node<>nil) then
  begin

   with node^ do
   begin
    status:=uncompress(dst,dst_len,src,src_len);
    //TODO: error codes?
   end;

   mtx_lock(zlib_sc_mtx);
    zlib_stash_node(context,node);
   mtx_unlock(zlib_sc_mtx);

   knote(@zlib_knlist,0,0);
  end;

 until (context^.mark_free<>0);

 mtx_lock(zlib_sc_mtx);

  with context^ do
  begin
   thread_dec_ref(td);
   cv_destroy(@queue_cv);
  end;
  FreeMem(context);

 mtx_unlock(zlib_sc_mtx);
end;

Function zlib_buffer_test(buffer:Pointer;length:DWORD):Integer;
label
 _EACCES;
var
 buffer_lo,buffer_hi:QWORD;
 map  :vm_map_t;
 entry:vm_map_entry_t;
 obj  :vm_map_object;

 offset_next:QWORD;
 offset     :QWORD;
 addr       :QWORD;
 addr_next  :QWORD;
begin
 if (buffer=nil) or
    (length=0) then Exit(EINVAL);

 buffer_lo:=QWORD(buffer) and QWORD(not PAGE_MASK);
 buffer_hi:=(QWORD(buffer) + length + PAGE_MASK) and QWORD(not PAGE_MASK);

 map:=p_proc.p_vmspace;

 if (buffer_hi < buffer_lo) or
    (buffer_lo < map^.header.start) or
    (map^.header.__end < buffer_hi) then Exit(EINVAL);

 if (buffer_hi <= buffer_lo) then Exit(EINVAL);

 vm_map_lock(map,False);

 offset_next:=QWORD(-1);
 offset     :=0;
 addr       :=buffer_lo;

 repeat

  if not vm_map_lookup_entry(map,addr,@entry) then
  begin
   _EACCES:
   vm_map_unlock(map);
   Exit(EACCES);
  end;

  obj:=entry^.vm_obj;
  if (obj=nil) then
  begin
   goto _EACCES;
  end;

  if ((obj^.flags and OBJ_DMEM_EXT)=0) then
  begin
   goto _EACCES;
  end;

  if (offset <> 0) and
     ((entry^.offset - entry^.start + addr) <> (offset_next + $4000)) then
  begin
   goto _EACCES;
  end;

  addr_next  :=addr + PAGE_SIZE;
  offset     :=offset - PAGE_SIZE;
  offset_next:=addr - entry^.start + entry^.offset;
  addr       :=addr_next;

 until (addr_next >= buffer_hi);

 vm_map_unlock(map);

 Result:=0;
end;

Function zlib_ioctl_inflate(dev:p_cdev;data:p_data_zlib_inflate):Integer;
label
 _unlock;
var
 context:p_zlib_context;
 node:p_zlib_node;
 requestId:QWORD;
begin
 Result:=0;

 if (data^.cmd <> 1) then Exit(EINVAL);

 context:=dev^.si_drv1;

 mtx_lock(zlib_knl_mtx);

  node:=zlib_alloc_node(context);

  if (node=nil) then
  begin
   Result:=EBUSY;
   goto _unlock;
  end;

  Result:=zlib_buffer_test(data^.source,data^.sourceLength);
  if (Result<>0) then
  begin
   zlib_free_node(context,node);
   goto _unlock;
  end;

  Result:=zlib_buffer_test(data^.destination,data^.destinationLength);
  if (Result<>0) then
  begin
   zlib_free_node(context,node);
   goto _unlock;
  end;

  requestId:=gen_requestId;

  node^.requestId:=requestId;
  node^.src      :=data^.source;
  node^.dst      :=data^.destination;
  node^.src_len  :=data^.sourceLength;
  node^.dst_len  :=data^.destinationLength;
  node^.status   :=0;

  data^.requestId:=requestId;

  zlib_send_node(context,node);

 _unlock:
 mtx_unlock(zlib_knl_mtx);
end;

Function zlib_ioctl_get_result(dev:p_cdev;data:p_data_zlib_inflate):Integer;
var
 context:p_zlib_context;
 node:p_zlib_node;
begin
 Result:=EINVAL;

 context:=dev^.si_drv1;

 mtx_lock(zlib_knl_mtx);

 node:=TAILQ_FIRST(@context^.stash_list);

 while (node<>nil) do
 begin
  //
  if (node^.requestId=data^.requestId) then
  begin
   Result:=0;
   data^.destinationLength:=node^.dst_len;
   data^.status           :=node^.status;

   zlib_restore_node(context,node);

   Break;
  end;
  //
  node:=TAILQ_NEXT(node,@node^.entry);
 end;

 mtx_unlock(zlib_knl_mtx);
end;

Function zlib_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('zlib_ioctl:0x',HexStr(cmd,8));

 case cmd of

  $c0185a03: //sceZlibInitialize
   begin
    Result:=zlib_ioctl_init(data);
   end;
  $80185a03: //sceZlibFinalize
   begin
    Result:=zlib_ioctl_fini(data);
   end;
  $c0305a01: //sceZlibInflate
   begin
    Result:=zlib_ioctl_inflate(dev,data);
   end;
  $c0305a02: //sceZlibGetResult
   begin
    Result:=zlib_ioctl_get_result(dev,data);
   end;

  else
   begin
    print_error_td('zlib_ioctl(0x'+HexStr(cmd,8)+')');
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

procedure zlib_detach(kn:p_knote);
begin
 mtx_lock(zlib_knl_mtx);

 knlist_remove(@zlib_knlist,kn,1);

 mtx_unlock(zlib_knl_mtx);
end;

function zlib_event(kn:p_knote;hint:QWORD):Integer;
var
 context:p_zlib_context;
 node:p_zlib_node;
begin
 node:=nil;

 kn^.kn_kevent.data:=0;

 context:=kn^.kn_hook;

 mtx_lock(zlib_sc_mtx);

 with context^ do
 begin
  node:=TAILQ_FIRST(@stash_list);
 end;

 if (node<>nil) then
 begin
  kn^.kn_kevent.data:=node^.requestId;
 end;

 mtx_unlock(zlib_sc_mtx);

 Result:=ord(node<>nil);
end;

const
 zlib_filterops:t_filterops=(
  f_isfd  :1;
  f_detach:@zlib_detach;
  f_event :@zlib_event;
 );

Function zlib_kqfilter(dev:p_cdev;kn:p_knote):Integer;
begin

 if (kn^.kn_kevent.filter = EVFILT_READ) then
 begin

  mtx_lock(zlib_knl_mtx);

  kn^.kn_fop :=@zlib_filterops;
  kn^.kn_hook:=dev^.si_drv1;

  knlist_add(@zlib_knlist,kn,1);

  mtx_unlock(zlib_knl_mtx);

  Result:=0;
 end else
 begin
  Result:=EINVAL;
 end;

end;

const
 zlib_cdevsw:t_cdevsw=(
  d_version     :D_VERSION;
  d_flags       :0;
  d_name        :'zlib';
  d_open        :@zlib_open;
  d_close       :@zlib_close;
  d_ioctl       :@zlib_ioctl;
  d_kqfilter    :@zlib_kqfilter;
 );

Function zlib_modevent(_mod,_type:Integer):Integer;
begin
 case (_type) of
  MOD_LOAD:
   begin
    mtx_init       (zlib_sc_mtx ,'zlib_sc_mtx');
    mtx_init       (zlib_knl_mtx,'zlib_knl_mtx');
    knlist_init_mtx(@zlib_knlist,@zlib_knl_mtx);
    zlib_dev:=make_dev_credf(0, @zlib_cdevsw, 0, UID_ROOT, GID_WHEEL, &666, 'sce_zlib',[]);
   end;

  MOD_UNLOAD:
   begin
    zlib_close    (zlib_dev,0,0);
    destroy_dev   (zlib_dev);
    mtx_destroy   (zlib_sc_mtx);
    knlist_clear  (@zlib_knlist, 0);
    knlist_destroy(@zlib_knlist);
    mtx_destroy   (zlib_knl_mtx);
   end;

  MOD_SHUTDOWN:;

  else
   Exit(EOPNOTSUPP);
 end;

 Exit(0);
end;

end.

