unit ps4_pthread_key;

{$mode ObjFPC}{$H+}

interface

uses
 LFQueue,
 spinlock,
 sys_kernel,
 sys_pthread,
 sys_signal;

procedure _thread_cleanupspecific;

function  ps4_pthread_key_create(pKey:Ppthread_key_t;dest:t_cb_proc):Integer; SysV_ABI_CDecl;
function  ps4_pthread_key_delete(Key:pthread_key_t):Integer; SysV_ABI_CDecl;
function  ps4_pthread_getspecific(Key:pthread_key_t):Pointer; SysV_ABI_CDecl;
function  ps4_pthread_setspecific(Key:pthread_key_t;value:Pointer):Integer; SysV_ABI_CDecl;

//undefined8 pthread_get_specificarray_np(long param_1,undefined4 *param_2)

function  ps4_scePthreadKeyCreate(pKey:Ppthread_key_t;dest:t_cb_proc):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadKeyDelete(Key:pthread_key_t):Integer; SysV_ABI_CDecl;
function  ps4_scePthreadGetspecific(Key:pthread_key_t):Pointer; SysV_ABI_CDecl;
function  ps4_scePthreadSetspecific(Key:pthread_key_t;value:Pointer):Integer; SysV_ABI_CDecl;

implementation

type
 p_pthread_key_node=^_pthread_key_node;
 _pthread_key_node=packed record
  next_:p_pthread_key_node;
  version_:ptruint;
  dest_:t_cb_proc;
 end;

var
 _pthread_key_nodes:array[0..SCE_PTHREAD_KEYS_MAX-1] of _pthread_key_node;
 _pthread_key_queue:TIntrusiveMPSCQueue;
 _pthread_key_queue_lock:Pointer;

procedure _pthread_keys_init;
var
 i:Integer;
begin
 _pthread_key_queue.Create;
 For i:=Low(_pthread_key_nodes) to High(_pthread_key_nodes) do
 begin
  _pthread_key_nodes[i]:=Default(_pthread_key_node);
  _pthread_key_queue.Push(@_pthread_key_nodes[i]);
 end;
end;

const
 PTHREAD_DESTRUCTOR_ITERATIONS=4;

procedure _thread_cleanupspecific;
var
 i,k:Integer;
 keys:p_pthread_key_data;
 local:p_pthread_key_data;

 node:p_pthread_key_node;
 version:ptruint;
 dest:t_cb_proc;

 nofree:Boolean;
begin
 keys:=tcb_thread^.specific;
 if (keys=nil) then Exit;

 local:=@keys[0];

 For i:=0 to PTHREAD_DESTRUCTOR_ITERATIONS-1 do
 begin
  nofree:=True;
  For k:=Low(_pthread_key_nodes) to High(_pthread_key_nodes) do
  begin
   node:=@_pthread_key_nodes[k];

   version:=load_consume(node^.version_);
   dest:=t_cb_proc(load_consume(Pointer(node^.dest_)));

   if (ptruint(dest)>1) and
      (local[k].version_=version) and
      (local[k].data_<>nil) then
   begin
    dest(local[k].data_);
    local[k].data_:=nil;
    nofree:=False;
   end;
  end;
  if nofree then Break;
 end;

 tcb_thread^.specific:=nil;
 SwFreeMem(keys);
end;

function ps4_pthread_key_create(pKey:Ppthread_key_t;dest:t_cb_proc):Integer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
 Key:pthread_key_t;
begin
 if (pKey=nil) then Exit(EINVAL);
 Writeln('pthread_key_create');

 if (dest=nil) then dest:=t_cb_proc(1);

 node:=nil;
 spin_lock(_pthread_key_queue_lock);
 _pthread_key_queue.Pop(node);
 spin_unlock(_pthread_key_queue_lock);

 if (node=nil) then Exit(EAGAIN);

 System.InterlockedIncrement(Pointer(node^.version_));
 XCHG(Pointer(node^.dest_),Pointer(dest));

 Key:=(node-p_pthread_key_node(@_pthread_key_nodes));
 Key:=Key+1;

 pKey^:=Key;
 Result:=0;
end;

function ps4_pthread_key_delete(Key:pthread_key_t):Integer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
begin
 Key:=Key-1;

 if (DWORD(Key)>SCE_PTHREAD_KEYS_MAX) then Exit(EINVAL);
 Writeln('pthread_key_delete');

 node:=@_pthread_key_nodes[Key];

 if (XCHG(Pointer(node^.dest_),nil)=nil) then Exit(EINVAL);

 System.InterlockedIncrement(Pointer(node^.version_));

 _pthread_key_queue.Push(node);
end;

function ps4_pthread_getspecific(Key:pthread_key_t):Pointer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
 version:ptruint;
 keys:p_pthread_key_data;
 local:p_pthread_key_data;
begin
 Key:=Key-1;

 if (DWORD(Key)>=SCE_PTHREAD_KEYS_MAX) then Exit(nil);

 node:=@_pthread_key_nodes[Key];

 version:=load_consume(node^.version_);
 if (load_consume(Pointer(node^.dest_))=nil) then Exit(nil);

 keys:=tcb_thread^.specific;
 if (keys=nil) then Exit(nil);

 local:=@keys[Key];

 if (local^.version_<>version) then Exit(nil);

 Result:=local^.data_;
end;

function ps4_pthread_setspecific(Key:pthread_key_t;value:Pointer):Integer; SysV_ABI_CDecl;
var
 node:p_pthread_key_node;
 version:ptruint;
 keys:p_pthread_key_data;
 local:p_pthread_key_data;
begin
 Key:=Key-1;

 if (DWORD(Key)>=SCE_PTHREAD_KEYS_MAX) then Exit(EINVAL);

 node:=@_pthread_key_nodes[Key];

 version:=load_consume(node^.version_);
 if (load_consume(Pointer(node^.dest_))=nil) then Exit(EINVAL);

 keys:=tcb_thread^.specific;
 if (keys=nil) then
 begin
  keys:=SwAllocMem(SizeOf(_pthread_keys));
  if (keys=nil) then Exit(ENOMEM);
  tcb_thread^.specific:=keys;
 end;

 local:=@keys[Key];

 local^.version_:=version;
 local^.data_   :=value;

 Result:=0;
end;

//interface

function ps4_scePthreadKeyCreate(pKey:Ppthread_key_t;dest:t_cb_proc):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_key_create(pKey,dest));
end;

function ps4_scePthreadKeyDelete(Key:pthread_key_t):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_key_delete(Key));
end;

function ps4_scePthreadGetspecific(Key:pthread_key_t):Pointer; SysV_ABI_CDecl;
begin
 Result:=ps4_pthread_getspecific(Key);
end;

function ps4_scePthreadSetspecific(Key:pthread_key_t;value:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=px2sce(ps4_pthread_setspecific(Key,value));
end;

initialization
 _pthread_keys_init;

end.

