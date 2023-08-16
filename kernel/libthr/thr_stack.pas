unit thr_stack;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 syscalls,
 pthread_md,
 thr_private,
 thr_init;

procedure _thr_stack_fix_protection(td:p_pthread);
function  _thr_stack_alloc(attr:p_pthread_attr):Integer;
procedure _thr_stack_free(attr:p_pthread_attr);

implementation

uses
 sys_mmap;

type
 //Spare thread stack.
 p_stack=^t_stack;
 t_stack=packed record
  qe       :LIST_ENTRY; //Stack queue linkage.
  stacksize:QWORD;      //Stack size (rounded up).
  guardsize:QWORD;      //Guard size.
  stackaddr:Pointer;    //Stack address.
 end;

var
 dstackq   :p_stack=nil;
 mstackq   :p_stack=nil;
 last_stack:Pointer=nil;

function round_up(size:QWORD):QWORD; inline;
begin
 if ((size mod _thr_page_size)<>0) then
 begin
  size:=((size div _thr_page_size)+1)*_thr_page_size;
 end;
 Result:=size;
end;

const
 _rtld_get_stack_prot=PROT_READ or PROT_WRITE;

procedure _thr_stack_fix_protection(td:p_pthread);
begin
 mprotect(td^.attr.stackaddr_attr+
          round_up(td^.attr.guardsize_attr),
          round_up(td^.attr.stacksize_attr),
         _rtld_get_stack_prot);
end;

function _thr_stack_alloc(attr:p_pthread_attr):Integer;
var
 curthread:p_pthread;
 spare_stack,next:p_stack;
 stacksize:QWORD;
 guardsize:QWORD;
 stackaddr:Pointer;
 r:Integer;
begin
 curthread:=_get_curthread;

 stacksize:=round_up(attr^.stacksize_attr);
 guardsize:=round_up(attr^.guardsize_attr);

 attr^.stackaddr_attr:=nil;
 attr^.flags:=attr^.flags and (not THR_STACK_USER);

 THREAD_LIST_WRLOCK(curthread);

 if ((stacksize=THR_STACK_DEFAULT) and
     (guardsize=_thr_guard_default)) then
 begin
  spare_stack:=LIST_FIRST(@dstackq);
  if (spare_stack<>nil) then
  begin
   LIST_REMOVE(spare_stack,@spare_stack^.qe);
   attr^.stackaddr_attr:=spare_stack^.stackaddr;
  end;
 end else
 begin
  spare_stack:=LIST_FIRST(@mstackq);
  While (spare_stack<>nil) do
  begin
   next:=LIST_NEXT(spare_stack,@spare_stack^.qe);
   if (spare_stack^.stacksize=stacksize) and
      (spare_stack^.guardsize=guardsize) then
   begin
    LIST_REMOVE(spare_stack,@spare_stack^.qe);
    attr^.stackaddr_attr:=spare_stack^.stackaddr;
    break;
   end;
   spare_stack:=next;
  end;
 end;

 if (attr^.stackaddr_attr<>nil) then
 begin
  THREAD_LIST_UNLOCK(curthread);
 end else
 begin
  if (last_stack=nil) then
  begin
   last_stack:=_usrstack-_thr_stack_initial-_thr_guard_default;
  end;

  stackaddr:=last_stack-stacksize-guardsize;

  last_stack:=last_stack-(stacksize+guardsize);

  THREAD_LIST_UNLOCK(curthread);

  stackaddr:=mmap(stackaddr,
                  stacksize+guardsize,
                  _rtld_get_stack_prot,
                  MAP_STACK,-1,0
                 );

  r:=0;
  if (stackaddr<>MAP_FAILED) then
  begin
   r:=mprotect(stackaddr,guardsize,PROT_NONE);
  end;

  if (stackaddr<>MAP_FAILED) and
     ((guardsize=0) or (r=0)) then
  begin
   sceKernelSetVirtualRangeName(stackaddr,guardsize,'stack guard');
   stackaddr:=stackaddr+guardsize;
  end else
  begin
   if (stackaddr<>MAP_FAILED) then
   begin
    munmap(stackaddr,stacksize+guardsize);
   end;
   stackaddr:=nil;
  end;
  attr^.stackaddr_attr:=stackaddr;
 end;

 if (attr^.stackaddr_attr<>nil) then
  Result:=0
 else
  Result:=-1;
end;

procedure _thr_stack_free(attr:p_pthread_attr);
var
 spare_stack:p_stack;
begin
 if (attr<>nil) and
    ((attr^.flags and THR_STACK_USER)=0) and
    (attr^.stackaddr_attr<>nil) then
 begin
  spare_stack:=attr^.stackaddr_attr+attr^.stacksize_attr-sizeof(t_stack);
  spare_stack^.stacksize:=round_up(attr^.stacksize_attr);
  spare_stack^.guardsize:=round_up(attr^.guardsize_attr);
  spare_stack^.stackaddr:=attr^.stackaddr_attr;

  if (spare_stack^.stacksize=THR_STACK_DEFAULT) and
     (spare_stack^.guardsize=_thr_guard_default) then
  begin
   LIST_INSERT_HEAD(@dstackq,spare_stack,@spare_stack^.qe);
  end else
  begin
   LIST_INSERT_HEAD(@mstackq,spare_stack,@spare_stack^.qe);
  end;
  attr^.stackaddr_attr:=nil;
 end;
end;

end.

