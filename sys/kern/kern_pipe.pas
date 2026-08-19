unit kern_pipe;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 time,
 kern_mtx,
 kern_id,
 kern_thr,
 sys_conf,
 vfcntl,
 vfile,
 vstat,
 kern_descrip,
 sys_event,
 vfilio,
 vttycom,
 vuio,
 vpoll,
 vselinfo,
 vfs_subr,
 vm,
 vmparam,
 kern_param;

const
 PIPE_SIZE      =16384;
 BIG_PIPE_SIZE  =(64*1024);
 SMALL_PIPE_SIZE=PAGE_SIZE;
 PIPE_MINDIRECT =8192;
 PIPENPAGES     =(BIG_PIPE_SIZE div PAGE_SIZE + 1);

{
 Pipe buffer information.
 Separate in, out, cnt are used to simplify calculations.
 Buffered write is active when the buffer.cnt field is set.
}

type
 p_pipebuf=^t_pipebuf;
 t_pipebuf=record
  cnt   :DWORD; //number of chars currently in buffer
  _in   :DWORD; //in pointer
  _out  :DWORD; //out pointer
  size  :DWORD; //size of buffer
  buffer:PBYTE; //kva of buffer
 end;

{
 Information to support direct transfers between processes for pipes.
}
 t_pipemapping=record
  cnt   :vm_size_t;  // number of chars in buffer
  pos   :vm_size_t;  // current position of transfer
  npages:Integer;    // number of pages
  //vm_page_t ms[PIPENPAGES]; /// pages in source process
 end;

 //Bits in pipe_state.
const
 PIPE_ASYNC   =$004; // Async? I/O.
 PIPE_WANTR   =$008; // Reader wants some characters.
 PIPE_WANTW   =$010; // Writer wants space to put characters.
 PIPE_WANT    =$020; // Pipe is wanted to be run-down.
 PIPE_SEL     =$040; // Pipe has a select active.
 PIPE_EOF     =$080; // Pipe is in EOF condition.
 PIPE_LOCKFL  =$100; // Process has exclusive access to pointers/data.
 PIPE_LWANT   =$200; // Process wants exclusive access to pointers/data.
 PIPE_DIRECTW =$400; // Pipe direct write active.
 PIPE_DIRECTOK=$800; // Direct mode ok.

{
 Per-pipe data structure.
 Two of these are linked together to produce bi-directional pipes.
}

type
 p_pipe=^t_pipe;
 p_pipepair=^t_pipepair;

 t_pipe=record
  pipe_buffer :t_pipebuf;     //data storage
  pipe_map    :t_pipemapping; //pipe mapping for direct I/O
  pipe_sel    :t_selinfo;     //for compat with select
  pipe_atime  :timespec;      //time of last access
  pipe_mtime  :timespec;      //time of last modify
  pipe_ctime  :timespec;      //time of status change
  //pipe_sigio  :p_sigio;       //information for async I/O
  pipe_peer   :p_pipe;        //link with other direction
  pipe_pair   :p_pipepair;    //container structure pointer
  pipe_state  :DWORD;         //pipe status info
  pipe_busy   :Integer;       //busy flag, mostly to handle rundown sanely
  pipe_present:Integer;       //still present?
  pipe_ino    :ino_t;         //fake inode for stat(2)
 end;

 {
  Container structure to hold the two pipe endpoints, mutex, and label
  pointer.
 }
 t_pipepair=record
  pp_rpipe:t_pipe;
  pp_wpipe:t_pipe;
  pp_mtx  :mtx;
  //pp_label:p_label;
 end;

const
 //Values for the pipe_present.
 PIPE_ACTIVE   =1;
 PIPE_CLOSING  =2;
 PIPE_FINALIZED=3;

{
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/conf.h>
#include <sys/fcntl.h>
#include <sys/file.h>
#include <sys/filedesc.h>
#include <sys/filio.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/mutex.h>
#include <sys/ttycom.h>
#include <sys/stat.h>
#include <sys/malloc.h>
#include <sys/poll.h>
#include <sys/selinfo.h>
#include <sys/signalvar.h>
#include <sys/syscallsubr.h>
#include <sys/sysctl.h>
#include <sys/sysproto.h>
#include <sys/pipe.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/uio.h>
#include <sys/event.h>

#include <security/mac/mac_framework.h>

#include <vm/vm.h>
#include <vm/vm_param.h>
#include <vm/vm_object.h>
#include <vm/vm_kern.h>
#include <vm/vm_extern.h>
#include <vm/pmap.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>
#include <vm/uma.h>
}

{$define PIPE_NODIRECT}

function pipe_read    (fp:p_file;uio:p_uio;flags:Integer):Integer;
function pipe_write   (fp:p_file;uio:p_uio;flags:Integer):Integer;
function pipe_truncate(fp:p_file;length:Int64):Integer;
function pipe_ioctl   (fp:p_file;cmd:QWORD;data:Pointer):Integer;
function pipe_poll    (fp:p_file;events:Integer):Integer;
function pipe_kqfilter(fp:p_file;kn:p_knote):Integer;
function pipe_stat    (fp:p_file;ub:p_stat):Integer;
function pipe_close   (fp:p_file):Integer;

const
 pipeops_f:fileops=(
  fo_read    :@pipe_read;
  fo_write   :@pipe_write;
  fo_truncate:@pipe_truncate;
  fo_ioctl   :@pipe_ioctl;
  fo_poll    :@pipe_poll;
  fo_kqfilter:@pipe_kqfilter;
  fo_stat    :@pipe_stat;
  fo_close   :@pipe_close;
  fo_chmod   :@invfo_chmod;
  fo_chown   :@invfo_chown;
  fo_flags   :DFLAG_PASSABLE
 );

procedure filt_pipedetach(kn:p_knote);
function  filt_piperead  (kn:p_knote;hint:QWORD):Integer;
function  filt_pipewrite (kn:p_knote;hint:QWORD):Integer;

const
 pipe_rfiltops:t_filterops=(
  f_isfd  :1;
  f_detach:@filt_pipedetach;
  f_event :@filt_piperead;
 );

 pipe_wfiltops:t_filterops=(
  f_isfd  :1;
  f_detach:@filt_pipedetach;
  f_event :@filt_pipewrite;
 );

{
 * Default pipe buffer size(s), this can be kind-of large now because pipe
 * space is pageable.  The pipe code will try to maintain locality of
 * reference for performance reasons, so small amounts of outstanding I/O
 * will not wipe the cache.
 }
const
 MINPIPESIZE=(PIPE_SIZE div 3);
 MAXPIPESIZE=(2*PIPE_SIZE div 3);

var
 amountpipekva    :QWORD  =0;
 pipefragretry    :Integer=0;
 pipeallocfail    :Integer=0;
 piperesizefail   :Integer=0;
 piperesizeallowed:Integer=1;

 maxpipekva       :QWORD=512*1024; //Limit on pipe KVA

//SYSCTL_LONG(_kern_ipc, OID_AUTO, maxpipekva, CTLFLAG_RDTUN,&maxpipekva, 0, 'Pipe KVA limit');
//SYSCTL_LONG(_kern_ipc, OID_AUTO, pipekva, CTLFLAG_RD,&amountpipekva, 0, 'Pipe KVA usage');
//SYSCTL_INT(_kern_ipc, OID_AUTO, pipefragretry, CTLFLAG_RD,&pipefragretry, 0, 'Pipe allocation retries due to fragmentation');
//SYSCTL_INT(_kern_ipc, OID_AUTO, pipeallocfail, CTLFLAG_RD,&pipeallocfail, 0, 'Pipe allocation failures');
//SYSCTL_INT(_kern_ipc, OID_AUTO, piperesizefail, CTLFLAG_RD,&piperesizefail, 0, 'Pipe resize failures');
//SYSCTL_INT(_kern_ipc, OID_AUTO, piperesizeallowed, CTLFLAG_RW,&piperesizeallowed, 0, 'Pipe resizing allowed');
//

procedure pipeinit; //SYSINIT(vfs, SI_SUB_VFS, SI_ORDER_ANY, pipeinit, nil);
procedure pipeclose(cpipe:p_pipe);
procedure pipe_free_kmem(cpipe:p_pipe);
function  pipe_create(pipe:p_pipe;backing:Integer):Integer;

function sys_pipe():Integer;

var
 pipeino_unr:p_id_desc_table;
 pipedev_ino:ino_t;

type
 t_fildes_pair=array[0..1] of Integer;

implementation

uses
 md_map,
 systm,
 subr_uio,
 vsys_generic;

function PIPE_MTX(pipe:p_pipe):p_mtx; inline;
begin
 Result:=@pipe^.pipe_pair^.pp_mtx
end;

procedure PIPE_LOCK(pipe:p_pipe); inline;
begin
 mtx_lock(PIPE_MTX(pipe)^);
end;

procedure PIPE_UNLOCK(pipe:p_pipe); inline;
begin
 mtx_unlock(PIPE_MTX(pipe)^);
end;

procedure PIPE_LOCK_ASSERT(pipe:p_pipe); inline;
begin
 mtx_assert(PIPE_MTX(pipe)^);
end;

///////

function new_unrhdr(min,max:Integer):p_id_desc_table;
begin
 Result:=AllocMem(SizeOf(t_id_desc_table));
 id_table_init(Result,min,max);
end;

var
 unr_desc:t_id_desc=(free:nil;refs:0); //temp

function alloc_unr(p:p_id_desc_table):Integer;
begin
 if id_new(p,@unr_desc,@Result) then
 begin
  id_release(@unr_desc); //<-id_new
 end else
 begin
  Result:=-1;
 end;
end;

procedure free_unr(p:p_id_desc_table;i:Integer);
begin
 id_del(p,i,nil);
end;

//

procedure pipeinit;
begin
 pipeino_unr:=new_unrhdr(1, High(Integer));
 Assert(pipeino_unr<>nil, 'pipe fake inodes not initialized');
 pipedev_ino:=devfs_alloc_cdp_inode();
 Assert(pipedev_ino > 0, 'pipe dev inode not initialized');
end;

procedure pipe_zone_ctor(mem:Pointer);
var
 pp:p_pipepair;
 rpipe,wpipe:p_pipe;
begin
 pp:=p_pipepair(mem);

 {
  * We zero both pipe endpoints to make sure all the kmem pointers
  * are nil, flag fields are zero'd, etc.  We timestamp both
  * endpoints with the same time.
  }
 rpipe:=@pp^.pp_rpipe;
 rpipe^:=Default(t_pipe);
 vfs_timestamp(@rpipe^.pipe_ctime);
 rpipe^.pipe_atime:=rpipe^.pipe_ctime;
 rpipe^.pipe_mtime:=rpipe^.pipe_ctime;

 wpipe:=@pp^.pp_wpipe;
 wpipe^:=Default(t_pipe);
 wpipe^.pipe_ctime:=rpipe^.pipe_ctime;
 wpipe^.pipe_atime:=rpipe^.pipe_ctime;
 wpipe^.pipe_mtime:=rpipe^.pipe_ctime;

 rpipe^.pipe_peer:=wpipe;
 rpipe^.pipe_pair:=pp;
 wpipe^.pipe_peer:=rpipe;
 wpipe^.pipe_pair:=pp;

 {
  * Mark both endpoints as present; they will later get free'd
  * one at a time.  When both are free'd, then the whole pair
  * is released.
  }
 rpipe^.pipe_present:=PIPE_ACTIVE;
 wpipe^.pipe_present:=PIPE_ACTIVE;

 {
  * Eventually, the MAC Framework may initialize the label
  * in ctor or init, but for now we do it elswhere to avoid
  * blocking in ctor or init.
  }
 //pp^.pp_label:=nil;
end;

procedure pipe_zone_init(mem:Pointer);
var
 pp:p_pipepair;
begin
 pp:=p_pipepair(mem);

 mtx_init(pp^.pp_mtx, 'pipe mutex');
end;

procedure pipe_zone_fini(mem:Pointer);
var
 pp:p_pipepair;
begin
 pp:=p_pipepair(mem);

 mtx_destroy(pp^.pp_mtx);
end;

function alloc_pipepair:p_pipepair;
begin
 Result:=AllocMem(SizeOf(t_pipepair));
 pipe_zone_init(Result);
 pipe_zone_ctor(Result);
end;

{
 * The pipe system call for the DTYPE_PIPE type of pipes.  If we fail, let
 * the zone pick up the pieces via pipeclose().
 }

function do_pipe(var fildes:t_fildes_pair;flags:Integer):Integer;
var
 //struct filedesc *fdp:=td^.td_proc^.p_fd;
 rf,wf:p_file;
 pp:p_pipepair;
 rpipe,wpipe:p_pipe;
 fd,fflags,error:Integer;
begin
 pp:=alloc_pipepair;

 rpipe:=@pp^.pp_rpipe;
 wpipe:=@pp^.pp_wpipe;

 knlist_init_mtx(@rpipe^.pipe_sel.si_note, PIPE_MTX(rpipe));
 knlist_init_mtx(@wpipe^.pipe_sel.si_note, PIPE_MTX(wpipe));

 { Only the forward direction pipe is backed by default }
 error:=pipe_create(rpipe, 1);
 if (error<>0) then
 begin
  pipeclose(rpipe);
  pipeclose(wpipe);
  Exit(error);
 end;

 error:=pipe_create(wpipe, 0);
 if (error<>0) then
 begin
  pipeclose(rpipe);
  pipeclose(wpipe);
  Exit(error);
 end;

 rpipe^.pipe_state:=rpipe^.pipe_state or PIPE_DIRECTOK;
 wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_DIRECTOK;

 error:=falloc(@rf, @fd, flags);
 if (error<>0) then
 begin
  pipeclose(rpipe);
  pipeclose(wpipe);
  Exit(error);
 end;
 { An extra reference on `rf' has been held for us by falloc(). }
 fildes[0]:=fd;

 fflags:=FREAD or FWRITE;
 if ((flags and O_NONBLOCK)<>0) then
 begin
  fflags:=fflags or FNONBLOCK;
 end;

 {
  * Warning: once we've gotten past allocation of the fd for the
  * read-side, we can only drop the read side via fdrop() in order
  * to avoid races against processes which manage to dup() the read
  * side while we are blocked trying to allocate the write side.
  }
 finit(rf, fflags, DTYPE_PIPE, rpipe, @pipeops_f);
 error:=falloc(@wf, @fd, flags);
 if (error<>0) then
 begin
  fdclose(rf, fildes[0]);
  fdrop(rf);
  { rpipe has been closed by fdrop(). }
  pipeclose(wpipe);
  Exit(error);
 end;
 { An extra reference on `wf' has been held for us by falloc(). }
 finit(wf, fflags, DTYPE_PIPE, wpipe, @pipeops_f);
 fdrop(wf);
 fildes[1]:=fd;
 fdrop(rf);

 Exit(0);
end;

function kern_pipe(var fildes:t_fildes_pair):Integer; inline;
begin
 Exit(do_pipe(fildes, 0));
end;

{ ARGSUSED }
function sys_pipe():Integer;
var
 td:p_kthread;
 error:Integer;
 fildes:t_fildes_pair;
begin
 td:=curkthread;
 if (td=nil) then Exit(EFAULT);

 fildes:=Default(t_fildes_pair);

 error:=kern_pipe(fildes);
 if (error<>0) then
 begin
  Exit(error);
 end;

 td^.td_retval[0]:=fildes[0];
 td^.td_retval[1]:=fildes[1];

 Exit(0);
end;

{
 * Allocate kva for pipe circular buffer, the space is pageable
 * This routine will 'realloc' the size of a pipe safely, if it fails
 * it will retain the old buffer.
 * If it fails it will Exit ENOMEM.
 }
function pipespace_new(cpipe:p_pipe;size:Integer):Integer;
label
 retry;
var
 buffer:PBYTE;
 cnt,firstseg:Integer;
 curfail:Integer;
 lastfail:timeval;
begin
 curfail:=0;

 Assert(not mtx_owned(PIPE_MTX(cpipe)^), 'pipespace: pipe mutex locked');
 Assert((cpipe^.pipe_state and PIPE_DIRECTW)=0,'pipespace: resize of direct writes not allowed');

retry:
 cnt:=cpipe^.pipe_buffer.cnt;
 if (cnt > size) then
 begin
  size:=cnt;
 end;

 size  :=round_page(size);

 buffer:=kmem_alloc(size,VM_RW);

 //buffer:=vm_map_min(pipe_map);
 //error:=vm_map_find(pipe_map, nil, 0, @buffer, size, 1, VM_PROT_ALL, VM_PROT_ALL, 0);

 if (buffer=nil) then
 begin
  if (cpipe^.pipe_buffer.buffer=nil) and
     (size > SMALL_PIPE_SIZE) then
  begin
   size:=SMALL_PIPE_SIZE;
   Inc(pipefragretry);
   goto retry;
  end;
  if (cpipe^.pipe_buffer.buffer=nil) then
  begin
   Inc(pipeallocfail);
   //if (ppsratecheck(@lastfail, @curfail, 1)) then
   //begin
   // printf('kern.ipc.maxpipekva exceeded; see tuning(7)\n');
   //end;
  end else
  begin
   Inc(piperesizefail);
  end;
  Exit(ENOMEM);
 end;

 { copy data, then free old resources if we're resizing }
 if (cnt > 0) then
 begin
  if (cpipe^.pipe_buffer._in <= cpipe^.pipe_buffer._out) then
  begin
   firstseg:=cpipe^.pipe_buffer.size - cpipe^.pipe_buffer._out;
   Move(cpipe^.pipe_buffer.buffer[cpipe^.pipe_buffer._out],buffer^,firstseg);
   if ((cnt - firstseg) > 0) then
   begin
    Move(cpipe^.pipe_buffer.buffer^, buffer[firstseg], cpipe^.pipe_buffer._in);
   end;
  end else
  begin
   Move(cpipe^.pipe_buffer.buffer[cpipe^.pipe_buffer._out], buffer^, cnt);
  end;
 end;

 pipe_free_kmem(cpipe);

 cpipe^.pipe_buffer.buffer:=buffer;
 cpipe^.pipe_buffer.size  :=size;
 cpipe^.pipe_buffer._in   :=cnt;
 cpipe^.pipe_buffer._out  :=0;
 cpipe^.pipe_buffer.cnt   :=cnt;

 System.InterlockedExchangeAdd64(amountpipekva, cpipe^.pipe_buffer.size);
 Exit(0);
end;

{
 * Wrapper for pipespace_new() that performs locking assertions.
 }
function pipespace(cpipe:p_pipe;size:Integer):Integer;
begin
 Assert((cpipe^.pipe_state and PIPE_LOCKFL)<>0,'Unlocked pipe passed to pipespace');
 Exit(pipespace_new(cpipe, size));
end;

{
 * lock a pipe for I/O, blocking other access
 }
function pipelock(cpipe:p_pipe;catch:Integer):Integer; inline;
var
 error:Integer;
begin
 PIPE_LOCK_ASSERT(cpipe);
 while ((cpipe^.pipe_state and PIPE_LOCKFL)<>0) do
 begin
  cpipe^.pipe_state:=cpipe^.pipe_state or PIPE_LWANT;

  if (catch<>0) then
  begin
   error:=msleep(cpipe, PIPE_MTX(cpipe), PRIBIO or PCATCH, 'pipelk', 0);
  end else
  begin
   error:=msleep(cpipe, PIPE_MTX(cpipe), PRIBIO, 'pipelk', 0);
  end;

  if (error<>0) then
  begin
   Exit(error);
  end;
 end;

 cpipe^.pipe_state:=cpipe^.pipe_state or PIPE_LOCKFL;
 Exit(0);
end;

{
 * unlock a pipe I/O lock
 }
procedure pipeunlock(cpipe:p_pipe);
begin
 PIPE_LOCK_ASSERT(cpipe);
 Assert((cpipe^.pipe_state and PIPE_LOCKFL)<>0,'Unlocked pipe passed to pipeunlock');

 cpipe^.pipe_state:=cpipe^.pipe_state and (not PIPE_LOCKFL);

 if (cpipe^.pipe_state and PIPE_LWANT)<>0 then
 begin
  cpipe^.pipe_state:=cpipe^.pipe_state and (not PIPE_LWANT);
  wakeup(cpipe);
 end;
end;

procedure pipeselwakeup(cpipe:p_pipe); inline;
begin
 PIPE_LOCK_ASSERT(cpipe);
 if (cpipe^.pipe_state and PIPE_SEL)<>0 then
 begin
  selwakeuppri(@cpipe^.pipe_sel, PSOCK);
  if (not SEL_WAITING(@cpipe^.pipe_sel)) then
  begin
   cpipe^.pipe_state:=cpipe^.pipe_state and (not PIPE_SEL);
  end;
 end;

 //if ((cpipe^.pipe_state and PIPE_ASYNC)<>0) and (cpipe^.pipe_sigio|<>nil) then
 //begin
 // pgsigio(@cpipe^.pipe_sigio, SIGIO, 0);
 //end;

 KNOTE_LOCKED(@cpipe^.pipe_sel.si_note, 0);
end;

{
 * Initialize and allocate VM and memory for pipe.  The structure
 * will start out zero'd from the ctor, so we just manage the kmem.
 }
function pipe_create(pipe:p_pipe;backing:Integer):Integer;
var
 error:Integer;
begin
 if (backing<>0) then
 begin
  if (amountpipekva > maxpipekva div 2) then
   error:=pipespace_new(pipe, SMALL_PIPE_SIZE)
  else
   error:=pipespace_new(pipe, PIPE_SIZE);
 end else
 begin
  { If we're not backing this pipe, no need to do anything. }
  error:=0;
 end;
 pipe^.pipe_ino:=-1;
 Exit(error);
end;

{ ARGSUSED }
function pipe_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
label
 unlocked_error;
var
 rpipe:p_pipe;
 error:Integer;
 nread:Integer;
 size :Integer;
begin
 rpipe:=fp^.f_data;
 nread:=0;

 PIPE_LOCK(rpipe);
 Inc(rpipe^.pipe_busy);

 error:=pipelock(rpipe, 1);

 if (error<>0) then
 begin
  goto unlocked_error;
 end;

 if (amountpipekva > (3 * maxpipekva) div 4) then
 begin
  if ((rpipe^.pipe_state and PIPE_DIRECTW)=0) and
     (rpipe^.pipe_buffer.size > SMALL_PIPE_SIZE) and
     (rpipe^.pipe_buffer.cnt <= SMALL_PIPE_SIZE) and
     (piperesizeallowed=1) then
  begin
   PIPE_UNLOCK(rpipe);
   pipespace(rpipe, SMALL_PIPE_SIZE);
   PIPE_LOCK(rpipe);
  end;
 end;

 while (uio^.uio_resid>0) do
 begin
  {
   * normal pipe buffer receive
   }
  if (rpipe^.pipe_buffer.cnt > 0) then
  begin
   size:=rpipe^.pipe_buffer.size - rpipe^.pipe_buffer._out;

   if (size > rpipe^.pipe_buffer.cnt) then
   begin
    size:=rpipe^.pipe_buffer.cnt;
   end;

   if (size > uio^.uio_resid) then
   begin
    size:=uio^.uio_resid;
   end;

   PIPE_UNLOCK(rpipe);

   error:=uiomove(@rpipe^.pipe_buffer.buffer[rpipe^.pipe_buffer._out], size, uio);

   PIPE_LOCK(rpipe);

   if (error<>0) then
   begin
    break;
   end;

   Inc(rpipe^.pipe_buffer._out,size);

   if (rpipe^.pipe_buffer._out >= rpipe^.pipe_buffer.size) then
   begin
    rpipe^.pipe_buffer._out:=0;
   end;

   Dec(rpipe^.pipe_buffer.cnt,size);

   {
    * If there is no more to read in the pipe, reset
    * its pointers to the beginning.  This improves
    * cache hit stats.
    }
   if (rpipe^.pipe_buffer.cnt=0) then
   begin
    rpipe^.pipe_buffer._in :=0;
    rpipe^.pipe_buffer._out:=0;
   end;

   Inc(nread,size);

{$ifndef PIPE_NODIRECT}
  {
   * Direct copy, bypassing a kernel buffer.
   }
  end else
  if (rpipe^.pipe_map.cnt<>0) and
     ((rpipe^.pipe_state and PIPE_DIRECTW)<>0) then
  begin
   size:=rpipe^.pipe_map.cnt;

   if (size > uio^.uio_resid) then
   begin
    size:=uio^.uio_resid;
   end;


   PIPE_UNLOCK(rpipe);

   error:=uiomove_fromphys(rpipe^.pipe_map.ms,rpipe^.pipe_map.pos, size, uio);

   PIPE_LOCK(rpipe);

   if (error<>0) then
   begin
    break;
   end;

   Inc(nread,size);
   Inc(rpipe^.pipe_map.pos,size);
   Dec(rpipe^.pipe_map.cnt,size);
   if (rpipe^.pipe_map.cnt=0) then
   begin
    rpipe^.pipe_state:=rpipe^.pipe_state and (not PIPE_DIRECTW);
    wakeup(rpipe);
   end;
{$endif}
  end else
  begin
   {
    * detect EOF condition
    * read Exits 0 on EOF, no need to set error
    }
   if (rpipe^.pipe_state and PIPE_EOF)<>0 then
   begin
    break;
   end;

   {
    * If the 'write-side' has been blocked, wake it up now.
    }
   if (rpipe^.pipe_state and PIPE_WANTW)<>0 then
   begin
    rpipe^.pipe_state:=rpipe^.pipe_state and (not PIPE_WANTW);
    wakeup(rpipe);
   end;

   {
    * Break if some data was read.
    }
   if (nread > 0) then
   begin
    break;
   end;

   {
    * Unlock the pipe buffer for our remaining processing.
    * We will either break out with an error or we will
    * sleep and relock to loop.
    }
   pipeunlock(rpipe);

   {
    * Handle non-blocking mode operation or
    * wait for more data.
    }
   if (fp^.f_flag and FNONBLOCK)<>0 then
   begin
    error:=EAGAIN;
   end else
   begin
    rpipe^.pipe_state:=rpipe^.pipe_state or PIPE_WANTR;

    error:=msleep(rpipe, PIPE_MTX(rpipe), PRIBIO or PCATCH, 'piperd', 0);

    if (error=0) then
    begin
     error:=pipelock(rpipe, 1);
    end;
   end;

   if (error<>0) then
   begin
    goto unlocked_error;
   end;
  end;
 end;

 pipeunlock(rpipe);

 { XXX: should probably do this before getting any locks. }
 if (error=0) then
 begin
  vfs_timestamp(@rpipe^.pipe_atime);
 end;

unlocked_error:
 Dec(rpipe^.pipe_busy);

 {
  * PIPE_WANT processing only makes sense if pipe_busy is 0.
  }
 if (rpipe^.pipe_busy=0) and ((rpipe^.pipe_state and PIPE_WANT)<>0) then
 begin
  rpipe^.pipe_state:=rpipe^.pipe_state and (not (PIPE_WANT or PIPE_WANTW));
  wakeup(rpipe);
 end else
 if (rpipe^.pipe_buffer.cnt < MINPIPESIZE) then
 begin
  {
   * Handle write blocking hysteresis.
   }
  if (rpipe^.pipe_state and PIPE_WANTW)<>0 then
  begin
   rpipe^.pipe_state:=rpipe^.pipe_state and (not PIPE_WANTW);
   wakeup(rpipe);
  end;
 end;

 if ((rpipe^.pipe_buffer.size - rpipe^.pipe_buffer.cnt) >= PIPE_BUF) then
 begin
  pipeselwakeup(rpipe);
 end;

 PIPE_UNLOCK(rpipe);
 Exit(error);
end;

{$ifndef PIPE_NODIRECT}
{
 * Map the sending processes' buffer into kernel space and wire it.
 * This is similar to a physical write operation.
 }
function pipe_build_write_buffer(wpipe:p_pipe;uio:p_uio):Integer;
var
 size:DWORD;
 i:Integer;
begin
 PIPE_LOCK_ASSERT(wpipe);
 Assert((wpipe^.pipe_state and PIPE_DIRECTW)<>0,'Clone attempt on non-direct write pipe!');

 if (uio^.uio_iov^.iov_len > wpipe^.pipe_buffer.size) then
  size:=wpipe^.pipe_buffer.size
 else
  size:=uio^.uio_iov^.iov_len;

 i:=vm_fault_quick_hold_pages(@curproc^.p_vmspace^.vm_map,
      uio^.uio_iov^.iov_base, size, VM_PROT_READ,
      wpipe^.pipe_map.ms, PIPENPAGES);

 if (i < 0) then
 begin
  Exit(EFAULT);
 end;

{
 * set up the control block
 }
 wpipe^.pipe_map.npages:=i;
 wpipe^.pipe_map.pos   :=uio^.uio_iov^.iov_base and PAGE_MASK;
 wpipe^.pipe_map.cnt   :=size;

{
 * and update the uio data
 }

 Dec(uio^.uio_iov^.iov_len,size);
 uio^.uio_iov^.iov_base:=uio^.uio_iov^.iov_base + size;
 if (uio^.uio_iov^.iov_len=0) then
 begin
  Inc(uio^.uio_iov);
 end;
 Dec(uio^.uio_resid ,size);
 Inc(uio^.uio_offset,size);
 Exit(0);
end;

{
 * unmap and unwire the process buffer
 }
procedure pipe_destroy_write_buffer(wpipe:p_pipe);
begin
 PIPE_LOCK_ASSERT(wpipe, MA_OWNED);
 vm_page_unhold_pages(wpipe^.pipe_map.ms, wpipe^.pipe_map.npages);
 wpipe^.pipe_map.npages:=0;
end;

{
 * In the case of a signal, the writing process might go away.  This
 * code copies the data into the circular buffer so that the source
 * pages can be freed without loss of data.
 }
procedure pipe_clone_write_buffer(wpipe:p_pipe);
var
 uio:t_uio;
 iov:iovec;
 size:Integer;
 pos :Integer;
begin
 PIPE_LOCK_ASSERT(wpipe);
 size:=wpipe^.pipe_map.cnt;
 pos :=wpipe^.pipe_map.pos;

 wpipe^.pipe_buffer._in :=size;
 wpipe^.pipe_buffer._out:=0;
 wpipe^.pipe_buffer.cnt :=size;
 wpipe^.pipe_state      :=wpipe^.pipe_state and (not PIPE_DIRECTW);

 PIPE_UNLOCK(wpipe);
 iov.iov_base:=wpipe^.pipe_buffer.buffer;
 iov.iov_len :=size;

 uio.uio_iov   :=@iov;
 uio.uio_iovcnt:=1;
 uio.uio_offset:=0;
 uio.uio_resid :=size;
 uio.uio_segflg:=UIO_SYSSPACE;
 uio.uio_rw    :=UIO_READ;
 uio.uio_td    :=curthread;

 uiomove_fromphys(wpipe^.pipe_map.ms, pos, size, @uio);

 PIPE_LOCK(wpipe);
 pipe_destroy_write_buffer(wpipe);
end;

{
 * This implements the pipe buffer write mechanism.  Note that only
 * a direct write OR a normal pipe write can be pending at any given time.
 * If there are any characters in the pipe buffer, the direct write will
 * be deferred until the receiving process grabs all of the bytes from
 * the pipe buffer.  Then the direct mapping write is set-up.
 }
function pipe_direct_write(wpipe:p_pipe;uio:p_uio):Integer;
label
 retry,
 error1;
var
 error:Integer;
begin

retry:
 PIPE_LOCK_ASSERT(wpipe);
 error:=pipelock(wpipe, 1);
 if (wpipe^.pipe_state and PIPE_EOF)<>0 then
 begin
  error:=EPIPE;
 end;

 if (error<>0) then
 begin
  pipeunlock(wpipe);
  goto error1;
 end;

 while (wpipe^.pipe_state and PIPE_DIRECTW)<>0 do
 begin
  if (wpipe^.pipe_state and PIPE_WANTR)<>0 then
  begin
   wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_WANTR);
   wakeup(wpipe);
  end;
  pipeselwakeup(wpipe);
  wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_WANTW;
  pipeunlock(wpipe);

  error:=msleep(wpipe, PIPE_MTX(wpipe), PRIBIO or PCATCH, 'pipdww', 0);

  if (error<>0) then
   goto error1
  else
   goto retry;
 end;

 wpipe^.pipe_map.cnt:=0; { transfer not ready yet }
 if (wpipe^.pipe_buffer.cnt > 0) then
 begin
  if (wpipe^.pipe_state and PIPE_WANTR)<>0 then
  begin
   wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_WANTR);
   wakeup(wpipe);
  end;
  pipeselwakeup(wpipe);
  wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_WANTW;
  pipeunlock(wpipe);
  error:=msleep(wpipe, PIPE_MTX(wpipe), PRIBIO or PCATCH, 'pipdwc', 0);
  if (error<>0) then
   goto error1
  else
   goto retry;
 end;

 wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_DIRECTW;

 PIPE_UNLOCK(wpipe);
 error:=pipe_build_write_buffer(wpipe, uio);
 PIPE_LOCK(wpipe);
 if (error<>0) then
 begin
  wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_DIRECTW);
  pipeunlock(wpipe);
  goto error1;
 end;

 error:=0;
 while (error=0) and ((wpipe^.pipe_state and PIPE_DIRECTW)<>0) do
 begin
  if (wpipe^.pipe_state and PIPE_EOF)<>0 then
  begin
   pipe_destroy_write_buffer(wpipe);
   pipeselwakeup(wpipe);
   pipeunlock(wpipe);
   error:=EPIPE;
   goto error1;
  end;
  if (wpipe^.pipe_state and PIPE_WANTR)<>0 then
  begin
   wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_WANTR);
   wakeup(wpipe);
  end;
  pipeselwakeup(wpipe);
  pipeunlock(wpipe);
  error:=msleep(wpipe, PIPE_MTX(wpipe), PRIBIO or PCATCH, 'pipdwt', 0);
  pipelock(wpipe, 0);
 end;

 if (wpipe^.pipe_state and PIPE_EOF)<>0 then
 begin
  error:=EPIPE;
 end;

 if (wpipe^.pipe_state and PIPE_DIRECTW)<>0 then
 begin
  {
   * this bit of trickery substitutes a kernel buffer for
   * the process that might be going away.
   }
  pipe_clone_write_buffer(wpipe);
 end else
 begin
  pipe_destroy_write_buffer(wpipe);
 end;
 pipeunlock(wpipe);
 Exit(error);

error1:
 wakeup(wpipe);
 Exit  (error);
end;
{$endif}

function Max(a,b:Integer):Integer; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function pipe_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
var
 error:Integer;
 desiredsize:Integer;
 orig_resid:Int64;
 wpipe,rpipe:p_pipe;
 space:Integer;
 size:Integer;    { Transfer size }
 segsize:Integer; { first segment to transfer }
begin
 error:=0;

 rpipe:=fp^.f_data;
 wpipe:=rpipe^.pipe_peer;

 PIPE_LOCK(rpipe);
 error:=pipelock(wpipe, 1);
 if (error<>0) then
 begin
  PIPE_UNLOCK(rpipe);
  Exit (error);
 end;
 {
  * detect loss of pipe read side, issue SIGPIPE if lost.
  }
 if (wpipe^.pipe_present<>PIPE_ACTIVE) or
    ((wpipe^.pipe_state and PIPE_EOF)<>0) then
 begin
  pipeunlock(wpipe);
  PIPE_UNLOCK(rpipe);
  Exit(EPIPE);
 end;

 Inc(wpipe^.pipe_busy);

 { Choose a larger size if it's advantageous }
 desiredsize:=max(SMALL_PIPE_SIZE, wpipe^.pipe_buffer.size);
 while (desiredsize < (wpipe^.pipe_buffer.cnt + uio^.uio_resid)) do
 begin
  if (piperesizeallowed<>1) then
  begin
   break;
  end;
  if (amountpipekva > maxpipekva div 2) then
  begin
   break;
  end;
  if (desiredsize=BIG_PIPE_SIZE) then
  begin
   break;
  end;
  desiredsize:=desiredsize * 2;
 end;

 { Choose a smaller size if we're in a OOM situation }
 if (amountpipekva > (3 * maxpipekva) div 4)    and
    (wpipe^.pipe_buffer.size > SMALL_PIPE_SIZE) and
    (wpipe^.pipe_buffer.cnt <= SMALL_PIPE_SIZE) and
    (piperesizeallowed=1) then
 begin
  desiredsize:=SMALL_PIPE_SIZE;
 end;

 { Resize if the above determined that a new size was necessary }
 if (desiredsize<>wpipe^.pipe_buffer.size) and
    ((wpipe^.pipe_state and PIPE_DIRECTW)=0) then
 begin
  PIPE_UNLOCK(wpipe);
  pipespace(wpipe, desiredsize);
  PIPE_LOCK(wpipe);
 end;

 if (wpipe^.pipe_buffer.size=0) then
 begin
  {
   * This can only happen for reverse direction use of pipes
   * in a complete OOM situation.
   }
  error:=ENOMEM;
  Dec(wpipe^.pipe_busy);
  pipeunlock(wpipe);
  PIPE_UNLOCK(wpipe);
  Exit (error);
 end;

 pipeunlock(wpipe);

 orig_resid:=uio^.uio_resid;

 while (uio^.uio_resid>0) do
 begin
  pipelock(wpipe, 0);
  if (wpipe^.pipe_state and PIPE_EOF)<>0 then
  begin
   pipeunlock(wpipe);
   error:=EPIPE;
   break;
  end;
{$ifndef PIPE_NODIRECT}
  {
   * If the transfer is large, we can gain performance if
   * we do process-to-process copies directly.
   * If the write is non-blocking, we don't use the
   * direct write mechanism.
   *
   * The direct write mechanism will detect the reader going
   * away on us.
   }
  if (uio^.uio_segflg=UIO_USERSPACE) and
     (uio^.uio_iov^.iov_len >= PIPE_MINDIRECT) and
     (wpipe^.pipe_buffer.size >= PIPE_MINDIRECT) and
     ((fp^.f_flag and FNONBLOCK)=0) then
  begin
   pipeunlock(wpipe);
   error:=pipe_direct_write(wpipe, uio);
   if (error<>0) then
   begin
    break;
   end;
   continue;
  end
{$endif}

  {
   * Pipe buffered writes cannot be coincidental with
   * direct writes.  We wait until the currently executing
   * direct write is completed before we start filling the
   * pipe buffer.  We break out if a signal occurs or the
   * reader goes away.
   }
  if (wpipe^.pipe_state and PIPE_DIRECTW)<>0 then
  begin
   if (wpipe^.pipe_state and PIPE_WANTR)<>0 then
   begin
    wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_WANTR);
    wakeup(wpipe);
   end;
   pipeselwakeup(wpipe);
   wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_WANTW;
   pipeunlock(wpipe);
   error:=msleep(wpipe, PIPE_MTX(rpipe), PRIBIO or PCATCH, 'pipbww', 0);
   if (error<>0) then
    break
   else
    continue;
  end;

  space:=wpipe^.pipe_buffer.size - wpipe^.pipe_buffer.cnt;

  { Writes of size <= PIPE_BUF must be atomic. }
  if (space < uio^.uio_resid) and (orig_resid <= PIPE_BUF) then
  begin
   space:=0;
  end;

  if (space > 0) then
  begin

   {
    * Transfer size is minimum of uio transfer
    * and free space in pipe buffer.
    }
   if (space > uio^.uio_resid) then
    size:=uio^.uio_resid
   else
    size:=space;
   {
    * First segment to transfer is minimum of
    * transfer size and contiguous space in
    * pipe buffer.  If first segment to transfer
    * is less than the transfer size, we've got
    * a wraparound in the buffer.
    }
   segsize:=wpipe^.pipe_buffer.size - wpipe^.pipe_buffer._in;

   if (segsize > size) then
   begin
    segsize:=size;
   end;

   { Transfer first segment }

   PIPE_UNLOCK(rpipe);

   error:=uiomove(@wpipe^.pipe_buffer.buffer[wpipe^.pipe_buffer._in], segsize, uio);

   PIPE_LOCK(rpipe);

   if (error=0) and (segsize < size) then
   begin
    Assert((wpipe^.pipe_buffer._in + segsize) = wpipe^.pipe_buffer.size,'Pipe buffer wraparound disappeared');

    {
     * Transfer remaining part now, to
     * support atomic writes.  Wraparound
     * happened.
     }

    PIPE_UNLOCK(rpipe);

    error:=uiomove(@wpipe^.pipe_buffer.buffer[0], size - segsize, uio);

    PIPE_LOCK(rpipe);
   end;

   if (error=0) then
   begin
    wpipe^.pipe_buffer._in:=wpipe^.pipe_buffer._in + size;
    if (wpipe^.pipe_buffer._in >= wpipe^.pipe_buffer.size) then
    begin
     Assert(wpipe^.pipe_buffer._in = (size - segsize + wpipe^.pipe_buffer.size), 'Expected wraparound bad');
     wpipe^.pipe_buffer._in:=size - segsize;
    end;

    Inc(wpipe^.pipe_buffer.cnt,size);

    Assert(wpipe^.pipe_buffer.cnt <= wpipe^.pipe_buffer.size,'Pipe buffer overflow');
   end;

   pipeunlock(wpipe);
   if (error<>0) then
   begin
    break;
   end;
  end else
  begin
   {
    * If the 'read-side' has been blocked, wake it up now.
    }
   if (wpipe^.pipe_state and PIPE_WANTR)<>0 then
   begin
    wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_WANTR);
    wakeup(wpipe);
   end;

   {
    * don't block on non-blocking I/O
    }
   if (fp^.f_flag and FNONBLOCK)<>0 then
   begin
    error:=EAGAIN;
    pipeunlock(wpipe);
    break;
   end;

   {
    * We have no more space and have something to offer,
    * wake up select/poll.
    }
   pipeselwakeup(wpipe);

   wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_WANTW;
   pipeunlock(wpipe);

   error:=msleep(wpipe, PIPE_MTX(rpipe), PRIBIO or PCATCH, 'pipewr', 0);

   if (error<>0) then
   begin
    break;
   end;
  end;
 end;

 pipelock(wpipe, 0);
 Dec(wpipe^.pipe_busy);

 if (wpipe^.pipe_busy=0) and ((wpipe^.pipe_state and PIPE_WANT)<>0) then
 begin
  wpipe^.pipe_state:=wpipe^.pipe_state and (not (PIPE_WANT or PIPE_WANTR));
  wakeup(wpipe);
 end else
 if (wpipe^.pipe_buffer.cnt > 0) then
 begin
  {
   * If we have put any characters in the buffer, we wake up
   * the reader.
   }
  if (wpipe^.pipe_state and PIPE_WANTR)<>0 then
  begin
   wpipe^.pipe_state:=wpipe^.pipe_state and (not PIPE_WANTR);
   wakeup(wpipe);
  end;
 end;

 {
  * Don't Exit EPIPE if I/O was successful
  }
 if (wpipe^.pipe_buffer.cnt=0) and
    (uio^.uio_resid=0) and
    (error=EPIPE) then
 begin
  error:=0;
 end;

 if (error=0) then
 begin
  vfs_timestamp(@wpipe^.pipe_mtime);
 end;

 {
  * We have something to offer,
  * wake up select/poll.
  }
 if (wpipe^.pipe_buffer.cnt<>0) then
 begin
  pipeselwakeup(wpipe);
 end;

 pipeunlock(wpipe);
 PIPE_UNLOCK(rpipe);
 Exit(error);
end;

{ ARGSUSED }
function pipe_truncate(fp:p_file;length:Int64):Integer;
begin
 Exit(EINVAL);
end;

{
 * we implement a very minimal set of ioctls for compatibility with sockets.
 }
function pipe_ioctl(fp:p_file;cmd:QWORD;data:Pointer):Integer;
label
 out_unlocked;
var
 mpipe:p_pipe;
 error:Integer;
begin
 mpipe:=fp^.f_data;

 PIPE_LOCK(mpipe);

 error:=0;
 case cmd of

  FIONBIO:;

  FIOASYNC:
   if (PInteger(data)^<>0) then
   begin
    mpipe^.pipe_state:=mpipe^.pipe_state or PIPE_ASYNC;
   end else
   begin
    mpipe^.pipe_state:=mpipe^.pipe_state and (not PIPE_ASYNC);
   end;

  FIONREAD:
   if (mpipe^.pipe_state and PIPE_DIRECTW)<>0 then
   begin
    PInteger(data)^:=mpipe^.pipe_map.cnt;
   end else
   begin
    PInteger(data)^:=mpipe^.pipe_buffer.cnt;
   end;

  FIOSETOWN:
   begin
    PIPE_UNLOCK(mpipe);
    error:=0;
    //error:=fsetown(PInteger(data)^, @mpipe^.pipe_sigio);
    goto out_unlocked;
   end;

  FIOGETOWN:
   PInteger(data)^:=0;
   //PInteger(data)^:=fgetown(@mpipe^.pipe_sigio);

  { This is deprecated, FIOSETOWN should be used instead. }
  TIOCSPGRP:
   begin
    PIPE_UNLOCK(mpipe);
    error:=0;
    //error:=fsetown(-PInteger(data)^, @mpipe^.pipe_sigio);
    goto out_unlocked;
   end;

  { This is deprecated, FIOGETOWN should be used instead. }
  TIOCGPGRP:
   PInteger(data)^:=0;
   //PInteger(data)^:=-fgetown(@mpipe^.pipe_sigio);

  else
   error:=ENOTTY;
 end;

 PIPE_UNLOCK(mpipe);

out_unlocked:
 Exit(error);
end;

function pipe_poll(fp:p_file;events:Integer):Integer;
var
 rpipe:p_pipe;
 wpipe:p_pipe;
 revents:Integer;
begin
 rpipe  :=fp^.f_data;
 revents:=0;

 wpipe:=rpipe^.pipe_peer;
 PIPE_LOCK(rpipe);

 if (events and (POLLIN or POLLRDNORM))<>0 then
  if ((rpipe^.pipe_state and PIPE_DIRECTW)<>0) or
     (rpipe^.pipe_buffer.cnt > 0) then
  begin
   revents:=revents or events and (POLLIN or POLLRDNORM);
  end;

 if (events and (POLLOUT or POLLWRNORM))<>0 then
  if (wpipe^.pipe_present<>PIPE_ACTIVE) or
      ((wpipe^.pipe_state and PIPE_EOF)<>0) or
      (
       ((wpipe^.pipe_state and PIPE_DIRECTW)=0) and
       (
        ((wpipe^.pipe_buffer.size - wpipe^.pipe_buffer.cnt) >= PIPE_BUF) or
        (wpipe^.pipe_buffer.size=0)
       )
      ) then
  begin
   revents:=revents or events and (POLLOUT or POLLWRNORM);
  end;

 if ((events and POLLINIGNEOF)=0) then
 begin
  if (rpipe^.pipe_state and PIPE_EOF)<>0 then
  begin
   revents:=revents or (events and (POLLIN or POLLRDNORM));
   if (wpipe^.pipe_present<>PIPE_ACTIVE) or
      ((wpipe^.pipe_state and PIPE_EOF)<>0) then
   begin
    revents:=revents or POLLHUP;
   end;
  end;
 end;

 if (revents=0) then
 begin
  if (events and (POLLIN or POLLRDNORM))<>0 then
  begin
   selrecord(curkthread, @rpipe^.pipe_sel);
   if (SEL_WAITING(@rpipe^.pipe_sel)) then
   begin
    rpipe^.pipe_state:=rpipe^.pipe_state or PIPE_SEL;
   end;
  end;

  if (events and (POLLOUT or POLLWRNORM))<>0 then
  begin
   selrecord(curkthread, @wpipe^.pipe_sel);
   if SEL_WAITING(@wpipe^.pipe_sel) then
   begin
    wpipe^.pipe_state:=wpipe^.pipe_state or PIPE_SEL;
   end;
  end;
 end;

 PIPE_UNLOCK(rpipe);

 Exit(revents);
end;

{
 * We shouldn't need locks here as we're doing a read and this should
 * be a natural race.
 }
function pipe_stat(fp:p_file;ub:p_stat):Integer;
var
 pipe:p_pipe;
 new_unr:Integer;
begin
 pipe:=fp^.f_data;
 PIPE_LOCK(pipe);

 {
  * Lazily allocate an inode number for the pipe.  Most pipe
  * users do not call fstat(2) on the pipe, which means that
  * postponing the inode allocation until it is must be
  * Exited to userland is useful.  If alloc_unr failed,
  * assign st_ino zero instead of Exiting an error.
  * Special pipe_ino values:
  *  -1 - not yet initialized;
  *  0  - alloc_unr failed, Exit 0 as st_ino forever.
  }
 if (pipe^.pipe_ino=ino_t(-1)) then
 begin
  new_unr:=alloc_unr(pipeino_unr);
  if (new_unr<>-1) then
  begin
   pipe^.pipe_ino:=new_unr;
  end else
  begin
   pipe^.pipe_ino:=0;
  end;
 end;

 PIPE_UNLOCK(pipe);

 ub^:=Default(t_stat);

 ub^.st_mode:=S_IFIFO;
 ub^.st_blksize:=PAGE_SIZE;

 if (pipe^.pipe_state and PIPE_DIRECTW)<>0 then
 begin
  ub^.st_size:=pipe^.pipe_map.cnt;
 end else
 begin
  ub^.st_size:=pipe^.pipe_buffer.cnt;
 end;

 ub^.st_blocks:=(ub^.st_size + ub^.st_blksize - 1) div ub^.st_blksize;
 ub^.st_atim  :=pipe^.pipe_atime;
 ub^.st_mtim  :=pipe^.pipe_mtime;
 ub^.st_ctim  :=pipe^.pipe_ctime;
 ub^.st_uid   :=0; //fp^.f_cred^.cr_uid;
 ub^.st_gid   :=0; //fp^.f_cred^.cr_gid;
 ub^.st_dev   :=pipedev_ino;
 ub^.st_ino   :=pipe^.pipe_ino;
 {
  * Left as 0: st_nlink, st_rdev, st_flags, st_gen.
  }
 Exit(0);
end;

{ ARGSUSED }

function pipe_close(fp:p_file):Integer;
var
 cpipe:p_pipe;
begin
 cpipe:=fp^.f_data;

 fp^.f_ops:=@badfileops;
 fp^.f_data:=nil;
 //funsetown(@cpipe^.pipe_sigio);
 pipeclose(cpipe);
 Exit(0);
end;

procedure pipe_free_kmem(cpipe:p_pipe);
begin
 Assert(not mtx_owned(PIPE_MTX(cpipe)^), 'pipe_free_kmem: pipe mutex locked');

 if (cpipe^.pipe_buffer.buffer<>nil) then
 begin
  System.InterlockedExchangeAdd64(amountpipekva, -cpipe^.pipe_buffer.size);

  kmem_free(cpipe^.pipe_buffer.buffer, cpipe^.pipe_buffer.size);

  //vm_map_remove(pipe_map,
  //              cpipe^.pipe_buffer.buffer,
  //              cpipe^.pipe_buffer.buffer + cpipe^.pipe_buffer.size);

  cpipe^.pipe_buffer.buffer:=nil;
 end;
{$ifndef PIPE_NODIRECT}
 begin
  cpipe^.pipe_map.cnt:=0;
  cpipe^.pipe_map.pos:=0;
  cpipe^.pipe_map.npages:=0;
 end;
{$endif}
end;

{
 * shutdown the pipe
 }
procedure pipeclose(cpipe:p_pipe);
var
 pp:p_pipepair;
 ppipe:p_pipe;
 ino:ino_t;
begin
 Assert(cpipe<>nil, ('pipeclose: cpipe=nil'));

 PIPE_LOCK(cpipe);
 pipelock(cpipe, 0);
 pp:=cpipe^.pipe_pair;

 pipeselwakeup(cpipe);

 {
  * If the other side is blocked, wake it up saying that
  * we want to close it down.
  }
 cpipe^.pipe_state:=cpipe^.pipe_state or PIPE_EOF;
 while (cpipe^.pipe_busy<>0) do
 begin
  wakeup(cpipe);
  cpipe^.pipe_state:=cpipe^.pipe_state or PIPE_WANT;
  pipeunlock(cpipe);
  msleep(cpipe, PIPE_MTX(cpipe), PRIBIO, 'pipecl', 0);
  pipelock(cpipe, 0);
 end;

 {
  * Disconnect from peer, if any.
  }
 ppipe:=cpipe^.pipe_peer;
 if (ppipe^.pipe_present=PIPE_ACTIVE) then
 begin
  pipeselwakeup(ppipe);

  ppipe^.pipe_state:=ppipe^.pipe_state or PIPE_EOF;
  wakeup(ppipe);
  KNOTE_LOCKED(@ppipe^.pipe_sel.si_note, 0);
 end;

 {
  * Mark this endpoint as free.  Release kmem resources.  We
  * don't mark this endpoint as unused until we've finished
  * doing that, or the pipe might disappear out from under
  * us.
  }
 PIPE_UNLOCK(cpipe);
 pipe_free_kmem(cpipe);
 PIPE_LOCK(cpipe);
 cpipe^.pipe_present:=PIPE_CLOSING;
 pipeunlock(cpipe);

 {
  * knlist_clear() may sleep dropping the PIPE_MTX. Set the
  * PIPE_FINALIZED, that allows other end to free the
  * pipe_pair, only after the knotes are completely dismantled.
  }
 knlist_clear(@cpipe^.pipe_sel.si_note, 1);
 cpipe^.pipe_present:=PIPE_FINALIZED;
 seldrain(@cpipe^.pipe_sel);
 knlist_destroy(@cpipe^.pipe_sel.si_note);

 {
  * Postpone the destroy of the fake inode number allocated for
  * our end, until pipe mtx is unlocked.
  }
 ino:=cpipe^.pipe_ino;

 {
  * If both endpoints are now closed, release the memory for the
  * pipe pair.  If not, unlock.
  }
 if (ppipe^.pipe_present=PIPE_FINALIZED) then
 begin
  PIPE_UNLOCK(cpipe);

  pp:=cpipe^.pipe_pair;
  pipe_zone_fini(pp);
  FreeMem(pp);
 end else
 begin
  PIPE_UNLOCK(cpipe);
 end;

 if (ino<>0) and (ino<>ino_t(-1)) then
 begin
  free_unr(pipeino_unr, ino);
 end;
end;

{ARGSUSED}
function pipe_kqfilter(fp:p_file;kn:p_knote):Integer;
var
 cpipe:p_pipe;
begin
 cpipe:=p_file(kn^.kn_fp)^.f_data;

 PIPE_LOCK(cpipe);

 case kn^.kn_filter of
  EVFILT_READ:
   begin
    kn^.kn_fop:=@pipe_rfiltops;
   end;
  EVFILT_WRITE:
   begin
    kn^.kn_fop:=@pipe_wfiltops;
    if (cpipe^.pipe_peer^.pipe_present<>PIPE_ACTIVE) then
    begin
     { other end of pipe has been closed }
     PIPE_UNLOCK(cpipe);
     Exit(EPIPE);
    end;
    cpipe:=cpipe^.pipe_peer;
   end;
  else
   begin
    PIPE_UNLOCK(cpipe);
    Exit(EINVAL);
   end;
 end;

 knlist_add(@cpipe^.pipe_sel.si_note, kn, 1);

 PIPE_UNLOCK(cpipe);

 Exit(0);
end;

procedure filt_pipedetach(kn:p_knote);
var
 cpipe:p_pipe;
begin
 cpipe:=p_file(kn^.kn_fp)^.f_data;

 PIPE_LOCK(cpipe);

 if (kn^.kn_filter=EVFILT_WRITE) then
 begin
  cpipe:=cpipe^.pipe_peer;
 end;

 knlist_remove(@cpipe^.pipe_sel.si_note, kn, 1);
 PIPE_UNLOCK(cpipe);
end;

{ARGSUSED}
function filt_piperead(kn:p_knote;hint:QWORD):Integer;
var
 rpipe:p_pipe;
 wpipe:p_pipe;
 ret:Integer;
begin
 rpipe:=p_file(kn^.kn_fp)^.f_data;
 wpipe:=rpipe^.pipe_peer;

 PIPE_LOCK(rpipe);

 kn^.kn_data:=rpipe^.pipe_buffer.cnt;

 if (kn^.kn_data=0) and ((rpipe^.pipe_state and PIPE_DIRECTW)<>0) then
 begin
  kn^.kn_data:=rpipe^.pipe_map.cnt;
 end;

 if ((rpipe^.pipe_state and PIPE_EOF)<>0) or
    (wpipe^.pipe_present<>PIPE_ACTIVE) or
    ((wpipe^.pipe_state and PIPE_EOF)<>0) then
 begin
  kn^.kn_flags:=kn^.kn_flags or EV_EOF;
  PIPE_UNLOCK(rpipe);
  Exit(1);
 end;

 ret:=ord(kn^.kn_data > 0);

 PIPE_UNLOCK(rpipe);
 Exit(ret);
end;

{ARGSUSED}
function filt_pipewrite(kn:p_knote;hint:QWORD):Integer;
var
 rpipe:p_pipe;
 wpipe:p_pipe;
 ret:Integer;
begin
 rpipe:=p_file(kn^.kn_fp)^.f_data;
 wpipe:=rpipe^.pipe_peer;

 PIPE_LOCK(rpipe);

 if (wpipe^.pipe_present<>PIPE_ACTIVE) or
    ((wpipe^.pipe_state and PIPE_EOF)<>0) then
 begin
  kn^.kn_data :=0;
  kn^.kn_flags:=kn^.kn_flags or EV_EOF;
  PIPE_UNLOCK(rpipe);
  Exit(1);
 end;

 if (wpipe^.pipe_buffer.size > 0) then
 begin
  kn^.kn_data:=(wpipe^.pipe_buffer.size - wpipe^.pipe_buffer.cnt);
 end else
 begin
  kn^.kn_data:=PIPE_BUF;
 end;

 if (wpipe^.pipe_state and PIPE_DIRECTW)<>0 then
 begin
  kn^.kn_data:=0;
 end;

 ret:=ord(kn^.kn_data >= PIPE_BUF);

 PIPE_UNLOCK(rpipe);

 Exit(ret);
end;



end.

