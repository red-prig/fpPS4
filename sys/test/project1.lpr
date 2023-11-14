
uses
 windows,
 dateutils,
 atomic,
 ntapi,
 mqueue,
 syscalls,
 signal,
 ucontext,
 _umtx,
 sys_umtx,
 time,
 kern_time,
 md_time,
 thr,
 kern_thread,
 md_thread,
 kern_rwlock,
 thr_private,
 sys_cpuset,
 trap,
 kern_psl,
 kern_umtx,
 thr_init,
 thr_error,
 pthread_md,
 sysutils,
 errno,
 md_context,
 subr_sleepqueue,
 kern_thr,
 kern_condvar,
 kern_osem,
 kern_id,
 kern_evf,
 rtprio,
 pthread,
 thr_stack,
 sys_mmap,
 kern_synch,
 murmurhash,
 hamt,
 vfs_subr,
 vfs_mount,
 vfs_default,
 init_sysent,
 vfs_syscalls,
 vsys_generic,
 vsocket,
 vsocketvar,
 vnode_if,
 sys_sysinit,
 sys_fnmatch,
 dead_vnops,
 devfs,
 devfs_devs,
 devfs_rule,
 devfs_vfsops,
 devfs_vnops,
 vfs_mountroot,
 vstat,
 vfcntl,
 vdirent,
 fdesc_vfsops,
 fdesc_vnops,
 fdescfs,
 kern_descrip,
 vnode,
 nullfs,
 null_subr,
 null_vnops,
 null_vfsops,
 ufs,
 vmount,
 kern_prot,
 sys_resource,
 kern_resource,
 md_proc,
 kern_ksched,
 kern_uuid,
 kern_gpo,
 md_sleep,
 sys_machdep,
 kern_context,
 kern_namedobj,
 sys_event,
 sys_eventvar,
 kern_event,
 kern_callout,
 kern_timeout,
 kern_exec,
 kern_dynlib,
 vmparam,
 kern_sysctl,
 kern_budget,
 kern_regmgr,
 kern_authinfo,
 kern_dmem,
 kern_blockpool,
 kern_bnet,
 uipc_syscalls,
 kern_ipmimgr,
 kern_mdbg,
 md_exception,
 systm,
 dev_tty,
 ps4_libSceSystemService,
 ps4_libSceIpmi,
 ps4_libSceDialogs,
 ps4_libSceAvSetting;

var
 mtx:umutex;
 rwl:urwlock;
 e:Integer;

 event:Thandle;

 //osem:Integer;
 evf:Integer;

procedure trap_test;
var
 td:p_kthread;
begin
 td:=curkthread;

 Writeln('trap_test: ',' curkthread:',HexStr(curkthread),' sptr:',HexStr(sptr),' ',HexStr(td^.td_frame.tf_rip,16));
end;

function _thread(parameter:pointer):ptrint;
var
 td:p_kthread;
 p:Pointer;
 qr:t_query_memory_prot;
begin
 Result:=0;
 NtWaitForSingleObject(event,false,nil);

 td:=thread_alloc;

 td^.td_tid   :=GetCurrentThreadId;
 td^.td_handle:=GetCurrentThread;
 td^.td_ref   :=1;

 //sched_priority(@td,700);

 BaseQueryInfo(td);

 set_curkthread(td);


 p:=mmap(Pointer($700000000),16*1024,PROT_CPU_ALL,MAP_VOID or MAP_FIXED,-1,0);
 Writeln(HexStr(p));

 p:=mmap(Pointer($700000000),16*1024,PROT_CPU_ALL,MAP_ANON or MAP_FIXED,-1,0);
 Writeln(HexStr(p));

 Result:=query_memory_protection(Pointer($700000000),@qr);
 Writeln(Result);

 sceKernelSetVirtualRangeName(Pointer($700000000),16*1024,'test');

 p:=mmap(Pointer($700000000+16*1024),16*1024,PROT_CPU_ALL,MAP_ANON {or MAP_VOID} or MAP_FIXED,-1,0);
 Writeln(HexStr(p));

 Result:=madvise(Pointer($00700000000),4*1024,MADV_FREE);
 Writeln(Result);

 Result:=madvise(Pointer($00700000000),4*1024,MADV_WILLNEED);
 Writeln(Result);

 Result:=munmap(Pointer($700000000),16*1024*2);
 Writeln(Result);

 e:=_umtx_op(nil,UMTX_OP_RW_WRLOCK,0,nil,nil);
 Writeln('  e=',e);

 repeat

 //Writeln('before: sptr:',HexStr(sptr));
 //asm
 // Movq trap_test,%rax
 // call fast_syscall
 //end;
 //Writeln('after:  sptr:',HexStr(sptr));

 //e:=_umtx_op(@mtx,{UMTX_OP_MUTEX_LOCK} UMTX_OP_LOCK,td.td_tid,nil,nil);
 //e:=_umtx_op(@mtx,UMTX_OP_MUTEX_LOCK,td^.td_tid,nil,nil);
 e:=_umtx_op(@rwl,UMTX_OP_RW_WRLOCK,0,nil,nil);
 Writeln('  lock[',GetCurrentThreadId,'] ',e);

 //e:=_do_lock_normal(GetCurrentThreadId,@mtx,0,NT_INFINITE,0);
 //Writeln('  lock[',GetCurrentThreadId,'] ',e);

 //sleep(100);

 //Writeln('before: sptr:',HexStr(sptr));

 //e:=_umtx_op(@mtx,{UMTX_OP_MUTEX_UNLOCK} UMTX_OP_UNLOCK,td.td_tid,nil,nil);
 //e:=_umtx_op(@mtx,UMTX_OP_MUTEX_UNLOCK,td^.td_tid,nil,nil);
 e:=_umtx_op(@rwl,UMTX_OP_RW_UNLOCK,0,nil,nil);
 Writeln('unlock[',GetCurrentThreadId,'] ',e);

 //Writeln('after:  sptr:',HexStr(sptr));

 //sleep(1000)

 //_umtx_obj_done(@mtx);

 until false;
end;

var
 mem,mem2:Pointer;
 mseg:Pointer;

 lock:Integer;

procedure _writefsbase_u64(base:Pointer);
begin
 asm
  Mov base,%rax

  //f3 48 0f ae d0   WRFSBASE RAX
  .byte 0xF3
  .byte 0x48
  .byte 0x0F
  .byte 0xAE
  .byte 0xD0
 end;
end;

function _readfsbase_u64:Pointer;
begin
 asm
  push %rsi

  //f3 48 0f ae c6   RDFSBASE RSI
  .byte 0xF3
  .byte 0x48
  .byte 0x0F
  .byte 0xAE
  .byte 0xC6

  Mov %rsi,Result

  pop %rsi
 end;
end;

procedure SetTlsBase(p:Pointer); assembler; nostackframe;
asm
 mov %rcx,%gs:(0x708)
end;

function GetTlsBase:Pointer; assembler; nostackframe;
asm
 mov %gs:(0x708),%rax
end;

function Get_SEH:Pointer; assembler; nostackframe;
asm
 mov %gs:(0),%rax
end;

threadvar
 intr:Integer;

procedure __ex_handler(sig,code:Integer;ctx:p_ucontext_t); SysV_ABI_CDecl;
begin
 intr:=1;
 Writeln('__ex_handler:',sig,' ',code);
end;

var
 tid,tid2:QWORD;

var
 xmm0:array[0..15] of Byte=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);
 xmm0_ptr:Pointer=@xmm0;

 ymm0:array[0..31] of Byte=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);
 ymm0_ptr:Pointer=@ymm0;

function ts_to_str(ts:timespec):RawByteString;
var
 D:TDateTime;
begin
 D:=UnixToDateTime(ts.tv_sec);
 D:=UniversalTimeToLocal(D);
 Result:=DateTimeToStr(D);
end;

function IncludeUnixTrailing(Const Path:RawByteString):RawByteString;
Var
 L:Integer;
begin
 Result:=Path;
 L:=Length(Result);
 If (L=0) or (Path[L]<>'/') then
 begin
  SetLength(Result,L+1);
  Result[L+1]:='/';
 end;
end;

procedure test_files;
var
 td:p_kthread;
 fs:t_statfs;
 fd_1,fd_2:Integer;
 i,err:Integer;

 buf:PChar;

 argv0:PChar;
begin
 td:=curkthread;

 {
 Writeln('sys_open=',sys_open('/app0/test.txt',O_RDWR or O_CREAT or O_TRUNC{ or O_APPEND} or O_EXLOCK,&777));
 fd_1:=td^.td_retval[0];

 buf:=AllocMem(64*1024);

 FillChar(buf^,64*1024,'0');

 Writeln('sys_pwrite=',sys_pwrite(fd_1,buf,64*1024,0));

 FillChar(buf^,64*1024,'1');

 Writeln('sys_pwrite=',sys_pwrite(fd_1,buf,64*1024,0));

 FillChar(buf^,64*1024,0);

 Writeln('sys_pread=',sys_pread(fd_1,buf,64*1024,0));

 FreeMem(buf);

 //Writeln('sys_fstatfs=',sys_fstatfs(fd_1,@fs));

 Writeln('sys_fsync=',sys_fsync(fd_1));
 Writeln('sys_fdatasync=',sys_fdatasync(fd_1));

 Writeln('sys_open=',sys_open('/app0/test.txt',O_RDWR{ or O_CREAT},&777));
 fd_2:=td^.td_retval[0];

 Writeln('sys_close=',sys_close(fd_2));
 Writeln('sys_close=',sys_close(fd_1));

 Writeln('sys_mkdir=',sys_mkdir('/test',&777));
 Writeln('sys_mkdir=',sys_mkdir('/test/test',&777));
 Writeln('sys_rmdir=',sys_rmdir('/test/test'));
 Writeln('sys_symlink=',sys_symlink('/app0','/test/test2'));
 Writeln('sys_unlink=',sys_unlink('/test/test2'));

 Writeln('sys_mkdir=',sys_mkdir('/app0/new',&777));
 Writeln('sys_link=',sys_link('/app0/test.txt','/app0/new/test_link.txt'));
 Writeln('sys_rename=',sys_rename('/app0/new/test_link.txt','/app0/renamed'));
 Writeln('sys_unlink=',sys_unlink('/app0/renamed'));

 Writeln('sys_rename=',sys_rename('/app0/new','/app0/renamed'));
 Writeln('sys_rmdir=',sys_rmdir('/app0/renamed'));

 Writeln('sys_unlink=',sys_unlink('/app0/test.txt'));

 Writeln('sys_rmdir=',sys_rmdir('/test'));
 }

 //readln;

 For i:=0 to High(deci_tty) do
 begin
  deci_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  deci_tty[i].t_wr_handle:=GetStdHandle(STD_OUTPUT_HANDLE);
 end;

                      //fs  guest     host
 err:=vfs_mount_mkdir('ufs','/app0'  ,'/'      ,nil,0);
 err:=vfs_mount_mkdir('ufs','/system','/system',nil,0);

 err:=vfs_unmount_rmdir('/app0'  ,0);
 err:=vfs_unmount_rmdir('/system',0);

 err:=vfs_mount_mkdir('ufs','/app0'  ,'/'      ,nil,0);
 err:=vfs_mount_mkdir('ufs','/system','/system',nil,0);

 //argv0:='/app0/basic-sample_debug.elf';
 //argv0:='/app0/simple.elf';
 //argv0:='/app0/videoout_basic.elf';
 argv0:='/app0/videoout_cursor.elf';

 //argv0:='/app0/scene2.bin';
 //argv0:='/app0/basic_quad_debug.elf';


 err:=_execve(argv0,@argv0,nil);
end;

procedure test_dirs(const dirp,namep:RawByteString;s:Byte);
label
 _next;
var
 td:p_kthread;

 sb:t_stat;
 buf:array[0..511] of Byte;
 dir:p_dirent;
 fd:Integer;
 err:Integer;
 c:Integer;
begin
 td:=curkthread;

 case RawByteString(namep) of
  '.',
  '..':
    begin
     Writeln(Space(s),namep:20,' |');
     Exit;
    end;
  else;
 end;

 err:=sys_lstat(PChar(dirp+namep),@sb);
 //err:=sys_stat(PChar(dirp+namep),@sb);

 if (err<>0) then
 begin
  Writeln(Space(s),namep:20,' | (',err,')');
 end else
 begin
  Write(Space(s),namep:20,' | ',ts_to_str(sb.st_birthtim {st_mtim}):19,' |');
  _next:

  if ((sb.st_mode and S_IFDIR)<>0) then
  begin
   Write(' DIR');
   Assert(sb.st_size=512);

   err:=sys_open(PChar(dirp+namep),O_RDONLY or O_DIRECTORY,0);

   if (err<>0) then
   begin
    Write(' | (',err,')');
    Exit;
   end else
   begin
    Writeln;
   end;

   fd:=td^.td_retval[0];

   Writeln(Space(s),'->');

   repeat
    FillChar(buf,512,0);
    dir:=@buf;
    err:=sys_getdents(fd,dir,512);
    if (err<0) then Break;

    c:=td^.td_retval[0];
    if (c=0) then Break;

    while (dir<(@buf+c)) do
    begin
     test_dirs(IncludeUnixTrailing(dirp+namep),RawByteString(dir^.d_name),s+2);

     PByte(dir):=PByte(dir)+dir^.d_reclen;
    end;

   until false;

   sys_close(fd);

   Writeln(Space(s),'<-');
  end else
  begin
   Case (sb.st_mode and S_IFMT) of
    S_IFIFO :Write(' IFO');
    S_IFCHR :Write(' CHR');
    S_IFDIR :Write(' DIR');
    S_IFBLK :Write(' BLK');
    S_IFREG :Write(' REG');
    S_IFLNK :Write(' LNK');
    S_IFSOCK:Write(' SCK');
    else
             Write(' ',(sb.st_mode and S_IFMT));
   end;

   Writeln(' | ',sb.st_size);
  end;
 end;
end;

procedure _timerexpire(arg:Pointer); sysv_abi_default;
var
 calloutp:p_callout;
begin
 calloutp:=arg;

 writeln('_timerexpire');

 callout_reset_curcpu(calloutp, 1000*1000*UNIT_PER_USEC-1, @_timerexpire, calloutp);
 //callout_drain(calloutp);
end;


{
Type
  TBacktraceStrFunc = Function (Addr: CodePointer): ShortString;
  TErrorProc = Procedure (ErrNo : Longint; Address : CodePointer; Frame : Pointer);
  TAbstractErrorProc = Procedure;
  TAssertErrorProc = Procedure(const msg,fname:ShortString;lineno:longint;erroraddr:pointer);
  TSafeCallErrorProc = Procedure(error : HResult;addr : pointer);


const
  BacktraceStrFunc  : TBacktraceStrFunc = @SysBacktraceStr;
  ErrorProc         : TErrorProc = nil;
  AbstractErrorProc : TAbstractErrorProc = nil;
  AssertErrorProc   : TAssertErrorProc = @SysAssert;
  SafeCallErrorProc : TSafeCallErrorProc = nil;
}

procedure test_thread; sysv_abi_default;
var
 rax:qword;
 act:sigaction_t;
 _sig:Integer;
 oset:sigset_t;
 i,t:Integer;

 uctx:ucontext_t;

 calloutp:p_callout;
begin

 writeln('Get_SEH:0x',HexStr(Get_SEH));

 //writeln('copyin:',copyin(mem2+64*1024*4-(sizeof(ucontext_t) div 2),@uctx,sizeof(ucontext_t)));

 //PPointer(nil)^:=nil;

 //Assert(false,'test assert');

 //if (tid<>curkthread^.td_tid) then
 //begin
 // calloutp:=AllocMem(SizeOf(t_callout));
 // callout_init(calloutp, CALLOUT_MPSAFE);
 // callout_reset_curcpu(calloutp, 1000*1000*UNIT_PER_USEC, @_timerexpire, calloutp);
 //end;

 //SetTlsBase(Pointer(qword(1)));

 if (tid<>curkthread^.td_tid) then
 begin
  test_files;
  Writeln('[--test_dirs--]');
  test_dirs('','/',1);
  Writeln('[--test_dirs--]');

  //readln;

  tid2:=curkthread^.td_tid;

  evf:=evf_create('evf test',EVF_ATTR_TH_PRIO,0);
  Writeln('evf=',evf,' _errno:',__error^);

  //osem:=osem_create('osem test',1,1,10);
  //Writeln('osem=',osem,' _errno:',__error^);

  act:=Default(sigaction_t);
  act.u.sa_handler:=sa_handler(@__ex_handler);
  act.sa_flags:=SA_RESTART;

  _sigaction(SIGUSR1,@act,nil);


  i:=syscalls.thr_suspend_ucontext(tid);
  Writeln('thr_suspend_ucontext:',i);

  i:=syscalls.thr_get_ucontext(tid,@uctx);
  Writeln('thr_get_ucontext:',i);

  i:=syscalls.thr_resume_ucontext(tid);
  Writeln('thr_resume_ucontext:',i);

  thr_kill(tid,SIGUSR1);
  //thr_wake(tid);

  thr_kill(tid,SIGUSR1);

  i:=evf_trywait(evf,1,EVF_WAITMODE_OR,nil);
  Writeln('_evf_trywait_err=',i,' _errno:',__error^);

  i:=evf_wait(evf,2,EVF_WAITMODE_OR,nil,nil);
  Writeln('_evf_wait_err=',i,' _errno:',__error^);

  //i:=_osem_wait_err(osem,1,nil);
  //Writeln('_osem_wait_err=',i,' _errno:',__error^);
  //
  //t:=400;
  //i:=_osem_wait_err(osem,1,nil);
  //Writeln('_osem_wait_err=',i,' _errno:',__error^);
  //
  //i:=_osem_delete_err(osem);
  //Writeln('_osem_delete_err=',i,' _errno:',__error^);

  writeln;
 end else
 begin
  //Writeln('thr_suspend:',thr_suspend(nil));

  {
  oset.qwords[0]:=QWORD(-1);
  oset.qwords[1]:=QWORD(-1);
  Writeln('sigwait:',sigwait(@oset,@_sig));
  Writeln('intr:',_sig);
  }

  Writeln('before: sptr:',HexStr(sptr));

  asm
   movqq xmm0_ptr,%rax
   movdqu (%rax),%xmm0

   movqq ymm0_ptr,%rax
   vmovdqu (%rax),%ymm0
  end;

  syscalls.getcontext(@uctx);

  oset.qwords[0]:=QWORD(-1);
  oset.qwords[1]:=QWORD(-1);
  Writeln('sigwait:',sigwait(@oset,@_sig));
  Writeln('_errno:',__error^);

  //repeat
  // asm
  //  pause
  // end;
  //until (intr<>0);

  asm
   movqq xmm0_ptr,%rax
   movdqu %xmm0,(%rax)

   movqq ymm0_ptr,%rax
   vmovdqu %ymm0,(%rax)
  end;

  Writeln('intr');
  Writeln('after: sptr:',HexStr(sptr));
 end;

 sleep(500);
 //_osem_post_err(osem,1);
 thr_kill(tid2,SIGUSR1);

 i:=evf_set(evf,2);

 i:=evf_set(evf,1);
 Writeln('_evf_set_err=',i,' _errno:',__error^);

 //_osem_post_err(osem,1);

 sig_lock;
 sig_lock;
  sleep(1);
 sig_unlock;
 sig_unlock;

 {
 rax:=0;
 asm
  mov %gs:(0x1C0),%rax
 end;
 Writeln('SpareBytes1[0]:',HexStr(rax,16));

 rax:=0;
 asm
  mov %gs:(0x1C8),%rax
 end;
 Writeln('SpareBytes1[1]:',HexStr(rax,16));

 rax:=0;
 asm
  mov %gs:(0x1D0),%rax
 end;
 Writeln('SpareBytes1[2]:',HexStr(rax,16));

 Writeln('GetTlsBase:',HexStr(GetTlsBase));

 rax:=0;
 asm
  mov %gs:(0x10),%rax
  mov %rax,rax
 end;
 Writeln('FiberData:',HexStr(rax,16));

 rax:=0;
 asm
  mov %gs:(0x14),%rax
  mov %rax,rax
 end;
 Writeln('ArbitraryData:',HexStr(rax,16));

 rax:=0;
 asm
  mov %gs:(0xC0),%rax
  mov %rax,rax
 end;
 Writeln('Wow64:',HexStr(rax,16));


 rax:=0;
 asm
  mov %gs:(0x1A8),%rax
  mov %rax,rax
 end;
 Writeln('ActivationContextStack:',HexStr(rax,16));



 rax:=0;
 asm
  mov %gs:(0x700),%rax
  mov %rax,rax
 end;
 Writeln('UserData[0]:',HexStr(rax,16));

 rax:=0;
 asm
  mov %gs:(0x708),%rax
  mov %rax,rax
 end;
 Writeln('UserData[1]:',HexStr(rax,16));


 rax:=0;
 asm
  mov %gs:(0x710),%eax
  mov %rax,rax
 end;
 Writeln('UserData[2]:',HexStr(rax,8));

 rax:=0;
 asm
  mov %gs:(0xE0C),%rax
  mov %rax,rax
 end;
 Writeln('DeallocationStack:',HexStr(rax,16));

 Writeln('mseg:',HexStr(mseg));

 FillChar(mseg^,64*1024,$11);

 PQWORD(mseg)^:=$0102030405060708;

 Writeln('_readfsbase_u64:',HexStr(_readfsbase_u64));

 _writefsbase_u64(mseg);

 //Writeln('_readfsbase_u64:',HexStr(_readfsbase_u64));

 //rax:=qword(mseg);

 asm
 // Mov rax,%rax
 //
 // .byte 0xF3
 // .byte 0x48
 // .byte 0x0F
 // .byte 0xAE
 // .byte 0xD0
 //
 // //F3 0F AE
 //
 //f3 48 0f ae d0 WRFSBASE RAX
 //
 // movq  %fs:(0),%rax

 //f3 48 0f ae d0       WRFSBASE RAX

 end;

 //sleep(1);

 //Writeln('_readfsbase_u64:',HexStr(_readfsbase_u64));

 //While (XCHG(lock,1)<>0) do spin_pause;

 rax:=0;

 asm
  movq  %fs:(0),%rax

  Mov %rax,rax
 end;

 writeln(HexStr(rax,16));
 writeln;
 }

 thr_exit(nil);
end;

function _VirtualQuery(addr:Pointer):Integer;
var
 Info:TMemoryBasicInformation;
begin
 Writeln('-----');


 Info:=Default(TMemoryBasicInformation);

 Result:=NtQueryVirtualMemory(
           NtCurrentProcess,
           @addr,
           MemoryBasicInformation,
           @Info,
           SizeOf(TMemoryBasicInformation),
           nil);

 //Result:=VirtualQuery(addr,Info,SizeOf(TMemoryBasicInformation));

 //if (Result=0) then
 //begin
 // Result:=GetLastError;
 //end else

 if (Result=0) then
 begin
  //Result:=0;

  Writeln('Q:BaseAddress   :',HexStr(Info.BaseAddress));
  Writeln('Q:AllocationBase:',HexStr(Info.AllocationBase));

  //AllocationProtect : DWORD;

  Writeln('Q:RegionSize    :',HexStr(Info.RegionSize,16));

  Write  ('Q:State         :');

  Case Info.State of
   MEM_COMMIT :Writeln('MEM_COMMIT ');
   MEM_RESERVE:Writeln('MEM_RESERVE');
   MEM_FREE   :Writeln('MEM_FREE   ');
  end;

  Write  ('Q:Protect       :');

  Case Info.Protect of
   0                     :Writeln('0');
   PAGE_NOACCESS         :Writeln('PAGE_NOACCESS         ');
   PAGE_READONLY         :Writeln('PAGE_READONLY         ');
   PAGE_READWRITE        :Writeln('PAGE_READWRITE        ');
   PAGE_EXECUTE          :Writeln('PAGE_EXECUTE          ');
   PAGE_EXECUTE_READ     :Writeln('PAGE_EXECUTE_READ     ');
   PAGE_EXECUTE_READWRITE:Writeln('PAGE_EXECUTE_READWRITE');
   else;
  end;

  Write  ('Q:Type          :');

  Case Info._Type of
   0          :Writeln('0');
   MEM_PRIVATE:Writeln('MEM_PRIVATE');
   MEM_MAPPED :Writeln('MEM_MAPPED ');
   MEM_IMAGE  :Writeln('MEM_IMAGE  ');
  end;

 end;

 Writeln('-----');
end;

const
 SECTION_MAP_EXECUTE=$8;

procedure test_map;
var
 F:THandle;
 n:Integer;
 SectionSize:LARGE_INTEGER;
 start:ULONG_PTR;
 CommitSize:ULONG_PTR;
 ViewSize:ULONG_PTR;
 hSection:THandle;
 Base1,Base2:Pointer;
 //dev:TVkDeviceMemory;
begin

 {
 Base1:=nil;
 ViewSize:=438*1024*1024*1024;

 n:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @Base1,
           0,
           @ViewSize,
           MEM_RESERVE,
           PAGE_NOACCESS
          );

 Writeln('NtAllocateVirtualMemory:',HexStr(n,8));
 Writeln('Base1:',HexStr(Base1));

 _VirtualQuery(Base1);

 readln;

 }

 F:=0; //page file

 F:=FileCreate('pagefile');

 FileTruncate(F,64*1024-10);

 SectionSize.QuadPart:={64*1024} {428*1024*1024*1024} 64*1024-10 { $FFFFFFFFFF};

 n:=NtCreateSection(
           @hSection,
           //{SECTION_EXTEND_SIZE or} SECTION_MAP_READ or SECTION_MAP_WRITE or SECTION_MAP_EXECUTE,
           //SECTION_EXTEND_SIZE or SECTION_ALL_ACCESS,
           SECTION_MAP_READ,
           nil,
           @SectionSize,
           //PAGE_READWRITE,
           PAGE_READONLY,
           //PAGE_EXECUTE_READWRITE,
           SEC_COMMIT,
           //SEC_RESERVE,
           F
          );

 Writeln('NtCreateSection:',HexStr(n,8));

 //0123456789ABCDEF0123456789ABCDEF01234567 16+16+7=39 39-11-28  48-39=9
 //1111111111111111111111111111111111111111
 //0000000000000000000000000001000000000000
 //                            BA9876543210

 //SectionSize.QuadPart:={64*1024} {428*1024*1024*1024}  $FFFFFFFFFF;
 //n:=NtExtendSection(
 //          hSection,
 //          @SectionSize
 //         );
 //
 //Writeln('NtExtendSection:',HexStr(n,8));

 Base1:=nil;
 Base2:=nil;
 ViewSize:={64*1024} 16*1024*1024*1024;
 start:=0;

 //

 CommitSize:=64*1024;
 ViewSize:=64*1024-10;

 n:=NtMapViewOfSection(hSection,
                       NtCurrentProcess,
                       @Base1,
                       0,
                       CommitSize,
                       nil,
                       @ViewSize,
                       ViewUnmap,
                       0
                       {MEM_RESERVE},
                       {PAGE_READWRITE}
                       PAGE_READONLY
                      );

 Writeln('NtMapViewOfSection:',HexStr(n,8));

 Writeln('Base1:',HexStr(Base1));

 _VirtualQuery(Base1);

 //

 ViewSize:=64*1024;

 n:=NtMapViewOfSection(hSection,
                       NtCurrentProcess,
                       @Base2,
                       0,
                       0,
                       nil,
                       @ViewSize,
                       ViewUnmap,
                       0
                       {MEM_RESERVE},
                       PAGE_READWRITE);

 Writeln('NtMapViewOfSection:',HexStr(n,8));

 Writeln('Base2:',HexStr(Base2));

 //

 ViewSize:=64*1024;

 n:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @Base1,
           0,
           @ViewSize,
           MEM_COMMIT,
           PAGE_READWRITE
          );

 Writeln('NtAllocateVirtualMemory:',HexStr(n,8));

 Writeln('Base1:',HexStr(Base1));

 _VirtualQuery(Base1);

 //

 ViewSize:=64*1024;

 n:=NtAllocateVirtualMemory(
           NtCurrentProcess,
           @Base2,
           0,
           @ViewSize,
           MEM_COMMIT,
           PAGE_READWRITE
          );

 Writeln('NtAllocateVirtualMemory:',HexStr(n,8));

 Writeln('Base2:',HexStr(Base2));

 //

 PQWORD(Base1)^:=12345;

 Writeln(PQWORD(Base2)^);

 //InitVulkan;
 //dev:=vkAllocHostPointer(Device.FHandle,ViewSize,0,Base1);
 //
 //n:=NtUnmapViewOfSection(
 //          NtCurrentProcess,
 //          Base1
 //         );
 //
 //Writeln('NtUnmapViewOfSection:',HexStr(n,8));

 //n:=NtFreeVirtualMemory(
 //          NtCurrentProcess,
 //          @Base1,
 //          @ViewSize,
 //          MEM_RESET
 //         );
 //
 //Writeln('NtFreeVirtualMemory:',HexStr(n,8));


 n:=NtProtectVirtualMemory(
           NtCurrentProcess,
           @Base1,
           @ViewSize,
           PAGE_NOACCESS,
           @start
          );

 Writeln('NtProtectVirtualMemory:',HexStr(n,8));

 Writeln('Base1:',HexStr(Base1));

 _VirtualQuery(Base1);

 Writeln(PQWORD(Base2)^);
 PQWORD(Base2)^:=6789;

end;

procedure id_test;
var
 table:t_id_desc_table;
 desc :t_id_desc;
 key  :Integer;
 res  :Boolean;

 procedure print_table;
 begin
  Writeln('table.FCount=',table.FCount);
  Writeln('table.FSpace=',table.FSpace);
  Writeln('table.FLast =',table.FLast );
  Writeln('table.FPos  =',table.FPos  );
 end;

begin
 id_table_init(@table,1,5);

 desc:=Default(t_id_desc);
 key:=0;

 //

 res:=id_new(@table,@desc,@key);

 Writeln(res,' ',key);

 print_table;

 //

 res:=id_new(@table,@desc,@key);

 Writeln(res,' ',key);

 print_table;

 //

 res:=id_new(@table,@desc,@key);

 Writeln(res,' ',key);

 print_table;

 //

 res:=id_new(@table,@desc,@key);

 Writeln(res,' ',key);

 print_table;

 //

 key:=3;
 res:=id_del(@table,key,nil);

 Writeln(res,' ',key);

 print_table;

 //

 key:=4;
 res:=id_del(@table,key,nil);

 Writeln(res,' ',key);

 print_table;

 //

 res:=id_new(@table,@desc,@key);

 Writeln(res,' ',key);

 print_table;

 id_table_fini(@table);
end;

type
 p_test_tailq=^test_tailq;
 test_tailq=packed record
  stub:array[0..2] of qword;
  entry:TAILQ_ENTRY;
  name:PChar;
 end;

procedure tailq;
var
 list:TAILQ_HEAD;
 e,n:p_test_tailq;
begin
 TAILQ_INIT(@list);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='one';
 //TAILQ_INSERT_TAIL(@list,e,@e^.entry);
 TAILQ_INSERT_HEAD(@list,e,@e^.entry);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='two';
 //TAILQ_INSERT_TAIL(@list,e,@e^.entry);
 TAILQ_INSERT_HEAD(@list,e,@e^.entry);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='thr';
 //TAILQ_INSERT_TAIL(@list,e,@e^.entry);
 TAILQ_INSERT_HEAD(@list,e,@e^.entry);

 Writeln('TAILQ_FIRST');
 e:=TAILQ_FIRST(@list);
 while (e<>nil) do
 begin
  Writeln(e^.name);
  e:=TAILQ_NEXT(e,@e^.entry);
 end;

 Writeln('TAILQ_LAST');
 e:=TAILQ_LAST(@list);
 while (e<>nil) do
 begin
  Writeln(e^.name);
  e:=TAILQ_PREV(e,@e^.entry);
 end;

 Writeln('TAILQ_REMOVE');
 e:=TAILQ_FIRST(@list);
 while (e<>nil) do
 begin
  n:=TAILQ_NEXT(e,@e^.entry);
  //
  TAILQ_REMOVE(@list,e,@e^.entry);
  Writeln(e^.name);
  FreeMem(e);
  //
  e:=n;
 end;

 //

 LIST_INIT(@list);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='one';
 LIST_INSERT_HEAD(@list,e,@e^.entry);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='two';
 LIST_INSERT_HEAD(@list,e,@e^.entry);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='thr';
 LIST_INSERT_HEAD(@list,e,@e^.entry);

 Writeln('LIST_FIRST');
 e:=LIST_FIRST(@list);
 while (e<>nil) do
 begin
  Writeln(e^.name);
  e:=LIST_NEXT(e,@e^.entry);
 end;

 //REMOVE
 Writeln('LIST_REMOVE');
 e:=LIST_FIRST(@list);
 while (e<>nil) do
 begin
  n:=LIST_NEXT(e,@e^.entry);
  //
  LIST_REMOVE(e,@e^.entry);
  Writeln(e^.name);
  FreeMem(e);
  //
  e:=n;
 end;

 //

 STAILQ_INIT(@list);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='one';
 STAILQ_INSERT_TAIL(@list,e,@e^.entry);
 //STAILQ_INSERT_HEAD(@list,e,@e^.entry);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='two';
 STAILQ_INSERT_TAIL(@list,e,@e^.entry);
 //STAILQ_INSERT_HEAD(@list,e,@e^.entry);

 e:=AllocMem(SizeOf(test_tailq));
 e^.name:='thr';
 STAILQ_INSERT_TAIL(@list,e,@e^.entry);
 //STAILQ_INSERT_HEAD(@list,e,@e^.entry);

 Writeln('STAILQ_FIRST');
 e:=STAILQ_FIRST(@list);
 while (e<>nil) do
 begin
  Writeln(e^.name);
  e:=STAILQ_NEXT(e,@e^.entry);
 end;
 writeln;

 Writeln('STAILQ_LAST');
 e:=STAILQ_LAST(@list,@p_test_tailq(nil)^.entry);
 Writeln(e^.name);

 Writeln('STAILQ_REMOVE');
 e:=STAILQ_FIRST(@list);
 while (e<>nil) do
 begin
  n:=STAILQ_NEXT(e,@e^.entry);
  //
  STAILQ_REMOVE(@list,e,@e^.entry);
  Writeln(e^.name);
  FreeMem(e);
  //
  e:=n;
 end;

 //

 writeln;
end;

var
 ThreadHandle:THandle;
 v:Integer;
 n:Integer;

 prio:t_rtprio;

 ktd:p_kthread;

 _time:Int64;
 _tv:timeval;

 _thr_param:thr_param;
 _uctx:ucontext_t;

 ru:t_rusage;

begin
 //tailq;
 id_test;

 //test_map;
 sys_init;

 //Writeln(get_proc_prio());
 //Writeln(set_proc_prio(14));
 //Writeln(get_proc_prio());

 //Writeln(sys_getrusage(RUSAGE_SELF,@ru));
 //Writeln(sys_getrusage(RUSAGE_THREAD,@ru));

 e:=_umtx_op(nil,UMTX_OP_RW_WRLOCK,0,nil,nil);
 Writeln('me=',e,' _errno:',__error^);

 //kern_clock_gettime_unit(CLOCK_PROCTIME,@_time);
 //writeln(_time/10000000:0:3);

 sys_adjtime(nil,@_tv);
 writeln(_tv.tv_sec,',',_tv.tv_usec);

 e:=NtCreateEvent(
         @event,
         EVENT_ALL_ACCESS,
         nil,
         NotificationEvent,
         False);

 _umutex_init(@mtx);

 //mtx.m_flags:=UMUTEX_PRIO_INHERIT;
 mtx.m_flags:=UMUTEX_PRIO_PROTECT;

 mseg:=VirtualAlloc(nil,64*1024,MEM_COMMIT,PAGE_READWRITE);
 mem:=VirtualAlloc(nil,64*1024,MEM_COMMIT,PAGE_READWRITE);

 mem2:=VirtualAlloc(nil,64*1024,MEM_COMMIT,PAGE_READWRITE);

 prio._type:=RTP_PRIO_NORMAL;
 prio._prio:=700;

 _thr_param:=Default(thr_param);

 _thr_param.start_func:=@test_thread;
 _thr_param.arg       :=nil;
 _thr_param.stack_base:=mem;
 _thr_param.stack_size:=64*1024;
 _thr_param.tls_base  :=mem;
 _thr_param.child_tid :=@tid;
 _thr_param.parent_tid:=nil;
 _thr_param.rtp       :=@prio;
 _thr_param.name      :='test';

 thr_new(@_thr_param,SizeOf(_thr_param));

 _thr_param.start_func:=@test_thread;
 _thr_param.arg       :=nil;
 _thr_param.stack_base:=mem2;
 _thr_param.stack_size:=64*1024;
 _thr_param.tls_base  :=mem2;
 _thr_param.child_tid :=nil;
 _thr_param.parent_tid:=nil;
 _thr_param.rtp       :=@prio;
 _thr_param.name      :='test2';

 thr_new(@_thr_param,SizeOf(_thr_param));

 //_uctx:=Default(ucontext_t);
 //_uctx.uc_mcontext.mc_len:=sizeof(mcontext_t);
 //
 //_uctx.uc_mcontext.mc_rsp:=qword(_thr_param.stack_base)+_thr_param.stack_size-8;
 //_uctx.uc_mcontext.mc_rip:=qword(@test_thread);
 //
 //thr_create(@_uctx,nil,0);

 //readln;

 sleep(500);

 ktd:=tdfind(tid);

 //NtSuspendThread(ktd^.td_handle,nil);
 //NtResumeThread(ktd^.td_handle,nil);

 sleep(-1);
 //readln;

 FillChar(mseg^,64*1024,$11);

 ThreadHandle:=BeginThread(@_thread);

 {
 For v:=-16 to 16 do
 begin
  n:=NtSetInformationThread(ThreadHandle,ThreadBasePriority,@v,SizeOf(Integer));
  if (n=0) then
  Writeln('v:',v,' ',HexStr(n,8));
 end;

 readln;
 }

 BeginThread(@_thread);
 //BeginThread(@_thread);
 //BeginThread(@_thread);

 NtSetEvent(event,nil);

 //e:=_do_lock_umtx(@mtx,GetCurrentThreadId);
 //Writeln('  lock[',GetCurrentThreadId,'] ',e);

 //e:=do_unlock_umtx(@mtx,GetCurrentThreadId);
 //Writeln('unlock[',GetCurrentThreadId,'] ',e);
 //
 //e:=_do_lock_umtx(@mtx,GetCurrentThreadId);
 //Writeln('  lock[',GetCurrentThreadId,'] ',e);
 //
 //e:=do_unlock_umtx(@mtx,GetCurrentThreadId);
 //Writeln('unlock[',GetCurrentThreadId,'] ',e);

 //_umutex_done(@mtx);

 readln;
end.

