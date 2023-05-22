
uses
 windows,
 dateutils,
 atomic,
 ntapi,
 mqueue,
 signal,
 ucontext,
 _umtx,
 sys_umtx,
 time,
 kern_time,
 thr,
 kern_thread,
 kern_rwlock,
 thr_private,
 sys_cpuset,
 trap,
 sys_sig,
 md_psl,
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
  sys_osem,
 kern_evf,
 sys_evf,
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
 sysent,
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
 vmount;

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

 Writeln('trap_test: ',' curkthread:',HexStr(curkthread),' sptr:',HexStr(sptr),' ',HexStr(td^.td_frame^.tf_rip,16));
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

 umtx_thread_init(td);

 set_curkthread(td);

 p:=mmap(Pointer($700000000),16*1024,PROT_CPU_ALL,MAP_VOID or MAP_FIXED,-1,0);
 Writeln(HexStr(p));

 p:=mmap(Pointer($700000000),16*1024,PROT_CPU_ALL,MAP_ANON or MAP_FIXED,-1,0);
 Writeln(HexStr(p));

 Result:=query_memory_protection(Pointer($700000000),16*1024,@qr);
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

procedure test_dirs(const dirp,namep:RawByteString;s:Byte);
label
 _next;
var
 td:p_kthread;
 fs:t_statfs;

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
   //sys_fstatfs(fd,@fs);

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

procedure test_thread; sysv_abi_default;
var
 rax:qword;
 act:sigaction_t;
 _sig:Integer;
 oset:sigset_t;
 i,t:Integer;
begin

 //SetTlsBase(Pointer(qword(1)));

 if (tid<>curkthread^.td_tid) then
 begin

  //Writeln('sys_mkdir=',sys_mkdir('/test',&777));
  //Writeln('sys_mkdir=',sys_mkdir('/test/test',&777));
  //Writeln('sys_rmdir=',sys_rmdir('/test/test'));
  //Writeln('sys_symlink=',sys_symlink('/app0','/test/test2'));
  //Writeln('sys_unlink=',sys_unlink('/test/test2'));
  //sys_symlink('lib/x86_64-win64/_umtx.o','/app0/link_lib');
  //sys_link('/app0/test.txt','/app0/lib/test_link.txt');
  //sys_mkdir('/app0/new',&777);

  Writeln('[--test_dirs--]');
  test_dirs('','/',1);
  Writeln('[--test_dirs--]');

  tid2:=curkthread^.td_tid;

  evf:=evf_create('evf test',EVF_ATTR_TH_PRIO,0);
  Writeln('evf=',evf,' _errno:',__error^);

  //osem:=osem_create('osem test',1,1,10);
  //Writeln('osem=',osem,' _errno:',__error^);

  act:=Default(sigaction_t);
  act.u.sa_handler:=sa_handler(@__ex_handler);
  act.sa_flags:=SA_RESTART;

  sigaction(SIGUSR1,@act,nil,0);

  thr_kill(tid,SIGUSR1);
  //thr_wake(tid);

  i:=_evf_trywait_err(evf,1,EVF_WAITMODE_OR,nil);
  Writeln('_evf_trywait_err=',i,' _errno:',__error^);

  i:=_evf_wait_err(evf,2,EVF_WAITMODE_OR,nil,nil);
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

  repeat
   asm
    pause
   end;
  until (intr<>0);

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

 i:=_evf_set_err(evf,2);

 i:=_evf_set_err(evf,1);
 Writeln('_evf_set_err=',i,' _errno:',__error^);

 //_osem_post_err(osem,1);

 sig_lock;
 sig_lock;
  sleep(1);
 sig_unlock;
 sig_unlock;

{
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

 _thr_param:thr_param;

begin
 tailq;

 //test_map;
 sys_init;

 e:=_umtx_op(nil,UMTX_OP_RW_WRLOCK,0,nil,nil);
 Writeln('me=',e,' _errno:',__error^);

 kern_clock_gettime_unit(CLOCK_PROCTIME,@_time);
 writeln(_time/10000000:0:3);

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

 //readln;

 sleep(500);

 ktd:=tdfind(tid);

 //NtSuspendThread(ktd^.td_handle,nil);
 //NtResumeThread(ktd^.td_handle,nil);

 readln;

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

 //BeginThread(@_thread);
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

