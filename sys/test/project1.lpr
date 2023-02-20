
uses
 windows,
 atomic,
 ntapi,
 signal,
 ucontext,
 _umtx,
 kern_umtx,
 sys_umtx,
 time,
 kern_time,
 kern_thread,
 kern_lock,
 kern_rwlock,
 thr_private,
 kern_sig,
 trap,
 sysutils,
 vulkan,
 vDevice;

var
 mtx:umutex;
 rwl:urwlock;
 e:Integer;

 event:Thandle;

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
 mem:Pointer;
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

procedure test_thread; sysv_abi_default;
var
 rax:qword;
begin

 //SetTlsBase(Pointer(qword(1)));

 sleep(1);

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

 thread_exit;
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
 dev:TVkDeviceMemory;
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

var
 ThreadHandle:THandle;
 v:Integer;
 n:Integer;

 prio:rtprio;

 tid:QWORD;
 ktd:p_kthread;

 _time:Int64;

begin
 //test_map;

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

 klock(lock);

 prio._type:=RTP_PRIO_NORMAL;
 prio._prio:=700;

 create_thread(nil,
               nil,
               @test_thread,
               nil,
               mem,
               64*1024,
               mem,
               @tid,
               nil,
               @prio,
               nil
              );


 //readln;

 sleep(500);

 ktd:=tdfind(tid);

 //NtSuspendThread(ktd^.td_handle,nil);
 //NtResumeThread(ktd^.td_handle,nil);

 kunlock(lock);

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

 BeginThread(@_thread);
 BeginThread(@_thread);
 BeginThread(@_thread);

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

