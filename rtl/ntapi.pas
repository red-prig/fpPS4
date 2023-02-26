unit ntapi;

{$mode objfpc}{$H+}

interface

uses
 Windows;

const
 STATUS_SUCCESS           =$00000000;
 STATUS_WAIT_0            =$00000000;
 STATUS_ABANDONED         =$00000080;
 STATUS_ABANDONED_WAIT_0  =$00000080;
 STATUS_USER_APC          =$000000C0;
 STATUS_KERNEL_APC        =$00000100;
 STATUS_ALERTED           =$00000101;
 STATUS_TIMEOUT           =$00000102;
 STATUS_PENDING           =$00000103;
 STATUS_NO_YIELD_PERFORMED=$40000024;
 STATUS_ACCESS_VIOLATION  =$C0000005;
 STATUS_INVALID_HANDLE    =$C0000008;
 STATUS_INVALID_PARAMETER =$C000000D;
 STATUS_END_OF_FILE       =$C0000011;
 STATUS_ACCESS_DENIED     =$C0000022;
 STATUS_DISK_FULL         =$C000007F;

 NT_INFINITE=$8000000000000000;

 ThreadBasicInformation          =  0;
 ThreadTimes                     =  1;
 ThreadPriority                  =  2;
 ThreadBasePriority              =  3;
 ThreadAffinityMask              =  4;
 ThreadImpersonationToken        =  5;
 ThreadDescriptorTableEntry      =  6;
 ThreadEnableAlignmentFaultFixup =  7;
 ThreadEventPair                 =  8;
 ThreadQuerySetWin32StartAddress =  9;
 ThreadZeroTlsCell               = 10;
 ThreadPerformanceCount          = 11;
 ThreadAmILastThread             = 12;
 ThreadIdealProcessor            = 13;
 ThreadPriorityBoost             = 14;
 ThreadSetTlsArrayAddress        = 15;
 ThreadIsIoPending               = 16;
 ThreadHideFromDebugger          = 17;

 ProcessBasicInformation=0;
 ProcessQuotaLimits     =1;
 ProcessIoCounters      =2;
 ProcessVmCounters      =3;
 ProcessTimes           =4;
 ProcessAffinityMask    =21;

 FileStandardInformation  = 5;
 FilePositionInformation  =14;
 FileAllocationInformation=19;
 FileEndOfFileInformation =20;

 MemoryBasicInformation=0;

 //EVENT_TYPE
 NotificationEvent   =0;
 SynchronizationEvent=1;

 EVENT_QUERY_STATE =$0001;
 EVENT_MODIFY_STATE=$0002;
 EVENT_ALL_ACCESS  =$1F0003;

 MUTANT_ALL_ACCESS =$1F0001;

 THREAD_ALL_ACCESS=$1fffff;

 NtCurrentProcess=THandle(-1);
 NtCurrentThread =THandle(-2);

 RPL_MASK        =$0003;

 KGDT64_R3_DATA  =$0028;
 KGDT64_R3_CODE  =$0030;
 KGDT64_R3_CMTEB =$0050;

 EFLAGS_INTERRUPT_MASK=$200;

 INITIAL_MXCSR   =$1f80;

 CONTEXT_THREAD  =CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS;

 ViewShare=1;
 ViewUnmap=2;

type
 PIO_STATUS_BLOCK=^IO_STATUS_BLOCK;
 IO_STATUS_BLOCK=packed record
  Status:DWORD;
  _Align:DWORD;
  Information:PTRUINT;
 end;

 PFILE_STANDARD_INFORMATION=^FILE_STANDARD_INFORMATION;
 FILE_STANDARD_INFORMATION=packed record
  AllocationSize:LARGE_INTEGER;
  EndOfFile     :LARGE_INTEGER;
  NumberOfLinks :ULONG;
  DeletePending :WORD;
  Directory     :WORD;
 end;

 PIO_APC_ROUTINE=procedure(ApcContext:Pointer;
                           IoStatusBlock:PIO_STATUS_BLOCK;
                           Reserved:ULONG); stdcall;

 PMUTANT_BASIC_INFORMATION=^MUTANT_BASIC_INFORMATION;
 MUTANT_BASIC_INFORMATION=packed record
  CurrentCount  :LONG;
  OwnedByCaller :WORD;
  AbandonedState:WORD;
 end;

 TCONTEXT=Windows.TCONTEXT;
 PCONTEXT=Windows.PCONTEXT;

 PCLIENT_ID=^TCLIENT_ID;
 TCLIENT_ID=packed record
  UniqueProcess:THandle;
  UniqueThread :THandle;
 end;

 PINITIAL_TEB=^TINITIAL_TEB;
 TINITIAL_TEB=packed record
  PreviousStackBase :Pointer;
  PreviousStackLimit:Pointer;
  StackBase         :Pointer;
  StackLimit        :Pointer;
  AllocatedStackBase:Pointer;
 end;

 PTHREAD_BASIC_INFORMATION=^THREAD_BASIC_INFORMATION;
 THREAD_BASIC_INFORMATION=packed record
  ExitStatus    :DWORD;
  _align        :DWORD;
  TebBaseAddress:Pointer; //PTEB
  ClientId      :TCLIENT_ID;
  AffinityMask  :ULONG_PTR;
  Priority      :DWORD;
  BasePriority  :DWORD;
 end;

 PPROCESS_BASIC_INFORMATION=^PROCESS_BASIC_INFORMATION;
 PROCESS_BASIC_INFORMATION=packed record
  ExitStatus      :DWORD;
  _align          :DWORD;
  PebBaseAddress  :QWORD;
  AffinityMask    :QWORD;
  BasePriority    :QWORD;
  UniqueProcessId :QWORD;
  InheritedFromUPI:QWORD;
 end;

 PKERNEL_USER_TIMES=^KERNEL_USER_TIMES;
 KERNEL_USER_TIMES=packed record
  CreateTime:LARGE_INTEGER;
  ExitTime  :LARGE_INTEGER;
  KernelTime:LARGE_INTEGER;
  UserTime  :LARGE_INTEGER;
 end;

function NtClose(Handle:THandle):DWORD; stdcall; external 'ntdll';

function NtCreateThread(
          hThread           :PHandle;
          DesiredAccess     :DWORD;
          ObjectAttributes  :Pointer;
          ProcessHandle     :THandle;
          ClientId          :PCLIENT_ID;
          ThreadContext     :PCONTEXT;
          InitialTeb        :PINITIAL_TEB;
          CreateSuspended   :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtTerminateThread(
          ThreadHandle:THandle;
          ExitStatus  :DWORD
         ):DWORD; stdcall; external 'ntdll';

function RtlExitUserThread(
          ExitStatus:DWORD
         ):DWORD; stdcall; external 'ntdll';

function NtReadVirtualMemory(
          ProcessHandle      :THandle;
          BaseAddress        :Pointer;
          Buffer             :Pointer;
          NumberOfBytesToRead:ULONG;
          NumberOfBytesReaded:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtWriteVirtualMemory(
          ProcessHandle      :THandle;
          BaseAddress        :Pointer;
          Buffer             :Pointer;
          NumberOfBytesToRead:ULONG;
          NumberOfBytesReaded:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtAlertThread(hThread:THandle):DWORD; stdcall; external 'ntdll';

function NtTestAlert():DWORD; stdcall; external 'ntdll';

function NtQueueApcThread(
          hThread      :THandle;
          ApcRoutine   :Pointer;
          ApcContext   :Pointer;
          IoStatusBlock:PIO_STATUS_BLOCK;
          ApcReserved  :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtYieldExecution():DWORD; stdcall; external 'ntdll';

function NtDelayExecution(
          Alertable:Boolean;
          DelayInterval:PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtWaitForSingleObject(
          ObjectHandle:THandle;
          Alertable:Boolean;
          TimeOut:PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtGetContextThread(
          ThreadHandle:THandle;
          Context:PCONTEXT
         ):DWORD; stdcall; external 'ntdll';

function NtSetContextThread(
          ThreadHandle:THandle;
          Context:PCONTEXT
         ):DWORD; stdcall; external 'ntdll';

function NtAlertResumeThread(
          ThreadHandle:THandle;
          SuspendCount:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtResumeThread(
          ThreadHandle:THandle;
          SuspendCount:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSuspendThread(
          ThreadHandle:THandle;
          SuspendCount:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtQueryInformationThread(
          ThreadHandle           :THandle;
          ThreadInformationClass :DWORD;
          ThreadInformation      :Pointer;
          ThreadInformationLength:ULONG;
          ReturnLength           :PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtQueryInformationProcess(
          ProcessHandle           :THandle;
          ProcessInformationClass :DWORD;
          ProcessInformation      :Pointer;
          ProcessInformationLength:ULONG;
          ReturnLength            :PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSetInformationProcess(
          ProcessHandle:THandle;
          ProcessInformationClass:DWORD;
          ProcessInformation:Pointer;
          ProcessInformationLength:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSetInformationThread(
          ThreadHandle:THandle;
          ThreadInformationClass:DWORD;
          ThreadInformation:Pointer;
          ThreadInformationLength:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtContinue(
          Context:PCONTEXT;
          RaiseAlert:Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtQueryPerformanceCounter(
          Counter,
          Frequency:PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtQueryTimerResolution(
          MinimumResolution:PULONG;
          MaximumResolution:PULONG;
          CurrentResolution:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSetTimerResolution(
          DesiredResolution:ULONG;
          SetResolution:Boolean;
          CurrentResolution:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtReadFile(
          FileHandle   :THandle;
          Event        :THandle;
          ApcRoutine   :Pointer;
          ApcContext   :Pointer;
          IoStatusBlock:PIO_STATUS_BLOCK;
          Buffer       :Pointer;
          Length       :ULONG;
          ByteOffset   :PLARGE_INTEGER;
          Key          :PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtWriteFile(
          FileHandle   :THandle;
          Event        :THandle;
          ApcRoutine   :Pointer;
          ApcContext   :Pointer;
          IoStatusBlock:PIO_STATUS_BLOCK;
          Buffer       :Pointer;
          Length       :ULONG;
          ByteOffset   :PLARGE_INTEGER;
          Key          :PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSetInformationFile(
          FileHandle          :THandle;
          IoStatusBlock       :PIO_STATUS_BLOCK;
          FileInformation     :Pointer;
          Length              :ULONG;
          FileInformationClass:DWORD
         ):DWORD; stdcall; external 'ntdll';

function NtQueryInformationFile(
          FileHandle          :THandle;
          IoStatusBlock       :PIO_STATUS_BLOCK;
          FileInformation     :Pointer;
          Length              :ULONG;
          FileInformationClass:DWORD
         ):DWORD; stdcall; external 'ntdll';

function NtCreateEvent(
          EventHandle     :PHandle;
          DesiredAccess   :DWORD;
          ObjectAttributes:Pointer;
          EventType       :DWORD;
          InitialState    :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtSetEvent(
          EventHandle     :THandle;
          PreviousState   :PLONG
         ):DWORD; stdcall; external 'ntdll';

function NtResetEvent(
          EventHandle     :THandle;
          PreviousState   :PLONG
         ):DWORD; stdcall; external 'ntdll';

function NtClearEvent(EventHandle:THandle):DWORD; stdcall; external 'ntdll';

function NtCreateMutant(
          MutantHandle    :PHandle;
          DesiredAccess   :DWORD;
          ObjectAttributes:Pointer;
          InitialOwner    :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtReleaseMutant(
          MutantHandle    :THandle;
          PreviousState   :PLONG
         ):DWORD; stdcall; external 'ntdll';

function NtQueryMutant(
          MutantHandle           :THandle;
          MutantInformationClass :DWORD;
          MutantInformation      :Pointer;
          MutantInformationLength:ULONG;
          ResultLength           :PULONG
         ):DWORD; stdcall; external 'ntdll';


function NtCreateSection(
          SectionHandle        :PHandle;
          DesiredAccess        :DWORD;
          ObjectAttributes     :Pointer;
          MaximumSize          :PLARGE_INTEGER;
          SectionPageProtection:ULONG;
          AllocationAttributes :ULONG;
          FileHandle           :THandle
         ):DWORD; stdcall; external 'ntdll';

function NtExtendSection(
          SectionHandle        :THandle;
          NewSectionSize       :PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtMapViewOfSection(
          SectionHandle        :THandle;
          ProcessHandle        :THandle;
          BaseAddress          :PPointer;
          ZeroBits             :ULONG_PTR;
          CommitSize           :ULONG_PTR;
          SectionOffset        :PLARGE_INTEGER;
          ViewSize             :PULONG_PTR;
          InheritDisposition   :DWORD;
          AllocationType       :ULONG;
          Protect              :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtUnmapViewOfSection(
          ProcessHandle        :THandle;
          BaseAddress          :PPointer
         ):DWORD; stdcall; external 'ntdll';

function NtAllocateVirtualMemory(
          ProcessHandle        :THandle;
          BaseAddress          :PPointer;
          ZeroBits             :ULONG_PTR;
          RegionSize           :PULONG_PTR;
          AllocationType       :ULONG;
          Protect              :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtFreeVirtualMemory(
          ProcessHandle        :THandle;
          BaseAddress          :PPointer;
          RegionSize           :PULONG_PTR;
          FreeType             :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtProtectVirtualMemory(
          ProcessHandle        :THandle;
          BaseAddress          :PPointer;
          RegionSize           :PULONG_PTR;
          NewAccessProtection  :ULONG;
          OldAccessProtection  :PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtQueryVirtualMemory(
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          MemoryInformationClass:DWORD;
          Buffer                :Pointer;
          Length                :ULONG_PTR;
          ResultLength          :PULONG_PTR
         ):DWORD; stdcall; external 'ntdll';

implementation

end.

