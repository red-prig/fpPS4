unit ntapi;

{$mode objfpc}{$H+}

interface

uses
 Windows;

const
 STATUS_SUCCESS              =$00000000;
 STATUS_WAIT_0               =$00000000;
 STATUS_ABANDONED            =$00000080; //EPERM
 STATUS_ABANDONED_WAIT_0     =$00000080;
 STATUS_USER_APC             =$000000C0; //EINTR
 STATUS_KERNEL_APC           =$00000100; //EINTR
 STATUS_ALERTED              =$00000101; //EINTR
 STATUS_TIMEOUT              =$00000102; //ETIMEDOUT
 STATUS_PENDING              =$00000103; //EWOULDBLOCK
 STATUS_NO_YIELD_PERFORMED   =$40000024;
 STATUS_NO_MORE_FILES        =$80000006;
 STATUS_ACCESS_VIOLATION     =$C0000005; //EFAULT
 STATUS_INVALID_HANDLE       =$C0000008; //EBADF
 STATUS_INVALID_PARAMETER    =$C000000D; //EINVAL
 STATUS_NO_SUCH_FILE         =$C000000F; //ENOENT
 STATUS_END_OF_FILE          =$C0000011;
 STATUS_NO_MEMORY            =$C0000017; //ENOMEM
 STATUS_ACCESS_DENIED        =$C0000022; //EPERM
 STATUS_DISK_CORRUPT_ERROR   =$C0000032; //EIO
 STATUS_OBJECT_NAME_NOT_FOUND=$C0000034; //ENOENT
 STATUS_OBJECT_NAME_COLLISION=$C0000035; //EEXIST
 STATUS_DISK_FULL            =$C000007F; //ENOSPC
 STATUS_FILE_IS_A_DIRECTORY  =$C00000BA; //EISDIR
 STATUS_DIRECTORY_NOT_EMPTY  =$C0000101; //ENOTEMPTY
 STATUS_FILE_CORRUPT_ERROR   =$C0000102; //EIO
 STATUS_NOT_A_DIRECTORY      =$C0000103; //ENOTDIR
 STATUS_NAME_TOO_LONG        =$C0000106; //ENAMETOOLONG
 STATUS_COMMITMENT_LIMIT     =$C000012D; //ENOMEM
 STATUS_IO_DEVICE_ERROR      =$C0000185; //EIO
 STATUS_TOO_MANY_LINKS       =$C0000265; //EMLINK


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

 FileBasicInformation          = 4;
 FileStandardInformation       = 5;
 FileInternalInformation       = 6;
 FileEaInformation             = 7;
 FileAccessInformation         = 8;
 FileNamesInformation          =12;
 FilePositionInformation       =14;
 FileModeInformation           =16;
 FileAlignmentInformation      =17;
 FileAllInformation            =18;
 FileAllocationInformation     =19;
 FileEndOfFileInformation      =20;
 FileIdFullDirectoryInformation=38;

 FileFsFullSizeInformation=7;

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

 SECTION_MAP_EXECUTE=$8;

 OBJ_INHERIT         =$00000002;
 OBJ_PERMANENT       =$00000010;
 OBJ_EXCLUSIVE       =$00000020;
 OBJ_CASE_INSENSITIVE=$00000040;
 OBJ_OPENIF          =$00000080;
 OBJ_OPENLINK        =$00000100;
 OBJ_VALID_ATTRIBUTES=$000001F2;

 // Create disposition
 FILE_SUPERSEDE                =$00000000;
 FILE_OPEN                     =$00000001;
 FILE_CREATE                   =$00000002;
 FILE_OPEN_IF                  =$00000003;
 FILE_OVERWRITE                =$00000004;
 FILE_OVERWRITE_IF             =$00000005;
 FILE_MAXIMUM_DISPOSITION      =$00000005;

 // Create/open flags
 FILE_DIRECTORY_FILE           =$00000001;
 FILE_WRITE_THROUGH            =$00000002;
 FILE_SEQUENTIAL_ONLY          =$00000004;
 FILE_NO_INTERMEDIATE_BUFFERING=$00000008;
 FILE_SYNCHRONOUS_IO_ALERT     =$00000010;
 FILE_SYNCHRONOUS_IO_NONALERT  =$00000020;
 FILE_NON_DIRECTORY_FILE       =$00000040;
 FILE_CREATE_TREE_CONNECTION   =$00000080;
 FILE_COMPLETE_IF_OPLOCKED     =$00000100;
 FILE_NO_EA_KNOWLEDGE          =$00000200;
 FILE_OPEN_FOR_RECOVERY        =$00000400;
 FILE_RANDOM_ACCESS            =$00000800;
 FILE_DELETE_ON_CLOSE          =$00001000;
 FILE_OPEN_BY_FILE_ID          =$00002000;
 FILE_OPEN_FOR_BACKUP_INTENT   =$00004000;
 FILE_NO_COMPRESSION           =$00008000;
 FILE_OPEN_REQUIRING_OPLOCK    =$00010000;
 FILE_DISALLOW_EXCLUSIVE       =$00020000;
 FILE_SESSION_AWARE            =$00040000;
 FILE_RESERVE_OPFILTER         =$00100000;
 FILE_OPEN_REPARSE_POINT       =$00200000; //open symlink
 FILE_OPEN_NO_RECALL           =$00400000;
 FILE_OPEN_FOR_FREE_SPACE_QUERY=$00800000;
 FILE_COPY_STRUCTURED_STORAGE  =$00000041;
 FILE_STRUCTURED_STORAGE       =$00000441;

 // I/O status information values for NtCreateFile/NtOpenFile
 FILE_SUPERSEDED               =$00000000;
 FILE_OPENED                   =$00000001;
 FILE_CREATED                  =$00000002;
 FILE_OVERWRITTEN              =$00000003;
 FILE_EXISTS                   =$00000004;
 FILE_DOES_NOT_EXIST           =$00000005;

 // Special ByteOffset parameters
 FILE_WRITE_TO_END_OF_FILE     =$ffffffff;
 FILE_USE_FILE_POINTER_POSITION=$fffffffe;

 FSCTL_GET_REPARSE_POINT=$000900A8;
 IO_REPARSE_TAG_SYMLINK =$A000000C;

type
 PIO_STATUS_BLOCK=^IO_STATUS_BLOCK;
 IO_STATUS_BLOCK=packed record
  Status     :DWORD;
  _Align     :DWORD;
  Information:PTRUINT;
 end;

 PUNICODE_STRING=^UNICODE_STRING;
 UNICODE_STRING=packed record
  Length       :USHORT; //size in byte
  MaximumLength:USHORT;
  _Align       :DWORD;
  Buffer       :PWSTR;
 end;

 POBJECT_ATTRIBUTES=^OBJECT_ATTRIBUTES;
 OBJECT_ATTRIBUTES=packed record
  Length                  :ULONG; //sizeof(OBJECT_ATTRIBUTES)
  _Align1                 :ULONG;
  RootDirectory           :THandle;
  ObjectName              :PUNICODE_STRING;
  Attributes              :ULONG;
  _Align2                 :ULONG;
  SecurityDescriptor      :Pointer;
  SecurityQualityOfService:Pointer;
 end;

 PFILE_BASIC_INFORMATION=^FILE_BASIC_INFORMATION;
 FILE_BASIC_INFORMATION=packed record
  CreationTime  :LARGE_INTEGER;
  LastAccessTime:LARGE_INTEGER;
  LastWriteTime :LARGE_INTEGER;
  ChangeTime    :LARGE_INTEGER;
  FileAttributes:ULONG;
  _align        :ULONG;
 end;

 PFILE_STANDARD_INFORMATION=^FILE_STANDARD_INFORMATION;
 FILE_STANDARD_INFORMATION=packed record
  AllocationSize:LARGE_INTEGER;
  EndOfFile     :LARGE_INTEGER;
  NumberOfLinks :ULONG;
  DeletePending :WORD;
  Directory     :WORD;
 end;

 PFILE_INTERNAL_INFORMATION=^FILE_INTERNAL_INFORMATION;
 FILE_INTERNAL_INFORMATION=packed record
  IndexNumber:LARGE_INTEGER;
 end;

 PFILE_EA_INFORMATION=^FILE_EA_INFORMATION;
 FILE_EA_INFORMATION=packed record
  EaSize:ULONG;
 end;

 PFILE_ACCESS_INFORMATION=^FILE_ACCESS_INFORMATION;
 FILE_ACCESS_INFORMATION=packed record
  AccessFlags:ACCESS_MASK;
 end;

 PFILE_POSITION_INFORMATION=^FILE_POSITION_INFORMATION;
 FILE_POSITION_INFORMATION=packed record
  CurrentByteOffset:LARGE_INTEGER;
 end;

 PFILE_MODE_INFORMATION=^FILE_MODE_INFORMATION;
 FILE_MODE_INFORMATION=packed record
  Mode:ULONG;
 end;

 PFILE_ALIGNMENT_INFORMATION=^FILE_ALIGNMENT_INFORMATION;
 FILE_ALIGNMENT_INFORMATION=packed record
  AlignmentRequirement:ULONG;
 end;

 PFILE_NAME_INFORMATION=^FILE_NAME_INFORMATION;
 FILE_NAME_INFORMATION=packed record
  FileNameLength:ULONG;
  FileName      :record end; //WCHAR
 end;

 PFILE_ALL_INFORMATION=^FILE_ALL_INFORMATION;
 FILE_ALL_INFORMATION=packed record
  BasicInformation    :FILE_BASIC_INFORMATION;
  StandardInformation :FILE_STANDARD_INFORMATION;
  InternalInformation :FILE_INTERNAL_INFORMATION;
  EaInformation       :FILE_EA_INFORMATION;
  AccessInformation   :FILE_ACCESS_INFORMATION;
  PositionInformation :FILE_POSITION_INFORMATION;
  ModeInformation     :FILE_MODE_INFORMATION;
  AlignmentInformation:FILE_ALIGNMENT_INFORMATION;
  NameInformation     :FILE_NAME_INFORMATION;
 end;

 PFILE_ID_FULL_DIR_INFORMATION=^FILE_ID_FULL_DIR_INFORMATION;
 FILE_ID_FULL_DIR_INFORMATION=packed record
  NextEntryOffset:ULONG;
  FileIndex      :ULONG;
  CreationTime   :LARGE_INTEGER;
  LastAccessTime :LARGE_INTEGER;
  LastWriteTime  :LARGE_INTEGER;
  ChangeTime     :LARGE_INTEGER;
  EndOfFile      :LARGE_INTEGER;
  AllocationSize :LARGE_INTEGER;
  FileAttributes :ULONG;
  FileNameLength :ULONG;
  EaSize         :ULONG;
  align          :ULONG;
  FileId         :LARGE_INTEGER;
  FileName       :record end; //WCHAR
 end;

 PFILE_FS_FULL_SIZE_INFORMATION=^FILE_FS_FULL_SIZE_INFORMATION;
 FILE_FS_FULL_SIZE_INFORMATION=packed record
  TotalAllocationUnits          :LARGE_INTEGER;
  CallerAvailableAllocationUnits:LARGE_INTEGER;
  ActualAvailableAllocationUnits:LARGE_INTEGER;
  SectorsPerAllocationUnit      :ULONG;
  BytesPerSector                :ULONG;
 end;

 PREPARSE_DATA_BUFFER=^REPARSE_DATA_BUFFER;
 REPARSE_DATA_BUFFER=packed record
  ReparseTag       :ULONG;
  ReparseDataLength:USHORT;
  Reserved         :USHORT;
  case byte of
   0:(SymbolicLinkReparseBuffer:packed record
       SubstituteNameOffset:USHORT;
       SubstituteNameLength:USHORT;
       PrintNameOffset     :USHORT;
       PrintNameLength     :USHORT;
       Flags               :ULONG;
       PathBuffer          :record end; //WCHAR
      end);
   1:(MountPointReparseBuffer:packed record
       SubstituteNameOffset:USHORT;
       SubstituteNameLength:USHORT;
       PrintNameOffset     :USHORT;
       PrintNameLength     :USHORT;
       PathBuffer          :record end; //WCHAR
      end);
   2:(GenericReparseBuffer:packed record
       DataBuffer          :record end; //WCHAR
      end);
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
          DesiredAccess     :ACCESS_MASK;
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

function NtCreateFile(
          FileHandle       :PHandle;
          DesiredAccess    :ACCESS_MASK;
          ObjectAttributes :POBJECT_ATTRIBUTES;
          IoStatusBlock    :PIO_STATUS_BLOCK;
          AllocationSize   :PLARGE_INTEGER;
          FileAttributes   :ULONG;
          ShareAccess      :ULONG;
          CreateDisposition:ULONG;
          CreateOptions    :ULONG;
          EaBuffer         :Pointer;
          EaLength         :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtOpenFile(
          FileHandle      :PHandle;
          DesiredAccess   :ACCESS_MASK;
          ObjectAttributes:POBJECT_ATTRIBUTES;
          IoStatusBlock   :PIO_STATUS_BLOCK;
          ShareAccess     :ULONG;
          OpenOptions     :ULONG
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

function NtFlushBuffersFile(
          FileHandle        :THandle;
          IoStatusBlock     :PIO_STATUS_BLOCK
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

function NtDeviceIoControlFile(
          FileHandle        :THandle;
          Event             :THandle;
          ApcRoutine        :Pointer;
          ApcContext        :Pointer;
          IoStatusBlock     :PIO_STATUS_BLOCK;
          IoControlCode     :ULONG;
          InputBuffer       :Pointer;
          InputBufferLength :ULONG;
          OutputBuffer      :Pointer;
          OutputBufferLength:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtFsControlFile(
          FileHandle        :THandle;
          Event             :THandle;
          ApcRoutine        :Pointer;
          ApcContext        :Pointer;
          IoStatusBlock     :PIO_STATUS_BLOCK;
          FsControlCode     :ULONG;
          InputBuffer       :Pointer;
          InputBufferLength :ULONG;
          OutputBuffer      :Pointer;
          OutputBufferLength:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtQueryDirectoryFile(
          FileHandle          :THandle;
          Event               :THandle;
          ApcRoutine          :Pointer;
          ApcContext          :Pointer;
          IoStatusBlock       :PIO_STATUS_BLOCK;
          FileInformation     :Pointer;
          Length              :ULONG;
          FileInformationClass:DWORD;
          ReturnSingleEntry   :Boolean;
          FileName            :PUNICODE_STRING;
          RestartScan         :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtQueryAttributesFile(
          ObjectAttributes:POBJECT_ATTRIBUTES;
          FileInformation :PFILE_BASIC_INFORMATION
         ):DWORD; stdcall; external 'ntdll';

function NtQueryEaFile(
          FileHandle       :THandle;
          IoStatusBlock    :PIO_STATUS_BLOCK;
          Buffer           :Pointer;
          Length           :ULONG;
          ReturnSingleEntry:Boolean;
          EaList           :Pointer;
          EaListLength     :ULONG;
          EaIndex          :PULONG;
          RestartScan      :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtQueryVolumeInformationFile(
          FileHandle        :THandle;
          IoStatusBlock     :PIO_STATUS_BLOCK;
          FsInformation     :Pointer;
          Length            :ULONG;
          FsInformationClass:ULONG
         ):DWORD; stdcall; external 'ntdll';



function NtCreateEvent(
          EventHandle     :PHandle;
          DesiredAccess   :ACCESS_MASK;
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
          DesiredAccess   :ACCESS_MASK;
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
          DesiredAccess        :ACCESS_MASK;
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

