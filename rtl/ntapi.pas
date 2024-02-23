unit ntapi;

{$mode objfpc}{$H+}

interface

uses
 Windows;

const
 STATUS_SUCCESS                =$00000000;
 STATUS_WAIT_0                 =$00000000;
 STATUS_ABANDONED              =$00000080; //EPERM
 STATUS_ABANDONED_WAIT_0       =$00000080;
 STATUS_USER_APC               =$000000C0; //EINTR
 STATUS_KERNEL_APC             =$00000100; //EINTR
 STATUS_ALERTED                =$00000101; //EINTR
 STATUS_TIMEOUT                =$00000102; //ETIMEDOUT
 STATUS_PENDING                =$00000103; //EWOULDBLOCK
 STATUS_NO_YIELD_PERFORMED     =$40000024;
 STATUS_NO_MORE_FILES          =$80000006;
 STATUS_PARTIAL_COPY           =$8000000D;
 STATUS_ACCESS_VIOLATION       =$C0000005; //EFAULT
 STATUS_INVALID_HANDLE         =$C0000008; //EBADF
 STATUS_INVALID_PARAMETER      =$C000000D; //EINVAL
 STATUS_NO_SUCH_FILE           =$C000000F; //ENOENT
 STATUS_END_OF_FILE            =$C0000011;
 STATUS_NO_MEMORY              =$C0000017; //ENOMEM
 STATUS_CONFLICTING_ADDRESSES  =$C0000018;
 STATUS_ACCESS_DENIED          =$C0000022; //EACCES
 STATUS_DISK_CORRUPT_ERROR     =$C0000032; //EIO
 STATUS_OBJECT_NAME_NOT_FOUND  =$C0000034; //ENOENT
 STATUS_OBJECT_NAME_COLLISION  =$C0000035; //EEXIST
 STATUS_OBJECT_PATH_NOT_FOUND  =$C000003A; //ENOENT
 STATUS_OBJECT_PATH_SYNTAX_BAD =$C000003B; //ENOTDIR
 STATUS_SHARING_VIOLATION      =$C0000043; //EACCES
 STATUS_FILE_LOCK_CONFLICT     =$C0000054; //EWOULDBLOCK
 STATUS_LOCK_NOT_GRANTED       =$C0000055; //EWOULDBLOCK
 STATUS_RANGE_NOT_LOCKED       =$C000007E; //ENOLCK
 STATUS_DISK_FULL              =$C000007F; //ENOSPC
 STATUS_FILE_IS_A_DIRECTORY    =$C00000BA; //EISDIR
 STATUS_NOT_SAME_DEVICE        =$C00000D4; //EXDEV
 STATUS_INSUFFICIENT_RESOURCES =$C000009A; //ENOMEM
 STATUS_DIRECTORY_NOT_EMPTY    =$C0000101; //ENOTEMPTY
 STATUS_FILE_CORRUPT_ERROR     =$C0000102; //EIO
 STATUS_NOT_A_DIRECTORY        =$C0000103; //ENOTDIR
 STATUS_NAME_TOO_LONG          =$C0000106; //ENAMETOOLONG
 STATUS_CANCELLED              =$C0000120;
 STATUS_PAGEFILE_QUOTA_EXCEEDED=$C000012C; //ENOMEM
 STATUS_COMMITMENT_LIMIT       =$C000012D; //ENOMEM
 STATUS_LOCAL_DISCONNECT       =$C000013B;
 STATUS_IO_DEVICE_ERROR        =$C0000185; //EIO
 STATUS_CONNECTION_RESET       =$C000020D;
 STATUS_CONNECTION_REFUSED     =$C0000236;
 STATUS_GRACEFUL_DISCONNECT    =$C0000237;
 STATUS_CONNECTION_ABORTED     =$C0000241;
 STATUS_TOO_MANY_LINKS         =$C0000265; //EMLINK
 STATUS_COMMITMENT_MINIMUM     =$C00002C8; //ENOMEM
 STATUS_CANT_CROSS_RM_BOUNDARY =$C0190038; //EXDEV

 NT_INFINITE=$8000000000000000;

 //ThreadInformationClass
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
 ThreadNameInformation           = 38;
 ThreadSystemThreadInformation   = 40;

 //ProcessInformationClass
 ProcessBasicInformation  =0;
 ProcessQuotaLimits       =1;
 ProcessIoCounters        =2;
 ProcessVmCounters        =3;
 ProcessTimes             =4;
 ProcessPriorityClass     =18;
 ProcessAffinityMask      =21;
 ProcessImageFileName     =27;
 ProcessImageFileNameWin32=43;

 //SystemInformationClass
 SystemTimeAdjustmentInformation      =28;
 SystemHypervisorSharedPageInformation=197;

 //ntapi PriorityClass
 PROCESS_PRIORITY_CLASS_UNKNOWN     =0;
 PROCESS_PRIORITY_CLASS_IDLE        =1;
 PROCESS_PRIORITY_CLASS_NORMAL      =2;
 PROCESS_PRIORITY_CLASS_HIGH        =3;
 PROCESS_PRIORITY_CLASS_REALTIME    =4;
 PROCESS_PRIORITY_CLASS_BELOW_NORMAL=5;
 PROCESS_PRIORITY_CLASS_ABOVE_NORMAL=6;

 //FileInformationClass
 FileBasicInformation            = 4;
 FileStandardInformation         = 5;
 FileInternalInformation         = 6;
 FileEaInformation               = 7;
 FileAccessInformation           = 8;
 FileRenameInformation           =10;
 FileLinkInformation             =11;
 FileNamesInformation            =12;
 FileDispositionInformation      =13;
 FilePositionInformation         =14;
 FileModeInformation             =16;
 FileAlignmentInformation        =17;
 FileAllInformation              =18;
 FileAllocationInformation       =19;
 FileEndOfFileInformation        =20;
 FilePipeInformation             =23;
 FileCompletionInformation       =30;
 FileIdFullDirectoryInformation  =38;
 FileReplaceCompletionInformation=61;

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

 MEM_REPLACE_PLACEHOLDER=$04000;
 MEM_RESERVE_PLACEHOLDER=$40000;

 MEM_COALESCE_PLACEHOLDERS=$01;
 MEM_PRESERVE_PLACEHOLDER =$02;

 MEM_RESET_UNDO=$1000000;

 OBJ_INHERIT         =$00000002;
 OBJ_PERMANENT       =$00000010;
 OBJ_EXCLUSIVE       =$00000020;
 OBJ_CASE_INSENSITIVE=$00000040;
 OBJ_OPENIF          =$00000080;
 OBJ_OPENLINK        =$00000100;
 OBJ_VALID_ATTRIBUTES=$000001F2;

 //ACCESS_MASK
 FILE_CAN_DELETE=$10000;

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

 // Special ByteOffset parameters (NtWriteFile LowPart)
 FILE_WRITE_TO_END_OF_FILE     =$ffffffff; //O_APPEND
 FILE_USE_FILE_POINTER_POSITION=$fffffffe;

 FILE_WRITE_TO_END_OF_FILE_L     :LARGE_INTEGER=(LowPart:FILE_WRITE_TO_END_OF_FILE     ;HighPart:-1);
 FILE_USE_FILE_POINTER_POSITION_L:LARGE_INTEGER=(LowPart:FILE_USE_FILE_POINTER_POSITION;HighPart:-1);

 // FsControlCode
 FSCTL_SET_REPARSE_POINT=$000900A4;
 FSCTL_GET_REPARSE_POINT=$000900A8;

 FSCTL_PIPE_PEEK=$11400c;

 // ReparseTag
 IO_REPARSE_TAG_SYMLINK =$A000000C;

 // ReparseFlags
 SYMLINK_FLAG_RELATIVE  =1;

 // Privileges
 SE_CREATE_SYMBOLIC_LINK_PRIVILEGE=35;

 //NamedPipeType
 FILE_PIPE_BYTE_STREAM_TYPE=$00000000;
 FILE_PIPE_MESSAGE_TYPE    =$00000001;

 //ReadMode
 FILE_PIPE_BYTE_STREAM_MODE=$00000000;
 FILE_PIPE_MESSAGE_MODE    =$00000001;

 //CompletionMode
 FILE_PIPE_QUEUE_OPERATION   =$00000000;
 FILE_PIPE_COMPLETE_OPERATION=$00000001;

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
  MaximumLength:USHORT; //size in byte
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
  FileNameLength:ULONG;      //size in byte
  FileName      :record end; //WCHAR
 end;

 PFILE_COMPLETION_INFORMATION=^FILE_COMPLETION_INFORMATION;
 FILE_COMPLETION_INFORMATION=packed record
  Port:THandle;
  Key :Pointer;
 end;

 PFILE_IO_COMPLETION_INFORMATION=^FILE_IO_COMPLETION_INFORMATION;
 FILE_IO_COMPLETION_INFORMATION=packed record
  KeyContext   :Pointer;
  ApcContext   :Pointer;
  IoStatusBlock:IO_STATUS_BLOCK;
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

 PFILE_LINK_INFORMATION=^FILE_LINK_INFORMATION;
 FILE_LINK_INFORMATION=packed record
  ReplaceIfExists:Boolean;
  _align:array[0..6] of Byte;
  RootDirectory  :THandle;
  FileNameLength :ULONG;      //size in byte
  FileName       :record end; //WCHAR
 end;

 PFILE_RENAME_INFORMATION=PFILE_LINK_INFORMATION;
 FILE_RENAME_INFORMATION=FILE_LINK_INFORMATION;

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
  FileNameLength :ULONG;      //size in byte
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
       SubstituteNameOffset:USHORT;  //offset in byte
       SubstituteNameLength:USHORT;  //size in byte
       PrintNameOffset     :USHORT;  //offset in byte
       PrintNameLength     :USHORT;  //size in byte
       Flags               :ULONG;
       PathBuffer          :record end; //WCHAR
      end);
   1:(MountPointReparseBuffer:packed record
       SubstituteNameOffset:USHORT;  //offset in byte
       SubstituteNameLength:USHORT;  //size in byte
       PrintNameOffset     :USHORT;  //offset in byte
       PrintNameLength     :USHORT;  //size in byte
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

 PIO_COUNTERS=^IO_COUNTERS;
 IO_COUNTERS=packed record
  ReadOperationCount :SIZE_T;
  WriteOperationCount:SIZE_T;
  OtherOperationCount:SIZE_T;
  ReadTransferCount  :SIZE_T;
  WriteTransferCount :SIZE_T;
  OtherTransferCount :SIZE_T;
 end;

 PVM_COUNTERS=^VM_COUNTERS;
 VM_COUNTERS=packed record
  PeakVirtualSize           :SIZE_T;
  VirtualSize               :SIZE_T;
  PageFaultCount            :ULONG;
  _align                    :ULONG;
  PeakWorkingSetSize        :SIZE_T;
  WorkingSetSize            :SIZE_T;
  QuotaPeakPagedPoolUsage   :SIZE_T;
  QuotaPagedPoolUsage       :SIZE_T;
  QuotaPeakNonPagedPoolUsage:SIZE_T;
  QuotaNonPagedPoolUsage    :SIZE_T;
  PagefileUsage             :SIZE_T;
  PeakPagefileUsage         :SIZE_T;
 end;

 PPROCESS_PRIORITY_CLASS=^PROCESS_PRIORITY_CLASS;
 PROCESS_PRIORITY_CLASS=packed record
  Foreground   :Boolean;
  PriorityClass:Byte;
 end;

 PKERNEL_USER_TIMES=^KERNEL_USER_TIMES;
 KERNEL_USER_TIMES=packed record
  CreateTime:LARGE_INTEGER;
  ExitTime  :LARGE_INTEGER;
  KernelTime:LARGE_INTEGER;
  UserTime  :LARGE_INTEGER;
 end;

 PSYSTEM_QUERY_TIME_ADJUST_INFORMATION=^SYSTEM_QUERY_TIME_ADJUST_INFORMATION;
 SYSTEM_QUERY_TIME_ADJUST_INFORMATION=packed record
  TimeAdjustment:ULONG;
  TimeIncrement :ULONG;
  Enable        :ULONG;
 end;

 T_PIPE_PEEK=packed record
  NamedPipeState   :DWORD;
  ReadDataAvailable:DWORD;
  NumberOfMessages :DWORD;
  MessageLength    :DWORD;
  data             :record end;
 end;

function NtClose(Handle:THandle):DWORD; stdcall; external 'ntdll';

function NtDuplicateObject(
          SourceProcessHandle:THandle;
          SourceHandle       :THandle;
          TargetProcessHandle:THandle;
          TargetHandle       :PHandle;
          DesiredAccess      :DWORD;
          HandleAttributes   :ULONG;
          Options            :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtCreateThread(
          hThread           :PHandle;
          DesiredAccess     :ACCESS_MASK;
          ObjectAttributes  :POBJECT_ATTRIBUTES;
          ProcessHandle     :THandle;
          ClientId          :PCLIENT_ID;
          ThreadContext     :PCONTEXT;
          InitialTeb        :PINITIAL_TEB;
          CreateSuspended   :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtOpenThread(
          ThreadHandle    :PHandle;
          DesiredAccess   :ACCESS_MASK;
          ObjectAttributes:POBJECT_ATTRIBUTES;
          ClientId        :PCLIENT_ID
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
          Alertable   :Boolean;
          DelayInterval:PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtWaitForSingleObject(
          ObjectHandle:THandle;
          Alertable   :Boolean;
          TimeOut     :PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtGetContextThread(
          ThreadHandle:THandle;
          Context     :PCONTEXT
         ):DWORD; stdcall; external 'ntdll';

function NtSetContextThread(
          ThreadHandle:THandle;
          Context     :PCONTEXT
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

function NtOpenProcess(
          ProcessHandle   :PHandle;
          DesiredAccess   :DWORD;
          ObjectAttributes:POBJECT_ATTRIBUTES;
          ClientId        :PCLIENT_ID
         ):DWORD; stdcall; external 'ntdll';

function NtSuspendProcess(
          ProcessHandle:THandle
         ):DWORD; stdcall; external 'ntdll';

function NtResumeProcess(
          ProcessHandle:THandle
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
          ProcessHandle           :THandle;
          ProcessInformationClass :DWORD;
          ProcessInformation      :Pointer;
          ProcessInformationLength:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSetInformationThread(
          ThreadHandle           :THandle;
          ThreadInformationClass :DWORD;
          ThreadInformation      :Pointer;
          ThreadInformationLength:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtTerminateProcess(
          ProcessHandle:THandle;
          ExitStatus   :DWORD
         ):DWORD; stdcall; external 'ntdll';

function NtContinue(
          Context:PCONTEXT;
          RaiseAlert:Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtQueryPerformanceCounter(
          Counter,
          Frequency:PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtCreateTimer(
          TimerHandle     :PHandle;
          DesiredAccess   :ACCESS_MASK;
          ObjectAttributes:POBJECT_ATTRIBUTES;
          TimerType       :DWORD
         ):DWORD; stdcall; external 'ntdll';

function NtSetTimer(
          TimerHandle     :THandle;
          DueTime         :PLARGE_INTEGER;
          TimerApcRoutine :Pointer;
          TimerContext    :Pointer;
          ResumeTimer     :Boolean;
          Period          :LONG;
          PreviousState   :PBOOLEAN
         ):DWORD; stdcall; external 'ntdll';

function NtCancelTimer(
          TimerHandle     :THandle;
          CurrentState    :PBOOLEAN
         ):DWORD; stdcall; external 'ntdll';

function NtQueryTimerResolution(
          MinimumResolution:PULONG;
          MaximumResolution:PULONG;
          CurrentResolution:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtSetTimerResolution(
          DesiredResolution:ULONG;
          SetResolution    :Boolean;
          CurrentResolution:PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtQuerySystemInformation(
          SystemInformationClass :ULONG;
          SystemInformation      :Pointer;
          SystemInformationLength:ULONG;
          ReturnLength           :PULONG
         ):DWORD; stdcall; external 'ntdll';

function NtCreateNamedPipeFile(
          FileHandle       :PHandle;
          DesiredAccess    :ACCESS_MASK;
          ObjectAttributes :POBJECT_ATTRIBUTES;
          IoStatusBlock    :PIO_STATUS_BLOCK;
          ShareAccess      :ULONG;
          CreateDisposition:ULONG;
          CreateOptions    :ULONG;
          NamedPipeType    :ULONG;
          ReadMode         :ULONG;
          CompletionMode   :ULONG;
          MaximumInstances :ULONG;
          InboundQuota     :ULONG;
          OutboundQuota    :ULONG;
          DefaultTimeout   :PLARGE_INTEGER
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

function NtLockFile(
          FileHandle     :THandle;
          Event          :THandle;
          ApcRoutine     :Pointer;
          ApcContext     :Pointer;
          IoStatusBlock  :PIO_STATUS_BLOCK;
          ByteOffset     :PLARGE_INTEGER;
          Length         :PLARGE_INTEGER;
          Key            :ULONG;
          FailImmediately:Boolean;
          ExclusiveLock  :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtUnlockFile(
          FileHandle     :THandle;
          IoStatusBlock  :PIO_STATUS_BLOCK;
          ByteOffset     :PLARGE_INTEGER;
          Length         :PLARGE_INTEGER;
          Key            :ULONG
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


function NtCreateIoCompletion(
          IoCompletionHandle:PHandle;
          DesiredAccess     :ACCESS_MASK;
          ObjectAttributes  :POBJECT_ATTRIBUTES;
          Count             :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtRemoveIoCompletionEx(
          IoCompletionHandle     :THandle;
          IoCompletionInformation:PFILE_IO_COMPLETION_INFORMATION;
          Count                  :ULONG;
          NumEntriesRemoved      :PULONG;
          Timeout                :PLARGE_INTEGER;
          Alertable              :Boolean
         ):DWORD; stdcall; external 'ntdll';

function NtCreateEvent(
          EventHandle     :PHandle;
          DesiredAccess   :ACCESS_MASK;
          ObjectAttributes:POBJECT_ATTRIBUTES;
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
          ObjectAttributes:POBJECT_ATTRIBUTES;
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
          ObjectAttributes     :POBJECT_ATTRIBUTES;
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

function NtMapViewOfSectionEx(
          SectionHandle         :THandle;
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          SectionOffset         :PLARGE_INTEGER;
          ViewSize              :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer;
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtUnmapViewOfSection(
          ProcessHandle:THandle;
          BaseAddress  :Pointer
         ):DWORD; stdcall; external 'ntdll';

function NtUnmapViewOfSectionEx(
          ProcessHandle:THandle;
          BaseAddress  :Pointer;
          Flags        :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtAllocateVirtualMemory(
          ProcessHandle        :THandle;
          BaseAddress          :PPointer;
          ZeroBits             :ULONG_PTR;
          RegionSize           :PULONG_PTR;
          AllocationType       :ULONG;
          Protect              :ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtAllocateVirtualMemoryEx(
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          RegionSize            :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer;
          ExtendedParameterCount:ULONG
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
          BaseAddress           :Pointer;
          MemoryInformationClass:DWORD;
          Buffer                :Pointer;
          Length                :ULONG_PTR;
          ResultLength          :PULONG_PTR
         ):DWORD; stdcall; external 'ntdll';

//

function RtlAcquirePrivilege(
          Privilege             :PULONG;
          NumPriv               :ULONG;
          Flags                 :ULONG;
          ReturnedState         :PPointer
         ):DWORD; stdcall; external 'ntdll';

function RtlReleasePrivilege(
          ReturnedState:Pointer
         ):DWORD; stdcall; external 'ntdll';


implementation

end.

