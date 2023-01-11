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

 FileStandardInformation  = 5;
 FilePositionInformation  =14;
 FileAllocationInformation=19;
 FileEndOfFileInformation =20;

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

function NtAlertThread(hThread:THandle):DWORD; stdcall; external 'ntdll';
function NtTestAlert():DWORD; stdcall; external 'ntdll';

function NtQueueApcThread(
          hThread:THandle;
          ApcRoutine:Pointer;
          ApcRoutineContext:PTRUINT;
          ApcStatusBlock:Pointer;
          ApcReserved:ULONG
         ):DWORD; stdcall; external 'ntdll';

function NtYieldExecution():DWORD; stdcall; external 'ntdll';

function NtDelayExecution(
          Alertable:BOOL;
          DelayInterval:PLARGE_INTEGER
         ):DWORD; stdcall; external 'ntdll';

function NtWaitForSingleObject(
          ObjectHandle:THandle;
          Alertable:BOOL;
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

function NtContinue(
          Context:PCONTEXT;
          RaiseAlert:BOOL
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
          SetResolution:BOOL;
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

implementation

end.

