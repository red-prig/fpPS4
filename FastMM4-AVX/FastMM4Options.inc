{

Fast Memory Manager: Options Include File

Set the default options for FastMM here.

}

{$define IgnoreMemoryAllocatedBefore}

{---------------------------Miscellaneous Options-----------------------------}

{Enable Align16Bytes define to align all data blocks on 16 byte boundaries,
 or enable Align32Bytes define to align all blocks on 32 byte boundaries,
 so aligned SSE instructions can be used safely.

 If neither of these options are enabled, then some of the
 smallest block sizes will be 8-byte aligned instead which may result in a
 reduction in memory usage.

 Even when small blocks are aligned by 8 bytes
 (no Align16Bytes or Align32Bytes are defined),
 Medium and large blocks are always 16-byte aligned.

 If you enable AVX, then the alignment will always be 32 bytes. However, if your
 CPU supports "Fast Short REP MOVSB" (Ice Lake or newer), you can disable AVX,
 and align by just 8 bytes, and this may even be faster because less memory is
 wasted on alignment}

{.$define Align16Bytes}
{.$define Align32Bytes}


{Enable to use faster fixed-size move routines when upsizing small blocks.
 These routines are much faster than the Borland RTL move procedure since they
 are optimized to move a fixed number of bytes. This option may be used
 together with the FastMove library for even better performance.
 You may define "DontUseCustomFixedSizeMoveRoutines" to undef this option}

{$define UseCustomFixedSizeMoveRoutines}


{Enable this option to use an optimized procedure for moving a memory block of
 an arbitrary size. Disable this option when using the Fastcode move
 ("FastMove") library. Using the Fastcode move library allows your whole
 application to gain from faster move routines, not just the memory manager. It
 is thus recommended that you use the Fastcode move library in conjunction with
 this memory manager and disable this option.
 You may define "DontUseCustomVariableSizeMoveRoutines" to undef this option.}

{$define UseCustomVariableSizeMoveRoutines}


{Enable this option to only install FastMM as the memory manager when the
 application is running inside the Delphi IDE. This is useful when you want
 to deploy the same EXE that you use for testing, but only want the debugging
 features active on development machines. When this option is enabled and
 the application is not being run inside the IDE debugger, then the default
 Delphi memory manager will be used (which, since Delphi 2006, is FastMM
 without FullDebugMode.}

{.$define InstallOnlyIfRunningInIDE}


{Due to QC#14070 ("Delphi IDE attempts to free memory after the shutdown code
 of borlndmm.dll has been called"), FastMM cannot be uninstalled safely when
 used inside a replacement borlndmm.dll for the IDE. Setting this option will
 circumvent this problem by never uninstalling the memory manager.}

{.$define NeverUninstall}


{Set this option when you use runtime packages in this application or library.
 This will automatically set the "AssumeMultiThreaded" option. Note that you
 have to ensure that FastMM is finalized after all live pointers have been
 freed - failure to do so will result in a large leak report followed by a lot
 of A/Vs. (See the FAQ for more detail.) You may have to combine this option
 with the NeverUninstall option.}

{.$define UseRuntimePackages}



{-----------------------Concurrency Management Options------------------------}

{Enable to always assume that the application is multithreaded. Enabling this
 option will cause a significant performance hit with single threaded
 applications. Enable if you are using multi-threaded third-party tools that do
 not properly set the IsMultiThread variable. Also set this option if you are
 going to share this memory manager between a single threaded application and a
 multi-threaded DLL. Since the primary goal of FastMM4-AVX is improvement in
 multi-threaded applications, this option is enabled by default. However, if you
 know for sure that your application is Single-Threaded, undefine this to improve
 performance - this will save yo from unnecessary locking!
 You can define "ForceSingleThreaded" to undefine "AssumeMultiThreaded"}

{$define AssumeMultiThreaded}


{Enable to always assume that the CPU supports "pause" instruction and Windows
supports SwitchToThread() API call. This option has no effect for 64-bit target,
since it is always assumed under 64-bit that both "pause" and SwitchToThread()
are supported. So it is only relevant for 32-bit platforms with very old CPUs.
If you are sure that "pause" and SwithchToThread() are always avaialbe, the
program may skip checking and improve speed. However, if you define
"DisablePauseAndSwitchToThread", then "AssumePauseAndSwitchToThreadAvailable"
will be automatically undefined}

{.$define AssumePauseAndSwitchToThreadAvailable}


{ If you disable "pause" and SwitchToThread() by defining the
DisablePauseAndSwitchToThread, then EnterCriticalSection/LeaveCriticalSection
calls will be used instead }

{.$define DisablePauseAndSwitchToThread}


{Enable this option to not call Sleep when a thread contention occurs. This
 option will improve performance if the ratio of the number of active threads
 to the number of CPU cores is low (typically < 2). With this option set a
 thread will usually enter a "busy waiting" loop instead of relinquishing its
 timeslice when a thread contention occurs, unless UseSwitchToThread is
 also defined (see below) in which case it will call SwitchToThread instead of
 Sleep.

*** Note: This option was added in FastMM 4 version Version 4.68
    on 3 July 2006, is provided only if you wish to restore old
    functionality (e.g. for testing, etc.), and is not recommended
    for FastMM4-AVX, since this it provides suboptimal performance compare
    to the new locking mechanism implemented in the FastMM4-AVX.
This option has no effect when SmallBlocksLockedCriticalSection/
MediumBlocksLockedCriticalSection/LargeBlocksLockedCriticalSection is enabled}

{.$define NeverSleepOnThreadContention}


{Set this option to call SwitchToThread instead of sitting in a "busy waiting"
 loop when a thread contention occurs. This is used in conjunction with the
 NeverSleepOnThreadContention option, and has no effect unless
 NeverSleepOnThreadContention is also defined. This option may improve
 performance with many CPU cores and/or threads of different priorities. Note
 that the SwitchToThread API call is only available on Windows 2000 and later,
 but FastMM4 loads it dynamically, so it would not fail even under very old
 versions of Windows.

*** Note: This option was added in FastMM 4 version Version 4.97
    on 30 September 2010, is provided only if you wish to restore old
    functionality (e.g. for testing, etc.), and is not recommended
    for FastMM4-AVX, since this it provides suboptimal performance compare
    to the new locking mechanism implemented in the FastMM4-AVX.
This option has no effect when SmallBlocksLockedCriticalSection/
MediumBlocksLockedCriticalSection/LargeBlocksLockedCriticalSection is enabled}

{.$define UseSwitchToThread}


{This option uses a simpler instruction to acquire a lock: "lock xchg", instead
of "lock cmpxchg" used in earlier versions of FastMM4: there is actually no
reason to use "cmpxchg", because  the simple instruction - "xchg" - perfectly
suits our need. Although "xchg" has exactly the same latency and costs in terms
of CPU cycles as "cmpxghg", it is just simper way to do the lock that we need,
and, according to the Occam's razor principle, simple things are better. If you
wish to restore old functionality of FastMM4 version 4.992, disable this option,
e.g. by defining "DontUseSimplifiedInterlockedExchangeByte" }

{$define SimplifiedInterlockedExchangeByte}


{These 3 options make FastMM4-AVX use a new approach to waiting for a lock:
CriticalSections or "pause"-based spin-wait loops instead of Sleep() or
SwitchToThread().
Using Sleep(0) or SwitchToThread() while waiting for a lock is a default
approach in the original version of FastMM.
With the new approach, the Sleep() will never be called, and SwitchToThread()
may only be called after 5000 cycles of "pause"-based spin-wait loop.
Testing has shown that the new approach provides significant gain in
multi-threaded scenarios, especially in situations when the number of threads
working with the memory manager is the same or higher than the number of
physical cores.
Critical Sections or "pause"-based spin-wait loops implemented as
"test, test-and-set" are much more CPU-friendly and have definitely lower
latency than Sleep() or SwitchToThread().

When these options are enabled, FastMM4-AVX checks:
 - whether the CPU supports SSE2 and thus the "pause" instruction, and
 - whether the operating system has the SwitchToThread() API call, and,
if both of the above conditions are met, uses
"pause"-based spin-wait loops for 5000 iterations and then
SwitchToThread() instead of critical sections; If a CPU doesn't have the
"pause" instrcution or Windows doesn't have the SwitchToThread() API
function, it will use EnterCriticalSection/LeaveCriticalSection.

If you wound not define the 3 options below, you will get the locking
mechanism from the original FastMM4.

Since these options are defined by default, you may define
DontUseSmallBlocksLockedCriticalSection,
DontUseMediumBlocksLockedCriticalSection and
DontUseLargeBlocksLockedCriticalSection, respectively, to undefine these
options }

{$define SmallBlocksLockedCriticalSection}
{$define MediumBlocksLockedCriticalSection}
{$define LargeBlocksLockedCriticalSection}



{ Use this option if you need that releasing a lock on data structure,
i.e. writing to a synchronization variable, to use bus-locking memory store
(lock xchg) rather than just the normal memory store (mov).
Using bus-locking memory store to release a lock on data structure is
an old approach of the original FastMM4, and is not recommended
for FastMM4-AVX. Look for "using normal memory store" in the comment section
at the beginning of the main .pas file for the discussion }

{.$define InterlockedRelease}


{-----------------------------Debugging Options-------------------------------}

{Enable this option to suppress the generation of debug info for the
 FastMM4.pas unit. This will prevent the integrated debugger from stepping into
 the memory manager code.}

 {$define NoDebugInfo}


{Enable this option to suppress the display of all message dialogs. This is
 useful in service applications that should not be interrupted.}

 {$define NoMessageBoxes}


{Set this option to use the Windows API OutputDebugString procedure to output
 debug strings on startup/shutdown and when errors occur.}

 {.$define UseOutputDebugString}


{Set this option to use the assembly language version of GetMem and FreeMem
 which is faster than the pascal version. Disable only for debugging purposes.
 Setting the CheckHeapForCorruption option automatically disables this option.
 You may also define DontUseASMVersion to disable this option. }

{$define ASMVersion}


{Set this option to disable any inline assembly at all. However, it would not
be able to use efficient locking without inline assembly.}

{.$define PurePascal}


{Define the "EnableAsmCodeAlign" to allow using ".align" assembler
 directive for the 32-bit or 64-bit inline assembly.
 Delphi 32-bit or 64-bit compiler incorrectly encodes conditional jumps
 (used 6-byte instructions instead of just 2 bytes, so it prevents branch
 predicions. So for Embarcadero (former Borland) 32-bit or 64-bit
 Delphi, EnableAsmCodeAlign will have no effect. However, undre FreePascal
 it turns on using the ".align". To force using it under Delphi, define
 "ForceAsmCodeAlign".

 To disable this option, define "DisableAsmCodeAlign",
 which affects both "EnableAsmCodeAlign" and "ForceAsmCodeAlign" }


{$define EnableAsmCodeAlign}
{.$define ForceAsmCodeAlign}


{Allow pascal code alignment, use "DisablePasCodeAlign" to disable this option}

{$define PasCodeAlign}


{FastMM always catches attempts to free the same memory block twice, however it
 can also check for corruption of the memory heap (typically due to the user
 program overwriting the bounds of allocated memory). These checks are
 expensive, and this option should thus only be used for debugging purposes.
 If this option is set then the ASMVersion option is automatically disabled.}

{.$define CheckHeapForCorruption}


{Enable this option to catch attempts to perform MM operations after FastMM has
 been uninstalled. With this option set when FastMM is uninstalled it will not
 install the previous MM, but instead a dummy MM handler that throws an error
 if any MM operation is attempted. This will catch attempts to use the MM
 after FastMM has been uninstalled.
 You may define "DontDetectMMOperationsAfterUninstall" to disable this option}

{.$define DetectMMOperationsAfterUninstall}


{Set the following option to do extensive checking of all memory blocks. All
 blocks are padded with both a header and trailer that are used to verify the
 integrity of the heap. Freed blocks are also cleared to ensure that they
 cannot be reused after being freed. This option slows down memory operations
 dramatically and should only be used to debug an application that is
 overwriting memory or reusing freed pointers. Setting this option
 automatically enables CheckHeapForCorruption and disables ASMVersion.
 Very important: If you enable this option your application will require the
 FastMM_FullDebugMode.dll library. If this library is not available you will
 get an error on startup.}

{.$define FullDebugMode}


  {Set this option to perform "raw" stack traces, i.e. check all entries on the
   stack for valid return addresses. Note that this is significantly slower
   than using the stack frame tracing method, but is usually more complete. Has
   no effect unless FullDebugMode is enabled}

  {.$define RawStackTraces}


  {Set this option to check for user code that uses an interface of a freed
   object. Note that this will disable the checking of blocks modified after
   being freed (the two are not compatible). This option has no effect if
   FullDebugMode is not also enabled.}

  {.$define CatchUseOfFreedInterfaces}


  {Set this option to log all errors to a text file in the same folder as the
   application. Memory errors (with the FullDebugMode option set) will be
   appended to the log file. Has no effect if "FullDebugMode" is not set.}

  {.$define LogErrorsToFile}


  {Set this option to log all memory leaks to a text file in the same folder as
   the application. Memory leak reports (with the FullDebugMode option set)
   will be appended to the log file. Has no effect if "LogErrorsToFile" and
   "FullDebugMode" are not also set. Note that usually all leaks are always
   logged, even if they are "expected" leaks registered through
   AddExpectedMemoryLeaks. Expected leaks registered by pointer may be excluded
   through the HideExpectedLeaksRegisteredByPointer option.}

  {.$define LogMemoryLeakDetailToFile}


  {Deletes the error log file on startup. No effect if LogErrorsToFile is not
   also set.}

  {.$define ClearLogFileOnStartup}


  {Loads the FASTMM_FullDebugMode.dll dynamically. If the DLL cannot be found
   then stack traces will not be available. Note that this may cause problems
   due to a changed DLL unload order when sharing the memory manager. Use with
   care.}

  {.$define LoadDebugDLLDynamically}
	
  {.$define MemoryLoadLibrarySupport}
  {Uses MemoryLoadLibrary to allow loading FastMM_FullDebugMode.dll from 
  Ressource if no DLL is found.}
 
  {.$define IncludeResource}
  (*Include Platform depending FullDebugMode.dll in JCLDebug-Mode.
	This has no effect without LoadDebugDLLDynamically or MemoryLoadLibrarySupport.
  If not defined linking can be done from Project-File:
  {$IFDEF Win64}
    {$R FastMM_FullDebugMode64.res}
  {$ELSE}
    {$R FastMM_FullDebugMode.res}
  {$ENDIF}	
  *)
	
  {.$define IncludeResource_madExcept}
  (*Include Platform depending FullDebugMode.dll in madExcept-Mode.
	This has no effect without LoadDebugDLLDynamically or MemoryLoadLibrarySupport.
  If not defined linking can be done from Project-File:
  {$IFDEF Win64}
    {$R FastMM_FullDebugMode_madExcept64.res}
  {$ELSE}
    {$R FastMM_FullDebugMode_madExcept.res}
  {$ENDIF}
  *)		

    {.$define DoNotInstallIfDLLMissing}
    {If the FastMM_FullDebugMode.dll file is not available then FastMM will not
     install itself. No effect unless FullDebugMode and LoadDebugDLLDynamically
     are also defined.}

    {.$define RestrictDebugDLLLoadPath}
    {Allow to load debug dll only from host module directory.}


  {FastMM usually allocates large blocks from the topmost available address and
   medium and small blocks from the lowest available address (This reduces
   fragmentation somewhat). With this option set all blocks are always
   allocated from the highest available address. If the process has a >2GB
   address space and contains bad pointer arithmetic code, this option should
   help to catch those errors sooner.}

  {.$define AlwaysAllocateTopDown}


  {Disables the logging of memory dumps together with the other detail for
   memory errors.}

  {.$define DisableLoggingOfMemoryDumps}


  {If FastMM encounters a problem with a memory block inside the FullDebugMode
   FreeMem handler then an "invalid pointer operation" exception will usually
   be raised. If the FreeMem occurs while another exception is being handled
   (perhaps in the try.. finally code) then the original exception will be
   lost. With this option set FastMM will ignore errors inside FreeMem when an
   exception is being handled, thus allowing the original exception to
   propagate.}

  {.$define SuppressFreeMemErrorsInsideException}


  {Adds support for notification of memory manager events in FullDebugMode.
   With this define set, the application may assign the OnDebugGetMemFinish,
   OnDebugFreeMemStart, etc. callbacks in order to be notified when the
   particular memory manager event occurs.}

  {.$define FullDebugModeCallBacks}



{---------------------------Memory Leak Reporting-----------------------------}

{Set the option EnableMemoryLeakReporting to enable reporting of memory leaks.
Combine it with the two options below for further fine-tuning.}

{$ifndef DisableMemoryLeakReporting}
{.$define EnableMemoryLeakReporting}
{$endif}


  {Set this option to suppress the display and logging of expected memory leaks
   that were registered by pointer. Leaks registered by size or class are often
   ambiguous, so these expected leaks are always logged to file (in
   FullDebugMode with the LogMemoryLeakDetailToFile option set) and are never
   hidden from the leak display if there are more leaks than are expected.}

  {$define HideExpectedLeaksRegisteredByPointer}


  {Set this option to require the presence of the Delphi IDE to report memory
   leaks. This option has no effect if the option "EnableMemoryLeakReporting"
   is not also set.}

  {.$define RequireIDEPresenceForLeakReporting}


  {Set this option to require the program to be run inside the IDE debugger to
   report memory leaks. This option has no effect if the option
   "EnableMemoryLeakReporting" is not also set. Note that this option does not
   work with libraries, only EXE projects.}

  {$define RequireDebuggerPresenceForLeakReporting}


  {Set this option to require the presence of debug info ($D+ option) in the
   compiled unit to perform memory leak checking. This option has no effect if
   the option "EnableMemoryLeakReporting" is not also set.}

  {.$define RequireDebugInfoForLeakReporting}


  {Set this option to enable manual control of the memory leak report. When
   this option is set the ReportMemoryLeaksOnShutdown variable (default = false)
   may be changed to select whether leak reporting should be done or not. When
   this option is selected then both the variable must be set to true and the
   other leak checking options must be applicable for the leak checking to be
   done.}

  {.$define ManualLeakReportingControl}


  {Set this option to disable the display of the hint below the memory leak
   message.}

  {.$define HideMemoryLeakHintMessage}


  {Set this option to use QualifiedClassName equivalent instead of ClassName
   equivalent during memory leak reporting.
   This is useful for duplicate class names (like EConversionError, which is in
   units Data.DBXJSONReflect, REST.JsonReflect and System.ConvUtils,
   or TClipboard being in Vcl.Clibprd and WinAPI.ApplicationModel.DataTransfer }

  {$define EnableMemoryLeakReportingUsesQualifiedClassName}


{--------------------------Instruction Set Options----------------------------}

{Set this option to enable the use of MMX instructions. Disabling this option
 will result in a slight performance hit, but will enable compatibility with
 AMD K5, Pentium I and earlier CPUs. MMX is currently only used in the variable
 size move routines, so if UseCustomVariableSizeMoveRoutines is not set then
 this option has no effect.}

{$define EnableMMX}


{$ifndef DontForceMMX}

  {Set this option (ForceMMX) to force the use of MMX instructions without checking
   whether the CPU supports it. If this option is disabled then the CPU will be
   checked for compatibility first, and if MMX is not supported it will fall
   back to the FPU move code. Has no effect unless EnableMMX is also set.}
  {$define ForceMMX}

{$endif}

{$ifndef DisableAVX}

  {Set this option (EnableAVX) to enable use of AVX instructions under 64-bit mode.
  This option has no effect under 32-bit mode. If enabled, the code will check
  whether the CPU supports AVX or AVX2, and, if yes, will use the 32-byte YMM
  registers for faster memory copy. Besides that, if this option is enabled,
  all allocated memory blocks will be aligned by 32 bytes, that will incur
  addition memory consumption overhead. Besides that, with this option, memory
  copy will be slightly more secure, because all XMM/YMM registers used to copy
  memory will be cleared by vxorps/vpxor at the end of a copy routine, so the
  leftovers of the copied memory data will not be kept in the XMM/YMM registers
  and will not be exposed. This option properly handles AVX-SSE transitions to not
  incur the transition penalties, only calls vzeroupper under AVX1, but not under
  AVX2, since it slows down subsequent SSE code under Kaby Lake}

  {$define EnableAVX}

{$endif}


{$ifdef EnableAVX}
  {If AVX is enabled, you can optionally disable one or more
  of the following AVX modes:
  - the first version - initial AVX (DisableAVX1); or
  - the second version AVX2 (DisableAVX2); or
  - AVX-512 (DisableAVX512);
  but you cannot disable all of the above modes at once.

  If you define DisableAVX1, it will not add to FastMM4 the instructions from
  the initial (first) version of the Advanced Vector Extensions instruction set,
  officially called just "AVX", proposed by Intel in March 2008 and first
  supported by Intel with the Sandy Bridge processor shipping in Q1 2011
  and later, on by AMD with the Bulldozer processor shipping in Q3 2011.

  If you define DisableAVX2, it will not add to FastMM4 the instructions from
  the second version of the Advanced Vector Extensions - officially called
  "AVX2", also known as Haswell New Instructions, which is an expansion of the
  AVX instruction set introduced in Intel's Haswell microarchitecture.
  Intel has shipped first processors with AVX2 on June 2, 2013: Core i7 4770,
  Core i5 4670, etc., and AMD has shipped first processors with AVX in Q2 2015
  (Carrizo processor). AMD Ryzen processor (Q1 2017) also supports AVX2.
  We use separate code for AVX1 and AVX2 because AVX2 doesn't use "vzeroupper"
  and uses the new, faster instruction "vpxor" which was not available in the
  initial AVX, which, in its turn, uses "vxorps" and "vzeroupper" before and
  after any AVX code to counteract the AVX-SSE transition penalties.
  FastMM4 checks whether AVX2 is supported by the CPU, and, if supported, never
  calls AVX1 functions, since calling "vzeroupper" even once in a thread
  significantly slows down all subsequent SSE code, which is not documented:
  neither in the Intel 64 and IA-32 Architectures Software Developers Manual
  nor in the Intel 64 and IA-32 Architectures Optimization Reference Manual.

  The code of AVX1 is grouped separately from the code of AVX2, to not scatter
  the cache}


  {.$define DisableAVX1}
  {.$define DisableAVX2}
  {$define DisableAVX512}
{$endif}


{$ifndef DisableERMS}

  {Set this option (EnableERMS) to enable Enhanced Rep Movsb/Stosb CPU feature,
  which improves speed of medium and large block memory copy
  under 32-bit or 64-bit modes after checking the corresponding CPUID bit}

  {$define EnableERMS}

{$endif}


{$ifndef DisableFSRM}

  {Set this option (EnableFSRM) to enable Fast Short REP MOVSB CPU feature,
  introduced by the Ice Lake microarchitecture, which improves speed of small
  block memory copy under 64-bit mode after checking the corresponding CPUID bit}

  {$define EnableFSRM}

{$endif}

{$ifndef DisableWaitPKG}

  {Set this option (EnableWaitPKG) to enable user mode wait (WaitPKG)
  instructions: umonitor/umwait, introduced by Alder Lake microarchitecture,
  launched in 2021, to wait for a synchronization variable
  instead of pause-based loop;
  however these instuctions for FastMM synchronization are not as efficient
  as pause-based loop, so only enable it if your tests show clear benefit
  in your scenarios }

  {.$define EnableWaitPKG}
{$endif}

{-----------------------Memory Manager Sharing Options------------------------}

{Allow sharing of the memory manager between a main application and DLLs that
 were also compiled with FastMM. This allows you to pass dynamic arrays and
 long strings to DLL functions provided both are compiled to use FastMM.
 Sharing will only work if the library that is supposed to share the memory
 manager was compiled with the "AttemptToUseSharedMM" option set. Note that if
 the main application is single threaded and the DLL is multi-threaded that you
 have to set the IsMultiThread variable in the main application to true or it
 will crash when a thread contention occurs. Note that statically linked DLL
 files are initialized before the main application, so the main application may
 well end up sharing a statically loaded DLL's memory manager and not the other
 way around. }

{.$define ShareMM}


  {Allow sharing of the memory manager by a DLL with other DLLs (or the main
   application if this is a statically loaded DLL) that were also compiled with
   FastMM. Set this option with care in dynamically loaded DLLs, because if the
   DLL that is sharing its MM is unloaded and any other DLL is still sharing
   the MM then the application will crash. This setting is only relevant for
   DLL libraries and requires ShareMM to also be set to have any effect.
   Sharing will only work if the library that is supposed to share the memory
   manager was compiled with the "AttemptToUseSharedMM" option set. Note that
   if DLLs are statically linked then they will be initialized before the main
   application and then the DLL will in fact share its MM with the main
   application. This option has no effect unless ShareMM is also set.}

  {.$define ShareMMIfLibrary}


{Define this to attempt to share the MM of the main application or other loaded
 DLLs in the same process that were compiled with ShareMM set. When sharing a
 memory manager, memory leaks caused by the sharer will not be freed
 automatically. Take into account that statically linked DLLs are initialized
 before the main application, so set the sharing options accordingly.}

{.$define AttemptToUseSharedMM}


{Define this to enable backward compatibility for the memory manager sharing
 mechanism used by Delphi 2006 and 2007, as well as older FastMM versions.}

 {$define EnableBackwardCompatibleMMSharing}



{-----------------------Security Options------------------------}

{Windows clears physical memory before reusing it in another process. However,
 it is not known how quickly this clearing is performed, so it is conceivable
 that confidential data may linger in physical memory longer than absolutely
 necessary. If you're paranoid about this kind of thing, enable this option to
 clear all freed memory before returning it to the operating system. Note that
 this incurs a noticeable performance hit.}

{.$define ClearMemoryBeforeReturningToOS}


{With this option enabled freed memory will immediately be cleared inside the
 FreeMem routine. This incurs a big performance hit, but may be worthwhile for
 additional peace of mind when working with highly sensitive data. This option
 supersedes the ClearMemoryBeforeReturningToOS option.}

{.$define AlwaysClearFreedMemory}



{----------------------------Lock Contention Logging--------------------------}

{Define this to lock stack traces for all occasions where GetMem/FreeMem
 go to sleep because of lock contention (IOW, when memory manager is already
 locked by another thread). At the end of the program execution top 10 sites
 (locations with highest occurrence) will be logged to the _MemoryManager_EventLog.txt
 file.
 This options works with FullDebugMode or without it, but requires
 FastMM_FullDebugMode.dll to be present in both cases.}

{.$define LogLockContention}

{If a block cannot be released immediately during a FreeMem call the block will
 added to a list of blocks that will be freed later, either in the background cleanup
 thread or during the next call to FreeMem.}

{.$define UseReleaseStack}

{--------------------------------Option Grouping------------------------------}

{Enabling this option enables FullDebugMode, InstallOnlyIfRunningInIDE and
 LoadDebugDLLDynamically. Consequently, FastMM will install itself in
 FullDebugMode if the application is being debugged inside the Delphi IDE.
 Otherwise the default Delphi memory manager will be used (which is equivalent
 to the non-FullDebugMode FastMM since Delphi 2006.)}

{.$define FullDebugModeInIDE}


{Combines the FullDebugMode, LoadDebugDLLDynamically and
 DoNotInstallIfDLLMissing options. Consequently FastMM will only be installed
 (In FullDebugMode) when the FastMM_FullDebugMode.dll file is available. This
 is useful when the same executable will be distributed for both debugging as
 well as deployment.}

{.$define FullDebugModeWhenDLLAvailable}


{Group the options you use for release and debug versions below}
{$ifdef Release}
  {Specify the options you use for release versions below}
  {.$undef FullDebugMode}
  {.$undef CheckHeapForCorruption}
  {.$define ASMVersion}
  {.$undef EnableMemoryLeakReporting}
  {.$undef UseOutputDebugString}
{$else}
  {Specify the options you use for debugging below}
  {.$define FullDebugMode}
  {.$define EnableMemoryLeakReporting}
  {.$define UseOutputDebugString}
{$endif}

{--------------------Compilation Options For borlndmm.dll---------------------}
{If you're compiling the replacement borlndmm.dll, set the defines below
 for the kind of dll you require.}

{Set this option when compiling the borlndmm.dll}
{.$define borlndmmdll}

{Set this option if the dll will be used by the Delphi IDE}
{.$define dllforide}

{Set this option if you're compiling a debug dll}
{.$define debugdll}

{Do not change anything below this line}
{$ifdef borlndmmdll}
  {$define AssumeMultiThreaded}
  {$undef HideExpectedLeaksRegisteredByPointer}
  {$undef RequireDebuggerPresenceForLeakReporting}
  {$undef RequireDebugInfoForLeakReporting}
  {$define DetectMMOperationsAfterUninstall}
  {$undef ManualLeakReportingControl}
  {$undef ShareMM}
  {$undef AttemptToUseSharedMM}
  {$ifdef dllforide}
    {$define NeverUninstall}
    {$define HideMemoryLeakHintMessage}
    {$undef RequireIDEPresenceForLeakReporting}
    {$ifndef debugdll}
      {$undef EnableMemoryLeakReporting}
    {$endif}
  {$else}
    {$define EnableMemoryLeakReporting}
    {$undef NeverUninstall}
    {$undef HideMemoryLeakHintMessage}
    {$define RequireIDEPresenceForLeakReporting}
  {$endif}
  {$ifdef debugdll}
    {$define FullDebugMode}
    {$define RawStackTraces}
    {$undef CatchUseOfFreedInterfaces}
    {$define LogErrorsToFile}
    {$define LogMemoryLeakDetailToFile}
    {$undef ClearLogFileOnStartup}
  {$else}
    {$undef FullDebugMode}
  {$endif}
{$endif}

{Move BCB related definitions here, because CB2006/CB2007 can build borlndmm.dll
 for tracing memory leaks in BCB applications with "Build with Dynamic RTL"
 switched on}
{------------------------------Patch BCB Terminate----------------------------}
{To enable the patching for BCB to make uninstallation and leak reporting
 possible, you may need to add "BCB" definition
 in "Project Options->Pascal/Delphi Compiler->Defines".
 (Thanks to JiYuan Xie for implementing this.)}

{$ifdef BCB}
  {$ifdef CheckHeapForCorruption}
    {$define PatchBCBTerminate}
  {$else}
    {$ifdef DetectMMOperationsAfterUninstall}
      {$define PatchBCBTerminate}
    {$else}
       {$ifdef EnableMemoryLeakReporting}
         {$define PatchBCBTerminate}
       {$endif}
    {$endif}
  {$endif}

  {$ifdef PatchBCBTerminate}
    {$define CheckCppObjectType}
    {$undef CheckCppObjectTypeEnabled}

    {$ifdef CheckCppObjectType}
      {$define CheckCppObjectTypeEnabled}
    {$endif}

    {Turn off "CheckCppObjectTypeEnabled" option if neither "CheckHeapForCorruption"
     option or "EnableMemoryLeakReporting" option were defined.}
    {$ifdef CheckHeapForCorruption}
    {$else}
      {$ifdef EnableMemoryLeakReporting}
      {$else}
        {$undef CheckCppObjectTypeEnabled}
      {$endif}
    {$endif}
  {$endif}
{$endif}
