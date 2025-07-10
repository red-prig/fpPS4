(*

FastMM4-AVX (efficient synchronization and AVX1/AVX2/AVX512/ERMS/FSRM support for FastMM4)
 - Copyright (C) 2017-2020 Ritlabs, SRL. All rights reserved.
 - Copyright (C) 2020-2023 Maxim Masiutin. All rights reserved.

Written by Maxim Masiutin <maxim@masiutin.com>

Version 1.0.7

This is a fork of the "Fast Memory Manager" (FastMM) v4.993 by Pierre le Riche
(see below for the original FastMM4 description)

What was added to FastMM4-AVX in comparison to the original FastMM4:

 - Efficient synchronization
   - improved synchronization between the threads; proper synchronization
     techniques are used depending on context and availability, i.e., spin-wait
     loops, umonitor / umwait, SwitchToThread, critical sections, etc.;
   - used the "test, test-and-set" technique for the spin-wait loops; this
     technique is recommended by Intel (see Section 11.4.3 "Optimization with
     Spin-Locks" of the Intel 64 and IA-32 Architectures Optimization Reference
     Manual) to determine the availability of the synchronization variable;
     according to this technique, the first "test" is done via the normal
     (non-locking) memory load to prevent excessive bus locking on each
     iteration of the spin-wait loop; if the variable is available upon
     the normal memory load of the first step ("test"), proceed to the
     second step ("test-and-set") which is done via the bus-locking atomic
     "xchg" instruction; however, this two-steps approach of using "test" before
     "test-and-set" can increase the cost for the un-contended case comparing
     to just single-step "test-and-set", this may explain why the speed benefits
     of the FastMM4-AVX are more pronounced when the memory manager is called
     from multiple threads in parallel, while in single-threaded use scenario
     there may be no benefit compared to the original FastMM4;
   - the number of iterations of "pause"-based spin-wait loops is 5000,
     before relinquishing to SwitchToThread();
   - see https://stackoverflow.com/a/44916975 for more details on the
     implementation of the "pause"-based spin-wait loops;
   - using normal memory store to release a lock:
     FastMM4-AVX uses normal memory store, i.e., the "mov" instruction, rather
     then the bus-locking "xchg" instruction to write into the synchronization
     variable (LockByte) to "release a lock" on a data structure,
     see https://stackoverflow.com/a/44959764
     for discussion on releasing a lock;
     you man define "InterlockedRelease" to get the old behavior of the original
     FastMM4.
   - implemented dedicated lock and unlock procedures that operate with
     synchronization variables (LockByte);
     before that, locking operations were scattered throughout the code;
     now the locking functions have meaningful names:
     AcquireLockByte and ReleaseLockByte;
     the values of the lock byte are now checked for validity when
     FullDebugMode or DEBUG is defined, to detect cases when the same lock is
     released twice, and other improper use of the lock bytes;
   - added compile-time options "SmallBlocksLockedCriticalSection",
     "MediumBlocksLockedCriticalSection" and "LargeBlocksLockedCriticalSection"
     which are set by default (inside the FastMM4Options.inc file) as
     conditional defines. If you undefine these options, you will get the
     old locking mechanism of the original FastMM4 based on loops of Sleep() or
     SwitchToThread().

 - AVX, AVX2 or AVX512 instructions for faster memory copy
   - if the CPU supports AVX or AVX2, use the 32-byte YMM registers
     for faster memory copy, and if the CPU supports AVX-512,
     use the 64-byte ZMM registers for even faster memory copy;
   - please note that the effect of using AVX instruction in speed improvement is
     negligible, compared to the effect brought by efficient synchronization;
     sometimes AVX instructions can even slow down the program because of AVX-SSE
     transition penalties and reduced CPU frequency caused by AVX-512
     instructions in some processors; use DisableAVX to turn AVX off completely
     or use DisableAVX1/DisableAVX2/DisableAVX512 to disable separately certain
     AVX-related instruction set from being compiled);
   - if EnableAVX is defined, all memory blocks are aligned by 32 bytes, but
     you can also use Align32Bytes define without AVX; please note that the memory
     overhead is higher when the blocks are aligned by 32 bytes, because some
     memory is lost by padding; however, if your CPU supports
     "Fast Short REP MOVSB" (Ice Lake or newer), you can disable AVX, and align
     by just 8 bytes, and this may even be faster because less memory is wasted
     on alignment;
   - with AVX, memory copy is secure - all XMM/YMM/ZMM registers used to copy
     memory are cleared by vxorps/vpxor, so the leftovers of the copied memory
     are not exposed in the XMM/YMM/ZMM registers;
   - the code attempts to properly handle AVX-SSE transitions to not incur the
     transition penalties, only call vzeroupper under AVX1, but not under AVX2
     since it slows down subsequent SSE code under Skylake / Kaby Lake;
   - on AVX-512, writing to xmm16-xmm31 registers will not affect the turbo
     clocks, and will not impose AVX-SSE transition penalties; therefore, when we
     have AVX-512, we now only use x(y/z)mm16-31 registers.

 - Speed improvements due to code optimization and proper techniques
   - if the CPU supports Enhanced REP MOVSB/STOSB (ERMS), use this feature
     for faster memory copy (under 32 bit or 64-bit) (see the EnableERMS define,
     on by default, use DisableERMS to turn it off);
   - if the CPU supports Fast Short REP MOVSB (FSRM), uses this feature instead
     of AVX;
   - branch target alignment in assembly routines is only used when
     EnableAsmCodeAlign is defined; Delphi incorrectly encodes conditional
     jumps, i.e., use long, 6-byte instructions instead of just short, 2-byte,
     and this may affect branch prediction, so the benefits of branch target
     alignment may not outweigh the disadvantage of affected branch prediction,
     see https://stackoverflow.com/q/45112065
   - compare instructions + conditional jump instructions are put together
     to allow macro-op fusion (which happens since Core2 processors, when
     the first instruction is a CMP or TEST instruction and the second
     instruction is a conditional jump instruction);
   - multiplication and division by a constant, which is a power of 2
     replaced to shl/shr, because Delphi64 compiler doesn't replace such
     multiplications and divisions to shl/shr processor instructions,
     and, according to the Intel Optimization Reference Manual, shl/shr is
     faster than imul/idiv, at least for some processors.

 - Safer, cleaner code with stricter type adherence and better compatibility
   - names assigned to some constants that used to be "magic constants",
     i.e., unnamed numerical constants - plenty of them were present
     throughout the whole code;
   - removed some typecasts; the code is stricter to let the compiler
     do the job, check everything and mitigate probable error. You can
     even compile the code with "integer overflow checking" and
     "range checking", as well as with "typed @ operator" - for safer
     code. Also added round bracket in the places where the typed @ operator
     was used, to better emphasize on who's address is taken;
   - the compiler environment is more flexible now: you can now compile FastMM4
     with, for example, typed "@" operator or any other option. Almost all
     externally-set compiler directives are honored by FastMM except a few
     (currently just one) - look for the "Compiler options for FastMM4" section
     below to see what options cannot be externally set and are always
     redefined by FastMM4 for itself - even if you set up these compiler options
     differently outside FastMM4, they will be silently
     redefined, and the new values will be used for FastMM4 only;
   - the type of one-byte synchronization variables (accessed via "lock cmpxchg"
     or "lock xchg") replaced from Boolean to Byte for stricter type checking;
   - those fixed-block-size memory move procedures that are not needed
     (under the current bitness and alignment combinations) are
     explicitly excluded from compiling, to not rely on the compiler
     that is supposed to remove these function after compilation;
   - added length parameter to what were the dangerous null-terminated string
     operations via PAnsiChar, to prevent potential stack buffer overruns
     (or maybe even stack-based exploitation?), and there some Pascal functions
     also left, the argument is not yet checked. See the "todo" comments
     to figure out where the length is not yet checked. Anyway, since these
     memory functions are only used in Debug mode, i.e., in development
     environment, not in Release (production), the impact of this
     "vulnerability" is minimal (albeit this is a questionable statement);
   - removed all non-US-ASCII characters, to avoid using UTF-8 BOM, for
     better compatibility with very early versions of Delphi (e.g., Delphi 5),
     thanks to Valts Silaputnins;
   - support for Lazarus 1.6.4 with FreePascal (the original FastMM4 4.992
     requires modifications, it doesn't work under Lazarus 1.6.4 with FreePascal
     out-of-the-box, also tested under Lazarus 1.8.2 / FPC 3.0.4 with Win32
     target; later versions should be also supported.

Here are the comparison of the Original FastMM4 version 4.992, with default
options compiled for Win64 by Delphi 10.2 Tokyo (Release with Optimization),
and the current FastMM4-AVX branch ("AVX-br."). Under some multi-threading
scenarios, the FastMM4-AVX branch is more than twice as fast compared to the
Original FastMM4. The tests have been run on two different computers: one
under Xeon E5-2543v2 with 2 CPU sockets, each has 6 physical cores
(12 logical threads) - with only 5 physical core per socket enabled for the
test application. Another test was done under an i7-7700K CPU.

Used the "Multi-threaded allocate, use and free" and "NexusDB"
test cases from the FastCode Challenge Memory Manager test suite,
modified to run under 64-bit.

                         Xeon E5-2543v2 2*CPU      i7-7700K CPU
                        (allocated 20 logical   (8 logical threads,
                         threads, 10 physical    4 physical cores),
                         cores, NUMA), AVX-1          AVX-2

                        Orig.  AVX-br.  Ratio   Orig.  AVX-br. Ratio
                        ------  -----  ------   -----  -----  ------
    02-threads realloc   96552  59951  62.09%   65213  49471  75.86%
    04-threads realloc   97998  39494  40.30%   64402  47714  74.09%
    08-threads realloc   98325  33743  34.32%   64796  58754  90.68%
    16-threads realloc  116273  45161  38.84%   70722  60293  85.25%
    31-threads realloc  122528  53616  43.76%   70939  62962  88.76%
    64-threads realloc  137661  54330  39.47%   73696  64824  87.96%
    NexusDB 02 threads  122846  90380  73.72%   79479  66153  83.23%
    NexusDB 04 threads  122131  53103  43.77%   69183  43001  62.16%
    NexusDB 08 threads  124419  40914  32.88%   64977  33609  51.72%
    NexusDB 12 threads  181239  55818  30.80%   83983  44658  53.18%
    NexusDB 16 threads  135211  62044  43.61%   59917  32463  54.18%
    NexusDB 31 threads  134815  48132  33.46%   54686  31184  57.02%
    NexusDB 64 threads  187094  57672  30.25%   63089  41955  66.50%

The above tests have been run on 14-Jul-2017.

Here are some more test results (Compiled by Delphi 10.2 Update 3):

                         Xeon E5-2667v4 2*CPU       i9-7900X CPU
                        (allocated 32 logical   (20 logical threads,
                         threads, 16 physical    10 physical cores),
                         cores, NUMA), AVX-2          AVX-512

                        Orig.  AVX-br.  Ratio   Orig.  AVX-br. Ratio
                        ------  -----  ------   -----  -----  ------
    02-threads realloc   80544  60025  74.52%   66100  55854  84.50%
    04-threads realloc   80751  47743  59.12%   64772  40213  62.08%
    08-threads realloc   82645  32691  39.56%   62246  27056  43.47%
    12-threads realloc   89951  43270  48.10%   65456  25853  39.50%
    16-threads realloc   95729  56571  59.10%   67513  27058  40.08%
    31-threads realloc  109099  97290  89.18%   63180  28408  44.96%
    64-threads realloc  118589 104230  87.89%   57974  28951  49.94%
    NexusDB 01 thread   160100 121961  76.18%   93341  95807 102.64%
    NexusDB 02 threads  115447  78339  67.86%   77034  70056  90.94%
    NexusDB 04 threads  107851  49403  45.81%   73162  50039  68.39%
    NexusDB 08 threads  111490  36675  32.90%   70672  42116  59.59%
    NexusDB 12 threads  148148  46608  31.46%   92693  53900  58.15%
    NexusDB 16 threads  111041  38461  34.64%   66549  37317  56.07%
    NexusDB 31 threads  123496  44232  35.82%   62552  34150  54.60%
    NexusDB 64 threads  179924  62414  34.69%   83914  42915  51.14%

The above tests (on Xeon E5-2667v4 and i9) have been done on 03-May-2018.

Here is the single-threading performance comparison in some selected
scenarios between FastMM v5.03 dated May 12, 2021 and FastMM4-AVX v1.05
dated May 20, 2021. FastMM4-AVX is compiled with default optinos. This 
test is run on May 20, 2021, under Intel Core i7-1065G7 CPU, Ice Lake
microarchitecture, base frequency: 1.3 GHz, max turbo frequencey: 3.90 GHz, 
4 cores, 8 threads. Compiled under Delphi 10.3 Update 3, 64-bit target. 
Please note that these are the selected scenarios where FastMM4-AVX is 
faster then FastMM5. In other scenarios, especially in multi-threaded 
with heavy contention, FastMM5 is faster.

                                             FastMM5  AVX-br.   Ratio
                                              ------  ------   ------
    ReallocMem Small (1-555b) benchmark         1425    1135   79.65%
    ReallocMem Medium (1-4039b) benchmark       3834    3309   86.31%
    Block downsize                             12079   10305   85.31%
    Address space creep benchmark              13283   12571   94.64%
    Address space creep (larger blocks)        16066   13879   86.39%
    Single-threaded reallocate and use          4395    3960   90.10%
    Single-threaded tiny reallocate and use     8766    7097   80.96%
    Single-threaded allocate, use and free     13912   13248   95.23%

You can find the program, used to generate the benchmark data,
at https://github.com/maximmasiutin/FastCodeBenchmark

You can find the program, used to generate the benchmark data,
at https://github.com/maximmasiutin/FastCodeBenchmark

FastMM4-AVX is released under a dual license, and you may choose to use it
under either the Mozilla Public License 2.0 (MPL 2.1, available from
https://www.mozilla.org/en-US/MPL/2.0/) or the GNU Lesser General Public
License Version 3, dated 29 June 2007 (LGPL 3, available from
https://www.gnu.org/licenses/lgpl.html).

FastMM4-AVX is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

FastMM4-AVX is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with FastMM4-AVX (see license_lgpl.txt and license_gpl.txt)
If not, see <http://www.gnu.org/licenses/>.


FastMM4-AVX Version History:

- 1.0.7 (21 March 2023) - implemented the use of umonitor/umwait instructions;
    thanks to TetzkatLipHoka for the updated FullDebugMode to v1.64
    of the original FastMM4.

- 1.0.6 (25 August 2021) - it can now be compiled with any alignment (8, 16, 32)
    regardless of the target (x86, x64) and whether inline assembly is used
    or not; the "PurePascal" conditional define to disable inline assembly at
    all, however, in this case, efficient locking would not work since it
    uses inline assembly; FreePascal now uses the original FreePascal compiler
    mode, rather than the Delphi compatibility mode as before; resolved many
    FreePascal compiler warnings; supported branch target alignment
    in FreePascal inline assembly; small block types now always have
    block sizes of 1024 and 2048 bytes, while in previous versions
    instead of 1024-byte blocks there were 1056-byte blocks,
    and instead of 2048-byte blocks were 2176-byte blocks;
    fixed Delphi compiler hints for 64-bit Release mode; Win32 and Win64 
    versions compiled under Delphi and FreePascal passed the all the FastCode 
    validation suites.

- 1.05 (20 May 2021) - improved speed of releasing memory blocks on higher thread
    contention. It is also possible to compile FastMM4-AVX without a single
    inline assembly code. Renamed some conditional defines to be self-explaining.
    Rewritten some comments to be meaningful. Made it compile under FreePascal
    for Linux 64-bit and 32-bit. Also made it compile under FreePascal for
    Windows 32-bit and 64-bit. Memory move functions for 152, 184 and 216 bytes
    were incorrect Linux. Move216AVX1 and Move216AVX2 Linux implementation had
    invalid opcodes. Added support for the GetFPCHeapStatus(). Optimizations on
    single-threaded performance. If you define DisablePauseAndSwitchToThread,
    it will use EnterCriticalSection/LeaveCriticalSectin. An attempt to free a
    memory block twice was not caught under 32-bit Delphi. Added SSE fixed block
    copy routines for 32-bit targets. Added support for the "Fast Short REP MOVSB"
    CPU feature. Removed redundant SSE code from 64-bit targets.
- 1.04 (O6 October 2020) - improved use of AVX-512 instructions to avoid turbo
    clock reduction and SSE/AVX transition penalty; made explicit order of
    parameters for GetCPUID to avoid calling convention ambiguity that could
    lead to incorrect use of registers and finally crashes, i.e., under Linux;
    improved explanations and comments, i.e., about the use of the
    synchronization techniques.
- 1.03 (04 May 2018) - minor fixes for the debug mode, FPC compatibility
    and code readability cosmetic fixes.
- 1.02 (07 November 2017) - added and tested support for the AVX-512
    instruction set.
- 1.01 (10 October 2017) - made the source code compile under Delphi5,
    thanks to Valts Silaputnins.
- 1.00 (27 July 2017) - initial revision.


The original FastMM4 description follows:

Fast Memory Manager 4.993

Description:
 A fast replacement memory manager for Embarcadero Delphi Win32 applications
 that scales well under multi-threaded usage, is not prone to memory
 fragmentation, and supports shared memory without the use of external .DLL
 files.

Homepage:
 Version 4: https://github.com/pleriche/FastMM4
 Version 5: https://github.com/pleriche/FastMM5

Advantages:
 - Fast
 - Low overhead. FastMM is designed for an average of 5% and maximum of 10%
   overhead per block.
 - Supports up to 3GB of user mode address space under Windows 32-bit and 4GB
   under Windows 64-bit. Add the "$SetPEFlags $20" option (in curly braces)
   to your .dpr to enable this.
 - Highly aligned memory blocks. Can be configured for either 8-byte, 16-byte
   or 32-byte alignment.
 - Good scaling under multi-threaded applications
 - Intelligent reallocations. Avoids slow memory move operations through
   not performing unnecessary downsizes and by having a minimum percentage
   block size growth factor when an in-place block upsize is not possible.
 - Resistant to address space fragmentation
 - No external DLL required when sharing memory between the application and
   external libraries (provided both use this memory manager)
 - Optionally reports memory leaks on program shutdown. (This check can be set
   to be performed only if Delphi is currently running on the machine, so end
   users won't be bothered by the error message.)
 - Supports Delphi 4 (or later), C++ Builder 4 (or later), Kylix 3.

Usage:
 Delphi:
  Place this unit as the very first unit under the "uses" section in your
  project's .dpr file. When sharing memory between an application and a DLL
  (e.g. when passing a long string or dynamic array to a DLL function), both the
  main application and the DLL must be compiled using this memory manager (with
  the required conditional defines set). There are some conditional defines
  (inside FastMM4Options.inc) that may be used to tweak the memory manager. To
  enable support for a user mode address space greater than 2GB you will have to
  use the EditBin* tool to set the LARGE_ADDRESS_AWARE flag in the EXE header.
  This informs Windows x64 or Windows 32-bit (with the /3GB option set) that the
  application supports an address space larger than 2GB (up to 4GB). In Delphi 6
  and later you can also specify this flag through the compiler directive
  {$SetPEFlags $20}
  *The EditBin tool ships with the MS Visual C compiler.
 C++ Builder 6:
  Refer to the instructions inside FastMM4BCB.cpp.

License:
 This work is copyright Professional Software Development / Pierre le Riche. It
 is released under a dual license, and you may choose to use it under either the
 Mozilla Public License 1.1 (MPL 1.1, available from
 http://www.mozilla.org/MPL/MPL-1.1.html) or the GNU Lesser General Public
 License 2.1 (LGPL 2.1, available from
 http://www.opensource.org/licenses/lgpl-license.php). If you find FastMM useful
 or you would like to support further development, a donation would be much
 appreciated.
 My PayPal account is:
   paypal@leriche.org

Contact Details:
 My contact details are shown below if you would like to get in touch with me.
 If you use this memory manager I would like to hear from you: please e-mail me
 your comments - good and bad.
 E-mail:
   fastmm@leriche.org

Support:
 If you have trouble using FastMM, you are welcome to drop me an e-mail at the
 address above.

Disclaimer:
 FastMM has been tested extensively with both single and multithreaded
 applications on various hardware platforms, but unfortunately, I am not in a
 position to make any guarantees. Use it at your own risk.

Acknowledgements (for version 4):
 - Eric Grange for his RecyclerMM on which the earlier versions of FastMM were
   based. RecyclerMM was what inspired me to try and write my own memory
   manager back in early 2004.
 - Primoz Gabrijelcic for several bugfixes and enhancements.
 - Dennis Christensen for his tireless efforts with the Fastcode project:
   helping to develop, optimize and debug the growing Fastcode library.
 - JiYuan Xie for implementing the leak reporting code for C++ Builder.
 - Sebastian Zierer for implementing the OS X support.
 - Pierre Y. for his suggestions regarding the extension of the memory leak
   checking options.
 - Hanspeter Widmer for his suggestion to have an option to display install and
   uninstall debug messages and moving options to a separate file, as well as
   the new usage tracker.
 - Anders Isaksson and Greg for finding and identifying the "DelphiIsRunning"
   bug under Delphi 5.
 - Francois Malan for various suggestions and bug reports.
 - Craig Peterson for helping me identify the cache associativity issues that
   could arise due to medium blocks always being an exact multiple of 256 bytes.
   Also for various other bug reports and enhancement suggestions.
 - Jarek Karciarz, Vladimir Ulchenko (Vavan) and Bob Gonder for their help in
   implementing the BCB support.
 - Ben Taylor for his suggestion to display the object class of all memory
   leaks.
 - Jean Marc Eber and Vincent Mahon (the Memcheck guys) for the call stack
   trace code and also the method used to catch virtual method calls on freed
   objects.
 - Nahan Hyn for the suggestion to be able to enable or disable memory leak
   reporting through a global variable (the "ManualLeakReportingControl"
   option.)
 - Leonel Togniolli for various suggestions with regard to enhancing the bug
   tracking features of FastMM and other helpful advice.
 - Joe Bain and Leonel Togniolli for the workaround to QC#10922 affecting
   compilation under Delphi 2005.
 - Robert Marquardt for the suggestion to make localisation of FastMM easier by
   having all string constants together.
 - Simon Kissel and Fikret Hasovic for their help in implementing Kylix support.
 - Matthias Thoma, Petr Vones, Robert Rossmair and the rest of the JCL team for
   their debug info library used in the debug info support DLL and also the
   code used to check for a valid call site in the "raw" stack trace code.
 - Andreas Hausladen for the suggestion to use an external DLL to enable the
   reporting of debug information.
 - Alexander Tabakov for various good suggestions regarding the debugging
   facilities of FastMM.
 - M. Skloff for some useful suggestions and bringing to my attention some
   compiler warnings.
 - Martin Aignesberger for the code to use madExcept instead of the JCL library
   inside the debug info support DLL.
 - Diederik and Dennis Passmore for the suggestion to be able to register
   expected leaks.
 - Dario Tiraboschi and Mark Gebauer for pointing out the problems that occur
   when range checking and complete boolean evaluation is turned on.
 - Arthur Hoornweg for notifying me of the image base being incorrect for
   borlndmm.dll.
 - Theo Carr-Brion and Hanspeter Widmer for finding the false alarm error
   message "Block Header Has Been Corrupted" bug in FullDebugMode.
 - Danny Heijl for reporting the compiler error in "release" mode.
 - Omar Zelaya for reporting the BCB support regression bug.
 - Dan Miser for various good suggestions, e.g. not logging expected leaks to
   file, enhancements the stack trace and messagebox functionality, etc.
 - Arjen de Ruijter for fixing the bug in GetMemoryLeakType that caused it
   to not properly detect expected leaks registered by class when in
   "FullDebugMode".
 - Aleksander Oven for reporting the installation problem when trying to use
   FastMM in an application together with libraries that all use runtime
   packages.
 - Kristofer Skaug for reporting the bug that sometimes causes the leak report
   to be shown, even when all the leaks have been registered as expected leaks.
   Also for some useful enhancement suggestions.
 - Guenter Schoch for the "RequireDebuggerPresenceForLeakReporting" option.
 - Jan Schlueter for the "ForceMMX" option.
 - Hallvard Vassbotn for various good enhancement suggestions.
 - Mark Edington for some good suggestions and bug reports.
 - Paul Ishenin for reporting the compilation error when the NoMessageBoxes
   option is set and also the missing call stack entries issue when "raw" stack
   traces are enabled, as well as for the Russian translation.
 - Cristian Nicola for reporting the compilation bug when the
   CatchUseOfFreedInterfaces option was enabled (4.40).
 - Mathias Rauen (madshi) for improving the support for madExcept in the debug
   info support DLL.
 - Roddy Pratt for the BCB5 support code.
 - Rene Mihula for the Czech translation and the suggestion to have dynamic
   loading of the FullDebugMode DLL as an option.
 - Artur Redzko for the Polish translation.
 - Bart van der Werf for helping me solve the DLL unload order problem when
   using the debug mode borlndmm.dll library, as well as various other
   suggestions.
 - JRG ("The Delphi Guy") for the Spanish translation.
 - Justus Janssen for Delphi 4 support.
 - Vadim Lopushansky and Charles Vinal for reporting the Delphi 5 compiler
   error in version 4.50.
 - Johni Jeferson Capeletto for the Brazilian Portuguese translation.
 - Kurt Fitzner for reporting the BCB6 compiler error in 4.52.
 - Michal Niklas for reporting the Kylix compiler error in 4.54.
 - Thomas Speck and Uwe Queisser for German translations.
 - Zaenal Mutaqin for the Indonesian translation.
 - Carlos Macao for the Portuguese translation.
 - Michael Winter for catching the performance issue when reallocating certain
   block sizes.
 - dzmitry[li] for the Belarussian translation.
 - Marcelo Montenegro for the updated Spanish translation.
 - Jud Cole for finding and reporting the bug which may trigger a read access
   violation when upsizing certain small block sizes together with the
   "UseCustomVariableSizeMoveRoutines" option.
 - Zdenek Vasku for reporting and fixing the memory manager sharing bug
   affecting Windows 95/98/Me.
 - RB Winston for suggesting the improvement to GExperts "backup" support.
 - Thomas Schulz for reporting the bug affecting large address space support
   under FullDebugMode, as well as the recursive call bug when attempting to
   report memory leaks when EnableMemoryLeakReporting is disabled.
 - Luigi Sandon for the Italian translation.
 - Werner Bochtler for various suggestions and bug reports.
 - Markus Beth for suggesting the "NeverSleepOnThreadContention" option.
 - JiYuan Xie for the Simplified Chinese translation.
 - Andrey Shtukaturov for the updated Russian translation, as well as the
   Ukrainian translation.
 - Dimitry Timokhov for finding two elusive bugs in the memory leak class
   detection code.
 - Paulo Moreno for fixing the AllocMem bug in FullDebugMode that prevented
   large blocks from being cleared.
 - Vladimir Bochkarev for the suggestion to remove some unnecessary code if the
   MM sharing mechanism is disabled.
 - Loris Luise for the version constant suggestion.
 - J.W. de Bokx for the MessageBox bugfix.
 - Igor Lindunen for reporting the bug that caused the Align16Bytes option to
   not work in FullDebugMode.
 - Ionut Muntean for the Romanian translation.
 - Florent Ouchet for the French translation.
 - Marcus Moennig for the ScanMemoryPoolForCorruptions suggestion and the
   suggestion to have the option to scan the memory pool before every
   operation when in FullDebugMode.
 - Francois Piette for bringing under my attention that
   ScanMemoryPoolForCorruption was not thread safe.
 - Michael Rabatscher for reporting some compiler warnings.
 - QianYuan Wang for the Simplified Chinese translation of FastMM4Options.inc.
 - Maurizio Lotauro and Christian-W. Budde for reporting some Delphi 5
   compiler errors.
 - Patrick van Logchem for the DisableLoggingOfMemoryDumps option.
 - Norbert Spiegel for the BCB4 support code.
 - Uwe Schuster for the improved string leak detection code.
 - Murray McGowan for improvements to the usage tracker.
 - Michael Hieke for the SuppressFreeMemErrorsInsideException option as well
   as a bugfix to GetMemoryMap.
 - Richard Bradbrook for fixing the Windows 95 FullDebugMode support that was
   broken in version 4.94.
 - Zach Saw for the suggestion to (optionally) use SwitchToThread when
   waiting for a lock on a shared resource to be released.
 - Everyone who have made donations. Thanks!
 - Any other Fastcoders or supporters that I have forgotten, and also everyone
   that helped with the older versions.

Change log:
 Version 1.00 (28 June 2004):
  - First version (called PSDMemoryManager). Based on RecyclerMM (free block
    stack approach) by Eric Grange.
 Version 2.00 (3 November 2004):
  - Complete redesign and rewrite from scratch. Name changed to FastMM to
    reflect this fact. Uses a linked-list approach. Is faster, has less memory
    overhead, and will now catch most bad pointers on FreeMem calls.
 Version 3.00 (1 March 2005):
  - Another rewrite. Reduced the memory overhead by: (a) not having a separate
    memory area for the linked list of free blocks (uses space inside free
    blocks themselves) (b) batch managers are allocated as part of chunks (c)
    block size lookup table size reduced. This should make FastMM more CPU
    cache friendly.
 Version 4.00 (7 June 2005):
  - Yet another rewrite. FastMM4 is in fact three memory managers in one: Small
    blocks (up to a few KB) are managed through the binning model in the same
    way as previous versions, medium blocks (from a few KB up to approximately
    256K) are allocated in a linked-list fashion, and large blocks are grabbed
    directly from the system through VirtualAlloc. This 3-layered design allows
    very fast operation with the most frequently used block sizes (small
    blocks), while also minimizing fragmentation and imparting significant
    overhead savings with blocks larger than a few KB.
 Version 4.01 (8 June 2005):
  - Added the options "RequireDebugInfoForLeakReporting" and
    "RequireIDEPresenceForLeakReporting" as suggested by Pierre Y.
  - Fixed the "DelphiIsRunning" function not working under Delphi 5, and
    consequently, no leak checking. (Reported by Anders Isaksson and Greg.)
 Version 4.02 (8 June 2005):
  - Fixed the compilation error when both the "AssumeMultiThreaded" and
    "CheckHeapForCorruption options were set. (Reported by Francois Malan.)
 Version 4.03 (9 June 2005):
  - Added descriptive error messages when FastMM4 cannot be installed because
    another MM has already been installed or memory has already been allocated.
 Version 4.04 (13 June 2005):
  - Added a small fixed offset to the size of medium blocks (previously always
    exact multiples of 256 bytes). This makes performance problems due to CPU
    cache associativity limitations much less likely. (Reported by Craig
    Peterson.)
 Version 4.05 (17 June 2005):
  - Added the Align16Bytes option. Disable this option to drop the 16 byte
    alignment restriction and reduce alignment to 8 bytes for the smallest
    block sizes. Disabling Align16Bytes should lower memory consumption at the
    cost of complicating the use of aligned SSE move instructions. (Suggested
    by Craig Peterson.)
  - Added a support unit for C++ Builder 6 - Add FastMM4BCB.cpp and
    FastMM4.pas to your BCB project to use FastMM instead of the RTL MM. Memory
    leak checking is not supported because (unfortunately) once an MM is
    installed under BCB you cannot uninstall it... at least not without
    modifying the RTL code in exit.c or patching the RTL code runtime. (Thanks
    to Jarek Karciarz, Vladimir Ulchenko and Bob Gonder.)
 Version 4.06 (22 June 2005):
  - Displays the class of all leaked objects on the memory leak report and also
    tries to identify leaked long strings. Previously it only displayed the
    sizes of all leaked blocks. (Suggested by Ben Taylor.)
  - Added support for displaying the sizes of medium and large block memory
    leaks. Previously it only displayed details for small block leaks.
 Version 4.07 (22 June 2005):
  - Fixed the detection of the class of leaked objects not working under
    Windows 98/Me.
 Version 4.08 (27 June 2005):
  - Added a BorlndMM.dpr project to allow you to build a borlndmm.dll that uses
    FastMM4 instead of the default memory manager. You may replace the old
    DLL in the Delphi \Bin directory to make the IDE use this memory manager
    instead.
 Version 4.09 (30 June 2005):
  - Included a patch fix for the bug affecting replacement borlndmm.dll files
    with Delphi 2005 (QC#14007). Compile the patch, close Delphi, and run it
    once to patch your vclide90.bpl. You will now be able to use the
    replacement borlndmm.dll to speed up the Delphi 2005 IDE as well.
 Version 4.10 (7 July 2005):
  - Due to QC#14070 ("Delphi IDE attempts to free memory after the shutdown
    code of borlndmm.dll has been called"), FastMM cannot be uninstalled
    safely when used inside a replacement borlndmm.dll for the IDE. Added a
    conditional define "NeverUninstall" for this purpose.
  - Added the "FullDebugMode" option to pad all blocks with a header and footer
    to help you catch memory overwrite bugs in your applications. All blocks
    returned to freemem are also zeroed out to help catch bugs involving the
    use of previously freed blocks. Also catches attempts at calling virtual
    methods of freed objects provided the block in question has not been reused
    since the object was freed. Displays stack traces on error to aid debugging.
  - Added the "LogErrorsToFile" option to log all errors to a text file in the
    same folder as the application.
  - Added the "ManualLeakReportingControl" option (suggested by Nahan Hyn) to
    enable control over whether the memory leak report should be done or not
    via a global variable.
 Version 4.11 (7 July 2005):
  - Fixed a compilation error under Delphi 2005 due to QC#10922. (Thanks to Joe
    Bain and Leonel Togniolli.)
  - Fixed leaked object classes not displaying in the leak report in
    "FullDebugMode".
  Version 4.12 (8 July 2005):
  - Moved all the string constants to one place to make it easier to do
    translations into other languages. (Thanks to Robert Marquardt.)
  - Added support for Kylix. Some functionality is currently missing: No
    support for detecting the object class on leaks and also no MM sharing.
    (Thanks to Simon Kissel and Fikret Hasovic).
  Version 4.13 (11 July 2005):
  - Added the FastMM_DebugInfo.dll support library to display debug info for
    stack traces.
  - Stack traces for the memory leak report is now logged to the log file in
    "FullDebugMode".
  Version 4.14 (14 July 2005):
  - Fixed string leaks not being detected as such in "FullDebugMode". (Thanks
    to Leonel Togniolli.)
  - Fixed the compilation error in "FullDebugMode" when "LogErrorsToFile" is
    not set. (Thanks to Leonel Togniolli.)
  - Added a "Release" option to allow the grouping of various options and to
    make it easier to make debug and release builds. (Thanks to Alexander
    Tabakov.)
  - Added a "HideMemoryLeakHintMessage" option to not display the hint below
    the memory leak message. (Thanks to Alexander Tabakov.)
  - Changed the fill character for "FullDebugMode" from zero to $80 to be able
    to differentiate between invalid memory accesses using nil pointers to
    invalid memory accesses using fields of freed objects. FastMM tries to
    reserve the 64K block starting at $80800000 at startup to ensure that an
    A/V will occur when this block is accessed. (Thanks to Alexander Tabakov.)
  - Fixed some compiler warnings. (Thanks to M. Skloff)
  - Fixed some display bugs in the memory leak report. (Thanks to Leonel
    Togniolli.)
  - Added a "LogMemoryLeakDetailToFile" option. Some applications leak a lot of
    memory and can make the log file grow very large very quickly.
  - Added the option to use madExcept instead of the JCL Debug library in the
    debug info support DLL. (Thanks to Martin Aignesberger.)
  - Added procedures "GetMemoryManagerState" and "GetMemoryMap" to retrieve
    statistics about the current state of the memory manager and memory pool.
    (A usage tracker form together with a demo is also available.)
  Version 4.15 (14 July 2005):
  - Fixed a false 4GB(!) memory leak reported in some instances.
  Version 4.16 (15 July 2005):
  - Added the "CatchUseOfFreedInterfaces" option to catch the use of interfaces
    of freed objects. This option is not compatible with checking that a freed
    block has not been modified, so enable this option only when hunting an
    invalid interface reference. (Only relevant if "FullDebugMode" is set.)
  - During shutdown FastMM now checks that all free blocks have not been
    modified since being freed. (Only when "FullDebugMode" is set and
    "CatchUseOfFreedInterfaces" is disabled.)
  Version 4.17 (15 July 2005):
 - Added the AddExpectedMemoryLeaks and RemoveExpectedMemoryLeaks procedures to
   register/unregister expected leaks, thus preventing the leak report from
   displaying if only expected leaks occurred. (Thanks to Diederik and Dennis
   Passmore for the suggestion.) (Note: these functions were renamed in later
   versions.)
 - Fixed the "LogMemoryLeakDetailToFile" not logging memory leak detail to file
   as it is supposed to. (Thanks to Leonel Togniolli.)
 Version 4.18 (18 July 2005):
 - Fixed some issues when range checking or complete boolean evaluation is
   switched on. (Thanks to Dario Tiraboschi and Mark Gebauer.)
 - Added the "OutputInstallUninstallDebugString" option to display a message when
   FastMM is installed or uninstalled. (Thanks to Hanspeter Widmer.)
 - Moved the options to a separate include file. (Thanks to Hanspeter Widmer.)
 - Moved message strings to a separate file for easy translation.
 Version 4.19 (19 July 2005):
 - Fixed Kylix support that was broken in 4.14.
 Version 4.20 (20 July 2005):
 - Fixed a false memory overwrite report at shutdown in "FullDebugMode". If you
   consistently got a "Block Header Has Been Corrupted" error message during
   shutdown at address $xxxx0070 then it was probably a false alarm. (Thanks to
   Theo Carr-Brion and Hanspeter Widmer.}
 Version 4.21 (27 July 2005):
 - Minor change to the block header flags to make it possible to immediately
   tell whether a medium block is being used as a small block pool or not.
   (Simplifies the leak checking and status reporting code.)
 - Expanded the functionality around the management of expected memory leaks.
 - Added the "ClearLogFileOnStartup" option. Deletes the log file during
   initialization. (Thanks to M. Skloff.)
 - Changed "OutputInstallUninstallDebugString" to use OutputDebugString instead
   of MessageBox. (Thanks to Hanspeter Widmer.)
 Version 4.22 (1 August 2005):
 - Added a FastAllocMem function that avoids an unnecessary FillChar call with
   large blocks.
 - Changed large block resizing behavior to be a bit more conservative. Large
   blocks will be downsized if the new size is less than half of the old size
   (the threshold was a quarter previously).
 Version 4.23 (6 August 2005):
 - Fixed BCB6 support (Thanks to Omar Zelaya).
 - Renamed "OutputInstallUninstallDebugString" to "UseOutputDebugString", and
   added debug string output on memory leak or error detection.
 Version 4.24 (11 August 2005):
 - Added the "NoMessageBoxes" option to suppress the display of message boxes,
   which is useful for services that should not be interrupted. (Thanks to Dan
   Miser).
 - Changed the stack trace code to return the line number of the caller and not
   the line number of the return address. (Thanks to Dan Miser).
 Version 4.25 (15 August 2005):
 - Fixed GetMemoryLeakType not detecting expected leaks registered by class
   when in "FullDebugMode". (Thanks to Arjen de Ruijter).
 Version 4.26 (18 August 2005):
 - Added a "UseRuntimePackages" option that allows FastMM to be used in a main
   application together with DLLs that all use runtime packages. (Thanks to
   Aleksander Oven.)
 Version 4.27 (24 August 2005):
 - Fixed a bug that sometimes caused the leak report to be shown even though all
   leaks were registered as expected leaks. (Thanks to Kristofer Skaug.)
 Version 4.29 (30 September 2005):
 - Added the "RequireDebuggerPresenceForLeakReporting" option to only display
   the leak report if the application is run inside the IDE. (Thanks to Guenter
   Schoch.)
 - Added the "ForceMMX" option, which when disabled will check the CPU for
   MMX compatibility before using MMX. (Thanks to Jan Schlueter.)
 - Added the module name to the title of error dialogs to more easily identify
   which application caused the error. (Thanks to Kristofer Skaug.)
 - Added an ASCII dump to the "FullDebugMode" memory dumps. (Thanks to Hallvard
   Vassbotn.)
 - Added the option "HideExpectedLeaksRegisteredByPointer" to suppress the
   display and logging of expected memory leaks that were registered by pointer.
   (Thanks to Dan Miser.) Leaks registered by size or class are often ambiguous,
   so these expected leaks are always logged to file (in FullDebugMode) and are
   never hidden from the leak display (only displayed if there is at least one
   unexpected leak).
 - Added a procedure "GetRegisteredMemoryLeaks" to return a list of all
   registered memory leaks. (Thanks to Dan Miser.)
 - Added the "RawStackTraces" option to perform "raw" stack traces, negating
   the need for stack frames. This will usually result in more complete stack
   traces in FullDebugMode error reports, but it is significantly slower.
   (Thanks to Hallvard Vassbotn, Dan Miser and the JCL team.)
 Version 4.31 (2 October 2005):
 - Fixed the crash bug when both "RawStackTraces" and "FullDebugMode" were
   enabled. (Thanks to Dan Miser and Mark Edington.)
 Version 4.33 (6 October 2005):
 - Added a header corruption check to all memory blocks that are identified as
   leaks in FullDebugMode. This allows better differentiation between memory
   pool corruption bugs and actual memory leaks.
 - Fixed the stack overflow bug when using "RawStackTraces".
 Version 4.35 (6 October 2005):
 - Fixed a compilation error when the "NoMessageBoxes" option is set. (Thanks
   to Paul Ishenin.)
 - Before performing a "raw" stack trace, FastMM now checks whether exception
   handling is in place. If exception handling is not in place FastMM falls
   back to stack frame tracing. (Exception handling is required to handle the
   possible A/Vs when reading invalid call addresses. Exception handling is
   usually always available except when SysUtils hasn't been initialized yet or
   after SysUtils has been finalized.)
 Version 4.37 (8 October 2005):
 - Fixed the missing call stack trace entry issue when dynamically loading DLLs.
   (Thanks to Paul Ishenin.)
 Version 4.39 (12 October 2005):
 - Restored the performance with "RawStackTraces" enabled back to the level it
   was in 4.35.
 - Fixed the stack overflow error when using "RawStackTraces" that I thought I
   had fixed in 4.31, but unfortunately didn't. (Thanks to Craig Peterson.)
 Version 4.40 (13 October 2005):
 - Improved "RawStackTraces" to have less incorrect extra entries. (Thanks to
   Craig Peterson.)
 - Added the Russian (by Paul Ishenin) and Afrikaans translations of
   FastMM4Messages.pas.
 Version 4.42 (13 October 2005):
 - Fixed the compilation error when "CatchUseOfFreedInterfaces" is enabled.
   (Thanks to Cristian Nicola.)
 Version 4.44 (25 October 2005):
 - Implemented a FastGetHeapStatus function in analogy with GetHeapStatus.
   (Suggested by Cristian Nicola.)
 - Shifted more of the stack trace code over to the support dll to allow third
   party vendors to make available their own stack tracing and stack trace
   logging facilities.
 - Mathias Rauen (madshi) improved the support for madExcept in the debug info
   support DLL. Thanks!
 - Added support for BCB5. (Thanks to Roddy Pratt.)
 - Added the Czech translation by Rene Mihula.
 - Added the "DetectMMOperationsAfterUninstall" option. This will catch
   attempts to use the MM after FastMM has been uninstalled, and is useful for
   debugging.
 Version 4.46 (26 October 2005):
 - Renamed FastMM_DebugInfo.dll to FastMM_FullDebugMode.dll and made the
   dependency on this library a static one. This solves a DLL unload order
   problem when using FullDebugMode together with the replacement
   borlndmm.dll. (Thanks to Bart van der Werf.)
 - Added the Polish translation by Artur Redzko.
 Version 4.48 (10 November 2005):
 - Fixed class detection for objects leaked in dynamically loaded DLLs that
   were relocated.
 - Fabio Dell'Aria implemented support for EurekaLog in the FullDebugMode
   support DLL. Thanks!
 - Added the Spanish translation by JRG ("The Delphi Guy").
 Version 4.49 (10 November 2005):
 - Implemented support for installing replacement AllocMem and leak
   registration mechanisms for Delphi/BCB versions that support it.
 - Added support for Delphi 4. (Thanks to Justus Janssen.)
 Version 4.50 (5 December 2005):
 - Renamed the ReportMemoryLeaks global variable to ReportMemoryLeaksOnShutdown
   to be more consistent with the Delphi 2006 memory manager.
 - Improved the handling of large blocks. Large blocks can now consist of
   several consecutive segments allocated through VirtualAlloc. This
   significantly improves speed when frequently resizing large blocks, since
   these blocks can now often be upsized in-place.
 Version 4.52 (7 December 2005):
 - Fixed the compilation error with Delphi 5. (Thanks to Vadim Lopushansky and
   Charles Vinal for reporting the error.)
 Version 4.54 (15 December 2005):
 - Added the Brazilian Portuguese translation by Johni Jeferson Capeletto.
 - Fixed the compilation error with BCB6. (Thanks to Kurt Fitzner.)
 Version 4.56 (20 December 2005):
 - Fixed the Kylix compilation problem. (Thanks to Michal Niklas.)
 Version 4.58 (1 February 2006):
 - Added the German translations by Thomas Speck and Uwe Queisser.
 - Added the Indonesian translation by Zaenal Mutaqin.
 - Added the Portuguese translation by Carlos Macao.
 Version 4.60 (21 February 2006):
 - Fixed a performance issue due to an unnecessary block move operation when
   allocating a block in the range 1261-1372 bytes and then reallocating it in
   the range 1373-1429 bytes twice. (Thanks to Michael Winter.)
 - Added the Belarussian translation by dzmitry[li].
 - Added the updated Spanish translation by Marcelo Montenegro.
 - Added a new option "EnableSharingWithDefaultMM". This option allows FastMM
   to be shared with the default MM of Delphi 2006. It is on by default, but
   MM sharing has to be enabled otherwise it has no effect (refer to the
   documentation for the "ShareMM" and "AttemptToUseSharedMM" options).
 Version 4.62 (22 February 2006):
 - Fixed a possible read access violation in the MoveX16LP routine when the
   UseCustomVariableSizeMoveRoutines option is enabled. (Thanks to Jud Cole for
   some great detective work in finding this bug.)
 - Improved the downsizing behaviour of medium blocks to better correlate with
   the reallocation behaviour of small blocks. This change reduces the number
   of transitions between small and medium block types when reallocating blocks
   in the 0.7K to 2.6K range. It cuts down on the number of memory move
   operations and improves performance.
 Version 4.64 (31 March 2006):
 - Added the following functions for use with FullDebugMode (and added the
   exports to the replacement BorlndMM.dll): SetMMLogFileName,
   GetCurrentAllocationGroup, PushAllocationGroup, PopAllocationGroup and
   LogAllocatedBlocksToFile. The purpose of these functions is to allow you to
   identify and log related memory leaks while your application is still
   running.
 - Fixed a bug in the memory manager sharing mechanism affecting Windows
   95/98/ME. (Thanks to Zdenek Vasku.)
 Version 4.66 (9 May 2006):
 - Added a hint comment in this file so that FastMM4Messages.pas will also be
   backed up by GExperts. (Thanks to RB Winston.)
 - Fixed a bug affecting large address space (> 2GB) support under
   FullDebugMode. (Thanks to Thomas Schulz.)
 Version 4.68 (3 July 2006):
 - Added the Italian translation by Luigi Sandon.
 - If FastMM is used inside a DLL it will now use the name of the DLL as base
   for the log file name. (Previously it always used the name of the main
   application executable file.)
 - Fixed a rare A/V when both the FullDebugMode and RawStackTraces options were
   enabled. (Thanks to Primoz Gabrijelcic.)
 - Added the "NeverSleepOnThreadContention" option. This option may improve
   performance if the ratio of the the number of active threads to the number
   of CPU cores is low (typically < 2). This option is only useful for 4+ CPU
   systems, it almost always hurts performance on single and dual CPU systems.
   (Thanks to Werner Bochtler and Markus Beth.)
 Version 4.70 (4 August 2006):
  - Added the Simplified Chinese translation by JiYuan Xie.
  - Added the updated Russian as well as the Ukrainian translation by Andrey
    Shtukaturov.
  - Fixed two bugs in the leak class detection code that would sometimes fail
    to detect the class of leaked objects and strings, and report them as
    'unknown'. (Thanks to Dimitry Timokhov)
  Version 4.72 (24 September 2006):
  - Fixed a bug that caused AllocMem to not clear blocks > 256K in
    FullDebugMode. (Thanks to Paulo Moreno.)
  Version 4.74 (9 November 2006):
  - Fixed a bug in the segmented large block functionality that could lead to
    an application freeze when upsizing blocks greater than 256K in a
    multithreaded application (one of those "what the heck was I thinking?"
    type bugs).
  Version 4.76 (12 January 2007):
  - Changed the RawStackTraces code in the FullDebugMode DLL
    to prevent it from modifying the Windows "GetLastError" error code.
    (Thanks to Primoz Gabrijelcic.)
  - Fixed a threading issue when the "CheckHeapForCorruption" option was
    enabled, but the "FullDebugMode" option was disabled. (Thanks to Primoz
    Gabrijelcic.)
  - Removed some unnecessary startup code when the MM sharing mechanism is
    disabled. (Thanks to Vladimir Bochkarev.)
  - In FullDebugMode leaked blocks would sometimes be reported as belonging to
    the class "TFreedObject" if they were allocated but never used. Such blocks
    will now be reported as "unknown". (Thanks to Francois Malan.)
  - In recent versions the replacement borlndmm.dll created a log file (when
    enabled) that used the "borlndmm" prefix instead of the application name.
    It is now fixed to use the application name, however if FastMM is used
    inside other DLLs the name of those DLLs will be used. (Thanks to Bart van
    der Werf.)
  - Added a "FastMMVersion" constant. (Suggested by Loris Luise.)
  - Fixed an issue with error message boxes not displaying under certain
    configurations. (Thanks to J.W. de Bokx.)
  - FastMM will now display only one error message at a time. If many errors
    occur in quick succession, only the first error will be shown (but all will
    be logged). This avoids a stack overflow with badly misbehaved programs.
    (Thanks to Bart van der Werf.)
  - Added a LoadDebugDLLDynamically option to be used in conjunction with
    FullDebugMode. In this mode FastMM_FullDebugMode.dll is loaded dynamically.
    If the DLL cannot be found, stack traces will not be available. (Thanks to
    Rene Mihula.)
  Version 4.78 (1 March 2007):
  - The MB_DEFAULT_DESKTOP_ONLY constant that is used when displaying messages
    boxes since 4.76 is not defined under Kylix, and the source would thus not
    compile. That constant is now defined. (Thanks to Werner Bochtler.)
  - Moved the medium block locking code that was duplicated in several places
    to a subroutine to reduce code size. (Thanks to Hallvard Vassbotn.)
  - Fixed a bug in the leak registration code that sometimes caused registered
    leaks to be reported erroneously. (Thanks to Primoz Gabrijelcic.)
  - Added the NoDebugInfo option (on by default) that suppresses the generation
    of debug info for the FastMM4.pas unit. This will prevent the integrated
    debugger from stepping into the memory manager. (Thanks to Primoz
    Gabrijelcic.)
  - Increased the default stack trace depth in FullDebugMode from 9 to 10 to
    ensure that the Align16Bytes setting works in FullDebugMode. (Thanks to
    Igor Lindunen.)
  - Updated the Czech translation. (Thanks to Rene Mihula.)
  Version 4.84 (7 July 2008):
  - Added the Romanian translation. (Thanks to Ionut Muntean.)
  - Optimized the GetMemoryMap procedure to improve speed.
  - Added the GetMemoryManagerUsageSummary function that returns a summary of
    the GetMemoryManagerState call. (Thanks to Hallvard Vassbotn.)
  - Added the French translation. (Thanks to Florent Ouchet.)
  - Added the "AlwaysAllocateTopDown" FullDebugMode option to help with
    catching bad pointer arithmetic code in an address space > 2GB. This option
    is enabled by default.
  - Added the "InstallOnlyIfRunningInIDE" option. Enable this option to
    only install FastMM as the memory manager when the application is run
    inside the Delphi IDE. This is useful when you want to deploy the same EXE
    that you use for testing, but only want the debugging features active on
    development machines. When this option is enabled and the application is
    not being run inside the IDE, then the default Delphi memory manager will
    be used (which, since Delphi 2006, is FastMM without FullDebugMode.) This
    option is off by default.
  - Added the "FullDebugModeInIDE" option. This is a convenient shorthand for
    enabling FullDebugMode, InstallOnlyIfRunningInIDE and
    LoadDebugDLLDynamically. This causes FastMM to be used in FullDebugMode
    when the application is being debugged on development machines, and the
    default memory manager when the same executable is deployed. This allows
    the debugging and deployment of an application without having to compile
    separate executables. This option is off by default.
  - Added a ScanMemoryPoolForCorruptions procedure that checks the entire
    memory pool for corruptions and raises an exception if one is found. It can
    be called at any time, but is only available in FullDebugMode. (Thanks to
    Marcus Moennig.)
  - Added a global variable "FullDebugModeScanMemoryPoolBeforeEveryOperation".
    When this variable is set to true and FullDebugMode is enabled, then the
    entire memory pool is checked for consistency before every GetMem, FreeMem
    and ReallocMem operation. An "Out of Memory" error is raised if a
    corruption is found (and this variable is set to false to prevent recursive
    errors). This obviously incurs a massive performance hit, so enable it only
    when hunting for elusive memory corruption bugs. (Thanks to Marcus Moennig.)
  - Fixed a bug in AllocMem that caused the FPU stack to be shifted by one
    position.
  - Changed the default for option "EnableMMX" to false, since using MMX may
    cause unexpected behaviour in code that passes parameters on the FPU stack
    (like some "compiler magic" routines, e.g. VarFromReal).
  - Removed the "EnableSharingWithDefaultMM" option. This is now the default
    behaviour and cannot be disabled. (FastMM will always try to share memory
    managers between itself and the default memory manager when memory manager
    sharing is enabled.)
  - Introduced a new memory manager sharing mechanism based on memory mapped
    files. This solves compatibility issues with console and service
    applications. This sharing mechanism currently runs in parallel with the
    old mechanism, but the old mechanism can be disabled by undefining
    "EnableBackwardCompatibleMMSharing" in FastMM4Options.inc.
  - Fixed the recursive call error when the EnableMemoryLeakReporting option
    is disabled and an attempt is made to register a memory leak under Delphi
    2006 or later. (Thanks to Thomas Schulz.)
  - Added a global variable "SuppressMessageBoxes" to enable or disable
    message boxes at runtime. (Thanks to Craig Peterson.)
  - Added the leak reporting code for C++ Builder, as well as various other
    C++ Builder bits written by JiYuan Xie. (Thank you!)
  - Added the new Usage Tracker written by Hanspeter Widmer. (Thank you!)
  Version 4.86 (31 July 2008):
  - Tweaked the string detection algorithm somewhat to be less strict, and
    allow non-class leaks to be more often categorized as strings.
  - Fixed a compilation error under Delphi 5.
  - Made LogAllocatedBlocksToFile and ScanMemoryPoolForCorruptions thread
    safe. (Thanks to Francois Piette.)
  Version 4.88 (13 August 2008):
  - Fixed compiler warnings in NoOpRegisterExpectedMemoryLeak and
    NoOpUnRegisterExpectedMemoryLeak. (Thanks to Michael Rabatscher.)
  - Added the Simplified Chinese translation of FastMM4Options.inc by
    QianYuan Wang. (Thank you!)
  - Included the updated C++ Builder files with support for BCB6 without
    update 4 applied. (Submitted by JiYuan Xie. Thanks!)
  - Fixed a compilation error under Delphi 5.
  - Made LogAllocatedBlocksToFile and ScanMemoryPoolForCorruptions thread
    safe - for real this time. (Thanks to Francois Piette.)
  Version 4.90 (9 September 2008):
  - Added logging of the thread ID when capturing and displaying stack
    traces. (Suggested by Allen Bauer and Mark Edington.)
  - Fixed a Delphi 5 compiler error under FullDebugMode. (Thanks to Maurizio
    Lotauro and Christian-W. Budde.)
  - Changed a default setting in FastMM4Options.inc: RawStackTraces is now
    off by default due to the high number of support requests I receive with
    regards to the false positives it may cause. I recommend compiling debug
    builds of applications with the "Stack Frames" option enabled.
  - Fixed a compilation error under Kylix. (Thanks to Werner Bochtler.)
  - Official support for Delphi 2009.
  Version 4.92 (25 November 2008):
  - Added the DisableLoggingOfMemoryDumps option under FullDebugMode. When
    this option is set, memory dumps will not be logged for memory leaks or
    errors. (Thanks to Patrick van Logchem.)
  - Exposed the class and string type detection code in the interface section
    for use in application code (if required). (Requested by Patrick van
    Logchem.)
  - Fixed a bug in SetMMLogFileName that could cause the log file name to be
    set incorrectly.
  - Added BCB4 support. (Thanks to Norbert Spiegel.)
  - Included the updated Czech translation by Rene Mihula.
  - When FastMM raises an error due to a freed block being modified, it now
    logs detail about which bytes in the block were modified.
  Version 4.94 (28 August 2009):
  - Added the DoNotInstallIfDLLMissing option that prevents FastMM from
    installing itself if the FastMM_FullDebugMode.dll library is not
    available. (Only applicable when FullDebugMode and LoadDebugDLLDynamically
    are both enabled.) This is useful when the same executable will be used for
    both debugging and deployment - when the debug support DLL is available
    FastMM will be installed in FullDebugMode, and otherwise the default memory
    manager will be used.
  - Added the FullDebugModeWhenDLLAvailable option that combines the
    FullDebugMode, LoadDebugDLLDynamically and DoNotInstallIfDLLMissing options.
  - Re-enabled RawStackTraces by default. The frame based stack traces (even
    when compiling with stack frames enabled) are generally too incomplete.
  - Improved the speed of large block operations under FullDebugMode: Since
    large blocks are never reused, there is no point in clearing them before
    and after use (so it does not do that anymore).
  - If an error occurs in FullDebugMode and FastMM is unable to append to the
    log file, it will attempt to write to a log file of the same name in the
    "My Documents" folder. This feature is helpful when the executable resides
    in a read-only location and the default log file, which is derived from the
    executable name, would thus not be writeable.
  - Added support for controlling the error log file location through an
    environment variable. If the 'FastMMLogFilePath' environment variable is
    set then any generated error logs will be written to the specified folder
    instead of the default location (which is the same folder as the
    application).
  - Improved the call instruction detection code in the FastMM_FullDebugMode
    library. (Thanks to the JCL team.)
  - Improved the string leak detection and reporting code. (Thanks to Uwe
    Schuster.)
  - New FullDebugMode feature: Whenever FreeMem or ReallocMem is called, FastMM
    will check that the block was actually allocated through the same FastMM
    instance. This is useful for tracking down memory manager sharing issues.
  - Compatible with Delphi 2010.
  Version 4.96 (31 August 2010):
  - Reduced the minimum block size to 4 bytes from the previous value of 12
    bytes (only applicable to 8 byte alignment). This reduces memory usage if
    the application allocates many blocks <= 4 bytes in size.
  - Added colour-coded change indication to the FastMM usage tracker, making
    it easier to spot changes in the memory usage grid. (Thanks to Murray
    McGowan.)
  - Added the SuppressFreeMemErrorsInsideException FullDebugMode option: If
    FastMM encounters a problem with a memory block inside the FullDebugMode
    FreeMem handler then an "invalid pointer operation" exception will usually
    be raised. If the FreeMem occurs while another exception is being handled
    (perhaps in the try.. finally code) then the original exception will be
    lost. With this option set FastMM will ignore errors inside FreeMem when an
    exception is being handled, thus allowing the original exception to
    propagate. This option is on by default. (Thanks to Michael Hieke.)
  - Fixed Windows 95 FullDebugMode support that was broken in 4.94. (Thanks to
    Richard Bradbrook.)
  - Fixed a bug affecting GetMemoryMap performance and accuracy of measurements
    above 2GB if a large address space is not enabled for the project. (Thanks
    to Michael Hieke.)
  - Added the FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak boolean flag.
    When set, all allocations are automatically registered as expected memory
    leaks. Only available in FullDebugMode. (Thanks to Brian Cook.)
  - Compatible with Delphi XE.
  Version 4.97 (30 September 2010):
  - Fixed a crash bug (that crept in in 4.96) that may manifest itself when
    resizing a block to 4 bytes or less.
  - Added the UseSwitchToThread option. Set this option to call SwitchToThread
    instead of sitting in a "busy waiting" loop when a thread contention
    occurs. This is used in conjunction with the NeverSleepOnThreadContention
    option, and has no effect unless NeverSleepOnThreadContention is also
    defined. This option may improve performance with many CPU cores and/or
    threads of different priorities. Note that the SwitchToThread API call is
    only available on Windows 2000 and later. (Thanks to Zach Saw.)
  Version 4.98 (23 September 2011):
  - Added the FullDebugModeCallBacks define which adds support for memory
    manager event callbacks. This allows the application to be notified of
    memory allocations, frees and reallocations as they occur. (Thanks to
    Jeroen Pluimers.)
  - Added security options ClearMemoryBeforeReturningToOS and
    AlwaysClearFreedMemory to force the clearing of memory blocks after being
    freed. This could possibly provide some protection against information
    theft, but at a significant performance penalty. (Thanks to Andrey
    Sozonov.)
  - Shifted the code in the initialization section to a procedure
    RunInitializationCode. This allows the startup code to be called before
    InitUnits, which is required by some software protection tools.
  - Added support for Delphi XE2 (Windows 32-bit and Windows 64-bit platforms
    only).
  Version 4.99 (6 November 2011):
  - Fixed crashes in the 64-bit BASM codepath when more than 4GB of memory is
    allocated.
  - Fixed bad record alignment under 64-bit that affected performance.
  - Fixed compilation errors with some older compilers.
  Version 4.991 (3 September 2012)
  - Added the LogMemoryManagerStateToFile call. This call logs a summary of
    the memory manager state to file: The total allocated memory, overhead,
    efficiency, and a breakdown of allocated memory by class and string type.
    This call may be useful to catch objects that do not necessarily leak, but
    do linger longer than they should.
  - OS X support added by Sebastian Zierer
  - Compatible with Delphi XE3
  Version 4.992 (21 October 2016)
  - OS X full debug mode added by Sebastian Zierer
  - Included the average block size in the memory state log file. (Thanks to
    Hallvard Vassbotn)
  - Support added for Free Pascal's OS X and Linux targets, both i386 and
    x86-64. (Thanks to Zoe Peterson - some fixes by Arnaud Bouchez)
  - Added the LogLockContention option which may be used to track down areas
    in the application that lead to frequent lock contentions in the memory
    manager. (Primoz Gabrijelcic)
  - Support for release stacks added by Primoz Gabrijelcic. Define
    "UseReleaseStack" to use this new feature: If a block cannot be released
    immediately during a FreeMem call the block will added to a list of blocks
    that will be freed later, either in the background cleanup thread or during
    the next call to FreeMem.
  Version 4.993 (10 August 2021)
  - Added some "address space slack" under FullDebugMode. This reserves a
    block of address space on startup (currently 5MB) that is released just
    before the first time an EOutOfMemory exception is raised, allowing some
    GetMem calls following the initial EOutOfMemory to succeed. This allows
    the application to perform any error logging and other shutdown operations
    successfully that would have failed it the address space was actually
    completely exhausted. (Under FullDebugMode address space is never released
    back to the operating system so once the address space has been exhausted
    there is very little room to manoeuvre.)
  - Added the RestrictDebugDLLLoadPath option to only load the debug DLL from
    the host module directory.
  - Performance and other enhancements to the call stack generation. (Thanks to
    Andreas Hausladen.)
  - Added FastMM artwork. (Thanks to Jim McKeeth.)
  - Added the FastMM_GetInstallationState function:  Allows determination of
    whether FastMM is installed or not, and if not whether the default memory
    manager is in use or a different third party memory manager.
*)

unit FastMM4;

interface

{$Include FastMM4Options.inc}

{$IFNDEF FPC}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}


{Defines to turn off options enabled by default}

{$IFDEF DontDetectMMOperationsAfterUninstall}
  {$undef DetectMMOperationsAfterUninstall}
{$ENDIF}

{$IFDEF DiablePasCodeAlign}
  {$undef PasCodeAlign}
{$ENDIF}

{$IFDEF DisableAsmCodeAlign}
  {$undef EnableAsmCodeAlign}
  {$undef ForceAsmCodeAlign}
{$ENDIF}

{$IFDEF DontUseASMVersion}
  {$undef ASMVersion}
{$ENDIF}

{$IFDEF DontUseSmallBlocksLockedCriticalSection}
  {$undef UseSmallBlocksLockedCriticalSection}
{$ENDIF}

{$IFDEF DontUseMediumBlocksLockedCriticalSection}
  {$undef UseMediumBlocksLockedCriticalSection}
{$ENDIF}

{$IFDEF DontUseLargeBlocksLockedCriticalSection}
  {$undef UseLargeBlocksLockedCriticalSection}
{$ENDIF}

{$IFDEF DontUseSimplifiedInterlockedExchangeByte}
  {$undef UseSimplifiedInterlockedExchangeByte}
{$ENDIF}

{$IFDEF DontUseCustomFixedSizeMoveRoutines}
  {$undef UseCustomFixedSizeMoveRoutines}
{$ENDIF}

{$IFDEF DontUseCustomVariableSizeMoveRoutines}
  {$undef UseCustomVariableSizeMoveRoutines}
{$ENDIF}

{$IFDEF ForceSingleThreaded}
  {$undef AssumeMultiThreaded}
{$ENDIF}

{Compiler version defines}
{$IFNDEF fpc}
  {$IFNDEF BCB}
    {$IFDEF ver120}
      {$define Delphi4or5}
    {$ENDIF}
    {$IFDEF ver130}
      {$define Delphi4or5}
    {$ENDIF}
    {$IFDEF ver140}
      {$define Delphi6}
    {$ENDIF}
    {$IFDEF ver150}
      {$define Delphi7}
    {$ENDIF}
    {$IFDEF ver170}
      {$define Delphi2005}
    {$ENDIF}
  {$ELSE}
    {for BCB4, use the Delphi 5 codepath}
    {$IFDEF ver120}
      {$define Delphi4or5}
      {$define BCB4}
    {$ENDIF}
    {for BCB5, use the Delphi 5 codepath}
    {$IFDEF ver130}
      {$define Delphi4or5}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF ver180}
    {$define BDS2006}
  {$ENDIF}
  {$define 32Bit}
  {$IFNDEF Delphi4or5}
    {$if SizeOf(Pointer) = 8}
      {$define 64Bit}
      {$undef 32Bit}
    {$ifend}
    {$if CompilerVersion >= 23}
      {$define XE2AndUp}
    {$ifend}
    {$define BCB6OrDelphi6AndUp}
    {$IFNDEF BCB}
      {$define Delphi6AndUp}
    {$ENDIF}
    {$IFNDEF Delphi6}
      {$define BCB6OrDelphi7AndUp}
      {$IFNDEF BCB}
        {$define Delphi7AndUp}
      {$ENDIF}
      {$IFNDEF BCB}
        {$IFNDEF Delphi7}
          {$IFNDEF Delphi2005}
            {$define BDS2006AndUp}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {Defines for FreePascal}
  {$asmmode intel}
  {$IFDEF CPUX64}
    {$asmmode intel}
    {$define 64bit}
    {$define fpc64bit}
    {$undef 32bit}
  {$ELSE}
    {$define 32bit}
    {$undef 64bit}
  {$ENDIF}
{$ENDIF}

{$IFNDEF 64Bit}
  {do not support AVX unless we are in the 64-bit mode}
  {$undef EnableAVX}
{$ENDIF}

{ The assembly implementation of FastGetmem and FastFreemem will check whether
 "pause" and SwitchToThread() are available, otherwisw will jump to pascal versions
 of FastGetmem and FastFreemem. However, if we assume that "pause" and SwitchToThread()
 are available (AssumePauseAndSwitchToThreadAvailable), we would not do any check,
 i.e., undefine CheckPauseAndSwitchToThreadFor
 }

{$IFDEF ASMVersion}
{$define CheckPauseAndSwitchToThreadForAsmVersion}
{$ENDIF}

{$IFDEF DisablePauseAndSwitchToThread}
  {$undef ASMVersion}
  {$undef AssumePauseAndSwitchToThreadAvailable}
  {$undef CheckPauseAndSwitchToThreadForAsmVersion}
  {$undef FastGetMemNeedAssemblerCode}
  {$define FastGetMemNeedPascalCode}
{$ELSE}
  {$IFDEF 64bit}
    {$define AssumePauseAndSwitchToThreadAvailable}
  {$ENDIF}
  {$IFDEF AssumePauseAndSwitchToThreadAvailable}
    {$undef CheckPauseAndSwitchToThreadForAsmVersion}
  {$ENDIF}
{$ENDIF}

{$IFDEF 64bit}

  {$IFDEF EnableAVX}
    {Under 64 bit with AVX, memory blocks must always be 32-byte aligned,
    since we are using 32-bit load/store, and they have to be aligned,
    a store across page boundary invokes 150-cycle penalty on Sandy Bridge}
    {$define Align32Bytes}
  {$ENDIF}

  {No need for MMX under 64-bit, since SSE2 is available}
  {$undef EnableMMX}
  {There is little need for raw stack traces under 64-bit, since frame based
   stack traces are much more accurate than under 32-bit. (And frame based
   stack tracing is much faster.)}
  {$undef RawStackTraces}
{$ENDIF}

{Lock contention logging requires ~ASMVersion.}
{$IFDEF LogLockContention}
  {$undef ASMVersion}
  {$IFNDEF FullDebugMode}
  {$message error 'LogLockContention requires FullDebugMode'}
  {$ENDIF}
  {$IFDEF UseReleaseStack}
    {$Message error 'LogLockContention requires FullDebugMode but UseReleaseStack is incompatible with FullDebugMode'}
  {$ENDIF}  
{$ENDIF}

{Release stack requires ~ASMVersion (for now).}
{$IFDEF UseReleaseStack}
  {$undef ASMVersion}
  {$IFDEF FullDebugMode}
  {$message error 'UseReleaseStack is not compatible with FullDebugMode'}
  {$ENDIF}
{$ENDIF}

{IDE debug mode always enables FullDebugMode and dynamic loading of the FullDebugMode DLL.}
{$IFDEF FullDebugModeInIDE}
  {$define InstallOnlyIfRunningInIDE}
  {$define FullDebugMode}
  {$define LoadDebugDLLDynamically}
{$ENDIF}

{Install in FullDebugMode only when the DLL is available?}
{$IFDEF FullDebugModeWhenDLLAvailable}
  {$define FullDebugMode}
  {$define LoadDebugDLLDynamically}
  {$define DoNotInstallIfDLLMissing}
{$ENDIF}

{$IFDEF Linux}
  {$define POSIX}
  {$IFDEF 64Bit}
    {$define PIC}  // Linux 64bit ASM is PIC
  {$ENDIF}
  {$IFNDEF FPC}
    {$define KYLIX}
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
  {$define POSIX}
  {$define PIC}
{$ENDIF}

{Some features not currently supported under Kylix / OS X}
{$IFDEF POSIX}
  {$IFNDEF MACOS}
    {$undef FullDebugMode}
    {$undef LogErrorsToFile}
    {$undef LogMemoryLeakDetailToFile}
  {$ENDIF}
  {$undef ShareMM}
  {$undef AttemptToUseSharedMM}
  {$undef RequireIDEPresenceForLeakReporting}
  {$undef UseOutputDebugString}
  {$IFDEF PIC}
    {BASM version does not support position independent code}
    {$undef ASMVersion}
  {$ENDIF}
  {$IFNDEF FPC}
    {$define MACOS_OR_KYLIX}
  {$ENDIF}
{$ENDIF}

{Do we require debug info for leak checking?}
{$IFDEF RequireDebugInfoForLeakReporting}
  {$ifopt D-}
    {$undef EnableMemoryLeakReporting}
  {$ENDIF}
{$ENDIF}

{Enable heap checking and leak reporting in full debug mode}
{$IFDEF FullDebugMode}
  {$define CheckHeapForCorruption}
  {$IFNDEF CatchUseOfFreedInterfaces}
    {$define CheckUseOfFreedBlocksOnShutdown}
  {$ENDIF}
{$ELSE}
  {Error logging requires FullDebugMode}
  {$undef LogErrorsToFile}
  {$undef CatchUseOfFreedInterfaces}
  {$undef RawStackTraces}
  {$undef AlwaysAllocateTopDown}
{$ENDIF}

{Set defines for security options}
{$IFDEF FullDebugMode}
  {In FullDebugMode small and medium blocks are always cleared when calling
   FreeMem. Large blocks are always returned to the OS immediately.}
  {$IFDEF ClearMemoryBeforeReturningToOS}
    {$define ClearLargeBlocksBeforeReturningToOS}
  {$ENDIF}
  {$IFDEF AlwaysClearFreedMemory}
    {$define ClearLargeBlocksBeforeReturningToOS}
  {$ENDIF}
{$ELSE}
  {If memory blocks are cleared in FreeMem then they do not need to be cleared
   before returning the memory to the OS.}
  {$IFDEF AlwaysClearFreedMemory}
    {$define ClearSmallAndMediumBlocksInFreeMem}
    {$define ClearLargeBlocksBeforeReturningToOS}
  {$ELSE}
    {$IFDEF ClearMemoryBeforeReturningToOS}
      {$define ClearMediumBlockPoolsBeforeReturningToOS}
      {$define ClearLargeBlocksBeforeReturningToOS}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{Only the Pascal version supports extended heap corruption checking.}
{$IFDEF CheckHeapForCorruption}
  {$undef ASMVersion}
{$ENDIF}

{For BASM bits that are not implemented in 64-bit.}
{$IFDEF 32Bit}
  {$IFDEF ASMVersion}
    {$define Use32BitAsm}
  {$ENDIF}
{$ENDIF}

{$IFDEF UseRuntimePackages}
  {$define AssumeMultiThreaded}
{$ENDIF}

{$IFDEF BCB6OrDelphi6AndUp}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{Leak detail logging requires error logging}
{$IFNDEF LogErrorsToFile}
  {$undef LogMemoryLeakDetailToFile}
  {$undef ClearLogFileOnStartup}
{$ENDIF}

{$IFNDEF EnableMemoryLeakReporting}
  {Manual leak reporting control requires leak reporting to be enabled}
  {$undef ManualLeakReportingControl}
{$ENDIF}

{$IFNDEF EnableMMX}
  {$undef ForceMMX}
{$ENDIF}

{Are any of the MM sharing options enabled?}
{$IFDEF ShareMM}
  {$define MMSharingEnabled}
{$ENDIF}
{$IFDEF AttemptToUseSharedMM}
  {$define MMSharingEnabled}
{$ENDIF}

{Instruct GExperts to back up the messages file as well.}
{#BACKUP FastMM4Messages.pas}

{Should debug info be disabled?}
{$IFDEF NoDebugInfo}
  {$DEBUGINFO OFF}
{$ENDIF}

{$IFDEF BCB}
  {$IFDEF borlndmmdll}
    {$OBJEXPORTALL OFF}
  {$ENDIF}
  {$IFNDEF PatchBCBTerminate}
    {Cannot uninstall safely under BCB}
    {$define NeverUninstall}
    {Disable memory leak reporting}
    {$undef EnableMemoryLeakReporting}
  {$ENDIF}
{$ENDIF}

{Stack tracer is needed for LogLockContention and for FullDebugMode.}
{$undef _StackTracer}
{$undef _EventLog}
{$IFDEF FullDebugMode}{$define _StackTracer}{$define _EventLog}{$ENDIF}
{$IFDEF LogLockContention}{$define _StackTracer}{$define _EventLog}{$ENDIF}
{$IFDEF UseReleaseStack}{$IFDEF DebugReleaseStack}{$define _EventLog}{$ENDIF}{$ENDIF}


{$IFNDEF fpc64bit}
  {$IFNDEF unix}
    {$define AllowAsmNoframe}
  {$ENDIF}
{$ENDIF}

{$IFDEF AllowAsmNoframe}
  {$define AllowAsmParams}
{$ENDIF}


{$IFNDEF POSIX}
  {$IFNDEF FPC}
     {$define VmtSupported}
  {$ENDIF}
{$ENDIF}

{$IFNDEF BCB6OrDelphi7AndUp}
  {$IFNDEF FPC}
    {$define SystemRunError}
  {$ENDIF}
{$ENDIF}


{$IFDEF XE2AndUp}
{$define FASTMM4_ALLOW_INLINES}
{$ENDIF}

{$IFDEF FPC}
{$define FASTMM4_ALLOW_INLINES}
{$ENDIF}

{$IFDEF DisableAVX512}
{$undef EnableAVX512}
{$ELSE}
{$define EnableAVX512}
{$ENDIF}

{$IFDEF 32bit}
  {$IFDEF FPC}
    {$define 32bit_SSE}
  {$ENDIF}
  {$IFDEF XE2AndUp}
    {$define 32bit_SSE}
  {$ENDIF}
{$ENDIF}

{------------------------Compiler options for FastMM4------------------------}


{This is the list of vital compiler options for FastMM4,
don't change them, otherwise FastMM4 would not work. FastMM4 does not support
other values of the options below than set here. The list currently consists
of just one option: "Boolean short-circuit evaluation".}


     {"BOOLEVAL OFF" means that the compiler generates code for short-circuit
     Boolean expression evaluation, which means that evaluation stops as soon
     as the result of the entire expression becomes evident in left to right
     order of evaluation.}

  {$BOOLEVAL OFF}

{$IFDEF FullDebugMode}

        {The stack framce force copmiler option should be ON for
        the FullDebugMode, otherwise the stack unmangling may not work
        properly for the call stack debug reports geneated
        by FastMM4.}

  {$STACKFRAMES ON}

{$ENDIF}

{$IFDEF PasCodeAlign}
  {$IFDEF FPC}
    {$CODEALIGN PROC=32}
    {$CODEALIGN JUMP=16}
    {$CODEALIGN LOOP=8}
    {$IFDEF Align32Bytes}
      {$ALIGN 32}
      {$PACKRECORDS 32}
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ALIGN 16}
        {$PACKRECORDS 16}
      {$ELSE}
        {$ALIGN 8}
        {$PACKRECORDS 8}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$IFDEF XE2AndUp}
      {$CODEALIGN 16}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF AsmVersion}
  {$undef CheckPauseAndSwitchToThreadForAsmVersion}
{$ENDIF}

{$IFNDEF DisablePauseAndSwitchToThread}
  {$IFDEF ASMVersion}
    {$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
      {$define FastGetMemNeedPascalCode}
      {$define FastGetMemNeedAssemblerCode}
    {$ELSE}
      {$define FastGetMemNeedAssemblerCode}
    {$ENDIF}
  {$ELSE}
     {$define FastGetMemNeedPascalCode}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FastGetMemNeedAssemblerCode}
{$undef CheckPauseAndSwitchToThreadForAsmVersion}
{$ENDIF}

{$IFDEF fpc}
{$IFDEF 64bit}
{$undef FastGetMemNeedAssemblerCode}
{$define FastGetMemNeedPascalCode}
{$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF 64bit}
    {$undef ASMVersion}
    {Assembler is not yet supportd under 64-bit FreePascal,
    because it incorrectly encodes relative values wither with +RIP or without}
    {$define AuxAsmRoutines}
  {$ENDIF}
{$ENDIF}

{$IFNDEF PurePascal}
  {$define USE_CPUID}
{$ENDIF}

{$IFNDEF DisablePauseAndSwitchToThread}
{$IFNDEF AssumePauseAndSwitchToThreadAvailable}
{$IFDEF USE_CPUID}
{$define AuxAsmRoutines}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF ASMVersion}
  {$define FastFreememNeedAssemberCode}
  {$define FastReallocMemNeedAssemberCode}
{$ENDIF}

{$IFNDEF PurePascal}
{$define AuxAsmRoutines}
{$ENDIF}

{$IFDEF PurePascal}
{$undef USE_CPUID}
{$undef EnableMMX}
{$undef ForceMMX}
{$undef EnableERMS}
{$undef EnableAVX}
{$undef EnableAVX512}
{$undef UseCustomFixedSizeMoveRoutines}
{$undef UseCustomVariableSizeMoveRoutines}
{$define DisableAVX}
{$define DisableAVX1}
{$define DisableAVX2}
{$define DisableAVX512}
{$define Use_GetEnabledXStateFeatures_WindowsAPICall}
{$ENDIF}

{$IFNDEF 64bit}
{$undef EnableAVX512} // AVX-512 is only implemented in 64-bit
{$ENDIF}


{$IFDEF unix}
{$undef EnableAVX512} // AVX-512 is not yet implemented for UNIX
{$ENDIF unix}


{$IFDEF 32bit}
{$define AuxAsmRoutines}
{$ENDIF}

{$IFDEF 64bit}
{$define AuxAsmRoutines}
{$ENDIF}

{$IFDEF EnableAsmCodeAlign}
  {$IFDEF FPC}
    {$define ForceAsmCodeAlign}
  {$ENDIF}
{$ENDIF}

{$IFDEF ForceAsmCodeAlign}
  {$define AsmCodeAlign}
  {$IFDEF FPC}
     {$define AsmAlNodot}
  {$ENDIF}
{$ENDIF}


{$IFDEF Align16Bytes}
{$define AlignAtLeast16Bytes}
{$ENDIF}

{$IFDEF Align32Bytes}
{$define AlignAtLeast16Bytes}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF PurePascal}
    {$define SynchroVarLongint}
  {$ENDIF}
{$ENDIF}

{$IFDEF PurePascal}
  {$undef AsmVersion}
  {$undef AuxAsmRoutines}
  {$define DisablePauseAndSwitchToThread}
{$ENDIF}

{$IFDEF XE2AndUp}
  {$define OperatorsInDefinesSupported}
{$ENDIF}

{$IFDEF FPC}
  {$define OperatorsInDefinesSupported}
{$ENDIF}


{-------------------------Public constants-----------------------------}
const
  {The current version of FastMM4-AVX}
  FastMM4AvxVersion = '1.0.7';
  {The current version of FastMM}
  FastMMVersion = '4.993';

  {A bit mask to check memory block alignment in DEBUG mode}
{$IFDEF DEBUG}
{$IFDEF Align32Bytes}
  AlignmentMask = 31;
{$ELSE}
{$IFDEF Align16Bytes}
  AlignmentMask = 15;
{$ELSE}
  AlignmentMask = 7;
{$ENDIF}
{$ENDIF Align32Bytes}
{$ENDIF DEBUG}

  {The number of small block types}
{$IFDEF Align32Bytes}
  NumSmallBlockTypes = 44;
{$ELSE}
{$IFDEF Align16Bytes}
  NumSmallBlockTypes = 46;
{$ELSE}
  NumSmallBlockTypes = 56;
{$ENDIF}
{$ENDIF}


{----------------------------Public types------------------------------}
type

  {Make sure all the required types are available}
{$IFDEF BCB6OrDelphi6AndUp}
  {$if CompilerVersion < 20}
  PByte = PAnsiChar; {$define PByteIsPAnsiChar}
  {NativeInt didn't exist or was broken before Delphi 2009.}
  NativeInt = Integer;
  {$ifend}
  {$if CompilerVersion < 21}
  {NativeUInt didn't exist or was broken before Delphi 2010.}
  NativeUInt = Cardinal;
  {$ifend}
  {$if CompilerVersion < 22}
  {PNativeUInt didn't exist before Delphi XE.}
  PNativeUInt = ^Cardinal;
  {$ifend}
  {$if CompilerVersion < 23}
  {IntPtr and UIntPtr didn't exist before Delphi XE2.}
  IntPtr = Integer;
  UIntPtr = Cardinal;
  {$ifend}
{$ELSE}
  {$IFNDEF fpc}
  PByte = PAnsiChar; {$define PByteIsPAnsiChar}
  NativeInt = Integer;
  NativeUInt = Cardinal;
  PNativeUInt = ^Cardinal;
  IntPtr = Integer;
  UIntPtr = Cardinal;
  {$ELSE}
  NativeUInt = PtrUInt;
  PNativeUInt = ^PtrUInt;
  {$ENDIF}
{$ENDIF}

  TSmallBlockTypeState = record
    {The internal size of the block type}
    InternalBlockSize: Cardinal;
    {Useable block size: The number of non-reserved bytes inside the block.}
    UseableBlockSize: Cardinal;
    {The number of allocated blocks}
    AllocatedBlockCount: NativeUInt;
    {The total address space reserved for this block type (both allocated and
     free blocks)}
    ReservedAddressSpace: NativeUInt;
  end;
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;

  TMemoryManagerState = record
    {Small block type states}
    SmallBlockTypeStates: TSmallBlockTypeStates;
    {Medium block stats}
    AllocatedMediumBlockCount: Cardinal;
    TotalAllocatedMediumBlockSize: NativeUInt;
    ReservedMediumBlockAddressSpace: NativeUInt;
    {Large block stats}
    AllocatedLargeBlockCount: Cardinal;
    TotalAllocatedLargeBlockSize: NativeUInt;
    ReservedLargeBlockAddressSpace: NativeUInt;
  end;

  TMemoryManagerUsageSummary = record
    {The total number of bytes allocated by the application.}
    AllocatedBytes: NativeUInt;
    {The total number of address space bytes used by control structures, or
     lost due to fragmentation and other overhead.}
    OverheadBytes: NativeUInt;
    {The efficiency of the memory manager expressed as a percentage. This is
     100 * AllocatedBytes / (AllocatedBytes + OverheadBytes).}
    EfficiencyPercentage: Double;
  end;

  {Memory map}
  TChunkStatus = (csUnallocated, csAllocated, csReserved, csSysAllocated,
    csSysReserved);
  TMemoryMap = array[0..65535] of TChunkStatus;

{$IFDEF EnableMemoryLeakReporting}
  {List of registered leaks}
  TRegisteredMemoryLeak = record
    LeakAddress: Pointer;
    LeakedClass: TClass;
    {$IFDEF CheckCppObjectTypeEnabled}
    LeakedCppTypeIdPtr: Pointer;
    {$ENDIF}
    LeakSize: NativeInt;
    LeakCount: Integer;
  end;
  TRegisteredMemoryLeaks = array of TRegisteredMemoryLeak;
{$ENDIF}

  {Used by the DetectStringData routine to detect whether a leaked block
   contains string data.}
  TStringDataType = (stUnknown, stAnsiString, stUnicodeString);

  {The callback procedure for WalkAllocatedBlocks.}
  TWalkAllocatedBlocksCallback = procedure(APBlock: Pointer; ABlockSize: NativeInt; AUserData: Pointer);

  TFastMM_MemoryManagerInstallationState = (
    {The default memory manager is currently in use.}
    mmisDefaultMemoryManagerInUse,
    {Another third party memory manager has been installed.}
    mmisOtherThirdPartyMemoryManagerInstalled,
    {A shared memory manager is being used.}
    mmisUsingSharedMemoryManager,
    {This memory manager has been installed.}
    mmisInstalled);

{--------------------------Public variables----------------------------}
var
  {If this variable is set to true and FullDebugMode is enabled, then the
   entire memory pool is checked for consistency before every memory
   operation. Note that this incurs a massive performance hit on top of
   the already significant FullDebugMode overhead, so enable this option
   only when absolutely necessary.}
  FullDebugModeScanMemoryPoolBeforeEveryOperation: Boolean;
  FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak: Boolean;
{$IFDEF ManualLeakReportingControl}
  {Variable is declared in system.pas in newer Delphi versions.}
  {$IFNDEF BDS2006AndUp}
  ReportMemoryLeaksOnShutdown: Boolean;
  {$ENDIF}
{$ENDIF}
  {If set to True, disables the display of all messageboxes}
  SuppressMessageBoxes: Boolean;

{-------------------------Public procedures----------------------------}
{Executes the code normally run in the initialization section. Running it
 earlier may be required with e.g. some software protection tools.}
procedure RunInitializationCode;
{Installation procedures must be exposed for the BCB helper unit FastMM4BCB.cpp}
{$IFDEF BCB}
procedure InitializeMemoryManager;
function CheckCanInstallMemoryManager: Boolean;
procedure InstallMemoryManager;

{$IFDEF FullDebugMode}
(*$HPPEMIT '#define FullDebugMode' *)

{$IFDEF ClearLogFileOnStartup}
(*$HPPEMIT '  #define ClearLogFileOnStartup' *)
procedure DeleteEventLog;
{$ENDIF}

{$IFDEF LoadDebugDLLDynamically}
(*$HPPEMIT '  #define LoadDebugDLLDynamically' *)
{$ENDIF}

{$IFDEF RawStackTraces}
(*$HPPEMIT '  #define RawStackTraces' *)
{$ENDIF}

{$ENDIF}

{$IFDEF PatchBCBTerminate}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define PatchBCBTerminate' *)

{$IFDEF EnableMemoryLeakReporting}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define EnableMemoryLeakReporting' *)
{$ENDIF}

{$IFDEF DetectMMOperationsAfterUninstall}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define DetectMMOperationsAfterUninstall' *)
{$ENDIF}

{Called in FastMM4BCB.cpp, should contain codes of original "finalization" section}
procedure FinalizeMemoryManager;

{For completion of "RequireDebuggerPresenceForLeakReporting" checking in "FinalizeMemoryManager"}
var
  pCppDebugHook: ^Integer = nil; //PInteger not defined in BCB5

{$IFDEF CheckCppObjectTypeEnabled}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define CheckCppObjectTypeEnabled' *)

type
  TGetCppVirtObjSizeByTypeIdPtrFunc = function(APointer: Pointer): Cardinal;
  TGetCppVirtObjTypeIdPtrFunc = function(APointer: Pointer; ASize: Cardinal): Pointer;
  TGetCppVirtObjTypeNameFunc = function(APointer: Pointer; ASize: Cardinal): PAnsiChar;
  TGetCppVirtObjTypeNameByTypeIdPtrFunc = function (APointer: Pointer): PAnsiChar;
  TGetCppVirtObjTypeNameByVTablePtrFunc = function(AVTablePtr: Pointer; AVTablePtrOffset: Cardinal): PAnsiChar;
var
  {Return virtual object's size from typeId pointer}
  GetCppVirtObjSizeByTypeIdPtrFunc: TGetCppVirtObjSizeByTypeIdPtrFunc = nil;
  {Retrieve virtual object's typeId pointer}
  GetCppVirtObjTypeIdPtrFunc: TGetCppVirtObjTypeIdPtrFunc = nil;
  {Retrieve virtual object's type name}
  GetCppVirtObjTypeNameFunc: TGetCppVirtObjTypeNameFunc = nil;
  {Return virtual object's type name from typeId pointer}
  GetCppVirtObjTypeNameByTypeIdPtrFunc: TGetCppVirtObjTypeNameByTypeIdPtrFunc = nil;
  {Retrieve virtual object's typeId pointer from it's virtual table pointer}
  GetCppVirtObjTypeNameByVTablePtrFunc: TGetCppVirtObjTypeNameByVTablePtrFunc = nil;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF FullDebugMode}
{The standard memory manager functions}
function FastGetMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
function FastFreeMem(APointer: Pointer): {$IFDEF fpc}{$IFDEF CPU64}PtrUInt{$ELSE}NativeUInt{$ENDIF}{$ELSE}Integer{$ENDIF};
function FastReallocMem({$IFDEF fpc}var {$ENDIF}APointer: Pointer; ANewSize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
function FastAllocMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Cardinal{$ENDIF}{$ENDIF}): Pointer;
{$ELSE}
{The FullDebugMode memory manager functions}
function DebugGetMem(ASize: {$IFDEF FPC}ptruint{$ELSE}{$IFDEF XE2AndUp}NativeInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
function DebugFreeMem(APointer: Pointer): {$IFDEF fpc}ptruint{$ELSE}Integer{$ENDIF};
function DebugReallocMem({$IFDEF FPC}var {$ENDIF}APointer: Pointer; ANewSize: {$IFDEF FPC}ptruint{$ELSE}{$IFDEF XE2AndUp}NativeInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
function DebugAllocMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}Cardinal{$ENDIF}): Pointer;
{Scans the memory pool for any corruptions. If a corruption is encountered an "Out of Memory" exception is
 raised.}
procedure ScanMemoryPoolForCorruptions;
{Returns the current "allocation group". Whenever a GetMem request is serviced
 in FullDebugMode, the current "allocation group" is stored in the block header.
 This may help with debugging. Note that if a block is subsequently reallocated
 that it keeps its original "allocation group" and "allocation number" (all
 allocations are also numbered sequentially).}
function GetCurrentAllocationGroup: Cardinal;
{Allocation groups work in a stack like fashion. Group numbers are pushed onto
 and popped off the stack. Note that the stack size is limited, so every push
 should have a matching pop.}
procedure PushAllocationGroup(ANewCurrentAllocationGroup: Cardinal);
procedure PopAllocationGroup;
{Logs detail about currently allocated memory blocks for the specified range of
 allocation groups. if ALastAllocationGroupToLog is less than
 AFirstAllocationGroupToLog or it is zero, then all allocation groups are
 logged. This routine also checks the memory pool for consistency at the same
 time, raising an "Out of Memory" error if the check fails.}
procedure LogAllocatedBlocksToFile(AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
{$ENDIF}
{$IFDEF _EventLog}
{Specify the full path and name for the filename to be used for logging memory
 errors, etc. If ALogFileName is nil or points to an empty string it will
 revert to the default log file name.}
procedure SetMMLogFileName(ALogFileName: PAnsiChar = nil);
{$ENDIF}

{Releases all allocated memory (use with extreme care)}
procedure FreeAllMemory;

{Returns summarised information about the state of the memory manager. (For
 backward compatibility.)}
function FastGetHeapStatus: THeapStatus;
{Returns statistics about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);
{Returns a summary of the information returned by GetMemoryManagerState}
function GetMemoryManagerUsageSummary: TMemoryManagerUsageSummary; overload;
procedure GetMemoryManagerUsageSummary(var AMemoryManagerUsageSummary: TMemoryManagerUsageSummary); overload;
{$IFNDEF POSIX}
{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);
{$ENDIF}
{Returns the current installation state of the memory manager.}
function FastMM_GetInstallationState: TFastMM_MemoryManagerInstallationState;

{$IFDEF EnableMemoryLeakReporting}
{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
function RegisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
function RegisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
{$IFDEF CheckCppObjectTypeEnabled}
{Registers expected memory leaks by virtual object's typeId pointer.
 Usage: RegisterExpectedMemoryLeak(typeid(ACppObject).tpp, Count);}
function RegisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): boolean; overload;
{$ENDIF}
{Removes expected memory leaks. Returns true on success.}
function UnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
function UnregisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
function UnregisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
{$IFDEF CheckCppObjectTypeEnabled}
{Usage: UnregisterExpectedMemoryLeak(typeid(ACppObject).tpp, Count);}
function UnregisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): boolean; overload;
{$ENDIF}
{Returns a list of all expected memory leaks}
function GetRegisteredMemoryLeaks: TRegisteredMemoryLeaks;
{$ENDIF}

{Returns the class for a memory block. Returns nil if it is not a valid class.
 Used by the leak detection code.}
function DetectClassInstance(APointer: Pointer): TClass;
{Detects the probable string data type for a memory block. Used by the leak
 classification code when a block cannot be identified as a known class
 instance.}
function DetectStringData(APMemoryBlock: Pointer;
  AAvailableSpaceInBlock: NativeInt): TStringDataType;
{Walks all allocated blocks, calling ACallBack for each. Passes the user block size and AUserData to the callback.
 Important note: All block types will be locked during the callback, so the memory manager cannot be used inside it.}
procedure WalkAllocatedBlocks(ACallBack: TWalkAllocatedBlocksCallback; AUserData: Pointer);
{Writes a log file containing a summary of the memory manager state and a summary of allocated blocks grouped by
 class. The file will be saved in UTF-8 encoding (in supported Delphi versions). Returns True on success. }
function LogMemoryManagerStateToFile(const AFileName: string; const AAdditionalDetails: string {$IFNDEF FPC}= ''{$ENDIF}): Boolean;

{$IFDEF UseReleaseStack}
{$IFDEF DebugReleaseStack}
procedure LogReleaseStackUsage;
{$ENDIF}
{$ENDIF}

{$IFDEF _StackTracer}
{------------- FullDebugMode/LogLockContention constants---------------}
const
  {The stack trace depth. (Must be an *uneven* number to ensure that the
   Align16Bytes option works in FullDebugMode.)}
  StackTraceDepth = 11;

type
  PStackTrace = ^TStackTrace;
  TStackTrace = array[0..StackTraceDepth - 1] of NativeUInt;
{$ENDIF}

{$IFDEF FullDebugMode}
{-------------FullDebugMode constants---------------}
const
  {The number of entries in the allocation group stack}
  AllocationGroupStackSize = 1000;
  {The number of fake VMT entries - used to track virtual method calls on
   freed objects. Do not change this value without also updating TFreedObject.GetVirtualMethodIndex}
  MaxFakeVMTEntries = 200;
  {The pattern used to fill unused memory}
  DebugFillByte = $80;
{$IFDEF 32Bit}
  DebugFillPattern = $01010101 * Cardinal(DebugFillByte); // Default value $80808080
  {The address that is reserved so that accesses to the address of the fill
   pattern will result in an A/V. (Not used under 64-bit, since the upper half
   of the address space is always reserved by the OS.)}
  DebugReservedAddress = $01010000 * Cardinal(DebugFillByte); // Default value $80800000
{$ELSE}
  DebugFillPattern = $8080808080808080;
{$ENDIF}
  {The number of bytes of address space that cannot be allocated under FullDebugMode.  This block is reserved on
  startup and freed the first time the system runs out of address space.  This allows some subsequent memory allocation
  requests to succeed in order to allow the application to allocate some memory for error handling, etc. in response to
  the first EOutOfMemory exception.}
  FullDebugModeAddressSpaceSlack = 5 * 1024 * 1024;

{-------------------------FullDebugMode structures--------------------}
type
  TBlockOperation = (boBlockCheck, boGetMem, boFreeMem, boReallocMem);

  {The header placed in front of blocks in FullDebugMode (just after the
   standard header). Must be a multiple of 16 bytes in size otherwise the
   Align16Bytes option will not work. Current size = 128 bytes under 32-bit,
   and 240 bytes under 64-bit.}
  PFullDebugBlockHeader = ^TFullDebugBlockHeader;
  TFullDebugBlockHeader = record
    {Space used by the medium block manager for previous/next block management.
     If a medium block is binned then these two fields will be modified.}
    Reserved1: Pointer;
    Reserved2: Pointer;
    {Is the block currently allocated? If it is allocated this will be the
     address of the getmem routine through which it was allocated, otherwise it
     will be nil.}
    AllocatedByRoutine: Pointer;
    {The allocation group: Can be used in the debugging process to group
     related memory leaks together}
    AllocationGroup: Cardinal;
    {The allocation number: All new allocations are numbered sequentially. This
     number may be useful in memory leak analysis. If it reaches 4G it wraps
     back to 0.}
    AllocationNumber: Cardinal;
    {The call stack when the block was allocated}
    AllocationStackTrace: TStackTrace;
    {The thread that allocated the block}
    AllocatedByThread: Cardinal;
    {The thread that freed the block}
    FreedByThread: Cardinal;
    {The call stack when the block was freed}
    FreeStackTrace: TStackTrace;
    {The user requested size for the block. 0 if this is the first time the
     block is used.}
    UserSize: NativeUInt;
    {The object class this block was used for the previous time it was
     allocated. When a block is freed, the pointer that would normally be in the
     space of the class pointer is copied here, so if it is detected that
     the block was used after being freed we have an idea what class it is.}
    PreviouslyUsedByClass: NativeUInt;
    {The sum of all the dwords(32-bit)/qwords(64-bit) in this structure
     excluding the initial two reserved fields and this field.}
    HeaderCheckSum: NativeUInt;
  end;
  {The NativeUInt following the user area of the block is the inverse of
   HeaderCheckSum. This is used to catch buffer overrun errors.}

  {The class used to catch attempts to execute a virtual method of a freed
   object}
  TFreedObject = class
  public
    procedure GetVirtualMethodIndex;
    procedure VirtualMethodError;
{$IFDEF CatchUseOfFreedInterfaces}
    procedure InterfaceError;
{$ENDIF}
  end;

{$IFDEF FullDebugModeCallBacks}
  {FullDebugMode memory manager event callbacks. Note that APHeaderFreedBlock in the TOnDebugFreeMemFinish
   will not be valid for large (>260K) blocks.}
  TOnDebugGetMemFinish = procedure(APHeaderNewBlock: PFullDebugBlockHeader; ASize: NativeInt);
  TOnDebugFreeMemStart = procedure(APHeaderBlockToFree: PFullDebugBlockHeader);
  TOnDebugFreeMemFinish = procedure(APHeaderFreedBlock: PFullDebugBlockHeader; AResult: Integer);
  TOnDebugReallocMemStart = procedure(APHeaderBlockToReallocate: PFullDebugBlockHeader; ANewSize: NativeInt);
  TOnDebugReallocMemFinish = procedure(APHeaderReallocatedBlock: PFullDebugBlockHeader; ANewSize: NativeInt);

var
  {Note: FastMM will not catch exceptions inside these hooks, so make sure your hook code runs without
   exceptions.}
  OnDebugGetMemFinish: TOnDebugGetMemFinish = nil;
  OnDebugFreeMemStart: TOnDebugFreeMemStart = nil;
  OnDebugFreeMemFinish: TOnDebugFreeMemFinish = nil;
  OnDebugReallocMemStart: TOnDebugReallocMemStart = nil;
  OnDebugReallocMemFinish: TOnDebugReallocMemFinish = nil;
{$ENDIF}

{$IFDEF LoadDebugDLLDynamically}
function FastMM_IsDebugSupportLibraryLoaded: boolean;
function FastMM_LoadDebugSupportLibrary : boolean;
function FastMM_FreeDebugSupportLibrary : boolean;
{$ENDIF}
{$ENDIF}

{$IFDEF USE_CPUID}
function GetFastMMCpuFeatures: Word;
function GetFastMMCpuFeaturesA: Byte;
function GetFastMMCpuFeaturesB: Byte;
{$ENDIF}

{$IFNDEF USE_CPUID}
  {$undef EnableWaitPKG}
{$ENDIF}


{$IFDEF EnableWaitPKG}
procedure GetFastMMCpuUserModeMonitorLineSizes(var Smallest, Largest: Word);
procedure FastMMDisableWaitPKG;
{$ENDIF}


implementation

uses
{$IFNDEF POSIX}
  Windows,
  md_map,
  {$IFDEF _EventLog}
    {$IFDEF Delphi4or5}
  ShlObj,
    {$ELSE}
      {$IFNDEF FPC}
  SHFolder,
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFDEF MACOS}
  Posix.Stdlib, Posix.Unistd, Posix.Fcntl, Posix.PThread, FastMM_OSXUtil,
  {$ELSE}
    {$IFDEF fpc}
  BaseUnix,
    {$ELSE}
  Libc,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF LogLockContention}
  {$IFDEF FPC}
    {$undef LogLockContention}
    {$message error 'LogLockContention is not supported under FPC'}
  {$ENDIF}
{$ENDIF}

{$IFDEF LogLockContention}
  FastMM4DataCollector,
{$ENDIF}
{$IFDEF UseReleaseStack}
  FastMM4LockFreeStack,
{$ENDIF}
{$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically ) AND Defined( MemoryLoadLibrarySupport )}
  FastMMMemoryModule,
{$IFEND}
  FastMM4Messages;

{$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically ) AND Defined( MemoryLoadLibrarySupport )}
{$IF Defined( IncludeResource_madExcept )}
  {$IFDEF Win64}
    {$R FastMM_FullDebugMode_madExcept64.res}
  {$ELSE}
    {$R FastMM_FullDebugMode_madExcept.res}
  {$ENDIF}
{$ELSEIF Defined( IncludeResource )}
  {$IFDEF Win64}
    {$R FastMM_FullDebugMode64.res}
  {$ELSE}
    {$R FastMM_FullDebugMode.res}
  {$ENDIF}
{$IFEND Defined( IncludeResource_madExcept )}
{$IFEND}

const
  MaxFileNameLength                  = 1024;
  {The MaxFileNameLengthDouble value is extracted from the FastMM4 code
  as an effort to replace all "magic" (unnamed numerical constants) with
  theier named counterparts. We have yet to igure out why some file names
  reserve a buffer of 1024 characters while some other file names reserve
  double of that} {todo: MaxFileNameLengthDouble figure out - see the comment}
  MaxFileNameLengthDouble            = MaxFileNameLength*2;
  MaxDisplayMessageLength            = 1024;
  MaxLogMessageLength                = 32768;

{$IFDEF fpc}
const
  clib = 'c';

function valloc(__size:size_t):pointer;cdecl;external clib name 'valloc';
procedure free(__ptr:pointer);cdecl;external clib name 'free';
function usleep(__useconds:dword):longint;cdecl;external clib name 'usleep';
{$ENDIF}

{Fixed size move procedures. The 64-bit versions assume 16-byte alignment.}
{$IFDEF 64bit}
{$IFDEF Align32Bytes}
  {Used to exclude the procedures that we don't need, from compiling, to not
  rely on the "smart" linker to do this job for us}
  {$define ExcludeSmallGranularMoves}
{$ENDIF}
{$ENDIF}

{$IFDEF UseCustomFixedSizeMoveRoutines}

{$IFNDEF ExcludeSmallGranularMoves}
procedure Move4(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move12(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move20(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move28(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move36(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move44(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move52(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move60(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move68(const ASource; var ADest; ACount: NativeInt); forward;
{$ENDIF}

{$IFDEF 64Bit}
{These are not needed and thus unimplemented under 32-bit}
{$IFNDEF ExcludeSmallGranularMoves}
procedure Move8(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move16(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move24(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move32(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move40(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move48(const ASource; var ADest; ACount: NativeInt); forward;
{$ENDIF}
procedure Move56(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move64(const ASource; var ADest; ACount: NativeInt); forward;
{$ENDIF}

{$ENDIF UseCustomFixedSizeMoveRoutines}

{$IFDEF DetectMMOperationsAfterUninstall}
{Invalid handlers to catch MM operations after uninstall}
function InvalidFreeMem(APointer: Pointer): {$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}; forward;
function InvalidGetMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer; forward;
function InvalidReallocMem({$IFDEF fpc}var {$ENDIF}APointer: Pointer; ANewSize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer; forward;
function InvalidAllocMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUint{$ELSE}Cardinal{$ENDIF}{$ENDIF}): Pointer; forward;
function InvalidRegisterAndUnRegisterMemoryLeak(APointer: Pointer): Boolean; forward;
{$ENDIF}

{-------------------------Private constants----------------------------}

const

{$IFDEF Align32Bytes}
  MediumBlockSizeOffset = 64;
{$ELSE}
  MediumBlockSizeOffset = 48;
{$ENDIF}

  {The size of a medium block pool. This is allocated through VirtualAlloc and
   is used to serve medium blocks. The size must be a multiple of 16 (or 32, depending on alignment) and at
   least "SizeOf(Pointer)" bytes less than a multiple of 4K (the page size) to
   prevent a possible read access violation when reading past the end of a
   memory block in the optimized move routine (MoveX16LP/MoveX32LP).
   In Full Debug mode we leave a trailing 256 bytes to be able to safely
   do a memory dump.}
  MediumBlockPoolSize = 20 * 64 * 1024 -
  {$IFNDEF FullDebugMode}
    {$IFDEF Align32Bytes}
          32
    {$ELSE}
          16
    {$ENDIF Align32Bytes}
  {$ELSE}
      256
  {$ENDIF FullDebugMode};

  UnsignedBit = NativeUInt(1);


  {According to the Intel 64 and IA-32 Architectures Software Developers Manual,
  p. 3.7.5 (Specifying an Offset) and 3.7.5.1 (Specifying an Offset in 64-Bit Mode):
  "Scale factor - A value of 2, 4, or 8 that is multiplied by the index value";
  The value of MaximumCpuScaleFactor is determined by the processor architecture}
  MaximumCpuScaleFactorPowerOf2 = 3;
  MaximumCpuScaleFactor = Byte(UnsignedBit shl MaximumCpuScaleFactorPowerOf2);
  {The granularity of small blocks}
{$IFDEF Align32Bytes}
  SmallBlockGranularityPowerOf2 = 5;
{$ELSE}
{$IFDEF Align16Bytes}
  SmallBlockGranularityPowerOf2 = 4;
{$ELSE}
  SmallBlockGranularityPowerOf2 = 3;
{$ENDIF}
{$ENDIF}
  SmallBlockGranularity = Byte(UnsignedBit shl SmallBlockGranularityPowerOf2);


  {The granularity of medium blocks. Newly allocated medium blocks are
   a multiple of this size plus MediumBlockSizeOffset, to avoid cache line
   conflicts}
  MediumBlockGranularityPowerOf2 = 8;
  MediumBlockGranularity = UnsignedBit shl MediumBlockGranularityPowerOf2;
  MediumBlockGranularityMask = NativeUInt(-NativeInt(MediumBlockGranularity));

  {The granularity of large blocks}
  LargeBlockGranularity = 65536;
  {The maximum size of a small block. Blocks Larger than this are either
   medium or large blocks.}
  LargeBlockGranularityMask = NativeUInt(-LargeBlockGranularity);

{$IFDEF Align32Bytes}
  MaximumSmallBlockSize = 2624;
{$ELSE}
  MaximumSmallBlockSize = 2608;
{$ENDIF}

  {The smallest medium block size. (Medium blocks are rounded up to the nearest
   multiple of MediumBlockGranularity plus MediumBlockSizeOffset)}
  MinimumMediumBlockSize = 11 * MediumBlockGranularity + MediumBlockSizeOffset;

  {$IFDEF OperatorsInDefinesSupported}
    {$if (MaximumSmallBlockSize mod SmallBlockGranularity) <> 0 }
      {$Message Fatal 'Invalid MaximumSmallBlockSize granularity'}
    {$ifend}
  {$ENDIF}

  {The number of bins reserved for medium blocks}
  MediumBlockBinsPerGroupPowerOf2 = 5;
  {Must be a power of 2, otherwise masks would not work}
  MediumBlockBinsPerGroup = Byte(UnsignedBit shl MediumBlockBinsPerGroupPowerOf2);
  MediumBlockBinGroupCount = 32;
  MediumBlockBinCount = MediumBlockBinGroupCount * MediumBlockBinsPerGroup;
  {The maximum size allocatable through medium blocks. Blocks larger than this
   fall through to VirtualAlloc ( = large blocks).}
  MaximumMediumBlockSize = MinimumMediumBlockSize + (MediumBlockBinCount - 1) * MediumBlockGranularity;
  {The target number of small blocks per pool. The actual number of blocks per
   pool may be much greater for very small sizes and less for larger sizes. The
   cost of allocating the small block pool is amortized across all the small
   blocks in the pool, however the blocks may not all end up being used so they
   may be lying idle.}
  TargetSmallBlocksPerPool = 48;
  {The minimum number of small blocks per pool. Any available medium block must
   have space for roughly this many small blocks (or more) to be useable as a
   small block pool.}
  MinimumSmallBlocksPerPool = 12;
  {The lower and upper limits for the optimal small block pool size}
  OptimalSmallBlockPoolSizeLowerLimit = 29 * 1024 - Cardinal(MediumBlockGranularity) + MediumBlockSizeOffset;
  OptimalSmallBlockPoolSizeUpperLimit = 64 * 1024 - Cardinal(MediumBlockGranularity) + MediumBlockSizeOffset;
  {The maximum small block pool size. If a free block is this size or larger
   then it will be split.}
  MaximumSmallBlockPoolSize = OptimalSmallBlockPoolSizeUpperLimit + MinimumMediumBlockSize;
  {-------------Block type flags--------------}
  {The lower 3 bits in the dword header of small blocks (4 bits in medium and
   large blocks) are used as flags to indicate the state of the block}
  {Set if the block is not in use}
  IsFreeBlockFlag = 1;
  {Set if this is a medium block}
  IsMediumBlockFlag = 2;
  {Set if it is a medium block being used as a small block pool. Only valid if
   IsMediumBlockFlag is set.}
  IsSmallBlockPoolInUseFlag = 4;
  {Set if it is a large block. Only valid if IsMediumBlockFlag is not set.}
  IsLargeBlockFlag = 4;
  {Is the medium block preceding this block available? (Only used by medium
   blocks)}
  PreviousMediumBlockIsFreeFlag = 8;
  {Is this large block segmented? I.e. is it actually built up from more than
   one chunk allocated through VirtualAlloc? (Only used by large blocks.)}
  LargeBlockIsSegmented = 8;
  {The flags masks for small blocks}
  DropSmallFlagsMask = NativeUint(-8);
  {$IFDEF CheckHeapForCorruption}
  ExtractSmallFlagsMask = NativeUint(7);
  {$ENDIF}
  {The flags masks for medium and large blocks}
{$IFDEF Align32Bytes}
  DropMediumAndLargeFlagsMask = -32;
  ExtractMediumAndLargeFlagsMask = 31;
{$ELSE}
  DropMediumAndLargeFlagsMask = -16;
  ExtractMediumAndLargeFlagsMask = 15;
{$ENDIF}
  {-------------Block resizing constants---------------}
  {The upsize and downsize checker must a a multiple of the granularity,
   otherwise on big-granularity and small upsize/downsize constant values,
   reallocating 1-byte blocks, keeping the same size as before, will return
   different pointer, and, as a result, the FastCode validation suite
   will not pass}
  SmallBlockDownsizeCheckAdder = SmallBlockGranularity*4;
  SmallBlockUpsizeAdder = SmallBlockGranularity*2;
  {When a medium block is reallocated to a size smaller than this, then it must
   be reallocated to a small block and the data moved. If not, then it is
   shrunk in place down to MinimumMediumBlockSize. Currently the limit is set
   at a quarter of the minimum medium block size.}
  MediumInPlaceDownsizeLimit = MinimumMediumBlockSize div 4;
  {-------------Memory leak reporting constants---------------}
  ExpectedMemoryLeaksListSize = 64 * 1024;
  {-------------Other constants---------------}
{$IFNDEF NeverSleepOnThreadContention}
  {Sleep time when a resource (small/medium/large block manager) is in use}
  InitialSleepTime = 0;
  {Used when the resource is still in use after the first sleep}
  AdditionalSleepTime = 1;
{$ENDIF}
  {Hexadecimal characters}
  HexTable: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
{$IFDEF FullDebugMode}
  {Virtual Method Called On Freed Object Errors}
{$IFNDEF FPC}
  StandardVirtualMethodNames: array[1 + vmtParent div SizeOf(Pointer) .. vmtDestroy div SizeOf(Pointer)] of PAnsiChar = (
{$IFDEF BCB6OrDelphi6AndUp}
  {$if RTLVersion >= 20}
    'Equals',
    'GetHashCode',
    'ToString',
  {$ifend}
{$ENDIF}
    'SafeCallException',
    'AfterConstruction',
    'BeforeDestruction',
    'Dispatch',
    'DefaultHandler',
    'NewInstance',
    'FreeInstance',
    'Destroy');
{$ENDIF}
  {The name of the FullDebugMode support DLL. The support DLL implements stack
   tracing and the conversion of addresses to unit and line number information.}
{$ENDIF}
{$IFDEF UseReleaseStack}
  ReleaseStackSize = 16;
  NumStacksPerBlock = 64; //should be power of 2
{$ENDIF}

{$IFDEF _StackTracer}
{$IFDEF 32Bit}
  FullDebugModeLibraryName = FullDebugModeLibraryName32Bit;
{$ELSE}
  FullDebugModeLibraryName = FullDebugModeLibraryName64Bit;
{$ENDIF}
{$ENDIF}

{$IFDEF Delphi4or5}
 reInvalidOp = 217;
{$ENDIF}


{-------------------------Private types----------------------------}
type

{$IFDEF Delphi4or5}
  {Delphi 5 Compatibility}
  PCardinal = ^Cardinal;
  PPointer = ^Pointer;
{$ENDIF}
{$IFDEF BCB4}
  {Define some additional types for BCB4}
  PInteger  = ^Integer;
{$ENDIF}

  {Move procedure type}
  TMoveProc = procedure(const ASource; var ADest; ACount: NativeInt);

{$IFDEF USE_CPUID}
  {Registers structure (for GetCPUID)
  The registers are used solely for the CPUID instruction,
  thus they are always 32-bit, even under 64-bit mode}
  TCpuIdRegisters = record
    RegEAX, RegEBX, RegECX, RegEDX: Cardinal;
  end;
{$ENDIF}

  {The layout of a string allocation. Used to detect string leaks.}
  PStrRec = ^StrRec;
  StrRec = packed record
{$IFDEF 64Bit}
    _Padding: Integer;
{$ENDIF}
{$IFDEF BCB6OrDelphi6AndUp}
  {$if RTLVersion >= 20}
    codePage: Word;
    elemSize: Word;
  {$ifend}
{$ENDIF}
    refCnt: Integer;
    length: Integer;
  end;

{$IFDEF EnableMemoryLeakReporting}
  {Different kinds of memory leaks}
  TMemoryLeakType = (mltUnexpectedLeak, mltExpectedLeakRegisteredByPointer,
    mltExpectedLeakRegisteredByClass, mltExpectedLeakRegisteredBySize);
{$ENDIF}

  TSynchronizationVariable =
  {$IFDEF SynchroVarLongint}
    LongInt
  {$ELSE}
    {$IFDEF XE2AndUp}
      System.ShortInt
    {$ELSE}
      Byte
    {$ENDIF}
  {$ENDIF}
  ;

  {---------------Small block structures-------------}

  {Pointer to the header of a small block pool}
  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  {Small block type (Size = 32 bytes for 32-bit, 64 bytes for 64-bit).}
  PSmallBlockType = ^TSmallBlockType;
  TSmallBlockType = record
    {True = Block type is locked}

    SmallBlockTypeLocked: TSynchronizationVariable; {The type is Byte for strict
				type checking when the typed "@" operator
				compiler option is ON.}

    {Bitmap indicating which of the first 8 medium block groups contain blocks
     of a suitable size for a block pool.}
    AllowedGroupsForBlockPoolBitmap: Byte;
{$IFDEF SynchroVarLongint}
    Reserved2: Byte;
{$ENDIF}
    {The block size for this block type}
    BlockSize: Word;
    {The minimum and optimal size of a small block pool for this block type}
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
    {The first partially free pool for the given small block. This field must
     be at the same offset as TSmallBlockPoolHeader.NextPartiallyFreePool.}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    {The last partially free pool for the small block type. This field must
     be at the same offset as TSmallBlockPoolHeader.PreviousPartiallyFreePool.}
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    {The offset of the last block that was served sequentially. The field must
     be at the same offset as TSmallBlockPoolHeader.FirstFreeBlock.}
    NextSequentialFeedBlockAddress: Pointer;
    {The last block that can be served sequentially.}
    MaxSequentialFeedBlockAddress: Pointer;
    {The pool that is current being used to serve blocks in sequential order}
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
{$IFDEF UseCustomFixedSizeMoveRoutines}
    {The fixed size move procedure used to move data for this block size when
     it is upsized. When a block is downsized (which usually does not occur
     that often) the variable size move routine is used.}
    UpsizeMoveProcedure: TMoveProc;
{$ELSE}
    {$IFNDEF SynchroVarLongint}
    Reserved1: Pointer;
    {$ENDIF}
{$ENDIF}
    {$IFDEF 64bit}
    Reserved3: Pointer;
    {$ENDIF}
{$IFDEF UseReleaseStack}
    ReleaseStack: array [0..NumStacksPerBlock - 1] of TLFStack;
{$ENDIF}
{$IFDEF LogLockContention}
    BlockCollector: TStaticCollector;
{$ENDIF}
  end;

  {Small block pool (Size = 32 bytes for 32-bit, 64 bytes for 64-bit).}
  TSmallBlockPoolHeader = record
    {BlockType}
    BlockType: PSmallBlockType;
{$IFDEF 32Bit}
    {Align the next fields to the same fields in TSmallBlockType and pad this
     structure to 32 bytes for 32-bit}
    Reserved1: Cardinal;
{$ENDIF}
    {The next and previous pool that has free blocks of this size. Do not
     change the position of these two fields: They must be at the same offsets
     as the fields in TSmallBlockType of the same name.}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    {Pointer to the first free block inside this pool. This field must be at
     the same offset as TSmallBlockType.NextSequentialFeedBlockAddress.}
    FirstFreeBlock: Pointer;
    {The number of blocks allocated in this pool.}
    BlocksInUse: Cardinal;
    {Padding}
    Reserved2: Cardinal;
    {The pool pointer and flags of the first block}
    FirstBlockPoolPointerAndFlags: NativeUInt;
{$IFDEF 64bit}
    Reserved3, Reserved4: Pointer; // Align the structure to 64-bit size
{$ENDIF}
  end;

  {Small block layout:
   At offset -SizeOf(Pointer) = Flags + address of the small block pool.
   At offset BlockSize - SizeOf(Pointer) = Flags + address of the small block
   pool for the next small block.
  }

  {------------------------Medium block structures------------------------}

  {The medium block pool from which medium blocks are drawn. Size = 16 bytes
   for 32-bit and 32 bytes for 64-bit.}
  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = record
    {Points to the previous and next medium block pools. This circular linked
     list is used to track memory leaks on program shutdown.}
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    {Padding}
    Reserved1: NativeUInt;
    {$IFDEF 32bit}
    {$IFDEF Align32Bytes}
    Reserved2, Reserved3, Reserved4, Reserved5: Pointer;
    {$ENDIF}
    {$ENDIF}
    {The block size and flags of the first medium block in the block pool}
    FirstMediumBlockSizeAndFlags: NativeUInt;
  end;

  {Medium block layout:
   Offset: -2 * SizeOf(Pointer) = Previous Block Size (only if the previous block is free)
   Offset: -SizeOf(Pointer) = This block size and flags
   Offset: 0 = User data / Previous Free Block (if this block is free)
   Offset: SizeOf(Pointer) = Next Free Block (if this block is free)
   Offset: BlockSize - 2*SizeOf(Pointer) = Size of this block (if this block is free)
   Offset: BlockSize - SizeOf(Pointer) = Size of the next block and flags}

  {A medium block that is unused}
  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  {-------------------------Large block structures------------------------}

  {Large block header record (Size = 16 for 32-bit unless we have 32-bytes alignment, 32 for 64-bit or if we have 32-bytes alignment)}
  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = record
    {Points to the previous and next large blocks. This circular linked
     list is used to track memory leaks on program shutdown.}
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    {$IFDEF 32bit}
    {$IFDEF Align32Bytes}
    Reserved1, Reserved2, Reserved3, Reserved4: Pointer;
    {$ENDIF}
    {$ENDIF}
    {The user allocated size of the Large block}
    UserAllocatedSize: NativeUInt;
    {The size of this block plus the flags}
    BlockSizeAndFlags: NativeUInt;
  end;

  {-------------------------Expected Memory Leak Structures--------------------}
{$IFDEF EnableMemoryLeakReporting}

  {The layout of an expected leak. All fields may not be specified, in which
   case it may be harder to determine which leaks are expected and which are
   not.}
  PExpectedMemoryLeak = ^TExpectedMemoryLeak;
  PPExpectedMemoryLeak = ^PExpectedMemoryLeak;
  TExpectedMemoryLeak = record
    {Linked list pointers}
    PreviousLeak, NextLeak: PExpectedMemoryLeak;
    {Information about the expected leak}
    LeakAddress: Pointer;
    LeakedClass: TClass;
    {$IFDEF CheckCppObjectTypeEnabled}
    LeakedCppTypeIdPtr: Pointer;
    {$ENDIF}
    LeakSize: NativeInt;
    LeakCount: Integer;
  end;

  TExpectedMemoryLeaks = record
    {The number of entries used in the expected leaks buffer}
    EntriesUsed: Integer;
    {Freed entries}
    FirstFreeSlot: PExpectedMemoryLeak;
    {Entries with the address specified}
    FirstEntryByAddress: PExpectedMemoryLeak;
    {Entries with no address specified, but with the class specified}
    FirstEntryByClass: PExpectedMemoryLeak;
    {Entries with only size specified}
    FirstEntryBySizeOnly: PExpectedMemoryLeak;
    {The expected leaks buffer (Need to leave space for this header)}
    ExpectedLeaks: array[0..(ExpectedMemoryLeaksListSize - 64) div SizeOf(TExpectedMemoryLeak) - 1] of TExpectedMemoryLeak;
  end;
  PExpectedMemoryLeaks = ^TExpectedMemoryLeaks;

{$ENDIF}

{-------------------------Private constants----------------------------}
const
  {$IFDEF 32bit}
  MediumFreeBlockSizePowerOf2 = 3;
  {$ELSE}
  MediumFreeBlockSizePowerOf2 = 4;
  {$ENDIF}

  {$IFDEF OperatorsInDefinesSupported}
    {$if 1 shl MediumFreeBlockSizePowerOf2 <> SizeOf(TMediumFreeBlock)}
      {$Message Fatal 'Invalid MediumFreeBlockSizePowerOf2 constant or SizeOf(TMediumFreeBlock) is not a power of 2'}
    {$ifend}
  {$ENDIF}

{$IFNDEF LogLockContention}
  {$define SmallBlockTypeRecSizeIsPowerOf2}
{$ENDIF}

{$IFNDEF SmallBlockTypeRecSizeIsPowerOf2}
  SmallBlockTypeRecSize = SizeOf(TSmallBlockType);
{$ENDIF}

{$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
  {$IFDEF 32bit}
    SmallBlockTypeRecSizePowerOf2 = 5;
  {$ENDIF}
  {$IFDEF 64bit}
    SmallBlockTypeRecSizePowerOf2 = 6;
  {$ENDIF}
  SmallBlockTypeRecSize = Byte(UnsignedBit shl SmallBlockTypeRecSizePowerOf2);
{$ENDIF}

{$IFNDEF UseReleaseStack}
  {$IFDEF OperatorsInDefinesSupported}
    {$if SmallBlockTypeRecSize <> SizeOf(TSmallBlockType)}
      {$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
        {$Message Fatal 'Invalid SmallBlockTypeRecSizePowerOf2 constant or SizeOf(TSmallBlockType) is not a power of 2'}
      {$ENDIF}
    {$ifend}
  {$ENDIF}
{$ENDIF}

{$IFNDEF BCB6OrDelphi7AndUp}
  reOutOfMemory = 1;
  reInvalidPtr = 2;
{$ENDIF}
  {The size of the block header in front of small and medium blocks}
  BlockHeaderSize = SizeOf(Pointer);

  {The size of a small block pool header: 32 bytes for 32-bit, 64 bytes for 64-bit).}
  SmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);
  {$IFDEF OperatorsInDefinesSupported}
    {$IFDEF 32bit}
    {$if SmallBlockPoolHeaderSize <> 32}
       {$Message Fatal 'SmallBlockPoolHeaderSize should be 32 bytes for 32-bit'}
    {$ifend}
    {$ELSE}
    {$if SmallBlockPoolHeaderSize <> 64}
       {$Message Fatal 'SmallBlockPoolHeaderSize should be 64 bytes for 64-bit'}
    {$ifend}
    {$ENDIF}
  {$ENDIF}

  {The size of a medium block pool header: 16 bytes for 32-bit and 32 bytes for 64-bit.}
  MediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);
  {$IFDEF OperatorsInDefinesSupported}
    {$IFDEF 32bit}
      {$IFDEF Align32Bytes}
        {$if MediumBlockPoolHeaderSize <> 32}
           {$Message Fatal 'MediumBlockPoolHeaderSize should be 32 bytes for 32-bit with 32-bytes alignment'}
        {$ifend}
      {$ELSE}
        {$if MediumBlockPoolHeaderSize <> 16}
           {$Message Fatal 'MediumBlockPoolHeaderSize should be 16 bytes for 32-bit unless we have 32-bytes alignment'}
        {$ifend}
      {$ENDIF}
    {$ELSE}
    {$if MediumBlockPoolHeaderSize <> 32}
       {$Message Fatal 'MediumBlockPoolHeaderSize should be 32 bytes for 64-bit'}
    {$ifend}
    {$ENDIF}
  {$ENDIF}

  {The size of the header in front of Large blocks}
  LargeBlockHeaderSize = SizeOf(TLargeBlockHeader);
{$IFDEF FullDebugMode}
  {We need space for the header, the trailer checksum and the trailing block
   size (only used by freed medium blocks).}
  FullDebugBlockOverhead = SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt) + SizeOf(Pointer);
{$ENDIF}


  {The distinction between AVX1 and AVX2 is on how it clears the registers
  and how it avoids AVX-SSE transition penalties.
  AVX2 uses the VPXOR instruction, not available on AVX1. On most Intel
  processors, VPXOR is faster is VXORPS. For example, on Sandybridge, VPXOR can
  run on any of the 3 ALU execution ports, p0/p1/p5.  VXORPS can only run on p5.
  Also, AVX1 uses the VZEROUPPER instruction, while AVX2 does not. Newer CPU
  doesn't have such a huge transition penaly, and VZEROUPPER is not needed,
  moreover, it can make subsequent SSE code slower}
  {On ERMSB, see p. 3.7.6 of the
  Intel 64 and IA-32 Architectures Optimization Reference Manual}

{$IFDEF EnableMMX}
  FastMMCpuFeatureMMX                           = Byte(UnsignedBit shl 0);
{$ENDIF}

{$IFDEF EnableAVX}
  FastMMCpuFeatureAVX1                          = Byte(UnsignedBit shl 1);
  FastMMCpuFeatureAVX2                          = Byte(UnsignedBit shl 2);
  {$IFDEF EnableAVX512}
  FastMMCpuFeatureAVX512                        = Byte(UnsignedBit shl 3);
  {$ENDIF}
{$ENDIF}

{$IFDEF EnableERMS}
  FastMMCpuFeatureERMS                          = Byte(UnsignedBit shl 4);
{$ENDIF}

{$IFNDEF DisablePauseAndSwitchToThread}
{$IFNDEF AssumePauseAndSwitchToThreadAvailable}
  {CPU supports "pause" instruction and Windows supports SwitchToThread() API call}
  FastMMCpuFeaturePauseAndSwitch                = Byte(UnsignedBit shl 5);
{$ENDIF}
{$ENDIF}

{$IFDEF 32bit_SSE}
  {CPU supports xmm registers in 32-bit mode}
  FastMMCpuFeatureSSE                           = Byte(UnsignedBit shl 6);
{$ENDIF}

{$IFDEF EnableFSRM}
  {Fast Short REP MOVSB }
  FastMMCpuFeatureFSRM                          = Byte(UnsignedBit shl 7);
{$ENDIF}

{$IFDEF EnableWaitPKG}
const
  {WAITPKG (UMONITOR/UMWAIT) }
  FastMMCpuFeatureB_WAITPKG                     = Byte(UnsignedBit shl 0);
{$ENDIF}

{-------------------------Private variables----------------------------}
var
  {-----------------Small block management------------------}
{$IFDEF SmallBlocksLockedCriticalSection}
  SmallBlockCriticalSections: array[0..NumSmallBlockTypes-1] of TRtlCriticalSection;
{$ENDIF}

{The small block types. Sizes include the leading header. Sizes are
 picked to limit maximum wastage to about 10% or 256 bytes (whichever is
 less) where possible.}
  SmallBlockTypes: array[0..NumSmallBlockTypes - 1] of TSmallBlockType;

  SmallBlockTypeSizes: array[0..NumSmallBlockTypes - 1] of Word = (
    {8/16 byte jumps}
{$IFNDEF Align32Bytes}
{$IFNDEF Align16Bytes}
    8,
{$ENDIF Align16Bytes}
    16,
{$IFNDEF Align16Bytes}
    24,
{$ENDIF Align16Bytes}
{$ENDIF Align32Bytes}
    32,
{$IFNDEF Align32Bytes}
{$IFNDEF Align16Bytes}
    40,
{$ENDIF}
    48,
{$IFNDEF Align16Bytes}
    56,
{$ENDIF}
{$ENDIF}
    64,
{$IFNDEF Align32Bytes}
{$IFNDEF Align16Bytes}
    72,
{$ENDIF}
    80,
{$IFNDEF Align16Bytes}
    88,
{$ENDIF}
{$ENDIF}
    96,
{$IFNDEF Align32Bytes}
{$IFNDEF Align16Bytes}
    104,
{$ENDIF}
    112,
{$IFNDEF Align16Bytes}
    120,
{$ENDIF}
{$ENDIF}
    128,

{$IFNDEF Align32Bytes}
{$IFNDEF Align16Bytes}
    136,
{$ENDIF}
    144,
{$IFNDEF Align16Bytes}
    152,
{$ENDIF}
{$ENDIF}
    160,
    {16 byte jumps}
{$IFNDEF Align32Bytes}
    176,
{$ENDIF}
    192,
{$IFNDEF Align32Bytes}
    208,
{$ENDIF}
    224,
{$IFNDEF Align32Bytes}
    240,
{$ENDIF}
    256,
{$IFNDEF Align32Bytes}
    272,
{$ENDIF}
    288,
{$IFNDEF Align32Bytes}
    304,
{$ENDIF}
    320,
    {32 byte jumps}
    352,
    384,
    416,
    448,
    480,
{$IFNDEF Align32Bytes}
    {48 byte jumps if alignment is less than 32 bytes}
    528,
    576,
    624,
    672,
    {64 byte jumps}
    736,
    800,
    {80 byte jumps if alignment is less than 32 bytes}
    880,
    1024{960},
    {96 byte jumps}
    1056,
    1152,
    {112 byte jumps}
    1264,
    1376,
    {128 byte jumps}
    1504,
    {144 byte jumps}
    1648,
    {160 byte jumps}
    1808,
    {176 byte jumps}
    2048{1984},
    {192 byte jumps}
    2176,
    {208 byte jumps}
    2384,
    {224 byte jumps}
{$ELSE}
    {keep 32-byte jumps if alignment is 32 bytes}
     512,
     544,
     576,
     608,
     640,
     672,
     704,
     736,
     768,
     800,
     832,
    {64 byte jumps}
     896,
     960,
     1024,
     1088,
     1152,
     1216,
     1280,
     1344,
     1408,
    {128 byte jumps}
     1536,
     1664,
     1792,
     1920,
     2048,
    {256 byte jumps}
     2304,
{$ENDIF}
    MaximumSmallBlockSize,
    {The last block size occurs three times. If, during a GetMem call, the
     requested block size is already locked by another thread then up to two
     larger block sizes may be used instead. Having the last block size occur
     three times avoids the need to have a size overflow check.}
    MaximumSmallBlockSize,
    MaximumSmallBlockSize);


  {Size to small block type translation table.
   This table helps us to quickly access a corresponding TSmallBlockType entry in the
   SmallBlockTypes array.}

{$IFDEF 32Bit}
  {$IFNDEF LogLockContention}
    {$DEFINE AllocSize2SmallBlockTypesPrecomputedOffsets}
  {$ENDIF}

  {$IFDEF ASMVersion}
      {Since the size of TSmallBlockType is 32 bytes in 32-bit mode,
      but the maximum scale factor of an index is 8 when calculating an offset on Intel CPUs,
      this table contains precomputed offsets from the start of the SmallBlockTypes ararray,
      divided by the maximum CPU scale factor, so we don't need to do shl, we just take a value from
      this table a and then use *8 scale factor to calculate the effective address and get the value}

      {$IFNDEF LogLockContention}
        {$DEFINE AllocSize2SmallBlockTypesPrecomputedOffsets}
      {$ENDIF}
  {$ENDIF}

  {$IFDEF FastGetMemNeedAssemblerCode}
     {$DEFINE AllocSize2SmallBlockTypesPrecomputedOffsets}
  {$ENDIF}

{$ENDIF}

{$IFDEF AllocSize2SmallBlockTypesPrecomputedOffsets}

  AllocSz2SmlBlkTypOfsDivSclFctr: array[0..(MaximumSmallBlockSize - 1) div SmallBlockGranularity] of Byte;

{$ELSE}

   {Since the size of TSmallBlockType is 64 bytes in 64-bit mode and 32 bytes in 32-bit mode,
   but the maximum scale factor of an index is 8 when calculating an offset on Intel CPUs,
   and the table contains more than 40 elements, one byte in the table is not enough to hold any
   offfset value divided by 8, so, for 64-bit mode, we keep here just indexes, and use one additional shl command,
   no offsets are precomputed}
  AllocSize2SmallBlockTypesIdx: array[0..(MaximumSmallBlockSize - 1) div SmallBlockGranularity] of Byte;
{$ENDIF}

  {-----------------Medium block management------------------}
  {A dummy medium block pool header: Maintains a circular list of all medium
   block pools to enable memory leak detection on program shutdown.}
  MediumBlockPoolsCircularList: TMediumBlockPoolHeader;

  {Are medium blocks locked?}

  MediumBlocksLocked: TSynchronizationVariable;

{$IFDEF MediumBlocksLockedCriticalSection}
  MediumBlocksLockedCS: TRTLCriticalSection;
{$ENDIF}

  {The sequential feed medium block pool.}
  LastSequentiallyFedMediumBlock: Pointer;
  MediumSequentialFeedBytesLeft: Cardinal;
  {The medium block bins are divided into groups of 32 bins. If a bit
   is set in this group bitmap, then at least one bin in the group has free
   blocks.}
  MediumBlockBinGroupBitmap: Cardinal;
  {The medium block bins: total of 32 * 32 = 1024 bins of a certain
   minimum size.}
  MediumBlockBinBitmaps: array[0..MediumBlockBinGroupCount - 1] of Cardinal;
  {The medium block bins. There are 1024 LIFO circular linked lists each
   holding blocks of a specified minimum size. The sizes vary in size from
   MinimumMediumBlockSize to MaximumMediumBlockSize. The bins are treated as
   type TMediumFreeBlock to avoid pointer checks.}
  MediumBlockBins: array[0..MediumBlockBinCount - 1] of TMediumFreeBlock;
  {-----------------Large block management------------------}
  {Are large blocks locked?}
  LargeBlocksLocked: TSynchronizationVariable;

{$IFDEF LargeBlocksLockedCriticalSection}
  LargeBlocksLockedCS: TRTLCriticalSection;
{$ENDIF}
  {A dummy large block header: Maintains a list of all allocated large blocks
   to enable memory leak detection on program shutdown.}
  LargeBlocksCircularList: TLargeBlockHeader;
  {-------------------------Expected Memory Leak Structures--------------------}
{$IFDEF EnableMemoryLeakReporting}
  {The expected memory leaks}
  ExpectedMemoryLeaks: PExpectedMemoryLeaks;
  ExpectedMemoryLeaksListLocked: TSynchronizationVariable;
{$ENDIF}
  {---------------------EventLog-------------------}
{$IFDEF _EventLog}
  {The current log file name}
  MMLogFileName: array[0..MaxFileNameLength-1] of AnsiChar;
{$ENDIF}
  {---------------------Full Debug Mode structures--------------------}
{$IFDEF FullDebugMode}
  {The allocation group stack}
  AllocationGroupStack: array[0..AllocationGroupStackSize - 1] of Cardinal;
  {The allocation group stack top (it is an index into AllocationGroupStack)}
  AllocationGroupStackTop: Cardinal;
  {The last allocation number used}
  CurrentAllocationNumber: Cardinal;
  {This is a count of the number of threads currently inside any of the
   FullDebugMode GetMem, Freemem or ReallocMem handlers. If this value
   is negative then a block scan is in progress and no thread may
   allocate, free or reallocate any block or modify any FullDebugMode
   block header or footer.}
  ThreadsInFullDebugModeRoutine: Integer;
  {The 64K block of reserved memory used to trap invalid memory accesses using
   fields in a freed object.}
  ReservedBlock: Pointer;
  {Points to a block of size FullDebugModeAddressSpaceSlack that is freed the first time the system runs out of memory.
  Memory is never release under FullDebugMode, so this allows the application to continue to function for a short while
  after the first EOutOfMemory exception.}
  AddressSpaceSlackPtr: Pointer;
  {The virtual method index count - used to get the virtual method index for a
   virtual method call on a freed object.}
  VMIndex: Integer;

{$IFNDEF FPC}
  {The fake VMT used to catch virtual method calls on freed objects.}
  FreedObjectVMT: packed record
    VMTData: array[vmtSelfPtr .. vmtParent + SizeOf(Pointer) - 1] of byte;
    VMTMethods: array[SizeOf(Pointer) + vmtParent .. vmtParent + MaxFakeVMTEntries * SizeOf(Pointer) + SizeOf(Pointer) - 1] of Byte;
  end;
{$ENDIF}
  {$IFDEF CatchUseOfFreedInterfaces}
  VMTBadInterface: array[0..MaxFakeVMTEntries - 1] of Pointer;
  {$ENDIF}
{$ENDIF}

  {---------------------Lock contention logging--------------------}
{$IFDEF LogLockContention}
  MediumBlockCollector: TStaticCollector;
  LargeBlockCollector: TStaticCollector;
{$ENDIF}

  {---------------------Release stack------------------------}
{$IFDEF UseReleaseStack}
  MediumReleaseStack: array [0..NumStacksPerBlock - 1] of TLFStack;
  LargeReleaseStack: array [0..NumStacksPerBlock - 1] of TLFStack;
  ReleaseStackCleanupThread: THandle = 0;
  ReleaseStackCleanupThreadTerminate: THandle = 0;
{$ENDIF}

  {--------------Other info--------------}
  {The memory manager that was replaced}
  OldMemoryManager: {$IFNDEF BDS2006AndUp}TMemoryManager{$ELSE}TMemoryManagerEx{$ENDIF};
  {The replacement memory manager}
  NewMemoryManager: {$IFNDEF BDS2006AndUp}TMemoryManager{$ELSE}TMemoryManagerEx{$ENDIF};
{$IFDEF DetectMMOperationsAfterUninstall}
  {Invalid handlers to catch MM operations after uninstall}
  InvalidMemoryManager: {$IFNDEF BDS2006AndUp}TMemoryManager{$ELSE}TMemoryManagerEx{$ENDIF};
{$ENDIF}

{$IFDEF MMSharingEnabled}
  {A string uniquely identifying the current process (for sharing the memory
   manager between DLLs and the main application)}
  MappingObjectName: array[0..25] of AnsiChar = ('L', 'o', 'c', 'a', 'l', '\',
    'F', 'a', 's', 't', 'M', 'M', '_', 'P', 'I', 'D', '_', '?', '?', '?', '?',
    '?', '?', '?', '?', #0);
{$IFDEF EnableBackwardCompatibleMMSharing}
  UniqueProcessIDString: array[1..20] of AnsiChar = ('?', '?', '?', '?', '?',
    '?', '?', '?', '_', 'P', 'I', 'D', '_', 'F', 'a', 's', 't', 'M', 'M', #0);
  UniqueProcessIDStringBE: array[1..23] of AnsiChar = ('?', '?', '?', '?', '?',
    '?', '?', '?', '_', 'P', 'I', 'D', '_', 'F', 'a', 's', 't', 'M', 'M', '_',
    'B', 'E', #0);
  {The handle of the MM window}
  MMWindow: HWND;
  {The handle of the MM window (for default MM of Delphi 2006 compatibility)}
  MMWindowBE: HWND;
{$ENDIF}
  {The handle of the memory mapped file}
  MappingObjectHandle: NativeUInt;
{$ENDIF}
  {Has FastMM been installed?}
  FastMMIsInstalled: Boolean;
  {Is the MM in place a shared memory manager?}
  IsMemoryManagerOwner: Boolean;

{$IFDEF USE_CPUID}
  {See FastMMCpuFeature... constants.
  We have packe the most interesting CPUID bits in one byte for faster comparison
  These features are mostly used for faster memory move operations}
  FastMMCpuFeaturesA: Byte;
  FastMMCpuFeaturesB: Byte;

  FastMMCpuSmallestMonitorLineSize: Word;
  FastMMCpuLargestMonitorLineSize: Word;
{$ENDIF}

  {Is a MessageBox currently showing? If so, do not show another one.}
  ShowingMessageBox: Boolean;
  {True if RunInitializationCode has been called already.}
  InitializationCodeHasRun: Boolean;

{----------------Utility Functions------------------}

{A copy of StrLen in order to avoid the SysUtils unit, which would have
 introduced overhead like exception handling code.}
function StrLen(const AStr: PAnsiChar): NativeUInt;
{$IFNDEF Use32BitAsm}
begin
  Result := 0;
  while AStr[Result] <> #0 do
  begin
    Inc(Result);
  end;
end;
{$ELSE}
 assembler;
asm
  {Check the first byte}
  cmp byte ptr [eax], 0
  je @ZeroLength
  {Get the negative of the string start in edx}
  mov edx, eax
  neg edx
  {Word align}
  add eax, 1
  and eax, -2
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@ScanLoop:
  mov cx, [eax]
  add eax, 2
  test cl, ch
  jnz @ScanLoop
  test cl, cl
  jz @ReturnLess2
  test ch, ch
  jnz @ScanLoop
  lea eax, [eax + edx - 1]
  jmp @Finish
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@ReturnLess2:
  lea eax, [eax + edx - 2]
  jmp @Finish
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@ZeroLength:
  xor eax, eax
@Finish:
end;
{$ENDIF}

{$IFDEF USE_CPUID}
{Returns true if the CPUID instruction is supported}
function CPUID_Supported: Boolean;
{$IFDEF 32bit} assembler;

{QUOTE from the Intel 64 and IA-32 Architectures Software Developer's Manual

22.16.1 Using EFLAGS Flags to Distinguish Between 32-Bit IA-32 Processors
The following bits in the EFLAGS register that can be used to differentiate between the 32-bit IA-32 processors:
- Bit 21 (the ID flag) indicates whether an application can execute the CPUID instruction. The ability to set and
clear this bit indicates that the processor is a P6 family or Pentium processor. The CPUID instruction can then
be used to determine which processor.

ENDQUOTE}


asm
  pushfd
  pop eax
  mov edx, eax
{Test the bit 21 (the ID flag}
  xor eax, $200000
  push eax
  popfd
  pushfd
  pop eax
  xor eax, edx
  setnz al
end;
{$ELSE 32bit}

{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}
// CPUID is always supported on 64-bit platforms
begin
  Result := True;
end;
{$ENDIF 32bit}

{Gets the CPUID}
procedure GetCPUID(AEax, AEcx: Cardinal; var R: TCpuIdRegisters); assembler;
{$IFDEF 32bit}
asm
  push ebx
  push esi

{ According to the 32-bit calling convention, the arguments are passed in the
  following registers:
  1) eax (first argument, in the GetCPUID function called "AEax" (Cardinal))
  2) edx (second argument in the GetCPUID function called "ECx" (Cardinal))
  3) ecx (third argument, the address of the "R" (TCpuIdRegisters) structure)}

  mov  esi, ecx // now the address of the TCpuIdRegisters structure is in esi register
  mov  ecx, edx // now the value of the second argument is in the ecx register
  {Clear the registers, not really needed, justs for sure/safe}
  xor  ebx, ebx
  xor  edx, edx
  {cpuid instruction}
{$IFDEF Delphi4or5}
  db $0f, $a2
{$ELSE}
  cpuid
{$ENDIF}
  {Save registers}
  mov TCpuIdRegisters[esi].RegEAX, eax
  mov TCpuIdRegisters[esi].RegEBX, ebx
  mov TCpuIdRegisters[esi].RegECX, ecx
  mov TCpuIdRegisters[esi].RegEDX, edx
  pop esi
  pop ebx
end;
{$ELSE 32bit}
asm
{$IFDEF AllowAsmNoframe}
  .noframe
{$ENDIF}
  mov r9, rbx           // preserve rbx


{ According to the 64-bit calling conventions, the arguments are passed in the
  following registers:

  N Windows Unix   Comment
  1 rcx     rdi    first argument, in the GetCPUID function called "AEax" (Cardinal)
  2 rdx     rsi    second argument in the GetCPUID function called "ECx" (Cardinal)
  3 r8      rdx    third argument, the address of the "R" (TCpuIdRegisters) structure

For Windows, we use Microsoft's Win64 "x64 ABI" calling convention.
For Unix (Linux), we use "System V AMD64 ABI" calling convention. }


// load first argument into eax

{$IFDEF unix}
  mov eax, edi
{$ELSE}
  mov eax, ecx
{$ENDIF}

// load second argument into ecx

{$IFDEF unix}
  mov ecx, esi
{$ELSE}
  mov ecx, edx
{$ENDIF}

// load third argument into r10

{$IFDEF unix}
  mov r10, rdx
{$ELSE}
  mov r10, r8
{$ENDIF}


  {Clear the register justs for sure, 32-bit operands in 64-bit mode also clear
  bits 63-32; moreover, CPUID only operates with 32-bit parts of the registers
  even in the 64-bit mode}

  xor ebx, ebx
  xor edx, edx
  cpuid
  {Save registers}
  mov TCpuIdRegisters[r10].RegEAX, eax
  mov TCpuIdRegisters[r10].RegEBX, ebx
  mov TCpuIdRegisters[r10].RegECX, ecx
  mov TCpuIdRegisters[r10].RegEDX, edx
  mov rbx, r9
end;
{$ENDIF 32bit}

{$ENDIF USE_CPUID}

const
// values for the synchronization variables
  cLockByteAvailable = 107;
  cLockByteLocked    = 109;
  cLockByteFinished  = 113;

// the spin-wait loop count for the "test, test-and-set" technique, details are in the comment section at the beginning of the file
  cPauseSpinWaitLoopCount = 5000;
  cUMWaitTime             = 7000000;

{$IFNDEF PurePascal}
{$define UseNormalLoadBeforeAcquireLock}
{$ENDIF}

{$IFDEF SimplifiedInterlockedExchangeByte}

{$IFDEF UseNormalLoadBeforeAcquireLock}
function AcquireLockTryNormalLoadFirst(var Target: TSynchronizationVariable): TSynchronizationVariable; assembler;
asm
{$IFDEF 32bit}
  {On entry:
    eax = Target address}
  mov ecx, eax
  movzx eax, byte ptr [ecx]
  cmp eax, cLockByteAvailable
  jne @Exit
  mov eax, cLockByteLocked
  lock xchg [ecx], al
{$ELSE}
  {$IFNDEF unix}
  {On entry:
    rcx = Target address}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movzx eax, byte ptr [rcx]
  cmp eax, cLockByteAvailable
  jne @Exit
  mov eax, cLockByteLocked
  lock xchg [rcx], al
  {$ELSE}
  {On entry:
    rdi = Target address}
  movzx eax, byte ptr [rdi]
  cmp eax, cLockByteAvailable
  jne @Exit
  mov eax, cLockByteLocked
  lock xchg [rdi], al
  {$ENDIF}
{$ENDIF}
@Exit:
end;
{$ELSE}
function InterlockedExchangeByte(var Target: TSynchronizationVariable; const Value: TSynchronizationVariable): TSynchronizationVariable;
{$IFNDEF ASMVersion}
begin
  Result :=
  {$IFDEF SynchroVarLongint}
  InterlockedExchange
  {$ELSE}
  Windows.InterlockedExchange8
  {$ENDIF}
  (Target, Value);
end;
{$ELSE ASMVersion}
assembler;
asm
{$IFDEF 32bit}
  {On entry:
    eax = Target address,
    dl  = NewVal}
  mov ecx, eax
  movzx eax, dl
  lock xchg [ecx], al
{$ELSE 32bit}
  {$IFNDEF unix}
  {On entry:
    rcx = Target address
    dl = NewVal}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movzx eax, dl
  lock xchg [rcx], al
  {$ELSE}
  {On entry:
    rdi = Target address
    sil = NewVal}
    movzx rax, sil
    lock xchg [rdi], al
  {$ENDIF}
{$ENDIF}
end;
{$ENDIF 32bit}
{$ENDIF ASMVersion}

{$ELSE !SimplifiedInterlockedExchangeByte}

{ The "InterlockedCompareExchangeByte" function is not compiled by default in
the FastMM4-AVX brach. The implementation below is the old functionality
of FastMM4 version 4.992. }

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function InterlockedCompareExchangeByte(const CompareVal, NewVal: TSynchronizationVariable; var Target: TSynchronizationVariable): TSynchronizationVariable; assembler; {$IFDEF fpc64bit}nostackframe;{$ENDIF}
asm
{$IFDEF 32Bit}
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
  {$IFNDEF unix}

{Remove false dependency on remainig bits of the eax (31-8), as eax may come
with these bits trashed, and, as a result, the function will also return these
bits trashed in EAX. So, it may produce faster code by removing dependency
and safer code by cleaning possbile trash}
  movzx eax, al
  movzx edx, dl

{Compare AL with byte ptr [ecx]. If equal, ZF is set and DL is
loaded into byte ptr [ecx]. Else, clear ZF and load byte ptr [ecx] into AL.}
  lock cmpxchg byte ptr [ecx], dl  // cmpxchg also uses AL as an implicit operand

{Clear the registers for safety}
  xor  ecx, ecx
  xor  edx, edx
  {$ELSE unix}
  {Workaround for Kylix compiler bug}
  db $F0, $0F, $B0, $11
  {$ENDIF unix}
{$ELSE 32Bit}

{Microsoft's Win64 "x64 ABI" calling convention.}

  {On entry:
    cl = CompareVal
    dl = NewVal
    r8 = AAddress}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movzx rax, cl {Remove false dependency on remainig bits of the rax}
  xor rcx, rcx
  lock cmpxchg byte ptr [r8], dl  // cmpxchg also uses AL as an implicit operand
  xor rdx, rdx
  xor r8, r8

  {$ELSE unix}

{"System V AMD64 ABI" calling convention - the de facto standard among Unix-like
operating systems. The first four integer or pointer arguments are passed in
registers RDI, RSI, RDX, RCX; return value is stored in RAX and RDX.}

  {On entry:
    dil = CompareVal
    sil = NewVal
    rdx = AAddress}

   movzx rax, dil
   lock cmpxchg byte ptr [rdx], sil  // cmpxchg also uses AL as an implicit operand
   xor rsi, rsi
   xor rdi, rdi
   xor rdx, rdx
  {$ENDIF unix}
{$ENDIF 32Bit}
end;

{$ENDIF SimplifiedInterlockedExchangeByte}

{$IFDEF FullDebugMode}
{$define DebugAcquireLockByte}
{$define DebugReleaseLockByte}
{$ENDIF}

{$IFDEF DEBUG}
{$define DebugAcquireLockByte}
{$define DebugReleaseLockByte}
{$ENDIF}


{$IFNDEF DisablePauseAndSwitchToThread}
{$IFDEF KYLIX}
procedure SwitchToThreadIfSupported;
begin
  sched_yield;
end;
{$ELSE}
{$IFDEF POSIX}
procedure SwitchToThreadIfSupported;
begin
  ThreadSwitch;
end;
{$ELSE}
type
  TSwitchToThread = function: Windows.BOOL; stdcall;
var
  FSwitchToThread: TSwitchToThread;

procedure SwitchToThreadIfSupported;
begin
  if Assigned(FSwitchToThread) then
  begin
    FSwitchToThread;
  end;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF DisablePauseAndSwitchToThread}

{$IFDEF FPC}
function GetMediumBlocksLockedPointer: PByte;
begin
  Result := @MediumBlocksLocked;
end;
{$ENDIF}

{$IFNDEF DisablePauseAndSwitchToThread}
{$IFDEF AuxAsmRoutines}
procedure AcquireSpinLockMediumBlocks; assembler;

{ Note that the assembler version of AcquireSpinLockMediumBlocks is assumed to preserve all volate registers except eax for 32-bit / rax for 64-bit).}

asm
{$IFDEF 64bit}

// 64-bit code for AcquireSpinLockMediumBlocks
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
   push r8
   {$IFDEF FPC}
   call GetMediumBlocksLockedPointer
   mov  r8, rax
   {$ELSE}
   lea  r8, MediumBlocksLocked
   {$ENDIF}

   mov  eax, cLockByteLocked
   cmp  [r8], al
   je   @DidntLockAtFirstAttempt
   lock xchg [r8], al
   cmp  al, cLockByteLocked
   je   @DidntLockAtFirstAttempt
   jmp  @Finish64Bit
@DidntLockAtFirstAttempt:

{$IFDEF EnableWaitPKG}
   {$IFDEF FPC}
   push r8
   call GetFastMMCpuFeaturesB
   pop  r8
   {$ELSE}
   movzx eax, FastMMCpuFeaturesB
   {$ENDIF}
   test al, FastMMCpuFeatureB_WaitPKG
   jz   @NoWaitPKG

   // Start of Umonitor-related section
   mov  eax, cLockByteLocked
   push rcx
   push rdx
   push r9
   push r10
   push r11

   jmp  @FirstLockMonitor
@DidntLockUmonitor:
   push r8
   call SwitchToThreadIfSupported
   pop  r8
   mov  eax, cLockByteLocked
   cmp  [r8], al
   jne  @TryXchgAfterUmonitor

@FirstLockMonitor:
   db   $F3, $41, $0F, $AE, $F0 // umonitor r8
   cmp  [r8], al
   jne  @TryXchgAfterUmonitor

   mov  eax, CUMWaitTime
   xor  rdx, rdx

   //   bit[0] = 0	C0.2	Slower	Larger	Improves performance of the other SMT thread(s) on the same core.
   //   bit[0] = 1	C0.1	Faster	Smaller	NA
   xor  r9, r9

   db   $F2, $41, $0F, $AE, $F1 // umwait, r9d

@TryXchgAfterUmonitor:
   mov  eax, cLockByteLocked
   lock xchg [r8], al
   cmp  al, cLockByteLocked
   je   @DidntLockUmonitor
   // Locked after umonitor
   pop  r11
   pop  r10
   pop  r9
   pop  rdx
   pop  rcx

   jmp  @Finish64Bit
@NoWaitPKG:

   mov  eax, cLockByteLocked
{$ENDIF EnableWaitPKG}
   push r9
   mov  r9d, cPauseSpinWaitLoopCount
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@DidntLockPause64:
@NormalLoadLoopPause64:
   dec  r9
   jz   @SwitchToThreadPause64 // for static branch prediction, jump forward means "unlikely"
   db   $F3, $90 // pause
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  [r8], al
   je   @NormalLoadLoopPause64 // for static branch prediction, jump backwards means "likely"
   lock xchg [r8], al
   cmp  al, cLockByteLocked
   je   @DidntLockPause64
   pop  r9
   jmp	@Finish64Bit
@SwitchToThreadPause64:
   push rcx
   push rdx
   push r8
   push r10
   push r11
   call SwitchToThreadIfSupported
   pop  r11
   pop  r10
   pop  r8
   pop  rdx
   pop  rcx
   mov  eax, cLockByteLocked
   mov  r9d, cPauseSpinWaitLoopCount
   jmp  @NormalLoadLoopPause64
@Finish64Bit:
   pop  r8

{$ELSE 64bit}

// 32-bit code for AcquireSpinLockMediumBlocks

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
   mov  edx, cPauseSpinWaitLoopCount
   mov  eax, cLockByteLocked
   jmp  @FirstComparePause32
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@DidntLockPause32:
@NormalLoadLoopPause32:
   dec  edx
   jz   @SwitchToThreadPause32
   db   $F3, $90 // pause
@FirstComparePause32:
   cmp  [MediumBlocksLocked], al
   je   @NormalLoadLoopPause32
   lock xchg [MediumBlocksLocked], al
   cmp  al, cLockByteLocked
   je   @DidntLockPause32
   jmp	@Finish32Bit
@SwitchToThreadPause32:
   call SwitchToThreadIfSupported
   mov  edx, cPauseSpinWaitLoopCount
   mov  eax, cLockByteLocked
   jmp  @FirstComparePause32
@Finish32Bit:

{$ENDIF}
end;


procedure AcquireSpinLockByte(var Target: TSynchronizationVariable); assembler;
asm
{$IFDEF 64bit}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  {$IFDEF unix}
   mov  rcx, rdi
  {$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
   mov   eax, cLockByteLocked
   cmp  [rcx], al
   je   @DidntLockAtFirstAttempt
   lock xchg [rcx], al
   cmp  al, cLockByteLocked
   je   @DidntLockAtFirstAttempt
   jmp  @Finish
@DidntLockAtFirstAttempt:
{$IFDEF EnableWaitPKG}
   {$IFDEF FPC}
   push rcx
   call GetFastMMCpuFeaturesB
   pop  rcx
   {$ELSE}
   movzx eax, FastMMCpuFeaturesB
   {$ENDIF}
   test al, FastMMCpuFeatureB_WaitPKG
   jz   @NoWaitPKG
   mov  eax, cLockByteLocked
   jmp  @FirstLockMonitor
@DidntLockUmonitor:
   push r8
   push rcx
   call SwitchToThreadIfSupported
   pop  rcx
   pop  r8
   mov  eax, cLockByteLocked
   cmp  [rcx], al
   jne  @TryXchgAfterUmonitor

@FirstLockMonitor:
   db   $F3, $0F, $AE, $F1 // umonitor rcx
   cmp  [rcx], al
   jne  @TryXchgAfterUmonitor

   mov  eax, CUMWaitTime
   xor  rdx, rdx

   //   bit[0] = 0	C0.2	Slower	Larger	Improves performance of the other SMT thread(s) on the same core.
   //   bit[0] = 1	C0.1	Faster	Smaller	NA
   xor  r9, r9

   db   $F2, $41, $0F, $AE, $F1 // umwait, r9d
@TryXchgAfterUmonitor:
   mov  eax, cLockByteLocked
   lock xchg [rcx], al
   cmp  al, cLockByteLocked
   je   @DidntLockUmonitor
   jmp  @Finish
@NoWaitPKG:
   mov  eax, cLockByteLocked
{$ENDIF EnableWaitPKG}

   mov  r9d, cPauseSpinWaitLoopCount
{$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@DidntLock:
@NormalLoadLoop:
   dec  r9
   jz   @SwitchToThread // for static branch prediction, jump forward means "unlikely"
   db   $F3, $90 // pause
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  [rcx], al
   je   @NormalLoadLoop // for static branch prediction, jump backwards means "likely"
   lock xchg [rcx], al
   cmp  al, cLockByteLocked
   je   @DidntLock
   jmp	@Finish
@SwitchToThread:
   push rcx
   call SwitchToThreadIfSupported
   pop  rcx
   mov  r9d, cPauseSpinWaitLoopCount
   mov  eax, cLockByteLocked
   jmp  @NormalLoadLoop
{$ELSE}
   mov  ecx, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@Init:
   mov  edx, cPauseSpinWaitLoopCount
   mov  eax, cLockByteLocked
   jmp  @FirstCompare
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@DidntLock:
@NormalLoadLoop:
   dec  edx
   jz   @SwitchToThread
   db   $F3, $90 // pause
@FirstCompare:
   cmp  [ecx], al
   je   @NormalLoadLoop
   lock xchg [ecx], al
   cmp  al, cLockByteLocked
   je   @DidntLock
   jmp	@Finish
@SwitchToThread:
   push ecx
   call SwitchToThreadIfSupported
   pop  ecx
   jmp  @Init
{$ENDIF}
@Finish:
end;
{$ENDIF AuxAsmRoutines}
{$ENDIF DisablePauseAndSwitchToThread}


function AcquireLockByte(var Target: TSynchronizationVariable): Boolean;
  {$IFNDEF DEBUG}{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}{$ENDIF}
var
  R: Byte;
begin
  {$IFDEF SimplifiedInterlockedExchangeByte}
    R :=
    {$IFDEF UseNormalLoadBeforeAcquireLock}
    AcquireLockTryNormalLoadFirst(Target);
    {$ELSE}
    InterlockedExchangeByte(Target, cLockByteLocked);
    {$ENDIF}
  {$ELSE}
    R := InterlockedCompareExchangeByte(cLockByteAvailable, cLockByteLocked, Target);
  {$ENDIF}
  {$IFDEF DebugAcquireLockByte}
    case R of
      cLockByteAvailable: Result := True;
      cLockByteLocked: Result := False;
      else
        begin
          Result := False;
      {$IFNDEF SystemRunError}
          System.Error(reInvalidOp);
      {$ELSE}
          System.RunError(reInvalidOp);
      {$ENDIF}
        end;
    end;
  {$ELSE}
    Result := R = CLockByteAvailable;
  {$ENDIF}
end;


{ Look for "using normal memory store" in the comment section
at the beginning of the file for the discussion on releasing locks on data
structures. You can also define the "InterlockedRelease" option in the
FastMM4Options.inc file to get the old behaviour of the origina FastMM4. }

procedure ReleaseLockByte(var Target: TSynchronizationVariable);

  {$IFNDEF DEBUG}{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}{$ENDIF}

{$IFDEF DebugReleaseLockByte}
var
  R: Byte;
{$ENDIF}
begin
  {$IFDEF InterlockedRelease}
    {$IFDEF SimplifiedInterlockedExchangeByte}
      {$IFDEF DebugReleaseLockByte}
      R :=
      {$ENDIF}
        InterlockedExchangeByte(Target, cLockByteAvailable);
    {$ELSE}
      {$IFDEF DebugReleaseLockByte}
      R :=
      {$ENDIF}
        InterlockedCompareExchangeByte(cLockByteLocked, cLockByteAvailable, Target);
    {$ENDIF}
  {$ELSE}
     {$IFDEF DebugReleaseLockByte}
     R := Target;
     {$ENDIF}
     Target := CLockByteAvailable;
  {$ENDIF}
    {$IFDEF DebugReleaseLockByte}
    if R <> cLockByteLocked  then
    begin
      {$IFNDEF SystemRunError}
        System.Error(reInvalidOp);
      {$ELSE}
        System.RunError(reInvalidOp);
      {$ENDIF}
    end;
    {$ENDIF}
end;



{$IFDEF MACOS_OR_KYLIX}

function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
var
  Len: Cardinal;
begin
  Result := Dest;
  Len := StrLen(Source);
  if Len > MaxLen then
    Len := MaxLen;
  Move(Source^, Dest^, Len * SizeOf(AnsiChar));
  Dest[Len] := #0;
end;

function GetModuleFileName(Module: HMODULE; Buffer: PAnsiChar; BufLen: Integer): Integer;
const
  CUnknown = 'unknown'#0;
var
  LUnknown: array[0..Length(CUnknown)-1] of AnsiChar = CUnknown;
begin
  if FastMMIsInstalled then
  begin
    Result := System.GetModuleFileName(Module, tmp, BufLen);
    StrLCopy(Buffer, PAnsiChar(AnsiString(tmp)), BufLen);
  end
  else
  begin
    Result := Length(CUnknown);
    StrLCopy(Buffer, PAnsiChar(CUnknown), Result + 1);
  end;
end;

const
  INVALID_HANDLE_VALUE = THandle(-1);

function FileCreate(const FileName: string): THandle;
begin
  Result := THandle({$IFDEF MACOS}__open{$ELSE}open{$ENDIF}(
    PAnsiChar(UTF8String(FileName)), O_RDWR or O_CREAT or O_TRUNC or O_EXCL, FileAccessRights));
end;

{$ENDIF}

{$IFDEF FPC}
function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
var
  Len: Cardinal;
begin
  Result := Dest;
  Len := StrLen(Source);
  if Len > MaxLen then
    Len := MaxLen;
  Move(Source^, Dest^, Len * SizeOf(AnsiChar));
  Dest[Len] := #0;
end;

function GetModuleFileName(Module: HMODULE; Buffer: PAnsiChar; BufLen: Integer): Integer;
const
  CUnknown = 'unknown'#0;
var
  LUnknown: array[0..Length(CUnknown)-1] of AnsiChar = CUnknown;
begin
  Result := Length(CUnknown);
  if Result > BufLen then Result := BufLen;
  StrLCopy(Buffer, @LUnknown, Result);
end;

{$IFDEF POSIX}
const
  INVALID_HANDLE_VALUE = THandle(-1);
  FileAcc = (S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH);

function FileCreate(const FileName: string): THandle;
begin
  Result := THandle(fpopen(PAnsiChar(UTF8String(FileName)), O_RDWR or O_CREAT or O_TRUNC or O_EXCL, FileAcc));
end;
{$ENDIF}

{$ENDIF}

{Writes the module filename to the specified buffer and returns the number of
 characters written.}
function AppendModuleFileName(ABuffer: PAnsiChar; ABufferLengthChars: Integer {including the terminating null character}): Integer;
var
  LModuleHandle: HModule;
begin
  {Get the module handle}
{$IFNDEF borlndmmdll}
  if IsLibrary then
    LModuleHandle := HInstance
  else
{$ENDIF}
    LModuleHandle := 0;
  {Get the module name}
{$IFNDEF POSIX}
  Result := GetModuleFileNameA(LModuleHandle, ABuffer, ABufferLengthChars);
{$ELSE}
  Result := GetModuleFileName(LModuleHandle, ABuffer, ABufferLengthChars);
{$ENDIF}
end;

{Copies the name of the module followed by the given string to the buffer,
 returning the pointer following the buffer.}
function AppendStringToModuleName(AString, ABuffer: PAnsiChar; AStringLength, ABufferLength: Cardinal): PAnsiChar;
const
  CNumReservedCharsInModuleName = 5; {reserve some extra characters for colon and space}
var
  LModuleNameLength: Cardinal;
  LCopyStart: PAnsiChar;
  LStringLength, LBufferLength: Cardinal;
  LString, LBuffer: PAnsiChar;
begin
  LString := AString;
  LStringLength := AStringLength;
  LBuffer := ABuffer;
  LBufferLength := ABufferLength;
  {Get the name of the application}
  LModuleNameLength := AppendModuleFileName(LBuffer, LBufferLength);
  {Replace the last few characters}
  if (LModuleNameLength > 0) and (LModuleNameLength + CNumReservedCharsInModuleName < LBufferLength) then
  begin
    {Find the last backslash}
    LCopyStart := PAnsiChar(PByte(LBuffer) + LModuleNameLength - 1);
    LModuleNameLength := 0;
    while (UIntPtr(LCopyStart) >= UIntPtr(LBuffer))
      and (LCopyStart^ <> '\') do
    begin
      Inc(LModuleNameLength);
      Dec(LCopyStart);
    end;
    {Copy the name to the start of the buffer}
    Inc(LCopyStart);
    System.Move(LCopyStart^, LBuffer^, LModuleNameLength*SizeOf(LCopyStart[0]));
    Inc(LBuffer, LModuleNameLength);
    if LBufferLength >= LModuleNameLength then
    begin
      Dec(LBufferLength, LModuleNameLength);
      if LBufferLength > 0 then
      begin
        LBuffer^ := ':';
        Inc(LBuffer);
        Dec(LBufferLength);
        if LBufferLength > 0 then
        begin
          LBuffer^ := ' ';
          Inc(LBuffer);
          Dec(LBufferLength);
        end;
      end;
    end;
  end;
  {Append the string}
  while (LString^ <> #0) and (LBufferLength > 0) and (LStringLength > 0) do
  begin
    LBuffer^ := LString^;
    Dec(LBufferLength);
    Inc(LBuffer);
    {Next char}
    Inc(LString);
    Dec(LStringLength);
  end;
  LBuffer^ := #0;
  Result := LBuffer;
end;

{----------------------------Faster Move Procedures----------------------------}

{Fixed size move operations ignore the size parameter. All moves are assumed to
 be non-overlapping.}

{$IFDEF UseCustomFixedSizeMoveRoutines}

{$IFDEF 64bit}

procedure Move24Reg64(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  mov rax, [rcx + 0*8]
  mov r8,  [rcx + 1*8]
  mov r9,  [rcx + 2*8]
  mov [rdx + 0*8], rax
  mov [rdx + 1*8], r8
  mov [rdx + 2*8], r9
  {$ELSE}
  mov rax, [rdi + 0*8]
  mov rdx, [rdi + 1*8]
  mov rcx,  [rdi + 2*8]
  mov [rsi + 0*8], rax
  mov [rsi + 1*8], rdx
  mov [rsi + 2*8], rcx
  {$ENDIF}
end;

procedure Move32Reg64(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  mov rax, [rcx + 0*8]
  mov r8,  [rcx + 1*8]
  mov r9,  [rcx + 2*8]
  mov r10, [rcx + 3*8]
  mov [rdx + 0*8], rax
  mov [rdx + 1*8], r8
  mov [rdx + 2*8], r9
  mov [rdx + 3*8], r10
  {$ELSE}
  mov rax, [rdi + 0*8]
  mov rdx, [rdi + 1*8]
  mov rcx, [rdi + 2*8]
  mov r8,  [rdi + 3*8]
  mov [rsi + 0*8], rax
  mov [rsi + 1*8], rdx
  mov [rsi + 2*8], rcx
  mov [rsi + 3*8], r8
  {$ENDIF}
end;


procedure Move40Reg64(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  mov rax, [rcx + 0*8]
  mov r8,  [rcx + 1*8]
  mov r9,  [rcx + 2*8]
  mov r10, [rcx + 3*8]
  mov r11, [rcx + 4*8]
  mov [rdx + 0*8], rax
  mov [rdx + 1*8], r8
  mov [rdx + 2*8], r9
  mov [rdx + 3*8], r10
  mov [rdx + 4*8], r11
  {$ELSE}
  mov rax, [rdi + 0*8]
  mov rdx, [rdi + 1*8]
  mov rcx, [rdi + 2*8]
  mov r8,  [rdi + 3*8]
  mov r9,  [rdi + 4*8]
  mov [rsi + 0*8], rax
  mov [rsi + 1*8], rdx
  mov [rsi + 2*8], rcx
  mov [rsi + 3*8], r8
  mov [rsi + 4*8], r9
  {$ENDIF}
end;


procedure Move48Reg64(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  mov rax, [rcx + 0*8]
  mov r8,  [rcx + 1*8]
  mov r9,  [rcx + 2*8]
  mov r10, [rcx + 3*8]
  mov [rdx + 0*8], rax
  mov [rdx + 1*8], r8
  mov [rdx + 2*8], r9
  mov [rdx + 3*8], r10
  mov rax, [rcx + 4*8]
  mov r8,  [rcx + 5*8]
  mov [rdx + 4*8], rax
  mov [rdx + 5*8], r8
  {$ELSE}
  mov rax, [rdi + 0*8]
  mov rdx, [rdi + 1*8]
  mov rcx, [rdi + 2*8]
  mov r8,  [rdi + 3*8]
  mov [rsi + 0*8], rax
  mov [rsi + 1*8], rdx
  mov [rsi + 2*8], rcx
  mov [rsi + 3*8], r8
  mov rax, [rdi + 4*8]
  mov rdx, [rdi + 5*8]
  mov [rsi + 4*8], rax
  mov [rsi + 5*8], rdx
  {$ENDIF}
end;


procedure Move56Reg64(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  mov rax, [rcx + 0*8]
  mov r8,  [rcx + 1*8]
  mov r9,  [rcx + 2*8]
  mov r10, [rcx + 3*8]
  mov [rdx + 0*8], rax
  mov [rdx + 1*8], r8
  mov [rdx + 2*8], r9
  mov [rdx + 3*8], r10
  mov rax, [rcx + 4*8]
  mov r8,  [rcx + 5*8]
  mov r9,  [rcx + 6*8]
  mov [rdx + 4*8], rax
  mov [rdx + 5*8], r8
  mov [rdx + 6*8], r9
  {$ELSE}
  mov rax, [rdi + 0*8]
  mov rdx, [rdi + 1*8]
  mov rcx, [rdi + 2*8]
  mov r8,  [rdi + 3*8]
  mov [rsi + 0*8], rax
  mov [rsi + 1*8], rdx
  mov [rsi + 2*8], rcx
  mov [rsi + 3*8], r8
  mov rax, [rdi + 4*8]
  mov rdx, [rdi + 5*8]
  mov rcx, [rdi + 6*8]
  mov [rsi + 4*8], rax
  mov [rsi + 5*8], rdx
  mov [rsi + 6*8], rcx
  {$ENDIF}
end;

{$ENDIF 64bit}

{$IFDEF EnableAVX}

{$IFNDEF DisableAVX1}

{----------------------------AVX1 Move Procedures----------------------------}

procedure Move24AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77      // vzeroupper

  {$IFNDEF unix}
  db $C5, $F9, $6F, $01 // vmovdqa xmm0, xmmword ptr[rcx]
                           mov r8, [rcx + 16]
  db $C5, $F9, $7F, $02 // vmovdqa xmmword ptr[rdx], xmm0
                           mov [rdx + 16], r8
  {$ELSE}
  db $C5, $F9, $6F, $07 // vmovdqa xmm0, xmmword ptr[rdi]
                           mov rdx, [rdi + 16]
  db $C5, $F9, $7F, $06 // vmovdqa xmmword ptr[rsi], xmm0
                           mov [rsi + 16], rdx
  {$ENDIF}
  db $C5, $F8, $57, $C0 // vxorps xmm0,xmm0,xmm0
  db $C5, $F8, $77      // vzeroupper
end;


procedure Move56AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77           // vzeroupper

  {$IFNDEF unix}
  db $C5, $FD, $6F, $01      // vmovdqa ymm0, ymmword ptr [rcx]
  db $C5, $F9, $6F, $49, $20 // vmovdqa xmm1, xmmword ptr [rcx+20h]
                                mov r8, [rcx + 48]
  db $C5, $FD, $7F, $02      // vmovdqa ymmword ptr [rdx], ymm0
  db $C5, $F9, $7F, $4A, $20 // vmovdqa xmmword ptr [rdx+20h], xmm1
                                mov [rdx + 48], r8
  {$ELSE}
  db $C5, $FD, $6F, $07      // vmovdqa ymm0, ymmword ptr [rdi]
  db $C5, $F9, $6F, $4F, $20 // vmovdqa xmm1, xmmword ptr [rdi+20h]
                                mov rdx, [rdi + 48]
  db $C5, $FD, $7F, $06      // vmovdqa ymmword ptr [rsi], ymm0
  db $C5, $F9, $7F, $4E, $20 // vmovdqa xmmword ptr [rsi+20h], xmm1
                                mov [rsi + 48], rdx
  {$ENDIF}
  db $C5, $FC, $57, $C0      // vxorps ymm0, ymm0, ymm0
  db $C5, $F0, $57, $C9      // vxorps xmm1, xmm1, xmm1
  db $C5, $F8, $77           // vzeroupper
end;

procedure Move88AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77           // vzeroupper

  {$IFNDEF unix}
  db $C5, $FD, $6F, $01      // vmovdqa ymm0, ymmword ptr [rcx]
  db $C5, $FD, $6F, $49, $20 // vmovdqa ymm1, ymmword ptr [rcx+20h]
  db $C5, $F9, $6F, $51, $40 // vmovdqa xmm2, xmmword ptr [rcx+40h]
                                mov rcx, [rcx + 50h]
  db $C5, $FD, $7F, $02      // vmovdqa ymmword ptr [rdx], ymm0
  db $C5, $FD, $7F, $4A, $20 // vmovdqa ymmword ptr [rdx+20h], ymm1
  db $C5, $F9, $7F, $52, $40 // vmovdqa xmmword ptr [rdx+40h], xmm2
                                mov [rdx + 50h], rcx
  {$ELSE}
  db $C5, $FD, $6F, $07      // vmovdqa ymm0, ymmword ptr [rdi]
  db $C5, $FD, $6F, $4F, $20 // vmovdqa ymm1, ymmword ptr [rdi+20h]
  db $C5, $F9, $6F, $57, $40 // vmovdqa xmm2, xmmword ptr [rdi+40h]
                                mov rdi, [rdi + 50h]
  db $C5, $FD, $7F, $06      // vmovdqa ymmword ptr [rsi], ymm0
  db $C5, $FD, $7F, $4E, $20 // vmovdqa ymmword ptr [rsi+20h], ymm1
  db $C5, $F9, $7F, $56, $40 // vmovdqa xmmword ptr [rsi+40h], xmm2
                                mov [rsi + 50h], rdi
  {$ENDIF}
  db $C5, $FC, $57, $C0      // vxorps ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9      // vxorps ymm1,ymm1,ymm1
  db $C5, $E8, $57, $D2      // vxorps xmm2,xmm2,xmm2
  db $C5, $F8, $77           // vzeroupper
end;

procedure Move120AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77           // vzeroupper

  {$IFNDEF unix}

{We are using that many ymm registers (not just two of them in a sequence),
because our routines allow overlapped moves (although it is not neede for
FastMM4 realloc). However, there is no speed increase in using more than
two registers, because we have just two load units and just one store unit
on most CPUs}

  db $C5, $FD, $6F, $01      // vmovdqa ymm0, ymmword ptr [rcx]
  db $C5, $FD, $6F, $49, $20 // vmovdqa ymm1, ymmword ptr [rcx+20h]
  db $C5, $FD, $6F, $51, $40 // vmovdqa ymm2, ymmword ptr [rcx+40h]
  db $C5, $F9, $6F, $59, $60 // vmovdqa xmm3, xmmword ptr [rcx+60h]
                                mov rcx, [rcx + 70h]
  db $C5, $FD, $7F, $02      // vmovdqa ymmword ptr [rdx], ymm0
  db $C5, $FD, $7F, $4A, $20 // vmovdqa ymmword ptr [rdx+20h], ymm1
  db $C5, $FD, $7F, $52, $40 // vmovdqa ymmword ptr [rdx+40h], ymm2
  db $C5, $F9, $7F, $5A, $60 // vmovdqa xmmword ptr [rdx+60h], xmm3
                                mov [rdx + 70h], rcx
  {$ELSE}
  db $C5, $FD, $6F, $07      // vmovdqa ymm0, ymmword ptr [rdi]
  db $C5, $FD, $6F, $4F, $20 // vmovdqa ymm1, ymmword ptr [rdi+20h]
  db $C5, $FD, $6F, $57, $40 // vmovdqa ymm2, ymmword ptr [rdi+40h]
  db $C5, $F9, $6F, $5F, $60 // vmovdqa xmm3, xmmword ptr [rdi+60h]
                                mov rdi, [rdi + 70h]
  db $C5, $FD, $7F, $06      // vmovdqa ymmword ptr [rsi], ymm0
  db $C5, $FD, $7F, $4E, $20 // vmovdqa ymmword ptr [rsi+20h], ymm1
  db $C5, $FD, $7F, $56, $40 // vmovdqa ymmword ptr [rsi+40h], ymm2
  db $C5, $F9, $7F, $5E, $60 // vmovdqa ymmword ptr [rsi+60h], xmm3
                                mov [rsi + 70h], rdi
  {$ENDIF}
  db $C5, $FC, $57, $C0      // vxorps ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9      // vxorps ymm1,ymm1,ymm1
  db $C5, $EC, $57, $D2      // vxorps ymm2,ymm2,ymm2
  db $C5, $E0, $57, $DB      // vxorps xmm3,xmm3,xmm3
  db $C5, $F8, $77           // vzeroupper
end;

procedure Move152AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77           // vzeroupper

  {$IFNDEF unix}

{We add to the source and destination registers to allow all future offsets
be in range -127..+127 to have 1-byte offset encoded in the opcodes, not 4
bytes, so the opcode will be shorter by 4 bytes, the overall code will be
shorter, and, as a result, faster, inspite of the sacrifice that we make
at the start of the routine. The sacrifice is small - maybe just 1 cycle, or
less, by "add rcx", but it pays up later}

                                add rcx, 60h
                                add rdx, 60h
  db $C5, $FD, $6F, $41, $A0 // vmovdqa ymm0, [rcx-60h]
  db $C5, $FD, $6F, $49, $C0 // vmovdqa ymm1, [rcx-40h]
  db $C5, $FD, $6F, $51, $E0 // vmovdqa ymm2, [rcx-20h]
  db $C5, $FD, $6F, $19      // vmovdqa ymm3, [rcx]
  db $C5, $F9, $6F, $61, $20 // vmovdqa xmm4, [rcx+20h]
                                mov rcx, [rcx+30h]
  db $C5, $FD, $7F, $42, $A0 // vmovdqa [rdx-60h], ymm0
  db $C5, $FD, $7F, $4A, $C0 // vmovdqa [rdx-40h], ymm1
  db $C5, $FD, $7F, $52, $E0 // vmovdqa [rdx-20h], ymm2
  db $C5, $FD, $7F, $1A      // vmovdqa [rdx],     ymm3
  db $C5, $F9, $7F, $62, $20 // vmovdqa [rdx+20h], xmm4
                                mov [rdx+30h],rcx
  {$ELSE}
                                add rdi, 60h
                                add rsi, 60h
  db $C5, $FD, $6F, $47, $A0 // vmovdqa ymm0, [rdi-60h]
  db $C5, $FD, $6F, $4F, $C0 // vmovdqa ymm1, [rdi-40h]
  db $C5, $FD, $6F, $57, $E0 // vmovdqa ymm2, [rdi-20h]
  db $C5, $FD, $6F, $1F      // vmovdqa ymm3, [rdi]
  db $C5, $F9, $6F, $67, $20 // vmovdqa xmm4, [rdi+20h]
                                mov rdi, [rdi+30h]
  db $C5, $FD, $7F, $46, $A0 // vmovdqa [rsi-60h], ymm0
  db $C5, $FD, $7F, $4E, $C0 // vmovdqa [rsi-40h], ymm1
  db $C5, $FD, $7F, $56, $E0 // vmovdqa [rsi-20h], ymm2
  db $C5, $FD, $7F, $1E      // vmovdqa [rsi],     ymm3
  db $C5, $F9, $7F, $66, $20 // vmovdqa [rsi+20h], xmm4
                                mov [rsi+30h], rdi
  {$ENDIF}
{See the comment at Move120AVX1 on why we are using that many ymm registers}
  db $C5, $FC, $57, $C0      // vxorps ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9      // vxorps ymm1,ymm1,ymm1
  db $C5, $EC, $57, $D2      // vxorps ymm2,ymm2,ymm2
  db $C5, $E4, $57, $DB      // vxorps ymm3,ymm3,ymm3
  db $C5, $D8, $57, $E4      // vxorps xmm4,xmm4,xmm4
  db $C5, $F8, $77           // vzeroupper
end;

procedure Move184AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77           // vzeroupper

  {$IFNDEF unix}

{We add to the source and destination registers to allow all future offsets
be in range -127..+127, see explanation at the Move152AVX1 routine}

                                add rcx, 60h
                                add rdx, 60h
  db $C5, $FD, $6F, $41, $A0 // vmovdqa ymm0, [rcx-60h]
  db $C5, $FD, $6F, $49, $C0 // vmovdqa ymm1, [rcx-40h]
  db $C5, $FD, $6F, $51, $E0 // vmovdqa ymm2, [rcx-20h]
  db $C5, $FD, $6F, $19      // vmovdqa ymm3, [rcx]
  db $C5, $FD, $6F, $61, $20 // vmovdqa ymm4, [rcx+20h]
  db $C5, $F9, $6F, $69, $40 // vmovdqa xmm5, [rcx+40h]
                                mov rcx, [rcx+50h]
  db $C5, $FD, $7F, $42, $A0 // vmovdqa [rdx-60h], ymm0
  db $C5, $FD, $7F, $4A, $C0 // vmovdqa [rdx-40h], ymm1
  db $C5, $FD, $7F, $52, $E0 // vmovdqa [rdx-20h], ymm2
  db $C5, $FD, $7F, $1A      // vmovdqa [rdx],     ymm3
  db $C5, $FD, $7F, $62, $20 // vmovdqa [rdx+20h], ymm4
  db $C5, $F9, $7F, $6A, $40 // vmovdqa [rdx+40h], xmm5
                                mov [rdx+50h], rcx
  {$ELSE}
                                add rdi, 60h
                                add rsi, 60h
  db $C5, $FD, $6F, $47, $A0 // vmovdqa ymm0, [rdi-60h]
  db $C5, $FD, $6F, $4F, $C0 // vmovdqa ymm1, [rdi-40h]
  db $C5, $FD, $6F, $57, $E0 // vmovdqa ymm2, [rdi-20h]
  db $C5, $FD, $6F, $1F      // vmovdqa ymm3, [rdi]
  db $C5, $FD, $6F, $67, $20 // vmovdqa ymm4, [rdi+20h]
  db $C5, $F9, $6F, $6F, $40 // vmovdqa xmm5, [rdi+40h]
                                mov rdi, [rdi+50h]
  db $C5, $FD, $7F, $46, $A0 // vmovdqa [rsi-60h], ymm0
  db $C5, $FD, $7F, $4E, $C0 // vmovdqa [rsi-40h], ymm1
  db $C5, $FD, $7F, $56, $E0 // vmovdqa [rsi-20h], ymm2
  db $C5, $FD, $7F, $1E      // vmovdqa [rsi],     ymm3
  db $C5, $FD, $7F, $66, $20 // vmovdqa [rsi+20h], ymm4
  db $C5, $F9, $7F, $6E, $40 // vmovdqa [rsi+40h], xmm5
                                mov [rsi+50h], rdi
  {$ENDIF}
  db $C5, $FC, $57, $C0      // vxorps ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9      // vxorps ymm1,ymm1,ymm1
  db $C5, $EC, $57, $D2      // vxorps ymm2,ymm2,ymm2
  db $C5, $E4, $57, $DB      // vxorps ymm3,ymm3,ymm3
  db $C5, $DC, $57, $E4      // vxorps ymm4,ymm4,ymm4
  db $C5, $D0, $57, $ED      // vxorps xmm5,xmm5,xmm5
  db $C5, $F8, $77           // vzeroupper
end;

procedure Move216AVX1(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

  db $C5, $F8, $77           // vzeroupper

  {$IFNDEF unix}
                                add rcx, 60h
                                add rdx, 60h
  db $C5, $FD, $6F, $41, $A0 // vmovdqa ymm0, [rcx-60h]
  db $C5, $FD, $6F, $49, $C0 // vmovdqa ymm1, [rcx-40h]
  db $C5, $FD, $6F, $51, $E0 // vmovdqa ymm2, [rcx-20h]
  db $C5, $FD, $6F, $19      // vmovdqa ymm3, [rcx]
  db $C5, $FD, $6F, $61, $20 // vmovdqa ymm4, [rcx+20h]
  db $C5, $FD, $6F, $69, $40 // vmovdqa ymm5, [rcx+40h]

{The xmm6/ymm6 register is nonvolatile, according to
Microsoft's x64 calling convention, used for Win64,
denoted "The x64 Application Binary Interface (ABI)", or, briefly, "x64 ABI".
Since we cannot use xmm6, we use general-purpose
64-bit registers to copy remaining data.

According to Microsoft, "The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 nonvolatile. They must be saved and restored by a function that uses them"

We are using that many ymm registers, not just two of them in a sequence,
because our routines allow overlapped moves (although it is not needed for
FastMM4 realloc) - see the comment at Move120AVX1 on why we are using that
many ymm registers.}


                                mov r9, [rcx+60h]
                                mov r10, [rcx+68h]
                                mov r11, [rcx+70h]
  db $C5, $FD, $7F, $42, $A0 // vmovdqa [rdx-60h], ymm0
  db $C5, $FD, $7F, $4A, $C0 // vmovdqa [rdx-40h], ymm1
  db $C5, $FD, $7F, $52, $E0 // vmovdqa [rdx-20h], ymm2
  db $C5, $FD, $7F, $1A      // vmovdqa [rdx],     ymm3
  db $C5, $FD, $7F, $62, $20 // vmovdqa [rdx+20h], ymm4
  db $C5, $FD, $7F, $6A, $40 // vmovdqa [rdx+40h], ymm5
                                mov [rdx+60h], r9
                                mov [rdx+68h], r10
                                mov [rdx+70h], r11
  {$ELSE}
                                add rdi, 60h
                                add rsi, 60h
  db $C5, $FD, $6F, $47, $A0 // vmovdqa ymm0, [rdi-60h]
  db $C5, $FD, $6F, $4F, $C0 // vmovdqa ymm1, [rdi-40h]
  db $C5, $FD, $6F, $57, $E0 // vmovdqa ymm2, [rdi-20h]
  db $C5, $FD, $6F, $1F      // vmovdqa ymm3, [rdi]
  db $C5, $FD, $6F, $67, $20 // vmovdqa ymm4, [rdi+20h]
  db $C5, $FD, $6F, $6F, $40 // vmovdqa ymm5, [rdi+40h]

{Although, under unix, we can use xmm6(ymm6) and xmm7 (ymm7), here we mimic
the Win64 code, thus use up to ymm5, and use general-purpose 64-bit registers
to copy remaining data - 24 bytes, which is still smaller than the full ymm
register (32 bytes)}
                                mov r9,  [rdi+60h]
                                mov r10, [rdi+68h]
                                mov r11, [rdi+70h]
  db $C5, $FD, $7F, $46, $A0 // vmovdqa [rsi-60h], ymm0
  db $C5, $FD, $7F, $4E, $C0 // vmovdqa [rsi-40h], ymm1
  db $C5, $FD, $7F, $56, $E0 // vmovdqa [rsi-20h], ymm2
  db $C5, $FD, $7F, $1E      // vmovdqa [rsi],     ymm3
  db $C5, $FD, $7F, $66, $20 // vmovdqa [rsi+20h], ymm4
  db $C5, $FD, $7F, $6E, $40 // vmovdqa [rsi+40h], ymm5
                                mov [rsi+60h], r9
                                mov [rsi+68h], r10
                                mov [rsi+70h], r11
  {$ENDIF}
  db $C5, $FC, $57, $C0      // vxorps ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9      // vxorps ymm1,ymm1,ymm1
  db $C5, $EC, $57, $D2      // vxorps ymm2,ymm2,ymm2
  db $C5, $E4, $57, $DB      // vxorps ymm3,ymm3,ymm3
  db $C5, $DC, $57, $E4      // vxorps ymm4,ymm4,ymm4
  db $C5, $D4, $57, $ED      // vxorps ymm5,ymm5,ymm5
  db $C5, $F8, $77           // vzeroupper
end;
{$ENDIF DisableAVX1}


{$IFNDEF DisableAVX2}

{----------------------------AVX2 Move Procedures----------------------------}

procedure Move24AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  db $C5, $F9, $6F, $01 // vmovdqa xmm0, xmmword ptr[rcx]
  mov r8, [rcx + 16]
  db $C5, $F9, $7F, $02 // vmovdqa xmmword ptr[rdx], xmm0
  mov [rdx + 16], r8
  {$ELSE}
  db $C5, $F9, $6F, $07 // vmovdqa xmm0, xmmword ptr[rdi]
  mov rdx, [rdi + 16]
  db $C5, $F9, $7F, $06 // vmovdqa xmmword ptr[rsi], xmm0
  mov [rsi + 16], rdx
  {$ENDIF}
  db $C5, $F9, $EF, $C0 // vpxor xmm0,xmm0,xmm0
end;

procedure Move56AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  db $C5, $FD, $6F, $01      // vmovdqa ymm0, ymmword ptr [rcx]
  db $C5, $F9, $6F, $49, $20 // vmovdqa xmm1, xmmword ptr [rcx+20h]
  mov r8, [rcx + 48]
  db $C5, $FD, $7F, $02      // vmovdqa ymmword ptr [rdx], ymm0
  db $C5, $F9, $7F, $4A, $20 // vmovdqa xmmword ptr [rdx+20h], xmm1
  mov [rdx + 48], r8
  {$ELSE}
  db $C5, $FD, $6F, $07      // vmovdqa ymm0, ymmword ptr [rdi]
  db $C5, $F9, $6F, $4F, $20 // vmovdqa xmm1, xmmword ptr [rdi+20h]
  mov rdx, [rdi + 48]
  db $C5, $FD, $7F, $06      // vmovdqa ymmword ptr [rsi], ymm0
  db $C5, $F9, $7F, $4E, $20 // vmovdqa xmmword ptr [rsi+20h], xmm1
  mov [rsi + 48], rdx
  {$ENDIF}
  db $C5, $FD, $EF, $C0      // vpxor ymm0, ymm0, ymm0
  db $C5, $F1, $EF, $C9      // vpxor xmm1, xmm1, xmm1
end;

procedure Move88AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  db $C5, $FD, $6F, $01      // vmovdqa ymm0, ymmword ptr [rcx]
  db $C5, $FD, $6F, $49, $20 // vmovdqa ymm1, ymmword ptr [rcx+20h]
  db $C5, $F9, $6F, $51, $40 // vmovdqa xmm2, xmmword ptr [rcx+40h]
  mov rcx, [rcx + 50h]
  db $C5, $FD, $7F, $02      // vmovdqa ymmword ptr [rdx], ymm0
  db $C5, $FD, $7F, $4A, $20 // vmovdqa ymmword ptr [rdx+20h], ymm1
  db $C5, $F9, $7F, $52, $40 // vmovdqa xmmword ptr [rdx+40h], xmm2
  mov [rdx + 50h], rcx
  {$ELSE}
  db $C5, $FD, $6F, $07      // vmovdqa ymm0, ymmword ptr [rdi]
  db $C5, $FD, $6F, $4F, $20 // vmovdqa ymm1, ymmword ptr [rdi+20h]
  db $C5, $F9, $6F, $57, $40 // vmovdqa xmm2, xmmword ptr [rdi+40h]
  mov rdi, [rdi + 50h]
  db $C5, $FD, $7F, $06      // vmovdqa ymmword ptr [rsi], ymm0
  db $C5, $FD, $7F, $4E, $20 // vmovdqa ymmword ptr [rsi+20h], ymm1
  db $C5, $F9, $7F, $56, $40 // vmovdqa xmmword ptr [rsi+40h], xmm2
  mov [rsi + 50h], rdi
  {$ENDIF}
  db $C5, $FD, $EF, $C0      // vpxor ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9      // vpxor ymm1,ymm1,ymm1
  db $C5, $E9, $EF, $D2      // vpxor xmm2,xmm2,xmm2
end;

procedure Move120AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  db $C5, $FD, $6F, $01      // vmovdqa ymm0, ymmword ptr [rcx]
  db $C5, $FD, $6F, $49, $20 // vmovdqa ymm1, ymmword ptr [rcx+20h]
  db $C5, $FD, $6F, $51, $40 // vmovdqa ymm2, ymmword ptr [rcx+40h]
  db $C5, $F9, $6F, $59, $60 // vmovdqa xmm3, xmmword ptr [rcx+60h]
  mov rcx, [rcx + 70h]
  db $C5, $FD, $7F, $02      // vmovdqa ymmword ptr [rdx], ymm0
  db $C5, $FD, $7F, $4A, $20 // vmovdqa ymmword ptr [rdx+20h], ymm1
  db $C5, $FD, $7F, $52, $40 // vmovdqa ymmword ptr [rdx+40h], ymm2
  db $C5, $F9, $7F, $5A, $60 // vmovdqa xmmword ptr [rdx+60h], xmm3
  mov [rdx + 70h], rcx
  {$ELSE}
  db $C5, $FD, $6F, $07      // vmovdqa ymm0, ymmword ptr [rdi]
  db $C5, $FD, $6F, $4F, $20 // vmovdqa ymm1, ymmword ptr [rdi+20h]
  db $C5, $FD, $6F, $57, $40 // vmovdqa ymm2, ymmword ptr [rdi+40h]
  db $C5, $F9, $6F, $5F, $60 // vmovdqa xmm3, xmmword ptr [rdi+60h]
  mov rdi, [rdi + 70h]
  db $C5, $FD, $7F, $06      // vmovdqa ymmword ptr [rsi], ymm0
  db $C5, $FD, $7F, $4E, $20 // vmovdqa ymmword ptr [rsi+20h], ymm1
  db $C5, $FD, $7F, $56, $40 // vmovdqa ymmword ptr [rsi+40h], ymm2
  db $C5, $F9, $7F, $5E, $60 // vmovdqa ymmword ptr [rsi+60h], xmm3
  mov [rsi + 70h], rdi
  {$ENDIF}
  db $C5, $FD, $EF, $C0      // vpxor ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9      // vpxor ymm1,ymm1,ymm1
  db $C5, $ED, $EF, $D2      // vpxor ymm2,ymm2,ymm2
  db $C5, $E1, $EF, $DB      // vpxor xmm3,xmm3,xmm3
end;

procedure Move152AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
                                add rcx, 60h
                                add rdx, 60h
  db $C5, $FD, $6F, $41, $A0 // vmovdqa ymm0, [rcx-60h]
  db $C5, $FD, $6F, $49, $C0 // vmovdqa ymm1, [rcx-40h]
  db $C5, $FD, $6F, $51, $E0 // vmovdqa ymm2, [rcx-20h]
  db $C5, $FD, $6F, $19      // vmovdqa ymm3, [rcx]
  db $C5, $F9, $6F, $61, $20 // vmovdqa xmm4, [rcx+20h]
  mov rcx, [rcx+30h]
  db $C5, $FD, $7F, $42, $A0 // vmovdqa [rdx-60h], ymm0
  db $C5, $FD, $7F, $4A, $C0 // vmovdqa [rdx-40h], ymm1
  db $C5, $FD, $7F, $52, $E0 // vmovdqa [rdx-20h], ymm2
  db $C5, $FD, $7F, $1A      // vmovdqa [rdx],     ymm3
  db $C5, $F9, $7F, $62, $20 // vmovdqa [rdx+20h], xmm4
  mov [rdx+30h], rcx
  {$ELSE}
                                add rdi, 60h
                                add rsi, 60h
  db $C5, $FD, $6F, $47, $A0 // vmovdqa ymm0, [rdi-60h]
  db $C5, $FD, $6F, $4F, $C0 // vmovdqa ymm1, [rdi-40h]
  db $C5, $FD, $6F, $57, $E0 // vmovdqa ymm2, [rdi-20h]
  db $C5, $FD, $6F, $1F      // vmovdqa ymm3, [rdi]
  db $C5, $F9, $6F, $67, $20 // vmovdqa xmm4, [rdi+20h]
                                mov rdi, [rdi+30h]
  db $C5, $FD, $7F, $46, $A0 // vmovdqa [rsi-60h], ymm0
  db $C5, $FD, $7F, $4E, $C0 // vmovdqa [rsi-40h], ymm1
  db $C5, $FD, $7F, $56, $E0 // vmovdqa [rsi-20h], ymm2
  db $C5, $FD, $7F, $1E      // vmovdqa [rsi],     ymm3
  db $C5, $F9, $7F, $66, $20 // vmovdqa [rsi+20h], xmm4
                                mov [rsi+30h], rdi
  {$ENDIF}
  db $C5, $FD, $EF, $C0      // vpxor ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9      // vpxor ymm1,ymm1,ymm1
  db $C5, $ED, $EF, $D2      // vpxor ymm2,ymm2,ymm2
  db $C5, $E5, $EF, $DB      // vpxor ymm3,ymm3,ymm3
  db $C5, $D9, $EF, $E4      // vpxor xmm4,xmm4,xmm4
end;

procedure Move184AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
                                add rcx, 60h
                                add rdx, 60h
  db $C5, $FD, $6F, $41, $A0 // vmovdqa ymm0, [rcx-60h]
  db $C5, $FD, $6F, $49, $C0 // vmovdqa ymm1, [rcx-40h]
  db $C5, $FD, $6F, $51, $E0 // vmovdqa ymm2, [rcx-20h]
  db $C5, $FD, $6F, $19      // vmovdqa ymm3, [rcx]
  db $C5, $FD, $6F, $61, $20 // vmovdqa ymm4, [rcx+20h]
  db $C5, $F9, $6F, $69, $40 // vmovdqa xmm5, [rcx+40h]
                                mov rcx, [rcx+50h]
  db $C5, $FD, $7F, $42, $A0 // vmovdqa [rdx-60h], ymm0
  db $C5, $FD, $7F, $4A, $C0 // vmovdqa [rdx-40h], ymm1
  db $C5, $FD, $7F, $52, $E0 // vmovdqa [rdx-20h], ymm2
  db $C5, $FD, $7F, $1A      // vmovdqa [rdx],     ymm3
  db $C5, $FD, $7F, $62, $20 // vmovdqa [rdx+20h], ymm4
  db $C5, $F9, $7F, $6A, $40 // vmovdqa [rdx+40h], xmm5
  mov [rdx+50h],rcx
  {$ELSE}
                                add rdi, 60h
                                add rsi, 60h
  db $C5, $FD, $6F, $47, $A0 // vmovdqa ymm0, [rdi-60h]
  db $C5, $FD, $6F, $4F, $C0 // vmovdqa ymm1, [rdi-40h]
  db $C5, $FD, $6F, $57, $E0 // vmovdqa ymm2, [rdi-20h]
  db $C5, $FD, $6F, $1F      // vmovdqa ymm3, [rdi]
  db $C5, $FD, $6F, $67, $20 // vmovdqa ymm4, [rdi+20h]
  db $C5, $F9, $6F, $6F, $40 // vmovdqa xmm5, [rdi+40h]
                                mov rdi, [rdi+50h]
  db $C5, $FD, $7F, $46, $A0 // vmovdqa [rsi-60h], ymm0
  db $C5, $FD, $7F, $4E, $C0 // vmovdqa [rsi-40h], ymm1
  db $C5, $FD, $7F, $56, $E0 // vmovdqa [rsi-20h], ymm2
  db $C5, $FD, $7F, $1E      // vmovdqa [rsi],     ymm3
  db $C5, $FD, $7F, $66, $20 // vmovdqa [rsi+20h], ymm4
  db $C5, $F9, $7F, $6E, $40 // vmovdqa [rsi+40h], xmm5
                                mov [rsi+50h], rdi
  {$ENDIF}
  db $C5, $FD, $EF, $C0      // vpxor ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9      // vpxor ymm1,ymm1,ymm1
  db $C5, $ED, $EF, $D2      // vpxor ymm2,ymm2,ymm2
  db $C5, $E5, $EF, $DB      // vpxor ymm3,ymm3,ymm3
  db $C5, $DD, $EF, $E4      // vpxor ymm4,ymm4,ymm4
  db $C5, $D1, $EF, $ED      // vpxor xmm5,xmm5,xmm5
end;

procedure Move216AVX2(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
                                add rcx, 60h
                                add rdx, 60h
  db $C5, $FD, $6F, $41, $A0 // vmovdqa ymm0, [rcx-60h]
  db $C5, $FD, $6F, $49, $C0 // vmovdqa ymm1, [rcx-40h]
  db $C5, $FD, $6F, $51, $E0 // vmovdqa ymm2, [rcx-20h]
  db $C5, $FD, $6F, $19      // vmovdqa ymm3, [rcx]
  db $C5, $FD, $6F, $61, $20 // vmovdqa ymm4, [rcx+20h]
  db $C5, $FD, $6F, $69, $40 // vmovdqa ymm5, [rcx+40h]
  mov r9, [rcx+60h]
  mov r10, [rcx+68h]
  mov r11, [rcx+70h]
  db $C5, $FD, $7F, $42, $A0 // vmovdqa [rdx-60h], ymm0
  db $C5, $FD, $7F, $4A, $C0 // vmovdqa [rdx-40h], ymm1
  db $C5, $FD, $7F, $52, $E0 // vmovdqa [rdx-20h], ymm2
  db $C5, $FD, $7F, $1A      // vmovdqa [rdx],     ymm3
  db $C5, $FD, $7F, $62, $20 // vmovdqa [rdx+20h], ymm4
  db $C5, $FD, $7F, $6A, $40 // vmovdqa [rdx+40h], ymm5
  mov [rdx+60h], r9
  mov [rdx+68h], r10
  mov [rdx+70h], r11
  {$ELSE}
                                add rdi, 60h
                                add rsi, 60h
  db $C5, $FD, $6F, $47, $A0 // vmovdqa ymm0, [rdi-60h]
  db $C5, $FD, $6F, $4F, $C0 // vmovdqa ymm1, [rdi-40h]
  db $C5, $FD, $6F, $57, $E0 // vmovdqa ymm2, [rdi-20h]
  db $C5, $FD, $6F, $1F      // vmovdqa ymm3, [rdi]
  db $C5, $FD, $6F, $67, $20 // vmovdqa ymm4, [rdi+20h]
  db $C5, $FD, $6F, $6F, $40 // vmovdqa ymm5, [rdi+40h]

{

Although, under unix, we can use xmm6(ymm6) and xmm7 (ymm7), here we mimic the Win64 code, see the comment at Move216AVX1 on this.

We cannot use xmm6(ymm6) and xmm7 (ymm7) under Windows due to the calling convention.

According to Microsoft, "The registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-15 are considered nonvolatile and must be saved and restored by a function that uses them."

}
                                mov r9,  [rdi+60h]
                                mov r10, [rdi+68h]
                                mov r11, [rdi+70h]
  db $C5, $FD, $7F, $46, $A0 // vmovdqa [rsi-60h], ymm0
  db $C5, $FD, $7F, $4E, $C0 // vmovdqa [rsi-40h], ymm1
  db $C5, $FD, $7F, $56, $E0 // vmovdqa [rsi-20h], ymm2
  db $C5, $FD, $7F, $1E      // vmovdqa [rsi],     ymm3
  db $C5, $FD, $7F, $66, $20 // vmovdqa [rsi+20h], ymm4
  db $C5, $FD, $7F, $6E, $40 // vmovdqa [rsi+40h], ymm5
                                mov [rsi+60h], r9
                                mov [rsi+68h], r10
                                mov [rsi+70h], r11
  {$ENDIF}
  db $C5, $FD, $EF, $C0      // vpxor ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9      // vpxor ymm1,ymm1,ymm1
  db $C5, $ED, $EF, $D2      // vpxor ymm2,ymm2,ymm2
  db $C5, $E5, $EF, $DB      // vpxor ymm3,ymm3,ymm3
  db $C5, $DD, $EF, $E4      // vpxor ymm4,ymm4,ymm4
  db $C5, $D5, $EF, $ED      // vpxor ymm5,ymm5,ymm5
end;
{$ENDIF DisableAVX2}

{$IFDEF EnableAVX512}
{$IFDEF unix}
{$message error AVX-512 is not yet implemented for UNIX}
{$ELSE unix}
procedure Move24AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move56AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move88AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move120AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move152AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move184AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move216AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move248AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move280AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move312AVX512(const ASource; var ADest; ACount: NativeInt); external;
procedure Move344AVX512(const ASource; var ADest; ACount: NativeInt); external;

{$ENDIF unix}
{$ENDIF EnableAVX512}

{$ENDIF EnableAVX}
{$ENDIF 64bit}

{--------------Register, FPU, MMX and SSE Move Procedures--------------}

{$IFNDEF ExcludeSmallGranularMoves}

procedure Move4(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  mov eax, [eax]
  mov [edx], eax
{$ELSE 32Bit}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  mov eax, [rcx]
  mov [rdx], eax
  {$ELSE unix}
  mov eax, [rdi]
  mov [rsi], eax
  {$ENDIF unix}
{$ENDIF 32bit}
end;

{$IFDEF 64Bit}
procedure Move8(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  mov rax, [rcx]
  mov [rdx], rax
{$ELSE}
  mov rax, [rdi]
  mov [rsi], rax
{$ENDIF}
end;

procedure Move16(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  mov rax, [rcx]
  mov rcx, [rcx+8]
  mov [rdx], rax
  mov [rdx+8], rcx
{$ELSE}
  mov rax, [rdi]
  mov rdi, [rdi+8]
  mov [rsi], rax
  mov [rsi+8], rdi
{$ENDIF}
end;


procedure Move32(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
    {$IFDEF AllowAsmNoframe}
    .noframe
    {$ENDIF}
    movdqa xmm0, [rcx]
    movdqa xmm1, [rcx+16]
    movdqa [rdx], xmm0
    movdqa [rdx+16], xmm1
  {$ELSE}
    movdqa xmm0, [rdi]
    movdqa xmm1, [rdi+16]
    movdqa [rsi], xmm0
    movdqa [rsi+16], xmm1
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
    {$IFDEF AllowAsmNoframe}
    .noframe
    {$ENDIF}
    movdqu xmm0, [rcx]
    movdqu xmm1, [rcx+16]
    movdqu [rdx], xmm0
    movdqu [rdx+16], xmm1
  {$ELSE}
    movdqu xmm0, [rdi]
    movdqu xmm1, [rdi+16]
    movdqu [rsi], xmm0
    movdqu [rsi+16], xmm1
  {$ENDIF}
{$ENDIF}
    xorps xmm0, xmm0
    xorps xmm1, xmm1
end;

{$ENDIF 64bit}

procedure Move12(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov eax, [eax + 8]
  mov [edx + 4], ecx
  mov [edx + 8], eax
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  mov rax, [rcx]
  mov ecx, [rcx + 8]
  mov [rdx], rax
  mov [rdx + 8], ecx
  {$ELSE}
  mov rax, [rdi]
  mov edi, [rdi + 8]
  mov [rsi], rax
  mov [rsi + 8], edi
  {$ENDIF}
{$ENDIF}
end;


{$IFDEF 32bit_SSE}
procedure Move20_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  mov eax, [eax+16]
  movaps [edx], xmm0
  mov [edx+16], eax
{$ELSE}
  movups xmm0, [eax]
  mov eax, [eax+16]
  movups [edx], xmm0
  mov [edx+16], eax
{$ENDIF}
  xorps xmm0, xmm0
end;
{$ENDIF 32bit_SSE}

procedure Move20(const ASource; var ADest; ACount: NativeInt); assembler;{$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov eax, [eax + 16]
  mov [edx + 12], ecx
  mov [edx + 16], eax
{$ELSE}
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  mov ecx, [rcx + 16]
  movdqa [rdx], xmm0
  mov [rdx + 16], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  mov edi, [rdi + 16]
  movdqa [rsi], xmm0
  mov [rsi + 16], edi
  {$ENDIF}
{$ELSE AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  mov ecx, [rcx + 16]
  movdqu [rdx], xmm0
  mov [rdx + 16], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  mov edi, [rdi + 16]
  movdqu [rsi], xmm0
  mov [rsi + 16], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
{$ENDIF 32Bit}
end;

{$ENDIF ExcludeSmallGranularMoves}


{$IFDEF 64bit}
procedure Move24(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm

{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  mov r8, [rcx + 16]
  movdqa [rdx], xmm0
  mov [rdx + 16], r8
  {$ELSE}
  movdqa xmm0, [rdi]
  mov rdx, [rdi + 16]
  movdqa [rsi], xmm0
  mov [rsi + 16], rdx
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  mov r8, [rcx + 16]
  movdqu [rdx], xmm0
  mov [rdx + 16], r8
  {$ELSE}
  movdqu xmm0, [rdi]
  mov rdx, [rdi + 16]
  movdqu [rsi], xmm0
  mov [rsi + 16], rdx
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
end;
{$ENDIF 64bit}


{$IFNDEF ExcludeSmallGranularMoves}

{$IFDEF 32bit_SSE}
procedure Move28_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler;{$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movups xmm1, [eax+12]
  movaps [edx], xmm0
  movups [edx+12], xmm1
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+12]
  movups [edx], xmm0
  movups [edx+12], xmm1
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
end;
{$ENDIF 32bit_SSE}

procedure Move28(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov [edx + 12], ecx
  mov ecx, [eax + 16]
  mov [edx + 16], ecx
  mov ecx, [eax + 20]
  mov eax, [eax + 24]
  mov [edx + 20], ecx
  mov [edx + 24], eax
{$ELSE}

{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  mov r8, [rcx + 16]
  mov ecx, [rcx + 24]
  movdqa [rdx], xmm0
  mov [rdx + 16], r8
  mov [rdx + 24], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  mov rdx, [rdi + 16]
  mov edi, [rdi + 24]
  movdqa [rsi], xmm0
  mov [rsi + 16], rdx
  mov [rsi + 24], edi
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  mov r8, [rcx + 16]
  mov ecx, [rcx + 24]
  movdqu [rdx], xmm0
  mov [rdx + 16], r8
  mov [rdx + 24], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  mov rdx, [rdi + 16]
  mov edi, [rdi + 24]
  movdqu [rsi], xmm0
  mov [rsi + 16], rdx
  mov [rsi + 24], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
{$ENDIF}
end;

{$IFDEF 32bit_SSE}
procedure Move36_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  mov    eax, [eax+32]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  mov    [edx+32], eax
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  mov    eax, [eax+32]
  movups [edx], xmm0
  movups [edx+16], xmm1
  mov    [edx+32], eax
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
end;
{$ENDIF 32bit_SSE}

procedure Move36(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  mov ecx, [eax + 32]
  mov [edx + 32], ecx
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$ELSE}

{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov ecx, [rcx + 32]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  mov edi, [rdi + 32]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  mov [rsi + 32], edi
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  mov ecx, [rcx + 32]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  mov [rdx + 32], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  mov edi, [rdi + 32]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  mov [rsi + 32], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
{$ENDIF}
end;

{$IFDEF 64bit}
procedure Move40(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], r8
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  mov rdx, [rdi + 32]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  mov [rsi + 32], rdx
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  mov [rdx + 32], r8
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  mov rdx, [rdi + 32]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  mov [rsi + 32], rdx
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
end;

procedure Move48(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  movdqa xmm2, [rdi + 32]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  movdqa [rsi + 32], xmm2
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  movdqu [rdx + 32], xmm2
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  movdqu xmm2, [rdi + 32]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  movdqu [rsi + 32], xmm2
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
end;


{$ENDIF}

{$IFDEF 32bit_SSE}
procedure Move44_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movups xmm2, [eax+28]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movups [edx+28], xmm2
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+28]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+28], xmm2
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
end;
{$ENDIF 32bit_SSE}

procedure Move44(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  mov ecx, [eax + 40]
  mov [edx + 40], ecx
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$ELSE}

{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  mov ecx, [rcx + 40]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], r8
  mov [rdx + 40], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  mov rdx, [rdi + 32]
  mov edi, [rdi + 40]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  mov [rsi + 32], rdx
  mov [rsi + 40], edi
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  mov ecx, [rcx + 40]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  mov [rdx + 32], r8
  mov [rdx + 40], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  mov rdx, [rdi + 32]
  mov edi, [rdi + 40]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  mov [rsi + 32], rdx
  mov [rsi + 40], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
{$ENDIF}
end;


{$IFDEF 32bit_SSE}
procedure Move52_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movaps xmm2, [eax+32]
  mov    eax, [eax+48]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movaps [edx+32], xmm2
  mov    [edx+48], eax
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+32]
  mov    eax, [eax+48]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+32], xmm2
  mov    [edx+48], eax
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
end;
{$ENDIF 32bit_SSE}

procedure Move52(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  mov ecx, [eax + 48]
  mov [edx + 48], ecx
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$ELSE}
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov ecx, [rcx + 48]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  movdqa xmm2, [rdi + 32]
  mov edi, [rdi + 48]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  movdqa [rsi + 32], xmm2
  mov [rsi + 48], edi
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]
  mov ecx, [rcx + 48]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  movdqu [rdx + 32], xmm2
  mov [rdx + 48], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  movdqu xmm2, [rdi + 32]
  mov edi, [rdi + 48]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  movdqu [rsi + 32], xmm2
  mov [rsi + 48], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
{$ENDIF}
end;

{$ENDIF ExcludeSmallGranularMoves}


{$IFDEF 64bit}
procedure Move56(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], r8
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  movdqa xmm2, [rdi + 32]
  mov rdx, [rdi + 48]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  movdqa [rsi + 32], xmm2
  mov [rsi + 48], rdx
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  movdqu [rdx + 32], xmm2
  mov [rdx + 48], r8
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  movdqu xmm2, [rdi + 32]
  mov rdx, [rdi + 48]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  movdqu [rsi + 32], xmm2
  mov [rsi + 48], rdx
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
end;


procedure Move64(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  movdqa xmm3, [rcx + 48]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  movdqa [rdx + 48], xmm3
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  movdqa xmm2, [rdi + 32]
  movdqa xmm3, [rdi + 48]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  movdqa [rsi + 32], xmm2
  movdqa [rsi + 48], xmm3
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]
  movdqu xmm3, [rcx + 48]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  movdqu [rdx + 32], xmm2
  movdqu [rdx + 48], xmm3
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  movdqu xmm2, [rdi + 32]
  movdqu xmm3, [rdi + 48]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  movdqu [rsi + 32], xmm2
  movdqu [rsi + 48], xmm3
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
end;

{$ENDIF 64bit}

{$IFNDEF ExcludeSmallGranularMoves}

{$IFDEF 32bit_SSE}
procedure Move60_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movaps xmm2, [eax+32]
  movups xmm3, [eax+44]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movaps [edx+32], xmm2
  movups [edx+44], xmm3
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+32]
  movups xmm3, [eax+44]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+32], xmm2
  movups [edx+44], xmm3
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
end;
{$ENDIF 32bit_SSE}

procedure Move60(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  mov ecx, [eax + 56]
  mov [edx + 56], ecx
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$ELSE}
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  mov ecx, [rcx + 56]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], r8
  mov [rdx + 56], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  movdqa xmm2, [rdi + 32]
  mov rdx, [rdi + 48]
  mov edi, [rdi + 56]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  movdqa [rsi + 32], xmm2
  mov [rsi + 48], rdx
  mov [rsi + 56], edi
  {$ENDIF}
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  mov ecx, [rcx + 56]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  movdqu [rdx + 32], xmm2
  mov [rdx + 48], r8
  mov [rdx + 56], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  movdqu xmm2, [rdi + 32]
  mov rdx, [rdi + 48]
  mov edi, [rdi + 56]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  movdqu [rsi + 32], xmm2
  mov [rsi + 48], rdx
  mov [rsi + 56], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
{$ENDIF}
end;

{$IFDEF 32bit_SSE}
procedure Move68_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movaps xmm2, [eax+32]
  movaps xmm3, [eax+48]
  mov    eax,  [eax+64]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movaps [edx+32], xmm2
  movaps [edx+48], xmm3
  mov    [edx+64], eax
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+32]
  movups xmm3, [eax+48]
  mov    eax,  [eax+64]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+32], xmm2
  movups [edx+48], xmm3
  mov    [edx+64], eax
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
end;
{$ENDIF 32bit_SSE}

procedure Move68(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  fild qword ptr [eax + 56]
  mov ecx, [eax + 64]
  mov [edx + 64], ecx
  fistp qword ptr [edx + 56]
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$ELSE 32Bit}
{$IFDEF AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  movdqa xmm3, [rcx + 48]
  mov ecx, [rcx + 64]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  movdqa [rdx + 48], xmm3
  mov [rdx + 64], ecx
  {$ELSE}
  movdqa xmm0, [rdi]
  movdqa xmm1, [rdi + 16]
  movdqa xmm2, [rdi + 32]
  movdqa xmm3, [rdi + 48]
  mov edi, [rdi + 64]
  movdqa [rsi], xmm0
  movdqa [rsi + 16], xmm1
  movdqa [rsi + 32], xmm2
  movdqa [rsi + 48], xmm3
  mov [rsi + 64], edi
  {$ENDIF}
{$ELSE AlignAtLeast16Bytes}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  movdqu xmm0, [rcx]
  movdqu xmm1, [rcx + 16]
  movdqu xmm2, [rcx + 32]
  movdqu xmm3, [rcx + 48]
  mov ecx, [rcx + 64]
  movdqu [rdx], xmm0
  movdqu [rdx + 16], xmm1
  movdqu [rdx + 32], xmm2
  movdqu [rdx + 48], xmm3
  mov [rdx + 64], ecx
  {$ELSE}
  movdqu xmm0, [rdi]
  movdqu xmm1, [rdi + 16]
  movdqu xmm2, [rdi + 32]
  movdqu xmm3, [rdi + 48]
  mov edi, [rdi + 64]
  movdqu [rsi], xmm0
  movdqu [rsi + 16], xmm1
  movdqu [rsi + 32], xmm2
  movdqu [rsi + 48], xmm3
  mov [rsi + 64], edi
  {$ENDIF}
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
{$ENDIF 32Bit}
end;

{$IFDEF 32bit_SSE}
procedure Move76_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movaps xmm2, [eax+32]
  movaps xmm3, [eax+48]
  movups xmm4, [eax+60]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movaps [edx+32], xmm2
  movaps [edx+48], xmm3
  movups [edx+60], xmm4
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+32]
  movups xmm3, [eax+48]
  movups xmm4, [eax+60]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+32], xmm2
  movups [edx+48], xmm3
  movups [edx+60], xmm4
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
  xorps xmm4, xmm4
end;
{$ENDIF 32bit_SSE}

{$IFDEF 32bit_SSE}
procedure Move84_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movaps xmm2, [eax+32]
  movaps xmm3, [eax+48]
  movaps xmm4, [eax+64]
  mov    eax,  [eax+80]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movaps [edx+32], xmm2
  movaps [edx+48], xmm3
  movaps [edx+64], xmm4
  mov    [edx+80], eax
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+32]
  movups xmm3, [eax+48]
  movups xmm4, [eax+64]
  mov    eax,  [eax+80]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+32], xmm2
  movups [edx+48], xmm3
  movups [edx+64], xmm4
  mov    [edx+80], eax
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
  xorps xmm4, xmm4
end;
{$ENDIF 32bit_SSE}


{$IFDEF 32bit_SSE}
procedure Move92_32bit_SSE(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF AlignAtLeast16Bytes}
  movaps xmm0, [eax]
  movaps xmm1, [eax+16]
  movaps xmm2, [eax+32]
  movaps xmm3, [eax+48]
  movaps xmm4, [eax+64]
  movups xmm5, [eax+76]
  movaps [edx], xmm0
  movaps [edx+16], xmm1
  movaps [edx+32], xmm2
  movaps [edx+48], xmm3
  movaps [edx+64], xmm4
  movups [edx+76], xmm5
{$ELSE}
  movups xmm0, [eax]
  movups xmm1, [eax+16]
  movups xmm2, [eax+32]
  movups xmm3, [eax+48]
  movups xmm4, [eax+64]
  movups xmm5, [eax+76]
  movups [edx], xmm0
  movups [edx+16], xmm1
  movups [edx+32], xmm2
  movups [edx+48], xmm3
  movups [edx+64], xmm4
  movups [edx+76], xmm5
{$ENDIF}
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  xorps xmm2, xmm2
  xorps xmm3, xmm3
  xorps xmm4, xmm4
  xorps xmm5, xmm5
end;
{$ENDIF 32bit_SSE}

{$ENDIF ExcludeSmallGranularMoves}


{$IFNDEF PurePascal}
procedure MoveWithErmsNoAVX(const ASource; var ADest; ACount: NativeInt); forward;


{Variable size move procedure: Rounds ACount up to the next multiple of 16 less
 SizeOf(Pointer). Important note: Always moves at least 16 - SizeOf(Pointer)
 bytes (the minimum small block size with 16 byte alignment), irrespective of
 ACount.}
procedure MoveX16LP(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit}  nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  test FastMMCpuFeaturesA, FastMMCpuFeatureERMS
  jz @NoERMS
  call MoveWithErmsNoAVX
  jmp  @Finish
@NoERMS:
  {Make the counter negative based: The last 12 bytes are moved separately}
  sub ecx, 12
  add eax, ecx
  add edx, ecx
{$IFDEF EnableMMX}
  {$IFNDEF ForceMMX}
  test FastMMCpuFeaturesA, FastMMCpuFeatureMMX
  jz @FPUMove
  {$ENDIF}
  {Make the counter negative based: The last 12 bytes are moved separately}
  neg ecx
  jns @MMXMoveLast12
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MMXMoveLoop:
  {Move a 16 byte block}
  {$IFDEF Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  db $0f, $6f, $4c, $01, $08
  db $0f, $7f, $04, $11
  db $0f, $7f, $4c, $11, $08
  {$ELSE Delphi4or5}
  movq mm0, [eax + ecx]
  movq mm1, [eax + ecx + 8]
  movq [edx + ecx], mm0
  movq [edx + ecx + 8], mm1
  {$ENDIF Delphi4or5}
  {Are there another 16 bytes to move?}
  add ecx, 16
  js @MMXMoveLoop
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MMXMoveLast12:
  {Do the last 12 bytes}
  {$IFDEF Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  {$ELSE Delphi4or5}
  movq mm0, [eax + ecx]
  {$ENDIF Delphi4or5}
  mov eax, [eax + ecx + 8]
  {$IFDEF Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $7f, $04, $11
  {$ELSE Delphi4or5}
  movq [edx + ecx], mm0
  {$ENDIF Delphi4or5}
  mov [edx + ecx + 8], eax
  {Exit MMX state}
  {$IFDEF Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $77
  {$ELSE Delphi4or5}
  emms
  {$ENDIF Delphi4or5}
  {$IFNDEF ForceMMX}
  jmp @Finish
  {$ENDIF ForceMMX}
{$ENDIF EnableMMX}
{FPU code is only used if MMX is not forced}
{$IFNDEF ForceMMX}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@FPUMove:
  neg ecx
  jns @FPUMoveLast12
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@FPUMoveLoop:
  {Move a 16 byte block}
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]
  {Are there another 16 bytes to move?}
  add ecx, 16
  js @FPUMoveLoop
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@FPUMoveLast12:
  {Do the last 12 bytes}
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  mov eax, [eax + ecx + 8]
  mov [edx + ecx + 8], eax
{$ENDIF ForceMMX}
{$ELSE 32bit}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  test [rip+FastMMCpuFeaturesA], FastMMCpuFeatureERMS
  jz @NoERMS
  call MoveWithErmsNoAVX
  jmp @Finish
@NoERMS:
  {Make the counter negative based: The last 8 bytes are moved separately}
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast8
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MoveLoop:
  {Move a 16 byte block}
{$IFDEF AlignAtLeast16Bytes}
  movdqa xmm0, [rcx + r8]
  movdqa [rdx + r8], xmm0
{$ELSE}
  movdqu xmm0, [rcx + r8]
  movdqu [rdx + r8], xmm0
{$ENDIF}
  {Are there another 16 bytes to move?}
  add r8, 16
  js @MoveLoop
  xorps xmm0, xmm0
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MoveLast8:
  {Do the last 8 bytes}
  mov r9, [rcx + r8]
  mov [rdx + r8], r9
  {$ELSE unix}
  {Make the counter negative based: The last 8 bytes are moved separately}
  sub rdx, 8
  add rdi, rdx
  add rsi, rdx
  neg rdx
  jns @MoveLast8
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MoveLoop:
  {Move a 16 byte block}
{$IFDEF AlignAtLeast16Bytes}
  movdqa xmm0, [rdi + rdx]
  movdqa [rsi + rdx], xmm0
{$ELSE}
  movdqu xmm0, [rdi + rdx]
  movdqu [rsi + rdx], xmm0
{$ENDIF}
  {Are there another 16 bytes to move?}
  add rdx, 16
  js @MoveLoop
  xorps xmm0, xmm0
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@MoveLast8:
  {Do the last 8 bytes}
  mov rcx, [rdi + rdx]
  mov [rsi + rdx], rcx
  {$ENDIF unix}
{$ENDIF 32bit}
@Finish:
end;


{Variable size move procedure: Rounds ACount up to the next multiple of 32 less
 SizeOf(Pointer). Important note: Always moves at least 32 - SizeOf(Pointer)
 bytes (the minimum small block size with 16 byte alignment), irrespective of
 ACount.}

{$IFDEF EnableAVX}

procedure MoveX32LpAvx1NoErms(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit}  nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  {Make the counter negative based: The last 24 bytes are moved separately}
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast8

  db $C5, $F8, $77      // vzeroupper

  cmp r8, -128
  jg  @SmallAvxMove

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@AvxBigMoveAlignedAll:
  db $C4, $C1, $7D, $6F, $04, $08      // vmovdqa ymm0, ymmword ptr [rcx+r8]
  db $C4, $C1, $7D, $6F, $4C, $08, $20 // vmovdqa ymm1, ymmword ptr [rcx+r8+20h]
  db $C4, $C1, $7D, $6F, $54, $08, $40 // vmovdqa ymm2, ymmword ptr [rcx+r8+40h]
  db $C4, $C1, $7D, $6F, $5C, $08, $60 // vmovdqa ymm3, ymmword ptr [rcx+r8+60h]
  db $C4, $C1, $7D, $7F, $04, $10      // vmovdqa ymmword ptr [rdx+r8], ymm0
  db $C4, $C1, $7D, $7F, $4C, $10, $20 // vmovdqa ymmword ptr [rdx+r8+20h], ymm1
  db $C4, $C1, $7D, $7F, $54, $10, $40 // vmovdqa ymmword ptr [rdx+r8+40h], ymm2
  db $C4, $C1, $7D, $7F, $5C, $10, $60 // vmovdqa ymmword ptr [rdx+r8+60h], ymm3
  add r8, 128
  cmp r8, -128
  jl  @AvxBigMoveAlignedAll

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@SmallAvxMove:

@MoveLoopAvx:
  {Move a 16 byte block}
  db $C4, $A1, $79, $6F, $04, $01      // vmovdqa xmm0,xmmword ptr [rcx+r8]
  db $C4, $A1, $79, $7F, $04, $02      // vmovdqa xmmword ptr [rdx+r8],xmm0
  {Are there another 16 bytes to move?}
  add r8, 16
  js @MoveLoopAvx

  db $C5, $FC, $57, $C0                // vxorps      ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9                // vxorps      ymm1,ymm1,ymm1
  db $C5, $EC, $57, $D2                // vxorps      ymm2,ymm2,ymm2
  db $C5, $E4, $57, $DB                // vxorps      ymm3,ymm3,ymm3
  db $C5, $F8, $77                     // vzeroupper

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}

@MoveLast8:
  {Do the last 8 bytes}
  mov rcx, [rcx + r8]
  mov [rdx + r8], rcx
  {$ELSE unix}
  {MoveX32LP is not implemented for Unix yet, call the 16-byte version}
  call MoveX16LP
  {$ENDIF unix}
@exit:
end;

procedure MoveX32LpAvx2NoErms(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit}  nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  {Make the counter negative based: The last 24 bytes are moved separately}
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast8

  cmp r8, -128
  jg  @SmallAvxMove

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@AvxBigMoveAlignedAll:
  db $C4, $C1, $7D, $6F, $04, $08      // vmovdqa ymm0, ymmword ptr [rcx+r8]
  db $C4, $C1, $7D, $6F, $4C, $08, $20 // vmovdqa ymm1, ymmword ptr [rcx+r8+20h]
  db $C4, $C1, $7D, $6F, $54, $08, $40 // vmovdqa ymm2, ymmword ptr [rcx+r8+40h]
  db $C4, $C1, $7D, $6F, $5C, $08, $60 // vmovdqa ymm3, ymmword ptr [rcx+r8+60h]
  db $C4, $C1, $7D, $7F, $04, $10      // vmovdqa ymmword ptr [rdx+r8], ymm0
  db $C4, $C1, $7D, $7F, $4C, $10, $20 // vmovdqa ymmword ptr [rdx+r8+20h], ymm1
  db $C4, $C1, $7D, $7F, $54, $10, $40 // vmovdqa ymmword ptr [rdx+r8+40h], ymm2
  db $C4, $C1, $7D, $7F, $5C, $10, $60 // vmovdqa ymmword ptr [rdx+r8+60h], ymm3
  add r8, 128
  cmp r8, -128
  jl  @AvxBigMoveAlignedAll

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@SmallAvxMove:

@MoveLoopAvx:
  {Move a 16 byte block}
  db $C4, $A1, $79, $6F, $04, $01       // vmovdqa xmm0,xmmword ptr [rcx+r8]
  db $C4, $A1, $79, $7F, $04, $02       // vmovdqa xmmword ptr [rdx+r8],xmm0
  {Are there another 16 bytes to move?}
  add r8, 16
  js @MoveLoopAvx

  db $C5, $FD, $EF, $C0                 // vpxor       ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9                 // vpxor       ymm1,ymm1,ymm1
  db $C5, $ED, $EF, $D2                 // vpxor       ymm2,ymm2,ymm2
  db $C5, $E5, $EF, $DB                 // vpxor       ymm3,ymm3,ymm3

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MoveLast8:
  {Do the last 8 bytes}
  mov rcx, [rcx + r8]
  mov [rdx + r8], rcx
  {$ELSE unix}
  {MoveX32LP is not implemented for Unix yet, call the 16-byte version}
  call MoveX16LP
  {$ENDIF unix}
@exit:
end;

{$IFDEF EnableERMS}

// According to the Intel Optimization Reference Manual (Section 3.7.6.2, Memcpy Considerations), rep movsb outperforms AVX copy on blocks of 2048 bytes and above

const
  cLeastErmsAdvantageLengh = 2048;

procedure MoveX32LpAvx1WithErms(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  {Make the counter negative based: The last 24 bytes are moved separately}
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast8

  cmp r8, 0-cLeastErmsAdvantageLengh
  jg @DontDoRepMovsb

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}

@DoRepMovsb:
  mov rax, rsi
  mov r9, rdi
  lea rsi, [rcx+r8]
  lea rdi, [rdx+r8]
  neg r8
  add r8, 8
  mov rcx, r8
  cld
  rep movsb
  mov rdi, r9
  mov rsi, rax
  jmp @exit

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@DontDoRepMovsb:

  db $C5, $F8, $77      // vzeroupper

  cmp r8, -128
  jg  @SmallAvxMove

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@AvxBigMoveAlignedAll:
  db $C4, $C1, $7D, $6F, $04, $08      // vmovdqa ymm0, ymmword ptr [rcx+r8]
  db $C4, $C1, $7D, $6F, $4C, $08, $20 // vmovdqa ymm1, ymmword ptr [rcx+r8+20h]
  db $C4, $C1, $7D, $6F, $54, $08, $40 // vmovdqa ymm2, ymmword ptr [rcx+r8+40h]
  db $C4, $C1, $7D, $6F, $5C, $08, $60 // vmovdqa ymm3, ymmword ptr [rcx+r8+60h]
  db $C4, $C1, $7D, $7F, $04, $10      // vmovdqa ymmword ptr [rdx+r8], ymm0
  db $C4, $C1, $7D, $7F, $4C, $10, $20 // vmovdqa ymmword ptr [rdx+r8+20h], ymm1
  db $C4, $C1, $7D, $7F, $54, $10, $40 // vmovdqa ymmword ptr [rdx+r8+40h], ymm2
  db $C4, $C1, $7D, $7F, $5C, $10, $60 // vmovdqa ymmword ptr [rdx+r8+60h], ymm3
  add r8, 128
  cmp r8, -128
  jl  @AvxBigMoveAlignedAll

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@SmallAvxMove:

@MoveLoopAvx:
  {Move a 16 byte block}
  db $C4, $A1, $79, $6F, $04, $01      // vmovdqa xmm0,xmmword ptr [rcx+r8]
  db $C4, $A1, $79, $7F, $04, $02      // vmovdqa xmmword ptr [rdx+r8],xmm0
  {Are there another 16 bytes to move?}
  add r8, 16
  js @MoveLoopAvx

  db $C5, $FC, $57, $C0                // vxorps      ymm0,ymm0,ymm0
  db $C5, $F4, $57, $C9                // vxorps      ymm1,ymm1,ymm1
  db $C5, $EC, $57, $D2                // vxorps      ymm2,ymm2,ymm2
  db $C5, $E4, $57, $DB                // vxorps      ymm3,ymm3,ymm3
  db $C5, $F8, $77                     // vzeroupper

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}

@MoveLast8:
  {Do the last 8 bytes}
  mov rcx, [rcx + r8]
  mov [rdx + r8], rcx
  {$ELSE unix}
  {MoveX32LP is not implemented for Unix yet, call the 16-byte version}
  call MoveX16LP
  {$ENDIF unix}
@exit:
end;

procedure MoveX32LpAvx2WithErms(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  {Make the counter negative based: The last 24 bytes are moved separately}
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast8

  cmp r8, 0-cLeastErmsAdvantageLengh
  jg @DontDoRepMovsb

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}

@DoRepMovsb:
  mov rax, rsi
  mov r9, rdi
  lea rsi, [rcx+r8]
  lea rdi, [rdx+r8]
  neg r8
  add r8, 8
  mov rcx, r8
  cld
  rep movsb
  mov rdi, r9
  mov rsi, rax
  jmp @exit

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@DontDoRepMovsb:
  cmp r8, -128
  jg  @SmallAvxMove

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@AvxBigMoveAlignedAll:
  db $C4, $C1, $7D, $6F, $04, $08      // vmovdqa ymm0, ymmword ptr [rcx+r8]
  db $C4, $C1, $7D, $6F, $4C, $08, $20 // vmovdqa ymm1, ymmword ptr [rcx+r8+20h]
  db $C4, $C1, $7D, $6F, $54, $08, $40 // vmovdqa ymm2, ymmword ptr [rcx+r8+40h]
  db $C4, $C1, $7D, $6F, $5C, $08, $60 // vmovdqa ymm3, ymmword ptr [rcx+r8+60h]
  db $C4, $C1, $7D, $7F, $04, $10      // vmovdqa ymmword ptr [rdx+r8], ymm0
  db $C4, $C1, $7D, $7F, $4C, $10, $20 // vmovdqa ymmword ptr [rdx+r8+20h], ymm1
  db $C4, $C1, $7D, $7F, $54, $10, $40 // vmovdqa ymmword ptr [rdx+r8+40h], ymm2
  db $C4, $C1, $7D, $7F, $5C, $10, $60 // vmovdqa ymmword ptr [rdx+r8+60h], ymm3
  add r8, 128
  cmp r8, -128
  jl  @AvxBigMoveAlignedAll

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@SmallAvxMove:

@MoveLoopAvx:
  {Move a 16 byte block}
  db $C4, $A1, $79, $6F, $04, $01      // vmovdqa xmm0,xmmword ptr [rcx+r8]
  db $C4, $A1, $79, $7F, $04, $02      // vmovdqa xmmword ptr [rdx+r8],xmm0
  {Are there another 16 bytes to move?}
  add r8, 16
  js @MoveLoopAvx

  db $C5, $FD, $EF, $C0                // vpxor       ymm0,ymm0,ymm0
  db $C5, $F5, $EF, $C9                // vpxor       ymm1,ymm1,ymm1
  db $C5, $ED, $EF, $D2                // vpxor       ymm2,ymm2,ymm2
  db $C5, $E5, $EF, $DB                // vpxor       ymm3,ymm3,ymm3

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}

@MoveLast8:
  {Do the last 8 bytes}
  mov rcx, [rcx + r8]
  mov [rdx + r8], rcx
  {$ELSE unix}
  {MoveX32LP is not implemented for Unix yet, call the 16-byte version}
  call MoveX16LP
  {$ENDIF unix}
@exit:
end;
{$ENDIF EnableERMS}

{$ENDIF EnableAVX}


{$IFDEF EnableERMS}

{This routine is only called with the CPU supports "Enhanced REP MOVSB/STOSB",
see "Intel 64 and IA-32 Architectures Optimization Reference Manual
p. 3.7.7 (Enhanced REP MOVSB and STOSB operation (ERMSB)).
We first check the corresponding bit in the CPUID, and, if it is supported,
call this routine.}

const
  cAlignErmsDestinationBits = 6;
  cAlignErmsDestinationBoundary = (1 shl cAlignErmsDestinationBits);
  cAlignErmsDestinationMask     = cAlignErmsDestinationBoundary-1;

  cRoundErmsBlockSizeBits = 6;
  cRoundErmsBlockSizeBoundary = (1 shl cRoundErmsBlockSizeBits);
  cRoundErmsBlockSizeMask     = cRoundErmsBlockSizeBoundary-1;

  cRepMovsSmallBlock = Cardinal(cRoundErmsBlockSizeBoundary) * 3;

procedure MoveWithErmsNoAVX(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
// Under 32-bit Windows or Unix, the call passes first parametr in EAX, second in EDX, third in ECX

  push    ebx
  push    esi
  push    edi
  mov     esi, eax
  mov     edi, edx

  cmp     ecx, cRepMovsSmallBlock
  jbe     @SmallBlock
// test destination alignment
  mov     eax, edi
  and     eax, cAlignErmsDestinationMask
  jz      @DestinationAligned
  mov     ebx, ecx
  mov     ecx, cAlignErmsDestinationBoundary
  sub     ecx, eax
  sub     ebx, ecx

@again:
  mov     eax, [esi]
  mov     edx, [esi+4]
  mov     [edi], eax
  mov     [edi+4], edx
  add     esi, 8
  add     edi, 8
  sub     ecx, 8
  jg      @again
  add     ebx, ecx
  add     esi, ecx
  add     edi, ecx
  mov     ecx, ebx

@DestinationAligned:

// test block size rounding
  mov     eax, ecx
  and     eax, cRoundErmsBlockSizeMask
  jz      @SingleMove  // the block size is aligned
  sub     ecx, eax
  shr     ecx, 2
  cld
  rep     movsd
  mov     ecx, eax
  jmp     @SmallBlock

@SingleMove:
  shr     ecx, 2
  cld
  rep     movsd
  jmp     @finish

@SmallBlock:

// on 32-bit, fast short strings do not work, at least on Ice Lake

  cmp     ecx, 8
  jb      @below8left

{$IFDEF 32bit_SSE}
  cmp     ecx, 32
  jb      @below32left

  test    FastMMCpuFeaturesA, FastMMCpuFeatureSSE
  jz      @NoSSE // no SSE

  sub     ecx, 32
@LoopSSE:
  movups  xmm0, [esi+16*0]
  movups  xmm1, [esi+16*1]
  movups  [edi+16*0], xmm0
  movups  [edi+16*1], xmm1
  add     esi, 32
  add     edi, 32
  sub     ecx, 32
  jge     @LoopSSE
  xorps   xmm0, xmm0
  xorps   xmm1, xmm1
  add     ecx, 32
  jz      @finish


@NoSSE:
{$ENDIF}

@below32left:
  sub     ecx, 8
  js      @below8left_add

@again3:
  mov     eax, [esi]
  mov     edx, [esi+4]
  mov     [edi], eax
  mov     [edi+4], edx
  add     esi, 8
  add     edi, 8
  sub     ecx, 8
  jge     @again3

@below8left_add:
  add     ecx, 8

@below8left:
  jz      @finish

@loop4:
  mov     eax, [esi]
  mov     [edi], eax
  add     esi, 4
  add     edi, 4
  sub     ecx, 4
  jg      @loop4

@finish:
  pop     edi
  pop     esi
  pop     ebx

{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}

// under Win64, first - RCX, second - RDX, third R8; the caller must preserve RSI and RDI

  cmp    r8, 32
  ja     @beg

// try a small move of up to 32 bytes
@again0:
  mov    rax, [rcx]
  mov    [rdx], rax
  add    rcx, 8
  add    rdx, 8
  sub    r8, 8
  jg     @again0
  jmp    @exit

@beg:
  mov    r9, rsi  // save rsi
  mov    r10, rdi // save rdi
  mov    rsi, rcx
  mov    rdi, rdx
  mov    rcx, r8

  cmp     rcx, cRepMovsSmallBlock
  jbe     @SmallBlock
// test destination alignment
  mov     rax, rdi
  and     rax, cAlignErmsDestinationMask
  jz      @DestinationAligned
  mov     r8, rcx
  mov     rcx, cAlignErmsDestinationBoundary
  sub     rcx, rax
  sub     r8, rcx

@again:
  mov     rax, [rsi]
  mov     rdx, [rsi+8]
  mov     [rdi], rax
  mov     [rdi+8], rdx
  add     rsi, 16
  add     rdi, 16
  sub     rcx, 16
  jg      @again
  add     r8, rcx
  add     rsi, rcx
  add     rdi, rcx
  mov     rcx, r8

@DestinationAligned:

// test block size rounding
  mov     rax, rcx
  and     rax, cRoundErmsBlockSizeMask
  jz      @SingleMove  // the block size is aligned
  sub     rcx, rax
  shr     rcx, 3
  cld
  rep     movsq
  mov     rcx, rax
  jmp     @TailAfterMovs

@SingleMove:
  shr     rcx, 3
  cld
  rep     movsq
  jmp     @finish

{$IFDEF EnableFSRM}
@movs:
  cld
  rep     movsb
  jmp     @finish
{$ENDIF}

@SmallBlock:
  cmp     rcx, 64
  jbe     @Left64OrLess

{$IFNDEF fpc}
{$IFDEF EnableFSRM}
  // moves of 64 bytes or less are good only when we have fast short strings on 64 bit,
  // but not on 32 bit
  test    [rip+FastMMCpuFeaturesA], FastMMCpuFeatureFSRM
  jnz     @movs
{$ENDIF}
{$ENDIF}

@Left64OrLess:

@TailAfterMovs:
  cmp     rcx, 16
  jb      @below16left
  sub     rcx, 16
@again3:
  mov     rax, [rsi]
  mov     rdx, [rsi+8]
  mov     [rdi], rax
  mov     [rdi+8], rdx
  add     rsi, 16
  add     rdi, 16
  sub     rcx, 16
  jge     @again3
  add     rcx, 16

@below16left:
  jz      @finish

@again2:
  mov     eax, [rsi]
  mov     [rdi], eax
  add     rsi, 4
  add     rdi, 4
  sub     rcx, 4
  jg      @again2
@finish:
  mov    rsi, r9
  mov    rdi, r10
  {$ELSE}
// Under Unix 64 the first 3 arguments are passed in RDI, RSI, RDX
  mov    rcx, rsi
  mov    rsi, rdi
  mov    rdi, rcx
  mov    rcx, rdx
  cld
  rep    movsb
  {$ENDIF}
{$ENDIF}
@exit:
end;

{$ENDIF EnableERMS}


{$IFDEF 64bit}
{$IFDEF EnableAVX512}
{$IFNDEF DisableMoveX32LpAvx512}
procedure MoveX32LpAvx512WithErms(const ASource; var ADest; ACount: NativeInt); external;
{$ENDIF}

{ FastMM4_AVX512.obj file is needed to enable AVX-512 code for FastMM4-AVX.
  Use "nasm.exe -Ox -f win64 FastMM4_AVX512.asm" to compile this .obj file,
  or run the Compile_FastMM4_AVX512.cmd file.

  Define DisableAVX512 if you don't want to compile this .obj file.}

{$L FastMM4_AVX512.obj}
{$ENDIF}
{$ENDIF}

{$IFDEF Align32Bytes}
procedure MoveX32LpUniversal(const ASource; var ADest; ACount: NativeInt);
var
  F: Byte;
begin
{$IFDEF USE_CPUID}
  F := FastMMCpuFeaturesA;
{$IFDEF EnableFSRM}
  if F and FastMMCpuFeatureFSRM <> 0 then
  begin
    MoveWithErmsNoAVX(ASource, ADest, ACount);
  end else
{$ENDIF}
  {$IFDEF EnableAVX}
  if (F and FastMMCpuFeatureAVX2) <> 0 then
  begin
    {$IFDEF EnableERMS}
    if (F and FastMMCpuFeatureERMS) <> 0 then
    begin
      {$IFDEF 64bit}
      {$IFDEF EnableAVX512}
      {$IFNDEF DisableMoveX32LpAvx512}
      if (F and FastMMCpuFeatureAVX512) <> 0 then
      begin
        MoveX32LpAvx512WithErms(ASource, ADest, ACount)
      end
      else
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      begin
        MoveX32LpAvx2WithErms(ASource, ADest, ACount)
      end;
    end else
    {$ENDIF}
    begin
      MoveX32LpAvx2NoErms(ASource, ADest, ACount)
    end;
  end else
  if (F and FastMMCpuFeatureAVX1) <> 0 then
  begin
    {$IFDEF EnableERMS}
    if (F and FastMMCpuFeatureERMS) <> 0 then
    begin
      MoveX32LpAvx1WithErms(ASource, ADest, ACount)
    end else
    {$ENDIF}
    begin
      MoveX32LpAvx1NoErms(ASource, ADest, ACount)
    end;
  end else
  {$ENDIF EnableAVX}
  begin
    {$IFDEF EnableERMS}
    if (F and FastMMCpuFeatureERMS) <> 0 then
    begin
      MoveWithErmsNoAVX(ASource, ADest, ACount)
    end else
    {$ENDIF}
    begin
      MoveX16LP(ASource, ADest, ACount)
    end;
  end;
{$ELSE}
  MoveX16LP(ASource, ADest, ACount)
{$ENDIF}
end;
{$ENDIF}

{Variable size move procedure: Rounds ACount up to the next multiple of 8 less
 SizeOf(Pointer). Important note: Always moves at least 8 - SizeOf(Pointer)
 bytes (the minimum small block size with 8 byte alignment), irrespective of
 ACount.}
procedure MoveX8LP(const ASource; var ADest; ACount: NativeInt); assembler; {$IFDEF fpc64bit}  nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  test FastMMCpuFeaturesA, FastMMCpuFeatureERMS
  jz @NoERMS
  call MoveWithErmsNoAVX
  jmp @Finish

@NoERMS:
  {Make the counter negative based: The last 4 bytes are moved separately}
  sub ecx, 4
  {4 bytes or less? -> Use the Move4 routine.}
  jle @FourBytesOrLess
  add eax, ecx
  add edx, ecx
  neg ecx
{$IFDEF EnableMMX}
  {$IFNDEF ForceMMX}
  test FastMMCpuFeaturesA, FastMMCpuFeatureMMX
  jz @FPUMoveLoop
  {$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@MMXMoveLoop:
  {Move an 8 byte block}
{$IFDEF Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  db $0f, $7f, $04, $11
{$ELSE}
  movq mm0, [eax + ecx]
  movq [edx + ecx], mm0
{$ENDIF}
  {Are there another 8 bytes to move?}
  add ecx, 8
  js @MMXMoveLoop
  {Exit MMX state}
{$IFDEF Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $77
{$ELSE}
  emms
{$ENDIF}
  {Do the last 4 bytes}
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
  jmp @Finish
{$ENDIF}
{FPU code is only used if MMX is not forced}
{$IFNDEF ForceMMX}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}

@FPUMoveLoop:
  {Move an 8 byte block}
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  {Are there another 8 bytes to move?}
  add ecx, 8
  js @FPUMoveLoop
  {Do the last 4 bytes}
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
  jmp @Finish
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@FourBytesOrLess:
  {Four or less bytes to move}
  mov eax, [eax]
  mov [edx], eax
{$ELSE}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  {Make the counter negative based}
  add rcx, r8
  add rdx, r8
  neg r8
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MoveLoop:
  {Move an 8 byte block}
  mov r9, [rcx + r8]
  mov [rdx + r8], r9
  {Are there another 8 bytes to move?}
  add r8, 8
  js @MoveLoop
  {$ELSE}
  {Make the counter negative based}
  add rdi, rdx
  add rsi, rdx
  neg rdx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MoveLoop:
  {Move an 8 byte block}
  mov rcx, [rdi + rdx]
  mov [rsi + rdx], rcx
  {Are there another 8 bytes to move?}
  add rdx, 8
  js @MoveLoop
  {$ENDIF}
{$ENDIF}
@Finish:
end;

{$ENDIF ASMVersion}

{----------------Windows Emulation Functions for Kylix / OS X Support-----------------}

{$IFDEF POSIX}

const
  {Messagebox constants}
  MB_OK = 0;
  MB_ICONERROR = $10;
  MB_TASKMODAL = $2000;
  MB_DEFAULT_DESKTOP_ONLY = $20000;
  {Virtual memory constants}
  MEM_COMMIT = Cardinal($1000);
  MEM_RELEASE = Cardinal($8000);
  MEM_TOP_DOWN = Cardinal($100000);
  PAGE_READWRITE = 4;

procedure MessageBoxA(hWnd: Cardinal; AMessageText, AMessageTitle: PAnsiChar; uType: Cardinal); stdcall;
begin
  if FastMMIsInstalled then
    writeln(AMessageText)
  else
    {$IFNDEF fpc}
    __write(STDERR_FILENO, AMessageText, StrLen(AMessageText));
    {$ELSE}
    FpWrite(StdErrorHandle, AMessageText, StrLen(AMessageText));
    {$ENDIF}
end;

{$IFNDEF MACOS}
function VirtualAlloc(lpvAddress: Pointer; dwSize, flAllocationType, flProtect: Cardinal): Pointer; stdcall;
begin
  Result := valloc(dwSize);
end;

function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: Cardinal): LongBool; stdcall;
begin
  free(lpAddress);
  Result := True;
end;
{$ENDIF}

function WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Boolean; stdcall;
begin
  {$IFNDEF fpc}
  lpNumberOfBytesWritten := __write(hFile, {$IFDEF MACOS}@Buffer{$ELSE}Buffer{$ENDIF},
    nNumberOfBytesToWrite);
  {$ELSE}
  lpNumberOfBytesWritten := fpwrite(hFile, Buffer, nNumberOfBytesToWrite);
  {$ENDIF}
  if lpNumberOfBytesWritten = Cardinal(-1) then
  begin
    lpNumberOfBytesWritten := 0;
    Result := False;
  end
  else
    Result := True;
end;

{$IFNDEF NeverSleepOnThreadContention}
procedure Sleep(dwMilliseconds: Cardinal); stdcall;
begin
  {Convert to microseconds (more or less)}
  usleep(dwMilliseconds shl 10);
end;
{$ENDIF}
{$ENDIF}

{-----------------Debugging Support Functions and Procedures------------------}

{$IFDEF FullDebugMode}

{Returns the current thread ID}
function GetThreadID: Cardinal;
{$IFDEF WIN32}
assembler;
asm
  mov eax, FS:[$24]
end;
{$ELSE}
begin
  Result := GetCurrentThreadId;
end;
{$ENDIF}

{Fills a block of memory with the given dword (32-bit) or qword (64-bit).
 Always fills a multiple of SizeOf(Pointer) bytes}
procedure DebugFillMem(var AAddress; AByteCount: NativeInt; AFillValue: NativeUInt); assembler; {$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 32Bit}
  {On Entry:
   eax = AAddress
   edx = AByteCount
   ecx = AFillValue}
  add eax, edx
  neg edx
  jns @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@FillLoop:
  mov [eax + edx], ecx
  add edx, 4
  js @FillLoop
@Done:
{$ELSE 32Bit}
  {$IFNDEF unix}

  {$IFDEF AllowAsmNoframe}
.noframe
  {$ENDIF}

  {On Entry:
   rcx = AAddress
   rdx = AByteCount
   r8 = AFillValue}
  add rcx, rdx
  neg rdx
  jns @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@FillLoop:
  mov [rcx + rdx], r8
  add rdx, 8
  js @FillLoop
@Done:
  {$ELSE unix}
    {On Entry:
   rdi = AAddress
   rsi = AByteCount
   rdx = AFillValue}
  add rdi, rsi
  neg rsi
  jns @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@FillLoop:
  mov [rdi + rsi], rdx
  add rsi, 8
  js @FillLoop
@Done:
  {$ENDIF unix}
{$ENDIF 32Bit}
end;
{$ENDIF}

{$IFDEF _StackTracer}
{------------------------Stack tracer---------------------------}

  {$IFNDEF LoadDebugDLLDynamically}

{The stack trace procedure. The stack trace module is external since it may
 raise handled access violations that result in the creation of exception
 objects and the stack trace code is not re-entrant.}
procedure GetStackTrace(AReturnAddresses: PNativeUInt;
  AMaxDepth, ASkipFrames: Cardinal); external FullDebugModeLibraryName
  name {$IFDEF RawStackTraces}'GetRawStackTrace'{$ELSE}'GetFrameBasedStackTrace'{$ENDIF};

{The exported procedure in the FastMM_FullDebugMode.dll library used to convert
 the return addresses of a stack trace to a text string.}
function LogStackTrace(AReturnAddresses: PNativeUInt;
  AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar; external FullDebugModeLibraryName
  name 'LogStackTrace';

  {$ELSE}

  {Default no-op stack trace and logging handlers}
  procedure NoOpGetStackTrace(AReturnAddresses: PNativeUInt;
    AMaxDepth, ASkipFrames: Cardinal);
  begin
    DebugFillMem(AReturnAddresses^, AMaxDepth * SizeOf(Pointer), 0);
  end;

  function NoOpLogStackTrace(AReturnAddresses: PNativeUInt;
    AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar;
  begin
    Result := ABuffer;
  end;

var

  {Handle to the FullDebugMode DLL}
  FullDebugModeDLL: HMODULE;
  {$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically ) AND Defined( MemoryLoadLibrarySupport )}
  FullDebugModeRDLL: PMemoryModule;
  {$IFEND}

  GetStackTrace: procedure (AReturnAddresses: PNativeUInt;
    AMaxDepth, ASkipFrames: Cardinal) = NoOpGetStackTrace;

  LogStackTrace: function (AReturnAddresses: PNativeUInt;
    AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar = NoOpLogStackTrace;

  {$ENDIF}

{$ENDIF}

{$IFDEF UseReleaseStack }
function GetStackSlot: DWORD;
begin
// http://burtleburtle.net/bob/hash/integer.html
  Result := GetCurrentThreadID;
  Result := (Result xor 61) xor (Result shr 16);
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 4);
  Result := Result * $27d4eb2d;
  Result := Result xor (Result shr 15);
  Result := Result and (NumStacksPerBlock - 1);
end;
{$ENDIF}

{$IFNDEF POSIX}
function DelphiIsRunning: Boolean;
begin
  Result := FindWindowA('TAppBuilder', nil) <> 0;
end;
{$ENDIF}

{Converts an unsigned integer to string at the buffer location, returning the
 new buffer position. Note: The 32-bit assembler version only supports numbers
 up to 2^31 - 1.}


{Input:
  ANum - the NativeUInt value to convert ;
  APBuffer - output buffer;
  ABufferLengthChars - the size of the output buffer in characters (not in bytes);
                       since currently one char is one byte, the maxiumum lenght
                       of the buffer in characters is the same as the size of the
                       buffer in bytes, but if we switch to double-byte charaters
                       in future (e.g. UTF-16), this will differ}

function NativeUIntToStrBuf(ANum: NativeUInt; APBuffer: PAnsiChar; ABufferLengthChars: Cardinal): PAnsiChar;
{$IFNDEF Use32BitAsm}
const
  MaxDigits = 20;
var
  LDigitBuffer: array[0..MaxDigits - 1] of AnsiChar;
  LCount: Cardinal;
  LIndex: Cardinal;
  LDigit: NativeUInt;
  LNum: NativeUInt;
begin
  {Generate the digits in the local buffer}
  LNum := ANum;
  LCount := 0;
  repeat
    LDigit := LNum;
    LNum := LNum div 10;
    LDigit := LDigit - LNum * 10;
    Inc(LCount);
    LIndex := MaxDigits - LCount;
    LDigitBuffer[LIndex] := AnsiChar(Ord('0') + LDigit);
  until (LNum = 0) or (LIndex = 0);
  {Copy the digits to the output buffer and advance it}
  if LCount < ABufferLengthChars then
  begin
    System.Move(LDigitBuffer[LIndex], APBuffer^, LCount*SizeOf(APBuffer[0]));
    Result := APBuffer + LCount;
  end else
  begin
    Result := APBuffer;
    Result^ := #0;
  end;
end;
{$ELSE}
assembler;
asm
  {On entry: eax = ANum, edx = APBuffer, ecx = ABufferLengthChars}
  {todo: implement ecx(ABufferLengthChars) checking for BASM}
  push edi
  mov edi, edx                //Pointer to the first character in edi
  {Calculate leading digit: divide the number by 1e9}
  add eax, 1                  //Increment the number
  mov edx, $89705F41          //1e9 reciprocal
  mul edx                     //Multplying with reciprocal
  shr eax, 30                 //Save fraction bits
  mov ecx, edx                //First digit in bits <31:29>
  and edx, $1FFFFFFF          //Filter fraction part edx<28:0>
  shr ecx, 29                 //Get leading digit into accumulator
  lea edx, [edx + 4 * edx]    //Calculate ...
  add edx, eax                //... 5*fraction
  mov eax, ecx                //Copy leading digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #2}
  mov eax, edx                //Point format such that 1.0 = 2^28
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 28                 //Next digit
  and edx, $0fffffff          //Fraction part edx<27:0>
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #3}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:27>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<26:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 27                 //Next digit
  and edx, $07ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #4}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:26>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<25:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 26                 //Next digit
  and edx, $03ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #5}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:25>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<24:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 25                 //Next digit
  and edx, $01ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #6}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:24>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<23:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 24                 //Next digit
  and edx, $00ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #7}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:23>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<31:23>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 23                 //Next digit
  and edx, $007fffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #8}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:22>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<22:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 22                 //Next digit
  and edx, $003fffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #9}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:21>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<21:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 21                 //Next digit
  and edx, $001fffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #10}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:20>
  cmp ecx, 1                  //Any-non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 20                 //Next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store last digit and end marker out to memory
  {Return a pointer to the next character}
  lea eax, [edi + 1]
  {Restore edi}
  pop edi
end;
{$ENDIF}

{Converts an unsigned integer to a hexadecimal string at the buffer location,
 returning the new buffer position.}
function NativeUIntToHexBuf(ANum: NativeUInt; APBuffer: PAnsiChar; ABufferLengthChars: Cardinal): PAnsiChar;
{$IFNDEF Use32BitAsm}
const
  MaxDigits = 16;
var
  LDigitBuffer: array[0..MaxDigits - 1] of AnsiChar;
  LCount: Cardinal;
  LIndex: Cardinal;
  LDigit: NativeUInt;
  LNum: NativeUInt;
begin
  {Generate the digits in the local buffer}
  LNum := ANum;
  LCount := 0;
  repeat
    LDigit := LNum;
    LNum := LNum shr 4 {div 16};
    LDigit := LDigit - (LNum shl 4) { * 16};
    Inc(LCount);
    LIndex := MaxDigits - LCount;
    LDigitBuffer[LIndex] := HexTable[LDigit];
  until (LNum = 0) or (LIndex = 0);
  {Copy the digits to the output buffer and advance it}
  if LCount < ABufferLengthChars then
  begin
    System.Move(LDigitBuffer[LIndex], APBuffer^, LCount*SizeOf(LDigitBuffer[0]));
    Result := APBuffer + LCount;
  end else
  begin
    Result := APBuffer;
    Result^ := #0;
  end;
end;
{$ELSE}
assembler;
asm
  {On entry:
    eax = ANum
    edx = ABuffer
    ecx = ABufferLengthChars}

  {todo: implement ecx(ABufferLengthChars) checking}

  push ebx
  push edi
  {Save ANum in ebx}
  mov ebx, eax
  {Get a pointer to the first character in edi}
  mov edi, edx
  {Get the number in ecx as well}
  mov ecx, eax
  {Keep the low nibbles in ebx and the high nibbles in ecx}
  and ebx, $0f0f0f0f
  and ecx, $f0f0f0f0
  {Swap the bytes into the right order}
  ror ebx, 16
  ror ecx, 20
  {Get nibble 7}
  movzx eax, ch
  mov dl, ch
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 6}
  movzx eax, bh
  or dl, bh
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 5}
  movzx eax, cl
  or dl, cl
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 4}
  movzx eax, bl
  or dl, bl
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Rotate ecx and ebx so we get access to the rest}
  shr ebx, 16
  shr ecx, 16
  {Get nibble 3}
  movzx eax, ch
  or dl, ch
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 2}
  movzx eax, bh
  or dl, bh
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 1}
  movzx eax, cl
  or dl, cl
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 0}
  movzx eax, bl
  movzx eax, byte ptr HexTable[eax]
  mov [edi], al
  {Return a pointer to the end of the string}
  lea eax, [edi + 1]
  {Restore registers}
  pop edi
  pop ebx
end;
{$ENDIF}

{Appends the source text to the destination and returns the new destination
 position}
function AppendStringToBuffer(const ASource, ADestination: PAnsiChar;
  ASourceLengthChars, ADestinationBufferLengthChars: Cardinal): PAnsiChar;
begin
  Result := ADestination;
  if ASourceLengthChars > 0 then
  begin
    if (ASourceLengthChars <= ADestinationBufferLengthChars) and
       (ASourceLengthChars < MaxInt div SizeOf(ASource[0])) and
       (ADestinationBufferLengthChars < MaxInt div SizeOf(ASource[0])) then
    begin
      System.Move(ASource^, ADestination^, ASourceLengthChars*SizeOf(ASource[0]));
      Result := ADestination;
      Inc(Result, ASourceLengthChars);
    end else
    begin
      Result^ := #0;
    end;
  end else
  begin
    Result^ := #0;
  end;
end;

{$IFDEF EnableMemoryLeakReportingUsesQualifiedClassName}
type
  PClassData = ^TClassData;
  TClassData = record
    ClassType: TClass;
    ParentInfo: Pointer;
    PropCount: SmallInt;
    UnitName: ShortString;
  end;
{$ENDIF EnableMemoryLeakReportingUsesQualifiedClassName}

{Appends the name of the class to the destination buffer and returns the new
 destination position}
function AppendClassNameToBuffer(AClass: TClass; ADestination: PAnsiChar; ADestinationBufferLengthChars: Cardinal): PAnsiChar;
var
{$IFDEF EnableMemoryLeakReportingUsesQualifiedClassName}
  FirstUnitNameChar: PAnsiChar;
  LClassInfo: Pointer;
  LClassInfoPByte: PByte;
  LClassInfoByte1: Byte;
  LPUnitName: PShortString;
  LClassData: PClassData;
{$ENDIF EnableMemoryLeakReportingUsesQualifiedClassName}
  LPClassName: PShortString;
begin
  {Get a pointer to the class name}
  if AClass <> nil then
  begin
    Result := ADestination;
{$IFDEF EnableMemoryLeakReportingUsesQualifiedClassName}
    // based on TObject.UnitScope
    LClassInfo := AClass.ClassInfo;
    if LClassInfo <> nil then // prepend the UnitName
    begin
      LClassInfoPByte := LClassInfo;
      LClassInfoByte1 := {$IFDEF PByteIsPAnsiChar}Byte{$ENDIF}(PByte(LClassInfoPByte + 1)^);
      Inc(LClassInfoPByte, 2);
      Inc(LClassInfoPByte, LClassInfoByte1);
      LClassData := PClassData(LClassInfoPByte);
      LPUnitName := @(LClassData^.UnitName);
      FirstUnitNameChar := @(LPUnitName^[1]);
      if FirstUnitNameChar^ <> '@' then
        Result := AppendStringToBuffer(FirstUnitNameChar, Result, Length(LPUnitName^), ADestinationBufferLengthChars)
      else // Pos does no memory allocations, so it is safe to use
      begin // Skip the '@', then copy until the ':' - never seen this happen in Delphi, but might be a C++ thing
        Result := AppendStringToBuffer(@(LPUnitName^[2]), Result, Pos(ShortString(':'), LPUnitName^) - 2, ADestinationBufferLengthChars);
      end;
      // dot between unit name and class name:
      Result := AppendStringToBuffer('.', Result, Length('.'), ADestinationBufferLengthChars);
    end;
{$ENDIF EnableMemoryLeakReportingUsesQualifiedClassName}
    LPClassName := PShortString(PPointer(PByte(AClass) + vmtClassName)^);
    {Append the class name}
    Result := AppendStringToBuffer(@LPClassName^[1], Result, Length(LPClassName^), ADestinationBufferLengthChars);
  end
  else
  begin
    Result := AppendStringToBuffer(UnknownClassNameMsg, ADestination, Length(UnknownClassNameMsg), ADestinationBufferLengthChars);
  end;
end;

{Shows a message box if the program is not showing one already.}
procedure ShowMessageBox(AText, ACaption: PAnsiChar);
begin
  if (not ShowingMessageBox) and (not SuppressMessageBoxes) then
  begin
    ShowingMessageBox := True;
    MessageBoxA(0, AText, ACaption,
      MB_OK or MB_ICONERROR or MB_TASKMODAL or MB_DEFAULT_DESKTOP_ONLY);
    ShowingMessageBox := False;
  end;
end;

{Returns the class for a memory block. Returns nil if it is not a valid class}
function DetectClassInstance(APointer: Pointer): TClass;
{$IFDEF VmtSupported}
var
  LMemInfo: TMemoryBasicInformation;

  {Checks whether the given address is a valid address for a VMT entry.}
  function IsValidVMTAddress(APAddress: Pointer): Boolean;
  begin
    {Do some basic pointer checks: Must be dword aligned and beyond 64K}
    if (UIntPtr(APAddress) > 65535)
      and ((UIntPtr(APAddress) and 3) = 0) then
    begin
      {Do we need to recheck the virtual memory?}
      if (UIntPtr(LMemInfo.BaseAddress) > UIntPtr(APAddress))
        or ((UIntPtr(LMemInfo.BaseAddress) + LMemInfo.RegionSize) < (UIntPtr(APAddress) + 4)) then
      begin
        {Get the VM status for the pointer}
        LMemInfo.RegionSize := 0;
        VirtualQuery(APAddress,  LMemInfo, SizeOf(LMemInfo));
      end;
      {Check the readability of the memory address}
      Result := (LMemInfo.RegionSize >= 4)
        and (LMemInfo.State = MEM_COMMIT)
        and ((LMemInfo.Protect and (PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY)) <> 0)
        and ((LMemInfo.Protect and PAGE_GUARD) = 0);
    end
    else
      Result := False;
  end;

  {Returns true if AClassPointer points to a class VMT}
  function InternalIsValidClass(AClassPointer: Pointer; ADepth: Integer = 0): Boolean;
  var
    LParentClassSelfPointer: PPointer;
  begin
    {Check that the self pointer as well as parent class self pointer addresses
     are valid}
    if (ADepth < 1000)
      and IsValidVMTAddress(Pointer(PByte(AClassPointer) + vmtSelfPtr))
      and IsValidVMTAddress(Pointer(PByte(AClassPointer) + vmtParent)) then
    begin
      {Get a pointer to the parent class' self pointer}
      LParentClassSelfPointer := PPointer(PByte(AClassPointer) + vmtParent)^;
      {Check that the self pointer as well as the parent class is valid}
      Result := (PPointer(PByte(AClassPointer) + vmtSelfPtr)^ = AClassPointer)
        and ((LParentClassSelfPointer = nil)
          or (IsValidVMTAddress(LParentClassSelfPointer)
            and InternalIsValidClass(LParentClassSelfPointer^, ADepth + 1)));
    end
    else
      Result := False;
  end;

begin
  {Get the class pointer from the (suspected) object}
  Result := TClass(PPointer(APointer)^);
  {No VM info yet}
  LMemInfo.RegionSize := 0;
  {Check the block}
  if (not InternalIsValidClass(Pointer(Result), 0))
{$IFDEF FullDebugMode}
    or (Pointer(Result) = @(FreedObjectVMT.VMTMethods[0]))
{$ENDIF}
  then
    Result := nil;
end;
{$ELSE VmtSupported}
begin
  {Not currently supported under Linux / OS X}
  Result := nil;
end;
{$ENDIF VmtSupported}

{Gets the available size inside a block}
function GetAvailableSpaceInBlock(APointer: Pointer): NativeUInt;
var
  LBlockHeader: NativeUInt;
  LPSmallBlockPool: PSmallBlockPoolHeader;
begin
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  if (LBlockHeader and (IsMediumBlockFlag or IsLargeBlockFlag)) = 0 then
  begin
    LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader and DropSmallFlagsMask);
    Result := LPSmallBlockPool^.BlockType^.BlockSize - BlockHeaderSize;
  end
  else
  begin
    Result := (LBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
    if (LBlockHeader and IsMediumBlockFlag) = 0 then
      Dec(Result, LargeBlockHeaderSize);
  end;
end;

{-----------------Small Block Management------------------}

{Locks all small block types}
procedure LockAllSmallBlockTypes;
var
  LIndC: Cardinal;
begin
  {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
    for LIndC := 0 to NumSmallBlockTypes - 1 do
    begin
      while not AcquireLockByte(SmallBlockTypes[LIndC].SmallBlockTypeLocked) do
      begin
{$IFDEF NeverSleepOnThreadContention}
  {$IFDEF UseSwitchToThread}
        SwitchToThreadIfSupported;
  {$ENDIF}
{$ELSE}
        Sleep(InitialSleepTime);
        if AcquireLockByte(SmallBlockTypes[LIndC].SmallBlockTypeLocked) then
          Break;
        Sleep(AdditionalSleepTime);
{$ENDIF}
      end;
    end;
  end;
end;

{Gets the first and last block pointer for a small block pool}
procedure GetFirstAndLastSmallBlockInPool(APSmallBlockPool: PSmallBlockPoolHeader;
  var AFirstPtr, ALastPtr: Pointer);
var
  LBlockSize: NativeUInt;
begin
  {Get the pointer to the first block}
  AFirstPtr := Pointer(PByte(APSmallBlockPool) + SmallBlockPoolHeaderSize);
  {Get a pointer to the last block}
  if (APSmallBlockPool^.BlockType^.CurrentSequentialFeedPool <> APSmallBlockPool)
    or (UIntPtr(APSmallBlockPool^.BlockType^.NextSequentialFeedBlockAddress) > UIntPtr(APSmallBlockPool^.BlockType^.MaxSequentialFeedBlockAddress)) then
  begin
    {Not the sequential feed - point to the end of the block}
    LBlockSize := PNativeUInt(PByte(APSmallBlockPool) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
    ALastPtr := Pointer(PByte(APSmallBlockPool) + LBlockSize - APSmallBlockPool^.BlockType^.BlockSize);
  end
  else
  begin
    {The sequential feed pool - point to before the next sequential feed block}
    ALastPtr := Pointer(PByte(APSmallBlockPool^.BlockType^.NextSequentialFeedBlockAddress) - 1);
  end;
end;

{-----------------Medium Block Management------------------}

{Advances to the next medium block. Returns nil if the end of the medium block
 pool has been reached}
function NextMediumBlock(APMediumBlock: Pointer): Pointer;
var
  LBlockSize: NativeUInt;
begin
  {Get the size of this block}
  LBlockSize := PNativeUInt(PByte(APMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  {Advance the pointer}
  Result := Pointer(PByte(APMediumBlock) + LBlockSize);
  {Is the next block the end of medium pool marker?}
  LBlockSize := PNativeUInt(PByte(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  if LBlockSize = 0 then
    Result := nil;
end;

{Gets the first medium block in the medium block pool}
function GetFirstMediumBlockInPool(APMediumBlockPoolHeader: PMediumBlockPoolHeader): Pointer;
begin
  if (MediumSequentialFeedBytesLeft = 0)
    or (UIntPtr(LastSequentiallyFedMediumBlock) < UIntPtr(APMediumBlockPoolHeader))
    or (UIntPtr(LastSequentiallyFedMediumBlock) > UIntPtr(APMediumBlockPoolHeader) + MediumBlockPoolSize) then
  begin
    Result := Pointer(PByte(APMediumBlockPoolHeader) + MediumBlockPoolHeaderSize);
  end
  else
  begin
    {Is the sequential feed pool empty?}
    if MediumSequentialFeedBytesLeft <> MediumBlockPoolSize - MediumBlockPoolHeaderSize then
      Result := LastSequentiallyFedMediumBlock
    else
      Result := nil;
  end;
end;



{$IFDEF Use32BitAsm}
  {$IFNDEF MediumBlocksLockedCriticalSection}
    {$define UseOriginalFastMM4_LockMediumBlocksAsm}
  {$ENDIF}
{$ENDIF}

{$IFDEF XE2AndUp}
  {$define UseSystemAtomicIntrinsics}
{$ENDIF}


{$IFDEF DisablePauseAndSwitchToThread}
const
  CpuFeaturePauseAndSwitch = False;
{$ELSE}
{$IFDEF AssumePauseAndSwitchToThreadAvailable}
const
  CpuFeaturePauseAndSwitch = True;
{$ELSE}
function CpuFeaturePauseAndSwitch: Boolean; {$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}
begin
  {$IFDEF USE_CPUID}
  Result := FastMMCpuFeaturesA and FastMMCpuFeaturePauseAndSwitch <> 0
  {$ELSE}
  Result := False;
  {$ENDIF}
end;
{$ENDIF}
{$ENDIF DisablePauseAndSwitchToThread}


{Locks the medium blocks. Note that the assembler version is assumed to preserve all volatile registers except eax for 32-bit Assembly / rax for 64-bit Assembly).}

{$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm}

function LockMediumBlocks({$IFDEF UseReleaseStack}APointer: Pointer = nil; APDelayRelease: PBoolean = nil{$ENDIF}): Boolean; // returns true if was contention

  {$IFDEF MediumBlocksLockedCriticalSection}
    {$IFNDEF DEBUG}{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}{$ENDIF}
  {$ENDIF}

{$IFDEF UseReleaseStack}
var
  LPReleaseStack: ^TLFStack;
{$ENDIF}
begin
  Result := False;
  {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
  {$IFDEF MediumBlocksLockedCriticalSection}
    {$IFNDEF DisablePauseAndSwitchToThread}
    if CpuFeaturePauseAndSwitch then
    begin
      if not AcquireLockByte(MediumBlocksLocked) then
      begin
        Result := True;
        AcquireSpinLockByte(MediumBlocksLocked);
      end;
    end else
    {$ENDIF}
    begin
      EnterCriticalSection(MediumBlocksLockedCS);
    end
  {$ELSE MediumBlocksLockedCriticalSection}
    while not AcquireLockByte(MediumBlocksLocked) do
    begin
      Result := True; // had contention
  {$IFDEF UseReleaseStack}
      if Assigned(APointer) then
      begin
         LPReleaseStack := @(MediumReleaseStack[GetStackSlot]);
         if (not LPReleaseStack^.IsFull) and LPReleaseStack.Push(APointer) then
         begin
           APointer := nil;
           APDelayRelease^ := True;
           Exit;
         end;
      end;
  {$ENDIF}
  {$IFDEF NeverSleepOnThreadContention}
  {$IFDEF UseSwitchToThread}
      SwitchToThreadIfSupported;
  {$ENDIF}
  {$ELSE}
      Sleep(InitialSleepTime);
      if AcquireLockByte(MediumBlocksLocked) then
        Break;
      Sleep(AdditionalSleepTime);
  {$ENDIF}
    end;
    {$IFDEF UseReleaseStack}
    if Assigned(APDelayRelease) then
      APDelayRelease^ := False;
  {$ENDIF}
  {$ENDIF MediumBlocksLockedCriticalSection}
  end;
end;

{$ELSE UseOriginalFastMM4_LockMediumBlocksAsm}

{ This is the original "LockMediumBlocks" assembly implementation that uses a
loop of Sleep() or SwitchToThread() as opposing to an efficient approach of FastMM4-AVX. }

procedure LockMediumBlocks;
asm
{ This implemenation will not be compiled into FastMM4-AVX unless you
  undefine the MediumBlocksLockedCriticalSection. You may only need
  this implementation if you would like to use the old locking mechanism of
  the original FastMM4 }

  {Note: This routine is assumed to preserve all volatile registers except eax for 32-bit/ rax for 64-bit Assembly}
@MediumBlockLockLoop:
  mov     eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  {Attempt to lock the medium blocks}
  lock    cmpxchg MediumBlocksLocked, ah  // cmpxchg also uses AL as an implicit operand
  je      @DoneNoContention
{$IFDEF NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  db      $F3, $90 // pause
  {$IFDEF UseSwitchToThread}
  push    ecx
  push    edx
  call    SwitchToThreadIfSupported
  pop     edx
  pop     ecx
  {$ENDIF}
  {Try again}
  jmp     @MediumBlockLockLoop
{$ELSE NeverSleepOnThreadContention}
  {Couldn't lock the medium blocks - sleep and try again}
  push    ecx
  push    edx
  push    InitialSleepTime
  call    Sleep
  pop     edx
  pop     ecx
  {Try again}
  mov     eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  {Attempt to grab the block type}
  lock    cmpxchg MediumBlocksLocked, ah  // cmpxchg also uses AL as an implicit operand
  je      @DoneWithContention
  {Couldn't lock the medium blocks - sleep and try again}
  push    ecx
  push    edx
  push    AdditionalSleepTime
  call    Sleep
  pop     edx
  pop     ecx
  {Try again}
  jmp     @MediumBlockLockLoop
{$ENDIF NeverSleepOnThreadContention}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DoneNoContention:
  xor     eax, eax
  jmp     @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DoneWithContention:
  mov     eax, 1
@Done:
end;
{$ENDIF UseOriginalFastMM4_LockMediumBlocksAsm}

procedure UnlockMediumBlocks;
  {$IFNDEF DEBUG}{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}{$ENDIF}
begin
  {$IFDEF MediumBlocksLockedCriticalSection}
  if CpuFeaturePauseAndSwitch then
  begin
    ReleaseLockByte(MediumBlocksLocked);
  end else
  begin
    LeaveCriticalSection(MediumBlocksLockedCS);
  end;
  {$ELSE}
  ReleaseLockByte(MediumBlocksLocked);
  {$ENDIF}
end;



{Removes a medium block from the circular linked list of free blocks.
 Does not change any header flags. Medium blocks should be locked
 before calling this procedure.}
procedure RemoveMediumFreeBlock(APMediumFreeBlock: PMediumFreeBlock);
{$IFNDEF ASMVersion}
var
  LMask: Cardinal;
  LShift: Byte;
  LPreviousFreeBlock,
  LNextFreeBlock: PMediumFreeBlock;
  LBinNumber,
  LBinGroupNumber: Cardinal;
begin
  {Get the current previous and next blocks}
  LNextFreeBlock := APMediumFreeBlock^.NextFreeBlock;
  LPreviousFreeBlock := APMediumFreeBlock^.PreviousFreeBlock;
  {Remove this block from the linked list}
  LPreviousFreeBlock^.NextFreeBlock := LNextFreeBlock;
  LNextFreeBlock^.PreviousFreeBlock := LPreviousFreeBlock;
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  if LPreviousFreeBlock = LNextFreeBlock then
  begin
    {Get the bin number for this block size}
    LBinNumber := (UIntPtr(LNextFreeBlock) - UIntPtr(@MediumBlockBins)) shr MediumFreeBlockSizePowerOf2;
    LBinGroupNumber := LBinNumber shr MediumBlockBinsPerGroupPowerOf2;
    {Flag this bin as empty}
    LShift := LBinNumber and (MediumBlockBinsPerGroup-1);
    LMask := not (Cardinal(UnsignedBit) shl LShift);
    MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber] and LMask;
    {Is the group now entirely empty?}
    if MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
    begin
      LMask := not (Cardinal(UnsignedBit) shl LBinGroupNumber);

      {Flag this group as empty}
      MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap and LMask;
    end;
  end;
end;
{$ELSE}
{$IFDEF 32Bit}
assembler;
asm
  {On entry: eax = APMediumFreeBlock}
  {Get the current previous and next blocks}
  mov ecx, TMediumFreeBlock[eax].NextFreeBlock
  mov edx, TMediumFreeBlock[eax].PreviousFreeBlock
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  cmp ecx, edx
  {Remove this block from the linked list}
  mov TMediumFreeBlock[ecx].PreviousFreeBlock, edx
  mov TMediumFreeBlock[edx].NextFreeBlock, ecx
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  je @BinIsNowEmpty
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@Done:
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@BinIsNowEmpty:
  {Get the bin number for this block size in ecx}
  sub ecx, offset MediumBlockBins
  mov edx, ecx
  shr ecx, MediumFreeBlockSizePowerOf2
  {Get the group number in edx}
  movzx edx, dh
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  jnz @Done
  {Flag this group as empty}
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and MediumBlockBinGroupBitmap, eax
@Exit:
end;
{$ELSE}
assembler;
asm
{$IFDEF AllowAsmNoframe}
  .noframe
{$ENDIF}
  {On entry: rcx = APMediumFreeBlock}
  mov rax, rcx
  {Get the current previous and next blocks}
  mov rcx, TMediumFreeBlock[rax].NextFreeBlock
  mov rdx, TMediumFreeBlock[rax].PreviousFreeBlock
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  cmp rcx, rdx
  {Remove this block from the linked list}
  mov TMediumFreeBlock[rcx].PreviousFreeBlock, rdx
  mov TMediumFreeBlock[rdx].NextFreeBlock, rcx
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  jne @Done
  {Get the bin number for this block size in rcx}
  lea r8, MediumBlockBins
  sub rcx, r8
  mov edx, ecx
  shr ecx, MediumFreeBlockSizePowerOf2
  {Get the group number in edx}
  shr edx, 9
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  lea r8, MediumBlockBinBitmaps
  and dword ptr [r8 + rdx * 4], eax
  jnz @Done
  {Flag this group as empty}
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and MediumBlockBinGroupBitmap, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@Done:
end;
{$ENDIF}
{$ENDIF}

{Inserts a medium block into the appropriate medium block bin.}
procedure InsertMediumBlockIntoBin(APMediumFreeBlock: PMediumFreeBlock; AMediumBlockSize: Cardinal);
{$IFNDEF ASMVersion}
var
  LMask: Cardinal;
  LShift: Byte;
  LBinNumber,
  LBinGroupNumber: Cardinal;
  LPBin,
  LPFirstFreeBlock: PMediumFreeBlock;
begin
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  LBinNumber := (AMediumBlockSize - MinimumMediumBlockSize) shr MediumBlockGranularityPowerOf2;
  if LBinNumber >= MediumBlockBinCount then
    LBinNumber := MediumBlockBinCount - 1;
  {Get the bin}
  LPBin := @(MediumBlockBins[LBinNumber]);
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  LPFirstFreeBlock := LPBin^.NextFreeBlock;
  APMediumFreeBlock^.PreviousFreeBlock := LPBin;
  APMediumFreeBlock^.NextFreeBlock := LPFirstFreeBlock;
  LPFirstFreeBlock^.PreviousFreeBlock := APMediumFreeBlock;
  LPBin^.NextFreeBlock := APMediumFreeBlock;
  {Was this bin empty?}
  if LPFirstFreeBlock = LPBin then
  begin
    {Get the group number}
    LBinGroupNumber := LBinNumber shr MediumBlockBinsPerGroupPowerOf2;
    LShift := LBinNumber and (MediumBlockBinsPerGroup-1); // We need a separate variable LShift to avoid range check error
    LMask := Cardinal(UnsignedBit) shl LShift;
    {Flag this bin as used}
    MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber] or LMask;
    LMask := Cardinal(UnsignedBit) shl LBinGroupNumber;
    {Flag the group as used}
    MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap or LMask;
  end;
end;
{$ELSE}
{$IFDEF 32Bit}
assembler;
asm
  {On entry: eax = APMediumFreeBlock, edx = AMediumBlockSize}
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  {Validate the bin number}
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  {Get the bin in ecx}
  lea ecx, [MediumBlockBins + edx * 8]
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  mov edx, TMediumFreeBlock[ecx].NextFreeBlock
  {Was this bin empty?}
  cmp edx, ecx
  mov TMediumFreeBlock[eax].PreviousFreeBlock, ecx
  mov TMediumFreeBlock[eax].NextFreeBlock, edx
  mov TMediumFreeBlock[edx].PreviousFreeBlock, eax
  mov TMediumFreeBlock[ecx].NextFreeBlock, eax
  {Was this bin empty?}
  je @BinWasEmpty
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@BinWasEmpty:
  {Get the bin number in ecx}
  sub ecx, offset MediumBlockBins
  mov edx, ecx
  shr ecx, 3
  {Get the group number in edx}
  movzx edx, dh
  {Flag this bin as not empty}
  mov eax, 1
  shl eax, cl
  or dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  {Flag the group as not empty}
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or MediumBlockBinGroupBitmap, eax
@Exit:
end;
{$ELSE}
assembler;
asm
{$IFDEF AllowAsmNoframe}
  .noframe
{$ENDIF}
  {On entry: rax = APMediumFreeBlock, edx = AMediumBlockSize}
  mov rax, rcx
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  {Validate the bin number}
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  mov r9, rdx
  {Get the bin address in rcx}
  lea rcx, MediumBlockBins
  shl edx, 4
  add rcx, rdx
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  mov rdx, TMediumFreeBlock[rcx].NextFreeBlock
  {Was this bin empty?}
  cmp rdx, rcx
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rcx
  mov TMediumFreeBlock[rax].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rax
  mov TMediumFreeBlock[rcx].NextFreeBlock, rax
  {Was this bin empty?}
  jne @Done
  {Get the bin number in ecx}
  mov rcx, r9
  {Get the group number in edx}
  mov rdx, r9
  shr edx, 5
  {Flag this bin as not empty}
  mov eax, 1
  shl eax, cl
  lea r8, MediumBlockBinBitmaps
  or dword ptr [r8 + rdx * 4], eax
  {Flag the group as not empty}
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or MediumBlockBinGroupBitmap, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@Done:
end;
{$ENDIF}
{$ENDIF}

{Bins what remains in the current sequential feed medium block pool. Medium
 blocks must be locked.}
procedure BinMediumSequentialFeedRemainder;
{$IFNDEF ASMVersion}
var
  LSequentialFeedFreeSize,
  LNextBlockSizeAndFlags: NativeUInt;
  LPRemainderBlock,
  LNextMediumBlock: Pointer;
begin
  LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
  if LSequentialFeedFreeSize > 0 then
  begin
    {Get the block after the open space}
    LNextMediumBlock := LastSequentiallyFedMediumBlock;
    LNextBlockSizeAndFlags := PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^;
    {Point to the remainder}
    LPRemainderBlock := Pointer(PByte(LNextMediumBlock) - LSequentialFeedFreeSize);
{$IFNDEF FullDebugMode}
    {Can the next block be combined with the remainder?}
    if (LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
    begin
      {Increase the size of this block}
      Inc(LSequentialFeedFreeSize, LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask);
      {Remove the next block as well}
      if (LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask) >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LNextMediumBlock);
    end
    else
    begin
{$ENDIF}
      {Set the "previous block is free" flag of the next block}
      PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^ := LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
{$IFNDEF FullDebugMode}
    end;
{$ENDIF}
    {Store the size of the block as well as the flags}
    PNativeUInt(PByte(LPRemainderBlock) - BlockHeaderSize)^ := LSequentialFeedFreeSize or IsMediumBlockFlag or IsFreeBlockFlag;
    {Store the trailing size marker}
    PNativeUInt(PByte(LPRemainderBlock) + LSequentialFeedFreeSize - BlockHeaderSize * 2)^ := LSequentialFeedFreeSize;
{$IFDEF FullDebugMode}
    {In full debug mode the sequential feed remainder will never be too small to
     fit a full debug header.}
    {Clear the user area of the block}
    DebugFillMem(Pointer(PByte(LPRemainderBlock) + SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt))^,
      LSequentialFeedFreeSize - FullDebugBlockOverhead - SizeOf(NativeUInt),
      {$IFNDEF CatchUseOfFreedInterfaces}NativeUInt(DebugFillPattern){$ELSE}NativeUInt(@VMTBadInterface){$ENDIF});
    {We need to set a valid debug header and footer in the remainder}
    PFullDebugBlockHeader(LPRemainderBlock)^.HeaderCheckSum := NativeUInt(LPRemainderBlock);
    PNativeUInt(PByte(LPRemainderBlock) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(LPRemainderBlock);
{$ENDIF}
    {Bin this medium block}
    if LSequentialFeedFreeSize >= MinimumMediumBlockSize then
    begin
      InsertMediumBlockIntoBin(LPRemainderBlock, LSequentialFeedFreeSize);
    end;
  end;
end;
{$ELSE}
{$IFDEF 32Bit}
assembler;
asm
  cmp MediumSequentialFeedBytesLeft, 0
  jne @MustBinMedium
  {Nothing to bin}
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MustBinMedium:
  {Get a pointer to the last sequentially allocated medium block}
  mov eax, LastSequentiallyFedMediumBlock
  {Is the block that was last fed sequentially free?}
  test byte ptr [eax - BlockHeaderSize], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  {Set the "previous block is free" flag in the last block fed}
  or dword ptr [eax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  {Get the remainder in edx}
  mov edx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub eax, edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@BinTheRemainder:
  {Status: eax = start of remainder, edx = size of remainder}
  {Store the size of the block as well as the flags}
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - BlockHeaderSize], ecx
  {Store the trailing size marker}
  mov [eax + edx - BlockHeaderSize * 2], edx
  {Bin this medium block}
  cmp edx, MinimumMediumBlockSize
  jnb InsertMediumBlockIntoBin
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@LastBlockFedIsFree:
  {Drop the flags}
  mov edx, DropMediumAndLargeFlagsMask
  and edx, [eax - BlockHeaderSize]
  {Free the last block fed}
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  {Last fed block is free - remove it from its size bin}
  call RemoveMediumFreeBlock
  {Re-read eax and edx}
  mov eax, LastSequentiallyFedMediumBlock
  mov edx, DropMediumAndLargeFlagsMask
  and edx, [eax - BlockHeaderSize]
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontRemoveLastFed:
  {Get the number of bytes left in ecx}
  mov ecx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub eax, ecx
  {edx = total size of the remainder}
  add edx, ecx
  jmp @BinTheRemainder
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@Done:
end;
{$ELSE}
assembler;
asm
  {Don't put ".noframe" here because this function calls other functions, e.g.
  "InsertMediumBlockIntoBin", "RemoveMediumFreeBlock", etc.
  According to the documentation at
  http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Assembly_Procedures_and_Functions
  ".noframe: forcibly disables the generation of a stack frame as long as there
  are no local variables declared and the parameter count <= 4.
  Thus, ".noframe" can only be used for leaf functions. A leaf function is one
  that does not call another function. That is one that is always at the bottom
  of the call tree.}
  {$IFDEF AllowAsmParams}
  .params 2
  {$ENDIF}
  xor eax, eax
  cmp MediumSequentialFeedBytesLeft, eax
  je @Done
  {Get a pointer to the last sequentially allocated medium block}
  mov rax, LastSequentiallyFedMediumBlock
  {Is the block that was last fed sequentially free?}
  test byte ptr [rax - BlockHeaderSize], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  {Set the "previous block is free" flag in the last block fed}
  or qword ptr [rax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  {Get the remainder in edx}
  mov edx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub rax, rdx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@BinTheRemainder:
  {Status: rax = start of remainder, edx = size of remainder}
  {Store the size of the block as well as the flags}
  lea rcx, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rax - BlockHeaderSize], rcx
  {Store the trailing size marker}
  mov [rax + rdx - 2 * BlockHeaderSize], rdx
  {Bin this medium block}
  cmp edx, MinimumMediumBlockSize
  jb @Done
  mov rcx, rax
  call InsertMediumBlockIntoBin
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@LastBlockFedIsFree:
  {Drop the flags}
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
  {Free the last block fed}
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  {Last fed block is free - remove it from its size bin}
  mov rcx, rax
  call RemoveMediumFreeBlock
  {Re-read rax and rdx}
  mov rax, LastSequentiallyFedMediumBlock
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontRemoveLastFed:
  {Get the number of bytes left in ecx}
  mov ecx, MediumSequentialFeedBytesLeft
  {Point rax to the start of the remainder}
  sub rax, rcx
  {edx = total size of the remainder}
  add edx, ecx
  jmp @BinTheRemainder
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@Done:
end;
{$ENDIF}
{$ENDIF}

{Allocates a new sequential feed medium block pool and immediately splits off a
 block of the requested size. The block size must be a multiple of 16 and
 medium blocks must be locked.}
function AllocNewSequentialFeedMediumPool(AFirstBlockSize: Cardinal): Pointer;
var
  LOldFirstMediumBlockPool: PMediumBlockPoolHeader;
  LNewPool: Pointer;
begin
  {Bin the current sequential feed remainder}
  BinMediumSequentialFeedRemainder;
  {Allocate a new sequential feed block pool}
  LNewPool := kmem_alloc(MediumBlockPoolSize,VM_RW);
  if LNewPool <> nil then
  begin
    {Insert this block pool into the list of block pools}
    LOldFirstMediumBlockPool := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(LNewPool)^.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
    MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := LNewPool;
    PMediumBlockPoolHeader(LNewPool)^.NextMediumBlockPoolHeader := LOldFirstMediumBlockPool;
    LOldFirstMediumBlockPool^.PreviousMediumBlockPoolHeader := LNewPool;
    {Store the sequential feed pool trailer}
    PNativeUInt(PByte(LNewPool) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    {Get the number of bytes still available}
    MediumSequentialFeedBytesLeft := (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - AFirstBlockSize;
    {Get the result}
    Result := Pointer(PByte(LNewPool) + MediumBlockPoolSize - AFirstBlockSize);
    LastSequentiallyFedMediumBlock := Result;
    {Store the block header}
    PNativeUInt(PByte(Result) - BlockHeaderSize)^ := AFirstBlockSize or IsMediumBlockFlag;
  end
  else
  begin
    {Out of memory}
    MediumSequentialFeedBytesLeft := 0;
    Result := nil;
  end;
end;

{-----------------Large Block Management------------------}


{Locks the large blocks}
function LockLargeBlocks({$IFDEF UseReleaseStack}APointer: Pointer = nil; APDelayRelease: PBoolean = nil{$ENDIF}): Boolean; // returns true if there was contention

{$IFDEF LargeBlocksLockedCriticalSection}
{$IFNDEF DEBUG}{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}{$ENDIF}
{$ENDIF}


{$IFDEF UseReleaseStack}
var
  LPReleaseStack: ^TLFStack;
{$ENDIF}
begin
  Result := False;
  {Lock the large blocks}

{$IFNDEF AssumeMultiThreaded}
{$IFDEF FullDebugMode}
  if not IsMultiThread then
  begin
    {The checks for IsMultiThread should be from outsize}
    {$IFNDEF SystemRunError}
       System.Error(reInvalidOp);
    {$ELSE}
       System.RunError(reInvalidOp);
    {$ENDIF}
  end;
{$ENDIF}
{$ENDIF}

{$IFDEF LargeBlocksLockedCriticalSection}
  {$IFNDEF DisablePauseAndSwitchToThread}
  if CpuFeaturePauseAndSwitch then
  begin
    if not AcquireLockByte(LargeBlocksLocked) then
    begin
      Result := True;
      AcquireSpinLockByte(LargeBlocksLocked);
    end;
  end else
  {$ENDIF}
  begin
    EnterCriticalSection(LargeBlocksLockedCS);
  end;
{$ELSE LargeBlocksLockedCriticalSection}
  while not AcquireLockByte(LargeBlocksLocked) do
  begin
    Result := True;
{$IFDEF UseReleaseStack}
    if Assigned(APointer) then
    begin
       LPReleaseStack := @LargeReleaseStack[GetStackSlot];
       if (not LPReleaseStack^.IsFull) and LPReleaseStack.Push(APointer) then
       begin
         APointer := nil;
         APDelayRelease^ := True;
         Exit;
       end;
    end;
{$ENDIF}
{$IFDEF NeverSleepOnThreadContention}
{$IFDEF UseSwitchToThread}
    SwitchToThreadIfSupported;
{$ENDIF}
{$ELSE}
    Sleep(InitialSleepTime);
    if AcquireLockByte(LargeBlocksLocked) then
      Break;
    Sleep(AdditionalSleepTime);
{$ENDIF}
  end;
{$IFDEF UseReleaseStack}
  if Assigned(APDelayRelease) then
    APDelayRelease^ := False;
{$ENDIF}
{$ENDIF LargeBlocksLockedCriticalSection}
end;

procedure UnlockLargeBlocks;
  {$IFNDEF DEBUG}{$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}{$ENDIF}
begin
  {$IFDEF LargeBlocksLockedCriticalSection}
  if CpuFeaturePauseAndSwitch then
  begin
    ReleaseLockByte(LargeBlocksLocked);
  end else
  begin
    LeaveCriticalSection(LargeBlocksLockedCS);
  end;
  {$ELSE}
  ReleaseLockByte(LargeBlocksLocked);
  {$ENDIF}
end;


{Allocates a Large block of at least ASize (actual size may be larger to
 allow for alignment etc.). ASize must be the actual user requested size. This
 procedure will pad it to the appropriate page boundary and also add the space
 required by the header.}
function AllocateLargeBlock(ASize: NativeUInt {$IFDEF LogLockContention}; var ADidSleep: Boolean{$ENDIF}): Pointer;
var
  LLargeUsedBlockSize: NativeUInt;
  LOldFirstLargeBlock: PLargeBlockHeader;
  {$IFNDEF AssumeMultiThreaded}
  LLockLargeBlocksLocked: Boolean;
  {$ENDIF}
begin
  {$IFNDEF AssumeMultiThreaded}
  LLockLargeBlocksLocked := False;
  {$ENDIF}
  {Pad the block size to include the header and granularity. We also add a
   SizeOf(Pointer) overhead so a huge block size is a multiple of 16 bytes less
   SizeOf(Pointer) (so we can use a single move function for reallocating all
   block types)}
  LLargeUsedBlockSize := (ASize + LargeBlockHeaderSize + LargeBlockGranularity - 1 + BlockHeaderSize)
    and LargeBlockGranularityMask;
  {Get the Large block}
  Result := kmem_alloc(LLargeUsedBlockSize,VM_RW);
  {Set the Large block fields}
  if Result <> nil then
  begin
    {Set the large block size and flags}
    PLargeBlockHeader(Result)^.UserAllocatedSize := ASize;
    PLargeBlockHeader(Result)^.BlockSizeAndFlags := LLargeUsedBlockSize or IsLargeBlockFlag;
  {$IFNDEF AssumeMultiThreaded}
    if IsMultiThread then
  {$ENDIF}
    begin
  {$IFNDEF AssumeMultiThreaded}
      LLockLargeBlocksLocked := True;
  {$ENDIF}
      {$IFDEF LogLockContention}ADidSleep:={$ENDIF}
       {Insert the large block into the linked list of large blocks}
      LockLargeBlocks;
    end;
    LOldFirstLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    PLargeBlockHeader(Result)^.PreviousLargeBlockHeader := @LargeBlocksCircularList;
    LargeBlocksCircularList.NextLargeBlockHeader := Result;
    PLargeBlockHeader(Result)^.NextLargeBlockHeader := LOldFirstLargeBlock;
    LOldFirstLargeBlock^.PreviousLargeBlockHeader := Result;
  {$IFNDEF AssumeMultiThreaded}
    if LLockLargeBlocksLocked then
  {$ENDIF}
    begin
  {$IFNDEF AssumeMultiThreaded}
      // LLockLargeBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
  {$ENDIF}
      UnlockLargeBlocks;
    end;
    {Add the size of the header}
    Inc(PByte(Result), LargeBlockHeaderSize);
{$IFDEF FullDebugMode}
    {Since large blocks are never reused, the user area is not initialized to
     the debug fill pattern, but the debug header and footer must be set.}
    PFullDebugBlockHeader(Result)^.HeaderCheckSum := NativeUInt(Result);
    PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(Result);
{$ENDIF}
  end;
end;

{Frees a large block, returning 0 on success, -1 otherwise}
function FreeLargeBlock(APointer: Pointer
  {$IFDEF UseReleaseStack}; ACleanupOperation: Boolean = False{$ENDIF}): Integer;
var
  LPointer: Pointer;
  LPreviousLargeBlockHeader,
  LNextLargeBlockHeader: PLargeBlockHeader;
{$IFNDEF POSIX}
  LRemainingSize: NativeUInt;
  LCurrentSegment: Pointer;
  LMemInfo: TMemoryBasicInformation;
{$ENDIF}
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
  LStackTrace: TStackTrace;
{$ENDIF}
{$IFDEF UseReleaseStack}
  LDelayRelease: Boolean;
  LPReleaseStack: ^TLFStack;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LLargeBlocksLocked: Boolean;
{$ENDIF}
begin
  LPointer := APointer;
{$IFNDEF AssumeMultiThreaded}
  LLargeBlocksLocked := False;
{$ENDIF}
{$IFDEF ClearLargeBlocksBeforeReturningToOS}
  FillChar(LPointer^,
    (PLargeBlockHeader(PByte(LPointer) - LargeBlockHeaderSize).BlockSizeAndFlags
      and DropMediumAndLargeFlagsMask) - LargeBlockHeaderSize, 0);
{$ENDIF}
  {When running a cleanup operation, large blocks are already locked}
{$IFDEF UseReleaseStack}
  if not ACleanupOperation then
  begin
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
    if IsMultiThread then
{$ENDIF}
    begin
{$IFNDEF AssumeMultiThreaded}
      LLargeBlocksLocked := True;
{$ENDIF}
      {$IFDEF LogLockContention}LDidSleep :={$ENDIF}
      LockLargeBlocks({$IFDEF UseReleaseStack}LPointer, @LDelayRelease{$ENDIF});
    end;
{$IFDEF UseReleaseStack}
    if LDelayRelease then
    begin
      Result := 0;
      Exit;
    end;
  {$IFDEF LogLockContention}
  end
  else
    LDidSleep := False;
  {$ELSE}
  end;
  {$ENDIF}
{$ENDIF}
{$IFDEF LogLockContention}
  if LDidSleep then
  begin
    GetStackTrace(@(LStackTrace[0]), StackTraceDepth, 1);
    LargeBlockCollector.Add(@(LStackTrace[0]), StackTraceDepth);
  end;
{$ENDIF}
{$IFDEF UseReleaseStack}
  repeat
{$ENDIF}
    {Point to the start of the large block}
    LPointer := Pointer(PByte(LPointer) - LargeBlockHeaderSize);
    {Get the previous and next large blocks}
    LPreviousLargeBlockHeader := PLargeBlockHeader(LPointer)^.PreviousLargeBlockHeader;
    LNextLargeBlockHeader := PLargeBlockHeader(LPointer)^.NextLargeBlockHeader;
  {$IFNDEF POSIX}
    {Is the large block segmented?}
    if (PLargeBlockHeader(LPointer)^.BlockSizeAndFlags and LargeBlockIsSegmented) = 0 then
    begin
  {$ENDIF}
      {Single segment large block: Try to free it}
      if VirtualFree(LPointer, 0, MEM_RELEASE) then
        Result := 0
      else
        Result := -1;
  {$IFNDEF POSIX}
    end
    else
    begin
      {The large block is segmented - free all segments}
      LCurrentSegment := LPointer;
      LRemainingSize := PLargeBlockHeader(LPointer)^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
      Result := 0;
      while True do
      begin
        {Get the size of the current segment}
        FillChar(LMemInfo, SizeOf(LMemInfo), 0);
        VirtualQuery(LCurrentSegment, LMemInfo, SizeOf(LMemInfo));
        {Free the segment}
        if not VirtualFree(LCurrentSegment, 0, MEM_RELEASE) then
        begin
          Result := -1;
          Break;
        end;
        {Done?}
        if NativeUInt(LMemInfo.RegionSize) >= LRemainingSize then
          Break;
        {Decrement the remaining size}
        Dec(LRemainingSize, NativeUInt(LMemInfo.RegionSize));
        Inc(PByte(LCurrentSegment), NativeUInt(LMemInfo.RegionSize));
      end;
    end;
  {$ENDIF}
    {Success?}
    if Result = 0 then
    begin
      {Remove the large block from the linked list}
      LNextLargeBlockHeader^.PreviousLargeBlockHeader := LPreviousLargeBlockHeader;
      LPreviousLargeBlockHeader^.NextLargeBlockHeader := LNextLargeBlockHeader;
    end;
{$IFDEF UseReleaseStack}
    if (Result <> 0) or ACleanupOperation then
      Break;
    LPReleaseStack := @LargeReleaseStack[GetStackSlot];
    if LPReleaseStack^.IsEmpty or (not LPReleaseStack.Pop(LPointer)) then
      Break;
  {$IFDEF ClearLargeBlocksBeforeReturningToOS}
    FillChar(LPointer^,
      (PLargeBlockHeader(PByte(LPointer) - LargeBlockHeaderSize).BlockSizeAndFlags
        and DropMediumAndLargeFlagsMask) - LargeBlockHeaderSize, 0);
  {$ENDIF}
  until False;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  if LLargeBlocksLocked then
{$ENDIF}
  begin
    // LLargeBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    {Unlock the large blocks}
    UnlockLargeBlocks;
  end;
end;

{$IFNDEF FullDebugMode}
{Reallocates a large block to at least the requested size. Returns the new
 pointer, or nil on error}
function ReallocateLargeBlock(APointer: Pointer; ANewSize: NativeUInt): Pointer;
var
  LOldAvailableSize,
  LBlockHeader,
  LOldUserSize,
  LMinimumUpsize,
  LNewAllocSize: NativeUInt;
{$IFNDEF POSIX}
  LNewSegmentSize: NativeUInt;
  LNextSegmentPointer: Pointer;
  LMemInfo: TMemoryBasicInformation;
{$ENDIF}
begin
  {Get the block header}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Large block - size is (16 + 4) less than the allocated size}
  LOldAvailableSize := (LBlockHeader and DropMediumAndLargeFlagsMask) - (LargeBlockHeaderSize + BlockHeaderSize);
  {Is it an upsize or a downsize?}
  if ANewSize > LOldAvailableSize then
  begin
    {This pointer is being reallocated to a larger block and therefore it is
     logical to assume that it may be enlarged again. Since reallocations are
     expensive, there is a minimum upsize percentage to avoid unnecessary
     future move operations.}
    {Add 25% for large block upsizes}
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if ANewSize < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize
    else
      LNewAllocSize := ANewSize;
{$IFNDEF POSIX}
    {Can another large block segment be allocated directly after this segment,
     thus negating the need to move the data?}
    LNextSegmentPointer := Pointer(PByte(APointer) - LargeBlockHeaderSize + (LBlockHeader and DropMediumAndLargeFlagsMask));
    FilLChar(LMemInfo, SizeOf(LMemInfo), 0);
    VirtualQuery(LNextSegmentPointer, LMemInfo, SizeOf(LMemInfo));
    if LMemInfo.State = MEM_FREE then
    begin
      {Round the region size to the previous 64K}
      LMemInfo.RegionSize := LMemInfo.RegionSize and LargeBlockGranularityMask;
      {Enough space to grow in place?}
      if NativeUInt(LMemInfo.RegionSize) > (ANewSize - LOldAvailableSize) then
      begin
        {There is enough space after the block to extend it - determine by how
         much}
        LNewSegmentSize := (LNewAllocSize - LOldAvailableSize + LargeBlockGranularity - 1) and LargeBlockGranularityMask;
        if LNewSegmentSize > LMemInfo.RegionSize then
          LNewSegmentSize := LMemInfo.RegionSize;
        {Attempy to reserve the address range (which will fail if another
         thread has just reserved it) and commit it immediately afterwards.}

        if (md_mmap(LNextSegmentPointer,LNewSegmentSize,VM_RW or MD_MAP_FIXED or MD_MAP_RESERVED)<>0) and
           (md_mmap(LNextSegmentPointer,LNewSegmentSize,VM_RW or MD_MAP_FIXED)<>0) then
        begin
          {Update the requested size}
          PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize)^.UserAllocatedSize := ANewSize;
          PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize)^.BlockSizeAndFlags :=
            (PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize)^.BlockSizeAndFlags + LNewSegmentSize)
            or LargeBlockIsSegmented;
          {Success}
          Result := APointer;
          Exit;
        end;
      end;
    end;
{$ENDIF}
    {Could not resize in place: Allocate the new block}
    Result := FastGetMem(LNewAllocSize);
    if Result <> nil then
    begin
      {If it's a large block - store the actual user requested size (it may
       not be if the block that is being reallocated from was previously
       downsized)}
      if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
        PLargeBlockHeader(PByte(Result) - LargeBlockHeaderSize)^.UserAllocatedSize := ANewSize;
      {The user allocated size is stored for large blocks}
      LOldUserSize := PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize)^.UserAllocatedSize;
      {The number of bytes to move is the old user size.}
{$IFDEF UseCustomVariableSizeMoveRoutines}
   {$IFDEF Align32Bytes}
        MoveX32LPUniversal(APointer^, Result^, LOldUserSize);
   {$ELSE}
     {$IFDEF Align16Bytes}
        MoveX16LP(APointer^, Result^, LOldUserSize);
     {$ELSE}
        MoveX8LP(APointer^, Result^, LOldUserSize);
     {$ENDIF}
   {$ENDIF}
{$ELSE}
      System.Move(APointer^, Result^, LOldUserSize);
{$ENDIF}
      {Free the old block}
      FastFreeMem(APointer);
    end;
  end
  else
  begin
    {It's a downsize: do we need to reallocate? Only if the new size is less
     than half the old size}
    if ANewSize >= (LOldAvailableSize shr 1) then
    begin
      {No need to reallocate}
      Result := APointer;
      {Update the requested size}
      PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize)^.UserAllocatedSize := ANewSize;
    end
    else
    begin
      {The block is less than half the old size, and the current size is
       greater than the minimum block size allowing a downsize: reallocate}
      Result := FastGetMem(ANewSize);
      if Result <> nil then
      begin
        {Still a large block? -> Set the user size}
        if ANewSize > (MaximumMediumBlockSize - BlockHeaderSize) then
          PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize)^.UserAllocatedSize := ANewSize;
        {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
{$IFDEF Align32Bytes}
        MoveX32LPUniversal(APointer^, Result^, ANewSize);
{$ELSE}
{$IFDEF Align16Bytes}
        MoveX16LP(APointer^, Result^, ANewSize);
{$ELSE}
        MoveX8LP(APointer^, Result^, ANewSize);
{$ENDIF}
{$ENDIF}
{$ELSE}
        System.Move(APointer^, Result^, ANewSize);
{$ENDIF}
        {Free the old block}
        FastFreeMem(APointer);
      end;
    end;
  end;
end;
{$ENDIF}

{---------------------Replacement Memory Manager Interface---------------------}

{This function is only needed to cope with an error that happens at runtime
when using the "typed @ operator" compiler option. We are having just
one typecast in this function to avoid using typecasts throught the
entire FastMM4 module.}

function NegCardinalMaskBit(A: Cardinal): Cardinal;
{$IFNDEF ASMVersion}
begin
  Result := Cardinal(0-Int64(A));
end;
{$ELSE}
assembler;
asm
{$IFDEF 32bit}
        neg     eax
{$ELSE}
   {$IFDEF unix}
        mov     eax, edi
   {$ELSE}
     {$IFDEF AllowAsmNoframe}
       .noframe
     {$ENDIF}
        mov     eax, ecx
   {$ENDIF}
        neg     eax
{$ENDIF}
end;
{$ENDIF}

function NegByteMaskBit(A: Byte): Byte;
{$IFNDEF ASMVersion}
begin
{$IFDEF XE2AndUp}
  Result := Byte((0-System.Int8(A)));
{$ELSE}
  Result := Byte((0-ShortInt(A)));
{$ENDIF}
end;
{$ELSE}
assembler;
asm
{$IFDEF 32bit}
        neg     al
{$ELSE}
   {$IFDEF unix}
        movzx   eax, dil
   {$ELSE}
   {$IFDEF AllowAsmNoframe}
     .noframe
   {$ENDIF}
        movzx   eax, cl
   {$ENDIF}
        neg     al
{$ENDIF}
end;
{$ENDIF ASMVersion}

function NegNativeUIntMaskBit(A: NativeUInt): NativeUint;
{$IFNDEF ASMVersion}
begin
  Result := NativeUInt(0-Int64(A));
end;
{$ELSE}
assembler;
asm
{$IFDEF 32bit}
        neg     eax
{$ELSE}
   {$IFDEF unix}
        mov     rax, rdi
   {$ELSE}
     {$IFDEF AllowAsmNoframe}
       .noframe
     {$ENDIF}
        mov     rax, rcx
   {$ENDIF}
        neg     rax
{$ENDIF}
end;
{$ENDIF ASMVersion}

{$IFDEF DebugReleaseLockByte}
procedure SmallBlockUnlockError;
begin
{$IFNDEF SystemRunError}
  System.Error(reInvalidOp);
{$ELSE}
  System.RunError(reInvalidOp);
{$ENDIF}
end;
{$ENDIF}


{$IFNDEF ASMVersion}
{$define NeedFindFirstSetBit}
{$ENDIF}

{$IFDEF FastGetMemNeedPascalCode}
{$define NeedFindFirstSetBit}
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{$define NeedFindFirstSetBit}
{$ENDIF}


{$IFDEF NeedFindFirstSetBit}
{Gets the first set bit in the 32-bit number, returning the bit index}
function FindFirstSetBit(ACardinal: Cardinal): Cardinal;
{$IFNDEF ASMVersion}
var
  LOffset : Integer;
  LCardinal: Cardinal;
begin
  LCardinal := ACardinal;
  LOffset := 0;
  if LCardinal <> 0 then
  begin
   while (LCardinal and 1) = 0 do
   begin
     Inc(LOffset);
     LCardinal := LCardinal shr 1;
   end;
  end;
  Result := LOffset;
end;
{$ELSE ASMVersion}
assembler;
{$IFDEF fpc64bit} nostackframe; {$ENDIF}
asm
{$IFDEF 64Bit}
  {$IFNDEF unix}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  mov eax, ecx
  {$ELSE}
  mov eax, edi
  {$ENDIF}
{$ENDIF}
  bsf eax, eax
end;
{$ENDIF ASMVersion}
{$ENDIF NeedFindFirstSetBit}



{$IFNDEF AssumeMultiThreaded}
const
  StateBitMultithreaded   = 1;
  StateBitSmallLocked     = 2;
  StateBitMediumLocked    = 3;
{$ENDIF}


{Replacement for SysGetMem}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  {$IFDEF FastGetMemNeedPascalCode}
    function FastGetMemPascal(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF fpc}{$ENDIF XE2AndUp}{$IFDEF FullDebugMode}{$IFDEF LogLockContention}; var ACollector: PStaticCollector{$ENDIF}{$ENDIF}): Pointer; forward;
  {$ENDIF}
  {$IFDEF FastGetMemNeedAssemblerCode}
    function FastGetMemAssembler(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF fpc}{$ENDIF XE2AndUp}{$IFDEF FullDebugMode}{$IFDEF LogLockContention}; var ACollector: PStaticCollector{$ENDIF}{$ENDIF}): Pointer; forward;
  {$ENDIF}
{$ENDIF}

{$IFDEF DEBUG}
procedure BadAlignmentOnGetMem;
begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
end;
{$ENDIF}




function FastGetMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF fpc}{$ENDIF XE2AndUp}{$IFDEF FullDebugMode}{$IFDEF LogLockContention}; var ACollector: PStaticCollector{$ENDIF}{$ENDIF}): Pointer;

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
assembler;
asm
  test [rip+FastMMCpuFeaturesA], FastMMCpuFeaturePauseAndSwitch
  jz @CallFastGetMemPascal
  call FastGetMemAssembler
  jmp @Finish
@CallFastGetMemPascal:
  call FastGetMemPascal
@Finish:
end;
{$ENDIF}


{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
function FastGetMemPascal(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF fpc}{$ENDIF XE2AndUp}{$IFDEF FullDebugMode}{$IFDEF LogLockContention}; var ACollector: PStaticCollector{$ENDIF}{$ENDIF}): Pointer;
{$ENDIF}

{$IFDEF FastGetMemNeedPascalCode}
var
  LMediumBlock: PMediumFreeBlock;
{$IFNDEF FullDebugMode}
  LNextFreeBlock, LSecondSplit: PMediumFreeBlock;
{$ENDIF}
  LNextMediumBlockHeader: PNativeUInt;
  LBlockSize, LAvailableBlockSize: NativeUInt;
{$IFNDEF FullDebugMode}
  LSecondSplitSize: NativeUInt;
{$ENDIF}
  LSequentialFeedFreeSize: NativeUInt;
  LPSmallBlockType: PSmallBlockType;
  LPSmallBlockPool, LPNewFirstPool: PSmallBlockPoolHeader;
  LNewFirstFreeBlock: Pointer;
  LPMediumBin: PMediumFreeBlock;
  LBinNumber: NativeUInt;
{$IFNDEF FullDebugMode}
  LBinGroupsMasked: NativeUInt;
{$ENDIF}
  LBinGroupMasked,
  LBinGroupNumber: NativeUInt;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
{$IFNDEF FullDebugMode}
  ACollector: PStaticCollector;
  LStackTrace: TStackTrace;
{$ENDIF FullDebugMode}
{$ENDIF LogLockContention}
{$IFDEF UseReleaseStack}
  LPReleaseStack: ^TLFStack;
{$ENDIF}
{$IFDEF SmallBlocksLockedCriticalSection}
  LSmallBlockCriticalSectionIndex: NativeUInt;
  LFailedToAcquireLock: Boolean;
{$ENDIF}
  LSmallBlockSizeInGranularUnits: NativeUInt;
{$IFNDEF AssumeMultiThreaded}
  LWasMultiThread: Boolean;
{$ENDIF}
  LMediumBlocksLocked: Boolean;
  LSmallBlockWithoutLock: Boolean;
  LBlockTypeIndex, LBlockTypeOffset: NativeUInt;
  LShift: Byte;
  LMask: Cardinal;
begin

  LMediumBlocksLocked := False;
  LSmallBlockWithoutLock := False;

{$IFNDEF AssumeMultiThreaded}
  LWasMultiThread := False;
{$ENDIF}
{$IFDEF LogLockContention}
  ACollector := nil;
{$ENDIF}
{$IFDEF SmallBlocksLockedCriticalSection}
  LSmallBlockCriticalSectionIndex := MaxInt;
  LFailedToAcquireLock := False;
{$ENDIF}
  {Is it a small block? -> Take the header size into account when
   determining the required block size}
  if NativeUInt(ASize) <= (MaximumSmallBlockSize - BlockHeaderSize) then
  begin
    {-------------------------Allocate a small block---------------------------}
    {Get the block type from the size}
    LSmallBlockSizeInGranularUnits := (NativeUInt(ASize) + (BlockHeaderSize - 1)) shr SmallBlockGranularityPowerOf2;
    LBlockTypeIndex :=
   {$IFDEF AllocSize2SmallBlockTypesPrecomputedOffsets}
    AllocSz2SmlBlkTypOfsDivSclFctr[LSmallBlockSizeInGranularUnits]
   {$ELSE}
    AllocSize2SmallBlockTypesIdx[LSmallBlockSizeInGranularUnits]
   {$ENDIF}
    ;
    LBlockTypeOffset := LBlockTypeIndex
    {$IFDEF AllocSize2SmallBlockTypesPrecomputedOffsets}
      shl MaximumCpuScaleFactorPowerOf2
    {$ELSE}
      {$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
        shl SmallBlockTypeRecSizePowerOf2
      {$ELSE}
        * SmallBlockTypeRecSize
      {$ENDIF}
    {$ENDIF}
    ;
    LPSmallBlockType := PSmallBlockType(LBlockTypeOffset+UIntPtr(@SmallBlockTypes[0]));
{$IFDEF UseReleaseStack}
    LPReleaseStack := @LPSmallBlockType.ReleaseStack[GetStackSlot];
    if (not LPReleaseStack^.IsEmpty) and LPReleaseStack^.Pop(Result) then
      Exit;
{$ENDIF}
    {Lock the block type}
{$IFNDEF AssumeMultiThreaded}
    if IsMultiThread then
{$ENDIF}
    begin
      {$IFNDEF AssumeMultiThreaded}
      LWasMultiThread := True;
      {$ENDIF}
      while True do
      begin
        {Try to lock the small block type (0)}
        if AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          Break;

        {Try the next block type (+1)}
        Inc(PByte(LPSmallBlockType), SmallBlockTypeRecSize);
        if AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          Break;

        {Try up to two sizes past the requested size (+2)}
        Inc(PByte(LPSmallBlockType), SmallBlockTypeRecSize);
        if AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          Break;

        {All three sizes locked - give up and sleep (revert pointer (-2))}
        Dec(PByte(LPSmallBlockType), 2 * SmallBlockTypeRecSize);

        {Try to once again, last time to lock the small block type (0)}
        if AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          Break;

{$IFDEF SmallBlocksLockedCriticalSection}
        LFailedToAcquireLock := True;
        Break;
{$ELSE}
   {$IFDEF LogLockContention}
        ACollector := @LPSmallBlockType.BlockCollector;
  {$ENDIF}
  {$IFDEF NeverSleepOnThreadContention}
    {$IFDEF UseSwitchToThread}
        SwitchToThreadIfSupported;
    {$ENDIF}
  {$ELSE}
        {Both this block type and the next is in use: sleep}
        Sleep(InitialSleepTime);
        {Try to acquire the lock again}
        if AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          Break;
        {Sleep longer}
        Sleep(AdditionalSleepTime);
  {$ENDIF}
{$ENDIF}
      end;

{$IFDEF SmallBlocksLockedCriticalSection}
      {$IFNDEF DisablePauseAndSwitchToThread}
      if CpuFeaturePauseAndSwitch then
      begin
        if LFailedToAcquireLock then
        begin
          AcquireSpinLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
        end;
      end else
      {$ENDIF}
      begin
        LSmallBlockCriticalSectionIndex := (NativeUint(LPSmallBlockType)-NativeUint(@SmallBlockTypes))
          {$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
            shr SmallBlockTypeRecSizePowerOf2
          {$ELSE}
            div SmallBlockTypeRecSize
         {$ENDIF}
        ;
        EnterCriticalSection(SmallBlockCriticalSections[LSmallBlockCriticalSectionIndex]);
        if LFailedToAcquireLock then
        begin
          {Try the lock again}
          if not AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          begin
            LSmallBlockWithoutLock := True;
          end;
        end;
      end;
{$ENDIF}

    end;
    {Get the first pool with free blocks}
    LPSmallBlockPool := LPSmallBlockType^.NextPartiallyFreePool;
    {Is the pool valid?}
    if UIntPtr(LPSmallBlockPool) <> UIntPtr(LPSmallBlockType) then
    begin
      {Get the first free offset}
      Result := LPSmallBlockPool^.FirstFreeBlock;
      {Get the new first free block}
      LNewFirstFreeBlock := PPointer(PByte(Result) - BlockHeaderSize)^;
{$IFDEF CheckHeapForCorruption}
      {The block should be free}
      if (NativeUInt(LNewFirstFreeBlock) and ExtractSmallFlagsMask) <> IsFreeBlockFlag then
  {$IFDEF BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
  {$ELSE}
        System.RunError(reInvalidPtr);
  {$ENDIF}
{$ENDIF CheckHeapForCorruption}
      LNewFirstFreeBlock := Pointer(UIntPtr(LNewFirstFreeBlock) and DropSmallFlagsMask);
      {Increment the number of used blocks}
      Inc(LPSmallBlockPool^.BlocksInUse);
      {Set the new first free block}
      LPSmallBlockPool^.FirstFreeBlock := LNewFirstFreeBlock;
      {Is the pool now full?}
      if LNewFirstFreeBlock = nil then
      begin
        {Pool is full - remove it from the partially free list}
        LPNewFirstPool := LPSmallBlockPool^.NextPartiallyFreePool;
        LPSmallBlockType^.NextPartiallyFreePool := LPNewFirstPool;
        LPNewFirstPool^.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
      end;
    end
    else
    begin
      {Try to feed a small block sequentially}
      Result := LPSmallBlockType^.NextSequentialFeedBlockAddress;
      {Can another block fit?}
      if UIntPtr(Result) <= UIntPtr(LPSmallBlockType^.MaxSequentialFeedBlockAddress) then
      begin
        {Get the sequential feed block pool}
        LPSmallBlockPool := LPSmallBlockType^.CurrentSequentialFeedPool;
        {Increment the number of used blocks in the sequential feed pool}
        Inc(LPSmallBlockPool^.BlocksInUse);
        {Store the next sequential feed block address}
        LPSmallBlockType^.NextSequentialFeedBlockAddress := Pointer(PByte(Result) + LPSmallBlockType^.BlockSize);
      end
      else
      begin
        {Need to allocate a pool: Lock the medium blocks}
        {$IFNDEF AssumeMultiThreaded}
        if IsMultiThread then
        {$ENDIF}
        begin
          {$IFNDEF AssumeMultiThreaded}
          LWasMultiThread := True;
          {$ENDIF}
          LMediumBlocksLocked := True;
          {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
        end;
{$IFDEF LogLockContention}
        if LDidSleep then
          ACollector := @MediumBlockCollector;
{$ENDIF}
{$IFNDEF FullDebugMode}
        {Are there any available blocks of a suitable size?}
        LBinGroupsMasked := MediumBlockBinGroupBitmap and ($ffffff00 or LPSmallBlockType^.AllowedGroupsForBlockPoolBitmap);
        if LBinGroupsMasked <> 0 then
        begin
          {Get the bin group with free blocks}
          LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);
          {Get the bin in the group with free blocks}
          LBinNumber := FindFirstSetBit(MediumBlockBinBitmaps[LBinGroupNumber])
            + (LBinGroupNumber shl MediumBlockBinsPerGroupPowerOf2);
          LPMediumBin := @(MediumBlockBins[LBinNumber]);
          {Get the first block in the bin}
          LMediumBlock := LPMediumBin^.NextFreeBlock;
          {Remove the first block from the linked list (LIFO)}
          LNextFreeBlock := LMediumBlock^.NextFreeBlock;
          LPMediumBin^.NextFreeBlock := LNextFreeBlock;
          LNextFreeBlock^.PreviousFreeBlock := LPMediumBin;
          {Is this bin now empty?}
          if LNextFreeBlock = LPMediumBin then
          begin
            LShift := LBinNumber and (MediumBlockBinsPerGroup-1);
            LMask := not (Cardinal(UnsignedBit) shl LShift);
            {Flag this bin as empty}
            MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber] and LMask;
            {Is the group now entirely empty?}
            if MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
            begin
              LMask := not (Cardinal(UnsignedBit) shl LBinGroupNumber);
              {Flag this group as empty}
              MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap and LMask;
            end;
          end;
          {Get the size of the available medium block}
          LBlockSize := PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  {$IFDEF CheckHeapForCorruption}
          {Check that this block is actually free and the next and previous blocks
           are both in use.}
          if ((PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag))
            or ((PNativeUInt(PByte(LMediumBlock) + (PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and IsFreeBlockFlag) <> 0)
          then
          begin
    {$IFDEF BCB6OrDelphi7AndUp}
            System.Error(reInvalidPtr);
    {$ELSE}
            System.RunError(reInvalidPtr);
    {$ENDIF}
          end;
  {$ENDIF}
          {Should the block be split?}
          if LBlockSize >= MaximumSmallBlockPoolSize then
          begin
            {Get the size of the second split}
            LSecondSplitSize := LBlockSize - LPSmallBlockType^.OptimalBlockPoolSize;
            {Adjust the block size}
            LBlockSize := LPSmallBlockType^.OptimalBlockPoolSize;
            {Split the block in two}
            LSecondSplit := PMediumFreeBlock(PByte(LMediumBlock) + LBlockSize);
            PNativeUInt(PByte(LSecondSplit) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
            {Store the size of the second split as the second last dword/qword}
            PNativeUInt(PByte(LSecondSplit) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
            {Put the remainder in a bin (it will be big enough)}
            InsertMediumBlockIntoBin(LSecondSplit, LSecondSplitSize);
          end
          else
          begin
            {Mark this block as used in the block following it}
            LNextMediumBlockHeader := PNativeUInt(PByte(LMediumBlock) + LBlockSize - BlockHeaderSize);
            LNextMediumBlockHeader^ := LNextMediumBlockHeader^ and (not PreviousMediumBlockIsFreeFlag);
          end;
        end
        else
{$ENDIF}
        begin
          {Check the sequential feed medium block pool for space}
          LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
          if LSequentialFeedFreeSize >= LPSmallBlockType^.MinimumBlockPoolSize then
          begin
            {Enough sequential feed space: Will the remainder be usable?}
            if LSequentialFeedFreeSize >= (LPSmallBlockType^.OptimalBlockPoolSize + MinimumMediumBlockSize) then
            begin
              LBlockSize := LPSmallBlockType^.OptimalBlockPoolSize;
            end
            else
              LBlockSize := LSequentialFeedFreeSize;
            {Get the block}
            LMediumBlock := Pointer(PByte(LastSequentiallyFedMediumBlock) - LBlockSize);
            {Update the sequential feed parameters}
            LastSequentiallyFedMediumBlock := LMediumBlock;
            MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
          end
          else
          begin
            {Need to allocate a new sequential feed medium block pool: use the
             optimal size for this small block pool}
            LBlockSize := LPSmallBlockType^.OptimalBlockPoolSize;
            {Allocate the medium block pool}
            LMediumBlock := AllocNewSequentialFeedMediumPool(LBlockSize);
            if LMediumBlock = nil then
            begin
              {Out of memory}
              {$IFNDEF AssumeMultiThreaded}
              if LWasMultiThread then
              {$ENDIF}
              begin
                {Unlock the medium blocks}
                if LMediumBlocksLocked then
                begin
                  LMediumBlocksLocked := False;
                  UnlockMediumBlocks;
                end;
                {Unlock the block type}
                if not LSmallBlockWithoutLock then
                begin
                  ReleaseLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
                end else
                begin
                  LSmallBlockWithoutLock := False;
                end;
                {$IFDEF SmallBlocksLockedCriticalSection}
                if LSmallBlockCriticalSectionIndex <> NativeUInt(MaxInt) then
                begin
                  LeaveCriticalSection(SmallBlockCriticalSections[LSmallBlockCriticalSectionIndex]);
                  LSmallBlockCriticalSectionIndex := NativeUInt(MaxInt);
                end;
                {$ENDIF}
              end;
              {Failed}
              Result := nil;
              {done}
              Exit;
            end;
          end;
        end;
        {Mark this block as in use}
        {Set the size and flags for this block}
        PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag or IsSmallBlockPoolInUseFlag;
        {Unlock medium blocks}
        {$IFNDEF AssumeMultiThreaded}
        if LWasMultiThread then
        {$ENDIF}
        begin
          if LMediumBlocksLocked then
          begin
            LMediumBlocksLocked := False;
            UnlockMediumBlocks;
          end;
        end;
        {Set up the block pool}
        LPSmallBlockPool := PSmallBlockPoolHeader(LMediumBlock);
        LPSmallBlockPool^.BlockType := LPSmallBlockType;
        LPSmallBlockPool^.FirstFreeBlock := nil;
        LPSmallBlockPool^.BlocksInUse := 1;
        {Set it up for sequential block serving}
        LPSmallBlockType^.CurrentSequentialFeedPool := LPSmallBlockPool;
        Result := Pointer(PByte(LPSmallBlockPool) + SmallBlockPoolHeaderSize);
        LPSmallBlockType^.NextSequentialFeedBlockAddress := Pointer(PByte(Result) + LPSmallBlockType^.BlockSize);
        LPSmallBlockType^.MaxSequentialFeedBlockAddress := Pointer(PByte(LPSmallBlockPool) + LBlockSize - LPSmallBlockType^.BlockSize);
      end;
{$IFDEF FullDebugMode}
      {Clear the user area of the block}
      DebugFillMem(Pointer(PByte(Result) + (SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt)))^,
        LPSmallBlockType^.BlockSize - FullDebugBlockOverhead - SizeOf(NativeUInt),
        {$IFNDEF CatchUseOfFreedInterfaces}NativeUInt(DebugFillPattern){$ELSE}NativeUInt(@VMTBadInterface){$ENDIF});
      {Block was fed sequentially - we need to set a valid debug header. Use
       the block address.}
      PFullDebugBlockHeader(Result)^.HeaderCheckSum := NativeUInt(Result);
      PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(Result);
{$ENDIF}
    end;
    {Set the block header}
    PNativeUInt(PByte(Result) - BlockHeaderSize)^ := UIntPtr(LPSmallBlockPool);
    {$IFNDEF AssumeMultiThreaded}
    if LWasMultiThread then
    {$ENDIF}
    begin
      {Unlock the block type}
      if not LSmallBlockWithoutLock then
      begin
        ReleaseLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
      end else
      begin
        LSmallBlockWithoutLock := False;
      end;
      {$IFDEF SmallBlocksLockedCriticalSection}
      if LSmallBlockCriticalSectionIndex <> NativeUInt(MaxInt) then
      begin
        LeaveCriticalSection(SmallBlockCriticalSections[LSmallBlockCriticalSectionIndex]);
        LSmallBlockCriticalSectionIndex := NativeUInt(MaxInt);
      end;
      {$ENDIF}
    end;
  end
  else
  begin
    {Medium block or Large block?}
    if NativeUInt(ASize) <= (MaximumMediumBlockSize - BlockHeaderSize) then
    begin
      {------------------------Allocate a medium block--------------------------}
      {Get the block size and bin number for this block size. Block sizes are
       rounded up to the next bin size.}
      LBlockSize := ((NativeUInt(ASize) + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
        and MediumBlockGranularityMask) + MediumBlockSizeOffset;
      {Get the bin number}
      LBinNumber := (LBlockSize - MinimumMediumBlockSize) shr MediumBlockGranularityPowerOf2;
      {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
      if IsMultiThread then
{$ENDIF}
      begin
{$IFNDEF AssumeMultiThreaded}
        LWasMultithread := True;
{$ENDIF}
        LMediumBlocksLocked := True;
        {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
        {$IFDEF LogLockContention}
        if LDidSleep then
        begin
          ACollector := @MediumBlockCollector;
        end;
        {$ENDIF}
      end;

      {Calculate the bin group}
      LBinGroupNumber := LBinNumber shr MediumBlockBinsPerGroupPowerOf2;
      LShift := LBinNumber and (MediumBlockBinsPerGroup-1);
      {Is there a suitable block inside this group?}
      LBinGroupMasked := MediumBlockBinBitmaps[LBinGroupNumber] and NegCardinalMaskBit(Cardinal(UnsignedBit) shl LShift);
      if LBinGroupMasked <> 0 then
      begin
        {Get the actual bin number}
        LBinNumber := FindFirstSetBit(LBinGroupMasked) + (LBinGroupNumber shl MediumBlockBinsPerGroupPowerOf2);
      end
      else
      begin
{$IFNDEF FullDebugMode}
        {Try all groups greater than this group}
        LBinGroupsMasked := MediumBlockBinGroupBitmap and NegNativeUIntMaskBit(NativeUInt(2) shl LBinGroupNumber);
        if LBinGroupsMasked <> 0 then
        begin
          {There is a suitable group with space: get the bin number}
          LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);
          {Get the bin in the group with free blocks}
          LBinNumber := FindFirstSetBit(MediumBlockBinBitmaps[LBinGroupNumber])
            + (LBinGroupNumber shl MediumBlockBinsPerGroupPowerOf2);
        end
        else
        begin
{$ENDIF}
          {There are no bins with a suitable block: Sequentially feed the required block}
          LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
          if LSequentialFeedFreeSize >= LBlockSize then
          begin
{$IFDEF FullDebugMode}
            {In full debug mode a medium block must have enough bytes to fit
             all the debug info, so we must make sure there are no tiny medium
             blocks at the start of the pool.}
            if LSequentialFeedFreeSize - LBlockSize < (FullDebugBlockOverhead + BlockHeaderSize) then
              LBlockSize := LSequentialFeedFreeSize;
{$ENDIF}
            {Block can be fed sequentially}
            Result := Pointer(PByte(LastSequentiallyFedMediumBlock) - LBlockSize);
            {Store the last sequentially fed block}
            LastSequentiallyFedMediumBlock := Result;
            {Store the remaining bytes}
            MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
            {Set the flags for the block}
            PNativeUInt(PByte(Result) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
          end
          else
          begin
            {Need to allocate a new sequential feed block}
            Result := AllocNewSequentialFeedMediumPool(LBlockSize);
          end;
{$IFDEF FullDebugMode}
          {Block was fed sequentially - we need to set a valid debug header}
          if Result <> nil then
          begin
            PFullDebugBlockHeader(Result)^.HeaderCheckSum := NativeUInt(Result);
            PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(Result);
            {Clear the user area of the block}
            DebugFillMem(Pointer(PByte(Result) + SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt))^,
              LBlockSize - FullDebugBlockOverhead - SizeOf(NativeUInt),
              {$IFNDEF CatchUseOfFreedInterfaces}NativeUInt(DebugFillPattern){$ELSE}NativeUInt(@VMTBadInterface){$ENDIF});
          end;
{$ENDIF}
          {Done}
          {$IFNDEF AssumeMultiThreaded}
          if LWasMultithread then
          {$ENDIF}
          begin
            if LMediumBlocksLocked then
            begin
              LMediumBlocksLocked := False;
              UnlockMediumBlocks;
            end;
          end;
{$IFDEF LogLockContention}
{$IFNDEF FullDebugMode}
          if Assigned(ACollector) then
          begin
            GetStackTrace(@(LStackTrace[0]), StackTraceDepth, 1);
            ACollector.Add(@(LStackTrace[0]), StackTraceDepth);
          end;
{$ENDIF}
{$ENDIF}
          Exit;
{$IFNDEF FullDebugMode}
        end;
{$ENDIF}
      end;
      {If we get here we have a valid LBinGroupNumber and LBinNumber:
       Use the first block in the bin, splitting it if necessary}
      {Get a pointer to the bin}
      LPMediumBin := @(MediumBlockBins[LBinNumber]);
      {Get the result}
      Result := LPMediumBin^.NextFreeBlock;
{$IFDEF CheckHeapForCorruption}
      {Check that this block is actually free and the next and previous blocks
       are both in use (except in full debug mode).}
      if ((PNativeUInt(PByte(Result) - BlockHeaderSize)^ and {$IFNDEF FullDebugMode}ExtractMediumAndLargeFlagsMask{$ELSE}(IsMediumBlockFlag or IsFreeBlockFlag){$ENDIF}) <> (IsFreeBlockFlag or IsMediumBlockFlag))
  {$IFNDEF FullDebugMode}
        or ((PNativeUInt(PByte(Result) + (PNativeUInt(PByte(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and (ExtractMediumAndLargeFlagsMask - IsSmallBlockPoolInUseFlag)) <> (IsMediumBlockFlag or PreviousMediumBlockIsFreeFlag))
  {$ENDIF}
      then
      begin
  {$IFDEF BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
  {$ELSE}
        System.RunError(reInvalidPtr);
  {$ENDIF}
      end;
{$ENDIF CheckHeapForCorruption}
      {Remove the block from the bin containing it}
      RemoveMediumFreeBlock(Result);
      {Get the block size}
      LAvailableBlockSize := PNativeUInt(PByte(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
{$IFNDEF FullDebugMode}
      {Is it an exact fit or not?}
      LSecondSplitSize := LAvailableBlockSize - LBlockSize;
      if LSecondSplitSize <> 0 then
      begin
        {Split the block in two}
        LSecondSplit := PMediumFreeBlock(PByte(Result) + LBlockSize);
        {Set the size of the second split}
        PNativeUInt(PByte(LSecondSplit) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
        {Store the size of the second split}
        PNativeUInt(PByte(LSecondSplit) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
        {Put the remainder in a bin if it is big enough}
        if LSecondSplitSize >= MinimumMediumBlockSize then
          InsertMediumBlockIntoBin(LSecondSplit, LSecondSplitSize);
      end
      else
      begin
{$ELSE}
        {In full debug mode blocks are never split or coalesced}
        LBlockSize := LAvailableBlockSize;
{$ENDIF}
        {Mark this block as used in the block following it}
        LNextMediumBlockHeader := Pointer(PByte(Result) + LBlockSize - BlockHeaderSize);
{$IFNDEF FullDebugMode}
  {$IFDEF CheckHeapForCorruption}
        {The next block must be in use}
        if (LNextMediumBlockHeader^ and (ExtractMediumAndLargeFlagsMask - IsSmallBlockPoolInUseFlag)) <> (IsMediumBlockFlag or PreviousMediumBlockIsFreeFlag) then
    {$IFDEF BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$ELSE}
        System.RunError(reInvalidPtr);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
        LNextMediumBlockHeader^ :=
          LNextMediumBlockHeader^ and (not PreviousMediumBlockIsFreeFlag);
{$IFNDEF FullDebugMode}
      end;
      {Set the size and flags for this block}
      PNativeUInt(PByte(Result) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
{$ELSE}
      {In full debug mode blocks are never split or coalesced}
      Dec(PNativeUInt(PByte(Result) - BlockHeaderSize)^, IsFreeBlockFlag);
{$ENDIF}
      {$IFNDEF AssumeMultiThreaded}
      if LWasMultithread then
      {$ENDIF}
      begin
        if LMediumBlocksLocked then
        begin
          {Unlock the medium blocks}
          LMediumBlocksLocked := False;
          UnlockMediumBlocks;
        end;
      end;
    end
    else
    begin
      {Allocate a Large block}
      if ASize > 0 then
      begin
        Result := AllocateLargeBlock(ASize {$IFDEF LogLockContention}, LDidSleep{$ENDIF});
{$IFDEF LogLockContention}
        if LDidSleep then
          ACollector := @LargeBlockCollector;
{$ENDIF}
      end
      else
        Result := nil;
    end;
  end;
{$IFDEF LogLockContention}
{$IFNDEF FullDebugMode}
  if Assigned(ACollector) then
  begin
    GetStackTrace(@(LStackTrace[0]), StackTraceDepth, 1);
    ACollector.Add(@(LStackTrace[0]), StackTraceDepth);
  end;
{$ENDIF}
{$ENDIF}
end;
{$ENDIF FastGetMemNeedPascalCode}

{$IFDEF FastGetMemNeedAssemblerCode}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  function FastGetMemAssembler(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF fpc}{$ENDIF XE2AndUp}{$IFDEF FullDebugMode}{$IFDEF LogLockContention}; var ACollector: PStaticCollector{$ENDIF}{$ENDIF}): Pointer;
{$ENDIF}

{$IFDEF 32Bit}
assembler;
asm
  {On entry:
    eax = ASize}

{EBP is not used at all in the assembly routine FastGetMem - use it for the FastMM flags,
like IsMultithreaded or MediumBlocksLocked}

{$IFNDEF AssumeMultiThreaded}
  push ebp {Save ebp}
{$ENDIF}
  push ebx {Save ebx}

{$IFNDEF AssumeMultiThreaded}
  xor ebp, ebp

  {Branchless operations to avoid misprediction}
  cmp byte ptr [IsMultiThread], 0
  setnz bl
  movzx ebx, bl
  shl ebx, StateBitMultithreaded
  or ebp, ebx
{$ENDIF}

  {Since most allocations are for small blocks, determine the small block type
   index so long}
  lea edx, [eax + BlockHeaderSize - 1]
  {Divide edx by SmallBlockGranularity which is always power of 2}
  shr edx, SmallBlockGranularityPowerOf2
  {Is it a small block?}
  cmp eax, (MaximumSmallBlockSize - BlockHeaderSize)
  {Is it a small block?}
  ja @NotASmallBlock
  {Get the small block type in ebx}
  movzx eax, byte ptr [AllocSz2SmlBlkTypOfsDivSclFctr + edx]
  lea ebx, [SmallBlockTypes + eax * MaximumCpuScaleFactor]
  {Do we need to lock the block type?}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMultithreaded)
  jnz @LockSmallBlockType {test+jnz invoke macro-op fusion}
  jmp @AfterLock
{$ELSE}
  jmp @LockSmallBlockType
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotLockOnSmallBlockType:
  {$IFDEF SmallBlocksLockedCriticalSection}{$IFDEF DebugAcquireLockByte}
  cmp al, cLockByteAvailable
  jne SmallBlockUnlockError
  {$ENDIF}{$ENDIF}

{$IFNDEF AssumeMultiThreaded}
  or ebp, (UnsignedBit shl StateBitSmallLocked)
{$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@AfterLock:
  {Find the next free block: Get the first pool with free blocks in edx}
  mov edx, TSmallBlockType[ebx].NextPartiallyFreePool
  {Get the first free block (or the next sequential feed address if edx = ebx)}
  mov eax, TSmallBlockPoolHeader[edx].FirstFreeBlock
  {Get the drop flags mask in ecx so long}
  mov ecx, DropSmallFlagsMask
  {Is there a pool with free blocks?}
  cmp edx, ebx
  je @TrySmallSequentialFeed
  {Increment the number of used blocks}
  add TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Get the new first free block}
  and ecx, [eax - BlockHeaderSize]
  {Set the new first free block}
  mov TSmallBlockPoolHeader[edx].FirstFreeBlock, ecx
  {Set the block header}
  mov [eax - BlockHeaderSize], edx
  {Is the chunk now full?}
  jz @RemoveSmallPool
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@UnlockSmallBlockAndExit:
  {Unlock the block type}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitSmallLocked)
  jz @Exit
{$ENDIF}
  {$IFDEF DebugReleaseLockByte}
  cmp TSmallBlockType[ebx].SmallBlockTypeLocked, cLockByteLocked
  jne SmallBlockUnlockError
  {$ENDIF}

{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov TSmallBlockType[ebx].SmallBlockTypeLocked, cLockByteAvailable
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@TrySmallSequentialFeed:
  {Try to feed a small block sequentially: Get the sequential feed block pool}
  mov edx, TSmallBlockType[ebx].CurrentSequentialFeedPool
  {Get the next sequential feed address so long}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  add ecx, eax
  {Can another block fit?}
  cmp eax, TSmallBlockType[ebx].MaxSequentialFeedBlockAddress
  ja @AllocateSmallBlockPool
  {Increment the number of used blocks in the sequential feed pool}
  add TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Store the next sequential feed block address}
  mov TSmallBlockType[ebx].NextSequentialFeedBlockAddress, ecx
  mov [eax - BlockHeaderSize], edx
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@RemoveSmallPool:
  {Pool is full - remove it from the partially free list}
  mov ecx, TSmallBlockPoolHeader[edx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, ebx
  mov TSmallBlockType[ebx].NextPartiallyFreePool, ecx
  jmp @UnlockSmallBlockAndExit

{===== START OF SMALL BLOCK LOCKING CODE; 32-BIT FASTGETMEM =====}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@LockSmallBlockType:

{$IFDEF SmallBlocksLockedCriticalSection}
   mov  eax, cLockByteLocked
   mov  edx, Type(TSmallBlockType)
  {$IFNDEF DebugAcquireLockByte}
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   je   @FirstBlockLocked
  {$ELSE}
   movzx eax, byte ptr TSmallBlockType([ebx]).SmallBlockTypeLocked
   cmp  al, cLockByteLocked
   je   @FirstBlockLocked
   cmp  al, cLockByteAvailable
   jne  SmallBlockUnlockError
   mov  eax, cLockByteLocked
  {$ENDIF}
   lock xchg TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@FirstBlockLocked:
  {Try the next size}
   add  ebx, edx
  {$IFNDEF DebugAcquireLockByte}
   cmp  TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   je   @SecondBlockLocked
  {$ELSE}
   movzx  eax, byte ptr TSmallBlockType([ebx]).SmallBlockTypeLocked
   cmp  al, cLockByteLocked
   je   @SecondBlockLocked
   cmp  al, cLockByteAvailable
   jne  SmallBlockUnlockError
   mov  eax, cLockByteLocked
  {$ENDIF}

   lock xchg TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@SecondBlockLocked:
  {Try the next size (up to two sizes larger)}
   add  ebx, edx
  {$IFNDEF DebugAcquireLockByte}
   cmp  TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   je   @ThirdBlockLocked
  {$ELSE}
   movzx eax, TSmallBlockType([ebx]).SmallBlockTypeLocked
   cmp  al, cLockByteLocked
   je   @ThirdBlockLocked
   cmp  al, cLockByteAvailable
   jne  SmallBlockUnlockError
   mov  eax, cLockByteLocked
  {$ENDIF}
   lock xchg TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@ThirdBlockLocked:
  {Block type and two sizes larger are all locked - give up and sleep}
   sub  ebx, edx
   sub  ebx, edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@LockSmallBlockTypeLoop:
   mov  edx, cPauseSpinWaitLoopCount
   mov  eax, cLockByteLocked
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@DidntLock:
@NormalLoadLoop:
   dec  edx
   jz   @SwitchToThread // for static branch prediction, jump forward means "unlikely"
   db   $F3, $90 // pause
   {$IFNDEF DebugAcquireLockByte}
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   je   @NormalLoadLoop // for static branch prediction, jump backwards means "likely"
   {$ELSE}
   movzx eax, TSmallBlockType([ebx]).SmallBlockTypeLocked
   cmp  al, cLockByteLocked
   je   @NormalLoadLoop
   cmp  al, cLockByteAvailable
   jne  SmallBlockUnlockError
   mov  eax, cLockByteLocked
   {$ENDIF}
   lock xchg TSmallBlockType([ebx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   je   @DidntLock
   {Congratulations! We've got the lock!}
   jmp	@GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@SwitchToThread:
   push  ebx
   push  ecx
   push  esi
   push  edi
   push  ebp
   call  SwitchToThreadIfSupported
   pop   ebp
   pop   edi
   pop   esi
   pop   ecx
   pop   ebx
   jmp   @LockSmallBlockTypeLoop

{$ELSE !SmallBlocksLockedCriticalSection}

{ The 32-bit implemenation from the original FastMM4 that employs a loop of Sleep() or SwitchToThread().
By default, it will not be compiled into FastMM4-AVX which uses more efficient approach.}
@LockSmallBlockTypeLoop:
  mov eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  mov edx, eax
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
  {Try the next size}
  add ebx, Type(TSmallBlockType)
  mov eax, edx
  lock cmpxchg TSmallBlockType([ebx]).SmallBlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size (up to two sizes larger)}
  add ebx, Type(TSmallBlockType)
  mov eax, edx
  lock cmpxchg TSmallBlockType([ebx]).SmallBlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Block type and two sizes larger are all locked - give up and sleep}
  sub ebx, 2 * Type(TSmallBlockType)
{$IFDEF NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  db $F3, $90 // pause
  {$IFDEF UseSwitchToThread}
  call SwitchToThreadIfSupported
  {$ENDIF}
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ELSE NeverSleepOnThreadContention}
  {Couldn't grab the block type - sleep and try again}
  push edx {just save edx}
  push InitialSleepTime {argument}
  call Sleep
  pop  eax {restore existing edx value straight into eax}
  {Try again}
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ENDIF NeverSleepOnThreadContention}
{$ENDIF !SmallBlocksLockedCriticalSection}

{===== END OF SMALL BLOCK LOCKING CODE; 32-BIT FASTGETMEM =====}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@AllocateSmallBlockPool:
  {save additional registers}
  push esi
  push edi
  {Do we need to lock the medium blocks?}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMultithreaded)
  jz @MediumBlocksLockedForPool
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  // Assembly version of LockMediumBlocks preserves all volatile register (except eax), but non-assembly version does not, so we save ecx and edx for non-assembly implementation
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push ecx; push edx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop edx; pop ecx {$ENDIF}
{$ELSE}
   // Assembly version of AcquireSpinLockMediumBlocks preserves all volatile register (except eax),
   // and we only have the assembly implementation of this function; there is no PurePascal implementation
   call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or ebp, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlocksLockedForPool:
  {Are there any available blocks of a suitable size?}
  movsx esi, TSmallBlockType[ebx].AllowedGroupsForBlockPoolBitmap
  and esi, MediumBlockBinGroupBitmap
  jz @NoSuitableMediumBlocks
  {Get the bin group number with free blocks in eax}
  bsf eax, esi
  {Get the bin number in ecx}
  lea esi, [eax * 8]
  mov ecx, dword ptr [MediumBlockBinBitmaps + eax * 4]
  bsf ecx, ecx
  lea ecx, [ecx + esi * 4]
  {Get a pointer to the bin in edi}
  lea edi, [MediumBlockBins + ecx * 8]
  {Get the free block in esi}
  mov esi, TMediumFreeBlock[edi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov edx, TMediumFreeBlock[esi].NextFreeBlock
  mov TMediumFreeBlock[edi].NextFreeBlock, edx
  mov TMediumFreeBlock[edx].PreviousFreeBlock, edi
  {Is this bin now empty?}
  cmp edi, edx
  jne @MediumBinNotEmpty
  {eax = bin group number, ecx = bin number, edi = @bin, esi = free block, ebx = block type}
  {Flag this bin as empty}
  mov edx, -2
  rol edx, cl
  and dword ptr [MediumBlockBinBitmaps + eax * 4], edx
  jnz @MediumBinNotEmpty
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBinNotEmpty:
  {esi = free block, ebx = block type}
  {Get the size of the available medium block in edi}
  mov edi, DropMediumAndLargeFlagsMask
  and edi, [esi - 4]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  {Split the block: get the size of the second part, new block size is the
   optimal size}
  mov edx, edi
  movzx edi, TSmallBlockType[ebx].OptimalBlockPoolSize
  sub edx, edi
  {Split the block in two}
  lea eax, [esi + edi]
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - BlockHeaderSize], ecx
  {Store the size of the second split as the second last dword}
  mov [eax + edx - BlockHeaderSize * 2], edx
  {Put the remainder in a bin (it will be big enough)}
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NoSuitableMediumBlocks:
  {Check the sequential feed medium block pool for space}
  movzx ecx, TSmallBlockType[ebx].MinimumBlockPoolSize
  mov edi, MediumSequentialFeedBytesLeft
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  {Get the address of the last block that was fed}
  mov esi, LastSequentiallyFedMediumBlock
  {Enough sequential feed space: Will the remainder be usable?}
  movzx ecx, TSmallBlockType[ebx].OptimalBlockPoolSize
  lea edx, [ecx + MinimumMediumBlockSize]
  cmp edi, edx
  jb @NotMuchSpace
  mov edi, ecx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotMuchSpace:
  sub esi, edi
  {Update the sequential feed parameters}
  sub MediumSequentialFeedBytesLeft, edi
  mov LastSequentiallyFedMediumBlock, esi
  {Get the block pointer}
  jmp @GotMediumBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@AllocateNewSequentialFeed:
  {Need to allocate a new sequential feed medium block pool: use the
   optimal size for this small block pool}
  movzx eax, TSmallBlockType[ebx].OptimalBlockPoolSize
  mov edi, eax
  {Allocate the medium block pool}
  call AllocNewSequentialFeedMediumPool
  mov esi, eax
  test eax, eax
  jnz @GotMediumBlock
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlMedBlksAftrAllocNewSeqFd
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontUnlMedBlksAftrAllocNewSeqFd:
  mov eax, esi
  pop edi
  pop esi
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@UseWholeBlock:
  {esi = free block, ebx = block type, edi = block size}
  {Mark this block as used in the block following it}
  and byte ptr [esi + edi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotMediumBlock:
  {esi = free block, ebx = block type, edi = block size}
  {Set the size and flags for this block}
  lea ecx, [edi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
  mov [esi - BlockHeaderSize], ecx
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlMedBlksAftrGotMedBlk
{$ENDIF}
  {Unlock medium blocks}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {it destroys eax, ecx and edx, but we don't need them}
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontUnlMedBlksAftrGotMedBlk:
  {Set up the block pool}
  xor eax, eax
  mov TSmallBlockPoolHeader[esi].BlockType, ebx
  mov TSmallBlockPoolHeader[esi].FirstFreeBlock, eax
  mov TSmallBlockPoolHeader[esi].BlocksInUse, 1
  {Set it up for sequential block serving}
  mov TSmallBlockType[ebx].CurrentSequentialFeedPool, esi
  {Return the pointer to the first block}
  lea eax, [esi + SmallBlockPoolHeaderSize]
  movzx ecx, TSmallBlockType[ebx].BlockSize
  lea edx, [eax + ecx]
  mov TSmallBlockType[ebx].NextSequentialFeedBlockAddress, edx
  add edi, esi
  sub edi, ecx
  mov TSmallBlockType[ebx].MaxSequentialFeedBlockAddress, edi
  {Set the small block header}
  mov [eax - BlockHeaderSize], esi
  {Restore registers}
  pop edi
  pop esi
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
{-------------------Medium block allocation-------------------}
@NotASmallBlock:
  cmp eax, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  {Get the bin size for this block size. Block sizes are
   rounded up to the next bin size.}
  lea ebx, [eax + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  and ebx, MediumBlockGranularityMask
  add ebx, MediumBlockSizeOffset
  {Do we need to lock the medium blocks?}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMultithreaded)
  jz @MediumBlocksLocked
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  // Assembly version of LockMediumBlocks preserves all volatile register (except eax), but non-assembly version does not, so we save ecx and edx for non-assembly implementation
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push ecx; push edx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop edx; pop ecx {$ENDIF}
{$ELSE}
   // Assembly version of AcquireSpinLockMediumBlocks preserves all volatile register (except eax),
   // and we only have the assembly implementation of this function; there is no PurePascal implementation
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or ebp, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlocksLocked:
  {Get the bin number in ecx and the group number in edx}
  lea edx, [ebx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  {Is there a suitable block inside this group?}
  mov eax, -1
  shl eax, cl
  and eax, dword ptr [MediumBlockBinBitmaps + edx * 4]
  jz @GroupIsEmpty
  {Get the actual bin number}
  and ecx, -MediumBlockBinsPerGroup
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GroupIsEmpty:
  {Try all groups greater than this group}
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, MediumBlockBinGroupBitmap
  jz @TrySequentialFeedMedium
  {There is a suitable group with space: get the bin number}
  bsf edx, eax
  {Get the bin in the group with free blocks}
  mov eax, dword ptr [MediumBlockBinBitmaps + edx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, 5
  or ecx, eax
  jmp @GotBinAndGroup
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@TrySequentialFeedMedium:
  mov ecx, MediumSequentialFeedBytesLeft
  {Block can be fed sequentially?}
  sub ecx, ebx
  jc @AllocateNewSequentialFeedForMedium
  {Get the block address}
  mov eax, LastSequentiallyFedMediumBlock
  sub eax, ebx
  mov LastSequentiallyFedMediumBlock, eax
  {Store the remaining bytes}
  mov MediumSequentialFeedBytesLeft, ecx
  {Set the flags for the block}
  or ebx, IsMediumBlockFlag
  mov [eax - BlockHeaderSize], ebx
  jmp @MediumBlockGetDone
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@AllocateNewSequentialFeedForMedium:
  mov eax, ebx
  call AllocNewSequentialFeedMediumPool
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockGetDone:
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz @Exit
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  mov ebx, eax {save eax}
  call UnlockMediumBlocks  {it also destroys ecx and edx, but we no longer need them}
  mov eax, ebx {restore eax}
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotBinAndGroup:
  {ebx = block size, ecx = bin number, edx = group number}
  push esi
  push edi
  {Get a pointer to the bin in edi}
  lea edi, [MediumBlockBins + ecx * 8]
  {Get the free block in esi}
  mov esi, TMediumFreeBlock[edi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov eax, TMediumFreeBlock[esi].NextFreeBlock
  mov TMediumFreeBlock[edi].NextFreeBlock, eax
  mov TMediumFreeBlock[eax].PreviousFreeBlock, edi
  {Is this bin now empty?}
  cmp edi, eax
  jne @MediumBinNotEmptyForMedium
  {eax = bin group number, ecx = bin number, edi = @bin, esi = free block, ebx = block size}
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBinNotEmptyForMedium:
  {esi = free block, ebx = block size}
  {Get the size of the available medium block in edi}
  mov edi, DropMediumAndLargeFlagsMask
  and edi, [esi - BlockHeaderSize]
  {Get the size of the second split in edx}
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  {Split the block in two}
  lea eax, [esi + ebx]
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - BlockHeaderSize], ecx
  {Store the size of the second split as the second last dword}
  mov [eax + edx - BlockHeaderSize * 2], edx
  {Put the remainder in a bin}
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlockForMedium
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@UseWholeBlockForMedium:
  {Mark this block as used in the block following it}
  and byte ptr [esi + edi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotMediumBlockForMedium:
  {Set the size and flags for this block}
  lea ecx, [ebx + IsMediumBlockFlag]
  mov [esi - BlockHeaderSize], ecx
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlMedBlkAftrGotMedBlkForMedium
{$ENDIF}
  {Unlock medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {it also destroys ecx and edx, but we no longer need them}
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@DontUnlMedBlkAftrGotMedBlkForMedium:
  mov eax, esi
  pop edi
  pop esi
  jmp @Exit
{-------------------Large block allocation-------------------}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}

@IsALargeBlockRequest:
  test eax, eax
  js  @DontAllocateLargeBlock
  pop ebx

{$IFNDEF AssumeMultiThreaded}
  pop ebp
{$ENDIF}
  call AllocateLargeBlock
  jmp @Finish
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@DontAllocateLargeBlock:
  xor eax, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@Exit:
  pop ebx
{$IFNDEF AssumeMultiThreaded}
  pop ebp
{$ENDIF}
@Finish:
{$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMemC
  jmp    BadAlignmentOnGetMem
@@OkAlignmentOnGetMemC:
{$ENDIF}
end;
{$ELSE}
{64-bit BASM implementation}
assembler;
asm
  {On entry:
    rcx = ASize}

  {Do not put ".noframe" here, for the reasons given at the comment
  in the "BinMediumSequentialFeedRemainder" function at the start of the
  64-bit assembler code}

  {$IFDEF AllowAsmParams}
  .params 2
  .pushnv rbx
  .pushnv rsi
  .pushnv rdi
  {$IFNDEF AssumeMultiThreaded}
  .pushnv r12
  {$ENDIF}
  {$ELSE}
  push rbx
  push rsi
  push rdi
  {$IFNDEF AssumeMultiThreaded}
  push r12
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF AssumeMultiThreaded}
  xor r12, r12
  {Get the IsMultiThread variable so long}
  lea rsi, [IsMultiThread]
  movzx esi, byte ptr [rsi] {this also clears highest bits of the rsi register}
  test esi, esi
  setnz sil
  shl esi, StateBitMultithreaded
  or r12, rsi
{$ENDIF}

  {Since most allocations are for small blocks, determine the small block type
   index so long.
  Because the argument is a 64-bit value, we should operate 64-bit registers here }
  lea rdx, [rcx + BlockHeaderSize - 1]
  {Divide rdx by SmallBlockGranularity which is always power of 2}
  shr rdx, SmallBlockGranularityPowerOf2

  {Preload the addresses of some small block structures}
  lea r8, AllocSize2SmallBlockTypesIdx
  lea rbx, SmallBlockTypes
  {Is it a small block?}
  cmp rcx, (MaximumSmallBlockSize - BlockHeaderSize)
  ja @NotASmallBlock
  {Get the small block type pointer in rbx}
  movzx ecx, byte ptr [r8 + rdx]
  {The offset in the array wan't be bigger than 2^32 anyway, but an ecx instruction takes one byte less than the rcx one}
  shl ecx, SmallBlockTypeRecSizePowerOf2
  add rbx, rcx
  {Do we need to lock the block type?}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMultithreaded)
  jnz @LockSmallBlockType
  jmp @AfterLockOnSmallBlockType
{$ELSE}
  jmp @LockSmallBlockType
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotLockOnSmallBlockType:
  {$IFDEF SmallBlocksLockedCriticalSection}{$IFDEF DebugAcquireLockByte}
  cmp al, cLockByteAvailable
  jne SmallBlockUnlockError
  {$ENDIF}{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitSmallLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@AfterLockOnSmallBlockType:

  {Find the next free block: Get the first pool with free blocks in rdx}
  mov rdx, TSmallBlockType[rbx].NextPartiallyFreePool
  {Get the first free block (or the next sequential feed address if rdx = rbx)}
  mov rax, TSmallBlockPoolHeader[rdx].FirstFreeBlock
  {Get the drop flags mask in rcx so long}
  mov rcx, DropSmallFlagsMask
  {Is there a pool with free blocks?}
  cmp rdx, rbx
  je @TrySmallSequentialFeed
  {Increment the number of used blocks}
  add TSmallBlockPoolHeader[rdx].BlocksInUse, 1
  {Get the new first free block}
  and rcx, [rax - BlockHeaderSize]
  {Set the new first free block}
  mov TSmallBlockPoolHeader[rdx].FirstFreeBlock, rcx
  {Set the block header}
  mov [rax - BlockHeaderSize], rdx
  {Is the chunk now full?}
  jz @RemoveSmallPool
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@UnlockSmallBlockAndExit:
  {Unlock the block type}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitSmallLocked)
  jz @Done
{$ENDIF}
  {$IFDEF DebugReleaseLockByte}
  cmp TSmallBlockType[rbx].SmallBlockTypeLocked, cLockByteLocked
  jne SmallBlockUnlockError
  {$ENDIF}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov TSmallBlockType[rbx].SmallBlockTypeLocked, cLockByteAvailable
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@TrySmallSequentialFeed:
  {Try to feed a small block sequentially: Get the sequential feed block pool}
  mov rdx, TSmallBlockType[rbx].CurrentSequentialFeedPool
  {Get the next sequential feed address so long}
  movzx ecx, TSmallBlockType[rbx].BlockSize
  add rcx, rax
  {Can another block fit?}
  cmp rax, TSmallBlockType[rbx].MaxSequentialFeedBlockAddress
  ja @AllocateSmallBlockPool
  {Increment the number of used blocks in the sequential feed pool}
  add TSmallBlockPoolHeader[rdx].BlocksInUse, 1
  {Store the next sequential feed block address}
  mov TSmallBlockType[rbx].NextSequentialFeedBlockAddress, rcx
  {Set the block header}
  mov [rax - BlockHeaderSize], rdx
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@RemoveSmallPool:
  {Pool is full - remove it from the partially free list}
  mov rcx, TSmallBlockPoolHeader[rdx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[rcx].PreviousPartiallyFreePool, rbx
  mov TSmallBlockType[rbx].NextPartiallyFreePool, rcx
  jmp @UnlockSmallBlockAndExit

{===== START OF SMALL BLOCK LOCKING CODE; 64-BIT FASTGETMEM =====}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}

@LockSmallBlockType:

{$IFDEF SmallBlocksLockedCriticalSection}
   mov  eax, cLockByteLocked
   mov  edx, Type(TSmallBlockType)
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @FirstBlockLocked
   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@FirstBlockLocked:
  {Try the next size}
   add  rbx, rdx
   cmp  TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @SecondBlockLocked
   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@SecondBlockLocked:
  {Try the next size (up to two sizes larger)}
   add  rbx, rdx
   cmp  TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @ThirdBlockLocked
   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@ThirdBlockLocked:
  {Block type and two sizes larger are all locked - give up and sleep}
   sub  rbx, rdx
   sub  rbx, rdx
   push rcx

   cmp  byte ptr TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @DidntLockAtFirstAttempt
   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   je   @DidntLockAtFirstAttempt
   jmp  @GotTheLock
@DidntLockAtFirstAttempt:
{$IFDEF EnableWaitPKG}
   test FastMMCpuFeaturesB, FastMMCpuFeatureB_WaitPKG
   jz   @NoWaitPKG

   push r8
   push r9
   push rdx
   lea  r8, TSmallBlockType([rbx]).SmallBlockTypeLocked
   jmp  @FirstLockMonitor

@DidntLockUmonitor:
   push r8
   push r9
   push rdx
   push rbx
   call SwitchToThreadIfSupported
   pop  rbx
   pop  rdx
   pop  r9
   pop  r8
   mov  eax, cLockByteLocked
   cmp  [r8], al
   jne  @TryXchgAfterUmonitor

@FirstLockMonitor:
   mov  eax, cLockByteLocked
   db   $F3, $41, $0F, $AE, $F0 // umonitor r8
   cmp  [r8], al
   jne  @TryXchgAfterUmonitor

   mov  eax, CUMWaitTime
   xor  rdx, rdx

   //   bit[0] = 0	C0.2	Slower	Larger	Improves performance of the other SMT thread(s) on the same core.
   //   bit[0] = 1	C0.1	Faster	Smaller	NA
   xor  r9, r9

   db   $F2, $41, $0F, $AE, $F1 // umwait, r9d
@TryXchgAfterUmonitor:
   mov  eax, cLockByteLocked
   lock xchg [r8], al
   cmp  al, cLockByteLocked
   je   @DidntLockUmonitor
   pop  rdx
   pop  r9
   pop  r8
   jmp  @GotTheLock

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NoWaitPKG:
{$ENDIF EnableWaitPKG}

  mov   edx, cPauseSpinWaitLoopCount

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@LockSmallBlockTypeLoop64:
@DidntLock:
@NormalLoadLoop:
   dec  edx
   jz   @SwitchToThread // for static branch prediction, jump forward means "unlikely"
   db   $F3, $90 // pause
 {$IFNDEF DebugAcquireLockByte}
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @NormalLoadLoop // for static branch prediction, jump backwards means "likely"
 {$ELSE}
  movzx eax, byte ptr TSmallBlockType([rbx]).SmallBlockTypeLocked
  cmp  al, cLockByteLocked
  je   @NormalLoadLoop
  cmp  al, cLockByteAvailable
  jne  SmallBlockUnlockError
  mov  eax, cLockByteLocked
 {$ENDIF}
  lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
  cmp  al, cLockByteLocked
  je   @DidntLock
@GotTheLock:
  pop   rcx
  {Congratulations! We've got the lock!}
  jmp	@GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@SwitchToThread:
  call  SwitchToThreadIfSupported
  mov   eax, cLockByteLocked
  mov   edx, cPauseSpinWaitLoopCount
  jmp   @LockSmallBlockTypeLoop64

{$ELSE !SmallBlocksLockedCriticalSection}

{ The 64-bit implemenation from the original FastMM4 that employs a loop of Sleep() or SwitchToThread().
By default, it will not be compiled into FastMM4-AVX which uses more efficient approach.}
@LockSmallBlockTypeLoop:
  mov eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  mov edx, eax
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
  {Try the next size}
  add rbx, Type(TSmallBlockType)
  mov eax, edx
  lock cmpxchg TSmallBlockType([rbx]).SmallBlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size (up to two sizes larger)}
  add rbx, Type(TSmallBlockType)
  mov eax, edx
  lock cmpxchg TSmallBlockType([rbx]).SmallBlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Block type and two sizes larger are all locked - give up and sleep}
  sub rbx, 2 * Type(TSmallBlockType)
{$IFDEF NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  db $F3, $90 // pause
  {$IFDEF UseSwitchToThread}
  call SwitchToThreadIfSupported
  {$ENDIF NeverSleepOnThreadContention}
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ELSE NeverSleepOnThreadContention}
  {Couldn't grab the block type - sleep and try again}
  push rdx {save rdx}
  mov ecx, InitialSleepTime
  call Sleep
  pop rax {restore previous value of rdx straight into rax}
  {Try again}
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).SmallBlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  mov ecx, AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ENDIF NeverSleepOnThreadContention}
{$ENDIF !SmallBlocksLockedCriticalSection}

{===== END OF SMALL BLOCK LOCKING CODE; 64-BIT FASTGETMEM =====}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@AllocateSmallBlockPool:
  {Do we need to lock the medium blocks?}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMultithreaded)
  jz @MediumBlocksLockedForPool
{$ENDIF}

 // Assembly versions of LockMediumBlocks / AcquireSpinLockMediumBlocks preserves all volatile register (except eax),
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
 // but even if we use a PurePascal version, we don't rely on any volatile registers, so we do not have to save them
  call LockMediumBlocks
{$ELSE}
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlocksLockedForPool:
  {Are there any available blocks of a suitable size?}
  movsx esi, TSmallBlockType[rbx].AllowedGroupsForBlockPoolBitmap
  and esi, MediumBlockBinGroupBitmap
  jz @NoSuitableMediumBlocks
  {Get the bin group number with free blocks in eax}
  bsf eax, esi
  {Get the bin number in ecx}
  lea r8, MediumBlockBinBitmaps
  lea r9, [rax * 4]
  mov ecx, [r8 + r9]
  bsf ecx, ecx
  lea ecx, [ecx + r9d * 8]
  {Get a pointer to the bin in edi}
  lea rdi, MediumBlockBins
  lea esi, [ecx * 8]
  lea rdi, [rdi + rsi * 2] //SizeOf(TMediumBlockBin) = 16
  {Get the free block in rsi}
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov rdx, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rdi
  {Is this bin now empty?}
  cmp rdi, rdx
  jne @MediumBinNotEmpty
  {r8 = @MediumBlockBinBitmaps, eax = bin group number,
   r9 = bin group number * 4, ecx = bin number, edi = @bin, esi = free block,
   ebx = block type}
  {Flag this bin as empty}
  mov edx, -2
  rol edx, cl
  and [r8 + r9], edx
  jnz @MediumBinNotEmpty
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBinNotEmpty:
  {esi = free block, ebx = block type}
  {Get the size of the available medium block in edi}
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  {Split the block: get the size of the second part, new block size is the
   optimal size}
  mov edx, edi
  movzx edi, TSmallBlockType[rbx].OptimalBlockPoolSize
  sub edx, edi
  {Split the block in two}
  lea rcx, [rsi + rdi]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Store the size of the second split as the second last qword}
  mov [rcx + rdx - BlockHeaderSize * 2], rdx
  {Put the remainder in a bin (it will be big enough)}
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NoSuitableMediumBlocks:
  {Check the sequential feed medium block pool for space}
  movzx ecx, TSmallBlockType[rbx].MinimumBlockPoolSize
  mov edi, MediumSequentialFeedBytesLeft
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  {Get the address of the last block that was fed}
  mov rsi, LastSequentiallyFedMediumBlock
  {Enough sequential feed space: Will the remainder be usable?}
  movzx ecx, TSmallBlockType[rbx].OptimalBlockPoolSize
  lea edx, [ecx + MinimumMediumBlockSize]
  cmp edi, edx
  jb @NotMuchSpace
  mov edi, ecx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotMuchSpace:
  sub rsi, rdi
  {Update the sequential feed parameters}
  sub MediumSequentialFeedBytesLeft, edi
  mov LastSequentiallyFedMediumBlock, rsi
  {Get the block pointer}
  jmp @GotMediumBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@AllocateNewSequentialFeed:
  {Need to allocate a new sequential feed medium block pool: use the
   optimal size for this small block pool}
  movzx ecx, TSmallBlockType[rbx].OptimalBlockPoolSize
  mov edi, ecx
  {Allocate the medium block pool}
  call AllocNewSequentialFeedMediumPool
  mov rsi, rax
  test rax, rax
  jnz @GotMediumBlock

{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @UnlockSmallBlockAndExit
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point}
  call UnlockMediumBlocks
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@UseWholeBlock:
  {rsi = free block, rbx = block type, edi = block size}
  {Mark this block as used in the block following it}
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotMediumBlock:
  {rsi = free block, rbx = block type, edi = block size}
  {Set the size and flags for this block}
  lea ecx, [edi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
  mov [rsi - BlockHeaderSize], rcx
  {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @NotLockedAftrGotMedBlk
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers
(RAX, RCX, RDX, R8, R9, R10, R11),
but we rely on nonvolatile (callee-saved) registers ( RBX, RBP, RDI, RSI, R12)}
  call UnlockMediumBlocks
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotLockedAftrGotMedBlk:
  {Set up the block pool}
  xor eax, eax
  mov TSmallBlockPoolHeader[rsi].BlockType, rbx
  mov TSmallBlockPoolHeader[rsi].FirstFreeBlock, rax
  mov TSmallBlockPoolHeader[rsi].BlocksInUse, 1
  {Set it up for sequential block serving}
  mov TSmallBlockType[rbx].CurrentSequentialFeedPool, rsi
  {Return the pointer to the first block}
  lea rax, [rsi + SmallBlockPoolHeaderSize]
  movzx ecx, TSmallBlockType[rbx].BlockSize
  lea rdx, [rax + rcx]
  mov TSmallBlockType[rbx].NextSequentialFeedBlockAddress, rdx
  add rdi, rsi
  sub rdi, rcx
  mov TSmallBlockType[rbx].MaxSequentialFeedBlockAddress, rdi
  {Set the small block header}
  mov [rax - BlockHeaderSize], rsi
  jmp @UnlockSmallBlockAndExit
{-------------------Medium block allocation-------------------}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@NotASmallBlock:
  cmp rcx, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  {Get the bin size for this block size. Block sizes are
   rounded up to the next bin size.
   Now we have a designed block size in ecx, it is for sure smaller than 32 bits,
   because it is less than the value of the MaximumMediumBlockSize constant,
   so we just use ecx/ebx here for smaller opcodes, not rcx/rbx  }
  lea ebx, [ecx + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  and ebx, MediumBlockGranularityMask
  add ebx, MediumBlockSizeOffset
  {Do we need to lock the medium blocks?}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMultithreaded)
  jz @MediumBlocksLocked
{$ENDIF}

  // Assembly versions of LockMediumBlocks / AcquireSpinLockMediumBlocks preserve all volatile register (except eax),
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  // but we do not rely on volatile register here, so even if we use a PurePascal implementation, we don't need to save registers here
  call LockMediumBlocks
{$ELSE}
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlocksLocked:
  {Get the bin number in ecx and the group number in edx}
  lea edx, [ebx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  {Is there a suitable block inside this group?}
  mov eax, -1
  shl eax, cl
  lea r8, MediumBlockBinBitmaps
  and eax, [r8 + rdx * 4]
  jz @GroupIsEmpty
  {Get the actual bin number}
  and ecx, -MediumBlockBinsPerGroup
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GroupIsEmpty:
  {Try all groups greater than this group}
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, MediumBlockBinGroupBitmap
  jz @TrySequentialFeedMedium
  {There is a suitable group with space: get the bin number}
  bsf edx, eax
  {Get the bin in the group with free blocks}
  mov eax, [r8 + rdx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, MediumBlockBinsPerGroupPowerOf2
  or ecx, eax
  jmp @GotBinAndGroup
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@TrySequentialFeedMedium:
  mov ecx, MediumSequentialFeedBytesLeft
  {Block can be fed sequentially?}
  sub ecx, ebx
  jc @AllocateNewSequentialFeedForMedium
  {Get the block address}
  mov rax, LastSequentiallyFedMediumBlock
  sub rax, rbx
  mov LastSequentiallyFedMediumBlock, rax
  {Store the remaining bytes}
  mov MediumSequentialFeedBytesLeft, ecx
  {Set the flags for the block}
  or rbx, IsMediumBlockFlag
  mov [rax - BlockHeaderSize], rbx
  jmp @MediumBlockGetDone
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@AllocateNewSequentialFeedForMedium:
  mov ecx, ebx
  call AllocNewSequentialFeedMediumPool
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockGetDone:
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @Done
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point - we only save RAX}
  mov rsi, rax
  call UnlockMediumBlocks
  mov rax, rsi
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotBinAndGroup:
  {ebx = block size, ecx = bin number, edx = group number}
  {Get a pointer to the bin in edi}
  lea rdi, MediumBlockBins
  lea eax, [ecx + ecx]
  lea rdi, [rdi + rax * 8]
  {Get the free block in esi}
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov rax, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rax
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rdi
  {Is this bin now empty?}
  cmp rdi, rax
  jne @MediumBinNotEmptyForMedium
  {edx = bin group number, ecx = bin number, rdi = @bin, rsi = free block, ebx = block size}
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  lea r8, MediumBlockBinBitmaps
  and [r8 + rdx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBinNotEmptyForMedium:
  {rsi = free block, ebx = block size}
  {Get the size of the available medium block in edi}
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  {Get the size of the second split in edx}
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  {Split the block in two}
  lea rcx, [rsi + rbx]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Store the size of the second split as the second last dword}
  mov [rcx + rdx - BlockHeaderSize * 2], rdx
  {Put the remainder in a bin}
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlockForMedium
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@UseWholeBlockForMedium:
  {Mark this block as used in the block following it}
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotMediumBlockForMedium:
  {Set the size and flags for this block}
  lea rcx, [rbx + IsMediumBlockFlag]
  mov [rsi - BlockHeaderSize], rcx
  mov rax, rsi
  {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @Done
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point - we only save RAX}
  call UnlockMediumBlocks
  mov rax, rsi
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  jmp @Done
{-------------------Large block allocation-------------------}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@IsALargeBlockRequest:
  xor rax, rax
  test rcx, rcx
  js @Done
  call AllocateLargeBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@Done: {it automatically restores 4 registers from stack}
{$IFNDEF AllowAsmParams}
 {$IFNDEF AssumeMultiThreaded}
  pop r12
 {$ENDIF}
  pop rdi
  pop rsi
  pop rbx
{$ENDIF}
{$IFDEF DEBUG}
  test rax, AlignmentMask
  jz @@OkAlignmentOnGetMemD
  jmp    BadAlignmentOnGetMem
@@OkAlignmentOnGetMemD:
{$ENDIF}

end;
{$ENDIF}
{$ENDIF FastGetMemNeedAssemblerCode}

{$IFNDEF FastFreememNeedAssemberCode}
{Frees a medium block, returning 0 on success, -1 otherwise}
function FreeMediumBlock(APointer: Pointer
  {$IFDEF UseReleaseStack}; ACleanupOperation: Boolean = false{$ENDIF}): Integer;
var
  LNextMediumBlock: PMediumFreeBlock;
{$IFNDEF FullDebugMode}
  LPreviousMediumBlock: PMediumFreeBlock;
{$ENDIF}
  LNextMediumBlockSizeAndFlags: NativeUInt;
  LBlockSize: Cardinal;
{$IFNDEF FullDebugMode}
  LPreviousMediumBlockSize: Cardinal;
{$ENDIF}
{$IFNDEF FullDebugMode}
  LPPreviousMediumBlockPoolHeader,
  LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
{$ENDIF}
  LBlockHeader: NativeUInt;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
  LStackTrace: TStackTrace;
{$ENDIF}
{$IFDEF UseReleaseStack}
  LDelayRelease: Boolean;
  LPReleaseStack: ^TLFStack;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LWasMultiThread: Boolean;
{$ENDIF}
  LMediumBlocksLocked: Boolean;
begin
  LMediumBlocksLocked := False;
{$IFNDEF AssumeMultiThreaded}
  LWasMultiThread := False;
{$ENDIF}
{$IFDEF LogLockContention}
  LDidSleep := False;
{$ENDIF}
  {Get the block header}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Get the medium block size}
  LBlockSize := LBlockHeader and DropMediumAndLargeFlagsMask;
  {When running a cleanup operation, medium blocks are already locked.}
{$IFDEF UseReleaseStack}
  if not ACleanupOperation then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
   if IsMultiThread then
{$ENDIF}
   begin
{$IFNDEF AssumeMultiThreaded}
     LWasMultiThread := True;
{$ENDIF}
     LMediumBlocksLocked := True;
    {Lock the medium blocks}
    {$IFDEF LogLockContention}LDidSleep:={$ENDIF} LockMediumBlocks(
      {$IFDEF UseReleaseStack}APointer, @LDelayRelease{$ENDIF});
    {$IFDEF UseReleaseStack}
    if LDelayRelease then
    begin
      Result := 0;
      Exit;
    end;
    {$ENDIF UseReleaseStack}
   end;
  end;
{$IFDEF LogLockContention}
  if LDidSleep then
  begin
    GetStackTrace(@(LStackTrace[0]), StackTraceDepth, 1);
    MediumBlockCollector.Add(@(LStackTrace[0]), StackTraceDepth);
  end;
{$ENDIF}
{$IFDEF UseReleaseStack}
  repeat
{$ENDIF}
    {Can we combine this block with the next free block?}
    LNextMediumBlock := PMediumFreeBlock(PByte(APointer) + LBlockSize);
    LNextMediumBlockSizeAndFlags := PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^;
{$IFNDEF FullDebugMode}
  {$IFDEF CheckHeapForCorruption}
    {Check that this block was flagged as in use in the next block}
    if (LNextMediumBlockSizeAndFlags and PreviousMediumBlockIsFreeFlag) <> 0 then
    {$IFDEF BCB6OrDelphi7AndUp}
      System.Error(reInvalidPtr);
    {$ELSE}
      System.RunError(reInvalidPtr);
    {$ENDIF}
  {$ENDIF}
    if (LNextMediumBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
    begin
      {Increase the size of this block}
      Inc(LBlockSize, LNextMediumBlockSizeAndFlags and DropMediumAndLargeFlagsMask);
      {Remove the next block as well}
      if LNextMediumBlockSizeAndFlags >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LNextMediumBlock);
    end
    else
    begin
{$ENDIF}
      {Reset the "previous in use" flag of the next block}
      PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^ := LNextMediumBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
{$IFNDEF FullDebugMode}
    end;
    {Can we combine this block with the previous free block? We need to
     re-read the flags since it could have changed before we could lock the
     medium blocks.}
    if (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
    begin
      {Get the size of the free block just before this one}
      LPreviousMediumBlockSize := PNativeUInt(PByte(APointer) - 2 * BlockHeaderSize)^;
      {Get the start of the previous block}
      LPreviousMediumBlock := PMediumFreeBlock(PByte(APointer) - LPreviousMediumBlockSize);
    {$IFDEF CheckHeapForCorruption}
      {Check that the previous block is actually free}
      if (PNativeUInt(PByte(LPreviousMediumBlock) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag) then
      {$IFDEF BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
      {$ELSE}
        System.RunError(reInvalidPtr);
      {$ENDIF}
    {$ENDIF}
      {Set the new block size}
      Inc(LBlockSize, LPreviousMediumBlockSize);
      {This is the new current block}
      APointer := LPreviousMediumBlock;
      {Remove the previous block from the linked list}
      if LPreviousMediumBlockSize >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LPreviousMediumBlock);
    end;
  {$IFDEF CheckHeapForCorruption}
    {Check that the previous block is currently flagged as in use}
    if (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
    {$IFDEF BCB6OrDelphi7AndUp}
      System.Error(reInvalidPtr);
    {$ELSE}
      System.RunError(reInvalidPtr);
    {$ENDIF}
  {$ENDIF}
    {Is the entire medium block pool free, and there are other free blocks
     that can fit the largest possible medium block? -> free it. (Except in
     full debug mode where medium pools are never freed.)}
    if (LBlockSize <> (MediumBlockPoolSize - MediumBlockPoolHeaderSize)) then
    begin
      {Store the size of the block as well as the flags}
      PNativeUInt(PByte(APointer) - BlockHeaderSize)^ := LBlockSize or (IsMediumBlockFlag or IsFreeBlockFlag);
{$ELSE}
      {Mark the block as free}
      Inc(PNativeUInt(PByte(APointer) - BlockHeaderSize)^, IsFreeBlockFlag);
{$ENDIF FullDebugMode}
      {Store the trailing size marker}
      PNativeUInt(PByte(APointer) + LBlockSize - 2 * BlockHeaderSize)^ := LBlockSize;
      {Insert this block back into the bins: Size check not required here,
       since medium blocks that are in use are not allowed to be
       shrunk smaller than MinimumMediumBlockSize}
      InsertMediumBlockIntoBin(APointer, LBlockSize);
{$IFNDEF FullDebugMode}
  {$IFDEF CheckHeapForCorruption}
      {Check that this block is actually free and the next and previous blocks are both in use.}
      if ((PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag))
        or ((PNativeUInt(PByte(APointer) + (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and IsFreeBlockFlag) <> 0) then
      begin
    {$IFDEF BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$ELSE}
        System.RunError(reInvalidPtr);
    {$ENDIF}
      end;
  {$ENDIF}
{$ENDIF}
  {$IFNDEF UseReleaseStack}
{$IFNDEF AssumeMultiThreaded}
      if LWasMultiThread then
{$ENDIF}
      begin
        if LMediumBlocksLocked then
        begin
          LMediumBlocksLocked := False;
          {Unlock medium blocks}
          UnlockMediumBlocks;
        end;
      end;
  {$ENDIF}
      {All OK}
      Result := 0;
{$IFNDEF FullDebugMode}
    end
    else
    begin
      {Should this become the new sequential feed?}
      if MediumSequentialFeedBytesLeft <> MediumBlockPoolSize - MediumBlockPoolHeaderSize then
      begin
        {Bin the current sequential feed}
        BinMediumSequentialFeedRemainder;
        {Set this medium pool up as the new sequential feed pool:
         Store the sequential feed pool trailer}
        PNativeUInt(PByte(APointer) + LBlockSize - BlockHeaderSize)^ := IsMediumBlockFlag;
        {Store the number of bytes available in the sequential feed chunk}
        MediumSequentialFeedBytesLeft := MediumBlockPoolSize - MediumBlockPoolHeaderSize;
        {Set the last sequentially fed block}
        LastSequentiallyFedMediumBlock := Pointer(PByte(APointer) + LBlockSize);
  {$IFNDEF UseReleaseStack}
        if LMediumBlocksLocked then
        begin
          LMediumBlocksLocked := False;
          {Unlock medium blocks}
           UnlockMediumBlocks;
        end;
  {$ENDIF}
        {Success}
        Result := 0;
      end
      else
      begin
        {Remove this medium block pool from the linked list}
        Dec(PByte(APointer), MediumBlockPoolHeaderSize);
        LPPreviousMediumBlockPoolHeader := PMediumBlockPoolHeader(APointer)^.PreviousMediumBlockPoolHeader;
        LPNextMediumBlockPoolHeader := PMediumBlockPoolHeader(APointer)^.NextMediumBlockPoolHeader;
        LPPreviousMediumBlockPoolHeader^.NextMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
        LPNextMediumBlockPoolHeader^.PreviousMediumBlockPoolHeader := LPPreviousMediumBlockPoolHeader;
        if LMediumBlocksLocked then
        begin
          LMediumBlocksLocked := False;
          {Unlock medium blocks}
          UnlockMediumBlocks;
        end;
  {$IFDEF ClearMediumBlockPoolsBeforeReturningToOS}
        FillChar(APointer^, MediumBlockPoolSize, 0);
  {$ENDIF}
        {Free the medium block pool}
        if VirtualFree(APointer, 0, MEM_RELEASE) then
          Result := 0
        else
          Result := -1;
  {$IFDEF UseReleaseStack}
        {Medium blocks are already unlocked so we can't continue unwinding the release stack.}
        Break;
  {$ENDIF UseReleaseStack}
      end;
    end;
{$ENDIF FullDebugMode}
{$IFDEF UseReleaseStack}
    if (Result <> 0) or ACleanupOperation then
    begin
      if LMediumBlocksLocked then
      begin
        LMediumBlocksLocked := False;
        UnlockMediumBlocks;
      end;
      Break;
    end;
    LPReleaseStack := @MediumReleaseStack[GetStackSlot];
    if LPReleaseStack^.IsEmpty or (not LPReleaseStack.Pop(APointer)) then
    begin
      if LMediumBlocksLocked then
      begin
        LMediumBlocksLocked := False;
        UnlockMediumBlocks;
      end;
      Break;
    end;
    {Get the block header}
    LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
    {Get the medium block size}
    LBlockSize := LBlockHeader and DropMediumAndLargeFlagsMask;
  until False;
{$ENDIF UseReleaseStack}
end;
{$ENDIF FastFreememNeedAssemberCode}

{$IFDEF DEBUG}
procedure BadAlignmentOnFreeMem;
begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
end;
{$ENDIF}

{Replacement for SysFreeMem}
function FastFreeMem(APointer: Pointer): {$IFDEF fpc}{$IFDEF CPU64}PtrUInt{$ELSE}NativeUInt{$ENDIF}{$ELSE}Integer{$ENDIF};
{$IFNDEF FastFreememNeedAssemberCode}
const
  CFastFreeMemReturnValueError = {$IFDEF fpc}NativeUInt(-1){$ELSE}-1{$ENDIF};
var
  LPSmallBlockPool: PSmallBlockPoolHeader;
{$IFNDEF FullDebugMode}
  LPPreviousPool,
  LPNextPool: PSmallBlockPoolHeader;
{$ENDIF}
  LPOldFirstPool: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
  LOldFirstFreeBlock: Pointer;
  LBlockHeader: NativeUInt;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
  LStackTrace: TStackTrace;
{$ENDIF}
{$IFDEF UseReleaseStack}
  LPReleaseStack: ^TLFStack;
{$ENDIF}
{$IFDEF SmallBlocksLockedCriticalSection}
  LSmallBlockCriticalSectionIndex: NativeUInt;
  LFailedToAcquireLock: Boolean;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LWasMultithread: Boolean;
{$ENDIF}
  LSmallBlockWithoutLock: Boolean;
  LFreeMediumBlockError: Boolean;
begin
  LSmallBlockWithoutLock := False;
  LFreeMediumBlockError := False;
{$IFNDEF AssumeMultiThreaded}
  LWasMultithread := False;
{$ENDIF}
{$IFDEF SmallBlocksLockedCriticalSection}
  LSmallBlockCriticalSectionIndex := MaxInt;
  LFailedToAcquireLock := False;
{$ENDIF}
  {$IFDEF fpc}
  if APointer = nil then
  begin
    Result := 0;
    Exit;
  end;
  {$ENDIF}
  {Get the small block header: Is it actually a small block?}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Is it a small block that is in use?}
  if (LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag)) = 0 then
  begin
    {Get a pointer to the block pool}
    LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader);
    {Get the block type}
    LPSmallBlockType := LPSmallBlockPool^.BlockType;
{$IFDEF ClearSmallAndMediumBlocksInFreeMem}
    FillChar(APointer^, LPSmallBlockType^.BlockSize - BlockHeaderSize, 0);
{$ENDIF}
    {Lock the block type}
{$IFDEF LogLockContention}
    LDidSleep := False;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
    if IsMultiThread then
{$ENDIF}
    begin
      {$IFNDEF AssumeMultiThreaded}
      LWasMultithread := True;
      {$ENDIF}

{$IFDEF SmallBlocksLockedCriticalSection}
      {$IFNDEF DisablePauseAndSwitchToThread}
      if CpuFeaturePauseAndSwitch then
      begin
        if not AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
        begin
          LFailedToAcquireLock := True;
          AcquireSpinLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
        end;
      end else
      {$ENDIF}
      begin
        LFailedToAcquireLock := not AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
        LSmallBlockCriticalSectionIndex := (NativeUint(LPSmallBlockType)-NativeUint(@(SmallBlockTypes[0])))
          {$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
            shr SmallBlockTypeRecSizePowerOf2
          {$ELSE}
            div SmallBlockTypeRecSize
         {$ENDIF}
        ;
        EnterCriticalSection(SmallBlockCriticalSections[LSmallBlockCriticalSectionIndex]);
        if LFailedToAcquireLock then
        begin
          if not AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          begin
            LSmallBlockWithoutLock := True;
          end;
        end;
      end;

{$ELSE SmallBlocksLockedCriticalSection}

      while not (AcquireLockByte(LPSmallBlockType.SmallBlockTypeLocked)) do
      begin
{$IFDEF UseReleaseStack}
        LPReleaseStack := @(LPSmallBlockType^.ReleaseStack[GetStackSlot]);
        if (not LPReleaseStack^.IsFull) and LPReleaseStack^.Push(APointer) then
        begin
          {Block will be released later.}
          Result := 0;
          Exit;
        end;
{$ENDIF}
{$IFDEF LogLockContention}
        LDidSleep := True;
{$ENDIF}
{$IFDEF NeverSleepOnThreadContention}
  {$IFDEF UseSwitchToThread}
        SwitchToThreadIfSupported;
  {$ENDIF}
{$ELSE}
        Sleep(InitialSleepTime);
        if AcquireLockByte(LPSmallBlockType^.SmallBlockTypeLocked) then
          Break;
        Sleep(AdditionalSleepTime);
{$ENDIF}
      end;

{$ENDIF SmallBlocksLockedCriticalSection}


    end;
{$IFDEF LogLockContention}
    if LDidSleep then
    begin
      GetStackTrace(@(LStackTrace[0]), StackTraceDepth, 1);
      LPSmallBlockType^.BlockCollector.Add(@(LStackTrace[0]), StackTraceDepth);
    end;
{$ENDIF}
{$IFDEF UseReleaseStack}
    while True do
    begin
{$ENDIF}
      {Get the old first free block}
      LOldFirstFreeBlock := LPSmallBlockPool^.FirstFreeBlock;
      {Was the pool manager previously full?}
      if LOldFirstFreeBlock = nil then
      begin
        {Insert this as the first partially free pool for the block size}
        LPOldFirstPool := LPSmallBlockType^.NextPartiallyFreePool;
        LPSmallBlockPool^.NextPartiallyFreePool := LPOldFirstPool;
        LPOldFirstPool^.PreviousPartiallyFreePool := LPSmallBlockPool;
        LPSmallBlockPool^.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
        LPSmallBlockType^.NextPartiallyFreePool := LPSmallBlockPool;
      end;
      {Store the old first free block}
      PNativeUInt(PByte(APointer) - BlockHeaderSize)^ := UIntPtr(LOldFirstFreeBlock) or IsFreeBlockFlag;
      {Store this as the new first free block}
      LPSmallBlockPool^.FirstFreeBlock := APointer;
      {Decrement the number of allocated blocks}
      Dec(LPSmallBlockPool^.BlocksInUse);
      {Small block pools are never freed in full debug mode. This increases the
       likehood of success in catching objects still being used after being
       destroyed.}
{$IFNDEF FullDebugMode}
      {Is the entire pool now free? -> Free it.}
      if LPSmallBlockPool^.BlocksInUse = 0 then
      begin
        {Get the previous and next chunk managers}
        LPPreviousPool := LPSmallBlockPool^.PreviousPartiallyFreePool;
        LPNextPool := LPSmallBlockPool^.NextPartiallyFreePool;
        {Remove this manager}
        LPPreviousPool^.NextPartiallyFreePool := LPNextPool;
        LPNextPool^.PreviousPartiallyFreePool := LPPreviousPool;
        {Is this the sequential feed pool? If so, stop sequential feeding}
        if (LPSmallBlockType^.CurrentSequentialFeedPool = LPSmallBlockPool) then
        begin
          LPSmallBlockType^.MaxSequentialFeedBlockAddress := nil;
        end;
        {$IFNDEF AssumeMultiThreaded}
        if LWasMultithread then
        {$ENDIF}
        begin
          {Unlock this block type}
          if not LSmallBlockWithoutLock then
          begin
            ReleaseLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
          end else
          begin
            LSmallBlockWithoutLock := False;
          end;
          {$IFDEF SmallBlocksLockedCriticalSection}
          if LSmallBlockCriticalSectionIndex <> NativeUInt(MaxInt) then
          begin
            LeaveCriticalSection(SmallBlockCriticalSections[LSmallBlockCriticalSectionIndex]);
            LSmallBlockCriticalSectionIndex := MaxInt;
          end;
          {$ENDIF}
        end;
        {Free the block pool}
        if FreeMediumBlock(LPSmallBlockPool) <> 0 then
        begin
          LFreeMediumBlockError := True;
        end;
{$IFDEF UseReleaseStack}
        {Stop unwinding the release stack.}
        Break;
{$ENDIF}
      end
      else
      begin
{$ENDIF}
{$IFDEF UseReleaseStack}
        LPReleaseStack := @LPSmallBlockType^.ReleaseStack[GetStackSlot];
        if LPReleaseStack^.IsEmpty or (not LPReleaseStack^.Pop(APointer)) then
        begin
{$ENDIF}

          {$IFNDEF AssumeMultiThreaded}
          if LWasMultithread then
          {$ENDIF}
          begin
          {Unlock this block type}
            if not LSmallBlockWithoutLock then
            begin
              ReleaseLockByte(LPSmallBlockType^.SmallBlockTypeLocked);
            end else
            begin
              LSmallBlockWithoutLock := False;
            end;
            {$IFDEF SmallBlocksLockedCriticalSection}
            if LSmallBlockCriticalSectionIndex <> NativeUInt(MaxInt) then
            begin
              LeaveCriticalSection(SmallBlockCriticalSections[LSmallBlockCriticalSectionIndex]);
              LSmallBlockCriticalSectionIndex := NativeUInt(MaxInt);
            end;
            {$ENDIF}
          end;
{$IFDEF UseReleaseStack}
          Break;
        end;
        LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
        LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader);
{$ENDIF}
{$IFNDEF FullDebugMode}
      end;
{$ENDIF}
{$IFDEF UseReleaseStack}
    end;
{$ENDIF}
    if LFreeMediumBlockError then
    begin
      Result := CFastFreeMemReturnValueError;
    end else
    begin
      {No error}
      Result := 0;
    end;
  end
  else
  begin
    {Is this a medium block or a large block?}
    if (LBlockHeader and (IsFreeBlockFlag or IsLargeBlockFlag)) = 0 then
    begin
{$IFDEF ClearSmallAndMediumBlocksInFreeMem}
      {Get the block header, extract the block size and clear the block it.}
      LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
      FillChar(APointer^,
        (LBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize, 0);
{$ENDIF}
      Result := FreeMediumBlock(APointer);
    end
    else
    begin
      {Validate: Is this actually a Large block, or is it an attempt to free an
       already freed small block?}
      if (LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag)) = 0 then
        Result := FreeLargeBlock(APointer)
      else
        Result := CFastFreeMemReturnValueError;
    end;
  end;
end;
{$ELSE FastFreememNeedAssemberCode}
{$IFDEF 32Bit}
assembler;
asm
  {$IFDEF fpc}
  test eax, eax
  jne @PointerNotNil
  jmp @Finish
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@PointerNotNil:
  {$ENDIF}
  {Get the block header in edx}
  mov edx, [eax - BlockHeaderSize]
  {Save the pointer in ecx}
  mov ecx, eax
{The EBP register is not used in FastFreeMem, so we will usee it
for flags like IsMultiThreaded or MediumBlocksLocked}

{$IFNDEF AssumeMultiThreaded}
  push ebp
  xor ebp, ebp
{$ENDIF}

  {Save ebx}
  push ebx
  xor ebx, ebx

  {Get the IsMultiThread variable}
{$IFNDEF AssumeMultiThreaded}
  {Branchless code to avoid misprediction}
  cmp byte ptr [IsMultiThread], 0
  setnz bl
  shl ebx, StateBitMultithreaded
  or ebp, ebx
{$ENDIF}


  {Is it a small block in use?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {the test+jnz instructions are together to allow macro-op fusion}
  jnz @NotSmallBlockInUse
{$IFDEF ClearSmallAndMediumBlocksInFreeMem}
  push edx
  push ecx
  mov edx, TSmallBlockPoolHeader[edx].BlockType
  movzx edx, TSmallBlockType(edx).BlockSize
  sub edx, BlockHeaderSize
  xor ecx, ecx
  call System.@FillChar
  pop ecx
  pop edx
{$ENDIF}
  {Do we need to lock the block type?}
  {Get the small block type in ebx}
  mov ebx, TSmallBlockPoolHeader[edx].BlockType
  {Do we need to lock the block type?}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMultithreaded)
  jnz @LockSmallBlockType {test+jnz provide macro-op fusion}
  jmp @AfterLock
{$ELSE}
  jmp @LockSmallBlockType
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotLockOnSmallBlockType:
  {$IFDEF SmallBlocksLockedCriticalSection}{$IFDEF DebugAcquireLockByte}
  cmp al, cLockByteAvailable
  jne SmallBlockUnlockError
  {$ENDIF}{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or ebp, (UnsignedBit shl StateBitSmallLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@AfterLock:
  {Current state: edx = @SmallBlockPoolHeader, ecx = APointer, ebx = @SmallBlockType}
  {Decrement the number of blocks in use}
  sub TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Get the old first free block}
  mov eax, TSmallBlockPoolHeader[edx].FirstFreeBlock
  {Is the pool now empty?}
  jz @PoolIsNowEmpty
  {Was the pool full?}
  test eax, eax
  {Store this as the new first free block}
  mov TSmallBlockPoolHeader[edx].FirstFreeBlock, ecx
  {Store the previous first free block as the block header}
  lea eax, [eax + IsFreeBlockFlag]
  mov [ecx - BlockHeaderSize], eax
  {Insert the pool back into the linked list if it was full}
  jz @SmallPoolWasFull
  {All ok}
  xor eax, eax

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}

@UnlockSmallBlockAndExit:
  {Unlock the block type}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitSmallLocked)
  jz @Exit
{$ENDIF}
  {$IFDEF DebugReleaseLockByte}
  cmp TSmallBlockType[ebx].SmallBlockTypeLocked, cLockByteLocked
  jne SmallBlockUnlockError
  {$ENDIF}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov TSmallBlockType[ebx].SmallBlockTypeLocked, cLockByteAvailable
  jmp @Exit

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@SmallPoolWasFull:
  {Insert this as the first partially free pool for the block size}
  mov eax, TSmallBlockType[ebx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[edx].PreviousPartiallyFreePool, ebx
  mov TSmallBlockPoolHeader[edx].NextPartiallyFreePool, eax
  mov TSmallBlockPoolHeader[eax].PreviousPartiallyFreePool, edx
  mov TSmallBlockType[ebx].NextPartiallyFreePool, edx
  {All ok}
  xor eax, eax
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PoolIsNowEmpty:
  { mark the current block as released to prevent further call to FreeMem}
  or dword ptr [ecx - BlockHeaderSize], IsFreeBlockFlag
  {Was this pool actually in the linked list of pools with space? If not, it
   can only be the sequential feed pool (it is the only pool that may contain
   only one block, i.e. other blocks have not been split off yet)}
  test eax, eax
  jz @IsSequentialFeedPool
  {Pool is now empty: Remove it from the linked list and free it}
  mov eax, TSmallBlockPoolHeader[edx].PreviousPartiallyFreePool
  mov ecx, TSmallBlockPoolHeader[edx].NextPartiallyFreePool
  {Remove this manager}
  mov TSmallBlockPoolHeader[eax].NextPartiallyFreePool, ecx
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, eax
  {Zero out eax}
  xor eax, eax
  {Is this the sequential feed pool? If so, stop sequential feeding}
  cmp TSmallBlockType[ebx].CurrentSequentialFeedPool, edx
  jne @NotSequentialFeedPool
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@IsSequentialFeedPool:
  mov TSmallBlockType[ebx].MaxSequentialFeedBlockAddress, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSequentialFeedPool:
  {Unlock the block type}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitSmallLocked)
  jz @DontUnlckSmlBlkAftrNotSeqFdPl
{$ENDIF}
  {$IFDEF DebugReleaseLockByte}
  cmp TSmallBlockType[ebx].SmallBlockTypeLocked, cLockByteLocked
  jne SmallBlockUnlockError
  {$ENDIF}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov TSmallBlockType[ebx].SmallBlockTypeLocked, cLockByteAvailable
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontUnlckSmlBlkAftrNotSeqFdPl:
  {Release this pool}
  mov eax, edx
  mov edx, [edx - 4]
  jmp @FreeMediumBlock

{===== START OF SMALL BLOCK LOCKING CODE; 32-BIT FASTFREEMEM =====}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@LockSmallBlockType:
{$IFDEF SmallBlocksLockedCriticalSection}
  mov  eax, cLockByteLocked
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
  cmp  TSmallBlockType([ebx]).SmallBlockTypeLocked, al
  je   @PrepareForSpinLoop
  lock xchg TSmallBlockType([ebx]).SmallBlockTypeLocked, al
  cmp  al, cLockByteLocked
  jne  @GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@PrepareForSpinLoop:
  push edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@LockSmallBlockTypeLoop:
  mov  edx, cPauseSpinWaitLoopCount
  mov  eax, cLockByteLocked
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@DidntLock:
@NormalLoadLoop:
  dec  edx
  jz   @SwitchToThread // for static branch prediction, jump forward means "unlikely"
  db $F3, $90 // pause
 {$IFNDEF DebugAcquireLockByte}
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
  cmp  TSmallBlockType([ebx]).SmallBlockTypeLocked, al
  je   @NormalLoadLoop // for static branch prediction, jump backwards means "likely"
 {$ELSE}
  movzx eax, byte ptr TSmallBlockType([ebx]).SmallBlockTypeLocked
  cmp  al, cLockByteLocked
  je   @NormalLoadLoop
  cmp  al, cLockByteAvailable
  jne  SmallBlockUnlockError
  mov  eax, cLockByteLocked
 {$ENDIF}
  lock xchg TSmallBlockType([ebx]).SmallBlockTypeLocked, al
  cmp  al, cLockByteLocked
  je   @DidntLock
  {Congratulations! We've got the lock!}
  pop  edx
  jmp	@GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
  @SwitchToThread:
  push  ebx
  push  ecx
  push  esi
  push  edi
  push  ebp
  call  SwitchToThreadIfSupported
  pop   ebp
  pop   edi
  pop   esi
  pop   ecx
  pop   ebx

  jmp   @LockSmallBlockTypeLoop

{$ELSE !SmallBlocksLockedCriticalSection}

{ The 32-bit implemenation from the original FastMM4 that employs a loop of Sleep() or SwitchToThread().
By default, it will not be compiled into FastMM4-AVX which uses more efficient approach.}
@LockSmallBlockTypeLoop:
  mov eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
{$IFDEF NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  db $F3, $90 // pause
  {$IFDEF UseSwitchToThread}
  push ecx
  push edx
  call SwitchToThreadIfSupported
  pop edx
  pop ecx
  {$ENDIF}
  {Try again}
  jmp @LockSmallBlockTypeLoop
  {Align branch target}
{$ELSE}
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push InitialSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  mov eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push AdditionalSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ENDIF}

{$ENDIF !SmallBlocksLockedCriticalSection}

{===== END OF SMALL BLOCK LOCKING CODE; 32-BIT FASTFREEMEM =====}


  {---------------------Medium blocks------------------------------}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSmallBlockInUse:
  {Not a small block in use: is it a medium or large block?}
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrMediumBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@FreeMediumBlock:
{$IFDEF ClearSmallAndMediumBlocksInFreeMem}
  push eax
  push edx
  and edx, DropMediumAndLargeFlagsMask
  sub edx, BlockHeaderSize
  xor ecx, ecx
  call System.@FillChar
  pop edx
  pop eax
{$ENDIF}
  {Drop the flags}
  and edx, DropMediumAndLargeFlagsMask
  {Free the medium block pointed to by eax, header in edx}
  {Block size in ebx}
  mov ebx, edx
  {Save registers}
  push esi
  {Pointer in esi}
  mov esi, eax
  {Do we need to lock the medium blocks?}
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMultithreaded)
  jz @MediumBlocksLocked
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  // Assembly version of LockMediumBlocks preserves all volatile register (except eax), but non-assembly version does not, so we save ecx and edx for non-assembly implementation
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push ecx; push edx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop edx; pop ecx {$ENDIF}
{$ELSE}
  // For AcquireSpinLockMediumBlocks, we have only the assembly version, and it saves all volatile registers
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or ebp, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlocksLocked:
  {Can we combine this block with the next free block?}
  test dword ptr [esi + ebx - BlockHeaderSize], IsFreeBlockFlag
  {Get the next block size and flags in ecx}
  mov ecx, [esi + ebx - BlockHeaderSize]
  jnz @NextBlockIsFree
  {Set the "PreviousIsFree" flag in the next block}
  or ecx, PreviousMediumBlockIsFreeFlag
  mov [esi + ebx - BlockHeaderSize], ecx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NextBlockChecked:
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  test byte ptr [esi - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PreviousBlockChecked:
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block -> free it.}
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@BinFreeMediumBlock:
  {Store the size of the block as well as the flags}
  lea eax, [ebx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [esi - BlockHeaderSize], eax
  {Store the trailing size marker}
  mov [esi + ebx - BlockHeaderSize*2], ebx
  {Insert this block back into the bins: Size check not required here,
   since medium blocks that are in use are not allowed to be
   shrunk smaller than MinimumMediumBlockSize}
  mov eax, esi
  mov edx, ebx
  {Insert into bin}
  call InsertMediumBlockIntoBin
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz  @DontUnlckMedBlksAftrBinFrMedBlk
{$ENDIF}
  {Unlock medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {it destroys ecx and edx, but we no longer need them}
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@DontUnlckMedBlksAftrBinFrMedBlk:
  {All OK}
  xor eax, eax
  {Restore registers}
  pop esi
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NextBlockIsFree:
  {Get the next block address in eax}
  lea eax, [esi + ebx]
  {Increase the size of this block}
  and ecx, DropMediumAndLargeFlagsMask
  add ebx, ecx
  {Was the block binned?}
  cmp ecx, MinimumMediumBlockSize
  jb @NextBlockChecked
  call RemoveMediumFreeBlock
  jmp @NextBlockChecked
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PreviousBlockIsFree:
  {Get the size of the free block just before this one}
  mov ecx, [esi - BlockHeaderSize*2]
  {Include the previous block}
  sub esi, ecx
  {Set the new block size}
  add ebx, ecx
  {Remove the previous block from the linked list}
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov eax, esi
  call RemoveMediumFreeBlock
  jmp @PreviousBlockChecked
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@EntireMediumPoolFree:
  {Should we make this the new sequential feed medium block pool? If the
   current sequential feed pool is not entirely free, we make this the new
   sequential feed pool.}
  cmp MediumSequentialFeedBytesLeft, MediumBlockPoolSize - MediumBlockPoolHeaderSize
  jne @MakeEmptyMediumPoolSequentialFeed
  {Point esi to the medium block pool header}
  sub esi, MediumBlockPoolHeaderSize
  {Remove this medium block pool from the linked list}
  mov eax, TMediumBlockPoolHeader[esi].PreviousMediumBlockPoolHeader
  mov edx, TMediumBlockPoolHeader[esi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[eax].NextMediumBlockPoolHeader, edx
  mov TMediumBlockPoolHeader[edx].PreviousMediumBlockPoolHeader, eax
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz  @DontUnlckMedBlcksAftrEntireMedPlFre
{$ENDIF}
  {Unlock medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {it destroys eax, ecx and edx, but we don't need them}
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontUnlckMedBlcksAftrEntireMedPlFre:
{$IFDEF ClearMediumBlockPoolsBeforeReturningToOS}
  mov eax, esi
  mov edx, MediumBlockPoolSize
  xor ecx, ecx
  call System.@FillChar
{$ENDIF}
  {Free the medium block pool}
{$IFDEF FPC}
  mov eax, MEM_RELEASE
  xor edx, edx
  push eax
  push edx
{$ELSE}
  push MEM_RELEASE
  push 0
{$ENDIF}
  push esi
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  {Restore registers}
  pop esi
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MakeEmptyMediumPoolSequentialFeed:
  {Get a pointer to the end-marker block}
  lea ebx, [esi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  {Bin the current sequential feed pool}
  call BinMediumSequentialFeedRemainder
  {Set this medium pool up as the new sequential feed pool:
   Store the sequential feed pool trailer}
  mov dword ptr [ebx - BlockHeaderSize], IsMediumBlockFlag
  {Store the number of bytes available in the sequential feed chunk}
  mov MediumSequentialFeedBytesLeft, MediumBlockPoolSize - MediumBlockPoolHeaderSize
  {Set the last sequentially fed block}
  mov LastSequentiallyFedMediumBlock, ebx
{$IFNDEF AssumeMultiThreaded}
  test ebp, (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlckMedBlksAftrMkEmptMedPlSeqFd
{$ENDIF}
  {Unlock medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {it destroys eax, ecx and edx, but we don't need them}
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@DontUnlckMedBlksAftrMkEmptMedPlSeqFd:
  {Success}
  xor eax, eax
  {Restore registers}
  pop esi
  jmp @Exit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotASmallOrMediumBlock:
  {Is it in fact a large block?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @DontFreeLargeBlock
  pop ebx
{$IFNDEF AssumeMultiThreaded}
  pop ebp
{$ENDIF}
  call FreeLargeBlock
  jmp @Finish
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontFreeLargeBlock:
  {Attempt to free an already free block}
  mov eax, -1
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@Exit:
  pop ebx
{$IFNDEF AssumeMultiThreaded}
  pop ebp
{$ENDIF}
@Finish:
end;

{$ELSE 32Bit}

{---------------64-bit BASM FastFreeMem---------------}
assembler;  // rcx = address
asm
  {$IFDEF DEBUG}
  test   rcx, AlignmentMask
  jz     @@OkAlign
  jmp    BadAlignmentOnFreeMem
@@OkAlign:
  {$ENDIF}

  {Do not put ".noframe" here, for the reasons given at the comment
  in the "BinMediumSequentialFeedRemainder" function at the start of the
  64-bit assembly code}
  {$IFDEF AllowAsmParams}
  .params 3
  .pushnv rbx
  .pushnv rsi
  {$IFNDEF AssumeMultiThreaded}
  .pushnv r12
  {$ENDIF}
  {$ELSE}
  push rbx
  push rsi
  {$IFNDEF AssumeMultiThreaded}
  push r12
  {$ENDIF}
  {$ENDIF}

{$IFNDEF AssumeMultiThreaded}
  xor r12, r12
  {Get the IsMultiThread variable so long}
  lea rsi, [IsMultiThread]
  movzx esi, byte ptr [rsi] {this also clears highest bits of the rsi register}
  test esi, esi
  setnz sil
  shl esi, StateBitMultithreaded
  or r12, rsi
{$ENDIF}

  {Get the block header in rdx}
  mov rdx, [rcx - BlockHeaderSize]
  {Is it a small block in use?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {put test+jnz together to allow macro-op fusion}
  jnz @NotSmallBlockInUse
{$IFDEF ClearSmallAndMediumBlocksInFreeMem}
  mov rsi, rcx
  mov rdx, TSmallBlockPoolHeader[rdx].BlockType
  movzx edx, TSmallBlockType(rdx).BlockSize
  sub edx, BlockHeaderSize
  xor r8, r8
  call System.@FillChar
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
{$ENDIF}
  {Get the small block type in rbx}
  mov rbx, TSmallBlockPoolHeader[rdx].BlockType
  {Do we need to lock the block type?}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMultithreaded)
  jnz @LockSmallBlockType // test+jnz are together to allow macro-op fusion
  jmp @AfterLockOnSmallBlockType
{$ELSE}
  jmp @LockSmallBlockType
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GotLockOnSmallBlockType:
  {$IFDEF SmallBlocksLockedCriticalSection}{$IFDEF DebugAcquireLockByte}
  cmp al, cLockByteAvailable
  jne SmallBlockUnlockError
  {$ENDIF}{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitSmallLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@AfterLockOnSmallBlockType:

  {Current state: rdx = @SmallBlockPoolHeader, rcx = APointer, rbx = @SmallBlockType}
  {Decrement the number of blocks in use}
  sub TSmallBlockPoolHeader[rdx].BlocksInUse, 1
  {Get the old first free block}
  mov rax, TSmallBlockPoolHeader[rdx].FirstFreeBlock
  {Is the pool now empty?}
  jz @PoolIsNowEmpty
  {Was the pool full?}
  test rax, rax
  {Store this as the new first free block}
  mov TSmallBlockPoolHeader[rdx].FirstFreeBlock, rcx
  {Store the previous first free block as the block header}
  lea rax, [rax + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Insert the pool back into the linked list if it was full}
  jz @SmallPoolWasFull
  {All ok}
  xor eax, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@UnlockSmallBlockAndExit:
  {Unlock the block type}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitSmallLocked)
  jz @Done
{$ENDIF}
  {$IFDEF DebugReleaseLockByte}
  cmp TSmallBlockType[rbx].SmallBlockTypeLocked, cLockByteLocked
  jne SmallBlockUnlockError
  {$ENDIF}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov TSmallBlockType[rbx].SmallBlockTypeLocked, cLockByteAvailable
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@SmallPoolWasFull:
  {Insert this as the first partially free pool for the block size}
  mov rcx, TSmallBlockType[rbx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[rdx].PreviousPartiallyFreePool, rbx
  mov TSmallBlockPoolHeader[rdx].NextPartiallyFreePool, rcx
  mov TSmallBlockPoolHeader[rcx].PreviousPartiallyFreePool, rdx
  mov TSmallBlockType[rbx].NextPartiallyFreePool, rdx
  {All ok}
  xor eax, eax
  jmp @UnlockSmallBlockAndExit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PoolIsNowEmpty:
  { mark the current block as released to prevent further call to FreeMem}
  or dword ptr [rcx-BlockHeaderSize], IsFreeBlockFlag
  {Was this pool actually in the linked list of pools with space? If not, it
   can only be the sequential feed pool (it is the only pool that may contain
   only one block, i.e. other blocks have not been split off yet)}
  test rax, rax
  jz @IsSequentialFeedPool
  {Pool is now empty: Remove it from the linked list and free it}
  mov rax, TSmallBlockPoolHeader[rdx].PreviousPartiallyFreePool
  mov rcx, TSmallBlockPoolHeader[rdx].NextPartiallyFreePool
  {Remove this manager}
  mov TSmallBlockPoolHeader[rax].NextPartiallyFreePool, rcx
  mov TSmallBlockPoolHeader[rcx].PreviousPartiallyFreePool, rax
  {Zero out eax}
  xor rax, rax
  {Is this the sequential feed pool? If so, stop sequential feeding}
  cmp TSmallBlockType[rbx].CurrentSequentialFeedPool, rdx
  jne @NotSequentialFeedPool
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@IsSequentialFeedPool:
  mov TSmallBlockType[rbx].MaxSequentialFeedBlockAddress, rax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSequentialFeedPool:
  {Unlock the block type}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitSmallLocked)
  jz @DontRelSmlBlkAftrNotSeqFdPl
{$ENDIF}
  {$IFDEF DebugReleaseLockByte}
  cmp TSmallBlockType[rbx].SmallBlockTypeLocked, cLockByteLocked
  jne SmallBlockUnlockError
  {$ENDIF}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov TSmallBlockType[rbx].SmallBlockTypeLocked, cLockByteAvailable
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontRelSmlBlkAftrNotSeqFdPl:
  {Release this pool}
  mov rcx, rdx
  mov rdx, [rdx - BlockHeaderSize]
  jmp @FreeMediumBlock

{===== START OF SMALL BLOCK LOCKING CODE; 64-BIT FASTFREEMEM =====}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@LockSmallBlockType:

{$IFDEF SmallBlocksLockedCriticalSection}

   mov  eax, cLockByteLocked

   {$IFNDEF DebugAcquireLockByte}
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @PrepareForSpinLoop
   {$ELSE}
   movzx eax, byte ptr TSmallBlockType([rbx]).SmallBlockTypeLocked
   cmp  al, cLockByteLocked
   je   @PrepareForSpinLoop
   cmp  al, cLockByteAvailable
   jne  SmallBlockUnlockError
   mov  eax, cLockByteLocked
   {$ENDIF}

   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   jne  @GotLockOnSmallBlockType

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@PrepareForSpinLoop:
   push rcx
   push rdx

   cmp  byte ptr TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @DidntLockAtFirstAttempt
   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   je   @DidntLockAtFirstAttempt
   jmp  @GotTheLock
@DidntLockAtFirstAttempt:
{$IFDEF EnableWaitPKG}
   test FastMMCpuFeaturesB, FastMMCpuFeatureB_WaitPKG
   jz   @NoWaitPKG

   push r8
   push r9
   push rdx
   lea  r8, TSmallBlockType([rbx]).SmallBlockTypeLocked
   jmp  @FirstLockMonitor

@DidntLockUmonitor:
   push r8
   push r9
   push rdx
   push rbx
   push rcx
   call SwitchToThreadIfSupported
   pop  rcx
   pop  rbx
   pop  rdx
   pop  r9
   pop  r8
   mov  eax, cLockByteLocked
   cmp  [r8], al
   jne  @TryXchgAfterUmonitor

@FirstLockMonitor:
   mov  eax, cLockByteLocked
   db   $F3, $41, $0F, $AE, $F0 // umonitor r8
   cmp  [r8], al
   jne  @TryXchgAfterUmonitor

   mov  eax, CUMWaitTime
   xor  rdx, rdx

   //   bit[0] = 0	C0.2	Slower	Larger	Improves performance of the other SMT thread(s) on the same core.
   //   bit[0] = 1	C0.1	Faster	Smaller	NA
   xor  r9, r9

   db   $F2, $41, $0F, $AE, $F1 // umwait, r9d
@TryXchgAfterUmonitor:
   mov  eax, cLockByteLocked
   lock xchg [r8], al
   cmp  al, cLockByteLocked
   je   @DidntLockUmonitor
   pop  rdx
   pop  r9
   pop  r8
   jmp  @GotTheLock

@NoWaitPKG:
{$ENDIF EnableWaitPKG}
   mov  edx, cPauseSpinWaitLoopCount

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@LockSmallBlockTypeLoop64:
@DidntLock:
@NormalLoadLoop:
   dec  edx
   jz   @SwitchToThread // for static branch prediction, jump forward means "unlikely"
   db   $F3, $90 // pause
   {$IFNDEF DebugAcquireLockByte}
// use the "test, test-and-set" technique, details are in the comment section at the beginning of the file
   cmp  TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   je   @NormalLoadLoop // for static branch prediction, jump backwards means "likely"
   {$ELSE}
   movzx eax, byte ptr TSmallBlockType([rbx]).SmallBlockTypeLocked
   cmp  al, cLockByteLocked
   je   @NormalLoadLoop
   cmp  al, cLockByteAvailable
   jne  SmallBlockUnlockError
   mov  eax, cLockByteLocked
   {$ENDIF}
   lock xchg TSmallBlockType([rbx]).SmallBlockTypeLocked, al
   cmp  al, cLockByteLocked
   je   @DidntLock

@GotTheLock:
   {Congratulations! We've got the lock!}
   pop  rdx
   pop  rcx
   jmp	@GotLockOnSmallBlockType
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@SwitchToThread:
   call  SwitchToThreadIfSupported
   mov  eax, cLockByteLocked
   mov  edx, cPauseSpinWaitLoopCount
   jmp   @LockSmallBlockTypeLoop64

{$ELSE !SmallBlocksLockedCriticalSection}

{ The 64-bit implemenation from the original FastMM4 that employs a loop of Sleep() or SwitchToThread().
By default, it will not be compiled into FastMM4-AVX which uses more efficient approach.}
@LockSmallBlockTypeLoop:
  mov eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
{$IFDEF NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  db $F3, $90 // pause
  {$IFDEF UseSwitchToThread}
  mov rsi, rcx
  call SwitchToThreadIfSupported
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
  {$ENDIF}
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ELSE}
  {Couldn't grab the block type - sleep and try again}
  mov rsi, rcx
  mov ecx, InitialSleepTime
  call Sleep
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
  {Try again}
  mov eax, (cLockbyteLocked shl 8) or cLockByteAvailable
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).SmallBlockTypeLocked, ah  // cmpxchg also uses AL as an implicit operand
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  mov rsi, rcx
  mov ecx, AdditionalSleepTime
  call Sleep
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
  {Try again}
  jmp @LockSmallBlockTypeLoop
{$ENDIF}

{$ENDIF !SmallBlocksLockedCriticalSection}

{===== END OF SMALL BLOCK LOCKING CODE; 64-BIT FASTFREEMEM =====}

  {---------------------Medium blocks------------------------------}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSmallBlockInUse:
  {Not a small block in use: is it a medium or large block?}
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrMediumBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@FreeMediumBlock:
{$IFDEF ClearSmallAndMediumBlocksInFreeMem}
  mov rsi, rcx
  and rdx, DropMediumAndLargeFlagsMask
  sub rdx, BlockHeaderSize
  xor r8, r8
  call System.@FillChar
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
{$ENDIF}
  {Drop the flags}
  and rdx, DropMediumAndLargeFlagsMask
  {Free the medium block pointed to by eax, header in edx}
  {Block size in rbx}
  mov rbx, rdx
  {Pointer in rsi}
  mov rsi, rcx
  {Do we need to lock the medium blocks?}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMultithreaded)
  jz @MediumBlocksLocked // put test+jz together to allow macro-op fusion
{$ENDIF}
{The PurePascal version of LockMediumBlocks call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them, since we keep our data
in nonvolatile (callee-saved) registers like  RBX, RSI, and R12}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call LockMediumBlocks
{$ELSE}
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlocksLocked:
  {Can we combine this block with the next free block?}
  test qword ptr [rsi + rbx - BlockHeaderSize], IsFreeBlockFlag
  {Get the next block size and flags in rcx}
  mov rcx, [rsi + rbx - BlockHeaderSize]
  jnz @NextBlockIsFree
  {Set the "PreviousIsFree" flag in the next block}
  or rcx, PreviousMediumBlockIsFreeFlag
  mov [rsi + rbx - BlockHeaderSize], rcx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NextBlockChecked:
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  test byte ptr [rsi - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PreviousBlockChecked:
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block -> free it.}
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@BinFreeMediumBlock:
  {Store the size of the block as well as the flags}
  lea rax, [rbx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rsi - BlockHeaderSize], rax
  {Store the trailing size marker}
  mov [rsi + rbx - 2 * BlockHeaderSize], rbx
  {Insert this block back into the bins: Size check not required here,
   since medium blocks that are in use are not allowed to be
   shrunk smaller than MinimumMediumBlockSize}
  mov rcx, rsi
  mov rdx, rbx
  {Insert into bin}
  call InsertMediumBlockIntoBin
  {All OK}
  xor eax, eax
  {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @Done
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point}
  call UnlockMediumBlocks
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  xor eax, eax
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NextBlockIsFree:
  {Get the next block address in rax}
  lea rax, [rsi + rbx]
  {Increase the size of this block}
  and rcx, DropMediumAndLargeFlagsMask
  add rbx, rcx
  {Was the block binned?}
  cmp rcx, MinimumMediumBlockSize
  jb @NextBlockChecked
  mov rcx, rax
  call RemoveMediumFreeBlock
  jmp @NextBlockChecked
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PreviousBlockIsFree:
  {Get the size of the free block just before this one}
  mov rcx, [rsi - 2 * BlockHeaderSize]
  {Include the previous block}
  sub rsi, rcx
  {Set the new block size}
  add rbx, rcx
  {Remove the previous block from the linked list}
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov rcx, rsi
  call RemoveMediumFreeBlock
  jmp @PreviousBlockChecked
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@EntireMediumPoolFree:
  {Should we make this the new sequential feed medium block pool? If the
   current sequential feed pool is not entirely free, we make this the new
   sequential feed pool.}
  lea r8, MediumSequentialFeedBytesLeft
  cmp dword ptr [r8], MediumBlockPoolSize - MediumBlockPoolHeaderSize //workaround for QC99023
  jne @MakeEmptyMediumPoolSequentialFeed
  {Point esi to the medium block pool header}
  sub rsi, MediumBlockPoolHeaderSize
  {Remove this medium block pool from the linked list}
  mov rax, TMediumBlockPoolHeader[rsi].PreviousMediumBlockPoolHeader
  mov rdx, TMediumBlockPoolHeader[rsi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[rax].NextMediumBlockPoolHeader, rdx
  mov TMediumBlockPoolHeader[rdx].PreviousMediumBlockPoolHeader, rax
  {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlckMedBlcksAftrEntireMedPlFre
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point}
  call UnlockMediumBlocks
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DontUnlckMedBlcksAftrEntireMedPlFre:
{$IFDEF ClearMediumBlockPoolsBeforeReturningToOS}
  mov rcx, rsi
  mov edx, MediumBlockPoolSize
  xor r8, r8
  call System.@FillChar
{$ENDIF}
  {Free the medium block pool}
  mov rcx, rsi
  xor edx, edx
  mov r8d, MEM_RELEASE
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MakeEmptyMediumPoolSequentialFeed:
  {Get a pointer to the end-marker block}
  lea rbx, [rsi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  {Bin the current sequential feed pool}
  call BinMediumSequentialFeedRemainder
  {Set this medium pool up as the new sequential feed pool:
   Store the sequential feed pool trailer}
  mov qword ptr [rbx - BlockHeaderSize], IsMediumBlockFlag
  {Store the number of bytes available in the sequential feed chunk}
  lea rax, MediumSequentialFeedBytesLeft
  mov dword ptr [rax], MediumBlockPoolSize - MediumBlockPoolHeaderSize //QC99023 workaround
  {Set the last sequentially fed block}
  mov LastSequentiallyFedMediumBlock, rbx
  {Success}
  xor eax, eax
  {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @Done
{$ENDIF}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point}
  call UnlockMediumBlocks
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  xor eax, eax
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotASmallOrMediumBlock:
  {Attempt to free an already free block?}
  mov eax, -1
  {Is it in fact a large block?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Done
  call FreeLargeBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNodot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@Done: {automatically restores registers from stack by implicitly inserting pop instructions (rbx, rsi and r12)}
{$IFNDEF AllowAsmParams}
  {$IFNDEF AssumeMultiThreaded}
   pop r12
  {$ENDIF}
   pop rsi
   pop rbx
{$ENDIF}
end;
{$ENDIF 32Bit}
{$ENDIF FastFreememNeedAssemberCode}


{$IFNDEF FullDebugMode}
{Replacement for SysReallocMem}
function FastReallocMem({$IFDEF fpc}var {$ENDIF}APointer: Pointer; ANewSize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
{$IFNDEF FastReallocMemNeedAssemberCode}

  {Upsizes a large block in-place. The following variables are assumed correct:
    LBlockFlags, LOldAvailableSize, LPNextBlock, LNextBlockSizeAndFlags,
    LNextBlockSize, LNewAvailableSize. Medium blocks must be locked on entry if
    required.}

var
  LBlockFlags,
  LNextBlockSizeAndFlags,
  LMinimumUpsize,
  LOldAvailableSize,
  LNewAllocSize,
  LNewBlockSize,
  LNewAvailableSize: NativeUInt;
  LPNextBlock: Pointer;
  LPNextBlockHeader: Pointer;
  LSecondSplitSize: NativeUInt;

  procedure MediumBlockInPlaceUpsize;
  var
    LSum: NativeUInt;
  begin
    {Remove the next block}
    if LNextBlockSizeAndFlags >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(LPNextBlock);
    {Add 25% for medium block in-place upsizes}
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if NativeUInt(ANewSize) < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize
    else
      LNewAllocSize := NativeUInt(ANewSize);
    {Round up to the nearest block size granularity}
    LNewBlockSize := ((LNewAllocSize + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and MediumBlockGranularityMask) + MediumBlockSizeOffset;
    {Does it fit?}
    LSum := LNewAvailableSize + BlockHeaderSize;
    if LSum <= LNewBlockSize then
    begin
      LSecondSplitSize := NativeUInt(-1);
      {The block size is the full available size plus header}
      LNewBlockSize := LNewAvailableSize + BlockHeaderSize;
      {Grab the whole block: Mark it as used in the block following it}
      LPNextBlockHeader := Pointer(PByte(APointer) + LNewAvailableSize);
      PNativeUInt(LPNextBlockHeader)^ :=
        PNativeUInt(LPNextBlockHeader)^ and (not PreviousMediumBlockIsFreeFlag);
    end
    else
    begin
      LSecondSplitSize := LSum - LNewBlockSize;
      {Split the block in two}
      LPNextBlock := PMediumFreeBlock(PByte(APointer) + LNewBlockSize);
      {Set the size of the second split}
      PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
      {Store the size of the second split before the header of the next block}
      PNativeUInt(PByte(LPNextBlock) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
      {Put the remainder in a bin if it is big enough}
      if LSecondSplitSize >= MinimumMediumBlockSize then
        InsertMediumBlockIntoBin(LPNextBlock, LSecondSplitSize);
    end;
    {Set the size and flags for this block}
    PNativeUInt(PByte(APointer) - BlockHeaderSize)^ := LNewBlockSize or LBlockFlags;
  end;

  {In-place downsize of a medium block. On entry Size must be less than half of
   LOldAvailableSize.}
  procedure MediumBlockInPlaceDownsize;
{$IFDEF LogLockContention}
  var
    LDidSleep: Boolean;
{$ENDIF}
  var
    LWasMultiThreadMediumBlocks: Boolean;
  begin
    LWasMultiThreadMediumBlocks := False;

    {Round up to the next medium block size}
    LNewBlockSize := ((NativeUInt(ANewSize) + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and MediumBlockGranularityMask) + MediumBlockSizeOffset;
    {Get the size of the second split}
    LSecondSplitSize := (LOldAvailableSize + BlockHeaderSize) - LNewBlockSize;
    {Lock the medium blocks}

{$IFNDEF AssumeMultiThreaded}
    if IsMultiThread then
{$ENDIF}
    begin
      LWasMultiThreadMediumBlocks := True;
    {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
  {$IFDEF LogLockContention}
      if LDidSleep then
        LCollector := @MediumBlockCollector;
  {$ENDIF}
    end;
    {Set the new size}
    PNativeUInt(PByte(APointer) - BlockHeaderSize)^ :=
      (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask)
      or LNewBlockSize;
    {Is the next block in use?}
    LPNextBlock := PNativeUInt(PByte(APointer) + LOldAvailableSize + BlockHeaderSize);
    LNextBlockSizeAndFlags := PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^;
    if (LNextBlockSizeAndFlags and IsFreeBlockFlag) = 0 then
    begin
      {The next block is in use: flag its previous block as free}
      PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^ :=
        LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
    end
    else
    begin
      {The next block is free: combine it}
      LNextBlockSizeAndFlags := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
      Inc(LSecondSplitSize, LNextBlockSizeAndFlags);
      if LNextBlockSizeAndFlags >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LPNextBlock);
    end;
    {Set the split}
    LPNextBlock := PNativeUInt(PByte(APointer) + LNewBlockSize);
    {Store the free part's header}
    PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
    {Store the trailing size field}
    PNativeUInt(PByte(LPNextBlock) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
    {Bin this free block}
    if LSecondSplitSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(LPNextBlock, LSecondSplitSize);
    if LWasMultiThreadMediumBlocks then
    begin
       LWasMultiThreadMediumBlocks := False;
      {Unlock the medium blocks}
       UnlockMediumBlocks;
    end;
  end;

var
  LBlockHeader,
  LNextBlockSize: NativeUInt;
  LPSmallBlockType: PSmallBlockType;
{$IFDEF LogLockContention}
  LCollector: PStaticCollector;
{$ENDIF}

var
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
  LStackTrace: TStackTrace;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LWasMultithread: Boolean;
{$ENDIF}
  LWasMediumBlockLocked: Boolean;
begin
{$IFNDEF AssumeMultiThreaded}
  LWasMultithread := False;
{$ENDIF}
  LWasMediumBlockLocked := False;
{$IFDEF fpc}
  if APointer = nil then
  begin
    if ANewSize <> 0 then
      APointer := FastGetMem(ANewSize);
    Result := APointer;
    Exit;
  end
  else if ANewSize = 0 then
  begin
    FastFreeMem(APointer);
    APointer := nil;
    Result := APointer;
    Exit;
  end;
{$ENDIF}
{$IFDEF LogLockContention}
  LCollector := nil;
  LPSmallBlockType := nil; // to remove "uninitialized" warning in the "finally" block
  try
{$ENDIF}
  {Get the block header: Is it actually a small block?}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Is it a small block that is in use?}
  if ((LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag))) = 0 then
  begin
    {-----------------------------------Small block-------------------------------------}
    {The block header is a pointer to the block pool: Get the block type}
    LPSmallBlockType := PSmallBlockPoolHeader(LBlockHeader)^.BlockType;
    {Get the available size inside blocks of this type.}
    LOldAvailableSize := LPSmallBlockType^.BlockSize - BlockHeaderSize;
    {Is it an upsize or a downsize?}
    if LOldAvailableSize >= NativeUInt(ANewSize) then
    begin
      {It's a downsize. Do we need to allocate a smaller block? Only if the new
       block size is less than a quarter of the available size less
       SmallBlockDownsizeCheckAdder bytes}
      if (NativeUInt(ANewSize) * 4 + SmallBlockDownsizeCheckAdder) >= LOldAvailableSize then
      begin
        {In-place downsize - return the pointer}
        Result := APointer;
        Exit;
      end
      else
      begin
        {Allocate a smaller block}
        Result := FastGetMem(ANewSize);
        {Allocated OK?}
        if Result <> nil then
        begin
          {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
  {$IFDEF Align32Bytes}
          MoveX32LPUniversal(APointer^, Result^, ANewSize);
  {$ELSE}
  {$IFDEF Align16Bytes}
          MoveX16LP(APointer^, Result^, ANewSize);
  {$ELSE}
          MoveX8LP(APointer^, Result^, ANewSize);
  {$ENDIF}
  {$ENDIF}
{$ELSE}
          System.Move(APointer^, Result^, ANewSize);
{$ENDIF}
          {Free the old pointer}
          FastFreeMem(APointer);
        end;
      end;
    end
    else
    begin
      {This pointer is being reallocated to a larger block and therefore it is
       logical to assume that it may be enlarged again. Since reallocations are
       expensive, there is a minimum upsize percentage to avoid unnecessary
       future move operations.}
      {Must grow with at least 100% + x bytes}
      LNewAllocSize := LOldAvailableSize * 2 + SmallBlockUpsizeAdder;
      {Still not large enough?}
      if LNewAllocSize < NativeUInt(ANewSize) then
        LNewAllocSize := NativeUInt(ANewSize);
      {Allocate the new block}
      Result := FastGetMem(LNewAllocSize);
      {Allocated OK?}
      if Result <> nil then
      begin
        {Do we need to store the requested size? Only large blocks store the
         requested size.}
        if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
          PLargeBlockHeader(PByte(Result) - LargeBlockHeaderSize)^.UserAllocatedSize := ANewSize;
        {Move the data across}
{$IFDEF UseCustomFixedSizeMoveRoutines}
        LPSmallBlockType^.UpsizeMoveProcedure(APointer^, Result^, LOldAvailableSize);
{$ELSE}
        System.Move(APointer^, Result^, LOldAvailableSize);
{$ENDIF}
        {Free the old pointer}
        FastFreeMem(APointer);
      end;
    end;
  end
  else
  begin
    {Is this a medium block or a large block?}
    if ((LBlockHeader and (IsFreeBlockFlag or IsLargeBlockFlag))) = 0 then
    begin
      {-------------------------------Medium block--------------------------------------}
      {What is the available size in the block being reallocated?}
      LOldAvailableSize := (LBlockHeader and DropMediumAndLargeFlagsMask);
      {Get a pointer to the next block}
      LPNextBlock := PNativeUInt(PByte(APointer) + LOldAvailableSize);
      {Subtract the block header size from the old available size}
      Dec(LOldAvailableSize, BlockHeaderSize);
      {Is it an upsize or a downsize?}
      if NativeUInt(ANewSize) > LOldAvailableSize then
      begin
        {Can we do an in-place upsize?}
        LNextBlockSizeAndFlags := PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^;
        {Is the next block free?}
        if (LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
        begin
          LNextBlockSize := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
          {The available size including the next block}
          LNewAvailableSize := LOldAvailableSize + LNextBlockSize;
          {Can the block fit?}
          if NativeUInt(ANewSize) <= LNewAvailableSize then
          begin
            {The next block is free and there is enough space to grow this
             block in place.}
{$IFNDEF AssumeMultiThreaded}
            if IsMultiThread then
            begin
              LWasMultithread := True;
{$ENDIF}
              {Multi-threaded application - lock medium blocks and re-read the
               information on the blocks.}
              LWasMediumBlockLocked := True;

               {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
{$IFDEF LogLockContention}
              if LDidSleep then
                LCollector := @MediumBlockCollector;
{$ENDIF}
              {Re-read the info for this block}
              LBlockFlags := PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask;
              {Re-read the info for the next block}
              LNextBlockSizeAndFlags := PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^;
              {Recalculate the next block size}
              LNextBlockSize := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
              {The available size including the next block}
              LNewAvailableSize := LOldAvailableSize + LNextBlockSize;
              {Is the next block still free and the size still sufficient?}
              if ((LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0)
                and (NativeUInt(ANewSize) <= LNewAvailableSize) then
              begin
                {Upsize the block in-place}
                MediumBlockInPlaceUpsize;
                if LWasMediumBlockLocked then
                begin
                  LWasMediumBlockLocked := False;
                  {Unlock the medium blocks}
                  UnlockMediumBlocks;
                end;
                {Return the result}
                Result := APointer;
                {Done}
                Exit;
              end;
              if LWasMediumBlockLocked then
              begin
                LWasMediumBlockLocked := False;
              {Couldn't use the block: Unlock the medium blocks}
                UnlockMediumBlocks;
              end;
{$IFNDEF AssumeMultiThreaded}
            end
            else
            begin
              {Extract the block flags}
              LBlockFlags := ExtractMediumAndLargeFlagsMask and LBlockHeader;
              {Upsize the block in-place}
              MediumBlockInPlaceUpsize;
              {Return the result}
              Result := APointer;
              {Done}
              Exit;
            end;
{$ENDIF}
          end;
        end;
        {Couldn't upsize in place. Grab a new block and move the data across:
         If we have to reallocate and move medium blocks, we grow by at
         least 25%}
        LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
        if NativeUInt(ANewSize) < LMinimumUpsize then
          LNewAllocSize := LMinimumUpsize
        else
          LNewAllocSize := NativeUInt(ANewSize);
        {Allocate the new block}
        Result := FastGetMem(LNewAllocSize);
        if Result <> nil then
        begin
          {If it's a large block - store the actual user requested size}
          if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
            PLargeBlockHeader(PByte(Result) - LargeBlockHeaderSize)^.UserAllocatedSize := ANewSize;
          {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
   {$IFDEF Align32Bytes}
          MoveX32LPUniversal(APointer^, Result^, LOldAvailableSize);
   {$ELSE}
     {$IFDEF Align16Bytes}
          MoveX16LP(APointer^, Result^, LOldAvailableSize);
     {$ELSE}
          MoveX8LP(APointer^, Result^, LOldAvailableSize);
     {$ENDIF}
   {$ENDIF}
{$ELSE}
          System.Move(APointer^, Result^, LOldAvailableSize);
{$ENDIF}
          {Free the old block}
          FastFreeMem(APointer);
        end;
      end
      else
      begin
        {Must be less than half the current size or we don't bother resizing.}
        if (NativeUInt(ANewSize) shl 1) >= LOldAvailableSize then
        begin
          Result := APointer;
        end
        else
        begin
          {In-place downsize? Balance the cost of moving the data vs. the cost
           of fragmenting the memory pool. Medium blocks in use may never be
           smaller than MinimumMediumBlockSize.}
          if NativeUInt(ANewSize) >= (MinimumMediumBlockSize - BlockHeaderSize) then
          begin
            MediumBlockInPlaceDownsize;
            Result := APointer;
          end
          else
          begin
            {The requested size is less than the minimum medium block size. If
             the requested size is less than the threshold value (currently a
             quarter of the minimum medium block size), move the data to a small
             block, otherwise shrink the medium block to the minimum allowable
             medium block size.}
            if NativeUInt(ANewSize) >= MediumInPlaceDownsizeLimit then
            begin
              {The request is for a size smaller than the minimum medium block
               size, but not small enough to justify moving data: Reduce the
               block size to the minimum medium block size}
              ANewSize := MinimumMediumBlockSize - BlockHeaderSize;
              {Is it already at the minimum medium block size?}
              if LOldAvailableSize > NativeUInt(ANewSize) then
                MediumBlockInPlaceDownsize;
              Result := APointer;
            end
            else
            begin
              {Allocate the new block}
              Result := FastGetMem(ANewSize);
              if Result <> nil then
              begin
                {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
  {$IFDEF Align32Bytes}
                MoveX32LPUniversal(APointer^, Result^, ANewSize);
  {$ELSE}
  {$IFDEF Align16Bytes}
                MoveX16LP(APointer^, Result^, ANewSize);
  {$ELSE}
                MoveX8LP(APointer^, Result^, ANewSize);
  {$ENDIF}
  {$ENDIF}
{$ELSE}
                System.Move(APointer^, Result^, ANewSize);
{$ENDIF}
                {Free the old block}
                FastFreeMem(APointer);
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      {Is this a valid large block?}
      if (LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag)) = 0 then
      begin
        {-----------------------Large block------------------------------}
        Result := ReallocateLargeBlock(APointer, ANewSize);
      end
      else
      begin
        {-----------------------Invalid block------------------------------}
        {Bad pointer: probably an attempt to reallocate a free memory block.}
        Result := nil;
      end;
    end;
  end;
{$IFDEF fpc}
  APointer := Result;
{$ENDIF}
{$IFDEF LogLockContention}
  finally
    if Assigned(LCollector) then
    begin
      GetStackTrace(@(LStackTrace[0]), StackTraceDepth, 1);
      LPSmallBlockType.BlockCollector.Add(@(LStackTrace[0]), StackTraceDepth);
    end;
  end;
{$ENDIF}
end;
{$ELSE FastReallocMemNeedAssemberCode}
{$IFDEF 32Bit}
assembler;
{$IFNDEF AssumeMultiThreaded}
const
  cLocalVarStackOfsMediumBlock = 4 {size of a 32-bit register} * 4 {4 saved registers to skip for a medium block};
{$ENDIF}
asm
{$IFDEF fpc}
  push esi
  mov esi, eax
  mov eax, [esi]
  test eax, eax
  jne @PointerNotNil
  test edx, edx
  je @SizeIsZero
  mov eax, edx
  call FastGetMem
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMem1
  jmp    BadAlignmentOnGetMem
@@OkAlignmentOnGetMem1:
  {$ENDIF}
  mov [esi], eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 2{$ENDIF}
@SizeIsZero:
  pop esi
  jmp @Final
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PointerNotNil:
  test edx, edx
  jne @GoRealloc
  call FastFreeMem
  mov dword ptr [esi], 0
  pop esi
  jmp @Final
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@GoRealloc:
{$ENDIF}
  {On entry: eax = APointer; edx = ANewSize}
  {Get the block header: Is it actually a small block?}

{$IFDEF AssumeMultiThreaded}
{$IFDEF FPC}
  xor ecx, ecx
  push ecx // empty local variable into the stack
{$ELSE}
  push 0 // empty local variable into the stack
{$ENDIF}
{$ELSE}
  {Branchless operations to avoid misprediction}
  cmp byte ptr [IsMultiThread], 0
  setnz cl
  movzx ecx, cl
  shl ecx, StateBitMultithreaded
  push ecx // put local variable into the stack
{$ENDIF}


  mov ecx, [eax - BlockHeaderSize]
  {Save ebx}
  push ebx
  {Save esi}
  push esi
  {Save the original pointer in esi}
  mov esi, eax
  {Is it a small block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotASmallBlock {test+jnz provides macro-op fusion}
  {-----------------------------------Small block-------------------------------------}
  {Get the block type in ebx}
  mov ebx, TSmallBlockPoolHeader[ecx].BlockType
  {Get the available size inside blocks of this type.}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  sub ecx, 4
  {Is it an upsize or a downsize?}
  cmp ecx, edx
  jb @SmallUpsize
  {It's a downsize. Do we need to allocate a smaller block? Only if the new
   size is less than a quarter of the available size less
   SmallBlockDownsizeCheckAdder bytes}
  lea ebx, [edx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jnb @Exit2Reg
//@NotSmallInPlaceDownsize:
  {Save the requested size}
  mov ebx, edx
  {Allocate a smaller block}
  mov eax, edx
  call FastGetMem
  {Allocated OK?}
  test eax, eax
  jz @Exit2Reg
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMem2
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem2:
  {$ENDIF}
  {Move data across: count in ecx}
  mov ecx, ebx
  {Destination in edx}
  mov edx, eax
  {Save the result in ebx}
  mov ebx, eax
  {Original pointer in eax}
  mov eax, esi
  {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
  {$IFDEF Align32Bytes}
  call MoveX32LPUniversal
  {$ELSE}
  {$IFDEF Align16Bytes}
  call MoveX16LP
  {$ELSE}
  call MoveX8LP
  {$ENDIF}
  {$ENDIF}
{$ELSE}
  call System.Move
{$ENDIF}
  {Free the original pointer}
  mov eax, esi
  call FastFreeMem
  {Return the pointer}
  mov eax, ebx
  jmp @Exit2Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@SmallUpsize:
  {State: esi = APointer, edx = ANewSize, ecx = Current Block Size, ebx = Current Block Type}
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes}
  lea ecx, [ecx + ecx + SmallBlockUpsizeAdder]
  {save edi}
  push edi
  {Save the requested size in edi}
  mov edi, edx
  {New allocated size is the maximum of the requested size and the minimum
   upsize}
  xor eax, eax
  sub ecx, edx
  adc eax, -1
  and eax, ecx
  add eax, edx
  {Allocate the new block}
  call FastGetMem
  {Allocated OK?}
  test eax, eax
  jz @Exit3Reg
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMem3
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem3:
  {$ENDIF}
  {Do we need to store the requested size? Only large blocks store the
   requested size.}
  cmp edi, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  {Store the user requested size}
  mov [eax - BlockHeaderSize*2], edi
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSmallUpsizeToLargeBlock:
  {Get the size to move across}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  sub ecx, BlockHeaderSize
  {Move to the new block}
  mov edx, eax
  {Save the result in edi}
  mov edi, eax
  {Move from the old block}
  mov eax, esi
  {Move the data across}
{$IFDEF UseCustomFixedSizeMoveRoutines}
  call TSmallBlockType[ebx].UpsizeMoveProcedure
{$ELSE}
  call System.Move
{$ENDIF}
  {Free the old pointer}
  mov eax, esi
  call FastFreeMem
  {Done}
  mov eax, edi
  jmp @Exit3Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotASmallBlock:
  {Is this a medium block or a large block?}
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  {-------------------------------Medium block--------------------------------------}
  {Status: ecx = Current Block Size + Flags, eax/esi = APointer,
   edx = Requested Size}
  mov ebx, ecx
  {Drop the flags from the header}
  and ecx, DropMediumAndLargeFlagsMask
  {Save edi}
  push edi
  {Get a pointer to the next block in edi}
  lea edi, [eax + ecx]
  {Subtract the block header size from the old available size}
  sub ecx, BlockHeaderSize
  {Get the complete flags in ebx}
  and ebx, ExtractMediumAndLargeFlagsMask

  {Save ebp}
  push ebp

  {Is it an upsize or a downsize?}
  cmp edx, ecx
  ja @MediumBlockUpsize {cmp+ja provides macro-op fusion}
  {Status: ecx = Current Block Size - 4, bl = Current Block Flags,
   edi = @Next Block, eax/esi = APointer, edx = Requested Size}
  {Must be less than half the current size or we don't bother resizing.}
  lea ebp, [edx + edx]
  cmp ebp, ecx
  jnb @Exit4Reg
  {In-place downsize? Balance the cost of moving the data vs. the cost of
   fragmenting the memory pool. Medium blocks in use may never be smaller
   than MinimumMediumBlockSize.}
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  {The requested size is less than the minimum medium block size. If the
  requested size is less than the threshold value (currently a quarter of the
  minimum medium block size), move the data to a small block, otherwise shrink
  the medium block to the minimum allowable medium block size.}
  cmp edx, MediumInPlaceDownsizeLimit
  jb @MediumDownsizeRealloc
  {The request is for a size smaller than the minimum medium block size, but
   not small enough to justify moving data: Reduce the block size to the
   minimum medium block size}
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
  {Is it already at the minimum medium block size?}
  cmp ecx, edx
  jna @Exit4Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockInPlaceDownsize:
  {Round up to the next medium block size}
  lea ebp, [edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and ebp, MediumBlockGranularityMask
  add ebp, MediumBlockSizeOffset
  {Get the size of the second split}
  add ecx, BlockHeaderSize
  sub ecx, ebp
  {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMultithreaded)
  jz @DoMediumInPlaceDownsize
{$ENDIF}
//@DoMediumLockForDownsize:

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  {When ussing UseOriginalFastMM4_LockMediumBlocksAsm, it preserves all registers
  (except eax), including ecx}
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push ecx; push edx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop edx; pop ecx {$ENDIF}
{$ELSE}
  // this function preserves all volatile registers except eax
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}

  {Reread the flags - they may have changed before medium blocks could be
   locked.}
  mov ebx, ExtractMediumAndLargeFlagsMask
  and ebx, [esi - BlockHeaderSize]
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DoMediumInPlaceDownsize:
  {Set the new size}
  or ebx, ebp
  mov [esi - BlockHeaderSize], ebx
  {Get the second split size in ebx}
  mov ebx, ecx
  {Is the next block in use?}
  mov edx, [edi - BlockHeaderSize]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  {The next block is in use: flag its previous block as free}
  or edx, PreviousMediumBlockIsFreeFlag
  mov [edi - BlockHeaderSize], edx
  jmp @MediumDownsizeDoSplit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumDownsizeNextBlockFree:
  {The next block is free: combine it}
  mov eax, edi
  and edx, DropMediumAndLargeFlagsMask
  add ebx, edx
  add edi, edx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumDownsizeDoSplit:
  {Store the trailing size field}
  mov [edi - BlockHeaderSize*2], ebx
  {Store the free part's header}
  lea eax, [ebx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [esi + ebp - BlockHeaderSize], eax
  {Bin this free block}
  cmp ebx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea eax, [esi + ebp]
  mov edx, ebx
  call InsertMediumBlockIntoBin
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockDownsizeDone:
  {Result = old pointer}
  mov eax, esi
{$IFNDEF AssumeMultiThreaded}
  test byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMediumLocked)
  jz @Exit4Reg
{$ENDIF}
  {Unlock the medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {this call destroys eax, ecx, edx, but we don't need them, since we are about to exit}
  {Result = old pointer}
  mov eax, esi
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  jmp @Exit4Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumDownsizeRealloc:
  {Save the requested size}
  mov edi, edx
  mov eax, edx
  {Allocate the new block}
  call FastGetMem
  test eax, eax
  jz @Exit4Reg
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMem4
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem4:
  {$ENDIF}

  {Save the result}
  mov ebp, eax
  mov edx, eax
  mov eax, esi
  mov ecx, edi
  {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
  {$IFDEF Align32Bytes}
  call MoveX32LPUniversal
  {$ELSE}
  {$IFDEF Align16Bytes}
  call MoveX16LP
  {$ELSE}
  call MoveX8LP
  {$ENDIF}
  {$ENDIF}
{$ELSE}
  call System.Move
{$ENDIF}
  mov eax, esi
  call FastFreeMem
  {Return the result}
  mov eax, ebp
  jmp @Exit4Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockUpsize:
  {Status: ecx = Current Block Size - 4, bl = Current Block Flags,
   edi = @Next Block, eax/esi = APointer, edx = Requested Size}
  {Can we do an in-place upsize?}
  mov eax, [edi - BlockHeaderSize]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  {Get the total available size including the next block}
  and eax, DropMediumAndLargeFlagsMask
  {ebp = total available size including the next block (excluding the header)}
  lea ebp, [eax + ecx]
  {Can the block fit?}
  cmp edx, ebp
  ja @CannotUpsizeMediumBlockInPlace
  {The next block is free and there is enough space to grow this
   block in place.}
{$IFNDEF AssumeMultiThreaded}
  test byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMultithreaded)
  je @DoMediumInPlaceUpsize
{$ENDIF}
//@DoMediumLockForUpsize:
  {Lock the medium blocks (ecx and edx *must* be preserved}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push ecx; push edx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop edx; pop ecx {$ENDIF}
{$ELSE}
  // this function saves all the volatile registers, so we do not have to save them
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {Re-read the info for this block (since it may have changed before the medium
   blocks could be locked)}
  mov ebx, ExtractMediumAndLargeFlagsMask
  and ebx, [esi - BlockHeaderSize]
  {Re-read the info for the next block}
  mov eax, [edi - BlockHeaderSize]
  {Next block still free?}
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  {Recalculate the next block size}
  and eax, DropMediumAndLargeFlagsMask
  {The available size including the next block}
  lea ebp, [eax + ecx]
  {Can the block still fit?}
  cmp edx, ebp
  ja @NextMediumBlockChanged
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DoMediumInPlaceUpsize:
  {Is the next block binnable?}
  cmp eax, MinimumMediumBlockSize
  {Remove the next block}
  jb @MediumInPlaceNoNextRemove
  mov eax, edi
  push ecx
  push edx
  call RemoveMediumFreeBlock
  pop edx
  pop ecx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@MediumInPlaceNoNextRemove:
  {Medium blocks grow a minimum of 25% in in-place upsizes}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  {Round up to the nearest block size granularity}
  lea eax, [eax + edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, MediumBlockGranularityMask
  add eax, MediumBlockSizeOffset
  {Calculate the size of the second split}
  lea edx, [ebp + BlockHeaderSize]
  sub edx, eax
  {Does it fit?}
  ja @MediumInPlaceUpsizeSplit
  {Grab the whole block: Mark it as used in the block following it}
  and dword ptr [esi + ebp], not PreviousMediumBlockIsFreeFlag
  {The block size is the full available size plus header}
  add ebp, BlockHeaderSize
  {Upsize done}
  jmp @MediumUpsizeInPlaceDone
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumInPlaceUpsizeSplit:
  {Store the size of the second split as the second last dword}
  mov [esi + ebp - BlockHeaderSize], edx
  {Set the second split header}
  lea edi, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [esi + eax - BlockHeaderSize], edi
  mov ebp, eax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  add eax, esi
  call InsertMediumBlockIntoBin
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumUpsizeInPlaceDone:
  {Set the size and flags for this block}
  or ebp, ebx
  mov [esi - BlockHeaderSize], ebp

  {Result = old pointer}
  mov eax, esi

{$IFNDEF AssumeMultiThreaded}
  test byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMediumLocked)
  jz @Exit4Reg
{$ENDIF}
  {Unlock the medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  call UnlockMediumBlocks {this call destroys eax, ecx, edx, but we don't need them now since we are about to exit}
  {Result = old pointer}
  mov eax, esi
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}
  jmp @Exit4Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NextMediumBlockChanged:
{$IFNDEF AssumeMultiThreaded}
  test byte ptr ss:[esp+cLocalVarStackOfsMediumBlock], (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlMedBlksAftrNxtMedBlkChg
{$ENDIF}
  {The next medium block changed while the medium blocks were being locked}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  push ecx
  push edx
  call UnlockMediumBlocks  {this function destroys eax, ecx and edx, so we save ecx and edx}
  pop edx
  pop ecx
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}

@DontUnlMedBlksAftrNxtMedBlkChg:

@CannotUpsizeMediumBlockInPlace:
  {Couldn't upsize in place. Grab a new block and move the data across:
   If we have to reallocate and move medium blocks, we grow by at
   least 25%}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  add eax, edx
  {Save the size to allocate}
  mov ebp, eax
  {Save the size to move across}
  mov edi, ecx
  {Get the block}
  push edx
  call FastGetMem
  pop edx
  {Success?}
  test eax, eax
  jz @Exit4Reg
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMem5
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem5:
  {$ENDIF}
  {If it's a Large block - store the actual user requested size}
  cmp ebp, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [eax - BlockHeaderSize*2], edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@MediumUpsizeNotLarge:
  {Save the result}
  mov ebp, eax
  {Move the data across}
  mov edx, eax
  mov eax, esi
  mov ecx, edi
{$IFDEF UseCustomVariableSizeMoveRoutines}
{$IFDEF Align32Bytes}
  call MoveX32LPUniversal
{$ELSE}
  {$IFDEF Align16Bytes}
  call MoveX16LP
  {$ELSE}
  call MoveX8LP
  {$ENDIF}
{$ENDIF}
{$ELSE}
  call System.Move
{$ENDIF}
  {Free the old block}
  mov eax, esi
  call FastFreeMem
  {Restore the result}
  mov eax, ebp
  jmp @Exit4Reg
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PossibleLargeBlock:
  {-----------------------Large block------------------------------}
  {Restore registers}
  pop esi
  pop ebx
  add esp, 4 {remove local variable, 4=size of 32-bit register}
  {Is this a valid large block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
{$IFNDEF fpc}
  jz ReallocateLargeBlock
{$ELSE}
  jnz @FpcError
  call ReallocateLargeBlock
  jmp @FpcDone
  {-----------------------Invalid block------------------------------}
@FpcError:
{$ENDIF}
  xor eax, eax
{$IFDEF fpc}
@FpcDone:
  mov [esi], eax
  pop esi
  jmp @FpcExitStrackRestored
{$ENDIF}

{Don't need alignment here since all instructions are just one-byte}
@Exit4Reg: {return, restoring 4 registers from the stack and one local variable}
  pop ebp
@Exit3Reg: {return, restoring 3 registers from the stack and one local variable}
  pop edi
@Exit2Reg: {return, restoring 2 registers from the stack and one local variable}
  pop esi
  pop ebx
  add esp, 4 {remove local variable, 4=size of 32-bit register}

{$IFDEF fpc}
  mov [esi], eax
  pop esi
{$ENDIF}

{$IFDEF fpc}
@FpcExitStrackRestored:
{$ENDIF}
@Final:
end;

{$ELSE}

{-----------------64-bit BASM FastReallocMem-----------------}
assembler;
asm
  {Do not put ".noframe" here, for the reasons given at the comment
  in the "BinMediumSequentialFeedRemainder" function at the start of the
  64-bit assembler code}
  {$IFDEF AllowAsmParams}
  .params 3
  .pushnv rbx
  .pushnv rsi
  .pushnv rdi
  {$IFNDEF AssumeMultiThreaded}
  .pushnv r12
  {$ENDIF}
  .pushnv r14
  .pushnv r15
  {$ELSE}
  push rbx
  push rsi
  push rdi
  {$IFNDEF AssumeMultiThreaded}
  push r12
  {$ENDIF}
  push r14
  push r15
  {$ENDIF}

{$IFNDEF AssumeMultiThreaded}
  xor r12, r12
  {Get the IsMultiThread variable so long}
  lea rsi, [IsMultiThread]
  movzx esi, byte ptr [rsi] {this also clears highest bits of the rsi register}
  test esi, esi
  setnz sil
  shl esi, StateBitMultithreaded
  or r12, rsi
{$ENDIF}


  {On entry: rcx = APointer; rdx = ANewSize}
  {Save the original pointer in rsi}
  mov rsi, rcx
  {Get the block header}
  mov rcx, [rcx - BlockHeaderSize]
  {Is it a small block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotASmallBlock
  {-----------------------------------Small block-------------------------------------}
  {Get the block type in rbx}
  mov rbx, TSmallBlockPoolHeader[rcx].BlockType
  {Get the available size inside blocks of this type.}
  movzx ecx, TSmallBlockType[rbx].BlockSize
  sub ecx, BlockHeaderSize
  {Is it an upsize or a downsize?}
  cmp rcx, rdx
  jb @SmallUpsize
  {It's a downsize. Do we need to allocate a smaller block? Only if the new
   size is less than a quarter of the available size less
   SmallBlockDownsizeCheckAdder bytes}
  lea ebx, [edx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jb @NotSmallInPlaceDownsize
  {In-place downsize - return the original pointer}
  mov rax, rsi
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSmallInPlaceDownsize:
  {Save the requested size}
  mov rbx, rdx
  {Allocate a smaller block}
  mov rcx, rdx
  call FastGetMem
  {Allocated OK?}
  test rax, rax
  jz @Done
  {$IFDEF DEBUG}
  test rax, AlignmentMask
  jz @@OkAlignmentOnGetMem6
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem6:
  {$ENDIF}
  {Move data across: count in r8}
  mov r8, rbx
  {Destination in edx}
  mov rdx, rax
  {Save the result in ebx}
  mov rbx, rax
  {Original pointer in ecx}
  mov rcx, rsi
  {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
  {$IFDEF Align32Bytes}
  call MoveX32LPUniversal
  {$ELSE}
  {$IFDEF Align16Bytes}
  call MoveX16LP
  {$ELSE}
  call MoveX8LP
  {$ENDIF}
  {$ENDIF}
{$ELSE}
  call System.Move
{$ENDIF}
  {Free the original pointer}
  mov rcx, rsi
  call FastFreeMem
  {Return the pointer}
  mov rax, rbx
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@SmallUpsize:
  {State: rsi = APointer, rdx = ANewSize, rcx = Current Block Size, rbx = Current Block Type}
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes}
  lea ecx, [ecx + ecx + SmallBlockUpsizeAdder]
  {Save the requested size in rdi}
  mov rdi, rdx
  {New allocated size is the maximum of the requested size and the minimum
   upsize}
  xor rax, rax
  sub rcx, rdx
  adc rax, -1
  and rcx, rax
  add rcx, rdx
  {Allocate the new block}
  call FastGetMem
  {Allocated OK?}
  test rax, rax
  jz @Done
  {$IFDEF DEBUG}
  test rax, AlignmentMask
  jz @@OkAlignmentOnGetMem7
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem7:
  {$ENDIF}
  {Do we need to store the requested size? Only large blocks store the
   requested size.}
  cmp rdi, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  {Store the user requested size}
  mov [rax - 2 * BlockHeaderSize], rdi
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotSmallUpsizeToLargeBlock:
  {Get the size to move across}
  movzx r8d, TSmallBlockType[rbx].BlockSize
  sub r8d, BlockHeaderSize
  {Move to the new block}
  mov rdx, rax
  {Save the result in edi}
  mov rdi, rax
  {Move from the old block}
  mov rcx, rsi
  {Move the data across}
{$IFDEF UseCustomFixedSizeMoveRoutines}
  call TSmallBlockType[rbx].UpsizeMoveProcedure
{$ELSE}
  call System.Move
{$ENDIF}
  {Free the old pointer}
  mov rcx, rsi
  call FastFreeMem
  {Done}
  mov rax, rdi
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NotASmallBlock:
  {Is this a medium block or a large block?}
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  {-------------------------------Medium block--------------------------------------}
  {Status: rcx = Current Block Size + Flags, rsi = APointer,
   rdx = Requested Size}
  mov rbx, rcx
  {Drop the flags from the header}
  and ecx, DropMediumAndLargeFlagsMask
  {Get a pointer to the next block in rdi}
  lea rdi, [rsi + rcx]
  {Subtract the block header size from the old available size}
  sub ecx, BlockHeaderSize
  {Get the complete flags in ebx}
  and ebx, ExtractMediumAndLargeFlagsMask
  {Is it an upsize or a downsize?}
  cmp rdx, rcx
  ja @MediumBlockUpsize
  {Status: ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
   rdi = @Next Block, rsi = APointer, rdx = Requested Size}
  {Must be less than half the current size or we don't bother resizing.}
  lea r15, [rdx + rdx]
  cmp r15, rcx
  jb @MediumMustDownsize
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumNoResize:
  mov rax, rsi
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumMustDownsize:
  {In-place downsize? Balance the cost of moving the data vs. the cost of
   fragmenting the memory pool. Medium blocks in use may never be smaller
   than MinimumMediumBlockSize.}
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  {The requested size is less than the minimum medium block size. If the
  requested size is less than the threshold value (currently a quarter of the
  minimum medium block size), move the data to a small block, otherwise shrink
  the medium block to the minimum allowable medium block size.}
  cmp edx, MediumInPlaceDownsizeLimit
  jb @MediumDownsizeRealloc
  {The request is for a size smaller than the minimum medium block size, but
   not small enough to justify moving data: Reduce the block size to the
   minimum medium block size}
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
  {Is it already at the minimum medium block size?}
  cmp ecx, edx
  jna @MediumNoResize
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MediumBlockInPlaceDownsize:
  {Round up to the next medium block size}
  lea r15, [rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and r15, MediumBlockGranularityMask
  add r15, MediumBlockSizeOffset
  {Get the size of the second split}
  add ecx, BlockHeaderSize
  sub ecx, r15d
  {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  lea r8, IsMultiThread
  cmp byte ptr [r8], False
  je @DoMediumInPlaceDownsize
{$ENDIF}
//@DoMediumLockForDownsize:
  {Lock the medium blocks}
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  // Assembly version of LockMediumBlocks preserves all volatile register (except eax), but non-assembly version does not, so we save rcx and rdx for non-assembly implementation
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push rcx; push rdx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop rdx; pop rcx {$ENDIF}
{$ELSE}
  // this function saves all volatile registers, so we do not have to save ecx, etc
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {Reread the flags - they may have changed before medium blocks could be
   locked.}
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [rsi - BlockHeaderSize]
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DoMediumInPlaceDownsize:
  {Set the new size}
  or rbx, r15
  mov [rsi - BlockHeaderSize], rbx
  {Get the second split size in ebx}
  mov ebx, ecx
  {Is the next block in use?}
  mov rdx, [rdi - BlockHeaderSize]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  {The next block is in use: flag its previous block as free}
  or rdx, PreviousMediumBlockIsFreeFlag
  mov [rdi - BlockHeaderSize], rdx
  jmp @MediumDownsizeDoSplit
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumDownsizeNextBlockFree:
  {The next block is free: combine it}
  mov rcx, rdi
  and rdx, DropMediumAndLargeFlagsMask
  add rbx, rdx
  add rdi, rdx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumDownsizeDoSplit:
  {Store the trailing size field}
  mov [rdi - 2 * BlockHeaderSize], rbx
  {Store the free part's header}
  lea rcx, [rbx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [rsi + r15 - BlockHeaderSize], rcx
  {Bin this free block}
  cmp rbx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea rcx, [rsi + r15]
  mov rdx, rbx
  call InsertMediumBlockIntoBin
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockDownsizeDone:
  {Result = old pointer}
  mov rax, rsi
  {Unlock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @Done
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point, since we are about to exit}
  call UnlockMediumBlocks
  {Result = old pointer}
  mov rax, rsi
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumDownsizeRealloc:
  {Save the requested size}
  mov rdi, rdx
  mov rcx, rdx
  {Allocate the new block}
  call FastGetMem
  test rax, rax
  jz @Done
  {$IFDEF DEBUG}
  test rax, AlignmentMask
  jz @@OkAlignmentOnGetMem8
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem8:
  {$ENDIF}
  {Save the result}
  mov r15, rax
  mov rdx, rax
  mov rcx, rsi
  mov r8, rdi
  {Move the data across}
{$IFDEF UseCustomVariableSizeMoveRoutines}
  {$IFDEF Align32Bytes}
  call MoveX32LPUniversal
  {$ELSE}
  {$IFDEF Align16Bytes}
  call MoveX16LP
  {$ELSE}
  call MoveX8LP
  {$ENDIF}
  {$ENDIF}
{$ELSE}
  call System.Move
{$ENDIF}
  mov rcx, rsi
  call FastFreeMem
  {Return the result}
  mov rax, r15
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumBlockUpsize:
  {Status: ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
   rdi = @Next Block, rsi = APointer, rdx = Requested Size}
  {Can we do an in-place upsize?}
  mov rax, [rdi - BlockHeaderSize]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  {Get the total available size including the next block}
  and rax, DropMediumAndLargeFlagsMask
  {r15 = total available size including the next block (excluding the header)}
  lea r15, [rax + rcx]
  {Can the block fit?}
  cmp rdx, r15
  ja @CannotUpsizeMediumBlockInPlace
  {The next block is free and there is enough space to grow this
   block in place.}
{$IFNDEF AssumeMultiThreaded}
  lea r8, IsMultiThread
  cmp byte ptr [r8], False
  je @DoMediumInPlaceUpsize
{$ENDIF}
//@DoMediumLockForUpsize:
{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
  {Lock the medium blocks.}
  // Assembly version of LockMediumBlocks preserves all volatile register (except eax), but non-assembly version does not, so we save ecx and edx for non-assembly implementation
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} push rcx; push rdx {$ENDIF}
  call LockMediumBlocks
  {$IFNDEF UseOriginalFastMM4_LockMediumBlocksAsm} pop rdx; pop rcx {$ENDIF}
{$ELSE}
  // This function preserves all volatile registers
  call AcquireSpinLockMediumBlocks
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  or r12b, (UnsignedBit shl StateBitMediumLocked)
{$ENDIF}
  {Re-read the info for this block (since it may have changed before the medium
   blocks could be locked)}
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [rsi - BlockHeaderSize]
  {Re-read the info for the next block}
  mov rax, [rdi - BlockheaderSize]
  {Next block still free?}
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  {Recalculate the next block size}
  and eax, DropMediumAndLargeFlagsMask
  {The available size including the next block}
  lea r15, [rax + rcx]
  {Can the block still fit?}
  cmp rdx, r15
  ja @NextMediumBlockChanged
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@DoMediumInPlaceUpsize:
  {Is the next block binnable?}
  cmp eax, MinimumMediumBlockSize
  {Remove the next block}
  jb @MediumInPlaceNoNextRemove
  mov r14, rcx
  mov rcx, rdi
  mov rdi, rdx
  call RemoveMediumFreeBlock
  mov rcx, r14
  mov rdx, rdi
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@MediumInPlaceNoNextRemove:
  {Medium blocks grow a minimum of 25% in in-place upsizes}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  {Round up to the nearest block size granularity}
  lea eax, [eax + edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, MediumBlockGranularityMask
  add eax, MediumBlockSizeOffset
  {Calculate the size of the second split}
  lea rdx, [r15 + BlockHeaderSize]
  sub edx, eax
  {Does it fit?}
  ja @MediumInPlaceUpsizeSplit
  {Grab the whole block: Mark it as used in the block following it}
  and qword ptr [rsi + r15], not PreviousMediumBlockIsFreeFlag
  {The block size is the full available size plus header}
  add r15, BlockHeaderSize
  {Upsize done}
  jmp @MediumUpsizeInPlaceDone
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@MediumInPlaceUpsizeSplit:
  {Store the size of the second split as the second last dword}
  mov [rsi + r15 - BlockHeaderSize], rdx
  {Set the second split header}
  lea edi, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rsi + rax - BlockHeaderSize], rdi
  mov r15, rax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  lea rcx, [rsi + rax]
  call InsertMediumBlockIntoBin
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@MediumUpsizeInPlaceDone:
  {Set the size and flags for this block}
  or r15, rbx
  mov [rsi - BlockHeaderSize], r15
  {Result = old pointer}
  mov rax, rsi
  {Unlock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @Done
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call destroys most of the volatile (caller-saved) registers,
(RAX, RCX, RDX, R8, R9, R10, R11),
but we don't need them at this point, since we are about to exit}
  call UnlockMediumBlocks
  {Result = old pointer}
  mov rax, rsi
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@NextMediumBlockChanged:
  {The next medium block changed while the medium blocks were being locked}
{$IFNDEF AssumeMultiThreaded}
  test r12b, (UnsignedBit shl StateBitMediumLocked)
  jz @DontUnlMedBlksAftrNxtMedBlkChg
{$ENDIF}

{$IFDEF CheckPauseAndSwitchToThreadForAsmVersion}
{The call to "UnlockMediumBlocks" destroys most of the volatile (caller-saved)
registers (RAX, RCX, RDX, R8, R9, R10, R11),
so ew save RCX and RDX}
  mov rbx, rcx // save rcx
  mov r15, rdx // save rdx
  call UnlockMediumBlocks
  mov rcx, rbx // restore rcx
  mov rdx, r15 // restore rdx
{$ELSE}
{$IFDEF InterlockedRelease}
  lock
{$ENDIF}
  mov MediumBlocksLocked, cLockByteAvailable
{$ENDIF}

  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}

@DontUnlMedBlksAftrNxtMedBlkChg:

@CannotUpsizeMediumBlockInPlace:
  {Couldn't upsize in place. Grab a new block and move the data across:
   If we have to reallocate and move medium blocks, we grow by at
   least 25%}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor rdi, rdi
  sub rax, rdx
  adc rdi, -1
  and rax, rdi
  add rax, rdx
  {Save the size to allocate}
  mov r15, rax
  {Save the size to move across}
  mov edi, ecx
  {Save the requested size}
  mov rbx, rdx
  {Get the block}
  mov rcx, rax
  call FastGetMem
  mov rdx, rbx
  {Success?}
  test eax, eax
  jz @Done
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMem9
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMem9:
  {$ENDIF}
  {If it's a Large block - store the actual user requested size}
  cmp r15, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [rax - 2 * BlockHeaderSize], rdx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@MediumUpsizeNotLarge:
  {Save the result}
  mov r15, rax
  {Move the data across}
  mov rdx, rax
  mov rcx, rsi
  mov r8, rdi
{$IFDEF UseCustomVariableSizeMoveRoutines}
{$IFDEF Align32Bytes}
  call MoveX32LPUniversal
{$ELSE}
  {$IFDEF Align16Bytes}
  call MoveX16LP
  {$ELSE}
  call MoveX8LP
  {$ENDIF}
{$ENDIF}
{$ELSE}
  call System.Move
{$ENDIF}
  {Free the old block}
  mov rcx, rsi
  call FastFreeMem
  {Restore the result}
  mov rax, r15
  jmp @Done
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 8{$ENDIF}
@PossibleLargeBlock:
  {-----------------------Large block------------------------------}
  {Is this a valid large block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Error
  mov rcx, rsi
  call ReallocateLargeBlock
  jmp @Done
  {-----------------------Invalid block------------------------------}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@Error:
  xor eax, eax
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@Done: {restores registers from stack}
{$IFNDEF AllowAsmParams}
  pop r15
  pop r14
  {$IFNDEF AssumeMultiThreaded}
  pop r12
  {$ENDIF}
  pop rdi
  pop rsi
  pop rbx
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}
{$ENDIF FastReallocMemNeedAssemberCode}

{Allocates a block and fills it with zeroes}
function FastAllocMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc} NativeUInt{$ELSE}Cardinal{$ENDIF}{$ENDIF}): Pointer;
{$IFNDEF ASMVersion}
{$IFDEF LogLockContention}
{$IFDEF FullDebugMode}
var
  LCollector: PStaticCollector;
{$ENDIF}
{$ENDIF}
begin
  {DebugAllocMem does not call FastAllocMem so in this case we can ignore returned collector.}
  Result := FastGetMem(ASize{$IFDEF LogLockContention}{$IFDEF FullDebugMode}, LCollector{$ENDIF}{$ENDIF});
  {Large blocks are already zero filled}
  if (Result <> nil) and (ASize <= (MaximumMediumBlockSize - BlockHeaderSize)) then
    FillChar(Result^, ASize, 0);
end;
{$ELSE}
{$IFDEF 32Bit}
assembler;
asm
  push ebx
  {Get the size rounded down to the previous multiple of 4 into ebx}
  lea ebx, [eax - 1]
  and ebx, -4
  {Get the block}
  call FastGetMem
  {$IFDEF DEBUG}
  test eax, AlignmentMask
  jz @@OkAlignmentOnGetMemA
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMemA:
  {$ENDIF}

  {Could a block be allocated? ecx = 0 if yes, $ffffffff if no}
  cmp eax, 1
  sbb ecx, ecx
  {Point edx to the last dword}
  lea edx, [eax + ebx]
  {ebx = $ffffffff if no block could be allocated, otherwise size rounded down
   to previous multiple of 4. If ebx = 0 then the block size is 1..4 bytes and
   the FPU based clearing loop should not be used (since it clears 8 bytes per
   iteration).}
  or ebx, ecx
  jz @ClearLastDWord
  {Large blocks are already zero filled}
  cmp ebx, MaximumMediumBlockSize - BlockHeaderSize
  jae @Done


  test FastMMCpuFeaturesA, FastMMCpuFeatureERMS
  jz @NoERMS

  push edi
  push eax
  xor  eax, eax
  mov  edi, edx
  sub  edi, ebx
  mov  ecx, ebx
  cld
  rep  stosb
  mov  [edi], eax // clear last 4 bytes
  pop  eax
  pop  edi
  jmp  @Done



@NoERMS:
  {Make the counter negative based}
  neg ebx
  {Load zero into st(0)}
  fldz
  {Clear groups of 8 bytes. Block sizes are always four less than a multiple
   of 8.}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@FillLoop:
  fst qword ptr [edx + ebx]
  add ebx, 8
  js @FillLoop
  {Clear st(0)}
  ffree st(0)
  {Correct the stack top}
  fincstp
  {Clear the last four bytes}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@ClearLastDWord:
  mov [edx], ecx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@Done:
  pop ebx
end;

{$ELSE}

{---------------64-bit BASM FastAllocMem---------------}
assembler;
asm
  {Do not put ".noframe" here since it calls other functions.}
  {$IFDEF AllowAsmParams}
  .params 1
  .pushnv rbx
  {$ELSE}
  push rbx
  {$ENDIF}
  {Get the size rounded down to the previous multiple of SizeOf(Pointer) into
   ebx}
  lea rbx, [rcx - 1]
  and rbx, -8
  {Get the block}
  call FastGetMem
  {$IFDEF DEBUG}
  test rax, AlignmentMask
  jz @@OkAlignmentOnGetMemB
  jmp BadAlignmentOnGetMem
@@OkAlignmentOnGetMemB:
  {$ENDIF}
  {Could a block be allocated? rcx = 0 if yes, -1 if no}
  cmp rax, 1
  sbb rcx, rcx
  {Point rdx to the last dword}
  lea rdx, [rax + rbx]
  {rbx = -1 if no block could be allocated, otherwise size rounded down
   to previous multiple of 8. If rbx = 0 then the block size is 1..8 bytes and
   the SSE2 based clearing loop should not be used (since it clears 16 bytes per
   iteration).}
  or rbx, rcx
  jz @ClearLastQWord
  {Large blocks are already zero filled}
  cmp rbx, MaximumMediumBlockSize - BlockHeaderSize
  jae @Done

  push rdi
  push rax
  xor  eax, eax
  mov  rdi, rdx
  sub  rdi, rbx
  mov  rcx, rbx
  cld
  rep  stosb
  mov  [rdi], rax // clear last 8 bytes
  pop  rax
  pop  rdi
  jmp  @Done
  {Clear the last 8 bytes}
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@ClearLastQWord:
  xor rcx, rcx
  mov [rdx], rcx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@Done:
{$IFNDEF AllowAsmParams}
  pop rbx
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}

{$IFDEF fpc}
function FastFreeMemSize(p: pointer; size: NativeUInt):NativeUInt;
{$IFNDEF ASMVersion}
begin
  if size=0 then
    exit(0);
  { can't free partial blocks, ignore size }
  result := FastFreeMem(p);
{$ELSE}
assembler;
asm
  test edx, edx
  jne @SizeNotZero
  mov eax, 0
  jmp @Final
@SizeNotZero:
  call FastFreeMem
@Final:
{$ENDIF}
end;

function FastMemSize(p: pointer): NativeUInt;
{$IFNDEF ASMVersion}
begin
  Result := GetAvailableSpaceInBlock(p);
{$ELSE}
assembler;
asm
  call GetAvailableSpaceInBlock
{$ENDIF}
end;
{$ENDIF}

{-----------------Post Uninstall GetMem/FreeMem/ReallocMem-------------------}

{$IFDEF DetectMMOperationsAfterUninstall}

function InvalidGetMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
{$IFNDEF NoMessageBoxes}
var
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
begin
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(InvalidGetMemMsg);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle, Length(InvalidOperationTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0])-1));
  ShowMessageBox(InvalidGetMemMsg, LErrorMessageTitle);
{$ENDIF}
  Result := nil;
end;

function InvalidFreeMem(APointer: Pointer): {$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF};
{$IFNDEF NoMessageBoxes}
var
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
begin
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(InvalidFreeMemMsg);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle, Length(InvalidOperationTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0])-1));
  ShowMessageBox(InvalidFreeMemMsg, LErrorMessageTitle);
{$ENDIF}
  Result := {$IFDEF fpc}NativeUInt(-1){$ELSE}-1{$ENDIF};
end;

function InvalidReallocMem({$IFDEF fpc}var {$ENDIF}APointer: Pointer; ANewSize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
{$IFNDEF NoMessageBoxes}
var
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
begin
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(InvalidReallocMemMsg);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle, Length(InvalidOperationTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0])-1));
  ShowMessageBox(InvalidReallocMemMsg, LErrorMessageTitle);
{$ENDIF}
  Result := nil;
end;

function InvalidAllocMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}{$IFDEF fpc}NativeUInt{$ELSE}Cardinal{$ENDIF}{$ENDIF}): Pointer;
{$IFNDEF NoMessageBoxes}
var
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
begin
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(InvalidAllocMemMsg);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle, Length(InvalidOperationTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
  ShowMessageBox(InvalidAllocMemMsg, LErrorMessageTitle);
{$ENDIF}
  Result := nil;
end;

function InvalidRegisterAndUnRegisterMemoryLeak(APointer: Pointer): Boolean;
begin
  Result := False;
end;

{$ENDIF}

{------------------------EventLog handling------------------------}

{$IFDEF _EventLog}
procedure DeleteEventLog;
begin
  {Delete the file}
  DeleteFileA(MMLogFileName);
end;

{Finds the start and length of the file name given a full path.}
procedure ExtractFileName(APFullPath: PAnsiChar; var APFileNameStart: PAnsiChar; var AFileNameLength: NativeInt);
var
  LChar: AnsiChar;
  LPFullPath: PAnsiChar;
begin
  {Initialize}
  LPFullPath := APFullPath;
  APFileNameStart := LPFullPath;
  AFileNameLength := 0;
  {Find the file }
  while True do
  begin
    {Get the next character}
    LChar := LPFullPath^;
    {End of the path string?}
    if LChar = #0 then
      Break;
    {Advance the buffer position}
    Inc(LPFullPath);
    {Found a backslash? -> May be the start of the file name}
    if LChar = '\' then
      APFileNameStart := LPFullPath;
  end;
  {Calculate the length of the file name}
  AFileNameLength := IntPtr(LPFullPath) - IntPtr(APFileNameStart);
end;

procedure AppendEventLog(ABuffer: Pointer; ACount: Cardinal);
const
  {Declared here, because it is not declared in the SHFolder.pas unit of some older Delphi versions.}
  SHGFP_TYPE_CURRENT = 0;
var
  LFileHandle: THandle; {use NativeUint if THandle is not available}
  LBytesWritten: Cardinal;
  LEventHeader: array[0..MaxDisplayMessageLength-1] of AnsiChar;
  LAlternateLogFileName: array[0..MaxFileNameLengthDouble-1] of AnsiChar;
  LPathLen, LNameLength: NativeInt;
  LInitialPtr, LMsgPtr, LPFileName: PAnsiChar;
  LInitialSize: Cardinal;
  LSystemTime: TSystemTime;
begin
  {Try to open the log file in read/write mode.}
  FillChar(LAlternateLogFileName, SizeOf(LAlternateLogFileName), 0);
  LFileHandle := CreateFileA(MMLogFileName, GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  {Did log file creation fail? If so, the destination folder is perhaps read-only:
   Try to redirect logging to a file in the user's "My Documents" folder.}
  if (LFileHandle = INVALID_HANDLE_VALUE)
{$IFNDEF FPC}
   {$IFNDEF MACOS}
{$IFDEF Delphi4or5}
    and SHGetSpecialFolderPathA(0, @(LAlternateLogFileName[0]), CSIDL_PERSONAL, True) then
{$ELSE}
    and (SHGetFolderPathA(0, CSIDL_PERSONAL or CSIDL_FLAG_CREATE, 0,
      SHGFP_TYPE_CURRENT, @(LAlternateLogFileName[0])) = S_OK) then
{$ENDIF}
  {$ELSE}
  then
  {$ENDIF}
{$ELSE}
then
{$ENDIF}
  begin
    {Extract the filename part from MMLogFileName and append it to the path of
     the "My Documents" folder.}
    LPathLen := StrLen(LAlternateLogFileName);
    {Ensure that there is a trailing backslash in the path}
    if (LPathLen = 0) or (LAlternateLogFileName[LPathLen - 1] <> '\') then
    begin
      LAlternateLogFileName[LPathLen] := '\';
      Inc(LPathLen);
    end;
    {Add the filename to the path}
    ExtractFileName(@(MMLogFileName[0]), LPFileName, LNameLength);
    System.Move(LPFileName^, LAlternateLogFileName[LPathLen], (LNameLength + 1)*SizeOf(LPFileName[0]));
    {Try to open the alternate log file}
    LFileHandle := CreateFileA(LAlternateLogFileName, GENERIC_READ or GENERIC_WRITE,
      0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  end;
  {Was the log file opened/created successfully?}
  if LFileHandle <> INVALID_HANDLE_VALUE then
  begin
    {Seek to the end of the file}
    SetFilePointer(LFileHandle, 0, nil, FILE_END);
    {Set the separator}
    LMsgPtr := @LEventHeader[0];
    LInitialPtr := LMsgPtr;
    LInitialSize := (SizeOf(LEventHeader) div SizeOf(LEventHeader[0]))-1;
    LMsgPtr := AppendStringToBuffer(CRLF, @LEventHeader[0], Length(CRLF), (SizeOf(LEventHeader) div SizeOf(LEventHeader[0])-1));
    LMsgPtr := AppendStringToBuffer(EventSeparator, LMsgPtr, Length(EventSeparator), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    {Set the date & time}
    GetLocalTime(LSystemTime);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wYear, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr^ := '/';
    Inc(LMsgPtr);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wMonth, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr^ := '/';
    Inc(LMsgPtr);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wDay, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr^ := ' ';
    Inc(LMsgPtr);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wHour, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr^ := ':';
    Inc(LMsgPtr);
    if LSystemTime.wMinute < 10 then
    begin
      LMsgPtr^ := '0';
      Inc(LMsgPtr);
    end;
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wMinute, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr^ := ':';
    Inc(LMsgPtr);
    if LSystemTime.wSecond < 10 then
    begin
      LMsgPtr^ := '0';
      Inc(LMsgPtr);
    end;
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.WSecond, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    {Write the header}
    LMsgPtr := AppendStringToBuffer(EventSeparator, LMsgPtr, Length(EventSeparator), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    WriteFile(LFileHandle, LEventHeader[0], NativeUInt(LMsgPtr) - NativeUInt(@LEventHeader[0]), LBytesWritten, nil);
    {Write the data}
    WriteFile(LFileHandle, ABuffer^, ACount, LBytesWritten, nil);
    {Close the file}
    CloseHandle(LFileHandle);
  end;
end;

{Sets the default log filename}
procedure SetDefaultMMLogFileName;
const
  LogFileExtAnsi: PAnsiChar = LogFileExtension;
var
  LEnvVarLength, LModuleNameLength: Cardinal;
  LPathOverride: array[0..MaxFileNameLengthDouble-1] of AnsiChar;
  LPFileName: PAnsiChar;
  LFileNameLength: NativeInt;
begin
  {Get the name of the application}
  LModuleNameLength := AppendModuleFileName(@(MMLogFileName[0]), SizeOf(MMLogFileName));
  {Replace the last few characters of the module name, and optionally override
   the path.}
  if LModuleNameLength > 0 then
  begin
    {Change the filename}
    System.Move(LogFileExtAnsi^, MMLogFileName[LModuleNameLength - 4], (StrLen(LogFileExtAnsi) + 1)*SizeOf(LogFileExtAnsi[0]));
    {Try to read the FastMMLogFilePath environment variable}
    LEnvVarLength := GetEnvironmentVariableA('FastMMLogFilePath',
      @LPathOverride[0], SizeOf(LPathOverride) div SizeOf(LPathOverride[0])-1);
    {Does the environment variable exist? If so, override the log file path.}
    if LEnvVarLength > 0 then
    begin
      {Ensure that there's a trailing backslash.}
      if LPathOverride[LEnvVarLength - 1] <> '\' then
      begin
        LPathOverride[LEnvVarLength] := '\';
        Inc(LEnvVarLength);
      end;
      {Add the filename to the path override}
      ExtractFileName(@MMLogFileName[0], LPFileName, LFileNameLength);
      System.Move(LPFileName^, LPathOverride[LEnvVarLength], (LFileNameLength + 1)*SizeOf(LPFileName[0]));
      {Copy the override path back to the filename buffer}
      System.Move(LPathOverride[0], MMLogFileName[0], SizeOf(MMLogFileName) - SizeOf(MMLogFileName[0]));
    end;
  end;
end;

{Specify the full path and name for the filename to be used for logging memory
 errors, etc. If ALogFileName is nil or points to an empty string it will
 revert to the default log file name.}
procedure SetMMLogFileName(ALogFileName: PAnsiChar = nil);
var
  LLogFileNameLen: Integer;
begin
  {Is ALogFileName valid?}
  if (ALogFileName <> nil) and (ALogFileName^ <> #0) then
  begin
    LLogFileNameLen := StrLen(ALogFileName);
    if LLogFileNameLen < Length(MMLogFileName) then
    begin
      {Set the log file name}
      System.Move(ALogFileName^, MMLogFileName, (LLogFileNameLen + 1)*SizeOf(ALogFileName[0]));
      Exit;
    end;
  end;
  {Invalid log file name}
  SetDefaultMMLogFileName;
end;
{$ENDIF}

{-----------------Full Debug Mode Memory Manager Interface--------------------}

{$IFDEF FullDebugMode}

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg32(CompareVal, NewVal: Integer; AAddress: PInteger): Integer; assembler;
asm
{$IFDEF 32Bit}
  {On entry for 32-bit Windows:
    eax = CompareVal,
    edx = NewVal,
    ecx = AAddress}
    lock cmpxchg [ecx], edx  // cmpxchg also uses EAX as an implicit operand
    xor edx, edx {Clear the edx and ecx value on exit just for safety}
    xor ecx, ecx
{$ELSE}
{$IFDEF AllowAsmNoframe}
.noframe
{$ENDIF}
  {On entry for 64-bit Windows:
    ecx = CompareVal,
    edx = NewVal,
    r8 = AAddress}
    mov eax, ecx  // higher bits (63-32) are automatically cleared
    xor ecx, ecx {Clear the ecx value on entry just for safety, after we had save the value to eax}
    lock cmpxchg [r8], edx  // cmpxchg also uses EAX as an implicit operand
    xor edx, edx
    xor r8, r8
{$ENDIF}
end;

{Called by DebugGetMem, DebugFreeMem and DebugReallocMem in order to block a
 free block scan operation while the memory pool is being modified.}
procedure StartChangingFullDebugModeBlock;
var
  LOldCount: Integer;
begin
  while True do
  begin
    {Get the old thread count}
    LOldCount := ThreadsInFullDebugModeRoutine;
    if (LOldCount >= 0)
      and (LockCmpxchg32(LOldCount, LOldCount + 1, @ThreadsInFullDebugModeRoutine) = LOldCount) then
    begin
      Break;
    end;
  {$IFDEF NeverSleepOnThreadContention}
    {$IFDEF UseSwitchToThread}
    SwitchToThreadIfSupported;
    {$ENDIF}
  {$ELSE}
    Sleep(InitialSleepTime);
    {Try again}
    LOldCount := ThreadsInFullDebugModeRoutine;
    if (LOldCount >= 0)
      and (LockCmpxchg32(LOldCount, LOldCount + 1, @ThreadsInFullDebugModeRoutine) = LOldCount) then
    begin
      Break;
    end;
    Sleep(AdditionalSleepTime);
  {$ENDIF}
  end;
end;

procedure DoneChangingFullDebugModeBlock; assembler;
asm
{$IFDEF 32Bit}
  lock dec ThreadsInFullDebugModeRoutine
{$ELSE}
{$IFDEF AllowAsmNoframe}
.noframe
{$ENDIF}
  lea rax, ThreadsInFullDebugModeRoutine
  lock dec dword ptr [rax]
{$ENDIF}
end;

{Increments the allocation number}
procedure IncrementAllocationNumber; assembler;
asm
{$IFDEF 32Bit}
  lock inc CurrentAllocationNumber
{$ELSE}
{$IFDEF AllowAsmNoframe}
.noframe
{$ENDIF}
  lea rax, CurrentAllocationNumber
  lock inc dword ptr [rax]
{$ENDIF}
end;

{Called by a routine wanting to lock the entire memory pool in FullDebugMode, e.g. before scanning the memory
 pool for corruptions.}
procedure BlockFullDebugModeMMRoutines;
begin
  while True do
  begin
    {Get the old thread count}
    if LockCmpxchg32(0, -1, @ThreadsInFullDebugModeRoutine) = 0 then
      Break;
{$IFDEF NeverSleepOnThreadContention}
  {$IFDEF UseSwitchToThread}
    SwitchToThreadIfSupported;
  {$ENDIF}
{$ELSE}
    Sleep(InitialSleepTime);
    {Try again}
    if LockCmpxchg32(0, -1, @ThreadsInFullDebugModeRoutine) = 0 then
      Break;
    Sleep(AdditionalSleepTime);
{$ENDIF}
  end;
end;

procedure UnblockFullDebugModeMMRoutines;
begin
  {Currently blocked? If so, unblock the FullDebugMode routines.}
  if ThreadsInFullDebugModeRoutine = -1 then
    ThreadsInFullDebugModeRoutine := 0;
end;

{Returns the current "allocation group". Whenever a GetMem request is serviced
 in FullDebugMode, the current "allocation group" is stored in the block header.
 This may help with debugging. Note that if a block is subsequently reallocated
 that it keeps its original "allocation group" and "allocation number" (all
 allocations are also numbered sequentially).}
function GetCurrentAllocationGroup: Cardinal;
begin
  Result := AllocationGroupStack[AllocationGroupStackTop];
end;

{Allocation groups work in a stack like fashion. Group numbers are pushed onto
 and popped off the stack. Note that the stack size is limited, so every push
 should have a matching pop.}
procedure PushAllocationGroup(ANewCurrentAllocationGroup: Cardinal);
begin
  if AllocationGroupStackTop < AllocationGroupStackSize - 1 then
  begin
    Inc(AllocationGroupStackTop);
    AllocationGroupStack[AllocationGroupStackTop] := ANewCurrentAllocationGroup;
  end
  else
  begin
    {Raise a runtime error if the stack overflows}
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
end;

procedure PopAllocationGroup;
begin
  if AllocationGroupStackTop > 0 then
  begin
    Dec(AllocationGroupStackTop);
  end
  else
  begin
    {Raise a runtime error if the stack underflows}
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
end;

{Sums all the dwords starting at the given address. ACount must be > 0 and a
 multiple of SizeOf(Pointer).}
function SumNativeUInts(AStartValue: NativeUInt; APointer: PNativeUInt;
  ACount: NativeUInt): NativeUInt; assembler;
asm
{$IFDEF 32Bit}
  {On entry: eax = AStartValue, edx = APointer; ecx = ACount}
  add edx, ecx
  neg ecx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@AddLoop:
  add eax, [edx + ecx]
  add ecx, 4
  js @AddLoop
{$ELSE}
{$IFDEF AllowAsmNoframe}
  .noframe
{$ENDIF}
  {On entry: rcx = AStartValue, rdx = APointer; r8 = ACount}
  add rdx, r8
  neg r8
  mov rax, rcx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@AddLoop:
  add rax, [rdx + r8]
  add r8, 8
  js @AddLoop
{$ENDIF}
end;

{Checks the memory starting at the given address for the fill pattern.
 Returns True if all bytes are all valid. ACount must be >0 and a multiple of
 SizeOf(Pointer).}
function CheckFillPattern(APointer: Pointer; ACount: NativeUInt;
  AFillPattern: NativeUInt): Boolean; assembler;
asm
{$IFDEF 32Bit}
  {On entry: eax = APointer; edx = ACount; ecx = AFillPattern}
  add eax, edx
  neg edx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@CheckLoop:
  cmp [eax + edx], ecx
  jne @Done
  add edx, 4
  js @CheckLoop
@Done:
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
  sete al
{$ELSE}
  {On entry: rcx = APointer; rdx = ACount; r8 = AFillPattern}
  {$IFDEF AllowAsmNoframe}
  .noframe
  {$ENDIF}
  add rcx, rdx
  neg rdx
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 16{$ENDIF}
@CheckLoop:
  cmp [rcx + rdx], r8
  jne @Done
  add rdx, 8
  js @CheckLoop
  {$IFDEF AsmCodeAlign}{$IFDEF AsmAlNoDot}align{$ELSE}.align{$ENDIF} 4{$ENDIF}
@Done:
  sete al
{$ENDIF}
end;

{Calculates the checksum for the debug header. Adds all dwords in the debug
 header to the start address of the block.}
function CalculateHeaderCheckSum(APointer: PFullDebugBlockHeader): NativeUInt;
begin
  Result := SumNativeUInts(
    NativeUInt(APointer),
    PNativeUInt(PByte(APointer) + 2 * SizeOf(Pointer)),
    SizeOf(TFullDebugBlockHeader) - 2 * SizeOf(Pointer) - SizeOf(NativeUInt));
end;

procedure UpdateHeaderAndFooterCheckSums(APointer: PFullDebugBlockHeader);
var
  LHeaderCheckSum: NativeUInt;
begin
  LHeaderCheckSum := CalculateHeaderCheckSum(APointer);
  APointer^.HeaderCheckSum := LHeaderCheckSum;
  PNativeUInt(PByte(APointer) + SizeOf(TFullDebugBlockHeader) + APointer^.UserSize)^ := not LHeaderCheckSum;
end;

function LogCurrentThreadAndStackTrace(ASkipFrames: Cardinal; ABuffer: PAnsiChar; ABufferLengthChars: Cardinal): PAnsiChar;
var
  LCurrentStackTrace: TStackTrace;
  LInitialBufPtr: PAnsiChar;
  LDiff, LInitialLengthChars, LC: NativeUInt;
  LBufferLengthChars: Cardinal;
  L: Integer;
begin
  LBufferLengthChars := ABufferLengthChars;
  {Get the current call stack}
  GetStackTrace(@LCurrentStackTrace[0], StackTraceDepth, ASkipFrames);
  {Log the thread ID}
  Result := ABuffer;
  L := Length(CurrentThreadIDMsg);
  if (L > 0) then
  begin
    LC := L;
    if LC < LBufferLengthChars then
    begin
      Result := AppendStringToBuffer(CurrentThreadIDMsg, ABuffer, Length(CurrentThreadIDMsg), LBufferLengthChars);
      Dec(LBufferLengthChars, Length(CurrentThreadIDMsg));
      LInitialBufPtr := Result;
      LInitialLengthChars := LBufferLengthChars;
      Result := NativeUIntToHexBuf(GetThreadID, Result, LInitialLengthChars-NativeUInt(LInitialBufPtr-Result));
      {List the stack trace}
      if LInitialBufPtr >= Result then
      begin
        LDiff := LInitialBufPtr-Result;
        if LDiff <= LInitialLengthChars then
        begin
          Result := AppendStringToBuffer(CurrentStackTraceMsg, Result, Length(CurrentStackTraceMsg), LInitialLengthChars-LDiff);
          if LInitialBufPtr >= Result then
          begin
            LDiff := LInitialBufPtr-Result;
            if LDiff <= LInitialLengthChars then
            begin
              Result := LogStackTrace(@LCurrentStackTrace[0], StackTraceDepth, Result);
            end;
          end;
        end;
      end;
    end;
  end;
end;

{$IFNDEF DisableLoggingOfMemoryDumps}
function LogMemoryDump(APointer: PFullDebugBlockHeader; ABuffer: PAnsiChar; ABufSize: Cardinal): PAnsiChar;
var
  LByteNum, LVal: Cardinal;
  LDataPtr: PByte;
begin
  Result := AppendStringToBuffer(MemoryDumpMsg, ABuffer, Length(MemoryDumpMsg), ABufSize);
  {todo: Implement ABufSize checking and in this function}
  Result := NativeUIntToHexBuf(NativeUInt(APointer) + SizeOf(TFullDebugBlockHeader), Result, ABufSize{todo});
  Result^ := ':';
  Inc(Result);
  {Add the bytes}
  LDataPtr := PByte(PByte(APointer) + SizeOf(TFullDebugBlockHeader));
  for LByteNum := 0 to 255 do
  begin
    if (LByteNum and 31) = 0 then
    begin
      Result^ := #13;
      Inc(Result);
      Result^ := #10;
      Inc(Result);
    end
    else
    begin
      Result^ := ' ';
      Inc(Result);
    end;
    {Set the hex data}
    LVal := Byte(LDataPtr^);
    Result^ := HexTable[LVal shr 4];
    Inc(Result);
    Result^ := HexTable[LVal and $f];
    Inc(Result);
    {Next byte}
    Inc(LDataPtr);
  end;
  {Dump ASCII}
  LDataPtr := PByte(PByte(APointer) + SizeOf(TFullDebugBlockHeader));
  for LByteNum := 0 to 255 do
  begin
    if (LByteNum and 31) = 0 then
    begin
      Result^ := #13;
      Inc(Result);
      Result^ := #10;
      Inc(Result);
    end
    else
    begin
      Result^ := ' ';
      Inc(Result);
      Result^ := ' ';
      Inc(Result);
    end;
    {Set the hex data}
    LVal := Byte(LDataPtr^);
    if LVal < 32 then
      Result^ := '.'
    else
      Result^ := AnsiChar(LVal);
    Inc(Result);
    {Next byte}
    Inc(LDataPtr);
  end;
end;
{$ENDIF}

{Rotates AValue ABitCount bits to the right}
function RotateRight(AValue, ABitCount: NativeUInt): NativeUInt; assembler;
asm
{$IFDEF 32Bit}
  mov ecx, edx
  ror eax, cl
{$ELSE}
{$IFDEF AllowAsmNoframe}
  .noframe
{$ENDIF}
  mov rax, rcx
  mov rcx, rdx
  ror rax, cl
{$ENDIF}
end;

{Determines whether a byte in the user portion of the freed block has been modified. Does not work beyond
 the end of the user portion (i.e. footer and beyond).}
function FreeBlockByteWasModified(APointer: PFullDebugBlockHeader; AUserOffset: NativeUInt): Boolean;
var
  LFillPattern: NativeUInt;
begin
  {Get the expected fill pattern}
  if AUserOffset < SizeOf(Pointer) then
  begin
{$IFDEF FPC}
    LFillPattern := NativeUInt(DebugFillPattern);
{$ELSE}
    LFillPattern := NativeUInt(@FreedObjectVMT.VMTMethods[0]);
{$ENDIF}
  end
  else
  begin
{$IFNDEF CatchUseOfFreedInterfaces}
    LFillPattern := NativeUInt(DebugFillPattern);
{$ELSE}
    LFillPattern := NativeUInt(@VMTBadInterface);
{$ENDIF}
  end;
  {Compare the byte value}
  Result := Byte(PByte(PByte(APointer) + SizeOf(TFullDebugBlockHeader) + AUserOffset)^) <>
    Byte(RotateRight(LFillPattern, (AUserOffset and (SizeOf(Pointer) - 1)) * 8));
end;

function LogBlockChanges(APointer: PFullDebugBlockHeader; ABuffer: PAnsiChar; ABufSize: Cardinal): PAnsiChar;
const
  CMaxLogChanges = 32; {Log a maximum of 32 changes}
var
  LOffset, LChangeStart, LCount: NativeUInt;
  LLogCount: Integer;
  LBuffer: PAnsiChar;
begin
  LBuffer := ABuffer;
  {No errors logged so far}
  LLogCount := 0;
  LOffset := 0;
  while (LOffset < APointer^.UserSize) and (LLogCount < CMaxLogChanges) do
  begin
    {Has the byte been modified?}
    if FreeBlockByteWasModified(APointer, LOffset) then
    begin
      {Found the start of a changed block, now find the length}
      LChangeStart := LOffset;
      LCount := 0;
      while True do
      begin
        Inc(LCount);
        Inc(LOffset);
        if (LOffset >= APointer^.UserSize)
          or (not FreeBlockByteWasModified(APointer, LOffset)) then
        begin
          Break;
        end;
      end;
      {Got the offset and length, now log it.}
      if LLogCount = 0 then
      begin
        LBuffer := AppendStringToBuffer(FreeModifiedDetailMsg, LBuffer, Length(FreeModifiedDetailMsg), ABufSize{todo: Implement ABufSize checking and in this function});
      end
      else
      begin
        LBuffer^ := ',';
        Inc(LBuffer);{todo: implement buffer size checking}
        LBuffer^ := ' ';
        Inc(LBuffer);{todo: ibidem}
      end;
      LBuffer := NativeUIntToStrBuf(LChangeStart, LBuffer, ABufSize{todo: ibidem});
      LBuffer^ := '(';
      Inc(LBuffer);
      LBuffer := NativeUIntToStrBuf(LCount, LBuffer, ABufSize{todo: ibidem});
      LBuffer^ := ')';
      Inc(LBuffer);
      {Increment the log count}
      Inc(LLogCount);
    end;
    {Next byte}
    Inc(LOffset);
  end;
  {Return the current buffer position}
  Result := LBuffer;
end;

function LogStackTraceSafe(AReturnAddresses: PNativeUInt; AMaxDepth: Cardinal; ABuffer: PAnsiChar; ADestinationBufferLengthChars: Cardinal): PAnsiChar;
begin
  {todo: implement the ADestinationBufferLengthChars chandling}
  Result := LogStackTrace(AReturnAddresses, AMaxDepth, ABuffer);
end;


procedure LogBlockError(APointer: PFullDebugBlockHeader; AOperation: TBlockOperation; LHeaderValid, LFooterValid: Boolean);
var
  LInitialPtr, LMsgPtr: PAnsiChar;
  LErrorMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
{$IFNDEF NoMessageBoxes}
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
  LClass: TClass;
  {$IFDEF CheckCppObjectTypeEnabled}
  LCppObjectTypeName: PAnsiChar;
  {$ENDIF}
  LInitialSize, Left: Cardinal;
begin
  {Display the error header and the operation type.}
  LMsgPtr := @(LErrorMessage[0]);
  LInitialPtr := LMsgPtr;
  LInitialSize := (SizeOf(LErrorMessage) div SizeOf(LErrorMessage[0]))-1;
  LMsgPtr := AppendStringToBuffer(ErrorMsgHeader, LMsgPtr, Length(ErrorMsgHeader), LInitialSize);
  Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
  case AOperation of
    boGetMem: LMsgPtr := AppendStringToBuffer(GetMemMsg, LMsgPtr, Length(GetMemMsg), Left);
    boFreeMem: LMsgPtr := AppendStringToBuffer(FreeMemMsg, LMsgPtr, Length(FreeMemMsg), Left);
    boReallocMem: LMsgPtr := AppendStringToBuffer(ReallocMemMsg, LMsgPtr, Length(ReallocMemMsg), Left);
    boBlockCheck: LMsgPtr := AppendStringToBuffer(BlockCheckMsg, LMsgPtr, Length(BlockCheckMsg), Left);
  end;
  Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
  LMsgPtr := AppendStringToBuffer(OperationMsg, LMsgPtr, Length(OperationMsg), Left);
  {Is the header still intact?}
  if LHeaderValid then
  begin
    {Is the footer still valid?}
    if LFooterValid then
    begin
      {A freed block has been modified, a double free has occurred, or an
       attempt was made to free a memory block allocated by a different
       instance of FastMM.}
      if AOperation <= boGetMem then
      begin
        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
        LMsgPtr := AppendStringToBuffer(FreeModifiedErrorMsg, LMsgPtr, Length(FreeModifiedErrorMsg), Left);

        {Log the exact changes that caused the error.}
        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
        LMsgPtr := LogBlockChanges(APointer, LMsgPtr, Left);
      end
      else
      begin
        {It is either a double free, or an attempt was made to free a block
         that was allocated via a different memory manager.}
        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);

        if APointer^.AllocatedByRoutine = nil then
          LMsgPtr := AppendStringToBuffer(DoubleFreeErrorMsg, LMsgPtr, Length(DoubleFreeErrorMsg), Left)
        else
          LMsgPtr := AppendStringToBuffer(WrongMMFreeErrorMsg, LMsgPtr, Length(WrongMMFreeErrorMsg), Left);
      end;
    end
    else
    begin
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(BlockFooterCorruptedMsg, LMsgPtr, Length(BlockFooterCorruptedMsg), Left)
    end;
    {Set the block size message}
    Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
    if AOperation <= boGetMem then
      LMsgPtr := AppendStringToBuffer(PreviousBlockSizeMsg, LMsgPtr, Length(PreviousBlockSizeMsg), Left)
    else
      LMsgPtr := AppendStringToBuffer(CurrentBlockSizeMsg, LMsgPtr, Length(CurrentBlockSizeMsg), Left);

    Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
    LMsgPtr := NativeUIntToStrBuf(APointer^.UserSize, LMsgPtr, Left);
    {The header is still intact - display info about the this/previous allocation}
    if APointer^.AllocationStackTrace[0] <> 0 then
    begin
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      if AOperation <= boGetMem then
        LMsgPtr := AppendStringToBuffer(ThreadIDPrevAllocMsg, LMsgPtr, Length(ThreadIDPrevAllocMsg), Left)
      else
        LMsgPtr := AppendStringToBuffer(ThreadIDAtAllocMsg, LMsgPtr, Length(ThreadIDAtAllocMsg), Left);

      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := NativeUIntToHexBuf(APointer^.AllocatedByThread, LMsgPtr, Left);

      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg), Left);


      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := LogStackTraceSafe(@(APointer^.AllocationStackTrace[0]), StackTraceDepth, LMsgPtr, Left);
    end;
    {Get the class this block was used for previously}
    LClass := DetectClassInstance(@(APointer^.PreviouslyUsedByClass));
    {$IFNDEF FPC}
    if (LClass <> nil) and (IntPtr(LClass) <> IntPtr(@FreedObjectVMT.VMTMethods[0])) then
    begin
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(PreviousObjectClassMsg, LMsgPtr, Length(PreviousObjectClassMsg), Left);

      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr, Left);
    end;
    {$ENDIF}
    {$IFDEF CheckCppObjectTypeEnabled}
    if (LClass = nil) and Assigned(GetCppVirtObjTypeNameByVTablePtrFunc) then
    begin
      LCppObjectTypeName := GetCppVirtObjTypeNameByVTablePtrFunc(Pointer(APointer.PreviouslyUsedByClass), 0);
      if Assigned(LCppObjectTypeName) then
      begin
        LMsgPtr := AppendStringToBuffer(PreviousObjectClassMsg, LMsgPtr, Length(PreviousObjectClassMsg));
        LMsgPtr := AppendStringToBuffer(LCppObjectTypeName, LMsgPtr, StrLen(LCppObjectTypeName));
      end;
    end;
    {$ENDIF}
    {Get the current class for this block}
    if (AOperation > boGetMem) and (APointer^.AllocatedByRoutine <> nil) then
    begin
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(CurrentObjectClassMsg, LMsgPtr, Length(CurrentObjectClassMsg), Left);
      LClass := DetectClassInstance(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)));
{$IFNDEF FPC}
      if IntPtr(LClass) = IntPtr(@FreedObjectVMT.VMTMethods[0]) then
{$ENDIF}
        LClass := nil;
      {$IFNDEF CheckCppObjectTypeEnabled}
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr, Left);
      {$ELSE}
      if (LClass = nil) and Assigned(GetCppVirtObjTypeNameFunc) then
      begin
        LCppObjectTypeName := GetCppVirtObjTypeNameFunc(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)),
          APointer.UserSize);
        if LCppObjectTypeName <> nil then
          LMsgPtr := AppendStringToBuffer(LCppObjectTypeName, LMsgPtr, StrLen(LCppObjectTypeName))
        else
          LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
      end
      else
      begin
        LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
      end;
      {$ENDIF}
      {Log the allocation group}
      if APointer^.AllocationGroup > 0 then
      begin
        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
        LMsgPtr := AppendStringToBuffer(CurrentAllocationGroupMsg, LMsgPtr, Length(CurrentAllocationGroupMsg), Left);

        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
        LMsgPtr := NativeUIntToStrBuf(APointer^.AllocationGroup, LMsgPtr, Left);
      end;
      {Log the allocation number}
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(CurrentAllocationNumberMsg, LMsgPtr, Length(CurrentAllocationNumberMsg), Left);

      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := NativeUIntToStrBuf(APointer^.AllocationNumber, LMsgPtr, Left);
    end
    else
    begin
      {Log the allocation group}
      if APointer^.AllocationGroup > 0 then
      begin
        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
        LMsgPtr := AppendStringToBuffer(PreviousAllocationGroupMsg, LMsgPtr, Length(PreviousAllocationGroupMsg), Left);

        Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
        LMsgPtr := NativeUIntToStrBuf(APointer^.AllocationGroup, LMsgPtr, Left);
      end;
      {Log the allocation number}
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(PreviousAllocationNumberMsg, LMsgPtr, Length(PreviousAllocationNumberMsg), Left);

      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := NativeUIntToStrBuf(APointer^.AllocationNumber, LMsgPtr, Left);
    end;
    {Get the call stack for the previous free}
    if APointer^.FreeStackTrace[0] <> 0 then
    begin
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(ThreadIDAtFreeMsg, LMsgPtr, Length(ThreadIDAtFreeMsg), Left);
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := NativeUIntToHexBuf(APointer^.FreedByThread, LMsgPtr, Left);
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg), Left);
      Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
      LMsgPtr := LogStackTraceSafe(@APointer^.FreeStackTrace[0], StackTraceDepth, LMsgPtr, Left);
    end;
  end
  else
  begin
    {Header has been corrupted}
    Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedMsg, LMsgPtr, Length(BlockHeaderCorruptedMsg), Left);
  end;
  {Add the current stack trace}
  Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
  LMsgPtr := LogCurrentThreadAndStackTrace(3 + Ord(AOperation <> boGetMem) + Ord(AOperation = boReallocMem), LMsgPtr, Left);
{$IFNDEF DisableLoggingOfMemoryDumps}
  {Add the memory dump}
  Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr);
  LMsgPtr := LogMemoryDump(APointer, LMsgPtr, Left);
{$ENDIF}

  {Trailing CRLF}
  if Left > 2 then
  begin
    LMsgPtr^ := #13;
    Inc(LMsgPtr);
    LMsgPtr^ := #10;
    Inc(LMsgPtr);
  end;
  // Left := LInitialSize-NativeUint(LMsgPtr-LInitialPtr); {this assignment produces a compiler "hint", but might have been useful for further development}

  {Trailing #0}
  LMsgPtr^ := #0;
{$IFDEF LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
  {$ENDIF}
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(LErrorMessage);
{$ENDIF}
  {Show the message}
{$IFNDEF NoMessageBoxes}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(BlockErrorMsgTitle, LErrorMessageTitle, Length(BlockErrorMsgTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
  ShowMessageBox(LErrorMessage, LErrorMessageTitle);
{$ENDIF}
end;

{Logs the stack traces for a memory leak to file}
procedure LogMemoryLeakOrAllocatedBlock(APointer: PFullDebugBlockHeader; IsALeak: Boolean);
var
  LHeaderValid: Boolean;
  LInitialPtr, LMsgPtr: PAnsiChar;
  LErrorMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
  LClass: TClass;
  {$IFDEF CheckCppObjectTypeEnabled}
  LCppObjectTypeName: PAnsiChar;
  {$ENDIF}
  LInitialSize: Cardinal;
begin
  {Display the error header and the operation type.}

  LMsgPtr := @LErrorMessage[0];
  LInitialPtr := LMsgPtr;
  LInitialSize := (SizeOf(LErrorMessage) div SizeOf(LErrorMessage[0]))-1;

  if IsALeak then
    LMsgPtr := AppendStringToBuffer(LeakLogHeader, LMsgPtr, Length(LeakLogHeader), LInitialSize-NativeUint(LMsgPtr-LInitialPtr))
  else
    LMsgPtr := AppendStringToBuffer(BlockScanLogHeader, LMsgPtr, Length(BlockScanLogHeader), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
  LMsgPtr := NativeUIntToStrBuf(GetAvailableSpaceInBlock(APointer) - FullDebugBlockOverhead, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
  {Is the debug info surrounding the block valid?}
  LHeaderValid := CalculateHeaderCheckSum(APointer) = APointer^.HeaderCheckSum;
  {Is the header still intact?}
  if LHeaderValid then
  begin
    {The header is still intact - display info about this/previous allocation}
    if APointer^.AllocationStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtAllocMsg, LMsgPtr, Length(ThreadIDAtAllocMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToHexBuf(APointer^.AllocatedByThread, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := LogStackTrace(@(APointer^.AllocationStackTrace[0]), StackTraceDepth, LMsgPtr {, LInitialSize-NativeUint(LMsgPtr-LInitialPtr)}{todo: Implement});
    end;
    LMsgPtr := AppendStringToBuffer(CurrentObjectClassMsg, LMsgPtr, Length(CurrentObjectClassMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    {Get the current class for this block}
    LClass := DetectClassInstance(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)));
{$IFNDEF FPC}
    if IntPtr(LClass) = IntPtr(@FreedObjectVMT.VMTMethods[0]) then
{$ENDIF}
      LClass := nil;
    {$IFNDEF CheckCppObjectTypeEnabled}
    if LClass <> nil then
    begin
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end
    else
    begin
      case DetectStringData(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)), APointer^.UserSize) of
        stUnknown: LMsgPtr := AppendClassNameToBuffer(nil, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
        stAnsiString: LMsgPtr := AppendStringToBuffer(AnsiStringBlockMessage, LMsgPtr, Length(AnsiStringBlockMessage), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
        stUnicodeString: LMsgPtr := AppendStringToBuffer(UnicodeStringBlockMessage, LMsgPtr, Length(UnicodeStringBlockMessage), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      end;
    end;
    {$ELSE}
    if (LClass = nil) and Assigned(GetCppVirtObjTypeNameFunc) then
    begin
      LCppObjectTypeName := GetCppVirtObjTypeNameFunc(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)),
        APointer.UserSize);
      if LCppObjectTypeName <> nil then
        LMsgPtr := AppendStringToBuffer(LCppObjectTypeName, LMsgPtr, StrLen(LCppObjectTypeName))
      else
      begin
        case DetectStringData(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)), APointer.UserSize) of
          stUnknown: LMsgPtr := AppendClassNameToBuffer(nil, LMsgPtr);
          stAnsiString: LMsgPtr := AppendStringToBuffer(AnsiStringBlockMessage, LMsgPtr, Length(AnsiStringBlockMessage));
          stUnicodeString: LMsgPtr := AppendStringToBuffer(UnicodeStringBlockMessage, LMsgPtr, Length(UnicodeStringBlockMessage));
        end;
      end;
    end
    else
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
    {$ENDIF}
    {Log the allocation group}
    if APointer^.AllocationGroup > 0 then
    begin
      LMsgPtr := AppendStringToBuffer(CurrentAllocationGroupMsg, LMsgPtr, Length(CurrentAllocationGroupMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToStrBuf(APointer^.AllocationGroup, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end;
    {Log the allocation number}
    LMsgPtr := AppendStringToBuffer(CurrentAllocationNumberMsg, LMsgPtr, Length(CurrentAllocationNumberMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr := NativeUIntToStrBuf(APointer^.AllocationNumber, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
  end
  else
  begin
    {Header has been corrupted}
    if LInitialSize-NativeUint(LMsgPtr-LInitialPtr) > 3 then
    begin
      LMsgPtr^ := '.';
      Inc(LMsgPtr);
      LMsgPtr^ := ' ';
      Inc(LMsgPtr);
      LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedMsg, LMsgPtr, Length(BlockHeaderCorruptedMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end;
  end;
{$IFNDEF DisableLoggingOfMemoryDumps}
  {Add the memory dump}
  LMsgPtr := LogMemoryDump(APointer, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
{$ENDIF}
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
end;

{Checks that a free block is unmodified}
function CheckFreeBlockUnmodified(APBlock: PFullDebugBlockHeader; ABlockSize: NativeUInt;
  AOperation: TBlockOperation): Boolean;
var
  LHeaderCheckSum: NativeUInt;
  LHeaderValid, LFooterValid, LBlockUnmodified: Boolean;
begin
  LHeaderCheckSum := CalculateHeaderCheckSum(APBlock);
  LHeaderValid := LHeaderCheckSum = APBlock^.HeaderCheckSum;
  {Is the footer itself still in place}
  LFooterValid := LHeaderValid
    and (PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + APBlock^.UserSize)^ = (not LHeaderCheckSum));
  {Is the footer and debug VMT in place? The debug VMT is only valid if the user size is greater than the size of a pointer.}
{$IFNDEF FPC}
  if LFooterValid
    and (APBlock.UserSize < SizeOf(Pointer)) or (PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader))^ = NativeUInt(@FreedObjectVMT.VMTMethods[0])) then
  begin
    {Store the debug fill pattern in place of the footer in order to simplify
     checking for block modifications.}
    PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + APBlock.UserSize)^ :=
    {$IFNDEF CatchUseOfFreedInterfaces}
      DebugFillPattern;
    {$ELSE}
      RotateRight(NativeUInt(@VMTBadInterface), (APBlock.UserSize and (SizeOf(Pointer) - 1)) * 8);
    {$ENDIF}
    {Check that all the filler bytes are valid inside the block, except for
     the "dummy" class header}
    LBlockUnmodified := CheckFillPattern(PNativeUInt(PByte(APBlock) + (SizeOf(TFullDebugBlockHeader) + SizeOf(Pointer))),
      ABlockSize - (FullDebugBlockOverhead + SizeOf(Pointer)),
      {$IFNDEF CatchUseOfFreedInterfaces}DebugFillPattern{$ELSE}NativeUInt(@VMTBadInterface){$ENDIF});
    {Reset the old footer}
    PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + APBlock.UserSize)^ := not LHeaderCheckSum;
  end
  else
{$ENDIF}
    LBlockUnmodified := False;
  if (not LHeaderValid) or (not LFooterValid) or (not LBlockUnmodified) then
  begin
    LogBlockError(APBlock, AOperation, LHeaderValid, LFooterValid);
    Result := False;
  end
  else
    Result := True;
end;

function DebugGetMem(ASize: {$IFDEF FPC}ptruint{$ELSE}{$IFDEF XE2AndUp}NativeInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
{$IFDEF LogLockContention}
var
  LCollector: PStaticCollector;
  LStackTrace: TStackTrace;
{$ENDIF}
begin
  {Scan the entire memory pool first?}
  if FullDebugModeScanMemoryPoolBeforeEveryOperation then
    ScanMemoryPoolForCorruptions;
  {Enter the memory manager: block scans may not be performed now}
  StartChangingFullDebugModeBlock;
  try
    {We need extra space for (a) The debug header, (b) the block debug trailer
     and (c) the trailing block size pointer for free blocks}
    Result := FastGetMem(ASize + FullDebugBlockOverhead {$IFDEF LogLockContention}, LCollector{$ENDIF});
    if Result <> nil then
    begin
      {Large blocks are always newly allocated (and never reused), so checking
       for a modify-after-free is not necessary.}
      if (ASize > (MaximumMediumBlockSize - BlockHeaderSize - FullDebugBlockOverhead))
        or CheckFreeBlockUnmodified(Result, GetAvailableSpaceInBlock(Result) + BlockHeaderSize, boGetMem) then
      begin
        {Set the allocation call stack}
        GetStackTrace(@(PFullDebugBlockHeader(Result)^.AllocationStackTrace[0]), StackTraceDepth, 1);
{$IFDEF LogLockContention}
        if assigned(LCollector) then
          LCollector.Add(@PFullDebugBlockHeader(Result).AllocationStackTrace[0], StackTraceDepth);
{$ENDIF LogLockContention}
        {Set the thread ID of the thread that allocated the block}
        PFullDebugBlockHeader(Result)^.AllocatedByThread := GetThreadID;
        {Block is now in use: It was allocated by this routine}
        PFullDebugBlockHeader(Result)^.AllocatedByRoutine := @DebugGetMem;
        {Set the group number}
        PFullDebugBlockHeader(Result)^.AllocationGroup := AllocationGroupStack[AllocationGroupStackTop];
        {Set the allocation number}
        IncrementAllocationNumber;
        PFullDebugBlockHeader(Result)^.AllocationNumber := CurrentAllocationNumber;
        {Clear the previous block trailer}
        PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(Result)^.UserSize)^ :=
        {$IFNDEF CatchUseOfFreedInterfaces}
          NativeUInt(DebugFillPattern);
        {$ELSE}
          RotateRight(NativeUInt(@VMTBadInterface), (PFullDebugBlockHeader(Result).UserSize and (SizeOf(Pointer) - 1)) * 8);
        {$ENDIF}
        {Set the user size for the block}
        PFullDebugBlockHeader(Result)^.UserSize := ASize;
        {Set the checksums}
        UpdateHeaderAndFooterCheckSums(Result);
        {$IFDEF FullDebugModeCallBacks}
        if Assigned(OnDebugGetMemFinish) then
          OnDebugGetMemFinish(PFullDebugBlockHeader(Result), ASize);
        {$ENDIF}
        {Return the start of the actual block}
        Result := Pointer(PByte(Result) + SizeOf(TFullDebugBlockHeader));
{$IFDEF EnableMemoryLeakReporting}
        {Should this block be marked as an expected leak automatically?}
        if FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak then
          RegisterExpectedMemoryLeak(Result);
{$ENDIF}
      end
      else
      begin
{$IFDEF LogLockContention}
        if assigned(LCollector) then
        begin
          GetStackTrace(@LStackTrace, StackTraceDepth, 1);
          LCollector.Add(@(LStackTrace[0]), StackTraceDepth);
        end;
{$ENDIF LogLockContention}
        Result := nil;
      end;
    end
    else
    begin
      {The process ran out of address space:  Release the address space slack so that some subsequent GetMem calls will
      succeed in order for any error logging, etc. to complete successfully.}
      if AddressSpaceSlackPtr <> nil then
      begin
        VirtualFree(AddressSpaceSlackPtr, 0, MEM_RELEASE);
        AddressSpaceSlackPtr := nil;
      end;
    end;
  finally
    {Leaving the memory manager routine: Block scans may be performed again.}
    DoneChangingFullDebugModeBlock;
  end;
end;

function CheckBlockBeforeFreeOrRealloc(APBlock: PFullDebugBlockHeader;
  AOperation: TBlockOperation): Boolean;
var
  LHeaderValid, LFooterValid: Boolean;
  LPFooter: PNativeUInt;
{$IFNDEF CatchUseOfFreedInterfaces}
  LBlockSize: NativeUInt;
  LPTrailingByte, LPFillPatternEnd: PByte;
{$ENDIF}
begin
  {Is the checksum for the block header valid?}
  LHeaderValid := CalculateHeaderCheckSum(APBlock) = APBlock^.HeaderCheckSum;
  {If the header is corrupted then the footer is assumed to be corrupt too.}
  if LHeaderValid then
  begin
    {Check the footer checksum: The footer checksum should equal the header
     checksum with all bits inverted.}
    LPFooter := PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(APBlock)^.UserSize);
    if APBlock^.HeaderCheckSum = (not (LPFooter^)) then
    begin
      LFooterValid := True;
{$IFNDEF CatchUseOfFreedInterfaces}
      {Large blocks do not have the debug fill pattern, since they are never reused.}
      if PNativeUInt(PByte(APBlock) - BlockHeaderSize)^ and (IsMediumBlockFlag or IsLargeBlockFlag) <> IsLargeBlockFlag then
      begin
        {Check that the application has not modified bytes beyond the block
         footer. The $80 fill pattern should extend up to 2 nativeints before
         the start of the next block (leaving space for the free block size and
         next block header.)}
        LBlockSize := GetAvailableSpaceInBlock(APBlock);
        LPFillPatternEnd := PByte(PByte(APBlock) + LBlockSize - SizeOf(Pointer));
        LPTrailingByte := PByte(PByte(LPFooter) + SizeOf(NativeUInt));
        while UIntPtr(LPTrailingByte) < UIntPtr(LPFillPatternEnd) do
        begin
          if Byte(LPTrailingByte^) <> DebugFillByte then
          begin
            LFooterValid := False;
            Break;
          end;
          Inc(LPTrailingByte);
        end;
      end;
{$ENDIF}
    end
    else
      LFooterValid := False;
  end
  else
    LFooterValid := False;
  {The header and footer must be intact and the block must have been allocated
   by this memory manager instance.}
  if LFooterValid and (
  APBlock^.AllocatedByRoutine =
  {$IFDEF fpc}
  Pointer(@DebugGetMem)
  {$ELSE}
  @DebugGetMem
  {$ENDIF}
  ) then
  begin
    Result := True;
  end
  else
  begin
    {Log the error}
    LogBlockError(APBlock, AOperation, LHeaderValid, LFooterValid);
    {Return an error}
    Result := False;
  end;
end;

function DebugFreeMem(APointer: Pointer): {$IFDEF fpc}ptruint{$ELSE}Integer{$ENDIF};
var
  LActualBlock: PFullDebugBlockHeader;
  LBlockHeader: NativeUInt;
begin
  {Scan the entire memory pool first?}
  if FullDebugModeScanMemoryPoolBeforeEveryOperation then
    ScanMemoryPoolForCorruptions;
  {Get a pointer to the start of the actual block}
  LActualBlock := PFullDebugBlockHeader(PByte(APointer)
    - SizeOf(TFullDebugBlockHeader));
  {Is the debug info surrounding the block valid?}
  if CheckBlockBeforeFreeOrRealloc(LActualBlock, boFreeMem) then
  begin
    {Enter the memory manager: block scans may not be performed now}
    StartChangingFullDebugModeBlock;
    try
      {$IFDEF FullDebugModeCallBacks}
      if Assigned(OnDebugFreeMemStart) then
        OnDebugFreeMemStart(LActualBlock);
      {$ENDIF}
      {Large blocks are never reused, so there is no point in updating their
       headers and fill pattern.}
      LBlockHeader := PNativeUInt(PByte(LActualBlock) - BlockHeaderSize)^;
      if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag) <> IsLargeBlockFlag then
      begin
        {Get the class the block was used for}
        LActualBlock^.PreviouslyUsedByClass := PNativeUInt(APointer)^;
        {Set the free call stack}
        GetStackTrace(@LActualBlock^.FreeStackTrace[0], StackTraceDepth, 1);
        {Set the thread ID of the thread that freed the block}
        LActualBlock^.FreedByThread := GetThreadID;
        {Block is now free}
        LActualBlock^.AllocatedByRoutine := nil;
        {Clear the user area of the block}
        DebugFillMem(APointer^, LActualBlock^.UserSize,
          {$IFNDEF CatchUseOfFreedInterfaces}NativeUInt(DebugFillPattern){$ELSE}NativeUInt(@VMTBadInterface){$ENDIF});
{$IFNDEF FPC}
        {Set a pointer to the dummy VMT}
        PNativeUInt(APointer)^ := NativeUInt(@FreedObjectVMT.VMTMethods[0]);
{$ELSE}
        PNativeUInt(APointer)^ := 0;
{$ENDIF}
        {Recalculate the checksums}
        UpdateHeaderAndFooterCheckSums(LActualBlock);
      end;
{$IFDEF EnableMemoryLeakReporting}
      {Automatically deregister the expected memory leak?}
      if FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak then
        UnregisterExpectedMemoryLeak(APointer);
{$ENDIF}
      {Free the actual block}
      Result := FastFreeMem(LActualBlock);
      {$IFDEF FullDebugModeCallBacks}
      if Assigned(OnDebugFreeMemFinish) then
        OnDebugFreeMemFinish(LActualBlock, Result);
      {$ENDIF}
    finally
      {Leaving the memory manager routine: Block scans may be performed again.}
      DoneChangingFullDebugModeBlock;
    end;
  end
  else
  begin
{$IFDEF SuppressFreeMemErrorsInsideException}
    if {$IFDEF BDS2006AndUp}ExceptObject{$ELSE}RaiseList{$ENDIF} <> nil then
      Result := 0
    else
{$ENDIF}
      Result := {$IFDEF FPC}ptruint(-1){$ELSE}-1{$ENDIF};
  end;
end;

function DebugReallocMem({$IFDEF FPC}var {$ENDIF}APointer: Pointer; ANewSize: {$IFDEF FPC}ptruint{$ELSE}{$IFDEF XE2AndUp}NativeInt{$ELSE}Integer{$ENDIF}{$ENDIF}): Pointer;
var
  LMoveSize, LBlockSpace: NativeUInt;
  LActualBlock, LNewActualBlock: PFullDebugBlockHeader;
begin
  {Scan the entire memory pool first?}
  if FullDebugModeScanMemoryPoolBeforeEveryOperation then
    ScanMemoryPoolForCorruptions;
  {Get a pointer to the start of the actual block}
  LActualBlock := PFullDebugBlockHeader(PByte(APointer)
    - SizeOf(TFullDebugBlockHeader));
  {Is the debug info surrounding the block valid?}
  if CheckBlockBeforeFreeOrRealloc(LActualBlock, boReallocMem) then
  begin
    {Get the current block size}
    LBlockSpace := GetAvailableSpaceInBlock(LActualBlock);
    {Can the block fit? We need space for the debug overhead and the block header
     of the next block}
    if LBlockSpace < (NativeUInt(ANewSize) + FullDebugBlockOverhead) then
    begin
      {Get a new block of the requested size.}
      Result := DebugGetMem(ANewSize);
      if Result <> nil then
      begin
        {Block scans may not be performed now}
        StartChangingFullDebugModeBlock;
        try
          {$IFDEF FullDebugModeCallBacks}
          if Assigned(OnDebugReallocMemStart) then
            OnDebugReallocMemStart(LActualBlock, ANewSize);
          {$ENDIF}
          {We reuse the old allocation number. Since DebugGetMem always bumps
           CurrentAllocationGroup, there may be gaps in the sequence of
           allocation numbers.}
          LNewActualBlock := PFullDebugBlockHeader(PByte(Result)
            - SizeOf(TFullDebugBlockHeader));
          LNewActualBlock^.AllocationGroup := LActualBlock^.AllocationGroup;
          LNewActualBlock^.AllocationNumber := LActualBlock^.AllocationNumber;
          {Recalculate the header and footer checksums}
          UpdateHeaderAndFooterCheckSums(LNewActualBlock);
          {$IFDEF FullDebugModeCallBacks}
          if Assigned(OnDebugReallocMemFinish) then
            OnDebugReallocMemFinish(LNewActualBlock, ANewSize);
          {$ENDIF}
        finally
          {Block scans can again be performed safely}
          DoneChangingFullDebugModeBlock;
        end;
        {How many bytes to move?}
        LMoveSize := LActualBlock^.UserSize;
        if LMoveSize > NativeUInt(ANewSize) then
          LMoveSize := ANewSize;
        {Move the data across}
        System.Move(APointer^, Result^, LMoveSize);
        {Free the old block}
        DebugFreeMem(APointer);
      end
      else
      begin
        Result := nil;
      end;
    end
    else
    begin
      {Block scans may not be performed now}
      StartChangingFullDebugModeBlock;
      try
        {$IFDEF FullDebugModeCallBacks}
        if Assigned(OnDebugReallocMemStart) then
          OnDebugReallocMemStart(LActualBlock, ANewSize);
        {$ENDIF}
        {Clear all data after the new end of the block up to the old end of the
         block, including the trailer.}
        DebugFillMem(Pointer(PByte(APointer) + NativeUInt(ANewSize) + SizeOf(NativeUInt))^,
          NativeInt(LActualBlock^.UserSize) - ANewSize,
{$IFNDEF CatchUseOfFreedInterfaces}
          NativeUInt(DebugFillPattern));
{$ELSE}
          RotateRight(NativeUInt(@VMTBadInterface), (ANewSize and (SizeOf(Pointer) - 1)) * 8));
{$ENDIF}
        {Update the user size}
        LActualBlock^.UserSize := ANewSize;
        {Set the new checksums}
        UpdateHeaderAndFooterCheckSums(LActualBlock);
        {$IFDEF FullDebugModeCallBacks}
        if Assigned(OnDebugReallocMemFinish) then
          OnDebugReallocMemFinish(LActualBlock, ANewSize);
        {$ENDIF}
      finally
        {Block scans can again be performed safely}
        DoneChangingFullDebugModeBlock;
      end;
      {Return the old pointer}
      Result := APointer;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

{Allocates a block and fills it with zeroes}
function DebugAllocMem(ASize: {$IFDEF XE2AndUp}NativeInt{$ELSE}Cardinal{$ENDIF}): Pointer;
begin
  Result := DebugGetMem(ASize);
  {Clear the block}
  if Result <>  nil then
    FillChar(Result^, ASize, 0);
end;

{Raises a runtime error if a memory corruption was encountered. Subroutine for
 InternalScanMemoryPool and InternalScanSmallBlockPool.}
procedure RaiseMemoryCorruptionError;
begin
  {Disable exhaustive checking in order to prevent recursive exceptions.}
  FullDebugModeScanMemoryPoolBeforeEveryOperation := False;
  {Unblock the memory manager in case the creation of the exception below
   causes an attempt to be made to allocate memory.}
  UnblockFullDebugModeMMRoutines;
  {Raise the runtime error}
{$IFDEF BCB6OrDelphi7AndUp}
  System.Error(reOutOfMemory);
{$ELSE}
  System.RunError(reOutOfMemory);
{$ENDIF}
end;

{Subroutine for InternalScanMemoryPool: Checks the given small block pool for
 allocated blocks}
procedure InternalScanSmallBlockPool(APSmallBlockPool: PSmallBlockPoolHeader;
  AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
var
  LCurPtr, LEndPtr: Pointer;
begin
  {Get the first and last pointer for the pool}
  GetFirstAndLastSmallBlockInPool(APSmallBlockPool, LCurPtr, LEndPtr);
  {Step through all blocks}
  while UIntPtr(LCurPtr) <= UIntPtr(LEndPtr) do
  begin
    {Is this block in use? If so, is the debug info intact?}
    if ((PNativeUInt(PByte(LCurPtr) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0) then
    begin
      if CheckBlockBeforeFreeOrRealloc(LCurPtr, boBlockCheck) then
      begin
        if (PFullDebugBlockHeader(LCurPtr)^.AllocationGroup >= AFirstAllocationGroupToLog)
          and (PFullDebugBlockHeader(LCurPtr)^.AllocationGroup <= ALastAllocationGroupToLog) then
        begin
          LogMemoryLeakOrAllocatedBlock(LCurPtr, False);
        end;
      end
      else
        RaiseMemoryCorruptionError;
    end
    else
    begin
      {Check that the block has not been modified since being freed}
      if not CheckFreeBlockUnmodified(LCurPtr, APSmallBlockPool^.BlockType^.BlockSize, boBlockCheck) then
        RaiseMemoryCorruptionError;
    end;
    {Next block}
    Inc(PByte(LCurPtr), APSmallBlockPool^.BlockType^.BlockSize);
  end;
end;

{Subroutine for LogAllocatedBlocksToFile and ScanMemoryPoolForCorruptions:
 Scans the memory pool for corruptions and optionally logs allocated blocks
 in the allocation group range.}
procedure InternalScanMemoryPool(AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
var
  LPLargeBlock: PLargeBlockHeader;
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: NativeUInt;
begin
  {Block all the memory manager routines while performing the scan. No memory
   block may be allocated or freed, and no FullDebugMode block header or
   footer may be modified, while the scan is in progress.}
  BlockFullDebugModeMMRoutines;
  try
    {Step through all the medium block pools}
    LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
    begin
      LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
        {Is the block in use?}
        if LMediumBlockHeader and IsFreeBlockFlag = 0 then
        begin
          {Block is in use: Is it a medium block or small block pool?}
          if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            {Get all the leaks for the small block pool}
            InternalScanSmallBlockPool(LPMediumBlock, AFirstAllocationGroupToLog, ALastAllocationGroupToLog);
          end
          else
          begin
            if CheckBlockBeforeFreeOrRealloc(LPMediumBlock, boBlockCheck) then
            begin
              if (PFullDebugBlockHeader(LPMediumBlock)^.AllocationGroup >= AFirstAllocationGroupToLog)
                and (PFullDebugBlockHeader(LPMediumBlock)^.AllocationGroup <= ALastAllocationGroupToLog) then
              begin
                LogMemoryLeakOrAllocatedBlock(LPMediumBlock, False);
              end;
            end
            else
              RaiseMemoryCorruptionError;
          end;
        end
        else
        begin
          {Check that the block has not been modified since being freed}
          if not CheckFreeBlockUnmodified(LPMediumBlock, LMediumBlockHeader and DropMediumAndLargeFlagsMask, boBlockCheck) then
            RaiseMemoryCorruptionError;
        end;
        {Next medium block}
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;
      {Get the next medium block pool}
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
    end;
    {Scan large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @LargeBlocksCircularList do
    begin
      if CheckBlockBeforeFreeOrRealloc(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), boBlockCheck) then
      begin
        if (PFullDebugBlockHeader(PByte(LPLargeBlock) + LargeBlockHeaderSize)^.AllocationGroup >= AFirstAllocationGroupToLog)
          and (PFullDebugBlockHeader(PByte(LPLargeBlock) + LargeBlockHeaderSize)^.AllocationGroup <= ALastAllocationGroupToLog) then
        begin
          LogMemoryLeakOrAllocatedBlock(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), False);
        end;
      end
      else
        RaiseMemoryCorruptionError;
      {Get the next large block}
      LPLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
    end;
  finally
    {Unblock the FullDebugMode memory manager routines.}
    UnblockFullDebugModeMMRoutines;
  end;
end;

{Logs detail about currently allocated memory blocks for the specified range of
 allocation groups. if ALastAllocationGroupToLog is less than
 AFirstAllocationGroupToLog or it is zero, then all allocation groups are
 logged. This routine also checks the memory pool for consistency at the same
 time, raising an "Out of Memory" error if the check fails.}
procedure LogAllocatedBlocksToFile(AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
var
  LFirstAllocationGroupToLog, LLastAllocationGroupToLog: Cardinal;
begin
  LFirstAllocationGroupToLog := AFirstAllocationGroupToLog;
  LLastAllocationGroupToLog := ALastAllocationGroupToLog;
  {Validate input}
  if (LLastAllocationGroupToLog = 0) or (LLastAllocationGroupToLog < LFirstAllocationGroupToLog) then
  begin
    {Bad input: log all groups}
    LFirstAllocationGroupToLog := 0;
    LLastAllocationGroupToLog := $ffffffff;
  end;
  {Scan the memory pool, logging allocated blocks in the requested range.}
  InternalScanMemoryPool(LFirstAllocationGroupToLog, LLastAllocationGroupToLog);
end;

{Scans the memory pool for any corruptions. If a corruption is encountered an "Out of Memory" exception is
 raised.}
procedure ScanMemoryPoolForCorruptions;
begin
  {Scan the memory pool for corruptions, but don't log any allocated blocks}
  InternalScanMemoryPool($ffffffff, 0);
end;

{-----------------------Invalid Virtual Method Calls-------------------------}

{ TFreedObject }

{Used to determine the index of the virtual method call on the freed object.
 Do not change this without updating MaxFakeVMTEntries. Currently 200.}
procedure TFreedObject.GetVirtualMethodIndex; assembler;
asm
{$IFNDEF FPC}
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
{$ENDIF}
  jmp TFreedObject.VirtualMethodError
end;

procedure TFreedObject.VirtualMethodError;
var
  LVMOffset, LVMIndex: Integer;
  LInitialPtr, LMsgPtr: PAnsiChar;
  LErrorMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
{$IFNDEF NoMessageBoxes}
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
  LClass: TClass;
  LActualBlock: PFullDebugBlockHeader;
  LInitialSize: Cardinal;
  LSelfPtr: Pointer;
begin
  {Get the offset of the virtual method}
  LVMOffset := (MaxFakeVMTEntries - VMIndex) * SizeOf(Pointer) + vmtParent + SizeOf(Pointer);
  {Reset the index for the next error}
  VMIndex := 0;
  {Get the address of the actual block}
  LSelfPtr := @Self;
  LActualBlock := PFullDebugBlockHeader(PByte(LSelfPtr) - SizeOf(TFullDebugBlockHeader));

  LMsgPtr := @LErrorMessage[0];
  LInitialPtr := LMsgPtr;
  LInitialSize := (SizeOf(LErrorMessage) div SizeOf(LErrorMessage[0]))-1;


  {Display the error header}
  LMsgPtr := AppendStringToBuffer(VirtualMethodErrorHeader, @LErrorMessage[0], Length(VirtualMethodErrorHeader), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
  {Is the debug info surrounding the block valid?}
  if CalculateHeaderCheckSum(LActualBlock) = LActualBlock^.HeaderCheckSum then
  begin
    {Get the class this block was used for previously}
    LClass := DetectClassInstance(@(LActualBlock^.PreviouslyUsedByClass));
    {$IFNDEF FPC}
    if (LClass <> nil) and (IntPtr(LClass) <> IntPtr(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LMsgPtr := AppendStringToBuffer(FreedObjectClassMsg, LMsgPtr, Length(FreedObjectClassMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end;
    {$ENDIF}
    {Get the virtual method name}
    LMsgPtr := AppendStringToBuffer(VirtualMethodName, LMsgPtr, Length(VirtualMethodName), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    {$IFNDEF FPC}
    if LVMOffset < 0 then
    begin
      LVMIndex := LVMOffset div SizeOf(Pointer);
      LMsgPtr := AppendStringToBuffer(StandardVirtualMethodNames[LVMIndex], LMsgPtr, Length(StandardVirtualMethodNames[LVMIndex]), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end
    else
    {$ENDIF}
    begin
      LMsgPtr := AppendStringToBuffer(VirtualMethodOffset, LMsgPtr, Length(VirtualMethodOffset), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToStrBuf(LVMOffset, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end;
    {Virtual method address}
    {$IFNDEF FPC}
    if (LClass <> nil) and (IntPtr(LClass) <> IntPtr(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LMsgPtr := AppendStringToBuffer(VirtualMethodAddress, LMsgPtr, Length(VirtualMethodAddress), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToHexBuf(PNativeUInt(PByte(LClass) + LVMOffset)^, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end;
    {$ENDIF}
    {Log the allocation group}
    if LActualBlock^.AllocationGroup > 0 then
    begin
      LMsgPtr := AppendStringToBuffer(PreviousAllocationGroupMsg, LMsgPtr, Length(PreviousAllocationGroupMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToStrBuf(LActualBlock^.AllocationGroup, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    end;
    {Log the allocation number}
    LMsgPtr := AppendStringToBuffer(PreviousAllocationNumberMsg, LMsgPtr, Length(PreviousAllocationNumberMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    LMsgPtr := NativeUIntToStrBuf(LActualBlock^.AllocationNumber, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
    {The header is still intact - display info about the this/previous allocation}
    if LActualBlock^.AllocationStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtObjectAllocMsg, LMsgPtr, Length(ThreadIDAtObjectAllocMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToHexBuf(LActualBlock^.AllocatedByThread, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := LogStackTrace(@LActualBlock^.AllocationStackTrace[0], StackTraceDepth, LMsgPtr);
    end;
    {Get the call stack for the previous free}
    if LActualBlock^.FreeStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtObjectFreeMsg, LMsgPtr, Length(ThreadIDAtObjectFreeMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToHexBuf(LActualBlock^.FreedByThread, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
      LMsgPtr := LogStackTrace(@LActualBlock^.FreeStackTrace[0], StackTraceDepth, LMsgPtr);
    end;
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedNoHistoryMsg, LMsgPtr, Length(BlockHeaderCorruptedNoHistoryMsg), LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
  end;
  {Add the current stack trace}
  LMsgPtr := LogCurrentThreadAndStackTrace(2, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
{$IFNDEF DisableLoggingOfMemoryDumps}
  {Add the pointer address}
  LMsgPtr := LogMemoryDump(LActualBlock, LMsgPtr, LInitialSize-NativeUint(LMsgPtr-LInitialPtr));
{$ENDIF}
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$IFDEF LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
  {$ENDIF}
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(LErrorMessage);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
  {Show the message}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(BlockErrorMsgTitle, LErrorMessageTitle, Length(BlockErrorMsgTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
  ShowMessageBox(LErrorMessage, LErrorMessageTitle);
{$ENDIF}
  {Raise an access violation}
  RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, nil);
end;

{$IFDEF CatchUseOfFreedInterfaces}
procedure TFreedObject.InterfaceError;
var
  LMsgPtr, LInitialPtr: PAnsiChar;
{$IFNDEF NoMessageBoxes}
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
  LErrorMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
  LInitialSize: Cardinal;
begin
  FillChar(LErrorMessage, SizeOf(LErrorMessage), 0);
  {$IFNDEF NoMessageBoxes}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  {$ENDIF}
  LMsgPtr := @LErrorMessage[0];
  LInitialPtr := LMsgPtr;
  LInitialSize := MaxLogMessageLength;
  LMsgPtr := AppendStringToBuffer(InterfaceErrorHeader, LMsgPtr, length(InterfaceErrorHeader), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
  {Add the current stack trace}
  LMsgPtr := LogCurrentThreadAndStackTrace(2, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$IFDEF LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
  {$ENDIF}
{$IFDEF UseOutputDebugString}
  OutputDebugStringA(LErrorMessage);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
  {Show the message}
  FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
  AppendStringToModuleName(BlockErrorMsgTitle, LErrorMessageTitle, Length(BlockErrorMsgTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
  ShowMessageBox(LErrorMessage, LErrorMessageTitle);
{$ENDIF}
  {Raise an access violation}
  RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, nil);
end;
{$ENDIF}

{$ENDIF _EventLog}

{----------------------------Memory Leak Checking-----------------------------}

{$IFDEF EnableMemoryLeakReporting}

{Adds a leak to the specified list}
function UpdateExpectedLeakList(APLeakList: PPExpectedMemoryLeak;
  APNewEntry: PExpectedMemoryLeak; AExactSizeMatch: Boolean = True): Boolean;
var
  LPInsertAfter, LPNewEntry: PExpectedMemoryLeak;
begin
  {Default to error}
  Result := False;
  {Find the insertion spot}
  LPInsertAfter := APLeakList^;
  while LPInsertAfter <> nil do
  begin
    {Too big?}
    if LPInsertAfter^.LeakSize > APNewEntry^.LeakSize then
    begin
      LPInsertAfter := LPInsertAfter^.PreviousLeak;
      Break;
    end;
    {Find a matching entry. If an exact size match is not required and the leak
     is larger than the current entry, use it if the expected size of the next
     entry is too large.}
    if (UIntPtr(LPInsertAfter^.LeakAddress) = UIntPtr(APNewEntry^.LeakAddress))
      and ((UIntPtr(LPInsertAfter^.LeakedClass) = UIntPtr(APNewEntry^.LeakedClass))
      {$IFDEF CheckCppObjectTypeEnabled}
       or (LPInsertAfter^.LeakedCppTypeIdPtr = APNewEntry.LeakedCppTypeIdPtr)
      {$ENDIF}
      )
      and ((LPInsertAfter^.LeakSize = APNewEntry^.LeakSize)
        or ((not AExactSizeMatch)
          and (LPInsertAfter^.LeakSize < APNewEntry^.LeakSize)
          and ((LPInsertAfter^.NextLeak = nil)
            or (LPInsertAfter^.NextLeak^.LeakSize > APNewEntry^.LeakSize))
          )) then
    begin
      if (LPInsertAfter^.LeakCount + APNewEntry^.LeakCount) >= 0 then
      begin
        Inc(LPInsertAfter^.LeakCount, APNewEntry^.LeakCount);
        {Is the count now 0?}
        if LPInsertAfter^.LeakCount = 0 then
        begin
          {Delete the entry}
          if LPInsertAfter^.NextLeak <> nil then
            LPInsertAfter^.NextLeak^.PreviousLeak := LPInsertAfter^.PreviousLeak;
          if LPInsertAfter^.PreviousLeak <> nil then
            LPInsertAfter^.PreviousLeak^.NextLeak := LPInsertAfter^.NextLeak
          else
            APLeakList^ := LPInsertAfter^.NextLeak;
          {Insert it as the first free slot}
          LPInsertAfter^.NextLeak := ExpectedMemoryLeaks^.FirstFreeSlot;
          ExpectedMemoryLeaks^.FirstFreeSlot := LPInsertAfter;
        end;
        Result := True;
      end;
      Exit;
    end;
    {Next entry}
    if LPInsertAfter^.NextLeak <> nil then
      LPInsertAfter := LPInsertAfter^.NextLeak
    else
      Break;
  end;
  if APNewEntry^.LeakCount > 0 then
  begin
    {Get a position for the entry}
    LPNewEntry := ExpectedMemoryLeaks^.FirstFreeSlot;
    if LPNewEntry <> nil then
    begin
      ExpectedMemoryLeaks^.FirstFreeSlot := LPNewEntry^.NextLeak;
    end
    else
    begin
      if ExpectedMemoryLeaks^.EntriesUsed < Length(ExpectedMemoryLeaks^.ExpectedLeaks) then
      begin
        LPNewEntry := @ExpectedMemoryLeaks^.ExpectedLeaks[ExpectedMemoryLeaks^.EntriesUsed];
        Inc(ExpectedMemoryLeaks^.EntriesUsed);
      end
      else
      begin
        {No more space}
        Exit;
      end;
    end;
    {Set the entry}
    LPNewEntry^ := APNewEntry^;
    {Insert it into the list}
    LPNewEntry^.PreviousLeak := LPInsertAfter;
    if LPInsertAfter <> nil then
    begin
      LPNewEntry^.NextLeak := LPInsertAfter^.NextLeak;
      if LPNewEntry^.NextLeak <> nil then
        LPNewEntry^.NextLeak^.PreviousLeak := LPNewEntry;
      LPInsertAfter^.NextLeak := LPNewEntry;
    end
    else
    begin
      LPNewEntry^.NextLeak := APLeakList^;
      if LPNewEntry^.NextLeak <> nil then
        LPNewEntry^.NextLeak^.PreviousLeak := LPNewEntry;
      APLeakList^ := LPNewEntry;
    end;
    Result := True;
  end;
end;

{Locks the expected leaks. Returns false if the list could not be allocated.}
function LockExpectedMemoryLeaksList: Boolean;
begin
  {Lock the expected leaks list}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
    while not AcquireLockByte(ExpectedMemoryLeaksListLocked) do
    begin
{$IFDEF NeverSleepOnThreadContention}
  {$IFDEF UseSwitchToThread}
      SwitchToThreadIfSupported;
  {$ENDIF}
{$ELSE}
      Sleep(InitialSleepTime);
      if AcquireLockByte(ExpectedMemoryLeaksListLocked) then
        Break;
      Sleep(AdditionalSleepTime);
{$ENDIF}
    end;
  end;
  {Allocate the list if it does not exist}
  if ExpectedMemoryLeaks = nil then
    ExpectedMemoryLeaks := kmem_alloc(ExpectedMemoryLeaksListSize,VM_RW);
  {Done}
  Result := ExpectedMemoryLeaks <> nil;
end;

{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
{$IFNDEF FullDebugMode}
  LNewEntry.LeakAddress := ALeakedPointer;
{$ELSE}
  LNewEntry.LeakAddress := Pointer(PByte(ALeakedPointer) - SizeOf(TFullDebugBlockHeader));
{$ENDIF}
  LNewEntry.LeakedClass := nil;
  {$IFDEF CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$ENDIF}
  LNewEntry.LeakSize := 0;
  LNewEntry.LeakCount := 1;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryByAddress, @LNewEntry);
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  ReleaseLockByte(ExpectedMemoryLeaksListLocked);
end;

function RegisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := nil;
  LNewEntry.LeakedClass := ALeakedObjectClass;
  {$IFDEF CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$ENDIF}
  LNewEntry.LeakSize := ALeakedObjectClass.InstanceSize;
  LNewEntry.LeakCount := ACount;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryByClass, @LNewEntry);
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  ReleaseLockByte(ExpectedMemoryLeaksListLocked);
end;

{$IFDEF CheckCppObjectTypeEnabled}
function RegisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  if Assigned(GetCppVirtObjSizeByTypeIdPtrFunc) then
  begin
    //Return 0 if not a proper type
    LNewEntry.LeakSize := GetCppVirtObjSizeByTypeIdPtrFunc(ALeakedCppVirtObjTypeIdPtr);
    if LNewEntry.LeakSize > 0 then
    begin
      LNewEntry.LeakAddress := nil;
      LNewEntry.LeakedClass := nil;
      LNewEntry.LeakedCppTypeIdPtr := ALeakedCppVirtObjTypeIdPtr;
      LNewEntry.LeakCount := ACount;
      {Add it to the correct list}
      Result := LockExpectedMemoryLeaksList
        and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByClass, @LNewEntry);
      {$IFNDEF AssumeMultiThreaded}
        if IsMultiThread then
      {$ENDIF}
      ReleaseLockByte(@ExpectedMemoryLeaksListLocked);
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;
{$ENDIF}

function RegisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := nil;
  LNewEntry.LeakedClass := nil;
  {$IFDEF CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$ENDIF}
  LNewEntry.LeakSize := ALeakedBlockSize;
  LNewEntry.LeakCount := ACount;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryBySizeOnly, @LNewEntry);
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  ReleaseLockByte(ExpectedMemoryLeaksListLocked);
end;

function UnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
{$IFNDEF FullDebugMode}
  LNewEntry.LeakAddress := ALeakedPointer;
{$ELSE}
  LNewEntry.LeakAddress := Pointer(PByte(ALeakedPointer) - SizeOf(TFullDebugBlockHeader));
{$ENDIF}
  LNewEntry.LeakedClass := nil;
  {$IFDEF CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$ENDIF}
  LNewEntry.LeakSize := 0;
  LNewEntry.LeakCount := -1;
  {Remove it from the list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryByAddress, @LNewEntry);
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  ReleaseLockByte(ExpectedMemoryLeaksListLocked);
end;

function UnregisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
begin
  Result := RegisterExpectedMemoryLeak(ALeakedObjectClass, - ACount);
end;

{$IFDEF CheckCppObjectTypeEnabled}
function UnregisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): Boolean; overload;
begin
  Result := RegisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr, - ACount);
end;
{$ENDIF}

function UnregisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer {$IFNDEF FPC}= 1{$ENDIF}): Boolean; overload;
begin
  Result := RegisterExpectedMemoryLeak(ALeakedBlockSize, - ACount);
end;

{Returns a list of all expected memory leaks}
function GetRegisteredMemoryLeaks: TRegisteredMemoryLeaks;

  procedure AddEntries(AEntry: PExpectedMemoryLeak);
  var
    LInd: Integer;
  begin
    while AEntry <> nil do
    begin
      LInd := Length(Result);
      SetLength(Result, LInd + 1);
      {Add the entry}
{$IFNDEF FullDebugMode}
      Result[LInd].LeakAddress := AEntry^.LeakAddress;
{$ELSE}
      Result[LInd].LeakAddress := Pointer(PByte(AEntry^.LeakAddress) + SizeOf(TFullDebugBlockHeader));
{$ENDIF}
      Result[LInd].LeakedClass := AEntry^.LeakedClass;
{$IFDEF CheckCppObjectTypeEnabled}
      Result[LInd].LeakedCppTypeIdPtr := AEntry.LeakedCppTypeIdPtr;
{$ENDIF}
      Result[LInd].LeakSize := AEntry^.LeakSize;
      Result[LInd].LeakCount := AEntry^.LeakCount;
      {Next entry}
      AEntry := AEntry^.NextLeak;
    end;
  end;

begin
  SetLength(Result, 0);
  if (ExpectedMemoryLeaks <> nil) and LockExpectedMemoryLeaksList then
  begin
    {Add all entries}
    AddEntries(ExpectedMemoryLeaks^.FirstEntryByAddress);
    AddEntries(ExpectedMemoryLeaks^.FirstEntryByClass);
    AddEntries(ExpectedMemoryLeaks^.FirstEntryBySizeOnly);
    {Unlock the list}
    ReleaseLockByte(ExpectedMemoryLeaksListLocked);
  end;
end;

{$ELSE}
  {$IFDEF BDS2006AndUp}
function NoOpRegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean;
begin
  {Do nothing. Used when memory leak reporting is disabled under Delphi 2006 and later.}
  Result := False;
end;

function NoOpUnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean;
begin
  {Do nothing. Used when memory leak reporting is disabled under Delphi 2006 and later.}
  Result := False;
end;
  {$ENDIF}
{$ENDIF}

{Detects the probable string data type for a memory block.}
function DetectStringData(APMemoryBlock: Pointer;
  AAvailableSpaceInBlock: NativeInt): TStringDataType;
const
  {If the string reference count field contains a value greater than this,
   then it is assumed that the block is not a string.}
  MaxRefCount = 255;
  {The lowest ASCII character code considered valid string data. If there are
   any characters below this code point then the data is assumed not to be a
   string. #9 = Tab.}
  MinCharCode = #9;
var
  LStringLength,
  LElemSize,
  LCharInd: Integer;
  LPAnsiStr: PAnsiChar;
  LPUniStr: PWideChar;
begin
  {Check that the reference count is within a reasonable range}
  if PStrRec(APMemoryBlock)^.refCnt > MaxRefCount then
  begin
    Result := stUnknown;
    Exit;
  end;
{$IFDEF BCB6OrDelphi6AndUp}
  {$if RTLVersion >= 20}
  LElemSize := PStrRec(APMemoryBlock).elemSize;
  {Element size must be either 1 (Ansi) or 2 (Unicode)}
  if (LElemSize <> 1) and (LElemSize <> 2) then
  begin
    Result := stUnknown;
    Exit;
  end;
  {$ifend}
  {$if RTLVersion < 20}
  LElemSize := 1;
  {$ifend}
{$ELSE}
  LElemSize := 1;
{$ENDIF}
  {Get the string length}
  LStringLength := PStrRec(APMemoryBlock)^.length;
  {Does the string fit?}
  if (LStringLength <= 0)
    or (LStringLength >= (AAvailableSpaceInBlock - SizeOf(StrRec)) div LElemSize) then
  begin
    Result := stUnknown;
    Exit;
  end;
  {Check for no characters outside the expected range. If there are,
   then it is probably not a string.}
  if LElemSize = 1 then
  begin
    {Check that all characters are in the range considered valid.}
    LPAnsiStr := PAnsiChar(PByte(APMemoryBlock) + SizeOf(StrRec));
    for LCharInd := 1 to LStringLength do
    begin
      if LPAnsiStr^ < MinCharCode then
      begin
        Result := stUnknown;
        Exit;
      end;
      Inc(LPAnsiStr);
    end;
    {Must have a trailing #0}
    if LPAnsiStr^ = #0 then
      Result := stAnsiString
    else
      Result := stUnknown;
  end
  else
  begin
    {Check that all characters are in the range considered valid.}
    LPUniStr := PWideChar(PByte(APMemoryBlock) + SizeOf(StrRec));
    for LCharInd := 1 to LStringLength do
    begin
      if LPUniStr^ < MinCharCode then
      begin
        Result := stUnknown;
        Exit;
      end;
      Inc(LPUniStr);
    end;
    {Must have a trailing #0}
    if LPUniStr^ = #0 then
      Result := stUnicodeString
    else
      Result := stUnknown;
  end;
end;

{Walks all allocated blocks, calling ACallBack for each. Passes the user block size and AUserData to the callback.
 Important note: All block types will be locked during the callback, so the memory manager cannot be used inside it.}
procedure WalkAllocatedBlocks(ACallBack: TWalkAllocatedBlocksCallback; AUserData: Pointer);
const
  DebugHeaderSize = {$IFDEF FullDebugMode}SizeOf(TFullDebugBlockHeader){$ELSE}0{$ENDIF};
  TotalDebugOverhead = {$IFDEF FullDebugMode}FullDebugBlockOverhead{$ELSE}0{$ENDIF};
var
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: NativeUInt;
  LPLargeBlock: PLargeBlockHeader;
  LBlockSize: NativeInt;
  LPSmallBlockPool: PSmallBlockPoolHeader;
  LCurPtr,
  LEndPtr: Pointer;
  LInd: Integer;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked: Boolean;
  LLargeBlocksLocked: Boolean;
{$ENDIF}
begin
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked := False;
  LLargeBlocksLocked := False;
{$ENDIF}
  {Lock all small block types}
  LockAllSmallBlockTypes;
  {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LMediumBlocksLocked := True;
{$ENDIF}
    {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
  end;
  try
    {Step through all the medium block pools}
    LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
    begin
      LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
        {Is the block in use?}
        if (LMediumBlockHeader and IsFreeBlockFlag) = 0 then
        begin
          if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            {Step through all the blocks in the small block pool}
            LPSmallBlockPool := LPMediumBlock;
            {Get the useable size inside a block}
            LBlockSize := LPSmallBlockPool^.BlockType^.BlockSize - BlockHeaderSize - TotalDebugOverhead;
            {Get the first and last pointer for the pool}
            GetFirstAndLastSmallBlockInPool(LPSmallBlockPool, LCurPtr, LEndPtr);
            {Step through all blocks}
            while UIntPtr(LCurPtr) <= UIntPtr(LEndPtr) do
            begin
              {Is this block in use?}
              if (PNativeUInt(PByte(LCurPtr) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0 then
              begin
                ACallBack(PByte(LCurPtr) + DebugHeaderSize, LBlockSize, AUserData);
              end;
              {Next block}
              Inc(PByte(LCurPtr), LPSmallBlockPool^.BlockType^.BlockSize);
            end;
          end
          else
          begin
            LBlockSize := (LMediumBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize - TotalDebugOverhead;
            ACallBack(PByte(LPMediumBlock) + DebugHeaderSize, LBlockSize, AUserData);
          end;
        end;
        {Next medium block}
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;
      {Get the next medium block pool}
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
    end;
  finally
    {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
    if LMediumBlocksLocked then
{$ENDIF}
    begin
      // LMediumBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
      UnlockMediumBlocks;
    end;
    {Unlock all the small block types}
    for LInd := 0 to NumSmallBlockTypes - 1 do
    begin
      ReleaseLockByte(SmallBlockTypes[LInd].SmallBlockTypeLocked);
    end;
  end;
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LLargeBlocksLocked := True;
{$ENDIF}
    {Step through all the large blocks}
    {$IFDEF LogLockContention}LDidSleep :={$ENDIF}
    LockLargeBlocks;
  end;
  try
    {Get all leaked large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @LargeBlocksCircularList do
    begin
      LBlockSize := (LPLargeBlock^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask) - BlockHeaderSize - LargeBlockHeaderSize - TotalDebugOverhead;
      ACallBack(PByte(LPLargeBlock) + LargeBlockHeaderSize + DebugHeaderSize, LBlockSize, AUserData);
      {Get the next large block}
      LPLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
    end;
  finally
{$IFNDEF AssumeMultiThreaded}
    if LLargeBlocksLocked then
{$ENDIF}
    begin
      // LLargeBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
      UnlockLargeBlocks;
    end;
  end;
end;

{-----------LogMemoryManagerStateToFile implementation------------}
const
  MaxMemoryLogNodes = 100000;
  QuickSortMinimumItemsInPartition = 4;

type
  {While scanning the memory pool the list of classes is built up in a binary search tree.}
  PMemoryLogNode = ^TMemoryLogNode;
  TMemoryLogNode = record
    {The left and right child nodes}
    LeftAndRightNodePointers: array[Boolean] of PMemoryLogNode;
    {The class this node belongs to}
    ClassPtr: TClass;
    {The number of instances of the class}
    InstanceCount: NativeInt;
    {The total memory usage for this class}
    TotalMemoryUsage: NativeInt;
  end;
  TMemoryLogNodes = array[0..MaxMemoryLogNodes - 1] of TMemoryLogNode;
  PMemoryLogNodes = ^TMemoryLogNodes;

  TMemoryLogInfo = record
    {The number of nodes in "Nodes" that are used.}
    NodeCount: Integer;
    {The root node of the binary search tree. The content of this node is not actually used, it just simplifies the
     binary search code.}
    RootNode: TMemoryLogNode;
    Nodes: TMemoryLogNodes;
  end;
  PMemoryLogInfo = ^TMemoryLogInfo;

{LogMemoryManagerStateToFile callback subroutine}
procedure LogMemoryManagerStateCallBack(APBlock: Pointer; ABlockSize: NativeInt; AUserData: Pointer);
var
  LClass,
  LClassHashBits: NativeUInt;
  LPLogInfo: PMemoryLogInfo;
  LPParentNode,
  LPClassNode: PMemoryLogNode;
  LChildNodeDirection: Boolean;
begin
  LPLogInfo := AUserData;
  {Detecting an object is very expensive (due to the VirtualQuery call), so we do some basic checks and try to find
   the "class" in the tree first.}
  LClass := PNativeUInt(APBlock)^;
  {Do some basic pointer checks: The "class" must be dword aligned and beyond 64K}
  if (LClass > 65535)
    and ((LClass and 3) = 0) then
  begin
    LPParentNode := @LPLogInfo^.RootNode;
    LClassHashBits := LClass;
    repeat
      LChildNodeDirection := Boolean(LClassHashBits and 1);
      {Split off the next bit of the class pointer and traverse in the appropriate direction.}
      LPClassNode := LPParentNode^.LeftAndRightNodePointers[LChildNodeDirection];
      {Is this child node the node the class we're looking for?}
      if (LPClassNode = nil) or (NativeUInt(LPClassNode^.ClassPtr) = LClass) then
        Break;
      {The node was not found: Keep on traversing the tree.}
      LClassHashBits := LClassHashBits shr 1;
      LPParentNode := LPClassNode;
    until False;
  end
  else
    LPClassNode := nil;
  {Was the "class" found?}
  if LPClassNode = nil then
  begin
    {The "class" is not yet in the tree: Determine if it is actually a class.}
    LClass := NativeUInt(DetectClassInstance(APBlock));
    {If it is not a class, try to detect the string type.}
    if LClass = 0 then
      LClass := Ord(DetectStringData(APBlock, ABlockSize));
    {Is this class already in the tree?}
    LPParentNode := @LPLogInfo^.RootNode;
    LClassHashBits := LClass;
    repeat
      LChildNodeDirection := Boolean(LClassHashBits and 1);
      {Split off the next bit of the class pointer and traverse in the appropriate direction.}
      LPClassNode := LPParentNode^.LeftAndRightNodePointers[LChildNodeDirection];
      {Is this child node the node the class we're looking for?}
      if LPClassNode = nil then
      begin
        {The end of the tree was reached: Add a new child node.}
        LPClassNode := @LPLogInfo^.Nodes[LPLogInfo^.NodeCount];
        Inc(LPLogInfo^.NodeCount);
        LPParentNode^.LeftAndRightNodePointers[LChildNodeDirection] := LPClassNode;
        LPClassNode^.ClassPtr := TClass(LClass);
        Break;
      end
      else
      begin
        if NativeUInt(LPClassNode^.ClassPtr) = LClass then
          Break;
      end;
      {The node was not found: Keep on traversing the tree.}
      LClassHashBits := LClassHashBits shr 1;
      LPParentNode := LPClassNode;
    until False;
  end;
  {Update the statistics for the class}
  Inc(LPClassNode^.InstanceCount);
  Inc(LPClassNode^.TotalMemoryUsage, ABlockSize);
end;

{This function is only needed to copy with an error given when using
the "typed @ operator" compiler option. We are having just one typecast
in this function to avoid using typecasts throught the entire program.}
function GetNodeListFromNode(ANode: PMemoryLogNode): PMemoryLogNodes;
  {$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}
begin
  {We have only one typecast here, in other places we have strict type checking}
  Result := PMemoryLogNodes(ANode);
end;

{LogMemoryManagerStateToFile subroutine: A median-of-3 quicksort routine for sorting a TMemoryLogNodes array.}
procedure QuickSortLogNodes(APLeftItem: PMemoryLogNodes; ARightIndex: Integer);
var
  LPLeftItem: PMemoryLogNodes;
  LRightIndex: Integer;
  M, I, J: Integer;
  LPivot,
  LTempItem: TMemoryLogNode;
  PMemLogNode: PMemoryLogNode; {This variable is just neede to simplify the accomodation
                                to "typed @ operator" - stores an intermediary value}
begin
  LPLeftItem := APLeftItem;
  LRightIndex := ARightIndex;
  while True do
  begin
    {Order the left, middle and right items in ascending order}
    M := LRightIndex shr 1;
    {Is the middle item larger than the left item?}
    if LPLeftItem^[0].TotalMemoryUsage > LPLeftItem^[M].TotalMemoryUsage then
    begin
      {Swap items 0 and M}
      LTempItem := LPLeftItem^[0];
      LPLeftItem^[0] := LPLeftItem^[M];
      LPLeftItem^[M] := LTempItem;
    end;
    {Is the middle item larger than the right?}
    if LPLeftItem^[M].TotalMemoryUsage > LPLeftItem^[LRightIndex].TotalMemoryUsage then
    begin
      {The right-hand item is not larger - swap it with the middle}
      LTempItem := LPLeftItem^[LRightIndex];
      LPLeftItem^[LRightIndex] := LPLeftItem^[M];
      LPLeftItem^[M] := LTempItem;
      {Is the left larger than the new middle?}
      if LPLeftItem^[0].TotalMemoryUsage > LPLeftItem^[M].TotalMemoryUsage then
      begin
        {Swap items 0 and M}
        LTempItem := LPLeftItem^[0];
        LPLeftItem^[0] := LPLeftItem^[M];
        LPLeftItem^[M] := LTempItem;
      end;
    end;
    {Move the pivot item out of the way by swapping M with R - 1}
    LPivot := LPLeftItem^[M];
    LPLeftItem^[M] := LPLeftItem^[LRightIndex - 1];
    LPLeftItem^[LRightIndex - 1] := LPivot;
    {Set up the loop counters}
    I := 0;
    J := LRightIndex - 1;
    while True do
    begin
      {Find the first item from the left that is not smaller than the pivot}
      repeat
        Inc(I);
      until LPLeftItem^[I].TotalMemoryUsage >= LPivot.TotalMemoryUsage;
      {Find the first item from the right that is not larger than the pivot}
      repeat
        Dec(J);
      until LPLeftItem^[J].TotalMemoryUsage <= LPivot.TotalMemoryUsage;
      {Stop the loop when the two indexes cross}
      if J < I then
        Break;
      {Swap item I and J}
      LTempItem := LPLeftItem^[I];
      LPLeftItem^[I] := LPLeftItem^[J];
      LPLeftItem^[J] := LTempItem;
    end;
    {Put the pivot item back in the correct position by swapping I with R - 1}
    LPLeftItem^[LRightIndex - 1] := LPLeftItem^[I];
    LPLeftItem^[I] := LPivot;
    {Sort the left-hand partition}
    if J >= (QuickSortMinimumItemsInPartition - 1) then
      QuickSortLogNodes(LPLeftItem, J);
    {Sort the right-hand partition}
    PMemLogNode := @(LPLeftItem[I + 1]);
    LPLeftItem := GetNodeListFromNode(PMemLogNode);
    LRightIndex := LRightIndex - I - 1;
    if LRightIndex < (QuickSortMinimumItemsInPartition - 1) then
      Break;
  end;
end;

{LogMemoryManagerStateToFile subroutine: An InsertionSort routine for sorting a TMemoryLogNodes array.}
procedure InsertionSortLogNodes(APLeftItem: PMemoryLogNodes; ARightIndex: Integer);
var
  I, J: Integer;
  LCurNode: TMemoryLogNode;
begin
  for I := 1 to ARightIndex do
  begin
    LCurNode := APLeftItem^[I];
    {Scan backwards to find the best insertion spot}
    J := I;
    while (J > 0) and (APLeftItem^[J - 1].TotalMemoryUsage > LCurNode.TotalMemoryUsage) do
    begin
      APLeftItem^[J] := APLeftItem^[J - 1];
      Dec(J);
    end;
    APLeftItem^[J] := LCurNode;
  end;
end;

{Writes a log file containing a summary of the memory mananger state and a summary of allocated blocks grouped by
 class. The file will be saved in UTF-8 encoding (in supported Delphi versions). Returns True on success. }
function LogMemoryManagerStateToFile(const AFileName: string; const AAdditionalDetails: string {$IFNDEF FPC}= ''{$ENDIF}): Boolean;
const
  MsgBufferSize = 65536;
  MaxLineLength = 512;
  {Write the UTF-8 BOM in Delphi versions that support UTF-8 conversion.}
  LogStateHeaderMsg = {$IFDEF BCB6OrDelphi7AndUp}#$EF#$BB#$BF + {$ENDIF}
    'FastMM State Capture:'#13#10'---------------------'#13#10#13#10;
  LogStateAllocatedMsg = 'K Allocated'#13#10;
  LogStateOverheadMsg = 'K Overhead'#13#10;
  LogStateEfficiencyMsg = '% Efficiency'#13#10#13#10'Usage Detail:'#13#10;
  LogStateAdditionalInfoMsg = #13#10'Additional Information:'#13#10'-----------------------'#13#10;
  AverageSizeLeadText = ' (';
  AverageSizeTrailingText = ' bytes avg.)'#13#10;
var
  LUMsg,
  LUBuf: NativeUInt;
  LPLogInfo: PMemoryLogInfo;
  LInd: Integer;
  LPNode: PMemoryLogNode;
  LMsgBuffer: array[0..MsgBufferSize - 1] of AnsiChar;
  LPInitialMsgPtr,
  LPMsg: PAnsiChar;
  LBufferSpaceUsed,
  LBytesWritten: Cardinal;
  LFileHandle: THandle; {use NativeUint if THandle is not available}
  LMemoryManagerUsageSummary: TMemoryManagerUsageSummary;
  LUTF8Str: AnsiString;
  LMemLogNode: PMemoryLogNode; {Just to store an interim result. Needed for
                                "typed @ operator", to simplify things and remove
                                typecasts that pose potential dannger.}
  LInitialSize: Cardinal;
  LCallback: TWalkAllocatedBlocksCallback;
begin
  {Get the current memory manager usage summary.}
  GetMemoryManagerUsageSummary(LMemoryManagerUsageSummary);
  {Allocate the memory required to capture detailed allocation information.}
  LPLogInfo := kmem_alloc(SizeOf(TMemoryLogInfo),VM_RW);
  if LPLogInfo <> nil then
  begin
    try
      {Log all allocated blocks by class.}
      LCallback := {$IFDEF FPC}@{$ENDIF}LogMemoryManagerStateCallBack;
      WalkAllocatedBlocks(LCallback, LPLogInfo);
      {Sort the classes by total memory usage: Do the initial QuickSort pass over the list to sort the list in groups
       of QuickSortMinimumItemsInPartition size.}
      if LPLogInfo^.NodeCount >= QuickSortMinimumItemsInPartition then
      begin
        LMemLogNode := @(LPLogInfo^.Nodes[0]);
        QuickSortLogNodes(GetNodeListFromNode(LMemLogNode), LPLogInfo^.NodeCount - 1);
      end;
      {Do the final InsertionSort pass.}
      LMemLogNode := @(LPLogInfo^.Nodes[0]);
      InsertionSortLogNodes(GetNodeListFromNode(LMemLogNode), LPLogInfo^.NodeCount - 1);
      {Create the output file}
      {$IFDEF POSIX}
      lFileHandle := FileCreate(AFilename);
      {$ELSE}
      LFileHandle := CreateFile(PChar(AFilename), GENERIC_READ or GENERIC_WRITE, 0,
        nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      {$ENDIF}
      if LFileHandle <> INVALID_HANDLE_VALUE then
      begin
        try
          {Log the usage summary}
          LPMsg := @(LMsgBuffer[0]);
          LPInitialMsgPtr := LPMsg;
          LInitialSize := (SizeOf(LMsgBuffer) div SizeOf(LMsgBuffer[0]))-1;
          LPMsg := AppendStringToBuffer(LogStateHeaderMsg, LPMsg, Length(LogStateHeaderMsg), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          LPMsg := NativeUIntToStrBuf(LMemoryManagerUsageSummary.AllocatedBytes shr 10, LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          LPMsg := AppendStringToBuffer(LogStateAllocatedMsg, LPMsg, Length(LogStateAllocatedMsg), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          LPMsg := NativeUIntToStrBuf(LMemoryManagerUsageSummary.OverheadBytes shr 10, LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          LPMsg := AppendStringToBuffer(LogStateOverheadMsg, LPMsg, Length(LogStateOverheadMsg), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          LPMsg := NativeUIntToStrBuf(Round(LMemoryManagerUsageSummary.EfficiencyPercentage), LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          LPMsg := AppendStringToBuffer(LogStateEfficiencyMsg, LPMsg, Length(LogStateEfficiencyMsg), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          {Log the allocation detail}
          for LInd := LPLogInfo^.NodeCount - 1 downto 0 do
          begin
            LPNode := @(LPLogInfo^.Nodes[LInd]);
            {Add the allocated size}
            LPMsg^ := ' ';
            Inc(LPMsg);
            LPMsg := NativeUIntToStrBuf(LPNode^.TotalMemoryUsage, LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
            LPMsg := AppendStringToBuffer(BytesMessage, LPMsg, Length(BytesMessage), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
            {Add the class type}
            case NativeUInt(LPNode^.ClassPtr) of
              {Unknown}
              0:
              begin
                LPMsg := AppendStringToBuffer(UnknownClassNameMsg, LPMsg, Length(UnknownClassNameMsg), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
              end;
              {AnsiString}
              1:
              begin
                LPMsg := AppendStringToBuffer(AnsiStringBlockMessage, LPMsg, Length(AnsiStringBlockMessage), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
              end;
              {UnicodeString}
              2:
              begin
                LPMsg := AppendStringToBuffer(UnicodeStringBlockMessage, LPMsg, Length(UnicodeStringBlockMessage), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
              end;
              {Classes}
            else
              begin
                LPMsg := AppendClassNameToBuffer(LPNode^.ClassPtr, LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
              end;
            end;
            {Add the count}
            LPMsg^ := ' ';
            Inc(LPMsg);
            LPMsg^ := 'x';
            Inc(LPMsg);
            LPMsg^ := ' ';
            Inc(LPMsg);
            LPMsg := NativeUIntToStrBuf(LPNode^.InstanceCount, LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
            LPMsg := AppendStringToBuffer(AverageSizeLeadText, LPMsg, Length(AverageSizeLeadText), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
            LPMsg := NativeUIntToStrBuf(LPNode^.TotalMemoryUsage div LPNode^.InstanceCount, LPMsg, LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
            LPMsg := AppendStringToBuffer(AverageSizeTrailingText, LPMsg, Length(AverageSizeTrailingText), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
            {Flush the buffer?}
            LUMsg := NativeUInt(LPMsg);
            LUBuf := NativeUInt(@LMsgBuffer);
            if LUMsg > LUBuf then
            begin
              LBufferSpaceUsed := LUMsg - LUBuf;
              if LBufferSpaceUsed > (MsgBufferSize - MaxLineLength) then
              begin
                LBytesWritten := 0;
                WriteFile(LFileHandle, LMsgBuffer, LBufferSpaceUsed, LBytesWritten, nil);
                LPMsg := @(LMsgBuffer[0]);
              end;
            end;
          end;
          if AAdditionalDetails <> '' then
          begin
            LPMsg := AppendStringToBuffer(LogStateAdditionalInfoMsg, LPMsg, Length(LogStateAdditionalInfoMsg), LInitialSize-NativeUInt(LPMsg-LPInitialMsgPtr));
          end;
          {Flush any remaining bytes}
          LUMsg := NativeUInt(LPMsg);
          LUBuf := NativeUInt(@LMsgBuffer);
          if LUMsg > LUBuf then
          begin
            LBufferSpaceUsed :=  LUMsg - LUBuf;
            WriteFile(LFileHandle, LMsgBuffer, LBufferSpaceUsed, LBytesWritten, nil);
          end;
          {Write the additional info}
          if AAdditionalDetails <> '' then
          begin
            {$IFDEF BCB6OrDelphi7AndUp}
            LUTF8Str := UTF8Encode(AAdditionalDetails);
            {$ELSE}
            LUTF8Str := AAdditionalDetails;
            {$ENDIF}
            if Length(LUTF8Str) > 0 then
            begin
              WriteFile(LFileHandle, PAnsiChar(LUTF8Str)^, Length(LUTF8Str), LBytesWritten, nil);
            end;
          end;
          {Success}
          Result := True;
        finally
          {Close the file}
          {$IFDEF POSIX}
            {$IFNDEF fpc}
          __close(LFileHandle)
            {$ELSE}
          fpclose(LFileHandle)
            {$ENDIF}
          {$ELSE}
          CloseHandle(LFileHandle);
          {$ENDIF}
        end;
      end
      else
        Result := False;
    finally
      VirtualFree(LPLogInfo, 0, MEM_RELEASE);
    end;
  end
  else
    Result := False;
end;

{-----------CheckBlocksOnShutdown implementation------------}

{Checks blocks for modification after free and also for memory leaks}
procedure CheckBlocksOnShutdown(ACheckForLeakedBlocks: Boolean);
{$IFDEF EnableMemoryLeakReporting}
type
  {Leaked class type}
  TLeakedClass = record
    ClassPointer: TClass;
    {$IFDEF CheckCppObjectTypeEnabled}
    CppTypeIdPtr: Pointer;
    {$ENDIF}
    NumLeaks: Cardinal;
  end;
  TLeakedClasses = array[0..255] of TLeakedClass;
  PLeakedClasses = ^TLeakedClasses;
  {Leak statistics for a small block type}
  TSmallBlockLeaks = array[0..NumSmallBlockTypes - 1] of TLeakedClasses;
  {A leaked medium or large block}
  TMediumAndLargeBlockLeaks = array[0..4095] of NativeUInt;
{$ENDIF}
var
{$IFDEF EnableMemoryLeakReporting}
  {The leaked classes for small blocks}
  LSmallBlockLeaks: TSmallBlockLeaks;
  LLeakType: TMemoryLeakType;
  {$IFDEF CheckCppObjectTypeEnabled}
  LLeakedCppTypeIdPtr: Pointer;
  LCppTypeName: PAnsiChar;
  {$ENDIF}
  LMediumAndLargeBlockLeaks: TMediumAndLargeBlockLeaks;
  LNumMediumAndLargeLeaks: Integer;
  LPLargeBlock: PLargeBlockHeader;
  LLeakMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
  {$IFNDEF NoMessageBoxes}
  LMessageTitleBuffer: array[0..MaxDisplayMessageLength-1] of AnsiChar;
  {$ENDIF}
  LPInitialPtr, LMsgPtr: PAnsiChar;
  LInitialSize: Cardinal;
  LExpectedLeaksOnly, LSmallLeakHeaderAdded, LBlockSizeHeaderAdded: Boolean;
  LBlockTypeInd, LClassInd, LBlockInd: Cardinal;
  LMediumBlockSize, LPreviousBlockSize, LLargeBlockSize, LThisBlockSize: NativeUInt;
{$ENDIF}
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: NativeUInt;

{$IFDEF EnableMemoryLeakReporting}
  {Tries to account for a memory leak. Returns true if the leak is expected and
   removes the leak from the list}
  function GetMemoryLeakType(AAddress: Pointer; ASpaceInsideBlock: NativeUInt): TMemoryLeakType;
  var
    LLeak: TExpectedMemoryLeak;
  begin
    {Default to not found}
    Result := mltUnexpectedLeak;
    if ExpectedMemoryLeaks <> nil then
    begin
      {Check by pointer address}
      LLeak.LeakAddress := AAddress;
      LLeak.LeakedClass := nil;
      {$IFDEF CheckCppObjectTypeEnabled}
      LLeak.LeakedCppTypeIdPtr := nil;
      {$ENDIF}
      LLeak.LeakSize := 0;
      LLeak.LeakCount := -1;
      if UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryByAddress, @LLeak, False) then
      begin
        Result := mltExpectedLeakRegisteredByPointer;
        Exit;
      end;
      {Check by class}
      LLeak.LeakAddress := nil;
      {$IFDEF FullDebugMode}
      LLeak.LeakedClass := TClass(PNativeUInt(PByte(AAddress)+ SizeOf(TFullDebugBlockHeader))^);
      {$ELSE}
      LLeak.LeakedClass := TClass(PNativeUInt(AAddress)^);
      {$ENDIF}
      {$IFDEF CheckCppObjectTypeEnabled}
      if Assigned(GetCppVirtObjTypeIdPtrFunc) then
      begin
        {$IFDEF FullDebugMode}
        LLeak.LeakedCppTypeIdPtr := GetCppVirtObjTypeIdPtrFunc(Pointer(PByte(AAddress)
          + SizeOf(TFullDebugBlockHeader)), ASpaceInsideBlock);
        {$ELSE}
        LLeak.LeakedCppTypeIdPtr := GetCppVirtObjTypeIdPtrFunc(AAddress, ASpaceInsideBlock);
        {$ENDIF}
      end;
      LLeakedCppTypeIdPtr := LLeak.LeakedCppTypeIdPtr;
      {$ENDIF}
      LLeak.LeakSize := ASpaceInsideBlock;
      if UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryByClass, @LLeak, False) then
      begin
        Result := mltExpectedLeakRegisteredByClass;
        Exit;
      end;
      {Check by size: the block must be large enough to hold the leak}
      LLeak.LeakedClass := nil;
      if UpdateExpectedLeakList(@ExpectedMemoryLeaks^.FirstEntryBySizeOnly, @LLeak, False) then
        Result := mltExpectedLeakRegisteredBySize;
    end;
  end;

  {Checks the small block pool for leaks.}
  procedure CheckSmallBlockPoolForLeaks(APSmallBlockPool: PSmallBlockPoolHeader);
  var
    LLeakedClass: TClass;
    {$IFDEF CheckCppObjectTypeEnabled}
    LLeakedCppObjectTypeId: Pointer;
    {$ENDIF}
    LSmallBlockLeakType: TMemoryLeakType;
    LClassIndex: Integer;
    LCurPtr, LEndPtr, LDataPtr: Pointer;
    LBlockTypeIndex: Cardinal;
    LPLeakedClasses: PLeakedClasses;
    LSmallBlockSize: Cardinal;
  begin
    {Get the useable size inside a block}
    LSmallBlockSize := APSmallBlockPool^.BlockType^.BlockSize - BlockHeaderSize;
  {$IFDEF FullDebugMode}
    Dec(LSmallBlockSize, FullDebugBlockOverhead);
  {$ENDIF}
    {Get the block type index}
    LBlockTypeIndex := (UIntPtr(APSmallBlockPool^.BlockType) - UIntPtr(@SmallBlockTypes[0]))
{$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
      shr SmallBlockTypeRecSizePowerOf2
{$ELSE}
      div SmallBlockTypeRecSize
{$ENDIF}
    ;
    LPLeakedClasses := @LSmallBlockLeaks[LBlockTypeIndex];
    {Get the first and last pointer for the pool}
    GetFirstAndLastSmallBlockInPool(APSmallBlockPool, LCurPtr, LEndPtr);
    {Step through all blocks}
    while UIntPtr(LCurPtr) <= UIntPtr(LEndPtr) do
    begin
      {Is this block in use? If so, is the debug info intact?}
      if ((PNativeUInt(PByte(LCurPtr) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0) then
      begin
  {$IFDEF FullDebugMode}
        if CheckBlockBeforeFreeOrRealloc(LCurPtr, boBlockCheck) then
  {$ENDIF}
        begin
          {$IFDEF CheckCppObjectTypeEnabled}
          LLeakedCppTypeIdPtr := nil;
          {$ENDIF}
          {Get the leak type}
          LSmallBlockLeakType := GetMemoryLeakType(LCurPtr, LSmallBlockSize);
    {$IFDEF LogMemoryLeakDetailToFile}
      {$IFDEF HideExpectedLeaksRegisteredByPointer}
          if LSmallBlockLeakType <> mltExpectedLeakRegisteredByPointer then
      {$ENDIF}
            LogMemoryLeakOrAllocatedBlock(LCurPtr, True);
    {$ENDIF}
          {Only expected leaks?}
          LExpectedLeaksOnly := LExpectedLeaksOnly and (LSmallBlockLeakType <> mltUnexpectedLeak);
    {$IFDEF HideExpectedLeaksRegisteredByPointer}
          if LSmallBlockLeakType <> mltExpectedLeakRegisteredByPointer then
    {$ENDIF}
          begin
            {Get a pointer to the user data}
    {$IFNDEF FullDebugMode}
            LDataPtr := LCurPtr;
    {$ELSE}
            LDataPtr := Pointer(PByte(LCurPtr) + SizeOf(TFullDebugBlockHeader));
    {$ENDIF}
            {Default to an unknown block}
            LClassIndex := 0;
            {Get the class contained by the block}
            LLeakedClass := DetectClassInstance(LDataPtr);
            {Not a Delphi class? -> is it perhaps a string or C++ object type?}
            if LLeakedClass = nil then
            begin
              {$IFDEF CheckCppObjectTypeEnabled}
              LLeakedCppObjectTypeId := LLeakedCppTypeIdPtr;
              if (LLeakedCppObjectTypeId = nil) and (ExpectedMemoryLeaks = nil) then
              begin
                if Assigned(GetCppVirtObjTypeIdPtrFunc) then
                begin
                  LLeakedCppObjectTypeId := GetCppVirtObjTypeIdPtrFunc(LDataPtr, LSmallBlockSize);
                end;
              end;
              if Assigned(LLeakedCppObjectTypeId) then
              begin
                LClassIndex := 3;
                while LClassIndex <= High(TLeakedClasses) do
                begin
                  if (Pointer(LPLeakedClasses[LClassIndex].CppTypeIdPtr) = LLeakedCppObjectTypeId)
                    or ((LPLeakedClasses[LClassIndex].CppTypeIdPtr = nil)
                    and (LPLeakedClasses[LClassIndex].ClassPointer = nil)) then
                  begin
                    Break;
                  end;
                  Inc(LClassIndex);
                end;
                if LClassIndex <= High(TLeakedClasses) then
                  Pointer(LPLeakedClasses[LClassIndex].CppTypeIdPtr) := LLeakedCppObjectTypeId
                else
                  LClassIndex := 0;
              end
              else
              begin
              {$ENDIF}
                {Not a known class: Is it perhaps string data?}
                case DetectStringData(LDataPtr, APSmallBlockPool^.BlockType^.BlockSize - (BlockHeaderSize {$IFDEF FullDebugMode} + FullDebugBlockOverhead{$ENDIF})) of
                  stAnsiString: LClassIndex := 1;
                  stUnicodeString: LClassIndex := 2;
                end;
              {$IFDEF CheckCppObjectTypeEnabled}
              end;
              {$ENDIF}
            end
            else
            begin
              LClassIndex := 3;
              while LClassIndex <= High(TLeakedClasses) do
              begin
                if (LPLeakedClasses^[LClassIndex].ClassPointer = LLeakedClass)
                  or ((LPLeakedClasses^[LClassIndex].ClassPointer = nil)
                  {$IFDEF CheckCppObjectTypeEnabled}
                  and (LPLeakedClasses[LClassIndex].CppTypeIdPtr = nil)
                  {$ENDIF}
                  ) then
                begin
                  Break;
                end;
                Inc(LClassIndex);
              end;
              if LClassIndex <= High(TLeakedClasses) then
                LPLeakedClasses^[LClassIndex].ClassPointer := LLeakedClass
              else
                LClassIndex := 0;
            end;
            {Add to the number of leaks for the class}
            Inc(LPLeakedClasses^[LClassIndex].NumLeaks);
          end;
        end;
      end
      else
      begin
  {$IFDEF CheckUseOfFreedBlocksOnShutdown}
        {Check that the block has not been modified since being freed}
        CheckFreeBlockUnmodified(LCurPtr, APSmallBlockPool^.BlockType^.BlockSize, boBlockCheck);
  {$ENDIF}
      end;
      {Next block}
      Inc(PByte(LCurPtr), APSmallBlockPool^.BlockType^.BlockSize);
    end;
  end;
{$ENDIF}

begin
{$IFDEF EnableMemoryLeakReporting}
  {Clear the leak arrays}
  FillChar(LSmallBlockLeaks, SizeOf(LSmallBlockLeaks), 0);
  FillChar(LMediumAndLargeBlockLeaks, SizeOf(LMediumAndLargeBlockLeaks), 0);
  {Step through all the medium block pools}
  LNumMediumAndLargeLeaks := 0;
  {No unexpected leaks so far}
  LExpectedLeaksOnly := True;
{$ENDIF}
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
      {Is the block in use?}
      if (LMediumBlockHeader and IsFreeBlockFlag) = 0 then
      begin
{$IFDEF EnableMemoryLeakReporting}
        if ACheckForLeakedBlocks then
        begin
          if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            {Get all the leaks for the small block pool}
            CheckSmallBlockPoolForLeaks(LPMediumBlock);
          end
          else
          begin
            if (LNumMediumAndLargeLeaks < Length(LMediumAndLargeBlockLeaks))
  {$IFDEF FullDebugMode}
              and CheckBlockBeforeFreeOrRealloc(LPMediumBlock, boBlockCheck)
  {$ENDIF}
            then
            begin
              LMediumBlockSize := (LMediumBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
  {$IFDEF FullDebugMode}
              Dec(LMediumBlockSize, FullDebugBlockOverhead);
  {$ENDIF}
              {Get the leak type}
              LLeakType := GetMemoryLeakType(LPMediumBlock, LMediumBlockSize);
              {Is it an expected leak?}
              LExpectedLeaksOnly := LExpectedLeaksOnly and (LLeakType <> mltUnexpectedLeak);
  {$IFDEF LogMemoryLeakDetailToFile}
    {$IFDEF HideExpectedLeaksRegisteredByPointer}
              if LLeakType <> mltExpectedLeakRegisteredByPointer then
    {$ENDIF}
                LogMemoryLeakOrAllocatedBlock(LPMediumBlock, True);
  {$ENDIF}
  {$IFDEF HideExpectedLeaksRegisteredByPointer}
              if LLeakType <> mltExpectedLeakRegisteredByPointer then
  {$ENDIF}
              begin
                {Add the leak to the list}
                LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LMediumBlockSize;
                Inc(LNumMediumAndLargeLeaks);
              end;
            end;
          end;
        end;
{$ENDIF}
      end
      else
      begin
{$IFDEF CheckUseOfFreedBlocksOnShutdown}
        {Check that the block has not been modified since being freed}
        CheckFreeBlockUnmodified(LPMediumBlock, LMediumBlockHeader and DropMediumAndLargeFlagsMask, boBlockCheck);
{$ENDIF}
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
  end;
{$IFDEF EnableMemoryLeakReporting}
  if ACheckForLeakedBlocks then
  begin
    {Get all leaked large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @LargeBlocksCircularList do
    begin
      if (LNumMediumAndLargeLeaks < length(LMediumAndLargeBlockLeaks))
  {$IFDEF FullDebugMode}
        and CheckBlockBeforeFreeOrRealloc(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), boBlockCheck)
  {$ENDIF}
      then
      begin
        LLargeBlockSize := (LPLargeBlock^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask) - BlockHeaderSize - LargeBlockHeaderSize;
  {$IFDEF FullDebugMode}
        Dec(LLargeBlockSize, FullDebugBlockOverhead);
  {$ENDIF}
        {Get the leak type}
        LLeakType := GetMemoryLeakType(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), LLargeBlockSize);
        {Is it an expected leak?}
        LExpectedLeaksOnly := LExpectedLeaksOnly and (LLeakType <> mltUnexpectedLeak);
  {$IFDEF LogMemoryLeakDetailToFile}
    {$IFDEF HideExpectedLeaksRegisteredByPointer}
        if LLeakType <> mltExpectedLeakRegisteredByPointer then
    {$ENDIF}
          LogMemoryLeakOrAllocatedBlock(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), True);
  {$ENDIF}
  {$IFDEF HideExpectedLeaksRegisteredByPointer}
        if LLeakType <> mltExpectedLeakRegisteredByPointer then
  {$ENDIF}
        begin
          {Add the leak}
          LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LLargeBlockSize;
          Inc(LNumMediumAndLargeLeaks);
        end;
      end;
      {Get the next large block}
      LPLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
    end;
    {Display the leak message if required}
    if not LExpectedLeaksOnly then
    begin
      {Small leak header has not been added}
      LSmallLeakHeaderAdded := False;
      LPreviousBlockSize := 0;
      {Set up the leak message header so long}

      LMsgPtr := @LLeakMessage[0];
      LPInitialPtr := LMsgPtr;
      LInitialSize := (SizeOf(LLeakMessage) div SizeOf(LLeakMessage[0]))-1;


      LMsgPtr := AppendStringToBuffer(LeakMessageHeader, LMsgPtr, length(LeakMessageHeader), LInitialSize);
      {Step through all the small block types}
      for LBlockTypeInd := 0 to NumSmallBlockTypes - 1 do
      begin
        LThisBlockSize := SmallBlockTypes[LBlockTypeInd].BlockSize - BlockHeaderSize;
  {$IFDEF FullDebugMode}
        if LThisBlockSize > FullDebugBlockOverhead then
        begin
          Dec(LThisBlockSize, FullDebugBlockOverhead);
        end else
        begin
          LThisBlockSize := 0;
        end;
  {$ENDIF}
        LBlockSizeHeaderAdded := False;
        {Any leaks?}
        for LClassInd := High(LSmallBlockLeaks[LBlockTypeInd]) downto 0 do
        begin
          {Is there still space in the message buffer? Reserve space for the message
           footer.}
          if LMsgPtr > @LLeakMessage[High(LLeakMessage) - MaxFileNameLengthDouble] then
            Break;
          {Check the count}
          if LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks > 0 then
          begin
            {Need to add the header?}
            if not LSmallLeakHeaderAdded then
            begin
              LMsgPtr := AppendStringToBuffer(SmallLeakDetail, LMsgPtr, Length(SmallLeakDetail), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              LSmallLeakHeaderAdded := True;
            end;
            {Need to add the size header?}
            if not LBlockSizeHeaderAdded then
            begin
              LMsgPtr^ := #13;
              Inc(LMsgPtr);
              LMsgPtr^ := #10;
              Inc(LMsgPtr);
              LMsgPtr := NativeUIntToStrBuf(LPreviousBlockSize + 1, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
              LMsgPtr^ := '-';
              Inc(LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
              LMsgPtr := NativeUIntToStrBuf(LThisBlockSize, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              LMsgPtr := AppendStringToBuffer(BytesMessage, LMsgPtr, Length(BytesMessage), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              LBlockSizeHeaderAdded := True;
            end
            else
            begin
              LMsgPtr^ := ',';
              Inc(LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
            end;
            {Show the count}
            case LClassInd of
              {Unknown}
              0:
              begin
                LMsgPtr := AppendStringToBuffer(UnknownClassNameMsg, LMsgPtr, Length(UnknownClassNameMsg), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              end;
              {AnsiString}
              1:
              begin
                LMsgPtr := AppendStringToBuffer(AnsiStringBlockMessage, LMsgPtr, Length(AnsiStringBlockMessage), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              end;
              {UnicodeString}
              2:
              begin
                LMsgPtr := AppendStringToBuffer(UnicodeStringBlockMessage, LMsgPtr, Length(UnicodeStringBlockMessage), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
              end;
              {Classes}
            else
              begin
                {$IFDEF CheckCppObjectTypeEnabled}
                if LSmallBlockLeaks[LBlockTypeInd][LClassInd].CppTypeIdPtr <> nil then
                begin
                  if Assigned(GetCppVirtObjTypeNameByTypeIdPtrFunc) then
                  begin
                    LCppTypeName := GetCppVirtObjTypeNameByTypeIdPtrFunc(LSmallBlockLeaks[LBlockTypeInd][LClassInd].CppTypeIdPtr);
                    LMsgPtr := AppendStringToBuffer(LCppTypeName, LMsgPtr, StrLen(LCppTypeName));
                  end
                  else
                    LMsgPtr := AppendClassNameToBuffer(nil, LMsgPtr);
                end
                else
                begin
                {$ENDIF}
                  LMsgPtr := AppendClassNameToBuffer(LSmallBlockLeaks[LBlockTypeInd][LClassInd].ClassPointer, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
                {$IFDEF CheckCppObjectTypeEnabled}
                end;
                {$ENDIF}
              end;
            end;
            {Add the count}
            LMsgPtr^ := ' ';
            Inc(LMsgPtr);
            LMsgPtr^ := 'x';
            Inc(LMsgPtr);
            LMsgPtr^ := ' ';
            Inc(LMsgPtr);
            LMsgPtr := NativeUIntToStrBuf(LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
          end;
        end;
        LPreviousBlockSize := LThisBlockSize;
      end;
      {Add the medium/large block leak message}
      if LNumMediumAndLargeLeaks > 0 then
      begin
        {Any non-small leaks?}
        if LSmallLeakHeaderAdded then
        begin
          LMsgPtr^ := #13;
          Inc(LMsgPtr);
          LMsgPtr^ := #10;
          Inc(LMsgPtr);
          LMsgPtr^ := #13;
          Inc(LMsgPtr);
          LMsgPtr^ := #10;
          Inc(LMsgPtr);
        end;
        {Add the medium/large block leak message}
        LMsgPtr := AppendStringToBuffer(LargeLeakDetail, LMsgPtr, Length(LargeLeakDetail), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
        {List all the blocks}
        for LBlockInd := 0 to LNumMediumAndLargeLeaks - 1 do
        begin
          if LBlockInd <> 0 then
          begin
            LMsgPtr^ := ',';
            Inc(LMsgPtr);
            LMsgPtr^ :=  ' ';
            Inc(LMsgPtr);
          end;
          LMsgPtr := NativeUIntToStrBuf(LMediumAndLargeBlockLeaks[LBlockInd], LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
          {Is there still space in the message buffer? Reserve space for the
           message footer.}
          if LMsgPtr > @LLeakMessage[High(LLeakMessage) - MaxFileNameLengthDouble] then
            Break;
        end;
      end;
  {$IFDEF LogErrorsToFile}
       {Set the message footer}
        LMsgPtr := AppendStringToBuffer(LeakMessageFooter, LMsgPtr, Length(LeakMessageFooter), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
        {Append the message to the memory errors file}
        AppendEventLog(@LLeakMessage[0], UIntPtr(LMsgPtr) - UIntPtr(@LLeakMessage[1]));
  {$ELSE}
      {Set the message footer}
      AppendStringToBuffer(LeakMessageFooter, LMsgPtr, Length(LeakMessageFooter), LInitialSize-NativeUInt(LMsgPtr-LPInitialPtr));
  {$ENDIF}
  {$IFDEF UseOutputDebugString}
      OutputDebugStringA(LLeakMessage);
  {$ENDIF}
  {$IFNDEF NoMessageBoxes}
      {Show the message}
      FillChar(LMessageTitleBuffer, SizeOf(LMessageTitleBuffer), 0);
      AppendStringToModuleName(LeakMessageTitle, LMessageTitleBuffer, Length(LeakMessageTitle), (SizeOf(LMessageTitleBuffer) div SizeOf(LMessageTitleBuffer[0]))-1);
      ShowMessageBox(LLeakMessage, LMessageTitleBuffer);
  {$ENDIF}
    end;
  end;
{$ENDIF}
end;

{Returns statistics about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);
const
  BlockHeaderSizeWithAnyOverhead = BlockHeaderSize{$IFDEF FullDebugMode} + FullDebugBlockOverhead{$ENDIF};
var
  LIndBlockSize,
  LUsableBlockSize: Cardinal;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumBlock: Pointer;
  LInd: Integer;
  LBlockTypeIndex,
  LMediumBlockSize: Cardinal;
  LMediumBlockHeader,
  LLargeBlockSize: NativeUInt;
  LPLargeBlock: PLargeBlockHeader;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked: Boolean;
  LLargeBlocksLocked: Boolean;
{$ENDIF}
begin
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked := False;
  LLargeBlocksLocked := False;
{$ENDIF}
  {Clear the structure}
  FillChar(AMemoryManagerState, SizeOf(AMemoryManagerState), 0);
  {Set the small block size stats}
  for LInd := 0 to NumSmallBlockTypes - 1 do
  begin
    LIndBlockSize := SmallBlockTypes[LInd].BlockSize;
    AMemoryManagerState.SmallBlockTypeStates[LInd].InternalBlockSize := LIndBlockSize;
    if LIndBlockSize > BlockHeaderSizeWithAnyOverhead then
    begin
      LUsableBlockSize := LIndBlockSize - BlockHeaderSizeWithAnyOverhead
    end else
    begin
      LUsableBlockSize := 0;
    end;
    AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize := LUsableBlockSize;
  end;
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
    {Lock all small block types}
    LockAllSmallBlockTypes;
    {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
    LMediumBlocksLocked := True;
{$ENDIF}
    {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
  end;
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the medium block used space}
    Inc(AMemoryManagerState.ReservedMediumBlockAddressSpace, MediumBlockPoolSize);
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
      {Is the block in use?}
      if (LMediumBlockHeader and IsFreeBlockFlag) = 0 then
      begin
        {Get the block size}
        LMediumBlockSize := LMediumBlockHeader and DropMediumAndLargeFlagsMask;
        if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
        begin
          {Get the block type index}
          LBlockTypeIndex := (UIntPtr(PSmallBlockPoolHeader(LPMediumBlock)^.BlockType) - UIntPtr(@SmallBlockTypes[0]))
    {$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
          shr SmallBlockTypeRecSizePowerOf2
    {$ELSE}
          div SmallBlockTypeRecSize
    {$ENDIF}
          ;
          {Subtract from medium block usage}
          Dec(AMemoryManagerState.ReservedMediumBlockAddressSpace, LMediumBlockSize);
          {Add it to the reserved space for the block size}
          Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].ReservedAddressSpace, LMediumBlockSize);
          {Add the usage for the pool}
          Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].AllocatedBlockCount,
            PSmallBlockPoolHeader(LPMediumBlock)^.BlocksInUse);
        end
        else
        begin
{$IFDEF FullDebugMode}
          Dec(LMediumBlockSize, FullDebugBlockOverhead);
{$ENDIF}
          Inc(AMemoryManagerState.AllocatedMediumBlockCount);
          Inc(AMemoryManagerState.TotalAllocatedMediumBlockSize, LMediumBlockSize - BlockHeaderSize);
        end;
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
  end;
  {Unlock medium blocks}
{$IFNDEF AssumeMultiThreaded}
  if LMediumBlocksLocked then
{$ENDIF}
  begin
    // LMediumBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    UnlockMediumBlocks;
  end;
  {Unlock all the small block types}
  for LInd := 0 to NumSmallBlockTypes - 1 do
  begin
    ReleaseLockByte(SmallBlockTypes[LInd].SmallBlockTypeLocked);
  end;
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LLargeBlocksLocked := True;
{$ENDIF}
    {Step through all the large blocks}
    {$IFDEF LogLockContention}LDidSleep:={$ENDIF}
    LockLargeBlocks;
  end;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    LLargeBlockSize := LPLargeBlock^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    Inc(AMemoryManagerState.AllocatedLargeBlockCount);
    Inc(AMemoryManagerState.ReservedLargeBlockAddressSpace, LLargeBlockSize);
    Inc(AMemoryManagerState.TotalAllocatedLargeBlockSize, LPLargeBlock^.UserAllocatedSize);
    {Get the next large block}
    LPLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
  end;
{$IFNDEF AssumeMultiThreaded}
  if LLargeBlocksLocked then
{$ENDIF}
  begin
    // LLargeBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    UnlockLargeBlocks;
  end;
end;

{Returns a summary of the information returned by GetMemoryManagerState}
function GetMemoryManagerUsageSummary: TMemoryManagerUsageSummary;
var
  LMMS: TMemoryManagerState;
  LAllocatedBytes,
  LReservedBytes: NativeUInt;
  LSBTIndex: Integer;
begin
  {Get the memory manager state}
  GetMemoryManagerState(LMMS);
  {Add up the totals}
  LAllocatedBytes := LMMS.TotalAllocatedMediumBlockSize + LMMS.TotalAllocatedLargeBlockSize;
  LReservedBytes := LMMS.ReservedMediumBlockAddressSpace + LMMS.ReservedLargeBlockAddressSpace;
  for LSBTIndex := 0 to NumSmallBlockTypes - 1 do
  begin
    Inc(LAllocatedBytes, LMMS.SmallBlockTypeStates[LSBTIndex].UseableBlockSize
      * LMMS.SmallBlockTypeStates[LSBTIndex].AllocatedBlockCount);
    Inc(LReservedBytes, LMMS.SmallBlockTypeStates[LSBTIndex].ReservedAddressSpace);
  end;
  {Set the structure values}
  Result.AllocatedBytes := LAllocatedBytes;
  Result.OverheadBytes := LReservedBytes - LAllocatedBytes;
  if LReservedBytes > 0 then
    Result.EfficiencyPercentage := LAllocatedBytes / LReservedBytes * 100
  else
    Result.EfficiencyPercentage := 100;
end;

procedure GetMemoryManagerUsageSummary(var AMemoryManagerUsageSummary: TMemoryManagerUsageSummary);
begin
  AMemoryManagerUsageSummary := GetMemoryManagerUsageSummary;
end;

{$IFNDEF POSIX}
{Gets the state of every 64K block in the 4GB address space. Under 64-bit this
 returns only the state for the low 4GB.}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPLargeBlock: PLargeBlockHeader;
  LIndNUI,
  LChunkIndex,
  LNextChunk,
  LLargeBlockSize: NativeUInt;
  LMBI: TMemoryBasicInformation;
  LCharToFill: AnsiChar;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked: Boolean;
  LLargeBlocksLocked: Boolean;
{$ENDIF}
begin
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked := False;
  LLargeBlocksLocked := False;
{$ENDIF}
  {Clear the map}
  FillChar(AMemoryMap, SizeOf(AMemoryMap), Ord(csUnallocated));
  {Step through all the medium block pools}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LMediumBlocksLocked := True;
{$ENDIF}
    {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
  end;
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the medium block used space}
    LChunkIndex := NativeUInt(LPMediumBlockPoolHeader) shr 16;
    for LIndNUI := 0 to (MediumBlockPoolSize - 1) shr 16 do
    begin
      if (LChunkIndex + LIndNUI) > High(AMemoryMap) then
        Break;
      AMemoryMap[LChunkIndex + LIndNUI] := csAllocated;
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
  end;
{$IFNDEF AssumeMultiThreaded}
  if LMediumBlocksLocked then
{$ENDIF}
  begin
    // LMediumBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    UnlockMediumBlocks;
  end;
  {Step through all the large blocks}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LLargeBlocksLocked := True;
{$ENDIF}
    {$IFDEF LogLockContention}LDidSleep:={$ENDIF}
    LockLargeBlocks;
  end;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    LChunkIndex := UIntPtr(LPLargeBlock) shr 16;
    LLargeBlockSize := LPLargeBlock^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    for LIndNUI := 0 to (LLargeBlockSize - 1) shr 16 do
    begin
      if (LChunkIndex + LIndNUI) > High(AMemoryMap) then
        Break;
      AMemoryMap[LChunkIndex + LIndNUI] := csAllocated;
    end;
    {Get the next large block}
    LPLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
  end;
{$IFNDEF AssumeMultiThreaded}
  if LLargeBlocksLocked then
{$ENDIF}
  begin
    // LLargeBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    UnlockLargeBlocks;
  end;
  {Fill in the rest of the map}
  LIndNUI := 0;
  while LIndNUI <= 65535 do
  begin
    {If the chunk is not allocated by this MM, what is its status?}
    if AMemoryMap[LIndNUI] = csUnallocated then
    begin
      {Query the address space starting at the chunk boundary}
      if VirtualQuery(Pointer(LIndNUI * 65536), LMBI, SizeOf(LMBI)) = 0 then
      begin
        {VirtualQuery may fail for addresses >2GB if a large address space is
         not enabled.}
        LCharToFill := AnsiChar(csSysReserved);
        FillChar(AMemoryMap[LIndNUI], 65536 - LIndNUI, LCharToFill);
        Break;
      end;
      {Get the chunk number after the region}
      LNextChunk := ((LMBI.RegionSize - 1) shr 16) + LIndNUI + 1;
      {Validate}
      if LNextChunk > 65536 then
        LNextChunk := 65536;
      {Set the status of all the chunks in the region}
      if LMBI.State = MEM_COMMIT then
      begin
        LCharToFill := AnsiChar(csSysReserved);
        FillChar(AMemoryMap[LIndNUI], LNextChunk - LIndNUI, LCharToFill);
      end
      else
      begin
        if LMBI.State = MEM_RESERVE then
        begin
          LCharToFill := AnsiChar(csSysReserved);
          FillChar(AMemoryMap[LIndNUI], LNextChunk - LIndNUI, LCharToFill);
        end;
      end;
      {Point to the start of the next chunk}
      LIndNUI := LNextChunk;
    end
    else
    begin
      {Next chunk}
      Inc(LIndNUI);
    end;
  end;
end;
{$ENDIF}

{This function is a helper function neede when using the "typed @ operator"
to have lowest possible number of typecats - just in this function. It is defined ad
"inline", so, when optimization compiler directive is turned on, this function will
be implemented in such a way that no actual code will be needed and no call/return.}
function SmallBlockTypePtrToPoolHeaderPtr(ASmallBlockTypePtr: PSmallBlockType): PSmallBlockPoolHeader;
  {$IFDEF FASTMM4_ALLOW_INLINES}inline;{$ENDIF}
begin
  {This function just does one typecast to avoid typecasts elsewhere}
  Result := PSmallBlockPoolHeader(ASmallBlockTypePtr);
end;

{Returns summarised information about the state of the memory manager. (For
 backward compatibility.)}
function FastGetHeapStatus: THeapStatus;
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumBlock: Pointer;
  LBlockTypeIndex,
  LMediumBlockSize: Cardinal;
  LSmallBlockUsage,
  LSmallBlockOverhead,
  LMediumBlockHeader,
  LLargeBlockSize: NativeUInt;
  LInd: Integer;
  LPLargeBlock: PLargeBlockHeader;
{$IFDEF LogLockContention}
  LDidSleep: Boolean;
{$ENDIF}
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked: Boolean;
  LLargeBlocksLocked: Boolean;
{$ENDIF}
begin
{$IFNDEF AssumeMultiThreaded}
  LMediumBlocksLocked := False;
  LLargeBlocksLocked := False;
{$ENDIF}
  {Clear the structure}
  FillChar(Result, SizeOf(Result), 0);
  {Lock all small block types}
  LockAllSmallBlockTypes;
  {Lock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LMediumBlocksLocked := True;
{$ENDIF}
    {$IFDEF LogLockContention}LDidSleep := {$ENDIF}LockMediumBlocks;
  end;
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the total and committed address space}
    Inc(Result.TotalAddrSpace, ((MediumBlockPoolSize + $ffff) and $ffff0000));
    Inc(Result.TotalCommitted, ((MediumBlockPoolSize + $ffff) and $ffff0000));
    {Add the medium block pool overhead}
    Inc(Result.Overhead, (((MediumBlockPoolSize + $ffff) and $ffff0000)
      - MediumBlockPoolSize + MediumBlockPoolHeaderSize));
    {Get the first medium block in the pool}
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      {Get the block header}
      LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
      {Get the block size}
      LMediumBlockSize := LMediumBlockHeader and DropMediumAndLargeFlagsMask;
      {Is the block in use?}
      if (LMediumBlockHeader and IsFreeBlockFlag) = 0 then
      begin
        if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
        begin
          {Get the block type index}
          LBlockTypeIndex := (UIntPtr(PSmallBlockPoolHeader(LPMediumBlock)^.BlockType) - UIntPtr(@SmallBlockTypes[0]))
    {$IFDEF SmallBlockTypeRecSizeIsPowerOf2}
          shr SmallBlockTypeRecSizePowerOf2
    {$ELSE}
          div SmallBlockTypeRecSize
    {$ENDIF}
          ;
          {Get the usage in the block}
          LSmallBlockUsage := PSmallBlockPoolHeader(LPMediumBlock)^.BlocksInUse
            * SmallBlockTypes[LBlockTypeIndex].BlockSize;
          {Get the total overhead for all the small blocks}
          LSmallBlockOverhead := PSmallBlockPoolHeader(LPMediumBlock)^.BlocksInUse
              * (BlockHeaderSize{$IFDEF FullDebugMode} + FullDebugBlockOverhead{$ENDIF});
          {Add to the totals}
          Inc(Result.FreeSmall, LMediumBlockSize - LSmallBlockUsage - BlockHeaderSize);
          Inc(Result.Overhead, LSmallBlockOverhead + BlockHeaderSize);
          Inc(Result.TotalAllocated, LSmallBlockUsage - LSmallBlockOverhead);
        end
        else
        begin
{$IFDEF FullDebugMode}
          Dec(LMediumBlockSize, FullDebugBlockOverhead);
          Inc(Result.Overhead, FullDebugBlockOverhead);
{$ENDIF}
          {Add to the result}
          Inc(Result.TotalAllocated, LMediumBlockSize - BlockHeaderSize);
          Inc(Result.Overhead, BlockHeaderSize);
        end;
      end
      else
      begin
        {The medium block is free}
        Inc(Result.FreeBig, LMediumBlockSize);
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
  end;
  {Add the sequential feed unused space}
  Inc(Result.Unused, MediumSequentialFeedBytesLeft);
  {Unlock the medium blocks}
{$IFNDEF AssumeMultiThreaded}
  if LMediumBlocksLocked then
{$ENDIF}
  begin
    // LMediumBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    UnlockMediumBlocks;
  end;
  {Unlock all the small block types}
  for LInd := 0 to NumSmallBlockTypes - 1 do
  begin
    ReleaseLockByte(SmallBlockTypes[LInd].SmallBlockTypeLocked);
  end;
{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
{$IFNDEF AssumeMultiThreaded}
    LLargeBlocksLocked := True;
{$ENDIF}
    {Step through all the large blocks}
    {$IFDEF LogLockContention}LDidSleep:={$ENDIF}
    LockLargeBlocks;
  end;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    LLargeBlockSize := LPLargeBlock^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    Inc(Result.TotalAddrSpace, LLargeBlockSize);
    Inc(Result.TotalCommitted, LLargeBlockSize);
    Inc(Result.TotalAllocated, LPLargeBlock^.UserAllocatedSize
      {$IFDEF FullDebugMode} - FullDebugBlockOverhead{$ENDIF});
    Inc(Result.Overhead, LLargeBlockSize - LPLargeBlock^.UserAllocatedSize
      {$IFDEF FullDebugMode} + FullDebugBlockOverhead{$ENDIF});
    {Get the next large block}
    LPLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
  end;
{$IFNDEF AssumeMultiThreaded}
  if LLargeBlocksLocked then
{$ENDIF}
  begin
    // LLargeBlocksLocked := False; {this assignment produces a compiler "hint", but might have been useful for further development}
    UnlockLargeBlocks;
  end;
  {Set the total number of free bytes}
  Result.TotalFree := Result.FreeSmall + Result.FreeBig + Result.Unused;
end;

{$IFDEF fpc}
function FastGetFPCHeapStatus: TFPCHeapStatus; //support get TFPCHeapStatus
var
  HS: THeapStatus;
begin
  HS := FastGetHeapStatus;
  Result.MaxHeapSize  := HS.TotalAddrSpace;
  Result.MaxHeapUsed  := HS.TotalAllocated;
  Result.CurrHeapSize := HS.TotalAddrSpace;
  Result.CurrHeapUsed := HS.TotalAllocated;
  Result.CurrHeapFree := HS.TotalFree;
end;
{$ENDIF}

{Frees all allocated memory. Does not support segmented large blocks (yet).}
procedure FreeAllMemory;
var
  LPMediumBlockPoolHeader,
  LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumFreeBlock: PMediumFreeBlock;
  LPLargeBlock,
  LPNextLargeBlock: PLargeBlockHeader;
  LPSmallBlockPoolHeader: PSmallBlockPoolHeader; {This is needed for simplicity, to
												  mitigate typecasts when used "typed @".}
  LPSmallBlockType: PSmallBlockType;
  LInd: Integer;
begin
  {Free all block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Get the next medium block pool so long}
    LPNextMediumBlockPoolHeader := LPMediumBlockPoolHeader^.NextMediumBlockPoolHeader;
{$IFDEF ClearMediumBlockPoolsBeforeReturningToOS}
    FillChar(LPMediumBlockPoolHeader^, MediumBlockPoolSize, 0);
{$ELSE}
    {$IFDEF ClearSmallAndMediumBlocksInFreeMem}
    FillChar(LPMediumBlockPoolHeader^, MediumBlockPoolSize, 0);
    {$ENDIF}
{$ENDIF}
    {Free this pool}
    VirtualFree(LPMediumBlockPoolHeader, 0, MEM_RELEASE);
    {Next pool}
    LPMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
  end;
  {Clear all small block types}
  for LInd := Low(SmallBlockTypes) to High(SmallBlockTypes) do
  begin
    LPSmallBlockType := @(SmallBlockTypes[Lind]);
    LPSmallBlockPoolHeader := SmallBlockTypePtrToPoolHeaderPtr(LPSmallBlockType);
    SmallBlockTypes[Lind].PreviousPartiallyFreePool := LPSmallBlockPoolHeader;
    SmallBlockTypes[Lind].NextPartiallyFreePool := LPSmallBlockPoolHeader;
    SmallBlockTypes[Lind].NextSequentialFeedBlockAddress := Pointer(1);
    SmallBlockTypes[Lind].MaxSequentialFeedBlockAddress := nil;
  end;
  {Clear all medium block pools}
  MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  {All medium bins are empty}
  for LInd := Low(MediumBlockBins) to High(MediumBlockBins) do
  begin
    LPMediumFreeBlock := @(MediumBlockBins[LInd]);
    LPMediumFreeBlock^.PreviousFreeBlock := LPMediumFreeBlock;
    LPMediumFreeBlock^.NextFreeBlock := LPMediumFreeBlock;
  end;
  MediumBlockBinGroupBitmap := 0;
  FillChar(MediumBlockBinBitmaps, SizeOf(MediumBlockBinBitmaps), 0);
  MediumSequentialFeedBytesLeft := 0;
  {Free all large blocks}
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    {Get the next large block}
    LPNextLargeBlock := LPLargeBlock^.NextLargeBlockHeader;
{$IFDEF ClearLargeBlocksBeforeReturningToOS}
    FillChar(LPLargeBlock^,
      LPLargeBlock^.BlockSizeAndFlags and DropMediumAndLargeFlagsMask, 0);
{$ENDIF}
    {Free this large block}
    VirtualFree(LPLargeBlock, 0, MEM_RELEASE);
    {Next large block}
    LPLargeBlock := LPNextLargeBlock;
  end;
  {There are no large blocks allocated}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{Returns the current installation state of the memory manager.}
function FastMM_GetInstallationState: TFastMM_MemoryManagerInstallationState;
begin
  if IsMemoryManagerSet then
  begin
    if FastMMIsInstalled then
    begin
      if IsMemoryManagerOwner then
        Result := mmisInstalled
      else
        Result := mmisUsingSharedMemoryManager;
    end
    else
      Result := mmisOtherThirdPartyMemoryManagerInstalled
  end
  else
    Result := mmisDefaultMemoryManagerInUse;
end;

{$IFDEF LogLockContention}
procedure ReportLockContention;
var
  count: Integer;
  data: {$IFDEF UNICODE}TStaticCollector.{$ENDIF}TCollectedData;
  i: Integer;
  LErrorMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
  LMessageTitleBuffer: array[0..MaxDisplayMessageLength-1] of AnsiChar;
  LMsgPtr, LInitialPtr: PAnsiChar;
  LInitialSize: Cardinal;
  mergedCount: Integer;
  mergedData: {$IFDEF UNICODE}TStaticCollector.{$ENDIF}TCollectedData;
begin
  LargeBlockCollector.GetData(mergedData, mergedCount);
  MediumBlockCollector.GetData(data, count);
  LargeBlockCollector.Merge(mergedData, mergedCount, data, count);
  for i := 0 to High(SmallBlockTypes) do
  begin
    SmallBlockTypes[i].BlockCollector.GetData(data, count);
    LargeBlockCollector.Merge(mergedData, mergedCount, data, count);
  end;

  if mergedCount > 0 then
  begin
    FillChar(LErrorMessage, SizeOf(LErrorMessage), 0);
    FillChar(LMessageTitleBuffer, SizeOf(LMessageTitleBuffer), 0);
    LMsgPtr := @LErrorMessage[0];
    LInitialPtr := LMsgPtr;
    LInitialSize := MaxLogMessageLength;
    LMsgPtr := AppendStringToBuffer(LockingReportHeader, LMsgPtr, Length(LockingReportHeader), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
    LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
    LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
    for i := 1 to 3 do
    begin
      if i > mergedCount then
        break; //for i
      if i > 1 then
        LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
      LMsgPtr := NativeUIntToStrBuf(mergedData[i].Count, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
      if LInitialSize-NativeUInt(LMsgPtr-LInitialPtr) < 5 then Break;
      LMsgPtr^ := ' ';
      Inc(LMsgPtr);
      LMsgPtr^ := 'x';
      Inc(LMsgPtr);
      LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
      LMsgPtr := LogStackTrace(PNativeUInt(@(mergedData[i].Data.Pointers[1])), mergedData[i].Data.Count, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
    end;
{$IFNDEF NoMessageBoxes}
    FillChar(LMessageTitleBuffer, SizeOf(LMessageTitleBuffer), 0);
    AppendStringToModuleName(LockingReportTitle, LMessageTitleBuffer, Length(LockingReportTitle), (SizeOf(LMessageTitleBuffer) div SizeOf(LMessageTitleBuffer[0]))-1);
    ShowMessageBox(LErrorMessage, LMessageTitleBuffer);
{$ENDIF}
      for i := 4 to 10 do
      begin
        if i > mergedCount then
          break; //for i
        LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
        LMsgPtr := NativeUIntToStrBuf(mergedData[i].Count, LMsgPtr, LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
        if LInitialSize-NativeUInt(LMsgPtr-LInitialPtr) < 5 then Break;
        LMsgPtr^ := ' ';
        Inc(LMsgPtr);
        LMsgPtr^ := 'x';
        Inc(LMsgPtr);
        LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
        LMsgPtr := LogStackTrace(PNativeUInt(@(mergedData[i].Data.Pointers[1])), mergedData[i].Data.Count, LMsgPtr);
        LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
      end;
      LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF), LInitialSize-NativeUInt(LMsgPtr-LInitialPtr));
      AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
      end;
  end;
{$ENDIF}

{$IFDEF UseReleaseStack}
{$IFDEF DebugReleaseStack}
procedure GetBlockSizeForStack(const AStack: TLFStack; var ABlockSize: NativeUInt; var ACount: integer);
var
  LBlockHeader: NativeUInt;
  LMemBlock: pointer;
  LTmpStack: TLFStack;
begin
  ABlockSize := 0;
  ACount := 0;
  LTmpStack.Initialize(ReleaseStackSize, SizeOf(pointer));
  while AStack.Pop(LMemBlock) do
  begin
    {Move each block to a temporary stack as we'll have to put them back later}
    LTmpStack.Push(LMemBlock);

    Inc(ACount);

    LBlockHeader := PNativeUInt(PByte(LMemBlock) - BlockHeaderSize)^;

    {Block should always be in use!}
    if (LBlockHeader and IsFreeBlockFlag) <> 0 then
    begin
      {$IFDEF BCB6OrDelphi7AndUp}
      System.Error(reInvalidPtr);
      {$ELSE}
      System.RunError(reInvalidPtr);
      {$ENDIF}
    end
    {Is this a medium block?}
    else if (LBlockHeader and IsMediumBlockFlag) <> 0 then
      Inc(ABlockSize, LBlockHeader and DropMediumAndLargeFlagsMask)
    {Is this a large block?}
    else if (LBlockHeader and IsLargeBlockFlag) <> 0 then
      Inc(ABlockSize, PLargeBlockHeader(Pointer(PByte(LMemBlock) - LargeBlockHeaderSize)).UserAllocatedSize)
    {It must be a small block}
    else
      Inc(ABlockSize, PSmallBlockPoolHeader(LBlockHeader).BlockType.BlockSize);
  end;

  {Cleanup, move memory blocks back to the release stack}
  while LTmpStack.Pop(LMemBlock) do
    AStack.Push(LMemBlock);
  LTmpStack.Finalize;
end;

procedure LogReleaseStackUsage;

  procedure NewLine;
  begin
    LMsgPtr^ := #13; Inc(LMsgPtr);
    LMsgPtr^ := #10; Inc(LMsgPtr);
  end;

  procedure AppendMemorySize(ASize: NativeUInt);
  begin
    if ASize < 10*1024 then
    begin
      LMsgPtr := NativeUIntToStrBuf(Round(ASize/1024), LMsgPtr);
      LMsgPtr^ := ' '; Inc(LMsgPtr);
      LMsgPtr^ := 'K'; Inc(LMsgPtr);
      LMsgPtr^ := 'B'; Inc(LMsgPtr);
    end
    else if ASize < 10*1024*1024 then
    begin
      LMsgPtr := NativeUIntToStrBuf(Round(ASize/1024), LMsgPtr);
      LMsgPtr^ := ' '; Inc(LMsgPtr);
      LMsgPtr^ := 'K'; Inc(LMsgPtr);
      LMsgPtr^ := 'B'; Inc(LMsgPtr);
    end
    else if (ASize div 1024) < 10*1024*1024 then
    begin
      LMsgPtr := NativeUIntToStrBuf(Round(ASize/1024/1024), LMsgPtr);
      LMsgPtr^ := ' '; Inc(LMsgPtr);
      LMsgPtr^ := 'M'; Inc(LMsgPtr);
      LMsgPtr^ := 'B'; Inc(LMsgPtr);
    end
    else
    begin
      LMsgPtr := NativeUIntToStrBuf(Round(ASize/1024/1024/1024), LMsgPtr);
      LMsgPtr^ := ' '; Inc(LMsgPtr);
      LMsgPtr^ := 'G'; Inc(LMsgPtr);
      LMsgPtr^ := 'B'; Inc(LMsgPtr);
    end;
  end;

  procedure AppendSlotInfo(ABlockSize: Integer);
  var
    LCount: Integer;
    LSlot: Integer;
    LTotal: NativeUInt;
  begin
    if ABlockSize > 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ReleaseStackUsageSmallBlocksMsg1, LMsgPtr, Length(ReleaseStackUsageSmallBlocksMsg1));
      LMsgPtr := NativeUIntToStrBuf(ABlockSize, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(ReleaseStackUsageSmallBlocksMsg2, LMsgPtr, Length(ReleaseStackUsageSmallBlocksMsg2));
    end
    else if ABlockSize = -1 then
      LMsgPtr := AppendStringToBuffer(ReleaseStackUsageMediumBlocksMsg, LMsgPtr, Length(ReleaseStackUsageMediumBlocksMsg))
    else
      LMsgPtr := AppendStringToBuffer(ReleaseStackUsageLargeBlocksMsg, LMsgPtr, Length(ReleaseStackUsageLargeBlocksMsg));

    LTotal := 0;
    LCount := 0;
    for LSlot := 0 to NumStacksPerBlock-1 do
    begin
      Inc(LTotal, LSlotSize[LSlot]);
      Inc(LCount, LSlotCount[LSlot]);
    end;

    AppendMemorySize(LTotal);
    LMsgPtr := AppendStringToBuffer(ReleaseStackUsageBuffers1Msg, LMsgPtr, Length(ReleaseStackUsageBuffers1Msg));
    LMsgPtr := NativeUIntToStrBuf(LCount, LMsgPtr);
    LMsgPtr := AppendStringToBuffer(ReleaseStackUsageBuffers2Msg, LMsgPtr, Length(ReleaseStackUsageBuffers2Msg));
    for LSlot := 0 to NumStacksPerBlock-1 do
    begin
      AppendMemorySize(LSlotSize[LSlot]);
      LMsgPtr^ := '/';
      Inc(LMsgPtr);
      LMsgPtr := NativeUIntToStrBuf(LSlotCount[LSlot], LMsgPtr);
      if LSlot < (NumStacksPerBlock-1) then
      begin
        LMsgPtr^ := ' ';
        Inc(LMsgPtr);
      end;
    end;
    LMsgPtr^ := ']';
    Inc(LMsgPtr);

    NewLine;
  end;

var
  LCount: integer;
  LInd: Integer;
  LMessage: array[0..MaxLogMessageLength-1] of AnsiChar;
  LMsgPtr: PAnsiChar;
  LSize: NativeUInt;
  LSlot: Integer;
  LSlotCount: array[0..NumStacksPerBlock-1] of integer;
  LSlotSize: array[0..NumStacksPerBlock-1] of NativeUInt;
  LTotalLarge: NativeUInt;
  LTotalMedium: NativeUInt;
  LTotalSmall: NativeUInt;
  LMediumBlocksLocked: Boolean;
  LLargeBlocksLocked: Boolean;
begin
  LMsgPtr := AppendStringToBuffer(ReleaseStackUsageHeader, @LMessage[0], Length(ReleaseStackUsageHeader));
  NewLine;
  NewLine;

{$IFNDEF AssumeMultiThreaded}
  if IsMultiThread then
{$ENDIF}
  begin
    LSmallBlocksLocked := True;
    LockAllSmallBlockTypes;
    LMediumBlocksLocked := True;
    LockMediumBlocks;
    LLargeBlocksLocked := True;
    LockLargeBlocks;
  end;

  LTotalSmall := 0;
  for LInd := 0 to High(SmallBlockTypes) do begin
    for LSlot := 0 to NumStacksPerBlock-1 do begin
      GetBlockSizeForStack(SmallBlockTypes[LInd].ReleaseStack[LSlot], LSize, LCount);
      LSlotSize[LSlot] := LSize;
      LSlotCount[LSlot] := LCount;
      Inc(LTotalSmall, LSize);
    end;
    if LSmallBlocksLocked then
    begin
      ReleaseLockByte(@SmallBlockTypes[LInd].SmallBlockTypeLocked);
    end;
    AppendSlotInfo(SmallBlockTypes[LInd].BlockSize);
  end;

  LMsgPtr := AppendStringToBuffer(ReleaseStackUsageTotalSmallBlocksMsg, LMsgPtr, Length(ReleaseStackUsageTotalSmallBlocksMsg));
  AppendMemorySize(LTotalSmall);
  NewLine;

  LTotalMedium := 0;
  for LSlot := 0 to NumStacksPerBlock-1 do begin
    GetBlockSizeForStack(MediumReleaseStack[LSlot], LSize, LCount);
    LSlotSize[LSlot] := LSize;
    LSlotCount[LSlot] := LCount;
    Inc(LTotalMedium, LSize);
  end;
  if LMediumBlocksLocked then
  begin
    LMediumBlocksLocked := False;
    UnlockMediumBlocks;
  end;
  AppendSlotInfo(-1);

  LTotalLarge := 0;
  for LSlot := 0 to NumStacksPerBlock-1 do begin
    GetBlockSizeForStack(LargeReleaseStack[LSlot], LSize, LCount);
    LSlotSize[LSlot] := LSize;
    LSlotCount[LSlot] := LCount;
    Inc(LTotalLarge, LSize);
  end;
  if LLargeBlocksLocked then
  begin
    LLargeBlocksLocked := False;
    UnlockLargeBlocks;
  end;
  AppendSlotInfo(-2);

  LMsgPtr := AppendStringToBuffer(ReleaseStackUsageTotalMemoryMsg, LMsgPtr, Length(ReleaseStackUsageTotalMemoryMsg));
  AppendMemorySize(LTotalSmall + LTotalMedium + LTotalLarge);
  NewLine;

  {Trailing #0}
  LMsgPtr^ := #0;

    AppendEventLog(@LMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LMessage[0]));
end;
{$ENDIF}
{$ENDIF}

{----------------------------Memory Manager Setup-----------------------------}

{$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
const
  // constants from the Windows SDK v10.0.15063
  XSTATE_LEGACY_FLOATING_POINT        = (0);
  XSTATE_LEGACY_SSE                   = (1);
  XSTATE_GSSE                         = (2);
  XSTATE_AVX                          = (XSTATE_GSSE);
  XSTATE_MPX_BNDREGS                  = (3);
  XSTATE_MPX_BNDCSR                   = (4);
  XSTATE_AVX512_KMASK                 = (5);
  XSTATE_AVX512_ZMM_H                 = (6);
  XSTATE_AVX512_ZMM                   = (7);
  XSTATE_IPT                          = (8);
  XSTATE_LWP                          = (62);
  MAXIMUM_XSTATE_FEATURES             = (64);

const
  cXstateAvx1Mask                     = (1 shl XSTATE_AVX);
  {$IFDEF EnableAVX512}
  cXstateAvx512Mask                   = (1 shl XSTATE_AVX512_KMASK) or (1 shl XSTATE_AVX512_ZMM_H) or (1 shl XSTATE_AVX512_ZMM);
  {$ENDIF}

{$ENDIF Use_GetEnabledXStateFeatures_WindowsAPICall}

{Use the NativeUint argument type to make Delphi clear the trash and not pass
it in bits 63-32 under 64-bit, although the xgetbv instruction only accepts
32-bits from the ECX/RCX register even under 64-bit mode}

{$IFDEF 64bit}
  {$IFNDEF FPC}
  { The following compilers do not understand the XGETBV instruction:
    - The 32-bit Delphi Tokyo 10.2 assembler;
    - FreePascal
  }
    {$IFDEF ASMVersion}
      {$define XGetBvAsmSupported}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF PurePascal}
function GetCpuXCR(Arg: NativeUint): Int64; assembler;
asm
 {$IFDEF 64bit}

{$IFDEF unix}

{Under Unix 64-bit, the first six integer or pointer arguments are passed
in registers RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9.
The return value is stored in RAX and RDX.
So Unix uses the same register for return value as Microsoft; don't correct
output registers, but correct the input one}
   mov    ecx, edi // this will also clear the highest bits in ecx (63-32).
{$ELSE}
{$IFDEF AllowAsmNoframe}
   .noframe
{$ENDIF}
{$ENDIF}
   xor   eax, eax
   xor   edx, edx
{ EDX:EAX <- XCR[ECX]; }

{$IFDEF XGetBvAsmSupported}
  xgetbv
{$ELSE}
  db $0F, $01, $D0
{$ENDIF}

{The output of xgetbv is a 64-bit value returned in two 32-bit registers:
eax/edx, even in 64-bit mode, so we should pack eax/edx intto rax}

   shl   rdx, 32
   or    rax, rdx
   xor   rdx, rdx

 {$ELSE}
   mov   ecx, eax
   xor   eax, eax
   xor   edx, edx
   {$IFDEF XGetBvAsmSupported}
     xgetbv
   {$ELSE}
     db $0F, $01, $D0
   {$ENDIF}
 {$ENDIF}
end;
{$ENDIF}

{Checks that no other memory manager has been installed after the RTL MM and
 that there are currently no live pointers allocated through the RTL MM.}
function CheckCanInstallMemoryManager: Boolean;
{$IFNDEF NoMessageBoxes}
var
  LErrorMessageTitle: array[0..MaxDisplayMessageLength-1] of AnsiChar;
{$ENDIF}
{$IFNDEF POSIX}
var
  HeapTotalAllocated: NativeUInt;
{$ENDIF}
begin
  {Default to error}
  Result := False;
{$IFDEF FullDebugMode}
  {$IFDEF LoadDebugDLLDynamically}
    {$IFDEF DoNotInstallIfDLLMissing}
  {Should FastMM be installed only if the FastMM_FullDebugMode.dll file is
   available?}
  if ( FullDebugModeDLL = 0 )
      {$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically ) AND Defined( MemoryLoadLibrarySupport )}
      AND ( MemoryResourceExists( {$IFNDEF 64Bit}'FastMM_FullDebugMode'{$ELSE}'FastMM_FullDebugMode64'{$ENDIF} ) = 0 ) 
      {$IFEND} then
    Exit;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  {Is FastMM already installed?}
  if FastMMIsInstalled then
  begin
{$IFDEF UseOutputDebugString}
    OutputDebugStringA(AlreadyInstalledMsg);
{$ENDIF}
{$IFNDEF NoMessageBoxes}
    FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
    AppendStringToModuleName(AlreadyInstalledTitle, LErrorMessageTitle, Length(AlreadyInstalledTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
    ShowMessageBox(AlreadyInstalledMsg, LErrorMessageTitle);
{$ENDIF}
    Exit;
  end;
  {Has another MM been set, or has the Embarcadero MM been used? If so, this
   file is not the first unit in the uses clause of the project's .dpr file.}

  if IsMemoryManagerSet then
  begin
    {When using runtime packages, another library may already have installed
     FastMM: Silently ignore the installation request.}
{$IFNDEF UseRuntimePackages}
    {Another memory manager has been set.}
  {$IFDEF UseOutputDebugString}
    OutputDebugStringA(OtherMMInstalledMsg);
  {$ENDIF}
  {$IFNDEF NoMessageBoxes}
    FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
    AppendStringToModuleName(OtherMMInstalledTitle, LErrorMessageTitle, Length(OtherMMInstalledTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
    ShowMessageBox(OtherMMInstalledMsg, LErrorMessageTitle);
  {$ENDIF}
{$ENDIF}
    Exit;
  end;

{$IFNDEF POSIX}
  HeapTotalAllocated := GetHeapStatus.TotalAllocated;
{ In FreePascal, we cannot rely on HeapTotalAllocated to check whether FastMM4
is the first unit and no memory have been allocated before, by another memory
manager, because the initialization section of the "system.pp" unit of
FreePascal calls the setup_arguments function to allocate memory for the
command line buffers and store these pointers in the "argc" global variable
(checked in versions 3.0.4 and 3.2.0), but version 3.3.1 allocates even more
memory in the initialization of "system.pp".
See https://bugs.freepascal.org/view.php?id=38391 for more details.
Please double-check that the FastMM4 unit is the first unit in the units ("uses")
list of your .lpr file (or any other main file where you define project
units). }
{$IFNDEF IgnoreMemoryAllocatedBefore}
  if HeapTotalAllocated <> 0 then
  begin
    {Memory has been already been allocated with the RTL MM}
{$IFDEF UseOutputDebugString}
    OutputDebugStringA(MemoryAllocatedMsg);
{$ENDIF}
  {$IFNDEF NoMessageBoxes}
    FillChar(LErrorMessageTitle, SizeOf(LErrorMessageTitle), 0);
    AppendStringToModuleName(MemoryAllocatedTitle, LErrorMessageTitle, Length(MemoryAllocatedTitle), (SizeOf(LErrorMessageTitle) div SizeOf(LErrorMessageTitle[0]))-1);
    {$IFDEF FPC}
    ShowMessageBox('In FreePascal, we cannot rely on HeapTotalAllocated to check '+
    'whether FastMM4 is the first unit and no memory has been allocated before, '+
    'by another memory manager, because the initialization section of the "system.pp" '+
    'unit of FreePascal calls the setup_arguments function to allocate memory for the command line buffers and store these pointers in the "argc" global variable (checked in versions 3.0.4 and 3.2.0). However, the version 3.3.1 allocates even more memory in the initialization of "system.pp". See https://bugs.freepascal.org/view.php?id=38391 for more details. Please double-check that the FastMM4 unit is the first unit in the units ("uses") list of your .lpr file (or any other main file where you define project units). You can recompile FastMM4-AVX with the IgnoreMemoryAllocatedBefore conditional define, but, in this case, there will be no check whether the FastMM4 is the first unit in the units section, and if it is not the first, you will get errors. Please consider supporting the https://bugs.freepascal.org/view.php?id=38391 and/or improving FreePascal to fix the bug registered under that URL.', LErrorMessageTitle);    {$ELSE}
    ShowMessageBox(MemoryAllocatedMsg, LErrorMessageTitle);
    {$ENDIF}
  {$ENDIF}
    Exit;
  end;
{$ENDIF}
{$ENDIF}
  {All OK}
  Result := True;
end;

procedure InitializeInvalidMemoryManager;
begin
{$IFDEF DetectMMOperationsAfterUninstall}
  with InvalidMemoryManager do
  begin
    GetMem :=  {$IFDEF FPC}@{$ENDIF}InvalidGetMem;
    FreeMem := {$IFDEF FPC}@{$ENDIF}InvalidFreeMem;
    ReallocMem := {$IFDEF FPC}@{$ENDIF}InvalidReallocMem;
  {$IFDEF BDS2006AndUp}
    AllocMem := {$IFDEF FPC}@{$ENDIF}InvalidAllocMem;
    RegisterExpectedMemoryLeak := {$IFDEF FPC}@{$ENDIF}InvalidRegisterAndUnRegisterMemoryLeak;
    UnRegisterExpectedMemoryLeak := {$IFDEF FPC}@{$ENDIF}InvalidRegisterAndUnRegisterMemoryLeak;
  {$ENDIF}
  end;
{$ENDIF}
end;

procedure InitializeBlockTypeSizes;
var
  i: Cardinal;
begin
  for i := 0 to NumSmallBlockTypes-1 do
  begin
    SmallBlockTypes[i].BlockSize := SmallBlockTypeSizes[i];
  end;
end;

{Initializes the lookup tables for the memory manager}
procedure InitializeMemoryManager;
{$IFDEF FullDebugMode}
const
  {The size of the Inc(VMTIndex) code in TFreedObject.GetVirtualMethodIndex}
  VMTIndexIncCodeSize = 6;
{$ENDIF}

{$IFDEF EnableAVX}

const
  {XCR0[2:1] = '11b' (XMM state and YMM state are enabled by OS).}
  CXcrXmmAndYmmMask = (4-1) shl 1;

{$IFDEF EnableAVX512}
const
  {XCR0[7:5] = '111b' (OPMASK state, upper 256-bit of ZMM0-ZMM15 and ZMM16-ZMM31 state are enabled by OS).}
  CXcrZmmMask       = (8-1) shl 5;
{$ENDIF EnableAVX512}

{$ENDIF EnableAVX}

{$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
type
  TGetEnabledXStateFeatures = function: Int64; stdcall;
{$ENDIF}

var
  LPSmallBlockPoolHeader: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
{$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
  FGetEnabledXStateFeatures: TGetEnabledXStateFeatures;
  EnabledXStateFeatures: Int64;
{$ENDIF}

{$IFDEF USE_CPUID}
{$IFDEF EnableAVX}
  CpuXCR0: Int64;
{$ENDIF}
  MaxInputValueBasic: Cardinal;
  LReg0, LReg1, LReg5, LReg7_0: TCpuIdRegisters;
{$ENDIF}

  LInd,
  LSizeInd,
  LMinimumPoolSize,
  LOptimalPoolSize,
  LGroupNumber,
  LBlocksPerPool, LPreviousBlockSize: Cardinal;
  LPMediumFreeBlock: PMediumFreeBlock;
{$IFDEF FullDebugMode}
  {$IFDEF LoadDebugDLLDynamically}
    {$IFDEF RestrictDebugDLLLoadPath}
    LModuleHandle: HModule;
    LFullFileName: array[0..MaxFileNameLengthDouble-1] of Char;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF UseReleaseStack}
  LSlot: Integer;
{$ENDIF}
  LByte: Byte;
  {$IFNDEF DisablePauseAndSwitchToThread}
  {$IFNDEF POSIX}
  LProcAddr: Pointer;
  {$ENDIF}
  {$ENDIF}
begin

{$IFNDEF DisablePauseAndSwitchToThread}
{$IFNDEF POSIX}
  LProcAddr := GetProcAddress(GetModuleHandle(Kernel32), 'SwitchToThread');
  FSwitchToThread := TSwitchToThread(LProcAddr);
{$ENDIF}
{$ENDIF}

{$IFDEF FullDebugMode}
  {$IFDEF LoadDebugDLLDynamically}
  {Attempt to load the FullDebugMode DLL dynamically.}
  FastMM_LoadDebugSupportLibrary;
  {$ENDIF}
{$ENDIF}


{$IFDEF USE_CPUID}
  if CPUID_Supported then
  begin

{
QUOTE

Two types of information are returned: basic and extended function information. If a value entered for CPUID.EAX
is higher than the maximum input value for basic or extended function for that processor then the data for the
highest basic information leaf is returned.

ENDQOTE}


//Basic CPUID Information

    with LReg0   do begin RegEAX := 0; RegEBX := 0; RegECX := 0; RegEDX := 0; end;
    with LReg1   do begin RegEAX := 0; RegEBX := 0; RegECX := 0; RegEDX := 0; end;
    with LReg5   do begin RegEAX := 0; RegEBX := 0; RegECX := 0; RegEDX := 0; end;
    with LReg7_0 do begin RegEAX := 0; RegEBX := 0; RegECX := 0; RegEDX := 0; end;

    GetCPUID(0, 0, LReg0);
    MaxInputValueBasic := LReg0.RegEax;
    if MaxInputValueBasic > 0 then
    begin
      if MaxInputValueBasic > 7 then
      begin
        GetCPUID(7, 0, LReg7_0);
      end;

{$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}

{For best results, we should call the GetEnabledXStateFeatures Windows API function
that gets a mask of enabled XState features on x86 or x64 processors.
This function is implemented starting from Windows 7, so we should use GetProcAddress
Not all features supported by a processor may be enabled on the system.
Using a feature which is not enabled may result in exceptions or undefined behavior.
This is because the operating system would not save the registers and the states between switches.
}

      FGetEnabledXStateFeatures:= GetProcAddress(GetModuleHandle(Kernel32),
        'GetEnabledXStateFeatures');
      if Assigned(FGetEnabledXStateFeatures) then
      begin
        EnabledXStateFeatures := FGetEnabledXStateFeatures;
      end else
      begin
        EnabledXStateFeatures :=
          (UnsignedBit shl XSTATE_LEGACY_FLOATING_POINT) or
          (UnsignedBit shl XSTATE_LEGACY_SSE);
      end;
{$ENDIF}

      GetCPUID(1, 0, LReg1);

      if
        ((LReg1.RegEDX and (UnsignedBit shl 26)) <> 0) {SSE2 bit}
      then
      begin
        {If we have SSE2 bit set in the CPUID, than we have the PAUSE
        instruction supported, we don't have to check for XState/CR0 for PAUSE,
        because PAUSE and other instructions like PREFETCHh, MOVNTI, etc.
        work regardless of the CR0 values}

        {$IFNDEF DisablePauseAndSwitchToThread}
        {$IFNDEF POSIX}
        if Assigned(FSwitchToThread) then
        {$ENDIF}
        begin
          {$IFNDEF AssumePauseAndSwitchToThreadAvailable}
          FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeaturePauseAndSwitch;
          {$ENDIF DisablePauseAndSwitchToThread}
        end;
        {$ENDIF}
      end;

{$IFDEF EnableMMX}
      if
        ((LReg1.RegEDX and (UnsignedBit shl 23)) <> 0)
{$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
        and ((EnabledXStateFeatures and (UnsignedBit shl XSTATE_LEGACY_SSE)) <> 0)
{$ENDIF}
      then
      begin
        FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureMMX;
      end;
{$ENDIF EnableMMX}

{$IFDEF 32bit}
      if
        ((LReg1.RegEDX and (UnsignedBit shl 25)) <> 0)
  {$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
        and ((EnabledXStateFeatures and (UnsignedBit shl XSTATE_LEGACY_SSE)) <> 0)
  {$ENDIF}
      then
      begin
  {$IFDEF 32bit_SSE}
        FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureSSE;
  {$ENDIF}
      end;
{$ENDIF 32bit}

{ Here is the Intel algorithm to detext AVX }
{ QUOTE from the Intel 64 and IA-32 Architectures Optimization Reference Manual
1) Detect CPUID.1:ECX.OSXSAVE[bit 27] = 1 (XGETBV enabled for application use1)
2) Issue XGETBV and verify that XCR0[2:1] = '11b' (XMM state and YMM state are enabled by OS).
3) detect CPUID.1:ECX.AVX[bit 28] = 1 (AVX instructions supported).
ENDQUOTE}

      {$IFDEF EnableAVX}
      if
        ((LReg1.RegECX and (UnsignedBit shl 27)) <> 0) {OSXSAVE bit} then
      begin
        CpuXCR0 := GetCpuXCR(0);
      end else
      begin
        CpuXCR0 := 0;
      end;
      {$ENDIF}

      {$IFDEF EnableAVX}
      if
         {verify that XCR0[2:1] = '11b' (XMM state and YMM state are enabled by OS).}
         (CpuXCR0 and CXcrXmmAndYmmMask = CXcrXmmAndYmmMask) and

         {verify that CPUID.1:ECX.AVX[bit 28] = 1 (AVX instructions supported)}
         ((LReg1.RegECX and (UnsignedBit shl 28)) <> 0) {AVX bit}

      {$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
         and ((EnabledXStateFeatures and (cXstateAvx1Mask) = cXstateAvx1Mask))
      {$ENDIF}

      then
      begin
        FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureAVX1;
      end;

      if (FastMMCpuFeaturesA and FastMMCpuFeatureAVX1 <> 0) then
      begin
      { Application Software must identify that hardware supports AVX, after that it must also detect support for AVX2 by
        checking CPUID.(EAX=07H, ECX=0H):EBX.AVX2[bit 5].}
        if (MaxInputValueBasic > 7) and
            ((LReg7_0.RegEBX and (UnsignedBit shl 5))<> 0) then
        begin
          FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureAVX2;

          // check for AVX-512
        {$IFDEF EnableAVX512}
          if
          ((CpuXCR0 and CXcrZmmMask) = CXcrZmmMask) and
          { Processor support of AVX-512 Foundation instructions is indicated by CPUID.(EAX=07H, ECX=0):EBX.AVX512F[bit16] = 1}
          ((LReg7_0.RegEBX and (1 shl 16)) <> 0)
        {$IFDEF Use_GetEnabledXStateFeatures_WindowsAPICall}
            and ((EnabledXStateFeatures and cXstateAvx512Mask) = cXstateAvx512Mask)
        {$ENDIF}
          then
          begin
            FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureAVX512;
          end;
        {$ENDIF}

        end;
      end;
      {$ENDIF EnableAVX}

      {$IFDEF EnableERMS}
      if (MaxInputValueBasic > 7) and
{EBX: Bit 09: Supports Enhanced REP MOVSB/STOSB if 1.}
      ((LReg7_0.RegEBX and (UnsignedBit shl 9))<> 0) then
      begin
        FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureERMS;
      end;
      {$ENDIF EnableERMS}

      {$IFDEF EnableFSRM}
      if (MaxInputValueBasic > 7) and
{EDX: Bit 04: Supports Fast Short REP MOVSB if 1.}
      ((LReg7_0.RegEDX and (UnsignedBit shl 4)) <> 0) then
      begin
        FastMMCpuFeaturesA := FastMMCpuFeaturesA or FastMMCpuFeatureFSRM;
      end;
      {$ENDIF}

      {$IFDEF EnableWaitPKG}
      if (MaxInputValueBasic > 7) and
{ECX: Bit 05: WAITPKG (supports UMONITOR/UMWAIT)}
      ((LReg7_0.RegECX and (UnsignedBit shl 5)) <> 0) then
      begin
        FastMMCpuFeaturesB := FastMMCpuFeaturesB or FastMMCpuFeatureB_WaitPKG;
        GetCPUID(5, 0, LReg5);
        FastMMCpuSmallestMonitorLineSize := Word(LReg5.RegEAX and $FFFF);
        FastMMCpuLargestMonitorLineSize := Word(LReg5.RegEBX and $FFFF);
      end;
      {$ENDIF}

    end;

  end;
{$ENDIF}

  {Initialize the memory manager}
  {-------------Set up the small block types-------------}

  {$IFDEF SmallBlocksLockedCriticalSection}
  if not CpuFeaturePauseAndSwitch then
  begin
    for LInd := Low(SmallBlockCriticalSections) to High(SmallBlockCriticalSections) do
    begin
      {$IFDEF fpc}InitCriticalSection{$ELSE}InitializeCriticalSection{$ENDIF}(SmallBlockCriticalSections[LInd]);
    end;
  end;
  {$ENDIF}

  InitializeInvalidMemoryManager;

  InitializeBlockTypeSizes;

  LPreviousBlockSize := 0;

  for LInd := 0 to High(SmallBlockTypes) do
  begin
    SmallBlockTypes[LInd].SmallBlockTypeLocked := CLockByteAvailable;



{$IFDEF UseCustomFixedSizeMoveRoutines}

    {Set the move procedure}

    {The upsize move procedure may move chunks in 16 bytes even with 8-byte
    alignment, since the new size will always be at least 8 bytes bigger than
    the old size.}


    {$IFDEF 32bit_SSE}
    {$IFNDEF unix}
    {$IFDEF USE_CPUID}
    // if we have SSE, use SSE copy
    // even if we have Fast Short REP MOVSB, it is not as fast for sizes below 92 bytes under 32-bit
    if ((FastMMCpuFeaturesA and FastMMCpuFeatureSSE) <> 0) then
    begin
      case SmallBlockTypes[LInd].BlockSize of
        24: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move20_32bit_SSE;
        32: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move28_32bit_SSE;
        40: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move36_32bit_SSE;
        48: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move44_32bit_SSE;
        56: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move52_32bit_SSE;
        64: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move60_32bit_SSE;
        72: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move68_32bit_SSE;
        80: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move76_32bit_SSE;
        88: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move84_32bit_SSE;
        96: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move92_32bit_SSE;
      end;
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}

    if not Assigned(SmallBlockTypes[LInd].UpsizeMoveProcedure) then
    begin
      case SmallBlockTypes[LInd].BlockSize of
        {$IFDEF 32bit}
        8: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move4;
        {$ENDIF}
        {$IFNDEF Align32Bytes}
        16: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32Bit}Move12{$ELSE}Move8{$ENDIF};
        {$IFNDEF Align16Bytes}
        24: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32bit}Move20{$ELSE}Move16{$ENDIF};
        {$ENDIF Align16Bytes}
        {$ENDIF Align32Bytes}

        32: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32Bit}Move28{$ELSE}Move24{$ENDIF};

        {$IFNDEF Align32Bytes}
        {$IFNDEF Align16Bytes}
        40: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32bit}Move36{$ELSE}Move32{$ENDIF};
        {$ENDIF}
        48: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32Bit}Move44{$ELSE}Move40{$ENDIF};
        {$IFNDEF Align16Bytes}
        56: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32Bit}Move52{$ELSE}Move48{$ENDIF};
        {$ENDIF}
        {$ENDIF}

        64: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32Bit}Move60{$ELSE}Move56{$ENDIF};

        {$IFNDEF Align32Bytes}
        {$IFNDEF Align16Bytes}
        72: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}{$IFDEF 32bit}Move68{$ELSE}Move64{$ENDIF};
        {$ENDIF}
        {$ENDIF}
      end;
    end;

    {$IFDEF 64bit}
    {$IFDEF EnableFSRM}
    {$IFDEF USE_CPUID}
    if (FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) <> 0 then
    begin
      // don't use any register copy if we have Fast Short REP MOVSB
      // Fast Short REP MOVSB is very fast under 64-bit
      case SmallBlockTypes[LInd].BlockSize of
         {$IFNDEF Align32Bytes}
         16: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move8;
         {$IFNDEF Align16Bytes}
         24: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move16;
         {$ENDIF Align16Bytes}
         {$ENDIF Align32Bytes}
         32: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move24Reg64;
         {$IFNDEF Align32Bytes}
         {$IFNDEF Align16Bytes}
         40: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move32Reg64;
         {$ENDIF}
         48: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move40Reg64;
         {$IFNDEF Align16Bytes}
         56: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move48Reg64;
         {$ENDIF}
         {$ENDIF}
         64: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move56Reg64;
         else  SmallBlockTypes[LInd].UpsizeMoveProcedure := nil;
      end;
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}


{$IFDEF 64Bit}
{$IFDEF EnableAVX}

  {$IFDEF EnableAVX512}
    // if we have AVX-512 but don't have FSRM
    if ((FastMMCpuFeaturesA and FastMMCpuFeatureAVX512) <> 0)
        {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF}
    then
    begin
      case SmallBlockTypes[LInd].BlockSize of
         32*01: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move24AVX512;
         32*02: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move56AVX512;
         32*03: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move88AVX512;
         32*04: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move120AVX512;
         32*05: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move152AVX512;
         32*06: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move184AVX512;
         32*07: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move216AVX512;
         32*08: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move248AVX512;
         32*09: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move280AVX512;
         32*10: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move312AVX512;
         32*11: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move344AVX512;
      end;
    end else
  {$ENDIF}
    {$IFNDEF DisableAVX2}
    // if we have AVX2 but don't have FSRM
    if ((FastMMCpuFeaturesA and FastMMCpuFeatureAVX2) <> 0)
      {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF}
    then
    begin
      case SmallBlockTypes[LInd].BlockSize of
         32*1: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move24AVX2;
         32*2: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move56AVX2;
         32*3: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move88AVX2;
         32*4: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move120AVX2;
         32*5: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move152AVX2;
         32*6: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move184AVX2;
         32*7: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move216AVX2;
      end;
    end else
    {$ENDIF DisableAVX2}
    {$IFNDEF DisableAVX1}
    // if we have AVX1 but don't have FSRM
    if ((FastMMCpuFeaturesA and FastMMCpuFeatureAVX1) <> 0)
      {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF}
      then
    begin
      case SmallBlockTypes[LInd].BlockSize of
         32*1: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move24AVX1;
         32*2: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move56AVX1;
         32*3: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move88AVX1;
         32*4: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move120AVX1;
         32*5: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move152AVX1;
         32*6: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move184AVX1;
         32*7: SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}Move216AVX1;
     end;
    end else
   {$ENDIF}
   begin
     // dummy block in case of no AVX code above is defined
   end;
{$ENDIF}
{$ENDIF}

    if not Assigned(SmallBlockTypes[LInd].UpsizeMoveProcedure) then
  {$IFDEF UseCustomVariableSizeMoveRoutines}
    {$IFDEF Align32Bytes}
    {$IFDEF EnableAVX}
      {We must check AVX1 bit before checking the AVX2 bit}
    if ((FastMMCpuFeaturesA and FastMMCpuFeatureAVX2) <> 0) {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF} then
    begin
      if ((FastMMCpuFeaturesA and FastMMCpuFeatureERMS) <> 0) {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF} then
      begin
      {$IFDEF EnableAVX512}
      {$IFNDEF DisableMoveX32LpAvx512}
        if ((FastMMCpuFeaturesA and FastMMCpuFeatureAVX512) <> 0) {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF} then
        begin
          SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveX32LpAvx512WithErms;
        end else
      {$ENDIF}
      {$ENDIF}
        begin
          SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveX32LpAvx2WithErms;
        end;
      end else
      begin
        SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveX32LpAvx2NoErms;
      end;
    end else
    if ((FastMMCpuFeaturesA and FastMMCpuFeatureAVX1) <> 0) {$IFDEF EnableFSRM}and ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) = 0){$ENDIF} then
    begin
      SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveX32LpAvx1NoErms;
    end else
    {$ENDIF EnableAVX}
    begin
      {$IFDEF EnableERMS}
      if ((FastMMCpuFeaturesA and FastMMCpuFeatureERMS) <> 0)
        {$IFDEF EnableFSRM}or ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) <> 0){$ENDIF}
      then
      begin
        SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveWithErmsNoAVX;
      end else
      {$ENDIF}
      begin
        SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveX16LP;
      end;
    end;
    {$ELSE Align32Bytes}
      {$IFDEF USE_CPUID}
      {$IFDEF EnableERMS}
      if ((FastMMCpuFeaturesA and FastMMCpuFeatureERMS) <> 0)
         {$IFDEF EnableFSRM}or ((FastMMCpuFeaturesA and FastMMCpuFeatureFSRM) <> 0){$ENDIF}
      then
      begin
        SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveWithErmsNoAVX;
      end
      else
      {$ENDIF EnableERMS}
      {$ENDIF USE_CPUID}
      begin
        SmallBlockTypes[LInd].UpsizeMoveProcedure := {$IFDEF FPC}@{$ENDIF}MoveX16LP
      end;
      ;
    {$ENDIF Align32Bytes}
  {$ELSE UseCustomVariableSizeMoveRoutines}
      SmallBlockTypes[LInd].UpsizeMoveProcedure := @System.Move;
  {$ENDIF UseCustomVariableSizeMoveRoutines}
{$ENDIF}
{$IFDEF LogLockContention}
    SmallBlockTypes[LInd].BlockCollector.Initialize;
{$ENDIF}
    {Set the first "available pool" to the block type itself, so that the
     allocation routines know that there are currently no pools with free
     blocks of this size.}
    LPSmallBlockType := @(SmallBlockTypes[LInd]);
    LPSmallBlockPoolHeader := SmallBlockTypePtrToPoolHeaderPtr(LPSmallBlockType);
    SmallBlockTypes[LInd].PreviousPartiallyFreePool := LPSmallBlockPoolHeader;
    SmallBlockTypes[LInd].NextPartiallyFreePool := LPSmallBlockPoolHeader;
    {Set the block size to block type index translation table}
    for LSizeInd := (LPreviousBlockSize div SmallBlockGranularity) to (NativeUInt(SmallBlockTypes[LInd].BlockSize - 1) shr SmallBlockGranularityPowerOf2) do
    begin
   {$IFDEF AllocSize2SmallBlockTypesPrecomputedOffsets}
      AllocSz2SmlBlkTypOfsDivSclFctr[LSizeInd] := LInd shl (SmallBlockTypeRecSizePowerOf2 - MaximumCpuScaleFactorPowerOf2);
   {$ELSE}
      AllocSize2SmallBlockTypesIdx[LSizeInd] := LInd;
   {$ENDIF}
    end;
    {Cannot sequential feed yet: Ensure that the next address is greater than
     the maximum address}
    SmallBlockTypes[LInd].MaxSequentialFeedBlockAddress := Pointer(0);
    SmallBlockTypes[LInd].NextSequentialFeedBlockAddress := Pointer(1);
    {Get the mask to use for finding a medium block suitable for a block pool}
    LMinimumPoolSize :=
      ((SmallBlockTypes[LInd].BlockSize * MinimumSmallBlocksPerPool
        + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)
      and MediumBlockGranularityMask) + MediumBlockSizeOffset;
    if LMinimumPoolSize < MinimumMediumBlockSize then
    begin
      LMinimumPoolSize := MinimumMediumBlockSize;
    end;
    {Get the closest group number for the minimum pool size}
    LGroupNumber := (LMinimumPoolSize - MinimumMediumBlockSize + MediumBlockBinsPerGroup * MediumBlockGranularity div 2)
      shr (MediumBlockBinsPerGroupPowerOf2 + MediumBlockGranularityPowerOf2);
    {Too large?}
    if LGroupNumber > 7 then
    begin
      LGroupNumber := 7;
    end;

    {Set the bitmap}
    LByte := Byte(UnsignedBit) shl LGroupNumber;
    SmallBlockTypes[LInd].AllowedGroupsForBlockPoolBitmap := NegByteMaskBit(LByte);
    {Set the minimum pool size}
    SmallBlockTypes[LInd].MinimumBlockPoolSize := MinimumMediumBlockSize + (LGroupNumber shl (MediumBlockGranularityPowerOf2 + MediumBlockBinsPerGroupPowerOf2));
    {Get the optimal block pool size}
    LOptimalPoolSize := ((SmallBlockTypes[LInd].BlockSize * TargetSmallBlocksPerPool
        + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)
      and MediumBlockGranularityMask) + MediumBlockSizeOffset;
    {Limit the optimal pool size to within range}
    if LOptimalPoolSize < OptimalSmallBlockPoolSizeLowerLimit then
    begin
      LOptimalPoolSize := OptimalSmallBlockPoolSizeLowerLimit;
    end;
    if LOptimalPoolSize > OptimalSmallBlockPoolSizeUpperLimit then
    begin
      LOptimalPoolSize := OptimalSmallBlockPoolSizeUpperLimit;
    end;
    {How many blocks will fit in the adjusted optimal size?}
    LBlocksPerPool := (LOptimalPoolSize - SmallBlockPoolHeaderSize) div SmallBlockTypes[LInd].BlockSize;
    {Recalculate the optimal pool size to minimize wastage due to a partial
     last block.}
    SmallBlockTypes[LInd].OptimalBlockPoolSize :=
      ((LBlocksPerPool * SmallBlockTypes[LInd].BlockSize + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset) and MediumBlockGranularityMask) + MediumBlockSizeOffset;
{$IFDEF UseReleaseStack}
    for LSlot := 0 to NumStacksPerBlock - 1 do
      SmallBlockTypes[LInd].ReleaseStack[LSlot].Initialize(ReleaseStackSize, SizeOf(Pointer));
{$ENDIF}
{$IFDEF CheckHeapForCorruption}
    {Debug checks}
    if (SmallBlockTypes[LInd].OptimalBlockPoolSize < MinimumMediumBlockSize)
      or ((SmallBlockTypes[LInd].BlockSize shr SmallBlockGranularityPowerOf2) shl SmallBlockGranularityPowerOf2 <> SmallBlockTypes[LInd].BlockSize) then
    begin
  {$IFDEF BCB6OrDelphi7AndUp}
      System.Error(reInvalidPtr);
  {$ELSE}
      System.RunError(reInvalidPtr);
  {$ENDIF}
    end;
{$ENDIF}
    {Set the previous small block size}
    LPreviousBlockSize := SmallBlockTypes[LInd].BlockSize;
  end;

  {-------------------Set up the medium blocks-------------------}

  MediumBlocksLocked := CLockByteAvailable;
  {$IFDEF MediumBlocksLockedCriticalSection}
  {$IFDEF fpc}InitCriticalSection{$ELSE}InitializeCriticalSection{$ENDIF}(MediumBlocksLockedCS);
  {$ENDIF}

{$IFDEF CheckHeapForCorruption}
  {Check that there are no gaps between where the small blocks end and the
   medium blocks start}
  if (((MaximumSmallBlockSize - 3) + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
    and MediumBlockGranularityMask) + MediumBlockSizeOffset < MinimumMediumBlockSize then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
{$ENDIF}
  {There are currently no medium block pools}
  MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  {All medium bins are empty}
  for LInd := 0 to High(MediumBlockBins) do
  begin
    LPMediumFreeBlock := @(MediumBlockBins[LInd]);
    LPMediumFreeBlock^.PreviousFreeBlock := LPMediumFreeBlock;
    LPMediumFreeBlock^.NextFreeBlock := LPMediumFreeBlock;
  end;
  {------------------Set up the large blocks---------------------}
  LargeBlocksLocked := CLockByteAvailable;
  {$IFDEF LargeBlocksLockedCriticalSection}
  {$IFDEF fpc}InitCriticalSection{$ELSE}InitializeCriticalSection{$ENDIF}(LargeBlocksLockedCS);
  {$ENDIF}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
  {------------------Set up the debugging structures---------------------}

{$IFDEF EnableMemoryLeakReporting}
  ExpectedMemoryLeaksListLocked := CLockByteAvailable;
{$ENDIF}

{$IFDEF FullDebugMode}
  {Set up the fake VMT}
  {Copy the basic info from the TFreedObject class}
{$IFNDEF FPC}
  System.Move(Pointer(PByte(TFreedObject) + vmtSelfPtr + SizeOf(Pointer))^,
    FreedObjectVMT.VMTData[vmtSelfPtr + SizeOf(Pointer)], vmtParent - vmtSelfPtr);
  PNativeUInt(@FreedObjectVMT.VMTData[vmtSelfPtr])^ := NativeUInt(@FreedObjectVMT.VMTMethods[0]);
  {Set up the virtual method table}
  for LInd := 0 to MaxFakeVMTEntries - 1 do
  begin
    PNativeUInt(@FreedObjectVMT.VMTMethods[Low(FreedObjectVMT.VMTMethods) + NativeInt(LInd * SizeOf(Pointer))])^ :=
      NativeUInt(@TFreedObject.GetVirtualMethodIndex) + LInd * VMTIndexIncCodeSize;
  {$IFDEF CatchUseOfFreedInterfaces}
    VMTBadInterface[LInd] := @TFreedObject.InterfaceError;
  {$ENDIF}
  end;
{$ENDIF}
{$ENDIF}
{$IFDEF _EventLog}
  {Set up the default log file name}
  SetDefaultMMLogFileName;
{$ENDIF}
  {Initialize lock contention loggers for medium and large blocks}
{$IFDEF LogLockContention}
  MediumBlockCollector.Initialize;
  LargeBlockCollector.Initialize;
{$ENDIF}
  {Initialize release stacks for medium and large blocks}
{$IFDEF UseReleaseStack}
  for LSlot := 0 to NumStacksPerBlock - 1 do
  begin
    MediumReleaseStack[LSlot].Initialize(ReleaseStackSize, SizeOf(pointer));
    LargeReleaseStack[LSlot].Initialize(ReleaseStackSize, SizeOf(pointer));
  end;
{$ENDIF}
end;

{Installs the memory manager (InitializeMemoryManager should be called first)}
procedure InstallMemoryManager;
{$IFDEF MMSharingEnabled}
var
  i, LCurrentProcessID: Cardinal;
  LPMapAddress: PPointer;
  LChar: AnsiChar;
{$ENDIF}
begin
  {$IFDEF fpc}
   FillChar(NewMemoryManager, SizeOf(NewMemoryManager), 0); // prevents potential undefined behavior on FPC caused by uninitialized data block
  {$ENDIF}
  if not FastMMIsInstalled then
  begin
{$IFDEF FullDebugMode}
  {$IFDEF 32Bit}
    {Try to reserve the 64K block covering address $80808080 so pointers with DebugFillPattern will A/V}
    ReservedBlock := VirtualAlloc(Pointer(DebugReservedAddress), 65536, MEM_RESERVE, PAGE_NOACCESS);
    {Allocate the address space slack.}
    AddressSpaceSlackPtr := VirtualAlloc(nil, FullDebugModeAddressSpaceSlack, MEM_RESERVE or MEM_TOP_DOWN, PAGE_NOACCESS);
  {$ENDIF}
{$ENDIF}
{$IFDEF MMSharingEnabled}
    {Build a string identifying the current process}
    LCurrentProcessID := GetCurrentProcessId;
    for i := 0 to 7 do
    begin
      LChar := HexTable[((LCurrentProcessID shr (i * 4)) and $F)];
      MappingObjectName[(High(MappingObjectName) - 1) - i] := LChar;
  {$IFDEF EnableBackwardCompatibleMMSharing}
      UniqueProcessIDString[8 - i] := LChar;
      UniqueProcessIDStringBE[8 - i] := LChar;
  {$ENDIF}
    end;
{$ENDIF}
{$IFDEF AttemptToUseSharedMM}
    {Is the replacement memory manager already installed for this process?}
{$IFDEF EnableBackwardCompatibleMMSharing}
    MMWindow := FindWindowA('STATIC', PAnsiChar(@UniqueProcessIDString[1]));
    MMWindowBE := FindWindowA('STATIC', PAnsiChar(@UniqueProcessIDStringBE[1]));
{$ENDIF}
    MappingObjectHandle := OpenFileMappingA(FILE_MAP_READ, False, MappingObjectName);
    {Is no MM being shared?}
{$IFDEF EnableBackwardCompatibleMMSharing}
    if (MMWindow or MMWindowBE or MappingObjectHandle) = 0 then
{$ELSE}
    if MappingObjectHandle = 0 then
{$ENDIF}
    begin
{$ENDIF}
{$IFDEF ShareMM}
      {Share the MM with other DLLs? - if this DLL is unloaded, then
       dependent DLLs will cause a crash.}
  {$IFNDEF ShareMMIfLibrary}
      if not IsLibrary then
  {$ENDIF}
      begin
  {$IFDEF EnableBackwardCompatibleMMSharing}
        {No memory manager installed yet - create the invisible window}
        MMWindow := CreateWindowA('STATIC', PAnsiChar(@UniqueProcessIDString[1]),
          WS_POPUP, 0, 0, 0, 0, 0, 0, hInstance, nil);
        MMWindowBE := CreateWindowA('STATIC', PAnsiChar(@UniqueProcessIDStringBE[1]),
          WS_POPUP, 0, 0, 0, 0, 0, 0, hInstance, nil);
        {The window data is a pointer to this memory manager}
        if MMWindow <> 0 then
          SetWindowLongA(MMWindow, GWL_USERDATA, NativeInt(@NewMemoryManager));
        if MMWindowBE <> 0 then
          SetWindowLongA(MMWindowBE, GWL_USERDATA, NativeInt(@NewMemoryManager));
  {$ENDIF}
        {Create the memory mapped file}
        MappingObjectHandle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil,
          PAGE_READWRITE, 0, SizeOf(Pointer), MappingObjectName);
        {Map a view of the memory}
        LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_WRITE, 0, 0, 0);
        {Set a pointer to the new memory manager}
        LPMapAddress^ := @NewMemoryManager;
        {Unmap the file}
        UnmapViewOfFile(LPMapAddress);
      end;
{$ENDIF}
      {We will be using this memory manager}
{$IFNDEF FullDebugMode}
      NewMemoryManager.GetMem := {$IFDEF FPC}@{$ENDIF}FastGetMem;
      NewMemoryManager.FreeMem := {$IFDEF FPC}@{$ENDIF}FastFreeMem;
      NewMemoryManager.ReallocMem := {$IFDEF FPC}@{$ENDIF}FastReallocMem;
      {$IFDEF fpc}
      NewMemoryManager.FreememSize := {$IFDEF FPC}@{$ENDIF}FastFreeMemSize;
      NewMemoryManager.AllocMem := {$IFDEF FPC}@{$ENDIF}FastAllocMem;
      NewMemoryManager.MemSize := {$IFDEF FPC}@{$ENDIF}FastMemSize;
      {$ENDIF}
{$ELSE}
{$IFDEF FPC}
       NewMemoryManager.GetMem := @DebugGetMem;
       NewMemoryManager.FreeMem := @DebugFreeMem;
       NewMemoryManager.ReallocMem := @DebugReallocMem;
{$ELSE}
       NewMemoryManager.GetMem := DebugGetMem;
       NewMemoryManager.FreeMem := DebugFreeMem;
       NewMemoryManager.ReallocMem := DebugReallocMem;
{$ENDIF}
{$ENDIF}
{$IFDEF fpc}
      NewMemoryManager.GetFPCHeapStatus := {$IFDEF FPC}@{$ENDIF}FastGetFPCHeapStatus; //support get TFPCHeapStatus
{$ENDIF}
{$IFDEF BDS2006AndUp}
  {$IFNDEF FullDebugMode}
      NewMemoryManager.AllocMem := {$IFDEF FPC}@{$ENDIF}FastAllocMem;
  {$ELSE}
      NewMemoryManager.AllocMem := {$IFDEF FPC}@{$ENDIF}DebugAllocMem;
  {$ENDIF}
  {$IFDEF EnableMemoryLeakReporting}
      NewMemoryManager.RegisterExpectedMemoryLeak := {$IFDEF FPC}@{$ENDIF}RegisterExpectedMemoryLeak;
      NewMemoryManager.UnRegisterExpectedMemoryLeak := {$IFDEF FPC}@{$ENDIF}UnRegisterExpectedMemoryLeak;
  {$ELSE}
      NewMemoryManager.RegisterExpectedMemoryLeak := {$IFDEF FPC}@{$ENDIF}NoOpRegisterExpectedMemoryLeak;
      NewMemoryManager.UnRegisterExpectedMemoryLeak := {$IFDEF FPC}@{$ENDIF}NoOpUnRegisterExpectedMemoryLeak;
  {$ENDIF}
{$ENDIF}
      {Owns the memory manager}
      IsMemoryManagerOwner := True;
{$IFDEF AttemptToUseSharedMM}
    end
    else
    begin
      {Get the address of the shared memory manager}
  {$IFNDEF BDS2006AndUp}
    {$IFDEF EnableBackwardCompatibleMMSharing}
      if MappingObjectHandle <> 0 then
      begin
    {$ENDIF}
        {Map a view of the memory}
        LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_READ, 0, 0, 0);
        {Set the new memory manager}
        NewMemoryManager := PMemoryManager(LPMapAddress^)^;
        {Unmap the file}
        UnmapViewOfFile(LPMapAddress);
    {$IFDEF EnableBackwardCompatibleMMSharing}
      end
      else
      begin
        if MMWindow <> 0 then
        begin
          NewMemoryManager := PMemoryManager(GetWindowLong(MMWindow, GWL_USERDATA))^;
        end
        else
        begin
          NewMemoryManager := PMemoryManager(GetWindowLong(MMWindowBE, GWL_USERDATA))^;
        end;
      end;
    {$ENDIF}
  {$ELSE}
    {$IFDEF EnableBackwardCompatibleMMSharing}
      if MappingObjectHandle <> 0 then
      begin
    {$ENDIF}
        {Map a view of the memory}
        LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_READ, 0, 0, 0);
        {Set the new memory manager}
        NewMemoryManager := PMemoryManagerEx(LPMapAddress^)^;
        {Unmap the file}
        UnmapViewOfFile(LPMapAddress);
    {$IFDEF EnableBackwardCompatibleMMSharing}
      end
      else
      begin
        if MMWindow <> 0 then
        begin
          NewMemoryManager := PMemoryManagerEx(GetWindowLong(MMWindow, GWL_USERDATA))^;
        end
        else
        begin
          NewMemoryManager := PMemoryManagerEx(GetWindowLong(MMWindowBE, GWL_USERDATA))^;
        end;
      end;
    {$ENDIF}
  {$ENDIF}
      {Close the file mapping handle}
      CloseHandle(MappingObjectHandle);
      MappingObjectHandle := 0;
      {The memory manager is not owned by this module}
      IsMemoryManagerOwner := False;
    end;
{$ENDIF}
    {Save the old memory manager}
    GetMemoryManager(OldMemoryManager);
    {Replace the memory manager with either this one or the shared one.}
    SetMemoryManager(NewMemoryManager);
    {FastMM is now installed}
    FastMMIsInstalled := True;
{$IFDEF UseOutputDebugString}
    if IsMemoryManagerOwner then
      OutputDebugStringA(FastMMInstallMsg)
    else
      OutputDebugStringA(FastMMInstallSharedMsg);
{$ENDIF}
  end;
end;

procedure UninstallMemoryManager;
begin
  {Is this the owner of the shared MM window?}
  if IsMemoryManagerOwner then
  begin
{$IFDEF ShareMM}
  {$IFDEF EnableBackwardCompatibleMMSharing}
    {Destroy the window}
    if MMWindow <> 0 then
    begin
      DestroyWindow(MMWindow);
      MMWindow := 0;
    end;
    if MMWindowBE <> 0 then
    begin
      DestroyWindow(MMWindowBE);
      MMWindowBE := 0;
    end;
  {$ENDIF}
    {Destroy the memory mapped file handle}
    if MappingObjectHandle <> 0 then
    begin
      CloseHandle(MappingObjectHandle);
      MappingObjectHandle := 0;
    end;
{$ENDIF}
{$IFDEF FullDebugMode}
    {Release the reserved block}
    if ReservedBlock <> nil then
    begin
      VirtualFree(ReservedBlock, 0, MEM_RELEASE);
      ReservedBlock := nil;
    end;
    {Release the address space slack}
    if AddressSpaceSlackPtr <> nil then
    begin
      VirtualFree(AddressSpaceSlackPtr, 0, MEM_RELEASE);
      AddressSpaceSlackPtr := nil;
    end;
{$ENDIF}
  end;
{$IFNDEF DetectMMOperationsAfterUninstall}
  {Restore the old memory manager}
  SetMemoryManager(OldMemoryManager);
{$ELSE}
  {Set the invalid memory manager: no more MM operations allowed}
  SetMemoryManager(InvalidMemoryManager);
{$ENDIF}
  {Memory manager has been uninstalled}
  FastMMIsInstalled := False;
{$IFDEF UseOutputDebugString}
  if IsMemoryManagerOwner then
    OutputDebugStringA(FastMMUninstallMsg)
  else
    OutputDebugStringA(FastMMUninstallSharedMsg);
{$ENDIF}
end;

{$IFDEF UseReleaseStack}
procedure CleanupReleaseStacks;
var
  LInd: Integer;
  LMemory: Pointer;
  LSlot: Integer;
begin
  for LInd := 0 to High(SmallBlockTypes) do begin
    for LSlot := 0 to NumStacksPerBlock-1 do
      while SmallBlockTypes[LInd].ReleaseStack[LSlot].Pop(LMemory) do
        FastFreeMem(LMemory);
    {Finalize all stacks only after all memory for this block has been freed.}
    {Otherwise, FastFreeMem could try to access a stack that was already finalized.}
    for LSlot := 0 to NumStacksPerBlock-1 do
      SmallBlockTypes[LInd].ReleaseStack[LSlot].Finalize;
  end;
  for LSlot := 0 to NumStacksPerBlock-1 do
  begin
    while MediumReleaseStack[LSlot].Pop(LMemory) do
      FastFreeMem(LMemory);
    while LargeReleaseStack[LSlot].Pop(LMemory) do
      FastFreeMem(LMemory);
  end;
  for LSlot := 0 to NumStacksPerBlock-1 do
  begin
    MediumReleaseStack[LSlot].Finalize;
    LargeReleaseStack[LSlot].Finalize;
  end;
end;

function ReleaseStackCleanupThreadProc(AParam: Pointer): Integer;
var
  LMemBlock: Pointer;
  LSlot: Integer;
begin
  {Clean up 1 medium and 1 large block for every thread slot, every 100ms.}
  while WaitForSingleObject(ReleaseStackCleanupThreadTerminate, 100) = WAIT_TIMEOUT do
  begin
    for LSlot := 0 to NumStacksPerBlock - 1 do
    begin
      if (not MediumReleaseStack[LSlot].IsEmpty)
        and (AcquireLockByte(MediumBlocksLocked)) then
      begin
        if MediumReleaseStack[LSlot].Pop(LMemBlock) then
          FreeMediumBlock(LMemBlock, True)
        else
        begin
          UnlockMediumBlocks;
        end;
      end;
      if (not LargeReleaseStack[LSlot].IsEmpty)
        and (AcquireLockByte(LargeBlocksLocked)) then
      begin
        if LargeReleaseStack[LSlot].Pop(LMemBlock) then
          FreeLargeBlock(LMemBlock, True)
        else
        begin
          UnlockLargeBlocks;
        end;
      end;
    end;
  end;
  Result := 0;
end;

procedure CreateCleanupThread;
var
  LThreadID: DWORD;
begin
  ReleaseStackCleanupThreadTerminate := CreateEvent(nil, False, False, nil);
  if ReleaseStackCleanupThreadTerminate = 0 then
    {$IFDEF BCB6OrDelphi7AndUp}System.Error(reInvalidPtr);{$ELSE}System.RunError(reInvalidPtr);{$ENDIF}
  ReleaseStackCleanupThread := BeginThread(nil, 0, ReleaseStackCleanupThreadProc, nil, 0, LThreadID);
  if ReleaseStackCleanupThread = 0 then
    {$IFDEF BCB6OrDelphi7AndUp}System.Error(reInvalidPtr);{$ELSE}System.RunError(reInvalidPtr);{$ENDIF}
  SetThreadPriority(ReleaseStackCleanupThread, THREAD_PRIORITY_LOWEST);
end;

procedure DestroyCleanupThread;
begin
  if ReleaseStackCleanupThread <> 0 then
  begin
    SetEvent(ReleaseStackCleanupThreadTerminate);
    WaitForSingleObject(ReleaseStackCleanupThread, INFINITE);
    CloseHandle(ReleaseStackCleanupThread);
    ReleaseStackCleanupThread := 0;
    CloseHandle(ReleaseStackCleanupThreadTerminate);
    ReleaseStackCleanupThreadTerminate := 0;
  end;
end;
{$ENDIF}

procedure FinalizeMemoryManager;
{$IFDEF SmallBlocksLockedCriticalSection}
var
  LInd: Integer;
{$ENDIF}
begin
  {Restore the old memory manager if FastMM has been installed}
  if FastMMIsInstalled then
  begin
{$IFDEF UseReleaseStack}
  DestroyCleanupThread;
  CleanupReleaseStacks;
{$ENDIF}
{$IFNDEF NeverUninstall}
    {Uninstall FastMM}
    UninstallMemoryManager;
{$ENDIF}
    {Do we own the memory manager, or are we just sharing it?}
    if IsMemoryManagerOwner then
    begin
{$IFDEF CheckUseOfFreedBlocksOnShutdown}
      CheckBlocksOnShutdown(
  {$IFDEF EnableMemoryLeakReporting}
        True
    {$IFDEF RequireIDEPresenceForLeakReporting}
        and DelphiIsRunning
    {$ENDIF}
    {$IFDEF RequireDebuggerPresenceForLeakReporting}
        and ( {$IFNDEF FPC}(DebugHook <> 0){$ELSE}True{$ENDIF}
        {$IFDEF PatchBCBTerminate}
        or (Assigned(pCppDebugHook) and (pCppDebugHook^ <> 0))
        {$ENDIF PatchBCBTerminate}
        )
    {$ENDIF}
    {$IFDEF ManualLeakReportingControl}
        and ReportMemoryLeaksOnShutdown
    {$ENDIF}
  {$ELSE}
        False
  {$ENDIF}
      );
{$ELSE}
  {$IFDEF EnableMemoryLeakReporting}
      if True
    {$IFDEF RequireIDEPresenceForLeakReporting}
        and DelphiIsRunning
    {$ENDIF}
    {$IFDEF RequireDebuggerPresenceForLeakReporting}
        {$IFNDEF fpc}
        and ((DebugHook <> 0)
        {$IFDEF PatchBCBTerminate}
        or (Assigned(pCppDebugHook) and (pCppDebugHook^ <> 0))
        {$ENDIF PatchBCBTerminate}
        )
        {$ENDIF}
    {$ENDIF}
    {$IFDEF ManualLeakReportingControl}
        and ReportMemoryLeaksOnShutdown
    {$ENDIF}
      then
        CheckBlocksOnShutdown(True);
  {$ENDIF}
{$ENDIF}
{$IFDEF EnableMemoryLeakReporting}
      {Free the expected memory leaks list}
      if ExpectedMemoryLeaks <> nil then
      begin
        VirtualFree(ExpectedMemoryLeaks, 0, MEM_RELEASE);
        ExpectedMemoryLeaks := nil;
      end;
{$ENDIF}
{$IFDEF LogLockContention}
      ReportLockContention;
{$ENDIF}
{$IFNDEF NeverUninstall}
     {$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically )} // MS
     FastMM_FreeDebugSupportLibrary;
     {$IFEND}

      {Clean up: Free all memory. If this is a .DLL that owns its own MM, then
       it is necessary to prevent the main application from running out of
       address space.}
      FreeAllMemory;
{$ENDIF}
    end;

  {$IFDEF MediumBlocksLockedCriticalSection}
  LargeBlocksLocked := CLockByteFinished;
  {$IFDEF fpc}DoneCriticalSection{$ELSE}DeleteCriticalSection{$ENDIF}(MediumBlocksLockedCS);
  {$ENDIF MediumBlocksLockedCriticalSection}

  {$IFDEF LargeBlocksLockedCriticalSection}
  LargeBlocksLocked := CLockByteFinished;
  {$IFDEF fpc}DoneCriticalSection{$ELSE}DeleteCriticalSection{$ENDIF}(LargeBlocksLockedCS);
  {$ENDIF LargeBlocksLockedCriticalSection}

  {$IFDEF SmallBlocksLockedCriticalSection}
  if not CpuFeaturePauseAndSwitch then
  begin
    for LInd := Low(SmallBlockCriticalSections) to High(SmallBlockCriticalSections) do
    begin
      {$IFDEF fpc}DoneCriticalSection{$ELSE}DeleteCriticalSection{$ENDIF}(SmallBlockCriticalSections[LInd]);
    end;
  end;

  for LInd := Low(SmallBlockTypes) to High(SmallBlockTypes) do
  begin
    SmallBlockTypes[LInd].SmallBlockTypeLocked := CLockByteFinished;
  end;
  {$ENDIF}

  end;
end;

{$IFDEF DEBUG}
procedure SelfTest;
begin
{$IFDEF NeedFindFirstSetBit}
  if
    (FindFirstSetBit(0) <> 0) or
    (FindFirstSetBit(1) <> 0) or
    (FindFirstSetBit(2) <> 1) or
    (FindFirstSetBit(3) <> 0) or
    (FindFirstSetBit(4) <> 2) or
    (FindFirstSetBit($80000000) <> 31) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
{$ENDIF}
  if (NegByteMaskBit(0) <> 0) or
     (NegByteMaskBit(1) <> $FF) or
     (NegByteMaskBit(2) <> $FE) or
     (NegByteMaskBit(3) <> $FD) or
     (NegByteMaskBit(4) <> $FC) or
     (NegByteMaskBit($7E) <> $82) or
     (NegByteMaskBit($7F) <> $81) or
     (NegByteMaskBit($80) <> $80) or
     (NegByteMaskBit($81) <> $7F) or
     (NegByteMaskBit($FE) <> 2) or
     (NegByteMaskBit($FF) <> 1) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;

  if (NegCardinalMaskBit(0) <> 0) or
     (NegCardinalMaskBit(1) <> $FFFFFFFF) or
     (NegCardinalMaskBit(2) <> $FFFFFFFE) or
     (NegCardinalMaskBit(3) <> $FFFFFFFD) or
     (NegCardinalMaskBit(4) <> $FFFFFFFC) or
     (NegCardinalMaskBit($7E) <> $FFFFFF82) or
     (NegCardinalMaskBit($7F) <> $FFFFFF81) or
     (NegCardinalMaskBit($80) <> $FFFFFF80) or
     (NegCardinalMaskBit($81) <> $FFFFFF7F) or
     (NegCardinalMaskBit($FE) <> $FFFFFF02) or
     (NegCardinalMaskBit($FF) <> $FFFFFF01) or
     (NegCardinalMaskBit($100) <> $FFFFFF00) or
     (NegCardinalMaskBit($101) <> $FFFFFEFF) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;

  if
     (NegCardinalMaskBit($7FFFFFFF) <> $80000001) or
     (NegCardinalMaskBit($80000000) <> $80000000) or
     (NegCardinalMaskBit($80000001) <> $7FFFFFFF) or
     (NegCardinalMaskBit($FFFFFFFF) <> 1) or
     (NegCardinalMaskBit($FFFFFFFE) <> 2) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;

  {$IFDEF 32bit}
  if (NegNativeUintMaskBit(0) <> 0) or
     (NegNativeUintMaskBit(1) <> $FFFFFFFF) or
     (NegNativeUintMaskBit(2) <> $FFFFFFFE) or
     (NegNativeUintMaskBit(3) <> $FFFFFFFD) or
     (NegNativeUintMaskBit(4) <> $FFFFFFFC) or
     (NegNativeUintMaskBit($7E) <> $FFFFFF82) or
     (NegNativeUintMaskBit($7F) <> $FFFFFF81) or
     (NegNativeUintMaskBit($80) <> $FFFFFF80) or
     (NegNativeUintMaskBit($81) <> $FFFFFF7F) or
     (NegNativeUintMaskBit($FE) <> $FFFFFF02) or
     (NegNativeUintMaskBit($FF) <> $FFFFFF01) or
     (NegNativeUintMaskBit($100) <> $FFFFFF00) or
     (NegNativeUintMaskBit($101) <> $FFFFFEFF) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
  if
     (NegNativeUintMaskBit($7FFFFFFF) <> $80000001) or
     (NegNativeUintMaskBit($80000000) <> $80000000) or
     (NegNativeUintMaskBit($80000001) <> $7FFFFFFF) or
     (NegNativeUintMaskBit($FFFFFFFF) <> 1) or
     (NegNativeUintMaskBit($FFFFFFFE) <> 2) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
  {$ELSE 32bit}
  if (NegNativeUintMaskBit(NativeUInt(0)) <> NativeUInt(0)) or
     (NegNativeUintMaskBit(NativeUInt(1)) <> NativeUInt($FFFFFFFFFFFFFFFF)) or
     (NegNativeUintMaskBit(NativeUInt(2)) <> NativeUInt($FFFFFFFFFFFFFFFE)) or
     (NegNativeUintMaskBit(NativeUInt(3)) <> NativeUInt($FFFFFFFFFFFFFFFD)) or
     (NegNativeUintMaskBit(NativeUInt(4)) <> NativeUInt($FFFFFFFFFFFFFFFC)) or
     (NegNativeUintMaskBit(NativeUInt($7E)) <> NativeUInt($FFFFFFFFFFFFFF82)) or
     (NegNativeUintMaskBit(NativeUInt($7F)) <> NativeUInt($FFFFFFFFFFFFFF81)) or
     (NegNativeUintMaskBit(NativeUInt($80)) <> NativeUInt($FFFFFFFFFFFFFF80)) or
     (NegNativeUintMaskBit(NativeUInt($81)) <> NativeUInt($FFFFFFFFFFFFFF7F)) or
     (NegNativeUintMaskBit(NativeUInt($FE)) <> NativeUInt($FFFFFFFFFFFFFF02)) or
     (NegNativeUintMaskBit(NativeUInt($FF)) <> NativeUInt($FFFFFFFFFFFFFF01)) or
     (NegNativeUintMaskBit(NativeUInt($100)) <> NativeUInt($FFFFFFFFFFFFFF00)) or
     (NegNativeUintMaskBit(NativeUInt($101)) <> NativeUInt($FFFFFFFFFFFFFEFF)) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
  if
     (NegNativeUintMaskBit(NativeUInt($7FFFFFFF)) <> NativeUInt($FFFFFFFF80000001)) or
     (NegNativeUintMaskBit(NativeUInt($80000000)) <> NativeUInt($FFFFFFFF80000000)) or
     (NegNativeUintMaskBit(NativeUInt($80000001)) <> NativeUInt($FFFFFFFF7FFFFFFF)) or
     (NegNativeUintMaskBit(NativeUInt($FFFFFFFF)) <> NativeUInt($FFFFFFFF00000001)) or
     (NegNativeUintMaskBit(NativeUInt($FFFFFFFE)) <> NativeUInt($FFFFFFFF00000002)) or
     (NegNativeUintMaskBit(NativeUInt($FFFFFFFFFFFFFFFF)) <> NativeUInt(1)) or
     (NegNativeUintMaskBit(NativeUInt($FFFFFFFFFFFFFFFE)) <> NativeUInt(2)) then
  begin
  {$IFDEF BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$ELSE}
    System.RunError(reInvalidPtr);
  {$ENDIF}
  end;
  {$ENDIF 32bit}
end;
{$ENDIF}

{$IFDEF USE_CPUID}
function GetFastMMCpuFeatures: Word;
var
  W: Word;
begin
  W := FastMMCpuFeaturesB;
  Result := (W shl 8) or FastMMCpuFeaturesA;
end;

function GetFastMMCpuFeaturesA: Byte;
begin
  Result := FastMMCpuFeaturesA
end;

function GetFastMMCpuFeaturesB: Byte;
begin
  Result := FastMMCpuFeaturesB
end;

procedure GetFastMMCpuUserModeMonitorLineSizes(var Smallest, Largest: Word);
begin
  Smallest := FastMMCpuSmallestMonitorLineSize;
  Largest := FastMMCpuLargestMonitorLineSize;
end;

{$IFDEF EnableWaitPKG}
procedure FastMMDisableWaitPKG;
begin
  FastMMCpuFeaturesB := FastMMCpuFeaturesB and not FastMMCpuFeatureB_WAITPKG;
end;
{$ENDIF EnableWaitPKG}

{$ENDIF}


procedure RunInitializationCode;
begin
  {Only run this code once during startup.}
  if InitializationCodeHasRun then
    Exit;
{$IFNDEF BCB}
{$IFDEF DEBUG}
  SelfTest;
{$ENDIF}
  {$IFDEF InstallOnlyIfRunningInIDE}
  if (DebugHook <> 0) and DelphiIsRunning then
  {$ENDIF}
  begin
    {Initialize all the lookup tables, etc. for the memory manager}
    InitializeMemoryManager;
    {Has another MM been set, or has the Embarcadero MM been used? If so, this
     file is not the first unit in the uses clause of the project's .dpr
     file.}
    if CheckCanInstallMemoryManager then
    begin
    {$IFDEF ClearLogFileOnStartup}
      DeleteEventLog;
    {$ENDIF}
      InstallMemoryManager;
    end;
    {$IFDEF UseReleaseStack}
    {Release stack mechanism needs a cleanup thread}
    CreateCleanupThread;
    {$ENDIF}

    InitializationCodeHasRun := True;    
    {$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically ) AND Defined( MemoryLoadLibrarySupport )}
    FastMM_LoadDebugSupportLibrary;
    {$IFEND}
  end;
{$ELSE}
  InitializationCodeHasRun := True;
{$ENDIF}
end;

{$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically )}
function FastMM_IsDebugSupportLibraryLoaded: boolean;
begin
  Result := ( FullDebugModeDLL <> 0 ) {$IFDEF MemoryLoadLibrarySupport}OR Assigned( FullDebugModeRDLL ){$ENDIF};
end;

function FastMM_LoadDebugSupportLibrary : boolean;
{$IFDEF RestrictDebugDLLLoadPath}
var
  LModuleHandle: HModule;
  LFullFileName: array[0..MaxFileNameLengthDouble-1] of Char;
{$ENDIF}
begin
  result := False;
  if ( FullDebugModeDLL <> 0 ) {$IFDEF MemoryLoadLibrarySupport}OR Assigned( FullDebugModeRDLL ){$ENDIF} then
    Exit;

  {$IFDEF RestrictDebugDLLLoadPath}
  FullDebugModeDLL := 0;
  LModuleHandle := 0;
  {$IFNDEF borlndmmdll}
  if IsLibrary then
    LModuleHandle := HInstance;
  {$ENDIF}

  LSizeInd := GetModuleFileName(LModuleHandle, LFullFileName, Sizeof(LFullFileName) div SizeOf(Char));
  while LSizeInd > 0 do
  begin
    Dec(LSizeInd);
    if LFullFileName[LSizeInd] = '\' then
      Break;
  end;
  if (LSizeInd > 0) and (LSizeInd + Cardinal(Length(FullDebugModeLibraryName)) + 1 < Sizeof(LFullFileName) div SizeOf(Char)) then
  begin
    LInd := 1;
    repeat
      LFullFileName[LSizeInd + LInd] := FullDebugModeLibraryName[LInd];
      Inc(LInd);
    until LInd > Cardinal(Length(FullDebugModeLibraryName));
    LFullFileName[LSizeInd + LInd] := #0;
    FullDebugModeDLL := LoadLibrary(LFullFileName);
  end;
  {$ELSE}
  FullDebugModeDLL := LoadLibrary(FullDebugModeLibraryName);
  {$ENDIF RestrictDebugDLLLoadPath}
  if ( FullDebugModeDLL <> 0 ) then
  begin
    GetStackTrace := GetProcAddress(FullDebugModeDLL,
      {$IFDEF RawStackTraces}'GetRawStackTrace'{$ELSE}'GetFrameBasedStackTrace'{$ENDIF});
    LogStackTrace := GetProcAddress(FullDebugModeDLL, 'LogStackTrace');
  end;

  {$IF Defined( FullDebugMode ) AND Defined( LoadDebugDLLDynamically ) AND Defined( MemoryLoadLibrarySupport )}
  if NOT InitializationCodeHasRun then // Resource is loaded after FastMM since we allocate Memory here .. 
    Exit;

  if ( FullDebugModeDLL = 0 ) then
    begin
    MemoryLoadLibrary( FullDebugModeLibraryName, FullDebugModeRDLL );
    if Assigned( FullDebugModeRDLL ) then
      begin
      GetStackTrace := MemoryGetProcAddress(FullDebugModeRDLL,
        {$IFDEF RawStackTraces}'GetRawStackTrace'{$ELSE}'GetFrameBasedStackTrace'{$ENDIF});
      LogStackTrace := MemoryGetProcAddress(FullDebugModeRDLL, 'LogStackTrace');
      end;
    end;
  {$IFEND}
  Result := ( FullDebugModeDLL <> 0 ) {$IFDEF MemoryLoadLibrarySupport}OR Assigned( FullDebugModeRDLL ){$ENDIF};
end;

function FastMM_FreeDebugSupportLibrary : boolean;
begin
  result := False;
  if ( FullDebugModeDLL = 0 ) {$IFDEF MemoryLoadLibrarySupport}AND NOT Assigned( FullDebugModeRDLL ){$ENDIF} then
    Exit;

  GetStackTrace := NoOpGetStackTrace;
  LogStackTrace := NoOpLogStackTrace;

  if ( FullDebugModeDLL <> 0 ) then
    begin
    FreeLibrary( FullDebugModeDLL );
    FullDebugModeDLL := 0;
    end
  {$IFDEF MemoryLoadLibrarySupport}
  else if Assigned( FullDebugModeRDLL ) then
    begin
    MemoryFreeLibrary( FullDebugModeRDLL );
    FullDebugModeRDLL := nil;
    end
  {$ENDIF}
  else
    Exit;

  Result := True;
end;
{$IFEND}

initialization
  RunInitializationCode;

finalization
{$IFNDEF PatchBCBTerminate}
  FinalizeMemoryManager;
{$ENDIF}

end.
