# FastMM4-AVX

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
     techniques are used depending on context and availability, i.e., pause-
     based spin-wait loops, umonitor/umwait (WaitPKG), SwitchToThread,
     critical sections, etc.;
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

- 1.0.7 (22 March 2023) - implemented the optional use of user mode wait
    (WaitPKG) umonitor/umwait instructions to wait for a synchronization
    variable; it is disabled by default; define the "EnableWaitPKG" conditional
    define to enable this feature; however it may not be as efficient
    as the pause-based loop, so only use this feature it if your tests
    show clear benefit in your scenarios.

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

# FastMM4
Fast Memory Manager

Description:
 A fast replacement memory manager for Embarcadero Delphi applications
 that scales well under multi-threaded usage, is not prone to memory
 fragmentation, and supports shared memory without the use of external .DLL
 files.

Homepage:
 https://github.com/pleriche/FastMM4

Advantages:
 - Fast
 - Low overhead. FastMM is designed for an average of 5% and maximum of 10%
   overhead per block.
 - Supports up to 3GB of user mode address space under Windows 32-bit and 4GB
   under Windows 64-bit. Add the "$SetPEFlags $20" option (in curly braces)
   to your .dpr to enable this.
 - Highly aligned memory blocks. Can be configured for either 8-byte or 16-byte
   alignment.
 - Good scaling under multi-threaded applications
 - Intelligent reallocations. Avoids slow memory move operations through
   not performing unneccesary downsizes and by having a minimum percentage
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
 C++ Builder:
  Refer to the instructions inside FastMM4BCB.cpp.


# FastMM4
Fast Memory Manager
![FastMM-Title.jpg with title only](images/FastMM-Title.jpg "FastMM-Title.jpg with title only")

## Description:
 A fast replacement memory manager for Embarcadero Delphi applications
 that scales well under multi-threaded usage, is not prone to memory
 fragmentation, and supports shared memory without the use of external .DLL
 files.

## Homepage:
 https://github.com/pleriche/FastMM4

## Advantages:
* Fast
* Low overhead. FastMM is designed for an average of 5% and maximum of 10%
   overhead per block.
* Supports up to 3GB of user mode address space under Windows 32-bit and 4GB
   under Windows 64-bit. Add the "$SetPEFlags $20" option (in curly braces)
   to your .dpr to enable this.
* Highly aligned memory blocks. Can be configured for either 8-byte or 16-byte
   alignment.
* Good scaling under multi-threaded applications
* Intelligent reallocations. Avoids slow memory move operations through
   not performing unneccesary downsizes and by having a minimum percentage
   block size growth factor when an in-place block upsize is not possible.
* Resistant to address space fragmentation
* No external DLL required when sharing memory between the application and
   external libraries (provided both use this memory manager)
* Optionally reports memory leaks on program shutdown. (This check can be set
   to be performed only if Delphi is currently running on the machine, so end
   users won't be bothered by the error message.)
* Supports Delphi 4 (or later), C++ Builder 4 (or later), Kylix 3.

## Usage:
### Delphi:
  Place this unit as the very first unit under the "uses" section in your
  project's .dpr file. When sharing memory between an application and a DLL
  (e.g. when passing a long string or dynamic array to a DLL function), both the
  main application and the DLL must be compiled using this memory manager (with
  the required conditional defines set).

  There are some conditional defines
  (inside `FastMM4Options.inc`) that may be used to tweak the memory manager. To
  enable support for a user mode address space greater than 2GB you will have to
  use the EditBin* tool to set the `LARGE_ADDRESS_AWARE` flag in the EXE header.
  This informs Windows x64 or Windows 32-bit (with the /3GB option set) that the
  application supports an address space larger than 2GB (up to 4GB). In Delphi 6
  and later you can also specify this flag through the compiler directive
  `{$SetPEFlags $20}`

 *The EditBin tool ships with the MS Visual C compiler.
### C++ Builder:
  Refer to the instructions inside `FastMM4BCB.cpp`.

