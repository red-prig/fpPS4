Fast Memory Manager - Readme
----------------------------

Description:
------------

A fast replacement memory manager for Embarcadero Delphi Win32 applications that scales well under multi-threaded usage, is not prone to memory fragmentation, and supports shared memory without the use of external .DLL files.



Homepage:
---------

https://github.com/pleriche/FastMM4



Usage:
------

Delphi: Place this unit as the very first unit under the "uses" section in your project's .dpr file. When sharing memory between an application and a DLL (e.g. when passing a long string or dynamic array to a DLL function), both the main application and the DLL must be compiled using this memory manager (with the required conditional defines set). There are some conditional defines (inside FastMM4Options.inc) that may be used to tweak the memory manager. To enable support for a user mode address space greater than 2GB you will have to use the EditBin* tool to set the LARGE_ADDRESS_AWARE flag in the EXE header. This informs Windows x64 or Windows 32-bit (with the /3GB option set) that the application supports an address space larger than 2GB (up to 4GB). In Delphi 6 and later you can also specify this flag through the compiler directive {$SetPEFlags $20}
*The EditBin tool ships with the MS Visual C compiler.
C++ Builder 6: Refer to the instructions inside FastMM4BCB.cpp.



License:
--------

This work is copyright Professional Software Development / Pierre le Riche. It is released under a dual license, and you may choose to use it under either the Mozilla Public License 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html) or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from http://www.opensource.org/licenses/lgpl-license.php). If you find FastMM useful or you would like to support further development, a donation would be much appreciated. My banking details are:
  Country: South Africa
  Bank: ABSA Bank Ltd
  Branch: Somerset West
  Branch Code: 334-712
  Account Name: PSD (Distribution)
  Account No.: 4041827693
  Swift Code: ABSAZAJJ
My PayPal account is:
   bof@psd.co.za



Contact Details:
----------------

My contact details are shown below if you would like to get in touch with me. If you use this memory manager I would like to hear from you: please e-mail me your comments - good and bad.

Snailmail:
  PO Box 2514
  Somerset West
  7129
  South Africa

E-mail:
  plr@psd.co.za



Support:
--------

If you have trouble using FastMM, you are welcome to drop me an e-mail at the address above, or you may post your questions in the BASM newsgroup on the Embarcadero news server (which is where I hang out quite frequently).



Disclaimer:
-----------

FastMM has been tested extensively with both single and multithreaded applications on various hardware platforms, but unfortunately I am not in a position to make any guarantees. Use it at your own risk.



This archive contains:
----------------------

1) FastMM4.pas - The replacement memory manager (to speed up your applications)

2) CPP Builder Support\FastMM4BCB.cpp - The Borland C++ Builder 6 support unit for FastMM4

3) Replacement BorlndMM DLL\BorlndMM.dpr - The project to build a replacement borlndmm.dll (to speed up the Delphi IDE)

4) FullDebugMode DLL\FastMM_FullDebugMode.dpr - The project to build the FastMM_FullDebugMode.dll. This support DLL is required only when using "FullDebugMode".

5) Usage Tracker\FastMMUsageTracker.pas - The address space and memory manager state monitoring utility for FastMM. (A demo is included in the same folder.)

6) Translations - This folder contains FastMM4Messages.pas files translated to various languages. The default FastMM4Messages.pas (in this folder) is the English version.

Documentation for each part is available inside its folder and also as comments inside the source. Refer to the FAQ if you have any questions, or contact me via e-mail.


FastMM Optional Features (FastMM4Options.Inc):
----------------------------------------------

The default options in FastMM4Options.Inc are configured for optimal performance when FastMM4.pas is added as the first unit in the uses clause of the .dpr. There are various other options available that control the sharing of the memory manager between libraries and the main application, as well as the debugging features of FastMM. There is a short description for each option inside the FastMM4Options.inc file that explains what the option does.

By default, memory leak checking is enabled only if the application is being run inside the debugger, and on shutdown FastMM will report all unexpected memory leaks. (Expected memory leaks can be registered beforehand.)

"FullDebugMode" is a special mode that radically changes the way in which FastMM works, and is intended as an aid in debugging applications. When the "FullDebugMode" define is set, FastMM places a header and footer around every memory block in order to catch memory overwrite bugs. It also stores a stack trace whenever a block is allocated or freed, and these stack traces are displayed if FastMM detects an error involving the block. When blocks are freed they are filled with a special byte pattern that allows FastMM to detect blocks that were modified after being freed (blocks are checked before being reused, and also on shutdown), and also to detect when a virtual method of a freed object is called. FastMM can also be set to detect the use of an interface of a freed object, but this facility is mutually exclusive to the detection of invalid virtual method calls. When "FullDebugMode" is enabled then the FastMM_FullDebugMode.dll library will be required by the application, otherwise not.


FastMM Technical Details:
-------------------------

FastMM is actually three memory managers in one: small (<2.5K), medium (< 260K) and large (> 260K) blocks are managed separately.

Requests for large blocks are passed through to the operating system (VirtualAlloc) to be allocated from the top of the address space. (Medium and small blocks are allocated from the bottom of the address space - keeping them separate improves fragmentation behaviour).

The medium block manager obtains memory from the OS in 1.25MB chunks. These chunks are called "medium block pools" and are subdivided into medium blocks as the application requests them. Unused medium blocks are kept in double-linked lists. There are 1024 such lists, and since the medium block granularity is 256 bytes that means there is a bin for every possible medium block size. FastMM maintains a two-level "bitmap" of these lists, so there is never any need to step through them to find a suitable unused block - a few bitwise operations on the "bitmaps" is all that is required. Whenever a medium block is freed, FastMM checks the neighbouring blocks to determine whether they are unused and can thus be combined with the block that is being freed. (There may never be two neighbouring medium blocks that are both unused.) FastMM has no background "clean-up" thread, so everything must be done as part of the freemem/getmem/reallocmem call.

In an object oriented programming language like Delphi, most memory allocations and frees are usually for small objects. In practical tests with various Delphi applications it was found that, on average, over 99% of all memory operations involve blocks <2K. It thus makes sense to optimize specifically for these small blocks. Small blocks are allocated from "small block pools". Small block pools are actually medium blocks that are subdivided into equal sized small blocks. Since a particular small block pool contains only equal sized blocks, and adjacent free small blocks are never combined, it allows the small block allocator to be greatly simplified and thus much faster. FastMM maintains a double-linked list of pools with available blocks for every small block size, so finding an available block for the requested size when servicing a getmem request is very speedy.

Moving data around in memory is typically a very expensive operation. Consequently, FastMM thus an intelligent reallocation algorithm to avoid moving memory as much as possible. When a block is upsized FastMM adjusts the block size in anticipation of future upsizes, thus improving the odds that the next reallocation can be done in place. When a pointer is resized to a smaller size, FastMM requires the new size to be significantly smaller than the old size otherwise the block will not be moved.

Speed is further improved by an improved locking mechanism: Every small block size, the medium blocks and large blocks are locked individually. If, when servicing a getmem request, the optimal block type is locked by another thread, then FastMM will try up to three larger block sizes. This design drastically reduces the number of thread contentions and improves performance for multi-threaded applications.


Important Notes Regarding Delphi 2005:
--------------------------------------

Presently the latest service pack for Delphi 2005 is SP3, but unfortunately there are still bugs that prevent a replacement borlndmm.dll from working stably with the Delphi 2005 IDE. There is a collection of unofficial patches that need to be installed before you can use the replacement borlndmm.dll with the Delphi 2005 IDE. You can get it from:

http://cc.embarcadero.com/item.aspx?id=23618

Installing these patches together with the replacement borlndmm.dll should provide you with a faster and more stable Delphi 2005 IDE.

