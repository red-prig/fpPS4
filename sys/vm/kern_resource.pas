unit kern_resource;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam;

const
 RLIMIT_CPU    = 0;  // maximum cpu time in seconds
 RLIMIT_FSIZE  = 1;  // maximum file size
 RLIMIT_DATA   = 2;  // data size
 RLIMIT_STACK  = 3;  // stack size
 RLIMIT_CORE   = 4;  // core file size
 RLIMIT_RSS    = 5;  // resident set size
 RLIMIT_MEMLOCK= 6;  // locked-in-memory address space
 RLIMIT_NPROC  = 7;  // number of processes
 RLIMIT_NOFILE = 8;  // number of open files
 RLIMIT_SBSIZE = 9;  // maximum size of all socket buffers
 RLIMIT_VMEM   =10;  // virtual process size (incl. mmap)
 RLIMIT_AS     =RLIMIT_VMEM; // standard name for RLIMIT_VMEM
 RLIMIT_NPTS   =11;  // pseudo-terminals
 RLIMIT_SWAP   =12;  // swap used

function lim_max(which:Integer):QWORD;
function lim_cur(which:Integer):QWORD;

implementation

function lim_max(which:Integer):QWORD;
begin
 Result:=0;
 Case which of
  RLIMIT_DATA   :Result:=MAXDSIZ;
  RLIMIT_STACK  :Result:=MAXSSIZ;
  RLIMIT_MEMLOCK:Result:=pageablemem;
  RLIMIT_VMEM   :Result:=pageablemem;
  else;
 end;
end;

function lim_cur(which:Integer):QWORD;
begin
 Result:=0;
 Case which of
  RLIMIT_DATA   :Result:=MAXDSIZ;
  RLIMIT_STACK  :Result:=MAXSSIZ;
  RLIMIT_MEMLOCK:Result:=pageablemem;
  RLIMIT_VMEM   :Result:=pageablemem;
  else;
 end;
end;

end.

