unit _resource;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time;

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
 RLIM_NLIMITS  =13;  // number of resource limits

 RLIM_INFINITY =(QWORD(1) shl 63)-1;

 maxprocperuid =4*1024;

type
 p_rlimit=^t_rlimit;
 t_rlimit=packed record
  rlim_cur:QWORD; //current (soft) limit
  rlim_max:QWORD; //maximum value for rlim_cur
 end;

const
 RUSAGE_SELF    = 0;
 RUSAGE_CHILDREN=-1;
 RUSAGE_THREAD  = 1;

type
 p_rusage=^t_rusage;
 t_rusage=packed record
  ru_utime   :timeval; // user time used
  ru_stime   :timeval; // system time used
  ru_maxrss  :QWORD;   // max resident set size
  ru_ixrss   :QWORD;   // integral shared memory size *
  ru_idrss   :QWORD;   // integral unshared data
  ru_isrss   :QWORD;   // integral unshared stack
  ru_minflt  :QWORD;   // page reclaims
  ru_majflt  :QWORD;   // page faults
  ru_nswap   :QWORD;   // swaps
  ru_inblock :QWORD;   // block input operations
  ru_oublock :QWORD;   // block output operations
  ru_msgsnd  :QWORD;   // messages sent
  ru_msgrcv  :QWORD;   // messages received
  ru_nsignals:QWORD;   // signals received
  ru_nvcsw   :QWORD;   // voluntary context switches
  ru_nivcsw  :QWORD;   // involuntary
 end;
 {$IF sizeof(t_rusage)<>144}{$STOP sizeof(t_rusage)<>144}{$ENDIF}

const
 //Process priority specifications to get/setpriority.
 PRIO_MIN=-20;
 PRIO_MAX= 20;

 PRIO_PROCESS=0;
 PRIO_PGRP   =1;
 PRIO_USER   =2;

implementation

end.

