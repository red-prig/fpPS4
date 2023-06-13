unit sys_cpuset;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_cpuset;

type
 p_cpuset_t=kern_cpuset.p_cpuset_t;
 cpuset_t  =kern_cpuset.cpuset_t;

const
 CPU_LEVEL_ROOT   =1; // All system cpus.
 CPU_LEVEL_CPUSET =2; // Available cpus for which.
 CPU_LEVEL_WHICH  =3; // Actual mask/id for which.

 CPU_WHICH_TID   =1; // Specifies a thread id.
 CPU_WHICH_PID   =2; // Specifies a process id.
 CPU_WHICH_CPUSET=3; // Specifies a set id.
 CPU_WHICH_IRQ   =4; // Specifies an irq #.
 CPU_WHICH_JAIL  =5; // Specifies a jail id.

implementation


end.

