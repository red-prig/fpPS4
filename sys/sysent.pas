unit sysent;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_sysent=^t_sysent;
 t_sysent=packed record
  sy_narg:ptruint;
  sy_call:Pointer;
  sy_name:PChar;
 end;

 p_sysentvec=^t_sysentvec;
 t_sysentvec=record
  sv_size              :Integer;
  sv_table             :p_sysent;
  sv_fixup             :Pointer;  //self_orbis_fixup
  sv_sendsig           :Pointer;  //sendsig
  sv_sigcode           :Pointer;  //sigcode
  sv_szsigcode         :PInteger; //szsigcode 0x20
  sv_name              :PChar;    //ORBIS kernel SELF
  sv_minsigstksz       :Integer;  //0x800
  sv_pagesize          :Integer;  //0x4000
  sv_minuser           :QWORD;    //0x0
  sv_maxuser           :QWORD;    //0x800000000000
  sv_stackprot         :Integer;  //0x3
  sv_copyout_strings   :Pointer;  //exec_copyout_strings
  sv_setregs           :Pointer;  //exec_setregs
  sv_set_syscall_retval:Pointer;  //cpu_set_syscall_retval
  sv_fetch_syscall_args:Pointer;  //cpu_fetch_syscall_args
  sv_shared_page_len   :QWORD;    //0x4000
  sv_sigcode_base      :Pointer;
 end;

implementation

end.



