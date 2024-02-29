unit elf_machdep;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysent,
 vmparam,
 signal;

var
 self_orbis_sysvec:t_sysentvec=(
  sv_size              :0;
  sv_table             :nil;
  sv_fixup             :nil; //self_orbis_fixup
  sv_sendsig           :nil; //sendsig
  sv_sigcode           :nil; //sigcode
  sv_szsigcode         :nil; //szsigcode 0x20
  sv_name              :'ORBIS kernel SELF';
  sv_minsigstksz       :MINSIGSTKSZ;
  sv_pagesize          :PAGE_SIZE;
  sv_minuser           :0;
  sv_maxuser           :$800000000000;
  sv_stackprot         :3;
  sv_copyout_strings   :nil; //exec_copyout_strings
  sv_setregs           :nil; //exec_setregs
  sv_set_syscall_retval:nil; //cpu_set_syscall_retval
  sv_fetch_syscall_args:nil; //cpu_fetch_syscall_args
  sv_shared_page_len   :PAGE_SIZE;
  sv_sigcode_base      :nil;
 );

procedure init_sysvec;

implementation

var
 sv_size :Integer; external;
 sv_table:array[0..0] of t_sysent; external;

 guest_sigcode  :array[0..0] of Byte; external;
 guest_szsigcode:Integer; external;

procedure init_sysvec;
begin
 self_orbis_sysvec.sv_size     :=sv_size;
 self_orbis_sysvec.sv_table    :=@sv_table;
 self_orbis_sysvec.sv_sigcode  :=@guest_sigcode;
 self_orbis_sysvec.sv_szsigcode:=@guest_szsigcode;
end;


end.



