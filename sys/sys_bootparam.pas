unit sys_bootparam;

{$mode ObjFPC}{$H+}

interface

uses
 host_ipc_interface;

const
 CPUID_BASE_MODE=$710f13; //$710f31
 CPUID_NEO_MODE =$740f00;

var
 p_system_sdk_version :DWORD=$10010001; // $10010001;
 p_cpuid              :DWORD=CPUID_BASE_MODE; //base mode = 0x710f13 / neo mode = 0x740f00
 p_base_ps4_mode      :DWORD=1; //[0..1]
 p_neomode            :DWORD=0; //[0..1]

 p_halt_on_exit       :DWORD=0;
 p_print_guest_syscall:DWORD=0;
 p_print_pmap         :DWORD=0;
 p_print_jit_preload  :DWORD=0;

 p_host_ipc:THostIpcInterface;

implementation

end.

