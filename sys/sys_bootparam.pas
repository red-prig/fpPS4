unit sys_bootparam;

{$mode ObjFPC}{$H+}

interface

uses
 host_ipc_interface;

const
 CPUID_BASE_MODE=$710f13; // $710f31
 CPUID_NEO_MODE =$740f00;

var
 p_system_sdk_version :DWORD=$10010001; // $10010001;
 p_cpuid              :DWORD=CPUID_BASE_MODE; //base mode = 0x710f13 / neo mode = 0x740f00
 p_base_ps4_mode      :DWORD=1; //[0..1]
 p_neomode            :DWORD=0; //[0..1]

 p_halt_on_exit       :Boolean=False;
 p_print_guest_syscall:Boolean=False;
 p_print_pmap         :Boolean=False;
 p_print_jit_preload  :Boolean=False;

 p_print_gpu_ops      :Boolean=False;
 p_print_gpu_hint     :Boolean=False;

 p_host_ipc           :THostIpcInterface=nil;
 p_host_handler       :THostIpcHandler  =nil;

function p_host_ipc_td:Pointer;

procedure set_neo_mode(neo:Boolean);

implementation

procedure set_neo_mode(neo:Boolean);
begin
 case neo of
  False:
   begin
    p_cpuid        :=CPUID_BASE_MODE;
    p_base_ps4_mode:=1;
    p_neomode      :=0;
   end;
  True:
   begin
    p_cpuid        :=CPUID_NEO_MODE;
    p_base_ps4_mode:=0;
    p_neomode      :=1;
   end;
 end;
end;

function p_host_ipc_td:Pointer;
begin
 Result:=nil;
 if (p_host_ipc<>nil) then
 begin
  Result:=p_host_ipc.Ftd;
 end;
end;

end.

