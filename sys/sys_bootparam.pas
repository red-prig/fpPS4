unit sys_bootparam;

{$mode ObjFPC}{$H+}

interface

uses
 host_ipc_interface;

const
 CPUID_BASE_MODE=$710f13; // $710f31
 CPUID_NEO_MODE =$740f00;

var
 p_system_sdk_version :DWORD=$11008001; // $10010001;
 p_cpuid              :DWORD=CPUID_BASE_MODE; //base mode = 0x710f13 / neo mode = 0x740f00
 p_base_ps4_mode      :DWORD=1; //[0..1]
 p_neomode            :DWORD=0; //[0..1]
 p_cpumode            :DWORD=5; //6CPU(0) 7CPU_LOW(1) COMPAT(2) 7CPU_NORMAL(5)
 p_cpuset             :QWORD=$7F;
 p_openpsid           :array[0..15] of Byte;

 //dipsw
 p_isDevelopmentMode          :Byte=0;
 p_isTestKit                  :Byte=0;
 p_IsDisableRazor             :Byte=0;
 p_IsDisableBinaryVersionCheck:Byte=0;

 p_is_fork            :Boolean=False;
 p_halt_on_exit       :Boolean=False;
 p_print_guest_syscall:Boolean=False;

 p_print_gpu_ops      :Boolean=False;
 p_print_gpu_hint     :Boolean=False;

 p_host_ipc           :THostIpc=nil;

procedure set_neo_mode(neo:Boolean);
procedure set_cpumode(cpumode:DWORD);

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

procedure set_cpumode(cpumode:DWORD);
begin
 case cpumode of
  0:p_cpuset:=$3f; //6CPU
  1:p_cpuset:=$7f; //7CPU_LOW
  2:p_cpuset:=$3f; //COMPAT
  5:p_cpuset:=$7f; //7CPU_NORMAL
  else
   Exit;
 end;

 p_cpumode:=cpumode;
end;


end.




