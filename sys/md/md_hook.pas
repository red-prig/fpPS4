unit md_hook;

{$mode ObjFPC}{$H+}

interface

Uses
 windows,
 ntapi,
 md_systm,
 md_map,
 x86_fpdbgdisas;

type
 t_dll_base_pair=record
  ProcessHandle:THandle;
  DllBaseCurr  :Pointer;
  DllBaseProc  :Pointer;
 end;

 t_hook_plan=record
  source:Pointer;
  inject:Pointer;
  p_orig:PPointer;
 end;

 t_hook_page=record
  hProcess:THandle;
  base    :Pointer;
  curr    :Pointer;
 end;

function NtQueryModuleByName(hProcess:THandle;DllName:PWideChar):t_dll_base_pair;
function NtQueryNtdllByRTUTS(hProcess:THandle;rip:QWORD):t_dll_base_pair;
function NtQueryProcByName  (const DllBase:t_dll_base_pair;name:pchar):Pointer;

Function NewHookPlan(const DllBase:t_dll_base_pair;name:pchar;inject:Pointer;p_orig:PPointer):t_hook_plan;
function NewHookPage(hProcess:THandle;lower:Pointer;size:QWORD;prot:DWORD):t_hook_page;
function WriteHook  (var page:t_hook_page;const h:t_hook_plan):Boolean;

implementation

type
 PLIST_ENTRY=^TLIST_ENTRY;
 TLIST_ENTRY=packed record
  Flink:Pointer;
  Blink:Pointer;
 end;

 PPEB_LDR_DATA=^TPEB_LDR_DATA;
 TPEB_LDR_DATA=packed record
  Length                         :ULONG;
  Initialized                    :ULONG;
  SsHandle                       :Pointer;
  InLoadOrderModuleList          :TLIST_ENTRY;
  InMemoryOrderModuleList        :TLIST_ENTRY;
  InInitializationOrderModuleList:TLIST_ENTRY;
  //....
 end;

 PLDR_DATA_TABLE_ENTRY=^TLDR_DATA_TABLE_ENTRY;
 TLDR_DATA_TABLE_ENTRY=packed record
  InLoadOrderLinks          :TLIST_ENTRY;
  InMemoryOrderLinks        :TLIST_ENTRY;
  InInitializationOrderLinks:TLIST_ENTRY;
  DllBase                   :Pointer;
  EntryPoint                :Pointer;
  SizeOfImage               :ULONG;
  _align                    :ULONG;
  FullDllName               :UNICODE_STRING;
  BaseDllName               :UNICODE_STRING;
  //....
 end;

function NtQueryPeb(hProcess:THandle;var peb:Pointer):Integer;
var
 data:array[0..SizeOf(PROCESS_BASIC_INFORMATION)-1+7] of Byte;
 p_info:PPROCESS_BASIC_INFORMATION;
begin
 p_info:=Align(@data,8);

 Result:=NtQueryInformationProcess(hProcess,
                                   ProcessBasicInformation,
                                   p_info,
                                   SizeOf(PROCESS_BASIC_INFORMATION),
                                   nil);
 if (Result=0) then
 begin
  peb:=Pointer(p_info^.PebBaseAddress);
 end;
end;

function NtQueryPebLdr(hProcess:THandle;var Ldr:PPEB_LDR_DATA):Integer;
var
 peb:Pointer;
 ImageBaseAddress:Pointer;
begin
 Result:=NtQueryPeb(hProcess,peb);
 if (Result=0) then
 begin
  Result:=md_copyin(peb+$018,@Ldr,SizeOf(Pointer),nil,hProcess);
 end;
end;

function NtQueryModuleList(hProcess:THandle;var List:TLIST_ENTRY):Integer;
var
 Ldr:PPEB_LDR_DATA;
begin
 Result:=NtQueryPebLdr(hProcess,Ldr);
 if (Result=0) then
 begin
  Result:=md_copyin(@Ldr^.InLoadOrderModuleList,@List,SizeOf(TLIST_ENTRY),nil,hProcess);
 end;
end;

function NtQueryModuleByName(hProcess:THandle;DllName:PWideChar):t_dll_base_pair;
var
 List:TLIST_ENTRY;
 node:PLDR_DATA_TABLE_ENTRY;
 data:TLDR_DATA_TABLE_ENTRY;

 BaseDllName:WideString;
begin
 Result.ProcessHandle:=hProcess;
 Result.DllBaseCurr  :=Pointer(GetModuleHandleW(DllName));
 Result.DllBaseProc  :=nil;

 if (Result.DllBaseCurr=nil) then Exit;

 if (hProcess=NtCurrentProcess) then
 begin
  Result.DllBaseProc:=Result.DllBaseCurr;
  Exit;
 end;

 BaseDllName:='';

 if (NtQueryModuleList(hProcess,List)=0) then
 begin
  node:=List.Blink;

  while (node<>nil) and (node<>Pointer(-1)) and (node<>List.Flink) do
  begin
   data:=Default(TLDR_DATA_TABLE_ENTRY);
   md_copyin(node,@data,SizeOf(TLDR_DATA_TABLE_ENTRY),nil,hProcess);

   SetLength(BaseDllName,data.BaseDllName.Length div 2);
   md_copyin(data.BaseDllName.Buffer,PWideChar(BaseDllName),data.BaseDllName.Length,nil,hProcess);

   if (BaseDllName=DllName) then
   begin
    Result.DllBaseProc:=data.DllBase;
    Break;
   end;

   node:=data.InLoadOrderLinks.Blink;
  end;

 end;

 SetLength(BaseDllName,0);
end;

function NtQueryNtdllByRTUTS(hProcess:THandle;rip:QWORD):t_dll_base_pair;
var
 addr:Pointer;
 offs:Int64;
begin
 Result.ProcessHandle:=hProcess;
 Result.DllBaseCurr  :=Pointer(GetModuleHandleW('ntdll.dll'));
 Result.DllBaseProc  :=nil;

 if (Result.DllBaseCurr=nil) then Exit;

 addr:=GetProcAddress(QWORD(Result.DllBaseCurr),'RtlUserThreadStart');
 if (addr=nil) then Exit;

 offs:=addr-Result.DllBaseCurr;

 Result.DllBaseProc:=Pointer(rip-offs);
end;

function NtQueryProcByName(const DllBase:t_dll_base_pair;name:pchar):Pointer;
var
 addr:Pointer;
 offs:Int64;
begin
 Result:=nil;

 addr:=GetProcAddress(HINST(DllBase.DllBaseCurr),name);
 if (addr=nil) then Exit;

 offs:=addr-DllBase.DllBaseCurr;

 Result:=DllBase.DllBaseProc+offs;
end;

function get_cmds_len(addr:Pointer;need:Integer):Integer;
var
 dis:TX86Disassembler;
 din:TInstruction;
 ptr,fin:Pointer;
begin
 ptr:=addr;
 fin:=addr+need;

 dis:=Default(TX86Disassembler);
 din:=Default(TInstruction);

 while (ptr<fin) do
 begin
  dis.Disassemble(dm64,ptr,din);
 end;

 Result:=Integer(ptr-addr);
end;

type
 phook_xcall=^thook_xcall;
 thook_xcall=packed record
  jmp:Word;  //0xFF 0x25
  rip:DWORD; //00000000
  dst:Pointer;
 end;

type
 t_cmd_place=array[0..15] of Byte;

const
 nop_fill:t_cmd_place=(
  $90,$90,$90,$90,$90,$90,$90,$90,
  $90,$90,$90,$90,$90,$90,$90,$90
 );

Function NewHookPlan(const DllBase:t_dll_base_pair;name:pchar;inject:Pointer;p_orig:PPointer):t_hook_plan;
begin
 Result:=Default(t_hook_plan);

 Result.source:=NtQueryProcByName(DllBase,name);
 if (Result.source=nil) then Exit;

 Result.inject:=inject;
 Result.p_orig:=p_orig;
end;

function NewHookPage(hProcess:THandle;lower:Pointer;size:QWORD;prot:DWORD):t_hook_page;
var
 r:Integer;
begin
 r:=md_mmap(lower,size,prot,0,0,hProcess);
 if (r<>0) then
 begin
  Writeln(stderr,'md_mmap:0x'+HexStr(r,8));
  Exit;
 end;

 Result.hProcess:=hProcess;
 Result.base    :=lower;
 Result.curr    :=lower;
end;

function inplace(var page:t_hook_page;const h:t_hook_plan):Pointer;
var
 place:t_cmd_place;
 len:Integer;
 i:Integer;
begin
 Result:=page.curr;

 place:=nop_fill;
 i:=md_copyin (h.source,@place,sizeof(place),nil,page.hProcess);

 len:=get_cmds_len(@place,sizeof(thook_xcall));

 i:=md_copyout(@place,page.curr,len,nil,page.hProcess);

 page.curr:=page.curr+len;

 place:=nop_fill;
 with phook_xcall(@place)^ do
 begin
  jmp:=$25FF;
  rip:=0;
  dst:=h.source+len;
 end;
 i:=md_copyout(@place,page.curr,sizeof(thook_xcall),nil,page.hProcess);

 page.curr:=page.curr+sizeof(thook_xcall);
end;

function WriteHook(var page:t_hook_page;const h:t_hook_plan):Boolean;
var
 place:t_cmd_place;
 curr:Pointer;
 num:PTRUINT;
 i:Integer;
begin
 curr:=inplace(page,h);

 place:=nop_fill;
 with phook_xcall(@place)^ do
 begin
  jmp:=$25FF;
  rip:=0;
  dst:=h.inject;
 end;
 num:=0;
 Result:=WriteProcessMemory(page.hProcess,h.source,@place,sizeof(thook_xcall),num);

 if Result then
 begin
  i:=md_copyout(@curr,h.p_orig,sizeof(Pointer),nil,page.hProcess);
 end else
 begin
  page.curr:=curr;
 end;

end;


end.



