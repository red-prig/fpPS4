unit md_systm_reserve;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 windows,
 ntapi;

type
 t_md_map_reserve_result=record
  error:DWORD;
  id   :DWORD;
  base :Pointer;
  size :QWORD;
 end;

function md_map_reserve(hProcess:THandle=NtCurrentProcess;rip:QWORD=0):t_md_map_reserve_result;

implementation

uses
 vmparam,
 md_systm,
 md_map,
 md_context,
 md_hook;

var
 md_map_init_flags:Byte=0;
 md_hook_page:Pointer;

///////////////////////////////////////////////////////

type
 t_NtCreateThreadEx=function(
           hThread         :PHandle;
           DesiredAccess   :ACCESS_MASK;
           ObjectAttributes:POBJECT_ATTRIBUTES;
           ProcessHandle   :THandle;
           StartRoutine    :Pointer;
           Argument        :Pointer;
           CreateFlags     :ULONG;   // THREAD_CREATE_FLAGS_*
           ZeroBits        :ULONG_PTR;
           StackSize       :ULONG_PTR;
           MaximumStackSize:ULONG_PTR;
           AttributeList   :PPS_ATTRIBUTE_LIST
          ):DWORD; stdcall;

var
 real_NtCreateThreadEx:t_NtCreateThreadEx=@NtCreateThreadEx;

function Inj_NtCreateThreadEx(
          hThread         :PHandle;
          DesiredAccess   :ACCESS_MASK;
          ObjectAttributes:POBJECT_ATTRIBUTES;
          ProcessHandle   :THandle;
          StartRoutine    :Pointer;
          Argument        :Pointer;
          CreateFlags     :ULONG;   // THREAD_CREATE_FLAGS_*
          ZeroBits        :ULONG_PTR;
          StackSize       :ULONG_PTR;
          MaximumStackSize:ULONG_PTR;
          AttributeList   :PPS_ATTRIBUTE_LIST
         ):DWORD; stdcall; forward;

///////////////////////////////////////////////////////

type
 t_NtMapViewOfSection=function(
          SectionHandle     :THandle;
          ProcessHandle     :THandle;
          BaseAddress       :PPointer;
          ZeroBits          :ULONG_PTR;
          CommitSize        :ULONG_PTR;
          SectionOffset     :PLARGE_INTEGER;
          ViewSize          :PULONG_PTR;
          InheritDisposition:DWORD;
          AllocationType    :ULONG;
          Protect           :ULONG
         ):DWORD; stdcall;

var
 real_NtMapViewOfSection:t_NtMapViewOfSection=@NtMapViewOfSection;

function Inj_NtMapViewOfSection(
          SectionHandle     :THandle;
          ProcessHandle     :THandle;
          BaseAddress       :PPointer;
          ZeroBits          :ULONG_PTR;
          CommitSize        :ULONG_PTR;
          SectionOffset     :PLARGE_INTEGER;
          ViewSize          :PULONG_PTR;
          InheritDisposition:DWORD;
          AllocationType    :ULONG;
          Protect           :ULONG
         ):DWORD; stdcall; forward;

///////////////////////////////////////////////////////

type
 t_NtMapViewOfSectionEx=function(
          SectionHandle         :THandle;
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          SectionOffset         :PLARGE_INTEGER;
          ViewSize              :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer; //MEM_EXTENDED_PARAMETER
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall;

var
 real_NtMapViewOfSectionEx:t_NtMapViewOfSectionEx=@NtMapViewOfSectionEx;

function Inj_NtMapViewOfSectionEx(
          SectionHandle         :THandle;
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          SectionOffset         :PLARGE_INTEGER;
          ViewSize              :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer; //MEM_EXTENDED_PARAMETER
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall; forward;

///////////////////////////////////////////////////////

type
 t_NtAllocateVirtualMemory=function(
          ProcessHandle :THandle;
          BaseAddress   :PPointer;
          ZeroBits      :ULONG_PTR;
          RegionSize    :PULONG_PTR;
          AllocationType:ULONG;
          Protect       :ULONG
         ):DWORD; stdcall;

var
 real_NtAllocateVirtualMemory:t_NtAllocateVirtualMemory=@NtAllocateVirtualMemory;

function Inj_NtAllocateVirtualMemory(
          ProcessHandle :THandle;
          BaseAddress   :PPointer;
          ZeroBits      :ULONG_PTR;
          RegionSize    :PULONG_PTR;
          AllocationType:ULONG;
          Protect       :ULONG
         ):DWORD; stdcall; forward;

///////////////////////////////////////////////////////

type
 t_NtAllocateVirtualMemoryEx=function(
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          RegionSize            :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer; //MEM_EXTENDED_PARAMETER
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall;

var
 real_NtAllocateVirtualMemoryEx:t_NtAllocateVirtualMemoryEx=@NtAllocateVirtualMemoryEx;

function Inj_NtAllocateVirtualMemoryEx(
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          RegionSize            :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer; //MEM_EXTENDED_PARAMETER
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall; forward;

///////////////////////////////////////////////////////

function md_map_reserve(hProcess:THandle=NtCurrentProcess;rip:QWORD=0):t_md_map_reserve_result;
var
 Module:t_dll_base_pair;
 page  :t_hook_page;

 temp_pmap_mem:t_addr_range_array;

 base:Pointer;
 size:QWORD;
 i:Integer;
 orig_flags:Byte;
 init_flags:Byte;
begin
 Result:=Default(t_md_map_reserve_result);

 //load flags
 init_flags:=0;
 md_copyin(@md_map_init_flags,@init_flags,sizeof(init_flags),nil,hProcess);
 orig_flags:=init_flags;

 if ((init_flags and 1)=0) then
 begin
  ///

  {
   if you reserve up to 43 bits of memory, then when searching for free memory from address 0,
   the Windows virtual memory manager starts to lag terribly, in particular,
   NtCreateThreadEx is visible here, which automatically allocates memory for the stack,
   it takes about 5 seconds to execute.
   Need to hook NtCreateThreadEx and emulate it through NtCreateThread and
   a custom allocator with setting the lower search limit to LowestStartingAddress
   and this generally worked and everything became faster.
  }

  if (hProcess<>NtCurrentProcess) and (rip<>0) then
  begin
   //RIP -> RtlUserThreadStart
   Module:=NtQueryNtdllByRTUTS(hProcess,rip);
  end else
  begin
   Module:=NtQueryModuleByName(hProcess,'ntdll.dll');
  end;

  page  :=NewHookPage(hProcess,Module.DllBaseProc,4*1024,VM_RW);

  WriteHook(page,NewHookPlan(Module,'NtCreateThreadEx'         ,@Inj_NtCreateThreadEx         ,@real_NtCreateThreadEx  ));
  WriteHook(page,NewHookPlan(Module,'NtMapViewOfSection'       ,@Inj_NtMapViewOfSection       ,@real_NtMapViewOfSection));
  WriteHook(page,NewHookPlan(Module,'NtMapViewOfSectionEx'     ,@Inj_NtMapViewOfSectionEx     ,@real_NtMapViewOfSectionEx));
  WriteHook(page,NewHookPlan(Module,'NtAllocateVirtualMemory'  ,@Inj_NtAllocateVirtualMemory  ,@real_NtAllocateVirtualMemory));
  WriteHook(page,NewHookPlan(Module,'NtAllocateVirtualMemoryEx',@Inj_NtAllocateVirtualMemoryEx,@real_NtAllocateVirtualMemoryEx));

  //allow execute
  i:=md_protect(page.base,4*1024,VM_RX,hProcess);

  //save change
  i:=md_copyout(@page.base,@md_hook_page,sizeof(Pointer),nil,hProcess);

  ///
  init_flags:=init_flags or 1;
 end;

 if ((init_flags and 2)=0) and (Length(temp_pmap_mem)<>0) then
 begin

  //copy original
  temp_pmap_mem:=initial_pmap_mem;

  i:=0;
  while (i<=High(temp_pmap_mem)) do
  begin
   base:=Pointer(temp_pmap_mem[i].start);

   //try union range
   if (base=Pointer(DL_AREA_START)) then
   begin
    size:=VM_MAXUSER_ADDRESS-DL_AREA_START;

    Result.error:=md_placeholder_mmap(base,size,MD_MAP_FIXED,hProcess);
    if (Result.error=0) then
    begin
     //union range
     temp_pmap_mem[i+0].__end:=VM_MAXUSER_ADDRESS;
     temp_pmap_mem[i+1].start:=VM_MAXUSER_ADDRESS;
     //
     i:=i+2;
     Continue;
    end;
   end;

   base:=Pointer(temp_pmap_mem[i].start);
   size:=temp_pmap_mem[i].__end-temp_pmap_mem[i].start;

   Result.error:=md_placeholder_mmap(base,size,MD_MAP_FIXED,hProcess);

   if (Result.error<>0) then
   begin
    Result.id  :=i;
    Result.base:=base;
    Result.size:=size;
    //STATUS_COMMITMENT_LIMIT = $C000012D
    Exit;
   end;

   //update start region
   temp_pmap_mem[i].start:=QWORD(base);

   i:=i+1;
  end;

  //save change
  md_copyout(@temp_pmap_mem,@pmap_mem,sizeof(pmap_mem),nil,hProcess);

  init_flags:=init_flags or 2;
 end;

 //save change
 if (orig_flags<>init_flags) then
 begin
  md_copyout(@init_flags,@md_map_init_flags,sizeof(init_flags),nil,hProcess);
 end;
end;

const
 C_REQ:TMEM_ADDRESS_REQUIREMENTS=(
  LowestStartingAddress:Pointer(KERNEL_LOWER);
  HighestEndingAddress :nil;
  Alignment            :0
 );
 C_EXT:TMEM_EXTENDED_PARAMETER=(
  pType  :MemExtendedParameterAddressRequirements;
  Pointer:@C_REQ
 );

type
 TMEM_EXTENDED_PARAMETER_4=array[0..3] of TMEM_EXTENDED_PARAMETER;

 TEXT_PATCHER=object
  data:TMEM_EXTENDED_PARAMETER_4;
  REQ :TMEM_ADDRESS_REQUIREMENTS;
  Procedure Patch(var EParams:Pointer;var ECounts:ULONG);
 end;

Procedure TEXT_PATCHER.Patch(var EParams:Pointer;var ECounts:ULONG); inline;
var
 P:TMEM_EXTENDED_PARAMETER;
 R:TMEM_ADDRESS_REQUIREMENTS;
 i:Byte;
 used:Boolean;
begin
 if (EParams=nil) or (ECounts=0) then
 begin
  EParams:=@C_EXT;
  ECounts:=1;
 end else
 if (ECounts<=Length(data)) then
 begin
  REQ:=C_REQ;
  used:=False;
  For i:=0 to ECounts-1 do
  begin
   P:=PMEM_EXTENDED_PARAMETER(EParams)[i]; //if use md_copyin then the initial load will crash 0xC0000005

   case P.pType of
    MemExtendedParameterAddressRequirements:
      begin
       R:=PMEM_ADDRESS_REQUIREMENTS(P.Pointer)^;

       if (R.LowestStartingAddress>REQ.LowestStartingAddress) then
       begin
        REQ.LowestStartingAddress:=R.LowestStartingAddress;
       end;

       //Ignore HighestEndingAddress

       if (R.Alignment>REQ.Alignment) then
       begin
        REQ.Alignment:=R.Alignment;
       end;

       P.Pointer:=@REQ;
       data[i]:=P;
       used:=True;
      end;
    else
      begin
       data[i]:=P;
      end;
   end; //case

  end; //for

  if (not used) then
  begin
   if (ECounts=Length(data)) then Exit;

   i:=ECounts;

   data[i]:=C_EXT;

   EParams:=@data;
   ECounts:=i+1;
  end;

 end;

end;

///////////////////////////////////////////////////////

procedure BaseInitializeStack(InitialTeb  :PINITIAL_TEB;
                              StackAddress:Pointer;
                              StackSize   :Ptruint); inline;
begin
 InitialTeb^.PreviousStackBase :=nil;
 InitialTeb^.PreviousStackLimit:=nil;
 InitialTeb^.StackBase         :=StackAddress+StackSize;  //start addr
 InitialTeb^.StackLimit        :=StackAddress;            //lo addr
 InitialTeb^.AllocatedStackBase:=StackAddress;            //DeallocationStack
end;

var
 BaseThreadInitThunk:Pointer=nil; //ECX->0 RDX->StartRoutine R8->Argument

procedure BaseInitializeContext(Context     :PCONTEXT;
                                Parameter   :Pointer;
                                StartAddress:Pointer;
                                StackAddress:Pointer); inline;
begin
 Context^:=Default(TCONTEXT);

 if (BaseThreadInitThunk=nil) then
 begin
  BaseThreadInitThunk:=GetProcAddress(GetModuleHandle('kernel32.dll'),'BaseThreadInitThunk');
 end;

 // Setup the Initial Win64 Thread Context
 Context^.Rsp:=ptruint(StackAddress - $A8);
 Context^.Rdx:=ptruint(StartAddress);
 Context^.R8 :=ptruint(Parameter);
 Context^.Rip:=ptruint(BaseThreadInitThunk);

 Context^.SegGs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegEs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegDs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegCs:=KGDT64_R3_CODE  or RPL_MASK;
 Context^.SegSs:=KGDT64_R3_DATA  or RPL_MASK;
 Context^.SegFs:=KGDT64_R3_CMTEB or RPL_MASK;

 Context^.EFlags:=$3000 or EFLAGS_INTERRUPT_MASK;

 Context^.MxCsr:=__INITIAL_MXCSR__;

 Context^.FltSave.ControlWord:=__INITIAL_FPUCW__;
 //Context^.FltSave.StatusWord: WORD;
 Context^.FltSave.MxCsr      :=__INITIAL_MXCSR__;
 Context^.FltSave.MxCsr_Mask :=__INITIAL_MXCSR_MASK__;

 Context^.ContextFlags:=CONTEXT_THREAD;
 //Context^.ContextFlags:=CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS;
end;

function NtQueryTeb(td_handle:THandle;var teb:Pointer):Integer;
var
 data:array[0..SizeOf(THREAD_BASIC_INFORMATION)-1+7] of Byte;
 P_TBI:PTHREAD_BASIC_INFORMATION;
begin
 Result:=0;
 teb:=nil;
 P_TBI:=Align(@data,8);
 P_TBI^:=Default(THREAD_BASIC_INFORMATION);

 Result:=NtQueryInformationThread(
          td_handle,
          ThreadBasicInformation,
          P_TBI,
          SizeOf(THREAD_BASIC_INFORMATION),
          nil);
 if (Result<>0) then Exit;

 teb:=P_TBI^.TebBaseAddress;
end;

const
 MinStackSize=$1000000;

function Inj_NtCreateThreadEx(
          hThread         :PHandle;
          DesiredAccess   :ACCESS_MASK;
          ObjectAttributes:POBJECT_ATTRIBUTES;
          ProcessHandle   :THandle;
          StartRoutine    :Pointer;
          Argument        :Pointer;
          CreateFlags     :ULONG;   // THREAD_CREATE_FLAGS_*
          ZeroBits        :ULONG_PTR;
          StackSize       :ULONG_PTR;
          MaximumStackSize:ULONG_PTR;
          AttributeList   :PPS_ATTRIBUTE_LIST
         ):DWORD; stdcall;
var

 _ClientId  :array[0..SizeOf(TCLIENT_ID  )+14] of Byte;
 _InitialTeb:array[0..SizeOf(TINITIAL_TEB)+14] of Byte;
 _Context   :array[0..SizeOf(TCONTEXT    )+14] of Byte;

 ClientId  :PCLIENT_ID;
 InitialTeb:PINITIAL_TEB;
 Context   :PCONTEXT;

 stack_base:Pointer;
 Stack     :Pointer;

 teb       :Pointer;

 Attribute :PS_ATTRIBUTE;

 a,count:Integer;
begin

 if (ZeroBits<>0) then
 begin
  Result:=real_NtCreateThreadEx(
          hThread         ,
          DesiredAccess   ,
          ObjectAttributes,
          ProcessHandle   ,
          StartRoutine    ,
          Argument        ,
          CreateFlags     ,
          ZeroBits        ,
          StackSize       ,
          MaximumStackSize,
          AttributeList
         );
  Exit;
 end;

 ClientId  :=Align(@_ClientId  ,16);
 InitialTeb:=Align(@_InitialTeb,16);
 Context   :=Align(@_Context   ,16);

 ClientId^.UniqueProcess:=hThread^;
 ClientId^.UniqueThread :=ProcessHandle;

 if (MaximumStackSize<MinStackSize)     then MaximumStackSize:=MinStackSize;
 if (StackSize       <MinStackSize)     then StackSize       :=MinStackSize;
 if (StackSize       <MaximumStackSize) then StackSize       :=MaximumStackSize;

 StackSize:=MinStackSize;

 stack_base:=nil;
 Result:=real_NtAllocateVirtualMemoryEx(
          ProcessHandle            ,
          @stack_base              ,
          @StackSize               ,
          MEM_COMMIT or MEM_RESERVE,
          PAGE_READWRITE           ,
          @C_EXT                   ,
          1
         );
 if (Result<>0) then Exit;

 BaseInitializeStack(InitialTeb,stack_base,StackSize);

 Stack:=stack_base+StackSize;
 Stack:=Pointer((ptruint(Stack) and (not $F)));

 BaseInitializeContext(Context,
                       Argument,
                       StartRoutine,
                       Stack);

 Result:=NtCreateThread(
          hThread,
          DesiredAccess,
          ObjectAttributes,
          ProcessHandle,
          ClientId,
          Context,
          InitialTeb,
          (CreateFlags and THREAD_CREATE_FLAGS_CREATE_SUSPENDED)<>0);

 if (Result=0) and (AttributeList<>nil) then
 begin
  count:=AttributeList^.TotalLength div sizeof(PS_ATTRIBUTE);

  if (count<>0) then
  for a:=0 to count-1 do
  begin

   Attribute:=AttributeList^.Attributes[a];

   case (Attribute.Attribute and PS_ATTRIBUTE_NUMBER_MASK) of

     PsAttributeClientId:
      if (Attribute.size<>0) and (Attribute.Value<>0) then
      begin
       if (Attribute.size>sizeof(TCLIENT_ID)) then Attribute.size:=sizeof(TCLIENT_ID);
       Move(ClientId^,Pointer(Attribute.Value)^,Attribute.size);
       if (Attribute.ReturnLength<>nil) then
       begin
        Attribute.ReturnLength^:=Attribute.size;
       end;
      end;
     PsAttributeTebAddress:
      if (Attribute.size<>0) and (Attribute.Value<>0) then
      begin
       if (Attribute.size>sizeof(Pointer)) then Attribute.size:=sizeof(Pointer);
       teb:=nil;
       if (NtQueryTeb(hThread^,teb)=0) then
       begin
        Move(teb,Pointer(Attribute.Value)^,Attribute.size);
        if (Attribute.ReturnLength<>nil) then
        begin
         Attribute.ReturnLength^:=Attribute.size;
        end;
       end;
      end;

     else;
   end;

  end;

 end;

end;

///////////////////////////////////////////////////////

function Inj_NtMapViewOfSection(
          SectionHandle     :THandle;
          ProcessHandle     :THandle;
          BaseAddress       :PPointer;
          ZeroBits          :ULONG_PTR;
          CommitSize        :ULONG_PTR;
          SectionOffset     :PLARGE_INTEGER;
          ViewSize          :PULONG_PTR;
          InheritDisposition:DWORD;
          AllocationType    :ULONG;
          Protect           :ULONG
         ):DWORD; stdcall;
begin
 AllocationType:=AllocationType and (not MEM_TOP_DOWN);

 if (ZeroBits=0) and
    (ViewSize<>nil) and
    (BaseAddress<>nil) then
 begin
  if (CommitSize=ViewSize^) and
     (BaseAddress^=nil) then
  begin

   Result:=real_NtMapViewOfSectionEx(
             SectionHandle ,
             ProcessHandle ,
             BaseAddress   ,
             SectionOffset ,
             ViewSize      ,
             AllocationType,
             Protect       ,
             @C_EXT        ,
             1
            );


   Exit;
  end;
 end;

 Result:=real_NtMapViewOfSection(
          SectionHandle     ,
          ProcessHandle     ,
          BaseAddress       ,
          ZeroBits          ,
          CommitSize        ,
          SectionOffset     ,
          ViewSize          ,
          InheritDisposition,
          AllocationType    ,
          Protect
         );

end;

///////////////////////////////////////////////////////

function Inj_NtMapViewOfSectionEx(
          SectionHandle         :THandle;
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          SectionOffset         :PLARGE_INTEGER;
          ViewSize              :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer; //MEM_EXTENDED_PARAMETER
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall;
var
 EXT:TEXT_PATCHER;
begin
 AllocationType:=AllocationType and (not MEM_TOP_DOWN);

 if (BaseAddress<>nil) then
 if (BaseAddress^=nil) then
 begin
  EXT.Patch(ExtendedParameters,ExtendedParameterCount);
 end;

 Result:=real_NtMapViewOfSectionEx(
          SectionHandle         ,
          ProcessHandle         ,
          BaseAddress           ,
          SectionOffset         ,
          ViewSize              ,
          AllocationType        ,
          Protect               ,
          ExtendedParameters    ,
          ExtendedParameterCount
         );

end;

///////////////////////////////////////////////////////

function Inj_NtAllocateVirtualMemory(
          ProcessHandle :THandle;
          BaseAddress   :PPointer;
          ZeroBits      :ULONG_PTR;
          RegionSize    :PULONG_PTR;
          AllocationType:ULONG;
          Protect       :ULONG
         ):DWORD; stdcall;
begin
 AllocationType:=AllocationType and (not MEM_TOP_DOWN);

 if (ZeroBits=0) and
    (BaseAddress<>nil) then
 begin
  if (BaseAddress^=nil) then
  begin

   Result:=real_NtAllocateVirtualMemoryEx(
            ProcessHandle ,
            BaseAddress   ,
            RegionSize    ,
            AllocationType,
            Protect       ,
            @C_EXT        ,
            1
           );

   Exit;
  end;
 end;

 Result:=real_NtAllocateVirtualMemory(
          ProcessHandle ,
          BaseAddress   ,
          ZeroBits      ,
          RegionSize    ,
          AllocationType,
          Protect
         );

end;

///////////////////////////////////////////////////////

function Inj_NtAllocateVirtualMemoryEx(
          ProcessHandle         :THandle;
          BaseAddress           :PPointer;
          RegionSize            :PULONG_PTR;
          AllocationType        :ULONG;
          Protect               :ULONG;
          ExtendedParameters    :Pointer; //MEM_EXTENDED_PARAMETER
          ExtendedParameterCount:ULONG
         ):DWORD; stdcall;

var
 EXT:TEXT_PATCHER;
begin
 AllocationType:=AllocationType and (not MEM_TOP_DOWN);

 if (BaseAddress<>nil) then
 if (BaseAddress^=nil) then
 begin
  EXT.Patch(ExtendedParameters,ExtendedParameterCount);
 end;

 Result:=real_NtAllocateVirtualMemoryEx(
          ProcessHandle         ,
          BaseAddress           ,
          RegionSize            ,
          AllocationType        ,
          Protect               ,
          ExtendedParameters    ,
          ExtendedParameterCount
         );

end;

///////////////////////////////////////////////////////

end.

