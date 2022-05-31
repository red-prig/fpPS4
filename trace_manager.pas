unit trace_manager;

{$mode objfpc}{$H+}

interface

uses
 stub_manager,
 ps4_program,
 ps4libdoc;

type
 PTraceInfo=^TTraceInfo;
 TTraceInfo=packed record
  nid:QWORD;
  lib:PLIBRARY;
  origin:Pointer;
  trace_enter:Pointer;
  trace_exit:Pointer;
 end;

 TStubMemoryTrace=object(TStubMemory)
  function NewTraceStub(nid:QWORD;lib,proc,trace_enter,trace_exit:Pointer):Pointer;
 end;

procedure _set_trace_local_print(enable:Boolean);
procedure _trace_enter(info:PTraceInfo;src:Pointer); MS_ABI_Default;
function  _trace_exit(info:PTraceInfo):Pointer; MS_ABI_Default;

implementation

type
 P_trace_cb_stub=^T_trace_cb_stub;
 T_trace_cb_stub=packed record
  info:TTraceInfo;
  stub:array[0..96] of Byte;
 end;

const
 _trace_cb_stub:T_trace_cb_stub=(

  info:(
   nid:0;
   lib:nil;
   origin:nil;
   trace_enter:nil;
   trace_exit:nil);

  stub:(
   $50,                         //push   %rax
   $51,                         //push   %rcx
   $52,                         //push   %rdx
   $41,$50,                     //push   %r8
   $41,$51,                     //push   %r9
   $41,$52,                     //push   %r10
   $41,$53,                     //push   %r11
   $48,$8b,$54,$24,$38,         //mov    0x38(%rsp),%rdx
   $48,$8d,$0d,$c1,$ff,$ff,$ff, //lea    -0x3f(%rip),%rcx
   $ff,$15,$d3,$ff,$ff,$ff,     //callq  *-0x2d(%rip)
   $41,$5b,                     //pop    %r11
   $41,$5a,                     //pop    %r10
   $41,$59,                     //pop    %r9
   $41,$58,                     //pop    %r8
   $5a,                         //pop    %rdx
   $59,                         //pop    %rcx
   $58,                         //pop    %rax
   $48,$8d,$64,$24,$08,         //lea    0x8(%rsp),%rsp
   $ff,$15,$b5,$ff,$ff,$ff,     //callq  *-0x4b(%rip)
   $48,$8d,$64,$24,$f8,         //lea    -0x8(%rsp),%rsp
   $50,                         //push   %rax
   $51,                         //push   %rcx
   $52,                         //push   %rdx
   $41,$50,                     //push   %r8
   $41,$51,                     //push   %r9
   $41,$52,                     //push   %r10
   $41,$53,                     //push   %r11
   $48,$8d,$0d,$8e,$ff,$ff,$ff, //lea    -0x72(%rip),%rcx
   $ff,$15,$a8,$ff,$ff,$ff,     //callq  *-0x58(%rip)
   $48,$89,$44,$24,$38,         //mov    %rax,0x38(%rsp)
   $41,$5b,                     //pop    %r11
   $41,$5a,                     //pop    %r10
   $41,$59,                     //pop    %r9
   $41,$58,                     //pop    %r8
   $5a,                         //pop    %rdx
   $59,                         //pop    %rcx
   $58,                         //pop    %rax
   $c3                          //retq
  );

 );

{
 .quad 0 //nid:QWORD;    //-40
 .quad 0 //lib:PLIBRARY; //-32
 .quad 0 //origin        //-24
 .quad 0 //trace_enter   //-16
 .quad 0 //trace_exit    //-8

 50                       push   %rax
 51                       push   %rcx
 52                       push   %rdx
 4150                     push   %r8
 4151                     push   %r9
 4152                     push   %r10
 4153                     push   %r11
 488b542438               mov    0x38(%rsp),%rdx
 488d0dc1ffffff           lea    -0x3f(%rip),%rcx
 ff15d3ffffff             callq  *-0x2d(%rip)
 415b                     pop    %r11
 415a                     pop    %r10
 4159                     pop    %r9
 4158                     pop    %r8
 5a                       pop    %rdx
 59                       pop    %rcx
 58                       pop    %rax
 488d642408               lea    0x8(%rsp),%rsp
 ff15b5ffffff             callq  *-0x4b(%rip)
 488d6424f8               lea    -0x8(%rsp),%rsp
 50                       push   %rax
 51                       push   %rcx
 52                       push   %rdx
 4150                     push   %r8
 4151                     push   %r9
 4152                     push   %r10
 4153                     push   %r11
 488d0d8effffff           lea    -0x72(%rip),%rcx
 ff15a8ffffff             callq  *-0x58(%rip)
 4889442438               mov    %rax,0x38(%rsp)
 415b                     pop    %r11
 415a                     pop    %r10
 4159                     pop    %r9
 4158                     pop    %r8
 5a                       pop    %rdx
 59                       pop    %rcx
 58                       pop    %rax
 c3                       retq
}


{
 asm
  .quad 0 //nid:QWORD;    //-40
  .quad 0 //lib:PLIBRARY; //-32
  .quad 0 //origin        //-24
  .quad 0 //trace_enter   //-16
  .quad 0 //trace_exit    //-8

  push %rax  //[1] +8
  push %rcx  //[1] +16
  push %rdx  //[1] +24
  push %r8   //[2] +32
  push %r9   //[2] +40
  push %r10  //[2] +48
  push %r11  //[2] +56

  mov   0x38(%rsp),%rdx    //[5] [param 2] load call src:+56
  lea   -0x3F(%rip),%rcx   //[7] [param 1] func info:-40 offset:-23
  callq -0x2D(%rip)        //[6] trace_enter:-16  offset:-29

  pop  %r11  //[2]
  pop  %r10  //[2]
  pop  %r9   //[2]
  pop  %r8   //[2]
  pop  %rdx  //[1]
  pop  %rcx  //[1]
  pop  %rax  //[1]

  lea    0x8(%rsp),%rsp   //[5] move stack back
  callq  -0x4B(%rip)      //[6] call origin:-24 offset:-51
  lea    -0x8(%rsp),%rsp  //[5] move stack forward

  push %rax  //[1] +8
  push %rcx  //[1] +16
  push %rdx  //[1] +24
  push %r8   //[2] +32
  push %r9   //[2] +40
  push %r10  //[2] +48
  push %r11  //[2] +56

  lea   -0x72(%rip),%rcx   //[7] [param 1] func info:-40 offset:-74
  callq -0x58(%rip)        //[6] trace_exit:-8  offset:-80
  mov   %rax,0x38(%rsp)    //[5] [result] store call src:+56

  pop  %r11  //[2]
  pop  %r10  //[2]
  pop  %r9   //[2]
  pop  %r8   //[2]
  pop  %rdx  //[1]
  pop  %rcx  //[1]
  pop  %rax  //[1]

  ret
 end;
}

function TStubMemoryTrace.NewTraceStub(nid:QWORD;lib,proc,trace_enter,trace_exit:Pointer):Pointer;
var
 buf:T_trace_cb_stub;
begin
 if (trace_enter=nil) or (trace_exit=nil) then Exit(proc);

 buf:=_trace_cb_stub;

 buf.info.nid        :=nid;
 buf.info.lib        :=lib;
 buf.info.origin     :=proc;
 buf.info.trace_enter:=trace_enter;
 buf.info.trace_exit :=trace_exit;

 Result:=NewStub(@buf,SizeOf(T_trace_cb_stub));
 Result:=Result+SizeOf(TTraceInfo);
end;

threadvar
 trace_local:record
  enable:Boolean;
  stack:array of Pointer;
 end;

procedure _set_trace_local_print(enable:Boolean);
begin
 trace_local.enable:=enable;
end;

procedure _trace_enter(info:PTraceInfo;src:Pointer); MS_ABI_Default;
var
 i:Integer;
begin
 //if trace_local.enable then
   Writeln(GetCurrentThreadId,':>',info^.lib^.strName,':',ps4libdoc.GetFunctName(info^.nid));

 i:=Length(trace_local.stack);
 SetLength(trace_local.stack,i+1);
 trace_local.stack[i]:=src;
end;

function _trace_exit(info:PTraceInfo):Pointer; MS_ABI_Default;
var
 i:Integer;
begin
 //if trace_local.enable then
   Writeln(GetCurrentThreadId,':<',info^.lib^.strName,':',ps4libdoc.GetFunctName(info^.nid));

 i:=Length(trace_local.stack);
 Assert(i<>0);
 i:=i-1;
 Result:=trace_local.stack[i];
 SetLength(trace_local.stack,i);
end;

end.

