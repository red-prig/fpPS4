unit stub_manager;

{$mode objfpc}{$H+}

interface

uses
 Windows;

type
 TStubMemory=object
  protected
   type
    PNode=^TNode;
    TNode=record
     pNext:PNode;
     Stub_va:Pointer;
    end;
   Const
    VA_SIZE=16*1024;
   var
    pHead:PNode;
    Stub_va:Pointer;
    Stub_pos:Word;
   procedure Push(P:Pointer);
   function  Pop:Pointer;
  public
   Procedure Clear;
   procedure FinStub;
   function  NewStub(data:Pointer;size:Word):Pointer;
 end;

 TStubMemoryProc=object(TStubMemory)
  function NewNopStub(nid:QWORD;lib,proc:Pointer):Pointer;
 end;

implementation

procedure TStubMemory.Push(P:Pointer);
var
 Node:PNode;
begin
 Node:=AllocMem(SizeOf(TNode));
 Node^.Stub_va:=P;
 if (pHead=nil) then
 begin
  node^.pNext:=nil;
 end else
 begin
  node^.pNext:=pHead;
 end;
 pHead:=node;
end;

function TStubMemory.Pop:Pointer;
var
 Node:PNode;
begin
 Result:=nil;
 Node:=pHead;
 if (pHead<>nil) then
 begin
  pHead:=pHead^.pNext;
 end;
 if (Node<>nil) then
 begin
  Node^.pNext:=nil;
 end;
 if (Node<>nil) then
 begin
  Result:=Node^.Stub_va;
  FreeMem(Node);
 end;
end;

Procedure TStubMemory.Clear;
var
 P:Pointer;
begin
 P:=Pop;
 While (P<>nil) do
 begin
  VirtualFree(P,0,MEM_RELEASE);
  P:=Pop;
 end;
 if (Stub_va<>nil) then
 begin
  VirtualFree(Stub_va,0,MEM_RELEASE);
 end;
 Stub_va:=nil;
 Stub_pos:=0;
end;

procedure TStubMemory.FinStub;
var
 dummy:DWORD;
begin
 if (Stub_va<>nil) then
 begin
  Push(Stub_va);
  VirtualProtect(Stub_va,VA_SIZE,PAGE_EXECUTE_READ,@dummy);
  FlushInstructionCache(GetCurrentProcess,Stub_va,VA_SIZE);
  Stub_va:=nil;
  Stub_pos:=0;
 end;
end;

function TStubMemory.NewStub(data:Pointer;size:Word):Pointer;
begin
 if ((Stub_pos+size)>VA_SIZE) then
   FinStub;

 if (Stub_va=nil) then
 begin
  Stub_va:=VirtualAlloc(nil,VA_SIZE,MEM_COMMIT or MEM_RESERVE,PAGE_READWRITE);
 end;

 Result:=Stub_va+Stub_pos;
 Move(data^,Result^,size);
 Stub_pos:=Stub_pos+size;
end;

//


//52                       push   %rdx
//51                       push   %rcx
//48ba0100000000000000     movabs $0x1,%rdx
//48b90200000000000000     movabs $0x2,%rcx
//48b80300000000000000     movabs $0x3,%rax
//ffd0                     callq  *%rax
//4831c0                   xor    %rax,%rax
//59                       pop    %rcx
//5a                       pop    %rdx
//c3                       retq


Type
 Pnopstub=^Tnopstub;
 Tnopstub=packed record
  _push_rdx:Byte; // $52
  _push_rcx:Byte; // $51
  _movabs_rdx:array[0..1] of Byte; // $48 $ba
  _lib:Pointer;
  _movabs_rcx:array[0..1] of Byte; // $48 $B9
  _nid:QWord;
  _movabs_rax:array[0..1] of Byte; // $48 $B8
  _addr:Pointer;
  _call_rax:array[0..1] of Byte;   // $FF $D0
  _pop_rcx:Byte; // $59
  _pop_rdx:Byte; // $5A
  _ret:Byte;     // $C3
 end;

const
 _nopstub:Tnopstub=(
  _push_rdx:$52;
  _push_rcx:$51;
  _movabs_rdx:($48,$BA);
  _lib:nil;
  _movabs_rcx:($48,$B9);
  _nid:0;
  _movabs_rax:($48,$B8);
  _addr:nil;
  _call_rax:($FF,$D0);
  _pop_rcx:$59;
  _pop_rdx:$5A;
  _ret:$C3
 );

function TStubMemoryProc.NewNopStub(nid:QWORD;lib,proc:Pointer):Pointer;
var
 nopstub:Tnopstub;
begin
 nopstub:=_nopstub;
 nopstub._lib:=lib;
 nopstub._nid:=nid;
 nopstub._addr:=proc;
 Result:=NewStub(@nopstub,SizeOf(Tnopstub));
end;


end.

