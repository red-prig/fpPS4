unit kern_mtx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils;

type
 p_mtx=^mtx;
 mtx=packed record
  n:PChar;
  h:THandle;
  OwningThread:TThreadID;
  //c:TRTLCriticalSection;
  {$IFDEF DEBUG_MTX}
  debug_own:array[0..2] of Pointer;
  {$ENDIF}
 end;

const
 //Flags for lockinit().
 LK_INIT_MASK =$0000FF;
 LK_CANRECURSE=$000001;
 LK_NODUP     =$000002;
 LK_NOPROFILE =$000004;
 LK_NOSHARE   =$000008;
 LK_NOWITNESS =$000010;
 LK_QUIET     =$000020;
 LK_ADAPTIVE  =$000040;

 //Additional attributes to be used in lockmgr().
 LK_EATTR_MASK=$00FF00;
 LK_INTERLOCK =$000100;
 LK_NOWAIT    =$000200;
 LK_RETRY     =$000400;
 LK_SLEEPFAIL =$000800;
 LK_TIMELOCK  =$001000;

 //Operations for lockmgr().
 LK_TYPE_MASK =$FF0000;
 LK_DOWNGRADE =$010000;
 LK_DRAIN     =$020000;
 LK_EXCLOTHER =$040000;
 LK_EXCLUSIVE =$080000;
 LK_RELEASE   =$100000;
 LK_SHARED    =$200000;
 LK_UPGRADE   =$400000;
 LK_TRYUPGRADE=$800000;

 LK_TOTAL_MASK=(LK_INIT_MASK or LK_EATTR_MASK or LK_TYPE_MASK);

procedure mtx_init   (var m:mtx;name:PChar);
procedure mtx_destroy(var m:mtx);
procedure mtx_lock   (var m:mtx);
function  mtx_trylock(var m:mtx):Boolean;
procedure mtx_unlock (var m:mtx);
function  mtx_owned  (var m:mtx):Boolean;
procedure mtx_assert (var m:mtx);

implementation

uses
 ntapi;

{$IFDEF DEBUG_MTX}
uses
 md_systm,
 kern_thr;
{$ENDIF}

procedure mtx_init(var m:mtx;name:PChar); //inline;
var
 R:DWORD;
begin
 m.n:=name;
 m.h:=0;

 R:=NtCreateMutant(@m.h,MUTANT_ALL_ACCESS,nil,False);
 Assert(R=0,'NtCreateMutant');

 //InitCriticalSection(m.c);
 //EnterCriticalSection(m.c);
 //LeaveCriticalSection(m.c);
end;

procedure mtx_destroy(var m:mtx); //inline;
begin
 NtClose(m.h);
 m.n:=nil;
 m.h:=0;
 //DoneCriticalSection(m.c);
end;

procedure mtx_lock(var m:mtx); {$IFNDEF DEBUG_MTX} inline; {$ENDIF}
{$IFDEF DEBUG_MTX}
var
 rbp:Pointer;
{$ENDIF}
var
 R:DWORD;
begin
 //Writeln('lock:',m.n,':',HexStr(@m));
 {$IFDEF DEBUG_MTX}
 if curkthread<>nil then
  curkthread^.td_debug_mtx:=@m;
 {$ENDIF}

 R:=NtWaitForSingleObject(m.h,False,nil);
 Assert(R=0,'mtx_lock');

 m.OwningThread:=ThreadID;

 //EnterCriticalSection(m.c);
 {$IFDEF DEBUG_MTX}
 if curkthread<>nil then
  curkthread^.td_debug_mtx:=nil;
 rbp:=nil;
 asm
  movq %rbp,rbp
 end;
 m.debug_own[0]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[1]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[2]:=md_fuword(PPointer(rbp)[1]);
 {$ENDIF}
end;

function mtx_trylock(var m:mtx):Boolean; {$IFNDEF DEBUG_MTX} inline; {$ENDIF}
{$IFDEF DEBUG_MTX}
var
 rbp:Pointer;
{$ENDIF}
var
 R:DWORD;
 t:QWORD;
begin
 t:=0;
 R:=NtWaitForSingleObject(m.h,False,@t);
 if (R=STATUS_TIMEOUT) then Exit(False);
 Assert(R=0,'mtx_trylock');

 m.OwningThread:=ThreadID;

 Result:=True;

 //Result:=TryEnterCriticalSection(m.c)<>0;
 {$IFDEF DEBUG_MTX}
 if Result then
 begin
  rbp:=nil;
  asm
   movq %rbp,rbp
  end;
 m.debug_own[0]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[1]:=md_fuword(PPointer(rbp)[1]); rbp:=md_fuword(PPointer(rbp)[0]);
 m.debug_own[2]:=md_fuword(PPointer(rbp)[1]);
 end;
 {$ENDIF}
end;

procedure RtlWakeAddressSingle(addr:Pointer); stdcall; external 'ntdll';
procedure RtlWakeAddressAll   (addr:Pointer); stdcall; external 'ntdll';

procedure mtx_unlock(var m:mtx); //{$IFNDEF DEBUG_MTX} inline; {$ENDIF}
var
 R:DWORD;
 INFO:MUTANT_BASIC_INFORMATION;
begin
 //Writeln('ulck:',m.n,HexStr(@m));
 mtx_assert(m);
 {$IFDEF DEBUG_MTX}
 m.debug_own[0]:=nil;
 m.debug_own[1]:=nil;
 m.debug_own[2]:=nil;
 {$ENDIF}

 INFO:=Default(MUTANT_BASIC_INFORMATION);
 R:=NtQueryMutant(m.h,0,@INFO,SizeOf(INFO),nil);
 Assert(R=0,'NtQueryMutant');

 if (INFO.CurrentCount=0) then
 begin
  m.OwningThread:=0;
 end;

 R:=NtReleaseMutant(m.h,nil);
 Assert(R=0,'NtReleaseMutant');
 //LeaveCriticalSection(m.c);
 //RtlWakeAddressAll(@m.c.LockCount);
end;

function mtx_owned(var m:mtx):Boolean;// inline;
begin
 Result:=m{.c}.OwningThread=ThreadID;
end;

procedure mtx_assert(var m:mtx); //inline;
begin
 if not mtx_owned(m) then
 begin
  Assert(false,'mtx_assert:'+IntToStr(m{.c}.OwningThread)+'<>'+IntToStr(ThreadID));
 end;
end;

end.

