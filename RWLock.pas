{ Implementation of Slim read-write lock based on https://github.com/neosmart/RWLock

  Copyright (C) 2018-2020 Red_prig

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

unit RWLock;

{$mode objfpc}{$H+}

interface

{$IFDEF UNIX}
 {$DEFINE SMART} //Force NeoSmart algoritm
{$ENDIF}
 {/$DEFINE SMART}

{$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
 uses
  UnixType,pthreads;
 type
  TRWLock=pthread_rwlock_t;
{$ELSE}
 {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
  uses
   Windows;
 {$ENDIF}
 type
  TRWLock=packed record
   Case Byte of
    0:(SRWLock:Pointer;Mode:SizeInt);
    1:(Event:PRTLEvent;vLock:DWORD);
  end;
{$ENDIF}

Procedure rwlock_init(Var L:TRWLock);
Procedure rwlock_destroy(Var L:TRWLock);
Procedure rwlock_rdlock(Var L:TRWLock);
Procedure rwlock_wrlock(Var L:TRWLock);
function  rwlock_tryrdlock(Var L:TRWLock):Boolean;
function  rwlock_trywrlock(Var L:TRWLock):Boolean;
Procedure rwlock_unlock(Var L:TRWLock);

implementation

{$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
type
 TSRWLockProc=procedure(Var SRWLock:Pointer); stdcall;
 TSRWLockTryFunc=Function(Var SRWLock:Pointer):Boolean; stdcall;

Var
 InitializeSRWLock,
 AcquireSRWLockShared,
 ReleaseSRWLockShared,
 AcquireSRWLockExclusive,
 ReleaseSRWLockExclusive:TSRWLockProc;
 TryAcquireSRWLockShared,
 TryAcquireSRWLockExclusive:TSRWLockTryFunc;

Procedure LoadSRW;
Var
 Handle:THandle;
begin
 InitializeSRWLock         :=nil;
 AcquireSRWLockShared      :=nil;
 ReleaseSRWLockShared      :=nil;
 AcquireSRWLockExclusive   :=nil;
 ReleaseSRWLockExclusive   :=nil;
 TryAcquireSRWLockShared   :=nil;
 TryAcquireSRWLockExclusive:=nil;

 Handle:=GetModuleHandle('kernel32.dll');
 if Handle<>INVALID_HANDLE_VALUE then
 begin
  Pointer(InitializeSRWLock         ):=GetProcAddress(Handle,'InitializeSRWLock');
  if InitializeSRWLock<>nil then
  begin
   Pointer(AcquireSRWLockShared      ):=GetProcAddress(Handle,'AcquireSRWLockShared');
   Pointer(ReleaseSRWLockShared      ):=GetProcAddress(Handle,'ReleaseSRWLockShared');
   Pointer(AcquireSRWLockExclusive   ):=GetProcAddress(Handle,'AcquireSRWLockExclusive');
   Pointer(ReleaseSRWLockExclusive   ):=GetProcAddress(Handle,'ReleaseSRWLockExclusive');
   Pointer(TryAcquireSRWLockShared   ):=GetProcAddress(Handle,'TryAcquireSRWLockShared');
   Pointer(TryAcquireSRWLockExclusive):=GetProcAddress(Handle,'TryAcquireSRWLockExclusive');
  end;
 end;
end;
{$ENDIF}

{$IF (not DEFINED(UNIX)) or DEFINED(SMART)}
Const
 MAX_SPIN=50000;

function ReaderCount(lock:DWORD):WORD; inline;
begin
 Result:=WORD(lock and $00007FFF);
end;

function SetReaders(lock:DWORD;readers:WORD):DWORD; inline;
begin
 Result:=(lock and (not $00007FFF)) or readers;
end;

function WaitingCount(lock:DWORD):WORD; inline;
begin
 Result:=WORD((lock and $3FFF8000) shr 15);
end;

function SetWaiting(lock:DWORD;waiting:WORD):DWORD; inline;
begin
 Result:=(lock and (not $3FFF8000)) or (waiting shl 15);
end;

function Writer(lock:DWORD):Boolean; inline;
begin
 Result:=(lock and $40000000)<>0;
end;

function SetWriter(lock:DWORD;writer:Boolean):DWORD; inline;
begin
 if writer then
  Result:=lock or $40000000
 else
  Result:=lock and (not $40000000);
end;

function AllClear(lock:DWORD):Boolean; inline;
begin
 Result:=(lock and $40007FFF)=0;
end;

function Initialized(lock:DWORD):Boolean; inline;
begin
 Result:=(lock and $80000000)<>0;
end;

function SetInitialized(lock:DWORD;init:Boolean):DWORD; inline;
begin
 if init then
  Result:=lock or $80000000
 else
  Result:=lock and (not $80000000);
end;

Procedure _rdlock(Var vLock:DWORD;Event:PRTLEvent); inline;
Var
 i:SizeUInt;
 temp:DWORD;
begin
 i:=0;
 repeat
  temp:=vLock;
  if not Writer(temp) then
  begin
   if System.InterlockedCompareExchange(vLock,SetReaders(temp,ReaderCount(temp)+1),temp)=temp then
    Break
   else
    Continue;
  end else
  begin
   if (i<MAX_SPIN) then
   begin
    ThreadSwitch;
    Continue;
   end;
   if System.InterlockedCompareExchange(vLock,SetWaiting(temp,WaitingCount(temp)+1),temp)<>temp then
    Continue;
   RTLeventWaitFor(Event);
   i:=0;
   repeat
    temp:=vLock;
    if (i>MAX_SPIN) then
    begin
     ThreadSwitch;
     Continue;
    end;
    Inc(i);
   until System.InterlockedCompareExchange(vLock,SetWaiting(temp,WaitingCount(temp)-1),temp)=temp;
   i:=0;
  end;
  Inc(i);
 until False;
end;

Procedure _wrlock(Var vLock:DWORD;Event:PRTLEvent); inline;
Var
 i:SizeUInt;
 temp:DWORD;
begin
 i:=0;
 repeat
  temp:=vLock;
  if AllClear(temp) then
  begin
   if System.InterlockedCompareExchange(vLock,SetWriter(temp,true),temp)=temp then
    Break
   else
    Continue;
  end else
  begin
   if (i<MAX_SPIN) then
   begin
    ThreadSwitch;
    Continue;
   end;
   if System.InterlockedCompareExchange(vLock,SetWaiting(temp,WaitingCount(temp)+1),temp)<>temp then
    Continue;
   RTLeventWaitFor(Event);
   i:=0;
   repeat
    temp:=vLock;
    if (i>MAX_SPIN) then
    begin
     ThreadSwitch;
     Continue;
    end;
    Inc(i);
   until System.InterlockedCompareExchange(vLock,SetWaiting(temp,WaitingCount(temp)-1),temp)=temp;
   i:=0;
  end;
  Inc(i);
 until False;
end;

Procedure _unlock(Var vLock:DWORD;Event:PRTLEvent); inline;
Var
 temp:DWORD;
begin
 if ReaderCount(vLock)=0 then
 begin
  repeat
   repeat
    temp:=vLock;
    if (WaitingCount(temp)=0) then break;
    RTLeventSetEvent(Event);
   until False;
  until System.InterlockedCompareExchange(vLock,SetWriter(temp,false),temp)=temp;
 end else
 begin
  repeat
   temp:=vLock;
   if (ReaderCount(temp)=1) and (WaitingCount(temp)<>0) then
    RTLeventSetEvent(Event);
  until System.InterlockedCompareExchange(vLock,SetReaders(temp,ReaderCount(temp)-1),temp)=temp;
 end;
end;

function _tryrdlock(Var vLock:DWORD):Boolean; inline;
Var
 temp:DWORD;
begin
 Result:=False;
 temp:=vLock;
 if not Writer(temp) then
 begin
  if System.InterlockedCompareExchange(vLock,SetReaders(temp,ReaderCount(temp)+1),temp)=temp then
   Result:=True;
 end;
end;

function _trywrlock(Var vLock:DWORD):Boolean; inline;
Var
 temp:DWORD;
begin
 Result:=False;
 temp:=vLock;
 if AllClear(temp) then
 begin
  if System.InterlockedCompareExchange(vLock,SetWriter(temp,true),temp)=temp then
   Result:=True;
 end;
end;

{$ENDIF}

Procedure rwlock_init(Var L:TRWLock);
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  pthread_rwlock_init(@L,nil);
 {$ELSE}
  L:=Default(TRWLock);
  {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
   if InitializeSRWLock<>nil then
   begin
    InitializeSRWLock(L.SRWLock);
    L.Mode:=0;
   end else
  {$ENDIF}
   begin
    L.vLock:=SetInitialized(0,true);
    L.Event:=RTLEventCreate;
   end;
 {$ENDIF}
end;

Procedure rwlock_destroy(Var L:TRWLock);
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  pthread_rwlock_destroy(@L);
 {$ELSE}
  {$IF DEFINED(WINDOWS) and (not defined(SMART))}
   if InitializeSRWLock=nil then
  {$ENDIF}
    RTLEventDestroy(L.Event);
 {$ENDIF}
end;

Procedure rwlock_rdlock(Var L:TRWLock);
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  pthread_rwlock_rdlock(@L);
 {$ELSE}
  {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
   if InitializeSRWLock<>nil then
   begin
    AcquireSRWLockShared(L.SRWLock);
    L.Mode:=1;
   end else
  {$ENDIF}
   _rdlock(L.vLock,L.Event);
 {$ENDIF}
end;

Procedure rwlock_wrlock(Var L:TRWLock);
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  pthread_rwlock_wrlock(@L);
 {$ELSE}
  {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
   if InitializeSRWLock<>nil then
   begin
    AcquireSRWLockExclusive(L.SRWLock);
    L.Mode:=2;
   end else
  {$ENDIF}
   _wrlock(L.vLock,L.Event);
 {$ENDIF}
end;

function  rwlock_tryrdlock(Var L:TRWLock):Boolean;
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  Result:=pthread_rwlock_tryrdlock(@L)=0;
 {$ELSE}
  {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
   if InitializeSRWLock<>nil then
   begin
    Result:=TryAcquireSRWLockShared(L.SRWLock);
    if Result then
     L.Mode:=1;
   end else
  {$ENDIF}
   Result:=_tryrdlock(L.vLock);
 {$ENDIF}
end;

function  rwlock_trywrlock(Var L:TRWLock):Boolean;
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  Result:=pthread_rwlock_trywrlock(@L)=0;
 {$ELSE}
  {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
   if InitializeSRWLock<>nil then
   begin
    Result:=TryAcquireSRWLockExclusive(L.SRWLock);
    if Result then
     L.Mode:=2;
   end else
  {$ENDIF}
   Result:=_trywrlock(L.vLock);
 {$ENDIF}
end;

Procedure rwlock_unlock(Var L:TRWLock);
begin
 {$IF DEFINED(UNIX)) and (not DEFINED(SMART))}
  pthread_rwlock_unlock(@L);
 {$ELSE}
  {$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
   if InitializeSRWLock<>nil then
   begin
    if L.SRWLock<>nil then
    Case L.Mode of
     1:ReleaseSRWLockShared(L.SRWLock);
     2:ReleaseSRWLockExclusive(L.SRWLock);
    end;
   end else
  {$ENDIF}
   _unlock(L.vLock,L.Event);
 {$ENDIF}
end;

{$IF DEFINED(WINDOWS) and (not DEFINED(SMART))}
initialization
begin
 LoadSRW;
end;
{$ENDIF}

end.

