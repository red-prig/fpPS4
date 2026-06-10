unit SaveDataBackend;

{$mode objfpc}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 vmparam,
 md_systm,
 md_systm_fork,
 md_map,
 md_sleep,
 SceSaveData;

type
 TSaveDataBackend=class
  hMem    :THandle;
  pMem    :Pointer;
  hProcess:THandle;
  fork_pid:Integer;
  Procedure Init;
 end;

implementation

type
 PForkData=^TForkData;
 TForkData=record
  hMem:THandle;
 end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl; forward;

Procedure TSaveDataBackend.Init;
var
 fork_info:t_fork_proc;
 data:TForkData;
 r:DWORD;
begin

 hMem:=0;
 R:=md_memfd_create(hMem,MD_PAGE_SIZE,VM_RW);

 if (r<>0) then
 begin
  Writeln('failed md_memfd_create(',HexStr(MD_PAGE_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 pMem:=Pointer(KERNEL_LOWER);
 R:=md_mmap(pMem,MD_PAGE_SIZE,VM_RW,hMem,0);

 if (r<>0) then
 begin
  Writeln('failed md_mmap(',HexStr(MD_PAGE_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 data.hMem:=hMem;

 fork_info.hInput :=GetStdHandle(STD_INPUT_HANDLE);
 fork_info.hOutput:=GetStdHandle(STD_OUTPUT_HANDLE);
 fork_info.hError :=GetStdHandle(STD_ERROR_HANDLE);

 fork_info.proc:=@savedata_process;
 fork_info.data:=@data;
 fork_info.size:=sizeof(data);

 r:=md_fork_process(fork_info,0);

 if (r<>0) then
 begin
  Writeln('failed md_fork_process:0x',HexStr(r,8));
  Assert(false,'TSaveDataBackend');
 end;

 hProcess:=fork_info.hProcess;
 fork_pid:=fork_info.fork_pid;
end;

procedure savedata_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 r:Integer;
 ppid:Integer;

 parent:THandle;

 hMem:THandle;
 pMem:Pointer;
begin
 //while not IsDebuggerPresent do sleep(100);

 hMem:=PForkData(data)^.hMem;

 //free shared
 FreeMem(data);

 ppid:=md_getppid;

 Writeln('savedata_process started pid:',md_getpid,' parent_pid:',ppid);

 parent:=md_pidfd_open(ppid);

 //dup
 hMem:=md_pidfd_getfd(parent,hMem);

 pMem:=Pointer(KERNEL_LOWER);
 R:=md_mmap(pMem,MD_PAGE_SIZE,VM_RW,hMem,0);

 if (r<>0) then
 begin
  Writeln('failed md_mmap(',HexStr(MD_PAGE_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'savedata_process');
 end;

 r:=md_waitpidfd(parent,nil);

 if (r<>0) then
 begin
  Writeln('failed md_waitpidfd(',HexStr(PAGE_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'savedata_process');
 end;

 Writeln('savedata_process stopped pid:',md_getpid,' parent_pid:',ppid);

 //msleep_td(0);
end;


end.

