unit vfs_aio2;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_aio_init(param:Pointer;size:DWORD;unknow1:QWORD;unknow2:DWORD):Integer;

implementation

uses
 errno,
 systm;

type
 SceKernelAioSchedulingParam=packed record
  schedulingWindowSize:DWORD;
  delayedCountLimit   :DWORD;
  enableSplit         :DWORD;
  splitSize           :DWORD;
  splitChunkSize      :DWORD;
 end;

 SceKernelAioParam=packed record
  low :SceKernelAioSchedulingParam;
  mid :SceKernelAioSchedulingParam;
  high:SceKernelAioSchedulingParam;
 end;
 {$IF sizeof(SceKernelAioParam)<>60}{$STOP sizeof(SceKernelAioParam)<>60}{$ENDIF}

function check_aio_shed_param(var param:SceKernelAioSchedulingParam):Integer; inline;
begin
 Result:=0;

 if (DWORD(param.schedulingWindowSize) > $80) then Exit(EINVAL);
 if (DWORD(param.delayedCountLimit   ) > $80) then Exit(EINVAL);

 if (param.enableSplit<>0) then
 begin
  if (param.enableSplit<>1)                        then Exit(EINVAL);
  if (DWORD(param.splitSize      - 1) >= $1000000) then Exit(EINVAL);
  if (DWORD(param.splitChunkSize - 1) >= $1000000) then Exit(EINVAL);
 end;
end;

function sys_aio_init(param:Pointer;size:DWORD;unknow1:QWORD;unknow2:DWORD):Integer;
var
 tmp:SceKernelAioParam;
begin
 if (unknow1<>0) or (unknow2<>0) then
 begin
  Exit(EINVAL);
 end;

 if (param<>nil) then
 begin
  //
  if (size<>SizeOf(SceKernelAioParam)) then
  begin
   Exit(EINVAL);
  end;
  //
  Result:=copyin(param,@tmp,SizeOf(SceKernelAioParam));
  if (Result<>0) then Exit;
  //
  Result:=check_aio_shed_param(tmp.low);
  if (Result<>0) then Exit;
  //
  Result:=check_aio_shed_param(tmp.mid);
  if (Result<>0) then Exit;
  //
  Result:=check_aio_shed_param(tmp.high);
  if (Result<>0) then Exit;

  //........
 end;

 //........

 Exit(0);
end;



end.

