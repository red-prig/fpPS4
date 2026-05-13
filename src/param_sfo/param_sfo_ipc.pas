unit param_sfo_ipc;

{$mode ObjFPC}{$H+}

interface

procedure init_param_sfo;
function  ParamSfoGetString(const name:RawByteString):RawByteString;
function  ParamSfoGetUInt  (const name:RawByteString):DWORD;

implementation

uses
 sysutils,
 atomic,
 sys_bootparam,
 host_ipc,
 param_sfo_gui,
 kern_rwlock;

var
 param_sfo_lock     :Pointer=nil;
 param_sfo_lazy_init:Integer=0;
 param_sfo_file     :TParamSfoFile=nil;

procedure init_param_sfo;
var
 Value:TIpcValue;
begin
 if (param_sfo_lazy_init=2) then Exit;

 Writeln('PARAM_SFO_INIT');

 if CAS(param_sfo_lazy_init,0,1) then
 begin
  rw_wlock(param_sfo_lock);

  Value:=p_host_ipc.InvokeSync('PARAM_SFO_INIT');
  param_sfo_file:=TParamSfoFile(Value.GetObject(TParamSfoFile));
  Value.Free;

  param_sfo_lazy_init:=2;
  rw_wunlock(param_sfo_lock);
 end else
 begin
  //sunc
  rw_wlock  (param_sfo_lock);
  rw_wunlock(param_sfo_lock);
 end;
end;

function ParamSfoGetString(const name:RawByteString):RawByteString;
begin
 init_param_sfo;
 rw_rlock(param_sfo_lock);
  Result:=param_sfo_file.GetString(name);
 rw_runlock(param_sfo_lock);
end;

function  ParamSfoGetUInt(const name:RawByteString):DWORD;
begin
 init_param_sfo;
 rw_rlock(param_sfo_lock);
  Result:=param_sfo_file.GetUInt(name);
 rw_runlock(param_sfo_lock);
end;


end.

