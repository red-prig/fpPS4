unit kern_ipmimgr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_ipmimgr_call(op,kid:Integer;res:PInteger;params:Pointer;paramsSize:QWORD):Integer;

implementation

uses
 errno,
 systm,
 trap,
 kern_thr,
 kern_named_id;

type
 p_IpmiClientCreateData=^t_IpmiClientCreateData;
 t_IpmiClientCreateData=packed record
  size   :QWORD;
  f_0x8  :DWORD;
  _align1:Integer;
  f_0x10 :QWORD;
  f_0x18 :QWORD;
  f_0x20 :DWORD;
  _align2:Integer;
  f_0x28 :QWORD;
  f_0x30 :QWORD;
  acount :DWORD;
  _align3:Integer;
  f_0x100:array[0..31] of QWORD;
  f_0x140:DWORD;
  f_0x144:DWORD;
  f_0x148:QWORD;
 end;

 p_syscallCreateClien=^t_syscallCreateClient;
 t_syscallCreateClient=packed record
  this:Pointer;
  name:pchar;
  data:p_IpmiClientCreateData;
 end;

function syscallCreateClient(src:p_syscallCreateClien;dst:PInteger):Integer;
var
 name:t_id_name;
 data:t_IpmiClientCreateData;
 kid:Integer;
begin
 name:=Default(t_id_name);
 Result:=copyinstr(src^.name,@name,25,nil);
 if (Result<>0) then Exit;

 if (name[24]<>#0) then Exit(ENAMETOOLONG);
 if (src^.data=nil) then Exit(EINVAL);

 data:=Default(t_IpmiClientCreateData);
 Result:=copyin(src^.data,@data,SizeOf(data));
 if (Result<>0) then Exit;

 if (data.size > $150) then Exit(EINVAL);
 if (data.f_0x8 > $10) then Exit(EINVAL);

 Writeln('syscallCreateClient(',HexStr(src^.this),',"',name,'")');

 kid:=222;
 dst^:=kid;
end;

type
 p_syscallConnectWithWaitServer=^t_syscallConnectWithWaitServer;
 t_syscallConnectWithWaitServer=packed record
  param_0:QWORD;
  param_1:QWORD;
  pRes   :PInteger; //out
  param_3:PInteger; //in
 end;

function syscallConnectWithWaitServer(kid:Integer;src:p_syscallConnectWithWaitServer;dst:PInteger):Integer;
var
 kval:Integer;
begin
 if (src^.pRes=nil) then Exit(EINVAL);

 if (src^.param_0=0) then
 begin
  if (src^.param_1<>0) then
  begin
   Exit(EINVAL);
  end;
 end else
 if ((src^.param_1 - 1) > $1fff) then
 begin
  Exit(EINVAL);
 end;

 if (src^.param_3<>nil) then
 begin
  Result:=copyin(src^.param_3,@kval,SizeOf(Integer));
  if (Result<>0) then Exit;
 end;

 Writeln('syscallConnectWithWaitServer(',kid,',',
                                         HexStr(src^.param_0,16),',',
                                         HexStr(src^.param_1,16),',',
                                         HexStr(src^.pRes),',',
                                         HexStr(src^.param_3),',',
                                         kval,')');

 kval:=0;
 Result:=copyout(@kval,src^.pRes,SizeOf(Integer));

 dst^:=0;
 Result:=0;
end;

type
 p_IpmiBufferInfo=^t_IpmiBufferInfo;
 t_IpmiBufferInfo=packed record
  data:Pointer;
  size:QWORD;
  prm3:QWORD;
 end;

 p_IpmiDataInfo=^t_IpmiDataInfo;
 t_IpmiDataInfo=packed record
  data:Pointer;
  size:QWORD;
 end;

 p_IpmiSyncCallParams=^t_IpmiSyncCallParams;
 t_IpmiSyncCallParams=packed record
  method     :DWORD;
  dataCount  :DWORD;
  bufCount   :DWORD;
  align1     :DWORD;
  pData      :p_IpmiDataInfo;
  pBuffers   :p_IpmiBufferInfo;
  pResult    :PInteger;
  resultCount:DWORD;
  align2     :DWORD;
 end;

//[NpMgrIpcCli] (method=0):get EventFlag

const
 NpEvent:PChar='NpMgrEvent';

function syscallSendReceiveInvokeSyncMethod(kid:Integer;src:p_IpmiSyncCallParams;dst:PInteger):Integer;
begin
 if (src^.dataCount >10) then Exit(EINVAL);
 if (src^.bufCount  >10) then Exit(EINVAL);
 if (src^.resultCount>1) then Exit(EINVAL);

 Writeln('syscallSendReceiveInvokeSyncMethod(',kid,',',
                                               src^.method,',',
                                               src^.dataCount,',',
                                               src^.bufCount,',',
                                               src^.resultCount,')');


 case src^.method of
  0:begin
     Result:=copyout(NpEvent,src^.pBuffers[0].data,Length(NpEvent));
    end;
  else;
 end;

 dst^:=0;
 Result:=0;
end;

type
 p_syscallPollEventFlag=^t_syscallPollEventFlag;
 t_syscallPollEventFlag=packed record
  unk_id    :Integer;
  _align1   :Integer;
  bitPattern:QWORD;
  waitMode  :DWORD;
  _align2   :Integer;
  pRes      :PQWORD;
 end;

function syscallPollEventFlag(kid:Integer;src:p_syscallPollEventFlag;dst:PInteger):Integer;
var
 kval:QWORD;
begin

 Writeln('syscallPollEventFlag(',kid,',',
                                 src^.unk_id,',',
                                 src^.bitPattern,',',
                                 src^.waitMode,')');


 kval:=0;
 Result:=copyout(@kval,src^.pRes,SizeOf(QWORD));

 dst^:=0;
 Result:=0;
end;

function sys_ipmimgr_call(op,kid:Integer;res:PInteger;params:Pointer;paramsSize:QWORD):Integer;
label
 _copyout;
type
 t_src=array[0..7] of QWORD;
var
 dst:Integer;
 src:t_src;
begin
 Result:=0;
 dst:=0;

 Writeln('sys_ipmimgr_call(',op,',',kid,',',paramsSize,')');

 if (paramsSize > 64) then
 begin
  dst:=-2146566143;
  goto _copyout;
 end;

 src:=Default(t_src);

 case op of
  2: //syscallCreateClient
    begin
     Result:=copyin(params,@src,paramsSize);
     if (Result<>0) then Exit;
     Result:=syscallCreateClient(@src,@dst);
    end;

  1024: //syscallConnectWithWaitServer
    begin
     Result:=copyin(params,@src,paramsSize);
     if (Result<>0) then Exit;
     Result:=syscallConnectWithWaitServer(kid,@src,@dst);
    end;

  800: //syscallSendReceiveInvokeSyncMethod
    begin
     Result:=copyin(params,@src,paramsSize);
     if (Result<>0) then Exit;
     Result:=syscallSendReceiveInvokeSyncMethod(kid,@src,@dst);
    end;

  1169: //syscallPollEventFlag
    begin
     Result:=copyin(params,@src,paramsSize);
     if (Result<>0) then Exit;
     Result:=syscallPollEventFlag(kid,@src,@dst);
    end;

  else
    begin
     Result:=0;
     dst:=-1;
     print_backtrace_c(stderr);
     Assert(False);
    end;
 end;



 if (Result=0) then
 begin
  _copyout:
  Result:=copyout(@dst,res,SizeOf(Integer));
 end;

end;

end.

