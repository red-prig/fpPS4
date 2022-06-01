unit ps4_libSceNet;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils;

implementation

function ps4_sceNetInit:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetPoolCreate(name:PChar;size,flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetPoolCreate:',name,':',size,':',flags);
 Result:=2;
end;

//

function ps4_sceNetCtlInit:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 SCE_NET_CTL_ERROR_INVALID_ADDR=-2143215353; //0x80412107;
 SCE_NET_CTL_ERROR_INVALID_SIZE=-2143215343; //0x80412111;

 SCE_NET_CTL_STATE_DISCONNECTED=0;
 SCE_NET_CTL_STATE_CONNECTING  =1;
 SCE_NET_CTL_STATE_IPOBTAINING =2;
 SCE_NET_CTL_STATE_IPOBTAINED  =3;

 SCE_NET_CTL_EVENT_TYPE_DISCONNECTED=1;
 SCE_NET_CTL_EVENT_TYPE_DISCONNECT_REQ_FINISHED=2;
 SCE_NET_CTL_EVENT_TYPE_IPOBTAINED=3;

function ps4_sceNetCtlGetState(state:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (state=nil) then Exit(SCE_NET_CTL_ERROR_INVALID_ADDR);
 state^:=SCE_NET_CTL_STATE_DISCONNECTED;
 Result:=0;
end;

type
 SceNetCtlCallback=Procedure(eventType:Integer;arg:Pointer); SysV_ABI_CDecl;

var
 NetCtlCb:packed record
  func:SceNetCtlCallback;
  arg:Pointer;
 end;

function ps4_sceNetCtlRegisterCallback(func:SceNetCtlCallback;arg:Pointer;cid:PInteger):Integer; SysV_ABI_CDecl;
begin
 NetCtlCb.func:=func;
 NetCtlCb.arg:=arg;
 if (cid<>nil) then cid^:=0;
 Result:=0;
end;

function ps4_sceNetCtlCheckCallback():Integer; SysV_ABI_CDecl;
begin
 if (NetCtlCb.func<>nil) then
 begin
  //NetCtlCb.func(SCE_NET_CTL_EVENT_TYPE_DISCONNECTED,NetCtlCb.arg);
 end;
 Result:=0;
end;

Const
 SCE_NET_CTL_ERROR_ETHERNET_PLUGOUT=$80412115;

function ps4_sceNetCtlGetResult(eventType:Integer;errorCode:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (errorCode=nil) then Exit(SCE_NET_CTL_ERROR_INVALID_ADDR);
 errorCode^:=Integer(SCE_NET_CTL_ERROR_ETHERNET_PLUGOUT);
 Result:=0;
end;

type
 SceNetInAddr_t=DWORD;
 SceNetInAddr=packed record
  s_addr:SceNetInAddr_t;
 end;

 pSceNetCtlNatInfo=^SceNetCtlNatInfo;
 SceNetCtlNatInfo=packed record
  size:Integer;
  stunStatus:Integer;
  natType:Integer;
  mappedAddr:SceNetInAddr;
 end;

const
 SCE_NET_CTL_NATINFO_STUN_UNCHECKED=0;
 SCE_NET_CTL_NATINFO_STUN_FAILED   =1;
 SCE_NET_CTL_NATINFO_STUN_OK       =2;

 SCE_NET_CTL_NATINFO_NAT_TYPE_1    =1;
 SCE_NET_CTL_NATINFO_NAT_TYPE_2    =2;
 SCE_NET_CTL_NATINFO_NAT_TYPE_3    =3;


function ps4_sceNetCtlGetNatInfo(natInfo:pSceNetCtlNatInfo):Integer; SysV_ABI_CDecl;
begin
 if (natInfo=nil) then Exit(SCE_NET_CTL_ERROR_INVALID_ADDR);
 if (natInfo^.size<>SizeOf(SceNetCtlNatInfo)) then Exit(SCE_NET_CTL_ERROR_INVALID_SIZE);

 natInfo^.stunStatus:=SCE_NET_CTL_NATINFO_STUN_FAILED;
 natInfo^.natType:=SCE_NET_CTL_NATINFO_NAT_TYPE_1;

 Result:=0;
end;


function ps4_sceNetCtlRegisterCallbackForNpToolkit(func:SceNetCtlCallback;arg:Pointer;cid:PInteger):Integer; SysV_ABI_CDecl;
begin
 NetCtlCb.func:=func;
 NetCtlCb.arg:=arg;
 if (cid<>nil) then cid^:=0;
 Result:=0;
end;

function ps4_sceNetCtlCheckCallbackForNpToolkit():Integer; SysV_ABI_CDecl;
begin
 if (NetCtlCb.func<>nil) then
 begin
  //NetCtlCb.func(SCE_NET_CTL_EVENT_TYPE_DISCONNECTED,NetCtlCb.arg);
 end;
 Result:=0;
end;

function Load_libSceNet(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNet');
 lib^.set_proc($3657AFECB83C9370,@ps4_sceNetInit);
 lib^.set_proc($76024169E2671A9A,@ps4_sceNetPoolCreate);
end;

function Load_libSceNetCtl(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNetCtl');
 lib^.set_proc($824CB4FA868D3389,@ps4_sceNetCtlInit);
 lib^.set_proc($B813E5AF495BBA22,@ps4_sceNetCtlGetState);
 lib^.set_proc($509F99ED0FB8724D,@ps4_sceNetCtlRegisterCallback);
 lib^.set_proc($890C378903E1BD44,@ps4_sceNetCtlCheckCallback);
 lib^.set_proc($D1C06076E3D147E3,@ps4_sceNetCtlGetResult);
 lib^.set_proc($24EE32B93B8CA0A2,@ps4_sceNetCtlGetNatInfo);

 lib:=Result._add_lib('libSceNetCtlForNpToolkit');

 lib^.set_proc($C08B0ACBE4DF78BB,@ps4_sceNetCtlRegisterCallbackForNpToolkit);
 lib^.set_proc($BB9A2AB6520FF85C,@ps4_sceNetCtlCheckCallbackForNpToolkit);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNet.prx'   ,@Load_libSceNet);
 ps4_app.RegistredPreLoad('libSceNetCtl.prx',@Load_libSceNetCtl);

end.

