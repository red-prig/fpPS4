unit ps4_libSceNet;

{$mode objfpc}{$H+}

interface

uses
  sockets,
  ps4_program,
  Classes,
  SysUtils;

implementation

threadvar
 sce_net_errno:Integer;

type
 SceNetInAddr_t=DWORD;

 pSceNetInAddr=^SceNetInAddr;
 SceNetInAddr=packed record
  s_addr:SceNetInAddr_t;
 end;

const
 SCE_NET_INET_ADDRSTRLEN=16;

 SCE_NET_ETHER_ADDR_LEN  =6;
 SCE_NET_ETHER_ADDRSTRLEN=18;

type
 pSceNetEtherAddr=^SceNetEtherAddr;
 SceNetEtherAddr=packed record
  data:array[0..SCE_NET_ETHER_ADDR_LEN-1] of Byte;
 end;

type
 pSceNetSocklen_t=^SceNetSocklen_t;
 SceNetSocklen_t=DWORD;

type
 SceNetSaFamily=Byte;

type
 SceNetEpollData= packed record
  Case Byte of //union
  0:(ptr:Pointer);
  1:(u32:DWORD);
  2:(fd:Integer);
  3:(u64:QWORD);
end;

type
 pSceNetSockaddr=^SceNetSockaddr;
 SceNetSockaddr = packed record
  sa_len:Byte;
  sa_family:SceNetSaFamily;
  sa_data:array[0..13] of Char;
 end;

type
 pSceNetEpollEvent=^SceNetEpollEvent;
 SceNetEpollEvent = packed record
  events:DWORD;
  reserved:DWORD;
  ident:QWORD;
  data:SceNetEpollData;
 end;

function libnet_tls_get_errno():PInteger;
begin
 Result:=@sce_net_errno;
end;

function _set_net_errno(r:Integer):Integer;
var
 perr:PInteger;
begin
 Result:=0;
 perr:=libnet_tls_get_errno;
 perr^:=r;
 if (r<>0) then
 begin
  Result:=r or Integer($80410100);
 end;
end;

function ps4_sceNetErrnoLoc:Pointer; SysV_ABI_CDecl;
begin
 Result:=libnet_tls_get_errno;
end;

function ps4_sceNetInit:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetTerm:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetPoolCreate(name:PChar;size,flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetPoolCreate:',name,':',size,':',flags);
 Result:=2; // iNetLibId
end;

function ps4_sceNetPoolDestroy(memid:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetPoolDestroy:',memid);
 Result:=0;
end;

function ps4_sceNetResolverCreate(name:PChar;memid,flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetResolverCreate:',name,':',memid);
 Result:=111;
end;

function ps4_sceNetEpollCreate(name:PChar;flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetEpollCreate:',name,':',flags);
 Result:=3;
end;

const
 AF_INET = 2;
 AF_INET6=28;

 SCE_NET_EINVAL      =22;
 SCE_NET_ENOSPC      =28;
 SCE_NET_EAFNOSUPPORT=47;

function ps4_sceNetInetPton(af:Integer;
                            src:Pchar;
                            dst:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;

 if (src=nil) or (dst=nil) then
 begin
  Exit(_set_net_errno(SCE_NET_EINVAL));
 end;

 Case af of
  AF_INET:
   begin
    if TryStrToHostAddr(src,in_addr(dst^)) then
    begin
     DWORD(dst^):=htonl(DWORD(dst^));
     Result:=1;
    end;
   end;
  AF_INET6:if TryStrToHostAddr6(src,in6_addr(dst^)) then Result:=1;
  else
   begin
    Exit(_set_net_errno(SCE_NET_EAFNOSUPPORT));
   end;
 end;

end;

function ps4_sceNetInetNtop(af:Integer;
                            src:Pointer;
                            dst:Pchar;
                            size:DWORD):Pchar; SysV_ABI_CDecl;
var
 S:RawByteString;
begin
 Result:=nil;

 if (src=nil) or (dst=nil) then
 begin
  _set_net_errno(SCE_NET_EINVAL);
  Exit;
 end;

 Case af of
  AF_INET :S:=NetAddrToStr (in_addr (src^));
  AF_INET6:S:=NetAddrToStr6(in6_addr(src^));
  else
   begin
    _set_net_errno(SCE_NET_EAFNOSUPPORT);
    Exit;
   end;
 end;

 if (size<=Length(S)) then
 begin
  _set_net_errno(SCE_NET_ENOSPC);
  Exit;
 end;

 Move(PChar(S)^,dst^,Length(S)+1);
 Result:=dst;
end;

function ps4_sceNetEtherNtostr(n:pSceNetEtherAddr;
                               str:Pchar;
                               len:QWORD):Integer; SysV_ABI_CDecl;
var
 i,p:Byte;
begin
 Result:=0;

 if (n=nil) or (str=nil) then
 begin
  Exit(_set_net_errno(SCE_NET_EINVAL));
 end;

 if (len<SCE_NET_ETHER_ADDRSTRLEN) then
 begin
  Exit(_set_net_errno(SCE_NET_ENOSPC));
 end;

 p:=0;
 For i:=0 to SCE_NET_ETHER_ADDR_LEN-1 do
 begin
  str[p]:=HexStr(n^.data[i]      ,1)[1]; Inc(p);
  str[p]:=HexStr(n^.data[i] shl 8,1)[1]; Inc(p);
  if (i<>SCE_NET_ETHER_ADDR_LEN-1) then
  begin
   str[p]:=':'; Inc(p);
  end;
 end;

end;

function ps4_sceNetHtons(host16:Word):Word; SysV_ABI_CDecl;
begin
 Result:=htons(host16);
end;

function ps4_sceNetHtonl(host32:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=htonl(host32);
end;

function ps4_sceNetGetMacAddress(addr:pSceNetEtherAddr;
                                 flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;

 if (addr=nil) then
 begin
  Exit(_set_net_errno(SCE_NET_EINVAL));
 end;

 FillChar(addr^,SizeOf(SceNetEtherAddr),11);
end;

function ps4_sceNetSocket(const name: PChar; family:Integer; socket_type:Integer; protocol:Integer):Integer; SysV_ABI_CDecl; //will return socketID which will be used as "s","rid","eid",etc by other functions
begin
 Result:=0;
end;

function ps4_sceNetSetsockopt(s:Integer; level:Integer; opname:Integer; const optval:Pointer; optlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetBind(s:Integer; const addr:pSceNetSockaddr; addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetListen(s:Integer; backlog:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetAccept(s:Integer; addr:pSceNetSockaddr; paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetRecv(s:Integer; buf:Pointer; len:size_t; flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetRecv:',flags);
 Result:=0;
end;

function ps4_sceNetSend(s:Integer; const buf:Pointer; len:size_t; flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetSend',flags);
 Result:=0;
end;

function ps4_sceNetShutdown(s:Integer; how:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetShutdown:',how);
 Result:=0;
end;

function ps4_sceNetSocketClose(s:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetGetsockname(s:Integer; addr:pSceNetSockaddr; paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetNtohl(net32:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=NToHl(net32);
end;

function ps4_sceNetNtohs(net16:Word):Integer; SysV_ABI_CDecl;
begin
 Result:=NToHs(net16);
end;

function ps4_sceNetEpollControl(eid:Integer; op:Integer; id:Integer; event:pSceNetEpollEvent):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

//function ps4_sceNetGetsockopt(s:Integer; level:Integer; optname:Integer; optval:Pointer; optlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
//begin
// Result:=0;
//end;

function ps4_sceNetResolverStartAton(rid:Integer; const addr:pSceNetInAddr; hostname:PChar; hostname_len:Integer; timeout:Integer; retry:Integer; flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceNetResolveStartAton:',hostname,':',flags);
 Result:=0;
end;

function ps4_sceNetResolverDestroy(rid:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetConnect(s:Integer; const addr:pSceNetSockaddr; addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetEpollWait(s:Integer; events:pSceNetEpollEvent; maxevents:Integer; timeout:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetEpollDestroy(eid:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetCtlInit:Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

procedure ps4_sceNetCtlTerm; SysV_ABI_CDecl;
begin
 //
end;

const
 SCE_NET_CTL_ERROR_NOT_INITIALIZED =-2143215359; // 0x80412101
 SCE_NET_CTL_ERROR_CALLBACK_MAX    =-2143215357; // 0x80412103
 SCE_NET_CTL_ERROR_ID_NOT_FOUND    =-2143215356; // 0x80412104
 SCE_NET_CTL_ERROR_INVALID_ID      =-2143215355; // 0x80412105
 SCE_NET_CTL_ERROR_INVALID_CODE    =-2143215354; // 0x80412106
 SCE_NET_CTL_ERROR_INVALID_ADDR    =-2143215353; // 0x80412107
 SCE_NET_CTL_ERROR_NOT_CONNECTED   =-2143215352; // 0x80412108
 SCE_NET_CTL_ERROR_NOT_AVAIL       =-2143215351; // 0x80412109
 SCE_NET_CTL_ERROR_NETWORK_DISABLED=-2143215347; // 0x8041210D
 SCE_NET_CTL_ERROR_DISCONNECT_REQ  =-2143215346; // 0x8041210E
 SCE_NET_CTL_ERROR_INVALID_TYPE    =-2143215345; // 0x8041210F
 SCE_NET_CTL_ERROR_INVALID_SIZE    =-2143215343; // 0x80412111
 SCE_NET_CTL_ERROR_ETHERNET_PLUGOUT=-2143215339; // 0x80412115
 SCE_NET_CTL_ERROR_WIFI_DEAUTHED   =-2143215338; // 0x80412116
 SCE_NET_CTL_ERROR_WIFI_BEACON_LOST=-2143215337; // 0x80412117

 // state
 SCE_NET_CTL_STATE_DISCONNECTED=0;
 SCE_NET_CTL_STATE_CONNECTING  =1;
 SCE_NET_CTL_STATE_IPOBTAINING =2;
 SCE_NET_CTL_STATE_IPOBTAINED  =3;

 // event type
 SCE_NET_CTL_EVENT_TYPE_DISCONNECTED=1;
 SCE_NET_CTL_EVENT_TYPE_DISCONNECT_REQ_FINISHED=2;
 SCE_NET_CTL_EVENT_TYPE_IPOBTAINED=3;

 // info code
 SCE_NET_CTL_INFO_DEVICE           = 1;
 SCE_NET_CTL_INFO_ETHER_ADDR       = 2;
 SCE_NET_CTL_INFO_MTU              = 3;
 SCE_NET_CTL_INFO_LINK             = 4;
 SCE_NET_CTL_INFO_BSSID            = 5;
 SCE_NET_CTL_INFO_SSID             = 6;
 SCE_NET_CTL_INFO_WIFI_SECURITY    = 7;
 SCE_NET_CTL_INFO_RSSI_DBM         = 8;
 SCE_NET_CTL_INFO_RSSI_PERCENTAGE  = 9;
 SCE_NET_CTL_INFO_CHANNEL          =10;
 SCE_NET_CTL_INFO_IP_CONFIG        =11;
 SCE_NET_CTL_INFO_DHCP_HOSTNAME    =12;
 SCE_NET_CTL_INFO_PPPOE_AUTH_NAME  =13;
 SCE_NET_CTL_INFO_IP_ADDRESS       =14;
 SCE_NET_CTL_INFO_NETMASK          =15;
 SCE_NET_CTL_INFO_DEFAULT_ROUTE    =16;
 SCE_NET_CTL_INFO_PRIMARY_DNS      =17;
 SCE_NET_CTL_INFO_SECONDARY_DNS    =18;
 SCE_NET_CTL_INFO_HTTP_PROXY_CONFIG=19;
 SCE_NET_CTL_INFO_HTTP_PROXY_SERVER=20;
 SCE_NET_CTL_INFO_HTTP_PROXY_PORT  =21;
 SCE_NET_CTL_INFO_RESERVED1        =22;
 SCE_NET_CTL_INFO_RESERVED2        =23;

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

function ps4_sceNetCtlGetResult(eventType:Integer;errorCode:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (errorCode=nil) then Exit(SCE_NET_CTL_ERROR_INVALID_ADDR);
 errorCode^:=SCE_NET_CTL_ERROR_ETHERNET_PLUGOUT;
 Result:=0;
end;

type
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

const
 SCE_NET_CTL_SSID_LEN         =(32 + 1);
 SCE_NET_CTL_HOSTNAME_LEN     =(255 + 1);
 SCE_NET_CTL_AUTH_NAME_LEN    =(127 + 1);
 SCE_NET_CTL_IPV4_ADDR_STR_LEN=(16);

type
 pSceNetCtlInfo=^SceNetCtlInfo;
 SceNetCtlInfo=packed record
  Case Byte of
    0:(device:DWORD);
    1:(ether_addr:SceNetEtherAddr);
    2:(mtu:DWORD);
    3:(link:DWORD);
    4:(bssid:SceNetEtherAddr);
    5:(ssid:array[0..SCE_NET_CTL_SSID_LEN-1] of Char);
    6:(wifi_security:DWORD);
    7:(rssi_dbm:Byte);
    8:(rssi_percentage:Byte);
    9:(channel:Byte);
   10:(ip_config:DWORD);
   11:(dhcp_hostname:array[0..SCE_NET_CTL_HOSTNAME_LEN-1] of Char);
   12:(pppoe_auth_name:array[0..SCE_NET_CTL_AUTH_NAME_LEN-1] of Char);
   13:(ip_address:array[0..SCE_NET_CTL_IPV4_ADDR_STR_LEN-1] of Char);
   14:(netmask:array[0..SCE_NET_CTL_IPV4_ADDR_STR_LEN-1] of Char);
   15:(default_route:array[0..SCE_NET_CTL_IPV4_ADDR_STR_LEN-1] of Char);
   16:(primary_dns:array[0..SCE_NET_CTL_IPV4_ADDR_STR_LEN-1] of Char);
   17:(secondary_dns:array[0..SCE_NET_CTL_IPV4_ADDR_STR_LEN-1] of Char);
   18:(http_proxy_config:DWORD);
   19:(http_proxy_server:array[0..SCE_NET_CTL_HOSTNAME_LEN-1] of Char);
   20:(http_proxy_port:WORD);
 end;


function ps4_sceNetCtlGetInfo(code:Integer;info:pSceNetCtlInfo):Integer; SysV_ABI_CDecl;
begin
 if (info=nil) then Exit(SCE_NET_CTL_ERROR_INVALID_ADDR);

 Result:=SCE_NET_CTL_ERROR_NOT_CONNECTED;

 case code of
  SCE_NET_CTL_INFO_DEVICE           :FillChar(info^,SizeOf(SceNetCtlInfo.device           ),0);
  SCE_NET_CTL_INFO_ETHER_ADDR       :FillChar(info^,SizeOf(SceNetCtlInfo.ether_addr       ),0);
  SCE_NET_CTL_INFO_MTU              :FillChar(info^,SizeOf(SceNetCtlInfo.mtu              ),0);
  SCE_NET_CTL_INFO_LINK             :FillChar(info^,SizeOf(SceNetCtlInfo.link             ),0);
  SCE_NET_CTL_INFO_BSSID            :FillChar(info^,SizeOf(SceNetCtlInfo.bssid            ),0);
  SCE_NET_CTL_INFO_SSID             :FillChar(info^,SizeOf(SceNetCtlInfo.ssid             ),0);
  SCE_NET_CTL_INFO_WIFI_SECURITY    :FillChar(info^,SizeOf(SceNetCtlInfo.wifi_security    ),0);
  SCE_NET_CTL_INFO_RSSI_DBM         :FillChar(info^,SizeOf(SceNetCtlInfo.rssi_dbm         ),0);
  SCE_NET_CTL_INFO_RSSI_PERCENTAGE  :FillChar(info^,SizeOf(SceNetCtlInfo.rssi_percentage  ),0);
  SCE_NET_CTL_INFO_CHANNEL          :FillChar(info^,SizeOf(SceNetCtlInfo.channel          ),0);
  SCE_NET_CTL_INFO_IP_CONFIG        :FillChar(info^,SizeOf(SceNetCtlInfo.ip_config        ),0);
  SCE_NET_CTL_INFO_DHCP_HOSTNAME    :FillChar(info^,SizeOf(SceNetCtlInfo.dhcp_hostname    ),0);
  SCE_NET_CTL_INFO_PPPOE_AUTH_NAME  :FillChar(info^,SizeOf(SceNetCtlInfo.pppoe_auth_name  ),0);
  SCE_NET_CTL_INFO_IP_ADDRESS       :FillChar(info^,SizeOf(SceNetCtlInfo.ip_address       ),0);
  SCE_NET_CTL_INFO_NETMASK          :FillChar(info^,SizeOf(SceNetCtlInfo.netmask          ),0);
  SCE_NET_CTL_INFO_DEFAULT_ROUTE    :FillChar(info^,SizeOf(SceNetCtlInfo.default_route    ),0);
  SCE_NET_CTL_INFO_PRIMARY_DNS      :FillChar(info^,SizeOf(SceNetCtlInfo.primary_dns      ),0);
  SCE_NET_CTL_INFO_SECONDARY_DNS    :FillChar(info^,SizeOf(SceNetCtlInfo.secondary_dns    ),0);
  SCE_NET_CTL_INFO_HTTP_PROXY_CONFIG:FillChar(info^,SizeOf(SceNetCtlInfo.http_proxy_config),0);
  SCE_NET_CTL_INFO_HTTP_PROXY_SERVER:FillChar(info^,SizeOf(SceNetCtlInfo.http_proxy_server),0);
  SCE_NET_CTL_INFO_HTTP_PROXY_PORT  :FillChar(info^,SizeOf(SceNetCtlInfo.http_proxy_port  ),0);
  else
   Exit(SCE_NET_CTL_ERROR_INVALID_CODE);
 end;

 case code of
  SCE_NET_CTL_INFO_DEVICE           :Writeln('SCE_NET_CTL_INFO_DEVICE           ');
  SCE_NET_CTL_INFO_ETHER_ADDR       :Writeln('SCE_NET_CTL_INFO_ETHER_ADDR       ');
  SCE_NET_CTL_INFO_MTU              :Writeln('SCE_NET_CTL_INFO_MTU              ');
  SCE_NET_CTL_INFO_LINK             :Writeln('SCE_NET_CTL_INFO_LINK             ');
  SCE_NET_CTL_INFO_BSSID            :Writeln('SCE_NET_CTL_INFO_BSSID            ');
  SCE_NET_CTL_INFO_SSID             :Writeln('SCE_NET_CTL_INFO_SSID             ');
  SCE_NET_CTL_INFO_WIFI_SECURITY    :Writeln('SCE_NET_CTL_INFO_WIFI_SECURITY    ');
  SCE_NET_CTL_INFO_RSSI_DBM         :Writeln('SCE_NET_CTL_INFO_RSSI_DBM         ');
  SCE_NET_CTL_INFO_RSSI_PERCENTAGE  :Writeln('SCE_NET_CTL_INFO_RSSI_PERCENTAGE  ');
  SCE_NET_CTL_INFO_CHANNEL          :Writeln('SCE_NET_CTL_INFO_CHANNEL          ');
  SCE_NET_CTL_INFO_IP_CONFIG        :Writeln('SCE_NET_CTL_INFO_IP_CONFIG        ');
  SCE_NET_CTL_INFO_DHCP_HOSTNAME    :Writeln('SCE_NET_CTL_INFO_DHCP_HOSTNAME    ');
  SCE_NET_CTL_INFO_PPPOE_AUTH_NAME  :Writeln('SCE_NET_CTL_INFO_PPPOE_AUTH_NAME  ');
  SCE_NET_CTL_INFO_IP_ADDRESS       :Writeln('SCE_NET_CTL_INFO_IP_ADDRESS       ');
  SCE_NET_CTL_INFO_NETMASK          :Writeln('SCE_NET_CTL_INFO_NETMASK          ');
  SCE_NET_CTL_INFO_DEFAULT_ROUTE    :Writeln('SCE_NET_CTL_INFO_DEFAULT_ROUTE    ');
  SCE_NET_CTL_INFO_PRIMARY_DNS      :Writeln('SCE_NET_CTL_INFO_PRIMARY_DNS      ');
  SCE_NET_CTL_INFO_SECONDARY_DNS    :Writeln('SCE_NET_CTL_INFO_SECONDARY_DNS    ');
  SCE_NET_CTL_INFO_HTTP_PROXY_CONFIG:Writeln('SCE_NET_CTL_INFO_HTTP_PROXY_CONFIG');
  SCE_NET_CTL_INFO_HTTP_PROXY_SERVER:Writeln('SCE_NET_CTL_INFO_HTTP_PROXY_SERVER');
  SCE_NET_CTL_INFO_HTTP_PROXY_PORT  :Writeln('SCE_NET_CTL_INFO_HTTP_PROXY_PORT  ');
  else;
 end;

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
 lib^.set_proc($1D03B09DF3068A94,@ps4_sceNetErrnoLoc);
 lib^.set_proc($3657AFECB83C9370,@ps4_sceNetInit);
 lib^.set_proc($7131A473AFD30652,@ps4_sceNetTerm);
 lib^.set_proc($76024169E2671A9A,@ps4_sceNetPoolCreate);
 lib^.set_proc($2BB465AD3908FE6C,@ps4_sceNetPoolDestroy);
 lib^.set_proc($0B85200C71CFBDDC,@ps4_sceNetResolverCreate);
 lib^.set_proc($485E3B901D8C353A,@ps4_sceNetEpollCreate);
 lib^.set_proc($F0A729E5DFEAD54A,@ps4_sceNetInetPton);
 lib^.set_proc($F6F036696F821EE0,@ps4_sceNetInetNtop);
 lib^.set_proc($BFA338B7179C0AEA,@ps4_sceNetEtherNtostr);
 lib^.set_proc($896416AF0892B7C0,@ps4_sceNetHtons);
 lib^.set_proc($F53DA90C5D91CAA8,@ps4_sceNetHtonl);
 lib^.set_proc($E8E7346CBB0861ED,@ps4_sceNetGetMacAddress);
 lib^.set_proc($438A81B8DFDCD193,@ps4_sceNetSocket);
 lib^.set_proc($DA6297D92A6CA3B2,@ps4_sceNetSetsockopt);
 lib^.set_proc($6C4AF1E3D3E0C726,@ps4_sceNetBind);
 lib^.set_proc($90E8F51E2006139E,@ps4_sceNetListen);
 lib^.set_proc($3C85AA867F684B17,@ps4_sceNetAccept);
 lib^.set_proc($F703BD5EB32C3617,@ps4_sceNetRecv);
 lib^.set_proc($6DE4635C19FFCFEA,@ps4_sceNetSend);
 lib^.set_proc($4D233AC21B5E9289,@ps4_sceNetShutdown);
 lib^.set_proc($E398201336A43C94,@ps4_sceNetSocketClose);
 lib^.set_proc($868380A1F86146F1,@ps4_sceNetGetsockname);
 lib^.set_proc($A501A91D8A290086,@ps4_sceNetNtohl);
 lib^.set_proc($45BBEDFB9636884C,@ps4_sceNetNtohs);
 lib^.set_proc($655C38E9BB1AB009,@ps4_sceNetEpollControl);
 //lib^.set_proc($C6986B66EB25EFC1,@ps4_sceNetGetsockopt);
 lib^.set_proc($0296F8603C4AB112,@ps4_sceNetResolverStartAton);
 lib^.set_proc($9099581F9B8C0162,@ps4_sceNetResolverDestroy);
 lib^.set_proc($3975D7E26524DEE9,@ps4_sceNetConnect);
 lib^.set_proc($76B8C86C36C0ED44,@ps4_sceNetEpollWait);
 lib^.set_proc($227A7595F2FE25DC,@ps4_sceNetEpollDestroy);
end;

function Load_libSceNetCtl(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceNetCtl');
 lib^.set_proc($824CB4FA868D3389,@ps4_sceNetCtlInit);
 lib^.set_proc($678C3008588110B4,@ps4_sceNetCtlTerm);
 lib^.set_proc($B813E5AF495BBA22,@ps4_sceNetCtlGetState);
 lib^.set_proc($509F99ED0FB8724D,@ps4_sceNetCtlRegisterCallback);
 lib^.set_proc($890C378903E1BD44,@ps4_sceNetCtlCheckCallback);
 lib^.set_proc($D1C06076E3D147E3,@ps4_sceNetCtlGetResult);
 lib^.set_proc($24EE32B93B8CA0A2,@ps4_sceNetCtlGetNatInfo);
 lib^.set_proc($A1BBB17538B0905F,@ps4_sceNetCtlGetInfo);

 lib:=Result._add_lib('libSceNetCtlForNpToolkit');

 lib^.set_proc($C08B0ACBE4DF78BB,@ps4_sceNetCtlRegisterCallbackForNpToolkit);
 lib^.set_proc($BB9A2AB6520FF85C,@ps4_sceNetCtlCheckCallbackForNpToolkit);
end;

initialization
 ps4_app.RegistredPreLoad('libSceNet.prx'   ,@Load_libSceNet);
 ps4_app.RegistredPreLoad('libSceNetCtl.prx',@Load_libSceNetCtl);

end.

