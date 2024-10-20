unit ps4_libSceNet;

{$mode objfpc}{$H+}

interface

uses
  sockets,
  ps4_program,
  Classes,
  SysUtils;

const
 AF_INET = 2;
 AF_INET6=28;

 SCE_NET_EINVAL      =22;
 SCE_NET_ENOSPC      =28;
 SCE_NET_EWOULDBLOCK =35;
 SCE_NET_EAFNOSUPPORT=47;
 SCE_NET_ECONNREFUSED=61;
 SCE_NET_EHOSTUNREACH=65;

 //

 SCE_NET_INET_ADDRSTRLEN=16;

 SCE_NET_ETHER_ADDR_LEN  =6;
 SCE_NET_ETHER_ADDRSTRLEN=18;

type
 SceNetInAddr_t=DWORD;

 pSceNetInAddr=^SceNetInAddr;
 SceNetInAddr=packed record
  s_addr:SceNetInAddr_t;
 end;

 pSceNetEtherAddr=^SceNetEtherAddr;
 SceNetEtherAddr=packed record
  data:array[0..SCE_NET_ETHER_ADDR_LEN-1] of Byte;
 end;

 pSceNetSocklen_t=^SceNetSocklen_t;
 SceNetSocklen_t=DWORD;

 pSceNetSaFamily_t=^SceNetSaFamily_t;
 SceNetSaFamily_t=Byte;

 pSceNetInPort_t=^SceNetInPort_t;
 SceNetInPort_t=word;

 pSceNetSockaddrIn=^SceNetSockaddrIn;
 SceNetSockaddrIn=packed record
  sin_len   :Byte;
  sin_family:SceNetSaFamily_t;
  sin_port  :SceNetInPort_t;
  sin_addr  :SceNetInAddr;
  sin_vport :SceNetInPort_t;
  sin_zero  :array[0..5] of char;
 end;

 SceNetEpollData=packed record
  Case Byte of //union
  0:(ptr:Pointer);
  1:(u32:DWORD);
  2:(fd :Integer);
  3:(u64:QWORD);
 end;

 pSceNetSockaddr=^SceNetSockaddr;
 SceNetSockaddr = packed record
  sa_len   :Byte;
  sa_family:SceNetSaFamily_t;
  sa_data  :array[0..13] of Byte;
 end;

 pSceNetEpollEvent=^SceNetEpollEvent;
 SceNetEpollEvent = packed record
  events  :DWORD;
  reserved:DWORD;
  ident   :QWORD;
  data    :SceNetEpollData;
 end;

const
 default_addr:SceNetSockaddr=(
  sa_len   :SizeOf(SceNetSockaddr);
  sa_family:AF_INET;
  sa_data  :(80,0,1,1,1,1,0,0,0,0,0,0,0,0);
 );

implementation

uses
 ps4_time;

threadvar
 sce_net_errno:Integer;

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
 //Writeln('sceNetPoolCreate:',name,':',size,':',flags);
 Result:=2; // iNetLibId
end;

function ps4_sceNetPoolDestroy(memid:Integer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNetPoolDestroy:',memid);
 Result:=0;
end;

function ps4_sceNetResolverCreate(name:PChar;memid,flags:Integer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNetResolverCreate:',name,':',memid);
 Result:=111;
end;

function ps4_sceNetEpollCreate(name:PChar;flags:Integer):Integer; SysV_ABI_CDecl;
begin
//Writeln('sceNetEpollCreate:',name,':',flags);
 Result:=3;
end;

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

function ps4_sceNetSocket(name:PChar;
                          family:Integer;
                          socket_type:Integer;
                          protocol:Integer):Integer; SysV_ABI_CDecl; //will return socketID which will be used as "s","rid","eid",etc by other functions
begin
 Result:=0;
end;

function ps4_sceNetSetsockopt(s:Integer;
                              level:Integer;
                              opname:Integer;
                              optval:Pointer;
                              optlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetBind(s:Integer;
                        addr:pSceNetSockaddr;
                        addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetListen(s:Integer;backlog:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetAccept(s:Integer;
                          addr:pSceNetSockaddr;
                          paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 sleep(-1);
 Result:=0;
 if (addr<>nil) then
 begin
  addr^:=default_addr;
 end;
 if (paddrlen<>nil) then
 begin
  paddrlen^:=SizeOf(SceNetSockaddr);
 end;
 Result:=_set_net_errno(SCE_NET_EWOULDBLOCK)
end;

function ps4_sceNetRecv(s:Integer;
                        buf:Pointer;
                        len:size_t;
                        flags:Integer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNetRecv:',flags);
 Result:=0;
end;

function ps4_sceNetSend(s:Integer;
                        buf:Pointer;
                        len:size_t;
                        flags:Integer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNetSend',flags);
 Result:=0;
end;

function ps4_sceNetRecvfrom(s:Integer;
                            buf:Pointer;
                            len:size_t;
                            flags:Integer;
                            addr:pSceNetSockaddr;
                            paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNetRecvfrom:',flags);
 Result:=0;
 if (addr<>nil) then
 begin
  addr^:=default_addr;
 end;
 if (paddrlen<>nil) then
 begin
  paddrlen^:=SizeOf(SceNetSockaddr);
 end;
end;

function ps4_sceNetShutdown(s:Integer;how:Integer):Integer; SysV_ABI_CDecl;
begin
 //Writeln('sceNetShutdown:',how);
 Result:=0;
end;

function ps4_sceNetSocketClose(s:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetGetsockname(s:Integer; addr:pSceNetSockaddr; paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 if (addr<>nil) then
 begin
  addr^:=default_addr;
 end;
 if (paddrlen<>nil) then
 begin
  paddrlen^:=SizeOf(SceNetSockaddr);
 end;
end;

function ps4_sceNetNtohl(net32:DWORD):DWORD; SysV_ABI_CDecl;
begin
 Result:=NToHl(net32);
end;

function ps4_sceNetNtohs(net16:Word):Integer; SysV_ABI_CDecl;
begin
 Result:=NToHs(net16);
end;

function htonll(Value:QWORD):QWORD; inline;
begin
 Result:=htonl(Value shr 32) or (htonl(Value) shl 32)
end;

function ps4_sceNetHtonll(host64:QWORD):QWORD; SysV_ABI_CDecl;
begin
 Result:=htonl(host64);
end;

function ps4_sceNetEpollControl(eid:Integer; op:Integer; id:Integer; event:pSceNetEpollEvent):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

const
 SCE_NET_SO_SNDBUF=$1001;
 SCE_NET_SO_RCVBUF=$1002;

function ps4_sceNetGetsockopt(s:Integer; level:Integer; optname:Integer; optval:Pointer; optlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;

 if (optval<>nil) then
 case optname of
  SCE_NET_SO_SNDBUF:PInteger(optval)^:=32768;
  SCE_NET_SO_RCVBUF:PInteger(optval)^:=65536;
  else;
 end;
end;

function ps4_sceNetResolverStartAton(rid:Integer; const addr:pSceNetInAddr; hostname:PChar; hostname_len:Integer; timeout:Integer; retry:Integer; flags:Integer):Integer; SysV_ABI_CDecl;
const
 chost:PChar='123.site.com'#0;
begin
 Result:=0;

 ps4_usleep(100);

 FillChar(hostname^,hostname_len,0);
 Move(chost^,hostname^,Length(chost));
end;

function ps4_sceNetResolverStartNtoa(rid:Integer; const hostname:PChar; addr:pSceNetInAddr; timeout:Integer; retry:Integer; flags:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;

 ps4_usleep(100);

 if (addr<>nil) then
 begin
  addr^.s_addr:=SceNetInAddr_t(Pointer(@default_addr.sa_data)^);
 end;
end;

function ps4_sceNetResolverDestroy(rid:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetConnect(s:Integer; const addr:pSceNetSockaddr; addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 sleep(200);
 Result:=_set_net_errno(SCE_NET_ECONNREFUSED);
end;

function ps4_sceNetEpollWait(s:Integer; events:pSceNetEpollEvent; maxevents:Integer; timeout:Integer):Integer; SysV_ABI_CDecl;
begin
 ps4_usleep(timeout);
 Result:=0;
end;

function ps4_sceNetEpollDestroy(eid:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceNetBandwidthControlSetDefaultParam(param1:Pointer;param2:Integer):Integer; SysV_ABI_CDecl;
begin
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
 lib^.set_proc($DF4E28A0D6715836,@ps4_sceNetRecvfrom);
 lib^.set_proc($4D233AC21B5E9289,@ps4_sceNetShutdown);
 lib^.set_proc($E398201336A43C94,@ps4_sceNetSocketClose);
 lib^.set_proc($868380A1F86146F1,@ps4_sceNetGetsockname);
 lib^.set_proc($A501A91D8A290086,@ps4_sceNetNtohl);
 lib^.set_proc($45BBEDFB9636884C,@ps4_sceNetNtohs);
 lib^.set_proc($DC21E2D4AD70B024,@ps4_sceNetHtonll);
 lib^.set_proc($655C38E9BB1AB009,@ps4_sceNetEpollControl);
 lib^.set_proc($C6986B66EB25EFC1,@ps4_sceNetGetsockopt);
 lib^.set_proc($0296F8603C4AB112,@ps4_sceNetResolverStartAton);
 lib^.set_proc($35DF7559A5A61B6C,@ps4_sceNetResolverStartNtoa);
 lib^.set_proc($9099581F9B8C0162,@ps4_sceNetResolverDestroy);
 lib^.set_proc($3975D7E26524DEE9,@ps4_sceNetConnect);
 lib^.set_proc($76B8C86C36C0ED44,@ps4_sceNetEpollWait);
 lib^.set_proc($227A7595F2FE25DC,@ps4_sceNetEpollDestroy);
 lib^.set_proc($3F8CD95C4EDBA6C0,@ps4_sceNetBandwidthControlSetDefaultParam); 
end;

initialization
 ps4_app.RegistredPreLoad('libSceNet.prx'   ,@Load_libSceNet);

end.

