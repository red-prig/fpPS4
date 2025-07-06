unit ps4_libnet;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

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

implementation

end.


