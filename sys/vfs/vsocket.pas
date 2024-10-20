unit vsocket;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vuio;

{
 * Definitions related to sockets: types, address families, options.
 }

{
 * Data types.
 }
type
 sa_family_t=Byte;

 p_socklen_t=^socklen_t;
 socklen_t  =Integer;

const
//socket types
 SOCK_STREAM   =1; { stream socket }
 SOCK_DGRAM    =2; { datagram socket }
 SOCK_RAW      =3; { raw-protocol interface }

//socket options
 SO_DEBUG       =$0001; { turn on debugging info recording }
 SO_ACCEPTCONN  =$0002; { socket has had listen() }
 SO_REUSEADDR   =$0004; { allow local address reuse }
 SO_KEEPALIVE   =$0008; { keep connections alive }
 SO_DONTROUTE   =$0010; { just use interface addresses }
 SO_BROADCAST   =$0020; { permit sending of broadcast msgs }
 SO_USELOOPBACK =$0040; { bypass hardware when possible }
 SO_LINGER      =$0080; { linger on close if data present }
 SO_OOBINLINE   =$0100; { leave received OOB data in line }
 SO_REUSEPORT   =$0200; { allow local address & port reuse }
 SO_TIMESTAMP   =$0400; { timestamp received dgram traffic }
 SO_NOSIGPIPE   =$0800; { no SIGPIPE from EPIPE }
 SO_ACCEPTFILTER=$1000; { there is an accept filter }
 SO_BINTIME     =$2000; { timestamp received dgram traffic }
 SO_NO_OFFLOAD  =$4000; { socket cannot be offloaded }
 SO_NO_DDP      =$8000; { disable direct data placement }

//Additional options, not kept in so_options.
 SO_SNDBUF       =$1001; { send buffer size }
 SO_RCVBUF       =$1002; { receive buffer size }
 SO_SNDLOWAT     =$1003; { send low-water mark }
 SO_RCVLOWAT     =$1004; { receive low-water mark }
 SO_SNDTIMEO     =$1005; { send timeout }
 SO_RCVTIMEO     =$1006; { receive timeout }
 SO_ERROR        =$1007; { get error status and clear }
 SO_TYPE         =$1008; { get socket type }
 SO_LABEL        =$1009; { socket's MAC label }
 SO_PEERLABEL    =$1010; { socket's peer's MAC label }
 SO_LISTENQLIMIT =$1011; { socket's backlog limit }
 SO_LISTENQLEN   =$1012; { socket's complete queue length }
 SO_LISTENINCQLEN=$1013; { socket's incomplete queue length }
 SO_SETFIB       =$1014; { use this FIB to route }
 SO_USER_COOKIE  =$1015; { user cookie (dummynet etc.) }
 SO_PROTOCOL     =$1016; { get socket protocol (Linux name) }
 SO_PROTOTYPE    =SO_PROTOCOL; { alias for SO_PROTOCOL (SunOS name) }

{
 * Space reserved for new socket options added by third-party vendors.
 * This range applies to all socket option levels.  New socket options
 * in FreeBSD should always use an option value less than SO_VENDOR.
 }
 SO_VENDOR=$80000000;

type
{
 * Structure used for manipulating linger option.
 }
 p_linger=^t_linger;
 t_linger=packed record
  l_onoff :Integer; { option on/off }
  l_linger:Integer; { linger time }
 end;

 t_accept_filter_arg=packed record
  af_name:array[0..15] of AnsiChar;
  af_arg :array[0..256-15] of Byte;
 end;

const
{
 * Level number for (get/set)sockopt() to apply to socket itself.
 }
 SOL_SOCKET=$ffff;  { options for socket level }

{ address families }
 AF_UNSPEC=0;
 AF_UNIX  =1;
 AF_LOCAL =AF_UNIX;
 AF_INET  =2;
 AF_INET6 =28;
 AF_MAX   =38;

 PF_UNSPEC=AF_UNSPEC;
 PF_LOCAL =AF_LOCAL;
 PF_UNIX  =PF_LOCAL;
 PF_INET  =AF_INET;
 PF_INET6 =AF_INET6;
 PF_MAX   =AF_MAX;

type
{
 * Structure used by kernel to store most
 * addresses.
 }
 pp_sockaddr=^p_sockaddr;
 p_sockaddr=^t_sockaddr;
 t_sockaddr=packed record
  sa_len   :Byte;                 { total length }
  sa_family:sa_family_t;          { address family }
  sa_data  :array[0..13] of Byte; { actually longer; address value }
 end;

const
 SOCK_MAXADDRLEN=255; { longest possible addresses }

{
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 }
type
 p_sockproto=^t_sockproto;
 t_sockproto=packed record
  sp_family  :Word; { address family }
  sp_protocol:Word; { protocol }
 end;

const
{
 * PF_ROUTE - Routing table
 *
 * Three additional levels are defined:
 * Fourth: address family, 0 is wildcard
 * Fifth: type of info, defined below
 * Sixth: flag(s) to mask with for NET_RT_FLAGS
 }
 NET_RT_DUMP    =1; { dump; may limit to a.f. }
 NET_RT_FLAGS   =2; { by flags, e.g. RESOLVING }
 NET_RT_IFLIST  =3; { survey interface list }
 NET_RT_IFMALIST=4; { return multicast address list }
 NET_RT_IFLISTL =5; { Survey interface list, using len versions of msghdr structs. }
 NET_RT_MAXID   =6;

{
 * Maximum queue length specifiable by listen.
 }
 SOMAXCONN=128;

type
{
 * Message header for recvmsg and sendmsg calls.
 * Used value-result for recvmsg, value only for sendmsg.
 }
 p_msghdr=^t_msghdr;
 t_msghdr=packed record
  msg_name      :Pointer;   { optional address }
  msg_namelen   :socklen_t; { size of address }
  _align1       :Integer;
  msg_iov       :p_iovec;   { scatter/gather array }
  msg_iovlen    :Integer;   { # elements in msg_iov }
  _align2       :Integer;
  msg_control   :Pointer;   { ancillary data, see below }
  msg_controllen:socklen_t; { ancillary data buffer len }
  msg_flags     :Integer;   { flags on received message }
 end;

const
 MSG_OOB         =$1    ; { process out-of-band data }
 MSG_PEEK        =$2    ; { peek at incoming message }
 MSG_DONTROUTE   =$4    ; { send without using routing tables }
 MSG_EOR         =$8    ; { data completes record }
 MSG_TRUNC       =$10   ; { data discarded before delivery }
 MSG_CTRUNC      =$20   ; { control data lost before delivery }
 MSG_WAITALL     =$40   ; { wait for full request or error }
 MSG_NOTIFICATION=$2000 ; { SCTP notification }
 MSG_DONTWAIT    =$80   ; { this message should be nonblocking }
 MSG_EOF         =$100  ; { data completes connection }
 MSG_NBIO        =$4000 ; { FIONBIO mode, used by fifofs }
 MSG_COMPAT      =$8000 ; { used in sendit() }
 MSG_SOCALLBCK   =$10000; { for use by socket callbacks - soreceive (TCP) }
 MSG_NOSIGNAL    =$20000; { do not generate SIGPIPE on EOF }

type
{
 * Header for ancillary data objects in msg_control buffer.
 * Used for additional information with/about a datagram
 * not expressible by flags.  The format is a sequence
 * of message elements headed by cmsghdr structures.
 }
 p_cmsghdr=^t_cmsghdr;
 t_cmsghdr=packed record
  cmsg_len  :socklen_t; { data byte count, including hdr }
  cmsg_level:Integer;   { originating protocol }
  cmsg_type :Integer;   { protocol-specific type }
  { followed by u_char  cmsg_data[]; }
 end;

const
{
 * While we may have more groups than this, the cmsgcred struct must
 * be able to fit in an mbuf and we have historically supported a
 * maximum of 16 groups.
}
 CMGROUP_MAX=16;

{ "Socket"-level control message types: }
 SCM_RIGHTS   =$01; { access rights (array of int) }
 SCM_TIMESTAMP=$02; { timestamp (struct timeval) }
 SCM_CREDS    =$03; { process creds (struct cmsgcred) }
 SCM_BINTIME  =$04; { timestamp (struct bintime) }

{
 * howto arguments for shutdown(2), specified by Posix.1g.
 }
 SHUT_RD  =0; { shut down the reading side }
 SHUT_WR  =1; { shut down the writing side }
 SHUT_RDWR=2; { shut down both sides }

{ we cheat and use the SHUT_XX defines for these }
 PRU_FLUSH_RD  =SHUT_RD  ;
 PRU_FLUSH_WR  =SHUT_WR  ;
 PRU_FLUSH_RDWR=SHUT_RDWR;

type
{
 * sendfile(2) header/trailer struct
 }
 p_sf_hdtr=^t_sf_hdtr;
 t_sf_hdtr=packed record
  headers :p_iovec; { pointer to an array of header struct iovec's }
  hdr_cnt :Integer; { number of header iovec's }
  _align  :Integer;
  trailers:p_iovec; { pointer to an array of trailer struct iovec's }
  trl_cnt :Integer; { number of trailer iovec's }
 end;

const
{
 * Sendfile-specific flag(s)
 }
 SF_NODISKIO=$00000001;
 SF_MNOWAIT =$00000002;
 SF_SYNC    =$00000004;

function CMSG_ALIGN(p:ptrint):ptrint; inline;
function CMSG_DATA(cmsg:Pointer):Pointer; inline;
function CMSG_FIRSTHDR(mhdr:p_msghdr):p_msghdr; inline;
function CMSG_NXTHDR(mhdr:p_msghdr;cmsg:p_cmsghdr):p_msghdr; inline;
function CMSG_SPACE(l:Ptruint):Ptruint;
function CMSG_LEN(l:Ptruint):Ptruint;

implementation

const
 _ALIGNBYTES=SizeOf(ptrint)-1;

function CMSG_ALIGN(p:ptrint):ptrint; inline;
begin
 Result:=(p+_ALIGNBYTES) and (not _ALIGNBYTES);
end;

{ given pointer to struct cmsghdr, return pointer to data }
function CMSG_DATA(cmsg:Pointer):Pointer; inline;
begin
 Result:=cmsg+CMSG_ALIGN(SizeOf(t_cmsghdr));
end;

{
 * RFC 2292 requires to check msg_controllen, in case that the kernel returns
 * an empty list for some reasons.
 }
function CMSG_FIRSTHDR(mhdr:p_msghdr):p_msghdr; inline;
begin
 if (mhdr^.msg_controllen>=SizeOf(t_cmsghdr)) then
  Result:=mhdr^.msg_control
 else
  Result:=nil;
end;

{ given pointer to struct cmsghdr, return pointer to next cmsghdr }
function CMSG_NXTHDR(mhdr:p_msghdr;cmsg:p_cmsghdr):p_msghdr; inline;
begin
 if (cmsg=nil) then
  Result:=CMSG_FIRSTHDR(mhdr)
 else
 if (Ptruint(cmsg)+CMSG_ALIGN(cmsg^.cmsg_len)+CMSG_ALIGN(SizeOf(t_cmsghdr)))>
    (Ptruint(mhdr^.msg_control)+mhdr^.msg_controllen) then
 begin
  Result:=nil;
 end else
 begin
  Result:=Pointer(cmsg)+CMSG_ALIGN(cmsg^.cmsg_len);
 end;
end;

{ RFC 2292 additions }
function CMSG_SPACE(l:Ptruint):Ptruint;
begin
 Result:=CMSG_ALIGN(SizeOf(t_cmsghdr))+CMSG_ALIGN(l);
end;

function CMSG_LEN(l:Ptruint):Ptruint;
begin
 Result:=CMSG_ALIGN(SizeOf(t_cmsghdr))+l;
end;


end.

