unit ps4_scesocket;

{$mode ObjFPC}{$H+}

interface

uses
 sys_kernel,
 ps4_libSceNet,
 ps4_program,
 Classes,
 SysUtils;

type
 pSceNetId=^SceNetId;
 SceNetId=Integer;

function ps4_socket(family,_type,protocol:Integer):Integer; SysV_ABI_CDecl;

function ps4_bind(s:SceNetId;
                  const addr:pSceNetSockaddr;
                  addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_setsockopt(s:SceNetId;
                        level,optname:Integer;
                        const optval:Pointer;
                        optlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_select(s:SceNetId;
                    readfds  :Pointer;
                    writefds :Pointer;
                    exceptfds:Pointer;
                    timeout  :Pointer
                    ):Integer; SysV_ABI_CDecl;

function ps4_recvfrom(s:SceNetId;
                      buf:Pointer;
                      len:QWORD;
                      flags:Integer;
                      addr:pSceNetSockaddr;
                      paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_listen(s:SceNetId;backlog:Integer):Integer; SysV_ABI_CDecl;

function ps4_shutdown(s:SceNetId;how:Integer):Integer; SysV_ABI_CDecl;

function ps4_accept(s:SceNetId;
                    addr:pSceNetSockaddr;
                    paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_sendto(s:SceNetId;
                    const buf:Pointer;
                    len:QWORD;
                    flags:Integer;
                    const addr:pSceNetSockaddr;
                    paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_getsockname(s:SceNetId;
                         addr:pSceNetSockaddr;
                         paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_connect(s:SceNetId;
                     const addr:pSceNetSockaddr;
                     addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;                                                                 

implementation

function ps4_socket(family,_type,protocol:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_bind(s:SceNetId;
                  const addr:pSceNetSockaddr;
                  addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_setsockopt(s:SceNetId;
                        level,optname:Integer;
                        const optval:Pointer;
                        optlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_select(s:SceNetId;
                    readfds  :Pointer;
                    writefds :Pointer;
                    exceptfds:Pointer;
                    timeout  :Pointer
                    ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_recvfrom(s:SceNetId;
                      buf:Pointer;
                      len:QWORD;
                      flags:Integer;
                      addr:pSceNetSockaddr;
                      paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_listen(s:SceNetId;backlog:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_shutdown(s:SceNetId;how:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_accept(s:SceNetId;
                    addr:pSceNetSockaddr;
                    paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 sleep(200);
 Result:=0;
 if (addr<>nil) then
 begin
  addr^:=default_addr;
 end;
 if (paddrlen<>nil) then
 begin
  paddrlen^:=SizeOf(SceNetSockaddr);
 end;
 Result:=_set_errno(EAGAIN);
end;

function ps4_sendto(s:SceNetId;
                    const buf:Pointer;
                    len:QWORD;
                    flags:Integer;
                    const addr:pSceNetSockaddr;
                    paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_getsockname(s:SceNetId;
                         addr:pSceNetSockaddr;
                         paddrlen:pSceNetSocklen_t):Integer; SysV_ABI_CDecl;
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

function ps4_connect(s:SceNetId;
                     const addr:pSceNetSockaddr;
                     addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;
begin
 sleep(200);
 Result:=_set_errno(ECONNREFUSED);
end;

end.

