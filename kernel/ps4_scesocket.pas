unit ps4_scesocket;

{$mode ObjFPC}{$H+}

interface

uses
  ps4_libSceNet,
  ps4_program,
  Classes,
  SysUtils;

type
 pSceNetId=^SceNetId;
 SceNetId=Integer;

function ps4_socket(const name:PChar;
                    family,_type,protocol:Integer):Integer; SysV_ABI_CDecl;

function ps4_bind(s:SceNetId;
                  const addr:pSceNetSockaddr;
                  addrlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_setsockopt(s:SceNetId;
                        level,optname:Integer;
                        const optval:Pointer;
                        optlen:SceNetSocklen_t):Integer; SysV_ABI_CDecl;

function ps4_select():Integer; SysV_ABI_CDecl;

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

implementation

function ps4_socket(const name:PChar;
                    family,_type,protocol:Integer):Integer; SysV_ABI_CDecl;
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

function ps4_select():Integer; SysV_ABI_CDecl;
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
 Result:=0;
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

end.

