unit vselect;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 signal,
 time;

type
 pp_fd_mask=^p_fd_mask;
 p_fd_mask=^fd_mask;
 fd_mask=QWORD;

{
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here should
 * be enough for most uses.
 }
const
 FD_SETSIZE=1024;
 NFDBITS   =(sizeof(fd_mask) * 8); { bits per mask }

type
 p_fd_set=^t_fd_set;
 t_fd_set=packed record
  fds_bits:array[0..((FD_SETSIZE + (NFDBITS - 1)) div NFDBITS)-1] of fd_mask;
 end;

procedure FD_CLR(n:Integer;p:p_fd_set);
procedure FD_COPY(f,t:p_fd_set);
function  FD_ISSET(n:Integer;p:p_fd_set):Boolean;
procedure FD_SET(n:Integer;p:p_fd_set);
procedure FD_ZERO(p:p_fd_set);

implementation

function __fdset_mask(n:Integer):fd_mask; inline;
begin
 Result:=fd_mask(1) shl (n mod NFDBITS);
end;

procedure FD_CLR(n:Integer;p:p_fd_set);
begin
 p^.fds_bits[n div NFDBITS]:=p^.fds_bits[n div NFDBITS] and (not __fdset_mask(n));
end;

procedure FD_COPY(f,t:p_fd_set);
begin
 f^:=t^;
end;

function FD_ISSET(n:Integer;p:p_fd_set):Boolean;
begin
 Result:=(p^.fds_bits[n div NFDBITS] and __fdset_mask(n))<>0;
end;

procedure FD_SET(n:Integer;p:p_fd_set);
begin
 p^.fds_bits[n div NFDBITS]:=p^.fds_bits[n div NFDBITS] or __fdset_mask(n);
end;

procedure FD_ZERO(p:p_fd_set);
begin
 p^:=Default(t_fd_set);
end;


end.

