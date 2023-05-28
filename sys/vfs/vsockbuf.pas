unit vsockbuf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vselinfo,
 kern_mtx;

const
 SB_MAX=(2*1024*1024); { default for max chars in sockbuf }

{
 * Constants for sb_flags field of struct sockbuf.
 }
 SB_WAIT      =$04 ; { someone is waiting for data/space }
 SB_SEL       =$08 ; { someone is selecting }
 SB_ASYNC     =$10 ; { ASYNC I/O, need signals }
 SB_UPCALL    =$20 ; { someone wants an upcall }
 SB_NOINTR    =$40 ; { operations not interruptible }
 SB_AIO       =$80 ; { AIO operations queued }
 SB_KNOTE     =$100; { kernel note attached }
 SB_NOCOALESCE=$200; { don't coalesce new data into existing mbufs }
 SB_IN_TOE    =$400; { socket buffer is in the middle of an operation }
 SB_AUTOSIZE  =$800; { automatically size socket buffer }

 SBS_CANTSENDMORE=$0010; { can't send more data to peer }
 SBS_CANTRCVMORE =$0020; { can't receive more data from peer }
 SBS_RCVATMARK   =$0040; { at mark on input }

//struct mbuf;
//struct sockaddr;
//struct socket;
//struct thread;

type
 t_xsockbuf=packed record
  sb_cc   :DWORD;
  sb_hiwat:DWORD;
  sb_mbcnt:DWORD;
  sb_mcnt :DWORD;
  sb_ccnt :DWORD;
  sb_mbmax:DWORD;
  sb_lowat:Integer;
  sb_timeo:Integer;
  sb_flags:Word;
 end;

{
 * Variables for socket buffering.
 }
 t_sb_upcall=function(s,p:Pointer;i:Integer):Integer;

 p_sockbuf=^t_sockbuf;
 t_sockbuf=record
  sb_sel       :p_selinfo  ; { process selecting read/write }
  sb_mtx       :mtx        ; { sockbuf lock }
  sb_sx        :Pointer    ; { prevent I/O interlacing }
  sb_state     :Word       ; { (c/d) socket state on sockbuf }
  //sb_startzero=sb_mb
  sb_mb        :Pointer    ; { (c/d) the mbuf chain }
  sb_mbtail    :Pointer    ; { (c/d) the last mbuf in the chain }
  sb_lastrecord:Pointer    ; { (c/d) first mbuf of last record in socket buffer }
  sb_sndptr    :Pointer    ; { (c/d) pointer into mbuf chain }
  sb_sndptroff :DWORD      ; { (c/d) byte offset of ptr into chain }
  sb_cc        :DWORD      ; { (c/d) actual chars in buffer }
  sb_hiwat     :DWORD      ; { (c/d) max actual char count }
  sb_mbcnt     :DWORD      ; { (c/d) chars of mbufs used }
  sb_mcnt      :DWORD      ; { (c/d) number of mbufs in buffer }
  sb_ccnt      :DWORD      ; { (c/d) number of clusters in buffer }
  sb_mbmax     :DWORD      ; { (c/d) max chars of mbufs to use }
  sb_ctl       :DWORD      ; { (c/d) non-data chars in buffer }
  sb_lowat     :Integer    ; { (c/d) low water mark }
  sb_timeo     :Integer    ; { (c/d) timeout for read/write }
  sb_flags     :Word       ; { (c/d) flags, see below }
  sb_upcall    :t_sb_upcall; { (c/d) }
  sb_upcallarg :Pointer    ; { (c/d) }
 end;

function  SOCKBUF_MTX(_sb:p_sockbuf):p_mtx; inline;
procedure SOCKBUF_LOCK_INIT(_sb:p_sockbuf;_name:PChar); inline;
procedure SOCKBUF_LOCK_DESTROY(_sb:p_sockbuf); inline;
procedure SOCKBUF_LOCK(_sb:p_sockbuf); inline;
function  SOCKBUF_OWNED(_sb:p_sockbuf):Boolean; inline;
procedure SOCKBUF_UNLOCK(_sb:p_sockbuf); inline;
procedure SOCKBUF_LOCK_ASSERT(_sb:p_sockbuf); inline;

function  sbspace(sb:p_sockbuf):DWORD; inline;

implementation

{
 * Per-socket buffer mutex used to protect most fields in the socket
 * buffer.
 }
function SOCKBUF_MTX(_sb:p_sockbuf):p_mtx; inline;
begin
 Result:=@_sb^.sb_mtx
end;

procedure SOCKBUF_LOCK_INIT(_sb:p_sockbuf;_name:PChar); inline;
begin
 mtx_init(SOCKBUF_MTX(_sb)^,_name);
end;

procedure SOCKBUF_LOCK_DESTROY(_sb:p_sockbuf); inline;
begin
 mtx_destroy(SOCKBUF_MTX(_sb)^);
end;

procedure SOCKBUF_LOCK(_sb:p_sockbuf); inline;
begin
 mtx_lock(SOCKBUF_MTX(_sb)^);
end;

function SOCKBUF_OWNED(_sb:p_sockbuf):Boolean; inline;
begin
 Result:=mtx_owned(SOCKBUF_MTX(_sb)^);
end;

procedure SOCKBUF_UNLOCK(_sb:p_sockbuf); inline;
begin
 mtx_unlock(SOCKBUF_MTX(_sb)^);
end;

procedure SOCKBUF_LOCK_ASSERT(_sb:p_sockbuf); inline;
begin
 mtx_assert(SOCKBUF_MTX(_sb)^);
end;

//

function sbspace(sb:p_sockbuf):DWORD; inline;
var
 s1,s2:DWORD;
begin
 s1:=sb^.sb_hiwat-sb^.sb_cc;
 s2:=sb^.sb_mbmax-sb^.sb_mbcnt;
 if (s1<s2) then
  Result:=s1
 else
  Result:=s2;
end;

end.

