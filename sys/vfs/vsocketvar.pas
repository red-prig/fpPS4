unit vsocketvar;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vfile,
 vsockstate,
 vsockbuf,
 kern_mtx;

//#include <sys/sockopt.h>

{
 * Kernel structure per socket.
 * Contains send and receive buffer queues,
 * handle on protocol and pointer to protocol
 * private data and error information.
 }
type
 so_gen_t=QWORD;

 t_accf_callback=function(so,arg:Pointer;waitflag:Integer):Integer;
 t_accf_create  =function(so,arg:Pointer):Pointer;
 t_accf_destroy =procedure(so:Pointer);

 p_accept_filter=^t_accept_filter;
 t_accept_filter=packed record
  accf_name    :array[0..15] of AnsiChar;
  accf_callback:t_accf_callback;
  accf_create  :t_accf_create  ;
  accf_destroy :t_accf_destroy ;
  accf_next    :SLIST_ENTRY    ;
 end;

{-
 * Locking key to struct socket:
 * (a) constant after allocation, no locking required.
 * (b) locked by SOCK_LOCK(so).
 * (c) locked by SOCKBUF_LOCK(@so^.so_rcv).
 * (d) locked by SOCKBUF_LOCK(@so^.so_snd).
 * (e) locked by ACCEPT_LOCK().
 * (f) not locked since integer reads/writes are atomic.
 * (g) used only as a sleep/wakeup address, no value.
 * (h) locked by global mutex so_global_mtx.
 }
 pp_socket=^p_socket;
 p_socket=^t_socket;
 t_socket=packed record
  so_count  :Integer; { (b) reference count }
  so_type   :Word;    { (a) generic type, see socket.h }
  so_options:Word;    { from socket call, see socket.h }
  so_linger :Word;    { time to linger while closing }
  so_state  :Word;    { (b) internal state flags SS_* }
  so_qstate :Integer; { (e) internal state flags SQ_* }
  so_pcb    :Pointer; { protocol control block }
  so_vnet   :Pointer; { network stack instance }
  so_proto  :Pointer; { (a) protocol handle }
 {
  * Variables for connection queuing.
  * Socket where accepts occur is so_head in all subsidiary sockets.
  * If so_head is 0, socket is not related to an accept.
  * For head socket so_incomp queues partially completed connections,
  * while so_comp is a queue of connections ready to be accepted.
  * If a connection is aborted and it has so_head set, then
  * it has to be pulled out of either so_incomp or so_comp.
  * We allow connections to queue up based on current queue lengths
  * and limit on number of queued connections for this socket.
  }
  so_head   :p_socket   ; { (e) back pointer to listen socket }
  so_incomp :TAILQ_HEAD ; { (e) queue of partial unaccepted connections }
  so_comp   :TAILQ_HEAD ; { (e) queue of complete unaccepted connections }
  so_list   :TAILQ_ENTRY; { (e) list of unaccepted connections }
  so_qlen   :Word       ; { (e) number of unaccepted connections }
  so_incqlen:Word       ; { (e) number of unaccepted incomplete connections }
  so_qlimit :Word       ; { (e) max number queued connections }
  so_timeo  :Word       ; { (g) connection timeout }
  so_error  :Word       ; { (f) error affecting connection }
  so_sigio  :Pointer    ; { [sg] information for async I/O or out of band data (SIGURG) }
  so_oobmark:QWORD      ; { (c) chars to oob mark }
  so_aiojobq:TAILQ_HEAD ; { AIO ops waiting on socket }

  so_rcv    :t_sockbuf;
  so_snd    :t_sockbuf;

  //label *so_label     ; { (b) MAC label for socket }
  //label *so_peerlabel ; { (b) cached MAC label for peer }
  { NB: generation count must not be first. }
  so_gencnt  :so_gen_t;  { (h) generation count }
  so_emuldata:Pointer ;  { (b) private data for emulators }

  so_accf:packed record
   so_accept_filter    :p_accept_filter;
   so_accept_filter_arg:Pointer; { saved filter args }
   so_accept_filter_str:Pointer; { saved user args }
  end;
  {
   * so_fibnum, so_user_cookie and friends can be used to attach
   * some user-specified metadata to a socket, which then can be
   * used by the kernel for various actions.
   * so_user_cookie is used by ipfw/dummynet.
   }
  so_fibnum     :Integer;  { routing domain for this socket }
  so_user_cookie:DWORD;
 end;

{
 * Global accept mutex to serialize access to accept queues and
 * fields associated with multiple sockets.  This allows us to
 * avoid defining a lock order between listen and accept sockets
 * until such time as it proves to be a good idea.
 }
var
 accept_mtx:mtx;

const
{
 * Socket state bits stored in so_qstate.
 }
 SQ_INCOMP=$0800; { unaccepted, incomplete connection }
 SQ_COMP  =$1000; { unaccepted, complete connection }

type
{
 * Externalized form of struct socket used by the sysctl(3) interface.
 }
 xsocket=packed record
  xso_len     :QWORD  ; { length of this structure }
  xso_so      :p_socket; { makes a convenient handle sometimes }
  so_type     :Word;
  so_options  :Word;
  so_linger   :Word;
  so_state    :Word;
  so_pcb      :Pointer;  { another convenient handle }
  xso_protocol:Integer;
  xso_family  :Integer;
  so_qlen     :Word;
  so_incqlen  :Word;
  so_qlimit   :Word;
  so_timeo    :Word;
  so_error    :Word;
  so_pgid     :Integer;
  so_oobmark  :QWORD;
  so_rcv      :t_xsockbuf;
  so_snd      :t_xsockbuf;
  so_uid      :uid_t;  { XXX }
 end;

{
 * Macros for sockets and socket buffering.
 }

const
{
 * Flags to sblock().
 }
 SBL_WAIT  =$00000001; { Wait if not immediately available. }
 SBL_NOINTR=$00000002; { Force non-interruptible sleep. }
 SBL_VALID =(SBL_WAIT or SBL_NOINTR);

{ 'which' values for socket upcalls. }
 SO_RCV=1;
 SO_SND=2;

{ Return values for socket upcalls. }
 SU_OK         =0;
 SU_ISCONNECTED=1;

procedure ACCEPT_LOCK_ASSERT(); inline;
procedure ACCEPT_LOCK(); inline;
procedure ACCEPT_UNLOCK(); inline;

function  SOCK_MTX(_so:p_socket):p_mtx; inline;
procedure SOCK_LOCK(_so:p_socket); inline;
function  SOCK_OWNED(_so:p_socket):Boolean; inline;
procedure SOCK_UNLOCK(_so:p_socket); inline;
procedure SOCK_LOCK_ASSERT(_so:p_socket); inline;

function  sb_notify(sb:p_sockbuf):Boolean; inline;
function  sosendallatonce(so:p_socket):Boolean; inline;
function  soreadabledata(so:p_socket):Boolean; inline;
function  soreadable(so:p_socket):Boolean; inline;
function  sowriteable(so:p_socket):Boolean; inline;
procedure soref(so:p_socket);
procedure sorele(so:p_socket);

implementation

procedure ACCEPT_LOCK_ASSERT(); inline;
begin
 mtx_assert(accept_mtx);
end;

procedure ACCEPT_LOCK(); inline;
begin
 mtx_lock(accept_mtx);
end;

procedure ACCEPT_UNLOCK(); inline;
begin
 mtx_unlock(accept_mtx);
end;

//

function SOCK_MTX(_so:p_socket):p_mtx; inline;
begin
 Result:=SOCKBUF_MTX(@_so^.so_rcv)
end;

procedure SOCK_LOCK(_so:p_socket); inline;
begin
 SOCKBUF_LOCK(@_so^.so_rcv)
end;

function SOCK_OWNED(_so:p_socket):Boolean; inline;
begin
 Result:=SOCKBUF_OWNED(@_so^.so_rcv);
end;

procedure SOCK_UNLOCK(_so:p_socket); inline;
begin
 SOCKBUF_UNLOCK(@_so^.so_rcv)
end;

procedure SOCK_LOCK_ASSERT(_so:p_socket); inline;
begin
 SOCKBUF_LOCK_ASSERT(@_so^.so_rcv);
end;

//

{
 * Do we need to notify the other side when I/O is possible?
 }
function sb_notify(sb:p_sockbuf):Boolean; inline;
begin
 Result:=(sb^.sb_flags and (SB_WAIT or SB_SEL or SB_ASYNC or SB_UPCALL or SB_AIO or SB_KNOTE))<>0;
end;

{ do we have to send all at once on a socket? }
function sosendallatonce(so:p_socket):Boolean; inline;
begin
 //Result:=((so^.so_proto^.pr_flags and PR_ATOMIC)<>0);
 Result:=False;
end;

{ can we read something from so? }
function soreadabledata(so:p_socket):Boolean; inline;
begin
 Result:=(so^.so_rcv.sb_cc >= so^.so_rcv.sb_lowat) or
         (not TAILQ_EMPTY(@so^.so_comp)) or (so^.so_error<>0);
end;

function soreadable(so:p_socket):Boolean; inline;
begin
 Result:=soreadabledata(so) or ((so^.so_rcv.sb_state and SBS_CANTRCVMORE)<>0);
end;

{ can we write something to so? }
function sowriteable(so:p_socket):Boolean; inline;
begin
 Result:=((sbspace(@so^.so_snd)>=so^.so_snd.sb_lowat) and
          (((so^.so_state and SS_ISCONNECTED)<>0) or
           ({(so^.so_proto^.pr_flags and PR_CONNREQUIRED)=0}False))) or
         ((so^.so_snd.sb_state and SBS_CANTSENDMORE)<>0) or
         (so^.so_error<>0);
end;

{
 * soref()/sorele() ref-count the socket structure.  Note that you must
 * still explicitly close the socket, but the last ref count will free
 * the structure.
 }
procedure soref(so:p_socket);
begin
 SOCK_LOCK_ASSERT(so);
 Inc(so^.so_count);
end;

procedure sorele(so:p_socket);
begin
 ACCEPT_LOCK_ASSERT();
 SOCK_LOCK_ASSERT(so);
 if (so^.so_count <= 0) then
  Assert(false,'sorele');
 Dec(so^.so_count);
 if (so^.so_count=0) then
 begin
  //sofree(so);
 end else
 begin
  SOCK_UNLOCK(so);
  ACCEPT_UNLOCK();
 end
end;


end.

