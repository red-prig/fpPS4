unit vsockstate;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
{
 * Socket state bits.
 *
 * Historically, this bits were all kept in the so_state field.  For
 * locking reasons, they are now in multiple fields, as they are
 * locked differently.  so_state maintains basic socket state protected
 * by the socket lock.  so_qstate holds information about the socket
 * accept queues.  Each socket buffer also has a state field holding
 * information relevant to that socket buffer (can't send, rcv).  Many
 * fields will be read without locks to improve performance and avoid
 * lock order issues.  However, this approach must be used with caution.
 }
 SS_NOFDREF        =$0001; { no file table ref any more }
 SS_ISCONNECTED    =$0002; { socket connected to a peer }
 SS_ISCONNECTING   =$0004; { in process of connecting to peer }
 SS_ISDISCONNECTING=$0008; { in process of disconnecting }
 SS_NBIO           =$0100; { non-blocking ops }
 SS_ASYNC          =$0200; { async i/o notify }
 SS_ISCONFIRMING   =$0400; { deciding to accept connection req }
 SS_ISDISCONNECTED =$2000; { socket disconnected from peer }

{
 * Protocols can mark a socket as SS_PROTOREF to indicate that, following
 * pru_detach, they still want the socket to persist, and will free it
 * themselves when they are done.  Protocols should only ever call sofree()
 * following setting this flag in pru_detach(), and never otherwise, as
 * sofree() bypasses socket reference counting.
 }
 SS_PROTOREF =$4000; { strong protocol reference }

{
 * Socket state bits now stored in the socket buffer state field.
 }
 SBS_CANTSENDMORE=$0010; { can't send more data to peer }
 SBS_CANTRCVMORE =$0020; { can't receive more data from peer }
 SBS_RCVATMARK   =$0040; { at mark on input }

implementation

end.

