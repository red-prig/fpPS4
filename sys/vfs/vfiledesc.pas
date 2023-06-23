unit vfiledesc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 sys_event,
 vfile,
 vnode,
 kern_sx,
 kern_id;

type
{
 * This structure is used for the management of descriptors.  It may be
 * shared by multiple processes.
 }
 P_NDSLOTTYPE=^NDSLOTTYPE;
 NDSLOTTYPE=QWORD;

 p_filedesc=^t_filedesc;
 t_filedesc=packed object
  fd_ofiles           :t_id_desc_table;
  fd_cdir             :p_vnode        ; { current directory }
  fd_rdir             :p_vnode        ; { root directory }
  fd_jdir             :p_vnode        ; { jail root directory }
  fd_sx               :t_sx           ; { protects members of this }
  fd_kqlist           :t_kqlist       ; { list of kqueues on this filedesc }
  fd_holdleaderscount :Integer        ; { block fdfree() for shared close() }
  fd_holdleaderswakeup:Integer        ; { fdfree() needs wakeup }
  fd_cmask            :Word           ; { mask for file creation }
  //
  property fd_nfiles  :Integer read fd_ofiles.FCount;
  property fd_lastfile:Integer read fd_ofiles.FPos;
  property fd_freefd  :Integer read fd_ofiles.FSpace;
 end;

{
 * Per-process open flags.
 }
const
 UF_EXCLOSE=$01; { auto-close on exec }

var
 fd_table:t_filedesc;

procedure FILEDESC_LOCK_INIT   (fdp:p_filedesc); inline;
procedure FILEDESC_LOCK_DESTROY(fdp:p_filedesc); inline;
function  FILEDESC_LOCK        (fdp:p_filedesc):p_sx; inline;
procedure FILEDESC_XLOCK       (fdp:p_filedesc); inline;
procedure FILEDESC_XUNLOCK     (fdp:p_filedesc); inline;
procedure FILEDESC_SLOCK       (fdp:p_filedesc); inline;
procedure FILEDESC_SUNLOCK     (fdp:p_filedesc); inline;

function  fget_locked(fdp:p_filedesc;fd:Integer):p_file; inline;

procedure fd_table_init; //SYSINIT

implementation

procedure FILEDESC_LOCK_INIT(fdp:p_filedesc); inline;
begin
 sx_init(@fdp^.fd_sx,'filedesc structure');
end;

procedure FILEDESC_LOCK_DESTROY(fdp:p_filedesc); inline;
begin
 //
end;

function FILEDESC_LOCK(fdp:p_filedesc):p_sx; inline;
begin
 Result:=@fdp^.fd_sx;
end;

procedure FILEDESC_XLOCK(fdp:p_filedesc); inline;
begin
 sx_xlock(@fdp^.fd_sx);
end;

procedure FILEDESC_XUNLOCK(fdp:p_filedesc); inline;
begin
 sx_xunlock(@fdp^.fd_sx);
end;

procedure FILEDESC_SLOCK(fdp:p_filedesc); inline;
begin
 sx_slock(@fdp^.fd_sx);
end;

procedure FILEDESC_SUNLOCK(fdp:p_filedesc); inline;
begin
 sx_sunlock(@fdp^.fd_sx);
end;

 //FILEDESC_LOCK_ASSERT(fdp) sx_assert(@(fdp)^.fd_sx, SX_LOCKED or SX_NOTRECURSED)
 //FILEDESC_XLOCK_ASSERT(fdp) sx_assert(@(fdp)^.fd_sx, SX_XLOCKED or SX_NOTRECURSED)

function fget_locked(fdp:p_filedesc;fd:Integer):p_file; inline;
begin
 if (fd<0) then
  Result:=nil
 else
  Result:=p_file(id_get(@fdp^.fd_ofiles,fd));
end;

procedure fd_table_init;
begin
 id_table_init(@fd_table.fd_ofiles,0,maxfilesperproc);
 FILEDESC_LOCK_INIT(@fd_table);
 TAILQ_INIT(@fd_table.fd_kqlist);
end;

finalization
 id_table_fini(@fd_table.fd_ofiles);

end.

