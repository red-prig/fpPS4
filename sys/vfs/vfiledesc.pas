unit vfiledesc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vfile,
 kern_rwlock,
 vfs_vnode;

type
{
 * This structure is used for the management of descriptors.  It may be
 * shared by multiple processes.
 }
 P_NDSLOTTYPE=^NDSLOTTYPE;
 NDSLOTTYPE=QWORD;

 p_filedesc=^t_filedesc;
 t_filedesc=packed record
  fd_ofiles           :pp_file     ; { file structures for open files }
  fd_ofileflags       :PChar       ; { per-process open file flags }
  fd_cdir             :p_vnode     ; { current directory }
  fd_rdir             :p_vnode     ; { root directory }
  fd_jdir             :p_vnode     ; { jail root directory }
  fd_nfiles           :Integer     ; { number of open files allocated }
  fd_map              :P_NDSLOTTYPE; { bitmap of free fds }
  fd_lastfile         :Integer     ; { high-water mark of fd_ofiles }
  fd_freefile         :Integer     ; { approx. next free file }
  fd_cmask            :Word        ; { mask for file creation }
  fd_refcnt           :Word        ; { thread reference count }
  fd_holdcnt          :Word        ; { hold count on structure + mutex }
  fd_sx               :Pointer     ; { protects members of this }
  //fd_kqlist           :kqlist      ; { list of kqueues on this filedesc }
  fd_holdleaderscount :Integer     ; { block fdfree() for shared close() }
  fd_holdleaderswakeup:Integer     ; { fdfree() needs wakeup }
 end;

{
 * Per-process open flags.
 }
const
 UF_EXCLOSE=$01; { auto-close on exec }

var
 g_fd:t_filedesc;

procedure FILEDESC_LOCK_INIT(fdp:p_filedesc); inline;
procedure FILEDESC_LOCK_DESTROY(fdp:p_filedesc); inline;
function  FILEDESC_LOCK(fdp:p_filedesc):PPointer; inline;
procedure FILEDESC_XLOCK(fdp:p_filedesc); inline;
procedure FILEDESC_XUNLOCK(fdp:p_filedesc); inline;
procedure FILEDESC_SLOCK(fdp:p_filedesc); inline;
procedure FILEDESC_SUNLOCK(fdp:p_filedesc); inline;

function  fget_locked(fdp:p_filedesc;fd:Integer):p_file; inline;

implementation

procedure FILEDESC_LOCK_INIT(fdp:p_filedesc); inline;
begin
 fdp^.fd_sx:=nil;
end;

procedure FILEDESC_LOCK_DESTROY(fdp:p_filedesc); inline;
begin
 //
end;

function FILEDESC_LOCK(fdp:p_filedesc):PPointer; inline;
begin
 Result:=@fdp^.fd_sx;
end;

procedure FILEDESC_XLOCK(fdp:p_filedesc); inline;
begin
 rw_wlock(fdp^.fd_sx);
end;

procedure FILEDESC_XUNLOCK(fdp:p_filedesc); inline;
begin
 rw_wunlock(fdp^.fd_sx);
end;

procedure FILEDESC_SLOCK(fdp:p_filedesc); inline;
begin
 rw_rlock(fdp^.fd_sx);
end;

procedure FILEDESC_SUNLOCK(fdp:p_filedesc); inline;
begin
 rw_runlock(fdp^.fd_sx);
end;

 //FILEDESC_LOCK_ASSERT(fdp) sx_assert(@(fdp)^.fd_sx, SX_LOCKED or SX_NOTRECURSED)
 //FILEDESC_XLOCK_ASSERT(fdp) sx_assert(@(fdp)^.fd_sx, SX_XLOCKED or SX_NOTRECURSED)

function fget_locked(fdp:p_filedesc;fd:Integer):p_file; inline;
begin
 if (fd<0) or (fd>=fdp^.fd_nfiles) then
  Result:=nil
 else
  Result:=fdp^.fd_ofiles[fd];
end;



end.

