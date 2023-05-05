unit fdesc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount,
 vfs_vnode,
 kern_mtx,
 subr_hash;

const
 { Private mount flags for fdescfs. }
 FMNT_UNMOUNTF=$01;
 FD_ROOT=1;
 FD_DESC=3;

type
 p_fdescmount=^t_fdescmount;
 t_fdescmount=packed record
  f_root:p_vnode; { Root node }
  flags:Integer;
 end;

 fdntype=(_Froot,_Fdesc);

 p_fdescnode=^t_fdescnode;
 t_fdescnode=packed record
  fd_hash :LIST_ENTRY; { Hash list }
  fd_vnode:p_vnode;    { Back ptr to vnode }
  fd_type :fdntype;    { Type of this node }
  fd_fd   :Integer;    { Fd to be dup'ed }
  fd_ix   :Integer;    { filesystem index }
 end;

function VFSTOFDESC(mp:p_mount):p_fdescmount;
function VTOFDESC  (vp:p_vnode):p_fdescnode;

function fdesc_init(vfsp:p_vfsconf):Integer;
function fdesc_uninit(vfsp:p_vfsconf):Integer;
function FD_NHASH(ix:Integer):Pointer;

const
 NFDCACHE=4;

type
 p_fdhashhead=^t_fdhashhead;
 t_fdhashhead=LIST_HEAD; //fdescnode

var
 fdesc_hashmtx:mtx;

 fdhashtbl:p_fdhashhead=nil;
 fdhash   :QWORD=0;

implementation

function VFSTOFDESC(mp:p_mount):p_fdescmount;
begin
 Result:=mp^.mnt_data;
end;

function VTOFDESC(vp:p_vnode):p_fdescnode;
begin
 Result:=vp^.v_data;
end;

{
 * Initialise cache headers
 }
function fdesc_init(vfsp:p_vfsconf):Integer;
begin
 mtx_init(fdesc_hashmtx, 'fdescfs_hash');
 fdhashtbl:=hashinit(NFDCACHE, @fdhash);
 Exit(0);
end;

{
 * Uninit ready for unload.
 }
function fdesc_uninit(vfsp:p_vfsconf):Integer;
begin
 hashdestroy(fdhashtbl, fdhash);
 mtx_destroy(fdesc_hashmtx);
 Exit(0);
end;

function FD_NHASH(ix:Integer):Pointer;
begin
 Result:=@fdhashtbl[ix and fdhash];
end;


end.

