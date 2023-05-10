unit nullfs;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount,
 vnode;

const
 NULLM_CACHE=$0001;

 NULLV_NOUNLOCK=$0001;
 NULLV_DROP    =$0002;

type
 p_null_mount=^t_null_mount;
 t_null_mount=packed record
  nullm_vfs   :p_mount;
  nullm_rootvp:p_vnode; { Reference to root null_node }
  nullm_flags :QWORD;
 end;

 p_null_node=^t_null_node;
 t_null_node=packed record
  null_hash   :LIST_ENTRY; { Hash list }
  null_lowervp:p_vnode;    { VREFed once }
  null_vnode  :p_vnode;    { Back pointer }
  null_flags  :DWORD;
 end;

function MOUNTTONULLMOUNT(mp:p_mount):p_null_mount;
function VTONULL(vp:p_vnode):p_null_node;
function NULLTOV(xp:p_null_node):p_vnode;
function NULLVPTOLOWERVP(vp:p_vnode):p_vnode;

implementation

function MOUNTTONULLMOUNT(mp:p_mount):p_null_mount;
begin
 Result:=mp^.mnt_data;
end;

function VTONULL(vp:p_vnode):p_null_node;
begin
 Result:=vp^.v_data;
end;

function NULLTOV(xp:p_null_node):p_vnode;
begin
 Result:=xp^.null_vnode;
end;

function NULLVPTOLOWERVP(vp:p_vnode):p_vnode;
begin
 Result:=VTONULL(vp)^.null_lowervp;
end;

end.

