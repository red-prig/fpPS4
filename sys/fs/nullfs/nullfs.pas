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

function NULLVPTOLOWERVP(vp:p_vnode):p_vnode;

implementation

function VTONULL(vp:p_vnode):p_null_node; inline;
begin
 Result:=vp^.v_data;
end;

function NULLVPTOLOWERVP(vp:p_vnode):p_vnode;
var
 xp:p_null_node;
begin
 xp:=VTONULL(vp);
 if (xp<>nil) then
 begin
  Result:=xp^.null_lowervp;
 end else
 begin
  Result:=nil;
 end;
end;

end.

