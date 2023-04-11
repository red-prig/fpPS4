unit kern_descrip;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 vcapability,
 vfs_vnode;

function fgetvp_rights(td:p_kthread;
                       fd:Integer;
                       need:cap_rights_t;
                       have:p_cap_rights_t;
                       vpp:pp_vnode):Integer;

implementation

function fgetvp_rights(td:p_kthread;
                       fd:Integer;
                       need:cap_rights_t;
                       have:p_cap_rights_t;
                       vpp:pp_vnode):Integer;
begin

end;

end.

