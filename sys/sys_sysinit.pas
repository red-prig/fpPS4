unit sys_sysinit;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

procedure sys_update;
procedure sys_init;

implementation

uses
 kern_time,
 subr_sleepqueue,
 kern_thr,
 kern_thread,
 kern_sig,
 kern_timeout,
 kern_umtx,
 kern_namedobj,
 vmount,
 vfiledesc,
 vm_map,
 kern_mtxpool,
 vsys_generic,
 vfs_subr,
 vfs_lookup,
 vfs_init,
 kern_event,
 devfs,
 devfs_devs,
 devfs_vfsops,
 fdesc_vfsops,
 null_vfsops,
 ufs,
 kern_descrip,
 vfs_mountroot;

//Daemon for a separate thread
procedure sys_update;
begin
 vnlru_proc;
end;

procedure module_init;
begin
 vfs_register(@devfs_vfsconf);
 vfs_register(@fdescfs_vfsconf);
 vfs_register(@nullfs_vfsconf);
 vfs_register(@ufs_vfsconf);
 vfs_mountroot.vfs_mountroot();
 fildesc_drvinit;
end;

//Manual order of lazy initialization
procedure sys_init;
begin
 timeinit;
 init_sleepqueues;
 PROC_INIT;
 threadinit;
 siginit;
 umtxq_sysinit;
 kern_timeout_init;
 named_table_init;
 vmountinit;
 fd_table_init;
 vminit;
 mtx_pool_setup_dynamic;
 selectinit;
 vntblinit;
 nameiinit;
 knote_init;
 vfs_event_init;
 devfs_mtx_init;
 devfs_devs_init;
 module_init;
end;

end.

