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
 kern_umtx,
 kern_osem,
 kern_evf,
 vmount,
 vfiledesc,
 vm_map,
 kern_mtxpool,
 vsys_generic,
 vfs_subr,
 vfs_lookup,
 devfs;

//Daemon for a separate thread
procedure sys_update;
begin
 vnlru_proc;
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
 osem_sysinit;
 evf_sysinit;
 vmountinit;
 fd_table_init;
 vminit;
 mtx_pool_setup_dynamic;
 selectinit;
 vntblinit;
 nameiinit;
 dirlist_mtx_init;
 devfs_devs_init;
end;

end.

