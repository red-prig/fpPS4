unit sys_sysinit;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

procedure sys_init;

implementation

uses
 init_sysent, //compile spec
 kern_rtprio, //compile spec
 kern_conf,   //compile spec
 devfs_vnops, //compile spec
 devfs_rule,  //compile spec
 time,
 kern_time,
 subr_sleepqueue,
 kern_sysctl,
 kern_thr,
 kern_thread,
 kern_proc,
 kern_sig,
 kern_timeout,
 kern_synch,
 kern_umtx,
 kern_namedobj,
 kern_rangelock,
 kern_hazard_pointer,
 kern_evf,
 kern_osem,
 vmount,
 vfiledesc,
 vm_map,
 vm_object,
 uma_core,
 kern_hamt,
 kern_dmem,
 kern_mtxpool,
 kern_malloc,
 vsys_generic,
 vfs_subr,
 vfs_lookup,
 vfs_init,
 kern_event,
 kern_pipe,
 devfs,
 devfs_devs,
 devfs_vfsops,
 fdesc_vfsops,
 null_vfsops,
 tmpfs_vfsops,
 ufs,
 kern_descrip,
 vfs_mount,
 vfs_mountroot,
 sys_conf,
 sched_ule,
 subr_dynlib,
 dev_null,
 dev_sce_zlib,
 dev_tty,
 dev_dmem,
 dev_dipsw,
 dev_rng,
 dev_random,
 dev_gc,
 dev_dce,
 dev_hid,
 dev_camera,
 kern_daemon;

procedure module_init;
begin
 vfs_register(@devfs_vfsconf);
 vfs_register(@fdescfs_vfsconf);
 vfs_register(@nullfs_vfsconf);
 vfs_register(@ufs_vfsconf);
 vfs_register(@tmpfs_vfsconf);
 vfs_mount_init();
 vfs_mountroot.vfs_mountroot();
 fildesc_drvinit;
 //
 null_modevent(0,MOD_LOAD);
 zlib_modevent(0,MOD_LOAD);
 ttyconsdev_init();
 dmemdev_init();
 dipsw_init();
 rng_init();
 random_init();
 gc_initialize();
 dce_initialize();
 hid_init();
 camera_init();
end;

//Manual order of lazy initialization
procedure sys_init;
begin
 uma_startup4();
 malloc_init;
 kern_hamt_init;
 timeinit;
 init_sleepqueues;
 sysctl_register_all;
 PROC_INIT;
 threadinit;
 siginit;
 umtxq_sysinit;
 kern_timeout_init;
 named_table_init;
 vmountinit;
 fd_table_init;
 rangelock_sys_init;
 vminit;
 vm_object_init;
 init_dmem_map;
 mtx_pool_setup_dynamic;
 selectinit;
 vntblinit;
 nameiinit;
 knote_init;
 vfs_event_init;
 devfs_mtx_init;
 devfs_devs_init;
 pipeinit;
 module_init;
 hazard_init;
 sys_init_evf;
 sys_init_osem;
 subr_dynlib_init;
 sys_daemon_init;
end;

end.

