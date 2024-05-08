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
 vmount,
 vfiledesc,
 vm_map,
 kern_dmem,
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
 vfs_mountroot,
 sys_conf,
 sched_ule,
 dev_null,
 dev_tty,
 dev_dmem,
 dev_dipsw,
 dev_rng,
 dev_gc,
 dev_dce,
 dev_hid;

var
 daemon_thr:p_kthread;

//Daemon for a separate thread
procedure sys_daemon(arg:Pointer);
begin
 sched_prio(curkthread,1000);
 repeat
  vnlru_proc;
  pause('sys_daemon',hz);
 until false;
end;

procedure sys_daemon_init;
var
 n:Integer;
begin
 n:=kthread_add(@sys_daemon,nil,@daemon_thr,0,'sys_daemon');
 Assert(n=0,'sys_daemon');
end;

procedure module_init;
begin
 vfs_register(@devfs_vfsconf);
 vfs_register(@fdescfs_vfsconf);
 vfs_register(@nullfs_vfsconf);
 vfs_register(@ufs_vfsconf);
 vfs_mountroot.vfs_mountroot();
 fildesc_drvinit;
 //
 null_modevent(0,MOD_LOAD);
 ttyconsdev_init();
 dmemdev_init();
 dipsw_init();
 rng_init();
 gc_initialize();
 dce_initialize();
 hid_init();
end;

//Manual order of lazy initialization
procedure sys_init;
begin
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
 vminit;
 init_dmem_map;
 mtx_pool_setup_dynamic;
 selectinit;
 vntblinit;
 nameiinit;
 knote_init;
 vfs_event_init;
 devfs_mtx_init;
 devfs_devs_init;
 module_init;
 sys_daemon_init;
end;

end.

