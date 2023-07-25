unit vfs_subr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 vmount,
 sys_event,
 vfile,
 vstat,
 vnode,
 vnode_if,
 vdirent,
 vfcntl,
 kern_mtx,
 kern_condvar,
 kern_synch,
 time,
 kern_time,
 kern_thr;

type
 t_insmntque1_dtr=procedure(v:p_vnode;p:Pointer);

function  makedev(x,y:Integer):Integer;

function  vfs_busy(mp:p_mount;flags:Integer):Integer;
procedure vfs_unbusy(mp:p_mount);
function  vfs_getvfs(fsid:p_fsid):p_mount;
procedure vfs_getnewfsid(mp:p_mount);
procedure vfs_timestamp(tsp:p_timespec);
procedure vattr_null(vap:p_vattr);
procedure v_incr_usecount(vp:p_vnode);

procedure vholdl(vp:p_vnode);
procedure vdropl(vp:p_vnode);
procedure vgonel(vp:p_vnode);

procedure vhold(vp:p_vnode);
procedure vdrop(vp:p_vnode);
function  vrecycle(vp:p_vnode):Integer;
procedure vgone(vp:p_vnode);

function  vget(vp:p_vnode;flags:Integer):Integer;
procedure vref(vp:p_vnode);
function  vrefcnt(vp:p_vnode):Integer;
procedure vrele(vp:p_vnode);
procedure vput(vp:p_vnode);
procedure vunref(vp:p_vnode);

procedure vinactive(vp:p_vnode);
function  vflush(mp:p_mount;rootrefs,flags:Integer):Integer;

procedure assert_vi_locked   (vp:p_vnode;str:PChar);
procedure assert_vi_unlocked (vp:p_vnode;str:PChar);
procedure assert_vop_locked  (vp:p_vnode;str:PChar);
procedure assert_vop_unlocked(vp:p_vnode;str:PChar);
procedure assert_vop_elocked (vp:p_vnode;str:PChar);

function  VOP_WRITE_PRE(ap:p_vop_write_args;var osize,ooffset:Int64):Integer;
procedure VOP_WRITE_POST(ap:p_vop_write_args;ret:Integer;var osize,ooffset:Int64);
procedure vop_rename_fail(ap:p_vop_rename_args);
procedure vop_rename_pre(ap:p_vop_rename_args);
procedure vop_create_post(ap:p_vop_create_args;rc:Integer);
procedure vop_link_post(ap:p_vop_link_args;rc:Integer);
procedure vop_mkdir_post(ap:p_vop_mkdir_args;rc:Integer);
procedure vop_mknod_post(ap:p_vop_mknod_args;rc:Integer);
procedure vop_remove_post(ap:p_vop_remove_args;rc:Integer);
procedure vop_rename_post(ap:p_vop_rename_args;rc:Integer);
procedure vop_rmdir_post(ap:p_vop_rmdir_args;rc:Integer);
procedure vop_setattr_post(ap:p_vop_setattr_args;rc:Integer);
procedure vop_symlink_post(ap:p_vop_symlink_args;rc:Integer);

procedure vfs_event_init(); //SYSINIT(vfs_knlist, SI_SUB_VFS, SI_ORDER_ANY, vfs_event_init, NULL);
procedure vfs_event_signal(fsid:p_fsid;event:DWORD;data:ptrint);
function  vfs_kqfilter(ap:p_vop_kqfilter_args):Integer;

function  vfs_read_dirent(ap:p_vop_readdir_args;dp:p_dirent;off:QWORD):Integer;
procedure vfs_mark_atime(vp:p_vnode);
function  vfs_unixify_accmode(accmode:p_accmode_t):Integer;

function  vcount(vp:p_vnode):Integer;
function  count_dev(dev:Pointer):Integer; //cdev

procedure vfs_msync(mp:p_mount;flags:Integer);

procedure destroy_vpollinfo_free(vi:p_vpollinfo);
procedure destroy_vpollinfo(vi:p_vpollinfo);
procedure v_addpollinfo(vp:p_vnode);
function  vn_pollrecord(vp:p_vnode;events:Integer):Integer;

function  vn_isdisk(vp:p_vnode;errp:PInteger):Boolean;

function  vaccess(_type:vtype;
                  file_mode:mode_t;
                  file_uid:uid_t;
                  file_gid:gid_t;
                  accmode:accmode_t;
                  privused:PInteger):Integer;

function  getnewvnode(tag:PChar;mp:p_mount;vops:p_vop_vector;vpp:pp_vnode):Integer;

procedure insmntque_stddtr(vp:p_vnode;dtr_arg:Pointer);
function  insmntque1(vp:p_vnode;mp:p_mount;dtr:t_insmntque1_dtr;dtr_arg:Pointer):Integer;
function  insmntque(vp:p_vnode;mp:p_mount):Integer;

function  vinvalbuf(vp:p_vnode;flags,slpflag,slptimeo:Integer):Integer;

function  __mnt_vnode_next_all(mvp:pp_vnode;mp:p_mount):p_vnode;
function  __mnt_vnode_first_all(mvp:pp_vnode;mp:p_mount):p_vnode;
procedure __mnt_vnode_markerfree_all(mvp:pp_vnode;mp:p_mount);

procedure mnt_vnode_markerfree_active(mvp:pp_vnode;mp:p_mount);
function  mnt_vnode_next_active(mvp:pp_vnode;mp:p_mount):p_vnode;

function  __mnt_vnode_next_active(mvp:pp_vnode;mp:p_mount):p_vnode;
function  __mnt_vnode_first_active(mvp:pp_vnode;mp:p_mount):p_vnode;
procedure __mnt_vnode_markerfree_active(mvp:pp_vnode;mp:p_mount);

procedure vntblinit;    //SYSINIT(vfs, SI_SUB_VFS, SI_ORDER_FIRST, vntblinit, NULL);
procedure vnlru_proc(); //SYSINIT(vnlru, SI_SUB_KTHREAD_UPDATE, SI_ORDER_FIRST, kproc_start, @vnlru_kp);

function  filt_fsattach(kn:p_knote):Integer;
procedure filt_fsdetach(kn:p_knote);
function  filt_fsevent(kn:p_knote;hint:QWORD):Integer;

const
 fs_filtops:t_filterops=(
  f_isfd  :0;
  _align  :0;
  f_attach:@filt_fsattach;
  f_detach:@filt_fsdetach;
  f_event :@filt_fsevent;
 );

{
 * List of vnodes that are ready for recycling.
 }
var
 numvnodes:QWORD=0;

 vnode_free_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@vnode_free_list.tqh_first); //vnode

 mntid_mtx:mtx;
 vnode_free_list_mtx:mtx;

 syncer_delayno:Integer;
 syncer_mask:QWORD;

 //LIST_HEAD(synclist, bufobj);
 //static struct synclist *syncer_workitem_pending[2];

 sync_mtx:mtx;
 sync_wakeup:t_cv;

 syncer_maxdelay:Integer=32;
 syncdelay:Integer=30;
 filedelay:Integer=30;
 dirdelay:Integer=29;
 metadelay:Integer=28;

 rushjob:Integer;
 stat_rush_requests:Integer;

 fs_knlist:t_knlist;

const
 SYNCER_SHUTDOWN_SPEEDUP=4;

var
 sync_vnode_count:Integer;
 syncer_worklist_len:Integer;

type
 syncer_state=(SYNCER_RUNNING, SYNCER_SHUTTING_DOWN, SYNCER_FINAL_DELAY);

var
 desiredvnodes :Integer=0;
 wantfreevnodes:Integer=0;
 freevnodes    :Integer=0;
 vnlru_nowhere :Integer=0;

implementation

uses
 errno,
 vfs_vnops,
 subr_uio,
 vm_object,
 vsys_generic,
 dead_vnops,
 rtprio,
 kern_conf,
 kern_event;

{
 * Macros to control when a vnode is freed and recycled.  All require
 * the vnode interlock.
 }
function VCANRECYCLE(vp:p_vnode):Boolean; inline;
begin
 Result:=((vp^.v_iflag and VI_FREE)<>0) and (vp^.v_holdcnt=0);
end;

function VSHOULDFREE(vp:p_vnode):Boolean; inline;
begin
 Result:=((vp^.v_iflag and VI_FREE)=0) and (vp^.v_holdcnt=0);
end;

function VSHOULDBUSY(vp:p_vnode):Boolean; inline;
begin
 Result:=((vp^.v_iflag and VI_FREE)<>0) and (vp^.v_holdcnt<>0);
end;


var
{ Shift count for (uintptr_t)vp to initialize vp^.v_hash. }
 vnsz2log:Integer;

{
 * Initialize the vnode management data structures.
 *
 * Reevaluate the following cap on the number of vnodes after the physical
 * memory size exceeds 512GB.  In the limit, as the physical memory size
 * grows, the ratio of physical pages to vnodes approaches sixteen to one.
 }
const
 MAXVNODES_MAX=(512 * (1024 * 1024 * 1024 div (16*1024) div 16));
 v_page_count=524288;

procedure vntblinit;
var
 i:DWORD;
begin
 desiredvnodes:=10000;
 if (desiredvnodes > MAXVNODES_MAX) then
 begin
  desiredvnodes:=MAXVNODES_MAX;
 end;
 wantfreevnodes:=desiredvnodes div 4;

 mtx_init(mntid_mtx,'mntid');
 mtx_init(vnode_free_list_mtx,'vnode_free_list');
 {
  * Initialize the filesystem syncer.
  }
 //syncer_workitem_pending[WI_MPSAFEQ]:=hashinit(syncer_maxdelay, M_VNODE,&syncer_mask);
 //syncer_workitem_pending[WI_GIANTQ]:=hashinit(syncer_maxdelay, M_VNODE,&syncer_mask);
 syncer_maxdelay:=syncer_mask + 1;
 mtx_init(sync_mtx,'Syncer mtx');
 cv_init(@sync_wakeup,'syncer');

 i:=1;
 While (i<=sizeof(t_vnode)) do
 begin
  Inc(vnsz2log);
  i:=i shl 1;
 end;
 Dec(vnsz2log);
end;

function vfs_busy(mp:p_mount;flags:Integer):Integer;
begin
 MNT_ILOCK(mp);
 MNT_REF(mp);

 while ((mp^.mnt_kern_flag and MNTK_UNMOUNT)<>0) do
 begin
  if ((flags and MBF_NOWAIT)<>0) or ((mp^.mnt_kern_flag and MNTK_REFEXPIRE)<>0) then
  begin
   MNT_REL(mp);
   MNT_IUNLOCK(mp);
   Exit(ENOENT);
  end;
  if ((flags and MBF_MNTLSTLOCK)<>0) then
  begin
   mtx_unlock(mountlist_mtx);
  end;
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag or MNTK_MWAIT;
  msleep(mp, MNT_MTX(mp), PVFS or PDROP,'vfs_busy', 0);
  if ((flags and MBF_MNTLSTLOCK)<>0) then
  begin
   mtx_lock(mountlist_mtx);
  end;
  MNT_ILOCK(mp);
 end;

 if ((flags and MBF_MNTLSTLOCK)<>0) then
 begin
  mtx_unlock(mountlist_mtx);
 end;

 Inc(mp^.mnt_lockref);
 MNT_IUNLOCK(mp);
 Exit(0);
end;

{
 * Free a busy filesystem.
 }
procedure vfs_unbusy(mp:p_mount);
begin
 MNT_ILOCK(mp);
 MNT_REL(mp);
 Assert(mp^.mnt_lockref>0,'negative mnt_lockref');
 Dec(mp^.mnt_lockref);
 if (mp^.mnt_lockref=0) and ((mp^.mnt_kern_flag and MNTK_DRAINING)<>0) then
 begin
  mp^.mnt_kern_flag:=mp^.mnt_kern_flag and (not MNTK_DRAINING);
  wakeup(@mp^.mnt_lockref);
 end;
 MNT_IUNLOCK(mp);
end;

{
 * Lookup a mount point by filesystem identifier.
 }
function vfs_getvfs(fsid:p_fsid):p_mount;
var
 mp:p_mount;
begin
 mtx_lock(mountlist_mtx);

 mp:=TAILQ_FIRST(@mountlist);
 while (mp<>nil) do
 begin
  if (mp^.mnt_stat.f_fsid.val[0]=fsid^.val[0]) and
     (mp^.mnt_stat.f_fsid.val[1]=fsid^.val[1]) then
  begin
   MNT_REL(mp);
   mtx_unlock(mountlist_mtx);
   Exit(mp);
  end;
  mp:=TAILQ_NEXT(mp,@mp^.mnt_list);
 end;
 mtx_unlock(mountlist_mtx);
 Exit(nil);
end;

function makedev(x,y:Integer):Integer;
begin
 Result:=(x shl 8) or y;
end;

procedure vfs_getnewfsid(mp:p_mount);
var
 mntid_base:Word;
 nmp:p_mount;
 tfsid:fsid_t;
 mtype:Integer;
begin
 mtx_lock(mntid_mtx);
 mtype:=mp^.mnt_vfc^.vfc_typenum;
 tfsid.val[1]:=mtype;
 mntid_base:=0;
 mtype:=(mtype and $FF) shl 24;
 repeat
  tfsid.val[0]:=makedev(255,mtype or ((mntid_base and $FF00) shl 8) or (mntid_base and $FF));
  Inc(mntid_base);
  nmp:=vfs_getvfs(@tfsid);
  if (nmp=nil) then break;
  MNT_REL(nmp);
 until false;
 mp^.mnt_stat.f_fsid.val[0]:=tfsid.val[0];
 mp^.mnt_stat.f_fsid.val[1]:=tfsid.val[1];
 mtx_unlock(mntid_mtx);
end;

{
 * Get a current timestamp.
 }
procedure vfs_timestamp(tsp:p_timespec);
begin
 getnanotime(tsp);
end;

{
 * Set vnode attributes to VNOVAL
 }
procedure vattr_null(vap:p_vattr);
begin
 vap^.va_type             :=VNON;
 vap^.va_size             :=VNOVAL;
 vap^.va_bytes            :=VNOVAL;
 vap^.va_mode             :=VNOVAL;
 vap^.va_nlink            :=VNOVAL;
 vap^.va_uid              :=VNOVAL;
 vap^.va_gid              :=VNOVAL;
 vap^.va_fsid             :=VNOVAL;
 vap^.va_fileid           :=VNOVAL;
 vap^.va_blocksize        :=VNOVAL;
 vap^.va_rdev             :=VNOVAL;
 vap^.va_atime.tv_sec     :=VNOVAL;
 vap^.va_atime.tv_nsec    :=VNOVAL;
 vap^.va_mtime.tv_sec     :=VNOVAL;
 vap^.va_mtime.tv_nsec    :=VNOVAL;
 vap^.va_ctime.tv_sec     :=VNOVAL;
 vap^.va_ctime.tv_nsec    :=VNOVAL;
 vap^.va_birthtime.tv_sec :=VNOVAL;
 vap^.va_birthtime.tv_nsec:=VNOVAL;
 vap^.va_flags            :=VNOVAL;
 vap^.va_gen              :=VNOVAL;
 vap^.va_vaflags          :=0;
end;

function vlrureclaim(mp:p_mount):Integer;
label
 next_iter,
 next_iter_mntunlocked,
 yield,
 relock_mnt;
var
 vp:p_vnode;
 done     :Integer;
 trigger  :Integer;
 usevnodes:Integer;
 count    :Integer;
begin
 usevnodes:=desiredvnodes;
 if (usevnodes <= 0) then usevnodes:=1;
 trigger:=v_page_count * 2 div usevnodes;
 done:=0;
 vn_start_write(nil, @mp, V_WAIT);
 MNT_ILOCK(mp);
 count:=mp^.mnt_nvnodelistsize div 10 + 1;
 while (count<>0) do
 begin
  vp:=TAILQ_FIRST(@mp^.mnt_nvnodelist);
  while (vp<>nil) do
  begin
   if (vp^.v_type<>VMARKER) then Break;
   vp:=TAILQ_NEXT(vp,@vp^.v_nmntvnodes);
  end;

  if (vp=nil) then
   break;

  TAILQ_REMOVE     (@mp^.mnt_nvnodelist,vp,@vp^.v_nmntvnodes);
  TAILQ_INSERT_TAIL(@mp^.mnt_nvnodelist,vp,@vp^.v_nmntvnodes);
  Dec(count);

  if (not VI_TRYLOCK(vp)) then
   goto next_iter;

  {
   * If it's been deconstructed already, it's still
   * referenced, or it exceeds the trigger, skip it.
   }
  if (vp^.v_usecount<>0) or
     {((vlru_allow_cache_src=0) and (not LIST_EMPTY(@vp^.v_cache_src))) or}
     ((vp^.v_iflag and VI_DOOMED)<>0) {or
     ((vp^.v_object<>nil) and (vp^.v_object^.resident_page_count > trigger))} then
  begin
   VI_UNLOCK(vp);
   goto next_iter;
  end;

  MNT_IUNLOCK(mp);
  vholdl(vp);
  if (VOP_LOCK(vp, LK_INTERLOCK or LK_EXCLUSIVE or LK_NOWAIT,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
  begin
   vdrop(vp);
   goto next_iter_mntunlocked;
  end;
  VI_LOCK(vp);
  {
   * v_usecount may have been bumped after VOP_LOCK() dropped
   * the vnode interlock and before it was locked again.
   *
   * It is not necessary to recheck VI_DOOMED because it can
   * only be set by another thread that holds both the vnode
   * lock and vnode interlock.  If another thread has the
   * vnode lock before we get to VOP_LOCK() and obtains the
   * vnode interlock after VOP_LOCK() drops the vnode
   * interlock, the other thread will be unable to drop the
   * vnode lock before our VOP_LOCK() call fails.
   }
  if (vp^.v_usecount<>0) {or
     ((vlru_allow_cache_src=0) and (not LIST_EMPTY(@vp^.v_cache_src))) or
     ((vp^.v_object<>nil) and (vp^.v_object^.resident_page_count > trigger))} then
  begin
   VOP_UNLOCK(vp, LK_INTERLOCK);
   goto next_iter_mntunlocked;
  end;

  Assert((vp^.v_iflag and VI_DOOMED)=0,'VI_DOOMED unexpectedly detected in vlrureclaim()');
  //atomic_add_long(@recycles_count, 1);
  vgonel(vp);
  VOP_UNLOCK(vp, 0);
  vdropl(vp);
  Inc(done);
 next_iter_mntunlocked:
  //if (not should_yield()) then
   goto relock_mnt;
  //goto yield;
 next_iter:
  //if (not should_yield()) then
   continue;
  MNT_IUNLOCK(mp);
 yield:
  kern_yield(PRI_UNCHANGED);
 relock_mnt:
  MNT_ILOCK(mp);
 end;
 MNT_IUNLOCK(mp);
 vn_finished_write(mp);
 Exit(done);
end;

function vtryrecycle(vp:p_vnode):Integer; forward;

procedure vnlru_free(count:Integer);
var
 vp:p_vnode;
 vfslocked:Integer;
begin
 mtx_assert(vnode_free_list_mtx);
 For count:=count downto 0 do
 begin
  vp:=TAILQ_FIRST(@vnode_free_list);
  {
   * The list can be modified while the free_list_mtx
   * has been dropped and vp could be nil here.
   }
  if (vp=nil) then
   break;

  Assert(vp^.v_op<>nil,'vnlru_free: vnode already reclaimed.');
  Assert((vp^.v_iflag and VI_FREE)<>0,'Removing vnode not on freelist');
  Assert((vp^.v_iflag and VI_ACTIVE)=0,'Mangling active vnode');

  TAILQ_REMOVE(@vnode_free_list,vp,@vp^.v_actfreelist);
  {
   * Don't recycle if we can't get the interlock.
   }
  if (not VI_TRYLOCK(vp)) then
  begin
   TAILQ_INSERT_TAIL(@vnode_free_list,vp,@vp^.v_actfreelist);
   continue;
  end;
  Assert(VCANRECYCLE(vp),'vp inconsistent on freelist');
  Dec(freevnodes);
  vp^.v_iflag:=vp^.v_iflag and (not VI_FREE);
  vholdl(vp);
  mtx_unlock(vnode_free_list_mtx);
  VI_UNLOCK(vp);
  vfslocked:=VFS_LOCK_GIANT(vp^.v_mount);
  vtryrecycle(vp);
  VFS_UNLOCK_GIANT(vfslocked);
  {
   * If the recycled succeeded this vdrop will actually free
   * the vnode.  If not it will simply place it back on
   * the free list.
   }
  vdrop(vp);
  mtx_lock(vnode_free_list_mtx);
 end;
end;

//static struct proc *vnlruproc;
var
 vnlruproc_sig:Integer=0;

procedure vnlru_proc();
var
 mp,nmp:p_mount;
 done,vfslocked:Integer;
begin
 //EVENTHANDLER_REGISTER(shutdown_pre_sync, kproc_shutdown, p, SHUTDOWN_PRI_FIRST);

 //kproc_suspend_check(p);
 mtx_lock(vnode_free_list_mtx);
 if (freevnodes > wantfreevnodes) then
 begin
  vnlru_free(freevnodes - wantfreevnodes);
 end;
 if (numvnodes <= desiredvnodes * 9 div 10) then
 begin
  vnlruproc_sig:=0;
  wakeup(@vnlruproc_sig);
  //
  mtx_unlock(vnode_free_list_mtx);
  Exit;
 end;
 mtx_unlock(vnode_free_list_mtx);

 done:=0;
 mtx_lock(mountlist_mtx);
 mp:=TAILQ_FIRST(@mountlist);
 While (mp<>nil) do
 begin
  if (vfs_busy(mp, MBF_NOWAIT or MBF_MNTLSTLOCK)<>0) then
  begin
   nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
   continue;
  end;
  vfslocked:=VFS_LOCK_GIANT(mp);
  Inc(done,vlrureclaim(mp));
  VFS_UNLOCK_GIANT(vfslocked);
  mtx_lock(mountlist_mtx);
  nmp:=TAILQ_NEXT(mp,@mp^.mnt_list);
  vfs_unbusy(mp);
  //
  mp:=nmp
 end;
 mtx_unlock(mountlist_mtx);

 if (done=0) then
 begin
  Inc(vnlru_nowhere);
 end;

end;

function vtryrecycle(vp:p_vnode):Integer;
var
 vnmp:p_mount;
begin
 Assert(vp^.v_holdcnt<>0,'vtryrecycle: Recycling vp %p without a reference.');
 {
  * This vnode may found and locked via some other list, if so we
  * can't recycle it yet.
  }
 if (VOP_LOCK(vp, LK_EXCLUSIVE or LK_NOWAIT,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%})<>0) then
 begin
  Exit(EWOULDBLOCK);
 end;
 {
  * Don't recycle if its filesystem is being suspended.
  }
 if (vn_start_write(vp, @vnmp, V_NOWAIT)<>0) then
 begin
  VOP_UNLOCK(vp, 0);
  Exit(EBUSY);
 end;
 {
  * If we got this far, we need to acquire the interlock and see if
  * anyone picked up this vnode from another list.  If not, we will
  * mark it with DOOMED via vgonel() so that anyone who does find it
  * will skip over it.
  }
 VI_LOCK(vp);
 if (vp^.v_usecount<>0) then
 begin
  VOP_UNLOCK(vp, LK_INTERLOCK);
  vn_finished_write(vnmp);
  Exit(EBUSY);
 end;
 if ((vp^.v_iflag and VI_DOOMED)=0) then
 begin
  //atomic_add_long(@recycles_count, 1);
  vgonel(vp);
 end;
 VOP_UNLOCK(vp, LK_INTERLOCK);
 vn_finished_write(vnmp);
 Exit(0);
end;

function getnewvnode_wait(suspended:Integer):Integer;
begin
 mtx_assert(vnode_free_list_mtx);

 if (curkthread<>nil) and (numvnodes > desiredvnodes) then
 begin
  if (suspended<>0) then
  begin
   {
    * File system is beeing suspended, we cannot risk a
    * deadlock here, so allocate new vnode anyway.
    }
   if (freevnodes > wantfreevnodes) then
   begin
    vnlru_free(freevnodes - wantfreevnodes);
   end;
   Exit(0);
  end;
  if (vnlruproc_sig=0) then
  begin
   vnlruproc_sig:=1; { avoid unnecessary wakeups }
   wakeup(@vnlruproc_sig);
  end;
  msleep(@vnlruproc_sig,@vnode_free_list_mtx,PVFS,'vlruwk', hz);
 end;

 if (numvnodes>desiredvnodes) then
  Exit(ENFILE)
 else
  Exit(0);
end;

procedure getnewvnode_reserve(count:DWORD);
var
 td:p_kthread;
begin
 td:=curkthread;
 { First try to be quick and racy. }
 if (System.InterlockedExchangeAdd64(numvnodes,count) + count <= desiredvnodes) then
 begin
  Inc(td^.td_vp_reserv,count);
  Exit;
 end else
  System.InterlockedExchangeAdd64(numvnodes, -count);

 mtx_lock(vnode_free_list_mtx);
 while (count > 0) do
 begin
  if (getnewvnode_wait(0)=0) then
  begin
   Dec(count);
   Inc(td^.td_vp_reserv);
   System.InterlockedIncrement64(numvnodes);
  end;
 end;
 mtx_unlock(vnode_free_list_mtx);
end;

procedure getnewvnode_drop_reserve();
var
 td:p_kthread;
begin
 td:=curkthread;
 System.InterlockedExchangeAdd64(numvnodes,-td^.td_vp_reserv);
 td^.td_vp_reserv:=0;
end;

{
 * Return the next vnode from the free list.
 }
function getnewvnode(tag:PChar;mp:p_mount;vops:p_vop_vector;vpp:pp_vnode):Integer;
label
 alloc;
var
 td:p_kthread;
 vp:p_vnode;
 //struct bufobj *bo;
 error,susp:Integer;
begin
 vp:=nil;
 td:=curkthread;
 if (td<>nil) then
 begin
  if (td^.td_vp_reserv>0) then
  begin
   Dec(td^.td_vp_reserv,1);
   goto alloc;
  end;
 end;
 mtx_lock(vnode_free_list_mtx);
 {
  * Lend our context to reclaim vnodes if they've exceeded the max.
  }
 if (freevnodes > wantfreevnodes) then
 begin
  vnlru_free(1);
 end;

 susp:=ord(False);
 if (mp<>nil) then
 begin
  susp:=ord(((mp^.mnt_kern_flag and MNTK_SUSPEND)<>0));
 end;
 error:=getnewvnode_wait(susp);

 System.InterlockedIncrement64(numvnodes);
 mtx_unlock(vnode_free_list_mtx);
 alloc:
 //atomic_add_long(@vnodes_created, 1);
 vp:=AllocMem(SizeOf(t_vnode));
 {
  * Setup locks.
  }
 vp^.v_vnlock:=@vp^.v_lock;
 mtx_init(vp^.v_interlock,'vnode interlock');
 {
  * By default, don't allow shared locks unless filesystems
  * opt-in.
  }
 mtx_init(vp^.v_vnlock^,'PVFS');
 //lockinit(vp^.v_vnlock, PVFS, tag, VLKTIMEOUT, LK_NOSHARE);
 {
  * Initialize bufobj.
  }
 //bo:=@vp^.v_bufobj;
 //bo^.__bo_vnode:=vp;
 //mtx_init(BO_MTX(bo), "bufobj interlock", nil, MTX_DEF);
 //bo^.bo_ops:=@buf_ops_bio;
 //bo^.bo_private:=vp;
 //TAILQ_INIT(@bo^.bo_clean.bv_hd);
 //TAILQ_INIT(@bo^.bo_dirty.bv_hd);
 {
  * Initialize namecache.
  }
 //LIST_INIT(@vp^.v_cache_src);
 //TAILQ_INIT(@vp^.v_cache_dst);
 {
  * Finalize various vnode identity bits.
  }
 vp^.v_type:=VNON;
 vp^.v_tag:=tag;
 vp^.v_op:=vops;
 v_incr_usecount(vp);
 vp^.v_data:=nil;

 //mac_vnode_init(vp);
 //if (mp<>nil and (mp^.mnt_flag and MNT_MULTILABEL)=0)
 // mac_vnode_associate_singlelabel(mp, vp);
 //else if (mp=nil and vops<>@dead_vnodeops)
 // printf'nil mp in getnewvnode()\n';

 if (mp<>nil) then
 begin
  //bo^.bo_bsize:=mp^.mnt_stat.f_iosize;
  if ((mp^.mnt_kern_flag and MNTK_NOKNOTE)<>0) then
   vp^.v_vflag:=vp^.v_vflag or VV_NOKNOTE;
 end;
 //rangelock_init(@vp^.v_rl);

 {
  * For the filesystems which do not use vfs_hash_insert(),
  * still initialize v_hash to have vfs_hash_index() useful.
  * E.g., nilfs uses vfs_hash_index() on the lower vnode for
  * its own hashing.
  }
 vp^.v_hash:=ptruint(vp) shr vnsz2log;

 vpp^:=vp;
 Exit(0);
end;

{
 * Delete from old mount point vnode list, if on one.
 }
procedure delmntque(vp:p_vnode);
var
 mp:p_mount;
 active:Integer;
begin
 mp:=vp^.v_mount;
 if (mp=nil) then
  Exit;
 MNT_ILOCK(mp);
 VI_LOCK(vp);
 Assert(mp^.mnt_activevnodelistsize <= mp^.mnt_nvnodelistsize,
     'Active vnode list size %d > Vnode list size %d');
 active:=vp^.v_iflag and VI_ACTIVE;
 vp^.v_iflag:=vp^.v_iflag and (not VI_ACTIVE);
 if (active<>0) then
 begin
  mtx_lock(vnode_free_list_mtx);
  TAILQ_REMOVE(@mp^.mnt_activevnodelist,vp,@vp^.v_actfreelist);
  Dec(mp^.mnt_activevnodelistsize);
  mtx_unlock(vnode_free_list_mtx);
 end;
 vp^.v_mount:=nil;
 VI_UNLOCK(vp);
 Assert(mp^.mnt_nvnodelistsize > 0,'bad mount point vnode list size');
 TAILQ_REMOVE(@mp^.mnt_nvnodelist,vp,@vp^.v_nmntvnodes);
 Dec(mp^.mnt_nvnodelistsize);
 MNT_REL(mp);
 MNT_IUNLOCK(mp);
end;

procedure insmntque_stddtr(vp:p_vnode;dtr_arg:Pointer);
begin
 vp^.v_data:=nil;
 vp^.v_op:=@dead_vnodeops;
 { XXX non mp-safe fs may still call insmntque with vnode
    unlocked }
 if (VOP_ISLOCKED(vp)=0) then
 begin
  vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 end;
 vgone(vp);
 vput(vp);
end;

{
 * Insert into list of vnodes for the new mount point, if available.
 }
function insmntque1(vp:p_vnode;mp:p_mount;dtr:t_insmntque1_dtr;dtr_arg:Pointer):Integer;
var
 locked:Integer;
begin
 Assert(vp^.v_mount=nil,'insmntque: vnode already on per mount vnode list');
 Assert(mp<>nil, 'Dont call insmntque(foo, nil)');

 {
  * We acquire the vnode interlock early to ensure that the
  * vnode cannot be recycled by another process releasing a
  * holdcnt on it before we get it on both the vnode list
  * and the active vnode list. The mount mutex protects only
  * manipulation of the vnode list and the vnode freelist
  * mutex protects only manipulation of the active vnode list.
  * Hence the need to hold the vnode interlock throughout.
  }
 MNT_ILOCK(mp);
 VI_LOCK(vp);
 if ((mp^.mnt_kern_flag and MNTK_NOINSMNTQ)<>0) and
    (((mp^.mnt_kern_flag and MNTK_UNMOUNTF)<>0) or
     (mp^.mnt_nvnodelistsize=0)) then
 begin
  locked:=VOP_ISLOCKED(vp);
  if (locked=0) or
     ((locked=LK_EXCLUSIVE) and
      ((vp^.v_vflag and VV_FORCEINSMQ)=0)) then
  begin
   VI_UNLOCK(vp);
   MNT_IUNLOCK(mp);
   if (dtr<>nil) then
   begin
    dtr(vp, dtr_arg);
   end;
   Exit(EBUSY);
  end;
 end;
 vp^.v_mount:=mp;
 MNT_REF(mp);
 TAILQ_INSERT_TAIL(@mp^.mnt_nvnodelist,vp,@vp^.v_nmntvnodes);
 Assert(mp^.mnt_nvnodelistsize >= 0,'neg mount point vnode list size');
 Inc(mp^.mnt_nvnodelistsize);
 Assert((vp^.v_iflag and VI_ACTIVE)=0,'Activating already active vnode');
 vp^.v_iflag:=vp^.v_iflag or VI_ACTIVE;
 mtx_lock(vnode_free_list_mtx);
 TAILQ_INSERT_HEAD(@mp^.mnt_activevnodelist,vp,@vp^.v_actfreelist);
 Inc(mp^.mnt_activevnodelistsize);
 mtx_unlock(vnode_free_list_mtx);
 VI_UNLOCK(vp);
 MNT_IUNLOCK(mp);
 Exit(0);
end;

function insmntque(vp:p_vnode;mp:p_mount):Integer;
begin
 Exit(insmntque1(vp, mp, @insmntque_stddtr, nil));
end;

{
{
 * Flush out and invalidate all buffers associated with a bufobj
 * Called with the underlying object locked.
 }
int
bufobj_invalbuf(struct bufobj *bo, int flags, int slpflag, int slptimeo)
begin
 int error;

 BO_LOCK(bo);
 if (flags and V_SAVE) begin
  error:=bufobj_wwait(bo, slpflag, slptimeo);
  if (error) begin
   BO_UNLOCK(bo);
   Exit(error);
  end;
  if (bo^.bo_dirty.bv_cnt > 0) begin
   BO_UNLOCK(bo);
   if ((error:=BO_SYNC(bo, MNT_WAIT))<>0)
    Exit(error);
   {
    * XXX We could save a lock/unlock if this was only
    * enabled under INVARIANTS
    }
   BO_LOCK(bo);
   if (bo^.bo_numoutput > 0 or bo^.bo_dirty.bv_cnt > 0)
    panic'vinvalbuf: dirty bufs';
  end;
 end;
 {
  * If you alter this loop please notice that interlock is dropped and
  * reacquired in flushbuflist.  Special care is needed to ensure that
  * no race conditions occur from this.
  }
 do begin
  error:=flushbuflist(@bo^.bo_clean,
      flags, bo, slpflag, slptimeo);
  if (error=0 and !(flags and V_CLEANONLY))
   error:=flushbuflist(@bo^.bo_dirty,
       flags, bo, slpflag, slptimeo);
  if (error<>0 and error<>EAGAIN) begin
   BO_UNLOCK(bo);
   Exit(error);
  end;
 end; while (error<>0);

 {
  * Wait for I/O to complete.  XXX needs cleaning up.  The vnode can
  * have write I/O in-progress but if there is a VM object then the
  * VM object can also have read-I/O in-progress.
  }
 do begin
  bufobj_wwait(bo, 0, 0);
  BO_UNLOCK(bo);
  if (bo^.bo_object<>nil) begin
   VM_OBJECT_LOCK(bo^.bo_object);
   vm_object_pip_wait(bo^.bo_object, "bovlbx';
   VM_OBJECT_UNLOCK(bo^.bo_object);
  end;
  BO_LOCK(bo);
 end; while (bo^.bo_numoutput > 0);
 BO_UNLOCK(bo);

 {
  * Destroy the copy in the VM cache, too.
  }
 if (bo^.bo_object<>nil and
     (flags and (V_ALT or V_NORMAL or V_CLEANONLY))=0) begin
  VM_OBJECT_LOCK(bo^.bo_object);
  vm_object_page_remove(bo^.bo_object, 0, 0, (flags and V_SAVE) ?
      OBJPR_CLEANONLY : 0);
  VM_OBJECT_UNLOCK(bo^.bo_object);
 end;

#ifdef INVARIANTS
 BO_LOCK(bo);
 if ((flags and (V_ALT or V_NORMAL or V_CLEANONLY))=0 and
     (bo^.bo_dirty.bv_cnt > 0 or bo^.bo_clean.bv_cnt > 0))
  panic'vinvalbuf: flush failed';
 BO_UNLOCK(bo);
#endif
 Exit(0);
end;
}

{
 * Flush out and invalidate all buffers associated with a vnode.
 * Called with the underlying object locked.
 }
function vinvalbuf(vp:p_vnode;flags,slpflag,slptimeo:Integer):Integer;
begin
 ASSERT_VOP_LOCKED(vp, 'vinvalbuf');

 if (vp^.v_object<>nil) then
 if (vm_object_t(vp^.v_object)^.handle<>vp) then
 begin
  Exit(0);
 end;

 if (vp^.v_object<>nil) then
 begin
  vm_object_deallocate(vp^.v_object);
  vp^.v_object:=nil;
 end;

 //Exit(bufobj_invalbuf(@vp^.v_bufobj, flags, slpflag, slptimeo));
 Result:=0;
end;

{
{
 * Flush out buffers on the specified list.
 *
 }
static int
flushbuflist( struct bufv *bufv, int flags, struct bufobj *bo, int slpflag,
    int slptimeo)
begin
 struct buf *bp, *nbp;
 int retval, error;
 daddr_t lblkno;
 b_xflags_t xflags;

 ASSERT_BO_LOCKED(bo);

 retval:=0;
 TAILQ_FOREACH_SAFE(bp, &bufv^.bv_hd, b_bobufs, nbp) begin
  if (((flags and V_NORMAL) and (bp^.b_xflags and BX_ALTDATA)) or
      ((flags and V_ALT) and (bp^.b_xflags and BX_ALTDATA)=0)) begin
   continue;
  end;
  lblkno:=0;
  xflags:=0;
  if (nbp<>nil) begin
   lblkno:=nbp^.b_lblkno;
   xflags:=nbp^.b_xflags &
    (BX_BKGRDMARKER or BX_VNDIRTY or BX_VNCLEAN);
  end;
  retval:=EAGAIN;
  error:=BUF_TIMELOCK(bp,
      LK_EXCLUSIVE or LK_SLEEPFAIL or LK_INTERLOCK, BO_MTX(bo),
      "flushbuf", slpflag, slptimeo);
  if (error) begin
   BO_LOCK(bo);
   Exit(error<>ENOLCK ? error : EAGAIN);
  end;
  Assert(bp^.b_bufobj=bo,
      'bp %p wrong b_bufobj %p should be %p",
      bp, bp^.b_bufobj, bo));
  if (bp^.b_bufobj<>bo) begin { XXX: necessary ? }
   BUF_UNLOCK(bp);
   BO_LOCK(bo);
   Exit(EAGAIN);
  end;
  {
   * XXX Since there are no node locks for NFS, I
   * believe there is a slight chance that a delayed
   * write will occur while sleeping just above, so
   * check for it.
   }
  if (((bp^.b_flags and (B_DELWRI or B_INVAL))=B_DELWRI) and
      (flags and V_SAVE)) begin
   BO_LOCK(bo);
   bremfree(bp);
   BO_UNLOCK(bo);
   bp^.b_flags:= or B_ASYNC;
   bwrite(bp);
   BO_LOCK(bo);
   Exit(EAGAIN); { XXX: why not loop ? }
  end;
  BO_LOCK(bo);
  bremfree(bp);
  BO_UNLOCK(bo);
  bp^.b_flags:= or (B_INVAL or B_RELBUF);
  bp^.b_flags:= and ~B_ASYNC;
  brelse(bp);
  BO_LOCK(bo);
  if (nbp<>nil and
      (nbp^.b_bufobj<>bo or
       nbp^.b_lblkno<>lblkno or
       (nbp^.b_xflags &
        (BX_BKGRDMARKER or BX_VNDIRTY or BX_VNCLEAN))<>xflags))
   break;   { nbp invalid }
 end;
 Exit(retval);
end;

{
 * Truncate a file's buffer and pages to a specified length.  This
 * is in lieu of the old vinvalbuf mechanism, which performed unneeded
 * sync activity.
 }
int
vtruncbuf(vp:p_vnode, struct ucred *cred, struct thread *td,
    off_t length, int blksize)
begin
 struct buf *bp, *nbp;
 int anyfreed;
 int trunclbn;
 struct bufobj *bo;

 CTR5(KTR_VFS, "%s: vp %p with cred %p and block %d:%ju", {$I %LINE%},
     vp, cred, blksize, (uintmax_t)length);

 {
  * Round up to the *next* lbn.
  }
 trunclbn:=(length + blksize - 1) div blksize;

 ASSERT_VOP_LOCKED(vp, "vtruncbuf';
restart:
 bo:=@vp^.v_bufobj;
 BO_LOCK(bo);
 anyfreed:=1;
 for (;anyfreed;) begin
  anyfreed:=0;
  TAILQ_FOREACH_SAFE(bp, &bo^.bo_clean.bv_hd, b_bobufs, nbp) begin
   if (bp^.b_lblkno < trunclbn)
    continue;
   if (BUF_LOCK(bp,
       LK_EXCLUSIVE or LK_SLEEPFAIL or LK_INTERLOCK,
       BO_MTX(bo))=ENOLCK)
    goto restart;

   BO_LOCK(bo);
   bremfree(bp);
   BO_UNLOCK(bo);
   bp^.b_flags:= or (B_INVAL or B_RELBUF);
   bp^.b_flags:= and ~B_ASYNC;
   brelse(bp);
   anyfreed:=1;

   BO_LOCK(bo);
   if (nbp<>nil and
       (((nbp^.b_xflags and BX_VNCLEAN)=0) or
       (nbp^.b_vp<>vp) or
       (nbp^.b_flags and B_DELWRI))) begin
    BO_UNLOCK(bo);
    goto restart;
   end;
  end;

  TAILQ_FOREACH_SAFE(bp, &bo^.bo_dirty.bv_hd, b_bobufs, nbp) begin
   if (bp^.b_lblkno < trunclbn)
    continue;
   if (BUF_LOCK(bp,
       LK_EXCLUSIVE or LK_SLEEPFAIL or LK_INTERLOCK,
       BO_MTX(bo))=ENOLCK)
    goto restart;
   BO_LOCK(bo);
   bremfree(bp);
   BO_UNLOCK(bo);
   bp^.b_flags:= or (B_INVAL or B_RELBUF);
   bp^.b_flags:= and ~B_ASYNC;
   brelse(bp);
   anyfreed:=1;

   BO_LOCK(bo);
   if (nbp<>nil and
       (((nbp^.b_xflags and BX_VNDIRTY)=0) or
       (nbp^.b_vp<>vp) or
       (nbp^.b_flags and B_DELWRI)=0)) begin
    BO_UNLOCK(bo);
    goto restart;
   end;
  end;
 end;

 if (length > 0) begin
restartsync:
  TAILQ_FOREACH_SAFE(bp, &bo^.bo_dirty.bv_hd, b_bobufs, nbp) begin
   if (bp^.b_lblkno > 0)
    continue;
   {
    * Since we hold the vnode lock this should only
    * fail if we're racing with the buf daemon.
    }
   if (BUF_LOCK(bp,
       LK_EXCLUSIVE or LK_SLEEPFAIL or LK_INTERLOCK,
       BO_MTX(bo))=ENOLCK) begin
    goto restart;
   end;
   Assert((bp^.b_flags and B_DELWRI), vp,
       'buf(%p) on dirty queue without DELWRI", bp));

   BO_LOCK(bo);
   bremfree(bp);
   BO_UNLOCK(bo);
   bawrite(bp);
   BO_LOCK(bo);
   goto restartsync;
  end;
 end;

 bufobj_wwait(bo, 0, 0);
 BO_UNLOCK(bo);
 vnode_pager_setsize(vp, length);

 Exit(0);
end;

{
 * buf_splay() - splay tree core for the clean/dirty list of buffers in
 *   a vnode.
 *
 * NOTE: We have to deal with the special case of a background bitmap
 * buffer, a situation where two buffers will have the same logical
 * block offset.  We want (1) only the foreground buffer to be accessed
 * in a lookup and (2) must differentiate between the foreground and
 * background buffer in the splay tree algorithm because the splay
 * tree cannot normally handle multiple entities with the same 'index'.
 * We accomplish this by adding differentiating flags to the splay tree's
 * numerical domain.
 }
static
struct buf *
buf_splay(daddr_t lblkno, b_xflags_t xflags, struct buf *root)
begin
 struct buf dummy;
 struct buf *lefttreemax, *righttreemin, *y;

 if (root=nil)
  Exit(nil);
 lefttreemax:=righttreemin:=@dummy;
 for (;;) begin
  if (lblkno < root^.b_lblkno or
      (lblkno=root^.b_lblkno and
      (xflags and BX_BKGRDMARKER) < (root^.b_xflags and BX_BKGRDMARKER))) begin
   if ((y:=root^.b_left)=nil)
    break;
   if (lblkno < y^.b_lblkno) begin
    { Rotate right. }
    root^.b_left:=y^.b_right;
    y^.b_right:=root;
    root:=y;
    if ((y:=root^.b_left)=nil)
     break;
   end;
   { Link into the new root's right tree. }
   righttreemin^.b_left:=root;
   righttreemin:=root;
  end; else if (lblkno > root^.b_lblkno or
      (lblkno=root^.b_lblkno and
      (xflags and BX_BKGRDMARKER) > (root^.b_xflags and BX_BKGRDMARKER))) begin
   if ((y:=root^.b_right)=nil)
    break;
   if (lblkno > y^.b_lblkno) begin
    { Rotate left. }
    root^.b_right:=y^.b_left;
    y^.b_left:=root;
    root:=y;
    if ((y:=root^.b_right)=nil)
     break;
   end;
   { Link into the new root's left tree. }
   lefttreemax^.b_right:=root;
   lefttreemax:=root;
  end; else begin
   break;
  end;
  root:=y;
 end;
 { Assemble the new root. }
 lefttreemax^.b_right:=root^.b_left;
 righttreemin^.b_left:=root^.b_right;
 root^.b_left:=dummy.b_right;
 root^.b_right:=dummy.b_left;
 Exit(root);
end;

static void
buf_vlist_remove(struct buf *bp)
begin
 struct buf *root;
 struct bufv *bv;

 Assert(bp^.b_bufobj<>nil, 'No b_bufobj %p", bp));
 ASSERT_BO_LOCKED(bp^.b_bufobj);
 Assert((bp^.b_xflags and (BX_VNDIRTY|BX_VNCLEAN)) !=
     (BX_VNDIRTY|BX_VNCLEAN),
     'buf_vlist_remove: Buf %p is on two lists", bp));
 if (bp^.b_xflags and BX_VNDIRTY)
  bv:=@bp^.b_bufobj^.bo_dirty;
 else
  bv:=@bp^.b_bufobj^.bo_clean;
 if (bp<>bv^.bv_root) begin
  root:=buf_splay(bp^.b_lblkno, bp^.b_xflags, bv^.bv_root);
  Assert(root=bp, 'splay lookup failed in remove');
 end;
 if (bp^.b_left=nil) begin
  root:=bp^.b_right;
 end; else begin
  root:=buf_splay(bp^.b_lblkno, bp^.b_xflags, bp^.b_left);
  root^.b_right:=bp^.b_right;
 end;
 bv^.bv_root:=root;
 TAILQ_REMOVE(@bv^.bv_hd, bp, b_bobufs);
 bv^.bv_cnt--;
 bp^.b_xflags:= and ~(BX_VNDIRTY or BX_VNCLEAN);
end;

{
 * Add the buffer to the sorted clean or dirty block list using a
 * splay tree algorithm.
 *
 * NOTE: xflags is passed as a constant, optimizing this inline function!
 }
static void
buf_vlist_add(struct buf *bp, struct bufobj *bo, b_xflags_t xflags)
begin
 struct buf *root;
 struct bufv *bv;

 ASSERT_BO_LOCKED(bo);
 Assert((bp^.b_xflags and (BX_VNDIRTY|BX_VNCLEAN))=0,
     'buf_vlist_add: Buf %p has existing xflags %d", bp, bp^.b_xflags));
 bp^.b_xflags:= or xflags;
 if (xflags and BX_VNDIRTY)
  bv:=@bo^.bo_dirty;
 else
  bv:=@bo^.bo_clean;

 root:=buf_splay(bp^.b_lblkno, bp^.b_xflags, bv^.bv_root);
 if (root=nil) begin
  bp^.b_left:=nil;
  bp^.b_right:=nil;
  TAILQ_INSERT_TAIL(@bv^.bv_hd, bp, b_bobufs);
 end; else if (bp^.b_lblkno < root^.b_lblkno or
     (bp^.b_lblkno=root^.b_lblkno and
     (bp^.b_xflags and BX_BKGRDMARKER) < (root^.b_xflags and BX_BKGRDMARKER))) begin
  bp^.b_left:=root^.b_left;
  bp^.b_right:=root;
  root^.b_left:=nil;
  TAILQ_INSERT_BEFORE(root, bp, b_bobufs);
 end; else begin
  bp^.b_right:=root^.b_right;
  bp^.b_left:=root;
  root^.b_right:=nil;
  TAILQ_INSERT_AFTER(@bv^.bv_hd, root, bp, b_bobufs);
 end;
 bv^.bv_cnt++;
 bv^.bv_root:=bp;
end;

{
 * Lookup a buffer using the splay tree.  Note that we specifically avoid
 * shadow buffers used in background bitmap writes.
 *
 * This code isn't quite efficient as it could be because we are maintaining
 * two sorted lists and do not know which list the block resides in.
 *
 * During a "make buildworld" the desired buffer is found at one of
 * the roots more than 60% of the time.  Thus, checking both roots
 * before performing either splay eliminates unnecessary splays on the
 * first tree splayed.
 }
struct buf *
gbincore(struct bufobj *bo, daddr_t lblkno)
begin
 struct buf *bp;

 ASSERT_BO_LOCKED(bo);
 if ((bp:=bo^.bo_clean.bv_root)<>nil and
     bp^.b_lblkno=lblkno and !(bp^.b_xflags and BX_BKGRDMARKER))
  Exit(bp);
 if ((bp:=bo^.bo_dirty.bv_root)<>nil and
     bp^.b_lblkno=lblkno and !(bp^.b_xflags and BX_BKGRDMARKER))
  Exit(bp);
 if ((bp:=bo^.bo_clean.bv_root)<>nil) begin
  bo^.bo_clean.bv_root:=bp:=buf_splay(lblkno, 0, bp);
  if (bp^.b_lblkno=lblkno and !(bp^.b_xflags and BX_BKGRDMARKER))
   Exit(bp);
 end;
 if ((bp:=bo^.bo_dirty.bv_root)<>nil) begin
  bo^.bo_dirty.bv_root:=bp:=buf_splay(lblkno, 0, bp);
  if (bp^.b_lblkno=lblkno and !(bp^.b_xflags and BX_BKGRDMARKER))
   Exit(bp);
 end;
 Exit(nil);
end;

{
 * Associate a buffer with a vnode.
 }
void
bgetvp(vp:p_vnode, struct buf *bp)
begin
 struct bufobj *bo;

 bo:=@vp^.v_bufobj;
 ASSERT_BO_LOCKED(bo);
 Assert(bp^.b_vp=nil, bp^.b_vp, 'bgetvp: not free');

 CTR3(KTR_BUF, "bgetvp(%p) vp %p flags %X", bp, vp, bp^.b_flags);
 Assert((bp^.b_xflags and (BX_VNDIRTY|BX_VNCLEAN))=0, vp,
     'bgetvp: bp already attached! %p", bp));

 vhold(vp);
 if (VFS_NEEDSGIANT(vp^.v_mount) or bo^.bo_flag and BO_NEEDSGIANT)
  bp^.b_flags:= or B_NEEDSGIANT;
 bp^.b_vp:=vp;
 bp^.b_bufobj:=bo;
 {
  * Insert onto list for new vnode.
  }
 buf_vlist_add(bp, bo, BX_VNCLEAN);
end;

{
 * Disassociate a buffer from a vnode.
 }
void
brelvp(struct buf *bp)
begin
 struct bufobj *bo;
 vp:p_vnode;

 CTR3(KTR_BUF, "brelvp(%p) vp %p flags %X", bp, bp^.b_vp, bp^.b_flags);
 Assert(bp^.b_vp<>nil, 'brelvp: nil');

 {
  * Delete from old vnode list, if on one.
  }
 vp:=bp^.b_vp;  { XXX }
 bo:=bp^.b_bufobj;
 BO_LOCK(bo);
 if (bp^.b_xflags and (BX_VNDIRTY or BX_VNCLEAN))
  buf_vlist_remove(bp);
 else
  panic'brelvp: Buffer %p not on queue.", bp);
 if ((bo^.bo_flag and BO_ONWORKLST) and bo^.bo_dirty.bv_cnt=0) begin
  bo^.bo_flag:= and ~BO_ONWORKLST;
  mtx_lock(@sync_mtx);
  LIST_REMOVE(bo, bo_synclist);
  syncer_worklist_len--;
  mtx_unlock(@sync_mtx);
 end;
 bp^.b_flags:= and ~B_NEEDSGIANT;
 bp^.b_vp:=nil;
 bp^.b_bufobj:=nil;
 BO_UNLOCK(bo);
 vdrop(vp);
end;

{
 * Add an item to the syncer work queue.
 }
static void
vn_syncer_add_to_worklist(struct bufobj *bo, int delay)
begin
 int queue, slot;

 ASSERT_BO_LOCKED(bo);

 mtx_lock(@sync_mtx);
 if (bo^.bo_flag and BO_ONWORKLST)
  LIST_REMOVE(bo, bo_synclist);
 else begin
  bo^.bo_flag:= or BO_ONWORKLST;
  syncer_worklist_len++;
 end;

 if (delay > syncer_maxdelay - 2)
  delay:=syncer_maxdelay - 2;
 slot:=(syncer_delayno + delay) and syncer_mask;

 queue:=VFS_NEEDSGIANT(bo^.__bo_vnode^.v_mount) ? WI_GIANTQ :
     WI_MPSAFEQ;
 LIST_INSERT_HEAD(@syncer_workitem_pending[queue][slot], bo,
     bo_synclist);
 mtx_unlock(@sync_mtx);
end;

static int
sysctl_vfs_worklist_len(SYSCTL_HANDLER_ARGS)
begin
 int error, len;

 mtx_lock(@sync_mtx);
 len:=syncer_worklist_len - sync_vnode_count;
 mtx_unlock(@sync_mtx);
 error:=SYSCTL_OUT(req, &len, sizeof(len));
 Exit(error);
end;

SYSCTL_PROC(_vfs, OID_AUTO, worklist_len, CTLTYPE_INT or CTLFLAG_RD, nil, 0,
    sysctl_vfs_worklist_len, "I", "Syncer thread worklist length';

static struct proc *updateproc;
static void sched_sync(void);
static struct kproc_desc up_kp:=begin
 "syncer",
 sched_sync,
 &updateproc
end;
SYSINIT(syncer, SI_SUB_KTHREAD_UPDATE, SI_ORDER_FIRST, kproc_start, &up_kp);

static int
sync_vnode(struct synclist *slp, struct bufobj **bo, struct thread *td)
begin
 vp:p_vnode;
 mp:p_mount;

 *bo:=LIST_FIRST(slp);
 if (*bo=nil)
  Exit(0);
 vp:=(*bo)^.__bo_vnode; { XXX }
 if (VOP_ISLOCKED(vp)<>0 or VI_TRYLOCK(vp)=0)
  Exit(1);
 {
  * We use vhold in case the vnode does not
  * successfully sync.  vhold prevents the vnode from
  * going away when we unlock the sync_mtx so that
  * we can acquire the vnode interlock.
  }
 vholdl(vp);
 mtx_unlock(@sync_mtx);
 VI_UNLOCK(vp);
 if (vn_start_write(vp, &mp, V_NOWAIT)<>0) begin
  vdrop(vp);
  mtx_lock(@sync_mtx);
  Exit(*bo=LIST_FIRST(slp));
 end;
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 (void) VOP_FSYNC(vp, MNT_LAZY, td);
 VOP_UNLOCK(vp, 0);
 vn_finished_write(mp);
 BO_LOCK(*bo);
 if (((*bo)^.bo_flag and BO_ONWORKLST)<>0) begin
  {
   * Put us back on the worklist.  The worklist
   * routine will remove us from our current
   * position and then add us back in at a later
   * position.
   }
  vn_syncer_add_to_worklist(*bo, syncdelay);
 end;
 BO_UNLOCK(*bo);
 vdrop(vp);
 mtx_lock(@sync_mtx);
 Exit(0);
end;

{
 * System filesystem synchronizer daemon.
 }
static void
sched_sync(void)
begin
 struct synclist *gnext, *next;
 struct synclist *gslp, *slp;
 struct bufobj *bo;
 long starttime;
 struct thread *td:=curthread;
 int last_work_seen;
 int net_worklist_len;
 int syncer_final_iter;
 int first_printf;
 int error;

 last_work_seen:=0;
 syncer_final_iter:=0;
 first_printf:=1;
 syncer_state:=SYNCER_RUNNING;
 starttime:=time_uptime;
 td^.td_pflags:= or TDP_NORUNNINGBUF;

 EVENTHANDLER_REGISTER(shutdown_pre_sync, syncer_shutdown, td^.td_proc,
     SHUTDOWN_PRI_LAST);

 mtx_lock(@sync_mtx);
 for (;;) begin
  if (syncer_state=SYNCER_FINAL_DELAY and
      syncer_final_iter=0) begin
   mtx_unlock(@sync_mtx);
   kproc_suspend_check(td^.td_proc);
   mtx_lock(@sync_mtx);
  end;
  net_worklist_len:=syncer_worklist_len - sync_vnode_count;
  if (syncer_state<>SYNCER_RUNNING and
      starttime<>time_uptime) begin
   if (first_printf) begin
    printf'\nSyncing disks, vnodes remaining...';
    first_printf:=0;
   end;
   printf'%d ", net_worklist_len);
  end;
  starttime:=time_uptime;

  {
   * Push files whose dirty time has expired.  Be careful
   * of interrupt race on slp queue.
   *
   * Skip over empty worklist slots when shutting down.
   }
  do begin
   slp:=@syncer_workitem_pending[WI_MPSAFEQ][syncer_delayno];
   gslp:=@syncer_workitem_pending[WI_GIANTQ][syncer_delayno];
   syncer_delayno += 1;
   if (syncer_delayno=syncer_maxdelay)
    syncer_delayno:=0;
   next:=@syncer_workitem_pending[WI_MPSAFEQ][syncer_delayno];
   gnext:=@syncer_workitem_pending[WI_GIANTQ][syncer_delayno];
   {
    * If the worklist has wrapped since the
    * it was emptied of all but syncer vnodes,
    * switch to the FINAL_DELAY state and run
    * for one more second.
    }
   if (syncer_state=SYNCER_SHUTTING_DOWN and
       net_worklist_len=0 and
       last_work_seen=syncer_delayno) begin
    syncer_state:=SYNCER_FINAL_DELAY;
    syncer_final_iter:=SYNCER_SHUTDOWN_SPEEDUP;
   end;
  end; while (syncer_state<>SYNCER_RUNNING and LIST_EMPTY(slp) and
      LIST_EMPTY(gslp) and syncer_worklist_len > 0);

  {
   * Keep track of the last time there was anything
   * on the worklist other than syncer vnodes.
   * Exitto the SHUTTING_DOWN state if any
   * new work appears.
   }
  if (net_worklist_len > 0 or syncer_state=SYNCER_RUNNING)
   last_work_seen:=syncer_delayno;
  if (net_worklist_len > 0 and syncer_state=SYNCER_FINAL_DELAY)
   syncer_state:=SYNCER_SHUTTING_DOWN;
  while (!LIST_EMPTY(slp)) begin
   error:=sync_vnode(slp, &bo, td);
   if (error=1) begin
    LIST_REMOVE(bo, bo_synclist);
    LIST_INSERT_HEAD(next, bo, bo_synclist);
    continue;
   end;

   if (first_printf=0)
    wdog_kern_pat(WD_LASTVAL);

  end;
  if (!LIST_EMPTY(gslp)) begin
   mtx_unlock(@sync_mtx);
   mtx_lock(@Giant);
   mtx_lock(@sync_mtx);
   while (!LIST_EMPTY(gslp)) begin
    error:=sync_vnode(gslp, &bo, td);
    if (error=1) begin
     LIST_REMOVE(bo, bo_synclist);
     LIST_INSERT_HEAD(gnext, bo,
         bo_synclist);
     continue;
    end;
   end;
   mtx_unlock(@Giant);
  end;
  if (syncer_state=SYNCER_FINAL_DELAY and syncer_final_iter > 0)
   syncer_final_iter--;
  {
   * The variable rushjob allows the kernel to speed up the
   * processing of the filesystem syncer process. A rushjob
   * value of N tells the filesystem syncer to process the next
   * N seconds worth of work on its queue ASAP. Currently rushjob
   * is used by the soft update code to speed up the filesystem
   * syncer process when the incore state is getting so far
   * ahead of the disk that the kernel memory pool is being
   * threatened with exhaustion.
   }
  if (rushjob > 0) begin
   rushjob -= 1;
   continue;
  end;
  {
   * Just sleep for a short period of time between
   * iterations when shutting down to allow some I/O
   * to happen.
   *
   * If it has taken us less than a second to process the
   * current work, then wait. Otherwise start right over
   * again. We can still lose time if any single round
   * takes more than two seconds, but it does not really
   * matter as we are just trying to generally pace the
   * filesystem activity.
   }
  if (syncer_state<>SYNCER_RUNNING or
      time_uptime=starttime) begin
   thread_lock(td);
   sched_prio(td, PPAUSE);
   thread_unlock(td);
  end;
  if (syncer_state<>SYNCER_RUNNING)
   cv_timedwait(@sync_wakeup, &sync_mtx,
       hz div SYNCER_SHUTDOWN_SPEEDUP);
  else if (time_uptime=starttime)
   cv_timedwait(@sync_wakeup, &sync_mtx, hz);
 end;
end;

{
 * Request the syncer daemon to speed up its work.
 * We never push it to speed up more than half of its
 * normal turn time, otherwise it could take over the cpu.
 }
int
speedup_syncer(void)
begin
 int ret:=0;

 mtx_lock(@sync_mtx);
 if (rushjob < syncdelay div 2) begin
  rushjob += 1;
  stat_rush_requests += 1;
  ret:=1;
 end;
 mtx_unlock(@sync_mtx);
 cv_broadcast(@sync_wakeup);
 Exit(ret);
end;

{
 * Tell the syncer to speed up its work and run though its work
 * list several times, then tell it to shut down.
 }
static void
syncer_shutdown(void *arg, int howto)
begin

 if (howto and RB_NOSYNC)
  Exit;
 mtx_lock(@sync_mtx);
 syncer_state:=SYNCER_SHUTTING_DOWN;
 rushjob:=0;
 mtx_unlock(@sync_mtx);
 cv_broadcast(@sync_wakeup);
 kproc_shutdown(arg, howto);
end;

{
 * Reassign a buffer from one vnode to another.
 * Used to assign file specific control information
 * (indirect blocks) to the vnode to which they belong.
 }
void
reassignbuf(struct buf *bp)
begin
 vp:p_vnode;
 struct bufobj *bo;
 int delay;
#ifdef INVARIANTS
 struct bufv *bv;
#endif

 vp:=bp^.b_vp;
 bo:=bp^.b_bufobj;
 ++reassignbufcalls;

 CTR3(KTR_BUF, "reassignbuf(%p) vp %p flags %X",
     bp, bp^.b_vp, bp^.b_flags);
 {
  * B_PAGING flagged buffers cannot be reassigned because their vp
  * is not fully linked in.
  }
 if (bp^.b_flags and B_PAGING)
  panic'cannot reassign paging buffer';

 {
  * Delete from old vnode list, if on one.
  }
 BO_LOCK(bo);
 if (bp^.b_xflags and (BX_VNDIRTY or BX_VNCLEAN))
  buf_vlist_remove(bp);
 else
  panic'reassignbuf: Buffer %p not on queue.", bp);
 {
  * If dirty, put on list of dirty buffers; otherwise insert onto list
  * of clean buffers.
  }
 if (bp^.b_flags and B_DELWRI) begin
  if ((bo^.bo_flag and BO_ONWORKLST)=0) begin
   switch (vp^.v_type) begin
   case VDIR:
    delay:=dirdelay;
    break;
   case VCHR:
    delay:=metadelay;
    break;
   default:
    delay:=filedelay;
   end;
   vn_syncer_add_to_worklist(bo, delay);
  end;
  buf_vlist_add(bp, bo, BX_VNDIRTY);
 end; else begin
  buf_vlist_add(bp, bo, BX_VNCLEAN);

  if ((bo^.bo_flag and BO_ONWORKLST) and bo^.bo_dirty.bv_cnt=0) begin
   mtx_lock(@sync_mtx);
   LIST_REMOVE(bo, bo_synclist);
   syncer_worklist_len--;
   mtx_unlock(@sync_mtx);
   bo^.bo_flag:= and ~BO_ONWORKLST;
  end;
 end;
#ifdef INVARIANTS
 bv:=@bo^.bo_clean;
 bp:=TAILQ_FIRST(@bv^.bv_hd);
 Assert(bp=nil or bp^.b_bufobj=bo,
     'bp %p wrong b_bufobj %p should be %p", bp, bp^.b_bufobj, bo));
 bp:=TAILQ_LAST(@bv^.bv_hd, buflists);
 Assert(bp=nil or bp^.b_bufobj=bo,
     'bp %p wrong b_bufobj %p should be %p", bp, bp^.b_bufobj, bo));
 bv:=@bo^.bo_dirty;
 bp:=TAILQ_FIRST(@bv^.bv_hd);
 Assert(bp=nil or bp^.b_bufobj=bo,
     'bp %p wrong b_bufobj %p should be %p", bp, bp^.b_bufobj, bo));
 bp:=TAILQ_LAST(@bv^.bv_hd, buflists);
 Assert(bp=nil or bp^.b_bufobj=bo,
     'bp %p wrong b_bufobj %p should be %p", bp, bp^.b_bufobj, bo));
#endif
 BO_UNLOCK(bo);
end;
}

{
 * Increment the use and hold counts on the vnode, taking care to reference
 * the driver's usecount if this is a chardev.  The vholdl() will remove
 * the vnode from the free list if it is presently free.  Requires the
 * vnode interlock and returns with it held.
 }
procedure v_incr_usecount(vp:p_vnode);
begin
 Inc(vp^.v_usecount);
 if (vp^.v_type=VCHR) and (vp^.v_rdev<>nil) then
 begin
  dev_lock();
  Inc(p_cdev(vp^.v_rdev)^.si_usecount);
  dev_unlock();
 end;
 vholdl(vp);
end;

{
 * Turn a holdcnt into a use+holdcnt such that only one call to
 * v_decr_usecount is needed.
 }
procedure v_upgrade_usecount(vp:p_vnode);
begin
 Inc(vp^.v_usecount);
 if (vp^.v_type=VCHR) and (vp^.v_rdev<>nil) then
 begin
  dev_lock();
  Inc(p_cdev(vp^.v_rdev)^.si_usecount);
  dev_unlock();
 end;
end;

{
 * Decrement the vnode use and hold count along with the driver's usecount
 * if this is a chardev.  The vdropl() below releases the vnode interlock
 * as it may free the vnode.
 }
procedure v_decr_usecount(vp:p_vnode);
begin
 ASSERT_VI_LOCKED(vp,{$I %LINE%});
 Assert(vp^.v_usecount>0,'v_decr_usecount: negative usecount');
 Dec(vp^.v_usecount);
 if (vp^.v_type=VCHR) and (vp^.v_rdev<>nil) then
 begin
  dev_lock();
  Inc(p_cdev(vp^.v_rdev)^.si_usecount);
  dev_unlock();
 end;
 vdropl(vp);
end;

{
 * Decrement only the use count and driver use count.  This is intended to
 * be paired with a follow on vdropl() to release the remaining hold count.
 * In this way we may vgone() a vnode with a 0 usecount without risk of
 * having it end up on a free list because the hold count is kept above 0.
 }
procedure v_decr_useonly(vp:p_vnode);
begin
 ASSERT_VI_LOCKED(vp,{$I %LINE%});
 Assert(vp^.v_usecount>0,'v_decr_useonly: negative usecount');
 Dec(vp^.v_usecount);
 if (vp^.v_type=VCHR) and (vp^.v_rdev<>nil) then
 begin
  dev_lock();
  Dec(p_cdev(vp^.v_rdev)^.si_usecount);
  dev_unlock();
 end;
end;

{
 * Grab a particular vnode from the free list, increment its
 * reference count and lock it.  VI_DOOMED is set if the vnode
 * is being destroyed.  Only callers who specify LK_RETRY will
 * see doomed vnodes.  If inactive processing was delayed in
 * vput try to do it here.
 }
function vget(vp:p_vnode;flags:Integer):Integer;
var
 error:Integer;
begin
 error:=0;
 VFS_ASSERT_GIANT(vp^.v_mount);
 Assert((flags and LK_TYPE_MASK)<>0,'vget: invalid lock operation');

 if ((flags and LK_INTERLOCK)=0) then
 begin
  VI_LOCK(vp);
 end;

 vholdl(vp);

 error:=vn_lock(vp,flags or LK_INTERLOCK);
 if (error<>0) then
 begin
  vdrop(vp);
  Exit(error);
 end;

 if ((vp^.v_iflag and VI_DOOMED)<>0) and ((flags and LK_RETRY)=0) then
  Assert(false,'vget: vn_lock failed to Exit ENOENT');

 VI_LOCK(vp);
 { Upgrade our holdcnt to a usecount. }
 v_upgrade_usecount(vp);
 {
  * We don't guarantee that any particular close will
  * trigger inactive processing so just make a best effort
  * here at preventing a reference to a removed file.  If
  * we don't succeed no harm is done.
  }
 if ((vp^.v_iflag and VI_OWEINACT)<>0) then
 begin
  if (VOP_ISLOCKED(vp)=LK_EXCLUSIVE) and
     ((flags and LK_NOWAIT)=0) then
  begin
   vinactive(vp);
  end;

  vp^.v_iflag:=vp^.v_iflag and (not VI_OWEINACT);
 end;
 VI_UNLOCK(vp);
 Exit(0);
end;

{
 * Increase the reference count of a vnode.
 }
procedure vref(vp:p_vnode);
begin
 VI_LOCK(vp);
 v_incr_usecount(vp);
 VI_UNLOCK(vp);
end;

{
 * Exitreference count of a vnode.
 *
 * The results of this call are only guaranteed when some mechanism other
 * than the VI lock is used to stop other processes from gaining references
 * to the vnode.  This may be the case if the caller holds the only reference.
 * This is also useful when stale data is acceptable as race conditions may
 * be accounted for by some other means.
 }
function vrefcnt(vp:p_vnode):Integer;
begin
 VI_LOCK(vp);
 Result:=vp^.v_usecount;
 VI_UNLOCK(vp);
end;

const
 VPUTX_VRELE =1;
 VPUTX_VPUT  =2;
 VPUTX_VUNREF=3;

procedure vputx(vp:p_vnode;func:Integer);
var
 error:Integer;
begin
 Assert(vp<>nil,'vputx: nil vp');
 if (func=VPUTX_VUNREF) then
  ASSERT_VOP_LOCKED(vp,'vunref')
 else
 if (func=VPUTX_VPUT) then
  ASSERT_VOP_LOCKED(vp,'vput')
 else
  Assert(func=VPUTX_VRELE,'vputx: wrong func');

 VFS_ASSERT_GIANT(vp^.v_mount);

 VI_LOCK(vp);

 { Skip this v_writecount check if we're going to panic below. }
 Assert((vp^.v_writecount < vp^.v_usecount) or (vp^.v_usecount < 1),'vputx: missed vn_close');
 error:=0;

 if (vp^.v_usecount > 1) or (((vp^.v_iflag and VI_DOINGINACT)<>0) and (vp^.v_usecount=1)) then
 begin
  if (func=VPUTX_VPUT) then
  begin
   VOP_UNLOCK(vp, 0);
  end;
  v_decr_usecount(vp);
  Exit;
 end;

 if (vp^.v_usecount<>1) then
 begin
  Assert(false,'vputx: negative ref cnt');
 end;
 {
  * We want to hold the vnode until the inactive finishes to
  * prevent vgone() races.  We drop the use count here and the
  * hold count below when we're done.
  }
 v_decr_useonly(vp);
 {
  * We must call VOP_INACTIVE with the node locked. Mark
  * as VI_DOINGINACT to avoid recursion.
  }
 vp^.v_iflag:=vp^.v_iflag or VI_OWEINACT;

 case (func) of
  VPUTX_VRELE:
  begin
   error:=vn_lock(vp, LK_EXCLUSIVE or LK_INTERLOCK);
   VI_LOCK(vp);
  end;
  VPUTX_VPUT:
  begin
   if (VOP_ISLOCKED(vp)<>LK_EXCLUSIVE) then
   begin
    error:=VOP_LOCK(vp, LK_UPGRADE or LK_INTERLOCK or LK_NOWAIT,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
    VI_LOCK(vp);
   end;
  end;
 VPUTX_VUNREF:
  begin
   if (VOP_ISLOCKED(vp)<>LK_EXCLUSIVE) then
   begin
    error:=VOP_LOCK(vp, LK_TRYUPGRADE or LK_INTERLOCK,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
    VI_LOCK(vp);
   end;
  end;
 end;

 if (vp^.v_usecount > 0) then
 begin
  vp^.v_iflag:=vp^.v_iflag and (not VI_OWEINACT);
 end;

 if (error=0) then
 begin
  if ((vp^.v_iflag and VI_OWEINACT)<>0) then
  begin
   vinactive(vp);
  end;

  if (func<>VPUTX_VUNREF) then
  begin
   VOP_UNLOCK(vp, 0);
  end;
 end;
 vdropl(vp);
end;

{
 * Vnode put/release.
 * If count drops to zero, call inactive routine and return to freelist.
 }
procedure vrele(vp:p_vnode);
begin
 vputx(vp, VPUTX_VRELE);
end;

{
 * Release an already locked vnode.  This give the same effects as
 * unlock+vrele(), but takes less time and avoids releasing and
 * re-aquiring the lock (as vrele() acquires the lock internally.)
 }
procedure vput(vp:p_vnode);
begin
 vputx(vp, VPUTX_VPUT);
end;

{
 * Release an exclusively locked vnode. Do not unlock the vnode lock.
 }
procedure vunref(vp:p_vnode);
begin
 vputx(vp, VPUTX_VUNREF);
end;

{
 * Somebody doesn't want the vnode recycled.
 }
procedure vhold(vp:p_vnode);
begin
 VI_LOCK(vp);
 vholdl(vp);
 VI_UNLOCK(vp);
end;

{
 * Increase the hold count and activate if this is the first reference.
 }
procedure vholdl(vp:p_vnode);
var
 mp:p_mount;
begin
 Inc(vp^.v_holdcnt);
 if (not VSHOULDBUSY(vp)) then
  Exit;
 ASSERT_VI_LOCKED(vp,'vholdl');
 Assert((vp^.v_iflag and VI_FREE)<>0,'vnode not free');
 Assert(vp^.v_op<>nil,'vholdl: vnode already reclaimed.');
 {
  * Remove a vnode from the free list, mark it as in use,
  * and put it on the active list.
  }
 mtx_lock(vnode_free_list_mtx);
 TAILQ_REMOVE(@vnode_free_list,vp,@vp^.v_actfreelist);
 Dec(freevnodes);
 vp^.v_iflag:=vp^.v_iflag and (not (VI_FREE or VI_AGE));
 Assert((vp^.v_iflag and VI_ACTIVE)=0,'Activating already active vnode');
 vp^.v_iflag:=vp^.v_iflag or VI_ACTIVE;
 mp:=vp^.v_mount;
 TAILQ_INSERT_HEAD(@mp^.mnt_activevnodelist,vp,@vp^.v_actfreelist);
 Inc(mp^.mnt_activevnodelistsize);
 mtx_unlock(vnode_free_list_mtx);
end;

{
 * Note that there is one less who cares about this vnode.
 * vdrop() is the opposite of vhold().
 }
procedure vdrop(vp:p_vnode);
begin
 VI_LOCK(vp);
 vdropl(vp);
end;

{
 * Drop the hold count of the vnode.  If this is the last reference to
 * the vnode we place it on the free list unless it has been vgone'd
 * (marked VI_DOOMED) in which case we will free it.
 }
procedure vdropl(vp:p_vnode);
var
 //struct bufobj *bo;
 mp:p_mount;
 active:Integer;
begin
 ASSERT_VI_LOCKED(vp,'vdropl');

 if (vp^.v_holdcnt <= 0) then
 begin
  Assert(false,'vdrop: holdcnt');
 end;

 Dec(vp^.v_holdcnt);
 if (vp^.v_holdcnt > 0) then
 begin
  VI_UNLOCK(vp);
  Exit;
 end;

 if ((vp^.v_iflag and VI_DOOMED)=0) then
 begin
  {
   * Mark a vnode as free: remove it from its active list
   * and put it up for recycling on the freelist.
   }
  Assert(vp^.v_op<>nil,'vdropl: vnode already reclaimed.');
  Assert((vp^.v_iflag and VI_FREE)=0,'vnode already free');
  Assert(VSHOULDFREE(vp),'vdropl: freeing when we shouldnt');

  active:=vp^.v_iflag and VI_ACTIVE;
  vp^.v_iflag:=vp^.v_iflag and (not VI_ACTIVE);
  mp:=vp^.v_mount;
  mtx_lock(vnode_free_list_mtx);
  if (active<>0) then
  begin
   TAILQ_REMOVE(@mp^.mnt_activevnodelist,vp,@vp^.v_actfreelist);
   Dec(mp^.mnt_activevnodelistsize);
  end;
  if ((vp^.v_iflag and VI_AGE)<>0) then
  begin
   TAILQ_INSERT_HEAD(@vnode_free_list,vp,@vp^.v_actfreelist);
  end else
  begin
   TAILQ_INSERT_TAIL(@vnode_free_list,vp,@vp^.v_actfreelist);
  end;
  Inc(freevnodes);
  vp^.v_iflag:=vp^.v_iflag and (not VI_AGE);
  vp^.v_iflag:=vp^.v_iflag or VI_FREE;
  mtx_unlock(vnode_free_list_mtx);
  VI_UNLOCK(vp);
  Exit;
 end;

 {
  * The vnode has been marked for destruction, so free it.
  }
 System.InterlockedDecrement64(numvnodes);
 //bo:=@vp^.v_bufobj;
 Assert((vp^.v_iflag and VI_FREE)=0,'cleaned vnode still on the free list.');
 Assert(vp^.v_data=nil, 'cleaned vnode isnt');
 Assert(vp^.v_holdcnt=0, 'Non-zero hold count');
 Assert(vp^.v_usecount=0, 'Non-zero use count');
 Assert(vp^.v_writecount=0, 'Non-zero write count');
 //Assert(bo^.bo_numoutput=0, 'Clean vnode has pending I/Os');
 //Assert(bo^.bo_clean.bv_cnt=0, 'cleanbufcnt not 0');
 //Assert(bo^.bo_clean.bv_root=nil, 'cleanblkroot not nil');
 //Assert(bo^.bo_dirty.bv_cnt=0, 'dirtybufcnt not 0');
 //Assert(bo^.bo_dirty.bv_root=nil, 'dirtyblkroot not nil');
 //Assert(TAILQ_EMPTY(@vp^.v_cache_dst), 'vp has namecache dst');
 //Assert(LIST_EMPTY(@vp^.v_cache_src), 'vp has namecache src');
 //Assert(vp^.v_cache_dd=nil, 'vp has namecache for ..');
 VI_UNLOCK(vp);

 //mac_vnode_destroy(vp);

 if (vp^.v_pollinfo<>nil) then
 begin
  destroy_vpollinfo(vp^.v_pollinfo);
 end;

 { XXX Elsewhere we detect an already freed vnode via nil v_op. }
 vp^.v_op:=nil;

 //rangelock_destroy(@vp^.v_rl);
 //lockdestroy(vp^.v_vnlock);
 mtx_destroy(vp^.v_vnlock^);
 mtx_destroy(vp^.v_interlock);

 mtx_destroy(vp^.v_lock);

 //mtx_destroy(BO_MTX(bo));

 FreeMem(vp);
end;

{
 * Call VOP_INACTIVE on the vnode and manage the DOINGINACT and OWEINACT
 * flags.  DOINGINACT prevents us from recursing in calls to vinactive.
 * OWEINACT tracks whether a vnode missed a call to inactive due to a
 * failed lock upgrade.
 }
procedure vinactive(vp:p_vnode);
var
 obj:vm_object_t;
begin
 ASSERT_VOP_ELOCKED(vp,'vinactive');
 ASSERT_VI_LOCKED(vp,'vinactive');
 Assert((vp^.v_iflag and VI_DOINGINACT)=0,'vinactive: recursed on VI_DOINGINACT');

 vp^.v_iflag:=vp^.v_iflag or VI_DOINGINACT;
 vp^.v_iflag:=vp^.v_iflag and (not VI_OWEINACT);
 VI_UNLOCK(vp);
 {
  * Before moving off the active list, we must be sure that any
  * modified pages are on the vnode's dirty list since these will
  * no longer be checked once the vnode is on the inactive list.
  * Because the vnode vm object keeps a hold reference on the vnode
  * if there is at least one resident non-cached page, the vnode
  * cannot leave the active list without the page cleanup done.
  }
 obj:=vp^.v_object;
 if (obj<>nil) then
 if ((obj^.flags and OBJ_MIGHTBEDIRTY)<>0) then
 begin
  VM_OBJECT_LOCK(obj);
  vm_object_page_clean(obj, 0, 0, OBJPC_NOSYNC);
  VM_OBJECT_UNLOCK(obj);
 end;
 VOP_INACTIVE(vp);
 VI_LOCK(vp);
 Assert((vp^.v_iflag and VI_DOINGINACT)<>0,'vinactive: lost VI_DOINGINACT');
 vp^.v_iflag:=vp^.v_iflag and (not VI_DOINGINACT);
end;

{
 * Remove any vnodes in the vnode table belonging to mount point mp.
 *
 * If FORCECLOSE is not specified, there should not be any active ones,
 * Exiterror if any are found (nb: this is a user error, not a
 * system error). If FORCECLOSE is specified, detach any active vnodes
 * that are found.
 *
 * If WRITECLOSE is set, only flush out regular file vnodes open for
 * writing.
 *
 * SKIPSYSTEM causes any vnodes marked VV_SYSTEM to be skipped.
 *
 * `rootrefs' specifies the base reference count for the root vnode
 * of this filesystem. The root vnode is considered busy if its
 * v_usecount exceeds this value. On a successful return, vflush(, td)
 * will call vrele() on the root vnode exactly rootrefs times.
 * If the SKIPSYSTEM or WRITECLOSE flags are specified, rootrefs must
 * be zero.
 }
function vflush(mp:p_mount;rootrefs,flags:Integer):Integer;
label
 loop;
var
 vp,mvp,rootvp:p_vnode;
 vattr:t_vattr;
 busy,error:Integer;
begin
 rootvp:=nil;
 busy:=0;

 if (rootrefs > 0) then
 begin
  Assert((flags and (SKIPSYSTEM or WRITECLOSE))=0,'vflush: bad args');
  {
   * Get the filesystem root vnode. We can vput() it
   * immediately, since with rootrefs > 0, it won't go away.
   }
  error:=VFS_ROOT(mp, LK_EXCLUSIVE, @rootvp);
  if (error<>0) then
  begin
   Exit(error);
  end;
  vput(rootvp);
 end;
loop:
 vp:=__mnt_vnode_first_all(@mvp,mp);
 while (vp<>nil) do
 begin
  vholdl(vp);
  error:=vn_lock(vp, LK_INTERLOCK or LK_EXCLUSIVE);
  if (error<>0) then
  begin
   vdrop(vp);
   //MNT_VNODE_FOREACH_ALL_ABORT
   MNT_ILOCK(mp);
   __mnt_vnode_markerfree_all(@mvp,mp);
   //MNT_VNODE_FOREACH_ALL_ABORT
   goto loop;
  end;
  {
   * Skip over a vnodes marked VV_SYSTEM.
   }
  if (((flags and SKIPSYSTEM)<>0) and ((vp^.v_vflag and VV_SYSTEM)<>0)) then
  begin
   VOP_UNLOCK(vp, 0);
   vdrop(vp);
   //
   vp:=__mnt_vnode_next_all(@mvp,mp);
   continue;
  end;
  {
   * If WRITECLOSE is set, flush out unlinked but still open
   * files (even if open only for reading) and regular file
   * vnodes open for writing.
   }
  if ((flags and WRITECLOSE)<>0) then
  begin
   if (vp^.v_object<>nil) then
   begin
    VM_OBJECT_LOCK(vp^.v_object);
    vm_object_page_clean(vp^.v_object, 0, 0, 0);
    VM_OBJECT_UNLOCK(vp^.v_object);
   end;
   error:=VOP_FSYNC(vp, MNT_WAIT);
   if (error<>0) then
   begin
    VOP_UNLOCK(vp, 0);
    vdrop(vp);
    //MNT_VNODE_FOREACH_ALL_ABORT
    MNT_ILOCK(mp);
    __mnt_vnode_markerfree_all(@mvp,mp);
    //MNT_VNODE_FOREACH_ALL_ABORT
    Exit(error);
   end;
   error:=VOP_GETATTR(vp, @vattr);
   VI_LOCK(vp);

   if ((vp^.v_type=VNON) or
       ((error=0) and (vattr.va_nlink > 0))) and
      ((vp^.v_writecount=0) or (vp^.v_type<>VREG)) then
   begin
    VOP_UNLOCK(vp, 0);
    vdropl(vp);
    //
    vp:=__mnt_vnode_next_all(@mvp,mp);
    continue;
   end;
  end else
   VI_LOCK(vp);
  {
   * With v_usecount=0, all we need to do is clear out the
   * vnode data structures and we are done.
   *
   * If FORCECLOSE is set, forcibly close the vnode.
   }
  if (vp^.v_usecount=0 or (flags and FORCECLOSE)) then
  begin
   Assert((vp^.v_usecount=0) or
          ((vp^.v_type<>VCHR) and (vp^.v_type<>VBLK)),'device VNODE %p is FORCECLOSED');
   vgonel(vp);
  end else
  begin
   Inc(busy);
  end;
  VOP_UNLOCK(vp, 0);
  vdropl(vp);
  //
  vp:=__mnt_vnode_next_all(@mvp,mp);
 end;
 if (rootrefs > 0) and ((flags and FORCECLOSE)=0) then
 begin
  {
   * If just the root vnode is busy, and if its refcount
   * is equal to `rootrefs', then go ahead and kill it.
   }
  VI_LOCK(rootvp);
  Assert(busy > 0, 'vflush: not busy');
  Assert(rootvp^.v_usecount >= rootrefs,'vflush: usecount %d < rootrefs %d');
  if (busy=1) and (rootvp^.v_usecount=rootrefs) then
  begin
   VOP_LOCK(rootvp, LK_EXCLUSIVE or LK_INTERLOCK,{$INCLUDE %FILE%},{$INCLUDE %LINENUM%});
   vgone(rootvp);
   VOP_UNLOCK(rootvp, 0);
   busy:=0;
  end else
   VI_UNLOCK(rootvp);
 end;
 if (busy<>0) then
 begin
  Exit(EBUSY);
 end;

 while (rootrefs > 0) do
 begin
  vrele(rootvp);
  Dec(rootrefs);
 end;
 Exit(0);
end;

{
 * Recycle an unused vnode to the front of the free list.
 }
function vrecycle(vp:p_vnode):Integer;
var
 recycled:Integer;
begin
 ASSERT_VOP_ELOCKED(vp, 'vrecycle');
 recycled:=0;
 VI_LOCK(vp);
 if (vp^.v_usecount=0)then
 begin
  recycled:=1;
  vgonel(vp);
 end;
 VI_UNLOCK(vp);
 Exit(recycled);
end;

{
 * Eliminate all activity associated with a vnode
 * in preparation for reuse.
 }
procedure vgone(vp:p_vnode);
begin
 VI_LOCK(vp);
 vgonel(vp);
 VI_UNLOCK(vp);
end;

{
 * vgone, with the vp interlock held.
 }
procedure vgonel(vp:p_vnode);
var
 oweinact:Integer;
 active:Integer;
 mp:p_mount;
begin
 ASSERT_VOP_ELOCKED(vp, 'vgonel');
 ASSERT_VI_LOCKED(vp, 'vgonel');
 Assert(vp^.v_holdcnt<>0,'vgonel: vp %p has no reference.');

 {
  * Don't vgonel if we're already doomed.
  }
 if ((vp^.v_iflag and VI_DOOMED)<>0) then Exit;

 vp^.v_iflag:=vp^.v_iflag or VI_DOOMED;

 {
  * Check to see if the vnode is in use.  If so, we have to call
  * VOP_CLOSE() and VOP_INACTIVE().
  }
 active:=vp^.v_usecount;
 oweinact:=(vp^.v_iflag and VI_OWEINACT);
 VI_UNLOCK(vp);

 {
  * Clean out any buffers associated with the vnode.
  * If the flush fails, just toss the buffers.
  }
 mp:=nil;

 //if (not TAILQ_EMPTY(@vp^.v_bufobj.bo_dirty.bv_hd)) then
 // vn_start_secondary_write(vp, &mp, V_WAIT);

 if (vinvalbuf(vp, V_SAVE, 0, 0)<>0) then
 begin
  vinvalbuf(vp, 0, 0, 0);
 end;

 {
  * If purging an active vnode, it must be closed and
  * deactivated before being reclaimed.
  }
 if (active<>0) then
 begin
  VOP_CLOSE(vp, FNONBLOCK);
 end;

 if (oweinact<>0) or (active<>0) then
 begin
  VI_LOCK(vp);

  if ((vp^.v_iflag and VI_DOINGINACT)=0) then
  begin
   vinactive(vp);
  end;

  VI_UNLOCK(vp);
 end;

 //if (vp^.v_type=VSOCK) then
 // vfs_unp_reclaim(vp);

 {
  * Reclaim the vnode.
  }
 if (VOP_RECLAIM(vp)<>0) then
 begin
  Assert(false,'vgone: cannot reclaim');
 end;

 //if (mp<>nil) then
 // vn_finished_secondary_write(mp);

 //Assert(vp^.v_object=nil,'vop_reclaim left v_object vp=%p, tag=%s'));

 {
  * Clear the advisory locks and wake up waiting threads.
  }
 VOP_ADVLOCKPURGE(vp);
 {
  * Delete from old mount point vnode list.
  }
 delmntque(vp);
 //cache_purge(vp);
 {
  * Done with purge, reset to the standard lock and invalidate
  * the vnode.
  }
 VI_LOCK(vp);
 vp^.v_vnlock:=@vp^.v_lock;
 vp^.v_op    :=@dead_vnodeops;
 vp^.v_tag   :='none';
 vp^.v_type  :=VBAD;
end;

{
 * Calculate the total number of references to a special device.
 }
function vcount(vp:p_vnode):Integer;
begin
 dev_lock();
 Result:=p_cdev(vp^.v_rdev)^.si_usecount;
 dev_unlock();
end;

{
 * Same as above, but using the struct cdev *as argument
 }
function count_dev(dev:Pointer):Integer; //cdev
begin
 dev_lock();
 Result:=p_cdev(dev)^.si_usecount;
 dev_unlock();
end;

{
 * perform msync on all vnodes under a mount point
 * the mount point must be locked.
 }
procedure vfs_msync(mp:p_mount;flags:Integer);
var
 vp,mvp:p_vnode;
 obj:vm_object_t;
begin
 vp:=__mnt_vnode_first_active(@mvp,mp);
 While (vp<>nil) do
 begin
  obj:=vp^.v_object;
  if (obj<>nil) and
     {((obj^.flags and OBJ_MIGHTBEDIRTY)<>0)} False and
     ((flags=MNT_WAIT) or (VOP_ISLOCKED(vp)=0)) then
  begin
   if (vget(vp, LK_EXCLUSIVE or LK_RETRY or LK_INTERLOCK)=0) then
   begin
    if ((vp^.v_vflag and VV_NOSYNC)<>0) then
    begin { unlinked }
     vput(vp);
     //
     vp:=__mnt_vnode_next_active(@mvp,mp);
     continue;
    end;

    obj:=vp^.v_object;
    if (obj<>nil) then
    begin
     VM_OBJECT_LOCK(obj);
     if (flags=MNT_WAIT) then
     begin
      vm_object_page_clean(obj, 0, 0, OBJPC_SYNC);
     end else
     begin
      vm_object_page_clean(obj, 0, 0, OBJPC_NOSYNC);
     end;
     VM_OBJECT_UNLOCK(obj);
    end;
    vput(vp);
   end;
  end else
  begin
   VI_UNLOCK(vp);
  end;
  //
  vp:=__mnt_vnode_next_active(@mvp,mp);
 end;
end;

procedure destroy_vpollinfo_free(vi:p_vpollinfo);
begin
 knlist_destroy(@vi^.vpi_selinfo.si_note);
 mtx_destroy(vi^.vpi_lock);
 FreeMem(vi);
end;

procedure destroy_vpollinfo(vi:p_vpollinfo);
begin
 knlist_clear(@vi^.vpi_selinfo.si_note, 1);
 seldrain(@vi^.vpi_selinfo);
 destroy_vpollinfo_free(vi);
end;

procedure vfs_knllock(arg:Pointer);             forward;
procedure vfs_knlunlock(arg:Pointer);           forward;
procedure vfs_knl_assert_locked(arg:Pointer);   forward;
procedure vfs_knl_assert_unlocked(arg:Pointer); forward;

{
 * Initalize per-vnode helper structure to hold poll-related state.
 }
procedure v_addpollinfo(vp:p_vnode);
var
 vi:p_vpollinfo;
begin
 if (vp^.v_pollinfo<>nil) then
  Exit;
 vi:=AllocMem(SizeOf(vpollinfo));
 mtx_init(vi^.vpi_lock,'vnode pollinfo');
 knlist_init(@vi^.vpi_selinfo.si_note, vp, @vfs_knllock, @vfs_knlunlock, @vfs_knl_assert_locked, @vfs_knl_assert_unlocked);
 VI_LOCK(vp);
 if (vp^.v_pollinfo<>nil) then
 begin
  VI_UNLOCK(vp);
  destroy_vpollinfo_free(vi);
  Exit;
 end;
 vp^.v_pollinfo:=vi;
 VI_UNLOCK(vp);
end;

{
 * Record a process's interest in events which might happen to
 * a vnode.  Because poll uses the historic select-style interface
 * internally, this routine serves as both the ``check for any
 * pending events'' and the ``record my interest in future events''
 * functions.  (These are done together, while the lock is held,
 * to avoid race conditions.)
 }
function vn_pollrecord(vp:p_vnode;events:Integer):Integer;
begin
 v_addpollinfo(vp);
 mtx_lock(vp^.v_pollinfo^.vpi_lock);
 if ((vp^.v_pollinfo^.vpi_revents and events)<>0) then
 begin
  {
   * This leaves events we are not interested
   * in available for the other process which
   * which presumably had requested them
   * (otherwise they would never have been
   * recorded).
   }
  events:=events and vp^.v_pollinfo^.vpi_revents;
  vp^.v_pollinfo^.vpi_revents:=vp^.v_pollinfo^.vpi_revents and (not events);

  mtx_unlock(vp^.v_pollinfo^.vpi_lock);
  Exit(events);
 end;
 vp^.v_pollinfo^.vpi_events:=vp^.v_pollinfo^.vpi_events or events;
 selrecord(curkthread, @vp^.v_pollinfo^.vpi_selinfo);
 mtx_unlock(vp^.v_pollinfo^.vpi_lock);
 Exit(0);
end;

{
 * Routine to create and manage a filesystem syncer vnode.
 }

{
#define sync_close ((int (*)(struct  vop_close_args *))nilop)
static int sync_fsync(struct  vop_fsync_args *);
static int sync_inactive(struct  vop_inactive_args *);
static int sync_reclaim(struct  vop_reclaim_args *);

static struct vop_vector sync_vnodeops:=begin
 .vop_bypass:=VOP_EOPNOTSUPP,
 .vop_close:=sync_close,  { close }
 .vop_fsync:=sync_fsync,  { fsync }
 .vop_inactive:=sync_inactive, { inactive }
 .vop_reclaim:=sync_reclaim, { reclaim }
 .vop_lock1:=vop_stdlock, { lock }
 .vop_unlock:=vop_stdunlock, { unlock }
 .vop_islocked:=vop_stdislocked, { islocked }
end;

{
 * Create a new filesystem syncer vnode for the specified mount point.
 }
void
vfs_allocate_syncvnode(mp:p_mount)
begin
 vp:p_vnode;
 struct bufobj *bo;
 static long start, incr, next;
 int error;

 { Allocate a new vnode }
 error:=getnewvnode'syncer", mp, &sync_vnodeops, &vp);
 if (error<>0)
  panic'vfs_allocate_syncvnode: getnewvnode() failed';
 vp^.v_type:=VNON;
 vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
 vp^.v_vflag:= or VV_FORCEINSMQ;
 error:=insmntque(vp, mp);
 if (error<>0)
  panic'vfs_allocate_syncvnode: insmntque() failed';
 vp^.v_vflag:= and ~VV_FORCEINSMQ;
 VOP_UNLOCK(vp, 0);
 {
  * Place the vnode onto the syncer worklist. We attempt to
  * scatter them about on the list so that they will go off
  * at evenly distributed times even if all the filesystems
  * are mounted at once.
  }
 next += incr;
 if (next=0 or next > syncer_maxdelay) begin
  start /= 2;
  incr /= 2;
  if (start=0) begin
   start:=syncer_maxdelay div 2;
   incr:=syncer_maxdelay;
  end;
  next:=start;
 end;
 bo:=@vp^.v_bufobj;
 BO_LOCK(bo);
 vn_syncer_add_to_worklist(bo, syncdelay > 0 ? next % syncdelay : 0);
 { XXX - vn_syncer_add_to_worklist() also grabs and drops sync_mtx. }
 mtx_lock(@sync_mtx);
 sync_vnode_count++;
 if (mp^.mnt_syncer=nil) begin
  mp^.mnt_syncer:=vp;
  vp:=nil;
 end;
 mtx_unlock(@sync_mtx);
 BO_UNLOCK(bo);
 if (vp<>nil) begin
  vn_lock(vp, LK_EXCLUSIVE or LK_RETRY);
  vgone(vp);
  vput(vp);
 end;
end;

void
vfs_deallocate_syncvnode(mp:p_mount)
begin
 vp:p_vnode;

 mtx_lock(@sync_mtx);
 vp:=mp^.mnt_syncer;
 if (vp<>nil)
  mp^.mnt_syncer:=nil;
 mtx_unlock(@sync_mtx);
 if (vp<>nil)
  vrele(vp);
end;

{
 * Do a lazy sync of the filesystem.
 }
static int
sync_fsync(struct vop_fsync_args *ap)
begin
 struct vnode *syncvp:=ap^.a_vp;
 mp:p_mount:=syncvp^.v_mount;
 int error, save;
 struct bufobj *bo;

 {
  * We only need to do something if this is a lazy evaluation.
  }
 if (ap^.a_waitfor<>MNT_LAZY)
  Exit(0);

 {
  * Move ourselves to the back of the sync list.
  }
 bo:=@syncvp^.v_bufobj;
 BO_LOCK(bo);
 vn_syncer_add_to_worklist(bo, syncdelay);
 BO_UNLOCK(bo);

 {
  * Walk the list of vnodes pushing all that are dirty and
  * not already on the sync list.
  }
 if (vfs_busy(mp, MBF_NOWAIT)<>0)
  Exit(0);
 if (vn_start_write(nil, &mp, V_NOWAIT)<>0) begin
  vfs_unbusy(mp);
  Exit(0);
 end;
 save:=curthread_pflags_set(TDP_SYNCIO);
 vfs_msync(mp, MNT_NOWAIT);
 error:=VFS_SYNC(mp, MNT_LAZY);
 curthread_pflags_restore(save);
 vn_finished_write(mp);
 vfs_unbusy(mp);
 Exit(error);
end;

{
 * The syncer vnode is no referenced.
 }
static int
sync_inactive(struct vop_inactive_args *ap)
begin

 vgone(ap^.a_vp);
 Exit(0);
end;

{
 * The syncer vnode is no longer needed and is being decommissioned.
 *
 * Modifications to the worklist must be protected by sync_mtx.
 }
static int
sync_reclaim(struct vop_reclaim_args *ap)
begin
 vp:p_vnode:=ap^.a_vp;
 struct bufobj *bo;

 bo:=@vp^.v_bufobj;
 BO_LOCK(bo);
 mtx_lock(@sync_mtx);
 if (vp^.v_mount^.mnt_syncer=vp)
  vp^.v_mount^.mnt_syncer:=nil;
 if (bo^.bo_flag and BO_ONWORKLST) begin
  LIST_REMOVE(bo, bo_synclist);
  syncer_worklist_len--;
  sync_vnode_count--;
  bo^.bo_flag:= and ~BO_ONWORKLST;
 end;
 mtx_unlock(@sync_mtx);
 BO_UNLOCK(bo);

 Exit(0);
end;
}

{
 * Check if vnode represents a disk device
 }
function vn_isdisk(vp:p_vnode;errp:PInteger):Boolean;
var
 error:Integer;
begin
 error:=0;
 dev_lock();
 if (vp^.v_type<>VCHR) then
  error:=ENOTBLK
 else
 if (vp^.v_rdev=nil) then
  error:=ENXIO
 else
 if (p_cdev(vp^.v_rdev)^.si_devsw=nil) then
  error:=ENXIO
 else{
 if ((p_cdev(vp^.v_rdev)^.si_devsw^.d_flags and D_DISK)=0) then
  error:=ENOTBLK};
 dev_unlock();
 error:=ENOTBLK;
 if (errp<>nil) then
  errp^:=error;
 Exit(error=0);
end;

{
 * Common filesystem object access control check routine.  Accepts a
 * vnode's type, "mode", uid and gid, requested access mode, credentials,
 * and optional call-by-reference privused argument allowing vaccess()
 * to indicate to the caller whether privilege was used to satisfy the
 * request (obsoleted).  Returns 0 on success, or an errno on failure.
 }
function vaccess(_type:vtype;
                 file_mode:mode_t;
                 file_uid:uid_t;
                 file_gid:gid_t;
                 accmode:accmode_t;
                 privused:PInteger):Integer;
label
 privcheck;
var
 dac_granted :accmode_t;
 priv_granted:accmode_t;
begin
 Assert((accmode and (not (VEXEC or VWRITE or VREAD or VADMIN or VAPPEND)))=0,'invalid bit in accmode');
 Assert(((accmode and VAPPEND)=0) or ((accmode and VWRITE)<>0),'VAPPEND without VWRITE');

 {
  * Look for a normal, non-privileged way to access the file/directory
  * as requested.  If it exists, go with that.
  }

 if (privused<>nil) then
  privused^:=0;

 dac_granted:=0;

 { Check the owner. }
 if {(cred^.cr_uid=file_uid)} True then
 begin
  dac_granted:=dac_granted or VADMIN;
  if ((file_mode and S_IXUSR)<>0) then
   dac_granted:=dac_granted or VEXEC;
  if ((file_mode and S_IRUSR)<>0) then
   dac_granted:=dac_granted or VREAD;
  if ((file_mode and S_IWUSR)<>0) then
   dac_granted:=dac_granted or (VWRITE or VAPPEND);

  if ((accmode and dac_granted)=accmode) then
   Exit(0);

  goto privcheck;
 end;

 { Otherwise, check the groups (first match) }
 if {(groupmember(file_gid, cred))} True then
 begin
  if ((file_mode and S_IXGRP)<>0) then
   dac_granted:=dac_granted or VEXEC;
  if ((file_mode and S_IRGRP)<>0) then
   dac_granted:=dac_granted or VREAD;
  if ((file_mode and S_IWGRP)<>0) then
   dac_granted:=dac_granted or (VWRITE or VAPPEND);

  if ((accmode and dac_granted)=accmode) then
   Exit(0);

  goto privcheck;
 end;

 { Otherwise, check everyone else. }
 if ((file_mode and S_IXOTH)<>0) then
  dac_granted:=dac_granted or VEXEC;
 if ((file_mode and S_IROTH)<>0) then
  dac_granted:=dac_granted or VREAD;
 if ((file_mode and S_IWOTH)<>0) then
  dac_granted:=dac_granted or (VWRITE or VAPPEND);

 if ((accmode and dac_granted)=accmode) then
  Exit(0);

privcheck:
 {
  * Build a privilege mask to determine if the set of privileges
  * satisfies the requirements when combined with the granted mask
  * from above.  For each privilege, if the privilege is required,
  * bitwise or the request type onto the priv_granted mask.
  }
 priv_granted:=0;

 if (_type=VDIR) then
 begin
  {
   * For directories, use PRIV_VFS_LOOKUP to satisfy VEXEC
   * requests, instead of PRIV_VFS_EXEC.
   }
  if ((accmode and VEXEC)<>0) and
     ((dac_granted and VEXEC)=0) {and
     (priv_check_cred(cred, PRIV_VFS_LOOKUP, 0)=0)} then
   priv_granted:=priv_granted or VEXEC;
 end else
 begin
  {
   * Ensure that at least one execute bit is on. Otherwise,
   * a privileged user will always succeed, and we don't want
   * this to happen unless the file really is executable.
   }
  if ((accmode and VEXEC)<>0) and
     ((dac_granted and VEXEC)=0) and
     ((file_mode and (S_IXUSR or S_IXGRP or S_IXOTH))<>0) {and
     (priv_check_cred(cred, PRIV_VFS_EXEC, 0)=0)} then
   priv_granted:=priv_granted or VEXEC;
 end;

 if ((accmode and VREAD)<>0) and
    ((dac_granted and VREAD)=0) {and
     (priv_check_cred(cred, PRIV_VFS_READ, 0)=0)} then
  priv_granted:=priv_granted or VREAD;

 if ((accmode and VWRITE)<>0) and
    ((dac_granted and VWRITE)=0) {and
    (priv_check_cred(cred, PRIV_VFS_WRITE, 0)=0)} then
  priv_granted:=priv_granted or (VWRITE or VAPPEND);

 if ((accmode and VADMIN)<>0) and
    ((dac_granted and VADMIN)=0) {and
    (priv_check_cred(cred, PRIV_VFS_ADMIN, 0)=0)} then
  priv_granted:=priv_granted or VADMIN;

 if ((accmode and (priv_granted or dac_granted))=accmode) then
 begin
  { XXX audit: privilege used }
  if (privused<>nil) then
   privused^:=1;
  Exit(0);
 end;

 if ((accmode and VADMIN)<>0) then
  Exit(EPERM)
 else
  Exit(EACCES);
end;

procedure vfs_badlock(msg,str:PChar;vp:p_vnode);
begin
 Writeln(msg,' ',str);
 Assert(false,RawByteString(msg)+' '+RawByteString(str));
end;

procedure assert_vi_locked(vp:p_vnode;str:PChar);
begin
 if {vfs_badlock_mutex and} (not mtx_owned(VI_MTX(vp)^)) then
  vfs_badlock('interlock is not locked but should be', str, vp);
end;

procedure assert_vi_unlocked(vp:p_vnode;str:PChar);
begin
 if {vfs_badlock_mutex and} mtx_owned(VI_MTX(vp)^) then
  vfs_badlock('interlock is locked but should not be', str, vp);
end;

procedure assert_vop_locked(vp:p_vnode;str:PChar);
var
 locked:Integer;
begin
 if (not IGNORE_LOCK(vp)) then
 begin
  locked:=VOP_ISLOCKED(vp);
  if (locked=0) or (locked=LK_EXCLOTHER) then
   vfs_badlock('is not locked but should be', str, vp);
 end;
end;

procedure assert_vop_unlocked(vp:p_vnode;str:PChar);
begin
 if (not IGNORE_LOCK(vp)) and (VOP_ISLOCKED(vp)=LK_EXCLUSIVE) then
  vfs_badlock('is locked but should not be', str, vp);
end;

procedure assert_vop_elocked(vp:p_vnode;str:PChar);
begin
 if (not IGNORE_LOCK(vp)) and (VOP_ISLOCKED(vp)<>LK_EXCLUSIVE) then
  vfs_badlock('is not exclusive locked but should be', str, vp);
end;

function VOP_WRITE_PRE(ap:p_vop_write_args;var osize,ooffset:Int64):Integer;
var
 va:t_vattr;
 error:Integer;
begin
 Result :=0;
 osize  :=0;
 ooffset:=0;
 if (not VN_KNLIST_EMPTY(ap^.a_vp)) then
 begin
  error:=VOP_GETATTR(ap^.a_vp, @va);
  if (error<>0) then Exit(error);
  ooffset:=ap^.a_uio^.uio_offset;
  osize:=va.va_size;
 end;
end;

procedure VOP_WRITE_POST(ap:p_vop_write_args;ret:Integer;var osize,ooffset:Int64);
var
 noffset:Int64;
begin
 noffset:=ap^.a_uio^.uio_offset;
 if (noffset>ooffset) and (not VN_KNLIST_EMPTY(ap^.a_vp)) then
 begin
  if (noffset>osize) then
  begin
   VFS_KNOTE_LOCKED(ap^.a_vp, NOTE_WRITE or NOTE_EXTEND);
  end else
  begin
   VFS_KNOTE_LOCKED(ap^.a_vp, NOTE_WRITE);
  end;
 end;
end;

procedure vop_rename_fail(ap:p_vop_rename_args);
begin
 if (ap^.a_tvp<>nil) then
  vput(ap^.a_tvp);

 if (ap^.a_tdvp=ap^.a_tvp) then
  vrele(ap^.a_tdvp)
 else
  vput(ap^.a_tdvp);

 vrele(ap^.a_fdvp);
 vrele(ap^.a_fvp);
end;

procedure vop_rename_pre(ap:p_vop_rename_args);
begin
 if (ap^.a_tdvp<>ap^.a_fdvp) then
  vhold(ap^.a_fdvp);

 if (ap^.a_tvp<>ap^.a_fvp) then
  vhold(ap^.a_fvp);

 vhold(ap^.a_tdvp);

 if (ap^.a_tvp<>nil) then
  vhold(ap^.a_tvp);
end;

procedure vop_create_post(ap:p_vop_create_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_dvp, NOTE_WRITE);
 end;
end;

procedure vop_link_post(ap:p_vop_link_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_vp, NOTE_LINK);
  VFS_KNOTE_LOCKED(ap^.a_tdvp, NOTE_WRITE);
 end;
end;

procedure vop_mkdir_post(ap:p_vop_mkdir_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_dvp, NOTE_WRITE or NOTE_LINK);
 end;
end;

procedure vop_mknod_post(ap:p_vop_mknod_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_dvp, NOTE_WRITE);
 end;
end;

procedure vop_remove_post(ap:p_vop_remove_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_dvp, NOTE_WRITE);
  VFS_KNOTE_LOCKED(ap^.a_vp, NOTE_DELETE);
 end;
end;

procedure vop_rename_post(ap:p_vop_rename_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_UNLOCKED(ap^.a_fdvp, NOTE_WRITE);
  VFS_KNOTE_UNLOCKED(ap^.a_tdvp, NOTE_WRITE);
  VFS_KNOTE_UNLOCKED(ap^.a_fvp, NOTE_RENAME);
  if (ap^.a_tvp<>nil) then
  begin
   VFS_KNOTE_UNLOCKED(ap^.a_tvp, NOTE_DELETE);
  end;
 end;
 if (ap^.a_tdvp<>ap^.a_fdvp) then
 begin
  vdrop(ap^.a_fdvp);
 end;
 if (ap^.a_tvp<>ap^.a_fvp) then
 begin
  vdrop(ap^.a_fvp);
 end;
 vdrop(ap^.a_tdvp);
 if (ap^.a_tvp<>nil) then
 begin
  vdrop(ap^.a_tvp);
 end;
end;

procedure vop_rmdir_post(ap:p_vop_rmdir_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_dvp, NOTE_WRITE or NOTE_LINK);
  VFS_KNOTE_LOCKED(ap^.a_vp, NOTE_DELETE);
 end;
end;

procedure vop_setattr_post(ap:p_vop_setattr_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_vp, NOTE_ATTRIB);
 end;
end;

procedure vop_symlink_post(ap:p_vop_symlink_args;rc:Integer);
begin
 if (rc=0) then
 begin
  VFS_KNOTE_LOCKED(ap^.a_dvp, NOTE_WRITE);
 end;
end;

procedure vfs_event_init();
begin
 knlist_init_mtx(@fs_knlist, nil);
end;

procedure vfs_event_signal(fsid:p_fsid;event:DWORD;data:ptrint);
begin
 KNOTE_UNLOCKED(@fs_knlist, event);
end;

function filt_fsattach(kn:p_knote):Integer;
begin
 kn^.kn_flags:=kn^.kn_flags or EV_CLEAR;
 knlist_add(@fs_knlist, kn, 0);
 Exit(0);
end;

procedure filt_fsdetach(kn:p_knote);
begin
 knlist_remove(@fs_knlist, kn, 0);
end;

function filt_fsevent(kn:p_knote;hint:QWORD):Integer;
begin
 kn^.kn_fflags:=kn^.kn_fflags or hint;
 Exit(ord(kn^.kn_fflags<>0));
end;

{
static int
sysctl_vfs_ctl(SYSCTL_HANDLER_ARGS)
begin
 struct vfsidctl vc;
 int error;
 mp:p_mount;

 error:=SYSCTL_IN(req, &vc, sizeof(vc));
 if (error)
  Exit(error);
 if (vc.vc_vers<>VFS_CTL_VERS1)
  Exit(EINVAL);
 mp:=vfs_getvfs(@vc.vc_fsid);
 if (mp=nil)
  Exit(ENOENT);
 { ensure that a specific sysctl goes to the right filesystem. }
 if (strcmp(vc.vc_fstypename, "*'<>0 and
     strcmp(vc.vc_fstypename, mp^.mnt_vfc^.vfc_name)<>0) begin
  vfs_rel(mp);
  Exit(EINVAL);
 end;
 VCTLTOREQ(@vc, req);
 error:=VFS_SYSCTL(mp, vc.vc_op, req);
 vfs_rel(mp);
 Exit(error);
end;

SYSCTL_PROC(_vfs, OID_AUTO, ctl, CTLTYPE_OPAQUE or CTLFLAG_WR, nil, 0, sysctl_vfs_ctl, "", "Sysctl by fsid';

{
 * Function to initialize a va_filerev field sensibly.
 * XXX: Wouldn't a random number make a lot more sense ??
 }
u_quad_t
init_va_filerev(void)
begin
 struct bintime bt;

 getbinuptime(@bt);
 Exit(((u_quad_t)bt.sec shl 32LL) or (bt.frac shr 32LL));
end;
}

procedure filt_vfsdetach(kn:p_knote);                    forward;
function  filt_vfsread  (kn:p_knote;hint:QWORD):Integer; forward;
function  filt_vfswrite (kn:p_knote;hint:QWORD):Integer; forward;
function  filt_vfsvnode (kn:p_knote;hint:QWORD):Integer; forward;

const
 vfsread_filtops:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:nil;
  f_detach:@filt_vfsdetach;
  f_event :@filt_vfsread;
 );

 vfswrite_filtops:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:nil;
  f_detach:@filt_vfsdetach;
  f_event :@filt_vfswrite;
 );

 vfsvnode_filtops:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:nil;
  f_detach:@filt_vfsdetach;
  f_event :@filt_vfsvnode;
 );

procedure vfs_knllock(arg:Pointer);
begin
 vn_lock(p_vnode(arg), LK_EXCLUSIVE or LK_RETRY);
end;

procedure vfs_knlunlock(arg:Pointer);
begin
 VOP_UNLOCK(p_vnode(arg), 0);
end;

procedure vfs_knl_assert_locked(arg:Pointer);
begin
 //
end;

procedure vfs_knl_assert_unlocked(arg:Pointer);
begin
 //
end;

function vfs_kqfilter(ap:p_vop_kqfilter_args):Integer;
var
 vp:p_vnode;
 kn:p_knote;
 knl:p_knlist;
begin
 vp:=ap^.a_vp;
 kn:=ap^.a_kn;

 case (kn^.kn_filter) of
  EVFILT_READ:
   kn^.kn_fop:=@vfsread_filtops;
  EVFILT_WRITE:
   kn^.kn_fop:=@vfswrite_filtops;
  EVFILT_VNODE:
   kn^.kn_fop:=@vfsvnode_filtops;
  else
   Exit(EINVAL);
 end;

 kn^.kn_hook:=vp;

 v_addpollinfo(vp);
 if (vp^.v_pollinfo=nil) then
  Exit(ENOMEM);

 knl:=@vp^.v_pollinfo^.vpi_selinfo.si_note;
 vhold(vp);
 knlist_add(knl, kn, 0);

 Exit(0);
end;

{
 * Detach knote from vnode
 }
procedure filt_vfsdetach(kn:p_knote);
var
 vp:p_vnode;
begin
 vp:=kn^.kn_hook;

 Assert(vp^.v_pollinfo<>nil, 'Missing v_pollinfo');
 knlist_remove(@vp^.v_pollinfo^.vpi_selinfo.si_note, kn, 0);
 vdrop(vp);
end;

{ARGSUSED}
function filt_vfsread(kn:p_knote;hint:QWORD):Integer;
var
 vp:p_vnode;
 va:t_vattr;
 res:Integer;
begin
 vp:=kn^.kn_hook;

 {
  * filesystem is gone, so set the EOF flag and schedule
  * the knote for deletion.
  }
 if (hint=NOTE_REVOKE) then
 begin
  VI_LOCK(vp);
  kn^.kn_flags:=kn^.kn_flags or (EV_EOF or EV_ONESHOT);
  VI_UNLOCK(vp);
  Exit(1);
 end;

 if (VOP_GETATTR(vp, @va)<>0) then
 begin
  Exit(0);
 end;

 VI_LOCK(vp);
 kn^.kn_data:=va.va_size - p_file(kn^.kn_fp)^.f_offset;
 res:=ord(kn^.kn_data<>0);
 VI_UNLOCK(vp);
 Exit(res);
end;

{ARGSUSED}
function filt_vfswrite(kn:p_knote;hint:QWORD):Integer;
var
 vp:p_vnode;
begin
 vp:=kn^.kn_hook;

 VI_LOCK(vp);

 {
  * filesystem is gone, so set the EOF flag and schedule
  * the knote for deletion.
  }
 if (hint=NOTE_REVOKE) then
 begin
  kn^.kn_flags:=kn^.kn_flags or (EV_EOF or EV_ONESHOT);
 end;

 kn^.kn_data:=0;
 VI_UNLOCK(vp);
 Exit(1);
end;

function filt_vfsvnode(kn:p_knote;hint:QWORD):Integer;
var
 vp:p_vnode;
 res:Integer;
begin
 vp:=kn^.kn_hook;

 VI_LOCK(vp);
 if ((kn^.kn_sfflags and hint)<>0) then
 begin
  kn^.kn_fflags:=kn^.kn_fflags or hint;
 end;
 if (hint=NOTE_REVOKE) then
 begin
  kn^.kn_flags:=kn^.kn_flags or EV_EOF;
  VI_UNLOCK(vp);
  Exit(1);
 end;
 res:=ord(kn^.kn_fflags<>0);
 VI_UNLOCK(vp);
 Exit(res);
end;

function vfs_read_dirent(ap:p_vop_readdir_args;dp:p_dirent;off:QWORD):Integer;
var
 error:Integer;
begin
 if (dp^.d_reclen > ap^.a_uio^.uio_resid) then
  Exit(ENAMETOOLONG);

 error:=uiomove(dp, dp^.d_reclen, ap^.a_uio);
 if (error<>0) then
 begin
  if (ap^.a_ncookies<>nil) then
  begin
   if (ap^.a_cookies<>nil) then
   begin
    FreeMem(ap^.a_cookies);
   end;
   ap^.a_cookies:=nil;
   ap^.a_ncookies^:=0;
  end;
  Exit(error);
 end;

 if (ap^.a_ncookies=nil) then Exit(0);

 Assert(ap^.a_cookies<>nil,'null ap^.a_cookies value with non-null ap^.a_ncookies!');

 ap^.a_cookies^:=ReAllocMem(ap^.a_cookies^,(ap^.a_ncookies^ + 1) * sizeof(QWORD));
 ap^.a_cookies^[ap^.a_ncookies^]:=off;

 Inc(ap^.a_ncookies^);
 Exit(0);
end;

{
 * Mark for update the access time of the file if the filesystem
 * supports VOP_MARKATIME.  This functionality is used by execve and
 * mmap, so we want to avoid the I/O implied by directly setting
 * va_atime for the sake of efficiency.
 }
procedure vfs_mark_atime(vp:p_vnode);
var
 mp:p_mount;
begin
 mp:=vp^.v_mount;
 VFS_ASSERT_GIANT(mp);
 ASSERT_VOP_LOCKED(vp,'vfs_mark_atime');

 if (mp<>nil) then
 if ((mp^.mnt_flag and (MNT_NOATIME or MNT_RDONLY))=0) then
 begin
  VOP_MARKATIME(vp);
 end;
end;

{
 * The purpose of this routine is to remove granularity from accmode_t,
 * reducing it into standard unix access bits - VEXEC, VREAD, VWRITE,
 * VADMIN and VAPPEND.
 *
 * If it returns 0, the caller is supposed to continue with the usual
 * access checks using 'accmode' as modified by this routine.  If it
 * returns nonzero value, the caller is supposed to Exitthat value
 * as errno.
 *
 * Note that after this routine runs, accmode may be zero.
 }
function vfs_unixify_accmode(accmode:p_accmode_t):Integer;
begin
 {
  * There is no way to specify explicit "deny" rule using
  * file mode or POSIX.1e ACLs.
  }
 if ((accmode^ and VEXPLICIT_DENY)<>0) then
 begin
  accmode^:=0;
  Exit(0);
 end;

 {
  * None of these can be translated into usual access bits.
  * Also, the common case for NFSv4 ACLs is to not contain
  * either of these bits. Caller should check for VWRITE
  * on the containing directory instead.
  }
 if ((accmode^ and (VDELETE_CHILD or VDELETE))<>0) then
  Exit(EPERM);

 if ((accmode^ and VADMIN_PERMS)<>0) then
 begin
  accmode^:=accmode^ and (not VADMIN_PERMS);
  accmode^:=accmode^ or VADMIN;
 end;

 {
  * There is no way to deny VREAD_ATTRIBUTES, VREAD_ACL
  * or VSYNCHRONIZE using file mode or POSIX.1e ACL.
  }
 accmode^:=accmode^ and (not (VSTAT_PERMS or VSYNCHRONIZE));

 Exit(0);
end;


{
 * These are helper functions for filesystems to traverse all
 * their vnodes.  See MNT_VNODE_FOREACH_ALL() in sys/mount.h.
 *
 * This interface replaces MNT_VNODE_FOREACH.
 }
function __mnt_vnode_next_all(mvp:pp_vnode;mp:p_mount):p_vnode;
var
 vp:p_vnode;
begin
 //if (should_yield())
  kern_yield(PRI_UNCHANGED);

 MNT_ILOCK(mp);
 Assert(mvp^^.v_mount=mp, 'marker vnode mount list mismatch');
 vp:=TAILQ_NEXT(mvp^,@mvp^^.v_nmntvnodes);
 while (vp<>nil) do
 begin
  if not ((vp^.v_type=VMARKER) or ((vp^.v_iflag and VI_DOOMED)<>0)) then Break;
  vp:=TAILQ_NEXT(vp,@vp^.v_nmntvnodes);
 end;

 { Check if we are done }
 if (vp=nil) then
 begin
  __mnt_vnode_markerfree_all(mvp, mp);
  { MNT_IUNLOCK(mp); -- done in above function }
  Exit(nil);
 end;
 TAILQ_REMOVE(@mp^.mnt_nvnodelist,mvp^,@mvp^^.v_nmntvnodes);
 TAILQ_INSERT_AFTER(@mp^.mnt_nvnodelist,vp,mvp^,@mvp^^.v_nmntvnodes);
 VI_LOCK(vp);
 MNT_IUNLOCK(mp);
 Exit(vp);
end;

function __mnt_vnode_first_all(mvp:pp_vnode;mp:p_mount):p_vnode;
var
 vp:p_vnode;
begin
 mvp^:=AllocMem(sizeof(t_vnode));
 MNT_ILOCK(mp);
 MNT_REF(mp);
 mvp^^.v_type:=VMARKER;

 vp:=TAILQ_FIRST(@mp^.mnt_nvnodelist);
 while (vp<>nil) and
       ((vp^.v_type=VMARKER) or ((vp^.v_iflag and VI_DOOMED)<>0)) do
 begin
  vp:=TAILQ_NEXT(vp,@vp^.v_nmntvnodes);
 end;

 { Check if we are done }
 if (vp=nil) then
 begin
  MNT_REL(mp);
  MNT_IUNLOCK(mp);
  FreeMem(mvp^);
  mvp^:=nil;
  Exit(nil);
 end;
 mvp^^.v_mount:=mp;
 TAILQ_INSERT_AFTER(@mp^.mnt_nvnodelist,vp,mvp^,@mvp^^.v_nmntvnodes);
 VI_LOCK(vp);
 MNT_IUNLOCK(mp);
 Exit(vp);
end;

procedure __mnt_vnode_markerfree_all(mvp:pp_vnode;mp:p_mount);
begin
 if (mvp^=nil) then
 begin
  MNT_IUNLOCK(mp);
  Exit;
 end;

 mtx_assert(MNT_MTX(mp)^);

 Assert(mvp^^.v_mount=mp, 'marker vnode mount list mismatch');
 TAILQ_REMOVE(@mp^.mnt_nvnodelist,mvp^,@mvp^^.v_nmntvnodes);
 MNT_REL(mp);
 MNT_IUNLOCK(mp);
 FreeMem(mvp^);
 mvp^:=nil;
end;

{
 * These are helper functions for filesystems to traverse their
 * active vnodes.  See MNT_VNODE_FOREACH_ACTIVE() in sys/mount.h
 }
procedure mnt_vnode_markerfree_active(mvp:pp_vnode;mp:p_mount);
begin
 Assert(mvp^^.v_mount=mp, 'marker vnode mount list mismatch');

 MNT_ILOCK(mp);
 MNT_REL(mp);
 MNT_IUNLOCK(mp);
 FreeMem(mvp^);
 mvp^:=nil;
end;

function mnt_vnode_next_active(mvp:pp_vnode;mp:p_mount):p_vnode;
label
 restart;
var
 vp,nvp:p_vnode;
begin
 mtx_assert(vnode_free_list_mtx);
 Assert(mvp^^.v_mount=mp, 'marker vnode mount list mismatch');
restart:
 vp:=TAILQ_NEXT(mvp^,@mvp^^.v_actfreelist);
 TAILQ_REMOVE(@mp^.mnt_activevnodelist,mvp^,@mvp^^.v_actfreelist);
 while (vp<>nil) do
 begin
  if (vp^.v_type=VMARKER) then
  begin
   vp:=TAILQ_NEXT(vp,@vp^.v_actfreelist);
   continue;
  end;
  if (not VI_TRYLOCK(vp)) then
  begin
   continue;
  end;
  Assert(vp^.v_type<>VMARKER, 'locked marker %p');
  Assert((vp^.v_mount=mp) or (vp^.v_mount=nil),'alien vnode on the active list %p %p');
  if (vp^.v_mount=mp) and ((vp^.v_iflag and VI_DOOMED)=0) then
  begin
   break;
  end;
  nvp:=TAILQ_NEXT(vp,@vp^.v_actfreelist);
  VI_UNLOCK(vp);
  vp:=nvp;
 end;

 { Check if we are done }
 if (vp=nil) then
 begin
  mtx_unlock(vnode_free_list_mtx);
  mnt_vnode_markerfree_active(mvp, mp);
  Exit(nil);
 end;
 TAILQ_INSERT_AFTER(@mp^.mnt_activevnodelist,vp,mvp^,@mvp^^.v_actfreelist);
 mtx_unlock(vnode_free_list_mtx);
 ASSERT_VI_LOCKED(vp, 'active iter');
 Assert((vp^.v_iflag and VI_ACTIVE)<>0, 'Non-active vp %p');
 Exit(vp);
end;

function __mnt_vnode_next_active(mvp:pp_vnode;mp:p_mount):p_vnode;
begin
 //if (should_yield())
  kern_yield(PRI_UNCHANGED);
 mtx_lock(vnode_free_list_mtx);
 Exit(mnt_vnode_next_active(mvp, mp));
end;

function __mnt_vnode_first_active(mvp:pp_vnode;mp:p_mount):p_vnode;
var
 vp:p_vnode;
begin
 mvp^:=AllocMem(sizeof(t_vnode));
 MNT_ILOCK(mp);
 MNT_REF(mp);
 MNT_IUNLOCK(mp);
 mvp^^.v_type:=VMARKER;
 mvp^^.v_mount:=mp;

 mtx_lock(vnode_free_list_mtx);
 vp:=TAILQ_FIRST(@mp^.mnt_activevnodelist);
 if (vp=nil) then
 begin
  mtx_unlock(vnode_free_list_mtx);
  mnt_vnode_markerfree_active(mvp, mp);
  Exit(nil);
 end;
 TAILQ_INSERT_BEFORE(vp, mvp^,@mvp^^.v_actfreelist);
 Exit(mnt_vnode_next_active(mvp, mp));
end;

procedure __mnt_vnode_markerfree_active(mvp:pp_vnode;mp:p_mount);
begin
 if (mvp^=nil) then
  Exit;

 mtx_lock(vnode_free_list_mtx);
 TAILQ_REMOVE(@mp^.mnt_activevnodelist,mvp^,@mvp^^.v_actfreelist);
 mtx_unlock(vnode_free_list_mtx);
 mnt_vnode_markerfree_active(mvp, mp);
end;


end.

