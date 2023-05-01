unit devfs_rule;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 devfs,
 kern_sx;

{
 * Kernel version of devfs_rule.
 }
type
 p_devfs_ruleset=^t_devfs_ruleset;

 p_devfs_krule=^t_devfs_krule;
 t_devfs_krule=packed record
  dk_list   :TAILQ_ENTRY; //devfs_krule
  dk_ruleset:p_devfs_ruleset;
  dk_rule   :t_devfs_rule;
 end;

 rulehead=TAILQ_HEAD; //devfs_krule

{
 * Structure to describe a ruleset.
 }
 t_devfs_ruleset=packed record
  ds_list    :TAILQ_ENTRY; //devfs_ruleset
  ds_rules   :rulehead;
  ds_number  :devfs_rsnum;
  ds_refcount:Integer;
 end;

function devfs_rid_input(rid:devfs_rid;dm:p_devfs_mount):devfs_rid;
//
procedure devfs_rule_applyde_recursive(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent);
procedure devfs_rule_applydm(dk:p_devfs_krule;dm:p_devfs_mount);
function  devfs_rule_autonumber(ds:p_devfs_ruleset;rnump:p_devfs_rnum):Integer;
function  devfs_rule_byid(rid:devfs_rid):p_devfs_krule;
function  devfs_rule_delete(dk:p_devfs_krule):Integer;
function  devfs_rule_getdev(de:p_devfs_dirent):p_cdev;
function  devfs_rule_input(dr:p_devfs_rule;dm:p_devfs_mount):Integer;
function  devfs_rule_insert(dr:p_devfs_rule):Integer;
function  devfs_rule_match(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent):Integer;
function  devfs_rule_matchpath(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent):Integer;
procedure devfs_rule_run(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent;depth:DWORD);
//
procedure devfs_ruleset_applyde(ds:p_devfs_ruleset;dm:p_devfs_mount;de:p_devfs_dirent;depth:DWORD);
procedure devfs_ruleset_applydm(ds:p_devfs_ruleset;dm:p_devfs_mount);
function  devfs_ruleset_bynum(rsnum:devfs_rsnum):p_devfs_ruleset;
function  devfs_ruleset_create(rsnum:devfs_rsnum):p_devfs_ruleset;
procedure devfs_ruleset_reap(ds:p_devfs_ruleset);
function  devfs_ruleset_use(rsnum:devfs_rsnum;dm:p_devfs_mount):Integer;

procedure devfs_rules_apply(dm:p_devfs_mount;de:p_devfs_dirent);
function  devfs_rules_ioctl(dm:p_devfs_mount;cmd:QWORD;data:Pointer):Integer;
procedure devfs_rules_cleanup(dm:p_devfs_mount);
procedure devfs_ruleset_set(rsnum:devfs_rsnum;dm:p_devfs_mount);
procedure devfs_ruleset_apply(dm:p_devfs_mount);

var
 //SX_SYSINIT(sx_rules, @sx_rules, 'DEVFS ruleset lock');
 sx_rules:t_sx=(n:'DEVFS ruleset lock';c:nil;m:0);

 devfs_rulesets:TAILQ_HEAD=(tqh_first:nil;tqh_last:@devfs_rulesets.tqh_first);

implementation

uses
 errno,
 vdirent,
 sys_fnmatch;

//

{
 * Called to apply the proper rules for 'de' before it can be
 * exposed to the userland.  This should be called with an exclusive
 * lock on dm in case we need to run anything.
 }
procedure devfs_rules_apply(dm:p_devfs_mount;de:p_devfs_dirent);
var
 ds:p_devfs_ruleset;
begin
 sx_assert(@dm^.dm_lock);

 if (dm^.dm_ruleset=0) then
  Exit;
 sx_slock(@sx_rules);
 ds:=devfs_ruleset_bynum(dm^.dm_ruleset);
 Assert(ds<>nil, ('mount-point has nil ruleset'));
 devfs_ruleset_applyde(ds, dm, de, devfs_rule_depth);
 sx_sunlock(@sx_rules);
end;

{
 * Rule subsystem ioctl hook.
 }
function devfs_rules_ioctl(dm:p_devfs_mount;cmd:QWORD;data:Pointer):Integer;
label
 _break;
var
 ds:p_devfs_ruleset;
 dk:p_devfs_krule;
 dr:p_devfs_rule;
 rsnum:devfs_rsnum;
 rnum:devfs_rnum;
 rid:devfs_rid;
 error:Integer;
begin
 sx_assert(@dm^.dm_lock);

 {
  * XXX: This Exits an error regardless of whether we actually
  * support the cmd or not.
  *
  * We could make this privileges finer grained if desired.
  }
 error:=EPERM;
 //error:=priv_check(td, PRIV_DEVFS_RULE);
 if (error<>0) then
  Exit(error);

 sx_xlock(@sx_rules);

 case cmd of
  DEVFSIO_RADD:
   begin
    dr:=p_devfs_rule(data);
    error:=devfs_rule_input(dr, dm);
    if (error<>0) then
     goto _break;
    dk:=devfs_rule_byid(dr^.dr_id);
    if (dk<>nil) then
    begin
     error:=EEXIST;
     goto _break;
    end;
    if (rid2rsn(dr^.dr_id)=0) then
    begin
     error:=EIO;
     goto _break;
    end;
    error:=devfs_rule_insert(dr);
   end;
  DEVFSIO_RAPPLY:
   begin
    dr:=p_devfs_rule(data);
    error:=devfs_rule_input(dr, dm);
    if (error<>0) then
     goto _break;

    {
     * This is one of many possible hackish
     * implementations.  The primary contender is an
     * implementation where the rule we read in is
     * temporarily inserted into some ruleset, perhaps
     * with a hypothetical DRO_NOAUTO flag so that it
     * doesn't get used where it isn't intended, and
     * applied in the normal way.  This can be done in the
     * userland (DEVFSIO_ADD, DEVFSIO_APPLYID,
     * DEVFSIO_DEL) or in the kernel; either way it breaks
     * some corner case assumptions in other parts of the
     * code (not that this implementation doesn't do
     * that).
     }
    if ((dr^.dr_iacts and DRA_INCSET)<>0) and
       (devfs_ruleset_bynum(dr^.dr_incset)=nil) then
    begin
     error:=ESRCH;
     goto _break;
    end;
    dk:=AllocMem(sizeof(t_devfs_krule));
    Move(dr^,dk^.dk_rule,sizeof(t_devfs_rule));
    devfs_rule_applydm(dk, dm);
    FreeMem(dk);
   end;
  DEVFSIO_RAPPLYID:
   begin
    rid:=p_devfs_rid(data)^;
    rid:=devfs_rid_input(rid, dm);
    dk:=devfs_rule_byid(rid);
    if (dk=nil) then
    begin
     error:=ENOENT;
     goto _break;
    end;
    devfs_rule_applydm(dk, dm);
   end;
  DEVFSIO_RDEL:
   begin
    rid:=p_devfs_rid(data)^;
    rid:=devfs_rid_input(rid, dm);
    dk:=devfs_rule_byid(rid);
    if (dk=nil) then
    begin
     error:=ENOENT;
     goto _break
    end;
    ds:=dk^.dk_ruleset;
    error:=devfs_rule_delete(dk);
   end;
  DEVFSIO_RGETNEXT:
   begin
    dr:=p_devfs_rule(data);
    error:=devfs_rule_input(dr, dm);
    if (error<>0) then
     goto _break;
    {
     * We can't use devfs_rule_byid() here since that
     * requires the rule specified to exist, but we want
     * getnext(N) to work whether there is a rule N or not
     * (specifically, getnext(0) must work, but we should
     * never have a rule 0 since the add command
     * interprets 0 to mean 'auto-number').
     }
    ds:=devfs_ruleset_bynum(rid2rsn(dr^.dr_id));
    if (ds=nil) then
    begin
     error:=ENOENT;
     goto _break;
    end;
    rnum:=rid2rn(dr^.dr_id);
    dk:=TAILQ_FIRST(@ds^.ds_rules);
    while (dk<>nil) do
    begin
     if (rid2rn(dk^.dk_rule.dr_id) > rnum) then
      break;
     dk:=TAILQ_NEXT(dk,@dk^.dk_list);
    end;
    if (dk=nil) then
    begin
     error:=ENOENT;
     goto _break;
    end;
    Move(dk^.dk_rule,dr^,sizeof(t_devfs_rule));
   end;
  DEVFSIO_SUSE:
   begin
    rsnum:=p_devfs_rsnum(data)^;
    error:=devfs_ruleset_use(rsnum, dm);
   end;
  DEVFSIO_SAPPLY:
   begin
    rsnum:=p_devfs_rsnum(data)^;
    rsnum:=rid2rsn(devfs_rid_input(mkrid(rsnum, 0), dm));
    ds:=devfs_ruleset_bynum(rsnum);
    if (ds=nil) then
    begin
     error:=ESRCH;
     goto _break;
    end;
    devfs_ruleset_applydm(ds, dm);
   end;
  DEVFSIO_SGETNEXT:
   begin
    rsnum:=p_devfs_rsnum(data)^;
    ds:=TAILQ_FIRST(@devfs_rulesets);
    while (ds<>nil) do
    begin
     if (ds^.ds_number > rsnum) then
      break;
     ds:=TAILQ_NEXT(ds,@ds^.ds_list);
    end;
    if (ds=nil) then
    begin
     error:=ENOENT;
     goto _break;
    end;
    p_devfs_rsnum(data)^:=ds^.ds_number;
   end;
  else
   error:=ENOIOCTL;
 end;
_break:

 sx_xunlock(@sx_rules);
 Exit(error);
end;

{
 * Adjust the rule identifier to use the ruleset of dm if one isn't
 * explicitly specified.
 *
 * Note that after this operation, rid2rsn(rid) might still be 0, and
 * that's okay; ruleset 0 is a valid ruleset, but when it's read in
 * from the userland, it means 'current ruleset for this mount-point'.
 }
function devfs_rid_input(rid:devfs_rid;dm:p_devfs_mount):devfs_rid;
begin
 if (rid2rsn(rid)=0) then
  Exit(mkrid(dm^.dm_ruleset, rid2rn(rid)))
 else
  Exit(rid);
end;

{
 * Apply dk to de and everything under de.
 *
 * XXX: This method needs a function call for every nested
 * subdirectory in a devfs mount.  If we plan to have many of these,
 * we might eventually run out of kernel stack space.
 * XXX: a linear search could be done through the cdev list instead.
 }
procedure devfs_rule_applyde_recursive(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent);
var
 de2:p_devfs_dirent;
begin
 de2:=TAILQ_FIRST(@de^.de_dlist);
 while (de2<>nil) do
 begin
  devfs_rule_applyde_recursive(dk, dm, de2);
  de2:=TAILQ_NEXT(de2,@de2^.de_list);
 end;
 devfs_rule_run(dk, dm, de, devfs_rule_depth);
end;

{
 * Apply dk to all entires in dm.
 }
procedure devfs_rule_applydm(dk:p_devfs_krule;dm:p_devfs_mount);
begin
 devfs_rule_applyde_recursive(dk, dm, dm^.dm_rootdir);
end;

{
 * Automatically select a number for a new rule in ds, and write the
 * result into rnump.
 }
function devfs_rule_autonumber(ds:p_devfs_ruleset;rnump:p_devfs_rnum):Integer;
var
 dk:p_devfs_krule;
begin
 { Find the last rule. }
 dk:=TAILQ_LAST(@ds^.ds_rules);
 if (dk=nil) then
  rnump^:=100
 else
 begin
  rnump^:=rid2rn(dk^.dk_rule.dr_id) + 100;
  { Detect overflow. }
  if (rnump^ < rid2rn(dk^.dk_rule.dr_id)) then
   Exit(ERANGE);
 end;
 Assert(devfs_rule_byid(mkrid(ds^.ds_number, rnump^))=nil,'autonumbering resulted in an already existing rule');
 Exit(0);
end;

{
 * Find a krule by id.
 }
function devfs_rule_byid(rid:devfs_rid):p_devfs_krule;
var
 ds:p_devfs_ruleset;
 dk:p_devfs_krule;
 rn:devfs_rnum;
begin
 rn:=rid2rn(rid);
 ds:=devfs_ruleset_bynum(rid2rsn(rid));
 if (ds=nil) then
  Exit(nil);
 dk:=TAILQ_FIRST(@ds^.ds_rules);
 while (dk<>nil) do
 begin
  if (rid2rn(dk^.dk_rule.dr_id)=rn) then
   Exit(dk)
  else
  if (rid2rn(dk^.dk_rule.dr_id) > rn) then
   break;
  dk:=TAILQ_NEXT(dk,@dk^.dk_list);
 end;
 Exit(nil);
end;

{
 * Remove dkp from any lists it may be on and remove memory associated
 * with it.
 }
function devfs_rule_delete(dk:p_devfs_krule):Integer;
var
 ds:p_devfs_ruleset;
begin
 if ((dk^.dk_rule.dr_iacts and DRA_INCSET)<>0) then
 begin
  ds:=devfs_ruleset_bynum(dk^.dk_rule.dr_incset);
  Assert(ds<>nil,'DRA_INCSET but bad dr_incset');
  Dec(ds^.ds_refcount);
  devfs_ruleset_reap(ds);
 end;
 ds:=dk^.dk_ruleset;
 TAILQ_REMOVE(@ds^.ds_rules,dk,@dk^.dk_list);
 devfs_ruleset_reap(ds);
 FreeMem(dk);
 Exit(0);
end;

{
 * Get a struct cdev *corresponding to de so we can try to match rules based
 * on it.  If this routine Exits nil, there is no struct cdev *associated
 * with the dirent (symlinks and directories don't have dev_ts), and
 * the caller should assume that any critera dependent on a dev_t
 * don't match.
 }
function devfs_rule_getdev(de:p_devfs_dirent):p_cdev;
begin
 if (de^.de_cdp=nil) then
  Exit(nil);
 if ((de^.de_cdp^.cdp_flags and CDP_ACTIVE)<>0) then
  Exit(@de^.de_cdp^.cdp_c)
 else
  Exit(nil);
end;

{
 * Do what we need to do to a rule that we just loaded from the
 * userland.  In particular, we need to check the magic, and adjust
 * the ruleset appropriate if desired.
 }
function devfs_rule_input(dr:p_devfs_rule;dm:p_devfs_mount):Integer;
begin
 if (dr^.dr_magic<>DEVFS_MAGIC) then
  Exit(ERPCMISMATCH);
 dr^.dr_id:=devfs_rid_input(dr^.dr_id, dm);
 Exit(0);
end;

{
 * Import dr into the appropriate place in the kernel (i.e., make a
 * krule).  The value of dr is copied, so the pointer may be destroyed
 * after this call completes.
 }
function devfs_rule_insert(dr:p_devfs_rule):Integer;
var
 ds,dsi:p_devfs_ruleset;
 k1:p_devfs_krule;
 dk:p_devfs_krule;
 rsnum:devfs_rsnum;
 dkrn:devfs_rnum;
 error:Integer;
begin
 {
  * This stuff seems out of place here, but we want to do it as
  * soon as possible so that if it fails, we don't have to roll
  * back any changes we already made (e.g., ruleset creation).
  }
 if ((dr^.dr_iacts and DRA_INCSET)<>0) then
 begin
  dsi:=devfs_ruleset_bynum(dr^.dr_incset);
  if (dsi=nil) then
   Exit(ESRCH);
 end else
  dsi:=nil;

 rsnum:=rid2rsn(dr^.dr_id);
 Assert(rsnum<>0, ('Inserting into ruleset zero'));

 ds:=devfs_ruleset_bynum(rsnum);
 if (ds=nil) then
  ds:=devfs_ruleset_create(rsnum);
 dkrn:=rid2rn(dr^.dr_id);
 if (dkrn=0) then
 begin
  error:=devfs_rule_autonumber(ds, @dkrn);
  if (error<>0) then
  begin
   devfs_ruleset_reap(ds);
   Exit(error);
  end;
 end;

 dk:=AllocMem(sizeof(t_devfs_krule));
 dk^.dk_ruleset:=ds;
 if (dsi<>nil) then
  Inc(dsi^.ds_refcount);
 { XXX: Inspect dr? }
 Move(dr^,dk^.dk_rule,sizeof(t_devfs_rule));
 dk^.dk_rule.dr_id:=mkrid(rid2rsn(dk^.dk_rule.dr_id), dkrn);
 k1:=TAILQ_FIRST(@ds^.ds_rules);
 while (k1<>nil) do
 begin
  if (rid2rn(k1^.dk_rule.dr_id) > dkrn) then
  begin
   TAILQ_INSERT_BEFORE(k1,dk,@dk^.dk_list);
   break;
  end;
  k1:=TAILQ_NEXT(k1,@k1^.dk_list);
 end;
 if (k1=nil) then
  TAILQ_INSERT_TAIL(@ds^.ds_rules,dk,@dk^.dk_list);
 Exit(0);
end;

{
 * Determine whether dk matches de.  Exits 1 if dk should be run on
 * de; 0, otherwise.
 }
function devfs_rule_match(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent):Integer;
var
 dr:p_devfs_rule;
 dev:p_cdev;
 //dsw:p_cdevsw;
 dsw:Pointer;
 ref:Integer;
begin
 dr:=@dk^.dk_rule;

 dev:=devfs_rule_getdev(de);
 {
  * At this point, if dev is nil, we should assume that any
  * criteria that depend on it don't match.  We should *not*
  * just ignore them (i.e., act like they weren't specified),
  * since that makes a rule that only has criteria dependent on
  * the struct cdev *match all symlinks and directories.
  *
  * Note also that the following tests are somewhat reversed:
  * They're actually testing to see whether the condition does
  * *not* match, since the default is to assume the rule should
  * be run (such as if there are no conditions).
  }
 if ((dr^.dr_icond and DRC_DSWFLAGS)<>0) then
 begin
  if (dev=nil) then
   Exit(0);
  dsw:=nil;
  //dsw:=dev_refthread(dev, @ref);
  if (dsw=nil) then
   Exit(0);
  //if ((dsw^.d_flags and dr^.dr_dswflags)=0) then
  //begin
  // dev_relthread(dev, ref);
  // Exit(0);
  //end;
  //dev_relthread(dev, ref);
 end;
 if ((dr^.dr_icond and DRC_PATHPTRN)<>0) then
  if (devfs_rule_matchpath(dk, dm, de)=0) then
   Exit(0);

 Exit(1);
end;

{
 * Determine whether dk matches de on account of dr_pathptrn.
 }
function devfs_rule_matchpath(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent):Integer;
var
 dr:p_devfs_rule;
 dev:p_cdev;
 pname,specname:PChar;
begin
 dr:=@dk^.dk_rule;

 specname:=nil;
 dev:=devfs_rule_getdev(de);
 if (dev<>nil) then
  pname:=dev^.si_name
 else
 if (de^.de_dirent^.d_type=DT_LNK) or
    ((de^.de_dirent^.d_type=DT_DIR) and
     (de<>dm^.dm_rootdir) and
     ((de^.de_flags and (DE_DOT or DE_DOTDOT))=0)) then
 begin
  specname:=AllocMem(SPECNAMELEN + 1);
  //pname:=devfs_fqpn(specname, dm, de, nil);
 end else
  Exit(0);

 Assert(pname<>nil, ('devfs_rule_matchpath: nil pname'));
 Result:=ord(fnmatch(dr^.dr_pathptrn, pname, FNM_PATHNAME)=0);
 FreeMem(specname);
 Exit;
end;

{
 * Run dk on de.
 }
procedure devfs_rule_run(dk:p_devfs_krule;dm:p_devfs_mount;de:p_devfs_dirent;depth:DWORD);
var
 dr:p_devfs_rule;
 ds:p_devfs_ruleset;
begin
 dr:=@dk^.dk_rule;

 if (devfs_rule_match(dk, dm, de)=0) then
  Exit;
 if ((dr^.dr_iacts and DRA_BACTS)<>0) then
 begin
  if ((dr^.dr_bacts and DRB_HIDE)<>0) then
   de^.de_flags:=de^.de_flags or DE_WHITEOUT;
  if ((dr^.dr_bacts and DRB_UNHIDE)<>0) then
   de^.de_flags:=de^.de_flags and (not DE_WHITEOUT);
 end;
 if ((dr^.dr_iacts and DRA_UID)<>0) then
  de^.de_uid:=dr^.dr_uid;
 if ((dr^.dr_iacts and DRA_GID)<>0) then
  de^.de_gid:=dr^.dr_gid;
 if ((dr^.dr_iacts and DRA_MODE)<>0)  then
  de^.de_mode:=dr^.dr_mode;
 if ((dr^.dr_iacts and DRA_INCSET)<>0) then
 begin
  {
   * XXX: we should tell the user if the depth is exceeded here
   * XXX: but it is not obvious how to.  A Exitvalue will
   * XXX: not work as this is called when devices are created
   * XXX: long time after the rules were instantiated.
   * XXX: a printf() would probably give too much noise, or
   * XXX: DoS the machine.  I guess a rate-limited message
   * XXX: might work.
   }
  if (depth > 0) then
  begin
   ds:=devfs_ruleset_bynum(dk^.dk_rule.dr_incset);
   Assert(ds<>nil,'DRA_INCSET but bad dr_incset');
   devfs_ruleset_applyde(ds, dm, de, depth - 1);
  end;
 end;
end;

{
 * Apply all the rules in ds to de.
 }
procedure devfs_ruleset_applyde(ds:p_devfs_ruleset;dm:p_devfs_mount;de:p_devfs_dirent;depth:DWORD);
var
 dk:p_devfs_krule;
begin
 dk:=TAILQ_FIRST(@ds^.ds_rules);
 while (dk<>nil) do
 begin
  devfs_rule_run(dk, dm, de, depth);
  dk:=TAILQ_NEXT(dk,@dk^.dk_list);
 end;
end;

{
 * Apply all the rules in ds to all the entires in dm.
 }
procedure devfs_ruleset_applydm(ds:p_devfs_ruleset;dm:p_devfs_mount);
var
 dk:p_devfs_krule;
begin
 {
  * XXX: Does it matter whether we do
  *
  * foreach(dk in ds)
  *  foreach(de in dm)
  *   apply(dk to de)
  *
  * as opposed to
  *
  * foreach(de in dm)
  *  foreach(dk in ds)
  *   apply(dk to de)
  *
  * The end result is obviously the same, but does the order
  * matter?
  }
 dk:=TAILQ_FIRST(@ds^.ds_rules);
 while (dk<>nil) do
 begin
  devfs_rule_applydm(dk, dm);
  dk:=TAILQ_NEXT(dk,@dk^.dk_list);
 end;
end;

{
 * Find a ruleset by number.
 }
function devfs_ruleset_bynum(rsnum:devfs_rsnum):p_devfs_ruleset;
var
 ds:p_devfs_ruleset;
begin
 ds:=TAILQ_FIRST(@devfs_rulesets);
 while (ds<>nil) do
 begin
  if (ds^.ds_number=rsnum) then
   Exit(ds);
  ds:=TAILQ_NEXT(ds,@ds^.ds_list);
 end;
 Exit(nil);
end;

{
 * Create a new ruleset.
 }
function devfs_ruleset_create(rsnum:devfs_rsnum):p_devfs_ruleset;
var
 s1:p_devfs_ruleset;
 ds:p_devfs_ruleset;
begin
 Assert(rsnum<>0,'creating ruleset zero');

 Assert(devfs_ruleset_bynum(rsnum)=nil,'creating already existent ruleset');

 ds:=AllocMem(sizeof(t_devfs_ruleset));
 ds^.ds_number:=rsnum;
 TAILQ_INIT(@ds^.ds_rules);

 s1:=TAILQ_FIRST(@devfs_rulesets);
 while (s1<>nil) do
 begin
  if (s1^.ds_number > rsnum) then
  begin
   TAILQ_INSERT_BEFORE(s1,ds,@ds^.ds_list);
   break;
  end;
  s1:=TAILQ_NEXT(s1,@s1^.ds_list);
 end;
 if (s1=nil) then
  TAILQ_INSERT_TAIL(@devfs_rulesets,ds,@ds^.ds_list);
 Exit(ds);
end;

{
 * Remove a ruleset from the system if it's empty and not used
 * anywhere.  This should be called after every time a rule is deleted
 * from this ruleset or the reference count is decremented.
 }
procedure devfs_ruleset_reap(ds:p_devfs_ruleset);
begin
 Assert(ds^.ds_number<>0,'reaping ruleset zero');

 if (not TAILQ_EMPTY(@ds^.ds_rules)) or (ds^.ds_refcount<>0) then
  Exit;

 TAILQ_REMOVE(@devfs_rulesets,ds,@ds^.ds_list);
 FreeMem(ds);
end;

{
 * Make rsnum the active ruleset for dm.
 }
function devfs_ruleset_use(rsnum:devfs_rsnum;dm:p_devfs_mount):Integer;
var
 cds,ds:p_devfs_ruleset;
begin
 if (dm^.dm_ruleset<>0) then
 begin
  cds:=devfs_ruleset_bynum(dm^.dm_ruleset);
  Dec(cds^.ds_refcount);
  devfs_ruleset_reap(cds);
 end;

 if (rsnum=0) then
 begin
  dm^.dm_ruleset:=0;
  Exit(0);
 end;

 ds:=devfs_ruleset_bynum(rsnum);
 if (ds=nil) then
  ds:=devfs_ruleset_create(rsnum);
 { These should probably be made atomic somehow. }
 Inc(ds^.ds_refcount);
 dm^.dm_ruleset:=rsnum;

 Exit(0);
end;

procedure devfs_rules_cleanup(dm:p_devfs_mount);
var
 ds:p_devfs_ruleset;
begin
 sx_assert(@dm^.dm_lock);
 if (dm^.dm_ruleset<>0) then
 begin
  ds:=devfs_ruleset_bynum(dm^.dm_ruleset);
  Dec(ds^.ds_refcount);
  devfs_ruleset_reap(ds);
 end;
end;

{
 * Make rsnum the active ruleset for dm (locked)
 }
procedure devfs_ruleset_set(rsnum:devfs_rsnum;dm:p_devfs_mount);
begin
 sx_assert(@dm^.dm_lock);
 sx_xlock(@sx_rules);
 devfs_ruleset_use(rsnum, dm);
 sx_xunlock(@sx_rules);
end;

{
 * Apply the current active ruleset on a mount
 }
procedure devfs_ruleset_apply(dm:p_devfs_mount);
var
 ds:p_devfs_ruleset;
begin
 sx_assert(@dm^.dm_lock);

 sx_xlock(@sx_rules);
 if (dm^.dm_ruleset=0) then
 begin
  sx_xunlock(@sx_rules);
  Exit;
 end;
 ds:=devfs_ruleset_bynum(dm^.dm_ruleset);
 if (ds<>nil) then
  devfs_ruleset_applydm(ds, dm);
 sx_xunlock(@sx_rules);
end;


end.

