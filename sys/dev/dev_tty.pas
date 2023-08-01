unit dev_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_conf;

procedure ttyconsdev_init(); //SYSINIT(tty, SI_SUB_DRIVERS, SI_ORDER_FIRST, ttyconsdev_init, nil);

implementation

uses
 errno,
 vfile,
 vstat,
 vuio,
 vttycom,
 subr_uio,
 vpoll,
 vm,
 sys_event,
 kern_event;

var
 dev_console:p_cdev;

{
 * Operations that are exposed through the character device in /dev.
 }
Function ttydev_open(dev:p_cdev;oflags,devtype:Integer):Integer;
begin
 Writeln('ttydev_open("',dev^.si_name,'",',oflags,',',devtype,')');
 Result:=0;
end;

Function ttydev_close(dev:p_cdev;fflag,devtype:Integer):Integer;
begin
 Result:=0;
end;

Function ttydev_read(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 error:Integer;
 len:QWORD;
 S:RawByteString;
begin
 Readln(S);

 len:=Length(S);
 if (len > uio^.uio_resid) then
 begin
  len:=uio^.uio_resid;
 end;

 error:=uiomove(PChar(S), len, uio);

 if (error=ENXIO) then error:=0;
 Exit(error);
end;

Function ttydev_write(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
var
 error:Integer;
 len:QWORD;
 S:RawByteString;
begin
 len:=uio^.uio_resid;
 SetLength(S,len);

 error:=uiomove(PChar(S), len, uio);

 SetLength(S,len-uio^.uio_resid);
 Write('[',dev^.si_name,']:',S);

 Exit(error);
end;

Function ttydev_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
var
 error:Integer;
begin
 error:=0;

 case (cmd) of
  TIOCCBRK,
  TIOCCONS,
  TIOCDRAIN,
  TIOCEXCL,
  TIOCFLUSH,
  TIOCNXCL,
  TIOCSBRK,
  TIOCSCTTY,
  TIOCSETA,
  TIOCSETAF,
  TIOCSETAW,
  TIOCSPGRP,
  TIOCSTART,
  TIOCSTAT,
  TIOCSTI,
  TIOCSTOP,
  TIOCSWINSZ:
   begin
    //error:=tty_wait_background(tp, curthread, SIGTTOU);
    //if (error) then goto done;
   end;
  else;
 end;

 Writeln('ttydev_ioctl("',dev^.si_name,'",0x',HexStr(cmd,8),',0x',HexStr(data),',0x',HexStr(fflag,8),')');
 //error:=tty_ioctl(tp, cmd, data, fflag, td);

 //done:

 Exit(error);
end;

Function ttydev_poll(dev:p_cdev;events:Integer):Integer;
var
 error:Integer;
 revents:Integer;
begin
 error:=0;
 revents:=0;

 //tty_lock(tp);

 if (error<>0) then
 begin
  Exit((events and (POLLIN or POLLRDNORM)) or POLLHUP);
 end;

 if ((events and (POLLIN or POLLRDNORM))<>0) then
 begin
  { See if we can read something. }
  //if (ttydisc_read_poll(tp) > 0) then
  //begin
  // revents:=revents or events and (POLLIN or POLLRDNORM);
  //end;
 end;

 //if (tp^.t_flags and TF_ZOMBIE) then
 //begin
 // { Hangup flag on zombie state. }
 // revents:=revents or POLLHUP;
 //end else
 if ((events and (POLLOUT or POLLWRNORM))<>0) then
 begin
  { See if we can write something. }
  //if (ttydisc_write_poll(tp) > 0) then
  //begin
  // revents:= or events and (POLLOUT or POLLWRNORM);
  //end;
 end;

 if (revents=0) then
 begin
  if ((events and (POLLIN or POLLRDNORM))<>0) then
  begin
   //selrecord(td, @tp^.t_inpoll);
  end;
  if ((events and (POLLOUT or POLLWRNORM))<>0) then
  begin
   //selrecord(td, @tp^.t_outpoll);
  end;
 end;

 //tty_unlock(tp);

 Exit(revents);
end;

Function ttydev_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
var
 error:Integer;
begin
 { Handle mmap() through the driver. }

 error:=EPERM;
 //error:=ttydevsw_mmap(tp, offset, paddr, nprot, memattr);

 Exit(error);
end;

{
 * kqueue support.
 }

procedure tty_kqops_read_detach(kn:p_knote);
begin
 //struct tty *tp:=kn^.kn_hook;
 //knlist_remove(@tp^.t_inpoll.si_note, kn, 0);
end;

function tty_kqops_read_event(kn:p_knote;hint:QWORD):Integer;
begin
 Result:=0;
 //struct tty *tp:=kn^.kn_hook;
 //
 //tty_lock_assert(tp, MA_OWNED);
 //
 //if (tty_gone(tp) or tp^.t_flags and TF_ZOMBIE) then
 //begin
 // kn^.kn_flags:=kn^.kn_flags or EV_EOF;
 // Exit(1);
 //end else
 //begin
 // kn^.kn_data:=ttydisc_read_poll(tp);
 // Exit(kn^.kn_data > 0);
 //end;
end;

procedure tty_kqops_write_detach(kn:p_knote);
begin
 //struct tty *tp:=kn^.kn_hook;
 //knlist_remove(@tp^.t_outpoll.si_note, kn, 0);
end;

function tty_kqops_write_event(kn:p_knote;hint:QWORD):Integer;
begin
 Result:=0;
 //struct tty *tp:=kn^.kn_hook;
 //
 //tty_lock_assert(tp, MA_OWNED);
 //
 //if (tty_gone(tp)) then
 //begin
 // kn^.kn_flags:=kn^.kn_flags or EV_EOF;
 // Exit(1);
 //end else
 //begin
 // kn^.kn_data:=ttydisc_write_poll(tp);
 // Exit(kn^.kn_data > 0);
 //end;
end;

const
 tty_kqops_read:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:nil;
  f_detach:@tty_kqops_read_detach;
  f_event :@tty_kqops_read_event;
  f_touch :nil;
 );

 tty_kqops_write:t_filterops=(
  f_isfd  :1;
  _align  :0;
  f_attach:nil;
  f_detach:@tty_kqops_write_detach;
  f_event :@tty_kqops_write_event;
  f_touch :nil;
 );

Function ttydev_kqfilter(dev:p_cdev;kn:p_knote):Integer;
var
 error:Integer;
begin
 error:=0;

 case (kn^.kn_filter) of
  EVFILT_READ:
   begin
    //kn^.kn_hook:=tp;
    kn^.kn_fop:=@tty_kqops_read;
    //knlist_add(@tp^.t_inpoll.si_note, kn, 1);
   end;
  EVFILT_WRITE:
   begin
    //kn^.kn_hook:=tp;
    kn^.kn_fop:=@tty_kqops_write;
    //knlist_add(@tp^.t_outpoll.si_note, kn, 1);
   end;
  else
   error:=EINVAL;
 end;

 Exit(error);
end;

const
 ttydev_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :D_TTY;
  d_name       :'ttydev';
  d_open       :@ttydev_open;
  d_fdopen     :nil;
  d_close      :@ttydev_close;
  d_read       :@ttydev_read;
  d_write      :@ttydev_write;
  d_ioctl      :@ttydev_ioctl;
  d_poll       :@ttydev_poll;
  d_mmap       :@ttydev_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :@ttydev_kqfilter;
  d_purge      :nil;
  d_mmap_single:nil;
 );

{
 * /dev/console handling.
 }

Function ttyconsdev_open(dev:p_cdev;oflags,devtype:Integer):Integer;
begin
 Writeln('ttyconsdev_open(',dev^.si_name,',',oflags,',',devtype,')');
 Result:=0;
end;

Function ttyconsdev_write(dev:p_cdev;uio:p_uio;ioflag:Integer):Integer;
begin
 Result:=ttydev_write(dev,uio,ioflag);
end;

const
 ttyconsdev_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :D_TTY;
  d_name       :'ttyconsdev';
  d_open       :@ttyconsdev_open;
  d_fdopen     :nil;
  d_close      :@ttydev_close;
  d_read       :@ttydev_read;
  d_write      :@ttyconsdev_write;
  d_ioctl      :@ttydev_ioctl;
  d_poll       :@ttydev_poll;
  d_mmap       :@ttydev_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :@ttydev_kqfilter;
  d_purge      :nil;
  d_mmap_single:nil;
 );

{
 * /dev/console is a little different than normal TTY's.  When opened,
 * it determines which TTY to use.  When data gets written to it, it
 * will be logged in the kernel message buffer.
 }

function tty_makedev(fmt:PChar;const Args:Array of const):p_cdev; register;
var
 dev:p_cdev;
 uid:uid_t;
 gid:gid_t;
 mode:mode_t;
begin
 // User device.
 uid :=0;
 gid :=GID_TTY;
 mode:=S_IRUSR or S_IWUSR or S_IWGRP;

 dev:=make_dev_cred(@ttydev_cdevsw, 0, uid, gid, mode, fmt, Args);

 Result:=dev;
end;

procedure ttyconsdev_init();
begin
 dev_console:=make_dev_credf(MAKEDEV_ETERNAL, @ttyconsdev_cdevsw, 0, UID_ROOT, GID_WHEEL, &600, 'console',[]);
 //
 tty_makedev('deci_stderr',[]);
 tty_makedev('deci_stdin' ,[]);
 tty_makedev('deci_stdout',[]);
 tty_makedev('deci_tty2'  ,[]);
 tty_makedev('deci_tty3'  ,[]);
 tty_makedev('deci_tty4'  ,[]);
 tty_makedev('deci_tty5'  ,[]);
 tty_makedev('deci_tty6'  ,[]);
 tty_makedev('deci_tty7'  ,[]);
 tty_makedev('deci_ttya0' ,[]);
 tty_makedev('deci_ttyb0' ,[]);
 tty_makedev('deci_ttyc0' ,[]);
end;


end.

