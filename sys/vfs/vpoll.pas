unit vpoll;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 nfds_t=DWORD;

 {
  * This structure is passed as an array to poll(2).
  }
 p_pollfd=^t_pollfd;
 t_pollfd=packed record
  fd     :Integer; { which file descriptor to poll }
  events :Word;    { events we are interested in }
  revents:Word;    { events found on return }
 end;

const
{
 * Requestable events.  If poll(2) finds any of these set, they are
 * copied to revents on return.
 * XXX Note that FreeBSD doesn't make much distinction between POLLPRI
 * and POLLRDBAND since none of the file types have distinct priority
 * bands - and only some have an urgent "mode".
 * XXX Note POLLIN isn't really supported in true SVSV terms.  Under SYSV
 * POLLIN includes all of normal, band and urgent data.  Most poll handlers
 * on FreeBSD only treat it as "normal" data.
 }
 POLLIN    =$0001;   { any readable data available }
 POLLPRI   =$0002;   { OOB/Urgent readable data }
 POLLOUT   =$0004;   { file descriptor is writeable }
 POLLRDNORM=$0040;   { non-OOB/URG data available }
 POLLWRNORM=POLLOUT; { no write type differentiation }
 POLLRDBAND=$0080;   { OOB/Urgent readable data }
 POLLWRBAND=$0100;   { OOB/Urgent data can be written }

{ General FreeBSD extension (currently only supported for sockets): }
 POLLINIGNEOF=$2000; { like POLLIN, except ignore EOF }

{
 * These events are set if they occur regardless of whether they were
 * requested.
 }
 POLLERR =$0008;  { some poll error occurred }
 POLLHUP =$0010;  { file descriptor was "hung up" }
 POLLNVAL=$0020;  { requested events "invalid" }

 POLLSTANDARD=(POLLIN or POLLPRI or POLLOUT or POLLRDNORM or POLLRDBAND or
               POLLWRBAND or POLLERR or POLLHUP or POLLNVAL);

{
 * Request that poll() wait forever.
 * XXX in SYSV, this is defined in stropts.h, which is not included
 * by poll.h.
 }
 INFTIM=-1;

implementation

end.

