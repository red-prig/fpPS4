unit kern_descrip;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_thr,
 kern_id,
 vfile,
 vstat,
 vuio,
 vcapability,
 vfcntl,
 vfs_vnode;

const
 FGET_GETCAP=$00000001;

function  falloc(resultfp:pp_file;resultfd:PInteger;flags:Integer):Integer;

procedure fhold(fp:p_file);
procedure fdrop(fp:p_file);

function  fget(fd:Integer;
               rights:cap_rights_t;
               fpp:pp_file):Integer;

function  fget_mmap(fd:Integer;
                    rights:cap_rights_t;
                    maxprotp:PByte;
                    fpp:pp_file):Integer;

function  fget_read(fd:Integer;
                    rights:cap_rights_t;
                    fpp:pp_file):Integer;

function  fget_write(fd:Integer;
                     rights:cap_rights_t;
                     fpp:pp_file):Integer;

function  fgetcap(fd:Integer;
                  fpp:pp_file):Integer;

function  fgetvp(fd:Integer;
                 rights:cap_rights_t;
                 vpp:pp_vnode):Integer;

function  fgetvp_rights(fd:Integer;
                        need:cap_rights_t;
                        have:p_cap_rights_t;
                        vpp:pp_vnode):Integer;

function  fgetvp_read(fd:Integer;
                      rights:cap_rights_t;
                      vpp:pp_vnode):Integer;

function  fgetvp_exec(fd:Integer;
                      rights:cap_rights_t;
                      vpp:pp_vnode):Integer;

function  fgetvp_write(fd:Integer;
                       rights:cap_rights_t;
                       vpp:pp_vnode):Integer;

function  fgetsock(fd:Integer;
                   rights:cap_rights_t;
                   spp:PPointer; //socket **
                   fflagp:PDWORD):Integer;

implementation

uses
 errno,
 vfiledesc,
 sys_capability,
 vfs_subr;

function badfo_readwrite(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Exit(EBADF);
end;

function badfo_truncate(fp:p_file;length:Int64):Integer;
begin
 Exit(EINVAL);
end;

function badfo_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin
 Exit(EBADF);
end;

function badfo_poll(fp:p_file;events:Integer):Integer;
begin
 Exit(0);
end;

function badfo_kqfilter(fp:p_file;kn:Pointer):Integer;
begin
 Exit(EBADF);
end;

function badfo_stat(fp:p_file;sb:p_stat):Integer;
begin
 Exit(EBADF);
end;

function badfo_close(fp:p_file):Integer;
begin
 Exit(EBADF);
end;

function badfo_chmod(fp:p_file;mode:mode_t):Integer;
begin
 Exit(EBADF);
end;

function badfo_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;
begin
 Exit(EBADF);
end;

const
 badfileops:fileops=(
  fo_read    :@badfo_readwrite;
  fo_write   :@badfo_readwrite;
  fo_truncate:@badfo_truncate;
  fo_ioctl   :@badfo_ioctl;
  fo_poll    :@badfo_poll;
  fo_kqfilter:@badfo_kqfilter;
  fo_stat    :@badfo_stat;
  fo_close   :@badfo_close;
  fo_chmod   :@badfo_chmod;
  fo_chown   :@badfo_chown;
  fo_flags   :0
 );

procedure _fdrop(data:pointer); forward;

function falloc_noinstall(resultfp:pp_file):Integer;
var
 fp:p_file;
begin
 Assert(resultfp<>nil,'resultfp=nil');

 System.InterlockedIncrement(openfiles);

 fp:=AllocMem(SizeOf(t_file));
 if (fp=nil) then Exit(ENOMEM);

 fp^.desc.free:=@_fdrop;
 fp^.f_ops    :=@badfileops;
 fp^.f_data   :=nil;
 fp^.f_vnode  :=nil;

 resultfp^:=fp;
 Exit(0);
end;

function finstall(fp:p_file;fd:PInteger;flags:Integer):Integer;
begin
 Assert(fd<>nil,'fd=nil');
 Assert(fp<>nil,'fp=nil');

 if not id_new(@fd_table.fd_ofiles,@fp^.desc,fd) then
 begin
  Exit(ENFILE);
 end;

 //if ((flags and O_CLOEXEC)<>0)

 Exit(0);
end;

function falloc(resultfp:pp_file;resultfd:PInteger;flags:Integer):Integer;
var
 fp:p_file;
 error,fd:Integer;
begin
 error:=falloc_noinstall(@fp);
 if (error<>0) then
  Exit(error);  { no reference held on error }

 error:=finstall(fp,@fd,flags);
 if (error<>0) then
 begin
  _fdrop(fp);  { one reference (fp only) }
  Exit(error);
 end;

 if (resultfp<>nil) then
  resultfp^:=fp  { copy out result }
 else
  fdrop(fp);  { release local reference }

 if (resultfd<>nil) then
  resultfd^:=fd;

 Exit(0);
end;

procedure _fdrop(data:pointer);
var
 fp:p_file;
begin
 fp:=data;

 if (fp^.f_ops<>@badfileops) then
 begin
  fo_close(fp);
 end;

 System.InterlockedDecrement(openfiles);

 //FreeMem(fp^.f_advice);
 FreeMem(fp);
end;

procedure fhold(fp:p_file);
begin
 id_acqure(@fp^.desc);
end;

procedure fdrop(fp:p_file);
begin
 id_release(@fp^.desc);
end;

function fget_unlocked(fd:Integer):p_file;
begin
 if (fd<0) then Exit(nil);
 Result:=p_file(id_get(@fd_table.fd_ofiles,fd));
end;

function _fget(fd:Integer;
               fpp:pp_file;
               flags:Integer;
               needrights:cap_rights_t;
               haverightsp:p_cap_rights_t;
               maxprotp:PByte;
               fget_flags:Integer):Integer;
var
 fp:p_file;
 fp_fromcap:p_file;
 error:Integer;
begin
 fpp^:=nil;

 fp:=fget_unlocked(fd);
 if (fp=nil) then
  Exit(EBADF);

 if (fp^.f_ops=@badfileops) then
 begin
  fdrop(fp);
  Exit(EBADF);
 end;

 {
  * If this is a capability, what rights does it have?
  }
 if (haverightsp<>nil) then
 begin
  if (fp^.f_type=DTYPE_CAPABILITY) then
   haverightsp^:=cap_rights(fp)
  else
   haverightsp^:=CAP_MASK_VALID;
 end;

 {
  * If a capability has been requested, return the capability directly.
  * Otherwise, check capability rights, extract the underlying object,
  * and check its access flags.
  }
 if ((fget_flags and FGET_GETCAP)<>0) then
 begin
  if (fp^.f_type<>DTYPE_CAPABILITY) then
  begin
   fdrop(fp);
   Exit(EINVAL);
  end;
 end else
 begin
  if (maxprotp=nil) then
   error:=cap_funwrap(fp, needrights,@fp_fromcap)
  else
   error:=cap_funwrap_mmap(fp, needrights, maxprotp,@fp_fromcap);

  if (error<>0) then
  begin
   fdrop(fp);
   Exit(error);
  end;

  {
   * If we've unwrapped a file, drop the original capability
   * and hold the new descriptor.  fp after this point refers to
   * the actual (unwrapped) object, not the capability.
   }
  if (fp<>fp_fromcap) then
  begin
   fhold(fp_fromcap);
   fdrop(fp);
   fp:=fp_fromcap;
  end;
 end;

 {
  * FREAD and FWRITE failure return EBADF as per POSIX.
  }
 error:=0;

 case (flags) of
  FREAD,
  FWRITE:
   begin
    if ((fp^.f_flag and flags)=0) then
     error:=EBADF;
   end;
  FEXEC:
   begin
    if ((fp^.f_flag and (FREAD or FEXEC))=0) or
       ((fp^.f_flag and FWRITE)<>0) then
    error:=EBADF;
   end;
  0:;
 else
  Assert(false,'wrong flags');
 end;

 if (error<>0) then
 begin
  fdrop(fp);
  Exit(error);
 end;

 fpp^:=fp;
 Exit(0);
end;

function fget(fd:Integer;
              rights:cap_rights_t;
              fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, 0, rights, nil, nil, 0));
end;

function fget_mmap(fd:Integer;
                   rights:cap_rights_t;
                   maxprotp:PByte;
                   fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, 0, rights, nil, maxprotp, 0));
end;

function fget_read(fd:Integer;
                   rights:cap_rights_t;
                   fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, FREAD, rights, nil, nil, 0));
end;

function fget_write(fd:Integer;
                    rights:cap_rights_t;
                    fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, FWRITE, rights, nil, nil, 0));
end;

{
 * Unlike the other fget() calls, which accept and check capability rights
 * but never Exitcapabilities, fgetcap() Exits the capability but doesn't
 * check capability rights.
}
function fgetcap(fd:Integer;
                 fpp:pp_file):Integer;
begin
 Exit(_fget(fd, fpp, 0, 0, nil, nil, FGET_GETCAP));
end;


{
 * Like fget() but loads the underlying vnode, or Exits an error if the
 * descriptor does not represent a vnode.  Note that pipes use vnodes but
 * never have VM objects.  The Exited vnode will be vref()'d.
 *
 * XXX: what about the unused flags ?
}
function _fgetvp(fd:Integer;
                 flags:Integer;
                 needrights:cap_rights_t;
                 haverightsp:p_cap_rights_t;
                 vpp:pp_vnode):Integer;
var
 fp:p_file;
 error:Integer;
begin
 vpp^:=nil;
 error:=_fget(fd, @fp, flags, needrights, haverightsp,nil, 0);
 if (error<>0) then
  Exit(error);
 if (fp^.f_vnode=nil) then
 begin
  error:=EINVAL;
 end else
 begin
  vpp^:=fp^.f_vnode;
  vref(vpp^);
 end;
 fdrop(fp);

 Exit(error);
end;

function fgetvp(fd:Integer;
                rights:cap_rights_t;
                vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, 0, rights, nil, vpp));
end;

function fgetvp_rights(fd:Integer;
                       need:cap_rights_t;
                       have:p_cap_rights_t;
                       vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, 0, need, have, vpp));
end;

function fgetvp_read(fd:Integer;
                     rights:cap_rights_t;
                     vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, FREAD, rights, nil, vpp));
end;

function fgetvp_exec(fd:Integer;
                     rights:cap_rights_t;
                     vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, FEXEC, rights, nil, vpp));
end;

function fgetvp_write(fd:Integer;
                      rights:cap_rights_t;
                      vpp:pp_vnode):Integer;
begin
 Exit(_fgetvp(fd, FWRITE, rights, nil, vpp));
end;

{
 * Like fget() but loads the underlying socket, or Exits an error if the
 * descriptor does not represent a socket.
 *
 * We bump the ref count on the Exited socket.  XXX Also obtain the SX lock
 * in the future.
 *
 * Note: fgetsock() and fputsock() are deprecated, as consumers should rely
 * on their file descriptor reference to prevent the socket from being free'd
 * during use.
}
function fgetsock(fd:Integer;
                  rights:cap_rights_t;
                  spp:PPointer; //socket **
                  fflagp:PDWORD):Integer;
var
 fp:p_file;
 error:Integer;
begin
 spp^:=nil;
 if (fflagp<>nil) then
  fflagp^:=0;
 error:=_fget(fd, @fp, 0, rights, nil, nil, 0);
 if (error<>0) then
  Exit(error);
 if (fp^.f_type<>DTYPE_SOCKET) then
 begin
  error:=ENOTSOCK;
 end else
 begin
  spp^:=fp^.f_data;
  if (fflagp<>nil) then
   fflagp^:=fp^.f_flag;
  //SOCK_LOCK(spp^);
  //soref(spp^);
  //SOCK_UNLOCK(spp^);
 end;
 fdrop(fp);

 Exit(error);
end;

end.

