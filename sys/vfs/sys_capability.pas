unit sys_capability;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vcapability,
 vfile,
 vuio,
 vstat,
 vm;

function sys_cap_new():Integer;
function sys_cap_getrights():Integer;
function sys_cap_enter():Integer;
function sys_cap_getmode():Integer;

{
 * struct capability describes a capability, and is hung off of its struct
 * file f_data field.  cap_file and cap_rightss are static once hooked up, as
 * neither the object it references nor the rights it encapsulates are
 * permitted to change.
}
type
 p_capability=^t_capability;
 t_capability=packed record
  cap_object:Pointer;      { Underlying object's file. }
  cap_file  :Pointer;      { Back-pointer to cap's file. }
  cap_rights:cap_rights_t; { Mask of rights on object. }
 end;

function cap_check(c:p_capability;rights:cap_rights_t):Integer;
function cap_rights(fp_cap:p_file):cap_rights_t;
function cap_funwrap(fp_cap:p_file;rights:cap_rights_t;fpp:pp_file):Integer;
function cap_funwrap_mmap(fp_cap:p_file;rights:cap_rights_t;maxprotp:PByte;fpp:pp_file):Integer;

function kern_capwrap(fp:p_file;rights:cap_rights_t;capfdp:PInteger):Integer;

function capability_close(fp:p_file):Integer;
function capability_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
function capability_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
function capability_truncate(fp:p_file;length:Int64):Integer;
function capability_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
function capability_poll(fp:p_file;events:Integer):Integer;
function capability_kqfilter(fp:p_file;kn:Pointer):Integer;
function capability_stat(fp:p_file;sb:p_stat):Integer;
function capability_chmod(fp:p_file;mode:mode_t):Integer;
function capability_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;

const
 capability_ops:fileops=(
  fo_read    :@capability_read;
  fo_write   :@capability_write;
  fo_truncate:@capability_truncate;
  fo_ioctl   :@capability_ioctl;
  fo_poll    :@capability_poll;
  fo_kqfilter:@capability_kqfilter;
  fo_stat    :@capability_stat;
  fo_close   :@capability_close;
  fo_chmod   :@capability_chmod;
  fo_chown   :@capability_chown;
  fo_flags   :DFLAG_PASSABLE
 );

 capability_ops_unpassable:fileops=(
  fo_read    :@capability_read;
  fo_write   :@capability_write;
  fo_truncate:@capability_truncate;
  fo_ioctl   :@capability_ioctl;
  fo_poll    :@capability_poll;
  fo_kqfilter:@capability_kqfilter;
  fo_stat    :@capability_stat;
  fo_close   :@capability_close;
  fo_chmod   :@capability_chmod;
  fo_chown   :@capability_chown;
  fo_flags   :0
 );

implementation

uses
 errno,
 kern_descrip;

{
 * Stub Capability functions for when options CAPABILITIES isn't compiled
 * into the kernel.
 }
function sys_cap_new():Integer;
begin
 Exit(ENOSYS);
end;

function sys_cap_getrights():Integer;
begin
 Exit(ENOSYS);
end;

function sys_cap_enter():Integer;
begin
 Exit(ENOSYS);
end;

function sys_cap_getmode():Integer;
begin
 Exit(ENOSYS);
end;

function cap_check(c:p_capability;rights:cap_rights_t):Integer;
begin
 if ((c^.cap_rights or rights)<>c^.cap_rights) then
  Exit(ENOTCAPABLE);
 Exit(0);
end;

{
 * Extract rights from a capability for monitoring purposes -- not for use in
 * any other way, as we want to keep all capability permission evaluation in
 * this one file.
 }
function cap_rights(fp_cap:p_file):cap_rights_t;
var
 c:p_capability;
begin
 Assert(fp_cap^.f_type=DTYPE_CAPABILITY,'cap_rights: !capability');

 c:=fp_cap^.f_data;
 Exit(c^.cap_rights);
end;

function cap_funwrap(fp_cap:p_file;rights:cap_rights_t;fpp:pp_file):Integer;
var
 c:p_capability;
 error:Integer;
begin
 if (fp_cap^.f_type<>DTYPE_CAPABILITY) then
 begin
  fpp^:=fp_cap;
  Exit(0);
 end;
 c:=fp_cap^.f_data;
 error:=cap_check(c, rights);
 if (error<>0) then
  Exit(error);
 fpp^:=c^.cap_object;
 Exit(0);
end;

{
 * Slightly different routine for memory mapping file descriptors: unwrap the
 * capability and check CAP_MMAP, but also Exita bitmask representing the
 * maximum mapping rights the capability allows on the object.
 }
function cap_funwrap_mmap(fp_cap:p_file;rights:cap_rights_t;maxprotp:PByte;fpp:pp_file):Integer;
var
 c:p_capability;
 maxprot:Byte;
 error:Integer;
begin
 if (fp_cap^.f_type<>DTYPE_CAPABILITY) then
 begin
  fpp^:=fp_cap;
  maxprotp^:=VM_PROT_ALL;
  Exit(0);
 end;
 c:=fp_cap^.f_data;
 error:=cap_check(c, rights or CAP_MMAP);
 if (error<>0) then
  Exit(error);
 fpp^:=c^.cap_object;
 maxprot:=0;
 if (c^.cap_rights and CAP_READ)<>0 then
  maxprot:=maxprot or VM_PROT_READ;
 if (c^.cap_rights and CAP_WRITE)<>0 then
  maxprot:=maxprot or VM_PROT_WRITE;
 if (c^.cap_rights and CAP_MAPEXEC)<>0 then
  maxprot:=maxprot or VM_PROT_EXECUTE;
 maxprotp^:=maxprot;
 Exit(0);
end;

{
 * Create a capability to wrap around an existing file.
 }
function kern_capwrap(fp:p_file;rights:cap_rights_t;capfdp:PInteger):Integer;
var
 cp,cp_old:p_capability;
 fp_object,fcapp:p_file;
 error:Integer;
begin
 if ((rights or CAP_MASK_VALID)<>CAP_MASK_VALID) then
  Exit(EINVAL);

 {
  * If a new capability is being derived from an existing capability,
  * then the new capability rights must be a subset of the existing
  * rights.
  }
 if (fp^.f_type=DTYPE_CAPABILITY) then
 begin
  cp_old:=fp^.f_data;
  if ((cp_old^.cap_rights or rights)<>cp_old^.cap_rights) then
   Exit(ENOTCAPABLE);
 end;

 {
  * Allocate a new file descriptor to hang the capability off of.
  }
 error:=falloc(@fcapp, capfdp, fp^.f_flag);
 if (error<>0) then
  Exit(error);

 {
  * Rather than nesting capabilities, directly reference the object an
  * existing capability references.  There's nothing else interesting
  * to preserve for future use, as we've incorporated the previous
  * rights mask into the new one.  This prevents us from having to
  * deal with capability chains.
  }
 if (fp^.f_type=DTYPE_CAPABILITY) then
  fp_object:=p_capability(fp^.f_data)^.cap_object
 else
  fp_object:=fp;
 fhold(fp_object);
 cp:=AllocMem(SizeOf(t_capability));
 cp^.cap_rights:=rights;
 cp^.cap_object:=fp_object;
 cp^.cap_file:=fcapp;

 if ((fp^.f_flag and DFLAG_PASSABLE)<>0) then
  finit(fcapp, fp^.f_flag, DTYPE_CAPABILITY, cp, @capability_ops)
 else
  finit(fcapp, fp^.f_flag, DTYPE_CAPABILITY, cp, @capability_ops_unpassable);

 {
  * Release our private reference (the proc filedesc still has one).
  }
 fdrop(fcapp);
 Exit(0);
end;

{
 * When a capability is closed, simply drop the reference on the underlying
 * object and free the capability.  fdrop() will handle the case where the
 * underlying object also needs to close, and the caller will have already
 * performed any object-specific lock or mqueue handling.
 }
function capability_close(fp:p_file):Integer;
var
 c:p_capability;
 fp_object:p_file;
begin
 Assert(fp^.f_type=DTYPE_CAPABILITY,('capability_close: !capability'));

 c:=fp^.f_data;
 fp^.f_ops:=@badfileops;
 fp^.f_data:=nil;
 fp_object:=c^.cap_object;
 FreeMem(c);

 Exit(fdrop(fp_object));
end;

{
 * In general, file descriptor operations should never make it to the
 * capability, only the underlying file descriptor operation vector, so panic
 * if we do turn up here.
 }
function capability_read(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Assert(False,'capability_read');
 Exit(ENOSYS);
end;

function capability_write(fp:p_file;uio:p_uio;flags:Integer):Integer;
begin
 Assert(False,'capability_write');
 Exit(ENOSYS);
end;

function capability_truncate(fp:p_file;length:Int64):Integer;
begin
 Assert(False,'capability_truncate');
 Exit(ENOSYS);
end;

function capability_ioctl(fp:p_file;com:QWORD;data:Pointer):Integer;
begin
 Assert(False,'capability_ioctl');
 Exit(ENOSYS);
end;

function capability_poll(fp:p_file;events:Integer):Integer;
begin
 Assert(False,'capability_poll');
 Exit(ENOSYS);
end;

function capability_kqfilter(fp:p_file;kn:Pointer):Integer;
begin
 Assert(False,'capability_kqfilter');
 Exit(ENOSYS);
end;

function capability_stat(fp:p_file;sb:p_stat):Integer;
begin
 Assert(False,'capability_stat');
 Exit(ENOSYS);
end;

function capability_chmod(fp:p_file;mode:mode_t):Integer;
begin
 Assert(False,'capability_chmod');
 Exit(ENOSYS);
end;

function capability_chown(fp:p_file;uid:uid_t;gid:gid_t):Integer;
begin
 Assert(False,'capability_chown');
 Exit(ENOSYS);
end;



end.

