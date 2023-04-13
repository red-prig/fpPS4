unit sys_capability;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 errno,
 vcapability,
 vfile,
 vm;

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

implementation

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

end.

