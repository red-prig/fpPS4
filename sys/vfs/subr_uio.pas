unit subr_uio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vuio,
 systm,
 kern_thr;

function uiomove(cp:Pointer;n:Integer;uio:p_uio):Integer;
function uiomove_nofault(cp:Pointer;n:Integer;uio:p_uio):Integer;
function uiomove_faultflag(cp:Pointer;n:Integer;uio:p_uio;nofault:Integer):Integer;

function copyinuio(iovp:p_iovec;iovcnt:DWORD;uiop:pp_uio):Integer;

implementation

uses
 errno,
 kern_synch;

function uiomove(cp:Pointer;n:Integer;uio:p_uio):Integer;
begin
 Exit(uiomove_faultflag(cp, n, uio, 0));
end;

function uiomove_nofault(cp:Pointer;n:Integer;uio:p_uio):Integer;
begin
 Exit(uiomove_faultflag(cp, n, uio, 1));
end;

function uiomove_faultflag(cp:Pointer;n:Integer;uio:p_uio;nofault:Integer):Integer;
label
 _out;
var
 td:p_kthread;
 iov:p_iovec;
 cnt:QWORD;
 error, newflags, save:Integer;
begin
 td:=curkthread;
 error:=0;

 Assert((uio^.uio_rw=UIO_READ) or (uio^.uio_rw=UIO_WRITE),'uiomove: mode');
 Assert((uio^.uio_segflg<>UIO_USERSPACE) or (uio^.uio_td=td),'uiomove proc');

 // XXX does it make a sense to set TDP_DEADLKTREAT for UIO_SYSSPACE ?
 newflags:=TDP_DEADLKTREAT;
 if (uio^.uio_segflg=UIO_USERSPACE) and (nofault<>0) then
 begin
  {
   * Fail if a non-spurious page fault occurs.
  }
  newflags:=newflags or TDP_NOFAULTING or TDP_RESETSPUR;
 end;
 save:=curthread_pflags_set(newflags);

 while (n > 0 and uio^.uio_resid) do
 begin
  iov:=uio^.uio_iov;
  cnt:=iov^.iov_len;
  if (cnt=0) then
  begin
   Inc(uio^.uio_iov);
   Dec(uio^.uio_iovcnt);
   continue;
  end;
  if (cnt > n) then
   cnt:=n;

  case (uio^.uio_segflg) of
   UIO_USERSPACE:
    begin
     maybe_yield();
     if (uio^.uio_rw=UIO_READ) then
      error:=copyout(cp, iov^.iov_base, cnt)
     else
      error:=copyin(iov^.iov_base, cp, cnt);
     if (error<>0) then
      goto _out;
    end;
   UIO_SYSSPACE:
    begin
     if (uio^.uio_rw=UIO_READ) then
      Move(cp^, iov^.iov_base^, cnt)
     else
      Move(iov^.iov_base^, cp^, cnt);
     break;
    end;
   UIO_NOCOPY:;
   else;
  end;

  iov^.iov_base:=iov^.iov_base + cnt;
  Dec(iov^.iov_len,cnt);
  Dec(uio^.uio_resid,cnt);
  Inc(uio^.uio_offset,cnt);
  cp:=cp + cnt;
  Dec(n,cnt);
 end;

_out:
 curthread_pflags_restore(save);
 Exit(error);
end;

function copyinuio(iovp:p_iovec;iovcnt:DWORD;uiop:pp_uio):Integer;
var
 iov:p_iovec;
 uio:p_uio;
 iovlen:DWORD;
 error,i:Integer;
begin
 uiop^:=nil;
 if (iovcnt > UIO_MAXIOV) then
  Exit(EINVAL);
 iovlen:=iovcnt * sizeof (iovec);
 uio:=AllocMem(iovlen + sizeof(t_uio));
 iov:=p_iovec(uio + 1);
 error:=copyin(iovp, iov, iovlen);
 if (error<>0) then
 begin
  FreeMem(uio);
  Exit(error);
 end;
 uio^.uio_iov   :=iov;
 uio^.uio_iovcnt:=iovcnt;
 uio^.uio_segflg:=UIO_USERSPACE;
 uio^.uio_offset:=-1;
 uio^.uio_resid :=0;

 For i:=0 to iovcnt-1 do
 begin
  if (iov^.iov_len > IOSIZE_MAX - uio^.uio_resid) then
  begin
   FreeMem(uio);
   Exit(EINVAL);
  end;
  Inc(uio^.uio_resid,iov^.iov_len);
  Inc(iov);
 end;

 uiop^:=uio;
 Exit(0);
end;

end.

