unit kern_sysctl;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface


const
 CTL_MAXNAME=24; // largest number of components supported

//Top-level identifiers
 CTL_UNSPEC  = 0; // unused
 CTL_KERN    = 1; // "high kernel": proc, limits
 CTL_VM      = 2; // virtual memory
 CTL_VFS     = 3; // filesystem, mount type is next
 CTL_NET     = 4; // network, see socket.h
 CTL_DEBUG   = 5; // debugging parameters
 CTL_HW      = 6; // generic cpu/io
 CTL_MACHDEP = 7; // machine dependent
 CTL_USER    = 8; // user-level
 CTL_P1003_1B= 9; // POSIX 1003.1B
 CTL_MAXID   =10; // number of valid top-level ids


//CTL_KERN identifiers
 KERN_PROC   =14;

 KERN_ARND   =37;

//KERN_PROC subtypes
 KERN_PROC_APPINFO     =35; //Application information
 KERN_PROC_SDK_VERSION =36; //SDK version of the executable file
 KERN_PROC_IDTABLE     =37; //ID table information

 KERN_PROC_TEXT_SEGMENT=44; //kern_dynlib_get_libkernel_text_segment




//SYSCTL_HANDLER_ARGS oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req

type
 p_sysctl_req=^t_sysctl_req;

 t_sysctl_func=function(req:p_sysctl_req;p:Pointer;s:QWORD):Integer;

 t_sysctl_req=record
  td      :Pointer; //p_kthread
  lock    :Integer;
  oldptr  :Pointer;
  oldlen  :QWORD;
  oldidx  :QWORD;
  oldfunc :t_sysctl_func;
  newptr  :Pointer;
  newlen  :QWORD;
  newidx  :QWORD;
  newfunc :t_sysctl_func;
  validlen:QWORD;
  flags   :Integer;
 end;

 p_sysctl_oid=^t_sysctl_oid;

 t_oid_handler=function(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;

 t_sysctl_oid=record
  oid_handler:t_oid_handler;
 end;

function sys___sysctl(name   :PInteger;
                      namelen:DWORD;
                      old    :Pointer;
                      oldlenp:PQWORD;
                      new    :Pointer;
                      newlen :QWORD):Integer;

procedure sysctl_register_all(); //SYSINIT(sysctl, SI_SUB_KMEM, SI_ORDER_ANY, sysctl_register_all, 0);

implementation

uses
 errno,
 systm,
 vmparam,
 kern_thr,
 kern_sx,
 md_arc4random;

var
 sysctllock   :t_sx;
 sysctlmemlock:t_sx;

procedure sysctl_register_all();
begin
 sx_init(@sysctlmemlock, 'sysctl mem');
 sx_init(@sysctllock   , 'sysctl lock');
end;

procedure SYSCTL_XLOCK(); inline;
begin
 sx_xlock(@sysctllock)
end;

procedure SYSCTL_XUNLOCK(); inline;
begin
 sx_xunlock(@sysctllock);
end;

procedure SYSCTL_ASSERT_XLOCKED(); inline;
begin
 sx_assert(@sysctllock)
end;

function SYSCTL_IN(req:p_sysctl_req;p:Pointer;s:QWORD):Integer; inline;
begin
 Result:=req^.newfunc(req,p,s);
end;

function SYSCTL_OUT(req:p_sysctl_req;p:Pointer;s:QWORD):Integer; inline;
begin
 Result:=req^.oldfunc(req,p,s);
end;

function SYSCTL_HANDLE(noid:p_sysctl_oid;func:Pointer):Integer; inline;
begin
 noid^.oid_handler:=t_oid_handler(func);
 Result:=0
end;

//Transfer function to/from user space.
function sysctl_old_user(req:p_sysctl_req;p:Pointer;l:QWORD):Integer;
var
 i,len,origidx:QWORD;
 error:Integer;
begin
 origidx:=req^.oldidx;
 Inc(req^.oldidx,l);

 if (req^.oldptr=nil) then
 begin
  Exit(0);
 end;

 i:=l;
 len:=req^.validlen;
 if (len <= origidx) then
 begin
  i:=0;
 end else
 begin
  if (i > len - origidx) then
  begin
   i:=len - origidx;
  end;
  //if (req^.lock=REQ_WIRED) then
  //begin
  // error:=copyout_nofault(p, req^.oldptr + origidx, i);
  //end else
  begin
   error:=copyout(p, req^.oldptr + origidx, i);
  end;
  if (error<>0) then
  begin
   Exit(error);
  end;
 end;
 if (i < l) then
 begin
  Exit(ENOMEM);
 end;
 Exit(0);
end;

function sysctl_new_user(req:p_sysctl_req;p:Pointer;l:QWORD):Integer;
var
 error:Integer;
begin
 if (req^.newptr=nil) then
 begin
  Exit(0);
 end;

 if ((req^.newlen - req^.newidx) < l) then
 begin
  Exit(EINVAL);
 end;

 error:=copyin(req^.newptr + req^.newidx, p, l);

 Inc(req^.newidx,l);
 Exit(error);
end;

//Exit(ENOTDIR);
//Exit(ENOENT);

function sysctl_kern_proc_idtable(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
begin
 //get idtable key count
 Exit(ENOENT); //sceSblACMgrIsSystemUcred
end;

function sysctl_kern_arandom(oidp:p_sysctl_oid;arg1:Pointer;arg2:ptrint;req:p_sysctl_req):Integer;
var
 len:Integer;
 data:array[0..254] of Byte;
begin
 len:=256;
 if (req^.oldlen < 256)  then
 begin
  len:=req^.oldlen;
 end;

 arc4rand(@data,len,0);

 Result:=SYSCTL_OUT(req,@data,len);
end;

function sysctl_kern_proc(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 Writeln(StdErr,'sysctl_kern_proc:',name[0]);
end;

function sysctl_kern(name:PInteger;namelen:DWORD;noid:p_sysctl_oid;req:p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOTDIR);
 Result:=ENOENT;

 case name[0] of
  KERN_PROC:Result:=sysctl_kern_proc(name+1,namelen-1,noid,req);

  KERN_ARND:Result:=SYSCTL_HANDLE(noid,@sysctl_kern_arandom);
  else
   begin
    Writeln(StdErr,'Unhandled sysctl_kern:',name[0]);
   end;
 end;
end;

function sysctl_find_oid(name   :PInteger;
                         namelen:DWORD;
                         noid   :p_sysctl_oid;
                         req    :p_sysctl_req):Integer;
begin
 if (namelen=0) then Exit(ENOENT);
 Result:=ENOENT;

 case name[0] of
  CTL_KERN:Result:=sysctl_kern(name+1,namelen-1,noid,req);
  else
   begin
    Writeln(StdErr,'Unhandled sysctl_find_oid:',name[0]);
   end;
 end;
end;

function sysctl_root(oidp:p_sysctl_oid;
                     arg1:PInteger;
                     arg2:DWORD;
                     req :p_sysctl_req):Integer;
var
 oid:t_sysctl_oid;
begin
 oid:=Default(t_sysctl_oid);

 Result:=sysctl_find_oid(arg1, arg2, @oid, req);
 if (Result<>0) then Exit;

 if (oid.oid_handler=nil) then Exit(EINVAL);

 //if ((oid.oid_kind and CTLTYPE)=CTLTYPE_NODE) then
 //begin
 // arg1:=arg1 + indx;
 // arg2:=arg2 - indx;
 //end else
 //begin
 // arg1:=oid.oid_arg1;
 // arg2:=oid.oid_arg2;
 //end;

 Result:=oid.oid_handler(@oid, arg1, arg2, req);

end;

function userland_sysctl(name    :PInteger;
                         namelen :DWORD;
                         old     :Pointer;
                         oldlenp :PQWORD;
                         inkernel:Integer;
                         new     :Pointer;
                         newlen  :QWORD;
                         retval  :PQWORD;
                         flags   :Integer):Integer;
var
 error,memlocked:Integer;
 req:t_sysctl_req;
begin
 error:=0;

 req:=Default(t_sysctl_req);

 req.td   :=curkthread;
 req.flags:=flags;

 if (oldlenp<>nil) then
 begin
  if (inkernel<>0) then
  begin
   req.oldlen:=oldlenp^;
  end else
  begin
   error:=copyin(oldlenp, @req.oldlen, sizeof(Pointer));
   if (error<>0) then Exit(error);
  end;
 end;
 req.validlen:=req.oldlen;

 if (old<>nil) then
 begin
  //if (!useracc(old, req.oldlen, VM_PROT_WRITE))
  // Exit(EFAULT);
  req.oldptr:=old;
 end;

 if (new<>nil) then
 begin
  //if (!useracc(new, newlen, VM_PROT_READ))
  // Exit(EFAULT);
  req.newlen:=newlen;
  req.newptr:=new;
 end;

 req.oldfunc:=@sysctl_old_user;
 req.newfunc:=@sysctl_new_user;
 //req.lock:=REQ_UNWIRED;

 if (req.oldlen > PAGE_SIZE) then
 begin
  memlocked:=1;
  sx_xlock(@sysctlmemlock);
 end else
 begin
  memlocked:=0;
 end;

 repeat
  req.oldidx:=0;
  req.newidx:=0;
  SYSCTL_XLOCK();
  error:=sysctl_root(nil, name, namelen, @req);
  SYSCTL_XUNLOCK();
  if (error<>EAGAIN) then
  begin
   break;
  end;
  //kern_yield(PRI_USER);
 until false;

 //if (req.lock=REQ_WIRED) and (req.validlen > 0) then
 //begin
 // vsunlock(req.oldptr, req.validlen);
 //end;

 if (memlocked<>0) then
 begin
  sx_xunlock(@sysctlmemlock);
 end;

 if (error<>0) and (error<>ENOMEM) then
 begin
  Exit(error);
 end;

 if (retval<>nil) then
 begin
  if (req.oldptr<>nil) and (req.oldidx > req.validlen) then
   retval^:=req.validlen
  else
   retval^:=req.oldidx;
 end;
 Exit(error);
end;

function sys___sysctl(name   :PInteger;
                      namelen:DWORD;
                      old    :Pointer;
                      oldlenp:PQWORD;
                      new    :Pointer;
                      newlen :QWORD):Integer;
var
 error,i:Integer;
 _name:array[0..CTL_MAXNAME-1] of Integer;
 j:QWORD;
begin
 if (namelen > CTL_MAXNAME) or (namelen < 2) then
 begin
  Exit(EINVAL);
 end;

 error:=copyin(name, @_name, namelen * sizeof(Integer));
 if (error<>0) then Exit(error);

 error:=userland_sysctl(@_name,
                        namelen,
                        old,
                        oldlenp,
                        0,
                        new,
                        newlen,
                        @j,
                        0);

 if (error<>0) and (error<>ENOMEM) then
 begin
  Exit(error);
 end;

 if (oldlenp<>nil) then
 begin
  i:=copyout(@j, oldlenp, sizeof(j));
  if (i<>0) then
  begin
   Exit(i);
  end;
 end;

 Exit(error);
end;






end.

