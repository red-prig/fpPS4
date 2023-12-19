unit systm;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function copystr(from,_to:pchar;maxlen:ptruint;lencopied:pptruint):Integer;
function copyin(udaddr,kaddr:Pointer;len:ptruint):Integer;
function copyin_nofault(udaddr,kaddr:Pointer;len:ptruint):Integer;
function copyinstr(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
function copyout(kaddr,udaddr:Pointer;len:ptruint):Integer;
function copyout_nofault(kaddr,udaddr:Pointer;len:ptruint):Integer;
function fubyte(var base:Byte):Byte;
function fuword32(var base:DWORD):DWORD;
function fuword64(var base:QWORD):QWORD;
function fuword(var base:Pointer):Pointer;
function casuword32(var base:DWORD;oldval,newval:DWORD):DWORD;
function casuword64(var base:QWORD;oldval,newval:QWORD):QWORD;
function suword32(var base:DWORD;word:DWORD):DWORD;
function suword64(var base:QWORD;word:QWORD):QWORD;
function suword(var base:Pointer;word:Pointer):Pointer;

implementation

uses
 errno,
 md_systm,
 vmparam,
 vm_pmap,
 kern_thr;

function copystr(from,_to:pchar;maxlen:ptruint;lencopied:pptruint):Integer;
var
 counter:ptruint;
begin
 counter:=0;
 while (from[counter]<>#0) and (counter<maxlen) do
 begin
  _to[counter]:=from[counter];
  Inc(counter);
 end;
 if (counter<maxlen) then
 begin
  _to[counter]:=#0;
  Inc(counter);
 end;
 if (lencopied<>nil) then
 begin
  lencopied^:=counter;
 end;
 Result:=0;
end;

function copyin(udaddr,kaddr:Pointer;len:ptruint):Integer;
label
 _fault,
 _next,
 _exit;
var
 src:Pointer;
 i,w:DWORD;
 guest:Boolean;
begin
 Result:=0;

 curthread_set_pcb_onfault(@_fault);

 while (len<>0) do
 begin
  i:=QWORD(udaddr) and PAGE_MASK;
  i:=PAGE_SIZE-i;
  if (i>len) then i:=len;
  w:=i;

  guest:=is_guest_addr(QWORD(udaddr));
  if guest then
  begin
   if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_READ)=0) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;

   src:=uplift(udaddr);
  end else
  begin
   src:=udaddr;
  end;

  if (src=nil) then
  begin
   Result:=EFAULT;
   goto _exit;

   _fault:
    w:=w-i;

    if guest then
    begin
     if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_READ)<>0) then
     begin
      goto _next;
     end else
     begin
      Result:=EFAULT;
      goto _exit;
     end;
    end else
    begin
     Result:=EFAULT;
     goto _exit;
    end;

  end;

  while (i<>0) do
  begin
   PByte(kaddr)^:=PByte(src)^;

   Dec(i    );
   Inc(src  );
   Inc(kaddr);
  end;

  _next:

  Dec(len   ,w);
  Inc(udaddr,w);
 end;

 _exit:
  curthread_set_pcb_onfault(nil);
end;

function copyin_nofault(udaddr,kaddr:Pointer;len:ptruint):Integer;
label
 _exit;
var
 src:Pointer;
 lencopied:ptruint;
 i:DWORD;
 guest:Boolean;
begin
 Result:=0;

 while (len<>0) do
 begin
  i:=QWORD(udaddr) and PAGE_MASK;
  i:=PAGE_SIZE-i;
  if (i>len) then i:=len;

  guest:=is_guest_addr(QWORD(udaddr));
  if guest then
  begin
   if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_READ)=0) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;

   src:=uplift(udaddr);
  end else
  begin
   src:=udaddr;
  end;

  if (src=nil) then
  begin
   Result:=EFAULT;
   goto _exit;
  end;

  lencopied:=0;
  Result:=md_copyin(src,kaddr,i,@lencopied);

  if (Result<>0) then
  begin
   if guest then
   begin
    if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_READ)<>0) then
    begin
     Result:=0;
    end else
    begin
     Result:=EFAULT;
     goto _exit;
    end;
   end else
   begin
    Result:=EFAULT;
    goto _exit;
   end;
  end;

  Dec(len   ,lencopied);
  Inc(udaddr,lencopied);
  Inc(kaddr ,lencopied);
 end;

 _exit:
end;

function copyinstr(udaddr,kaddr:Pointer;len:ptruint;lencopied:pptruint):Integer;
label
 _fault,
 _next,
 _exit;
var
 src:Pointer;
 copied:ptruint;
 i,w:DWORD;
 guest:Boolean;
 b:Byte;
begin
 Result:=0;
 copied:=0;

 curthread_set_pcb_onfault(@_fault);

 while (len<>0) do
 begin
  i:=QWORD(udaddr) and PAGE_MASK;
  i:=PAGE_SIZE-i;
  if (i>len) then i:=len;
  w:=i;

  guest:=is_guest_addr(QWORD(udaddr));
  if guest then
  begin
   if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_READ)=0) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;

   src:=uplift(udaddr);
  end else
  begin
   src:=udaddr;
  end;

  if (src=nil) then
  begin
   Result:=EFAULT;
   goto _exit;

   _fault:
    w:=w-i;

    if guest then
    begin
     if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_READ)<>0) then
     begin
      goto _next;
     end else
     begin
      Result:=EFAULT;
      goto _exit;
     end;
    end else
    begin
     Result:=EFAULT;
     goto _exit;
    end;
  end;

  w:=i;
  while (i<>0) do
  begin
   b:=PByte(src)^;
   PByte(kaddr)^:=b;

   Inc(copied);

   if (b=0) then
   begin
    Result:=0;
    goto _exit;
   end;

   Dec(i    );
   Inc(src  );
   Inc(kaddr);
  end;

  _next:

  Dec(len   ,w);
  Inc(udaddr,w);
 end;

 _exit:
  curthread_set_pcb_onfault(nil);
  if (lencopied<>nil) then
  begin
   lencopied^:=copied;
  end;
end;

function copyout(kaddr,udaddr:Pointer;len:ptruint):Integer;
label
 _fault,
 _next,
 _exit;
var
 dst:Pointer;
 i,w:DWORD;
 guest:Boolean;
begin
 Result:=0;

 curthread_set_pcb_onfault(@_fault);

 while (len<>0) do
 begin
  i:=QWORD(udaddr) and PAGE_MASK;
  i:=PAGE_SIZE-i;
  if (i>len) then i:=len;
  w:=i;

  guest:=is_guest_addr(QWORD(udaddr));
  if guest then
  begin
   if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_WRITE)=0) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;

   dst:=uplift(udaddr);
  end else
  begin
   dst:=udaddr;
  end;

  if (dst=nil) then
  begin
   Result:=EFAULT;
   goto _exit;

   _fault:
    w:=w-i;

    if guest then
    begin
     if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_WRITE)<>0) then
     begin
      goto _next;
     end else
     begin
      Result:=EFAULT;
      goto _exit;
     end;
    end else
    begin
     Result:=EFAULT;
     goto _exit;
    end;

  end;

  while (i<>0) do
  begin
   PByte(dst)^:=PByte(kaddr)^;

   Dec(i    );
   Inc(dst  );
   Inc(kaddr);
  end;

  _next:

  Dec(len   ,w);
  Inc(udaddr,w);
 end;

 _exit:
  curthread_set_pcb_onfault(nil);
end;

function copyout_nofault(kaddr,udaddr:Pointer;len:ptruint):Integer;
label
 _exit;
var
 dst:Pointer;
 lencopied:ptruint;
 i:DWORD;
 guest:Boolean;
begin
 Result:=0;

 while (len<>0) do
 begin
  i:=QWORD(udaddr) and PAGE_MASK;
  i:=PAGE_SIZE-i;
  if (i>len) then i:=len;

  guest:=is_guest_addr(QWORD(udaddr));
  if guest then
  begin
   if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_WRITE)=0) then
   begin
    Result:=EFAULT;
    goto _exit;
   end;

   dst:=uplift(udaddr);
  end else
  begin
   dst:=udaddr;
  end;

  if (dst=nil) then
  begin
   Result:=EFAULT;
   goto _exit;
  end;

  lencopied:=0;
  Result:=md_copyout(kaddr,dst,i,@lencopied);

  if (Result<>0) then
  begin
   if guest then
   begin
    if ((pmap_get_prot(QWORD(udaddr)) and PAGE_PROT_WRITE)<>0) then
    begin
     Result:=0;
    end else
    begin
     Result:=EFAULT;
     goto _exit;
    end;
   end else
   begin
    Result:=EFAULT;
    goto _exit;
   end;
  end;

  Dec(len   ,lencopied);
  Inc(udaddr,lencopied);
  Inc(kaddr ,lencopied);
 end;

 _exit:
end;

function fubyte(var base:Byte):Byte;
begin
 if (copyin(@base,@Result,SizeOf(base))<>0) then
 begin
  Result:=BYTE(-1);
 end;
end;

function fuword32(var base:DWORD):DWORD;
begin
 if (copyin(@base,@Result,SizeOf(base))<>0) then
 begin
  Result:=DWORD(-1);
 end;
end;

function fuword64(var base:QWORD):QWORD;
begin
 if (copyin(@base,@Result,SizeOf(base))<>0) then
 begin
  Result:=QWORD(-1);
 end;
end;

function fuword(var base:Pointer):Pointer;
begin
 if (copyin(@base,@Result,SizeOf(base))<>0) then
 begin
  Result:=Pointer(QWORD(-1));
 end;
end;

function casuword32(var base:DWORD;oldval,newval:DWORD):DWORD;
label
 _begin,
 _fault,
 _exit;
var
 src:PDWORD;
 guest:Boolean;
begin
 curthread_set_pcb_onfault(@_fault);

 _begin:

 guest:=is_guest_addr(QWORD(@base));

 if guest then
 begin
  if pmap_test_cross(QWORD(@base),3) then
  begin
   Assert(false,'casuword32');
  end;

  src:=uplift(@base);
 end else
 begin
  src:=@base;
 end;

 if (src=nil) then
 begin
  Result:=DWORD(-1);
  goto _exit;

  _fault:
   if guest then
   begin
    if ((pmap_get_prot(QWORD(@base)) and PAGE_PROT_RW)=PAGE_PROT_RW) then
    begin
     goto _begin;
    end else
    begin
     Result:=DWORD(-1);
     goto _exit;
    end;
   end else
   begin
    Result:=DWORD(-1);
    goto _exit;
   end;
 end;

 Result:=System.InterlockedCompareExchange(src^,newval,oldval);

 _exit:
  curthread_set_pcb_onfault(nil);
end;

function casuword64(var base:QWORD;oldval,newval:QWORD):QWORD;
label
 _begin,
 _fault,
 _exit;
var
 src:PQWORD;
 guest:Boolean;
begin
 curthread_set_pcb_onfault(@_fault);

 _begin:

 guest:=is_guest_addr(QWORD(@base));

 if guest then
 begin
  if pmap_test_cross(QWORD(@base),7) then
  begin
   Assert(false,'casuword64');
  end;

  src:=uplift(@base);
 end else
 begin
  src:=@base;
 end;

 if (src=nil) then
 begin
  Result:=QWORD(-1);
  goto _exit;

  _fault:
   if guest then
   begin
    if ((pmap_get_prot(QWORD(@base)) and PAGE_PROT_RW)=PAGE_PROT_RW) then
    begin
     goto _begin;
    end else
    begin
     Result:=QWORD(-1);
     goto _exit;
    end;
   end else
   begin
    Result:=QWORD(-1);
    goto _exit;
   end;
 end;

 Result:=System.InterlockedCompareExchange64(src^,newval,oldval);

 _exit:
  curthread_set_pcb_onfault(nil);
end;

function suword32(var base:DWORD;word:DWORD):DWORD;
begin
 if (copyout(@word,@base,SizeOf(base))=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=DWORD(-1);
 end;
end;

function suword64(var base:QWORD;word:QWORD):QWORD;
begin
 if (copyout(@word,@base,SizeOf(base))=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=QWORD(-1);
 end;
end;

function suword(var base:Pointer;word:Pointer):Pointer;
begin
 if (copyout(@word,@base,SizeOf(base))=0) then
 begin
  Result:=nil;
 end else
 begin
  Result:=Pointer(QWORD(-1));
 end;
end;


end.



