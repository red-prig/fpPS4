unit vm_mmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vm_map,
 sys_vm_object;

type
 p_query_memory_prot=^t_query_memory_prot;
 t_query_memory_prot=packed record
  start:Pointer;
  __end:Pointer;
  prot  :Integer;
  eflags:Integer;
 end;
 {$IF sizeof(t_query_memory_prot)<>24}{$STOP sizeof(t_query_memory_prot)<>24}{$ENDIF}

 function sys_mmap(vaddr:Pointer;
                   vlen :QWORD;
                   prot :Integer;
                   flags:Integer;
                   fd   :Integer;
                   pos  :QWORD):Pointer;

function sys_munmap(addr:Pointer;len:QWORD):Integer;
function sys_mprotect(addr:Pointer;len:QWORD;prot:Integer):Integer;
function sys_madvise(addr:Pointer;len:QWORD;behav:Integer):Integer;
function sys_mname(addr:Pointer;len:QWORD;name:PChar):Integer;
function sys_query_memory_protection(addr:Pointer;info:Pointer):Integer;
function sys_get_page_table_stats(vm_container,cpu_gpu:Integer;p_total,p_available:PInteger):Integer;

function vm_mmap_to_errno(rv:Integer):Integer; inline;

function vm_mmap2(map        :vm_map_t;
                  addr       :p_vm_offset_t;
                  size       :vm_size_t;
                  prot       :vm_prot_t;
                  maxprot    :vm_prot_t;
                  flags      :Integer;
                  handle_type:objtype_t;
                  handle     :Pointer;
                  foff       :vm_ooffset_t):Integer;

implementation

uses
 vcapability,
 systm,
 errno,
 kern_thr,
 kern_proc,
 vmparam,
 sys_resource,
 kern_resource,
 kern_mtx,
 kern_descrip,
 vmount,
 vstat,
 vfile,
 vfcntl,
 vnode,
 vfs_subr,
 vnode_if,
 kern_conf,
 vm_pager;

function vm_mmap_cdev(objsize     :vm_size_t;
                      prot        :vm_prot_t;
                      maxprotp    :p_vm_prot_t;
                      flagsp      :PInteger;
                      cdev        :p_cdev;
                      foff        :p_vm_ooffset_t;
                      objp        :p_vm_object_t):Integer;
var
 obj:vm_object_t;
 dsw:p_cdevsw;
 error,flags,ref:Integer;
begin
 flags:=flagsp^;

 dsw:=dev_refthread(cdev, @ref);
 if (dsw=nil) then
 begin
  Exit(ENXIO);
 end;

 if ((dsw^.d_flags and D_MMAP_ANON)<>0) then
 begin
  dev_relthread(cdev, ref);
  maxprotp^:=VM_PROT_ALL;
  flagsp^:=flagsp^ or MAP_ANON;
  Exit(0);
 end;
 {
  * cdevs do not provide private mappings of any kind.
  }
 if ((maxprotp^ and VM_PROT_WRITE)=0) and
    ((prot and VM_PROT_WRITE)<>0) then
 begin
  dev_relthread(cdev, ref);
  Exit(EACCES);
 end;

 if ((flags and (MAP_PRIVATE{ or MAP_COPY}))<>0) then
 begin
  dev_relthread(cdev, ref);
  Exit(EINVAL);
 end;
 {
  * Force device mappings to be shared.
  }
 flags:=flags or MAP_SHARED;

 {
 * First, try d_mmap_single().  If that is not implemented
 * (returns ENODEV), fall back to using the device pager.
 * Note that d_mmap_single() must return a reference to the
 * object (it needs to bump the reference count of the object
 * it returns somehow).
  *
  * XXX assumes VM_PROT_*=PROT_*
  }
 error:=dsw^.d_mmap_single(cdev, foff, objsize, objp, prot);

 dev_relthread(cdev, ref);

 if (error<>ENODEV) then
 begin
  Exit(error);
 end;

 obj:=vm_pager_allocate(OBJT_DEVICE, cdev, objsize, prot, foff^);

 if (obj=nil) then
 begin
  Exit(EINVAL);
 end;

 objp^:=obj;
 flagsp^:=flags;
 Exit(0);
end;

function vm_mmap_vnode(objsize     :vm_size_t;
                       prot        :vm_prot_t;
                       maxprotp    :p_vm_prot_t;
                       flagsp      :PInteger;
                       vp          :p_vnode;
                       foffp       :p_vm_ooffset_t;
                       objp        :p_vm_object_t;
                       writecounted:PBoolean):Integer;
label
 mark_atime,
 done;
var
 va:t_vattr;
 obj:vm_object_t;
 foff:vm_offset_t;
 mp:p_mount;
 error,flags,locktype,vfslocked:Integer;
begin
 mp:=vp^.v_mount;

 if ((maxprotp^ and VM_PROT_WRITE)<>0) and ((flagsp^ and MAP_SHARED)<>0) then
  locktype:=LK_EXCLUSIVE
 else
  locktype:=LK_SHARED;

 vfslocked:=VFS_LOCK_GIANT(mp);
 error:=vget(vp, locktype);
 if (error<>0) then
 begin
  VFS_UNLOCK_GIANT(vfslocked);
  Exit(error);
 end;
 foff:=foffp^;
 flags:=flagsp^;
 obj:=nil;
 obj:=vp^.v_object;

 case vp^.v_type of
  VREG:
       begin
        {
         * Get the proper underlying object
         }
        if (obj=nil) then
        begin
         error:=EINVAL;
         goto done;
        end;
        if (obj^.handle<>vp) then
        begin
         vput(vp);
         vp:=obj^.handle;
         {
          * Bypass filesystems obey the mpsafety of the
          * underlying fs.
          }
         error:=vget(vp, locktype);
         if (error<>0) then
         begin
          VFS_UNLOCK_GIANT(vfslocked);
          Exit(error);
         end;
        end;
        if (locktype=LK_EXCLUSIVE) then
        begin
         writecounted^:=TRUE;
         //vnode_pager_update_writecount(obj, 0, objsize);
        end;
       end;
  VCHR:
       begin
        error:=vm_mmap_cdev(objsize, prot, maxprotp, flagsp, vp^.v_rdev, foffp, objp);
        if (error=0) then goto mark_atime;
        goto done;
       end

  else
       begin
        error:=EINVAL;
        goto done;
       end;
 end;

 error:=VOP_GETATTR(vp, @va);
 if (error<>0) then
 begin
  goto done;
 end;

 if ((flags and MAP_SHARED)<>0) then
 begin
  if ((va.va_flags and (SF_SNAPSHOT or IMMUTABLE or APPEND))<>0) then
  begin
   if ((prot and VM_PROT_WRITE)<>0) then
   begin
    error:=EPERM;
    goto done;
   end;
   maxprotp^:=maxprotp^ and (not VM_PROT_WRITE);
  end;
 end;
 {
  * If it is a regular file without any references
  * we do not need to sync it.
  * Adjust object size to be the size of actual file.
  }
 objsize:=round_page(va.va_size);
 if (va.va_nlink=0) then
 begin
  flags:=flags or MAP_NOSYNC;
 end;
 obj:=vm_pager_allocate(OBJT_VNODE, vp, objsize, prot, foff);
 if (obj=nil) then
 begin
  error:=ENOMEM;
  goto done;
 end;
 objp^:=obj;
 flagsp^:=flags;

mark_atime:
 vfs_mark_atime(vp);

done:
 if (error<>0) and writecounted^ then
 begin
  writecounted^:=FALSE;
  //vnode_pager_update_writecount(obj, objsize, 0);
 end;
 vput(vp);
 VFS_UNLOCK_GIANT(vfslocked);
 Result:=(error);
end;

function vm_mmap_shm(objsize     :vm_size_t;
                     prot        :vm_prot_t;
                     maxprotp    :p_vm_prot_t;
                     flagsp      :PInteger;
                     shmfd       :Pointer; //shmfd
                     foff        :p_vm_ooffset_t;
                     objp        :p_vm_object_t):Integer;
var
 error:Integer;
begin
 if ((flagsp^ and MAP_SHARED)<>0) and
    ((maxprotp^ and VM_PROT_WRITE)=0) and
    ((prot and VM_PROT_WRITE)<>0) then
 begin
  Exit(EACCES);
 end;

 //error:=shm_mmap(shmfd, objsize, foff, objp);
 error:=EOPNOTSUPP;

 Exit(error);
end;

function vm_mmap_dmem(handle      :Pointer;
                      objsize     :vm_size_t;
                      foff        :p_vm_ooffset_t;
                      objp        :p_vm_object_t):Integer;
begin
 //todo
 Exit(EOPNOTSUPP);
end;

function vm_mmap_to_errno(rv:Integer):Integer; inline;
begin
 Case rv of
  KERN_SUCCESS           :Result:=0;
  KERN_INVALID_ADDRESS,
  KERN_NO_SPACE          :Result:=ENOMEM;
  KERN_PROTECTION_FAILURE:Result:=EACCES;
  KERN_RESOURCE_SHORTAGE :
   begin
    Result:=ENOMEM;
    if (p_proc.p_sdk_version < $3500000) then
    begin
     Result:=EINVAL;
    end;
   end;
  else
   Result:=EINVAL;
 end;
end;

function vm_mmap2(map        :vm_map_t;
                  addr       :p_vm_offset_t;
                  size       :vm_size_t;
                  prot       :vm_prot_t;
                  maxprot    :vm_prot_t;
                  flags      :Integer;
                  handle_type:objtype_t;
                  handle     :Pointer;
                  foff       :vm_ooffset_t):Integer;
var
 obj:vm_object_t;
 docow,error,findspace,rv:Integer;
 fitit:Boolean;
 writecounted:Boolean;
begin
 if (size=0) then Exit(0);

 obj:=nil;

 size:=round_page(size);

 if (map^.size + size) > lim_cur(RLIMIT_VMEM) then
 begin
  Exit(ENOMEM);
 end;

 if (foff and PAGE_MASK)<>0 then Exit(EINVAL);

 if ((flags and MAP_FIXED)=0) then
 begin
  fitit:=TRUE;
  addr^:=round_page(addr^);
 end else
 begin
  if (addr^<>trunc_page(addr^)) then
  begin
   Exit(EINVAL);
  end;
  fitit:=FALSE;
 end;
 writecounted:=False;

 Case handle_type of
  OBJT_DEVICE:
   begin
    error:=vm_mmap_cdev(size,prot,@maxprot,@flags,handle,@foff,@obj);
   end;
  OBJT_SELF,  //same as file
  OBJT_VNODE:
   begin
    error:=vm_mmap_vnode(size,prot,@maxprot,@flags,handle,@foff,@obj,@writecounted);
   end;
  OBJT_SWAP:
   begin
    error:=vm_mmap_shm(size,prot,@maxprot,@flags,handle,@foff,@obj);
   end;
  OBJT_PHYSHM:
   begin
    error:=EACCES;
    if ((prot and (VM_PROT_WRITE or VM_PROT_GPU_WRITE))=0) or
       ((maxprot and VM_PROT_WRITE)<>0) then
    begin
     error:=vm_mmap_dmem(handle,size,@foff,@obj);
    end;
   end;

  OBJT_DEFAULT:
   begin
    if (handle=nil) then
    begin
     error:=0;
    end else
    begin
     error:=EINVAL;
    end;
   end;
  else
   error:=EINVAL;
 end;

 if (error<>0) then Exit(error);

 if ((flags and MAP_ANON)<>0) then
 begin
  obj:=nil;
  docow:=0;
  if (handle=nil) then foff:=0;
 end else
 if ((flags and MAP_PREFAULT_READ)<>0) then
  docow:=MAP_PREFAULT
 else
  docow:=MAP_PREFAULT_PARTIAL;

 if ((flags and (MAP_ANON or MAP_SHARED))=0) then
  docow:=docow or MAP_COPY_ON_WRITE;

 if ((flags and MAP_NOSYNC)<>0) then
  docow:=docow or MAP_DISABLE_SYNCER;

 if ((flags and MAP_NOCORE)<>0) then
  docow:=docow or MAP_DISABLE_COREDUMP;

 // Shared memory is also shared with children.
 if ((flags and MAP_SHARED)<>0) then
  docow:=docow or MAP_INHERIT_SHARE;

 if (writecounted) then
  docow:=docow or MAP_VN_WRITECOUNT;

 if ((flags and MAP_STACK)<>0) then
 begin
  rv:=vm_map_stack(map, addr^, size, prot, maxprot, docow or MAP_STACK_GROWS_DOWN);
 end else
 if (fitit) then
 begin
  if ((flags and MAP_ALIGNMENT_MASK)=MAP_ALIGNED_SUPER) then
  begin
   findspace:=VMFS_SUPER_SPACE;
  end else
  if ((flags and MAP_ALIGNMENT_MASK)<>0) then
  begin
   findspace:=VMFS_ALIGNED_SPACE(flags shr MAP_ALIGNMENT_SHIFT);
  end else
  begin
   findspace:=VMFS_OPTIMAL_SPACE;
  end;
  rv:=vm_map_find(map, obj, foff, addr, size, findspace, prot, maxprot, docow);
 end else
 begin
  rv:=vm_map_fixed(map, obj, foff, addr^, size, prot, maxprot, docow,
       ord((flags and MAP_NO_OVERWRITE)=0));
 end;

 if (rv=KERN_SUCCESS) then
 begin
  //
 end else
 begin
  if (writecounted) then
  begin
   //vnode_pager_release_writecount(vm_obj, 0, size);
  end;

  vm_object_deallocate(obj);
 end;
 Exit(vm_mmap_to_errno(rv));
end;

function sys_mmap(vaddr:Pointer;
                  vlen :QWORD;
                  prot :Integer;
                  flags:Integer;
                  fd   :Integer;
                  pos  :QWORD):Pointer;
label
 _map,
 _done;
var
 td:p_kthread;
 fp:p_file;
 vp:p_vnode;
 addr:vm_offset_t;
 size,pageoff:vm_size_t;
 cap_maxprot,maxprot:vm_prot_t;
 handle:Pointer;
 handle_type:obj_type;
 align:Integer;
 rights:cap_rights_t;
begin
 td:=curkthread;
 if (td=nil) then Exit(Pointer(-1));

 addr:=vm_offset_t(vaddr);
 size:=vlen;
 prot:=prot and VM_PROT_ALL;

 fp:=nil;

 if ((flags and $200000)<>0) {and (devkit_parameter(0)=0)} then
 begin
  Exit(Pointer(EINVAL));
 end;

 if (size = 0) and
    {(sv_flags > -1) and}
    (p_proc.p_osrel > $c3567) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((flags and (MAP_VOID or MAP_ANON))<>0) then
 begin
  if (pos<>0) or (fd<>-1) then
  begin
   Exit(Pointer(EINVAL));
  end;
 end;

 if ((flags and MAP_ANON)<>0) then
 begin
  pos:=0;
 end;

 if ((flags and MAP_STACK)<>0) then
 begin
  if (fd<>-1) or
     ((prot and (VM_PROT_READ or VM_PROT_WRITE))<>(VM_PROT_READ or VM_PROT_WRITE)) then
  begin
   Exit(Pointer(EINVAL));
  end;
  flags:=flags or MAP_ANON;
  pos:=0;
 end;

 pageoff:=(pos and PAGE_MASK);
 pos:=pos-pageoff;

 // Adjust size for rounding (on both ends).
 size:=size+pageoff;     // low end...
 size:=round_page(size); // hi end

 // Ensure alignment is at least a page and fits in a pointer.
 align:=flags and MAP_ALIGNMENT_MASK;
 if (align<>0) and
    (align<>Integer(MAP_ALIGNED_SUPER)) and
    (((align shr MAP_ALIGNMENT_SHIFT)>=sizeof(Pointer)*NBBY) or
    ((align shr MAP_ALIGNMENT_SHIFT) < PAGE_SHIFT)) then
 begin
  Exit(Pointer(EINVAL));
 end;

 if ((flags and MAP_FIXED)<>0) then
 begin
  addr:=addr-pageoff;
  if (addr and PAGE_MASK)<>0 then
  begin
   Exit(Pointer(EINVAL));
  end;

  //Address range must be all in user VM space.
  if (addr < vm_map_min(@g_vmspace.vm_map)) or
     (addr + size > vm_map_max(@g_vmspace.vm_map)) then
  begin
   Exit(Pointer(EINVAL));
  end;

  if (addr+size<addr) then
  begin
   Exit(Pointer(EINVAL));
  end;
 end else
 begin
  if (addr=0) then
  begin
   if (p_proc.p_sce_replay_exec=0) then
   begin
    addr:=SCE_USR_HEAP_START;
   end;
  end else
  if ((addr and QWORD($fffffffdffffffff))=0) then
  begin
   addr:=SCE_USR_HEAP_START;
  end;
 end;

 if ((flags and MAP_VOID)<>0) then
 begin
  //MAP_VOID
  handle:=nil;
  handle_type:=OBJT_DEFAULT;
  maxprot:=0;
  cap_maxprot:=0;
  flags:=flags or MAP_ANON;
  rights:=0;
  prot:=0;
  goto _map;
 end;

 if ((flags and MAP_ANON)<>0) then
 begin
  //Mapping blank space is trivial.
  handle:=nil;
  handle_type:=OBJT_DEFAULT;
  maxprot:=VM_PROT_ALL;
  cap_maxprot:=VM_PROT_ALL;
  goto _map;
 end;

 //Mapping file
 rights:=CAP_MMAP;
 if ((prot and (VM_PROT_READ or VM_PROT_GPU_READ))<>0) then
 begin
  rights:=rights or CAP_READ;
 end;
 if ((flags and MAP_SHARED)<>0) then
 begin
  if ((prot and (VM_PROT_WRITE or VM_PROT_GPU_WRITE))<>0) then
  begin
   rights:=rights or CAP_WRITE;
  end;
 end;
 if ((prot and VM_PROT_EXECUTE)<>0) then
 begin
  rights:=rights or CAP_MAPEXEC;
 end;

 Result:=Pointer(fget_mmap(fd,rights,@cap_maxprot,@fp));
 if (Result<>nil) then goto _done;

 case fp^.f_type of
  DTYPE_VNODE:
    begin
     vp:=fp^.f_vnode;

     maxprot:=VM_PROT_EXECUTE;

     if (vp^.v_mount<>nil) then
     if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_NOEXEC)<>0) then
     begin
      maxprot:=VM_PROT_NONE;
     end;

     if ((fp^.f_flag and FREAD)<>0) then
     begin
      maxprot:=maxprot or VM_PROT_READ;
     end else
     if ((prot and VM_PROT_READ)<>0) then
     begin
      Result:=Pointer(EACCES);
      goto _done;
     end;

     if ((flags and MAP_SHARED)<>0) then
     begin
      if ((fp^.f_flag and FWRITE)<>0) then
      begin
       maxprot:=maxprot or VM_PROT_WRITE;
      end else
      if ((prot and VM_PROT_WRITE)<>0) then
      begin
       Result:=Pointer(EACCES);
       goto _done;
      end;
     end else
     if (vp^.v_type<>VCHR) or ((fp^.f_flag and FWRITE)<>0) then
     begin
      maxprot:=maxprot or VM_PROT_WRITE;
      cap_maxprot:=cap_maxprot or VM_PROT_WRITE;
     end;

     handle:=vp;
     handle_type:=OBJT_VNODE;
    end;

  DTYPE_SHM:
    begin
     handle:=fp^.f_data;
     handle_type:=OBJT_SWAP;
     maxprot:=VM_PROT_NONE;

     // FREAD should always be set.
     if ((fp^.f_flag and FREAD)<>0) then
     begin
      maxprot:=maxprot or (VM_PROT_EXECUTE or VM_PROT_READ);
     end;
     if ((fp^.f_flag and FWRITE)<>0) then
     begin
      maxprot:=maxprot or VM_PROT_WRITE;
     end;
     goto _map;
    end;

  DTYPE_PHYSHM:
    begin
     handle:=fp^.f_data;
     handle_type:=OBJT_PHYSHM;

     prot:=VM_PROT_READ or VM_PROT_GPU_READ;

     if ((fp^.f_flag and FREAD)=0) then
     begin
      prot:=VM_PROT_NONE;
     end;

     maxprot:=prot or (VM_PROT_WRITE or VM_PROT_GPU_WRITE);

     if ((fp^.f_flag and FWRITE)=0) then
     begin
      maxprot:=prot;
     end;
    end;

  DTYPE_BLOCKPOOL:
    begin
     handle:=fp^.f_data;
     handle_type:=OBJT_BLOCKPOOL;
     maxprot:=VM_PROT_ALL;
    end;

  else
    begin
     Writeln('DTYPE_',fp^.f_type,' TODO');
     Result:=Pointer(ENODEV);
     goto _done;
    end;
 end;

_map:
 td^.td_fpop:=fp;
 maxprot:=maxprot and cap_maxprot;
 Result:=Pointer(vm_mmap2(@g_vmspace.vm_map,@addr,size,prot,maxprot,flags,handle_type,handle,pos));
 td^.td_fpop:=nil;

 td^.td_retval[0]:=(addr+pageoff);

_done:
 if (fp<>nil) then
 begin
  fdrop(fp);
 end;
end;

function sys_munmap(addr:Pointer;len:QWORD):Integer;
var
 size,pageoff:vm_size_t;
begin
 size:=len;
 if (size=0) then
 begin
  Exit(EINVAL);
 end;

 pageoff:=(vm_size_t(addr) and PAGE_MASK);
 addr:=addr-pageoff;
 size:=size+pageoff;
 size:=round_page(size);

 if (addr + size < addr) then
 begin
  Exit(EINVAL);
 end;

 {
  * Check for illegal addresses.  Watch out for address wrap...
  }
 if (qword(addr) < vm_map_min(@g_vmspace.vm_map)) or (qword(addr) + size > vm_map_max(@g_vmspace.vm_map)) then
 begin
  Exit(EINVAL);
 end;

 vm_map_lock  (@g_vmspace.vm_map);
 vm_map_delete(@g_vmspace.vm_map, qword(addr), qword(addr) + size);
 vm_map_unlock(@g_vmspace.vm_map);

 // vm_map_delete returns nothing but KERN_SUCCESS anyway
 Exit(0);
end;

function sys_mprotect(addr:Pointer;len:QWORD;prot:Integer):Integer;
var
 size,pageoff:vm_size_t;
begin
 size:=len;
 prot:=prot and VM_PROT_ALL;

 pageoff:=(vm_size_t(addr) and PAGE_MASK);
 addr:=addr-pageoff;
 size:=size+pageoff;
 size:=round_page(size);

 if (addr + size < addr) then
 begin
  Exit(EINVAL);
 end;

 Result:=vm_map_protect(@g_vmspace.vm_map, QWORD(addr), QWORD(addr) + size, prot, FALSE);

 case Result of
  KERN_SUCCESS           :Exit(0);
  KERN_PROTECTION_FAILURE:Exit(EACCES);
  KERN_RESOURCE_SHORTAGE :Exit(ENOMEM);
  else
                          Exit(EINVAL);
 end;
end;

function sys_madvise(addr:Pointer;len:QWORD;behav:Integer):Integer;
var
 start,__end:vm_offset_t;
begin
 {
  * Check for our special case, advising the swap pager we are
  * "immortal."
  }
 if (behav=MADV_PROTECT) then
 begin
  Exit(0);
 end;

 {
  * Check for illegal behavior
  }
 if (behav < 0) or (behav > MADV_CORE) then
 begin
  Exit(EINVAL);
 end;
 {
  * Check for illegal addresses.  Watch out for address wrap... Note
  * that VM_*_ADDRESS are not constants due to casts (argh).
  }
 if (vm_offset_t(addr) < vm_map_min(@g_vmspace.vm_map)) or
    (vm_offset_t(addr) + len > vm_map_max(@g_vmspace.vm_map)) then
 begin
  Exit(EINVAL);
 end;

 if (vm_offset_t(addr) + len) < vm_offset_t(addr) then
 begin
  Exit(EINVAL);
 end;

 {
  * Since this routine is only advisory, we default to conservative
  * behavior.
  }
 start:=trunc_page(vm_offset_t(addr));
 __end:=round_page(vm_offset_t(addr) + len);

 if (vm_map_madvise(@g_vmspace.vm_map, start, __end, behav))<>0 then
 begin
  Exit(EINVAL);
 end;

 Exit(0);
end;

function sys_mname(addr:Pointer;len:QWORD;name:PChar):Integer;
var
 start,__end:vm_offset_t;
 _name:array[0..31] of Char;
begin
 if (vm_offset_t(addr) < vm_map_min(@g_vmspace.vm_map)) or
    (vm_offset_t(addr) + len > vm_map_max(@g_vmspace.vm_map)) then
 begin
  Exit(EINVAL);
 end;

 if (vm_offset_t(addr) + len) < vm_offset_t(addr) then
 begin
  Exit(EINVAL);
 end;

 Result:=copyinstr(name,@_name,32,nil);
 if (Result<>0) then Exit;

 start:=trunc_page(vm_offset_t(addr));
 __end:=round_page(vm_offset_t(addr) + len);

 vm_map_set_name(@g_vmspace.vm_map,start,__end,@_name);
end;

function sys_query_memory_protection(addr:Pointer;info:Pointer):Integer;
var
 map:vm_map_t;
 _addr:vm_offset_t;
 __end:vm_offset_t;
 entry:vm_map_entry_t;
 data:t_query_memory_prot;
begin
 Result:=EINVAL;
 _addr:=trunc_page(vm_offset_t(addr));
 map:=@g_vmspace.vm_map;
 __end:=vm_map_max(map);
 if (_addr<__end) or (_addr=__end) then
 begin
  vm_map_lock(map);
  if not vm_map_lookup_entry(map,_addr,@entry) then
  begin
   vm_map_unlock(map);
   Result:=EACCES;
  end else
  begin
   data.start:=Pointer(entry^.start);
   data.__end:=Pointer(entry^.__end);
   data.prot:=(entry^.max_protection and entry^.protection);
   data.eflags:=entry^.eflags;
   vm_map_unlock(map);
   Result:=copyout(@data,info,SizeOf(t_query_memory_prot));
  end;
 end;
end;

function sys_get_page_table_stats(vm_container,cpu_gpu:Integer;p_total,p_available:PInteger):Integer;
begin
 Exit(ENOENT); //devkit_parameter(0)=0
end;

end.

