unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_backtrace,
 vm,
 vmparam,
 vm_object,
 vnode,
 vuio,
 kern_mtx,
 kern_rangelock,
 md_map,
 md_file,
 vm_pmap_prot,
 vm_tracking_map,
 vm_nt_map,
 vm_priv_map;

const
 PMAPP_BLK_SHIFT =29;
 PMAPP_BLK_SIZE  =QWORD(QWORD(1) shl PMAPP_BLK_SHIFT);
 PMAPP_BLK_MASK  =PMAPP_BLK_SIZE-1;

 PMAPP_BLK_DMEM_BLOCKS=(QWORD(VM_DMEM_SIZE)+PMAPP_BLK_MASK) shr PMAPP_BLK_SHIFT;

var
 DMEM_FD:array[0..PMAPP_BLK_DMEM_BLOCKS-1] of vm_nt_file_obj;

 DEV_INFO:record
  DEV_FD  :vm_nt_file_obj;
  DEV_SIZE:QWORD;
  DEV_POS :QWORD;
  DEV_PTR :Pointer;
 end;

function  uplift(addr:Pointer):Pointer;
procedure iov_uplift(iov:p_iovec);

type
 p_pmap=^t_pmap;
 t_pmap=packed object
  vm_map:Pointer;
  //rmlock:rangelock;
  //rm_mtx:mtx;
  nt_map:t_vm_nt_map;
  gp_map:t_vm_nt_map;
  tr_map:t_vm_track_map;
  priv_p:t_vm_priv_pool;
 end;

 pmap_t=p_pmap;

function  atop(x:QWORD):DWORD; inline;
function  ptoa(x:DWORD):QWORD; inline;

function  ctob(x:QWORD):QWORD; inline;
function  btoc(x:QWORD):QWORD; inline;

function  dev_mem_alloc(pages:Integer):Pointer;

procedure pmap_pinit(pmap:p_pmap;vm_map:Pointer);

procedure pmap_align_superpage(obj   :vm_object_t;
                               offset:vm_ooffset_t;
                               addr  :p_vm_offset_t;
                               size  :vm_size_t);

function  pmap_wlock(pmap :pmap_t;
                     start:vm_offset_t;
                     __end:vm_offset_t):Pointer;

function  pmap_rlock(pmap :pmap_t;
                     start:vm_offset_t;
                     __end:vm_offset_t):Pointer;

procedure pmap_unlock(pmap:pmap_t;cookie:Pointer);

procedure pmap_copy_pages(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prot :vm_prot_t);

procedure pmap_enter_object(pmap  :pmap_t;
                            obj   :vm_object_t;
                            offset:vm_ooffset_t;
                            start :vm_offset_t;
                            __end :vm_offset_t;
                            prot  :vm_prot_t);

procedure pmap_gpu_enter_object(pmap :pmap_t;
                                start:vm_offset_t;
                                __end:vm_offset_t;
                                prot :vm_prot_t);

procedure pmap_enter_dmem_block(pmap  :pmap_t;
                                offset:vm_ooffset_t;
                                start :vm_offset_t;
                                prot  :vm_prot_t);

procedure pmap_protect(pmap :pmap_t;
                       obj  :vm_object_t;
                       start:vm_offset_t;
                       __end:vm_offset_t;
                       prot :vm_prot_t);

procedure pmap_gpu_protect(pmap :pmap_t;
                           start:vm_offset_t;
                           __end:vm_offset_t;
                           prot :vm_prot_t);

procedure pmap_prot_track(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prot :Byte);

procedure pmap_prot_restore(pmap :pmap_t;
                            start:vm_offset_t;
                            __end:vm_offset_t);

procedure pmap_madvise(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       advise:Integer);

procedure pmap_remove(pmap :pmap_t;
                      obj  :vm_object_t;
                      start:vm_offset_t;
                      __end:vm_offset_t);

procedure pmap_gpu_remove(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t);

function  pmap_mirror_map(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t):Pointer;

procedure pmap_mirror_unmap(pmap:pmap_t;
                            base:Pointer;
                            size:QWORD);

function  pmap_danger_zone(pmap:pmap_t;
                           addr:vm_offset_t;
                           size:vm_offset_t):Boolean;

function  pmap_expand(pmap :pmap_t;
                      start:vm_offset_t;
                      __end:vm_offset_t):Boolean;

procedure pmap_gpu_get_bound(pmap:pmap_t;
                             var start:vm_offset_t;
                             var __end:vm_offset_t);

implementation

uses
 sysutils,
 ntapi,
 md_systm_reserve,
 sys_bootparam;

function atop(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function ptoa(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function ctob(x:QWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function btoc(x:QWORD):QWORD; inline;
begin
 Result:=((x+PAGE_MASK) shr PAGE_SHIFT);
end;

const
 external_dmem_swap_mode:Boolean=False;

procedure dmem_init;
var
 r:Integer;

 fname:RawByteString;
 fd:THandle;
 md:THandle;
 size:QWORD;
begin
 if external_dmem_swap_mode then
 begin
  size:=VM_DMEM_SIZE; // 6144MB

  fd:=0;
  fname:=sysutils.GetTempFileName(GetTempDir,'fpps4_dmem');
  r:=md_create_swap_file(fname,size,fd);
  if (r<>0) then
  begin
   Writeln('failed md_create_swap_file("',fname,'",',HexStr(size,16),'):0x',HexStr(r,8));
   Assert(false,'dmem_init');
  end;

  md:=0;
  r:=md_memfd_open(md,fd,VM_PROT_RW);
  Assert(r=0);

  DMEM_FD[0].hfile:=md;
  DMEM_FD[0].maxp :=VM_RW;
 end else
 begin
  //
 end;
end;

procedure dev_mem_init;
var
 r:Integer;
begin
 DEV_INFO.DEV_SIZE:=VM_MAX_DEV_ADDRESS-VM_MIN_DEV_ADDRESS;

 R:=md_memfd_create(DEV_INFO.DEV_FD.hfile,DEV_INFO.DEV_SIZE,VM_RW);
 if (r<>0) then
 begin
  Writeln('failed md_memfd_create(',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init'+HexStr(r,8));
 end;

 DEV_INFO.DEV_FD.maxp:=VM_RW;

 DEV_INFO.DEV_PTR:=Pointer(VM_MIN_DEV_ADDRESS);

 r:=md_placeholder_commit(DEV_INFO.DEV_PTR,DEV_INFO.DEV_SIZE,VM_RW,DEV_INFO.DEV_FD.hfile,0);
 if (r<>0) then
 begin
  Writeln('failed md_placeholder_commit(',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init:'+HexStr(r,8));
 end;
end;

function dev_mem_alloc(pages:Integer):Pointer;
var
 size:QWORD;
begin
 size:=pages*PAGE_SIZE;

 if (size>(DEV_INFO.DEV_SIZE-DEV_INFO.DEV_POS)) then
 begin
  Assert(false,'dev_mem_alloc limit');
  Exit(nil);
 end;

 Result:=DEV_INFO.DEV_PTR+DEV_INFO.DEV_POS;

 DEV_INFO.DEV_POS:=DEV_INFO.DEV_POS+size;
end;

procedure pmap_pinit(pmap:p_pmap;vm_map:Pointer);
var
 i:Integer;
 m:t_md_map_reserve_result;
begin
 m:=md_map_reserve();
 if (m.error<>0) then
 begin
  Writeln('failed pmap_reserve(',HexStr(m.base),',',HexStr(m.base+m.size),'):0x',HexStr(m.error,8));
  Assert(false,'pmap_pinit');
  Exit;
 end;

 dmem_init;
 dev_mem_init;

 if (PAGE_PROT=nil) then
 begin
  PAGE_PROT:=kmem_alloc(PAGE_MAP_COUNT_SZ1,VM_RW);
  Assert(PAGE_PROT<>nil,'pmap_pinit');
  //Set cache to be enabled on demand
  md_dontneed(PAGE_PROT,PAGE_MAP_COUNT_SZ1);
 end;

 //rangelock_init(@pmap^.rmlock);
 //mtx_init(pmap^.rm_mtx,'pmap');

 pmap^.vm_map:=vm_map;

 sys_init_vm_nt;

 vm_nt_map_init(@pmap^.nt_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS);
 vm_nt_map_init(@pmap^.gp_map,VM_MIN_GPU_ADDRESS,VM_MAX_GPU_ADDRESS);

 //exclude
 if Length(pmap_mem_guest)>1 then
 begin
  //mark all space as hole
  vm_nt_map_insert(@pmap^.nt_map,
                   nil,0,
                   VM_MINUSER_ADDRESS,
                   VM_MAXUSER_ADDRESS,
                   VM_MAXUSER_ADDRESS-VM_MINUSER_ADDRESS,
                   0);
  //
  For i:=0 to High(pmap_mem_guest) do
  begin
   //mark used regions as free
   vm_nt_map_delete(@pmap^.nt_map,
                    pmap_mem_guest[i].start,
                    pmap_mem_guest[i].__end);
  end;
 end;

 vm_track_map_init(@pmap^.tr_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS,vm_map);

 pmap^.tr_map.pmap:=pmap;

 vm_priv_pool_init(@pmap^.priv_p);
end;

type
 t_fd_info=record
  start :QWORD;
  __end :QWORD;
  obj   :p_vm_nt_file_obj;
  offset:QWORD;
  olocal:QWORD;
 end;

procedure get_priv_fd(pmap:pmap_t;var info:t_fd_info);
var
 size  :QWORD;
 R     :DWORD;
 palloc:t_vm_priv_alloc;
begin
 size:=(info.__end-info.start);

 R:=vm_priv_pool_alloc(@pmap^.priv_p,size,@palloc);
 if (R<>0) then
 begin
  Writeln('failed vm_priv_pool_alloc(',HexStr(size,11),'):0x',HexStr(r,8));
  Writeln(' priv_stat=',pmap^.priv_p.size,':',pmap^.priv_p.invm);

  print_backtrace_td(stderr);

  Assert(false,'get_priv_fd');
 end;

 info.obj   :=palloc.obj;
 info.olocal:=palloc.start;           //block local offset
 info.__end :=info.start+palloc.size; //apply size

 vm_nt_file_obj_reference(info.obj);
end;

procedure _print_dmem_fd; public;
var
 i:Integer;
 base:Pointer;
begin
 Writeln('[DMEM_FD]');
 if external_dmem_swap_mode then
 begin
  Writeln(' 0x',HexStr(VM_MIN_GPU_ADDRESS,16),'..',HexStr(VM_MAX_GPU_ADDRESS,16));
 end else
 begin

  For i:=0 to High(DMEM_FD) do
  begin
   //
   if (DMEM_FD[i].hfile<>0) then
   begin
    base:=Pointer(VM_MIN_GPU_ADDRESS+i*PMAPP_BLK_SIZE);
    Writeln(' 0x',HexStr(base),'..',HexStr(base+PMAPP_BLK_SIZE));
   end;
  end;

 end;
end;

procedure get_dev_fd(var info:t_fd_info;map_base:Pointer);
var
 o:QWORD;
begin
 o:=info.offset;

 if (map_base>=Pointer(VM_MIN_DEV_ADDRESS)) and
    (map_base< Pointer(VM_MAX_DEV_ADDRESS)) then
 begin
  //dev

  o:=o+(QWORD(map_base)-VM_MIN_DEV_ADDRESS);

  info.obj:=@DEV_INFO.DEV_FD;
 end else
 begin
  //unknow
  info.obj:=nil;
  o:=0;
 end;

 vm_nt_file_obj_reference(info.obj);

 info.olocal:=o; //block local offset
end;

procedure get_dmem_fd(var info:t_fd_info);
var
 BLK_SIZE:QWORD;
 MEM_SIZE:QWORD;
 o:QWORD;
 e:QWORD;
 i:DWORD;
 r:DWORD;
begin
 o:=info.offset;

 //get mem size
 MEM_SIZE:=(info.__end-info.start);

 //dmem
 if external_dmem_swap_mode then
 begin
  BLK_SIZE:=VM_DMEM_SIZE; // 6144MB

  info.obj:=@DMEM_FD[0];
 end else
 begin
  BLK_SIZE:=PMAPP_BLK_SIZE;

  //current block id
  i:=o shr PMAPP_BLK_SHIFT;

  Assert(i<Length(DMEM_FD));

  if (DMEM_FD[i].hfile=0) then
  begin
   R:=md_memfd_create(DMEM_FD[i].hfile,BLK_SIZE,VM_RW);

   DMEM_FD[i].maxp:=VM_RW;

   if (r<>0) then
   begin
    Writeln('failed md_memfd_create(',HexStr(BLK_SIZE,11),'):0x',HexStr(r,8));
    Assert(false,'get_dmem_fd');
   end;
  end;

  info.obj:=@DMEM_FD[i];
 end;
 //dmem

 vm_nt_file_obj_reference(info.obj);

 //current block offset
 o:=o mod BLK_SIZE;
 info.olocal:=o; //block local offset

 //max offset
 e:=o+MEM_SIZE;

 // |start         end|
 // |offset  |max
 if (e>BLK_SIZE) then
 begin
  e:=BLK_SIZE-o;
  e:=e+info.start;
  info.__end:=e;
 end;
end;

{
* Increase the starting virtual address of the given mapping if a
* different alignment might result in more superpage mappings.
}
procedure pmap_align_superpage(obj   :vm_object_t;
                               offset:vm_ooffset_t;
                               addr  :p_vm_offset_t;
                               size  :vm_size_t);
var
 superpage_offset:vm_offset_t;
begin
 if (size < NBPDR) then Exit;
 if (obj<>nil) then
 if ((obj^.flags and OBJ_COLORED)<>0) then
 begin
  offset:=offset+ptoa(obj^.pg_color);
 end;
 superpage_offset:=offset and PDRMASK;
 if (size - ((NBPDR - superpage_offset) and PDRMASK) < NBPDR) or
    ((addr^ and PDRMASK)=superpage_offset) then
 begin
  Exit;
 end;
 if ((addr^ and PDRMASK) < superpage_offset) then
 begin
  addr^:=(addr^ and (not PDRMASK)) + superpage_offset
 end else
 begin
  addr^:=((addr^ + PDRMASK) and (not PDRMASK)) + superpage_offset;
 end;
end;

function uplift(addr:Pointer):Pointer;
begin
 Result:=Pointer(QWORD(addr) and (VM_MAXUSER_ADDRESS-1));
end;

procedure iov_uplift(iov:p_iovec);
begin
 if (QWORD(iov^.iov_base)>=VM_MAXUSER_ADDRESS) then
 begin
  iov^:=Default(iovec);
  Exit;
 end;

 if ((QWORD(iov^.iov_base)+iov^.iov_len)>VM_MAXUSER_ADDRESS) then
 begin
  iov^.iov_len:=VM_MAXUSER_ADDRESS-QWORD(iov^.iov_base);
  Exit;
 end;
end;

function get_vnode_handle(obj:vm_object_t;var maxprot:Integer):THandle;
var
 vp:p_vnode;
begin
 Result:=0;

 vp:=obj^.handle;

 if (vp<>nil) then
 begin
  VI_LOCK(vp);

  Result:=THandle(vp^.v_un);
  maxprot:=vp^.v_prot;

  VI_UNLOCK(vp);
 end;
end;

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function fit_to_vnode_size(obj:vm_object_t;offset,size:QWORD):QWORD; inline;
begin
 //max unaligned size
 size:=size+offset;

 size:=Min(size,obj^.un_pager.vnp.vnp_size);

 //dec offset
 if (size>offset) then
 begin
  size:=size-offset;
 end else
 begin
  size:=0;
 end;

 Result:=size;
end;

function  vm_map_lock_range  (map:Pointer;start,__end:off_t;mode:Integer):Pointer; external;
procedure vm_map_unlock_range(map:Pointer;cookie:Pointer); external;

function pmap_wlock(pmap :pmap_t;
                    start:vm_offset_t;
                    __end:vm_offset_t):Pointer; inline;
begin
 //Writeln('pmap_wlock:',HexStr(start,11),'..',HexStr(__end,11));

 Result:=vm_map_lock_range(pmap^.vm_map,start,__end,RL_LOCK_WRITE);
end;

function pmap_rlock(pmap :pmap_t;
                    start:vm_offset_t;
                    __end:vm_offset_t):Pointer; inline;
begin
 //Writeln('pmap_rlock:',HexStr(start,11),'..',HexStr(__end,11));

 Result:=vm_map_lock_range(pmap^.vm_map,start,__end,RL_LOCK_READ);
end;

procedure pmap_unlock(pmap:pmap_t;cookie:Pointer); inline;
begin
 //Writeln('pmap_unlock:',HexStr(p_rl_q_entry(cookie)^.rl_q_start,11),'..',HexStr(p_rl_q_entry(cookie)^.rl_q_end,11));

 vm_map_unlock_range(pmap^.vm_map,cookie);
end;

procedure pmap_copy_dst(src_adr :Pointer;
                        dst_obj :p_vm_nt_file_obj;
                        dst_ofs :vm_ooffset_t;
                        size    :vm_ooffset_t);
var
 dst:Pointer;
 r:Integer;
begin
 if (size=0) then Exit;

 //alloc placeholder for dst
 dst:=Pointer(KERNEL_LOWER); //lower
 r:=md_placeholder_mmap(dst,size);
 if (r<>0) then
 begin
  Writeln('failed md_placeholder_mmap(',HexStr(size,11),'):0x',HexStr(r,8));
  Assert(false,'pmap_copy');
  Exit;
 end;

 //commit dst
 r:=md_placeholder_commit(dst,size,VM_RW,dst_obj^.hfile,dst_ofs);
 if (r<>0) then
 begin
  Writeln('failed md_placeholder_commit(',HexStr(dst),',',
                                          size,',',
                                          'VM_RW',',',
                                          HexStr(dst_obj^.hfile,16),',',
                                          HexStr(dst_ofs,8),'):0x',
                                          HexStr(r,8));
  Assert(false,'pmap_copy');
  Exit;
 end;

 Move(src_adr^,dst^,size);

 md_cacheflush(dst,size,DCACHE);

 r:=md_placeholder_unmap(dst,size);
 if (r<>0) then
 begin
  Writeln('failed md_placeholder_unmap(',HexStr(dst),',',size,'):0x',HexStr(r,8));
  Assert(false,'pmap_copy');
 end;

end;

function convert_to_gpu_prot(prot:vm_prot_t):vm_prot_t; inline;
const
 strict_prot=False;
begin
 if strict_prot then
 begin
  Result:=((prot shr VM_PROT_GPU_SHIFT) and VM_RW);
 end else
 begin
  Result:=VM_RW;
 end;
end;

function fixup_prot(prot:vm_prot_t):vm_prot_t; inline;
begin
 //fixup writeonly cpu/gpu
 Result:=prot or ((prot and (VM_PROT_WRITE or VM_PROT_GPU_WRITE)) shr 1);
end;

procedure pmap_copy_pages(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prot :vm_prot_t);
var
 delta:QWORD;

 info:t_fd_info;

 lock:Pointer;

 r:Integer;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_copy_pages:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
 end;

 prot:=fixup_prot(prot);

 lock:=pmap_wlock(pmap,start,__end);

  ppmap_mark_rwx(start,__end,prot);

  info.start:=start;
  info.__end:=__end;

  while (info.start<>info.__end) do
  begin
   get_priv_fd(pmap,info);

   delta:=(info.__end-info.start);
   if (delta=0) then Break;

   pmap_copy_dst(Pointer(info.start),
                 info.obj,
                 info.olocal, //block local offset
                 delta);

   //remove backing
   vm_nt_map_delete(@pmap^.nt_map,info.start,info.__end);

   //map to guest
   r:=vm_nt_map_insert(@pmap^.nt_map,
                       info.obj,
                       info.olocal, //block local offset
                       info.start,
                       info.__end,
                       delta,
                       (prot and VM_RW));

   if (r<>0) then
   begin
    Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
    Assert(false,'pmap_enter_object');
   end;

   //map to GPU
   if (prot and VM_PROT_GPU_ALL)<>0 then
   begin
    //extra obj link
    vm_nt_file_obj_reference(info.obj);
    //
    r:=vm_nt_map_insert(@pmap^.gp_map,
                        info.obj,
                        info.olocal, //block local offset
                        info.start+VM_MIN_GPU_ADDRESS,
                        info.__end+VM_MIN_GPU_ADDRESS,
                        delta,
                        convert_to_gpu_prot(prot));

    if (r<>0) then
    begin
     Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
     Assert(false,'pmap_enter_object');
    end;
   end;

   info.start :=info.start+delta;
   info.__end :=__end;

  end; //while

 pmap_unlock(pmap,lock);
end;

procedure pmap_enter_object(pmap  :pmap_t;
                            obj   :vm_object_t;
                            offset:vm_ooffset_t;
                            start :vm_offset_t;
                            __end :vm_offset_t;
                            prot  :vm_prot_t);
label
 _default;
var
 fd:THandle;
 md:THandle;

 size:QWORD;
 delta:QWORD;
 paddi:QWORD;

 info:t_fd_info;
 cow :p_vm_nt_file_obj;

 lock:Pointer;

 max:Integer;

 r:Integer;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_enter_object:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
 end;

 prot:=fixup_prot(prot);

 lock:=pmap_wlock(pmap,start,__end);

 ppmap_mark_rwx(start,__end,prot);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

      Assert((prot and VM_PROT_COPY)=0);

      info.start:=start;
      info.__end:=__end;

      while (info.start<>info.__end) do
      begin
       get_priv_fd(pmap,info);

       delta:=(info.__end-info.start);
       if (delta=0) then Break;

       //map to guest
       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.olocal, //block local offset
                           info.start,
                           info.__end,
                           delta,
                           (prot and VM_RW));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       //map to GPU
       if (prot and VM_PROT_GPU_ALL)<>0 then
       begin
        //extra obj link
        vm_nt_file_obj_reference(info.obj);
        //
        r:=vm_nt_map_insert(@pmap^.gp_map,
                            info.obj,
                            info.olocal, //block local offset
                            info.start+VM_MIN_GPU_ADDRESS,
                            info.__end+VM_MIN_GPU_ADDRESS,
                            delta,
                            convert_to_gpu_prot(prot));

        if (r<>0) then
        begin
         Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
         Assert(false,'pmap_enter_object');
        end;
       end;

       info.start :=info.start+delta;
       info.__end :=__end;
      end;

    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     Assert((prot and VM_PROT_COPY)=0);

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin

      if (p_print_pmap) then
      begin
       Writeln('pmap_enter_gpuobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(offset,11),':',HexStr(prot,2));
      end;

      info.start :=start;
      info.__end :=__end;
      info.offset:=offset;

      while (info.start<>info.__end) do
      begin
       get_dmem_fd(info);

       delta:=(info.__end-info.start);
       if (delta=0) then Break;

       if (p_print_pmap) then
       begin
        Writeln('vm_nt_map_insert:',HexStr(info.start,11),':',HexStr(info.__end,11),':',HexStr(info.offset,11));
       end;

       //map to guest
       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.olocal, //block local offset
                           info.start,
                           info.__end,
                           delta,
                           (prot and VM_RW));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       //map to GPU
       if (prot and VM_PROT_GPU_ALL)<>0 then
       begin
        //extra obj link
        vm_nt_file_obj_reference(info.obj);
        //
        r:=vm_nt_map_insert(@pmap^.gp_map,
                            info.obj,
                            info.olocal, //block local offset
                            info.start+VM_MIN_GPU_ADDRESS,
                            info.__end+VM_MIN_GPU_ADDRESS,
                            delta,
                            convert_to_gpu_prot(prot));

        if (r<>0) then
        begin
         Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
         Assert(false,'pmap_enter_object');
        end;
       end;

       info.start :=info.start +delta;
       info.__end :=__end;
       info.offset:=info.offset+delta;
      end;

     end else
     begin

      if (p_print_pmap) then
      begin
       Writeln('pmap_enter_devobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(offset,11),':',HexStr(prot,2));
      end;

      info.start :=start;
      info.__end :=__end;
      info.offset:=offset;

      get_dev_fd(info,obj^.un_pager.map_base);

      delta:=(info.__end-info.start);

      if (p_print_pmap) then
      begin
       Writeln('vm_nt_map_insert:',HexStr(info.start,11),':',HexStr(info.__end,11),':',HexStr(info.offset,11));
      end;

      //map to guest
      r:=vm_nt_map_insert(@pmap^.nt_map,
                          info.obj,
                          info.olocal, //block local offset
                          info.start,
                          info.__end,
                          delta,
                          (prot and VM_RW));

      if (r<>0) then
      begin
       Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
       Assert(false,'pmap_enter_object');
      end;

      //map to GPU
      if (prot and VM_PROT_GPU_ALL)<>0 then
      begin
       //extra obj link
       vm_nt_file_obj_reference(info.obj);
       //
       r:=vm_nt_map_insert(@pmap^.gp_map,
                           info.obj,
                           info.olocal, //block local offset
                           info.start+VM_MIN_GPU_ADDRESS,
                           info.__end+VM_MIN_GPU_ADDRESS,
                           delta,
                           convert_to_gpu_prot(prot));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;
      end;

     end;

    end;
  OBJT_VNODE:
    begin
     delta:=0;
     paddi:=0;
     md:=0;

     VM_OBJECT_LOCK(obj);

       fd:=get_vnode_handle(obj,max);

       if (fd<>0) then
       begin
        size:=fit_to_vnode_size(obj,offset,(__end-start));

        if (max<>VM_RW) then
        begin
         //reopen file to RW
         r:=md_openat(fd,'',O_RDWR,0,fd);

         if (r=0) then
         begin
          max:=VM_RW;
          r:=md_memfd_open(md,fd,max);
          md_close(fd); //close dub
         end;

        end else
        begin
         r:=md_memfd_open(md,fd,max);
        end;

       end;

     VM_OBJECT_UNLOCK(obj);

     if (r<>0) then
     begin
      Writeln('failed md_memfd_open:0x',HexStr(r,8));
      Assert(false,'pmap_enter_object');
     end;

     if (md=0) then
     begin
      Writeln('zero file fd');
      Assert(false,'pmap_enter_object');
     end;

     //align host page
     paddi:=(size+(MD_PAGE_SIZE-1)) and (not (MD_PAGE_SIZE-1));

     info.obj   :=vm_nt_file_obj_allocate(md,max);
     info.offset:=offset;
     info.start :=start;
     info.__end :=start+paddi;

     //map to guest
     r:=vm_nt_map_insert(@pmap^.nt_map,
                         info.obj,
                         info.offset, //offset in file
                         info.start,
                         info.__end,
                         size,
                         (prot and VM_RW));

     if (r<>0) then
     begin
      Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
      Assert(false,'pmap_enter_object');
     end;

     //map to GPU
     if (prot and VM_PROT_GPU_ALL)<>0 then
     begin
      //extra obj link
      vm_nt_file_obj_reference(info.obj);
      //
      r:=vm_nt_map_insert(@pmap^.gp_map,
                          info.obj,
                          info.offset, //offset in file
                          info.start+VM_MIN_GPU_ADDRESS,
                          info.__end+VM_MIN_GPU_ADDRESS,
                          size,
                          convert_to_gpu_prot(prot));

      if (r<>0) then
      begin
       Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
       Assert(false,'pmap_enter_object');
      end;
     end;

     ppmap_mark_rwx(info.start,info.__end,prot);

     //upper pages
     delta:=(paddi and PAGE_MASK);

     if (delta<>0) then
     begin
      offset:=0;
      start:=start+paddi;
      prot:=prot and (not VM_PROT_COPY);
      goto _default;
     end;

    end;
  else
    begin
     Writeln('TODO:',vm_object_type(obj));
     Assert(False);
    end;
 end;

 pmap_unlock(pmap,lock);
end;

procedure pmap_gpu_enter_object(pmap :pmap_t;
                                start:vm_offset_t;
                                __end:vm_offset_t;
                                prot :vm_prot_t);
var
 lock:Pointer;

 r:Integer;

 p__start:vm_offset_t;
 p____end:vm_offset_t;
 p_offset:vm_offset_t;
 p____obj:p_vm_nt_file_obj;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_gpu_enter_object:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
 end;

 prot:=fixup_prot(prot);

 lock:=pmap_wlock(pmap,start,__end);

 while (start<__end) do
 begin
  if not vm_nt_map_fetch(@pmap^.nt_map,
                         start,
                         __end,
                         p__start,
                         p____end,
                         p_offset,
                         p____obj
                        ) then
  begin
   Assert(false,'vm_nt_map_fetch');
  end;

  if (p__start>start) then
  begin
   start:=p__start;
  end;

  p_offset:=p_offset+(start-p__start);

  if (p____end>__end) then
  begin
   p____end:=__end;
  end;

  //map to GPU
  if (p____obj<>nil) then
  begin
   //extra obj link
   vm_nt_file_obj_reference(p____obj);
   //
   r:=vm_nt_map_insert(@pmap^.gp_map,
                       p____obj,
                       p_offset, //block local offset
                       start   +VM_MIN_GPU_ADDRESS,
                       p____end+VM_MIN_GPU_ADDRESS,
                       (p____end-start),
                       convert_to_gpu_prot(prot));

   if (r<>0) then
   begin
    Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
    Assert(false,'pmap_gpu_enter_object');
   end;
  end;

  start:=p____end;
 end;

 pmap_unlock(pmap,lock);
end;

procedure pmap_enter_dmem_block(pmap  :pmap_t;
                                offset:vm_ooffset_t;
                                start :vm_offset_t;
                                prot  :vm_prot_t);
var
 __end:vm_offset_t;

 delta:QWORD;

 info:t_fd_info;

 lock:Pointer;

 r:Integer;
begin
 __end:=start+(64*1024);

 if (p_print_pmap) then
 begin
  Writeln('pmap_enter_dmem_block:',HexStr(offset,11),':',HexStr(start,11),':',HexStr(prot,2));
 end;

 prot:=fixup_prot(prot);

 lock:=pmap_wlock(pmap,start,__end);

 ppmap_mark_rwx(start,__end,prot);

 r:=0;

 info.start :=start;
 info.__end :=__end;
 info.offset:=offset;

 while (info.start<>info.__end) do
 begin
  get_dmem_fd(info);

  delta:=(info.__end-info.start);
  if (delta=0) then Break;

  if (p_print_pmap) then
  begin
   Writeln('vm_nt_map_insert:',HexStr(info.start,11),':',HexStr(info.__end,11),':',HexStr(info.offset,11));
  end;

  //map to guest
  r:=vm_nt_map_insert(@pmap^.nt_map,
                      info.obj,
                      info.olocal, //block local offset
                      info.start,
                      info.__end,
                      delta,
                      (prot and VM_RW));

  if (r<>0) then
  begin
   Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
   Assert(false,'pmap_enter_object');
  end;

  //map to GPU
  if (prot and VM_PROT_GPU_ALL)<>0 then
  begin
   //extra obj link
   vm_nt_file_obj_reference(info.obj);
   //
   r:=vm_nt_map_insert(@pmap^.gp_map,
                       info.obj,
                       info.olocal, //block local offset
                       info.start+VM_MIN_GPU_ADDRESS,
                       info.__end+VM_MIN_GPU_ADDRESS,
                       delta,
                       convert_to_gpu_prot(prot));

   if (r<>0) then
   begin
    Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
    Assert(false,'pmap_enter_object');
   end;
  end;

  info.start :=info.start +delta;
  info.__end :=__end;
  info.offset:=info.offset+delta;
 end;

 pmap_unlock(pmap,lock);
end;

procedure pmap_gpu_get_bound(pmap:pmap_t;
                             var start:vm_offset_t;
                             var __end:vm_offset_t);
var
 lock:Pointer;

 i_start:vm_offset_t;
 i___end:vm_offset_t;

 min_start:vm_offset_t;
 max___end:vm_offset_t;

 p__start:vm_offset_t;
 p____end:vm_offset_t;
 p_offset:vm_offset_t;
 p____obj:p_vm_nt_file_obj;
begin
 i_start:=start;
 i___end:=__end;

 min_start:=0;
 max___end:=0;

 lock:=pmap_wlock(pmap,start,__end);

 while (i_start<i___end) do
 begin

  if vm_nt_map_fetch(@pmap^.gp_map,
                     i_start,
                     i___end,
                     p__start,
                     p____end,
                     p_offset,
                     p____obj
                    ) then
  begin
   //is hole space
   if (p__start>i_start) then
   begin
    Break;
   end;

   if (min_start=0) then
   begin
    min_start:=p__start;
   end;

   max___end:=p____end;
  end else
  begin
   //not found
   Break;
  end;

  i_start:=p____end;
 end;

 pmap_unlock(pmap,lock);

 start:=min_start;
 __end:=max___end;
end;

procedure pmap_protect(pmap :pmap_t;
                       obj  :vm_object_t;
                       start:vm_offset_t;
                       __end:vm_offset_t;
                       prot :vm_prot_t);
var
 lock:Pointer;
label
 _default;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':prot:',HexStr(prot,2));
 end;

 prot:=fixup_prot(prot);

 lock:=pmap_rlock(pmap,start,__end);

 ppmap_mark_rwx(start,__end,prot);

 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     //map to guest
     vm_nt_map_protect(@pmap^.nt_map,
                       start,
                       __end,
                       (prot and VM_RW));

    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     if (p_print_pmap) then
     begin
      if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
      begin
       Writeln('pmap_protect_gpuobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
      end else
      begin
       Writeln('pmap_protect_devobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
      end;
     end;

     goto _default;
    end;
  OBJT_VNODE:
    begin
     goto _default;
    end;
  else
    begin
     Writeln('TODO:',vm_object_type(obj));
     Assert(False);
    end;
 end;

 pmap_unlock(pmap,lock);
end;

procedure pmap_gpu_protect(pmap :pmap_t;
                           start:vm_offset_t;
                           __end:vm_offset_t;
                           prot :vm_prot_t);
var
 lock:Pointer;
begin
 prot:=fixup_prot(prot);

 lock:=pmap_rlock(pmap,start,__end);

 //map to GPU
 vm_nt_map_protect(@pmap^.gp_map,
                   start+VM_MIN_GPU_ADDRESS,
                   __end+VM_MIN_GPU_ADDRESS,
                   convert_to_gpu_prot(prot));


 pmap_unlock(pmap,lock);
end;

procedure pmap_prot_track(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prot :Byte); public;
var
 lock:Pointer;
begin
 //exit;

 if (p_print_pmap) then
 begin
  Writeln('pmap_prot_track:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
 end;

 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 lock:=pmap_wlock(pmap,start,__end);

 ppmap_track(start,__end,prot);

 vm_nt_map_tracking(@pmap^.nt_map,
                    start,
                    __end,
                    prot);

 pmap_unlock(pmap,lock);
end;

procedure pmap_prot_restore(pmap :pmap_t;
                            start:vm_offset_t;
                            __end:vm_offset_t);
var
 lock:Pointer;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_prot_restore:',HexStr(start,11),':',HexStr(__end,11));
 end;

 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 lock:=pmap_wlock(pmap,start,__end);

 vm_nt_map_prot_fix(@pmap^.nt_map,
                    start,
                    __end,
                    REMAP_PROT);

 pmap_unlock(pmap,lock);
end;

procedure pmap_madvise(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       advise:Integer);
label
 _default;
var
 lock:Pointer;

 r:Integer;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_madvise:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(advise,2));
 end;

 lock:=pmap_wlock(pmap,start,__end);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     vm_nt_map_madvise(@pmap^.nt_map,
                       start,
                       __end,
                       advise);
    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;
     //ignore
    end;
  OBJT_PHYSHM:
    begin
     //ignore
    end;
  OBJT_VNODE:
    begin
     //ignore
    end;
  else
    begin
     Writeln('TODO:',vm_object_type(obj));
     Assert(False);
    end;
 end;

 if (r<>0) then
 begin
  Writeln('failed md_reset:0x',HexStr(r,8));
  Assert(false,'pmap_madvise');
 end;

 pmap_unlock(pmap,lock);
end;

procedure unmap_dmem_gc(start,__end:QWORD); external;

procedure pmap_remove(pmap :pmap_t;
                      obj  :vm_object_t;
                      start:vm_offset_t;
                      __end:vm_offset_t);
label
 _default;
var
 lock:Pointer;

 r:Integer;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_remove:',HexStr(start,11),':',HexStr(__end,11));
 end;

 lock:=pmap_wlock(pmap,start,__end);

 ppmap_unmark(start,__end,VM_PROT_ALL);

 vm_track_map_remove_memory(@pmap^.tr_map,start,__end);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin

     _default:

     r:=vm_nt_map_delete(@pmap^.nt_map,
                         start,
                         __end);

     if (r<>0) then
     begin
      Writeln('failed vm_nt_map_delete:0x',HexStr(r,8));
      Assert(false,'pmap_remove');
     end;

     r:=vm_nt_map_delete(@pmap^.gp_map,
                         start+VM_MIN_GPU_ADDRESS,
                         __end+VM_MIN_GPU_ADDRESS);

     if (r<>0) then
     begin
      Writeln('failed vm_nt_map_delete:0x',HexStr(r,8));
      Assert(false,'pmap_remove');
     end;

    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     if (p_print_pmap) then
     begin
      if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
      begin
       Writeln('pmap_remove_gpuobj:',HexStr(start,11),':',HexStr(__end,11));
      end else
      begin
       Writeln('pmap_remove_devobj:',HexStr(start,11),':',HexStr(__end,11));
      end;
     end;

     goto _default;
    end;
  OBJT_VNODE:
    begin
     goto _default;
    end;
  else
    begin
     Writeln('TODO:',vm_object_type(obj));
     Assert(False);
    end;
 end;

 pmap_unlock(pmap,lock);

 unmap_dmem_gc(start+VM_MIN_GPU_ADDRESS,
               __end+VM_MIN_GPU_ADDRESS);
end;

procedure pmap_gpu_remove(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t);
var
 lock:Pointer;

 r:Integer;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_gpu_remove:',HexStr(start,11),':',HexStr(__end,11));
 end;

 lock:=pmap_wlock(pmap,start,__end);

 r:=vm_nt_map_delete(@pmap^.gp_map,
                     start+VM_MIN_GPU_ADDRESS,
                     __end+VM_MIN_GPU_ADDRESS);

 if (r<>0) then
 begin
  Writeln('failed vm_nt_map_delete:0x',HexStr(r,8));
  Assert(false,'pmap_gpu_remove');
 end;

 pmap_unlock(pmap,lock);
end;

function pmap_mirror_map(pmap :pmap_t;
                         start:vm_offset_t;
                         __end:vm_offset_t):Pointer;
var
 lock:Pointer;
begin
 lock:=pmap_rlock(pmap,start,__end);

 Result:=vm_nt_map_mirror(@pmap^.nt_map,
                          start,
                          __end);

 pmap_unlock(pmap,lock);
end;

procedure pmap_mirror_unmap(pmap:pmap_t;
                            base:Pointer;
                            size:QWORD);
var
 r:Integer;
begin
 r:=md_placeholder_unmap(base,size);
 if (r<>0) then
 begin
  Writeln('failed md_placeholder_unmap:0x',HexStr(r,8));
  Assert(false,'pmap_mirror_unmap');
 end;
end;

function pmap_danger_zone(pmap:pmap_t;
                          addr:vm_offset_t;
                          size:vm_offset_t):Boolean;
begin
 Result:=False;
 while (pmap^.nt_map.danger_zone.in_range(addr,size)) do
 begin
  Result:=True;
  pmap^.nt_map.danger_zone.d_wait(addr,size);
 end;
end;

function pmap_expand(pmap :pmap_t;
                     start:vm_offset_t;
                     __end:vm_offset_t):Boolean;
var
 base:Pointer;
 lock:Pointer;
 r:Integer;
begin
 Result:=True;

 if (p_print_pmap) then
 begin
  Writeln('pmap_expand:',HexStr(start,11),':',HexStr(__end,11));
 end;

 base:=Pointer(start);

 lock:=pmap_wlock(pmap,start,__end);

  r:=md_placeholder_mmap(base,__end-start,MD_MAP_FIXED);

  if (r<>0) then
  begin
   Writeln('failed md_placeholder_mmap:0x',HexStr(r,8));
   Assert(false,'pmap_expand');
   Result:=False;
  end;

  if Result then
  begin
   //mark used regions as free
   vm_nt_map_delete(@pmap^.nt_map,start,__end);
  end;

 pmap_unlock(pmap,lock);
end;

end.


