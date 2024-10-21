unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_backtrace,
 vm,
 vmparam,
 sys_vm_object,
 vnode,
 vuio,
 kern_mtx,
 kern_rangelock,
 md_map,
 vm_pmap_prot,
 vm_tracking_map,
 vm_nt_map;

const
 PMAPP_BLK_SHIFT =29;
 PMAPP_BLK_SIZE  =QWORD(QWORD(1) shl PMAPP_BLK_SHIFT);
 PMAPP_BLK_MASK  =PMAPP_BLK_SIZE-1;

 PMAPP_BLK_PRIV_BLOCKS=QWORD(VM_MAXUSER_ADDRESS                   ) shr PMAPP_BLK_SHIFT;
 PMAPP_BLK_DMEM_BLOCKS=QWORD(VM_MAX_GPU_ADDRESS-VM_MIN_GPU_ADDRESS) shr PMAPP_BLK_SHIFT;

var
 PRIV_FD:array[0..PMAPP_BLK_PRIV_BLOCKS-1] of vm_nt_file_obj;
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
  rmlock:rangelock;
  rm_mtx:mtx;
  nt_map:t_vm_nt_map;
  tr_map:t_vm_track_map;
 end;

 pmap_t=p_pmap;

 t_pmap_reserve_result=record
  error:DWORD;
  base :Pointer;
  size :QWORD;
 end;

function  atop(x:QWORD):DWORD; inline;
function  ptoa(x:DWORD):QWORD; inline;

function  ctob(x:QWORD):QWORD; inline;
function  btoc(x:QWORD):QWORD; inline;

function  dev_mem_alloc(pages:Integer):Pointer;

function  pmap_reserve:t_pmap_reserve_result;

procedure pmap_pinit(pmap:p_pmap);

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

procedure pmap_enter_object(pmap   :pmap_t;
                            obj    :vm_object_t;
                            offset :vm_ooffset_t;
                            start  :vm_offset_t;
                            __end  :vm_offset_t;
                            prot   :vm_prot_t);

procedure pmap_protect(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       prot  :vm_prot_t);

{
procedure _pmap_prot_fix(pmap :pmap_t;
                         start:vm_offset_t;
                         __end:vm_offset_t;
                         mode :Integer);

procedure _pmap_prot_int(pmap :pmap_t;
                         start:vm_offset_t;
                         __end:vm_offset_t;
                         prot :vm_prot_t);
}

procedure pmap_prot_track(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prots:Byte);

procedure pmap_prot_restore(pmap :pmap_t;
                            start:vm_offset_t;
                            __end:vm_offset_t);

procedure pmap_madvise(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       advise:Integer);

procedure pmap_remove(pmap  :pmap_t;
                      obj   :vm_object_t;
                      start :vm_offset_t;
                      __end :vm_offset_t);

function  pmap_mirror_map(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t):Pointer;

procedure pmap_mirror_unmap(pmap:pmap_t;
                            base:Pointer;
                            size:QWORD);

function  pmap_danger_zone(pmap:pmap_t;
                           addr:vm_offset_t;
                           size:vm_offset_t):Boolean;

implementation

uses
 ntapi,
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

procedure dmem_init;
var
 base:Pointer;
 i,r:Integer;
begin
 for i:=0 to PMAPP_BLK_DMEM_BLOCKS-2 do
 begin
  base:=Pointer(VM_MIN_GPU_ADDRESS+i*PMAPP_BLK_SIZE);
  r:=md_split(base,PMAPP_BLK_SIZE);
  if (r<>0) then
  begin
   Writeln('failed md_split(',HexStr(base),',',HexStr(base+PMAPP_BLK_SIZE),'):0x',HexStr(r,8));
   Assert(false,'dmem_init');
  end;
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
  Assert(false,'dev_mem_init');
 end;

 DEV_INFO.DEV_FD.maxp:=VM_RW;

 DEV_INFO.DEV_PTR:=Pointer(VM_MIN_DEV_ADDRESS);
 r:=md_reserve_ex(DEV_INFO.DEV_PTR,DEV_INFO.DEV_SIZE);
 if (r<>0) then
 begin
  Writeln('failed md_reserve(',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init');
 end;

 DEV_INFO.DEV_PTR:=Pointer(VM_MIN_DEV_ADDRESS); //force

 r:=md_split(DEV_INFO.DEV_PTR,DEV_INFO.DEV_SIZE);
 if (r<>0) then
 begin
  Writeln('failed md_split(',HexStr(DEV_INFO.DEV_PTR),',',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init');
 end;

 r:=md_file_mmap_ex(DEV_INFO.DEV_FD.hfile,DEV_INFO.DEV_PTR,0,DEV_INFO.DEV_SIZE,VM_RW);
 if (r<>0) then
 begin
  Writeln('failed md_file_mmap_ex(',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init');
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

function pmap_reserve:t_pmap_reserve_result;
var
 base:Pointer;
 size:QWORD;
 i:Integer;
begin
 Result:=Default(t_pmap_reserve_result);

 if Length(pmap_mem)<>0 then
 begin
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(pmap_mem[i].start);

   //try union range
   if (base=Pointer(DL_AREA_START)) then
   begin
    size:=VM_MAXUSER_ADDRESS-DL_AREA_START;

    Result.error:=md_reserve_ex(base,size);
    if (Result.error=0) then
    begin
     //union range
     pmap_mem[i+0].__end:=VM_MAXUSER_ADDRESS;
     pmap_mem[i+1].start:=VM_MAXUSER_ADDRESS;
     //
     Break;
    end;
   end;

   size:=pmap_mem[i].__end-pmap_mem[i].start;

   Result.error:=md_reserve_ex(base,size);

   if (Result.error<>0) then
   begin
    Result.base:=base;
    Result.size:=size;
    //STATUS_COMMITMENT_LIMIT = $C000012D
    Exit;
   end;

   //update start region
   pmap_mem[i].start:=QWORD(base);

   {
   if wr then
   begin
    Writeln('md_reserve_ex(',HexStr(base),',',HexStr(base+size),'):0x',HexStr(Result,8));
   end;
   }
  end;
 end;

 //dmem mirror
 base:=Pointer(VM_MIN_GPU_ADDRESS);
 size:=VM_MAX_GPU_ADDRESS-VM_MIN_GPU_ADDRESS;

 Result.error:=md_reserve_ex(base,size);

 if (Result.error<>0) then
 begin
  Result.base:=base;
  Result.size:=size;
  Exit;
 end;
end;

procedure pmap_pinit(pmap:p_pmap);
var
 i,r:Integer;
 m:t_pmap_reserve_result;
begin
 m:=pmap_reserve;
 if (m.error<>0) then
 begin
  Writeln('failed md_reserve_ex(',HexStr(m.base),',',HexStr(m.base+m.size),'):0x',HexStr(m.error,8));
  Assert(false,'pmap_pinit');
  Exit;
 end;

 dmem_init;
 dev_mem_init;

 if (PAGE_PROT=nil) then
 begin
  r:=md_mmap(PAGE_PROT,PAGE_MAP_COUNT,VM_RW);

  if (r<>0) then
  begin
   Writeln('failed md_mmap(',HexStr(PAGE_MAP_COUNT,11),'):0x',HexStr(r,8));
   Assert(false,'pmap_pinit');
  end;
 end;

 rangelock_init(@pmap^.rmlock);
 mtx_init(pmap^.rm_mtx,'pmap');

 vm_nt_map_init(@pmap^.nt_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS);

  //exclude
 if Length(pmap_mem)>1 then
 begin
  For i:=0 to High(pmap_mem)-1 do
  begin
   vm_nt_map_insert(@pmap^.nt_map,
                    nil,0,
                    pmap_mem[  i].__end,
                    pmap_mem[i+1].start,
                    pmap_mem[i+1].start-pmap_mem[i].__end,
                    0);
  end;
 end;

 vm_track_map_init(@pmap^.tr_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS);

 pmap^.tr_map.pmap:=pmap;
end;

type
 t_fd_info=record
  start :QWORD;
  __end :QWORD;
  obj   :p_vm_nt_file_obj;
  offset:QWORD;
 end;

function get_priv_block_count:Integer;
var
 i:Integer;
begin
 Result:=0;
 For i:=0 to High(PRIV_FD) do
 begin
  if (PRIV_FD[i].hfile<>0) then
  begin
   Inc(Result);
  end;
 end;
end;

procedure get_priv_fd(var info:t_fd_info);
var
 o:QWORD;
 e:QWORD;
 d:QWORD;
 i:DWORD;
 r:DWORD;
begin
 o:=info.start;

 //current block id
 i:=o shr PMAPP_BLK_SHIFT;

 if (PRIV_FD[i].hfile=0) then
 begin
  R:=md_memfd_create(PRIV_FD[i].hfile,PMAPP_BLK_SIZE,VM_RW);

  PRIV_FD[i].flags:=NT_FILE_FREE;
  PRIV_FD[i].maxp :=VM_RW;

  if (r<>0) then
  begin
   Writeln('failed md_memfd_create(',HexStr(PMAPP_BLK_SIZE,11),'):0x',HexStr(r,8));
   Writeln(' priv_block_count=',get_priv_block_count);

   print_backtrace_td(stderr);

   Assert(false,'get_priv_fd');
  end;
 end;

 info.obj:=@PRIV_FD[i];

 vm_nt_file_obj_reference(info.obj);

 //current block offset
 o:=o and PMAPP_BLK_MASK;

 //mem size
 d:=info.__end-info.start;

 //max offset
 e:=o+d;

 // |start         end|
 // |offset  |max
 if (e>PMAPP_BLK_SIZE) then
 begin
  e:=PMAPP_BLK_SIZE-o;
  e:=e+info.start;
  info.__end:=e;
 end;
end;

procedure get_dmem_fd(var info:t_fd_info);
var
 base:Pointer;
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

 if (o>=(VM_MIN_DEV_ADDRESS-VM_MIN_GPU_ADDRESS)) then
 begin
  //dev
  BLK_SIZE:=DEV_INFO.DEV_SIZE;

  info.obj:=@DEV_INFO.DEV_FD;
 end else
 begin
  //dmem
  BLK_SIZE:=PMAPP_BLK_SIZE;

  //current block id
  i:=o shr PMAPP_BLK_SHIFT;

  if (DMEM_FD[i].hfile=0) then
  begin
   R:=md_memfd_create(DMEM_FD[i].hfile,BLK_SIZE,VM_RW);

   DMEM_FD[i].maxp:=VM_RW;

   if (r<>0) then
   begin
    Writeln('failed md_memfd_create(',HexStr(BLK_SIZE,11),'):0x',HexStr(r,8));
    Assert(false,'get_dmem_fd');
   end;

   //dmem mirror
   base:=Pointer(VM_MIN_GPU_ADDRESS+i*PMAPP_BLK_SIZE);
   r:=md_file_mmap_ex(DMEM_FD[i].hfile,base,0,BLK_SIZE,VM_RW);
   if (r<>0) then
   begin
    Writeln('failed md_file_mmap_ex(',HexStr(base),',',HexStr(base+BLK_SIZE),'):0x',HexStr(r,8));
    Assert(false,'get_dmem_fd');
   end;
  end;

  info.obj:=@DMEM_FD[i];
 end;

 vm_nt_file_obj_reference(info.obj);

 //current block offset
 o:=o and PMAPP_BLK_MASK;

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

function get_vnode_handle(obj:vm_object_t):THandle;
var
 vp:p_vnode;
begin
 Result:=0;

 vp:=obj^.handle;

 if (vp<>nil) then
 begin
  VI_LOCK(vp);

  Result:=THandle(vp^.v_un);

  VI_UNLOCK(vp);
 end;
end;

function pmap_wlock(pmap :pmap_t;
                    start:vm_offset_t;
                    __end:vm_offset_t):Pointer;
begin
 //Writeln('pmap_wlock:',HexStr(start,10),'..',HexStr(__end,10));

 Result:=rangelock_wlock(@pmap^.rmlock,start,__end,@pmap^.rm_mtx);
end;

function pmap_rlock(pmap :pmap_t;
                    start:vm_offset_t;
                    __end:vm_offset_t):Pointer;
begin
 //Writeln('pmap_rlock:',HexStr(start,10),'..',HexStr(__end,10));

 Result:=rangelock_rlock(@pmap^.rmlock,start,__end,@pmap^.rm_mtx);
end;

procedure pmap_unlock(pmap:pmap_t;cookie:Pointer);
begin
 //Writeln('pmap_unlock:',HexStr(p_rl_q_entry(cookie)^.rl_q_start,10),'..',HexStr(p_rl_q_entry(cookie)^.rl_q_end,10));

 rangelock_unlock(@pmap^.rmlock,cookie,@pmap^.rm_mtx);
end;

procedure pmap_copy(src_obj :p_vm_nt_file_obj;
                    src_ofs :vm_ooffset_t;
                    dst_obj :p_vm_nt_file_obj;
                    dst_ofs :vm_ooffset_t;
                    size    :vm_ooffset_t;
                    max_size:vm_ooffset_t);
var
 start :vm_ooffset_t;
 __end :vm_ooffset_t;
 src,dst:Pointer;
 r:Integer;
begin
 if (size>max_size) then
 begin
  size:=max_size;
 end;

 start  :=src_ofs and (not (MD_ALLOC_GRANULARITY-1)); //dw
 __end  :=src_ofs+size; //up
 src_ofs:=src_ofs and (MD_ALLOC_GRANULARITY-1);

 src:=nil;
 r:=md_file_mmap(src_obj^.hfile,src,start,__end-start,VM_PROT_READ);

 if (r<>0) then
 begin
  Writeln('failed md_file_mmap:0x',HexStr(r,8));
  Assert(false,'pmap_copy');
 end;

 start  :=dst_ofs and (not (MD_ALLOC_GRANULARITY-1)); //dw
 __end  :=dst_ofs+size; //up
 dst_ofs:=dst_ofs and (MD_ALLOC_GRANULARITY-1);

 dst:=nil;
 r:=md_file_mmap(dst_obj^.hfile,dst,start,__end-start,VM_RW);

 if (r<>0) then
 begin
  Writeln('failed md_file_mmap:0x',HexStr(r,8));
  Assert(false,'pmap_copy');
 end;

 Move((src+src_ofs)^,(dst+dst_ofs)^,size);

 md_cacheflush(dst,size,DCACHE);

 r:=md_file_unmap(dst,0);

 if (r<>0) then
 begin
  Writeln('failed md_file_unmap:0x',HexStr(r,8));
  Assert(false,'pmap_copy');
 end;

 r:=md_file_unmap(src,0);

 if (r<>0) then
 begin
  Writeln('failed md_file_unmap:0x',HexStr(r,8));
  Assert(false,'pmap_copy');
 end;
end;

{
 * Maps a sequence of resident pages belonging to the same object.
 * The sequence begins with the given page m_start.  This page is
 * mapped at the given virtual address start.  Each subsequent page is
 * mapped at a virtual address that is offset from start by the same
 * amount as the page is offset from m_start within the object.  The
 * last page in the sequence is the page with the largest offset from
 * m_start that can be mapped at a virtual address less than the given
 * virtual address end.  Not every virtual page between start and end
 * is mapped; only those for which a resident page exists with the
 * corresponding offset from m_start are mapped.
}
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
       get_priv_fd(info);

       delta:=(info.__end-info.start);
       if (delta=0) then Break;

       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.start and PMAPP_BLK_MASK, //block local offset
                           info.start,
                           info.__end,
                           delta,
                           (prot and VM_RW));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       //fill zero if needed
       vm_nt_map_madvise(@pmap^.nt_map,
                         info.start,
                         info.__end,
                         MADV_NORMAL);

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
      //transform by base addr
      offset:=offset + (QWORD(obj^.un_pager.map_base) - VM_MIN_GPU_ADDRESS);

      if (p_print_pmap) then
      begin
       Writeln('pmap_enter_gpuobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(offset,11),':',HexStr(prot,2));
      end;

      info.start:=start;
      info.__end:=__end;
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

       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.offset and PMAPP_BLK_MASK, //block local offset
                           info.start,
                           info.__end,
                           delta,
                           (prot and VM_RW));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       info.start :=info.start +delta;
       info.__end :=__end;
       info.offset:=info.offset+delta;
      end;

     end else
     begin
      Assert(false,'non dmem OBJT_DEVICE');
     end;

    end;
  OBJT_VNODE:
    begin
     delta:=0;
     paddi:=0;
     md:=0;

     VM_OBJECT_LOCK(obj);

       fd:=get_vnode_handle(obj);

       if (fd<>0) then
       begin
        delta:=(__end-start);

        //max unaligned size
        size:=offset+delta;
        if (size>obj^.un_pager.vnp.vnp_size) then
        begin
         size:=obj^.un_pager.vnp.vnp_size;
        end;
        size:=size-offset;

        max:=VM_PROT_RW;
        r:=md_memfd_open(md,fd,max);

        if (DWORD(r)=STATUS_ACCESS_DENIED) then
        begin
         max:=VM_PROT_READ;
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

     if ((prot and VM_PROT_COPY)<>0) then
     begin
      if (p_print_pmap) then
      begin
       Writeln('pmap_enter_cowobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
      end;

      cow:=vm_nt_file_obj_allocate(md,VM_PROT_READ);

      info.offset:=offset;
      info.start :=start;
      info.__end :=start+paddi;

      while (info.start<>info.__end) do
      begin
       get_priv_fd(info);

       delta:=(info.__end-info.start);
       if (delta=0) then Break;

       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.start and PMAPP_BLK_MASK, //block local offset
                           info.start,
                           info.__end,
                           delta,
                           (prot and VM_RW));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       //restore
       vm_nt_map_madvise(@pmap^.nt_map,
                         info.start,
                         info.__end,
                         MADV_WILLNEED);

       //copy
       pmap_copy(cow,
                 info.offset,
                 info.obj,
                 info.start and PMAPP_BLK_MASK, //block local offset
                 delta,
                 size);

       info.start :=info.start +delta;
       info.__end :=start+paddi;
       info.offset:=info.offset+delta;

       size:=size-delta; //unaligned size
      end;

      vm_nt_file_obj_destroy(cow);

     end else
     begin
      info.obj   :=vm_nt_file_obj_allocate(md,max);
      info.offset:=offset;
      info.start :=start;
      info.__end :=start+paddi;

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

procedure pmap_protect(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       prot  :vm_prot_t);
var
 lock:Pointer;
label
 _default;
begin
 if (p_print_pmap) then
 begin
  Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':prot:',HexStr(prot,2));
 end;

 lock:=pmap_rlock(pmap,start,__end);

 ppmap_mark_rwx(start,__end,prot);

 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     vm_nt_map_prot_fix(@pmap^.nt_map,
                        start,
                        __end,
                        TRACK_PROT or REMAP_PROT);

     //vm_nt_map_protect(@pmap^.nt_map,
     //                  start,
     //                  __end,
     //                  (prot and VM_RW));

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

procedure _pmap_prot_fix(pmap :pmap_t;
                         start:vm_offset_t;
                         __end:vm_offset_t;
                         mode :Integer);
begin
 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 vm_nt_map_prot_fix(@pmap^.nt_map,
                    start,
                    __end,
                    mode);
end;

procedure _pmap_prot_int(pmap :pmap_t;
                         start:vm_offset_t;
                         __end:vm_offset_t;
                         prot :vm_prot_t);
begin
 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 vm_nt_map_protect(@pmap^.nt_map,
                   start,
                   __end,
                   (prot and VM_RW));
end;

procedure pmap_prot_track(pmap :pmap_t;
                          start:vm_offset_t;
                          __end:vm_offset_t;
                          prots:Byte); public;
begin
 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 ppmap_track(start,__end,prots);

 vm_nt_map_prot_fix(@pmap^.nt_map,
                    start,
                    __end,
                    TRACK_PROT or REMAP_PROT);
end;

procedure pmap_prot_restore(pmap :pmap_t;
                            start:vm_offset_t;
                            __end:vm_offset_t);
begin
 start:=start              and (not PMAPP_MASK);
 __end:=(__end+PMAPP_MASK) and (not PMAPP_MASK);

 vm_nt_map_prot_fix(@pmap^.nt_map,
                    start,
                    __end,
                    REMAP_PROT);
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
  Writeln('pmap_madv_free:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(advise,2));
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
  Assert(false,'pmap_madv_free');
 end;

 pmap_unlock(pmap,lock);
end;

procedure pmap_remove(pmap  :pmap_t;
                      obj   :vm_object_t;
                      start :vm_offset_t;
                      __end :vm_offset_t);
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

 ppmap_unmark(start,__end);

 vm_track_map_remove_memory(@pmap^.tr_map,start,__end);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin

     vm_nt_map_madvise(@pmap^.nt_map,
                       start,
                       __end,
                       MADV_FREE);

     _default:

     r:=vm_nt_map_delete(@pmap^.nt_map,
                         start,
                         __end);
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

 if (r<>0) then
 begin
  Writeln('failed vm_nt_map_delete:0x',HexStr(r,8));
  Assert(false,'pmap_remove');
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
 r:=md_unmap_ex(base,size);
 if (r<>0) then
 begin
  Writeln('failed md_unmap_ex:0x',HexStr(r,8));
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


end.


