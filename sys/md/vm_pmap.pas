unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 subr_backtrace,
 mqueue,
 vm,
 vmparam,
 sys_vm_object,
 vnode,
 vuio,
 kern_thr,
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

 PMAPP_BLK_DMEM_BLOCKS=QWORD(VM_DMEM_SIZE) shr PMAPP_BLK_SHIFT;

var
 DMEM_FD:array[0..PMAPP_BLK_DMEM_BLOCKS-1] of vm_nt_file_obj;

 DEV_INFO:record
  DEV_FD  :vm_nt_file_obj;
  DEV_SIZE:QWORD;
  DEV_POS :QWORD;
  DEV_PTR :Pointer;
 end;

type
 P_PRIV_FD=^T_PRIV_FD;
 T_PRIV_FD=record
  elist:TAILQ_ENTRY;
  efree:TAILQ_ENTRY;
  obj  :vm_nt_file_obj;
  size :DWORD;
  pos  :DWORD;
 end;

var
 PRIV_FD_LIST:TAILQ_HEAD=(tqh_first:nil;tqh_last:@PRIV_FD_LIST.tqh_first);
 PRIV_FD_FREE:TAILQ_HEAD=(tqh_first:nil;tqh_last:@PRIV_FD_FREE.tqh_first);

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

procedure pmap_gpu_get_bound(pmap:pmap_t;
                             var start:vm_offset_t;
                             var __end:vm_offset_t);

implementation

uses
 sysutils,
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
  Assert(false,'dev_mem_init');
 end;

 DEV_INFO.DEV_FD.maxp:=VM_RW;

 DEV_INFO.DEV_PTR:=Pointer(VM_MIN_DEV_ADDRESS);

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
  i:=0;
  while (i<=High(pmap_mem)) do
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
     i:=i+2;
     Continue;
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
   i:=i+1;
  end;
 end;
end;

procedure pmap_pinit(pmap:p_pmap;vm_map:Pointer);
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

 //rangelock_init(@pmap^.rmlock);
 //mtx_init(pmap^.rm_mtx,'pmap');

 pmap^.vm_map:=vm_map;

 vm_nt_map_init(@pmap^.nt_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS);
 vm_nt_map_init(@pmap^.gp_map,VM_MIN_GPU_ADDRESS,VM_MAX_GPU_ADDRESS);

  //exclude
 if Length(pmap_mem_guest)>1 then
 begin
  For i:=0 to High(pmap_mem_guest)-1 do
  begin
   vm_nt_map_insert(@pmap^.nt_map,
                    nil,0,
                    pmap_mem_guest[  i].__end,
                    pmap_mem_guest[i+1].start,
                    pmap_mem_guest[i+1].start-pmap_mem_guest[i].__end,
                    0);
  end;
 end;

 vm_track_map_init(@pmap^.tr_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS,vm_map);

 pmap^.tr_map.pmap:=pmap;
end;

type
 t_fd_info=record
  start :QWORD;
  __end :QWORD;
  obj   :p_vm_nt_file_obj;
  offset:QWORD;
  olocal:QWORD;
 end;

function get_priv_block_count:Integer;
var
 node:P_PRIV_FD;
begin
 Result:=0;
 node:=TAILQ_FIRST(@PRIV_FD_LIST);
 While (node<>nil) do
 begin
  Inc(Result);
  node:=TAILQ_NEXT(node,@node^.elist);
 end;
end;

function get_priv_free_count:Integer;
var
 node:P_PRIV_FD;
begin
 Result:=0;
 node:=TAILQ_FIRST(@PRIV_FD_FREE);
 While (node<>nil) do
 begin
  Inc(Result);
  node:=TAILQ_NEXT(node,@node^.efree);
 end;
end;

procedure on_free_priv(obj:p_vm_nt_file_obj);
var
 node:P_PRIV_FD;
begin
 node:=POINTER(PTRUINT(obj)-PTRUINT(@P_PRIV_FD(nil)^.obj));

 if (node^.efree.tqe_next<>nil) and
    (node^.efree.tqe_prev<>nil) then
 begin
  TAILQ_REMOVE(@PRIV_FD_FREE,node,@node^.efree);
 end;

 TAILQ_REMOVE(@PRIV_FD_LIST,node,@node^.elist);

 FreeMem(node);
end;

function find_free_priv(size:DWORD):P_PRIV_FD;
var
 node:P_PRIV_FD;
begin
 Result:=nil;
 node:=TAILQ_FIRST(@PRIV_FD_FREE);
 While (node<>nil) do
 begin
  if ((node^.size-node^.pos)>=size) then
  begin
   Exit(node);
  end;
  //
  node:=TAILQ_NEXT(node,@node^.efree);
 end;
end;

procedure get_priv_fd(var info:t_fd_info);
const
 MAX_PRIV_SIZE=256*1024*1024;
var
 node  :P_PRIV_FD;
 size  :QWORD;
 offset:QWORD;
 R     :DWORD;
begin
 //Find empty space or create new one (do not reuse old ones) no more than 256MB

 size:=(info.__end-info.start);

 if (size<MAX_PRIV_SIZE) then
 begin
  node:=find_free_priv(size);
 end else
 begin
  node:=nil;
 end;

 if (node<>nil) then
 begin
  //linear alloc
  offset:=node^.pos;
  node^.pos:=node^.pos+size;

  if (node^.pos=node^.size) then
  begin
   //delete with free list
   TAILQ_REMOVE(@PRIV_FD_FREE,node,@node^.efree);
  end;

 end else
 begin
  //trunc size
  if (size>MAX_PRIV_SIZE) then size:=MAX_PRIV_SIZE;
  offset:=0;

  //new block
  node:=AllocMem(SizeOf(T_PRIV_FD));
  node^.obj.free :=@on_free_priv;
  node^.obj.flags:=NT_FILE_FREE;
  node^.obj.maxp :=VM_RW;
  node^.size:=MAX_PRIV_SIZE;
  node^.pos :=size; //prealloc

  //insert to list
  TAILQ_INSERT_TAIL(@PRIV_FD_LIST,node,@node^.elist);

  if (node^.pos<>node^.size) then
  begin
   //insert with free list
   TAILQ_INSERT_TAIL(@PRIV_FD_FREE,node,@node^.efree);
  end;

  R:=md_memfd_create(node^.obj.hfile,MAX_PRIV_SIZE,VM_RW);

  if (R<>0) then
  begin
   Writeln('failed md_memfd_create(',HexStr(MAX_PRIV_SIZE,11),'):0x',HexStr(r,8));
   Writeln(' priv_block_count=',get_priv_block_count,':',get_priv_free_count);

   print_backtrace_td(stderr);

   Assert(false,'get_priv_fd');
  end;
 end;

 info.obj   :=@node^.obj;
 info.olocal:=offset;          //block local offset
 info.__end :=info.start+size; //apply size

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

function  vm_map_lock_range  (map:Pointer;start,__end:off_t;mode:Integer):Pointer; external;
procedure vm_map_unlock_range(map:Pointer;cookie:Pointer); external;

function pmap_wlock(pmap :pmap_t;
                    start:vm_offset_t;
                    __end:vm_offset_t):Pointer; inline;
begin
 //Writeln('pmap_wlock:',HexStr(start,10),'..',HexStr(__end,10));

 Result:=vm_map_lock_range(pmap^.vm_map,start,__end,RL_LOCK_WRITE);
end;

function pmap_rlock(pmap :pmap_t;
                    start:vm_offset_t;
                    __end:vm_offset_t):Pointer; inline;
begin
 //Writeln('pmap_rlock:',HexStr(start,10),'..',HexStr(__end,10));

 Result:=vm_map_lock_range(pmap^.vm_map,start,__end,RL_LOCK_READ);
end;

procedure pmap_unlock(pmap:pmap_t;cookie:Pointer); inline;
begin
 //Writeln('pmap_unlock:',HexStr(p_rl_q_entry(cookie)^.rl_q_start,10),'..',HexStr(p_rl_q_entry(cookie)^.rl_q_end,10));

 vm_map_unlock_range(pmap^.vm_map,cookie);
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

 //fixup writeonly
 if ((prot and VM_PROT_RWX)=VM_PROT_WRITE) then
 begin
  prot:=prot or VM_PROT_READ;
 end;

 //fixup gpu writeonly
 if ((prot and VM_PROT_GPU_ALL)=VM_PROT_GPU_WRITE) then
 begin
  prot:=prot or VM_PROT_GPU_READ;
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
        r:=vm_nt_map_insert(@pmap^.gp_map,
                            info.obj,
                            info.olocal, //block local offset
                            info.start+VM_MIN_GPU_ADDRESS,
                            info.__end+VM_MIN_GPU_ADDRESS,
                            delta,
                            ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));

        if (r<>0) then
        begin
         Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
         Assert(false,'pmap_enter_object');
        end;
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
        r:=vm_nt_map_insert(@pmap^.gp_map,
                            info.obj,
                            info.olocal, //block local offset
                            info.start+VM_MIN_GPU_ADDRESS,
                            info.__end+VM_MIN_GPU_ADDRESS,
                            delta,
                            ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));

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
       r:=vm_nt_map_insert(@pmap^.gp_map,
                           info.obj,
                           info.olocal, //block local offset
                           info.start+VM_MIN_GPU_ADDRESS,
                           info.__end+VM_MIN_GPU_ADDRESS,
                           delta,
                           ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));

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
        r:=vm_nt_map_insert(@pmap^.gp_map,
                            info.obj,
                            info.olocal, //block local offset
                            info.start+VM_MIN_GPU_ADDRESS,
                            info.__end+VM_MIN_GPU_ADDRESS,
                            delta,
                            ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));

        if (r<>0) then
        begin
         Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
         Assert(false,'pmap_enter_object');
        end;
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
                 info.olocal, //block local offset
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
       r:=vm_nt_map_insert(@pmap^.gp_map,
                           info.obj,
                           info.offset, //offset in file
                           info.start+VM_MIN_GPU_ADDRESS,
                           info.__end+VM_MIN_GPU_ADDRESS,
                           size,
                           ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;
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

 //fixup gpu writeonly
 if ((prot and VM_PROT_GPU_ALL)=VM_PROT_GPU_WRITE) then
 begin
  prot:=prot or VM_PROT_GPU_READ;
 end;

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
   r:=vm_nt_map_insert(@pmap^.gp_map,
                       p____obj,
                       p_offset, //block local offset
                       start   +VM_MIN_GPU_ADDRESS,
                       p____end+VM_MIN_GPU_ADDRESS,
                       (p____end-start),
                       ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));

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

 //fixup writeonly
 if ((prot and VM_PROT_RWX)=VM_PROT_WRITE) then
 begin
  prot:=prot or VM_PROT_READ;
 end;

 //fixup gpu writeonly
 if ((prot and VM_PROT_GPU_ALL)=VM_PROT_GPU_WRITE) then
 begin
  prot:=prot or VM_PROT_GPU_READ;
 end;

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
 //fixup gpu writeonly
 if ((prot and VM_PROT_GPU_ALL)=VM_PROT_GPU_WRITE) then
 begin
  prot:=prot or VM_PROT_GPU_READ;
 end;

 lock:=pmap_rlock(pmap,start,__end);

 //map to GPU
 vm_nt_map_protect(@pmap^.gp_map,
                   start+VM_MIN_GPU_ADDRESS,
                   __end+VM_MIN_GPU_ADDRESS,
                   ((prot shr VM_PROT_GPU_SHIFT) and VM_RW));


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

 ppmap_unmark(start,__end);

 vm_track_map_remove_memory(@pmap^.tr_map,start,__end);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin

     //Disable page caching in swap file
     vm_nt_map_madvise(@pmap^.nt_map,
                       start,
                       __end,
                       MADV_FREE);

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


