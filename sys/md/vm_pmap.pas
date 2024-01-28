unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam,
 sys_vm_object,
 vnode,
 vuio,
 md_map,
 vm_nt_map;

const
 PMAPP_SHIFT=12;
 PMAPP_SIZE =1 shl PMAPP_SHIFT;
 PMAPP_MASK =PMAPP_SIZE-1;

 PAGE_MAP_COUNT   =(QWORD(VM_MAXUSER_ADDRESS) shr PMAPP_SHIFT);
 PAGE_MAP_MASK    =PAGE_MAP_COUNT-1;

 PAGE_PROT_READ   =VM_PROT_READ;
 PAGE_PROT_WRITE  =VM_PROT_WRITE;
 PAGE_PROT_EXECUTE=VM_PROT_EXECUTE;

 PAGE_PROT_RW     =PAGE_PROT_READ or PAGE_PROT_WRITE;

 PAGE_PROT_LIFT   =$40;

 //PAGE_BUSY_FLAG   =DWORD($10000000);
 //PAGE_PATCH_FLAG  =DWORD($08000000);

 PMAPP_1GB_SHIFT =30;
 PMAPP_1GB_SIZE  =QWORD(QWORD(1) shl PMAPP_1GB_SHIFT);
 PMAPP_1GB_MASK  =PMAPP_1GB_SIZE-1;

 PMAPP_1GB_DMEM_BLOCKS=QWORD(VM_MAX_GPU_ADDRESS-VM_MIN_GPU_ADDRESS) shr PMAPP_1GB_SHIFT;

var
 PAGE_PROT:PBYTE=nil;

 DMEM_FD:array[0..PMAPP_1GB_DMEM_BLOCKS-1] of vm_nt_file_obj;

 DEV_INFO:record
  DEV_FD  :vm_nt_file_obj;
  DEV_SIZE:QWORD;
  DEV_POS :QWORD;
  DEV_PTR :Pointer;
 end;

procedure pmap_mark      (start,__end:vm_offset_t;prots:Byte);
procedure pmap_unmark    (start,__end:vm_offset_t);
function  pmap_get_prot  (addr:vm_offset_t):Byte;

function  uplift(addr:Pointer):Pointer;
procedure iov_uplift(iov:p_iovec);

type
 p_pmap=^_pmap;
 _pmap=packed object
  nt_map:_vm_nt_map;
 end;

 pmap_t=p_pmap;

function  atop(x:QWORD):DWORD; inline;
function  ptoa(x:DWORD):QWORD; inline;

function  ctob(x:QWORD):QWORD; inline;
function  btoc(x:QWORD):QWORD; inline;

function  dev_mem_alloc(pages:Integer):Pointer;

procedure pmap_reserve(wr:Boolean);

procedure pmap_pinit(pmap:p_pmap;vmap:Pointer);

procedure pmap_align_superpage(obj   :vm_object_t;
                               offset:vm_ooffset_t;
                               addr  :p_vm_offset_t;
                               size  :vm_size_t);

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

procedure pmap_madvise(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       advise:Integer);

procedure pmap_remove(pmap  :pmap_t;
                      obj   :vm_object_t;
                      start :vm_offset_t;
                      __end :vm_offset_t);

implementation

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

procedure dev_mem_init(pages:Integer);
var
 r:Integer;
begin
 DEV_INFO.DEV_SIZE:=pages*PAGE_SIZE;

 R:=md_memfd_create(DEV_INFO.DEV_FD.hfile,DEV_INFO.DEV_SIZE);
 if (r<>0) then
 begin
  Writeln('failed md_memfd_create(',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init');
 end;

 r:=md_reserve(DEV_INFO.DEV_PTR,DEV_INFO.DEV_SIZE);
 if (r<>0) then
 begin
  Writeln('failed md_reserve(',HexStr(DEV_INFO.DEV_SIZE,11),'):0x',HexStr(r,8));
  Assert(false,'dev_mem_init');
 end;

 r:=md_file_mmap_ex(DEV_INFO.DEV_FD.hfile,DEV_INFO.DEV_PTR,0,DEV_INFO.DEV_SIZE,MD_PROT_RW);
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

procedure pmap_reserve(wr:Boolean);
var
 base:Pointer;
 size:QWORD;
 i,r:Integer;
begin
 if Length(pmap_mem)<>0 then
 begin
  For i:=0 to High(pmap_mem) do
  begin
   base:=Pointer(pmap_mem[i].start);
   size:=pmap_mem[i].__end-pmap_mem[i].start;

   r:=md_reserve(base,size);

   if (r<>0) then
   begin
    if wr then
    begin
     Writeln('failed md_reserve(',HexStr(base),',',HexStr(base+size),'):0x',HexStr(r,8));
    end;
    //STATUS_COMMITMENT_LIMIT = $C000012D
    Assert(false,'pmap_init');
   end;

   pmap_mem[i].start:=QWORD(base);

   if wr then
   begin
    Writeln('md_reserve(',HexStr(base),',',HexStr(base+size),'):0x',HexStr(r,8));
   end;
  end;
 end;
end;

procedure pmap_pinit(pmap:p_pmap;vmap:Pointer);
var
 i,r:Integer;
begin

 pmap_reserve(True);

 dev_mem_init(4);

 PAGE_PROT:=nil;

 r:=md_mmap(PAGE_PROT,PAGE_MAP_COUNT,MD_PROT_RW);

 if (r<>0) then
 begin
  Writeln('failed md_mmap(',HexStr(PAGE_MAP_COUNT,11),'):0x',HexStr(r,8));
  Assert(false,'pmap_init');
 end;

 vm_nt_map_init(@pmap^.nt_map,VM_MINUSER_ADDRESS,VM_MAXUSER_ADDRESS,vmap);

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
                    MD_PROT_NONE);
  end;
 end;

end;

type
 t_fd_info=record
  start :QWORD;
  __end :QWORD;
  obj   :p_vm_nt_file_obj;
  offset:QWORD;
 end;

procedure get_private_fd(var info:t_fd_info);
var
 hfile:THandle;
 i:QWORD;
 s:QWORD;
 e:QWORD;
 r:DWORD;
begin
 s:=info.start;
 e:=info.__end;

 i:=(e-s);

 if (i>PMAPP_1GB_SIZE) then
 begin
  i:=PMAPP_1GB_SIZE;
  info.__end:=(s+i);
 end;

 hfile:=0;
 r:=md_memfd_create(hfile,i);

 if (r<>0) then
 begin
  Writeln('failed md_memfd_create(',HexStr(i,11),'):0x',HexStr(r,8));
  Assert(false,'get_private_fd');
 end;

 info.obj:=vm_nt_file_obj_allocate(hfile);

 with info.obj^ do
 begin
  flags:=flags and (not NT_UNION_OBJ);
 end;
end;

procedure get_dmem_fd(var info:t_fd_info);
var
 o:QWORD;
 e:QWORD;
 i:DWORD;
 r:DWORD;
begin
 o:=info.offset;

 i:=o shr PMAPP_1GB_SHIFT;

 if (DMEM_FD[i].hfile=0) then
 begin
  R:=md_memfd_create(DMEM_FD[i].hfile,PMAPP_1GB_SIZE);

  if (r<>0) then
  begin
   Writeln('failed md_memfd_create(',HexStr(PMAPP_1GB_SIZE,11),'):0x',HexStr(r,8));
   Assert(false,'get_dmem_fd');
  end;
 end;

 info.obj:=@DMEM_FD[i];

 vm_nt_file_obj_reference(info.obj);

 e:=o+(info.__end-info.start);

 if (e>PMAPP_1GB_SIZE) then
 begin
  e:=PMAPP_1GB_SIZE;
  e:=(e-o)+info.start;
  info.__end:=e;
 end;
end;

procedure get_dev_fd(ptr:Pointer;var info:t_fd_info);
var
 o:QWORD;
begin
 o:=QWORD(ptr)-QWORD(DEV_INFO.DEV_PTR);

 if (o>DEV_INFO.DEV_SIZE) then
 begin
  Assert(false,'get_dev_fd wrong');
  Exit();
 end;

 info.offset:=info.offset+o;
 info.obj:=@DEV_INFO.DEV_FD;

 vm_nt_file_obj_reference(info.obj);
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

//PAGE_MAP

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PMAPP_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PMAPP_SHIFT;
end;

function MAX_IDX(x:DWORD):DWORD; inline;
begin
 if (x>PAGE_MAP_MASK) then
  Result:=PAGE_MAP_MASK
 else
  Result:=x;
end;

procedure pmap_mark(start,__end:vm_offset_t;prots:Byte);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  PAGE_PROT[start]:=prots;
  Inc(start);
 end;
 WriteBarrier;
end;

procedure pmap_unmark(start,__end:vm_offset_t);
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 while (start<__end) do
 begin
  PAGE_PROT[start]:=0;
  Inc(start);
 end;
 WriteBarrier;
end;

function pmap_get_prot(addr:vm_offset_t):Byte;
begin
 addr:=OFF_TO_IDX(addr);
 addr:=addr and PAGE_MAP_MASK;
 Result:=PAGE_PROT[addr];
end;

//rax,rdi,rsi
function uplift(addr:Pointer):Pointer; assembler; nostackframe;
asm
 mov %rdi,%rax
 ret
end;

procedure iov_uplift(iov:p_iovec);
begin
 //
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
procedure pmap_enter_object(pmap   :pmap_t;
                            obj    :vm_object_t;
                            offset :vm_ooffset_t;
                            start  :vm_offset_t;
                            __end  :vm_offset_t;
                            prot   :vm_prot_t);
label
 _default;
var
 fd:THandle;
 md:THandle;

 size:QWORD;
 delta:QWORD;
 paddi:QWORD;

 info:t_fd_info;

 r:Integer;
begin
 Writeln('pmap_enter_object:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

      size:=__end-start;

      info.start:=start;
      info.__end:=__end;
      info.offset:=0;

      while (size<>0) do
      begin
       get_private_fd(info);

       delta:=(info.__end-info.start);

       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.offset,
                           info.start,
                           info.__end,
                           delta,
                           wprots[prot and VM_RWX]);

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       info.start :=info.start+delta;
       info.offset:=0;

       size:=size-delta;
      end;

    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin
      Writeln('pmap_enter_gpuobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

      size:=__end-start;

      info.start:=start;
      info.__end:=__end;
      info.offset:=offset;

      while (size<>0) do
      begin
       get_dmem_fd(info);

       delta:=(info.__end-info.start);

       r:=vm_nt_map_insert(@pmap^.nt_map,
                           info.obj,
                           info.offset,
                           info.start,
                           info.__end,
                           delta,
                           wprots[prot and VM_RWX]);

       if (r<>0) then
       begin
        Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
        Assert(false,'pmap_enter_object');
       end;

       info.start :=info.start +delta;
       info.offset:=info.offset+delta;

       size:=size-delta;
      end;

     end else
     begin
      Writeln('pmap_enter_devobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

      info.start:=start;
      info.__end:=__end;
      info.offset:=offset;

      get_dev_fd(obj^.un_pager.map_base,info);

      delta:=(info.__end-info.start);

      r:=vm_nt_map_insert(@pmap^.nt_map,
                          info.obj,
                          info.offset,
                          info.start,
                          info.__end,
                          delta,
                          wprots[prot and VM_RWX]);

      if (r<>0) then
      begin
       Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
       Assert(false,'pmap_enter_object');
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

        r:=md_memfd_open(md,fd);
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

     info.obj   :=vm_nt_file_obj_allocate(md);
     info.offset:=offset;
     info.start :=start;
     info.__end :=start+paddi;

     r:=vm_nt_map_insert(@pmap^.nt_map,
                         info.obj,
                         info.offset,
                         info.start,
                         info.__end,
                         size,
                         wprots[prot and VM_RWX]);

     if (r<>0) then
     begin
      Writeln('failed vm_nt_map_insert:0x',HexStr(r,8));
      Assert(false,'pmap_enter_object');
     end;

     pmap_mark(info.start,info.__end,prot and VM_RWX);

     //aligned size
     size:=paddi;

     //padding pages
     paddi:=PAGE_SIZE-((delta-size) and PAGE_MASK);

     if (paddi<>0) then
     begin
      offset:=0;
      start:=start+size;
      goto _default;
     end;

     Exit;
    end;
  else
    begin
     Writeln('TODO:',vm_object_type(obj));
     Assert(False);
     Exit;
    end;
 end;

 pmap_mark(start,__end,prot and VM_RWX);
end;

{
procedure pmap_move(pmap    :pmap_t;
                    start   :vm_offset_t;
                    ofs_old :vm_offset_t;
                    ofs_new :vm_offset_t;
                    size    :vm_offset_t;
                    new_prot:vm_prot_t);
var
 r:Integer;
begin
 //pmap_mark_flags(start,start+size,PAGE_BUSY_FLAG);

 //set old to readonly
 r:=md_protect(Pointer(ofs_old),size,MD_PROT_R);

 if (r<>0) then
 begin
  Writeln('failed md_protect:0x',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //alloc new
 r:=md_enter(Pointer(ofs_new),size,MD_PROT_RW);

 if (r<>0) then
 begin
  Writeln('failed md_enter:0x',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //move data
 Move(Pointer(ofs_old)^,Pointer(ofs_new)^,size);

 //prot new
 if (MD_PROT_RW<>wprots[new_prot and VM_RWX]) then
 begin
  r:=md_protect(Pointer(ofs_new),size,wprots[new_prot and VM_RWX]);

  if (r<>0) then
  begin
   Writeln('failed md_protect:0x',HexStr(r,8));
   Assert(false,'pmap_move');
  end;
 end;

 pmap_mark(start,start+size,ofs_new,new_prot);

 //free old
 r:=md_remove(Pointer(ofs_old),size);

 if (r<>0) then
 begin
  Writeln('failed md_remove:0x',HexStr(r,8));
  Assert(false,'pmap_move');
 end;
end;
}

procedure pmap_protect(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       prot  :vm_prot_t);
label
 _default;
begin
 Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':prot:',HexStr(prot,2));

 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     vm_nt_map_protect(@pmap^.nt_map,
                       start,
                       __end,
                       wprots[prot and VM_RWX]);

    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin
      Writeln('pmap_protect_gpuobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
     end else
     begin
      Writeln('pmap_protect_devobj:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));
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
     Exit;
    end;
 end;

 pmap_mark(start,__end,prot and VM_RWX);
end;

procedure pmap_madvise(pmap  :pmap_t;
                       obj   :vm_object_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       advise:Integer);
label
 _default;
var
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_madv_free:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(advise,2));

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     size:=(__end-start);

     r:=md_dontneed(Pointer(start),size);
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
     Exit;
    end;
 end;

 if (r<>0) then
 begin
  Writeln('failed md_reset:0x',HexStr(r,8));
  Assert(false,'pmap_madv_free');
 end;
end;

procedure pmap_remove(pmap  :pmap_t;
                      obj   :vm_object_t;
                      start :vm_offset_t;
                      __end :vm_offset_t);
label
 _default;
var
 r:Integer;
begin
 Writeln('pmap_remove:',HexStr(start,11),':',HexStr(__end,11));

 pmap_unmark(start,__end);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
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

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin
      Writeln('pmap_remove_gpuobj:',HexStr(start,11),':',HexStr(__end,11));
     end else
     begin
      Writeln('pmap_remove_devobj:',HexStr(start,11),':',HexStr(__end,11));
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
     Exit;
    end;
 end;

 if (r<>0) then
 begin
  Writeln('failed vm_nt_map_delete:0x',HexStr(r,8));
  Assert(false,'pmap_remove');
 end;
end;


end.

