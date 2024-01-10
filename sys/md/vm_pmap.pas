unit vm_pmap;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm,
 vmparam,
 sys_vm_object,
 vm_file,
 vnode;

const
 PAGE_MAP_COUNT   =(QWORD(VM_MAXUSER_ADDRESS) shr PAGE_SHIFT);
 PAGE_MAP_MASK    =PAGE_MAP_COUNT-1;

 PAGE_PROT_EXECUTE=VM_PROT_EXECUTE;
 PAGE_PROT_WRITE  =VM_PROT_WRITE;
 PAGE_PROT_READ   =VM_PROT_READ;

 PAGE_PROT_RW     =PAGE_PROT_READ or PAGE_PROT_WRITE;

 PAGE_PROT_LIFT   =$40;

 //PAGE_BUSY_FLAG   =DWORD($10000000);
 //PAGE_PATCH_FLAG  =DWORD($08000000);

var
 PAGE_MAP   :PInteger=nil;
 PAGE_PROT  :PBYTE   =nil;

procedure pmap_mark      (start,__end,__off:vm_offset_t;prots:Byte);
procedure pmap_unmark    (start,__end:vm_offset_t);
function  pmap_get_prot  (addr:vm_offset_t):Byte;
function  pmap_get_page  (addr:vm_offset_t):DWORD;
function  pmap_test_cross(addr:vm_offset_t;h:Integer):Boolean;

function  uplift(addr:Pointer):Pointer;

type
 p_pmap=^_pmap;
 _pmap=packed object
  //
 end;

 pmap_t=p_pmap;

function  atop(x:QWORD):DWORD; inline;
function  ptoa(x:DWORD):QWORD; inline;

function  ctob(x:QWORD):QWORD; inline;
function  btoc(x:QWORD):QWORD; inline;

procedure pmap_pinit(pmap:p_pmap);

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
                       offset:vm_ooffset_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       prot  :vm_prot_t);

procedure pmap_madv_free(pmap  :pmap_t;
                         obj   :vm_object_t;
                         offset:vm_ooffset_t;
                         start :vm_offset_t;
                         __end :vm_offset_t;
                         prot  :vm_prot_t);

procedure pmap_remove(pmap  :pmap_t;
                      obj   :vm_object_t;
                      offset:vm_ooffset_t;
                      start :vm_offset_t;
                      __end :vm_offset_t;
                      prot  :vm_prot_t);

implementation

uses
 md_map;

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

procedure pmap_pinit(pmap:p_pmap);
var
 base:Pointer;
 size:QWORD;
 prot:QWORD;
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
    Writeln('failed md_reserve(',HexStr(base),',',HexStr(base+size),'):',HexStr(r,8));
    //STATUS_COMMITMENT_LIMIT = $C000012D
    Assert(false,'pmap_init');
   end;

  end;
 end;

 PAGE_MAP:=nil;
 size:=PAGE_MAP_COUNT*SizeOf(DWORD);

 prot:=size;
 size:=size+PAGE_MAP_COUNT;

 r:=md_mmap(PAGE_MAP,size,MD_PROT_RW);

 if (r<>0) then
 begin
  Writeln('failed md_mmap(',HexStr(PAGE_MAP),',',HexStr(PAGE_MAP+size),'):',HexStr(r,8));
  Assert(false,'pmap_init');
 end;

 PAGE_PROT:=Pointer(PAGE_MAP)+prot;

 //init zero
 pmap_unmark(0,VM_MAXUSER_ADDRESS);

 //Mapping to internal memory, isolate TODO
 if Length(exclude_mem)<>0 then
 begin
  For i:=0 to High(exclude_mem) do
  begin
   pmap_mark(exclude_mem[i].start,exclude_mem[i].__end,exclude_mem[i].start,0);
  end;
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

//PAGE_MAP

function IDX_TO_OFF(x:DWORD):QWORD; inline;
begin
 Result:=QWORD(x) shl PAGE_SHIFT;
end;

function OFF_TO_IDX(x:QWORD):DWORD; inline;
begin
 Result:=QWORD(x) shr PAGE_SHIFT;
end;

function MAX_IDX(x:DWORD):DWORD; inline;
begin
 if (x>PAGE_MAP_MASK) then
  Result:=PAGE_MAP_MASK
 else
  Result:=x;
end;

procedure pmap_mark(start,__end,__off:vm_offset_t;prots:Byte);
var
 d:Integer;
begin
 start:=OFF_TO_IDX(start);
 __end:=OFF_TO_IDX(__end);
 __off:=OFF_TO_IDX(__off);
 start:=MAX_IDX(start);
 __end:=MAX_IDX(__end);
 d:=Integer(__off-start);
 prots:=prots or (ord(d<>0)*PAGE_PROT_LIFT);
 while (start<__end) do
 begin
  PAGE_MAP [start]:=d;
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
  PAGE_MAP [start]:=Integer(0-start);
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

function pmap_get_page(addr:vm_offset_t):DWORD;
begin
 addr:=OFF_TO_IDX(addr);
 addr:=addr and PAGE_MAP_MASK;
 Result:=addr+PAGE_MAP[addr];
end;

function pmap_test_cross(addr:vm_offset_t;h:Integer):Boolean;
var
 page1,page2:DWORD;
begin
 page1:=OFF_TO_IDX(addr  ) and PAGE_MAP_MASK;
 page2:=OFF_TO_IDX(addr+h) and PAGE_MAP_MASK;
 if (page1=page2) then
 begin
  Result:=False;
 end else
 begin
  page1:=PAGE_MAP[page1];
  page2:=PAGE_MAP[page2];
  Result:=(page1<>page2);
 end;
end;

//rax,rdi,rsi
function uplift(addr:Pointer):Pointer; assembler; nostackframe;
label
 _exit;
asm
 //orig addr (rsi)
 mov %rdi,%rsi
 //high addr (rdi)
 shr PAGE_SHIFT,%rdi
 //test
 cmp PAGE_MAP_COUNT,%rdi
 ja _exit
 //uplift (rdi)
 mov PAGE_MAP(%rip),%rax
 mov (%rax,%rdi,4) ,%edi
 //high addr (rdi)
 shl PAGE_SHIFT,%rdi
 //combine (rdi+rsi)
 lea (%rsi,%rdi),%rdi
 //test
 cmp PAGE_SIZE,%rdi
 jna _exit
 //result
 mov %rdi,%rax
 ret
 _exit:
  xor %eax,%eax
end;

const
 VM_RWX=VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;

 wprots:array[0..7] of Byte=(
  MD_PROT_NONE,//___
  MD_PROT_R   ,//__R
  MD_PROT_W   ,//_W_
  MD_PROT_RW  ,//_WR
  MD_PROT_R   ,//X__
  MD_PROT_R   ,//X_R
  MD_PROT_RW  ,//XW_
  MD_PROT_RW   //XWR
 );

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

function vm_file_map_fixed(map   :p_vm_file_map;
                           offset:vm_ooffset_t;
                           start :vm_offset_t;
                           __end :vm_offset_t;
                           base  :Pointer;
                           size  :QWORD):Integer;
var
 obj:p_vm_file_obj;
begin
 vm_file_map_delete(map,start,__end);

 obj:=vm_file_obj_allocate(base,size,@md_file_unmap);

 Result:=vm_file_map_insert(map,obj,offset,start,__end);
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
 base:Pointer;
 size:QWORD;
 delta:QWORD;
 r:Integer;
begin
 Writeln('pmap_enter_object:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     base:=Pointer(trunc_page(start));
     size:=trunc_page(__end-start);

     r:=md_enter(base,size,wprots[prot and VM_RWX]);
    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     base:=obj^.un_pager.map_base+trunc_page(offset);
     size:=trunc_page(__end-start);

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin
      Writeln('pmap_enter_gpuobj:',HexStr(QWORD(base),11),':',HexStr(QWORD(base)+size,11),':',HexStr(prot,2));

      r:=md_enter(base,size,MD_PROT_RWX);
     end;
    end;
   OBJT_VNODE:
    begin
     VM_OBJECT_LOCK(obj);

       fd:=get_vnode_handle(obj);

       if (fd<>0) then
       begin

        delta :=offset and (MD_ALLOC_GRANULARITY-1);
        offset:=offset and (not (MD_ALLOC_GRANULARITY-1));

        size:=delta+offset+(__end-start);

        if (size>obj^.un_pager.vnp.vnp_size) then
        begin
         size:=obj^.un_pager.vnp.vnp_size;
        end;
        size:=size-offset;

        base:=nil;
        r:=md_file_mmap(fd,base,offset,size,wprots[prot and VM_RWX]);

        if (r=0) then
        begin
         r:=vm_file_map_fixed(@obj^.un_pager.vnp.file_map,
                              delta,
                              start,
                              __end,
                              base,
                              size);
        end;

       end;

     VM_OBJECT_UNLOCK(obj);
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
  Writeln('failed md_enter:',HexStr(r,8));
  Assert(false,'pmap_enter_object');
 end;

 pmap_mark(start,__end,QWORD(base),prot);
end;

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
  Writeln('failed md_protect:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;

 //alloc new
 r:=md_enter(Pointer(ofs_new),size,MD_PROT_RW);

 if (r<>0) then
 begin
  Writeln('failed md_enter:',HexStr(r,8));
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
   Writeln('failed md_protect:',HexStr(r,8));
   Assert(false,'pmap_move');
  end;
 end;

 pmap_mark(start,start+size,ofs_new,new_prot);

 //free old
 r:=md_remove(Pointer(ofs_old),size);

 if (r<>0) then
 begin
  Writeln('failed md_remove:',HexStr(r,8));
  Assert(false,'pmap_move');
 end;
end;

procedure pmap_protect(pmap  :pmap_t;
                       obj   :vm_object_t;
                       offset:vm_ooffset_t;
                       start :vm_offset_t;
                       __end :vm_offset_t;
                       prot  :vm_prot_t);
label
 _default;
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_protect:',HexStr(start,11),':',HexStr(__end,11),':prot:',HexStr(prot,2));

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     base:=Pointer(trunc_page(start));
     size:=trunc_page(__end-start);

     r:=md_protect(base,size,wprots[prot and VM_RWX]);
    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     base:=obj^.un_pager.map_base+trunc_page(offset);
     size:=trunc_page(__end-start);

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin
      Writeln('pmap_protect_gpuobj:',HexStr(QWORD(base),11),':',HexStr(QWORD(base)+size,11),':',HexStr(prot,2));

      r:=md_protect(base,size,MD_PROT_RWX);
     end;
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
  Writeln('failed md_protect:',HexStr(r,8));
  Assert(false,'pmap_protect');
 end;

 pmap_mark(start,__end,QWORD(base),prot);
end;

procedure pmap_madv_free(pmap  :pmap_t;
                         obj   :vm_object_t;
                         offset:vm_ooffset_t;
                         start :vm_offset_t;
                         __end :vm_offset_t;
                         prot  :vm_prot_t);
label
 _default;
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_madv_free:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     base:=Pointer(trunc_page(start));
     size:=trunc_page(__end-start);

     r:=md_reset(base,size,wprots[prot and VM_RWX]);
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
  else
    begin
     Writeln('TODO:',vm_object_type(obj));
     Assert(False);
     Exit;
    end;
 end;

 if (r<>0) then
 begin
  Writeln('failed md_reset:',HexStr(r,8));
  Assert(false,'pmap_madv_free');
 end;
end;

procedure pmap_remove(pmap  :pmap_t;
                      obj   :vm_object_t;
                      offset:vm_ooffset_t;
                      start :vm_offset_t;
                      __end :vm_offset_t;
                      prot  :vm_prot_t);
label
 _default;
var
 base:Pointer;
 size:QWORD;
 r:Integer;
begin
 Writeln('pmap_remove:',HexStr(start,11),':',HexStr(__end,11),':',HexStr(prot,2));

 pmap_unmark(start,__end);

 r:=0;
 case vm_object_type(obj) of
  OBJT_SELF  , // same?

  OBJT_DEFAULT:
    begin
     _default:

     base:=Pointer(trunc_page(start));
     size:=trunc_page(__end-start);

     r:=md_remove(base,size);
    end;
  OBJT_DEVICE:
    begin
     if (obj^.un_pager.map_base=nil) then
     begin
      goto _default;
     end;

     base:=obj^.un_pager.map_base+trunc_page(offset);
     size:=trunc_page(__end-start);

     if ((obj^.flags and OBJ_DMEM_EXT)<>0) then
     begin
      Writeln('pmap_remove_gpuobj:',HexStr(QWORD(base),11),':',HexStr(QWORD(base)+size,11),':',HexStr(prot,2));

      r:=md_remove(base,size);
     end;
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
  Writeln('failed md_remove:',HexStr(r,8));
  Assert(false,'pmap_remove');
 end;
end;


end.

