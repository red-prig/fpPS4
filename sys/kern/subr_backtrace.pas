unit subr_backtrace;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_thr;

procedure print_frame(var f:text;frame:Pointer);

procedure print_backtrace(var f:text;rip,rbp:Pointer;skipframes:sizeint);
procedure print_backtrace_td(var f:text);
procedure print_error_td(const str:shortstring);

implementation

uses
 vmparam,
 md_systm,
 sys_bootparam,
 kern_named_id,
 subr_dynlib,
 elf_nid_utils,
 ps4libdoc;

function IS_TRAP_FUNC(rip:qword):Boolean; external;
function IS_JIT_FUNC (rip:qword):Boolean; external;

function fuptr(var base:Pointer):Pointer;
begin
 Result:=nil;
 md_copyin(@base,@Result,SizeOf(Pointer),nil);
end;

function fuptr(var base:QWORD):QWORD;
begin
 Result:=0;
 md_copyin(@base,@Result,SizeOf(QWORD),nil);
end;

function CaptureBacktrace(td:p_kthread;rbp:PPointer;skipframes,count:sizeint;frames:PCodePointer):sizeint;
label
 _next;
var
 adr:Pointer;

 procedure push; inline;
 begin
  if (skipframes<>0) then
  begin
   Dec(skipframes);
  end else
  if (count<>0) then
  begin
   frames[0]:=adr;
   Dec(count);
   Inc(frames);
   Inc(Result);
  end;
 end;

begin
 Result:=0;

 while (rbp<>nil) and
       (rbp<>Pointer(QWORD(-1))) and
       (count<>0) do
 begin
  adr:=fuptr(rbp[1]);
  rbp:=fuptr(rbp[0]);

  _next:

  if (adr<>nil) then
  begin
   push;
  end else
  begin
   Break;
  end;

  if (td<>nil) and
     (IS_TRAP_FUNC(QWORD(adr)) or IS_JIT_FUNC(QWORD(adr))) then
  begin
   adr:=Pointer(td^.td_frame.tf_rip);
   rbp:=Pointer(td^.td_frame.tf_rbp);
   goto _next;
  end;

 end;
end;

type
 TLQRec=record
  Base   :Pointer;
  Addr   :Pointer;
  LastAdr:Pointer;
  LastNid:QWORD;
 end;

Function trav_proc(h_entry:p_sym_hash_entry;var r:TLQRec):Integer;
var
 adr:Pointer;
begin
 Result:=0;
 adr:=r.Base+fuptr(h_entry^.sym.st_value);
 if (adr<=r.Addr) then
 if (adr>r.LastAdr) then
 begin
  r.LastAdr:=adr;
  r.LastNid:=fuptr(h_entry^.nid);
  Result:=1;
 end;
end;

Function find_proc_lib_entry(lib_entry:p_Lib_Entry;var r:TLQRec):Integer;
var
 h_entry:p_sym_hash_entry;
begin
 Result:=0;
 h_entry:=fuptr(lib_entry^.syms.tqh_first);
 while (h_entry<>nil) do
 begin
  Result:=Result+trav_proc(h_entry,r);
  h_entry:=fuptr(h_entry^.link.tqe_next);
 end;
end;

Function find_proc_obj(obj:p_lib_info;var r:TLQRec):Integer;
var
 lib_entry:p_Lib_Entry;
begin
 Result:=0;
 lib_entry:=fuptr(obj^.lib_table.tqh_first);
 while (lib_entry<>nil) do
 begin
  Result:=Result+find_proc_lib_entry(lib_entry,r);
  lib_entry:=fuptr(lib_entry^.link.tqe_next);
 end;
end;

type
 TDynlibLineInfo=record
  func     :shortstring;
  source   :t_id_name;
  base_addr:ptruint;
  func_addr:ptruint;
 end;

function GetDynlibLineInfo(addr:ptruint;var info:TDynlibLineInfo):boolean;
var
 obj:p_lib_info;
 r:TLQRec;
 adr:QWORD;
 lock:Boolean;
begin
 Result:=False;

 lock:=False;
 if not dynlibs_locked then
 begin
  dynlibs_lock;
  lock:=True;
 end;

 obj:=fuptr(dynlibs_info.obj_list.tqh_first);
 while (obj<>nil) do
 begin
  if (Pointer(addr)>=obj^.map_base) and
     (Pointer(addr)<(obj^.map_base+obj^.map_size)) then
  begin
   r.LastAdr:=nil;
   r.LastNid:=0;

   r.Addr:=Pointer(addr);
   r.Base:=fuptr(obj^.map_base);

   info.base_addr:=QWORD(r.Base);

   info.source:=Default(t_id_name);
   md_copyin(@obj^.name,@info.source,SizeOf(t_id_name),nil);

   if (info.source='') then
   begin
    md_copyin(obj^.lib_path,@info.source,SizeOf(t_id_name),nil);
   end;

   if (find_proc_obj(obj,r)<>0) then
   begin
    info.func:=ps4libdoc.GetFunctName(r.LastNid);
    if (info.func='Unknow') then
    begin
     info.func:=EncodeValue64(r.LastNid);
    end;
    info.func_addr:=QWORD(r.LastAdr);
    Result:=True;
   end else
   begin
    info.func_addr:=0;

    adr:=QWORD(obj^.init_proc_addr);
    if (adr<=QWORD(r.Addr)) then
    if (adr>info.func_addr) then
    begin
     info.func:='dtInit';
     info.func_addr:=adr;
     Result:=True;
    end;

    adr:=QWORD(obj^.fini_proc_addr);
    if (adr<=QWORD(r.Addr)) then
    if (adr>info.func_addr) then
    begin
     info.func:='dtFini';
     info.func_addr:=adr;
     Result:=True;
    end;

    adr:=QWORD(obj^.entry_addr);
    if (adr<=QWORD(r.Addr)) then
    if (adr>info.func_addr) then
    begin
     info.func:='Entry';
     info.func_addr:=adr;
     Result:=True;
    end;

   end;

   Break;
  end;
  //
  obj:=fuptr(obj^.link.tqe_next);
 end;

 if lock then
 begin
  dynlibs_unlock;
 end;
end;

function find_obj_by_handle(id:Integer):p_lib_info;
var
 obj:p_lib_info;
begin
 Result:=nil;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if (obj^.id=id) then
  begin
   Exit(obj);
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;
end;

procedure print_frame(var f:text;frame:Pointer);
var
 info:TDynlibLineInfo;
 offset1:QWORD;
 offset2:QWORD;
begin
 if is_guest_addr(ptruint(frame)) then
 begin
  info:=Default(TDynlibLineInfo);

  if GetDynlibLineInfo(ptruint(frame),info) then
  begin
   offset1:=QWORD(frame)-QWORD(info.base_addr);
   offset2:=QWORD(frame)-QWORD(info.func_addr);

   Writeln(f,'  offset $',HexStr(offset1 shr 48,5),'|',HexStr(offset1,6),'  ',info.source,':',info.func,'+$',HexStr(offset2,6));
  end else
  begin
   if (info.base_addr<>0) then
   begin
    offset1:=QWORD(frame)-QWORD(info.base_addr);

    Writeln(f,'  offset $',HexStr(offset1 shr 48,5),'|',HexStr(offset1,6),'  ',info.source);
   end else
   begin
    Writeln(f,'  $',HexStr(frame),'  ',info.source);
   end;
  end;
 end else
 if (BackTraceStrFunc<>nil) then
 begin
  Writeln(f,BackTraceStrFunc(frame));
 end else
 begin
  Writeln(f,'  $',HexStr(frame));
 end;
end;

procedure print_backtrace(var f:text;rip,rbp:Pointer;skipframes:sizeint);
var
 i,count:sizeint;
 frames:array [0..255] of codepointer;
begin
 count:=max_frame_dump;
 count:=30;

 if (skipframes>0) then
 begin
  Dec(skipframes);
 end else
 begin
  print_frame(f,rip);
 end;

 count:=CaptureBacktrace(curkthread,rbp,skipframes,count,@frames[0]);

 if (count<>0) then
 for i:=0 to count-1 do
 begin
  print_frame(f,frames[i]);
 end;
end;

procedure print_backtrace_td(var f:text);
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit;
 //
 print_backtrace(stderr,Pointer(td^.td_frame.tf_rip),Pointer(td^.td_frame.tf_rbp),0);
end;

procedure print_error_td(const str:shortstring);
begin
 thread_suspend_all(p_host_ipc.Ftd);

 Writeln(StdErr,str);
 p_host_ipc.error(str);

 print_backtrace_td(StdErr);
end;

end.

