unit kern_dlsym;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 elf64,
 subr_dynlib;

const
 SYMLOOK_BASE64   =$001;
 SYMLOOK_IN_PLT   =$002;
 SYMLOOK_NOT_DBG  =$008;
 SYMLOOK_DLSYM    =$00A;
 SYMLOOK_MANGLED  =$100;

type
 p_SymLook=^t_SymLook;
 t_SymLook=record
  libname   :pchar;
  modname   :pchar;
  symbol    :pchar;
  nid       :QWORD;
  flags     :DWORD;
  obj       :p_lib_info;
  defobj_out:p_lib_info;
  sym_out   :p_elf64_sym;
 end;

function do_dlsym(obj:p_lib_info;symbol,libname:pchar;flags:DWORD):Pointer;
function name_dlsym(name,symbol:pchar;addrp:ppointer):Integer;
function find_symdef(symnum:QWORD;
                     refobj:p_lib_info;
                     var defobj_out:p_lib_info;
                     flags:DWORD;
                     cache:p_SymCache;
                     where:Pointer):p_elf64_sym;

implementation

uses
 hamt,
 errno,
 systm,
 elf_nid_utils,
 kern_stub,
 vm_patch_link,
 subr_backtrace,
 ps4libdoc;

function symlook_obj(req:p_SymLook;obj:p_lib_info):Integer;
label
 _next;
var
 Lib_Entry:p_Lib_Entry;
 data:PPointer;
 h_entry:p_sym_hash_entry;
 symp:p_elf64_sym;
 ST_TYPE:Integer;
begin
 Result:=0;

 Lib_Entry:=get_lib_entry_by_name(obj,req^.libname);
 if (Lib_Entry=nil) then Exit(ESRCH);

 data:=HAMT_search64(Lib_Entry^.hamt,req^.nid);
 if (data=nil) then Exit(ESRCH);

 h_entry:=data^;
 if (h_entry=nil) then Exit(ESRCH);

 symp:=@h_entry^.sym;

 ST_TYPE:=ELF64_ST_TYPE(symp^.st_info);

 Case ST_TYPE of
  STT_SECTION:;
  STT_NOTYPE,
  STT_OBJECT,
  STT_FUN,
  STT_SCE:
     if (symp^.st_value<>0) then
     begin
      if (symp^.st_shndx<>SHN_UNDEF) or
         ((ST_TYPE=STT_FUN) and
          ((req^.flags and SYMLOOK_IN_PLT)=0)
         ) then
      begin
       goto _next;
      end;
     end;
  STT_TLS:
     if (symp^.st_shndx<>SHN_UNDEF) then
     begin
      goto _next;
     end;
  else;
 end; //case

 Exit(ESRCH);

 _next:

 req^.sym_out:=symp;
 req^.defobj_out:=obj;

 //needed_filtees/needed_aux_filtees->symlook_needed not used

 Result:=0;
end;

function symlook_list(req:p_SymLook;var objlist:TAILQ_HEAD;var dlp:t_DoneList):Integer;
label
 _symlook_obj;
var
 modname:pchar;
 req1:t_SymLook;
 elm:p_Objlist_Entry;
 def:p_elf64_sym;
 defobj:p_lib_info;
 str:pchar;
begin
 Result:=0;

 if ((req^.flags and SYMLOOK_MANGLED)=0) then
 begin
  modname:=req^.modname;
 end else
 if (req^.symbol=nil) then
 begin
  modname:=nil;
 end else
 begin
  modname:=strrscan(req^.symbol,'#');
  if (modname<>nil) then
  begin
   modname:=modname+1;
  end;
 end;

 def   :=nil;
 defobj:=nil;

 elm:=TAILQ_FIRST(@objlist);

 while (elm<>nil) do
 begin
  if not donelist_check(dlp,elm^.obj) then
  begin
   if (modname=nil) then //any module?
   begin
    _symlook_obj:
    req1:=req^;
    Result:=symlook_obj(@req1,elm^.obj);

    if (Result=0) then
    begin
     if (def=nil) or (ELF64_ST_BIND(req1.sym_out^.st_info)<>STB_WEAK) then
     begin
      def   :=req1.sym_out;
      defobj:=req1.defobj_out;
      if (ELF64_ST_BIND(def^.st_info)<>STB_WEAK) then Break;
     end;
    end;
   end else
   begin
    str:=get_mod_name_by_id(elm^.obj,0); //export=0
    if (StrComp(str,modname)=0) then
    begin
     goto _symlook_obj;
    end;
   end;
  end;
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;

 if (def<>nil) then
 begin
  req^.sym_out   :=def;
  req^.defobj_out:=defobj;
  Exit(0);
 end;

 Exit(ESRCH);
end;

function symlook_global(req:p_SymLook;var donelist:t_DoneList):Integer;
var
 req1:t_SymLook;
 elm:p_Objlist_Entry;
begin
 req1:=req^;

 //Search all objects loaded at program start up.
 if (req^.defobj_out=nil) or
    (ELF64_ST_BIND(req^.sym_out^.st_info)=STB_WEAK) then
 begin
  Result:=symlook_list(@req1, dynlibs_info.list_main, donelist);

  if (Result=0) then
  begin
   if (req^.defobj_out=nil) or
      (ELF64_ST_BIND(req1.sym_out^.st_info)<>STB_WEAK) then
   begin
    req^.sym_out   :=req1.sym_out;
    req^.defobj_out:=req1.defobj_out;
    Assert(req^.defobj_out<>nil,'req->defobj_out is NULL #1');
   end;
  end;
 end;

 //Search all DAGs whose roots are RTLD_GLOBAL objects.
 elm:=TAILQ_FIRST(@dynlibs_info.list_global);
 while (elm<>nil) do
 begin
  if (req^.defobj_out<>nil) and
     (ELF64_ST_BIND(req^.sym_out^.st_info)<>STB_WEAK) then
  begin
   Break;
  end;

  Result:=symlook_list(@req1,elm^.obj^.dagmembers,donelist);

  if (Result=0) then
  begin
   if (req^.defobj_out=nil) or
      (ELF64_ST_BIND(req1.sym_out^.st_info)<>STB_WEAK) then
   begin
    req^.sym_out   :=req1.sym_out;
    req^.defobj_out:=req1.defobj_out;
    Assert(req^.defobj_out<>nil,'req->defobj_out is NULL #2');
   end;
  end;

  //
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;

 if (req^.sym_out<>nil) then
  Exit(0)
 else
  Exit(ESRCH);
end;

function symlook_default(req:p_SymLook;refobj:p_lib_info):Integer;
var
 donelist:t_DoneList;
 elm:p_Objlist_Entry;
 req1:t_SymLook;
begin
 donelist:=Default(t_DoneList);
 donelist_init(donelist);
 req1:=req^;

 symlook_global(req,donelist);

 elm:=TAILQ_FIRST(@refobj^.dagmembers);
 while (elm<>nil) do
 begin

  if (req^.sym_out<>nil) then
  begin
   if (ELF64_ST_BIND(req^.sym_out^.st_info)<>STB_WEAK) then
   begin
    Break;
   end;
  end;

  Result:=symlook_list(@req1,elm^.obj^.dagmembers,donelist);

  if (Result=0) then
  begin
   if (req^.sym_out=nil) or
      (ELF64_ST_BIND(req1.sym_out^.st_info)<>STB_WEAK) then
   begin
    req^.sym_out   :=req1.sym_out;
    req^.defobj_out:=req1.defobj_out;
    Assert(req^.defobj_out<>nil,'req->defobj_out is NULL #2');
   end;
  end;

  //
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;

 if (req^.sym_out<>nil) then
  Exit(0)
 else
  Exit(ESRCH);
end;

function do_dlsym(obj:p_lib_info;symbol,libname:pchar;flags:DWORD):Pointer;
var
 req:t_SymLook;
 donelist:t_DoneList;
 err:Integer;
begin
 Result:=nil;
 if (obj=nil) then Exit;

 req:=Default(t_SymLook);
 req.flags:=flags or SYMLOOK_DLSYM;

 if ((flags and SYMLOOK_BASE64)=0) then
 begin
  req.modname:=get_mod_name_by_id(obj,0); //export=0
  req.libname:=libname;
  if (libname=nil) then
  begin
   req.libname:=req.modname;
  end;
  req.nid:=ps4_nid_hash(symbol);
 end else
 begin
  req.libname:=nil;
  req.modname:=nil;
  DecodeValue64(symbol,strlen(symbol),req.nid);
 end;

 req.symbol:=symbol;
 req.obj :=obj;

 donelist:=Default(t_DoneList);
 donelist_init(donelist);

 err:=0;
 if (obj^.rtld_flags.mainprog=0) then
 begin
  err:=symlook_list(@req,obj^.dagmembers,donelist);
 end else
 begin
  err:=symlook_global(@req,donelist);
 end;

 if (err<>0) then
 begin
  req.defobj_out:=nil;
  req.sym_out   :=nil;
 end;

 if (req.sym_out=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=req.defobj_out^.relocbase + req.sym_out^.st_value;
 end;
end;

function name_dlsym(name,symbol:pchar;addrp:ppointer):Integer;
label
 _exit;
var
 obj:p_lib_info;
 ptr:Pointer;

 fname:array[0..31] of char;
 fsymb:array[0..2560-1] of char;
begin
 Result:=copyinstr(name,@fname,sizeof(fname),nil);
 if (Result<>0) then Exit;

 Result:=copyinstr(symbol,@fsymb,sizeof(fsymb),nil);
 if (Result<>0) then Exit;

 dynlibs_lock;

 obj:=find_obj_by_name(@fname);
 if (obj=nil) then
 begin
  Result:=ESRCH;
  goto _exit;
 end;

 ptr:=do_dlsym(obj,@fsymb,nil,0);

 if (ptr=nil) then
 begin
  Result:=ESRCH;
  goto _exit;
 end;

 Result:=copyout(@ptr,addrp,SizeOf(Pointer));

 _exit:
  dynlibs_unlock;
end;

//48 8D 3D 00 00 00 00 lea rdi,[rip+$00000000]   lea (%rip),%rdi

type
 p_jmpq64_trampoline=^t_jmpq64_trampoline;
 t_jmpq64_trampoline=packed record
  lea    :array[0..6] of Byte; //48 8D 3D F9 FF FF FF
  nop1   :Byte;  //90
  //
  and_rsp:DWORD; //48 83 E4 F0
  //
  inst   :Word;  //FF 15
  offset :DWORD; //02
  ret    :Byte;  //C3
  nop2   :Byte;  //90
  addr   :QWORD;
  nid    :QWORD;
  libname:PChar;
 end;

const
 c_jmpq64_trampoline:t_jmpq64_trampoline=(lea    :($48,$8D,$3D,$F9,$FF,$FF,$FF);
                                          nop1   :$90;
                                          and_rsp:($F0E48348);
                                          inst   :$15FF;offset:$02;
                                          ret    :$C3;
                                          nop2   :$90;
                                          addr   :0;
                                          nid    :0;
                                          libname:nil);

procedure _unresolve_symbol(data:p_jmpq64_trampoline);
var
 str:shortstring;
begin
 str:=ps4libdoc.GetFunctName(data^.nid);
 if (str='Unknow') then
 begin
  str:=EncodeValue64(data^.nid);
 end;

 Writeln(StdErr,'_unresolve_symbol:0x',HexStr(data^.nid,16),':',str,':',data^.libname);
 print_backtrace_td(StdErr);
 Assert(false);
end;

function get_unresolve_ptr(refobj:p_lib_info;where:Pointer;nid:QWORD;libname:PChar):Pointer;
var
 stub:p_stub_chunk;
begin
 stub:=p_alloc(nil,SizeOf(t_jmpq64_trampoline));

 p_jmpq64_trampoline(@stub^.body)^:=c_jmpq64_trampoline;
 p_jmpq64_trampoline(@stub^.body)^.addr:=QWORD(@_unresolve_symbol);
 p_jmpq64_trampoline(@stub^.body)^.nid:=nid;
 p_jmpq64_trampoline(@stub^.body)^.libname:=libname;

 Result:=@stub^.body;

 vm_add_patch_link(refobj^.rel_data^.obj,where,SizeOf(Pointer),pt_unresolve,stub);
end;

function find_symdef(symnum:QWORD;
                     refobj:p_lib_info;
                     var defobj_out:p_lib_info;
                     flags:DWORD;
                     cache:p_SymCache;
                     where:Pointer):p_elf64_sym;

var
 req:t_SymLook;
 def:p_elf64_sym;
 ref:p_elf64_sym;
 defobj:p_lib_info;
 str:pchar;
 count:Integer;
 err:Integer;
 ST_BIND:Integer;

 mod_id,lib_id:WORD;

 ptr:Pointer;
 stub:p_stub_chunk;
begin
 Result:=nil;

 if (refobj^.rel_data^.dynsymcount<=symnum) then Exit(nil);

 if (cache<>nil) then
 begin
  if (cache[symnum].sym<>nil) then
  begin
   defobj_out:=cache[symnum].obj;
   Exit(cache[symnum].sym);
  end;
 end;

 def:=nil;
 defobj_out:=nil;

 count:=refobj^.rel_data^.symtab_size div SizeOf(elf64_sym);
 if (symnum>=count) then Exit(nil);

 ref:=refobj^.rel_data^.symtab_addr + symnum;

 if (ref^.st_shndx=SHN_UNDEF) then //is import
 if ((flags and $200)<>0) then     //is export only
 begin
  Exit(nil);
 end;

 str:=obj_get_str(refobj,ref^.st_name);

 ST_BIND:=ELF64_ST_BIND(ref^.st_info);
 if (ST_BIND=STB_LOCAL) then
 begin
  def   :=ref;
  defobj:=refobj;
 end else
 begin
  if (ST_BIND=STT_SECTION) then
  begin
   Writeln(StdErr,'find_symdef:',refobj^.lib_path,': Bogus symbol table entry ',symnum);
  end;

  req:=Default(t_SymLook);

  req.symbol:=str;
  req.flags :=flags;
  req.obj   :=refobj;

  mod_id:=0;
  lib_id:=0;

  if DecodeSym(refobj,
               str,
               ELF64_ST_TYPE(ref^.st_info),
               mod_id,
               lib_id,
               req.nid) then
  begin
   req.modname:=get_mod_name_by_id(refobj,mod_id);
   req.libname:=get_lib_name_by_id(refobj,lib_id);
  end;

  err:=symlook_default(@req, refobj);
  if (err=0) then
  begin
   def   :=req.sym_out;
   defobj:=req.defobj_out;
  end;
 end;

 if (def=nil) and ((flags and $200)<>0) then //is export only
 begin
  Exit(nil);
 end;

 if (def=nil) then
 begin
  if (ELF64_ST_BIND(ref^.st_info)=STB_WEAK) then
  begin
   def   :=@dynlibs_info.sym_zero;
   defobj:=dynlibs_info.libprogram;
  end else
  if (where<>nil) and
     (ELF64_ST_TYPE(ref^.st_info)<>STT_OBJECT) then
  begin
   stub:=vm_get_patch_link(refobj^.rel_data^.obj,where);

   if (stub<>nil) then
   begin
    ptr:=@stub^.body;
    p_dec_ref(stub);
   end else
   begin
    ptr:=get_unresolve_ptr(refobj,where,req.nid,req.libname);
   end;

   dynlibs_info.sym_nops.st_info :=(STB_GLOBAL shl 4) or STT_NOTYPE;
   dynlibs_info.sym_nops.st_shndx:=SHN_UNDEF;
   dynlibs_info.sym_nops.st_value:=-Int64(dynlibs_info.libprogram^.relocbase)+Int64(ptr);

   def   :=@dynlibs_info.sym_nops;
   defobj:=dynlibs_info.libprogram;
  end;
 end else
 begin
  vm_rem_patch_link(refobj^.rel_data^.obj,where);
 end;

 if (def<>nil) then
 begin
  defobj_out:=defobj;

  if (cache<>nil) then
  begin
   cache[symnum].sym:=def;
   cache[symnum].obj:=defobj;
  end;
 end;

 Exit(def);
end;


end.

