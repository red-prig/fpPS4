unit kern_dlsym;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 mqueue,
 elf64,
 kern_thr,
 kern_rtld,
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
  name      :pchar;
  modname   :pchar;
  libname   :pchar;
  symbol    :pchar;
  hash      :QWORD;
  flags     :DWORD;
  obj       :p_lib_info;
  defobj_out:p_lib_info;
  sym_out   :p_elf64_sym;
 end;

function do_dlsym(obj:p_lib_info;symbol,modname:pchar;flags:DWORD):Pointer;
function find_symdef(symnum:QWORD;refobj:p_lib_info;var defobj_out:p_lib_info;flags:DWORD;cache:p_SymCache):p_elf64_sym;

implementation

uses
 errno,
 elf_nid_utils;

function convert_raw_symbol_str_to_base64(symbol:pchar):RawByteString;
var
 nid:QWORD;
begin
 nid:=ps4_nid_hash(symbol);
 Result:=EncodeValue64(nid);
end;

function symlook_obj(req:p_SymLook;obj:p_lib_info):Integer;
begin
 ///////

end;

function symlook_list(req:p_SymLook;var objlist:TAILQ_HEAD;var dlp:t_DoneList):Integer;
label
 _symlook_obj;
var
 libname:pchar;
 req1:t_SymLook;
 elm:p_Objlist_Entry;
 def:p_elf64_sym;
 defobj:p_lib_info;
 lib_entry:p_Lib_Entry;
 offset:QWORD;
 str:pchar;
begin
 Result:=0;

 if ((req^.flags and SYMLOOK_MANGLED)=0) then
 begin
  libname:=req^.libname;
 end else
 if (req^.symbol=nil) then
 begin
  libname:=nil;
 end else
 begin
  libname:=strrscan(req^.symbol,'#');
  if (libname<>nil) then
  begin
   libname:=libname+1;
  end;
 end;

 def   :=nil;
 defobj:=nil;

 elm:=TAILQ_FIRST(@objlist);

 while (elm<>nil) do
 begin
  if not donelist_check(dlp,elm^.obj) then
  begin
   if (libname=nil) then
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
    lib_entry:=TAILQ_FIRST(@elm^.obj^.lib_table);
    while (lib_entry<>nil) do
    begin
     if (lib_entry^.dval.id=0) then //export?
     begin
      offset:=lib_entry^.dval.name_offset;
      str:=obj_get_str(elm^.obj,offset);
      if (StrComp(str,libname)=0) then
      begin
       goto _symlook_obj;
      end;
      Break;
     end;
     lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
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

  Result:=symlook_list(@req1, dynlibs_info.needed, donelist);

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

function do_dlsym(obj:p_lib_info;symbol,modname:pchar;flags:DWORD):Pointer;
var
 req:t_SymLook;
 lib_entry:p_Lib_Entry;
 offset:QWORD;
 base64:RawByteString;
 donelist:t_DoneList;
 err:Integer;
begin
 Result:=nil;

 if TAILQ_EMPTY(@obj^.lib_table) then
 begin
  req.libname:=nil;
 end else
 begin
  req.libname:=nil;
  lib_entry:=TAILQ_FIRST(@obj^.lib_table);
  while (lib_entry<>nil) do
  begin
   if (lib_entry^.dval.id=0) then //export?
   begin
    offset:=lib_entry^.dval.name_offset;
    req.libname:=obj_get_str(obj,offset);
   end;
   lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
  end;
 end;

 req.flags:=flags or SYMLOOK_DLSYM;

 if ((flags and SYMLOOK_BASE64)=0) then
 begin
  req.modname:=modname;
  if (modname=nil) then
  begin
   req.modname:=req.libname;
  end;
  base64:=convert_raw_symbol_str_to_base64(symbol);
  symbol:=pchar(base64);
 end else
 begin
  req.modname:=nil;
  req.libname:=nil;
 end;

 req.symbol    :=nil;
 req.name      :=symbol;
 //req.hash      :=elf_hash(@req);
 req.defobj_out:=nil;
 req.sym_out   :=nil;
 req.obj       :=obj;

 donelist:=Default(t_DoneList);
 donelist_init(donelist);

 err:=0;
 if (obj^.mainprog=0) then
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

function find_symdef(symnum:QWORD;refobj:p_lib_info;var defobj_out:p_lib_info;flags:DWORD;cache:p_SymCache):p_elf64_sym;
begin
 Result:=nil;
 ////
end;


end.

