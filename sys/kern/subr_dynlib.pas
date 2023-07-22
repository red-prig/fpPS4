unit subr_dynlib;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 mqueue,
 elf64,
 kern_thr,
 kern_rtld,
 kern_named_id,
 kern_sx;

type
 p_rel_data=^t_rel_data;
 t_rel_data=record
  //entry:TAILQ_ENTRY;
  //vm_obj:vm_object_t;
  //refs:Integer;
  //full_size:QWORD;

  symtab_addr:p_elf64_sym;
  symtab_size:QWORD;

  strtab_addr:pchar;
  strtab_size:QWORD;

  pltrela_addr:p_elf64_rela;
  pltrela_size:QWORD;

  rela_addr:p_elf64_rela;
  rela_size:QWORD;

  hash_addr:Pointer;
  hash_size:QWORD;

  dynamic_addr:p_elf64_dyn;
  dynamic_size:QWORD;

  sce_comment_addr:PByte;
  sce_comment_size:QWORD;

  sce_dynlib_addr:Pointer;
  sce_dynlib_size:QWORD;

  execpath:pchar;

  buckets:PDWORD;
  buckets_size:QWORD;

  chains:PDWORD;
  chains_size:QWORD;

  nbuckets:DWORD;
  dynsymcount:DWORD;

  original_filename:pchar;

  //void * sce_dynlib_data;
  //void * elf_hdr;
 end;

 p_lib_info=^t_lib_info;
 t_lib_info=object(t_id_named_desc)
  link:TAILQ_ENTRY;

  lib_path   :PAnsiChar;
  lib_dirname:PAnsiChar;

  ref_count:Integer;
  id       :Integer;

  map_base :Pointer;
  map_size :QWORD;
  text_size:QWORD;

  data_addr:Pointer;
  data_size:QWORD;

  relro_addr:Pointer;
  relro_size:QWORD;

  relocbase :Pointer;
  entry_addr:Pointer;

  tls_index:QWORD;

  tls_init_addr:Pointer;
  tls_init_size:QWORD;
  tls_size     :QWORD;
  tls_offset   :QWORD;
  tls_align    :QWORD;

  //pltgot:Pointer;

  needed     :TAILQ_HEAD; //Needed_Entry
  lib_table  :TAILQ_HEAD; //Lib_Entry
  mod_table  :TAILQ_HEAD; //Lib_Entry
  names      :TAILQ_HEAD; //Name_Entry

  init_proc_addr:Pointer;
  fini_proc_addr:Pointer;

  eh_frame_hdr_addr:Pointer;
  eh_frame_hdr_size:QWORD;

  eh_frame_addr:Pointer;
  eh_frame_size:QWORD;

  loaded:Integer;

  //t_rtld_bits rtld_flags;

  mainprog     :Integer;
  tls_done     :Integer;
  init_scanned :Integer;
  init_done    :Integer;
  on_fini_list :Integer;
  not_get_proc :Integer;
  textrel      :Integer;
  init_plt     :Integer;
  is_system    :Integer;
  dag_inited   :Integer;
  jmpslots_done:Integer;

  dldags    :TAILQ_HEAD; //Objlist_Entry
  dagmembers:TAILQ_HEAD; //Objlist_Entry

  relo_bits:PByte;

  rel_data:p_rel_data;

  fingerprint:array[0..19] of Byte;

  module_param:pSceModuleParam;
 end;

 p_Objlist_Entry=^Objlist_Entry;
 Objlist_Entry=record
  link:TAILQ_ENTRY;
  obj :p_lib_info;
 end;

 p_Needed_Entry=^Needed_Entry;
 Needed_Entry=record
  link :TAILQ_ENTRY;
  obj  :p_lib_info;
  flags:QWORD;
  name :Char;
 end;

 p_Name_Entry=^Name_Entry;
 Name_Entry=record
  link:TAILQ_ENTRY;
  name:Char;
 end;

 p_Lib_Entry=^Lib_Entry;
 Lib_Entry=record
  link  :TAILQ_ENTRY;
  dval  :TLibraryValue;
  attr  :WORD;
  import:WORD;
 end;

 t_DoneList=record
  objs:array of p_lib_info;
  num_used:DWORD;
 end;

 p_SymCache=^t_SymCache;
 t_SymCache=record
  sym:p_elf64_sym;
  obj:p_lib_info;
 end;

 p_dynlibs_info=^t_dynlibs_info;
 t_dynlibs_info=record
  lock       :t_sx;

  obj_list   :TAILQ_HEAD; //p_lib_info
  libprogram :p_lib_info;
  libkernel  :p_lib_info;
  obj_count  :Integer;
  list_global:TAILQ_HEAD; //p_Objlist_Entry
  list_main  :TAILQ_HEAD; //p_Objlist_Entry
  init_list  :TAILQ_HEAD; //p_Objlist_Entry
  fini_list  :TAILQ_HEAD; //p_Objlist_Entry

  init_proc_list:TAILQ_HEAD; //p_Objlist_Entry
  fini_proc_list:TAILQ_HEAD; //p_Objlist_Entry

  tls_last_offset :QWORD;
  tls_last_size   :QWORD;
  tls_static_space:QWORD;

  tls_count   :Integer;
  tls_max     :Integer;

  proc_param_addr:pSceProcParam;
  proc_param_size:QWORD;

  rep_unpf:Pointer;
  __freeze:Pointer;
  sysc_s00:Pointer;
  sysc_e00:Pointer;

  sym_zero:elf64_sym;

  dyn_non_exist:Integer;
 end;

procedure dynlibs_lock;
procedure dynlibs_unlock;

function  obj_new():p_lib_info;
procedure obj_free(obj:p_lib_info);

procedure objlist_push_tail(var list:TAILQ_HEAD;obj:p_lib_info);
function  objlist_find(var list:TAILQ_HEAD;obj:p_lib_info):p_Objlist_Entry;
procedure objlist_remove(var list:TAILQ_HEAD;obj:p_lib_info);

function  obj_get_str      (obj:p_lib_info;offset:Int64):pchar;
procedure object_add_name  (obj:p_lib_info;name:pchar);
function  object_match_name(obj:p_lib_info;name:pchar):Boolean;
procedure obj_set_lib_path (obj:p_lib_info;path:PAnsiChar);

function  Needed_new(obj:p_lib_info;str:pchar):p_Needed_Entry;

function  Lib_Entry_new(d_val:QWORD;import:Word):p_Lib_Entry;
procedure Lib_Entry_free(lib:p_Lib_Entry);

function  get_mod_name(obj:p_lib_info;id:Word):pchar;
function  get_lib_name(obj:p_lib_info;id:Word):pchar;

procedure release_per_file_info_obj(obj:p_lib_info);
function  acquire_per_file_info_obj(imgp:p_image_params;new:p_lib_info):Integer;

function  allocate_tls_offset(obj:p_lib_info):Boolean;
procedure free_tls_offset(obj:p_lib_info);

procedure initlist_add_objects(var fini_proc_list:TAILQ_HEAD;
                               obj :p_lib_info;
                               tail:p_lib_info;
                               var init_proc_list:TAILQ_HEAD);

procedure initlist_add_neededs(var fini_proc_list:TAILQ_HEAD;
                               needed:p_Needed_Entry;
                               var init_proc_list:TAILQ_HEAD);

function  digest_dynamic(obj:p_lib_info):Integer;

procedure dynlibs_add_obj(obj:p_lib_info);

procedure init_relo_bits (obj:p_lib_info);
function  check_relo_bits(obj:p_lib_info;i:Integer):Boolean;
procedure set_relo_bits  (obj:p_lib_info;i:Integer);
procedure reset_relo_bits(obj:p_lib_info;i:Integer);

procedure donelist_init(var dlp:t_DoneList);
function  donelist_check(var dlp:t_DoneList;obj:p_lib_info):Boolean;

function  change_relro_protection(obj:p_lib_info;prot:Integer):Integer;
function  change_relro_protection_all(prot:Integer):Integer;

function  relocate_text_or_data_segment(obj:p_lib_info;src,dst:Pointer;size:QWORD):Integer;

procedure init_dag (root:p_lib_info);
procedure ref_dag  (root:p_lib_info);
procedure unref_dag(root:p_lib_info);

function  dynlib_initialize_pltgot_each(obj:p_lib_info):Integer;

function  do_load_object(path:pchar;flags:DWORD;var err:Integer):p_lib_info;
procedure unload_object(root:p_lib_info);

function  relocate_object(root:p_lib_info):Integer;
function  dynlib_load_relocate():Integer;

function  preload_prx_modules(path:pchar;flags:DWORD;var err:Integer):p_lib_info;
function  load_prx(path:pchar;flags:DWORD;var pobj:p_lib_info):Integer;
function  unload_prx(obj:p_lib_info):Integer;

function  alloc_obj_id(obj:p_lib_info):Boolean;
function  free_obj_id (id:Integer):Boolean;
function  find_obj_id (id:Integer):p_lib_info;

function  find_obj_by_handle(id:Integer):p_lib_info;

function  dynlib_load_needed_shared_objects():Integer;

var
 dynlibs_info:t_dynlibs_info;

implementation

uses
 errno,
 systm,
 vm,
 vmparam,
 vm_map,
 vm_mmap,
 vm_object,
 vuio,
 vstat,
 vfcntl,
 vnode,
 vmount,
 vnamei,
 vfs_lookup,
 vfs_subr,
 vnode_if,
 kern_reloc,
 kern_namedobj;

procedure dynlibs_lock;
begin
 sx_xlock(@dynlibs_info.lock);
end;

procedure dynlibs_unlock;
begin
 sx_xunlock(@dynlibs_info.lock);
end;

function obj_new():p_lib_info;
begin
 Result:=AllocMem(SizeOf(t_lib_info));

 TAILQ_INIT(@Result^.needed);
 TAILQ_INIT(@Result^.lib_table);
 TAILQ_INIT(@Result^.mod_table);
 TAILQ_INIT(@Result^.names);

 TAILQ_INIT(@Result^.dldags);
 TAILQ_INIT(@Result^.dagmembers);

 //Result^.rel_data:=(t_rel_data *)0x0;

 //puVar1:=&(Result^.rtld_flags).field_0x1;
 //*puVar1:=*puVar1 | 2;
end;

function preprocess_dt_entries(new:p_lib_info;hdr_e_type:Integer):Integer;
label
 _unsupp;
var
 dt_ent:p_elf64_dyn;
 i,count:Integer;

 SCE_SYMTABSZ         :Boolean;
 SCE_HASHSZ           :Boolean;
 SCE_SYMENT           :Boolean;
 SCE_SYMTAB           :Boolean;
 SCE_STRSZ            :Boolean;
 SCE_STRTAB           :Boolean;
 SCE_RELAENT          :Boolean;
 SCE_PLTREL           :Boolean;
 SCE_RELASZ           :Boolean;
 SCE_RELA             :Boolean;
 SCE_PLTRELSZ         :Boolean;
 SCE_JMPREL           :Boolean;
 SCE_PLTGOT           :Boolean;
 SCE_HASH             :Boolean;
 SCE_MODULE_INFO      :Boolean;
 SCE_ORIGINAL_FILENAME:Boolean;
 SCE_FINGERPRINT      :Boolean;
begin
 Result:=0;

 dt_ent:=new^.rel_data^.dynamic_addr;
 count :=new^.rel_data^.dynamic_size div sizeof(elf64_dyn);

 SCE_SYMTABSZ         :=False;
 SCE_HASHSZ           :=False;
 SCE_SYMENT           :=False;
 SCE_SYMTAB           :=False;
 SCE_STRSZ            :=False;
 SCE_STRTAB           :=False;
 SCE_RELAENT          :=False;
 SCE_PLTREL           :=False;
 SCE_RELASZ           :=False;
 SCE_RELA             :=False;
 SCE_PLTRELSZ         :=False;
 SCE_JMPREL           :=False;
 SCE_PLTGOT           :=False;
 SCE_HASH             :=False;
 SCE_MODULE_INFO      :=False;
 SCE_ORIGINAL_FILENAME:=False;
 SCE_FINGERPRINT      :=False;

 if (count<>0) then
 For i:=0 to count-1 do
 begin
  case dt_ent^.d_tag of
   DT_NULL,
   DT_NEEDED,
   DT_INIT,
   DT_FINI,
   DT_SONAME,
   DT_SYMBOLIC,
   DT_DEBUG,
   DT_TEXTREL,
   DT_INIT_ARRAY,
   DT_FINI_ARRAY,
   DT_INIT_ARRAYSZ,
   DT_FINI_ARRAYSZ,
   DT_FLAGS,
   DT_PREINIT_ARRAY,
   DT_PREINIT_ARRAYSZ,
   DT_SCE_NEEDED_MODULE,
   DT_SCE_MODULE_ATTR,
   DT_SCE_EXPORT_LIB,
   DT_SCE_IMPORT_LIB,
   DT_SCE_EXPORT_LIB_ATTR,
   DT_SCE_IMPORT_LIB_ATTR,
   DT_RELACOUNT,
   DT_FLAGS_1:; //ignore

   DT_PLTRELSZ,
   DT_SCE_PLTRELSZ:
     begin
      SCE_PLTRELSZ:=true;
      new^.rel_data^.pltrela_size:=dt_ent^.d_un.d_val;
     end;

   DT_PLTREL,
   DT_SCE_PLTREL:
     begin
      SCE_PLTREL:=true;
      if (dt_ent^.d_un.d_val<>7) then
      begin
       Writeln(StdErr,'preprocess_dt_entries:','illegal value in DT_PLTREL entry',' found in ',new^.lib_path);
       Exit(EINVAL);
      end;
     end;

   DT_RELASZ,
   DT_SCE_RELASZ:
     begin
      SCE_RELASZ:=true;
      new^.rel_data^.rela_size:=dt_ent^.d_un.d_val;
     end;

   DT_RELAENT,
   DT_SCE_RELAENT:
     begin
      SCE_RELAENT:=true;
      if (dt_ent^.d_un.d_val<>24) then
      begin
       Writeln(StdErr,'preprocess_dt_entries:','illegal value in DT_RELAENT entry',' found in ',new^.lib_path);
       Exit(EINVAL);
      end;
     end;

   DT_STRSZ,
   DT_SCE_STRSZ:
     begin
      SCE_STRSZ:=true;
      new^.rel_data^.strtab_size:=dt_ent^.d_un.d_val;
     end;

   DT_SYMENT,
   DT_SCE_SYMENT:
     begin
      SCE_SYMENT:=true;
      if (dt_ent^.d_un.d_val<>24) then
      begin
       Writeln(StdErr,'preprocess_dt_entries:','illegal value in DT_SYMENT entry',' found in ',new^.lib_path);
       Exit(EINVAL);
      end;
     end;

   DT_SCE_FINGERPRINT:
     begin
      SCE_FINGERPRINT:=true;
     end;

   DT_SCE_ORIGINAL_FILENAME:
     begin
      SCE_ORIGINAL_FILENAME:=true;
     end;

   DT_SCE_MODULE_INFO:
     begin
      SCE_MODULE_INFO:=true;
     end;

   DT_SCE_PLTGOT:
     begin
      SCE_PLTGOT:=true;
     end;

   DT_SCE_HASH:
     begin
      SCE_HASH:=true;
      new^.rel_data^.hash_addr:=Pointer(dt_ent^.d_un.d_val);
     end;

   DT_SCE_JMPREL:
     begin
      SCE_JMPREL:=true;
      new^.rel_data^.pltrela_addr:=Pointer(dt_ent^.d_un.d_val);
     end;

   DT_SCE_RELA:
     begin
      SCE_RELA:=true;
      new^.rel_data^.rela_addr:=Pointer(dt_ent^.d_un.d_val);
     end;

   DT_SCE_STRTAB:
     begin
      SCE_STRTAB:=true;
      new^.rel_data^.strtab_addr:=Pointer(dt_ent^.d_un.d_val);
     end;

   DT_SCE_SYMTAB:
     begin
      SCE_SYMTAB:=true;
      new^.rel_data^.symtab_addr:=Pointer(dt_ent^.d_un.d_val);
     end;

   DT_SCE_HASHSZ:
     begin
      SCE_HASHSZ:=true;
      new^.rel_data^.hash_size:=dt_ent^.d_un.d_val;
     end;

   DT_SCE_SYMTABSZ:
     begin
      SCE_SYMTABSZ:=true;
      new^.rel_data^.symtab_size:=dt_ent^.d_un.d_val;
     end;

   DT_PLTGOT,
   DT_RPATH,
   DT_BIND_NOW,
   DT_RUNPATH,
   DT_ENCODING,
   $61000008,
   $6100000a,
   $6100000b,
   $6100000c,
   $6100000e,
   $61000010,
   $61000012,
   $61000014,
   $61000016,
   $61000018,
   $6100001a,
   $6100001b,
   $6100001c,
   DT_SCE_STUB_MODULE_NAME,
   $6100001e,
   DT_SCE_STUB_MODULE_VERSION,
   $61000020,
   DT_SCE_STUB_LIBRARY_NAME,
   $61000022,
   DT_SCE_STUB_LIBRARY_VERSION,
   $61000024,
   $61000026,
   $61000028,
   $6100002a,
   $6100002c,
   $6100002e,
   $61000030,
   $61000032,
   $61000034,
   $61000036,
   $61000038,
   $6100003a,
   $6100003c,
   $6100003e:
     begin
      _unsupp:
      Writeln(StdErr,'preprocess_dt_entries:','Unsupported DT tag 0x',HexStr(dt_ent^.d_tag,8),' found in ',new^.lib_path);
      Exit(ENOEXEC);
     end;

   DT_HASH,
   DT_STRTAB,
   DT_SYMTAB,
   DT_RELA,
   DT_JMPREL:
     begin
      Writeln(StdErr,'preprocess_dt_entries:','ORBIS object file does not support DT tag 0x',HexStr(dt_ent^.d_tag,8),' found in ',new^.lib_path);
      Exit(EINVAL);
     end;

   else
     goto _unsupp;
  end;

  Inc(dt_ent);
 end;

 if (SCE_HASHSZ) and (SCE_SYMTABSZ) then
 begin
   if  ( (hdr_e_type=ET_SCE_DYNAMIC) and ((not SCE_ORIGINAL_FILENAME) or (not SCE_MODULE_INFO)) ) or
       (not SCE_FINGERPRINT) or
       (not SCE_HASH) or
       (not SCE_PLTGOT) or
       (not SCE_JMPREL) or
       (not SCE_PLTREL) or
       (not SCE_PLTRELSZ) or
       (not SCE_RELA) or
       (not SCE_RELASZ) or
       (not SCE_RELAENT) or
       (not SCE_STRTAB) or
       (not SCE_STRSZ) or
       (not SCE_SYMTAB) or
       (not SCE_SYMENT) then
   begin
    Writeln(StdErr,'preprocess_dt_entries:',new^.lib_path,' does not have required tabs.');
    Exit(EINVAL);
   end;
 end else
 begin
  Writeln(StdErr,'preprocess_dt_entries:',new^.lib_path,' does not have DT_SCE_SYMTABSZ or DT_SCE_HASHSZ tabs.');
  Exit(EINVAL);
 end;

end;

procedure release_per_file_info_obj(obj:p_lib_info);
begin
 if (obj^.rel_data<>nil) then
 begin
  FreeMem(obj^.rel_data);
  obj^.rel_data:=nil;
 end;
end;

function acquire_per_file_info_obj(imgp:p_image_params;new:p_lib_info):Integer;
var
 full_size:QWORD;
 src,dst:Pointer;
begin
 Result:=0;

 if (imgp^.dyn_exist=0) then Exit(EINVAL);

 full_size:=sizeOf(t_rel_data)+
            AlignUp(imgp^.sce_dynlib_data_size,8)+
            AlignUp(imgp^.sce_comment_filesz  ,8)+
            strlen(imgp^.execpath)+1;

 new^.rel_data:=AllocMem(full_size);

 dst:=Pointer(new^.rel_data+1);

 src:=Pointer(imgp^.image_header)+imgp^.sce_dynlib_data_addr;

 Move(src^,dst^,imgp^.sce_dynlib_data_size);

 new^.rel_data^.sce_dynlib_addr:=dst;
 new^.rel_data^.sce_dynlib_size:=imgp^.sce_dynlib_data_size;

 dst:=dst+AlignUp(imgp^.sce_dynlib_data_size,8);

 if (imgp^.sce_comment_filesz<>0) then
 begin
  src:=Pointer(imgp^.image_header)+imgp^.sce_comment_offset;

  Move(src^,dst^,imgp^.sce_comment_filesz);

  new^.rel_data^.sce_comment_addr:=dst;
  new^.rel_data^.sce_comment_size:=imgp^.sce_comment_filesz;

  dst:=dst+AlignUp(imgp^.sce_comment_filesz,8);
 end;

 Move(imgp^.execpath^,dst^,strlen(imgp^.execpath));

 new^.rel_data^.execpath:=dst;

 src:=new^.rel_data^.sce_dynlib_addr;

 new^.rel_data^.dynamic_addr:=src+imgp^.dyn_offset;
 new^.rel_data^.dynamic_size:=imgp^.dyn_filesz;

 Result:=preprocess_dt_entries(new,imgp^.hdr_e_type);

 if (Result<>0) then
 begin
  FreeMem(new^.rel_data);
  new^.rel_data:=nil;
  Exit;
 end;

 src:=new^.rel_data^.sce_dynlib_addr;

 new^.rel_data^.symtab_addr :=Pointer(QWORD(src)+QWORD(new^.rel_data^.symtab_addr ));
 new^.rel_data^.strtab_addr :=Pointer(QWORD(src)+QWORD(new^.rel_data^.strtab_addr ));
 new^.rel_data^.pltrela_addr:=Pointer(QWORD(src)+QWORD(new^.rel_data^.pltrela_addr));
 new^.rel_data^.rela_addr   :=Pointer(QWORD(src)+QWORD(new^.rel_data^.rela_addr   ));
 new^.rel_data^.hash_addr   :=Pointer(QWORD(src)+QWORD(new^.rel_data^.hash_addr   ));

 src:=new^.rel_data^.hash_addr;

 new^.rel_data^.nbuckets    :=PDWORD(src)^;
 new^.rel_data^.buckets_size:=new^.rel_data^.nbuckets shl 2;

 new^.rel_data^.buckets    :=Pointer(QWORD(src) + 8);
 new^.rel_data^.dynsymcount:=PDWORD (QWORD(src) + 4)^;
 new^.rel_data^.chains_size:=new^.rel_data^.dynsymcount shl 2;

 new^.rel_data^.chains:=Pointer(QWORD(src) + (new^.rel_data^.nbuckets + 2) * 4);

end;

function allocate_tls_offset(obj:p_lib_info):Boolean;
var
 off:Int64;
begin
 if (obj^.tls_done<>0) then
 begin
  Exit(True);
 end;

 off:=obj^.tls_size;
 if (off=0) then
 begin
  obj^.tls_done:=1;
  Exit(True);
 end;

 if (obj^.tls_index=1) then
 begin
  off:=off + -1;
 end else
 begin
  off:=off + -1 + dynlibs_info.tls_last_offset;
 end;

 off:=(off + obj^.tls_align) and (-obj^.tls_align);

 if ((dynlibs_info.tls_static_space<>0) and (dynlibs_info.tls_static_space < off)) then
 begin
  Exit(False);
 end;

 obj^.tls_offset:=off;

 dynlibs_info.tls_last_offset:=off;
 dynlibs_info.tls_last_size  :=obj^.tls_size;

 Result:=True;
end;

procedure free_tls_offset(obj:p_lib_info);
begin
 if (obj^.tls_done<>0) and (obj^.tls_offset=dynlibs_info.tls_last_offset) then
 begin
  dynlibs_info.tls_last_offset:=obj^.tls_offset - obj^.tls_size;
  dynlibs_info.tls_last_size  :=0;
 end;
end;

procedure obj_free(obj:p_lib_info);
var
 needed:p_Needed_Entry;
 names:p_Name_Entry;
 dag:p_Objlist_Entry;
 Lib_Entry:p_Lib_Entry;
begin
 free_tls_offset(obj);

 needed:=TAILQ_FIRST(@obj^.needed);
 while (needed<>nil) do
 begin
  TAILQ_REMOVE(@obj^.needed,needed,@needed^.link);
  FreeMem(needed);
  needed:=TAILQ_FIRST(@obj^.needed);
 end;

 names:=TAILQ_FIRST(@obj^.names);
 while (names<>nil) do
 begin
  TAILQ_REMOVE(@obj^.names,names,@names^.link);
  FreeMem(names);
  names:=TAILQ_FIRST(@obj^.names);
 end;

 dag:=TAILQ_FIRST(@obj^.dldags);
 while (dag<>nil) do
 begin
  TAILQ_REMOVE(@obj^.dldags,dag,@dag^.link);
  FreeMem(dag);
  dag:=TAILQ_FIRST(@obj^.dldags);
 end;

 dag:=TAILQ_FIRST(@obj^.dagmembers);
 while (dag<>nil) do
 begin
  TAILQ_REMOVE(@obj^.dagmembers,dag,@dag^.link);
  FreeMem(dag);
  dag:=TAILQ_FIRST(@obj^.dagmembers);
 end;

 if (obj^.lib_dirname<>nil) then
 begin
  FreeMem(obj^.lib_dirname);
  obj^.lib_dirname:=nil;
 end;

 if (obj^.lib_path<>nil) then
 begin
  FreeMem(obj^.lib_path);
  obj^.lib_path:=nil;
 end;

 if (obj^.relo_bits<>nil) then
 begin
  FreeMem(obj^.relo_bits);
  obj^.relo_bits:=nil
 end;

 Lib_Entry:=TAILQ_FIRST(@obj^.lib_table);
 while (Lib_Entry<>nil) do
 begin
  TAILQ_REMOVE(@obj^.lib_table,Lib_Entry,@Lib_Entry^.link);
  Lib_Entry_free(Lib_Entry);
  Lib_Entry:=TAILQ_FIRST(@obj^.lib_table);
 end;

 Lib_Entry:=TAILQ_FIRST(@obj^.mod_table);
 while (Lib_Entry<>nil) do
 begin
  TAILQ_REMOVE(@obj^.mod_table,Lib_Entry,@Lib_Entry^.link);
  Lib_Entry_free(Lib_Entry);
  Lib_Entry:=TAILQ_FIRST(@obj^.mod_table);
 end;

 release_per_file_info_obj(obj);

 FreeMem(obj);
end;

procedure objlist_push_tail(var list:TAILQ_HEAD;obj:p_lib_info);
var
 entry:p_Objlist_Entry;
begin
 entry:=AllocMem(SizeOf(Objlist_Entry));
 entry^.obj:=obj;
 //
 TAILQ_INSERT_TAIL(@list,entry,@entry^.link);
end;

function objlist_find(var list:TAILQ_HEAD;obj:p_lib_info):p_Objlist_Entry;
var
 elm:p_Objlist_Entry;
begin
 elm:=TAILQ_FIRST(@dynlibs_info.list_global);
 while (elm<>nil) do
 begin
  if (elm^.obj=obj) then Exit(elm);
  //
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;
 Result:=nil;
end;

procedure objlist_remove(var list:TAILQ_HEAD;obj:p_lib_info);
var
 elm:p_Objlist_Entry;
begin
 elm:=objlist_find(list,obj);
 if (elm<>nil) then
 begin
  TAILQ_REMOVE(@list,elm,@elm^.link);
  FreeMem(elm);
 end;
end;

function obj_get_str(obj:p_lib_info;offset:Int64):pchar;
begin
 if (obj^.rel_data^.strtab_size<=offset) then
 begin
  Writeln(StdErr,'obj_get_str:','offset=0x',HexStr(offset,8),' is out of range of string table of ',obj^.lib_path);
  Exit(nil);
 end;

 Result:=obj^.rel_data^.strtab_addr+offset;
end;

procedure object_add_name(obj:p_lib_info;name:pchar);
var
 len:Integer;
 entry:p_Name_Entry;
begin
 len:=strlen(name);
 entry:=AllocMem(SizeOf(Name_Entry)+len);
 Move(name^,entry^.name,len);
 //
 TAILQ_INSERT_TAIL(@obj^.names,entry,@entry^.link);
end;

function object_match_name(obj:p_lib_info;name:pchar):Boolean;
var
 entry:p_Name_Entry;
begin
 entry:=TAILQ_FIRST(@obj^.names);
 while (entry<>nil) do
 begin
  if (StrComp(name,@entry^.name)=0) then
  begin
   Exit(True);
  end;
  entry:=TAILQ_NEXT(entry,@entry^.link);
 end;
 Result:=False;
end;

procedure obj_set_lib_path(obj:p_lib_info;path:PAnsiChar);
var
 size:int64;
begin
 size:=strlen(path);
 obj^.lib_path:=AllocMem(size+1);
 Move(path^,obj^.lib_path^,size);
end;

function Needed_new(obj:p_lib_info;str:pchar):p_Needed_Entry;
var
 len:Integer;
begin
 len:=strlen(str);
 Result:=AllocMem(SizeOf(Needed_Entry)+len);
 Result^.obj :=obj;
 Move(str^,Result^.name,len);
end;

function Lib_Entry_new(d_val:QWORD;import:Word):p_Lib_Entry;
begin
 Result:=AllocMem(SizeOf(Lib_Entry));
 QWORD(Result^.dval):=d_val;
 Result^.import:=import;
end;

procedure Lib_Entry_free(lib:p_Lib_Entry);
begin
 //
 FreeMem(lib);
end;

function get_mod_name(obj:p_lib_info;id:Word):pchar;
var
 lib_entry:p_Lib_Entry;
 offset:QWORD;
begin
 Result:=nil;
 lib_entry:=TAILQ_FIRST(@obj^.mod_table);
 while (lib_entry<>nil) do
 begin
  if (lib_entry^.dval.id=id) then
  begin
   offset:=lib_entry^.dval.name_offset;
   Result:=obj_get_str(obj,offset);
   Exit;
  end;
  lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
 end;
end;

function get_lib_name(obj:p_lib_info;id:Word):pchar;
var
 lib_entry:p_Lib_Entry;
 offset:QWORD;
begin
 Result:=nil;
 lib_entry:=TAILQ_FIRST(@obj^.lib_table);
 while (lib_entry<>nil) do
 begin
  if (lib_entry^.dval.id=id) then
  begin
   offset:=lib_entry^.dval.name_offset;
   Result:=obj_get_str(obj,offset);
   Exit;
  end;
  lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
 end;
end;

procedure initlist_add_objects(var fini_proc_list:TAILQ_HEAD;
                               obj :p_lib_info;
                               tail:p_lib_info;
                               var init_proc_list:TAILQ_HEAD);
begin
 if (obj^.init_scanned<>0) or (obj^.init_done<>0) then Exit;
 obj^.init_scanned:=1;

 if (obj<>tail) then
 begin
  initlist_add_objects(fini_proc_list,obj^.link.tqe_next,tail,init_proc_list);
 end;

 if (obj^.needed.tqh_first<>nil) then
 begin
  initlist_add_neededs(fini_proc_list,obj^.needed.tqh_first,init_proc_list);
 end;

 if (obj^.init_proc_addr<>nil) then
 begin
  objlist_push_tail(init_proc_list,obj);
 end;

 if (obj^.fini_proc_addr<>nil) and (obj^.on_fini_list=0) then
 begin
  objlist_push_tail(fini_proc_list,obj);

  obj^.on_fini_list:=1;
 end;
end;

procedure initlist_add_neededs(var fini_proc_list:TAILQ_HEAD;
                               needed:p_Needed_Entry;
                               var init_proc_list:TAILQ_HEAD);
var
 obj:p_lib_info;
begin
 if (needed^.link.tqe_next<>nil) then
 begin
  initlist_add_neededs(fini_proc_list,needed^.link.tqe_next,init_proc_list);
 end;

 obj:=needed^.obj;
 if (obj<>nil) then
 begin
  initlist_add_objects(fini_proc_list,obj,obj,init_proc_list);
 end;
end;

function digest_dynamic(obj:p_lib_info):Integer;
var
 dt_ent:p_elf64_dyn;
 i,count:Integer;

 str:pchar;

 needed:p_Needed_Entry;
 lib_entry:p_Lib_Entry;

 addr:Pointer;
 dval:QWORD;

 dyn_soname:p_elf64_dyn;
 dt_fingerprint:Int64;
begin
 Result:=0;

 dyn_soname:=nil;
 dt_fingerprint:=-1;

 if (obj^.rel_data<>nil) then
 begin
  dt_ent:=obj^.rel_data^.dynamic_addr;
  count :=obj^.rel_data^.dynamic_size div sizeof(elf64_dyn);

  if (count<>0) then
  For i:=0 to count-1 do
  begin

   case dt_ent^.d_tag of
    DT_NULL,
    DT_PLTRELSZ,
    DT_HASH,
    DT_STRTAB,
    DT_SYMTAB,
    DT_RELA,
    DT_RELASZ,
    DT_RELAENT,
    DT_STRSZ,
    DT_SYMENT,
    DT_PLTREL,
    DT_DEBUG,
    DT_JMPREL,
    DT_INIT_ARRAY,
    DT_FINI_ARRAY,
    DT_INIT_ARRAYSZ,
    DT_FINI_ARRAYSZ,
    DT_PREINIT_ARRAY,
    DT_PREINIT_ARRAYSZ,
    DT_SCE_HASH,
    DT_SCE_JMPREL,
    DT_SCE_PLTREL,
    DT_SCE_PLTRELSZ,
    DT_SCE_RELA,
    DT_SCE_RELASZ,
    DT_SCE_RELAENT,
    DT_SCE_STRTAB,
    DT_SCE_STRSZ,
    DT_SCE_SYMTAB,
    DT_SCE_SYMENT,
    DT_SCE_HASHSZ,
    DT_SCE_SYMTABSZ,
    DT_RELACOUNT:;  //ignore

    DT_SONAME:
     begin
      dyn_soname:=dt_ent;
     end;

    DT_PLTGOT,
    DT_SCE_PLTGOT:
     begin
      //pltgot
     end;

    DT_NEEDED:
      begin
       str:=obj_get_str(obj,dt_ent^.d_un.d_val);

       if (str=nil) then
       begin
        Writeln(StdErr,'digest_dynamic:',{$INCLUDE %LINE%});
        Exit(EINVAL);
       end;

       needed:=Needed_new(obj,str);
       TAILQ_INSERT_TAIL(@obj^.needed,needed,@Needed^.link);
      end;

    DT_INIT:
      begin
       addr:=obj^.relocbase+dt_ent^.d_un.d_val;
       obj^.init_proc_addr:=addr;

       if (obj^.map_base>addr) or ((addr+8)>(obj^.map_base+obj^.text_size)) then
       begin
        Writeln(StdErr,'digest_dynamic:',{$INCLUDE %LINE%});
        Exit(ENOEXEC);
       end;
      end;

    DT_FINI:
      begin
       addr:=obj^.relocbase+dt_ent^.d_un.d_val;
       obj^.fini_proc_addr:=addr;

       if (obj^.map_base>addr) or ((addr+8)>(obj^.map_base+obj^.text_size)) then
       begin
        Writeln(StdErr,'digest_dynamic:',{$INCLUDE %LINE%});
        Exit(ENOEXEC);
       end;
      end;

    DT_SYMBOLIC:
      begin
       Writeln(StdErr,'digest_dynamic:','DT_SYMBOLIC is obsolete.');
       Exit(EINVAL);
      end;

    DT_TEXTREL:
      begin
       obj^.textrel:=1;
      end;

    DT_FLAGS:
      begin
       dval:=dt_ent^.d_un.d_val;

       if ((dval and DF_SYMBOLIC)<>0) then
       begin
        Writeln(StdErr,'digest_dynamic:','DT_SYMBOLIC is obsolete.');
        Exit(EINVAL);
       end;

       if ((dval and DF_BIND_NOW)<>0) then
       begin
        Writeln(StdErr,'digest_dynamic:','DF_BIND_NOW is obsolete.');
        Exit(EINVAL);
       end;

       if ((dval and DF_TEXTREL)<>0) then
       begin
        obj^.textrel:=1;
       end;
      end;

    DT_SCE_FINGERPRINT:
      begin
       dt_fingerprint:=dt_ent^.d_un.d_val;

       if (obj^.rel_data=nil) or
          ((dt_fingerprint + 20)>obj^.rel_data^.sce_dynlib_size) then
       begin
        Writeln(StdErr,'digest_dynamic:',{$INCLUDE %LINE%});
        Exit(ENOEXEC);
       end;
      end;

    DT_SCE_ORIGINAL_FILENAME:
      begin
       str:=obj_get_str(obj,dt_ent^.d_un.d_val);

       if (str=nil) then
       begin
        Writeln(StdErr,'digest_dynamic:',{$INCLUDE %LINE%});
        Exit(EINVAL);
       end;

       obj^.rel_data^.original_filename:=str;
      end;

    DT_SCE_MODULE_INFO,
    DT_SCE_NEEDED_MODULE:
      begin
       lib_entry:=Lib_Entry_new(dt_ent^.d_un.d_val,ord(dt_ent^.d_tag=DT_SCE_NEEDED_MODULE));
       TAILQ_INSERT_TAIL(@obj^.mod_table,lib_entry,@lib_entry^.link);
      end;

    DT_SCE_MODULE_ATTR:
      begin
       dval:=dt_ent^.d_un.d_val;

       lib_entry:=TAILQ_FIRST(@obj^.mod_table);
       while (lib_entry<>nil) do
       begin
        if (TLibraryAttr(dval).id=lib_entry^.dval.id) then
        begin
         Break;
        end;
        lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
       end;

       if (lib_entry=nil) then
       begin
        Writeln(StdErr,'digest_dynamic:','unknown ID found in DT_SCE_*_MODULE_ATTR entry ',TLibraryAttr(dval).id);
        Exit(EINVAL);
       end;

       lib_entry^.attr:=TLibraryAttr(dval).attr;
      end;

    DT_SCE_EXPORT_LIB,
    DT_SCE_IMPORT_LIB:
      begin
       lib_entry:=Lib_Entry_new(dt_ent^.d_un.d_val,ord(dt_ent^.d_tag=DT_SCE_IMPORT_LIB));
       TAILQ_INSERT_TAIL(@obj^.lib_table,lib_entry,@lib_entry^.link);
      end;

    DT_SCE_EXPORT_LIB_ATTR,
    DT_SCE_IMPORT_LIB_ATTR:
      begin
       dval:=dt_ent^.d_un.d_val;

       lib_entry:=TAILQ_FIRST(@obj^.lib_table);
       while (lib_entry<>nil) do
       begin
        if (TLibraryAttr(dval).id=lib_entry^.dval.id) then
        begin
         Break;
        end;
        lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
       end;

       if (lib_entry=nil) then
       begin
        Writeln(StdErr,'digest_dynamic:','unknown ID found in DT_SCE_*_LIB_ATTR entry ',TLibraryAttr(dval).id);
        Exit(EINVAL);
       end;

       lib_entry^.attr:=TLibraryAttr(dval).attr;
      end;

    DT_FLAGS_1:
      begin
       dval:=dt_ent^.d_un.d_val;

       if ((dval and DF_1_BIND_NOW)<>0) then
       begin
        Writeln(StdErr,'digest_dynamic:','DF_1_BIND_NOW is obsolete.');
        Exit(EINVAL);
       end;

       if ((dval and DF_1_NODELETE)<>0) then
       begin
        Writeln(StdErr,'digest_dynamic:','DF_1_NODELETE is obsolete.');
        Exit(EINVAL);
       end;

       if ((dval and DF_1_LOADFLTR)<>0) then
       begin
        Writeln(StdErr,'digest_dynamic:','DF_1_LOADFLTR is obsolete.');
        Exit(EINVAL);
       end;

       if ((dval and DF_1_NOOPEN)<>0) then
       begin
        Writeln(StdErr,'digest_dynamic:','DF_1_NOOPEN is obsolete.');
        Exit(EINVAL);
       end;
      end;

    else
      begin
       Writeln(StdErr,'digest_dynamic:','Unsupported DT tag 0x',HexStr(dt_ent^.d_tag,8),' found in ',obj^.lib_path);
       Exit(ENOEXEC);
      end;

   end; //case

   Inc(dt_ent);
  end; //for

 end;

 addr:=obj^.rel_data^.sce_dynlib_addr;

 if (dt_fingerprint=-1) then
 begin
  if (addr<>nil) then
  begin
   Move(addr^,obj^.fingerprint,20);
  end;
 end else
 begin
  if (addr<>nil) then
  begin
   Move((addr+dt_fingerprint)^,obj^.fingerprint,20);
  end;
 end;

 if (obj^.lib_path<>nil) then
 begin
  obj^.lib_dirname:=AllocMem(strlen(obj^.lib_path)+1);
  //
  Result:=rtld_dirname(obj^.lib_path,obj^.lib_dirname);
  if (Result<>0) then
  begin
   Exit(EINVAL);
  end;
 end;

 if (dyn_soname<>nil) then
 begin
  str:=obj_get_str(obj,dyn_soname^.d_un.d_val);

  if (str=nil) then
  begin
   Writeln(StdErr,'digest_dynamic:',{$INCLUDE %LINE%});
   Exit(EINVAL);
  end;

  object_add_name(obj,str);
 end;

end;

procedure dynlibs_add_obj(obj:p_lib_info);
begin
 TAILQ_INSERT_TAIL(@dynlibs_info.obj_list,obj,@obj^.link);
 Inc(dynlibs_info.obj_count);
end;

procedure init_relo_bits(obj:p_lib_info);
var
 count:Integer;
begin
 if (obj^.rel_data=nil) then
 begin
  count:=0;
 end else
 begin
  count:=(obj^.rel_data^.pltrela_size div sizeof(elf64_rela))+(obj^.rel_data^.rela_size div sizeof(elf64_rela));
 end;

 if (count=0) then
 begin
  obj^.relo_bits:=nil;
 end else
 begin
  obj^.relo_bits:=AllocMem((count+7) div 8);
 end;

end;

function check_relo_bits(obj:p_lib_info;i:Integer):Boolean;
begin
 if (obj^.relo_bits=nil) then Exit(False);

 Result:=((obj^.relo_bits[i shr 3] shr (i and 7)) and 1)<>0;
end;

procedure set_relo_bits(obj:p_lib_info;i:Integer);
begin
 if (obj^.relo_bits=nil) then Exit;

 obj^.relo_bits[i shr 3]:=obj^.relo_bits[i shr 3] or (1 shl (i and 7))
end;

procedure reset_relo_bits(obj:p_lib_info;i:Integer);
begin
 if (obj^.relo_bits=nil) then Exit;

 obj^.relo_bits[i shr 3]:=obj^.relo_bits[i shr 3] and (not (1 shl (i and 7)))
end;

function dynlib_load_sections(imgp:p_image_params;new:p_lib_info;phdr:p_elf64_phdr;count:Integer;delta:QWORD):Integer;
var
 i:Integer;

 hdr:p_elf64_hdr;

 total_size:QWORD;
 data_size :QWORD;
 data_addr :QWORD;
 text_addr :QWORD;
 text_size :QWORD;

 p_memsz   :QWORD;
 p_vaddr   :QWORD;
 p_filesz  :QWORD;
 p_offset  :QWORD;

 addr:QWORD;
 size:QWORD;

 p_type   :Elf64_Word;
 p_flags  :Byte;
 _2mb_mode:Boolean;
 used_mode_2m:Boolean;

 fname:RawByteString;
begin
 Result:=0;

 fname:=ExtractFileName(imgp^.execpath);

 total_size:=0;
 data_size :=0;
 data_addr :=0;
 text_addr :=0;
 text_size :=0;

 if (budget_ptype_caller=0) then
 begin
  _2mb_mode:=((g_mode_2mb or 1)=3);
 end else
 begin
  _2mb_mode:=False;
 end;

 if (count<>0) then
 For i:=0 to count-1 do
 begin
  p_type :=phdr^.p_type;
  p_memsz:=phdr^.p_memsz;

  if ((p_type=PT_SCE_RELRO) or (p_type=PT_LOAD)) and (p_memsz<>0) then
  begin

   p_flags:=VM_PROT_READ or VM_PROT_WRITE;
   if (p_type<>PT_SCE_RELRO) then
   begin
    p_flags:=convert_prot(phdr^.p_flags);
   end;

   p_vaddr:=delta+phdr^.p_vaddr;

   p_filesz:=phdr^.p_filesz;
   p_offset:=phdr^.p_offset;

   if (p_type=PT_SCE_RELRO) then
   begin

    if (_2mb_mode=false) then
    begin
     used_mode_2m:=false;
    end else
    begin
     used_mode_2m:=is_used_mode_2mb(phdr,1,budget_ptype_caller);
    end;

    Result:=self_load_section(imgp,
                              i,
                              p_vaddr,
                              p_offset,
                              p_memsz,
                              p_filesz,
                              p_flags,
                              used_mode_2m,
                              pchar(fname));

   end else
   begin

    if (_2mb_mode=false) then
    begin
     used_mode_2m:=false;
    end else
    begin
     used_mode_2m:=is_used_mode_2mb(phdr,1,budget_ptype_caller);
    end;

    Result:=self_load_section(imgp,
                              i,
                              p_vaddr,
                              p_offset,
                              p_memsz,
                              p_filesz,
                              p_flags,
                              used_mode_2m,
                              pchar(fname));
   end;
   if (Result<>0) then Exit;

   addr:=(p_vaddr and QWORD($ffffffffffffc000));
   size:=((p_vaddr and $3fff) + $3fff + phdr^.p_memsz) and QWORD($ffffffffffffc000);

   if (p_type=PT_SCE_RELRO) then
   begin
    imgp^.relro_addr:=Pointer(addr);
    imgp^.relro_size:=size;
   end else
   if ((phdr^.p_flags and PF_X)<>0) and (text_size < size) then
   begin
    text_size:=size;
    text_addr:=addr;
   end else
   begin
    data_size:=size;
    data_addr:=addr;
   end;

   total_size:=total_size+size;
  end;

  Inc(phdr);
 end;

 if (data_addr=0) and (data_size=0) then
 begin
  data_addr:=text_addr;
  data_size:=text_size;
 end;

 if (imgp^.relro_addr<>nil) and (imgp^.relro_size<>0) then
 begin
  Result:=vm_map_protect(@g_vmspace.vm_map,QWORD(imgp^.relro_addr),QWORD(imgp^.reloc_base)+imgp^.relro_size,VM_PROT_READ,False);
  Result:=vm_mmap_to_errno(Result);
  if (Result<>0) then Exit;
 end;

 addr:=imgp^.min_addr;

 imgp^.dyn_vaddr    :=Pointer(imgp^.dyn_vaddr    )+addr;
 imgp^.entry_addr   :=Pointer(imgp^.entry_addr   )+addr;
 imgp^.tls_init_addr:=Pointer(imgp^.tls_init_addr)+addr;

 if (imgp^.eh_frame_hdr_addr<>nil) then
 begin
  imgp^.eh_frame_hdr_addr:=Pointer(imgp^.eh_frame_hdr_addr)+addr;
 end;

 if (imgp^.module_param_addr<>nil) then
 begin
  imgp^.module_param_addr:=Pointer(imgp^.module_param_addr)+addr;
 end;

 if (elf64_get_eh_frame_info(new^.eh_frame_hdr_addr,
                             new^.eh_frame_hdr_size,
                             delta,
                             text_size + text_addr,
                             @new^.eh_frame_addr,
                             @new^.eh_frame_size)<>0) then
 begin
  new^.eh_frame_addr:=nil;
  new^.eh_frame_size:=0;
 end;

 hdr:=imgp^.image_header;

 new^.map_base    :=Pointer(addr);
 new^.map_size    :=imgp^.max_addr - imgp^.min_addr;
 new^.text_size   :=text_size;
 new^.data_addr   :=Pointer(data_addr);
 new^.data_size   :=data_size;
 new^.relocbase   :=Pointer(addr);
 new^.entry_addr  :=Pointer(delta + hdr^.e_entry);
 new^.module_param:=imgp^.module_param_addr;
 new^.relro_addr  :=imgp^.relro_addr;
 new^.relro_size  :=imgp^.relro_size;

end;

function self_load_shared_object(path:pchar;new:p_lib_info;wire:Integer):Integer;
label
 _fail_dealloc;
var
 nd:t_nameidata;
 error:Integer;
 budget:Integer;

 image_params:t_image_params;
 imgp:p_image_params;
 attr:t_vattr;
 vp:p_vnode;

 hdr :p_elf64_hdr;
 phdr:p_elf64_phdr;

 addr,delta:QWORD;
begin
 Result:=-1;
 if (path=nil) then Exit;

 error:=0;
 imgp:=@image_params;
 image_params:=Default(t_image_params);

 attr:=Default(t_vattr);
 imgp^.attr:=@attr;
 imgp^.execpath:=path;

 NDINIT(@nd, LOOKUP, ISOPEN or LOCKLEAF or FOLLOW or SAVENAME or MPSAFE or AUDITVNODE1, UIO_SYSSPACE, path, curkthread);

 error:=nd_namei(@nd);
 if (error<>0) then
 begin
  if (error<>EACCES) then Exit(error);
  Writeln(StdErr,'self_load_shared_object:','namei() error (path=',path,')');
  Exit(error);
 end;

 vp:=nd.ni_vp;
 imgp^.vp:=vp;

 { Get file attributes }
 error:=VOP_GETATTR(vp, imgp^.attr);
 if (error<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Exit(error);
 end;

 if ((p_mount(vp^.v_mount)^.mnt_flag and MNT_NOEXEC)<>0) or
    ((attr.va_mode and (S_IXUSR or S_IXGRP or S_IXOTH))=0) or
    (attr.va_type<>VREG) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Writeln(StdErr,'self_load_shared_object:','mount flag / attribute error (path=',path,')');
  Exit(EACCES);
 end;

 if (attr.va_size<32) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Exit(ENOEXEC);
 end;

 error:=VOP_ACCESS(vp, VEXEC);
 if (error<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Writeln(StdErr,'self_load_shared_object:','VOP_ACCESS() error (path=',path,')');
  Exit(error);
 end;

 if (vp^.v_writecount<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Exit(ETXTBSY);
 end;

 error:=VOP_OPEN(vp, FREAD, nil);
 if (error<>0) then
 begin
  NDFREE(@nd, NDF_ONLY_PNBUF);
  Writeln(StdErr,'self_load_shared_object:','VOP_OPEN() error (path=',path,')');
  Exit(error);
 end;

 error:=rtld_load_self(imgp);
 if (error<>0) then goto _fail_dealloc;

 hdr:=imgp^.image_header;

 if (hdr=nil) then Exit(EINVAL);

 Case hdr^.e_type of
  ET_SCE_DYNAMIC:
  else
   begin
    Writeln(StdErr,'self_load_shared_object:',imgp^.execpath,' Unsupported ELF e_type:0x',HexStr(hdr^.e_type,4));
    error:=ENOEXEC;
    goto _fail_dealloc;
   end;
 end;

 budget:=budget_ptype_caller;

 if is_system_path(path) then
 begin
  if not is_libc_or_fios(path) then
  begin
   budget:=2;
  end;
 end;

 imgp^.hdr_e_type:=hdr^.e_type;

 phdr:=get_elf_phdr(hdr);

 error:=scan_phdr(imgp,phdr,hdr^.e_phnum);
 if (error<>0) then
 begin
  Writeln(StdErr,'self_load_shared_object:','found illegal segment header in ',imgp^.execpath);
  goto _fail_dealloc;
 end;

 if (imgp^.dyn_exist=0) then
 begin
  Writeln(StdErr,'self_load_shared_object:','illegal ELF file image',imgp^.execpath);
  error:=ENOEXEC;
  goto _fail_dealloc;
 end;

 rtld_load_auth(imgp);

 new^.tls_size         :=imgp^.tls_size;
 new^.tls_align        :=imgp^.tls_align;
 new^.tls_init_size    :=imgp^.tls_init_size;
 new^.tls_init_addr    :=imgp^.tls_init_addr;
 new^.eh_frame_hdr_addr:=imgp^.eh_frame_hdr_addr;
 new^.eh_frame_hdr_size:=imgp^.eh_frame_hdr_size;

 error:=scan_dyn_offset(imgp,phdr,hdr^.e_phnum);
 if (error<>0) then
 begin
  goto _fail_dealloc;
 end;

 addr:=ET_DYN_LOAD_ADDR_USR;
 if (budget=2) then
 begin
  addr:=ET_DYN_LOAD_ADDR_SYS;
 end;

 error:=rtld_mmap(@addr,imgp^.max_addr-imgp^.min_addr);
 if (error<>0) then
 begin
  Writeln(StdErr,'self_load_shared_object:','failed to allocate VA for ',imgp^.execpath);
  goto _fail_dealloc;
 end;

 delta:=addr-imgp^.min_addr;
 imgp^.min_addr:=addr;
 imgp^.max_addr:=imgp^.max_addr+delta;

 new^.tls_init_addr    :=new^.tls_init_addr    +delta;
 new^.eh_frame_hdr_addr:=new^.eh_frame_hdr_addr+delta;

 error:=dynlib_load_sections(imgp,new,phdr,hdr^.e_phnum,delta);
 if (error<>0) then
 begin
  goto _fail_dealloc;
 end;

 if (budget=2) then
 begin
  new^.is_system:=1;
 end;

 error:=acquire_per_file_info_obj(imgp,new);
 if (error<>0) then
 begin
  Writeln(StdErr,'self_load_shared_object:','acquire_per_file_info_obj()=',error);
  goto _fail_dealloc;
 end;

 _fail_dealloc:
  rtld_free_self(imgp);
  NDFREE(@nd, NDF_ONLY_PNBUF);
  VOP_CLOSE(vp, FREAD);
  vput(vp);

 Exit(error);
end;

function change_relro_protection(obj:p_lib_info;prot:Integer):Integer;
var
 map:vm_map_t;
 addr:Pointer;
 size:QWORD;
begin
 map:=@g_vmspace.vm_map;

 addr:=obj^.relro_addr;
 size:=obj^.relro_size;

 if (addr<>nil) and (size<>0) then
 begin
  Result:=vm_map_protect(map,QWORD(addr),QWORD(addr)+size,prot,False);
  if (Result<>0) then
  begin
   Writeln(StdErr,'change_relro_protection:','failed to make RELRO segment writable.');
  end;
 end;
end;

function change_relro_protection_all(prot:Integer):Integer;
var
 obj:p_lib_info;
begin
 Result:=0;
 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  Result:=change_relro_protection(obj,prot);
  if (Result<>0) then Exit;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;
end;

function relocate_text_or_data_segment(obj:p_lib_info;src,dst:Pointer;size:QWORD):Integer;
var
 map:vm_map_t;
begin
 if (obj^.textrel=0) or
    (obj^.map_base > dst) or
    ((obj^.map_base + obj^.text_size) < (dst + size)) then
 begin
  Result:=copyout(src,dst,size);
 end else
 if (p_proc.p_sdk_version < $1700000) then
 begin
  map:=@g_vmspace.vm_map;
  //
  vm_map_lock(map);
  //
  Result:=change_relro_protection(obj,VM_PROT_RW);
  if (Result<>0) then Exit;

  Result:=copyout(src,dst,size);

  change_relro_protection(obj,VM_PROT_READ);
  //
  vm_map_unlock(map);
 end else
 begin
  Writeln(StdErr,'relocate_text_or_data_segment:','text relocation is prohibited.');
  Exit(ENOEXEC);
 end;
end;

procedure donelist_init(var dlp:t_DoneList);
begin
 SetLength(dlp.objs,dynlibs_info.obj_count);
 dlp.num_used:=0;
end;

function donelist_check(var dlp:t_DoneList;obj:p_lib_info):Boolean;
var
 i:DWORD;
begin
 if (dlp.num_used<>0) then
 For i:=0 to dlp.num_used-1 do
 begin
  if (dlp.objs[i]=obj) then Exit(True);
 end;

 if (dlp.num_used < Length(dlp.objs)) then
 begin
  dlp.objs[dlp.num_used]:=obj;
  Inc(dlp.num_used);
 end;

 Result:=False;
end;

procedure init_dag(root:p_lib_info);
label
 _continue;
var
 needed:p_Needed_Entry;
 elm:p_Objlist_Entry;
 donelist:t_DoneList;
begin
 if (root^.dag_inited<>0) then Exit;

 donelist:=Default(t_DoneList);
 donelist_init(donelist);

 // Root object belongs to own DAG.
 objlist_push_tail(root^.dldags,     root);
 objlist_push_tail(root^.dagmembers, root);
 donelist_check(donelist, root);

 {
 * Add dependencies of root object to DAG in breadth order
 * by exploiting the fact that each new object get added
 * to the tail of the dagmembers list.
 }
 elm:=TAILQ_FIRST(@root^.dagmembers);
 while (elm<>nil) do
 begin

  needed:=TAILQ_FIRST(@elm^.obj^.needed);
  while (needed<>nil) do
  begin
   if (needed^.obj=nil) then
   begin
    goto _continue;
   end;

   if donelist_check(donelist,needed^.obj) then
   begin
    goto _continue;
   end;

   objlist_push_tail(needed^.obj^.dldags, root);
   objlist_push_tail(root^.dagmembers, needed^.obj);

   _continue:
   needed:=TAILQ_NEXT(needed,@needed^.link);
  end;

  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;

 root^.dag_inited:=1;
end;

procedure ref_dag(root:p_lib_info);
var
 elm:p_Objlist_Entry;
begin
 Assert(root^.dag_inited<>0,'DAG is not initialized');

 elm:=TAILQ_FIRST(@root^.dagmembers);
 while (elm<>nil) do
 begin
  Inc(elm^.obj^.ref_count);
  //
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;
end;

procedure unref_dag(root:p_lib_info);
var
 elm:p_Objlist_Entry;
begin
 Assert(root^.dag_inited<>0,'DAG is not initialized');

 elm:=TAILQ_FIRST(@root^.dagmembers);
 while (elm<>nil) do
 begin
  Inc(elm^.obj^.ref_count);
  //
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;
end;

function dynlib_initialize_pltgot_each(obj:p_lib_info):Integer;
var
 addr:Pointer;
 entry:p_elf64_rela;

 kaddr:QWORD;

 i,count,err:Integer;
begin
 Result:=0;

 if (obj^.init_plt<>0) then Exit;

 Result:=change_relro_protection(obj,VM_PROT_RW);
 if (Result<>0) then Exit;

 entry:=obj^.rel_data^.pltrela_addr;
 count:=obj^.rel_data^.pltrela_size div SizeOf(elf64_rela);

 if (entry<>nil) and (count<>0) then
  For i:=0 to count-1 do
  begin
   kaddr:=i or QWORD($effffffe00000000);

   addr:=Pointer(obj^.relocbase) + entry^.r_offset;

   if (
       (addr < obj^.data_addr) or
       ((obj^.data_addr + obj^.data_size) < (addr + 8))
      ) and
      ( (obj^.relro_addr=nil) or
        (obj^.relro_addr>addr) or
        (obj^.relro_size=0) or
        ((obj^.relro_size + obj^.relro_addr) < (addr + 8))
      ) then
   begin
    Result:=ENOEXEC;
    Break;
   end;

   err:=copyout(@kaddr,addr,SizeOf(Pointer));
   if (err<>0) then
   begin
    Writeln(StdErr,'relro:0x',HexStr(obj^.relro_addr),'..0x',HexStr(obj^.relro_addr+obj^.relro_size));
    Writeln(StdErr,'dynlib_initialize_pltgot_each:','ERROR in .pltrela: where=0x',HexStr(addr));
    Result:=ENOEXEC;
    Break;
   end;

   Inc(entry);
  end;

 err:=change_relro_protection(obj,VM_PROT_READ);

 obj^.init_plt:=1;
end;

function do_load_object(path:pchar;flags:DWORD;var err:Integer):p_lib_info;
label
 _inc_max,
 _error;
var
 fname:RawByteString;
 new:p_lib_info;
 obj:p_lib_info;
 i:Integer;
 tls_max:Integer;
begin
 Result:=nil;

 new:=obj_new();

 err:=self_load_shared_object(path,new,ord((flags and $20)<>0));
 if (err<>0) then
 begin
  goto _error;
 end;

 fname:=ExtractFileName(path);
 object_add_name(new,pchar(fname));

 obj_set_lib_path(new,path);

 if (new^.tls_size=0) then
 begin
  i:=0;
 end else
 begin
  dynlibs_info.tls_count:=dynlibs_info.tls_count + 1;
  tls_max:=dynlibs_info.tls_max;

  if (tls_max<1) then
  begin
   _inc_max:
   i:=tls_max+1;
   dynlibs_info.tls_max:=i;
  end else
  begin
   i:=1;
   obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
   while (obj<>nil) do
   begin
    while (obj^.tls_index=i) do
    begin
     i:=i+1;
     obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
     if (tls_max < i) then
     begin
      goto _inc_max;
     end;
    end;
    obj:=TAILQ_NEXT(obj,@obj^.link);
   end;
  end;
 end;

 new^.tls_index:=i;

 err:=digest_dynamic(new);
 if (err<>0) then
 begin
  Writeln(StdErr,'do_load_object:','digest_dynamic() failed rv=',err);
  goto _error;
 end;

 err:=dynlib_initialize_pltgot_each(new);
 if (err<>0) then
 begin
  Writeln(StdErr,'do_load_object:','dynlib_initialize_pltgot_each() failed rv=',err);
  goto _error;
 end;

 if (new^.textrel<>0) then
 begin
  Writeln(StdErr,'do_load_object:',new^.lib_path,' has impure text');
  err:=EINVAL;
  goto _error;
 end;

 init_relo_bits(obj);
 dynlibs_add_obj(new);
 new^.loaded:=1;
 Exit(new);

 _error:

 rtld_munmap(new^.map_base,new^.map_size);

 obj_free(new);

 Exit(nil);
end;

procedure unlink_object(root:p_lib_info);
var
 elm,next:p_Objlist_Entry;
begin
 if (root^.ref_count<>0) then Exit;

 //Remove the object from the RTLD_GLOBAL list.
 objlist_remove(dynlibs_info.list_global, root);

 //Remove the object from all objects' DAG lists.
 elm:=TAILQ_FIRST(@root^.dagmembers);
 while (elm<>nil) do
 begin
  next:=TAILQ_NEXT(elm,@elm^.link);
  //
  objlist_remove(elm^.obj^.dldags, root);
  if (elm^.obj<>root) then
  begin
   unlink_object(elm^.obj);
  end;
  //
  elm:=next;
 end;
end;

procedure unload_object(root:p_lib_info);
var
 obj,next:p_lib_info;
begin
 Assert(root^.ref_count=0,'unload_object ref_count');

 {
  * Pass over the DAG removing unreferenced objects from
  * appropriate lists.
 }
 unlink_object(root);

 // Unmap all objects that are no longer referenced.

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  next:=TAILQ_NEXT(obj,@obj^.link);
  //
  rtld_munmap(obj^.map_base, obj^.map_size);

  TAILQ_REMOVE(@dynlibs_info.obj_list,obj,@obj^.link);

  Dec(dynlibs_info.obj_count);

  //dynlib_notify_event(td,lib->id,0x80);

  obj_free(obj);
  //
  obj:=next;
 end;

end;

function preload_prx_modules(path:pchar;flags:DWORD;var err:Integer):p_lib_info;
label
 _do_load;
var
 obj:p_lib_info;
 fname:RawByteString;
begin
 Result:=nil;
 err:=0;

 fname:=ExtractFileName(path);

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if object_match_name(obj,pchar(fname)) then
  begin
   Exit(obj);
  end;
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 fname:=path;

 if rtld_file_exists(pchar(fname)) then goto _do_load;

 fname:=ChangeFileExt(fname,'.sprx');
 if rtld_file_exists(pchar(fname)) then goto _do_load;

 fname:=ChangeFileExt(fname,'.prx');
 if rtld_file_exists(pchar(fname)) then goto _do_load;


 fname:=path;

 if (fname[1]<>'/') then
 begin
  fname:='/'+fname;
 end;
 fname:=p_proc.p_randomized_path+fname;

 if rtld_file_exists(pchar(fname)) then goto _do_load;

 fname:=ChangeFileExt(fname,'.sprx');
 if rtld_file_exists(pchar(fname)) then goto _do_load;

 fname:=ChangeFileExt(fname,'.prx');
 if rtld_file_exists(pchar(fname)) then goto _do_load;

 err:=ENOENT;
 Exit(nil);

 _do_load:

 Result:=do_load_object(pchar(fname),flags,err);
end;

function relocate_object(root:p_lib_info):Integer;
var
 obj:p_lib_info;
begin
 Result:=change_relro_protection_all(VM_PROT_RW);
 if (Result<>0) then Exit;

 Result:=relocate_one_object(root,ord(root^.jmpslots_done=0));

 if (Result=0) then
 begin
  obj:=dynlibs_info.libprogram;

  while (obj<>nil) do
  begin
   if (obj<>root) then
   begin
    Result:=relocate_one_object(obj,ord(root^.jmpslots_done=0));
   end;
   obj:=TAILQ_NEXT(obj,@obj^.link);
  end;
 end;

 change_relro_protection_all(VM_PROT_READ);
end;

function dynlib_load_relocate():Integer;
var
 elm:p_Objlist_Entry;
begin
 elm:=TAILQ_FIRST(@dynlibs_info.list_main);
 while (elm<>nil) do
 begin
  allocate_tls_offset(elm^.obj);
  //
  elm:=TAILQ_NEXT(elm,@elm^.link);
 end;
 //
 Result:=relocate_object(dynlibs_info.libprogram);
end;

function load_prx(path:pchar;flags:DWORD;var pobj:p_lib_info):Integer;
var
 obj:p_lib_info;
 err,pflags:Integer;
begin
 Result:=0;
 err:=0;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  if (StrLComp(obj^.lib_path,path,$400)=0) then
  begin
   Exit(0);
  end;
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 pflags:=2;
 if ((flags and $00001)<>0) then pflags:=pflags or $20; //vm_map_wire
 if ((flags and $10000)<>0) then pflags:=pflags or $40; //priv libs?

 obj:=preload_prx_modules(path,pflags,err);
 if (obj=nil) then Exit(err);

 if (objlist_find(dynlibs_info.list_global,obj)=nil) then
 begin
  objlist_push_tail(dynlibs_info.list_global,obj);
 end;

 if (obj^.ref_count=0) then
 begin
  if ((flags and $20000)<>0) then //set jmpslots_done?
  begin
   obj^.jmpslots_done:=1;
  end;

  if ((flags and $40000)<>0) then //set not_get_proc?
  begin
   obj^.not_get_proc:=1;
  end;

  init_dag(obj);
  ref_dag(obj);

  err:=relocate_object(obj);
  if (err<>0) then
  begin
   unref_dag(obj);
   if (obj^.ref_count=0) then
   begin
    unload_object(obj);
   end;
   Writeln(StdErr,'load_prx:','Fail to relocate ',path);
   Exit(err);
  end;
 end else
 begin
  ref_dag(obj);
 end;

 pobj:=obj;
 Result:=0;
end;

function dynlib_unlink_imported_symbols(root:p_lib_info;modname:pchar):Integer;
var
 obj:p_lib_info;
 lib_entry:p_Lib_Entry;
 offset:QWORD;
 str:pchar;
begin
 Result:=change_relro_protection_all(VM_PROT_RW);
 if (Result<>0) then Exit;

 if (Result=0) then
 begin
  obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
  while (obj<>nil) do
  begin
   if (obj<>root) then
   begin
    lib_entry:=TAILQ_FIRST(@obj^.mod_table);
    while (lib_entry<>nil) do
    begin
     if (lib_entry^.dval.id<>0) then //import
     begin
      offset:=lib_entry^.dval.name_offset;
      str:=obj_get_str(obj,offset);
      //
      if (modname<>nil) then
      if (StrComp(str,modname)=0) then //used module
      begin
       Result:=dynlib_unlink_imported_symbols_each(root,obj);
       //ended
       Break;
      end;
     end;
     //
     lib_entry:=TAILQ_NEXT(lib_entry,@lib_entry^.link)
    end;
   end;
   //
   obj:=TAILQ_NEXT(obj,@obj^.link);
  end;
 end;

 change_relro_protection_all(VM_PROT_READ);
end;

function unload_prx(obj:p_lib_info):Integer;
var
 modname:pchar;
begin
 if (obj^.ref_count=0) then
 begin
  Assert(False,'Invalid shared object handle');
  Exit(EINVAL);
 end;

 unref_dag(obj);

 if (obj^.ref_count<>1) then
 begin
  Exit(0);
 end;

 modname:=get_mod_name(obj,0); //export=0

 if (modname<>nil) then
 begin
  Result:=dynlib_unlink_imported_symbols(obj,modname);
 end;

 unload_object(obj);

 Result:=0;
end;

function alloc_obj_id(obj:p_lib_info):Boolean;
var
 key:Integer;
begin
 Result:=False;
 if (obj^.id>0) then Exit(True);

 obj^.desc.free:=nil;
 obj^.objt:=NAMED_DYNL;
 obj^.name:='';

 key:=-1;
 if id_name_new(@named_table,obj,@key) then
 begin
  obj^.id:=(key+1);
  id_release(obj);
  Result:=True;
 end;
end;

function free_obj_id(id:Integer):Boolean;
var
 key:Integer;
begin
 if (id<=0) then Exit(True);

 key:=id-1;
 Result:=id_name_del(@named_table,key,NAMED_DYNL,nil);
end;

function find_obj_id(id:Integer):p_lib_info;
var
 key:Integer;
begin
 key:=id-1;

 Result:=id_name_get(@named_table,key,NAMED_DYNL);
 if (Result=nil) then Exit;

 id_release(Result);
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

function dynlib_load_needed_shared_objects():Integer;
var
 obj:p_lib_info;
 needed:PPointer;
 i:Integer;
begin
 obj:=dynlibs_info.libprogram;

 if (obj<>nil) then
 begin
  i:=0;
  repeat
   needed:=@obj^.needed.tqh_first;
   obj:=TAILQ_NEXT(obj,@obj^.link);
   i:=(i+1)-ord(needed^=nil);
  until (obj=nil);

  if (i<0) then
  begin
   Writeln(StdErr,'dynlib_load_needed_shared_objects:','load_needed_objects() fails');
   Exit(EINVAL);
  end;
 end;

 obj:=TAILQ_FIRST(@dynlibs_info.obj_list);
 while (obj<>nil) do
 begin
  init_dag(obj);
  ref_dag(obj);
  //
  obj:=TAILQ_NEXT(obj,@obj^.link);
 end;

 Result:=0;
end;


end.

