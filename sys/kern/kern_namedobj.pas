unit kern_namedobj;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 kern_named_id;

type
 p_namedobj=^t_namedobj;
 t_namedobj=packed object(t_id_named_desc)
  link:TAILQ_ENTRY;
  objp:Pointer;
 end;

var
 named_table:t_id_desc_table;

procedure named_table_init; //SYSINIT

function  get_obj_name(objp:Pointer;objt:Integer;name:PChar):Boolean;

function  sys_namedobj_create(name:PChar;objp:Pointer;objt:Integer):Integer;
function  sys_namedobj_delete(id,objt:Integer):Integer;

const
 NAMED_OBJT=$1000;
 NAMED_DYNL=$2000;

implementation

uses
 errno,
 systm,
 kern_thr,
 kern_rwlock;

var
 namedobj_list:TAILQ_HEAD=(tqh_first:nil;tqh_last:@namedobj_list.tqh_first);
 namedobj_lock:Pointer;

procedure named_table_init;
begin
 id_table_init(@named_table,1);
end;

procedure namedobj_add(data:p_namedobj);
begin
 rw_wlock(namedobj_lock);
 TAILQ_INSERT_TAIL(@namedobj_list,data,@data^.link);
 rw_wunlock(namedobj_lock);
end;

procedure namedobj_rem(data:p_namedobj);
begin
 rw_wlock(namedobj_lock);
 TAILQ_REMOVE(@namedobj_list,data,@data^.link);
 rw_wunlock(namedobj_lock);
end;

procedure namedobj_free(data:pointer);
begin
 namedobj_rem(data);
 FreeMem(data);
end;

function get_obj_name(objp:Pointer;objt:Integer;name:PChar):Boolean;
var
 entry:p_namedobj;
begin
 Result:=False;
 objt:=Word(objt) or NAMED_OBJT;

 rw_rlock(namedobj_lock);

 entry:=TAILQ_FIRST(@namedobj_list);
 while (entry<>nil) do
 begin
  if (entry^.objp=objp) and
     (entry^.objt=objt) then
  begin
   Result:=True;

   copystr(entry^.name,name,SizeOf(entry^.name),nil);

   Break;
  end;

  entry:=TAILQ_NEXT(entry,@entry^.link);
 end;

 rw_runlock(namedobj_lock);
end;

function sys_namedobj_create(name:PChar;objp:Pointer;objt:Integer):Integer;
var
 td:p_kthread;
 _name:t_id_name;
 obj:p_namedobj;
 key:Integer;
begin
 if (name=nil) then Exit(EINVAL);

 td:=curkthread;
 if (td=nil) then Exit(-1);

 _name:=Default(t_id_name);

 Result:=copyinstr(name,@_name,SizeOf(t_id_name),nil);
 if (Result<>0) then Exit;

 obj:=AllocMem(SizeOf(t_namedobj));
 if (obj=nil) then Exit(ENOMEM);

 obj^.desc.free:=@namedobj_free;
 obj^.objt:=Word(objt) or NAMED_OBJT;
 obj^.name:=name;
 obj^.objp:=objp;

 namedobj_add(obj);

 if not id_name_new(@named_table,obj,@key) then
 begin
  namedobj_free(obj);
  Exit(EAGAIN);
 end;

 id_release(obj);

 td^.td_retval[0]:=key;

 Result:=0;
end;

function sys_namedobj_delete(id,objt:Integer):Integer;
var
 obj:p_namedobj;
begin
 Result:=ESRCH;

 if not id_name_del(@named_table,id,Word(objt) or NAMED_OBJT,@obj) then Exit;

 id_release(obj);

 Result:=0;
end;



end.

