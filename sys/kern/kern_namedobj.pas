unit kern_namedobj;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_named_id;

type
 p_namedobj=^t_namedobj;
 t_namedobj=packed record
  desc:t_id_named_desc;
  objp:Pointer;
 end;

var
 named_table:t_id_desc_table;

procedure named_table_init; //SYSINIT

function  sys_namedobj_create(name:PChar;objp:Pointer;objt:Integer):Integer;
function  sys_namedobj_delete(id,objt:Integer):Integer;

implementation

uses
 errno,
 systm,
 kern_thr;

const
 NAMED_OBJT=$1000;

procedure named_table_init;
begin
 id_table_init(@named_table,1);
end;

procedure namedobj_free(data:pointer);
begin
 FreeMem(data);
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

 obj^.desc.desc.free:=@namedobj_free;
 obj^.desc.objt:=Word(objt) or NAMED_OBJT;
 obj^.desc.name:=name;
 obj^.objp:=objp;

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

