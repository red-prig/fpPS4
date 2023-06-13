unit kern_named_id;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_id;

type
 t_id_name=array[0..31] of AnsiChar;

 p_id_named_desc=^t_id_named_desc;
 t_id_named_desc=packed record
  desc:t_id_desc;
  objt:Integer;
  name:t_id_name;
 end;

 p_id_desc_table=kern_id.p_id_desc_table;
 t_id_desc_table=kern_id.t_id_desc_table;

Procedure id_acqure (d:Pointer);
Procedure id_release(d:Pointer);

procedure id_table_init(t:p_id_desc_table;min:Integer;max:Integer=def_max_key);
procedure id_table_fini(t:p_id_desc_table);

function  id_name_new(t:p_id_desc_table;d:Pointer;pKey:PInteger):Boolean;
function  id_name_get(t:p_id_desc_table;Key,objt:Integer):Pointer;
function  id_name_del(t:p_id_desc_table;Key,objt:Integer;old:PPointer):Boolean;

implementation

type
 t_objt_filter=object
  objt:Integer;
  function filter(d:p_id_desc):Boolean;
 end;

function t_objt_filter.filter(d:p_id_desc):Boolean;
begin
 Result:=(objt=p_id_named_desc(d)^.objt);
end;

Procedure id_acqure(d:Pointer);
begin
 kern_id.id_acqure(d);
end;

Procedure id_release(d:Pointer);
begin
 kern_id.id_release(d);
end;

procedure id_table_init(t:p_id_desc_table;min:Integer;max:Integer=def_max_key);
begin
 kern_id.id_table_init(t,min,max);
end;

procedure id_table_fini(t:p_id_desc_table);
begin
 kern_id.id_table_fini(t);
end;

function id_name_new(t:p_id_desc_table;d:Pointer;pKey:PInteger):Boolean;
begin
 Result:=kern_id.id_new(t,d,pKey);
end;

function id_name_get(t:p_id_desc_table;Key,objt:Integer):Pointer;
begin
 Result:=kern_id.id_get(t,Key,@t_objt_filter(objt).filter);
end;

function id_name_del(t:p_id_desc_table;Key,objt:Integer;old:PPointer):Boolean;
begin
 Result:=id_del(t,Key,old,@t_objt_filter(objt).filter);
end;


end.



