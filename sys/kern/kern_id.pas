unit kern_id;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 hamt,
 kern_rwlock;

Const
 def_max_key=$7FFFFFFF;

type
 p_id_desc=^t_id_desc;
 t_id_desc=packed record
  refs:QWORD;
  free:procedure(data:pointer);
 end;

 p_id_desc_table=^t_id_desc_table;
 t_id_desc_table=packed record
  FLock  :Pointer;
  FHAMT  :TSTUB_HAMT32;
  FCount :Integer;
  FPos   :Integer;
  min_key:Integer;
  max_key:Integer;
 end;

Procedure id_acqure (d:p_id_desc);
Procedure id_release(d:p_id_desc);

procedure id_table_init(t:p_id_desc_table;min:Integer);
procedure id_table_fini(t:p_id_desc_table);

function  id_new(t:p_id_desc_table;d:p_id_desc;pKey:PInteger):Boolean;
function  id_set(t:p_id_desc_table;d:p_id_desc;Key:Integer;old:PPointer):Boolean;
function  id_get(t:p_id_desc_table;Key:Integer):p_id_desc;
function  id_del(t:p_id_desc_table;Key:Integer;old:PPointer):Boolean;

function  id_get_spaces(t:p_id_desc_table):Integer;

implementation

Procedure id_acqure(d:p_id_desc);
begin
 if (d=nil) then Exit;
 System.InterlockedIncrement64(d^.refs);
end;

Procedure id_release(d:p_id_desc);
begin
 if (d=nil) then Exit;
 if (System.InterlockedDecrement64(d^.refs)=0) then
 if (d^.free<>nil) then
 begin
  d^.free(d);
 end;
end;

procedure _free_data_cb(data,userdata:Pointer); register;
begin
 if (data<>nil) then
 begin
  id_release(data);
 end;
end;

procedure id_table_init(t:p_id_desc_table;min:Integer);
begin
 if (t=nil) then Exit;
 FillChar(t^,SizeOf(t_id_desc_table),0);
 t^.min_key:=min;
 t^.max_key:=def_max_key;
 t^.FPos   :=min;
end;

procedure id_table_fini(t:p_id_desc_table);
begin
 if (t=nil) then Exit;
 HAMT_clear32(@t^.FHAMT,@_free_data_cb,nil);
end;

function id_new(t:p_id_desc_table;d:p_id_desc;pKey:PInteger):Boolean;
Label
 _data,_exit;
Var
 i,m:Integer;
 data:PPointer;
begin
 Result:=False;
 if (t=nil) or (pKey=nil) then Exit;

 rw_wlock(t^.FLock);

 m:=(t^.max_key-t^.min_key);
 if (t^.FCount>=m+1) then goto _exit;

 if (t^.FPos<t^.min_key) or
    (t^.FPos>t^.max_key) then
 begin
  t^.FPos:=t^.min_key;
 end;

 if (t^.FCount=0) then
 begin
  pKey^:=t^.FPos;
  Inc(t^.FPos);

  data:=HAMT_insert32(@t^.FHAMT,pKey^,Pointer(d));

  if (data=nil) then goto _exit;
  if (data^<>Pointer(d)) then goto _exit;
 end else
 begin
  For i:=0 to m do
  begin
   pKey^:=t^.FPos;
   Inc(t^.FPos);

   if (t^.FPos>t^.max_key) then
   begin
    t^.FPos:=t^.min_key;
   end;

   data:=HAMT_insert32(@t^.FHAMT,pKey^,Pointer(d));

   if (data=nil) then goto _exit;
   if (data^=Pointer(d)) then goto _data;
  end;
  goto _exit;
 end;

 _data:
  Inc(t^.FCount);
  id_acqure(d);
  id_acqure(d);

  Result:=True;

 _exit:
  rw_wunlock(t^.FLock);
end;

function id_set(t:p_id_desc_table;d:p_id_desc;Key:Integer;old:PPointer):Boolean;
Label
 _exit;
Var
 data:PPointer;
begin
 Result:=False;
 if (t=nil) or (Key<t^.min_key) or (Key>t^.max_key) then Exit;

 rw_wlock(t^.FLock);

 data:=HAMT_insert32(@t^.FHAMT,Key,Pointer(d));

 if (data=nil) then goto _exit;

 if (data^<>Pointer(d)) then
 begin
  if (old<>nil) then
  begin
   old^:=data^;
   data^:=Pointer(d);
  end else
  begin
   goto _exit;
  end;
 end else
 if (old<>nil) then
 begin
  old^:=nil;
 end;

 id_acqure(d);

 Result:=True;

 _exit:
  rw_wunlock(t^.FLock);
end;

function id_get(t:p_id_desc_table;Key:Integer):p_id_desc;
Var
 data:PPointer;
Label
 _exit;
begin
 Result:=nil;
 if (t=nil) or (Key<t^.min_key) or (Key>t^.max_key) then Exit;

 rw_rlock(t^.FLock);

 data:=HAMT_search32(@t^.FHAMT,Key);
 if (data=nil) then Goto _exit;

 Pointer(Result):=data^;

 id_acqure(Result);

 _exit:
  rw_runlock(t^.FLock);
end;

function id_del(t:p_id_desc_table;Key:Integer;old:PPointer):Boolean;
Var
 d,rel:p_id_desc;
begin
 Result:=False;
 if (t=nil) or (Key<t^.min_key) or (Key>t^.max_key) then Exit;

 d:=nil;
 rel:=nil;
 rw_wlock(t^.FLock);

 if HAMT_delete32(@t^.FHAMT,Key,@d) then
 begin
  if (old<>nil) then
  begin
   old^:=d;
  end else
  begin
   rel:=d;
  end;
  Dec(t^.FCount);
  Result:=True;
 end else
 if (old<>nil) then
 begin
  old^:=nil;
 end;

 rw_wunlock(t^.FLock);
 //
 id_release(rel);
end;

function id_get_spaces(t:p_id_desc_table):Integer;
var
 i,last:Integer;
begin
 Result:=0;
 if (t=nil) then Exit;

 rw_rlock(t^.FLock);

 last:=t^.FPos;
 For i:=0 to last do
 begin
  if (HAMT_search32(@t^.FHAMT,i)=nil) then
  begin
   Inc(Result);
  end;
 end;

 rw_runlock(t^.FLock);
end;

end.

