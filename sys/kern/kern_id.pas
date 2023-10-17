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
  free:procedure(data:pointer);
  refs:Integer;
 end;

 p_id_desc_table=^t_id_desc_table;
 t_id_desc_table=packed record
  FLock  :Pointer;      //table lock
  FHAMT  :TSTUB_HAMT32; //hamt by id
  FCount :Integer;      //count alloc id
  FSpace :Integer;      //count free id
  FLast  :Integer;      //last free id
  FPos   :Integer;      //max uses pos
  min_key:Integer;      //min key [min_key,max_key)
  max_key:Integer;      //max key [min_key,max_key)
 end;

 t_filter_cb=function(d:p_id_desc):Boolean of object;

Procedure id_acqure (d:p_id_desc);
Procedure id_release(d:p_id_desc);

procedure id_table_init(t:p_id_desc_table;min:Integer;max:Integer=def_max_key);
procedure id_table_fini(t:p_id_desc_table);

function  id_new(t:p_id_desc_table;d:p_id_desc;pKey:PInteger):Boolean;
function  id_set(t:p_id_desc_table;d:p_id_desc;Key:Integer;old:PPointer):Boolean;
function  id_get(t:p_id_desc_table;Key:Integer;cb:t_filter_cb=nil):p_id_desc;
function  id_del(t:p_id_desc_table;Key:Integer;old:PPointer;cb:t_filter_cb=nil):Boolean;

implementation

Procedure id_acqure(d:p_id_desc);
begin
 if (d=nil) then Exit;
 System.InterlockedIncrement(d^.refs);
end;

Procedure id_release(d:p_id_desc);
begin
 if (d=nil) then Exit;
 Assert(d^.refs>0);
 if (System.InterlockedDecrement(d^.refs)=0) then
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

procedure id_table_init(t:p_id_desc_table;min:Integer;max:Integer=def_max_key);
begin
 if (t=nil) then Exit;
 FillChar(t^,SizeOf(t_id_desc_table),0);
 if (min>max) then
 begin
  t^.min_key:=max;
  t^.max_key:=min;
 end else
 begin
  t^.min_key:=min;
  t^.max_key:=max;
 end;
 t^.FPos :=t^.min_key;
 t^.FLast:=-1;
end;

procedure id_table_fini(t:p_id_desc_table);
begin
 if (t=nil) then Exit;
 HAMT_clear32(@t^.FHAMT,@_free_data_cb,nil);
end;

function id_new(t:p_id_desc_table;d:p_id_desc;pKey:PInteger):Boolean;
Label
 _insert,
 _exit;
Var
 i:Integer;
 data:PPointer;
begin
 Result:=False;
 if (t=nil) or (pKey=nil) then Exit;

 rw_wlock(t^.FLock);

 i:=(t^.max_key-t^.min_key);
 if (t^.FCount>=i) then goto _exit; //limit reached

 if (t^.FPos<t^.min_key) then
 begin
  t^.FPos:=t^.min_key; //fixup
 end;

 if (t^.FPos>t^.max_key) then
 begin
  t^.FPos:=t^.max_key; //fixup
 end;

 if (t^.FSpace<>0) then
 begin
  if (t^.FPos=t^.min_key) then
  begin
   t^.FSpace:=0; //fixup
  end else
  if (t^.FLast=-1) or //no last free
     ((t^.FLast<t^.FPos) and (t^.FSpace<>1)) then
  begin
   //find space id (not an efficient linear search)
   For i:=t^.min_key to t^.FPos-1 do
   begin
    if (i<>t^.FLast) then //not last free
    begin
     if (HAMT_search32(@t^.FHAMT,i)=nil) then
     begin
      //found
      if (t^.FSpace<>0) then
      begin
       Dec(t^.FSpace);
      end;
      goto _insert;
     end;
    end;
   end;
   //not found
  end;
 end;

 i:=t^.FPos;

 if (i=t^.max_key) then //limit
 begin
  //check last free
  i:=t^.FLast;
  if (i=-1) then goto _exit;
  if (t^.FSpace<>0) then
  begin
   Dec(t^.FSpace);
  end;
 end else
 begin
  Inc(t^.FPos);

  if (i=t^.FLast) and //last free?
     (t^.FPos<t^.max_key) then //bound?
  begin
   //skip last free
   i:=t^.FPos;
   Inc(t^.FSpace);
   Inc(t^.FPos);
  end;
 end;

 _insert:
  pKey^:=i;
  data:=HAMT_insert32(@t^.FHAMT,pKey^,Pointer(d));

  if (data=nil) then goto _exit; //nomem
  if (data^<>Pointer(d)) then goto _exit; //wtf

  Inc(t^.FCount);

  if (i=t^.FLast) then
  begin
   //reset last free
   t^.FLast:=-1;
  end;

  id_acqure(d); //table ref
  id_acqure(d); //ref for further use

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

 if (data=nil) then goto _exit; //nomem

 if (data^<>Pointer(d)) then //another
 begin
  if (old<>nil) then //swap
  begin
   old^:=data^;
   data^:=Pointer(d);
  end else
  begin
   goto _exit; //not set
  end;
 end else
 begin //new
  if (old<>nil) then
  begin
   old^:=nil; //not old value
  end;
  //
  Inc(t^.FCount);
  if (Key=t^.FLast) then
  begin
   //reset last free
   t^.FLast:=-1;
  end;
  if (Key<t^.FPos) then
  begin
   //in the space of use
   if (t^.FSpace<>0) then
   begin
    Dec(t^.FSpace);
   end;
  end;
 end;

 id_acqure(d);

 Result:=True;

 _exit:
  rw_wunlock(t^.FLock);
end;

function id_get(t:p_id_desc_table;Key:Integer;cb:t_filter_cb=nil):p_id_desc;
Var
 data:PPointer;
Label
 _exit;
begin
 Result:=nil;
 if (t=nil) or (Key<t^.min_key) or (Key>t^.max_key) then Exit;

 rw_rlock(t^.FLock);

 data:=HAMT_search32(@t^.FHAMT,Key);
 if (data=nil) then Goto _exit; //not found

 Pointer(Result):=data^;

 if (cb<>nil) then
 begin
  if not cb(Result) then Goto _exit; //filtred
 end;

 id_acqure(Result); //ref for further use

 _exit:
  rw_runlock(t^.FLock);
end;

function id_del(t:p_id_desc_table;Key:Integer;old:PPointer;cb:t_filter_cb=nil):Boolean;
Var
 data:PPointer;
 d,rel:p_id_desc;
 p:Integer;
Label
 _exit;
begin
 Result:=False;
 if (t=nil) or (Key<t^.min_key) or (Key>t^.max_key) then Exit;

 d:=nil;
 rel:=nil;
 rw_wlock(t^.FLock);

 if (cb<>nil) then
 begin
  data:=HAMT_search32(@t^.FHAMT,Key);
  if (data=nil) then Goto _exit; //not found
  d:=data^;
  if not cb(d) then Goto _exit; //filtred
  d:=nil;
 end;

 if HAMT_delete32(@t^.FHAMT,Key,@d) then
 begin
  //deleted
  if (old<>nil) then
  begin
   old^:=d;
  end else
  begin
   rel:=d;
  end;

  Dec(t^.FCount);

  t^.FLast:=Key;

  p:=t^.FPos-1;

  if (p=Key) and
     (p>=t^.min_key) and
     (p<=t^.max_key) then
  begin
   //dec max uses
   t^.FPos:=p;
   Dec(p);

   while (p>=t^.min_key) do
   begin
    if (HAMT_search32(@t^.FHAMT,p)<>nil) then Break; //found
    //dec space
    if (t^.FSpace<>0) then
    begin
     Dec(t^.FSpace);
    end;
    t^.FPos:=p;
    Dec(p);
   end;

  end else
  begin
   //insert space
   Inc(t^.FSpace);
  end;

  Result:=True;
 end else
 if (old<>nil) then
 begin
  //not found
  old^:=nil;
 end;

 _exit:
  rw_wunlock(t^.FLock);
 //
 id_release(rel);
end;


end.

