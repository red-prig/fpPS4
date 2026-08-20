unit placeholder_fmt;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils;

type
 p_placeholder_value=^t_placeholder_value;
 t_placeholder_value=record
  id     :DWORD;
  maxsize:DWORD;
  name   :RawByteString;
  fmt    :RawByteString;
 end;

 t_fmt_builder=object
  fmt :RawByteString;
  ids :array of DWORD;
  max :DWORD;
  bits:QWORD;
  procedure build(const placeholder:RawByteString;values:p_placeholder_value;count:Integer);
 end;

type
 t_resolve_values_cb=function(const name:RawByteString;userdata:Pointer):RawByteString;

function GetPathValues(const name:RawByteString;userdata:Pointer):RawByteString;
Function ResolvePlaceholder(const placeholder:RawByteString;values:t_resolve_values_cb;userdata:Pointer):RawByteString;
Function ResolvePath(const placeholder:RawByteString):RawByteString;

implementation

procedure t_fmt_builder.build(const placeholder:RawByteString;values:p_placeholder_value;count:Integer);
var
 i,state:Integer;
 name:RawByteString;

 procedure Add;
 var
  i:Integer;
 begin

  if (name='') then
  begin
   fmt:=fmt+'%%';
  end else
  begin
   name:=Trim(name);
   For i:=0 to count-1 do
   if CompareText(name,Trim(values[i].name))=0 then
   begin
    fmt:=fmt+values[i].fmt;
    Insert(values[i].id,ids,Length(ids));
    max:=max+values[i].maxsize;
    bits:=bits or (QWORD(1) shl values[i].id);
    Break;
   end;
  end;

 end;

begin
 Self:=Default(t_fmt_builder);
 //
 if (values=nil) or (count=0) then Exit;
 if (Length(placeholder)=0) then Exit;
 //
 state:=0;
 name:='';

 For i:=1 to Length(placeholder) do
 begin

  if (state=0) then
  begin

   if (placeholder[i]='%') then
   begin
    state:=1;
   end else
   begin
    fmt:=fmt+placeholder[i];
    max:=max+1;
   end;

  end else
  begin

   if (placeholder[i]='%') then
   begin
    Add;
    name:='';
    state:=0;
   end else
   begin
    name:=name+placeholder[i];
   end;

  end;

 end; //for

 if (state=1) then
 begin
  Add;
 end;

end;

////

function GetPathValues(const name:RawByteString;userdata:Pointer):RawByteString;
begin
 Result:='';
 case name of
  'Cd'             :Result:=GetCurrentDir;
  'TempDir'        :Result:=GetTempDir;
  'TempFileName'   :Result:=GetTempFileName;
  'AppConfigDir'   :Result:=GetAppConfigDir(False);
  'UserDir'        :Result:=GetUserDir;
  'ApplicationName':Result:=ApplicationName;
  else
                    Result:=GetEnvironmentVariable(name);
 end;
end;

Function ResolvePlaceholder(const placeholder:RawByteString;values:t_resolve_values_cb;userdata:Pointer):RawByteString;
var
 i,state:Integer;
 name:RawByteString;

 procedure Add;
 begin
  name:=Trim(name);
  Result:=Result+values(name,userdata);
 end;

begin
 Result:='';
 //
 if (Length(placeholder)=0) then Exit;
 //
 state:=0;
 name:='';

 For i:=1 to Length(placeholder) do
 begin

  if (state=0) then
  begin

   if (placeholder[i]='%') then
   begin
    state:=1;
   end else
   begin
    Result:=Result+placeholder[i];
   end;

  end else
  begin

   if (placeholder[i]='%') then
   begin
    Add;
    name:='';
    state:=0;
   end else
   begin
    name:=name+placeholder[i];
   end;

  end;

 end; //for

 if (state=1) then
 begin
  Add;
 end;

end;

Function ResolvePath(const placeholder:RawByteString):RawByteString;
begin
 Result:=ResolvePlaceholder(placeholder,@GetPathValues,nil);
end;



end.


