unit param_sfo_gui;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils,
 core_serialization;

const
 //sfo_value_format
 SFO_FORMAT_BLOB  =$004;
 SFO_FORMAT_STRING=$204;
 SFO_FORMAT_UINT32=$404;

 SFO_MAGIC=$46535000;

 SFO_HEADER_SIZE     =$14;
 SFO_TABLE_ENTRY_SIZE=$10;

type
 t_sfo_header=packed record
  magic             :DWORD;
  version           :DWORD;
  key_table_offset  :DWORD;
  value_table_offset:DWORD;
  entry_count       :DWORD;
 end;

 p_sfo_table_entry=^t_sfo_table_entry;
 t_sfo_table_entry=packed record
  key_offset  :WORD;   //<-key_table
  format      :WORD;
  size        :DWORD;
  max_size    :DWORD;
  value_offset:DWORD;  //<-value_table
 end;

//

 TParamSfoValue=class(TSerializeObject)
  private
   Fformat:ptruint;
   Fname  :RawByteString;
   Fvalue :RawByteString;
  published
   property format:ptruint       read Fformat write Fformat;
   property name  :RawByteString read Fname   write Fname;
   property value :RawByteString read Fvalue  write Fvalue;
  public
   Function GetLength:QWORD;
   Function GetString:RawByteString;
   Function GetUInt  :DWORD;
   Function GetUInt64:QWORD;
 end;

 TParamSfoFile=class(TSerializeArray)
  params:array of TParamSfoValue;
  //
  Destructor Destroy; override;
  //
  Function   GetString(const name:RawByteString):RawByteString;
  Function   GetUInt  (const name:RawByteString):DWORD;
  //
  Function   GetArrayCount:SizeInt;          override;
  Function   GetArrayItem(i:SizeInt):TValue; override;
  Function   AddObject:TSerializeObject;     override;
  Function   AddArray :TSerializeArray;      override;
  procedure  AddValue(Value:TValue);         override;
 end;

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;

implementation

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;
Var
 fd:THandle;

 fsize:Int64;

 entry_table_size:DWORD;
 key_table_size  :DWORD;
 value_table_size:DWORD;

 hdr:t_sfo_header;

 entry_table:p_sfo_table_entry;
 key_table  :PChar;
 value_table:PByte;

 function load_chunk(offset,size:DWORD):Pointer;
 begin
  Result:=AllocMem(size);
  FileSeek(fd,offset,fsFromBeginning);
  if (FileRead(fd,Result^,size)<>size) then
  begin
   FreeMem(Result);
   Result:=nil;
  end;
 end;

 function check_entry_table:Boolean;
 var
  i:DWORD;
  e:DWORD;
  entry:p_sfo_table_entry;
 begin
  Result:=True;

  entry:=entry_table;

  For i:=0 to hdr.entry_count-1 do
  begin

   if (hdr.value_table_offset<=(entry^.key_offset+hdr.key_table_offset)) then
   begin
    Exit(False);
   end;

   case entry^.format of
      $0,
      $4,
      $8,
     $10,
     $20,
    $100,
    $104,
    $108,
    $110,
    $120,
    $204,
    $304,
    $404,
    $504:;
    else
      Exit(False);
   end;

   if (entry^.max_size<entry^.size) then
   begin
    Exit(False);
   end;

   e:=entry^.value_offset+entry^.max_size;

   if (fsize<e) then
   begin
    Exit(False);
   end;

   if ((hdr.entry_count-1)=i) then
   begin
    if ((e+hdr.value_table_offset)<>fsize) then
    begin
     Exit(False);
    end;
   end else
   if (e<>entry[1].value_offset) then
   begin
    Exit(False);
   end;

   Inc(entry);
  end;

 end;

 function check_key_table:Boolean;
 var
  i:DWORD;
  p:pchar;
 begin
  Result:=True;

  p:=key_table;

  For i:=0 to key_table_size-1 do
  begin

   case p^ of
    #0:;
    #1..#32,#35,#127:Exit(False);
    else;
   end;

   Inc(p);
  end;

 end;

 procedure do_load;
 var
  i:DWORD;
  format:WORD;
  size,data_size:DWORD;
  name,value:RawByteString;
 begin

  if (hdr.entry_count<>0) then
  begin
   SetLength(Result.params,hdr.entry_count);

   For i:=0 to hdr.entry_count-1 do
   begin
    format:=entry_table[i].format;

    name :=PChar(key_table+entry_table[i].key_offset);

    value:='';
    size:=entry_table[i].max_size;
    data_size:=size;

    case format of
     SFO_FORMAT_UINT32:
       begin
       if (data_size<4) then data_size:=4;
       end;
     else;
    end;

    SetLength(value,data_size);
    FillChar(value[1],data_size,0);

    Move(PChar(value_table+entry_table[i].value_offset)^,value[1],size);

    case format of
     SFO_FORMAT_STRING:
       begin
        //fixup len
        SetLength(value,strlen(PChar(@value[1])));
       end;
     else;
    end;

    Result.params[i]:=TParamSfoValue.Create;
    Result.params[i].format:=format;
    Result.params[i].name  :=name;
    Result.params[i].value :=value;
   end;
  end;

 end;

label
 err_table;

begin
 Result:=nil;

 fd:=FileOpen(path,fmOpenRead);
 if (fd=feInvalidHandle) then
 begin
  Writeln(StdErr,'Error sfo open:',path);
  Exit;
 end;

 hdr:=Default(t_sfo_header);
 if (FileRead(fd,hdr,SizeOf(hdr))<>SizeOf(hdr)) then
 begin
  Writeln(StdErr,'Error sfo read:',path);
  FileClose(fd);
  Exit;
 end;

 if (hdr.magic<>SFO_MAGIC) then
 begin
  Writeln(StdErr,'Invalid sfo file(magic<>SFO_MAGIC):',path);
  FileClose(fd);
  Exit;
 end;

 if (hdr.version<>$101) then
 begin
  Writeln(StdErr,'Invalid sfo file(version<>$101):',path);
  FileClose(fd);
  Exit;
 end;

 fsize:=FileSeek(fd,0,fsFromEnd);

 if (hdr.key_table_offset>=fsize) then
 begin
  Writeln(StdErr,'Invalid sfo file(key_table_offset>=fsize):',path);
  FileClose(fd);
  Exit;
 end;

 if (hdr.value_table_offset>=fsize) then
 begin
  Writeln(StdErr,'Invalid sfo file(value_table_offset>=fsize):',path);
  FileClose(fd);
  Exit;
 end;

 if (hdr.key_table_offset>=hdr.value_table_offset) then
 begin
  Writeln(StdErr,'Invalid sfo file(key_table_offset>=value_table_offset):',path);
  FileClose(fd);
  Exit;
 end;

 entry_table_size:=hdr.entry_count*SizeOf(t_sfo_table_entry);

 if ((SizeOf(hdr)+entry_table_size)>=fsize) then
 begin
  Writeln(StdErr,'Invalid sfo file((SizeOf(hdr)+entry_table_size)>=fsize):',path);
  FileClose(fd);
  Exit;
 end;

 entry_table:=load_chunk(SizeOf(hdr),entry_table_size);

 key_table_size  :=hdr.value_table_offset-hdr.key_table_offset;
 value_table_size:=fsize                 -hdr.value_table_offset;

 key_table  :=load_chunk(hdr.key_table_offset  ,key_table_size  );
 value_table:=load_chunk(hdr.value_table_offset,value_table_size);

 //

 if (entry_table=nil) or
    (key_table=nil) or
    (value_table=nil) then
 begin
  Writeln(StdErr,'Error sfo read:',path);

  err_table:

  FreeMem(entry_table);
  FreeMem(key_table);
  FreeMem(value_table);

  FileClose(fd);
  Exit;
 end;

 if (not check_entry_table) then
 begin
  Writeln(StdErr,'Invalid sfo file(check_entry_table):',path);
  goto err_table;
 end;

 if (not check_key_table) then
 begin
  Writeln(StdErr,'Invalid sfo file(check_key_table):',path);
  goto err_table;
 end;

 Result:=TParamSfoFile.Create;

 do_load;

 FreeMem(entry_table);
 FreeMem(key_table);
 FreeMem(value_table);

 FileClose(fd);
end;

//

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

Function TParamSfoValue.GetLength:QWORD;
begin
 Result:=Length(value);
end;

Function TParamSfoValue.GetString:RawByteString;
var
 D:DWORD;
begin
 Result:='';
 case format of
  SFO_FORMAT_BLOB:
    begin
     Result:='';
     if Length(value)<>0 then
     For D:=1 to Length(value) do
     begin
      Result:=Result+HexStr(Byte(value[D]),2);
     end;
    end;
  SFO_FORMAT_STRING:
    begin
     Result:=value;
    end;
  SFO_FORMAT_UINT32:
    begin
     D:=PDWORD(@value[1])^;
     Result:=UIntToStr(D);
    end;
  else;
 end;
end;

Function TParamSfoValue.GetUInt:DWORD;
var
 D:DWORD;
begin
 Result:=0;
 case format of
  SFO_FORMAT_BLOB:
    begin
     D:=0;
     Move(value[1],D,Min(SizeOf(DWORD),Length(value)));
     Result:=D;
    end;
  SFO_FORMAT_STRING:
    begin
     D:=0;
     TryStrToDWord(value,D);
     Result:=D;
    end;
  SFO_FORMAT_UINT32:
    begin
     D:=PDWORD(@value[1])^;
     Result:=D;
    end;
  else;
 end;
end;

Function TParamSfoValue.GetUInt64:QWORD;
var
 D:QWORD;
begin
 Result:=0;
 case format of
  SFO_FORMAT_BLOB:
    begin
     D:=0;
     Move(value[1],D,Min(SizeOf(QWORD),Length(value)));
     Result:=D;
    end;
  SFO_FORMAT_STRING:
    begin
     D:=0;
     TryStrToQWord(value,D);
     Result:=D;
    end;
  SFO_FORMAT_UINT32:
    begin
     D:=PDWORD(@value[1])^;
     Result:=D;
    end;
  else;
 end;
end;

//

Destructor TParamSfoFile.Destroy;
var
 i:Integer;
begin
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  FreeAndNil(params[i]);
 end;
 SetLength(params,0);
 inherited;
end;

Function TParamSfoFile.GetString(const name:RawByteString):RawByteString;
var
 i:Integer;
begin
 Result:='';
 if (Self=nil) then Exit;
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  if (params[i].name=name) then
  begin
   Result:=params[i].GetString;
   Exit;
  end;
 end;
end;

Function TParamSfoFile.GetUInt(const name:RawByteString):DWORD;
var
 i:Integer;
begin
 Result:=0;
 if (Self=nil) then Exit;
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  if (params[i].name=name) then
  begin
   Result:=params[i].GetUInt;
   Exit;
  end;
 end;
end;

//////

Function TParamSfoFile.GetArrayCount:SizeInt;
begin
 Result:=Length(params);
end;

Function TParamSfoFile.GetArrayItem(i:SizeInt):TValue;
begin
 if (i>=Length(params)) then
 begin
  Result:=TValue.Empty;
 end else
 begin
  Result:=params[i];
 end;
end;

Function TParamSfoFile.AddObject:TSerializeObject;
begin
 Result:=TParamSfoValue.Create;
end;

Function TParamSfoFile.AddArray:TSerializeArray;
begin
 Result:=nil;
end;

procedure TParamSfoFile.AddValue(Value:TValue);
begin
 Insert(Value.AsObject,params,Length(params));
end;

end.

