unit param_sfo;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils;

const
 //sfo_value_format
 SFO_FORMAT_BLOB  =$004;
 SFO_FORMAT_STRING=$204;
 SFO_FORMAT_UINT32=$404;

 SFO_MAGIC  =$46535000;
 SFO_VERSION=$101;

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

 t_param_sfo_load_cb=procedure(userdata:Pointer;name,value:pchar;format:WORD;size,max_size,i:DWORD);

 TParamSfoFileLoader=object
  path:RawByteString;

  fd:THandle;

  fsize:Int64;

  entry_table_size:DWORD;
  key_table_size  :DWORD;
  value_table_size:DWORD;

  hdr:t_sfo_header;

  entry_table:p_sfo_table_entry;
  key_table  :PChar;
  value_table:PByte;

  function  open(const _path:RawByteString):Boolean;
  procedure Free;
  function  load_chunk(offset,size:DWORD):Pointer;
  function  save_chunk(buf:Pointer;size:DWORD):Boolean;
  function  check_entry_table:Boolean;
  function  check_key_table:Boolean;
  function  parse():Boolean;
  procedure ForAll(cbs:t_param_sfo_load_cb;userdata:Pointer);
  //
  procedure New(e,k,v:DWORD);
  procedure AddNameValue(name,value:pchar;format:WORD;size,max_size:DWORD);
  function  save(const _path:RawByteString):Boolean;
 end;

implementation

function TParamSfoFileLoader.open(const _path:RawByteString):Boolean;
begin
 Result:=False;

 path:=_path;
 fd:=FileOpen(path,fmOpenRead);
 if (fd=feInvalidHandle) then
 begin
  Writeln(StdErr,'Error sfo open:',path);
  Exit;
 end;

 entry_table:=nil;
 key_table  :=nil;
 value_table:=nil;

 Result:=True;
end;

procedure TParamSfoFileLoader.Free;
begin
 if (entry_table<>nil) then FreeMem(entry_table);
 if (key_table  <>nil) then FreeMem(key_table);
 if (value_table<>nil) then FreeMem(value_table);

 if (fd<>feInvalidHandle) then FileClose(fd);
end;

function TParamSfoFileLoader.load_chunk(offset,size:DWORD):Pointer;
begin
 Result:=AllocMem(size);
 FileSeek(fd,offset,fsFromBeginning);
 if (FileRead(fd,Result^,size)<>size) then
 begin
  FreeMem(Result);
  Result:=nil;
 end;
end;

function TParamSfoFileLoader.save_chunk(buf:Pointer;size:DWORD):Boolean;
begin
 Result:=(FileWrite(fd,buf^,size)=size);
end;

function TParamSfoFileLoader.parse():Boolean;
begin
 Result:=False;

 hdr:=Default(t_sfo_header);
 if (FileRead(fd,hdr,SizeOf(hdr))<>SizeOf(hdr)) then
 begin
  Writeln(StdErr,'Error sfo read:',path);
  Exit;
 end;

 if (hdr.magic<>SFO_MAGIC) then
 begin
  Writeln(StdErr,'Invalid sfo file(magic<>SFO_MAGIC):',path);
  Exit;
 end;

 if (hdr.version<>SFO_VERSION) then
 begin
  Writeln(StdErr,'Invalid sfo file(version<>$101):',path);
  Exit;
 end;

 fsize:=FileSeek(fd,0,fsFromEnd);

 if (hdr.key_table_offset>=fsize) then
 begin
  Writeln(StdErr,'Invalid sfo file(key_table_offset>=fsize):',path);
  Exit;
 end;

 if (hdr.value_table_offset>=fsize) then
 begin
  Writeln(StdErr,'Invalid sfo file(value_table_offset>=fsize):',path);
  Exit;
 end;

 if (hdr.key_table_offset>=hdr.value_table_offset) then
 begin
  Writeln(StdErr,'Invalid sfo file(key_table_offset>=value_table_offset):',path);
  Exit;
 end;

 entry_table_size:=hdr.entry_count*SizeOf(t_sfo_table_entry);

 if ((SizeOf(hdr)+entry_table_size)>=fsize) then
 begin
  Writeln(StdErr,'Invalid sfo file((SizeOf(hdr)+entry_table_size)>=fsize):',path);
  Exit;
 end;

 entry_table:=load_chunk(SizeOf(hdr),entry_table_size);

 key_table_size  :=hdr.value_table_offset-hdr.key_table_offset;
 value_table_size:=fsize                 -hdr.value_table_offset;

 key_table  :=load_chunk(hdr.key_table_offset  ,key_table_size  );
 value_table:=load_chunk(hdr.value_table_offset,value_table_size);

 if (entry_table=nil) or
    (key_table=nil) or
    (value_table=nil) then
 begin
  Writeln(StdErr,'Error sfo read:',path);
  Exit;
 end;

 if (not check_entry_table) then
 begin
  Writeln(StdErr,'Invalid sfo file(check_entry_table):',path);
  Exit;
 end;

 if (not check_key_table) then
 begin
  Writeln(StdErr,'Invalid sfo file(check_key_table):',path);
  Exit;
 end;

 if (fd<>feInvalidHandle) then
 begin
  FileClose(fd);
  fd:=feInvalidHandle;
 end;

 Result:=True;
end;

function TParamSfoFileLoader.check_entry_table:Boolean;
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

function TParamSfoFileLoader.check_key_table:Boolean;
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

procedure TParamSfoFileLoader.ForAll(cbs:t_param_sfo_load_cb;userdata:Pointer);
var
 i:DWORD;
 name :pchar;
 value:Pointer;
begin

 if (hdr.entry_count<>0) then
 begin

  For i:=0 to hdr.entry_count-1 do
  begin
   name :=PChar(key_table  +entry_table[i].key_offset);
   value:=PChar(value_table+entry_table[i].value_offset);

   cbs(userdata,
       name,
       value,
       entry_table[i].format,
       entry_table[i].size,
       entry_table[i].max_size,
       i
      );

  end;
 end;

end;

procedure TParamSfoFileLoader.New(e,k,v:DWORD);
begin
 Self:=Default(TParamSfoFileLoader);
 hdr.magic  :=SFO_MAGIC;
 hdr.version:=SFO_VERSION;
 //prealloc
 ReAllocMem(entry_table,e);
 ReAllocMem(key_table  ,k);
 ReAllocMem(value_table,v);
end;

procedure TParamSfoFileLoader.AddNameValue(name,value:pchar;format:WORD;size,max_size:DWORD);
var
 i,k,v:DWORD;
 name_len:DWORD;
 entry:p_sfo_table_entry;
 np:Pointer;
 vp:Pointer;
begin
 i:=hdr.entry_count;
 k:=key_table_size;
 v:=value_table_size;

 hdr.entry_count:=i+1;

 name_len:=strlen(name)+1;

 entry_table_size:=hdr.entry_count*SizeOf(t_sfo_table_entry);
 key_table_size  :=key_table_size  +name_len;
 value_table_size:=value_table_size+max_size;

 ReAllocMem(entry_table,entry_table_size);
 ReAllocMem(key_table  ,key_table_size);
 ReAllocMem(value_table,value_table_size);

 entry:=@entry_table[i];
 entry^.key_offset  :=k;
 entry^.format      :=format;
 entry^.size        :=size;
 entry^.max_size    :=max_size;
 entry^.value_offset:=v;

 np:=key_table  +k;
 vp:=value_table+v;

 Move(name^ ,np^,name_len);
 Move(value^,vp^,size);
end;

function TParamSfoFileLoader.save(const _path:RawByteString):Boolean;
begin
 Result:=False;

 path:=_path;
 fd:=FileCreate(path);
 if (fd=feInvalidHandle) then
 begin
  Writeln(StdErr,'Error sfo create:',path);
  Exit;
 end;

 //align
 if (key_table_size and (-4))<>0 then
 begin
  key_table_size:=(key_table_size+3) and (-4);
  ReAllocMem(key_table  ,key_table_size);
 end;

 hdr.key_table_offset  :=SizeOf(hdr)+entry_table_size;
 hdr.value_table_offset:=hdr.key_table_offset+key_table_size;

 if not save_chunk(@hdr,SizeOf(hdr)) then
 begin
  Writeln(StdErr,'Error sfo write:',path);
  Exit;
 end;

 if not save_chunk(entry_table,entry_table_size) then
 begin
  Writeln(StdErr,'Error sfo write:',path);
  Exit;
 end;

 if not save_chunk(key_table,key_table_size) then
 begin
  Writeln(StdErr,'Error sfo write:',path);
  Exit;
 end;

 if not save_chunk(value_table,value_table_size) then
 begin
  Writeln(StdErr,'Error sfo write:',path);
  Exit;
 end;

 FileFlush(fd);

 Result:=True;
end;


end.

