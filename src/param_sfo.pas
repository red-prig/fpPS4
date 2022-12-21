unit param_sfo;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_kernel_file;

const
 //sfo_value_format
 SFO_FORMAT_STRING_SPECIAL=$004;
 SFO_FORMAT_STRING        =$204;
 SFO_FORMAT_UINT32        =$404;

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

 TParamSfoValue=packed record
  format:ptruint;
  name,value:RawByteString;
 end;

 TParamSfoFile=class
  params:array of TParamSfoValue;
  Function GetString(const name:RawByteString):PChar;
  Function GetInt(const name:RawByteString):Integer;
 end;

function  LoadParamSfoFile(path:PChar):TParamSfoFile;

procedure init_param_sfo;
function  ParamSfoGetString(const name:RawByteString):PChar;
function  ParamSfoGetInt(const name:RawByteString):Integer;

implementation

uses
 atomic,
 spinlock;

function LoadParamSfoFile(path:PChar):TParamSfoFile;
Var
 fd:Integer;

 fsize:Int64;

 i:DWORD;

 key_table_size  :DWORD;
 value_table_size:DWORD;

 hdr:t_sfo_header;

 entry_table:p_sfo_table_entry;
 key_table:PChar;
 value_table:PByte;

 size:DWORD;
 name,value:RawByteString;

 function load_chunk(offset,size:DWORD):Pointer;
 begin
  Result:=AllocMem(size);
  if (ps4_pread(fd,Result,size,offset)<>size) then
  begin
   FreeMem(Result);
   Result:=nil;
  end;
 end;

begin
 Result:=nil;

 fd:=ps4_open(path,0,0);
 if (fd=-1) then
 begin
  Writeln(StdErr,'Error open:',path);
  Exit;
 end;

 hdr:=Default(t_sfo_header);
 if (ps4_read(fd,@hdr,SizeOf(hdr))<>SizeOf(hdr)) then
 begin
  Writeln(StdErr,'Error read:',path);
  ps4_close(fd);
  Exit;
 end;

 if (hdr.magic<>SFO_MAGIC) then
 begin
  Writeln(StdErr,'Invalid file:',path);
  ps4_close(fd);
  Exit;
 end;

 entry_table:=load_chunk(SizeOf(hdr),hdr.entry_count*SizeOf(t_sfo_table_entry));

 fsize:=ps4_lseek(fd,0,2);

 if (hdr.key_table_offset>hdr.value_table_offset) then
 begin
  key_table_size  :=fsize               -hdr.key_table_offset;
  value_table_size:=hdr.key_table_offset-hdr.value_table_offset;
 end else
 begin
  key_table_size  :=hdr.value_table_offset-hdr.key_table_offset;
  value_table_size:=fsize                 -hdr.value_table_offset;
 end;

 key_table  :=load_chunk(hdr.key_table_offset  ,key_table_size);
 value_table:=load_chunk(hdr.value_table_offset,value_table_size);

 //

 if (entry_table=nil) or
    (key_table=nil) or
    (value_table=nil) then
 begin
  Writeln(StdErr,'Error read:',path);

  FreeMem(entry_table);
  FreeMem(key_table);
  FreeMem(value_table);

  ps4_close(fd);
  Exit;
 end;

 Result:=TParamSfoFile.Create;

 if (hdr.entry_count<>0) then
 begin
  SetLength(Result.params,hdr.entry_count);

  For i:=0 to hdr.entry_count-1 do
  begin
   name :=PChar(key_table+entry_table[i].key_offset);

   value:='';
   size:=entry_table[i].max_size;
   SetLength(value,size);
   Move(PChar(value_table+entry_table[i].value_offset)^,PChar(value)^,size);

   Result.params[i].format:=entry_table[i].format;
   Result.params[i].name  :=name;
   Result.params[i].value :=value;
  end;
 end;

 FreeMem(entry_table);
 FreeMem(key_table);
 FreeMem(value_table);

 ps4_close(fd);
end;

//

Function TParamSfoFile.GetString(const name:RawByteString):PChar;
var
 i:Integer;
begin
 Result:=nil;
 if (Self=nil) then Exit;
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  if (params[i].name=name) then
  begin
   Result:=PChar(params[i].value);
   Exit;
  end;
 end;
end;

Function TParamSfoFile.GetInt(const name:RawByteString):Integer;
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
   Result:=PInteger(PChar(params[i].value))^;
   Exit;
  end;
 end;
end;

//

var
 param_sfo_lazy_init:Integer=0;
 param_sfo_file:TParamSfoFile=nil;

procedure init_param_sfo;
begin
 if CAS(param_sfo_lazy_init,0,1) then
 begin
  param_sfo_file:=LoadParamSfoFile('/app0/sce_sys/param.sfo');
  param_sfo_lazy_init:=2;
 end else
 begin
  wait_until_equal(param_sfo_lazy_init,1);
 end;
end;

function ParamSfoGetString(const name:RawByteString):PChar;
begin
 init_param_sfo;
 Result:=param_sfo_file.GetString(name);
end;

function ParamSfoGetInt(const name:RawByteString):Integer;
begin
 init_param_sfo;
 Result:=param_sfo_file.GetInt(name);
end;



end.

