unit param_sfo_gui;

{$mode ObjFPC}{$H+}

interface

uses
 sysutils;

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

 TParamSfoValue=packed object
  format:ptruint;
  name,value:RawByteString;
  Function GetString:RawByteString;
  Function GetUInt  :DWORD;
 end;

 TParamSfoFile=class
  params:array of TParamSfoValue;
  Function GetString(const name:RawByteString):RawByteString;
  Function GetUInt  (const name:RawByteString):DWORD;
 end;

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;

implementation

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;
Var
 fd:THandle;

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
  FileSeek(fd,offset,fsFromBeginning);
  if (FileRead(fd,Result^,size)<>size) then
  begin
   FreeMem(Result);
   Result:=nil;
  end;
 end;

begin
 Result:=nil;

 fd:=FileOpen(path,fmOpenRead);
 if (fd=feInvalidHandle) then
 begin
  Writeln(StdErr,'Error open:',path);
  Exit;
 end;

 hdr:=Default(t_sfo_header);
 if (FileRead(fd,hdr,SizeOf(hdr))<>SizeOf(hdr)) then
 begin
  Writeln(StdErr,'Error read:',path);
  FileClose(fd);
  Exit;
 end;

 if (hdr.magic<>SFO_MAGIC) then
 begin
  Writeln(StdErr,'Invalid file:',path);
  FileClose(fd);
  Exit;
 end;

 entry_table:=load_chunk(SizeOf(hdr),hdr.entry_count*SizeOf(t_sfo_table_entry));

 fsize:=FileSeek(fd,0,fsFromEnd);

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

  FileClose(fd);
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

 FileClose(fd);
end;

//

Function TParamSfoValue.GetString:RawByteString;
var
 D:DWORD;
begin
 Result:='';
 case format of
  SFO_FORMAT_STRING_SPECIAL,
  SFO_FORMAT_STRING        :Result:=value;
  SFO_FORMAT_UINT32        :
    begin
     D:=PDWORD(PChar(value))^;
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
  SFO_FORMAT_STRING_SPECIAL,
  SFO_FORMAT_STRING        :
    begin
     D:=0;
     TryStrToDWord(value,D);
     Result:=D;
    end;
  SFO_FORMAT_UINT32        :
    begin
     D:=PDWORD(PChar(value))^;
     Result:=D;
    end;
  else;
 end;
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


end.

