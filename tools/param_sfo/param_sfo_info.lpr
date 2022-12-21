
uses
 {$IFDEF Linux}
 cmem,
 cthreads,
 {$ENDIF}
 Classes,
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

function get_str_format(format:WORD):RawByteString;
begin
 Case format of
  SFO_FORMAT_STRING_SPECIAL:Result:='STRING_SPECIAL';
  SFO_FORMAT_STRING        :Result:='STRING';
  SFO_FORMAT_UINT32        :Result:='UINT32';
  else
                            Result:='0x'+HexStr(format,4);
 end;
end;

function LoadParamSfoFile(Const name:RawByteString):Boolean;
Var
 F:THandle;

 fsize:Int64;

 i:DWORD;

 key_table_size  :DWORD;
 value_table_size:DWORD;

 hdr:t_sfo_header;

 entry_table:p_sfo_table_entry;
 key_table:PChar;
 value_table:PByte;

 function load_chunk(offset,size:DWORD):Pointer;
 begin
  Result:=AllocMem(size);
  FileSeek(F,offset,fsFromBeginning);
  FileRead(F,Result^,size);
 end;

begin
 Result:=False;
 if (name='') then Exit;
 F:=FileOpen(name,fmOpenRead or fmShareDenyNone);

 if (F=feInvalidHandle) then
 begin
  Writeln('Error open file:',name);
  Exit;
 end;

 hdr:=Default(t_sfo_header);

 FileRead(F,hdr,SizeOf(hdr));

 Writeln('magic              :0x',HexStr(hdr.magic,8));

 if (hdr.magic<>SFO_MAGIC) then
 begin
  Writeln('Invalid sfo file:',name);
  FileClose(F);
  Exit;
 end;

 Writeln('version            :0x',HexStr(hdr.version,8));
 Writeln('key_table_offset   :',hdr.key_table_offset);
 Writeln('value_table_offset :',hdr.value_table_offset);
 Writeln('entry_count        :',hdr.entry_count);

 entry_table:=load_chunk(SizeOf(hdr),hdr.entry_count*SizeOf(t_sfo_table_entry));

 fsize:=FileSeek(F,0,fsFromEnd);

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

 if (hdr.entry_count<>0) then
 begin
  For i:=0 to hdr.entry_count-1 do
  begin
   Writeln('[entry]:',i);
   //Writeln('key_offset  :',entry_table[i].key_offset);
   Writeln('format      :',get_str_format(entry_table[i].format));
   //Writeln('size        :',entry_table[i].size);
   //Writeln('max_size    :',entry_table[i].max_size);
   //Writeln('value_offset:',entry_table[i].value_offset);
   Writeln('key         :',PChar(key_table+entry_table[i].key_offset));

   Case entry_table[i].format of
    SFO_FORMAT_STRING_SPECIAL,
    SFO_FORMAT_STRING        :
     begin
      Writeln('value       :',PChar(value_table+entry_table[i].value_offset));
     end;
    SFO_FORMAT_UINT32        :
     begin
      Writeln('value       :0x',HexStr(PQWORD(value_table+entry_table[i].value_offset)^,entry_table[i].max_size*2));
     end;
    else;
   end;

  end;
 end;

 FileClose(F);
end;

var
 i,n:Integer;

label
 promo;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;

 if (ParamCount=0) then
 begin
  promo:
  Writeln('Tool for print param.sfo data');
  Writeln(' Parameters:');
  Writeln('  -i <name>  //file name to load param.sfo');
  Exit;
 end;

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
    '-i':n:=0;
   else
     if (n<>-1) then
     begin
      Case n of
       0:LoadParamSfoFile(ParamStr(i));
      end;
      n:=-1;
     end;
  end;
 end;

 writeln;
end.



