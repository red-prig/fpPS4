unit playgo_chunk_loader;

{$mode ObjFPC}{$H+}

interface

uses
 bittype,
 ps4_kernel_file;

const
 PLAYGO_MAGIC=$6F676C70;

type
 t_playgo_chunk_loc=bitpacked record
  offset:bit48;
  _align1:Byte;
  image_no:bit4;
  _align2:bit4;
 end;

 t_playgo_chunk_size=bitpacked record
  size:bit48;
  _align:Word;
 end;

 t_chunk=packed record
  offset:DWORD;
  size  :DWORD;
 end;

 t_playgo_header=packed record
  magic:DWORD;

  version_major :Word;
  version_minor :Word;
  image_count   :Word; // [0;1]
  chunk_count   :Word; // [0;1000]
  mchunk_count  :Word; // [0;8000]
  scenario_count:Word; // [0;32]

  file_size  :DWORD;
  default_scenario_id:Word;
  attrib     :Word;
  sdk_version:DWORD;
  disc_count :Word;  // [0;2] (if equals to 0 then disc count = 1)
  layer_bmp  :Word;

  reserved:array[0..31] of Byte;

  content_id:array[0..127] of AnsiChar;

  // chunk attributes
  chunk_attrs:t_chunk; // [0;32000]

  // chunk mchunks
  chunk_mchunks:t_chunk;

  // chunk labels
  chunk_labels:t_chunk; // [0;16000]

  // mchunk attributes
  mchunk_attrs:t_chunk; // [0;12800]

  // scenario attributes
  scenario_attrs:t_chunk; // [0;1024]

  // scenarios chunks
  scenario_chunks:t_chunk;

  // scenario labels
  scenario_labels:t_chunk;

  // inner mchunk attributes
  inner_mchunk_attrs:t_chunk; // [0;12800]
 end;

 p_playgo_scenario_attr_entry=^t_playgo_scenario_attr_entry;
 t_playgo_scenario_attr_entry=packed record
  _type:Byte;
  _unk:array[0..18] of Byte;
  initial_chunk_count:WORD;
  chunk_count  :WORD;
  chunks_offset:DWORD;       //<-scenario_chunks
  label_offset :DWORD;       //<-scenario_labels
 end;

 t_image_disc_layer_no=bitpacked record
  layer_no:bit2;
  disc_no :bit2;
  image_no:bit4;
 end;

 p_playgo_chunk_attr_entry=^t_playgo_chunk_attr_entry;
 t_playgo_chunk_attr_entry=packed record
  flag:Byte;
  image_disc_layer_no:t_image_disc_layer_no;
  req_locus:Byte;
  unk:array[0..10] of Byte;
  mchunk_count:Word;
  language_mask:QWORD;
  mchunks_offset:DWORD;     //<-chunk_mchunks
  label_offset:DWORD;       //<-chunk_labels
 end;

 p_playgo_mchunk_attr_entry=^t_playgo_mchunk_attr_entry;
 t_playgo_mchunk_attr_entry=packed record
  loc:t_playgo_chunk_loc;
  size:t_playgo_chunk_size;
 end;

//

 TPlaygoChunk=packed record
  req_locus:QWORD;
  language_mask:QWORD;
  total_size:QWORD;
  label_name:RawByteString;
 end;

 TPlaygoFile=class
  content_id:RawByteString;
  chunks:array of TPlaygoChunk;
 end;

function LoadPlaygoFile(path:PChar):TPlaygoFile;

implementation

function LoadPlaygoFile(path:PChar):TPlaygoFile;
var
 fd:Integer;

 i,m,mchunk_count,mchunk_id:Word;

 total_size:QWORD;

 mchunks:PWord;

 hdr:t_playgo_header;

 chunk_attrs:p_playgo_chunk_attr_entry;
 chunk_mchunks:PWord;
 chunk_labels:PChar;
 mchunk_attrs:p_playgo_mchunk_attr_entry;

 function load_chunk(t:t_chunk):Pointer;
 begin
  Result:=AllocMem(t.size);
  if (ps4_pread(fd,Result,t.size,t.offset)<>t.size) then
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

 hdr:=Default(t_playgo_header);
 if (ps4_read(fd,@hdr,SizeOf(hdr))<>SizeOf(hdr)) then
 begin
  Writeln(StdErr,'Error read:',path);
  ps4_close(fd);
  Exit;
 end;

 if (hdr.magic<>PLAYGO_MAGIC) then
 begin
  Writeln(StdErr,'Invalid file:',path);
  ps4_close(fd);
  Exit;
 end;

 chunk_attrs       :=load_chunk(hdr.chunk_attrs);
 chunk_mchunks     :=load_chunk(hdr.chunk_mchunks);
 chunk_labels      :=load_chunk(hdr.chunk_labels);
 mchunk_attrs      :=load_chunk(hdr.mchunk_attrs);

 if (chunk_attrs=nil) or
    (chunk_mchunks=nil) or
    (chunk_labels=nil) or
    (mchunk_attrs=nil) then
 begin
  Writeln(StdErr,'Error read:',path);

  FreeMem(chunk_attrs  );
  FreeMem(chunk_mchunks);
  FreeMem(chunk_labels );
  FreeMem(mchunk_attrs );

  ps4_close(fd);
  Exit;
 end;

 Result:=TPlaygoFile.Create;
 Result.content_id:=hdr.content_id;

 if (hdr.chunk_count<>0) then
 begin
  SetLength(Result.chunks,hdr.chunk_count);

  For i:=0 to hdr.chunk_count-1 do
  begin

   Result.chunks[i].req_locus    :=chunk_attrs[i].req_locus;
   Result.chunks[i].language_mask:=chunk_attrs[i].language_mask;
   Result.chunks[i].label_name   :=PChar(chunk_labels+chunk_attrs[i].label_offset);

   total_size:=0;
   mchunk_count:=chunk_attrs[i].mchunk_count;
   if (mchunk_count<>0) then
   begin
    mchunks:=PWord(Pointer(chunk_mchunks)+chunk_attrs[i].mchunks_offset);
    For m:=0 to mchunk_count-1 do
    begin
     mchunk_id:=mchunks[m];

     total_size:=total_size+mchunk_attrs[mchunk_id].size.size;
    end;
   end;
   Result.chunks[i].total_size:=total_size;

  end;
 end;


 FreeMem(chunk_attrs  );
 FreeMem(chunk_mchunks);
 FreeMem(chunk_labels );
 FreeMem(mchunk_attrs );

 ps4_close(fd);
end;


end.

