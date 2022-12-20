
uses
 {$IFDEF Linux}
 cmem,
 cthreads,
 {$ENDIF}
 Classes,
 sysutils;

const
 PLAYGO_CONTENT_ID_SIZE=$30;

 PLAYGO_MAX_IMAGES          =2;
 PLAYGO_MAX_DISCS           =2;
 PLAYGO_MAX_LAYERS_FOR_DISC1=2;
 PLAYGO_MAX_LAYERS_FOR_DISC2=1;
 PLAYGO_MAX_CHUNKS          =1000;
 PLAYGO_MAX_MCHUNKS         =8000;
 PLAYGO_MAX_SCENARIOS       =32;
 PLAYGO_MAX_LANGUAGES       =64;

 PLAYGO_SCENARIO_TYPE_SP=1;
 PLAYGO_SCENARIO_TYPE_MP=2;

 PLAYGO_SCENARIO_TYPE_USER_00=(16+1);

 PLAYGO_ALL_LANGUAGES_MASK:QWORD=$FFFFFFFFFFFFFFFF;

 PLAYGO_LANG_JAPANESE            = 0;
 PLAYGO_LANG_ENGLISH_US          = 1;
 PLAYGO_LANG_FRENCH              = 2;
 PLAYGO_LANG_SPANISH             = 3;
 PLAYGO_LANG_GERMAN              = 4;
 PLAYGO_LANG_ITALIAN             = 5;
 PLAYGO_LANG_DUTCH               = 6;
 PLAYGO_LANG_PORTUGUESE_PT       = 7;
 PLAYGO_LANG_RUSSIAN             = 8;
 PLAYGO_LANG_KOREAN              = 9;
 PLAYGO_LANG_CHINESE_TRADITIONAL = 10;
 PLAYGO_LANG_CHINESE_SIMPLIFIED  = 11;
 PLAYGO_LANG_FINNISH             = 12;
 PLAYGO_LANG_SWEDISH             = 13;
 PLAYGO_LANG_DANISH              = 14;
 PLAYGO_LANG_NORWEGIAN           = 15;
 PLAYGO_LANG_POLISH              = 16;
 PLAYGO_LANG_PORTUGUESE_BR       = 17;
 PLAYGO_LANG_ENGLISH_UK          = 18;
 PLAYGO_LANG_TURKISH             = 19;
 PLAYGO_LANG_SPANISH_LA          = 20;
 PLAYGO_LANG_ARABIC              = 21;
 PLAYGO_LANG_FRENCH_CA           = 22;

 PLAYGO_LANG_USER_00 = 48;
 PLAYGO_LANG_USER_01 = 49;
 PLAYGO_LANG_USER_02 = 50;
 PLAYGO_LANG_USER_03 = 51;
 PLAYGO_LANG_USER_04 = 52;
 PLAYGO_LANG_USER_05 = 53;
 PLAYGO_LANG_USER_06 = 54;
 PLAYGO_LANG_USER_07 = 55;
 PLAYGO_LANG_USER_08 = 56;
 PLAYGO_LANG_USER_09 = 57;
 PLAYGO_LANG_USER_10 = 58;
 PLAYGO_LANG_USER_11 = 59;
 PLAYGO_LANG_USER_12 = 60;
 PLAYGO_LANG_USER_13 = 61;
 PLAYGO_LANG_USER_14 = 62;

 PLAYGO_LOCUS_NOT_DOWNLOADED = 0;
 PLAYGO_LOCUS_LOCAL_SLOW     = 2;
 PLAYGO_LOCUS_LOCAL_FAST     = 3;

 PLAYGO_INSTALL_SPEED_SUSPENDED = 0;
 PLAYGO_INSTALL_SPEED_TRICKLE   = 1;
 PLAYGO_INSTALL_SPEED_FULL      = 2;

 PLAYGO_MAGIC=$6F676C70;

 PLAYGO_HEADER_SIZE=$100;
 PLAYGO_SCENARIO_ATTRIBUTE_ENTRY_SIZE=$20;
 PLAYGO_CHUNK_ATTRIBUTE_ENTRY_SIZE   =$20;
 PLAYGO_MCHUNK_ATTRIBUTE_ENTRY_SIZE  =$10;

type
 t_playgo_lang_desc=packed record
  code:DWORD;
  name:PChar;
  iso1:PChar;
  iso2:PChar;
 end;

 p_playgo_scenario_attr_desc=^t_playgo_scenario_attr_desc;
 t_playgo_scenario_attr_desc=packed record
  _type              :DWORD;
  initial_chunk_count:DWORD;
  chunk_count        :DWORD;
  _label:PChar;
  chunks:PWord;
 end;

 p_playgo_chunk_attr_desc=^t_playgo_chunk_attr_desc;
 t_playgo_chunk_attr_desc=packed record
  flag        :DWORD;
  disc_no     :DWORD;
  layer_no    :DWORD;
  image_no    :DWORD;
  req_locus   :DWORD;
  mchunk_count:DWORD;
  language_mask:QWORD;
  _label:PChar;
  mchunks:PWord;
 end;

 p_playgo_mchunk_attr_desc=^t_playgo_mchunk_attr_desc;
 t_playgo_mchunk_attr_desc=packed record
  offset:QWORD;
  size:QWORD;
  image_no:DWORD;
 end;

 t_playgo=packed record
  version_major:Word;
  version_minor:Word;

  file_size:DWORD;

  sdk_version:DWORD;
  attrib     :WORD;

  image_count   :WORD;
  chunk_count   :WORD;
  mchunk_count  :WORD;
  scenario_count:WORD;
  disc_count    :WORD;

  layer_bmp         :WORD;
  default_scenario_id:WORD;

  content_id:array[0..PLAYGO_CONTENT_ID_SIZE] of AnsiChar;

  scenario_attrs    :p_playgo_scenario_attr_desc;
  chunk_attrs       :p_playgo_chunk_attr_desc;
  mchunk_attrs      :p_playgo_mchunk_attr_desc;
  inner_mchunk_attrs:p_playgo_mchunk_attr_desc;
 end;

 bits2 =0..((1 shl  2)-1);
 bits4 =0..((1 shl  4)-1);
 bits48=0..((1 shl 48)-1);

 t_playgo_chunk_loc=bitpacked record
  offset:bits48;
  _align1:Byte;
  image_no:bits4;
  _align2:bits4;
 end;

 t_playgo_chunk_size=bitpacked record
  size:bits48;
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
  layer_no:bits2;
  disc_no :bits2;
  image_no:bits4;
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

const
 s_languages:array[0..37] of t_playgo_lang_desc=(
  (code:PLAYGO_LANG_JAPANESE           ;name:'Japanese'                 ;iso1:'ja'     ;iso2:nil),
  (code:PLAYGO_LANG_ENGLISH_US         ;name:'English'                  ;iso1:'en-US'  ;iso2:'en'),
  (code:PLAYGO_LANG_FRENCH             ;name:'French'                   ;iso1:'fr'     ;iso2:nil),
  (code:PLAYGO_LANG_SPANISH            ;name:'Spanish'                  ;iso1:'es-ES'  ;iso2:'es'),
  (code:PLAYGO_LANG_GERMAN             ;name:'German'                   ;iso1:'de'     ;iso2:nil),
  (code:PLAYGO_LANG_ITALIAN            ;name:'Italian'                  ;iso1:'it'     ;iso2:nil),
  (code:PLAYGO_LANG_DUTCH              ;name:'Dutch'                    ;iso1:'nl'     ;iso2:nil),
  (code:PLAYGO_LANG_PORTUGUESE_PT      ;name:'Portuguese'               ;iso1:'pt-PT'  ;iso2:'pt'),
  (code:PLAYGO_LANG_RUSSIAN            ;name:'Russian'                  ;iso1:'ru'     ;iso2:nil),
  (code:PLAYGO_LANG_KOREAN             ;name:'Korean'                   ;iso1:'ko'     ;iso2:nil),
  (code:PLAYGO_LANG_CHINESE_TRADITIONAL;name:'Trad.Chinese'             ;iso1:'zh-Hant';iso2:nil),
  (code:PLAYGO_LANG_CHINESE_SIMPLIFIED ;name:'Simp.Chinese'             ;iso1:'zh-Hans';iso2:nil),
  (code:PLAYGO_LANG_FINNISH            ;name:'Finnish'                  ;iso1:'fi'     ;iso2:nil),
  (code:PLAYGO_LANG_SWEDISH            ;name:'Swedish'                  ;iso1:'sv'     ;iso2:nil),
  (code:PLAYGO_LANG_DANISH             ;name:'Danish'                   ;iso1:'da'     ;iso2:nil),
  (code:PLAYGO_LANG_NORWEGIAN          ;name:'Norwegian'                ;iso1:'no'     ;iso2:nil),
  (code:PLAYGO_LANG_POLISH             ;name:'Polish'                   ;iso1:'pl'     ;iso2:nil),
  (code:PLAYGO_LANG_PORTUGUESE_BR      ;name:'Braz.Portuguese'          ;iso1:'pt-BR'  ;iso2:nil),
  (code:PLAYGO_LANG_ENGLISH_UK         ;name:'UK English'               ;iso1:'en-GB'  ;iso2:nil),
  (code:PLAYGO_LANG_TURKISH            ;name:'Turkish'                  ;iso1:'tr'     ;iso2:nil),
  (code:PLAYGO_LANG_SPANISH_LA         ;name:'Latin American Spanish'   ;iso1:'es-LA'  ;iso2:nil),
  (code:PLAYGO_LANG_ARABIC             ;name:'Arabic'                   ;iso1:'ar'     ;iso2:nil),
  (code:PLAYGO_LANG_FRENCH_CA          ;name:'Canadian French'          ;iso1:'fr-CA'  ;iso2:nil),

  (code:PLAYGO_LANG_USER_00            ;name:'User-defined Language #0' ;iso1:'user00' ;iso2:nil),
  (code:PLAYGO_LANG_USER_01            ;name:'User-defined Language #1' ;iso1:'user01' ;iso2:nil),
  (code:PLAYGO_LANG_USER_02            ;name:'User-defined Language #2' ;iso1:'user02' ;iso2:nil),
  (code:PLAYGO_LANG_USER_03            ;name:'User-defined Language #3' ;iso1:'user03' ;iso2:nil),
  (code:PLAYGO_LANG_USER_04            ;name:'User-defined Language #4' ;iso1:'user04' ;iso2:nil),
  (code:PLAYGO_LANG_USER_05            ;name:'User-defined Language #5' ;iso1:'user05' ;iso2:nil),
  (code:PLAYGO_LANG_USER_06            ;name:'User-defined Language #6' ;iso1:'user06' ;iso2:nil),
  (code:PLAYGO_LANG_USER_07            ;name:'User-defined Language #7' ;iso1:'user07' ;iso2:nil),
  (code:PLAYGO_LANG_USER_08            ;name:'User-defined Language #8' ;iso1:'user08' ;iso2:nil),
  (code:PLAYGO_LANG_USER_09            ;name:'User-defined Language #9' ;iso1:'user09' ;iso2:nil),
  (code:PLAYGO_LANG_USER_10            ;name:'User-defined Language #10';iso1:'user10' ;iso2:nil),
  (code:PLAYGO_LANG_USER_11            ;name:'User-defined Language #11';iso1:'user11' ;iso2:nil),
  (code:PLAYGO_LANG_USER_12            ;name:'User-defined Language #12';iso1:'user12' ;iso2:nil),
  (code:PLAYGO_LANG_USER_13            ;name:'User-defined Language #13';iso1:'user13' ;iso2:nil),
  (code:PLAYGO_LANG_USER_14            ;name:'User-defined Language #14';iso1:'user14' ;iso2:nil)
 );

function get_str_t_chunk(t:t_chunk):RawByteString;
begin
 Result:='offset:0x'+HexStr(t.offset,8)+' size:0x'+HexStr(t.size,8);
end;

function get_str_scenario_type(_type:Byte):RawByteString;
begin
 Case _type of
  PLAYGO_SCENARIO_TYPE_SP:Result:='Singleplayer';
  PLAYGO_SCENARIO_TYPE_MP:Result:='Multiplayer';
  PLAYGO_SCENARIO_TYPE_USER_00..$FF:
    begin
     Result:='User-defined Scenario #'+IntTostr(_type-PLAYGO_SCENARIO_TYPE_USER_00+1);
    end;
  else
    Result:=IntToStr(_type);
 end;
end;

function get_str_req_locus(req_locus:Byte):RawByteString;
begin
 Case req_locus of
  PLAYGO_LOCUS_NOT_DOWNLOADED:Result:='Not downloaded';
  PLAYGO_LOCUS_LOCAL_SLOW    :Result:='Slow';
  PLAYGO_LOCUS_LOCAL_FAST    :Result:='Fast';
  else
   Result:=IntToStr(req_locus);
 end;
end;

function LoadPlaygoFile(Const name:RawByteString):Boolean;
Var
 F:THandle;

 i,count:DWORD;
 x,ofs,chunk_count:DWORD;

 hdr:t_playgo_header;

 scenario_attrs:p_playgo_scenario_attr_entry;
 scenario_chunks:PWord;
 scenario_labels:PChar;

 chunk_attrs:p_playgo_chunk_attr_entry;
 chunk_mchunks:PWord;
 chunk_labels:PChar;

 mchunk_attrs:p_playgo_mchunk_attr_entry;
 inner_mchunk_attrs:p_playgo_mchunk_attr_entry;

 function load_chunk(t:t_chunk):Pointer;
 begin
  Result:=AllocMem(t.size);
  FileSeek(F,t.offset,fsFromBeginning);
  FileRead(F,Result^,t.size);
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

 hdr:=Default(t_playgo_header);

 FileRead(F,hdr,SizeOf(hdr));

 Writeln('magic              :0x',HexStr(hdr.magic,8));

 if (hdr.magic<>PLAYGO_MAGIC) then
 begin
  Writeln('Invalid playgo file:',name);
  FileClose(F);
  Exit;
 end;

 Writeln('version            :0x',HexStr((hdr.version_major),4),'.0x',HexStr(hdr.version_minor,4));
 Writeln('image_count        :',hdr.image_count              );
 Writeln('chunk_count        :',hdr.chunk_count              );
 Writeln('mchunk_count       :',hdr.mchunk_count             );
 Writeln('scenario_count     :',hdr.scenario_count           );
 Writeln('file_size          :',hdr.file_size                );
 Writeln('default_scenario_id:',hdr.default_scenario_id      );
 Writeln('attrib             :',hdr.attrib                   );
 Writeln('sdk_version        :0x',HexStr(hdr.sdk_version,8)  );
 Writeln('disc_count         :',hdr.disc_count               );
 Writeln('layer_bmp          :',hdr.layer_bmp                );
 Writeln('content_id         :',hdr.content_id               );
 Writeln('chunk_attrs        :',get_str_t_chunk(hdr.chunk_attrs       ));
 Writeln('chunk_mchunks      :',get_str_t_chunk(hdr.chunk_mchunks     ));
 Writeln('chunk_labels       :',get_str_t_chunk(hdr.chunk_labels      ));
 Writeln('mchunk_attrs       :',get_str_t_chunk(hdr.mchunk_attrs      ));
 Writeln('scenario_attrs     :',get_str_t_chunk(hdr.scenario_attrs    ));
 Writeln('scenario_chunks    :',get_str_t_chunk(hdr.scenario_chunks   ));
 Writeln('scenario_labels    :',get_str_t_chunk(hdr.scenario_labels   ));
 Writeln('inner_mchunk_attrs :',get_str_t_chunk(hdr.inner_mchunk_attrs));

 scenario_attrs    :=load_chunk(hdr.scenario_attrs);
 scenario_chunks   :=load_chunk(hdr.scenario_chunks);
 scenario_labels   :=load_chunk(hdr.scenario_labels);

 chunk_attrs       :=load_chunk(hdr.chunk_attrs);;
 chunk_mchunks     :=load_chunk(hdr.chunk_mchunks);
 chunk_labels      :=load_chunk(hdr.chunk_labels);

 mchunk_attrs      :=load_chunk(hdr.mchunk_attrs);
 inner_mchunk_attrs:=load_chunk(hdr.inner_mchunk_attrs);

 //

 count:=hdr.scenario_attrs.size div SizeOf(t_playgo_scenario_attr_entry);
 if (count<>0) then
 begin
  For i:=0 to count-1 do
  begin
   Writeln('[scenario_attrs]:',i);
   Writeln(' type               :',get_str_scenario_type(scenario_attrs[i]._type));
   Writeln(' initial_chunk_count:',scenario_attrs[i].initial_chunk_count);
   Writeln(' chunk_count        :',scenario_attrs[i].chunk_count        );
   Writeln(' chunks_offset      :',scenario_attrs[i].chunks_offset      );
   Writeln(' label_offset       :',scenario_attrs[i].label_offset       );
   Writeln(' label              :',PChar(scenario_labels+scenario_attrs[i].label_offset));

   chunk_count:=scenario_attrs[i].chunk_count;
   if (chunk_count<>0) then
   begin
    ofs:=scenario_attrs[i].chunks_offset;
    For x:=0 to chunk_count-1 do
    begin
     Writeln(' [chunk]:',PWord(Pointer(scenario_chunks)+ofs)[x]);
    end;
   end;

  end;
 end;

 //

 count:=hdr.chunk_attrs.size div SizeOf(t_playgo_chunk_attr_entry);
 if (count<>0) then
 begin
  For i:=0 to count-1 do
  begin
   Writeln('[chunk_attrs]:',i);
   Writeln(' flag          :0x',HexStr(chunk_attrs[i].flag,2));
   Writeln(' image_no      :',chunk_attrs[i].image_disc_layer_no.image_no);
   Writeln(' disc_no       :',chunk_attrs[i].image_disc_layer_no.disc_no);
   Writeln(' layer_no      :',chunk_attrs[i].image_disc_layer_no.layer_no);
   Writeln(' req_locus     :',get_str_req_locus(chunk_attrs[i].req_locus));
   Writeln(' mchunk_count  :',chunk_attrs[i].mchunk_count       );
   Writeln(' language_mask :0x',HexStr(chunk_attrs[i].language_mask,16));
   Writeln(' mchunks_offset:',chunk_attrs[i].mchunks_offset     );
   Writeln(' label_offset  :',chunk_attrs[i].label_offset       );
   Writeln(' label         :',PChar(chunk_labels+chunk_attrs[i].label_offset));

   chunk_count:=chunk_attrs[i].mchunk_count;
   if (chunk_count<>0) then
   begin
    ofs:=chunk_attrs[i].mchunks_offset;
    For x:=0 to chunk_count-1 do
    begin
     Writeln(' [mchunk]:',PWord(Pointer(chunk_mchunks)+ofs)[x]);
    end;
   end;

  end;
 end;

 count:=hdr.mchunk_attrs.size div SizeOf(t_playgo_mchunk_attr_entry);
 if (count<>0) then
 begin
  For i:=0 to count-1 do
  begin
   Writeln('[mchunk_attrs]:',i);
   Writeln(' image_no:',mchunk_attrs[i].loc.image_no);
   Writeln(' offset  :',mchunk_attrs[i].loc.offset);
   Writeln(' size    :',mchunk_attrs[i].size.size);
  end;
 end;

 count:=hdr.inner_mchunk_attrs.size div SizeOf(t_playgo_mchunk_attr_entry);
 if (count<>0) then
 begin
  For i:=0 to count-1 do
  begin
   Writeln('[inner_mchunk_attrs]:',i);
   Writeln(' offset  :',inner_mchunk_attrs[i].loc.offset);
   Writeln(' size    :',inner_mchunk_attrs[i].size.size);
  end;
 end;

 FreeMem(scenario_attrs    );
 FreeMem(scenario_chunks   );
 FreeMem(scenario_labels   );

 FreeMem(chunk_attrs       );
 FreeMem(chunk_mchunks     );
 FreeMem(chunk_labels      );

 FreeMem(mchunk_attrs      );
 FreeMem(inner_mchunk_attrs);

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
  Writeln('Tool for print playgo data');
  Writeln(' Parameters:');
  Writeln('  -i <name>  //file name to load playgo-chunk.dat');
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
       0:LoadPlaygoFile(ParamStr(i));
      end;
      n:=-1;
     end;
  end;
 end;

 writeln;
end.



