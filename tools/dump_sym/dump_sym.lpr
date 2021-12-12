
uses
 {$IFDEF Linux}
 cmem,
 cthreads,
 {$ENDIF}
 Classes,sysutils,
 ps4_elf;

var
 ifname:RawByteString='';
 ofname:RawByteString='';
 sfname:RawByteString='';

 i,n:Integer;

 elf:Telf_file;
 F:THandle;

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
  Writeln('Tool for dump symbols from *.prx/*.sprx files');
  Writeln(' Parameters:');
  Writeln('  -i <name>  //file name to load prx/sprx');
  Writeln('  -o <name>  //file name to save txt');
  Writeln('  -s <name>  //file name to save prx');
  Exit;
 end;

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
    '-i':n:=0;
    '-o':n:=1;
    '-s':n:=2;
   else
     if (n<>-1) then
     begin
      Case n of
       0:ifname:=ParamStr(i);
       1:ofname:=ParamStr(i);
       2:sfname:=ParamStr(i);
      end;
      n:=-1;
     end;
  end;
 end;

 if not FileExists(ifname) then
 begin
  Writeln('File not found:',ifname);
  Writeln;
  Goto promo;
 end;

 elf:=Telf_file(LoadPs4ElfFromFile(ifname));
 elf.Prepare;

 if (sfname<>'') then
  elf.SavePs4ElfToFile(sfname);

 if (ofname='') then
 begin
  ofname:=ChangeFileExt(ifname,'.txt');
 end;

 F:=FileCreate(ofname);
 elf.DympSymbol(F);
 FileClose(F);
 FreeAndNil(elf);

 writeln;
end.



