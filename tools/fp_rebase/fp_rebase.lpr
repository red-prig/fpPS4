
{$mode ObjFPC}{$H+}

Uses
 SysUtils,
 coff64,
 fpdwarf;

var
 IMAGE_COFF64:TIMAGE_COFF64;

var
 DwarfDebugFile:TDwarfDebugFile;

function GetSectionByName(const name:RawByteString):TDwarfSectionInfo;
var
 section:PIMAGE_SECTION_HEADER;
begin
 Result:=Default(TDwarfSectionInfo);

 section:=IMAGE_COFF64.GetSectionByName(name);
 if (section=nil) then Exit;

 Result.RawData:=IMAGE_COFF64.RawToPtr(section^.PointerToRawData);
 Result.RawSize:=section^.SizeOfRawData;
end;

Procedure PrintSections(const IMAGE_COFF64:TIMAGE_COFF64);
var
 STR_TABLE:PAnsiChar;
 Name:PAnsiChar;
 NumberOfSections:DWORD;
 i:DWORD;
 O:DWORD;
begin
 Writeln('[Sections]');
 STR_TABLE:=IMAGE_COFF64.STR_TABLE;
 NumberOfSections:=IMAGE_COFF64.NT_HEADERS^.FileHeader.NumberOfSections;
 if (NumberOfSections<>0) then
 For i:=0 to NumberOfSections-1 do
 begin
  if (IMAGE_COFF64.SECTION_HEADER[i].Name[0]='/') then
  begin
   Name:=@IMAGE_COFF64.SECTION_HEADER[i].Name[1];
   O:=StrToDWordDef(Name,0);
   Name:=@STR_TABLE[O];
  end else
  begin
   Name:=@IMAGE_COFF64.SECTION_HEADER[i].Name;
  end;

  Writeln(i:2,' ',Name);
 end;
end;

Procedure PrintDirectotys(Base:Pointer;DataDirectory:PIMAGE_DATA_DIRECTORY;NumberOfRvaAndSizes:DWORD);
var
 i:DWORD;
begin
 Writeln('[DataDirectory]');
 if (NumberOfRvaAndSizes<>0) then
 For i:=0 to NumberOfRvaAndSizes-1 do
 begin
  Writeln(i,' ',HexStr(PtrUint(@DataDirectory[i])-PtrUint(Base),8),' ',HexStr(DataDirectory[i].VirtualAddress,8),' ',HexStr(DataDirectory[i].Size,8));
 end;
end;

Procedure PrintRelocs(VirtualAddress:DWORD;RELOC:PRELOC;Count:DWORD);
var
 i:DWORD;
 RawAddress:DWORD;
 VAddr:DWORD;
 RAddr:DWORD;
 Value:QWORD;
begin
 Writeln('[Relocs:',Count,']');
 RawAddress:=IMAGE_COFF64.VirtualToRaw(VirtualAddress);
 if (Count<>0) then
 For i:=0 to Count-1 do
 begin
  VAddr:=VirtualAddress+RELOC[i].ROffset;
  RAddr:=RawAddress    +RELOC[i].ROffset;

  Value:=0;
  case RELOC[i].RType of
   IMAGE_REL_BASED_DIR64:
     begin
      Value:=PQWORD(IMAGE_COFF64.RawToPtr(RAddr))^;
     end;
   else;
  end;

  Writeln(i:3,' 0x',HexStr(PWORD(RELOC)[i],4),' ',RELOC[i].RType:2,' ',HexStr(VAddr,8),' ',HexStr(RAddr,8),' ',HexStr(Value,16));

 end;
end;

Procedure PrintBaseRelocs(BASE_REL:PIMAGE_BASE_RELOCATION;Size:DWORD);
begin
 while (Size>=SizeOf(TIMAGE_BASE_RELOCATION)) do
 begin
  Writeln('BASE_REL.VirtualAddress:0x',HexStr(BASE_REL^.VirtualAddress,8));
  Writeln('BASE_REL.SizeOfBlock   :0x',HexStr(BASE_REL^.SizeOfBlock,8));

  PrintRelocs(BASE_REL^.VirtualAddress,Pointer(BASE_REL+1),(BASE_REL^.SizeOfBlock-SizeOf(TIMAGE_BASE_RELOCATION)) div 2);

  if (Size>BASE_REL^.SizeOfBlock) then
  begin
   Size:=Size-BASE_REL^.SizeOfBlock;
  end else
  begin
   Size:=0;
  end;

  BASE_REL:=Pointer(BASE_REL)+BASE_REL^.SizeOfBlock;
 end;
end;

////

type
 TCodePatcher=object
  ExeFilename : RawByteString;
  OutFilename : RawByteString;
  NewImageBase: QWORD;
  RebaseDwarf : Boolean;
  //
  ImageBase: QWORD;
  ImageSize: QWORD;
  Procedure Callback(P:PQWORD);
 end;

Procedure TCodePatcher.Callback(P:PQWORD);
Var
 Value:QWORD;
begin
 Value:=P^;
 if (Value>=ImageBase) and
    (Value<(ImageBase+ImageSize)) then
 begin
  Value:=Value-ImageBase+NewImageBase;
  //Writeln('=0x',HexStr(Value,16));

  //save
  P^:=Value;
 end;
end;

var
 CodePatcher:TCodePatcher;

////

function ParseCmd:Boolean;
var
 i,n:Integer;
 s:RawByteString;
label
 promo;
begin
 if (ParamCount=0) then
 begin
  promo:
  Writeln('[fp_rebase]');
  Writeln('Copyright (c) 2024 by red-prig');
  Writeln('Utility designed to rebase COFF64 executable files compiled on Free pascal');
  Writeln(' Parameters:');
  Writeln('   fp_rebase -rebase 0xADDR file [-dwarf] [-out file]');

  Writeln(' -rebase 0xADDR //set new image base addres (required)');
  Writeln(' file           //load file name            (required)');
  Writeln(' -dwarf         //rebase DWARF info         (optional)');
  Writeln(' -out file      //save file name            (optional)');

  Exit(False);
 end;

 CodePatcher:=Default(TCodePatcher);

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
   '-rebase':n:=0;
    '-dwarf':CodePatcher.RebaseDwarf:=True;
      '-out':n:=1;

   else
     begin
      Case n of
       -1:CodePatcher.ExeFilename:=ParamStr(i); //[ExeFilename]
        0:
          begin
           s:=ParamStr(i);
           if (Lowercase(copy(s,1,2))='0x') then
           begin
            s:='$'+copy(s,3);
           end;
           CodePatcher.NewImageBase:=StrToQWordDef(s,0);
          end;
        1:CodePatcher.OutFilename:=ParamStr(i);//[-out]

      end;
      n:=-1;
     end;
  end;
 end;

 if (CodePatcher.ExeFilename='') or
    (CodePatcher.NewImageBase=0) then
 begin
  goto promo;
 end;

 if (CodePatcher.OutFilename='') then
 begin
  CodePatcher.OutFilename:=CodePatcher.ExeFilename;
 end;

 Result:=True;
end;

var
 retry:Integer;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;

 if not ParseCmd then Exit;

 IMAGE_COFF64:=Default(TIMAGE_COFF64);

 retry:=0;
 repeat

  if IMAGE_COFF64.LoadFile(CodePatcher.ExeFilename) then
  begin
   Break;
  end else
  begin
   Inc(retry);
   if (retry>3) then
   begin
    Writeln(stderr,'Error load file:',CodePatcher.ExeFilename);
    Exit;
   end else
   begin
    Sleep(100);
   end;
  end;

 until false;

 if not IMAGE_COFF64.Init then Exit;

 CodePatcher.ImageBase:=IMAGE_COFF64.NT_HEADERS^.OptionalHeader.ImageBase;
 CodePatcher.ImageSize:=IMAGE_COFF64.NT_HEADERS^.OptionalHeader.SizeOfImage;

 //Writeln('REL_RAW:0x',HexStr(IMAGE_COFF64.REL_RAW,8));
 //Writeln('TLS_RAW:0x',HexStr(IMAGE_COFF64.TLS_RAW,8));

 //PrintBaseRelocs(IMAGE_COFF64.BASE_REL,IMAGE_COFF64.REL_DIRECTORY.Size);

 //PrintDirectotys(IMAGE_COFF64.DOS_HEADER,@IMAGE_COFF64.NT_HEADERS^.OptionalHeader.DataDirectory,IMAGE_COFF64.NT_HEADERS^.OptionalHeader.NumberOfRvaAndSizes);

 PrintSections(IMAGE_COFF64);

 IMAGE_COFF64.Calc(@CodePatcher.Callback);

 if CodePatcher.RebaseDwarf then
 begin
  DwarfDebugFile:=TDwarfDebugFile.Create;
  DwarfDebugFile.cb:=@CodePatcher.Callback;

  DwarfDebugFile.debug_abbrev :=GetSectionByName('.debug_abbrev');
  DwarfDebugFile.debug_info   :=GetSectionByName('.debug_info');
  DwarfDebugFile.debug_str    :=GetSectionByName('.debug_str');
  DwarfDebugFile.debug_line   :=GetSectionByName('.debug_line');
  DwarfDebugFile.debug_aranges:=GetSectionByName('.debug_aranges');

  DwarfDebugFile.LoadCompilationUnits();
  DwarfDebugFile.LoadArangesUnits();
 end;


 retry:=0;
 repeat

  if IMAGE_COFF64.SaveFile(CodePatcher.OutFilename) then
  begin
   Break;
  end else
  begin
   Inc(retry);
   if (retry>3) then
   begin
    Writeln(stderr,'Error save file:',CodePatcher.OutFilename);
    Exit;
   end else
   begin
    Sleep(100);
   end;
  end;

 until false;

 Writeln(stderr,'Saved:',CodePatcher.OutFilename);

end.

