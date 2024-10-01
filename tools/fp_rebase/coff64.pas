unit coff64;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils;

const
 IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;
 IMAGE_SIZEOF_SHORT_NAME = 8;

 IMAGE_DIRECTORY_ENTRY_BASERELOC=5;
 IMAGE_DIRECTORY_ENTRY_TLS      =9;

type
 PIMAGE_DOS_HEADER = ^TIMAGE_DOS_HEADER;
 TIMAGE_DOS_HEADER = packed record
  e_magic : WORD;
  e_cblp : WORD;
  e_cp : WORD;
  e_crlc : WORD;
  e_cparhdr : WORD;
  e_minalloc : WORD;
  e_maxalloc : WORD;
  e_ss : WORD;
  e_sp : WORD;
  e_csum : WORD;
  e_ip : WORD;
  e_cs : WORD;
  e_lfarlc : WORD;
  e_ovno : WORD;
  e_res : array[0..3] of WORD;
  e_oemid : WORD;
  e_oeminfo : WORD;
  e_res2 : array[0..9] of WORD;
  case boolean of
     true : (e_lfanew : longint);
     false: (_lfanew : longint); // delphi naming
 end;

 PIMAGE_FILE_HEADER = ^TIMAGE_FILE_HEADER;
 TIMAGE_FILE_HEADER = packed record
  Machine: WORD;
  NumberOfSections: WORD;
  TimeDateStamp: DWORD;
  PointerToSymbolTable: DWORD;
  NumberOfSymbols: DWORD;
  SizeOfOptionalHeader: WORD;
  Characteristics: WORD;
 end;

 PIMAGE_DATA_DIRECTORY = ^TIMAGE_DATA_DIRECTORY;
 TIMAGE_DATA_DIRECTORY = packed record
  VirtualAddress: DWORD;
  Size: DWORD;
 end;

 PIMAGE_OPTIONAL_HEADER64 = ^TIMAGE_OPTIONAL_HEADER64;
 TIMAGE_OPTIONAL_HEADER64 = packed record
  Magic: Word;
  MajorLinkerVersion: Byte;
  MinorLinkerVersion: Byte;
  SizeOfCode: DWORD;
  SizeOfInitializedData: DWORD;
  SizeOfUninitializedData: DWORD;
  AddressOfEntryPoint: DWORD;
  BaseOfCode: DWORD;
  ImageBase: Int64;
  SectionAlignment: DWORD;
  FileAlignment: DWORD;
  MajorOperatingSystemVersion: Word;
  MinorOperatingSystemVersion: Word;
  MajorImageVersion: Word;
  MinorImageVersion: Word;
  MajorSubsystemVersion: Word;
  MinorSubsystemVersion: Word;
  Win32VersionValue: DWORD;
  SizeOfImage: DWORD;
  SizeOfHeaders: DWORD;
  CheckSum: DWORD;
  Subsystem: Word;
  DllCharacteristics: Word;
  SizeOfStackReserve: Int64;
  SizeOfStackCommit: Int64;
  SizeOfHeapReserve: Int64;
  SizeOfHeapCommit: Int64;
  LoaderFlags: DWORD;
  NumberOfRvaAndSizes: DWORD;
  DataDirectory: array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TIMAGE_DATA_DIRECTORY;
 end;

 PIMAGE_NT_HEADERS64 = ^TIMAGE_NT_HEADERS64;
 TIMAGE_NT_HEADERS64 = packed record
  Signature: DWORD;
  FileHeader: TIMAGE_FILE_HEADER;
  OptionalHeader: TIMAGE_OPTIONAL_HEADER64;
 end;

 TImage_Section_SubHeader =packed record
  case longint of
   0 : ( PhysicalAddress : DWORD );
   1 : ( VirtualSize : DWORD );
 end;

 PIMAGE_SECTION_HEADER = ^TIMAGE_SECTION_HEADER;
 TIMAGE_SECTION_HEADER = packed record
  Name : array[0..(IMAGE_SIZEOF_SHORT_NAME)-1] of ANSICHAR;
  Misc : TImage_Section_SubHeader;
  VirtualAddress : DWORD;
  SizeOfRawData : DWORD;
  PointerToRawData : DWORD;
  PointerToRelocations : DWORD;
  PointerToLinenumbers : DWORD;
  NumberOfRelocations : WORD;
  NumberOfLinenumbers : WORD;
  Characteristics : DWORD;
 end;

 PIMAGE_TLS_DIRECTORY64 = ^TIMAGE_TLS_DIRECTORY64;
 TIMAGE_TLS_DIRECTORY64 =  packed record
  StartAddressOfRawData : QWORD;
  EndAddressOfRawData : QWORD;
  AddressOfIndex : QWORD;               { PDWORD }
  AddressOfCallBacks : QWORD;           { PIMAGE_TLS_CALLBACK *; }
  SizeOfZeroFill : DWORD;
  case longint of
   0 : ( Characteristics : DWORD );
   1 : ( CharacteristicsFields:  bitpacked record
          Reserved0 : 0..$FFFFF;  // 5 nibbles=20 bits
          Alignment : 0..$F;      // 4 bits
          Reserved1 : 0..$FF;     // 8 bits
         end );
 end;

 PIMAGE_BASE_RELOCATION = ^TIMAGE_BASE_RELOCATION;
 TIMAGE_BASE_RELOCATION = packed record
  VirtualAddress: DWORD;
  SizeOfBlock: DWORD;
 end;

 PSYMENT = ^TSYMENT;
 TSYMENT = packed record
  n_name  : array[0..7] of AnsiChar; // Symbol Name
  n_value : DWORD;                   // Value of Symbol
  n_scnum : WORD;                    // Section Number
  n_type  : WORD;                    // Symbol Type
  n_sclass: BYTE;                    // Storage Class
  n_numaux: BYTE;                    // Auxiliary Count
 end;

type
 PRELOC = ^TRELOC;
 TRELOC = bitpacked record
  ROffset:0..4095;
  RType  :0..15;
 end;

const
 IMAGE_REL_BASED_ABSOLUTE=0;  //SKIP
 IMAGE_REL_BASED_HIGH    =1;  //HIWORD
 IMAGE_REL_BASED_LOW     =2;  //LOWORD
 IMAGE_REL_BASED_HIGHLOW =3;  //DWORD
 IMAGE_REL_BASED_DIR64   =10; //QWORD

type
 TOnAdd64Link=procedure(P:PQWORD) of object;

 TIMAGE_COFF64=object
  DOS_HEADER    :PIMAGE_DOS_HEADER;
  FILE_SIZE     :PTRUINT;
  NT_HEADERS    :PIMAGE_NT_HEADERS64;
  SECTION_HEADER:PIMAGE_SECTION_HEADER;
  REL_DIRECTORY :TIMAGE_DATA_DIRECTORY;
  TLS_DIRECTORY :TIMAGE_DATA_DIRECTORY;
  STR_TABLE     :PAnsiChar;
  BASE_REL      :PIMAGE_BASE_RELOCATION;
  BASE_TLS      :PIMAGE_TLS_DIRECTORY64;
  STR_RAW       :DWORD;
  REL_RAW       :DWORD;
  TLS_RAW       :DWORD;
  Procedure FreeFile;
  function  LoadFile(const Filename:RawByteString):Boolean;
  function  SaveFile(const Filename:RawByteString):Boolean;
  Function  Init:Boolean;
  Function  RawToPtr(Raw:QWORD):Pointer;
  Function  VirtualToRaw(VirtualAddress:DWORD):DWORD;
  Function  GetSectionByName(const name:RawByteString):PIMAGE_SECTION_HEADER;
  Procedure CalcRelocs(cb:TOnAdd64Link;VirtualAddress:DWORD;RELOC:PRELOC;Count:DWORD);
  Procedure CalcBaseRelocs(cb:TOnAdd64Link);
  Procedure Calc(cb:TOnAdd64Link);
 end;


implementation

Procedure TIMAGE_COFF64.FreeFile;
begin
 if (DOS_HEADER<>nil) then
 begin
  FreeMem(DOS_HEADER);
  DOS_HEADER:=nil;
 end;
end;

function TIMAGE_COFF64.LoadFile(const Filename:RawByteString):Boolean;
var
 F:THandle;
begin
 Result:=False;
 if not FileExists(Filename) then Exit;

 F:=FileOpen(Filename,fmOpenRead);
 if (F=feInvalidHandle) then Exit;

 FILE_SIZE:=FileSeek(F,0,fsFromEnd);
 FileSeek(F,0,fsFromBeginning);

 DOS_HEADER:=GetMem(FILE_SIZE);

 Result:=FileRead(F,DOS_HEADER^,FILE_SIZE)=FILE_SIZE;

 FileClose(F);
end;

function TIMAGE_COFF64.SaveFile(const Filename:RawByteString):Boolean;
var
 F:THandle;
begin
 Result:=False;

 F:=FileCreate(Filename);
 if (F=feInvalidHandle) then Exit;

 Result:=FileWrite(F,DOS_HEADER^,FILE_SIZE)=FILE_SIZE;

 FileClose(F);
end;

Function TIMAGE_COFF64.Init:Boolean;
begin
 Result:=False;

 if (DOS_HEADER^.e_magic<>$5a4d) then
 begin
  Writeln(StdErr,'Wrong DOS.e_magic:0x',HexStr(DOS_HEADER^.e_magic,4));
  Exit;
 end;

 NT_HEADERS:=RawToPtr(DOS_HEADER^.e_lfanew);

 if (NT_HEADERS^.Signature<>$4550) then
 begin
  Writeln(StdErr,'Wrong NT.Signature:0x',HexStr(NT_HEADERS^.Signature,8));
  Exit;
 end;

 if (NT_HEADERS^.FileHeader.Machine<>$8664) then
 begin
  Writeln(StdErr,'Wrong NT.FileHeader.Machine:0x',HexStr(NT_HEADERS^.FileHeader.Machine,4));
  Exit;
 end;

 if (NT_HEADERS^.OptionalHeader.Magic<>$020B) then
 begin
  Writeln(StdErr,'Wrong NT.OptionalHeader.Magic:0x',HexStr(NT_HEADERS^.OptionalHeader.Magic,4));
  Exit;
 end;

 REL_DIRECTORY:=NT_HEADERS^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];
 TLS_DIRECTORY:=NT_HEADERS^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_TLS];

 SECTION_HEADER:=Pointer(@NT_HEADERS^.OptionalHeader)+NT_HEADERS^.FileHeader.SizeOfOptionalHeader;

 STR_RAW:=NT_HEADERS^.FileHeader.PointerToSymbolTable + NT_HEADERS^.FileHeader.NumberOfSymbols*SizeOf(TSYMENT);

 STR_TABLE:=RawToPtr(STR_RAW);

 REL_RAW:=VirtualToRaw(REL_DIRECTORY.VirtualAddress);
 TLS_RAW:=VirtualToRaw(TLS_DIRECTORY.VirtualAddress);

 BASE_REL:=RawToPtr(REL_RAW);
 BASE_TLS:=RawToPtr(TLS_RAW);

 Result:=True;
end;

Function TIMAGE_COFF64.RawToPtr(Raw:QWORD):Pointer;
begin
 Result:=Pointer(DOS_HEADER)+Raw;
end;

Function TIMAGE_COFF64.VirtualToRaw(VirtualAddress:DWORD):DWORD;
var
 NumberOfSections:DWORD;
 i:DWORD;
 D:DWORD;
begin
 Result:=0;
 NumberOfSections:=NT_HEADERS^.FileHeader.NumberOfSections;
 if (NumberOfSections<>0) then
 For i:=0 to NumberOfSections-1 do
 begin
  if (VirtualAddress>=SECTION_HEADER[i].VirtualAddress) and
     (VirtualAddress<SECTION_HEADER[i].VirtualAddress+SECTION_HEADER[i].SizeOfRawData) then
  begin
   D:=VirtualAddress-SECTION_HEADER[i].VirtualAddress;
   Exit(SECTION_HEADER[i].PointerToRawData+D);
  end;
 end;
end;

Function TIMAGE_COFF64.GetSectionByName(const name:RawByteString):PIMAGE_SECTION_HEADER;
var
 SName:PAnsiChar;
 NumberOfSections:DWORD;
 i:DWORD;
 O:DWORD;
begin
 NumberOfSections:=NT_HEADERS^.FileHeader.NumberOfSections;
 if (NumberOfSections<>0) then
 For i:=0 to NumberOfSections-1 do
 begin
  if (SECTION_HEADER[i].Name[0]='/') then
  begin
   SName:=@SECTION_HEADER[i].Name[1];
   O:=StrToDWordDef(SName,0);
   SName:=@STR_TABLE[O];
  end else
  begin
   SName:=@SECTION_HEADER[i].Name;
  end;

  if (SName=name) then
  begin
   Exit(@SECTION_HEADER[i]);
  end;

 end;
end;

Procedure TIMAGE_COFF64.CalcRelocs(cb:TOnAdd64Link;VirtualAddress:DWORD;RELOC:PRELOC;Count:DWORD);
var
 i:DWORD;
 RawAddress:DWORD;
 RAddr:DWORD;
begin
 if (cb=nil) then Exit;
 RawAddress:=VirtualToRaw(VirtualAddress);
 if (Count<>0) then
 For i:=0 to Count-1 do
 begin

  case RELOC[i].RType of
   IMAGE_REL_BASED_DIR64:
     begin
      RAddr:=RawAddress+RELOC[i].ROffset;
      //
      cb(RawToPtr(RAddr));
     end;
   else;
  end;

 end;
end;

Procedure TIMAGE_COFF64.CalcBaseRelocs(cb:TOnAdd64Link);
var
 i,Size:DWORD;
 REL:PIMAGE_BASE_RELOCATION;
begin
 if (cb=nil) then Exit;
 if (BASE_REL=nil) then Exit;
 Size:=REL_DIRECTORY.Size;
 REL:=BASE_REL;
 while (Size>=SizeOf(TIMAGE_BASE_RELOCATION)) do
 begin

  if (Size>=REL^.SizeOfBlock) then
  begin
   i:=REL^.SizeOfBlock;
  end else
  begin
   i:=Size;
  end;

  if (i>SizeOf(TIMAGE_BASE_RELOCATION)) then
  begin
   CalcRelocs(cb,REL^.VirtualAddress,Pointer(REL+1),(i-SizeOf(TIMAGE_BASE_RELOCATION)) div 2);
  end;

  Size:=Size-i;

  REL:=Pointer(REL)+i;
 end;
end;

Procedure TIMAGE_COFF64.Calc(cb:TOnAdd64Link);
begin
 if (cb=nil) then Exit;
 if (NT_HEADERS=nil) then Exit;

 cb(@NT_HEADERS^.OptionalHeader.ImageBase);

 if (BASE_TLS<>nil) then
 begin
  cb(@BASE_TLS^.StartAddressOfRawData);
  cb(@BASE_TLS^.EndAddressOfRawData  );
  cb(@BASE_TLS^.AddressOfIndex       );
  cb(@BASE_TLS^.AddressOfCallBacks   );
 end;

 CalcBaseRelocs(cb);
end;

end.

