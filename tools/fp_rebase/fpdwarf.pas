unit fpdwarf;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 fpdbgdwarfconst;

type
 TOnAdd64Link=procedure(P:PQWORD) of object;

 TDwarfSectionInfo = record
  RawData: Pointer;
  RawSize: QWord;
 end;

 TDwarfDebugFile = class
  debug_abbrev :TDwarfSectionInfo;
  debug_info   :TDwarfSectionInfo;
  debug_str    :TDwarfSectionInfo;
  debug_line   :TDwarfSectionInfo;
  debug_aranges:TDwarfSectionInfo;
  //

  cb:TOnAdd64Link;

  function  GetStrEntryDataForForm(var AEntryData: Pointer; AForm: Cardinal; AddrSize: Byte; IsDwarf64: boolean; Version: word; Var str:RawByteString): Boolean;

  Procedure AddCompilationUnit(ADataOffset: QWord;
                               ALength: QWord;
                               AVersion: Word;
                               AAbbrevOffset: QWord;
                               AAddressSize: Byte;
                               AIsDwarf64: Boolean);

  Procedure LoadCompilationUnits();

  Procedure AddArange(data_offset  : QWord;
                      data_length  : QWord;
                      address_size : Byte;
                      segment_size : Byte);

  Procedure LoadArangesUnits();
 end;

type
 TDwarfAbbrevEntry = record
  Attribute: Cardinal;
  Form     : Cardinal;
  Parent   : Cardinal;
 end;
 PDwarfAbbrevEntry = ^TDwarfAbbrevEntry;

 {$PACKRECORDS 1}

 TDwarfAbbrev = record
  tag: Cardinal;
  abbrev: Integer;
  index: Integer;
  count: Integer;
  HasChildren: Integer;
  Ptr:Pointer;
 end;
 PDwarfAbbrev = ^TDwarfAbbrev;

 {%region Dwarf Header Structures }
   // compilation unit header
   // In version 5 of the Dwarf-specification, the header has been changed.
 PDwarfCUHeader32 = ^TDwarfCUHeader32;
 TDwarfCUHeader32 = record
   Length: LongWord;
   Version: Word;
   AbbrevOffset: LongWord;
   AddressSize: Byte;
 end;

 PDwarfCUHeader32v5 = ^TDwarfCUHeader32v5;
 TDwarfCUHeader32v5 = record
   Length: LongWord;
   Version: Word;
   unit_type: Byte;
   AddressSize: Byte;
   AbbrevOffset: LongWord;
 end;

 PDwarfCUHeader64 = ^TDwarfCUHeader64;
 TDwarfCUHeader64 = record
   Signature: LongWord;
   Length: QWord;
   Version: Word;
   AbbrevOffset: QWord;
   AddressSize: Byte;
 end;

 PDwarfCUHeader64v5 = ^TDwarfCUHeader64v5;
 TDwarfCUHeader64v5 = record
   Signature: LongWord;
   Length: QWord;
   Version: Word;
   unit_type: Byte;
   AddressSize: Byte;
   AbbrevOffset: QWord;
 end;

 // Line number program header
 PDwarfLNPInfoHeader = ^TDwarfLNPInfoHeader;
 TDwarfLNPInfoHeader = record
   MinimumInstructionLength: Byte;
   //MaximumInstructionLength: Byte; // Version 4 and up
   DefaultIsStmt: Byte;
   LineBase: ShortInt;
   LineRange: Byte;
   OpcodeBase: Byte;
   StandardOpcodeLengths: record end; {array[1..OpcodeBase-1] of Byte}
   {IncludeDirectories: asciiz, asciiz..z}
   {FileNames: asciiz, asciiz..z}
 end;

 PDwarfLNPHeader32 = ^TDwarfLNPHeader32;
 TDwarfLNPHeader32 = record
   UnitLength: LongWord;
   Version: Word;
   HeaderLength: LongWord;
   Info: TDwarfLNPInfoHeader;
 end;

 PDwarfLNPHeader64 = ^TDwarfLNPHeader64;
 TDwarfLNPHeader64 = record
   Signature: LongWord;
   UnitLength: QWord;
   Version: Word;
   HeaderLength: QWord;
   Info: TDwarfLNPInfoHeader;
 end;

   {$PACKRECORDS C}
 {%endregion Dwarf Header Structures }

function ULEB128toOrdinal(var p: PByte): QWord;
function SLEB128toOrdinal(var p: PByte): Int64;

function ReadByte(var AEntryData:Pointer):Byte;
function ReadWord(var AEntryData:Pointer):Word;
function ReadDWORD(var AEntryData:Pointer):DWORD;
function ReadQWORD(var AEntryData:Pointer):QWORD;
function ReadHex(var AEntryData:Pointer;AddrSize:Byte):RawByteString;
function ReadHexArray(var AEntryData:Pointer;AddrSize:Byte):RawByteString;
function ReadString(var AEntryData:Pointer):RawByteString;

function SkipEntryDataForForm(var AEntryData: Pointer; AForm: Cardinal; AddrSize: Byte; IsDwarf64: boolean; Version: word): Boolean;

type
 TDwarfAbbrevList=object
  FAbbrevList :array of TDwarfAbbrev;
  FDefinitions:array of TDwarfAbbrevEntry;
  function  AddAttrib(attrib,Form,Parent:Cardinal):DWORD;
  function  AddAbbrev(const D:TDwarfAbbrev):DWORD;
  function  FindAbbrevFromPointer(P:Pointer):PDwarfAbbrev;
  function  FindAbbrevFromId(abbrev:Integer):PDwarfAbbrev;
  Procedure LoadAbbrevs(ptr_beg,ptr_end:Pointer);
 end;

type
 TFLineInfo = record
   Header: Pointer;
   DataStart: Pointer;
   DataEnd: Pointer;

   Valid: Boolean;
   Version: Word;
   Addr64: Boolean;
   AddrSize: Byte;
   MinimumInstructionLength: Byte;
   MaximumInstructionLength: Byte; // Dwarf 4
   DefaultIsStmt: Boolean;
   LineBase: ShortInt;
   LineRange: Byte;
   StandardOpcodeLengths: array of Byte; //record end; {array[1..OpcodeBase-1] of Byte}
   //Directories: TStringList;
   //FileNames: TStringList;
   // the line info is build incrementy when needed
   //StateMachine: TDwarfLineInfoStateMachine;
   //StateMachines: TFPObjectList; // list of state machines to be freed
 end;

 PDwarfCompilationUnit=^TDwarfCompilationUnit;
 TDwarfCompilationUnit=object
  DebugFile    :TDwarfDebugFile;

  FDataOffset  :QWord;
  FLength      :QWord;
  FVersion     :Word;
  FAbbrevOffset:QWord;
  FAddressSize :Byte;
  FIsDwarf64   :Boolean;
  //
  FInfoData    :Pointer;
  FAbbrevData  :Pointer;
  //
  FAbbrevList  :TDwarfAbbrevList;
  //

  FProducer:RawByteString;
  FLineInfo:TFLineInfo;

  function  LocateAttribute(AEntry: Pointer; AAttribute: Cardinal;
                            out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;
  function  ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: String): Boolean;
  function  ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
  procedure FillLineInfo(AData: Pointer);
  Procedure PrintAttrDef(var AEntry:Pointer;Def:PDwarfAbbrev;level:Integer);
  Procedure CalcPtrForm(Form:Cardinal;AEntry:Pointer);
  Procedure CalcBlockForm(Form:Cardinal;AEntry:Pointer);
  Procedure CalcAttrDef(var AEntry:Pointer;Def:PDwarfAbbrev);
  Procedure PrintAll();
  Procedure Calc();
 end;

 TDwarfLineInfoStateMachine = object

   FOwner: PDwarfCompilationUnit;
   FLineInfoPtr: Pointer;
   FMaxPtr: Pointer;
   FEnded: Boolean;

   FAddress: QWord;
   FFileName: String;
   FLine: Cardinal;
   FColumn: Cardinal;
   FIsStmt: Boolean;
   FBasicBlock: Boolean;
   FEndSequence: Boolean;
   FPrologueEnd: Boolean;
   FEpilogueBegin: Boolean;
   FIsa: QWord;

   Procedure Init(AOwner: PDwarfCompilationUnit; ALineInfoPtr, AMaxPtr: Pointer);
   function  Clone: TDwarfLineInfoStateMachine;
   function  NextLine: Boolean;
   procedure Reset;

   procedure SetFileName(AIndex: Cardinal);
 end;

implementation

function ULEB128toOrdinal(var p: PByte): QWord;
var
  n: Byte;
  Stop: Boolean;
begin
  Result := 0;
  n := 0;
  repeat
    Stop := (p^ and $80) = 0;
    Result := Result or (QWord(p^ and $7F) shl n);
    Inc(n, 7);
    Inc(p);
  until Stop or (n > 128);
end;

function SLEB128toOrdinal(var p: PByte): Int64;
var
  n: Byte;
  Stop: Boolean;
begin
  Result := 0;
  n := 0;
  repeat
    Stop := (p^ and $80) = 0;
    Result := Result or (Int64(p^ and $7F) shl n);
    Inc(n, 7);
    Inc(p);
  until Stop or (n > 128);

  // sign extend when msbit = 1
  if ((p[-1] and $40) <> 0) and (n < 64) // only supports 64 bit
  then Result := Result or (Int64(-1) shl n);
end;

function ReadByte(var AEntryData:Pointer):Byte;
begin
 Result:=PBYTE(AEntryData)^;
 Inc(AEntryData,1);
end;

function ReadWord(var AEntryData:Pointer):Word;
begin
 Result:=PWORD(AEntryData)^;
 Inc(AEntryData,2);
end;

function ReadDWORD(var AEntryData:Pointer):DWORD;
begin
 Result:=PDWORD(AEntryData)^;
 Inc(AEntryData,4);
end;

function ReadQWORD(var AEntryData:Pointer):QWORD;
begin
 Result:=PQWORD(AEntryData)^;
 Inc(AEntryData,8);
end;

function ReadOrdinal(var AEntryData:Pointer;AddrSize:Byte):QWORD;
begin
 Result:=0;
 Move(AEntryData^,Result,AddrSize);
 Inc(AEntryData,AddrSize);
end;

function ReadHex(var AEntryData:Pointer;AddrSize:Byte):RawByteString;
begin
 Result:='';
 while (AddrSize<>0) do
 begin
  Result:=HexStr(PBYTE(AEntryData)^,2)+Result;
  Dec(AddrSize);
  Inc(AEntryData);
 end;
 Result:='0x'+Result;
end;

function ReadHexArray(var AEntryData:Pointer;AddrSize:Byte):RawByteString;
begin
 Result:='0x';
 while (AddrSize<>0) do
 begin
  Result:=Result+HexStr(PBYTE(AEntryData)^,2);
  Dec(AddrSize);
  Inc(AEntryData);
 end;
end;

function ReadString(var AEntryData:Pointer):RawByteString;
begin
 Result:='';
 while PByte(AEntryData)^ <> 0 do
 begin
  Result:=Result+PAnsiChar(AEntryData)^;
  Inc(AEntryData);
 end;
 Inc(AEntryData);
end;

function SkipEntryDataForForm(var AEntryData: Pointer; AForm: Cardinal; AddrSize: Byte; IsDwarf64: boolean; Version: word): Boolean;
var
  UValue: QWord;
begin
  Result := True;
  case AForm of
    DW_FORM_addr     : Inc(AEntryData, AddrSize);
    DW_FORM_block,
    DW_FORM_exprloc  :
      begin
        UValue := ULEB128toOrdinal(AEntryData);
        Inc(AEntryData, UValue);
      end;
    DW_FORM_block1   : Inc(AEntryData, PByte(AEntryData)^ + 1);
    DW_FORM_block2   : Inc(AEntryData, PWord(AEntryData)^ + 2);
    DW_FORM_block4   : Inc(AEntryData, PLongWord(AEntryData)^ + 4);
    DW_FORM_data1    : Inc(AEntryData, 1);
    DW_FORM_data2    : Inc(AEntryData, 2);
    DW_FORM_data4    : Inc(AEntryData, 4);
    DW_FORM_data8    : Inc(AEntryData, 8);
    DW_FORM_sdata    :
      begin
        while (PByte(AEntryData)^ and $80) <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_udata    :
      begin
        while (PByte(AEntryData)^ and $80) <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_flag     : Inc(AEntryData, 1);
    DW_FORM_ref1     : Inc(AEntryData, 1);
    DW_FORM_ref2     : Inc(AEntryData, 2);
    DW_FORM_ref4     : Inc(AEntryData, 4);
    DW_FORM_ref8     : Inc(AEntryData, 8);
    DW_FORM_ref_udata:
      begin
        while (PByte(AEntryData)^ and $80) <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_ref_sig8 : Inc(AEntryData, 8);
    DW_FORM_strp,
    DW_FORM_sec_offset:
      begin
        if IsDwarf64 then
          Inc(AEntryData, 8)
        else
          Inc(AEntryData, 4);
      end;
    DW_FORM_ref_addr :
      begin
        // In Dwarf-version 3 and higher, the size of a DW_FORM_ref_addr depends
        // on the Dwarf-format. In prior Dwarf-versions it is equal to the
        // Addres-size.
        if Version>2 then
        begin
          if IsDwarf64 then
            Inc(AEntryData, 8)
          else
            Inc(AEntryData, 4);
        end else
        begin
          Inc(AEntryData, AddrSize);
        end;
      end;
    DW_FORM_string   :
      begin
        while PByte(AEntryData)^ <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_indirect :
      begin
        while AForm = DW_FORM_indirect do AForm := ULEB128toOrdinal(AEntryData);
        Result := SkipEntryDataForForm(AEntryData, AForm, AddrSize, IsDwarf64, Version);
      end;
    DW_FORM_flag_present: ; // No data
  else
    begin
      Writeln(StdErr,'Error: Unknown Form: ', AForm);
      Result := False;
    end;
  end;

end;

function TDwarfDebugFile.GetStrEntryDataForForm(var AEntryData: Pointer; AForm: Cardinal; AddrSize: Byte; IsDwarf64: boolean; Version: word; Var str:RawByteString): Boolean;
var
  UValue: QWord;
begin
  str:='';
  Result := True;
  case AForm of
    DW_FORM_addr     : str:=ReadHex(AEntryData, AddrSize);
    DW_FORM_block,
    DW_FORM_exprloc  :
      begin
        UValue := ULEB128toOrdinal(AEntryData);
        str:=ReadHexArray(AEntryData, UValue);
      end;
    DW_FORM_block1   :
      begin
       UValue:=ReadByte(AEntryData);
       str:=ReadHexArray(AEntryData, UValue);
      end;
    DW_FORM_block2   :
      begin
       UValue:=ReadWord(AEntryData);
       str:=ReadHexArray(AEntryData, UValue);
      end;
    DW_FORM_block4   :
      begin
       UValue:=ReadDWord(AEntryData);
       str:=ReadHexArray(AEntryData, UValue);
      end;
    DW_FORM_data1    : str:=ReadHex(AEntryData, 1);
    DW_FORM_data2    : str:=ReadHex(AEntryData, 2);
    DW_FORM_data4    : str:=ReadHex(AEntryData, 4);
    DW_FORM_data8    : str:=ReadHex(AEntryData, 8);
    DW_FORM_sdata    : str:=IntToStr(SLEB128toOrdinal(AEntryData));
    DW_FORM_udata    : str:=IntToStr(ULEB128toOrdinal(AEntryData));
    DW_FORM_flag     : str:=ReadHex(AEntryData, 1);
    DW_FORM_ref1     : str:=ReadHex(AEntryData, 1);
    DW_FORM_ref2     : str:=ReadHex(AEntryData, 2);
    DW_FORM_ref4     : str:=ReadHex(AEntryData, 4);
    DW_FORM_ref8     : str:=ReadHex(AEntryData, 8);
    DW_FORM_ref_udata: str:=IntToStr(ULEB128toOrdinal(AEntryData));
    DW_FORM_ref_sig8 : str:=ReadHex(AEntryData, 8);

    DW_FORM_strp:
      begin
        if IsDwarf64 then
          UValue:=ReadQWORD(AEntryData)
        else
          UValue:=ReadDWORD(AEntryData);
        //
        if (debug_str.RawData<>nil) then
        begin
         str := pchar(PtrUInt(debug_str.RawData)+UValue);
        end;
      end;

    DW_FORM_sec_offset:
      begin
        if IsDwarf64 then
          str:=ReadHex(AEntryData, 8)
        else
          str:=ReadHex(AEntryData, 4);
      end;

    DW_FORM_ref_addr :
      begin
        // In Dwarf-version 3 and higher, the size of a DW_FORM_ref_addr depends
        // on the Dwarf-format. In prior Dwarf-versions it is equal to the
        // Addres-size.
        if (Version>2) then
        begin
          if IsDwarf64 then
            str:=ReadHex(AEntryData, 8)
          else
            str:=ReadHex(AEntryData, 4);
        end else
        begin
          str:=ReadHex(AEntryData, AddrSize);
        end;
      end;
    DW_FORM_string   :
      begin
        str:=ReadString(AEntryData);
      end;
    DW_FORM_indirect :
      begin
        while AForm = DW_FORM_indirect do AForm := ULEB128toOrdinal(AEntryData);
        Result := SkipEntryDataForForm(AEntryData, AForm, AddrSize, IsDwarf64, Version);
      end;
    DW_FORM_flag_present: str := '1';
  else
    begin
      Writeln(StdErr,'Error: Unknown Form: ', AForm);
      Result := False;
    end;
  end;

end;

///

function TDwarfAbbrevList.AddAttrib(attrib,Form,Parent:Cardinal):DWORD;
var
 I:DWORD;
begin
 I:=Length(FDefinitions);
 SetLength(FDefinitions,I+1);
 //

 FDefinitions[I].Attribute := attrib;
 FDefinitions[I].Form      := form;
 FDefinitions[I].Parent    := Parent;

 Result:=I;
end;

function TDwarfAbbrevList.AddAbbrev(const D:TDwarfAbbrev):DWORD;
var
 I:DWORD;
begin
 I:=Length(FAbbrevList);
 SetLength(FAbbrevList,I+1);
 //

 FAbbrevList[I] := D;

 Result:=I;
end;

function TDwarfAbbrevList.FindAbbrevFromPointer(P:Pointer):PDwarfAbbrev;
var
 I:DWORD;
begin
 Result:=nil;
 For i:=0 to High(FAbbrevList) do
 if (FAbbrevList[i].Ptr=P) then
 begin
  Result:=@FAbbrevList[i];
 end;
end;

function TDwarfAbbrevList.FindAbbrevFromId(abbrev:Integer):PDwarfAbbrev;
var
 I:DWORD;
begin
 Result:=nil;
 For i:=0 to High(FAbbrevList) do
 if (FAbbrevList[i].abbrev=abbrev) then
 begin
  Result:=@FAbbrevList[i];
 end;
end;

Procedure TDwarfAbbrevList.LoadAbbrevs(ptr_beg,ptr_end:Pointer);
var
 abbrev_ptr:Pointer;
 Def:TDwarfAbbrev;
 CurAbbrevIndex: Integer;
 n: Integer;
 attrib: Integer;
 form: Integer;

 //level:Integer;
begin
 abbrev_ptr:=ptr_beg;

 //level:=0;

 CurAbbrevIndex:=0;

 while (abbrev_ptr < ptr_end) and (pbyte(abbrev_ptr)^ <> 0) do
 begin

  Def:=Default(TDwarfAbbrev);

  Def.Ptr:=abbrev_ptr;

  Def.abbrev      := ULEB128toOrdinal(pbyte(abbrev_ptr));
  Def.tag         := ULEB128toOrdinal(pbyte(abbrev_ptr));
  Def.HasChildren := ReadByte        (pbyte(abbrev_ptr));

  //Writeln('  offset:  ', ptruint(abbrev_ptr-ptr_beg));
  //Writeln(Space(level),' abbrev:  ', Def.abbrev);
  //Writeln(Space(level),' tag:     ', Def.tag, '=', DwarfTagToString(Def.tag));
  //Writeln(Space(level),' children:', Def.HasChildren, '=', DwarfChildrenToString(Def.HasChildren));

  n := 0;
  Def.Index := CurAbbrevIndex;

  while pword(abbrev_ptr)^ <> 0 do
  begin
    attrib := ULEB128toOrdinal(pbyte(abbrev_ptr));

    form := ULEB128toOrdinal(pbyte(abbrev_ptr));
    if (form > DW_FORM_MAX) then
    begin
      Writeln(StdErr,'Unknown FW_FORM: ', form, ' found. Aborting');
      exit;
    end;

    //Writeln(Space(level),' [', n, '] attrib: ', attrib:2, '=',
    // DwarfAttributeToString(attrib):42,
    // ', form: ', form, '=',
    // DwarfAttributeFormToString(form));

    AddAttrib(attrib,form,CurAbbrevIndex);
    Inc(CurAbbrevIndex);

   Inc(n);
  end;

  {
  case Def.HasChildren of
   DW_CHILDREN_no :if (level<>0) then Dec(level);
   DW_CHILDREN_yes:Inc(level);
   else;
  end;
  }

  Def.Count := n;

  AddAbbrev(Def);

  Inc(pword(abbrev_ptr));
 end;

end;

Procedure TDwarfDebugFile.AddCompilationUnit(ADataOffset: QWord;
                                             ALength: QWord;
                                             AVersion: Word;
                                             AAbbrevOffset: QWord;
                                             AAddressSize: Byte;
                                             AIsDwarf64: Boolean);
var
 CU:TDwarfCompilationUnit;

 FEntry   :Pointer;
 Attrib   :Pointer;
 Form     :Cardinal;

 FName:String;
begin
 {
 Writeln('[CUClass.Create]');
 Writeln(' ADataOffset   = ',ADataOffset);
 Writeln(' ALength       = ',ALength);
 Writeln(' AVersion      = ',AVersion);
 Writeln(' AAbbrevOffset = ',AAbbrevOffset);
 Writeln(' AAddressSize  = ',AAddressSize);
 Writeln(' AIsDwarf64    = ',AIsDwarf64);
 }

 CU:=Default(TDwarfCompilationUnit);
 CU.DebugFile    :=Self;
 CU.FDataOffset  :=ADataOffset;
 CU.FLength      :=ALength;
 CU.FVersion     :=AVersion;
 CU.FAbbrevOffset:=AAbbrevOffset;
 CU.FAddressSize :=AAddressSize;
 CU.FIsDwarf64   :=AIsDwarf64;

 CU.FInfoData  :=debug_info.RawData   + ADataOffset;
 CU.FAbbrevData:=debug_abbrev.RawData + AAbbrevOffset;

 CU.FAbbrevList.LoadAbbrevs(CU.FAbbrevData,debug_abbrev.RawData + debug_abbrev.RawSize);

 FEntry:=CU.FInfoData;
 FName:='';
 if CU.LocateAttribute(FEntry,
                       DW_AT_name,
                       Attrib,Form) then
 begin
  CU.ReadValue(Attrib,Form,FName);
  Writeln('DWARF CU:',FName);
 end;

 FEntry:=CU.FInfoData;
 FName:='';
 if CU.LocateAttribute(FEntry,
                       DW_AT_comp_dir,
                       Attrib,Form) then
 begin
  CU.ReadValue(Attrib,Form,FName);
  //Writeln(FName);
 end;

 FEntry:=CU.FInfoData;
 FName:='';
 if CU.LocateAttribute(FEntry,
                       DW_AT_producer,
                       Attrib,Form) then
 begin
  CU.ReadValue(Attrib,Form,FName);
  CU.FProducer:=FName;
  //Writeln(FName);
 end;

 //CU.PrintAll();
 CU.Calc();

end;

Procedure TDwarfDebugFile.LoadCompilationUnits();
var
  p, pe: Pointer;
  CU32: PDwarfCUHeader32 absolute p;
  CU64: PDwarfCUHeader64 absolute p;
  CU32v5: PDwarfCUHeader32v5 absolute p;
  CU64v5: PDwarfCUHeader64v5 absolute p;

  DataOffs, DataLen: QWord;
  AbbrevOffset: QWord;
  AddressSize: Byte;
begin
 if (debug_abbrev.RawData=nil) then Exit;
 if (debug_info  .RawData=nil) then Exit;

 p  := debug_info.RawData;
 pe := debug_info.RawData + debug_info.RawSize;

 while (p <> nil) and (p < pe) do
 begin
   if (CU64^.Signature = DWARF_HEADER64_SIGNATURE) then
   begin
     if CU64^.Version < 3 then
     begin
      Writeln(StdErr,'Unexpected 64 bit signature found for DWARF version 2'); // or version 1...
     end;

     if CU32^.Version<5 then
     begin
       DataOffs := PtrUInt(CU64 + 1) - PtrUInt(debug_info.RawData);
       DataLen := CU64^.Length - SizeOf(CU64^) + SizeOf(CU64^.Signature) + SizeOf(CU64^.Length);
       AbbrevOffset := CU32v5^.AbbrevOffset;
       AddressSize := CU32v5^.AddressSize;
     end
     else
     begin
       DataOffs := PtrUInt(CU64v5 + 1) - PtrUInt(debug_info.RawData);
       DataLen := CU64v5^.Length - SizeOf(CU64v5^) + SizeOf(CU64v5^.Signature) + SizeOf(CU64v5^.Length);
       AbbrevOffset := CU32v5^.AbbrevOffset;
       AddressSize := CU32v5^.AddressSize;

       if CU64v5^.unit_type <> $01 then
       begin
         Writeln(StdErr,Format('Found Dwarf-5 partial compilation unit ot offset %d, which is not supported. Compilation unit is skipped.', [DataOffs]));
         break; // Do not process invalid data
       end;
     end;

     if (DataOffs + DataLen > debug_info.RawSize) then
     begin
       Writeln(StdErr,Format('Error: Invalid size for compilation unit at offest %d with size %d exceeds section size %d', [DataOffs, DataLen, debug_info.RawSize]));
       break; // Do not process invalid data
     end;

     AddCompilationUnit(
      DataOffs,
      DataLen,
      CU64^.Version,
      AbbrevOffset,
      AddressSize,
      True);

     p := Pointer(@CU64^.Version) + CU64^.Length;
   end else
   begin
     if CU32^.Length = 0 then Break;
     if CU32^.Version<5 then
     begin
       DataOffs := PtrUInt(CU32 + 1) - PtrUInt(debug_info.RawData);
       DataLen := CU32^.Length - SizeOf(CU32^) + SizeOf(CU32^.Length);
       AbbrevOffset := CU32^.AbbrevOffset;
       AddressSize := CU32^.AddressSize;
     end
     else
     begin
       DataOffs := PtrUInt(CU32v5 + 1) - PtrUInt(debug_info.RawData);
       DataLen := CU32v5^.Length - SizeOf(CU32v5^) + SizeOf(CU32v5^.Length);
       AbbrevOffset := CU32v5^.AbbrevOffset;
       AddressSize := CU32v5^.AddressSize;

       if CU32v5^.unit_type <> $01 then
       begin
         Writeln(StdErr,Format('Found Dwarf-5 partial compilation unit ot offset %d, which is not supported. Compilation unit is skipped.', [DataOffs]));
         break; // Do not process invalid data
       end;
     end;

     if (DataOffs + DataLen > debug_info.RawSize) then
     begin
       Writeln(StdErr,Format('Error: Invalid size for compilation unit at offest %d with size %d exceeds section size %d', [DataOffs, DataLen, debug_info.RawSize]));
       break; // Do not process invalid data
     end;

     AddCompilationUnit(
      DataOffs,
      DataLen,
      CU32^.Version,
      AbbrevOffset,
      AddressSize,
      False);

     p := Pointer(@CU32^.Version) + CU32^.Length;
   end;

   //FCompilationUnits.Add(CU);
   //if CU.Valid then SetHasInfo;
 end;


end;

////////

function TDwarfCompilationUnit.LocateAttribute(AEntry: Pointer; AAttribute: Cardinal;
                                               out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;
var
 Def: PDwarfAbbrev;
 abbrev : Integer;
 n: Integer;
begin
 abbrev:=ULEB128toOrdinal(AEntry);

 Def:=FAbbrevList.FindAbbrevFromId(abbrev);

 if (Def = nil) then
 begin
  Writeln('Error: Abbrev not found: ',abbrev);
  Result := False;
  Exit;
 end;

 for n := Def^.index to Def^.index + Def^.count - 1 do
 begin
  if (FAbbrevList.FDefinitions[n].Attribute = AAttribute) then
  begin
   Result := True;
   AAttribPtr := AEntry;
   AForm := FAbbrevList.FDefinitions[n].Form;
   Exit;
  end else
  begin
    if not SkipEntryDataForForm(AEntry, FAbbrevList.FDefinitions[n].Form, FAddressSize, FIsDwarf64, FVersion) then
      break;
  end;
 end;
 Result := False;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: String): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_string:
    begin
      AValue := PChar(AAttribute);
    end;
    DW_FORM_strp:
    begin
     if (DebugFile.debug_str.RawData<>nil) then
     begin
       if FIsDwarf64 then
         AValue := pchar(PtrUInt(DebugFile.debug_str.RawData)+PQWord(AAttribute)^)
       else
         AValue := pchar(PtrUInt(DebugFile.debug_str.RawData)+PDWord(AAttribute)^);
     end else
     begin
      AValue := '';
     end;
    end
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr     : AValue:=ReadOrdinal(AAttribute, FAddressSize);
    DW_FORM_ref_addr :
      begin
        // In Dwarf-version 3 and higher, the size of a DW_FORM_ref_addr depends
        // on the Dwarf-format. In prior Dwarf-versions it is equal to the
        // Addres-size.
        if (FVersion>2) then
        begin
          if FIsDwarf64 then
            AValue:=ReadOrdinal(AAttribute, 8)
          else
            AValue:=ReadOrdinal(AAttribute, 4);
        end else
        begin
          AValue:=ReadOrdinal(AAttribute, FAddressSize);
        end;
      end;
    DW_FORM_flag_present: AValue := 1;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PByte(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PWord(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongWord(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PQWord(AAttribute)^;
    end;
    DW_FORM_sec_offset: begin
      if FIsDwarf64 then
        AValue := PQWord(AAttribute)^
      else
        AValue := PLongWord(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := QWord(SLEB128toOrdinal(AAttribute));
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;


procedure TDwarfCompilationUnit.FillLineInfo(AData: Pointer);
var
  LNP32: PDwarfLNPHeader32 absolute AData;
  LNP64: PDwarfLNPHeader64 absolute AData;
  Info: PDwarfLNPInfoHeader;

  UnitLength: QWord;
  Version: Word;
  HeaderLength: QWord;
  Name: PChar;
  diridx: Cardinal;
  S, S2: String;
  pb: PByte absolute Name;
  oldFpc: Boolean;
  i: SizeInt;

begin
  FLineInfo.Header := AData;
  if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
  then begin
    if FVersion < 3 then
      Writeln('Unexpected 64 bit signature found for DWARF version 2'); // or version 1...
    UnitLength := LNP64^.UnitLength;
    FLineInfo.DataEnd := Pointer(@LNP64^.Version) + UnitLength;
    Version := LNP64^.Version;
    HeaderLength := LNP64^.HeaderLength;
    Info := @LNP64^.Info;
  end
  else begin
    UnitLength := LNP32^.UnitLength;
    FLineInfo.DataEnd := Pointer(@LNP32^.Version) + UnitLength;
    Version := LNP32^.Version;
    HeaderLength := LNP32^.HeaderLength;
    Info := @LNP32^.Info;
  end;
  if Version=0 then ;
  FLineInfo.Addr64 := FAddressSize = 8;
  FLineInfo.AddrSize := FAddressSize;
  FLineInfo.DataStart := PByte(Info) + HeaderLength;
  FLineInfo.Version := Version;


  FLineInfo.MinimumInstructionLength := Info^.MinimumInstructionLength;
  FLineInfo.MaximumInstructionLength := 1;
  if Version >= 4 then begin
    // Older FreePascal writes an incorrect header
    oldFpc := False;
    s := LowerCase(FProducer);
    i := Pos('free pascal ',  s);
    if i > 0 then begin
      s := copy(s, i+12,5);
      oldFpc := (Length(s) = 5) and (
        (s[1] = '2') or                                   // fpc 2.x
        ( (s[1] = '3') and (s[3] in ['0', '1']) ) or      // fpc 3.0 / 3.1
        ( (s[1] = '3') and (s[3] = '2') and (s[5] in ['0', '1', '2', '3']) ) // fpc 3.2.[0123]]
      );
    end;
    if not oldFpc then begin
      inc(PByte(Info)); // All fields move by 1 byte // Dwarf-4 has a new field
      FLineInfo.MaximumInstructionLength := Info^.MinimumInstructionLength;
    end;
  end;
  FLineInfo.DefaultIsStmt := Info^.DefaultIsStmt <> 0;
  FLineInfo.LineBase := Info^.LineBase;
  FLineInfo.LineRange := Info^.LineRange;

  // opcodelengths
  SetLength(FLineInfo.StandardOpcodeLengths, Info^.OpcodeBase - 1);
  Move(Info^.StandardOpcodeLengths, FLineInfo.StandardOpcodeLengths[0], Info^.OpcodeBase - 1);

  // directories & filenames
  //FLineInfo.Directories := TStringList.Create;
  //FLineInfo.Directories.Add(''); // current dir
  Name := PChar(@Info^.StandardOpcodeLengths);
  Inc(Name, Info^.OpcodeBase-1);

  // directories
  while Name^ <> #0 do
  begin
    S := String(Name);
    Inc(pb, Length(S)+1);
    //FLineInfo.Directories.Add(S + DirectorySeparator);
  end;
  Inc(Name);

  // filenames
  //FLineInfo.FileNames := TStringList.Create;
  while Name^ <> #0 do
  begin
    S := String(Name);
    Inc(pb, Length(S)+1);
    //diridx
    diridx := ULEB128toOrdinal(pb);

    {
    if diridx < FLineInfo.Directories.Count then
    begin
      S2 := FLineInfo.Directories[diridx] + S;
      S := CreateAbsolutePath(S2, FCompDir);
      if (diridx = 0) and not FileExistsUTF8(S2) and (FLineInfo.FileNames.Count > 0) then // https://bugs.freepascal.org/view.php?id=37658
        S := S2;
    end
    else
      S := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + S;
    }

    //FLineInfo.FileNames.Add(S);
    //last modified
    ULEB128toOrdinal(pb);
    //length
    ULEB128toOrdinal(pb);
  end;

  //FLineInfo.StateMachine  := TDwarfLineInfoStateMachine.Create(Self, FLineInfo.DataStart, FLineInfo.DataEnd);
  //FLineInfo.StateMachines := TFPObjectList.Create(True);

  // MaximumInstructionLength is currently not supported
  if FLineInfo.MaximumInstructionLength <> 1 then
    exit;

  FLineInfo.Valid := True;
end;


Procedure TDwarfCompilationUnit.PrintAttrDef(var AEntry:Pointer;Def:PDwarfAbbrev;level:Integer);
var
 n: Integer;
 str:RawByteString;
 Attribute:Cardinal;
 Form     :Cardinal;
begin
 For n:=Def^.index to Def^.index + Def^.count -1 do
 begin
  Attribute:=FAbbrevList.FDefinitions[n].Attribute;
  Form     :=FAbbrevList.FDefinitions[n].Form;

  str:='';
  if DebugFile.GetStrEntryDataForForm(AEntry,
                                      Form,
                                      FAddressSize,
                                      FIsDwarf64,
                                      FVersion,
                                      str) then
  begin

   //if ((Form=DW_FORM_addr) or (Form=DW_FORM_ref_addr)) then
   begin
    Writeln(Space(level),'--[',Def^.abbrev,',', DwarfTagToString(Def^.tag), ']--');

    Writeln(Space(level),'[', n:3, ']',
            DwarfAttributeToString(Attribute):13, ':',
            DwarfAttributeFormToString(Form), '=',
            str);
   end;

  end else
  begin
   Writeln('crash');
  end;

 end;
end;

Procedure TDwarfCompilationUnit.CalcPtrForm(Form:Cardinal;AEntry:Pointer);
begin
  if (DebugFile.cb=nil) then Exit;

 case Form of
   DW_FORM_addr:
     begin
      if (FAddressSize=8) then //64bit
      begin
        DebugFile.cb(AEntry);
      end;
     end;
   DW_FORM_ref_addr:
     begin
       // In Dwarf-version 3 and higher, the size of a DW_FORM_ref_addr depends
       // on the Dwarf-format. In prior Dwarf-versions it is equal to the
       // Addres-size.
       if (FVersion>2) then
       begin
         if FIsDwarf64 then //64bit
         begin
           DebugFile.cb(AEntry);
         end;
       end else
       begin
         if (FAddressSize=8) then //64bit
         begin
           DebugFile.cb(AEntry);
         end;
       end;
     end;
   else;
 end;
end;

Procedure TDwarfCompilationUnit.CalcBlockForm(Form:Cardinal;AEntry:Pointer);
var
  UValue: QWord;
begin
 if (DebugFile.cb=nil) then Exit;

 case Form of
   DW_FORM_block:
     begin
      UValue := ULEB128toOrdinal(AEntry);
      if (UValue>8) then //64bit
      begin
        UValue:=ReadByte(AEntry);
        if (UValue=DW_OP_addr) then //is addr
        begin
         DebugFile.cb(AEntry);
        end;
      end;
     end;
   DW_FORM_block1:
     begin
      UValue:=ReadByte(AEntry);
      if (UValue>8) then //64bit
      begin
       UValue:=ReadByte(AEntry);
       if (UValue=DW_OP_addr) then //is addr
       begin
        DebugFile.cb(AEntry);
       end;
      end;
     end;
   DW_FORM_block2:
     begin
      UValue:=ReadWord(AEntry);
      if (UValue>8) then //64bit
      begin
       UValue:=ReadByte(AEntry);
       if (UValue=DW_OP_addr) then //is addr
       begin
        DebugFile.cb(AEntry);
       end;
      end;
     end;
   DW_FORM_block4:
     begin
      UValue:=ReadDWord(AEntry);
      if (UValue>8) then //64bit
      begin
       UValue:=ReadByte(AEntry);
       if (UValue=DW_OP_addr) then //is addr
       begin
        DebugFile.cb(AEntry);
       end;
      end;
     end;
   DW_FORM_data8:
     begin
      //64bit
      DebugFile.cb(AEntry);
     end;
   else;
 end;
end;

Procedure TDwarfCompilationUnit.CalcAttrDef(var AEntry:Pointer;Def:PDwarfAbbrev);
var
 n: Integer;
 Attribute:Cardinal;
 Form     :Cardinal;

 StatementListOffs: QWord;

 StateMachine:TDwarfLineInfoStateMachine;
begin

 For n:=Def^.index to Def^.index + Def^.count -1 do
 begin
  Attribute:=FAbbrevList.FDefinitions[n].Attribute;
  Form     :=FAbbrevList.FDefinitions[n].Form;

  if (Attribute=DW_AT_stmt_list) then
  begin
   if not ReadValue(AEntry,Form,StatementListOffs) then
   begin
    Writeln(stderr,'ReadValue crash');
   end;

   if (DebugFile.debug_line.RawData<>nil) then
   begin
    FillLineInfo(DebugFile.debug_line.RawData + StatementListOffs);

    StateMachine:=Default(TDwarfLineInfoStateMachine);
    StateMachine.Init(@self,FLineInfo.DataStart, FLineInfo.DataEnd);

    while StateMachine.NextLine do
    begin
     //Writeln;
    end;
   end;

  end else
  if (Attribute=DW_AT_location) then
  begin
   CalcBlockForm(Form,AEntry);
  end;

  CalcPtrForm(Form,AEntry);

  //next
  if not SkipEntryDataForForm(AEntry,Form,FAddressSize,FIsDwarf64,FVersion) then
  begin
   Writeln(stderr,'SkipEntryDataForForm crash');
  end;

 end; //For
end;

Procedure TDwarfCompilationUnit.PrintAll();
var
 AEntry: Pointer;
 AEnd  : Pointer;
 Def: PDwarfAbbrev;
 level:Integer;

 abbrev   : Integer;

begin
 AEntry:=FInfoData;
 AEnd  :=FInfoData + FLength;

 level:=0;

 while (AEntry<AEnd) do
 begin
  //Writeln(' 0x',HexStr(AEntry-FInfoData+FDataOffset,4));

  abbrev:=ULEB128toOrdinal(AEntry);

  if (abbrev=0) then
  begin
   //writeln;
   Continue;
  end;

  Def:=FAbbrevList.FindAbbrevFromId(abbrev);

  if (Def=nil) then
  begin
   Writeln('Unknow abbrev = ',abbrev,' 0x',HexStr(AEntry-FInfoData+FDataOffset,4));
  end;

  //Writeln(Space(level),'--[',abbrev,',', DwarfTagToString(Def^.tag), ']--');

  PrintAttrDef(AEntry,Def,level);

 end;

end;

Procedure TDwarfCompilationUnit.Calc();
var
 AEntry: Pointer;
 AEnd  : Pointer;
 Def: PDwarfAbbrev;

 abbrev   : Integer;

begin
 AEntry:=FInfoData;
 AEnd  :=FInfoData + FLength;

 while (AEntry<AEnd) do
 begin
  abbrev:=ULEB128toOrdinal(AEntry);

  if (abbrev=0) then
  begin
   Continue;
  end;

  Def:=FAbbrevList.FindAbbrevFromId(abbrev);

  if (Def=nil) then
  begin
   Writeln('Unknow abbrev = ',abbrev,' 0x',HexStr(AEntry-FInfoData+FDataOffset,4));
   Exit;
  end;

  CalcAttrDef(AEntry,Def);

 end;

end;

///////

function TDwarfLineInfoStateMachine.Clone: TDwarfLineInfoStateMachine;
begin
  Result := Default(TDwarfLineInfoStateMachine);
  Result.Init(FOwner, FLineInfoPtr, FMaxPtr);
  Result.FAddress := FAddress;
  Result.FFileName := FFileName;
  Result.FLine := FLine;
  Result.FColumn := FColumn;
  Result.FIsStmt := FIsStmt;
  Result.FBasicBlock := FBasicBlock;
  Result.FEndSequence := FEndSequence;
  Result.FPrologueEnd := FPrologueEnd;
  Result.FEpilogueBegin := FEpilogueBegin;
  Result.FIsa := FIsa;
  Result.FEnded := FEnded;
end;

procedure TDwarfLineInfoStateMachine.Init(AOwner: PDwarfCompilationUnit; ALineInfoPtr, AMaxPtr: Pointer);
begin
 FOwner := AOwner;
 FLineInfoPtr := ALineInfoPtr;
 FMaxPtr := AMaxPtr;
 Reset;
end;

function TDwarfLineInfoStateMachine.NextLine: Boolean;
var
  p: Pointer;
  Opcode: Byte;
  instrlen: Cardinal;
  diridx: Cardinal;
begin
  Result := False;
  if FEndSequence then
  begin
    Reset;
  end;

  while pbyte(FLineInfoPtr) < FMaxPtr do
  begin
    Opcode := pbyte(FLineInfoPtr)^;
    Inc(pbyte(FLineInfoPtr));
    if Opcode <= Length(FOwner^.FLineInfo.StandardOpcodeLengths) then
    begin
      // Standard opcode
      case Opcode of
        DW_LNS_copy:
        begin
          Result := True;
          Exit;
        end;
        DW_LNS_advance_pc:
        begin
          {$PUSH}{$R-}{$Q-}
          Inc(FAddress, ULEB128toOrdinal(pbyte(FLineInfoPtr)));
          {$POP}
          //Writeln('DW_LNS_advance_pc=0x',HexStr(FAddress,16));
        end;
        DW_LNS_advance_line:
        begin
          Inc(FLine, SLEB128toOrdinal(pbyte(FLineInfoPtr)));
          //Writeln('DW_LNS_advance_line',FLine);
        end;
        DW_LNS_set_file:
        begin
          //SetFileName(ULEB128toOrdinal(pbyte(FLineInfoPtr)));
          //Writeln('DW_LNS_set_file=',ULEB128toOrdinal(pbyte(FLineInfoPtr)));
        end;
        DW_LNS_set_column:
        begin
          FColumn := ULEB128toOrdinal(pbyte(FLineInfoPtr));
          //Writeln('DW_LNS_set_column=',FColumn);
        end;
        DW_LNS_negate_stmt:
        begin
          FIsStmt := not FIsStmt;
          //Writeln('DW_LNS_negate_stmt');
        end;
        DW_LNS_set_basic_block:
        begin
          FBasicBlock := True;
          //Writeln('DW_LNS_set_basic_block');
        end;
        DW_LNS_const_add_pc:
        begin
          Opcode := 255 - Length(FOwner^.FLineInfo.StandardOpcodeLengths);
          {$PUSH}{$R-}{$Q-}
          if FOwner^.FLineInfo.LineRange = 0
          then Inc(FAddress, Opcode * FOwner^.FLineInfo.MinimumInstructionLength)
          else Inc(FAddress, (Opcode div FOwner^.FLineInfo.LineRange) * FOwner^.FLineInfo.MinimumInstructionLength);
          {$POP}
          // version 4 also op_index register, if architecture has VLIW

          //Writeln('DW_LNS_const_add_pc=0x',HexStr(FAddress,16));
        end;
        DW_LNS_fixed_advance_pc:
        begin
          {$PUSH}{$R-}{$Q-}
          Inc(FAddress, PWord(FLineInfoPtr)^);
          {$POP}
          Inc(pbyte(FLineInfoPtr), 2);

          //Writeln('DW_LNS_fixed_advance_pc=0x',HexStr(FAddress,16));
        end;
        DW_LNS_set_prologue_end:
        begin
          FPrologueEnd := True;
          //Writeln('DW_LNS_set_prologue_end');
        end;
        DW_LNS_set_epilogue_begin:
        begin
          FEpilogueBegin := True;
          //Writeln('DW_LNS_set_epilogue_begin');
        end;
        DW_LNS_set_isa:
        begin
          FIsa := ULEB128toOrdinal(pbyte(FLineInfoPtr));
          //Writeln('DW_LNS_set_isa=',FIsa);
        end;
        // Extended opcode
        DW_LNS_extended_opcode:
        begin
          instrlen := ULEB128toOrdinal(pbyte(FLineInfoPtr)); // instruction length

          case pbyte(FLineInfoPtr)^ of
            DW_LNE_end_sequence:
            begin
              //Writeln('DW_LNE_end_sequence');
              FEndSequence := True;
              Result := True;
              Inc(pbyte(FLineInfoPtr), instrlen);
              Exit;
            end;
            DW_LNE_set_address:
            begin
              if FOwner^.FLineInfo.AddrSize = 8 then //64bit
              begin
                if (FOwner^.DebugFile.cb<>nil) then
                begin
                 FOwner^.DebugFile.cb(PQWord(pbyte(FLineInfoPtr)+1));
                end;
                //
                FAddress := PQWord(pbyte(FLineInfoPtr)+1)^;
              end
              else if FOwner^.FLineInfo.AddrSize = 4 then
                FAddress:= PLongWord(pbyte(FLineInfoPtr)+1)^
              else
                FAddress := PWord(pbyte(FLineInfoPtr)+1)^;

              //Writeln('DW_LNE_set_address=0x',HexStr(FAddress,16));
            end;
            DW_LNE_define_file:
            begin
              // don't move pb, it's done at the end by instruction length
              p := pbyte(FLineInfoPtr);
              FFileName := String(PChar(p));
              Inc(p, Length(FFileName) + 1);

              //diridx
              diridx := ULEB128toOrdinal(p);

              //Writeln('DW_LNE_define_file:',diridx);

              {
              if diridx < FOwner^.FLineInfo.Directories.Count
              then FFileName := FOwner.FLineInfo.Directories[diridx] + FFileName
              else FFileName := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + FFileName;
              }

              //last modified
              //ULEB128toOrdinal(p);
              //length
              //ULEB128toOrdinal(p));
            end;
            // Version-4
             DW_LNE_set_discriminator:
             begin
               // for now just skif the value
               //p := pbyte(FLineInfoPtr);
               //FDiscriminator := ULEB128toOrdinal(pbyte(p));
             end;
          else
            // unknown extendend opcode
          end;
          Inc(pbyte(FLineInfoPtr), instrlen);
        end;
      else
        // unknown opcode
        if Opcode >= Length(FOwner^.FLineInfo.StandardOpcodeLengths) then
        begin
          Writeln(stderr,'Error, unknown line machine opcode: ', Opcode);
          exit(False); // can't handle unknow upcode
        end;
        Writeln(stderr,'Skipping unknown line machine opcode: ', Opcode);
        Inc(pbyte(FLineInfoPtr), FOwner^.FLineInfo.StandardOpcodeLengths[Opcode])
      end;
      Continue;
    end;

    // Special opcode
    Dec(Opcode, Length(FOwner^.FLineInfo.StandardOpcodeLengths)+1);
    {$PUSH}{$R-}{$Q-}
    if FOwner^.FLineInfo.LineRange = 0 then
    begin
      Inc(FAddress, Opcode * FOwner^.FLineInfo.MinimumInstructionLength);

      //Writeln('Special_opcode =0x',HexStr(FAddress,16));
    end else
    begin
      Inc(FAddress, (Opcode div FOwner^.FLineInfo.LineRange) * FOwner^.FLineInfo.MinimumInstructionLength);
      Inc(FLine   , FOwner^.FLineInfo.LineBase + (Opcode mod FOwner^.FLineInfo.LineRange));

      //Writeln('Special_opcode=0x',HexStr(FAddress,16),' ',FLine);
    end;
    {$POP}
    FBasicBlock    := False;
    FPrologueEnd   := False;
    FEpilogueBegin := False;
    //FDiscriminator := False;

    Result := True;
    Exit;
  end;
  Result := False;
  FEnded := True;
end;

procedure TDwarfLineInfoStateMachine.Reset;
begin
  FAddress := 0;
  SetFileName(1);
  FLine := 1;
  FColumn := 0;
  FIsStmt := FOwner^.FLineInfo.DefaultIsStmt;
  FBasicBlock := False;
  FEndSequence := False;
  FPrologueEnd := False;
  FEpilogueBegin := False;
  FIsa := 0;
end;

procedure TDwarfLineInfoStateMachine.SetFileName(AIndex: Cardinal);
begin
 //Writeln('SetFileName:',AIndex);
 {
  if (Aindex > 0) and (AIndex <= FOwner.FLineInfo.FileNames.Count)
  then FFileName := FOwner.FLineInfo.FileNames[AIndex - 1]
  else FFileName := Format('Unknown fileindex(%u)', [AIndex]);
 }
end;

//

type
 PDebugArangesHeader32 = ^TDebugArangesHeader32;
 TDebugArangesHeader32 = packed record
   unit_length : DWord;
   version : Word;
   debug_info_offset : DWord;
   address_size : Byte;
   segment_size : Byte;
 {$ifndef CPUI8086}
   padding : DWord;
 {$endif CPUI8086}
 end;

 PDebugArangesHeader64 = ^TDebugArangesHeader64;
 TDebugArangesHeader64 = packed record
   magic : DWord;
   unit_length : QWord;
   version : Word;
   debug_info_offset : QWord;
   address_size : Byte;
   segment_size : Byte;
 {$ifndef CPUI8086}
   padding : DWord;
 {$endif CPUI8086}
 end;

Procedure TDwarfDebugFile.AddArange(data_offset  : QWord;
                                    data_length  : QWord;
                                    address_size : Byte;
                                    segment_size : Byte);
var
 p, pe: Pointer;

 arange_start  :QWord;
 arange_segment:QWord;
 arange_size   :QWord;
begin
 p  := debug_aranges.RawData + data_offset;
 pe := p + data_length;

 while (p <> nil) and (p < pe) do
 begin

  if (address_size=8) then //64bit
  if (cb<>nil) then
  begin
   cb(p);
  end;

  arange_start  :=ReadOrdinal(p,address_size);
  arange_segment:=ReadOrdinal(p,segment_size);
  arange_size   :=ReadOrdinal(p,address_size);

  if (arange_start=0) and
     (arange_segment=0) and
     (arange_size=0) then Break;

  {
  Writeln('---------------------------------');
  Writeln('arange_start  =0x',HexStr(arange_start,16));
  Writeln('arange_segment=0x',HexStr(arange_segment,16));
  Writeln('arange_size   =0x',HexStr(arange_size,16));
  }
 end;

end;

Procedure TDwarfDebugFile.LoadArangesUnits();
var
  p, pe: Pointer;
  AR32: PDebugArangesHeader32 absolute p;
  AR64: PDebugArangesHeader64 absolute p;

  data_offset : QWord;
  data_length : QWord;
  address_size : Byte;
  segment_size : Byte;
begin
 if (debug_aranges.RawData=nil) then Exit;

 p  := debug_aranges.RawData;
 pe := debug_aranges.RawData + debug_aranges.RawSize;

 while (p <> nil) and (p < pe) do
 begin
   if (AR64^.magic = DWARF_HEADER64_SIGNATURE) then
   begin

    data_offset  := PtrUInt(AR64 + 1) - PtrUInt(debug_aranges.RawData);
    data_length  := AR64^.unit_length - SizeOf(AR64^) + SizeOf(AR64^.magic) + SizeOf(AR64^.unit_length);
    address_size := AR64^.address_size;
    segment_size := AR64^.segment_size;

    if (data_offset + data_length > debug_aranges.RawSize) then
    begin
      Writeln(StdErr,Format('Error: Invalid size for arrange unit at offest %d with size %d exceeds section size %d', [data_offset, data_length, debug_aranges.RawSize]));
      break; // Do not process invalid data
    end;

    AddArange(data_offset,data_length,address_size,segment_size);

    p := Pointer(@AR64^.Version) + AR64^.unit_length;
   end else
   begin
     if (AR32^.unit_length = 0) then Break;

     data_offset  := PtrUInt(AR32 + 1) - PtrUInt(debug_aranges.RawData);
     data_length  := AR32^.unit_length - SizeOf(AR32^) + SizeOf(AR32^.unit_length);
     address_size := AR32^.address_size;
     segment_size := AR32^.segment_size;

     if (data_offset + data_length > debug_aranges.RawSize) then
     begin
       Writeln(StdErr,Format('Error: Invalid size for arrange unit at offest %d with size %d exceeds section size %d', [data_offset, data_length, debug_aranges.RawSize]));
       break; // Do not process invalid data
     end;

     AddArange(data_offset,data_length,address_size,segment_size);

     p := Pointer(@AR32^.Version) + AR32^.unit_length;
   end;

 end;


end;


end.

