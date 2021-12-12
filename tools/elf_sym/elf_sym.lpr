
uses
 {$IFDEF Linux}
 cmem,
 cthreads,
 {$ENDIF}
 Classes,sysutils,
 gset,
 sha1,
 ps4_types,
 uarch;

type
 TRawStrCompare=class
  class function c(var a,b:RawByteString):boolean; static;
 end;
 TRawStrSet=specialize TSet<RawByteString,TRawStrCompare>;

class function TRawStrCompare.c(var a,b:RawByteString):boolean;
var
 count1,count2:SizeInt;
begin
 Count1:=Length(a);
 Count2:=Length(b);
 Result:=(Count1<Count2) or (
          (Count1=Count2) and (CompareMemRange(PChar(a),PChar(b),Count1)<0)
         );
end;

function ps4_nid_hash(name:PChar):QWORD;
const
 salt:array[0..15] of Byte=($51,$8D,$64,$A6,$35,$DE,$D8,$C1,$E6,$B0,$39,$B1,$C3,$E5,$52,$30);
var
 Context:TSHA1Context;
 Digest:TSHA1Digest;
begin
 SHA1Init(Context);
 SHA1Update(Context,name^,StrLen(name));
 SHA1Update(Context,salt,Length(salt));
 SHA1Final(Context,Digest);
 Result:=PQWORD(@Digest)^;
end;

function DecodeValue64(strEnc:PAnsiChar;len:SizeUint;var nVal:QWORD):Boolean;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 Result:=False;
 nVal:=0;
 if (len>nEncLenMax) or (len=0) then Exit;
 For i:=0 to len-1 do
 begin
  case strEnc[i] of
   'A'..'Z':nIndex:=Byte(strEnc[i])-Byte('A')+0;
   'a'..'z':nIndex:=Byte(strEnc[i])-Byte('a')+26;
   '0'..'9':nIndex:=Byte(strEnc[i])-Byte('0')+52;
   '+':nIndex:=62;
   '-':nIndex:=63;
   else Exit;
  end;
  if (i<(nEncLenMax-1)) then
  begin
   nVal:=nVal shl 6;
   nVal:=nVal or nIndex;
  end else
  begin
   nVal:=nVal shl 4;
   nVal:=nVal or (nIndex shr 2);
  end;
 end;
 Result:=True;
end;

function EncodeValue64(nVal:QWORD):RawByteString;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 SetLength(Result,nEncLenMax);
 For i:=nEncLenMax downto 1 do
 begin
  if (i<>nEncLenMax) then
  begin
   nIndex:=nVal and 63;
   nVal:=nVal shr 6;
  end else
  begin
   nIndex:=(nVal and 15) shl 2;
   nVal:=nVal shr 4;
  end;
  case nIndex of
    0..25:Result[i]:=Char(nIndex+Byte('A')-0);
   26..51:Result[i]:=Char(nIndex+Byte('a')-26);
   52..61:Result[i]:=Char(nIndex+Byte('0')-52);
       62:Result[i]:='+';
       63:Result[i]:='-';
  end;
 end;
end;

type
 TPcharCb=procedure(pName:Pchar;data:Pointer);

procedure _LoadSym(F:THandle;offset:Int64;cb:TPcharCb;data:Pointer);
Var
 i:SizeInt;
 elf_hdr:elf64_hdr;
 elf_shdr:elf64_shdr;
 symbol:elf64_sym;

 pStrTab_pAddr:QWORD;
 pStrTab_nSize:QWORD;

 pSymTab_pAddr:QWORD;
 pSymTab_nSize:QWORD;

 pStrTable:PChar;
 pName:PChar;
begin
 FileSeek(F,offset,0);
 FileRead(F,elf_hdr,SizeOf(elf_hdr));

 if (PDWORD(@elf_hdr.e_ident)^<>ELFMAG) then Exit; //is elf
 if (elf_hdr.e_phnum=0) then Exit;

 pStrTab_pAddr:=0;
 pStrTab_nSize:=0;
 pSymTab_pAddr:=0;
 pSymTab_nSize:=0;

 FileSeek(F,offset+Int64(elf_hdr.e_shoff),0);
 For i:=0 to elf_hdr.e_shnum-1 do
 begin
  FileRead(F,elf_shdr,SizeOf(elf64_shdr));
  case elf_shdr.sh_type of
   SHT_STRTAB:
    begin
     pStrTab_pAddr:=elf_shdr.sh_offset;
     pStrTab_nSize:=elf_shdr.sh_size;
    end;
   SHT_SYMTAB:
    begin
     pSymTab_pAddr:=elf_shdr.sh_offset;
     pSymTab_nSize:=elf_shdr.sh_size div SizeOf(Elf64_Sym);
     Assert(SizeOf(elf64_sym)=elf_shdr.sh_entsize);
    end;
  end;
 end;

 if (pStrTab_pAddr=0) then Exit;
 if (pStrTab_nSize=0) then Exit;
 if (pSymTab_pAddr=0) then Exit;
 if (pSymTab_nSize=0) then Exit;

 pStrTable:=AllocMem(pStrTab_nSize);
 FileSeek(F,offset+Int64(pStrTab_pAddr),0);
 FileRead(F,pStrTable^,pStrTab_nSize);

 FileSeek(F,offset+Int64(pSymTab_pAddr),0);
 For i:=0 to pSymTab_nSize-1 do
 begin
  FileRead(F,symbol,SizeOf(elf64_sym));

  if (symbol.st_size=0) then Continue;

  case ELF64_ST_BIND(symbol.st_info) of
   STB_GLOBAL,
   STB_WEAK:;
   else
    Continue;
  end;

  case ELF64_ST_TYPE(symbol.st_info) of
   STT_OBJECT,
   STT_FUN,
   STT_COMMON,
   STT_TLS:;
   else
    Continue;
  end;

  if ELF64_ST_VISIBILITY(symbol.st_other)<>STV_DEFAULT then
  begin
   Continue;
  end;

  pName:=@pStrTable[symbol.st_name];

  case PWORD(pName)^ of
   $502F: // /P
    begin
     Case pName[2] of
      'G':pName:=@pName[3];
      'L':Continue;
      else
          pName:=@pName[2];
     end;
    end;
   $472F: // /G
    begin
     Case pName[2] of
      '/':Continue;
      else
          pName:=@pName[2];
     end;
    end;
   $532F: // /S
    begin
     Continue;
    end;
  end;

  {if Pos('/P',pName)<>0 then
  begin
   Writeln(ELF64_ST_BIND(symbol.st_info));
   Writeln(ELF64_ST_TYPE(symbol.st_info));
   Writeln(ELF64_ST_VISIBILITY(symbol.st_other));

   h:=ps4_nid_hash(pName);
   Writeln('h1:',HexStr(h,16));

   sh:=EncodeValue64(h);
   Writeln('sh:',sh);

   h:=0;
   DecodeValue64(PChar(sh),Length(sh),h);
   Writeln('h2:',HexStr(h,16));

   writeln;
  end;}

  cb(pName,data);
 end;

 FreeMem(pStrTable);
end;

function LoadSymFromFile(Const name:RawByteString;cb:TPcharCb;data:Pointer):Boolean;
Var
 F:THandle;
 elf_hdr:elf64_hdr;

 arch_hdr:Tarch_header;

 size,offset,fsize:Int64;

begin
 Result:=False;
 if (name='') or (cb=nil) then Exit;
 F:=FileOpen(name,fmOpenRead or fmShareDenyNone);
 if (F=feInvalidHandle) then
 begin
  Writeln('Error open file:',name);
  Exit;
 end;
 FileRead(F,elf_hdr,SizeOf(elf_hdr));
 if (PDWORD(@elf_hdr.e_ident)^=ELFMAG) then //is elf
 begin
  _LoadSym(F,0,cb,data);
 end else
 if (Parch_sig(@elf_hdr.e_ident)^=arch_sig) then //is arch
 begin
  size:=FileSeek(F,0,fsFromEnd);
  offset:=FileSeek(F,SizeOf(Tarch_sig),0);
  While (offset>0) and (offset<size) do
  begin
   FileRead(F,arch_hdr,SizeOf(Tarch_header));
   offset:=FileSeek(F,0,fsFromCurrent);
   fsize:=StrToInt64Def(Trim(arch_hdr.FileSize),0);
   if (fsize<>0) then
   begin
    _LoadSym(F,offset,cb,data);
   end;
   offset:=offset+Align(fsize,2);
   offset:=FileSeek(F,offset,0);
  end;
 end else
 begin
  FileClose(F);
  Writeln(name,' is unknow file type!');
  Exit;
 end;

 FileClose(F);
end;

{
procedure FWriteStr(pName:Pchar;data:Pointer);
var
 S:RawByteString;
begin
 S:=pName+#13#10;
 FileWrite(THandle(data),PChar(S)^,Length(S));
end;
}

procedure FInsertStr(pName:Pchar;data:Pointer);
begin
 TRawStrSet(data).Insert(pName);
end;

function SaveSym(iList:TStringList;Const ofname:RawByteString;exclude:TRawStrSet):Boolean;
Var
 F:THandle;
 FSet:TRawStrSet;
 i,c:Integer;
 it:TRawStrSet.TIterator;
 S:RawByteString;
begin
 Result:=False;
 if (iList=nil) or (ofname='') then Exit;
 FSet:=TRawStrSet.Create;
 c:=iList.Count;
 if (c<>0) then
  For i:=0 to c-1 do
  begin
   LoadSymFromFile(iList.Strings[i],@FInsertStr,Pointer(FSet));
  end;

 F:=FileCreate(ofname);
 if (F=feInvalidHandle) then
 begin
  Writeln('Error create file:',ofname);
  FreeAndNil(FSet);
  Exit;
 end;

 it:=FSet.Min;
 if (it<>nil) then
 begin
  repeat
   S:=it.Data;
   if (exclude.NFind(S)=nil) then
   begin
    S:=S+#13#10;
    FileWrite(F,PChar(S)^,Length(S));
   end;
  until not it.Next;
  FreeAndNil(it);
 end;

 FileClose(F);
 FreeAndNil(FSet);
end;

var
 i,n:Integer;

 iList:TStringList;
 ofname:RawByteString='';

 exclude:TRawStrSet;

label
 promo;

procedure LoadFolder(iList:TStringList;const fname:RawByteString);
Var
 Info:TSearchRec;
 f,n:RawByteString;
begin
 If FindFirst(fname,faAnyFile,Info)=0 then
 begin
  f:=IncludeTrailingPathDelimiter(ExtractFileDir(fname));
  Repeat
   If (Info.Attr and faDirectory)=0 then
   begin
    n:=ExcludeLeadingPathDelimiter(f+Info.Name);
    iList.Add(n);
   end;
  Until FindNext(info)<>0;
  FindClose(Info);
 end;
end;

procedure LoadRecrusive(iList:TStringList;const fname:RawByteString);
Var
 Info:TSearchRec;
 f,n:RawByteString;
begin
 LoadFolder(iList,fname);
 f:=IncludeTrailingPathDelimiter(ExtractFileDir(fname));
 if (ExcludeLeadingPathDelimiter(f)='') then
 begin
  f:=IncludeTrailingPathDelimiter(GetCurrentDir);
 end;
 n:=ExtractFileName(fname);
 If FindFirst(f+'*',faDirectory,Info)=0 then
 begin
  Repeat
   Case Info.Name of
    '.','..':;
    else
     LoadRecrusive(iList,IncludeTrailingPathDelimiter(f+Info.Name)+n);
   end;
  Until FindNext(info)<>0;
  FindClose(Info);
 end;
end;

procedure LoadExclude(exclude:TRawStrSet;const fname:RawByteString);
var
 L:TStringList;
 i,c:Integer;
 s:RawByteString;
begin
 L:=TStringList.Create;
 try
  L.LoadFromFile(fname);
 except
  Writeln('Error open file:',fname);
  FreeAndNil(L);
  Exit;
 end;
 c:=L.Count;
 if (c<>0) then
  For i:=0 to c-1 do
  begin
   s:=Trim(L.Strings[i]);
   if (s<>'') then
    exclude.Insert(s);
  end;
 FreeAndNil(L);
end;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;

 iList:=TStringList.Create;
 exclude:=TRawStrSet.Create;

 if (ParamCount=0) then
 begin
  promo:
  Writeln('Tool for extract strings from *.prx.sym files');
  Writeln(' Parameters:');
  Writeln('  -i <name>  //file name to load');
  Writeln('  -o <name>  //file name to save');
  Writeln('  -f <name>  //folder and file mask to load');
  Writeln('  -r <name>  //folder and file mask to load (Recrusive)');
  Writeln('  -e <name>  //text file name to exclude values');
  Exit;
 end;

 n:=-1;
 For i:=1 to ParamCount do
 begin
  case LowerCase(ParamStr(i)) of
    '-i':n:=0;
    '-o':n:=1;
    '-f':n:=2;
    '-r':n:=3;
    '-e':n:=4;
   else
     if (n<>-1) then
     begin
      Case n of
       0:iList.Add(ParamStr(i));
       1:ofname:=ParamStr(i);
       2:LoadFolder(iList,ParamStr(i));
       3:LoadRecrusive(iList,ParamStr(i));
       4:LoadExclude(exclude,ParamStr(i));
      end;
      n:=-1;
     end;
  end;
 end;

 if (iList.Count=0) then goto promo;

 if (ofname='') then
 begin
  if (iList.Count=1) then
  begin
   ofname:=ChangeFileExt(iList.Strings[0],'.txt');
  end else
  begin
   ofname:='list.txt';
  end;
 end;

 SaveSym(iList,ofname,exclude);
 writeln;
end.



