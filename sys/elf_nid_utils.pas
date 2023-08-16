unit elf_nid_utils;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
  sha1;

function ps4_nid_hash (const name:RawByteString):QWORD;
function EncodeValue64(nVal:QWORD):RawByteString;
function DecodeValue64(strEnc:PAnsiChar;len:SizeUint;var nVal:QWORD):Boolean;
function DecodeValue16(strEnc:PAnsiChar;len:SizeUint;var nVal:WORD):Boolean;
function DecodeEncName(strEncName:PAnsiChar;var nModuleId,nLibraryId:WORD;var nNid:QWORD):Boolean;
function BaseEncName  (strEncName:PAnsiChar):RawByteString;

implementation

function ps4_nid_hash(const name:RawByteString):QWORD;
const
 salt:array[0..15] of Byte=($51,$8D,$64,$A6,$35,$DE,$D8,$C1,$E6,$B0,$39,$B1,$C3,$E5,$52,$30);
var
 Context:TSHA1Context;
 Digest:TSHA1Digest;
begin
 SHA1Init(Context);
 SHA1Update(Context,PChar(name)^,Length(name));
 SHA1Update(Context,salt,Length(salt));
 SHA1Final(Context,Digest);
 Result:=PQWORD(@Digest)^;
end;

function EncodeValue64(nVal:QWORD):RawByteString;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 Result:='';
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

function DecodeValue16(strEnc:PAnsiChar;len:SizeUint;var nVal:WORD):Boolean;
const
 nEncLenMax=3;
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
  begin
   nVal:=nVal shl 6;
   nVal:=nVal or nIndex;
  end;
 end;
 Result:=True;
end;

function DecodeEncName(strEncName:PAnsiChar;var nModuleId,nLibraryId:WORD;var nNid:QWORD):Boolean;
var
 i,len:Integer;
begin
 Result:=False;
 len:=StrLen(strEncName);
 i:=IndexByte(strEncName^,len,Byte('#'));
 if (i=-1) then Exit;
 if not DecodeValue64(strEncName,i,nNid) then Exit;
 Inc(i);
 Inc(strEncName,i);
 Dec(len,i);
 i:=IndexByte(strEncName^,len,Byte('#'));
 if (i=-1) then Exit;
 if not DecodeValue16(strEncName,i,nLibraryId) then Exit;
 Inc(i);
 Inc(strEncName,i);
 Dec(len,i);
 if not DecodeValue16(strEncName,len,nModuleId) then Exit;
 Result:=True;
end;

function BaseEncName(strEncName:PAnsiChar):RawByteString;
var
 i,len:Integer;
begin
 Result:=strEncName;
 len:=StrLen(strEncName);
 i:=IndexByte(strEncName^,len,Byte('#'));
 if (i=-1) then Exit;
 SetLength(Result,i);
end;

end.

