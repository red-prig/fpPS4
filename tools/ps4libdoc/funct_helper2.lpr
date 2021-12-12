{$mode objfpc}{$H+}

Uses
 classes,
 Sysutils,
 sha1,
 gmap;

type
 TQWORDCompare=class
  class function c(var a,b:QWORD):boolean; static;
 end;
 TMapSym=specialize TMap<QWORD,RawByteString,TQWORDCompare>;

class function TQWORDCompare.c(var a,b:QWORD):boolean;
begin
 Result:=a<b;
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

procedure loadInlFile(MapSym:TMapSym;Const fname:RawByteString);
Var
 List:TStringList;
 i,p:SizeInt;
 S,name,sid:RawByteString;
 nid:QWORD;
begin
 nid:=0;
 List:=TStringList.Create;
 List.LoadFromFile(fname);
 For i:=0 to List.Count-1 do
 begin
  S:=List.Strings[i];
  p:=Pos('(',S);
  if (p<>0) then S:=copy(s,p+1,Length(s)-p);
  if (s[Length(s)]=')') then SetLength(s,Length(s)-1);
  p:=Pos(',',S);
  if (p<>0) then
  begin
   name:=copy(s,1,p-1);
   sid:=copy(s,p+1,Length(s)-p);
   if (sid[Length(sid)]='"') then SetLength(sid,Length(sid)-1);
   if (sid[1]='"') then sid:=copy(sid,2,Length(sid)-1);
   if not DecodeValue64(PChar(sid),Length(sid),nid) then
   begin
    Writeln('Error decode with:',sid);
    Continue;
   end;
   if (name<>'') then
   begin
    if ps4_nid_hash(pchar(name))<>nid then
    begin
     Writeln('Error: nid hash check:',nid,'<>',ps4_nid_hash(pchar(name)));
    end;
    if MapSym.TryGetValue(nid,s) and (s<>name) then
    begin
     Writeln('Error: nid hash collision:',nid,':',s,'<>',name);
    end else
     MapSym.Insert(nid,name);
   end;

  end;
 end;
 FreeAndNil(List);
end;

procedure loadTxtFile(MapSym:TMapSym;Const fname:RawByteString);
Var
 List:TStringList;
 i:SizeInt;
 name,s:RawByteString;
 nid:QWORD;
begin
 nid:=0;
 List:=TStringList.Create;
 List.LoadFromFile(fname);
 For i:=0 to List.Count-1 do
 begin
  name:=Trim(List.Strings[i]);
  if (name<>'') then
  begin
   nid:=ps4_nid_hash(pchar(name));
   if MapSym.TryGetValue(nid,s) and (s<>name) then
   begin
    Writeln('Error: nid hash collision:',nid,':',s,'<>',name);
   end else
    MapSym.Insert(nid,name);
  end;
 end;
 FreeAndNil(List);
end;

Var
 MapSym:TMapSym;

Const
 prolog1='unit ps4libdoc;'#$0D#$0A#$0D#$0A+
         '{$mode objfpc}{$H+}'#$0D#$0A#$0D#$0A+
         '{$WARNINGS OFF}'#$0D#$0A#$0D#$0A+
         'interface'#$0D#$0A#$0D#$0A+
         'Function GetFunctName(N:QWORD):PChar;'#$0D#$0A#$0D#$0A+
         'implementation'#$0D#$0A#$0D#$0A+

         'type'#$0D#$0A+
         ' TFunctData=packed record'#$0D#$0A+
         '  i:QWORD;'#$0D#$0A+
         '  n:PChar;'#$0D#$0A+
         ' end;'#$0D#$0A#$0D#$0A+

         'var'#$0D#$0A+
         ' FunctData:array[0..';

 prolog2='] of TFunctData=('#$0D#$0A;

 ep_func=' );'#$0D#$0A#$0D#$0A+
         'Function GetFunctName(N:QWORD):PChar;'#$0D#$0A+
         'Var'#$0D#$0A+
         ' l,r,m:SizeInt;'#$0D#$0A+
         'begin'#$0D#$0A+
         ' Result:='#$27'Unknow'#$27';'#$0D#$0A+
         ' l:=0;'#$0D#$0A+
         ' r:=Length(FunctData);'#$0D#$0A+
         ' while (l<r) do'#$0D#$0A+
         ' begin'#$0D#$0A+
         '  m:=l+(r-l) div 2;'#$0D#$0A+
         '  if (FunctData[m].i=N) then Exit(FunctData[m].n);'#$0D#$0A+
         '  if (FunctData[m].i>N) then'#$0D#$0A+
         '  begin'#$0D#$0A+
         '   r:=m;'#$0D#$0A+
         '  end else'#$0D#$0A+
         '  begin'#$0D#$0A+
         '   l:=m+1;'#$0D#$0A+
         '  end;'#$0D#$0A+
         ' end;'#$0D#$0A+
         'end;'#$0D#$0A#$0D#$0A+
         'end.'#$0D#$0A;

Procedure SaveToPas(Const FName:RawByteString);
Var
 F:Thandle;

 SymIter:TMapSym.TIterator;
 i,_id:QWORD;
 _name:RawByteString;
begin
 F:=FileCreate(FName);
 FileWrite(F,PChar(prolog1)^,Length(prolog1));

 _name:=IntToStr(MapSym.Size-1);
 FileWrite(F,PChar(_name)^,Length(_name));

 FileWrite(F,PChar(prolog2)^,Length(prolog2));

 i:=0;
 SymIter:=MapSym.Min;
 if (SymIter<>nil) then
 begin
  repeat
   Inc(i);

   _id  :=SymIter.Key;
   _name:=SymIter.Value;
   _name:='  (i:$'+HexStr(_id,16)+';n:'#$27+_name+#$27')';
   if (i=MapSym.Size) then
    _name:=_name+#$0D#$0A
   else
    _name:=_name+','#$0D#$0A;

   FileWrite(F,PChar(_name)^,Length(_name));
  until (not SymIter.Next);
  FreeAndNil(SymIter);
 end;

 FileWrite(F,PChar(ep_func)^,Length(ep_func));
 FileClose(F);
end;

begin
 DefaultSystemCodePage:=CP_UTF8;
 DefaultUnicodeCodePage:=CP_UTF8;
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 UTF8CompareLocale:=CP_UTF8;

 MapSym:=TMapSym.Create;
 if FileExists('ps4libdoc.inl') then
  loadInlFile(MapSym,'ps4libdoc.inl');
 if FileExists('list.txt') then
  loadTxtFile(MapSym,'list.txt');
 Writeln('Load is Fin');
 SaveToPas('ps4libdoc.pas');
 Writeln('Save is Fin');

 readln;
end.

