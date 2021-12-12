{$mode objfpc}{$H+}

Uses
 classes,
 Sysutils,
 gmap,
 //spirv,
 UJson;

type
 TRawStrCompare=class
  class function c(var a,b:RawByteString):boolean; static;
 end;

 TMapStr=specialize TMap<RawByteString,RawByteString,TRawStrCompare>;
 TMapGroup=class(TMapStr)
  _type:RawByteString;
 end;

 TOpInfo=packed record
  op_min:Word;
  op_max:Word;
  result:Boolean;
  rstype:Boolean;
  align:Word;
 end;

 TOpInfoSet=specialize TMap<RawByteString,TOpInfo,TRawStrCompare>;

function ReCompareText(const S1,S2:RawByteString):sizeint;
var
 i,count1,count2: sizeint;
 Chr1, Chr2: byte;
 P1, P2: PChar;
begin
 Count1 := Length(S1);
 Count2 := Length(S2);
 if (Count1<>Count2) then Exit(Count1-Count2);
 if (Count1>0) then
 begin
   i := 0;
   P1 := @S1[1];
   P2 := @S2[1];
   while (i<Count1) do
   begin
    Chr1 := byte(p1[i]);
    Chr2 := byte(p2[i]);
    if (Chr1<>Chr2) then
    begin
     Exit(Chr1-Chr2);
    end;
    Inc(I);
   end;
 end;
end;

class function TRawStrCompare.c(var a,b:RawByteString):boolean;
begin
 Result:=ReCompareText(a,b)<0;
end;

var
 Comment:RawByteString;
 LConstMeta:TStringList;

 LEnums:TStringList;

 OpInfoSet:TOpInfoSet;

function _getComment(sComment:Tjson):RawByteString;
var
 i1,s1,i2,s2:Integer;
 r:RawByteString;
 tmp:Tjson;
begin
 Result:='';
 s1:=sComment.Count;
 if (s1<>0) then
 For i1:=0 to s1-1 do
 begin
  tmp:=sComment.Item[i1];
  s2:=tmp.Count;
  if (s2<>0) then
  For i2:=0 to s2-1 do
  begin
   r:=tmp.Item[i2].AsStr;
   r:=Trim(r);
   if (r<>'') then r:=' '+r;
   Result:=Result+r+#13#10;
  end;
  if (i1<>s1-1) then
   Result:=Result+#13#10;
 end;
 if (Result<>'') then Result:='{'#13#10+Result+'}'#13#10;
end;

Function GetPasLabel(_name:RawByteString):RawByteString;
begin
 Case _name of
  'Function',
  'Generic',
  'Private',
  'Repeat',
  'Const',
  'Inline',
  'Export':Result:=_name+'_';
  else
           Result:=_name;
 end;
end;

function _getGroup(sValues:Tjson;const _type:RawByteString):TMapGroup;
Var
 i,s:Integer;
 val:QWORD;
 _name,_value,tmp:RawByteString;
begin
 Result:=TMapGroup.Create;

 s:=sValues.Count;
 if (s<>0) then
 For i:=0 to s-1 do
  begin
   _name:=sValues.Name[i];
   _value:=sValues.Item[i].AsStr;

   if (_type='Bit') then
    if TryStrToQWord(_value,val) then
    begin
     _value:=IntToStr(1 shl val);
    end;

   if Result.TryGetValue(_value,tmp) then
   begin
    if (Length(_name)<Length(tmp)) then
    if (copy(tmp,Length(tmp)-2,3)<>'KHR') then
    begin
     Result.Delete(_value);
     Result.Insert(_value,_name);
    end;
   end else
   begin
    Result.Insert(_value,_name);
   end;
  end;

 if (_type='Bit') then
  Result.Insert('0','None');
end;

procedure LoadOp(LGroup:TMapGroup);
var
 IG:TMapGroup.TIterator;
begin
 OpInfoSet:=TOpInfoSet.Create;
 IG:=LGroup.Min;
 if Assigned(IG) then
 repeat
  OpInfoSet.Insert(IG.Value,Default(TOpInfo));
 until (not IG.Next);
 FreeAndNil(IG);
end;

procedure loadSpirvJson(Const fname:RawByteString);
Var
 J,meta,enum,tmp:Tjson;
 i,s:Integer;
 _name,_type:RawByteString;

 LGroup:TMapGroup;
begin
 J:=Tjson.NewFromFile(fname);

 Comment:=_getComment(J.Path['spv.meta.Comment']);

 LConstMeta:=TStringList.Create;
 meta:=J.Path['spv.meta'];
 s:=meta.Count;
 if (s<>0) then
 For i:=0 to s-1 do
  if (meta.Name[i]<>'Comment') then
  begin
   LConstMeta.Add(meta.Name[i]+' = '+meta.Item[i].AsStr);
  end;

 LEnums:=TStringList.Create;
 enum:=J.Path['spv.enum'];
 s:=enum.Count;
 if (s<>0) then
 For i:=0 to s-1 do
  begin
   tmp:=enum.Item[i];
   _name:=tmp.Path['Name'].AsStr;
   _type:=tmp.Path['Type'].AsStr;

   LGroup:=_getGroup(tmp.Path['Values'],_type);
   LGroup._type:=_type;

   LEnums.AddObject(_name,LGroup);

   if (_name='Op') then
    LoadOp(LGroup);

  end;

 J.Free;
end;

function _get_OpInfo(oper:Tjson):TOpInfo;
Var
 q,k:RawByteString;
 tmp:Tjson;
 i,s:Integer;
begin
 Result:=Default(TOpInfo);

 s:=oper.Count;
 if (s<>0) then
 For i:=0 to s-1 do
  begin
   tmp:=oper.Item[i];
   k:=tmp.Path['kind'].AsStr;

   case k of
    'IdResultType':Result.rstype:=True;
    'IdResult'    :Result.result:=True;
   end;

   q:=tmp.Path['quantifier'].AsStr;
   case q of
    '*':Result.op_max:=$FFFF;
    '?':if (Result.op_max<>$FFFF) then
        begin
         Result.op_max:=Result.op_max+1;
        end;
    else
        begin
         Result.op_min:=Result.op_min+1;
         if (Result.op_max<>$FFFF) then
         begin
          Result.op_max:=Result.op_max+1;
         end;
        end;
   end;

  end;

end;

procedure loadSpirvGrammarJson(Const fname:RawByteString);
Var
 J,inst,tmp:Tjson;
 i,s:Integer;
 opname:RawByteString;
 OpInfo:TOpInfo;
 IT:TOpInfoSet.TIterator;
begin
 J:=Tjson.NewFromFile(fname);

 inst:=J.Path['instructions'];

 s:=inst.Count;
 if (s<>0) then
 For i:=0 to s-1 do
  begin
   tmp:=inst.Item[i];
   opname:=tmp.Path['opname'].AsStr;
   OpInfo:=_get_OpInfo(tmp.Path['operands']);

   IT:=OpInfoSet.Find(opname);
   if Assigned(IT) then
   begin
    IT.Value:=OpInfo;
    FreeAndNil(IT);
   end;

  end;

 J.Free;
end;

Function IsJson(Const FName:RawByteString):Boolean;
begin
 Result:=False;
 Case UpperCase(ExtractFileExt(FName)) of
  '.JSON':Result:=True;
 end;
end;

Const
 prologf='unit spirv;'#$0D#$0A#$0D#$0A+
         '{$mode objfpc}{$H+}'#$0D#$0A#$0D#$0A+
         '{$WARNINGS OFF}'#$0D#$0A#$0D#$0A+
         'interface'#$0D#$0A#$0D#$0A;

 ep_impl='implementation'#$0D#$0A#$0D#$0A;

 ep_func='end.'#$0D#$0A;

 NL=#$0D#$0A;
 NLNL=#$0D#$0A#$0D#$0A;

 LConst='Const'#$0D#$0A;
 LType ='Type'#$0D#$0A;
 LEnd  =' end;';

 LGetStr_i='  function GetStr(w:Word):RawByteString; static;'#$0D#$0A;

 LGetInfo_i='  type'#$0D#$0A+
            '   TOpInfo=packed record'#$0D#$0A+
            '    op_min:Word;'#$0D#$0A+
            '    op_max:Word;'#$0D#$0A+
            '    result:Boolean;'#$0D#$0A+
            '    rstype:Boolean;'#$0D#$0A+
            '    align:Word;'#$0D#$0A+
            '   end;'#$0D#$0A+
            '  function GetInfo(w:Word):TOpInfo; static;'#$0D#$0A;

 LGetInfo_p='function Op.GetInfo(w:Word):TOpInfo; static;'#$0D#$0A+
            'begin'#$0D#$0A+
            ' Result:=Default(TOpInfo);'#$0D#$0A+
            ' Case w of'#$0D#$0A;

 LFunc='function ';

 LGetStr_p='.GetStr(w:Word):RawByteString;'#$0D#$0A+
           'begin'#$0D#$0A+
           ' Result:=''???'';'#$0D#$0A+
           ' Case w of'#$0D#$0A;

 LGetStr_e=' end;'#$0D#$0A+
           'end;'#$0D#$0A#$0D#$0A;

Procedure SaveToPas(Const FName:RawByteString);
Var
 F:Thandle;

 i,s:Integer;
 _name:RawByteString;

 LGroup:TMapGroup;
 IG:TMapGroup.TIterator;
 IT:TOpInfoSet.TIterator;

begin
 F:=FileCreate(FName);
 FileWrite(F,PChar(Comment)^,Length(Comment));
 FileWrite(F,PChar(prologf)^,Length(prologf));

 FileWrite(F,PChar(LConst)^,Length(LConst));
 s:=LConstMeta.Count;
 if (s<>0) then
 begin
  For i:=0 to s-1 do
  begin
   _name:=' '+LConstMeta.Strings[i]+';'+NL;
   FileWrite(F,PChar(_name)^,Length(_name));
  end;
  FileWrite(F,PChar(NL)^,Length(NL));
 end;

 FileWrite(F,PChar(LType)^,Length(LType));
 s:=LEnums.Count;
 if (s<>0) then
 begin
  For i:=0 to s-1 do
  begin
   LGroup:=TMapGroup(LEnums.Objects[i]);
   _name:=LEnums.Strings[i];

   _name:=' '+_name+'=object'+' //'+LGroup._type+NL+'  '+LConst;
   FileWrite(F,PChar(_name)^,Length(_name));

   IG:=LGroup.Min;
   if Assigned(IG) then
   repeat
    _name:='   '+GetPasLabel(IG.Value)+' = '+IG.Key+';'+NL;
    FileWrite(F,PChar(_name)^,Length(_name));
   until (not IG.Next);
   FreeAndNil(IG);

   if (LGroup._type='Value') then
    FileWrite(F,PChar(LGetStr_i)^,Length(LGetStr_i));

   if (LEnums.Strings[i]='Op') then
    FileWrite(F,PChar(LGetInfo_i)^,Length(LGetInfo_i));

   FileWrite(F,PChar(LEnd)^,Length(LEnd));
   FileWrite(F,PChar(NLNL)^,Length(NLNL));
  end;
 end;

 FileWrite(F,PChar(ep_impl)^,Length(ep_impl));
 s:=LEnums.Count;
 if (s<>0) then
 begin
  For i:=0 to s-1 do
  begin
   LGroup:=TMapGroup(LEnums.Objects[i]);
   if (LGroup._type<>'Value') then Continue;
   _name:=LEnums.Strings[i];

   _name:=LFunc+_name+LGetStr_p;

   FileWrite(F,PChar(_name)^,Length(_name));

   IG:=LGroup.Min;
   if Assigned(IG) then
   repeat
    _name:='  '+GetPasLabel(IG.Value)+':Result:='''+IG.Value+''';'+NL;
    FileWrite(F,PChar(_name)^,Length(_name));
   until (not IG.Next);
   FreeAndNil(IG);

   FileWrite(F,PChar(LGetStr_e)^,Length(LGetStr_e));

   if (LEnums.Strings[i]='Op') then
   begin
    FileWrite(F,PChar(LGetInfo_p)^,Length(LGetInfo_p));

    IG:=LGroup.Min;
    if Assigned(IG) then
    repeat

     IT:=OpInfoSet.Find(IG.Value);
     if Assigned(IT) then
     begin
      _name:='  '+GetPasLabel(IG.Value)+':QWORD(Result):=$'+HexStr(QWORD(IT.Value),16)+';'+NL;
      FileWrite(F,PChar(_name)^,Length(_name));
      FreeAndNil(IT);
     end;

    until (not IG.Next);
    FreeAndNil(IG);

    FileWrite(F,PChar(LGetStr_e)^,Length(LGetStr_e));
   end;

  end;
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

 loadSpirvJson('spirv.json');
 loadSpirvGrammarJson('spirv.core.grammar.json');
 Writeln('Load is Fin');
 SaveToPas('spirv.pas');
 Writeln('Save is Fin');

 readln;
end.

