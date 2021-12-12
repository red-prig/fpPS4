
uses

 gset,
 gmap,
 Classes,SysUtils;

type
 TRawStrCompare=class
  class function c(var a,b:RawByteString):boolean; static;
 end;

 TMapStr=specialize TMap<RawByteString,RawByteString,TRawStrCompare>;
 TSetStr=specialize TSet<RawByteString,TRawStrCompare>;

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

type
 TChars=Set of AnsiChar;

function FetchAny(var Value:RawByteString;Delimiters,Quotations:TChars):RawByteString;
Var
 i:SizeUInt;
 Quote:AnsiChar;
 State:Byte;
begin
 Result:='';
 Quote:=#0;
 State:=0;
 if Length(Value)>0 then
 begin
  For i:=1 to Length(Value) do
  begin
   case State of
    0:begin
       if (Value[i] in Quotations)  then
       begin
        State:=2;
        Quote:=Value[i];
       end else
       if (Value[i] in Delimiters) then
       begin

       end else
       begin
        Result:=Result+Value[i];
        State:=1;
       end;
      end;
    1:begin
       if (Value[i] in Quotations)  then
       begin
        State:=2;
        Quote:=Value[i];
       end else
       if (Value[i] in Delimiters) then
       begin
        System.Delete(Value,1,i);
        Exit;
       end else
       begin
        Result:=Result+Value[i];
       end;
      end;
    2:begin
       if Value[i]=Quote then
       begin
        State:=3;
       end else
       begin
        Result:=Result+Value[i];
       end;
      end;
    3:begin
       if Value[i]=Quote then
       begin
        State:=2;
        Result:=Result+Quote;
       end else
       if (Value[i] in Delimiters) then
       begin
        System.Delete(Value,1,i);
        Exit;
       end else
       begin
        State:=1;
        Quote:=#0;
        Result:=Result+Value[i];
       end;
      end;
   end;
  end;
  Value:='';
 end;
end;

Procedure CutEnd(var Value:RawByteString;const S:RawByteString);
begin
 if Copy(Value,Length(Value)-Length(S)+1,Length(S))=S then
 begin
  Delete(Value,Length(Value)-Length(S)+1,Length(S));
 end;
end;

function BeginIs(const Value,S:RawByteString):Boolean;
begin
 Result:=Copy(Value,1,Length(S))=S;
end;

function EndIs(const Value,S:RawByteString):Boolean;
begin
 Result:=Copy(Value,Length(Value)-Length(S)+1,Length(S))=S;
end;

Var
 RMNV_offsets:TMapStr;
 RMVN_offsets:TMapStr;
 RMNV_default:TMapStr;
 RMVN_default:TMapStr;

 Excl_default:TSetStr;

Procedure load_offsets(const fname:RawByteString);
var
 L:TStringList;
 i:Integer;
 S,Name,Value:RawByteString;
 maxlen:Integer;
 It:TMapStr.TIterator;
 F:THandle;
begin
 RMNV_offsets:=TMapStr.Create;
 RMVN_offsets:=TMapStr.Create;
 maxlen:=0;
 L:=TStringList.Create;
 L.LoadFromFile(fname);
 For i:=0 to L.Count -1 do
 begin
  S:=L.Strings[i];
  Case FetchAny(S,[' ',#9],[]) of
   'constexpr':
    begin
     if (FetchAny(S,[' ',#9],[])='unsigned') then
     if (FetchAny(S,[' ',#9],[])='int') then
     begin
      Name:=FetchAny(S,[' ',#9],[]);

      if (not EndIs(Name,'__SI')) and
         (not EndIs(Name,'__SI__CI')) and
         (not BeginIs(Name,'mmDBG')) and
         (
          BeginIs(Name,'mmCB') or
          BeginIs(Name,'mmDB') or
          BeginIs(Name,'mmGB') or
          BeginIs(Name,'mmGRBM') or
          BeginIs(Name,'mmPA') or
          BeginIs(Name,'mmSPI') or
          //BeginIs(Name,'mmSX') or
          //BeginIs(Name,'mmSQ') or
           //BeginIs(Name,'mmTA') or
          BeginIs(Name,'mmVGT') or
          BeginIs(Name,'mmIA') or
          BeginIs(Name,'mmCOMPUTE')) then



       if (FetchAny(S,[' ',#9],[])='=') then
       begin
        CutEnd(Name,'__CI__VI');
        CutEnd(Name,'__VI');
        Value:=FetchAny(S,[' ',#9,';'],[]);
        if BeginIs(Value,'0x') then
        begin
         System.Delete(Value,1,2);
         Value:='$'+Value;
        end;
        it:=RMNV_offsets.Find(Name);
        if Assigned(It) then
        begin
         if (It.Value<>Value) then
          Writeln('Double:',Name,'=',Value,'<>',It.Value);
         FreeAndNil(It);
        end else
        if StrToIntDef(Value,0)<=$FFFF then
        begin
         it:=RMVN_offsets.Find(Value);
         if Assigned(It) then
         begin
          if (It.Value<>Name) then
           Writeln('Double:',Name,'=',Value,'<>',It.Value);
          FreeAndNil(It);
         end;
         if Length(Name)>maxlen then maxlen:=Length(Name);
         RMNV_offsets.Insert(Name,Value);
         RMVN_offsets.Insert(Value,Name);
        end;
       end;
     end;
    end;
  end;

 end;
 L.Free;

 F:=FileCreate(ChangeFileExt(fname,'.pas'));

 S:='unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10#13#10+
    'interface'#13#10#13#10+
    '{$mode objfpc}{$H+}'#13#10#13#10+
    'const'#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 It:=RMVN_offsets.Min;
 if Assigned(It) then
 begin
  repeat
   S:=' '+It.Value+Space(maxlen-Length(It.Value))+'='+It.Key+';'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  until not It.Next;
  FreeAndNil(It);
 end;

 S:=#13#10'function getRegName(i:Word):RawByteString;'#13#10#13#10+
    'implementation'#13#10#13#10+
    'function getRegName(i:Word):RawByteString;'#13#10+
    'begin'#13#10+
    ' case i of'#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 It:=RMVN_offsets.Min;
 if Assigned(It) then
 begin
  repeat
   S:='  '+It.Value+Space(maxlen-Length(It.Value))+':Result:='#$27+It.Value+#$27';'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  until not It.Next;
  FreeAndNil(It);
 end;

 S:=' else'#13#10+
    '  Result:=HexStr(i,4);'#13#10+
    ' end;'#13#10+
    'end;'#13#10+
    #13#10'end.'#13#10#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 FileClose(F);
end;

//

type
 TUnionList=class(TStringList)
  public
   name:RawByteString;
 end;
 TStructList=TUnionList;

 TMapUnionList=specialize TMap<RawByteString,TUnionList,TRawStrCompare>;

var
 UnionList:TMapUnionList;

Procedure load_registers(const fname:RawByteString);
var
 L:TStringList;
 maxlen:Integer;
 i,w:Integer;
 S:RawByteString;
 It:TMapUnionList.TIterator;
 F:THandle;

 is_union,is_struct:Boolean;
 reserved:Integer;
 union_field:TUnionList;
 struct_field:TStructList;

 name,value:RawByteString;

begin
 UnionList:=TMapUnionList.Create;
 is_union:=false;
 is_struct:=false;
 L:=TStringList.Create;
 L.LoadFromFile(fname);
 For i:=0 to L.Count -1 do
 begin
  S:=L.Strings[i];
  Case FetchAny(S,[' ',#9],[]) of
   'union':
     begin
      name:=FetchAny(S,[' ',#9],[]);
      if (not EndIs(Name,'__SI')) and
         (not EndIs(Name,'__CI')) and
         (not EndIs(Name,'__SI__CI')) and
         (not BeginIs(Name,'mmDBG')) and
         (
          BeginIs(Name,'CB') or
          BeginIs(Name,'DB') or
          BeginIs(Name,'GB') or
          BeginIs(Name,'GRBM') or
          BeginIs(Name,'PA') or
          BeginIs(Name,'SPI') or
          //BeginIs(Name,'SX') or
          //BeginIs(Name,'SQ') or
           //BeginIs(Name,'TA') or
          BeginIs(Name,'VGT') or
          BeginIs(Name,'IA') or
          BeginIs(Name,'COMPUTE')) then
      begin
       CutEnd(Name,'__CI__VI');
       CutEnd(Name,'__VI');
       is_union:=True;
       is_struct:=false;
       reserved:=0;
       union_field:=TUnionList.Create;
       union_field.name:=name;
      end;
     end;
   '};':
     if is_union then
     begin
      is_union:=false;
      is_struct:=false;
      It:=UnionList.Find(union_field.name);
      If Assigned(It) then
      begin
       Writeln('Double:',union_field.name);
       FreeAndNil(It);
      end else
      begin
       UnionList.Insert(union_field.name,union_field);
      end;
      union_field:=nil;
     end;
   'struct':
     if is_union then
     begin
      is_struct:=True;
      struct_field:=TStructList.Create;
      struct_field.NameValueSeparator:=':';
     end;
   '}':
     if is_struct then
     begin
      is_struct:=False;
      Name:=FetchAny(S,[' ',#9,',',';'],[]);
      CutEnd(Name,'__CI__VI');
      CutEnd(Name,'__VI');
      CutEnd(Name,'__SI');
      struct_field.name:=Name;
      union_field.AddObject(struct_field.name,struct_field);
      struct_field:=nil;
     end;
   'unsigned':
     if is_struct then
     if (FetchAny(S,[' ',#9],[])='int') then
     begin
      name:=FetchAny(S,[' ',#9],[]);
      if (name=':') then
      begin
       repeat
        name:='RESERVED'+IntToStr(reserved);
        Inc(reserved);
       until (struct_field.IndexOfName(name)=-1);
      end else
      if FetchAny(S,[' ',#9],[])<>':' then
      begin
       Writeln('wtf?:',i);
      end;
      CutEnd(Name,'__CI__VI');
      CutEnd(Name,'__VI');
      CutEnd(Name,'__SI');
      Case name of
       'OVERRIDE',
       'TYPE':name:='_'+name;
      end;
      value:=FetchAny(S,[' ',#9,';'],[]);
      struct_field.Add(name+':bit'+value);
     end;
  end;

 end;
 L.Free;

 F:=FileCreate(ChangeFileExt(fname,'.pas'));

 S:='unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10#13#10+
    '{$mode objfpc}{$H+}'#13#10#13#10+
    'interface'#13#10#13#10+
    'uses'#13#10+
    ' bittype;'#13#10#13#10+
    'type'#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 It:=UnionList.Min;
 if Assigned(It) then
 begin
  repeat
   union_field:=It.Value;

   if (union_field.Count=1) then
   begin
    struct_field:=TStructList(union_field.Objects[0]);
    if (struct_field.Count=1) then
    begin
     struct_field.GetNameValue(0,String(name),String(Value));
     S:=' T'+union_field.name+'='+Value+';'+#13#10#13#10;
     FileWrite(F,Pchar(S)^,Length(S));
    end else
    begin
     S:=' T'+union_field.name+'=bitpacked record'#13#10;
     FileWrite(F,Pchar(S)^,Length(S));

     maxlen:=0;
     For i:=0 to struct_field.Count-1 do
     begin
      name:=struct_field.Names[i];
      if Length(name)>maxlen then maxlen:=Length(name);
     end;

     For i:=0 to struct_field.Count-1 do
     begin
      struct_field.GetNameValue(i,String(name),String(Value));
      S:='  '+name+Space(maxlen-Length(name))+':'+Value+';'#13#10;
      FileWrite(F,Pchar(S)^,Length(S));
     end;

     S:=' end;'#13#10#13#10;
     FileWrite(F,Pchar(S)^,Length(S));
    end;
   end else
   begin
    S:=' T'+union_field.name+'=packed record'#13#10+
       '  Case Byte of'#13#10;
    FileWrite(F,Pchar(S)^,Length(S));

    For w:=0 to union_field.Count-1 do
    begin
     S:='   '+IntToStr(w)+':('#13#10;
     FileWrite(F,Pchar(S)^,Length(S));

     struct_field:=TStructList(union_field.Objects[w]);

     if (struct_field.Count=1) then
     begin
      struct_field.GetNameValue(0,String(name),String(Value));
      S:='    '+struct_field.name+':'+Value+');'+#13#10;
      FileWrite(F,Pchar(S)^,Length(S));
     end else
     begin
      S:='    '+struct_field.name+':bitpacked record'#13#10;
      FileWrite(F,Pchar(S)^,Length(S));

      maxlen:=0;
      For i:=0 to struct_field.Count-1 do
      begin
       name:=struct_field.Names[i];
       if Length(name)>maxlen then maxlen:=Length(name);
      end;

      For i:=0 to struct_field.Count-1 do
      begin
       struct_field.GetNameValue(i,String(name),String(Value));
       S:='     '+name+Space(maxlen-Length(name))+':'+Value+';'#13#10;
       FileWrite(F,Pchar(S)^,Length(S));
      end;

      S:='    end);'#13#10;
      FileWrite(F,Pchar(S)^,Length(S));
     end;

    end;

    S:=' end;'#13#10#13#10;
    FileWrite(F,Pchar(S)^,Length(S));

   end;

  until not It.Next;
  FreeAndNil(It);
 end;

 S:=#13#10'implementation'#13#10#13#10+
    'end.'#13#10#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 FileClose(F);
end;

type
 TEnum=TUnionList;

var
 Enum_Set:TSetStr;
 EnumList:TStringList;

Procedure load_enum(const fname:RawByteString);
var
 L:TStringList;
 maxlen:Integer;
 i,e:Integer;
 S,name,value:RawByteString;

 Enum:TEnum;

 is_enum:Boolean;

 F:THandle;

begin
 Enum_Set:=TSetStr.Create;
 EnumList:=TStringList.Create;
 is_enum:=False;
 maxlen:=0;

 L:=TStringList.Create;
 L.LoadFromFile(fname);
 For i:=0 to L.Count-1 do
 begin
  S:=L.Strings[i];
  name:=FetchAny(S,[' ',#9],[]);
  Case name of
   'typedef':
     begin
      if FetchAny(S,[' ',#9],[])='enum' then
      begin
       name:=FetchAny(S,[' ',#9,'{'],[]);
       is_enum:=True;
       Enum:=TEnum.Create;
       Enum.name:=name;
       Enum.NameValueSeparator:='=';
      end;
     end;
   '}':
     if is_enum then
     begin
      EnumList.AddObject(Enum.name,Enum);
      Enum:=nil;
      is_enum:=False;
     end;
   else
     if is_enum then
     begin
      if (not EndIs(Name,'__SI')) and
         (not EndIs(Name,'__CI')) and
         (not EndIs(Name,'__SI__CI')) then
      if FetchAny(S,[' ',#9],[])='=' then
      begin
       CutEnd(Name,'__CI__VI');
       CutEnd(Name,'__VI');
       CutEnd(Name,'__SI');
       value:=FetchAny(S,[' ',#9,','],[]);
       if BeginIs(Value,'0x') then
       begin
        System.Delete(Value,1,2);
        Value:='$'+Value;
       end;
       if Enum_Set.NFind(Name)=nil then
       begin
        if Length(name)>maxlen then maxlen:=Length(name);
        Enum_Set.Insert(Name);
        Enum.Add(Name+'='+value);
       end else
       begin
        Writeln('Double enum:',Name);
       end;
      end;
     end;
  end;
 end;
 L.Free;

 F:=FileCreate(ChangeFileExt(fname,'.pas'));

 S:='unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10#13#10+
    '{$mode objfpc}{$H+}'#13#10#13#10+
    'interface'#13#10#13#10+
    'Const'#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  S:=' // '+Enum.name+#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  For e:=0 to Enum.Count-1 do
  begin
   Enum.GetNameValue(e,String(name),String(Value));

   S:=' '+name+Space(maxlen-Length(name))+'='+Value+';'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));

  end;
 end;

 S:=#13#10'implementation'#13#10#13#10+
    'end.'#13#10#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 FileClose(F);

end;

begin
 load_offsets('si_ci_vi_merged_offset.h');
 load_registers('si_ci_vi_merged_registers.h');
 load_enum('si_ci_vi_merged_enum.h');
 readln;
end.

