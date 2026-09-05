unit param_sfo_gui;

{$mode ObjFPC}{$H+}

interface

uses
 param_sfo,
 sysutils,
 Classes,
 core_serialization;

const
 SFO_FORMAT_BLOB  =param_sfo.SFO_FORMAT_BLOB  ;
 SFO_FORMAT_STRING=param_sfo.SFO_FORMAT_STRING;
 SFO_FORMAT_UINT32=param_sfo.SFO_FORMAT_UINT32;

type
 TParamSfoValue=class(TSerializeObject)
  private
   Fformat:ptruint;
   Fname  :RawByteString;
   Fvalue :RawByteString;
  published
   property format:ptruint       read Fformat write Fformat;
   property name  :RawByteString read Fname   write Fname;
   property value :RawByteString read Fvalue  write Fvalue;
  public
   Function GetLength:QWORD;
   Function GetString:RawByteString;
   Function GetUInt  :DWORD;
   Function GetUInt64:QWORD;
 end;

 TParamSfoFile=class(TSerializeArray)
  //
  params:array of TParamSfoValue;
  //
  Destructor Destroy; override;
  //
  Function   IndexOf  (const name:RawByteString):Integer;
  Function   GetString(const name:RawByteString):RawByteString;
  Function   GetUInt  (const name:RawByteString):DWORD;
  //
  Function   GetArrayCount:SizeInt;          override;
  Function   GetArrayItem(i:SizeInt):TValue; override;
  Function   AddObject:TSerializeObject;     override;
  Function   AddArray :TSerializeArray;      override;
  procedure  AddValue(Value:TValue);         override;
  //
  procedure  Merge(src:TParamSfoFile);
 end;

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;

function LoadParamSfoByPath(const path:RawByteString):TParamSfoFile;
function TestParamSfoByPath(const path,title_id:RawByteString):Boolean;

procedure AutoDetectOverlays    (const path,title_id:RawByteString;dst:TStrings);
function  LoadParamSfoByOverlays(const path:RawByteString;overlays:TStrings):TParamSfoFile;

implementation

function LoadParamSfoByOverlays(const path:RawByteString;overlays:TStrings):TParamSfoFile;
var
 i:Integer;
 Tmp:TParamSfoFile;
begin
 Result:=LoadParamSfoByPath(path);
 if (Result=nil) then Exit;

 if (overlays.Count<>0) then
 For i:=0 to overlays.Count-1 do
 begin
  Tmp:=LoadParamSfoByPath(overlays.Strings[i]);
  if (Tmp<>nil) then
  begin
   Result.Merge(Tmp);
   FreeAndNil(Tmp);
  end;
 end;

end;

procedure AutoDetectOverlays(const path,title_id:RawByteString;dst:TStrings);
var
 V:RawByteString;
begin

 V:=ExcludeTrailingPathDelimiter(path)+'-patch';
 if TestParamSfoByPath(V,title_id) then
 begin
  dst.Add(V);
 end;

 V:=ExcludeTrailingPathDelimiter(path)+'-mods';
 if FileExists(V) then
 begin
  dst.Add(V);
 end;

end;

procedure TParamSfoFile.Merge(src:TParamSfoFile);
var
 s,d:Integer;
begin
 if (src=nil) then Exit;

 if (Length(src.params)<>0) then
 For s:=0 to High(src.params) do
 if (src.params[s].name<>'CATEGORY') then
 begin
  d:=Self.IndexOf(src.params[s].name);

  if (d=-1) then
  begin
   d:=Length(Self.params);
   Insert(TParamSfoValue.Create,Self.params,d);
  end;

  Self.params[d].format:=src.params[s].format;
  Self.params[d].name  :=src.params[s].name  ;
  Self.params[d].value :=src.params[s].value ;
 end;

end;

function LoadParamSfoByPath(const path:RawByteString):TParamSfoFile;
begin
 Result:=LoadParamSfoFile(ExcludeTrailingPathDelimiter(path)+
                          DirectorySeparator+
                          'sce_sys'+
                          DirectorySeparator+
                          'param.sfo');
end;

function TestParamSfoByPath(const path,title_id:RawByteString):Boolean;
var
 ParamSfo:TParamSfoFile;
begin
 ParamSfo:=LoadParamSfoByPath(path);
 //
 if (ParamSfo=nil) then Exit(False);

 Result:=(title_id=ParamSfo.GetString('TITLE_ID'));

 FreeAndNil(ParamSfo);
end;

//

procedure on_load(userdata:Pointer;name,value:pchar;format:WORD;size,max_size,i:DWORD);
var
 data_size:DWORD;
 svalue:RawByteString;
begin

  svalue:='';
  data_size:=max_size;

  case format of
   SFO_FORMAT_UINT32:
     begin
     if (data_size<4) then data_size:=4;
     end;
   else;
  end;

  SetLength(svalue,data_size);
  FillChar (svalue[1],data_size,0);

  Move(value^,svalue[1],size);

  case format of
   SFO_FORMAT_STRING:
     begin
      //fixup len
      SetLength(svalue,strlen(PChar(@svalue[1])));
     end;
   else;
  end;

  with TParamSfoFile(userdata) do
  begin
   params[i]:=TParamSfoValue.Create;
   params[i].format:=format;
   params[i].name  :=name;
   params[i].value :=svalue;
  end;

end;

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;
Var
 Loader:TParamSfoFileLoader;
begin
 Result:=nil;

 if not FileExists(path) then
 begin
  Exit;
 end;

 if not Loader.open(path) then
 begin
  Exit;
 end;

 if not Loader.parse() then
 begin
  Loader.Free;
  Exit;
 end;

 Result:=TParamSfoFile.Create;

 if (Loader.hdr.entry_count<>0) then
 begin
  SetLength(Result.params,Loader.hdr.entry_count);

  Loader.ForAll(@on_load,Pointer(Result));
 end;

 Loader.Free;
end;

//

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

Function TParamSfoValue.GetLength:QWORD;
begin
 Result:=Length(value);
end;

Function TParamSfoValue.GetString:RawByteString;
var
 D:DWORD;
begin
 Result:='';
 case format of
  SFO_FORMAT_BLOB:
    begin
     Result:='';
     if Length(value)<>0 then
     For D:=1 to Length(value) do
     begin
      Result:=Result+HexStr(Byte(value[D]),2);
     end;
    end;
  SFO_FORMAT_STRING:
    begin
     Result:=value;
    end;
  SFO_FORMAT_UINT32:
    begin
     D:=PDWORD(@value[1])^;
     Result:=UIntToStr(D);
    end;
  else;
 end;
end;

Function TParamSfoValue.GetUInt:DWORD;
var
 D:DWORD;
begin
 Result:=0;
 case format of
  SFO_FORMAT_BLOB:
    begin
     D:=0;
     Move(value[1],D,Min(SizeOf(DWORD),Length(value)));
     Result:=D;
    end;
  SFO_FORMAT_STRING:
    begin
     D:=0;
     TryStrToDWord(value,D);
     Result:=D;
    end;
  SFO_FORMAT_UINT32:
    begin
     D:=PDWORD(@value[1])^;
     Result:=D;
    end;
  else;
 end;
end;

Function TParamSfoValue.GetUInt64:QWORD;
var
 D:QWORD;
begin
 Result:=0;
 case format of
  SFO_FORMAT_BLOB:
    begin
     D:=0;
     Move(value[1],D,Min(SizeOf(QWORD),Length(value)));
     Result:=D;
    end;
  SFO_FORMAT_STRING:
    begin
     D:=0;
     TryStrToQWord(value,D);
     Result:=D;
    end;
  SFO_FORMAT_UINT32:
    begin
     D:=PDWORD(@value[1])^;
     Result:=D;
    end;
  else;
 end;
end;

//

Destructor TParamSfoFile.Destroy;
var
 i:Integer;
begin
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  FreeAndNil(params[i]);
 end;
 SetLength(params,0);
 inherited;
end;

Function TParamSfoFile.IndexOf(const name:RawByteString):Integer;
var
 i:Integer;
begin
 Result:=-1;
 if (Self=nil) then Exit;
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  if (params[i].name=name) then
  begin
   Exit(i);
  end;
 end;
end;

Function TParamSfoFile.GetString(const name:RawByteString):RawByteString;
var
 i:Integer;
begin
 Result:='';
 i:=IndexOf(name);
 if (i<>-1) then
 begin
  Result:=params[i].GetString;
 end;
end;

Function TParamSfoFile.GetUInt(const name:RawByteString):DWORD;
var
 i:Integer;
begin
 Result:=0;
 i:=IndexOf(name);
 if (i<>-1) then
 begin
  Result:=params[i].GetUInt;
 end;
end;

//////

Function TParamSfoFile.GetArrayCount:SizeInt;
begin
 Result:=Length(params);
end;

Function TParamSfoFile.GetArrayItem(i:SizeInt):TValue;
begin
 if (i>=Length(params)) then
 begin
  Result:=TValue.Empty;
 end else
 begin
  Result:=params[i];
 end;
end;

Function TParamSfoFile.AddObject:TSerializeObject;
begin
 Result:=TParamSfoValue.Create;
end;

Function TParamSfoFile.AddArray:TSerializeArray;
begin
 Result:=nil;
end;

procedure TParamSfoFile.AddValue(Value:TValue);
begin
 Insert(Value.AsObject,params,Length(params));
end;

end.

