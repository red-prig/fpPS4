unit param_sfo_gui;

{$mode ObjFPC}{$H+}

interface

uses
 param_sfo,
 sysutils,
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
  params:array of TParamSfoValue;
  //
  Destructor Destroy; override;
  //
  Function   GetString(const name:RawByteString):RawByteString;
  Function   GetUInt  (const name:RawByteString):DWORD;
  //
  Function   GetArrayCount:SizeInt;          override;
  Function   GetArrayItem(i:SizeInt):TValue; override;
  Function   AddObject:TSerializeObject;     override;
  Function   AddArray :TSerializeArray;      override;
  procedure  AddValue(Value:TValue);         override;
 end;

function LoadParamSfoFile(const path:RawByteString):TParamSfoFile;

implementation

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

Function TParamSfoFile.GetString(const name:RawByteString):RawByteString;
var
 i:Integer;
begin
 Result:='';
 if (Self=nil) then Exit;
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  if (params[i].name=name) then
  begin
   Result:=params[i].GetString;
   Exit;
  end;
 end;
end;

Function TParamSfoFile.GetUInt(const name:RawByteString):DWORD;
var
 i:Integer;
begin
 Result:=0;
 if (Self=nil) then Exit;
 if (Length(params)=0) then Exit;
 For i:=0 to High(params) do
 begin
  if (params[i].name=name) then
  begin
   Result:=params[i].GetUInt;
   Exit;
  end;
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

