unit core_shell;

{$mode ObjFPC}{$H+}

interface

uses
 core_serialization;

function  encode_shell(const src:RawByteString):RawByteString;
function  encode_shell(argv:TSerializeStringArray):RawByteString;

procedure free_params(argv:PPChar);
function  parse_params(const params:RawByteString;var argv:PPChar):Integer;

implementation

function encode_shell(const src:RawByteString):RawByteString;
var
 i:Integer;
begin
 if (Pos(' ',src)=0) then
 begin
  Result:=src;
 end else
 if (Pos('"',src)=0) then
 begin
  Result:='"'+src+'"';
 end else
 if (Pos('''',src)=0) then
 begin
  Result:=''''+src+'''';
 end else
 begin
  Result:='"';
  For i:=1 to Length(src) do
  begin
   if (src[i]='"') then
   begin
    Result:=Result+'"'+'\"'+'"';
   end else
   begin
    Result:=Result+src[i];
   end;
  end;
  Result:=Result+'"';
 end;
end;

function encode_shell(argv:TSerializeStringArray):RawByteString;
var
 i:Integer;
begin
 Result:='';
 if Length(argv.values)<>0 then
 begin
  For i:=0 to High(argv.values) do
  begin
   if (Result<>'') then Result:=Result+' ';
   Result:=Result+encode_shell(argv.values[i]);
  end;
 end;
end;

//

procedure free_params(argv:PPChar);
var
 curr:PPChar;
begin
 if (argv=nil) then Exit;
 curr:=argv;
 while (curr^<>nil) do
 begin
  FreeMem(curr^);
  Inc(curr);
 end;
 FreeMem(argv);
end;

function parse_params(const params:RawByteString;var argv:PPChar):Integer;
var
 curr:PChar;
 last:PChar;

 barg:PChar;
 blen:Integer;

 argc:Integer;

 state:char;

 procedure concat_arg(delta:Integer);
 var
  i:Integer;
 begin
  if (curr<>last) then
  begin
   i:=(curr-last);
   ReAllocMem(barg,blen+i+1); //zero truncate
   Move(last^,barg[blen],i);
   blen:=blen+i;
   barg[blen]:=#0;
  end;
  last:=curr+delta;
 end;

 procedure next_arg;
 begin
  if (barg<>nil) then
  begin
   ReAllocMem(argv,SizeOf(Pointer)*(argc+1+1)); //zero truncate
   argv[argc]:=barg;
   Inc(argc);
   argv[argc]:=nil; //truncate
   barg:=nil;       //reset
   blen:=0;         //reset
  end;
 end;

begin
 Result:=1;

 //init
 argc:=0;
 argv:=AllocMem(SizeOf(Pointer)*2);
 argv[0]:=nil; //truncate

 curr:=@params[1];
 last:=curr;

 barg:=nil;
 blen:=0;

 state:=#0;

 if (curr<>nil) then
 while (curr^<>#0) do
 begin

  case state of
   ' ':
     if (curr^<>' ') then
     begin
      last:=curr; //update pos
      state:=#0;
     end;
   '\':
     begin
      last:=curr; //update pos
      state:=#0;
      //skip
      Inc(curr);
      Continue;
     end;
   else;
  end;

  case curr^ of

   ' ':
     begin
      if (state=#0) then
      begin
       concat_arg(1);
       next_arg;
       state:=' ';
      end;
     end;

   '''',
    '"':
     begin
      if (state=#0) then
      begin
       concat_arg(1);
       state:=curr^;
      end else
      if (state=curr^) then
      begin
       concat_arg(1);
       state:=#0;
      end;
     end;

    '\':
     begin
      concat_arg(1);
      state:='\';
     end;

   else;
  end;

  Inc(curr);
 end;

 if (state<>' ') then
 begin
  concat_arg(0);
  next_arg;
 end;

 Result:=argc;
end;

end.

