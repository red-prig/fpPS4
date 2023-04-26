
uses
 Classes,
 SysUtils;

type
 TChars=Set of AnsiChar;

Function CountOf(var Value:RawByteString;Delimiters:TChars):SizeUInt;
Var
 i:SizeUInt;
begin
 Result:=0;
 if Length(Value)>0 then
 begin
  For i:=1 to Length(Value) do
  begin
   if (Value[i] in Delimiters) then Inc(Result);
  end;
 end;
end;

procedure DelChars(var Value:RawByteString;Delimiters:TChars);
Var
 i:SizeUInt;
begin
 if Length(Value)>0 then
 begin
  i:=1;
  While (i<=Length(Value)) do
  begin
   if (Value[i] in Delimiters) then
   begin
    System.Delete(Value,i,1);
   end else
   begin
    Inc(i);
   end;
  end;
 end;
end;

function SpaceChars(b:byte;ch:char):RawByteString;
begin
 SetLength(Result,b);
 FillChar(Result[1],b,ch);
end;

procedure TrimComm(var Value:RawByteString;Delimiters:TChars);
Var
 i:SizeUInt;
begin
 if Length(Value)>0 then
 begin
  For i:=1 to Length(Value) do
  begin
   if (Value[i] in Delimiters) then
   begin
    System.Delete(Value,i,Length(Value)-i+1);
    Exit;
   end;
  end;
 end;
end;

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

type
 TEnum=class(TStringList)
  public
   name:RawByteString;
   namelen:SizeUInt;
   pre :RawByteString;
   post:RawByteString;
 end;

var
 EnumList:TStringList;

function FixFieldName(const S:RawByteString):RawByteString;
begin
 Case S of
  'type' ,
  'label',
  'file' ,
  'a'    ,
  'v'    ,
  's'    :Result:='_'+S;
  else
          Result:=S;
 end;
end;

function FixFuncName(const S:RawByteString):RawByteString;
begin
 Case S of
  'vop_lock1':Result:='vop_lock';
  else
              Result:=S;
 end;
end;

Procedure load_if(const fname:RawByteString);
var
 L:TStringList;
 i,x:Integer;
 S,Name:RawByteString;

 pre,post:RawByteString;

 F:THandle;

 Enum:TEnum;

 state:Integer;

 links,maxlen:SizeUInt;
begin
 EnumList:=TStringList.Create;

 state:=0;
 pre :='';
 post:='';

 L:=TStringList.Create;
 L.LoadFromFile(fname);
 For i:=0 to L.Count -1 do
 begin
  S:=L.Strings[i];

  TrimComm(S,['#']);

  Name:=FetchAny(S,[' ',#9],[]);

  Case Name of
   ''  :;
   '%%':;
   '%!':begin //pre/post
         Name:=FetchAny(S,[' ',#9],[]); //enum name
         Name:=FetchAny(S,[' ',#9],[]); //cmd

         //exclude
         Case S of
          'vop_strategy_pre',
          'vop_lock_pre',
          'vop_lock_post',
          'vop_unlock_pre',
          'vop_unlock_post':S:='';
          else;
         end;

         Case Name of
          'pre' :pre :=S;
          'post':post:=S;
          else;
         end;
        end;
   '};':begin //close
         state:=0;
         pre :='';
         post:='';
         Enum:=nil;
        end;
   else
    Case state of
     0:begin //open
        Case Name of
         'vop_spare4':state:=2; //skip
         'vop_spare5':state:=2; //skip
         else
          begin
           Enum:=TEnum.Create;
           Enum.name:=name;
           Enum.NameValueSeparator:=':';

           Enum.pre :=pre;
           Enum.post:=post;

           EnumList.AddObject(Enum.name,Enum);

           state:=1;
          end;
        end;
       end;
     1:begin //value

        repeat
         case Name of
          'IN':;
          'OUT':;
          'INOUT':;
          'WILLRELE':;
          'struct':;
          'const':;
          else
           Break;
         end;
         Name:=FetchAny(S,[' ',#9],[]);
        until false;

        Case Name of
         'thread':Continue; //skip
         'ucred' :Continue; //skip
        end;

        links:=CountOf(S,['*']);
        DelChars(S,['*',';']);
        S:=Trim(S);

        Case Name of
         'int'       :Name:=SpaceChars(links,'P')+'Integer';
         'void'      :Name:=SpaceChars(links-1,'P')+'Pointer';
         'u_long',
         'register_t',
         'vm_ooffset_t',
         'size_t',
         'off_t'     :Name:=SpaceChars(links,'P')+'PtrUint';
         'char'      :Name:=SpaceChars(links,'P')+'Char';
         else
                     begin
                      if (links<>0) then
                       Name:=SpaceChars(links,'p')+'_'+Name;
                     end;
        end;

        Case S of
         'end'  :S:='__end';
         else;
        end;

        if (Length(S)>Enum.namelen) then Enum.namelen:=Length(S);

        Enum.AddPair(S,Name);
        Writeln(S,':',Name);
       end;
    end;
  end;

 end;
 L.Free;

 F:=FileCreate(ChangeFileExt(fname,'.pas'));

 S:='unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10#13#10+
    'interface'#13#10#13#10+
    '{$mode objfpc}{$H+}'#13#10+
    '{$CALLING SysV_ABI_CDecl}'#13#10#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 S:='uses'#13#10;
 S:=S+' vfs_vnode,'#13#10;
 S:=S+' vnamei,'#13#10;
 S:=S+' vfile,'#13#10;
 S:=S+' vuio,'#13#10;
 S:=S+' vmount,'#13#10;
 S:=S+' vfcntl,'#13#10;
 S:=S+' vsocketvar;'#13#10;
 S:=S+#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 S:='type'#13#10;
 S:=S+' PPPtrUint     =^PPtrUint;'#13#10;
 S:=S+' pp_bufobj     =Pointer;'#13#10;
 S:=S+' daddr_t       =PtrUint;'#13#10;
 S:=S+' p_daddr_t     =PPtrUint;'#13#10;
 S:=S+' p_buf         =Pointer;'#13#10;
 S:=S+' p_task        =Pointer;'#13#10;
 S:=S+' p_cluster_save=Pointer;'#13#10;
 S:=S+' p_vm_page_t   =Pointer;'#13#10;
 S:=S+' acl_type_t    =Integer;'#13#10;
 S:=S+' p_acl         =Pointer;'#13#10;
 S:=S+' p_label       =Pointer;'#13#10;
 S:=S+#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 maxlen:=0;

 //records
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  S:=' p_'+Enum.name+'_args=^'+Enum.name+'_args;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  S:=' '+Enum.name+'_args=record'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  if (Length(Enum.name)>maxlen) then maxlen:=Length(Enum.name);

  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.Names[x];
   S:='  a_'+Name+Space(Enum.namelen-Length(Name))+':'+Enum.ValueFromIndex[x]+';'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:=' end;'#13#10#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
 end;

 //callbacks
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  S:=' '+Enum.name+'_t'+Space(maxlen-Length(Enum.name))+'=function(ap:p_'+Enum.name+'_args):Integer;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
 end;

 S:=#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 //list
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  S:='//'+Enum.name+Space(maxlen-Length(Enum.name))+':Pointer;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
 end;

 S:=#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 //functions header
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  Name:=FixFuncName(Enum.name);
  Name:=Upcase(Name);

  S:='function '+Name+'(';
  FileWrite(F,Pchar(S)^,Length(S));

  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.Names[x];
   S:=FixFieldName(Name)+':'+Enum.ValueFromIndex[x];
   if (x<>Enum.Count-1) then
   begin
    S:=S+';';
   end;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:='):Integer;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
 end;

 S:=#13#10'implementation'#13#10#13#10;
 S:=S+'uses'#13#10;
 S:=S+' vfs_subr;'#13#10#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 //functions body
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  //header
  Name:=FixFuncName(Enum.name);
  Name:=Upcase(Name);

  S:='function '+Name+'(';
  FileWrite(F,Pchar(S)^,Length(S));

  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.Names[x];
   S:=FixFieldName(Name)+':'+Enum.ValueFromIndex[x];
   if (x<>Enum.Count-1) then
   begin
    S:=S+';';
   end;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:='):Integer;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
  //header

  //var
  S:='var'#13#10;
  S:=S+' v:p_vop_vector;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //var
  if (Enum.Count>1) then
  begin
   S:=' a:'+Enum.name+'_args;'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:=' s:Boolean;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //body
  S:='begin'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  Name:=FixFuncName(Enum.name);
  Name:=Upcase(Name);

  S:=' v:='+FixFieldName(Enum.Names[0])+'^.v_op;'#13#10;
  S:=S+' while (v<>nil) do'#13#10;
  S:=S+' begin'#13#10;
  S:=S+'  if (v^.'+Enum.name+'<>nil) or (v^.vop_bypass<>nil) then Break;'#13#10;
  S:=S+'  v:=v^.vop_default;'#13#10;
  S:=S+' end;'#13#10;
  S:=S+' Assert(v<>nil,'+''''+Name+''''+');'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  if (Enum.Count>1) then
  begin
   //set val
   For x:=0 to Enum.Count-1 do
   begin
    Name:=Enum.Names[x];

    S:=' a.a_'+Name+Space(Enum.namelen-Length(Name))+':='+FixFieldName(Name)+';'#13#10;
    FileWrite(F,Pchar(S)^,Length(S));
   end;
  end;

  if (Enum.Count>1) then
  begin
   Name:='a';
  end else
  begin
   Name:=FixFieldName(Enum.Names[0]);
  end;

  //pre
  if (Enum.pre<>'') then
  begin
   S:=' '+Enum.pre+'(@'+Name+');'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:=' s:=VFS_PROLOGUE('+FixFieldName(Enum.Names[0])+'^.v_mount);'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //call
  S:=' if (v^.'+Enum.name+'<>nil) then'#13#10;
  S:=S+' begin'#13#10;
  S:=S+'  Result:='+Enum.name+'_t(v^.'+Enum.name+')(@'+Name+');'#13#10;
  S:=S+' end else'#13#10;
  S:=S+' begin'#13#10;
  S:=S+'  Result:='+Enum.name+'_t(v^.vop_bypass)(@'+Name+');'#13#10;
  S:=S+' end;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  S:=' VFS_EPILOGUE(s);'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //post
  if (Enum.post<>'') then
  begin
   S:=' '+Enum.post+'(@'+Name+',Result);'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:='end;'#13#10#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
  //body
 end;

 S:='end.'#13#10#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 FileClose(F);
end;

//

begin
 load_if('vnode_if.src');
 readln;
end.

