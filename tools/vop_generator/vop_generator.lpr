
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
 releflags:RawByteString;
 vpp_offset:RawByteString;
 rele:ptruint;

 F:THandle;

 Enum:TEnum;

 state:Integer;

 links,maxlen:SizeUInt;
 vpnum:Integer;
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

        rele:=0;
        repeat
         case Name of
          'IN':;
          'OUT':;
          'INOUT':;
          'WILLRELE':rele:=1;
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

        Enum.AddPair(S,Name,TObject(rele));
        Writeln(S,':',Name);
       end;
    end;
  end;

 end;
 L.Free;

 F:=FileCreate(ChangeFileExt(fname,'.pas'));

 S:='{This file is automatically generated by "vop_generator"}'#13#10;
 S:=S+#13#10;
 S:=S+'unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10;
 S:=S+#13#10;
 S:=S+'interface'#13#10;
 S:=S+#13#10;
 S:=S+'{$mode objfpc}{$H+}'#13#10;
 S:=S+'{$CALLING SysV_ABI_CDecl}'#13#10;
 S:=S+#13#10;

 S:=S+'uses'#13#10;
 S:=S+' vnode,'#13#10;
 S:=S+' vnamei,'#13#10;
 S:=S+' vfile,'#13#10;
 S:=S+' vuio,'#13#10;
 S:=S+' vmount,'#13#10;
 S:=S+' vfcntl,'#13#10;
 S:=S+' vsocketvar;'#13#10;
 S:=S+#13#10;

 S:=S+'type'#13#10;
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
  S:=S+' '+Enum.name+'_args=record'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  if (Length(Enum.name)>maxlen) then maxlen:=Length(Enum.name);

  if Length('gen')>Enum.namelen then Enum.namelen:=Length('gen');

  S:='  a_gen'+Space(Enum.namelen-Length('gen'))+':p_vnodeop_desc;'#13#10;

  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.Names[x];
   S:=S+'  a_'+Name+Space(Enum.namelen-Length(Name))+':'+Enum.ValueFromIndex[x]+';'#13#10;
  end;

  S:=S+' end;'#13#10#13#10;
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
 //For i:=0 to EnumList.Count-1 do
 //begin
 // Enum:=TEnum(EnumList.Objects[i]);
 //
 // S:='//'+Enum.name+Space(maxlen-Length(Enum.name))+':Pointer;'#13#10;
 // FileWrite(F,Pchar(S)^,Length(S));
 //end;

 //S:=#13#10;
 //FileWrite(F,Pchar(S)^,Length(S));

 //functions header
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  Name:=FixFuncName(Enum.name);
  Name:=Upcase(Name);

  S:='function '+Name+Space(maxlen-Length(Name))+'(';
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

 //const
 S:=#13#10'const'#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 //vp_offsets
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  S:=' '+Enum.name+'_vp_offsets'+Space(maxlen-Length(Enum.name))+':array[0..';

  //calc count
  vpnum:=0;
  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.ValueFromIndex[x];
   if (Name='p_vnode') then
   begin
    Inc(vpnum);
   end;
  end;
  Assert(vpnum<>0);

  S:=S+IntToStr(vpnum)+'] of Byte=(';

  //offsets
  vpnum:=0;
  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.ValueFromIndex[x];
   if (Name='p_vnode') then
   begin
    if (vpnum<>0) then
    begin
     S:=S+',';
    end;

    Name:=Enum.Names[x];
    Name:='a_'+Name;

    S:=S+'Byte(ptrint(@p_'+Enum.name+'_args(nil)^.'+Name+'))';

    Inc(vpnum);
   end;
  end;

  S:=S+',Byte(-1));'#13#10;

  FileWrite(F,Pchar(S)^,Length(S));
 end;

 S:=#13#10;
 S:=S+' vop_default_desc:t_vnodeop_desc=('#13#10;
 S:=S+'  vdesc_name                :''default'';'#13#10;
 S:=S+'  vdesc_call                :nil;'#13#10;
 S:=S+'  vdesc_vp_offsets          :nil;'#13#10;
 S:=S+'  vdesc_flags               :0;'#13#10;
 S:=S+'  vdesc_vpp_offset          :-1;'#13#10;
 S:=S+' );'#13#10;
 FileWrite(F,Pchar(S)^,Length(S));

 //vnodeop_desc
 For i:=0 to EnumList.Count-1 do
 begin
  Enum:=TEnum(EnumList.Objects[i]);

  //releflags
  vpnum:=0;
  releflags:='';
  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.ValueFromIndex[x];
   if (Name='p_vnode') then
   begin
    if (Enum.Objects[x]<>nil) then //rele
    begin
     if (releflags<>'') then
     begin
      releflags:=releflags+' or ';
     end;
     releflags:=releflags+'VDESC_VP'+IntToStr(vpnum)+'_WILLRELE';
    end;
    Inc(vpnum);
   end;
  end;

  //vppwillrele,vpp_offset
  vpp_offset:='';
  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.ValueFromIndex[x];
   if (Name='pp_vnode') then
   begin
    if (vpp_offset='') then
    begin
     Name:=Enum.Names[x];
     Name:='a_'+Name;

     vpp_offset:='Integer(ptrint(@p_'+Enum.name+'_args(nil)^.'+Name+'))';
    end;

    if (Enum.Objects[x]<>nil) then //rele
    begin
     if (releflags<>'') then
     begin
      releflags:=releflags+' or ';
     end;
     releflags:=releflags+'VDESC_VPP_WILLRELE';
     Break;
    end;
   end;
  end;

  if (releflags='') then  releflags:='0';
  if (vpp_offset='') then vpp_offset:='-1';

  S:=#13#10;
  S:=S+' '+Enum.name+'_desc:t_vnodeop_desc=('#13#10;
  S:=S+'  vdesc_name                :'''+Enum.name+''';'#13#10;
  S:=S+'  vdesc_call                :@p_vop_vector(nil)^.'+Enum.name+';'#13#10;
  S:=S+'  vdesc_vp_offsets          :@'+Enum.name+'_vp_offsets;'#13#10;
  S:=S+'  vdesc_flags               :'+releflags+';'#13#10;
  S:=S+'  vdesc_vpp_offset          :'+vpp_offset+';'#13#10;
  S:=S+' );'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));
 end;

 S:=#13#10;
 S:=S+'implementation'#13#10;
 S:=S+#13#10;
 S:=S+'uses'#13#10;
 S:=S+' errno,'#13#10;
 S:=S+' vfs_subr;'#13#10;
 S:=S+#13#10;

 S:=S+'function get_vp_cb(vp:p_vnode;offset:Pointer):Pointer; inline;'#13#10;
 S:=S+'var'#13#10;
 S:=S+' v:p_vop_vector;'#13#10;
 S:=S+' p:Pointer;'#13#10;
 S:=S+'begin'#13#10;
 S:=S+' Result:=nil;'#13#10;
 S:=S+' if (vp=nil) then Exit;'#13#10;
 S:=S+' v:=vp^.v_op;'#13#10;
 S:=S+' while (v<>nil) do'#13#10;
 S:=S+' begin'#13#10;
 S:=S+'  p:=PPointer(Pointer(v)+ptrint(offset))^;'#13#10;
 S:=S+'  if (p<>nil) then'#13#10;
 S:=S+'  begin'#13#10;
 S:=S+'   Exit(p);'#13#10;
 S:=S+'  end;'#13#10;
 S:=S+'  p:=v^.vop_bypass;'#13#10;
 S:=S+'  if (p<>nil) then'#13#10;
 S:=S+'  begin'#13#10;
 S:=S+'   Exit(p);'#13#10;
 S:=S+'  end;'#13#10;
 S:=S+'  v:=v^.vop_default;'#13#10;
 S:=S+' end;'#13#10;
 S:=S+'end;'#13#10;
 S:=S+#13#10;

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

  if (Upcase(Enum.name)='VOP_WRITE') then
  begin
   S:=S+' osize,ooffset:Int64;'#13#10;
  end;

  S:=S+' c:Pointer;'#13#10;
  S:=S+' a:'+Enum.name+'_args;'#13#10;
  S:=S+' s:Boolean;'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //body
  S:='begin'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  Name:=FixFuncName(Enum.name);
  Name:=Upcase(Name);

  S:=' c:=get_vp_cb('+FixFieldName(Enum.Names[0])+','+Enum.name+'_desc.vdesc_call);'#13#10;
  S:=S+' Assert(c<>nil,'+''''+Name+''''+');'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //set val
  S:=' a.a_gen'+Space(Enum.namelen-Length('gen'))+':=@'+Enum.name+'_desc;'#13#10;

  For x:=0 to Enum.Count-1 do
  begin
   Name:=Enum.Names[x];

   S:=S+' a.a_'+Name+Space(Enum.namelen-Length(Name))+':='+FixFieldName(Name)+';'#13#10;
  end;

  FileWrite(F,Pchar(S)^,Length(S));

  //pre
  if (Enum.pre<>'') then
  begin
   S:=' '+Enum.pre+'(@'+'a';
   if (Upcase(Enum.name)='VOP_WRITE') then
   begin
    S:=S+',osize,ooffset';
   end;
   S:=S+');'#13#10;
   FileWrite(F,Pchar(S)^,Length(S));
  end;

  S:=' s:=VFS_PROLOGUE('+FixFieldName(Enum.Names[0])+'^.v_mount);'#13#10;

  //call
  S:=S+' Result:='+Enum.name+'_t(c)(@a);'#13#10;

  S:=S+' VFS_EPILOGUE(s);'#13#10;
  FileWrite(F,Pchar(S)^,Length(S));

  //post
  if (Enum.post<>'') then
  begin
   S:=' '+Enum.post+'(@a,Result';
   if (Upcase(Enum.name)='VOP_WRITE') then
   begin
    S:=S+',osize,ooffset';
   end;
   S:=S+');'#13#10;
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

