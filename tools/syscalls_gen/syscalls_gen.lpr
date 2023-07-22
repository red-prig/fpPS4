
uses
 Classes,
 SysUtils,
 gset,
 mpaslex,
 _sysent;

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

var
 Exclude:TRawStrSet;
 Sysentu:TRawStrSet;
 FileList:TStringList;

Procedure AddExclude(const s:RawByteString);
begin
 Exclude.Insert(lowercase(Trim(s)));
end;

Function IsExclude(const s:RawByteString):Boolean;
begin
 Result:=(Exclude.NFind(lowercase(Trim(s)))<>nil);
end;

type
 TState=(sNone,sNameFunc,sBeforeParam,sParamName,sParamType,sAfterParam,sReturn,sEndFunc,sError);

type
 TFuncDecl=class(TStringList)
  public
   funit:RawByteString;
   fhead:RawByteString;
   fname:RawByteString;
   fretv:RawByteString;
   fprint_interface:Boolean;
   fprint_implementation:Boolean;
 end;

var
 sysent_maxlen:Integer=0;
 sysent_func:array[0..high(sysent_table)] of TFuncDecl;

procedure AddSysFunc(f:TFuncDecl);
var
 i:Integer;
 s:Boolean;
begin

 if (lowercase(f.fname)='sys_mmap') then
 begin
  f.fretv:='Pointer';
 end;

 s:=False;
 For i:=0 to high(sysent_table) do
 begin
  if (lowercase(sysent_table[i].sy_name)=lowercase(f.fname)) then
  begin
   if (sysent_func[i]<>nil) then
   begin
    Writeln('Collision[',i:3,']:',f.fname,' prev unit:',sysent_func[i].funit,' new unit:',f.funit);
   end else
   begin
    Writeln('Found[',i:3,']:',f.fname:sysent_maxlen,' in unit:',f.funit);
    sysent_func[i]:=f;
    s:=True;
   end;
  end;
 end;
 if not s then
 begin
  FreeAndNil(f);
 end;
end;

procedure print_decl(FuncDecl:TFuncDecl);
var
 i:Integer;
begin
 Write(FuncDecl.fhead,' ',FuncDecl.fname,'(');
 if (FuncDecl.Count<>0) then
 For i:=0 to FuncDecl.Count-1 do
 begin
  Write(FuncDecl.Strings[i]);
 end;
 Write(')');

 if (FuncDecl.fretv<>'') then
 begin
  Write(':',FuncDecl.fretv);
 end;
 Writeln(';');
end;

function Max(a, b: Integer): Integer;inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function TrimType(const S: RawByteString): RawByteString;
var
  Ofs, Len: sizeint;
begin
  len := Length(S);
  while (Len>0) and ((S[Len]<=' ') or (S[Len]=',') or (S[Len]=';')) do
   dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and ((S[Ofs]<=' ') or (S[Ofs]=',') or (S[Ofs]<=';')) do
    Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end;

function get_str_no_sys(const S:RawByteString):RawByteString;
begin
 Result:=S;
 if (lowercase(copy(Result,1,4))='sys_') then
 begin
  Delete(Result,1,4);
 end;

 case lowercase(Result) of
  'accept',
  'close',
  'connect',
  'exit',
  'fcntl',
  'fpathconf',
  'fstatfs',
  'getsockopt',
  'nanosleep',
  'open',
  'openat',
  'read',
  'readv',
  'recvfrom',
  'recvmsg',
  'sendmsg',
  'sendto',
  'sigsuspend',
  'wait4',
  'write',
  'writev':Result:='_'+Result;
  else;
 end;
end;

function get_str_decl(FuncDecl:TFuncDecl;align:Boolean):RawByteString;
var
 i,p:Integer;
 n,t:RawByteString;
begin
 if align then
 begin
  i:=Max(0,Length('procedure ')-Length(FuncDecl.fhead));
 end else
 begin
  i:=1;
 end;

 Result:=FuncDecl.fhead+space(i)+get_str_no_sys(FuncDecl.fname)+'(';

 if (FuncDecl.Count<>0) then
 For i:=0 to FuncDecl.Count-1 do
 begin
  n:=FuncDecl.Strings[i];
  t:='';

  p:=Pos(':',n);

  if (p<>0) then
  begin
   t:=copy(n,p+1);
   t:=TrimType(t);
   t:=lowercase(t);
  end;

  if (Copy(t,1,1)='p') then
  begin
   case t of
    'pointer':;
    'pchar':;
    'pdword':;
    'pqword':;
    'pinteger':;
    'pbyte':;
    'pint64':;
    'ppointer':;
    'ppchar':;
    else
      Writeln('*',t,'*',' in ',FuncDecl.fname,' (',FuncDecl.funit,')');
   end;
  end;

  Result:=Result+n;
 end;
 Result:=Result+')';

 if (FuncDecl.fretv<>'') then
 begin
  Result:=Result+':'+FuncDecl.fretv;
 end;
 Result:=Result+';';
end;

Procedure load_pas(const fname:RawByteString);
var
 F:THandle;
 size:Int64;
 buf:PChar;

 state:TState;

 token:RawByteString;

 FuncDecl:TFuncDecl;

 procedure add_to_param(S:RawByteString);
 begin
  S:=Trim(S);
  if (S='') then S:=' ';
  token:=Trim(token)+S;
 end;

 procedure add_param(S:RawByteString);
 begin
  S:=Trim(S);
  if (S<>'') then FuncDecl.Add(S);
 end;

begin
 if (fname='') then Exit;

 F:=FileOpen(fname,fmOpenRead or fmShareDenyNone);
 if (F=feInvalidHandle) then
 begin
  Writeln('Error open file:',fname);
  Exit;
 end;

 size:=FileSeek(F,0,fsFromEnd);
 FileSeek(F,0,fsFromBeginning);

 buf:=AllocMem(size+1);

 FileRead(F,buf^,size);

 mwPasLex.Origin:=buf;

 FuncDecl:=nil;
 state:=sNone;

 while (mwPasLex.RunPos<size) do
 begin
  mwPasLex.Next;
  Case mwPasLex.TokenID of
   tkInterface:Break;
   else;
  end;
 end;

 while (mwPasLex.RunPos<size) do
 begin

  case state of
   sParamName,
   sParamType,
   sReturn   :mwPasLex.Next;
   else
              mwPasLex.NextNoJunk;
  end;

  Case mwPasLex.TokenID of
   tkImplementation:Break;
   else;
  end;

  case state of
   sNone:
     begin
      Case mwPasLex.TokenID of
       tkProcedure,
       tkFunction:
       begin
        FuncDecl:=TFuncDecl.Create;
        FuncDecl.NameValueSeparator:=':';

        FuncDecl.funit:=ChangeFileExt(ExtractFileName(fname),'');
        FuncDecl.fhead:=mwPasLex.Token;

        state:=sNameFunc;
       end;
       else;
      end;
     end;
   sNameFunc:
     begin
      //Writeln(mwPasLex.TokenID);
      Case mwPasLex.TokenID of
       tkIdentifier:
         begin
          token:=mwPasLex.Token;
          if (LowerCase(Copy(token,1,4))='sys_') then
          begin
           FuncDecl.fname:=token;
           token:='';
           state:=sBeforeParam;
          end else
          begin
           state:=sError;
          end;
         end
       else
         state:=sError;
        end;
      end;
   sBeforeParam:
     begin
      Case mwPasLex.TokenID of
       tkRoundOpen:state:=sParamName;
       tkColon:
         begin
          token:='';
          state:=sReturn;
         end;
       tkSemiColon:state:=sEndFunc;
       else;
        state:=sError;
      end;
     end;
   sParamName:
     begin
      //Writeln(mwPasLex.TokenID,':*',mwPasLex.Token,'*');
      Case mwPasLex.TokenID of
       tkComma:
         begin
          add_to_param(mwPasLex.Token);
          add_param(token);
          token:='';
         end;
       tkColon:
         begin
          add_to_param(mwPasLex.Token);
          state:=sParamType;
         end;
       tkRoundClose:
         begin
          add_param(token);
          token:='';
          state:=sAfterParam;
         end;
       else
        add_to_param(mwPasLex.Token);
      end;
     end;
   sParamType:
     begin
      //Writeln(mwPasLex.TokenID,':*',mwPasLex.Token,'*');
      Case mwPasLex.TokenID of
       tkSemiColon:
         begin
          add_to_param(mwPasLex.Token);
          add_param(token);
          token:='';
          state:=sParamName;
         end;
       tkRoundClose:
         begin
          add_param(token);
          token:='';
          state:=sAfterParam;
         end;
       else
        add_to_param(mwPasLex.Token);
      end;
     end;
   sAfterParam:
     begin
      Case mwPasLex.TokenID of
       tkColon:
         begin
          token:='';
          state:=sReturn;
         end;
       tkSemiColon:state:=sEndFunc;
       else;
        state:=sError;
      end;
     end;
   sReturn:
     begin
      Case mwPasLex.TokenID of
       tkSemiColon:
         begin
          FuncDecl.fretv:=token;
          token:='';
          state:=sEndFunc;
         end;
       else
        add_to_param(mwPasLex.Token);
      end;
     end;
   else;
  end;

  case state of
   sError:
     begin
      state:=sNone;
      FreeAndNil(FuncDecl);
     end;
   sEndFunc:
     begin
      state:=sNone;

      if (Sysentu.NFind(lowercase(FuncDecl.fname))<>nil) then
      begin

       //print_decl(FuncDecl);

       AddSysFunc(FuncDecl);
       FuncDecl:=nil;
      end;

     end;
   else;
  end;

 end;

 FreeMem(buf);
 FileClose(F);
end;

procedure LoadRecrusive(const basep,reltv:RawByteString);
Var
 Info:TSearchRec;
 f:RawByteString;
begin
 f:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(basep)+reltv);
 If FindFirst(f+'*',faDirectory,Info)=0 then
 begin
  Repeat
   Case Info.Name of
    '.','..':;
    else
     if not IsExclude(Info.Name) then
     begin
      If ((Info.Attr and faDirectory)<>0) then
      begin
       LoadRecrusive(basep,IncludeTrailingPathDelimiter(reltv)+Info.Name);
      end else
      if (lowercase(ExtractFileExt(Info.Name))='.pas') then
      begin
       FileList.Add(IncludeTrailingPathDelimiter(reltv)+Info.Name);
       //Writeln(IncludeTrailingPathDelimiter(reltv)+Info.Name);
       load_pas(basep+IncludeTrailingPathDelimiter(reltv)+Info.Name);
      end;
     end;
   end;
  Until FindNext(info)<>0;
  FindClose(Info);
 end;
end;

procedure LoadSysent;
var
 i:Integer;
begin
 For i:=0 to high(sysent_table) do
 begin
  if (sysent_table[i].sy_call=nil) then
  begin
   if Length(sysent_table[i].sy_name)>sysent_maxlen then sysent_maxlen:=Length(sysent_table[i].sy_name);
   Sysentu.Insert(lowercase(sysent_table[i].sy_name));
  end;
 end;
end;

procedure LoadStats;
var
 i:Integer;
 f,n:Integer;
begin
 f:=0;
 n:=0;
 For i:=0 to high(sysent_table) do
 begin
  if (sysent_table[i].sy_call=nil) then
  begin
   if (sysent_func[i]<>nil) then
   begin
    if (sysent_table[i].sy_narg<>sysent_func[i].Count) then
    begin
     Writeln('Wrong narg[',i:3,']:',sysent_table[i].sy_narg,'<>',sysent_func[i].Count,':');
     Write(' ');
     print_decl(sysent_func[i]);
    end;
    Inc(f);
   end else
   begin
    Writeln('Not found[',i:3,']:',sysent_table[i].sy_name:sysent_maxlen);
    Inc(n);
   end;
  end;
 end;
 Writeln('syscalls status:',f,'/',f+n,':%',(f/(f+n))*100:0:2);
end;

procedure Save_sysent(const fname:RawByteString);
var
 F:THandle;
 S:RawByteString;
 i:Integer;
 D:TFuncDecl;
 FUnits:TRawStrSet;
 iter:TRawStrSet.TIterator;
 inext:Boolean;
begin
 if (fname='') then Exit;

 F:=FileCreate(fname);
 if (F=feInvalidHandle) then
 begin
  Writeln('Error create file:',fname);
  Exit;
 end;

 FUnits:=TRawStrSet.Create;
 For i:=0 to high(sysent_table) do
 begin
  D:=sysent_func[i];
  if (D<>nil) then
  begin
   FUnits.Insert(D.funit);
  end;
 end;

 S:='{This file is automatically generated by "syscalls_gen"}'#13#10;
 S:=S+#13#10;
 S:=S+'unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10;
 S:=S+#13#10;
 S:=S+'{$mode objfpc}{$H+}'#13#10;
 S:=S+'{$CALLING SysV_ABI_CDecl}'#13#10;
 S:=S+#13#10;
 S:=S+'interface'#13#10;
 S:=S+#13#10;
 S:=S+'uses'#13#10;

 FileWrite(F,Pchar(S)^,Length(S));

 iter:=FUnits.Min;
 if (iter<>nil) then
 begin
  repeat
   S:=' '+iter.GetData;

   inext:=iter.Next;
   if inext then
   begin
    S:=S+',';
   end else
   begin
    S:=S+';';
   end;

   S:=S+#13#10;

   FileWrite(F,Pchar(S)^,Length(S));
  until not inext;
  FreeAndNil(iter);
 end;

 S:=#13#10;
 S:=S+'function nosys:Integer;'#13#10;
 S:=S+'function nosup:Integer;'#13#10;
 S:=S+'function nzero:Integer;'#13#10;
 S:=S+#13#10;
 S:=S+'type'#13#10;
 S:=S+' t_sysent=packed record'#13#10;
 S:=S+'  sy_narg:ptruint;'#13#10;
 S:=S+'  sy_call:Pointer;'#13#10;
 S:=S+'  sy_name:PChar;'#13#10;
 S:=S+' end;'#13#10;
 S:=S+#13#10;
 S:=S+'const'#13#10;
 S:=S+' sysent_table:array[0..'+IntToStr(high(sysent_table))+'] of t_sysent=('#13#10;

 FileWrite(F,Pchar(S)^,Length(S));

 For i:=0 to high(sysent_table) do
 begin

  S:=  '  (//['+IntToStr(i)+']'#13#10;
  S:=S+'     sy_narg:'+IntToStr(sysent_table[i].sy_narg)+';'#13#10;
  S:=S+'     sy_call:';

  if (sysent_table[i].sy_call=Pointer(@nosys)) then
  begin
   S:=S+'@nosys';
  end else
  if (sysent_table[i].sy_call=Pointer(@nosup)) then
  begin
   S:=S+'@nosup';
  end else
  if (sysent_table[i].sy_call=Pointer(@nzero)) then
  begin
   S:=S+'@nzero';
  end else
  begin
   //
   D:=sysent_func[i];
   if (D=nil) then
   begin
    S:=S+'nil';
   end else
   begin
    S:=S+'@'+D.fname;
   end;
  end;

  S:=S+';'#13#10;

  S:=S+'     sy_name:'''+sysent_table[i].sy_name+''''#13#10;
  S:=S+'  )';

  if (i<>high(sysent_table)) then
  begin
   S:=S+',';
  end;

  S:=S+#13#10;

  FileWrite(F,Pchar(S)^,Length(S));

 end;

 S:=  ' );'#13#10;
 S:=S+#13#10;
 S:=S+'implementation'#13#10;
 S:=S+#13#10;
 S:=S+'const'#13#10;
 S:=S+' ENOSYS =78;'#13#10;
 S:=S+' ENOTSUP=45;'#13#10;
 S:=S+#13#10;
 S:=S+'function nosys:Integer;'#13#10;
 S:=S+'begin'#13#10;
 S:=S+' Result:=ENOSYS;'#13#10;
 S:=S+'end;'#13#10;
 S:=S+#13#10;
 S:=S+'function nosup:Integer;'#13#10;
 S:=S+'begin'#13#10;
 S:=S+' Result:=ENOTSUP;'#13#10;
 S:=S+'end;'#13#10;
 S:=S+#13#10;
 S:=S+'function nzero:Integer;'#13#10;
 S:=S+'begin'#13#10;
 S:=S+' Result:=0;'#13#10;
 S:=S+'end;'#13#10;
 S:=S+#13#10;
 S:=S+'end.'#13#10;
 S:=S+#13#10;

 FileWrite(F,Pchar(S)^,Length(S));

 FileClose(F);

 FUnits.Free;
end;

procedure Save_syscalls(const fname:RawByteString);
var
 F:THandle;
 S:RawByteString;
 i:Integer;
 D:TFuncDecl;
begin
 if (fname='') then Exit;

 F:=FileCreate(fname);
 if (F=feInvalidHandle) then
 begin
  Writeln('Error create file:',fname);
  Exit;
 end;

 S:='{This file is automatically generated by "syscalls_gen"}'#13#10;
 S:=S+#13#10;
 S:=S+'unit '+ChangeFileExt(ExtractFileName(fname),'')+';'#13#10;
 S:=S+#13#10;
 S:=S+'{$mode objfpc}{$H+}'#13#10;
 S:=S+'{$CALLING SysV_ABI_CDecl}'#13#10;
 S:=S+#13#10;
 S:=S+'interface'#13#10;
 S:=S+#13#10;

 FileWrite(F,Pchar(S)^,Length(S));

 For i:=0 to high(sysent_table) do
 begin
  D:=sysent_func[i];
  if (D<>nil) then
  if (not D.fprint_interface) then
  begin
   S:=get_str_decl(D,True)+#13#10;

   FileWrite(F,Pchar(S)^,Length(S));

   D.fprint_interface:=True;
  end;
 end;

 S:=#13#10;
 S:=S+'implementation'#13#10;
 S:=S+#13#10;
 S:=S+'uses'#13#10;
 S:=S+' trap,'#13#10;
 S:=S+' thr_error;'#13#10;
 S:=S+#13#10;

 FileWrite(F,Pchar(S)^,Length(S));

 For i:=0 to high(sysent_table) do
 begin
  D:=sysent_func[i];
  if (D<>nil) then
  if (not D.fprint_implementation) then
  begin
   S:=get_str_decl(D,False);

   S:=S+' assembler; nostackframe;'#13#10;
   S:=S+'asm'#13#10;
   S:=S+' movq  $'+IntToStr(i)+',%rax'#13#10;
   S:=S+' call  fast_syscall'#13#10;
   S:=S+' jmp   cerror'#13#10;
   S:=S+'end;'#13#10;
   S:=S+#13#10;

   FileWrite(F,Pchar(S)^,Length(S));

   D.fprint_implementation:=True;
  end;
 end;

 S:=#13#10;
 S:=S+'end.'#13#10;
 S:=S+#13#10;

 FileWrite(F,Pchar(S)^,Length(S));

 FileClose(F);
end;

begin
 mwPasLex:=TmwPasLex.Create;
 Exclude:=TRawStrSet.Create;
 Sysentu:=TRawStrSet.Create;
 FileList:=TStringList.Create;

 AddExclude('backup');

 LoadSysent;

 LoadRecrusive('..\..\sys','');

 LoadStats;

 Save_sysent('sysent.pas');
 Save_syscalls('syscalls.pas');

 readln;
end.

