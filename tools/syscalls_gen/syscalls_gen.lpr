

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
 end;

var
 sysent_maxlen:Integer=0;
 sysent_func:array[0..high(sysent_table)] of TFuncDecl;

procedure AddSysFunc(f:TFuncDecl);
var
 i:Integer;
 s:Boolean;
begin
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

Procedure load_pas(const fname:RawByteString);
var
 F:THandle;
 size:Int64;
 buf:PChar;

 state:TState;

 i:Integer;

 token:RawByteString;

 FuncDecl:TFuncDecl;

 procedure add_to_param(S:RawByteString);
 begin
  S:=Trim(S);
  if (S='') then S:=' ';
  token:=Trim(token)+S;
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
       tkFunction :
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
       tkIdentifier :
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
       tkColon    :
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
       tkColon     :
         begin
          state:=sParamType;
          add_to_param(mwPasLex.Token);
         end;
       tkRoundClose:state:=sAfterParam;
       else
        add_to_param(mwPasLex.Token);
      end;
     end;
   sParamType:
     begin
      //Writeln(mwPasLex.TokenID,':*',mwPasLex.Token,'*');
      Case mwPasLex.TokenID of
       tkSemiColon :
         begin
          FuncDecl.Add(token);
          token:='';
          state:=sParamName;
         end;
       tkRoundClose:
         begin
          FuncDecl.Add(token);
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
       AddSysFunc(FuncDecl);
       FuncDecl:=nil;
       {
       Write(FuncDecl.fhead,' ',FuncDecl.fname,'(');
       if (FuncDecl.Count<>0) then
       For i:=0 to FuncDecl.Count-1 do
       begin
        Write(FuncDecl.Strings[i]);
        if (i<>FuncDecl.Count-1) then
        begin
         Write(';');
        end;
       end;
       Write(')');

       if (FuncDecl.fretv<>'') then
       begin
        Write(':',FuncDecl.fretv);
       end;
       Writeln(';');
       }
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

begin
 mwPasLex:=TmwPasLex.Create;
 Exclude:=TRawStrSet.Create;
 Sysentu:=TRawStrSet.Create;
 FileList:=TStringList.Create;

 AddExclude('backup');

 LoadSysent;
 LoadRecrusive('..\..\sys','');

 LoadStats;

 readln;
end.

