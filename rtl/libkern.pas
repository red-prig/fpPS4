unit libkern;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 strings;

function strtoq(const nptr:PChar;endptr:PPChar;base:Integer):Int64;
function strtol(nptr:PChar;endptr:PPChar;base:Integer):Int64;
function strcasecmp(str1,str2:PChar):Integer;
function strncasecmp(str1,str2:PChar;maxlen:ptrint):Integer;
function strcmp(s1,s2:PChar):Integer;
function strncmp(str1,str2:PChar;maxlen:ptrint):Integer;
function strnlen(s:PChar;maxlen:ptrint):ptrint;
function strnlen_s(s:PChar;maxlen:ptrint):ptrint;
function strsep(stringp:PPChar;delim:PChar):PChar;
function strlcpy(dst,src:PChar;size:ptrint):ptrint;
function strncpy_s(dst,src:PChar;maxlen:ptrint):PChar;

implementation

function strtoq(const nptr:PChar;endptr:PPChar;base:Integer):Int64;
var
 p:PChar;
 neg:Boolean;
 val:QWord;
 overflow:Boolean;
 digit:Integer;
 any:Boolean;
 c:Char;
begin
 //strtoq_errno := STRTOQ_OK;
 overflow:=False;
 any:=False;
 val:=0;

 p:=nptr;
 while (p^<>#0) and (p^ in [' ',#9,#10,#13,#11,#12]) do
  Inc(p);

 neg:=False;
 if (p^='-') then
 begin
  neg:=True;
  Inc(p);
 end else
 if (p^='+') then
  Inc(p);

 if (base=0) then
 begin
  if p^='0' then
  begin
   Inc(p);
   if (p^='x') or (p^='X') then
   begin
    Inc(p);
    base:=16;
   end else
    base:=8;
  end else
   base:=10;
 end;

 if (base<2) or (base>36) then
 begin
  //strtoq_errno := STRTOQ_EINVAL;
  if (endptr<>nil) then
   endptr^:=nptr;
  Exit(0);
 end;

 while True do
 begin
  c:=p^;
  if (c>='0') and (c<='9') then
   digit:=Ord(c)-Ord('0')
  else if (c>='A') and (c<='Z') then
   digit:=Ord(c)-Ord('A')+10
  else if (c>='a') and (c<='z') then
   digit:=Ord(c)-Ord('a')+10
  else
   Break;

  if (digit>=base) then
   Break;

  if (val>(High(QWord) div QWord(base))) or
     ((val=(High(QWord) div QWord(base))) and (QWord(digit)>(High(QWord) mod QWord(base)))) then
  begin
   overflow:=True;
  end else
   val:=val*QWord(base)+QWord(digit);

  any:=True;
  Inc(p);
 end;

 if not any then
 begin
  if (endptr<>nil) then
   endptr^:=nptr;
  Exit(0);
 end;

 if (endptr<>nil) then
  endptr^:=p;

 if overflow then
 begin
  //strtoq_errno := STRTOQ_ERANGE;
  if neg then
   Result:=Low(Int64)
  else
   Result:=High(Int64);

  Exit;
 end;

 if neg then
 begin
  if val>(QWord(High(Int64))+1) then
  begin
   //strtoq_errno := STRTOQ_ERANGE;
   Result:=Low(Int64);
  end else
  if val=(QWord(High(Int64))+1) then
   Result:=Low(Int64)
  else
   Result:=-Int64(val);
 end else
 begin
  if val>High(Int64) then
  begin
   //strtoq_errno := STRTOQ_ERANGE;
   Result:=High(Int64);
  end else
   Result:=Int64(val);
 end;
end;

function strtol(nptr:PChar;endptr:PPChar;base:Integer):Int64;
var
 s:PChar;
 acc:QWord;
 c:Char;
 cutoff:QWord;
 neg:Integer;
 any:Integer;
 cutlim:QWord;
 digit:Integer;
begin
 s:=nptr;
 neg:=0;

 repeat
  c:=s^;
  Inc(s);
 until not ((c=' ') or (c=#9) or (c=#10) or (c=#13) or (c=#11) or (c=#12));

 if (c='-') then
 begin
  neg:=1;
  c:=s^;
  Inc(s);
 end else
 if (c='+') then
 begin
  c:=s^;
  Inc(s);
 end;

 if ((base=0) or (base=16)) and
    (c='0') and
    ((s^='x') or (s^='X')) then
 begin
  c:=s[1];
  s:=s+2;
  base:=16;
 end;

 if (base=0) then
 begin
  if (c='0') then
   base:=8
  else
   base:=10;
 end;

 if (neg<>0) then
  cutoff:=QWord(Low(Int64))
 else
  cutoff:=High(Int64);

 cutlim:=cutoff mod QWord(base);
 cutoff:=cutoff div QWord(base);

 acc:=0;
 any:=0;

 while True do
 begin
  if (Ord(c)>=128) then Break;

  if (c>='0') and (c<='9') then
  begin
   digit:=Ord(c)-Ord('0');
  end else
  if ((c>='a') and (c<='z')) or ((c>='A') and (c<='Z')) then
  begin
   if (c>='A') and (c<='Z') then
    digit:=Ord(c)-(Ord('A')-10)
   else
    digit:=Ord(c)-(Ord('a')-10);
  end else
  begin
   Break;
  end;

  if (digit>=base) then Break;

  if (any<0) or (acc>cutoff) or ((acc=cutoff) and (QWord(digit)>cutlim)) then
  begin
   any:=-1
  end else
  begin
   any:=1;
   acc:=acc*QWord(base)+QWord(digit);
  end;

  c:=s^;
  Inc(s);
 end;

 if (any<0) then
 begin
  if (neg<>0) then
   acc:=QWord(Low(Int64))
  else
   acc:=High(Int64);
 end else
 if (neg<>0) then
 begin
  acc:=-acc;
 end;

 if (endptr<>nil) then
 begin
  if (any<>0) then
   endptr^:=s-1
  else
   endptr^:=nptr;
 end;

 Result:=acc;
end;

function strcasecmp(str1,str2:PChar):Integer;
begin
 repeat
  if (LowerCase(str1^)<>LowerCase(str2^)) then
  begin
   Exit(ord(LowerCase(str1^))-ord(LowerCase(str2^)));
  end;

  if (str1^=#0) then break;

  Inc(str1);
  Inc(str2);
 until False;

 Result:=0;
end;

function strncasecmp(str1,str2:PChar;maxlen:ptrint):Integer;
begin
 if (maxlen<>0) then
 begin
  repeat
   if (LowerCase(str1^)<>LowerCase(str2^)) then
   begin
    Exit(ord(LowerCase(str1^))-ord(LowerCase(str2^)));
   end;

   if (str1^=#0) then break;

   Inc(str1);
   Inc(str2);

   Dec(maxlen);
  until (maxlen=0);

 end;
 Result:=0;
end;

function strcmp(s1,s2:PChar):Integer;
begin
 while (s1^=s2^) do
 begin
  if (s1^=#0) then Exit(0);
  Inc(s1);
  Inc(s2);
 end;
 Result:=Ord(s1^)-Ord(s2^);
end;

function strncmp(str1,str2:PChar;maxlen:ptrint):Integer;
begin
 Result:=CompareChar0(str1^,str2^,maxlen);
end;

function strnlen(s:PChar;maxlen:ptrint):ptrint;
var
 i:size_t;
begin
 i:=0;
 if (maxlen<>0) then
 begin
  repeat
   if (s[i]=#0) then Exit(i);
   Inc(i);
  until (maxlen=i);
 end;
 Exit(maxlen);
end;

function strnlen_s(s:PChar;maxlen:ptrint):ptrint;
var
 i:size_t;
begin
 if (s=nil) then Exit(0);
 i:=0;
 if (maxlen<>0) then
 begin
  repeat
   if (s[i]=#0) then Exit(i);
   Inc(i);
  until (maxlen=i);
 end;
 Exit(maxlen);
end;

function strsep(stringp:PPChar;delim:PChar):PChar;
var
 b,e:PChar;
begin
 b:=stringp^;
 if (b=nil) then Exit(nil);

 e:=strpos(b,delim)+strlen(delim);

 if (e^<>#0) then
 begin
  e^:=#0;
  Inc(e);
  stringp^:=e;
 end else
 begin
  stringp^:=nil;
 end;

 Result:=b;
end;

function strlcpy(dst,src:PChar;size:ptrint):ptrint;
begin
 strlcopy(dst,src,size);
 Result:=strlen(dst);
end;

function strncpy_s(dst,src:PChar;maxlen:ptrint):PChar;
begin
 if (dst=nil) or (src=nil) then Exit(nil);
 Result:=StrLCopy(dst,src,maxlen);
end;

end.
