unit sys_fnmatch;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 FNM_NOMATCH =1;    { Match failed. }

 FNM_NOESCAPE=$01; { Disable backslash escaping. }
 FNM_PATHNAME=$02; { Slash must be matched by slash. }
 FNM_PERIOD  =$04; { Period must be matched by period. }

 FNM_NOSYS   =(-1); { Reserved. }

 FNM_LEADING_DIR=$08; { Ignore /<tail> after Imatch. }
 FNM_CASEFOLD   =$10; { Case insensitive search. }
 FNM_IGNORECASE =FNM_CASEFOLD;
 FNM_FILE_NAME  =FNM_PATHNAME;

 EOS=#0;

 RANGE_MATCH  =1;
 RANGE_NOMATCH=0;
 RANGE_ERROR  =(-1);

function rangematch(pattern:pchar;test:char;flags:Integer;newp:ppchar):Integer;
function fnmatch(pattern,str:pchar;flags:Integer):Integer;

implementation

uses
 sysutils;

{
 * Function fnmatch() as specified in POSIX 1003.2-1992, section B.6.
 * Compares a filename or pathname to a pattern.
 }
function fnmatch(pattern,str:pchar;flags:Integer):Integer;
label
 norm;
var
 stringstart:pchar;
 newp:pchar;
 c,test:char;
begin
 stringstart:=str;
 while true do
 begin
  c:=pattern^;
  Inc(pattern);
  case (c) of
   EOS:
    begin
     if ((flags and FNM_LEADING_DIR)<>0) and (str^='/') then
      Exit(0);
     if (str^=EOS) then
      Exit(0)
     else
      Exit(FNM_NOMATCH);
    end;
   '?':
    begin
     if (str^=EOS) then
      Exit(FNM_NOMATCH);
     if (str^='/') and ((flags and FNM_PATHNAME)<>0) then
      Exit(FNM_NOMATCH);
     if (str^='.') and
        ((flags and FNM_PERIOD)<>0) and
        ((str=stringstart) or
         (((flags and FNM_PATHNAME)<>0) and
          ((str - 1)^='/'))) then
      Exit(FNM_NOMATCH);
     Inc(str);
    end;
   '*':
    begin
     c:=pattern^;
     { Collapse multiple stars. }
     while (c='*') do
     begin
      Inc(pattern);
      c:=pattern^;
     end;

     if (str^='.') and
        ((flags and FNM_PERIOD)<>0) and
        ((str=stringstart) or
         (((flags and FNM_PATHNAME)<>0) and
          ((str - 1)^='/'))) then
      Exit(FNM_NOMATCH);

     { Optimize for pattern with * at end or before /. }
     if (c=EOS) then
     begin
      if ((flags and FNM_PATHNAME)<>0) then
      begin
       if ((flags and FNM_LEADING_DIR)<>0) or (strscan(str, '/')=nil) then
        Exit(0)
       else
        Exit(FNM_NOMATCH);
      end else
       Exit(0);
     end else
     if (c='/') and ((flags and FNM_PATHNAME)<>0) then
     begin
      str:=strscan(str, '/');
      if (str=nil) then
       Exit(FNM_NOMATCH);
      continue;
     end;

     { General case, use recursion. }
     test:=str^;
     while (test<>EOS) do
     begin
      if (fnmatch(pattern, str, flags and (not FNM_PERIOD))=0) then
       Exit(0);
      if (test='/') and ((flags and FNM_PATHNAME)<>0) then
       break;
      Inc(str);
      test:=str^;
     end;
     Exit(FNM_NOMATCH);
    end;
   '[':
    begin
     if (str^=EOS) then
      Exit(FNM_NOMATCH);
     if (str^='/') and ((flags and FNM_PATHNAME)<>0) then
      Exit(FNM_NOMATCH);
     if (str^='.') and
        ((flags and FNM_PERIOD)<>0) and
        ((str=stringstart) or
         (((flags and FNM_PATHNAME)<>0) and
          ((str - 1)^='/'))) then
      Exit(FNM_NOMATCH);

     case (rangematch(pattern, str^, flags, @newp)) of
      RANGE_ERROR:
       goto norm;
      RANGE_MATCH:
       pattern:=newp;
      RANGE_NOMATCH:
       Exit(FNM_NOMATCH);
     end;
     Inc(str);
    end;
   '\':
    begin
     if ((flags and FNM_NOESCAPE)=0) then
     begin
      c:=pattern^;
      Inc(pattern);
      if (c=EOS) then
      begin
       c:='\';
       Dec(pattern);
      end;
     end;
     goto norm;
    end;
    { FALLTHROUGH }
   else
    begin
     norm:
     if (c=str^) then
     begin
      //
     end else
     if ((flags and FNM_CASEFOLD)<>0) and
        (lowercase(c)=lowercase(str^)) then
     begin
      //
     end else
      Exit(FNM_NOMATCH);
     Inc(str);
    end;
  end;
 end;
 { NOTREACHED }
end;

function rangematch(pattern:pchar;test:char;flags:Integer;newp:ppchar):Integer;
var
 negate,ok:Integer;
 c,c2:char;

 function _cond:Boolean; inline;
 begin
  c2:=(pattern+1)^;
  Result:=(c2<>EOS) and (c2<>']');
 end;

begin
 {
  * A bracket expression starting with an unquoted circumflex
  * character produces unspecified results (IEEE 1003.2-1992,
  * 3.13.2).  This implementation treats it like '!', for
  * consistency with the regular expression syntax.
  * J.T. Conklin (conklin@ngai.kaleida.com)
  }
 negate:=ord((pattern^='!') or (pattern^='^'));
 if (negate<>0) then
  Inc(pattern);

 if ((flags and FNM_CASEFOLD)<>0) then
  test:=lowercase(test);

 {
  * A right bracket shall lose its special meaning and represent
  * itself in a bracket expression if it occurs first in the list.
  * -- POSIX.2 2.8.3.2
  }
 ok:=0;
 c:=pattern^;
 Inc(pattern);
 repeat
  if (c='\') and ((flags and FNM_NOESCAPE)=0) then
  begin
   c:=pattern^;
   Inc(pattern);
  end;
  if (c=EOS) then
   Exit(RANGE_ERROR);

  if (c='/') and ((flags and FNM_PATHNAME)<>0) then
   Exit(RANGE_NOMATCH);

  if ((flags and FNM_CASEFOLD)<>0) then
   c:=lowercase(c);

  if (pattern^='-') and _cond then
  begin
   Inc(pattern,2);
   if (c2='\') and ((flags and FNM_NOESCAPE)=0) then
   begin
    c2:=pattern^;
    Inc(pattern);
   end;
   if (c2=EOS) then
    Exit(RANGE_ERROR);

   if ((flags and FNM_CASEFOLD)<>0) then
    c2:=lowercase(c2);

   if (c <= test) and (test <= c2) then
    ok:=1;
  end else
  if (c=test) then
   ok:=1;
  c:=pattern^;
  Inc(pattern);
 until (c=']');

 newp^:=pattern;

 if (ok=negate) then
  Exit(RANGE_NOMATCH)
 else
  Exit(RANGE_MATCH);
end;


end.

