unit sys_path;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 SysUtils,
 RWLock,
 ps4_program,
 sys_kernel;

Function  FetchSaveMount(path,point:PChar;mode:Integer):Integer;
Function  UnMountSavePath(path:PChar):Integer;

function  parse_filename(filename:PChar;var r:RawByteString):Integer;

implementation

const
 SCE_SAVE_DATA_ERROR_PARAMETER  =-2137063424; // 0x809F0000
 SCE_SAVE_DATA_ERROR_EXISTS     =-2137063417; // 0x809F0007
 SCE_SAVE_DATA_ERROR_NOT_FOUND  =-2137063416; // 0x809f0008
 SCE_SAVE_DATA_ERROR_MOUNT_FULL =-2137063412; // 0x809F000C
 SCE_SAVE_DATA_ERROR_NOT_MOUNTED=-2137063420; // 0x809F0004
 SCE_SAVE_DATA_ERROR_INTERNAL   =-2137063413; // 0x809f000b
 SCE_SAVE_DATA_ERROR_BUSY       =-2137063421; // 0x809f0003

 SCE_SAVE_DATA_MOUNT_MODE_RDONLY      =1;  //Read-only
 SCE_SAVE_DATA_MOUNT_MODE_RDWR        =2;  //Read/write-enabled
 SCE_SAVE_DATA_MOUNT_MODE_CREATE      =4;  //Create new (error if save data directory already exists)
 SCE_SAVE_DATA_MOUNT_MODE_DESTRUCT_OFF=8;  //Turn off corrupt flag (not recommended)
 SCE_SAVE_DATA_MOUNT_MODE_COPY_ICON   =16; //Copy save_data.png in package as icon when newly creating save data
 SCE_SAVE_DATA_MOUNT_MODE_CREATE2     =32; //Create new (mount save data directory if it already exists)

 DIRNAME_MAXSIZE=32;
 MOUNT_MAXSIZE  =16;

type
 TMountDir=array[0..DIRNAME_MAXSIZE-1] of AnsiChar;

var
 FSaveMounts_lock:TRWLock;
 FSaveMounts:array[0..15] of TMountDir;

Procedure DoFixRelative(var Path:RawByteString);
Var
 i,L,CF,PF:SizeInt;

  Procedure _c; inline;
  begin
   Case (i-CF) of
    2:if (PWORD(@Path[CF])^=$2E2E) then //..
      begin
       i:=i-PF+1;
       L:=L-i;
       Delete(Path,PF,i);
       CF:=PF;
       i:=PF-1;
      end;
    1:if (Path[CF]='.') then //.
      begin
       Delete(Path,1,CF);
       L:=Length(Path);
       CF:=1;
       i:=1;
      end;
   end;
   PF:=CF;
   CF:=i+1;
  end;

begin
 PF:=1;
 CF:=1;
 i:=1;
 L:=Length(Path);
 While (i<=L) do
 begin
  if (Path[i]='/') then _c;
  Inc(i);
 end;
 _c;
end;

Procedure DoDirSeparators(var Path:RawByteString); inline;
var
 i:Integer;
begin
 if ('/'<>DirectorySeparator) and (Path<>'') then
  For i:=1 to Length(Path) do
   if (Path[i]='/') then Path[i]:=DirectorySeparator;
end;

Function IsSep(c:AnsiChar):Boolean; inline;
begin
 Result:=False;
 Case c of
  '\',
  '/':Result:=True;
 end;
end;

function IncludeTrailingPathDelimiter(Const Path:RawByteString):RawByteString; inline;
begin
 Result:=Path;
 if (Result='') or (not IsSep(Result[Length(Result)])) then Result:=Result+DirectorySeparator;
end;

function PathConcat(Path,filename:RawByteString):RawByteString;
begin
 Path:=Trim(Path);
 If (Path='') then Exit('');
 if (not IsSep(Path[Length(Path)])) then Path:=Path+DirectorySeparator;
 DoDirSeparators(filename);
 Result:=Path+filename;
end;

Function FetchSaveMount(path,point:PChar;mode:Integer):Integer;
var
 sp:RawByteString;
 s:TMountDir;
 i,m:Integer;
Label
 _exit;
begin
 Result:=0;
 if (path=nil) or (point=nil) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 s:=Default(TMountDir);
 MoveChar0(path^,s,DIRNAME_MAXSIZE);

 i:=IndexChar(s,MOUNT_MAXSIZE,#0);
 if (i=0) then Exit(SCE_SAVE_DATA_ERROR_PARAMETER);

 sp:=IncludeTrailingPathDelimiter(ps4_app.save_path)+s;

 if (mode and SCE_SAVE_DATA_MOUNT_MODE_CREATE2)<>0 then
 begin
  if not ForceDirectories(ps4_app.save_path) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  CreateDir(sp);
 end else
 if (mode and SCE_SAVE_DATA_MOUNT_MODE_CREATE)<>0 then
 begin
  if not ForceDirectories(ps4_app.save_path) then Exit(SCE_SAVE_DATA_ERROR_INTERNAL);
  //if not CreateDir(sp) then Exit(SCE_SAVE_DATA_ERROR_EXISTS);
  CreateDir(sp);
 end else
 begin
  if not DirectoryExists(sp) then Exit(SCE_SAVE_DATA_ERROR_NOT_FOUND);
 end;

 rwlock_wrlock(FSaveMounts_lock);

 m:=-1;
 For i:=0 to 15 do
 begin
  if (FSaveMounts[i]='') then
  begin
   if (m=-1) then m:=i;
  end else
  begin
   if (FSaveMounts[i]=s) then
   begin
    Result:=SCE_SAVE_DATA_ERROR_BUSY;
    goto _exit;
   end;
  end;
 end;

 if (m<>-1) then
 begin
  FSaveMounts[m]:=s;
  s:='/savedata'+IntToStr(m);
  Move(s,point^,MOUNT_MAXSIZE);
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_MOUNT_FULL;
 end;

 _exit:
 rwlock_unlock(FSaveMounts_lock);
end;

Function UnMountId(id:Byte):Integer;
begin
 Result:=0;
 rwlock_wrlock(FSaveMounts_lock);

 if (FSaveMounts[id]<>'') then
 begin
  FSaveMounts[id]:='';
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end;

 rwlock_unlock(FSaveMounts_lock);
end;

Function UnMountSavePath(path:PChar):Integer;
var
 s:TMountDir;
 i:Integer;
begin
 Result:=SCE_SAVE_DATA_ERROR_PARAMETER;
 if (path=nil) then Exit;

 s:=Default(TMountDir);
 MoveChar0(path^,s,MOUNT_MAXSIZE);

 if (s[0]<>'/') then Exit;

 i:=IndexChar(s,MOUNT_MAXSIZE,#0);

 Case i of
  10:Case PQWORD(@s[1])^ of
     $6174616465766173: //savedata
       begin
        Case s[9] of
         '0'..'9':
           begin
            Result:=UnMountId(ord(s[9])-ord('0'));
           end;
        end;
       end;
    end;

  11:Case PQWORD(@s[1])^ of
      $6174616465766173: //savedata
        begin
         Case PWORD(PBYTE(@s)+9)^ of
          $3031, //10
          $3131, //11
          $3231, //12
          $3331, //13
          $3431, //14
          $3531: //15
            begin
             Result:=UnMountId(ord(s[10])-ord('0')+10);
            end;
         end;
        end;
     end;
 end;

end;

Function GetSaveMount(id:Byte):RawByteString;
begin
 rwlock_rdlock(FSaveMounts_lock);
 Result:=FSaveMounts[id];
 rwlock_unlock(FSaveMounts_lock);
end;

// /app0
// /savedata0
// /savedata15

function MountSaveConcat(id:Byte;const filename:RawByteString):RawByteString;
var
 s:RawByteString;
begin
 s:=GetSaveMount(id);
 if (s='') then Exit('');

 s:=IncludeTrailingPathDelimiter(ps4_app.save_path)+s;

 Result:=PathConcat(s,filename);
end;

const
 P_ERR  =-1;
 P_NEXT =0;
 P_MOUNT=1;

function _parse_cast(var pp,fp:PChar;var Path:RawByteString):Integer;
begin
 Result:=P_NEXT;
 Case (fp-pp) of
  0:pp:=fp+1; //next
  3:Case (PDWORD(pp)^ and $00FFFFFF) of
     $00766564: //dev
       begin
        if (fp^<>#0) then Inc(fp);
        Result:=P_ERR; //TODO
       end;
     else
        Result:=P_ERR;
    end;
  4:Case PDWORD(pp)^ of
     $30707061: //app0
       begin
        if (fp^<>#0) then Inc(fp);
        Path:=PathConcat(ps4_app.app_path,fp);
        Result:=P_MOUNT;
       end;
     else
        Result:=P_ERR;
    end;
  9:Case PQWORD(pp)^ of
     $6174616465766173: //savedata
       begin
        Case (pp+8)^ of
         '0'..'9':
           begin
            if (fp^<>#0) then Inc(fp);
            Path:=MountSaveConcat(ord((pp+8)^)-ord('0'),fp);
            Result:=P_MOUNT;
           end;
         else
            Result:=P_ERR;
        end;
       end;
     else
       Result:=P_ERR;
    end;
 10:Case PQWORD(pp)^ of
     $6174616465766173: //savedata
       begin
        Case PWORD(pp+8)^ of
         $3031, //10
         $3131, //11
         $3231, //12
         $3331, //13
         $3431, //14
         $3531: //15
           begin
            if (fp^<>#0) then Inc(fp);
            Path:=MountSaveConcat(ord((pp+9)^)-ord('0')+10,fp);
            Result:=P_MOUNT;
           end;
         else
          Result:=P_ERR;
        end;
       end;
     else
       Result:=P_ERR;
    end;
  else
    begin
     //Writeln((fp-pp),'*',fp,'*',pp);
     Result:=P_ERR;
    end;
 end;
end;

function parse_filename(filename:PChar;var r:RawByteString):Integer;
var
 Path:RawByteString;
 pp,fp:PChar;

begin
 r:='';
 //Writeln(filename);
 if (filename=nil) then Exit(EINVAL);

 Path:=filename;
 DoFixRelative(Path);
 fp:=PChar(Path);
 pp:=PChar(Path);

 While (fp^<>#0) do
 begin
  Case fp^ of
   '/':
   begin
    Case _parse_cast(pp,fp,r) of
     P_MOUNT:Exit(0);      //mapped
     P_ERR  :Exit(EACCES); //not mapped
     else;
                           //next char
    end;
   end;
  end;
  Inc(fp);
 end;

 Case _parse_cast(pp,fp,r) of
  P_MOUNT:Exit(0); //mapped
  else;
    Exit(EACCES);  //not mapped
 end;

end;

initialization
 rwlock_init(FSaveMounts_lock);

end.


