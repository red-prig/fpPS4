unit open_dialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

function DoOpenFile(const Input,InitialDir:RawByteString):RawByteString;
function DoOpenDir (const Input,InitialDir:RawByteString):RawByteString;

implementation

uses
 ms_shell_hack;

function DoOpenFile(const Input,InitialDir:RawByteString):RawByteString;
var
 d:TOpenDialog;
 Cookie:Pointer;
begin
 Result:=Input;
 d:=nil;

 Cookie:=RegisterDllHack;

 try
  d:=TOpenDialog.Create(nil);
  d.InitialDir:=InitialDir;
  d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];
  if d.Execute then
  begin
   Result:=d.FileName;
  end;
 except
  //
 end;
 FreeAndNil(d);

 UnregisterDllHack(Cookie);
end;

function DoOpenDir(const Input,InitialDir:RawByteString):RawByteString;
var
 d:TSelectDirectoryDialog;
 Cookie:Pointer;
begin
 Result:=Input;
 d:=nil;

 Cookie:=RegisterDllHack;

 try
  d:=TSelectDirectoryDialog.Create(nil);
  d.InitialDir:=InitialDir;
  d.Options:=[ofPathMustExist,ofEnableSizing,ofViewDetail];
  if d.Execute then
  begin
   Result:=d.FileName;
  end;
 except
  //
 end;
 FreeAndNil(d);

 UnregisterDllHack(Cookie);
end;

end.

