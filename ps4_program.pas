unit ps4_program;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  Classes,
  SysUtils,
  RWLock,
  hamt,
  sys_types,
  sys_kernel,
  ps4_handles;

type
 PMODULE=^TMODULE;
 TMODULE=packed record
  attr:DWORD;
  Import:Boolean;
  strName:string[80];
 end;

 PLIBRARY=^TLIBRARY;

 T_set_proc_cb=function(lib:PLIBRARY;nid:QWORD;value:Pointer):Boolean;
 T_get_proc_cb=function(lib:PLIBRARY;nid:QWORD):Pointer;

 TElf_node=class;

 TLIBRARY=packed object
  parent:TElf_node;
  MapSymbol:THAMT;
  attr:DWORD;
  Import:Boolean;
  strName:string[80];
  Fset_proc_cb:T_set_proc_cb;
  Fget_proc_cb:T_get_proc_cb;
  function _set_proc(nid:QWORD;value:Pointer):Boolean;
  function _get_proc(nid:QWORD):Pointer;
  function set_proc(nid:QWORD;value:Pointer):Boolean;
  function get_proc(nid:QWORD):Pointer;
 end;

 TElf_node=class(TClassHandle)
  private
   pPrev,pNext:TElf_node;
  protected
   FHandle:Integer;
   FPrepared:Boolean;
   FLoadImport:Boolean;
   FInitProt:Boolean;
   FInitThread:Boolean;
   FInitCode:Boolean;
   aNeed:array of RawByteString;
   aMods:array of TMODULE;
   aLibs:array of PLIBRARY;
   procedure _set_filename(const name:RawByteString);
   procedure _add_need(const name:RawByteString); inline;
   procedure _set_mod(id:Word;_md:TMODULE); inline;
   procedure _set_mod_attr(u:TModuleValue); inline;
   function  _get_mod(id:Word):PMODULE; inline;
   procedure _set_lib(id:Word;lib:TLIBRARY); inline;
   procedure _set_lib_attr(u:TLibraryValue); inline;
   function  _get_lib(id:Word):PLIBRARY; inline;
  public
   pFileName:RawByteString;
   property   Handle:Integer read FHandle;
   property   Next:TElf_node read pNext;
   function   _add_lib(const strName:RawByteString):PLIBRARY;
   function   ModuleNameFromId(id:WORD):RawByteString;
   function   LibraryNameFromId(id:WORD):RawByteString;
   destructor Destroy; override;
   Procedure  Clean; virtual;
   function   Prepare:Boolean; virtual;
   Procedure  LoadSymbolImport(cbs,data:Pointer); virtual;
   Procedure  ReLoadSymbolImport(cbs,data:Pointer); virtual;
   Procedure  InitThread(is_static:QWORD); virtual;
   Procedure  FreeThread; virtual;
   Procedure  InitProt;   virtual;
   Procedure  InitCode;   virtual;
   function   module_start(argc:size_t;argp:PPointer):Integer; virtual;
   function   GetCodeFrame:TMemChunk; virtual;
   function   GetEntryPoint:Pointer; virtual;
   Function   GetModuleInfo:TKernelModuleInfo; virtual;
   Function   get_proc(nid:QWORD):Pointer;
   Function   get_proc_by_name(const name:RawByteString):Pointer;
 end;

 TOnElfLoadCb=function(Const name:RawByteString):TElf_node;

 Thamt64locked=object
  lock:TRWLock;
  hamt:TSTUB_HAMT64;
  Procedure Init;
  Procedure LockRd;
  Procedure LockWr;
  Procedure Unlock;
 end;

 Thamt64locked_proc=object(Thamt64locked)
  function _set_proc(nid:QWORD;value:Pointer):Boolean;
  function _get_proc(nid:QWORD):Pointer;
 end;

 TElfNodeList=object(Thamt64locked)
  pHead,pTail:TElf_node;
  procedure Push_head(Node:TElf_node);
  procedure Push_tail(Node:TElf_node);
  function  Pop_head:TElf_node;
  function  Pop_tail:TElf_node;
  procedure InsertAfter(node,new:TElf_node);
  procedure InsertBefore(node,new:TElf_node);
  procedure Remove(node:TElf_node);
 end;

 Tps4_program=object
  public
   resolve_cb:Pointer;
   reload_cb:Pointer;
   prog:TElf_node;
   app_file:RawByteString;
   app_path:RawByteString;
   save_path:RawByteString;
  private
   pre_load:Thamt64locked_proc;
   fin_load:Thamt64locked_proc;

   files:TElfNodeList;
   mods:Thamt64locked;
   libs:Thamt64locked;
   elfs:TIntegerHandles;
   Procedure RegistredFile(node:TElf_node);
   Procedure RegistredMod(node:TElf_node;const strName:RawByteString);
  public
   Procedure LockRd;
   Procedure Unlock;
   function  FirstFile:TElf_node;
   function  AcqureFileByName(const strName:RawByteString):TElf_node;
   procedure PopupFile(node:TElf_node);
   Procedure SetLib(lib:PLIBRARY);
   function  GetLib(const strName:RawByteString):PLIBRARY;
   Procedure RegistredElf(node:TElf_node);
   Procedure RegistredPreLoad(const strName:RawByteString;cb:TOnElfLoadCb);
   Procedure RegistredFinLoad(const strName:RawByteString;cb:TOnElfLoadCb);
   function  Loader(Const name:RawByteString):TElf_node;
   Procedure ResolveDepended(node:TElf_node);
   Procedure LoadSymbolImport(data:Pointer);
   Procedure ReLoadSymbolImport(data:Pointer);
   Procedure InitProt;
   Procedure InitCode;
   Procedure InitThread(is_static:QWORD);
   Procedure FreeThread;
   function  AcqureFileByCodeAdr(Adr:Pointer):TElf_node;
   function  AcqureFileByHandle(handle:Integer):TElf_node;
 end;

var
 ps4_app:Tps4_program;

Procedure DumpExceptionCallStack(E: Exception);

Function  FetchMount(path,point:PChar):Integer;
Function  UnMountPath(path:PChar):Integer;

function  _parse_filename(filename:PChar):RawByteString;

function  GetProcParam:Pointer;
Function  get_dev_progname:RawByteString;

implementation

uses
 ps4_elf,ps4_elf_tls;

Procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := '';
  if E <> nil then
  begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Writeln(StdErr,Report);
end;

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

const
 DIRNAME_MAXSIZE=32;
 MOUNT_MAXSIZE=16;

type
 TMountDir=array[0..DIRNAME_MAXSIZE-1] of AnsiChar;

var
 FMounts_lock:TRWLock;
 FMounts:array[0..15] of TMountDir;

const
 SCE_SAVE_DATA_ERROR_PARAMETER  =-2137063424; // 0x809F0000
 SCE_SAVE_DATA_ERROR_EXISTS     =-2137063417; // 0x809F0007
 SCE_SAVE_DATA_ERROR_MOUNT_FULL =-2137063412; // 0x809F000C
 SCE_SAVE_DATA_ERROR_NOT_MOUNTED=-2137063420; // 0x809F0004

Function FetchMount(path,point:PChar):Integer;
var
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

 rwlock_wrlock(FMounts_lock);

 m:=-1;
 For i:=0 to 15 do
 begin
  if (FMounts[i]='') then
  begin
   if (m=-1) then m:=i;
  end else
  begin
   if (FMounts[i]=s) then
   begin
    Result:=SCE_SAVE_DATA_ERROR_EXISTS;
    goto _exit;
   end;
  end;
 end;

 if (m<>-1) then
 begin
  FMounts[m]:=s;
  s:='/savedata'+IntToStr(m);
  Move(s,point^,MOUNT_MAXSIZE);
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_MOUNT_FULL;
 end;

 _exit:
 rwlock_unlock(FMounts_lock);
end;

Function UnMountId(id:Byte):Integer;
begin
 Result:=0;
 rwlock_wrlock(FMounts_lock);

 if (FMounts[id]<>'') then
 begin
  FMounts[id]:='';
 end else
 begin
  Result:=SCE_SAVE_DATA_ERROR_NOT_MOUNTED;
 end;

 rwlock_unlock(FMounts_lock);
end;

Function UnMountPath(path:PChar):Integer;
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

Function GetMount(id:Byte):RawByteString;
begin
 rwlock_rdlock(FMounts_lock);
 Result:=FMounts[id];
 rwlock_unlock(FMounts_lock);
end;

// /app0
// /savedata0
// /savedata15

function MountConcat(id:Byte;const filename:RawByteString):RawByteString;
var
 s:RawByteString;
begin
 s:=GetMount(id);
 if (s='') then Exit('');

 s:=IncludeTrailingPathDelimiter(ps4_app.save_path)+s;

 if not ForceDirectories(s) then Exit('');

 Result:=PathConcat(s,filename);
end;

function _parse_filename(filename:PChar):RawByteString;
var
 Path:RawByteString;
 pp,fp:PChar;
begin
 Result:='';
 //Writeln(filename);
 if (filename=nil) then Exit;
 Path:=filename;
 DoFixRelative(Path);
 fp:=PChar(Path);
 pp:=PChar(Path);
 While (fp^<>#0) do
 begin
  Case fp^ of
   '/':Case (fp-pp) of
        0:pp:=fp+1; //next
        4:Case PDWORD(pp)^ of
           $30707061: //app0
             begin
              Inc(fp);
              Result:=PathConcat(ps4_app.app_path,fp);
              Exit;
             end;
           else
             Break;
          end;
        9:Case PQWORD(pp)^ of
           $6174616465766173: //savedata
             begin
              Case (pp+8)^ of
               '0'..'9':
                 begin
                  Inc(fp);
                  Result:=MountConcat(ord((pp+8)^)-ord('0'),fp);
                  //Result:=PathConcat(GetCurrentDir,fp);
                  Exit;
                 end;
               else
                Break;
              end;
             end;
           else
             Break;
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
                  Inc(fp);
                  Result:=MountConcat(ord((pp+9)^)-ord('0')+10,fp);
                  //Result:=PathConcat(GetCurrentDir,fp);
                  Exit;
                 end;
               else
                Break;
              end;
             end;
           else
             Break;
          end;
        else
          begin
           //Writeln((fp-pp),'*',fp,'*',pp);
           Break;
          end;
       end;
  end;
  Inc(fp);
 end;
 fp:=PChar(Path);
 if (fp^='/') then Inc(fp);
 Result:=PathConcat(GetCurrentDir,fp);
end;

//--

procedure TElfNodeList.Push_head(Node:TElf_node);
begin
 if (pHead=nil) then
 begin
  pTail:=node;
  node.pNext:=nil;
 end else
 begin
  pHead.pPrev:=node;
  node.pNext:=pHead;
 end;
 node.pPrev:=nil;
 pHead:=node;
end;

procedure TElfNodeList.Push_tail(Node:TElf_node);
begin
 if (pTail=nil) then
 begin
  pHead:=node;
  node.pPrev:=nil;
 end else
 begin
  pTail.pNext:=node;
  node.pPrev:=pTail;
 end;
 node.pNext:=nil;
 pTail:=node;
end;

function TElfNodeList.Pop_head:TElf_node;
begin
 if (pHead=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pHead;
  pHead:=pHead.pNext;
  if (pHead=nil) then
  begin
   pTail:=nil;
  end else
  begin
   pHead.pPrev:=nil;
  end;
  Result.pPrev:=nil;
  Result.pNext:=nil;
 end;
end;

function TElfNodeList.Pop_tail:TElf_node;
begin
 if (pTail=nil) then
 begin
  Result:=nil;
 end else
 begin
  Result:=pTail;
  pTail:=pTail.pPrev;
  if (pTail=nil) then
  begin
   pHead:=nil;
  end else
  begin
   pTail.pNext:=nil;
  end;
  Result.pPrev:=nil;
  Result.pNext:=nil;
 end;
end;

procedure TElfNodeList.InsertAfter(node,new:TElf_node);
begin
 new.pPrev:=node;
 if (node.pNext=nil) then
 begin
  new.pNext:=nil;
  pTail:=new;
 end else
 begin
  new.pNext:=node.pNext;
  node.pNext.pPrev:=new;
 end;
 node.pNext:=new;
end;

procedure TElfNodeList.InsertBefore(node,new:TElf_node);
begin
 new.pNext:=node;
 if (node.pPrev=nil) then
 begin
  new.pPrev:=nil;
  pHead:=new;
 end else
 begin
  new.pPrev:=node.pPrev;
  node.pPrev.pNext:=new;
 end;
 node.pPrev:=new;
end;

procedure TElfNodeList.Remove(node:TElf_node);
begin
 if (node.pPrev=nil) then
 begin
  if (pHead=node) then
  begin
   pHead:=node.pNext;
  end;
 end else
 begin
  node.pPrev.pNext:=node.pNext;
 end;
 if (node.pNext=nil) then
 begin
  if (pTail=node) then
  begin
   pTail:=node.pPrev;
  end;
 end else
 begin
  node.pNext.pPrev:=node.pPrev;
 end;
end;

//

procedure TElf_node._set_filename(const name:RawByteString);
var
 i,L:SizeInt;
begin
 i:=Length(name);
 While (i>1) do
 begin
  if (name[i] in AllowDirectorySeparators) then
  begin
   Inc(i);
   L:=Length(name)-i+1;
   pFileName:=Copy(name,i,L);
   Exit;
  end;
  Dec(i);
 end;
 pFileName:=name;
end;

procedure TElf_node._add_need(const name:RawByteString); inline;
var
 i:SizeInt;
begin
 i:=Length(aNeed);
 SetLength(aNeed,i+1);
 aNeed[i]:=name;
end;

procedure TElf_node._set_mod(id:Word;_md:TMODULE); inline;
var
 i:SizeInt;
begin
 i:=Length(aMods);
 if (i<=id) then
 begin
  i:=id+1;
  SetLength(aMods,i);
 end;
 aMods[id]:=_md;
end;

procedure TElf_node._set_mod_attr(u:TModuleValue); inline;
var
 i:SizeInt;
begin
 i:=Length(aMods);
 if (i<=u.id) then
 begin
  i:=u.id+1;
  SetLength(aMods,i);
 end;
 aMods[u.id].attr:=u.name_offset;
end;

function TElf_node._get_mod(id:Word):PMODULE; inline;
begin
 Result:=nil;
 if (Length(aMods)>id) then
 begin
  Result:=@aMods[id];
 end;
end;

procedure TElf_node._set_lib(id:Word;lib:TLIBRARY); inline;
var
 i:SizeInt;
 plib:PLIBRARY;
begin
 i:=Length(aLibs);
 if (i<=id) then
 begin
  i:=id+1;
  SetLength(aLibs,i);
 end;
 plib:=aLibs[id];
 if (plib=nil) then plib:=AllocMem(SizeOf(TLIBRARY));
 plib^:=lib;
 plib^.parent:=Self;
 aLibs[id]:=plib;
end;

procedure TElf_node._set_lib_attr(u:TLibraryValue); inline;
var
 i:SizeInt;
 plib:PLIBRARY;
begin
 i:=Length(aLibs);
 if (i<=u.id) then
 begin
  i:=u.id+1;
  SetLength(aLibs,i);
 end;
 plib:=aLibs[u.id];
 if (plib=nil) then plib:=AllocMem(SizeOf(TLIBRARY));
 plib^.attr:=u.name_offset;
 plib^.parent:=Self;
 aLibs[u.id]:=plib;
end;

function TElf_node._get_lib(id:Word):PLIBRARY; inline;
begin
 Result:=nil;
 if (Length(aLibs)>id) then
 begin
  Result:=aLibs[id];
 end;
end;

function TElf_node._add_lib(const strName:RawByteString):PLIBRARY;
var
 i:SizeInt;
begin
 i:=Length(aLibs);
 SetLength(aLibs,i+1);
 Result:=AllocMem(SizeOf(TLIBRARY));
 Result^.parent:=Self;
 Result^.strName:=strName;
 aLibs[i]:=Result;
end;

function TElf_node.ModuleNameFromId(id:WORD):RawByteString;
var
 _md:PMODULE;
begin
 Result:='';
 if (Self=nil) then Exit;
 _md:=_get_mod(id);
 if (_md<>nil) then Result:=_md^.strName;
end;

function TElf_node.LibraryNameFromId(id:WORD):RawByteString;
var
 lib:PLIBRARY;
begin
 Result:='';
 if (Self=nil) then Exit;
 lib:=_get_lib(id);
 if (lib<>nil) then Result:=lib^.strName;
end;

procedure _free_map_cb(data,userdata:Pointer);
begin
 FreeMem(data);
end;

Procedure TElf_node.Clean;
var
 i:SizeInt;
 lib:PLIBRARY;
begin
 if (Self=nil) then Exit;
 if Length(aLibs)<>0 then
 begin
  For i:=0 to Length(aLibs)-1 do
  begin
   lib:=aLibs[i];
   if (lib<>nil) then
   begin
    HAMT_destroy64(lib^.MapSymbol,@_free_map_cb,nil);
    lib^.strName:='';
    FreeMem(lib);
   end;
  end;
 end;
 pFileName:='';
 SetLength(aNeed,0);
 SetLength(aMods,0);
 SetLength(aLibs,0);
end;

destructor TElf_node.Destroy;
begin
 Clean;
 inherited;
end;

function TElf_node.Prepare:Boolean;
begin
 Result:=True;
 FPrepared:=True;;
end;

Procedure TElf_node.LoadSymbolImport(cbs,data:Pointer);
begin
 FLoadImport:=True;
end;

Procedure TElf_node.ReLoadSymbolImport(cbs,data:Pointer);
begin

end;

Procedure TElf_node.InitThread(is_static:QWORD);
begin

end;

Procedure TElf_node.FreeThread;
begin

end;

Procedure TElf_node.InitProt;
begin
 FInitProt:=True;
end;

Procedure TElf_node.InitCode;
begin
 FInitCode:=True;
end;

function TElf_node.module_start(argc:size_t;argp:PPointer):Integer;
begin
 Result:=0;
end;

function TElf_node.GetCodeFrame:TMemChunk;
begin
 Result:=Default(TMemChunk);
end;

function TElf_node.GetEntryPoint:Pointer;
begin
 Result:=nil;
end;

Function TElf_node.GetModuleInfo:TKernelModuleInfo;
begin
 Result:=Default(TKernelModuleInfo);
 Result.size:=SizeOf(TKernelModuleInfo);

 MoveChar0(PChar(pFileName)^,Result.name,SCE_DBG_MAX_NAME_LENGTH);

//segmentInfo:array[0..SCE_DBG_MAX_SEGMENTS-1] of TKernelModuleSegmentInfo;
//segmentCount:DWORD;
//fingerprint:array[0..SCE_DBG_NUM_FINGERPRINT-1] of Byte;
end;

Function TElf_node.get_proc(nid:QWORD):Pointer;
var
 i:Integer;
begin
 Result:=nil;
 if Length(aLibs)<>0 then
 begin
  For i:=0 to Length(aLibs)-1 do
  if (aLibs[i]<>nil) then
  if (not aLibs[i]^.Import) then
  begin
   Result:=aLibs[i]^.get_proc(nid);
   if (Result<>nil) then Exit;
  end;
 end;
end;

Function TElf_node.get_proc_by_name(const name:RawByteString):Pointer;
begin
 Result:=get_proc(ps4_nid_hash(name));
end;

function TLIBRARY._set_proc(nid:QWORD;value:Pointer):Boolean;
var
 data:PPointer;
 PP:PPointer;
begin
 if (MapSymbol=nil) then MapSymbol:=HAMT_create64;

 data:=nil;
 PP:=HAMT_search64(MapSymbol,nid);
 if (PP<>nil) then data:=PP^;

 if (data=nil) then
 begin
  data:=GetMem(SizeOf(Pointer)*2);
  data[0]:=value;
  data[1]:=Pointer(nid);
  PP:=HAMT_insert64(MapSymbol,nid,data);
  Assert(PP<>nil);
  Result:=(PP^=data);
  if not Result then
  begin
   FreeMem(data);
  end;
 end else
 begin
  data[0]:=value;
 end;

end;

function TLIBRARY._get_proc(nid:QWORD):Pointer;
var
 data:PPointer;
 PP:PPointer;
begin
 Result:=nil;
 data:=nil;
 PP:=HAMT_search64(MapSymbol,nid);
 if (PP<>nil) then data:=PP^;
 if (data<>nil) then Result:=data^;
end;

function TLIBRARY.set_proc(nid:QWORD;value:Pointer):Boolean;
begin
 if (Fset_proc_cb<>nil) then
  Result:=Fset_proc_cb(@self,nid,value)
 else
  Result:=_set_proc(nid,value);
end;

function TLIBRARY.get_proc(nid:QWORD):Pointer;
begin
 if (Fget_proc_cb<>nil) then
  Result:=Fget_proc_cb(@self,nid)
 else
  Result:=_get_proc(nid);
end;

Procedure Tps4_program.RegistredFile(node:TElf_node);
var
 nid:QWORD;
 PP:PPointer;
begin
 files.LockWr;
 files.Remove(node);
 files.Push_tail(node);

 nid:=ps4_nid_hash(node.pFileName);
 PP:=HAMT_insert64(@files.hamt,nid,Pointer(node));
 Assert(PP<>nil);
 if (PP^<>Pointer(node)) then Writeln('Warn, ',node.pFileName,' file is registred');

 files.Unlock;
end;

Procedure Tps4_program.LockRd;
begin
 files.LockRd;
end;

Procedure Tps4_program.Unlock;
begin
 files.Unlock;
end;

function Tps4_program.FirstFile:TElf_node;
begin
 Result:=files.pHead;
end;

function Tps4_program.AcqureFileByName(const strName:RawByteString):TElf_node;
var
 nid:QWORD;
 PP:PPointer;
begin
 Result:=nil;
 nid:=ps4_nid_hash(strName);

 files.LockRd;

 PP:=HAMT_search64(@files.hamt,nid);
 if (PP<>nil) then Result:=TElf_node(PP^);

 if (Result<>nil) then Result.Acqure;

 files.Unlock;
end;

procedure Tps4_program.PopupFile(node:TElf_node);
begin
 if (node=nil) then Exit;
 files.LockRd;
 files.Remove(node);
 files.Push_head(node);
 files.Unlock;
end;

Procedure Tps4_program.RegistredMod(node:TElf_node;const strName:RawByteString);
var
 nid:QWORD;
 PP:PPointer;
begin
 nid:=ps4_nid_hash(strName);
 PP:=HAMT_insert64(@mods.hamt,nid,Pointer(node));
 Assert(PP<>nil);
 if (PP^<>Pointer(node)) then Writeln('Warn, ',strName,' module is registred');
end;

Procedure Tps4_program.SetLib(lib:PLIBRARY);
var
 nid:QWORD;
 PP:PPointer;
begin
 if (lib=nil) then Exit;
 if lib^.Import then Exit;
 //Writeln('Regs, ',lib^.strName);
 nid:=ps4_nid_hash(lib^.strName);
 PP:=HAMT_insert64(@libs.hamt,nid,Pointer(lib));
 Assert(PP<>nil);
 if (PP^<>Pointer(lib)) then Writeln('Warn, ',lib^.strName,' lib is registred');
end;

function Tps4_program.GetLib(const strName:RawByteString):PLIBRARY;
var
 nid:QWORD;
 PP:PPointer;
begin
 Result:=nil;
 nid:=ps4_nid_hash(strName);
 libs.LockRd;
 PP:=HAMT_search64(@libs.hamt,nid);
 if (PP<>nil) then Result:=PP^;
 libs.Unlock;
end;

Procedure Tps4_program.RegistredElf(node:TElf_node);
var
 FHandle:Integer;
 i:SizeInt;
begin
 if (node=nil) then Exit;
 FHandle:=0;
 if not elfs.New(node,FHandle) then
  raise Exception.Create('Error alloc handle');
 node.FHandle:=FHandle;

 RegistredFile(node);

 if Length(node.aMods)<>0 then
 begin
  mods.LockWr;
  For i:=0 to Length(node.aMods)-1 do
  with node.aMods[i] do
   if not Import then
    RegistredMod(node,strName);
  mods.Unlock;
 end;

 if Length(node.aLibs)<>0 then
 begin
  libs.LockWr;
  For i:=0 to Length(node.aLibs)-1 do
   SetLib(node.aLibs[i]);
  libs.Unlock;
 end;

 node.Release;
end;

function Thamt64locked_proc._set_proc(nid:QWORD;value:Pointer):Boolean;
var
 data:PPointer;
 PP:PPointer;
begin
 data:=GetMem(SizeOf(Pointer));
 data^:=value;
 PP:=HAMT_insert64(@hamt,nid,data);
 Assert(PP<>nil);
 Result:=(PP^=data);
 if not Result then
 begin
  FreeMem(data);
 end;
end;

function Thamt64locked_proc._get_proc(nid:QWORD):Pointer;
var
 data:PPointer;
 PP:PPointer;
begin
 Result:=nil;
 data:=nil;
 PP:=HAMT_search64(@hamt,nid);
 if (PP<>nil) then data:=PP^;
 if (data<>nil) then Result:=data^;
end;

Procedure Tps4_program.RegistredPreLoad(const strName:RawByteString;cb:TOnElfLoadCb);
var
 nid:QWORD;
begin
 if (cb=nil) then Exit;
 nid:=ps4_nid_hash(strName);
 pre_load.LockWr;
 if not pre_load._set_proc(nid,Pointer(cb)) then
 begin
  Writeln('Warn, ',strName,' is registred')
 end;
 pre_load.Unlock;
end;

Procedure Tps4_program.RegistredFinLoad(const strName:RawByteString;cb:TOnElfLoadCb);
var
 nid:QWORD;
begin
 if (cb=nil) then Exit;
 nid:=ps4_nid_hash(strName);
 fin_load.LockWr;
 if not fin_load._set_proc(nid,Pointer(cb)) then
 begin
  Writeln('Warn, ',strName,' is registred')
 end;
 fin_load.Unlock;
end;

type
 TNodeStack=object
  type
   PNode=^TNode;
   TNode=record
    pNext:PNode;
    S:RawByteString;
   end;
  var
   pHead:PNode;
  procedure Push(Const S:RawByteString);
  function  Pop:RawByteString;
 end;

procedure TNodeStack.Push(Const S:RawByteString);
var
 Node:PNode;
begin
 Node:=AllocMem(SizeOf(TNode));
 Node^.S:=S;
 if (pHead=nil) then
 begin
  node^.pNext:=nil;
 end else
 begin
  node^.pNext:=pHead;
 end;
 pHead:=node;
end;

function TNodeStack.Pop:RawByteString;
var
 Node:PNode;
begin
 Result:='';
 Node:=pHead;
 if (pHead<>nil) then
 begin
  pHead:=pHead^.pNext;
 end;
 if (Node<>nil) then
 begin
  Node^.pNext:=nil;
 end;
 if (Node<>nil) then
 begin
  Result:=Node^.S;
  FreeMem(Node);
 end;
end;

function Tps4_program.Loader(Const name:RawByteString):TElf_node;
var
 nid:QWORD;
 PP:PPointer;
 cb:TOnElfLoadCb;
begin
 Result:=nil;
 nid:=ps4_nid_hash(name);
 files.LockRd;
 PP:=HAMT_search64(@files.hamt,nid);
 if (PP<>nil) then Result:=TElf_node(PP^);
 files.Unlock;
 if (Result<>nil) then Exit; //is loaded

 cb:=nil;
 pre_load.LockRd;
 cb:=TOnElfLoadCb(pre_load._get_proc(nid));
 pre_load.Unlock;
 if (cb<>nil) then Result:=cb(name);
 if (Result<>nil) then //is pre load
 begin
  Result.Prepare;
  ps4_app.RegistredElf(Result);
  Exit;
 end;

 Result:=LoadPs4ElfFromFile(IncludeTrailingPathDelimiter(app_path)+'sce_module'+DirectorySeparator+name);
 if (Result<>nil) then //is default load app_path\sce_module
 begin
  Result.Prepare;
  ps4_app.RegistredElf(Result);
  Exit;
 end;

 //
 //Result:=LoadPs4ElfFromFile(IncludeTrailingPathDelimiter(app_path)+'Media'+DirectorySeparator+'Modules'+DirectorySeparator+name);
 //if (Result<>nil) then //is app_path\Media\Modules
 //begin
 // Result.Prepare;
 // ps4_app.RegistredElf(Result);
 // Exit;
 //end;
 //
 //Result:=LoadPs4ElfFromFile(IncludeTrailingPathDelimiter(app_path)+'Media'+DirectorySeparator+'Plugins'+DirectorySeparator+name);
 //if (Result<>nil) then //is app_path\Media\Plugins
 //begin
 // Result.Prepare;
 // ps4_app.RegistredElf(Result);
 // Exit;
 //end;

 //

 Result:=LoadPs4ElfFromFile(IncludeTrailingPathDelimiter(GetCurrentDir)+'sce_module'+DirectorySeparator+name);
 if (Result<>nil) then //is default load current_dir\sce_module
 begin
  Result.Prepare;
  ps4_app.RegistredElf(Result);
  Exit;
 end;

 cb:=nil;
 fin_load.LockRd;
 cb:=TOnElfLoadCb(fin_load._get_proc(nid));
 fin_load.Unlock;
 if (cb<>nil) then Result:=cb(name);
 if (Result<>nil) then //is fin load
 begin
  Result.Prepare;
  ps4_app.RegistredElf(Result);
  Exit;
 end;
end;

Procedure Tps4_program.ResolveDepended(node:TElf_node);
var
 fstack:TNodeStack;
 s:RawByteString;

 Procedure _to_stack;
 var
  i:SizeInt;
 begin
  if Length(node.aNeed)<>0 then
   For i:=0 to Length(node.aNeed)-1 do
   begin
    fstack.Push(node.aNeed[i]);
   end;
 end;

begin
 if (node=nil) then Exit;
 fstack:=Default(TNodeStack);
 _to_stack;
 While (fstack.pHead<>nil) do
 begin
  S:=fstack.Pop;
  node:=Loader(S);
  if (node=nil) then
  begin
   Writeln('Warn, file ',S,' not loaded!');
  end else
  begin
   PopupFile(node);
   _to_stack;
  end;
 end;
end;

Procedure Tps4_program.LoadSymbolImport(data:Pointer);
var
 Node:TElf_node;
begin
 files.LockRd;
 Node:=files.pHead;
 While (Node<>nil) do
 begin
  Node.LoadSymbolImport(resolve_cb,data);
  Node:=Node.pNext;
 end;
 files.Unlock;
end;

Procedure Tps4_program.ReLoadSymbolImport(data:Pointer);
var
 Node:TElf_node;
begin
 files.LockRd;
 Node:=files.pHead;
 While (Node<>nil) do
 begin
  Node.ReLoadSymbolImport(reload_cb,data);
  Node:=Node.pNext;
 end;
 files.Unlock;
end;

Procedure Tps4_program.InitProt;
var
 Node:TElf_node;
begin
 files.LockRd;
 Node:=files.pHead;
 While (Node<>nil) do
 begin
  Node.InitProt;
  Node:=Node.pNext;
 end;
 files.Unlock;
end;

Procedure Tps4_program.InitCode;
var
 Node:TElf_node;
begin
 Assert(ps4_app.prog<>nil);
 files.LockRd;

 Node:=files.pHead;
 While (Node<>nil) do
 begin
  Node.InitCode;
  Node:=Node.pNext;
 end;

 files.Unlock;
end;

Procedure Tps4_program.InitThread(is_static:QWORD);
var
 Node:TElf_node;
begin
 files.LockRd;
 Node:=files.pHead;
 While (Node<>nil) do
 begin
  Node.InitThread(is_static);
  Node:=Node.pNext;
 end;
 files.Unlock;
end;

Procedure Tps4_program.FreeThread;
var
 Node:TElf_node;
begin
 files.LockRd;
 Node:=files.pHead;
 While (Node<>nil) do
 begin
  Node.FreeThread;
  Node:=Node.pNext;
 end;
 files.Unlock;
 _free_tls_tcb_all;
end;

function Tps4_program.AcqureFileByCodeAdr(Adr:Pointer):TElf_node;
var
 tmp:TElfNodeList;
 Node:TElf_node;
 Mem:TMemChunk;
begin
 Result:=nil;
 tmp:=Default(TElfNodeList);
 if safe_move(files,tmp,SizeOf(TElfNodeList))<>SizeOf(TElfNodeList) then Exit;
 files.LockRd;

 Node:=tmp.pHead;
 While (Node<>nil) do
 begin
  Mem:=Node.GetCodeFrame;
  if (Mem.pAddr<>nil) and (Mem.nSize<>0) then
  begin
   if (Adr>=Mem.pAddr) and (Adr<(Mem.pAddr+Mem.nSize)) then
   begin
    Result:=Node;
    Result.Acqure;

    files.Unlock;
    Exit;
   end;
  end;
  safe_move_ptr(Node.pNext,Node);
 end;

 files.Unlock;
end;

function Tps4_program.AcqureFileByHandle(handle:Integer):TElf_node;
begin
 Result:=TElf_node(elfs.Acqure(handle));
end;

Procedure Thamt64locked.Init;
begin
 FillChar(Self,SizeOf(Self),0);
 rwlock_init(lock);
end;

Procedure Thamt64locked.LockRd;
begin
 rwlock_rdlock(lock);
end;

Procedure Thamt64locked.LockWr;
begin
 rwlock_wrlock(lock);
end;

Procedure Thamt64locked.Unlock;
begin
 rwlock_unlock(lock);
end;

function GetProcParam:Pointer;
var
 elf:Telf_file;
begin
 Result:=nil;
 elf:=Telf_file(ps4_program.ps4_app.prog);
 if (elf=nil) then Exit;
 if (elf.pProcParam=0) then Exit;
 Result:=elf.mMap.pAddr+elf.pProcParam;
end;

Function get_dev_progname:RawByteString;
begin
 Result:='';
 if (ps4_app.prog=nil) then Exit;
 Result:=ChangeFileExt(ExtractFileName(Telf_file(ps4_app.prog).pSceFileName),'');
end;

initialization
 ps4_app.pre_load.Init;
 ps4_app.fin_load.Init;
 ps4_app.files.Init;
 ps4_app.mods.Init;
 ps4_app.libs.Init;
 ps4_app.elfs:=TIntegerHandles.Create;

 rwlock_init(FMounts_lock);

end.

