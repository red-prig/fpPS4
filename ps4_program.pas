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
   FStatic:Boolean;
   FPrepared:Boolean;
   FLoadImport:Boolean;
   FInitProt:Boolean;
   FInitThread:Boolean;
   FInitCode:Boolean;
   aNeed:array of RawByteString;
   aMods:array of TMODULE;
   aLibs:array of PLIBRARY;
   procedure _set_filename(const name:RawByteString);
   procedure _add_need(const name:RawByteString);
   procedure _set_mod(id:Word;_md:TMODULE);
   procedure _set_mod_attr(u:TModuleValue);
   function  _get_mod(id:Word):PMODULE;
   procedure _set_lib(id:Word;lib:TLIBRARY);
   procedure _set_lib_attr(u:TLibraryValue);
   function  _get_lib(id:Word):PLIBRARY;
   function  _find_mod_export:Word;
   function  _find_lib_export:Word;
  public
   pFileName:RawByteString;
   property    IsStatic:Boolean read FStatic write FStatic;
   property    IsInit:Boolean read FInitCode write FInitCode;
   property    Handle:Integer read FHandle;
   property    Next:TElf_node read pNext;
   function    _add_lib(const strName:RawByteString):PLIBRARY;
   function    ModuleNameFromId(id:WORD):RawByteString;
   function    LibraryNameFromId(id:WORD):RawByteString;
   Constructor Create;
   destructor  Destroy; override;
   Procedure   Clean; virtual;
   function    Prepare:Boolean; virtual;
   Procedure   LoadSymbolImport(cbs,data:Pointer); virtual;
   Procedure   ReLoadSymbolImport(cbs,data:Pointer); virtual;
   Procedure   InitThread(is_static:QWORD); virtual;
   Procedure   FreeThread; virtual;
   Procedure   InitProt;   virtual;
   Procedure   InitCode;   virtual;
   function    module_start(argc:size_t;argp,param:PPointer):Integer; virtual;
   function    GetCodeFrame:TMemChunk; virtual;
   function    GetEntryPoint:Pointer; virtual;
   Function    GetModuleInfo:SceKernelModuleInfo; virtual;
   Function    GetModuleInfoEx:SceKernelModuleInfoEx; virtual;
   Function    get_proc(nid:QWORD):Pointer;
   Function    get_proc_by_name(const name:RawByteString):Pointer;
 end;

 TOnElfLoadCb=function(Const name:RawByteString):TElf_node;

 Phamt64locked=^Thamt64locked;
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
   function  RegistredFile(node:TElf_node):Boolean;
   Procedure RegistredMod(node:TElf_node;const strName:RawByteString);
  public
   Procedure LockRd;
   Procedure LockWr;
   Procedure Unlock;
   function  FirstFile:TElf_node;
   function  AcqureFileByName(const strName:RawByteString):TElf_node;
   procedure PopupFile(node:TElf_node);
   Procedure SetLib(lib:PLIBRARY);
   function  GetLib(const strName:RawByteString):PLIBRARY;
   function  RegistredElf(node:TElf_node):Boolean;
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

function  GetSceProcParam:Pointer;
function  GetSceUserMainThreadName:PChar;
function  GetSceUserMainThreadPriority:PDWORD;
function  GetSceUserMainThreadStackSize:PDWORD;
function  GetSceLibcParam:Pointer;
function  GetSceKernelMemParam:Pointer;
function  GetSceKernelFlexibleMemorySize:PQWORD;

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

procedure TElf_node._add_need(const name:RawByteString);
var
 i:SizeInt;
begin
 i:=Length(aNeed);
 SetLength(aNeed,i+1);
 aNeed[i]:=name;
end;

procedure TElf_node._set_mod(id:Word;_md:TMODULE);
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

procedure TElf_node._set_mod_attr(u:TModuleValue);
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

function TElf_node._get_mod(id:Word):PMODULE;
begin
 Result:=nil;
 if (Length(aMods)>id) then
 begin
  Result:=@aMods[id];
 end;
end;

function TElf_node._find_mod_export:Word;
var
 i:Word;
begin
 Result:=0;
 if Length(aMods)>0 then
 For i:=0 to High(aMods) do
  if not aMods[i].Import then
  begin
   Exit(i);
  end;
end;

procedure TElf_node._set_lib(id:Word;lib:TLIBRARY);
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

procedure TElf_node._set_lib_attr(u:TLibraryValue);
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

function TElf_node._get_lib(id:Word):PLIBRARY;
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

function TElf_node._find_lib_export:Word;
var
 i:Word;
begin
 Result:=0;
 if Length(aLibs)>0 then
 For i:=0 to High(aLibs) do
 if (aLibs[i]<>nil) then
  if not aLibs[i]^.Import then
  begin
   Exit(i);
  end;
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

Constructor TElf_node.Create;
begin
 FStatic:=True;
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

function TElf_node.module_start(argc:size_t;argp,param:PPointer):Integer;
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

Function TElf_node.GetModuleInfo:SceKernelModuleInfo;
begin
 Result:=Default(SceKernelModuleInfo);
 Result.size:=SizeOf(SceKernelModuleInfo);

 MoveChar0(PChar(pFileName)^,Result.name,SCE_DBG_MAX_NAME_LENGTH);
end;

Function TElf_node.GetModuleInfoEx:SceKernelModuleInfoEx;
begin
 Result:=Default(SceKernelModuleInfoEx);
 Result.st_size:=SizeOf(SceKernelModuleInfoEx);

 MoveChar0(PChar(pFileName)^,Result.name,SCE_DBG_MAX_NAME_LENGTH);

 Result.id:=FHandle;
 Result.ref_count:=1;
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
 if (@Self=nil) then Exit(False);
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
 if (@Self=nil) then Exit;
 data:=nil;
 PP:=HAMT_search64(MapSymbol,nid);
 if (PP<>nil) then data:=PP^;
 if (data<>nil) then Result:=data^;
end;

function TLIBRARY.set_proc(nid:QWORD;value:Pointer):Boolean;
begin
 if (@Self=nil) then Exit(False);
 if (Fset_proc_cb<>nil) then
  Result:=Fset_proc_cb(@self,nid,value)
 else
  Result:=_set_proc(nid,value);
end;

function TLIBRARY.get_proc(nid:QWORD):Pointer;
begin
 if (@Self=nil) then Exit(nil);
 if (Fget_proc_cb<>nil) then
  Result:=Fget_proc_cb(@self,nid)
 else
  Result:=_get_proc(nid);
end;

function Tps4_program.RegistredFile(node:TElf_node):Boolean;
var
 nid:QWORD;
 PP:PPointer;
begin
 Result:=True;

 files.LockWr;
 files.Remove(node);
 files.Push_tail(node);

 nid:=ps4_nid_hash(node.pFileName);
 PP:=HAMT_insert64(@files.hamt,nid,Pointer(node));
 Assert(PP<>nil);

 if (PP^<>Pointer(node)) then
 begin
  Writeln(StdErr,'Warn, ',node.pFileName,' file is registred');
  Result:=False;
 end;

 files.Unlock;
end;

Procedure Tps4_program.LockRd;
begin
 files.LockRd;
end;

Procedure Tps4_program.LockWr;
begin
 files.LockWr;
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
 if (PP^<>Pointer(node)) then Writeln(StdErr,'Warn, ',strName,' module is registred');
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
 if (PP^<>Pointer(lib)) then Writeln(StdErr,'Warn, ',lib^.strName,' lib is registred');
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

function Tps4_program.RegistredElf(node:TElf_node):Boolean;
var
 FHandle:Integer;
 i:SizeInt;
begin
 if (node=nil) then Exit(False);
 FHandle:=0;

 if not elfs.New(node,FHandle) then
  raise Exception.Create('Error alloc handle');
 node.FHandle:=FHandle;

 Result:=RegistredFile(node);
 if not Result then Exit;

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
  Writeln(StdErr,'Warn, ',strName,' is registred')
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
  Writeln(StdErr,'Warn, ',strName,' is registred')
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

Function sce_load_filter(Const name:RawByteString):Boolean;
const
 c_libc='libc';
 c_libSce='libSce';
begin
 Result:=(Copy(name,1,Length(c_libc))=c_libc) or
         (Copy(name,1,Length(c_libSce))=c_libSce);
end;

function TryLoadElf(Const path,name:RawByteString):TElf_node;
var
 s:RawByteString;
begin
 //bulid path
 s:=IncludeTrailingPathDelimiter(path)+'sce_module'+DirectorySeparator+name;
 Result:=LoadPs4ElfFromFile(s); //try defaut .prx
 if (Result=nil) then
 begin
  s:=ChangeFileExt(s,'.sprx');
  Result:=LoadPs4ElfFromFile(s); //try .sprx
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

 if sce_load_filter(name) then
 begin
  Result:=TryLoadElf(app_path,name);
  if (Result<>nil) then //is default load app_path\sce_module
  begin
   Result.Prepare;
   ps4_app.RegistredElf(Result);
   Exit;
  end;
 end;

 //

 Result:=TryLoadElf(GetCurrentDir,name);
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
   Writeln(StdErr,'Warn, file ',S,' not loaded!');
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

function GetSceProcParam:Pointer;
var
 elf:Telf_file;
begin
 Result:=nil;
 elf:=Telf_file(ps4_program.ps4_app.prog);
 if (elf=nil) then Exit;
 if (elf.pProcParam=0) then Exit;
 Result:=elf.mMap.pAddr+elf.pProcParam;
end;

function GetSceUserMainThreadName:PChar;
var
 p:PSceProcParam;
begin
 Result:=nil;
 p:=GetSceProcParam;
 if (p=nil) then Exit;

 if (P^.Header.Size>=qword(@PSceProcParam(nil)^.sceUserMainThreadName)+SizeOf(Pointer)) then
 begin
  Result:=p^.sceUserMainThreadName;
 end;
end;

function GetSceUserMainThreadPriority:PDWORD;
var
 p:PSceProcParam;
begin
 Result:=nil;
 p:=GetSceProcParam;
 if (p=nil) then Exit;

 if (P^.Header.Size>=qword(@PSceProcParam(nil)^.SceUserMainThreadPriority)+SizeOf(Pointer)) then
 begin
  Result:=p^.SceUserMainThreadPriority;
 end;
end;

function GetSceUserMainThreadStackSize:PDWORD;
var
 p:PSceProcParam;
begin
 Result:=nil;
 p:=GetSceProcParam;
 if (p=nil) then Exit;

 if (P^.Header.Size>=qword(@PSceProcParam(nil)^.SceUserMainThreadStackSize)+SizeOf(Pointer)) then
 begin
  Result:=p^.SceUserMainThreadStackSize;
 end;
end;

function GetSceLibcParam:Pointer;
var
 p:PSceProcParam;
begin
 Result:=nil;
 p:=GetSceProcParam;
 if (p=nil) then Exit;

 if (P^.Header.Size>=qword(@PSceProcParam(nil)^._sceLibcParam)+SizeOf(Pointer)) then
 begin
  Result:=p^._sceLibcParam;
 end;
end;

function GetSceKernelMemParam:Pointer;
var
 p:PSceProcParam;
begin
 Result:=nil;
 p:=GetSceProcParam;
 if (p=nil) then Exit;

 if (P^.Header.Size>=qword(@PSceProcParam(nil)^._sceKernelMemParam)+SizeOf(Pointer)) then
 begin
  Result:=p^._sceKernelMemParam;
 end;
end;

function GetSceKernelFlexibleMemorySize:PQWORD;
var
 p:PSceKernelMemParam;
begin
 Result:=nil;
 p:=GetSceKernelMemParam;
 if (p=nil) then Exit;

 if (P^.Size>=qword(@PSceKernelMemParam(nil)^.sceKernelFlexibleMemorySize)+SizeOf(Pointer)) then
 begin
  Result:=p^.sceKernelFlexibleMemorySize;
 end;
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

end.

