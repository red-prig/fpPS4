unit ps4_libSceFiber;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 SysUtils,
 ps4_program;

const
 SCE_FIBER_ERROR_NULL       = $80590001;
 SCE_FIBER_ERROR_RANGE      = $80590003;
 SCE_FIBER_ERROR_INVALID    = $80590004;
 SCE_FIBER_ERROR_PERMISSION = $80590004;
 SCE_FIBER_ERROR_STATE      = $80590006;

 SCE_FIBER_CONTEXT_MINIMUM_SIZE = 512;
 SCE_FIBER_MAX_NAME_LENGTH      = 31;

type
 SceFiberEntry=procedure(argOnInitialize,argOnRun:QWord); SysV_ABI_CDecl;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceFiber=packed record
  handle    :Pointer;               // 8 - Pointer to Windows's fiber
  name      :array[0..SCE_FIBER_MAX_NAME_LENGTH] of Char; // 40
  entry     :SceFiberEntry;         // 48
  argInit   :QWord;                 // 56
  pArgRun   :PQWord;                // 64
  pArgReturn:PQWord;                // 72
  argRun    :QWord;                 // 80
  argReturn :QWord;                 // 88
  state     :QWord;                 // 80 - 0 = init, 1 = running
  _unknown :array[0..159] of Byte; // 256
 end;
 PSceFiber=^SceFiber;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceFiberOptParam=packed record
  _unknown:array[0..127] of Byte;
 end;
 PSceFiberOptParam=^SceFiberOptParam;

 SceFiberInfo=packed record
  size             :QWord;                // 8
  entry            :SceFiberEntry;        // 16
  argOnInitialize  :QWord;                // 24
  addrContext      :Pointer;              // 32
  sizeContext      :QWord;                // 40
  name             :array[0..SCE_FIBER_MAX_NAME_LENGTH] of Char; // 72
  sizeContextMargin:QWord;                // 80
  _unkown          :array[0..47] of Byte; // 128
 end;

function ps4_sceFiberSwitch(fiber:PSceFiber;argOnRunTo:QWord;argOnRun:PQWord):Integer; SysV_ABI_CDecl;
function ps4_sceFiberFinalize(fiber:PSceFiber):Integer; SysV_ABI_CDecl;
function ps4_sceFiberRun(fiber:PSceFiber;argRun:QWord;argReturn:PQWord):Integer; SysV_ABI_CDecl;
function ps4_sceFiberReturnToThread(fiber:PSceFiber;argReturn:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;

implementation

threadvar
 _threadFiber :Pointer;   // TODO: Memory leak if thread is destroyed
 _currentFiber:PSceFiber;

// Imports

function ConvertThreadToFiber(lpParameter:LPVOID):LPVOID; external 'kernel32' name 'ConvertThreadToFiber';
function CreateFiber(dwStackSize:size_t;lpStartAddress:LPVOID;lpParameter:LPVOID):LPVOID; external 'kernel32' name 'CreateFiber';
procedure SwitchToFiber(lpFiber:LPVOID); external 'kernel32' name 'SwitchToFiber';
procedure DeleteFiber(lpFiber:LPVOID); external 'kernel32' name 'DeleteFiber';

// Wrappers

procedure _FiberEntry(fiber:PSceFiber);
begin
 _currentFiber:=fiber;
 Writeln('_FiberEntry,name=',PChar(@fiber^.name[0]));
 fiber^.entry(fiber^.argInit,fiber^.argRun);
 ps4_sceFiberReturnToThread(fiber,fiber^.pArgReturn^,fiber^.pArgRun);
end;

function _CreateFiber(fiber      :PSceFiber;
                      name       :PChar;
                      entry      :SceFiberEntry;
                      argInit    :QWord;
                      addrContext:Pointer;
                      sizeContext:QWord):Integer;
begin
 fiber^.argInit   :=argInit;
 fiber^.pArgRun   :=@fiber^.argRun;
 fiber^.pArgReturn:=@fiber^.argReturn;
 fiber^.entry     :=entry;
 fiber^.state     :=0;
 StrLCopy(@fiber^.name[0],name,SCE_FIBER_MAX_NAME_LENGTH);
 fiber^.handle :=CreateFiber(0,@_FiberEntry,fiber);
 Result:=0;
end;

function _RunFiber(fiber:PSceFiber;argRun:QWord;argReturn:PQWord):Integer;
begin
 if _currentFiber<>nil then
  Exit(SCE_FIBER_ERROR_PERMISSION);
 fiber^.pArgRun^  :=argRun;
 fiber^.pArgReturn:=argReturn;
 SwitchToFiber(fiber^.handle);
 Result:=0;
end;

function _DeleteFiber(fiber:PSceFiber):Integer;
begin
 DeleteFiber(fiber^.handle);
 Result:=0;
end;

// APIs

function ps4_sceFiberSwitch(fiber:PSceFiber;argOnRunTo:QWord;argOnRun:PQWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberSwitch');
 Result:=0;
end;

function ps4_sceFiberInitialize(fiber      :PSceFiber;
                                name       :PChar;
                                entry      :SceFiberEntry;
                                argInit    :QWord;
                                addrContext:Pointer;
                                sizeContext:QWord;
                                option     :PSceFiberOptParam):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberFinalize,name=',name,',sizeContext=',sizeContext,',argInit=',argInit);
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 if (sizeContext mod 16 <> 0) or
    ((addrContext=nil) and (sizeContext>0)) or
    ((addrContext<>nil) and (sizeContext=0)) then
  Exit(SCE_FIBER_ERROR_INVALID);
 if sizeContext<SCE_FIBER_CONTEXT_MINIMUM_SIZE then
  Exit(SCE_FIBER_ERROR_RANGE);
 // Create a main fiber
 if _threadFiber=nil then
 begin
  _threadFiber:=ConvertThreadToFiber(nil);
 end;
 Result:=_CreateFiber(fiber,name,entry,argInit,addrContext,sizeContext);
end;

function ps4_sceFiberFinalize(fiber:PSceFiber):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberFinalize');
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 if fiber=_currentFiber then
  Exit(SCE_FIBER_ERROR_STATE);
 Result:=_DeleteFiber(fiber);
end;

function ps4_sceFiberRun(fiber:PSceFiber;argRun:QWord;argReturn:PQWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberRun');
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 if fiber^.state=1 then
  Exit(SCE_FIBER_ERROR_STATE);
 Result:=_RunFiber(fiber,argRun,argReturn);
end;

function ps4_sceFiberReturnToThread(fiber:PSceFiber;argReturn:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;
begin
 if _currentFiber=nil then
  Exit(SCE_FIBER_ERROR_PERMISSION);
 _currentFiber:=nil;
 fiber^.pArgRun    :=argRun;
 fiber^.pArgReturn^:=argReturn;
 SwitchToFiber(_threadFiber);
 Result:=0;
end;

function ps4_sceFiberOptParamInitialize(param:PSceFiberOptParam):Integer; SysV_ABI_CDecl;
begin
 if param=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 Writeln('sceFiberOptParamInitialize');
 Result:=0;
end;

function ps4_sceFiberStartContextSizeCheck(flags:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberStartContextSizeCheck');
 Result:=0;
end;

function ps4_sceFiberStopContextSizeCheck(flags:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberStopContextSizeCheck');
 Result:=0;
end;

function ps4_sceFiberRename(fiber:PSceFiber;name:PChar):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceFiberRename');
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 StrLCopy(@fiber^.name[0],name,SCE_FIBER_MAX_NAME_LENGTH);
 Result:=0;
end;

function Load_libSceFiber(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceFiber');

 lib^.set_proc($25E357E45FCDCD05,@ps4_sceFiberFinalize);
 lib^.set_proc($6B42CBAD959A7343,@ps4_sceFiberRun);
 lib^.set_proc($3C54F64BFB49ED49,@ps4_sceFiberSwitch);
 lib^.set_proc($074657DA1C7D0CCC,@ps4_sceFiberReturnToThread);
 lib^.set_proc($6AC8D4249F9A6BCB,@ps4_sceFiberOptParamInitialize);
 lib^.set_proc($2DCAADCBE40D5857,@ps4_sceFiberStartContextSizeCheck);
 lib^.set_proc($2A3E275CCA6733C6,@ps4_sceFiberStopContextSizeCheck);
 lib^.set_proc($273C93F75B9C1837,@ps4_sceFiberRename);
end;

initialization
 ps4_app.RegistredPreLoad('Load_libSceFiber.prx',@Load_libSceFiber);

end.

