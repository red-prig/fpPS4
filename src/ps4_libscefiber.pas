unit ps4_libSceFiber;

{$mode ObjFPC}{$H+}

interface

uses
 Windows,
 SysUtils,
 ps4_program,
 sys_kernel;

const
 SCE_FIBER_ERROR_NULL      =$80590001;
 SCE_FIBER_ERROR_RANGE     =$80590003;
 SCE_FIBER_ERROR_INVALID   =$80590004;
 SCE_FIBER_ERROR_PERMISSION=$80590005;
 SCE_FIBER_ERROR_STATE     =$80590006;

 SCE_FIBER_CONTEXT_MINIMUM_SIZE=512;
 SCE_FIBER_MAX_NAME_LENGTH     =31;

 FIBER_STATE_INIT   =0;
 FIBER_STATE_RUN    =1;
 FIBER_STATE_SUSPEND=2;

type
 SceFiberEntry=procedure(argInit,argRun:QWord); SysV_ABI_CDecl;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceFiber=packed record
  handle           :Pointer;               // 8 - Pointer to Windows's fiber
  name             :array[0..SCE_FIBER_MAX_NAME_LENGTH] of Char; // 40
  entry            :SceFiberEntry;         // 48
  argInit          :QWord;                 // 56
  pArgRun          :PQWord;                // 64
  pArgReturn       :PQWord;                // 72
  argRun           :QWord;                 // 80
  argReturn        :QWord;                 // 88
  state            :QWord;                 // 96 - 0 = init, 1 = running, 2 = suspend
  addrContext      :Pointer;               // 104
  sizeContext      :QWord;                 // 112
  sizeContextMargin:QWord;                 // 120
  _unknown         :array[0..255-120] of Byte; // 256
 end;
 PSceFiber =^SceFiber;
 PPSceFiber=^PSceFiber;

 // While we keep the size correct, the content is not the same as the one in original lib
 SceFiberOptParam=packed record
  _unknown:array[0..127] of Byte;
 end;
 PSceFiberOptParam=^SceFiberOptParam;

 SceFiberInfo=packed record
  size             :QWord;                // 8
  entry            :SceFiberEntry;        // 16
  argInit          :QWord;                // 24
  addrContext      :Pointer;              // 32
  sizeContext      :QWord;                // 40
  name             :array[0..SCE_FIBER_MAX_NAME_LENGTH] of Char; // 72
  sizeContextMargin:QWord;                // 80
  _unkown          :array[0..127-80] of Byte; // 128
 end;
 PSceFiberInfo=^SceFiberInfo;

function GetFiberString:RawByteString;
function GetFiberStringParam(fiber:PSceFiber):RawByteString;
function ps4_sceFiberInitialize(fiber      :PSceFiber;
                                name       :PChar;
                                entry      :SceFiberEntry;
                                argInit    :QWord;
                                addrContext:Pointer;
                                sizeContext:QWord;
                                option     :PSceFiberOptParam):Integer; SysV_ABI_CDecl;
function ps4_sceFiberFinalize(fiber:PSceFiber):Integer; SysV_ABI_CDecl;
function ps4_sceFiberSwitch(fiber:PSceFiber;argRunTo:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;
function ps4_sceFiberRun(fiber:PSceFiber;argRun:QWord;argReturn:PQWord):Integer; SysV_ABI_CDecl;
function ps4_sceFiberReturnToThread(argReturn:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;

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

procedure _CheckFail(P:Pointer);
begin
 if P=nil then
  raise Exception.Create(Format('Error with code %d', [GetLastError]));
end;

function GetFiberStringParam(fiber:PSceFiber):RawByteString;
begin
 Result:='F:['+PChar(@fiber^.name[0])+':'+IntToStr(QWord(fiber^.handle))+'] ';
end;

function GetFiberString:RawByteString;
begin
 if _currentFiber<>nil then
  Result:=GetFiberStringParam(_currentFiber);
end;

procedure _CreateThreadFiber;
begin
 // Create a main fiber
 if _threadFiber=nil then
 begin
  _threadFiber:=ConvertThreadToFiber(nil);
  _CheckFail(_threadFiber);
 end;
end;

procedure _FiberEntry(fiber:PSceFiber);
var
 argRun   :QWord=0;
 argReturn:QWord=0;
begin
 _currentFiber:=fiber;
 Writeln(SysLogPrefix,'_FiberEntry Start');
 if fiber^.pArgRun<>nil then
  argRun:=fiber^.pArgRun^;
 fiber^.entry(fiber^.argInit,argRun);
 Writeln(SysLogPrefix,'_FiberEntry End');
 if fiber^.pArgReturn<>nil then
  argReturn:=fiber^.pArgReturn^;
 ps4_sceFiberReturnToThread(fiber^.argReturn,fiber^.pArgRun);
end;

function _CreateFiber(fiber      :PSceFiber;
                      name       :PChar;
                      entry      :SceFiberEntry;
                      argInit    :QWord;
                      addrContext:Pointer;
                      sizeContext:QWord):Integer;
begin
 fiber^.argInit          :=argInit;
 fiber^.argRun           :=0;
 fiber^.argReturn        :=0;
 fiber^.pArgRun          :=@fiber^.argRun;
 fiber^.pArgReturn       :=@fiber^.argReturn;
 fiber^.entry            :=entry;
 fiber^.state            :=FIBER_STATE_INIT;
 fiber^.addrContext      :=addrContext;
 fiber^.sizeContext      :=sizeContext;
 fiber^.sizeContextMargin:=sizeContext;
 fiber^.handle           :=CreateFiber(sizeContext,@_FiberEntry,fiber);
 StrLCopy(@fiber^.name[0],name,SCE_FIBER_MAX_NAME_LENGTH);
 Result:=0;
end;

function _RunFiber(fiber:PSceFiber;argRun:QWord;argReturn:PQWord):Integer;
begin
 if _currentFiber<>nil then
  Exit(SCE_FIBER_ERROR_PERMISSION);
 _CreateThreadFiber;
 _currentFiber    :=fiber;
 if fiber^.pArgRun<>nil then
  fiber^.pArgRun^ :=argRun;
 fiber^.pArgReturn:=argReturn;
 fiber^.state     :=FIBER_STATE_RUN;
 SwitchToFiber(fiber^.handle);
 Result:=0;
end;

function _DeleteFiber(fiber:PSceFiber):Integer;
begin
 if (fiber^.state=FIBER_STATE_RUN) or (fiber^.state=FIBER_STATE_SUSPEND) then
  DeleteFiber(fiber^.handle);
 Result:=0;
end;

function _ReCreateFiber(fiber:PSceFiber):Integer;
begin
 _DeleteFiber(fiber);
 fiber^.handle:=CreateFiber(fiber^.sizeContext,@_FiberEntry,fiber);
 fiber^.state :=FIBER_STATE_INIT;
 Result:=0;
end;

function _SwitchFiber(fiber:PSceFiber;argRunTo:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;
begin
 if _currentFiber^.addrContext<>nil then
 begin
  _currentFiber^.state:=FIBER_STATE_SUSPEND;
 end else
 begin
  _ReCreateFiber(_currentFiber);
 end;
 if _currentFiber^.pArgRun<>nil then
  _currentFiber^.pArgRun^:=argRunTo;
 fiber^.pArgRun  :=argRun;
 fiber^.state    :=FIBER_STATE_RUN;
 _currentFiber   :=fiber;
 SwitchToFiber(fiber^.handle);
 Result:=0;
end;

// APIs

function ps4_sceFiberSwitch(fiber:PSceFiber;argRunTo:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;
begin
 if _currentFiber=nil then
  Exit(SCE_FIBER_ERROR_PERMISSION);
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 Writeln(SysLogPrefix,'sceFiberSwitch,to=',GetFiberStringParam(fiber));
 Result:=_SwitchFiber(fiber,argRunTo,argRun);
end;

function ps4_sceFiberInitialize(fiber      :PSceFiber;
                                name       :PChar;
                                entry      :SceFiberEntry;
                                argInit    :QWord;
                                addrContext:Pointer;
                                sizeContext:QWord;
                                option     :PSceFiberOptParam):Integer; SysV_ABI_CDecl;
begin
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 if (sizeContext mod 16<>0) or
    ((addrContext=nil) and (sizeContext>0)) or
    ((addrContext<>nil) and (sizeContext=0)) then
  Exit(SCE_FIBER_ERROR_INVALID);
 if sizeContext<SCE_FIBER_CONTEXT_MINIMUM_SIZE then
  Exit(SCE_FIBER_ERROR_RANGE);
 Result:=_CreateFiber(fiber,name,entry,argInit,addrContext,sizeContext);
 Writeln(SysLogPrefix,'sceFiberInitialize,fiber=',GetFiberStringParam(fiber),',sizeContext=',sizeContext,',argInit=',argInit);
end;

function ps4_sceFiberFinalize(fiber:PSceFiber):Integer; SysV_ABI_CDecl;
begin
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 if fiber=_currentFiber then
  Exit(SCE_FIBER_ERROR_STATE);
 Writeln(SysLogPrefix,'sceFiberFinalize,fiber=',GetFiberStringParam(fiber));
 Result:=_DeleteFiber(fiber);
end;

function ps4_sceFiberRun(fiber:PSceFiber;argRun:QWord;argReturn:PQWord):Integer; SysV_ABI_CDecl;
begin
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 if fiber^.state=1 then
  Exit(SCE_FIBER_ERROR_STATE);
 Writeln(SysLogPrefix,'sceFiberRun,fiber=',GetFiberStringParam(fiber));
 fiber^.state:=1;
 Result:=_RunFiber(fiber,argRun,argReturn);
end;

function ps4_sceFiberReturnToThread(argReturn:QWord;argRun:PQWord):Integer; SysV_ABI_CDecl;
begin
 if _currentFiber=nil then
  Exit(SCE_FIBER_ERROR_PERMISSION);
 Writeln(SysLogPrefix,'sceFiberReturnToThread');
 if _currentFiber^.addrContext<>nil then
 begin
  _currentFiber^.state:=FIBER_STATE_SUSPEND;
 end else
 begin
  _ReCreateFiber(_currentFiber); // TODO: Proper reset fiber
 end;
 _currentFiber^.pArgRun     :=argRun;
 if _currentFiber^.pArgReturn<>nil then
  _currentFiber^.pArgReturn^:=argReturn;
 _currentFiber:=nil;
 SwitchToFiber(_threadFiber);
 Result:=0;
end;

function ps4_sceFiberOptParamInitialize(param:PSceFiberOptParam):Integer; SysV_ABI_CDecl;
begin
 if param=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 Writeln(SysLogPrefix,'sceFiberOptParamInitialize');
 Result:=0;
end;

function ps4_sceFiberStartContextSizeCheck(flags:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceFiberStartContextSizeCheck');
 Result:=0;
end;

function ps4_sceFiberStopContextSizeCheck(flags:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceFiberStopContextSizeCheck');
 Result:=0;
end;

function ps4_sceFiberRename(fiber:PSceFiber;name:PChar):Integer; SysV_ABI_CDecl;
begin
 //Writeln(SysLogPrefix,'sceFiberRename,fiber=',GetFiberStringParam(fiber),',newName=',name);
 if fiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 StrLCopy(@fiber^.name[0],name,SCE_FIBER_MAX_NAME_LENGTH);
 Result:=0;
end;

function ps4_sceFiberGetSelf(pfiber:PPSceFiber):Integer; SysV_ABI_CDecl;
begin
 if _currentFiber=nil then
  Exit(SCE_FIBER_ERROR_PERMISSION);
 if pfiber=nil then
  Exit(SCE_FIBER_ERROR_NULL);
 //Writeln(SysLogPrefix,'sceFiberGetSelf');
 pfiber^:=_currentFiber;
 Result:=0;
end;

function ps4_sceFiberGetInfo(fiber:PSceFiber;fiberInfo:PSceFiberInfo):Integer; SysV_ABI_CDecl;
begin
 if (fiber=nil) or (fiberInfo=nil) then
  Exit(SCE_FIBER_ERROR_NULL);
 fiberInfo^.size             :=128;
 fiberInfo^.entry            :=fiber^.entry;
 fiberInfo^.argInit          :=fiber^.argInit;
 fiberInfo^.addrContext      :=fiber^.addrContext;
 fiberInfo^.sizeContext      :=fiber^.sizeContext;
 fiberInfo^.sizeContextMargin:=fiber^.sizeContextMargin;
 Result:=0;
end;

function Load_libSceFiber(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceFiber');

 lib^.set_proc($855603ECEBB6A424,@ps4_sceFiberInitialize);
 lib^.set_proc($25E357E45FCDCD05,@ps4_sceFiberFinalize);
 lib^.set_proc($6B42CBAD959A7343,@ps4_sceFiberRun);
 lib^.set_proc($3C54F64BFB49ED49,@ps4_sceFiberSwitch);
 lib^.set_proc($074657DA1C7D0CCC,@ps4_sceFiberReturnToThread);
 lib^.set_proc($6AC8D4249F9A6BCB,@ps4_sceFiberOptParamInitialize);
 lib^.set_proc($2DCAADCBE40D5857,@ps4_sceFiberStartContextSizeCheck);
 lib^.set_proc($2A3E275CCA6733C6,@ps4_sceFiberStopContextSizeCheck);
 lib^.set_proc($273C93F75B9C1837,@ps4_sceFiberRename);
 lib^.set_proc($A7ECCB20E836EF35,@ps4_sceFiberGetSelf);
 lib^.set_proc($BAAD98E41173D0F1,@ps4_sceFiberGetInfo);
end;

initialization
 ps4_app.RegistredPreLoad('libSceFiber.prx',@Load_libSceFiber);

end.
