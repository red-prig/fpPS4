unit seh64;

{$mode objfpc}{$H+}

interface

implementation

uses
  Windows,
  SysConst,
  SysUtils,
  hamt,
  ps4libdoc,
  ps4_types,
  ps4_program;

function AddVectoredExceptionHandler(FirstHandler: DWORD; VectoredHandler: pointer): pointer; stdcall;
  external 'kernel32.dll' name 'AddVectoredExceptionHandler';
function RemoveVectoredExceptionHandler(VectoredHandlerHandle: pointer): ULONG; stdcall;
  external 'kernel32.dll' name 'RemoveVectoredExceptionHandler';  
function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: pointer; var hModule: THandle): BOOL; stdcall;
  external 'kernel32.dll' name 'GetModuleHandleExA';

// sysutils.GetModuleName();

const
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = 2;
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS       = 4;

function GetModuleByAdr(adr:Pointer):THandle;
var
 Flags:DWORD;
begin
 Flags:=GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS or GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT;
 Result:=0;
 GetModuleHandleEx(Flags,adr,Result);
end;

function RunErrorCode(const rec: TExceptionRecord): longint;
begin
  { negative result means 'FPU reset required' }
  case rec.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:      result := 200;    { reDivByZero }
    STATUS_FLOAT_DIVIDE_BY_ZERO:        result := -208;   { !!reZeroDivide }
    STATUS_ARRAY_BOUNDS_EXCEEDED:       result := 201;    { reRangeError }
    STATUS_STACK_OVERFLOW:              result := 202;    { reStackOverflow }
    STATUS_FLOAT_OVERFLOW:              result := -205;   { reOverflow }
    STATUS_FLOAT_DENORMAL_OPERAND,
    STATUS_FLOAT_UNDERFLOW:             result := -206;   { reUnderflow }
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:           result := -207;   { reInvalidOp }
    STATUS_INTEGER_OVERFLOW:            result := 215;    { reIntOverflow }
    STATUS_ILLEGAL_INSTRUCTION:         result := -216;
    STATUS_ACCESS_VIOLATION:            result := 216;    { reAccessViolation }
    STATUS_CONTROL_C_EXIT:              result := 217;    { reControlBreak }
    STATUS_PRIVILEGED_INSTRUCTION:      result := 218;    { rePrivilegedInstruction }
    STATUS_FLOAT_MULTIPLE_TRAPS,
    STATUS_FLOAT_MULTIPLE_FAULTS:       result := -255;   { indicate FPU reset }
  else
    result := 255;                                        { reExternalException }
  end;
end;

procedure TranslateMxcsr(mxcsr: longword; var code: longint);
begin
  { we can return only one value, further one's are lost }
  { InvalidOp }
  if (mxcsr and 1)<>0 then
    code:=-207
  { Denormal }
  else if (mxcsr and 2)<>0 then
    code:=-206
  { !!reZeroDivide }
  else if (mxcsr and 4)<>0 then
    code:=-208
  { reOverflow }
  else if (mxcsr and 8)<>0 then
    code:=-205
  { Underflow }
  else if (mxcsr and 16)<>0 then
    code:=-206
  { Precision }
  else if (mxcsr and 32)<>0 then
    code:=-207
  else { this should not happen }
    code:=-255
end;


function RunErrorCodex64(const rec: TExceptionRecord; const context: TContext): Longint;
begin
 Result:=RunErrorCode(rec);
 if (Result=-255) then
   TranslateMxcsr(context.MxCsr,result);
end;

type
 _TElf_node=class(TElf_node)
 end;

 PTLQRec=^TLQRec;
 TLQRec=record
  pAddr:Pointer;
  ExceptAddr:Pointer;
  LastAdr:Pointer;
  LastNid:QWORD;
 end;

procedure trav_proc(data,userdata:Pointer);
var
 adr:Pointer;
 nid:QWORD;
begin
 if (data=nil) then Exit;
 safe_move_ptr(PPointer(data)[0],adr);
 safe_move_ptr(PPointer(data)[1],nid);
 if (adr>=PTLQRec(userdata)^.pAddr) then
 if (adr<=PTLQRec(userdata)^.ExceptAddr) then
 if (adr>PTLQRec(userdata)^.LastAdr) then
 begin
  PTLQRec(userdata)^.LastAdr:=adr;
  PTLQRec(userdata)^.LastNid:=nid;
 end;
end;

function IsSubTrie64(n:PHAMTNode64):Boolean; inline;
var
 BaseValue:PtrUint;
begin
 safe_move_ptr(n^.BaseValue,BaseValue);
 Result:=(BaseValue and 1)<>0;
end;

function GetBitMapSize64(n:PHAMTNode64):QWORD; inline;
var
 BitMapKey:QWORD;
begin
 safe_move_ptr(n^.BitMapKey,BitMapKey);
 Result:=PopCnt(BitMapKey);
 Result:=Result and HAMT64.node_mask;
 if (Result=0) then Result:=HAMT64.node_size;
end;

function GetSubTrie64(n:PHAMTNode64):PHAMTNode64; inline;
var
 BaseValue:PtrUint;
begin
 safe_move_ptr(n^.BaseValue,BaseValue);
 PtrUint(Result):=(BaseValue or 1) xor 1;
end;

function GetValue64(n:PHAMTNode64):Pointer; inline;
begin
 safe_move_ptr(n^.BaseValue,Result);
end;

procedure HAMT_traverse_trie64(node:PHAMTNode64;cb:Tfree_data_cb;userdata:Pointer); inline;
type
 PStackNode=^TStackNode;
 TStackNode=packed record
  cnode,enode:PHAMTNode64;
 end;
var
 curr:PStackNode;
 data:array[0..HAMT64.stack_max] of TStackNode;
 Size:QWORD;
begin
 if IsSubTrie64(node) then
 begin
  curr:=@data;
  Size:=GetBitMapSize64(node);
  With curr^ do
  begin
   cnode:=GetSubTrie64(node);
   enode:=@cnode[Size];
  end;
  repeat
   if (curr^.cnode>=curr^.enode) then
   begin
    if (curr=@data) then Break;
    Dec(curr);
    Inc(curr^.cnode);
    Continue;
   end;
   if IsSubTrie64(curr^.cnode) then
   begin
    node:=curr^.cnode;
    Inc(curr);
    Size:=GetBitMapSize64(node);
    With curr^ do
    begin
     cnode:=GetSubTrie64(node);
     enode:=@cnode[Size];
    end;
   end else
   begin
    if (cb<>nil) then
     cb(GetValue64(curr^.cnode),userdata);
    Inc(curr^.cnode);
   end;
  until false;
 end else
 begin
  if (cb<>nil) then
   cb(GetValue64(node),userdata);
 end;
end;

function HAMT_traverse64(hamt:THAMT;cb:Tfree_data_cb;userdata:Pointer):Boolean;
var
 i:Integer;
 node:PHAMTNode64;
begin
 if (hamt=nil) then Exit(False);
 For i:=0 to HAMT64.root_mask do
 begin
  node:=@PHAMTNode64(hamt)[i];
  HAMT_traverse_trie64(node,cb,userdata);
 end;
 Result:=True;
end;

Function FindLQProc(node:TElf_node;r:PTLQRec):Boolean;
var
 i,l:SizeInt;
 lib:PLIBRARY;
 MapSymbol:THAMT;
 Import:Boolean;
begin
 Result:=false;
 l:=Length(_TElf_node(node).aLibs);
 if (l<>0) then
 begin
  r^.LastAdr:=nil;
  r^.LastNid:=0;
  For i:=0 to l-1 do
  begin
   safe_move_ptr(_TElf_node(node).aLibs[i],lib);
   if (lib<>nil) then
   begin
    Import:=True;
    safe_move(lib^.Import,Import,SizeOf(Boolean));
    if not Import then
    begin
     safe_move_ptr(lib^.MapSymbol,MapSymbol);
     HAMT_traverse64(MapSymbol,@trav_proc,r);
    end;
   end;
  end;
  Result:=(r^.LastAdr<>nil);
 end;
end;

Procedure WriteErr(Const s:shortstring);
var
 num:DWORD;
begin
 WriteConsole(GetStdHandle(STD_ERROR_HANDLE),@s[1],ord(s[0]),num,nil);
end;

function IntToStr(Value:longint): shortstring;
begin
 System.Str(Value,result);
end;

function GetModuleName(Module:HMODULE): shortstring;
var
 Len:DWORD;
 Buffer:array[0..MAX_PATH] of WideChar;
 P:PWideChar;
begin
 Len:=GetModuleFileNameW(Module,@Buffer,MAX_PATH);
 P:=@Buffer[Len];
 While (P<>@Buffer) do
 begin
  if (P^='\') then
  begin
   Inc(P);
   Break;
  end;
  Dec(P);
 end;
 Len:=@Buffer[Len]-P;
 Len:=UnicodeToUtf8(@Result[1],255,P,Len);
 Byte(Result[0]):=Len;
end;

Procedure DumpException(node:TElf_node;code:Longint;ExceptAddr:Pointer;ContextRecord:PCONTEXT);
var
 Report:shortstring;
 pFileName:PChar;
 Mem:TMemChunk;
 top,rbp:PPointer;

 procedure print_adr;
 var
  r:TLQRec;
 begin
  Report:='  $'+hexstr(ExceptAddr);
  if (node<>nil) then
  begin
   Mem:=node.GetCodeFrame;
   if (Mem.pAddr<>nil) and (Mem.nSize<>0) then
   begin
    safe_move_ptr(node.pFileName,pFileName);
    Report:=Report+' offset $'+hexstr(ExceptAddr-Mem.pAddr,8)+' '+safe_str(pFileName);
    r.pAddr:=Mem.pAddr;
    r.ExceptAddr:=ExceptAddr;
    if FindLQProc(node,@r) then
    begin
     Report:=Report+':'+ps4libdoc.GetFunctName(r.LastNid)+'+$'+hexstr(ExceptAddr-r.LastAdr,8);
    end else
    if (node.GetEntryPoint<>nil) then
    begin
     Report:=Report+':EntryPoint+$'+hexstr(ExceptAddr-node.GetEntryPoint,8);
    end;
   end;
  end;
  Report:=Report+#13#10;
  WriteErr(Report);
 end;

 procedure print_adr2;
 begin
  Report:='  $'+hexstr(ExceptAddr);
  Report:=Report+' '+GetModuleName(GetModuleByAdr(ExceptAddr));
  Report:=Report+#13#10;
  WriteErr(Report);
 end;

begin
 Report:='';
 Report:=Report+'Message: '+SysConst.GetRunError(abs(code));
 Report:=Report+' ('+IntToStr(longint(code))+')';
 Report:=Report+#13#10;
 WriteErr(Report);
 print_adr;
 top:=Pointer(ContextRecord^.Rbp);
 //if (top>StackBottom) and (top<StackTop) then
 begin
  rbp:=top;
  repeat
   safe_move_ptr(rbp[1],ExceptAddr);
   safe_move_ptr(rbp[0],rbp);
   if (ExceptAddr<>nil) then
   begin
    node:=ps4_app.FindFileByCodeAdr(ExceptAddr);
    if (node<>nil) then
    begin
     print_adr;
    end else
    begin
     print_adr2;
    end;
   end;
  until (node=nil) {or (rbp>top) or (rbp<StackBottom)};
 end;
end;

const
 FPC_EXCEPTION_CODE=$E0465043;

function ProcessException(p: PExceptionPointers): longint; stdcall;
var
 code: Longint;
 node:TElf_node;
begin
 Result := 0;

 if (p^.ExceptionRecord^.ExceptionCode=FPC_EXCEPTION_CODE) then Exit(EXCEPTION_CONTINUE_SEARCH);

 node:=ps4_app.FindFileByCodeAdr(p^.ExceptionRecord^.ExceptionAddress);
 if (node=nil) and
    (GetModuleByAdr(p^.ExceptionRecord^.ExceptionAddress)<>GetModuleByAdr(@ProcessException)) then
    Exit(EXCEPTION_CONTINUE_SEARCH);

 code:=RunErrorCodex64(p^.ExceptionRecord^,p^.ContextRecord^);
 DumpException(node,code,p^.ExceptionRecord^.ExceptionAddress,P^.ContextRecord);
 halt;
end;

var
  VEHandler: pointer = Nil;

procedure InstallExceptionHandler;
begin
  VEHandler := AddVectoredExceptionHandler(1, @ProcessException);
end;

procedure UninstallExceptionHandler;
begin
  if Assigned(VEHandler) then
  begin
    RemoveVectoredExceptionHandler(VEHandler);
    VEHandler := Nil;
  end;
end;

initialization
  InstallExceptionHandler;

finalization
  UninstallExceptionHandler;
end.
