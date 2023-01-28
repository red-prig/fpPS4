unit ps4_libSceComposite;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 mmap,
 ps4_map_mm,
 ps4_libSceGnmDriver,
 ps4_shader,
 sys_crt,
 Classes,
 SysUtils;

implementation

const
 //SceCompositorCommandType
 kSceCompositorCommandLocalStall           =$0;
 kSceCompositorCommandNop                  =$1;
 kSceCompositorCommandGnmContext           =$2;
 kSceCompositorCommandSeparateContext      =$3;
 kSceCompositorCommandCompositeCanvas      =$4;
 kSceCompositorCommandRequestInvalideCanvas=$5;
 kSceCompositorCommandEndOfFrame           =$6;
 kSceCompositorCommandRepeat               =$7;
 kSceCompositorCommandEvent                =$8;
 kSceCompositorCommandMemory               =$9;
 kSceCompositorCommandGameSurfaceControl   =$a;
 kSceCompositorCommandPatch                =$b;
 kSceCompositorCommandDebugPosition        =$c;
 kSceCompositorCommandAcceptQuitRequest    =$d;
 kSceCompositorCommandAcceptInvalidCanvas  =$e;
 //sceCompositorSetPostEventCommand = 0x15

type
 TMemInfo=packed record
  addr  :Pointer;
  offset:QWORD;
  size  :QWORD;
 end;

var
 SceCompositorCmdMtx:TRTLCriticalSection;

 g_System:TMemInfo;
 g_Video :TMemInfo;

 g_index_bits:DWORD=0;

function alloc_dmem(var mem:TMemInfo;len:QWORD):Integer;
begin
 Result:=ps4_sceKernelAllocateMainDirectMemory(len,0,SCE_KERNEL_WB_ONION,@mem.offset);
 if (Result<>0) then Exit;

 Result:=ps4_sceKernelMapDirectMemory(@mem.addr,len,7,0,mem.offset,0);
 if (Result<>0) then Exit;

 mem.size:=len;
end;

function ps4_sceCompositorInitWithProcessOrder(sharedSystemMemorySize:DWORD;
                                               sharedVideoMemorySize :DWORD;
                                               privateVideoMemorySize:DWORD;
                                               processOrder          :DWORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceCompositorInitWithProcessOrder:');
 Writeln(' sharedSystemMemorySize:', sharedSystemMemorySize);
 Writeln(' sharedVideoMemorySize :', sharedVideoMemorySize );
 Writeln(' privateVideoMemorySize:', privateVideoMemorySize);
 Writeln(' processOrder          :', processOrder          );

 InitCriticalSection(SceCompositorCmdMtx);

 Result:=alloc_dmem(g_System,sharedSystemMemorySize);
 if (Result<>0) then Exit;

 Result:=alloc_dmem(g_Video ,sharedVideoMemorySize);
 if (Result<>0) then Exit;

 Result:=0;
end;

function ps4_sceCompositorInit(sharedSystemMemorySize:DWORD;
                               sharedVideoMemorySize :DWORD;
                               privateVideoMemorySize:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceCompositorInitWithProcessOrder(sharedSystemMemorySize,
                                               sharedVideoMemorySize ,
                                               privateVideoMemorySize,
                                               0);
end;

function ps4_sceCompositorGetSystemAddress():Pointer; SysV_ABI_CDecl;
begin
 Result:=g_System.addr;
end;

function ps4_sceCompositorGetVideoAddress():Pointer; SysV_ABI_CDecl;
begin
 Result:=g_Video.addr;
end;

function ps4_sceCompositorGetSystemSize():QWORD; SysV_ABI_CDecl;
begin
 Result:=g_System.size;
end;

function ps4_sceCompositorGetVideoSize():QWORD; SysV_ABI_CDecl;
begin
 Result:=g_Video.size;
end;

function ps4_sceCompositorLockCommandBuffer():Integer; SysV_ABI_CDecl;
begin
 EnterCriticalSection(SceCompositorCmdMtx);
 Result:=0;
end;

function ps4_sceCompositorReleaseCommandBuffer():Integer; SysV_ABI_CDecl;
begin
 LeaveCriticalSection(SceCompositorCmdMtx);
 Result:=0;
end;

function ps4_sceCompositorIsDebugCaptureEnabled():Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceCompositorInsertThreadTraceMarker(cmdBuffer:PDWORD;numDwords:DWORD;param1:DWORD;param2:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=ps4_sceGnmInsertThreadTraceMarker(cmdBuffer,numDwords,param1,param2);
end;

function ps4_sceCompositorAllocateIndex():Integer; SysV_ABI_CDecl;
var
 i,m:Integer;
begin
 Result:=-1;
 EnterCriticalSection(SceCompositorCmdMtx);
 For i:=0 to 15 do
 begin
  m:=(1 shl i);
  if ((g_index_bits and m)=0) then
  begin
   g_index_bits:=g_index_bits or m;
   Result:=i;
   Break;
  end;
 end;
 LeaveCriticalSection(SceCompositorCmdMtx);
end;

procedure ps4_sceCompositorReleaseIndex(id:DWORD); SysV_ABI_CDecl;
begin
 if (id<16) then
 begin
  EnterCriticalSection(SceCompositorCmdMtx);
  g_index_bits:=g_index_bits and (not DWORD(1 shl id));
  LeaveCriticalSection(SceCompositorCmdMtx);
 end;
end;

function ps4_sceCompositorGetCanvasHandle(canvasId:Byte;ret_handle:PDWORD):Integer; SysV_ABI_CDecl;
var
 app_id:Word;
begin
 app_id:=Word(GetProcessID);
 ret_handle^:=app_id or (canvasId shl 16);
 Result:=0;
end;

function ps4_sceCompositorSetDebugPositionCommand(canvasId:Byte;
                                                  x,y,width,height:WORD):Integer; SysV_ABI_CDecl;
begin
 //(SceCompositorCommand)(dbuf * 0x100 | 0xc);
 Writeln('sceCompositorSetDebugPositionCommand:',canvasId);
 Writeln(' x     :',x     );
 Writeln(' y     :',y     );
 Writeln(' width :',width );
 Writeln(' height:',height);
 Result:=0;
end;

//set texture to canvas id
function ps4_sceCompositorSetCompositeCanvasCommandInC(canvasId:Byte;tex:PDWORD):Integer; SysV_ABI_CDecl;
begin
 //(SceCompositorCommand)(dbuf * 0x100 | 0x4);
 Writeln(StdWrn,'sceCompositorSetCompositeCanvasCommandInC:',canvasId);
 print_tsharp8(PTSharpResource8(tex));
 Result:=0;
end;

function ps4_sceCompositorSetGnmContextCommand(
          dcbGpuAddress :Pointer;
          dcbSizeInBytes:DWORD;
          ccbGpuAddress :Pointer;
          ccbSizeInByte :DWORD):Integer; SysV_ABI_CDecl;
begin
 //(SceCompositorCommand)(dbuf * 0x100 | 0x2);
 Writeln(StdWrn,'sceCompositorSetCompositeCanvasCommandInC');
 Result:=0;
end;

function ps4_sceCompositorSetPostEventCommand(event:Byte):Integer; SysV_ABI_CDecl;
begin
 //(SceCompositorCommand)(cmd << 8 | 0x15);
 Writeln(StdWrn,'sceCompositorSetPostEventCommand:',event);
 Result:=0;
end;

function ps4_sceCompositorWaitPostEvent(bit_num:Byte):Integer; SysV_ABI_CDecl;
begin
 //1 << (bit_num & 63)
 Writeln(StdWrn,'sceCompositorWaitPostEvent:',bit_num);
 Result:=0;
end;

function ps4_sceCompositorFlush():Integer; SysV_ABI_CDecl;
begin
 //(SceCompositorCommand)0x0;
 //(SceCompositorCommand)0x1;
 Writeln(StdWrn,'sceCompositorFlush');
 Result:=0;
end;

function ps4_sceCompositorCheckCrash(param:Byte):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function Load_libSceComposite(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceComposite');

 lib^.set_proc($C4891C12974CC6BC,@ps4_sceCompositorInit);
 lib^.set_proc($2149691A7BA84757,@ps4_sceCompositorInitWithProcessOrder);

 lib^.set_proc($4FA09591D0833BBA,@ps4_sceCompositorGetSystemAddress);
 lib^.set_proc($6F1B7E9AEC22B74C,@ps4_sceCompositorGetVideoAddress);
 lib^.set_proc($37A203D0A367CD8F,@ps4_sceCompositorGetSystemSize);
 lib^.set_proc($1534024C35346F88,@ps4_sceCompositorGetVideoSize);

 lib^.set_proc($1A03ABC22FBDBDC0,@ps4_sceCompositorLockCommandBuffer);
 lib^.set_proc($D4E5DBB962D1C6A2,@ps4_sceCompositorReleaseCommandBuffer);

 lib^.set_proc($1B843C28D91BE571,@ps4_sceCompositorAllocateIndex);
 lib^.set_proc($670B01077B3CA8C9,@ps4_sceCompositorReleaseIndex);

 lib^.set_proc($37B3EB33E94F316D,@ps4_sceCompositorGetCanvasHandle);

 lib^.set_proc($ABE430D444B10A3F,@ps4_sceCompositorIsDebugCaptureEnabled);
 lib^.set_proc($FF02001B9F3C9AF8,@ps4_sceCompositorInsertThreadTraceMarker);

 lib^.set_proc($7472BEAAEE43D873,@ps4_sceCompositorSetDebugPositionCommand);
 lib^.set_proc($815A0E137D804E0D,@ps4_sceCompositorSetCompositeCanvasCommandInC);
 lib^.set_proc($0E1B4A7A554021A0,@ps4_sceCompositorSetGnmContextCommand);
 lib^.set_proc($DD0F397B9712DDED,@ps4_sceCompositorSetPostEventCommand);
 lib^.set_proc($75E2A8BDFDEA5620,@ps4_sceCompositorWaitPostEvent);
 lib^.set_proc($A99345D37FA084B2,@ps4_sceCompositorFlush);
 lib^.set_proc($F4CEC791BC14B3F1,@ps4_sceCompositorCheckCrash);
end;

initialization
 ps4_app.RegistredPreLoad('libSceComposite.prx',@Load_libSceComposite);

end.

