unit ps4_libSceComposite;

{$mode ObjFPC}{$H+}

interface

uses
 ps4_program,
 mmap,
 ps4_map_mm,
 Classes,
 SysUtils;

implementation

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

function ps4_sceCompositorSetDebugPositionCommand(canvasId:Byte;
                                                  x,y,width,height:WORD):Integer; SysV_ABI_CDecl;
begin
 Writeln('sceCompositorSetDebugPositionCommand:');
 Writeln(' x     :',x     );
 Writeln(' y     :',y     );
 Writeln(' width :',width );
 Writeln(' height:',height);

 Result:=0;
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

function Load_libSceComposite(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;

 lib:=Result._add_lib('libSceComposite');

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

 lib^.set_proc($7472BEAAEE43D873,@ps4_sceCompositorSetDebugPositionCommand);
end;

initialization
 ps4_app.RegistredPreLoad('libSceComposite.prx',@Load_libSceComposite);

end.

