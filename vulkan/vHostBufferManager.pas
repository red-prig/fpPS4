unit vHostBufferManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 RWLock,
 sys_types,
 g23tree,
 Vulkan,
 vDevice,
 vMemory,
 vBuffer,
 vCmdBuffer;

type
 TvHostBuffer=class(TvBuffer)
  FAddr:Pointer;
  Fhost:TvPointer;
  Foffset:TVkDeviceSize; //offset inside buffer
  //
  FRefs:ptruint;
  Procedure Acquire(Sender:TObject);
  procedure Release(Sender:TObject);
 end;

function FetchHostBuffer(cmd:TvCustomCmdBuffer;Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TvHostBuffer;

implementation

const
 buf_ext:TVkExternalMemoryBufferCreateInfo=(
  sType:VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
  pNext:nil;
  handleTypes:ord(VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
 );

type
 TvAddrCompare=object
  function c(a,b:PPointer):Integer; static;
 end;

 _TvHostBufferSet=specialize T23treeSet<PPointer,TvAddrCompare>;
 TvHostBufferSet=object(_TvHostBufferSet)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

var
 FHostBufferSet:TvHostBufferSet;

Procedure TvHostBufferSet.Init;
begin
 rwlock_init(lock);
end;

Procedure TvHostBufferSet.Lock_wr;
begin
 rwlock_wrlock(lock);
end;

Procedure TvHostBufferSet.Unlock;
begin
 rwlock_unlock(lock);
end;

function TvAddrCompare.c(a,b:PPointer):Integer;
begin
 Result:=Integer(a^>b^)-Integer(a^<b^);
end;

function _Find(Addr:Pointer):TvHostBuffer;
var
 i:TvHostBufferSet.Iterator;
begin
 Result:=nil;
 i:=FHostBufferSet.find(@Addr);
 if (i.Item<>nil) then
 begin
  Result:=TvHostBuffer(ptruint(i.Item^)-ptruint(@TvHostBuffer(nil).FAddr));
 end;
end;

function _New(host:TvPointer;Size:TVkDeviceSize;usage:TVkFlags):TvHostBuffer;
var
 t:TvHostBuffer;
 mr:TVkMemoryRequirements;
 pAlign:TVkDeviceSize;
 Foffset:TVkDeviceSize;
begin
 t:=TvHostBuffer.Create(Size,usage,@buf_ext);

 mr:=t.GetRequirements;

 Foffset:=0;
 if (Size<mr.size) or (not IsAlign(host.FOffset,mr.alignment)) then
 begin
  pAlign:=AlignDw(host.FOffset,mr.alignment);
  Foffset:=(host.FOffset-pAlign);

  host.FOffset:=pAlign;
  Size:=Size+Foffset;

  if (Size<mr.size) then Size:=mr.size;

  FreeAndNil(t);

  t:=TvHostBuffer.Create(Size,usage,@buf_ext);
 end;

 t.Fhost:=host;
 t.Foffset:=Foffset;
 t.BindMem(host);

 Result:=t;
end;

function FetchHostBuffer(cmd:TvCustomCmdBuffer;Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TvHostBuffer;
var
 t:TvHostBuffer;
 host:TvPointer;

 _size:qword;

label
 _exit;

begin
 Result:=nil;

 FHostBufferSet.Lock_wr;

 t:=_Find(Addr);

 if (t<>nil) then
 begin
  if (t.FSize<Size) or
     ((t.FUsage and usage)<>usage) then
  begin
   usage:=usage or t.FUsage;
   FHostBufferSet.delete(@t.FAddr);
   t.Release(nil);
   t:=nil;
  end;
 end;

 if (t=nil) then
 begin
  //Writeln('NewBuf:',HexStr(Addr));
  host:=Default(TvPointer);
  if not TryGetHostPointerByAddr(addr,host,@_size) then
  begin
   Goto _exit;
  end;
  Assert(_size>=Size,'Sparse buffers TODO:'+BooltoStr(vDevice.sparseBinding,True));
  t:=_New(host,Size,usage);
  t.FAddr:=addr;
  FHostBufferSet.Insert(@t.FAddr);
  t.Acquire(nil);
 end;

 if (cmd<>nil) and (t<>nil) then
 begin
  if cmd.AddDependence(@t.Release) then
  begin
   t.Acquire(cmd);
  end;
 end;

 _exit:
 FHostBufferSet.Unlock;
 Result:=t;
end;

Procedure TvHostBuffer.Acquire(Sender:TObject);
begin
 System.InterlockedIncrement(Pointer(FRefs));
end;

procedure TvHostBuffer.Release(Sender:TObject);
begin
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

initialization
 FHostBufferSet.Init;

end.


