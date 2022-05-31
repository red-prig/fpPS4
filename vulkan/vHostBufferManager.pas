unit vHostBufferManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 RWLock,
 sys_types,
 g23tree,
 Vulkan,
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
  //FDeps:TObjectSetLock;
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

 _TvHostBufferPool=specialize T23treeSet<PPointer,TvAddrCompare>;
 TvHostBufferPool=object(_TvHostBufferPool)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

var
 FHostBufferPool:TvHostBufferPool;

Procedure TvHostBufferPool.Init;
begin
 rwlock_init(lock);
end;

Procedure TvHostBufferPool.Lock_wr;
begin
 rwlock_wrlock(lock);
end;

Procedure TvHostBufferPool.Unlock;
begin
 rwlock_unlock(lock);
end;

function TvAddrCompare.c(a,b:PPointer):Integer;
begin
 Result:=Integer(a^>b^)-Integer(a^<b^);
end;

function _Find(Addr:Pointer):TvHostBuffer;
var
 i:TvHostBufferPool.Iterator;
begin
 Result:=nil;
 i:=FHostBufferPool.find(@Addr);
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
 if not IsAlign(host.FOffset,mr.alignment) then
 begin
  pAlign:=AlignDw(host.FOffset,mr.alignment);
  Foffset:=(host.FOffset-pAlign);

  host.FOffset:=pAlign;
  Size:=Size+Foffset;

  FreeAndNil(t);

  t:=TvHostBuffer.Create(Size,usage,@buf_ext);
  //t.FDeps.Init;
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

label
 _exit;

begin
 Result:=nil;

 FHostBufferPool.Lock_wr;

 t:=_Find(Addr);

 if (t<>nil) then
 begin
  if (t.FSize<Size) or
     ((t.FUsage and usage)<>usage) then
  begin
   usage:=usage or t.FUsage;
  end;
  FHostBufferPool.delete(@t.FAddr);
  t.Release(nil);
  t:=nil;
 end;

 if (t=nil) then
 begin
  host:=Default(TvPointer);
  if not TryGetHostPointerByAddr(addr,host) then
  begin
   Goto _exit;
  end;
  t:=_New(host,Size,usage);
  FHostBufferPool.Insert(@t.FAddr);
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
 FHostBufferPool.Unlock;
 Result:=t;
end;

Procedure TvHostBuffer.Acquire(Sender:TObject);
begin
 System.InterlockedIncrement(Pointer(FRefs));
 //if (Sender<>nil) then
 //begin
 // FDeps.Insert(Sender);
 //end;
end;

procedure TvHostBuffer.Release(Sender:TObject);
begin
 //if (Sender<>nil) then
 //begin
 // FDeps.delete(Sender);
 //end;
 if System.InterlockedDecrement(Pointer(FRefs))=nil then
 begin
  Free;
 end;
end;

initialization
 FHostBufferPool.Init;

end.


