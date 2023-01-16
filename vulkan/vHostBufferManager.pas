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
 AVkSparseMemoryBind=array of TVkSparseMemoryBind;

 TvHostBuffer=class(TvBuffer)
  FAddr:Pointer;
  Fhost:TvPointer;
  Foffset:TVkDeviceSize; //offset inside buffer
  //
  FSparse:AVkSparseMemoryBind;
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

function Max(a,b:QWORD):QWORD; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function Min(a,b:QWORD):QWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function _fix_buf_size(sparce:Boolean;var Addr:Pointer;var Size:TVkDeviceSize;usage:TVkFlags):TVkDeviceSize;
var
 mr:TVkMemoryRequirements;
begin
 mr:=GetRequirements(sparce,Size,usage,@buf_ext);

 Result:=(ptruint(Addr) mod mr.alignment);

 Addr:=Pointer(ptruint(Addr)-Result);
 Size:=Size+Result;
end;

function _is_sparce(Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):Integer;
var
 host:TvPointer;
 hsize:qword;
begin
 _fix_buf_size(False,Addr,Size,usage);

 host:=Default(TvPointer);
 if not TryGetHostPointerByAddr(addr,host,@hsize) then
 begin
  Exit(-1);
 end;

 if (hsize>=Size) then
 begin
  Result:=0;
 end else
 begin
  Result:=1;
 end;
end;

function _New_simple(Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TvHostBuffer;
var
 host:TvPointer;

 t:TvHostBuffer;
 delta:TVkDeviceSize;
begin
 Result:=nil;

 delta:=_fix_buf_size(False,Addr,Size,usage);

 host:=Default(TvPointer);
 if not TryGetHostPointerByAddr(addr,host) then Exit;

 t:=TvHostBuffer.Create(Size,usage,@buf_ext);

 t.Fhost  :=host;
 t.Foffset:=delta;
 t.BindMem(host);

 Result:=t;
end;

function _New_sparce(queue:TVkQueue;Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TvHostBuffer;
var
 host:TvPointer;

 asize:qword;
 hsize:qword;
 msize:qword;

 Offset,delta:TVkDeviceSize;

 bind:TVkSparseMemoryBind;
 Binds:AVkSparseMemoryBind;
 i:Integer;

 t:TvHostBuffer;
begin
 Result:=nil;

 //hack; alignment is the same in virtual memory
 delta:=_fix_buf_size(True,Addr,Size,usage);

 Binds:=Default(AVkSparseMemoryBind);
 host :=Default(TvPointer);
 hsize:=0;

 Offset:=0;
 asize:=Size;
 While (asize<>0) do
 begin
  if not TryGetHostPointerByAddr(addr,host,@hsize) then Exit;

  msize:=Min(hsize,asize);

  bind:=Default(TVkSparseMemoryBind);
  bind.resourceOffset:=Offset;
  bind.size          :=msize;
  bind.memory        :=host.FHandle;
  bind.memoryOffset  :=host.FOffset;

  i:=Length(Binds);
  SetLength(Binds,i+1);
  Binds[i]:=bind;

  //next
  Offset:=Offset+msize;
  addr  :=addr  +msize;
  asize :=asize -msize;
 end;

 t:=TvHostBuffer.CreateSparce(Size,usage,@buf_ext);

 t.Foffset:=delta;
 t.FSparse:=Binds;

 if (VkBindSparseBufferMemory(queue,t.FHandle,Length(Binds),@Binds[0])<>VK_SUCCESS) then
 begin
  t.Free;
  Exit;
 end;

 Result:=t;
end;

function FetchHostBuffer(cmd:TvCustomCmdBuffer;Addr:Pointer;Size:TVkDeviceSize;usage:TVkFlags):TvHostBuffer;
var
 t:TvHostBuffer;

label
 _exit;

begin
 Result:=nil;
 Assert(Size<>0);

 FHostBufferSet.Lock_wr;

 t:=_Find(Addr); //find by key

 if (t<>nil) then
 begin
  if (t.FSize<(t.Foffset+Size)) or
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

  t:=nil;
  Case _is_sparce(Addr,Size,usage) of
   0:begin
      t:=_New_simple(Addr,Size,usage);
      Assert(t<>nil,'create simple buffer fail');
     end;
   1:begin  //is Sparse buffers
      Assert(vDevice.sparseBinding,'sparseBinding not support');
      Assert(MemManager.SparceSupportHost,'sparse not support for host');
      t:=_New_sparce(cmd.FQueue.FHandle,Addr,Size,usage);
      Assert(t<>nil,'create sparse buffer fail');
     end;
   else
    Assert(false,'Is not GPU Addr:'+HexStr(Addr));
  end;

  t.FAddr:=addr; //save key

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


