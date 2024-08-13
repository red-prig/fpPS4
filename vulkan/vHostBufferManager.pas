unit vHostBufferManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 g23tree,
 Vulkan,
 vMemory,
 vBuffer,
 vDependence;

type
 TvHostBuffer=class(TvBuffer)
  FAddr:QWORD;
 end;

function FetchHostBuffer(cmd:TvDependenciesObject;
                         Addr:QWORD;
                         Size:TVkDeviceSize;
                         device_local:Boolean=False):TvHostBuffer;

implementation

uses
 kern_rwlock,
 kern_dmem;

type
 TvHostBufferKey=packed record
  FAddr  :QWORD;
  FBuffer:TvHostBuffer;
 end;

 TvAddrCompare=object
  function c(const a,b:TvHostBufferKey):Integer; static;
 end;

 _TvHostBufferSet=specialize T23treeSet<TvHostBufferKey,TvAddrCompare>;
 TvHostBufferSet=object(_TvHostBufferSet)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

var
 FHostBufferSet:TvHostBufferSet;


Procedure TvHostBufferSet.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TvHostBufferSet.Unlock_wr;
begin
 rw_wunlock(lock);
end;

function TvAddrCompare.c(const a,b:TvHostBufferKey):Integer;
begin
 //1 FAddr
 Result:=Integer(a.FAddr>b.FAddr)-Integer(a.FAddr<b.FAddr);
 if (Result<>0) then Exit;
end;

var
 host_alignment:TVkDeviceSize=0;

function _fix_buf_size(var Addr:QWORD;var Size:TVkDeviceSize;usage:TVkFlags):TVkDeviceSize;
var
 mr:TVkMemoryRequirements;
begin
 if (host_alignment=0) then
 begin
  mr:=GetRequirements(false,Size,usage,@buf_ext);
  host_alignment:=mr.alignment;
 end;

 Result:=(Addr mod host_alignment);

 Addr:=Addr-Result;
 Size:=Size+Result;
end;

function _FindHostBuffer(Addr:QWORD;Size:TVkDeviceSize):TvHostBuffer;
label
 _repeat,
 _delete;
var
 It:TvHostBufferSet.Iterator;
 tmp:TvHostBufferKey;
 buf:TvHostBuffer;
 __end:QWORD;
begin
 Result:=nil;
 __end:=Addr+Size;

 _repeat:

 tmp:=Default(TvHostBufferKey);
 tmp.FAddr :=Addr;

 It:=FHostBufferSet.find(tmp);

 if (It.Item<>nil) then
 begin
  buf:=It.Item^.FBuffer;

  if buf.Acquire(nil) then
  begin

   if ((buf.FAddr+buf.FSize)>=__end) then
   begin
    Exit(buf);
   end else
   begin
    buf.Release(nil);
    //The search key matches but the size does not.
    goto _delete;
   end;

  end else
  begin
   _delete:
   //mem is deleted, free buf
   FHostBufferSet.erase(It);
   buf._Release(nil); //map ref
   buf:=nil;
   goto _repeat;
  end;

 end;

 tmp:=Default(TvHostBufferKey);
 tmp.FAddr :=__end;

 //[s|new|e] ->
 //      [s|old|e]

 It:=FHostBufferSet.find_ls(tmp);

 while (It.Item<>nil) do
 begin
  buf:=It.Item^.FBuffer;

  if buf.Acquire(nil) then
  begin

   if (buf.FAddr<=Addr) and
      ((buf.FAddr+buf.FSize)>=__end) then
   begin
    Exit(buf);
   end;

   buf.Release(nil);
  end else
  begin
   goto _delete;
  end;

  if not It.Prev then Break;
 end;

end;

const
 ALL_BUFFER_USAGE=
  ord(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
  ord(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
  ord(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT) or
  ord(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
  ord(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or
  ord(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or
  ord(VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT);

function FetchHostBuffer(cmd:TvDependenciesObject;
                         Addr:QWORD;
                         Size:TVkDeviceSize;
                         device_local:Boolean=False):TvHostBuffer;
label
 _repeat;
var
 dmem_addr:QWORD;
 key:TvHostBufferKey;
 mem:TvPointer;
begin
 Result:=nil;
 Assert(Size<>0);

 dmem_addr:=0;
 if not get_dmem_ptr(Pointer(Addr),@dmem_addr,nil) then
 begin
  Assert(false,'addr:0x'+HexStr(Pointer(Addr))+' not in dmem!');
 end;

 dmem_addr:=dmem_addr-_fix_buf_size(Addr,Size,ALL_BUFFER_USAGE);

 key:=Default(TvHostBufferKey);
 key.FAddr:=Addr;

 //
 FHostBufferSet.Lock_wr;
 //

 _repeat:

 key.FBuffer:=_FindHostBuffer(Addr,Size);

 //
 FHostBufferSet.Unlock_wr;
 //

 if (key.FBuffer<>nil) then
 begin
  mem:=Default(TvPointer);
 end else
 begin
  //create new

  mem:=MemManager.FetchHostMap(dmem_addr,Size,device_local);

  if (mem.FMemory=nil) then
  begin

   if device_local then
   begin
    mem:=MemManager.FetchHostMap(dmem_addr,Size,False);

    if (mem.FMemory=nil) then
    begin
     //ENOMEM
     Exit(nil);
    end;

   end else
   begin
    //ENOMEM
    Exit(nil);
   end;

  end;

  key.FBuffer:=TvHostBuffer.Create(Size,ALL_BUFFER_USAGE,@buf_ext);
  key.FBuffer.FAddr:=Addr;

  key.FBuffer.SetObjectName('HB_0x'+HexStr(Addr,10)+'..'+HexStr(Addr+Size,10));

  if (key.FBuffer.BindMem(mem)<>VK_SUCCESS) then
  begin
   //unknow error
   FreeAndNil(key.FBuffer);
   mem.Release; //release [FetchHostMap]
   //
   Exit(nil);
  end;

  //
  FHostBufferSet.Lock_wr;
  //

  if FHostBufferSet.Insert(key) then
  begin
   key.FBuffer._Acquire(nil); //map ref
  end else
  begin
   //collision?

   FreeAndNil(key.FBuffer);
   mem.Release; //release [FetchHostMap]

   //
   goto _repeat;
  end;

  key.FBuffer.Acquire(nil); //analog ref in [_FindHostBuffer]

  //
  FHostBufferSet.Unlock_wr;
  //

  //create new
 end;

 Result:=key.FBuffer;

 //add dep
 cmd.RefTo(Result);

 if (Result<>nil) then
 begin
  Result.Release(nil); //release [_FindHostBuffer]
  mem.Release;         //release [FetchHostMap]
 end;

end;


end.


