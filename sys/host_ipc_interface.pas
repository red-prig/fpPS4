unit host_ipc_interface;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 CharStream,
 murmurhash,
 hamt,
 sys_event,
 core_serialization;

const
 iRESULT=0;
 iBROKEN=0;

type
 TIpcValue=packed object
  const
   m_imm   =0;
   m_owned =1;
   m_static=2;
   m_stream=3;
  var
   Flen   :DWORD;
   Fmode  :Word;
   Foffset:Word;
   u:record
    case Byte of
     0:(Fimm:Ptruint); // len <= 8
     1:(Fbuf:Pointer); // len >  8
   end;
  function  New    (buf:Pointer;len:DWORD):TIpcValue; static;
  function  Inplace(node,buf:Pointer;len:DWORD):TIpcValue; static;
  function  Static (buf:Pointer;len:DWORD):TIpcValue; static;
  function  Stream (mem:TCustomMemoryStream):TIpcValue; static;
  function  &Object(obj:TSerializeObject):TIpcValue; static;
  function  AsQWORD(v:QWORD):TIpcValue; static;
  function  Copy:TIpcValue;
  Procedure Free;
  function  GetLen:DWORD;
  function  GetBuf:Pointer;
  procedure MoveTo(buf:Pointer;maxlen:DWORD);
  function  GetDWORD:DWORD;
  function  GetQWORD:QWORD;
  function  GetString:RawByteString;
  function  GetObject(src:TSerializeObjectClass):TSerializeObject;
 end;

 TOnMessage=function(Value:TIpcValue):TIpcValue of object;

 TMsgHash=object
  f_mtype:DWORD;
  msg:RawByteString;
  function mtype:DWORD;
 end;

 THostIpcHandler=class
  private
   type
    PCBNode=^TCBNode;
    TCBNode=object
     cb:TOnMessage;
    end;
   var
    FLock    :Pointer;
    FMsgTable:TSTUB_HAMT32;
  public
   Destructor Destroy; override;
   //
   Procedure  AddCallback(const msg:RawByteString;cb:TOnMessage);
   Procedure  DelCallback(const msg:RawByteString);
   Function   GetCallback(mtype:DWORD):TOnMessage;
 end;

 THostIpcInterface=class
  public
   FHandler:THostIpcHandler;
   //
   procedure   error(const s:RawByteString);
   function    warning(const s:RawByteString):Ptruint;
   procedure   kevent(kev:p_kevent;count:Integer);
   function    OpenMainWindows():THandle;
   procedure   SetCaptionFps(Ffps:QWORD);
   //
   Function    GetCallback(mtype:DWORD):TOnMessage;           virtual;
   function    NewSyncKey:Pointer;                            virtual; abstract;
   procedure   FreeSyncKey (key:Pointer);                     virtual; abstract;
   procedure   WaitSyncKey (key:Pointer);                     virtual; abstract;
   function    GetSyncValue(key:Pointer):TIpcValue;           virtual; abstract;
   procedure   Send(mtype:DWORD;key:Pointer;value:TIpcValue); virtual; abstract;
   procedure   Update();                                      virtual;
   procedure   Disconnect();                                  virtual;
   //
   function    InvokeSync(msg:TMsgHash;Value:TIpcValue):TIpcValue;
   function    InvokeSync(msg:TMsgHash):TIpcValue;
   //
   function    InvokeSync2(msg:TMsgHash;Value:TIpcValue):Ptruint;
   function    InvokeSync2(msg:TMsgHash;buf:Pointer;mlen:DWORD):Ptruint;
   function    InvokeSync2(msg:TMsgHash):Ptruint;
   //
   procedure   InvokeAsyn(msg:TMsgHash;Value:TIpcValue);
   procedure   InvokeAsyn(msg:TMsgHash;buf:Pointer;mlen:DWORD);
   procedure   InvokeAsyn(msg:TMsgHash);
   procedure   InvokeBroken();
   //
   function    HoldResult:DWORD;                        virtual; abstract;
   procedure   InvokeResult(tid:DWORD;value:TIpcValue); virtual; abstract;
 end;

operator := (A:RawByteString):TMsgHash;
operator := (A:DWORD):TMsgHash;

Function HashIpcStr(const msg:RawByteString):DWORD;

//id cache
var
 iERROR       :TMsgHash=(msg:'ERROR');
 iWARNING     :TMsgHash=(msg:'WARNING');
 iKEV_CHANGE  :TMsgHash=(msg:'KEV_CHANGE');
 iKEV_EVENT   :TMsgHash=(msg:'KEV_EVENT');
 iMAIN_WINDOWS:TMsgHash=(msg:'MAIN_WINDOWS');
 iCAPTION_FPS :TMsgHash=(msg:'CAPTION_FPS');

implementation

uses
 kern_rwlock;

function TMsgHash.mtype:DWORD;
begin
 if (f_mtype=0) then
 begin
  f_mtype:=HashIpcStr(msg)
 end;
 Result:=f_mtype;
end;

operator := (A:RawByteString):TMsgHash;
begin
 Result:=Default(TMsgHash);
 Result.msg:=A;
end;

operator := (A:DWORD):TMsgHash;
begin
 Result:=Default(TMsgHash);
 Result.f_mtype:=A;
end;

Procedure SmallMove(src,dst:Pointer;count:DWORD); inline;
type
 PXWORD=^TXWORD;
 TXWORD=array[0..1] of QWORD;
begin
 case count of
   0:;
   1:PByte(dst)[0]:=PByte(src)[0];
   2:PWORD(dst)[0]:=PWORD(src)[0];
   4:PDWORD(dst)[0]:=PDWORD(src)[0];
   8:PQWORD(dst)[0]:=PQWORD(src)[0];
  16:PXWORD(dst)[0]:=PXWORD(src)[0];
  else
    Move(src^,dst^,count);
 end;
end;

function TIpcValue.New(buf:Pointer;len:DWORD):TIpcValue;
begin
 Result:=Default(TIpcValue);
 Result.Flen:=len;
 //
 if (len<=SizeOf(Ptruint)) then
 begin
  //imm
  Result.Fmode:=m_imm;
  SmallMove(buf,@Result.u.Fimm,len);
 end else
 begin
  //copy
  Result.Fmode:=m_owned;
  Result.u.Fbuf:=GetMem(len);
  SmallMove(buf,Result.u.Fbuf,len);
 end;
end;

function TIpcValue.Inplace(node,buf:Pointer;len:DWORD):TIpcValue;
var
 offset:PtrInt;
begin
 offset:=PtrInt(buf)-PtrInt(node);
 Assert(PtrUint(offset)<=High(Word));
 //
 Result:=Default(TIpcValue);
 Result.Flen   :=len;
 Result.Foffset:=offset;
 Result.Fmode  :=m_owned;
 Result.u.Fbuf :=node;
end;

function TIpcValue.Static(buf:Pointer;len:DWORD):TIpcValue;
begin
 Result:=Default(TIpcValue);
 Result.Flen:=len;
 //
 if (len<=SizeOf(Ptruint)) then
 begin
  //imm
  Result.Fmode:=m_imm;
  SmallMove(buf,@Result.u.Fimm,len);
 end else
 begin
  //static
  Result.Fmode :=m_static;
  Result.u.Fbuf:=buf;
 end;
end;

function TIpcValue.Stream(mem:TCustomMemoryStream):TIpcValue;
begin
 Result:=Default(TIpcValue);
 if (mem<>nil) then
 begin
  Result.Flen  :=mem.Size;
  Result.Fmode :=m_stream;
  Result.u.Fbuf:=mem;
 end;
end;

function TIpcValue.&Object(obj:TSerializeObject):TIpcValue; static;
var
 mem:TMemoryStream;
begin
 mem:=nil;
 if (obj<>nil) then
 begin
  mem:=TMemoryStream.Create;
  obj.Serialize(mem);
 end;
 Result:=TIpcValue.Stream(mem);
end;

function TIpcValue.AsQWORD(v:QWORD):TIpcValue;
begin
 Result:=Default(TIpcValue);
 Result.Flen:=SizeOf(QWORD);
 //imm
 Result.Fmode :=m_imm;
 Result.u.Fimm:=v;
end;

function TIpcValue.Copy:TIpcValue;
begin
 if (Fmode=m_static) then
 begin
  Result:=TIpcValue.New(GetBuf,GetLen);
 end else
 begin
  Result:=Self;
 end;
end;

Procedure TIpcValue.Free;
begin
 case Fmode of
  m_owned :FreeMem(u.Fbuf);
  m_imm   :;
  m_static:;
  m_stream:TObject(u.Fbuf).Free;
  else;
 end;
 Self:=Default(TIpcValue);
end;

function TIpcValue.GetLen:DWORD;
begin
 Result:=Flen;
end;

function TIpcValue.GetBuf:Pointer;
begin
 case Fmode of
  m_imm   :Result:=@u.Fimm;
  m_owned :Result:=u.Fbuf+Foffset;
  m_static:Result:=u.Fbuf+Foffset;
  m_stream:Result:=TCustomMemoryStream(u.Fbuf).Memory+Foffset;
  else
           Result:=nil;
 end;
end;

procedure TIpcValue.MoveTo(buf:Pointer;maxlen:DWORD);
var
 len:DWORD;
begin
 len:=Flen;
 if (len>maxlen) then len:=maxlen;
 SmallMove(GetBuf,buf,len);
end;

function TIpcValue.GetDWORD:DWORD;
begin
 if (Fmode=m_imm) then
 begin
  Result:=u.Fimm;
 end else
 if (Flen>=SizeOf(DWORD)) then
 begin
  Result:=PDWORD(GetBuf)^
 end else
 begin
  Result:=0;
  SmallMove(GetBuf,@Result,Flen);
 end;
end;

function TIpcValue.GetQWORD:QWORD;
begin
 if (Fmode=m_imm) then
 begin
  Result:=u.Fimm;
 end else
 if (Flen>=SizeOf(QWORD)) then
 begin
  Result:=PQWORD(GetBuf)^
 end else
 begin
  Result:=0;
  SmallMove(GetBuf,@Result,Flen);
 end;
end;

function TIpcValue.GetString:RawByteString;
begin
 Result:='';
 SetLength(Result,Flen);
 SmallMove(GetBuf,@Result[1],Flen);
end;

function TIpcValue.GetObject(src:TSerializeObjectClass):TSerializeObject;
var
 mem:TPCharStream;
begin
 if (src=nil) or (Flen=0) then
 begin
  Exit(nil);
 end;

 mem:=TPCharStream.Create(GetBuf,Flen);

 Result:=src.Create;
 Result.Deserialize(mem);

 mem.Free;
end;

//

Function HashIpcStr(const msg:RawByteString):DWORD;
var
 i:QWORD;
begin
 i:=MurmurHash64A(@msg[1],Length(msg),$F1F0C0DE);
 Result:=DWORD(i) xor DWORD(i shr 32);
end;

////

procedure _clear_hamt(data,userdata:Pointer);
begin
 FreeMem(data);
end;

Destructor THostIpcHandler.Destroy;
begin
 HAMT_clear32(@FMsgTable,@_clear_hamt,nil);
 inherited;
end;

Procedure THostIpcHandler.AddCallback(const msg:RawByteString;cb:TOnMessage);
var
 hash:DWORD;
 ptr :PCBNode;
 data:PPointer;
begin
 hash:=HashIpcStr(msg);
 Assert(hash<>iRESULT,'Hash is zero!');

 ptr:=AllocMem(SizeOf(TCBNode));
 ptr^.cb:=cb;

 rw_wlock(FLock);

 data:=HAMT_insert32(@FMsgTable,hash,ptr);

 if (data<>nil) then
 begin
  if (data^=ptr) then
  begin
   //
  end else
  begin
   Assert(False,'AddCallback');
   //FreeMem(data^); //free old
   //data^:=ptr;     //set new
  end;
 end else
 begin
  Assert(False,'NOMEM');
 end;

 rw_wunlock(FLock);
end;

Procedure THostIpcHandler.DelCallback(const msg:RawByteString);
var
 hash:DWORD;
 ptr:PCBNode;
begin
 hash:=HashIpcStr(msg);
 Assert(hash<>iRESULT,'Hash is zero!');

 rw_wlock(FLock);

 ptr:=nil;
 if HAMT_delete32(@FMsgTable,hash,@ptr) then
 begin
  FreeMem(ptr);
 end;

 rw_wunlock(FLock);
end;

Function THostIpcHandler.GetCallback(mtype:DWORD):TOnMessage;
var
 ptr :PCBNode;
 data:PPointer;
begin
 Result:=nil;

 rw_rlock(FLock);

 data:=HAMT_search32(@FMsgTable,mtype);
 if (data<>nil) then
 begin
  ptr:=data^;
  if (ptr<>nil) then
  begin
   Result:=ptr^.cb;
  end;
 end;

 rw_runlock(FLock);
end;

Function THostIpcInterface.GetCallback(mtype:DWORD):TOnMessage;
begin
 Result:=nil;
 if (FHandler<>nil) then
 begin
  Result:=FHandler.GetCallback(mtype);
 end;
end;

function THostIpcInterface.InvokeSync(msg:TMsgHash;Value:TIpcValue):TIpcValue;
var
 key:Pointer;
begin
 key:=NewSyncKey;

 Send(msg.mtype,key,Value);

 WaitSyncKey(key);

 Result:=GetSyncValue(key);

 FreeSyncKey(key);
end;

function THostIpcInterface.InvokeSync(msg:TMsgHash):TIpcValue;
begin
 Result:=InvokeSync(msg,Default(TIpcValue));
end;

function THostIpcInterface.InvokeSync2(msg:TMsgHash;Value:TIpcValue):Ptruint;
var
 Output:TIpcValue;
begin
 Output:=InvokeSync(msg,Value);
 Result:=Output.GetQWORD;
 Output.Free;
end;

function THostIpcInterface.InvokeSync2(msg:TMsgHash;buf:Pointer;mlen:DWORD):Ptruint;
begin
 Result:=InvokeSync2(msg,TIpcValue.Static(buf,mlen));
end;

function THostIpcInterface.InvokeSync2(msg:TMsgHash):Ptruint;
begin
 Result:=InvokeSync2(msg,Default(TIpcValue));
end;

procedure THostIpcInterface.InvokeAsyn(msg:TMsgHash;Value:TIpcValue);
begin
 Send(msg.mtype,nil,Value);
end;

procedure THostIpcInterface.InvokeAsyn(msg:TMsgHash;buf:Pointer;mlen:DWORD);
begin
 Send(msg.mtype,nil,TIpcValue.Static(buf,mlen));
end;

procedure THostIpcInterface.InvokeAsyn(msg:TMsgHash);
begin
 Send(msg.mtype,nil,Default(TIpcValue));
end;

procedure THostIpcInterface.InvokeBroken();
begin
 Send(iRESULT,nil,Default(TIpcValue));
end;

procedure THostIpcInterface.Update();
begin
 //
end;

procedure THostIpcInterface.Disconnect();
begin
 //
end;

//

procedure THostIpcInterface.error(const s:RawByteString);
begin
 if (self=nil) then Exit;
 InvokeSync2(iERROR.mtype,pchar(s),Length(s));
end;

function THostIpcInterface.warning(const s:RawByteString):Ptruint;
begin
 if (self=nil) then Exit(-1);
 Result:=InvokeSync2(iWARNING.mtype,pchar(s),Length(s));
end;

procedure THostIpcInterface.kevent(kev:p_kevent;count:Integer);
begin
 if (self=nil) then Exit;
 InvokeAsyn(iKEV_CHANGE.mtype,kev,count*SizeOf(t_kevent));
end;

function THostIpcInterface.OpenMainWindows():THandle;
begin
 if (self=nil) then Exit(0);
 Result:=THandle(InvokeSync2(iMAIN_WINDOWS.mtype));
end;

procedure THostIpcInterface.SetCaptionFps(Ffps:QWORD);
begin
 if (self=nil) then Exit;
 InvokeAsyn(iCAPTION_FPS.mtype,@Ffps,SizeOf(Ffps));
end;


end.

