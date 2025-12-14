unit host_ipc_interface;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 CharStream,
 murmurhash,
 kern_hamt,
 sys_event,
 game_info;

const
 iRESULT=0;

{
type
 t_mtype=(
  iRESULT,
  iERROR,
  iKEV_CHANGE,
  iKEV_EVENT,
  iMOUNT,
  iMAIN_WINDOWS,
  iCAPTION_FPS
 );
}

type
 TOnMessage=function(mlen:DWORD;buf:Pointer):Ptruint of object;
 TOnObject =function(obj:TObject):Ptruint            of object;

 THostIpcHandler=class
  private
   type
    PCBNode=^TCBNode;
    TCBNode=object
     cb:TOnMessage;
    end;
    //
    PCBNodeObject=^TCBNodeObject;
    TCBNodeObject=object(TCBNode)
     Creator:TAbstractObjectClass;
     cb_obj :TOnObject;
     function OnObject(mlen:DWORD;buf:Pointer):Ptruint;
    end;
   var
    FLock    :Pointer;
    FMsgTable:TSTUB_HAMT32;
  public
   Destructor Destroy; override;
   //
   Procedure  AddCallback(const msg:RawByteString;cb:TOnMessage);
   Procedure  AddCallback(const msg:RawByteString;cb:TOnObject;Creator:TAbstractObjectClass);
   Procedure  DelCallback(const msg:RawByteString);
   Function   GetCallback(mtype:DWORD):TOnMessage;
 end;

 THostIpcInterface=class
  public
   FHandler:THostIpcHandler;
   Ftd     :Pointer; //p_kthread
   FStop   :Boolean;
   //
   procedure   error(const s:RawByteString);
   procedure   kevent(kev:p_kevent;count:Integer);
   function    OpenMainWindows():THandle;
   procedure   SetCaptionFps(Ffps:QWORD);
   //
   Function    GetCallback(mtype:DWORD):TOnMessage;            virtual;
   function    NewSyncKey:Pointer;                             virtual; abstract;
   procedure   FreeSyncKey (key:Pointer);                      virtual; abstract;
   procedure   WaitSyncKey (key:Pointer);                      virtual; abstract;
   function    GetSyncValue(key:Pointer):Ptruint;              virtual; abstract;
   procedure   Send(mtype,mlen:DWORD;buf,key:Pointer);         virtual; abstract;
   procedure   Update  ();                                     virtual;
   //
   function    SendSync(mtype,mlen:DWORD;buf:Pointer):Ptruint;
   procedure   SendAsyn(mtype,mlen:DWORD;buf:Pointer);
   function    SendSync(const msg:RawByteString):Ptruint;
   function    SendSync(mtype:DWORD;obj:TAbstractObject):Ptruint;
   procedure   SendAsyn(mtype:DWORD;obj:TAbstractObject);
   function    SendSync(const msg:RawByteString;obj:TAbstractObject):Ptruint;
   procedure   SendAsyn(const msg:RawByteString;obj:TAbstractObject);
   //
 end;

Function HashIpcStr(const msg:RawByteString):DWORD;

//id cache
var
 iERROR       :DWORD=0;
 iKEV_CHANGE  :DWORD=0;
 iKEV_EVENT   :DWORD=0;
 iMAIN_WINDOWS:DWORD=0;
 iCAPTION_FPS :DWORD=0;

implementation

uses
 kern_rwlock;

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

function THostIpcHandler.TCBNodeObject.OnObject(mlen:DWORD;buf:Pointer):Ptruint;
var
 mem:TPCharStream;
 obj:TAbstractObject;
begin
 if (Creator=nil) or (cb_obj=nil) then
 begin
  Exit(Ptruint(-1));
 end;

 if (mlen=0) then
 begin
  obj:=nil;
 end else
 begin
  mem:=TPCharStream.Create(buf,mlen);

  obj:=Creator.Create;
  obj.Deserialize(mem);

  mem.Free;
 end;

 Result:=cb_obj(obj);
end;

Procedure THostIpcHandler.AddCallback(const msg:RawByteString;cb:TOnObject;Creator:TAbstractObjectClass);
var
 hash:DWORD;
 ptr :PCBNodeObject;
 data:PPointer;
begin
 hash:=HashIpcStr(msg);
 Assert(hash<>iRESULT,'Hash is zero!');

 ptr:=AllocMem(SizeOf(TCBNodeObject));
 ptr^.cb:=@ptr^.OnObject;
 ptr^.Creator:=Creator;
 ptr^.cb_obj :=cb;

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

function THostIpcInterface.SendSync(mtype,mlen:DWORD;buf:Pointer):Ptruint;
var
 key:Pointer;
begin
 key:=NewSyncKey;

 Send(mtype,mlen,buf,key);

 WaitSyncKey(key);

 Result:=GetSyncValue(key);

 FreeSyncKey(key);
end;

procedure THostIpcInterface.SendAsyn(mtype,mlen:DWORD;buf:Pointer);
begin
 Send(mtype,mlen,buf,nil);
end;

procedure THostIpcInterface.Update();
begin
 //
end;

//

function THostIpcInterface.SendSync(const msg:RawByteString):Ptruint;
begin
 Result:=SendSync(HashIpcStr(msg),0,nil);
end;

function THostIpcInterface.SendSync(mtype:DWORD;obj:TAbstractObject):Ptruint;
var
 key:Pointer;
 mem:TMemoryStream;
begin
 if (obj<>nil) then
 begin
  mem:=TMemoryStream.Create;
  obj.Serialize(mem);
 end;

 key:=NewSyncKey;

 if (obj<>nil) then
 begin
  Send(mtype,mem.Size,mem.Memory,key);
  mem.Free;
 end else
 begin
  Send(mtype,0,nil,key);
 end;

 WaitSyncKey(key);

 Result:=GetSyncValue(key);

 FreeSyncKey(key);
end;

procedure THostIpcInterface.SendAsyn(mtype:DWORD;obj:TAbstractObject);
var
 mem:TMemoryStream;
begin
 if (obj<>nil) then
 begin
  mem:=TMemoryStream.Create;
  obj.Serialize(mem);

  Send(mtype,mem.Size,mem.Memory,nil);

  mem.Free;
 end else
 begin
  Send(mtype,0,nil,nil);
 end;
end;

function THostIpcInterface.SendSync(const msg:RawByteString;obj:TAbstractObject):Ptruint;
begin
 Result:=SendSync(HashIpcStr(msg),obj);
end;

procedure THostIpcInterface.SendAsyn(const msg:RawByteString;obj:TAbstractObject);
begin
 SendAsyn(HashIpcStr(msg),obj);
end;

//

procedure THostIpcInterface.error(const s:RawByteString);
begin
 if (self=nil) then Exit;
 if (iERROR=0) then iERROR:=HashIpcStr('ERROR');
 SendSync(iERROR,Length(s)+1,pchar(s));
end;

procedure THostIpcInterface.kevent(kev:p_kevent;count:Integer);
begin
 if (self=nil) then Exit;
 if (iKEV_CHANGE=0) then iKEV_CHANGE:=HashIpcStr('KEV_CHANGE');
 SendAsyn(iKEV_CHANGE,count*SizeOf(t_kevent),kev);
end;

function THostIpcInterface.OpenMainWindows():THandle;
begin
 if (self=nil) then Exit(0);
 if (iMAIN_WINDOWS=0) then iMAIN_WINDOWS:=HashIpcStr('MAIN_WINDOWS');
 Result:=THandle(SendSync(iMAIN_WINDOWS,0,nil));
end;

procedure THostIpcInterface.SetCaptionFps(Ffps:QWORD);
begin
 if (self=nil) then Exit;
 if (iCAPTION_FPS=0) then iCAPTION_FPS:=HashIpcStr('CAPTION_FPS');
 SendAsyn(iCAPTION_FPS,SizeOf(Ffps),@Ffps);
end;


end.

