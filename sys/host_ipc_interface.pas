unit host_ipc_interface;

{$mode ObjFPC}{$H+}

interface

uses
 murmurhash,
 HAMT,
 sys_event;

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
 POnMessage=^TOnMessage;
 TOnMessage=function(mlen:DWORD;buf:Pointer):Ptruint of object;

 THostIpcHandler=class
  private
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
   Ftd     :Pointer; //p_kthread
   FStop   :Boolean;
   //
   procedure   error(const s:RawByteString);
   procedure   kevent(kev:p_kevent;count:Integer);
   function    OpenMainWindows():THandle;
   procedure   SetCaptionFps(Ffps:QWORD);
   //
   Function    GetCallback(mtype:DWORD):TOnMessage;            virtual;
   function    SendSync(mtype,mlen:DWORD;buf:Pointer):Ptruint; virtual;
   procedure   SendAsyn(mtype,mlen:DWORD;buf:Pointer);         virtual;
   procedure   Update  ();                                     virtual;
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
 ptr:POnMessage;
 data:PPointer;
begin
 hash:=HashIpcStr(msg);
 Assert(hash<>iRESULT,'Hash is zero!');

 ptr:=AllocMem(SizeOf(TOnMessage));
 ptr^:=cb;

 data:=HAMT_insert32(@FMsgTable,hash,ptr);

 if (data<>nil) then
 begin
  if (data^=ptr) then
  begin
   //
  end else
  begin
   FreeMem(data^); //free old
   data^:=ptr;     //set new
  end;
 end else
 begin
  Assert(False,'NOMEM');
 end;
end;

Procedure THostIpcHandler.DelCallback(const msg:RawByteString);
var
 hash:DWORD;
var
 ptr:POnMessage;
begin
 hash:=HashIpcStr(msg);
 Assert(hash<>iRESULT,'Hash is zero!');

 ptr:=nil;
 if HAMT_delete32(@FMsgTable,hash,@ptr) then
 begin
  FreeMem(ptr);
 end;
end;

Function THostIpcHandler.GetCallback(mtype:DWORD):TOnMessage;
var
 ptr :POnMessage;
 data:PPointer;
begin
 Result:=nil;
 data:=HAMT_search32(@FMsgTable,mtype);
 if (data<>nil) then
 begin
  ptr:=data^;
  if (ptr<>nil) then
  begin
   Result:=ptr^;
  end;
 end;
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
begin
 //
end;

procedure THostIpcInterface.SendAsyn(mtype,mlen:DWORD;buf:Pointer);
begin
 //
end;

procedure THostIpcInterface.Update();
begin
 //
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

