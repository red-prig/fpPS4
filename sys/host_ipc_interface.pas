unit host_ipc_interface;

{$mode ObjFPC}{$H+}

interface

uses
 sys_event;

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

 THostIpcHandler=class
  function OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint; virtual;
 end;

 THostIpcInterface=class
  public
   Ftd:Pointer; //p_kthread
   FStop:Boolean;
   //
   procedure   error(const s:RawByteString);
   procedure   kevent(kev:p_kevent;count:Integer);
   function    OpenMainWindows():THandle;
   procedure   SetCaptionFps(Ffps:QWORD);
   //
   function    SendSync(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint; virtual;
   procedure   SendAsyn(mtype:t_mtype;mlen:DWORD;buf:Pointer);         virtual;
   procedure   Update  (Handler:THostIpcHandler);                      virtual;
   //
 end;

implementation

function THostIpcHandler.OnMessage(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
begin
 Result:=0;
end;

function THostIpcInterface.SendSync(mtype:t_mtype;mlen:DWORD;buf:Pointer):Ptruint;
begin
 //
end;

procedure THostIpcInterface.SendAsyn(mtype:t_mtype;mlen:DWORD;buf:Pointer);
begin
 //
end;

procedure THostIpcInterface.Update(Handler:THostIpcHandler);
begin
 //
end;

//

procedure THostIpcInterface.error(const s:RawByteString);
begin
 if (self=nil) then Exit;
 SendSync(iERROR,Length(s)+1,pchar(s));
end;

procedure THostIpcInterface.kevent(kev:p_kevent;count:Integer);
begin
 if (self=nil) then Exit;
 SendAsyn(iKEV_CHANGE,count*SizeOf(t_kevent),kev);
end;

function THostIpcInterface.OpenMainWindows():THandle;
begin
 if (self=nil) then Exit(0);
 Result:=THandle(SendSync(iMAIN_WINDOWS,0,nil));
end;

procedure THostIpcInterface.SetCaptionFps(Ffps:QWORD);
begin
 if (self=nil) then Exit;
 SendAsyn(iCAPTION_FPS,SizeOf(Ffps),@Ffps);
end;


end.

