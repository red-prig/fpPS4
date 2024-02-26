unit display_soft;

{$mode ObjFPC}{$H+}

interface

uses
 LFQueue,
 display_interface,
 time,
 md_time,
 kern_thr,
 kern_mtx;

type
 p_attr=^t_attr;
 t_attr=packed record
  init :QWORD;
  attr :t_register_buffer_attr;
 end;

 p_buffer=^t_buffer;
 t_buffer=packed record
  init :QWORD;
  attr :QWORD;
  left :Pointer; //buffer ptr
  right:Pointer; //Stereo ptr
 end;

 PQNode=^TQNode;
 TOnParent=Procedure(node:PQNode) of object;
 TQNode=object
  next_ :PQNode;
  Parent:TOnParent;
  u:record
   Case Byte of
    0:(submit:t_submit_flip;
       tsc   :QWORD);
    1:(attr  :t_register_buffer_attr;);
  end;
 end;

 TSubmitQueue=object
  FNodes:array[0..31] of TQNode;
  FAlloc:TIntrusiveMPSCQueue;
  FQueue:TIntrusiveMPSCQueue;
  Procedure Init;
  function  Alloc:PQNode;
  procedure Free(P:PQNode);
 end;

 TDisplayHandleSoft=class(TDisplayHandle)

  hWindow:THandle;

  FEvent:PRTLEvent;
  Ftd:p_kthread;
  FQueue:TSubmitQueue;
  FTerminate:Boolean;

  m_attr:array[0.. 3] of t_attr;
  m_bufs:array[0..15] of t_buffer;

  Ffps     :QWORD;
  Ftsc_prev:QWORD;

  function   Open                     ():Integer; override;
  Destructor Destroy; override;
  //function  GetFlipStatus          (status:p_flip_status):Integer; virtual;
  //function  GetResolutionStatus    (status:p_resolution_status):Integer; virtual;
  //function  SetFlipRate            (rate:Integer):Integer; virtual;
  function   RegisterBufferAttribute  (attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  function   SubmitBufferAttribute    (attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  function   UnregisterBufferAttribute(attrid:Byte):Integer; override;
  function   RegisterBuffer           (buf:p_register_buffer):Integer; override;
  function   UnregisterBuffer         (index:Integer):Integer; override;
  function   SubmitFlip               (submit:p_submit_flip):Integer; override;
  //function  SubmitFlipEop          (submit:p_submit_flip;submit_id:QWORD):Integer; virtual;
  function   Vblank                   ():Integer; override;
  //
  procedure  OnSubmit(Node:PQNode);
 end;

implementation

uses
 errno,
 windows,
 {
 Types,
 LCLType,
 LCLIntf,
 }
 kern_proc,
 kern_thread;

Procedure TSubmitQueue.Init;
var
 i:Integer;
begin
 FAlloc.Create;
 FQueue.Create;
 For i:=0 to High(FNodes) do
 begin
  FAlloc.Push(@FNodes[i]);
 end;
end;

function TSubmitQueue.Alloc:PQNode;
begin
 Result:=nil;
 FAlloc.Pop(Result);
end;

procedure TSubmitQueue.Free(P:PQNode);
begin
 if (P=nil) then Exit;
 FAlloc.Push(P);
end;

procedure dce_thread(parameter:pointer); SysV_ABI_CDecl; forward;

function TDisplayHandleSoft.Open():Integer;
begin
 Result:=inherited;

 Writeln('OpenMainWindows->');

 hWindow:=p_proc.p_host_ipc.OpenMainWindows();

 Writeln('OpenMainWindows:',hWindow);

 FEvent:=RTLEventCreate;

 FQueue.Init;

 if (Ftd=nil) then
 begin
  kthread_add(@dce_thread,Self,@Ftd,'[dce_soft]');
 end;
end;

Destructor TDisplayHandleSoft.Destroy;
begin
 if (Ftd<>nil) then
 begin
  FTerminate:=True;
  RTLEventSetEvent(FEvent);
  WaitForThreadTerminate(Ftd^.td_handle,0);
  thread_dec_ref(Ftd);
  Ftd:=nil;
 end;
 RTLEventDestroy(FEvent);
 inherited;
end;

function TDisplayHandleSoft.RegisterBufferAttribute(attrid:Byte;attr:p_register_buffer_attr):Integer;
begin
 if (m_attr[attrid].init<>0) then Exit(EINVAL);

 m_attr[attrid].init:=1;
 m_attr[attrid].attr:=attr^;

 Result:=0;
end;

function TDisplayHandleSoft.SubmitBufferAttribute(attrid:Byte;attr:p_register_buffer_attr):Integer;
begin
 m_attr[attrid].init:=1;
 m_attr[attrid].attr:=attr^;

 Result:=0;
end;

function TDisplayHandleSoft.UnregisterBufferAttribute(attrid:Byte):Integer;
begin
 if (m_attr[attrid].init=0) then Exit(EINVAL);

 m_attr[attrid].init:=0;

 Result:=0;
end;

function TDisplayHandleSoft.RegisterBuffer(buf:p_register_buffer):Integer;
var
 i,a:Integer;
begin
 i:=buf^.index;
 a:=buf^.attrid;

 if (m_bufs[i].init<>0) then Exit(EINVAL);
 if (m_attr[a].init=0 ) then Exit(EINVAL);

 m_bufs[i].init :=1;
 m_bufs[i].attr :=a;
 m_bufs[i].left :=buf^.left;
 m_bufs[i].right:=buf^.right;

 Result:=0;
end;

function TDisplayHandleSoft.UnregisterBuffer(index:Integer):Integer;
begin
 if (m_bufs[index].init=0) then Exit(EINVAL);

 m_bufs[index].init:=0;

 Result:=0;
end;

procedure SoftFlip(hWindow:THandle;buf:p_buffer;attr:p_attr);
var
 hdc:THandle;
 bi:BITMAPINFO;
 rect:TRect;
begin
 hdc:=GetDC(hWindow);

 rect:=Default(TRect);
 GetClientRect(hWindow,rect);

 bi:=Default(BITMAPINFO);

 bi.bmiHeader.biSize       :=sizeof(bi.bmiHeader);
 bi.bmiHeader.biWidth      :=attr^.attr.width;
 bi.bmiHeader.biHeight     :=-attr^.attr.height;
 bi.bmiHeader.biPlanes     :=1;
 bi.bmiHeader.biBitCount   :=32;
 bi.bmiHeader.biCompression:=BI_RGB;

 //SetStretchBltMode(hdc, HALFTONE);

 StretchDIBits(hdc,
               0,0,
               rect.Width, rect.Height,
               0,0,
               attr^.attr.width,
               attr^.attr.height,
               buf^.left,
               bi,
               DIB_RGB_COLORS,
               SRCCOPY
 );

 {
 SetDIBitsToDevice(hdc,
                  0, 0,
                  attr^.attr.width, attr^.attr.height,
                  0, 0,
                  0, attr^.attr.height,
                  buf^.left, bi, DIB_RGB_COLORS);
 }

 ReleaseDC(hWindow, hdc);
end;

function TDisplayHandleSoft.SubmitFlip(submit:p_submit_flip):Integer;
var
 buf :p_buffer;
 attr:p_attr;
 Node:PQNode;
begin
 if (submit^.bufferIndex<>-1) then
 begin
  buf:=@m_bufs[submit^.bufferIndex];
  if (buf^.init=0) then Exit(EINVAL);
  attr:=@m_attr[buf^.attr];
  if (attr^.init=0) then Exit(EINVAL);
 end;

 Node:=FQueue.Alloc;
 if (Node=nil) then Exit(EBUSY);

 Node^.Parent  :=@OnSubmit;
 Node^.u.submit:=submit^;
 Node^.u.tsc   :=rdtsc;

 FQueue.FQueue.Push(Node);
 //RTLEventSetEvent(FEvent);

 last_status.flipPendingNum0:=last_status.flipPendingNum0+1;

 Result:=0;
end;

function TDisplayHandleSoft.Vblank():Integer;
begin
 RTLEventSetEvent(FEvent);
 Result:=0;
end;

procedure TDisplayHandleSoft.OnSubmit(Node:PQNode);
var
 submit:p_submit_flip;
 buf :p_buffer;
 attr:p_attr;
begin
 submit:=@Node^.u.submit;

 if (submit^.bufferIndex=-1) then Exit;

 buf:=@m_bufs[submit^.bufferIndex];
 if (buf^.init=0) then Exit;
 attr:=@m_attr[buf^.attr];
 if (attr^.init=0) then Exit;

 SoftFlip(hWindow,buf,attr);

 mtx_lock(mtx^);

  last_status.flipPendingNum0:=last_status.flipPendingNum0-1;
  last_status.flipArg        :=submit^.flipArg;
  last_status.flipArg2       :=submit^.flipArg2;
  last_status.count          :=last_status.count+1;
  last_status.submitTsc      :=Node^.u.tsc;
  last_status.currentBuffer  :=submit^.bufferIndex;
  last_status.tsc            :=rdtsc;
  last_status.processTime    :=last_status.tsc;

 mtx_unlock(mtx^);

 knote_eventid(EVENTID_FLIP, submit^.flipArg);

 if (Ftsc_prev=0) then
 begin
  Ftsc_prev:=last_status.tsc;
  Ffps:=1;
 end else
 begin
  Inc(Ffps);
  if ((last_status.tsc-Ftsc_prev) div tsc_freq)>=1 then
  begin
   p_proc.p_host_ipc.SetCaptionFPS(Ffps);
   Ffps:=0;
   Ftsc_prev:=last_status.tsc;
  end;
 end;

end;

procedure dce_thread(parameter:pointer); SysV_ABI_CDecl;
var
 dce:TDisplayHandleSoft;
 Node:PQNode;
begin
 dce:=TDisplayHandleSoft(parameter);

 repeat
  RTLEventWaitFor(dce.FEvent);

  Node:=nil;
  if dce.FQueue.FQueue.Pop(Node) then
  begin
   if (Node^.Parent<>nil) then
   begin
    Node^.Parent(Node);
   end;
   dce.FQueue.Free(Node);
  end;

 until dce.FTerminate;

end;


end.




