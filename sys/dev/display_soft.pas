unit display_soft;

{$mode ObjFPC}{$H+}

interface

uses
 display_interface,
 time,
 md_time;

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

 TDisplayHandleSoft=class(TDisplayHandle)

  hWindow:THandle;

  m_attr:array[0.. 3] of t_attr;
  m_bufs:array[0..15] of t_buffer;

  function  Open                     ():Integer; override;
  //function  GetFlipStatus          (status:p_flip_status):Integer; virtual;
  //function  GetResolutionStatus    (status:p_resolution_status):Integer; virtual;
  //function  SetFlipRate            (rate:Integer):Integer; virtual;
  function  RegisterBufferAttribute  (attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  function  SubmitBufferAttribute    (attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  function  UnregisterBufferAttribute(attrid:Byte):Integer; override;
  function  RegisterBuffer           (buf:p_register_buffer):Integer; override;
  function  UnregisterBuffer         (index:Integer):Integer; override;
  function  SubmitFlip               (submit:p_submit_flip):Integer; override;
  //function  SubmitFlipEop          (submit:p_submit_flip;submit_id:QWORD):Integer; virtual;
 end;

implementation

uses
 errno,
 windows,
 kern_proc,
 kern_thr;

function TDisplayHandleSoft.Open():Integer;
begin
 Result:=inherited;

 Writeln('OpenMainWindows->');

 hWindow:=p_proc.p_host_ipc.OpenMainWindows();

 Writeln('OpenMainWindows:',hWindow);
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
 mode:Integer;
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

 mode:=GetStretchBltMode(hdc);
 SetStretchBltMode(hdc, HALFTONE);

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

 SetStretchBltMode(hdc, mode);
 ReleaseDC(hWindow, hdc);
end;

var
 Ffps     :QWORD=0;
 Ftsc_prev:QWORD=0;

function TDisplayHandleSoft.SubmitFlip(submit:p_submit_flip):Integer;
var
 buf :p_buffer;
 attr:p_attr;
begin
 buf:=@m_bufs[submit^.bufferIndex];

 if (buf^.init=0) then Exit(EINVAL);

 attr:=@m_attr[buf^.attr];

 if (attr^.init=0) then Exit(EINVAL);

 last_status.flipArg      :=submit^.flipArg;
 last_status.flipArg2     :=submit^.flipArg2;
 last_status.count        :=last_status.count+1;
 last_status.submitTsc    :=rdtsc;
 last_status.currentBuffer:=submit^.bufferIndex;

 SoftFlip(hWindow,buf,attr);

 knote_eventid(EVENTID_FLIP, submit^.flipArg);

 last_status.tsc        :=rdtsc;
 last_status.processTime:=last_status.tsc;

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

 Result:=0;
end;


end.




