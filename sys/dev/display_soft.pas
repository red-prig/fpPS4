unit display_soft;

{$mode ObjFPC}{$H+}

interface

uses
 display_interface,
 time,
 md_time;

type
 p_buffer=^t_buffer;
 t_buffer=packed record
  attr :t_register_buffer_attr;
  left :Pointer; //buffer ptr
  right:Pointer; //Stereo ptr
 end;

 TDisplayHandleSoft=class(TDisplayHandle)

  hWindow:THandle;

  m_attr:array[0..3 ] of t_register_buffer_attr;
  m_bufs:array[0..15] of t_buffer;

  function  Open                   ():Integer; override;
  //function  GetFlipStatus          (status:p_flip_status):Integer; virtual;
  //function  GetResolutionStatus    (status:p_resolution_status):Integer; virtual;
  //function  SetFlipRate            (rate:Integer):Integer; virtual;
  function  RegisterBufferAttribute(attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  //function  SubmitBufferAttribute  (attrid:Byte;attr:p_register_buffer_attr):Integer; virtual;
  function  RegisterBuffer         (buf:p_register_buffer):Integer; override;
  //function  UnregisterBuffer       (index:Integer):Integer; virtual;
  function  SubmitFlip             (submit:p_submit_flip):Integer; override;
  //function  SubmitFlipEop          (submit:p_submit_flip;submit_id:QWORD):Integer; virtual;
 end;

implementation

uses
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
 m_attr[attrid]:=attr^;

 Result:=0;
end;

function TDisplayHandleSoft.RegisterBuffer(buf:p_register_buffer):Integer;
var
 i:Integer;
begin
 i:=buf^.index;

 m_bufs[i].attr :=m_attr[buf^.attrid];
 m_bufs[i].left :=buf^.left;
 m_bufs[i].right:=buf^.right;

 Result:=0;
end;

procedure SoftFlip(hWindow:THandle;buf:p_buffer);
var
 hdc:THandle;
 bi:BITMAPINFO;

begin
 hdc:=GetDC(hWindow);

 bi:=Default(BITMAPINFO);

 bi.bmiHeader.biSize       :=sizeof(bi.bmiHeader);
 bi.bmiHeader.biWidth      :=buf^.attr.width;
 bi.bmiHeader.biHeight     :=buf^.attr.height;
 bi.bmiHeader.biPlanes     :=1;
 bi.bmiHeader.biBitCount   :=32;
 bi.bmiHeader.biCompression:=BI_RGB;

 SetDIBitsToDevice(hdc,
                  0, 0,
                  buf^.attr.width, buf^.attr.height,
                  0, 0,
                  0, buf^.attr.height,
                  buf^.left, bi, DIB_RGB_COLORS);

 ReleaseDC(hWindow, hdc);
end;

var
 Ffps     :QWORD=0;
 Ftsc_prev:QWORD=0;

function TDisplayHandleSoft.SubmitFlip(submit:p_submit_flip):Integer;
var
 buf:p_buffer;
begin
 last_status.flipArg      :=submit^.flipArg;
 last_status.flipArg2     :=submit^.flipArg2;
 last_status.count        :=last_status.count+1;
 last_status.submitTsc    :=rdtsc;
 last_status.currentBuffer:=submit^.bufferIndex;

 buf:=@m_bufs[submit^.bufferIndex];

 SoftFlip(hWindow,buf);

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




