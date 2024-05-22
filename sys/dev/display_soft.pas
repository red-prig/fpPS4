unit display_soft;

{$mode ObjFPC}{$H+}

interface

uses
 LFQueue,
 display_interface,
 time,
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
  left_dmem :Pointer; //buffer ptr
  right_dmem:Pointer; //Stereo ptr
 end;

 PQNode=^TQNode;
 TQNode=object
  next_:PQNode;
 end;

 PQNodeSubmit=^TQNodeSubmit;
 TQNodeSubmit=object(TQNode)
  submit:t_submit_flip;
  tsc   :QWORD;
 end;

 TSubmitAlloc=object
  FNodes:array[0..31] of TQNodeSubmit;
  FAlloc:TIntrusiveMPSCQueue;
  Procedure Init;
  function  Alloc:PQNodeSubmit;
  procedure Free(P:PQNodeSubmit);
 end;

 PQNodeFlip=^TQNodeFlip;
 TQNodeFlip=object(TQNode)
  submit   :PQNodeSubmit;
  submit_id:QWORD;
 end;

 TFlipAlloc=object
  FNodes:array[0..17] of TQNodeFlip;
  FAlloc:TIntrusiveMPSCQueue;
  Procedure Init;
  function  Alloc:PQNodeFlip;
  procedure Free(P:PQNodeFlip);
 end;

 TDisplayHandleSoft=class(TDisplayHandle)

  hWindow:THandle;

  FEvent:PRTLEvent;
  Ftd:p_kthread;

  FSubmitAlloc:TSubmitAlloc;
  FSubmitQueue:TIntrusiveMPSCQueue;

  FFlipAlloc:TFlipAlloc;

  FTerminate:Boolean;

  flip_rate:Integer;
  vblank_count:Integer;

  flipPendingNum:Integer;

  m_attr:array[0.. 3] of t_attr;
  m_bufs:array[0..15] of t_buffer;

  m_sbat:array[0.. 3] of t_attr;

  dst_cache:Pointer;

  Ffps     :QWORD;
  Ftsc_prev:QWORD;

  function   Open                     ():Integer; override;
  Destructor Destroy; override;
  //function  GetFlipStatus          (status:p_flip_status):Integer; virtual;
  //function  GetResolutionStatus    (status:p_resolution_status):Integer; virtual;
  function   SetFlipRate              (rate:Integer):Integer; override;
  function   RegisterBufferAttribute  (attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  function   SubmitBufferAttribute    (attrid:Byte;attr:p_register_buffer_attr):Integer; override;
  function   UnregisterBufferAttribute(attrid:Byte):Integer; override;
  function   RegisterBuffer           (buf:p_register_buffer):Integer; override;
  function   UnregisterBuffer         (index:Integer):Integer; override;
  function   SubmitFlip               (submit:p_submit_flip):Integer; override;
  function   SubmitFlipEop            (submit:p_submit_flip;submit_id:QWORD):Integer; override;
  function   TriggerFlipEop           (submit_id:QWORD):Integer; override;
  function   Vblank                   ():Integer; override;
  //
  procedure  OnSubmit(Node:PQNodeSubmit);
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
 sys_bootparam,
 kern_dmem,
 dev_dce;

//

Procedure TSubmitAlloc.Init;
var
 i:Integer;
begin
 FAlloc.Create;
 For i:=0 to High(FNodes) do
 begin
  FAlloc.Push(@FNodes[i]);
 end;
end;

function TSubmitAlloc.Alloc:PQNodeSubmit;
begin
 Result:=nil;
 FAlloc.Pop(Result);
end;

procedure TSubmitAlloc.Free(P:PQNodeSubmit);
begin
 if (P=nil) then Exit;
 FAlloc.Push(P);
end;

//

Procedure TFlipAlloc.Init;
var
 i:Integer;
begin
 FAlloc.Create;
 For i:=0 to High(FNodes) do
 begin
  FAlloc.Push(@FNodes[i]);
 end;
end;

function TFlipAlloc.Alloc:PQNodeFlip;
begin
 Result:=nil;
 FAlloc.Pop(Result);
end;

procedure TFlipAlloc.Free(P:PQNodeFlip);
begin
 if (P=nil) then Exit;
 FAlloc.Push(P);
end;

//

procedure dce_thread(parameter:pointer); SysV_ABI_CDecl; forward;

function TDisplayHandleSoft.Open():Integer;
begin
 Result:=inherited;

 Writeln('OpenMainWindows->');

 hWindow:=p_host_ipc.OpenMainWindows();

 Writeln('OpenMainWindows:',hWindow);

 FEvent:=RTLEventCreate;

 FSubmitAlloc.Init;
 FSubmitQueue.Create;

 FFlipAlloc.Init;

 if (Ftd=nil) then
 begin
  kthread_add(@dce_thread,Self,@Ftd,0,'[dce_soft]');
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
 FreeMem(dst_cache);
 inherited;
end;

function TDisplayHandleSoft.SetFlipRate(rate:Integer):Integer;
begin
 flip_rate:=rate;

 Result:=0;
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
 if (m_sbat[attrid].init<>0) then Exit(EBUSY);

 m_sbat[attrid].attr:=attr^;
 m_sbat[attrid].init:=1;

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

 left :Pointer;
 right:Pointer;
 left_dmem :Pointer;
 right_dmem:Pointer;
begin
 i:=buf^.index;
 a:=buf^.attrid;

 if (m_bufs[i].init<>0) then Exit(EINVAL);
 if (m_attr[a].init=0 ) then Exit(EINVAL);

 left :=buf^.left;
 right:=buf^.right;

 if (left=nil) then Exit(EINVAL);

left_dmem :=nil;
right_dmem:=nil;

 //TODO: check size!
 if not get_dmem_ptr(left,@left_dmem,nil) then
 begin
  Exit(EINVAL);
 end;

 if (right<>nil) then
 begin
  //TODO: check size!
  if not get_dmem_ptr(right,@right_dmem,nil) then
  begin
   Exit(EINVAL);
  end;
 end;

 m_bufs[i].init :=1;
 m_bufs[i].attr :=a;
 m_bufs[i].left :=left;
 m_bufs[i].right:=right;
 m_bufs[i].left_dmem :=left_dmem;
 m_bufs[i].right_dmem:=right_dmem;


 Result:=0;
end;

function TDisplayHandleSoft.UnregisterBuffer(index:Integer):Integer;
begin
 if (m_bufs[index].init=0) then Exit(EINVAL);

 m_bufs[index].init:=0;

 Result:=0;
end;

function getTiledElementByteOffset_32(PITCH,x,y:DWORD):QWORD;
var
 element_index:DWORD;
 pipe,bank:QWORD;
 total_offset:QWORD;
begin
 //getElementIndex [0..5]
 element_index:=                 ( (x      ) and 1) shl 2;
 element_index:=element_index or ( (x shr 1) and 1) shl 3;
 element_index:=element_index or ( (y      ) and 1) shl 4;
 element_index:=element_index or ( (x shr 2) and 1) shl 5;
 element_index:=element_index or ( (y shr 1) and 1) shl 6;
 element_index:=element_index or ( (y shr 2) and 1) shl 7;

 //getPipeIndex [6..8]
 pipe:=        ( ((x shr 3) xor (y shr 3) xor (x shr 4)) and 1) shl 8;
 pipe:=pipe or ( ((x shr 4) xor (y shr 4))               and 1) shl 9;
 pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and 1) shl 10;

 //getBankIndex [9..12]
 bank:=        ( ((x shr 6) xor (y shr 6))                and 1) shl 11;
 bank:=bank or ( ((x shr 7) xor (y shr 5)  xor (y shr 6)) and 1) shl 12;
 bank:=bank or ( ((x shr 8) xor (y shr 4))                and 1) shl 13;
 bank:=bank or ( ((x shr 9) xor (y shr 3))                and 1) shl 14;

 total_offset:=((y shr 6)*PITCH + (x shr 7));

 Result := element_index or pipe or bank or (total_offset shl 15)
end;


function getTiledElementByteOffset_32_NEO(PITCH,x,y:DWORD):QWORD;
var
 element_index:DWORD;
 pipe,bank:QWORD;
 total_offset:QWORD;
begin
 //getElementIndex [0..5]
 element_index:=                 ( (x      ) and 1) shl 2;
 element_index:=element_index or ( (x shr 1) and 1) shl 3;
 element_index:=element_index or ( (y      ) and 1) shl 4;
 element_index:=element_index or ( (x shr 2) and 1) shl 5;
 element_index:=element_index or ( (y shr 1) and 1) shl 6;
 element_index:=element_index or ( (y shr 2) and 1) shl 7;

 //getPipeIndex  [6..9]
 pipe:=        ( ((x shr 3) xor (y shr 3) xor (x shr 4)) and 1) shl 8;
 pipe:=pipe or ( ((x shr 4) xor (y shr 4))               and 1) shl 9;
 pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and 1) shl 10;
 pipe:=pipe or ( ((x shr 6) xor (y shr 5))               and 1) shl 11;

 //getBankIndex [10..12]
 bank:=        ( ((x shr 7) xor (y shr 6))                and 1) shl 12;
 bank:=bank or ( ((x shr 8) xor (y shr 5)  xor (y shr 6)) and 1) shl 13;
 bank:=bank or ( ((x shr 9) xor (y shr 4))                and 1) shl 14;

 total_offset:=
  ( (x shr 7)        shl 1)+
  (((y shr 7)*PITCH) shl 1)+
  ( (y shr 3)        and 1);

 Result := element_index or pipe or bank or (total_offset shl 15);
end;

type
 t_tile_offset_cb=function(PITCH,x,y:DWORD):QWORD;

procedure detile32bppBuf_slow(attr:p_attr;l_w:Ptrint;src,dst:Pointer);
var
 x,y:Ptrint;
 tiled_offset,linear_offset:Ptrint;

 linear_pitch:Ptrint;

 get_tile_offset:t_tile_offset_cb;

 PITCH:Ptrint;
begin
 if (p_neomode<>0) then
 begin
  get_tile_offset:=@getTiledElementByteOffset_32_NEO;
 end else
 begin
  get_tile_offset:=@getTiledElementByteOffset_32;
 end;

 PITCH:=(attr^.attr.pitchPixel+127) div 128;

 y:=0;
 While (y<attr^.attr.height) do
 begin
  linear_pitch:=y*l_w;

  x:=0;
  While (x<attr^.attr.width) do
  begin
   tiled_offset:=get_tile_offset(PITCH,x,y);

   linear_offset:=(x+linear_pitch)*4;
   PDWORD(dst+linear_offset)^:=PDWORD(src+tiled_offset)^;

   x:=x+1;
  end;

  y:=y+1;
 end;

end;

const
 kMicroTileWidth  = 8;
 kMicroTileHeight = 8;

//RCX RDX R8 R9
procedure detile32bppDisplay_AVX(dst:Pointer;destPitch:DWORD); assembler; nostackframe; MS_ABI_CDecl;
asm
 leal         (%rdx,%rdx,2), %eax
 leal         (    ,%rdx,8), %r8d
 leal         (    ,%rdx,4), %r9d
 sall         $2, %eax
 sall         $4, %edx
 vmovups          %xmm3,   (%rcx)
 vmovups          %xmm2, 16(%rcx)
 vextractf128 $1, %ymm3,   (%rcx,%r9)
 vextractf128 $1, %ymm2, 16(%rcx,%r9)
 vmovups          %xmm1,   (%rcx,%r8)
 vmovups          %xmm0, 16(%rcx,%r8)
 vextractf128 $1, %ymm1,   (%rcx,%rax)
 vextractf128 $1, %ymm0, 16(%rcx,%rax)

 leal         (%rdx, %rcx), %rcx
 vmovups          %xmm7,   (%rcx)
 vmovups          %xmm6, 16(%rcx)
 vextractf128 $1, %ymm7,   (%rcx,%r9)
 vextractf128 $1, %ymm6, 16(%rcx,%r9)
 vmovups          %xmm5,   (%rcx,%r8)
 vmovups          %xmm4, 16(%rcx,%r8)
 vextractf128 $1, %ymm5,   (%rcx,%rax)
 vextractf128 $1, %ymm4, 16(%rcx,%rax)
end;

procedure detile32bppBuf_AVX(attr:p_attr;l_w:Ptrint;src,dst:Pointer);
var
 x,y:Ptrint;
 tiled_offset,linear_offset:Ptrint;

 linear_pitch:Ptrint;

 get_tile_offset:t_tile_offset_cb;

 PITCH:Ptrint;

 ptr:Pointer;
begin
 if (p_neomode<>0) then
 begin
  get_tile_offset:=@getTiledElementByteOffset_32_NEO;
 end else
 begin
  get_tile_offset:=@getTiledElementByteOffset_32;
 end;

 PITCH:=(attr^.attr.pitchPixel+127) div 128;

 y:=0;
 While (y<attr^.attr.height) do
 begin
  linear_pitch:=y*l_w;

  x:=0;
  While (x<attr^.attr.width) do
  begin

   //cacheLine=0
   ptr:=(src+get_tile_offset(PITCH,x+0,y+0));
   asm
    movq        ptr , %rax
    vmovdqa  0(%rax), %ymm3
    vmovdqa 32(%rax), %ymm2
   end ['rax'];

   //cacheLine=1
   ptr:=(src+get_tile_offset(PITCH,x+0,y+2));
   asm
    movq        ptr , %rax
    vmovdqa  0(%rax), %ymm1
    vmovdqa 32(%rax), %ymm0
   end ['rax'];

   //cacheLine=2
   ptr:=(src+get_tile_offset(PITCH,x+0,y+4));
   asm
    movq        ptr , %rax
    vmovdqa  0(%rax), %ymm7
    vmovdqa 32(%rax), %ymm6
   end ['rax'];

   //cacheLine=3
   ptr:=(src+get_tile_offset(PITCH,x+0,y+6));
   asm
    movq        ptr , %rax
    vmovdqa  0(%rax), %ymm5
    vmovdqa 32(%rax), %ymm4
   end ['rax'];

   linear_offset:=(x+linear_pitch)*4;

   detile32bppDisplay_AVX(dst + linear_offset, l_w);

   x:=x+kMicroTileWidth;
  end;

  y:=y+kMicroTileHeight;
 end;

end;

procedure SoftFlip(hWindow:THandle;buf:p_buffer;attr:p_attr;p_dst:PPointer);
var
 hdc:THandle;
 bi:BITMAPINFO;
 yofs:Integer;
 rect:TRect;

 len:Ptrint;
 dst:Pointer;
begin
 hdc:=GetDC(hWindow);

 rect:=Default(TRect);
 GetClientRect(hWindow,rect);

 bi:=Default(BITMAPINFO);

 bi.bmiHeader.biSize       :=sizeof(bi.bmiHeader);
 bi.bmiHeader.biWidth      :=attr^.attr.pitchPixel;
 bi.bmiHeader.biHeight     :=attr^.attr.height;
 bi.bmiHeader.biPlanes     :=1;
 bi.bmiHeader.biBitCount   :=32;
 bi.bmiHeader.biCompression:=BI_RGB;

 if {(attr^.attr.tilingMode<>0)} false then
 begin
  //alloc aligned 128x128
  bi.bmiHeader.biWidth :=(attr^.attr.pitchPixel+127) and (not 127);
  bi.bmiHeader.biHeight:=(attr^.attr.height    +127) and (not 127);

  len:=bi.bmiHeader.biWidth*bi.bmiHeader.biHeight*4;

  if (p_dst^=nil) then
  begin
   p_dst^:=AllocMem(len);
  end else
  if (MemSize(p_dst^)<len) then
  begin
   p_dst^:=ReAllocMem(p_dst^,len);
  end;

  dst:=p_dst^;

  //detile32bppBuf_slow(attr,bi.bmiHeader.biWidth,buf^.left_dmem,dst);
  detile32bppBuf_AVX(attr,bi.bmiHeader.biWidth,buf^.left_dmem,dst);
 end else
 begin
  bi.bmiHeader.biWidth:=(bi.bmiHeader.biWidth+63) and (not 63);

  dst:=buf^.left_dmem;
 end;

 //flip
 yofs:=bi.bmiHeader.biHeight-attr^.attr.height;
 bi.bmiHeader.biHeight:=-bi.bmiHeader.biHeight;

 //SetStretchBltMode(hdc, HALFTONE);

 StretchDIBits(hdc,
               0,0,
               rect.Width, rect.Height,
               0,yofs,
               attr^.attr.width,
               attr^.attr.height,
               dst,
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
                  buf^.left_dmem, bi, DIB_RGB_COLORS);
 }

 ReleaseDC(hWindow, hdc);
end;

function TDisplayHandleSoft.SubmitFlip(submit:p_submit_flip):Integer;
var
 buf :p_buffer;
 attr:p_attr;
 Node:PQNodeSubmit;
begin
 if (submit^.bufferIndex<>-1) then
 begin
  buf:=@m_bufs[submit^.bufferIndex];
  if (buf^.init=0) then Exit(EINVAL);
  attr:=@m_attr[buf^.attr];
  if (attr^.init=0) then Exit(EINVAL);
 end;

 Node:=FSubmitAlloc.Alloc;
 if (Node=nil) then Exit(EBUSY);

 Node^.submit:=submit^;
 Node^.tsc   :=rdtsc();

 if (submit^.bufferIndex<>-1) then
 begin
  dce_page^.labels[submit^.bufferIndex]:=1;
 end;

 //
 System.InterlockedIncrement(last_status.flipPendingNum0);

 FSubmitQueue.Push(Node);

 if (Node^.submit.flipMode=SCE_VIDEO_OUT_FLIP_MODE_HSYNC) then
 begin
  RTLEventSetEvent(FEvent);
 end;
 //

 Result:=0;
end;

function TDisplayHandleSoft.SubmitFlipEop(submit:p_submit_flip;submit_id:QWORD):Integer;
var
 buf :p_buffer;
 attr:p_attr;
 Node:PQNodeSubmit;
 Flip:PQNodeFlip;
begin
 if (submit^.bufferIndex<>-1) then
 begin
  buf:=@m_bufs[submit^.bufferIndex];
  if (buf^.init=0) then Exit(EINVAL);
  attr:=@m_attr[buf^.attr];
  if (attr^.init=0) then Exit(EINVAL);
 end;

 Node:=FSubmitAlloc.Alloc;
 if (Node=nil) then Exit(EBUSY);

 Flip:=FFlipAlloc.Alloc;
 if (Flip=nil) then
 begin
  FSubmitAlloc.Free(Node);
  Exit(EBUSY);
 end;

 Node^.submit:=submit^;
 Node^.tsc   :=rdtsc();

 Flip^.next_    :=nil;
 Flip^.submit   :=Node;
 Flip^.submit_id:=submit_id;

 if (submit^.bufferIndex<>-1) then
 begin
  dce_page^.labels[submit^.bufferIndex]:=1;
 end;

 System.InterlockedIncrement(last_status.gcQueueNum);

 Result:=0;
end;

function TDisplayHandleSoft.TriggerFlipEop(submit_id:QWORD):Integer;
var
 Node:PQNodeSubmit;
 Flip:PQNodeFlip;
 i:Integer;
begin
 Result:=0;
 //
 For i:=0 to High(FFlipAlloc.FNodes) do
 begin
  Flip:=@FFlipAlloc.FNodes[i];

  if (Flip^.next_=nil) and
     (Flip^.submit<>nil) and
     (Flip^.submit_id=submit_id) then
  begin
   Node:=Flip^.submit;

   Flip^.submit   :=nil;
   Flip^.submit_id:=0;
   FFlipAlloc.Free(Flip);

   System.InterlockedDecrement(last_status.gcQueueNum);

   //
   System.InterlockedIncrement(last_status.flipPendingNum0);

   FSubmitQueue.Push(Node);

   if (Node^.submit.flipMode=SCE_VIDEO_OUT_FLIP_MODE_HSYNC) then
   begin
    RTLEventSetEvent(FEvent);
   end;
   //

   Exit;
  end;

 end;
 //
 Result:=1;
end;

function TDisplayHandleSoft.Vblank():Integer;
begin
 vblank_count:=vblank_count+1;

 if (vblank_count>flip_rate) then
 begin
  vblank_count:=0;
  RTLEventSetEvent(FEvent);
 end;

 Result:=0;
end;

procedure TDisplayHandleSoft.OnSubmit(Node:PQNodeSubmit);
var
 i:Integer;
 submit:p_submit_flip;
 buf :p_buffer;
 attr:p_attr;
begin
 //submit attr
 For i:=0 to High(m_sbat) do
 if (m_sbat[i].init<>0) then
 begin
  mtx_lock(mtx^);

   m_attr[i].init:=1;
   m_attr[i].attr:=m_sbat[i].attr;

   m_sbat[i].init:=0;

  mtx_unlock(mtx^);
 end;

 submit:=@Node^.submit;

 if (submit^.bufferIndex<>-1) then
 begin
  buf:=@m_bufs[submit^.bufferIndex];
  if (buf^.init=0) then Exit;
  attr:=@m_attr[buf^.attr];
  if (attr^.init=0) then Exit;

  SoftFlip(hWindow,buf,attr,@dst_cache);
 end;

 mtx_lock(mtx^);

  if (submit^.bufferIndex<>-1) then
  begin
   dce_page^.labels[submit^.bufferIndex]:=0;
  end;
  dce_page^.label_:=0;

  System.InterlockedDecrement(last_status.flipPendingNum0);

  last_status.flipArg        :=submit^.flipArg;
  last_status.flipArg2       :=submit^.flipArg2;
  last_status.count          :=last_status.count+1;
  last_status.submitTsc      :=Node^.tsc;
  last_status.currentBuffer  :=submit^.bufferIndex;
  last_status.tsc            :=rdtsc();
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
   p_host_ipc.SetCaptionFPS(Ffps);
   Ffps:=0;
   Ftsc_prev:=last_status.tsc;
  end;
 end;

end;

procedure dce_thread(parameter:pointer); SysV_ABI_CDecl;
var
 dce:TDisplayHandleSoft;
 Node:PQNodeSubmit;
begin
 dce:=TDisplayHandleSoft(parameter);

 repeat
  RTLEventWaitFor(dce.FEvent);

  Node:=nil;
  if dce.FSubmitQueue.Pop(Node) then
  begin
   dce.OnSubmit(Node);
   dce.FSubmitAlloc.Free(Node);
  end;

 until dce.FTerminate;

end;


end.




