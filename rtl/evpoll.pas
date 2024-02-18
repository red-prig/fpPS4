{ multithread IO event poll (so far only windows)

  Copyright (C) 2018-2023 Red_prig

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
}

unit evpoll;

{//$DEFINE NO_WATERMARKS}
{//$DEFINE NO_RATELIMIT}
{$mode objfpc}{$H+}

interface

Uses
 atomic,
 evbuffer,
 ntapi,
 Windows,
 sysutils,
 WinSock2;

Const
 BEV_EVENT_READING     =$01;
 BEV_EVENT_WRITING     =$02;
 BEV_EVENT_EOF         =$10;
 BEV_EVENT_ERROR       =$20;
 BEV_EVENT_TIMEOUT     =$40;
 BEV_EVENT_CONNECTED   =$80;

 BEV_EVENT_EOE=BEV_EVENT_ERROR or BEV_EVENT_EOF;

type
 TWaterMark=record
  lo,hi:SizeUInt;
 end;

 PWaterMarks=^TWaterMarks;
 TWaterMarks=record
  RD,WR:TWaterMark;
 end;

 Pevpoll_config=^Tevpoll_config;
 Tevpoll_config=record
  FReadBufSize:SizeUInt;
  FWaterMarks:TWaterMarks;
  Finitcb,
  Ffinicb:TProcedure;
 end;

 Pevpoll=^Tevpoll;
 Tevpoll=object
  private
   hIOCP:Thandle;
   FState:SizeUInt;
   FRefs:SizeUInt;
   Ffunc:tthreadfunc;
   Fcfg:Tevpoll_config;
 end;

{
 len: [1|1|15|15]
 [
  len :SizeUInt;
  pos :SizeUInt;
  vec_free:TfuncFree;
  base:Pointer;
  [buf_free:TfuncFree;]
 ]

 [
  : [1|1|len|pos]
  [vec_free:TfuncFree;]
  //data of len
 ]

}

 Ptimer=^Ttimer;
 Ttimer_cb=Procedure(ev:Ptimer;arg:pointer);

 Ttimer=object
  private
   Fbase:Pevpoll;
   FHandle:THandle;
   FState:SizeUInt;
   FCb:Ttimer_cb;
   FPtr:Pointer;
   Ftime:Int64;
 end;

 Prate_limit_group=^Trate_limit_group;
 Trate_limit_group=object
  public
   Fspeed_r:SizeUint; //b/sec
   Fspeed_w:SizeUint; //b/sec
  private
   Ftm_rec:SizeUint;  //last time
   Fsp_cpl_r:SizeUint;
   Fsp_cpl_w:SizeUint;
 end;

 Prate_limit=^Trate_limit;
 Trate_limit=record
  Fgroup:Prate_limit_group;
  Fr_timer:Ptimer;
  Fw_timer:Ptimer;
 end;

 PBufferevent=^TBufferevent;

 TBufferevent_eventcb=Procedure(bev:Pbufferevent;events:SizeUInt;ctx:pointer);

 t_be_ops_cb=function(bufferevent:Pbufferevent;ctrl_data:Pointer):Boolean;

 Pbufferevent_ops=^Tbufferevent_ops;
 Tbufferevent_ops=packed record
  BEV_CTRL_ENABLE :t_be_ops_cb;
  BEV_CTRL_DISABLE:t_be_ops_cb;
  BEV_CTRL_CONNECT:t_be_ops_cb;
  BEV_CTRL_FREE   :t_be_ops_cb;
  BEV_CTRL_CLEAN  :t_be_ops_cb;
  BEV_CTRL_READ   :t_be_ops_cb;
  BEV_CTRL_WRITE  :t_be_ops_cb;
  BEV_CTRL_TRIGGER:t_be_ops_cb;
  BEV_CTRL_EVENT  :t_be_ops_cb;
  BEV_CTRL_POST   :t_be_ops_cb;
  BEV_CTRL_GET_IE :t_be_ops_cb;
  BEV_CTRL_GET_OE :t_be_ops_cb;
  {$IFNDEF NO_RATELIMIT}
  BEV_CTRL_GET_RL :t_be_ops_cb;
  BEV_CTRL_SET_RL :t_be_ops_cb;
  {$ENDIF}
  {$IFNDEF NO_WATERMARKS}
  BEV_CTRL_GET_WM :t_be_ops_cb;
  BEV_CTRL_SET_WM :t_be_ops_cb;
  {$ENDIF}
  BEV_CTRL_SET_RBS:t_be_ops_cb;
  BEV_CTRL_SET_PA :t_be_ops_cb;
  BEV_CTRL_GET_PA :t_be_ops_cb;
 end;

 PHandle_ops=^THandle_ops;
 THandle_ops=packed record
  H_RECV    :t_be_ops_cb;
  H_PEEK    :t_be_ops_cb;
  H_SEND    :t_be_ops_cb;
  H_RCVBUF  :t_be_ops_cb;
  H_NOBTR   :t_be_ops_cb;
  H_SHUTDOWN:t_be_ops_cb;
  H_CLOSE   :t_be_ops_cb;
 end;

 PSockInfo=^TSockInfo;
 TSockInfo=record
  sa:Psockaddr;
  len:SizeUInt;
 end;

 TBufferevent=object
  protected
   be_ops:Pbufferevent_ops;
   he_ops:PHandle_ops;
   Fbase:Pevpoll;
  private
   FHandle:THandle;
   Fctx:pointer;
   Feventcb:TBufferevent_eventcb;
   FRefCount:SizeUInt;
   Felock:SizeUInt;
   Fclock:Pointer;
 end;

const
 AEX_AL=SizeOf(TSockAddrIn6)+16;
 AEX_BUF_SIZE=AEX_AL*2;

type
 Pevconnlistener=^Tevconnlistener;

 Tevconnlistener_cb=procedure(listener:Pevconnlistener;fd:THandle;sa:Psockaddr;socklen:SizeUInt;ptr:pointer);

 Tevconnlistener=object
  private
   type
    Plistener_acceptex=^Tlistener_acceptex;
    Tlistener_acceptex=record
     O:TOVERLAPPED;
     FHandle:THandle;
     FListener:Pevconnlistener;
     Buf:array[0..AEX_BUF_SIZE-1] of Byte;
    end;
   var
    Fbase:Pevpoll;
    FHandle:THandle;
    FPtr:Pointer;
    Fcb:Tevconnlistener_cb;
    FState:SizeUInt;
    FNew:Tlistener_acceptex;
 end;

 PBufferevent_sio=^TBufferevent_sio;
 TBufferevent_sio=object(TBufferevent)
  private
   type
    PWSA_iovec=^TWSA_iovec;
    TWSA_iovec=object
     private
      O:TOVERLAPPED;
      P:PBufferevent;
      Buf:Pointer;
     public
    end;
   var
    F_read_buf_size:SizeUInt;

    Frlock:SizeUInt;
    Fwlock:SizeUInt;

    FRBUF,FWBUF:Tevbuffer;

    FRD_WSA,FWR_WSA:TWSA_iovec;

    {$IFNDEF NO_RATELIMIT}
    Frate_limit:Prate_limit;
    {$ENDIF}
    {$IFNDEF NO_WATERMARKS}
    FWaterMarks:PWaterMarks;
    {$ENDIF}

  public
 end;

 PBufferevent_sio_pair=^TBufferevent_sio_pair;
 TBufferevent_sio_pair=object(TBufferevent_sio)
  FPair:PBufferevent_sio;
 end;

 Pevpoll_post_cb=Procedure(param1:SizeUInt;param2:Pointer);

function  calc_effective_align_mem(min,max:SizeUInt):SizeUInt;

function  evpoll_init(base:Pevpoll;cfg:Pevpoll_config):Boolean;
function  evpoll_loop(base:Pevpoll):Boolean;
function  evpoll_break(base:Pevpoll):Boolean;
function  evpoll_free(base:Pevpoll):Boolean;
function  evpoll_isinit(base:Pevpoll):Boolean;
function  evpoll_cfg(base:Pevpoll):Pevpoll_config;
function  evpoll_post(base:Pevpoll;cb:Pevpoll_post_cb;param1:SizeUInt;param2:Pointer):Boolean;

function  bufferevent_socket_new(base:Pevpoll;fd:THandle):Pbufferevent;
function  bufferevent_pipe_new(base:Pevpoll;fd:THandle):Pbufferevent;
function  bufferevent_socket_connect(bev:Pbufferevent;sa:Psockaddr;socklen:SizeUInt):Boolean;
function  bufferevent_socket_connect_hostname(bev:Pbufferevent;family:Integer;hostname:PAnsiChar;port:Word):Boolean;
function  bufferevent_get_fd(bev:Pbufferevent):THandle;
function  bufferevent_close(bev:Pbufferevent):Boolean;
function  bufferevent_shutdown(bev:Pbufferevent;how:Longint):Boolean;
function  bufferevent_free(bev:Pbufferevent):Boolean;
function  bufferevent_get_input(bev:Pbufferevent):Pevbuffer;
function  bufferevent_get_output(bev:Pbufferevent):Pevbuffer;
Procedure bufferevent_setcb(bev:Pbufferevent;eventcb:TBufferevent_eventcb;cbarg:pointer);
function  bufferevent_enable(bev:Pbufferevent):Boolean;
function  bufferevent_disable(bev:Pbufferevent):Boolean;
function  bufferevent_write(bev:Pbufferevent):Boolean;
function  bufferevent_read(bev:Pbufferevent):Boolean;

function  bufferevent_set_rate_limit(bev:Pbufferevent;rg:Prate_limit_group):boolean;
function  bufferevent_get_rate_limit(bev:Pbufferevent):Prate_limit_group;
function  bufferevent_set_watermarks(bev:Pbufferevent;wm:PWaterMarks):boolean;
function  bufferevent_set_read_buf_size(bev:Pbufferevent;New:SizeUInt):boolean;

function  _bufferevent_sio_new(base:Pevpoll;fd:THandle;fd_ops:PHandle_ops;size:SizeUInt):Pbufferevent;

function  bufferevent_socket_pair_new(base:Pevpoll;fd:THandle;pair:Pbufferevent):Pbufferevent;
function  bufferevent_set_pair(bev,new_pair:PBufferevent):Boolean;
function  bufferevent_get_pair(bev:PBufferevent):PBufferevent;

function  bufferevent_inc_ref(bev:Pbufferevent):Boolean;
function  bufferevent_dec_ref(bev:Pbufferevent):Boolean;

procedure evconnlistener_free(lev:Pevconnlistener);
function  evconnlistener_new_bind(base:Pevpoll;cb:Tevconnlistener_cb;ptr:Pointer;Reusable:Boolean;backlog:SizeUInt;sa:Psockaddr;socklen:SizeUInt):Pevconnlistener;
function  evconnlistener_get_fd(lev:Pevconnlistener):THandle;
function  evconnlistener_enable(lev:Pevconnlistener):Boolean;
function  evconnlistener_disable(lev:Pevconnlistener):Boolean;
procedure evconnlistener_set_cb(lev:Pevconnlistener;cb:Tevconnlistener_cb;ptr:Pointer);

function  evtimer_new(base:Pevpoll;cb:Ttimer_cb;arg:pointer):Ptimer;
function  evtimer_set_cb(ev:Ptimer;cb:Ttimer_cb;arg:pointer):Boolean;
function  evtimer_add(ev:Ptimer;tv:Ptimeval):Boolean;
function  evtimer_add(ev:Ptimer;us:Int64):Boolean;
function  evtimer_del(ev:Ptimer):Boolean;
function  evtimer_reuse(var ev:Ptimer;base:Pevpoll;cb:Ttimer_cb;arg:pointer):Boolean;

Procedure SetKeepAlive(fd:THandle;Enable:Boolean;idle,int,cnt:dword);

//

Function sock_recv(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function sock_peek(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function sock_send(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function sock_rcvbuf(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function sock_nobtr(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function sock_shutdown(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function sock_close(bev:Pbufferevent;ctrl_data:Pointer):Boolean;

const
 sock_ops:THandle_ops=(
  H_RECV    :@sock_recv;
  H_PEEK    :@sock_peek;
  H_SEND    :@sock_send;
  H_RCVBUF  :@sock_rcvbuf;
  H_NOBTR   :@sock_nobtr;
  H_SHUTDOWN:@sock_shutdown;
  H_CLOSE   :@sock_close;
 );

//

Function pipe_recv(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function pipe_peek(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function pipe_send(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function pipe_rcvbuf(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Function pipe_nobtr(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function pipe_shutdown(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function pipe_close(bev:Pbufferevent;ctrl_data:Pointer):Boolean;

const
 pipe_ops:THandle_ops=(
  H_RECV    :@pipe_recv;
  H_PEEK    :@pipe_peek;
  H_SEND    :@pipe_send;
  H_RCVBUF  :@pipe_rcvbuf;
  H_NOBTR   :@pipe_nobtr;
  H_SHUTDOWN:@pipe_shutdown;
  H_CLOSE   :@pipe_close;
 );

//

function  be_ops_sio_enable(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_sio_clean(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_sio_disable(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_sio_connect(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_false(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_sio_read(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_sio_write(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_trigger(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_event(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_post(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_get_ie(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_get_oe(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
{$IFNDEF NO_RATELIMIT}
function  be_ops_get_rl(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_set_rl(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
{$ENDIF}
{$IFNDEF NO_WATERMARKS}
function  be_ops_get_wm(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function  be_ops_set_wm(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
{$ENDIF}
function  be_ops_set_rbs(bev:Pbufferevent;ctrl_data:Pointer):Boolean;

const
 buf_sio_ops:Tbufferevent_ops=(
  BEV_CTRL_ENABLE :@be_ops_sio_enable;
  BEV_CTRL_DISABLE:@be_ops_sio_disable;
  BEV_CTRL_CONNECT:@be_ops_sio_connect;
  BEV_CTRL_FREE   :@be_ops_false;
  BEV_CTRL_CLEAN  :@be_ops_sio_clean;
  BEV_CTRL_READ   :@be_ops_sio_read;
  BEV_CTRL_WRITE  :@be_ops_sio_write;
  BEV_CTRL_TRIGGER:@be_ops_trigger;
  BEV_CTRL_EVENT  :@be_ops_event;
  BEV_CTRL_POST   :@be_ops_post;
  BEV_CTRL_GET_IE :@be_ops_get_ie;
  BEV_CTRL_GET_OE :@be_ops_get_oe;
  {$IFNDEF NO_RATELIMIT}
  BEV_CTRL_GET_RL :@be_ops_get_rl;
  BEV_CTRL_SET_RL :@be_ops_set_rl;
  {$ENDIF}
  {$IFNDEF NO_WATERMARKS}
  BEV_CTRL_GET_WM :@be_ops_get_wm;
  BEV_CTRL_SET_WM :@be_ops_set_wm;
  {$ENDIF}
  BEV_CTRL_SET_RBS:@be_ops_set_rbs;
  BEV_CTRL_SET_PA :@be_ops_false;
  BEV_CTRL_GET_PA :@be_ops_false;
 );

function be_ops_pair_trigger(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function be_ops_pair_event(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function be_ops_pair_set_pa(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function be_ops_pair_get_pa(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
function be_ops_pair_free(bev:Pbufferevent;ctrl_data:Pointer):Boolean;

const
 buf_sio_pair_ops:Tbufferevent_ops=(
  BEV_CTRL_ENABLE :@be_ops_sio_enable;
  BEV_CTRL_DISABLE:@be_ops_sio_disable;
  BEV_CTRL_CONNECT:@be_ops_sio_connect;
  BEV_CTRL_FREE   :@be_ops_pair_free;
  BEV_CTRL_CLEAN  :@be_ops_sio_clean;
  BEV_CTRL_READ   :@be_ops_sio_read;
  BEV_CTRL_WRITE  :@be_ops_sio_write;
  BEV_CTRL_TRIGGER:@be_ops_pair_trigger;
  BEV_CTRL_EVENT  :@be_ops_pair_event;
  BEV_CTRL_POST   :@be_ops_post;
  BEV_CTRL_GET_IE :@be_ops_get_ie;
  BEV_CTRL_GET_OE :@be_ops_get_oe;
  {$IFNDEF NO_RATELIMIT}
  BEV_CTRL_GET_RL :@be_ops_get_rl;
  BEV_CTRL_SET_RL :@be_ops_set_rl;
  {$ENDIF}
  {$IFNDEF NO_WATERMARKS}
  BEV_CTRL_GET_WM :@be_ops_get_wm;
  BEV_CTRL_SET_WM :@be_ops_set_wm;
  {$ENDIF}
  BEV_CTRL_SET_RBS:@be_ops_set_rbs;
  BEV_CTRL_SET_PA :@be_ops_pair_set_pa;
  BEV_CTRL_GET_PA :@be_ops_pair_get_pa;
 );

implementation

function spin_trylock(Var P:Pointer):Boolean; inline;
begin
 Result:=XCHG(P,Pointer(1))=nil;
end;

function spin_tryunlock(Var P:Pointer):Boolean; inline;
begin
 Result:=XCHG(P,nil)=Pointer(1);
end;

procedure spin_lock(Var P:Pointer); inline;
begin
 While (XCHG(P,Pointer(1))<>nil) do System.ThreadSwitch;
end;

procedure spin_unlock(Var P:Pointer); inline;
begin
 store_release(P,nil);
end;

Const
 LST_DISABLE=0;
 LST_ENABLE =1;
 LST_ACCEPT =2;
 LST_CLOSED =3;
 LST_FREE   =4;

 EL_DIS=0; //disable
 EL_BEN=1; //begin enable
 EL_ENB=2; //enable
 EL_BDI=3; //begin disable
 EL_BCN=4; //begin connect

 ET_NEW=0;
 ET_PST=1;
 ET_WTT=2;
 ET_DEL=3;

 WL_DIS=0; //disable write
 WL_NWR=1; //need write
 WL_ENB=2; //enable write

 min_rbs={64}512;
 max_rbs=16*1024;

type
 Plistener_acceptex=Tevconnlistener.Plistener_acceptex;
 PWSA_iovec=TBufferevent_sio.PWSA_iovec;
 PCTXProc=Procedure(NOBT:SizeUInt;Data:POVERLAPPED);

const

 WSAID_ACCEPTEX:TGUID=(Data1:$b5367df1;
                       Data2:$cbac;
                       Data3:$11cf;
                       Data4:($95,$ca,$00,$80,$5f,$48,$a1,$92));

 WSAID_GETACCEPTEXSOCKADDRS:TGUID=(
                       Data1:$b5367df2;
                       Data2:$cbac;
                       Data3:$11cf;
                       Data4:($95,$ca,$00,$80,$5f,$48,$a1,$92));

 WSAID_CONNECTEX:TGUID=(
                       Data1:$25a207b9;
                       Data2:$ddf3;
                       Data3:$4660;
                       Data4:($8e,$e9,$76,$e5,$8c,$74,$06,$3e));



type
 LPFN_AcceptEx=function(sListenSocket,sAcceptSocket:THandle;
                        lpOutputBuffer:Pointer;
                        dwReceiveDataLength,
                        dwLocalAddressLength,
                        dwRemoteAddressLength:DWORD;
                        lpdwBytesReceived:LPDWORD;
                        Overlapped:LPOVERLAPPED):BOOL; stdcall;

 LPFN_GetAcceptExSockaddrs=procedure(
                        lpOutputBuffer:Pointer;
                        dwReceiveDataLength,
                        dwLocalAddressLength,
                        dwRemoteAddressLength:DWORD;
                    var LocalSockaddr:PSOCKADDR;
                    var LocalSockaddrLength:Integer;
                    var RemoteSockaddr:PSOCKADDR;
                    var RemoteSockaddrLength:Integer); stdcall;

 LPFN_ConnectEx=function(s:THandle;
                         name:PSOCKADDR;
                         namelen:Integer;
                         lpSendBuffer:Pointer;
                         dwSendDataLength:DWORD;
                         lpdwBytesSent:PDWORD;
                         Overlapped:LPOVERLAPPED):BOOL; stdcall;

 LPFN_CancelIoEx=function(hfile:THandle;overlapped:LPOverlapped):WINBOOL; stdcall;

 TOVERLAPPED_ENTRY=record
  lpCompletionKey:ULONG_PTR;
  lpOverlapped:POverlapped;
  Internal:ULONG_PTR;
  dwNumberOfBytesTransferred:DWORD;
 end;
 POVERLAPPED_ENTRY=^TOVERLAPPED_ENTRY;

 LPFN_GetQueuedCompletionStatusEX=function(CompletionPort:THandle;
                                           lpCompletionPortEntries:POVERLAPPED_ENTRY;
                                           ulCount:ULONG;
                                           var ulNumEntriesRemoved:ULONG;
                                           dwMilliseconds:DWORD;
                                           fAlertable:BOOL):BOOL; stdcall;

 PADDRINFOA=^TaddrinfoA;
 TaddrinfoA=packed record
  ai_flags     :Integer;
  ai_family    :Integer;
  ai_socktype  :Integer;
  ai_protocol  :Integer;
  ai_addrlen   :size_t;
  ai_canonname :PAnsiChar;
  ai_addr      :Psockaddr;
  ai_next      :PADDRINFOA;
 end;

function getaddrinfo(
          pNodeName,pServiceName:PAnsiChar;
          pHints:PADDRINFOA;
          var ppResult:PADDRINFOA
         ):Integer; stdcall; external WINSOCK2_DLL;

Procedure freeaddrinfo(pAddrInfo:PADDRINFOA); stdcall; external WINSOCK2_DLL;


Var
 _AcceptEx:LPFN_AcceptEx=nil;
 _GetAcceptExSockaddrs:LPFN_GetAcceptExSockaddrs=nil;
 _ConnectEx:LPFN_ConnectEx=nil;
 _CancelIoEx:LPFN_CancelIoEx=nil;
 _GetQueuedCompletionStatusEX:LPFN_GetQueuedCompletionStatusEX=nil;

Function getsock_family(FHandle:THandle):Word;
var
 sa:TSockAddrIn6;
 socklen:integer;
begin
 Result:=0;
 socklen:=SizeOf(TSockAddrIn6);

 if getsockname(FHandle,PSockAddr(@sa)^,socklen)=0 then
 begin
  Result:=sa.sin6_family;
 end;

end;

function Support_AcceptEx(sListenSocket:THandle):Boolean;
Var
 Flag:DWORD;
begin
 Result:=false;
 if Pointer(_AcceptEx)=Pointer(1) then
 begin
  Exit
 end;
 if _AcceptEx=nil then
 begin
  Flag:=1;
  if WSAIoctl(sListenSocket,SIO_GET_EXTENSION_FUNCTION_POINTER,
             @WSAID_ACCEPTEX,SizeOf(TGUID),
             @_AcceptEx,SizeOf(Pointer),
             @Flag,nil,nil)=SOCKET_ERROR then
  begin
   Pointer(_AcceptEx):=Pointer(1);
   Exit;
  end;
 end;
 Result:=True;
end;

function AcceptEx(sListenSocket,sAcceptSocket:THandle;
                  lpOutputBuffer:Pointer;
                  dwReceiveDataLength:DWORD;
                  Overlapped:LPOVERLAPPED):Boolean;
begin
 Result:=Support_AcceptEx(sListenSocket);
 if Result then
 begin
  _AcceptEx(sListenSocket,sAcceptSocket,
            lpOutputBuffer,
            dwReceiveDataLength,AEX_AL,AEX_AL,nil,
            Overlapped);
 end;
end;

function Support_GetAcceptExSockaddrs(hSocket:THandle):Boolean;
Var
 Flag:DWORD;
begin
 Result:=false;
 if Pointer(_GetAcceptExSockaddrs)=Pointer(1) then
 begin
  Exit
 end;
 if _GetAcceptExSockaddrs=nil then
 begin
  Flag:=1;
  if WSAIoctl(hSocket,SIO_GET_EXTENSION_FUNCTION_POINTER,
             @WSAID_GETACCEPTEXSOCKADDRS,SizeOf(TGUID),
             @_GetAcceptExSockaddrs,SizeOf(Pointer),
             @Flag,nil,nil)=SOCKET_ERROR then
  begin
   Pointer(_GetAcceptExSockaddrs):=Pointer(1);
   Exit;
  end;
 end;
 Result:=True;
end;

function GetAcceptExSockaddrs(hSocket:THandle;lpOutputBuffer:Pointer;
                              dwReceiveDataLength:DWORD;
                              var LocalSockaddr:PSOCKADDR;
                              var LocalSockaddrLength:Integer;
                              var RemoteSockaddr:PSOCKADDR;
                              var RemoteSockaddrLength:Integer):Boolean;
begin
 Result:=Support_GetAcceptExSockaddrs(hSocket);
 if Result then
 begin
  _GetAcceptExSockaddrs(lpOutputBuffer,
       dwReceiveDataLength,AEX_AL,AEX_AL,
       LocalSockaddr ,LocalSockaddrLength,
       RemoteSockaddr,RemoteSockaddrLength);
 end;
end;

function Support_ConnectEx(hSocket:THandle):Boolean;
Var
 Flag:DWORD;
begin
 Result:=false;
 if Pointer(_ConnectEx)=Pointer(1) then
 begin
  Exit
 end;
 if _ConnectEx=nil then
 begin
  Flag:=1;
  if WSAIoctl(hSocket,SIO_GET_EXTENSION_FUNCTION_POINTER,
             @WSAID_CONNECTEX,SizeOf(TGUID),
             @_ConnectEx,SizeOf(Pointer),
             @Flag,nil,nil)=SOCKET_ERROR then
  begin
   Pointer(_ConnectEx):=Pointer(1);
   Exit;
  end;
 end;
 Result:=True;
end;

function ConnectEx(hSocket:THandle;
                   name:PSOCKADDR;
                   namele:Integer;
                   Overlapped:LPOVERLAPPED):Boolean;
begin
 Result:=Support_ConnectEx(hSocket);
 if Result then
  _ConnectEx(hSocket,name,namele,nil,0,nil,Overlapped);
end;


function Support_CancelIoEx:Boolean;
begin
 Result:=True;
 if Pointer(_CancelIoEx)=Pointer(1) then
 begin
  Result:=false;
  Exit
 end;
 if _CancelIoEx=nil then
 begin
  Pointer(_CancelIoEx):=GetProcAddress(GetModuleHandle(kernel32),'CancelIoEx');
  if _CancelIoEx=nil then
  begin
   Pointer(_CancelIoEx):=Pointer(1);
   Result:=false;
   Exit;
  end;
 end;
end;

function CancelIoEx(hfile:THandle;overlapped:LPOverlapped):Boolean;
begin
 Result:=Support_CancelIoEx;
 if Result then
  Result:=_CancelIoEx(hfile,overlapped);
end;

function Support_CompletionEx:Boolean;
begin
 Result:=True;
 if Pointer(_GetQueuedCompletionStatusEX)=Pointer(1) then
 begin
  Result:=false;
  Exit
 end;
 if _GetQueuedCompletionStatusEX=nil then
 begin
  Pointer(_GetQueuedCompletionStatusEX):=GetProcAddress(GetModuleHandle(kernel32),'GetQueuedCompletionStatusEx');
  if _GetQueuedCompletionStatusEX=nil then
  begin
   Pointer(_GetQueuedCompletionStatusEX):=Pointer(1);
   Result:=false;
   Exit;
  end;
 end;
end;

function NtGetOverlappedError(lpOverlapped:POVERLAPPED):DWORD; inline;
begin
 Result:=PDWORD(lpOverlapped)^;
end;

///////

Function sock_recv(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 len:SizeUInt;
 msg:SizeUInt;
 FLAGR,NOBTR,R:DWORD;
 iovec:WSABUF;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 with PBufferevent_sio(bev)^ do
 begin
  len:=PSizeUInt(ctrl_data)[0];
  msg:=PSizeUInt(ctrl_data)[1];

  FLAGR:=0;
  NOBTR:=0;

  iovec.len:=len;
  iovec.buf:=FRD_WSA.Buf;

  if (msg<>0) then
  begin
   R:=WSARecv(bev^.FHandle,@iovec,1,NOBTR,FLAGR,@FRD_WSA,nil);
  end else
  begin
   R:=WSARecv(bev^.FHandle,@iovec,1,NOBTR,FLAGR,nil,nil);
  end;

  if (R<>0) then
  begin
   R:=WSAGetLastError();
   if (R=WSA_IO_PENDING) then R:=0;
  end;
  PSizeUInt(ctrl_data)[0]:=NOBTR;
  Result:=(R=0);
 end;
end;

Function sock_peek(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 FLAGR,NOBTR,R:DWORD;
 iovec:WSABUF;
begin
 with PBufferevent_sio(bev)^ do
 begin
  FLAGR:=MSG_PEEK;
  NOBTR:=0;
  iovec:=Default(WSABUF);
  R:=WSARecv(bev^.FHandle,@iovec,1,NOBTR,FLAGR,@FRD_WSA,nil);
  if (R<>0) then
  begin
   R:=WSAGetLastError();
   if (R=WSA_IO_PENDING) then R:=0;
  end;
  Result:=(R=0);
 end;
end;

function evbuffer_wsa_copy(buf:Pevbuffer;size:SizeUInt;var P:PWSABUF):SizeUint; forward;

Function sock_send(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 len:SizeUInt;
 FLAGW,R:DWORD;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 with PBufferevent_sio(bev)^ do
 begin
  len:=PSizeUInt(ctrl_data)^;
  len:=evbuffer_wsa_copy(@FWBUF,len,PWSABUF(FWR_WSA.Buf));
  FLAGW:=0;
  R:=WSASend(bev^.FHandle,PWSABUF(FWR_WSA.buf),len,PDWORD(nil)^,FLAGW,@FWR_WSA,nil);
  if (R<>0) then
  begin
   R:=WSAGetLastError();
   if (R=WSA_IO_PENDING) then R:=0;
  end;
  Result:=(R=0);
 end;
end;

Function sock_rcvbuf(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 NOBT,Len:Longint;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  NOBT:=0;
  Len:=SizeOf(DWORD);
  getsockopt(bev^.FHandle,SOL_SOCKET,SO_RCVBUF,NOBT,Len);

  PSizeUInt(ctrl_data)^:=NOBT;
 end;
end;

Function sock_nobtr(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
var
 NOBT:DWORD;
 Flag:DWORD;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  NOBT:=0;
  Flag:=0;

  Result:=WSAIoctl(bev^.FHandle,FIONREAD,
                   @NOBT,SizeOf(NOBT),
                   @NOBT,SizeOf(NOBT),
                   @Flag,nil,nil)<>0;

  PSizeUInt(ctrl_data)^:=NOBT;
 end;
end;

function sock_shutdown(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=shutdown(bev^.FHandle,ptruint(ctrl_data))=0;
end;

function sock_close(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
var
 h:THandle;
begin
 h:=XCHG(bev^.FHandle,INVALID_HANDLE_VALUE);
 if (h<>INVALID_HANDLE_VALUE) then
 begin
  Result:=(CloseSocket(h)=0);
 end;
end;

///////


Function pipe_recv(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 len:SizeUInt;
 msg:SizeUInt;
 OFFSET:Int64;
 R:DWORD;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 with PBufferevent_sio(bev)^ do
 begin
  len:=PSizeUInt(ctrl_data)[0];
  msg:=PSizeUInt(ctrl_data)[1];

  OFFSET:=0;

  PIO_STATUS_BLOCK(@FRD_WSA)^.Status     :=STATUS_PENDING;
  PIO_STATUS_BLOCK(@FRD_WSA)^.Information:=0;

  if (msg<>0) then
  begin
   R:=NtReadFile(bev^.FHandle,0,nil,@FRD_WSA,@FRD_WSA,FRD_WSA.Buf,len,@OFFSET,nil);
  end else
  begin
   R:=NtReadFile(bev^.FHandle,0,nil,nil,@FRD_WSA,FRD_WSA.Buf,len,@OFFSET,nil);
  end;

  PSizeUInt(ctrl_data)^:=PIO_STATUS_BLOCK(@FRD_WSA)^.Information;
  Result:=(R=0) or (R=STATUS_PENDING);
 end;
end;

Function pipe_peek(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 OFFSET:Int64;
 R:DWORD;
begin
 with PBufferevent_sio(bev)^ do
 begin
  OFFSET:=0;
  PIO_STATUS_BLOCK(@FRD_WSA)^.Status     :=STATUS_PENDING;
  PIO_STATUS_BLOCK(@FRD_WSA)^.Information:=0;
  R:=NtReadFile(bev^.FHandle,0,nil,@FRD_WSA,@FRD_WSA,nil,0,@OFFSET,nil);
  Result:=(R=0) or (R=STATUS_PENDING);
 end;
end;

function evbuffer_wsa_single(buf:Pevbuffer;var P:PWSABUF):SizeUint; forward;

Function pipe_send(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 OFFSET:Int64;
 P:PWSABUF;
 R:DWORD;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 with PBufferevent_sio(bev)^ do
 begin
  evbuffer_wsa_single(@FWBUF,PWSABUF(FWR_WSA.Buf));

  P:=FWR_WSA.Buf;

  OFFSET:=0;

  PIO_STATUS_BLOCK(@FWR_WSA)^.Status     :=STATUS_PENDING;
  PIO_STATUS_BLOCK(@FWR_WSA)^.Information:=0;

  R:=NtWriteFile(bev^.FHandle,0,nil,@FWR_WSA,@FWR_WSA,P^.buf,P^.len,@OFFSET,nil);

  PSizeUInt(ctrl_data)^:=PIO_STATUS_BLOCK(@FWR_WSA)^.Information;
  Result:=(R=0) or (R=STATUS_PENDING);
 end;
end;

Function pipe_rcvbuf(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  PSizeUInt(ctrl_data)^:=0;
 end;
end;

Function pipe_nobtr(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
var
 BLK:IO_STATUS_BLOCK;
 BUF:T_PIPE_PEEK;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  BLK:=Default(IO_STATUS_BLOCK);
  BUF:=Default(T_PIPE_PEEK);

  NtFsControlFile(bev^.FHandle,
                  0,
                  nil,
                  nil,
                  @BLK,
                  FSCTL_PIPE_PEEK,
                  nil,
                  0,
                  @BUF,
                  SizeOf(T_PIPE_PEEK));

  PSizeUInt(ctrl_data)^:=BUF.ReadDataAvailable;
  Result:=True;
 end;
end;

function pipe_shutdown(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=False;
end;

function pipe_close(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
var
 h:THandle;
begin
 h:=XCHG(bev^.FHandle,INVALID_HANDLE_VALUE);
 if (h<>INVALID_HANDLE_VALUE) then
 begin
  Result:=(NtClose(h)=0);
 end;
end;

///////

function calc_effective_align_mem(min,max:SizeUInt):SizeUInt;
Var
 P:Pointer;
 ef:SizeUInt;
 min_efc,min_res:SizeUInt;

begin
 Result:=max;

 if (min=0) or (max=0) or (min=max) then Exit;

 if (min>max) then
 begin

  min_efc:=max;
  min_res:=max;

  While (min>Result) do
  begin
   P:=GetMem(Result);
   ef:=MemSize(P)-Result;
   FreeMem(P);
   if (ef=0) then Exit;
   if (ef<min_efc) then
   begin
    min_efc:=ef;
    min_res:=Result;
   end;
   Result:=Result+SizeOf(SizeUInt);
  end;

  Result:=min_res;

  Exit;
 end;

 min_efc:=max;
 min_res:=max;

 While (min<Result) do
 begin
  P:=GetMem(Result);
  ef:=MemSize(P)-Result;
  FreeMem(P);
  if (ef=0) then Exit;
  if (ef<min_efc) then
  begin
   min_efc:=ef;
   min_res:=Result;
  end;
  Result:=Result-SizeOf(SizeUInt);
 end;

 Result:=min_res;
end;

function  rate_begin_read(rt:Prate_limit;Size:SizeUint;bev:PBufferevent;cb:Ttimer_cb):SizeUint;  forward;
function  rate_begin_write(rt:Prate_limit;Size:SizeUint;bev:PBufferevent;cb:Ttimer_cb):SizeUint; forward;
Procedure rate_end_read(rt:Prate_limit;Size:SizeUint);  forward;
Procedure rate_end_write(rt:Prate_limit;Size:SizeUint); forward;
function _rate_free(rt:Prate_limit):SizeUInt; forward;
function _rate_disable(rt:Prate_limit):SizeUInt; forward;


function bufferevent_get_fd(bev:Pbufferevent):THandle;
begin
 Result:=INVALID_HANDLE_VALUE;
 if Assigned(bev) then
  Result:=bev^.FHandle;
end;

function bufferevent_shutdown(bev:Pbufferevent;how:Longint):Boolean;
begin
 Result:=false;
 if Assigned(bev) then
  bev^.he_ops^.H_SHUTDOWN(bev,Pointer(how));
end;

function bufferevent_close(bev:Pbufferevent):Boolean;
begin
 Result:=false;
 if Assigned(bev) then
 With bev^ do
 begin
  Result:=CAS(Felock,EL_ENB,EL_BDI);
  if Result then
  begin
   Result:=bev^.he_ops^.H_CLOSE(bev,nil);
   store_release(Felock,EL_DIS);
  end;
 end;
end;

function  bufferevent_get_input(bev:Pbufferevent):Pevbuffer;
begin
 Result:=nil;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  bev^.be_ops^.BEV_CTRL_GET_IE(bev,@Result);
end;

function  bufferevent_get_output(bev:Pbufferevent):Pevbuffer;
begin
 Result:=nil;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  bev^.be_ops^.BEV_CTRL_GET_OE(bev,@Result);
end;

function _bufferevent_clean(bev:Pbufferevent):Boolean; inline;
begin
 Result:=False;
 if Assigned(bev^.be_ops) then
 begin
  Result:=bev^.be_ops^.BEV_CTRL_CLEAN(bev,nil);
 end;
end;

function bufferevent_write(bev:Pbufferevent):Boolean;
Var
 ev:Pevbuffer;
begin
 Result:=False;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
 if bev^.be_ops^.BEV_CTRL_GET_OE(bev,@ev) then
 if (evbuffer_get_length(ev)<>0) then
 begin
  Result:=bev^.be_ops^.BEV_CTRL_WRITE(bev,nil);
 end;
end;

function bufferevent_read(bev:Pbufferevent):Boolean;
begin
 Result:=False;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
 begin
  Result:=bev^.be_ops^.BEV_CTRL_READ(bev,nil);
 end;
end;

function _bufferevent_dec_ref(bev:Pbufferevent):Boolean; forward;

function _be_ops_sio_write_unlock(bev:Pbufferevent):SizeUInt; forward;

Procedure _be_ops_sio_write_timer(ev:Ptimer;bev:pointer);
Var
 i:SizeUInt;
begin
 i:=_be_ops_sio_write_unlock(bev);
 Case i of
  1,2:
  begin
   store_release(Pbufferevent_sio(bev)^.Fwlock,WL_DIS); //public disable write
  end;
 end;
 _bufferevent_dec_ref(bev);
end;

Procedure alloc_wsabuf(len:SizeUint;var P:PWSABUF); inline;
begin
 len:=len*SizeOf(WSABUF);
 if (P=nil) or (MemSize(P)<len) then
 begin
  P:=ReAllocMem(P,len);
 end;
end;

function evbuffer_wsa_single(buf:Pevbuffer;var P:PWSABUF):SizeUint;
Var
 vec:Peviovec;
begin
 alloc_wsabuf(1,P);

 vec:=evbuffer_peek(buf);

 if (vec=nil) then
 begin
  P^.buf:=nil;
  P^.len:=0;
 end else
 begin
  P^.buf:=eviovec_getdata(vec);
  P^.len:=eviovec_getlen(vec);
 end;

 Result:=1;
end;

function evbuffer_wsa_copy(buf:Pevbuffer;size:SizeUInt;var P:PWSABUF):SizeUint;
Var
 vec:Peviovec;
 R:SizeUInt;
begin
 Result:=0;

 vec:=evbuffer_peek(buf);
 if not Assigned(vec) then Exit;

 R:=eviovec_getlen(vec);

 if (R<=size) then
 begin

  Result:=1;
  alloc_wsabuf(Result,P);
  P^.buf:=eviovec_getdata(vec);
  P^.len:=R;

 end else
 begin

  Result:=1;
  alloc_wsabuf(Result,P);
  P^.buf:=eviovec_getdata(vec);
  P^.len:=eviovec_getlen(vec);

  repeat
   vec:=eviovec_next(buf,vec);
   if not Assigned(vec) then Break;

   R:=R+eviovec_getlen(vec);

   if (R>size) then
   begin

    R:=eviovec_getlen(vec)-(R-size);

    alloc_wsabuf(Result+1,P);
    P[Result].buf:=eviovec_getdata(vec);
    P[Result].len:=R;
    Result:=Result+1;

    Break;
   end;

   alloc_wsabuf(Result+1,P);
   P[Result].buf:=eviovec_getdata(vec);
   P[Result].len:=eviovec_getlen(vec);
   Result:=Result+1;

  until false;
 end;
end;

Function ev_send(bev:Pbufferevent;len:SizeUInt):Boolean; forward;

function _be_ops_sio_write_unlock(bev:Pbufferevent):SizeUInt;
Var
 s:SizeUInt;
begin
 Result:=0;

 With Pbufferevent_sio(bev)^ do
 begin

  //check enable
  if (load_acq_rel(FHandle)=INVALID_HANDLE_VALUE) or
     (load_acq_rel(Felock)<>EL_ENB) then Exit;

  s:=evbuffer_get_atless_size(@FWBUF,16*1024);
  if (s=0) then
  begin
   Exit(1);
  end;

  {$IFNDEF NO_RATELIMIT}
  s:=rate_begin_write(Frate_limit,s,bev,@_be_ops_sio_write_timer);
  if (s<>0) then
  {$ENDIF}
  begin
   if (not ev_send(bev,s)) then
   begin
    Exit(2);
   end;
  end;

 end;

end;

function be_ops_sio_write(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 With Pbufferevent_sio(bev)^ do
  repeat
   //check enable
   if (load_acq_rel(FHandle)=INVALID_HANDLE_VALUE) or
      (load_acq_rel(Felock)<>EL_ENB) then Exit(false);
   //public need write and try enable
   if (XCHG(Fwlock,WL_NWR)=WL_DIS) then
   begin

    store_release(Fwlock,WL_ENB); //public enable write
    Case _be_ops_sio_write_unlock(bev) of
     0://succes
       begin
        Exit(True);
       end;
     1://empty
       begin
        if CAS(Fwlock,WL_ENB,WL_DIS) then //try disable write
        begin
         Exit(True);
        end else
        begin
         store_release(Fwlock,WL_DIS); //public disable write
         //and continue
        end;
       end;
     2://error
       begin
        store_release(Fwlock,WL_DIS); //public disable write
        Exit(false);
       end;
    end;
   end else
   begin
    //other thread accqure
    Exit(True);
   end;
  until false;
end;

function _be_ops_sio_read_unlock(bev:Pbufferevent):SizeUInt; forward;

Procedure _be_ops_sio_read_timer(ev:Ptimer;bev:pointer);
Var
 i:SizeUInt;
begin
 i:=_be_ops_sio_read_unlock(bev);
 Case i of
  1,2:
  begin
   store_release(Pbufferevent_sio(bev)^.Frlock,WL_DIS); //public disable read
  end;
 end;
 _bufferevent_dec_ref(bev);
end;

Procedure _InitBuf(var P:Pointer;Len:SizeUInt); inline;
begin
 if (P=nil) or (MemSize(P)<len) then
 begin
  P:=ReAllocMem(P,len);
 end;
end;

{$IFNDEF NO_WATERMARKS}
Function GetWaterMarks(bev:Pbufferevent):TWaterMarks;
Var
 P:PWaterMarks;
begin
 Result:=Default(TWaterMarks);
 if Assigned(bev) then
 With Pbufferevent_sio(bev)^ do
 begin
  P:=FWaterMarks;
  if Assigned(P) then Result:=P^;
 end;
end;
{$ENDIF}

Function ev_nobtr(bev:Pbufferevent):SizeUInt;
begin
 Result:=0;
 bev^.he_ops^.H_NOBTR(bev,@Result);
end;

Function ev_recv0(bev:Pbufferevent;var len:SizeUInt;msg:Boolean):Boolean;
var
 a:array[0..1] of SizeUInt;
begin
 a[0]:=len;
 a[1]:=ord(msg);

 Result:=bev^.he_ops^.H_RECV(bev,@a);

 len:=a[0];
end;

function ev_peek0(bev:Pbufferevent):Boolean;
begin
 fetch_add(bev^.FRefCount,1);

 Result:=bev^.he_ops^.H_PEEK(bev,nil);

 if (not Result) then
 begin
  fetch_sub(bev^.FRefCount,1);
 end;
end;

function _iocp_post(base:Pevpoll;NOBT:SizeUInt;CTXProc:PCTXProc;lp:POVERLAPPED):Boolean; forward;
Procedure bufferevent_CTXProc(NOBT:SizeUInt;Data:PWSA_iovec); forward;

function ev_peek1(bev:Pbufferevent;len:SizeUInt):Boolean;
begin

 fetch_add(bev^.FRefCount,1);

 Result:=ev_recv0(bev,len,false);

 if (not Result) then
 begin
  fetch_sub(bev^.FRefCount,1);
 end else
 with Pbufferevent_sio(bev)^ do
 begin
  _iocp_post(Fbase,len,PCTXProc(@bufferevent_CTXProc),POVERLAPPED(@FRD_WSA));
 end;

end;

Function ev_recv1(bev:Pbufferevent;len:SizeUInt):Boolean;
var
 NOBTR:SizeUInt;
begin

 fetch_add(bev^.FRefCount,1);

 NOBTR:=ev_nobtr(bev);
 if (NOBTR<>0) then
 begin
  if (NOBTR<len) then
  begin
   len:=NOBTR;
  end;

  Result:=ev_recv0(bev,len,false);

  if (not Result) then
  begin
   fetch_sub(bev^.FRefCount,1);
  end else
  with Pbufferevent_sio(bev)^ do
  begin
   _iocp_post(Fbase,len,PCTXProc(@bufferevent_CTXProc),POVERLAPPED(@FRD_WSA));
  end;

 end else
 begin

  Result:=ev_recv0(bev,len,true);

  if (not Result) then
  begin
   fetch_sub(bev^.FRefCount,1);
  end;

 end;

end;

Function ev_send(bev:Pbufferevent;len:SizeUInt):Boolean;
begin
 fetch_add(bev^.FRefCount,1);

 Result:=bev^.he_ops^.H_SEND(bev,@len);

 if (not Result) then
 begin
  fetch_sub(bev^.FRefCount,1);
 end;

end;

function _be_ops_sio_read_unlock_ext(bev:Pbufferevent):SizeUInt;
Var
 S:SizeUInt;
begin
 Result:=0;
 With Pbufferevent_sio(bev)^ do
 begin

  {$IFNDEF NO_WATERMARKS}
  S:=GetWaterMarks(bev).RD.hi;
  if (S<>0) and (S<=evbuffer_get_length(@FRBUF)) then Exit;
  {$ENDIF}

  Result:=ev_nobtr(bev);
  if (Result<>0) then
  begin
   {$IFNDEF NO_RATELIMIT}
   Result:=rate_begin_read(Frate_limit,s,bev,nil);
   if (Result<>0) then
   {$ENDIF}
   begin
    _InitBuf(FRD_WSA.Buf,Result);
    if (not ev_recv0(bev,Result,false)) then Result:=0;
   end;
  end;

 end;
end;

function _be_ops_sio_read_unlock(bev:Pbufferevent):SizeUInt; //inline;
Var
 S:SizeUInt;
begin
 Result:=0;
 With Pbufferevent_sio(bev)^ do
 begin

  {$IFNDEF NO_WATERMARKS}
  S:=GetWaterMarks(bev).RD.hi;
  if (S<>0) and (S<=evbuffer_get_length(@FRBUF)) then
  begin
   Exit(1);
  end;
  {$ENDIF}

  s:=F_read_buf_size;
  if s=0 then
  begin

   s:=ev_nobtr(bev);
   if (s<>0) then
   begin
    {$IFNDEF NO_RATELIMIT}
    s:=rate_begin_read(Frate_limit,s,bev,@_be_ops_sio_read_timer);
    if (s<>0) then
    {$ENDIF}
    begin
     _InitBuf(FRD_WSA.Buf,s);
     if (not ev_peek1(bev,s)) then
     begin
      Exit(2);
     end;
    end;
   end else
   begin
    if (not ev_peek0(bev)) then
    begin
     Exit(2);
    end;
   end;

  end else
  begin

   {$IFNDEF NO_RATELIMIT}
   s:=rate_begin_read(Frate_limit,s,bev,@_be_ops_sio_read_timer);
   if (s<>0) then
   {$ENDIF}
   begin
    _InitBuf(FRD_WSA.Buf,s);
    if (not ev_recv1(bev,s)) then
    begin
     Exit(2);
    end;
   end;

  end;


 end;
end;

function be_ops_sio_read(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 With Pbufferevent_sio(bev)^ do
  repeat
   //check enable
   if (load_acq_rel(FHandle)=INVALID_HANDLE_VALUE) or
      (load_acq_rel(Felock)<>EL_ENB) then Exit(false);
   //public need read and try enable
   if (XCHG(Frlock,WL_NWR)=WL_DIS) then
   begin
    store_release(Frlock,WL_ENB); //public enable read
    Case _be_ops_sio_read_unlock(bev) of
     0://succes
       begin
        Exit(True);
       end;
     1://cancel?
       begin
        if CAS(Frlock,WL_ENB,WL_DIS) then //try disable read
        begin
         Exit(True);
        end else
        begin
         store_release(Frlock,WL_DIS); //public disable read
         //and continue
        end;
       end;
     2://error
       begin
        store_release(Frlock,WL_DIS); //public disable read
        Exit(false);
       end;
    end;
   end else
   begin
    //other thread accqure
    Exit(True);
   end;
  until false;
end;

function iocp_Loop_fb(parameter:Pevpoll):ptrint;
Var
 NOBT:DWORD;
 CTXProc:PCTXProc;
 Overlap:POVERLAPPED;
 Q:Boolean;
begin
 Result:=0;
 NOBT:=0;
 CTXProc:=nil;
 Overlap:=nil;
 With parameter^ do
 begin

  if Assigned(Fcfg.Finitcb) then Fcfg.Finitcb();

  Repeat
   Q:=GetQueuedCompletionStatus(hIOCP,NOBT,ULONG_PTR(CTXProc),Pointer(Overlap),100);
   if Q then
   begin
    if (NOBT=1) and (ULONG_PTR(CTXProc)=1) and (Overlap=nil) then
    begin
     PostQueuedCompletionStatus(hIOCP,1,1,nil);
     Break;
    end;
    if Assigned(CTXProc) then
    begin
     CTXProc(NOBT,Overlap);
    end;
   end else
   begin
    Case GetLastError of
      ERROR_NETNAME_DELETED:
      begin
       if Assigned(CTXProc) then
       begin
        CTXProc(NOBT,Overlap);
       end;
       Q:=True;
      end;
      WAIT_TIMEOUT:
      begin
       SleepEx(0,True);
       Q:=True;
      end;
    end;
   end;
   FreeMem(nil);
  Until (not Q);

  if Assigned(Fcfg.Ffinicb) then Fcfg.Ffinicb();

 end;
end;

function iocp_Loop_ex(parameter:Pevpoll):ptrint;
Const
 //INFINITE
 LTIME=10000;
Var
 OE:TOVERLAPPED_ENTRY;
 ulNum:ULONG;
 CTXProc:PCTXProc;
 Q:Boolean;
begin
 Result:=0;
 OE:=Default(TOVERLAPPED_ENTRY);
 CTXProc:=nil;
 With parameter^ do
 begin

  if Assigned(Fcfg.Finitcb) then Fcfg.Finitcb();

  Repeat
   ulNum:=0;
   Q:=_GetQueuedCompletionStatusEX(hIOCP,@OE,1,ulNum,LTIME,True);
   if Q then
   begin
    if (ulNum<>0) then
    begin
     CTXProc:=PCTXProc(OE.lpCompletionKey);
     if (OE.dwNumberOfBytesTransferred=1) and (OE.lpCompletionKey=1) and (OE.lpOverlapped=nil) then
     begin
      PostQueuedCompletionStatus(hIOCP,1,1,nil);
      Break;
     end;
     if Assigned(CTXProc) then
     begin
      CTXProc(OE.dwNumberOfBytesTransferred,OE.lpOverlapped);
     end;
    end;
   end else
   begin
    Case GetLastError of
     WAIT_TIMEOUT,WAIT_IO_COMPLETION:Q:=True;
     ERROR_NETNAME_DELETED:
     begin
      if Assigned(CTXProc) then
      begin
       CTXProc(OE.dwNumberOfBytesTransferred,OE.lpOverlapped);
      end;
      Q:=True;
     end;
    end;
   end;
   FreeMem(nil);
  Until (not Q);

  if Assigned(Fcfg.Ffinicb) then Fcfg.Ffinicb();

 end;
end;

function evpoll_init(base:Pevpoll;cfg:Pevpoll_config):Boolean;
begin
 if not Assigned(base) then Exit(false);
 With base^ do
 begin
  Result:=CAS(FState,0,1);
  if not Result then Exit;

  Fcfg:=Default(Tevpoll_config);
  if Assigned(cfg) then
  begin
   Fcfg:=cfg^;
  end;

  if (Fcfg.FReadBufSize<>0) then
  begin
   if (Fcfg.FReadBufSize<min_rbs) then Fcfg.FReadBufSize:=min_rbs;
   if (Fcfg.FReadBufSize>max_rbs) then Fcfg.FReadBufSize:=max_rbs;
  end;

  if (Fcfg.FWaterMarks.RD.lo<>0) and (Fcfg.FWaterMarks.RD.hi<>0) then
  if (Fcfg.FWaterMarks.RD.lo>Fcfg.FWaterMarks.RD.hi) then
  begin
   Fcfg.FWaterMarks.RD.lo:=Fcfg.FWaterMarks.RD.hi;
  end;

  if (Fcfg.FWaterMarks.WR.lo<>0) and (Fcfg.FWaterMarks.WR.hi<>0) then
  if (Fcfg.FWaterMarks.WR.lo>Fcfg.FWaterMarks.WR.hi) then
  begin
   Fcfg.FWaterMarks.WR.lo:=Fcfg.FWaterMarks.WR.hi;
  end;

  hIOCP:=0;
  NtCreateIoCompletion(@hIOCP,
                       $1f0003,
                       nil,
                       ULONG(-1));

  if (hIOCP=0) then
  begin
   store_release(FState,0);
   Result:=False;
   Exit;
  end;

  if Support_CompletionEx then
  begin
   Ffunc:=tthreadfunc(@iocp_Loop_ex);
  end else
  begin
   Ffunc:=tthreadfunc(@iocp_Loop_fb);
  end;

  store_release(FState,2);

 end;
end;

function evpoll_loop(base:Pevpoll):Boolean;
begin
 if not Assigned(base) then Exit(false);
 if (load_acquire(base^.FState)<>2) then Exit(false);
 if not Assigned(base^.Ffunc) then Exit(false);

 base^.Ffunc(base);
end;

function evpoll_break(base:Pevpoll):Boolean;
begin
 if not Assigned(base) then Exit(false);
 if (load_acquire(base^.FState)<>2) then Exit(false);

 With base^ do
 begin
  PostQueuedCompletionStatus(hIOCP,1,1,nil);
 end;
end;

function evpoll_free(base:Pevpoll):Boolean;
Var
 n:SizeUInt;
begin
 if not Assigned(base) then Exit(false);
 With base^ do
 begin
  Result:=CAS(FState,2,1);
  if not Result then Exit;

  //FRefs

  //PostQueuedCompletionStatus(hIOCP,1,1,nil);
  //WaitForMultipleObjects(Fthreads_count,hThreads,TRUE,INFINITE);

  NtClose(hIOCP);

  store_release(FState,0);
 end;
end;

function evpoll_isinit(base:Pevpoll):Boolean; inline;
begin
 if not Assigned(base) then Exit(false);
 Result:=load_acquire(base^.FState)=2;
end;

function evpoll_cfg(base:Pevpoll):Pevpoll_config; inline;
begin
 if not Assigned(base) then Exit(nil);
 Result:=@base^.Fcfg;
end;

function _iocp_bind(base:Pevpoll;H:THandle;CTXProc:PCTXProc):Boolean;
var
 BLK:IO_STATUS_BLOCK;
 INFO:FILE_COMPLETION_INFORMATION;
 R:DWORD;
begin
 Result:=False;
 if evpoll_isinit(base) then
 begin
  BLK:=Default(IO_STATUS_BLOCK);
  INFO.Port:=base^.hIOCP;
  INFO.Key :=CTXProc;
  R:=NtSetInformationFile(H,@BLK,@INFO,SizeOf(INFO),FileCompletionInformation);
  if (R=STATUS_INVALID_PARAMETER) then
  begin
   R:=NtSetInformationFile(H,@BLK,@INFO,SizeOf(INFO),FileReplaceCompletionInformation);
  end;
  Result:=(R=0);
 end;
end;

function _iocp_post(base:Pevpoll;NOBT:SizeUInt;CTXProc:PCTXProc;lp:POVERLAPPED):Boolean; inline;
begin
 Result:=False;
 if evpoll_isinit(base) then
 begin
  Result:=PostQueuedCompletionStatus(base^.hIOCP,NOBT,ULONG_PTR(CTXProc),lp);
 end;
end;

function  evpoll_post(base:Pevpoll;cb:Pevpoll_post_cb;param1:SizeUInt;param2:Pointer):Boolean;
begin
 if not Assigned(base) then Exit(false);
 Result:=_iocp_post(base,param1,PCTXProc(cb),param2);
end;

procedure _bufferevent_inc_ref(bev:Pbufferevent); inline;
begin
 fetch_add(bev^.FRefCount,1);
end;

procedure _bufferevent_sio_fin_connect(bev:Pbufferevent_sio;var events:SizeUInt); forward;
function  _bufferevent_sio_fin_write(bev:Pbufferevent_sio;NOBT:SizeUInt):Boolean; forward;
function  _bufferevent_sio_fin_read(bev:Pbufferevent_sio;NOBT:SizeUInt):Boolean; forward;
Procedure _bufferevent_do_trigger(bev:PBufferevent_sio;events:SizeUInt); forward;

Procedure bufferevent_do_trigger_cb(events:SizeUInt;bev:PBufferevent_sio);
begin
 if not Assigned(bev) then Exit;

 _bufferevent_do_trigger(bev,events);

 _bufferevent_dec_ref(bev);
end;

function _trigger_cb_post(bev:PBufferevent_sio;events:SizeUInt):Boolean;
begin
 Result:=Assigned(bev^.Fbase);
 if not Result then Exit;
 _bufferevent_inc_ref(bev);
 _iocp_post(bev^.Fbase,events,PCTXProc(@bufferevent_do_trigger_cb),POVERLAPPED(bev));
end;

Procedure _bufferevent_do_trigger_be_ops(bev_lock,bev_call:PBufferevent_sio;events:SizeUInt); inline;
begin
 Case load_acq_rel(bev_call^.Felock) of
  EL_ENB,EL_BCN:;
  else
   Exit;
 end;

 if (load_acq_rel(bev_call^.FHandle)=INVALID_HANDLE_VALUE) then Exit;

 if bev_call<>bev_lock then
 begin

  Case load_acq_rel(bev_lock^.Felock) of
   EL_ENB,EL_BCN:;
   else
    Exit;
  end;

  if (load_acq_rel(bev_lock^.FHandle)=INVALID_HANDLE_VALUE) then Exit;

 end;

 if Assigned(bev_call^.be_ops) then
  if spin_trylock(bev_lock^.Fclock) then
  begin

   bev_call^.be_ops^.BEV_CTRL_EVENT(bev_call,@events);

   spin_unlock(bev_lock^.Fclock);
  end else
  begin
   _trigger_cb_post(bev_call,events);
  end;
end;

Procedure _bufferevent_do_trigger(bev:PBufferevent_sio;events:SizeUInt); inline;
begin
 if Assigned(bev^.be_ops) then
 begin
  bev^.be_ops^.BEV_CTRL_TRIGGER(bev,@events);
 end;
end;

Procedure bufferevent_CTXProc_connect(bev:PBufferevent_sio); inline;
Var
 events:SizeUInt;
 err:DWORD;
begin
 events:=0;
 err:=NtGetOverlappedError(@bev^.FWR_WSA.O);

 Case err of
  STATUS_GRACEFUL_DISCONNECT,
  STATUS_CONNECTION_REFUSED,
  STATUS_CONNECTION_RESET,
  STATUS_CONNECTION_ABORTED:
  begin
   events:=events or BEV_EVENT_ERROR or BEV_EVENT_EOF;
  end;
  STATUS_SUCCESS,   //normal case
  STATUS_CANCELLED:;//cancel not error
  else
   events:=events or BEV_EVENT_ERROR;
 end;

 _bufferevent_sio_fin_connect(bev,events);

 if (events<>0) then
 begin
  _bufferevent_do_trigger(bev,events);
 end;

 _bufferevent_dec_ref(bev);
end;

Procedure bufferevent_CTXProc_read(NOBT:SizeUInt;bev:PBufferevent_sio);
Var
 events:SizeUInt;
 err:DWORD;
begin
 events:=0;

 if (NOBT=0) then
 begin
  err:=NtGetOverlappedError(@bev^.FRD_WSA.O);

  if (err=0) then
  begin
   NOBT:=ev_nobtr(bev);
   if (NOBT<>0) then
   begin

    {$IFNDEF NO_RATELIMIT}
    NOBT:=rate_begin_read(bev^.Frate_limit,NOBT,bev,@_be_ops_sio_read_timer);
    if (NOBT<>0) then
    begin
    {$ENDIF}
     _InitBuf(bev^.FRD_WSA.Buf,NOBT);
     ev_recv0(bev,NOBT,false);
     err:=NtGetOverlappedError(@bev^.FRD_WSA.O);
    {$IFNDEF NO_RATELIMIT}
    end else
    begin
     //break with rate limit
     _bufferevent_dec_ref(bev);///////
     Exit;
    end;
    {$ENDIF}

   end;

  end;

  Case err of
   STATUS_SUCCESS:; //ignore
   STATUS_GRACEFUL_DISCONNECT,
   STATUS_CONNECTION_REFUSED,
   STATUS_CONNECTION_RESET,
   STATUS_CONNECTION_ABORTED:
   begin
    events:=events or BEV_EVENT_ERROR or BEV_EVENT_EOF;
   end;
   STATUS_CANCELLED:;//cancel not error
   STATUS_PENDING:;  //ignore
   STATUS_TIMEOUT:;  //ignore
   else
    events:=events or BEV_EVENT_ERROR;
  end;

 end;

 if _bufferevent_sio_fin_read(bev,NOBT) then
 begin
  events:=events or BEV_EVENT_READING;
 end;

 if (load_acq_rel(bev^.FHandle)=INVALID_HANDLE_VALUE) or
    (load_acq_rel(bev^.Felock)<>EL_ENB) then
 begin
  _bufferevent_dec_ref(bev);///////
  Exit;
 end;

 if (NOBT<>0) then
 begin
  be_ops_sio_read(bev,nil);
 end;

 if (events<>0) then
 begin
  _bufferevent_do_trigger(bev,events);
 end;

 _bufferevent_dec_ref(bev);///////
end;

Procedure bufferevent_CTXProc_write(NOBT:SizeUInt;bev:PBufferevent_sio);
Var
 events:SizeUInt;
 err:DWORD;
begin
 events:=0;

 if (NOBT=0) then
 begin
  err:=NtGetOverlappedError(@bev^.FWR_WSA.O);

  Case err of
   STATUS_SUCCESS,
   STATUS_GRACEFUL_DISCONNECT,
   STATUS_CONNECTION_REFUSED,
   STATUS_CONNECTION_RESET,
   STATUS_CONNECTION_ABORTED:
   begin
    events:=events or BEV_EVENT_ERROR or BEV_EVENT_EOF;
   end;
   STATUS_CANCELLED:;//cancel not error
   STATUS_PENDING:;  //ignore
   STATUS_TIMEOUT:;  //ignore
   else
    events:=events or BEV_EVENT_ERROR;
  end;

 end;

 if _bufferevent_sio_fin_write(bev,NOBT) then
 begin
  events:=events or BEV_EVENT_WRITING;
 end;

 if (load_acq_rel(bev^.FHandle)=INVALID_HANDLE_VALUE) or
    (load_acq_rel(bev^.Felock)<>EL_ENB) then
 begin
  _bufferevent_dec_ref(bev);///////
  Exit;
 end;

 if (NOBT<>0) then
 begin
  be_ops_sio_write(bev,nil);
 end;

 if (events<>0) then
 begin
  _bufferevent_do_trigger(bev,events);
 end;

 _bufferevent_dec_ref(bev);///////
end;

Procedure bufferevent_CTXProc(NOBT:SizeUInt;Data:PWSA_iovec);
Var
 bev:PBufferevent_sio;

begin

 bev:=PBufferevent_sio(Data^.P);
 assert(bev<>nil);
 if (bev=nil) then Exit;

 if (load_acq_rel(bev^.Felock)=EL_BCN) then
 begin
  bufferevent_CTXProc_connect(bev);
  Exit;
 end else
 if (@bev^.FRD_WSA=Data) then //isRead
 begin
  bufferevent_CTXProc_read(NOBT,bev);
 end else
 begin
  bufferevent_CTXProc_write(NOBT,bev);
 end;

end;

procedure _WSA_iovec_init(PW:PWSA_iovec;P:Pbufferevent); inline;
begin
 PW^.O:=Default(TOVERLAPPED);
 PW^.P:=P;
end;

function be_ops_sio_enable(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=True;
end;

function _be_ops_do_eventcb(bev_cb,bev_dst:Pbufferevent;events:SizeUInt):Boolean; inline;
Var
 _eventcb:TBufferevent_eventcb;
begin
 Pointer(_eventcb):=load_acq_rel(Pointer(bev_cb^.Feventcb));
 Result:=Assigned(_eventcb);
 if Result then
 begin
  _eventcb(bev_dst,events,load_acq_rel(bev_cb^.Fctx));
 end;
end;

function _bufferevent_set_rate_limit(bev:Pbufferevent;rg:Prate_limit_group):boolean; forward;
function _bufferevent_sio_connect(bev:Pbufferevent_sio;P:PSockInfo):Boolean; forward;

function be_ops_sio_connect(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=_bufferevent_sio_connect(Pbufferevent_sio(bev),PSockInfo(ctrl_data));
end;

Procedure _set_read_buf_size(bev:Pbufferevent;New:SizeUInt);
Var
 Len:SizeUInt;
begin
 with PBufferevent_sio(bev)^ do
 begin
  if (New=0) then
  begin
   Len:=0;
   bev^.he_ops^.H_RCVBUF(bev,@Len);
   if (Len<>0) then
   begin
    F_read_buf_size:=New;
   end;
  end else
  begin
   if New<min_rbs then New:=min_rbs;
   if New>max_rbs then New:=max_rbs;
   F_read_buf_size:=New;
  end;
 end;
end;

function be_ops_false(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=False;
end;

function be_ops_trigger(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  _bufferevent_do_trigger_be_ops(Pbufferevent_sio(bev),Pbufferevent_sio(bev),PSizeUInt(ctrl_data)^);
  Result:=true;
 end;
end;

function be_ops_event(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  Result:=_be_ops_do_eventcb(bev,bev,PSizeUInt(ctrl_data)^);
 end;
end;

function be_ops_post(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  Result:=_trigger_cb_post(Pbufferevent_sio(bev),PSizeUInt(ctrl_data)^);
 end;
end;

function be_ops_get_ie(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  PPointer(ctrl_data)^:=@Pbufferevent_sio(bev)^.FRBUF;
  Result:=true;
 end;
end;

function be_ops_get_oe(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  PPointer(ctrl_data)^:=@Pbufferevent_sio(bev)^.FWBUF;
  Result:=true;
 end;
end;

{$IFNDEF NO_RATELIMIT}
function be_ops_get_rl(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  PPointer(ctrl_data)^:=Pbufferevent_sio(bev)^.Frate_limit;
  Result:=true;
 end;
end;

function be_ops_set_rl(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=_bufferevent_set_rate_limit(bev,ctrl_data);
end;
{$ENDIF}


{$IFNDEF NO_WATERMARKS}
function be_ops_get_wm(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  PWaterMarks(ctrl_data)^:=GetWaterMarks(bev);
  Result:=true;
 end;
end;

function be_ops_set_wm(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  Pbufferevent_sio(bev)^.FWaterMarks:=ctrl_data;
  Result:=true;
 end;
end;
{$ENDIF}

function be_ops_set_rbs(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  _set_read_buf_size(bev,PSizeUInt(ctrl_data)^);
  Result:=true;
 end;
end;

function bufferevent_set_watermarks(bev:Pbufferevent;wm:PWaterMarks):boolean;
begin
 Result:=False;
 {$IFNDEF NO_WATERMARKS}
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  Result:=bev^.be_ops^.BEV_CTRL_SET_WM(bev,wm);
 {$ENDIF}
end;

function bufferevent_set_read_buf_size(bev:Pbufferevent;New:SizeUInt):boolean;
begin
 Result:=False;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  Result:=bev^.be_ops^.BEV_CTRL_SET_RBS(bev,@New);
end;

function _bufferevent_sio_new(base:Pevpoll;fd:THandle;fd_ops:PHandle_ops;size:SizeUInt):Pbufferevent;
begin
 Result:=nil;
 if not evpoll_isinit(base) then Exit;

 Result:=AllocMem(size);

 With PBufferevent_sio(Result)^ do
 begin
  be_ops:=@buf_sio_ops;
  he_ops:=fd_ops;

  FHandle:=fd;

  Fbase:=base;

  _WSA_iovec_init(@FRD_WSA,Result);
  _WSA_iovec_init(@FWR_WSA,Result);

  evbuffer_init(@FRBUF);
  evbuffer_init(@FWBUF);

  {$IFNDEF NO_WATERMARKS}
  FWaterMarks:=@base^.Fcfg.FWaterMarks;
  {$ENDIF}


  F_read_buf_size:=1024;
  _set_read_buf_size(Result,base^.Fcfg.FReadBufSize);

  fetch_add(FRefCount,1);

 end;

 _iocp_bind(base,fd,PCTXProc(@bufferevent_CTXProc));

end;

function bufferevent_socket_new(base:Pevpoll;fd:THandle):Pbufferevent; inline;
begin
 Result:=_bufferevent_sio_new(base,fd,@sock_ops,SizeOf(TBufferevent_sio));
end;

function bufferevent_pipe_new(base:Pevpoll;fd:THandle):Pbufferevent; inline;
begin
 Result:=_bufferevent_sio_new(base,fd,@pipe_ops,SizeOf(TBufferevent_sio));
end;

function bufferevent_socket_connect(bev:Pbufferevent;sa:Psockaddr;socklen:SizeUInt):Boolean;
Var
 SI:TSockInfo;
begin
 Result:=False;
 if Assigned(sa) and (socklen>0) then
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
 With bev^ do
 begin
  if CAS(bev^.Felock,EL_DIS,EL_BCN) then
  begin
   if (load_acq_rel(FHandle)=INVALID_HANDLE_VALUE) then
   begin
    SI.sa :=sa;
    SI.len:=socklen;
    Result:=bev^.be_ops^.BEV_CTRL_CONNECT(bev,@SI);
   end;
   if not Result then
   begin
    store_release(bev^.Felock,EL_DIS);
   end;
  end;
 end;
end;

function bufferevent_socket_connect_hostname(bev:Pbufferevent;family:Integer;hostname:PAnsiChar;port:Word):Boolean;
Var
 hint:TaddrinfoA;
 pResult,p:PADDRINFOA;
begin
 Result:=False;

 hint:=Default(TaddrinfoA);
 hint.ai_family  :=family;
 hint.ai_protocol:=IPPROTO_TCP;
 hint.ai_socktype:=SOCK_STREAM;

 pResult:=nil;
 if getaddrinfo(hostname,nil,@hint,pResult)<>0 then Exit;

 p:=pResult;
 While Assigned(p) do
 begin
  if (p^.ai_family=family) then
  begin
   p^.ai_addr^.sin_port:=htons(port);
   Result:=bufferevent_socket_connect(bev,Pointer(p^.ai_addr),p^.ai_addrlen);
   Break;
  end;
  p:=p^.ai_next;
 end;
 if Assigned(pResult) then
  freeaddrinfo(pResult);
end;

function _bufferevent_sio_connect(bev:Pbufferevent_sio;P:PSockInfo):Boolean; inline;
Var
 sa:TSockAddrIn6;
 hSocket:THandle;
begin
 Result:=false;
 if not Assigned(P) then Exit;
 if not (Assigned(P^.sa) and (P^.len>0)) then Exit;

 hSocket:=WSASocket(P^.sa^.sin_family,SOCK_STREAM,IPPROTO_TCP,nil,0,WSA_FLAG_OVERLAPPED);
 if hSocket=INVALID_SOCKET then
 begin
  Exit;
 end;

 _iocp_bind(bev^.Fbase,hSocket,PCTXProc(@bufferevent_CTXProc));

 sa:=Default(TSockAddrIn6);
 sa.sin6_family:=P^.sa^.sin_family;

 if (bind(hSocket,@sa,P^.len)<>0) then
 begin
  CloseSocket(hSocket);
  Exit;
 end;

 store_release(bev^.FHandle,hSocket);
 fetch_add(bev^.FRefCount,1);

 Result:=ConnectEx(hSocket,P^.sa,P^.len,@bev^.FWR_WSA.O);

 if Result then
 begin
  Case WSAGetLastError() of
   0,WSA_IO_PENDING:;
   else
    Result:=False;
  end;
 end;

 if not Result then
 begin
  store_release(bev^.FHandle,INVALID_SOCKET);
  CloseSocket(hSocket);
  fetch_sub(bev^.FRefCount,1);
 end;

end;

function _connect_enable(bev:Pbufferevent):Boolean; inline;
begin
 Result:=False;
 With bev^ do
 begin
  Result:=CAS(Felock,EL_BCN,EL_BEN);
  if Result then
  begin
   Result:=bev^.be_ops^.BEV_CTRL_ENABLE(bev,nil);
   if Result then
   begin
    store_release(Felock,EL_ENB);
    Result:=bev^.be_ops^.BEV_CTRL_READ(bev,nil);
   end else
   begin
    store_release(Felock,EL_DIS);
   end;
  end;
 end;
end;

Const
 SO_UPDATE_CONNECT_CONTEXT=$7010;

procedure _bufferevent_sio_fin_connect(bev:Pbufferevent_sio;var events:SizeUInt); inline;
begin
 if (load_acq_rel(bev^.FHandle)=INVALID_HANDLE_VALUE) then //cancel?
 begin
  Exit;
 end;

 if (events and BEV_EVENT_ERROR)<>0 then Exit;

 if setsockopt(bev^.FHandle,SOL_SOCKET,SO_UPDATE_CONNECT_CONTEXT,nil,0)<>0 then
 begin
  events:=events or BEV_EVENT_ERROR;
  Exit;
 end;

 if not _connect_enable(bev) then
 begin
  events:=events or BEV_EVENT_ERROR;
  Exit;
 end;

 events:=events or BEV_EVENT_CONNECTED;
end;

function _bufferevent_dec_ref(bev:Pbufferevent):Boolean;
begin
 Result:=False;
 With bev^ do
 begin
  Result:=fetch_sub(FRefCount,1)=1;
  if Result then
  begin
   _bufferevent_clean(bev);
  end;
 end;
end;

function bufferevent_inc_ref(bev:Pbufferevent):Boolean; inline;
begin
 Result:=False;
 if Assigned(bev) then
 begin
  _bufferevent_inc_ref(bev);
  Result:=True;
 end;
end;

function bufferevent_dec_ref(bev:Pbufferevent):Boolean; inline;
begin
 Result:=False;
 if Assigned(bev) then
 begin
  Result:=_bufferevent_dec_ref(bev);
 end;
end;

function be_ops_sio_clean(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=True;
 With Pbufferevent_sio(bev)^ do
 begin

  {$IFNDEF NO_RATELIMIT}
  _rate_free(Frate_limit);
  {$ENDIF}

  evbuffer_clear(@FRBUF);
  evbuffer_clear(@FWBUF);

  FreeMem(FRD_WSA.buf);
  FreeMem(FWR_WSA.buf);

  FreeMem(bev);
 end;
end;

function bufferevent_free(bev:Pbufferevent):Boolean;
begin
 Result:=False;
 if bev=nil then Exit;
 if (bev^.he_ops^.H_CLOSE(bev,nil)) then
 begin
  bev^.be_ops^.BEV_CTRL_FREE(bev,nil);
  Result:=_bufferevent_dec_ref(bev);
 end;
end;

Procedure bufferevent_setcb(bev:Pbufferevent;eventcb:TBufferevent_eventcb;cbarg:pointer); inline;
begin
 if bev=nil then Exit;
 bev^.Feventcb:=eventcb;
 store_seq_cst(bev^.Fctx,cbarg);
end;

function bufferevent_enable(bev:Pbufferevent):Boolean;
begin
 Result:=False;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
 With bev^ do
 begin
  Result:=CAS(Felock,EL_DIS,EL_BEN);
  if Result then
  begin
   Result:=bev^.be_ops^.BEV_CTRL_ENABLE(bev,nil);
   if Result then
   begin
    store_release(Felock,EL_ENB);
    Result:=bev^.be_ops^.BEV_CTRL_READ(bev,nil);
   end else
   begin
    store_release(Felock,EL_DIS);
   end;
  end;
 end;
end;

function bufferevent_disable(bev:Pbufferevent):Boolean;
begin
 Result:=False;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
 With bev^ do
 begin
  Result:=CAS(Felock,EL_ENB,EL_BDI);
  if Result then
  begin
   Result:=bev^.be_ops^.BEV_CTRL_DISABLE(bev,nil);
   store_release(Felock,EL_DIS);
  end;
 end;
end;

function be_ops_sio_disable(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 dr:SizeUint;
begin
 Result:=true;

 With Pbufferevent_sio(bev)^ do
 begin

  if Support_CancelIoEx then
  begin
   CancelIoEx(FHandle,@FRD_WSA);
   CancelIoEx(FHandle,@FWR_WSA);
  end else
  begin
   CancelIo(FHandle);
  end;

  {$IFNDEF NO_RATELIMIT}
  dr:=_rate_disable(Frate_limit);
  While (dr<>0) do
  begin
   _bufferevent_dec_ref(bev);
   Dec(dr);
  end;
  {$ENDIF}

 end;
end;

function _bufferevent_sio_fin_write(bev:Pbufferevent_sio;NOBT:SizeUInt):Boolean; inline;
var
 S:SizeUint;
begin

 With bev^ do
 begin
  {$IFNDEF NO_RATELIMIT}
  rate_end_write(Frate_limit,NOBT);
  {$ENDIF}

  evbuffer_drain(@FWBUF,NOBT);

  Result:=evbuffer_isempty(@FWBUF);

  XCHG(Fwlock,WL_DIS); //public disable write

  {$IFNDEF NO_WATERMARKS}
  S:=GetWaterMarks(bev).WR.lo;
  if (S<>0) then
  begin
   Result:=evbuffer_get_length(@FWBUF)<=S;
  end;
  {$ENDIF}

 end;

end;

function _bufferevent_sio_fin_read(bev:Pbufferevent_sio;NOBT:SizeUInt):Boolean; inline;
var
 S:SizeUint;
begin
 Result:=true;
 With bev^ do
 begin
  if (NOBT<>0) then
  begin
   {$IFNDEF NO_RATELIMIT}
   rate_end_read(Frate_limit,NOBT);
   {$ENDIF}
   S:=MemSize(FRD_WSA.buf) div 2;
   if (NOBT>S) then
   begin
    evbuffer_add_ref(@FRBUF,FRD_WSA.buf,0,NOBT,Freemem_ptr);
    FRD_WSA.buf:=nil;
   end else
   begin
    evbuffer_add(@FRBUF,FRD_WSA.buf,NOBT);
   end;
  end;

  NOBT:=_be_ops_sio_read_unlock_ext(bev);
  if (NOBT<>0) then
  begin
   {$IFNDEF NO_RATELIMIT}
   rate_end_read(Frate_limit,NOBT);
   {$ENDIF}
   S:=MemSize(FRD_WSA.buf) div 2;
   if (NOBT>S) then
   begin
    evbuffer_add_ref(@FRBUF,FRD_WSA.buf,0,NOBT,Freemem_ptr);
    FRD_WSA.buf:=nil;
   end else
   begin
    evbuffer_add(@FRBUF,FRD_WSA.buf,NOBT);
   end;
  end;


  XCHG(Frlock,WL_DIS); //public disable read

  {$IFNDEF NO_WATERMARKS}
  S:=GetWaterMarks(bev).RD.lo;
  if (S<>0) then
  begin
   Result:=evbuffer_get_length(@FRBUF)>=S;
  end;
  {$ENDIF}

 end;

end;

//TBufferevent_sio_pair

function bufferevent_set_pair(bev,new_pair:PBufferevent):Boolean; inline;
begin
 Result:=False;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  Result:=bev^.be_ops^.BEV_CTRL_SET_PA(bev,new_pair);
end;

function bufferevent_get_pair(bev:PBufferevent):PBufferevent; inline;
begin
 Result:=nil;
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  bev^.be_ops^.BEV_CTRL_GET_PA(bev,@Result);
end;

function _bufferevent_set_pair(bev,new_pair:PBufferevent_sio_pair):Boolean;
var
 pair:PBufferevent_sio;
 i:Integer;
begin
 Result:=True;

 i:=0;

 pair:=XCHG(bev^.FPair,nil);
 if Assigned(pair) then
 begin
  i:=i-1;
  _bufferevent_dec_ref(pair);
 end;

 if Assigned(new_pair) then
 begin
  _bufferevent_inc_ref(new_pair);
  if CAS(bev^.FPair,nil,new_pair) then
  begin
   i:=i+1;
  end else
  begin
   _bufferevent_dec_ref(new_pair);
  end;
 end;

 Case i of
  -1:_bufferevent_dec_ref(bev);
   1:_bufferevent_inc_ref(bev);
 end;

end;

function be_ops_pair_trigger(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
var
 pair:PBufferevent_sio;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  pair:=PBufferevent_sio_pair(bev)^.FPair;
  if Assigned(pair) then
  begin
   _bufferevent_do_trigger_be_ops(pair,PBufferevent_sio(bev),PSizeUInt(ctrl_data)^);
  end;
  Result:=true;
 end;
end;

function be_ops_pair_event(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
Var
 pair:PBufferevent_sio;
begin
 Result:=false;
 if Assigned(ctrl_data) then
 begin
  pair:=PBufferevent_sio_pair(bev)^.FPair;
  if Assigned(pair) then
  begin
   Result:=_be_ops_do_eventcb(pair,bev,PSizeUInt(ctrl_data)^);
  end;
 end;
end;

function be_ops_pair_set_pa(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=_bufferevent_set_pair(PBufferevent_sio_pair(bev),ctrl_data);
end;

function be_ops_pair_get_pa(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 Result:=False;
 if Assigned(ctrl_data) then
 begin
  PPointer(ctrl_data)^:=PBufferevent_sio_pair(bev)^.FPair;
  Result:=True;
 end;
end;

function be_ops_pair_free(bev:Pbufferevent;ctrl_data:Pointer):Boolean;
begin
 _bufferevent_set_pair(PBufferevent_sio_pair(bev),nil);
 Result:=True;
end;

function bufferevent_socket_pair_new(base:Pevpoll;fd:THandle;pair:Pbufferevent):Pbufferevent;
begin
 Result:=_bufferevent_sio_new(base,fd,@sock_ops,SizeOf(TBufferevent_sio_pair));
 if Assigned(Result) then
 begin
  Result^.be_ops:=@buf_sio_pair_ops;
  _bufferevent_set_pair(PBufferevent_sio_pair(Result),PBufferevent_sio_pair(pair));
 end;
end;

//----evconnlistener----

function  evconnlistener_CloseEx(Listener:Pevconnlistener;New:Plistener_acceptex):Boolean; forward;
function  evconnlistener_AcceptEx(Listener:Pevconnlistener;New:Plistener_acceptex):Boolean; forward;
Procedure evconnlistener_CTXProc_AcceptEx(NOBT:SizeUInt;Data:Plistener_acceptex); forward;

function evconnlistener_get_fd(lev:Pevconnlistener):THandle; inline;
begin
 Result:=INVALID_HANDLE_VALUE;
 if Assigned(lev) then
  Result:=lev^.FHandle;
end;

function evconnlistener_enable(lev:Pevconnlistener):Boolean;
begin
 Result:=False;
 if Assigned(lev) then
 With lev^ do
 begin
  Result:=(Fcb<>nil);
  if Result then
  begin
   Result:=CAS(FState,LST_DISABLE,LST_ENABLE);
   if Result then
   begin
    evconnlistener_AcceptEx(lev,@FNew);
   end;
  end;
 end;
end;

function evconnlistener_disable(lev:Pevconnlistener):Boolean;
Var
 Prev:SizeUInt;
begin
 Result:=False;
 if Assigned(lev) then
 With lev^ do
 begin
  Prev:=XCHG(FState,LST_CLOSED);
  Case Prev of
   LST_ENABLE:
   begin
    store_release(FState,LST_DISABLE);
    Result:=True;
   end;
   LST_ACCEPT:
   begin
    if Support_CancelIoEx then
    begin
     Result:=CancelIoEx(FHandle,@FNew);
    end else
    begin
     CloseSocket(XCHG(lev^.FNew.FHandle,INVALID_SOCKET));
    end;
   end;
   else
   begin
    store_release(FState,Prev);
   end;
  end;
 end;
end;

procedure _evconnlistener_free(lev:Pevconnlistener); inline;
begin
 With lev^ do
 begin
  CloseSocket(FHandle);
  CloseSocket(FNew.FHandle);
 end;
 FreeMem(lev);
end;

procedure evconnlistener_free(lev:Pevconnlistener);
Var
 base:Pevpoll;
 Prev:SizeUInt;

begin
 if lev=nil then Exit;

 base:=lev^.Fbase;

 Prev:=XCHG(lev^.FState,LST_FREE);

 if (evpoll_isinit(base) and (Prev=LST_ACCEPT)) then
 begin
  CloseSocket(XCHG(lev^.FHandle,INVALID_SOCKET));
 end else
 begin
  _evconnlistener_free(lev);
 end;

end;

procedure evconnlistener_set_cb(lev:Pevconnlistener;cb:Tevconnlistener_cb;ptr:Pointer); inline;
begin
 if Assigned(lev) then
 With lev^ do
 begin
  Fcb:=cb;
  FPtr:=ptr;
 end;
end;

function  evconnlistener_new_bind(base:Pevpoll;cb:Tevconnlistener_cb;ptr:Pointer;Reusable:Boolean;backlog:SizeUInt;sa:Psockaddr;socklen:SizeUInt):Pevconnlistener;
Var
 af:Longint;
 hSocket:THandle;
 Flag:DWORD;
begin
 Result:=nil;

 if base=nil then Exit;
 if not evpoll_isinit(base) then Exit;

 af:=0;
 if Assigned(sa) then
 begin
  af:=sa^.sin_family;
 end;

 hSocket:=WSASocket(af,SOCK_STREAM,IPPROTO_TCP,nil,0,WSA_FLAG_OVERLAPPED);
 if hSocket=INVALID_SOCKET then
 begin
  Exit;
 end;

 if Reusable then
 begin
  Flag:=1;
  setsockopt(hSocket,SOL_SOCKET,SO_REUSEADDR,@Flag,SizeOf(DWORD));
 end;

 if Bind(hSocket,sa,socklen)=SOCKET_ERROR then
 begin
  CloseSocket(hSocket);
  Exit;
 end;

 Result:=AllocMem(SizeOf(Tevconnlistener));

 With Result^ do
 begin
  FHandle:=hSocket;
  Fbase:=base;
  Fcb:=cb;
  FPtr:=ptr;
  FNew.FHandle:=INVALID_HANDLE_VALUE;
 end;

 Listen(hSocket,backlog);

 _iocp_bind(base,hSocket,PCTXProc(@evconnlistener_CTXProc_AcceptEx));

 evconnlistener_enable(Result);

end;

function GetAcceptEx(New:Plistener_acceptex;var sa:PSockAddrIn6;var socklen:SizeUInt):Boolean; inline;
Var
 Loc:PSockAddrIn6;
 nloclen:Integer;
 nremlen:Integer;
begin
 Result:=False;
 socklen:=0;

 if New=nil then Exit;


 Loc:=nil;
 nloclen:=0;
 nremlen:=0;


 if not GetAcceptExSockaddrs(New^.FHandle,@New^.Buf,0,
                             PSOCKADDR(Loc),nloclen,
                             PSOCKADDR(sa) ,nremlen) then Exit;

 socklen:=nremlen;

 if setsockopt(New^.FHandle,SOL_SOCKET,SO_UPDATE_ACCEPT_CONTEXT,@New^.FHandle,sizeof(THandle))=0 then
 begin
  Result:=True;
 end;

end;

Procedure evconnlistener_CTXProc_AcceptEx(NOBT:SizeUInt;Data:Plistener_acceptex);
Var
 FState:SizeUInt;
 Listener:Pevconnlistener;
 sa:PSockAddrIn6;
 socklen:SizeUInt;
 _cb:Tevconnlistener_cb;
begin
 if Data=nil then Exit;

 Listener:=Data^.FListener;
 if Listener=nil then Exit;

 FState:=load_acquire(Listener^.FState);

 Case FState of
  LST_ACCEPT:
  begin

   _cb:=Listener^.Fcb;
   if Assigned(_cb) then
   begin
    sa:=nil;
    socklen:=0;
    if GetAcceptEx(Data,sa,socklen) then
    begin
     _cb(Listener,Data^.FHandle,PSockAddrIn(sa),socklen,Listener^.FPtr);
    end else
    begin
     CloseSocket(Data^.FHandle);
    end;
   end;

   evconnlistener_AcceptEx(Listener,Data);
  end;
  LST_CLOSED,
  LST_DISABLE:
  begin
   evconnlistener_CloseEx(Listener,Data);
  end;
  LST_FREE:
  begin
   _evconnlistener_free(Listener);
  end;
 end;

end;

function evconnlistener_CloseEx(Listener:Pevconnlistener;New:Plistener_acceptex):Boolean; inline;
begin
 Result:=False;
 if Listener=nil then Exit;
 if New=nil then Exit;

 store_release(Listener^.FState,LST_DISABLE);
 CloseSocket(New^.FHandle);
 New^.FHandle:=INVALID_HANDLE_VALUE;

 Result:=True;
end;


function evconnlistener_AcceptEx(Listener:Pevconnlistener;New:Plistener_acceptex):Boolean;
Var
 base:Pevpoll;
 hSocket:THandle;
begin
 Result:=False;
 if Listener=nil then Exit;
 if New=nil then Exit;

 base:=Listener^.Fbase;
 if base=nil then Exit;

 hSocket:=WSASocket(getsock_family(Listener^.FHandle),SOCK_STREAM,IPPROTO_TCP,nil,0,WSA_FLAG_OVERLAPPED);
 if hSocket=INVALID_SOCKET then
 begin
  Exit;
 end;

 With New^ do
 begin
  O:=Default(TOVERLAPPED);
  FHandle:=hSocket;
  FListener:=Listener;
 end;

 store_release(Listener^.FState,LST_ACCEPT);


 Result:=AcceptEx(Listener^.FHandle,
                  hSocket,
                  @New^.BUF,
                  0,
                  @New^.O);

 if Result then
 begin
  Case WSAGetLastError() of
   0,WSA_IO_PENDING:;
   else
    Result:=False;
  end;
 end;

 if not Result then
 begin
  store_release(Listener^.FState,LST_DISABLE);
  New^.FHandle:=INVALID_SOCKET;
  CloseSocket(hSocket);
 end;

end;

//--evtimer--

function evtimer_new(base:Pevpoll;cb:Ttimer_cb;arg:pointer):Ptimer;
Var
 H:THandle;
begin
 Result:=nil;

 if not evpoll_isinit(base) then Exit;

 H:=CreateWaitableTimer(nil,true,nil);

 if (H=0) then Exit;

 Result:=AllocMem(SizeOf(Ttimer));

 With Ptimer(Result)^ do
 begin
  Fbase:=base;
  FCb  :=cb;
  FPtr:=arg;
  FHandle:=H;
  store_release(FState,ET_NEW);
 end;

end;

Procedure _evtimer_free(ev:Ptimer); inline;
begin
 FreeMem(ev);
end;

function evtimer_set_cb(ev:Ptimer;cb:Ttimer_cb;arg:pointer):Boolean; inline;
begin
 Result:=false;
 if ev=nil then Exit;
 With Ptimer(ev)^ do
 begin
  FCb  :=cb;
  FPtr:=arg;
 end;
 Result:=true;
end;

procedure wt_event(lpArgToCompletionRoutine:Pointer;dwTimerLowValue,dwTimerHighValue:DWORD); stdcall;
Var
 _Cb:Ttimer_cb;
begin
 if lpArgToCompletionRoutine=nil then Exit;
 With Ptimer(lpArgToCompletionRoutine)^ do
 begin
  _Cb:=FCb;
  if Assigned(_Cb) then
  begin

   if not CAS(FState,ET_WTT,ET_NEW) then
   begin
    _evtimer_free(Ptimer(lpArgToCompletionRoutine));
    Exit;
   end;

   _Cb(Ptimer(lpArgToCompletionRoutine),FPtr);
  end;
 end;
end;

Procedure wt_timer_add(NOBT:SizeUInt;ev:Ptimer);
Var
 f:Int64;
begin
 if Assigned(ev) then
 With Ptimer(ev)^ do
 begin

   if not CAS(FState,ET_PST,ET_WTT) then
   begin
    _evtimer_free(ev);
    Exit;
   end;

   f:=-Ftime*10;
   if not SetWaitableTimer(FHandle,f,0,@wt_event,ev,false) then
   begin
    CloseHandle(FHandle);
    _evtimer_free(ev);
    Exit;
   end;

 end;
end;

function evtimer_add(ev:Ptimer;tv:Ptimeval):Boolean;
var
 us:Int64;
begin
 Result:=false;
 if (tv=nil) then Exit;
 us:=tv^.tv_usec+(Int64(tv^.tv_sec)*1000000);
 Result:=evtimer_add(ev,us);
end;

function evtimer_add(ev:Ptimer;us:Int64):Boolean;
begin
 Result:=false;
 if (ev=nil) then Exit;
 With Ptimer(ev)^ do
 begin

  Result:=CAS(FState,ET_NEW,ET_PST);
  if not Result then Exit;

  Ftime:=us;

  if Assigned(Fbase) then
  begin
   Result:=_iocp_post(Fbase,0,PCTXProc(@wt_timer_add),POVERLAPPED(ev));
  end;

  if not Result then
  begin
   if not CAS(FState,ET_PST,ET_NEW) then
   begin
    _evtimer_free(ev);
   end;
  end;

 end;
end;

function evtimer_del(ev:Ptimer):Boolean;
var
 f:SizeUInt;
 Handle:THandle;
begin
 Result:=True;
 if (ev=nil) then Exit;
 With Ptimer(ev)^ do
 begin
  f:=XCHG(FState,ET_DEL);
  Case f of
   ET_PST,
   ET_DEL:Result:=False;
   ET_NEW:_evtimer_free(ev);
   ET_WTT:
   begin
    Handle:=XCHG(FHandle,0);
    Result:=not CancelWaitableTimer(Handle);
    CloseHandle(Handle);
   end;
  end;
 end;
end;

function _evtimer_stop(ev:Ptimer):Boolean; inline;
var
 f:SizeUInt;
begin
 Result:=false;
 if (ev=nil) then Exit;
 With Ptimer(ev)^ do
 begin
  f:=XCHG(FState,ET_NEW);
  Case f of
   ET_PST,
   ET_DEL:Result:=False;
   ET_NEW:Result:=True;
   ET_WTT:
   begin
    CancelWaitableTimer(load_acq_rel(FHandle));
    Result:=True;
   end;
  end;
 end;
end;

function evtimer_reuse(var ev:Ptimer;base:Pevpoll;cb:Ttimer_cb;arg:pointer):Boolean;
begin
 if _evtimer_stop(ev) then
 begin
  Result:=evtimer_set_cb(ev,cb,arg);
 end else
 begin
  ev:=evtimer_new(base,cb,arg);
  Result:=Assigned(ev);
 end;
end;

//rate limit

Procedure _rate_update(fs,time:SizeUint;var cpl:SizeUint); inline;
Var
 nb,ob:SizeUint;
begin
 if (fs<>0) then
 begin
  nb:=(time*fs div 1000); //time to bytes
  ob:=load_consume(cpl);
  if ob<nb then nb:=ob;
  fetch_sub(cpl,nb);
 end;
end;

procedure rate_group_update(rg:Prate_limit_group);
Var
 nt,ot,tt:SizeUint;

begin
 if not Assigned(rg) then Exit;

 nt:=SizeUint(Sysutils.GetTickCount64);
 ot:=load_consume(rg^.Ftm_rec);
 tt:=nt-ot;

 if tt<=500 then Exit;

 if CAS(rg^.Ftm_rec,ot,nt) then
 begin
  _rate_update(load_consume(rg^.Fspeed_r),tt,rg^.Fsp_cpl_r);
  _rate_update(load_consume(rg^.Fspeed_w),tt,rg^.Fsp_cpl_w);
 end;

end;

procedure _rate_get_limit(fs,cpl:SizeUint;var size,time:SizeUint;minsize,mintime,maxtime:SizeUint); inline;
begin
 if fs=0 then Exit;
 if (cpl>=fs) then
 begin
  cpl:=cpl-fs+size;
  time:=(cpl*1000) div fs; //bytes to time
  if time<mintime then time:=mintime else if time>maxtime then time:=maxtime; //200 1000
  size:=0;
 end else
 begin
  time:=0;
  cpl:=fs-cpl;
  if (cpl<minsize) and ((Sysutils.GetTickCount64 and $80)<>0) then //magic prior
  begin
   cpl:=minsize-cpl;
   time:=(cpl*1000) div fs; //bytes to time
   if time<mintime then time:=mintime else if time>maxtime then time:=maxtime; //200 1000
   size:=0;
  end else
  if size>cpl then
  begin
   if cpl<minsize then size:=minsize else size:=cpl; //1024
  end;

 end;
end;

procedure rate_group_get_limit_r(rg:Prate_limit_group;var size,time:SizeUint); inline;
begin
 if not Assigned(rg) then Exit;
 _rate_get_limit(load_consume(rg^.Fspeed_r),load_consume(rg^.Fsp_cpl_r),size,time,min_rbs,200,1000);
end;

procedure rate_group_get_limit_w(rg:Prate_limit_group;var size,time:SizeUint); inline;
begin
 if not Assigned(rg) then Exit;
 _rate_get_limit(load_consume(rg^.Fspeed_w),load_consume(rg^.Fsp_cpl_w),size,time,1024,200,1000);
end;

procedure rate_group_complite_r(rg:Prate_limit_group;size:SizeUint); inline;
begin
 if not Assigned(rg) then Exit;
 fetch_add(rg^.Fsp_cpl_r,size);
end;

procedure rate_group_complite_w(rg:Prate_limit_group;size:SizeUint); inline;
begin
 if not Assigned(rg) then Exit;
 fetch_add(rg^.Fsp_cpl_w,size);
end;

function rate_begin_read(rt:Prate_limit;Size:SizeUint;bev:PBufferevent;cb:Ttimer_cb):SizeUint;
var
 time:SizeUint;
begin
 Result:=Size;
 if not Assigned(rt) then Exit;
 if not Assigned(bev) then Exit;
 rate_group_update(rt^.Fgroup);
 time:=0;
 rate_group_get_limit_r(rt^.Fgroup,size,time);
 if (size=0) then
 begin
  if Assigned(cb) then
   if evtimer_reuse(rt^.Fr_timer,bev^.Fbase,cb,bev) then
   begin
    _bufferevent_inc_ref(bev);
    evtimer_add(rt^.Fr_timer,time*1000);
    Result:=0;
   end;
 end else
 begin
  Result:=Size;
 end;
end;

function rate_begin_write(rt:Prate_limit;Size:SizeUint;bev:PBufferevent;cb:Ttimer_cb):SizeUint;
var
 time:SizeUint;
begin
 Result:=Size;
 if not Assigned(rt) then Exit;
 if not Assigned(bev) then Exit;
 rate_group_update(rt^.Fgroup);
 time:=0;
 rate_group_get_limit_w(rt^.Fgroup,size,time);
 if (size=0) then
 begin
  if Assigned(cb) then
   if evtimer_reuse(rt^.Fw_timer,bev^.Fbase,cb,bev) then
   begin
    _bufferevent_inc_ref(bev);
    evtimer_add(rt^.Fw_timer,time*1000);
    Result:=0;
   end;
 end else
 begin
  Result:=Size;
 end;
end;

Procedure rate_end_read(rt:Prate_limit;Size:SizeUint); inline;
begin
 if not Assigned(rt) then Exit;
 rate_group_complite_r(rt^.Fgroup,Size);
end;

Procedure rate_end_write(rt:Prate_limit;Size:SizeUint); inline;
begin
 if not Assigned(rt) then Exit;
 rate_group_complite_w(rt^.Fgroup,Size);
end;

function _rate_free(rt:Prate_limit):SizeUInt; inline;
begin
 Result:=0;
 if not Assigned(rt) then Exit;
 if not evtimer_del(rt^.Fr_timer) then Inc(Result);
 if not evtimer_del(rt^.Fw_timer) then Inc(Result);
 FreeMem(rt);
end;

function _rate_disable(rt:Prate_limit):SizeUInt; inline;
begin
 Result:=0;
 if not Assigned(rt) then Exit;
 if not evtimer_del(rt^.Fr_timer) then Inc(Result);
 if not evtimer_del(rt^.Fw_timer) then Inc(Result);
end;

function bufferevent_set_rate_limit(bev:Pbufferevent;rg:Prate_limit_group):boolean;
begin
 Result:=False;
 {$IFNDEF NO_RATELIMIT}
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  Result:=bev^.be_ops^.BEV_CTRL_SET_RL(bev,rg);
 {$ENDIF}
end;

function bufferevent_get_rate_limit(bev:Pbufferevent):Prate_limit_group;
begin
 Result:=nil;
 {$IFNDEF NO_RATELIMIT}
 if Assigned(bev) then
 if Assigned(bev^.be_ops) then
  bev^.be_ops^.BEV_CTRL_GET_RL(bev,@Result);
 {$ENDIF}
end;

function _bufferevent_set_rate_limit(bev:Pbufferevent;rg:Prate_limit_group):boolean;
Var
 New:Prate_limit;
 dr:SizeUInt;
begin
 Result:=False;
 {$IFNDEF NO_RATELIMIT}
 With PBufferevent_sio(bev)^ do
 begin
  if Assigned(rg) then
  begin
   New:=load_acquire(Frate_limit);
   While (New=nil) do
   begin
    //check enable
    if (load_acq_rel(FHandle)=INVALID_HANDLE_VALUE) then Exit;

    New:=AllocMem(SizeOf(Trate_limit));
    if not CAS(Frate_limit,nil,New) then
    begin
     FreeMem(New);
     New:=load_acquire(Frate_limit);
    end;
   end;
   New^.Fgroup:=rg;
   Result:=True;
  end else
  begin
   //check enable
   if (load_acq_rel(FHandle)=INVALID_HANDLE_VALUE) then Exit;

   New:=XCHG(Frate_limit,nil);

   dr:=_rate_free(New);
   While (dr<>0) do
   begin
    _bufferevent_dec_ref(bev);
    Dec(dr);
   end;

   Result:=True;
  end;
 end;
 {$ENDIF}
end;

const
 TCP_KEEPIDLE = 4;
 TCP_KEEPINTVL = 5;
 TCP_KEEPCNT = 6;

 SIO_KEEPALIVE_VALS=IOC_IN or IOC_VENDOR or 4;

{$IFDEF WINDOWS}
type
 Ttcp_keepalive=packed record
  onoff,keepalivetime,keepaliveinterval:DWORD;
 end;
{$ENDIF}

Procedure SetKeepAlive(fd:THandle;Enable:Boolean;idle,int,cnt:dword);
Var
 {$IFDEF WINDOWS}
 alive:Ttcp_keepalive;
 {$ENDIF}
 val:dword;
begin
 {$IFDEF WINDOWS}
  if Enable then
  begin
   alive.onoff:=1;
   alive.keepalivetime    :=idle*1000; //msec
   alive.keepaliveinterval:=int *1000; //msec
   WSAIoctl(fd,SIO_KEEPALIVE_VALS,@alive,sizeof(alive),nil,0,@val,nil,nil);
   val:=cnt;
   setsockopt(fd,IPPROTO_TCP,TCP_KEEPCNT,@val,sizeof(val));
  end else
  begin
   val:=0;
   setsockopt(fd,SOL_SOCKET,SO_KEEPALIVE,@val,sizeof(val));
  end;
 {$ELSE}
  if Enable then
  begin
   val:=1;//on
   setsockopt(fd,SOL_SOCKET,SO_KEEPALIVE,@val,sizeof(val));
   val:=idle;//sec
   setsockopt(fd,IPPROTO_TCP,TCP_KEEPIDLE,@val,sizeof(val));
   val:=int; //sec
   setsockopt(fd,IPPROTO_TCP,TCP_KEEPINTVL,@val,sizeof(val));
   val:=cnt;
   setsockopt(fd,IPPROTO_TCP,TCP_KEEPCNT,@val,sizeof(val));
  end else
  begin
   val:=0;
   setsockopt(fd,SOL_SOCKET,SO_KEEPALIVE,@val,sizeof(val));
  end;
 {$ENDIF}
end;

end.

