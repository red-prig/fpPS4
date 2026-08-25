unit md_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 md_systm,
 windows,
 ntapi,
 vuio,
 subr_uio,
 sys_tty,
 kern_mtx,
 subr_msgbuf,
 md_file;

const
 TTY_STACKBUF=256;

type
 p_priv_tty=^t_priv_tty;
 t_priv_tty=record
  rd_handle:THandle;
  wr_handle:THandle;

  wr_prefx:PChar;

  wr_msgbuf:t_msgbuf;
 end;

function md_tty_new(_rd_handle,_wr_handle:THandle):p_priv_tty;

function md_tty_read_poll (tp:p_tty;priv:p_priv_tty):QWORD;
function md_tty_write_poll(tp:p_tty;priv:p_priv_tty):QWORD;

function md_tty_read (tp:p_tty;priv:p_priv_tty;uio:p_uio;ioflag:Integer):Integer;
function md_tty_write(tp:p_tty;priv:p_priv_tty;uio:p_uio;ioflag:Integer):Integer;

procedure md_init_tty;

implementation

uses
 kern_thr;

function md_tty_read_poll(tp:p_tty;priv:p_priv_tty):QWORD;
var
 N:DWORD;
begin
 N:=0;

 case GetFileType(priv^.rd_handle) of
  FILE_TYPE_DISK:
   begin
    N:=1;
   end;
  FILE_TYPE_CHAR:
   begin
    GetNumberOfConsoleInputEvents(priv^.rd_handle,@N);
   end;
  FILE_TYPE_PIPE:
   begin
    PeekNamedPipe(priv^.rd_handle,
                  nil,
                  0,
                  nil,
                  @N,
                  nil);
   end;
  else;
 end;

 Result:=N;
end;

function md_tty_write_poll(tp:p_tty;priv:p_priv_tty):QWORD;
begin
 Result:=1;
end;

function _ttydisc_read(tp:THandle;uio:p_uio;buf_addr:Pointer):Integer;
var
 BLK   :IO_STATUS_BLOCK;
 OFFSET:Int64;
begin
 //init
 BLK   :=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_USE_FILE_POINTER_POSITION_L);
 //
 NtReadFile(tp,0,nil,nil,@BLK,buf_addr,uio^.uio_resid,@OFFSET,nil);
 //
 Result:=uiomove(buf_addr, BLK.Information, uio);
end;

function _ttydisc_read0(tp:THandle;uio:p_uio):Integer; inline;
var
 BUF:array[0..TTY_STACKBUF-1] of AnsiChar;
begin
 Result:=_ttydisc_read(tp,uio,@BUF);
end;

function md_tty_read(tp:p_tty;priv:p_priv_tty;uio:p_uio;ioflag:Integer):Integer;
begin
 uio^.uio_td:=curkthread;
 if (uio^.uio_td=nil) then
 begin
  if (uio^.uio_resid<=TTY_STACKBUF) then
  begin
   Result:=_ttydisc_read0(priv^.rd_handle,uio);
  end else
  begin
   uio^.uio_td:=GetMem(uio^.uio_resid);
   Result:=_ttydisc_read(priv^.rd_handle,uio,uio^.uio_td);
   FreeMem(uio^.uio_td);
   uio^.uio_td:=nil;
  end;
 end else
 begin
  Result:=_ttydisc_read(priv^.rd_handle,uio,thread_get_local_buffer(uio^.uio_td,uio^.uio_resid));
 end;
end;

type
 p_tcb=^t_tcb;
 t_tcb=record
  tcb_self  :Pointer;
  tcb_dtv   :Pointer;
  tcb_thread:Pointer;
  tcb_spare :array[0..2] of Pointer;
  tcb_fbdata:Pointer;
 end;

function get_fiber_self(td:p_kthread):Pointer;
var
 tcb_fbdata:Pointer;
begin
 Result:=nil;
 if (td=nil) then Exit;
 if (td^.pcb_fsbase=nil) then Exit;

 tcb_fbdata:=md_fuword(p_tcb(td^.pcb_fsbase)^.tcb_fbdata);

 if (tcb_fbdata=Pointer(-1)) then tcb_fbdata:=nil;

 if (tcb_fbdata<>nil) then
 begin
  Result:=md_fuword(PPointer(tcb_fbdata+$48)^);

  if (Result=Pointer(-1)) then Result:=nil;
 end;
end;

const
 CODES:array[0..31] of Byte=(
  $5A, $5A, $66, $66,
  $99, $99, $66, $96,
  $99, $66, $99, $96,
  $33, $33, $CC, $CC,
  $33, $C3, $CC, $33,
  $CC, $C3, $55, $55,
  $AA, $AA, $55, $A5,
  $AA, $55, $AA, $A5
);

function fiber_decode_name(src,dst:pchar):Integer;
var
 i:DWORD;
 val:Byte;
begin
 i:=0;
 while True do
 begin
  val:=CODES[i] xor Byte(src[i]);
  dst[i]:=char(val);
  if (val<>0) and (i=30) then Break;
  i:=i+1;
  if (val=0) then
  begin
   Exit(i);
  end;
 end;
 Exit(i);
end;

function get_fiber_name(fiber:Pointer;dst:pchar):Integer;
var
 name:t_td_name;
begin
 Result:=0;
 if (fiber=nil) or (dst=nil) then Exit;

 name:=Default(t_td_name);

 md_copyin((fiber+$28),@name,SizeOf(t_td_name),nil);

 Result:=fiber_decode_name(@name,dst);
end;

//  if (td^.td_name='SceVideoOutServiceThread') then exit;

procedure uio_drain(uio:p_uio);
var
 iov:p_iovec;
 cnt:QWORD;
begin
 while (uio^.uio_resid > 0) do
 begin
  iov:=uio^.uio_iov;
  cnt:=iov^.iov_len;

  if (cnt=0) then
  begin
   Inc(uio^.uio_iov);
   Dec(uio^.uio_iovcnt);
   continue;
  end;

  Inc(iov^.iov_base  ,cnt);
  Dec(iov^.iov_len   ,cnt);
  Dec(uio^.uio_resid ,cnt);
  Inc(uio^.uio_offset,cnt);
 end;
end;

function md_tty_write(tp:p_tty;priv:p_priv_tty;uio:p_uio;ioflag:Integer):Integer;
var
 BLK   :IO_STATUS_BLOCK;
 OFFSET:Int64;
 LEN   :QWORD;

 _td_name :pchar;
 _td_tid  :DWORD;
 _fib_addr:pchar;

 buf:PChar;

 prefix:PChar;
 prefix_len:Integer;

 _tid_str:array[0..7] of AnsiChar;
begin
 Result:=0;

 if (priv=nil) then Exit;

 if (priv^.wr_handle=INVALID_HANDLE_VALUE) then
 begin
  uio_drain(uio);
  Exit;
 end;

 _td_name :='';
 _td_tid  :=0;
 _fib_addr:=nil;

 if (curkthread<>nil) then
 with curkthread^ do
 begin
  _td_name:=@td_name;
  _td_tid :=td_tid;

  if (_td_name[0]=#0) then
  begin
   _tid_str:=IntToStr(_td_tid);
   _td_name:=@_tid_str;
  end;

  if (tty_prefix.bits and 4)<>0 then
  begin
   _fib_addr:=get_fiber_self(curkthread);
  end;

 end;

 OFFSET:=Int64(FILE_USE_FILE_POINTER_POSITION_L);

 prefix    :=nil;
 prefix_len:=0;

 mtx_lock(priv^.wr_msgbuf.msg_lock);

  prefix:=ReAllocMem(priv^.wr_prefx,tty_prefix.max);

  StrLFmt(prefix,tty_prefix.max,pchar(tty_prefix.fmt),[tp^.t_name,_td_name,_td_tid,QWORD(_fib_addr)]);

  prefix_len:=strlen(prefix);

  while (uio^.uio_resid > 0) do
  begin

   msgbuf_adduio(@priv^.wr_msgbuf, uio, prefix, prefix_len);

   LEN:=msgbuf_peekread(@priv^.wr_msgbuf,@buf);

   while (LEN<>0) do
   begin
    BLK:=Default(IO_STATUS_BLOCK);

    Result:=NtWriteFile(priv^.wr_handle,0,nil,nil,@BLK,buf,LEN,@OFFSET,nil);
    //
    if (Result=STATUS_PENDING) then
    begin
     Result:=NtWaitForSingleObject(priv^.wr_handle,False,nil);
     if (Result=0) then
     begin
      Result:=BLK.Status;
     end;
    end;
    //

    //drain
    msgbuf_getbytes(@priv^.wr_msgbuf,nil,LEN);

    LEN:=msgbuf_peekread(@priv^.wr_msgbuf,@buf);
   end;

  end;

 mtx_unlock(priv^.wr_msgbuf.msg_lock);

 if (tp^.t_update<>nil) then
 begin
  tp^.t_update();
 end;

 Result:=0; //ignore errors
end;

function md_tty_new(_rd_handle,_wr_handle:THandle):p_priv_tty;
begin
 Result:=AllocMem(sizeof(t_priv_tty)+MSGBUF_SIZE);

 Result^.rd_handle:=_rd_handle;
 Result^.wr_handle:=_wr_handle;

 msgbuf_init(@Result^.wr_msgbuf, Result+1, MSGBUF_SIZE);
end;

procedure md_init_tty;
var
 i:Integer;

 priv:p_priv_tty;
 wr_handle:THandle;
begin
 For i:=0 to High(tty_init_array) do
 with tty_init_array[i].tp^ do
  if (t_target<>nil) then
  begin
   priv:=t_target^.priv;
   //
   if (priv=nil) then
   begin

    case LowerCase(RawByteString(t_target^.target)) of
     'null'  :wr_handle:=INVALID_HANDLE_VALUE;
     'stdin' :wr_handle:=StdInputHandle;
     'stdout':wr_handle:=StdOutputHandle;
     'stderr':wr_handle:=StdErrorHandle;
     else
              begin
               wr_handle:=INVALID_HANDLE_VALUE;
               md_open(t_target^.target,O_WRONLY or O_CREAT or O_APPEND,&0777,wr_handle);
              end;
    end;

    priv:=md_tty_new(StdInputHandle,wr_handle);
    t_target^.priv:=priv;
   end;
   //
   t_priv:=priv;
  end;
end;

initialization


end.



