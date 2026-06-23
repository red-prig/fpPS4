unit md_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 md_systm,
 windows,
 ntapi,
 vuio,
 subr_uio,
 sys_tty;

const
 TTY_STACKBUF=256;

function  ttydisc_read_poll (tp:p_tty):QWORD;
function  ttydisc_write_poll(tp:p_tty):QWORD;

function  ttydisc_read (tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
function  ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;

implementation

uses
 kern_thr;

function ttydisc_read_poll(tp:p_tty):QWORD;
var
 N:DWORD;
begin
 N:=0;

 case GetFileType(tp^.t_rd_handle) of
  FILE_TYPE_DISK:
   begin
    N:=1;
   end;
  FILE_TYPE_CHAR:
   begin
    GetNumberOfConsoleInputEvents(tp^.t_rd_handle,@N);
   end;
  FILE_TYPE_PIPE:
   begin
    PeekNamedPipe(tp^.t_rd_handle,
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

function ttydisc_write_poll(tp:p_tty):QWORD;
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

function ttydisc_read(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
begin
 uio^.uio_td:=curkthread;
 if (uio^.uio_td=nil) then
 begin
  if (uio^.uio_resid<=TTY_STACKBUF) then
  begin
   Result:=_ttydisc_read0(tp^.t_rd_handle,uio);
  end else
  begin
   uio^.uio_td:=GetMem(uio^.uio_resid);
   Result:=_ttydisc_read(tp^.t_rd_handle,uio,uio^.uio_td);
   FreeMem(uio^.uio_td);
   uio^.uio_td:=nil;
  end;
 end else
 begin
  Result:=_ttydisc_read(tp^.t_rd_handle,uio,thread_get_local_buffer(uio^.uio_td,uio^.uio_resid));
 end;
end;

function tty_get_full_size(tp:p_tty;uio:p_uio):QWORD; inline;
begin
 Result:=0;
 //
 if (tp^.t_newline<>0) then
 begin
  if ((tp^.t_flags and TF_TTY_NAME_PREFIX)<>0) then
  begin
   Result:=Result+tp^.t_nlen;
  end;
  //
  if (uio^.uio_td<>nil) then
  if ((tp^.t_flags and TF_THD_NAME_PREFIX)<>0) then
  begin
   with p_kthread(uio^.uio_td)^ do
   begin
    if td_name[0]=#0 then
    begin
     Result:=Result+8+3;
    end else
    begin
     Result:=Result+strlen(@td_name)+3;
    end;
   end;
   if ((tp^.t_flags and TF_FIB_ADDR_PREFIX)<>0) then
   begin
    Result:=Result+4+10;
   end;
  end;
 end;
 //
 Result:=Result+uio^.uio_resid;
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

function _ttydisc_write(tp:p_tty;uio:p_uio;buf_addr:Pointer):Integer;
var
 CURR  :Pointer;
 BLK   :IO_STATUS_BLOCK;
 OFFSET:Int64;
 LEN   :QWORD;

 fiber:Pointer;

 procedure WRITE(ch:AnsiChar); inline;
 begin
  PAnsiChar(CURR)[0]:=ch;
  Inc(CURR,1);
  Inc(LEN ,1);
 end;

 procedure WRITE(N:Pointer;L:QWORD); inline;
 begin
  Move(N^,CURR^,L);
  Inc(CURR,L);
  Inc(LEN ,L);
 end;

 procedure WRITE(const S:RawByteString); inline;
 begin
  WRITE(pchar(s),Length(s));
 end;

begin
 Result:=0;
 //init
 BLK   :=Default(IO_STATUS_BLOCK);
 OFFSET:=Int64(FILE_USE_FILE_POINTER_POSITION_L);
 CURR  :=buf_addr;
 LEN   :=0;
 //
 if (tp^.t_newline<>0) then
 begin
  //tty name
  if ((tp^.t_flags and TF_TTY_NAME_PREFIX)<>0) then
  begin
   WRITE(tp^.t_name,tp^.t_nlen);
  end;
  //thread name
  if (uio^.uio_td<>nil) then
  if ((tp^.t_flags and TF_THD_NAME_PREFIX)<>0) then
  begin
   WRITE('(');

   with p_kthread(uio^.uio_td)^ do
   begin
    if td_name[0]=#0 then
    begin
     WRITE('#'+HexStr(td_tid,7));
    end else
    begin
     WRITE(@p_kthread(uio^.uio_td)^.td_name,strlen(@td_name));
    end;
   end;

   if ((tp^.t_flags and TF_FIB_ADDR_PREFIX)<>0) then
   begin
    fiber:=get_fiber_self(uio^.uio_td);
    if (fiber<>nil) then
    begin
     WRITE(':F0x'+HexStr(QWORD(fiber),10));
    end;
   end;

   WRITE(pchar('):'),2);
  end;
 end;
 //text
 LEN:=LEN+uio^.uio_resid;
 Result:=uiomove(CURR, uio^.uio_resid, uio);
 CURR:=buf_addr+LEN-1;
 //
 tp^.t_newline:=ord((PAnsiChar(CURR)^=#13) or (PAnsiChar(CURR)^=#10));
 //
 Result:=NtWriteFile(tp^.t_wr_handle,0,nil,nil,@BLK,buf_addr,LEN,@OFFSET,nil);
 //
 if (Result=STATUS_PENDING) then
 begin
  Result:=NtWaitForSingleObject(tp^.t_wr_handle,False,nil);
  if (Result=0) then
  begin
   Result:=BLK.Status;
  end;
 end;
 //
 Result:=0; //ignore errors
 //
 if (tp^.t_update<>nil) then
 begin
  tp^.t_update();
 end;
end;

function _ttydisc_write0(tp:p_tty;uio:p_uio):Integer; inline;
var
 BUF:array[0..TTY_STACKBUF-1] of AnsiChar;
begin
 Result:=_ttydisc_write(tp,uio,@BUF);
end;

function ttydisc_write(tp:p_tty;uio:p_uio;ioflag:Integer):Integer;
var
 size:QWORD;
 buf:Pointer;
begin
 uio^.uio_td:=curkthread;
 size:=tty_get_full_size(tp,uio);
 if (uio^.uio_td=nil) then
 begin
  if (size<=TTY_STACKBUF) then
  begin
   Result:=_ttydisc_write0(tp,uio);
  end else
  begin
   buf:=GetMem(size);
   Result:=_ttydisc_write(tp,uio,buf);
   FreeMem(buf);
  end;
 end else
 begin
  Result:=_ttydisc_write(tp,uio,thread_get_local_buffer(uio^.uio_td,size));
 end;
end;

procedure md_init_tty; register;
var
 i:Integer;
begin
 For i:=0 to High(std_tty) do
 begin
  std_tty[i].t_rd_handle:=StdInputHandle;
  std_tty[i].t_wr_handle:=StdOutputHandle;
 end;

 For i:=0 to High(deci_tty) do
 begin
  deci_tty[i].t_rd_handle:=StdInputHandle;
  deci_tty[i].t_wr_handle:=StdOutputHandle;
 end;

 std_tty [2].t_wr_handle:=StdErrorHandle;
 deci_tty[2].t_wr_handle:=StdErrorHandle;

 debug_tty.t_rd_handle:=StdInputHandle;
 debug_tty.t_wr_handle:=StdOutputHandle;
end;

initialization
 init_tty:=@md_init_tty;

end.



