unit subr_msgbuf;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_mtx,
 md_time,
 time,
 vuio,
 subr_uio;

const
 MSG_MAGIC       = $063062;  // magic value of a msgbuf
 MSGBUF_NEEDNL   = $01;      // set when newline needed
 MSGBUF_SIZE     = 32768*2;  // default kernel msgbuf size

 // Maximum number conversion buffer length: uintmax_t in base 2, plus <>
 // around the priority, and a terminating NUL.
 MAXPRIBUF       = SizeOf(Int64)*8+3;

type
 p_msgbuf=^t_msgbuf;
 t_msgbuf=packed record
  msg_ptr    :PChar;    // pointer to buffer
  msg_magic  :DWORD;    // MSG_MAGIC
  msg_size   :DWORD;    // size of buffer area
  msg_wseq   :DWORD;    // write sequence number
  msg_rseq   :DWORD;    // read sequence number
  msg_cksum  :DWORD;    // checksum of contents
  msg_seqmod :DWORD;    // range for sequence numbers
  msg_lastpri:Integer;  // saved priority value
  msg_flags  :DWORD;    // MSGBUF_NEEDNL &c
  msg_prefix :Integer;  // prefix state
  msg_lock   :mtx;      // mutex to protect the buffer
 end;

var
 // Timestamps in msgbuf are useful when trying to diagnose when core dumps
 // or other actions occured.
 msgbuf_show_timestamp:Integer=0;

procedure msgbuf_init     (mbp:p_msgbuf;ptr:Pointer;size:Integer);
procedure msgbuf_reinit   (mbp:p_msgbuf;ptr:Pointer;size:Integer);
procedure msgbuf_clear    (mbp:p_msgbuf);
function  msgbuf_getcount (mbp:p_msgbuf):Integer;
procedure msgbuf_addchar  (mbp:p_msgbuf;c:AnsiChar);
procedure msgbuf_addstr   (mbp:p_msgbuf;pri:Integer;str:PChar;filter_cr:Integer);
function  msgbuf_addstr   (mbp:p_msgbuf;str:PChar;len:SizeUInt;prefix:PChar;prefix_len:Integer):Integer;
procedure msgbuf_adduio   (mbp:p_msgbuf;uio:p_uio;prefix:PChar;prefix_len:Integer);
function  msgbuf_getchar  (mbp:p_msgbuf):Integer;
function  msgbuf_getbytes (mbp:p_msgbuf;buf:PChar;buflen:Integer):Integer;
function  msgbuf_peekread (mbp:p_msgbuf;buf:PPChar):Integer;
function  msgbuf_peekbytes(mbp:p_msgbuf;buf:PChar;buflen:Integer;seqp:PDWORD):Integer;
procedure msgbuf_copy     (src,dst:p_msgbuf);

implementation

// Forward declaration (called from msgbuf_reinit before its definition).
function msgbuf_cksum(mbp:p_msgbuf):DWORD; forward;

// Read/write sequence numbers are modulo a multiple of the buffer size.
function msgbuf_seqmod_size(size:DWORD):DWORD; inline;
begin
 Result:=size*16;
end;

// Normalise a sequence number or a difference between sequence numbers.
function msgbuf_seqnorm(mbp:p_msgbuf;seq:DWORD):DWORD; inline;
begin
 Result:=(seq+mbp^.msg_seqmod) mod mbp^.msg_seqmod;
end;

// Map a sequence number to a position within the buffer.
function msgbuf_seq_to_pos(mbp:p_msgbuf;seq:DWORD):DWORD; inline;
begin
 Result:=seq mod mbp^.msg_size;
end;

// Subtract sequence numbers.  Note that only positive values result.
function msgbuf_seqsub(mbp:p_msgbuf;seq1,seq2:DWORD):DWORD; inline;
begin
 Result:=msgbuf_seqnorm(mbp,seq1-seq2);
end;

procedure msgbuf_do_addchar(mbp:p_msgbuf;seq:PDWORD;c:AnsiChar);
var
 pos:DWORD;
begin
 // Make sure we properly wrap the sequence number.
 pos:=msgbuf_seq_to_pos(mbp,seq^);
 mbp^.msg_cksum:=mbp^.msg_cksum+(DWORD(c)-DWORD(mbp^.msg_ptr[pos]));
 mbp^.msg_ptr[pos]:=c;
 seq^:=msgbuf_seqnorm(mbp,seq^+1);
end;

{
 * Initialize a message buffer of the specified size at the specified
 * location. This also zeros the buffer area.
 }
procedure msgbuf_init(mbp:p_msgbuf;ptr:Pointer;size:Integer);
begin
 mbp^.msg_ptr   :=PChar(ptr);
 mbp^.msg_size  :=DWORD(size);
 mbp^.msg_seqmod:=msgbuf_seqmod_size(DWORD(size));

 msgbuf_clear(mbp);

 mbp^.msg_magic:=MSG_MAGIC;
 mbp^.msg_lastpri:=-1;
 mbp^.msg_flags:=0;
 mbp^.msg_prefix:=0;

 mtx_init(mbp^.msg_lock,'msgbuf');
end;

{
 * Reinitialize a message buffer, retaining its previous contents if
 * the size and checksum are correct. If the old contents cannot be
 * recovered, the message buffer is cleared.
 }
procedure msgbuf_reinit(mbp:p_msgbuf;ptr:Pointer;size:Integer);
var
 cksum:DWORD;
begin
 if (mbp^.msg_magic<>MSG_MAGIC) or (mbp^.msg_size<>DWORD(size)) then
 begin
  msgbuf_init(mbp,ptr,size);
  Exit;
 end;
 mbp^.msg_seqmod:=msgbuf_seqmod_size(DWORD(size));
 mbp^.msg_wseq:=msgbuf_seqnorm(mbp,mbp^.msg_wseq);
 mbp^.msg_rseq:=msgbuf_seqnorm(mbp,mbp^.msg_rseq);
 mbp^.msg_ptr:=PChar(ptr);
 cksum:=msgbuf_cksum(mbp);
 if (cksum<>mbp^.msg_cksum) then
 begin
  //
  msgbuf_clear(mbp);
 end;
 mbp^.msg_lastpri:=-1;
 // Assume that the old message buffer didn't end in a newline.
 mbp^.msg_flags:=mbp^.msg_flags or MSGBUF_NEEDNL;
 FillChar(mbp^.msg_lock,SizeOf(mbp^.msg_lock),0);
 mtx_init(mbp^.msg_lock,'msgbuf');
end;

{
 * Clear the message buffer.
 }
procedure msgbuf_clear(mbp:p_msgbuf);
begin
 FillChar(mbp^.msg_ptr[0],mbp^.msg_size,0);
 mbp^.msg_wseq:=0;
 mbp^.msg_rseq:=0;
 mbp^.msg_cksum:=0;
end;

{
 * Get a count of the number of unread characters in the message buffer.
 }
function msgbuf_getcount(mbp:p_msgbuf):Integer;
var
 len:DWORD;
begin
 len:=msgbuf_seqsub(mbp,mbp^.msg_wseq,mbp^.msg_rseq);
 if (len>mbp^.msg_size) then
 begin
  len:=mbp^.msg_size;
 end;
 Result:=Integer(len);
end;

{
 * Append a character to a message buffer.
 }
procedure msgbuf_addchar(mbp:p_msgbuf;c:AnsiChar);
begin
 mtx_lock(mbp^.msg_lock);
 msgbuf_do_addchar(mbp,@mbp^.msg_wseq,c);
 mtx_unlock(mbp^.msg_lock);
end;

{
 * Append a NUL-terminated string with a priority to a message buffer.
 * Filter carriage returns if the caller requests it.
 *
 * XXX The carriage return filtering behavior is present in the
 * msglogchar() API, however testing has shown that we don't seem to send
 * carriage returns down this path.  So do we still need it?
 }
procedure msgbuf_addstr(mbp:p_msgbuf;pri:Integer;str:PChar;filter_cr:Integer);
var
 seq:DWORD;
 len,prefix_len:SizeUInt;
 prefix:array[0..MAXPRIBUF-1] of AnsiChar;
 ts:AnsiString;
 i,j:SizeUInt;
 needtime:Integer;
begin
 len:=StrLen(str);
 prefix_len:=0;
 needtime  :=0;

 // If we have a zero-length string, no need to do anything.
 if (len=0) then Exit;

 mtx_lock(mbp^.msg_lock);

 // If this is true, we may need to insert a new priority sequence,
 // so prepare the prefix.
 if (pri<>-1) then
 begin
  StrFmt(prefix,'<%d>',[pri]);
  prefix_len:=StrLen(prefix);
 end;

 // Starting write sequence number.
 seq:=mbp^.msg_wseq;

 {
  * Whenever there is a change in priority, we have to insert a
  * newline, and a priority prefix if the priority is not -1.  Here
  * we detect whether there was a priority change, and whether we
  * did not end with a newline.  If that is the case, we need to
  * insert a newline before this string.
  }
 if (mbp^.msg_lastpri<>pri) and ((mbp^.msg_flags and MSGBUF_NEEDNL)<>0) then
 begin
  msgbuf_do_addchar(mbp,@seq,#10);
  mbp^.msg_flags:=mbp^.msg_flags and (not MSGBUF_NEEDNL);
 end;

 needtime:=1;
 for i:=0 to len-1 do
 begin
  {
   * If we just had a newline, and the priority is not -1
   * (and therefore prefix_len != 0), then we need a priority
   * prefix for this line.
   }
  if ((mbp^.msg_flags and MSGBUF_NEEDNL)=0) and (prefix_len<>0) then
  begin
   for j:=0 to prefix_len-1 do
   begin
    msgbuf_do_addchar(mbp,@seq,prefix[j]);
   end;
  end;

  if (msgbuf_show_timestamp<>0) and (needtime=1) and
     ((mbp^.msg_flags and MSGBUF_NEEDNL)=0) then
  begin
   ts:='['+IntToStr(get_unit_uptime div UNIT_PER_SEC)+'] ';
   for j:=1 to Length(ts) do
   begin
    msgbuf_do_addchar(mbp,@seq,ts[j]);
   end;
   needtime:=0;
  end;

  {
   * Don't copy carriage returns if the caller requested
   * filtering.
   *
   * XXX This matches the behavior of msglogchar(), but is it
   * necessary?  Testing has shown that we don't seem to get
   * carriage returns here.
   }
  if (filter_cr<>0) and (str[i]=#13) then
  begin
   Continue;
  end;

  {
   * Clear this flag if we see a newline.  This affects whether
   * we need to insert a new prefix or insert a newline later.
   }
  if (str[i]=#10) then
   mbp^.msg_flags:=mbp^.msg_flags and (not MSGBUF_NEEDNL)
  else
   mbp^.msg_flags:=mbp^.msg_flags or MSGBUF_NEEDNL;

  msgbuf_do_addchar(mbp,@seq,str[i]);
 end;

 {
  * Update the write sequence number for the actual number of
  * characters we put in the message buffer.  (Depends on whether
  * carriage returns are filtered.)
  }
 mbp^.msg_wseq:=seq;

 // Set the last priority.
 mbp^.msg_lastpri:=pri;

 mtx_unlock(mbp^.msg_lock);
end;

function msgbuf_addstr(mbp:p_msgbuf;str:PChar;len:SizeUInt;prefix:PChar;prefix_len:Integer):Integer;
label
 _break_break;
var
 wseq,rseq,size:DWORD;
 j:Integer;
begin
 Result:=0;

 // If we have a zero-length string, no need to do anything.
 if (len=0) then Exit;

 mtx_lock(mbp^.msg_lock);

 // Starting write sequence number.
 wseq:=mbp^.msg_wseq;
 rseq:=mbp^.msg_rseq;
 size:=mbp^.msg_size;

 while (len<>0) do
 begin

  if (msgbuf_seqsub(mbp,wseq,rseq)=size) then
  begin
   Break;
  end;

  {
   * If we just had a newline, and the priority is not -1
   * (and therefore prefix_len != 0), then we need a priority
   * prefix for this line.
   }
  if ((mbp^.msg_flags and MSGBUF_NEEDNL)=0) and (prefix_len<>0) then
  begin
   j:=mbp^.msg_prefix;

   for j:=j to prefix_len-1 do
   begin

    if (msgbuf_seqsub(mbp,wseq,rseq)=size) then
    begin
     mbp^.msg_prefix:=j;
     goto _break_break;
    end;

    msgbuf_do_addchar(mbp,@wseq,prefix[j]);
   end;

   mbp^.msg_prefix:=0;
  end;

  {
   * Clear this flag if we see a newline.  This affects whether
   * we need to insert a new prefix or insert a newline later.
   }
  if (str[0]=#10) then
   mbp^.msg_flags:=mbp^.msg_flags and (not MSGBUF_NEEDNL)
  else
   mbp^.msg_flags:=mbp^.msg_flags or MSGBUF_NEEDNL;

  msgbuf_do_addchar(mbp,@wseq,str[0]);

  Inc(Result);
  Inc(str);
  Dec(len);
 end;

 _break_break:

 {
  * Update the write sequence number for the actual number of
  * characters we put in the message buffer.  (Depends on whether
  * carriage returns are filtered.)
  }
 mbp^.msg_wseq:=wseq;

 mtx_unlock(mbp^.msg_lock);
end;

procedure msgbuf_adduio(mbp:p_msgbuf;uio:p_uio;prefix:PChar;prefix_len:Integer);
var
 iov:p_iovec;
 cnt,n:QWORD;
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

  n:=msgbuf_addstr(mbp,iov^.iov_base,cnt,prefix,prefix_len);

  Inc(iov^.iov_base  ,n);
  Dec(iov^.iov_len   ,n);
  Dec(uio^.uio_resid ,n);
  Inc(uio^.uio_offset,n);

  if (n<cnt) then
  begin
   Break;
  end;

 end;

end;

{
 * Read and mark as read a character from a message buffer.
 * Returns the character, or -1 if no characters are available.
 }
function msgbuf_getchar(mbp:p_msgbuf):Integer;
var
 len,wseq:DWORD;
begin
 mtx_lock(mbp^.msg_lock);

 wseq:=mbp^.msg_wseq;
 len:=msgbuf_seqsub(mbp,wseq,mbp^.msg_rseq);

 if (len=0) then
 begin
  mtx_unlock(mbp^.msg_lock);
  Exit(-1);
 end;

 if (len>mbp^.msg_size) then
 begin
  mbp^.msg_rseq:=msgbuf_seqnorm(mbp,wseq-mbp^.msg_size);
 end;

 Result:=Integer(Byte(mbp^.msg_ptr[msgbuf_seq_to_pos(mbp,mbp^.msg_rseq)]));
 mbp^.msg_rseq:=msgbuf_seqnorm(mbp,mbp^.msg_rseq+1);

 mtx_unlock(mbp^.msg_lock);
end;

{
 * Read and mark as read a number of characters from a message buffer.
 * Returns the number of characters that were placed in `buf'.
 }
function msgbuf_getbytes(mbp:p_msgbuf;buf:PChar;buflen:Integer):Integer;
var
 len,pos,wseq:DWORD;
begin
 mtx_lock(mbp^.msg_lock);

 wseq:=mbp^.msg_wseq;
 len:=msgbuf_seqsub(mbp,wseq,mbp^.msg_rseq);

 if (len=0) then
 begin
  mtx_unlock(mbp^.msg_lock);
  Exit(0);
 end;

 if (len>mbp^.msg_size) then
 begin
  mbp^.msg_rseq:=msgbuf_seqnorm(mbp,wseq-mbp^.msg_size);
  len:=mbp^.msg_size;
 end;

 pos:=msgbuf_seq_to_pos(mbp,mbp^.msg_rseq);
 if (len>mbp^.msg_size-pos) then len:=mbp^.msg_size-pos;
 if (len>DWORD(buflen)) then len:=DWORD(buflen);

 if (buf<>nil) then
 begin
  Move(mbp^.msg_ptr[pos],buf[0],len);
 end;

 mbp^.msg_rseq:=msgbuf_seqnorm(mbp,mbp^.msg_rseq+len);

 mtx_unlock(mbp^.msg_lock);

 Result:=Integer(len);
end;

function msgbuf_peekread(mbp:p_msgbuf;buf:PPChar):Integer;
var
 len,pos,wseq,rseq:DWORD;
begin
 mtx_lock(mbp^.msg_lock);

 wseq:=mbp^.msg_wseq;
 rseq:=mbp^.msg_rseq;

 len:=msgbuf_seqsub(mbp,wseq,rseq);

 if (len=0) then
 begin
  mtx_unlock(mbp^.msg_lock);
  Exit(0);
 end;

 if (len>mbp^.msg_size) then
 begin
  rseq:=msgbuf_seqnorm(mbp,wseq-mbp^.msg_size);
  len:=mbp^.msg_size;
 end;

 pos:=msgbuf_seq_to_pos(mbp,rseq);
 if (len>mbp^.msg_size-pos) then len:=mbp^.msg_size-pos;

 if (buf<>nil) then
 begin
  buf^:=@mbp^.msg_ptr[pos];
 end;

 mtx_unlock(mbp^.msg_lock);

 Result:=Integer(len);
end;

{
 * Peek at the full contents of a message buffer without marking any
 * data as read. `seqp' should point to an unsigned integer that
 * msgbuf_peekbytes() can use to retain state between calls so that
 * the whole message buffer can be read in multiple short reads.
 * To initialise this variable to the start of the message buffer,
 * call msgbuf_peekbytes() with a NULL `buf' parameter.
 *
 * Returns the number of characters that were placed in `buf'.
 }
function msgbuf_peekbytes(mbp:p_msgbuf;buf:PChar;buflen:Integer;seqp:PDWORD):Integer;
var
 len,pos,wseq:DWORD;
begin
 mtx_lock(mbp^.msg_lock);

 if (buf=nil) then
 begin
  // Just initialise *seqp.
  seqp^:=msgbuf_seqnorm(mbp,mbp^.msg_wseq-mbp^.msg_size);
  mtx_unlock(mbp^.msg_lock);
  Exit(0);
 end;

 wseq:=mbp^.msg_wseq;
 len:=msgbuf_seqsub(mbp,wseq,seqp^);
 if (len=0) then
 begin
  mtx_unlock(mbp^.msg_lock);
  Exit(0);
 end;

 if (len>mbp^.msg_size) then
 begin
  seqp^:=msgbuf_seqnorm(mbp,wseq-mbp^.msg_size);
  len:=mbp^.msg_size;
 end;

 pos:=msgbuf_seq_to_pos(mbp,seqp^);
 if (len>mbp^.msg_size-pos) then len:=mbp^.msg_size-pos;
 if (len>DWORD(buflen)) then len:=DWORD(buflen);

 Move(mbp^.msg_ptr[pos],buf[0],len);
 seqp^:=msgbuf_seqnorm(mbp,seqp^+len);

 mtx_unlock(mbp^.msg_lock);

 Result:=Integer(len);
end;

{
 * Compute the checksum for the complete message buffer contents.
 }
function msgbuf_cksum(mbp:p_msgbuf):DWORD;
var
 i:DWORD;
 sum:DWORD;
begin
 sum:=0;
 for i:=0 to mbp^.msg_size-1 do
 begin
  sum:=sum+DWORD(Byte(mbp^.msg_ptr[i]));
 end;
 Result:=sum;
end;

{
 * Copy from one message buffer to another.
 }
procedure msgbuf_copy(src,dst:p_msgbuf);
var
 c:Integer;
begin
 while True do
 begin
  c:=msgbuf_getchar(src);
  if (c<0) then Break;
  msgbuf_addchar(dst,AnsiChar(c));
 end;
end;




end.


