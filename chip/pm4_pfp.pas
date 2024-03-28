unit pm4_pfp;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 bittype,
 pm4_ring,
 pm4defs;

type
 p_pfp_ctx=^t_pfp_ctx;

 t_pm4_parse_cb=function(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;

 p_pm4_ibuffer=^t_pm4_ibuffer;
 t_pm4_ibuffer=record
  next:TAILQ_ENTRY;
  buff:Pointer;
  size:Ptruint;
  bpos:Ptruint;
  picb:t_pm4_parse_cb;
 end;

 t_pfp_ctx=object
  lastn:TAILQ_HEAD;
  stall:TAILQ_HEAD;
  procedure add_stall(ibuf:p_pm4_ibuffer);
  procedure free;
 end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;buff:Pointer;size:Ptruint;icb:t_pm4_parse_cb):Boolean;
function pm4_ibuf_init(ibuf:p_pm4_ibuffer;buf:PPM4CMDINDIRECTBUFFER;icb:t_pm4_parse_cb):Boolean;

function pm4_ibuf_parse(pctx:p_pfp_ctx;ibuf:p_pm4_ibuffer):Integer;

implementation

uses
 kern_dmem;

function PM4_TYPE(token:DWORD):Byte; inline;
begin
 Result:=(token shr 30) and 3;
end;

function PM4_LENGTH(token:DWORD):DWORD; inline;
begin
 Result:=((token shr 14) and $FFFC) + 8;
end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;buff:Pointer;size:Ptruint;icb:t_pm4_parse_cb):Boolean;
begin
 Result:=True;
 ibuf^.next:=Default(TAILQ_ENTRY);
 ibuf^.buff:=buff;
 ibuf^.size:=size;
 ibuf^.bpos:=0;
 ibuf^.picb:=icb;
end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;buf:PPM4CMDINDIRECTBUFFER;icb:t_pm4_parse_cb):Boolean;
var
 ib_base:QWORD;
 ib_size:QWORD;
 addr:Pointer;
 size:QWORD;
begin
 Result:=False;

 ib_base:=QWORD(buf^.ibBase);
 ib_size:=QWORD(buf^.ibSize)*sizeof(DWORD);

 addr:=nil;
 size:=0;
 if get_dmem_ptr(Pointer(ib_base),@addr,@size) then
 begin
  if (ib_size>size) then
  begin
   Assert(false,'addr:0x'+HexStr(ib_base+ib_size,16)+' not in dmem!');
  end else
  begin
   Writeln(' addr:0x'+HexStr(ib_base,16)+' '+HexStr(ib_size,16));

   ibuf^.next:=Default(TAILQ_ENTRY);
   ibuf^.buff:=addr;
   ibuf^.size:=ib_size;
   ibuf^.bpos:=0;
   ibuf^.picb:=icb;

   Result:=True;
  end;
 end else
 begin
  Assert(false,'addr:0x'+HexStr(ib_base,16)+' not in dmem!');
 end;

end;

function pm4_ibuf_parse(pctx:p_pfp_ctx;ibuf:p_pm4_ibuffer):Integer;
var
 buff:Pointer;
 i,token,len:DWORD;
begin
 Result:=0;

 i:=ibuf^.bpos;
 buff:=ibuf^.buff+i;
 i:=ibuf^.size-i;

 while (i<>0) do
 begin
  token:=PDWORD(buff)^;

  if (PM4_TYPE(token)=2) then
  begin
   len:=sizeof(DWORD);
  end else
  begin
   len:=PM4_LENGTH(token);
  end;

  if (len>i) then
  begin
   i:=0;
   Break;
  end;

  Result:=ibuf^.picb(pctx,token,buff);
  if (Result<>0) then
  begin
   Break;
  end;

  Inc(buff,len);
  Dec(i,len);
 end;

 ibuf^.bpos:=ibuf^.size-i;
end;

procedure t_pfp_ctx.add_stall(ibuf:p_pm4_ibuffer);
var
 node:p_pm4_ibuffer;
begin
 node:=TAILQ_FIRST(@lastn);
 if (node<>nil) then
 begin
  TAILQ_REMOVE(@lastn,node,@node^.next);
 end else
 begin
  node:=AllocMem(SizeOf(t_pm4_ibuffer));
 end;

 node^:=ibuf^;

 if (stall.tqh_first=nil) and (stall.tqh_last=nil) then
 begin
  TAILQ_INIT(@stall);
 end;

 TAILQ_INSERT_TAIL(@stall,node,@node^.next);
end;

procedure t_pfp_ctx.free;
var
 node:p_pm4_ibuffer;
begin
 node:=TAILQ_FIRST(@lastn);
 while (node<>nil) do
 begin
  TAILQ_REMOVE(@lastn,node,@node^.next);
  FreeMem(node);
  node:=TAILQ_FIRST(@lastn);
 end;
 //
 node:=TAILQ_FIRST(@stall);
 while (node<>nil) do
 begin
  TAILQ_REMOVE(@stall,node,@node^.next);
  FreeMem(node);
  node:=TAILQ_FIRST(@stall);
 end;
end;

end.

