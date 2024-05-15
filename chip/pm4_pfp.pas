unit pm4_pfp;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 bittype,
 pm4_ring,
 pm4defs,
 pm4_stream,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

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
  //
  stream_dcb:t_pm4_stream;
  stream_ccb:t_pm4_stream;
  //
  SH_REG:TSH_REG_GROUP;         // 0x2C00
  CX_REG:TCONTEXT_REG_GROUP;    // 0xA000
  UC_REG:TUSERCONFIG_REG_SHORT; // 0xC000
  //
  print_ops :Boolean;
  print_hint:Boolean;
  LastSetReg:Word;
  //
  procedure add_stall(ibuf:p_pm4_ibuffer);
  procedure free;
  //
  procedure set_esgs_gsvs_ring_size(esgsRingSize,gsvsRingSize:DWORD);
  //
  procedure set_reg(i:word;data:DWORD);
  procedure set_sh_reg(i:word;data:DWORD);
  procedure set_ctx_reg(i:word;data:DWORD);
  function  get_reg(i:word):DWORD;
  procedure clear_state;
 end;

function pm4_ibuf_init(ibuf:p_pm4_ibuffer;buff:Pointer;size:Ptruint;icb:t_pm4_parse_cb):Boolean;
function pm4_ibuf_init(ibuf:p_pm4_ibuffer;buf:PPM4CMDINDIRECTBUFFER;icb:t_pm4_parse_cb):Boolean;

function pm4_ibuf_parse(pctx:p_pfp_ctx;ibuf:p_pm4_ibuffer):Integer;

function pm4_parse_ccb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
function pm4_parse_dcb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;

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
 op:DWORD;
 ib_base:QWORD;
 ib_size:QWORD;
 addr:Pointer;
 size:QWORD;
begin
 Result:=False;

 op:=buf^.header;

 if ((op<>$c0023300) and (op<>$c0023f00)) then
 begin
  Assert(false,'init not indirect buffer');
 end;

 ib_base:=QWORD(buf^.ibBase);
 ib_size:=QWORD(buf^.ibSize)*sizeof(DWORD);

 addr:=nil;
 size:=0;

 if get_dmem_ptr(Pointer(ib_base),@addr,@size) then
 begin
  if (ib_size>size) then
  begin
   Assert(false,'addr:0x'+HexStr(ib_base+size,16)+' not in dmem!');
  end else
  begin
   //Writeln(' addr:0x'+HexStr(ib_base,16)+' '+HexStr(ib_size,16));

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

 pctx^.LastSetReg:=0;

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

procedure t_pfp_ctx.set_esgs_gsvs_ring_size(esgsRingSize,gsvsRingSize:DWORD);
begin
 UC_REG.VGT_ESGS_RING_SIZE:=esgsRingSize;
 UC_REG.VGT_GSVS_RING_SIZE:=gsvsRingSize;
end;

procedure t_pfp_ctx.set_reg(i:word;data:DWORD);
begin
 case i of
  $2C00..$2E7F:PDWORD(@SH_REG)[i-$2C00]:=data;
  $A000..$A38F:PDWORD(@CX_REG)[i-$A000]:=data;
  $C079:PDWORD(@UC_REG.CP_COHER_BASE_HI  )^:=data;
  $C07C:PDWORD(@UC_REG.CP_COHER_CNTL     )^:=data;
  $C07D:PDWORD(@UC_REG.CP_COHER_SIZE     )^:=data;
  $C07E:PDWORD(@UC_REG.CP_COHER_BASE     )^:=data;
  $C08C:PDWORD(@UC_REG.CP_COHER_SIZE_HI  )^:=data;
  $C200:PDWORD(@UC_REG.GRBM_GFX_INDEX    )^:=data;
  $C240:PDWORD(@UC_REG.VGT_ESGS_RING_SIZE)^:=data;
  $C241:PDWORD(@UC_REG.VGT_GSVS_RING_SIZE)^:=data;
  $C242:PDWORD(@UC_REG.VGT_PRIMITIVE_TYPE)^:=data;
  $C243:PDWORD(@UC_REG.VGT_INDEX_TYPE    )^:=data;
  $C24C:PDWORD(@UC_REG.VGT_NUM_INDICES   )^:=data;
  $C24D:PDWORD(@UC_REG.VGT_NUM_INSTANCES )^:=data;
  $C258:PDWORD(@CX_REG.IA_MULTI_VGT_PARAM)^:=data;
  else
   begin
    Writeln(stderr,getRegName(i),':=0x',HexStr(data,8));
   end;
 end;
end;

procedure t_pfp_ctx.set_sh_reg(i:word;data:DWORD);
begin
 if (i<=$27F) then
 begin
  PDWORD(@SH_REG)[i]:=data;
 end else
 begin
  Writeln(stderr,getRegName(i+$2C00),':=0x',HexStr(data,8));
 end;
end;

procedure t_pfp_ctx.set_ctx_reg(i:word;data:DWORD);
begin
 if (i<=$38F) then
 begin
  PDWORD(@CX_REG)[i]:=data;
 end else
 begin
  Writeln(stderr,getRegName(i+$A000),':=0x',HexStr(data,8));
 end;
end;

function t_pfp_ctx.get_reg(i:word):DWORD;
begin
 case i of
  $2C00..$2E7F:Result:=PDWORD(@SH_REG)[i-$2C00];
  $A000..$A38F:Result:=PDWORD(@CX_REG)[i-$A000];
  $C079:Result:=PDWORD(@UC_REG.CP_COHER_BASE_HI  )^;
  $C07C:Result:=PDWORD(@UC_REG.CP_COHER_CNTL     )^;
  $C07D:Result:=PDWORD(@UC_REG.CP_COHER_SIZE     )^;
  $C07E:Result:=PDWORD(@UC_REG.CP_COHER_BASE     )^;
  $C08C:Result:=PDWORD(@UC_REG.CP_COHER_SIZE_HI  )^;
  $C200:Result:=PDWORD(@UC_REG.GRBM_GFX_INDEX    )^;
  $C240:Result:=PDWORD(@UC_REG.VGT_ESGS_RING_SIZE)^;
  $C241:Result:=PDWORD(@UC_REG.VGT_GSVS_RING_SIZE)^;
  $C242:Result:=PDWORD(@UC_REG.VGT_PRIMITIVE_TYPE)^;
  $C243:Result:=PDWORD(@UC_REG.VGT_INDEX_TYPE    )^;
  $C24C:Result:=PDWORD(@UC_REG.VGT_NUM_INDICES   )^;
  $C24D:Result:=PDWORD(@UC_REG.VGT_NUM_INSTANCES )^;
  $C258:Result:=PDWORD(@CX_REG.IA_MULTI_VGT_PARAM)^;
  else
        Result:=0;
 end;
end;

procedure t_pfp_ctx.clear_state;
begin
 PDWORD(@CX_REG)[$000]:=$00000000;
 PDWORD(@CX_REG)[$001]:=$00000000;
 PDWORD(@CX_REG)[$002]:=$00000000;
 PDWORD(@CX_REG)[$003]:=$00000000;
 PDWORD(@CX_REG)[$004]:=$00000000;
 PDWORD(@CX_REG)[$005]:=$00000000;
 PDWORD(@CX_REG)[$008]:=$00000000;
 PDWORD(@CX_REG)[$009]:=$00000000;
 PDWORD(@CX_REG)[$00a]:=$00000000;
 PDWORD(@CX_REG)[$00b]:=$00000000;
 PDWORD(@CX_REG)[$00c]:=$00000000;
 PDWORD(@CX_REG)[$00d]:=$40004000;
 PDWORD(@CX_REG)[$00f]:=$00000000;
 PDWORD(@CX_REG)[$010]:=$00000000;
 PDWORD(@CX_REG)[$011]:=$00000000;
 PDWORD(@CX_REG)[$012]:=$00000000;
 PDWORD(@CX_REG)[$013]:=$00000000;
 PDWORD(@CX_REG)[$014]:=$00000000;
 PDWORD(@CX_REG)[$015]:=$00000000;
 PDWORD(@CX_REG)[$016]:=$00000000;
 PDWORD(@CX_REG)[$017]:=$00000000;
 PDWORD(@CX_REG)[$020]:=$00000000;
 PDWORD(@CX_REG)[$021]:=$00000000;
 PDWORD(@CX_REG)[$07a]:=$00000000;
 PDWORD(@CX_REG)[$07b]:=$00000000;
 PDWORD(@CX_REG)[$07c]:=$00000000;
 PDWORD(@CX_REG)[$07d]:=$00000000;
 PDWORD(@CX_REG)[$07e]:=$00000000;
 PDWORD(@CX_REG)[$07f]:=$00000000;
 PDWORD(@CX_REG)[$080]:=$00000000;
 PDWORD(@CX_REG)[$081]:=$80000000;
 PDWORD(@CX_REG)[$082]:=$40004000;
 PDWORD(@CX_REG)[$084]:=$00000000;
 PDWORD(@CX_REG)[$085]:=$40004000;
 PDWORD(@CX_REG)[$086]:=$00000000;
 PDWORD(@CX_REG)[$087]:=$40004000;
 PDWORD(@CX_REG)[$088]:=$00000000;
 PDWORD(@CX_REG)[$089]:=$40004000;
 PDWORD(@CX_REG)[$08a]:=$00000000;
 PDWORD(@CX_REG)[$08b]:=$40004000;
 PDWORD(@CX_REG)[$08c]:=$aa99aaaa;
 PDWORD(@CX_REG)[$08d]:=$00000000;
 PDWORD(@CX_REG)[$090]:=$80000000;
 PDWORD(@CX_REG)[$091]:=$40004000;
 PDWORD(@CX_REG)[$092]:=$00000000;
 PDWORD(@CX_REG)[$093]:=$00000000;
 PDWORD(@CX_REG)[$094]:=$80000000;
 PDWORD(@CX_REG)[$095]:=$40004000;
 PDWORD(@CX_REG)[$096]:=$80000000;
 PDWORD(@CX_REG)[$097]:=$40004000;
 PDWORD(@CX_REG)[$098]:=$80000000;
 PDWORD(@CX_REG)[$099]:=$40004000;
 PDWORD(@CX_REG)[$09a]:=$80000000;
 PDWORD(@CX_REG)[$09b]:=$40004000;
 PDWORD(@CX_REG)[$09c]:=$80000000;
 PDWORD(@CX_REG)[$09d]:=$40004000;
 PDWORD(@CX_REG)[$09e]:=$80000000;
 PDWORD(@CX_REG)[$09f]:=$40004000;
 PDWORD(@CX_REG)[$0a0]:=$80000000;
 PDWORD(@CX_REG)[$0a1]:=$40004000;
 PDWORD(@CX_REG)[$0a2]:=$80000000;
 PDWORD(@CX_REG)[$0a3]:=$40004000;
 PDWORD(@CX_REG)[$0a4]:=$80000000;
 PDWORD(@CX_REG)[$0a5]:=$40004000;
 PDWORD(@CX_REG)[$0a6]:=$80000000;
 PDWORD(@CX_REG)[$0a7]:=$40004000;
 PDWORD(@CX_REG)[$0a8]:=$80000000;
 PDWORD(@CX_REG)[$0a9]:=$40004000;
 PDWORD(@CX_REG)[$0aa]:=$80000000;
 PDWORD(@CX_REG)[$0ab]:=$40004000;
 PDWORD(@CX_REG)[$0ac]:=$80000000;
 PDWORD(@CX_REG)[$0ad]:=$40004000;
 PDWORD(@CX_REG)[$0ae]:=$80000000;
 PDWORD(@CX_REG)[$0af]:=$40004000;
 PDWORD(@CX_REG)[$0b0]:=$80000000;
 PDWORD(@CX_REG)[$0b1]:=$40004000;
 PDWORD(@CX_REG)[$0b2]:=$80000000;
 PDWORD(@CX_REG)[$0b3]:=$40004000;
 PDWORD(@CX_REG)[$0b4]:=$00000000;
 PDWORD(@CX_REG)[$0b5]:=$3f800000;
 PDWORD(@CX_REG)[$0b6]:=$00000000;
 PDWORD(@CX_REG)[$0b7]:=$3f800000;
 PDWORD(@CX_REG)[$0b8]:=$00000000;
 PDWORD(@CX_REG)[$0b9]:=$3f800000;
 PDWORD(@CX_REG)[$0ba]:=$00000000;
 PDWORD(@CX_REG)[$0bb]:=$3f800000;
 PDWORD(@CX_REG)[$0bc]:=$00000000;
 PDWORD(@CX_REG)[$0bd]:=$3f800000;
 PDWORD(@CX_REG)[$0be]:=$00000000;
 PDWORD(@CX_REG)[$0bf]:=$3f800000;
 PDWORD(@CX_REG)[$0c0]:=$00000000;
 PDWORD(@CX_REG)[$0c1]:=$3f800000;
 PDWORD(@CX_REG)[$0c2]:=$00000000;
 PDWORD(@CX_REG)[$0c3]:=$3f800000;
 PDWORD(@CX_REG)[$0c4]:=$00000000;
 PDWORD(@CX_REG)[$0c5]:=$3f800000;
 PDWORD(@CX_REG)[$0c6]:=$00000000;
 PDWORD(@CX_REG)[$0c7]:=$3f800000;
 PDWORD(@CX_REG)[$0c8]:=$00000000;
 PDWORD(@CX_REG)[$0c9]:=$3f800000;
 PDWORD(@CX_REG)[$0ca]:=$00000000;
 PDWORD(@CX_REG)[$0cb]:=$3f800000;
 PDWORD(@CX_REG)[$0cc]:=$00000000;
 PDWORD(@CX_REG)[$0cd]:=$3f800000;
 PDWORD(@CX_REG)[$0ce]:=$00000000;
 PDWORD(@CX_REG)[$0cf]:=$3f800000;
 PDWORD(@CX_REG)[$0d0]:=$00000000;
 PDWORD(@CX_REG)[$0d1]:=$3f800000;
 PDWORD(@CX_REG)[$0d2]:=$00000000;
 PDWORD(@CX_REG)[$0d3]:=$3f800000;
 PDWORD(@CX_REG)[$0d4]:=$2a00161a;
 PDWORD(@CX_REG)[$0d5]:=$00000000;
 PDWORD(@CX_REG)[$0d6]:=$00000000;
 PDWORD(@CX_REG)[$0d8]:=$00000000;
 PDWORD(@CX_REG)[$103]:=$00000000;
 PDWORD(@CX_REG)[$105]:=$00000000;
 PDWORD(@CX_REG)[$106]:=$00000000;
 PDWORD(@CX_REG)[$107]:=$00000000;
 PDWORD(@CX_REG)[$108]:=$00000000;
 PDWORD(@CX_REG)[$10b]:=$00000000;
 PDWORD(@CX_REG)[$10c]:=$00000000;
 PDWORD(@CX_REG)[$10d]:=$00000000;
 PDWORD(@CX_REG)[$10f]:=$00000000;
 PDWORD(@CX_REG)[$110]:=$00000000;
 PDWORD(@CX_REG)[$111]:=$00000000;
 PDWORD(@CX_REG)[$112]:=$00000000;
 PDWORD(@CX_REG)[$113]:=$00000000;
 PDWORD(@CX_REG)[$114]:=$00000000;
 PDWORD(@CX_REG)[$115]:=$00000000;
 PDWORD(@CX_REG)[$116]:=$00000000;
 PDWORD(@CX_REG)[$117]:=$00000000;
 PDWORD(@CX_REG)[$118]:=$00000000;
 PDWORD(@CX_REG)[$119]:=$00000000;
 PDWORD(@CX_REG)[$11a]:=$00000000;
 PDWORD(@CX_REG)[$11b]:=$00000000;
 PDWORD(@CX_REG)[$11c]:=$00000000;
 PDWORD(@CX_REG)[$11d]:=$00000000;
 PDWORD(@CX_REG)[$11e]:=$00000000;
 PDWORD(@CX_REG)[$11f]:=$00000000;
 PDWORD(@CX_REG)[$120]:=$00000000;
 PDWORD(@CX_REG)[$121]:=$00000000;
 PDWORD(@CX_REG)[$122]:=$00000000;
 PDWORD(@CX_REG)[$123]:=$00000000;
 PDWORD(@CX_REG)[$124]:=$00000000;
 PDWORD(@CX_REG)[$125]:=$00000000;
 PDWORD(@CX_REG)[$126]:=$00000000;
 PDWORD(@CX_REG)[$127]:=$00000000;
 PDWORD(@CX_REG)[$128]:=$00000000;
 PDWORD(@CX_REG)[$129]:=$00000000;
 PDWORD(@CX_REG)[$12a]:=$00000000;
 PDWORD(@CX_REG)[$12b]:=$00000000;
 PDWORD(@CX_REG)[$12c]:=$00000000;
 PDWORD(@CX_REG)[$12d]:=$00000000;
 PDWORD(@CX_REG)[$12e]:=$00000000;
 PDWORD(@CX_REG)[$12f]:=$00000000;
 PDWORD(@CX_REG)[$130]:=$00000000;
 PDWORD(@CX_REG)[$131]:=$00000000;
 PDWORD(@CX_REG)[$132]:=$00000000;
 PDWORD(@CX_REG)[$133]:=$00000000;
 PDWORD(@CX_REG)[$134]:=$00000000;
 PDWORD(@CX_REG)[$135]:=$00000000;
 PDWORD(@CX_REG)[$136]:=$00000000;
 PDWORD(@CX_REG)[$137]:=$00000000;
 PDWORD(@CX_REG)[$138]:=$00000000;
 PDWORD(@CX_REG)[$139]:=$00000000;
 PDWORD(@CX_REG)[$13a]:=$00000000;
 PDWORD(@CX_REG)[$13b]:=$00000000;
 PDWORD(@CX_REG)[$13c]:=$00000000;
 PDWORD(@CX_REG)[$13d]:=$00000000;
 PDWORD(@CX_REG)[$13e]:=$00000000;
 PDWORD(@CX_REG)[$13f]:=$00000000;
 PDWORD(@CX_REG)[$140]:=$00000000;
 PDWORD(@CX_REG)[$141]:=$00000000;
 PDWORD(@CX_REG)[$142]:=$00000000;
 PDWORD(@CX_REG)[$143]:=$00000000;
 PDWORD(@CX_REG)[$144]:=$00000000;
 PDWORD(@CX_REG)[$145]:=$00000000;
 PDWORD(@CX_REG)[$146]:=$00000000;
 PDWORD(@CX_REG)[$147]:=$00000000;
 PDWORD(@CX_REG)[$148]:=$00000000;
 PDWORD(@CX_REG)[$149]:=$00000000;
 PDWORD(@CX_REG)[$14a]:=$00000000;
 PDWORD(@CX_REG)[$14b]:=$00000000;
 PDWORD(@CX_REG)[$14c]:=$00000000;
 PDWORD(@CX_REG)[$14d]:=$00000000;
 PDWORD(@CX_REG)[$14e]:=$00000000;
 PDWORD(@CX_REG)[$14f]:=$00000000;
 PDWORD(@CX_REG)[$150]:=$00000000;
 PDWORD(@CX_REG)[$151]:=$00000000;
 PDWORD(@CX_REG)[$152]:=$00000000;
 PDWORD(@CX_REG)[$153]:=$00000000;
 PDWORD(@CX_REG)[$154]:=$00000000;
 PDWORD(@CX_REG)[$155]:=$00000000;
 PDWORD(@CX_REG)[$156]:=$00000000;
 PDWORD(@CX_REG)[$157]:=$00000000;
 PDWORD(@CX_REG)[$158]:=$00000000;
 PDWORD(@CX_REG)[$159]:=$00000000;
 PDWORD(@CX_REG)[$15a]:=$00000000;
 PDWORD(@CX_REG)[$15b]:=$00000000;
 PDWORD(@CX_REG)[$15c]:=$00000000;
 PDWORD(@CX_REG)[$15d]:=$00000000;
 PDWORD(@CX_REG)[$15e]:=$00000000;
 PDWORD(@CX_REG)[$15f]:=$00000000;
 PDWORD(@CX_REG)[$160]:=$00000000;
 PDWORD(@CX_REG)[$161]:=$00000000;
 PDWORD(@CX_REG)[$162]:=$00000000;
 PDWORD(@CX_REG)[$163]:=$00000000;
 PDWORD(@CX_REG)[$164]:=$00000000;
 PDWORD(@CX_REG)[$165]:=$00000000;
 PDWORD(@CX_REG)[$166]:=$00000000;
 PDWORD(@CX_REG)[$167]:=$00000000;
 PDWORD(@CX_REG)[$168]:=$00000000;
 PDWORD(@CX_REG)[$169]:=$00000000;
 PDWORD(@CX_REG)[$16a]:=$00000000;
 PDWORD(@CX_REG)[$16b]:=$00000000;
 PDWORD(@CX_REG)[$16c]:=$00000000;
 PDWORD(@CX_REG)[$16d]:=$00000000;
 PDWORD(@CX_REG)[$16e]:=$00000000;
 PDWORD(@CX_REG)[$16f]:=$00000000;
 PDWORD(@CX_REG)[$170]:=$00000000;
 PDWORD(@CX_REG)[$171]:=$00000000;
 PDWORD(@CX_REG)[$172]:=$00000000;
 PDWORD(@CX_REG)[$173]:=$00000000;
 PDWORD(@CX_REG)[$174]:=$00000000;
 PDWORD(@CX_REG)[$175]:=$00000000;
 PDWORD(@CX_REG)[$176]:=$00000000;
 PDWORD(@CX_REG)[$177]:=$00000000;
 PDWORD(@CX_REG)[$178]:=$00000000;
 PDWORD(@CX_REG)[$179]:=$00000000;
 PDWORD(@CX_REG)[$17a]:=$00000000;
 PDWORD(@CX_REG)[$17b]:=$00000000;
 PDWORD(@CX_REG)[$17c]:=$00000000;
 PDWORD(@CX_REG)[$17d]:=$00000000;
 PDWORD(@CX_REG)[$17e]:=$00000000;
 PDWORD(@CX_REG)[$17f]:=$00000000;
 PDWORD(@CX_REG)[$180]:=$00000000;
 PDWORD(@CX_REG)[$181]:=$00000000;
 PDWORD(@CX_REG)[$182]:=$00000000;
 PDWORD(@CX_REG)[$183]:=$00000000;
 PDWORD(@CX_REG)[$184]:=$00000000;
 PDWORD(@CX_REG)[$185]:=$00000000;
 PDWORD(@CX_REG)[$186]:=$00000000;
 PDWORD(@CX_REG)[$191]:=$00000000;
 PDWORD(@CX_REG)[$192]:=$00000000;
 PDWORD(@CX_REG)[$193]:=$00000000;
 PDWORD(@CX_REG)[$194]:=$00000000;
 PDWORD(@CX_REG)[$195]:=$00000000;
 PDWORD(@CX_REG)[$196]:=$00000000;
 PDWORD(@CX_REG)[$197]:=$00000000;
 PDWORD(@CX_REG)[$198]:=$00000000;
 PDWORD(@CX_REG)[$199]:=$00000000;
 PDWORD(@CX_REG)[$19a]:=$00000000;
 PDWORD(@CX_REG)[$19b]:=$00000000;
 PDWORD(@CX_REG)[$19c]:=$00000000;
 PDWORD(@CX_REG)[$19d]:=$00000000;
 PDWORD(@CX_REG)[$19e]:=$00000000;
 PDWORD(@CX_REG)[$19f]:=$00000000;
 PDWORD(@CX_REG)[$1a0]:=$00000000;
 PDWORD(@CX_REG)[$1a1]:=$00000000;
 PDWORD(@CX_REG)[$1a2]:=$00000000;
 PDWORD(@CX_REG)[$1a3]:=$00000000;
 PDWORD(@CX_REG)[$1a4]:=$00000000;
 PDWORD(@CX_REG)[$1a5]:=$00000000;
 PDWORD(@CX_REG)[$1a6]:=$00000000;
 PDWORD(@CX_REG)[$1a7]:=$00000000;
 PDWORD(@CX_REG)[$1a8]:=$00000000;
 PDWORD(@CX_REG)[$1a9]:=$00000000;
 PDWORD(@CX_REG)[$1aa]:=$00000000;
 PDWORD(@CX_REG)[$1ab]:=$00000000;
 PDWORD(@CX_REG)[$1ac]:=$00000000;
 PDWORD(@CX_REG)[$1ad]:=$00000000;
 PDWORD(@CX_REG)[$1ae]:=$00000000;
 PDWORD(@CX_REG)[$1af]:=$00000000;
 PDWORD(@CX_REG)[$1b0]:=$00000000;
 PDWORD(@CX_REG)[$1b1]:=$00000000;
 PDWORD(@CX_REG)[$1b3]:=$00000000;
 PDWORD(@CX_REG)[$1b4]:=$00000000;
 PDWORD(@CX_REG)[$1b5]:=$00000000;
 PDWORD(@CX_REG)[$1b6]:=$00000002;
 PDWORD(@CX_REG)[$1b8]:=$00000000;
 PDWORD(@CX_REG)[$1ba]:=$00000000;
 PDWORD(@CX_REG)[$1c3]:=$00000000;
 PDWORD(@CX_REG)[$1c4]:=$00000000;
 PDWORD(@CX_REG)[$1c5]:=$00000000;
 PDWORD(@CX_REG)[$1e0]:=$00000000;
 PDWORD(@CX_REG)[$1e1]:=$00000000;
 PDWORD(@CX_REG)[$1e2]:=$00000000;
 PDWORD(@CX_REG)[$1e3]:=$00000000;
 PDWORD(@CX_REG)[$1e4]:=$00000000;
 PDWORD(@CX_REG)[$1e5]:=$00000000;
 PDWORD(@CX_REG)[$1e6]:=$00000000;
 PDWORD(@CX_REG)[$1e7]:=$00000000;
 PDWORD(@CX_REG)[$1f5]:=$00000000;
 PDWORD(@CX_REG)[$1f6]:=$00000000;
 PDWORD(@CX_REG)[$1f7]:=$00000000;
 PDWORD(@CX_REG)[$1f8]:=$00000000;
 PDWORD(@CX_REG)[$200]:=$00000000;
 PDWORD(@CX_REG)[$201]:=$00000000;
 PDWORD(@CX_REG)[$202]:=$00000000;
 PDWORD(@CX_REG)[$203]:=$00000000;
 PDWORD(@CX_REG)[$204]:=$00090000;
 PDWORD(@CX_REG)[$205]:=$00000004;
 PDWORD(@CX_REG)[$206]:=$00000000;
 PDWORD(@CX_REG)[$207]:=$00000000;
 PDWORD(@CX_REG)[$208]:=$00000000;
 PDWORD(@CX_REG)[$209]:=$00000000;
 PDWORD(@CX_REG)[$20a]:=$00000000;
 PDWORD(@CX_REG)[$20b]:=$00000000;
 PDWORD(@CX_REG)[$280]:=$00000000;
 PDWORD(@CX_REG)[$281]:=$00000000;
 PDWORD(@CX_REG)[$282]:=$00000000;
 PDWORD(@CX_REG)[$283]:=$00000000;
 PDWORD(@CX_REG)[$284]:=$00000000;
 PDWORD(@CX_REG)[$285]:=$00000000;
 PDWORD(@CX_REG)[$286]:=$00000000;
 PDWORD(@CX_REG)[$287]:=$00000000;
 PDWORD(@CX_REG)[$288]:=$00000000;
 PDWORD(@CX_REG)[$289]:=$00000000;
 PDWORD(@CX_REG)[$28a]:=$00000000;
 PDWORD(@CX_REG)[$28b]:=$00000000;
 PDWORD(@CX_REG)[$28c]:=$00000000;
 PDWORD(@CX_REG)[$28d]:=$00000000;
 PDWORD(@CX_REG)[$28e]:=$00000000;
 PDWORD(@CX_REG)[$28f]:=$00000000;
 PDWORD(@CX_REG)[$290]:=$00000000;
 PDWORD(@CX_REG)[$291]:=$00000000;
 PDWORD(@CX_REG)[$292]:=$00000000;
 PDWORD(@CX_REG)[$293]:=$00000000;
 PDWORD(@CX_REG)[$294]:=$00000000;
 PDWORD(@CX_REG)[$295]:=$00000100;
 PDWORD(@CX_REG)[$296]:=$00000080;
 PDWORD(@CX_REG)[$297]:=$00000002;
 PDWORD(@CX_REG)[$298]:=$00000000;
 PDWORD(@CX_REG)[$299]:=$00000000;
 PDWORD(@CX_REG)[$29a]:=$00000000;
 PDWORD(@CX_REG)[$29b]:=$00000000;
 PDWORD(@CX_REG)[$29c]:=$00000000;
 PDWORD(@CX_REG)[$2a0]:=$00000000;
 PDWORD(@CX_REG)[$2a1]:=$00000000;
 PDWORD(@CX_REG)[$2a3]:=$00000000;
 PDWORD(@CX_REG)[$2a5]:=$00000000;
 PDWORD(@CX_REG)[$2a8]:=$00000000;
 PDWORD(@CX_REG)[$2a9]:=$00000000;
 PDWORD(@CX_REG)[$2aa]:=$000000ff;
 PDWORD(@CX_REG)[$2ab]:=$00000000;
 PDWORD(@CX_REG)[$2ac]:=$00000000;
 PDWORD(@CX_REG)[$2ad]:=$00000000;
 PDWORD(@CX_REG)[$2ae]:=$00000000;
 PDWORD(@CX_REG)[$2af]:=$00000000;
 PDWORD(@CX_REG)[$2b0]:=$00000000;
 PDWORD(@CX_REG)[$2b1]:=$00000000;
 PDWORD(@CX_REG)[$2b2]:=$00000000;
 PDWORD(@CX_REG)[$2b4]:=$00000000;
 PDWORD(@CX_REG)[$2b5]:=$00000000;
 PDWORD(@CX_REG)[$2b7]:=$00000000;
 PDWORD(@CX_REG)[$2b8]:=$00000000;
 PDWORD(@CX_REG)[$2b9]:=$00000000;
 PDWORD(@CX_REG)[$2bb]:=$00000000;
 PDWORD(@CX_REG)[$2bc]:=$00000000;
 PDWORD(@CX_REG)[$2bd]:=$00000000;
 PDWORD(@CX_REG)[$2bf]:=$00000000;
 PDWORD(@CX_REG)[$2c0]:=$00000000;
 PDWORD(@CX_REG)[$2c1]:=$00000000;
 PDWORD(@CX_REG)[$2c3]:=$00000000;
 PDWORD(@CX_REG)[$2ca]:=$00000000;
 PDWORD(@CX_REG)[$2cb]:=$00000000;
 PDWORD(@CX_REG)[$2cc]:=$00000000;
 PDWORD(@CX_REG)[$2ce]:=$00000000;
 PDWORD(@CX_REG)[$2d5]:=$00000000;
 PDWORD(@CX_REG)[$2d6]:=$00000000;
 PDWORD(@CX_REG)[$2d7]:=$00000000;
 PDWORD(@CX_REG)[$2d8]:=$00000000;
 PDWORD(@CX_REG)[$2d9]:=$00000000;
 PDWORD(@CX_REG)[$2da]:=$00000000;
 PDWORD(@CX_REG)[$2db]:=$00000000;
 PDWORD(@CX_REG)[$2dc]:=$00000000;
 PDWORD(@CX_REG)[$2dd]:=$00000000;
 PDWORD(@CX_REG)[$2de]:=$00000000;
 PDWORD(@CX_REG)[$2df]:=$00000000;
 PDWORD(@CX_REG)[$2e0]:=$00000000;
 PDWORD(@CX_REG)[$2e1]:=$00000000;
 PDWORD(@CX_REG)[$2e2]:=$00000000;
 PDWORD(@CX_REG)[$2e3]:=$00000000;
 PDWORD(@CX_REG)[$2e4]:=$00000000;
 PDWORD(@CX_REG)[$2e5]:=$00000000;
 PDWORD(@CX_REG)[$2e6]:=$00000000;
 PDWORD(@CX_REG)[$2f5]:=$00000000;
 PDWORD(@CX_REG)[$2f6]:=$00000000;
 PDWORD(@CX_REG)[$2f7]:=$00001000;
 PDWORD(@CX_REG)[$2f8]:=$00000000;
 PDWORD(@CX_REG)[$2f9]:=$00000005;
 PDWORD(@CX_REG)[$2fa]:=$3f800000;
 PDWORD(@CX_REG)[$2fb]:=$3f800000;
 PDWORD(@CX_REG)[$2fc]:=$3f800000;
 PDWORD(@CX_REG)[$2fd]:=$3f800000;
 PDWORD(@CX_REG)[$2fe]:=$00000000;
 PDWORD(@CX_REG)[$2ff]:=$00000000;
 PDWORD(@CX_REG)[$300]:=$00000000;
 PDWORD(@CX_REG)[$301]:=$00000000;
 PDWORD(@CX_REG)[$302]:=$00000000;
 PDWORD(@CX_REG)[$303]:=$00000000;
 PDWORD(@CX_REG)[$304]:=$00000000;
 PDWORD(@CX_REG)[$305]:=$00000000;
 PDWORD(@CX_REG)[$306]:=$00000000;
 PDWORD(@CX_REG)[$307]:=$00000000;
 PDWORD(@CX_REG)[$308]:=$00000000;
 PDWORD(@CX_REG)[$309]:=$00000000;
 PDWORD(@CX_REG)[$30a]:=$00000000;
 PDWORD(@CX_REG)[$30b]:=$00000000;
 PDWORD(@CX_REG)[$30c]:=$00000000;
 PDWORD(@CX_REG)[$30d]:=$00000000;
 PDWORD(@CX_REG)[$316]:=$0000000e;
 PDWORD(@CX_REG)[$317]:=$00000010;
 PDWORD(@CX_REG)[$318]:=$00000000;
 PDWORD(@CX_REG)[$319]:=$00000000;
 PDWORD(@CX_REG)[$31a]:=$00000000;
 PDWORD(@CX_REG)[$31b]:=$00000000;
 PDWORD(@CX_REG)[$31c]:=$00000000;
 PDWORD(@CX_REG)[$31d]:=$00000000;
 PDWORD(@CX_REG)[$31f]:=$00000000;
 PDWORD(@CX_REG)[$320]:=$00000000;
 PDWORD(@CX_REG)[$321]:=$00000000;
 PDWORD(@CX_REG)[$322]:=$00000000;
 PDWORD(@CX_REG)[$323]:=$00000000;
 PDWORD(@CX_REG)[$324]:=$00000000;
 PDWORD(@CX_REG)[$327]:=$00000000;
 PDWORD(@CX_REG)[$328]:=$00000000;
 PDWORD(@CX_REG)[$329]:=$00000000;
 PDWORD(@CX_REG)[$32a]:=$00000000;
 PDWORD(@CX_REG)[$32b]:=$00000000;
 PDWORD(@CX_REG)[$32c]:=$00000000;
 PDWORD(@CX_REG)[$32e]:=$00000000;
 PDWORD(@CX_REG)[$32f]:=$00000000;
 PDWORD(@CX_REG)[$330]:=$00000000;
 PDWORD(@CX_REG)[$331]:=$00000000;
 PDWORD(@CX_REG)[$332]:=$00000000;
 PDWORD(@CX_REG)[$333]:=$00000000;
 PDWORD(@CX_REG)[$336]:=$00000000;
 PDWORD(@CX_REG)[$337]:=$00000000;
 PDWORD(@CX_REG)[$338]:=$00000000;
 PDWORD(@CX_REG)[$339]:=$00000000;
 PDWORD(@CX_REG)[$33a]:=$00000000;
 PDWORD(@CX_REG)[$33b]:=$00000000;
 PDWORD(@CX_REG)[$33d]:=$00000000;
 PDWORD(@CX_REG)[$33e]:=$00000000;
 PDWORD(@CX_REG)[$33f]:=$00000000;
 PDWORD(@CX_REG)[$340]:=$00000000;
 PDWORD(@CX_REG)[$341]:=$00000000;
 PDWORD(@CX_REG)[$342]:=$00000000;
 PDWORD(@CX_REG)[$345]:=$00000000;
 PDWORD(@CX_REG)[$346]:=$00000000;
 PDWORD(@CX_REG)[$347]:=$00000000;
 PDWORD(@CX_REG)[$348]:=$00000000;
 PDWORD(@CX_REG)[$349]:=$00000000;
 PDWORD(@CX_REG)[$34a]:=$00000000;
 PDWORD(@CX_REG)[$34c]:=$00000000;
 PDWORD(@CX_REG)[$34d]:=$00000000;
 PDWORD(@CX_REG)[$34e]:=$00000000;
 PDWORD(@CX_REG)[$34f]:=$00000000;
 PDWORD(@CX_REG)[$350]:=$00000000;
 PDWORD(@CX_REG)[$351]:=$00000000;
 PDWORD(@CX_REG)[$354]:=$00000000;
 PDWORD(@CX_REG)[$355]:=$00000000;
 PDWORD(@CX_REG)[$356]:=$00000000;
 PDWORD(@CX_REG)[$357]:=$00000000;
 PDWORD(@CX_REG)[$358]:=$00000000;
 PDWORD(@CX_REG)[$359]:=$00000000;
 PDWORD(@CX_REG)[$35b]:=$00000000;
 PDWORD(@CX_REG)[$35c]:=$00000000;
 PDWORD(@CX_REG)[$35d]:=$00000000;
 PDWORD(@CX_REG)[$35e]:=$00000000;
 PDWORD(@CX_REG)[$35f]:=$00000000;
 PDWORD(@CX_REG)[$360]:=$00000000;
 PDWORD(@CX_REG)[$363]:=$00000000;
 PDWORD(@CX_REG)[$364]:=$00000000;
 PDWORD(@CX_REG)[$365]:=$00000000;
 PDWORD(@CX_REG)[$366]:=$00000000;
 PDWORD(@CX_REG)[$367]:=$00000000;
 PDWORD(@CX_REG)[$368]:=$00000000;
 PDWORD(@CX_REG)[$36a]:=$00000000;
 PDWORD(@CX_REG)[$36b]:=$00000000;
 PDWORD(@CX_REG)[$36c]:=$00000000;
 PDWORD(@CX_REG)[$36d]:=$00000000;
 PDWORD(@CX_REG)[$36e]:=$00000000;
 PDWORD(@CX_REG)[$36f]:=$00000000;
 PDWORD(@CX_REG)[$372]:=$00000000;
 PDWORD(@CX_REG)[$373]:=$00000000;
 PDWORD(@CX_REG)[$374]:=$00000000;
 PDWORD(@CX_REG)[$375]:=$00000000;
 PDWORD(@CX_REG)[$376]:=$00000000;
 PDWORD(@CX_REG)[$377]:=$00000000;
 PDWORD(@CX_REG)[$379]:=$00000000;
 PDWORD(@CX_REG)[$37a]:=$00000000;
 PDWORD(@CX_REG)[$37b]:=$00000000;
 PDWORD(@CX_REG)[$37c]:=$00000000;
 PDWORD(@CX_REG)[$37d]:=$00000000;
 PDWORD(@CX_REG)[$37e]:=$00000000;
 PDWORD(@CX_REG)[$381]:=$00000000;
 PDWORD(@CX_REG)[$382]:=$00000000;
 PDWORD(@CX_REG)[$383]:=$00000000;
 PDWORD(@CX_REG)[$384]:=$00000000;
 PDWORD(@CX_REG)[$385]:=$00000000;
 PDWORD(@CX_REG)[$386]:=$00000000;
 PDWORD(@CX_REG)[$388]:=$00000000;
 PDWORD(@CX_REG)[$389]:=$00000000;
 PDWORD(@CX_REG)[$38a]:=$00000000;
 PDWORD(@CX_REG)[$38b]:=$00000000;
 PDWORD(@CX_REG)[$38c]:=$00000000;
 PDWORD(@CX_REG)[$38d]:=$00000000;
end;

///

procedure onLoadConstRam(pctx:p_pfp_ctx;Body:PPM4CMDCONSTRAMLOAD);
var
 addr:Pointer;
 size:QWORD;
begin
 {
 Writeln(' adr=0x',HexStr(Body^.addr,16));
 Writeln(' len=0x',HexStr(Body^.numDwords*4,4));
 Writeln(' ofs=0x',HexStr(Body^.offset,4));
 }

 addr:=nil;
 size:=0;

 if get_dmem_ptr(Pointer(Body^.addr),@addr,@size) then
 begin
  pctx^.stream_ccb.LoadConstRam(addr,Body^.numDwords,Body^.offset);
 end else
 begin
  Assert(false,'addr:0x'+HexStr(Body^.addr,16)+' not in dmem!');
 end;
end;

const
 ShdrType:array[0..1] of Pchar=('(GX)','(CS)');

function pm4_parse_ccb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     if pctx^.print_ops then Writeln('PM4_TYPE_0');
    end;
  2:begin //PM4_TYPE_2
     if pctx^.print_ops then Writeln('PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     if pctx^.print_ops then
     if (PM4_TYPE_3_HEADER(token).opcode<>IT_NOP) or
        (not pctx^.print_hint) then
     begin
      Writeln('IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),
                ' ',ShdrType[PM4_TYPE_3_HEADER(token).shaderType],
              ' len:',PM4_LENGTH(token));
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_NOP:;

      IT_LOAD_CONST_RAM:onLoadConstRam(pctx,buff);

      else;
     end;

    end;
  else
   begin
    Writeln(stderr,'PM4_TYPE_',PM4_TYPE(token));
    Assert(False);
   end;
 end;

end;

procedure onEventWrite(pctx:p_pfp_ctx;Body:PTPM4CMDEVENTWRITE);
begin
 DWORD(pctx^.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 Case Body^.eventType of
  CACHE_FLUSH_AND_INV_EVENT  :Writeln(' eventType=FLUSH_AND_INV_EVENT');
  FLUSH_AND_INV_CB_PIXEL_DATA:Writeln(' eventType=FLUSH_AND_INV_CB_PIXEL_DATA');
  FLUSH_AND_INV_DB_DATA_TS   :Writeln(' eventType=FLUSH_AND_INV_DB_DATA_TS');
  FLUSH_AND_INV_DB_META      :Writeln(' eventType=FLUSH_AND_INV_DB_META');
  FLUSH_AND_INV_CB_DATA_TS   :Writeln(' eventType=FLUSH_AND_INV_CB_DATA_TS');
  FLUSH_AND_INV_CB_META      :Writeln(' eventType=FLUSH_AND_INV_CB_META');
  THREAD_TRACE_MARKER        :Writeln(' eventType=THREAD_TRACE_MARKER');
  PIPELINESTAT_STOP          :Writeln(' eventType=PIPELINESTAT_STOP');
  else
                              Writeln(' eventType=0x',HexStr(Body^.eventType,2));
 end;

 pctx^.stream_dcb.EventWrite(Body^.eventType);
end;

procedure onEventWriteEop(pctx:p_pfp_ctx;Body:PPM4CMDEVENTWRITEEOP);
var
 addr:Pointer;
begin
 Case Body^.eventType of
  kEopFlushCbDbCaches:;
  kEopFlushAndInvalidateCbDbCaches:;
  kEopCbDbReadsDone:;
  else
   Assert(False,'EventWriteEop: eventType=0x'+HexStr(Body^.eventType,1));
 end;

 if (Body^.eventIndex<>EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP) then
 begin
  Assert(False,'EventWriteEop: eventIndex=0x'+HexStr(Body^.eventIndex,1));
 end;

 DWORD(pctx^.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 Writeln(' eventType  =0x',HexStr(Body^.eventType,2));
 Writeln(' interrupt  =0x',HexStr(Body^.intSel shr 1,2));
 Writeln(' srcSelector=0x',HexStr(Body^.dataSel,2));
 Writeln(' dstGpuAddr =0x',HexStr(Body^.address,16));
 Writeln(' immValue   =0x',HexStr(Body^.DATA,16));

 if (Body^.destTcL2<>0) then Exit; //write to L2

 {
 Case Body^.dataSel of
  //
  EVENTWRITEEOP_DATA_SEL_DISCARD            :;
  EVENTWRITEEOP_DATA_SEL_SEND_DATA32        :PDWORD(Body^.address)^:=Body^.DATA;
  EVENTWRITEEOP_DATA_SEL_SEND_DATA64        :PQWORD(Body^.address)^:=Body^.DATA;
  EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK     :; //system 100Mhz global clock.
  EVENTWRITEEOP_DATA_SEL_SEND_CP_PERFCOUNTER:; //GPU 800Mhz clock.
  else;
 end;
 }

 addr:=nil;

 if (Body^.dataSel in [1..4]) then
 begin
  if get_dmem_ptr(Pointer(Body^.address),@addr,nil) then
  begin
   //
  end else
  begin
   Assert(false,'addr:0x'+HexStr(Body^.address,16)+' not in dmem!');
  end;
 end;

 pctx^.stream_dcb.EventWriteEop(addr,Body^.DATA,Body^.eventType,Body^.dataSel,(Body^.intSel shr 1));

end;

procedure onEventWriteEos(pctx:p_pfp_ctx;Body:PPM4CMDEVENTWRITEEOS);
var
 addr:Pointer;
begin
 Case Body^.eventType of
  CS_DONE:;
  PS_DONE:;
  else
   Assert(False,'EventWriteEos: eventType=0x'+HexStr(Body^.eventType,1));
 end;

 if (Body^.eventIndex<>EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP) then
 begin
  Assert(False,'EventWriteEos: eventIndex=0x'+HexStr(Body^.eventIndex,1));
 end;

 DWORD(pctx^.CX_REG.VGT_EVENT_INITIATOR):=Body^.eventType;

 if get_dmem_ptr(Pointer(Body^.address),@addr,nil) then
 begin
  //
 end else
 begin
  Assert(false,'addr:0x'+HexStr(Body^.address,16)+' not in dmem!');
 end;

 pctx^.stream_dcb.EventWriteEos(addr,Body^.data,Body^.eventType,Body^.command);

end;

procedure onDmaData(pctx:p_pfp_ctx;Body:PPM4DMADATA);
var
 adrSrc,adrDst:QWORD;
 byteCount:DWORD;
 srcSel,dstSel:Byte;
begin
 srcSel:=((PDWORD(Body)[1] shr $1d) and 3) or ((PDWORD(Body)[6] shr $19) and 8) or ((PDWORD(Body)[6] shr $18) and 4);
 dstSel:=((PDWORD(Body)[1] shr $14) and 1) or ((PDWORD(Body)[6] shr $1a) and 8) or ((PDWORD(Body)[6] shr $19) and 4);

 adrSrc:=Body^.srcAddr;
 adrDst:=Body^.dstAddr;
 byteCount:=Body^.Flags2.byteCount;

 case dstSel of
  kDmaDataDstRegister,
  kDmaDataDstRegisterNoIncrement:
    if (DWORD(adrDst)=$3022C) then
    begin
     //prefetchIntoL2
     Exit;
    end;
  else;
 end;

 Case Body^.Flags1.engine of
  CP_DMA_ENGINE_ME:
   begin
    pctx^.stream_dcb.DmaData(dstSel,adrDst,srcSel,adrSrc,byteCount,Body^.Flags1.cpSync);
   end;
  CP_DMA_ENGINE_PFP:
   begin
    //Execute on the parser side

    case (srcSel or (dstSel shl 4)) of
     (kDmaDataSrcMemory        or (kDmaDataDstMemory        shl 4)),
     (kDmaDataSrcMemoryUsingL2 or (kDmaDataDstMemory        shl 4)),
     (kDmaDataSrcMemory        or (kDmaDataDstMemoryUsingL2 shl 4)),
     (kDmaDataSrcMemoryUsingL2 or (kDmaDataDstMemoryUsingL2 shl 4)):
       begin
        Move(Pointer(adrSrc)^,Pointer(adrDst)^,byteCount);
       end;
     (kDmaDataSrcData          or (kDmaDataDstMemory        shl 4)),
     (kDmaDataSrcData          or (kDmaDataDstMemoryUsingL2 shl 4)):
       begin
        FillDWORD(Pointer(adrDst)^,(byteCount div 4),DWORD(adrSrc));
       end;
    else
       Assert(false,'DmaData: srcSel=0x'+HexStr(srcSel,1)+' dstSel=0x'+HexStr(dstSel,1));
    end;

   end;
  else
   Assert(false,'DmaData: engine=0x'+HexStr(Body^.Flags1.engine,1));
 end;

end;

procedure onWriteData(pctx:p_pfp_ctx;Body:PPM4CMDWRITEDATA);
var
 addr:PDWORD;
 count:Word;
 engineSel:Byte;
 dstSel:Byte;
begin

 Assert(Body^.CONTROL.wrOneAddr=0,'WriteData: wrOneAddr<>0');

 count:=Body^.header.count;
 if (count<3) then Exit;

 engineSel:=Body^.CONTROL.engineSel;
 dstSel:=Body^.CONTROL.dstSel;

 Case engineSel of
  WRITE_DATA_ENGINE_ME:
    begin
     pctx^.stream_dcb.WriteData(dstSel,QWORD(addr),QWORD(@Body^.DATA),count);
    end;
  WRITE_DATA_ENGINE_PFP:
    begin

     case dstSel of
      WRITE_DATA_DST_SEL_MEMORY_SYNC,  //writeDataInline
      WRITE_DATA_DST_SEL_TCL2,         //writeDataInlineThroughL2
      WRITE_DATA_DST_SEL_MEMORY_ASYNC:
        begin
         count:=count-2;
         addr:=Pointer(Body^.dstAddr);
         Move(Body^.DATA,addr^,count*SizeOf(DWORD));
        end;
      else
        Assert(false,'WriteData: dstSel=0x'+HexStr(dstSel,1));
     end;

    end;
  else
    Assert(false,'WriteData: engineSel=0x'+HexStr(engineSel,1));
 end;

end;

procedure onWaitRegMem(pctx:p_pfp_ctx;Body:PPM4CMDWAITREGMEM);
begin

 Case Body^.memSpace of
  WAIT_REG_MEM_SPACE_MEMORY:;
  else
   Assert(False,'WaitRegMem: memSpace=0x'+HexStr(Body^.memSpace,1));
 end;

 Case Body^.engine of
  WAIT_REG_MEM_ENGINE_ME:
    begin
     pctx^.stream_dcb.WaitRegMem(Body^.pollAddress,Body^.reference,Body^.mask,Body^.compareFunc);
    end;
  WAIT_REG_MEM_ENGINE_PFP:
    begin
     Assert(false,'WaitRegMem: engine=0x'+HexStr(Body^.engine,1));
    end;
  else
    Assert(false,'WaitRegMem: engine=0x'+HexStr(Body^.engine,1));
 end;

end;

procedure onAcquireMem(pctx:p_pfp_ctx;Body:PPM4ACQUIREMEM);
{var
 addr,size:QWORD;}
begin
 pctx^.UC_REG.CP_COHER_BASE_HI.COHER_BASE_HI_256B:=Body^.coherBaseHi;
 DWORD(pctx^.UC_REG.CP_COHER_CNTL)               :=Body^.coherCntl;
 pctx^.UC_REG.CP_COHER_SIZE                      :=Body^.coherSizeLo;
 pctx^.UC_REG.CP_COHER_BASE                      :=Body^.coherBaseLo;
 pctx^.UC_REG.CP_COHER_SIZE_HI.COHER_SIZE_HI_256B:=Body^.coherSizeHi;

 {
 addr:=(QWORD(Body^.coherBaseLo) shl 8) or (QWORD(Body^.coherBaseHi) shl 40);
 size:=(QWORD(Body^.coherSizeLo) shl 8) or (QWORD(Body^.coherSizeHi) shl 40);

 Writeln('onAcquireMem:');
 Writeln(' addr=0x',HexStr(addr,16));
 Writeln(' size=0x',HexStr(size,16));
 }
end;

function revbinstr(val:int64;cnt:byte):shortstring;
var
 i:Integer;
begin
 Result[0]:=AnsiChar(cnt);
 for i:=1 to cnt do
 begin
  Result[i]:=AnsiChar(48+val and 1);
  val:=val shr 1;
 end;
end;

procedure onContextControl(Body:PPM4CMDCONTEXTCONTROL);
begin
 if (DWORD(Body^.loadControl )<>$80000000) or
    (DWORD(Body^.shadowEnable)<>$80000000) then
 begin
  Writeln(stderr,' loadControl =b',revbinstr(DWORD(Body^.loadControl ),32));
  Writeln(stderr,' shadowEnable=b',revbinstr(DWORD(Body^.shadowEnable),32));
 end;
end;

procedure onSetBase(Body:PPM4CMDDRAWSETBASE);
var
 addr:QWORD;
begin
 addr:=QWORD(Body^.address);
 if (addr<>0) then
 begin
  Writeln(stderr,' baseIndex=0x',HexStr(Body^.baseIndex,4));
  Writeln(stderr,' address  =0x',HexStr(addr,16));
 end;
end;

procedure onSetPredication(Body:PPM4CMDSETPREDICATION);
var
 addr:QWORD;
begin
 addr:=QWORD(Body^.startAddress);
 if (addr<>0) then
 begin
  Writeln(stderr,' startAddress=0x',HexStr(addr,16));
  Writeln(stderr,' pred        =',Body^.predicationBoolean);
  Writeln(stderr,' hint        =',Body^.hint);
  Writeln(stderr,' predOp      =',Body^.predOp);
  Writeln(stderr,' continueBit =',Body^.continueBit);
 end;
end;

procedure onDrawPreamble(pctx:p_pfp_ctx;Body:PPM4CMDDRAWPREAMBLE);
begin
 pctx^.UC_REG.VGT_PRIMITIVE_TYPE:=Body^.control1;
 pctx^.CX_REG.IA_MULTI_VGT_PARAM:=Body^.control2;
 pctx^.CX_REG.VGT_LS_HS_CONFIG  :=Body^.control3;
end;

procedure onClearState(pctx:p_pfp_ctx;Body:Pointer);
begin
 pctx^.clear_state;
end;

const
 CONFIG_SPACE_START=$2000;

procedure onSetConfigReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=CONFIG_SPACE_START+Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   pctx^.set_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=CONFIG_SPACE_START+c-1;
 end;
end;

const
 CONTEXT_REG_BASE=$A000;

procedure onSetContextReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   pctx^.set_ctx_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=CONTEXT_REG_BASE+c-1;
 end;
end;

const
 SH_REG_BASE=$2C00;

procedure onSetShReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   pctx^.set_sh_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=SH_REG_BASE+c-1;
 end;
end;

Const
 USERCONFIG_REG_BASE=$C000;

procedure onSetUConfigReg(pctx:p_pfp_ctx;Body:PPM4CMDSETDATA);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.header.count;
 if (c<>0) then
 begin
  For i:=0 to c-1 do
  begin
   r:=USERCONFIG_REG_BASE+Body^.REG_OFFSET+i;
   v:=PDWORD(@Body^.REG_DATA)[i];
   pctx^.set_reg(r,v);
  end;
  //
  pctx^.LastSetReg:=USERCONFIG_REG_BASE+c-1;
 end;
end;

procedure onPm40(pctx:p_pfp_ctx;Body:PPM4_TYPE_0_HEADER);
var
 i,c,r:WORD;
 v:DWORD;
begin
 c:=Body^.count;
 if (c<>0) then
 For i:=0 to c-1 do
 begin
  r:=Body^.baseIndex+i;
  v:=PDWORD(@Body[1])[i];
  pctx^.set_reg(r,v);
 end;
end;

procedure onIndexBufferSize(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXBUFFERSIZE);
begin
 pctx^.CX_REG.VGT_DMA_SIZE   :=Body^.numIndices;
 pctx^.UC_REG.VGT_NUM_INDICES:=Body^.numIndices;
end;

procedure onIndexType(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXTYPE);
begin
 Assert(Body^.swapMode=0);
 pctx^.CX_REG.VGT_DMA_INDEX_TYPE.INDEX_TYPE:=Body^.indexType;
 pctx^.UC_REG.VGT_INDEX_TYPE.INDEX_TYPE    :=Body^.indexType;
end;

procedure onIndexBase(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXBASE);
begin
 Assert(Body^.baseSelect=0);
 pctx^.CX_REG.VGT_DMA_BASE             :=Body^.indexBaseLo;
 pctx^.CX_REG.VGT_DMA_BASE_HI.BASE_ADDR:=Body^.indexBaseHi;
end;

procedure onNumInstances(pctx:p_pfp_ctx;Body:PPM4CMDDRAWNUMINSTANCES);
begin
 pctx^.CX_REG.VGT_DMA_NUM_INSTANCES:=Body^.numInstances;
 pctx^.UC_REG.VGT_NUM_INSTANCES    :=Body^.numInstances;
end;

procedure onDrawIndex2(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEX2);
var
 addr:Pointer;
 size:QWORD;
begin
 if (DWORD(Body^.drawInitiator)<>0) then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 pctx^.CX_REG.VGT_DMA_MAX_SIZE         :=Body^.maxSize;
 pctx^.CX_REG.VGT_DMA_BASE             :=Body^.indexBaseLo;
 pctx^.CX_REG.VGT_DMA_BASE_HI.BASE_ADDR:=Body^.indexBaseHi;
 pctx^.CX_REG.VGT_DMA_SIZE             :=Body^.indexCount;
 pctx^.UC_REG.VGT_NUM_INDICES          :=Body^.indexCount;
 pctx^.CX_REG.VGT_DRAW_INITIATOR       :=Body^.drawInitiator;

 addr:=nil;
 size:=0;

 if get_dmem_ptr(PPointer(@Body^.indexBaseLo)^,@addr,@size) then
 begin
  //
 end else
 begin
  Assert(false,'addr:0x'+HexStr(PPointer(@Body^.indexBaseLo)^)+' not in dmem!');
 end;

 pctx^.stream_dcb.DrawIndex2(addr,
                             pctx^.SH_REG,
                             pctx^.CX_REG,
                             pctx^.UC_REG);
end;

procedure onDrawIndexAuto(pctx:p_pfp_ctx;Body:PPM4CMDDRAWINDEXAUTO);
begin
 if (DWORD(Body^.drawInitiator)<>2) then
 begin
  Writeln(stderr,' drawInitiator=b',revbinstr(DWORD(Body^.drawInitiator),32));
 end;

 pctx^.CX_REG.VGT_DMA_SIZE      :=Body^.indexCount;
 pctx^.UC_REG.VGT_NUM_INDICES   :=Body^.indexCount;
 pctx^.CX_REG.VGT_DRAW_INITIATOR:=Body^.drawInitiator;

 pctx^.stream_dcb.DrawIndexAuto(pctx^.SH_REG,
                                pctx^.CX_REG,
                                pctx^.UC_REG);
end;

procedure onPushMarker(Body:PChar);
begin
 Writeln('\HINT_PUSH_MARKER:',Body);
end;

procedure onSetMarker(Body:PChar);
begin
 Writeln('\HINT_SET_MARKER:',Body);
end;

procedure onWidthHeight(Body:PWORD);
begin
 Writeln('\HINT_',Body[0],'_',Body[1]);
end;

procedure onPrepareFlipLabel(Body:PPM4PrepareFlip);
begin
 Writeln('\HINT_PREPARE_FLIP_LABEL:0x',HexStr(Body^.ADDRES,16),':',HexStr(Body^.DATA,8));
end;

procedure onPrepareFlipWithEopInterrupt(Body:PPM4PrepareFlipWithEopInterrupt);
begin
 Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT:0x',HexStr(Body^.ADDRES,16),':',HexStr(Body^.DATA,8));
end;

procedure onPrepareFlipWithEopInterruptLabel(Body:PPM4PrepareFlipWithEopInterrupt);
begin
 Writeln('\HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL:0x',HexStr(Body^.ADDRES,16),':',HexStr(Body^.DATA,8));
end;

procedure onNop(pctx:p_pfp_ctx;Body:PDWORD);
begin

 Case pctx^.LastSetReg of
  mmPA_SC_SCREEN_SCISSOR_BR,

  mmCB_COLOR0_FMASK_SLICE,
  mmCB_COLOR1_FMASK_SLICE,
  mmCB_COLOR2_FMASK_SLICE,
  mmCB_COLOR3_FMASK_SLICE,
  mmCB_COLOR4_FMASK_SLICE,
  mmCB_COLOR5_FMASK_SLICE,
  mmCB_COLOR6_FMASK_SLICE,
  mmCB_COLOR7_FMASK_SLICE,

  mmCB_COLOR0_DCC_BASE,
  mmCB_COLOR1_DCC_BASE,
  mmCB_COLOR2_DCC_BASE,
  mmCB_COLOR3_DCC_BASE,
  mmCB_COLOR4_DCC_BASE,
  mmCB_COLOR5_DCC_BASE,
  mmCB_COLOR6_DCC_BASE,
  mmCB_COLOR7_DCC_BASE,

  mmDB_HTILE_SURFACE:
   begin
    if pctx^.print_hint then
    begin
     onWidthHeight(@Body[1]);
    end;
    Exit;
   end;
  else;
 end;

 case Body[1] of

  OP_HINT_PUSH_MARKER:
   if pctx^.print_hint then
   begin
    onPushMarker(@Body[2]);
   end;

  OP_HINT_SET_MARKER:
   if pctx^.print_hint then
   begin
    onSetMarker(@Body[2]);
   end;

  OP_HINT_PREPARE_FLIP_LABEL:
   if pctx^.print_hint then
   begin
    onPrepareFlipLabel(@Body[2]);
   end;

  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_VOID:
   if pctx^.print_hint then
   begin
    onPrepareFlipWithEopInterrupt(@Body[2]);
   end;

  OP_HINT_PREPARE_FLIP_WITH_EOP_INTERRUPT_LABEL:
   if pctx^.print_hint then
   begin
    onPrepareFlipWithEopInterruptLabel(@Body[2]);
   end;

  else
   if pctx^.print_hint then
   begin
    Writeln('\HINT_',get_hint_name(Body[1]));
   end;
 end;
end;

function pm4_parse_dcb(pctx:p_pfp_ctx;token:DWORD;buff:Pointer):Integer;
begin
 Result:=0;

 case PM4_TYPE(token) of
  0:begin //PM4_TYPE_0
     if pctx^.print_ops then Writeln('PM4_TYPE_0 len:',PM4_LENGTH(token));
     onPm40(pctx,buff);
    end;
  2:begin //PM4_TYPE_2
     if pctx^.print_ops then Writeln('PM4_TYPE_2');
     //no body
    end;
  3:begin //PM4_TYPE_3
     if pctx^.print_ops then
     if (PM4_TYPE_3_HEADER(token).opcode<>IT_NOP) or
        (not pctx^.print_hint) then
     begin
      Writeln('IT_',get_op_name(PM4_TYPE_3_HEADER(token).opcode),
                ' ',ShdrType[PM4_TYPE_3_HEADER(token).shaderType],
              ' len:',PM4_LENGTH(token));
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_NOP                :onNop(pctx,buff);
      IT_WRITE_DATA         :onWriteData      (pctx,buff);
      IT_EVENT_WRITE        :onEventWrite     (pctx,buff);
      IT_EVENT_WRITE_EOP    :onEventWriteEop  (pctx,buff);
      IT_EVENT_WRITE_EOS    :onEventWriteEos  (pctx,buff);
      IT_DMA_DATA           :onDmaData        (pctx,buff);
      IT_WAIT_REG_MEM       :onWaitRegMem     (pctx,buff);
      IT_ACQUIRE_MEM        :onAcquireMem     (pctx,buff);
      IT_CONTEXT_CONTROL    :onContextControl (buff);
      IT_DRAW_PREAMBLE      :onDrawPreamble   (pctx,buff);
      IT_CLEAR_STATE        :onClearState     (pctx,buff);
      IT_SET_CONFIG_REG     :onSetConfigReg   (pctx,buff);
      IT_SET_CONTEXT_REG    :onSetContextReg  (pctx,buff);
      IT_SET_SH_REG         :onSetShReg       (pctx,buff);
      IT_SET_UCONFIG_REG    :onSetUConfigReg  (pctx,buff);
      IT_INDEX_BUFFER_SIZE  :onIndexBufferSize(pctx,buff);
      IT_INDEX_TYPE         :onIndexType      (pctx,buff);
      IT_INDEX_BASE         :onIndexBase      (pctx,buff);
      IT_NUM_INSTANCES      :onNumInstances   (pctx,buff);
      IT_DRAW_INDEX_2       :onDrawIndex2     (pctx,buff);
      IT_DRAW_INDEX_OFFSET_2:Assert(false,'IT_DRAW_INDEX_OFFSET_2');
      IT_DRAW_INDEX_AUTO    :onDrawIndexAuto  (pctx,buff);
      IT_DISPATCH_DIRECT    :Assert(false,'IT_DISPATCH_DIRECT');
      IT_PFP_SYNC_ME        :Assert(false,'IT_PFP_SYNC_ME');

      IT_SET_BASE           :onSetBase(buff);
      IT_SET_PREDICATION    :onSetPredication(buff);

      else
       begin
        Writeln(stderr,'PM4_TYPE_3.opcode:0x',HexStr(PM4_TYPE_3_HEADER(token).opcode,2));
        Assert(False);
       end;
     end;

     case PM4_TYPE_3_HEADER(token).opcode of
      IT_SET_CONFIG_REG :;
      IT_SET_CONTEXT_REG:;
      IT_SET_SH_REG     :;
      IT_SET_UCONFIG_REG:;
      else
       pctx^.LastSetReg:=0;
     end;


    end;
  else
   begin
    Writeln(stderr,'PM4_TYPE_',PM4_TYPE(token));
    Assert(False);
   end;
 end;

end;

end.

