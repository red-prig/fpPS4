unit emit_MTBUF;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srReg,
  srLayout,
  emit_fetch,
  srVBufInfo,
  emit_vbuf_load,
  emit_vbuf_store;

type
 TEmit_MTBUF=class(TEmitFetch)
  procedure emit_MTBUF;
  procedure emit_TBUFFER_LOAD_FORMAT (count:Byte);
  procedure emit_TBUFFER_STORE_FORMAT(count:Byte);
 end;

implementation

procedure TEmit_MTBUF.emit_TBUFFER_LOAD_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;

begin
 if not get_srsrc(FSPI.MTBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);

 TEmit_vbuf_load(TObject(Self)).buf_load(
  Buf_info(grp,
           dst_sel_identity,
           FSPI.MTBUF.DFMT,
           FSPI.MTBUF.NFMT,
           count,
           FSPI.MTBUF.GLC,
           FSPI.MTBUF.SLC)
 );

end;

procedure TEmit_MTBUF.emit_TBUFFER_STORE_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;

begin
 if not get_srsrc(FSPI.MTBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);

 TEmit_vbuf_store(TObject(Self)).buf_store(
  Buf_info(grp,
           dst_sel_identity,
           FSPI.MTBUF.DFMT,
           FSPI.MTBUF.NFMT,
           count,
           FSPI.MTBUF.GLC,
           FSPI.MTBUF.SLC)
 );

end;

procedure TEmit_MTBUF.emit_MTBUF;
begin

 case FSPI.MTBUF.OP of
  TBUFFER_LOAD_FORMAT_X    :emit_TBUFFER_LOAD_FORMAT(1);
  TBUFFER_LOAD_FORMAT_XY   :emit_TBUFFER_LOAD_FORMAT(2);
  TBUFFER_LOAD_FORMAT_XYZ  :emit_TBUFFER_LOAD_FORMAT(3);
  TBUFFER_LOAD_FORMAT_XYZW :emit_TBUFFER_LOAD_FORMAT(4);

  TBUFFER_STORE_FORMAT_X   :emit_TBUFFER_STORE_FORMAT(1);
  TBUFFER_STORE_FORMAT_XY  :emit_TBUFFER_STORE_FORMAT(2);
  TBUFFER_STORE_FORMAT_XYZ :emit_TBUFFER_STORE_FORMAT(3);
  TBUFFER_STORE_FORMAT_XYZW:emit_TBUFFER_STORE_FORMAT(4);

  else
      Assert(false,'MTBUF?'+IntToStr(FSPI.MTBUF.OP)+' '+get_str_spi(FSPI));
 end;

 //OFFSET:bit12; //Unsigned byte offset.
 //OFFEN:bit1;   //enable offset in VADDR
 //IDXEN:bit1;   //enable index in VADDR
 //GLC:bit1;     //globally coherent.
 //
 //VADDR:Byte;   //VGPR address source.
 //VDATA:Byte;   //Vector  GPR to read/write result  to.
 //SRSRC:bit5;   //Scalar GPR that specifies the resource constant, in units of four  SGPRs
 //SLC:bit1;     //System Level Coherent.
 //TFE:bit1;     //Texture Fail Enable
 //SOFFSET:Byte;

end;

end.

