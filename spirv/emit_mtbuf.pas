unit emit_MTBUF;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srReg,
  srLayout,
  SprvEmit,
  srVBufInfo,
  emit_vbuf_load,
  emit_vbuf_store,
  emit_op;

type
 TEmit_MTBUF=object(TEmitOp)
  procedure _emit_MTBUF;
  procedure _emit_TBUFFER_LOAD_FORMAT(count:Byte);
  procedure _emit_TBUFFER_STORE_FORMAT(count:Byte);
 end;

implementation

procedure TEmit_MTBUF._emit_TBUFFER_LOAD_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;

begin
 if not FRegsStory.get_srsrc(FSPI.MTBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);

 TEmit_vbuf_load(Self).buf_load(
  Buf_info(grp,
           dst_sel_identity,
           FSPI.MTBUF.DFMT,
           FSPI.MTBUF.NFMT,
           count)
 );

end;

procedure TEmit_MTBUF._emit_TBUFFER_STORE_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;

begin
 if not FRegsStory.get_srsrc(FSPI.MTBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);

 TEmit_vbuf_store(Self).buf_store(
  Buf_info(grp,
           dst_sel_identity,
           FSPI.MTBUF.DFMT,
           FSPI.MTBUF.NFMT,
           count)
 );

end;

procedure TEmit_MTBUF._emit_MTBUF;
begin

 case FSPI.MTBUF.OP of
  TBUFFER_LOAD_FORMAT_X    :_emit_TBUFFER_LOAD_FORMAT(1);
  TBUFFER_LOAD_FORMAT_XY   :_emit_TBUFFER_LOAD_FORMAT(2);
  TBUFFER_LOAD_FORMAT_XYZ  :_emit_TBUFFER_LOAD_FORMAT(3);
  TBUFFER_LOAD_FORMAT_XYZW :_emit_TBUFFER_LOAD_FORMAT(4);

  TBUFFER_STORE_FORMAT_X   :_emit_TBUFFER_STORE_FORMAT(1);
  TBUFFER_STORE_FORMAT_XY  :_emit_TBUFFER_STORE_FORMAT(2);
  TBUFFER_STORE_FORMAT_XYZ :_emit_TBUFFER_STORE_FORMAT(3);
  TBUFFER_STORE_FORMAT_XYZW:_emit_TBUFFER_STORE_FORMAT(4);
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

