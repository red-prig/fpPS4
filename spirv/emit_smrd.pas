unit emit_SMRD;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srType,
  srConst,
  srReg,
  srLayout,
  emit_fetch;

type
 TEmit_SMRD=class(TEmitFetch)
  procedure emit_SMRD;
  procedure emit_LOAD_DWORDX(grp:TsrDataLayout;count:Byte);
  procedure emit_LOAD_DWORDX(count:Byte);
  procedure emit_BUFFER_LOAD_DWORDX(count:Byte);
 end;

implementation

procedure TEmit_SMRD.emit_LOAD_DWORDX(grp:TsrDataLayout;count:Byte);
var
 dst:PsrRegSlot;

 ofs_r,idx_r:TsrRegNode;

 lvl_0:TsrChainLvl_0;
 lvl_1:TsrChainLvl_1;

 i:Byte;

 is_const_offset:Boolean;
 offset:PtrUint;
begin

 if (FSPI.SMRD.IMM<>0) then
 begin
  offset:=FSPI.SMRD.OFFSET;
  //offset is represented as a DWORD value
  offset:=offset*4;
  is_const_offset:=True;
 end else
 if is_const_ssrc9(FSPI.SMRD.OFFSET) then
 begin
  offset:=get_soffset_const_int(FSPI.SMRD.OFFSET,FSPI.INLINE32);
  //offset is represented as a DWORD value,
  // even for the case when it is not an IMM,
  // but a declaration of a constant value
  offset:=offset*4;
  is_const_offset:=True;
 end else
 begin
  is_const_offset:=False;
 end;

 if is_const_offset then
 begin
  For i:=0 to count-1 do
  begin
   dst:=get_sdst7(FSPI.SMRD.SDST+i);
   Assert(dst<>nil);

   lvl_0.offset:=offset+i*4;
   lvl_0.size  :=4;

   MakeChain(dst,grp,@lvl_0,nil);
  end;
 end else
 begin
  ofs_r:=fetch_ssrc9(FSPI.SMRD.OFFSET,dtUint32);

  idx_r:=OpShrTo(ofs_r,2);

  For i:=0 to count-1 do
  begin
   dst:=get_sdst7(FSPI.SMRD.SDST+i);
   Assert(dst<>nil);

   if (i=0) then
   begin
    ofs_r:=idx_r;
   end else
   begin
    ofs_r:=OpIAddTo(idx_r,i);
   end;

   lvl_0.offset:=0;
   lvl_0.size  :=4;

   lvl_1.pIndex:=ofs_r;
   lvl_1.stride:=4;

   MakeChain(dst,grp,@lvl_0,@lvl_1);
  end;
 end;

end;

procedure TEmit_SMRD.emit_LOAD_DWORDX(count:Byte);
var
 src:array[0..1] of PsrRegSlot;
 grp:TsrDataLayout;
begin
 if not get_sbase(FSPI.SMRD.SBASE,2,@src) then Assert(false);
 grp:=GroupingSharp(@src,rtBufPtr2);
 emit_LOAD_DWORDX(grp,count);
end;

procedure TEmit_SMRD.emit_BUFFER_LOAD_DWORDX(count:Byte);
var
 src:array[0..3] of PsrRegSlot;
 grp:TsrDataLayout;
begin
 if not get_sbase(FSPI.SMRD.SBASE,4,@src) then Assert(false);
 grp:=GroupingSharp(@src,rtVSharp4);
 emit_LOAD_DWORDX(grp,count);
end;

procedure TEmit_SMRD.emit_SMRD;
begin

 Case FSPI.SMRD.OP of
  S_BUFFER_LOAD_DWORD   : emit_BUFFER_LOAD_DWORDX(1);
  S_BUFFER_LOAD_DWORDX2 : emit_BUFFER_LOAD_DWORDX(2);
  S_BUFFER_LOAD_DWORDX4 : emit_BUFFER_LOAD_DWORDX(4);
  S_BUFFER_LOAD_DWORDX8 : emit_BUFFER_LOAD_DWORDX(8);
  S_BUFFER_LOAD_DWORDX16: emit_BUFFER_LOAD_DWORDX(16);

  S_LOAD_DWORD   : emit_LOAD_DWORDX(1);
  S_LOAD_DWORDX2 : emit_LOAD_DWORDX(2);
  S_LOAD_DWORDX4 : emit_LOAD_DWORDX(4);
  S_LOAD_DWORDX8 : emit_LOAD_DWORDX(8);
  S_LOAD_DWORDX16: emit_LOAD_DWORDX(16);

  else
    Assert(false,'SMRD?'+IntToStr(FSPI.SMRD.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

