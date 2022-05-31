unit emit_vbuf_chain;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  ps4_shader,
  ps4_pssl,
  srTypes,
  srConst,
  srReg,
  srLayout,
  SprvEmit,
  emit_op,
  srVBufInfo;

type
 TBuf_adr=packed record
  stride,align,fsize,csize:PtrInt;
  soffset,ioffset,voffset:PtrInt;
  sof,ofs,idx:PsrRegNode;
 end;

 TvcType=(vcInvalid,vcChainVector,vcChainElement,vcUniformVector,vcUniformElement);

 TvarChain=record
  vType:TvcType;
  data:array[0..1] of Pointer;
 end;

 TEmit_vbuf_chain=object(TEmitOp)
  function  OpDivTo(pReg:PsrRegNode;i:QWORD):PsrRegNode;
  function  OpMulTo(pReg:PsrRegNode;i:QWORD):PsrRegNode;
  function  OpAddTo(pReg:PsrRegNode;i:QWORD):PsrRegNode;
  function  OpAddTo(pReg0,pReg1:PsrRegNode):PsrRegNode;
  procedure get_reg_adr(var adr:TBuf_adr);
  function  get_sum_ofs(var adr:TBuf_adr):PsrRegNode;
  function  get_idx_elm(var adr:TBuf_adr):PsrRegNode;
  function  get_idx_fmt(var adr:TBuf_adr):PsrRegNode;
  function  get_chain(info:TBuf_info):TvarChain;
 end;

implementation

function isPowerOfTwo(x:QWORD):Boolean; inline;
begin
 Result:=((x-1) and x)=0;
end;

function fastIntLog2(i:QWORD):QWORD; inline;
begin
 Result:=BsfQWORD(i);
end;

function TEmit_vbuf_chain.OpDivTo(pReg:PsrRegNode;i:QWORD):PsrRegNode;
begin
 if (pReg=nil) or (i<=1) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 if isPowerOfTwo(i) then
 begin
  i:=fastIntLog2(i);
  if (SignType(pReg^.dtype)<>0) then
  begin
   _emit_OpShrA(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
  end else
  begin
   _emit_OpShr(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
  end;
 end else
 begin
  if (SignType(pReg^.dtype)<>0) then
  begin
   _emit_OpSDiv(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
  end else
  begin
   _emit_OpUDiv(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
  end;
 end;
end;

function TEmit_vbuf_chain.OpMulTo(pReg:PsrRegNode;i:QWORD):PsrRegNode;
begin
 if (pReg=nil) or (i<=1) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 if isPowerOfTwo(i) then
 begin
  i:=fastIntLog2(i);
  _emit_OpShl(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
 end else
 begin
  _emit_OpIMul(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
 end;
end;

function TEmit_vbuf_chain.OpAddTo(pReg:PsrRegNode;i:QWORD):PsrRegNode;
begin
 if (pReg=nil) or (i=0) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 _emit_OpIAdd(line,Result,pReg,FetchReg(FConsts.Fetchi(pReg^.dtype,i)));
end;

function TEmit_vbuf_chain.OpAddTo(pReg0,pReg1:PsrRegNode):PsrRegNode;
begin
 if (pReg0=nil) then Exit(pReg1);
 if (pReg1=nil) then Exit(pReg0);
 Result:=NewReg(pReg0^.dtype);
 pReg0^.mark_read;
 pReg1^.mark_read;
 _emit_OpIAdd(line,Result,pReg0,pReg1);
end;

procedure TEmit_vbuf_chain.get_reg_adr(var adr:TBuf_adr);
var
 ofs:PsrRegSlot;
begin
 if is_const_soffset(FSPI.MUBUF.SOFFSET) then
 begin
  adr.soffset:=get_soffset_const_int(FSPI.MUBUF.SOFFSET);
 end else
 begin
  ofs:=FRegsStory.get_ssrc8(FSPI.MUBUF.SOFFSET);
  if (ofs<>nil) then
  begin
   adr.sof:=ofs^.current;
   if (adr.sof<>nil) then
   begin
    if (adr.sof^.is_const) then
    begin
     adr.soffset:=adr.sof^.AsConst^.AsInt;
     adr.sof:=nil;
    end else
    begin
     Assert(false,'FSPI.MUBUF.SOFFSET');
    end;
   end;
  end;
 end;

 ofs:=nil;
 if (FSPI.MUBUF.IDXEN=1) then
 begin
  adr.idx:=fetch_vsrc8(FSPI.MUBUF.VADDR+0,dtInt32);
  if (FSPI.MUBUF.OFFEN=1) then
  begin
   ofs:=FRegsStory.get_vsrc8(FSPI.MUBUF.VADDR+1);
  end;
 end else
 if (FSPI.MUBUF.OFFEN=1) then
 begin
  ofs:=FRegsStory.get_vsrc8(FSPI.MUBUF.VADDR+0);
 end;

 if (ofs<>nil) then
 begin
  adr.ofs:=ofs^.current;
  if (adr.ofs<>nil) then
  begin
   if (adr.ofs^.is_const) then
   begin
    adr.voffset:=adr.ofs^.AsConst^.AsInt;
    adr.ofs:=nil;
   end else
   begin
    adr.ofs:=MakeRead(ofs,dtInt32);
   end;
  end;
 end;
end;

function TEmit_vbuf_chain.get_sum_ofs(var adr:TBuf_adr):PsrRegNode;
var
 foffset:PtrInt;

 sum_d,ofs_d,idx_m:PsrRegNode;

begin
 foffset:=adr.soffset+adr.ioffset+adr.voffset;

 if (foffset mod adr.align=0) and       //const offset is align
    ((adr.idx=nil) or                   //no index or
     (adr.stride mod adr.align=0)) then //stride is align
 begin

  //(foffset is Align)
  //(stride  is Align)
  //result=(foffset/Align+ofs/Align+idx*(stride/Align)) in elem

  adr.ofs^.mark_unread;
  ofs_d:=OpDivTo(adr.ofs,adr.align); //ofs/Align
  sum_d:=OpAddTo(ofs_d,foffset div adr.align);  //foffset/Align+ofs/Align

  if (adr.idx<>nil) then
  begin
   adr.idx^.mark_unread;
   idx_m:=OpMulTo(adr.idx,adr.stride div adr.align); //idx*(stride/Align)
   sum_d:=OpAddTo(sum_d,idx_m);
  end;

 end else
 begin

  //result=(foffset+ofs+idx*stride)/Align in elem

  adr.ofs^.mark_unread;
  sum_d:=OpAddTo(adr.ofs,foffset);  //foffset+ofs

  if (adr.idx<>nil) then
  begin
   adr.idx^.mark_unread;
   idx_m:=OpMulTo(adr.idx,adr.stride); //idx*stride
   sum_d:=OpAddTo(sum_d,idx_m);
  end;

  sum_d:=OpDivTo(sum_d,adr.align); // sum/Align
 end;

 Result:=sum_d;
end;

function TEmit_vbuf_chain.get_idx_elm(var adr:TBuf_adr):PsrRegNode;
var
 foffset:PtrInt;

 sum_d,idx_m:PsrRegNode;

begin
 //result=(foffset/Align+idx*(stride/Align)) in elem

 foffset:=adr.soffset+adr.ioffset+adr.voffset;
 foffset:=foffset div adr.align;

 if (adr.idx=nil) then
 begin
  sum_d:=FetchReg(FConsts.Fetchi(dtUint32,foffset));
  sum_d^.mark_unread;
 end else
 begin
  adr.idx^.mark_unread;
  idx_m:=OpMulTo(adr.idx,adr.stride div adr.align); //idx*(stride/Align)
  sum_d:=OpAddTo(idx_m,foffset);
 end;

 Result:=sum_d;
end;

function TEmit_vbuf_chain.get_idx_fmt(var adr:TBuf_adr):PsrRegNode;
var
 foffset:PtrInt;

 sum_d,idx_m:PsrRegNode;

begin
 //result=(foffset/size+idx*(stride/size)) in format

 foffset:=adr.soffset+adr.ioffset+adr.voffset;
 foffset:=foffset div adr.fsize;

 if (adr.idx=nil) then
 begin
  sum_d:=FetchReg(FConsts.Fetchi(dtUint32,foffset));
  sum_d^.mark_unread;
 end else
 begin
  adr.idx^.mark_unread;
  idx_m:=OpMulTo(adr.idx,adr.stride div adr.fsize); //idx*(stride/size)
  sum_d:=OpAddTo(idx_m,foffset);
 end;

 Result:=sum_d;
end;

function Min(a,b:PtrInt):PtrInt; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function TEmit_vbuf_chain.get_chain(info:TBuf_info):TvarChain;
var
 PV:PVSharpResource4;

 adr:TBuf_adr;
 foffset:PtrInt;

 sum_d:PsrRegNode;

 ext:TsrChainExt;

 img:TsrImageInfo;

begin
 Result:=Default(TvarChain);

 if (info.GetElemCount=0) then
 begin
  Result.vType:=vcInvalid;
  Exit;
 end;

 PV:=info.grp^.pData;
 Assert(PV^.swizzle_en=0,'swizzle_en');
 Assert(PV^.addtid_en =0,'addtid_en');

 adr:=Default(TBuf_adr);
 adr.stride :=PV^.stride;
 adr.align  :=info.GetAlignSize;
 adr.fsize  :=info.GetSizeFormat;
 adr.csize  :=Min(info.GetElemSize*info.count,adr.fsize);
 adr.ioffset:=FSPI.MUBUF.OFFSET;

 if (adr.stride=0) then adr.stride:=1;

 get_reg_adr(adr);

 foffset:=adr.soffset+adr.ioffset+adr.voffset;
 Assert(foffset>=0,'WTF');

 if (adr.ofs=nil) and (adr.idx=nil) then //simple
 begin

  Result.vType  :=vcChainVector;
  Result.data[0]:=info.grp^.Fetch(foffset,adr.csize,nil);

  Exit;
 end;

 //result=Align(foffset+ofs+idx*stride)  in byte

 if (adr.ofs<>nil) then
 begin
  sum_d:=get_sum_ofs(adr);

  //minTexelBufferOffsetAlignment
  if FUseTexelBuffer then
  if (info.IsComp) and
     (adr.stride div adr.align=0) then
  begin
   //is uniform buffer per element

   img:=info.GetImageInfoElement;

   Result.vType  :=vcUniformElement;
   Result.data[0]:=FetchImage(info.grp,img.dtype,img.tinfo);
   Result.data[1]:=sum_d;

   Exit;
  end;

  sum_d^.mark_read;
  ext:=Default(TsrChainExt);
  ext.pIndex:=sum_d;
  ext.stride:=adr.align;

  Result:=Default(TvarChain);
  Result.vType  :=vcChainElement;
  Result.data[0]:=info.grp^.Fetch(0,adr.align,@ext);

  Exit;
 end else
 begin //idx<>nil

  //minTexelBufferOffsetAlignment
  if FUseTexelBuffer then
  if (info.IsComp) and
     (adr.stride mod adr.fsize=0) and
     (foffset mod adr.fsize=0) then
  begin
   //is uniform buffer per format

   sum_d:=get_idx_fmt(adr);

   img:=info.GetImageInfo;

   Result.vType  :=vcUniformVector;
   Result.data[0]:=FetchImage(info.grp,img.dtype,img.tinfo);
   Result.data[1]:=sum_d;

   Exit;
  end;

  //minTexelBufferOffsetAlignment
  if FUseTexelBuffer then
  if (info.IsComp) and
     (adr.stride div adr.align=0) then
  begin
   //is uniform buffer per element

   sum_d:=get_idx_elm(adr);

   img:=info.GetImageInfoElement;

   Result.vType  :=vcUniformElement;
   Result.data[0]:=FetchImage(info.grp,img.dtype,img.tinfo);
   Result.data[1]:=sum_d;

   Exit;
  end;

  //adr.idx^.mark_read;
  ext:=Default(TsrChainExt);
  ext.pIndex:=adr.idx;
  ext.stride:=adr.stride;

  Result.vType  :=vcChainVector;
  Result.data[0]:=info.grp^.Fetch(foffset,adr.csize,@ext);

  Exit;
 end;

end;

end.

