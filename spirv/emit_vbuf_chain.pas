unit emit_vbuf_chain;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  ps4_shader,
  ps4_pssl,
  srType,
  srConst,
  srReg,
  srLayout,
  srConfig,
  emit_fetch,
  srVBufInfo;

type
 TBuf_adr=packed record
  stride      :PtrInt;
  aligned_size:PtrInt;
  format_size :PtrInt;
  csize       :PtrInt;
  soffset,ioffset,voffset:PtrInt;
  ofs,idx:TsrRegNode;
 end;

 TvcType=(vcInvalid,vcChainVector,vcChainElement,vcUniformVector,vcUniformElement);

 TvarChain=record
  vType:TvcType;
  data:array[0..1] of Pointer;
 end;

 TEmit_vbuf_chain=class(TEmitFetch)
  procedure get_reg_adr(var adr:TBuf_adr);
  function  get_sum_ofs(var adr:TBuf_adr):TsrRegNode;
  function  get_idx_elm(var adr:TBuf_adr):TsrRegNode;
  function  get_idx_fmt(var adr:TBuf_adr):TsrRegNode;
  function  get_chain  (info:TBuf_info;dtype:TsrDataType):TvarChain;
 end;

implementation

procedure TEmit_vbuf_chain.get_reg_adr(var adr:TBuf_adr);
var
 pSlot:PsrRegSlot;
 sof:TsrRegNode;
 ofs:TsrRegNode;
begin
 sof:=nil;
 ofs:=nil;

 if is_const_soffset(FSPI.MUBUF.SOFFSET) then
 begin
  adr.soffset:=get_soffset_const_int(FSPI.MUBUF.SOFFSET,FSPI.INLINE32);
 end else
 begin
  pSlot:=get_ssrc8(FSPI.MUBUF.SOFFSET);
  if (pSlot<>nil) then
  begin
   sof:=pSlot^.current;
   if (sof<>nil) then
   begin
    if (sof.is_const) then
    begin
     adr.soffset:=sof.AsConst.AsInt32;
     sof:=nil;
    end else
    begin
     sof:=MakeRead(pSlot,dtInt32);
    end;
   end;
  end;
 end;

 pSlot:=nil;
 if (FSPI.MUBUF.IDXEN=1) then
 begin
  if (adr.stride<>0) then //ignore index when stride=0
  begin
   adr.idx:=fetch_vsrc8(FSPI.MUBUF.VADDR+0,dtInt32);
  end;
  if (FSPI.MUBUF.OFFEN=1) then
  begin
   pSlot:=get_vsrc8(FSPI.MUBUF.VADDR+1);
  end;
 end else
 if (FSPI.MUBUF.OFFEN=1) then
 begin
  pSlot:=get_vsrc8(FSPI.MUBUF.VADDR+0);
 end;

 if (pSlot<>nil) then
 begin
  ofs:=pSlot^.current;
  if (ofs<>nil) then
  begin
   if (ofs.is_const) then
   begin
    adr.voffset:=ofs.AsConst.AsInt32;
    ofs:=nil;
   end else
   begin
    ofs:=MakeRead(pSlot,dtInt32);
   end;
  end;
 end;

 if (sof<>nil) and (ofs<>nil) then
 begin
  adr.ofs:=OpIAddTo(sof,ofs);
 end else
 if (sof<>nil) then
 begin
  adr.ofs:=sof;
 end else
 begin
  adr.ofs:=ofs;
 end;

end;

function TEmit_vbuf_chain.get_sum_ofs(var adr:TBuf_adr):TsrRegNode;
var
 foffset:PtrInt;

 sum_d,ofs_d,idx_m:TsrRegNode;

begin
 foffset:=adr.soffset+adr.ioffset+adr.voffset;

 if (foffset mod adr.aligned_size=0) and       //const offset is align
    ((adr.idx=nil) or                          //no index or
     (adr.stride mod adr.aligned_size=0)) then //stride is align
 begin

  //(foffset is Align)
  //(stride  is Align)
  //result=(foffset/Align+ofs/Align+idx*(stride/Align)) in elem

  ofs_d:=OpIDivTo(adr.ofs,adr.aligned_size);           //ofs/Align
  sum_d:=OpIAddTo(ofs_d,foffset div adr.aligned_size); //foffset/Align+ofs/Align

  if (adr.idx<>nil) then
  begin
   idx_m:=OpIMulTo(adr.idx,adr.stride div adr.aligned_size); //idx*(stride/Align)
   sum_d:=OpIAddTo(sum_d,idx_m);
  end;

 end else
 begin

  //result=(foffset+ofs+idx*stride)/Align in elem

  sum_d:=OpIAddTo(adr.ofs,foffset);  //foffset+ofs

  if (adr.idx<>nil) then
  begin
   idx_m:=OpIMulTo(adr.idx,adr.stride); //idx*stride
   sum_d:=OpIAddTo(sum_d,idx_m);
  end;

  sum_d:=OpIDivTo(sum_d,adr.aligned_size); // sum/Align
 end;

 Result:=sum_d;
end;

function TEmit_vbuf_chain.get_idx_elm(var adr:TBuf_adr):TsrRegNode;
var
 foffset:PtrInt;

 sum_d,idx_m:TsrRegNode;

begin
 //result=(foffset/Align+idx*(stride/Align)) in elem

 foffset:=adr.soffset+adr.ioffset+adr.voffset;
 foffset:=foffset div adr.aligned_size;

 if (adr.idx=nil) then
 begin
  sum_d:=NewImm_q(dtUint32,foffset);
 end else
 begin
  idx_m:=OpIMulTo(adr.idx,adr.stride div adr.aligned_size); //idx*(stride/Align)
  sum_d:=OpIAddTo(idx_m,foffset);
 end;

 Result:=sum_d;
end;

function TEmit_vbuf_chain.get_idx_fmt(var adr:TBuf_adr):TsrRegNode;
var
 foffset:PtrInt;

 sum_d,idx_m:TsrRegNode;

begin
 //result=(foffset/size+idx*(stride/size)) in format

 foffset:=adr.soffset+adr.ioffset+adr.voffset;
 foffset:=foffset div adr.format_size;

 if (adr.idx=nil) then
 begin
  sum_d:=NewImm_q(dtUint32,foffset);
 end else
 begin
  idx_m:=OpIMulTo(adr.idx,adr.stride div adr.format_size); //idx*(stride/size)
  sum_d:=OpIAddTo(idx_m,foffset);
 end;

 Result:=sum_d;
end;

function Min(a,b:PtrInt):PtrInt; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function TEmit_vbuf_chain.get_chain(info:TBuf_info;dtype:TsrDataType):TvarChain;
var
 PV:PVSharpResource4;

 adr:TBuf_adr;
 foffset:PtrInt;

 sum_d:TsrRegNode;

 lvl_0:TsrChainLvl_0;
 lvl_1:TsrChainLvl_1;

 img:TsrImageInfo;

begin
 Result:=Default(TvarChain);

 if (info.GetElemCount=0) then
 begin
  Result.vType:=vcInvalid;
  Exit;
 end;

 PV:=info.grp.GetSharp;
 Assert(PV^.swizzle_en=0,'swizzle_en');
 Assert(PV^.addtid_en =0,'addtid_en');

 adr:=Default(TBuf_adr);
 adr.stride      :=PV^.stride;
 adr.aligned_size:=info.GetAlignSize;
 adr.format_size :=info.GetSizeFormat;
 adr.csize       :=adr.format_size;
 //adr.csize     :=Min(info.GetElemSize*info.count,adr.fsize);
 adr.ioffset     :=FSPI.MUBUF.OFFSET;

 get_reg_adr(adr);

 if (adr.stride=0) then
 begin
  adr.stride:=adr.aligned_size; //set to size
 end;

 foffset:=adr.soffset+adr.ioffset+adr.voffset;
 Assert(foffset>=0,'WTF');

 if (adr.ofs=nil) and (adr.idx=nil) then //simple
 begin

  lvl_0.offset:=foffset;
  lvl_0.size  :=adr.csize;
  Result.data[0]:=info.grp.Fetch(@lvl_0,nil,cflags(dtype,info.GLC,info.SLC));

  Result.vType  :=vcChainVector;
  Exit;
 end;

 //result=Align(foffset+ofs+idx*stride)  in byte

 if (adr.ofs<>nil) then
 begin
  sum_d:=get_sum_ofs(adr);

  //minTexelBufferOffsetAlignment
  if Config.UseTexelBuffer then
  if (info.IsComp) and
     (adr.stride div adr.aligned_size=0) then
  begin
   //is uniform buffer per element

   img:=info.GetImageInfoElement;

   Result.vType  :=vcUniformElement;
   Result.data[0]:=FetchImage(info.grp,img);
   Result.data[1]:=sum_d;

   Exit;
  end;

  lvl_0.offset:=0;
  lvl_0.size  :=adr.aligned_size;

  lvl_1.pIndex:=sum_d;
  lvl_1.stride:=adr.aligned_size;

  Result.data[0]:=info.grp.Fetch(@lvl_0,@lvl_1,cflags(dtype,info.GLC,info.SLC));

  Result.vType  :=vcChainElement;
  Exit;
 end else
 begin //idx<>nil

  //minTexelBufferOffsetAlignment
  if Config.UseTexelBuffer then
  if (info.IsComp) and
     (adr.stride mod adr.format_size=0) and
     (foffset mod adr.format_size=0) then
  begin
   //is uniform buffer per format

   sum_d:=get_idx_fmt(adr);

   img:=info.GetImageInfo;

   Result.vType  :=vcUniformVector;
   Result.data[0]:=FetchImage(info.grp,img);
   Result.data[1]:=sum_d;

   Exit;
  end;

  //minTexelBufferOffsetAlignment
  if Config.UseTexelBuffer then
  if (info.IsComp) and
     (adr.stride div adr.aligned_size=0) then
  begin
   //is uniform buffer per element

   sum_d:=get_idx_elm(adr);

   img:=info.GetImageInfoElement;

   Result.vType  :=vcUniformElement;
   Result.data[0]:=FetchImage(info.grp,img);
   Result.data[1]:=sum_d;

   Exit;
  end;

  lvl_0.offset:=foffset;
  lvl_0.size  :=adr.csize;

  lvl_1.pIndex:=adr.idx;
  lvl_1.stride:=adr.stride;

  Result.data[0]:=info.grp.Fetch(@lvl_0,@lvl_1,cflags(dtype,info.GLC,info.SLC));

  Result.vType  :=vcChainVector;
  Exit;
 end;

end;

end.

