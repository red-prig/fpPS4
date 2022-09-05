unit emit_vbuf_load;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srType,
  srReg,
  srLayout,
  emit_fetch,
  srVBufInfo,
  emit_vbuf_chain;

type
 Tload_cache=record
  info:TBuf_info;
  v:TvarChain;
  dst:PsrRegSlot;
  elem_orig:TsrDataType;
  elem_resl:TsrDataType;
  elem_count:ptruint;
  rsl:PsrRegNode;
  elm:array[0..3] of PsrRegNode;
 end;

 TEmit_vbuf_load=class(TEmitFetch)
  procedure buf_load(info:TBuf_info);
  function  convert_e(var lc:Tload_cache;src:PsrRegNode):PsrRegNode;
  procedure make_load_cv_id(var lc:Tload_cache;i:Byte);
  procedure make_load_ce_id(var lc:Tload_cache;i:Byte);
  procedure make_load_uv_id(var lc:Tload_cache;i:Byte);
  procedure make_load_ue_id(var lc:Tload_cache;i:Byte);
  procedure make_load_zero(var lc:Tload_cache);
  procedure make_load_one(var lc:Tload_cache);
  procedure buf_load_cv(info:TBuf_info;v:TvarChain);
 end;

implementation

procedure TEmit_vbuf_load.buf_load(info:TBuf_info);
var
 v:TvarChain;
begin
 v:=TEmit_vbuf_chain(TObject(Self)).get_chain(info);

 if (v.vType=vcUniformVector) then
 begin
  //reset dst sel
  info.dsel:=dst_sel_identity;
 end;

 buf_load_cv(info,v);
end;

function TEmit_vbuf_load.convert_e(var lc:Tload_cache;src:PsrRegNode):PsrRegNode;
begin
 Result:=src;
 if (lc.elem_resl<>lc.elem_orig) then
  Case lc.info.NFMT of
   BUF_NUM_FORMAT_UNORM   :  //Unsigned, normalized to range [0.0..1.0]; data/(1<<nbits-1)
    begin
     Result:=OpUToF(src,dtFloat32);
     Result:=OpFMulToS(Result,1/lc.elem_orig.High);
    end;

   BUF_NUM_FORMAT_SNORM   :  //Signed, normalized to range [-1.0..1.0]; data/(1<<(nbits-1)-1) clamped
    begin
     Result:=OpSToF(src,dtFloat32);
     Result:=OpFMulToS(Result,1/(lc.elem_orig.High shr 1));
     Result:=OpFAddToS(Result,-1);
    end;

   BUF_NUM_FORMAT_USCALED :  //Unsigned integer to float [0.0 .. (1<<nbits)-1]
    begin
     Result:=OpUToF(src,dtFloat32);
    end;

   BUF_NUM_FORMAT_SSCALED :  //Signed integer to float [-(1<<(nbits-1)) ..(1<<(nbits-1))-1]
    begin
     Result:=OpSToF(src,dtFloat32);
    end;

   BUF_NUM_FORMAT_SNORM_NZ:  //Signed, normalized to range [-1.0..1.0]; (data*2+1)/(1<<nbits-1)
    begin
     if (lc.info.GetElemSize=4) then
     begin
      Result:=OpSToF(src,dtFloat32);
      Result:=OpFMulToS(Result,2);
      Result:=OpFAddToS(Result,1);
      Result:=OpFMulToS(Result,1/lc.elem_orig.High);
     end else
     begin
      Result:=OpShlTo(src,1);
      Result:=OpIAddTo(Result,1);
      Result:=OpSToF(Result,dtFloat32);
      Result:=OpFMulToS(Result,1/lc.elem_orig.High);
     end;
    end;

   BUF_NUM_FORMAT_UINT    :
    begin
     Result:=OpUToU(src,lc.elem_resl);
    end;

   BUF_NUM_FORMAT_SINT    :
    begin
     Result:=OpSToS(src,lc.elem_resl);
    end;

   BUF_NUM_FORMAT_FLOAT   :
    begin
     Result:=OpFToF(src,lc.elem_resl);
    end;

  end;
end;

procedure TEmit_vbuf_load.make_load_cv_id(var lc:Tload_cache;i:Byte);
var
 rsl:PsrRegNode;
begin
 rsl:=lc.rsl;
 if (rsl=nil) then
 begin
  rsl:=FetchLoad(lc.v.data[0],lc.elem_orig.AsVector(lc.elem_count));
  lc.rsl:=rsl;
 end;

 if (lc.elm[i]=nil) then
 begin

  if (lc.elem_count=1) then
  begin
   rsl:=convert_e(lc,rsl);

   lc.elm[i]:=rsl;
  end else
  begin
   lc.elm[i]:=NewReg(lc.elem_orig);

   OpExtract(line,lc.elm[i],rsl,i);

   lc.elm[i]:=convert_e(lc,lc.elm[i]);
  end;

 end;

 MakeCopy(lc.dst,lc.elm[i]);
end;

procedure TEmit_vbuf_load.make_load_ce_id(var lc:Tload_cache;i:Byte);
var
 orig,elm:PsrChain;
 sum_d:PsrRegNode;
 lvl_0:TsrChainLvl_0;
 lvl_1:TsrChainLvl_1;
 rsl:PsrRegNode;
begin

 if (lc.elm[i]=nil) then
 begin
  orig:=lc.v.data[0];
  sum_d:=orig^.pIndex;

  if (i=0) then
  begin
   elm:=orig;
  end else
  begin
   sum_d:=OpIAddTo(sum_d,i);

   lvl_0.offset:=0;
   lvl_0.size  :=orig^.size;

   lvl_1.pIndex:=sum_d;
   lvl_1.stride:=orig^.stride;

   elm:=lc.info.grp^.Fetch(@lvl_0,@lvl_1);
  end;

  rsl:=FetchLoad(elm,lc.elem_orig);

  rsl:=convert_e(lc,rsl);

  lc.elm[i]:=rsl;
 end;

 MakeCopy(lc.dst,lc.elm[i]);
end;

procedure TEmit_vbuf_load.make_load_uv_id(var lc:Tload_cache;i:Byte);
var
 rsl,idx:PsrRegNode;
begin

 rsl:=lc.rsl;
 if (rsl=nil) then
 begin
  rsl:=NewReg(lc.elem_resl.AsVector(4));
  idx:=lc.v.data[1];

  OpImageRead(line,lc.v.data[0],rsl,idx);

  lc.rsl:=rsl;
 end;

 if (lc.elm[i]=nil) then
 begin
  lc.dst^.New(line,lc.elem_resl);
  OpExtract(line,lc.dst^.current,rsl,i);

  lc.elm[i]:=lc.dst^.current;
 end else
 begin
  MakeCopy(lc.dst,lc.elm[i]);
 end;
end;

procedure TEmit_vbuf_load.make_load_ue_id(var lc:Tload_cache;i:Byte);
var
 rsl,idx,sum_d:PsrRegNode;
begin

 if (lc.elm[i]=nil) then
 begin
  idx:=lc.v.data[1];

  if (i=0) then
  begin
   sum_d:=idx;
  end else
  begin
   sum_d:=OpIAddTo(idx,i);
  end;

  rsl:=lc.dst^.New(line,lc.elem_resl);

  OpImageRead(line,lc.v.data[0],rsl,sum_d);

  lc.elm[i]:=rsl;
 end else
 begin
  MakeCopy(lc.dst,lc.elm[i]);
 end;
end;

procedure TEmit_vbuf_load.make_load_zero(var lc:Tload_cache);
begin
 SetConst_i(lc.dst,lc.elem_resl,0);
end;

procedure TEmit_vbuf_load.make_load_one(var lc:Tload_cache);
begin
 if (lc.elem_resl=dtFloat32) then
 begin
  SetConst_s(lc.dst,lc.elem_resl,1);
 end else
 begin
  SetConst_i(lc.dst,lc.elem_resl,1);
 end;
end;

procedure TEmit_vbuf_load.buf_load_cv(info:TBuf_info;v:TvarChain);
var
 lc:Tload_cache;

 i,d,count:Byte;
begin

 if info.IsExtFormat then Assert(false,'TODO');

 lc:=Default(Tload_cache);
 lc.info      :=info;
 lc.v         :=v;
 lc.elem_resl :=info.GetResultType;
 lc.elem_orig :=info.GetElemType;
 lc.elem_count:=info.GetElemCount;

 count:=info.count;

 For i:=0 to count-1 do
 begin
  lc.dst:=get_vdst8(FSPI.MUBUF.VDATA+i);
  if (lc.dst=nil) then Assert(false);

  //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
  Case info.dsel[i] of
   0:begin //0
      make_load_zero(lc);
     end;
   1:begin //1
      make_load_one(lc);
     end;
   4..7:
     begin //RGBA
      d:=info.dsel[i]-4;

      if (d<lc.elem_count) then
      begin

       Case v.vType of
        vcInvalid       :make_load_zero(lc);
        vcChainVector   :make_load_cv_id(lc,d);
        vcChainElement  :make_load_ce_id(lc,d);
        vcUniformVector :make_load_uv_id(lc,d);
        vcUniformElement:make_load_ue_id(lc,d);
       end;

      end else
      begin //as zero
       make_load_zero(lc);
      end;

     end;
   else
    begin //as zero
     make_load_zero(lc);
    end;
  end;

 end;

end;

end.

