unit emit_vbuf_load;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  spirv,
  ps4_pssl,
  srTypes,
  srConst,
  srReg,
  srLayout,
  SprvEmit,
  emit_op,
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

 TEmit_vbuf_load=object(TEmitOp)
  procedure buf_load(info:TBuf_info);
  function  OpMulF(pReg:PsrRegNode;s:Single):PsrRegNode;
  function  OpAddF(pReg:PsrRegNode;s:Single):PsrRegNode;
  function  OpUToF(pReg:PsrRegNode):PsrRegNode;
  function  OpSToF(pReg:PsrRegNode):PsrRegNode;
  function  OpShlI(pReg:PsrRegNode;i:DWORD):PsrRegNode;
  function  OpAddI(pReg:PsrRegNode;i:DWORD):PsrRegNode;
  function  _convert_e(var lc:Tload_cache;src:PsrRegNode):PsrRegNode;
  procedure _make_load_cv_id(var lc:Tload_cache;i:Byte);
  procedure _make_load_ce_id(var lc:Tload_cache;i:Byte);
  procedure _make_load_uv_id(var lc:Tload_cache;i:Byte);
  procedure _make_load_ue_id(var lc:Tload_cache;i:Byte);
  procedure _make_load_zero(var lc:Tload_cache);
  procedure _make_load_one(var lc:Tload_cache);
  procedure buf_load_cv(info:TBuf_info;v:TvarChain);
 end;

implementation

procedure TEmit_vbuf_load.buf_load(info:TBuf_info);
var
 v:TvarChain;
begin
 v:=TEmit_vbuf_chain(Self).get_chain(info);

 if (v.vType=vcUniformVector) then
 begin
  //reset dst sel
  info.dsel:=dst_sel_identity;
 end;

 buf_load_cv(info,v);
end;

function TEmit_vbuf_load.OpMulF(pReg:PsrRegNode;s:Single):PsrRegNode;
begin
 if (pReg=nil) or (s=1) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 _emit_Op2(line,Op.OpFMul,Result,pReg,FetchReg(FConsts.Fetchf(dtFloat32,s)));
end;

function TEmit_vbuf_load.OpAddF(pReg:PsrRegNode;s:Single):PsrRegNode;
begin
 if (pReg=nil) or (s=0) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 _emit_Op2(line,Op.OpFAdd,Result,pReg,FetchReg(FConsts.Fetchf(dtFloat32,s)));
end;

function TEmit_vbuf_load.OpUToF(pReg:PsrRegNode):PsrRegNode;
begin
 if (pReg=nil) then Exit(pReg);
 Result:=NewReg(dtFloat32);
 pReg^.mark_read;
 _emit_Op1(line,Op.OpConvertUToF,Result,pReg);
end;

function TEmit_vbuf_load.OpSToF(pReg:PsrRegNode):PsrRegNode;
begin
 if (pReg=nil) then Exit(pReg);
 Result:=NewReg(dtFloat32);
 pReg^.mark_read;
 _emit_Op1(line,Op.OpConvertSToF,Result,pReg);
end;

function TEmit_vbuf_load.OpShlI(pReg:PsrRegNode;i:DWORD):PsrRegNode;
begin
 if (pReg=nil) or (i=0) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 _emit_OpShl(line,Result,pReg,FetchReg(FConsts.Fetchi(dtUint32,i)));
end;

function TEmit_vbuf_load.OpAddI(pReg:PsrRegNode;i:DWORD):PsrRegNode;
begin
 if (pReg=nil) or (i=0) then Exit(pReg);
 Result:=NewReg(pReg^.dtype);
 pReg^.mark_read;
 _emit_OpIAdd(line,Result,pReg,FetchReg(FConsts.Fetchi(dtUint32,i)));
end;

function TEmit_vbuf_load._convert_e(var lc:Tload_cache;src:PsrRegNode):PsrRegNode;
begin
 Result:=src;
 if (lc.elem_resl<>lc.elem_orig) then
  Case lc.info.NFMT of
   BUF_NUM_FORMAT_UNORM   :  //Unsigned, normalized to range [0.0..1.0]; data/(1<<nbits-1)
    begin
     Result:=OpUToF(src);
     Result:=OpMulF(Result,1/GetTypeHigh(lc.elem_orig));
    end;

   BUF_NUM_FORMAT_SNORM   :  //Signed, normalized to range [-1.0..1.0]; data/(1<<(nbits-1)-1) clamped
    begin
     Result:=OpSToF(src);
     Result:=OpMulF(Result,1/(GetTypeHigh(lc.elem_orig) shr 1));
     Result:=OpAddF(Result,-1);
    end;

   BUF_NUM_FORMAT_USCALED :  //Unsigned integer to float [0.0 .. (1<<nbits)-1]
    begin
     Result:=OpUToF(src);
    end;

   BUF_NUM_FORMAT_SSCALED :  //Signed integer to float [-(1<<(nbits-1)) ..(1<<(nbits-1))-1]
    begin
     Result:=OpSToF(src);
    end;

   BUF_NUM_FORMAT_SNORM_NZ:  //Signed, normalized to range [-1.0..1.0]; (data*2+1)/(1<<nbits-1)
    begin
     if (lc.info.GetElemSize=4) then
     begin
      Result:=OpSToF(src);
      Result:=OpMulF(Result,2);
      Result:=OpAddF(Result,1);
      Result:=OpMulF(Result,1/GetTypeHigh(lc.elem_orig));
     end else
     begin
      Result:=OpShlI(src,1);
      Result:=OpAddI(Result,1);
      Result:=OpSToF(Result);
      Result:=OpMulF(Result,1/GetTypeHigh(lc.elem_orig));
     end;
    end;

   BUF_NUM_FORMAT_UINT    :
    begin
     Result:=NewReg(lc.elem_resl);
     src^.mark_read;
     _emit_Op1(line,Op.OpUConvert,Result,src);
    end;

   BUF_NUM_FORMAT_SINT    :
    begin
     Result:=NewReg(lc.elem_resl);
     src^.mark_read;
     _emit_Op1(line,Op.OpSConvert,Result,src);
    end;

   BUF_NUM_FORMAT_FLOAT   :
    begin
     Result:=NewReg(lc.elem_resl);
     src^.mark_read;
     _emit_Op1(line,Op.OpFConvert,Result,src);
    end;

  end;
end;

procedure TEmit_vbuf_load._make_load_cv_id(var lc:Tload_cache;i:Byte);
var
 rsl:PsrRegNode;
begin
 rsl:=lc.rsl;
 if (rsl=nil) then
 begin
  rsl:=FetchLoad(lc.v.data[0],GetVecType(lc.elem_orig,lc.elem_count));
  lc.rsl:=rsl;
 end;

 if (lc.elm[i]=nil) then
 begin

  if (lc.elem_count=1) then
  begin
   rsl:=_convert_e(lc,rsl);

   lc.elm[i]:=rsl;
  end else
  begin
   lc.elm[i]:=NewReg(lc.elem_orig);

   rsl^.mark_read;
   emit_OpCompExtract(line,lc.elm[i],rsl,i);

   lc.elm[i]:=_convert_e(lc,lc.elm[i]);
  end;

 end;

 MakeCopy(lc.dst,lc.elm[i]);
end;

procedure TEmit_vbuf_load._make_load_ce_id(var lc:Tload_cache;i:Byte);
var
 orig,elm:PsrChain;
 sum_d:PsrRegNode;
 ext:TsrChainExt;
 rsl:PsrRegNode;
begin

 if (lc.elm[i]=nil) then
 begin
  orig:=lc.v.data[0];
  sum_d:=orig^.key.ext.pIndex;

  if (i=0) then
  begin
   elm:=orig;
  end else
  begin
   sum_d:=TEmit_vbuf_chain(Self).OpAddTo(sum_d,i);

   sum_d^.mark_read;
   ext:=Default(TsrChainExt);
   ext.pIndex:=sum_d;
   ext.stride:=orig^.key.ext.stride;

   elm:=lc.info.grp^.Fetch(0,orig^.key.size,@ext);
  end;

  rsl:=FetchLoad(elm,lc.elem_orig);

  rsl:=_convert_e(lc,rsl);

  lc.elm[i]:=rsl;
 end;

 MakeCopy(lc.dst,lc.elm[i]);
end;

procedure TEmit_vbuf_load._make_load_uv_id(var lc:Tload_cache;i:Byte);
var
 rsl,idx:PsrRegNode;
begin

 rsl:=lc.rsl;
 if (rsl=nil) then
 begin
  rsl:=NewReg(GetVecType(lc.elem_resl,4));
  idx:=lc.v.data[1];

  idx^.mark_read;
  emit_OpImageRead(line,lc.v.data[0],rsl,idx);

  lc.rsl:=rsl;
 end;

 if (lc.elm[i]=nil) then
 begin
  rsl^.mark_read;
  lc.dst^.New(line,lc.elem_resl);
  emit_OpCompExtract(line,lc.dst^.current,rsl,i);

  lc.elm[i]:=lc.dst^.current;
 end else
 begin
  MakeCopy(lc.dst,lc.elm[i]);
 end;
end;

procedure TEmit_vbuf_load._make_load_ue_id(var lc:Tload_cache;i:Byte);
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
   sum_d:=TEmit_vbuf_chain(Self).OpAddTo(idx,i);
  end;

  rsl:=lc.dst^.New(line,lc.elem_resl);

  sum_d^.mark_read;
  emit_OpImageRead(line,lc.v.data[0],rsl,sum_d);

  lc.elm[i]:=rsl;
 end else
 begin
  MakeCopy(lc.dst,lc.elm[i]);
 end;
end;

procedure TEmit_vbuf_load._make_load_zero(var lc:Tload_cache);
begin
 SetConst(lc.dst,FConsts.Fetchi(lc.elem_resl,0));
end;

procedure TEmit_vbuf_load._make_load_one(var lc:Tload_cache);
begin
 if (lc.elem_resl=dtFloat32) then
 begin
  SetConst(lc.dst,FConsts.Fetchf(lc.elem_resl,1));
 end else
 begin
  SetConst(lc.dst,FConsts.Fetchi(lc.elem_resl,1));
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
  lc.dst:=FRegsStory.get_vdst8(FSPI.MUBUF.VDATA+i);
  if (lc.dst=nil) then Assert(false);

  //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
  Case info.dsel[i] of
   0:begin //0
      _make_load_zero(lc);
     end;
   1:begin //1
      _make_load_one(lc);
     end;
   4..7:
     begin //RGBA
      d:=info.dsel[i]-4;

      if (d<lc.elem_count) then
      begin

       Case v.vType of
        vcInvalid       :_make_load_zero(lc);
        vcChainVector   :_make_load_cv_id(lc,d);
        vcChainElement  :_make_load_ce_id(lc,d);
        vcUniformVector :_make_load_uv_id(lc,d);
        vcUniformElement:_make_load_ue_id(lc,d);
       end;

      end else
      begin //as zero
       _make_load_zero(lc);
      end;

     end;
   else
    begin //as zero
     _make_load_zero(lc);
    end;
  end;

 end;

end;

end.

