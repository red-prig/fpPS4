unit emit_vbuf_store;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils,
  spirv,
  ps4_pssl,
  srOp,
  srNode,
  srType,
  srReg,
  srLayout,
  emit_fetch,
  srVBufInfo,
  emit_vbuf_chain;

type
 Tstore_cache=record
  info:TBuf_info;
  v:TvarChain;
  elem_orig:TsrDataType;
  elem_resl:TsrDataType;
  elem_count:ptruint;
  elm:array[0..3] of TsrRegNode;
 end;

 TEmit_vbuf_store=class(TEmitFetch)
  procedure buf_store(info:TBuf_info);
  function  fetch_id(var lc:Tstore_cache;i:Byte):TsrRegNode;
  function  fetch_zero(var lc:Tstore_cache):TsrRegNode;
  function  fetch_one(var lc:Tstore_cache):TsrRegNode;
  function  ClampTo(src:TsrRegNode;min_s,max_s:Single):TsrRegNode;
  function  F32ToF10(reg:TsrRegNode):TsrRegNode;
  function  F32ToF11(reg:TsrRegNode):TsrRegNode;
  procedure make_store_cv(var lc:Tstore_cache);
  procedure make_store_ce(var lc:Tstore_cache);
  procedure make_store_uv(var lc:Tstore_cache);
  procedure make_store_ue(var lc:Tstore_cache);
  procedure buf_store_cv(info:TBuf_info;v:TvarChain);
 end;

implementation

procedure TEmit_vbuf_store.buf_store(info:TBuf_info);
var
 v:TvarChain;
begin
 v:=TEmit_vbuf_chain(TObject(Self)).get_chain(info,dtUnknow);

 if (v.vType=vcUniformVector) then
 begin
  //reset dst sel
  info.dsel:=dst_sel_identity;
 end else
 begin
  info.dsel:=get_reverse_dst_sel(info.dsel);
 end;

 buf_store_cv(info,v);
end;

function TEmit_vbuf_store.fetch_id(var lc:Tstore_cache;i:Byte):TsrRegNode;
begin
 Result:=fetch_vdst8(FSPI.MUBUF.VDATA+i,lc.elem_resl);
 if (Result=nil) then Assert(false);
end;

function TEmit_vbuf_store.fetch_zero(var lc:Tstore_cache):TsrRegNode;
begin
 Result:=NewImm_q(lc.elem_resl,0);
end;

function TEmit_vbuf_store.fetch_one(var lc:Tstore_cache):TsrRegNode;
begin
 if (lc.elem_resl=dtFloat32) then
 begin
  Result:=NewImm_s(lc.elem_resl,1);
 end else
 begin
  Result:=NewImm_i(lc.elem_resl,1);
 end;
end;

function Min(a,b:PtrUInt):PtrUInt; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function TEmit_vbuf_store.ClampTo(src:TsrRegNode;min_s,max_s:Single):TsrRegNode;
var
 min,max:TsrRegNode;
begin
 Result:=NewReg(dtFloat32);

 min:=NewImm_s(dtFloat32,min_s);
 max:=NewImm_s(dtFloat32,max_s);

 _OpGlsl3(line,GlslOp.FClamp,Result,src,min,max);
end;

{

uint F32ToF10(float f) {
    uint i = *reinterpret_cast<uint*>(&f);

    uint t1 = i & 0x7fffffff; // Non-sign bits
    uint t3 = i & 0xff800000; // Exponent + sign

    t1 = t1 >> 18; // Align mantissa on MSB

    t1 = t1 - 0xE00; // Adjust bias

    if (t3 < 0x38800000) t1 = 0;     // Flush-to-zero
    if (t3 > 0x47000000) t1 = 0x3FF; // Clamp-to-max

    return t1;
}

uint F32ToF11(float f) {
    uint i = *reinterpret_cast<uint*>(&f);

    uint t1 = i & 0x7fffffff; // Non-sign bits
    uint t3 = i & 0xff800000; // Exponent + sign

    //    [S|E|M]
    //F32 [1|8|23]
    //F16 [1|5|10] -> 13 -> 0x1C000 -> 0x7BFF -> 0x47000000
    //F11 [0|6|5]  -> 18 -> 0x1C00  -> 0x7FF  -> 0x87000000
    //F10 [0|5|5]  -> 18 -> 0xE00   -> 0x3FF  -> 0x47000000

    t1 = t1 >> 18; // Align mantissa on MSB

    t1 = t1 - 0x1C00; // Adjust bias

    if (t3 < 0x38800000) t1 = 0;     // Flush-to-zero
    if (t3 > 0x87000000) t1 = 0x7FF; // Clamp-to-max

    return t1;
}

//10_11_11
//      RR  GG  BB
//high [10][11][11] low

//11_11_10
//      RR  GG  BB
//high [11][11][10] low

}

function TEmit_vbuf_store.F32ToF10(reg:TsrRegNode):TsrRegNode;
var
 i,t1,t3,cond:TsrRegNode;
begin
 i:=BitcastList.FetchRead(dtUint32,reg);

 t1:=OpAndTo(i,$7fffffff); // Non-sign bits
 t3:=OpAndTo(i,$ff800000); // Exponent + sign

 t1.PrepType(ord(dtUint32));
 t3.PrepType(ord(dtInt32 ));

 t1:=OpShrTo(t1,18); // Align mantissa on MSB

 t1:=OpISubTo(t1,$E00); // Adjust bias

 cond:=OpCmpTo(Op.OpSLessThan,t3,NewImm_q(dtUint32,$38800000)); //(t3 < 0x38800000)

 //cond,src_true,src_false
 t1:=OpSelectTo(cond,NewImm_q(dtUint32,0),t1); //if (t3 < 0x38800000) t1 = 0;     // Flush-to-zero

 cond:=OpCmpTo(Op.OpSGreaterThan,t3,NewImm_q(dtUint32,$47000000)); //(t3 > 0x47000000)

 //cond,src_true,src_false
 t1:=OpSelectTo(cond,NewImm_q(dtUint32,$3FF),t1); //if (t3 > 0x47000000) t1 = 0x3FF; // Clamp-to-max

 Result:=t1;
end;

function TEmit_vbuf_store.F32ToF11(reg:TsrRegNode):TsrRegNode;
var
 i,t1,t3,cond:TsrRegNode;
begin
 i:=BitcastList.FetchRead(dtUint32,reg);

 t1:=OpAndTo(i,$7fffffff); // Non-sign bits
 t3:=OpAndTo(i,$ff800000); // Exponent + sign

 t1.PrepType(ord(dtUint32));
 t3.PrepType(ord(dtInt32 ));

 t1:=OpShrTo(t1,18); // Align mantissa on MSB

 t1:=OpISubTo(t1,$1C00); // Adjust bias

 cond:=OpCmpTo(Op.OpSLessThan,t3,NewImm_q(dtUint32,$38800000)); //(t3 < 0x38800000)

 //cond,src_true,src_false
 t1:=OpSelectTo(cond,NewImm_q(dtUint32,0),t1); //if (t3 < 0x38800000) t1 = 0;     // Flush-to-zero

 cond:=OpCmpTo(Op.OpSGreaterThan,t3,NewImm_q(dtUint32,$87000000)); //(t3 > 0x87000000)

 //cond,src_true,src_false
 t1:=OpSelectTo(cond,NewImm_q(dtUint32,$7FF),t1); //if (t3 > 0x47000000) t1 = 0x7FF; // Clamp-to-max

 Result:=t1;
end;

procedure TEmit_vbuf_store.make_store_cv(var lc:Tstore_cache);
var
 rsl:TsrRegNode;
 i:Byte;
 csize:PtrUInt;
 orig,mnew:TsrChain;
 idx:TsrRegNode;
 lvl_0:TsrChainLvl_0;
 lvl_1:TsrChainLvl_1;
begin

 For i:=0 to lc.elem_count-1 do //fill
  if (lc.elm[i]=nil) then
  begin
   Case lc.info.dsel[i] of
    1:lc.elm[i]:=fetch_one(lc);
    else
      lc.elm[i]:=fetch_zero(lc);
   end;
  end;

 //special types
 Case lc.info.DFMT of
  BUF_DATA_FORMAT_10_11_11  ,
  BUF_DATA_FORMAT_11_11_10  ,
  BUF_DATA_FORMAT_10_10_10_2,
  BUF_DATA_FORMAT_2_10_10_10:
    begin
     lc.elem_orig :=dtUint32;
     lc.elem_count:=1;
    end;
  else;
 end;

 if (lc.elem_resl<>lc.elem_orig) then
 begin

  Case lc.info.DFMT of
   BUF_DATA_FORMAT_10_11_11:
     begin
      //10_11_11
      //      RR  GG  BB
      //high [10][11][11] low

      lc.elm[0]:=F32ToF10(lc.elm[0]); //R
      lc.elm[1]:=F32ToF11(lc.elm[1]); //G
      lc.elm[2]:=F32ToF11(lc.elm[2]); //B

      lc.elm[0]:=OpShlTo(lc.elm[0],11+11);
      lc.elm[1]:=OpShlTo(lc.elm[1],11);

      lc.elm[2]:=OpOrTo(lc.elm[2],lc.elm[1]); //G|B

      lc.elm[0]:=OpOrTo(lc.elm[2],lc.elm[0]); //R|G|B
      lc.elm[1]:=nil;
      lc.elm[2]:=nil;
     end;
   BUF_DATA_FORMAT_11_11_10:
     begin
      //11_11_10
      //      RR  GG  BB
      //high [11][11][10] low

      lc.elm[0]:=F32ToF11(lc.elm[0]); //R
      lc.elm[1]:=F32ToF11(lc.elm[1]); //G
      lc.elm[2]:=F32ToF10(lc.elm[2]); //B

      lc.elm[0]:=OpShlTo(lc.elm[0],11+10);
      lc.elm[1]:=OpShlTo(lc.elm[1],10);

      lc.elm[2]:=OpOrTo(lc.elm[2],lc.elm[1]); //G|B

      lc.elm[0]:=OpOrTo(lc.elm[2],lc.elm[0]); //R|G|B
      lc.elm[1]:=nil;
      lc.elm[2]:=nil;
     end;
   else

     case lc.elem_resl of
      dtFloat32: //isScalar
        begin

         Case lc.info.NFMT of
          BUF_NUM_FORMAT_FLOAT:
            begin
             //float->float
             For i:=0 to lc.elem_count-1 do
             begin
              lc.elm[i]:=OpFToF(lc.elm[i],lc.elem_orig);
             end;
            end;
          BUF_NUM_FORMAT_UNORM:
            begin
             //float->byte
             For i:=0 to lc.elem_count-1 do
             begin
              lc.elm[i]:=OpFMulToS(lc.elm[i],lc.elem_orig.High);
              lc.elm[i]:=ClampTo  (lc.elm[i],0,lc.elem_orig.High);
              lc.elm[i]:=OpFToU   (lc.elm[i],lc.elem_orig);
             end;
            end;
          else
           Assert(false,'TODO CONVERT:Float32->'+IntToStr(lc.info.NFMT));
         end;

        end;
      dtUint32:
        begin

         Case lc.info.NFMT of
          BUF_NUM_FORMAT_UINT:
            begin
             //uint->uint
             For i:=0 to lc.elem_count-1 do
             begin
              lc.elm[i]:=OpUToU(lc.elm[i],lc.elem_orig);
             end;
            end;
          else
           Assert(false,'TODO CONVERT:Uint32->'+IntToStr(lc.info.NFMT));
         end;

        end;

      dtInt32:
        begin

         Case lc.info.NFMT of
          BUF_NUM_FORMAT_SINT:
            begin
             //int->int
             For i:=0 to lc.elem_count-1 do
             begin
              lc.elm[i]:=OpSToS(lc.elm[i],lc.elem_orig);
             end;
            end;
          else
           Assert(false,'TODO CONVERT:Int32->'+IntToStr(lc.info.NFMT));
         end;

        end;
      else
       Assert(False);
     end;

  end; //Case lc.info.DFMT of

 end; //if (lc.elem_resl<>lc.elem_orig) then

 Case lc.elem_count of
  1:rsl:=lc.elm[0];
  else
   begin
    rsl:=OpMakeVec(line,lc.elem_orig.AsVector(lc.elem_count),@lc.elm);
   end;
 end;

 csize:=Min(lc.info.GetElemSize*lc.elem_count,lc.info.GetSizeFormat);
 orig:=TsrChain(lc.v.data[0]);

 if (orig.size<>csize) then //refetch
 begin
  idx:=orig.pIndex;
  if (idx<>nil) then
  begin
   lvl_0.offset:=orig.offset;
   lvl_0.size  :=csize;

   lvl_1.pIndex:=idx;
   lvl_1.stride:=orig.stride;

   mnew:=lc.info.grp.Fetch(line.Parent,@lvl_0,@lvl_1,cflags(dtUnknow,lc.info.GLC,lc.info.SLC));
  end else
  begin
   lvl_0.offset:=orig.offset;
   lvl_0.size  :=csize;

   mnew:=lc.info.grp.Fetch(line.Parent,@lvl_0,nil,cflags(dtUnknow,lc.info.GLC,lc.info.SLC));
  end;
  orig:=mnew;
 end;

 FetchStore(orig,rsl);
end;

procedure TEmit_vbuf_store.make_store_ce(var lc:Tstore_cache);
var
 orig,elm:TsrChain;
 sum_d:TsrRegNode;
 lvl_0:TsrChainLvl_0;
 lvl_1:TsrChainLvl_1;
 i:Byte;
begin
 orig:=TsrChain(lc.v.data[0]);
 sum_d:=orig.pIndex;

 For i:=0 to lc.elem_count-1 do
  if (lc.elm[i]<>nil) then
  begin

   if (i=0) then
   begin
    elm:=orig;
   end else
   begin
    sum_d:=OpIAddTo(sum_d,i);

    lvl_0.offset:=0;
    lvl_0.size  :=orig.size;

    lvl_1.pIndex:=sum_d;
    lvl_1.stride:=orig.stride;

    elm:=lc.info.grp.Fetch(line.Parent,@lvl_0,@lvl_1,cflags(dtUnknow,lc.info.GLC,lc.info.SLC));
   end;

   Assert(lc.elem_resl=lc.elem_orig,'TODO CONVERT:make_store_ce');

   FetchStore(elm,lc.elm[i]);

  end;

end;

procedure TEmit_vbuf_store.make_store_uv(var lc:Tstore_cache);
var
 rsl,idx:TsrRegNode;
 i:Byte;
begin

 For i:=0 to lc.elem_count-1 do //fill
  if (lc.elm[i]=nil) then
  begin
   Case lc.info.dsel[i] of
    1:lc.elm[i]:=fetch_one(lc);
    else
      lc.elm[i]:=fetch_zero(lc);
   end;
  end;

 Case lc.elem_count of
  1:rsl:=lc.elm[0];
  else
   begin
    rsl:=OpMakeVec(line,lc.elem_resl.AsVector(lc.elem_count),@lc.elm);
   end;
 end;

 idx:=TsrRegNode(lc.v.data[1]);

 OpImageWrite(line,TsrNode(lc.v.data[0]),idx,rsl);
end;

procedure TEmit_vbuf_store.make_store_ue(var lc:Tstore_cache);
var
 sum_d,idx,rsl:TsrRegNode;
 i:Byte;
begin
 idx:=TsrRegNode(lc.v.data[1]);

 For i:=0 to lc.elem_count-1 do
  if (lc.elm[i]<>nil) then
  begin
   rsl:=lc.elm[i];

   if (i=0) then
   begin
    sum_d:=idx;
   end else
   begin
    sum_d:=OpIAddTo(idx,i);
   end;

   OpImageWrite(line,TsrNode(lc.v.data[0]),sum_d,rsl);

  end;
end;

procedure TEmit_vbuf_store.buf_store_cv(info:TBuf_info;v:TvarChain);
var
 lc:Tstore_cache;

 i:Byte;
begin

 Case v.vType of
  vcInvalid:Exit;
  else;
 end;

 Case info.DFMT of
  BUF_DATA_FORMAT_10_10_10_2:Assert(false,'TODO: STORE:10_10_10_2');
  BUF_DATA_FORMAT_2_10_10_10:Assert(false,'TODO: STORE:2_10_10_10');
  else;
 end;

 lc:=Default(Tstore_cache);
 lc.info      :=info;
 lc.v         :=v;
 lc.elem_resl :=info.GetResultType;
 lc.elem_orig :=info.GetElemType;
 lc.elem_count:=info.GetElemCount;

 For i:=0 to lc.elem_count-1 do
 begin
  lc.elm[i]:=nil;
  Case lc.info.dsel[i] of
   4..7:
     begin //RGBA
      lc.elm[i]:=fetch_id(lc,lc.info.dsel[i]-4);
     end;
   else;
  end;
 end;

 While (lc.elem_count<>0) do //trim count
 begin
  i:=lc.elem_count-1;
  if (lc.elm[i]<>nil) then Break;
  Dec(lc.elem_count);
 end;

 if (lc.elem_count=0) then Exit;

 Case v.vType of
  vcChainVector   :make_store_cv(lc);
  vcChainElement  :make_store_ce(lc);
  vcUniformVector :make_store_uv(lc);
  vcUniformElement:make_store_ue(lc)
  else;
 end;

end;

end.

