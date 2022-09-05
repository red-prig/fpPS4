unit emit_vbuf_store;

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
 Tstore_cache=record
  info:TBuf_info;
  v:TvarChain;
  elem_orig:TsrDataType;
  elem_resl:TsrDataType;
  elem_count:ptruint;
  elm:array[0..3] of PsrRegNode;
 end;

 TEmit_vbuf_store=class(TEmitFetch)
  procedure buf_store(info:TBuf_info);
  function  fetch_id(var lc:Tstore_cache;i:Byte):PsrRegNode;
  function  fetch_zero(var lc:Tstore_cache):PsrRegNode;
  function  fetch_one(var lc:Tstore_cache):PsrRegNode;
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
 v:=TEmit_vbuf_chain(TObject(Self)).get_chain(info);

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

function TEmit_vbuf_store.fetch_id(var lc:Tstore_cache;i:Byte):PsrRegNode;
begin
 Result:=fetch_vdst8(FSPI.MUBUF.VDATA+i,lc.elem_resl);
 if (Result=nil) then Assert(false);
end;

function TEmit_vbuf_store.fetch_zero(var lc:Tstore_cache):PsrRegNode;
begin
 Result:=NewReg_q(lc.elem_resl,0);
end;

function TEmit_vbuf_store.fetch_one(var lc:Tstore_cache):PsrRegNode;
begin
 if (lc.elem_resl=dtFloat32) then
 begin
  Result:=NewReg_s(lc.elem_resl,1);
 end else
 begin
  Result:=NewReg_i(lc.elem_resl,1);
 end;
end;

function Min(a,b:PtrUInt):PtrUInt; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

procedure TEmit_vbuf_store.make_store_cv(var lc:Tstore_cache);
var
 rsl:PsrRegNode;
 i:Byte;
 csize:PtrUInt;
 orig,new:PsrChain;
 idx:PsrRegNode;
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

 Case lc.elem_count of
  1:rsl:=lc.elm[0];
  else
   begin
    rsl:=OpMakeVec(line,lc.elem_resl.AsVector(lc.elem_count),@lc.elm);
   end;
 end;

 Assert(lc.elem_resl=lc.elem_orig,'TODO CONVERT');

 csize:=Min(lc.info.GetElemSize*lc.elem_count,lc.info.GetSizeFormat);
 orig:=lc.v.data[0];

 if (orig^.size<>csize) then //refetch
 begin
  idx:=orig^.pIndex;
  if (idx<>nil) then
  begin
   lvl_0.offset:=orig^.offset;
   lvl_0.size  :=csize;

   lvl_1.pIndex:=idx;
   lvl_1.stride:=orig^.stride;

   new:=lc.info.grp^.Fetch(@lvl_0,@lvl_1);
  end else
  begin
   lvl_0.offset:=orig^.offset;
   lvl_0.size  :=csize;

   new:=lc.info.grp^.Fetch(@lvl_0,nil);
  end;
  orig:=new;
 end;

 FetchStore(orig,rsl);
end;

procedure TEmit_vbuf_store.make_store_ce(var lc:Tstore_cache);
var
 orig,elm:PsrChain;
 sum_d:PsrRegNode;
 lvl_0:TsrChainLvl_0;
 lvl_1:TsrChainLvl_1;
 i:Byte;
begin
 orig:=lc.v.data[0];
 sum_d:=orig^.pIndex;

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
    lvl_0.size  :=orig^.size;

    lvl_1.pIndex:=sum_d;
    lvl_1.stride:=orig^.stride;

    elm:=lc.info.grp^.Fetch(@lvl_0,@lvl_1);
   end;

   Assert(lc.elem_resl=lc.elem_orig,'TODO CONVERT');

   FetchStore(elm,lc.elm[i]);

  end;

end;

procedure TEmit_vbuf_store.make_store_uv(var lc:Tstore_cache);
var
 rsl,idx:PsrRegNode;
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

 idx:=lc.v.data[1];

 OpImageWrite(line,lc.v.data[0],idx,rsl);
end;

procedure TEmit_vbuf_store.make_store_ue(var lc:Tstore_cache);
var
 sum_d,idx,rsl:PsrRegNode;
 i:Byte;
begin
 idx:=lc.v.data[1];

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

   OpImageWrite(line,lc.v.data[0],sum_d,rsl);

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

 if info.IsExtFormat then Assert(false,'TODO');

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

