unit emit_MUBUF;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_shader,
  ps4_pssl,
  srCFGCursor,
  srConfig,
  srType,
  srOp,
  srReg,
  srLayout,
  srInput,
  spirv,
  emit_fetch,
  srVBufInfo,
  emit_vbuf_chain,
  emit_vbuf_load,
  emit_vbuf_store;

type
 TEmit_MUBUF=class(TEmitFetch)
  procedure emit_MUBUF;
  procedure make_load_zero(dst:PsrRegSlot;dtype:TsrDataType);
  procedure make_load_one(dst:PsrRegSlot;dtype:TsrDataType);
  procedure make_load_comp(dst:PsrRegSlot;dtype:TsrDataType;rsl:TsrRegNode;i:Byte);
  function  emit_BUFFER_LOAD_VA(src:PPsrRegSlot;count:Byte):Boolean;
  procedure emit_BUFFER_LOAD_FORMAT (count:Byte);
  procedure emit_BUFFER_STORE_FORMAT(count:Byte);
  procedure emit_BUFFER_LOAD_DWORDX (count,dfmt:Byte);
  procedure emit_BUFFER_STORE_DWORDX(count,dfmt:Byte);
  procedure buf_atomic(info:TBuf_info;OpId:DWORD);
  procedure emit_BUFFER_ATOMIC(count,dfmt,nfmt:Byte;OpId:DWORD);
  procedure emit_BUFFER_ATOMIC_FMINMAX(count:Byte;OpId:DWORD);
 end;

implementation

procedure TEmit_MUBUF.make_load_zero(dst:PsrRegSlot;dtype:TsrDataType);
begin
 SetConst_q(dst,dtype,0);
end;

procedure TEmit_MUBUF.make_load_one(dst:PsrRegSlot;dtype:TsrDataType);
begin
 if (dtype=dtFloat32) then
 begin
  SetConst_s(dst,dtype,1);
 end else
 begin
  SetConst_i(dst,dtype,1);
 end;
end;

procedure TEmit_MUBUF.make_load_comp(dst:PsrRegSlot;dtype:TsrDataType;rsl:TsrRegNode;i:Byte);
begin
 if rsl.dtype.isVector then
 begin
  dst^.New(dtype);
  OpExtract(line,dst^.current,rsl,i);
 end else
 begin
  Assert(i=0);
  MakeCopy(dst,rsl);
 end;
end;

function TEmit_MUBUF.emit_BUFFER_LOAD_VA(src:PPsrRegSlot;count:Byte):Boolean;
var
 dst:PsrRegSlot;

 grp:TsrDataLayout;
 PV:PVSharpResource4;

 idx:TsrRegNode;
 rsl:TsrRegNode;

 inp_idx:TsrInput;

 info:TBuf_info;
 elem_res:TsrDataType;
 elem_vec:TsrDataType;

 i,d,elem_count:Byte;

begin
 Result:=False;

 idx:=get_vsrc8(FSPI.MUBUF.VADDR+0)^.current;
 inp_idx:=GetInputRegNode(idx);
 if (inp_idx<>nil) then
 if (inp_idx.itype=itVIndex) then //is input attach
 begin

  grp:=GroupingSharp(src,rtVSharp4);
  PV:=grp.GetSharp;

  //
  grp.RINF:=True;
  //

  info:=Buf_info(grp,
                 dst_sel(PV^.dst_sel_x,
                         PV^.dst_sel_y,
                         PV^.dst_sel_z,
                         PV^.dst_sel_w),
                 PV^.dfmt,
                 PV^.nfmt,
                 count,
                 FSPI.MUBUF.GLC,
                 FSPI.MUBUF.SLC,
                 PV^.num_records);

  elem_res:=info.GetResultType;
  elem_vec:=elem_res.AsVector(info.GetElemCount);

  rsl:=AddVertLayout(grp,elem_vec);

  elem_count:=info.GetElemCount;

  For i:=0 to count-1 do
  begin
   dst:=get_vdst8(FSPI.MUBUF.VDATA+i);
   if (dst=nil) then Assert(false);

   //0=0, 1=1, 2=0, 3=0, 4=R, 5=G, 6=B, 7=A
   Case info.dsel[i] of
    0:begin //0
       make_load_zero(dst,elem_res);
      end;
    1:begin //1
       make_load_one(dst,elem_res);
      end;
    4..7:
      begin //RGBA
       d:=info.dsel[i]-4;

       if (d<elem_count) then
       begin
        make_load_comp(dst,elem_res,rsl,d);
       end else
       begin //as zero
        make_load_zero(dst,elem_res);
       end;

      end;
    else
     begin //as zero
      make_load_zero(dst,elem_res);
     end;
   end;

  end;

  Result:=True;
 end;

end;

procedure TEmit_MUBUF.emit_BUFFER_LOAD_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;
 PV:PVSharpResource4;
begin
 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 if (FSPI.MUBUF.LDS<>0) then
 begin
  //TODO: FSPI.MUBUF.LDS
  Assert(false,'TODO: FSPI.MUBUF.LDS');
 end;

 if Config.UseVertexInput then
 if (FExecutionModel=ExecutionModel.Vertex) then //Vertex only
 if (FSPI.MUBUF.IDXEN=1) and
    (FSPI.MUBUF.OFFEN=0) and
    (FSPI.MUBUF.SOFFSET=128) and
    (FSPI.MUBUF.LDS=0)   then
 begin
  if emit_BUFFER_LOAD_VA(@src,count) then Exit;
 end;

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp.GetSharp;

 //
 grp.RINF:=True;
 //

 TEmit_vbuf_load(TObject(Self)).buf_load(
  Buf_info(grp,
           dst_sel(PV^.dst_sel_x,
                   PV^.dst_sel_y,
                   PV^.dst_sel_z,
                   PV^.dst_sel_w),
           PV^.dfmt,
           PV^.nfmt,
           count,
           FSPI.MUBUF.GLC,
           FSPI.MUBUF.SLC,
           PV^.num_records)
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_STORE_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;
 PV:PVSharpResource4;

begin
 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 if (FSPI.MUBUF.LDS<>0) then
 begin
  //TODO: FSPI.MUBUF.LDS
  Assert(false,'TODO: FSPI.MUBUF.LDS');
 end;

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp.GetSharp;

 //
 grp.RINF:=True;
 //

 TEmit_vbuf_store(TObject(Self)).buf_store(
  Buf_info(grp,
           dst_sel(PV^.dst_sel_x,
                   PV^.dst_sel_y,
                   PV^.dst_sel_z,
                   PV^.dst_sel_w),
           PV^.dfmt,
           PV^.nfmt,
           count,
           FSPI.MUBUF.GLC,
           FSPI.MUBUF.SLC,
           PV^.num_records)
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_LOAD_DWORDX(count,dfmt:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;
 PV:PVSharpResource4;

begin
 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 if (FSPI.MUBUF.LDS<>0) then
 begin
  //TODO: FSPI.MUBUF.LDS
  Assert(false,'TODO: FSPI.MUBUF.LDS');
 end;

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp.GetSharp;

 TEmit_vbuf_load(TObject(Self)).buf_load(
  Buf_info(grp,
           dst_sel_identity,
           dfmt,
           //PV^.nfmt,
           BUF_NUM_FORMAT_FLOAT,
           count,
           FSPI.MUBUF.GLC,
           FSPI.MUBUF.SLC,
           PV^.num_records)
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_STORE_DWORDX(count,dfmt:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;
 PV:PVSharpResource4;

begin
 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 if (FSPI.MUBUF.LDS<>0) then
 begin
  //TODO: FSPI.MUBUF.LDS
  Assert(false,'TODO: FSPI.MUBUF.LDS');
 end;

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp.GetSharp;

 TEmit_vbuf_store(TObject(Self)).buf_store(
  Buf_info(grp,
           dst_sel_identity,
           dfmt,
           //PV^.nfmt,
           BUF_NUM_FORMAT_FLOAT,
           count,
           FSPI.MUBUF.GLC,
           FSPI.MUBUF.SLC,
           PV^.num_records)
 );

end;

procedure TEmit_MUBUF.buf_atomic(info:TBuf_info;OpId:DWORD);
var
 v:TvarChain;
 rtype:TsrDataType;
 vsrc:TsrRegNode;
 pChain:TsrChain;
 vdst:TsrRegNode;

 dst:array[0..1] of PsrRegSlot;
begin

 if (info.count=2) then
 begin

  case info.NFMT of
   BUF_NUM_FORMAT_UINT :rtype:=dtUint64;
   BUF_NUM_FORMAT_SINT :rtype:=dtInt64;
   BUF_NUM_FORMAT_FLOAT:rtype:=dtFloat64;
   else
    Assert(False);
  end;

 end else
 begin

  case info.NFMT of
   BUF_NUM_FORMAT_UINT :rtype:=dtUint32;
   BUF_NUM_FORMAT_SINT :rtype:=dtInt32;
   BUF_NUM_FORMAT_FLOAT:rtype:=dtFloat32;
   else
    Assert(False);
  end;

 end;

 v:=TEmit_vbuf_chain(TObject(Self)).get_chain(info,rtype);
 Assert(v.vType in [vcChainVector,vcChainElement]);

 pChain:=TsrChain(v.data[0]);

 if (info.count=2) then
 begin
  vsrc:=fetch_vdst8_64(FSPI.MUBUF.VDATA,dtUint64);
 end else
 begin
  vsrc:=fetch_vdst8(FSPI.MUBUF.VDATA,rtype);
 end;

 vdst:=FetchAtomic(pChain,OpId,rtype,vsrc);

 if (info.GLC<>0) then
 begin
  //save result
  if (info.count=2) then
  begin
   dst[0]:=get_vdst8(FSPI.MUBUF.VDATA+0);
   dst[1]:=get_vdst8(FSPI.MUBUF.VDATA+1);

   MakeCopy64(dst[0],dst[1],vdst);
  end else
  begin
   dst[0]:=get_vdst8(FSPI.MUBUF.VDATA);

   MakeCopy(dst[0],vdst);
  end;
 end else
 begin
  //no result
  vdst.mark_read(nil); //self link
 end;

end;

procedure TEmit_MUBUF.emit_BUFFER_ATOMIC(count,dfmt,nfmt:Byte;OpId:DWORD);
var
 src:array[0..3] of PsrRegSlot;

 grp:TsrDataLayout;
 PV:PVSharpResource4;

begin
 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 if (FSPI.MUBUF.LDS<>0) then
 begin
  //TODO: FSPI.MUBUF.LDS
  Assert(false,'TODO: FSPI.MUBUF.LDS');
 end;

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp.GetSharp;

 buf_atomic(
  Buf_info(grp,
           dst_sel_identity,
           dfmt,
           nfmt,
           count,
           FSPI.MUBUF.GLC,
           FSPI.MUBUF.SLC,
           PV^.num_records),
           OpId
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_ATOMIC_FMINMAX(count:Byte;OpId:DWORD);
Var
 vsrc:TsrRegNode;

 pOpMerge:TsrOpBlock;
begin
 if Config.UseAtomicFloatMinMax then
 begin
  emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_FLOAT,OpId);
  Exit;
 end;

 if (count=2) then
 begin
  vsrc:=fetch_vdst8_64(FSPI.MUBUF.VDATA,dtInt64);
 end else
 begin
  vsrc:=fetch_vdst8(FSPI.MUBUF.VDATA,dtInt32);
 end;

 vsrc:=OpIsSSignTo(vsrc);

 pOpMerge:=NewMerge(Default(TsrCursor)); //open merge

 NewIf(pOpMerge,Default(TsrCursor),vsrc,True); //open if

  case OpId of
   Op.OpAtomicFMinEXT:
     begin
      emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_UINT,Op.OpAtomicUMax);
     end;
   Op.OpAtomicFMaxEXT:
     begin
      emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_UINT,Op.OpAtomicUMin);
     end;
  end;

 PopBlockOp; //close other
 PopBlockOp; //close if
 NewElse(pOpMerge,Default(TsrCursor)); //open else

  case OpId of
   Op.OpAtomicFMinEXT:
     begin
      emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_SINT,Op.OpAtomicSMin);
     end;
   Op.OpAtomicFMaxEXT:
     begin
      emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_SINT,Op.OpAtomicSMax);
     end;
  end;

 PopBlockOp; //close other
 PopBlockOp; //close else
 PopBlockOp; //close merge
end;

procedure TEmit_MUBUF.emit_MUBUF;
begin
 case FSPI.MUBUF.OP of
  BUFFER_LOAD_FORMAT_X    : emit_BUFFER_LOAD_FORMAT(1);
  BUFFER_LOAD_FORMAT_XY   : emit_BUFFER_LOAD_FORMAT(2);
  BUFFER_LOAD_FORMAT_XYZ  : emit_BUFFER_LOAD_FORMAT(3);
  BUFFER_LOAD_FORMAT_XYZW : emit_BUFFER_LOAD_FORMAT(4);

  BUFFER_STORE_FORMAT_X   : emit_BUFFER_STORE_FORMAT(1);
  BUFFER_STORE_FORMAT_XY  : emit_BUFFER_STORE_FORMAT(2);
  BUFFER_STORE_FORMAT_XYZ : emit_BUFFER_STORE_FORMAT(3);
  BUFFER_STORE_FORMAT_XYZW: emit_BUFFER_STORE_FORMAT(4);

  BUFFER_LOAD_DWORD       : emit_BUFFER_LOAD_DWORDX(1,BUF_DATA_FORMAT_32);
  BUFFER_LOAD_DWORDX2     : emit_BUFFER_LOAD_DWORDX(2,BUF_DATA_FORMAT_32_32);
  BUFFER_LOAD_DWORDX3     : emit_BUFFER_LOAD_DWORDX(3,BUF_DATA_FORMAT_32_32_32);
  BUFFER_LOAD_DWORDX4     : emit_BUFFER_LOAD_DWORDX(4,BUF_DATA_FORMAT_32_32_32_32);

  BUFFER_STORE_DWORD      : emit_BUFFER_STORE_DWORDX(1,BUF_DATA_FORMAT_32);
  BUFFER_STORE_DWORDX2    : emit_BUFFER_STORE_DWORDX(2,BUF_DATA_FORMAT_32_32);
  BUFFER_STORE_DWORDX3    : emit_BUFFER_STORE_DWORDX(3,BUF_DATA_FORMAT_32_32_32);
  BUFFER_STORE_DWORDX4    : emit_BUFFER_STORE_DWORDX(4,BUF_DATA_FORMAT_32_32_32_32);

  BUFFER_ATOMIC_ADD       : emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_UINT,Op.OpAtomicIAdd);
  BUFFER_ATOMIC_SWAP      : emit_BUFFER_ATOMIC(1,BUF_DATA_FORMAT_32,BUF_NUM_FORMAT_UINT,Op.OpAtomicExchange);

  BUFFER_ATOMIC_FMIN      : emit_BUFFER_ATOMIC_FMINMAX(1,Op.OpAtomicFMinEXT);
  BUFFER_ATOMIC_FMAX      : emit_BUFFER_ATOMIC_FMINMAX(1,Op.OpAtomicFMaxEXT);

  else
      Assert(false,'MUBUF?'+IntToStr(FSPI.MUBUF.OP)+' '+get_str_spi(FSPI));
 end;

end;

end.

