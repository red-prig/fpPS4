unit emit_MUBUF;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_shader,
  ps4_pssl,
  srConfig,
  srType,
  srReg,
  srLayout,
  srInput,
  spirv,
  emit_fetch,
  srVBufInfo,
  emit_vbuf_load,
  emit_vbuf_store;

type
 TEmit_MUBUF=class(TEmitFetch)
  procedure emit_MUBUF;
  procedure make_load_zero(dst:PsrRegSlot;dtype:TsrDataType);
  procedure make_load_one(dst:PsrRegSlot;dtype:TsrDataType);
  procedure make_load_comp(dst:PsrRegSlot;dtype:TsrDataType;rsl:PsrRegNode;i:Byte);
  function  emit_BUFFER_LOAD_VA(src:PPsrRegSlot;count:Byte):Boolean;
  procedure emit_BUFFER_LOAD_FORMAT(count:Byte);
  procedure emit_BUFFER_STORE_FORMAT(count:Byte);
  procedure emit_BUFFER_LOAD_DWORDX(count,dfmt:Byte);
  procedure emit_BUFFER_STORE_DWORDX(count,dfmt:Byte);
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

procedure TEmit_MUBUF.make_load_comp(dst:PsrRegSlot;dtype:TsrDataType;rsl:PsrRegNode;i:Byte);
begin
 dst^.New(line,dtype);
 OpExtract(line,dst^.current,rsl,i);
end;

function TEmit_MUBUF.emit_BUFFER_LOAD_VA(src:PPsrRegSlot;count:Byte):Boolean;
var
 dst:PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

 idx:PsrRegNode;
 rsl:PsrRegNode;

 inp_idx:PsrInput;

 info:TBuf_info;
 elem_res:TsrDataType;
 elem_vec:TsrDataType;

 i,d,elem_count:Byte;

begin
 Result:=False;

 idx:=get_vsrc8(FSPI.MUBUF.VADDR+0)^.current;
 inp_idx:=GetInputRegNode(idx);
 if (inp_idx<>nil) then
 if (inp_idx^.itype=itVIndex) then //is input attach
 begin

  grp:=GroupingSharp(src,rtVSharp4);
  PV:=grp^.pData;

  info:=Buf_info(grp,
                 dst_sel(PV^.dst_sel_x,
                         PV^.dst_sel_y,
                         PV^.dst_sel_z,
                         PV^.dst_sel_w),
                 PV^.dfmt,
                 PV^.nfmt,
                 count);

  elem_res:=info.GetResultType;
  elem_vec:=elem_res.AsVector(info.GetElemCount);

  rsl:=AddVertLayout(grp,elem_vec);

  elem_count:=GetElemCount(PV^.dfmt);

  For i:=0 to count-1 do
  begin
   dst:=get_vdst8(FSPI.MUBUF.VDATA+i);
   if (dst=nil) then Assert(false);

   //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
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

 grp:PsrDataLayout;
 PV:PVSharpResource4;
begin
 Assert(FSPI.MUBUF.LDS=0,'FSPI.MUBUF.LDS');

 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

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
 PV:=grp^.pData;

 TEmit_vbuf_load(TObject(Self)).buf_load(
  Buf_info(grp,
           dst_sel(PV^.dst_sel_x,
                   PV^.dst_sel_y,
                   PV^.dst_sel_z,
                   PV^.dst_sel_w),
           PV^.dfmt,
           PV^.nfmt,
           count)
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_STORE_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

begin
 Assert(FSPI.MUBUF.LDS=0,'FSPI.MUBUF.LDS');

 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp^.pData;

 TEmit_vbuf_store(TObject(Self)).buf_store(
  Buf_info(grp,
           dst_sel(PV^.dst_sel_x,
                   PV^.dst_sel_y,
                   PV^.dst_sel_z,
                   PV^.dst_sel_w),
           PV^.dfmt,
           PV^.nfmt,
           count)
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_LOAD_DWORDX(count,dfmt:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

begin
 Assert(FSPI.MUBUF.LDS=0,'FSPI.MUBUF.LDS');

 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp^.pData;

 TEmit_vbuf_load(TObject(Self)).buf_load(
  Buf_info(grp,
           dst_sel_identity,
           dfmt,
           PV^.nfmt,
           count)
 );

end;

procedure TEmit_MUBUF.emit_BUFFER_STORE_DWORDX(count,dfmt:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

begin
 Assert(FSPI.MUBUF.LDS=0,'FSPI.MUBUF.LDS');

 if not get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp^.pData;

 TEmit_vbuf_store(TObject(Self)).buf_store(
  Buf_info(grp,
           dst_sel_identity,
           dfmt,
           PV^.nfmt,
           count)
 );

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

  else
      Assert(false,'MUBUF?'+IntToStr(FSPI.MUBUF.OP));
 end;

end;

end.

