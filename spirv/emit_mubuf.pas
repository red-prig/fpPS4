unit emit_MUBUF;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_shader,
  ps4_pssl,
  srTypes,
  srReg,
  srLayout,
  srInput,
  spirv,
  SprvEmit,
  srVBufInfo,
  emit_vbuf_load,
  emit_vbuf_store,
  emit_op;

type
 TEmit_MUBUF=object(TEmitOp)
  procedure _emit_MUBUF;
  procedure _make_load_zero(dst:PsrRegSlot);
  procedure _make_load_one(dst:PsrRegSlot);
  procedure _make_load_comp(dst:PsrRegSlot;rsl:PsrRegNode;i:Byte);
  function  _emit_BUFFER_LOAD_VA(src:PPsrRegSlot;count:Byte):Boolean;
  procedure _emit_BUFFER_LOAD_FORMAT(count:Byte);
  procedure _emit_BUFFER_STORE_FORMAT(count:Byte);
 end;

implementation

procedure TEmit_MUBUF._make_load_zero(dst:PsrRegSlot);
begin
 SetConst(dst,FConsts.Fetchi(dtFloat32,0));
end;

procedure TEmit_MUBUF._make_load_one(dst:PsrRegSlot);
begin
 SetConst(dst,FConsts.Fetchf(dtFloat32,1));
end;

procedure TEmit_MUBUF._make_load_comp(dst:PsrRegSlot;rsl:PsrRegNode;i:Byte);
begin
 dst^.New(line,dtFloat32);
 rsl^.mark_read;
 emit_OpCompExtract(line,dst^.current,rsl,i);
end;

function TEmit_MUBUF._emit_BUFFER_LOAD_VA(src:PPsrRegSlot;count:Byte):Boolean;
var
 dst:PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

 idx:PsrRegNode;
 rsl:PsrRegNode;

 inp_idx:PsrInput;

 dsel:Tdst_sel;

 i,d,elem_count:Byte;

begin
 Result:=False;

 idx:=FRegsStory.get_vsrc8(FSPI.MUBUF.VADDR+0)^.current;
 inp_idx:=GetInputRegNode(idx);
 if (inp_idx<>nil) then
 if (inp_idx^.key.itype=itVIndex) then //is input attach
 begin

  grp:=GroupingSharp(src,rtVSharp4);
  PV:=grp^.pData;
  rsl:=AddVertLayout(grp,dtVec4f);

  dsel:=dst_sel(PV^.dst_sel_x,
                PV^.dst_sel_y,
                PV^.dst_sel_z,
                PV^.dst_sel_w);

  elem_count:=GetElemCount(PV^.dfmt);

  For i:=0 to count-1 do
  begin
   dst:=FRegsStory.get_vdst8(FSPI.MUBUF.VDATA+i);
   if (dst=nil) then Assert(false);

   //0=0, 1=1, 4=R, 5=G, 6=B, 7=A
   Case dsel[i] of
    0:begin //0
       _make_load_zero(dst);
      end;
    1:begin //1
       _make_load_one(dst);
      end;
    4..7:
      begin //RGBA
       d:=dsel[i]-4;

       if (d<elem_count) then
       begin
        _make_load_comp(dst,rsl,d);
       end else
       begin //as zero
        _make_load_zero(dst);
       end;

      end;
    else
     begin //as zero
      _make_load_zero(dst);
     end;
   end;

  end;

  Result:=True;
 end;

end;

procedure TEmit_MUBUF._emit_BUFFER_LOAD_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

begin
 Assert(FSPI.MUBUF.LDS=0,'FSPI.MUBUF.LDS');

 if not FRegsStory.get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 if FUseVertexInput then
 if (FExecutionModel=ExecutionModel.Vertex) then //Vertex only
 if (FSPI.MUBUF.IDXEN=1) and
    (FSPI.MUBUF.OFFEN=0) and
    (FSPI.MUBUF.SOFFSET=128) and
    (FSPI.MUBUF.LDS=0)   then
 begin
  if _emit_BUFFER_LOAD_VA(@src,count) then Exit;
 end;

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp^.pData;

 TEmit_vbuf_load(Self).buf_load(
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

procedure TEmit_MUBUF._emit_BUFFER_STORE_FORMAT(count:Byte);
var
 src:array[0..3] of PsrRegSlot;

 grp:PsrDataLayout;
 PV:PVSharpResource4;

begin
 Assert(FSPI.MUBUF.LDS=0,'FSPI.MUBUF.LDS');

 if not FRegsStory.get_srsrc(FSPI.MUBUF.SRSRC,4,@src) then Assert(false);

 grp:=GroupingSharp(@src,rtVSharp4);
 PV:=grp^.pData;

 TEmit_vbuf_store(Self).buf_store(
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

procedure TEmit_MUBUF._emit_MUBUF;
begin
 case FSPI.MUBUF.OP of
  BUFFER_LOAD_FORMAT_X:
    begin
     _emit_BUFFER_LOAD_FORMAT(1);
    end;
  BUFFER_LOAD_FORMAT_XY:
    begin
     _emit_BUFFER_LOAD_FORMAT(2);
    end;
  BUFFER_LOAD_FORMAT_XYZ:
    begin
     _emit_BUFFER_LOAD_FORMAT(3);
    end;
  BUFFER_LOAD_FORMAT_XYZW:
    begin
     _emit_BUFFER_LOAD_FORMAT(4);
    end;

  BUFFER_STORE_FORMAT_X:
    begin
     _emit_BUFFER_STORE_FORMAT(1);
    end;
  BUFFER_STORE_FORMAT_XY:
    begin
     _emit_BUFFER_STORE_FORMAT(2);
    end;
  BUFFER_STORE_FORMAT_XYZ:
    begin
     _emit_BUFFER_STORE_FORMAT(3);
    end;
  BUFFER_STORE_FORMAT_XYZW:
    begin
     _emit_BUFFER_STORE_FORMAT(4);
    end;

  else
      Assert(false,'MUBUF?'+IntToStr(FSPI.MUBUF.OP));
 end;

end;

end.

