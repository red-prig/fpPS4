unit emit_SMRD;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ps4_pssl,
  srTypes,
  srReg,
  srLayout,
  SprvEmit,
  emit_op;

type
 TEmit_SMRD=object(TEmitOp)
  procedure _emit_SMRD;
  procedure _emit_LOAD_DWORDX(grp:PsrDataLayout;count:Byte);
  procedure emit_LOAD_DWORDX(count:Byte);
  procedure emit_BUFFER_LOAD_DWORDX(count:Byte);
 end;

implementation

procedure TEmit_SMRD._emit_LOAD_DWORDX(grp:PsrDataLayout;count:Byte);
var
 dst:PsrRegSlot;

 ofs_r,idx_r:PsrRegNode;
 dtype:TsrDataType;

 ext:TsrChainExt;

 i:Byte;

begin

 if (FSPI.SMRD.IMM<>0) then
 begin
  For i:=0 to count-1 do
  begin
   dst:=FRegsStory.get_sdst7(FSPI.SMRD.SDST+i);
   Assert(dst<>nil);
   MakeChain(dst,grp,(FSPI.SMRD.OFFSET+i)*4,4,nil);
  end;
 end else
 begin
  ofs_r:=fetch_ssrc9(FSPI.SMRD.OFFSET,dtUint32);
  dtype:=ofs_r^.dtype;

  idx_r:=NewReg(dtype);

  _emit_OpShr(line,idx_r,ofs_r,FetchReg(FConsts.Fetchi(dtype,2)));

  For i:=0 to count-1 do
  begin
   dst:=FRegsStory.get_sdst7(FSPI.SMRD.SDST+i);
   Assert(dst<>nil);

   if (i=0) then
   begin
    ofs_r:=idx_r;
   end else
   begin
    ofs_r:=NewReg(dtype);
    idx_r^.mark_read;
    _emit_OpIAdd(line,ofs_r,idx_r,FetchReg(FConsts.Fetchi(dtype,i)));
   end;

   ofs_r^.mark_read;

   ext:=Default(TsrChainExt);
   ext.pIndex:=ofs_r;
   ext.stride:=4;
   MakeChain(dst,grp,0,4,@ext);
  end;
 end;

end;

procedure TEmit_SMRD.emit_LOAD_DWORDX(count:Byte);
var
 src:array[0..3] of PsrRegSlot;
 grp:PsrDataLayout;
begin
 if not FRegsStory.get_sbase(FSPI.SMRD.SBASE,2,@src) then Assert(false);
 grp:=GroupingSharp(@src,rtBufPtr2);
 _emit_LOAD_DWORDX(grp,count);
end;

procedure TEmit_SMRD.emit_BUFFER_LOAD_DWORDX(count:Byte);
var
 src:array[0..3] of PsrRegSlot;
 grp:PsrDataLayout;
begin
 if not FRegsStory.get_sbase(FSPI.SMRD.SBASE,4,@src) then Assert(false);
 grp:=GroupingSharp(@src,rtVSharp4);
 _emit_LOAD_DWORDX(grp,count);
end;

procedure TEmit_SMRD._emit_SMRD;
begin

 Case FSPI.SMRD.OP of
  S_BUFFER_LOAD_DWORD:
    begin
     emit_BUFFER_LOAD_DWORDX(1);
    end;
  S_BUFFER_LOAD_DWORDX2:
    begin
     emit_BUFFER_LOAD_DWORDX(2);
    end;
  S_BUFFER_LOAD_DWORDX4:
    begin
     emit_BUFFER_LOAD_DWORDX(4);
    end;
  S_BUFFER_LOAD_DWORDX8:
    begin
     emit_BUFFER_LOAD_DWORDX(8);
    end;
  S_BUFFER_LOAD_DWORDX16:
    begin
     emit_BUFFER_LOAD_DWORDX(16);
    end;

  S_LOAD_DWORD:
    begin
     emit_LOAD_DWORDX(1);
    end;
  S_LOAD_DWORDX2:
    begin
     emit_LOAD_DWORDX(2);
    end;
  S_LOAD_DWORDX4:
    begin
     emit_LOAD_DWORDX(4);
    end;
  S_LOAD_DWORDX8:
    begin
     emit_LOAD_DWORDX(8);
    end;
  S_LOAD_DWORDX16:
    begin
     emit_LOAD_DWORDX(16);
    end;

  else
    Assert(false,'SMRD?'+IntToStr(FSPI.SMRD.OP));
 end;

end;

end.

