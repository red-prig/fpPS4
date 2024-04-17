unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 LFQueue,

 vBuffer,
 vHostBufferManager,
 vImage,
 vImageManager,
 vRenderPassManager,
 vShader,
 vShaderExt,
 vShaderManager,
 vRegs2Vulkan,

 shader_dump,

 kern_thr,
 md_sleep,
 bittype,
 pm4defs,
 pm4_stream,
 si_ci_vi_merged_offset,
 si_ci_vi_merged_enum,
 si_ci_vi_merged_registers,
 si_ci_vi_merged_groups;

type
 p_pm4_me=^t_pm4_me;
 t_pm4_me=object
  //
  queue:TIntrusiveMPSCQueue; //p_pm4_stream
  //
  started:Pointer;
  td:p_kthread;
  //
  procedure Init;
  procedure start;
  procedure Push(var stream:t_pm4_stream);
  procedure free_stream(node:p_pm4_stream); static;
 end;

implementation

procedure t_pm4_me.Init;
begin
 queue.Create;
end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl; forward;

procedure t_pm4_me.start;
begin
 if (XCHG(started,Pointer(1))=nil) then
 begin
  kthread_add(@pm4_me_thread,@self,@td,(8*1024*1024) div (16*1024),'[GFX_ME]');
 end;
end;

procedure t_pm4_me.Push(var stream:t_pm4_stream);
var
 node:p_pm4_stream;
begin
 if (stream.First=nil) then Exit;
 //self alloc
 node:=stream.allocator.Alloc(SizeOf(t_pm4_stream));
 //
 node^:=stream;
 //
 stream:=Default(t_pm4_stream);
 //
 queue.Push(node);
 //
 start;
end;

procedure t_pm4_me.free_stream(node:p_pm4_stream);
var
 tmp:t_pm4_stream;
begin
 tmp:=node^;
 tmp.Free;
end;

//

procedure pm4_DrawIndex2(node:p_pm4_node_DrawIndex2);
var
 i:Integer;

 GPU_REGS:TGPU_REGS;

 RT_COUNT:Byte;
 RT_INFO:array[0..7] of TRT_INFO;
 DB_INFO:TDB_INFO;

 FVSShader:TvShaderExt;
 FPSShader:TvShaderExt;

 FShadersKey:TvShadersKey;
 FShaderGroup:TvShaderGroup;

 RP_KEY:TvRenderPassKey;
 RP:TvRenderPass2;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SH_REG:=@node^.SH_REG;
 GPU_REGS.CX_REG:=@node^.CX_REG;

 {fdump_ps:=}DumpPS(GPU_REGS);
 {fdump_vs:=}DumpVS(GPU_REGS);

 FPSShader:=FetchShader(vShaderStagePs,0,GPU_REGS,nil{@pa});
 FVSShader:=FetchShader(vShaderStageVs,1,GPU_REGS,nil{@pa});

 FShadersKey:=Default(TvShadersKey);
 FShadersKey.SetVSShader(FVSShader);
 FShadersKey.SetPSShader(FPSShader);

 FShaderGroup:=FetchShaderGroup(@FShadersKey);
 Assert(FShaderGroup<>nil);

 RP_KEY.Clear;

 RT_COUNT:=0;

 if GPU_REGS.COMP_ENABLE then
 For i:=0 to 7 do
 if GPU_REGS.RT_ENABLE(i) then
  begin
   RT_INFO[RT_COUNT]:=GPU_REGS.GET_RT_INFO(i);

   RP_KEY.AddColorRef(RT_COUNT,RT_INFO[RT_COUNT].IMAGE_USAGE);

   RP_KEY.AddColorAt(RT_INFO[RT_COUNT].FImageInfo.cformat,
                     RT_INFO[RT_COUNT].IMAGE_USAGE,
                     RT_INFO[RT_COUNT].FImageInfo.params.samples);

   Inc(RT_COUNT);
  end;

 if GPU_REGS.DB_ENABLE then
 begin
  DB_INFO:=GPU_REGS.GET_DB_INFO;

  RP_KEY.SetDepthRef(RT_COUNT,
                     DB_INFO.DEPTH_USAGE,
                     DB_INFO.STENCIL_USAGE);

  RP_KEY.AddDepthAt(DB_INFO.FImageInfo.cformat,
                    DB_INFO.DEPTH_USAGE,
                    DB_INFO.STENCIL_USAGE);

  RP_KEY.SetZorderStage(DB_INFO.zorder_stage);

 end;

 RP:=FetchRenderPass(nil,@RP_KEY);


end;

procedure pm4_me_thread(me:p_pm4_me); SysV_ABI_CDecl;
var
 stream:p_pm4_stream;
 node:p_pm4_node;
begin

 repeat

  stream:=nil;
  if me^.queue.Pop(stream) then
  begin
   //
   node:=stream^.First;
   while (node<>nil) do
   begin
    Writeln('+',node^.ntype);

    case node^.ntype of
     ntDrawIndex2:pm4_DrawIndex2(Pointer(node));
     else
    end;

    //
    node:=stream^.Next(node);
   end;

   me^.free_stream(stream);
  end;

   //

  msleep_td(100);
 until false;

end;

end.

