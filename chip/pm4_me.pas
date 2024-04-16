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
 GPU_REGS:TGPU_REGS;

 FVSShader:TvShaderExt;
 FPSShader:TvShaderExt;

 FShadersKey:TvShadersKey;
 FShaderGroup:TvShaderGroup;
begin
 GPU_REGS:=Default(TGPU_REGS);
 GPU_REGS.SH_REG:=@node^.SH_REG;
 GPU_REGS.CX_REG:=@node^.CX_REG;

 {fdump_ps:=}DumpPS(GPU_REGS);
 {fdump_vs:=}DumpVS(GPU_REGS);

 FPSShader:=FetchShader(vShaderStagePs,0,GPU_REGS,nil{@pa});
 FVSShader:=FetchShader(vShaderStageVs,1,GPU_REGS,nil{@pa});


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

