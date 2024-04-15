unit pm4_me;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 mqueue,
 LFQueue,

 vBuffer,
 vImage,
 vImageManager,
 vRenderPassManager,

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
  kthread_add(@pm4_me_thread,@self,@td,0,'[GFX_ME]');
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

