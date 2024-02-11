unit game_run;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 Classes,
 SysUtils,
 kern_thr,
 md_pipe,
 host_ipc,
 md_host_ipc,
 game_info;

type
 TGameRunConfig=record
  hOutput:THandle;
  hError :THandle;

  fork_proc:Boolean;
 end;

 TGameProcess=class
  g_ipc :THostIpcConnect;
  g_fork:Boolean;
 end;

 TGameProcessSimple=class(TGameProcess)
  Ftd:p_kthread;
 end;

 TGameProcessPipe=class(TGameProcess)
  FProcess:THandle;
  FChild:THandle;
 end;

var

 kern_ipc:THostIpcConnect=nil;

function run_item(const cfg:TGameRunConfig;Item:TGameItem):TGameProcess;

implementation

uses
 sys_sysinit,
 kern_param,
 kern_thread,
 kern_exec,
 vfs_mountroot,
 sys_crt, //<- init writeln redirect
 sys_tty,
 md_exception, //<- install custom

 kern_proc,
 md_systm,

 //internal libs
 ps4_libSceSystemService,
 ps4_libSceUserService,
 ps4_libSceIpmi,
 ps4_libSceDialogs,
 ps4_libSceAvSetting
 //internal libs

 ;

var
 runing:Boolean=False;

procedure re_init_tty; register;
var
 i:Integer;
begin
 For i:=0 to High(std_tty) do
 begin
  //std_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  //std_tty[i].t_wr_handle:=t_wr_handle;
  //std_tty[i].t_update   :=@WakeMainThread;
 end;

 For i:=0 to High(deci_tty) do
 begin
  //deci_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  //deci_tty[i].t_wr_handle:=t_wr_handle;
  //deci_tty[i].t_update   :=@WakeMainThread;
 end;

 //debug_tty.t_wr_handle:=t_wr_handle;
 //debug_tty.t_update   :=@WakeMainThread;
end;

procedure prepare(Item:TGameItem); SysV_ABI_CDecl;
var
 td:p_kthread;
 err:Integer;
 len:Integer;
 exec:array[0..PATH_MAX] of Char;
 argv:array[0..1] of PChar;
begin
 //re_init_tty;
 //init_tty:=@re_init_tty;

 //init all
 sys_init;

 PROC_INIT_HOST_IPC(kern_ipc);

 Writeln(Item.FGameInfo.Exec);
 Writeln(Item.FMountList.app0);
 Writeln(Item.FMountList.system);
 Writeln(Item.FMountList.data);



                       //fs  guest     host
 err:=vfs_mount_mkdir('ufs','/app0'  ,pchar(Item.FMountList.app0  ),nil,0);
 Assert(err=0);

 err:=vfs_mount_mkdir('ufs','/system',pchar(Item.FMountList.system),nil,0);
 Assert(err=0);

 err:=vfs_mount_mkdir('ufs','/data'  ,pchar(Item.FMountList.data  ),nil,0);
 Assert(err=0);

 ///argv
 FillChar(exec,SizeOf(exec),0);

 len:=Length(Item.FGameInfo.Exec);
 if (len>PATH_MAX) then len:=PATH_MAX;

 Move(pchar(Item.FGameInfo.Exec)^,exec,len);

 argv[0]:=@exec;
 argv[1]:=nil;
 ///argv

 //unset sys mark
 td:=curkthread;
 td^.td_pflags:=td^.td_pflags and (not TDP_KTHREAD);

 err:=main_execve(argv[0],@argv[0],nil);
 Assert(err=0);
 //

end;

type
 TPCharStream=class(TCustomMemoryStream)
  public
   constructor Create(P:PChar;len:SizeUint); virtual; overload;
   procedure   SetNew(P:PChar;len:SizeUint);
 end;

constructor TPCharStream.Create(P:PChar;len:SizeUint);
begin
 inherited Create;
 SetPointer(P,len);
end;

procedure TPCharStream.SetNew(P:PChar;len:SizeUint);
begin
 SetPosition(0);
 SetPointer(P,len);
end;

procedure fork_process(data:Pointer;size:QWORD); SysV_ABI_CDecl;
var
 td:p_kthread;
 r:Integer;

 pipefd:THandle;
 parent:THandle;

 kipc:THostIpcPipeKERN;

 mem:TPCharStream;
 Item:TGameItem;
begin
 mem:=TPCharStream.Create(data,size);

 mem.Read(pipefd,SizeOf(THandle));

 Item:=TGameItem.Create;
 Item.Deserialize(mem);

 mem.Free;

 //free shared
 md_fork_unshare;

 parent:=md_pidfd_open(md_getppid);

 pipefd:=md_pidfd_getfd(parent,pipefd);

 kipc:=THostIpcPipeKERN.Create;
 kipc.set_pipe(pipefd);

 kern_ipc:=THostIpcConnect(kipc);

 td:=nil;
 r:=kthread_add(@prepare,Item,@td,'[main]');
 Assert(r=0);

 sleep(-1);
end;

function run_item(const cfg:TGameRunConfig;Item:TGameItem):TGameProcess;
var
 r:Integer;

 kern2mgui:array[0..1] of THandle;

 p_mgui_ipc:THostIpcPipeMGUI;

 s_kern_ipc:THostIpcSimpleKERN;
 s_mgui_ipc:THostIpcSimpleMGUI;

 mem:TMemoryStream;
begin
 Result:=nil;
 if Item.FLock then Exit;

 if runing then Exit;

 SetStdHandle(STD_OUTPUT_HANDLE,cfg.hOutput);
 SetStdHandle(STD_ERROR_HANDLE ,cfg.hError );

 if cfg.fork_proc then
 begin
  Result:=TGameProcessPipe.Create;
  Result.g_fork:=cfg.fork_proc;

  with TGameProcessPipe(Result) do
  begin
   md_pipe2(@kern2mgui,MD_PIPE_ASYNC0 or MD_PIPE_ASYNC1);

   p_mgui_ipc:=THostIpcPipeMGUI.Create;
   p_mgui_ipc.set_pipe(kern2mgui[0]);

   g_ipc:=p_mgui_ipc;
   FChild:=kern2mgui[1];
  end;

  //

  mem:=TMemoryStream.Create;

  mem.Write(kern2mgui[1],SizeOf(THandle));

  Item.Serialize(mem);

  with TGameProcessPipe(Result) do
  begin
   FProcess:=md_fork_process(@fork_process,mem.Memory,mem.Size);
  end;

  mem.Free;
 end else
 begin
  Result:=TGameProcessSimple.Create;
  Result.g_fork:=cfg.fork_proc;

  with TGameProcessSimple(Result) do
  begin

   s_kern_ipc:=THostIpcSimpleKERN.Create;
   s_mgui_ipc:=THostIpcSimpleMGUI.Create;

   s_kern_ipc.FDest:=s_mgui_ipc;
   s_mgui_ipc.FDest:=s_kern_ipc;

   g_ipc:=s_mgui_ipc;

   kern_ipc:=s_kern_ipc;

   Ftd:=nil;
   r:=kthread_add(@prepare,Item,@Ftd,'[main]');
  end;

 end;

 runing:=True;
end;



end.



