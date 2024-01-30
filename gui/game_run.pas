unit game_run;

{$mode ObjFPC}{$H+}

interface

uses
 windows,
 Classes,
 SysUtils,
 game_info;

var
 //FLogUpdate:Integer=0;

 t_wr_handle:THandle;

procedure run_item(Item:TGameItem);

implementation

uses
 sys_sysinit,
 kern_param,
 kern_thr,
 kern_thread,
 kern_exec,
 vfs_mountroot,
 sys_crt, //<- init writeln redirect
 sys_tty,
 md_exception, //<- install custom

 kern_proc,
 host_ipc,
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

procedure WakeMainThread;
begin
 //System.InterlockedIncrement(FLogUpdate);

 if Assigned(Classes.WakeMainThread) then
 begin
  Classes.WakeMainThread(nil);
 end;
end;

procedure re_init_tty; register;
var
 i:Integer;
begin
 For i:=0 to High(std_tty) do
 begin
  //std_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  //std_tty[i].t_wr_handle:=t_wr_handle;
  std_tty[i].t_update   :=@WakeMainThread;
 end;

 For i:=0 to High(deci_tty) do
 begin
  //deci_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  //deci_tty[i].t_wr_handle:=t_wr_handle;
  deci_tty[i].t_update   :=@WakeMainThread;
 end;

 //debug_tty.t_wr_handle:=t_wr_handle;
 debug_tty.t_update   :=@WakeMainThread;
end;

procedure prepare(Item:TGameItem); SysV_ABI_CDecl;
var
 td:p_kthread;
 err:Integer;
 len:Integer;
 exec:array[0..PATH_MAX] of Char;
 argv:array[0..1] of PChar;

 host_ipc:THostIpcConnect;
begin
 //re_init_tty;
 //init_tty:=@re_init_tty;

 //init all
 sys_init;

 Writeln('Hollo World!');

 Writeln(Item.FGameInfo.Exec);

 //host_ipc:=THostIpcConnect(THostIpcSimpleKERN.Create);
 //PROC_INIT_HOST_IPC(host_ipc);

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

 mem:TPCharStream;
 Item:TGameItem;
begin
 mem:=TPCharStream.Create(data,size);

 Item:=TGameItem.Create;
 Item.Deserialize(mem);

 mem.Free;

 //free shared
 md_fork_unshare;

 //sys_init;

 //sleep(-1);

 td:=nil;
 r:=kthread_add(@prepare,Item,@td,'[main]');
 Assert(r=0);

 sleep(-1);
end;

procedure run_item(Item:TGameItem);
var
 td:p_kthread;
 r:Integer;

 mem:TMemoryStream;
begin
 if Item.FLock then Exit;

 if runing then Exit;

 SetStdHandle(STD_ERROR_HANDLE ,t_wr_handle);
 SetStdHandle(STD_OUTPUT_HANDLE,t_wr_handle);

 mem:=TMemoryStream.Create;

 Item.Serialize(mem);

 //mem.Position:=0;

 md_fork_process(@fork_process,mem.Memory,mem.Size);

 mem.Free;

 {
 td:=nil;
 r:=kthread_add(@prepare,Item,@td,'[main]');
 Assert(r=0);
 }

 runing:=True;
end;



end.



