unit game_run;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 SysUtils,
 game_info;

var
 FLogUpdate:Integer=0;

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

 //internal libs
 ps4_libSceSystemService,
 ps4_libSceIpmi,
 ps4_libSceDialogs,
 ps4_libSceAvSetting
 //internal libs

 ;

var
 runing:Boolean=False;

procedure WakeMainThread;
begin
 System.InterlockedIncrement(FLogUpdate);

 if Assigned(Classes.WakeMainThread) then
 begin
  Classes.WakeMainThread(nil);
 end;
end;

procedure re_init_tty;
var
 i:Integer;
begin
 For i:=0 to High(std_tty) do
 begin
  //std_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  std_tty[i].t_wr_handle:=t_wr_handle;
  std_tty[i].t_update   :=@WakeMainThread;
 end;

 For i:=0 to High(deci_tty) do
 begin
  //deci_tty[i].t_rd_handle:=GetStdHandle(STD_INPUT_HANDLE);
  deci_tty[i].t_wr_handle:=t_wr_handle;
  deci_tty[i].t_update   :=@WakeMainThread;
 end;

 debug_tty.t_wr_handle:=t_wr_handle;
 debug_tty.t_update   :=@WakeMainThread;
end;

procedure prepare(Item:TGameItem); SysV_ABI_CDecl;
var
 td:p_kthread;
 err:Integer;
 len:Integer;
 exec:array[0..PATH_MAX] of Char;
 argv:array[0..1] of PChar;
begin
 //init all
 sys_init;

 re_init_tty;
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

procedure run_item(Item:TGameItem);
var
 r:Integer;
 td:p_kthread;
begin
 if Item.FLock then Exit;

 if runing then Exit;

 re_init_tty;

 r:=kthread_add(@prepare,Item,@td,'[main]');
 Assert(r=0);

 runing:=True;
end;



end.



