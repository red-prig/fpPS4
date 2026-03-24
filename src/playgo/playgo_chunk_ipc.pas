unit playgo_chunk_ipc;

{$mode ObjFPC}{$H+}

interface

uses
 playgo_chunk_gui;

var
 playgo_lock     :Pointer=nil;
 playgo_lazy_init:Integer=0;
 playgo_file     :TPlaygoFile=nil;

function  is_init_playgo:Boolean;
procedure init_playgo;
procedure free_playgo;

implementation

uses
 sysutils,
 atomic,
 sys_bootparam,
 host_ipc,
 kern_rwlock;

function is_init_playgo:Boolean;
begin
 Result:=(playgo_lazy_init=2);
end;

procedure init_playgo;
var
 Value:TIpcValue;
begin
 if (playgo_lazy_init=2) then Exit;

 Writeln('PLAYGO_INIT');

 if CAS(playgo_lazy_init,0,1) then
 begin
  rw_wlock(playgo_lock);

  Value:=p_host_ipc.InvokeSync('PLAYGO_INIT');
  playgo_file:=TPlaygoFile(Value.GetObject(TPlaygoFile));
  Value.Free;

  playgo_lazy_init:=2;
  rw_wunlock(playgo_lock);
 end else
 begin
  //sunc
  rw_wlock  (playgo_lock);
  rw_wunlock(playgo_lock);
 end;
end;

procedure free_playgo;
begin
 rw_wlock  (playgo_lock);

 FreeAndNil(playgo_file);

 rw_wunlock(playgo_lock);
end;

end.

