unit audioout_interface;

{$mode ObjFPC}{$H+}

interface

type
 pAudioOutParam=^TAudioOutParam;

 TAudioOutHandle=class
  const
   f_freq=48000;
  var
   f_userId   :DWORD;
   f_type     :DWORD;
   f_len      :DWORD;
   f_param    :DWORD;
   f_channels :DWORD;
  Function  Open(const device_id:RawByteString):Boolean; virtual; abstract;
  procedure SetVolume(channel,vol:Integer);              virtual; abstract;
  procedure SetMixLevelPadSpk(mixLevel:Integer);         virtual; abstract;
  function  GetLastOutputTime:QWORD;                     virtual; abstract; //microseconds
  function  Output(ptr:Pointer):Integer;                 virtual; abstract;
  //
  class procedure Outputs(param:pAudioOutParam;num:DWORD); virtual; abstract;
 end;

 TAudioOutParam=packed record
  handle:TAudioOutHandle;
  ptr   :Pointer;
 end;

 TAbstractAudioOut=class of TAudioOutHandle;

 TAudioOutNull=class(TAudioOutHandle)
  f_period   :QWORD; //microseconds
  f_last_time:QWORD; //microseconds
  //
  Function  Open(const device_id:RawByteString):Boolean; override;
  procedure SetVolume(channel,vol:Integer);              override;
  procedure SetMixLevelPadSpk(mixLevel:Integer);         override;
  function  GetLastOutputTime:QWORD;                     override;
  function  Output(ptr:Pointer):Integer;                 override;
  //
  class procedure Outputs(param:pAudioOutParam;num:DWORD); override;
 end;

implementation

uses
 time,
 syscalls;

function GetProcessTime():QWORD; //microseconds
var
 time:timespec;
begin
 time:=Default(timespec);
 clock_gettime(15,@time);
 //
 Result:=(time.tv_nsec div 1000) + (time.tv_sec * 1000000);
end;

procedure usleep(usec:QWORD); //microseconds
var
 time:timespec;
begin
 time.tv_sec :=usec div 1000000;
 time.tv_nsec:=((usec mod 1000000) * 1000);
 _nanosleep(@time,nil);
end;

Function TAudioOutNull.Open(const device_id:RawByteString):Boolean;
begin
 f_period   :=((QWORD(1000000) * QWORD(f_len)) + (48000 div 2)) div 48000;
 f_last_time:=0;
 Result:=True;
end;

procedure TAudioOutNull.SetVolume(channel,vol:Integer);
begin
 //
end;

procedure TAudioOutNull.SetMixLevelPadSpk(mixLevel:Integer);
begin
 //
end;

function TAudioOutNull.GetLastOutputTime:QWORD;
begin
 Result:=f_last_time;
end;

function TAudioOutNull.Output(ptr:Pointer):Integer;
var
 time:QWORD;
begin
 Result:=0;
 if (ptr<>nil) then
 begin
  //async send

  time:=GetProcessTime;

  if (f_last_time=0) or
     ((time-f_last_time)>=f_period) then
  begin
   f_last_time:=time;
  end else
  begin
   Result:=-1; //BUSY
  end;

 end else
 begin
  //sync wait

  if (f_last_time=0) then
  begin
   Exit;
  end;

  time:=GetProcessTime;

  if (f_last_time>time) then
  begin
   Exit;
  end;

  time:=(time-f_last_time);

  if (time<f_period) then
  begin
   time:=f_period-time;
   //
   usleep(time);
  end;

 end;
end;

//
class procedure TAudioOutNull.Outputs(param:pAudioOutParam;num:DWORD);
var
 i:DWORD;
begin
 for i:=0 to num-1 do
 begin
  param[i].handle.Output(param[i].ptr);
 end;
end;


end.

