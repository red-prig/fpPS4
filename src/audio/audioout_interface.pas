unit audioout_interface;

{$mode ObjFPC}{$H+}

interface

type
 pAudioOutParam=^TAudioOutParam;

 TAudioParams=bitpacked record
  channels      :0..8;
  is_float      :Boolean;
  is_std        :Boolean;
  is_restricted :Boolean;
  is_mix_to_main:Boolean;
 end;

 TAudioOutHandle=class
  const
   f_freq=48000;
  var
   f_userId   :DWORD;
   f_type     :DWORD;
   f_len      :DWORD;
   f_param    :TAudioParams;
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
  f_next_time:QWORD; //microseconds
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
 md_time,
 time,
 md_sleep;

procedure usleep(usec:QWORD); inline; //microseconds
begin
 if (usec<>0)then
  msleep_td(USEC_TO_UNIT(usec));
end;

Function TAudioOutNull.Open(const device_id:RawByteString):Boolean;
begin
 f_period   :=((QWORD(1000000) * QWORD(f_len)) + (f_freq div 2)) div f_freq;
 f_last_time:=0;
 f_next_time:=0;
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
 time,d:QWORD;
 f_need_wait:QWORD;
begin
 Result:=0;

 time:=GetProcessTime;

 if (f_next_time=0) then
 begin
  //first send
  f_next_time:=(time+f_period);
  f_need_wait:=0;
 end else
 if (time>f_next_time) then
 begin
  //underflow
  f_next_time:=(time+f_period);
  f_need_wait:=0;
 end else
 begin
  //wait prev
  f_need_wait:=f_next_time;
  f_next_time:=(f_next_time+f_period);
 end;

 if (f_need_wait<>0) then
 begin
  repeat
   time:=GetProcessTime;
   if (time>=f_need_wait) then Break;
   //
   d:=f_need_wait-time;
   if d>10 then d:=d-10;
   //
   usleep(d);
  until false;
 end;

 f_last_time:=GetProcessTime;
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

