unit vSamplerManager;

{$mode objfpc}{$H+}

interface

uses
 ps4_shader,
 vRegs2Vulkan,
 SysUtils,
 g23tree,
 Vulkan,
 vDevice,
 vSampler,
 vCmdBuffer;

function FetchSampler(cmd:TvCustomCmdBuffer;PS:PSSharpResource4):TvSampler;

implementation

uses
 kern_rwlock;

type
 TvSampler2Compare=object
  function c(a,b:PSSharpResource4):Integer; static;
 end;

 TvSampler2=class(TvSampler)
  key:TSSharpResource4;
 end;

 _TvSampler2Set=specialize T23treeSet<PSSharpResource4,TvSampler2Compare>;
 TvSampler2Set=object(_TvSampler2Set)
  lock:Pointer;
  Procedure Lock_wr;
  Procedure Unlock_wr;
 end;

var
 FSampler2Set:TvSampler2Set;

Procedure TvSampler2Set.Lock_wr;
begin
 rw_wlock(lock);
end;

Procedure TvSampler2Set.Unlock_wr;
begin
 rw_wunlock(lock);
end;

function TvSampler2Compare.c(a,b:PSSharpResource4):Integer;
begin
 Result:=CompareByte(a^,b^,SizeOf(TSSharpResource4));
end;

function _Find(PS:PSSharpResource4):TvSampler2;
var
 i:TvSampler2Set.Iterator;
begin
 Result:=nil;
 i:=FSampler2Set.find(PS);
 if (i.Item<>nil) then
 begin
  Result:=TvSampler2(ptruint(i.Item^)-ptruint(@TvSampler2(nil).key));
 end;
end;

function _FetchSampler(PS:PSSharpResource4):TvSampler2;
var
 t:TvSampler2;
 info:TVkSamplerCreateInfo;
begin
 Result:=nil;

 t:=_Find(PS);

 if (t=nil) then
 begin
  info:=_get_ssharp_info(PS);

  t:=TvSampler2.Create;
  t.key:=PS^;

  if not t.Compile(@info) then
  begin
   FreeAndNil(t);
  end else
  begin
   t.Acquire(nil);
   FSampler2Set.Insert(@t.key);
  end;
 end;

 Result:=t;
end;

function FetchSampler(cmd:TvCustomCmdBuffer;PS:PSSharpResource4):TvSampler;
begin
 Result:=nil;
 if (PS=nil) then Exit;

 FSampler2Set.Lock_wr;

 Result:=_FetchSampler(PS);

 cmd.RefTo(Result);

 FSampler2Set.Unlock_wr;
end;

end.

