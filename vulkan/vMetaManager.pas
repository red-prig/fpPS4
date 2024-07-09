unit vMetaManager;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 g23tree,
 vDependence,
 vImage,
 vRegs2Vulkan;

type
 PMetaKey=^TMetaKey;
 TMetaKey=packed object
  mkey :TvImageKey;
  mtype:t_image_usage;
  msize:DWORD;
  function c(a,b:PMetaKey):Integer; static;
 end;

 TvCustomMeta=class(TvRefsObject)
  Key:TMetaKey;
 end;

 //

 TvHtile=class(TvCustomMeta)
  rclear:Boolean;
 end;

function FetchHtile(cmd:TvDependenciesObject;const F:TvImageKey;size:DWORD):TvHtile;

implementation

uses
 kern_rwlock;

type
 TvCustomMeta2Set=specialize T23treeSet<PMetaKey,TMetaKey>;

function TMetaKey.c(a,b:PMetaKey):Integer;
begin
 Result:=ord(ord(a^.mtype)>ord(b^.mtype))-ord(ord(a^.mtype)<ord(b^.mtype));
 if (Result<>0) then Exit;
 //
 Result:=CompareNormalized(a^.mkey,b^.mkey);
 if (Result<>0) then Exit;
 //
 Result:=ord(a^.msize>b^.msize)-ord(a^.msize<b^.msize);
end;

var
 lock:Pointer=nil;
 FMeta2Set:TvCustomMeta2Set;


function _Find(const F:TMetaKey):TvCustomMeta;
var
 i:TvCustomMeta2Set.Iterator;
begin
 Result:=nil;
 i:=FMeta2Set.find(@F);
 if (i.Item<>nil) then
 begin
  Result:=TvCustomMeta(ptruint(i.Item^)-ptruint(@TvCustomMeta(nil).key));
 end;
end;

function _FetchHtile(const F:TvImageKey;size:DWORD):TvHtile;
var
 t:TvHtile;
 M:TMetaKey;
begin
 Result:=nil;

 M.mtype:=iu_htile;
 M.mkey :=F;
 M.msize:=size;

 t:=TvHtile(_Find(M));

 if (t<>nil) then
 begin
  //
 end else
 begin
  t:=TvHtile.Create;
  t.Key:=M;

  if FMeta2Set.Insert(@t.key) then
  begin
   t.Acquire(nil); //map ref
  end;
 end;

 Result:=t;
end;

function FetchHtile(cmd:TvDependenciesObject;const F:TvImageKey;size:DWORD):TvHtile;
begin
 rw_wlock(lock);

 Result:=_FetchHtile(F,size);

 cmd.RefTo(Result);

 rw_wunlock(lock);
end;

end.

