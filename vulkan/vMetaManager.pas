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
  mtype:t_image_usage;
  msize:DWORD;
  mkey :TvImageKey;
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
function FetchHtile(cmd:TvDependenciesObject;addr:Pointer;size:DWORD):TvHtile;

implementation

uses
 kern_rwlock;

type
 TvCustomMeta2Set=specialize T23treeSet<PMetaKey,TMetaKey>;

function TMetaKey.c(a,b:PMetaKey):Integer;
begin
 //type
 Result:=ord(ord(a^.mtype)>ord(b^.mtype))-ord(ord(a^.mtype)<ord(b^.mtype));
 if (Result<>0) then Exit;
 //addr
 Result:=ord(a^.mkey.Addr>b^.mkey.Addr)-ord(a^.mkey.Addr<b^.mkey.Addr);
 if (Result<>0) then Exit;
 //size
 Result:=ord(a^.msize>b^.msize)-ord(a^.msize<b^.msize);
 if (Result<>0) then Exit;
 //params
 Result:=CompareNormalized(a^.mkey,b^.mkey);
end;

var
 lock:Pointer=nil;
 FMeta2Set:TvCustomMeta2Set;

function Key2CustomMeta(P:PMetaKey):TvCustomMeta; inline;
begin
 Result:=TvCustomMeta(ptruint(P)-ptruint(@TvCustomMeta(nil).key));
end;

function _Find(const F:TMetaKey):TvCustomMeta;
label
 _repeat;
var
 T:TMetaKey;
 i:TvCustomMeta2Set.Iterator;
 P:PMetaKey;
begin
 Result:=nil;

 _repeat:

 i:=FMeta2Set.find(@F);

 if (i.Item=nil) then
 begin
  T:=F;
  T.mkey:=Default(TvImageKey);
  T.mkey.Addr:=F.mkey.Addr;
  //
  i:=FMeta2Set.find(@T);
  //
  if (i.Item<>nil) then
  begin
   P:=i.Item^;
   //
   FMeta2Set.erase(i);
   //
   P^:=F;
   //
   FMeta2Set.Insert(P);
   //
   goto _repeat;
  end;
 end;

 if (i.Item<>nil) then
 begin
  Result:=Key2CustomMeta(i.Item^);
 end;
end;

function _Find_be(const F:TMetaKey):TvCustomMeta;
var
 i:TvCustomMeta2Set.Iterator;
 P:PMetaKey;
begin
 Result:=nil;

 i:=FMeta2Set.find_be(@F);

 if (i.Item<>nil) then
 begin
  P:=i.Item^;

  if (P^.mtype<>F.mtype) then Exit(nil);
  if (P^.msize<>F.msize) then Exit(nil);
  if (P^.mkey.Addr<>F.mkey.Addr) then Exit(nil);

  Result:=Key2CustomMeta(P);
 end;

end;

function _FetchHtile(const F:TvImageKey;size:DWORD;is_full:Boolean):TvHtile;
var
 t:TvHtile;
 M:TMetaKey;
begin
 Result:=nil;

 M.mtype:=iu_htile;
 M.mkey :=F;
 M.msize:=size;

 if is_full then
 begin
  t:=TvHtile(_Find(M));
 end else
 begin
  t:=TvHtile(_Find_be(M));
 end;

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

 Result:=_FetchHtile(F,size,True);

 cmd.RefTo(Result);

 rw_wunlock(lock);
end;

function FetchHtile(cmd:TvDependenciesObject;addr:Pointer;size:DWORD):TvHtile;
var
 F:TvImageKey;
begin
 F:=Default(TvImageKey);
 F.Addr:=addr;

 rw_wlock(lock);

 Result:=_FetchHtile(F,size,False);

 cmd.RefTo(Result);

 rw_wunlock(lock);
end;


end.

