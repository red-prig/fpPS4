unit vMetaManager;

{$mode ObjFPC}{$H+}

interface

uses
 SysUtils,
 g23tree,
 vDependence,
 vImage;

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

 TvMetaBuffer=class(TvCustomMeta)
  rclear:Boolean;
 end;

 //
 TvMetaHtile=class(TvCustomMeta)
  private
   function  get_rclear:Boolean;
   procedure set_rclear(b:Boolean);
  public
   buffer:TvMetaBuffer;
   property rclear:Boolean read get_rclear write set_rclear;
 end;

function FetchHtile (cmd:TvDependenciesObject;const F:TvImageKey;size:DWORD;force:Boolean=True):TvMetaHtile;
function FetchBuffer(cmd:TvDependenciesObject;addr:Pointer;size:DWORD;force:Boolean=True):TvMetaBuffer;

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

//

function TvMetaHtile.get_rclear:Boolean; inline;
begin
 Result:=buffer.rclear;
end;

procedure TvMetaHtile.set_rclear(b:Boolean); inline;
begin
 buffer.rclear:=b;
end;

//

function Key2CustomMeta(P:PMetaKey):TvCustomMeta; inline;
begin
 Result:=TvCustomMeta(ptruint(P)-ptruint(@TvCustomMeta(nil).key));
end;

function _Find(const F:TMetaKey):TvCustomMeta;
var
 i:TvCustomMeta2Set.Iterator;
begin
 Result:=nil;

 i:=FMeta2Set.find(@F);

 if (i.Item<>nil) then
 begin
  Result:=Key2CustomMeta(i.Item^);
 end;
end;

{
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

function _Find_buffer(const F:TvImageKey;size:DWORD):TvCustomMeta;
var
 M:TMetaKey;
begin
 M.mtype:=iu_buffer;
 M.mkey :=F;
 M.msize:=size;
 //
 Result:=_Find_be(M);
end;
}

function _FetchMeta(mtype:t_image_usage;const F:TvImageKey;size:DWORD;force:Boolean):TvCustomMeta;
var
 t:TvCustomMeta;
 M:TMetaKey;
begin
 Result:=nil;

 M.mtype:=mtype;
 M.mkey :=F;
 M.msize:=size;

 t:=_Find(M);

 if (t<>nil) then
 begin
  //
 end else
 if force then
 begin
  case mtype of
   iu_buffer:t:=TvMetaBuffer.Create;
   iu_htile :t:=TvMetaHtile .Create;
   else;
  end;

  if (t<>nil) then
  begin
   t.Key:=M;

   if FMeta2Set.Insert(@t.key) then
   begin
    t.Acquire(nil); //map ref
   end;
  end;

 end;

 Result:=t;
end;

function FetchHtile(cmd:TvDependenciesObject;const F:TvImageKey;size:DWORD;force:Boolean=True):TvMetaHtile;
var
 B:TvImageKey;
begin
 rw_wlock(lock);

 Result:=TvMetaHtile(_FetchMeta(iu_htile,F,size,force));

 if (Result<>nil) then
 if (Result.buffer=nil) then
 begin
  //link buffer
  B:=Default(TvImageKey);
  B.Addr:=F.Addr;
  //
  Result.buffer:=TvMetaBuffer(_FetchMeta(iu_buffer,B,size,True));
 end;

 cmd.RefTo(Result);

 rw_wunlock(lock);
end;

function FetchBuffer(cmd:TvDependenciesObject;addr:Pointer;size:DWORD;force:Boolean=True):TvMetaBuffer;
var
 B:TvImageKey;
begin
 B:=Default(TvImageKey);
 B.Addr:=addr;

 rw_wlock(lock);

 Result:=TvMetaBuffer(_FetchMeta(iu_buffer,B,size,force));

 cmd.RefTo(Result);

 rw_wunlock(lock);
end;


end.

