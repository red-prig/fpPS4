unit vSetLayoutManager;

{$mode objfpc}{$H+}

interface

uses
 SysUtils,
 RWLock,
 g23tree,
 Vulkan,
 vPipeline;

Function FetchSetLayout(FStage:TVkShaderStageFlags;
                        FFlags:TVkUInt32;
                        const A:AVkDescriptorSetLayoutBinding):TvSetLayout;

implementation

type
 TvSetLayoutCompare=class
  class function c(a,b:PvSetLayoutKey):Integer; static;
 end;

 _TvSetLayoutsPool=specialize T23treeSet<PvSetLayoutKey,TvSetLayoutCompare>;
 TvSetLayoutsPool=object(_TvSetLayoutsPool)
  lock:TRWLock;
  Procedure Init;
  Procedure Lock_wr;
  Procedure Unlock;
 end;

var
 FSetLayoutsPool:TvSetLayoutsPool;

Procedure TvSetLayoutsPool.Init;
begin
 rwlock_init(lock);
end;

Procedure TvSetLayoutsPool.Lock_wr;
begin
 rwlock_wrlock(lock);
end;

Procedure TvSetLayoutsPool.Unlock;
begin
 rwlock_unlock(lock);
end;

function CompareBind(var a,b:TVkDescriptorSetLayoutBinding):Integer; forward;

procedure BubbleSort(Var A:AVkDescriptorSetLayoutBinding);
var
 n,w,i:Integer;

 procedure Swap(var A,B:TVkDescriptorSetLayoutBinding); inline;
 var
  T:TVkDescriptorSetLayoutBinding;
 begin
  T:=A;
  A:=B;
  B:=T;
 end;

begin
 if (Length(A)=0) then Exit;
 n:=High(A);
 repeat
  w:=0;
  for i:=1 to n do
  begin
   if (CompareBind(A[i-1],A[i])>0) then
   begin
    Swap(A[i-1],A[i]);
    w:=i;
   end;
  end;
  n:=w;
 until (n=0);
end;

Function FetchSetLayout(FStage:TVkShaderStageFlags;
                        FFlags:TVkUInt32;
                        const A:AVkDescriptorSetLayoutBinding):TvSetLayout;
var
 key:TvSetLayoutKey;

 t:TvSetLayout;
 i:TvSetLayoutsPool.Iterator;
begin
 key:=Default(TvSetLayoutKey);
 key.FStage:=FStage;
 key.FFlags:=FFlags;
 key.FBinds:=A;
 BubbleSort(key.FBinds);

 FSetLayoutsPool.Lock_wr;

 i:=FSetLayoutsPool.find(@key);
 if (i.Item=nil) then
 begin
  t:=TvSetLayout.Create;
  t.key:=key;
  FSetLayoutsPool.Insert(@t.key);
  Result:=t;
 end else
 begin
  t:=TvSetLayout(ptruint(i.Item^)-ptruint(@TvSetLayout(nil).key));

  Result:=t;
 end;

 FSetLayoutsPool.Unlock;

 t.Compile;
end;


function CompareBind(var a,b:TVkDescriptorSetLayoutBinding):Integer;
begin
 //1 binding
 Result:=Integer(a.binding>b.binding)-Integer(a.binding<b.binding);
 if (Result<>0) then Exit;
 //2 descriptorType
 Result:=Integer(a.descriptorType>b.descriptorType)-Integer(a.descriptorType<b.descriptorType);
 if (Result<>0) then Exit;
 //3 descriptorCount
 Result:=Integer(a.descriptorCount>b.descriptorCount)-Integer(a.descriptorCount<b.descriptorCount);
 if (Result<>0) then Exit;
 //4 stageFlags
 Result:=Integer(a.stageFlags>b.stageFlags)-Integer(a.stageFlags<b.stageFlags);
 if (Result<>0) then Exit;
 //5 pImmutableSamplers
 Result:=Integer(a.pImmutableSamplers>b.pImmutableSamplers)-Integer(a.pImmutableSamplers<b.pImmutableSamplers);
end;

function CompareBinds(var buf1,buf2:AVkDescriptorSetLayoutBinding;count:PtrUint):Integer;
var
 i:PtrUint;
begin
 Result:=0;
 if (count<>0) then
  For i:=0 to count-1 do
  begin
   Result:=CompareBind(buf1[i],buf2[i]);
   if (Result<>0) then Exit;
  end;
end;

class function TvSetLayoutCompare.c(a,b:PvSetLayoutKey):Integer;
begin
 //1 FStage
 Result:=Integer(a^.FStage>b^.FStage)-Integer(a^.FStage<b^.FStage);
 if (Result<>0) then Exit;
 //2 FFlag
 Result:=Integer(a^.FFlags>b^.FFlags)-Integer(a^.FFlags<b^.FFlags);
 if (Result<>0) then Exit;
 //3 Length(FBinds)
 Result:=Integer(Length(a^.FBinds)>Length(b^.FBinds))-Integer(Length(a^.FBinds)<Length(b^.FBinds));
 if (Result<>0) then Exit;
 //4 FBinds
 Result:=CompareBinds(a^.FBinds,b^.FBinds,Length(a^.FBinds));
end;

initialization
 FSetLayoutsPool.Init;

end.


