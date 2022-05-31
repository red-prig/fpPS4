unit srConst;

{$mode objfpc}{$H+}

interface

uses
  srNodes,
  srRefId,
  srTypes,
  half16;

type
 PPsrConst=^PsrConst;
 PsrConst=^TsrConst;
 TsrConst=object
  pPrev,pNext,pLeft,pRight:PsrConst;
  //----
  read_count:DWORD;
  ID:TsrRefId; //post id
  pType:PsrType;
  key:packed record
   dtype:TsrDataType;
   count:DWORD;
  end;
  Data:QWORD;
  function  c(n1,n2:PsrConst):Integer; static;
  Procedure mark_read;
  Procedure mark_unread;
  function  GetCompItem(i:Byte):PsrConst; inline;
  Procedure SetCompItem(i:Byte;p:PsrConst); inline;
  function  GetData:QWORD;
  Function  isZeroVal:Boolean; inline;
  Function  isBoolVal:Boolean;
  Function  AsBool:Boolean; inline;
  Function  AsUint:DWORD;
  function  AsInt:Integer; inline;
  Function  AsUint64:QWORD; inline;
  Function  AsInt64:Int64; inline;
  function  AsFloat32:Single;
  function  AsHalf16:THalf16;
  function  AsWord:Word;
  function  AsByte:Byte;
  Procedure _mark_read_child;
  Procedure _mark_unread_child;
  Procedure Clear;
 end;

 TsrConstList=object
  type
   TNodeList=specialize TNodeList<PsrConst>;
   TNodeFetch=specialize TNodeFetch<PsrConst,TsrConst>;
  var
   Alloc:TfnAlloc;
   FList:TNodeList;
   FNTree:TNodeFetch;
  function _Fetch(node:PsrConst):PsrConst;
  function Fetch(dtype:TsrDataType;value:QWORD):PsrConst;
  function Fetchb(value:Boolean):PsrConst; inline;
  function Fetchi(dtype:TsrDataType;value:Integer):PsrConst; inline;
  Function Fetchf(dtype:TsrDataType;value:Single):PsrConst; inline;
  function Fetch_ssrc9_const(SSRC:Word;d2:DWORD):PsrConst;
  function Fetch_ssrc9_const(SSRC:Word;d2:DWORD;rtype:TsrDataType):PsrConst;
  function Fetch_ssrc8_const(SSRC:Byte;d2:DWORD):PsrConst; inline;
  function Fetch_ssrc8_const(SSRC:Byte;d2:DWORD;rtype:TsrDataType):PsrConst; inline;
  function Fetch_vec(rtype:TsrDataType;count:Byte;nodes:PPsrConst):PsrConst;
  function Bitcast(rtype:TsrDataType;old:PsrConst):PsrConst;
 end;

function get_soffset_const_int(SSRC:Word):Integer;
function is_const_soffset(SSRC:Byte):Boolean; inline;
function is_const_ssrc8(SSRC:Byte):Boolean; inline;
function is_const_ssrc9(SSRC:Word):Boolean; inline;
function CompareConst(r1,r2:PsrConst):Boolean;

implementation

function TsrConst.c(n1,n2:PsrConst):Integer;
var
 count:DWORD;
begin
 Result:=CompareByte(n1^.key,n2^.key,SizeOf(TsrConst.key));
 if (Result<>0) then Exit;
 count:=n1^.key.count;
 if (count=0) then
 begin
  Result:=Integer(n1^.Data>n2^.Data)-Integer(n1^.Data<n2^.Data);
 end else
 begin
  Result:=ComparePtruint(@n1^.Data,@n2^.Data,count);
 end;
end;

Procedure TsrConst.mark_read;
begin
 Inc(read_count);
end;

Procedure TsrConst.mark_unread;
begin
 Assert(read_count<>0);
 if (read_count<>0) then Dec(read_count);
end;

function TsrConst.GetCompItem(i:Byte):PsrConst; inline;
begin
 Result:=PPsrConst(@Data)[i];
end;

Procedure TsrConst.SetCompItem(i:Byte;p:PsrConst); inline;
begin
 PPsrConst(@Data)[i]:=p;
end;

function TsrConst.GetData:QWORD;
begin
 Result:=0;
 case key.dtype of
  dtVec2f:
   begin
    PSingle(@Result)[0]:=GetCompItem(0)^.AsFloat32;
    PSingle(@Result)[1]:=GetCompItem(1)^.AsFloat32;
   end;
  dtVec2h:
    begin
     PHalf16(@Result)[0]:=GetCompItem(0)^.AsHalf16;
     PHalf16(@Result)[1]:=GetCompItem(1)^.AsHalf16;
    end;
  dtVec4h:
    begin
     PHalf16(@Result)[0]:=GetCompItem(0)^.AsHalf16;
     PHalf16(@Result)[1]:=GetCompItem(1)^.AsHalf16;
     PHalf16(@Result)[2]:=GetCompItem(2)^.AsHalf16;
     PHalf16(@Result)[3]:=GetCompItem(3)^.AsHalf16;
    end;
  dtStruct2u:
     begin
      PDWord(@Result)[0]:=GetCompItem(0)^.AsUint;
      PDWord(@Result)[1]:=GetCompItem(1)^.AsUint;
     end;
  else
     begin
      Assert(key.count=0,'count<>0');
      Result:=data;
     end;
 end;
end;

Function TsrConst.isZeroVal:Boolean;
begin
 case key.dtype of
  dtInt32  ,
  dtUint32 ,
  dtFloat32:Result:=(AsUint=0);
  else
   Result:=False;
 end;
end;

Function TsrConst.isBoolVal:Boolean;
begin
 if (key.count<>0) then Exit(False);
 Case Data of
  0,1:Result:=True;
  else
      Result:=False;
 end;
end;

Function TsrConst.AsBool:Boolean; inline;
begin
 Result:=PBoolean(@Data)^;
end;

Function TsrConst.AsUint:DWORD;
begin
 Result:=PDWORD(@Data)^;
end;

function TsrConst.AsInt:Integer; inline;
begin
 Result:=PInteger(@Data)^;
end;

Function TsrConst.AsUint64:QWORD; inline;
begin
 Result:=Data;
end;

Function TsrConst.AsInt64:Int64; inline;
begin
 Result:=Int64(Data);
end;

function TsrConst.AsFloat32:Single;
begin
 Result:=PSingle(@Data)^;
end;

function TsrConst.AsHalf16:THalf16;
begin
 Result:=PHalf16(@Data)^
end;

function TsrConst.AsWord:Word;
begin
 Result:=PWord(@Data)^;
end;

function TsrConst.AsByte:Byte;
begin
 Result:=PByte(@Data)^;
end;

Procedure TsrConst._mark_read_child;
var
 i:DWORD;
begin
 if (key.count<>0) then
  For i:=0 to key.count-1 do
  begin
   GetCompItem(i)^.mark_read;
  end;
end;

Procedure TsrConst._mark_unread_child;
var
 i:DWORD;
begin
 if (key.count<>0) then
  For i:=0 to key.count-1 do
  begin
   GetCompItem(i)^.mark_unread;
  end;
end;

Procedure TsrConst.Clear;
var
 i:DWORD;
begin
 if (key.count<>0) then
 begin
  For i:=0 to key.count-1 do
  begin
   GetCompItem(i)^.mark_unread;
  end;
  key.count:=0;
 end;
end;

function TsrConstList._Fetch(node:PsrConst):PsrConst;
var
 size:DWORD;
begin
 Result:=FNTree.Find(node);
 if (Result=nil) then
 begin
  size:=node^.key.count;
  if (size=0) then
   size:=SizeOf(TsrConst)
  else
   size:=SizeOf(TsrConst)-SizeOf(TsrConst.Data)+SizeOf(Pointer)*size;
  Result:=Alloc(size);
  Move(node^,Result^,size);
  FNTree.Insert(Result);
  FList.Push_tail(Result);
 end else
 begin
  node^._mark_unread_child;
 end;
end;

function TsrConstList.Fetch(dtype:TsrDataType;value:QWORD):PsrConst;
var
 rec:record
  node:TsrConst;
  align:array[0..3] of Pointer;
 end;
 h:array[0..3] of PsrConst;
begin
 Result:=nil;
 rec.node:=Default(TsrConst);

 value:=value and GetTypeHigh(dtype);

 Case dtype of
  dtUnknow,
  dtBool,
  dtFloat32,
  dtHalf16,
  dtInt32,
  dtUint32,
  dtInt64,
  dtUint64:

    begin
     rec.node.key.dtype:=dtype;
     rec.node.Data:=value;
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtStruct2u,
  dtVec2u,
  dtVec2i,
  dtVec2f:
    begin
     rec.node.key.dtype:=GetVecChild(dtype);
     rec.node.Data:=PDWORD(@value)[0];
     h[0]:=_Fetch(@rec.node);
     h[0]^.mark_read;
     rec.node.Data:=PDWORD(@value)[1];
     h[1]:=_Fetch(@rec.node);
     h[1]^.mark_read;
     rec.node.key.dtype:=dtype;
     rec.node.key.count:=2;
     rec.node.SetCompItem(0,h[0]);
     rec.node.SetCompItem(1,h[1]);
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtVec2h:
    begin
     rec.node.key.dtype:=dtHalf16;
     rec.node.Data:=PWORD(@value)[0];
     h[0]:=_Fetch(@rec.node);
     h[0]^.mark_read;
     rec.node.Data:=PWORD(@value)[1];
     h[1]:=_Fetch(@rec.node);
     h[1]^.mark_read;
     rec.node.key.dtype:=dtVec2h;
     rec.node.key.count:=2;
     rec.node.SetCompItem(0,h[0]);
     rec.node.SetCompItem(1,h[1]);
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  dtVec4h:
    begin
     rec.node.key.dtype:=dtHalf16;
     rec.node.Data:=PWORD(@value)[0];
     h[0]:=_Fetch(@rec.node);
     h[0]^.mark_read;
     rec.node.Data:=PWORD(@value)[1];
     h[1]:=_Fetch(@rec.node);
     h[1]^.mark_read;
     rec.node.Data:=PWORD(@value)[2];
     h[2]:=_Fetch(@rec.node);
     h[2]^.mark_read;
     rec.node.Data:=PWORD(@value)[3];
     h[3]:=_Fetch(@rec.node);
     h[3]^.mark_read;
     rec.node.key.dtype:=dtVec4h;
     rec.node.key.count:=4;
     rec.node.SetCompItem(0,h[0]);
     rec.node.SetCompItem(1,h[1]);
     rec.node.SetCompItem(2,h[2]);
     rec.node.SetCompItem(3,h[3]);
     Result:=_Fetch(@rec.node);
     Result^.mark_read;
    end;

  else
    Assert(false);
 end;

end;

function TsrConstList.Fetchb(value:Boolean):PsrConst; inline;
begin
 Result:=Fetch(dtBool,QWORD(value));
end;

function TsrConstList.Fetchi(dtype:TsrDataType;value:Integer):PsrConst; inline;
begin
 Result:=Fetch(dtype,PDWORD(@value)^);
end;

Function TsrConstList.Fetchf(dtype:TsrDataType;value:Single):PsrConst; inline;
begin
 Result:=Fetch(dtype,PDWORD(@value)^);
end;

function TsrConstList.Fetch_ssrc9_const(SSRC:Word;d2:DWORD):PsrConst;
begin
 Case SSRC of
  128:Result:=Fetch(dtUnknow,0);
  129..192:Result:=Fetch(dtUint32,SSRC-128);
  193..208:Result:=Fetchi(dtInt32,-(SSRC-192));
  240:Result:=Fetchf(dtFloat32, 0.5);
  241:Result:=Fetchf(dtFloat32,-0.5);
  242:Result:=Fetchf(dtFloat32, 1.0);
  243:Result:=Fetchf(dtFloat32,-1.0);
  244:Result:=Fetchf(dtFloat32, 2.0);
  245:Result:=Fetchf(dtFloat32,-2.0);
  246:Result:=Fetchf(dtFloat32, 4.0);
  247:Result:=Fetchf(dtFloat32,-4.0);
  255:Result:=Fetch(dtUnknow,d2);
  else
      Result:=nil;
 end;
end;

function TsrConstList.Fetch_ssrc9_const(SSRC:Word;d2:DWORD;rtype:TsrDataType):PsrConst;
begin
 Case SSRC of
  128:Result:=Fetch(rtype,0);
  129..192:
    begin
     if CompareType(dtUint32,rtype) then
     begin
      Result:=Fetch(dtUint32,SSRC-128);
     end else
     begin
      Result:=Fetch(rtype,SSRC-128);
     end;
    end;
  193..208:
    begin
     if CompareType(dtInt32,rtype) then
     begin
      Result:=Fetchi(dtInt32,-(SSRC-192));
     end else
     begin
      Result:=Fetchi(rtype,-(SSRC-192));
     end;
    end;
  240:Result:=Fetchf(rtype, 0.5);
  241:Result:=Fetchf(rtype,-0.5);
  242:Result:=Fetchf(rtype, 1.0);
  243:Result:=Fetchf(rtype,-1.0);
  244:Result:=Fetchf(rtype, 2.0);
  245:Result:=Fetchf(rtype,-2.0);
  246:Result:=Fetchf(rtype, 4.0);
  247:Result:=Fetchf(rtype,-4.0);
  255:Result:=Fetch(rtype,d2);
  else
      Result:=nil;
 end;
end;

function TsrConstList.Fetch_ssrc8_const(SSRC:Byte;d2:DWORD):PsrConst; inline;
begin
 Result:=Fetch_ssrc9_const(SSRC,d2);
end;

function TsrConstList.Fetch_ssrc8_const(SSRC:Byte;d2:DWORD;rtype:TsrDataType):PsrConst; inline;
begin
 Result:=Fetch_ssrc9_const(SSRC,d2,rtype);
end;

function TsrConstList.Fetch_vec(rtype:TsrDataType;count:Byte;nodes:PPsrConst):PsrConst;
var
 rec:record
  node:TsrConst;
  align:array[0..3] of Pointer;
 end;
 h:PsrConst;
 i:Byte;
begin
 Assert(count<>0);
 Assert(count<=4);
 rec.node:=Default(TsrConst);
 rec.node.key.dtype:=rtype;
 rec.node.key.count:=count;
 For i:=0 to count-1 do
 begin
  h:=nodes[i];
  h^.mark_read;
  rec.node.SetCompItem(i,h);
 end;
 Result:=_Fetch(@rec.node);
 Result^.mark_read;
end;

function TsrConstList.Bitcast(rtype:TsrDataType;old:PsrConst):PsrConst;
var
 data:qword;
begin
 Result:=nil;
 if (old=nil) then Exit;
 if (rtype=dtUnknow) or (rtype=old^.key.dtype) then
 begin
  old^.mark_read;
  Exit(old);
 end;

 if (old^.key.dtype=dtUnknow) then
 begin
  data:=old^.Data;
  Result:=Fetch(rtype,data);
  Exit;
 end;

 if TryBitcastType(rtype,old^.key.dtype) then
 begin
  data:=old^.GetData;
  Result:=Fetch(rtype,data);
 end else
 begin
  Assert(false,'const bitcast');
 end;
end;

function get_soffset_const_int(SSRC:Word):Integer;
type
 TRec=packed record
  Case byte of
   0:(i:Integer);
   1:(s:Single);
 end;
begin
 Case SSRC of
       128:Result:=0;
  129..192:Result:=SSRC-128;
  193..208:Result:=-(SSRC-192);
       240:TRec(Result).s:= 0.5;
       241:TRec(Result).s:=-0.5;
       242:TRec(Result).s:= 1.0;
       243:TRec(Result).s:=-1.0;
       244:TRec(Result).s:= 2.0;
       245:TRec(Result).s:=-2.0;
       246:TRec(Result).s:= 4.0;
       247:TRec(Result).s:=-4.0;
  else
           Result:=0;
 end;
end;

function is_const_soffset(SSRC:Byte):Boolean; inline;
begin
 Case SSRC of
  128..192,
  193..208,
  240..247:Result:=True;
  else
      Result:=False;
 end;
end;

function is_const_ssrc8(SSRC:Byte):Boolean; inline;
begin
 Case SSRC of
  128..192,
  193..208,
  240..247,
  255:Result:=True;
  else
      Result:=False;
 end;
end;

function is_const_ssrc9(SSRC:Word):Boolean; inline;
begin
 Case SSRC of
  128..192,
  193..208,
  240..247,
  255:Result:=True;
  else
      Result:=False;
 end;
end;

function CompareConstCount(buf1,buf2:PPsrConst;count:PtrUint):Boolean;
begin
 Result:=true;
 While (count<>0) do
 begin
  Result:=CompareConst(buf1^,buf2^);
  if not Result then Exit;
  Inc(buf1);
  Inc(buf2);
  Dec(count);
 end;
end;

function CompareConst(r1,r2:PsrConst):Boolean;
begin
 Result:=false;
 if (r1<>nil) and (r2<>nil) then
 begin
  Result:=(r1=r2);
  if Result then Exit;
  if CompareType(r1^.key.dtype,r2^.key.dtype) and (r1^.key.count=r2^.key.count) then
  begin
   if (r1^.key.count=0) then
   begin
    Result:=(r1^.Data=r2^.Data);
   end else
   begin
    Result:=CompareConstCount(@r1^.Data,@r2^.Data,r1^.key.count);
   end;
  end;
 end;
end;

end.

