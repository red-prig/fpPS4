unit srConst;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 spirv,
 ginodes,
 srNode,
 srLiteral,
 srRefId,
 srType,
 srTypes,
 half16;

const
 FLT_MAX=3.402823466e+38;

type
 TsrConst=class;

 PPsrConst=^TsrConst;

 PsrConstKey=^TsrConstKey;
 TsrConstKey=packed record
  fOpId :WORD;
  fCount:WORD;
  FType :TsrType;
  pData :PPsrNode;
 end;

 TsrConst=class(TsrNode)
  public
   pPrev,pNext,pLeft,pRight:TsrConst;
   class function c(n1,n2:PsrConstKey):Integer; static;
  private
   ID:TsrRefId; //post id
   key:TsrConstKey;
  public
   //
   Procedure _zero_read    ;               override;
   Procedure _zero_unread  ;               override;
   function  _Next         :TsrNode;       override;
   function  _Prev         :TsrNode;       override;
   Function  _GetPtype     :TsrNode;       override;
   function  _GetPrintName :RawByteString; override;
   function  _GetRef       :Pointer;       override;
   //
   property  pType:TsrType     read key.FType;
   property  OpId:WORD         read key.fOpId;
   property  Count:WORD        read key.fCount;
   property  ItemCount:WORD    read key.fCount;
   function  dtype:TsrDataType;
   function  GetItem(i:Word):TsrNode;
   function  GetLiteral(i:Word):TsrLiteral;
   function  GetConst(i:Word):TsrConst;
   Function  AsBool:Boolean;   inline;
   function  AsUint8:Byte;     inline;
   function  AsInt8:Shortint;  inline;
   function  AsUint16:Word;    inline;
   function  AsInt16:Smallint; inline;
   Function  AsUint32:DWORD;   inline;
   function  AsInt32:Integer;  inline;
   Function  AsUint64:QWORD;   inline;
   Function  AsInt64:Int64;    inline;
   function  AsHalf16:THalf16; inline;
   function  AsFloat32:Single; inline;
   function  AsFloat64:Double; inline;
   function  GetData:QWORD;
   Function  isZeroVal:Boolean; inline;
   Function  isBoolVal:Boolean; inline;
   function  GetPrintName:RawByteString;
 end;

 ntConst=TsrConst;

 PsrConstList=^TsrConstList;
 TsrConstList=object
  type
   TNodeList=specialize TNodeListClass<TsrConst>;
   TNodeTree=specialize TNodeTreeClass<TsrConst>;
  var
   FEmit:TCustomEmit;
   FList:TNodeList;
   FTree:TNodeTree;
  Procedure Init(Emit:TCustomEmit); inline;
  function  _Fetch(key:PsrConstKey;copy:Boolean):TsrConst;
  function  _Fetch1(dtype:TsrDataType;OpId:DWORD;value:QWORD):TsrConst;
  function  _FetchVector(dtype:TsrDataType;value:QWORD):TsrConst;
  function  Fetch(dtype:TsrDataType;value:QWORD):TsrConst;
  function  Fetch_b(value:Boolean):TsrConst; inline;
  function  Fetch_i(dtype:TsrDataType;value:Integer):TsrConst; inline;
  Function  Fetch_s(dtype:TsrDataType;value:Single):TsrConst; inline;
  function  FetchVector(pType:TsrType;count:Word;pData:PPsrConst;copy:Boolean):TsrConst;
  function  FetchVector(dtype:TsrDataType;pData:PPsrConst;copy:Boolean):TsrConst;
  function  Bitcast(rtype:TsrDataType;old:TsrConst):TsrConst;
  function  Fetch_ssrc9_const(SSRC:Word;d2:DWORD):TsrConst;
  function  Fetch_ssrc9_const(SSRC:Word;d2:DWORD;rtype:TsrDataType):TsrConst;
  function  Fetch_ssrc8_const(SSRC:Byte;d2:DWORD):TsrConst; inline;
  function  Fetch_ssrc8_const(SSRC:Byte;d2:DWORD;rtype:TsrDataType):TsrConst; inline;
  function  First:TsrConst; inline;
 end;

function get_soffset_const_int(SSRC:Word):Integer;
function is_const_soffset(SSRC:Byte):Boolean; inline;
function is_const_ssrc8(SSRC:Byte):Boolean; inline;
function is_const_ssrc9(SSRC:Word):Boolean; inline;
function CompareConst(r1,r2:TsrConst):Boolean;

Function TryTruncInt64(c:Single;var i:int64):Boolean;

operator := (i:TsrNode):TsrConst; inline;

implementation

operator := (i:TsrNode):TsrConst; inline;
begin
 Result:=TsrConst(Pointer(i)); //typecast hack
end;

Procedure TsrConst._zero_read;
var
 i:WORD;
begin
 pType.mark_read(Self);
 if (Count<>0) then
  For i:=0 to Count-1 do
  begin
   GetItem(i).mark_read(Self);
  end;
end;

Procedure TsrConst._zero_unread;
var
 i:WORD;
begin
 pType.mark_unread(Self);
 if (Count<>0) then
  For i:=0 to Count-1 do
  begin
   GetItem(i).mark_unread(Self);
  end;
end;

function TsrConst._Next:TsrNode;
begin
 Result:=pNext;
end;

function TsrConst._Prev:TsrNode;
begin
 Result:=pPrev;
end;

Function TsrConst._GetPtype:TsrNode;
begin
 Result:=pType;
end;

function TsrConst._GetPrintName:RawByteString;
begin
 Result:=GetPrintName;
end;

function TsrConst._GetRef:Pointer;
begin
 Result:=@ID;
end;

//

class function TsrConst.c(n1,n2:PsrConstKey):Integer;
begin
 //first FType (order sort)
 Result:=ord(n1^.FType.Order>n2^.FType.Order)-ord(n1^.FType.Order<n2^.FType.Order);
 if (Result<>0) then Exit;
 //second fCount
 Result:=ord(n1^.fCount>n2^.fCount)-ord(n1^.fCount<n2^.fCount);
 if (Result<>0) then Exit;
 //third pData (order sort)
 Result:=CompareNodes(n1^.pData,n2^.pData,n1^.fCount);
end;

function TsrConst.dtype:TsrDataType;
begin
 Result:=pType.dtype;
end;

function TsrConst.GetItem(i:Word):TsrNode;
begin
 if (i>Count) then Exit(nil);
 Result:=key.pData[i];
end;

function TsrConst.GetLiteral(i:Word):TsrLiteral;
begin
 Result:=GetItem(i).specialize AsType<ntLiteral>;
end;

function TsrConst.GetConst(i:Word):TsrConst;
begin
 Result:=GetItem(i).specialize AsType<ntConst>;
end;

Function TsrConst.AsBool:Boolean; inline;
begin
 Result:=GetLiteral(0).AsBool;
end;

function TsrConst.AsUint8:Byte; inline;
begin
 Result:=GetLiteral(0).AsUint8;
end;

function TsrConst.AsInt8:Shortint; inline;
begin
 Result:=GetLiteral(0).AsInt8;
end;

function TsrConst.AsUint16:Word; inline;
begin
 Result:=GetLiteral(0).AsUint16;
end;

function TsrConst.AsInt16:Smallint; inline;
begin
 Result:=GetLiteral(0).AsInt16;
end;

Function TsrConst.AsUint32:DWORD; inline;
begin
 Result:=GetLiteral(0).AsUint32;
end;

function TsrConst.AsInt32:Integer; inline;
begin
 Result:=GetLiteral(0).AsInt32;
end;

Function TsrConst.AsUint64:QWORD; inline;
begin
 Result:=GetLiteral(0).AsUint64;
end;

Function TsrConst.AsInt64:Int64; inline;
begin
 Result:=GetLiteral(0).AsInt64;
end;

function TsrConst.AsHalf16:THalf16; inline;
begin
 Result:=GetLiteral(0).AsHalf16;
end;

function TsrConst.AsFloat32:Single; inline;
begin
 Result:=GetLiteral(0).AsFloat32;
end;

function TsrConst.AsFloat64:Double; inline;
begin
 Result:=GetLiteral(0).AsFloat64;
end;

function TsrConst.GetData:QWORD;
begin
 Result:=0;
 case dtype of
  dtVec2u8,
  dtVec2i8:
   begin
    PBYTE(@Result)[0]:=GetConst(0).AsUint8;
    PBYTE(@Result)[1]:=GetConst(1).AsUint8;
   end;

  dtVec4u8,
  dtVec4i8:
   begin
    PBYTE(@Result)[0]:=GetConst(0).AsUint8;
    PBYTE(@Result)[1]:=GetConst(1).AsUint8;
    PBYTE(@Result)[2]:=GetConst(2).AsUint8;
    PBYTE(@Result)[3]:=GetConst(3).AsUint8;
   end;

  dtStruct2u,
  dtVec2u,
  dtVec2i,
  dtVec2f:
   begin
    PDWORD(@Result)[0]:=GetConst(0).AsUint32;
    PDWORD(@Result)[1]:=GetConst(1).AsUint32;
   end;

  dtVec2u16,
  dtVec2i16,
  dtVec2h  :
   begin
    PWORD(@Result)[0]:=GetConst(0).AsUint16;
    PWORD(@Result)[1]:=GetConst(1).AsUint16;
   end;

  dtVec4u16,
  dtVec4i16,
  dtVec4h  :
   begin
    PWORD(@Result)[0]:=GetConst(0).AsUint16;
    PWORD(@Result)[1]:=GetConst(1).AsUint16;
    PWORD(@Result)[2]:=GetConst(2).AsUint16;
    PWORD(@Result)[3]:=GetConst(3).AsUint16;
   end;

  else
     begin
      Assert(Count=1,'Count<>1');
      Result:=AsUint64;
     end;
 end;
end;

Function TsrConst.isZeroVal:Boolean;
begin
 case dtype of
  dtInt8,
  dtUint8:Result:=(AsUint8=0);

  dtHalf16,
  dtInt16,
  dtUint16:Result:=(AsUint16=0);

  dtFloat32,
  dtInt32,
  dtUint32:Result:=(AsUint32=0);

  dtFloat64,
  dtInt64,
  dtUint64:Result:=(AsUint64=0);
  else
   Result:=False;
 end;
end;

Function TsrConst.isBoolVal:Boolean; inline;
begin
 if (Count<>1) then Exit(False);
 Case AsUint64 of
  0,1:Result:=True;
  else
      Result:=False;
 end;
end;

function TsrConst.GetPrintName:RawByteString;
var
 s:Single;
 i:Int64;
 ui:qword;
begin
 Result:='';

 Case dtype of
  dtBool:
    begin
     Case AsBool of
      true :Result:='true';
      False:Result:='false';
     end;
    end;

  dtHalf16:
    begin
     s:=Single(AsHalf16);
     i:=0;
     if TryTruncInt64(s,i) then
     begin
      if (s=i) then
      begin
       Case i of
          0..99:Result:='ch'+IntToStr(i);
         -9..-1:Result:='chm'+IntToStr(abs(i));
        else;
       end;
      end;
     end;
    end;

  dtFloat32:
    begin
     s:=AsFloat32;
     i:=0;
     if TryTruncInt64(s,i) then
     begin
      if (s=i) then
      begin
       Case i of
          0..99:Result:='cf'+IntToStr(i);
         -9..-1:Result:='cfm'+IntToStr(abs(i));
        else;
       end;
      end;
     end;
    end;

   dtInt32 :
     begin
      i:=AsInt32;
      Case i of
         0..99:Result:='ci'+IntToStr(i);
        -9..-1:Result:='cim'+IntToStr(abs(i));
       else;
      end;
     end;

   dtUint32:
     begin
      ui:=AsUint32;
      Case ui of
         0..99:Result:='cu'+IntToStr(ui);
         else;
      end;
     end;

  else;
 end;

 if (Result='') then
 begin
  Assert(ID.Alloc);
  Result:='c'+IntToStr(ID.ID);
 end;
end;

//

Procedure TsrConstList.Init(Emit:TCustomEmit); inline;
begin
 FEmit:=Emit;
end;

function TsrConstList._Fetch(key:PsrConstKey;copy:Boolean):TsrConst;
var
 size:DWORD;
begin
 Result:=FTree.Find(key);
 if (Result=nil) then
 begin
  Result:=FEmit.specialize New<TsrConst>;
  Result.key:=key^;

  if copy and (key^.fCount<>0) then
  begin
   size:=SizeOf(Pointer)*key^.fCount;
   Result.key.pData:=FEmit.Alloc(size);
   Move(key^.pData^,Result.key.pData^,Size);
  end;

  FTree.Insert(Result);
  FList.Push_tail(Result);
 end;
end;

function TsrConstList._Fetch1(dtype:TsrDataType;OpId:DWORD;value:QWORD):TsrConst;
var
 pLiteralList:PsrLiteralList;
 pTypeList:PsrTypeList;
 key:TsrConstKey;
 item:array[0..0] of TsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 pTypeList   :=FEmit.GetTypeList;
 //
 item[0]:=pLiteralList^.FetchConst(dtype,value);
 //
 key:=Default(TsrConstKey);
 key.fOpId :=OpId;
 key.fCount:=1;
 key.FType :=pTypeList^.Fetch(dtype);
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrConstList._FetchVector(dtype:TsrDataType;value:QWORD):TsrConst;
var
 pTypeList:PsrTypeList;
 key:TsrConstKey;
 item:array[0..3] of TsrNode;
 Child:TsrDataType;
 High:QWORD;
 i,BitSize:Byte;
begin
 Result:=nil;
 pTypeList:=FEmit.GetTypeList;
 //
 Child  :=dtype.Child;
 BitSize:=Child.BitSize;
 High   :=Child.High;
 For i:=0 to dtype.Count-1 do
 begin
  item[i]:=Fetch(Child,value and High);
  value:=value shr BitSize;
 end;
 //
 key:=Default(TsrConstKey);
 key.fOpId :=Op.OpConstantComposite;
 key.fCount:=dtype.Count;
 key.FType :=pTypeList^.Fetch(dtype);
 key.pData :=@item;
 //
 Result:=_Fetch(@key,True);
end;

function TsrConstList.Fetch(dtype:TsrDataType;value:QWORD):TsrConst;
begin
 Result:=nil;

 value:=value and dtype.High;

 Case dtype of
  dtUnknow:
   begin
    Result:=_Fetch1(dtype,Op.OpNop,value);
   end;

  dtBool:
   begin
    if Boolean(value) then
    begin
     Result:=_Fetch1(dtype,Op.OpConstantTrue ,value);
    end else
    begin
     Result:=_Fetch1(dtype,Op.OpConstantFalse,value);
    end;
   end;

  dtHalf16,
  dtFloat32,
  dtFloat64,
  dtInt8,
  dtUint8,
  dtInt16,
  dtUint16,
  dtInt32,
  dtUint32,
  dtInt64,
  dtUint64:
    begin
     Result:=_Fetch1(dtype,Op.OpConstant,value);
    end;

  dtStruct2u,
  dtVec2u8,
  dtVec4u8,
  dtVec2i8,
  dtVec4i8,
  dtVec2u16,
  dtVec4u16,
  dtVec2i16,
  dtVec4i16,
  dtVec2u,
  dtVec2i,
  dtVec2h,
  dtVec4h,
  dtVec2f:
    begin
     Result:=_FetchVector(dtype,value);
    end;

  else
    Assert(false);
 end;

end;

function TsrConstList.Fetch_b(value:Boolean):TsrConst; inline;
begin
 Result:=Fetch(dtBool,QWORD(value));
end;

function TsrConstList.Fetch_i(dtype:TsrDataType;value:Integer):TsrConst; inline;
begin
 Result:=Fetch(dtype,PDWORD(@value)^);
end;

Function TsrConstList.Fetch_s(dtype:TsrDataType;value:Single):TsrConst; inline;
begin
 Result:=Fetch(dtype,PDWORD(@value)^);
end;

function TsrConstList.FetchVector(pType:TsrType;count:Word;pData:PPsrConst;copy:Boolean):TsrConst;
var
 key:TsrConstKey;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 //
 key:=Default(TsrConstKey);
 key.fOpId :=Op.OpConstantComposite;
 key.fCount:=count;
 key.FType :=pType;
 key.pData :=Pointer(pData);
 //
 Result:=_Fetch(@key,copy);
end;

function TsrConstList.FetchVector(dtype:TsrDataType;pData:PPsrConst;copy:Boolean):TsrConst;
var
 pTypeList:PsrTypeList;
begin
 pTypeList:=FEmit.GetTypeList;
 Result:=FetchVector(pTypeList^.Fetch(dtype),dtype.Count,pData,copy);
end;

function TsrConstList.Bitcast(rtype:TsrDataType;old:TsrConst):TsrConst;
var
 data:qword;
begin
 Result:=nil;

 if not old.IsType(ntConst) then Exit(old);

 if (rtype=dtUnknow) or (rtype=old.dtype) then
 begin
  Exit(old);
 end;

 if (old.dtype=dtUnknow) then
 begin
  data:=old.GetData;
  Result:=Fetch(rtype,data);
  Exit;
 end;

 if TryBitcastType(rtype,old.dtype) then
 begin
  data:=old.GetData;
  Result:=Fetch(rtype,data);
 end else
 begin
  Assert(false,'const bitcast');
 end;
end;

function TsrConstList.Fetch_ssrc9_const(SSRC:Word;d2:DWORD):TsrConst;
begin
 Case SSRC of
  128:Result:=Fetch(dtUnknow,0);
  129..192:Result:=Fetch(dtUint32,SSRC-128);
  193..208:Result:=Fetch_i(dtInt32,-(SSRC-192));
  240:Result:=Fetch_s(dtFloat32, 0.5);
  241:Result:=Fetch_s(dtFloat32,-0.5);
  242:Result:=Fetch_s(dtFloat32, 1.0);
  243:Result:=Fetch_s(dtFloat32,-1.0);
  244:Result:=Fetch_s(dtFloat32, 2.0);
  245:Result:=Fetch_s(dtFloat32,-2.0);
  246:Result:=Fetch_s(dtFloat32, 4.0);
  247:Result:=Fetch_s(dtFloat32,-4.0);
  255:Result:=Fetch(dtUnknow,d2);
  else
      Result:=nil;
 end;
end;

function TsrConstList.Fetch_ssrc9_const(SSRC:Word;d2:DWORD;rtype:TsrDataType):TsrConst;
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
      Result:=Fetch_i(dtInt32,-(SSRC-192));
     end else
     begin
      Result:=Fetch_i(rtype,-(SSRC-192));
     end;
    end;
  240:Result:=Fetch_s(rtype, 0.5);
  241:Result:=Fetch_s(rtype,-0.5);
  242:Result:=Fetch_s(rtype, 1.0);
  243:Result:=Fetch_s(rtype,-1.0);
  244:Result:=Fetch_s(rtype, 2.0);
  245:Result:=Fetch_s(rtype,-2.0);
  246:Result:=Fetch_s(rtype, 4.0);
  247:Result:=Fetch_s(rtype,-4.0);
  255:Result:=Fetch(rtype,d2);
  else
      Result:=nil;
 end;
end;

function TsrConstList.Fetch_ssrc8_const(SSRC:Byte;d2:DWORD):TsrConst; inline;
begin
 Result:=Fetch_ssrc9_const(SSRC,d2);
end;

function TsrConstList.Fetch_ssrc8_const(SSRC:Byte;d2:DWORD;rtype:TsrDataType):TsrConst; inline;
begin
 Result:=Fetch_ssrc9_const(SSRC,d2,rtype);
end;

function TsrConstList.First:TsrConst; inline;
begin
 Result:=FList.pHead;
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

function CompareConstCount(buf1,buf2:PPsrConst;count:Byte):Boolean;
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

function CompareConst(r1,r2:TsrConst):Boolean;
begin
 Result:=false;
 if (r1<>nil) and (r2<>nil) then
 begin
  Result:=(r1=r2);
  if Result then Exit;
  if CompareType(r1.dtype,r2.dtype) and (r1.Count=r2.Count) then
  begin
   if (r1.Count=1) then
   begin
    Result:=(r1.AsUint64=r2.AsUint64);
   end else
   begin
    Result:=CompareConstCount(Pointer(r1.key.pData),Pointer(r2.key.pData),r1.Count);
   end;
  end;
 end;
end;

Function TryTruncInt64(c:Single;var i:int64):Boolean;
begin
 Result:=(c>=Low(int64)) and (c<=High(int64));
 if Result then
 begin
  i:=Trunc(c);
 end;
end;

end.

