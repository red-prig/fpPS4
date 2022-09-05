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

type
 ntConst=class(TsrNodeVmt)
  class Procedure zero_read     (node:PsrNode);               override;
  class Procedure zero_unread   (node:PsrNode);               override;
  class function  Next          (node:PsrNode):Pointer;       override;
  class function  Prev          (node:PsrNode):Pointer;       override;
  class Function  GetPtype      (node:PsrNode):PsrNode;       override;
  class function  GetPrintName  (node:PsrNode):RawByteString; override;
  class function  GetRef        (node:PsrNode):Pointer;       override;
 end;

 PPsrConst=^PsrConst;
 PsrConst=^TsrConst;
 TsrConst=packed object(TsrNode)
  private
   pPrev,pNext,pLeft,pRight:PsrConst;
   //--
   ID:TsrRefId; //post id
   fOpId:WORD;
   fCount:WORD;
   FType:PsrType;
   pData:PPsrNode;
   function  c(n1,n2:PsrConst):Integer; static;
  public
   property  pType:PsrType     read FType;
   property  OpId:WORD         read fOpId;
   property  ItemCount:WORD    read fCount;
   Procedure Init; inline;
   function  dtype:TsrDataType;
   function  GetItem(i:Word):PsrNode;
   function  GetLiteral(i:Word):PsrLiteral;
   function  GetConst(i:Word):PsrConst;
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

 PsrConstList=^TsrConstList;
 TsrConstList=object
  type
   TNodeList=specialize TNodeList<PsrConst>;
   TNodeFetch=specialize TNodeFetch<PsrConst,TsrConst>;
  var
   FEmit:TCustomEmit;
   FList:TNodeList;
   FNTree:TNodeFetch;
  Procedure Init(Emit:TCustomEmit); inline;
  function  _Fetch(node:PsrConst;copy:Boolean):PsrConst;
  function  _Fetch1(dtype:TsrDataType;OpId:DWORD;value:QWORD):PsrConst;
  function  _FetchVector(dtype:TsrDataType;value:QWORD):PsrConst;
  function  Fetch(dtype:TsrDataType;value:QWORD):PsrConst;
  function  Fetch_b(value:Boolean):PsrConst; inline;
  function  Fetch_i(dtype:TsrDataType;value:Integer):PsrConst; inline;
  Function  Fetch_s(dtype:TsrDataType;value:Single):PsrConst; inline;
  function  FetchVector(pType:PsrType;count:Word;pData:PPsrConst;copy:Boolean):PsrConst;
  function  FetchVector(dtype:TsrDataType;pData:PPsrConst;copy:Boolean):PsrConst;
  function  Bitcast(rtype:TsrDataType;old:PsrConst):PsrConst;
  function  Fetch_ssrc9_const(SSRC:Word;d2:DWORD):PsrConst;
  function  Fetch_ssrc9_const(SSRC:Word;d2:DWORD;rtype:TsrDataType):PsrConst;
  function  Fetch_ssrc8_const(SSRC:Byte;d2:DWORD):PsrConst; inline;
  function  Fetch_ssrc8_const(SSRC:Byte;d2:DWORD;rtype:TsrDataType):PsrConst; inline;
  function  First:PsrConst; inline;
 end;

function get_soffset_const_int(SSRC:Word):Integer;
function is_const_soffset(SSRC:Byte):Boolean; inline;
function is_const_ssrc8(SSRC:Byte):Boolean; inline;
function is_const_ssrc9(SSRC:Word):Boolean; inline;
function CompareConst(r1,r2:PsrConst):Boolean;

implementation

class Procedure ntConst.zero_read(node:PsrNode);
var
 i:WORD;
begin
 With PsrConst(node)^ do
 begin
  FType^.mark_read(node);
  if (fCount<>0) then
   For i:=0 to fCount-1 do
   begin
    GetItem(i)^.mark_read(node);
   end;
 end;
end;

class Procedure ntConst.zero_unread(node:PsrNode);
var
 i:WORD;
begin
 With PsrConst(node)^ do
 begin
  FType^.mark_unread(node);
  if (fCount<>0) then
   For i:=0 to fCount-1 do
   begin
    GetItem(i)^.mark_unread(node);
   end;
 end;
end;

class function ntConst.Next(node:PsrNode):Pointer;
begin
 Result:=PsrConst(node)^.pNext;
end;

class function ntConst.Prev(node:PsrNode):Pointer;
begin
 Result:=PsrConst(node)^.pPrev;
end;

class Function ntConst.GetPtype(node:PsrNode):PsrNode;
begin
 Result:=PsrConst(node)^.FType;
end;

class function ntConst.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:=PsrConst(node)^.GetPrintName;
end;

class function ntConst.GetRef(node:PsrNode):Pointer;
begin
 Result:=@PsrConst(node)^.ID;
end;

//

Procedure TsrConst.Init; inline;
begin
 fntype:=ntConst;
end;

function TsrConst.c(n1,n2:PsrConst):Integer;
begin
 //first FType
 Result:=Integer(n1^.FType>n2^.FType)-Integer(n1^.FType<n2^.FType);
 if (Result<>0) then Exit;
 //second fCount
 Result:=Integer(n1^.fCount>n2^.fCount)-Integer(n1^.fCount<n2^.fCount);
 if (Result<>0) then Exit;
 //third pData
 Result:=ComparePtruint(PPtruint(n1^.pData),PPtruint(n2^.pData),n1^.fCount);
end;

function TsrConst.dtype:TsrDataType;
begin
 Result:=FType^.dtype;
end;

function TsrConst.GetItem(i:Word):PsrNode;
begin
 if (i>fCount) then Exit(nil);
 Result:=pData[i];
end;

function TsrConst.GetLiteral(i:Word):PsrLiteral;
begin
 Result:=GetItem(i)^.AsType(ntLiteral);
end;

function TsrConst.GetConst(i:Word):PsrConst;
begin
 Result:=GetItem(i)^.AsType(ntConst);
end;

Function TsrConst.AsBool:Boolean; inline;
begin
 Result:=GetLiteral(0)^.AsBool;
end;

function TsrConst.AsUint8:Byte; inline;
begin
 Result:=GetLiteral(0)^.AsUint8;
end;

function TsrConst.AsInt8:Shortint; inline;
begin
 Result:=GetLiteral(0)^.AsInt8;
end;

function TsrConst.AsUint16:Word; inline;
begin
 Result:=GetLiteral(0)^.AsUint16;
end;

function TsrConst.AsInt16:Smallint; inline;
begin
 Result:=GetLiteral(0)^.AsInt16;
end;

Function TsrConst.AsUint32:DWORD; inline;
begin
 Result:=GetLiteral(0)^.AsUint32;
end;

function TsrConst.AsInt32:Integer; inline;
begin
 Result:=GetLiteral(0)^.AsInt32;
end;

Function TsrConst.AsUint64:QWORD; inline;
begin
 Result:=GetLiteral(0)^.AsUint64;
end;

Function TsrConst.AsInt64:Int64; inline;
begin
 Result:=GetLiteral(0)^.AsInt64;
end;

function TsrConst.AsHalf16:THalf16; inline;
begin
 Result:=GetLiteral(0)^.AsHalf16;
end;

function TsrConst.AsFloat32:Single; inline;
begin
 Result:=GetLiteral(0)^.AsFloat32;
end;

function TsrConst.AsFloat64:Double; inline;
begin
 Result:=GetLiteral(0)^.AsFloat64;
end;

function TsrConst.GetData:QWORD;
begin
 Result:=0;
 case dtype of
  dtVec2u8,
  dtVec2i8:
   begin
    PBYTE(@Result)[0]:=GetConst(0)^.AsUint8;
    PBYTE(@Result)[1]:=GetConst(1)^.AsUint8;
   end;

  dtVec4u8,
  dtVec4i8:
   begin
    PBYTE(@Result)[0]:=GetConst(0)^.AsUint8;
    PBYTE(@Result)[1]:=GetConst(1)^.AsUint8;
    PBYTE(@Result)[2]:=GetConst(2)^.AsUint8;
    PBYTE(@Result)[3]:=GetConst(3)^.AsUint8;
   end;

  dtStruct2u,
  dtVec2u,
  dtVec2i,
  dtVec2f:
   begin
    PDWORD(@Result)[0]:=GetConst(0)^.AsUint32;
    PDWORD(@Result)[1]:=GetConst(1)^.AsUint32;
   end;

  dtVec2u16,
  dtVec2i16,
  dtVec2h  :
   begin
    PWORD(@Result)[0]:=GetConst(0)^.AsUint16;
    PWORD(@Result)[1]:=GetConst(1)^.AsUint16;
   end;

  dtVec4u16,
  dtVec4i16,
  dtVec4h  :
   begin
    PWORD(@Result)[0]:=GetConst(0)^.AsUint16;
    PWORD(@Result)[1]:=GetConst(1)^.AsUint16;
    PWORD(@Result)[2]:=GetConst(2)^.AsUint16;
    PWORD(@Result)[3]:=GetConst(3)^.AsUint16;
   end;

  else
     begin
      Assert(fCount=1,'fCount<>1');
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
 if (fCount<>1) then Exit(False);
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
     i:=Trunc(s);
     if (s=i) then
     begin
      Case i of
         0..99:Result:='ch'+IntToStr(i);
        -9..-1:Result:='chm'+IntToStr(abs(i));
       else;
      end;
     end;
    end;

  dtFloat32:
    begin
     s:=AsFloat32;
     i:=Trunc(s);
     if (s=i) then
     begin
      Case i of
         0..99:Result:='cf'+IntToStr(i);
        -9..-1:Result:='cfm'+IntToStr(abs(i));
       else;
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

function TsrConstList._Fetch(node:PsrConst;copy:Boolean):PsrConst;
var
 size:DWORD;
begin
 Result:=FNTree.Find(node);
 if (Result=nil) then
 begin
  Result:=FEmit.Alloc(SizeOf(TsrConst));
  Move(node^,Result^,SizeOf(TsrConst));

  if copy and (node^.fCount<>0) then
  begin
   size:=SizeOf(Pointer)*node^.fCount;
   Result^.pData:=FEmit.Alloc(size);
   Move(node^.pData^,Result^.pData^,Size);
  end;

  FNTree.Insert(Result);
  FList.Push_tail(Result);
 end;
end;

function TsrConstList._Fetch1(dtype:TsrDataType;OpId:DWORD;value:QWORD):PsrConst;
var
 pLiteralList:PsrLiteralList;
 pTypeList:PsrTypeList;
 node:TsrConst;
 item:array[0..0] of PsrNode;
begin
 Result:=nil;
 pLiteralList:=FEmit.GetLiteralList;
 pTypeList   :=FEmit.GetTypeList;
 //
 item[0]:=pLiteralList^.FetchConst(dtype,value);
 //
 node:=Default(TsrConst);
 node.Init;
 node.fOpId :=OpId;
 node.fCount:=1;
 node.FType :=pTypeList^.Fetch(dtype);
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrConstList._FetchVector(dtype:TsrDataType;value:QWORD):PsrConst;
var
 pTypeList:PsrTypeList;
 node:TsrConst;
 item:array[0..3] of PsrNode;
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
 node:=Default(TsrConst);
 node.Init;
 node.fOpId :=Op.OpConstantComposite;
 node.fCount:=dtype.Count;
 node.FType :=pTypeList^.Fetch(dtype);
 node.pData :=@item;
 Result:=_Fetch(@node,True);
end;

function TsrConstList.Fetch(dtype:TsrDataType;value:QWORD):PsrConst;
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

function TsrConstList.Fetch_b(value:Boolean):PsrConst; inline;
begin
 Result:=Fetch(dtBool,QWORD(value));
end;

function TsrConstList.Fetch_i(dtype:TsrDataType;value:Integer):PsrConst; inline;
begin
 Result:=Fetch(dtype,PDWORD(@value)^);
end;

Function TsrConstList.Fetch_s(dtype:TsrDataType;value:Single):PsrConst; inline;
begin
 Result:=Fetch(dtype,PDWORD(@value)^);
end;

function TsrConstList.FetchVector(pType:PsrType;count:Word;pData:PPsrConst;copy:Boolean):PsrConst;
var
 node:TsrConst;
begin
 Assert(count<>0);
 Assert(pData<>nil);
 node:=Default(TsrConst);
 node.Init;
 node.fOpId :=Op.OpConstantComposite;
 node.fCount:=count;
 node.FType :=pType;
 node.pData :=Pointer(pData);
 Result:=_Fetch(@node,copy);
end;

function TsrConstList.FetchVector(dtype:TsrDataType;pData:PPsrConst;copy:Boolean):PsrConst;
var
 pTypeList:PsrTypeList;
begin
 pTypeList:=FEmit.GetTypeList;
 Result:=FetchVector(pTypeList^.Fetch(dtype),dtype.Count,pData,copy);
end;

function TsrConstList.Bitcast(rtype:TsrDataType;old:PsrConst):PsrConst;
var
 data:qword;
begin
 Result:=nil;

 if not old^.IsType(ntConst) then Exit(old);

 if (rtype=dtUnknow) or (rtype=old^.dtype) then
 begin
  Exit(old);
 end;

 if (old^.dtype=dtUnknow) then
 begin
  data:=old^.GetData;
  Result:=Fetch(rtype,data);
  Exit;
 end;

 if TryBitcastType(rtype,old^.dtype) then
 begin
  data:=old^.GetData;
  Result:=Fetch(rtype,data);
 end else
 begin
  Assert(false,'const bitcast');
 end;
end;

function TsrConstList.Fetch_ssrc9_const(SSRC:Word;d2:DWORD):PsrConst;
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

function TsrConstList.Fetch_ssrc8_const(SSRC:Byte;d2:DWORD):PsrConst; inline;
begin
 Result:=Fetch_ssrc9_const(SSRC,d2);
end;

function TsrConstList.Fetch_ssrc8_const(SSRC:Byte;d2:DWORD;rtype:TsrDataType):PsrConst; inline;
begin
 Result:=Fetch_ssrc9_const(SSRC,d2,rtype);
end;

function TsrConstList.First:PsrConst; inline;
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

function CompareConst(r1,r2:PsrConst):Boolean;
begin
 Result:=false;
 if (r1<>nil) and (r2<>nil) then
 begin
  Result:=(r1=r2);
  if Result then Exit;
  if CompareType(r1^.dtype,r2^.dtype) and (r1^.fCount=r2^.fCount) then
  begin
   if (r1^.fCount=1) then
   begin
    Result:=(r1^.AsUint64=r2^.AsUint64);
   end else
   begin
    Result:=CompareConstCount(Pointer(r1^.pData),Pointer(r2^.pData),r1^.fCount);
   end;
  end;
 end;
end;

end.

