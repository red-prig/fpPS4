unit emit_print;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  spirv,
  srNodes,
  srTypes,
  srConst,
  srReg,
  srLayout,
  srVariable,
  srOp,
  srOpUtils,
  srCap,
  srRefId,
  Half16,
  SprvEmit;

type
 TSprvEmit_print=object(TSprvEmit)
  procedure Print;
  procedure PrintCaps;
  procedure PrintOpList(node:PspirvOp);
  procedure PrintHeaderInfo;
  procedure PrintTypes;
  procedure PrintConst(node:PsrConst);
  procedure PrintConst;
  procedure PrintVariable;
  procedure PrintFunc;
  procedure PrintOp(node:PSpirvOp;print_offset:Boolean);
  procedure PrintOpBlock(pBlock:PsrOpBlock);
 end;

implementation

procedure TSprvEmit_print.Print;
begin
 PrintCaps;
 Writeln;
 PrintHeaderInfo;
 Writeln;
 PrintTypes;
 Writeln;
 PrintConst;
 Writeln;
 PrintVariable;
 PrintFunc;
end;

procedure PrintRefId(P:PsrRefId);
begin
 Assert(P<>nil  ,'PrintRefId$1');
 Assert(P^.Alloc,'PrintRefId$2');
 Write('%',P^.ID);
end;

procedure PrintConstId(P:PsrConst);
var
 s:Single;
 i:Int64;
 u:QWORD;
begin
 Assert(P<>nil     ,'PrintConstId$1');
 Assert(P^.ID.Alloc,'PrintConstId$2');

 Case P^.key.dtype of
  dtBool:
    begin
     Case P^.AsBool of
      true :Write('%true');
      False:Write('%false');
     end;
    end;

  dtHalf16:
    begin
     s:=Single(P^.AsHalf16);
     i:=Trunc(s);
     if (s=i) then
     begin
      Case i of
         0..99:Write('%ch',i);
       else
               Write('%c',P^.ID.ID);
      end;
     end else
     begin
      Write('%c',P^.ID.ID);
     end;
    end;

  dtFloat32:
    begin
     s:=P^.AsFloat32;
     i:=Trunc(s);
     if (s=i) then
     begin
      Case i of
         0..99:Write('%cf',i);
        -9..-1:Write('%cfm',abs(i));
       else
               Write('%c',P^.ID.ID);
      end;
     end else
     begin
      Write('%c',P^.ID.ID);
     end;
    end;

   dtInt32 :
     begin
      i:=P^.AsInt;
      Case i of
         0..99:Write('%ci',i);
        -9..-1:Write('%cim',abs(i));
       else
               Write('%c',P^.ID.ID);
      end;
     end;

   dtUint32:
     begin
      u:=P^.AsUint;
      Case u of
         0..99:Write('%cu',u);
         else
               Write('%c',P^.ID.ID);
      end;
     end;

  else
   Write('%c',P^.ID.ID);
 end;

end;

procedure PrintVar(P:PsrVariable);
Var
 n:RawByteString;
begin
 Assert(P<>nil     ,'PrintVar$1');
 Assert(P^.ID.Alloc,'PrintVar$2');
 if (P^.pSource.pData<>nil) then
 begin
  n:=P^.GetName;
  if (n<>'') then
  begin
   Write('%',n);
  end else
  begin
   Write('%v',P^.ID.ID);
  end;
 end else
 begin
  Write('%v',P^.ID.ID);
 end;
end;

function type_get_base_name(dtype:TsrDataType):RawByteString;
begin
 Result:='';
 case dtype of

  dtBool       :Result:='bool';

  dtFloat32    :Result:='float';
  dtHalf16     :Result:='half';

  dtInt8       :Result:='int8';
  dtUint8      :Result:='uint8';

  dtInt16      :Result:='int16';
  dtUint16     :Result:='uint16';

  dtInt32      :Result:='int';
  dtUint32     :Result:='uint';

  dtInt64      :Result:='int64';
  dtUint64     :Result:='uint64';

  //Composite types
  dtVec2b      :Result:='bvec2';
  dtVec3b      :Result:='bvec3';
  dtVec4b      :Result:='bvec4';

  dtStruct2u   :Result:='rec2u';

  dtVec2u8     :Result:='u8vec2';
  dtVec4u8     :Result:='u8vec4';

  dtVec2i8     :Result:='i8vec2';
  dtVec4i8     :Result:='i8vec4';

  dtVec2u16    :Result:='u16vec2';
  dtVec4u16    :Result:='u16vec4';

  dtVec2i16    :Result:='i16vec2';
  dtVec4i16    :Result:='i16vec4';

  dtVec2u      :Result:='uvec2';
  dtVec3u      :Result:='uvec3';
  dtVec4u      :Result:='uvec4';

  dtVec2i      :Result:='ivec2';
  dtVec3i      :Result:='ivec3';
  dtVec4i      :Result:='ivec4';

  dtVec2f      :Result:='vec2';
  dtVec3f      :Result:='vec3';
  dtVec4f      :Result:='vec4';

  dtVec2h      :Result:='hvec2';
  dtVec4h      :Result:='hvec4';

  dtTypeVoid   :Result:='void';

  dtTypeSampler:Result:='samp';

  else;
 end;
end;

function type_get_base_name(node:PsrType):RawByteString;
var
 n:PsrType;
begin
 Result:='';
 case node^.dtype of
  {dtTypeImage:
    begin
     if DWORD(node^.key.ext.image)=DWORD(ExtImgBuf) then
     begin
      Result:='buf';
     end else
     if DWORD(node^.key.ext.image)=DWORD(ExtImage2D) then
     begin
      Result:='img2D';
     end;
    end;}
  dtTypeSampledImage:
    begin
     if (node^.key.count<>1) then Exit;
     n:=node^.GetCompItem(0);
     Result:=type_get_base_name(n);
     if (Result='') then Exit;
     Result:='sm'+Result;
    end;
  dtTypeArray:
    begin
     n:=node^.GetCompItem(0);
     Result:=type_get_base_name(n);
     if (Result='') then Exit;
     Result:='ar'+IntToStr(node^.key.ext.array_count)+Result;
    end;
  dtTypeRuntimeArray:
    begin
     n:=node^.GetCompItem(0);
     Result:=type_get_base_name(n);
     if (Result='') then Exit;
     Result:='ra'+Result;
    end;
  dtTypeStruct:
    begin
     if (node^.key.count<>1) then Exit;
     n:=node^.GetCompItem(0);
     Result:=type_get_base_name(n);
     if (Result='') then Exit;
     Result:='st'+Result;
    end;
  dtTypeFunction:
    begin
     if (node^.key.count<>1) then Exit;
     n:=node^.GetCompItem(0);
     Result:=type_get_base_name(n);
     if (Result='') then Exit;
     Result:='fn'+Result;
    end;
  else
    Result:=type_get_base_name(node^.dtype);
 end;
end;

procedure PrintTypeId(node:PsrType);
var
 s:RawByteString;
 n:PsrType;
begin
 case node^.dtype of
  dtTypePointer:
    begin
     n:=node^.GetCompItem(0);
     S:=type_get_base_name(n);
     if (S='') then
      PrintRefId(@node^.ID)
     else
     begin
      S:='p'+S;
      Case node^.key.ext.storage_class of
       StorageClass.UniformConstant :S:=S+'_uc';
       StorageClass.Input           :S:=S+'_in';
       StorageClass.Uniform         :S:=S+'_uf';
       StorageClass.Output          :S:=S+'_ot';
       StorageClass.Workgroup       :S:=S+'_wg';
       StorageClass.CrossWorkgroup  :S:=S+'_cw';
       StorageClass.Private_        :S:=S+'_pv';
       StorageClass.Function_       :S:=S+'_fc';
       StorageClass.PushConstant    :S:=S+'_pc';
       StorageClass.Image           :S:=S+'_im';
       StorageClass.StorageBuffer   :S:=S+'_sb';
       else
        S:='';
      end;
      if (S='') then
       PrintRefId(@node^.ID)
      else
       Write('%',S);
     end;
    end;
  else
   begin
    S:=type_get_base_name(node);
    if (S='') then
     PrintRefId(@node^.ID)
    else
     Write('%',S);
   end;
 end;
end;

procedure PrintChain(P:PsrChain);
begin
 Assert(P<>nil     ,'PrintChain$1');
 Assert(P^.ID.Alloc,'PrintChain$2');
 Write('%ac',P^.ID.ID);
end;

procedure PrintReg(P:PsrRegNode);
begin
 Assert(P<>nil,'PrintReg$1');
 Case P^.pWriter.ntype of
  ntConst:
    begin
     PrintConstId(P^.pWriter.pData);
    end;
  ntOp:
    begin
     if (not P^.ID.Alloc) then Assert(false,'PrintReg$2');
     Write('%r',P^.ID.ID);
     //Write('(',P^.read_count,')');
    end;
  else
   Assert(false,'PrintReg$3');
 end;
end;

procedure TSprvEmit_print.PrintCaps;
var
 node:PSpirvCap;
begin
 node:=FSpirvCaps.First;
 While (node<>nil) do
 begin
  Writeln(Op.GetStr(Op.OpCapability),' ',Capability.GetStr(node^.ID));
  node:=FSpirvCaps.Next(node);
 end;
end;

procedure TSprvEmit_print.PrintOpList(node:PspirvOp);
begin
 While (node<>nil) do
 begin
  PrintOp(node,false);
  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_print.PrintHeaderInfo;
begin
 PrintOpList(FHeader.pHead);
 Writeln;
 PrintOpList(FDebugInfo.pHead);
 if (FDebugInfo.pHead<>nil) then Writeln;
 PrintOpList(FDecorates.pHead);
end;

function Dim_GetStr(w:Word):RawByteString;
begin
 Result:='';
 Case w of
  Dim.Dim1D      :Result:='1D';
  Dim.Dim2D      :Result:='2D';
  Dim.Dim3D      :Result:='3D';
  Dim.Cube       :Result:='Cube';
  Dim.Rect       :Result:='Rect';
  Dim.Buffer     :Result:='Buffer';
  Dim.SubpassData:Result:='SubpassData';
  else
   Assert(False,'Dim_GetStr');
 end;
end;

procedure TSprvEmit_print.PrintTypes;
var
 node:PsrType;
 pConst:PsrConst;
 i:dword;
 ie:Boolean;
begin
 node:=FSpirvTypes.FList.pHead;
 While (node<>nil) do
 begin
  ie:=True;

  pConst:=nil;
  case node^.dtype of
   dtTypeArray:
     begin
      //find a const
      pConst:=FConsts.Fetchi(dtUInt32,node^.key.ext.array_count);
      PrintConst(pConst);
     end;
   else;
  end;

  PrintTypeId(node);
  Write(' = ');

  Write(Op.GetStr(node^.key.OpId));

  case node^.key.OpId of

   Op.OpTypeFloat:
    begin
     Write(' ',node^.key.ext.float_size);
    end;

   Op.OpTypeInt:
    begin
     Write(' ',node^.key.ext.int_size);
     Write(' ',node^.key.ext.int_sign);
    end;

   Op.OpTypeVector:
    begin
     ie:=False;
     Write(' ');
     PrintTypeId(node^.GetCompItem(0));
     Write(' ',node^.key.ext.array_count);
    end;

   Op.OpTypePointer:
    begin
     Write(' ',StorageClass.GetStr(node^.key.ext.Storage_Class));
    end;

   Op.OpTypeArray:
    begin
     ie:=False;
     Write(' ');
     PrintTypeId(node^.GetCompItem(0));
     Write(' ');
     PrintConstId(pConst);
    end;

   Op.OpTypeRuntimeArray:
    begin
     ie:=False;
     Write(' ');
     PrintTypeId(node^.GetCompItem(0));
    end;

   Op.OpTypeImage:
    begin
     ie:=False;
     Write(' ');
     PrintTypeId(node^.GetCompItem(0));
     With node^.key.ext.image do
      Write(' ',
            Dim_GetStr(Dim),' ',
            Depth,' ',
            Arrayed,' ',
            MS,' ',
            Sampled,' ',
            Spirv.ImageFormat.GetStr(Format));
    end;


  end;

  if ie then
   if (node^.key.count<>0) then
   begin
    For i:=0 to node^.key.count-1 do
    begin
     Write(' ');
     PrintTypeId(node^.GetCompItem(i));
    end;
   end;

  Writeln;

  node:=node^.pNext;
 end;
end;

const
 DefaultFormatSettings : TFormatSettings = (
   CurrencyFormat: 1;
   NegCurrFormat: 5;
   ThousandSeparator: ',';
   DecimalSeparator: '.';
   CurrencyDecimals: 2;
   DateSeparator: '-';
   TimeSeparator: ':';
   ListSeparator: ',';
   CurrencyString: '$';
   ShortDateFormat: 'd/m/y';
   LongDateFormat: 'dd" "mmmm" "yyyy';
   TimeAMString: 'AM';
   TimePMString: 'PM';
   ShortTimeFormat: 'hh:nn';
   LongTimeFormat: 'hh:nn:ss';
   ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                     'Jul','Aug','Sep','Oct','Nov','Dec');
   LongMonthNames: ('January','February','March','April','May','June',
                    'July','August','September','October','November','December');
   ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
   LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
   TwoDigitYearCenturyWindow: 50;
 );

procedure TSprvEmit_print.PrintConst(node:PsrConst);
var
 i:dword;
begin
 PrintConstId(node);
 Write(' = ');

 if (node^.key.count=0) then
 begin
  if (node^.key.dtype=dtBool) then
  begin
   Case node^.AsBool of
    True :Write(Op.GetStr(Op.OpConstantTrue));
    False:Write(Op.GetStr(Op.OpConstantFalse));
   end;
   Write(' ');
   PrintTypeId(node^.pType);
  end else
  begin
   Write(Op.GetStr(Op.OpConstant));
   Write(' ');
   PrintTypeId(node^.pType);
   Write(' ');
   case node^.key.dtype of
    dtFloat32:Write(FloatToStr(node^.AsFloat32,DefaultFormatSettings));
    dtHalf16 :Write(FloatToStr(Single(node^.AsHalf16),DefaultFormatSettings));
    dtInt32  :Write(node^.AsInt);
    dtUint32 :Write(node^.AsUint);
    dtInt64  :Write(node^.AsInt64);
    dtUint64 :Write(node^.AsUint64);
    else
     Assert(false,'PrintConst');
   end;
  end;
 end else
 begin
  Write(Op.GetStr(Op.OpConstantComposite));
  Write(' ');
  PrintTypeId(node^.pType);
  For i:=0 to node^.key.count-1 do
  begin
   Write(' ');
   PrintConstId(node^.GetCompItem(i));
  end;
 end;

 Writeln;
end;

procedure TSprvEmit_print.PrintConst;
var
 node:PsrConst;
begin
 node:=FConsts.FList.pHead;
 While (node<>nil) do
 begin

  //if not node^.is_first then
  //begin
   if (node^.key.dtype=dtUnknow) then
   begin
    Write('; ');
    PrintConstId(node);
    Writeln(' = dtUnknow: read_count=',node^.read_count,' value=',node^.Data);
   end else
   //Assert(node^.dtype<>dtUnknow);
   PrintConst(node);
  //end;

  node:=node^.pNext;
 end;
end;

procedure TSprvEmit_print.PrintVariable;
var
 node:PsrVariable;
begin
 node:=FVariables.pHead;
 While (node<>nil) do
 begin
  if (node^.pType<>nil) then
  begin
   PrintVar(node);
   Write(' = ',Op.GetStr(Op.OpVariable),' ');
   PrintTypeId(node^.pType);
   Writeln(' ',StorageClass.GetStr(node^.GetStorageClass));
  end;
  node:=node^.pNext;
 end;
end;

procedure PrintFuncId(P:PSpirvFunc);
begin
 Assert(P<>nil,'PrintFuncId');
 if (P^.name<>'') then
 begin
  Write('%',P^.name);
 end else
 begin
  PrintRefId(@P^.ID);
 end;
end;

procedure PrintOpParamSingle(const Param:TOpParamSingle);
begin
 Assert(Param.pData<>nil,'PrintOpParamSingle$1');
 Case Param.ntype of
   ntFunc :PrintFuncId(Param.pData);
   ntRefId:PrintRefId(Param.pData);
   ntType :PrintTypeId(Param.pData);
   ntReg  :PrintReg(Param.pData);
   ntVar  :PrintVar(Param.pData);
   ntChain:PrintChain(Param.pData);
   ntConst:PrintConstId(Param.pData);
  else
   Assert(false,'PrintOpParamSingle$2');
 end;
end;

procedure PrintOpParamNode(node:POpParamNode);
begin
 Case node^.ntype of
  ntLiteral:
    begin
     if StrLen(@node^.name)=0 then
     begin
      Write(node^.Value);
     end else
     begin
      Write(PChar(@node^.name));
     end;
    end;
  ntString:
    begin
      Write('"',PChar(@node^.name),'"');
    end;
  else
   begin
    PrintOpParamSingle(node^.AsParam);
   end;
 end;
end;

procedure TSprvEmit_print.PrintFunc;
var
 pFunc:PSpirvFunc;
begin
 pFunc:=FSpirvFuncs.FList.pHead;
 While (pFunc<>nil) do
 begin
  Writeln;
  PrintOpBlock(@pFunc^.FTop);
  pFunc:=pFunc^.pNext;
 end;
end;

procedure TSprvEmit_print.PrintOp(node:PSpirvOp;print_offset:Boolean);
var
 Param:POpParamNode;
 Info:Op.TOpInfo;
begin
 if (node=nil) then Exit;

 Info:=Op.GetInfo(node^.OpId);

 if Info.result then //dst
 begin
  Assert(node^.dst.ntype<>ntUnknow,'PrintOp$1');
  Assert(node^.dst.pData<>nil,'PrintOp$2');
  PrintOpParamSingle(node^.dst);
  Write(' = ');
  Write(Op.GetStr(node^.OpId));
 end else
 begin  //no dst
  Write(Op.GetStr(node^.OpId));
  if (node^.dst.ntype<>ntUnknow) then
  begin
   Assert(node^.dst.pData<>nil,'PrintOp$3');
   Write(' ');
   PrintOpParamSingle(node^.dst);
  end;
 end;

 if Info.rstype then //dst type
 begin
  Assert(node^.dst_type<>nil,'PrintOp$4');
  Write(' ');
  PrintTypeId(node^.dst_type);
 end;

 Param:=node^.pParam.pHead;
 While (Param<>nil) do
 begin
  Write(' ');
  PrintOpParamNode(Param);
  Param:=Param^.pNext;
 end;

 if (node^.OpId=Op.OpLabel) then
 begin
  print_offset:=true;
 end;

 Case print_offset of
  True :Writeln(' ;0x',HexStr(Node^.Adr.Offdw*4,4));
  False:Writeln;
 end;
end;

procedure TSprvEmit_print.PrintOpBlock(pBlock:PsrOpBlock);
var
 node:PSpirvOp;
begin
 if (pBlock=nil) then Exit;
 node:=pBlock^.pHead;
 While (node<>nil) do
 begin

  if (node^.OpId=OpBlock) then
  begin
   if (node^.dst.ntype=ntBlock) then
   begin
    PrintOpBlock(node^.dst.pData);
   end;
  end else
  begin
   Write(Space(pBlock^.FLevel));
   PrintOp(node,false);
  end;

  node:=node^.pNext;
 end;
end;


end.

